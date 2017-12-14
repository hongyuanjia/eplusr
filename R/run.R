################################################################################
#                                Run EnergyPlus                                #
################################################################################

#' @title Find EnergyPlus locations on current computer.
#'
#' \code{find_eplus} try to find the locations of EnergyPlus on current
#' computer.
#'
#' @param ver The version to locate for. If NULL, all versions will be returned.
#' @param verbose If TRUE, extra message will be shown.
#' @return A tibble object with two columns named 'path' and 'version'.
#' @importFrom purrr flatten_chr map
#' @importFrom stringr str_detect
#' @export
find_eplus <- function(ver = NULL, verbose = FALSE){
    # {{{1
    # Define searching paths.
    # 1. check if drives exist.
    disks <- get_volumes()
    # 2. search EnergyPlus in drive root path and program file folders.
    os <- Sys.info()['sysname']
    if (os == "Windows") {
        path_list <- c(paste0(disks, "\\"),
                       file_path(disks, "Program Files"),
                       file_path(disks, "Program Files (x86)"))
    } else {
        stop('Unsupported OS.')
    }

    # Find if EnergyPlus folder exists in path list.
    eplus_dir <-
        purrr::flatten_chr(purrr::map(path_list,
                                      ~grep(x = dir(path = .x, full.names = TRUE),
                                            "EnergyPlus", value = TRUE, fixed = TRUE)))

    # If not, give a warning of specifying EnergyPlus program location
    # mannually.
    if (length(eplus_dir) == 0L) {
        stop(paste("Cannot find EnergyPlus folder. ",
                   "Please specify EnergyPlus path mannually."),
             call. = FALSE)
    }

    # Validate the paths
    validated <- check_energyplus(eplus_dir)
    # Get the version(s) of EnergyPlus.
    idd_ver <- get_idd_ver(eplus_dir)
    results <- dplyr::tibble(path = eplus_dir, version = idd_ver)
    # Get the latest version of EnergyPlus and its path.
    results <- dplyr::arrange(results, version)
    eplus_dir_latest <- results[nrow(results),][["path"]]
    idd_ver_latest <- results[nrow(results),][["version"]]

    # If no EnergyPlus version is specified, use the latest version.
    if (is.null(ver)) {
        # If multiple EnergyPlus versions are found, use the latest version.
        if (length(eplus_dir) > 1) {
            if (verbose) {
                # List the paths and versions.
                message(paste("Multiple EnergyPlus versions are found: \n"),
                        paste(eplus_dir, "Version:", idd_ver, collapse = "\n"), "\n")
            }
        # If only one EnergyPlus version is found, print message.
        } else {
            if (verbose) {
                message(paste("EnergyPlus Version:", idd_ver_latest,
                              "has been successfully located:\n", eplus_dir_latest, "\n"))
            }
        }
    } else {
        if (!stringr::str_detect(ver, "^\\d+\\.\\d+$")) {
            stop("'ver' should be a format of 'X.Y'.", call. = FALSE)
        }
        ver <- paste0(gsub(x = as.character(ver), " ", ""), ".0")
        if (is.na(match(ver, idd_ver))) {
            stop(paste0("Cannot find EnergyPlus Version:", ver, " ",
                        "Please specify EnergyPlus path mannually.\n"), call. = FALSE)
        } else {
            eplus_dir_matched <- eplus_dir[match(ver, idd_ver)]
            idd_ver_matched <- idd_ver[match(ver, idd_ver)]
            if (verbose) {
                message(paste0("EnergyPlus Version: ", idd_ver_matched,
                               " has been successfully located:\n", eplus_dir_matched, "\n"))
            }
        }
        results <- dplyr::tibble(path = eplus_dir_matched, version = idd_Ver_matched)
    }

    return(results)
}
# }}}1

#' @importFrom readr read_lines
#' @importFrom stringr str_interp
#' @importFrom tools file_path_sans_ext file_ext
#' @export
# run_eplus: A function to run EnergyPlus in R.
# run_eplus {{{1
run_eplus <- function (model, weather = NULL, output_dir = NULL, output_prefix = NULL,
                       output_suffix = c("C", "L", "D"), csv = TRUE, echo = FALSE,
                       special_run = NULL, eplus_dir = NULL, eplus_ver = NULL,
                       start_title = NULL, finish_title = NULL) {
    # In order to chain commands, this has to be used before commands.
    cmd_head <- "cmd.exe /c"
    # Get default values {{{2
    weather <- weather %||% get_default_epw()
    output_dir <- output_dir %||% file_path(dirname(model))
    output_prefix <- output_prefix %||% file_prefix(model)
    output_suffix <- rlang::arg_match(output_suffix)
    # }}}2
    # 'eplus_dir' and 'eplus_ver' checking {{{2
    eplus_dir_ver <- check_eplus_dir_ver(eplus_dir, eplus_ver)
    eplus_dir <- eplus_dir_ver[["eplus_dir"]]
    eplus_ver <- eplus_dir_ver[["eplus_ver"]]
    # }}}2
    # Get job info {{{2
    job_info <- pre_proc(model = model, weather = weather,
                         output_dir = output_dir, output_prefix = output_prefix,
                         output_suffix = output_suffix, csv = csv, echo = echo,
                         special_run = special_run, eplus_dir = eplus_dir,
                         start_title = start_title, finish_title = finish_title)
    cmd_job <- job_info[["cmd_job"]]
    cmd_run <- job_info[["cmd_run"]]
    output_dir <- job_info[["output_dir"]]
    output_prefix <- job_info[["output_prefix"]]
    # }}}2
    # Set working dir as 'output_dir' {{{2
    ori_wd <- getwd()
    setwd(output_dir)
    on.exit(setwd(ori_wd))
    # }}}2
    # Clean output dir before simulation {{{2
    output_model <- file_path(output_dir, paste0(output_prefix, ".idf"))
    # If output dir is the dir where the model is, keep the model and weather in
    # order to use them in simulation
    keep_input <- if (identical(file_path(model), output_model)) TRUE else FALSE
    if (echo) {
        clean_wd(path = output_model, suffix_type = output_suffix, keep_input = keep_input)
    } else {
        suppressMessages(clean_wd(path = output_model, suffix_type = output_suffix, keep_input = keep_input))
    }
    # }}}2
    # Write 'run.bat' and 'eplusr_run.bat'into the output dir {{{2
    readr::write_lines(cmd_run, "run.bat")
    readr::write_lines(cmd_job, "eplusr_run.bat")
    # }}}2

    command <- paste0(cmd_head, " eplusr_run.bat && DEL eplusr_run.bat")
    status <- system(command = command, wait = echo, invisible = echo)
    if (status != 0L) {
        stop("Error occured when running commands.", call. = FALSE)
    }
    if (!echo) {
        msg_head <- paste0(
            "Simulation has been successfully executed using EnergyPlus V",
            eplus_ver, " located at", sQuote(eplus_dir), ".\n----------\n")
        msg_job <- paste0(
            "Model: ", file_path(model), "\n",
            "Weather: ", file_path(weather), "\n----------")
        message(msg_head, msg_job)
    }

    return(invisible())
}
# }}}1

#' @importFrom purrr map_chr
#' @importFrom stringr str_replace
#' @importFrom readr read_lines
# get_idd_ver {{{1
# A helper to get IDD version
get_idd_ver <- function(eplus_dir){
    idd <- file_path(eplus_dir, "Energy+.idd")
    idd_ver <- purrr::map_chr(idd, ~stringr::str_replace(readr::read_lines(.x, n_max = 1),
                                                         "!IDD_Version ", ""))
    return(idd_ver)
}
# }}}1
# check_energyplus{{{1
# A helper to check if EnergyPlus executable exists in the input dir.
check_energyplus <- function (eplus_dir) {
    os <- Sys.info()['sysname']
    if (os == "Windows") {
        energyplus <- file_path(eplus_dir, "energyplus.exe")
    } else {
        energyplus <- file_path(eplus_dir, "energyplus")
    }
    idd <- file_path(eplus_dir, "Energy+.idd")

    purrr::map2_lgl(energyplus, idd, ~{ if (all(file.exists(.x, .y))) TRUE else FALSE })
}
# }}}1
# get_volumes{{{1
# A helper to get all disks (borrowed from 'getVolumes' function in 'shinyFiles'
# package).
get_volumes <- function(exclude = NULL) {
    os <- Sys.info()['sysname']
    if (os == 'Darwin') {
        volumes <- list.files('/Volumes/', full.names=T)
        names(volumes) <- basename(volumes)
    } else if (os == 'Linux') {
        volumes <- c('Computer'='/')
        local <- list.files('/usr/local/', full.names=T)
        names(local) <- basename(local)
        volumes <- c(volumes, local)
    } else if (os == 'Windows') {
        volumes <- system('wmic logicaldisk get Caption', intern=T)
        volumes <- sub(' *\\r$', '', volumes)
        keep <- !tolower(volumes) %in% c('caption', '')
        volumes <- volumes[keep]
        volNames <- system('wmic logicaldisk get VolumeName', intern=T)
        volNames <- sub(' *\\r$', '', volNames)
        volNames <- volNames[keep]
        volNames <- paste0(volNames, ifelse(volNames == "", "", " "))
        volNames <- paste0(volNames, '(', volumes, ')')
        names(volumes) <- volNames
    } else {
        stop('unsupported OS')
    }
    if (!is.null(exclude)) {
        volumes <- volumes[!names(volumes) %in% exclude]
    }

    return(volumes)
}
# }}}1
# pre_proc {{{1
pre_proc <- function (model, weather, output_dir = NULL, output_prefix = NULL,
                      output_suffix = c("C", "L", "D"), csv = TRUE, echo = FALSE,
                      special_run = NULL, eplus_dir = NULL, pause = !echo,
                      start_title = NULL, finish_title = NULL) {
    # File existing checking {{{2
    if (!file.exists(model)) {
        stop("Input model '", basename(model),
             "' file does not exist. Please check input.", call. = FALSE)
    }
    if (!file.exists(weather)) {
        stop("Weather file '", basename(weather),
             "' does not exist. Please check input.", call. = FALSE)
    }
    # }}}2
    # File extension checking {{{2
    if (!has_model_ext(model)) {
       stop("'model' should be a full file path with an extension of '.idf' or '.imf'.",
            call. = FALSE)
    }
    if (!has_epw_ext(weather)) {
       stop("'weather' should be a full file path with an extension of '.epw'.",
            call. = FALSE)
    }
    # }}}2
    # Parametric field existence checking {{{2
    model_basename <- basename(model)
    model_lines <- read_idf_lines(model)
    if (is_param_exist(model_lines)) {
        params <- paste0(list_params(model_lines), collapse = ", ")
        info <- paste0("Model ", model_basename, ": ", params)
        msg <- paste0(
            "Model ", model_basename, " has parametric fields which do not have values. ",
            "Please clean the model before simulation.\n")
        msg_all <- c(msg, "\n", paste0(info, collapse = "\n"))
        stop(msg_all, call. = FALSE)
    }
    # }}}2
    # Get energyplus exec path {{{2
    energyplus <- file_path(eplus_dir, "energyplus.exe")
    # }}}2
    # Model file version and EnergyPlus verion match checking {{{2
    eplus_ver <- get_idd_ver(eplus_dir)
    model_ver <- paste0(get_idf_ver(model_lines), ".0")
    if (!identical(model_ver, eplus_ver)) {
        warning(paste0(
            "The input model '", model, "' indicates an EnergyPlus verion of '",
            model_ver, "' but is simulated using EnergyPlus '", eplus_ver,
            "'. Unexpected results may occur."), call. = FALSE)
    }
    # }}}2
    # Copy files used in 'Schedule:File' to output dir {{{2
    copy_external_file(model_lines, output_dir = output_dir)
    # }}}2
    # Get value of 'annual' and 'design_day' {{{2
    if (!is.null(special_run)) {
        if (identical(special_run, "annual")) {
            annual <- TRUE
            design_day <- FALSE
        } else if (identical(special_run, "design_day")) {
            annual <- FALSE
            design_day <- TRUE
        } else {
            stop("'special_run' should be NULL or one of c('annual', 'design_day')",
                 call. = FALSE)
        }
    } else {
        annual <- FALSE
        design_day <- FALSE
    }
    # }}}2
    # Get command of `system`{{{2
    cmd <- cmd_run(eplus_dir = eplus_dir, model = model, weather = weather,
                   output_dir = output_dir, output_prefix = output_prefix,
                   output_suffix = output_suffix, expand_obj = TRUE,
                   readvars = csv, annual = annual, design_day = design_day,
                   idd = NULL, pause = pause,
                   start_title = start_title, finish_title = finish_title)
    # }}}2
    # Get 'run.bat' {{{2
    # Use 'eplus_dir' to locate the command
    run_bat <- cmd[grepl(energyplus, cmd, fixed = TRUE)]
    run_bat <- change_output_dir(run_bat, output_dir)
    # }}}2

    info <- list(cmd_job = cmd, cmd_run = run_bat, output_dir = output_dir,
                 output_prefix = output_prefix)

    return(info)
}
# }}}1
# cmd_run {{{
cmd_run <- function (eplus_dir, model, weather, output_dir, output_prefix,
                     output_suffix, expand_obj = TRUE, readvars = TRUE,
                     annual = FALSE, design_day = FALSE, idd = NULL,
                     pause = FALSE, start_title = NULL, finish_title = NULL) {
    cmd_head <- "@ECHO OFF"
    # Get OS-dependent file paths {{{2
    eplus_dir <- file_path(eplus_dir)
    model <- file_path(model)
    weather <- file_path(weather)
    output_dir <- file_path(output_dir)
    # }}}2
    # Get model and weather name {{{2
    model_name <- file_prefix(model)
    weather_name <- file_prefix(weather)
    # }}}2
    # Get model extension {{{2
    model_ext <- tolower(tools::file_ext(model))
    weather_ext <- tolower(tools::file_ext(weather))
    # }}}2
    # Use temp folder in the output_dir {{{2
    wd <- paste0("eplusr_", stringi::stri_rand_strings(1, 5, pattern = "[a-z0-9]"))
    wd <- file_path(output_dir, wd)
    if (!dir.exists(wd)) {
        dir.create(wd, recursive = TRUE, showWarnings = TRUE)
    }
    # }}}2
    # Create 'Energy+.ini' in temp folder {{{2
    create_eplus_ini(eplus_dir = eplus_dir, working_dir = wd)
    # }}}2
    # Get the new name of model and weather according to 'output_prefix' {{{2
    new_model_name <- paste0(output_prefix, ".", model_ext)
    new_weather_name <- paste0(output_prefix, ".", weather_ext)
    new_model <- file_path(wd, new_model_name)
    new_weather <- file_path(wd, new_weather_name)
    # }}}2
    # cmd_mkdir: Make temp dir if it does not exists {{{2
    cmd_mkdir <- paste("IF NOT EXIST", shQuote(file_path(wd, "\\")),
                       "MKDIR", shQuote(wd), sep = " ")
    # }}}2
    # cmd_copy_input: Copy model and weather file into temp dir {{{2
    cmd_copy_model <- paste("COPY /Y", shQuote(model), shQuote(new_model), ">NUL", sep = " ")
    cmd_copy_weather <- paste("COPY /Y", shQuote(weather), shQuote(new_weather), ">NUL", sep = " ")
    cmd_copy_input <- c(cmd_copy_model, cmd_copy_weather)
    # }}}2
    # cmd_working(output)_dir: Change working dir when run EnergyPlus. {{{2
    cmd_working_dir <- paste0("cd ", dQuote(wd))
    # }}}2
    # cmd_postproc: Post process after simulation {{{2
    epmacro <- ifelse(identical(model_ext, "imf"), TRUE, FALSE)
    cmd_post_proc <- cmd_postproc(model_name = model_name, epmacro = epmacro,
                                  path_wd = wd, path_copy_to = output_dir)
    # }}}2
    # cmd_title_e(s): Get the title of command window {{{2
    if (is.null(start_title)) {
        cmd_title_s <- paste0("TITLE ",
            shQuote(paste0("Model: ", basename(model), " | Simulation in progress")))
    } else {
        cmd_title_s <- paste0("TITLE ", shQuote(start_title))
    }
    if (is.null(finish_title)) {
        cmd_title_e <- paste0("TITLE ",
            shQuote(paste0("Model: ", basename(model), " | Simulation finished")))
    } else {
        cmd_title_e <- paste0("TITLE ", shQuote(finish_title))
    }
    # }}}2
    # cmd_msg: Show a cmd window when the simulation finishs. {{{2
    end_file <- output_files(output_prefix, output_suffix, ext = ".end", simplify = TRUE)
    cmd_msg <- paste0("type ", end_file)
    # }}}2
    # cmd_pause: Retain the cmd window {{{2
    if (pause) cmd_pause <- "PAUSE" else cmd_pause <- NULL
    # }}}2
    # cmd_eplus {{{2
    cmd_eplus <- cmd_eplus(eplus_dir = eplus_dir,
                           model = new_model_name, weather = new_weather_name,
                           output_dir = wd, output_prefix = output_prefix,
                           output_suffix = output_suffix,
                           expand_obj = expand_obj, readvars = readvars,
                           annual = annual, design_day = design_day, idd = idd)
    # }}}2
    # Combine all commands {{{2
    cmd <- c(cmd_head, cmd_mkdir, cmd_copy_input,
             cmd_title_s, cmd_working_dir, cmd_eplus,
             cmd_post_proc, cmd_msg, cmd_title_e, cmd_pause)
    # }}}2
    return(cmd)
}
# }}}
# cmd_postproc {{{
cmd_postproc <- function (model_name, epmacro = TRUE, path_wd, path_copy_to) {
    post_proc <- c()
    # Delete "fort.6" and "Energy+.ini" created by "EnergyPlus.exe" and
    # "readvars.audit" created by "ReadVarsESO.exe" after simulations.
    del_list <- c("fort.6", "Energy+.ini", "readvars.audit")
    cmd_del <- paste("IF EXIST", del_list, "DEL", del_list)
    post_proc <- c(post_proc, cmd_del)
    if (epmacro) {
        rename_imf <- paste0("IF EXIST out.idf MOVE out.idf ", model_name, ".idf")
        post_proc <- c(post_proc, rename_imf)
    }
    cmd_cd_path <- paste("CD", shQuote(path_copy_to), sep = " ")
    cmd_move <- paste("MOVE /Y" ,
                      shQuote(file_path(path_wd, "*.*")),
                      shQuote(path_copy_to), ">NUL", sep = " ")
    cmd_deldir <- paste("RMDIR" , shQuote(path_wd), "/S/Q >NUL", sep = " ")
    post_proc <- c(post_proc, cmd_cd_path, cmd_move, cmd_deldir)

    return(post_proc)
}
# }}}
# cmd_eplus {{{1
cmd_eplus <- function (eplus_dir, model, weather, output_dir, output_prefix,
                       output_suffix, expand_obj = TRUE, readvars = TRUE,
                       annual = FALSE, design_day = FALSE, idd = NULL) {
    ############################################################################
    #           Notes on arguments (From EnergyPlus documentation)             #
    ############################################################################
    # 1. model
    # Full path of a model file with extension of idf or imf to use.

    # 2. weather
    # Full path of a weather file to use.

    # 3. output_dir
    # Output directory of the simulation results.

    # 4. output_prefix
    # Prefix for output file names (default: eplus)

    # 5. output_suffix
    # Suffix style for output file names (default: L)
    #     L: Legacy (e.g., eplustbl.csv)
    #     C: Capital (e.g., eplusTable.csv)
    #     D: Dash (e.g., eplus-table.csv)

    # 6. epmacro
    # If TRUE, EPMacro will be run perior to simulation.

    # 7. expand_obj
    # If TRUE, ExpandObjects will be run perior to simulation.

    # 8. readvars
    # If TRUE, ReadVarsESO will be run after simulation.

    # 9. annual
    # If TRUE, force annual simulation.

    # 10. design_day
    # If TRUE, force design-day-only simulation.

    # 11. idd
    # The full path of Input Data Dictionary. (default: Energy+.idd in
    # executable directory)

    # 12. legacy (Currently not implemented yet.)
    # If TRUE, use legacy mode to run EnergyPlus. By using legacy mode, the
    # input idf file and weather file will be renamed into "in.idf" and "in.epw"
    # respectively. The file "Energy+.idd" in EnergyPlus folder will be copied
    # into the working direcotry.
    # BACCKGROUND: The command line interface is a new feature as of EnergyPlus
    # 8.3. Prior to version 8.3, the EnergyPlus executable took no command line
    # arguments, and instead expected the IDD (Input Data Dictionary) file and
    # the IDF files to be located in the current working directory and named
    # Energy+.idd and in.idf respectively.  If a weather file was required by
    # the simulation, then an in.epw file was also required in the same
    # directory. This behavior is still respected if no arguments are passed on
    # the command line.
    ############################################################################

    # Get 'energyplus' exec path {{{2
    # TODO: Add support for Linux and Mac support
    energyplus <- file_path(eplus_dir, "energyplus.exe")
    # }}}2
    # Get the right format of the input command to EnergyPlus. {{{2
    # NOTE: `ifelse` function cannot return NULL.
    if (has_imf_ext(model)) {
        epmacro <- TRUE
        cmd_epmacro <- "--epmacro"
    } else {
        epmacro <- FALSE
        cmd_epmacro <- NULL
    }
    if (expand_obj) cmd_expand_obj <- "--expandobjects" else cmd_expand_obj <- NULL
    if (readvars) cmd_readvars <- "--readvars" else cmd_readvars <- NULL
    if (annual) cmd_annual <- "--annual" else cmd_annual <- NULL
    if (design_day) cmd_design_day <- "--design-day" else cmd_design_day <- NULL
    if (!is.null(idd)) cmd_idd <- paste0("--idd", shQuote(idd)) else cmd_idd <- NULL
    # }}}2
    # In case there are spaces in user input, quote all pathes {{{2
    cmd_eplus <- paste(
        shQuote(energyplus), "--weather", shQuote(weather),
        "--output-directory", shQuote(output_dir),
        "--output-prefix", shQuote(output_prefix),
        "--output-suffix", shQuote(output_suffix),
        cmd_epmacro, cmd_expand_obj, cmd_readvars, cmd_annual, cmd_design_day, cmd_idd,
        shQuote(model)
    )
    # }}}2

    return(cmd_eplus)
}
# }}}1
# get_default_epw {{{1
get_default_epw <- function (weather = NULL) {
    if (!is.null(weather)) return(weather)
    eplus_dir <- getOption("eplusr.eplus_dir")
    weather_dir <- file_path(eplus_dir, "WeatherData")
    epws <- list.files(weather_dir, pattern = "\\.epw", full.names = FALSE)
    if (identical(length(epws), 0L)) stop("No weather file found.", call. = FALSE)
    chicago_epw <- grep("Chicago", epws, value = TRUE)
    if (identical(length(chicago_epw), 0L)) {
        default_epw <- chicago_epw
    } else {
        default_epw <- epws[1]
    }

    warning("Missing weather input, weather file ", sQuote(default_epw),
            " located at ", sQuote(weather_dir), " will been used.",
            call. = FALSE)

    epw_path <- file_path(weather_dir, default_epw)
    return(epw_path)
}
# }}}1
# check_eplus_dir_ver{{{1
check_eplus_dir_ver <- function (eplus_dir = NULL, eplus_ver = NULL) {
    if (!is.null(eplus_dir)) {
        assertthat::assert_that(assertthat::is.string(eplus_dir))
    }
    if (!is.null(eplus_ver)) {
        assertthat::assert_that(assertthat::is.string(eplus_ver))
    }
    default_eplus_dir <- getOption("eplusr.eplus_dir")
    all_eplus <- find_eplus(verbose = FALSE)
    validated_eplus_dirs <- all_eplus[["path"]]
    validated_eplus_vers <- all_eplus[["version"]]
    # If 'eplus_ver' is given{{{2
    if (!is.null(eplus_ver)) {
        if (!stringr::str_detect(eplus_ver, "^\\d+\\.\\d+$")) {
            stop("'eplus_ver' should be a format of 'X.Y'.", call. = FALSE)
        }
        full_eplus_ver <- paste0(eplus_ver, ".0")
        # Case 1. But 'eplus_dir' is not given{{{3
        if (is.null(eplus_dir)) {
            # Sub Case 1. If the given 'eplus_ver' is valid{{{4
            idx_ver <- match(full_eplus_ver, validated_eplus_vers)
            if (!is.na(idx_ver)) {
                eplus_dir_used <- validated_eplus_dirs[idx_ver]
                eplus_ver_used <- validated_eplus_vers[idx_ver]
                message("EnergyPlus V", eplus_ver_used, " located at '",
                        eplus_dir_used, "' will be used.")
            # }}}4
            # Sub Case 2. If the given 'eplus_ver' is not valid{{{4
            } else {
                # If no EnergyPlus location was found when 'eplusr' loaded, i.e.
                # getOption('eplusr.eplus_dir') is "".
                if (default_eplus_dir == "") {
                    stop("'eplusr' could not detect valid EnergyPlus locations automatically.\nPlease change the option 'eplusr.eplus_dir', or give a valid EnergyPlus location in 'eplus_dir'.", call. = FALSE)
                } else {
                    eplus_dir_used <- default_eplus_dir
                    eplus_ver_used <- get_idd_ver(default_eplus_dir)
                    warning("Could not find the specified EnergyPlus V", full_eplus_ver, ".\nThe default EnergyPlus V", eplus_ver_used, " located at '", eplus_dir_used, "' will be used.", call. = FALSE)
                }
            # }}}4
            }
        # }}}3
        # Case 2. And 'eplus_dir' is given.{{{3
        } else {
            # Sub Case 1. If the given 'eplus_ver' is valid{{{4
            idx_ver <- match(full_eplus_ver, validated_eplus_vers)
            if (!is.na(idx_ver)) {
                eplus_dir_from_ver <- validated_eplus_dirs[idx_ver]
                eplus_ver_from_ver <- validated_eplus_vers[idx_ver]
                # Sub Sub Case 1. If the given 'eplus_dir' is valid too
                if (check_energyplus(eplus_dir)) {
                    msg_head <- "Both 'eplus_dir' and 'eplus_ver' are given.\n"
                    # Get the version from 'eplus_dir'
                    eplus_dir_from_dir <- eplus_dir
                    eplus_ver_from_dir <- get_idd_ver(eplus_dir)
                    # Get the latest EnergyPlus version.
                    if (eplus_ver_from_ver > eplus_ver_from_dir) {
                        eplus_dir_used <- eplus_dir_from_ver
                        eplus_ver_used <- eplus_ver_from_ver
                        msg_ver <- paste0("'eplus_ver' indicates a newer version of EnergyPlus V",
                                          eplus_ver_from_ver, " than the version of EnergyPlus V",
                                          eplus_ver_from_dir," indicated from 'eplus_dir'.\n",
                                          "The newer one will be used:\n\n")
                    } else if (eplus_ver_from_ver < eplus_ver_from_dir) {
                        eplus_dir_used <- eplus_dir_from_dir
                        eplus_ver_used <- eplus_ver_from_dir
                        msg_ver <- paste0("'eplus_dir' indicates a newer version of EnergyPlus V",
                                          eplus_ver_from_dir, " than the version of EnergyPlus V",
                                          eplus_ver_from_ver," indicated from 'eplus_ver'.\n",
                                          "The newer one will be used:\n\n")
                    } else {
                        eplus_dir_used <- eplus_dir_from_dir
                        eplus_ver_used <- eplus_ver_from_dir
                        msg_ver <- paste0("'eplus_dir' and 'eplus_ver' indicate a same version of EnergyPlus V", eplus_ver_used,".\n'eplus_dir' has the priority over 'eplus_ver', and EnergyPlus below will be used:\n\n")
                    }
                    warning(msg_head, msg_ver,
                            "EnergyPlus V", eplus_ver_used, " located at '", eplus_dir_used, "'.",
                            call. = FALSE)
                # Sub Sub Case 2. If the given 'eplus_dir' is not valid
                } else {
                    eplus_dir_used <- eplus_dir_from_ver
                    eplus_ver_used <- eplus_ver_from_ver
                    warning("Invalid EnergyPlus location in 'eplus_dir'\n.",
                            "The EnergyPlus V", eplus_ver_from_ver,
                            " indicated by 'eplus_ver' will be used:\n\n",
                            "EnergyPlus V", eplus_ver_used, " located at '", eplus_dir_used, "'.",
                            call. = FALSE)
                }
            # }}}4
            # Sub Case 2. If the given 'eplus_ver' is not valid{{{4
            } else {
                # Sub Sub Case 1. If the given 'eplus_dir' is valid{{{5
                if (check_energyplus(eplus_dir)) {
                    # Get the version from 'eplus_dir'
                    eplus_dir_used <- eplus_dir
                    eplus_ver_used <- get_idd_ver(eplus_dir)
                    msg_head <- paste0("Both 'eplus_dir' and 'eplus_ver' are given.\nBut could not find the EnergyPlus V", full_eplus_ver, " indicated by 'eplus_ver'.\n")
                    msg_ver <- paste0("EnergyPlus V", eplus_ver_used, " indicated by 'eplus_dir' will be used:\n\n")
                    warning(msg_head, msg_ver,
                            "EnergyPlus V", eplus_ver_used, " located at '", eplus_dir_used, "'.",
                            call. = FALSE)
                # }}}5
                # Sub Sub Case 2. If the given 'eplus_dir' is not valid{{{5
                } else {
                    # If no EnergyPlus location was found when 'eplusr' loaded, i.e.
                    # getOption('eplusr.eplus_dir') is "".
                    if (default_eplus_dir == "") {
                        stop("Both 'eplus_dir' and 'eplus_ver' are invalid.\nCould not detect valid EnergyPlus locations automatically.\nPlease change the option 'eplusr.eplus_dir', or give a valid EnergyPlus location in 'eplus_dir'.", call. = FALSE)
                    } else {
                        eplus_dir_used <- default_eplus_dir
                        eplus_ver_used <- get_idd_ver(default_eplus_dir)
                        warning("Both 'eplus_dir' and 'eplus_ver' are invalid.\nThe default EnergyPlus V", eplus_ver_used, " located at '", eplus_dir_used, "' will be used.", call. = FALSE)
                    }
                }
                # }}}5
            }
            # }}}4
        }
        # }}}3
    # }}}2
    # If 'eplus_ver' is not given{{{2
    } else {
        # Case 1. And 'eplus_dir' is not given neither{{{3
        if (is.null(eplus_dir)) {
            # If no EnergyPlus location was found when 'eplusr' loaded, i.e.
            # getOption('eplusr.eplus_dir') is "".
            if (default_eplus_dir == "") {
                stop("'eplusr' could not detect valid EnergyPlus locations automatically.\nPlease change the option 'eplusr.eplus_dir', or give a valid EnergyPlus location in 'eplus_dir'.", call. = FALSE)
            } else {
                eplus_dir_used <- default_eplus_dir
                eplus_ver_used <- get_idd_ver(default_eplus_dir)
                message("The default EnergyPlus V", eplus_ver_used,
                        " located at '", eplus_dir_used, "' will be used.")
            }
        # }}}3
        # Case 2. But 'eplus_dir' is given{{{3
        } else {
            # Sub Case 1. But is not valid, use the default location{{{4
            if (!check_energyplus(eplus_dir)) {
                if (default_eplus_dir != "") {
                    eplus_dir_used <- default_eplus_dir
                    eplus_ver_used <- get_idd_ver(default_eplus_dir)
                    warning("Invalid EnergyPlus path. The default EnergyPlus V", eplus_ver_used,
                            " located at '", eplus_dir_used, "' will be used.", call. = FALSE)
                } else {
                    stop("'eplusr' could not detect valid EnergyPlus locations automatically.\nPlease change the option 'eplusr.eplus_dir', or give a valid EnergyPlus location in 'eplus_dir'.", call. = FALSE)
                }
            # }}}4
            # Sub Case 2. And is valid{{{4
            } else {
                eplus_dir_used <- eplus_dir
                eplus_ver_used <- get_idd_ver(eplus_dir)
            }
            # }}}4
        }
        # }}}3
    }
    # }}}2
    return(list(eplus_dir = file_path(eplus_dir_used), eplus_ver = eplus_ver_used))
}
# }}}1
# create_eplus_ini {{{1
create_eplus_ini <- function (eplus_dir, working_dir) {
    eplus_dir_win <- paste0(gsub("/", "\\\\", eplus_dir), "\\")
    eplus_ini <- file_path(working_dir, "Energy+.ini")
    if (file.exists(eplus_ini)) {
        unlink(eplus_ini, force = TRUE)
    }
    file.create(eplus_ini)
    writeLines(text = paste0("[Program]\ndir=", eplus_dir_win),
               con = eplus_ini)
}
# }}}1
# get_external_file {{{1
get_external_file <- function (idf_lines) {
    schedule_files <- stringr::str_subset(idf_lines, "!\\s*-\\s*File Name$")
    # Check if the lines are commented
    idx_comment <- str_detect(stringr::str_trim(schedule_files), "^!")
    files <- unique(stringr::str_extract(schedule_files[!idx_comment], "^.*(?=,)"))
    return(files)
}
# }}}1
# copy_external_file{{{1
copy_external_file <- function (idf_lines, output_dir) {
    files <- get_external_file(idf_lines)
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }
    purrr::walk(files,
        ~{flag <- file.copy(from = .x, to = file_path(output_dir, basename(.x)),
                            overwrite = TRUE, copy.date = TRUE)
          if (!flag) {
              warning("Cannot copy file '", basename(.x), "' to path ", output_dir,
                      call. = FALSE)
          }
         }
    )
}
# }}}1
# get_default_epw {{{1
get_default_epw <- function (weather = NULL) {
    if (!is.null(weather)) return(weather)
    eplus_dir <- getOption("eplusr.eplus_dir")
    weather_dir <- file_path(eplus_dir, "WeatherData")
    epws <- list.files(weather_dir, pattern = "\\.epw", full.names = FALSE)
    if (identical(length(epws), 0L)) stop("No weather file found.", call. = FALSE)
    chicago_epw <- grep("Chicago", epws, value = TRUE)
    if (identical(length(chicago_epw), 0L)) {
        default_epw <- chicago_epw
    } else {
        default_epw <- epws[1]
    }

    warning("Missing weather input, weather file ", sQuote(default_epw),
            " located at ", sQuote(weather_dir), " will been used.",
            call. = FALSE)

    epw_path <- file_path(weather_dir, default_epw)
    return(epw_path)
}
# }}}1
# change_output_dir {{{1
change_output_dir <- function (cmd_run, output_dir) {
    run_bat <- gsub("(--output-directory ).*( --output-prefix)",
                    paste0('\\1"', gsub("\\\\", "\\\\\\\\", file_path(output_dir)), '"\\2'),
                    cmd_run)
    return(run_bat)
}
# }}}1

#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom stringr str_length str_interp
#' @importFrom purrr map_lgl map_chr walk walk2 map keep negate flatten_chr
#' @importFrom readr read_lines write_lines
#' @export
# run_multi
# run_multi {{{1
run_multi <- function (models, weathers, cores = NULL,
                       output_dirs = NULL, output_prefixes = NULL,
                       output_suffix = c("C", "L", "D"), special_run = NULL,
                       eplus_ver = NULL, eplus_dir = NULL, show_msg = TRUE) {
    # Get tempdir {{{2
    temp_dir <- getOption("eplusr.temp_dir")
    if (!dir.exists(temp_dir)) {
        dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
    }
    # }}}2
    # Get 'cores' {{{2
    # Get total job number.
    n_jobs <- length(models)
    cores <- cores %||% getOption("eplusr.parallel_num")
    # If the total job number is smaller than the core specified, set the core
    # equal to total job number.
    if (cores > n_jobs) {
        cores <- n_jobs
    }
    # }}}2
    # 'eplus_dir' and 'eplus_ver' checking {{{2
    eplus_dir_ver <- check_eplus_dir_ver(eplus_dir, eplus_ver)
    eplus_dir <- eplus_dir_ver[["eplus_dir"]]
    eplus_ver <- eplus_dir_ver[["eplus_ver"]]
    # }}}2
    # Get default value of 'weathers', 'output_dirs', 'output_prefixes' and 'output_suffix' {{{2
    weathers <- weathers %||% get_default_epw()
    output_dirs <- output_dirs %||% dirname(models)
    output_prefixes <- output_prefixes %||% file_prefix(models)
    output_suffix <- rlang::arg_match(output_suffix)
    # }}}2
    # 'weathers', 'output_dirs' and 'output_prefixes' length checking {{{2
    if (!is.null(weathers)) {
        assertthat::assert_that(assertthat::is.string(weathers) ||
                                assertthat::are_equal(n_jobs, length(weathers)))
    }
    if (!is.null(output_dirs)) {
        assertthat::assert_that(assertthat::is.string(output_dirs) ||
                                assertthat::are_equal(n_jobs, length(output_dirs)))
    }
    if (!is.null(output_prefixes)) {
        assertthat::assert_that(assertthat::is.string(output_prefixes) ||
                                assertthat::are_equal(n_jobs, length(output_prefixes)))
    }
    # }}}2
    # Combine all inputs as a tbl {{{2
    inputs <- dplyr::tibble(model = models, weather = weathers,
                            output_dir = output_dirs,
                            output_prefix = output_prefixes)
    # }}}2
    # Command heading and titles {{{2
    cmd_head <- "cmd.exe /c @ECHO OFF &&"
    # Title
    seq <- stringr::str_pad(1:n_jobs, nchar(n_jobs), side = "left", pad = "0")
    cmd_title <- paste0("Simulation in progress [", seq, "/", n_jobs, "]")
    # }}}2
    # Get run commands per model{{{2
    job_infos <- purrr::map(seq(1:nrow(inputs)),
       ~{
            input <- inputs[.x,]
            title <- cmd_title[.x]
            pre_proc(model = input[["model"]], weather = input[["weather"]],
                     output_dir = input[["output_dir"]],
                     output_prefix = input[["output_prefix"]],
                     output_suffix = output_suffix,
                     eplus_dir = eplus_dir, pause = FALSE,
                     start_title = title, finish_title = title)
        }
    )
    job_infos <- purrr::transpose(job_infos)
    cmd_jobs <- job_infos[["cmd_job"]]
    cmd_runs <- job_infos[["cmd_run"]]
    output_dirs <- job_infos[["output_dir"]]
    output_prefixes <- job_infos[["output_prefix"]]
    # }}}2
    # Divide jobs according to total core number {{{2
    divided_jobs <- divide_jobs(cores, cmd_jobs, append = "EXIT")
    # }}}2
    # Write 'run.bat' and 'multi_run_*.bat' {{{2
    # Write 'run.bat' to output dir
    path_run_bats <- file_path(output_dirs, "run.bat")
    purrr::walk2(cmd_runs, path_run_bats, ~readr::write_lines(.x, .y))
    # Write 'multi_run_*.bat' to tempdir
    path_multi_run_bats <- file_path(temp_dir, paste0("multi_run_", seq(1:cores), ".bat"))
    purrr::walk2(divided_jobs, path_multi_run_bats, ~readr::write_lines(.x, .y))
    # }}}2
    # Run the job with multithreading {{{2
    cmd_starts <- paste0(cmd_head, " START ", path_multi_run_bats)
    status <- purrr::map_int(cmd_starts, ~system(command = .x, wait = FALSE, invisible = FALSE))
    if (any(status != 0L)) {
        stop("Error occured when running commands", call. = FALSE)
    }
    # }}}2
    # Info message {{{2
    if (show_msg) {
        msg_head <- paste0(
            "The job has been successfully executed using EnergyPlus V",
            eplus_ver, " located at ", sQuote(eplus_dir), ".\n----------\n")

        msg_job <- paste0("[Job ", seq, "]:\n",
                          "Model: ", file_path(models), "\n",
                          "Weather: ", file_path(weathers), "\n----------\n")
        message(msg_head, msg_job)
    }
    # }}}2
}
# }}}1
# divide_job {{{1
divide_jobs <- function (cores, jobs, append = NULL) {
    id_cores <- seq(1:cores)
    divided_jobs <- purrr::map(id_cores, ~divide_jobs_to_core(.x, jobs))
    if (!is.null(append)) {
        divided_jobs <- purrr::map(divided_jobs, base::append, values = append)
    }

    return(divided_jobs)
}

divide_jobs_to_core <- function (id_core, jobs) {
    id_jobs <- seq_along(jobs)
    n_jobs <- length(jobs)
    job_divided <- purrr::map(id_jobs,
        ~assign_job_to_core(id_job = .x, id_core = id_core,
            job = jobs[[.x]], n_jobs = n_jobs
        )
    )
    job_per_core <- purrr::discard(job_divided, is.null);
    job_per_core <- purrr::flatten_chr(job_per_core)

    return(job_per_core)
}

assign_job_to_core <- function (id_job, id_core, job, n_jobs) {
    if (identical(id_job %% n_jobs + 1L, id_core)) {
        job
    } else {
        NULL
    }
}
# }}}1

# validate_job {{{1
validate_job <- function (job, type = c("epg", "jeplus", "epat")) {
    type <- rlang::arg_match(type)
    val_job <- switch(type,
        epg = validate_epg(job),
        jeplus = validate_jeplus(job),
        epat = validate_epat(job)
    )

    return(val_job)
}
# }}}1

#' @importFrom purrr walk2
#' @importFrom stringr str_replace_all
# create_param
# {{{1
create_param <- function (idf_path, param_name, param_field, param_value) {
    con <- environment()
    idf_raw <- read_idf(idf_path, parse = FALSE)

    purrr::walk(param_name,
                ~{purrr::walk2(param_field, param_value,
                               ~{assign("idf_raw", stringr::str_replace_all(idf_raw, .x, .y), envir = con)})})

    idf_ver <- get_idf_ver(idf_raw)
    attrs <- list(ver = idf_ver, type = "string")
    idf_raw <- add_attrs(idf_raw, attrs)

    return(idf_raw)
}
# }}}1

#' @importFrom purrr flatten_chr map set_names cross_n
#' @importFrom data.table setcolorder rbindlist
#' @importFrom readr write_lines write_csv
# create_job
# {{{1
create_job <- function (param_tbl) {
    all_names <- names(param_tbl)
    is_param_tbl <- all(match("idf_path", all_names),
                        match("weather_path", all_names),
                        match("param_field", all_names),
                        match("param_value", all_names))

    if (!is_param_tbl) {
        stop("Invalid parametric table.", call. = FALSE)
    }

    idf_path <- param_tbl[["idf_path"]]
    idf_name <- basename(idf_path)
    weather_path <- param_tbl[["weather_path"]]
    weather_name <- basename(weather_path)
    param_name <- names(param_tbl[["param_field"]])
    param_field <- purrr::flatten_chr(param_tbl[["param_field"]])
    param_value <- purrr::map(param_tbl[["param_value"]], purrr::flatten_chr)

    # Create a txt progress bar
    p <- dplyr::progress_estimated(length(param_value))

    # Create idf/imfs and save it into tempdir.
    # Create tempdir for eplus package
    temp_dir <- getOption("eplusr.temp_dir")
    if (!dir.exists(temp_dir)) {
        dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
    }

    # TODO: Find a functionial way to determine the file extension.
    file_name <- file_path(temp_dir, paste0(names(param_value), ".imf"))
    file_name <- normalizePath(file_name, mustWork = FALSE)
    param_idfs <-
        purrr::map(seq_along(param_value),
                   ~{idf <- create_param(idf_path = idf_path,
                                         param_name = param_name,
                                         param_field = param_field,
                                         param_value = param_value[[.x]])
                     p$tick()$print();idf})
    p$stop()
    param_idfs <- purrr::set_names(param_idfs, names(param_value))
    purrr::walk2(param_idfs, file_name, ~readr::write_lines(x = .x, path = .y))

    # Creat job info
    combinations <- purrr::cross_n(list(names(param_value), idf_name, weather_path))
    job <-
        purrr::map(seq_along(combinations),
                   ~{input_info <- combinations[[.x]]
                     case_name <- input_info[[1]]
                     field_values <- param_value[[case_name]]
                     info <- append(combinations[[.x]], field_values)
                   })
    # Convert the job info into a data.table
    job <- dplyr::as_tibble(data.table::rbindlist(job))
    # Add job number
    job <- dplyr::mutate(job, `#` = seq_along(V1))
    job <- dplyr::select(job, dplyr::one_of("#"), dplyr::everything())
    # Set names as jEPlus style.
    job <- purrr::set_names(job, c("#", "model_case", "model_template", "weather", param_field))
    job_index <- dplyr::mutate(job, weather = basename(weather))
    job <- dplyr::transmute(job, model = normalizePath(file_path(temp_dir, paste0(model_case, ".imf")), mustWork = TRUE), weather)

    job_return <- list(job = job, job_index = job_index)
    return(job_return)
}
# }}}1

#' @export
# run_jeplus{{{1
run_jeplus <- function(jeplus, case_name = NULL, output_dir = NULL, n = NULL,
                       eplus_ver = NULL, eplus_dir = NULL) {

    # Check 'jeplus'{{{2
    if (is.character(jeplus)) {
        if (!file.exists(jeplus)) {
            stop("Input 'jeplus' file does not exists.", call. = FALSE)
        } else {
            if (tools::file_ext(jeplus) == "json"){
                jeplus <- read_jeplus(jeplus)
            } else {
                stop("Input 'jeplus' file should have an extension of 'json'.",
                     call. = FALSE)
            }
        }
    } else {
        if (!check_jeplus(jeplus = jeplus)) {
            stop("'jeplus' should be either a jEPlus .json project, or a job imported using 'read_jeplus'.",
                 call. = FALSE)
        }
    }
    # }}}2

    # Get job{{{2
    job_info <- create_job(param_tbl = jeplus)
    job <- job_info[["job"]]
    job_index <- job_info[["job_index"]]
    # }}}2

    # Other arg check{{{2
    if (is.null(output_dir)) {
        output_dir <- dirname(jeplus[["idf_path"]])
    }
    if (!is.null(case_name)) {
        output_dir <- file_path(output_dir, case_name)
    }
    if (is.null(eplus_dir)) {
        eplus_dir <- epat[["eplus_path"]]
    }
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    }
    # }}}2

    # Write 'job_index.csv'.
    readr::write_csv(job_index, file_path(output_dir, "job_index.csv"))

    # Run
    run_multi(models = job[["model"]], weathers = job[["weather"]],
              output_dirs = output_dir, output_prefixes = job_index[["model_case"]],
              n = n, eplus_ver = eplus_ver, eplus_dir = eplus_dir)
}
# }}}1

# check_jeplus{{{1
check_jeplus <- function (jeplus) {
    if (!is.list(jeplus)) return(FALSE)
    type <- attr(jeplus, "job_type")
    if (type != "jeplus") return(FALSE)
    var_names <- names(jeplus)
    required <- c("execSettings", "parameters", "rvx", "projectType",
                  "weatherDir", "idfdir", "weatherFile", "projectID",
                  "projectNotes", "idftemplate", "rvxFile", "paramFile")
    ex_vars <- setdiff(var_names, required)
    mis_vars <- setdiff(required, var_names)
    if (all(length(ex_vars) == 0L, length(mis_vars) == 0L)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}1
