################################################################################
#                                Run EnergyPlus                                #
################################################################################

#' @importFrom purrr map_chr
#' @importFrom stringr str_replace
#' @importFrom readr read_lines
# get_idd_ver: A function that gets the versions of EnergyPlus.
# get_idd_ver
# {{{1
get_idd_ver <- function(eplus_dir){
    idd <- file.path(eplus_dir, "Energy+.idd")
    idd_ver <- purrr::map_chr(idd, ~stringr::str_replace(readr::read_lines(.x, n_max = 1), "!IDD_Version ", ""))
    return(idd_ver)
}
# }}}1

# check_energyplus
# check_energyplus{{{1
check_energyplus <- function (eplus_dir) {
    os <- Sys.info()['sysname']
    if (os == "Windows") {
        energyplus <- normalizePath(file.path(eplus_dir, "energyplus.exe"), mustWork = FALSE)
    } else {
        energyplus <- normalizePath(file.path(eplus_dir, "energyplus"), mustWork = FALSE)
    }
    idd <- normalizePath(file.path(eplus_dir, "Energy+.idd"), mustWork = FALSE)

    purrr::map2_lgl(energyplus, idd,
                   ~{
                        if (all(file.exists(.x, .y))) {
                            return(TRUE)
                        } else {
                            return(FALSE)
                        }
                   })
}
# }}}1

# get_volumes: A function to get all disks (borrowed from 'getVolumes' function
# in 'shinyFiles' package).
# get_volumes{{{1
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
    volumes
}
# }}}1

#' Find EnergyPlus locations on current computer.
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
#' @examples
#' find_eplus()
# find_eplus
# {{{1
find_eplus <- function(ver = NULL, verbose = TRUE){
    # Define searching paths.
    # 1. check if drives exist.
    disks <- get_volumes()
    # 2. search EnergyPlus in drive root path and program file folders.
    os <- Sys.info()['sysname']
    if (os == "Windows") {
        path_list <- c(paste0(disks, "/"),
                       file.path(disks, "Program Files"),
                       file.path(disks, "Program Files (x86)"))
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

# create_eplus_ini: A function to create an "Energy+.ini" file per working
# direcotry.
# create_eplus_ini
# {{{1
create_eplus_ini <- function (eplus_dir, working_dir) {
    eplus_dir_win <- paste0(gsub(x=eplus_dir, "/", "\\\\"), "\\")
    eplus_ini <- file.path(working_dir, "Energy+.ini")
    if (file.exists(eplus_ini)) {
        unlink(eplus_ini, force = TRUE)
    }
    file.create(eplus_ini)
    writeLines(text = paste0("[Program]\ndir=", eplus_dir_win),
               con = eplus_ini)
}
# }}}1

#' @importFrom stringr str_subset str_extract
# get_external_fill: A helper function to get file paths in 'Schedule:File'.
# get_external_fill
# {{{1
get_external_file <- function (idf_lines) {
    schedule_files <- stringr::str_subset(idf_lines, "!\\s*-\\s*File Name$")
    files <- unique(stringr::str_extract(schedule_files, "^.*(?=,)"))
    return(files)
}
# }}}1

#' @importFrom purrr walk
# copy_external_file: A helper function to copy files used in 'Schedule:File' to
# output dir.
# copy_external_file
# {{{1
copy_external_file <- function (idf_lines, output_dir) {
    files <- get_external_file(idf_lines)
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }
    purrr::walk(files,
        ~{flag <- file.copy(from = .x, to = file.path(output_dir, basename(.x)),
                            overwrite = TRUE, copy.date = TRUE)
          if (!flag) {
              warning("Cannot copy file '", basename(.x), "' to path ", output_dir,
                      call. = FALSE)
          }
         }
    )
}
# }}}1

# Epl_run_bat: A wrapper function of "Epl-run.bat" distributed with EnergyPlus.
# Epl_run_bat
# {{{1
Epl_run_bat <- function(eplus_dir = find_eplus(),
                        eplus_in, eplus_out, eplus_in_ext, eplus_wthr,
                        eplus_type, pausing, max_col, conv_eso, csv,
                        echo = FALSE){
    ############################################################################
    #                            Notes on arguments                            #
    ############################################################################
    # 0. eplus_dir
    # EnergyPlus location.

    # 1. eplus_in
    # Contains the file with full path and no extensions for input files.

    # 2. eplus_out
    # Contains the file with full path and no extensions for output files.

    # 3. eplus_in_ext
    # Contains the extension of the file selected from the EP-Launch program.
    # Could be imf or idf -- having this parameter ensures that the correct user
    # selected file will be used in the run.

    # 4. eplus_wthr
    # Contains the file with full path and extension for the weather file.

    # 5. eplus_type
    # Contains either "EP" or "NONE" to indicate if a weather file is used.

    # 6. pausing
    # Contains Y if pause should occur between major portions of batch file. Any
    # other character or word except "Y" will keep EnergyPlus running without
    # pausing.

    # 7. max_col
    # Contains "250" if limited to 250 columns otherwise contains "nolimit" if
    # unlimited (used when calling readVarsESO).

    # 8. conv_eso
    # Contains Y if convertESOMTR program should be called to convert all SI
    # units into IP (Inch-Pound) units. Any other character or word except "Y"
    # will skip this process.

    # 9. csv
    # Contains Y if csvProc program should be called to generate CSV files from
    # "ESO" and "MTR" files or not. Any other character or word except "Y" will
    # skip this process.
    ############################################################################

    # In order to chain commands, this has to be used before commands.
    cmd_head <- "cmd.exe /c"

    # Use "Epl-run.bat" to run EnergyPlus rather than "RunEPlus.bat".
    eplus_bat <- file.path(eplus_dir, "Epl-run.bat")

    # In case there are spaces in user input, quote all pathes.
    # eplus_in <- paste0('"', eplus_in, '"')
    # eplus_out <- paste0('"', eplus_out, '"')
    eplus_wthr <- paste0('"', eplus_wthr, '"')

    # Check if extensions are given.
    check_ext_in <- length(grep(x=basename(eplus_in), "(\\.imf$)|(\\.idf$)",
                                 ignore.case = TRUE)) == 0L
    if (!check_ext_in) {
        stop("Argument 'eplus_in' should be the path of input files WITHOUT ",
             "extensions.")
    }

    check_ext_out <- length(grep(x=basename(eplus_out), "(\\.imf$)|(\\.idf$)",
                                 ignore.case = TRUE)) == 0L
    if (!check_ext_out) {
        stop("Argument 'eplus_out' should be the path of input files WITHOUT ",
             "extensions.")
    }

    # Get the working directory.
    working_dir <- dirname(eplus_in)
    create_eplus_ini(eplus_dir = eplus_dir, working_dir = working_dir)

    command <- paste(cmd_head, "cd", working_dir, "&&",
                     eplus_bat,
                     eplus_in, eplus_out, eplus_in_ext, eplus_wthr,
                     eplus_type, pausing, max_col, conv_eso, csv,
                     sep = " ")

    if (echo) {
        # eplus_stdout <- exec_wait(cmd = command)
        # eplus_stdout <- system2(command = command, wait = TRUE)
        eplus_stdout <- system(command = command, wait = TRUE)
        return(eplus_stdout)

    } else {
        eplus_stdout <- system(command = command, wait = FALSE, invisible = FALSE)

        message("EnergyPlus has been successfully execuated in background.")

        return(invisible())
    }

}
# }}}1

#' @importFrom tools file_path_sans_ext
#' @importFrom stringr str_interp
#' @importFrom readr write_lines
# energyplus_exe: A wrapper function of EnergyPlus command line interface.
# energyplus_exe
#  {{{1
energyplus_exe <- function (eplus_dir = find_eplus(),
                            idf, weather, output_dir,
                            output_prefix = "eplus", output_suffix = "L",
                            epmacro = TRUE, expand_obj = TRUE, readvars = TRUE,
                            annual = FALSE, design_day = FALSE, idd = NULL,
                            legacy = FALSE, echo = FALSE) {
    ############################################################################
    #                            Notes on arguments                            #
    ############################################################################
    # 1. idf
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

    # In order to chain commands, this has to be used before commands.
    cmd_head <- "cmd.exe /c"

    # Use "energyplus.exe" to run EnergyPlus.
    energyplus_exe <- file.path(eplus_dir, "energyplus.exe")

    # Get the working directory.
    working_dir <- dirname(idf)
    idf_name <- tools::file_path_sans_ext(basename(idf))
    # Create an "Energy+.ini" file and copy it to the current working directory.
    create_eplus_ini(eplus_dir = eplus_dir, working_dir = working_dir)

    # In case there are spaces in user input, quote all pathes.
    idf <- paste0('"', idf, '"')
    weather <- paste0('"', weather, '"')
    output_dir <- paste0('"', output_dir, '"')
    # output_prefix <- paste0('"', output_prefix, '"')
    # output_suffix <- paste0('"', output_suffix, '"')

    # Get the right format of the input command to EnergyPlus.
    # NOTE: `ifelse` function cannot return NULL.
    if (epmacro) epmacro <- "--epmacro" else epmacro <- NULL
    if (expand_obj) expand_obj <- "--expandobjects" else expand_obj <- NULL
    if (readvars) readvars <- "--readvars" else readvars <- NULL
    if (annual) annual <- "--annual" else annual <- NULL
    if (design_day) design_day <- "--design-day" else design_day <- NULL
    if (!is.null(idd)) idd <- paste0("--idd", '"', idd, '"') else idd <- NULL

    # Delete "fort.6" and "Energy+.ini" created by "EnergyPlus.exe" and
    # "readvars.audit" created by "ReadVarsESO.exe" after simulations.
    del_list <- c("fort.6", "Energy+.ini", "readvars.audit")
    del_cmd <- paste("IF EXIST", del_list, "DEL", del_list)
    post_proc <- c("@ECHO OFF", del_cmd)
    if (!is.null(epmacro)) {
        rename_imf <- stringr::str_interp("IF EXIST out.idf MOVE out.idf ${idf_name}.idf")
        post_proc <- c(post_proc, rename_imf)
    }
    readr::write_lines(post_proc, "post_proc.bat")

    # Show a cmd window when the simulation finishs.
    msg <- paste0("type ", output_prefix, ".end && PAUSE")

    command <- paste(cmd_head, "cd", working_dir, "&&",
                     energyplus_exe,
                     "--weather", weather,
                     "--output-directory", output_dir,
                     "--output-prefix", output_prefix,
                     "--output-suffix", output_suffix,
                     epmacro, expand_obj, readvars, annual, design_day, idd, idf,
                     "&& post_proc.bat && DEL post_proc.bat &&", msg,
                     sep = " ")

    if (echo) {
        # eplus_stdout <- exec_wait(cmd = command)
        # eplus_stdout <- system2(command = command, wait = TRUE)
        eplus_stdout <- system(command = command, wait = TRUE)
        return(eplus_stdout)

    } else {
        eplus_stdout <- system(command = command, wait = FALSE, invisible = FALSE)

        message("EnergyPlus has been successfully execuated in background.")

        return(invisible())
    }
}
# }}}1

#' @importFrom readr read_lines
#' @importFrom stringr str_interp
#' @importFrom tools file_path_sans_ext file_ext
#' @export
# run_eplus: A function to run EnergyPlus in R.
# run_eplus
# {{{1
run_eplus <- function (input, weather, output_dir = NULL, output_prefix = NULL,
                       csv = TRUE, echo = FALSE, run = c("exe", "bat"),
                       eplus_dir = NULL, eplus_ver = NULL) {

    # Backup the original working directory.
    ori_wd <- getwd()
    # Set the working directory as the same folder of input file.
    setwd(dirname(input))
    # Avoid case sensitive match in `identical`.
    input <- normalizePath(input, winslash = "/", mustWork = FALSE)
    weather <- normalizePath(weather, winslash = "/", mustWork = FALSE)
    input_lines <- read_idf_lines(input)
    run <- rlang::arg_match(run)

    # 'eplus_dir' and 'eplus_ver' checking
    # {{{2
    eplus_dir_ver <- check_eplus_dir_ver(eplus_dir, eplus_ver)
    eplus_dir <- eplus_dir_ver[["eplus_dir"]]
    eplus_ver <- eplus_dir_ver[["eplus_ver"]]
    energyplus <- normalizePath(file.path(eplus_dir, "energyplus.exe"), mustWork = TRUE)
    # }}}2

    # File existing checking
    # {{{2
    if (!file.exists(input)) {
        stop("Input model file does not exist. Please check input.", call. = FALSE)
    }
    if (!file.exists(weather)) {
        stop("Weather file does not exist. Please check input.", call. = FALSE)
    }
    # }}}2

    # Input file version and EnergyPlus verion match checking
    # {{{2
    idf_ver <- paste0(get_idf_ver(input_lines), ".0")
    if (!identical(idf_ver, eplus_ver)) {
        warning(glue::glue("The input model {input} indicates an EnergyPlus verion of {idf_ver} but is simulated using EnergyPlus {eplus_ver}. Unexpected results may occur. It is recommended to use 'IDFVersionUpdater' distributed with EnergyPlus before simulation."), call. = FALSE)
    }
    # }}}2

    # File extension checking
    # {{{2
    input_prefix <- tools::file_path_sans_ext(basename(input))
    eplus_in_ext <- tools::file_ext(input)
    wthr_prefix <- tools::file_path_sans_ext(basename(weather))
    eplus_wthr_ext <- tools::file_ext(weather)
    if (!grepl(x = eplus_in_ext, pattern = "i[dm]f", ignore.case = TRUE)) {
       stop("'input' should be a full file path with an extension of '.idf' or '.imf'.")
    }
    if (!grepl(x = eplus_wthr_ext, pattern = "epw", ignore.case = TRUE)) {
       stop("'weather' should be a full file path with an extension of '.epw'.")
    }
    # }}}2

    # Input file renaming
    # {{{2
    # If output directory is given,
    if (!is.null(output_dir)) {
        output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
        if (!dir.exists(output_dir)) {
            output_dir_flag <- dir.create(output_dir, showWarnings = TRUE, recursive = TRUE)
            if(!output_dir_flag) {
                stop("Could not find or create the specified output direcotry.", call. = FALSE)
            }
        }

        # (a) and also is output prefix.
        if (!is.null(output_prefix)) {
            new_name_idf <- file.path(output_dir, paste0(output_prefix, ".", eplus_in_ext))
            new_name_wthr <- file.path(output_dir, paste0(wthr_prefix, ".", eplus_wthr_ext))

        # (b) but output_prefix is not given.
        } else {
            new_name_idf <- file.path(output_dir, paste0(input_prefix, ".", eplus_in_ext))
            new_name_wthr <- file.path(output_dir, paste0(wthr_prefix, ".", eplus_wthr_ext))
        }
    # If output directory is not given,
    } else {
        # (a) but output prefix is given
        if (!is.null(output_prefix)) {
            new_name_idf <- file.path(dirname(input), paste0(output_prefix, ".", eplus_in_ext))
            new_name_wthr <- file.path(dirname(input), paste0(wthr_prefix, ".", eplus_wthr_ext))

        # (b) neither is output prefix.
        } else {
            new_name_idf <- file.path(dirname(input), paste0(input_prefix, ".", eplus_in_ext))
            new_name_wthr <- file.path(dirname(input), paste0(wthr_prefix, ".", eplus_wthr_ext))
        }
    }
    if (!identical(new_name_idf, input)) {
        # Copy the input file into the working directory.
        file.copy(from = input, to = new_name_idf, overwrite = TRUE,
                  copy.mode = TRUE, copy.date = TRUE)
    }
    if (!identical(new_name_wthr, weather)) {
        # Copy the weather file into the working directory after renaming it.
        file.copy(from = weather, to = new_name_wthr, overwrite = TRUE,
                  copy.mode = TRUE, copy.date = TRUE)
    }
    # }}}2

    # Use Epl_run_bat, i.e. "Epl-run.bat", to run EnergyPlus.
    # {{{2
    if (run == "bat") {
        eplus_in <- gsub(x=input, "\\.i[dm]f$", "", ignore.case = TRUE)

        if (is.null(output_dir)) {
            eplus_out <- eplus_in
        } else {
            eplus_out <- file.path(output_dir, output_prefix)
        }

        # Copy files used in 'Schedule:File' to output dir.
        # {{{3
        copy_external_file(input_lines, output_dir = eplus_out)
        # }}}3

        eplus_wthr <- weather

        # Who will run EnergyPlus Without weather file?
        use_weather <- TRUE
        eplus_type <- ifelse(use_weather, "EP", "NONE")

        # Other misc arguments.
        pausing <- "N"
        max_col <- "nolimit"
        # conv_eso <- ifelse(ip_unit, "Y", "N")
        conv_eso <- "N"
        csv <- ifelse(csv, "Y", "N")

        Epl_run_bat(eplus_dir = eplus_dir,
                    eplus_in = eplus_in, eplus_out = eplus_out,
                    eplus_in_ext = eplus_in_ext, eplus_wthr = eplus_wthr,
                    eplus_type = eplus_type, pausing = pausing,
                    max_col = max_col, conv_eso = conv_eso, csv = csv,
                    echo = echo)
    # }}}2

    # Use energyplus, i.e. "energyplus.exe", to run EnergyPlus.
    # {{{2
    } else {
        idf <- input
        if (is.null(output_dir)) {
            output_dir <- dirname(input)
        }

        # Copy files used in 'Schedule:File' to output dir.
        # {{{3
        copy_external_file(input_lines, output_dir = output_dir)
        # }}}3

        if (is.null(output_prefix)) {
            output_prefix <- tools::file_path_sans_ext(basename(input))
        }

        epmacro <- ifelse(identical(tools::file_ext(input), "imf"), TRUE, FALSE)

        output_suffix <- "C"
        readvars <- ifelse(csv, TRUE, FALSE)

        energyplus_exe(eplus_dir = eplus_dir,
                       idf = idf, weather = weather, output_dir = output_dir,
                       output_prefix = output_prefix,
                       output_suffix = output_suffix,
                       epmacro = epmacro, expand_obj = TRUE, readvars = readvars,
                       annual = FALSE, design_day = FALSE, idd = NULL,
                       legacy = FALSE, echo = echo)
    }
    # }}}2

    # Set the woring directory back.
    setwd(ori_wd)
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
    file_name <- file.path(temp_dir, paste0(names(param_value), ".imf"))
    file_name <- normalizePath(file_name, winslash = "/", mustWork = FALSE)
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
    job <- dplyr::transmute(job, model = normalizePath(file.path(temp_dir, paste0(model_case, ".imf")), mustWork = TRUE), weather)

    job_return <- list(job = job, job_index = job_index)
    return(job_return)
}
# }}}1

#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom stringr str_length str_interp
#' @importFrom purrr map_lgl map_chr walk walk2 map keep negate flatten_chr
#' @importFrom readr read_lines write_lines
#' @export
# run_multi
# {{{1
run_multi <- function (models, weathers, output_dirs, output_prefixes, case_name,
                       n = NULL, eplus_ver = NULL, eplus_dir = NULL) {

    # Get tempdir
    # {{{2
    temp_dir <- getOption("eplusr.temp_dir")
    if (dir.exists(temp_dir)) {
        dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
    }
    # }}}2

    # Get basic info of input files.
    # {{{2
    models <- normalizePath(models, winslash = "/", mustWork = FALSE)
    weathers <- normalizePath(weathers, winslash = "/", mustWork = FALSE)
    output_dirs <- normalizePath(output_dirs, winslash = "/", mustWork = FALSE)
    model_names <- tools::file_path_sans_ext(basename(models))
    weather_names <- tools::file_path_sans_ext(basename(weathers))
    model_exts <- tools::file_ext(models)
    weather_exts <- tools::file_ext(weathers)
    # }}}2

    # Get 'n'
    # {{{2
    # Get total job number.
    total <- length(models)
    if (is.null(n)) {
        n <- getOption("eplusr.parallel_num")
    }
    # If the total job number is smaller than the core specified, set the core
    # equal to total job number.
    if (n > total) {
        n <- total
    }
    # }}}2

    # File existing checking
    # {{{2
    idx_model <- any(!file.exists(models))
    idx_weather <- any(!file.exists(weathers))
    if (idx_model) {
        ori_error_len <- getOption("warning.length")
        missing_models <- models[idx_model]
        error_msg <- paste0("Input model file '", missing_models, "' does not exist. Please check input.", collapse = "\n")
        options("warning.length" = stringr::str_length(error_msg) + 1000)
        stop(error_msg, call. = FALSE)
        options("warning.length" = ori_error_len)
    }
    if (idx_weather) {
        ori_error_len <- getOption("warning.length")
        missing_weathers <- weathers[idx_weather]
        error_msg <- paste0("Input weather file '", missing_weathers, "' does not exist. Please check input.", collapse = "\n")
        options("warning.length" = stringr::str_length(error_msg) + 1000)
        stop(error_msg, call. = FALSE)
        options("warning.length" = ori_error_len)
    }
    # }}}2

    # Parametric field existence checking
    # {{{2
    idx_has_param <- purrr::map_lgl(models, ~{raw <- readr::read_lines(.x);is_param_exist(raw)})
    if (any(idx_has_param)) {
        param_idfs <- models[idx_has_param]
        param_list <- purrr::map_chr(param_idfs,
                                      ~{raw <- readr::read_lines(.x)
                                        params <- list_params(raw)
                                        paste(params, collapse = ", ")
                                       })
        info <- purrr::map2_chr(param_idfs, param_list,
                                ~stringr::str_interp("For ${basename(.x)}: ${.y}"))
        if (!is.na(match(type, c("jeplus", "epat")))) {
            msg <- stringr::str_interp("There are parametric fields which do not have values. Please check the .json project file and make sure every parametric field has a value string.\n")
        } else {
            msg <- map(param_idfs, ~stringr::str_interp("Model ${.x} has parametric fields which do not have values. Please clean the model before simulation.\n"))
        }
        msg_all <- c(msg, "\n", paste0(info, collapse = "\n"))
        stop(msg_all, call. = FALSE)
    }
    # }}}2

    # File extension checking
    # {{{2
    idx_model_exts <- grepl(x = model_exts, pattern = "i[dm]f", ignore.case = TRUE)
    idx_weather_exts <- grepl(x = weather_exts, pattern = "epw", ignore.case = TRUE)
    if (any(!idx_model_exts)) {
        msg_head <- "Model should have an extension of '.idf' or '.imf':\n"
        wrong_models <- models[!idx_model_exts]
        wrong_exts <- model_exts[!idx_model_exts]
        msg_info <-paste0("Model '", wrong_models, "' has an extenssion of '", wrong_exts, "'.",
                          collapse = "\n")
        stop(msg_head, msg_info, call. = FALSE)
    }
    if (any(!idx_weather_exts)) {
        msg_head <- "Weather should have an extension of '.epw':\n"
        wrong_weathers <- weathers[!idx_weather_exts]
        wrong_exts <- weather_exts[!idx_weather_exts]
        msg_info <-paste0("Weather '", wrong_weathers, " has an extenssion of '", wrong_exts, "'.",
                          collapse = "\n")
        stop(msg_head, msg_info, call. = FALSE)
    }
    # }}}2

    # 'eplus_dir' and 'eplus_ver' checking
    # {{{2
    eplus_dir_ver <- check_eplus_dir_ver(eplus_dir, eplus_ver)
    eplus_dir <- eplus_dir_ver[["eplus_dir"]]
    eplus_ver <- eplus_dir_ver[["eplus_ver"]]
    energyplus <- normalizePath(file.path(eplus_dir, "energyplus.exe"), mustWork = TRUE)
    # }}}2

    # Input file version and EnergyPlus verion match checking
    # {{{2
    idf_vers <- purrr::map_chr(models, ~paste0(get_idf_ver(readr::read_lines(.x)), ".0"))
    is_ver_matched <- purrr::map_lgl(idf_vers, ~{identical(.x, eplus_ver)})
    if (!all(is_ver_matched)) {
        if (identical(job_type, c("jeplus", "epat"))) {
            warning(stringr::str_interp("The input template model indicates an EnergyPlus verion of ${unique(idf_vers)} but is simulated using EnergyPlus ${eplus_ver}. Unexpected results may occur. It is recommended to use 'IDFVersionUpdater' distributed with EnergyPlus before simulation."), call. = FALSE)
        } else {
            un_matched_models <- models[!is_ver_matched]
            un_matched_vers <- idf_vers[!is_ver_matched]
            msg <- paste(paste0("Model:", un_matched_models, ", indicated version:", un_matched_vers), collapse = "\n")
            warning(stringr::str_interp("There are models indicating EnergyPlus versions which are different from the EnergyPlus version ${eplus_ver} used in simulation:\n"), msg, call. = FALSE)
        }
    }
    # }}}2

    # 'output_dirs' checking
    # {{{2
    if (!is.null(output_dirs)) {
        if (any(!is.character(output_dirs), all(length(output_dirs) != 1, length(output_dirs) != total))) {
            stop("'output_dirs' should be a character vector of length 1 or length equal to total job number.",
                 call. = FALSE)
        }
    }
    # }}}2

    # 'output_prefixes' checking
    # {{{2
    if (!is.null(output_prefixes)) {
        if (any(!is.character(output_prefixes), all(length(output_prefixes) != 1, length(output_prefixes) != total))) {
            stop("'output_prefixes' should be a character vector of length 1 or length equal to total job number.",
                 call. = FALSE)
        }
    }
    # }}}2

    # Create the parent output directory.
    # {{{2
    if (!is.null(output_dirs)) {
        # Get the full path of output_dirs if is given as a relative path.
        output_dirs <- normalizePath(file.path(output_dirs, output_prefixes), winslash = "/", mustWork = FALSE)
    } else {
        # set the parent output directory as current directory.
        output_dirs <- normalizePath(file.path(dirname(models), output_prefixes), winslash = "/", mustWork = FALSE)
    }
    # }}}2

    # Create output directories and 'Energy+.ini' per directory.
    # {{{2
    purrr::walk(output_dirs, ~{if (!dir.exists(.x)) dir.create(.x, recursive = TRUE);
                create_eplus_ini(eplus_dir = eplus_dir, working_dir = .x)})
    # }}}2

    # Clean output directory per case.
    # {{{2
    purrr::walk(output_dirs, clean_wd)
    # }}}2

    # Copy files used in 'Schedule:File' to output dir.
    # {{{2
    purrr::walk2(models, output_dirs, ~copy_external_file(read_idf_lines(.x), .y))
    # }}}2

    # Get new input file names.
    # {{{2
    if (length(output_prefixes) == 1) {
        output_prefixes <- rep(output_prefixes, total)
    }
    if (!is.null(output_prefixes)) {
        new_model_names <- file.path(output_dirs, paste0(output_prefixes, ".", model_exts))
        new_weather_names <- file.path(output_dirs, paste0(weather_names, ".", weather_exts))

    } else {
        output_prefixes <- model_names
        new_model_names <- file.path(output_dirs, paste0(model_names, ".", model_exts))
        new_weather_names <- file.path(output_dirs, paste0(weather_names, ".", weather_exts))
    }
    # }}}2

    # Copy and rename input files according to new names.
    # {{{2
    purrr::walk2(c(new_model_names, new_weather_names),
                 normalizePath(c(models, weathers), winslash = "/"),
                 function(new, old) {
                     if (!identical(new, old)) {
                         file.copy(from = old, to = new, overwrite = TRUE,
                                   copy.mode = TRUE, copy.date = TRUE)
                     }
                 })
    # }}}2

    # Command heading and titles
    # {{{2
    cmd_head <- "cmd.exe /c"
    # Change directory
    echo_off <- "@ECHO OFF"
    cmd_cd <- paste("CD", normalizePath(output_dirs), sep = " ")

    # Title
    cmd_title <- map_chr(1:total, ~stringr::str_interp("TITLE Simulation in progress [${.x}/${total}]"))
    # }}}2

    # EnergyPlus commands
    # {{{2
    # Get the default value of parameters transfered to 'energyplus.exe'
    # {{{3
    output_suffix <- "C"
    # If any imf file exist in the job table, turn epmacro on.
    epmacro <- purrr::map_lgl(model_exts, ~{if (identical(.x, "imf")) TRUE else FALSE})
    readvars <- TRUE
    expand_obj <- TRUE
    annual <- FALSE
    design_day <- FALSE
    idd <- NULL
    output_suffix <- "C"
    # }}}3

    # Get the right format of the input command to EnergyPlus.
    # {{{3
    # NOTE: `ifelse` function cannot return NULL.
    if (expand_obj) expand_obj <- "--expandobjects" else expand_obj <- NULL
    if (readvars) readvars <- "--readvars" else readvars <- NULL
    if (annual) annual <- "--annual" else annual <- NULL
    if (design_day) design_day <- "--design-day" else design_day <- NULL
    if (!is.null(idd)) idd <- paste0("--idd", '"', idd, '"') else idd <- NULL
    cmd_eplus <-
        purrr::map_chr(1:total,
                       ~{model <- stringr::str_interp('"${models[.x]}"');
                         weather <- stringr::str_interp('"${weathers[.x]}"');
                         output_dirs <- output_dirs[.x]
                         output_prefixes <- output_prefixes[.x]
                         if (epmacro[.x]) epmacro <- "--epmacro" else epmacro <- NULL;
                         paste(energyplus,
                               "--weather", weather,
                               "--output-directory", output_dirs,
                               "--output-prefix", output_prefixes,
                               "--output-suffix", output_suffix,
                               epmacro, expand_obj, readvars, annual, design_day,
                               idd, model, sep = " ")}
                )
    # }}}3
    # }}}2

    # Post process commands
    # {{{2
    del_list <- c("fort.6", "Energy+.ini", "readvars.audit")
    cmd_post_proc <- paste("IF EXIST", del_list, "DEL", del_list)
    cmd_post_proc <-
        map(1:total,
            ~{if (epmacro[.x]) {
                  rename_imf <- stringr::str_interp("IF EXIST out.idf MOVE out.idf ${model_names[.x]}.idf")
                  c(cmd_post_proc, rename_imf)
              } else {
                  c(cmd_post_proc)
            }})
    # }}}2

    # Divide the job according to total core number
    # {{{2
    divided_job <-
        purrr::map(1:n,
                   ~{divided_job <-
                     purrr::map(1:total,
                                function (job_id = .x, core_id = core_id) {
                                    if (identical(job_id %% n + 1L, core_id)) {
                                        c(cmd_cd[job_id],
                                          cmd_title[job_id],
                                          cmd_eplus[job_id],
                                          cmd_post_proc[[job_id]])
                                    }
                                  }, core_id = .x);
                     divided_job <- purrr::keep(divided_job, purrr::negate(is.null));
                     divided_job <- purrr::flatten_chr(divided_job);
                     divided_job <- c(echo_off, divided_job, "EXIT")})
    # }}}2

    # Run the job with multithreading
    # {{{2
    start_cmd <-
        map_chr(1:n,
                ~{bat_path <- file.path(temp_dir, stringr::str_interp("multi_run_${.x}.bat"))
                bat_contents <- divided_job[[.x]]
                readr::write_lines(bat_contents, bat_path)
                cmd <- paste0(cmd_head, " START ", bat_path)
                })

    walk(start_cmd, ~system(.x, wait = FALSE, invisible = FALSE))
    # }}}2

    message(stringr::str_interp("The job has been successfully executed using EnergyPlus V${eplus_ver} located at '${eplus_dir}'."))
}
# }}}1

#' @expoprt
# run_epg{{{1
run_epg <- function (epg, n = NULL, eplus_ver = NULL, eplus_dir = NULL) {

    # Check 'epg'{{{2
    if (is.character(epg)) {
        if (!file.exists(epg)) {
            stop("Input 'epg' file does not exists.", call. = FALSE)
        } else {
            job <- import_epg(epg)
        }
    } else if (is.data.frame(epg)) {
        type <- attr(epg, "job_type")
        col_names <- colnames(epg)
        required <- c("model", "weather", "output_dir", "output_prefix", "run_times")
        ex_col <- setdiff(col_names, required)
        mis_col <- setdiff(required, col_names)
        if (all(type == "epg", length(ex_col) == 0L, length(mis_col) == 0L)) {
            job <- epg
        } else {
            stop("Invalid 'epg'. Please use 'import_epg' to get the correct type", call. = FALSE)
        }
    } else {
        stop("'epg' should be either a data.frame or an .epg file path.", call. = FALSE)
    }
    # }}}2

    # Write epg file{{{2
    if (unique(job[["output_dir"]]) == 1L) {
        path <- unique(job[["output_dir"]])
        if (!dir.exists(path)) {
            dir.create(path, showWarnings = FALSE, recursive = TRUE)
        }
        file_name <- normalizePath(file.path(path, "run.epg"), mustWork = FALSE)
        write_epg(models = normalizePath(job[["model"]], mustWork = TRUE),
                  weathers = normalizePath(job[["weather"]], mustWork = TRUE),
                  output_dirs = normalizePath(file.path(job[["output_dir"]], job[["output_prefix"]])), mustWork = FALSE,
                  run_times = job[["run_times"]], path = file_name)
    }
    # }}}2

    # Run
    rum_multi(models = epg[["model"]], weathers = epg[["weather"]],
              output_dirs = epg[["output_dir"]], output_prefix = epg[["outout_prefixes"]],
              n = n, eplus_ver = eplus_ver, eplus_dir = eplus_dir)
}
# }}}1

# run_epat{{{1
run_epat <- function(epat, case_name = NULL, output_dir = NULL, n = NULL,
                     eplus_ver = NULL, eplus_dir = NULL) {

    # Check 'epat'{{{2
    if (is.character(epat)) {
        if (!file.exists(epat)) {
            stop("Input 'epat' file does not exists.", call. = FALSE)
        } else {
            if (tools::file_ext(epat) == "epat"){
                epat <- import_epat(epat)
            } else {
                stop("Input 'epat' file should have an extension of 'epat'.",
                     call. = FALSE)
            }
        }
    } else {
        if (!check_epat(epat = epat)) {
            stop("'epat' should be either an EPAT project, or a job imported using 'import_epat'.",
                 call. = FALSE)
        }
    }
    # }}}2

    # Get job{{{2
    job_info <- create_job(param_tbl = epat)
    job <- job_info[["job"]]
    job_index <- job_info[["job_index"]]
    # }}}2

    # Other args check{{{2
    if (is.null(output_dir)) {
        if (epat[["wd_path"]] != "") {
            output_dir <- epat[["wd_path"]]
        } else {
            output_dir <- dirname(epat[["idf_path"]])
        }
    }
    if (!is.null(case_name)) {
        output_dir <- file.path(output_dir, case_name)
    }
    if (is.null(n)) {
        n <- epat[["parallel_num"]]
    }
    if (is.null(eplus_dir)) {
        eplus_dir <- epat[["eplus_path"]]
    }
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    }
    # }}}2

    # Write 'job_index.csv'.
    readr::write_csv(job_index, file.path(output_dir, "job_index.csv"))

    run_multi(models = job[["model"]], weathers = job[["weather"]],
              output_dirs = output_dir, output_prefixes = job_index[["model_case"]],
              case_name = case_name, n = n, eplus_ver = eplus_ver, eplus_dir = eplus_dir)
}
# }}}1

# run_jeplus{{{1
run_jeplus <- function(jeplus, case_name = NULL, output_dir = NULL, n = NULL,
                       eplus_ver = NULL, eplus_dir = NULL) {

    # Check 'jeplus'{{{2
    if (is.character(jeplus)) {
        if (!file.exists(jeplus)) {
            stop("Input 'jeplus' file does not exists.", call. = FALSE)
        } else {
            if (tools::file_ext(jeplus) == "json"){
                jeplus <- import_jeplus(jeplus)
            } else {
                stop("Input 'jeplus' file should have an extension of 'json'.",
                     call. = FALSE)
            }
        }
    } else {
        if (!check_jeplus(jeplus = jeplus)) {
            stop("'jeplus' should be either a jEPlus .json project, or a job imported using 'import_jeplus'.",
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
        output_dir <- file.path(output_dir, case_name)
    }
    if (is.null(eplus_dir)) {
        eplus_dir <- epat[["eplus_path"]]
    }
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    }
    # }}}2

    # Write 'job_index.csv'.
    readr::write_csv(job_index, file.path(output_dir, "job_index.csv"))

    # Run
    run_multi(models = job[["model"]], weathers = job[["weather"]],
              output_dirs = output_dir, output_prefixes = job_index[["model_case"]],
              n = n, eplus_ver = eplus_ver, eplus_dir = eplus_dir)
}
# }}}1

# check_eplus_dir_ver{{{1
check_eplus_dir_ver <- function (eplus_dir = NULL, eplus_ver = NULL) {
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
    return(list(eplus_dir = eplus_dir_used, eplus_ver = eplus_ver_used))
}
# }}}1

# write_epg{{{1
write_epg <- function(models, weathers, output_dirs, run_times, path){
    header <- paste0(
        "! EnergyPlus Group File",
        "! ------------------------------------------------------------------------------------------------",
        "! Each line represents a specific simulation. If you don't want a simulation to run, add a comment",
        "! character (an exclamation point) to the beginning of that line. Commas are used to separate the ",
        "! fields. Each line consists of the following fields: ",
        "!",
        "!    input file name, weather file name, output file name (no extension), counter",
        "!",
        "! ------------------------------------------------------------------------------------------------",
        collapse = "\n")

    contents <- paste(models, weathers, output_dirs, run_times, sep = ",")
    epg_info <- paste(header, contents)
    readr::write_lines(epg_info, path)
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

# check_epat{{{1
check_epat <- function (epat) {
    if (!is.list(epat)) return(FALSE)
    type <- attr(epat, "job_type")
    if (type != "epat") return(FALSE)
    var_names <- names(epat)
    required <- c("idf_path", "weather_path", "param_field", "param_value",
                  "eplus_path", "wd_path", "parallel_num")
    ex_vars <- setdiff(var_names, required)
    mis_vars <- setdiff(required, var_names)
    if (all(length(ex_vars) == 0L, length(mis_vars) == 0L)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}1

# check_epg{{{1
check_epg <- function (epg) {
    if (!is.data.frame(epg)) return(FALSE)
    type <- attr(epg, "job_type")
    if (type != "epg") return(FALSE)
    var_names <- names(epg)
    required <- c("model", "weather", "output_dir", "output_prefix", "run_times")
    ex_vars <- setdiff(var_names, required)
    mis_vars <- setdiff(required, var_names)
    if (all(length(ex_vars) == 0L, length(mis_vars) == 0L)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}1
