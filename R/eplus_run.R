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

#' @importFrom purrr flatten_chr map
#' @importFrom stringr str_detect
# find_eplus: A function to locate EnergyPlus folder.
# find_eplus
# {{{1
find_eplus <- function(ver = NULL, verbose = TRUE){
    # Define searching paths.
    # 1. check if drives exist.
    disks <- c("C:", "D:", "E:", "F:", "G:")
    disks <- disks[dir.exists(paste0(disks,"/"))]
    # 2. search EnergyPlus in drive root path and program file folders.
    path_list <- c(paste0(disks, "/"),
                   file.path(disks, "Program Files"),
                   file.path(disks, "Program Files (x86)"))

    # Find if EnergyPlus folder exists in path list.
    eplus_dir <-
        purrr::flatten_chr(purrr::map(path_list,
                                      ~grep(x = dir(path = .x, full.names = TRUE),
                                            "EnergyPlus", value = TRUE, fixed = TRUE)))

    # If not, give a warning of specifying EnergyPlus program location
    # mannually.
    if (length(eplus_dir) == 0L) {
        stop(paste("Cannot find EnergyPlus folder. ",
                   "Please specify EnergyPlus installed path mannually."),
             call. = FALSE)
    }

    # Get the version(s) of EnergyPlus.
    idd_ver <- get_idd_ver(eplus_dir)
    # Get the latest version of EnergyPlus and its path.
    eplus_dir_latest <- eplus_dir[max(order(idd_ver))]
    idd_ver_latest <- max(idd_ver)

    # If no EnergyPlus version is specified, use the latest version.
    if (is.null(ver)) {
        # If multiple EnergyPlus versions are found, use the latest version.
        if (length(eplus_dir) > 1) {
            if (verbose) {
                # List the paths and versions.
                message(paste("Multiple EnergyPlus versions are found: \n"),
                        paste(eplus_dir, "Version:", idd_ver, collapse = "\n"), "\n")
                # Use the latest version.
                message(paste("NOTE: Only the latest version of EnergyPlus",
                              idd_ver_latest,
                              "will be used if argument 'ver' is not sepcified.\n"))
            }
        # If only one EnergyPlus version is found, print message.
        } else {
            if (verbose) {
                message(paste("EnergyPlus Version:", idd_ver_latest,
                              "has been successfully located:\n", eplus_dir_latest, "\n"))
            }
        }
        ver_returned <- idd_ver_latest
        dir_returned <- eplus_dir_latest
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

        ver_returned <- idd_ver_matched
        dir_returned <- eplus_dir_matched
        }
    }

    attr(dir_returned, "ver") <- ver_returned
    return(dir_returned)
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
# run_eplus: A function to run EnergyPlus in R.
# run_eplus
# {{{1
run_eplus <- function (input, weather, eplus_dir = find_eplus(),
                       output_dir = NULL, output_prefix = NULL,
                       csv = TRUE, echo = FALSE,
                       run = "exe", ver = NULL) {

    # Backup the original working directory.
    ori_wd <- getwd()
    # Set the working directory as the same folder of input file.
    setwd(dirname(input))

    # 'eplus_dir' checking
    # {{{2
    # If EnergyPlus version has been specified.
    if (!is.null(ver)) {
        # Case 1. But 'eplus_dir' is not given
        if (missingArg(eplus_dir)) {
            eplus_dir <- find_eplus(ver = ver)
            ver <- attr(eplus_dir, "ver")
        # Case 2. And 'eplus_dir' is given.
        } else {
            energyplus_exe <- normalizePath(file.path(eplus_dir, "energyplus.exe"))
            if (!file.exists(energyplus_exe)) {
                stop("Invalid EnergyPlus path. Please change 'eplus_dir'.", call. = FALSE)
            } else {
            ver <- get_idd_ver(eplus_dir)
            warning("Argument 'ver' will be ignored as 'eplus_dir' has been ",
                    "specifed manually.", call. = FALSE)
            }
        }
    }
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
    idf_ver <- paste0(get_idf_ver(readr::read_lines(input)), ".0")
    if (!identical(idf_ver, ver)) {
        warning(stringr::str_interp("The input model ${input} indicates an EnergyPlus verion of ${idf_ver} but is simulated using EnergyPlus ${ver}. Unexpected results may occur. It is recommended to use 'IDFVersionUpdater' distributed with EnergyPlus before simulation."), call. = FALSE)
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
                stop("Could not find or create the specified output direcotry.")
            }
        }

        # (a) and also is output prefix.
        if (!is.null(output_prefix)) {
            new_name_idf <- file.path(output_dir, paste0(output_prefix, ".", eplus_in_ext))
            new_name_wthr <- file.path(output_dir, paste0(wthr_prefix, "(", output_prefix, ").", eplus_wthr_ext))

        # (b) but output_prefix is not given.
        } else {
            new_name_idf <- file.path(output_dir, paste0(input_prefix, ".", eplus_in_ext))
            new_name_wthr <- file.path(output_dir, paste0(wthr_prefix, "(", input_prefix, ").", eplus_wthr_ext))
        }
    # If output directory is not given,
    } else {
        # (a) but output prefix is given
        if (!is.null(output_prefix)) {
            new_name_idf <- file.path(dirname(input), paste0(output_prefix, ".", eplus_in_ext))
            new_name_wthr <- file.path(dirname(input), paste0(wthr_prefix, "(", output_prefix, ").", eplus_wthr_ext))

        # (b) neither is output prefix.
        } else {
            new_name_idf <- file.path(dirname(input), paste0(input_prefix, ".", eplus_in_ext))
            new_name_wthr <- file.path(dirname(input), paste0(wthr_prefix, "(", input_prefix, ").", eplus_wthr_ext))
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
    } else if (run == "exe"){
        idf <- input
        if (is.null(output_dir)) {
            output_dir <- dirname(input)
        }

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

    } else {
        stop("Argument 'run' should be one of c('exe', 'bat').", call. = FALSE)
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
    attrs <- list(ver = idf_ver,
                  type = "string")
    idf_raw <- add_attrs(idf_raw, attrs)

    return(idf_raw)
}
# }}}1


#' @importFrom purrr flatten_chr map set_names cross_n
#' @importFrom data.table setcolorder rbindlist
#' @importFrom readr write_lines write_csv
# create_job
# {{{1
create_job <- function (param_tbl, path = NULL) {
    all_names <- names(param_tbl)
    is_param_tbl <- all(match("idf_path", all_names),
                        match("weather_path", all_names),
                        match("param_field", all_names),
                        match("param_value", all_names))

    if (!is_param_tbl) {
        stop("Invalid parametric table.", call. = FALSE)
    }

    if (is.null(path)) {
        path <- getwd()
    }

    idf_path <- param_tbl[["idf_path"]]
    idf_name <- basename(idf_path)
    weather_path <- param_tbl[["weather_path"]]
    weather_name <- basename(weather_path)
    param_name <- names(param_tbl[["param_field"]])
    param_field <- purrr::flatten_chr(param_tbl[["param_field"]])
    param_value <- map(param_tbl[["param_value"]], flatten_chr)

    # Creat idf/imfs and save it into tempdir.
    # Create tempdir for eplus package
    temp_dir <- normalizePath(paste0(tempdir(), "\\eplusr"), winslash = "/", mustWork = FALSE)
    if (!dir.exists(temp_dir)) {
        dir.create(temp_dir)
    }

    # TODO: Find a functionial way to determine the file extension.
    file_name <- file.path(temp_dir, paste0(names(param_value), ".imf"))
    file_name <- normalizePath(file_name, winslash = "/", mustWork = FALSE)
    param_idfs <-
        purrr::map(seq_along(param_value),
                   ~create_param(idf_path = idf_path,
                                 param_name = param_name,
                                 param_field = param_field,
                                 param_value = param_value[[.x]]))
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
    job <- data.table::rbindlist(job)
    # Add job number
    job <- job[, `#` := seq_along(V1)]
    job <- data.table::setcolorder(job, c("#", col_names(job, "#", invert = TRUE)))
    # Set names as jEPlus style.
    job <- purrr::set_names(job, c("#", "model_case", "model_template", "weather", param_field))

    # Modify the job table before writing it to "job_index.csv"
    job_ <- copy(job)
    job_ <- job_[, `:=`(weather = basename(weather))]

    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
    }
    job_file <- file.path(path, "job_index.csv")
    readr::write_csv(job_, path = job_file)

    job <- job[, .(model = normalizePath(file.path(temp_dir, paste0(model_case, ".imf")), mustWork = TRUE), weather)]
    return(job)
}
# }}}1

#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom stringr str_length str_interp
#' @importFrom purrr map_lgl map_chr walk walk2 map keep negate flatten_chr
#' @importFrom readr read_lines write_lines
# run_job
# {{{1
run_job <- function (job, eplus_dir = find_eplus(),
                     output_dir = NULL, output_prefix = NULL,
                     by = c("model", "weather"),
                     n = parallel::detectCores(logical = FALSE), ver = NULL) {

    # Create tempdir for eplus package
    # {{{2
    temp_dir <- normalizePath(paste0(tempdir(), "\\eplusr"), winslash = "/", mustWork = FALSE)
    if (!dir.exists(temp_dir)) {
        dir.create(temp_dir)
    }
    # }}}2

    # Check job type.
    # {{{2
    job_type <- attr(job, "job_type")
    # If the job is imported from `import_epg`
    if (identical(job_type, "epg")) {
        if (any(!is.null(output_dir), !is.null(output_prefix))) {
            warning("For job imported from jEPlus, 'output_dir' and 'output_prefix will be omitted.'", call. = FALSE)
        }
        output_dir <- job[["output_dir"]]
        output_prefix <- job[["output_prefix"]]
    }
    # If the job is imported from `import_jeplus`
    if (identical(job_type, "jeplus")) {
        if (is.null(output_dir)) {
            output_dir <- getwd()
        }
        job <- create_job(param_tbl = job, path = output_dir)
    }

    # If the job is imported from `import_epat`
    if (identical(job_type, "epat")) {
        job_parallel_num <- job$parallel_num
        job_eplus_path <- job$eplus_path
        job_wd_path <- job$wd_path
        if (is.null(output_dir)) {
            if (any(is.null(job_wd_path), identical(job_wd_path, ""), is.na(job_wd_path))) {
                output_dir <- getwd()
            } else {
                output_dir <- job_wd_path
            }
        }
        job <- create_job(param_tbl = job, path = output_dir)

        # Get n
        if (!all(is.null(job_parallel_num), identical(job_parallel_num, ""), is.na(job_parallel_num))) {
            n <- as.integer(job_parallel_num)
        }

        if (!all(is.null(job_eplus_path), identical(job_eplus_path, ""), is.na(job_eplus_path))) {
            eplus_dir <- job_eplus_path
        }
    }

    # 'job' format checking
    if (!is.data.frame(job)) {
        stop("Invalid job. 'job' should be a data.frame with two columns named 'model' and 'weather' respectively.", call. = FALSE)
    }
    all_names <- names(job)
    check_job <- all(match("model", all_names), match("weather", all_names))
    if (!check_job) {
        stop("Invalid job. 'job' should be a data.frame having two columns named 'model' and 'weather' respectively.", call. = FALSE)
    }
    # }}}2

    # Get basic info of input files.
    # {{{2
    model <- job[["model"]]
    weather <- job[["weather"]]
    model_name <- tools::file_path_sans_ext(basename(job[["model"]]))
    weather_name <- tools::file_path_sans_ext(basename(job[["weather"]]))
    model_ext <- tools::file_ext(model)
    weather_ext <- tools::file_ext(weather)

    # Get total job number.
    total <- nrow(job)
    # If the total job number is smaller than the core specified, set the core
    # equal to total job number.
    if (n > total) {
        n <- total
    }
    # }}}2

    # File existing checking
    # {{{2
    idx_model <- any(!file.exists(model))
    idx_weather <- any(!file.exists(weather))
    if (idx_model) {
        ori_error_len <- getOption("warning.length")
        missing_model <- model[idx_model]
        error_msg <- paste0("Input model file '", missing_model, "' does not exist. Please check input.", collapse = "\n")
        options("warning.length" = stringr::str_length(error_msg) + 1000)
        stop(error_msg, call. = FALSE)
        options("warning.length" = ori_error_len)
    }
    if (idx_weather) {
        ori_error_len <- getOption("warning.length")
        missing_weather <- weather[idx_weather]
        error_msg <- paste0("Input weather file '", missing_weather, "' does not exist. Please check input.", collapse = "\n")
        options("warning.length" = stringr::str_length(error_msg) + 1000)
        stop(error_msg, call. = FALSE)
        options("warning.length" = ori_error_len)
    }
    # }}}2

    # Parametric field existence checking
    # {{{2
    idx_has_param <- purrr::map_lgl(model, ~{raw <- readr::read_lines(.x);is_param_exist(raw)})
    if (any(idx_has_param)) {
        param_idfs <- model[idx_has_param]
        param_list <- purrr::map_chr(param_idfs,
                                      ~{raw <- readr::read_lines(.x)
                                        params <- list_params(raw)
                                        paste(params, collapse = ", ")
                                       })
        info <- purrr::map2_chr(param_idfs, param_list,
                                ~stringr::str_interp("For ${basename(.x)}: ${.y}"))
        if (!is.na(match(job_type, c("jeplus", "epat")))) {
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
    if (any(!grepl(x = model_ext, pattern = "i[dm]f", ignore.case = TRUE))) {
       stop("'input' should be a full file path with an extension of '.idf' or '.imf'.", call. = FALSE)
    }
    if (any(!grepl(x = weather_ext, pattern = "epw", ignore.case = TRUE))) {
       stop("'weather' should be a full file path with an extension of '.epw'.", call. = FALSE)
    }
    # }}}2

    # 'eplus_dir' checking
    # {{{2
    # If EnergyPlus version has been specified.
    if (!is.null(ver)) {
        # Case 1. But 'eplus_dir' is not given
        if (missing(eplus_dir)) {
            eplus_dir <- find_eplus(ver = ver)
            ver <- attr(eplus_dir, "ver")
        # Case 2. And 'eplus_dir' is given.
        } else {
            energyplus_exe <- normalizePath(file.path(eplus_dir, "energyplus.exe"))
            if (!file.exists(energyplus_exe)) {
                stop("Invalid EnergyPlus path. Please change 'eplus_dir'.", call. = FALSE)
            } else {
            ver <- get_idd_ver(eplus_dir)
            warning("Argument 'ver' will be ignored as 'eplus_dir' has been ",
                    "specifed manually.", call. = FALSE)
            }
        }
    }
    energyplus_exe <- normalizePath(file.path(eplus_dir, "energyplus.exe"))
    # }}}2

    # Input file version and EnergyPlus verion match checking
    # {{{2
    idf_vers <- purrr::map_chr(model, ~paste0(get_idf_ver(readr::read_lines(.x)), ".0"))
    is_ver_matched <- purrr::map_lgl(idf_vers, ~{identical(.x, ver)})
    if (!all(is_ver_matched)) {
        if (identical(job_type, "jeplus")) {
            warning(stringr::str_interp("The input template model indicates an EnergyPlus verion of ${unique(idf_vers)} but is simulated using EnergyPlus ${ver}. Unexpected results may occur. It is recommended to use 'IDFVersionUpdater' distributed with EnergyPlus before simulation."), call. = FALSE)
        } else {
            warning(stringr::str_interp("There are models indicating EnergyPlus versions which are different from the EnergyPlus version ${ver} used in simulation:\n"))
            un_matched_models <- model[!is_ver_matched]
            un_matched_vers <- idf_vers[!is_ver_matched]
            msg <- paste(paste0("Model:", un_matched_models, ", indicated version:", un_matched_vers), collapse = "\n")
            warning(meg, call. = FALSE)
        }
    }
    # }}}2

    # 'output_dir' checking
    # {{{2
    if (!is.null(output_dir)) {
        if (all(!is.character(output_dir), length(output_dir) != 1, length(output_dir) != total)) {
            stop("'output_dir' should be a character vector of length 1 or length equal to total job number.",
                 call. = FALSE)
        }
    }
    # }}}2

    # 'output_prefix' checking
    # {{{2
    if (!is.null(output_prefix)) {
        if (all(!is.character(output_prefix), length(output_prefix) != 1, length(output_prefix) != total)) {
            stop("'output_prefix' should be a character vector of length 1 or length equal to total job number.",
                 call. = FALSE)
        }
    }
    # }}}2

    # 'by' value checking
    # {{{2
    if (missingArg(by)) {
        by <- "weather"
    } else {
        if (is.na(match(by, c("model", "weather")))) {
            stop("`by` should be one of c('model', 'weather').", call. = FALSE)
        }
    }
    # }}}2

    # Create a directory per case.
    # {{{2
    if (!is.null(output_dir)) {
        # Get the full path of output_dir if is given as a relative path.
        output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
        if (!dir.exists(output_dir)) {
            output_dir_flag <- dir.create(output_dir, showWarnings = TRUE, recursive = TRUE)
            if(!output_dir_flag) {
                stop("Could not find or create the specified output direcotry.", call. = FALSE)
            }
        }
    } else {
        # set the parent output directory as current directory.
        output_dir <- dirname(model)
    }
    # }}}2

    # Get output directory per case.
    # {{{2
    if (length(output_dir) == 1) {
        output_dir <- rep(output_dir, total)
    }
    if (identical(by, "model")) {
        output_dir <- purrr::map_chr(1:total, ~file.path(output_dir[.x], model_name[.x], weather_name[.x]))
    } else {
        output_dir <- purrr::map_chr(1:total, ~file.path(output_dir[.x], weather_name[.x], model_name[.x]))
    }
    # }}}2

    # Create output directories and 'Energy+.ini' per directory.
    # {{{2
    purrr::walk(output_dir, ~{if (!dir.exists(.x)) dir.create(.x, recursive = TRUE);
                create_eplus_ini(eplus_dir = eplus_dir, working_dir = .x)})
    # }}}2

    # Get new input file names.
    # {{{2
    if (length(output_prefix) == 1) {
        output_prefix <- rep(output_prefix, total)
    }
    if (!is.null(output_prefix)) {
        new_model_name <- file.path(output_dir, paste0(output_prefix, ".", model_ext))
        new_weather_name <- file.path(output_dir, paste0(weather_name, "(", output_prefix, ").", weather_ext))

    } else {
        output_prefix <- model_name
        new_model_name <- file.path(output_dir, paste0(model_name, ".", model_ext))
        new_weather_name <- file.path(output_dir, paste0(weather_name, "(", model_name, ").", weather_ext))
    }
    # }}}2

    # Copy and rename input files according to new names.
    # {{{2
    purrr::walk2(c(new_model_name, new_weather_name),
                 normalizePath(c(model, weather), winslash = "/"),
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
    cmd_cd <- paste("CD", normalizePath(output_dir), sep = " ")

    # Title
    cmd_title <- map_chr(1:total, ~stringr::str_interp("TITLE Simulation in progress [${.x}/${total}]"))
    # }}}2

    # EnergyPlus commands
    # {{{2
    # Get the default value of parameters transfered to 'energyplus.exe'
    # {{{3
    output_suffix <- "C"
    # If any imf file exist in the job table, turn epmacro on.
    epmacro <- purrr::map_lgl(model_ext, ~{if (identical(.x, "imf")) TRUE else FALSE})
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
                       ~{model <- stringr::str_interp('"${model[.x]}"');
                         weather <- stringr::str_interp('"${weather[.x]}"');
                         output_dir <- output_dir[.x]
                         output_prefix <- output_prefix[.x]
                         if (epmacro[.x]) epmacro <- "--epmacro" else epmacro <- NULL;
                         paste(energyplus_exe,
                               "--weather", weather,
                               "--output-directory", output_dir,
                               "--output-prefix", output_prefix,
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
                  rename_imf <- stringr::str_interp("IF EXIST out.idf MOVE out.idf ${model_name[.x]}.idf")
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

    message(stringr::str_interp("The job has been successfully executed using EnergyPlus ${ver}."))
}
# }}}1
