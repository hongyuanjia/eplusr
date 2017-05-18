################################################################################
#                                Run EnergyPlus                                #
################################################################################

# get_idd_ver: A function that gets the versions of EnergyPlus.
# {{{1
get_idd_ver <- function(eplus_dir){
    idd <- file.path(eplus_dir, "Energy+.idd")
    idd_ver <- purrr::map_chr(idd, ~gsub(x = readLines(.x, n = 1), "!IDD_Version ", ""))
    return(idd_ver)
}
# }}}1

# find_eplus: A function to locate EnergyPlus folder.
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
                                            "EnergyPlus", value = T, fixed = T)))
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

    # If multiple EnergyPlus versions are found, list the paths and versions.
    if (length(eplus_dir) > 1) {
        if (verbose) {
            message(paste("Multiple EnergyPlus versions are found: \n"),
                    paste(eplus_dir, "Version:", idd_ver, collapse = "\n"), "\n")
        }
    }

    # If no EnergyPlus version is specified, use the latest version.
    if (is.null(ver)) {
        # If multiple EnergyPlus versions are found, use the latest version.
        if (length(eplus_dir) > 1) {
            if (verbose) {
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

        return(eplus_dir_latest)
    } else {
        ver <- paste0(gsub(x = as.character(ver), " ", ""), ".0")
        if (is.na(match(ver, idd_ver))) {
            stop(paste0("Cannot find EnergyPlus Version:", ver, " ",
                        "Please specify EnergyPlus path mannually.\n",
                        "NOTE: Version format should be 'X.Y.Z'."))
        } else {
            eplus_dir_matched <- eplus_dir[match(ver, idd_ver)]
            idd_ver_matched <- idd_ver[match(ver, idd_ver)]
            if (verbose) {
                message(paste0("EnergyPlus Version: ", idd_ver_matched,
                               " has been successfully located:\n", eplus_dir_matched, "\n"))
            }

        return(eplus_dir_matched)
        }
    }
}
# }}}1

# read_eplus_end: A help function to read ".end" file which contains the
# simulation summary info.
# {{{1
read_eplus_end <- function (path, prefix = "eplusout") {
    readLines(file.path(path, paste0(prefix, ".end")))
}
# }}}1

# create_eplus_ini: A function to create an "Energy+.ini" file per working
# direcotry.
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

# energyplus_exe: A wrapper function of EnergyPlus command line interface.
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

# eplus_run: A function to run EnergyPlus in R.
# {{{1
eplus_run <- function (input, weather, eplus_dir = find_eplus(),
                       output_dir = NULL, output_prefix = NULL,
                       csv = TRUE, echo = FALSE,
                       run = "exe", ver = NULL) {

    # Backup the original working directory.
    ori_wd <- getwd()
    # Set the working directory as the same folder of input file.
    setwd(dirname(input))

    # If EnergyPlus version has been specified.
    if (!is.null(ver)) {
        # Case 1. But 'eplus_dir' is not given
        if (missingArg(eplus_dir)) {
            eplus_dir <- find_eplus(ver = ver)
        # Case 2. And 'eplus_dir' is given.
        } else {
            warning("Argument 'ver' will be ignored as 'eplus_dir' has been ",
                    "specifed manually.")
        }
    }

    # File existing checking
    if (!file.exists(input)) {
        stop("Input model file does not exist. Please check input.", call. = FALSE)
    }
    if (!file.exists(weather)) {
        stop("Weather file does not exist. Please check input.", call. = FALSE)
    }

    input_prefix <- tools::file_path_sans_ext(basename(input))
    eplus_in_ext <- tools::file_ext(input)
    wthr_prefix <- tools::file_path_sans_ext(basename(weather))
    eplus_wthr_ext <- tools::file_ext(weather)

    # File extension checking.
    if (!grepl(x = eplus_in_ext, pattern = "i[dm]f", ignore.case = TRUE)) {
       stop("'input' should be a full file path with an extension of '.idf' or '.imf'.")
    }
    if (!grepl(x = eplus_wthr_ext, pattern = "epw", ignore.case = TRUE)) {
       stop("'weather' should be a full file path with an extension of '.epw'.")
    }

    # If output directory is given,
    if (!is.null(output_dir)) {
        output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
        if (!dir.exists(output_dir)) {
            output_dir_flag <- dir.create(output_dir, showWarnings = TRUE)
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

    # Use Epl_run_bat, i.e. "Epl-run.bat", to run EnergyPlus.
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

    # Use energyplus, i.e. "energyplus.exe", to run EnergyPlus.
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

    # Set the woring directory back.
    setwd(ori_wd)

}
# }}}1
