# eplus_path {{{
eplus_path <- function (ver = NULL, path = NULL) {
    os <- Sys.info()['sysname']
    if (!is.null(ver)) {
        assert_that(is_eplus_ver(ver))
        ver_dash <- dash_ver(ver)
        eplus_home <- switch(os,
            "Windows" = paste0("C:/EnergyPlusV", ver_dash),
            "Linux" = paste0("/usr/local/EnergyPlus-", ver_dash),
            "Darwin" = paste0("/Applications/EnergyPlus-", ver_dash))
        if (!dir.exists(eplus_home)) {
            stop(msg("Cannot locate EnergyPlus V", ver, " at ",
                     sQuote(eplus_home), ". Please give 'path'."))
        }
    } else if (!is.null(path)) {
        if (!dir.exists(eplus_home)) stop(msg(sQuote(path), " does not exists."))
        eplus_home <- path
    } else {
        stop("Both 'ver' and 'path' are NULL.", call. = FALSE)
    }

    ext <- ""
    if (os == "Windows") ext <- ".exe"

    eplus_exe <- normalizePath(
        file.path(eplus_home, paste0("energyplus", ext)),
        winslash = "/", mustWork = FALSE
    )

    energyplus_idd <- normalizePath(
        file.path(eplus_home, "Energy+.idd"), winslash = "/", mustWork = FALSE
    )

    chicago_epw <- normalizePath(
        file.path(eplus_home, "WeatherData", "USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw"),
        winslash = "/", mustWork = FALSE
    )

    if (!file.exists(eplus_exe)) {
        stop(msg("EnergyPlus executable does not exist in the folder."))
    }

    if (!file.exists(energyplus_idd)) {
        warning(msg(sQuote("Energy+.idd"), " does not exist in EnergyPlus
                    installation folder."))
        energyplus_idd <- NULL
    }

    if (!file.exists(chicago_epw)) chicago_epw <- NULL

    eplus_info <- c(home = eplus_home, eplus = eplus_exe,
                    idd = energyplus_idd, epw = chicago_epw)

    return(eplus_info)
}
# }}}
# dash_ver {{{
dash_ver <- function (ver) {
    assert_that(is_eplus_ver(ver))
    paste0(sub(".", "-", ver, fixed = TRUE), "-0")
}
# }}}
# cmd_args {{{1
cmd_args <- function (model, weather, output_dir, output_prefix,
                      output_suffix, expand_obj = TRUE, readvars = TRUE,
                      annual = FALSE, design_day = FALSE, idd = NULL) {
    # docs {{{
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

    # }}}
    # Get the right format of the input command to EnergyPlus. {{{2
    if (missing(output_dir)) {
        output_dir <- dirname(model)
    }
    if (missing(output_prefix)) {
        output_prefix <- tools::file_path_sans_ext(basename(model))
    }
    if (missing(output_suffix)) {
        output_suffix <- "C"
    }

    # NOTE: `ifelse` function cannot return NULL.
    if (has_ext(model, "imf")) {
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
    args <- paste(
        "--weather", shQuote(weather),
        "--output-directory", shQuote(output_dir),
        "--output-prefix", shQuote(output_prefix),
        "--output-suffix", output_suffix,
        cmd_epmacro, cmd_expand_obj, cmd_readvars, cmd_annual, cmd_design_day, cmd_idd,
        shQuote(model)
    )
    # }}}2

    return(args)
}
# }}}1
# run_idf {{{
run_idf <- function (eplus_exe, model, weather, ..., echo = FALSE) {
    ori_wd <- getwd()
    wd <- dirname(model)
    setwd(wd)
    on.exit(setwd(ori_wd))
    if (echo) {
        invisible(processx::run(eplus_exe, cmd_args(model, weather, ...),
                                windows_verbatim_args = TRUE, echo = TRUE))
    } else {
        p <- processx::process$new(
            eplus_exe, cmd_args(model, weather, ...),
            stdout = "|", stderr = "|", cleanup = TRUE,
            echo_cmd = TRUE, windows_verbatim_args = TRUE,
            windows_hide_window = FALSE)
        return(p)
    }
}
# }}}
