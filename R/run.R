#' @importFrom processx run process
#' @importFrom lubridate ymd
#' @importFrom tools file_path_sans_ext
#' @importFrom rlang f_lhs f_rhs f_env eval_tidy
#' @importFrom data.table month mday setattr
NULL

# eplus_default_path {{{
eplus_default_path <- function (ver) {
    assert_that(is.numeric_version(ver))
    ver_dash <- paste0(ver[,1], "-", ver[,2], "-0")
    if (is_windows()) {
        d <- paste0("C:/EnergyPlusV", ver_dash)
    } else if (is_linux()) {
        d <- paste0("/usr/local/EnergyPlus-", ver_dash)
    } else {
        d <- paste0("/Applications/EnergyPlus-", ver_dash)
    }
    d
}
# }}}
# exe {{{
exe <- function () if (is_windows()) ".exe" else  ""
# }}}
# is_valid_eplus_path {{{
is_valid_eplus_path <- function (path) {
    if (!dir.exists(path)) {
        FALSE
    } else {
        all(file.exists(c(file.path(path, paste0("energyplus",exe())),
                          file.path(path, "Energy+.idd"))))
    }
}
# }}}
# get_ver_from_path {{{
get_ver_from_path <- function (path) {
    # try to get version form EnergyPlus path
    ver <- tryCatch(as.numeric_version(gsub("^.*V", "", path)), error = function (e) NULL)
    # then from the first line of Energy+.idd
    if (is.null(ver)) {
        h <- readr::read_lines(file.path(path, "Energy+.idd"), n_max = 1L)
        # if still failed, just return NULL
        ver <- tryCatch(get_idd_ver(h), error = function (e) NULL)
    }
    ver
}
# }}}
# use_eplus {{{
use_eplus <- function (eplus) {
    # if eplus is a version, try to locate it in the default path
    if (is_supported_ver(eplus)) {
        ver <- as.numeric_version(eplus)
        eplus_dir <- eplus_default_path(eplus)
        if (!is_valid_eplus_path(eplus_dir)) {
            stop(msg("Cannot locate EnergyPlus V", trimws(eplus), " at default
                     installation path ", backtick(eplus_dir), ". Please
                     give exact path of EnergyPlus installation."),
                call. = FALSE)
        }
    } else {
        ver <- get_ver_from_path(eplus)
        eplus_dir <- eplus
        if (!is_valid_eplus_path(eplus)) {
            stop(msg(backtick(eplus_dir), "is not a valid EnergyPlus installation path."),
                call. = FALSE)

        }
    }

    list(version = ver, dir = eplus_dir, exe = paste0("energyplus", exe()))
}
# }}}
# eplus_path {{{
eplus_path <- function (ver = NULL, path = NULL) {
    os <- osname()

    # if path is given, use it
    if (!is.null(path)) {
        path <- unname(path)
        if (!dir.exists(path)) stop(msg(backtick(path), " does not exists."), call. = FALSE)
        eplus_home <- path
    } else if (!is.null(ver)) {
        assert_that(is_supported_ver(ver))
        ver_dash <- dash_ver(ver)
        eplus_home <- switch(os,
            "Windows" = paste0("C:/EnergyPlusV", ver_dash),
            "Linux" = paste0("/usr/local/EnergyPlus-", ver_dash),
            "Darwin" = paste0("/Applications/EnergyPlus-", ver_dash))
        # if failed
        if (!dir.exists(eplus_home)) {
            # and path is NULL, error
            stop(msg("Cannot locate EnergyPlus V", ver, " at default
                     installation path ", backtick(eplus_home), ". Please
                     give exact 'path' of EnergyPlus installation."))
        }
    # if none, error
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
        stop(msg("EnergyPlus executable does not exist in folder ", backtick(eplus_home), "."))
    }

    if (!file.exists(energyplus_idd)) {
        stop(msg(backtick("Energy+.idd"), " does not exist in EnergyPlus
                 installation folder."))
        energyplus_idd <- NULL
    }

    if (!file.exists(chicago_epw)) chicago_epw <- NULL

    eplus_info <- c(home = eplus_home, eplus = eplus_exe,
                    idd = energyplus_idd, epw = chicago_epw)

    return(eplus_info)
}
# }}}
# cmd_args {{{
cmd_args <- function (model, weather, output_dir, output_prefix,
                      output_suffix = "C", expand_obj = TRUE, readvars = TRUE,
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
    # Get the right format of the input command to EnergyPlus. {{{
    # NOTE: `ifelse` function cannot return NULL.
    if (missing(output_prefix) || is.null(output_prefix)) {
        output_prefix <- tools::file_path_sans_ext(basename(model))
    }
    if (missing(output_suffix) || is.null(output_suffix)) {
        output_suffix <- "C"
    }

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
    # }}}
    # In case there are spaces in user input, quote all pathes {{{
    args <- paste(
        "--weather", shQuote(weather),
        "--output-directory", shQuote(output_dir),
        "--output-prefix", shQuote(output_prefix),
        "--output-suffix", output_suffix,
        cmd_epmacro, cmd_expand_obj, cmd_readvars, cmd_annual, cmd_design_day, cmd_idd,
        shQuote(model)
    )
    # }}}

    return(args)
}
# }}}
# copy_run_files {{{
copy_run_files <- function (file, dir) {
    file <- normalizePath(file, mustWork = FALSE)
    loc <- normalizePath(file.path(dir, basename(file)), mustWork = FALSE)
    flag <- FALSE

    if (file == loc) return(file)

    flag <- file.copy(from = file, to = loc, overwrite = TRUE, copy.date = TRUE)

    if (!flag) stop(msg(sprintf("Unable to copy file %s into simulation output directory.",
                                backtick(basename(file)))),
                    call. = FALSE)

    return(loc)
}
# }}}
# run_idf {{{
run_idf <- function (eplus_exe, model, weather, output_dir = NULL,
                     design_day = FALSE, annual = FALSE, expand_obj = TRUE,
                     echo = FALSE) {
    model <- normalizePath(model, mustWork = FALSE)
    weather <- normalizePath(weather, mustWork = FALSE)
    assert_that(file.exists(model))
    assert_that(file.exists(weather))

    # get output directory
    if (is.null(output_dir)) output_dir <- dirname(model)
    output_dir <- normalizePath(output_dir, mustWork = FALSE)
    if (!dir.exists(output_dir)) {
        tryCatch(dir.create(output_dir, recursive = TRUE),
            warning = function (w) {
                stop("Failed to create output directory: ",
                     backtick(output_dir), call. = FALSE)
            }
        )
    }

    # copy input files
    loc_m <- copy_run_files(model, output_dir)
    loc_w <- copy_run_files(weather, output_dir)

    # set working dirctory
    ori_wd <- getwd()
    setwd(dirname(loc_m))
    on.exit(setwd(ori_wd), add = TRUE)

    # get arguments of energyplus
    args <- cmd_args(loc_m, loc_w, output_dir = output_dir, annual = annual,
                     design_day = design_day, expand_obj = expand_obj)

    if (echo) {
        # have to suppress warnings here as it always complains about warnings
        # on 'can nonly read in bytes in a non-UTF-8 MBCS locale'.
        invisible(suppressWarnings(processx::run(eplus_exe, args,
            windows_verbatim_args = TRUE, echo = TRUE)))
    } else {
        processx::process$new(
            eplus_exe, args,
            stdout = "|", stderr = "|", cleanup = TRUE,
            echo_cmd = TRUE, windows_verbatim_args = TRUE,
            windows_hide_window = FALSE)
    }
}
# }}}

# days_in_month {{{
days_in_month <- function (x) {
    days_all <- c(`1` = 31L, `2` = 28L, `3` = 31L,
                  `4` = 30L, `5` = 31L, `6` = 30L,
                  `7` = 31L, `8` = 31L, `9` = 30L,
                  `10` = 31L, `11` = 30L, `12` = 31L)
    unname(days_all[which(names(days_all) == month(x))])
}
# }}}
# format_runperiod {{{
format_runperiod <- function (runperiod, side = c("lhs", "rhs")) {

    side <- match.arg(side)

    # just a random non-leep year
    const_year <- 2017L

    if (as.character(runperiod) %in% c("asis", "annual", "design_day")) {
        return(runperiod)
    }

    split_str <- unlist(strsplit(as.character(runperiod), "[-/.]|[[:space:]]"))

    # handle month
    if (length(split_str) == 1L) {
        out <- suppressWarnings(lubridate::ymd(paste0(const_year, "-", split_str), truncated = 1L))
        if (is.na(out)) stop("Cannot parse run period.", call. = FALSE)

        if (side == "rhs") {
            total_days <- days_in_month(out)
            out <- lubridate::ymd(paste0(const_year, "-", split_str, "-", total_days))
        }

    } else if (length(split_str) == 2L) {
        out <- suppressWarnings(lubridate::ymd(paste0(const_year, "-", paste0(split_str, collapse = "-"))))
    } else {
        stop("Cannot parse run period.", call. = FALSE)
    }

    if (is.na(out)) stop("Cannot parse run period.", call. = FALSE)

    return(out)
}
# }}}
# parse_runperiod {{{
parse_runperiod <- function (runperiod) {
    # NOTE: inspired by `tibbletime` package.
    # lhs/rhs list
    rp <- list(lhs = rlang::f_lhs(runperiod), rhs = rlang::f_rhs(runperiod))

    # Environment to evaluate the sides in
    rp_env <- rlang::f_env(runperiod)
    rp_env$. <- "asis"

    # Tidy evaluation
    rp <- lapply(rp,
        function(x) {
            rlang::eval_tidy(x, env = rp_env)
        }
    )

    # Double up if 1 sided
    # length = 2 means that it has ~ and 1 side
    if (length(runperiod) == 2) {
        rp$lhs <- rp$rhs
    }

    out <- list(start = NA, end = NA)
    out$start <- format_runperiod(rp$lhs, "lhs")
    out$end <- format_runperiod(rp$rhs, "rhs")

    if (out$end %in% c("annual", "design_day", "asis") &&
        as.character(out$start) != out$end) {
        stop(msg("Invalid run period formula. Left hand side should be empty if
                 right hand side is 'annual', 'design_day', or '.'."),
                 call. = FALSE)
    } else if (out$start %in% c("annual", "design_day", "asis") &&
               out$start != as.character(out$end)) {
        stop(msg("Invalid run period formula. Formula should be in a format of
                 '~RHS' if 'annual', 'design_day' or '.' is used."),
                 call. = FALSE)
    } else if (out$start > out$end) {
        stop(msg("Invalid run period formula. Start date should be smaller than
                 end date."), call. = FALSE)
    }

    out
}
# }}}
# set_runperiod {{{
set_runperiod <- function (idf, runperiod, idd, hide_others = TRUE) {
    rp <- parse_runperiod(runperiod)

    setattr(idf, "runperiod", rp)

    if (rp$end %in% c("asis", "annual", "design_day")) return(idf)

    ids <- get_id(idf, "RunPeriod")

    # if the model has already been set before, use it
    if (not_empty(ids)) {
        rp_eplusr <- idf$value[object_id %in% ids][field_order == 1L][
            value == "run_period_eplusr", object_id]
        rp_others <- setdiff(ids, rp_eplusr)
        if (not_empty(rp_eplusr)) {
            idf <- invisible(
                set_object(idf, id = rp_eplusr, name = "run_period_eplusr",
                           begin_month = month(rp$start),
                           begin_day_of_month = mday(rp$start),
                           end_month = month(rp$end),
                           end_day_of_month = mday(rp$end), idd = idd)
            )
        } else {
            idf <- invisible(
                add_object(idf, class = "RunPeriod", name = "run_period_eplusr",
                           begin_month = month(rp$start),
                           begin_day_of_month = mday(rp$start),
                           end_month = month(rp$end),
                           end_day_of_month = mday(rp$end), idd = idd)
            )
        }

        if (hide_others && not_empty(rp_others)) {
            for (i in rp_others) {
                idf <- add_comment(idf, i, append = FALSE, type = 1L,
                                   "[ * Commented out automatically by eplusr * ]")
                idf <- del_object(idf, i, idd, hide = TRUE)
            }
            warning(msg(sprintf("Objects in class %s with ID %s has/have been
                                commented out to use the input run period.",
                                backtick("RunPeriod"),
                                paste(rp_others, collapse = ", "))), call. = FALSE)
        }
    }

    return(idf)
}
# }}}
