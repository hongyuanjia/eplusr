#' @importFrom processx run process
#' @importFrom lubridate ymd
#' @importFrom tools file_path_sans_ext
#' @importFrom rlang f_lhs f_rhs f_env eval_tidy
#' @importFrom data.table setattr
NULL

# eplus_default_path {{{
eplus_default_path <- function (ver) {
    stopifnot(is_eplus_ver(ver))
    ver <- standerize_ver(ver)
    ver_dash <- paste0(ver[1,1], "-", ver[1,2], "-", ver[1,3])
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
    ver <- tryCatch(gsub("^.*V", "", path), error = function (e) NULL)
    # then from the first line of Energy+.idd
    if (is.null(ver)) {
        h <- readr::read_lines(file.path(path, "Energy+.idd"), n_max = 1L)
        # if still failed, just return NULL
        ver <- tryCatch(get_idd_ver(h), error = function (e) NULL)
    }
    standerize_ver(ver)
}
# }}}

#' @export
# use_eplus {{{
use_eplus <- function (eplus) {
    # if eplus is a version, try to locate it in the default path
    if (is_supported_ver(eplus)) {
        ver <- standerize_ver(eplus)
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
        if (is.null(ver)) {
            stop("Failed to detect the version of EnergyPlus located in ",
                 backtick(eplus_dir), ".", call. = FALSE)
        }
    }

    res <- list(version = ver, dir = eplus_dir, exe = paste0("energyplus", exe()))
    .globals$eplus_config[[as.character(ver)]] <- res
    message("EnergyPlus v", ver, " has been added to EnergyPlus location dictionary.")
}
# }}}

#' @export
# avail_eplus {{{
avail_eplus <- function () names(.globals$eplus_config)
# }}}

#' @export
# eplus_config {{{
eplus_config <- function (ver) {
    assert_that(is_eplus_ver(ver))
    ver <- standerize_ver(ver)
    .globals$eplus_config[[as.character(ver)]]
}
# }}}

#' @export
# eplus_available {{{
eplus_available <- function (ver) !is.null(eplus_config(ver))
# }}}

# standerize_ver {{{
standerize_ver <- function (ver) {
    if (is_integerish(ver)) ver <- paste0(ver, ".0")
    ver <- as.numeric_version(ver)
    if (is.na(ver[1,3])) ver[1,3] <- 0
    ver
}
# }}}

# init_avail_eplus {{{
init_avail_eplus <- function () {
    lapply(c(8.5, 8.6, 8.7, 8.8, 8.9),
           function (x) tryCatch(use_eplus(x), error = function (e) NULL))
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

#' @export
# clean_wd {{{
clean_wd <- function (model_path) {

    base <- tools::file_path_sans_ext(basename(model_path))
    without_ext <- tools::file_path_sans_ext(model_path)
    wd <- dirname(model_path)

    # files_to_delete {{{
    suffix <- c(".Zsz", ".audit", ".bnd", ".csv", ".dbg", ".det", ".dfs",
        ".dxf", ".edd", ".eio", ".end", ".epmdet", ".epmidf", ".err", ".eso",
        ".expidf", ".inp", ".log", ".mat", ".mdd", ".mtd", ".mtr", ".rdd",
        ".rvaudit", ".sci", ".shd", ".sln", ".sql", ".ssz", ".svg", ".tab",
        ".txt", ".wrl", "DElight.dfdmp", "DElight.eldmp", "DElight.in",
        "DElight.out", "DFS.csv", "Map.csv", "Map.tab", "Map.txt", "Meter.csv",
        "Meter.tab", "Meter.txt", "Screen.csv", "Spark.log", "Sqlite.err",
        "Ssz.csv", "Ssz.tab", "Ssz.txt", "Table.csv", "Table.htm", "Table.html",
        "Table.tab", "Table.txt", "Table.xml", "Zsz.csv", "Zsz.tab", "Zsz.txt")
    out_files <- paste0(without_ext, suffix)

    individual <- c("BasementGHTIn.idf", "audit.out", "expanded.idf",
        "expandedidf.err", "in.epw", "in.idf", "in.imf", "in.stat", "out.idf",
        "readvars.audit", "slab.int", "sqlite.err", "test.mvi", "fort.6")

    if (base == "in") {
        individual <- setdiff(individual, c("in.epw", "in.idf"))
    } else {
        individual <- setdiff(individual, paste0(base, ".idf"))
    }

    seperates <- normalizePath(file.path(wd, individual), mustWork = FALSE)

    target <- c(out_files, seperates)
    # }}}

    purrr::walk(target, unlink)
}
# }}}

#' @export
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
    # clean output directory
    clean_wd(model)

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
