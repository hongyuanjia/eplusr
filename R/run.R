#' @importFrom tools file_path_sans_ext
#' @importFrom readr read_lines
#' @importFrom processx run process
#' @importFrom data.table data.table
#' @importFrom future plan
#' @importFrom furrr future_map2
NULL

#' Clean working directory of a previous EnergyPlus simulation
#'
#' Clean working directory of an EnergyPlus simulation by deleting all input and
#' output files of previous simulation.
#'
#' @param path An `.idf` or `.imf` file path.
#' @details
#' `clean_wd()` imitates the same process that EnergyPlus does whenever a new
#' simulation is get to start. It deletes all related output files that have
#' the same name prefix as the input path. The input model itself and any
#' weather file is not deleted.
#'
#' `clean_wd()` is called internally in functions that call EnergyPlus, such as
#' `run_idf()`, `run_multi()` and `$run()` in [`EplusJob`][job] class.
#' @examples
#' \dontrun{
#' clean_wd("foo.idf")
#' }
#' @export
#' @author Hongyuan Jia
# clean_wd {{{
clean_wd <- function (path) {

    base <- tools::file_path_sans_ext(basename(path))
    without_ext <- tools::file_path_sans_ext(path)
    wd <- dirname(path)

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

    for (f in target) unlink(f)
}
# }}}

#' Run simulations of EnergyPlus models.
#'
#' `run_idf()` is a wrapper of EnergyPlus command line interface which enables to
#' run EnergyPlus model with different options.
#'
#' `run_multi()` provides the functionality of running multiple models in
#' parallel.
#'
#' @param eplus An acceptable input of [use_eplus()] and [eplus_config()].
#' @param model A path of an EnergyPlus IDF or IMF file.
#' @param weather A path of an EnergyPlus EPW weather file.
#' @param output_dir Output directory path. If NULL, the directory of input
#'     model is used.
#' @param design_day Force design-day-only simulation. Default: `FALSE`.
#' @param annual Force design-day-only simulation. Default: `FALSE`.
#' @param echo Only applicable to `run_idf`. If `TRUE`, show EnergyPlus
#'     simulation process information to the console. If `FALSE`, which is default, a
#'     [processx::process] object is returned. Default: `TRUE`.
#' @param expand_obj Where to run ExpandObject preprocessor before simulation.
#'     Default: `TRUE`.
#' @param parallel_backend Acceptable input for [future::plan()]. Default:
#'     `future::multiprocess`, which tries to run all models in parallel.
#'
#' @details
#' Behind the scene, `run_multi()` uses the
#' [furrr](https://cran.r-project.org/package=furrr) package which provides
#' mapping functions in parallel using
#' [future](https://cran.r-project.org/package=future) package.
#'
#' It is suggested to run simulations using [EplusJob][job] class, which
#' provides much more detailed controls on the simulation and also methods to
#' extract simulation output.
#'
#' @references
#' [Running EnergyPlus from Command Line (EnergyPlus GitHub Repository)](https://github.com/NREL/EnergyPlus/blob/develop/doc/running-energyplus-from-command-line.md)
#' @examples
#' \dontrun{
#' idf_path <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
#'
#' if (is_avail_eplus(8.8)) {
#'     # run a single model
#'     epw_path <- file.path(
#'         eplus_config(8.8)$dir,
#'         "WeatherData",
#'         "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'     )
#'
#'     run_idf(8.8, idf_path, epw_path, output_dir = tempdir())
#'
#'     # run multiple model in parallel
#'     idf_paths <- file.path(eplus_config(8.8)$dir, "ExampleFiles",
#'         c("1ZoneUncontrolled.idf", "1ZoneUncontrolledFourAlgorithms.idf")
#'     )
#'     epw_paths <- rep(epw_path, times = 2L)
#'     output_dirs <- file.path(tempdir(), tools::file_path_sans_ext(basename(idf_paths)))
#'     run_multi(8.8, idf_paths, epw_paths, output_dir = output_dirs)
#' }
#' }
#' @rdname run_model
#' @seealso [EplusJob][job] class and [ParametricJob][param] class which provide
#' a more friendly interface to run EnergyPlus simulations and collect outputs.
#' @author Hongyuan Jia
#' @export
# run_idf {{{
run_idf <- function (eplus, model, weather, output_dir, design_day = FALSE,
                     annual = FALSE, expand_obj = TRUE, echo = TRUE) {

    exe <- eplus_exe(eplus)

    model <- normalizePath(model, mustWork = TRUE)
    weather <- normalizePath(weather, mustWork = TRUE)

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
        out <- invisible(suppressWarnings(processx::run(exe, args,
            windows_verbatim_args = TRUE, echo = TRUE)))
    } else {
        out <- processx::process$new(
            exe, args,
            stdout = "|", stderr = "|", cleanup = TRUE,
            echo_cmd = FALSE, windows_verbatim_args = TRUE,
            windows_hide_window = FALSE)
        message("EnergyPlus is running in the background with PID ",
            backtick(out$get_pid()), ".\n", sep = "")
    }

    # delete "readvars.audit" generated by ReadEsoVars
    unlink("readvars.audit")

    out
}
# }}}

#' @export
#' @rdname run_model
# run_multi {{{
run_multi <- function (eplus, model, weather, output_dir, design_day = FALSE,
                       annual = FALSE, parallel_backend = future::multiprocess) {
    if (!is_scalar(model)) {
        if (!is_scalar(weather))
            assert_that(is_same_len(model, weather))

        if (!is_scalar(eplus))
            assert_that(is_same_len(model, eplus))
    }

    model <- normalizePath(model, mustWork = TRUE)
    weather <- normalizePath(weather, mustWork = TRUE)
    eplus_exe <- vapply(eplus, eplus_exe, character(1))

    if (anyDuplicated(model) & is.null(output_dir))
        stop("`model` cannot have any duplications when `output_dir` is not given.",
            call. = FALSE)

    if (is.null(output_dir)) {
        output_dir <- dirname(model)
    } else {
        assert_that(is_same_len(model, output_dir))
    }

    output_dir <- normalizePath(output_dir, mustWork = FALSE)

    input <- data.table::data.table(model = model, output_dir = output_dir)

    if (anyDuplicated(input))
        stop("Duplication found in the combination of `model` and `output_dir`.",
            " One model could not be run in the same output directory multiple ",
            "times simultaneously.", call. = FALSE)

    d <- unique(output_dir)[!dir.exists(unique(output_dir))]
    created <- vapply(d, dir.create, logical(1L), showWarnings = FALSE, recursive = TRUE)
    if (any(!created))
        stop("Failed to create output directory:\n",
            paste0(backtick(d[!created]), collapse = "\n"), call. = FALSE)

    lapply(unique(input$model), clean_wd)

    input[, `:=`(weather = weather, eplus_exe = eplus_exe)]
    input[, `:=`(loc_model = copy_run_files(model, output_dir),
                 loc_weather = copy_run_files(weather, output_dir))]
    input[, index := .I]
    input[, `:=`(run_args = list(cmd_args(loc_model, loc_weather, output_dir,
                     design_day = design_day, annual = annual))), by = index]

    future::plan(parallel_backend)
    l <- furrr::future_map2(input$eplus_exe, input$run_args,
        ~invisible(suppressWarnings(processx::run(.x, .y,
            windows_verbatim_args = TRUE, echo = FALSE))), .progress = TRUE)
    # close all RScript process after simulation complete
    # Reference: https://github.com/HenrikBengtsson/future/issues/117
    future::plan(future::sequential)

    # delete "readvars.audit" generated by ReadEsoVars
    unlink("readvars.audit")

    # TODO: summary info of multiple simualtions
    l
}
# }}}

# eplus_exe {{{
eplus_exe <- function (eplus) {
    if (!is_avail_eplus(eplus)) use_eplus(eplus)
    config <- tryCatch(eplus_config(eplus), warning = function (w) stop(w))
    normalizePath(file.path(config$dir, config$exe), mustWork = TRUE)
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

    args <- c(
        "--weather", weather,
        "--output-directory", output_dir,
        "--output-prefix", output_prefix,
        "--output-suffix", output_suffix,
        cmd_epmacro, cmd_expand_obj, cmd_readvars, cmd_annual, cmd_design_day, cmd_idd,
        model
    )
}
# }}}
# copy_run_files {{{
copy_run_files <- function (file, dir) {
    file <- normalizePath(file, mustWork = TRUE)
    loc <- normalizePath(file.path(dir, basename(file)), mustWork = FALSE)
    flag <- FALSE

    if (all(file == loc)) return(file)

    copy <- unique(data.table::data.table(from = file, to = loc))
    flag <- purrr::map2_lgl(copy$from, copy$to, file.copy,
        overwrite = TRUE, copy.date = TRUE)

    if (any(!flag))
        stop("Unable to copy file ", backtick(basename(file[!flag])), "into ",
            "simulation output directory.", call. = FALSE)

    return(loc)
}
# }}}
