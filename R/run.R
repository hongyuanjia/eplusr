#' @importFrom callr r_bg
#' @importFrom cli cat_line
#' @importFrom crayon red
#' @importFrom data.table data.table setattr setnames
#' @importFrom later later
#' @importFrom lubridate hms with_tz
#' @importFrom tools file_path_sans_ext
#' @importFrom processx process
#' @importFrom progress progress_bar
#' @importFrom tools file_path_sans_ext
NULL

#' Clean working directory of a previous EnergyPlus simulation
#'
#' Clean working directory of an EnergyPlus simulation by deleting all input and
#' output files of previous simulation.
#'
#' @param path An `.idf` or `.imf` file path.
#'
#' @details
#' `clean_wd()` imitates the same process that EnergyPlus does whenever a new
#'     simulation is getting to start. It deletes all related output files that
#'     have the same name prefix as the input path. The input model itself and
#'     any weather file are not deleted. `clean_wd()` is called internally when
#'     running EnergyPlus models using [run_idf()] and [run_multi()].
#'
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

    seperates <- file.path(wd, individual)

    target <- c(out_files, seperates)
    # }}}

    unlink(target[file.exists(target)])
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
#' @param model A path (for `run_idf()`) or a vector of paths (for
#'     `run_multi()`) of EnergyPlus IDF or IMF files.
#' @param weather A path (for `run_idf()`) or a vector of paths (for
#'     `run_multi()`) of EnergyPlus EPW weather files. For `run_multi()`,
#'     `weather` can also be a single EPW file path. In this case, that weather
#'     will be used for all simulations; otherwise, `model` and `weather` should
#'     have the same length.
#' @param output_dir Output directory path (for `rum_idf()`) or paths (for
#'     `run_mult()`). If NULL, the directory of input model is used. For
#'     `run_multi()`, `output_dir`, if not `NULL`, should have the same length
#'     as `model`. Any duplicated combination of `model` and `output_dir` is
#'     prohibited.
#' @param design_day Force design-day-only simulation. Default: `FALSE`.
#' @param annual Force design-day-only simulation. Default: `FALSE`.
#' @param expand_obj Whether to run ExpandObject preprocessor before simulation.
#'     Default: `TRUE`.
#' @param echo Whether to show standard output and error from EnergyPlus
#'     command line interface for `run_idf()` and simulation status for
#'     `run_multi()`.Default: `TRUE`.
#' @param wait If `TRUE`, R will hang on and wait all EnergyPlus simulations
#'     finish. If `FALSE`, all EnergyPlus simulations are run in the background.
#'     and a [processx::process] object is returned. Note that, if `FALSE`, R is
#'     *not blocked* even when `echo` is `TRUE`. Default: `TRUE`.
#' @param eplus An acceptable input (for `run_idf()`) or inputs (for
#'     `run_multi()`) of [use_eplus()] and [eplus_config()]. If
#'     `NULL`, which is the default, the version of EnergyPlus to use is
#'     determined by the version of input model. For `run_multi()`, `eplus`, if not
#'     `NULL`, should have the same length as `model`.
#'
#' @details
#'
#' [later](https://cran.r-project.org/package=later) package is used to poll the
#'     standard output and error of background EnergyPlus process or background
#'     R process that handles parallel simulations. The print interval is set to
#'     0.5 sec.
#'
#' For `run_idf()`, a named list will be returned:
#'
#'   * `idf`: The path of IDF file
#'   * `epw`: The path of EPW file
#'   * `exit_status`: The exit code of the process if it has finished and NULL
#'     otherwise. Always being `NULL` if `wait` is FALSE, but you can manually
#'     get the exit code using the process object, i.e.
#'     `process$get_exit_status()` after simulation *completed*.
#'   * `start_time`: When the EnergyPlus process started.
#'   * `end_time`: When the EnergyPlus process stopped. All being `NULL` if
#'      `wait` is `FALSE`, but you can manually check EnergyPlus `stdout` to get
#'      the simulation time
#'   * `output_dir`: The simulation output directory
#'   * `energyplus`: The path of EnergyPlus executable
#'   * `stdout`: All standard output from EnergyPlus. Always being `NULL` if
#'      `wait` is `FALSE`, but you can manually get all standard output using
#'      `process$read_all_output_lines()`.
#'   * `stderr`: All standard error from EnergyPlus. Always being `NULL` if
#'      `wait` is `FALSE`, but you can manually get all standard output using
#'      `process$read_all_output_lines()`.
#'   * `process`: A [processx::process] object of current EnergyPlus simulation
#'
#' For `run_multi()`, if `wait` is `TRUE`, a
#' [data.table][data.table::data.table()] contains all data (excluding
#' `process`) with same column names as above, and also another two columns:
#'
#'   * `index`: The index of simulation
#'   * `status`: The status of simulation status. Should be one of below:
#'       - `"completed"`: the simulation job is completed. This only indicates
#'          that the calling of EnergyPlus was successfully and EnergyPlus was
#'          not terminated during simulation. Even `"completed" is `TRUE`, the
#'          job can still end with error.  Please check `exit_status` to
#'          determine whether EnergyPlus ran successfully
#'       - `"terminated"`: the simulation job started but was terminated
#'       - `"cancelled"`: the simulation job was cancelled, i.e. did not start
#'          at all.
#'
#' For `run_multi()`, if `wait` is `FALSE`, a [r_process][callr::r_bg()]
#'     object of background R process which handles all simulation jobs is
#'     returned. You can check if the jobs are completed using `$is_alive()` and
#'     get the final data.table using `$get_result()`.
#'
#' It is suggested to run simulations using [EplusJob] class and
#'     [ParametricJob] class, which provide much more detailed controls
#'     on the simulation and also methods to extract simulation outputs.
#'
#' @return A list for `run_idf()`. For `rum_multi()`, a
#' [data.table][data.table::data.table()] if `wait` is `TRUE` or a
#' [process][processx::process] if `wait` is `FALSE`.
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
#'     run_idf(idf_path, epw_path, output_dir = tempdir())
#'
#'     # run multiple model in parallel
#'     idf_paths <- file.path(eplus_config(8.8)$dir, "ExampleFiles",
#'         c("1ZoneUncontrolled.idf", "1ZoneUncontrolledFourAlgorithms.idf")
#'     )
#'     epw_paths <- rep(epw_path, times = 2L)
#'     output_dirs <- file.path(tempdir(), tools::file_path_sans_ext(basename(idf_paths)))
#'     run_multi(idf_paths, epw_paths, output_dir = output_dirs)
#' }
#' }
#' @rdname run_model
#' @seealso [EplusJob] class and [ParametricJob] class which provide
#' a more friendly interface to run EnergyPlus simulations and collect outputs.
#' @author Hongyuan Jia
#' @export
# run_idf {{{
run_idf <- function (model, weather, output_dir, design_day = FALSE,
                     annual = FALSE, expand_obj = TRUE, wait = TRUE, echo = TRUE, eplus = NULL) {

    model <- normalizePath(model, mustWork = TRUE)
    weather <- normalizePath(weather, mustWork = TRUE)

    eplus <- eplus %||% as.character(get_idf_ver(read_lines(model)))
    energyplus_exe <- eplus_exe(eplus)

    if (is_empty(eplus)) {
        stop("Missing version field in input IDF file. Failed to determine the ",
            "version of EnergyPlus to use.", call. = FALSE)
    }

    # get output directory
    if (is.null(output_dir)) output_dir <- dirname(model)
    output_dir <- normalizePath(output_dir, mustWork = FALSE)
    if (!dir.exists(output_dir)) {
        tryCatch(dir.create(output_dir, recursive = TRUE),
            warning = function (w) {
                stop("Failed to create output directory: ",
                     surround(output_dir), call. = FALSE)
            }
        )
    }

    # copy input files
    loc_m <- copy_run_files(model, output_dir)
    loc_w <- copy_run_files(weather, output_dir)

    # clean wd
    clean_wd(loc_m)

    res <- energyplus(energyplus_exe, loc_m, loc_w, output_dir = output_dir,
        annual = annual, design_day = design_day, expand_obj = expand_obj,
        wait = wait, echo = echo)

    res$idf <- model
    res$epw <- weather
    res$version <- as.character(eplus_config(eplus)$version)

    res[c("idf", "epw", "exit_status", "start_time", "end_time", "output_dir",
        "energyplus", "stdout", "stderr", "process")]
}
# }}}

#' @export
#' @rdname run_model
# run_multi {{{
run_multi <- function (model, weather, output_dir, design_day = FALSE,
                       annual = FALSE, wait = TRUE, echo = TRUE, eplus = NULL) {
    assert(is_flag(design_day))
    assert(is_flag(annual))
    assert(is_flag(wait))
    assert(is_flag(echo))

    if (annual && design_day) {
        stop("Cannot force both design-day and annual simulations.", call. = FALSE)
    }

    if (!is_scalar(model) && !is_scalar(weather))
        assert(have_same_len(model, weather))

    if (!is_scalar(model) && !is.null(eplus) && !is_scalar(eplus))
        assert(have_same_len(model, eplus))

    model <- normalizePath(model, mustWork = TRUE)
    weather <- normalizePath(weather, mustWork = TRUE)

    if (is.null(eplus)) {
        ver_list <- lapply(model, function (x) {
            as.character(get_idf_ver(read_lines(x)))
        })
        ver_miss <- vapply(ver_list, is_empty, logical(1))
        if (any(ver_miss)) {
            msg <- paste0("  ", seq_along(model)[ver_miss], "| ", surround(model[ver_miss]),
                collapse = "\n")
            stop("Missing version field in input IDF file. Failed to determine the ",
                "version of EnergyPlus to use:\n", msg, call. = FALSE)
        }

        eplus <- unlist(ver_list)
    }

    energyplus_exe <- vapply(eplus, eplus_exe, FUN.VALUE = character(1))

    if (anyDuplicated(model) & is.null(output_dir))
        stop("`model` cannot have any duplications when `output_dir` is NULL.",
            call. = FALSE)

    if (is.null(output_dir)) {
        output_dir <- dirname(model)
    } else {
        assert(have_same_len(model, output_dir))
    }

    output_dir <- normalizePath(output_dir, mustWork = FALSE)

    jobs <- data.table::data.table(input_model = model, output_dir = output_dir)

    if (anyDuplicated(jobs))
        stop("Duplication found in the combination of `model` and `output_dir`.",
            " One model could not be run in the same output directory multiple ",
            "times simultaneously.", call. = FALSE)

    d <- output_dir[!dir.exists(output_dir)]
    created <- vapply(d, dir.create, logical(1L), showWarnings = FALSE, recursive = TRUE)
    if (any(!created))
        stop("Failed to create output directory:\n",
            paste0(surround(d[!created]), collapse = "\n"), call. = FALSE)

    jobs[, `:=`(input_weather = weather, energyplus = energyplus_exe)]
    jobs[, `:=`(model = copy_run_files(model, output_dir),
                 weather = copy_run_files(weather, output_dir),
                 index = .I, annual = annual, design_day = design_day)]

    options <- list(num_parallel = eplusr_option("num_parallel"), echo = echo)

    if (wait) {
        run_parallel_jobs(jobs, options)
    } else {
        ext_funs <- tempfile("eplusr_run_parallel_jobs_fun", fileext = ".eplusr_temp")

        base::dump(list = c("run_parallel_jobs", "kill_jobs", "schedule_next_sim",
            "run_job", "are_all_completed", "handle_events", "sim_status", "clean_wd",
            "energyplus", "is_string", "is_flag", "has_ext", "lpad", "surround", "assert",
            "is_integer", "is_scalar"),
            file = ext_funs
        )

        # always echo in order to catch standard output and error
        options$echo <- TRUE
        proc <- callr::r_bg(function (ext_funs, jobs, options) {
            requireNamespace("data.table", quietly = TRUE)
            source(ext_funs)
            run_parallel_jobs(jobs, options)
        }, args = list(ext_funs = ext_funs, jobs = jobs, options = options))

        # watch_proc {{{
        # reference: https://yihui.name/en/2017/10/later-recursion
        watch_proc = function() {
            if (proc_print(proc, c(TRUE, TRUE))) {
                later::later(watch_proc, 0.5)
            }
        }
        # }}}

        if (echo) watch_proc()

        proc
    }

}
# }}}

# proc_print {{{
# reference: https://github.com/rstudio/blogdown/blob/master/R/serve.R
proc_print <- function(p, control = c(TRUE, TRUE)) {
    if (!p$is_alive()) {
        # We might still have output
        while (p$is_incomplete_output() || p$is_incomplete_error()) {
            p$poll_io(-1)
            out <- p$read_output_lines(2000)
            err <- p$read_error_lines(2000)
            if (length(out)) cli::cat_line(out, file = stdout(), col = "green")
            if (length(err)) cli::cat_line(err, file = stdout(), col = "white", background_col = "red")
        }
        return(FALSE)
    }

    if (control[1]) {
        out <- p$read_output_lines()
        if (length(out)) cli::cat_line(out, file = stdout(), col = "green")
    }

    if (control[2]) {
        err <- p$read_error_lines()
        if (length(err)) cli::cat_line(err, file = stderr(), col = "white", background_col = "red")
    }

    TRUE
}
# }}}
# run_parallel_jobs {{{
# reference: https://github.com/r-lib/revdepcheck/blob/master/R/event-loop.R
run_parallel_jobs <- function(jobs, options) {
    if (nrow(jobs) == 0) return()
    assert(is_integer(options$num_parallel))

    start_time <- Sys.time()

    ## Kill all child processes if we quit from this function
    on.exit(kill_jobs(jobs, options), add = TRUE)

    # initialize job status and worker
    jobs[, `:=`(status = "waiting", index_str = lpad(index), process = list(),
        stdout = list(), stderr = list(), exit_status = NA_integer_
    )]

    # Our global progress bar
    progress_bar <- progress::progress_bar$new(
        total = nrow(jobs), clear = FALSE,
        format = "[:current/:total | :percent] :bar [Elapsed: :elapsedfull]"
    )

    # Initialise one task for each worker
    num <- min(options$num_parallel, nrow(jobs))
    for (i in seq_len(num)) {
        progress_bar$tick(0)
        jobs <- schedule_next_sim(jobs, options, progress_bar)
        jobs <- run_job(jobs, options, progress_bar)
    }

    num_head <- num_tail <- 0L
    while (1) {
        if (are_all_completed(jobs)) break;
        progress_bar$tick(0)
        jobs <- handle_events(jobs, options, progress_bar)
        jobs <- schedule_next_sim(jobs, options, progress_bar)
        jobs <- run_job(jobs, options, progress_bar)
    }

    jobs[, c("model", "weather") := NULL]
    data.table::setnames(jobs, c("input_model", "input_weather"), c("idf", "epw"))

    jobs[, .SD, .SDcols = c("index", "status", "idf", "epw", "exit_status",
        "start_time", "end_time", "energyplus", "output_dir", "stdout", "stderr"
    )]

    end_time <- Sys.time()

    data.table::setattr(jobs, "start_time", start_time)
    data.table::setattr(jobs, "end_time", end_time)

    jobs
}
# }}}
# kill_jobs {{{
kill_jobs <- function(jobs, options) {
    jobs[vapply(process, function (x) {!is.null(x) && x$is_alive()}, logical(1)), `:=`(
        status = {for (p in process) p$kill(); "terminated"}
    )]

    jobs[status %in% c("waiting", "ready"), `:=`(status = "cancelled")]


    if (any(jobs$status == "terminated")) {
        jobs[status == "terminated", `:=`(
            stdout = lapply(process, function (x) x$read_all_output_lines()),
            stderr = lapply(process, function (x) x$read_all_error_lines()),
            exit_status = vapply(process, function (x) x$get_exit_status(), integer(1))
        )]
    }

    if (options$echo) {

        if (any(jobs$status == "terminated")) {
            terminated <- jobs[status == "terminated",
                sim_status("terminate", index_str, model, weather)
            ]

            cli::cat_line(terminated, col = "white", background_col = "red")
        }

        if (any(jobs$status == "cancelled")) {
            cancelled <- jobs[status == "cancelled",
                sim_status("cancel", index_str, model, weather)
            ]

            cli::cat_line(cancelled, col = "white", background_col = "red")
        }
    }
}
# }}}
# schedule_next_sim {{{
schedule_next_sim <- function(jobs, options, progress_bar) {
    # Cannot run more workers?
    if (sum(jobs$status == "running") >= options$num_parallel) {
        return(jobs)
    }

    # waiting -> running
    ready <- jobs$status == "waiting"
    if (any(ready)) {
        jobs[jobs[ready, index[1L]], `:=`(status = "ready")]
    }

    jobs
}
# }}}
# run_job {{{
run_job <- function(jobs, options, progress_bar) {
    # clean wd
    lapply(jobs[status == "ready", model], clean_wd)

    jobs[status == "ready", `:=`(status = "newly_started",
        process = list(energyplus(eplus = energyplus, model = model,
            weather = weather, output_dir = output_dir, annual = annual,
            design_day = design_day, wait = FALSE, echo = FALSE)$process)
    )]

    data.table::data.table(x = 1:5)[, c("y", "z") := list("a", "b")][]
    if (any(jobs$status == "newly_started")) {
        completed <- jobs[status == "newly_started",
            sim_status("run", index_str, model, weather)
        ]

        if (options$echo) {
            progress_bar$message(paste0(completed, collapse = "\n"))
        }
        progress_bar$tick(0)
    }

    jobs[status == "newly_started", `:=`(status = "running",
        start_time = do.call("c", lapply(process, function (x) x$get_start_time()))
    )]

    jobs
}
# }}}
# are_all_completed {{{
are_all_completed <- function(jobs) {
    all(jobs$status == "completed")
}
# }}}
# handle_events {{{
handle_events <- function(jobs, options, progress_bar) {
    jobs[status == "running" &
         vapply(process, function (x) {!is.null(x) && !x$is_alive()}, logical(1)),
        `:=`(
            stdout = lapply(process, function (x) x$read_all_output_lines()),
            stderr = lapply(process, function (x) x$read_all_error_lines()),
            exit_status = vapply(process, function (x) x$get_exit_status(), integer(1)),
            status = "newly_completed", end_time = Sys.time()
        )
    ]

    if (any(jobs$status == "newly_completed")) {
        num <- sum(jobs$status == "newly_completed")

        completed <- jobs[status == "newly_completed",
            sim_status("complete", index_str, model, weather)
        ]

        if (options$echo) {
            progress_bar$message(paste0(completed, collapse = "\n"))
        }
        progress_bar$tick(num)
    }

    jobs[status == "newly_completed", `:=`(status = "completed")]

    jobs
}
# }}}
# sim_status {{{
sim_status <- function (type, index, model, weather) {
    status <- c("run", "complete", "cancel", "terminate")
    if (type %in% status) {
        type <- switch(type,
            run       = "RUNNING   ",
            complete  = "COMPLETED ",
            cancel    = "CANCELLED ",
            terminate = "TERMINATED"
        )
    }

    paste0(lpad(index), "|", type, " --> ",
        "[IDF]", surround(basename(model)),
        " + ",
        "[EPW]", surround(basename(weather))
    )
}
# }}}
# energyplus {{{
energyplus <- function (eplus, model, weather, output_dir, output_prefix = NULL,
                        output_suffix = c("C", "L", "D"), expand_obj = TRUE,
                        readvars = TRUE, annual = FALSE, design_day = FALSE,
                        idd = NULL, echo = TRUE, wait = TRUE) {

    output_suffix <- match.arg(output_suffix)

    assert(
        file.exists(eplus),
        file.exists(model),
        file.exists(weather),
        is.null(output_dir) || dir.exists(output_dir),
        is.null(output_prefix) || is_string(output_prefix),
        is_flag(expand_obj),
        is_flag(readvars),
        is_flag(annual),
        is_flag(design_day),
        is.null(idd) || file.exists(idd),
        is_flag(echo),
        is_flag(wait)
    )

    if (annual && design_day) {
        stop("Cannot force both design-day and annual simulations.", call. = FALSE)
    }

    # argument docs {{{
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
    if (is.null(output_dir)) output_dir <- dirname(model)
    if (is.null(output_prefix)) output_prefix <- tools::file_path_sans_ext(basename(model))

    if (has_ext(model, "imf")) {
        cmd_epmacro <- "--epmacro"
    } else {
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
        cmd_epmacro,
        cmd_expand_obj,
        cmd_readvars,
        cmd_annual,
        cmd_design_day,
        cmd_idd,
        model
    )

    res <- list()

    # run energyplus
    proc <- processx::process$new(command = eplus, args = args, wd = dirname(model),
        cleanup = TRUE, windows_verbatim_args = TRUE,
        stdout = "|", stderr = "|", post_process = function () Sys.time()
    )

    # kill energyplus on exit
    exit_callback <- function () {
        if (!proc$is_alive()) return(NULL)

        k <- tryCatch(proc$kill(), error = function (e) FALSE)

        if (k) {
            msg <- paste0("TERMINATED--> [Idf]`", basename(model), "` + [Epw]`", basename(weather), "`")
            cli::cat_line(msg, col = "white", background_col = "red")
        }
    }

    if (wait) {
        # kill the process when function exits
        on.exit(exit_callback(), add = TRUE)

        res <- eplus_run_wait(proc, echo = echo)
    } else {
        # watch_proc {{{
        # reference: https://yihui.name/en/2017/10/later-recursion
        watch_proc = function() {
            if (proc_print(proc, c(TRUE, TRUE))) {
                later::later(watch_proc, 0.5)
            }
        }
        # }}}
        if (echo) watch_proc()

        # TODO: exit time
        # just return the process
        res <- list(
            process = proc,
            exit_status = NULL,
            stdout = NULL,
            stderr = NULL,
            start_time = lubridate::with_tz(proc$get_start_time(), Sys.timezone()),
            end_time = NULL
        )
    }

    # add more data
    res$output_dir <- output_dir
    res$energyplus <- eplus

    res
}
# }}}
# eplus_run_wait {{{
eplus_run_wait <- function (proc, echo = TRUE) {
    stdout <- c()
    stderr <- c()

    # get_output {{{
    get_output <- function (echo = TRUE) {
        newout <- proc$read_output_lines(2000)
        if (echo) cli::cat_line(newout)
        if (length(newout) && all(nzchar(newout))) {
            stdout <<- c(stdout, newout)
        }

        newerr <- proc$read_error(2000)
        if (echo) cat(crayon::red(newerr), sep = "")
        if (length(newerr) && all(nzchar(newerr))) {
            stderr <<- c(stderr, newerr)
        }
    }
    # }}}

    while (proc$is_alive()) {
        polled <- proc$poll_io(200)

        ## If output/error, then collect it
        if (any(polled == "ready")) get_output(echo)
    }

    # Needed to get the exit status
    proc$wait()

    # We might still have output
    while (proc$is_incomplete_output() || proc$is_incomplete_error()) {
        proc$poll_io(-1)
        get_output(echo)
    }

    list(process = proc,
         exit_status = proc$get_exit_status(),
         stdout = stdout,
         stderr = stderr,
         start_time = lubridate::with_tz(proc$get_start_time(), Sys.timezone()),
         end_time = Sys.time()
    )
}
# }}}
# eplus_exe {{{
eplus_exe <- function (eplus) {
    if (!is_avail_eplus(eplus)) use_eplus(eplus)
    config <- tryCatch(eplus_config(eplus), warning = function (w) stop(w))
    normalizePath(file.path(config$dir, config$exe), mustWork = TRUE)
}
# }}}
# copy_run_files {{{
copy_run_files <- function (file, dir) {
    file <- normalizePath(file, mustWork = TRUE)
    loc <- normalizePath(file.path(dir, basename(file)), mustWork = FALSE)
    flag <- FALSE

    if (all(file == loc)) return(file)

    copy <- unique(data.table::data.table(from = file, to = loc))
    flag <- apply2_int(copy$from, copy$to, file.copy,
        more_args = list(overwrite = TRUE, copy.date = TRUE)
    )

    if (any(!flag))
        stop("Unable to copy file ", surround(basename(file[!flag])), "into ",
            "simulation output directory.", call. = FALSE)

    return(loc)
}
# }}}
# get_run_time {{{
get_run_time <- function (stdout) {
    last <- stdout[length(stdout)]

    period <- lubridate::hms(last, quiet = TRUE)
    if (is.na(period)) return(NULL)
    period
}
# }}}
