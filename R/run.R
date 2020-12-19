#' @importFrom callr r_bg
#' @importFrom checkmate assert_flag assert_file_exists assert_directory_exists
#' @importFrom checkmate assert_logical
#' @importFrom cli cat_line
#' @importFrom crayon red
#' @importFrom data.table data.table setattr setnames
#' @importFrom lubridate with_tz
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
#' # run a test simulation
#' idf_path <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
#' epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData",
#'      "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#' )
#' dir <- file.path(tempdir(), "test")
#' run_idf(idf_path, epw_path, output_dir = dir, echo = FALSE)
#'
#' list.files(dir)
#'
#' # remove all output files
#' clean_wd(file.path(dir, basename(idf_path)))
#'
#' list.files(dir)
#' }
#' @export
#' @author Hongyuan Jia
# clean_wd {{{
clean_wd <- function (path) {
    assert_string(path)
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
#' @param model A path (for `run_idf()`) or a vector of paths (for
#'        `run_multi()`) of EnergyPlus IDF or IMF files.
#'
#' @param weather A path (for `run_idf()`) or a vector of paths (for
#'        `run_multi()`) of EnergyPlus EPW weather files. For `run_multi()`,
#'        `weather` can also be a single EPW file path. In this case, that
#'        weather will be used for all simulations; otherwise, `model` and
#'        `weather` should have the same length.
#'
#' @param output_dir Output directory path (for `rum_idf()`) or paths (for
#'        `run_mult()`). If NULL, the directory of input model is used. For
#'        `run_multi()`, `output_dir`, if not `NULL`, should have the same
#'        length as `model`. Any duplicated combination of `model` and
#'        `output_dir` is prohibited.
#'
#' @param design_day Force design-day-only simulation. For `rum_multi()`,
#'        `design_day` can also be a logical vector which has the same length as
#'        `model`. Default: `FALSE`.
#'
#' @param annual Force design-day-only simulation. For `rum_multi()`,
#'        `annual` can also be a logical vector which has the same length as
#'        `model`. Note that `design_day` and `annual` cannot be all `TRUE` at
#'        the same time. Default: `FALSE`.
#'
#' @param expand_obj Whether to run ExpandObject preprocessor before simulation.
#'        Default: `TRUE`.
#'
#' @param echo Only applicable when `wait` is `TRUE`. Whether to show standard
#'        output and error from EnergyPlus command line interface for
#'        `run_idf()` and simulation status for `run_multi()`. Default: `TRUE`.
#'
#' @param wait If `TRUE`, R will hang on and wait all EnergyPlus simulations
#'        finish. If `FALSE`, all EnergyPlus simulations are run in the
#'        background, and a [process][processx::process] object is returned.
#'
#' @param eplus An acceptable input (for `run_idf()`) or inputs (for
#'        `run_multi()`) of [use_eplus()] and [eplus_config()]. If `NULL`, which
#'        is the default, the version of EnergyPlus to use is determined by the
#'        version of input model. For `run_multi()`, `eplus`, if not `NULL`,
#'        should have the same length as `model`.
#'
#' @details
#'
#' `run_idf()` is a wrapper of EnergyPlus command line interface which enables to
#' run EnergyPlus model with different options.
#'
#' `run_multi()` provides the functionality of running multiple models in
#' parallel.
#'
#' `run_idf()` and `run_multi()` currently only support EnergyPlus v8.3 and
#' above. This is because eplusr uses EnergyPlus command line interface which is
#' a new feature as of EnergyPlus v8.3.
#'
#' It is suggested to run simulations using [EplusJob] class and [EplusGroupJob]
#' class, which provide much more detailed controls on the simulation and also
#' methods to extract simulation outputs.
#'
#' @return
#'
#' * For `run_idf()`, a named list of 11 elements:
#'
#'   | No.  | Column        | Type                         | Description                                                                          |
#'   | ---: | -----         | -----                        | -----                                                                                |
#'   | 1    | `idf`         | `character(1)`               | Full path of input IDF file                                                          |
#'   | 2    | `epw`         | `character(1)` or `NULL`     | Full path of input EPW file                                                          |
#'   | 3    | `version`     | `character(1)`               | Version of called EnergyPlus                                                         |
#'   | 4    | `exit_status` | `integer(1)` or `NULL`       | Exit status of EnergyPlus. `NULL` if terminated or `wait` is `FALSE`                 |
#'   | 5    | `start_time`  | `POSIXct(1)`                 | Start of time of simulation                                                          |
#'   | 6    | `end_time`    | `POSIXct(1)` or `NULL`       | End of time of simulation. `NULL` if `wait` is `FALSE`                               |
#'   | 7    | `output_dir`  | `character(1)`               | Full path of simulation output directory                                             |
#'   | 8    | `energyplus`  | `character(1)`               | Full path of called EnergyPlus executable                                            |
#'   | 9    | `stdout`      | `character(1)` or `NULL`     | Standard output of EnergyPlus during simulation                                      |
#'   | 10   | `stderr`      | `character(1)` or `NULL`     | Standard error of EnergyPlus during simulation                                       |
#'   | 11   | `process`     | [process][processx::process] | A [process][processx::process] object which called EnergyPlus and ran the simulation |
#'
#' * For `rum_multi()`, if `wait` is TRUE, a
#'   [data.table][data.table::data.table()] of 12 columns:
#'
#'   | No.  | Column        | Type        | Description                                                      |
#'   | ---: | -----         | -----       | -----                                                            |
#'   | 1    | `index`       | `integer`   | Index of simulation                                              |
#'   | 2    | `status`      | `character` | Simulation status                                                |
#'   | 3    | `idf`         | `character` | Full path of input IDF file                                      |
#'   | 4    | `epw`         | `character` | Full path of input EPW file. `NA` for design-day-only simulation |
#'   | 5    | `version`     | `character` | Version of EnergyPlus                                            |
#'   | 6    | `exit_status` | `integer`   | Exit status of EnergyPlus. `NA` if terminated                    |
#'   | 7    | `start_time`  | `POSIXct`   | Start of time of simulation                                      |
#'   | 8    | `end_time`    | `POSIXct`   | End of time of simulation.                                       |
#'   | 9    | `output_dir`  | `character` | Full path of simulation output directory                         |
#'   | 10   | `energyplus`  | `character` | Full path of called EnergyPlus executable                        |
#'   | 11   | `stdout`      | `list`      | Standard output of EnergyPlus during simulation                  |
#'   | 12   | `stderr`      | `list`      | Standard error of EnergyPlus during simulation                   |
#'
#'   For column `status`, there are 4 possible values:
#'   - `"completed"`: the simulation job is completed successfully
#'   - `"failed"`: the simulation job ended with error
#'   - `"terminated"`: the simulation job started but was terminated
#'   - `"cancelled"`: the simulation job was cancelled, i.e. did not start at all
#'
#' * For `run_multi()`, if `wait` is `FALSE`, a [r_process][callr::r_bg()]
#'   object of background R process which handles all simulation jobs is
#'   returned. You can check if the jobs are completed using `$is_alive()` and
#'   get the final data.table using `$get_result()` method.
#'
#' @references
#' [Running EnergyPlus from Command Line (EnergyPlus GitHub Repository)](https://github.com/NREL/EnergyPlus/blob/develop/doc/running-energyplus-from-command-line.md)
#'
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
    if (!is.null(weather)) weather <- normalizePath(weather, mustWork = TRUE)

    eplus <- eplus %||% as.character(get_idf_ver(read_lines(model)))
    if (!length(eplus)) {
        abort(paste0("Missing version field in input IDF file. ",
            "Failed to determine the version of EnergyPlus to use."),
            "miss_idf_ver"
        )
    }

    energyplus_exe <- eplus_exe(eplus)

    # get output directory
    if (is.null(output_dir)) output_dir <- dirname(model)
    output_dir <- normalizePath(output_dir, mustWork = FALSE)
    if (!dir.exists(output_dir)) {
        tryCatch(dir.create(output_dir, recursive = TRUE),
            warning = function (w) abort(paste0("Failed to create output directory: ", surround(output_dir)), "create_output_dir")
        )
    }

    # copy input files
    loc_m <- copy_run_files(model, output_dir)
    if (is.null(weather)) {
        loc_w <- NULL
    } else {
        loc_w <- copy_run_files(weather, output_dir)
    }

    # clean wd
    clean_wd(loc_m)

    # Let energyplus commandline interface itself to handle output file
    # directory. This is the easiest way to external file dependencies, instead
    # of doing it on the R side.
    # See #344
    res <- energyplus(energyplus_exe, model, weather, output_dir = output_dir,
        annual = annual, design_day = design_day, expand_obj = expand_obj,
        wait = wait, echo = echo)

    res$idf <- model
    # in case no weather is given
    res["epw"] <- list(weather)
    res$version <- as.character(eplus_config(eplus)$version)

    res[c("idf", "epw", "version", "exit_status", "start_time", "end_time", "output_dir",
        "energyplus", "stdout", "stderr", "process")]
}
# }}}

#' @export
#' @rdname run_model
# run_multi {{{
run_multi <- function (model, weather, output_dir, design_day = FALSE,
                       annual = FALSE, expand_obj = TRUE, wait = TRUE, echo = TRUE, eplus = NULL) {
    assert_flag(wait)
    assert_flag(echo)
    assert_logical(design_day, any.missing = FALSE)
    assert_logical(annual, any.missing = FALSE)
    assert_logical(expand_obj, any.missing = FALSE)

    if (length(model) != 1L) {
        if (!is.null(weather) && length(weather) != 1L) {
            assert_same_len(model, weather)
        }
        if (!is.null(eplus) && length(eplus) != 1L) {
            assert_same_len(model, eplus)
        }
        if (length(design_day) != 1L) {
            assert_same_len(model, design_day)
        }
        if (length(annual) != 1L) {
            assert_same_len(model, annual)
        }
        if (length(expand_obj) != 1L) {
            assert_same_len(model, expand_obj)
        }
    }

    if (any(annual & design_day)) {
        abort("Cannot force both design-day-only simulation and annual simulation at the same time",
            "both_ddy_annual"
        )
    }

    model <- normalizePath(model, mustWork = TRUE)

    if (is.null(weather)) {
        input_weather <- rep(NA_character_, length(model))
        weather <- list(NULL)
    } else {
        if (length(weather) == 1L) weather <- rep(weather, length(model))
        ddy <- is.na(weather)
        input_weather <- weather
        input_weather[!ddy] <- normalizePath(input_weather[!ddy], mustWork = TRUE)
        weather <- as.list(weather)
        weather[ddy] <- list(NULL)
    }

    if (is.null(eplus)) {
        ver_list <- lapply(model, function (x) as.character(get_idf_ver(read_lines(x))))
        ver_miss <- viapply(ver_list, length) == 0L
        if (any(ver_miss)) {
            msg <- paste0("  #", lpad(seq_along(model)[ver_miss]), "| ", surround(model[ver_miss]),
                collapse = "\n")
            abort(paste0("Missing version field in input IDF file. Failed to determine the ",
                "version of EnergyPlus to use:\n", msg), "miss_idf_ver")
        }

        ver <- unlist(ver_list)
        energyplus_exe <- vcapply(ver, eplus_exe)
        ver <- vcapply(ver, function (v) as.character(eplus_config(v)$version))
    } else {
        energyplus_exe <- vcapply(eplus, eplus_exe)
        ver <- vcapply(eplus, function (v) as.character(eplus_config(v)$version))
    }

    if (anyDuplicated(model) & is.null(output_dir)) {
        abort("'model' cannot have any duplications when 'output_dir' is NULL.", "duplicated_sim")
    }

    if (is.null(output_dir)) {
        output_dir <- dirname(model)
    } else {
        assert_same_len(model, output_dir)
    }

    output_dir <- normalizePath(output_dir, mustWork = FALSE)

    jobs <- data.table::data.table(input_model = model, output_dir = output_dir)

    if (anyDuplicated(jobs))
        stop(paste0("Duplication found in the combination of 'model' and 'output_dir'. ",
            "One model could not be run in the same output directory multiple times simultaneously."),
            "duplicated_sim"
        )

    d <- unique(output_dir[!dir.exists(output_dir)])
    created <- vapply(d, dir.create, logical(1L), showWarnings = FALSE, recursive = TRUE)
    if (any(!created)) {
        abort(paste0("Failed to create output directory:\n",
            paste0(surround(d[!created]), collapse = "\n")
        ))
    }

    jobs[, `:=`(
        energyplus = energyplus_exe,
        model = copy_run_files(model, output_dir), version = ver,
        index = .I, annual = annual, design_day = design_day, expand_obj = expand_obj
    )]

    if (is.null(weather)) {
        set(jobs, NULL, c("input_weather", "weather"), list(NA_character_, list(NULL)))
    } else {
        if (any(!is.na(input_weather))) {
            weather[!is.na(input_weather)] <- as.list(
                copy_run_files(
                    unlist(weather[!is.na(input_weather)]),
                    output_dir[!is.na(input_weather)]
                )
            )
        }
        set(jobs, NULL, c("input_weather", "weather"), list(input_weather, weather))
    }

    options <- list(num_parallel = eplusr_option("num_parallel"), echo = echo)

    if (wait) {
        run_parallel_jobs(jobs, options)
    } else {
        # always echo in order to catch standard output and error
        options$echo <- TRUE
        callr::r_bg(function (jobs, options) {
            utils::getFromNamespace("run_parallel_jobs", "eplusr")(jobs, options)
        }, args = list(jobs = jobs, options = options))
    }
}
# }}}

# run_parallel_jobs {{{
# reference: https://github.com/r-lib/revdepcheck/blob/master/R/event-loop.R
run_parallel_jobs <- function(jobs, options) {
    if (nrow(jobs) == 0) return()
    assert_count(options$num_parallel, positive = TRUE)

    # in case run in background
    jobs <- setDT(jobs)

    ## Kill all child processes if we quit from this function
    on.exit(kill_jobs(jobs, options), add = TRUE)

    # initialize job status and worker
    set(jobs, NULL, c("status", "index_str", "process", "stdout", "stderr", "exit_status", "start_time", "end_time"),
        list("waiting", lpad(jobs$index, "0"), list(), list(), list(), NA_integer_, as.POSIXct(NA), as.POSIXct(NA))
    )
    setindexv(jobs, "status")

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

    # Run until all simulation complete
    while (TRUE) {
        if (are_all_completed(jobs)) break;
        progress_bar$tick(0)
        jobs <- handle_events(jobs, options, progress_bar)
        jobs <- schedule_next_sim(jobs, options, progress_bar)
        jobs <- run_job(jobs, options, progress_bar)
    }

    set(jobs, NULL, c("model", "weather"), NULL)
    setnames(jobs, c("input_model", "input_weather"), c("idf", "epw"))

    jobs[, .SD, .SDcols = c("index", "status", "idf", "epw", "version", "exit_status",
        "start_time", "end_time", "output_dir", "energyplus", "stdout", "stderr"
    )]
}
# }}}
# kill_jobs {{{
kill_jobs <- function(jobs, options) {
    jobs[vlapply(process, function (x) inherits(x, "process") && x$is_alive()), `:=`(
        status = {for (p in process) p$kill(); "terminated"}
    )]

    jobs[J(c("waiting", "ready")), on = "status", status := "cancelled"]

    if (any(is_term <- jobs$status == "terminated")) {
        jobs[is_term, `:=`(
            stdout = lapply(process, function (x) tryCatch(x$read_all_output_lines(), error = function (e) NA_character_)),
            stderr = lapply(process, function (x) tryCatch(x$read_all_error_lines(), error = function (e) NA_character_)),
            exit_status = viapply(process, function (x) x$get_exit_status())
        )]
    }

    if (options$echo) {
        if (any(is_term)) {
            terminated <- jobs[is_term, sim_status("terminate", index_str, model, weather)]
            cat_line(terminated, col = "white", background_col = "red")
        }

        if (any(is_canc <- jobs$status == "cancelled")) {
            cancelled <- jobs[is_canc, sim_status("cancel", index_str, model, weather)]
            cat_line(cancelled, col = "white", background_col = "red")
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
    # always schedule only one new job
    if (any(ready <- jobs$status == "waiting")) {
        set(jobs, jobs$index[ready][1L], "status", "ready")
    }

    jobs
}
# }}}
# run_job {{{
run_job <- function(jobs, options, progress_bar) {
    # clean wd
    ready <- which(jobs$status == "ready")

    if (!length(ready)) return(jobs)

    jobs[ready, c("status", "process", "start_time") := {
        clean_wd(model)

        process <- energyplus(eplus = energyplus, model = model,
            weather = unlist(weather), output_dir = output_dir, annual = annual,
            design_day = design_day, wait = FALSE, echo = FALSE,
            expand_obj = expand_obj)$process

        if (options$echo) {
            run <- sim_status("run", index_str, model, weather)
            progress_bar$message(paste0(run, collapse = "\n"))
        }
        progress_bar$tick(0)

        start_time <- lubridate::with_tz(process$get_start_time(), Sys.timezone())

        list(status = "running", process = list(process), start_time = start_time)
    }]
}
# }}}
# are_all_completed {{{
are_all_completed <- function(jobs) {
    all(jobs$status %chin% c("completed", "failed"))
}
# }}}
# handle_events {{{
handle_events <- function(jobs, options, progress_bar) {
    run <- jobs$status == "running"
    if (!any(run)) return(jobs)

    jobs[run & vlapply(process, function (x) !is.null(x) && !x$is_alive()),
        c("stdout", "stderr", "exit_status", "status", "end_time") := {
            res <- lapply(process, function (p) p$get_result())

            # somehow get_exit_status() function may return NA after execution
            # of a (successful) command
            # ref: https://github.com/r-lib/processx/issues/220
            exit_code <- viapply(process, function (x) x$get_exit_status())
            exit_code[is.na(exit_code)] <- 0L

            if (options$echo) {
                comp <- sim_status("complete", index_str, model, weather, exit_code)
                progress_bar$message(paste0(comp, collapse = "\n"))
            }
            progress_bar$tick(.N)

            status[exit_code == 0L] <- "completed"
            status[exit_code != 0L] <- "failed"

            list(stdout = lapply(res, "[[", "stdout"), stderr = lapply(res, "[[", "stderr"),
                 exit_status = exit_code, status = status, end_time = Sys.time()
            )
        }
    ]
}
# }}}
# sim_status {{{
sim_status <- function (type, index, model, weather, exit_code = NULL) {
    status <- c("run", "complete", "cancel", "terminate")
    if (length(type) == 1L && type %in% status) {
        type <- switch(type,
            run       = "RUNNING   ",
            complete  = "COMPLETED ",
            cancel    = "CANCELLED ",
            terminate = "TERMINATED"
        )
        if (!is.null(exit_code)) type[exit_code != 0L] <- "FAILED    "
    }

    mes <- paste0(lpad(index, "0"), "|", type, " --> ",
        "[IDF]", surround(basename(model))
    )

    has_epw <- !vlapply(weather, function (x) is.null(x) || is.na(x))

    if (any(has_epw)) {
        mes[has_epw] <- paste0(mes, " + ", "[EPW]", surround(basename(unlist(weather[has_epw]))))
    }

    mes
}
# }}}
# energyplus {{{
energyplus <- function (eplus, model, weather, output_dir, output_prefix = NULL,
                        output_suffix = c("C", "L", "D"), expand_obj = TRUE,
                        readvars = TRUE, annual = FALSE, design_day = FALSE,
                        idd = NULL, echo = TRUE, wait = TRUE) {

    output_suffix <- match.arg(output_suffix)

    assert_file_exists(eplus)
    assert_file_exists(model)
    assert_flag(expand_obj)
    assert_flag(readvars)
    assert_flag(annual)
    assert_flag(design_day)
    assert_flag(echo)
    assert_flag(wait)
    if (!is.null(weather)) assert_file_exists(weather)
    if (!is.null(output_dir)) assert_directory_exists(output_dir, "w")
    if (!is.null(output_prefix)) assert_string(output_prefix)
    if (!is.null(idd)) assert_file_exists(idd)

    if (annual && design_day) {
        abort("Cannot force both design-day and annual simulations", "both_ddy_annual")
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

    if (readvars) cmd_readvars <- "--readvars" else cmd_readvars <- NULL
    if (annual) cmd_annual <- "--annual" else cmd_annual <- NULL
    if (design_day) cmd_design_day <- "--design-day" else cmd_design_day <- NULL
    if (!is.null(idd)) cmd_idd <- paste0("--idd", shQuote(idd)) else cmd_idd <- NULL
    # }}}

    # manually run ExpandObjects first
    # see #213
    # backup original model path
    model_ori <- model
    expanded <- FALSE
    if (expand_obj) {
        model <- expand_objects(eplus, model, keep_ext = FALSE)
        expanded <- attr(model, "expanded")
    }

    arg_weather <- if (is.null(weather)) NULL else c("--weather", weather)
    args <- c(
        arg_weather,
        "--output-directory", output_dir,
        "--output-prefix", output_prefix,
        "--output-suffix", output_suffix,
        cmd_epmacro,
        cmd_readvars,
        cmd_annual,
        cmd_design_day,
        cmd_idd,
        model
    )

    if (expanded) {
        model_exp <- model
        model <- model_ori
        # _exp.idf --> .expidf
        rename_exp <- function () {
            path <- basename(paste0(stri_sub(tools::file_path_sans_ext(model_exp), to = -5L), ".expidf"))
            try(file.rename(model_exp, file.path(output_dir, path)), silent = TRUE)
        }
    }

    res <- list()

    if (wait) {
        p_stdout <- p_stderr <- "|"
        post_fun <- NULL
    # should always poll stdout and stderr
    # see https://github.com/r-lib/processx/issues/235
    } else {
        p_stdout <- tempfile()
        p_stderr <- tempfile()
        post_fun <- function () {
            stdout <- suppressWarnings(read_lines(p_stdout)$string)
            stderr <- suppressWarnings(read_lines(p_stderr)$string)
            if (!length(stdout)) stdout <- character(0)
            if (!length(stderr)) stderr <- character(0)
            if (expanded) rename_exp()
            unlink(c(p_stdout, p_stderr))
            list(stdout = stdout, stderr = stderr, end_time = Sys.time())
        }
    }

    # run energyplus
    proc <- processx::process$new(command = eplus, args = args, wd = dirname(model),
        cleanup = TRUE, windows_verbatim_args = FALSE,
        stdout = p_stdout, stderr = p_stderr, post_process = post_fun
    )

    # kill energyplus on exit
    exit_callback <- function () {
        if (expanded) rename_exp()
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
    config <- tryCatch(eplus_config(eplus),
        eplusr_warning_miss_eplus_config = function (w) abort(conditionMessage(w), "miss_eplus_config")
    )

    if (config$version < 8.3) {
        abort(paste0("Currently, eplusr only supports running IDFs of EnergyPlus v8.3 and above. ",
             "This is because eplusr uses EnergyPlus command line interface ",
             "which is available only in EnergyPlus v8.3 and above. ",
             "You can update the version of your model using 'transition()' or 'version_updater()' and try again."),
             "eplus_ver_not_supported"
        )
    }

    normalizePath(file.path(config$dir, config$exe), mustWork = TRUE)
}
# }}}
# copy_run_files {{{
copy_run_files <- function (file, dir) {
    file <- normalizePath(file, mustWork = TRUE)
    loc <- normalizePath(file.path(dir, basename(file)), mustWork = FALSE)
    flag <- FALSE

    if (all(file == loc)) return(file)

    copy <- unique(data.table(from = file, to = loc))
    flag <- apply2_int(copy$from, copy$to, file.copy,
        more_args = list(overwrite = TRUE, copy.date = TRUE)
    )

    if (any(!flag))
        abort(paste0("Unable to copy file ", surround(basename(file[!flag])), "into ",
            "simulation output directory."), "copy_run_files")

    loc
}
# }}}
# get_run_time {{{
get_run_time <- function (stdout) {
    if (!length(stdout)) return(NULL)
    last <- stdout[length(stdout)]

    period <- lubridate::hms(last, quiet = TRUE)
    if (is.na(period)) NULL else period
}
# }}}
# expand_objects {{{
expand_objects <- function (eplus, idf, keep_ext = FALSE) {
    exe <- file.path(dirname(eplus), paste0("ExpandObjects", if (is_windows()) ".exe" else ""))

    dir <- dirname(idf)

    # create "in.idf"
    file.copy(idf, file.path(dir, "in.idf"), overwrite = TRUE, copy.date = TRUE)

    # create ini file
    ini <- file.path(dir, "Energy+.ini")
    write_lines(c("[program]", sprintf("dir=%s", normalizePath(dirname(eplus)))), ini)

    # run ExpandObjects
    processx::run(exe, wd = dir)

    # remove ini file
    unlink(ini, force = TRUE)

    # get output file path
    if (keep_ext) {
        out <- file.path(dir, paste0(tools::file_path_sans_ext(basename(idf)), ".expidf"))
    } else {
        out <- file.path(dir, paste0(tools::file_path_sans_ext(basename(idf)), "_exp.idf"))
    }

    # rename genereated expidf file
    if (file.exists(file.path(dir, "in.expidf"))) {
        file.rename(file.path(dir, "in.expidf"), out)
        expanded <- TRUE
    } else if (file.exists(file.path(dir, "expanded.idf"))) {
        file.rename(file.path(dir, "expanded.idf"), out)
        expanded <- TRUE
    } else {
        expanded <- FALSE
        unlink(file.path(dir, "in.idf"), force  = TRUE)
        unlink(file.path(dir, "expandedidf.err"), force  = TRUE)
        unlink(file.path(dir, "fort.6"), force  = TRUE)
    }

    unlink(file.path(dirname(idf), "in.idf"), force  = TRUE)
    unlink(file.path(dirname(idf), "expandedidf.err"), force  = TRUE)
    unlink(file.path(dirname(idf), "fort.6"), force  = TRUE)
    if (file.exists(out)) {
        attr(out, "expanded") <- expanded
        out
    } else {
        attr(idf, "expanded") <- expanded
        idf
    }
}
# }}}
