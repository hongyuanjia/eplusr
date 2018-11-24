#' @importFrom R6 R6Class
#' @importFrom cli cat_boxx cat_bullet cat_line cat_rule
#' @importFrom crayon bold
#' @importFrom data.table rbindlist set setattr setcolorder
#' @importFrom tools file_path_sans_ext
NULL

#' Create and Run Parametric Analysis, and Collect Results
#'
#' `ParametricJob` class provides a prototype of conducting parametric analysis
#' of EnergyPlus simulations.
#'
#' Basically, it is a collection of multiple `EplusJob` objects. However, the
#'     model is first parsed and the Idf object is stored internally, instead of
#'     storing only the path of Idf in [EplusJob] class. Also, an object
#'     in `Output:SQLite` with `Option Type` value of `SimpleAndTabular` will be
#'     automatically created if it does not exists like [Idf] class.
#'
#' @section Usage:
#' ```
#' param <- param_job(idf, epw)
#' param$seed()
#' param$weater()
#' param$apply_measure(measure, ..., .names = NULL)
#' param$run(dir = NULL, wait = TRUE)
#' param$kill()
#' param$status()
#' param$errors(info = FALSE)
#' param$output_dir(which = NULL)
#' param$locate_output(which = NULL, suffix = ".err", strict = TRUE)
#' param$report_data_dict(which = NULL)
#' param$report_data(which = NULL, key_value = NULL, name = NULL, year = NULL, tz = "GMT", case = "auto")
#' param$tabular_data(which = NULL)
#' job$clone(deep = FALSE)
#' param$print()
#' ```
#' @section Create:
#' ```
#' param <- param_job(idf, epw)
#' ```
#'
#' **Arguments**
#'
#' * `idf`: Path to EnergyPlus IDF file or an `Idf` object.
#' * `epw`: Path to EnergyPlus EPW file or an `Epw` object.
#'
#' @section Get Seed Model and Weather:
#' ```
#' param$seed()
#' param$weather()
#' ```
#'
#' `$seed()` will return the input `Idf` object.
#'
#' `$weather()` will return the input `Epw` object.
#'
#' @section Apply Design Alternatives:
#' ```
#' param$apply_measure(measure, ..., .names = NULL)
#' ```
#'
#' `$apply_measure()` allows to apply a measure to an `Idf` and creates
#'     parametric models for analysis. Basically, a measure is just a function
#'     that takes an `Idf` object and other arguments as input, and returns a
#'     modified `Idf` object as output. Use `...` to supply different arguments
#'     to that measure. Under the hook, [mapply()] is used to create multiple
#'     `Idf`s according to the input values.
#'
#' **Arguments**
#'
#' * `measure`: A function that takes an `Idf` and other arguments as input and
#'     returns an `Idf` object as output.
#' * `...`: Other arguments passed to that `measure`.
#' * `.names`: A character vector of the names of parametric `Idf`s. If `NULL`,
#'     the new `Idf`s will be named in format `measure_name + number`.
#'
#' @section Run and Collect Results:
#' ```
#' param$run(dir = NULL, wait = TRUE)
#' param$kill()
#' param$status()
#' param$errors(info = FALSE)
#' param$output_dir(which = NULL)
#' param$locate_output(which = NULL, suffix = ".err", strict = TRUE)
#' param$report_data_dict(which = NULL)
#' param$report_data(which = NULL, key_value = NULL, name = NULL, year = NULL, tz = "GMT", case = "auto")
#' param$tabular_data(which = NULL)
#' ```
#'
#' All those functions have the same meaning as in [EplusJob] class, except
#' that they only return the results of specified simulations. Most arguments
#' have the same meanings as in [EplusJob] class.
#'
#' `$run()` runs the all parametric simulations in parallel. The number of
#'     parallel EnergyPlus process can be controlled by
#'     `eplusr_option("num_parallel")`. If `wait` is FALSE, then the job will be
#'     run in the background. You can get updated job status by just print the
#'     ParametricJob object.
#'
#' `$kill()` kills the all background EnergyPlus processes that are current
#'     running if possible. It only works when simulation runs in non-waiting
#'     mode.
#'
#' `$status()` returns a named list of values indicates the status of the job:
#'
#'   * `run_before`: `TRUE` if the job has been run before. `FALSE` otherwise.
#'   * `alive`: `TRUE` if the job is still running in the background. `FALSE`
#'     otherwise.
#'   * `terminated`: `TRUE` if the job was terminated during last
#'      simulation. `FALSE` otherwise. `NA` if the job has not been run yet.
#'   * `successful`: `TRUE` if last simulation ended successfully. `FALSE`
#'     otherwise. `NA` if the job has not been run yet.
#'   * `changed_after`: `TRUE` if the *seed model* has been modified since last
#'      simulation. `FALSE` otherwise.
#'
#' `$output_dir()` returns the output directory of specified simulations.
#'
#' `$locate_output()` returns the path of a single output file of specified
#'     simulations.
#'
#' `$report_data_dict()` returns a data.table which contains all information
#'     about report data for specified simulations. For details on the meaning
#'     of each columns, please see "2.20.2.1 ReportDataDictionary Table" in
#'     EnergyPlus "Output Details and Examples" documentation.
#'
#' `$report_data()` extracts the report data in a data.table using key values
#'     and variable names.
#'
#' `$tabular_data()` extracts all tabular data in a data.table.
#'
#' For `$report_data_dict()`, `$report_data()` and `$tabular_data()`, the
#'     returned data.table has a `Case` column in the returned data.table that
#'     indicates the names of parametric models.
#'
#' **Arguments**
#'
#' * `which`: An integer vector of the indexes or a character vector or names of
#'     parametric simulations. If `NULL`, which is the default, results of all
#'     parametric simulations are returned.
#' * `dir`: The parent output directory for all simulation. Outputs of each
#'    simulation are placed in a separate folder under the parent directory.
#' * `wait`: If `TRUE`, R will hang on and wait all EnergyPlus simulations
#'     finish. If `FALSE`, all EnergyPlus simulations are run in the background.
#'     Default: `TRUE`.
#' * `suffix`: A string that indicates the file suffix of simulation output.
#'     Default: `".err"`.
#' * `strict`: If `TRUE`, it will check if the simulation was terminated, is
#'     still running or the file exists or not. Default: `TRUE`.
#' * `key_value`: A character vector to identify key name of the data. If
#'    `NULL`, all keys of that variable will be returned. Default: `NULL`.
#' * `name`: A character vector to specify the actual data name. If `NULL`, all
#'    variables will be returned. Default: `NULL`.
#' * `year`: The year of the date and time in column `DateTime`. If `NULL`, it
#'    will be the current year. Default: `NULL`
#' * `tz`: Time zone of date and time in column `DateTime`. Default: `"GMT"`.
#'
#' @section Clone:
#'
#' ```
#' job$clone(deep = FALSE)
#' ```
#'
#' `$clone()` copies and returns the cloned job. Because `ParametricJob` uses
#'     `R6Class` under the hook which has "modify-in-place" semantics, `job_2 <-
#'     job_1` does not copy `job_1` at all but only create a new binding to
#'     `job_1`. Modify `job_1` will also affect `job_2` as well, as these two
#'     are exactly the same thing underneath. In order to create a complete
#'     cloned copy, please use `$clone(deep = TRUE)`.
#'
#' **Arguments**
#'
#' * `deep`: Has to be `TRUE` if a complete cloned copy is desired.
#'
#' @section Printing:
#' ```
#' param$print()
#' print(param)
#' ```
#'
#' `$print()` shows the core information of this ParametricJob, including the
#'     path of seed model and weather, the version and path of EnergyPlus used
#'     to run simulations, the measured that has been applied and parametric
#'     models generated, and the simulation job status.
#'
#' `$print()` is quite useful to get the simulation status, especially when
#'     `wait` is `FALSE` in `$run()`. The job status will be updated and printed
#'     whenever `$print()` is called.
#'
#' @examples
#' if (is_avail_eplus(8.8)) {
#'     idf_name <- "1ZoneUncontrolled.idf"
#'     epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'
#'     idf_path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", idf_name)
#'     epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData", epw_name)
#'
#'     # create from local files
#'     param_job(idf_path, epw_path)
#'
#'     # create from an Idf and an Epw object
#'     param_job(read_idf(idf_path), read_epw(epw_path))
#' }
#' @docType class
#' @name ParametricJob
#' @author Hongyuan Jia
NULL

#' Create An EnergyPlus Parametric Simulation Job
#'
#' `param_job()` takes an IDF and EPW as input and returns a `ParametricJob`.
#' For details on `ParametricJob`, please see [ParametricJob] class.
#'
#' @param idf A path to EnergyPlus IDF or IMF file or an `Idf` object.
#' @param epw A path to EnergyPlus EPW file or an `Epw` object.
#' @return A `ParametricJob` object.
#' @examples
#' if (is_avail_eplus(8.8)) {
#'     idf_name <- "1ZoneUncontrolled.idf"
#'     epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'
#'     idf_path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", idf_name)
#'     epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData", epw_name)
#'
#'     # create from local files
#'     param_job(idf_path, epw_path)
#'
#'     # create from an Idf and an Epw object
#'     param_job(read_idf(idf_path), read_epw(epw_path))
#' }
#' @seealso [eplus_job()] for creating an EnergyPlus single simulation job.
#' @export
#' @author Hongyuan Jia
# param_job {{{
param_job <- function (idf, epw) {
    Parametric$new(idf, epw)
}
# }}}

# Parametric {{{
Parametric <- R6::R6Class(classname = "ParametricJob", cloneable = FALSE,
    public = list(

        # INITIALIZE {{{
        initialize = function (idf, epw) {

            if (is_idf(idf)) {
                private$m_idf <- idf$clone()
            } else {
                private$m_idf <- read_idf(idf)
            }

            # add sql output
            idf_self <- ._get_self(private$m_idf)
            idf_priv <- ._get_private(private$m_idf)
            i_idf_add_output_sqlite(idf_self, idf_priv)

            # save uuid
            private$m_log$uuid <- idf_priv$m_log$uuid

            if (is_epw(epw)) {
                private$m_epw <- epw
            } else {
                private$m_epw <- read_epw(epw)
            }
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        seed = function ()
            private$m_idf,

        weather = function ()
            private$m_epw,

        apply_measure = function (measure, ..., .names = NULL)
            i_param_apply_measure(self, private, measure, ..., .names = .names),

        run = function (dir = NULL, wait = TRUE)
            i_param_run(self, private, dir, wait),

        kill = function ()
            i_param_kill(self, private),

        status = function ()
            i_param_status(self, private),

        output_dir = function (which = NULL)
            i_param_output_dir(self, private, which),

        locate_output = function (which = NULL, suffix = ".err", strict = TRUE)
            i_param_locate_output(self, private, which, suffix, strict),

        errors = function (which = NULL, info = FALSE)
            i_param_output_errors(self, private, which, info),

        report_data_dict = function (which = NULL)
            i_param_report_data_dict(self, private, which),

        report_data = function (which = NULL, key_value = NULL, name = NULL,
                                all = FALSE, year = NULL, tz = "GMT")
            i_param_report_data(self, private, which, key_value, name, all, year, tz),

        tabular_data = function(which = NULL)
            i_param_tabular_data(self, private, which),

        print = function ()
            i_param_print(self, private)
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_idf = NULL,
        m_epw = NULL,
        m_job = NULL,
        m_sql = NULL,
        m_log = NULL,
        m_param = NULL,
        # }}}

        deep_clone = function (name, value)
            i_deep_clone(self, private, name, value)
    )
)
# }}}

# i_param_apply_measure {{{
i_param_apply_measure <- function (self, private, measure, ..., .names = NULL) {
    assert_that(is.function(measure))

    if (length(formals(measure)) == 0L) {
        stop("'measure' function must have at lease one argument")
    }

    measure_wrapper <- function (idf, ...) {
        assert_that(is_idf(idf))
        idf <- idf$clone(deep = TRUE)
        measure(idf, ...)
    }

    mea_nm <- deparse(substitute(measure, parent.frame()))
    private$m_log$measure_name <- mea_nm

    out <- mapply(measure_wrapper, ...,
        MoreArgs = list(idf = private$m_idf), SIMPLIFY = FALSE, USE.NAMES = FALSE)

    if (is.null(.names)) {
        out_nms <- paste0(mea_nm, "_", seq_along(out))
    } else {
        if (!is_same_len(out, .names))
            stop(length(out), " models created with only ", length(.names),
                " names given.", call. = FALSE)
        nms <- as.character(.names)
        out_nms <- make.names(gsub(" ", "_", fixed = TRUE, make.unique(nms, sep = "_")))
    }

    data.table::setattr(out, "names", out_nms)

    private$m_param <- out

    message("Measure ", backtick(mea_nm), " has been applied with ", length(out),
        " new models created:\n", paste0(seq_along(out_nms), ": ", out_nms, collapse = "\n"))
}
# }}}

# i_param_retrieve_data {{{
i_param_retrieve_data <- function (self, private) {
    status <- i_param_status(self, private)

    if (!status$run_before) return(invisible())

    if (status$alive) {

        private$m_log$stdout <- c(private$m_log$stdout, private$m_job$read_output_lines(10000))
        private$m_log$stderr <- c(private$m_log$stderr, private$m_job$read_error_lines(10000))

    } else {

        if (inherits(private$m_job, "r_process")) {
            private$m_log$stdout <- c(private$m_log$stdout, private$m_job$read_all_output_lines())
            private$m_log$stderr <- c(private$m_log$stderr, private$m_job$read_all_error_lines())
        }

        if (status$successful) {

            if (inherits(private$m_job, "r_process")) {
                private$m_job <- tryCatch(private$m_job$get_result(),
                    error = function (e) {
                        stop("Failed to retrieve output of parametric job. ", e, "\n",
                            private$m_log$stderr, call. = FALSE)
                    }
                )
            }

            if (is.null(private$m_log$end_time)) {
                end_times <- private$m_job[!is.na(end_time), end_time]
                if (not_empty(end_times)) private$m_log$end_time <- max(end_times)
            }
        }
    }
}
# }}}

# i_param_job_from_which {{{
i_param_job_from_which <- function (self, private, which) {
    status <- i_param_status(self, private)

    if (!isTRUE(status$run_before))
        stop("Parametric job did not run before. Please run it using `$run()` ",
            "before collect output", call. = FALSE)

    if (isTRUE(status$terminated))
        stop("Parametric job was terminated before. Please solve ",
            "the problems and re-run it before collect output.", call. = FALSE)

    if (isTRUE(status$alive))
        stop("Parametric job is still running. Please wait it ",
            "to finish before collecting results.", call. = FALSE)

    if (isTRUE(status$run_before) && !isTRUE(status$successful))
        stop("Parametric job ended with errors. Please solve ",
            "the problems and re-run it before collect output.", call. = FALSE)

    if (isTRUE(status$changed_after))
        warning("The seed model has been changed since last run. ",
            "The job output may not be correct.", call. = FALSE)

    # if success, retrieve data
    i_param_retrieve_data(self, private)

    jobs <- private$m_job

    idx <- i_param_case_from_which(self, private, which, name = FALSE)

    job <- jobs[idx]

    if (not_empty(job[status != "completed"])) {
        incomplete <- job[status != "completed"]
        msg <- incomplete[, sim_status(rpad(toupper(status)), index, idf, epw)]
        stop("Some of jobs failed to completed\n:", paste0(msg, collpase = "\n"),
            call. = FALSE
        )
    }

    if (not_empty(job[exit_status != 0L])) {
        unsuccessful <- job[exit_status != 0L]
        msg <- unsuccessful[, sim_status("UNSUCCESSFUL", index, idf, epw)]
        stop("Some of jobs completed unsuccessfully:\n", paste0(msg, collpase = "\n"),
            call. = FALSE
        )
    }

    job
}
# }}}

# i_param_case_from_which {{{
i_param_case_from_which <- function (self, private, which = NULL, name = FALSE) {
    nms <- names(private$m_param)
    if (is.null(which)) {
        if (name) return(nms) else return(seq_along(nms))
    }

    if (is.character(which)) {
        valid <- match(which, nms)
        if (anyNA(valid))
            stop("Invalid job name found for current parametric job: ",
                backtick_collapse(which[is.na(valid)]), ".", call. = FALSE)

        idx <- valid
    } else if (are_count(which)) {
        valid <- which <= length(nms)
        if (any(!valid))
            stop("Invalid job index found for current parametric job: ",
                backtick_collapse(which[!valid]), ".", call. = FALSE)
        idx <- which
    } else {
        stop("`which` should either be a character or an integer vector.",
            call. = FALSE)
    }

    if (name) nms[idx] else idx
}
# }}}

# i_param_run {{{
i_param_run <- function (self, private, output_dir = NULL, wait = TRUE) {
    if (is.null(private$m_param))
        stop("No measure has been applied.", call. = FALSE)

    ver <- private$m_idf$version()

    nms <- names(private$m_param)

    path_idf <- normalizePath(private$m_idf$path(), mustWork = TRUE)
    path_epw <- normalizePath(private$m_epw$path(), mustWork = TRUE)

    if (is.null(output_dir))
        output_dir <- dirname(path_idf)
    else {
        assert_that(is_string(output_dir))
    }

    if (!dir.exists(output_dir)) {
        tryCatch(dir.create(output_dir, recursive = TRUE),
            warning = function (w) {
                stop("Failed to create output directory: ",
                     backtick(output_dir), call. = FALSE)
            }
        )
    }

    path_param <- file.path(output_dir, nms, paste0(nms, ".idf"))

    apply2(private$m_param, path_param, function (x, y) x$save(y, overwrite = TRUE, copy_external = TRUE))

    private$m_log$start_time <- Sys.time()
    private$m_log$killed <- NULL

    tbl <- run_multi(path_param, path_epw, NULL, wait = wait, echo = wait, eplus = ver)

    private$m_job <- tbl

    if (wait) private$m_log$end_time <- Sys.time()

    self
}
# }}}

# i_param_kill {{{
i_param_kill <- function (self, private) {

    if (is.null(private$m_job)) {

        message("The parametric job is not running.")
        return(invisible(FALSE))

    }

    if (!inherits(private$m_job, "process")) {

        message("The parametric job is not running.")
        return(invisible(FALSE))
    }

    proc <- private$m_job

    if (!proc$is_alive()) {
        message("The parametric job is not running.")
        return(invisible(FALSE))
    }

    k <- tryCatch(proc$kill(), error = function (e) FALSE)

    if (isTRUE(k)) {
        message("The parametric job has been successfully killed.")
        private$m_log$killed <- TRUE
        return(invisible(TRUE))
    } else {
        message("Failed to kill parametric job, because it was already finished/dead.")
        return(invisible(FALSE))
    }
}
# }}}

# i_param_status {{{
i_param_status <- function (self, private) {
    status <- list(
        run_before = FALSE, # if the model has been run before
        alive = FALSE, # if simulation is still running
        terminated = NA, # if last simulation was terminated
        successful = NA, # if last simulation was successful
        changed_after = NA # if the seed model has been changed after last simulation
    )

    proc <- private$m_job

    if (is.null(private$m_job)) return(status)

    status$run_before <- TRUE

    if (isTRUE(private$m_log$killed)) {
        status$terminated <- TRUE
    } else {
        status$terminated <- FALSE
    }

    status$changed_after <- FALSE
    if (!identical(private$m_log$uuid, ._get_private(private$m_idf)$m_log$uuid)) {
        status$changed_after <- TRUE
    }

    if (inherits(proc, "r_process")) {
        if (proc$is_alive()) {
            status$alive <- TRUE
        } else {
            status$alive <- FALSE
            proc$wait()
            exit_status <- proc$get_exit_status()
            if (!is.na(exit_status) && exit_status == 0L) {
                status$successful <- TRUE
            } else {
                status$successful <- FALSE
            }
        }

    } else {
        status$alive <- FALSE
        status$successful <- TRUE
    }

    status
}
# }}}

# i_param_output_dir {{{
i_param_output_dir <- function (self, private, which) {
    job <- i_param_job_from_which(self, private, which)
    job$output_dir
}
# }}}

# i_param_locate_output {{{
i_param_locate_output <- function (self, private, which, suffix = ".err", strict = TRUE) {
    job <- i_param_job_from_which(self, private, which)

    out <- paste0(tools::file_path_sans_ext(job$idf), suffix)

    if (strict && any(!file.exists(out))) {
        msg <- job[!file.exists(out), sim_status("MISSING", index, idf, epw)]
        stop("Path does not exist for job:\n", paste0(msg, collpase = "\n"), call. = FALSE)
    }

    out
}
# }}}

# i_param_output_errors {{{
i_param_output_errors <- function (self, private, which, info = FALSE) {
    path_err <- i_param_locate_output(self, private, which, ".err")

    err <- lapply(path_err, parse_err_file)

    names(err) <- i_param_case_from_which(self, private, which, name = TRUE)

    if (!info) {
        err <- lapply(err, function (x) {
            x$data <- x$data[!(level == "Info" & begin_environment == FALSE)]
            x
        })
    }

    err
}
# }}}

# i_param_sql_path {{{
i_param_sql_path <- function (self, private, which) {
    i_param_locate_output(self, private, which, ".sql")
}
# }}}

# i_param_report_data_dict {{{
i_param_report_data_dict <- function (self, private, which) {
    sqls <- i_param_sql_path(self, private, which)
    cases <- i_param_case_from_which(self, private, which, name = TRUE)

    dicts <- lapply(sqls, sql_report_data_dict)

    # add case
    for (idx in seq_along(cases)) {
        data.table::set(dicts[[idx]], j = "Case", value = cases[idx])
        data.table::setcolorder(dicts[[idx]], c("Case", setdiff(names(dicts[[idx]]), "Case")))
    }

    data.table::rbindlist(dicts)
}
# }}}

# i_param_report_data {{{
i_param_report_data <- function (self, private, which = NULL, key_value = NULL,
                                 name = NULL, all = FALSE, year = NULL, tz = "GMT") {
    sqls <- i_param_sql_path(self, private, which)
    cases <- i_param_case_from_which(self, private, which, name = TRUE)

    d <- mapply(sql_report_data, sql = sqls, case = cases,
        MoreArgs = list(key_value = key_value, name = name, all = all, year = year,
            tz = tz),
        SIMPLIFY = FALSE, USE.NAMES = FALSE
    )

    data.table::rbindlist(d)
}
# }}}

# i_param_tabular_data {{{
i_param_tabular_data <- function (self, private, which = NULL) {
    sqls <- i_param_sql_path(self, private, which)
    cases <- i_param_case_from_which(self, private, which, name = TRUE)

    d <- lapply(sqls, sql_tabular_data)

    # add case
    for (idx in seq_along(cases)) {
        data.table::set(d[[idx]], j = "Case", value = cases[idx])
        data.table::setcolorder(d[[idx]], c("Case", setdiff(names(d[[idx]]), "Case")))
    }

    data.table::rbindlist(d)
}
# }}}

# i_param_print {{{
i_param_print <- function (self, private) {
    cli::cat_rule(crayon::bold("EnergPlus Parametric Job"), col = "green")
    config <- eplus_config(private$m_idf$version())
    cli::cat_bullet(c(
        paste0(crayon::bold("Seed Model"), ": ", backtick(normalizePath(private$m_idf$path(), mustWork = FALSE))),
        paste0(crayon::bold("Weather"), ": ", backtick(private$m_epw$path())),
        paste0(crayon::bold("EnergyPlus Version"), ": ", backtick(config$version)),
        paste0(crayon::bold("EnergyPlus Path"), ": ", backtick(normalizePath(config$dir)))
    ), col = "cyan", bullet_col = "cyan")

    if (is.null(private$m_param)) {
        cli::cat_line("<< No measure has been applied >>",
            col = "white", background_col = "blue")
        return(invisible())
    }

    cli::cat_bullet(c(
        paste0(crayon::bold("Applied Measure"), ": ", backtick(private$m_log$measure_name)),
        paste0(crayon::bold("Parametric Models"), " [", length(private$m_param), "]: ")
    ), col = "cyan", bullet_col = "cyan")

    cli::cat_line(paste0("  - ", names(private$m_param), collapse = "\n"),
        col = "cyan"
    )

    status <- i_param_status(self, private)

    i_param_retrieve_data(self, private)

    if (!status$run_before) {

        cli::cat_line("<< Job has not been run before >>",
            col = "white", background_col = "blue")
        return(invisible())
    }

    if (isTRUE(status$terminated)) {

        cli::cat_line(" Job was terminated before.",
            col = "white", background_col = "red")

    } else if (status$alive) {

        cli::cat_line(" Job started at ",
            backtick(private$m_log$start_time), " and is still running...",
            col = "black", background_col = "green"
        )

    } else if (!isTRUE(status$successful)) {

        cli::cat_line(" Job started at ",
            backtick(private$m_log$start_time), " and ended unsuccessfully...",
            col = "white", background_col = "red"
        )

    } else {

        if (!is.null(private$m_log$end_time)) {
            run_time <- format(round(difftime(
                private$m_log$end_time, private$m_log$start_time), digits = 2L)
            )

            cli::cat_line(" Simulation started at ",
                backtick(private$m_log$start_time), " and completed successfully after ",
                run_time, ".",
                col = "black", background_col = "green"
            )

        } else {

            cli::cat_line(" Simulation started at ",
                backtick(private$m_log$start_time), " and completed successfully.",
                col = "black", background_col = "green"
            )
        }
    }

    if (status$alive) {
        if (length(private$m_log$stdout))
            cli::cat_boxx(private$m_log$stdout)

        if (length(private$m_log$stderr)) {
            stderr <- private$m_log$stderr
            safe_width <- getOption("width") - 2L
            stderr_trunc <- vapply(stderr, function (l) {
                if (nchar(l) > safe_width)
                    paste0(substr(l, 1, safe_width), "...")
            }, FUN.VALUE = character(1))

            cli::cat_boxx(stderr_trunc, col = "green", border_col = "green",
                padding = 0)
        }
    }
}
# }}}
