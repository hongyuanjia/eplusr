#' @importFrom R6 R6Class
#' @importFrom cli cat_boxx cat_line cat_rule
#' @importFrom data.table rbindlist set setattr setcolorder
#' @importFrom tools file_path_sans_ext
NULL

#' Create and Run Parametric Analysis, and Collect Results
#'
#' `ParametricJob` class provides a prototype of conducting parametric analysis
#' of EnergyPlus simulations.
#'
#' Basically, it is a collection of multiple `EplusJob` objects. However, the
#' model is first parsed and the [Idf] object is stored internally, instead of
#' storing only the path of Idf like in [EplusJob] class. Also, an object in
#' `Output:SQLite` with `Option Type` value of `SimpleAndTabular` will be
#' automatically created if it does not exists, like [Idf] class does.
#'
#' @section Usage:
#' ```
#' param <- param_job(idf, epw)
#' param$version()
#' param$seed()
#' param$weater()
#' param$apply_measure(measure, ..., .names = NULL, .mix = FALSE)
#' param$run(dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE)
#' param$kill()
#' param$status()
#' param$errors(info = FALSE)
#' param$output_dir(which = NULL)
#' param$locate_output(which = NULL, suffix = ".err", strict = TRUE)
#' param$report_data_dict(which = NULL)
#' param$report_data(which = NULL, key_value = NULL, name = NULL, year = NULL, tz = "UTC", case = "auto", all = FALSE,
#'                   period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
#'                   interval = NULL, simulation_days = NULL, day_type = NULL, environment_name = NULL)
#' param$tabular_data(which, report_name = NULL, report_for = NULL, table_name = NULL, column_name = NULL, row_name = NULL)
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
#' param$version()
#' param$seed()
#' param$weather()
#' ```
#'
#' `$version()` returns the version of input [Idf] object.
#'
#' `$seed()` returns the input [Idf] object.
#'
#' `$weather()` returns the input [Epw] object.
#'
#' @section Apply Design Alternatives:
#' ```
#' param$apply_measure(measure, ..., .names = NULL)
#' ```
#'
#' `$apply_measure()` allows to apply a measure to an [Idf] and creates
#' parametric models for analysis. Basically, a measure is just a function that
#' takes an [Idf] object and other arguments as input, and returns a modified
#' [Idf] object as output. Use `...` to supply different arguments to that
#' measure. Under the hook, [base::mapply()] is used to create multiple [Idf]s
#' according to the input values.
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
#' param$run(dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE)
#' param$kill()
#' param$status()
#' param$errors(info = FALSE)
#' param$output_dir(which = NULL)
#' param$locate_output(which = NULL, suffix = ".err", strict = TRUE)
#' param$report_data_dict(which = NULL)
#' param$report_data(which = NULL, key_value = NULL, name = NULL, year = NULL, tz = "UTC", case = "auto", all = FALSE,
#'                   period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
#'                   interval = NULL, simulation_days = NULL, day_type = NULL, environment_name = NULL)
#' param$tabular_data(which, report_name = NULL, report_for = NULL, table_name = NULL, column_name = NULL, row_name = NULL)
#' ```
#'
#' All those functions have the same meaning as in [EplusJob] class, except
#' that they only return the results of specified simulations. Most arguments
#' have the same meanings as in [EplusJob] class.
#'
#' `$run()` runs all parametric simulations in parallel. The number of parallel
#' EnergyPlus process can be controlled by `eplusr_option("num_parallel")`. If
#' `wait` is FALSE, then the job will be run in the background. You can get
#' updated job status by just printing the `ParametricJob` object.
#'
#' `$kill()` kills all background EnergyPlus processes that are current running
#' if possible. It only works when simulations run in non-waiting mode.
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
#' $errors() returns an [ErrFile][read_err()] object which contains all contents
#' of the simulation error file (`.err`). If `info` is `FALSE`, only warnings
#' and errors are printed.
#'
#' `$output_dir()` returns the output directory of specified simulations.
#'
#' `$locate_output()` returns the path of a single output file of specified
#' simulations.
#'
#' `$report_data_dict()` returns a [data.table][data.table::data.table()] which
#' contains all information about report data for specified simulations. For
#' details on the meaning of each columns, please see "2.20.2.1
#' ReportDataDictionary Table" in EnergyPlus "Output Details and Examples"
#' documentation.
#'
#' `$report_data()` extracts the report data in a
#' [data.table][data.table::data.table()] using key values, variable names and other arguments.
#'
#' `$tabular_data()` extracts tabular data in a
#' [data.table][data.table::data.table()].
#'
#' For convenience, input character arguments matching in `$report_data()` and
#' `$tabular_data()` are **case-insensitive**.
#'
#' For `$report_data_dict()`, `$report_data()` and `$tabular_data()`, the
#' returned data.table has a `case` column in the returned
#' [data.table][data.table::data.table()] that indicates the names of parametric
#' models. For detailed documentation of those methods, please see [EplusSql].
#'
#' **Arguments**
#'
#' * `which`: An integer vector of the indexes or a character vector or names of
#'   parametric simulations. If `NULL`, results of all parametric simulations
#'   are returned. Default: `NULL`.
#' * `dir`: The parent output directory for specified simulations. Outputs of
#'   each simulation are placed in a separate folder under the parent directory.
#' * `wait`: If `TRUE`, R will hang on and wait all EnergyPlus simulations
#'   finish. If `FALSE`, all EnergyPlus simulations are run in the background.
#'   Default: `TRUE`.
#' * `force`: Only applicable when the last simulation runs with `wait` equals
#'   to `FALSE` and is still running. If `TRUE`, current running job is
#'   forced to stop and a new one will start. Default: `FALSE`.
#' * `copy_external`: If `TRUE`, the external files that every `Idf` object
#'   depends on will also be copied into the simulation output directory. The
#'   values of file paths in the Idf will be changed automatically. Currently,
#'   only `Schedule:File` class is supported.  This ensures that the output
#'   directory will have all files needed for the model to run. Default is
#'   `FALSE`.
#' * `suffix`: A string that indicates the file extension of simulation output.
#'   Default: `".err"`.
#' * `strict`: If `TRUE`, it checks if the simulation was terminated, is
#'   still running or the file does not exist. Default: `TRUE`.
#' * `info`: If `FALSE`,only warnings and errors are printed. Default: `FALSE`.
#' * `key_value`: A character vector to identify key values of the data. If
#'   `NULL`, all keys of that variable will be returned. `key_value` can also be
#'   data.frame that contains `key_value` and `name` columns. In this case,
#'   `name` argument in `$report_data()` is ignored. All available `key_value`
#'   for current simulation output can be obtained using `$report_data_dict()`.
#'   Default: `NULL`.
#' * `name`: A character vector to identify names of the data. If
#'   `NULL`, all names of that variable will be returned. If `key_value` is a
#'   data.frame, `name` is ignored. All available `name` for current simulation
#'   output can be obtained using `$report_data_dict()`.  Default: `NULL`.
#' * `year`: Year of the date time in column `datetime`. If `NULL`, it
#'    will calculate a year value that meets the start day of week restriction
#'    for each environment. Default: `NULL`.
#' * `tz`: Time zone of date time in column `datetime`. Default: `"UTC"`.
#' * `case`: If not `NULL`, a character column will be added indicates the case
#'   of this simulation. If `"auto"`, the name of the IDF file without extension
#'   is used.
#' * `all`: If `TRUE`, extra columns are also included in the returned
#'   [data.table][data.table::data.table()].
#' * `period`: A Date or POSIXt vector used to specify which time period to
#'    return. The year value does not matter and only month, day, hour and
#'    minute value will be used when subsetting. If `NULL`, all time period of
#'    data is returned. Default: `NULL`.
#' * `month`, `day`, `hour`, `minute`: Each is an integer vector for month, day,
#'    hour, minute subsetting of `datetime` column when querying on the SQL
#'    database. If `NULL`, no subsetting is performed on those components. All
#'    possible `month`, `day`, `hour` and `minute` can be obtained using
#'    `$read_table("Time")`.  Default: `NULL`.
#' * `interval`: An integer vector used to specify which interval length of
#'    report to extract. If `NULL`, all interval will be used. Default: `NULL`.
#' * `simulation_days`: An integer vector to specify which simulation day data
#'    to extract. Note that this number resets after warmup and at the beginning
#'    of an environment period. All possible `simulation_days` can be obtained
#'    using `$read_table("Time")`. If `NULL`, all simulation days will be used.
#'    Default: `NULL`.
#' * `day_type`: A character vector to specify which day type of data to
#'    extract. All possible day types are: `Sunday`, `Monday`, `Tuesday`,
#'   `Wednesday`, `Thursday`, `Friday`, `Saturday`, `Holiday`,
#'   `SummerDesignDay`, `WinterDesignDay`, `CustomDay1`, and `CustomDay2`. All
#'   possible values for current simulation output can be obtained using
#'   `$read_table("Time")`.
#' * `environment_name`: A character vector to specify which environment data to
#'    extract. All possible `environment_name` for current simulation output can
#'    be obtained using `$read_table("EnvironmentPeriods"). `If `NULL`, all
#'    environment data are returned. Default: `NULL`.
#' * `report_name`, `report_for`, `table_name`, `column_name`, `row_name`:
#'   Each is a character vector for subsetting when querying the SQL database.
#'   For the meaning of each argument, please see the description above.
#'
#' @section Printing:
#' ```
#' param$print()
#' print(param)
#' ```
#'
#' `$print()` shows the core information of this `ParametricJob`, including the
#' path of seed model and weather, the version and path of EnergyPlus used to
#' run simulations, the measure that has been applied and parametric models
#' generated, and also the simulation job status.
#'
#' `$print()` is quite useful to get the simulation status, especially when
#' `wait` is `FALSE` in `$run()`. The job status will be updated and printed
#' whenever `$print()` is called.
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
                private$m_idf <- idf$clone(deep = TRUE)
            } else {
                private$m_idf <- read_idf(idf)
            }

            # add sql output
            idf_self <- ._get_self(private$m_idf)
            idf_priv <- ._get_private(private$m_idf)
            idf_add_output_sqlite(private$m_idf)

            # save uuid
            private$m_log$uuid <- idf_priv$m_log$uuid

            if (is_epw(epw)) {
                private$m_epw <- epw$clone(deep = TRUE)
            } else {
                private$m_epw <- read_epw(epw)
            }
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        version = function ()
            param_version(self, private),

        seed = function ()
            param_seed(self, private),

        weather = function ()
            param_weather(self, private),

        apply_measure = function (measure, ..., .names = NULL)
            param_apply_measure(self, private, measure, ..., .names = .names),

        run = function (dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE)
            param_run(self, private, dir, wait, force, copy_external),

        kill = function ()
            param_kill(self, private),

        status = function ()
            param_status(self, private),

        output_dir = function (which = NULL)
            param_output_dir(self, private, which),

        locate_output = function (which = NULL, suffix = ".err", strict = TRUE)
            param_locate_output(self, private, which, suffix, strict),

        errors = function (which = NULL, info = FALSE)
            param_output_errors(self, private, which, info),

        report_data_dict = function (which = NULL)
            param_report_data_dict(self, private, which),

        report_data = function (which = NULL, key_value = NULL, name = NULL,
                                year = NULL, tz = "UTC", all = FALSE,
                                period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                                interval = NULL, simulation_days = NULL, day_type = NULL,
                                environment_name = NULL)
            param_report_data(self, private, which,
                key_value = key_value, name = name, year = year, tz = tz, all = all,
                period = period, month = month, day = day, hour = hour, minute = minute,
                interval = interval, simulation_days = simulation_days, day_type = day_type,
                environment_name = environment_name
            ),

        tabular_data = function(which = NULL, report_name = NULL, report_for = NULL,
                                table_name = NULL, column_name = NULL, row_name = NULL)
            param_tabular_data(self, private, which, report_name = report_name,
                report_for = report_for, table_name = table_name,
                column_name = column_name, row_name = row_name),

        print = function ()
            param_print(self, private)
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_idf = NULL,
        m_epw = NULL,
        m_job = NULL,
        m_log = NULL,
        m_param = NULL
        # }}}
    )
)
# }}}

# param_version {{{
param_version <- function (self, private) {
    private$m_idf$version()
}
# }}}
# param_seed {{{
param_seed <- function (self, private) {
    private$m_idf
}
# }}}
# param_weather {{{
param_weather <- function (self, private) {
    private$m_epw
}
# }}}
# param_apply_measure {{{
param_apply_measure <- function (self, private, measure, ..., .names = NULL) {
    assert(is.function(measure))

    if (length(formals(measure)) == 0L) {
        abort("error_measure_no_arg", "`measure` function must have at lease one argument.")
    }

    measure_wrapper <- function (idf, ...) {
        assert(is_idf(idf), msg = paste0("Measure should take an `Idf` object as input, not `", class(idf)[[1]], "`."))
        idf <- idf$clone(deep = TRUE)
        idf <- measure(idf, ...)
        assert(is_idf(idf), msg = paste0("Measure should return an `Idf` object, not `", class(idf)[[1]], "`."))
        idf
    }

    mea_nm <- deparse(substitute(measure, parent.frame()))
    private$m_log$measure_name <- mea_nm

    out <- mapply(measure_wrapper, ...,
        MoreArgs = list(idf = private$m_idf), SIMPLIFY = FALSE, USE.NAMES = FALSE)

    if (is.null(.names)) {
        out_nms <- paste0(mea_nm, "_", seq_along(out))
    } else {
        assert(have_same_len(out, .names),
            msg = paste0(length(out), " models created with only ", length(.names), " names given.")
        )

        nms <- as.character(.names)
        out_nms <- make.names(gsub(" ", "_", fixed = TRUE, make.unique(nms, sep = "_")))
    }

    setattr(out, "names", out_nms)

    private$m_param <- out

    message("Measure ", surround(mea_nm), " has been applied with ", length(out),
        " new models created:\n", paste0(seq_along(out_nms), ": ", out_nms, collapse = "\n"))
}
# }}}
# param_retrieve_data {{{
param_retrieve_data <- function (self, private) {
    status <- param_status(self, private)

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
# param_job_from_which {{{
param_job_from_which <- function (self, private, which, keep_unsucess = FALSE) {
    status <- param_status(self, private)

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
    param_retrieve_data(self, private)

    jobs <- private$m_job

    idx <- param_case_from_which(self, private, which, name = FALSE)

    job <- jobs[idx]

    if (not_empty(job[status != "completed"])) {
        incomplete <- job[status != "completed"]
        msg <- incomplete[, sim_status(rpad(toupper(status)), index, idf, epw)]
        stop("Some of jobs failed to completed\n:", paste0(msg, collapse = "\n"),
            call. = FALSE
        )
    }

    # setting `keep_unsucess` to TRUE makes it possible to continue to parse
    # some output files such like .err files. (#24)
    if (isTRUE(!keep_unsucess) && not_empty(job[exit_status != 0L])) {
        unsuccessful <- job[exit_status != 0L]
        msg <- unsuccessful[, sim_status("UNSUCCESSFUL", index, idf, epw)]
        stop("Some of jobs completed unsuccessfully:\n", paste0(msg, collapse = "\n"),
            call. = FALSE
        )
    }

    job
}
# }}}
# param_case_from_which {{{
param_case_from_which <- function (self, private, which = NULL, name = FALSE) {
    nms <- names(private$m_param)
    if (is.null(which)) {
        if (name) return(nms) else return(seq_along(nms))
    }

    if (is.character(which)) {
        valid <- match(which, nms)
        if (anyNA(valid))
            stop("Invalid job name found for current parametric job: ",
                collapse(which[is.na(valid)]), ".", call. = FALSE)

        idx <- valid
    } else if (all(are_count(which))) {
        valid <- which <= length(nms)
        if (any(!valid))
            stop("Invalid job index found for current parametric job: ",
                collapse(which[!valid]), ".", call. = FALSE)
        idx <- which
    } else {
        stop("`which` should either be a character or an integer vector.",
            call. = FALSE)
    }

    if (name) nms[idx] else idx
}
# }}}
# param_run {{{
param_run <- function (self, private, output_dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE) {
    if (is.null(private$m_param))
        stop("No measure has been applied.", call. = FALSE)

    ver <- private$m_idf$version()

    nms <- names(private$m_param)

    path_idf <- normalizePath(private$m_idf$path(), mustWork = TRUE)
    path_epw <- normalizePath(private$m_epw$path(), mustWork = TRUE)

    if (is.null(output_dir))
        output_dir <- dirname(path_idf)
    else {
        assert(is_string(output_dir))
    }

    if (!dir.exists(output_dir)) {
        tryCatch(dir.create(output_dir, recursive = TRUE),
            warning = function (w) {
                stop("Failed to create output directory: ",
                     surround(output_dir), call. = FALSE)
            }
        )
    }

    # check if the model is still running
    old <- private$m_job
    if (!is.null(old)) {
        # update status
        param_retrieve_data(self, private)
        old <- private$m_job

        # check if running in non-waiting mode
        if (inherits(old, "process") && old$is_alive()) {
            pid <- old$get_pid()
            if (force) {
                message("Force to kill all current running parametric simulations (",
                    "Parent R Process PID: ", pid, ") and restart...")
                suppressMessages(self$kill())
            } else {
                stop("Current parametric simulations are still running (Parent R Process PID: ",
                    pid, "). Please set `force` to TRUE if you want ",
                    "to kill the running process and restart.",
                    call. = FALSE)
            }
        }
    }

    path_param <- file.path(output_dir, nms, paste0(nms, ".idf"))

    apply2(private$m_param, path_param, function (x, y) x$save(y, overwrite = TRUE, copy_external = copy_external))

    private$m_log$start_time <- Sys.time()
    private$m_log$killed <- NULL

    tbl <- run_multi(path_param, path_epw, NULL, wait = wait, echo = wait, eplus = ver)

    private$m_job <- tbl

    if (wait) private$m_log$end_time <- Sys.time()

    self
}
# }}}
# param_kill {{{
param_kill <- function (self, private) {

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
# param_status {{{
param_status <- function (self, private) {
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
# param_output_dir {{{
param_output_dir <- function (self, private, which = NULL) {
    param_job_from_which(self, private, which)$output_dir
}
# }}}
# param_locate_output {{{
param_locate_output <- function (self, private, which = NULL, suffix = ".err", strict = TRUE, keep_unsucess = FALSE) {
    job <- param_job_from_which(self, private, which, keep_unsucess = keep_unsucess)

    out <- paste0(tools::file_path_sans_ext(job$idf), suffix)

    if (strict && any(!file.exists(out))) {
        msg <- job[!file.exists(out), sim_status("MISSING", index, idf, epw)]
        stop("Path does not exist for job:\n", paste0(msg, collapse = "\n"), call. = FALSE)
    }

    out
}
# }}}
# param_output_errors {{{
param_output_errors <- function (self, private, which, info = FALSE) {
    # continue to parse err file for jobs having non-zero exits (#24)
    path_err <- param_locate_output(self, private, which, ".err", keep_unsucess = TRUE)

    err <- lapply(path_err, parse_err_file)

    names(err) <- param_case_from_which(self, private, which, name = TRUE)

    if (!info) err <- lapply(err, function (x) x[!(level == "Info")])

    err
}
# }}}
# param_sql_path {{{
param_sql_path <- function (self, private, which) {
    param_locate_output(self, private, which, ".sql")
}
# }}}
# param_report_data_dict {{{
param_report_data_dict <- function (self, private, which) {
    sqls <- param_sql_path(self, private, which)
    cases <- param_case_from_which(self, private, which, name = TRUE)

    dicts <- lapply(sqls, get_sql_report_data_dict)

    # add case
    for (idx in seq_along(cases)) {
        set(dicts[[idx]], j = "case", value = cases[idx])
        setcolorder(dicts[[idx]], c("case", setdiff(names(dicts[[idx]]), "case")))
    }

    rbindlist(dicts)
}
# }}}
# param_report_data {{{
param_report_data <- function (self, private, which = NULL, key_value = NULL,
                               name = NULL, year = NULL, tz = "GMT", all = FALSE,
                               period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                               interval = NULL, simulation_days = NULL, day_type = NULL,
                               environment_name = NULL) {
    sqls <- param_sql_path(self, private, which)
    cases <- param_case_from_which(self, private, which, name = TRUE)

    rbindlist(mapply(get_sql_report_data, sql = sqls, case = cases,
        MoreArgs = list(key_value = key_value, name = name, all = all, year = year,
            tz = tz, period = period, month = month, day = day, hour = hour, minute = minute,
            interval = interval, simulation_days = simulation_days, day_type = day_type,
            environment_name = environment_name),
        SIMPLIFY = FALSE, USE.NAMES = FALSE
    ))
}
# }}}
# param_tabular_data {{{
param_tabular_data <- function (self, private, which = NULL, report_name = NULL, report_for = NULL,
                                table_name = NULL, column_name = NULL, row_name = NULL) {
    sqls <- param_sql_path(self, private, which)
    cases <- param_case_from_which(self, private, which, name = TRUE)

    d <- lapply(sqls, get_sql_tabular_data,
        report_name = report_name, report_for = report_for,
        table_name = table_name, column_name = column_name, row_name = row_name
    )

    # add case
    for (idx in seq_along(cases)) {
        set(d[[idx]], j = "case", value = cases[idx])
        setcolorder(d[[idx]], c("case", setdiff(names(d[[idx]]), "case")))
    }

    rbindlist(d)
}
# }}}
# param_print {{{
param_print <- function (self, private) {
    cli::cat_rule("EnergPlus Parametric Job", col = "green")
    config <- eplus_config(private$m_idf$version())
    cli::cat_line(c(
        str_trunc(paste0("Seed Model: ", surround(normalizePath(private$m_idf$path(), mustWork = FALSE)))),
        str_trunc(paste0("Weather: ", surround(private$m_epw$path()))),
        paste0("EnergyPlus Version: ", surround(config$version)),
        paste0("EnergyPlus Path: ", surround(normalizePath(config$dir)))
    ))

    if (is.null(private$m_param)) {
        cli::cat_line("<< No measure has been applied >>",
            col = "white", background_col = "blue")
        return(invisible())
    }

    cli::cat_line(c(
        paste0("Applied Measure: ", surround(private$m_log$measure_name)),
        paste0("Parametric Models [", length(private$m_param), "]: ")
    ))

    cli::cat_line(paste0("  - ", names(private$m_param), collapse = "\n"))

    status <- param_status(self, private)

    param_retrieve_data(self, private)

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
            surround(private$m_log$start_time), " and is still running...",
            col = "black", background_col = "green"
        )
    } else if (!isTRUE(status$successful)) {
        cli::cat_line(" Job started at ",
            surround(private$m_log$start_time), " and ended unsuccessfully...",
            col = "white", background_col = "red"
        )
    } else {
        if (!is.null(private$m_log$end_time)) {
            run_time <- format(round(difftime(
                private$m_log$end_time, private$m_log$start_time), digits = 2L)
            )
            cli::cat_line(" Simulation started at ",
                surround(private$m_log$start_time), " and completed successfully after ",
                run_time, ".",
                col = "black", background_col = "green"
            )
        } else {
            cli::cat_line(" Simulation started at ",
                surround(private$m_log$start_time), " and completed successfully.",
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
# S3 ParametricJob methods {{{
#' @export
str.ParametricJob <- function (object, ...) {
    object$print()
}

#' @export
format.ParametricJob <- function (x, ...) {
    paste0(utils::capture.output(x$print()), collapse = "\n")
}
# }}}
