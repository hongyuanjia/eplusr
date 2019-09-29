#' @importFrom R6 R6Class
#' @importFrom cli cat_boxx cat_line cat_rule
#' @importFrom data.table rbindlist set setattr setcolorder
#' @importFrom tools file_path_sans_ext
NULL

#' Create and Run Parametric Analysis, and Collect Results
#'
#' `EplusGroupJob` class is a wrapper of [run_multi()] and provides an interface to
#' group multiple EnergyPlus simulations together for running and collecting
#' outputs.
#'
#' @section Usage:
#' ```
#' group <- group_job(idfs, epws)
#' group$run(dir = NULL, wait = TRUE, force = FALSE, echo = wait)
#' group$kill()
#' group$status()
#' group$output_dir(which = NULL)
#' group$locate_output(which = NULL, suffix = ".err", strict = TRUE)
#' group$errors(which = NULL, info = FALSE)
#' group$list_table(which = NULL)
#' group$read_table(which = NULL, table)
#' group$read_rdd(which = NULL)
#' group$read_mdd(which = NULL)
#' group$report_data_dict(which = NULL)
#' group$report_data(which = NULL, key_value = NULL, name = NULL, year = NULL, tz = "UTC",
#'                   case = "auto", all = FALSE, wide = FALSE, period = NULL, month = NULL,
#'                   day = NULL, hour = NULL, minute = NULL, interval = NULL,
#'                   simulation_days = NULL, day_type = NULL, environment_name = NULL)
#' group$tabular_data(which, report_name = NULL, report_for = NULL, table_name = NULL,
#'                    column_name = NULL, row_name = NULL)
#' group$print()
#' ```
#' @section Create:
#' ```
#' group <- group_job(idfs, epws)
#' ```
#'
#' **Arguments**
#'
#' * `idfs`: Paths to EnergyPlus IDF files or a list of IDF files and [Idf]
#'   objects.
#' * `epws`: Paths to EnergyPlus EPW files or a list of EPW files and [Epw]
#'   objects. Each element in the list can be `NULL`, which will force
#'   design-day-only simulation. Note this needs at least one `Sizing:DesignDay`
#'   object exists in that [Idf]. If `epws` is `NULL`, design-day-only
#'   simulation will be conducted for all input models.
#'
#' @section Run and Collect Results:
#' ```
#' group$run(dir = NULL, wait = TRUE, force = FALSE, echo = wait)
#' group$kill()
#' group$status()
#' group$errors(info = FALSE)
#' group$output_dir(which = NULL)
#' group$locate_output(which = NULL, suffix = ".err", strict = TRUE)
#' group$list_table(which = NULL)
#' group$read_table(which = NULL, table)
#' group$read_rdd(which = NULL)
#' group$read_mdd(which = NULL)
#' group$report_data_dict(which = NULL)
#' group$report_data(which = NULL, key_value = NULL, name = NULL, year = NULL, tz = "UTC",
#'                   case = "auto", all = FALSE, wide = FALSE, period = NULL, month = NULL,
#'                   day = NULL, hour = NULL, minute = NULL, interval = NULL,
#'                   simulation_days = NULL, day_type = NULL, environment_name = NULL)
#' group$tabular_data(which, report_name = NULL, report_for = NULL, table_name = NULL,
#'                    column_name = NULL, row_name = NULL)
#' ```
#'
#' All those functions have the same meaning as in [EplusJob] class, except
#' that they only return the results of specified simulations. Most arguments
#' have the same meanings as in [EplusJob] class, except that there is an
#' argument `which` to select specific simulations.
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
#'   * `successful`: `TRUE` if all simulations ended successfully. `FALSE` if
#'     there is any simulation failed. `NA` if the job has not been run yet.
#'   * `changed_after`: `TRUE` if the *seed model* has been modified since last
#'      simulation. `FALSE` otherwise.
#'   * `job_status`: A [data.table][data.table::data.table()] contains meta data
#'     for each simulation job. For details, please see [run_multi()]. If the
#'     job has not been run before, a [data.table][data.table::data.table()]
#'     with 4 columns is returned:
#'     - `index`: The index of simulation
#'     - `status`: The status of simulation. As the simulation has not been run,
#'       `status` will always be "idle".
#'     - `idf`: The path of input IDF file.
#'     - `epw`: The path of input EPW file. If not provided, `NA` will be
#'       assigned.
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
#' $errors() returns a list of [ErrFile][read_err()] objects which contain all
#' contents of the simulation error file (`.err`). If `info` is `FALSE`, only
#' warnings and errors are printed.
#'
#' `$list_table()` returns a list of character vectors containing all available
#' table and view names in the SQLite file.
#'
#' `$read_table()` takes a valid table name of those from `$list_table()` and
#' returns that table data in a [data.table][data.table::data.table()] format.
#'
#' `$read_rdd()` and `$read_mdd()` return the core data of Report Data
#' Dictionary (RDD) file and Meter Data Dictionary (MDD) file respectively. For
#' details, please see [read_rdd()].
#'
#' `$report_data_dict()` returns a [data.table][data.table::data.table()] which
#' contains all information about report data for specified simulations. For
#' details on the meaning of each columns, please see "2.20.2.1
#' ReportDataDictionary Table" in EnergyPlus "Output Details and Examples"
#' documentation.
#'
#' `$report_data()` extracts the report data in a
#' [data.table][data.table::data.table()] using key values, variable names and
#' other arguments.
#'
#' `$tabular_data()` extracts tabular data in a
#' [data.table][data.table::data.table()].
#'
#' For convenience, input character arguments matching in `$report_data()` and
#' `$tabular_data()` are **case-insensitive**.
#'
#' For `$read_table()`, `$read_rdd()`, `$read_mdd()`, `$report_data_dict()`,
#' `$report_data()` and `$tabular_data()`, the returned data.table has a `case`
#' column in the returned [data.table][data.table::data.table()] that indicates
#' the names of models. For detailed documentation on the results of those
#' methods, please see [EplusJob].
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
#' * `echo`: Only applicable when `wait` is `TRUE`. Whether to simulation
#'   status. Default: `TRUE`.
#' * `suffix`: A string that indicates the file extension of simulation output.
#'   Default: `".err"`.
#' * `table`: A string specifying which table to read. Valid table names can be
#'   obtained using `$list_table()`.
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
#' * `wide`: If `TRUE`, the output is formated in the same way as standard
#'   EnergyPlus csv output file.
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
#'    extract. If `NULL`, all environment data are returned. Default: `NULL`.
#'    All possible `environment_name` for current simulation output can
#'    be obtained using
#'
#'    ```
#'    $read_table("EnvironmentPeriods")
#'    ```
#'
#' * `report_name`, `report_for`, `table_name`, `column_name`, `row_name`:
#'   Each is a character vector for subsetting when querying the SQL database.
#'   For the meaning of each argument, please see the description above.
#'
#' @section Printing:
#' ```
#' group$print()
#' print(group)
#' ```
#'
#' `$print()` shows the core information of this `EplusGroupJob`, including the
#' path of IDFs and EPWs and also the simulation job status.
#'
#' `$print()` is quite useful to get the simulation status, especially when
#' `wait` is `FALSE` in `$run()`. The job status will be updated and printed
#' whenever `$print()` is called.
#'
#' @examples
#' \dontrun{
#' if (is_avail_eplus(8.8)) {
#'     dir <- eplus_config(8.8)$dir
#'     path_idfs <- list.files(file.path(dir, "ExampleFiles"), "\\.idf")[1:5]
#'     path_epws <- list.files(file.path(dir, "WeatherData"), "\\.epw")[1:5]
#'
#'     group <- group_job(path_idfs, path_epws)
#'
#'     # get status of current job
#'     group$status()
#'
#'     # run parametric simulations
#'     group$run(wait = TRUE)
#'
#'     # run in background
#'     group$run(wait = FALSE)
#'     # get detailed job status by printing
#'     print(group)
#'
#'     # status now includes a data.table with detailed information on each simulation
#'     group$status()
#'
#'     # print simulation errors
#'     group$errors()
#'
#'     # extract output of all simulations
#'     group$report_data()
#'
#'     # extract only some simulations
#'     group$report_data(c(1, 3))
#'     group$tabular_data(c(1, 3))
#'     group$report_data(c("rotate_30", "rotate_120"))
#'     group$tabular_data(c("rotate_30", "rotate_120"))
#'
#'     # get output directory
#'     group$output_dir()
#'     group$output_dir(c(1, 3))
#'
#'     # get path of specific output file
#'     group$locate_output(c(1, 3), ".csv")
#' }
#' }
#' @docType class
#' @name EplusGroupJob
#' @author Hongyuan Jia
NULL

#' Create An EnergyPlus Parametric Simulation Job
#'
#' `group_job()` takes IDFs and EPWs as input and returns a `EplusGroupJob`.
#' For details on `EplusGroupJob`, please see [EplusGroupJob] class.
#'
#' @param idfs Paths to EnergyPlus IDF files or a list of IDF files and [Idf]
#' objects.
#' @param epws Paths to EnergyPlus EPW files or a list of EPW files and [Epw]
#' objects. Each element in the list can be `NULL`, which will force
#' design-day-only simulation when [`$run()`][EplusGroupJob] method is called.
#' Note this needs at least one `Sizing:DesignDay` object exists in that [Idf].
#' If `epws` is `NULL`, design-day-only simulation will be conducted for all
#' input models.
#' @return A `EplusGroupJob` object.
#' @examples
#' if (is_avail_eplus(8.8)) {
#'     dir <- eplus_config(8.8)$dir
#'     path_idfs <- list.files(file.path(dir, "ExampleFiles"), "\\.idf")[1:5]
#'     path_epws <- list.files(file.path(dir, "WeatherData"), "\\.epw")[1:5]
#'
#'     # create from local files
#'     group <- group_job(path_idfs, path_epws)
#'
#'     # create from Idfs and Epws object
#'     group_job(lapply(path_idfs, read_idf), lapply(path_epws, read_epw))
#' }
#' @seealso [eplus_job()] for creating an EnergyPlus single simulation job.
#' @export
#' @author Hongyuan Jia
# group_job {{{
group_job <- function (idfs, epws) {
    EplusGroup$new(idfs, epws)
}
# }}}

# EplusGroup {{{
EplusGroup <- R6::R6Class(classname = "EplusGroupJob", cloneable = FALSE,
    public = list(

        # INITIALIZE {{{
        initialize = function (idfs, epws) {
            input <- get_epgroup_input(idfs, epws)
            private$m_idfs <- input$idfs
            private$m_epws <- input$epws

            # add Output:SQLite if necessary
            add_sql <- vlapply(private$m_idfs, idf_add_output_sqlite)
            # add Output:VariableDictionary if necessary
            add_dict <- vlapply(private$m_idfs, idf_add_output_vardict)
            # log if the input idf has been changed
            private$m_log$unsaved <- add_sql | add_dict

            # save uuid
            private$m_log$uuid <- vcapply(private$idfs, function (idf) ._get_private(idf)$m_log$uuid)
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        run = function (dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait)
            epgroup_run(self, private, dir, wait, force, copy_external, echo),

        kill = function ()
            epgroup_kill(self, private),

        status = function ()
            epgroup_status(self, private),

        output_dir = function (which = NULL)
            epgroup_output_dir(self, private, which),

        locate_output = function (which = NULL, suffix = ".err", strict = TRUE)
            epgroup_locate_output(self, private, which, suffix, strict),

        errors = function (which = NULL, info = FALSE)
            epgroup_output_errors(self, private, which, info),

        list_table = function (which = NULL)
            epgroup_list_table(self, private, which),

        read_table = function (which = NULL, table)
            epgroup_read_table(self, private, which, table),

        read_rdd = function (which = NULL)
            epgroup_read_rdd(self, private, which),

        read_mdd = function (which = NULL)
            epgroup_read_mdd(self, private, which),

        report_data_dict = function (which = NULL)
            epgroup_report_data_dict(self, private, which),

        report_data = function (which = NULL, key_value = NULL, name = NULL,
                                year = NULL, tz = "UTC", all = FALSE, wide = FALSE,
                                period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                                interval = NULL, simulation_days = NULL, day_type = NULL,
                                environment_name = NULL)
            epgroup_report_data(self, private, which,
                key_value = key_value, name = name, year = year, tz = tz, all = all, wide = wide,
                period = period, month = month, day = day, hour = hour, minute = minute,
                interval = interval, simulation_days = simulation_days, day_type = day_type,
                environment_name = environment_name
            ),

        tabular_data = function(which = NULL, report_name = NULL, report_for = NULL,
                                table_name = NULL, column_name = NULL, row_name = NULL)
            epgroup_tabular_data(self, private, which, report_name = report_name,
                report_for = report_for, table_name = table_name,
                column_name = column_name, row_name = row_name),

        print = function ()
            epgroup_print(self, private)
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_idfs = NULL,
        m_epws = NULL,
        m_job = NULL,
        m_log = NULL
        # }}}
    )
)
# }}}

# epgroup_run {{{
epgroup_run <- function (self, private, output_dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait) {
    # check if generated models have been modified outside
    uuid <- vcapply(private$m_idfs, function (idf) ._get_private(idf)$m_log$uuid)
    if (any(uuid != private$m_log$uuid)) {
        warn("warning_param_modified", paste0(
            "Some of the grouped models have been modified. ",
            "Running these models will result in simulation outputs that may be not reproducible. ",
            paste0(" # ", seq_along(uuid)[uuid != private$m_log$uuid]," | ",
                names(uuid)[uuid != private$m_log$uuid], collapse = "\n"
            )
        ))
    }

    epgroup_run_models(self, private, output_dir, wait, force, copy_external, echo)
}
# }}}
# epgroup_run_models {{{
epgroup_run_models <- function (self, private, output_dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait) {
    nms <- names(private$m_idfs)
    path_idf <- vcapply(private$m_idfs, function (idf) idf$path())

    if (is.null(private$m_epws)) {
        path_epw <- NULL
        design_day <- TRUE
    } else {
        path_epw <- lapply(private$m_epws, function (epw) if (!is.null(epw)) epw$path())
        if (length(path_epw) == 1L) {
            path_epw <- rep(path_epw, length(path_idf))
        }
        design_day <- vlapply(path_epw, is.null)
    }

    if (is.null(output_dir))
        output_dir <- dirname(path_idf)
    else if (length(output_dir) == 1L) {
        output_dir <- rep(output_dir, length(path_idf))
    } else {
        assert(have_same_len(path_idf, output_dir))
    }
    output_dir <- normalizePath(output_dir, mustWork = FALSE)

    if (any(!dir.exists(unique(output_dir)))) {
        create_dir <- dir.create(unique(output_dir), showWarnings = FALSE, recursive = TRUE)
        abort("error_create_output_dir", paste0("Failed to create output directory: ",
            collapse(unique(output_dir))[!create_dir])
        )
    }

    # check if the model is still running
    old <- private$m_job
    if (!is.null(old)) {
        # update status
        epgroup_retrieve_data(self, private)
        old <- private$m_job

        # check if running in non-waiting mode
        if (inherits(old, "process") && old$is_alive()) {
            pid <- old$get_pid()
            if (force) {
                verbose_info("Force to kill all current running parametric simulations (",
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

    path_group <- normalizePath(file.path(output_dir, nms, paste0(nms, ".idf")), mustWork = FALSE)

    if (any(to_save <- path_group != path_idf | private$m_log$unsaved)) {
        apply2(private$m_idfs[to_save], path_group[to_save],
            function (x, y) x$save(y, overwrite = TRUE, copy_external = copy_external)
        )
    }

    # reset status
    private$m_log$start_time <- Sys.time()
    private$m_log$killed <- NULL
    private$m_log$stdout <- NULL
    private$m_log$stderr <- NULL
    private$m_job <- NULL

    ver <- vcapply(private$m_idfs, function (idf) as.character(idf$version()))
    tbl <- run_multi(path_group, path_epw, NULL, design_day = design_day,
        wait = wait, echo = echo, eplus = ver
    )

    private$m_job <- tbl

    if (wait) private$m_log$end_time <- Sys.time()

    self
}
# }}}
# epgroup_kill {{{
epgroup_kill <- function (self, private) {
    if (is.null(private$m_job)) {
        verbose_info("The parametric job is not running.")
        return(invisible(FALSE))
    }

    if (!inherits(private$m_job, "process")) {
        verbose_info("The parametric job is not running.")
        return(invisible(FALSE))
    }

    proc <- private$m_job

    if (!proc$is_alive()) {
        verbose_info("The parametric job is not running.")
        return(invisible(FALSE))
    }

    k <- tryCatch(proc$kill(), error = function (e) FALSE)

    if (isTRUE(k)) {
        verbose_info("The parametric job has been successfully killed.")
        private$m_log$killed <- TRUE
        return(invisible(TRUE))
    } else {
        verbose_info("Failed to kill parametric job, because it was already finished/dead.")
        return(invisible(FALSE))
    }
}
# }}}
# epgroup_status {{{
epgroup_status <- function (self, private) {
    status <- list(
        run_before = FALSE, # if the model has been run before
        alive = FALSE, # if simulation is still running
        terminated = NA, # if last simulation was terminated
        successful = NA, # if last simulation was successful
        changed_after = NA, # if the seed model has been changed after last simulation
        job_status = data.table() # if no simulation has been run
    )

    proc <- private$m_job

    if (is.null(private$m_job)) {
        if (!is.null(private$m_idfs)) {
            status$job_status <- data.table(
                index = seq_along(private$m_idfs),
                status = "idle",
                idf = vcapply(private$m_idfs, function (idf) idf$path())
            )
            if (is.null(private$m_epws)) {
                epw <- NA_character_
            } else {
                epw <- vcapply(private$m_epws, function (epw) if (is.null(epw)) NA_character_ else epw$path())
            }
            set(status$job_status, NULL, "epw", epw)
        }

        return(status)
    }

    status$run_before <- TRUE

    if (isTRUE(private$m_log$killed)) {
        status$terminated <- TRUE
    } else {
        status$terminated <- FALSE
    }

    status$changed_after <- FALSE
    if (!identical(private$m_log$seed_uuid, ._get_private(private$m_seed)$m_log$uuid)) {
        status$changed_after <- TRUE
    }

    if (inherits(proc, "r_process")) {
        if (proc$is_alive()) {
            status$alive <- TRUE
        } else {
            status$alive <- FALSE
            proc$wait()
            exit_status <- proc$get_exit_status()

            status$job_status <- tryCatch(proc$get_result(), error = function (e) data.table())

            # only if all simulation ran successfully
            if (!is.na(exit_status) && exit_status == 0L &&
                nrow(status$job_status) && all(status$job_status$exit_status == 0L)) {
                status$successful <- TRUE
            } else {
                status$successful <- FALSE
            }
        }

    } else {
        status$alive <- FALSE
        status$successful <- all(proc$exit_status == 0L)
        status$job_status <- proc
    }

    status
}
# }}}
# epgroup_output_dir {{{
epgroup_output_dir <- function (self, private, which = NULL) {
    epgroup_job_from_which(self, private, which, keep_unsucess = TRUE)$output_dir
}
# }}}
# epgroup_locate_output {{{
epgroup_locate_output <- function (self, private, which = NULL, suffix = ".err", strict = TRUE, keep_unsucess = FALSE) {
    job <- epgroup_job_from_which(self, private, which, keep_unsucess = keep_unsucess)

    out <- paste0(tools::file_path_sans_ext(job$idf), suffix)

    if (strict && any(!file.exists(out))) {
        msg <- job[!file.exists(out), sim_status("MISSING", index, idf, epw)]
        stop("Path does not exist for job:\n", paste0(msg, collapse = "\n"), call. = FALSE)
    }

    out
}
# }}}
# epgroup_output_errors {{{
epgroup_output_errors <- function (self, private, which, info = FALSE) {
    # continue to parse err file for jobs having non-zero exits (#24)
    path_err <- epgroup_locate_output(self, private, which, ".err", keep_unsucess = TRUE)

    err <- lapply(path_err, parse_err_file)

    names(err) <- epgroup_case_from_which(self, private, which, name = TRUE)

    if (!info) err <- lapply(err, function (x) x[!(level == "Info")])

    err
}
# }}}
# epgroup_list_table {{{
epgroup_list_table <- function (self, private, which = NULL) {
    cases <- epgroup_case_from_which(self, private, which, name = TRUE)
    lists <- lapply(epgroup_sql_path(self, private, which), list_sql_table)
    setattr(lists, "names", cases)[]
}
# }}}
# epgroup_read_table {{{
epgroup_read_table <- function (self, private, which = NULL, table) {
    tables <- lapply(epgroup_sql_path(self, private, which), read_sql_table, table)
    epgroup_combine_data(self, private, which, tables)[]
}
# }}}
# epgroup_read_rdd {{{
epgroup_read_rdd <- function (self, private, which = NULL) {
    rdds <- lapply(epgroup_rdd_path(self, private, which, "rdd"), read_rdd)
    epgroup_combine_data(self, private, which, rdds)[]
}
# }}}
# epgroup_read_mdd {{{
epgroup_read_mdd <- function (self, private, which = NULL) {
    mdds <- lapply(epgroup_rdd_path(self, private, which, "mdd"), read_mdd)
    epgroup_combine_data(self, private, which, mdds)[]
}
# }}}
# epgroup_report_data_dict {{{
epgroup_report_data_dict <- function (self, private, which) {
    dicts <- lapply(epgroup_sql_path(self, private, which), get_sql_report_data_dict)
    epgroup_combine_data(self, private, which, dicts)[]
}
# }}}
# epgroup_report_data {{{
epgroup_report_data <- function (self, private, which = NULL, key_value = NULL,
                               name = NULL, year = NULL, tz = "GMT", all = FALSE, wide = FALSE,
                               period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                               interval = NULL, simulation_days = NULL, day_type = NULL,
                               environment_name = NULL) {
    sqls <- epgroup_sql_path(self, private, which)
    cases <- epgroup_case_from_which(self, private, which, name = TRUE)

    rbindlist(mapply(get_sql_report_data, sql = sqls, case = cases,
        MoreArgs = list(key_value = key_value, name = name, all = all, wide = wide, year = year,
            tz = tz, period = period, month = month, day = day, hour = hour, minute = minute,
            interval = interval, simulation_days = simulation_days, day_type = day_type,
            environment_name = environment_name),
        SIMPLIFY = FALSE, USE.NAMES = FALSE
    ), fill = TRUE)
}
# }}}
# epgroup_tabular_data {{{
epgroup_tabular_data <- function (self, private, which = NULL, report_name = NULL, report_for = NULL,
                                table_name = NULL, column_name = NULL, row_name = NULL) {
    d <- lapply(epgroup_sql_path(self, private, which), get_sql_tabular_data,
        report_name = report_name, report_for = report_for,
        table_name = table_name, column_name = column_name, row_name = row_name
    )
    epgroup_combine_data(self, private, which, d)[]
}
# }}}
# epgroup_print {{{
epgroup_print <- function (self, private) {
    cli::cat_rule("EnergPlus Group Simulation Job", col = "green")
    cli::cat_line(paste0("Grouped Jobs [", length(private$m_idfs), "]: "))

    epgroup_print_status(self, private)
}
# }}}

# helper
# get_epgroup_input {{{
get_epgroup_input <- function (idfs, epws) {
    # check idf {{{
    if (is_idf(idfs)) {
        idfs <- list(get_init_idf(idfs))
    } else {
        idfs <- tryCatch(lapply(idfs, get_init_idf),
            error_idf_not_local = function (e) e,
            error_idf_path_not_exist = function (e) e,
            error_idf_not_saved = function (e) e
        )
    }

    if (any(!vlapply(idfs, is_idf))) {
        for (err in c("error_idf_not_local", "error_idf_path_not_exist", "error_idf_not_saved")) {
            if (any(invld <- vlapply(idfs, inherits, err))) {
                abort(err, paste0(conditionMessage(idfs[[which(invld)[[1L]]]]),
                    " Invalid input index: ", collapse(which(invld))
                ))
            }
        }
    }

    nm_idf <- tools::file_path_sans_ext(basename(vcapply(idfs, function (idf) idf$path())))
    setattr(idfs, "names", make.unique(nm_idf, "_"))
    # }}}

    # check epw {{{
    get_epw <- function (epw) {
        if (is.null(epw)) return(NULL)
        get_init_epw(epw)
    }

    if (is_epw(epws)) {
        epws <- list(get_init_epw(epws))
    } else {
        epws <- tryCatch(lapply(epws, get_epw),
            error_epw_not_local = function (e) e,
            error_epw_path_not_exist = function (e) e,
            error_epw_not_saved = function (e) e
        )
    }

    if (any(!vlapply(epws, is_epw))) {
        for (err in c("error_epw_not_local", "error_epw_path_not_exist", "error_epw_not_saved")) {
            if (any(invld <- vlapply(epws, inherits, err))) {
                abort(err, paste0(conditionMessage(epws[[which(invld)[[1L]]]]),
                    " Invalid input index: ", collapse(which(invld))
                ))
            }
        }
    }

    if (!length(epws)) epws <- NULL
    # }}}

    # check length
    if (!is.null(epws)) {
        assert(have_same_len(idfs, epws))
        nm_epw <- tools::file_path_sans_ext(basename(vcapply(epws, function (epw) epw$path())))
        setattr(epws, "names", make.unique(nm_epw, "_"))
    }

    list(idfs = idfs, epws = epws)
}
# }}}
# epgroup_retrieve_data {{{
epgroup_retrieve_data <- function (self, private) {
    status <- epgroup_status(self, private)

    if (!status$run_before) return(invisible())

    if (status$alive) {
        private$m_log$stdout <- c(private$m_log$stdout, private$m_job$read_output_lines(10000))
        private$m_log$stderr <- c(private$m_log$stderr, private$m_job$read_error_lines(10000))
    } else {
        if (inherits(private$m_job, "r_process") & !status$terminated) {
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
# epgroup_job_from_which {{{
epgroup_job_from_which <- function (self, private, which, keep_unsucess = FALSE) {
    status <- epgroup_status(self, private)

    if (!isTRUE(status$run_before))
        stop("Parametric job did not run before. Please run it using `$run()` ",
            "before collect output", call. = FALSE)

    if (isTRUE(status$terminated))
        stop("Parametric job was terminated before. Please solve ",
            "the problems and re-run it before collect output.", call. = FALSE)

    if (isTRUE(status$alive))
        stop("Parametric job is still running. Please wait it ",
            "to finish before collecting results.", call. = FALSE)

    if (isTRUE(status$changed_after))
        warning("The seed model has been changed since last run. ",
            "The job output may not be correct.", call. = FALSE)

    # if success, retrieve data
    epgroup_retrieve_data(self, private)

    jobs <- private$m_job

    idx <- epgroup_case_from_which(self, private, which, name = FALSE)

    job <- jobs[idx]

    # setting `keep_unsucess` to TRUE makes it possible to continue to parse
    # some output files such like .err files. (#24)
    if (not_empty(job[status != "completed"])) {
        incomplete <- job[status != "completed"]
        msg <- incomplete[, sim_status(rpad(toupper(status)), index, idf, epw)]
        if (keep_unsucess) {
            warn("error_job_error", paste0("Some of jobs failed to complete. ",
                "Simulation results may not be correct:\n",
                paste0(msg, collapse = "\n")
            ))
        } else {
            abort("error_job_error", paste0("Some of jobs failed to complete. ",
                "Please fix the problems and re-run it before collecting output:\n",
                paste0(msg, collapse = "\n")
            ))
        }
    }

    job
}
# }}}
# epgroup_case_from_which {{{
epgroup_case_from_which <- function (self, private, which = NULL, name = FALSE) {
    nms <- names(private$m_idfs)
    if (is.null(which)) {
        if (name) return(nms) else return(seq_along(nms))
    }

    if (is.character(which)) {
        valid <- chmatch(stri_trans_tolower(which), stri_trans_tolower(nms))
        if (anyNA(valid))
            stop("Invalid job name found: ",
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
# epgroup_sql_path {{{
epgroup_sql_path <- function (self, private, which) {
    epgroup_locate_output(self, private, which, ".sql")
}
# }}}
# epgroup_rdd_path {{{
epgroup_rdd_path <- function (self, private, which, type = c("rdd", "mdd")) {
    type <- match.arg(type)
    epgroup_locate_output(self, private, which, paste0(".", type))
}
# }}}
# epgroup_combine_data {{{
epgroup_combine_data <- function (self, private, which, data, fill = TRUE) {
    cases <- epgroup_case_from_which(self, private, which, name = TRUE)

    # add case
    for (idx in seq_along(cases)) {
        set(data[[idx]], j = "case", value = cases[idx])
        setcolorder(data[[idx]], c("case", setdiff(names(data[[idx]]), "case")))
    }

    rbindlist(data, fill = fill)
}
# }}}
# epgroup_print_status {{{
epgroup_print_status <- function (self, private, epw = TRUE) {
    epgroup_retrieve_data(self, private)
    status <- epgroup_status(self, private)

    if (!epw) {
        nm <- str_trunc(paste0(
            "[", lpad(seq_along(private$m_idfs), 0), "]: ",
            surround(names(private$m_idfs))
        ))
    } else {
        nm_idf <- str_trunc(paste0(
            "[", lpad(seq_along(private$m_idfs), 0), "]: ",
            paste0("[IDF] ", surround(names(private$m_idfs)))
        ))

        if (is.null(private$m_epws)) {
            nm_epw <- "[EPW] << Not specified >>"
        } else {
            nm_epw <- paste0("[EPW] ", surround(names(private$m_epws)))
        }

        nm <- paste0(rpad(nm_idf), " + ", nm_epw)
    }

    if (!status$run_before) {
        cli::cat_line(paste0(str_trunc(nm), collapse = "\n"))
        cli::cat_line("<< Job has not been run before >>",
            col = "white", background_col = "blue")
        return(invisible())
    }

    # each job status {{{
    if (status$alive) {
        if (length(private$m_log$stderr)) {
            stderr <- private$m_log$stderr
            # keep the latest status
            job_status <- as.data.table(stri_split_fixed(stderr, "|", n = 2L, simplify = TRUE))
            job_status <- unique(job_status, fromLast = TRUE, by = "V1")
            # get index
            set(job_status, NULL, "index", as.integer(job_status$V1))
            # order by index
            setorder(job_status, "index")
            # make sure all models are included
            job_status <- job_status[J(seq_along(nm)), on = "index"]
            # for models that are idle
            job_status[J(NA_character_), on = "V2", V2 := paste0(
                "IDLE       --> [IDF]", surround(names(private$m_idfs)[index]))]
            stderr <- paste0(lpad(job_status$index, "0"), "|" ,job_status$V2)
            safe_width <- getOption("width") - 2L
            stderr_trunc <- vcapply(stderr, function (l) {
                if (nchar(l) > safe_width) {
                    paste0(substr(l, 1, safe_width), "...")
                } else {
                    l
                }
            })

            cli::cat_boxx(stderr_trunc, col = "green", border_col = "green",
                padding = 0)
        }
    } else {
        if (isTRUE(status$terminated)) {
            cli::cat_line(paste0(str_trunc(rpad(nm), width = getOption("width", 60L) - 15L),
                " <-- TERMINATED", collapse = "\n"))
        } else {
            nm <- private$m_job[, paste0(
                ifelse(exit_status == 0L,
                    paste0(str_trunc(rpad(nm), getOption("width", 60L) - 14L), " <-- SUCCEEDED"),
                    paste0(str_trunc(rpad(nm), getOption("width", 60L) - 11L), " <-- FAILED")
                )
            )]
            cli::cat_line(paste0(nm, collapse = "\n"))
        }
    }
    # }}}

    # print summary status {{{
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
    # }}}
}
# }}}

# S3 EplusGroupJob methods {{{
#' @export
str.EplusGroupJob <- function (object, ...) {
    object$print()
}

#' @export
format.EplusGroupJob <- function (x, ...) {
    paste0(utils::capture.output(x$print()), collapse = "\n")
}
# }}}
