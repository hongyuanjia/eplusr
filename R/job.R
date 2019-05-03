#' @importFrom R6 R6Class
#' @importFrom cli cat_bullet cat_line cat_rule
#' @importFrom tools file_path_sans_ext
NULL

#' Run EnergyPlus Simulation and Collect Outputs
#'
#' `EplusJob` class wraps the EnergyPlus command line interface and provides
#' methods to extract simulation outputs.
#'
#' eplusr uses the EnergyPlus SQL output for extracting simulation outputs.
#' `EplusJob` has provide some wrappers that do SQL query to get report data
#' results, i.e. results from `Output:Variable` and `Output:Meter*`. But for
#' `Output:Table` results, you have to be familiar with the structure of the
#' EnergyPlus SQL results, especially for table *"TabularDataWithStrings"*. For
#' details, please see *"2.20 eplusout.sql"*, especially *"2.20.4.4 TabularData
#' Table"* in EnergyPlus *"Output Details and Examples"* documentation.
#'
#' @section NOTE:
#'
#' When using `$run()` in [Idf] class, which internally creates an `EplusJob`
#' object and calls its `$run()` method, an object in `Output:SQLite` with
#' `Option Type` value of `SimpleAndTabular` will be automatically created if it
#' does not exists.
#'
#' However, when creating an `EplusJob` using [eplus_job()], the IDF file is not
#' parsed but directly pass its path to EnergyPlus. Thus, that process of
#' handling `Output:SQLite` class is not performed. If you want to ensure that
#' the output collection functionality in `EplusJob` class works successfully,
#' it is recommended to first read that IDF file using [read_idf()] and then use
#' `$run()` method in [Idf] class by doing `idf$run()`.
#'
#' @section Usage:
#' ```
#' job <- eplus_job(idf, epw)
#' job$path(type = c("all", "idf", "epw"))
#' job$run(wait = TRUE, force = FALSE)
#' job$kill()
#' job$status()
#' job$errors(info = FALSE)
#' job$output_dir(open = FALSE)
#' job$locate_output(suffix = ".err", strict = TRUE)
#' job$list_table()
#' job$read_table(name)
#' job$report_data_dict()
#' job$report_data(key_value = NULL, name = NULL, year = NULL, tz = "UTC", case = "auto", all = FALSE,
#'                 period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
#'                 interval = NULL, simulation_days = NULL, day_type = NULL, environment_name = NULL)
#' job$tabular_data(report_name = NULL, report_for = NULL, table_name = NULL, column_name = NULL, row_name = NULL)
#' job$print()
#' ```
#'
#' @section Basic info:
#' ```
#' job <- eplus_job(idf, epw)
#' job$path(type = c("all", "idf", "epw"))
#' ```
#'
#' `$path()` returns the path of IDF or EPW of current job.
#'
#' **Arguments**
#'
#' * `idf`: Path to an local EnergyPlus IDF file or an [Idf] object.
#' * `epw`: Path to an local EnergyPlus EPW file or an [Epw] object.
#' * `type`: If `"all"`, both the [Idf] path and [Epw] path are returned. If
#'   `"idf"`, only IDF path is returned. If `"epw"`, only EPW path is returned.
#'   Default: `"all"`.
#'
#' @section Run:
#' ```
#' job$run(wait = TRUE, force = FALSE)
#' job$kill()
#' job$status()
#' job$errors(info = FALSE)
#' ```
#'
#' `$run()` runs the simulation using input IDF and EPW file. If `wait`
#' is `FALSE`, the job is run in the background. You can get updated job
#' status by just printing the `EplusJob` object.
#'
#' `$kill()` kills the background EnergyPlus process if possible. It only works
#' when simulation runs in non-waiting mode.
#'
#' `$status()` returns a named list of values that indicates the status of the
#' job:
#'
#' * `run_before`: `TRUE` if the job has been run before. `FALSE` otherwise.
#' * `alive`: `TRUE` if the simulation is still running in the background.
#'   `FALSE` otherwise.
#' * `terminated`: `TRUE` if the simulation was terminated during last
#'    simulation. `FALSE` otherwise. `NA` if the job has not been run yet.
#' * `successful`: `TRUE` if last simulation ended successfully. `FALSE`
#'   otherwise. `NA` if the job has not been run yet.
#' * `changed_after`: `TRUE` if the IDF file has been changed since last
#'    simulation. `FALSE` otherwise. `NA` if the job has not been run yet.
#'
#' $errors() returns an [ErrFile][read_err()] object which contains all contents
#' of the simulation error file (`.err`). If `info` is `FALSE`, only warnings
#' and errors are printed.
#'
#' **Arguments**
#'
#' * `wait`: If `TRUE`, R will hang on and wait for the simulation to complete.
#'   EnergyPlus standard output (stdout) and error (stderr) is printed to
#'   R console. If `FALSE`, simulation will be run in a background process.
#'   Default: `TRUE`.
#' * `force`: Only applicable when the last job runs with `wait` equals
#'   to `FALSE` and is still running. If `TRUE`, current running job is
#'   forced to stop and a new one will start. Default: `FALSE`.
#' * `info`: If `FALSE`,only warnings and errors are printed. Default: `FALSE`.
#'
#' @section Simulation Output Extraction:
#' ```
#' job$output_dir(open = FALSE)
#' job$locate_output(suffix = ".err", strict = TRUE)
#' job$list_table()
#' job$read_table(table)
#' job$report_data_dict()
#' job$report_data(key_value = NULL, name = NULL, year = NULL, tz = "UTC",case = "auto", all = FALSE,
#'                 period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
#'                 interval = NULL, simulation_days = NULL, day_type = NULL, environment_name = NULL)
#' job$tabular_data(report_name = NULL, report_for = NULL, table_name = NULL, column_name = NULL, row_name = NULL)
#' ```
#'
#' `$output_dir()` returns the output directory of simulation results.
#'
#' `$locate_output()` returns the path of a single output file specified by file
#' suffix.
#'
#' `$list_table()` returns all available table and view names in the SQLite file.
#'
#' `$read_table()` takes a valid `table` name of those from `$list_table()` and
#' returns that table data in a [data.table][data.table::data.table()] format.
#'
#' `$report_data_dict()` returns a [data.table][data.table::data.table()] which
#' contains all information about report data. For details on the meaning of
#' each columns, please see "2.20.2.1 ReportDataDictionary Table" in EnergyPlus
#' "Output Details and Examples" documentation.
#'
#' `$report_data()` extracts the report data in a
#' [data.table][data.table::data.table()] using key values, variable names and
#' other specifications. `$report_data()` can also directly take all or subset
#' output from `$report_data_dict()` as input, and extract all data specified.
#' The returned column numbers varies depending on `all` argument.
#'
#' * `all` is `FALSE`, the returned [data.table][data.table::data.table()] has 6
#'   columns:
#'   * `case`: Simulation case specified using `case` argument
#'   * `datetime`: The date time of simulation result
#'   * `key_value`: Key name of the data
#'   * `name`: Actual report data name
#'   * `units`: The data units
#'   * `value`: The data value
#' * `all` is `TRUE`, besides columns described above, extra columns are also
#'   included:
#'   * `month`: The month of reported date time
#'   * `day`: The day of month of reported date time
#'   * `hour`: The hour of reported date time
#'   * `minute`: The minute of reported date time
#'   * `dst`: Daylight saving time indicator. Possible values: `0` and `1`
#'   * `interval`: Length of reporting interval
#'   * `simulation_days`: Day of simulation
#'   * `day_type`: The type of day, e.g. `Monday`, `Tuesday` and etc.
#'   * `environment_name`: A text string identifying the environment
#'   * `is_meter`: Whether report data is a meter data. Possible values: `0` and
#'     `1`
#'   * `type`: Nature of data type with respect to state. Possible values: `Sum`
#'     and `Avg`
#'   * `index_group`: The report group, e.g. `Zone`, `System`
#'   * `timestep_type`: Type of data timestep. Possible values: `Zone` and `HVAC
#'     System`
#'   * `reporting_frequency`: The reporting frequency of the variable, e.g.
#'   `HVAC System Timestep`, `Zone Timestep`.
#'   * `schedule_name`: Name of the the schedule that controls reporting
#'     frequency.
#'
#' With the `datetime` column, it is quite straightforward to apply time-series
#' analysis on the simulation output. However, another painful thing is that
#' every simulation run period has its own `Day of Week for Start Day`. Randomly
#' setting the `year` may result in a date time series that does not have
#' the same start day of week as specified in the RunPeriod objects.
#'
#' eplusr provides a simple solution for this. By setting `year` to `NULL`,
#' which is the default behavior, eplusr will calculate a year value (from
#' current year backwards) for each run period that compliance with the start
#' day of week restriction.
#'
#' `$tabular_data()` extracts the tabular data in a
#' [data.table][data.table::data.table()] using report, table, column and row
#' name specifications. The returned [data.table][data.table::data.table()] has
#' 8 columns:
#'
#' * `index`: Tabular data index
#' * `report_name`: The name of the report that the record belongs to
#' * `report_for`: The `For` text that is associated with the record
#' * `table_name`: The name of the table that the record belongs to
#' * `column_name`: The name of the column that the record belongs to
#' * `row_name`: The name of the row that the record belongs to
#' * `units`: The units of the record
#' * `value`: The value of the record **in string format**
#'
#' For convenience, input character arguments matching in `$report_data()` and
#' `$tabular_data()` are **case-insensitive**.
#'
#' **Arguments**
#'
#' * `open`: If `TRUE`, the output directory will be opened.
#' * `suffix`: A string that indicates the file extension of simulation output.
#'   Default: `".err"`.
#' * `strict`: If `TRUE`, it will check if the simulation was terminated, is
#'   still running or the file exists or not. Default: `TRUE`.
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
#' job$print()
#' print(job)
#' ```
#'
#' `$print()` shows the core information of this `EplusJob` object, including
#' the path of model and weather, the version and path of EnergyPlus used to run
#' simulations, and the simulation job status.
#'
#' `$print()` is quite useful to get the simulation status, especially when
#' `wait` is `FALSE` in `$run()`. The job status will be updated and printed
#' whenever `$print()` is called.
#'
#' @examples
#' \dontrun{
#' if (is_avail_eplus(8.8)) {
#'     idf_name <- "1ZoneUncontrolled.idf"
#'     epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'
#'     idf_path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", idf_name)
#'     epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData", epw_name)
#'
#'     # copy to tempdir
#'     file.copy(c(idf_path, epw_path), tempdir())
#'
#'     # create an EplusJob from local an IDF and an EPW file
#'     job <- eplus_job(file.path(tempdir(), idf_name), file.path(tempdir(), epw_name))
#'
#'     # get paths of idf and epw
#'     job$path("all")
#'     job$path("idf")
#'     job$path("epw")
#'
#'     # get current job status
#'     job$status()
#'
#'     # check if the job has been run before
#'     job$status()$run_before
#'
#'     # run the job in waiting mode
#'     job$run(wait = TRUE)
#'
#'     # check the job status again
#'     job$status()$run_before
#'     job$status()$successful
#'
#'     # get output directory
#'     job$output_dir()
#'
#'     # open the output directory
#'     job$output_dir(open = TRUE)
#'
#'     # check simulation errors
#'     job$errors()
#'
#'     # check simulation errors, only including warnings and errors
#'     job$errors(info = FALSE)
#'
#'     # get the file path of an output with a given suffix
#'     job$locate_output(".err")
#'
#'     # give an error when simulation did not complete successfully or that file
#'     # does not exist
#'     job$locate_output(".exe", strict = TRUE)
#'
#'     # retrieve simulation results will fail if there is no EnergyPlus SQL output.
#'     job$report_data_dict()
#'
#'     # instead, using `$run()` method in Idf class, which will add an
#'     # `Output:SQLite` object automatically
#'     idf <- read_idf(file.path(tempdir(), idf_name))
#'     job <- idf$run(file.path(tempdir(), epw_name), dir = NULL)
#'
#'     # get report data dictionary
#'     str(job$report_data_dict())
#'
#'     # extract all report data
#'     str(job$report_data())
#'
#'     # extract some report variable
#'     str(job$report_data(name = "EnergyTransfer:Building", case = NULL))
#'
#'     # add a "case" column in the returned data.table
#'     str(job$report_data(name = "EnergyTransfer:Building", case = "Test"))
#'
#'     # change the format of datetime column in the returned data.table
#'     str(job$report_data(name = "EnergyTransfer:Building", year = 2016L, tz = Sys.timezone()))
#'
#'     # get all tabular data
#'     str(job$tabular_data())
#' }
#' }
#' @docType class
#' @name EplusJob
#' @seealso [ParametricJob] class for EnergyPlus parametric simulations.
#' @author Hongyuan Jia
NULL

#' Create an EnergyPlus Simulation Job
#'
#' `eplus_job()` takes an IDF and EPW as input, and returns an `EplusJob` object
#' for running EnergyPlus simulation and collecting outputs. For more details,
#' please see [EplusJob].
#'
#' @param idf A path to an local EnergyPlus IDF file or an `Idf` object.
#' @param epw A path to an local EnergyPlus EPW file or an `Epw` object.
#' @return An `EplusJob` object.
#' @examples
#' if (is_avail_eplus(8.8)) {
#'     idf_name <- "1ZoneUncontrolled.idf"
#'     epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'
#'     idf_path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", idf_name)
#'     epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData", epw_name)
#'
#'     # create from local files
#'     eplus_job(idf_path, epw_path)
#'
#'     # create from an Idf and an Epw object
#'     eplus_job(read_idf(idf_path), read_epw(epw_path))
#' }
#' @seealso [param_job()] for creating an EnergyPlus parametric job.
#' @author Hongyuan Jia
#' @export
# eplus_job {{{
eplus_job <- function (idf, epw) {
    EplusJob$new(idf, epw)
}
# }}}

# EplusJob {{{
EplusJob <- R6::R6Class(classname = "EplusJob", cloneable = FALSE,
    public = list(

        # INITIALIZE {{{
        initialize = function (idf, epw, eplus_ver = NULL) {

            if (is_idf(idf)) {
                private$m_path_idf <- idf$path()
                if (is.null(private$m_path_idf)) {
                    abort("error_idf_not_local",
                        paste0(
                            "The Idf object is not created from local file. ",
                             "Please give save it to disk before run."
                        )
                    )
                }

                if (idf$is_unsaved()) {
                    abort("error_idf_not_saved",
                        paste0("Idf has been modified since read or last saved. ",
                            "Please save Idf using $save() before run."
                        )
                    )
                }
            } else {
                assert(is_string(idf))
                private$m_path_idf <- idf
            }

            if (!file.exists(private$m_path_idf)) {
                abort("error_idf_not_exist", "Input idf does not exists.")
            }

            private$m_path_idf <- normalizePath(private$m_path_idf, mustWork = TRUE)

            if (is_epw(epw)) {
                private$m_path_epw <- epw$path()
                if (is.null(private$m_path_epw)) {
                    abort("error_epw_not_local",
                        paste0(
                            "The Epw object is not created from local file. ",
                            "Please give save it to disk before run."
                        )
                    )
                }

                if (epw$is_unsaved()) {
                    abort("error_epw_not_saved",
                        paste0("Epw has been modified since read or last saved. ",
                            "Please save Epw using $save() before run."
                        )
                    )
                }
            } else {
                assert(is_string(epw))
                private$m_path_epw <- epw
            }

            if (!file.exists(private$m_path_epw)) {
                abort("error_epw_not_exist", "Input epw file does not exists.")
            }

            private$m_path_epw <- normalizePath(private$m_path_epw, mustWork = TRUE)

            # get Idf version
            if (!is.null(eplus_ver)) {
                assert(is_eplus_ver(eplus_ver, strict = TRUE))
            } else {
                eplus_ver <- get_idf_ver(read_lines(private$m_path_idf))
                if (is.null(eplus_ver)) {
                    abort("error_idf_no_version", "Could not find version of input IDF file.")
                }
            }

            if (eplus_ver < 8.3) {
                abort("error_eplus_lower_8.3",
                    "Currently, `EplusJob` only supports EnergyPlus V8.3.0 or higher."
                )
            }

            if (!is_avail_eplus(eplus_ver)) {
                abort("error_eplus_not_avail",
                    paste0(
                        "Could not locate EnergyPlus v", eplus_ver, ". Please set ",
                        "the path of EnergyPlus v", eplus_ver, "using `use_eplus()`."
                    )
                )
            }

            private$m_version <- complete_patch_ver(eplus_ver)
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        path = function (type = c("all", "idf", "epw"))
            job_path(self, private, type),

        run = function (wait = TRUE, force = FALSE)
            job_run(self, private, wait = wait, force),

        kill = function ()
            job_kill(self, private),

        status = function ()
            job_status(self, private, based_suffix = ".err"),

        output_dir = function (open = FALSE)
            job_output_dir(self, private, open),

        locate_output = function (suffix = ".err", strict = TRUE)
            job_locate_output(self, private, suffix, strict, must_exist = strict),

        errors = function (info = FALSE)
            job_output_errors(self, private, info),

        list_table = function ()
            job_list_table(self, private),

        read_table = function (name)
            job_read_table(self, private, name),

        report_data_dict = function ()
            job_report_data_dict(self, private),

        report_data = function (key_value = NULL, name = NULL, year = NULL,
                                tz = "UTC", case = "auto", all = FALSE,
                                period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                                interval = NULL, simulation_days = NULL, day_type = NULL,
                                environment_name = NULL)
            job_report_data(self, private, key_value = key_value, name = name, year = year,
                tz = tz, case = case, all = all,
                period = period, month = month, day = day, hour = hour, minute = minute,
                interval = interval, simulation_days = simulation_days, day_type = day_type,
                environment_name = environment_name
            ),

        tabular_data = function(report_name = NULL, report_for = NULL, table_name = NULL,
                                column_name = NULL, row_name = NULL)
            job_tabular_data(self, private, report_name = report_name,
                report_for = report_for, table_name = table_name,
                column_name = column_name, row_name = row_name),

        print = function ()
            job_print(self, private)
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_version = NULL,
        m_path_idf = NULL,
        m_path_epw = NULL,
        m_eplus_config = NULL,
        m_job = NULL,
        m_log = NULL
        # }}}
    )
)
# }}}

# job_path {{{
job_path <- function (self, private, type = c("all", "idf", "epw")) {
    type <- match.arg(type)

    switch(type,
        all = c(idf = private$m_path_idf, epw = private$m_path_epw),
        idf = private$m_path_idf, epw = private$m_path_epw
    )
}
# }}}
# job_run {{{
job_run <- function (self, private, wait = TRUE, force = FALSE) {
    # check if the model is still running
    old <- private$m_job
    if (!is.null(old)) {
        proc <- old$process
        if (inherits(proc, "process") && proc$is_alive()) {
            pid <- proc$get_pid()
            if (force) {
                message("Force to kill current running simulation (PID: ", pid,
                    ") and start a new simulation...")
                suppressMessages(self$kill())
            } else {
                stop("The simulation of current Idf is still running (PID: ",
                    pid, "). Please set `force` to TRUE if you want ",
                    "to kill the running process and start a new simulation.",
                    call. = FALSE)
            }
        }
    }

    private$m_log$start_time <- Sys.time()
    private$m_log$killed <- NULL

    private$m_job <- run_idf(private$m_path_idf, private$m_path_epw,
        output_dir = NULL, echo = wait, wait = wait, eplus = private$m_version)

    if (wait) private$m_log$end_time <- Sys.time()
    self
}
# }}}
# job_kill {{{
job_kill <- function (self, private) {
    if (is.null(private$m_job)) {
        message("The job has not been run yet.")
        return(invisible(FALSE))
    }

    proc <- private$m_job$process

    if (!proc$is_alive()) {
        message("The job is not running.")
        return(invisible(FALSE))
    }

    k <- tryCatch(proc$kill(), error = function (e) FALSE)

    if (isTRUE(k)) {
        private$m_log$killed <- TRUE
        message("The job has been successfully killed.")
        return(invisible(TRUE))
    } else {
        message("Failed to kill the job, because it was already finished/dead.")
        return(invisible(FALSE))
    }
}
# }}}
# job_status {{{
job_status <- function (self, private, based_suffix = ".err") {
    # init
    status <- list(
        run_before = FALSE, # if the model has been run before
        alive = FALSE, # if simulation is still running
        terminated = NA, # if last simulation was terminated
        successful = NA, # if last simulation was successful
        changed_after = NA # if the model has been changed after last simulation
    )

    proc <- private$m_job

    # if the model has not been run before
    if (is.null(proc)) {
        if (!file.exists(private$m_path_idf)) {
            warning("Could not find local idf file ", surround(private$m_path_idf),
                ".", call. = FALSE)
        }
        return(status)
    }

    status$run_before <- TRUE

    if (isTRUE(private$m_log$killed)) {
        status$terminated <- TRUE
    } else {
        status$terminated <- FALSE
    }

    # check if the model is still running
    if (proc$process$is_alive()) {
        status$alive <- TRUE
    } else {
        status$alive <- FALSE
        proc$process$wait()
        exit_status <- proc$process$get_exit_status()
        if (!is.na(exit_status) && exit_status == 0L) {
            status$successful <- TRUE
        } else {
            status$successful <- FALSE
        }
    }

    status$changed_after <- FALSE
    prefix <- tools::file_path_sans_ext(private$m_path_idf)
    basefile <- paste0(prefix, based_suffix)

    if (!file.exists(basefile)) return(status)

    base_ctime <- file.info(basefile)$mtime

    if (!file.exists(private$m_path_idf)) return(status)

    idf_ctime <- file.info(private$m_path_idf)$mtime

    if (base_ctime < idf_ctime) status$changed_after <- TRUE

    status
}
# }}}
# job_output_dir {{{
job_output_dir <- function (self, private, open = FALSE) {
    dir <- dirname(private$m_path_idf)
    if (!open) return(dir)
    if (open) {
        if (is.null(dir)) {
            message("No simulation has been run yet.")
            return(invisible())
        }

        # Reference:
        # http://r.789695.n4.nabble.com/R-command-to-open-a-file-quot-browser-quot-on-Windows-and-Mac-td4710688.html
        if (is_windows()) {
            shell.exec(dir)
        } else if (is_macos()) {
            system2("open", dir)
        } else if (is_linux()) {
            system(paste0("xdg-open ", dir))
        } else {
            message("Current platform not supported.")
        }
    }
    dir
}
# }}}
# job_locate_output {{{
job_locate_output <- function (self, private, suffix = ".err", strict = TRUE, must_exist = TRUE) {
    out <- paste0(tools::file_path_sans_ext(private$m_path_idf), suffix)

    if (strict) {
        status <- job_status(self, private, suffix)

        if (!isTRUE(status$run_before)) {
            stop("Simulation did not run before. Please run it using `$run()` ",
                "before collect output", call. = FALSE)
        }

        if (isTRUE(status$terminated))
            stop("Simulation was terminated before. Please solve ",
                "the problems and re-run the simulation before collect ",
                "output", call. = FALSE)

        if (isTRUE(status$alive))
            stop("Simulation is still running. Please wait simulation ",
                "to finish before collecting results.", call. = FALSE)

        if (isTRUE(status$changed_after))
            warning("The Idf has been changed since last simulation. ",
                "The simulation output may not be correct.", call. = FALSE)

        if (isTRUE(status$run_before) && !isTRUE(status$successful))
            warning("Simulation ended with errors. Simulation results ",
                "may not be correct.", call. = FALSE)

    }

    if (must_exist) assert(file.exists(out))

    out
}
# }}}
# job_output_errors {{{
job_output_errors <- function (self, private, info = FALSE) {
    path_err <- job_locate_output(self, private, ".err")

    err <- parse_err_file(path_err)

    if (!info) err$data <- err$data[!J("Info"), on = "level"]

    err
}
# }}}
# job_sql_path {{{
job_sql_path <- function (self, private) {
    path_sql <- job_locate_output(self, private, ".sql", must_exist = FALSE)
    if (!file.exists(path_sql))
        stop("Simulation SQL output does not exists. ",
             "eplusr uses the EnergyPlus SQL output for extracting simulation outputs. ",
             "Please add an object in `Output:SQLite` with `Option Type` value of `SimpleAndTabular` ",
             "and run the Idf again. It is recommended to first read that IDF file using `read_idf()` ",
             "and then use `$run()` method in Idf class by doing `idf$run()` ",
             "which automatically handle this.", call. = FALSE)
    path_sql
}
# }}}
# job_list_table {{{
job_list_table <- function (self, private) {
    list_sql_table(job_sql_path(self, private))
}
# }}}
# job_read_table {{{
job_read_table <- function (self, private, table) {
    read_sql_table(job_sql_path(self, private), table)
}
# }}}
# job_report_data_dict {{{
job_report_data_dict <- function (self, private) {
    get_sql_report_data_dict(job_sql_path(self, private))
}
# }}}
# job_report_data {{{
job_report_data <- function (self, private, key_value = NULL, name = NULL, year = NULL,
                             tz = "UTC", case = "auto", all = FALSE,
                             period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                             interval = NULL, simulation_days = NULL, day_type = NULL,
                             environment_name = NULL) {
    if (identical(case, "auto")) case <- tools::file_path_sans_ext(basename(job_sql_path(self, private)))
    get_sql_report_data(job_sql_path(self, private),
        key_value = key_value, name = name, year = year,
        tz = tz, case = case, all = all,
        period = period, month = month, day = day, hour = hour, minute = minute,
        interval = interval, simulation_days = simulation_days, day_type = day_type,
        environment_name = environment_name
    )
}
# }}}
# job_tabular_data {{{
job_tabular_data <- function (self, private, report_name = NULL, report_for = NULL,
                              table_name = NULL, column_name = NULL, row_name = NULL) {
    get_sql_tabular_data(job_sql_path(self, private), report_name = report_name, report_for = report_for,
        table_name = table_name, column_name = column_name, row_name = row_name)
}
# }}}
# job_print {{{
job_print <- function (self, private) {
    status <- job_status(self, private)
    cli::cat_rule("EnergyPlus Simulation Job")
    config <- eplus_config(private$m_version)
    cli::cat_line(c(
        paste0("* Model: ", surround(str_trunc(private$m_path_idf, getOption("width") - 11L))),
        paste0("* Weather: ", surround(str_trunc(private$m_path_epw, getOption("width") - 13L))),
        paste0("* EnergyPlus Version: ", surround(config$version)),
        paste0("* EnergyPlus Path: ", surround(str_trunc(normalizePath(config$dir), getOption("width") - 21L)))
    ))

    if (!status$run_before) {
        cli::cat_line("< Simulation has not been run before >",
            col = "white", background_col = "blue")
    } else if (isTRUE(status$terminated)) {
        cli::cat_line(" Simulation was terminated before.",
            col = "white", background_col = "red")
    } else if (status$alive) {
        cli::cat_line(" Simulation started at ",
            surround(private$m_log$start_time), " and is still running...",
            col = "black", background_col = "green")
    } else if (!isTRUE(status$successful)) {
        cli::cat_line(" Simulation started at ",
            surround(private$m_log$start_time), " and ended unsuccessfully...",
            col = "white", background_col = "red")
    } else {
        job_update_endtime(self, private)

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
}
# }}}
# job_update_endtime {{{
job_update_endtime <- function (self, private) {
    if (is.null(private$m_log$end_time)) {

        if (is.null(private$m_job$stdout)) {
            private$m_job$stdout <- private$m_job$process$read_all_output_lines()
        }

        if (is.null(private$m_job$stderr)) {
            private$m_job$stderr <- private$m_job$process$read_all_error_lines()
        }

        run_time <- get_run_time(private$m_job$stdout)

        if (!is.null(run_time)) {
            private$m_job$end_time <- run_time + private$m_job$start_time
            private$m_log$end_time <- private$m_job$end_time
        }
    }
}
# }}}
# S3 EplusJob methods {{{
#' @export
str.EplusSql <- function (object, ...) {
    object$print()
}

#' @export
format.EplusSql <- function (x, ...) {
    paste0(utils::capture.output(x$print()), collapse = "\n")
}
# }}}
