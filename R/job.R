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
#' Table"* in EnergyPlus *"Output Details and Examples"* documentation. An
#' object in `Output:SQLite` with `Option Type` value of `SimpleAndTabular` will
#' be automatically created if it does not exists, to ensure that the output
#' collection functionality works successfully.
#'
#' In order to make sure `.rdd` (Report Data Dictionary) and `.mdd` (Meter Data
#' Dictionary) files are created during simulation, an object in
#' `Output:VariableDictionary` class with `Key Field` value being `IDF` will be
#' automatically created if it does not exists.
#'
#' @docType class
#' @name EplusJob
#' @seealso [ParametricJob] class for EnergyPlus parametric simulations.
#' @author Hongyuan Jia
NULL

#' @export
# EplusJob {{{
EplusJob <- R6::R6Class(classname = "EplusJob", cloneable = FALSE, inherit = EplusSql,
    public = list(

        # INITIALIZE {{{
        #' @description
        #' Create an `EplusJob` object
        #'
        #' @param idf Path to an local EnergyPlus IDF file or an [Idf] object.
        #' @param epw Path to an local EnergyPlus EPW file or an [Epw] object.
        #'
        #' @return An `EplusJob` object.
        #'
        #' @examples
        #' if (is_avail_eplus(8.8)) {
        #'     idf_name <- "1ZoneUncontrolled.idf"
        #'     epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
        #'
        #'     idf_path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", idf_name)
        #'     epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData", epw_name)
        #'
        #'     job <- EplusJob$new(idf_path, epw_path)
        #' }
        initialize = function (idf, epw) {
            job_initialize(self, private, idf, epw)
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        # version {{{
        #' @description
        #' Get the version of IDF in current job
        #'
        #' @details
        #' `$version()` reutrns the version of IDF that current `EplusJob` uses.
        #'
        #' @return A [base::numeric_version()] object.
        #'
        #' @examples
        #' \dontrun{
        #' job$version()
        #' }
        #'
        version = function ()
            job_version(self, private),
        # }}}

        # path {{{
        #' @description
        #' Get the paths of file that current `EpwSql` uses
        #'
        #' @details
        #' `$path()` returns the path of IDF or EPW of current job.
        #'
        #' @param type If `"all"`, both the [Idf] path and [Epw] path are
        #'        returned. If `"idf"`, only IDF path is returned. If `"epw"`,
        #'        only EPW path is returned. If `epw` is `NULL`, `NA` is
        #'        returned for EPW path. Default: `"all"`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' job$path()
        #' job$path("idf")
        #' job$path("epw")
        #' }
        #'
        path = function (type = c("all", "idf", "epw"))
            job_path(self, private, type),
        # }}}

        # run {{{
        #' @description
        #' Run simulationA
        #'
        #' @details
        #' `$run()` runs the simulation using input IDF and EPW file. If `wait`
        #' is `FALSE`, the job is run in the background. You can get updated job
        #' status by just
        #' \href{../../eplusr/html/EplusJob.html#method-print}{printing}
        #' the `EplusJob` object.
        #'
        #' Parameter `epw` can be used to reset the EPW file to use for
        #' simulation. If not given, the `epw` input used when creating
        #' this `EplusJob` object will be used.
        #'
        #' @param epw A path to an `.epw` file or an [Epw] object. `epw` can
        #'        also be `NULL` which will force design-day-only simulation.
        #'        Note this needs at least one `Sizing:DesignDay` object exists
        #'        in the `Idf`. If not given, the `epw` input used when creating
        #'        this `EplusJob` object will be used.
        #' @param dir The directory to save the simulation results. If `NULL`,
        #'        the input `idf` folder will be used. Default: `NULL`.
        #' @param wait If `TRUE`, R will hang on and wait for the simulation to
        #'        complete. EnergyPlus standard output (stdout) and error
        #'        (stderr) is printed to R console. If `FALSE`, simulation will
        #'        be run in a background process.  Default: `TRUE`.
        #' @param echo Only applicable when `wait` is `TRUE`. Whether to show
        #'        standard output and error from EnergyPlus. Default: same as
        #'        `wait`.
        #' @param force Only applicable when the last job runs with `wait`
        #'        equals to `FALSE` and is still running. If `TRUE`, current
        #'        running job is forced to stop and a new one will start.
        #'        Default: `FALSE`.
        #' @param copy_external If `TRUE`, the external files that current `Idf`
        #'        object depends on will also be copied into the simulation
        #'        output directory. The values of file paths in the Idf will be
        #'        changed automatically. Currently, only `Schedule:File` class
        #'        is supported.  This ensures that the output directory will
        #'        have all files needed for the model to run. Default is
        #'        `FALSE`.
        #'
        #' @return The `EplusJob` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # only run design day
        #' job$run(NULL)
        #'
        #' # specify output directory
        #' job$run(tempdir())
        #'
        #' # run in the background
        #' job$run(wait = TRUE)
        #' # see job status
        #' job$status()
        #'
        #' # force to kill background job before running the new one
        #' job$run(force = TRUE)
        #'
        #' # do not show anything in the console
        #' job$run(echo = FALSE)
        #'
        #' # copy external files used in the model to simulation output directory
        #' job$run(copy_external = TRUE)
        #' }
        #'
        run = function (epw, dir = NULL, wait = TRUE, force = FALSE, echo = wait, copy_external = FALSE)
            job_run(self, private, epw, dir, wait, force, echo, copy_external),
        # }}}

        # kill {{{
        #' @description
        #' Kill current running job
        #'
        #' @details
        #' `$kill()` kills the background EnergyPlus process if possible. It
        #' only works when simulation runs in non-waiting mode.
        #'
        #' @return A single logical value of `TRUE` or `FALSE`, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' job$kill()
        #' }
        #'
        kill = function ()
            job_kill(self, private),
        # }}}

        # status {{{
        #' @description
        #' Get the job status
        #'
        #' @details
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
        #' @return A named list of 5 elements.
        #'
        #' @examples
        #' \dontrun{
        #' job$status()
        #' }
        #'
        status = function ()
            job_status(self, private),
        # }}}

        # errors {{{
        #' @description
        #' Read simulation errors
        #'
        #' @details
        #' $errors() returns an [ErrFile][read_err()] object which contains all
        #' contents of the simulation error file (`.err`). If `info` is `FALSE`,
        #' only warnings and errors are printed.
        #'
        #' @param info If `FALSE`, only warnings and errors are printed.
        #'        Default: `FALSE`.
        #'
        #' @return An [ErrFile][read_err()] object.
        #'
        #' @examples
        #' \dontrun{
        #' job$errors()
        #'
        #' # show all information
        #' job$errors(info = TRUE)
        #' }
        #'
        errors = function (info = FALSE)
            job_output_errors(self, private, info),
        # }}}

        # output_dir {{{
        #' @description
        #' Get simulation output directory
        #'
        #' @details
        #' `$output_dir()` returns the output directory of simulation results.
        #'
        #' @param open If `TRUE`, the output directory will be opened.
        #'
        #' @examples
        #' \dontrun{
        #' job$output_dir()
        #'
        #' # open output directory
        #' job$output_dir(open = TRUE)
        #' }
        #'
        output_dir = function (open = FALSE)
            job_output_dir(self, private, open),
        # }}}

        # locate_output {{{
        #' @description
        #' Get path of output file
        #'
        #' @details
        #' `$locate_output()` returns the path of a single output file specified
        #' by file suffix.
        #'
        #' @param suffix A string that indicates the file extension of
        #'        simulation output. Default: `".err"`.
        #' @param strict If `TRUE`, it will check if the simulation was
        #'        terminated, is still running or the file exists or not.
        #'        Default: `TRUE`.
        #'
        #' @examples
        #' \dontrun{
        #' # get the file path of the error file
        #' job$locate_output(".err", strict = FALSE)
        #'
        #' # can use to detect if certain output file exists
        #' job$locate_output(".expidf", strict = TRUE)
        #' }
        #'
        locate_output = function (suffix = ".err", strict = TRUE)
            job_locate_output(self, private, suffix, strict, must_exist = strict),
        # }}}

        # read_rdd {{{
        #' @description
        #' Read Report Data Dictionary (RDD) file
        #'
        #' @details
        #' `$read_rdd()` return the core data of Report Data Dictionary (RDD)
        #' file. For details, please see [read_rdd()].
        #'
        #' @return An [RddFile][read_rdd()] object.
        #'
        #' @examples
        #' \dontrun{
        #' job$read_rdd()
        #' }
        #'
        read_rdd = function ()
            job_read_rdd(self, private),
        # }}}

        # read_mdd {{{
        #' @description
        #' Read Report Data Dictionary (RDD) file
        #'
        #' @details
        #' `$read_mdd()` return the core data of Meter Data Dictionary (MDD)
        #' file. For details, please see [read_mdd()].
        #'
        #' @return An [MddFile][read_mdd()] object.
        #'
        #' @examples
        #' \dontrun{
        #' job$read_mdd()
        #' }
        #'
        read_mdd = function ()
            job_read_mdd(self, private),
        # }}}

        # print {{{
        #' @description
        #' Print `EplusSql` object
        #'
        #' @details
        #' `$print()` shows the core information of this `EplusJob` object,
        #' including the path of model and weather, the version and path of
        #' EnergyPlus used to run simulations, and the simulation job status.
        #'
        #' `$print()` is quite useful to get the simulation status, especially
        #' when `wait` is `FALSE` in `$run()`. The job status will be updated
        #' and printed whenever `$print()` is called.
        #'
        #' @return The `EplusSql` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' job$print()
        #' }
        #'
        print = function ()
            job_print(self, private)
        # }}}
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_idf = NULL,
        m_epw = NULL,
        m_job = NULL,
        m_log = NULL
        # }}}
    )
)
# }}}

#' Create an EnergyPlus Simulation Job
#'
#' `eplus_job()` takes an IDF and EPW as input, and returns an `EplusJob` object
#' for running EnergyPlus simulation and collecting outputs. For more details,
#' please see [EplusJob].
#'
#' @param idf A path to an local EnergyPlus IDF file or an `Idf` object.
#' @param epw A path to an local EnergyPlus EPW file or an `Epw` object. `epw`
#' can also be `NULL` which will force design-day-only simulation when
#' [`$run()`][EplusJob] method is called. Note this needs at least one
#' `Sizing:DesignDay` object exists in the [Idf].
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

# job_initialize {{{
job_initialize <- function (self, private, idf, epw) {
    private$m_idf <- get_init_idf(idf)
    if (!is.null(epw)) private$m_epw <- get_init_epw(epw)

    # add Output:SQLite if necessary
    add_sql <- idf_add_output_sqlite(private$m_idf)
    # add Output:VariableDictionary if necessary
    add_dict <- idf_add_output_vardict(private$m_idf)
    # log if the input idf has been changed
    private$m_log$unsaved <- add_sql || add_dict

    # save uuid
    private$m_log$seed_uuid <- ._get_private(private$m_idf)$m_log$uuid
}
# }}}
# job_version {{{
job_version <- function (self, private) {
    private$m_idf$version()
}
# }}}
# job_path {{{
job_path <- function (self, private, type = c("all", "idf", "epw")) {
    type <- match.arg(type)

    path_epw <- if (is.null(private$m_epw)) NA_character_ else private$m_epw$path()
    switch(type,
        all = c(idf = private$m_idf$path(), epw = path_epw),
        idf = private$m_idf$path(), epw = path_epw
    )
}
# }}}
# job_run {{{
job_run <- function (self, private, epw, dir = NULL, wait = TRUE, force = FALSE,
                     echo = wait, copy_external = FALSE) {
    # stop if idf object has been changed accidentally
    if (!identical(._get_private(private$m_idf)$m_log$uuid, private$m_log$seed_uuid)) {
        abort("error_job_idf_modified", paste0(
            "The idf has been modified after job was created. ",
            "Running this idf will result in simulation outputs that may be not reproducible.",
            "Please recreate the job using new idf and then run it."
        ))
    }

    if (missing(epw)) epw <- private$m_epw

    if (is.null(epw)) {
        private$m_epw <- epw
        path_epw <- NULL
    } else {
        private$m_epw <- get_init_epw(epw)
        path_epw <- private$m_epw$path()
    }

    path_idf <- private$m_idf$path()
    if (is.null(dir))
        run_dir <- dirname(path_idf)
    else {
        run_dir <- dir
        path_idf <- normalizePath(file.path(run_dir, basename(path_idf)), mustWork = FALSE)
    }

    # if necessary, resave the model
    if (private$m_log$unsaved || !is.null(dir)) {
        path_idf <- private$m_idf$save(path_idf, overwrite = TRUE, copy_external = copy_external)
    }

    # when no epw is given, at least one design day object should exists
    if (is.null(private$m_epw)) {
        if (!private$m_idf$is_valid_class("SizingPeriod:DesignDay")) {
            assert("error_run_no_ddy",
                paste0("When no weather file is given, input IDF should contain ",
                    "`SizingPeriod:DesignDay` object to enable Design-Day-only ",
                    "simulation."
                )
            )
        }
    }

    # check if the model is still running
    old <- private$m_job
    if (!is.null(old)) {
        proc <- old$process
        if (inherits(proc, "process") && proc$is_alive()) {
            pid <- proc$get_pid()
            if (force) {
                verbose_info("Force to kill current running simulation (PID: ", pid,
                    ") and start a new simulation...")
                suppressMessages(self$kill())
            } else {
                abort("error_job_still_alive", paste0(
                    "The simulation of current Idf is still running (PID: ",
                    pid, "). Please set `force` to TRUE if you want ",
                    "to kill the running process and start a new simulation."
                ))
            }
        }
    }

    private$m_log$start_time <- Sys.time()
    private$m_log$killed <- NULL

    private$m_job <- run_idf(path_idf, path_epw,
        output_dir = NULL, echo = echo, wait = wait, eplus = private$m_version,
        design_day = is.null(private$m_epw)
    )

    if (wait) private$m_log$end_time <- Sys.time()
    self
}
# }}}
# job_kill {{{
job_kill <- function (self, private) {
    if (is.null(private$m_job)) {
        verbose_info("The job has not been run yet.")
        return(invisible(FALSE))
    }

    proc <- private$m_job$process

    if (!proc$is_alive()) {
        verbose_info("The job is not running.")
        return(invisible(FALSE))
    }

    k <- tryCatch(proc$kill(), error = function (e) FALSE)

    if (isTRUE(k)) {
        private$m_log$killed <- TRUE
        verbose_info("The job has been successfully killed.")
        return(invisible(TRUE))
    } else {
        verbose_info("Failed to kill the job, because it was already finished/dead.")
        return(invisible(FALSE))
    }
}
# }}}
# job_status {{{
job_status <- function (self, private) {
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
        if (!file.exists(private$m_idf$path())) {
            warning("Could not find local idf file ", surround(private$m_idf$path()),
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
    if (!identical(private$m_log$seed_uuid, ._get_private(private$m_idf)$m_log$uuid)) {
        status$changed_after <- TRUE
    }

    status
}
# }}}
# job_output_dir {{{
job_output_dir <- function (self, private, open = FALSE) {
    dir <- dirname(private$m_idf$path())
    if (!open) return(dir)
    if (open) {
        if (is.null(dir)) {
            verbose_info("No simulation has been run yet.")
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
            verbose_info("Current platform not supported.")
        }
    }
    dir
}
# }}}
# job_locate_output {{{
job_locate_output <- function (self, private, suffix = ".err", strict = TRUE, must_exist = TRUE) {
    out <- paste0(tools::file_path_sans_ext(private$m_idf$path()), suffix)

    if (strict) {
        status <- job_status(self, private)

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

    if (must_exist) {
        assert(file.exists(out), msg = paste0("File ", surround(out), " does not exists."),
            err_type = "error_file_not_exist"
        )
    }

    out
}
# }}}
# job_output_errors {{{
job_output_errors <- function (self, private, info = FALSE) {
    path_err <- job_locate_output(self, private, ".err")

    err <- parse_err_file(path_err)

    if (!info) err[!J("Info"), on = "level"] else err
}
# }}}
# job_sql_path {{{
job_sql_path <- function (self, private) {
    path_sql <- job_locate_output(self, private, ".sql", must_exist = FALSE)
    if (!file.exists(path_sql)) {
        abort("error_sql_not_exist", paste0("Simulation SQL output does not exist."))
    }
    path_sql
}
# }}}
# job_rdd_path {{{
job_rdd_path <- function (self, private, type = c("rdd", "mdd")) {
    type <- match.arg(type)
    path <- job_locate_output(self, private, paste0(".", type), must_exist = FALSE)
    name <- switch(type,
        rdd = "Report Data Dictionary (RDD) file",
        mdd = "Meter Data Dictionary (MDD) file"
    )
    if (!file.exists(path)) {
        assert(paste0("error_", type, "_not_exist"), paste0(name, " does not exist."))
    }

    path
}
# }}}
# job_list_table {{{
job_list_table <- function (self, private) {
    list_sql_table(job_sql_path(self, private))
}
# }}}
# job_read_rdd {{{
job_read_rdd <- function (self, private) {
    read_rdd(job_rdd_path(self, private, "rdd"))
}
# }}}
# job_read_mdd {{{
job_read_mdd <- function (self, private) {
    read_mdd(job_rdd_path(self, private, "mdd"))
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
                             tz = "UTC", case = "auto", all = FALSE, wide = FALSE,
                             period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                             interval = NULL, simulation_days = NULL, day_type = NULL,
                             environment_name = NULL) {
    if (identical(case, "auto")) case <- tools::file_path_sans_ext(basename(job_sql_path(self, private)))
    get_sql_report_data(job_sql_path(self, private),
        key_value = key_value, name = name, year = year,
        tz = tz, case = case, all = all, wide = wide,
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
    path_epw <- if (is.null(private$m_epw)) NULL else private$m_epw$path()
    print_job_header(title = "EnergPlus Simulation Job",
        path_idf = private$m_idf$path(),
        path_epw = path_epw,
        eplus_ver = private$m_idf$version(),
        name_idf = "Model", name_epw = "Weather"
    )
    status <- job_status(self, private)

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

# helper
# get_init_idf {{{
get_init_idf <- function (idf) {
    if (!is_idf(idf)) return(read_idf(idf))

    idf <- idf$clone(deep = TRUE)

    if (is.null(idf$path())) {
        abort("error_idf_not_local",
            paste0(
                "The Idf object is not created from local file. ",
                "Please save it using `$save()` before running."
            )
        )
    }

    if (!utils::file_test("-f", idf$path())) {
        abort("error_idf_path_not_exist",
            paste0(
                "Failed to locate the local IDF file of input Idf object. ",
                "Path: ", surround(idf$path()), " ",
                "Please re-save it to disk using `$save()` before running."
            )
        )
    }

    if (idf$is_unsaved()) {
        abort("error_idf_not_saved",
            paste0("Idf has been modified since read or last saved. ",
                "Please save it using `$save()` before running."
            )
        )
    }

    idf
}
# }}}
# get_init_epw {{{
get_init_epw <- function (epw) {
    if (!is_epw(epw)) return(read_epw(epw))

    epw <- epw$clone(deep = TRUE)

    if (is.null(epw$path())) {
        abort("error_epw_not_local",
            paste0(
                "The Epw object is not created from local file. ",
                "Please save it using `$save()` before running."
            )
        )
    }

    if (!utils::file_test("-f", epw$path())) {
        abort("error_epw_path_not_exist",
            paste0(
                "Failed to locate the local EPW file of input Epw object. ",
                "Path: ", surround(epw$path()), " ",
                "Please re-save it to disk using `$save()` before running."
            )
        )
    }

    if (epw$is_unsaved()) {
        abort("error_epw_not_saved",
            paste0("Epw has been modified since read or last saved. ",
                "Please save it using `$save()` before running."
            )
        )
    }

    epw
}
# }}}
# print_job_header {{{
print_job_header <- function (title = "EnergyPlus Simulation Job", path_idf, path_epw,
                              eplus_ver = read_idf(path_idf)$version(),
                              name_idf = "Model", name_epw = "Weather") {
    cli::cat_rule(title, col = "green")
    config <- eplus_config(eplus_ver)
    cli::cat_line(c(
        str_trunc(paste0("* ", name_idf, ": ", surround(normalizePath(path_idf, mustWork = FALSE)))),
        str_trunc(paste0("* ", name_epw, ": ", if (is.null(path_epw)) "<< Not specified >>" else surround(path_epw))),
        paste0("* EnergyPlus Version: ", surround(config$version)),
        str_trunc(paste0("* EnergyPlus Path: ", surround(normalizePath(config$dir))))
    ))
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
