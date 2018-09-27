#' @importFrom R6 R6Class
#' @importFrom readr read_lines
#' @importFrom cli cat_bullet cat_line cat_rule
#' @importFrom crayon bold
#' @importFrom stringr str_trim
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
#' When using `$run()` in [Idf] class, which internally creates an
#'     `EplusJob` object and calls its `$run()` method, an object in
#'     `Output:SQLite` with `Option Type` value of `SimpleAndTabular` will be
#'     automatically created if it does not exists.
#'
#' However, when creating an `EplusJob` using [eplus_job()], the IDF file is not
#'     parsed but directly pass its path to EnergyPlus. Thus, that process of
#'     handling `Output:SQLite` class is not performed. If you want to ensure
#'     that the output collection functionality in `EplusJob` class works
#'     successfully, it is recommended to first read that IDF file using
#'     [read_idf()] and then use `$run()` method in [Idf] class by doing
#'     `idf$run()`.
#'
#' @section Usage:
#' ```
#' job$run(wait = TRUE)
#' job$kill()
#' job$status()
#' job$errors(info = FALSE)
#' job$output_dir(open = FALSE)
#' job$locate_output(suffix = ".err", strict = TRUE)
#' job$report_data_dict()
#' job$report_data(key_value = NULL, name = NULL, year = NULL, tz = "GMT", case = "auto")
#' job$tabular_data()
#' job$clone(deep = FALSE)
#' job$print()
#' ```
#'
#' @section Create:
#' ```
#' job <- eplus_job(idf, epw)
#' ```
#'
#' **Arguments**
#'
#' * `idf`: Path to an local EnergyPlus IDF file or an `Idf` object.
#' * `epw`: Path to an local EnergyPlus EPW file or an `Epw` object.
#'
#' @section Basic info:
#' ```
#' job$path(type = c("all", "idf", "epw"))
#' ```
#'
#' `$path()` returns the path of IDF or EPW of current job.
#'
#' **Arguments**
#'
#' * `type`: If `"all"`, both the IDF path and EPW path are returned. If
#'     `"idf"`, only IDF path is returned. If `"epw"`, only EPW path is
#'     returned.  Default: `"all"`.
#'
#' @section Run:
#' ```
#' job$run(wait = TRUE)
#' job$kill()
#' job$status()
#' ```
#'
#' `$run()` runs the simulation using input model and weather file.
#'     If `wait` is FALSE, then the job will be run in the background. You can
#'     get updated job status by just print the EplusJob object.
#'
#' `$kill()` kills the background EnergyPlus process if possible. It only
#'     works when simulation runs in non-waiting mode.
#'
#' `$status()` returns a named list of values indicates the status of the job:
#'
#'   * `run_before`: `TRUE` if the job has been run before. `FALSE` otherwise.
#'   * `alive`: `TRUE` if the simulation is still running in the background.
#'     `FALSE` otherwise.
#'   * `terminated`: `TRUE` if the simulation was terminated during last
#'      simulation. `FALSE` otherwise. `NA` if the job has not been run yet.
#'   * `successful`: `TRUE` if last simulation ended successfully. `FALSE`
#'     otherwise. `NA` if the job has not been run yet.
#'   * `changed_after`: `TRUE` if the IDF file has been changed since last
#'      simulation. `FALSE` otherwise. `NA` if the job has not been run yet.
#'
#' **Arguments**
#'
#' * `wait`: If `TRUE`, R will hang on and wait for the simulation to complete.
#'     EnergyPlus standard output (stdout) and error (stderr) is printed to the
#'     R console. If `FALSE`, simulation will be run in a background process.
#'     Default: `TRUE`.
#'
#' @section Results Extraction:
#' ```
#' job$output_dir(open = FALSE)
#' job$locate_output(suffix = ".err", strict = TRUE)
#' job$report_data_dict()
#' job$report_data(key_value = NULL, name = NULL, year = NULL, tz = "GMT", case = "auto")
#' job$tabular_data()
#' ```
#'
#' `$output_dir()` returns the output directory of simulation results.
#'
#' `$locate_output()` returns the path of a single output file specified by file
#'     suffix.
#'
#' `$report_data_dict()` returns a data.table which contains all information about
#'     report data. For details on the meaning of each columns, please see
#'     "2.20.2.1 ReportDataDictionary Table" in EnergyPlus "Output Details and
#'     Examples" documentation.
#'
#' `$report_data()` extracts the report data in a data.table using key values
#'     and variable names.
#'
#' `$tabular_data()` extracts all tabular data in a data.table.
#'
#' **Arguments**:
#'
#' * `open`: If `TRUE`, the output directory will be opened. It may only work
#'     well on Windows.
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
#' * `case`: If not `NULL`, a character column will be added indicates the case
#'     of this simulation. If `"auto"`, the name of the IDF file will be used.
#'
#' @section Clone:
#' ```
#' job$clone(deep = FALSE)
#' ```
#'
#' `$clone()` copies and returns the cloned job. Because `EplusJob` uses
#'     `R6Class` under the hook which has "modify-in-place" semantics, `job_2 <-
#'     job_1` does not copy `job_1` at all but only create a new binding to
#'     `job_1`.  Modify `job_1` will also affect `job_2` as well, as these two
#'     are exactly the same thing underneath. In order to create a complete
#'     cloned copy, please use `$clone(deep = TRUE)`.
#'
#' **Arguments**
#'
#' * `deep`: Has to be `TRUE` if a complete cloned copy is desired.
#'
#' @section Printing:
#' ```
#' job$print()
#' print(job)
#' ```
#'
#' `$print()` shows the core information of this EplusJob, including the
#'     path of model and weather, the version and path of EnergyPlus used
#'     to run simulations, and the simulation job status.
#'
#' `$print()` is quite useful to get the simulation status, especially when
#'     `wait` is `FALSE` in `$run()`. The job status will be updated and printed
#'     whenever `$print()` is called.
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
#'     # add a "Case" column in the returned data.table
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
                if (is.null(private$m_path_idf))
                    stop("The Idf object is not created from local file. ",
                         "Please give save it to disk before run.", call. = FALSE)
            } else {
                assert_that(is_string(idf))
                private$m_path_idf <- idf
            }

            if (!file.exists(private$m_path_idf))
                stop("Input idf does not exists.", call. = FALSE)

            private$m_path_idf <- normalizePath(private$m_path_idf, mustWork = TRUE)

            if (is_epw(epw)) {
                private$m_path_epw <- epw$path()
                if (is.null(private$m_path_epw))
                    stop("The Epw object is not created from local file. ",
                         "Please give save it to disk before run.", call. = FALSE)
            } else {
                assert_that(is_string(epw))
                private$m_path_epw <- epw
            }

            if (!file.exists(private$m_path_epw))
                stop("Input weather file does not exists.", call. = FALSE)

            private$m_path_epw <- normalizePath(private$m_path_epw, mustWork = TRUE)

            # get Idf version
            if (!is.null(eplus_ver)) {
                assert_that(is_eplus_ver(eplus_ver, strict = TRUE))
            } else {
                eplus_ver <- get_idf_ver(stringr::str_trim(readr::read_lines(private$m_path_idf), "both"))
                if (is.null(eplus_ver))
                    stop("Could not find version of input idf file.", call. = FALSE)
            }

            if (eplus_ver < 8.3)
                stop("Currently, `EplusJob` only supports EnergyPlus V8.3.0 or higher.", call. = FALSE)

            if (!is_avail_eplus(eplus_ver))
                stop("Could not locate EnergyPlus v", eplus_ver, ". Please set ",
                    "the path of EnergyPlus v", eplus_ver, "using `use_eplus()`.", call. = FALSE)

            private$m_version <- eplus_ver
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        path = function (type = c("all", "idf", "epw"))
            i_job_path(self, private, type),

        run = function (wait = TRUE)
            i_job_run(self, private, wait = wait),

        kill = function ()
            i_job_kill(self, private),

        status = function ()
            i_job_status(self, private, based_suffix = ".err"),

        output_dir = function (open = FALSE)
            i_job_output_dir(self, private, open),

        locate_output = function (suffix = ".err", strict = TRUE)
            i_job_locate_output(self, private, suffix, strict, must_exist = strict),

        errors = function (info = FALSE)
            i_job_output_errors(self, private, info),

        report_data_dict = function ()
            i_job_report_data_dict(self, private),

        report_data = function (key_value = NULL, name = NULL,
                                year = NULL, tz = "GMT", case = "auto")
            i_job_report_data(self, private, key_value, name, year, tz, case),

        tabular_data = function()
            i_job_tabular_data(self, private),

        print = function ()
            i_job_print(self, private)
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_version = NULL,
        m_path_idf = NULL,
        m_path_epw = NULL,
        m_eplus_config = NULL,
        m_job = NULL,
        m_sql = NULL,
        m_log = NULL,
        # }}}

        deep_clone = function (name, value)
            i_deep_clone(self, private, name, value)
    )
)
# }}}

# i_job_path {{{
i_job_path <- function (self, private, type = c("all", "idf", "epw")) {
    type <- match.arg(type)

    switch(type,
        all = c(private$m_path_idf, private$m_path_epw),
        idf = private$m_path_idf, epw = private$m_path_epw
    )
}
# }}}

# i_job_run {{{
i_job_run <- function (self, private, wait = TRUE) {
    private$m_log$start_time <- Sys.time()
    private$m_log$killed <- NULL

    private$m_job <- run_idf(private$m_path_idf, private$m_path_epw,
        output_dir = NULL, echo = wait, wait = wait, eplus = private$m_version)

    if (wait) private$m_log$end_time <- Sys.time()
    self
}
# }}}

# i_job_kill {{{
i_job_kill <- function (self, private) {
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

# i_job_status {{{
i_job_status <- function (self, private, based_suffix = ".err") {
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
            warning("Could not find local idf file ", backtick(private$m_path_idf),
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

# i_job_output_dir {{{
i_job_output_dir <- function (self, private, open = FALSE) {
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

# i_job_locate_output {{{
i_job_locate_output <- function (self, private, suffix = ".err", strict = TRUE, must_exist = TRUE) {
    out <- paste0(tools::file_path_sans_ext(private$m_path_idf), suffix)

    if (strict) {
        status <- i_job_status(self, private, suffix)

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

    if (must_exist) assert_that(file.exists(out))

    out
}
# }}}

# i_job_output_errors {{{
i_job_output_errors <- function (self, private, info = FALSE) {
    path_err <- i_job_locate_output(self, private, ".err")

    err <- parse_err_file(path_err)

    if (!info) err$data <- err$data[!(level == "Info" & begin_environment == FALSE)]

    err
}
# }}}

# i_job_sql_path {{{
i_job_sql_path <- function (self, private) {
    path_sql <- i_job_locate_output(self, private, ".sql", must_exist = FALSE)
    if (!file.exists(path_sql))
        stop("Simulation SQL output does not exists. ",
             "eplusr uses the EnergyPlus SQL output for extracting simulation outputs. ",
             "Please add an object in `Output:SQLite` with `Option Type` value of `SimpleAndTabular` ",
             "and run the Idf again. It is recommended to first read that IDF file using read_idf() ",
             "and then use `$run()` method in Idf class by doing `idf$run()` ",
             "which automatically handle this.", call. = FALSE)
    path_sql
}
# }}}

# i_job_report_data_dict {{{
i_job_report_data_dict <- function (self, private) {
    sql <- i_job_sql_path(self, private)
    sql_report_data_dict(sql)
}
# }}}

# i_job_report_data {{{
i_job_report_data <- function (self, private, key_value = NULL, name = NULL,
                               year = NULL, tz = "GMT", case = "auto", all = FALSE) {
    sql <- i_job_sql_path(self, private)
    if (identical(case, "auto")) case <- tools::file_path_sans_ext(basename(private$m_path_idf))
    sql_report_data(sql, key_value, name, year, tz, case, all)
}
# }}}

# i_job_tabular_data {{{
i_job_tabular_data <- function (self, private) {
    sql <- i_job_sql_path(self, private)
    sql_tabular_data(sql)
}
# }}}

# i_job_print {{{
i_job_print <- function (self, private) {
    status <- i_job_status(self, private)
    cli::cat_rule(crayon::bold("EnergyPlus Simulation Job"), col = "green")
    config <- eplus_config(private$m_version)
    cli::cat_bullet(c(
        paste0(crayon::bold("Model"), ": ", backtick(private$m_path_idf)),
        paste0(crayon::bold("Weather"), ": ", backtick(private$m_path_epw)),
        paste0(crayon::bold("EnergyPlus Version"), ": ", backtick(config$version)),
        paste0(crayon::bold("EnergyPlus Path"), ": ", backtick(normalizePath(config$dir)))
    ), col = "cyan", bullet_col = "cyan")

    if (!status$run_before) {
        cli::cat_line("<< Simulation has not been run before >>",
            col = "white", background_col = "blue")
    } else if (isTRUE(status$terminated)) {
        cli::cat_line(" Simulation was terminated before.",
            col = "white", background_col = "red")
    } else if (status$alive) {
        cli::cat_line(" Simulation started at ",
            backtick(private$m_log$start_time), " and is still running...",
            col = "black", background_col = "green")
    } else if (!isTRUE(status$successful)) {
        cli::cat_line(" Simulation started at ",
            backtick(private$m_log$start_time), " and ended unsuccessfully...",
            col = "white", background_col = "red")
    } else {
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
}
# }}}
