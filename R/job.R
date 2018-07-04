#' @importFrom R6 R6Class
#' @importFrom readr read_lines
#' @importFrom tools file_path_sans_ext
#' @importFrom RSQLite SQLite dbConnect dbDisconnect dbGetQuery dbReadTable dbListTables
#' @importFrom data.table setDT setcolorder setorder
#' @importFrom lubridate year force_tz
#' @importFrom fasttime fastPOSIXct
#' @importFrom cli cat_rule cat_line
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
                assert_that(is_eplus_ver(eplus_ver))
            } else {
                eplus_ver <- get_idf_ver(readr::read_lines(private$m_path_idf))
                if (is.null(eplus_ver))
                    stop("Could not find version of input idf file.", call. = FALSE)
            }

            if (!is_avail_eplus(eplus_ver)) {
                stop("Could not locate EnergyPlus v", eplus_ver, " at ",
                     "the default installation path. Please set the path of ",
                     "EnergyPlus v", eplus_ver, "using `use_eplus()`.", call. = FALSE)
            }

            private$m_version <- eplus_ver
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        run = function (wait = TRUE)
            i_job_run(self, private, wait),

        status = function ()
            i_job_status(self, private, based_suffix = ".err"),

        output_dir = function (open = FALSE)
            i_job_output_dir(self, private, open),

        locate_output = function (suffix = ".err", strict = TRUE)
            i_job_locate_output(self, private, suffix, strict),

        errors = function (info = FALSE)
            i_job_output_errors(self, private, info),

        report_data_dict = function ()
            i_sql_report_data_dict(self, private),

        report_data = function (key_value = NULL, name = NULL, all = FALSE,
                                year = NULL, tz = "GMT", case = FALSE)
            i_sql_report_data(self, private, key_value, name, all, year, tz, case),

        tabular_data = function()
            i_sql_tabular_data(self, private),

        print = function ()
            i_job_print(self, private)
        # }}}
    ),

    # PRIVATE FIELDS {{{
    private = list(
        m_version = NULL,
        m_path_idf = NULL,
        m_path_epw = NULL,
        m_eplus_config = NULL,
        m_process = NULL,
        m_sql = NULL,
        m_log = NULL
    )
    # }}}
)

# i_job_run {{{
i_job_run <- function (self, private, wait = TRUE) {
    private$m_log$start_time <- Sys.time()

    private$m_process <- run_idf(private$m_version,
        private$m_path_idf, private$m_path_epw, echo = wait)

    private$m_log$end_time <- Sys.time()
}
# }}}

# i_job_status {{{
i_job_status <- function (self, private, based_suffix = ".err") {
    # init
    status <- list(
        run_before = FALSE, # if the model has been run before
        changed_after = FALSE, # if the model has been changed after last simulation
        terminated = FALSE, # if last simulation was terminated
        successful = FALSE, # if last simulation was successful
        alive = FALSE, # if simulation is still running
        wait = FALSE # if simulation run in wait mode
    )

    proc <- private$m_process
    # if the model has not been run before
    if (is.null(proc)) {
        if (!file.exists(private$m_path_idf)) {
            warning("Could not find local idf file ", backtick(private$m_path_idf),
                ".", call. = FALSE)
            return(status)
        }
    # if the model has been run before
    } else {
        status$run_before <- TRUE
        # check if the model was run in waiting mode
        if (!inherits(proc, "process")) {
            # check the exist status of last simulationa
            status$wait <- TRUE
            exit_status <- proc$status
            if (is.na(exit_status)) {
                status$terminated <- TRUE
            } else if (exit_status == 0) {
                status$successful <- TRUE
            }
        } else {
            exit_status <- proc$get_exit_status()
            # check if the model is still running
            if (proc$is_alive()) {
                status$alive <- TRUE
            } else if (!is.null(exit_status) && exit_status == 0L) {
                status$successful <- TRUE
            }
        }
    }

    prefix <- tools::file_path_sans_ext(private$m_path_idf)
    basefile <- paste0(prefix, based_suffix)

    if (!file.exists(basefile)) return(status)

    base_ctime <- file.info(basefile)$ctime
    idf_ctime <- file.info(private$m_path_idf)$ctime
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
i_job_locate_output <- function (self, private, suffix = ".err", strict = TRUE) {
    if (strict) {
        status <- i_job_status(self, private, suffix)

        if (status$terminated)
            stop("Simulation was terminated before. Please solve ",
                "the problems and re-run the simulation before collect ",
                "output", call. = FALSE)

        if (status$alive)
            stop("Simulation is still running. Please wait simulation ",
                "to finish before collecting results.", call. = FALSE)

        if (status$changed_after)
            warning("The Idf has been changed since last simulation. ",
                "The simulation output may not be correct.", call. = FALSE)

        if (status$run_before && !status$successful)
            warning("Simulation ended with errors. Simulation results ",
                "may not be correct.", call. = FALSE)
    }

    paste0(tools::file_path_sans_ext(private$m_path_idf), suffix)
}
# }}}

# i_job_output_errors {{{
i_job_output_errors <- function (self, private, info = FALSE) {
    path_err <- i_job_locate_output(self, private, ".err", strict = FALSE)

    err <- parse_err_file(path_err)

    if (!info) err$data <- err$data[!(level == "Info" & begin_environment == FALSE)]

    err
}
# }}}

# i_job_output_sql {{{
i_job_output_sql <- function (self, private) {
    path_sql <- i_job_locate_output(self, private, ".sql", strict = TRUE)
    if (!file.exists(path_sql))
        stop("Simulation SQL output does not exists.", call. = FASE)

    RSQLite::dbConnect(RSQLite::SQLite(), path_sql)
}
# }}}

# i_is_job_not_running {{{
i_is_job_not_running <- function (self, private) {
    proc <- private$m_process
    if (is.null(proc)) return(TRUE)

    if (!inherits(proc, "process")) return(TRUE)

    if (!proc$is_alive()) return(TRUE)

    FALSE
}
# }}}

# i_assert_job_not_running {{{
i_assert_is_not_running <- function (self, private) {
    not <- i_is_job_not_running(self, private)
    if (!not)
        stop("Simulation is still running.", call. = FALSE)
}
# }}}

# i_sql_read_table {{{
i_sql_read_table <- function (self, private, table) {
    sql <- i_job_output_sql(self, private)
    on.exit(RSQLite::dbDisconnect(sql), add = TRUE)
    res <- data.table::setDT(RSQLite::dbReadTable(sql, table))
    res
}
# }}}

# i_sql_get_query {{{
i_sql_get_query <- function (self, private, query) {
    sql <- i_job_output_sql(self, private)
    on.exit(RSQLite::dbDisconnect(sql), add = TRUE)

    res <- RSQLite::dbGetQuery(sql, query)
    if (is.data.frame(res)) data.table::setDT(res)
    res
}
# }}}

# i_sql_report_data_query {{{
i_sql_report_data_query <- function (self, private, key_value = NULL, name = NULL) {
    if (is.null(key_value)) {
        if (is.null(name)) {
            where <- "ReportDataDictionary"
        } else {
            stopifnot(is.character(name))
            where <- paste0(
                "
                SELECT *
                FROM ReportDataDictionary
                WHERE Name IN (", paste0("'", unique(name), "'", collapse = ","),")
                "
            )
        }
    } else {
        stopifnot(is.character(key_value))
        if (is.null(name)) {
            where <- paste0(
                "
                SELECT *
                FROM ReportDataDictionary
                WHERE KeyValue IN (", paste0("'", unique(key_value), "'", collapse = ","),")
                "
            )
        } else {
            where <- paste0(
                "
                SELECT *
                FROM ReportDataDictionary
                WHERE KeyValue IN (", paste0("'", unique(key_value), "'", collapse = ","),")
                      AND
                      Name IN (", paste0("'", unique(name), "'", collapse = ","),")
                "
            )
        }
    }

    query <- paste0(
        "
        SELECT t.Month,
               t.Day,
               t.Hour,
               t.Minute,
               t.Dst,
               t.Interval,
               t.IntervalType,
               t.SimulationDays,
               t.DayType,
               t.EnvironmentPeriodIndex,
               t.WarmupFlag,
               rdd.IsMeter,
               rdd.Type,
               rdd.IndexGroup,
               rdd.TimestepType,
               rdd.KeyValue,
               rdd.Name,
               rdd.ReportingFrequency,
               rdd.ScheduleName,
               rdd.Units,
               rd.Value
        FROM ReportData AS rd
        INNER JOIN
            (
                ", where, "
            ) As rdd
            ON rd.ReportDataDictionaryIndex = rdd.ReportDataDictionaryIndex
        INNER JOIN Time As t
            ON rd.TimeIndex = t.TimeIndex
        "
    )

    query
}
# }}}

# i_sql_all_table {{{
i_sql_all_table <- function (self, private) {
    sql <- i_job_output_sql(self, private)
    on.exit(RSQLite::dbDisconnect(sql), add = TRUE)
    RSQLite::dbListTables(sql)
}
# }}}

# i_sql_report_data_dict {{{
i_sql_report_data_dict <- function (self, private) {
    i_sql_read_table(self, private, "ReportDataDictionary")
}
# }}}

# i_sql_report_data {{{
i_sql_report_data <- function (self, private, key_value = NULL, name = NULL,
                               all = FALSE, year = NULL, tz = "GMT", case = FALSE) {
    q <- i_sql_report_data_query(self, private, key_value, name)
    res <- i_sql_get_query(self, private, q)

    Year <- year %||% lubridate::year(Sys.Date())
    res[, DateTime := fasttime::fastPOSIXct(
        paste0(Year, "-", Month, "-", Day, " ", Hour, ":", Minute, ":00"),
        required.components = 5L, tz = "GMT")]

    if (!all) {
        res <- res[, .SD, .SDcols = c("DateTime", "KeyValue", "Name", "Units", "Value")]
    } else {
        data.table::setcolorder(res, c("DateTime", setdiff(names(res), "DateTime")))
    }

    data.table::setorder(res, KeyValue, Name, DateTime)

    if (tz != "GMT") res$DateTime <- lubridate::force_tz(res$DateTime, tz)

    if (case) {
        res[, Case := tools::file_path_sans_ext(basename(private$m_path))]
        data.table::setcolorder(res, c("Case", setdiff(names(res), "Case")))
    }

    res
}
# }}}

# i_sql_tabular_data {{{
i_sql_tabular_data <- function (self, private) {
    i_sql_read_table(self, private, "TabularDataWithStrings")
}
# }}}

# i_job_print {{{
i_job_print <- function (self, private) {
    status <- i_job_status(self, private)
    cli::cat_rule("EnergyPlus Simulation Job")
    cli::cat_line("# Model: ", backtick(private$m_path_idf))
    cli::cat_line("# Weather: ", backtick(private$m_path_epw))
    config <- eplus_config(private$m_version)
    cli::cat_line("# EnergyPlus Version: ", backtick(config$version))
    cli::cat_line("# EnergyPlus Path: ", backtick(normalizePath(config$dir)))

    if (!status$run_before) {
        cli::cat_line("<< Simulation has not been run before >>")
    } else if (status$terminated) {
        cli::cat_line(" Simulation was terminated before.")
    } else if (status$alive) {
        cli::cat_line(" Simulation started at ",
            backtick(private$m_log$start_time), " and is still running...")
    } else if (!status$successful) {
        cli::cat_line(" Simulation started at ",
            backtick(private$m_log$start_time), " and ended unsuccessfully...")
    } else if (status$successful && status$wait) {
        take_time <- format(round(difftime(
            private$m_log$end_time, private$m_log$start_time), digits = 2L))
        cli::cat_line(" Simulation started at ",
            backtick(private$m_log$start_time), " and completed successfully after ",
            take_time, ".")
    } else {
        cli::cat_line(" Simulation started at ",
            backtick(private$m_log$start_time), " and completed successfully.")
    }

}
# }}}
