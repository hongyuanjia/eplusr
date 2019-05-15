#' @importFrom R6 R6Class
#' @include impl-sql.R
NULL

#' Retrieve Simulation Outputs Using EnergyPlus SQLite Output File
#'
#' `EplusSql` class wraps SQL queries that can retrieve simulation outputs using
#' EnergyPlus SQLite output file.
#'
#' SQLite output is an optional output format for EnergyPlus. It will be created
#' if there is an object in class `Output:SQLite`. If the value of field
#' `Option` in class `Output:SQLite` is set to `"SimpleAndTabular"`, then
#' database tables related to the tabular reports will be also included.
#'
#' There are more than 30 tables in the SQLite output file which contains all of
#' the data found in EnergyPlus's tabular output files, standard variable and
#' meter output files, plus a number of reports that are found in the
#' eplusout.eio output file. The full description for SQLite outputs can be
#' found in the EnergyPlus *"Output Details and Examples"* documentation.  Note
#' that all column names of tables returned have been tidied, i.e. `"KeyValue"`
#' becomes `"key_value"`, `"IsMeter"` becomes `"is_meter"` and etc.
#'
#' `EplusSql` class makes it possible to directly retrieve simulation results
#' without creating an [EplusJob] object. [EplusJob] can only get simulation
#' outputs after the job was successfully run before.
#'
#' However, it should be noted that, unlike [EplusJob], there is no checking on
#' whether the simulation is terminated or completed unsuccessfully or, the
#' parent Idf has been changed since last simulation. This means that you may
#' encounter some problems when retrieve data from an unsuccessful simulation.
#' It is suggested to carefully go through the `.err` file using [read_err()] to
#' make sure the output data in the SQLite is correct and reliable.
#'
#' @section Usage:
#' ```
#' epsql <- eplus_sql(sql)
#' epsql$path()
#' epsql$path_idf()
#' epsql$list_table()
#' epsql$read_table(table)
#' epsql$report_data_dict()
#' epsql$report_data(
#'     key_value = NULL, name = NULL, year = NULL, tz = "UTC", case = "auto", all = FALSE,
#'     period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
#'     interval = NULL, simulation_days = NULL, day_type = NULL, environment_name = NULL
#' )
#' job$tabular_data(report_name = NULL, report_for = NULL, table_name = NULL, column_name = NULL, row_name = NULL)
#' epsql$print()
#' print(epsql)
#' ```
#'
#' @section Basic Info:
#' ```
#' epsql <- eplus_sql(sql)
#' epsql$path()
#' epsql$path_idf()
#' ```
#'
#' `$path()` returns the path of EnergyPlus SQLite file.
#'
#' `$path_idf()` returns the IDF file path with same name as the SQLite file in
#'     the same folder. `NULL` is returned if no corresponding IDF is found.
#'
#' **Arguments**:
#'
#' * `epsql`: An `EplusSQL` object.
#' * `sql`: A path to an local EnergyPlus SQLite output file.
#'
#' @section Read Tables:
#' ```
#' epsql$list_table()
#' epsql$read_table(table)
#' epsql$report_data_dict()
#' epsql$report_data(key_value = NULL, name = NULL, year = NULL, tz = "UTC",case = "auto", all = FALSE,
#'                   period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
#'                   interval = NULL, simulation_days = NULL, day_type = NULL, environment_name = NULL)
#' epsql$tabular_data(report_name = NULL, report_for = NULL, table_name = NULL, column_name = NULL, row_name = NULL)
#' ```
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
#' current year backwards) for each run period that compliances with the start
#' day of week restriction.
#'
#' It is worth noting that EnergyPlus uses 24-hour clock system where 24 is only
#' used to denote midnight at the end of a calendar day. In EnergyPlus output,
#' "00:24:00" with a time interval being 15 mins represents a time period from
#' "00:23:45" to "00:24:00", and similarly "00:15:00" represents a time period
#' from "00:24:00" to "00:15:00" of the next day. This means that if current day
#' is Friday, day of week rule applied in schedule time period "00:23:45" to
#' "00:24:00" (presented as "00:24:00" in the output) is also Friday, but not
#' Saturday. However, if you try to get the day of week of time "00:24:00" in R,
#' you will get Saturday, but not Friday. This introduces inconsistency and may
#' cause problems when doing data analysis considering day of week value.
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
#' @section Print:
#' ```
#' epsql$print()
#' print(epsql)
#' ```
#'
#' `$print()` shows the core information of this `EplusSql` object, including
#' the path of the EnergyPlus SQLite file, last modified time of the SQLite file
#' and the path of the IDF file with the same name in the same folder.
#'
#' **Arguments**
#'
#' * `epsql`: An `EplusSQL` object.
#'
#' @docType class
#' @name EplusSql
#' @examples
#' \dontrun{
#' if (is_avail_eplus(8.8)) {
#'     idf_name <- "1ZoneUncontrolled.idf"
#'     epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'
#'     idf_path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", idf_name)
#'     epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData", epw_name)
#'
#'     # copy to tempdir and run the model
#'     idf <- read_idf(idf_path)
#'     idf$run(epw_path, tempdir())
#'
#'     # create from local file
#'     sql <- eplus_sql(file.path(tempdir(), "1ZoneUncontrolled.sql"))
#'
#'     # get sql file path
#'     sql$path()
#'
#'     # get the parent IDF file path
#'     sql$path_idf()
#'
#'     # list all tables in the sql file
#'     sql$list_table()
#'
#'     # read a specific table
#'     sql$read_read("Zones")
#'
#'     # read report data dictionary
#'     sql$report_data_dict()
#'
#'     # read report data
#'     sql$report_data(name = "EnergyTransfer:Building")
#'
#'     # read tabular data
#'     sql$tabular_data()
#' }
#' }
#'
#' @author Hongyuan Jia
NULL

#' Read an Energy SQLite Output File
#'
#' `eplus_sql()` takes an EnergyPlus SQLite output file as input, and returns an
#' `EplusSQL` object for collecting simulation outputs. For more details, please
#' see [EplusSql].
#'
#' @param sql A path to an local EnergyPlus SQLite output file.
#' @return An [EplusSql] object.
#' @export
#' @examples
#' \dontrun{
#' if (is_avail_eplus(8.8)) {
#'     idf_name <- "1ZoneUncontrolled.idf"
#'     epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'
#'     idf_path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", idf_name)
#'     epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData", epw_name)
#'
#'     # copy to tempdir and run the model
#'     idf <- read_idf(idf_path)
#'     idf$run(epw_path, tempdir())
#'
#'     # create from local file
#'     sql <- eplus_sql(file.path(tempdir(), "1ZoneUncontrolled.sql"))
#' }
#' }
#' @author Hongyuan Jia
#' @export
# eplus_sql {{{
eplus_sql <- function (sql) {
    EplusSql$new(sql)
}
# }}}

# EplusSql {{{
EplusSql <- R6::R6Class(classname = "EplusSql", cloneable = FALSE,
    public = list(

        # INITIALIZE {{{
        initialize = function (sql) {
            assert(is_string(sql), has_ext(sql, "sql"))
            private$m_path_sql <- normalizePath(sql, mustWork = TRUE)
            private$m_path_idf <- paste0(tools::file_path_sans_ext(private$m_path_sql), ".idf")
            if (!file.exists(private$m_path_idf)) {
                private$m_path_idf <- file.path(dirname(private$m_path_sql), "in.idf")
                if (!file.exists(private$m_path_idf)) {
                    private$m_path_idf <- NULL
                }
            }
            if (!is.null(private$m_path_idf)) {
                private$m_path_idf <- normalizePath(private$m_path_idf, mustWork = TRUE)
            }
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        path = function ()
            sql_path(self, private),

        path_idf = function ()
            sql_path_idf(self, private),

        list_table = function ()
            sql_list_table(self, private),

        read_table = function (name)
            sql_read_table(self, private, name),

        report_data_dict = function ()
            sql_report_data_dict(self, private),

        report_data = function (key_value = NULL, name = NULL, year = NULL,
                                tz = "UTC", case = "auto", all = FALSE,
                                period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                                interval = NULL, simulation_days = NULL, day_type = NULL,
                                environment_name = NULL)
            sql_report_data(self, private, key_value = key_value, name = name, year = year,
                tz = tz, case = case, all = all,
                period = period, month = month, day = day, hour = hour, minute = minute,
                interval = interval, simulation_days = simulation_days, day_type = day_type,
                environment_name = environment_name
            ),

        tabular_data = function(report_name = NULL, report_for = NULL, table_name = NULL,
                                column_name = NULL, row_name = NULL)
            sql_tabular_data(self, private, report_name = report_name,
                report_for = report_for, table_name = table_name,
                column_name = column_name, row_name = row_name),

        print = function ()
            sql_print(self, private)
        # }}}
    ),

    # PRIVATE FIELDS {{{
    private = list(
        m_path_sql = NULL,
        m_path_idf = NULL
    )
    # }}}
)
# }}}

# sql_path {{{
sql_path <- function (self, private) private$m_path_sql
# }}}
# sql_path_idf {{{
sql_path_idf <- function (self, private) private$m_path_idf
# }}}
# sql_list_table {{{
sql_list_table <- function (self, private) {
    list_sql_table(private$m_path_sql)
}
# }}}
# sql_read_table {{{
sql_read_table <- function (self, private, table) {
    read_sql_table(private$m_path_sql, table)
}
# }}}
# sql_report_data_dict {{{
sql_report_data_dict <- function (self, private) {
    get_sql_report_data_dict(private$m_path_sql)
}
# }}}
# sql_report_data {{{
sql_report_data <- function (self, private, key_value = NULL, name = NULL, year = NULL,
                             tz = "UTC", case = "auto", all = FALSE,
                             period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                             interval = NULL, simulation_days = NULL, day_type = NULL,
                             environment_name = NULL) {
    if (identical(case, "auto")) case <- tools::file_path_sans_ext(basename(private$m_path_sql))
    get_sql_report_data(private$m_path_sql,
        key_value = key_value, name = name, year = year,
        tz = tz, case = case, all = all,
        period = period, month = month, day = day, hour = hour, minute = minute,
        interval = interval, simulation_days = simulation_days, day_type = day_type,
        environment_name = environment_name
    )
}
# }}}
# sql_tabular_data {{{
sql_tabular_data <- function (self, private, report_name = NULL, report_for = NULL,
                              table_name = NULL, column_name = NULL, row_name = NULL) {
    get_sql_tabular_data(private$m_path_sql, report_name = report_name, report_for = report_for,
        table_name = table_name, column_name = column_name, row_name = row_name)
}
# }}}
# sql_print {{{
sql_print <- function (self, private) {
    cli::cat_rule("EnergyPlus SQLite Output")

    path_idf <- paste0(tools::file_path_sans_ext(private$m_path_sql), ".idf")
    if (is.null(private$m_path_idf)) {
        idf <- crayon::bgRed$bold("Not Found")
    } else {
        idf <- surround(str_trunc(path_idf, width = getOption("width") - 16L))
    }

    m_time <- file.info(private$m_path_sql)$mtime

    cli::cat_line(c(
        paste0("* File: ", surround(str_trunc(private$m_path_sql, width = getOption("width") - 10L))),
        paste0("* Last Modified: ", m_time),
        paste0("* Parent Idf: ", idf)
    ))
}
# }}}
# S3 EplusSql methods {{{
#' @export
str.EplusSql <- function (object, ...) {
    object$print()
}

#' @export
format.EplusSql <- function (x, ...) {
    paste0(utils::capture.output(x$print()), collapse = "\n")
}
# }}}
