#' @importFrom R6 R6Class
#' @include impl-sql.R
NULL

#' Retrieve Simulation Outputs Using EnergyPlus SQLite Output File
#'
#' `EplusSql` class wraps SQL queries that can retrieve simulation outputs using
#'     EnergyPlus SQLite output file.
#'
#' SQLite output is an optional output format for EnergyPlus. It will be
#'     created if there is an object in class `Output:SQLite`. If the value of
#'     field `Option` in class `Output:SQLite` is set to `"SimpleAndTabular"`,
#'     then database tables related to the tabular reports will be also included.
#'
#' There are more than 30 tables in the SQLite output file which contains all of
#'     the data found in EnergyPlus's tabular output files, standard variable
#'     and meter output files, plus a number of reports that are found in the
#'     eplusout.eio output file. The full description for SQLite outputs can be
#'     found in the EnergyPlus *"Output Details and Examples"* documentation.
#'     Note that all column names of tables returned have been tidied, i.e.
#'     `"KeyValue"` becomes `"key_value"`, `"IsMeter"` becomes `"is_meter"` and
#'     etc.
#'
#' `EplusSql` class makes it possible to directly retrieve simulation results
#'     without creating an [EplusJob] object which can only get simulation
#'     outputs after the job was successfully run before.
#'
#' However, it should be noted that, unlike [EplusJob], there is no checking on
#'     whether the simulation is terminated or completed unsuccessfully or, the
#'     parent Idf has been changed since last simulation. This means that you
#'     may encounter some problems when retrieve data from an unsuccessful
#'     simulation. It is suggested to carefully go through the `.err` file to
#'     make sure the output data in the SQLite is correct and reliable.
#'
#' @section Usage:
#' ```
#' epsql <- eplus_sql(sql)
#' epsql$path()
#' epsql$path_idf()
#' epsql$list()
#' epsql$read(table)
#' epsql$data(
#'     key_value = NULL, name = NULL, year = NULL, tz = "UTC", case = "auto", all = FALSE,
#'     period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
#'     interval = NULL, simulation_days = NULL, day_type = NULL, environment_name = NULL
#' )
#' epsql$data_dict()
#' epsql$tabular()
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
#' epsql$list()
#' epsql$read(table)
#' ```
#'
#' `$list()` returns all available table and view names in current SQLite file.
#'
#' `$read()` takes a valid `table` and returns that table data in a
#'     [data.table::data.table] form.
#'
#' **Arguments**
#'
#' * `epsql`: An `EplusSQL` object.
#' * `table`: A valid name of table to extract data from.
#'
#' @section Output Data:
#' ```
#' epsql$data_dict()
#' epsql$data(
#'     key_value = NULL, name = NULL, year = NULL, tz = "UTC", case = "auto", all = FALSE,
#'     period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
#'     interval = NULL, simulation_days = NULL, day_type = NULL, environment_name = NULL
#' )
#' epsql$tabular()
#' ```
#'
#' `$data_dict()` returns a [data.table::data.table] which contains all
#'     information about report data (in `Output:Variable` and `Output:Meter*`
#'     class). For details on the meaning of each columns, please see "2.20.2.1
#'     ReportDataDictionary Table" in EnergyPlus "Output Details and Examples"
#'     documentation.
#'
#' `$data()` returns a [data.table::data.table] with report data specified using
#'     key values and variable names and other specifiations. By default (when
#'     `all` is FALSE), the returned [data.table::data.table] contains 6
#'     columns:
#'
#'         * `case`: A character vector describes the simulation case.
#'         * `datetime`: A POSIXct vector contains datetime of each data. Year
#'           of the time can be changed using `year` parameter.
#'         * `key_value`: A character vector contains key names of the data.
#'         * `name`: A character vector contains the actual report data name.
#'         * `units`: A character vector contains the data unit.
#'         * `value`: A numeric vector contains the acutal data.
#'
#'    The returned data can also be controlled using other parameters. See
#'    **Arguments**.
#'
#' `$tabular()` returns a [data.table::data.table] with all tabular data. Please
#'     note that the `value` column in the returned [data.table::data.table] is
#'     a character vector not numeric. Further cleaning work may needed.
#'
#' **Arguments**:
#'
#' * `epsql`: An `EplusSQL` object.
#' * `key_value`: A character vector of key names or a data.frame containing at
#'    least columns, `key_value` and `name`. This makes it possible to directly
#'    pass a subset of report data dictionary to extract data. See example. If
#'    `NULL`, all keys of that variable will be returned. Default: `NULL`.
#' * `name`: A character vector of variable name. If `NULL`, all variables will
#'    be returned. Default: `NULL`.
#' * `year`: The year of the date and time in column `datetime`. If `NULL`,
#'    current year will be used. Default: `NULL`
#' * `tz`: Time zone of date and time in column `datetime`. Default: `"UTC"`.
#' * `case`: If not `NULL`, a character column will be added indicating the case
#'    of this simulation. If `"auto"`, the name of the SQL file will be used.
#' * `all`: Default: `FALSE`. If `TRUE`, extra columns are returned as well,
#'   including:
#'
#'        * `dst`: daylight saving time indicator
#'        * `simulation_days` :day of simulation
#'        * `day_type`: type of day
#'        * `environment_name`: environment of simulation, including runperiod
#'        *  names
#'        * `is_meter`: boolean flag whether data is meter data or not
#'        * `type`: Nature of data type with respect to state, e.g. `"Sum"`, adn
#'          `"Avg"`
#'        * `index_group`: reporting group, e.g. `"Zone"`, `"Plant"`, etc.
#'        * `timestep_type`: type of timestep for data, e.g. `"Zone"`, `"HVAC
#'          system"`
#'        * `schedule_name`: name of the schedule that controls reporting
#'          frequency
#'
#' * `period`: A Date or POSIXt vector used to specify which time period to
#'    return. The year value does not matter and only month, day, hour and
#'    minute value will be used when subsetting. If `NULL`, all time period of
#'    data is returned. Default: `NULL`.
#' * `month`, `day`, `hour`, `minute`: Each is an integer vector for month, day,
#'    hour, minute subsetting of `datetime` column. If `NULL`, no subsetting
#'    is performed on those components. Default: `NULL`.
#' * `interval`: An integer vector used to specify which interval length of
#'    report to extract. If `NULL`, all interval will be used. Default: `NULL`.
#' * `simulation_days`: An integer vector to specify which simulation day data
#'    to extract. Note that this number resets after warmup and at the beginning
#'    of an environment period. If `NULL`, all simulation days will be used.
#'    Default: `NULL`.
#' * `day_type`: A character vector to specify which day type of data to
#'    extract. All possible day types are: `Sunday`, `Monday`, `Tuesday`,
#'   `Wednesday`, `Thursday`, `Friday`, `Saturday`, `Holiday`,
#'   `SummerDesignDay`, `WinterDesignDay`, `CustomDay1`, and `CustomDay2`.
#' * `environment_name`: A character vector to specify which environment data to
#'    extract. If `NULL`, all environment data are returned. Default: `NULL`.
#'
#' @section Print:
#' ```
#' epsql$print()
#' print(epsql)
#' ```
#'
#' `$print()` shows the core information of this `EplusSql` object, including the
#'     path of the EnergyPlus SQLite file, last modified time of the SQLite file
#'     and the path of the IDF file with the same name in the same folder.
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
#'     sql$list()
#'
#'     # read a specific table
#'     sql$read("Zones")
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
#'     `EplusSQL` object for collecting simulation outputs. For more details,
#'     please see [EplusSql].
#'
#' @param sql A path to an local EnergyPlus SQLite output file.
#' @return An `EplusSql` object.
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
