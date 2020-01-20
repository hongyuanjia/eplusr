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
#' @docType class
#' @name EplusSql
#' @author Hongyuan Jia
NULL

#' @export
# EplusSql {{{
EplusSql <- R6::R6Class(classname = "EplusSql", cloneable = FALSE,
    public = list(

        # INITIALIZE {{{
        #' @description
        #' Create an `EplusSql` object
        #'
        #' @param sql A path to an local EnergyPlus SQLite output file.
        #' @return An `EplusSql` object.
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
        #'     # copy to tempdir and run the model
        #'     idf <- read_idf(idf_path)
        #'     idf$run(epw_path, tempdir(), echo = FALSE)
        #'
        #'     # create from local file
        #'     sql <- eplus_sql(file.path(tempdir(), "1ZoneUncontrolled.sql"))
        #' }
        #' }
        #'
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
        # path {{{
        #' @description
        #' Get the file path of current `EpwSql` object
        #'
        #' @details
        #' `$path()` returns the path of EnergyPlus SQLite file.
        #'
        #' @return A single string.
        #'
        #' @examples
        #' \dontrun{
        #' # get path
        #' sql$path()
        #' }
        #'
        path = function ()
            sql_path(self, private),
        # }}}

        # path_idf {{{
        #' @description
        #' Get the path of corresponding IDF file
        #'
        #' @details
        #' `$path_idf()` returns the IDF file path with same name as the SQLite
        #' file in the same folder. `NULL` is returned if no corresponding IDF
        #' is found.
        #'
        #' @return NULL or a single string.
        #'
        #' @examples
        #' \dontrun{
        #' # get path
        #' sql$path_idf()
        #' }
        #'
        path_idf = function ()
            sql_path_idf(self, private),
        # }}}

        # list_table {{{
        #' @description
        #' List all table names in current EnergyPlus SQL output
        #'
        #' @details
        #' `$list_table()` returns all available table and view names in the
        #' EnergyPlus SQLite file.
        #'
        #' @return A character vector
        #'
        #' @examples
        #' \dontrun{
        #' sql$list_table()
        #' }
        #'
        list_table = function ()
            sql_list_table(self, private),
        # }}}

        # read_table {{{
        #' @description
        #' Read a single table from current EnergyPlus SQL output
        #'
        #' @details
        #' `$read_table()` takes a valid table `name` of those from
        #' \href{../../eplusr/html/EplusSql.html#method-list_table}{\code{$list_table()}}
        #' and returns that table data in a [data.table::data.table()] format.
        #'
        #' @param name A single string specifying the name of table to read.
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' # read a specific table
        #' sql$read_table("Zones")
        #' }
        #'
        read_table = function (name)
            sql_read_table(self, private, name),
        # }}}

        # report_data_dict {{{
        #' @description
        #' Read report data dictionary from current EnergyPlus SQL output
        #'
        #' @details
        #' `$report_data_dict()` returns a [data.table::data.table()] which
        #' contains all information about report data.
        #'
        #' For details on the meaning of each columns, please see "2.20.2.1
        #' ReportDataDictionary Table" in EnergyPlus "Output Details and
        #' Examples" documentation.
        #'
        #' @return A [data.table::data.table()] of 10 columns:
        #'
        #' * `report_data_dictionary_index`: The integer used to link the
        #'   dictionary data to the variable data. Mainly useful when joining
        #'   different tables
        #' * `is_meter`: Whether report data is a meter data. Possible values:
        #'   `0` and `1`
        #' * `timestep_type`: Type of data timestep. Possible values: `Zone` and
        #'   `HVAC System`
        #' * `key_value`: Key name of the data
        #' * `name`: Actual report data name
        #' * `reporting_frequency`:
        #' * `schedule_name`: Name of the the schedule that controls reporting
        #'     frequency.
        #' * `units`: The data units
        #'
        #' @examples
        #' \dontrun{
        #' sql$report_data_dict()
        #' }
        #'
        report_data_dict = function ()
            sql_report_data_dict(self, private),
        # }}}

        # report_data {{{
        #' @description
        #' Read report data
        #'
        #' @details
        #' `$report_data()` extracts the report data in a
        #' [data.table::data.table()] using key values, variable names and other
        #' specifications.
        #'
        #' `$report_data()` can also directly take all or subset output from
        #' `$report_data_dict()` as input, and extract all data specified.
        #'
        #' The returned column numbers varies depending on `all` argument.
        #'
        #' * `all` is `FALSE`, the returned [data.table::data.table()] has 6 columns:
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
        #'   * `environment_period_index`: The indices of environment.
        #'   * `environment_name`: A text string identifying the environment.
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
        #' With `wide` equals `TRUE`, `$report_data()` will format the simulation output
        #' in the same way as standard EnergyPlus csv output file. Sometimes this can be
        #' useful as there may be existing tools/workflows that depend on this format.
        #' When both `wide` and `all` are `TRUE`, columns of runperiod environment names
        #' and date time components are also returned, including:
        #' `environment_period_index", "environment_name`, `simulation_days`,
        #' `datetime`, `month`, `day`, `hour`, `minute`, `day_type`.
        #'
        #' For convenience, input character arguments matching in
        #' `$report_data()` are **case-insensitive**.
        #'
        #' @param key_value A character vector to identify key values of the
        #'        data. If `NULL`, all keys of that variable will be returned.
        #'        `key_value` can also be data.frame that contains `key_value`
        #'        and `name` columns. In this case, `name` argument in
        #'        `$report_data()` is ignored. All available `key_value` for
        #'        current simulation output can be obtained using
        #'        \href{../../eplusr/html/EplusSql.html#method-report_data_dict}{\code{$report_data_dict()}}.
        #'        Default: `NULL`.
        #'
        #' @param name A character vector to identify names of the data. If
        #'        `NULL`, all names of that variable will be returned. If
        #'        `key_value` is a data.frame, `name` is ignored. All available
        #'        `name` for current simulation output can be obtained using
        #'        \href{../../eplusr/html/EplusSql.html#method-report_data_dict}{\code{$report_data_dict()}}.
        #'        Default: `NULL`.
        #'
        #' @param year Year of the date time in column `datetime`. If `NULL`, it
        #'        will calculate a year value that meets the start day of week
        #'        restriction for each environment. Default: `NULL`.
        #'
        #' @param tz Time zone of date time in column `datetime`. Default:
        #'        `"UTC"`.
        #'
        #' @param case A single string used to add a character column `case` in
        #'        the returned results to indicate the case of this simulation.
        #'        If `NULL`, no column is added. If `"auto"`, the name of the
        #'        IDF file without extension is used. Default: `"auto"`.
        #'
        #' @param all If `TRUE`, extra columns are also included in the returned
        #'        [data.table::data.table()].
        #'
        #' @param wide If `TRUE`, the output is formatted in the same way as
        #'        standard EnergyPlus csv output file.
        #'
        #' @param period A Date or POSIXt vector used to specify which time
        #'        period to return. The year value does not matter and only
        #'        month, day, hour and minute value will be used when
        #'        subsetting. If `NULL`, all time period of data is returned.
        #'        Default: `NULL`.
        #'
        #' @param month,day,hour,minute Each is an integer vector for month,
        #'        day, hour, minute subsetting of `datetime` column when
        #'        querying on the SQL database. If `NULL`, no subsetting is
        #'        performed on those components. All possible `month`, `day`,
        #'        `hour` and `minute` can be obtained using
        #'        \href{../../eplusr/html/EplusSql.html#method-read_table}{\code{$read_table("Time")}}.
        #'        Default: `NULL`.
        #'
        #' @param interval An integer vector used to specify which interval
        #'        length of report to extract. If `NULL`, all interval will be
        #'        used. Default: `NULL`.
        #'
        #' @param simulation_days An integer vector to specify which simulation
        #'        day data to extract. Note that this number resets after warmup
        #'        and at the beginning of an environment period. All possible
        #'        `simulation_days` can be obtained using
        #'        \href{../../eplusr/html/EplusSql.html#method-read_table}{\code{$read_table("Time")}}.
        #'        If `NULL`, all simulation days will be used. Default: `NULL`.
        #'
        #' @param day_type A character vector to specify which day type of data
        #'        to extract. All possible day types are: `Sunday`, `Monday`,
        #'        `Tuesday`, `Wednesday`, `Thursday`, `Friday`, `Saturday`,
        #'        `Holiday`, `SummerDesignDay`, `WinterDesignDay`, `CustomDay1`,
        #'        and `CustomDay2`. All possible values for current simulation
        #'        output can be obtained using
        #'        \href{../../eplusr/html/EplusSql.html#method-read_table}{\code{$read_table("Time")}}.
        #'
        #' @param environment_name A character vector to specify which
        #'        environment data to extract. If `NULL`, all environment data
        #'        are returned. Default: `NULL`. All possible
        #'        `environment_name` for current simulation output can be
        #'        obtained using:
        #' ```
        #' $read_table("EnvironmentPeriods")
        #' ```
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' # read all report data
        #' sql$report_data()
        #'
        #' # specify output variables using report data dictionary
        #' dict <- sql$report_data_dict()
        #' sql$report_data(dict[units == "C"])
        #'
        #' # specify output variables using 'key_value' and 'name'
        #' sql$report_data("environment", "site outdoor air drybulb temperature")
        #'
        #' # explicitly specify year value and time zone
        #' sql$report_data(dict[1], year = 2020, tz = "Etc/GMT+8")
        #'
        #' # explicitly specify case name
        #' sql$report_data(dict[1], case = "example")
        #'
        #' # get all possible columns
        #' sql$report_data(dict[1], all = TRUE)
        #'
        #' # return in a format that is similar as EnergyPlus CSV output
        #' sql$report_data(dict[1], wide = TRUE)
        #'
        #' # return in a format that is similar as EnergyPlus CSV output with
        #' # extra columns
        #' sql$report_data(dict[1], wide = TRUE, all = TRUE)
        #'
        #' # only get data at the working hour on the first Monday
        #' sql$report_data(dict[1], hour = 8:18, day_type = "monday", simulation_days = 1:7)
        #'
        #' # only get specified run period data
        #' sql$read_table("EnvironmentPeriods") # possible environment name
        #' sql$report_data(dict[1], environment_name = "San Francisco Intl Ap CA USA TMY3 WMO#=724940")
        #' # can also be done using 'environment_period_index' column
        #' sql$report_data(dict[1], all = TRUE)[environment_period_index == 3L]
        #' }
        #'
        report_data = function (key_value = NULL, name = NULL, year = NULL,
                                tz = "UTC", case = "auto", all = FALSE, wide = FALSE,
                                period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                                interval = NULL, simulation_days = NULL, day_type = NULL,
                                environment_name = NULL)
            sql_report_data(self, private, key_value = key_value, name = name, year = year,
                tz = tz, case = case, all = all, wide = wide,
                period = period, month = month, day = day, hour = hour, minute = minute,
                interval = interval, simulation_days = simulation_days, day_type = day_type,
                environment_name = environment_name
            ),
        # }}}

        # tabular_data {{{
        #' @description
        #' Read tabular data
        #'
        #' @details
        #' `$tabular_data()` extracts the tabular data in a
        #' [data.table::data.table()] using report, table, column and row name
        #' specifications. The returned [data.table::data.table()] has
        #' 9 columns:
        #'
        #' * `case`: Simulation case specified using `case` argument
        #' * `index`: Tabular data index
        #' * `report_name`: The name of the report that the record belongs to
        #' * `report_for`: The `For` text that is associated with the record
        #' * `table_name`: The name of the table that the record belongs to
        #' * `column_name`: The name of the column that the record belongs to
        #' * `row_name`: The name of the row that the record belongs to
        #' * `units`: The units of the record
        #' * `value`: The value of the record **in string format** by default.
        #'
        #' For convenience, input character arguments matching in
        #' `$tabular_data()` are **case-insensitive**.
        #'
        #' @param report_name,report_for,table_name,column_name,row_name Each is
        #'        a character vector for subsetting when querying the SQL
        #'        database.  For the meaning of each argument, please see the
        #'        description above.
        #' @param case A single string used to add a character column `case` in
        #'        the returned results to indicate the case of this simulation.
        #'        If `NULL`, no column is added. If `"auto"`, the name of the
        #'        IDF file without extension is used. Default: `"auto"`.
        #' @param wide If `TRUE`, each table will be converted into the similar
        #'        format as it is shown in EnergyPlus HTML output file. Default:
        #'        `FALSE`.
        #' @param string_value Only applicable when `wide` is `TRUE`. If
        #'        `string_value` is `FALSE`, instead of keeping all values as
        #'        characters, values in possible numeric columns are converted
        #'        into numbers. Default: the opposite of `wide`. Possible
        #'        numeric columns indicate column that:
        #' * columns that have associated units
        #' * columns that contents numbers
        #'
        #' @return A [data.table::data.table()] with 9 columns (when `wide` is
        #' `FALSE`) or a named list of [data.table::data.table()]s where the
        #' names are the combination of `report_name`, `report_for` and
        #' `table_name`.
        #'
        #' @examples
        #' \dontrun{
        #' # read all tabular data
        #' sql$tabular_data()
        #'
        #' # explicitly specify data you want
        #' str(sql$tabular_data(
        #'     report_name = "AnnualBuildingUtilityPerformanceSummary",
        #'     table_name = "Site and Source Energy",
        #'     column_name = "Total Energy",
        #'     row_name = "Total Site Energy"
        #' ))
        #'
        #' # get tabular data in wide format and coerce numeric values
        #' str(sql$tabular_data(
        #'     report_name = "AnnualBuildingUtilityPerformanceSummary",
        #'     table_name = "Site and Source Energy",
        #'     column_name = "Total Energy",
        #'     row_name = "Total Site Energy",
        #'     wide = TRUE, string_value = FALSE
        #' ))
        #' }
        #'
        tabular_data = function(report_name = NULL, report_for = NULL, table_name = NULL,
                                column_name = NULL, row_name = NULL,
                                case = "auto", wide = FALSE, string_value = !wide)
            sql_tabular_data(self, private, report_name = report_name,
                report_for = report_for, table_name = table_name,
                column_name = column_name, row_name = row_name,
                case = case, wide = wide, string_value = string_value),
        # }}}

        # print {{{
        #' @description
        #' Print `EplusSql` object
        #'
        #' @details
        #' `$print()` shows the core information of this `EplusSql` object,
        #' including the path of the EnergyPlus SQLite file, last modified
        #' time of the SQLite file and the path of the IDF file with the
        #' same name in the same folder.
        #'
        #' @return The `EplusSql` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' sql$print()
        #' }
        #'
        print = function ()
            sql_print(self, private)
        # }}}
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
#'     idf$run(epw_path, tempdir(), echo = FALSE)
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
                             tz = "UTC", case = "auto", all = FALSE, wide = FALSE,
                             period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                             interval = NULL, simulation_days = NULL, day_type = NULL,
                             environment_name = NULL) {
    if (identical(case, "auto")) case <- tools::file_path_sans_ext(basename(private$m_path_sql))
    get_sql_report_data(private$m_path_sql,
        key_value = key_value, name = name, year = year,
        tz = tz, case = case, all = all, wide = wide,
        period = period, month = month, day = day, hour = hour, minute = minute,
        interval = interval, simulation_days = simulation_days, day_type = day_type,
        environment_name = environment_name
    )
}
# }}}
# sql_tabular_data {{{
sql_tabular_data <- function (self, private, report_name = NULL, report_for = NULL,
                              table_name = NULL, column_name = NULL, row_name = NULL,
                              case = "auto", wide = FALSE, string_value = !wide) {
    if (identical(case, "auto")) case <- tools::file_path_sans_ext(basename(private$m_path_sql))
    get_sql_tabular_data(private$m_path_sql, report_name = report_name, report_for = report_for,
        table_name = table_name, column_name = column_name, row_name = row_name,
        case = case, wide = wide, string_value = string_value)
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
