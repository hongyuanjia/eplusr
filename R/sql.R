#' @importFrom RSQLite SQLite dbConnect dbDisconnect dbGetQuery dbListTables dbReadTable
#' @importFrom data.table setDT setcolorder setorder
#' @importFrom fasttime fastPOSIXct
#' @importFrom lubridate year force_tz
NULL

#' Retrieve Simulation Outputs Using EnergyPlus SQLite Output File
#'
#' `EplusSql` class wraps SQL queries that can retrieve simulation outputs using
#'     EnergyPlus SQLite output file.
#'
#' SQLite output is an optional output format for EnergyPlus. It will be
#'     created if there is an object in class `Output:SQLite`. If the value of
#'     field `Option` in class `Output:SQLite` is set to `"SimpleAndTabular"`,
#'     then database tables related to the tabular reports will be included.
#'
#' There are more than 30 tables in the SQLite output file which contains all of
#'     the data found in EnergyPlus's tabular output files, standard variable
#'     and meter output files, plus a number of reports that are found in the
#'     eplusout.eio output file. The full description for SQLite outputs is
#'     described in the EnergyPlus *"Output Details and Examples"*
#'     documentation.
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
#' epsql$list_table()
#' epsql$read_table(table)
#' epsql$report_data(key_value = NULL, name = NULL, year = NULL, tz = "GMT", case = "auto", all = FALSE)
#' epsql$report_data_dict()
#' epsql$tabular_data()
#' epsql$print()
#' print(epsql)
#' ```
#'
#' `$path()` returns the path of EnergyPlus SQLite file.
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
#' `$print()` shows the core information of this EplusSql object, including the
#'     path of the EnergyPlus SQLite file, last modified time of the SQLite file
#'     and the path of the IDF file with the same name in the same folder.
#'
#' **Arguments**:
#'
#' * `epsql`: An `EplusSQL` object.
#' * `sql`: A path to an local EnergyPlus SQLite output file.
#' * `key_value`: A character vector to identify key name of the data. If
#'    `NULL`, all keys of that variable will be returned. Default: `NULL`.
#' * `name`: A character vector to specify the actual data name. If `NULL`, all
#'    variables will be returned. Default: `NULL`.
#' * `year`: The year of the date and time in column `DateTime`. If `NULL`, it
#'    will be the current year. Default: `NULL`
#' * `tz`: Time zone of date and time in column `DateTime`. Default: `"GMT"`.
#' * `case`: If not `NULL`, a character column will be added indicates the case
#'     of this simulation. If `"auto"`, the name of the SQL file will be used.
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
#'     # list all tables in the sql file
#'     sql$list_table()
#'
#'     # read a specific table
#'     sql$read_table("Zones")
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
EplusSql <- R6::R6Class(classname = "EplusSql",
    public = list(

        # INITIALIZE {{{
        initialize = function (sql) {
            assert_that(is_string(sql))
            assert_that(file.exists(sql))
            assert_that(eplusr:::has_ext(sql, "sql"))
            private$m_path_sql <- normalizePath(sql, mustWork = TRUE)
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        path = function ()
            i_sql_path(self, private),

        list_table = function ()
            i_sql_list_table(self, private),

        read_table = function (name)
            i_sql_read_table(self, private, name),

        report_data_dict = function ()
            i_sql_report_data_dict(self, private),

        report_data = function (key_value = NULL, name = NULL,
                                year = NULL, tz = "GMT", case = "auto")
            i_sql_report_data(self, private, key_value, name, year, tz, case),

        tabular_data = function()
            i_sql_tabular_data(self, private),

        print = function ()
            i_sql_print(self, private)
        # }}}
    ),

    # PRIVATE FIELDS {{{
    private = list(
        m_path_sql = NULL
    )
    # }}}
)
# }}}

# i_is_sql_table_exists {{{
i_is_sql_table_exists <- function (self, private, table) {
    table %in% i_sql_list_table(self, private)
}
# }}}

# i_sql_path {{{
i_sql_path <- function (self, private) private$m_path_sql
# }}}

# i_sql_list_table {{{
i_sql_list_table <- function (self, private) {
    db <- sql_connect(i_sql_path(self, private))
    on.exit(RSQLite::dbDisconnect(db), add = TRUE)
    RSQLite::dbListTables(db)
}
# }}}

# i_sql_read_table {{{
i_sql_read_table <- function (self, private, table) {
    assert_that(is_string(table))
    sql_read_table(i_sql_path(self, private), table)
}
# }}}

# i_sql_report_data_dict {{{
i_sql_report_data_dict <- function (self, private) {
    sql_report_data_dict(i_sql_path(self, private))
}
# }}}

# i_sql_report_data {{{
i_sql_report_data <- function (self, private, key_value = NULL, name = NULL,
                               year = NULL, tz = "GMT", case = "auto", all = FALSE) {
    if (identical(case, "auto")) case <- tools::file_path_sans_ext(basename(private$m_path_sql))
    sql_report_data(i_sql_path(self, private), key_value, name, year, tz, case, all)
}
# }}}

# i_sql_tabular_data {{{
i_sql_tabular_data <- function (self, private) {
    sql_tabular_data(i_sql_path(self, private))
}
# }}}

# i_sql_print {{{
i_sql_print <- function (self, private) {
    cli::cat_rule(crayon::bold("EnergyPlus SQLite Output"), col = "green")

    path_idf <- paste0(tools::file_path_sans_ext(private$m_path_sql), ".idf")
    if (!file.exists(path_idf)) {
        idf <- crayon::bgRed$bold("Not Found")
    } else {
        idf <- backtick(path_idf)
    }

    m_time <- file.info(private$m_path_sql)$mtime

    cli::cat_bullet(c(
        paste0(crayon::bold("File"), ": ", backtick(private$m_path_sql)),
        paste0(crayon::bold("Last Modified"), ": ", m_time),
        paste0(crayon::bold("Parent Idf"), ": ", idf)
    ), col = "cyan", bullet_col = "cyan")
}
# }}}

# sql_connect {{{
sql_connect <- function (sql) {
    stopifnot(file.exists(sql))
    RSQLite::dbConnect(RSQLite::SQLite(), sql)
}
# }}}

# sql_read_table {{{
sql_read_table <- function (sql, table) {
    db <- sql_connect(sql)
    on.exit(RSQLite::dbDisconnect(db), add = TRUE)
    res <- data.table::setDT(RSQLite::dbReadTable(db, table))
    res
}
# }}}

# sql_get_query {{{
sql_get_query <- function (sql, query) {
    db <- sql_connect(sql)
    on.exit(RSQLite::dbDisconnect(db), add = TRUE)
    res <- RSQLite::dbGetQuery(db, query)
    if (is.data.frame(res)) data.table::setDT(res)
    res
}
# }}}

# sql_report_data_query {{{
sql_report_data_query <- function (key_value = NULL, name = NULL) {
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

# sql_all_table {{{
sql_all_table <- function (sql) {
    db <- sql_connect(sql)
    on.exit(RSQLite::dbDisconnect(db), add = TRUE)
    RSQLite::dbListTables(db)
}
# }}}

# sql_report_data {{{
sql_report_data <- function (sql, key_value = NULL, name = NULL, year = NULL,
                             tz = "GMT", case = "auto", all = FALSE) {
    q <- sql_report_data_query(key_value, name)
    res <- sql_get_query(sql, q)

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

    if (not_empty(case)) {
        assert_that(is_scalar(case))
        case_name <- as.character(case)
        res[, Case := case_name]
        data.table::setcolorder(res, c("Case", setdiff(names(res), "Case")))
    }

    res[]
}
# }}}

# sql_report_data_dict {{{
sql_report_data_dict <- function (sql) {
    sql_read_table(sql, "ReportDataDictionary")
}
# }}}

# sql_tabular_data {{{
sql_tabular_data <- function (sql) {
    sql_read_table(sql, "TabularDataWithStrings")
}
# }}}
