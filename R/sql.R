#' @importFrom R6 R6Class
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
            assert(is_string(sql), has_ext(sql, "sql"))
            private$m_path_sql <- normalizePath(sql, mustWork = TRUE)
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        path = function ()
            sql_path(self, private),

        list_table = function ()
            sql_list_table(self, private),

        read_table = function (name)
            sql_read_table(self, private, name),

        report_data_dict = function ()
            sql_report_data_dict(self, private),

        report_data = function (key_value = NULL, name = NULL,
                                year = NULL, tz = "GMT", case = "auto")
            sql_report_data(self, private, key_value, name, year, tz, case),

        tabular_data = function()
            sql_tabular_data(self, private),

        print = function ()
            sql_print(self, private)
        # }}}
    ),

    # PRIVATE FIELDS {{{
    private = list(
        m_path_sql = NULL
    )
    # }}}
)
# }}}
# sql_is_table_exists {{{
sql_is_table_exists <- function (self, private, table) {
    table %in% sql_list_table(self, private)
}
# }}}
# sql_path {{{
sql_path <- function (self, private) private$m_path_sql
# }}}
# sql_list_table {{{
sql_list_table <- function (self, private) {
    db <- connect_sql(sql_path(self, private))
    on.exit(RSQLite::dbDisconnect(db), add = TRUE)
    RSQLite::dbListTables(db)
}
# }}}
# sql_read_table {{{
sql_read_table <- function (self, private, table) {
    assert(is_string(table))
    read_sql_table(sql_path(self, private), table)
}
# }}}
# sql_report_data_dict {{{
sql_report_data_dict <- function (self, private) {
    get_sql_report_data_dict(sql_path(self, private))
}
# }}}
# sql_report_data {{{
sql_report_data <- function (self, private, key_value = NULL, name = NULL,
                               year = NULL, tz = "GMT", case = "auto", all = FALSE) {
    if (identical(case, "auto")) case <- tools::file_path_sans_ext(basename(private$m_path_sql))
    get_sql_report_data(sql_path(self, private), key_value, name, year, tz, case, all)
}
# }}}
# sql_tabular_data {{{
sql_tabular_data <- function (self, private) {
    get_sql_tabular_data(sql_path(self, private))
}
# }}}
# sql_print {{{
sql_print <- function (self, private) {
    cli::cat_rule(crayon::bold("EnergyPlus SQLite Output"), col = "green")

    path_idf <- paste0(tools::file_path_sans_ext(private$m_path_sql), ".idf")
    if (!file.exists(path_idf)) {
        idf <- crayon::bgRed$bold("Not Found")
    } else {
        idf <- surround(path_idf)
    }

    m_time <- file.info(private$m_path_sql)$mtime

    cli::cat_bullet(c(
        paste0(crayon::bold("File"), ": ", surround(private$m_path_sql)),
        paste0(crayon::bold("Last Modified"), ": ", m_time),
        paste0(crayon::bold("Parent Idf"), ": ", idf)
    ), col = "cyan", bullet_col = "cyan")
}
# }}}
