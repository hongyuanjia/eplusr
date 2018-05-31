#' @importFrom R6 R6Class
#' @importFrom RSQLite SQLite dbConnect dbReadTable dbListTables
#' @importFrom data.table setDT
Sql <- R6::R6Class(classname = "EpSql", cloneable = FALSE, lock_class = TRUE,
    public = list(
        # INITIALIZE {{{
        initialize = function (path) {
            private$m_path <- path
            # private$m_dict <- self$report_data_dict()
        },
        # }}}

        all_tables = function () {
            # list all available tables
            # {{{
            private$connect()
            res <- RSQLite::dbListTables(private$m_sql)
            private$disconnect()
            res
            # }}}
        },

        report_data_dict = function () {
            # return report data dict
            # {{{
            private$read_sql_table("ReportDataDictionary")
            # }}}
        },

        tabular_data = function () {
            # returan tabular data
            # {{{
            private$read_sql_table("TabularDataWithStrings")
            # }}}
        },

        report_data = function (key_value = NULL, name = NULL) {
            # read report data
            # {{{
            q <- private$vars_query(key_value = key_value, name = name)
            private$get_sql_query(q)
            # }}}
        },

        read_table = function (name) {
            # read a table
            private$read_sql_table(name)
        }
    ),

    private = list(
        m_path = NULL,
        m_sql = NULL,
        m_dict = NULL,

        connect = function () {
            private$m_sql <- RSQLite::dbConnect(RSQLite::SQLite(), private$m_path)
        },

        disconnect = function () {
            private$m_sql <- RSQLite::dbDisconnect(private$m_sql)
        },

        read_sql_table = function (table) {
            private$connect()
            res <- data.table::setDT(RSQLite::dbReadTable(private$m_sql, table))
            private$disconnect()
            res
        },

        get_sql_query = function (query) {
            private$connect()
            res <- RSQLite::dbGetQuery(private$m_sql, query)
            private$disconnect()
            if (is.data.frame(res)) {
                data.table::setDT(res)
            }
            res
        },

        vars_query = function (key_value = NULL, name = NULL) {
            # generate sql for query report data
            # {{{
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
            # }}}
        }
    )
)

to_time = function (dt, year = NULL) {
    # create date time from column `Month`, `Day`, `Hour`, and `Minute`
    # {{{
    dt <- data.table::copy(dt)
    assert_that(has_names(dt, c("Month", "Day", "Hour", "Minute", "DayType")))
    if (is.null(year)) {
        # get the year from date and `DayType`
        f <- dt[Hour != 24L][1L]
        check_date <- f[, paste0(Month, "-", Day)]
        year <- get_epw_year(year = lubridate::year(Sys.Date()),
                             week_day = f[["DayType"]],
                             check_date = check_date)
    }

    dt[, DateTime := paste0(year, "-", Month, "-", Day, " ", Hour, ":", Minute, ":00")][
       , DateTime := (fasttime::fastPOSIXct(DateTime, tz = "GMT"))][
       , c("Month", "Day", "Hour", "Minute") := NULL]

    data.table::setcolorder(dt, c("DateTime", setdiff(names(dt), "DateTime")))

    dt[]
    # }}}
}
