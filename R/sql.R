#' @importFrom RSQLite SQLite dbConnect dbDisconnect dbGetQuery dbListTables dbReadTable
#' @importFrom data.table setDT setcolorder setorder
#' @importFrom fasttime fastPOSIXct
#' @importFrom lubridate year force_tz
NULL

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
