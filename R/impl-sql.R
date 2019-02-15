#' @importFrom RSQLite SQLite dbConnect dbDisconnect dbGetQuery dbListTables dbReadTable
#' @importFrom data.table setDT setcolorder setorder
#' @importFrom lubridate year force_tz make_datetime
NULL

# connect_sql {{{
connect_sql <- function (sql) {
    assert(file.exists(sql))
    RSQLite::dbConnect(RSQLite::SQLite(), sql)
}
# }}}

# read_sql_table {{{
read_sql_table <- function (sql, table) {
    db <- connect_sql(sql)
    on.exit(RSQLite::dbDisconnect(db), add = TRUE)
    res <- setDT(RSQLite::dbReadTable(db, table))
    res
}
# }}}

# get_sql_query {{{
get_sql_query <- function (sql, query) {
    db <- connect_sql(sql)
    on.exit(RSQLite::dbDisconnect(db), add = TRUE)
    res <- RSQLite::dbGetQuery(db, query)
    if (is.data.frame(res)) setDT(res)
    res
}
# }}}

# get_sql_report_data_query {{{
get_sql_report_data_query <- function (key_value = NULL, name = NULL) {
    if (is.null(key_value)) {
        if (is.null(name)) {
            where <- "ReportDataDictionary"
        } else {
            assert(is.character(name))
            where <- paste0(
                "
                SELECT *
                FROM ReportDataDictionary
                WHERE Name IN (", paste0("'", unique(name), "'", collapse = ","),")
                "
            )
        }
    } else {
        assert(is.character(key_value))
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

# list_sql_table {{{
list_sql_table <- function (sql) {
    db <- connect_sql(sql)
    on.exit(RSQLite::dbDisconnect(db), add = TRUE)
    RSQLite::dbListTables(db)
}
# }}}

# get_sql_report_data {{{
get_sql_report_data <- function (sql, key_value = NULL, name = NULL, year = NULL,
                             tz = "UTC", case = "auto", all = FALSE) {
    q <- get_sql_report_data_query(key_value, name)
    res <- get_sql_query(sql, q)

    Year <- year %||% lubridate::year(Sys.Date())
    res[, DateTime := lubridate::make_datetime(Year, Month, Day, Hour, Minute, tz = tz)]

    if (!all) {
        res <- res[, .SD, .SDcols = c("DateTime", "KeyValue", "Name", "Units", "Value")]
    } else {
        setcolorder(res, c("DateTime", setdiff(names(res), "DateTime")))
    }

    setorder(res, KeyValue, Name, DateTime)

    if (not_empty(case)) {
        assert(is_scalar(case))
        case_name <- as.character(case)
        res[, Case := case_name]
        setcolorder(res, c("Case", setdiff(names(res), "Case")))
    }

    res
}
# }}}

# get_sql_report_data_dict {{{
get_sql_report_data_dict <- function (sql) {
    read_sql_table(sql, "ReportDataDictionary")
}
# }}}

# get_sql_tabular_data {{{
get_sql_tabular_data <- function (sql) {
    read_sql_table(sql, "TabularDataWithStrings")
}
# }}}
