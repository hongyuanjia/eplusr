#' @importFrom RSQLite SQLite dbConnect dbDisconnect dbGetQuery dbListTables dbReadTable
#' @importFrom data.table setDT setcolorder setorder
#' @importFrom lubridate year force_tz make_datetime
NULL

# with_sql {{{
with_sql <- function (sql, ...) {
    assert(file.exists(sql))
    con <- RSQLite::dbConnect(RSQLite::SQLite(), sql)
    nm <- tempfile()
    base::attach(list(con = con), name = nm, warn.conflicts = FALSE)
    on.exit(
        {
            RSQLite::dbDisconnect(con)
            base::detach(nm, character.only = TRUE)
        },
        add = TRUE
    )
    force(...)
}
# }}}

# read_sql_table {{{
read_sql_table <- function (sql, table) {
    with_sql(sql,
        tidy_sql_name(setDT(RSQLite::dbReadTable(con, table)))[]
    )
}
# }}}

# get_sql_query {{{
get_sql_query <- function (sql, query) {
    with_sql(sql, {
        res <- RSQLite::dbGetQuery(con, query)
        if (is.data.frame(res)) setDT(res)
        res
    })
}
# }}}

# get_sql_report_data_query {{{
get_sql_report_data_query <- function (key_value = NULL, name = NULL,
                                       period = NULL,
                                       month = NULL, day = NULL, hour = NULL, minute = NULL,
                                       interval = NULL, simulation_days = NULL, day_type = NULL,
                                       environment_name = NULL) {
    # helper {{{
    sep <- function (x) {
        if (is.character(x)) {
            paste(paste0("\"", x, "\""), sep = ",", collapse = ",")
        } else {
            paste(x, sep = ",", collapse = ",")
        }
    }
    make <- function (arg, assertion = NULL, sql_col = NULL) {
        a <- substitute(assertion)
        if (is.null(arg)) return(NULL)
        eval(a)
        if (is.null(sql_col)) {
            sql_col <- stri_trans_totitle(deparse(substitute(arg)))
        }
        paste0(sql_col, " IN (", sep(unique(arg)), ")")
    }
    `%and%` <- function (x, y) {
        if (is.null(y)) return(x)
        if (is.null(x)) return(y)
        paste0("(", x, ") AND (", y, ")")
    }
    # }}}
    # Query for Time {{{
    time <- "WarmupFlag == 0" %and%
        make(month, assert(are_count(month), month <= 12L)) %and%
        make(day, assert(are_count(day), day <= 31L)) %and%
        make(hour, assert(are_count(hour, TRUE), hour <= 23L)) %and%
        make(minute, assert(are_count(minute, TRUE), minute <= 59L)) %and%
        make(interval, assert(are_count(interval))) %and%
        make(simulation_days, assert(are_count(simulation_days)), "SimulationDays")

    if (!is.null(day_type)) {
        dt <- match_in_vec(day_type, label = TRUE,
            c("Monday", "Tuesday", "Wednesday", "Thursday",
              "Friday", "Saturday", "Sunday", "Holiday",
              "SummerDesignDay", "WinterDesignDay", "CustomDay1", "CustomDay2"
            )
        )
        assert(!is.na(dt), msg = paste0("Invalid day type found: ", collapse(day_type[is.na(dt)]), "."))
        time <- time %and% make(dt, sql_col = "DayType")
    }

    if (is.null(environment_name)) {
        env <- "EnvironmentPeriods"
    } else {
        assert(is.character(environment_name), no_na(environment_name))
        env <- paste0("
            (
            SELECT *
            FROM EnvironmentPeriods
            WHERE ", make(environment_name, sql_col = "EnvironmentName"), "
            )"
        )
    }

    if (is.null(time)) {
        time <- paste0("
            SELECT TimeIndex, Month, Day, Hour, Minute, Dst, Interval,
                   SimulationDays, DayType, EnvironmentName
            FROM (
                SELECT *
                FROM ", env, " AS e
                INNER JOIN Time AS t
                ON e.EnvironmentPeriodIndex = t.EnvironmentPeriodIndex
            )
            "
        )
    } else {
        if (is.null(period)) {
            time <- paste0("
                SELECT TimeIndex, Month, Day, Hour, Minute, Dst, Interval,
                       SimulationDays, DayType, EnvironmentName
                FROM (
                    SELECT *
                    FROM ", env, " AS e
                    INNER JOIN (
                        SELECT *
                        FROM Time
                        WHERE ", time, "
                    ) AS t
                    ON e.EnvironmentPeriodIndex = t.EnvironmentPeriodIndex
                )
                "
            )
        # make date time string
        } else {
            assert(any(c("Date", "POSIXt") %in% class(period)),
                msg = "`period` should be a Date or DateTime vector."
            )
            p <- unique(period)
            period <- paste(
                lubridate::month(p),
                lubridate::mday(p),
                lubridate::hour(p),
                lubridate::minute(p),
                sep = "-"
            )
            period <- make(period, sql_col = "DateTime")

            dt <- "(Month || \"-\" || Day || \"-\" || Hour || \"-\" || Minute) AS DateTime"
            time <- paste0("
                SELECT TimeIndex, Month, Day, Hour, Minute, Dst, Interval,
                       SimulationDays, DayType, EnvironmentName
                FROM (
                    SELECT *
                    FROM ", env, " AS e
                    INNER JOIN (
                        SELECT *, ", dt, "
                        FROM Time
                        WHERE ", period %and% time, "
                    ) AS t
                    ON e.EnvironmentPeriodIndex = t.EnvironmentPeriodIndex
                )
                "
            )
        }
    }
    # }}}
    # Query for dict {{{
    if (is.data.frame(key_value)) {
        assert(has_name(key_value, c("key_value", "name")))
        name <- key_value$name
        key_value <- key_value$key_value
    }

    key_var <- NULL %and%
        make(key_value, assert(is.character(key_value), no_na(key_value)), "KeyValue") %and%
        make(name, assert(is.character(name), no_na(name)), "Name")

    if (is.null(key_value)) {
        key_var <- "ReportDataDictionary"
    } else {
        key_var <- paste0("
            SELECT *
                FROM ReportDataDictionary
            WHERE ", key_var
        )
    }
    # }}}

    query <- paste0(
        "
        SELECT t.Month AS month,
               t.Day AS day,
               t.Hour AS hour,
               t.Minute AS minute,
               t.Dst AS dst,
               t.Interval AS interval,
               t.SimulationDays AS simulation_days,
               t.DayType AS day_type,
               t.EnvironmentName AS environment_name,
               rdd.IsMeter AS is_meter,
               rdd.Type AS type,
               rdd.indexgroup AS index_group,
               rdd.timesteptype AS timestep_type,
               rdd.KeyValue AS key_value,
               rdd.Name AS name,
               rdd.ReportingFrequency AS reporting_frequency,
               rdd.ScheduleName AS schedule_name,
               rdd.Units As units,
               rd.Value As value
        FROM ReportData AS rd
        INNER JOIN (", key_var, ") As rdd
            ON rd.ReportDataDictionaryIndex = rdd.ReportDataDictionaryIndex
        INNER JOIN (", time, ") As t
            ON rd.TimeIndex = t.TimeIndex
        "
    )

    query
}
# }}}

# list_sql_table {{{
list_sql_table <- function (sql) {
    with_sql(sql, RSQLite::dbListTables(con))
}
# }}}

# get_sql_report_data {{{
get_sql_report_data <- function (sql, key_value = NULL, name = NULL, year = NULL,
                                 tz = "UTC", case = "auto", all = FALSE,
                                 period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                                 interval = NULL, simulation_days = NULL, day_type = NULL,
                                 environment_name = NULL) {
    q <- get_sql_report_data_query(
        key_value, name,
        period, month, day, hour, minute,
        interval, simulation_days, day_type, environment_name
    )
    res <- get_sql_query(sql, q)

    year <- year %||% lubridate::year(Sys.Date())
    set(res, NULL, "datetime",
        lubridate::make_datetime(year, res$month, res$day, res$hour, res$minute, tz = tz)
    )

    if (!all) {
        res <- res[, .SD, .SDcols = c("datetime", "key_value", "name", "units", "value")]
    } else {
        setcolorder(res, c("datetime", setdiff(names(res), "datetime")))
    }

    if (not_empty(case)) {
        assert(is_scalar(case))
        case_name <- as.character(case)
        set(res, NULL, "case", case_name)
        setcolorder(res, c("case", setdiff(names(res), "case")))
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

# tidy_sql_name {{{
tidy_sql_name <- function (x) {
    setnames(x, stri_sub(gsub("([A-Z])", "_\\L\\1", names(x), perl = TRUE), 2L))
}
# }}}
