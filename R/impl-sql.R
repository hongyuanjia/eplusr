#' @importFrom RSQLite SQLite dbConnect dbDisconnect dbGetQuery dbListTables dbReadTable
#' @importFrom data.table setDT setcolorder setorder
#' @importFrom lubridate year force_tz make_datetime
NULL

# conn_sql {{{
conn_sql <- function (sql) {
    RSQLite::dbConnect(RSQLite::SQLite(), sql)
}
# }}}
# with_sql {{{
with_sql <- function (sql, expr) {
    on.exit(RSQLite::dbDisconnect(sql), add = TRUE)
    force(expr)
}
# }}}
# read_sql_table {{{
read_sql_table <- function (sql, table) {
    con <- conn_sql(sql)
    with_sql(con, tidy_sql_name(setDT(RSQLite::dbReadTable(con, table)))[])
}
# }}}
# get_sql_query {{{
get_sql_query <- function (sql, query) {
    con <- conn_sql(sql)
    with_sql(con, {
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
    sep <- function (x, ignore_case = TRUE) {
        if (is.character(x)) {
            if (ignore_case) {
                stri_trans_tolower(paste(paste0("\"", x, "\""), sep = ",", collapse = ","))
            } else {
                paste(paste0("\"", x, "\""), sep = ",", collapse = ",")
            }
        } else {
            paste(x, sep = ",", collapse = ",")
        }
    }
    make <- function (arg, assertion = NULL, sql_col = NULL, ignore_case = TRUE) {
        a <- substitute(assertion)
        if (is.null(arg)) return(NULL)
        eval(a)
        if (is.null(sql_col)) {
            sql_col <- stri_trans_totitle(deparse(substitute(arg)))
        }
        if (is.character(arg) && ignore_case) {
            paste0("lower(", sql_col, ") IN (", sep(unique(arg), TRUE), ")")
        } else {
            paste0(sql_col, " IN (", sep(unique(arg), FALSE), ")")
        }
    }
    `%and%` <- function (x, y) {
        if (is.null(y)) return(x)
        if (is.null(x)) return(y)
        paste0("(", x, ") AND (", y, ")")
    }
    # }}}
    # Query for Time {{{
    time <- NULL %and%
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

    if (is.null(time) && is.null(period)) {
        time <- paste0("
            SELECT TimeIndex, Month, Day, Hour, Minute, Dst, Interval,
                   SimulationDays, DayType, EnvironmentName, EnvironmentPeriodIndex
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
                       SimulationDays, DayType, EnvironmentName, EnvironmentPeriodIndex
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
                       SimulationDays, DayType, EnvironmentName, EnvironmentPeriodIndex
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
               t.EnvironmentPeriodIndex AS environment_period_index,
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
# get_sql_tabular_data_query {{{
get_sql_tabular_data_query <- function (report_name = NULL, report_for = NULL,
                                        table_name = NULL, column_name = NULL,
                                        row_name = NULL) {
    # helper {{{
    sep <- function (x, ignore_case = TRUE) {
        if (is.character(x)) {
            if (ignore_case) {
                stri_trans_tolower(paste(paste0("\"", x, "\""), sep = ",", collapse = ","))
            } else {
                paste(paste0("\"", x, "\""), sep = ",", collapse = ",")
            }
        } else {
            paste(x, sep = ",", collapse = ",")
        }
    }
    make <- function (arg, assertion = NULL, sql_col = NULL, ignore_case = TRUE) {
        a <- substitute(assertion)
        if (is.null(arg)) return(NULL)
        eval(a)
        sql_col <- deparse(substitute(arg))
        if (is.character(arg) && ignore_case) {
            paste0("lower(", sql_col, ") IN (", sep(unique(arg), TRUE), ")")
        } else {
            paste0(sql_col, " IN (", sep(unique(arg), FALSE), ")")
        }
    }
    `%and%` <- function (x, y) {
        if (is.null(y)) return(x)
        if (is.null(x)) return(y)
        paste0("(", x, ") AND (", y, ")")
    }
    # }}}
    # basic view {{{
    view <-
        "
        SELECT td.TabularDataIndex As tabular_data_index,
               reportn.Value As report_name,
               fs.Value As report_for,
               tn.Value As table_name,
               cn.Value As column_name,
               rn.Value As row_name,
               u.Value As units,
               td.Value As value

        FROM TabularData As td

        INNER JOIN Strings As reportn
        ON reportn.StringIndex = td.ReportNameIndex

        INNER JOIN Strings As fs
        ON fs.StringIndex = td.ReportForStringIndex

        INNER JOIN Strings As tn
        ON tn.StringIndex = td.TableNameIndex

        INNER JOIN Strings As rn
        ON rn.StringIndex = td.RowNameIndex

        INNER JOIN Strings As cn
        ON cn.StringIndex = td.ColumnNameIndex

        INNER JOIN Strings As u
        ON u.StringIndex = td.UnitsIndex
        "
    # }}}

    q <- NULL %and%
        make(report_name, assert(is.character(report_name), no_na(report_name))) %and%
        make(report_for, assert(is.character(report_for), no_na(report_for))) %and%
        make(table_name, assert(is.character(table_name), no_na(table_name))) %and%
        make(column_name, assert(is.character(column_name), no_na(column_name))) %and%
        make(row_name, assert(is.character(row_name), no_na(row_name)))

    if (is.null(q)) return(view)

    paste0( "SELECT * FROM (", view, ") WHERE ", q)
}
# }}}
# list_sql_table {{{
list_sql_table <- function (sql) {
    con <- conn_sql(sql)
    with_sql(con, RSQLite::dbListTables(con))
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

    # check leap year
    leap <- any(res$month == 2L & res$day == 29L)

    # get input year
    if (is.null(year)) {
        year <- lubridate::year(Sys.Date())

        # get wday per environment
        w <- get_sql_wday(sql)

        # for WinterDesignDay and SummerDesignDay, set to Monday
        set(w, NULL, "date", lubridate::make_date(year, w$month, w$day))
        set(w, NULL, "dt", get_epw_wday(w$day_type))
        w[day_type %chin% c("WinterDesignDay", "SummerDesignDay"), dt := 1L]

        if (any(!is.na(w$dt))) {
            for (i in which(!is.na(w$dt))) {
                set(w, i, "year", find_nearst_wday_year(w$date[i], w$dt[i], year, leap))
            }
        }

        # make sure all environments have a year value
        w[is.na(dt), year := lubridate::year(Sys.Date())]

        set(res, NULL, "year", w[J(res$environment_period_index), on = "environment_period_index", year])
    } else {
        set(res, NULL, "year", year)
    }

    set(res, NULL, "datetime",
        lubridate::make_datetime(res$year, res$month, res$day, res$hour, res$minute, tz = tz)
    )

    set(res, NULL, c("year", "environment_period_index"), NULL)

    # stop if any invalid datetime found
    if (anyNA(res$datetime)) {
        invld <- res[is.na(datetime)]
        mes <- invld[, paste0("Original: ", month, "-", day, " ",  hour, ":", minute,
            " --> New year: ", year)]
        abort("error_invalid_epw_date_introduced",
            paste0("Invalid date introduced with input start year:\n",
                paste0(mes, collapse = "\n")
            )
        )
    }

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
get_sql_tabular_data <- function (sql, report_name = NULL, report_for = NULL,
                                  table_name = NULL, column_name = NULL, row_name = NULL) {
    q <- get_sql_tabular_data_query(report_name, report_for, table_name, column_name, row_name)
    setnames(get_sql_query(sql, q), "tabular_data_index", "index")[]
}
# }}}
# get_sql_wday {{{
get_sql_wday <- function (sql) {
    q <- "
         SELECT Month AS month,
                Day AS day,
                DayType AS day_type,
                EnvironmentPeriodIndex AS environment_period_index
         FROM Time
         WHERE SimulationDays == 1 AND DayType IS NOT NULL
         GROUP BY EnvironmentPeriodIndex
        "
    get_sql_query(sql, q)
}
# }}}
# tidy_sql_name {{{
tidy_sql_name <- function (x) {
    setnames(x, stri_sub(gsub("([A-Z])", "_\\L\\1", names(x), perl = TRUE), 2L))
}
# }}}
