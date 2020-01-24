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
    # Query for Time {{{
    time <- NULL %and%
        .sql_make(month, assert(are_count(month), month <= 12L), "Month") %and%
        .sql_make(day, assert(are_count(day), day <= 31L), "Day") %and%
        .sql_make(hour, assert(are_count(hour, TRUE), hour <= 23L), "Hour") %and%
        .sql_make(minute, assert(are_count(minute, TRUE), minute <= 59L), "Minute") %and%
        .sql_make(interval, assert(are_count(interval)), "Interval") %and%
        .sql_make(simulation_days, assert(are_count(simulation_days)), "SimulationDays")

    if (!is.null(day_type)) {
        dt <- match_in_vec(day_type, label = TRUE,
            c("Monday", "Tuesday", "Wednesday", "Thursday",
              "Friday", "Saturday", "Sunday", "Holiday",
              "SummerDesignDay", "WinterDesignDay", "CustomDay1", "CustomDay2"
            )
        )
        assert(!is.na(dt), msg = paste0("Invalid day type found: ", collapse(day_type[is.na(dt)]), "."))
        time <- time %and% .sql_make(dt, sql_col = "DayType")
    }

    if (is.null(environment_name)) {
        env <- "EnvironmentPeriods"
    } else {
        assert(is.character(environment_name), no_na(environment_name))
        env <- paste0("
            (
            SELECT *
            FROM EnvironmentPeriods
            WHERE ", .sql_make(environment_name, sql_col = "EnvironmentName"), "
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
            period <- .sql_make(period, sql_col = "DateTime")

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
        .sql_make(key_value, assert(is.character(key_value), no_na(key_value)), "KeyValue") %and%
        .sql_make(name, assert(is.character(name), no_na(name)), "Name")

    if (is.null(key_var)) {
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
        .sql_make(report_name, assert(is.character(report_name), no_na(report_name))) %and%
        .sql_make(report_for, assert(is.character(report_for), no_na(report_for))) %and%
        .sql_make(table_name, assert(is.character(table_name), no_na(table_name))) %and%
        .sql_make(column_name, assert(is.character(column_name), no_na(column_name))) %and%
        .sql_make(row_name, assert(is.character(row_name), no_na(row_name)))

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
                                 tz = "UTC", case = "auto", all = FALSE, wide = FALSE,
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
    # month, day, hour, minute may be NA if reporting frequency is Monthly or
    # RunPeriod
    if (is.na(leap)) leap <- FALSE

    # get input year
    if (is.null(year)) {
        year <- lubridate::year(Sys.Date())

        # get wday per environment
        w <- get_sql_wday(sql)

        # in case there is no valid day type and get_sql_wday() returns nothing
        if (!nrow(w)) {
            set(res, NULL, "year", year)
        } else {
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
        }
    } else {
        set(res, NULL, "year", year)
    }

    set(res, NULL, "datetime",
        lubridate::make_datetime(res$year, res$month, res$day, res$hour, res$minute, tz = tz)
    )

    set(res, NULL, "year", NULL)

    # warning if any invalid datetime found
    # month, day, hour, minute may be NA if reporting frequency is Monthly or
    # RunPeriod
    if (anyNA(res[!is.na(month) & !is.na(day) & !is.na(hour) & !is.na(minute), datetime])) {
        invld <- res[!is.na(month) & !is.na(day) & !is.na(hour) & !is.na(minute)]
        mes <- invld[, paste0("Original: ", month, "-", day, " ",  hour, ":", minute,
            " --> New year: ", year)]
        warn("warn_invalid_epw_date_introduced",
            paste0("Invalid date introduced with input start year:\n",
                paste0(mes, collapse = "\n")
            )
        )
    }

    if (!all & !wide) {
        res <- res[, .SD, .SDcols = c("datetime", "key_value", "name", "units", "value")]
    } else {
        setcolorder(res, c("datetime", setdiff(names(res), "datetime")))
    }

    # change to wide table
    if (wide) res <- report_dt_to_wide(res, all)

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
                                  table_name = NULL, column_name = NULL, row_name = NULL,
                                  case = "auto", wide = FALSE, string_value = !wide) {
    q <- get_sql_tabular_data_query(report_name, report_for, table_name, column_name, row_name)
    dt <- setnames(get_sql_query(sql, q), "tabular_data_index", "index")[]

    if (not_empty(case)) {
        assert(is_scalar(case))
        case_name <- as.character(case)
        set(dt, NULL, "case", case_name)
        setcolorder(dt, c("case", setdiff(names(dt), "case")))
    }

    if (!wide) return(dt)

    if (!string_value) {
        set(dt, NULL, "is_num", FALSE)
        dt[!J(c("", " ")), on = "units", is_num := TRUE]
        # https://stackoverflow.com/questions/638565/parsing-scientific-notation-sensibly
        dt[J(FALSE), on = "is_num",
            is_num := any(stri_detect_regex(value, "^\\s*-?\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?$")),
            by = c("report_name", "report_for", "table_name", "column_name")
        ]
    }

    # add row index
    dt[, row_index := seq_len(.N), by = c("case"[has_name(dt, "case")], "report_name", "report_for", "table_name", "column_name")]

    # remove empty rows
    dt <- dt[!J(c("", "-"), c("", "-")), on = c("row_name", "value")]

    # fill downwards row names
    if (nrow(dt)) {
        dt[, row_name := row_name[[1L]],
            by = list(report_name, report_for, table_name, column_name, cumsum(row_name != ""))]
    }

    # combine column names and units
    dt[!J("", " "), on = "units", column_name := paste0(column_name, " [", units, "]")]

    l <- split(dt, by = c("report_name", "report_for", "table_name"))
    lapply(l, wide_tabular_data, string_value = string_value)
}
# }}}
# wide_tabular_data {{{
wide_tabular_data <- function (dt, string_value = TRUE) {
    # retain original column order
    cols <- unique(dt$column_name)

    # get numeric columns
    cols_num <- unique(dt$column_name[dt$is_num])

    # format table
    if (has_name(dt, "case")) {
        dt <- data.table::dcast.data.table(dt,
            case + report_name + report_for + table_name + row_index + row_name ~ column_name,
            value.var = "value"
        )
    } else {
        dt <- data.table::dcast.data.table(dt,
            report_name + report_for + table_name + row_index + row_name ~ column_name,
            value.var = "value"
        )
    }

    # clean
    set(dt, NULL, "row_index", NULL)

    # column order
    setcolorder(dt, c(setdiff(names(dt), cols), cols))

    # coerece type
    if (!string_value && length(cols_num)) {
        as_numeric <- function (x) suppressWarnings(as.numeric(x))
        dt[, c(cols_num) := lapply(.SD, as_numeric), .SDcols = cols_num]
    }

    dt[]
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
# get_sql_date {{{
get_sql_date <- function (sql, environment_period_index, simulation_days) {
    cond <- NULL %and%
        .sql_make(environment_period_index, sql_col = "EnvironmentPeriodIndex") %and%
        .sql_make(simulation_days, sql_col = "SimulationDays")
    q <- paste0("
         SELECT DISTINCT
                EnvironmentPeriodIndex as environment_period_index,
                SimulationDays as simulation_days,
                Month AS month,
                Day AS day
         FROM Time
         WHERE ", cond, " AND (Month IS NOT NULL) AND (Day IS NOT NULL)"
        )
    get_sql_query(sql, q)
}
# }}}
# tidy_sql_name {{{
tidy_sql_name <- function (x) {
    setnames(x, stri_sub(gsub("([A-Z])", "_\\L\\1", names(x), perl = TRUE), 2L))
}
# }}}
# report_dt_to_wide {{{
report_dt_to_wide <- function (dt, date_components = FALSE) {
    assert(has_name(dt, c("datetime", "month", "day", "hour", "minute",
        "key_value", "name", "environment_period_index", "environment_name",
        "reporting_frequency", "is_meter", "simulation_days", "day_type"
    )))

    # change detailed level frequency to "Each Call"
    dt[, Variable := reporting_frequency]
    dt[J("HVAC System Timestep"), on = "reporting_frequency", Variable := "Each Call"]
    dt[J("Zone Timestep"), on = "reporting_frequency", Variable := "TimeStep"]
    # combine key_value, name, and unit
    dt[J(1L), on = "is_meter", Variable := paste0(name, " [", units, "](", Variable, ")")]
    dt[J(0L), on = "is_meter", Variable := paste0(key_value, ":", name, " [", units, "](", Variable, ")")]

    # handle RunPeriod frequency
    if ("Run Period" %in% unique(dt$reporting_frequency)) {
        last_day <- dt[!is.na(datetime), .SD[.N],
            .SDcols = c("datetime", "month", "day", "hour", "minute"),
            by = "environment_period_index"
        ]
        set(last_day, NULL, "reporting_frequency", "Run Period")

        dt[last_day, on = c("environment_period_index", "reporting_frequency"),
            `:=`(datetime = i.datetime, month = i.month, day = i.day,
                 hour = i.hour, minute = i.minute
            )
        ]
    }

    # format datetime
    dt[, `Date/Time` := paste0(" ",
        stringi::stri_pad(month, 2, pad = "0"), "/",
        stringi::stri_pad(day, 2, pad = "0"), "  ",
        stringi::stri_pad(hour, 2, pad = "0"), ":",
        stringi::stri_pad(minute, 2, pad = "0")
    )]

    # handle special cases
    if (nrow(dt) & all(is.na(dt$datetime))) {
        dt[reporting_frequency != "Monthly", `Date/Time` := paste0("simdays=", simulation_days)]
        dt[reporting_frequency == "Monthly", `Date/Time` := {
            m <- get_sql_date(sql, .BY$environment_period_index, simulation_days)$month
            get_epw_month(m, label = TRUE)
        }, by = "environment_period_index"]
    }

    if (date_components) {
        # fill day_type
        dt[is.na(day_type) & !is.na(datetime) & hour == 24L,
            `:=`(day_type = wday(datetime - hours(1L), label = TRUE))
        ]
        dt[is.na(day_type) & !is.na(datetime) & hour != 24L,
            `:=`(day_type = wday(datetime, label = TRUE))
        ]

        if (has_name(dt, "case")) {
            dt <- dcast.data.table(dt, case +
                environment_period_index + environment_name + simulation_days +
                datetime + month + day + hour + minute +
                day_type + `Date/Time` ~ Variable,
                value.var = "value")
        } else {
            dt <- dcast.data.table(dt,
                environment_period_index + environment_name + simulation_days +
                datetime + month + day + hour + minute +
                day_type + `Date/Time` ~ Variable,
                value.var = "value")
        }
    } else {
        if (has_name(dt, "case")) {
            dt <- dcast.data.table(dt, case +
                environment_period_index + environment_name + simulation_days +
                `Date/Time` ~ Variable,
                value.var = "value")[, .SD, .SDcols = -(1:4)]
        } else {
            dt <- dcast.data.table(dt,
                environment_period_index + environment_name + simulation_days +
                `Date/Time` ~ Variable,
                value.var = "value")[, .SD, .SDcols = -(1:3)]
        }
    }

    dt
}
# }}}

# helper {{{
.sql_sep <- function (x, ignore_case = TRUE) {
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
.sql_make <- function (arg, assertion = NULL, sql_col = NULL, ignore_case = TRUE, env = parent.frame()) {
    a <- substitute(assertion, env)
    if (is.null(arg)) return(NULL)
    eval(a)
    if (is.null(sql_col)) {
        sql_col <- deparse(substitute(arg))
    }
    if (is.character(arg) && ignore_case) {
        paste0("lower(", sql_col, ") IN (", .sql_sep(unique(arg), TRUE), ")")
    } else {
        paste0(sql_col, " IN (", .sql_sep(unique(arg), FALSE), ")")
    }
}
`%and%` <- function (x, y) {
    if (is.null(y)) return(x)
    if (is.null(x)) return(y)
    paste0("(", x, ") AND (", y, ")")
}
# }}}
