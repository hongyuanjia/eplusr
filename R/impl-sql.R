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
# get_sql_tabular_data_query {{{
#' @importFrom checkmate assert_character
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

    assert_character(report_name, any.missing = FALSE, null.ok = TRUE)
    assert_character(report_for, any.missing = FALSE, null.ok = TRUE)
    assert_character(table_name, any.missing = FALSE, null.ok = TRUE)
    assert_character(column_name, any.missing = FALSE, null.ok = TRUE)
    assert_character(row_name, any.missing = FALSE, null.ok = TRUE)
    q <- NULL %and%
        .sql_make(report_name) %and%
        .sql_make(report_for) %and%
        .sql_make(table_name) %and%
        .sql_make(column_name) %and%
        .sql_make(row_name)

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
#' @importFrom checkmate assert_scalar
get_sql_report_data <- function (sql, key_value = NULL, name = NULL, year = NULL,
                                 tz = "UTC", case = "auto", all = FALSE, wide = FALSE,
                                 period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                                 interval = NULL, simulation_days = NULL, day_type = NULL,
                                 environment_name = NULL) {
    # report data dictionary {{{
    rpvar_dict <- read_sql_table(sql, "ReportDataDictionary")
    # ignore case
    set(rpvar_dict, NULL, c("key_value_lower", "name_lower"),
        list(stri_trans_tolower(rpvar_dict$key_value), stri_trans_tolower(rpvar_dict$name))
    )
    subset_rpvar <- FALSE
    if (!is.null(key_value)) {
        subset_rpvar <- TRUE
        if (is.data.frame(key_value)) {
            if (!is.null(name)) {
                warn("'name' will be ignored when 'key_value' is a data.frame.")
                name <- NULL
            }
            assert_names(names(key_value), must.include = c("key_value", "name"))
            if (ncol(key_value) > 2) set(key_value, NULL, setdiff(names(key_value), c("key_value", "name")), NULL)
            kv <- unique(key_value)
            set(kv, NULL, c("key_value_lower", "name_lower"),
                list(stri_trans_tolower(kv$key_value), stri_trans_tolower(kv$name))
            )
            rpvar_dict <- rpvar_dict[kv, on = c("key_value_lower", "name_lower"), nomatch = NULL]
            set(kv, NULL, c("key_value_lower", "name_lower"), NULL)
        } else {
            assert_character(key_value, any.missing = FALSE)
            KEY_VALUE <- unique(stri_trans_tolower(key_value))
            rpvar_dict <- rpvar_dict[J(KEY_VALUE), on = "key_value_lower", nomatch = NULL]
        }
    }

    if (!is.null(name)) {
        subset_rpvar <- TRUE
        assert_character(name, any.missing = FALSE)
        NAME <- unique(stri_trans_tolower(name))
        rpvar_dict <- rpvar_dict[J(NAME), on = "name_lower"]
    }
    set(rpvar_dict, NULL, c("key_value_lower", "name_lower"), NULL)
    # }}}

    # environment periods {{{
    env_periods <- read_sql_table(sql, "EnvironmentPeriods")
    if (!is.null(environment_name)) {
        assert_character(environment_name, any.missing = FALSE)
        ENVIRONMENT_NAME <- unique(stri_trans_toupper(environment_name))
        env_periods <- env_periods[J(ENVIRONMENT_NAME), on = "environment_name", nomatch = NULL]
    }
    # }}}

    # time {{{
    time <- read_sql_table(sql, "Time")
    set(time, NULL, c("warmup_flag", "interval_type"), NULL)
    setnames(time, toupper(names(time)))
    subset_time <- FALSE
    # store day of week for the first simulation day
    if (is.null(year) && !"year" %chin% names(time)) {
        # get wday of first simulation day per environment
        w <- time[SIMULATION_DAYS == 1L & !is.na(DAY_TYPE), .SD[1L],
            .SDcols = c("MONTH", "DAY", "DAY_TYPE", "ENVIRONMENT_PERIOD_INDEX"),
            by = "ENVIRONMENT_PERIOD_INDEX"
        ][!J(c("WinterDesignDay", "SummerDesignDay")), on = "DAY_TYPE"]
    }
    if (!is.null(month)) {
        subset_time <- TRUE
        assert_integerish(month, lower = 1L, upper = 12L, any.missing = FALSE)
        time <- time[J(unique(month)), on = "MONTH", nomatch = NULL]
    }
    if (!is.null(day)) {
        subset_time <- TRUE
        assert_integerish(day, lower = 1L, upper = 31L, any.missing = FALSE)
        time <- time[J(unique(day)), on = "DAY", nomatch = NULL]
    }
    if (!is.null(hour)) {
        subset_time <- TRUE
        assert_integerish(hour, lower = 0L, upper = 24L, any.missing = FALSE)
        time <- time[J(unique(hour)), on = "HOUR", nomatch = NULL]
    }
    if (!is.null(minute)) {
        subset_time <- TRUE
        assert_integerish(minute, lower = 0L, upper = 60L, any.missing = FALSE)
        time <- time[J(unique(minute)), on = "MINUTE", nomatch = NULL]
    }
    if (!is.null(interval)) {
        subset_time <- TRUE
        assert_integerish(interval, lower = 1L, upper = 527040, any.missing = FALSE)
        time <- time[J(unique(interval)), on = "INTERVAL", nomatch = NULL]
    }
    if (!is.null(simulation_days)) {
        subset_time <- TRUE
        assert_integerish(simulation_days, lower = 1L, upper = 366, any.missing = FALSE)
        time <- time[J(unique(simulation_days)), on = "SIMULATION_DAYS", nomatch = NULL]
    }
    if (!is.null(period)) {
        subset_time <- TRUE
        if (!any(c("Date", "POSIXt") %in% class(period)))
            abort("'period' should be a Date or DateTime vector.")
        p <- unique(period)
        if (inherits(period, "Date")) {
            period <- data.table(
                MONTH = lubridate::month(p),
                DAY = lubridate::mday(p)
            )
        } else {
            period <- data.table(
                MONTH = lubridate::month(p),
                DAY = lubridate::mday(p),
                HOUR = lubridate::hour(p),
                MINUTE = lubridate::minute(p)
            )
        }
        time <- time[period, on = names(period), nomatch = NULL]
    }
    if (!is.null(day_type)) {
        subset_time <- TRUE
        weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
        weekend <- c("Saturday", "Sunday")
        designday <- c("SummerDesignDay", "WinterDesignDay")
        customday <- c("CustomDay1", "CustomDay2")
        specialday <- c(designday, customday)
        normalday <- c(weekday, weekend, "Holiday")
        dt <- match_in_vec(day_type, label = TRUE, unique(
            c(weekday,   weekend,   designday,   customday,   specialday,   normalday,
              "Weekday", "Weekend", "DesignDay", "CustomDay", "SpecialDay", "NormalDay"
        )))
        if (anyNA(dt)) abort(paste0("Invalid day type found: ", collapse(day_type[is.na(dt)]), "."))
        # expand
        expd <- c()
        if ("Weekday" %chin% dt) {expd <- c(expd, weekday); dt <- setdiff(dt, "Weekday")}
        if ("Weekend" %chin% dt) {expd <- c(expd, weekend); dt <- setdiff(dt, "Weekend")}
        if ("DesignDay" %chin% dt) {expd <- c(expd, designday); dt <- setdiff(dt, "DesignDay")}
        if ("CustomDay" %chin% dt) {expd <- c(expd, customday); dt <- setdiff(dt, "CustomDay")}
        if ("SpecialDay" %chin% dt) {expd <- c(expd, specialday); dt <- setdiff(dt, "SpecialDay")}
        if ("NormalDay" %chin% dt) {expd <- c(expd, normalday); dt <- setdiff(dt, "NormalDay")}
        dt <- unique(c(dt, expd))
        time <- time[J(dt), on = "DAY_TYPE"]
    }

    setnames(time, tolower(names(time)))
    time <- time[env_periods, on = "environment_period_index", nomatch = NULL]

    # get input year
    if (!is.null(year)) {
        year <- assert_count(year, positive = TRUE, coerce = TRUE)
        set(time, NULL, "year", year)
    } else {
        # current year
        cur_year <- lubridate::year(Sys.Date())

        if ("year" %in% names(time)) {
            time[J(0L), on = "year", year := NA_integer_]
        } else {
            setnames(w, tolower(names(w)))

            # in case there is no valid day type
            if (!nrow(w)) {
                # directly assign current year
                set(time, NULL, "year", cur_year)
            } else {
                set(w, NULL, "date", lubridate::make_date(cur_year, w$month, w$day))
                set(w, NULL, "dt", get_epw_wday(w$day_type))

                # check leap year
                leap <- time[J(2L, 29L), on = c("month", "day"), nomatch = NULL, .N > 0]

                if (any(!is.na(w$dt))) {
                    for (i in which(!is.na(w$dt))) {
                        set(w, i, "year", find_nearst_wday_year(w$date[i], w$dt[i], cur_year, leap))
                    }
                }

                # make sure all environments have a year value
                w[is.na(dt), year := lubridate::year(Sys.Date())]

                set(time, NULL, "year", w[J(time$environment_period_index), on = "environment_period_index", year])
            }
        }

        # for SummerDesignDay and WinterDesignDay, directly use current year
        time[J(c("WinterDesignDay", "SummerDesignDay")), on = "day_type", year := cur_year]
    }

    set(time, NULL, "datetime",
        lubridate::make_datetime(time$year, time$month, time$day, time$hour, time$minute, tz = tz)
    )

    set(time, NULL, "year", NULL)

    # warning if any invalid datetime found
    # month, day, hour, minute, day_type may be NA if reporting frequency is
    # Monthly or RunPeriod
    norm_time <- na.omit(time, cols = c("month", "day", "hour", "minute", "day_type"))
    if (anyNA(norm_time$datetime)) {
        mes <- norm_time[is.na(datetime), paste0(
            "Original: ", month, "-", day, " ",  hour, ":", minute, " --> New year: ", year
        )]
        warn(paste0("Invalid date introduced with input start year:\n",
                paste0(mes, collapse = "\n")
            ),
            "invalid_epw_date_introduced"
        )
    }
    # }}}

    # construct queries for ReportData {{{
    # no subset on variables
    if (subset_rpvar) {
        # no subset on time
        if (!subset_time) {
            rp_data <- read_sql_table(sql, "ReportData")
        # subset on time with no matched
        } else if (!nrow(time)) {
            rp_data <- tidy_sql_name(get_sql_query(sql, "SELECT * FROM ReportData LIMIT 0"))
        } else {
            rp_data <- tidy_sql_name(get_sql_query(sql, paste0(
                "SELECT * FROM ReportData WHERE TimeIndex IN ",
                "(", paste0(sprintf("'%s'", time$time_index), collapse = ", "), ")"
            )))
        }
    # subset on variables with no matched
    } else if (!nrow(rpvar_dict)) {
        rp_data <- tidy_sql_name(get_sql_query(sql, "SELECT * FROM ReportData LIMIT 0"))
    } else {
        # no subset on time
        if (!subset_time) {
            rp_data <- tidy_sql_name(get_sql_query(sql, paste0(
                "SELECT * FROM ReportData WHERE ReportDataDictionaryIndex IN ",
                "(", paste0(sprintf("'%s'", rpvar_dict$report_data_dictionary_index), collapse = ", "), ")"
            )))
        # subset on time with no matched
        } else if (!nrow(time)) {
            rp_data <- tidy_sql_name(get_sql_query(sql, "SELECT * FROM ReportData LIMIT 0"))
        } else {
            rp_data <- tidy_sql_name(get_sql_query(sql, paste0(
                "SELECT * FROM ReportData WHERE TimeIndex IN ",
                "(", paste0(sprintf("'%s'", time$time_index), collapse = ", "), ")",
                "AND ReportDataDictionaryIndex IN ",
                "(", paste0(sprintf("'%s'", rpvar_dict$report_data_dictionary_index), collapse = ", "), ")"
            )))
        }
    }
    # }}}

    res <- time[rp_data, on = "time_index", nomatch = NULL][rpvar_dict, on = "report_data_dictionary_index", nomatch = NULL]
    cols <- c("datetime", "month", "day", "hour", "minute", "dst",
        "interval", "simulation_days", "day_type", "environment_name",
        "environment_period_index", "is_meter", "type", "index_group",
        "timestep_type", "key_value", "name", "reporting_frequency",
        "schedule_name", "units", "value"
    )
    set(res, NULL, setdiff(names(res), cols), NULL)
    setcolorder(res, cols)

    if (!all & !wide) {
        res <- res[, .SD, .SDcols = c("datetime", "key_value", "name", "units", "value")]
    }

    # change to wide table
    if (wide) res <- report_dt_to_wide(res, all)

    if (!is.null(case)) {
        assert_scalar(case)
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
#' @importFrom checkmate assert_scalar
get_sql_tabular_data <- function (sql, report_name = NULL, report_for = NULL,
                                  table_name = NULL, column_name = NULL, row_name = NULL,
                                  case = "auto", wide = FALSE, string_value = !wide) {
    q <- get_sql_tabular_data_query(report_name, report_for, table_name, column_name, row_name)
    dt <- setnames(get_sql_query(sql, q), "tabular_data_index", "index")[]

    if (!is.null(case)) {
        assert_scalar(case)
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
    dt[, row_index := seq_len(.N), by = c("case"[has_names(dt, "case")], "report_name", "report_for", "table_name", "column_name")]

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
    if (has_names(dt, "case")) {
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
# tidy_sql_name {{{
tidy_sql_name <- function (x) {
    setnames(x, stri_sub(gsub("([A-Z])", "_\\L\\1", names(x), perl = TRUE), 2L))
}
# }}}
# report_dt_to_wide {{{
#' @importFrom checkmate assert_names
report_dt_to_wide <- function (dt, date_components = FALSE) {
    assert_names(names(dt), must.include = c(
        "datetime", "month", "day", "hour", "minute",
        "key_value", "name", "environment_period_index", "environment_name",
        "reporting_frequency", "is_meter", "simulation_days", "day_type"
    ))

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
        dt[, `Date/Time` := paste0("simdays=", simulation_days)]
    }

    if (date_components) {
        # fill day_type
        dt[is.na(day_type) & !is.na(datetime) & hour == 24L,
            `:=`(day_type = wday(datetime - hours(1L), label = TRUE))
        ]
        dt[is.na(day_type) & !is.na(datetime) & hour != 24L,
            `:=`(day_type = wday(datetime, label = TRUE))
        ]

        if (has_names(dt, "case")) {
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
        if (has_names(dt, "case")) {
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
