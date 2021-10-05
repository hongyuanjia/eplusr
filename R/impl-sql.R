#' @importFrom RSQLite SQLite dbConnect dbDisconnect dbGetQuery dbListTables dbReadTable
#' @importFrom data.table setDT setcolorder setorder
#' @importFrom lubridate year force_tz make_datetime
NULL

# RPFREQ {{{
RPFREQ <- c(
    "Each Call" = -1L,
    "TimeStep"  =  0L,
    "Hourly"    =  1L,
    "Daily"     =  2L,
    "Monthly"   =  3L,
    "RunPeriod" =  4L,
    "Annual"    =  5L
)
# }}}
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
# get_sql_report_data_dict {{{
get_sql_report_data_dict <- function (sql) {
    read_sql_table(sql, "ReportDataDictionary")
}
# }}}
# get_sql_report_data {{{
#' @importFrom checkmate assert_scalar
get_sql_report_data <- function (sql, key_value = NULL, name = NULL, year = NULL,
                                 tz = "UTC", case = "auto", all = FALSE, wide = FALSE,
                                 period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                                 interval = NULL, simulation_days = NULL, day_type = NULL,
                                 environment_name = NULL, index = NULL) {
    dict <- read_sql_table(sql, "ReportDataDictionary")
    env <- read_sql_table(sql, "EnvironmentPeriods")
    time <- read_sql_table(sql, "Time")

    res <- read_report_data_sql(sql, env, dict, time,
        key_value = key_value, name = name,
        year = year, tz = tz, period = period,
        month = month, day = day, hour = hour, minute = minute,
        interval = interval, simulation_days = simulation_days,
        environment_name = environment_name,
        day_type = day_type, wide = wide, all = all)

    if (!is.null(case)) {
        assert_scalar(case)
        case_name <- as.character(case)
        set(res, NULL, "case", case_name)
        setcolorder(res, "case")
    }
    if (!is.null(index)) {
        assert_int(index)
        set(res, NULL, "index", index)
        setcolorder(res, "index")
    }

    res
}
# }}}
# get_sql_tabular_data {{{
#' @importFrom checkmate assert_scalar
get_sql_tabular_data <- function (sql, report_name = NULL, report_for = NULL,
                                  table_name = NULL, column_name = NULL, row_name = NULL,
                                  case = "auto", wide = FALSE, string_value = !wide, index = NULL) {
    q <- get_sql_tabular_data_query(report_name, report_for, table_name, column_name, row_name)
    dt <- set(get_sql_query(sql, q), NULL, "tabular_data_index", NULL)

    if (!is.null(case)) {
        assert_scalar(case)
        case_name <- as.character(case)
        set(dt, NULL, "case", case_name)
        setcolorder(dt, "case")
    }
    if (!is.null(index)) {
        assert_int(index)
        set(dt, NULL, "index", index)
        setcolorder(dt, "index")
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
    dt[, row_index := seq_len(.N), by = c(
        "index"[has_names(dt, "index")], "case"[has_names(dt, "case")],
        "report_name", "report_for", "table_name", "column_name")]

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
# subset_sql_time {{{
subset_sql_time <- function (time, year = NULL, tz = "UTC", period = NULL,
                             month = NULL, day = NULL, hour = NULL, minute = NULL,
                             interval = NULL, simulation_days = NULL, day_type = NULL,
                             environment_name = NULL, datetime = TRUE) {
    setnames(time, toupper(names(time)))

    subset_time <- FALSE

    # store day of week for the first simulation day
    if (datetime && is.null(year)) {
        # get wday of first normal simulation day per environment
        # here should exclude special days including SummerDesignDay,
        # WinterDesignDay, Holiday, CustomDay1 and CustomDay2
        # see #450
        #
        # note that it is possible that there is only one day in a run period
        # and that day is Holiday/CustomDay1/CustomDay2
        # there is no way to get the day of the week of start day directly from
        # the SQL
        # in this case, still return the first day and the nearest year will be
        # used
        w <- time[!is.na(DAY_TYPE) &
            !DAY_TYPE %chin% c("WinterDesignDay", "SummerDesignDay"),
            {
                if (all(unique(DAY_TYPE) %chin% c("Holiday", "CustomDay1", "CustomDay2"))) {
                    .SD[1L]
                } else {
                    .SD[which(!DAY_TYPE %chin% c("Holiday", "CustomDay1", "CustomDay2"))[1L]]
                }
            },
            .SDcols = c("MONTH", "DAY", "DAY_TYPE", "SIMULATION_DAYS"),
            by = "ENVIRONMENT_PERIOD_INDEX"
        ]
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
        time <- time[J(unique(match_daytype(day_type))), on = "DAY_TYPE", nomatch = NULL]
    }

    setnames(time, tolower(names(time)))

    if (datetime) time <- create_sql_datetime(time, w, year, tz)

    setorderv(time, "time_index")
    setattr(time, "filtered", subset_time)

    time
}
# }}}
# subset_sql_report_data_dict {{{
subset_sql_report_data_dict <- function (dict, key_value = NULL, name = NULL) {
    # ignore case
    set(dict, NULL, c("key_value_lower", "name_lower"),
        list(stri_trans_tolower(dict$key_value), stri_trans_tolower(dict$name))
    )

    subset_rpvar <- FALSE
    if (!is.null(key_value)) {
        subset_rpvar <- TRUE

        if (!is.data.frame(key_value)) {
            assert_character(key_value, any.missing = FALSE)
            KEY_VALUE <- unique(stri_trans_tolower(key_value))
            dict <- dict[J(KEY_VALUE), on = "key_value_lower", nomatch = NULL]
        } else {
            if (!is.null(name)) {
                warn("'name' will be ignored when 'key_value' is a data.frame.")
                name <- NULL
            }
            if (!inherits(key_value, "data.table")) key_value <- as.data.table(key_value)

            assert_names(names(key_value), must.include = c("key_value", "name"))

            if (ncol(key_value) > 2) set(key_value, NULL, setdiff(names(key_value), c("key_value", "name")), NULL)

            kv <- unique(key_value)
            set(kv, NULL, c("key_value_lower", "name_lower"),
                list(stri_trans_tolower(kv$key_value), stri_trans_tolower(kv$name))
            )
            dict <- dict[kv, on = c("key_value_lower", "name_lower"), nomatch = NULL]
            set(kv, NULL, c("key_value_lower", "name_lower"), NULL)
        }
    }

    if (!is.null(name)) {
        subset_rpvar <- TRUE
        assert_character(name, any.missing = FALSE)
        NAME <- unique(stri_trans_tolower(name))
        dict <- dict[J(NAME), on = "name_lower", nomatch = NULL]
    }

    set(dict, NULL, c("key_value_lower", "name_lower"), NULL)
    setorderv(dict, "report_data_dictionary_index")
    setattr(dict, "filtered", subset_rpvar)

    dict
}
# }}}
# subset_sql_environment_periods {{{
subset_sql_environment_periods <- function (env, environment_name = NULL) {
    if (is.null(environment_name)) return(setattr(env, "filtered", FALSE))

    assert_character(environment_name, any.missing = FALSE)
    ENVIRONMENT_NAME <- unique(stri_trans_toupper(environment_name))
    setattr(env[J(ENVIRONMENT_NAME), on = "environment_name", nomatch = NULL],
        "filtered", TRUE
    )
}
# }}}
# create_sql_datetime {{{
create_sql_datetime <- function (time, first_day = NULL, year = NULL, tz = "UTC") {
    # get input year
    if (!is.null(year)) {
        year <- assert_count(year, positive = TRUE, coerce = TRUE)
        set(time, NULL, "year", year)
    } else {
        # use the nearest year as EnergyPlus
        # see EnergyPlus/WeatherManager/findYearForWeekday
        nearest_year <- 2017L

        if ("year" %in% names(time)) {
            time[J(0L), on = "year", year := NA_integer_]
        }

        setnames(first_day, tolower(names(first_day)))

        # in case there is no valid day type
        if (!nrow(first_day)) {
            # directly assign current year
            set(time, NULL, "year", nearest_year)
        } else {
            set(first_day, NULL, "date", lubridate::make_date(nearest_year, first_day$month, first_day$day))
            set(first_day, NULL, "dt", get_epw_wday(first_day$day_type))

            # check leap year
            leap <- time[J(2L, 29L), on = c("month", "day"), nomatch = NULL, .N > 0]

            if (any(!is.na(first_day$dt))) {
                for (i in which(!is.na(first_day$dt))) {
                    set(first_day, i, "year", find_nearst_wday_year(first_day$date[i], first_day$dt[i], nearest_year, leap))
                }
            }

            # make sure all environments have a year value
            first_day[is.na(dt), year := lubridate::year(Sys.Date())]

            time[first_day, on = "environment_period_index", year := i.year]
        }

        # for SummerDesignDay and WinterDesignDay, directly use current year
        time[J(c("WinterDesignDay", "SummerDesignDay")), on = "day_type", year := nearest_year]
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

    time
}
# }}}
# complete_sql_time {{{
complete_sql_time <- function (time) {
    if (!nrow(time)) return(time)

    # for annual
    cols <- c("datetime", "month", "day", "hour", "minute", "dst", "simulation_days", "day_type")
    cols <- cols[has_names(time, cols)]

    update_cols <- function (dt, type, cols) {
        if (!any(i <- dt$interval_type == type)) return(dt)

        env <- call("%in%", as.name("environment_period_index"), unique(dt$environment_period_index[i]))
        ind <- call("!", call("is.na", as.name(cols[1L])))
        int <- call("=", as.name("interval_type"), min(dt$interval_type))
        expr <- call("&", call("&", env, ind), int)

        by <- "environment_period_index"
        if (type == RPFREQ["Monthly"]) by <- c(by, "month")
        last_day <- dt[eval(expr), .SD[.N], .SDcols = cols, by = by]
        set(last_day, NULL, "interval_type", type)

        expr <- as.call(c(list(as.name(":=")), stats::setNames(lapply(paste0("i.", cols), as.name), cols)))

        dt[last_day, on = c(by, "interval_type"), eval(expr)]

        dt
    }

    update_cols(time, RPFREQ["Monthly"], c("dst", "day_type"))
    update_cols(time, RPFREQ["RunPeriod"], cols)
    update_cols(time, RPFREQ["Annual"], cols)

    time
}
# }}}
# wide_tabular_data {{{
wide_tabular_data <- function (dt, string_value = TRUE) {
    # retain original column order
    cols <- unique(dt$column_name)

    # get numeric columns
    cols_num <- unique(dt$column_name[dt$is_num])

    # format table
    fml <- "report_name + report_for + table_name + row_index + row_name ~ column_name"
    if (has_names(dt, "case")) {
        fml <- paste("case", fml, sep = " + ")
    }
    if (has_names(dt, "index")) {
        fml <- paste("index", fml, sep = " + ")
    }
    dt <- data.table::dcast.data.table(dt, stats::as.formula(fml), value.var = "value")

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
# read_report_data_sql {{{
read_report_data_sql <- function (sql, env, dict, time,
                                  # dict
                                  key_value = NULL, name = NULL,
                                  # time
                                  year = NULL, tz = "UTC", period = NULL,
                                  month = NULL, day = NULL, hour = NULL, minute = NULL,
                                  interval = NULL, simulation_days = NULL, day_type = NULL,
                                  # run period
                                  environment_name = NULL,
                                  all = FALSE, wide = FALSE) {
    # See https://github.com/NREL/EnergyPlus/issues/8268
    # If `Do HVAC Sizing Simulation for Sizing Periods` is set to `Yes` in
    # `SimulationControl`, extra run periods will be added for winter design day
    # and summer design day. The problem is that the Time table has correctly
    # reflected this by adding new time for those added run periods, but
    # EnvironmentPeriods table did not have entries for those periods.
    ind_time_env <- unique(time$environment_period_index)
    if (length(extra_env <- setdiff(ind_time_env, env$environment_period_index))) {
        # append rows for environment period table
        time_env <- time[J(ind_time_env), on = "environment_period_index", mult = "first",
            .SD, .SDcols = c("day_type", "environment_period_index")]
        setorderv(time_env, c("environment_period_index", "day_type"))
        env <- env[time_env, on = "environment_period_index"]
        setnafill(env, "locf", cols = "simulation_index")
        env[J(c("SummerDesignDay", "WinterDesignDay")), on = "day_type", by = "day_type",
            environment_name := {
                # construct environment name
                n_pass <- .N - 1L
                # It is possible that 'Run Simulation for Sizing Periods' is set to
                # 'No' but 'Do HVAC Sizing Simulation for Sizing Periods' is set to
                # 'Yes'. In this case, no DDY name is in the 'EnvironmentPeriods'
                # table.
                if (!n_pass) {
                    environment_name <- paste(.BY$day_type, "HVAC Sizing Pass 1")
                } else {
                    suffix <- paste("HVAC Sizing Pass", seq_len(n_pass))
                    environment_name[is.na(environment_name)] <- paste(environment_name[1L], suffix)
                }
                environment_name
            }
        ]
    }

    dict <- subset_sql_report_data_dict(dict, key_value = key_value, name = name)
    env <- subset_sql_environment_periods(env, environment_name = environment_name)
    time <- time[env, on = "environment_period_index", nomatch = NULL]
    time <- complete_sql_time(time)
    time <- subset_sql_time(time, year = year, tz = tz, period = period,
        month = month, day = day, hour = hour, minute = minute,
        interval = interval, simulation_days = simulation_days,
        day_type = day_type)

    subset_var <- attr(dict, "filtered")
    subset_time <- attr(time, "filtered") || attr(env, "filtered")

    # no subset on variables
    if (!subset_var) {
        # no subset on time
        if (!subset_time) {
            data <- read_sql_table(sql, "ReportData")
        # subset on time with no matched
        } else if (!nrow(time)) {
            data <- tidy_sql_name(get_sql_query(sql, "SELECT * FROM ReportData LIMIT 0"))
        } else {
            data <- tidy_sql_name(get_sql_query(sql, paste0(
                "SELECT * FROM ReportData WHERE TimeIndex IN ",
                "(", paste0(sprintf("'%s'", time$time_index), collapse = ", "), ")"
            )))
        }
    # subset on variables with no matched
    } else if (!nrow(dict)) {
        data <- tidy_sql_name(get_sql_query(sql, "SELECT * FROM ReportData LIMIT 0"))
    } else {
        # no subset on time
        if (!subset_time) {
            data <- tidy_sql_name(get_sql_query(sql, paste0(
                "SELECT * FROM ReportData WHERE ReportDataDictionaryIndex IN ",
                "(", paste0(sprintf("'%s'", dict$report_data_dictionary_index), collapse = ", "), ")"
            )))
        # subset on time with no matched
        } else if (!nrow(time)) {
            data <- tidy_sql_name(get_sql_query(sql, "SELECT * FROM ReportData LIMIT 0"))
        } else {
            data <- tidy_sql_name(get_sql_query(sql, paste0(
                "SELECT * FROM ReportData WHERE TimeIndex IN ",
                "(", paste0(sprintf("'%s'", time$time_index), collapse = ", "), ")",
                "AND ReportDataDictionaryIndex IN ",
                "(", paste0(sprintf("'%s'", dict$report_data_dictionary_index), collapse = ", "), ")"
            )))
        }
    }

    if (wide) {
        # generate Date/Time column
        if (nrow(time)) set(time, NULL, "interval_type", min(time$interval_type))

        time <- add_csv_time(time)

        setorderv(data, "time_index")

        if (!nrow(data)) {
            data <- data.table(time_index = integer(), report_data_dictionary_index = integer())
        } else {
            # it is possible that same datetime has different time indices
            add_joined_cols(time, data, "time_index", c("environment_period_index", "datetime"))
            time <- unique(time, by = c("environment_period_index", "datetime"))

            data <- dcast.data.table(data, environment_period_index + datetime ~ report_data_dictionary_index, value.var = "value")
            # restore time index
            data[time, on = c("environment_period_index", "datetime"), time_index := i.time_index]
            set(data, NULL, c("environment_period_index", "datetime"), NULL)
        }

        # add long output name
        if (!nrow(data)) {
            set(data, NULL, "report_data_dictionary_index", NULL)
        } else {
            dict <- add_csv_variable(dict)
            cols <- setdiff(names(data), "time_index")
            setnames(data, cols, dict[J(as.integer(cols)), on = "report_data_dictionary_index", Variable])
        }

        if (!all) {
            cols <- "Date/Time"
        } else {
            cols <- c("environment_period_index", "environment_name", "simulation_days",
                "datetime", "month", "day", "hour", "minute", "day_type", "Date/Time")
        }

        add_joined_cols(time, data, "time_index", cols)
        setcolorder(data, cols)
        set(data, NULL, "time_index", NULL)
    } else {
        set(data, NULL, "report_data_index", NULL)

        if (!all) {
            cols_time <- "datetime"
            cols_dict <- c("key_value", "name", "units")
        } else {
            cols_time <- c("datetime", "month", "day", "hour", "minute", "dst",
                "interval", "simulation_days", "day_type", "environment_name",
                "environment_period_index")
            cols_dict <- c("is_meter", "type", "index_group", "timestep_type",
                "key_value", "name", "reporting_frequency", "schedule_name",
                "units")
        }

        add_joined_cols(time, data, "time_index", cols_time)
        add_joined_cols(dict, data, "report_data_dictionary_index", cols_dict)

        setorderv(data, c("report_data_dictionary_index", "time_index"))
        set(data, NULL, c("report_data_dictionary_index", "time_index"), NULL)
        setcolorder(data, c(cols_time, cols_dict))
    }

    data
}
# }}}
# get_sql_reporting_freq {{{
get_sql_reporting_freq <- function (freq) {
    freq[freq == "HVAC System Timestep"] <- "Each Call"
    freq[freq == "Zone Timestep"] <- "TimeStep"
    freq[freq == "Run Period"] <- "RunPeriod"

    RPFREQ[freq]
}
# }}}
# add_csv_variable {{{
add_csv_variable <- function (dict) {
    # change detailed level frequency to "Each Call"
    dict[, Variable := reporting_frequency]
    dict[J("HVAC System Timestep"), on = "reporting_frequency", Variable := "Each Call"]
    dict[J("Zone Timestep"), on = "reporting_frequency", Variable := "TimeStep"]
    dict[J("Run Period"), on = "reporting_frequency", Variable := "RunPeriod"]
    # combine key_value, name, and unit
    dict[, Variable := paste0(name, " [", units, "](", Variable, ")")]
    dict[!J(1L), on = "is_meter", Variable := paste0(key_value, ":", Variable)]
    dict[J(1L, "Cumulative "), on = c("is_meter", "key_value"), Variable := paste0(key_value, Variable)]

    dict
}
# }}}
# add_csv_time {{{
add_csv_time <- function (time) {
    if (!nrow(time)) set(time, NULL, "Date/Time", character())

    time[J(RPFREQ[c("Each Call", "TimeStep", "Hourly")]), on = "interval_type",
        `Date/Time` := paste0(
            stringi::stri_pad(month, 2, pad = "0"), "/",
            stringi::stri_pad(day, 2, pad = "0"), "  ",
            stringi::stri_pad(hour, 2, pad = "0"), ":",
            stringi::stri_pad(minute, 2, pad = "0"), ":00"
        )
    ]

    time[J(RPFREQ["Daily"]), on = "interval_type",
        `Date/Time` := paste0(
            stringi::stri_pad(month, 2, pad = "0"), "/",
            stringi::stri_pad(day, 2, pad = "0")
        )
    ]

    time[J(RPFREQ["Monthly"]), on = "interval_type",
        `Date/Time` := base::month.name[month]
    ]

    time[J(RPFREQ[c("RunPeriod", "Annual")]), on = "interval_type",
        `Date/Time` := paste0("simdays=", simulation_days)
    ]

    time
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
