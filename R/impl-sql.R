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
get_sql_report_data <- function (sql, csv = NULL, key_value = NULL, name = NULL, year = NULL,
                                 tz = "UTC", case = "auto", all = FALSE, wide = FALSE,
                                 period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                                 interval = NULL, simulation_days = NULL, day_type = NULL,
                                 environment_name = NULL) {
    dict <- read_sql_table(sql, "ReportDataDictionary")
    env <- read_sql_table(sql, "EnvironmentPeriods")
    time <- read_sql_table(sql, "Time")

    if (!is.null(csv)) {
        timestep <- read_sql_table(sql, "Simulations")$num_timesteps_per_hour
        res <- read_report_data_csv(csv, env, dict, time,
            key_value = key_value, name = name,
            year = year, tz = tz, period = period,
            month = month, day = day, hour = hour, minute = minute,
            interval = interval, simulation_days = simulation_days,
            environment_name = environment_name,
            day_type = day_type, wide = wide, all = all, timestep = timestep)
    } else {
        res <- read_report_data_sql(sql, env, dict, time,
            key_value = key_value, name = name,
            year = year, tz = tz, period = period,
            month = month, day = day, hour = hour, minute = minute,
            interval = interval, simulation_days = simulation_days,
            environment_name = environment_name,
            day_type = day_type, wide = wide, all = all)
    }

    if (!is.null(case)) {
        assert_scalar(case)
        case_name <- as.character(case)
        set(res, NULL, "case", case_name)
        setcolorder(res, c("case", setdiff(names(res), "case")))
    }

    res
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
# subset_sql_time {{{
subset_sql_time <- function (time, year = NULL, tz = "UTC", period = NULL,
                             month = NULL, day = NULL, hour = NULL, minute = NULL,
                             interval = NULL, simulation_days = NULL, day_type = NULL,
                             environment_name = NULL, datetime = TRUE) {
    setnames(time, toupper(names(time)))

    subset_time <- FALSE

    # store day of week for the first simulation day
    if (datetime && is.null(year) && !"year" %chin% names(time)) {
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
        # current year
        cur_year <- lubridate::year(Sys.Date())

        if ("year" %in% names(time)) {
            time[J(0L), on = "year", year := NA_integer_]
        } else {
            setnames(first_day, tolower(names(first_day)))

            # in case there is no valid day type
            if (!nrow(first_day)) {
                # directly assign current year
                set(time, NULL, "year", cur_year)
            } else {
                set(first_day, NULL, "date", lubridate::make_date(cur_year, first_day$month, first_day$day))
                set(first_day, NULL, "dt", get_epw_wday(first_day$day_type))

                # check leap year
                leap <- time[J(2L, 29L), on = c("month", "day"), nomatch = NULL, .N > 0]

                if (any(!is.na(first_day$dt))) {
                    for (i in which(!is.na(first_day$dt))) {
                        set(first_day, i, "year", find_nearst_wday_year(first_day$date[i], first_day$dt[i], cur_year, leap))
                    }
                }

                # make sure all environments have a year value
                first_day[is.na(dt), year := lubridate::year(Sys.Date())]

                set(time, NULL, "year", first_day[J(time$environment_period_index), on = "environment_period_index", year])
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
# read_report_data_csv {{{
read_report_data_csv <- function (csv, env, dict, time,
                                  # dict
                                  key_value = NULL, name = NULL,
                                  # time
                                  year = NULL, tz = "UTC", period = NULL,
                                  month = NULL, day = NULL, hour = NULL, minute = NULL,
                                  interval = NULL, simulation_days = NULL, day_type = NULL,
                                  # run period
                                  environment_name = NULL,
                                  all = FALSE, wide = FALSE, timestep = NULL) {
    # get start row and number of rows to read using fread before subsetting
    # use the more detailed one for row subsetting
    int <- min(get_sql_reporting_freq(dict$reporting_frequency))

    # handle RunPeriod frequency
    time <- complete_sql_time(time)

    # get time data used for row subsetting
    # should check if detailed output is required
    # in this case, interval_type for TimeStep will also be -1 but not 0
    # timestep is in the time table
    if (int == RPFREQ["Each Call"] | int == RPFREQ["TimeStep"]) {
        time_csv <- unique(time[J(RPFREQ["Each Call"]), on = "interval_type"], by = c("environment_period_index", "month", "day", "hour", "minute"))
    } else {
        time_csv <- time[J(int), on = "interval_type", nomatch = NULL]
    }

    # store all time index in case used when fread failed to detect correct
    # column numbers
    time_index_all <- time_csv$time_index

    dict <- subset_sql_report_data_dict(dict, key_value = key_value, name = name)
    env <- subset_sql_environment_periods(env, environment_name = environment_name)

    subset_var <- attr(dict, "filtered")
    subset_env <- attr(env, "filtered")

    time_csv <- time_csv[env, on = "environment_period_index", nomatch = NULL]

    # create datetime column
    # necessary for running 'complete_sql_time'
    if (is.null(year) && !"year" %chin% names(time_csv)) {
        # get wday of first simulation day per environment
        w <- time_csv[simulation_days == 1L & !is.na(day_type), .SD[1L],
            .SDcols = c("month", "day", "day_type", "environment_period_index"),
            by = "environment_period_index"
        ][!J(c("WinterDesignDay", "SummerDesignDay")), on = "day_type"]
    }

    # create target columns
    dict <- add_csv_variable(dict)

    # read csv header
    vars <- names(fread(csv, nrows = 0, sep = ","))

    # in case the *:MeterFileOnly
    if (length(vars) < nrow(dict)) dict <- dict[Variable %chin% vars]

    # get the interval type for target variables
    int_var <- get_sql_reporting_freq(dict$reporting_frequency)

    # subset time using input to get the target time index
    time_sub <- subset_sql_time(time_csv, year = year, tz = tz, period = period,
        month = month, day = day, hour = hour, minute = minute,
        interval = interval, simulation_days = simulation_days,
        day_type = day_type)
    subset_time <- attr(time_sub, "filtered") || subset_env

    # subset_sql_time will change column names to upper case
    setnames(time_csv, stri_trans_tolower(names(time_csv)))

    # get the row index
    if (!subset_time) {
        skip <- "__auto__"
        nrows <- Inf
    } else if (!nrow(time_sub)) {
        skip <- "__auto__"
        nrows <- 0
    } else {
        range <- range(time_sub$time_index)
        range_csv <- match(range, time_csv$time_index)
        # count from the header row which is 1
        skip <- range_csv[1L]
        nrows <- range_csv[2L] - skip + 1L
    }

    # get target csv column indices and names
    if (!subset_var) {
        select <- NULL
        col.names <- vars
    } else {
        # append "Date/Time"
        select <- which(vars %chin% c("Date/Time", dict$Variable))
        col.names <- vars[select]
    }

    if (!nrow(dict)) {
        data <- data.table("Date/Time" = character())
    } else {
        # read actual data
        if (skip == "__auto__" || length(unique(int_var)) == 1L) {
            data <- fread(csv, sep = ",", fill = TRUE, skip = skip, nrows = nrows,
                select = select, col.names = col.names)
        } else {
            first <- fread(csv, sep = ",", fill = TRUE, skip = skip, nrows = 1L, header = FALSE)

            if (ncol(first) == length(col.names)) {
                data <- fread(csv, sep = ",", fill = TRUE, skip = skip, nrows = nrows,
                    select = select, col.names = col.names)
            } else {
                data <- fread(csv, sep = ",", fill = TRUE, header = TRUE,
                    select = select, col.names = col.names)
                set(data, NULL, "time_index", time_index_all)
                data <- data[J(time_sub$time_index), on = "time_index"]
            }
        }
    }

    # subet using time index
    if (!subset_time) {
        set(data, NULL, "time_index", time_sub$time_index)
    } else if (!nrow(time_sub)) {
        set(data, NULL, "time_index", integer())
    } else {
        set(data, NULL, "time_index", time_csv$time_index[range_csv[[1L]]:range_csv[[2L]]])
        data <- data[J(time_sub$time_index), on = "time_index"]
    }

    subset_by_interval <- function (data, interval, timestep = NULL) {
        if (interval %in% RPFREQ["Annual"]) {
            data <- data[, .SD[.N], by = "environment_period_index"]
        } else if (interval %in% RPFREQ["RunPeriod"]) {
            data <- data[, .SD[.N], by = "environment_period_index"]
        } else if (interval == RPFREQ["Monthly"]) {
            data <- data[, .SD[.N], by = c("environment_period_index", "month")]
        } else if (interval == RPFREQ["Daily"]) {
            data <- data[J(24L, 0L), on = c("hour", "minute")]
        } else if (interval == RPFREQ["Hourly"]) {
            data <- data[J(0L), on = "minute"]
        } else if (interval == RPFREQ["TimeStep"]) {
            cols <- names(data)
            int <- as.integer(60 / timestep)
            tm <- data[, by = "environment_period_index", {
                r <- range(datetime)
                list(datetime = seq(r[[1L]], r[[2L]], by = paste(int, "mins")))
            }]
            data <- tm[data, on = c("environment_period_index", "datetime"), mult = "first", nomatch = NULL]
            setcolorder(data, cols)
        }
        data
    }

    # save to keep NAs
    if (wide) {
        if (length(int_var)) {
            int_var <- min(int_var)
            if (int != int_var) {
                time_sub <- time[J(int_var), on = "interval_type"]
                # should re-subset based on targeting environment
                time_sub <- time_sub[env, on = "environment_period_index", nomatch = NULL]

                if (subset_time) {
                    time_sub <- subset_sql_time(time_sub, year = year, tz = tz, period = period,
                        month = month, day = day, hour = hour, minute = minute,
                        interval = interval, simulation_days = simulation_days,
                        day_type = day_type)
                }

                data <- subset_by_interval(data, int_var)
                set(data, NULL, "time_index", time_sub$time_index)
            }
        }

        # use dict order
        setcolorder(data, c("Date/Time", dict$Variable))

        if (!all) {
            set(data, NULL, "time_index", NULL)
        } else {
            cols <- c("environment_period_index", "environment_name", "simulation_days",
                "datetime", "month", "day", "hour", "minute", "day_type"
            )
            add_joined_cols(time_sub, data, "time_index", cols)
            set(data, NULL, "time_index", NULL)
            setcolorder(data, cols)
        }

    # if data frequency is not the same as variable frequency, should re-subset
    } else {
        # remove 'Date/Time'
        set(data, NULL, "Date/Time", NULL)

        # should handle separately by each frequency
        set(dict, NULL, "interval_type", int_var)

        melt_data <- function (data, int_per, dict_per, time_per) {
            if (!nrow(time_per)) return(data.table(time_index = integer(), report_data_dictionary_index = integer(), value = double()))

            cols <- setdiff(names(data), setdiff(dict$Variable, dict_per$Variable))
            data <- subset_by_interval(fast_subset(data, cols), int_per, timestep)

            # remove time components after subsetting
            set(data, NULL, names(time_per), NULL)
            # update time index
            set(data, NULL, "time_index", time_per$time_index)

            # add report data dictionary index
            setnames(data, dict_per$Variable, as.character(dict_per$report_data_dictionary_index))

            data <- melt.data.table(data, id.vars = "time_index",
                measure.vars = as.character(dict_per$report_data_dictionary_index),
                variable.name = "report_data_dictionary_index", variable.factor = TRUE
            )

            # convert index to integer back
            set(data, NULL, "report_data_dictionary_index",
                .subset(
                    dict_per$report_data_dictionary_index,
                    as.integer(data$report_data_dictionary_index)
                )
            )
            data
        }

        if (!nrow(data)) {
            data <- data.table(time_index = integer(), report_data_dictionary_index = character(), value = double())
        } else {
            # add time column for further subsetting
            set(data, NULL, names(time_sub), time_sub)

            # get time table for each frequency
            l <- lapply(split(dict, by = "interval_type"), function (dict_per) {
                # for 'Each Call' there is no way to distinguish between Zone time
                # step and HVAC time step
                # see: https://github.com/NREL/EnergyPlus/issues/8227
                int_per <- dict_per$interval_type[[1L]]
                # same as the interval in the csv
                if (int_per == int) {
                    time_per <- time_sub
                    if (int_per != RPFREQ["Each Call"]) {
                        data <- melt_data(data, int_per, dict_per, time_per)
                    } else {
                        # manually separate variables output in zone and hvac
                        # timestep
                        is_zone <- vlapply(fast_subset(data, dict_per$Variable), anyNA)
                        time_zone <- subset_by_interval(time_per, RPFREQ["TimeStep"], timestep)
                        set(time_zone, NULL, "interval", as.integer(60 / timestep))
                        set(time_zone, NULL, "time_index", -time_zone$time_index)

                        data <- rbindlist(list(
                            melt_data(data, RPFREQ["TimeStep"], dict_per[is_zone][, interval_type := RPFREQ["TimeStep"]], time_zone),
                            melt_data(data, int_per, dict_per[!is_zone], time_per)
                        ))
                        time_per <- rbindlist(list(time_zone, time_per))
                    }
                } else {
                    time_int <- subset_by_interval(time_sub, int_per, timestep)

                    time_per <- time[J(int_per), on = "interval_type"]
                    # should re-subset based on targeting environment
                    time_per <- time_per[env, on = "environment_period_index", nomatch = NULL]
                    time_per <- subset_sql_time(time_per, year = year, tz = tz,
                        period = period, month = month, day = day, hour = hour,
                        minute = minute, interval = interval,
                        simulation_days = simulation_days, day_type = day_type,
                        datetime = FALSE)
                    # keep original datetime and day type
                    set(time_per, NULL, c("datetime", "day_type"), list(time_int$datetime, time_int$day_type))

                    set(data, NULL, "interval_type", int_per)

                    data <- melt_data(data, int_per, dict_per, time_per)
                }

                list(data = data, time = time_per)
            })

            data <- rbindlist(lapply(l, .subset2, "data"))
            time_sub <- rbindlist(lapply(l, .subset2, "time"))
        }

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

        add_joined_cols(time_sub, data, "time_index", cols_time)
        data[time_index < 0L, time_index := -time_index]

        add_joined_cols(dict, data, "report_data_dictionary_index", cols_dict)

        setorderv(data, c("report_data_dictionary_index", "time_index"))
        set(data, NULL, c("report_data_dictionary_index", "time_index"), NULL)
        setcolorder(data, c(cols_time, cols_dict))
    }

    data
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
