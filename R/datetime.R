#' Transform EnergyPlus DateTime format into POSIXct class.
#'
#' \code{time_from_eplus} takes an EnergyPlus DateTime format string, e.g.
#' " %m/%d  %H:%M:%d" into a POSIXct sequence in R.
#'
#' @param x An EnergyPlus DateTime format string vector.
#' @param year Year number.
#' @param tz Time zone of the result POSIXct sequence.
#' @return A POSIXct sequence.
#' @importFrom lubridate year ymd_hms tz leap_year
#' @importFrom stringr str_trim
#' @importFrom assertthat assert_that are_equal
#' @export
#'
# time_from_eplus{{{
time_from_eplus <- function (x, year = current_year(), tz = Sys.timezone()) {
    assertthat::assert_that(is.character(x))

    x_trim <- stringr::str_trim(x, side = "both")
    freq <- guess_freq(x_trim)
    if (freq == "date") {
        x_datetime <- paste0(paste0(as.character(year), "/"), x_trim, " 00:00:00")
    } else if (freq == "datetime") {
        x_datetime <- paste0(paste0(as.character(year), "/"), x_trim)
    } else if (freq == "monthly") {
        x_datetime <- paste0(paste0(as.character(year), "-"), x_trim, "-01 00:00:00")
    } else {
        warning(msg("No parsing has been done. Fail to parse EnergyPlus
                    DateTime format."), call. = FALSE)
        return(x)
    }

    x_parsed <- lubridate::ymd_hms(x_datetime, tz = tz)

    if (lubridate::leap_year(year)) {
        is_leap_datetime <-
            as.Date(x_parsed, tz = lubridate::tz(x_parsed)) ==
            as.Date(paste0(year, "-02-29"), tz = lubridate::tz(x_parsed))
        if (any(is_leap_datetime)) {
            x_parsed[is_leap_datetime] <- x_parsed[is_leap_datetime] + lubridate::days(1)
            warning("'year' is a leap year. All instances with Feb 29th will be replaced with Mar 1st.",
                    call. = FALSE)
        }
    }

    if (any(is.na(x_parsed))) {
        fails <- x[is.na(x_parsed)]
        ids <- which(is.na(x_parsed))
        warning("No parsing has been done. Fail to parse instances below:\n",
            msg("No. ", ids,": ", sQuote(fails)), call. = FALSE
        )
        return(x)
    }

    return(x_parsed)
}
# }}}

#' Transform a POSIXct object to EnergyPlus DateTime format string.
#'
#' \code{time_to_eplus} takes a POSIXct and formats it into a string with
#' EnergyPlus DateTime format, e.g." %m/%d  %H:%M:%d".
#'
#' @param x A POSIXct object.
#' @return A string vector
#' @importFrom lubridate date
#' @importFrom dplyr tibble mutate case_when transmute pull
#' @importFrom assertthat assert_that is.time
#' @importFrom lubridate days
#' @importFrom stringr str_c
#' @export
# time_to_eplus{{{
time_to_eplus <- function (x) {
    assertthat::assert_that(assertthat::is.time(x))
    # TODO: Add leap year checking
    x_date <- lubridate::date(x)
    x_std_time_str <- format(x, "%T")
    x_tbl <- dplyr::tibble(date = x_date, time_str = x_std_time_str)
    x_tbl_eptime <- dplyr::mutate(x_tbl,
                                  eptime_str = dplyr::case_when(time_str == "00:00:00" ~ "24:00:00",
                                                                TRUE                   ~ time_str),
                                  epdate = dplyr::case_when(time_str == "00:00:00" ~ date - lubridate::days(1),
                                                            TRUE                   ~ date))
    x_tbl_epdate <- dplyr::mutate(x_tbl_eptime, epdate_str = format(epdate, " %m/%d  "))
    x_tbl_ep_fmt <- dplyr::transmute(x_tbl_epdate, datetime = stringr::str_c(epdate_str, eptime_str))
    eplus_time <- dplyr::pull(x_tbl_ep_fmt, datetime)

    return(eplus_time)
}
# }}}

#' Get the hour number in a year.
#'
#' \code{yhour} returns the hour number in a year.
#'
#' @param x A datetime object.
#' @param one_year If TRUE, the calculation will based on the earliest year in
#' the input
#' @param no_leap If TRUE, and the year is a leap year, Feb 29th will be
#' neglected during the calculation.
#' @return A numeric vector indicates the hour number in the year.
#' @importFrom lubridate year tz leap_year
#' @importFrom assertthat assert_that is.time is.date
#' @export
# yhour {{{
yhour <- function (x, one_year = FALSE, no_leap = FALSE) {
    assertthat::assert_that(assertthat::is.time(x) || assertthat::is.date(x))

    if (one_year) {
        year <- min(lubridate::year(x))
    } else {
        year <- lubridate::year(x)
    }

    tz <- lubridate::tz(x)

    diffs <- difftime(x, as.POSIXct(paste0(year, "-01-01 00:00:00"), tz = tz),
                      units = "hour")

    if (no_leap && lubridate::leap_year(year)) {
        if (has_leap_day(x)) {
            stop("Input is a leap year and has a leap day. Ignore 'no_leap'.",
                 call. = FALSE)
        }
        diffs[diffs > (24 * (31 + 28))] <- (diffs[diffs > (24 * (31 + 28))] - 24L)
    }

    diff_hour <- as.numeric(diffs)

    return(diff_hour)
}
# }}}

#' Add a padding time column for further aggregation.
#'
#' \code{add_time} adds a padding time column in a data.frame for further easy
#' data aggregation. It can also replace the datetime that is (year+1)-01-01
#' with (year)-12-31.
#'
#' @param data A data frame containing at least one datetime column.
#' @param base A character indicates the name of datetime column.  If not given,
#' it will be auto-detected.
#' @param step The timestep of the added datetime variable, which should be
#' higher than the interval of the input datetime variable. If 'NULL', it will
#' be one level higher than the interval of the input datetime variable. It can
#' also be an integer indicates the interval of minutes. In conclusion,
#' 'interval' should be a number or one of c("year", "quarter", "month", "week",
#' "yday", "mday", "wday", "day", "date", "hour", "min", "second").
#' @param new The column name of the added variable. If 'NULL' it will be
#' the name of the original datetime variable with the interval name added to
#' it, separeted by an underscore.
#' @param one_year If TRUE, the (year+1)-01-01 will be replaced with
#' (year)-12-31.
#' @importFrom lubridate ceiling_date floor_date round_date
#' @importFrom stringr str_replace_all
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate
#' @export
# add_time
# add_time{{{1
add_time <- function (data, base = NULL, new = NULL, step,
                      toward = c("up", "down", "center"),
                      no_leap = FALSE) {

    # TODO: Add checking for invalid step such as '600 secs'.
    assertthat::assert_that(is.data.frame(data))
    is_tbl <- tibble::is_tibble(data)
    is_dt <- data.table::is.data.table(data)

    if (is.null(base)) {
        base <- check_date_col(data)
    }
    datetimes <- data[[base]]

    if (is.null(new)) {
        new <- paste0(base, "_", stringr::str_replace_all(step, "\\s", ""))
    } else {
        new <- stringr::str_replace_all(new, "\\s", "")
    }

    toward <- rlang::arg_match(toward)

    data_thicken <-
        switch(toward,
               up = dplyr::mutate(data, rlang::UQ(new) := lubridate::ceiling_date(datetimes, unit = step)),
               down = dplyr::mutate(data, rlang::UQ(new) := lubridate::floor_date(datetimes, unit = step)),
               center = dplyr::mutate(data, rlang::UQ(new) := lubridate::round_date(datetimes, unit = step))
               )

    if (no_leap) {
        datetime_thicken <- data_thicken[[new]]
        is_leap_day <- is_leap_day(datetime_thicken)
        datetime_thicken[is_leap_day] <- datetime_thicken[is_leap_day] + lubridate::days(1)
        data_thicken[[new]] <- datetime_thicken
    }

    if (identical(new, base)) {
        message(msg("Column '", new, "' has been replaced with new timestep of '",
                 step, "'."))
    } else {
        message(msg("A new column named '", new, "' with step '", step,
                 "' has been added based on column '", base, "'."))
    }

    if (is_tbl) {
        data_thicken <- dplyr::as_tibble(data_thicken)
    }
    if (is_dt) {
        data_thicken <- data.table::as.data.table(data_thicken)
    }

    return(data_thicken)
}
# }}}1

# has_leap_day {{{
has_leap_day <- function (x) {
    is_leap_day <- is_leap_day(x)

    if (any(is_leap_day)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}

# is_leap_day {{{
is_leap_day <- function (x) {
    assertthat::assert_that(assertthat::is.time(x) || assertthat::is.date(x))

    is_leap_year <- lubridate::leap_year(x)

    if (all(!is_leap_year)) {
        return(rep(FALSE, length(x)))
    } else {
        x_leap_year <- x[is_leap_year]
    }

    tz <- lubridate::tz(x_leap_year)

    leap_day <- as.Date(paste0(lubridate::year(x_leap_year), "-02-29"), tz = tz)
    x_day <- as.Date(x_leap_year, tz = tz)

    is_leap_day <- x_day == leap_day

    results <- logical(length(x))

    results[!is_leap_year] <- FALSE
    results[is_leap_year] <- is_leap_day

    return(results)
}
# }}}

# guess_freq {{{1
guess_freq <- function (x, n_max = 10L) {
    # Use first ten instances to guess output freq
    x_10 <- na.omit(x[1:n_max])
    regex_daily <- "^\\s*(0[1-9]|1[0-2])/(0[1-9]|[1-2][0-9]|3[0-1])$"
    regex_datetime <- "^\\s*(0[1-9]|1[0-2])/(0[1-9]|[1-2][0-9]|3[0-1])  (0[0-9]|1[0-9]|2[0-4]):(0[0-9]|[1-5][0-9]):(0[0-9]|[1-5][0-9])$"

    if (all(grepl(regex_daily, x_10, perl = TRUE))) {
        freq <- "date"
    } else if (all(grepl(regex_datetime, x_10, perl = TRUE))) {
        freq <- "datetime"
    } else if (all(!is.na(match(x_10, base::month.name)))) {
        freq <- "monthly"
    } else {
        freq <- "unknown"
    }

    return(freq)
}
# }}}1
