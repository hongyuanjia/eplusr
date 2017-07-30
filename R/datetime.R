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
time_from_eplus <- function (x, year = lubridate::year(Sys.Date()), tz = Sys.timezone()) {
    assertthat::assert_that(assertthat::are_equal(class(x), "character"))

    x_trim <- stringr::str_trim(x, side = "both")
    x_with_year <- paste0(paste0(as.character(year), "/"), x_trim)
    x_parsed <- lubridate::ymd_hms(x_with_year, tz = tz)

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

# has_leap_day {{{
has_leap_day <- function (x) {
    assertthat::assert_that(assertthat::is.time(x) || assertthat::is.date(x))

    is_leap_year <- lubridate::leap_year(x)
    if (!all(is_leap_year)) {
        return(FALSE)
    } else {
        x_leap_year <- x[is_leap_year]
    }

    years <- unique(lubridate::year(x_leap_year))

    is_leap_day <-
        as.Date(x, tz = lubridate::tz(x)) ==
        as.Date(paste0(year, "-02-29"), tz = lubridate::tz(x))

    if (any(is_leap_day)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}
