# from_eplus_time{{{
from_eplus_time <- function (x, year = lubridate::year(Sys.Date()), tz = Sys.timezone()) {
    assertthat::assert_that(assertthat::are_equal(class(x), "character"))
    x_trim <- stringr::str_trim(x, side = "both")
    x_with_year <- paste0(paste0(as.character(year), "/"), x_trim)
    x_parsed <- lubridate::ymd_hms(x_with_year, tz = tz)
    return(x_parsed)
}
# }}}

# to_eplus_time{{{
to_eplus_time <- function (x) {
    assertthat::assert_that(assertthat::is.time(x))
    x_date <- lubridate::date(x)
    x_std_time_str <- format(x, "%T")
    x_tbl <- dplyr::tibble(date = x_date, time_str = x_std_time_str)
    x_tbl %>% slice(n())
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
        diffs[diffs > (24*(31+28))] <- (diffs[diffs > (24*(31+28))] - 24L)
    }

    diff_hour <- as.numeric(diffs)

    return(diff_hour)
}
# }}}
