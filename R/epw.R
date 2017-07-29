# read_epw{{{
read_epw <- function(file) {
    epw_lines <- readr::read_lines(file)
    location <- read_epw_location(epw_lines)
    data <- read_epw_data(epw_lines)
    epw <- list(location = location,
                data = data)
    return(epw)
}
# }}}
# read_epw_data{{{
read_epw_data <- function (epw_lines) {
    data_regex <- stringr::regex("
        ^\\d{4},                   # year
        ([1-9]|1[0-2]),            # month
        ([1-9]|[1-2][0-9]|3[0-1]), # day
        ([0-9]|1[0-9]|2[0-4]),     # hour
        ([0-9]|5[0-9]),            # minute
        ", comments = TRUE
    )
    data_lines <- stringr::str_subset(epw_lines, data_regex)
    data_line <- paste0(data_lines, collapse = "\n")

    colnames <- c("year", "month", "day", "hour", "minute", "source", "drybulb",
                  "dewpoint", "relhum", "atmos_pressure", "ET_hor_rad",
                  "ET_dir_nor_rad", "horzirsky", "glohorrad", "dirnorrad",
                  "difhorrad", "glohorillum", "dir_nor_illu", "difhorillum",
                  "zen_lum", "winddir", "windspd", "totskycov", "opaqskycvr",
                  "visi", "ceilhgt", "weathobs", "weathcode", "precwa",
                  "aerooptdpt", "snowdpt", "daylassno", "albedo", "liq_pre_dpt",
                  "liq_pre_qua")
    data <- readr::read_csv(file = data_line, col_names = colnames, trim_ws = TRUE,
                           col_types = cols(.default = col_double(),
                                            year = col_integer(),
                                            month = col_integer(),
                                            day = col_integer(),
                                            hour = col_integer(),
                                            minute = col_integer(),
                                            source = col_character()))

    return(data)
}
# }}}
# read_epw_location{{{
read_epw_location <- function(epw_lines) {
    loc_regex <- stringr::regex("
        ^LOCATION,                                                 # location header
        (.+?),                                                     # city
        (.+?),                                                     # state/province
        (.+?),                                                     # country
        (.+),                                                      # WMO fields
        ([-+]?(?:[0-9]|[1-9][0-9]|1[0-7][0-9]|180)(?:\\.[0-9]+)*), # latitute (N+/S-)
        ([-+]?(?:[0-9]|[1-9][0-9]|1[0-7][0-9]|180)(?:\\.[0-9]+)*), # longitude (E+/W-)
        ([-+]?(?:[0-9]|1[0-2])\\.?0?),                             # timezone (+/-GMT)
        ([-+]?(?:\\d+\\.?\\d*))$                                   # elevation
        ", comments = TRUE)

    loc_line <- stringr::str_subset(epw_lines, loc_regex)
    loc_match <- as.character(stringr::str_match(loc_line, loc_regex))

    loc_header <- tibble(
        city = as.character(loc_match[2]),
        state = as.character(loc_match[3]),
        country = as.character(loc_match[4]),
        wmo_fields = as.character(loc_match[5]),
        latitude = as.double(loc_match[6]),
        longitute = as.double(loc_match[7]),
        timezone = as.integer(loc_match[8]),
        elevation = as.double(loc_match[9])
    )

    return(loc_header)
}
# }}}
