# read_epw{{{
read_epw <- function(file) {
    epw_lines <- readr::read_lines(file)
    location <- read_epw_location(epw_lines)
    data <- read_epw_data(epw_lines)

    loc_idx <- attr(location, "line_idx")
    data_idx <- attr(data, "line_idx")
    unknown_idx <- setdiff(seq_along(epw_lines), c(loc_idx, data_idx))

    unknown_lines <- epw_lines[unknown_idx]
    attr(unknown_lines, "line_idx") <- unknown_idx
    attr(unknown_lines, "prefix") <- NULL

    epw <- list(location = location, data = data, unknown = unknown_lines)
    class(epw) <- "epw"
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
        ([0-9]|[1-5][0-9]),        # minute
        ", comments = TRUE
    )
    data_line_idx <- stringr::str_which(epw_lines, data_regex)
    data_lines <- epw_lines[data_line_idx]
    data_line <- paste0(data_lines, collapse = "\n")

    colnames <- c("year", "month", "day", "hour", "minute", "source", "dry_bulb",
                  "dew_point", "rel_hum", "atmos_pressure", "ET_hor_rad",
                  "ET_dir_nor_rad", "hor_zir_sky", "glo_hor_rad", "dir_nor_rad",
                  "dif_hor_rad", "glo_hor_illum", "dir_nor_illu", "dif_hor_illum",
                  "zen_lum", "wind_dir", "wind_spd", "tot_sky_cvr", "opaq_sky_cvr",
                  "vis", "ceil_hgt", "weath_obs", "weath_code", "prec_wat",
                  "aero_opt_dpt", "snow_dpt", "day_last_snow", "albedo", "liq_pre_dpt",
                  "liq_pre_qua")
    data <- readr::read_csv(file = data_line, col_names = colnames, trim_ws = TRUE,
                           col_types = readr::cols(
                                .default = readr::col_double(),
                                year = readr::col_integer(),
                                month = readr::col_integer(),
                                day = readr::col_integer(),
                                hour = readr::col_integer(),
                                minute = readr::col_integer(),
                                source = readr::col_character()))

    attr(data, "line_idx") <- data_line_idx
    attr(data, "prefix") <- NULL
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

    loc_line_idx <- stringr::str_which(epw_lines, loc_regex)
    loc_line <- epw_lines[loc_line_idx]
    loc_match <- as.character(stringr::str_match(loc_line, loc_regex))

    loc_header <- dplyr::tibble(
        city = as.character(loc_match[2]),
        state = as.character(loc_match[3]),
        country = as.character(loc_match[4]),
        wmo_fields = as.character(loc_match[5]),
        latitude = as.double(loc_match[6]),
        longitute = as.double(loc_match[7]),
        timezone = as.integer(loc_match[8]),
        elevation = as.double(loc_match[9])
    )

    attr(loc_header, "line_idx") <- loc_line_idx
    attr(loc_header, "prefix") <- "LOCATION"
    return(loc_header)
}
# }}}
# write_epw {{{
write_epw <- function (epw, path) {
    assertthat::assert_that(inherits(epw, "epw"),
                            msg = "Input should be an 'epw' object.")

    scipen_ori <- getOption("scipen")
    options(scipen = 999)
    epw_tbl <- purrr::map_df(epw, ~{
        if (!is.na(match("data.frame", class(.x)))) {
            chr <- tbl_to_chr(.x)
        } else {
            chr <- .x
        }
        prefix <- attr(.x, "prefix")
        if (!is.null(prefix)) {
            chr <- paste(prefix, chr, sep = ",")
        }
        row_num <- attr(.x, "line_idx")
        dplyr::tibble(row_num = row_num, line = chr)
    })

    epw_line <- dplyr::pull(dplyr::arrange(epw_tbl, row_num), line)

    readr::write_lines(epw_line, path)

    return(invisible())
}
# }}}
# tbl_to_chr {{{
tbl_to_chr <- function (tbl) {
    temp_csv <- tempfile(fileext = ".csv")
    # As `write_csv` in `readr`will automatically use scitific notaion when
    # saving, `fwrite` from `data.table` package will be used. Details can be
    # found in this PR:
    # https://github.com/tidyverse/readr/pull/679
    # And also, as there is a 'quote' option in `fwrite`, there is no need to
    # used `str_replace_all` to delte double quotes.
    data.table::fwrite(tbl, temp_csv, quote = FALSE, col.names = FALSE)
    chr <- readr::read_lines(temp_csv)
    # chr <- stringr::str_replace_all(chr, '"', "")
    return(chr)
}
# }}}
# print.epw {{{
print.epw <- function (epw) {
    city <- tidyr::unite(epw$location, city, city, state, country, sep = ", ")$city
    cat("Location:", city, "\n")
    epw$data
}
# }}}
