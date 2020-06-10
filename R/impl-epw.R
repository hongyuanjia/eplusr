#' @importFrom stringi stri_isempty stri_match_first_regex stri_replace_all_charclass
#' @importFrom stringi stri_split_charclass stri_split_fixed stri_sub "stri_sub<-"
#' @importFrom stringi stri_trans_tolower stri_trans_totitle stri_trans_toupper
#' @importFrom lubridate as_datetime make_date make_datetime force_tz tz
#' @importFrom lubridate days_in_month minutes hours days years leap_year
#' @importFrom lubridate mday month yday year "year<-" parse_date_time
#' @importFrom data.table "%chin%" chmatch copy fread fwrite as.data.table
#' @importFrom data.table data.table melt.data.table set setattr setcolorder
#' @importFrom data.table setnames setorderv
#' @importFrom units set_units drop_units
#' @importFrom cli cat_rule cat_line
#' @importFrom utils combn
#' @importFrom stats na.omit
#' @importFrom methods setOldClass
#' @include parse.R
#' @include utils.R
#' @include assertions.R

# HELPER
# abort_bad_epw_period {{{
abort_bad_epw_period <- function (period, n) {
    abort("error_invalid_data_period_index",
        paste0("Invalid data period index found. EPW contains only ", n,
            " data period(s) but ", collapse(period), " is specified."
        ),
        period = period
    )
}
# }}}
# combine_date {{{
combine_date <- function (year = NULL, month, day, hour) {
    y <- if (!is.null(year)) paste0(year, "/") else ""
    paste0(y, month, "/", day, " ", lpad(hour, "0", 2L), ":XX")
}
# }}}
# std_atm_press {{{
std_atm_press <- function (elevation) 101325 * (1 - 2.25577e-05 * elevation)^5.2559
# }}}
# as_date {{{
as_date <- function (x, ...) {
    as.Date(lubridate::as_date(x, ...))
}
# }}}

# CONSTANTS
# EPW_HEADER {{{
EPW_HEADER <- list(
    location = "LOCATION",
    design = "DESIGN CONDITIONS",
    typical = "TYPICAL/EXTREME PERIODS",
    ground = "GROUND TEMPERATURES",
    holiday = "HOLIDAYS/DAYLIGHT SAVINGS",
    comment1 = "COMMENTS 1",
    comment2 = "COMMENTS 2",
    period = "DATA PERIODS"
)
# }}}
# EPW_UNIT {{{
EPW_UNIT <- list(
    dry_bulb_temperature = "degC",
    dew_point_temperature = "degC",
    relative_humidity = "%",
    atmospheric_pressure = "Pa",
    extraterrestrial_horizontal_radiation = "W*h/m^2",
    extraterrestrial_direct_normal_radiation = "W*h/m^2",
    horizontal_infrared_radiation_intensity_from_sky = "W*h/m^2",
    global_horizontal_radiation = "W*h/m^2",
    direct_normal_radiation = "W*h/m^2",
    diffuse_horizontal_radiation = "W*h/m^2",
    global_horizontal_illuminance = "lux",
    direct_normal_illuminance = "lux",
    diffuse_horizontal_illuminance = "lux",
    zenith_luminance = "lux",
    wind_direction = "degree",
    wind_speed = "m/s",
    visibility = "km",
    ceiling_height = "m",
    precipitable_water = "mm",
    snow_depth = "cm",
    days_since_last_snow = "day",
    liquid_precip_depth = "mm",
    liquid_precip_rate = "hour"
)
# }}}
# EPW_TYPE {{{
EPW_TYPE <- list(
    year                                             = "integer",
    month                                            = "integer",
    day                                              = "integer",
    hour                                             = "integer",
    minute                                           = "integer",
    datasource                                       = "character",
    dry_bulb_temperature                             = "double",
    dew_point_temperature                            = "double",
    relative_humidity                                = "double",
    atmospheric_pressure                             = "double",
    extraterrestrial_horizontal_radiation            = "double",
    extraterrestrial_direct_normal_radiation         = "double",
    horizontal_infrared_radiation_intensity_from_sky = "double",
    global_horizontal_radiation                      = "double",
    direct_normal_radiation                          = "double",
    diffuse_horizontal_radiation                     = "double",
    global_horizontal_illuminance                    = "double",
    direct_normal_illuminance                        = "double",
    diffuse_horizontal_illuminance                   = "double",
    zenith_luminance                                 = "double",
    wind_direction                                   = "double",
    wind_speed                                       = "double",
    total_sky_cover                                  = "integer",
    opaque_sky_cover                                 = "integer",
    visibility                                       = "double",
    ceiling_height                                   = "double",
    present_weather_observation                      = "integer",
    present_weather_codes                            = "character",
    precipitable_water                               = "double",
    aerosol_optical_depth                            = "double",
    snow_depth                                       = "double",
    days_since_last_snow                             = "integer",
    albedo                                           = "double",
    liquid_precip_depth                              = "double",
    liquid_precip_rate                               = "double"
)
# }}}
# EPW_FORMAT {{{
EPW_FORMAT <- list(
    dry_bulb_temperature = fmt_int,
    dew_point_temperature = fmt_int,
    relative_humidity = as.integer,
    atmospheric_pressure = as.integer,
    extraterrestrial_horizontal_radiation = as.integer,
    extraterrestrial_direct_normal_radiation = as.integer,
    horizontal_infrared_radiation_intensity_from_sky = as.integer,
    global_horizontal_radiation = as.integer,
    direct_normal_radiation = as.integer,
    diffuse_horizontal_radiation = as.integer,
    global_horizontal_illuminance = as.integer,
    direct_normal_illuminance = as.integer,
    diffuse_horizontal_illuminance = as.integer,
    zenith_luminance = as.integer,
    wind_direction = as.integer,
    wind_speed = fmt_int,
    visibility = fmt_int,
    ceiling_height = as.integer,
    precipitable_water = as.integer,
    aerosol_optical_depth = function (x) fmt_dbl(x, 4L),
    snow_depth = as.integer,
    albedo = function (x) fmt_dbl(x, 3L),
    liquid_precip_depth = fmt_int,
    liquid_precip_rate = fmt_int
)
# }}}
# EPW_MISSING_CODE {{{
EPW_MISSING_CODE <- list(
    dry_bulb_temperature = 99.9,
    dew_point_temperature = 99.9,
    relative_humidity = 999,
    atmospheric_pressure = 999999,
    extraterrestrial_horizontal_radiation = 9999,
    extraterrestrial_direct_normal_radiation = 9999,
    horizontal_infrared_radiation_intensity_from_sky = 9999,
    global_horizontal_radiation = 9999,
    direct_normal_radiation = 999999,
    diffuse_horizontal_radiation = 9999,
    global_horizontal_illuminance = 999999,
    direct_normal_illuminance = 999999,
    diffuse_horizontal_illuminance = 999999,
    zenith_luminance = 99990,
    wind_direction = 999,
    wind_speed = 999,
    total_sky_cover = 99,
    opaque_sky_cover = 99,
    visibility = 9999,
    ceiling_height = 99999,
    present_weather_observation = 9L,
    present_weather_codes = "999999999",
    precipitable_water = 999,
    aerosol_optical_depth = 0.999,
    snow_depth = 999,
    days_since_last_snow = 99,
    albedo = 999,
    liquid_precip_depth = 999,
    liquid_precip_rate = 99
)
# }}}
# EPW_INIT_MISSING {{{
EPW_INIT_MISSING <- list(
    dry_bulb_temperature = 6.0,
    dew_point_temperature = 3.0,
    relative_humidity = 50.0,
    wind_speed = 2.5,
    wind_direction = 180,
    total_sky_cover = 5L,
    opaque_sky_cover = 5L,
    visibility = 777.7,
    ceiling = 77777,
    precipitable_water = 0,
    aerosol_optical_depth = 0,
    snow_depth = 0,
    days_since_last_snow = 88L,
    albedo = 0.0,
    liquid_precip_depth = 0
)
# }}}
# EPW_RANGE_EXIST {{{
EPW_RANGE_EXIST <- list(
    # special
    wind_direction = ranger(0, TRUE, 360, TRUE), # SPECIAL, value greater than MISSING_CODE will be treated as missing
    present_weather_observation = ranger(0, TRUE, 9, TRUE), # SPECIAL, negative value will be replaced with 9
    # missing = var >= MISSING_CODE
    dry_bulb_temperature = ranger(-Inf, FALSE, EPW_MISSING_CODE$dry_bulb_temperature, FALSE),
    dew_point_temperature = ranger(-Inf, FALSE, EPW_MISSING_CODE$dew_point_temperature, FALSE),
    # missing = ! var %in% [0, MISSING_CODE)
    relative_humidity = ranger(0, TRUE, EPW_MISSING_CODE$relative_humidity, FALSE),
    atmospheric_pressure = ranger(0, TRUE, EPW_MISSING_CODE$atmospheric_pressure, FALSE),
    global_horizontal_illuminance = ranger(0, TRUE, EPW_MISSING_CODE$global_horizontal_illuminance, FALSE),
    direct_normal_illuminance = ranger(0, TRUE, EPW_MISSING_CODE$direct_normal_illuminance, FALSE),
    diffuse_horizontal_illuminance = ranger(0, TRUE, EPW_MISSING_CODE$diffuse_horizontal_illuminance, FALSE),
    wind_speed = ranger(0, TRUE, EPW_MISSING_CODE$wind_speed, FALSE),
    total_sky_cover = ranger(0, TRUE, EPW_MISSING_CODE$total_sky_cover, TRUE),
    opaque_sky_cover = ranger(0, TRUE, EPW_MISSING_CODE$opaque_sky_cover, TRUE),
    extraterrestrial_horizontal_radiation = ranger(0, TRUE, EPW_MISSING_CODE$extraterrestrial_horizontal_radiation, FALSE),
    extraterrestrial_direct_normal_radiation = ranger(0, TRUE, EPW_MISSING_CODE$extraterrestrial_direct_normal_radiation, FALSE),
    horizontal_infrared_radiation_intensity_from_sky = ranger(0, TRUE, EPW_MISSING_CODE$horizontal_infrared_radiation_intensity_from_sky, FALSE),
    global_horizontal_radiation = ranger(0, TRUE, EPW_MISSING_CODE$global_horizontal_radiation, FALSE),
    direct_normal_radiation = ranger(0, TRUE, EPW_MISSING_CODE$direct_normal_radiation, FALSE),
    diffuse_horizontal_radiation = ranger(0, TRUE, EPW_MISSING_CODE$diffuse_horizontal_radiation, FALSE),
    zenith_luminance = ranger(0, TRUE, EPW_MISSING_CODE$zenith_luminance, FALSE),
    visibility = ranger(0, TRUE, EPW_MISSING_CODE$visibility, FALSE),
    ceiling_height = ranger(0, TRUE, EPW_MISSING_CODE$ceiling_height, FALSE),
    precipitable_water = ranger(0, TRUE, EPW_MISSING_CODE$precipitable_water, FALSE),
    aerosol_optical_depth = ranger(0, TRUE, EPW_MISSING_CODE$aerosol_optical_depth, FALSE),
    snow_depth = ranger(0, TRUE, EPW_MISSING_CODE$snow_depth, FALSE),
    days_since_last_snow = ranger(0, TRUE, EPW_MISSING_CODE$days_since_last_snow, FALSE),
    albedo = ranger(0, TRUE, EPW_MISSING_CODE$albedo, FALSE),
    liquid_precip_depth = ranger(0, TRUE, EPW_MISSING_CODE$liquid_precip_depth, FALSE),
    liquid_precip_rate = ranger(0, TRUE, EPW_MISSING_CODE$liquid_precip_rate, FALSE)
)
# }}}
# EPW_RANGE_VALID {{{
EPW_RANGE_VALID <- list(
    # missing = var >= MISSING_CODE
    dry_bulb_temperature = ranger(-90, TRUE, 70, TRUE),
    dew_point_temperature = ranger(-90, TRUE, 70, TRUE),
    # missing = ! var %in% [0, MISSING_CODE)
    relative_humidity = ranger(0, TRUE, 110, TRUE),
    atmospheric_pressure = ranger(31000, FALSE, 120000, TRUE),
    global_horizontal_illuminance = ranger(0, TRUE, 999900, FALSE),
    direct_normal_illuminance = ranger(0, TRUE, 999900, FALSE),
    diffuse_horizontal_illuminance = ranger(0, TRUE, 999900, FALSE),
    wind_direction = ranger(0, TRUE, 360, TRUE),
    wind_speed = ranger(0, TRUE, 40, TRUE),
    total_sky_cover = ranger(0, TRUE, 10, TRUE),
    opaque_sky_cover = ranger(0, TRUE, 10, TRUE),
    present_weather_observation = ranger(0, TRUE, 9, TRUE),
    extraterrestrial_horizontal_radiation = ranger(0, TRUE, EPW_MISSING_CODE$extraterrestrial_horizontal_radiation, FALSE),
    extraterrestrial_direct_normal_radiation = ranger(0, TRUE, EPW_MISSING_CODE$extraterrestrial_direct_normal_radiation, FALSE),
    horizontal_infrared_radiation_intensity_from_sky = ranger(0, TRUE, EPW_MISSING_CODE$horizontal_infrared_radiation_intensity_from_sky, FALSE),
    global_horizontal_radiation = ranger(0, TRUE, EPW_MISSING_CODE$global_horizontal_radiation, FALSE),
    direct_normal_radiation = ranger(0, TRUE, EPW_MISSING_CODE$direct_normal_radiation, FALSE),
    diffuse_horizontal_radiation = ranger(0, TRUE, EPW_MISSING_CODE$diffuse_horizontal_radiation, FALSE),
    zenith_luminance = ranger(0, TRUE, EPW_MISSING_CODE$zenith_luminance, FALSE),
    visibility = ranger(0, TRUE, EPW_MISSING_CODE$visibility, FALSE),
    ceiling_height = ranger(0, TRUE, EPW_MISSING_CODE$ceiling_height, FALSE),
    precipitable_water = ranger(0, TRUE, EPW_MISSING_CODE$precipitable_water, FALSE),
    aerosol_optical_depth = ranger(0, TRUE, EPW_MISSING_CODE$aerosol_optical_depth, FALSE),
    snow_depth = ranger(0, TRUE, EPW_MISSING_CODE$snow_depth, FALSE),
    days_since_last_snow = ranger(0, TRUE, EPW_MISSING_CODE$days_since_last_snow, FALSE),
    albedo = ranger(0, TRUE, EPW_MISSING_CODE$albedo, FALSE),
    liquid_precip_depth = ranger(0, TRUE, EPW_MISSING_CODE$liquid_precip_depth, FALSE),
    liquid_precip_rate = ranger(0, TRUE, EPW_MISSING_CODE$liquid_precip_rate, FALSE)
)
# }}}
# EPW_REPORT_MISSING {{{
EPW_REPORT_MISSING <- list(
    # In EnergyPlus, the first missing values of variables will be set to the
    # initial missing values. All after are set to privious valid one.
    use_previous = c(
        "dry_bulb_temperature",
        "atmospheric_pressure",
        "relative_humidity",
        "dew_point_temperature",
        "wind_speed",
        "wind_direction",
        "total_sky_cover",
        "opaque_sky_cover",
        "snow_depth",
        "liquid_precip_depth"
    ),
    use_zero = c(
        "direct_normal_radiation",
        "diffuse_horizontal_radiation"
    )
    # NOT REPORT IN ENERGYPLUS
    # Missing values of variables below are neither reported nor handled, as
    # they are currently not used in EnergyPlus. But in WeatherConvertor,
    # missing values are replaced with previous valid values. For speed concern,
    # here do nothing to them just as EnergyPlus does.
    # 1. "visibility"
    # 2. "aerosol_optical_depth"
    # 3. "ceiling_height"
    # 4. "precipitable_water"
    # 5. "albedo"
    # 6. "days_since_last_snow"
    #
    # NOT REPORT IN EPLUSR
    # Unlike in EnergyPlus, missing and abnormal values of present weather codes
    # have been handled during data reading in read_epw_data(), so no warnings
    # will be given
    # "present_weather_codes"
)
# }}}
# EPW_REPORT_RANGE {{{
EPW_REPORT_RANGE <- list(
    do_nothing = c(
        "dry_bulb_temperature",
        "relative_humidity",
        "dew_point_temperature",
        "wind_direction",
        "wind_speed"
    ),
    # In EnergyPlus, out of range values of variables below are treated in the
    # same way as missing, i.e., set to 0
    use_zero = c(
        "direct_normal_radiation",
        "diffuse_horizontal_radiation"
    ),
    # In EnergyPlus, out of range values of variables below are treated in the
    # same way as missing, i.e., set to 0
    # Note in EnergyPlus, for wind direction, firstly values < -360 or values >
    # 360 are set to missing code, and replaced with previous valid values; then
    # values in [-360, 0) are treated as out of ranges with warnings. Currently,
    # I have no idea how EnergyPlus will treat those nagitive wind direction
    # values. Here values out of range [0, 360] are treated as missing and there
    # will be no out of range values.
    use_previous = c(
        "atmospheric_pressure"
    )
)
# }}}
# EPWDATE_TYPE {{{
EPWDATE_TYPE <- list(invalid = -1L, zero = 0L, jul = 1L, md = 2L, ymd = 3L, nth = 4L, last = 5L)
# }}}
# EPWDATE_YEAR {{{
EPWDATE_YEAR <- list(
    # type:       0,         1,        2,       4,         5
      leap = list(zero = 0L, jul = 4L, md = 8L, nth = 12L, last = 16L),
    noleap = list(zero = 1L, jul = 5L, md = 9L, nth = 13L, last = 17L)
)
# }}}

# PARSE
# parse_epw_file {{{
# (a) no match checking will be made between the year in data periods and the
#     year column
# (b) the day order does not matter, as long as all data in one day stay
#     together
# (c) headers should come in order, but after one has be parsed, the
#     duplications will be ignored. the only exception is "DATA PERIODS".
#     EnergyPlus will try to read weather data just after that header
# (d) EnergyPlus uses "DATA PERIODS" header to check if run periods specified in
#     IDF exist in the weather file. If no, terminate; if yes, then try to read
#     corresponding data specified in run priods. i.e., there can be only one
#     day data in the weather file but with a "DATA PERIODS" header saying that
#     this is a whole year weather data, as long as run period in IDF only
#     request that day.
#
# Procedures of reading weather data in energyplus
#
# 1. Locate the first line of request date, e.g. Jan 02
# 2. Start from hour 01 (01:00:00), for each interval, read weather data, and
#    make sure hour is the same, e.g. Jan 02 01:00:00, Jan 02 01:30:00. This
#    means that there is no difference in EnergyPlus between these two time
#    sequences:
#    *. Jan 02 01:00, Jan 02 01:30
#    *. Jan 02 01:30, Jan 02 01:60
#
#    Only the number after the first occurrence of that hour counts. Even though
#    the "Minute" column is read and stored, EnergyPlus never use that data.
#
# 3. The Hour column in the core weather data corresponds the period from
#    (Hour-1)th to (Hour)th. For instance, if the number of interval per hour is
#    1, Hour of 1 on a certain day corresponds to the period between 00:00:01 to
#    01:00:00, Hour of 2 corresponds to the period between 01:00:01 to 02:00:00,
#    and etc. The minute column is **not used** to determine currently sub-hour
#    time.

parse_epw_file <- function (path, warning = FALSE) {
    # read and parse header
    epw_header <- parse_epw_header(read_epw_header(path), warning = warning)

    # read core weather data
    epw_data <- read_epw_data(path)

    # parse date time
    # first ignore minute column here
    epw_data[, datetime := lubridate::make_datetime(year, month, day, hour, tz = "UTC")]

    # make sure that each data period exists and get corresponding line number
    epw_header$period$period[, c("from", "to", "missing", "out_of_range") :=
        match_epw_data_period(epw_data, .SD,
            epw_header$period$interval,
            epw_header$holiday$leapyear, warning),
        by = index
    ]

    # stop if there are overlaps in multiple data periods {{{
    if (nrow(epw_header$period$period) > 1L) {
        p <- epw_header$period$period
        comb <- utils::combn(p$index, 2L, simplify = FALSE)
        for (i in comb) {
            int <- intersect(seq(p$from[i[1L]], p$to[i[1L]]), seq(p$from[i[2L]], p$to[i[2L]]))
            if (length(int)) {
                parse_issue("error_epw_data_period_overlapped", "epw",
                    "Overlapping in data periods found",
                    data = data.table(line = 8L, string = paste(EPW_HEADER$period, format_epw_header_period(epw_header$period), sep = ",")),
                    post = paste0("Each data period should not have overlapped period with others. ",
                        "Data period #", i[1L], " ", surround(p$name[i[1L]]), " overlaps with ",
                        "data period #", i[2L], " ", surround(p$name[i[2L]]), "."
                    )
                )
            }
        }
    }
    # }}}

    # warning if redundant lines found
    if (warning) {
        redundant <- get_epw_data_redundant_line(epw_data, epw_header, simplify = TRUE)
        if (length(redundant)) {
            all <- nrow(epw_data)
            unused <- round(length(redundant)/all, 4L)
            parse_issue("warning_redundant_epw_data", "epw",
                "Redundant weather data found", num = length(redundant),
                post = paste0(
                    "All data periods only cover ",
                    all - length(redundant), " rows (", round(100 * (1 - unused), 2L), " %",
                    ") of weather data, leaving ", length(redundant), " rows (", 100 * unused, " %",
                    ") unused."
                ),
                stop = FALSE
            )
        }
    }

    # clean and set column order
    set(epw_data, NULL, "line", NULL)
    setcolorder(epw_data, c("datetime", setdiff(names(epw_data), "datetime")))

    list(header = epw_header, data = epw_data)
}
# }}}
## header
# read_epw_header {{{
read_epw_header <- function (path) {
    num_header <- 8L

    # HEADER
    # header dict {{{
    dict_header <- data.table(
        name = unlist(EPW_HEADER, use.names = FALSE),
        # currently, only location, holidays and data periods are parsed
        parse = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
    )
    # }}}

    # read header {{{
    header <- read_lines(path, nrows = num_header)

    # split header lines using comma
    set(header, NULL, "contents", stri_split_fixed(header$string, ","))
    # get header name
    set(header, NULL, "name", stri_trans_toupper(vapply(header$contents, "[[", character(1L), 1L)))

    header <- merge(dict_header, header, by = "name", all = TRUE, sort = FALSE)

    if (any(is.na(header$line))) {
        mis <- header[is.na(line), name]
        parse_issue("error_miss_epw_header", "epw",
            paste0("Missing required header data ", collapse(mis)), num = length(mis)
        )
    }

    if (any(is.na(header$parse))) {
        parse_issue("error_invalid_epw_header", "epw", "Invalid header name ", header[is.na(parse)])
    }

    # header should come in order
    # Reference: EnergyPlus/WeatherManager.cc
    if (!identical(header$line, 1L:8L)) {
        parse_issue("error_invalid_epw_header_order", "epw", "Invalid header order",
            header[order(line)],
            post = paste0("EPW header should come in order: ", collapse(dict_header$name))
        )
    }
    # }}}

    set(header, NULL, "parse", NULL)
    setcolorder(header, c("line", "name", "contents", "string"))
    header
}
# }}}
# parse_epw_header {{{
parse_epw_header <- function (header, warning = FALSE) {
    epw_header <- EPW_HEADER
    for (i in seq_along(epw_header)) {
        fun <- match.fun(paste0("parse_epw_header_", names(epw_header)[i]))
        epw_header[[i]] <- fun(input = header[i, contents][[1L]], data = header[i], warning = warning)
    }

    # update EpwDate year according to leapyear element in HOLIDAYS header
    set(epw_header$period$period, NULL, c("start_day", "end_day"),
        list(reset_epwdate_year(epw_header$period$period$start_day, epw_header$holiday$leapyear),
             reset_epwdate_year(epw_header$period$period$end_day, epw_header$holiday$leapyear)
        )
    )

    # check if leap day is found in period but leap year is not allowed in the header {{{
    if (!epw_header$holiday$leapyear &&
        any(format(as.Date.EpwDate(epw_header$period$period$start_day), "%m-%d") == "02-29" |
            format(as.Date.EpwDate(epw_header$period$period$end_day), "%m-%d") == "02-29"
        )
    ) {
        parse_issue("error_invalid_epw_data_period_leapday", "epw",
            "Invalid start/end day of data period found",
            data = header[name == EPW_HEADER$period],
            post = paste0("EPW file header ", surround(EPW_HEADER$holiday),
                " indicates no leap year but start/end day on Feb 29 found in",
                "header", surround(EPW_HEADER$period), "."
            )
        )
    }
    # }}}

    epw_header
}
# }}}
# parse_epw_header_basic {{{
parse_epw_header_basic <- function (
    header_type, input,
    len = NULL, name = NULL, type = NULL, range = NULL, raw = TRUE, coerce = TRUE, strict = TRUE, ...
) {

    # convert to a list if necessary
    if (!is.list(input)) input <- as.list(input)

    # set names
    if (!is.null(name)) {
        if (length(input) < length(name)) {
            name <- name[1:length(input)]
        }
        setattr(input, "names", name)
    }

    # check length {{{
    # use length of names as expect minimum length of input
    if (!is.null(len)) {
        if (is_range(len) || are_integer(len)) {
            assert(has_len(input, len), prefix = EPW_HEADER[[header_type]])
        } else {
            assert(is.list(len), has_name(len, "len"))
            assert(has_len(input, len$len, len$step), prefix = EPW_HEADER[[header_type]])
        }
    }
    # }}}

    # check type and coerce to corresponding type {{{
    if (!is.null(type)) {
        assert(is_named(type))
        tnm <- names(type)
        # type is a list of functions
        if (has_name(input, tnm)) {
            for (i in tnm) {
                fun <- header_data_type_fun(type[[i]], raw = raw, coerce = coerce,
                    prefix = paste(EPW_HEADER[[header_type]]), strict = strict
                )
                input[[i]] <- tryCatch(fun(input[[i]]),
                    error_assertion = function (e) header_error_cnd(e, header_type, "type", tnm, raw, ...)
                )
            }
        } else {
            # for each function
            for (i in tnm) {

                # get member indices
                indices <- type[[i]]

                # if input is raw string and indices are names, make sure input
                # has corresponding members
                if (is.character(indices)) {
                    if (raw && !has_name(input, indices)) {
                        stop("Invalid input name in `type`.")
                    } else {
                        indices <- indices[indices %in% names(input)]
                    }
                }

                # get type assert and coerce function
                fun <- header_data_type_fun(i, raw = raw, coerce = coerce, prefix = paste(EPW_HEADER[[header_type]]), strict = strict)

                # for each specified index
                for (idx in indices) {
                    # assert and coerce
                    input[[idx]] <- tryCatch(fun(input[[idx]]),
                        error_assertion = function (e) header_error_cnd(e, header_type, "type", idx, raw, ...)
                    )
                }
            }
        }
    }
    # }}}

    # check range {{{
    if (!is.null(range)) {
        # for raw string, make sure input exists
        if (raw) {
            assert(has_name(input, names(range)))
        } else {
            range <- range[names(range) %in% names(input)]
        }
        for (name in names(range)) {
            tryCatch(assert(in_range(input[[name]], range[[name]]), prefix = paste(EPW_HEADER[[header_type]])),
                error_assertion = function (e) {
                    if (!strict) NA_real_ else header_error_cnd(e, header_type, "range", name, raw, ...)
                }
            )
        }
    }
    # }}}

    input
}
# }}}
# parse_epw_header_location {{{
parse_epw_header_location <- function (input, warning = TRUE, ...) {
    res <- parse_epw_header_basic("location", input, len = 10L,
        name = c("header_name",
            "city", "state_province", "country", "data_source", "wmo_number",
            "latitude", "longitude", "time_zone", "elevation"
        ),
        type = list(dbl = c("latitude", "longitude", "elevation", "time_zone")),
        range = list(
            latitude = ranger(-90, TRUE, 90, TRUE),
            longitude = ranger(-180, TRUE, 180, TRUE),
            time_zone = ranger(-12, TRUE, 14, TRUE),
            elevation = ranger(-1000, TRUE, 9999.9, FALSE)
        ),
        coerce = TRUE,
        raw = TRUE,
        strict = FALSE,
        ...
    )[-1L]

    if (warning) warn_epw_header_na(input, res)

    res
}
# }}}
# parse_epw_header_design {{{
# currently, only parse annual design day conditions as specified in ASHRAE HOF
# 2009
parse_epw_header_design <- function (input, warning = TRUE, ...) {
    n <- suppressWarnings(as.integer(input[2L]))

    if (is.na(n) || !n %in% c(0L, 1L)) {
        parse_issue("error_invalid_epw_header_design_number", "epw",
            paste("Non-integral design condition number field"),
            post = paste0("Number of design condition can either be 0 or 1. ",
                "Note that currently only one design conditon specified in ",
                "ASHRAE HOF 2009 and above is supported per EPW file."
            ),
            ...
        )
    }

    # "DESIGN CONDITIONS, 0"
    if (length(input) == 2L && n == 0L) return(list(list(NULL)))

    res <- parse_epw_header_basic("design", input, len = 70L,
        # name {{{
        name = c(
        "header_name",                                     # [1] chr
        "n",                                               # [2] int
        "source",                                          # [3] chr
        "empty_separator",                                 # [4] chr
        "heating",                                         # [5] chr
        "coldest_month",                                   # [6] int
        "heating_db_99.6",                                 # [7] dbl
        "heating_db_99.0",                                 # [8] dbl
        "humidification_dp_99.6",                          # [9] dbl
        "humidification_hr_99.6",                          #[10] dbl
        "humidification_mcdb_99.6",                        #[11] dbl
        "humidification_dp_99.0",                          #[12] dbl
        "humidification_hr_99.0",                          #[13] dbl
        "humidification_mcdb_99.0",                        #[14] dbl
        "coldest_month_ws_0.4",                            #[15] dbl
        "coldest_month_mcdb_0.4",                          #[16] dbl
        "coldest_month_ws_1.0",                            #[17] dbl
        "coldest_month_mcdb_1.0",                          #[18] dbl
        "mcws_99.6_db",                                    #[19] dbl
        "pcwd_99.6_db",                                    #[20] dbl
        "cooling",                                         #[21] chr
        "hotest_month",                                    #[22] int
        "hotest_month_db_range",                           #[23] dbl
        "cooling_db_0.4",                                  #[24] dbl
        "cooling_mcwb_0.4",                                #[25] dbl
        "cooling_db_1.0",                                  #[26] dbl
        "cooling_mcwb_1.0",                                #[27] dbl
        "cooling_db_2.0",                                  #[28] dbl
        "cooling_mcwb_2.0",                                #[29] dbl
        "evaporation_wb_0.4",                              #[30] dbl
        "evaporation_mcdb_0.4",                            #[31] dbl
        "evaporation_wb_1.0",                              #[32] dbl
        "evaporation_mcdb_1.0",                            #[33] dbl
        "evaporation_wb_2.0",                              #[34] dbl
        "evaporation_mcdb_2.0",                            #[35] dbl
        "mcws_0.4_db",                                     #[36] dbl
        "pcwd_0.4_db",                                     #[37] dbl
        "dehumification_dp_0.4",                           #[38] dbl
        "dehumification_hr_0.4",                           #[39] dbl
        "dehumification_mcdb_0.4",                         #[40] dbl
        "dehumification_dp_1.0",                           #[41] dbl
        "dehumification_hr_1.0",                           #[42] dbl
        "dehumification_mcdb_1.0",                         #[43] dbl
        "dehumification_dp_2.0",                           #[44] dbl
        "dehumification_hr_2.0",                           #[45] dbl
        "dehumification_mcdb_2.0",                         #[46] dbl
        "enthalpy_0.4",                                    #[47] dbl
        "mcdb_0.4",                                        #[48] dbl
        "enthalpy_1.0",                                    #[49] dbl
        "mcdb_1.0",                                        #[50] dbl
        "enthalpy_2.0",                                    #[51] dbl
        "mcdb_2.0",                                        #[52] dbl
        "hours_8_to_4_12.8_20.6",                          #[53] dbl
        "extremes",                                        #[54] chr
        "extreme_annual_ws_1.0",                           #[55] dbl
        "extreme_annual_ws_2.5",                           #[56] dbl
        "extreme_annual_ws_5.0",                           #[57] dbl
        "extreme_max_wb",                                  #[58] dbl
        "extreme_annual_db_mean_min",                      #[59] dbl
        "extreme_annual_db_mean_max",                      #[60] dbl
        "extreme_annual_db_sd_min",                        #[61] dbl
        "extreme_annual_db_sd_max",                        #[62] dbl
        "5_year_return_period_values_of_extreme_db_min",   #[63] dbl
        "5_year_return_period_values_of_extreme_db_max",   #[64] dbl
        "10_year_return_period_values_of_extreme_db_min",  #[65] dbl
        "10_year_return_period_values_of_extreme_db_max",  #[66] dbl
        "20_year_return_period_values_of_extreme_db_min",  #[67] dbl
        "20_year_return_period_values_of_extreme_db_max",  #[68] dbl
        "50_year_return_period_values_of_extreme_db_min",  #[69] dbl
        "50_year_return_period_values_of_extreme_db_max"   #[70] dbl
        ),
        # }}}
        type = list(
            int = c("n", "coldest_month", "hotest_month"),
            dbl = c(7L:20L, 23L:53L, 55L:70L)
        ),
        range = list(
            `hours_8_to_4_12.8_20.6` = ranger(0L, TRUE),
            `coldest_month` = ranger(1L, TRUE, 12L, TRUE),
            `hotest_month` = ranger(1L, TRUE, 12L, TRUE)
        ),
        raw = TRUE, coerce = TRUE, strict = FALSE, ...
    )

    if (warning) warn_epw_header_na(input, res)

    list(source = res$source, heating = res[6L:20L], cooling = res[22L:53L], extremes = res[55L:70L])
}
# }}}
# parse_epw_header_typical {{{
parse_epw_header_typical <- function (input, warning = TRUE, ...) {
    # get number of typical periods
    n <- parse_epw_header_basic("typical", input[2L], name = "n",
        type = list(int = "n"), range = list(n = ranger(0, TRUE)),
        raw = TRUE, coerce = TRUE, ...
    )$n

    # check length
    assert(has_len(input, 2L, step = 4L), prefix = EPW_HEADER$typical)

    # "TYPICAL/EXTREME PERIODS, 0"
    if (length(input) == 2L && n == 0L) return(list(list(data.table())))

    if ((actual_n <- (length(input) - 2) %/% 4) != n) {
        if (actual_n * 4 != (length(input) - 2)) input <- input[1:(actual_n * 4 + 2)]

        if (warning) {
            parse_issue("warning_invalid_epw_header_typical_length", "epw",
                paste("Invalid", input[[1]], "header data format"), num = 1,
                post = paste0("Number of periods '", n, "' did not match the actual data period presented '",
                    actual_n, "'. The latter will be used during parsing."),
                stop = FALSE
            )
        }
    }

    input <- input[-c(1L, 2L)]
    n <- actual_n

    days <- data.table(matrix(input, nrow = n, byrow = TRUE,
        dimnames = list(as.character(1:n), c("name", "type", "start_day", "end_day"))
    ))

    set(days, NULL, "index", seq_len(nrow(days)))
    set(days, NULL, "type", stri_trans_tolower(days$type))
    set(days, NULL, "start_day", epw_date(days$start_day))
    set(days, NULL, "end_day", epw_date(days$end_day))
    setcolorder(days, c("index", "name", "type", "start_day", "end_day"))

    tryCatch(
        assert(is_unique(days$name), prefix = paste("Name of", EPW_HEADER$typical)),
        error_assertion = function (e) {
            if (warning) header_error_cnd(e, "typical", "day_name", stop = FALSE, ...)
        }
    )
    tryCatch(
        assert(is_choice(days$type, c("Extreme", "Typical")), prefix = paste("Day type of", EPW_HEADER$typical)),
        error_assertion = function (e) {
            if (warning) header_error_cnd(e, "typical", "day_type", stop = FALSE, ...)
        }
    )
    tryCatch(
        assert(not_epwdate_realyear(days$start_day), prefix = paste("Start day of", EPW_HEADER$typical)),
        error_assertion = function (e) {
            if (warning) header_error_cnd(e, "typical", "day_date", stop = FALSE, ...)
        }
    )
    tryCatch(
        assert(not_epwdate_realyear(days$end_day), prefix = paste("End day of", EPW_HEADER$typical)),
        error_assertion = function (e) {
            if (warning) header_error_cnd(e, "typical", "day_date", stop = FALSE, ...)
        }
    )
    days
}
# }}}
# parse_epw_header_ground {{{
parse_epw_header_ground <- function (input, warning = TRUE, ...) {
    # get number of ground temperature periods
    n <- parse_epw_header_basic("ground", input[2L], name = "n",
        type = list(int = "n"), range = list(n = ranger(0, TRUE)),
        raw = TRUE, coerce = TRUE, strict = TRUE, ...
    )$n

    # check length
    assert(has_len(input, 2L, step = 16L), prefix = EPW_HEADER$ground)

    # "GROUND TEMPERATURES, 0"
    if (length(input) == 2L && n == 0L) return(list(list(data.table())))

    if ((actual_n <- (length(input) - 2) %/% 16) != n) {
        if (actual_n * 16 != (length(input) - 2)) input <- input[1:(actual_n * 16 + 2)]

        if (warning) {
            parse_issue("warning_invalid_epw_header_typical_length", "epw",
                paste("Invalid", input[[1]], "header data format"), num = 1,
                post = paste0("Number of periods '", n, "' did not match the actual data period presented '",
                    actual_n, "'. The latter will be used during parsing."),
                stop = FALSE
            )
        }
    }

    if (warning) type <- input[1L]

    input <- input[-c(1L, 2L)]
    n <- actual_n

    m <- matrix(suppressWarnings(as.numeric(input)), nrow = n, byrow = TRUE)
    if (warning) {
        inp <- matrix(input, nrow = n, byrow = TRUE)
        if (any(na <- is.na(m[, 1L]))) {
            parse_issue("warning_invalid_epw_header_ground_depth", "epw",
                paste("Invalid", type, "header data format"),
                num = sum(na),
                post = sprintf("[%s]: failed to parse ground depth value '%s' at field position #%i. NA was introduced.",
                    lpad(seq_len(sum(na)), "0"), as.character(inp[na, 1L]), (which(na) - 1L) * 16 + 2L
                ),
                stop = FALSE
            )
        }

        if (any(na <- is.na(m[, 5L:16L]))) {
            i_fld <- which(t(na)) %% 12L
            i_dep <- vlapply(seq_len(n), function(i) any(na[i, ]))
            parse_issue("warning_invalid_epw_header_ground_temp", "epw",
                paste("Invalid", type, "header data format"),
                num = sum(na),
                post = sprintf("[%s]: failed to parse ground temp value '%s' at field position #%i of #%i depth '%s'. NA was introduced.",
                    lpad(seq_len(sum(na)), "0"), as.character(unlist(inp[, 5L:16L])[unlist(na)]),
                    i_fld + 3L, which(i_dep), m[, 1L][i_dep]
                ),
                stop = FALSE
            )
        }
    }

    # change into a data.table
    temp <- data.table(m)
    setnames(temp,
        c("depth",
          "soil_conductivity",
          "soil_density",
          "soil_specific_heat",
          "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"
        )
    )

    tryCatch(
        assert(is_unique(temp$depth), prefix = paste("Depth of", EPW_HEADER$ground)),
        error_assertion = function (e) {
            if (warning) header_error_cnd(e, "ground", "depth", stop = FALSE, ...)
        }
    )

    # change into tidy format
    temp <- melt.data.table(temp[, index := .I],
        id.vars = c("index", "depth", "soil_conductivity", "soil_density", "soil_specific_heat"),
        variable.name = "month", value.name = "temperature", variable.factor = FALSE
    )
    set(temp, NULL, "month", as.integer(temp$month))
    setcolorder(temp, c("index", "depth", "month", "soil_conductivity", "soil_density", "soil_specific_heat", "temperature"))
    setorderv(temp, c("index", "month"))

    temp
}
# }}}
# parse_epw_header_holiday {{{
parse_epw_header_holiday <- function (input, warning = TRUE, ...) {
    # get number of holidays
    res <- parse_epw_header_basic("holiday", input[2L:5L],
        name = c("leapyear", "dst_start_day", "dst_end_day", "n"),
        type = list(
            leapyear = list(
                function (x) is_choice(x, c("Yes", "No", "Y", "N")),
                function (x) if (stri_trans_tolower(x) == "yes") TRUE else FALSE
            ),
            dst_start_day = list(not_epwdate_realyear, epw_date),
            dst_end_day = list(not_epwdate_realyear, epw_date),
            n = list(is_strint, as.integer)
        ),
        range = list(n = ranger(0, TRUE)),
        raw = TRUE, coerce = TRUE, ...
    )

    # check length
    assert(has_len(input, 5L, step = 2L), prefix = EPW_HEADER$holiday)

    # if dst start date is given but end date is not
    if (res$dst_start_day == 0L && res$dst_end_day != 0L) {
        parse_issue("error_invalid_epw_dst_end_date", "epw",
            "Missing daylight saving period end date field",
            post = paste0("Daylight saving start date is given (", surround(res$dst_start_day),"), ",
                "but end date is not."
            ), ...
        )
    }
    # if dst start date is not given but end date is given
    if (res$dst_start_day != 0L && res$dst_end_day == 0L) {
        parse_issue("error_invalid_epw_dst_end_date", "epw",
            "Missing daylight saving period start date field",
            post = paste0("Daylight saving end date is given (", surround(res$dst_end_day),"), ",
                "but start date is not given."
            ), ...
        )
    }

    res$dst <- c(res$dst_start_day, res$dst_end_day)
    res$dst_start_day <- NULL
    res$dst_end_day <- NULL

    # "HOLIDAYS/DAYLIGHT SAVINGS, No, 0, 0, 0"
    if (length(input) == 5L && res$n == 0L) {
        res$n <- NULL
        res$holiday <- data.table()
        return(res)
    }

    # change into a data.table
    holiday <- data.table(matrix(input[-c(1L:5L)], nrow = res$n, byrow = TRUE))
    setnames(holiday, c("name", "day"))
    set(holiday, NULL, "index", seq_len(nrow(holiday)))
    setcolorder(holiday, c("index", "name", "day"))

    # parse holiday
    set(days, NULL, "day", epw_date(holiday$day))

    tryCatch(
        assert(is_unique(holiday$name), prefix = paste("Name of", EPW_HEADER$holiday)),
        error_assertion = function (e) header_error_cnd(e, "holiday", "name", ...)
    )
    tryCatch(
        assert(not_epwdate_realyear(holiday$day), prefix = EPW_HEADER$holiday),
        error_assertion = function (e) header_error_cnd(e, "holiday", "day_date", ...)
    )

    res$n <- NULL
    res
}
# }}}
# parse_epw_header_comment1 {{{
parse_epw_header_comment1 <- function (input, warning = TRUE, ...) {
    Reduce(function (...) paste(..., sep = ","), input[-1L])
}
# }}}
# parse_epw_header_comment2 {{{
parse_epw_header_comment2 <- parse_epw_header_comment1
# }}}
# parse_epw_header_period {{{
parse_epw_header_period <- function (input, warning = TRUE, ...) {
    # get number of data periods
    res <- parse_epw_header_basic("period", input, len = list(len = 7L, step = 4L),
        name = c("header_name", "n", "interval"),
        type = list(
            n = list(is_strint, as.integer),
            interval = list(is_strint, as.integer)
        ),
        range = list(n = ranger(1L, TRUE), interval = ranger(1L, TRUE, 60L, TRUE)),
        raw = TRUE, coerce = TRUE, ...
    )
    res <- res[2L:3L]

    if (60L %% res$interval != 0L){
        parse_issue("error_invalid_epw_data_interval", "epw",
            "Invalid number of records per hour field in DATA PERIODS header",
            post = paste0(
                "Number of records per hour of ", surround(res$interval),
                " does not result in integral number of minutes between records"
            ), ...
        )
    }

    # change into a data.table
    period <- data.table(matrix(input[-c(1L:3L)], nrow = res$n, byrow = TRUE))

    # check if invalid number of data period
    if (ncol(period) != 4L) {
        parse_issue("error_invalid_epw_data_period_number", "epw",
            "Invalid number of data periods field in DATA PERIODS header",
            post = paste0(as.integer(ncol(period) / 4L), " data periods found but ",
                "the field only indicates ", res$n, "."
            ), ...
        )
    }
    setnames(period, c("name", "start_day_of_week", "start_day", "end_day"))
    set(period, NULL, "index", seq_len(nrow(period)))
    set(period, NULL, "start_day_of_week", get_epw_wday(period$start_day_of_week))
    set(period, NULL, "start_day", epw_date(period$start_day))
    set(period, NULL, "end_day", epw_date(period$end_day))
    setcolorder(period, c("index", "name", "start_day_of_week", "start_day", "end_day"))

    tryCatch(
        assert(are_wday(period$start_day_of_week), prefix = paste("Start day of week in", EPW_HEADER$period)),
        error_assertion = function (e) header_error_cnd(e, "period", "start_wday", ...)
    )
    tryCatch(
        assert(not_epwdate_weekday(period$start_day, zero = FALSE), prefix = EPW_HEADER$period),
        error_assertion = function (e) header_error_cnd(e, "period", "start_day", ...)
    )
    tryCatch(
        assert(not_epwdate_weekday(period$end_day, zero = FALSE), prefix = EPW_HEADER$period),
        error_assertion = function (e) header_error_cnd(e, "period", "end_day", ...)
    )

    i <- period[is_epwdate_type(start_day, "ymd") & !is_epwdate_type(end_day, "ymd"), which = TRUE]
    if (length(i)) {
        period[i, end_day := set_epwdate_year(end_day, lubridate::year(start_day))]

        # check if invalid date introduced after updating year
        if (any(is.na(period$end_day[i]))) {
            parse_issue("error_invalid_epw_date_introduced", "epw",
                "Missing year data in data period end day",
                post = paste0(
                    "Start day of data period contains year but end day does not. ",
                    "Assuming same year for those data periods introduces invalid date. ",
                    "Usually this means that the year is not a leap year but end day occurs on Feb 29."
                ),
                ...
            )
        } else {
            parse_issue("warning_conflict_epw_period", "epw",
                "Missing year data in data period end day",
                post = paste0(
                    "Start day of data period contains year but end day does not. ",
                    "Assuming same year for those data periods."
                ),
                stop = FALSE, ...
            )
        }
    }

    i <- period[!is_epwdate_type(start_day, "ymd") & is_epwdate_type(end_day, "ymd"), which = TRUE]
    if (length(i)) {
        period[i, end_day := set_epwdate_year(end, lubridate::year(start_day))]

        parse_issue("warning_conflict_epw_period", "epw",
            "Missing year data in data period start day",
            post = paste0(
                "End day of data period contains year but start day does not. ",
                "Assuming non-real-year for those data periods."
            ),
            stop = FALSE, ...
        )
    }

    # for real year, check if day of week matches
    i <- period[is_epwdate_type(start_day, "ymd") & start_day_of_week != wday(start_day), which = TRUE]
    if (length(i)) {
        parse_issue("warning_mismatch_epw_period_dayofweek", "epw",
            "Mismatched start day of week",
            post = paste0("The actual start day (", period$start_day[i], ") of ",
                    "data period #", period$index[i], " ", surround(period$name[i]) ," is ",
                    wday(period$start_day[i], label = TRUE),
                    " but specified as ",
                    wday(period$start_day_of_week[i], label = TRUE),
                    collapse = "\n"
            ),
            stop = FALSE, ...
        )
    }

    # check if not real year and end day smaller than start day
    if (period[as_date(start_day) > as_date(align_epwdate_type(end_day, start_day)), .N]) {
        parse_issue("error_invalid_epw_data_period_endday", "epw",
            "Invalid data period end day",
            post = paste0(
                "Currently rewinded data period is not supported. ",
                "End day should always be euqal as or later than start day."
            ), ...
        )
    }

    res$n <- NULL
    res$period <- period
    res
}
# }}}
# warn_epw_header_na {{{
warn_epw_header_na <- function (input, res) {
    if (!any(na <- vlapply(res, is.na))) return()

    nm <- gsub("_", " ", names(res[na]), fixed = TRUE)

    parse_issue("warning_invalid_epw_header_design", "epw",
        paste("Invalid", input[[1]], "header data format"),
        num = sum(na),
        post = sprintf("[%s]: failed to parse value '%s' at field position #%i. NA was introduced.",
            lpad(seq_along(nm), "0"), unlist(input[na]), which(na)
        ),
        stop = FALSE
    )

    res
}
# }}}
# header_data_type_fun {{{
header_data_type_fun <- function (type, coerce = TRUE, raw = FALSE, strict = TRUE, ...) {
    factory <- function (before, after = NULL) {
        if (!coerce) {
            function (x) {assert(before(x), ...); x}
        } else if (strict) {
            function (x) {assert(before(x), ...); after(x)}
        } else {
            function (x) suppressWarnings(after(x))
        }
    }

    # if input is a list of function
    if (is.list(type)) return(factory(type[[1]], type[[2]]))

    switch(type,
        int = if (raw) factory(is_strint, as.integer) else factory(is_integer, as.integer),
        dbl = if (raw) factory(is_strnum, as.double) else factory(is_number, as.double),
        chr = factory(is_string, as.character),
        epwdate = factory(is_epwdate, epw_date),
        integer = if (raw) factory(are_strint, as.integer) else factory(are_integer, as.integer),
        double = if (raw) factory(are_strnum, as.double) else factory(are_number, as.double),
        character = factory(are_string, as.character),
        stop("Invalid type specification")
    )
}
# }}}
# header_error_cnd {{{
header_error_cnd <- function (cnd, header_type, check_type, idx = NULL, raw = TRUE, stop = TRUE, ...) {
    # error message from the original assertion or coercion
    msg <- conditionMessage(cnd)

    # new error type according to header name
    err_type <- c(paste("error_invalid_epw_header", header_type, check_type, sep = "_"))

    # position message
    if (!is.null(idx)) {
        if (is.character(idx)) {
            nm <- gsub("_", " ", idx)
        } else {
            nm <- paste0("#", idx)
        }
        msg <- paste(nm, "in", msg)
    }

    if (raw) {
        parse_issue(err_type, "epw",
            title = paste("Invalid", EPW_HEADER[[header_type]], "header data format"),
            post = msg, stop = stop, ...
        )
    } else {
        abort(err_type, msg)
    }
}
# }}}
# get_epw_header_data {{{
get_epw_header_data <- function (epw_header, name) {
    nm <- name
    epw_header[name == EPW_HEADER[[nm]], contents][[1L]]
}
# }}}
# get_epw_wday {{{
get_epw_wday <- function (x, label = FALSE, abbr = FALSE){
    wd <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

    res <- if (label) rep(NA_character_, length(x)) else rep(NA_integer_, length(x))

    if (is.numeric(x)) {
        is_ok <- x == trunc(x) & x >= 1L & x <= 7L
        if (!label) {
            res[is_ok] <- as.integer(x[is_ok])
        } else {
            if (abbr) {
                res[is_ok] <- stri_sub(wd[x[is_ok]], 3L)
            } else {
                res[is_ok] <- wd[x[is_ok]]
            }
        }
    } else {
        m <- match_in_vec(x, wd, label = FALSE)

        if (!label) {
            res[!is.na(m)] <- m[!is.na(m)]
        } else {
            if (abbr) {
                res[!is.na(m)] <- stri_sub(wd[m[!is.na(m)]], to = 3L)
            } else {
                res[!is.na(m)] <- wd[m[!is.na(m)]]
            }
        }
    }

    res
}
# }}}
# get_epw_month {{{
get_epw_month <- function (x, label = FALSE){
    mon <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    match_in_vec(x, mon, label = label)
}
# }}}
# EpwDate {{{
# S3 class for EPW format date {{{
# type:
# -1 = invalid
#  0 = empty
#  1 = julian day of year
#  2 = month/day
#  3 = year/month/day
#  4 = nth day in month
#  5 = last day in month

# EnergyPlus supports multiple formats of date specification
# Reference: Table 2.14, Chap 2 Weather Converter Program, Auxillary Program
# 0. 0 [type: 0L]
# 1. Julian day of year [type: 1L]
# 2. num_Month/num_Day  [type: 2L]
# 3. num_Month/num_Day/num_Year (only for DataPeriod) [type: 3L]
# 4. num_Day alp_Month [type: 2L]
# 5. alp_Month num_Day [type: 2L]
# 6. num Weekday In Month (only for Holiday/DaylightSavingPeriod) [type: 4L]
# 7. last Weekday In Month (only for Holiday/DaylightSavingPeriod) [type: 5L]
# }}}
# epw_date {{{
epw_date <- function (x, leapyear = TRUE) {
    as_EpwDate(x, leapyear = leapyear)
}
init_epwdate_vctr <- function (len, init = NA) {
    structure(rep(as.Date(init), len), class = c("EpwDate", "Date"))
}
assign_epwdate <- function (x) {
    setattr(x, "class", c("EpwDate", "Date"))
}
get_epwdate_type <- function (x) {
    y <- as.integer(lubridate::year(x))
    res <- rep(EPWDATE_TYPE$invalid, length(x))
    res[y >= EPWDATE_YEAR$leap$zero & y <= EPWDATE_YEAR$noleap$zero] <- EPWDATE_TYPE$zero
    res[y >= EPWDATE_YEAR$leap$jul  & y <= EPWDATE_YEAR$noleap$jul ] <- EPWDATE_TYPE$jul
    res[y >= EPWDATE_YEAR$leap$md   & y <= EPWDATE_YEAR$noleap$md  ] <- EPWDATE_TYPE$md
    res[y >= EPWDATE_YEAR$leap$nth  & y <= EPWDATE_YEAR$noleap$nth ] <- EPWDATE_TYPE$nth
    res[y >= EPWDATE_YEAR$leap$last & y <= EPWDATE_YEAR$noleap$last] <- EPWDATE_TYPE$last
    res[                              y >  EPWDATE_YEAR$noleap$last] <- EPWDATE_TYPE$ymd
    res
}
is_epwdate_type <- function (x, type) {
    get_epwdate_type(x) %in% unlist(EPWDATE_TYPE[type], use.names = FALSE)
}
set_epwdate_year <- function(x, year) {
    tmp <- as_date(x)
    lubridate::year(tmp) <- year
    assign_epwdate(tmp)
}
align_epwdate_type <- function (x, to) {
    assert(have_same_len(x, to))
    t <- get_epwdate_type(x)
    # only for julian and month day
    can_align <- t >= EPWDATE_TYPE$jul & t <= EPWDATE_TYPE$md
    x[can_align] <- set_epwdate_year(x[can_align], lubridate::year(to[can_align]))
    x
}
set_epwdate_type <- function(x, type = ("md")) {
    type <- match.arg(type, c("md", "ymd"))
    assert(is_epwdate_type(x, c("md", "ymd")))
    is_leap <- lubridate::leap_year(x)
    y_l <- EPWDATE_TYPE$leap[[type]]
    y_nol <- EPWDATE_TYPE$noleap[[type]]
    x[t]
}
reset_epwdate_year <- function (x, leapyear) {
    # expect empty and real year
    t <- get_epwdate_type(x)
    if (any(t == EPWDATE_TYPE$nth | t == EPWDATE_TYPE$last)) warning("Cannot reset year of nth or last format date.")
    x[t == EPWDATE_TYPE$jul] <- set_epwdate_year(x, if (leapyear) EPWDATE_YEAR$leap$jul else EPWDATE_YEAR$noleap$jul)
    x[t == EPWDATE_TYPE$md] <- set_epwdate_year(x, if (leapyear) EPWDATE_YEAR$leap$md else EPWDATE_YEAR$noleap$md)
    x
}
ymd_to_md <- function (x) {
    is_leap <- lubridate::leap_year(as_date(x))
    x[is_leap] <- set_epwdate_year(x[is_leap], EPWDATE_YEAR$leap$md)
    x[!is_leap] <- set_epwdate_year(x[!is_leap], EPWDATE_YEAR$noleap$md)
    x
}
# }}}
# as_EpwDate {{{
as_EpwDate <- function (x, ...) {
    UseMethod("as_EpwDate")
}
as_EpwDate.default <- function (x, ...) {
    stop("Missing method to convert <", class(x)[1L], "> object to <EpwDate>.")
}
# as_EpwDate.integer {{{
as_EpwDate.integer <- function (x, leapyear = TRUE) {
    res <- init_epwdate_vctr(length(x))

    if (length(x) == 0L) return(res)

    # if is 0
    res[!is.na(x) & x == 0L] <- lubridate::make_date(0L)

    v <- !is.na(x) & in_range(x, ranger(1, TRUE, 366, TRUE))
    if (all(!v)) return(res)

    y <- if (leapyear) 4L else 5L
    res[v] <- lubridate::make_date(y) + lubridate::days(x[v] - 1L)

    res
}
# }}}
# as_EpwDate.numeric {{{
as_EpwDate.numeric <- function (x, leapyear = TRUE) {
    res <- init_epwdate_vctr(length(x))
    if (length(x) == 0L) return(res)

    # parse julian type first
    is_jul <- !is.na(x) & x == trunc(x)
    res[is_jul] <- as_EpwDate.integer(as.integer(x[is_jul]))

    if (sum(is_jul) == length(x)) return(res)

    y <- if (leapyear) 8L else 9L
    s <- stri_split_fixed(x[!is_jul], ".", simplify = TRUE)
    res[!is_jul] <- lubridate::make_date(y, s[, 1L], s[, 2L])
    res
}
# }}}
# as_EpwDate.character {{{
as_EpwDate.character <- function (x, leapyear = TRUE) {
    res <- init_epwdate_vctr(length(x))
    if (length(x) == 0L) return(res)

    # coerce to double first
    is_dbl <- !is.na(suppressWarnings(as.double(x)))
    # check if ending with zero, e.g. "3.10"
    is_tenth <- as.double(stri_split_fixed(x[is_dbl], ".", 2L, simplify = TRUE)[, 2L]) %% 10 == 0L
    is_dbl[is_tenth] <- FALSE

    res[is_dbl] <- as_EpwDate.numeric(as.double(x[is_dbl]))

    if (sum(is_dbl) == length(x)) return(res)

    # init
    x <- x[!is_dbl]
    d <- res[!is_dbl]

    # component separator, include "." to support "Month.Day" format
    sep <- "[/\\-\\.]"
    s <- stri_split_charclass(x, sep, n = 5L, omit_empty = TRUE, simplify = TRUE)

    # month-day type
    is_md <- stri_isempty(s[, 3L]) & !stri_isempty(s[, 2L])
    d[is_md] <- parse_epwdate_md(x[is_md], leapyear)

    # year-month-day type
    # only accept numeric values for each field
    is_ymd <- stri_isempty(s[, 4L]) & !stri_isempty(s[, 3L])
    s_ymd <- matrix(suppressWarnings(as.integer(s[is_ymd, ])), ncol = 5L)
    d[is_ymd] <- parse_epwdate_ymd(s_ymd[, 1L], s_ymd[, 2L], s_ymd[, 3L], leapyear)

    # weekday type
    # split by space
    res_wkd <- parse_epwdate_wday(x[!is_md & !is_ymd], leapyear)
    d[!is_md & !is_ymd] <- res_wkd

    res[!is_dbl] <- d
    res
}
# }}}
# as_EpwDate.logical {{{
as_EpwDate.logical <- as_EpwDate.integer
# }}}
# as_EpwDate.Date {{{
as_EpwDate.Date <- function (x, ...) {
    # treat as default "yyyy-mm-dd" format
    assign_epwdate(copy(x))
}
# }}}
# as_EpwDate.POSIXt{{{
as_EpwDate.POSIXt <- function (x, ...) {
    # treat as default "yyyy-mm-dd" format
    assign_epwdate(as_date(x))
}
# }}}
# as_EpwDate.EpwDate {{{
as_EpwDate.EpwDate <- function (x, ...) x
# }}}
# parse_epwdate_md {{{
parse_epwdate_md <- function (x, leapyear = TRUE) {
    res <- as_date(lubridate::parse_date_time(
        paste0(2000L, "-", x), "Ymd", tz = "UTC", quiet = TRUE
    ))
    res[is.na(res)] <- as_date(lubridate::parse_date_time(
        paste0(2000L, "-", x[is.na(res)]), "Ydm", tz = "UTC", quiet = TRUE
    ))
    set_epwdate_year(res, if (leapyear) EPWDATE_YEAR$leap$md else EPWDATE_YEAR$noleap$md)
}
# }}}
# parse_epwdate_ymd {{{
parse_epwdate_ymd <- function (year, month, day, leapyear = TRUE) {
    res <- as_date(lubridate::make_date(year, month, day))
    res[is.na(res)] <- as_date(lubridate::make_date(day, year, month))
    res
}
# }}}
# parse_epwdate_wday {{{
parse_epwdate_wday <- function (x, leapyear = TRUE) {
    res <- init_epwdate_vctr(length(x))
    if (length(x) == 0L) return(res)

    # split by space
    s <- stri_split_fixed(x, " ", n = 5L, omit_empty = TRUE, simplify = TRUE)

    # try to parse "Month Day" format
    is_md <- stri_isempty(s[, 3L]) & !stri_isempty(s[, 2L])
    res[is_md] <- parse_epwdate_md(x[is_md], leapyear)

    # init component
    # n day of week. 0 == "last"
    n <- rep(-1L, nrow(s))
    # day of week
    wkd <- rep(NA_integer_, nrow(s))
    # month
    mth <- rep(NA_integer_, nrow(s))

    # get first component
    # must start from 1-4 or last
    reg <- "^(last|1(?:st)?|2(?:nd)?|3(?:rd)?|4(?:th)?)$"
    n_str <- stri_match_first_regex(s[, 1L], reg, case_insensitive = TRUE)[, 2L]
    n[!is.na(n_str) & n_str == "last"] <- 0L
    n[!is.na(n_str) & n_str != "last"] <- as.integer(stri_sub(n_str[!is.na(n_str) & n_str != "last"], to = 1L))

    if (all(n == -1L)) return(res)

    # "nth/last Month DayOfWeek"
    is_sty1 <- n > -1L & !stri_isempty(s[, 3L]) & stri_isempty(s[4L])
    wkd[is_sty1] <- get_epw_wday(s[is_sty1])
    mth[is_sty1] <- get_epw_month(s[is_sty1])

    # "last/nth DayOfWeek in/of Month"
    is_sty2 <- n > -1L & !stri_isempty(s[, 4L]) & stri_isempty(s[, 5L]) & stri_trans_tolower(s[, 3L]) %chin% c("in", "of")
    wkd[is_sty2] <- get_epw_wday(s[, 2L][is_sty2])
    mth[is_sty2] <- get_epw_month(s[, 4L][is_sty2])

    is_wkd <- !is_md & n > -1L & !is.na(wkd) & !is.na(mth)
    if (sum(is_wkd) == 0L) return(res)

    # subset
    # n day of week. 0 == "last"
    n <- n[is_wkd]
    # day of week
    wkd <- wkd[is_wkd]
    # month
    mth <- mth[is_wkd]

    ref_end <- rep(1L, sum(is_wkd))
    ref_end[n == 0L] <- lubridate::days_in_month(mth[n == 0L])
    ref_day <- lubridate::make_date(if (leapyear) 12L else 13L, mth, ref_end)
    ref_day[n == 0L] <- lubridate::make_date(if (leapyear) 16L else 17L, mth, ref_end)

    # get the weekday of first/last day in month
    wkd1 <- wday(ref_day)
    # locate_wkd {{{
    locate_wkd <- function (wkd, ref_day, wkd1, n) {
        # for example: wkd1 Sat(6), wkd Mon(1)
        if (wkd1 > wkd) {
            if (n == 0L) {
                ref_day - lubridate::days(wkd1 - wkd)
            } else {
                ref_day + lubridate::days(7L - wkd1 + wkd + 7L * (n - 1L))
            }
        # for example: wkd1 Mon(1), wkd Sat(6)
        } else if (wkd1 < wkd) {
            if (n == 0L) {
                ref_day - lubridate::days(wkd1 + 7L - wkd)
            } else {
                ref_day + lubridate::days(wkd - wkd1 + 7 * (n - 1L))
            }
        } else {
            ref_day
        }
    }
    # }}}
    d <- res[is_wkd]
    for (i in seq_along(d)) d[i] <- locate_wkd(wkd[i], ref_day[i], wkd1[i], n[i])

    res[is_wkd] <- d
    res
}
# }}}
# }}}
# is_EpwDate {{{
is_EpwDate <- function (x) {
    inherits(x, "EpwDate")
}
# }}}
#' @export
# format.EpwDate {{{
format.EpwDate <- function (x, m_spc = FALSE, ...) {
    on.exit(Sys.setlocale("LC_TIME", Sys.getlocale("LC_TIME")), add = TRUE)
    Sys.setlocale("LC_TIME", "C")
    t <- get_epwdate_type(x)
    res <- rep(NA_character_, length(x))
    res[t == 0L] <- "0"
    res[t == 1L] <- as.character(lubridate::yday(x[t == 1L]))
    res[t == 2L] <- paste(lpad(month(x[t == 2L]), width = if (m_spc) 2L else 1L), format.Date(x[t == 2L], "%e"), sep = "/")
    res[t == 3L] <- paste(year(x[t == 3L]), lpad(month(x[t == 3L]), width = if (m_spc) 2L else 1L), format.Date(x[t == 3L], "%e"), sep = "/")
    res[t == 4L] <- format_epwdate_nthwkd(x[t == 4L])
    res[t == 5L] <- format_epwdate_nthwkd(x[t == 5L], last = TRUE)
    res
}
format_epwdate_julian <- function (x) {
    suffix <- rep("th", length(x))
    suffix[x == 1L] <- "st"
    suffix[x == 2L] <- "nd"
    suffix[x == 3L] <- "rd"
    paste0(x, suffix, " day")
}
format_epwdate_nthwkd <- function (x, last = FALSE) {
    if (last) {
        n <- "Last"
        suffix <- ""
    } else {
        n <- floor(lubridate::mday(x) / 7L) + 1L
        suffix <- rep("th", length(x))
        suffix[n == 1L] <- "st"
        suffix[n == 2L] <- "nd"
        suffix[n == 3L] <- "rd"
    }

    paste0(n, suffix, " ",
        wday(x, label = TRUE),
        " in ",
        lubridate::month(x, label = TRUE, abbr = FALSE, locale = "C")
    )
}
# }}}
#' @export
# print.EpwDate {{{
print.EpwDate <- function (x, ...) {
    on.exit(Sys.setlocale("LC_TIME", Sys.getlocale("LC_TIME")), add = TRUE)
    Sys.setlocale("LC_TIME", "C")
    t <- get_epwdate_type(x)
    res <- rep(NA_character_, length(x))
    res[t == 0L] <- "0 <empty>"
    res[t == 1L] <- format_epwdate_julian(lubridate::yday(x[t == 1L]))
    res[t == 2L] <- format.Date(x[t == 2L], "%b %d")
    res[t == 3L] <- format.Date(x[t == 3L], "%Y-%m-%d")
    res[t == 4L] <- format_epwdate_nthwkd(x[t == 4L])
    res[t == 5L] <- format_epwdate_nthwkd(x[t == 5L], last = TRUE)

    print(res)
    invisible(x)
}
# }}}
#' @export
# [.EpwDate {{{
`[.EpwDate` <- function (x, i) {
    NextMethod("[")
}
# }}}
#' @export
# [[.EpwDate {{{
`[[.EpwDate` <- function (x, i) {
    NextMethod("[[")
}
# }}}
#' @export
# [<-.EpwDate {{{
`[<-.EpwDate` <- function (x, ..., value) {
    assign_epwdate(NextMethod("[<-.Date", value = value, ...))
}
# }}}
#' @export
# [[<-.EpwDate {{{
`[[<-.EpwDate` <- function (x, ..., value) {
    assign_epwdate(NextMethod("[[", value = value, ...))
}
# }}}
#' @export
# c.EpwDate {{{
c.EpwDate <- function (...) {
    res <- assign_epwdate(NextMethod(...))
    res
}
# }}}
#' @export
# as.Date.EpwDate {{{
as.Date.EpwDate <- function (x, ...) {
    class(x) <- "Date"
    x
}
# }}}
#' @export
# as.POSIXct.EpwDate {{{
as.POSIXct.EpwDate <- function (x, ...) {
    lubridate::force_tz(lubridate::as_datetime(as.Date.EpwDate(x)), tzone = "UTC")
}
# }}}
# }}}
## data
# read_epw_data {{{
read_epw_data <- function (path) {
    num_header <- 8L

    # parse the rest of file {{{
    # colnames refers to column "Long Name" in Table 2.8 in
    # "AuxiliaryPrograms.pdf" of EnergyPlus 8.6
    # TODO: fread will directly skip those few abnormal rows
    header_epw_data <- fread(path, sep = ",", skip = num_header, nrows = 0L, header = FALSE)
    if (ncol(header_epw_data) != 35L) {
        parse_issue("error_invalid_epw_data_column_num", "epw",
            "Invalid weather data column", num = 1L,
            post = paste0(
                "Expected 35 fields in EPW data instead of ",
                surround(ncol(header_epw_data)), " in current file"
            )
        )
    }

    # As documented, fread will only promote a column to a higher type if
    # colClasses requests it. It won't downgrade a column to a lower type since
    # NAs would result.
    # This means that even if a column is specified as integer in colClasses, it
    # still could be resulted as character or double.
    epw_data <- fread(path, skip = num_header, col.names = names(EPW_TYPE), colClasses = unlist(EPW_TYPE, use.names = FALSE))
    # check column types
    int_chk <- epw_data[, vapply(.SD, is.integer, logical(1L)), .SDcols = names(which(unlist(EPW_TYPE) == "integer"))]
    dbl_chk <- epw_data[, vapply(.SD, is.double, logical(1L)), .SDcols = names(which(unlist(EPW_TYPE) == "double"))]
    if (!any(int_chk)) {
        parse_issue("error_invalid_epw_data_integer_type", "epw",
            "Failed to parse variables as integer", num = sum(!int_chk),
            post = paste0("Failed variables: ", collapse(names(which(!int_chk))))
        )
    }
    if (!any(dbl_chk)) {
        parse_issue("error_invalid_epw_data_double_type", "epw",
            "Failed to parse variables as double", num = sum(!int_chk),
            post = paste0("Failed variables: ", collapse(names(which(!int_chk))))
        )
    }
    # }}}

    # handle abnormal values of present weather codes
    epw_data[, present_weather_codes := {
        # delete single quote, e.g. "'999999999'", as fread() will read then as it is
        stri_sub(present_weather_codes[stri_sub(present_weather_codes, 1L, 1L) == "'"], 1L, 1L) <- ""
        stri_sub(present_weather_codes[stri_sub(present_weather_codes, -1L, -1L) == "'"], -1L, -1L) <- ""
        # if not a 9-length string, including empty string "", replace with default missing code
        present_weather_codes[nchar(present_weather_codes) != 9L] <- EPW_MISSING_CODE$present_weather_codes
        # replace non-digits with "9"
        stri_replace_all_charclass(present_weather_codes, "[^0-9]", "9")
    }]

    # add line index
    set(epw_data, NULL, "line", seq_len(nrow(epw_data)) + num_header)

    epw_data
}
# }}}
# add_epw_raw_string {{{
add_epw_raw_string <- function (dt, exclude = c("datetime", "line")) {
    dt[, string := Reduce(function (...) paste(..., sep = ","), .SD), .SDcols = setdiff(names(EPW_TYPE), exclude)]
}
# }}}
# create_epw_datetime_sequence {{{
create_epw_datetime_sequence <- function (start, end, interval, tz = "UTC", leapyear = FALSE) {
    if (is_epwdate(start)) start <- reset_epwdate_year(start, leapyear)
    if (is_epwdate(end)) end <- reset_epwdate_year(end, leapyear)

    start <- as_date(start)
    end <- as_date(end)

    step <- 60L / interval
    offset <- lubridate::minutes(step)

    seq(lubridate::force_tz(start + offset, tzone = tz),
        lubridate::force_tz(lubridate::as_datetime(end + lubridate::hours(24L)), tzone = tz),
        by = paste(step, "mins")
    )
}
# }}}
# create_epw_datetime_components {{{
create_epw_datetime_components <- function (start, end, interval, tz = "UTC", leapyear = FALSE) {
    if (is_epwdate(start)) start <- reset_epwdate_year(start, leapyear)
    if (is_epwdate(end)) end <- reset_epwdate_year(end, leapyear)

    start <- as_date(start)
    end <- as_date(end)

    step <- as.integer(60L / interval)
    offset <- lubridate::minutes(step)

    s <- lubridate::force_tz(start + offset, tzone = tz)
    e <- lubridate::force_tz(end + offset + lubridate::days(1L), tzone = tz)

    # get hour
    h <- rep(1L:24L, each = interval, times = difftime(e, s, units = "days"))

    # get minute
    start_min <- if (interval == 1L) 0L else step
    m <- rep(seq(start_min, 60L, length.out = interval), times = difftime(e, s, units = "hours"))

    # get year, month and day
    ymd <- rep(seq(as_date(s), as_date(e) - lubridate::days(1L), by = "day"),
        each = 24 * interval
    )

    data.table(
        year = as.integer(lubridate::year(ymd)),
        month = as.integer(lubridate::month(ymd)),
        day = as.integer(lubridate::mday(ymd)),
        hour = h, minute = m
    )
}
# }}}
# match_epw_data_period {{{
match_epw_data_period <- function (epw_data, data_period, interval, leapyear, warning = FALSE) {
    # check if real year {{{
    if (get_epwdate_type(data_period$start_day) < EPWDATE_TYPE$ymd) {
        realyear <- FALSE
    } else if (get_epwdate_type(data_period$start_day) == EPWDATE_TYPE$ymd){
        realyear <- TRUE
    } else {
        # just in case
        stop("Invalid EPW date type.")
    }
    # }}}

    data <- match_epw_data_datetime(epw_data, data_period$index, data_period$name,
        data_period$start_day, data_period$end_day, interval, leapyear, realyear
    )

    # check if there is any NA in data {{{
    if (nrow(na.omit(data, invert = TRUE))) {
        na <- na.omit(data, invert = TRUE)
        parse_issue("error_invalid_epw_data_data", "epw",
            paste0("Invalid data found for ", nm),
            data = add_epw_raw_string(na)
        )
    }
    # }}}

    # get row range {{{
    from <- epw_data[line == data$line[1L], which = TRUE]
    to <- from + nrow(data) - 1L
    # }}}

    # find missing or out-of-range value
    abnormal <- find_epw_data_abnormal_line(data, offset = from - 1L, warning,
        period_name = paste0("#", data_period$index, " ", surround(data_period$name),
            " (", 60/interval, " mins interval)"
        ), from = from
    )

    # update datetime and minute column {{{
    set(epw_data, from:to, c("datetime", "minute"),
        list(create_epw_datetime_sequence(data_period$start_day, data_period$end_day, interval, leapyear = leapyear),
             data$minute_in
        )
    )
    # reset year
    set(epw_data, NULL, "datetime", {d <- epw_data$datetime; lubridate::year(d) <- epw_data$year; d})
    # }}}

    list(from = from, to = to, missing = abnormal[1L], out_of_range = abnormal[2L])
}
# }}}
# match_epw_data_datetime {{{
match_epw_data_datetime <- function (epw_data, index, name, start, end, interval,
                                     leapyear, realyear, check_minute = FALSE) {
    if (realyear && check_minute) {
        datetime <- create_epw_datetime_sequence(start, end, interval, leapyear = leapyear)

        # find first match
        day_1 <- datetime[1L]
        l_1 <- epw_data[J(day_1), on = "datetime", mult = "first", line]
    } else {
        datetime <- create_epw_datetime_components(start, end, interval, leapyear = leapyear)

        # find first match
        day_1 <- datetime[1L]
        col_on <- if (realyear) c("year", "month", "day", "hour") else c("month", "day", "hour")
        l_1 <- epw_data[day_1, on = col_on, mult = "first", line]
    }

    # name for error printing
    nm <- paste0("data period #", index, " ", surround(name), " (", (60 / interval), " mins interval)")

    # find the first match {{{
    if (!length(l_1) || is.na(l_1)) {
        if (realyear) {
            if (check_minute) {
                first_date <- day_1
            } else {
                first_date <- combine_date(day_1$year, day_1$month, day_1$day, day_1$hour)
            }
        } else {
            first_date <- combine_date(NULL, day_1$month, day_1$day, day_1$hour)
        }

        parse_issue("error_epw_data_first_date", "epw",
            paste0("Failed to find start date time for  ", nm),
            num = 1L,
            post = paste0("Failed to find date time ", first_date, " in weather data ")
        )
    }
    # }}}

    # combine
    if (realyear && check_minute) {
        l <- seq_along(datetime)
        joined <- set(epw_data[J(l), on = "line"], NULL, "datetime1", datetime)
    } else {
        setnames(datetime, paste0(names(datetime), "_in"))
        l <- seq_len(nrow(datetime)) + (l_1 - 1L)
        joined <- cbind(epw_data[J(l), on = "line"], datetime)
    }

    # find missing or invalid datetime {{{
    if (any(is.na(joined$datetime))) {
        miss <- joined[is.na(datetime)]
        if (realyear) {
            if (check_minute) {
                miss[, string := as.character(datetime1)]
            } else {
                miss[, string := combine_date(year_in, month_in, day_in, hour_in)]
            }
        } else {
            miss[, string := combine_date(NULL, month_in, day_in, hour_in)]
        }
        parse_issue("error_invalid_epw_data_date", "epw",
            paste0("Missing or invalid date found for ", nm),
            data = miss[, string := paste0("input - <empty>, but expecting - ", string)]
        )
    }
    # }}}

    # find mismatched {{{
    if (realyear) {
        if (check_minute) {
            q <- quote(datetime != datetime1)
        } else {
            q <- quote(year != year_in | month != month_in | day != day_in | hour != hour_in)
        }
    } else {
        q <- quote(month != month_in | day != day_in | hour != hour_in)
    }
    if (nrow(joined[eval(q)])) {
        mismatched <- joined[eval(q)]

        if (realyear) {
            if (check_minute) {
                mismatched[, string := paste0(
                    "input - ", as.character(datetime), ", ",
                    "but expecting - ", as.character(datetime1))
                ]
            } else {
                mismatched[, string := paste0(
                    "input - ", combine_date(year, month, day, hour), ", ",
                    "but expecting - ", combine_date(year_in, month_in, day_in, hour_in))
                ]
            }
        } else {
            mismatched[, string := paste0(
                "input - ", combine_date(NULL, month, day, hour), ", ",
                "but expecting - ", combine_date(NULL, month_in, day_in, hour_in))
            ]
        }

        parse_issue("error_mismatch_epw_data_date", "epw",
            paste0("Date time mismatch for ", nm),
            data = mismatched
        )
    }
    # }}}

    # check if leap day is found in the data but leap year is not allowed in the header {{{
    if (nrow(joined[month == 2L & day == 29L]) && !leapyear) {
        parse_issue("error_invalid_epw_data_leapday", "epw",
            paste0("Data on Feb 29 found for ", nm),
            data = add_epw_raw_string(joined[month == 2L & day == 29L]),
            post = paste("EPW file header", surround(EPW_HEADER$holiday), "indicates no leap year.")
        )
    }
    # }}}

    joined
}
# }}}
# find_epw_data_abnormal_line {{{
find_epw_data_abnormal_line <- function (epw_data, offset = 0L, warning = FALSE, period_name = NULL, from = 0L) {
    # data period name for reporting
    nm <- if (is.null(period_name)) "." else paste0(" for data period ", period_name, ".")

    # here check all rows if there are any missing values or values out of range
    # store the row number for further use during making NAs and fill NAs
    epw_data[, {
        ln <- .I + offset
        # 8 core environment data
        # EnergyPlus will stop reading weather data if any first row of these 8
        # variable out of range
        core <- c(
            "dry_bulb_temperature",
            "dew_point_temperature",
            "relative_humidity",
            "atmospheric_pressure",
            "wind_direction", "wind_speed",
            "direct_normal_radiation",
            "diffuse_horizontal_radiation"
        )

        # just in case
        assert(have_same_len(EPW_RANGE_EXIST, EPW_RANGE_VALID))

        miss <- vector("list", length(EPW_RANGE_EXIST))
        range <- vector("list", length(EPW_RANGE_VALID))
        setattr(miss, "names", names(EPW_RANGE_EXIST))
        setattr(range, "names", names(EPW_RANGE_VALID))
        rpt_miss <- unlist(EPW_REPORT_MISSING, use.names = FALSE)
        rpt_range <- unlist(EPW_REPORT_RANGE, use.names = FALSE)

        mes_miss <- NULL
        mes_range <- NULL
        mes_first <- NULL

        for (name in names(miss)) {
            val <- if (inherits(get(name), "units")) units::drop_units(get(name)) else get(name)
            m <- ln[!in_range(val, EPW_RANGE_EXIST[[name]])]
            r <- setdiff(ln[!in_range(val, EPW_RANGE_VALID[[name]])], m)
            if (name %in% rpt_miss && length(m)) {
                mes_miss <- c(mes_miss, paste0(gsub("_", " ", name, fixed = TRUE), " is missing"))
            }
            if (name %in% rpt_range && length(r)) {
                mes_r <- paste0(gsub("_", " ", name, fixed = TRUE), " should in range ", EPW_RANGE_VALID[[name]], ".")
                if (name %in% core && any(r == from)) mes_first <- c(mes_first, mes_r)
                mes_range <- c(mes_range, mes_r)
            }
            miss[[name]] <- m
            range[[name]] <- r
        }

        # check core weather data range on the row of first day, just as EnergyPlus does
        if (!is.null(mes_first)) {
            parse_issue("error_invalid_epw_data_first_day", "epw",
                paste0("Out of range error found for initial row", nm),
                data = add_epw_raw_string(.SD[1L]), num = length(mes_first),
                post = paste0("At ", combine_date(year[1L], month[1L], day[1L], hour[1L]), ": ", mes_first)
            )
        }

        if (warning && !is.null(mes_miss)) {
            ln_miss <- unlist(miss[rpt_miss], use.names = FALSE) - offset
            parse_issue("warning_epw_data_missing", "epw",
                paste0("Missing data found", nm),
                data = add_epw_raw_string(.SD[ln_miss]),
                num = length(unlist(miss, use.names = FALSE)),
                post = paste0("At ", combine_date(year[ln_miss], month[ln_miss], day[ln_miss], hour[ln_miss]), ": ", mes_miss),
                stop = FALSE
            )
        }

        if (warning && !is.null(mes_range)) {
            ln_range <- unlist(range[rpt_range], use.names = FALSE) - offset
            parse_issue("warning_epw_data_out_of_range", "epw",
                paste0("Out of range data found", nm),
                data = add_epw_raw_string(.SD[ln_range]),
                num = length(unlist(range, use.names = FALSE)),
                post = paste0("Note that the out of range values in ",
                    collapse(EPW_REPORT_RANGE$do_nothing), " will not be ",
                    "changed by EnergyPlus and could affect your simulation.\n",
                    paste0("At ", combine_date(year[ln_range], month[ln_range], day[ln_range], hour[ln_range]), ": ", mes_range)),
                stop = FALSE
            )
        }

        list(list(miss, range))
    }]$V1
}
# }}}
# find_epw_data_na_line {{{
find_epw_data_na_line <- function (epw_data, offset = 0L, warning = FALSE, period_name = NULL) {
    # data period name for reporting
    nm <- if (is.null(period_name)) "." else paste0(" for data period ", period_name, ".")

    epw_data[, {
        ln <- .I + offset
        # 8 core environment data
        # EnergyPlus will stop reading weather data if any first row of these 8
        # variable out of range
        core <- c(
            "dry_bulb_temperature",
            "dew_point_temperature",
            "relative_humidity",
            "atmospheric_pressure",
            "wind_direction", "wind_speed",
            "direct_normal_radiation",
            "diffuse_horizontal_radiation"
        )

        na <- vector("list", length(EPW_RANGE_EXIST))
        setattr(na, "names", names(EPW_RANGE_EXIST))
        rpt_na <- unlist(EPW_REPORT_MISSING, use.names = FALSE)

        mes_na <- NULL

        for (name in names(na)) {
            l <- ln[is.na(get(name))]
            if (name %in% rpt_na && length(l)) {
                mes_na <- c(mes_na, paste0(gsub("_", " ", name, fixed = TRUE),
                    " contains NA(s) which will be treated as missing value(s).")
                )
            }
            na[[name]] <- l
        }

        if (warning && !is.null(mes_na)) {
            ln_na <- unlist(na[rpt_na], use.names = FALSE) - offset
            parse_issue("warning_epw_data_na", "epw",
                paste0("NA found in data", nm),
                data = add_epw_raw_string(.SD[ln_na]),
                num = length(unlist(na, use.names = FALSE)),
                post = paste0("At ", combine_date(year[ln_na], month[ln_na], day[ln_na], hour[ln_na]), ": ", mes_na),
                stop = FALSE
            )
        }

        list(list(na))
    }]$V1[[1L]]
}
# }}}

# HEADER
# get_epw_data_period {{{
get_epw_data_period <- function (epw_header, period = NULL) {
    if (is.null(period)) return(NULL)

    n <- nrow(epw_header$period$period)
    assert(are_count(period), is_unique(period))
    if (any(period > n)) abort_bad_epw_period(period[period > n], n)
    as.integer(period)
}
# }}}
# set_epw_location {{{
set_epw_location <- function (epw_header, input) {
    res <- parse_epw_header_basic("location", input,
        type = list(
            chr = c("city", "state_province", "country", "data_source", "wmo_number"),
            dbl = c("latitude", "longitude", "elevation", "time_zone")
        ),
        range = list(
            latitude = ranger(-90, TRUE, 90, TRUE),
            longitude = ranger(-180, TRUE, 180, TRUE),
            time_zone = ranger(-12, TRUE, 12, TRUE),
            elevation = ranger(-1000, TRUE, 9999.9, FALSE)
        ),
        coerce = FALSE,
        raw = FALSE
    )

    for (name in names(res)) epw_header$location[[name]] <- res[[name]]

    epw_header
}
# }}}
# set_epw_design_condition {{{
set_epw_design_condition <- function (epw_header, idfobj = FALSE) {

}
# }}}
# set_epw_holiday {{{
set_epw_holiday <- function (epw_header, leapyear, dst, holiday) {
    if (!missing(leapyear)) {
        assert(is_flag(leapyear))
        # note that parsed start and end day in data period can only be
        # either md or ymd type
        s <- epw_header$period$period$start_day
        e <- epw_header$period$period$end_day

        # current is leap year but want to change to non-leap year
        # for md type, it is ok to change only if that period does not cover
        # Feb 29, e.g. [01/02, 02/28]
        # for ymd type, if that period covers multiple years, e.g.
        # [2007-01-01, 2009-01-01], there is a need to check 2008-02-28
        if (epw_header$holiday$leapyear & !leapyear) {
            for (i in seq_along(s)) {
                # in case ymd format that spans multiple years
                feb29 <- lubridate::make_date(c(lubridate::year(s[i]) : lubridate::year(e[i])), 2, 29)
                # for case [2007-01-01, 2009-01-01]
                feb29 <- feb29[!is.na(feb29)]

                # if February exists in the data
                if (any(s[i] <= feb29 & feb29 <= e[i])) {
                    abort("error_invalid_epw_header_leapyear",
                        paste0("Failed to change leap year indicator to ", leapyear, ", ",
                            "because data period ",
                            epw_header$period$period[i, paste0("#", index, " ", surround(name))],
                            " (", 60 / epw_header$period$interval, " mins interval) ",
                            " contains weather data of February 29th."
                        )
                    )
                }
            }

        # current is non-leap year but want to change to leap year
        # for md type, it is ok to change only if that period does not
        # across Feb, e.g. [01/02, 02/28], [03/01, 12/31]
        # for ymd type, it is always OK
        } else if (!epw_header$holiday$leapyear & leapyear) {
            is_md <- is_epwdate_type(s, "md")
            if (any(is_md)) {
                s_md <- s[is_md]
                e_md <- e[is_md]
                for (i in seq_along(s_md)) {
                    # in case ymd format that spans multiple years
                    feb28 <- lubridate::make_date(lubridate::year(s_md[i]), 2L, 28L)

                    if (!all(e_md[i] <= feb28 | feb28 <= s_md[i])) {
                        abort("error_invalid_epw_header_leapyear",
                            paste0("Failed to change leap year indicator to ", leapyear, ", ",
                                "because data period ",
                                epw_header$period$period[is_md][i, paste0("#", index, " ", surround(name))],
                                " (", 60 / epw_header$period$interval, " mins interval) ",
                                " contains weather data that cross February."
                            )
                        )
                    }
                }
            }

        }

        s <- reset_epwdate_year(s, leapyear)
        e <- reset_epwdate_year(e, leapyear)

        epw_header$holiday$leapyear <- leapyear
        epw_header$period$period[, `:=`(start_day = s, end_day = e)]
    }

    if (!missing(dst)) {
        assert(!is.list(dst))
        assert(has_len(dst, 2L))
        dst <- epw_date(dst)

        assert(are_epwdate(dst), prefix = "Daylight saving time")

        # make it possible for directly giving Date-Time object
        if (any(is_epwdate_type(dst, "ymd"))) {
            is_ymd <- is_epwdate_type(dst, "ymd")
            dst[is_ymd] <- ymd_to_md(dst[is_ymd])
        }

        epw_header$holiday$dst <- dst
    }

    if (!missing(holiday)) {
        assert(is.list(holiday), has_len(holiday, 2L), has_name(holiday, c("name", "day")),
            msg = paste("holiday should be a list or a data.frame containing",
                "`name` and `day` element(column)."
            )
        )
        holiday <- as.data.table(holiday)
        set(holiday, NULL, "day", reset_epwdate_year(epw_date(holiday$day), epw_header$holiday$leapyear))
        assert(are_epwdate(holiday$day), prefix = "Holiday")

        epw_header$holiday$holiday <- holiday
    }

    epw_header
}
# }}}
# set_epw_period_basic {{{
set_epw_period_basic <- function (epw_header, period, name, start_day_of_week) {
    n <- nrow(epw_header$period$period)

    if (missing(period)) {
        period <- seq_len(n)
    } else {
        period <- get_epw_data_period(epw_header, period)
    }

    if (missing(name)) {
        name <- NULL
    } else {
        assert(have_same_len(period, name))
        assert(!name %in% epw_header$period$period$name,
            msg = "Input new name should not be the same as existing ones.",
            err_type = "error_invalid_epw_data_period_name"
        )
        set(epw_header$period$period, period, "name", name)
    }

    if (missing(start_day_of_week)) {
        start_day_of_week <- NULL
    } else {
        assert(have_same_len(period, start_day_of_week))
        assert(is_wday(start_day_of_week))
        sdow <- get_epw_wday(start_day_of_week)

        # for real year, check if day of week matches
        # have to get the line instead of subsetting
        # see https://github.com/Rdatatable/data.table/issues/3388
        start_day <- epw_header$period$period[period][is_epwdate_type(start_day, "ymd"), start_day]
        if (length(start_day)) {
            real_wday <- wday(start_day)
            if (any(real_wday != sdow)) {
                idx <- real_wday != sdow
                warn("warning_mismatch_epw_period_dayofweek",
                    paste0("Mismatched start day of week found. The actual day ",
                        "of week for start day (", start_day[idx], ") of ",
                        "data period #", period[idx], " is ",
                        wday(real_wday[idx], label = TRUE),
                        " but specified as ",
                        wday(sdow[idx], label = TRUE),
                        collapse = "\n"
                    )
                )
            }
        }

        set(epw_header$period$period, period, "start_day_of_week", sdow)
    }

    epw_header
}
# }}}

# DATA
# merge_list {{{
merge_list <- function (x, y) {
    assert(have_same_len(x, y))
    for (i in seq_along(y)) {
        if (length(y[[i]])) {
            if (length(x[[i]])) {
                x[[i]] <- sort(unique(c(x[[i]], y[[i]])))
            } else {
                x[[i]] <- y[[i]]
            }
        }
    }
    x
}
# }}}
# merge_data_period_abnormal_index {{{
merge_data_period_abnormal_index <- function (epw_header, period = NULL, missing = TRUE, out_of_range = TRUE) {
    if (is.null(period)) {
        p <- epw_header$period$period
    } else {
        period <- get_epw_data_period(epw_header, period)
        p <- epw_header$period$period[period]
    }

    m <- NULL
    r <- NULL
    # get all out of range lines
    if (missing) {
        m <- Reduce(function (...) merge_list(...), p$missing)
    }
    if (out_of_range) {
        r <- Reduce(function (...) merge_list(...), p$out_of_range)
    }

    list(missing = m, out_of_range = r)
}
# }}}
# get_epw_data_abnormal {{{
get_epw_data_abnormal <- function (epw_data, epw_header, period = 1L, cols = NULL,
                                   keep_all = TRUE, type = c("both", "missing", "out_of_range")) {
    assert(is_count(period))
    period <- get_epw_data_period(epw_header, period)
    assert(is_flag(keep_all))
    type <- match.arg(type)

    p <- epw_header$period$period[period]
    if (type == "both") {
        l <- merge_list(p$missing[[1L]], p$out_of_range[[1L]])
    } else {
        l <- p[[type]][[1L]]
    }

    if (is.null(cols)) {
        cols <- names(which(vlapply(l, function (x) length(x) > 0L)))
    } else {
        if (!has_name(p$missing[[1L]], cols)) {
            abort("error_invaid_epw_data_column_name",
                paste0("Invalid EPW data variable name found: ", collapse(cols[!cols %in% names(p[[type]][[1L]])])),
                cols = cols
            )
        }
        l <- l[cols]
    }

    l <- sort(unique(unlist(l, use.names = FALSE)))
    if (keep_all) {
        d <- epw_data[l]
    } else {
        d <- epw_data[l, .SD, .SDcols = c("datetime", "year", "month", "day", "hour", "minute", cols)]
    }

    if (!length(l)) verbose_info("No abnormal data found.")

    set(d, NULL, "line", l + 8L)
    setcolorder(d, c("line", setdiff(names(d), "line")))
    d
}
# }}}
# get_epw_data_redundant {{{
get_epw_data_redundant <- function (epw_data, epw_header) {
    redundant <- get_epw_data_redundant_line(epw_data, epw_header, simplify = TRUE)
    if (!length(redundant)) verbose_info("No redundant data found.")

    d <- epw_data[redundant]
    set(d, NULL, "line", redundant + 8L)
    setcolorder(d, c("line", setdiff(names(d), "line")))
    d
}
# }}}
# get_epw_data_redundant_line {{{
get_epw_data_redundant_line <- function (epw_data, epw_header, simplify = FALSE) {
    p <- epw_header$period$period[order(from)]
    res <- vector("list", nrow(p) + 1L)

    l <- epw_data[, .I]
    n <- nrow(epw_data)
    for (i in seq_len(nrow(p))) {
        if (i == 1L) {
            res[[i]] <- setdiff(l, seq(p$from[i], nrow(epw_data)))
        } else if (i == nrow(p)) {
            res[[i]] <- setdiff(seq(p$to[i - 1L] + 1L, p$to[i]), seq(p$from[i], p$to[i]))
            res[[i + 1L]] <- setdiff(l, seq(1L, p$to[i]))
        } else {
            res[[i]] <- setdiff(seq(p$to[i - 1L] + 1L, p$to[i]), seq(p$from[i], p$to[i]))
        }
    }

    if (simplify) res <- unlist(res, use.names = FALSE)
    res
}
# }}}
# make_epw_data_na {{{
# Logic directly derived from WeatherManager.cc in EnergyPlus source code
make_epw_data_na <- function (epw_data, epw_header, period = NULL,
                                       missing = FALSE, out_of_range = FALSE) {
    mr <- merge_data_period_abnormal_index(epw_header, period, missing, out_of_range)

    if (missing) {
        epw_data <- make_epw_data_na_line(epw_data, mr$missing)
    }

    if (out_of_range) {
        epw_data <- make_epw_data_na_line(epw_data, mr$out_of_range)
    }
    epw_data
}
# }}}
# make_epw_data_na_line {{{
make_epw_data_na_line <- function (epw_data, line_list) {
    for (name in names(line_list)) {
        if (length(line_list[[name]])) set(epw_data, line_list[[name]], name, NA)
    }
    epw_data
}
# }}}
# fill_epw_data_abnormal {{{
fill_epw_data_abnormal <- function (epw_data, epw_header, period = NULL,
                                    missing = TRUE, out_of_range = TRUE, special = FALSE,
                                    miss_na = FALSE, range_na = FALSE) {
    if (!missing && !out_of_range) return(epw_data)

    # get atmospheric pressure at current elevation
    EPW_INIT_MISSING$atmospheric_pressure <- std_atm_press(epw_header$location$elevation)

    # get all abnormal row indices in specific periods
    mr <- merge_data_period_abnormal_index(epw_header, period, missing = TRUE, out_of_range = TRUE)
    m <- mr$missing
    r <- mr$out_of_range

    # just in case
    assert(have_same_len(m, r))

    # add previous valid line {{{
    if (special) {
        for (nm in EPW_REPORT_MISSING$use_previous) {
            if (length(m[[nm]])) {
                if (length(r[[nm]])) {
                    comb <- sort(c(m[[nm]], r[[nm]]))
                    bound <- c(0L, diff(x)) != 1L
                    pre <- rep(comb[bound] - 1L, times = diff(c(bound, length(comb) + 1L)))
                    m[[nm]] <- sort(unique(c(m[[nm]], pre[m[[nm]] == comb])))
                } else {
                    m[[nm]] <- sort(c(m[[nm]], m[[nm]][c(0, diff(m[[nm]])) != 1L] - 1L))
                }
            }
        }
        for (nm in EPW_REPORT_RANGE$use_previous) {
            if (length(r[[nm]])) {
                if (length(m[[nm]])) {
                    comb <- sort(c(m[[nm]], r[[nm]]))
                    bound <- c(0L, diff(x)) != 1L
                    pre <- rep(comb[bound] - 1L, times = diff(c(bound, length(comb) + 1L)))
                    r[[nm]] <- sort(unique(c(r[[nm]], pre[r[[nm]] == comb])))
                } else {
                    r[[nm]] <- sort(c(r[[nm]], r[[nm]][c(0, diff(r[[nm]])) != 1L] - 1L))
                }
            }
        }
    }
    # }}}

    if (missing) {
        fill_epw_data_abnormal_line(epw_data, m, miss_na, special, "missing")
    }

    if (out_of_range) {
        fill_epw_data_abnormal_line(epw_data, r, range_na, special, "out_of_range")
    }

    epw_data
}
# }}}
# fill_epw_data_abnormal_line {{{
fill_epw_data_abnormal_line <- function (epw_data, line_list, na_made, special = FALSE, type = c("missing", "out_of_range")) {
    const <- switch(type,
        missing = EPW_REPORT_MISSING,
        out_of_range = EPW_REPORT_RANGE,
        stop("Invalid abnormal type, should be either `missing` or `out_of_range`.")
    )

    # no special
    if (!special) {
        for (name in names(line_list)) {
            if (length(line_list[[name]])) set(epw_data, line_list[[name]], name, EPW_MISSING_CODE[[name]])
        }
    } else {
        # for each variable
        for (name in names(line_list)) {

            # if there are missing values
            if (length(line_list[[name]])) {

                # keep that column as it is if requested
                if (name %chin% const$do_nothing) {
                    # <DO NOTHING>

                # set to 0 if applicable
                } else if (name %chin% const$use_zero) {
                    set(epw_data, line_list[[name]], name, 0)

                # set to previous value if applicable
                } else if (name %chin% const$use_previous) {
                    l <- line_list[[name]]

                    # if there is no previous valid line, set the first
                    # missing value to initial missing value
                    if (l[1L] == 0L) {
                        set(epw_data, l[2L], name, EPW_INIT_MISSING[[name]])
                        l <- l[-1L]
                    }

                    # already change missing to NAs
                    if (na_made) {
                        epw_data[l,
                            c(name) := get(name)[1L],
                            by = list(cumsum(!is.na(get(name))))
                        ]
                    # still is presented as missing code
                    } else {
                        epw_data[l,
                            c(name) := get(name)[1L],
                            by = list(cumsum(get(name) != EPW_MISSING_CODE[[name]]))
                        ]
                    }

                # for others set to missing code
                } else {
                    set(epw_data, line_list[[name]], name, EPW_MISSING_CODE[[name]])
                }
            }
        }
    }
    epw_data
}
# }}}
# add_epw_data_unit {{{
add_epw_data_unit <- function (epw_data) {
    for (nm in names(EPW_UNIT)) {
        set(epw_data, NULL, nm, units::set_units(epw_data[[nm]], EPW_UNIT[[nm]], mode = "standard"))
    }
    epw_data
}
# }}}
# drop_epw_data_unit {{{
drop_epw_data_unit <- function (epw_data) {
    for (nm in names(EPW_UNIT)) {
        if (inherits(epw_data[[nm]], "units")) {
            set(epw_data, NULL, nm, units::drop_units(epw_data[[nm]]))
        }
    }
    epw_data
}
# }}}
# purge_epw_data_redundant {{{
purge_epw_data_redundant <- function (epw_data, epw_header) {
    redundant <- get_epw_data_redundant_line(epw_data, epw_header)
    l <- unlist(redundant, use.names = FALSE)
    if (!length(l)) {
        verbose_info("No redundant data found. Nothing to purge.")
        return(list(header = epw_header, data = epw_data))
    }

    if (eplusr_option("verbose_info")) {
        if (length(l) >= 10L) {
            msg <- paste0(paste0("#", l[1L:10L], collapse = ", "), " and etc.")
        } else {
            msg <- paste0("#", l)
        }
        verbose_info("Deleting lines ", msg, " that are not used in any data period.")
    }

    len <- vapply(redundant[-length(redundant)], length, integer(1L))

    # no need to update
    if (all(len == 0L)) return(list(header = epw_header, data = epw_data[-l]))

    p <- epw_header$period$period[order(from)]

    # update period data
    for (i in seq_along(len)) {
        offset <- len[[i]]
        if (!length(offset)) next
        p[i, `:=`(
            from = from - offset, to = to - offset,
            missing = list(list(lapply(missing[[1L]], "-", offset))),
            out_of_range = list(list(lapply(out_of_range[[1L]], "-", offset)))
        )]
    }

    epw_header$period$period <- setorderv(p, "index")
    list(header = epw_header, data = epw_data[-l])
}
# }}}
# get_epw_data {{{
get_epw_data <- function (epw_data, epw_header, period = 1L, start_year = NULL,
                          align_wday = FALSE, tz = "UTC", update = FALSE) {
    assert(is_flag(update))
    assert(is_scalar(tz))
    if (!is.null(start_year)) assert(is_count(start_year))
    assert(is_count(period))

    period <- get_epw_data_period(epw_header, period)
    p <- epw_header$period$period[period]

    # get data
    d <- epw_data[p$from:p$to]

    can_update <- FALSE

    # use the year column
    if (is.null(start_year)) {
        if (!align_wday) {
            set(d, NULL, "datetime", {year(d$datetime) <- d$year; d$datetime})
        } else {
            can_update <- TRUE

            # align start day of week
            start_year <- find_nearst_wday_year(d$datetime[[1L]], p$start_day_of_week,
                lubridate::year(Sys.Date()), epw_header$holiday$leapyear
            )
            set(d, NULL, "datetime", {year(d$datetime) <- start_year; d$datetime})

            # get the start of next year
            nextyear <- d[month == 12L & day == 31L & hour == 24L & minute == 0L, which = TRUE]

            # add one year
            if (length(nextyear)) {
                for (i in seq_along(nextyear)) {
                    if (is.na(nextyear[i+1])) {
                        s <- nextyear[i]:nrow(d)
                    } else {
                        s <- nextyear[i]:nextyear[i+1]
                    }
                    set(d, s, "datetime", {year(d$datetime[s]) <- start_year + i; d$datetime[s]})
                }
            }
        }
    } else {
        can_update <- TRUE

        # if real year and year argument is given, issue an warning
        if (is_epwdate_type(p$start_day, "ymd")) {
            s <- as_date(p$start_day)
            lubridate::year(s) <- start_year
            warn("warning_rewrite_epw_acutal_year",
                paste0("Data period #", period, " ", surround(p$name),
                    " seems like a real-year data starting from ",
                    format(as_date(p$start_day)), " to ",
                    format(as_date(p$end_day)), ". ",
                    "The starting date will be overwriten as ",
                    format(s), "."
                )
            )
        }

        set(d, NULL, "datetime1", d$datetime)
        set(d, NULL, "datetime1", {year(d$datetime1) <- start_year;d$datetime1})

        # get the start of next year
        nextyear <- d[month == 12L & day == 31L & hour == 24L & minute == 0L, which = TRUE]

        # add one year
        if (length(nextyear)) {
            for (i in seq_along(nextyear)) {
                if (is.na(nextyear[i+1])) {
                    s <- nextyear[[i]]:nrow(d)
                } else {
                    s <- nextyear[[i]]:nextyear[[i+1]]
                }
                set(d, s, "datetime", {year(d$datetime1[s]) <- start_year + i; d$datetime1[s]})
            }
        }

        # original data should not have any NA as this has been checked when
        # parsing. NA will be introduced in cases when input year is a leap year:
        # "2016-02-29" + lubridate::years(1)
        if (any(is.na(d$datetime1))) {
            invld <- d[is.na(datetime1)]
            mes <- invld[, paste0("Original: ", datetime, " --> New year: ",
                lubridate::year(datetime) + dis
            )]
            abort("error_invalid_epw_date_introduced",
                paste0("Invalid date introduced with input start year:\n",
                    paste0(mes, collapse = "\n")
                )
            )
        }

        set(d, NULL, "datetime", NULL)
        setnames(d, "datetime1", "datetime")
        setcolorder(d, c("datetime", setdiff(names(d), "datetime")))
    }

    if (tz != lubridate::tz(d$datetime[1L])) {
        can_update <- TRUE
        set(d, NULL, "datetime1", lubridate::force_tz(d$datetime, tz))

        if (any(is.na(d$datetime1))) {
            invld <- d[is.na(datetime1)]
            mes <- invld[, paste0("Original: ", datetime, " --> New time zone: ", tz)]
            abort("error_invalid_epw_date_introduced",
                paste0("Invalid date introduced with input time zone:\n",
                    paste0(mes, collapse = "\n")
                )
            )
        }

        set(d, NULL, "datetime", NULL)
        setnames(d, "datetime1", "datetime")
        setcolorder(d, c("datetime", setdiff(names(d), "datetime")))
    }

    if (update && can_update) set(d, NULL, "year", year(d$datetime))

    d
}
# }}}
# del_epw_data {{{
del_epw_data <- function (epw_data, epw_header, period) {
    assert(is_count(period))
    period <- get_epw_data_period(epw_header, period)

    # check if this is the only data period.
    # If so, stop. Since it makes no sense to create an EPW without any data
    # in it.
    if (nrow(epw_header$period$period) == 1L) {
        abort("error_epw_delete_only_period",
            paste0("The EPW file contains only one data period named ",
                surround(epw_header$period$period$name), ". It cannot be deleted ",
                "since each EPW file should contain at least one data period."
            )
        )
    }

    current <- epw_header$period$period[period]
    other <- epw_header$period$period[-period]

    epw_data <- epw_data[-seq(current$from, current$to)]
    epw_header$period$period <- other[, index := .I]

    verbose_info("Data period #", current$index, " ", surround(current$name),
        " has been successfully deleted from the EPW file."
    )

    list(header = epw_header, data = epw_data)
}
# }}}
# add_epw_data {{{
add_epw_data <- function (epw_data, epw_header, data, realyear = FALSE,
                          name = NULL, start_day_of_week = NULL, after = 0L,
                          warning = TRUE) {
    assert(is.data.frame(data))
    assert(has_name(data, setdiff(names(epw_data), c("year", "month", "day", "hour", "minute"))))
    assert(is_flag(realyear))
    if (!is.null(start_day_of_week)) assert(is_wday(start_day_of_week))
    if (!is.null(name)) assert(is_string(name))

    after <- as.integer(after)
    assert(is_count(after, zero = TRUE))

    # get data period
    n <- nrow(epw_header$period$period)
    # use nearest as template
    if (after > n) after <- n
    target_period <- if (after == 0L) 1L else after
    other_periods <- seq_len(n)

    # get new name
    if (is.null(name)) {
        all_nm <- epw_header$period$period$name
        num <- stri_match_first_regex(all_nm, "^data(\\d{0,})$", case_insensitive = TRUE)[, 2L]
        num <- num[!is.na(num)]
        if (!length(num)) {
            name <- "Data"
        } else {
            num[stri_isempty(num)] <- "0"
            name <- paste0("Data", max(as.integer(num)) + 1L)
        }
    }

    lst <- check_epw_new_data(epw_data, epw_header, data, target_period, other_periods,
        FALSE, realyear, name, start_day_of_week, warning
    )

    nm <- name
    # update period index
    if (after == 0L) {
        lst$header$period$period[, index := index + 1L]
        lst$header$period$period[name == nm, index := 1L]
    } else {
        lst$header$period$period[index > target_period, index := index + 1L]
        lst$header$period$period[name == nm, index := after + 1L]
    }
    setorderv(lst$header$period$period, "index")

    lst
}
# }}}
# set_epw_data {{{
set_epw_data <- function (epw_data, epw_header, data, realyear = FALSE,
                          name = NULL, start_day_of_week = NULL, period = 1L,
                          warning = TRUE) {
    assert(is.data.frame(data))
    assert(has_name(data, setdiff(names(epw_data), c("year", "month", "day", "hour", "minute"))))
    assert(is_flag(realyear))
    if (!is.null(start_day_of_week)) assert(is_wday(start_day_of_week))
    if (!is.null(name)) assert(is_string(name))
    assert(is_count(period))

    # get data period
    target_period <- get_epw_data_period(epw_header, period)
    other_periods <- epw_header$period$period[-period, index]
    reset <- if (length(other_periods)) FALSE else TRUE

    check_epw_new_data(epw_data, epw_header, data, target_period, other_periods,
        reset, realyear, name, start_day_of_week, warning
    )
}
# }}}
# check_epw_new_data {{{
check_epw_new_data <- function (epw_data, epw_header, data, target_period, other_periods,
                                reset = FALSE, realyear = FALSE, name = NULL,
                                start_day_of_week = NULL, warning = TRUE) {
    # get current data period and other periods
    p <- epw_header$period$period[target_period]
    p_other <- epw_header$period$period[other_periods]

    # coerce input data into a data.table
    data <- as.data.table(data)

    # add line indicator
    set(data, NULL, "line", seq_len(nrow(data)))

    # check datetime column type first, then others
    assert(inherits(data$datetime, "POSIXct"),
        msg = paste0("Column `datetime` of input data should be `POSIXct` class, not ",
            surround(class(data$datetime)[[1L]]), " class."
        )
    )

    # change time zone of input datetime to "UTC"
    set(data, NULL, "datetime", force_tz(data$datetime, "UTC"))

    # get start and end day
    # assume that datetime is sorted
    start <- data$datetime[1L]
    end <- data$datetime[nrow(data) - 1L]

    # get time step and interval using first two rows {{{
    step <- difftime(data$datetime[2L], start, units = "mins")
    if (60L %% as.numeric(step) != 0L){
        abort("error_invalid_epw_data_interval",
            paste0(
                "Invalid number of records per hour in input data. The difference ",
                "between second and first datetime is ", format(step), ", leading to ",
                "non-integral number of records per hour."
            )
        )
    }
    step <- as.numeric(step)
    if (reset) {
        interval <- as.integer(60L / step)
    } else {
        interval <- epw_header$period$interval
        if (interval != 60 / step) {
            abort("error_conflict_epw_data_interval",
                paste0(
                    "Invalid number of records per hour in input data. Value ",
                    "calculated between second and first datetime is ",
                    as.integer(60/step), " which is different from value ",
                    interval, " in the EPW file."
                )
            )
        }
    }
    # }}}

    # after cal interval, change to md format if not real year
    if (!realyear) {
        start <- ymd_to_md(start)
        end <- ymd_to_md(end)
    }
    # set start and and day
    set(p, NULL, c("start_day", "end_day"), list(epw_date(start), epw_date(end)))

    # get leap year {{{
    if (!reset) {
        leapyear <- epw_header$holiday$leapyear
    # reset
    } else {
        if (realyear) {
            if (any(lubridate::leap_year(data$datetime))) {
                leapyear <- TRUE
            } else {
                leapyear <- FALSE
            }
        } else {
            if (any(lubridate::month(data$datetime) == 2 & lubridate::mday(data$datetime) == 29)) {
                leapyear <- TRUE
            } else {
                leapyear <- FALSE
            }
        }
        # reset leap year indicator
        epw_header$holiday$leapyear <- leapyear
    }
    # }}}

    # make sure input new name is not the same as others {{{
    if (!is.null(name)) {
        if (!reset && stri_trans_tolower(name) %in% stri_trans_tolower(p_other$name)) {
            abort("error_invalid_epw_data_period_name",
                paste0("Input data period name cannot be the same as existing ones ",
                    "i.e. ", collapse(p_other$name)
                )
            )
        }
        set(p, NULL, "name", name)
    }
    # }}}

    # if AMY data given, use the day of week of the first day if not explicitly specified {{{
    if (is.null(start_day_of_week)) {
        if (realyear) {
            set(p, NULL, "start_day_of_week", wday(start))
        } else {
            set(p, NULL, "start_day_of_week", 7L)
        }
    } else {
        set(p, NULL, "start_day_of_week", get_epw_wday(start_day_of_week))
    }
    # }}}

    # match datetime
    data <- match_epw_data_datetime(data, target_period, p$name, start, end, interval,
        leapyear, realyear, check_minute = realyear
    )

    # update datetime components
    set(data, NULL, c("month", "day", "hour", "minute"),
        create_epw_datetime_components(start, end, interval, leapyear = leapyear)[, -"year"]
    )

    # make sure there is no overlapping with other periods {{{
    if (!reset) {
        for (i in seq_len(nrow(p_other))) {
            if (realyear) {
                overlapped <- any(data$datetime %in% epw_data[p_other$from[i]:p_other$to[i], datetime])
            } else {
                overlapped <- nrow(data[epw_data[p_other$from[i]:p_other$to[i]], on = c("month", "day"), mult = "first"])
            }
            if (overlapped) {
                abort("error_epw_data_overlap",
                    paste0("Failed to set target data period because date time in ",
                        "input data has overlapped with data period ",
                        p_other[i, paste0("#", index, " ", surround(name),
                            " [", start_day, ", ", end_day, "]"
                        )], "."
                    )
                )
            }
        }
    }
    # }}}

    # check other column types {{{
    check_type <- function (type) {
        fun <- switch(type, integer = is.integer, double = is.double, character = is.character)
        chk <- data[, vapply(.SD, fun, logical(1L)), .SDcols = names(which(unlist(EPW_TYPE) == type))]
        if (any(!chk)) {
            err_type <- data[, vapply(.SD, typeof, character(1L)), .SDcols = names(which(!chk))]
            abort(paste0("error_invalid_epw_data_", type, "_type"),
                paste0("Invalid column data type found in input data:\n",
                    paste0(" #", rpad(seq_len(sum(!chk))), ": ",
                        "Column ", surround(names(which(!chk))), " should be ",
                        surround(type), " class, not ", surround(err_type), ".",
                        collapse = "\n"
                    )
                )
            )
        }
    }
    check_type("integer")
    check_type("double")
    check_type("character")
    # }}}

    # set column order
    setcolorder(data, names(epw_data))

    # find lines contains NA with missing code
    na <- find_epw_data_na_line(data, warning = warning,
        period_name = paste0("#", p$index, " ", surround(p$name),
            " (", 60/interval, " mins interval)"
        )
    )
    data <- fill_epw_data_abnormal_line(data, na, FALSE, FALSE, "missing")

    abnormal <- find_epw_data_abnormal_line(data, warning = warning,
        period_name = paste0("#", p$index, " ", surround(p$name),
            " (", 60/interval, " mins interval)"
        )
    )

    # update interval if necessary
    epw_header$period$interval <- interval
    # reset
    if (reset) {
        previous <- after <- epw_data[0L]

        # set range
        set(p, NULL, c("from", "to"), list(1L, nrow(data)))
        set(p, NULL, "missing", list(abnormal[1L]))
        set(p, NULL, "out_of_range", list(abnormal[2L]))

        # after update all data, reset period
        epw_header$period$period <- p
    } else {
        p_previous <- p_other[to < p$from]
        p_after <- p_other[from > p$to]
        p_other <- p_other[!index %in% c(p_previous$index, p_after$index)]

        if (!nrow(p_previous)) {
            previous <- epw_data[0L]
        } else {
            previous <- epw_data[1L:max(p$from - 1L)]
        }

        if (!nrow(p_after)) {
            after <- epw_data[0L]
        } else {
            after <- epw_data[max(p$to + 1L):nrow(epw_data)]
        }

        # offset for later period
        offset <- (p$to - p$from + 1L) - nrow(data)

        # have to offset all abnormal lines in other period
        if (offset > 0L) {
            if (nrow(p_after)) {
                p_after[, `:=`(
                    from = from + offset, to = to + offset,
                    missing = list(list(lapply(missing[[1L]], "+", offset))),
                    out_of_range = list(list(lapply(out_of_range[[1L]], "+", offset)))
                    ),
                    by = "index"
                ]
            }

            p[, `:=`(
                to = to + offset,
                missing = list(list(lapply(missing[[1L]], "+", offset))),
                out_of_range = list(list(lapply(out_of_range[[1L]], "+", offset)))
            )]
        }

        epw_header$period$period <- setorderv(rbindlist(list(p_previous, p, p_after, p_other)), "index")
    }

    # clean
    set(data, NULL, setdiff(names(data), names(epw_data)), NULL)

    list(header = epw_header, previous = previous, data = data, after = after)
}
# }}}
# find_nearst_wday_year {{{
find_nearst_wday_year <- function (date, week_day, year = NULL, leap_year = FALSE) {
    m <- month(date)
    d <- mday(date)

    # start out from current year if not specified
    year <- as.integer(year %||% lubridate::year(Sys.Date()))
    targ <- get_epw_wday(week_day)

    if (leap_year) {
        while (wday(make_date(year, m, d)) != targ || !leap_year(year)) {
            year <- year - 1L
        }
    } else {
        while (wday(make_date(year, m, d)) != targ) {
            year <- year - 1L
        }
    }

    year
}
# }}}

# FORMAT
# format_epw {{{
format_epw <- function (epw_data, epw_header, fmt_digit = TRUE, fill = FALSE, purge = FALSE, ...) {
    list(
        header = format_epw_header(epw_header),
        data = format_epw_data(epw_data, epw_header, fmt_digit = TRUE, fill = FALSE, purge = FALSE, ...)
    )
}
# }}}
# format_epw_header {{{
format_epw_header <- function (epw_header) {
    str <- EPW_HEADER
    for (i in names(str)) {
        str[i] <- paste(
            str[[i]],
            match.fun(paste0("format_epw_header_", i))(epw_header[[i]]),
            sep = ","
        )
    }
    str
}
# }}}
# format_epw_header_location {{{
format_epw_header_location <- function (location) {
    location$latitude <- fmt_dbl(location$latitude)
    location$longitude <- fmt_dbl(location$longitude)
    location$time_zone <- fmt_int(location$time_zone)
    location$elevation <- fmt_int(location$elevation)
    paste0(unlist(location, use.names = FALSE), collapse = ",")
}
# }}}
# format_epw_header_design {{{
format_epw_header_design <- function (design) {
    if (length(design) == 0L) return("0")
    res <- paste("1", design$source, "", sep = ",")
    for (i in 2L:4L) {
        res <- paste(res,
            stri_trans_totitle(names(design)[i]),
            if (i < 4L) {
                paste(
                    design[[i]][1L], # month
                    paste0(fmt_dbl(unlist(
                        design[[i]][-c(1L, length(design[[i]]))],
                        use.names = FALSE), 1L), collapse = ","
                    ),
                    design[[i]][length(design[[i]])], # number of hours
                    sep = ","
                )
            } else {
                paste0(fmt_dbl(unlist(design[[i]], use.names = FALSE), 1L), collapse = ",")
            },
            sep = ","
        )
    }
    res
}
# }}}
# format_epw_header_typical {{{
format_epw_header_typical <- function (typical) {
    if (length(typical) == 0L) return("0")
    paste(nrow(typical),
        typical[, paste(
            name,
            stri_trans_totitle(type),
            format(start_day),
            format(end_day),
            sep = ",",
            collapse = ","
        )],
        sep = ","
    )
}
# }}}
# format_epw_header_ground {{{
format_epw_header_ground <- function (ground) {
    if (length(ground) == 0L) return("0")
    d <- dcast.data.table(copy(ground)[, temp := fmt_dbl(temperature)],
        index + depth + soil_conductivity + soil_density + soil_specific_heat ~ month,
        value.var = "temp"
    )
    d[, res := paste(
        round(depth, digits = 2),
        ifelse(is.na(soil_conductivity), "", fmt_dbl(soil_conductivity)),
        ifelse(is.na(soil_density), "", fmt_dbl(soil_density)),
        ifelse(is.na(soil_specific_heat), "", fmt_dbl(soil_specific_heat)),
        sep = ","
    )]
    d[, res := paste(res, do.call(function (...) paste(..., sep = ","), .SD), sep = ","),
        .SDcols = as.character(1L:12L)
    ]
    paste(max(d$index), paste0(d$res, collapse = ","), sep = ",", collapse = ",")
}
# }}}
# format_epw_header_holiday {{{
format_epw_header_holiday <- function (holiday) {
    res <- paste(if (holiday$leapyear) "Yes" else "No", holiday$dst[1L], holiday$dst[2L], sep = ",")
    if (nrow(holiday$holiday) == 0L) return(paste(res, "0", sep = ","))
    paste(res, holiday$holiday[, paste(name, day, sep = ",", collapse = ",")], sep = ",")
}
# }}}
# format_epw_header_comment1 {{{
format_epw_header_comment1 <- function (comment1) {
    paste(comment1, sep = ",", collapse = ",")
}
# }}}
# format_epw_header_comment2 {{{
format_epw_header_comment2 <- format_epw_header_comment1
# }}}
# format_epw_header_period {{{
format_epw_header_period <- function (period) {
    res <- paste(nrow(period$period), period$interval, sep = ",")
    paste(res,
        period$period[, paste(
            name,
            get_epw_wday(start_day_of_week, TRUE),
            format(start_day, m_spc = TRUE),
            format(end_day, m_spc = TRUE),
            sep = ",",
            collapse = ","
        )],
        sep = ","
    )
}
# }}}
# format_epw_data {{{
format_epw_data <- function (epw_data, epw_header, fmt_digit = TRUE, fill = FALSE, purge = FALSE, ...) {
    if (purge) epw_data <- purge_epw_data_redundant(epw_data, epw_header)
    d <- epw_data[, -"datetime"]
    if (fill) d <- fill_epw_data_abnormal(d, epw_header, ...)

    # round digits as WeatherConvertor
    if (fmt_digit) {
        for (nm in names(EPW_FORMAT)) {
            set(d, NULL, nm, EPW_FORMAT[[nm]](d[[nm]]))
        }
    }
    d
}
# }}}

# PRINT
# print_epw_header {{{
print_epw_header <- function (epw_header) {
    cli::cat_rule("EnergyPlus Weather File", line = 2)
    # lat_lon {{{
    lat_lon <- function(lat, longitude = FALSE) {
        sig <- if (longitude) c("E", "W") else c("N", "S")
        sig <- if (lat >= 0) sig[1L] else sig[2L]
        lat <- abs(lat)
        p1 <- trunc(lat)
        p2 <- floor((lat - p1) * 60)
        paste0(sig, " ", p1, "\u00B0", p2, "'")
    }
    # }}}
    loc <- epw_header$location
    cli::cat_line(sprintf("[Location ]: %s, %s, %s", loc$city, loc$state_province, loc$country))
    # format time zone into UTC offset
    tz <- loc$time_zone
    h <- abs(trunc(tz))
    m <- round((abs(tz) - h) * 60)
    cli::cat_line(sprintf("             {%s}, {%s}, {UTC%s}",
            lat_lon(loc$latitude),
            lat_lon(loc$longitude, TRUE),
            paste0(if (tz >= 0) "+" else "-", lpad(h, "0", 2L), ":", lpad(m, "0", 2L))
    ))
    cli::cat_line(sprintf("[Elevation]: %.fm %s see level", abs(loc$elevation), if (loc$elevation >= 0) "above" else "below"))
    cli::cat_line(sprintf("[Data Src ]: %s", loc$data_source))
    cli::cat_line(sprintf("[WMO Stat ]: %s", loc$wmo_number))
    cli::cat_line(        "[Leap Year]: ", epw_header$holiday$leapyear)
    cli::cat_line(        "[Interval ]: ", (60 / epw_header$period$interval), " mins")
    cli::cat_line()
    cli::cat_rule("Data Periods")
    print(epw_header$period$period[,
       list(Name = name,
        `StartDayOfWeek` = get_epw_wday(start_day_of_week, label = TRUE),
        `StartDay` = start_day, `EndDay` = end_day)]
    )
    cli::cat_line()
    cli::cat_rule()
    invisible(epw_header)
}
# }}}

# SAVE
# save_epw_file {{{
save_epw_file <- function (epw_data, epw_header, path, overwrite = FALSE,
                           fmt_digit = TRUE, fill = FALSE, purge = FALSE, ...) {
    if (!file.exists(path)) {
        new_file <- TRUE
    } else {
        new_file <- FALSE
        if (!overwrite) {
            abort("error_not_overwrite_epw",
                paste("Target EPW file already exists. Please set `overwrite` to",
                     "TRUE if you want to replace it."
                )
            )
        }
    }

    l <- format_epw(epw_data, epw_header, fmt_digit = fmt_digit, fill = fill, purge = FALSE, ...)
    write_lines(unlist(l$header, use.names = FALSE), path)
    fwrite(l$data, path, append = TRUE)

    if (!new_file && overwrite) {
        verbose_info("Replace the existing EPW file located at ", normalizePath(path), ".")
    }

    normalizePath(path)
}
# }}}
