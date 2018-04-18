#' @importFrom R6 R6Class
#' @importFrom data.table month mday year copy fwrite
#' @importFrom readr write_lines
#' @importFrom cli cat_line rule cli_bullet
#' @importFrom units ud_units
# Epw {{{
Epw <- R6::R6Class(classname = "Epw",
    # ACTIVE {{{
    active = list(
        city = function (value) {
            # {{{
            if (missing(value)) {
                private$m_location$city
            } else {
                private$m_location$city <- value
            }
            # }}}
        },

        state_province = function (value) {
            # {{{
            if (missing(value)) {
                private$m_location$state_province
            } else {
                private$m_location$state_province <- value
            }
            # }}}
        },

        country = function (value) {
            # {{{
            if (missing(value)) {
                private$m_location$country
            } else {
                private$m_location$country <- value
            }
            # }}}
        },

        data_source = function (value) {
            # {{{
            if (missing(value)) {
                private$m_location$data_source
            } else {
                private$m_location$data_source <- value
            }
            # }}}
        },

        wmo_number = function (value) {
            # {{{
            if (missing(value)) {
                private$m_location$wmo_number
            } else {
                private$m_location$wmo_number <- value
            }
            # }}}
        },

        latitude = function (value) {
            # {{{
            if (missing(value)) {
                private$m_location$latitude
            } else {
                private$m_location$latitude <- value
            }
            # }}}
        },

        longtitude = function (value) {
            # {{{
            if (missing(value)) {
                private$m_location$longtitude
            } else {
                private$m_location$longtitude <- value
            }
            # }}}
        },

        time_zone = function (value) {
            # {{{
            if (missing(value)) {
                private$m_location$time_zone
            } else {
                private$m_location$time_zone <- value
            }
            # }}}
        },

        elevation = function (value) {
            # {{{
            if (missing(value)) {
                private$m_location$elevation
            } else {
                private$m_location$elevation <- value
            }
            # }}}
        }

    ),
    # }}}

    public = list(

        # INITIALIZE {{{
        initialize = function (path) {
            epw_file <- parse_epw_file(path)
            private$m_path <- normalizePath(path)
            private$m_location <- epw_file$location
            private$m_header_unparsed <- epw_file$header_unparsed
            private$m_data_periods <- epw_file$data_periods
            private$m_data <- private$set_na(epw_file$data)
        },
        # }}}

        get_data = function (year = NULL, unit = TRUE) {
            # return weather data
            # {{{
            if (unit) {
                private$set_units()
            } else {
                private$drop_units()
            }
            private$m_data
            # }}}
        },

        set_data = function (data) {
            # replace weather data
            # {{{
            private$m_data <- private$set_na(data)
            # TODO:
            # set NA
            # set units
            # update data periods
            # }}}
        },

        save = function (path, overwrite = FALSE) {
            # save Epw object as an .epw file
            # {{{
            assert_that(is_string(path))
            assert_that(has_exts(path, "epw"))

            if (file.exists(path)) {
                if (!overwrite) {
                    stop("Target already exists. Please set `overwrite` to ",
                         "FALSE if you want to replace it.", call. = FALSE)
                }
            }

            loc <- paste0("LOCATION,", paste0(private$m_location, collapse = ","))

            data_periods <- private$m_data_periods
            start_date <- data_periods$start_date
            start_month <- data.table::month(start_date)
            start_day <- data.table::mday(start_date)
            start <- paste0(lpad(start_month, width = 2L), "/", lpad(start_day, width = 2L))
            end_date <- private$m_data_periods$end_date
            end_month <- data.table::month(end_date)
            end_day <- data.table::mday(end_date)
            end <- paste0(lpad(end_month, width = 2L), "/", lpad(end_day, width = 2L))
            if (data_periods$is_real_year) {
                start_year <- data.table::year(start_date)
                end_year <- data.table::year(end_date)
                start <- paste0(start, "/", start_year)
                end <- paste0(end, "/", end_year)
            }
            dp <- paste("DATA PERIODS",
                         data_periods$n_data_period,
                         data_periods$time_step,
                         "Data",
                         data_periods$start_day_of_week,
                         start, end, sep = ",")
            header <- c(loc, private$m_header_unparsed, dp)
            readr::write_lines(header, path)

            d <- data.table::copy(private$m_data)[, `:=`(datetime = NULL)]
            d <- private$drop_na(d)
            # TODO: format integerish double with tailing zeros
            data.table::fwrite(d, path, append = TRUE, col.names = FALSE, eol = "\n")
            # }}}
        },

        print = function () {
            cli::cat_line(cli::rule("EnergyPlus Weather File"))
            loc <- paste0(c("City", "State or Province", "Country", "Data Source",
                            "WMO Number", "Latitude", "Longtitude", "Time Zone",
                            "Evevation"), ":", private$m_location)
            cli::cat_bullet(loc)
        }
    ),

    private = list(
        m_path = NULL,
        m_location = NULL,
        m_header_unparsed = NULL,
        m_data_periods = NULL,
        m_data = NULL,

        set_units = function () {
            # set units to weather data if applicable
            # {{{
            units(private$m_data$dry_bulb_temperature) <- with(units::ud_units, "degC")
            units(private$m_data$dew_point_temperature) <- with(units::ud_units, "degC")
            units(private$m_data$relative_humidity) <- with(units::ud_units, "%")
            units(private$m_data$atmospheric_pressure) <- with(units::ud_units, "Pa")
            units(private$m_data$extraterrestrial_horizontal_radiation) <- with(units::ud_units, "W*h/m^2")
            units(private$m_data$extraterrestrial_direct_normal_radiation) <- with(units::ud_units, "W*h/m^2")
            units(private$m_data$horizontal_infrared_radiation_intensity_from_sky) <- with(units::ud_units, "W*h/m^2")
            units(private$m_data$global_horizontal_radiation) <- with(units::ud_units, "W*h/m^2")
            units(private$m_data$direct_normal_radiation) <- with(units::ud_units, "W*h/m^2")
            units(private$m_data$diffuse_horizontal_radiation) <- with(units::ud_units, "W*h/m^2")
            units(private$m_data$global_horizontal_illuminance) <- with(units::ud_units, "lux")
            units(private$m_data$direct_normal_illuminance) <- with(units::ud_units, "lux")
            units(private$m_data$diffuse_horizontal_illuminance) <- with(units::ud_units, "lux")
            units(private$m_data$zenith_luminance) <- with(units::ud_units, "lux")
            units(private$m_data$wind_direction) <- with(units::ud_units, "degree")
            units(private$m_data$wind_speed) <- with(units::ud_units, "m/s")
            units(private$m_data$total_sky_cover) <- with(units::ud_units, 1L)
            units(private$m_data$opaque_sky_cover) <- with(units::ud_units, 1L)
            units(private$m_data$visibility) <- with(units::ud_units, "km")
            units(private$m_data$ceiling_height) <- with(units::ud_units, "m")
            units(private$m_data$present_weather_observation) <- with(units::ud_units, 1L)
            units(private$m_data$present_weather_codes) <- with(units::ud_units, 1L)
            units(private$m_data$precipitable_water) <- with(units::ud_units, "mm")
            units(private$m_data$aerosol_optical_depth) <- with(units::ud_units, 1L)
            units(private$m_data$snow_depth) <- with(units::ud_units, "cm")
            units(private$m_data$days_since_last_snow) <- with(units::ud_units, 1L)
            units(private$m_data$albedo) <- with(units::ud_units, 1L)
            units(private$m_data$liquid_precip_depth) <- with(units::ud_units, "mm")
            units(private$m_data$liquid_precip_rate) <- with(units::ud_units, "hour")
            # }}}
        },

        drop_units = function () {
            # remove units in weather data
            # {{{
            private$m_data <- private$m_data[
                , lapply(.SD, function(x) if(inherits(x, "units")) units::drop_units(x) else x)]
            # }}}
        },

        set_na = function (x) {
            # set values to NA
            # {{{
            x[!data.table::between(dry_bulb_temperature, -70, 70), dry_bulb_temperature := NA][
              !data.table::between(dew_point_temperature, -70, 70), dew_point_temperature := NA][
              !data.table::between(relative_humidity, 0, 110), relative_humidity := NA][
              !data.table::between(atmospheric_pressure, 31000, 120000), atmospheric_pressure := NA][
              extraterrestrial_horizontal_radiation < 0 | extraterrestrial_horizontal_radiation >= 9999, extraterrestrial_horizontal_radiation := NA][
              extraterrestrial_direct_normal_radiation < 0 | extraterrestrial_direct_normal_radiation >= 9999, extraterrestrial_direct_normal_radiation := NA][
              horizontal_infrared_radiation_intensity_from_sky < 0 | horizontal_infrared_radiation_intensity_from_sky >= 9999, horizontal_infrared_radiation_intensity_from_sky := NA][
              global_horizontal_radiation < 0 | global_horizontal_radiation >= 9999, global_horizontal_radiation := NA][
              direct_normal_radiation < 0 | direct_normal_radiation >= 9999, direct_normal_radiation := NA][
              diffuse_horizontal_radiation < 0 | diffuse_horizontal_radiation >= 9999, diffuse_horizontal_radiation := NA][
              global_horizontal_illuminance < 0 | global_horizontal_illuminance >= 999900, global_horizontal_illuminance := NA][
              direct_normal_illuminance < 0 | direct_normal_illuminance >= 999900, direct_normal_illuminance := NA][
              diffuse_horizontal_illuminance < 0 | diffuse_horizontal_illuminance >= 999900, diffuse_horizontal_illuminance := NA][
              zenith_luminance < 0 | zenith_luminance >= 9999, zenith_luminance := NA][
              !data.table::between(wind_direction, 0, 360), wind_direction := NA][
              !data.table::between(wind_speed, 0, 40), wind_speed := NA][
              !data.table::between(total_sky_cover, 0, 10), total_sky_cover := NA][
              !data.table::between(opaque_sky_cover, 0, 10), opaque_sky_cover := NA][
              visibility >= 9999, visibility := NA][
              ceiling_height >= 99999, ceiling_height := NA][
              present_weather_codes >= 999999999, present_weather_codes := NA][
              precipitable_water >= 999, precipitable_water := NA][
              aerosol_optical_depth >= 0.999, aerosol_optical_depth := NA][
              snow_depth >= 999, snow_depth := NA][
              days_since_last_snow >= 99, days_since_last_snow := NA][
              albedo >= 999, albedo := NA][
              liquid_precip_depth >= 999, liquid_precip_depth := NA][
              liquid_precip_rate >= 99, liquid_precip_rate := NA]
            # }}}
        },

        drop_na = function (x) {
            # set NA to corresponding missing mark
            # {{{
            x[is.na(dry_bulb_temperature), dry_bulb_temperature := 99.9][
              is.na(dew_point_temperature), dew_point_temperature := 99.9][
              is.na(relative_humidity), relative_humidity := 999][
              is.na(atmospheric_pressure), atmospheric_pressure := 999999][
              is.na(extraterrestrial_horizontal_radiation), extraterrestrial_horizontal_radiation := 9999][
              is.na(extraterrestrial_direct_normal_radiation), extraterrestrial_direct_normal_radiation := 9999][
              is.na(horizontal_infrared_radiation_intensity_from_sky), horizontal_infrared_radiation_intensity_from_sky := 9999][
              is.na(global_horizontal_radiation), global_horizontal_radiation := 9999][
              is.na(direct_normal_radiation), direct_normal_radiation := 9999][
              is.na(diffuse_horizontal_radiation), diffuse_horizontal_radiation := 9999][
              is.na(global_horizontal_illuminance), global_horizontal_illuminance := 999999][
              is.na(direct_normal_illuminance), direct_normal_illuminance := 999999][
              is.na(diffuse_horizontal_illuminance), diffuse_horizontal_illuminance := 999999][
              is.na(zenith_luminance), zenith_luminance := 9999][
              is.na(wind_direction), wind_direction := 999][
              is.na(wind_speed), wind_speed := 999][
              is.na(total_sky_cover), total_sky_cover := 99][
              is.na(opaque_sky_cover), opaque_sky_cover := 99][
              is.na(visibility), visibility := 9999][
              is.na(ceiling_height), ceiling_height := 99999][
              is.na(present_weather_codes), present_weather_codes := 999999999][
              is.na(precipitable_water), precipitable_water := 999][
              is.na(aerosol_optical_depth), aerosol_optical_depth := 0.999][
              is.na(snow_depth), snow_depth := 999][
              is.na(days_since_last_snow), days_since_last_snow := 99][
              is.na(albedo), albedo := 999][
              is.na(liquid_precip_depth), liquid_precip_depth := 999][
              is.na(liquid_precip_rate), liquid_precip_rate := 99]
            # }}}
        }
    )
)
# }}}

#' @importFrom readr read_lines
#' @importFrom stringr str_trim
#' @importFrom data.table fread setnames shift month minute mday year
#' @importFrom fasttime fastPOSIXct
# parse_epw_file {{{
parse_epw_file <- function (path, strict = TRUE) {
    header <- readr::read_lines(path, n_max = 8L)
    header <- stringr::str_trim(header, "left")

    # parse first line: "LOCATION"
    # {{{
    # LOCATION, city, stateProvinceRegion, country, dataSource, wmoNumber, latitude, longitude, timeZone, elevation
    loc <- stringr::str_trim(strsplit(header[1], ",")[[1]])
    len_loc <- length(loc)
    if (len_loc < 10L) {
        stop("Expected 10 location fields rather than ", backtick(len_loc),
             " fields in EPW file ", backtick(path), ".", call. = FALSE)
    } else if (len_loc > 10L) {
        warning("Expected 10 location fields rather than ", backtick(len_loc),
                " fields in EPW file ", backtick(path), ".", call. = FALSE)
    }

    if (loc[1] != "LOCATION") {
        stop("Missing `LOCATION` specifier in EPW file ", backtick(path), ".",
             call. = FALSE)
    }

    location <- list()
    location[["city"]] <- loc[2]
    location[["state"]] <- loc[3]
    location[["country"]] <- loc[4]
    location[["data_source"]] <- loc[5]
    location[["wmo_number"]] <- loc[6]
    location[["latitude"]] <- as.double(loc[7])
    location[["longtitude"]] <- as.double(loc[8])
    location[["time_zone"]] <- as.double(loc[9])
    location[["elevation"]] <- as.double(loc[10])
    if (is.na(location[["latitude"]])) {
        stop("Non-numerical latitude found in EPW file ", backtick(path),
             ": ", backtick(loc[7]), ".", call. = FALSE)
    }
    if (is.na(location[["longtitude"]])) {
        stop("Non-numerical longtitude found in EPW file ", backtick(path),
             ": ", backtick(loc[8]), ".", call. = FALSE)
    }
    if (is.na(location[["time_zone"]])) {
        stop("Non-numerical time zone found in EPW file ", backtick(path),
             ": ", backtick(loc[9]), ".", call. = FALSE)
    }
    if (is.na(location[["elevation"]])) {
        stop("Non-numerical elevation found in EPW file ", backtick(path),
             ": ", backtick(loc[10]), ".", call. = FALSE)
    }
    # }}}

    # parse 8th line: "DATA PERIODS"
    # {{{
    # DATA PERIODS, nDataPeriods, timeStep, startDayOfWeek, startDate, endDate
    # NOTE THAT ONLY ONE DATA PERIOD IS SUPPORTED
    dp <- stringr::str_trim(strsplit(header[8], ",")[[1]])
    len_dp <- length(dp)
    if(len_dp < 7L) {
        stop("Expected 7 data period fields rather than the ", backtick(len_dp),
             " fields in EPW file ", backtick(path), ".", call. = FALSE)
    } else if(len_dp > 7L) {
        warning("Expected 7 data period fields rather than the ", backtick(len_dp),
                " fields in EPW file ", backtick(path), ".", call. = FALSE)
    }
    if (dp[1] != "DATA PERIODS") {
        stop("Missing `DATA PERIODS` specifier in EPW file ", backtick(path), ".",
             call. = FALSE)
    }
    data_periods <- list()
    data_periods[["n_data_periods"]] <- as.integer(dp[2])
    data_periods[["time_step"]] <- as.integer(dp[3])
    data_periods[["start_day_of_week"]] <- dp[5]
    data_periods[["start_date"]] <- dp[6]
    data_periods[["end_date"]] <- dp[7]
    data_periods[["start_date_actual_year"]] <- NA_integer_
    data_periods[["end_date_actual_year"]] <- NA_integer_
    n <- data_periods[["n_data_periods"]]
    if (is.na(n)) {
        stop("Non-integral number of data periods in EPW file ", backtick(path),
             ": ", backtick(dp[2]), ".", call. = FALSE)
    } else if (n > 1L){
        stop("More than one data period in EPW file ", backtick(path),
             ": ", backtick(n), ", which is not supported", call. = FALSE)
    }

    ts <- data_periods[["time_step"]]
    if (is.na(ts)) {
        stop("Non-integral number of timestep in EPW file ", backtick(path), ".",
             ": ", backtick(dp[3]), ".", call. = FALSE)
    } else if (60L %% ts != 0L){
        stop("Number of records per hour of ", backtick(ts), " does not result ",
             "in integral number of minutes between records in EPW file ", backtick(path),
             call. = FALSE);
    }

    dw <- get_epw_week_day(data_periods[["start_day_of_week"]])
    if (is.na(dw)) {
        stop("Bad start day of week found in EPW file ", backtick(path), ": ",
             backtick(dp[5]), ".", call. = FALSE)
    }

    start_date <- get_epw_date(path, dp[6], dw, "start")
    end_date <- get_epw_date(path, dp[7], dw, "end")
    real_year <- attr(start_date, "real_year")
    data_periods[["start_date"]] <- start_date
    data_periods[["end_date"]] <- end_date
    data_periods[["is_real_year"]] <- real_year
    # }}}

    # store unparsed head lines
    header_unparsed <- header[2L:7L]

    # parse the rest of file
    # colnames refers to column "Long Name" in Table 2.8 in
    # "AuxiliaryPrograms.pdf" of EnergyPlus 8.6
    # {{{
    epw_data <- data.table::fread(path, skip = 8L)
    if (ncol(epw_data) != 35L) {
        stop("Expected 35 fields in EPW data instead of the ",
             backtick(ncol(epw_data)), " recieved in EPW file ",
             backtick(path), ".", call. = FALSE)
    }

    data.table::setnames(epw_data, c(
        "year", "month", "day", "hour", "minute", "datasource",
        "dry_bulb_temperature",
        "dew_point_temperature",
        "relative_humidity",
        "atmospheric_pressure",
        "extraterrestrial_horizontal_radiation",
        "extraterrestrial_direct_normal_radiation",
        "horizontal_infrared_radiation_intensity_from_sky",
        "global_horizontal_radiation",
        "direct_normal_radiation",
        "diffuse_horizontal_radiation",
        "global_horizontal_illuminance",
        "direct_normal_illuminance",
        "diffuse_horizontal_illuminance",
        "zenith_luminance",
        "wind_direction",
        "wind_speed",
        "total_sky_cover",
        "opaque_sky_cover",
        "visibility",
        "ceiling_height",
        "present_weather_observation",
        "present_weather_codes",
        "precipitable_water",
        "aerosol_optical_depth",
        "snow_depth",
        "days_since_last_snow",
        "albedo",
        "liquid_precip_depth",
        "liquid_precip_rate"
    ))
    # }}}
    # error checking
    # {{{
    epw_data[, datetime := fasttime::fastPOSIXct(
        paste0(year, "-", month, "-", day, " ", hour, ":", minute, ":00"),
        required.components = 5L, tz = "GMT")]

    epw_data[, datetime_shifted := data.table::shift(datetime)]
    epw_data[, datetime_delta := difftime(datetime, datetime_shifted, units = "days")]
    # check datetime step
    gt_1day <- epw_data[datetime_delta > 1, which = TRUE]
    if (not_empty(gt_1day) && data_periods[["is_real_year"]]) {
        warning("Successive data points (", backtick(epw_data[gt_1day[1], datetime]), " to ",
                 backtick(epw_data[gt_1day[1], datetime_shifted]), ", ending on line ",
                 backtick((gt_1day[1] + 8L)), ") are greater than 1 day apart in EPW file ",
                 backtick(path), ". Data will be treated as typical (TMY).",
                 call. = FALSE)
    }
    # check if warp around
    epw_data[, dt_month := data.table::month(datetime)]
    epw_data[, dt_month_shifted := data.table::month(datetime_shifted)]
    wrap_around <- epw_data[-.N][dt_month < dt_month_shifted, .N > 0L]

    # check for agreement between the file value and the computed value
    min_per_rcd <- 60L/data_periods[["time_step"]]
    min_seq <- seq(0, 60, length.out = data_periods[["time_step"]] + 1L)[-1L]
    epw_data[, dt_minute := data.table::minute(datetime)]
    epw_data[, dt_minute_cal := {rep(min_seq, nrow(epw_data))}][
        dt_minute_cal == 60L, dt_minute_cal := 0L]
    min_mismatch <- epw_data[dt_minute != dt_minute_cal, which = TRUE]
    if (not_empty(min_mismatch)) {
        warning("Minutes field (", backtick(epw_data[min_mismatch[1], minute]), ") on line ",
                backtick(min_mismatch[1]), " of EPW file ", backtick(path),
                " does not agree with computed value (",
                backtick(epw_data[min_mismatch[1], dt_minute_cal]),
                "). Using computed value.")
        data_periods[["minute_match"]] <- FALSE
    } else {
        data_periods[["minute_match"]] <- TRUE
    }

    # check NAs
    epw_data[, `:=`(datetime_shifted = NULL, datetime_delta = NULL,
                    dt_month = NULL, dt_month_shifted = NULL,
                    dt_minute = NULL, dt_minute_cal = NULL)]
    na_epw_data <- na.omit(epw_data, invert = TRUE)
    if (not_empty(na_epw_data)) {
        stop("Invalid weather data line found in EPW file ", backtick(path), call. = FALSE)
    }

    # check start date and end date mismatching
    first_date <- as.Date(epw_data[1L, datetime])
    if (data.table::month(first_date) != data.table::month(start_date) ||
        data.table::mday(first_date) != data.table::mday(start_date)) {
        stop("Header start date does not match data in EPW file ", backtick(path), call. = FALSE)
    }
    last_date_list <- epw_data[.N, list(datetime, hour, minute)]
    if (last_date_list[["hour"]] == 24L && last_date_list[["minute"]] == 0L) {
        last_date <- last_date_list[["datetime"]] - lubridate::days(1)
    } else {
        last_date <- last_date_list[["datetime"]]
    }
    if (data.table::month(last_date) != data.table::month(end_date) ||
        data.table::mday(last_date) != data.table::mday(end_date)) {
        stop("Header end date does not match data in EPW file ", backtick(path), call. = FALSE)
    }
    if (real_year) {
        if (weekdays(first_date) != weekdays(first_date)) {
            warning("Header start day of the week and actual start day of the ",
                    "week do not match in EPW file ", backtick(path), ". Data ",
                    "will be treated as typical (TMY)", call. = FALSE)
            data_periods[["is_real_year"]] <- FALSE
        } else {
            data_periods[["start_date_actual_year"]] <- data.table::year(start_date)
            data_periods[["end_date_actual_year"]] <- data.table::year(end_date)
        }
    }
    if (!real_year && wrap_around) {
        stop("Wrap around years not supported for TMY data, EPW file ", backtick(path), call. = FALSE)
    }
    # }}}

    # set column order
    data.table::setcolorder(epw_data, c("datetime", names(epw_data)[-ncol(epw_data)]))

    list(location = location, data_periods = data_periods, data = epw_data,
         header_unparsed = header_unparsed)
}
# }}}

# get_epw_week_day {{{
get_epw_week_day <- function (x, num = FALSE){
    l_x <- tolower(x)

    std <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
             "Sunday")
    abbr <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

    l_std <- tolower(std)
    l_abbr <- tolower(abbr)

    found <- l_x == l_std
    if (sum(found) == 1L) {
        res <- std[found]
    } else {
        found <- l_x == l_abbr
        if (sum(found) == 1L) {
            res <- std[found]
        } else {
            res <- NA_character_
        }
    }

    if (num) {
        res <- (1L:7L)[found]
    }

    res
}
# }}}
# get_epw_year {{{
get_epw_year <- function (year = NULL, start_week_day, leap_year = FALSE) {
    # start out assuming 2009
    year <- as.integer(year %||% 2009L)
    targ <- as.character(get_epw_week_day(start_week_day, num = TRUE))
    while (lubridate::leap_year(year) != leap_year ||
           strftime(paste0(year, "-1-1"),'%u') != targ) {
        year <- year - 1L
    }

    year
}
# }}}
# get_epw_date {{{
get_epw_date <- function (path, x, start_day_of_week, type = c("start", "end")) {
    type <- match.arg(type)

    s <- stringr::str_trim(strsplit(x, "/")[[1]])
    len_s <- length(s)
    if (len_s != 2L && len_s != 3L) {
        stop("Bad data period ", type, " date format found in EPW file ",
             backtick(path), ":", backtick(x), ".", call. = FALSE)
    } else if (len_s == 2L) {
        assume_year <- get_epw_year(year = NULL, start_day_of_week)
        res <- as.Date(paste0(assume_year, "-", s[1], "-", s[2]))
        real_year <- FALSE
    } else {
        year <- s[3]
        year <- get_epw_year(year, start_day_of_week)
        res <- as.Date(paste0(year, "-", s[1], "-", s[2]))
        real_year <- TRUE
    }

    data.table::setattr(res, "real_year", real_year)
    res
}
# }}}
