#' @importFrom R6 R6Class
#' @importFrom data.table fread setnames shift copy fwrite between as.ITime setDT
#' @importFrom readr write_lines
#' @importFrom cli cat_line rule cat_bullet
#' @importFrom crayon bold
#' @importFrom lubridate year month mday hour minute leap_year days force_tz
#' @importFrom units ud_units drop_units
#' @importFrom stringr str_trim
#' @importFrom fasttime fastPOSIXct
#' @importFrom stats na.omit
NULL

#' Read, and modify an EnergyPlus Weather File (EPW)
#'
#' Reading an EPW file starts with function [read_epw()], which parses an EPW
#' file and returns an `Epw` object. The parsing process is extremely inspired
#' by [OpenStudio utilities library](https://openstudio-sdk-documentation.s3.amazonaws.com/cpp/OpenStudio-2.5.1-doc/utilities/html/classopenstudio_1_1_epw_file.html)
#' with some simplifications.
#'
#' An EPW file can be divided into two parts, headers and weather data.  The
#' first eight lines of a standard EPW file are normally headers which contains
#' data of location, design conditions, typical/extreme periods, ground
#' temperatures, holidays/daylight savings, data periods and other comments. For
#' now, eplusr only parses headers of location, holidays/daylight savings and
#' data periods. All other headers are left as they were when parsing and
#' saving. For details on the data structure of EPW file, please see "Chapter 2 - Weather Converter Program"
#' in EnergyPlus "Auxiliary Programs" documentation. An online version can be
#' found [here](https://bigladdersoftware.com/epx/docs/).
#'
#' There are about 35 variables in the core weather data. However, not all of
#' them are used by EnergyPlus. Actually, despite of date and time columns, only
#' 14 columns are used:
#'
#'  1. dry bulb temperature
#'  2. dew point temperature
#'  3. relative humidity
#'  4. atmospheric pressure
#'  5. horizontal infrared radiation intensity from sky
#'  6. direct normal radiation
#'  7. diffuse horizontal radiation
#'  8. wind direction
#'  9. wind speed
#' 10. present weather observation
#' 11. present weather codes
#' 12. snow depth
#' 13. liquid precipitation depth
#' 14. liquid precipitation rate
#'
#' **NOTE**: Even though `Epw` class provides methods to replace core weather data,
#' it is still not recommended.
#'
#' @section Usage:
#' ```
#' epw <- read_epw(path)
#' epw$city
#' epw$city <- "city"
#' epw$state_province
#' epw$state_province <- "state_province"
#' epw$country
#' epw$country <- "country"
#' epw$data_source
#' epw$data_source <- "data_source"
#' epw$wmo_number
#' epw$wmo_number <- "wmo_number"
#' epw$latitude
#' epw$latitude <- "latitude"
#' epw$longitute
#' epw$longitute <- "longitute"
#' epw$time_zone
#' epw$time_zone <- "time_zone"
#' epw$elevation
#' epw$elevation <- "elevation"
#' epw$time_step
#' epw$time_step <- "time_step"
#' epw$start_day_of_week
#' epw$start_day_of_week <- "start_day_of_week"
#' epw$path()
#' epw$get_data(year = NULL, unit = FALSE, tz = Sys.timezone(), update = FALSE)
#' epw$set_data(data)
#' epw$save(path, overwrite = FALSE)
#' epw$print()
#' print(epw)
#' ```
#'
#' @section Read:
#' ```
#' epw <- read_epw(path)
#' ```
#'
#' **Arguments**
#'
#' * `path`: Path of an EnergyPlus `EPW` file.
#'
#' @section Query and Modify Header:
#' ```
#' epw$city
#' epw$city <- "city"
#' epw$state_province
#' epw$state_province <- "state_province"
#' epw$country
#' epw$country <- "country"
#' epw$data_source
#' epw$data_source <- "data_source"
#' epw$wmo_number
#' epw$wmo_number <- "wmo_number"
#' epw$latitude
#' epw$latitude <- "latitude"
#' epw$longitute
#' epw$longitute <- "longitute"
#' epw$time_zone
#' epw$time_zone <- "time_zone"
#' epw$elevation
#' epw$elevation <- "elevation"
#' epw$time_step
#' epw$time_step <- "time_step"
#' epw$start_day_of_week
#' epw$start_day_of_week <- "start_day_of_week"
#' ```
#'
#' `$city`, `$state_province`, `$country`, `$data_source`, `$wmo_number`,
#' `$latitude`, `$longitute`, `$time_zone`, `$elevation`, `$time_step` and
#' `$start_day_of_week` are all active bindings, which means that you can get
#' the value and also set new value to it.
#'
#' **NOTE**: Please be super careful when trying to modify those data. Some of them
#' must be consistent with weather data in order to make the weather file
#' successfully parsed by EnergyPlus and eplusr.
#'
#' @section Query and Modify Data:
#' ```
#' epw$path()
#' epw$get_data(year = NULL, unit = FALSE, tz = Sys.timezone(), update = FALSE)
#' epw$set_data(data)
#' ```
#'
#' `$path()` returns the path of EPW file. `NULL` if Epw is not created from
#'     local file.
#'
#' `$get_data()` returns the core weather data in a data.table.
#'
#' `$set_data()` replaces core weather data with input data. NOTE: This feature is
#'     experimental. There is no validation when replacing.
#'
#' **Arguments**
#'
#' * `year`: A integer to indicate the year value in the return `datetime` column.
#'     If `NULL`, which is the default, the year is left as it is in EPW file.
#' * `tz`: The time zone of Date and Time in `datetime` column. Default:
#'   value of `Sys.timezone()`.
#' * `unit`: If `TRUE`, units are set to all numeric columns using
#'     [units::set_units()]. Default: `FALSE`.
#' * `update`: If `TRUE`, not only `datetime` column, but also `year`, `month`,
#'     `day`, `hour` and `minute` are also updated according to the input
#'     `year` value. Default: FALSE
#' * `data`: A data.frame which has all required columns.
#'
#' @section Read:
#' ```
#' epw$save(path, overwrite = FALSE)
#' ```
#' **Arguments**
#'
#' * `path`: A path where to save the weather file. If `NULL`, the path of the
#'     weather file itself is used.
#' * `overwrite`: Whether to overwrite the file if it already exists. Default is
#'     `FALSE`.
#' @examples
#' \dontrun{
#' # read an EPW file from EnergyPlus website
#' path_base <- "https://energyplus.net/weather-download"
#' path_region <- "north_and_central_america_wmo_region_4/USA/CA"
#' path_file <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#' path_epw <- file.path(path_base, path_region, path_file)
#' epw <- read_epw(path_epw)
#'
#' # read an EPW file distributed with EnergyPlus
#' if (is_avail_eplus(8.8)) {
#'     epw_path <- file.path(
#'         eplus_config(8.8)$dir,
#'         "WeatherData",
#'         "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'     )
#'     epw <- read_epw(path_epw)
#' }
#'
#' # get file path
#' epw$path()
#'
#' # get basic info
#' epw$city
#' epw$state_province
#' epw$country
#' epw$data_source
#' epw$wmo_number
#' epw$latitude
#' epw$longitude
#' epw$time_zone
#' epw$elevation
#' epw$time_step
#' epw$start_day_of_week
#'
#' # set basic info
#' # NOTE: Use with caution. May mess up your weather data
#' epw$city <- "Chongqing"
#' epw$city
#'
#' epw$state_province <- "Chongqing"
#' epw$state_province
#'
#' epw$country <- "China"
#' epw$country
#'
#' epw$data_source <- "TMY"
#' epw$data_source
#'
#' epw$wmo_number <- "724944"
#' epw$wmo_number
#'
#' epw$latitude <- 20.0
#' epw$latitude
#'
#' epw$longitude <- -120.0
#' epw$longitude
#'
#' epw$time_zone <- 8
#' epw$time_zone
#'
#' epw$elevation <- 100
#' epw$elevation
#'
#' epw$time_step <- 2
#' epw$time_step
#'
#' epw$start_day_of_week <- "Monday"
#' epw$start_day_of_week
#'
#' # get weather data
#' str(epw$get_data())
#'
#' # get weather data but change the year to 2018
#' # the year column is not changed by default, only the returned datetime column
#' str(epw$get_data(year = 2018)$datetime)
#' str(epw$get_data(year = 2018)$year)
#' # you can force to update the year column
#' str(epw$get_data(year = 2018, update = TRUE)$year)
#'
#' # get weather data with units
#' str(epw$get_data(unit = TRUE))
#' # with units specified, you can easily perform unit conversion using units
#' # package
#' t_dry_bulb <- epw$get_data(unit = TRUE)$dry_bulb_temperature
#' units(t_dry_bulb) <- with(units::ud_units, "kelvin")
#' str(t_dry_bulb)
#'
#' # change the time zone of datetime column in the returned weather data
#' attributes(epw$get_data()$datetime)
#' attributes(epw$get_data(tz = "America/Chicago")$datetime)
#'
#' # change the weather data
#' # NOTE: This feature is experimental. There is no validation when replacing.
#' epw$set_data(epw$get_data())
#' # save the weather file
#' epw$save(file.path(tempdir(), "weather.epw"))
#' }
#' @docType class
#' @name Epw
#' @author Hongyuan Jia
NULL

#' Read and Parse EnergyPlus Weather File (EPW)
#'
#' `read_epw()` parses an EPW file and returns an `Epw` object. The parsing
#' process is extreme inspired by [OpenStudio utilities library](https://openstudio-sdk-documentation.s3.amazonaws.com/cpp/OpenStudio-2.5.1-doc/utilities/html/classopenstudio_1_1_epw_file.html)
#' with some simplifications. For more details on `Epw`, please see [Epw] class.
#'
#' @param path A path of an EnergyPlus `EPW` file.
#' @return An `Epw` object.
#' @examples
#' # read an EPW file from EnergyPlus v8.8 installation folder
#' if (is_avail_eplus(8.8)) {
#'     path_epw <- file.path(
#'         eplus_config(8.8)$dir,
#'         "WeatherData",
#'         "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'    )
#'    epw <- read_epw(path_epw)
#' }
#'
#' \dontrun{
#' # read an EPW file from EnergyPlus website
#' path_base <- "https://energyplus.net/weather-download"
#' path_region <- "north_and_central_america_wmo_region_4/USA/CA"
#' path_file <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#' path_epw <- file.path(path_base, path_region, path_file)
#' epw <- read_epw(path_epw)
#' }
#' @seealso [Epw] class
#' @author Hongyuan Jia
#' @export
# read_epw {{{
read_epw <- function (path) {
    Epw$new(path)
}
# }}}

# Epw {{{
Epw <- R6::R6Class(classname = "Epw",
    # ACTIVE {{{
    active = list(
        city = function (value) {
            # {{{
            if (missing(value)) {
                private$m_header$location$city
            } else {
                private$m_header$location$city <- value
            }
            # }}}
        },

        state_province = function (value) {
            # {{{
            if (missing(value)) {
                private$m_header$location$state
            } else {
                private$m_header$location$state <- value
            }
            # }}}
        },

        country = function (value) {
            # {{{
            if (missing(value)) {
                private$m_header$location$country
            } else {
                private$m_header$location$country <- value
            }
            # }}}
        },

        data_source = function (value) {
            # {{{
            if (missing(value)) {
                private$m_header$location$data_source
            } else {
                private$m_header$location$data_source <- value
            }
            # }}}
        },

        wmo_number = function (value) {
            # {{{
            if (missing(value)) {
                private$m_header$location$wmo_number
            } else {
                private$m_header$location$wmo_number <- value
            }
            # }}}
        },

        latitude = function (value) {
            # {{{
            if (missing(value)) {
                private$m_header$location$latitude
            } else {
                private$m_header$location$latitude <- value
            }
            # }}}
        },

        longitude = function (value) {
            # {{{
            if (missing(value)) {
                private$m_header$location$longitude
            } else {
                private$m_header$location$longitude <- value
            }
            # }}}
        },

        time_zone = function (value) {
            # {{{
            if (missing(value)) {
                private$m_header$location$time_zone
            } else {
                private$m_header$location$time_zone <- value
            }
            # }}}
        },

        elevation = function (value) {
            # {{{
            if (missing(value)) {
                private$m_header$location$elevation
            } else {
                private$m_header$location$elevation <- value
            }
            # }}}
        },

        time_step = function (value) {
            # {{{
            if (missing(value)) {
                private$m_header$data_periods$time_step
            } else {
                private$m_header$data_periods$time_step <- value
            }
            # }}}
        },

        start_day_of_week = function (value) {
            # {{{
            if (missing(value)) {
                private$m_header$data_periods$start_day_of_week
            } else {
                private$m_header$data_periods$start_day_of_week <- value
            }
            # }}}
        }

    ),
    # }}}

    public = list(

        # INITIALIZE {{{
        initialize = function (path) {
            if (is_string(path)) {
                if (file.exists(path)) {
                    private$m_path <- normalizePath(path)
                }
            }

            epw_file <- parse_epw_file(path)
            private$m_header <- epw_file$header
            private$m_data <- private$set_na(epw_file$data)
        },
        # }}}

        path = function () {
            # {{{
            private$m_path
            # }}}
        },

        get_data = function (year = NULL, unit = FALSE, tz = Sys.timezone(),
                             update = FALSE) {
            # return weather data
            # {{{
            if (unit) {
                private$set_units()
            } else {
                private$drop_units()
            }

            d <- data.table::copy(private$m_data)
            if (!is.null(year)) {
                assert_that(is_integerish(year))
                # NOTE: if use a variable `year` on the RHS of
                # `lubridate::year<-`, it will fail.
                y <- as.integer(year)
                if (private$m_header$data_periods$n_data_periods > 1L) {
                    warning("Epw file has more than 1 data periods. ",
                            "Year will be applied to all periods which will ",
                            "introduce duplicated date time.", call. = FALSE)
                }
                if (private$m_header$holidays_daylight_savings$leap_year &
                    !lubridate::leap_year(year)) {
                    stop("Epw file contains data of a leap year. Input year ",
                         backtick(year), " is not a leap year.", call. = FALSE)
                }
                if (!private$m_header$holidays_daylight_savings$leap_year &
                    lubridate::leap_year(year)) {
                    warning("Input year ", backtick(year), " is a leap year ",
                            "while Epw file only contains data of a non-leap year. ",
                            "Datetime inconsistency may occur.", call. = FALSE)
                }

                d[, `:=`(dt_month = lubridate::month(datetime),
                         dt_day = lubridate::mday(datetime),
                         dt_hour = lubridate::hour(datetime),
                         dt_minute = lubridate::minute(datetime),
                         line = .I)]
                l_before_change <- d[dt_month == 12 & dt_day == 31 & dt_hour == 23 & dt_minute < 60, line[.N]]
                l_after_change <- d[dt_month == 1 & dt_day == 1 & dt_hour == 0 & dt_minute == 0, line[.N]]
                if (l_before_change + 1L == l_after_change) {
                    d[line < l_after_change, `:=`(datetime = {lubridate::year(datetime) <- y; datetime})]
                    d[line >=l_after_change, datetime := {lubridate::year(datetime) <- y + 1L; datetime}]
                } else {
                    stop("Failed to detect the first line of the next year.",
                         call. = FALSE)
                }

                d[, c("dt_month", "dt_day", "dt_hour", "dt_minute", "line") := NULL]
                if (update) {
                    d <- format_epw_date(d)
                }
            }

            if (attr(d$datetime, "tzone") != tz) {
                d[, datetime := lubridate::force_tz(datetime, tz)]
            }

            d[]
            # }}}
        },

        set_data = function (data) {
            # replace weather data
            # {{{
            assert_that(has_names(data, names(private$m_data)))
            data.table::setDT(data)
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
                         "TRUE if you want to replace it.", call. = FALSE)
                }
            }
            header <- private$m_header
            header$location <- paste0("LOCATION,", paste0(private$m_header$location, collapse = ","))

            data_periods <- private$m_header$data_periods
            start_date <- data_periods$start_date
            start_month <- lubridate::month(start_date)
            start_day <- lubridate::mday(start_date)
            start <- paste0(lpad(start_month, width = 2L), "/", lpad(start_day, width = 2L))
            end_date <- private$m_header$data_periods$end_date
            end_month <- lubridate::month(end_date)
            end_day <- lubridate::mday(end_date)
            end <- paste0(lpad(end_month, width = 2L), "/", lpad(end_day, width = 2L))
            if (data_periods$is_real_year) {
                start_year <- lubridate::year(start_date)
                end_year <- lubridate::year(end_date)
                start <- paste0(start, "/", start_year)
                end <- paste0(end, "/", end_year)
            }
            header$data_periods <- paste("DATA PERIODS",
                         data_periods$n_data_period,
                         data_periods$time_step,
                         "Data",
                         data_periods$start_day_of_week,
                         start, end, sep = ",")

            hldys_dlt_svgs <- private$m_header$holidays_daylight_savings
            hldys_dlt_svgs$leap_year <- ifelse(hldys_dlt_svgs$leap_year, "yes", "no")
            if (is.na(hldys_dlt_svgs$dls_start_day)) hldys_dlt_svgs$dls_start_day <- "0"
            if (is.na(hldys_dlt_svgs$dls_end_day)) hldys_dlt_svgs$dls_end_day <- "0"
            if (hldys_dlt_svgs$num_of_holiday == 0L) {
                hldys_dlt_svgs$holidays <- NULL
            }
            header$holidays_daylight_savings <- paste0(
                c("HOLIDAYS/DAYLIGHT SAVINGS", hldys_dlt_svgs), collapse = ",")

            header <- Filter(not_empty, header)
            write_lines_eol(header, path)

            d <- data.table::copy(private$m_data)[, `:=`(datetime = NULL)]
            d <- private$drop_na(d)
            # TODO: format integerish double with tailing zeros
            data.table::fwrite(d, path, append = TRUE, col.names = FALSE, eol = "\n")
            private$m_path <- path
            # }}}
        },

        print = function () {
            # print
            # {{{
            cli::cat_line(crayon::bold(cli::rule("Location")), col = "green")
            loc_keys <- c(
                crayon::bold("[ City    ]"),
                crayon::bold("[ State   ]"),
                crayon::bold("[ Country ]"),
                crayon::bold("[ Source  ]"),
                crayon::bold("[ WMO Num ]"),
                crayon::bold("[Latitude ]"),
                crayon::bold("[Longitude]"),
                crayon::bold("[Time Zone]"),
                crayon::bold("[Evevation]"))
            loc_str <- paste0(loc_keys, ": ", private$m_header$location)
            cli::cat_bullet(loc_str, col = "cyan", bullet_col = "cyan")

            cli::cat_line("\n", cli::rule(crayon::bold("Data Period"), col = "green"))
            dp_keys <- c(
                crayon::bold("[Period Num ]"),
                crayon::bold("[Time Step  ]"),
                crayon::bold("[Date Range ]"),
                crayon::bold("[1st Weekday]"),
                crayon::bold("[Real Year  ]"))
            dp <- private$m_header$data_periods
            num <- dp$n_data_periods
            time_step <- paste0(60/ dp$time_step, " min")
            start_date <- format(dp$start_date, "%b %d")
            end_date <- format(dp$end_date, "%b %d")
            range <- paste0(start_date, " - ", end_date)
            weekday <- dp$start_day_of_week
            real_year <- dp$is_real_year
            dp_str <- paste0(dp_keys, ": ", c(num, time_step, range, weekday))
            cli::cat_bullet(dp_str, col = "cyan", bullet_col = "cyan")

            cli::cat_line("\n", cli::rule(crayon::bold("Holidays and Daylight Savings"), col = "green"))
            dst_keys <- c(
                crayon::bold("[ Leap Year ]"),
                crayon::bold("[ DST Range ]"),
                crayon::bold("[Holiday Num]"))
            dst <- private$m_header$holidays_daylight_savings
            if (!is.na(dst$dls_start_day)) {
                dst_range <- paste0(dst$dls_start_day, " - ", dst$dls_end_day)
            } else {
                dst_range <- NA
            }
            hol_num <- dst$num_of_holiday
            dst_str <- paste0(dst_keys, ": ", list(dst$leap_year, dst_range, hol_num))
            cli::cat_bullet(dst_str, col = "cyan", bullet_col = "cyan")
            # }}}
        }
    ),

    private = list(
        m_path = NULL,
        m_header = NULL,
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
              is.na(present_weather_codes), present_weather_codes := "999999999"][
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

# parse_epw_file {{{
parse_epw_file <- function (path, strict = TRUE) {
    assert_that(is_scalar(path))

    is_path <- file.exists(path)
    error_end <- ifelse(is_path, paste0(" ", backtick(path)), "")

    num_header <- 8L
    header <- readr::read_lines(path, n_max = num_header)
    header <- stringr::str_trim(header, "left")

    # read head pairs {{{
    header_key <- c("LOCATION", "DESIGN CONDITIONS", "TYPICAL/EXTREME PERIODS",
        "GROUND TEMPERATURES", "HOLIDAYS/DAYLIGHT SAVINGS", "COMMENTS 1",
        "COMMENTS 2", "DATA PERIODS" )
    l_header_pairs <- lapply(header_key, grep, x = header, fixed = TRUE)
    names(l_header_pairs) <- header_key
    missing_header <- vapply(l_header_pairs, is_empty, FUN.VALUE = logical(1))
    to_parse <- c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)

    if (any(missing_header)) {
        if (any(missing_header[to_parse]))
            stop("Missing ", backtick_collapse(names(which(missing_header[to_parse]))),
                " specifier in EPW file", error_end, ".", call. = FALSE)

        l_unknown <- seq_len(num_header)[missing_header]
        l_known <- as.integer(l_header_pairs[!missing_header])

        if (any(l_unknown < max(l_known)))
            stop("Invalid header line found in EPW file", error_end, ": \n",
                 paste0(backtick(header[l_unknown]), collapse = "\n"),
                 call. = FALSE)

        num_header <- max(l_known)
    }
    header_pairs <- lapply(l_header_pairs, function (x) header[x])
    names(header_pairs) <- tolower(gsub("[^[:alnum:]]", "_", names(header_pairs)))
    # }}}

    # parse line: "LOCATION"
    # {{{
    # LOCATION, city, stateProvinceRegion, country, dataSource, wmoNumber, latitude, longitude, timeZone, elevation
    loc <- stringr::str_trim(strsplit(header_pairs$location, ",")[[1]])
    len_loc <- length(loc)
    if (len_loc < 10L) {
        stop("Expected 10 location fields rather than ", backtick(len_loc),
             " fields in EPW file", error_end, ".", call. = FALSE)
    } else if (len_loc > 10L) {
        warning("Expected 10 location fields rather than ", backtick(len_loc),
                " fields in EPW file", error_end, ".", call. = FALSE)
    }

    location <- list()
    location[["city"]] <- loc[2]
    location[["state"]] <- loc[3]
    location[["country"]] <- loc[4]
    location[["data_source"]] <- loc[5]
    location[["wmo_number"]] <- loc[6]
    location[["latitude"]] <- suppressWarnings(as.double(loc[7]))
    location[["longitude"]] <- suppressWarnings(as.double(loc[8]))
    location[["time_zone"]] <- suppressWarnings(as.double(loc[9]))
    location[["elevation"]] <- suppressWarnings(as.double(loc[10]))
    if (is.na(location[["latitude"]])) {
        stop("Non-numerical latitude found in EPW file", error_end,
             ": ", backtick(loc[7]), ".", call. = FALSE)
    }
    if (is.na(location[["longitude"]])) {
        stop("Non-numerical longitude found in EPW file", error_end,
             ": ", backtick(loc[8]), ".", call. = FALSE)
    }
    if (is.na(location[["time_zone"]])) {
        stop("Non-numerical time zone found in EPW file", error_end,
             ": ", backtick(loc[9]), ".", call. = FALSE)
    }
    if (is.na(location[["elevation"]])) {
        stop("Non-numerical elevation found in EPW file", error_end,
             ": ", backtick(loc[10]), ".", call. = FALSE)
    }
    # }}}

    # parse line: "DATA PERIODS"
    # {{{
    # DATA PERIODS, nDataPeriods, timeStep, startDayOfWeek, startDate, endDate
    # NOTE THAT ONLY ONE DATA PERIOD IS SUPPORTED
    dp <- stringr::str_trim(strsplit(header_pairs$data_periods, ",")[[1]])
    len_dp <- length(dp)
    if(len_dp < 7L) {
        stop("Expected 7 data period fields rather than the ", backtick(len_dp),
             " fields in EPW file", error_end, ".", call. = FALSE)
    } else if(len_dp > 7L) {
        warning("Expected 7 data period fields rather than the ", backtick(len_dp),
                " fields in EPW file", error_end, ".", call. = FALSE)
    }
    data_periods <- list()
    data_periods[["n_data_periods"]] <- suppressWarnings(as.integer(dp[2]))
    data_periods[["time_step"]] <- suppressWarnings(as.integer(dp[3]))
    data_periods[["start_day_of_week"]] <- dp[5]
    data_periods[["start_date"]] <- dp[6]
    data_periods[["end_date"]] <- dp[7]
    data_periods[["start_date_actual_year"]] <- NA_integer_
    data_periods[["end_date_actual_year"]] <- NA_integer_
    n <- data_periods[["n_data_periods"]]
    if (is.na(n)) {
        stop("Non-integral number of data periods in EPW file", error_end,
             ": ", backtick(dp[2]), ".", call. = FALSE)
    } else if (n > 1L){
        stop("More than one data period in EPW file", error_end,
             ": ", backtick(n), ", which is not supported", call. = FALSE)
    }

    ts <- data_periods[["time_step"]]
    if (is.na(ts)) {
        stop("Non-integral number of timestep in EPW file", error_end, ".",
             ": ", backtick(dp[3]), ".", call. = FALSE)
    } else if (60L %% ts != 0L){
        stop("Number of records per hour of ", backtick(ts), " does not result ",
             "in integral number of minutes between records in EPW file", error_end,
             call. = FALSE);
    }

    dw <- get_epw_week_day(data_periods[["start_day_of_week"]])
    if (is.na(dw)) {
        stop("Bad start day of week found in EPW file", error_end, ": ",
             backtick(dp[5]), ".", call. = FALSE)
    }

    start_date <- get_epw_date(path, dp[6], dw, "start")
    end_date <- get_epw_date(path, dp[7], dw, "end")
    real_year <- attr(start_date, "real_year")
    data_periods[["start_date"]] <- start_date
    data_periods[["end_date"]] <- end_date
    data_periods[["is_real_year"]] <- real_year
    # }}}

    # parse line: "HOLIDAYS/DAYLIGHT SAVINGS"
    # {{{
    # HOLIDAYS/DAYLIGHT SAVINGS, LeapYear Observed?, Daylight Saving Start Day, Daylight Saving End Day, Number of Holidays, Holiday 1 Name, Holiday 1 Day
    # NOTE THAT ONLY ONE DATA PERIOD IS SUPPORTED
    ho <- stringr::str_trim(strsplit(header_pairs$holidays_daylight_savings, ",")[[1]])
    len_ho <- length(ho)
    if(len_ho < 5L) {
        stop("Expected at least 5 holidays and daylight saving fields rather ",
             "than the ", backtick(len_ho), " fields in EPW file ",
             backtick(path), ".", call. = FALSE)
    }

    holidays_dls <- list()
    leap_year <- ho[2]
    dls_start_day <- ifelse(ho[3] == "0", NA, ho[3])
    dls_end_day <- ifelse(ho[4] == "0", NA, ho[4])
    num_of_holiday <- suppressWarnings(as.integer(ho[5]))
    ho_data <- ho[-(1:5)]

    if (!tolower(leap_year) %in% c("yes", "no")) {
        stop("Value of LeapYear Observed is neither `yes` nor `no` in EPW file ",
             backtick(path), ": ", backtick(leap_year), ".", call. = FALSE)
    }
    holidays_dls[["leap_year"]] <- ifelse(tolower(leap_year) == "yes", TRUE, FALSE)

    if (!is.na(dls_start_day) & is.na(dls_end_day)) {
        stop("Daylight saving start day specified without daylight saving end day.",
             call. = FALSE)
    }
    if (is.na(dls_start_day) & !is.na(dls_end_day)) {
        stop("Daylight saving end day specified without daylight saving start day.",
             call. = FALSE)
    }
    holidays_dls[["dls_start_day"]] <- dls_start_day
    holidays_dls[["dls_end_day"]] <- dls_end_day

    if (is.na(num_of_holiday)) {
        stop("Non-integral number of holiday number in EPW file", error_end,
             ": ", backtick(ho[5]), ".", call. = FALSE)
    } else if (num_of_holiday < 0L) {
        stop("Non-positive number of holiday number in EPW file", error_end,
             ": ", backtick(ho[5]), ".", call. = FALSE)
    } else if (num_of_holiday > 0L){
        if ((length(ho) - 5L) != num_of_holiday * 2L) {
            stop("Expected ", num_of_holiday, " holiday data rather ",
                 "than the", backtick((length(ho) - 5L) %/% 2L), " in EPW file ",
                 backtick(path), ".", call. = FALSE)
        } else {
            holidays <- ho_data[rep(c(FALSE, TRUE), length = num_of_holiday)]
            names(holidays) <- ho_data[rep(c(TRUE, FALSE), length = num_of_holiday)]
        }
    }
    holidays_dls[["num_of_holiday"]] <- num_of_holiday
    if (num_of_holiday == 0L) {
        holidays_dls["holidays"] <- list(NULL)
    } else {
        holidays_dls[["holidays"]] <- holidays
    }
    # }}}

    # parse the rest of file
    # colnames refers to column "Long Name" in Table 2.8 in
    # "AuxiliaryPrograms.pdf" of EnergyPlus 8.6
    # {{{
    header_epw_data <- data.table::fread(path, skip = num_header, nrows = 0L)
    if (ncol(header_epw_data) != 35L) {
        stop("Expected 35 fields in EPW data instead of the ",
             backtick(ncol(epw_data)), " recieved in EPW file ",
             backtick(path), ".", call. = FALSE)
    }

    epw_data <- data.table::fread(path, skip = num_header,
        col.names = c("year",                                             # 1. integer
                      "month",                                            # 2. integer
                      "day",                                              # 3. integer
                      "hour",                                             # 4. integer
                      "minute",                                           # 5. integer
                      "datasource",                                       # 6. character
                      "dry_bulb_temperature",                             # 7. double
                      "dew_point_temperature",                            # 8. double
                      "relative_humidity",                                # 9. double
                      "atmospheric_pressure",                             # 10. double
                      "extraterrestrial_horizontal_radiation",            # 11. double
                      "extraterrestrial_direct_normal_radiation",         # 12. double
                      "horizontal_infrared_radiation_intensity_from_sky", # 13. double
                      "global_horizontal_radiation",                      # 14. double
                      "direct_normal_radiation",                          # 15. double
                      "diffuse_horizontal_radiation",                     # 16. double
                      "global_horizontal_illuminance",                    # 17. double
                      "direct_normal_illuminance",                        # 18. double
                      "diffuse_horizontal_illuminance",                   # 19. double
                      "zenith_luminance",                                 # 20. double
                      "wind_direction",                                   # 21. double
                      "wind_speed",                                       # 22. double
                      "total_sky_cover",                                  # 23. integer
                      "opaque_sky_cover",                                 # 24. integer
                      "visibility",                                       # 25. double
                      "ceiling_height",                                   # 26. double
                      "present_weather_observation",                      # 27. integer
                      "present_weather_codes",                            # 28. character
                      "precipitable_water",                               # 29. double
                      "aerosol_optical_depth",                            # 30. double
                      "snow_depth",                                       # 31. double
                      "days_since_last_snow",                             # 32. integer
                      "albedo",                                           # 33. double
                      "liquid_precip_depth",                              # 34. double
                      "liquid_precip_rate"),                              # 35. double
        colClasses = list(
            integer = c(1:5, 23, 27, 32),
            character = c(6, 28),
            double = c(7:22, 25, 26, 29:31, 33:35))
        )

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
    # # check if warp around
    # epw_data[, dt_month := lubridate::month(datetime)]
    # epw_data[, dt_month_shifted := lubridate::month(datetime_shifted)]
    # wrap_around <- epw_data[-.N][dt_month < dt_month_shifted, .N > 0L]

    # check for agreement between the file value and the computed value
    min_per_rcd <- 60L/data_periods[["time_step"]]
    min_seq <- seq(0, 60, length.out = data_periods[["time_step"]] + 1L)[-1L]

    epw_data[, dt_minute := lubridate::minute(datetime)]
    epw_data[, dt_minute_cal := {rep_len(min_seq, nrow(epw_data))}][
        dt_minute_cal == 60L, dt_minute_cal := 0L]
    min_mismatch <- epw_data[dt_minute != dt_minute_cal, which = TRUE]
    if (not_empty(min_mismatch)) {
        warning("Minutes field (", backtick(epw_data[min_mismatch[1], minute]), ") on line ",
                backtick(min_mismatch[1]), " of EPW file", error_end,
                " does not agree with computed value (",
                backtick(epw_data[min_mismatch[1], dt_minute_cal]),
                "). Using computed value.")
        data_periods[["minute_match"]] <- FALSE
    } else {
        data_periods[["minute_match"]] <- TRUE
    }

    # check NAs
    epw_data[, `:=`(datetime_shifted = NULL, datetime_delta = NULL,
                    dt_minute = NULL, dt_minute_cal = NULL)]
    na_epw_data <- stats::na.omit(epw_data, invert = TRUE)
    if (not_empty(na_epw_data)) {
        stop("Invalid weather data line found in EPW file", error_end, call. = FALSE)
    }

    # check start date and end date mismatching
    first_date <- as.Date(epw_data[1L, datetime])
    if (lubridate::month(first_date) != lubridate::month(start_date) ||
        lubridate::mday(first_date) != lubridate::mday(start_date)) {
        stop("Header start date does not match data in EPW file", error_end, call. = FALSE)
    }
    last_date_list <- epw_data[.N, list(datetime, hour, minute)]
    if (last_date_list[["hour"]] == 24L) {
        last_date <- last_date_list[["datetime"]] - lubridate::days(1)
    } else {
        last_date <- last_date_list[["datetime"]]
    }
    if (lubridate::month(last_date) != lubridate::month(end_date) ||
        lubridate::mday(last_date) != lubridate::mday(end_date)) {
        stop("Header end date does not match data in EPW file", error_end, call. = FALSE)
    }
    if (real_year) {
        if (weekdays(first_date) != weekdays(first_date)) {
            warning("Header start day of the week and actual start day of the ",
                    "week do not match in EPW file", error_end, ". Data ",
                    "will be treated as typical (TMY)", call. = FALSE)
            data_periods[["is_real_year"]] <- FALSE
        } else {
            data_periods[["start_date_actual_year"]] <- lubridate::year(start_date)
            data_periods[["end_date_actual_year"]] <- lubridate::year(end_date)
        }
    }
    # if (!real_year && wrap_around) {
    #     stop("Wrap around years not supported for TMY data, EPW file ", backtick(path), call. = FALSE)
    # }
    # }}}

    # set column order
    data.table::setcolorder(epw_data, c("datetime", names(epw_data)[-ncol(epw_data)]))

    header_pairs[["location"]] <- location
    header_pairs[["data_periods"]] <- data_periods
    header_pairs[["holidays_daylight_savings"]] <- holidays_dls
    list(header = header_pairs, data = epw_data)
}
# }}}
# format_epw_date {{{
format_epw_date <- function (dt) {
    # recreate minute column
    dt[, `:=`(minute = as.integer(lubridate::minute(datetime)))]
    # create ITime column
    dt[, `:=`(time = data.table::as.ITime(datetime))]
    # create an one-day-offset datetime column
    dt[, `:=`(offset = datetime - lubridate::days(1))]
    # at 00:00:00, reset month and day by one day backwards and set hour to 23 and
    # minute to 60
    dt[time == data.table::as.ITime("00:00:00"),
       `:=`(month = as.integer(lubridate::month(offset)),
            day = as.integer(lubridate::mday(offset)),
            hour = 23L, minute = 60L)]

    # after 00:00:00 and before 01:00:00, reset month and day by one day backwards
    # and set hour to 24
    dt[time > data.table::as.ITime("00:00:00") &
       time < data.table::as.ITime("01:00:00"),
       `:=`(month = as.integer(lubridate::month(offset)),
            day = as.integer(lubridate::mday(offset)),
            hour = 24L)]

    # for 01:00:00, reset month and day by one day backwards
    # and set hour to 24 and minute to 60
    dt[time == data.table::as.ITime("01:00:00"),
       `:=`(month = as.integer(lubridate::month(offset)),
            day = as.integer(lubridate::mday(offset)),
            hour = 24L, minute = 60L)]

    # for XX:00:00 at any hour except 01:00:00, reset hour by 1 backwards and set
    # minute to 60
    dt[minute == 0L & time != data.table::as.ITime("00:00:00"),
       `:=`(hour = hour - 1L, minute = 60L)]

    # update year
    dt[, `:=`(year = as.integer(lubridate::year(datetime)))]

    # clean
    dt[, `:=`(time = NULL, offset = NULL)]

    dt
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
get_epw_year <- function (year = NULL, week_day, check_date = "1-1", leap_year = FALSE) {
    # start out from current year
    year <- as.integer(year %||% lubridate::year(Sys.Date()))
    targ <- get_epw_week_day(week_day, num = TRUE)
    # while (lubridate::leap_year(year) != leap_year ||
    while (lubridate::wday(lubridate::ymd(paste0(year, "-", check_date)), week_start = 1L) != targ) {
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
        day <- switch(type, start = x, end = "1-1")
        assume_year <- get_epw_year(year = NULL, start_day_of_week, check_date = day)
        res <- lubridate::ymd(paste0(assume_year, "-", x))
        real_year <- FALSE
    } else {
        res <- lubridate::mdy(x)
        real_year <- TRUE
    }

    data.table::setattr(res, "real_year", real_year)
    res
}
# }}}
