#' @importFrom data.table as.data.table
#' @importFrom stringi stri_detect_regex
#' @importFrom cli rule cat_rule
#' @importFrom crayon bold green
#' @importFrom R6 R6Class
#' @importFrom utils menu download.file
#' @include impl-epw.R
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
#' epw$clone(deep = FALSE)
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
#' @section Save:
#' ```
#' epw$save(path, overwrite = FALSE)
#' ```
#'
#' **Arguments**
#'
#' * `path`: A path where to save the weather file. If `NULL`, the path of the
#'     weather file itself is used.
#' * `overwrite`: Whether to overwrite the file if it already exists. Default is
#'     `FALSE`.
#'
#' @section Clone:
#'
#' ```
#' epw$clone(deep = FALSE)
#' ```
#'
#' `$clone()` copies and returns the cloned `Epw` object. Because `Epw` uses
#'     `R6Class` under the hook which has "modify-in-place" semantics, `epw_2 <-
#'     epw_1` does not copy `epw_1` at all but only create a new binding to
#'     `epw_1`.  Modify `epw_1` will also affect `epw_2` as well, as these two
#'     are exactly the same thing underneath. In order to create a complete
#'     cloned copy, please use `$clone(deep = TRUE)`.
#'
#' **Arguments**
#'
#' * `deep`: Has to be `TRUE` if a complete cloned copy is desired.
#'
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
read_epw <- function (path, warning = FALSE) {
    Epw$new(path, warning = warning)
}
# }}}

# Epw {{{
Epw <- R6::R6Class(classname = "Epw",
    # ACTIVE {{{
    active = list(
        city = function (value) {
            # {{{
            warn("warning_eplusr_deprecated_fun",
                paste0("`$city` in Epw class is deprecated. Instead, please use ",
                       "`$location()$city` to get and ",
                       "`$location(city = \"city_name\")` to set."
                )
            )
            if (missing(value)) {
                self$location()$city
            } else {
                self$location(city = value)
            }
            # }}}
        },

        state_province = function (value) {
            # {{{
            warn("warning_eplusr_deprecated_fun",
                paste0("`$state_province` in Epw class is deprecated. Instead, please use ",
                       "`$location()$state_province` to get and ",
                       "`$location(state_province = \"state_province_name\")` to set."
                )
            )
            if (missing(value)) {
                self$location()$state_province
            } else {
                self$location(state_province = value)
            }
            # }}}
        },

        country = function (value) {
            # {{{
            warn("warning_eplusr_deprecated_fun",
                paste0("`$country` in Epw class is deprecated. Instead, please use ",
                       "`$location()$country` to get and ",
                       "`$location(country = \"country_name\")` to set."
                )
            )
            if (missing(value)) {
                self$location()$country
            } else {
                self$location(country = value)
            }
            # }}}
        },

        data_source = function (value) {
            # {{{
            warn("warning_eplusr_deprecated_fun",
                paste0("`$data_source` in Epw class is deprecated. Instead, please use ",
                       "`$location()$data_source` to get and ",
                       "`$location(data_source = \"data_source_name\")` to set."
                )
            )
            if (missing(value)) {
                self$location()$data_source
            } else {
                self$location(data_source = value)
            }
            # }}}
        },

        wmo_number = function (value) {
            # {{{
            warn("warning_eplusr_deprecated_fun",
                paste0("`$wmo_number` in Epw class is deprecated. Instead, please use ",
                       "`$location()$wmo_number` to get and ",
                       "`$location(wmo_number = \"wmo_number_name\")` to set."
                )
            )
            if (missing(value)) {
                self$location()$wmo_number
            } else {
                self$location(wmo_number = value)
            }
            # }}}
        },

        latitude = function (value) {
            # {{{
            warn("warning_eplusr_deprecated_fun",
                paste0("`$latitude` in Epw class is deprecated. Instead, please use ",
                       "`$location()$latitude` to get and ",
                       "`$location(latitude = \"latitude_name\")` to set."
                )
            )
            if (missing(value)) {
                self$location()$latitude
            } else {
                self$location(latitude = value)
            }
            # }}}
        },

        longitude = function (value) {
            # {{{
            warn("warning_eplusr_deprecated_fun",
                paste0("`$longitude` in Epw class is deprecated. Instead, please use ",
                       "`$location()$longitude` to get and ",
                       "`$location(longitude = \"longitude_name\")` to set."
                )
            )
            if (missing(value)) {
                self$location()$longitude
            } else {
                self$location(longitude = value)
            }
            # }}}
        },

        time_zone = function (value) {
            # {{{
            warn("warning_eplusr_deprecated_fun",
                paste0("`$time_zone` in Epw class is deprecated. Instead, please use ",
                       "`$location()$time_zone` to get and ",
                       "`$location(time_zone = \"time_zone_name\")` to set."
                )
            )
            if (missing(value)) {
                self$location()$time_zone
            } else {
                self$location(time_zone = value)
            }
            # }}}
        },

        elevation = function (value) {
            # {{{
            warn("warning_eplusr_deprecated_fun",
                paste0("`$elevation` in Epw class is deprecated. Instead, please use ",
                       "`$location()$elevation` to get and ",
                       "`$location(elevation = \"elevation_name\")` to set."
                )
            )
            if (missing(value)) {
                self$location()$elevation
            } else {
                self$location(elevation = value)
            }
            # }}}
        },

        time_step = function (value) {
            # {{{
            if (missing(value)) {
                warn("warning_eplusr_deprecated_fun",
                    paste0("`$time_step` in Epw class is deprecated. Instead, please use ",
                           "`$interval()`."
                    )
                )
                self$interval()
            } else {
                abort("error_eplusr_deprecated_time_step",
                    paste("Directly setting time step (number of intervals per",
                          "hour) is deprecated in Epw class, because it will",
                           "cause errors during simulation when using correctly.",
                           "Now time step will automatically modified internally",
                           "to make sure it complies with the actual time step",
                           "in the weather data."
                    )
                )
            }
            # }}}
        },

        start_day_of_week = function (value) {
            # {{{
            warn("warning_eplusr_deprecated_fun",
                paste0("`$start_day_of_week` in Epw class is deprecated. Instead, please ",
                       "use `$period(period = 1)$start_day_of_week` to get and ",
                       "use `$period(period = 1, start_day_of_week = \"name_of_week_day\")`",
                       " to set."
                )
            )
            if (missing(value)) {
                get_epw_wday(self$period(1L)$start_day_of_week, label = TRUE)
            } else {
                self$period(period = 1L, start_day_of_week = value)
            }
            # }}}
        }

    ),
    # }}}

    public = list(

        # INITIALIZE {{{
        initialize = function (path, warning = FALSE) {
            if (is_string(path) & file.exists(path)) {
                private$m_path <- normalizePath(path)
            }

            epw_file <- parse_epw_file(path, warning = warning)

            private$m_header <- epw_file$header
            private$m_data <- epw_file$data

            private$m_log <- new.env(hash = FALSE, parent = emptyenv())
            private$m_log$unit <- FALSE
            private$m_log$miss_na <- FALSE
            private$m_log$range_na <- FALSE
            private$m_log$miss_filled <- FALSE
            private$m_log$range_filled <- FALSE
            private$m_log$miss_filled_special <- FALSE
            private$m_log$range_filled_special <- FALSE
            private$m_log$purged <- FALSE
        },
        # }}}

        path = function ()
            epw_path(self, private),

        # HEADER getter and setter {{{
        location = function (city, state_province, country, data_source, wmo_number,
                             latitude, longitude, time_zone, elevation)
            epw_location(self, private, city, state_province, country, data_source,
                         wmo_number, latitude, longitude, time_zone, elevation),

        design_condition = function (idfobj = FALSE)
            epw_design_condition(self, private, idfobj),

        typical_extreme_period = function ()
            epw_typical_extreme_period(self, private),

        ground_temperature = function ()
            epw_ground_temperature(self, private),

        holiday = function (leapyear, dst, holiday)
            epw_holiday(self, private, leapyear, dst, holiday),

        comment1 = function (comment)
            epw_comment1(self, private, comment),

        comment2 = function (comment)
            epw_comment2(self, private, comment),

        num_period = function ()
            epw_num_period(self, private),

        interval = function ()
            epw_interval(self, private),

        period = function (period, name, start_day_of_week)
            epw_period(self, private, period, name, start_day_of_week),
        # }}}
        # CONSTANTS {{{
        missing_code = function ()
            epw_missing_code(self, private),

        initial_missing_value = function ()
            epw_initial_missing_value(self, private),

        range_exist = function ()
            epw_range_exist(self, private),

        range_valid = function ()
            epw_range_valid(self, private),

        fill_action = function ()
            epw_fill_action(self, private),
        # }}}
        # CORE DATA {{{
        # GETTER (COPY RETURNED) {{{
        get_data = function (year = NULL, unit = FALSE, tz = Sys.timezone(), update = FALSE)
            epw_get_data(self, private, year, unit, tz, update),

        data = function (period = 1L, start_year = NULL, tz = Sys.timezone(), update = FALSE)
            epw_data(self, private, period, start_year, tz, update),

        abnormal_data = function (period = 1L, cols = NULL, keep_all = TRUE, type = c("both", "missing", "out_of_range"))
            epw_abnormal_data(self, private, period, cols, keep_all, type),

        redundant_data = function ()
            epw_redundant_data(self, private),
        # }}}
        # MODIFY IN-PLACE {{{
        make_na = function (period = NULL, missing = FALSE, out_of_range = FALSE)
            epw_make_na(self, private, period, missing, out_of_range),

        fill_abnormal = function (period = NULL, missing = FALSE, out_of_range = FALSE,
                                  special = FALSE)
            epw_fill_abnormal(self, private, period, missing, out_of_range, special),

        add_unit = function ()
            epw_add_unit(self, private),

        drop_unit = function ()
            epw_drop_unit(self, private),

        # scale = function (interval)
        #     epw_scale(self, private),

        purge = function ()
            epw_purge(self, private),
        # }}}
        # SETTER {{{
        add = function (data, realyear = FALSE, name = NULL, start_day_of_week = NULL, after = 0L, warning = TRUE)
            epw_add(self, private, data, realyear, name, start_day_of_week, after, warning),

        set_data = function (data)
            epw_set_data(self, private, data),

        set = function (data, realyear = FALSE, name = NULL, start_day_of_week = NULL, period = 1L, warning = TRUE)
            epw_set(self, private, data, realyear, name, start_day_of_week, period, warning),

        delete = function (period)
            epw_delete(self, private, period),
        # }}}
        # }}}
        save = function (path = NULL, overwrite = FALSE)
            epw_save(self, private, path, overwrite),

        print = function ()
            epw_print(self, private)
    ),

    private = list(
        m_path = NULL,
        m_header = NULL,
        m_data = NULL,
        m_log = NULL
    )
)
# }}}

# epw_path {{{
epw_path <- function (self, private) {
    private$m_path
}
# }}}
# epw_location {{{
epw_location <- function (self, private,
                          city, state_province, country, data_source,
                          wmo_number, latitude, longitude, time_zone, elevation)
{
    l <- list()
    if (!missing(city)) l$city <- city
    if (!missing(state_province)) l$state_province <- state_province
    if (!missing(country)) l$country <- country
    if (!missing(data_source)) l$data_source <- data_source
    if (!missing(wmo_number)) l$wmo_number <- wmo_number
    if (!missing(latitude)) l$latitude <- latitude
    if (!missing(longitude)) l$longitude <- longitude
    if (!missing(time_zone)) l$time_zone <- time_zone
    if (!missing(elevation)) l$elevation <- elevation

    if (!length(l)) return(private$m_header$location)

    private$m_header <- set_epw_location(private$m_header, l)
    private$m_header$location
}
# }}}
# epw_design_condition {{{
epw_design_condition <- function (self, private, idfobj = FALSE) {
    if (!idfobj) return(private$m_header$design)
    stop("Remember to implement this method!")
}
# }}}
# epw_typical_extreme_period {{{
epw_typical_extreme_period <- function (self, private) {
    copy(private$m_header$typical)
}
# }}}
# epw_ground_temperature {{{
epw_ground_temperature <- function (self, private) {
    copy(private$m_header$ground)
}
# }}}
# epw_holiday {{{
epw_holiday <- function (self, private, leapyear, dst, holiday) {
    if (missing(leapyear) && missing(dst) && missing(holiday)) {
        copy(private$m_header$holiday)
    }

    private$m_header <- set_epw_holiday(private$m_header, leapyear, dst, holiday)
    copy(private$m_header$holiday)
}
# }}}
# epw_comment1 {{{
epw_comment1 <- function (self, private, comment) {
    if (missing(comment)) {
        return(private$m_header$comment1)
    } else {
        assert(is_string(comment))
        (private$m_header$comment1 <- comment)
    }
}
# }}}
# epw_comment2 {{{
epw_comment2 <- function (self, private, comment) {
    if (missing(comment)) {
        return(private$m_header$comment2)
    } else {
        assert(is_string(comment))
        (private$m_header$comment2 <- comment)
    }
}
# }}}
# epw_num_period {{{
epw_num_period <- function (self, private) {
    nrow(private$m_header$period$period)
}
# }}}
# epw_interval {{{
epw_interval <- function (self, private) {
    private$m_header$period$interval
}
# }}}
# epw_period {{{
epw_period <- function (self, private, period, name, start_day_of_week) {
    if (missing(period) && missing(name) && missing(start_day_of_week)) {
        return(private$m_header$period$period[, -c("from", "to", "missing", "out_of_range")])
    }

    private$m_header <- set_epw_period_basic(private$m_header, period, name, start_day_of_week)
    private$m_header$period$period
}
# }}}
# epw_missing_code {{{
epw_missing_code <- function (self, private) {
    EPW_MISSING_CODE
}
# }}}
# epw_initial_missing_value {{{
epw_initial_missing_value <- function (self, private) {
    EPW_INIT_MISSING$atmospheric_pressure <- std_atm_press(private$m_header$location$elevation)
    EPW_INIT_MISSING
}
# }}}
# epw_range_exist {{{
epw_range_exist <- function (self, private) {
    EPW_RANGE_EXIST
}
# }}}
# epw_range_valid {{{
epw_range_valid <- function (self, private) {
    EPW_RANGE_VALID
}
# }}}
# epw_fill_action {{{
epw_fill_action <- function (self, private, type = c("missing", "out_of_range")) {
    type <- match.arg(type)
    if (type == "missing") {
        EPW_REPORT_MISSING
    } else {
        EPW_REPORT_RANGE
    }
}
# }}}
# epw_data {{{
epw_data <- function (self, private, period = 1L, start_year = NULL,
                      tz = Sys.timezone(), update = FALSE) {
    get_epw_data(private$m_data, private$m_header, period, start_year, tz, update)
}
# }}}
# epw_get_data {{{
epw_get_data <- function (self, private, year = NULL, unit = FALSE, tz = Sys.timezone(), update = FALSE) {
    warn("warning_eplusr_deprecated_fun",
        paste(
            "`$get_data()` in Epw class is deprecated. Please use `$data() instead.",
            "Note that `unit` argument is equal to call `$add_unit()`.",
            "Please see documentation of `$add_unit()` for more details."
        )
    )

    self$data(1L, year, tz, update)
}
# }}}
# epw_abnormal_data {{{
epw_abnormal_data <- function (self, private, period = 1L, cols = NULL,
                               keep_all = TRUE, type = c("both", "missing", "out_of_range")) {
    get_epw_data_abnormal(private$m_data, private$m_header, period, cols, keep_all, type)
}
# }}}
# epw_redundant_data {{{
epw_redundant_data <- function (self, private) {
    get_epw_data_redundant(private$m_data, private$m_header)
}
# }}}
# epw_make_na {{{
epw_make_na <- function (self, private, period = NULL, missing = FALSE, out_of_range = FALSE) {
    assert(is_flag(missing), is_flag(out_of_range))
    if (!missing && !out_of_range) return(invisible(self))
    if (missing) {
        if (private$m_log$miss_na) {
            verbose_info("Missing values have been already converted to NAs before. Skip...")
            missing <- FALSE
        } else {
            private$m_log$miss_na <- TRUE
            private$m_log$miss_filled <- FALSE
        }
    }
    if (out_of_range) {
        if (private$m_log$range_na) {
            verbose_info("Out-of-range values have been already converted to NAs before. Skip...")
            out_of_range <- FALSE
        } else {
            private$m_log$range_na <- TRUE
            private$m_log$range_filled <- FALSE
        }
    }
    private$m_data <- make_epw_data_na(private$m_data, private$m_header, period = period,
        missing = missing, out_of_range = out_of_range
    )
    invisible(self)
}
# }}}
# epw_fill_abnormal {{{
epw_fill_abnormal <- function (self, private, period = NULL, missing = FALSE, out_of_range = FALSE, special = FALSE) {
    assert(is_flag(missing), is_flag(out_of_range), is_flag(special))
    if (!missing && !out_of_range) return(invisible(self))
    miss_na <- private$m_log$miss_na
    if (missing) {
        if (private$m_log$miss_filled) {
            verbose_info("Missing values have been already filled before. Skip...")
            missing <- FALSE
        } else {
            private$m_log$miss_filled <- TRUE
            private$m_log$miss_filled_special <- special
            miss_na <- TRUE
        }
    }
    range_na <- private$m_log$range_na
    if (out_of_range) {
        if (private$m_log$range_filled) {
            verbose_info("Out-of-range values have been already filled before. Skip...")
            out_of_range <- FALSE
        } else {
            private$m_log$range_filled <- TRUE
            private$m_log$range_filled_special <- special
            range_na <- TRUE
        }
    }
    private$m_data <- fill_epw_data_abnormal(private$m_data, private$m_header,
        period, missing, out_of_range, special, private$m_log$miss_na, private$m_log$range_na
    )
    # have to update na status after filling, as it was used when doing filling
    private$m_log$miss_na <- miss_na
    private$m_log$range_na <- range_na
    invisible(self)
}
# }}}
# epw_add_unit {{{
epw_add_unit <- function (self, private) {
    if (private$m_log$unit) {
        verbose_info("Units have been already added before. Skip...")
    } else {
        private$m_data <- add_epw_data_unit(private$m_data)
        private$m_log$unit <- TRUE
    }
    invisible(self)
}
# }}}
# epw_drop_unit {{{
epw_drop_unit <- function (self, private) {
    if (!private$m_log$unit) {
        verbose_info("Units have been already dropped before. Skip...")
    } else {
        private$m_data <- drop_epw_data_unit(private$m_data)
        private$m_log$unit <- FALSE
    }
    invisible(self)
}
# }}}
# # epw_scale {{{
# epw_scale <- function (interval) {
    
# }
# # }}}
# epw_purge {{{
epw_purge <- function (self, private) {
    if (private$m_log$purged) {
        verbose_info("Redundant data has already been purged before. Skip...")
    } else {
        lst <- purge_epw_data_redundant(private$m_data, private$m_header)
        private$m_header <- lst$header
        private$m_data <- lst$data
    }
    invisible(self)
}
# }}}
# epw_align_data_status {{{
epw_align_data_status <- function (self, private, data, data_period) {
    if (private$m_log$miss_na) {
        data <- make_epw_data_na_line(data, data_period$missing[[1L]])
    } else if (private$m_log$miss_filled) {
        data <- fill_epw_data_abnormal_line(data, data_period$missing[[1L]], FALSE,
            private$m_log$miss_filled_special, "missing")
    }

    if (private$m_log$range_na) {
        data <- make_epw_data_na_line(data, data_period$out_of_range[[1L]])
    } else if (private$m_log$range_filled) {
        data <- fill_epw_data_abnormal_line(data, data_period$out_of_range[[1L]], FALSE,
            private$m_log$miss_filled_special, "out_of_range")
    }

    if (private$m_log$unit) {
        data <- add_epw_data_unit(data)
    }

    data
}
# }}}
# epw_add {{{
epw_add <- function (self, private, data, realyear = FALSE, name = NULL,
                     start_day_of_week = NULL, after = 0L, warning = TRUE) {
    lst <- add_epw_data(private$m_data, private$m_header, data, realyear, name, start_day_of_week, after, warning)
    lst$data <- epw_align_data_status(self, private, lst$data, lst$header$period$period[after + 1L])
    private$m_header <- lst$header
    private$m_data <- rbindlist(lst[names(lst) != "header"])

    if (eplusr_option("verbose_info")) {
        # get data period
        n <- nrow(lst$header$period$period)
        # use nearest as template
        if (after > n) after <- n - 1L

        cli::cat_rule(crayon::bold("Info"), col = "green")
        cat(crayon::green("New data period has been added successfully:\n"))

        print(private$m_header$period$period[after + 1L,
           .(" " = paste0(index, ": "), Name = name,
            `StartDayOfWeek` = get_epw_wday(start_day_of_week, label = TRUE),
            `StartDay` = start_day, `EndDay` = end_day)],
            row.names = FALSE
        )
    }

    invisible(self)
}
# }}}
# epw_set {{{
epw_set <- function (self, private, data, realyear = FALSE, name = NULL,
                     start_day_of_week = NULL, period = 1L, warning = TRUE) {
    lst <- set_epw_data(private$m_data, private$m_header, data, realyear, name, start_day_of_week, period, warning)
    lst$data <- epw_align_data_status(self, private, lst$data, lst$header$period$period[period])
    private$m_header <- lst$header
    private$m_data <- rbindlist(lst[names(lst) != "header"])

    if (eplusr_option("verbose_info")) {
        cli::cat_rule(crayon::bold("Info"), col = "green")
        cat(crayon::green("Data period", paste0("#", period), "has been replaced with input data.\n"))

        print(private$m_header$period$period[period,
           .(" " = paste0(index, ": "), Name = name,
            `StartDayOfWeek` = get_epw_wday(start_day_of_week, label = TRUE),
            `StartDay` = start_day, `EndDay` = end_day)],
            row.names = FALSE
        )
    }

    invisible(self)
}
# }}}
# epw_set_data {{{
epw_set_data <- function (self, private, data) {
    warn("warning_eplusr_deprecated_fun",
        "`$set_data()` in Epw class is deprecated. Please use `$set() instead."
    )
    self$set(data)
}
# }}}
# epw_delete {{{
epw_delete <- function (self, private, period) {
    l <- delete_epw_data(private$m_data, private$m_header, period)
    private$m_header <- l$header
    private$m_data <- l$data
    invisible(self)
}
# }}}
# epw_save {{{
epw_save <- function (self, private, path = NULL, overwrite = FALSE, purge = FALSE) {
    if (is.null(path)) {
        if (is.null(private$m_path)) {
            abort("error_not_local",
                paste0(
                    "The Epw object is not created from local file. ",
                    "Please give the path to save."
                )
            )
        } else {
            path <- private$m_path
        }
    }

    assert(is_string(path), has_ext(path, "epw"), is_flag(overwrite), is_flag(purge))

    # fill all NAs with missing code
    fill <- if (!private$m_log$miss_filled || !private$m_log$range_filled) TRUE else FALSE

    p <- save_epw_file(private$m_data, private$m_header, path, overwrite, fmt_digit = TRUE,
        fill = fill,
        missing = private$m_log$miss_filled,
        out_of_range = private$m_log$range_filled,
        miss_na = private$m_log$miss_na,
        range_na = private$m_log$range_na,
        purge = purge
    )

    # update path
    private$m_path <- path
    invisible(self)
}
# }}}
# epw_print {{{
epw_print <- function (self, private) {
    print_epw_header(private$m_header)
}
# }}}
# S3 Epw methods {{{
str.Epw <- function (x, ...) x$print(...)
format.Epw <- function (x, ...) x$print(...)
# }}}

#' Download EnergyPlus Weather File (EPW) and Design Day File (DDY)
#'
#' `download_weather()` makes it easy to download EnergyPlus weather files (EPW)
#' and design day files (DDY).
#'
#' @param pattern A regular expression used to search locations, e.g. `"los
#'     angeles.*tmy3"`. The search is case-insensitive.
#' @param filename File names (without extension) used to save downloaded files.
#'     Internally, [make.unique()] is called to ensure unique names.
#' @param dir Directory to save downloaded files
#' @param type File type to download. Should be one of `"all"`, `"epw"` and
#'     `"ddy"`. If `"all"`, both weather files and design day files will be
#'     downloaded.
#' @param ask If `TRUE`, a command line menu will be shown to let you select
#'     which one to download. If `FALSE` and the number of returned results is
#'     less than `max_match`, files are downloaded automatically without asking.
#' @param max_match The max results allowed to download when `ask` is `FALSE`.
#' @return A character vector containing paths of downloaded files.
#' @examples
#' \dontrun{
#' download_weather("los angeles.*tmy3", "la")
#' }
#' @author Hongyuan Jia
#' @export
# download_weather {{{
download_weather <- function (pattern, filename = NULL, dir = ".", type = c("all", "epw", "ddy"),
                              ask = TRUE, max_match = 3) {
    pattern <- gsub("\\s+", ".", pattern)
    d <- as.data.table(weather_db)
    res <- d[stri_detect_regex(title, pattern, case_insensitive = TRUE)]

    mes_location <- function (index = NULL, title, country, state_province, location, wmo_number, source_type, longitude, latitude) {
        if (!is.null(index)) {
            h <- cli::rule(paste0("[", index, "] ", title))
        } else {
            h <- cli::rule(paste0(title))
        }
        country <- if (is.na(country)) NULL else paste0(" * Country: ", country)
        state_province <- if (is.na(state_province)) NULL else paste0(" * State or Province: ", state_province)
        location <- if (is.na(location)) NULL else paste0(" * Location: ", location)
        wmo_number <- if (is.na(wmo_number)) NULL else paste0(" * WMO number: ", wmo_number)
        source_type <- if (is.na(source_type)) NULL else paste0(" * Source type: ", source_type)
        longitude <- paste0(" * Longitude: ", longitude)
        latitude <- paste0(" * Latitude: ", latitude)
        paste(h, country, state_province, location, wmo_number, source_type, longitude, latitude,
            sep = "\n"
        )
    }

    res[, index := .I]

    if (!nrow(res)) {
        message("No matched result found.")
        return(invisible(NULL))
    }

    if (nrow(res) > 1L) {
        m <- res[, mes_location(index, title, country, state_province, location, wmo_number, source_type, longitude, latitude), by = index]$V1
        if (ask) {
            h <- paste0(nrow(res), " matched results found. Please select which one to download:")
            ch <- c(res$title, "All")
            r <- utils::menu(ch, title = paste0(h, "\n\n", paste(m, collapse = "\n\n")))
            if (r == 0) return(invisible(NULL))
            if (r < length(ch)) res <- res[index == r]
        } else {
            if (nrow(res) <= max_match) {
                message(nrow(res), " matched results found. All of them will be downloaded:\n",
                    paste0(m, collapse = "\n\n")
                )
            } else {
                stop(nrow(res), " matched results found which exceeds current ",
                    "max allowed match number (", max_match, "). Please modify your search string.\n\n",
                    paste0("[", res$index, "] ", res$title, collapse = "\n"), call. = FALSE
                )
            }
        }
    } else {
        m <- res[, mes_location(NULL, title, country, state_province, location, wmo_number, source_type, longitude, latitude), by = index]$V1
        if (ask) {
            h <- paste0("One matched result found. Please confirm to start downloading:")
            r <- utils::menu(c("Yes", "No"), title = paste0(h, "\n\n", paste(m, collapse = "\n\n")))
            if (r != 1) return(invisible(NULL))
        } else {
            h <- paste0("One matched results found. Start downloading:")
            message("One matched results found. Start downloading:\n",
                paste0(m, collapse = "\n\n")
            )
        }
    }

    type <- match.arg(type)
    if (!dir.exists(dir)) dir.create(dir)
    if (is.null(filename)) {
        res[, `:=`(epw_name = basename(epw_url), ddy_name = basename(ddy_url))]
    } else {
        res[, `:=`(
            epw_name = make.unique(paste0(filename, ".epw"), sep = "_"),
            ddy_name = make.unique(paste0(filename, ".ddy"), sep = "_")
        )]
    }

    res[, `:=`(
        epw_path = normalizePath(file.path(dir, epw_name), mustWork = FALSE),
        ddy_path = normalizePath(file.path(dir, ddy_name), mustWork = FALSE))
    ]

    if (type == "all") {
        utils::download.file(res$epw_url, res$epw_path, method = "libcurl", mode = "wb")
        utils::download.file(res$ddy_url, res$ddy_path, method = "libcurl", mode = "wb")
        c(res$epw_path, res$ddy_path)
    } else if (type == "ddy") {
        utils::download.file(res$ddy_url, res$ddy_path, method = "libcurl", mode = "wb")
        res$ddy_path
    } else {
        utils::download.file(res$epw_url, res$epw_path, method = "libcurl", mode = "wb")
        res$epw_path
    }
}
# }}}
