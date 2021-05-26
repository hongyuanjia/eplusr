#' @importFrom data.table as.data.table
#' @importFrom stringi stri_detect_regex
#' @importFrom cli rule cat_rule
#' @importFrom R6 R6Class
#' @importFrom utils menu
#' @importFrom checkmate assert_list assert_names assert_flag
#' @include idd.R
#' @include idf.R
#' @include impl-epw.R
NULL

# EpwIdd {{{
EpwIdd <- R6::R6Class(classname = "EpwIdd", cloneable = FALSE, lock_objects = FALSE,
    inherit = Idd,
    public = list(
        initialize = function (path) {
            # add a uuid
            private$m_log <- new.env(hash = FALSE, parent = emptyenv())
            private$m_log$uuid <- unique_id()

            idd_file <- parse_idd_file(path, epw = TRUE)
            private$m_version <- idd_file$version
            private$m_build <- idd_file$build

            private$m_idd_env <- list2env(
                idd_file[!names(idd_file) %in% c("version", "build")], parent = emptyenv()
            )

            # add current idd to .globals
            .globals$epw <- self
        },

        print = function ()
            epwidd_print(self, private)
    )
)
# }}}
# epwidd_print {{{
epwidd_print <- function (self, private) {
    cli::cat_rule("EnergyPlus Weather File Data Dictionary")
    cli::cat_line("* ", c(
        paste0("Version", ": ", private$m_version),
        paste0("Build", ": ", private$m_build),
        paste0("Total Class", ": ", nrow(private$m_idd_env$class))
    ))
}
# }}}

#' Read, and modify an EnergyPlus Weather File (EPW)
#'
#' Reading an EPW file starts with function [read_epw()], which parses an EPW
#' file and returns an `Epw` object. The parsing process is basically the same
#' as \[EnergyPlus/WeatherManager.cc\] in EnergyPlus, with some simplifications.
#'
#' An EPW file can be divided into two parts, headers and weather data. The
#' first eight lines of a standard EPW file are normally headers which contains
#' data of location, design conditions, typical/extreme periods, ground
#' temperatures, holidays/daylight savings, data periods and other comments.
#' `Epw` class provides methods to directly extract those data. For details on
#' the data structure of EPW file, please see "Chapter 2 - Weather Converter
#' Program" in EnergyPlus "Auxiliary Programs" documentation. An online version
#' can be found [here](https://bigladdersoftware.com/epx/docs/).
#'
#' There are about 35 variables in the core weather data. However, not all of
#' them are used by EnergyPlus. Actually, despite of date and time columns, only
#' 13 columns are used:
#'
#' 1. dry bulb temperature
#' 2. dew point temperature
#' 3. relative humidity
#' 4. atmospheric pressure
#' 5. horizontal infrared radiation intensity from sky
#' 6. direct normal radiation
#' 7. diffuse horizontal radiation
#' 8. wind direction
#' 9. wind speed
#' 10. present weather observation
#' 11. present weather codes
#' 12. snow depth
#' 13. liquid precipitation depth
#'
#' **Note** the `hour` column in the core weather data corresponds to the period
#' from **(Hour-1)th** to **(Hour)th**. For instance, if the number of interval
#' per hour is 1, hour of 1 on a certain day corresponds to the period between
#' 00:00:01 to 01:00:00, Hour of 2 corresponds to the period between
#' 01:00:01 to 02:00:00, and etc. Currently, in EnergyPlus the minute column is
#' **not used** to determine currently sub-hour time. For instance, if the
#' number of interval per hour is 2, there is no difference between two rows
#' with following time columns (a) Hour 1, Minute 0; Hour 1, Minute 30 and (b)
#' Hour 1, Minute 30; Hour 1, Minute 60. Only the number of rows count.
#' When EnergyPlus reads the EPW file, both (a) and (b) represent the same time
#' period: 00:00:00 - 00:30:00 and 00:30:00 - 01:00:00.
#
#' Missing data on the weather file used can be summarized in the eplusout.err
#' file, if `DisplayWeatherMissingDataWarnings` is turned on in
#' `Output:Diagnostics` object. In EnergyPlus, missing data is shown only for
#' fields that EnergyPlus will use. EnergyPlus will fill some missing data
#' automatically during simulation. Likewise out of range values are counted for
#' each occurrence and summarized. However, note that the out of range values
#' will **not be changed** by EnergyPlus and could affect your simulation.
#'
#' `Epw` class provides methods to easily extract and inspect those abnormal
#' (missing and out of range) weather data and also to know what kind of actions
#' that EnergyPlus will perform on those data.
#'
#' EnergyPlus energy model calibration often uses actual measured weather data.
#' In order to streamline the error-prone process of creating custom EPW file,
#' `Epw` provides methods to direction add, replace the core weather data.
#'
#' @docType class
#' @name Epw
#' @author Hongyuan Jia
NULL

#' @export
# Epw {{{
Epw <- R6::R6Class(classname = "Epw",
    public = list(

        # INITIALIZE {{{
        #' @description
        #' Create an `Epw` object
        #'
        #' @details
        #' It takes an EnergyPlus Weather File (EPW) as input and returns an
        #' `Epw` object.
        #'
        #' @param path Either a path, a connection, or literal data (either a
        #'        single string or a raw vector) to an EnergyPlus Weather File
        #'        (EPW).  If a file path, that file usually has a extension
        #'        `.epw`.
        #'
        #' @return An `Epw` object.
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
        #'     path_epw <- file.path(
        #'         eplus_config(8.8)$dir,
        #'         "WeatherData",
        #'         "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
        #'     )
        #'     epw <- read_epw(path_epw)
        #' }
        #' }
        #'
        initialize = function (path) {
            if (checkmate::test_file_exists(path, "r")) {
                private$m_path <- normalizePath(path)
            }

            private$m_idd <- get_epw_idd()

            epw_file <- parse_epw_file(path, idd = private$m_idd)

            private$m_idf_env <- list2env(epw_file$header, parent = emptyenv())
            private$m_data <- epw_file$data

            private$m_log <- new.env(hash = FALSE, parent = emptyenv())
            private$m_log$matched <- epw_file$matched
            private$m_log$unit <- FALSE
            private$m_log$miss_na <- FALSE
            private$m_log$range_na <- FALSE
            private$m_log$miss_filled <- FALSE
            private$m_log$range_filled <- FALSE
            private$m_log$miss_filled_special <- FALSE
            private$m_log$range_filled_special <- FALSE
            private$m_log$purged <- FALSE
            private$m_log$unsaved <- FALSE
            private$m_log$uuid <- unique_id()
            private$m_log$order <- private$m_idf_env$object[, list(object_id)][
                , object_order := 0L]
        },
        # }}}

        # META {{{
        # path {{{
        #' @description
        #' Get the file path of current `Epw`
        #'
        #' @details
        #' `$path()` returns the full path of current `Epw` or `NULL` if the
        #' `Epw` object is created using a character vector and not saved
        #' locally.
        #'
        #' @return `NULL` or a single string.
        #'
        #' @examples
        #' \dontrun{
        #' # get path
        #' epw$path()
        #' }
        #'
        path = function ()
            epw_path(self, private),
        # }}}
        # definition {{{
        #' @description
        #' Get the [IddObject] object for specified EPW class.
        #'
        #' @details
        #' `$definition()` returns an [IddObject] of given EPW class. [IddObject]
        #' contains all data used for parsing that EPW class.
        #'
        #' Currently, all supported EPW classes are:
        #'
        #' * `LOCATION`
        #' * `DESIGN CONDITIONS`
        #' * `TYPICAL/EXTREME PERIODS`
        #' * `GROUND TEMPERATURES`
        #' * `HOLIDAYS/DAYLIGHT SAVINGS`
        #' * `COMMENTS 1`
        #' * `COMMENTS 2`
        #' * `DATA PERIODS`
        #' * `WEATHER DATA`
        #'
        #' @param class A single string.
        #'
        #' @examples
        #' \dontrun{
        #' # get path
        #' epw$definition("LOCATION")
        #' }
        #'
        definition = function (class)
            epw_definition(self, private, class),
        # }}}
        # }}}

        # HEADER {{{
        # location {{{
        #' @description
        #' Get and modify LOCATION header
        #'
        #' @details
        #' `$location()` takes new values for `LOCATION` header fields and
        #' returns the parsed values of `LOCATION` header in a list format. If
        #' no input is given, current `LOCATION` header value is returned.
        #'
        #' @param city A string of city name recorded in the `LOCATION` header.
        #' @param state_province A string of state or province name recorded in
        #'        the `LOCATION` header.
        #' @param country A string of country name recorded in the `LOCATION`
        #'        header.
        #' @param data_source A string of data source recorded in the `LOCATION`
        #'        header.
        #' @param wmo_number A string of WMO (World Meteorological Organization)
        #'        number recorded in the `LOCATION` header.
        #' @param latitude A number of latitude recorded in the `LOCATION`
        #'        header. North latitude is positive and south latitude is
        #'        negative. Should in range `[-90, +90]`.
        #' @param longitude A number of longitude recorded in the `LOCATION`
        #'        header. East longitude is positive and west longitude is
        #'        negative. Should in range `[-180, +180]`.
        #' @param time_zone A number of time zone recorded in the `LOCATION`
        #'        header. Usually presented as the offset hours from UTC time.
        #'        Should in range `[-12, +14]`.
        #' @param elevation A number of elevation recorded in the `LOCATION`
        #'        header. Should in range `[-1000, 9999.9)`.
        #'
        #' @return A named list of 9 elements.
        #'
        #' @examples
        #' \dontrun{
        #' epw$location()
        #'
        #' # modify location data
        #' epw$location(city = "MyCity")
        #' }
        #'
        location = function (city, state_province, country, data_source, wmo_number,
                             latitude, longitude, time_zone, elevation)
            epw_location(self, private, city, state_province, country, data_source,
                         wmo_number, latitude, longitude, time_zone, elevation),
        # }}}

        # design_condition {{{
        #' @description
        #' Get DESIGN CONDITION header
        #'
        #' @details
        #' `$design_condition()` returns the parsed values of `DESIGN CONDITION`
        #' header in a list format with 4 elements:
        #'
        #' * `source`: A string of source field
        #' * `heating`: A list, usually of length 16, of the heading design conditions
        #' * `cooling`: A list, usually of length 32, of the cooling design conditions
        #' * `extremes`: A list, usually of length 16, of the extreme design conditions
        #'
        #' For the meaning of each element, please see ASHRAE Handbook of Fundamentals.
        #'
        #' @return A named list of 4 elements.
        #'
        #' @examples
        #' \dontrun{
        #' epw$design_condition()
        #' }
        #'
        design_condition = function ()
            epw_design_condition(self, private),
        # }}}

        # typical_extreme_period {{{
        #' @description
        #' Get TYPICAL/EXTREME header
        #'
        #' @details
        #' `$typical_extreme_period()` returns the parsed values of `TYPICAL/EXTREME
        #' PERIOD` header in a [data.table][data.table::data.table()] format with 6
        #' columns:
        #'
        #' * `index`: Integer type. The index of typical or extreme period record
        #' * `name`: Character type. The name of typical or extreme period record
        #' * `type`: Character type. The type of period. Possible value: `typical` and
        #'   `extreme`
        #' * `start_day`: Date type with customized formatting. The start day of the
        #'   period
        #' * `start_day`: Date type with customized formatting. The end day of the
        #'   period
        #'
        #' @return A [data.table::data.table()] with 6 columns.
        #'
        #' @examples
        #' \dontrun{
        #' epw$typical_extreme_period()
        #' }
        #'
        typical_extreme_period = function ()
            epw_typical_extreme_period(self, private),
        # }}}

        # ground_temperature {{{
        #' @description
        #' Get GROUND TEMPERATURE header
        #'
        #' @details
        #' `$ground_temperature()` returns the parsed values of `GROUND TEMPERATURE`
        #' header in a [data.table][data.table::data.table()] format with 17 columns:
        #'
        #' * `index`: Integer type. The index of ground temperature record
        #' * `depth`: Numeric type. The depth of the ground temperature is measured
        #' * `soil_conductivity`: Numeric type. The soil conductivity at measured depth
        #' * `soil_density`: Numeric type. The soil density at measured depth
        #' * `soil_specific heat`: Numeric type. The soil specific heat at measured depth
        #' * `January` to `December`: Numeric type. The measured group
        #'   temperature for each month.
        #'
        #' @return A [data.table::data.table()] with 17 columns.
        #'
        #' @examples
        #' \dontrun{
        #' epw$ground_temperature()
        #' }
        #'
        ground_temperature = function ()
            epw_ground_temperature(self, private),
        # }}}

        # holiday {{{
        #' @description
        #' Get and modify HOLIDAYS/DAYLIGHT SAVINGS header
        #'
        #' @details
        #' `$holiday()` takes new value for leap year indicator, daylight saving time
        #' and holiday specifications, set these new values and returns the parsed values
        #' of `HOLIDAYS/DAYLIGHT SAVINGS` header. If no input is given, current values
        #' of `HOLIDAYS/DAYLIGHT SAVINGS` header is returned. It returns a list of 3
        #' elements:
        #'
        #' * `leapyear`: A single logical vector. `TRUE` means that the weather data
        #'   contains leap year data
        #' * `dst`: A Date vector contains the start and end day of daylight saving time
        #' * `holiday`: A [data.table][data.table::data.table()] contains 2 columns. If
        #'   no holiday specified, an empty [data.table][data.table::data.table()]
        #'   * `name`: Name of the holiday
        #'   * `day`: Date of the holiday
        #'
        #' Validation process below is performed when changing the `leapyear`
        #' indicator:
        #'
        #' * If current record of `leapyear` is `TRUE`, but new input is `FALSE`, the
        #'   modification is only conducted when all data periods do not cover Feb 29.
        #' * If current record of `leapyear` is `FALSE`, but new input is `TRUE`, the
        #'   modification is only conducted when TMY data periods do not across Feb,
        #'   e.g. \[01/02, 02/28\], \[03/01, 12/31\]; for AMY data, it is always OK.
        #'
        #' The date specifications in `dst` and `holiday` should follow the rules of
        #' **"Table 2.14: Weather File Date File Interpretation"** in
        #' "AuxiliaryPrograms" documentation. eplusr is able to handle all those kinds of
        #' formats automatically. Basically, 5 formats are allowed:
        #'
        #' 1. A single integer is interpreted as the Julian day of year. For example,
        #'    `1`, `2`, `3` and `4` will be parsed and presented as `1st day`, `2nd
        #'    day`, `3rd day` and `4th day`.
        #' 2. A single number is interpreted as `Month.Day`. For example, `1.2` and `5.6`
        #'    will be parsed and presented as `Jan 02` and `May 06`.
        #' 3. A string giving `MonthName / DayNumber`, `DayNumber / MonthName`, and
        #'    `MonthNumber / DayNumber`. A year number can be also included. For
        #'    example, `"Jan/1"`, `"05/Dec"`, `"7/8"`, `"02/10/2019"`, and
        #'    `"2019/04/05"` will be parsed and presented as `Jan 02`, `Dec 06`, `Jul
        #'    8`, `2019-02-10` and `2019-04-15`.
        #' 4. A string giving `number Weekday in Month`. For example, `"2 Sunday in
        #'    Jan"` will be parsed and presented as `2th Sunday in January`.
        #' 5. A string giving `Last Weekday in Month`. For example, `"last Sunday in
        #'    Dec"` will be parsed and presented as `Last Sunday in December`.
        #'
        #' For convenience, besides all the formats described above, `dst` and days in
        #' `holiday` also accept standard Dates input. They will be treated as the same
        #' way as No.3 format described above.
        #'
        #' @param leapyear Either `TRUE` or `FALSE`.
        #'
        #' @param dst A length 2 EPW date specifications identifying the start
        #'        and end of daylight saving time. For example, `c(3.10, 10.3)`.
        #'
        #' @param holiday a list or a data.frame containing two elements
        #'        (columns) `name` and `day` where `name` are the holiday names
        #'        and `day` are valid EPW date specifications. For example:
        #' ```
        #' list(name = c("New Year's Day", "Christmas Day"), day = c("1.1", "25 Dec"))
        #' ```
        #'
        #' @return A named list of 3 elements.
        #'
        #' @examples
        #' \dontrun{
        #' epw$holiday()
        #'
        #' # add daylight saving time
        #' epw$holiday(dst = c(3.10, 11.3))
        #' }
        #'
        holiday = function (leapyear, dst, holiday)
            epw_holiday(self, private, leapyear, dst, holiday),
        # }}}

        # comment1 {{{
        #' @description
        #' Get and modify COMMENT1 header
        #'
        #' @details
        #' `$comment1()` takes a single string of new comments and replaces the
        #' old comment with input one. If `NULL` is given, the comment is
        #' removed. Empty string or a string that contains only spaces will be
        #' treated as `NULL`. If no input is given, current comment is returned.
        #' If no comments exist, `NULL` is returned.
        #'
        #' @param comment A string of new comments.
        #'
        #' @return A single string.
        #'
        #' @examples
        #' \dontrun{
        #' epw$comment1()
        #'
        #' epw$comment1("Comment1")
        #' }
        #'
        comment1 = function (comment)
            epw_comment1(self, private, comment),
        # }}}

        # comment2 {{{
        #' @description
        #' Get and modify COMMENT2 header
        #'
        #' @details
        #' `$comment2()` takes a single string of new comments and replaces the
        #' old comment with input one. If `NULL` is given, the comment is
        #' removed. Empty string or a string that contains only spaces will be
        #' treated as `NULL`. If no input is given, current comment is returned.
        #' If no comments exist, `NULL` is returned.
        #'
        #' @param comment A string of new comments.
        #'
        #' @return A single string.
        #'
        #' @examples
        #' \dontrun{
        #' epw$comment2()
        #'
        #' epw$comment2("Comment2")
        #' }
        #'
        comment2 = function (comment)
            epw_comment2(self, private, comment),
        # }}}

        # num_period {{{
        #' @description
        #' Get number of data periods in DATA PERIODS header
        #'
        #' @details
        #' `$num_period()` returns a single positive integer of how many data
        #' periods current `Epw` contains.
        #'
        #' @return A single integer.
        #'
        #' @examples
        #' \dontrun{
        #' epw$num_period()
        #' }
        #'
        num_period = function ()
            epw_num_period(self, private),
        # }}}

        # interval {{{
        #' @description
        #' Get the time interval in DATA PERIODS header
        #'
        #' @details
        #' `$interval()` returns a single positive integer of how many records
        #' of weather data exist in one hour.
        #'
        #' @return A single integer.
        #'
        #' @examples
        #' \dontrun{
        #' epw$interval()
        #' }
        #'
        interval = function ()
            epw_interval(self, private),
        # }}}

        # period {{{
        #' @description
        #' Get and modify data period meta data in DATA PERIODS header
        #'
        #' @details
        #' `$period()` takes a data period index, a new period name and start
        #' day of week specification, and uses that input to replace the data
        #' period's name and start day of week. If no input is given, data
        #' periods in current `Epw` is returned.
        #'
        #' @param period A positive integer vector identifying the data period
        #'        indexes.
        #' @param name A character vector used as new names for specified data
        #'        periods. Should have the same length as `index`.
        #' @param start_day_of_week A character vector or an integer vector used
        #'        as the new start days of week of specified data periods.
        #'        Should have the same length as `index`.
        #'
        #' @return A [data.table][data.table::data.table()] with 5 columns:
        #'
        #' * `index`: Integer type. The index of data period.
        #' * `name`: Character type. The name of data period.
        #' * `start_day_of_week`: Character type. The start day of week of data period.
        #' * `start_day`: Date (EpwDate) type. The start day of data period.
        #' * `end_day`: Date (EpwDate) type. The end day of data period.
        #'
        #' @examples
        #' \dontrun{
        #' # modify data period name
        #' epw$period(1, name = "test")
        #'
        #' # change start day of week
        #' epw$period(1, start_day_of_week = 3)
        #' }
        #'
        period = function (period, name, start_day_of_week)
            epw_period(self, private, period, name, start_day_of_week),
        # }}}
        # }}}

        # CONSTANTS {{{
        # missing_code {{{
        #' @description
        #' Get missing code for weather data variables
        #'
        #' @details
        #' `$missing_code()` returns a list of 29 elements containing the value
        #' used as missing value identifier for all weather data.
        #'
        #' @return A named list of 29 elements.
        #'
        #' @examples
        #' \dontrun{
        #' epw$missing_code()
        #' }
        #'
        missing_code = function ()
            epw_missing_code(self, private),
        # }}}

        # initial_missing_value {{{
        #' @description
        #' Get initial value for missing data of weather data variables
        #'
        #' @details
        #' `$initial_missing_value()` returns a list of 16 elements containing
        #' the initial value used to replace missing values for corresponding
        #' weather data.
        #'
        #' @return A named list of 16 elements.
        #'
        #' @examples
        #' \dontrun{
        #' epw$initial_missing_value()
        #' }
        #'
        initial_missing_value = function ()
            epw_initial_missing_value(self, private),
        # }}}

        # range_exist {{{
        #' @description
        #' Get value ranges for existing values of weather data variables
        #'
        #' @details
        #' `$range_exist()` returns a list of 28 elements containing the range
        #' each numeric weather data should fall in. Any values out of this
        #' range are treated as missing.
        #'
        #' @return A named list of 28 elements.
        #'
        #' @examples
        #' \dontrun{
        #' epw$range_exist()
        #' }
        #'
        range_exist = function ()
            epw_range_exist(self, private),
        # }}}

        # range_valid {{{
        #' @description
        #' Get value ranges for valid values of weather data variables
        #'
        #' @details
        #' `$range_valid()` returns a list of 28 elements containing the range
        #' each numeric weather data should fall in. Any values out of this
        #' range are treated as invalid.
        #'
        #' @return A named list of 28 elements.
        #'
        #' @examples
        #' \dontrun{
        #' epw$range_valid()
        #' }
        #'
        range_valid = function ()
            epw_range_valid(self, private),
        # }}}

        # fill_action {{{
        #' @description
        #' Get fill actions for abnormal values of weather data variables
        #'
        #' @details
        #' `$fill_action()` returns a list containing `action`s that EnergyPlus
        #' will perform when certain abnormal data found for corresponding
        #' weather data. There are 3 types of actions in total:
        #'
        #' * `do_nothing`: All abnormal values are left as they are.
        #' * `use_zero`: All abnormal values are reset to zeros.
        #' * `use_previous`: The first abnormal values of variables will be set to the
        #'   initial missing values. All after are set to previous valid one.
        #'
        #' @param type What abnormal type of actions to return. Should be one of
        #'        `"missing"` and `"out_of_range"`. Default: `"missing"`.
        #'
        #' @return A named list.
        #'
        #' @examples
        #' \dontrun{
        #' epw$fill_action("missing")
        #'
        #' epw$fill_action("out_of_range")
        #' }
        #'
        fill_action = function (type = c("missing", "out_of_range"))
            epw_fill_action(self, private, type = type),
        # }}}
        # }}}

        # CORE DATA {{{
        # GETTER (COPY RETURNED) {{{
        # data {{{
        #' @description
        #' Get weather data
        #'
        #' @details
        #' `$data()` returns weather data of specific data period.
        #'
        #' Usually, EPW file downloaded from [EnergyPlus website](https://energyplus.net/)
        #' contains TMY weather data. As years of weather data is not
        #' consecutive, it may be more convenient to align the year values to be
        #' consecutive, which will makes it possible to direct analyze and plot
        #' weather data. The `start_year` argument in `$data()` method can help
        #' to achieve this. However, randomly setting the `year` may result in a
        #' date time series that does not have the same start day of week as
        #' specified in the `DATA PERIODS` header.  eplusr provides a simple
        #' solution for this. By setting `year` to `NULL` and `align_wday` to
        #' `TRUE`, eplusr will calculate a year value (from current year
        #' backwards) for each data period that compliance with the start day of
        #' week restriction.
        #'
        #' Note that if current data period contains AMY data and `start_year`
        #' is given, a warning is given because the actual year values will be
        #' overwritten by input `start_year`. An error is given if:
        #'
        #' * Using input `start_year` introduces invalid date time. This may
        #'   happen when weather data contains leap year but input `start_year`
        #'   is not a leap year.
        #' * Applying specified time zone specified using `tz` introduces
        #'   invalid date time.
        #'
        #' @param period A single positive integer identifying the data period
        #'        index. Data periods information can be obtained using
        #'        \href{../../eplusr/html/Epw.html#method-period}{\code{$period()}}
        #'        described above.
        #' @param start_year A positive integer identifying the year of first
        #'        date time in specified data period. If `NULL`, the values in
        #'        the `year` column are used as years of `datetime` column.
        #'        Default: `NULL`.
        #' @param align_wday Only applicable when `start_year` is `NULL`. If
        #'        `TRUE`, a year value is automatically calculated for specified
        #'        data period that compliance with the start day of week value
        #'        specified in `DATA PERIODS` header.
        #' @param tz A valid time zone to be assigned to the `datetime` column.
        #'        All valid time zone names can be obtained using
        #'        `OlsonNames()`. Default:`"UTC"`.
        #' @param update If `TRUE`, the `year` column are updated according to
        #'        the newly created `datetime` column using `start_year`. If
        #'        `FALSE`, original year data in the `Epw` object is kept.
        #'        Default: `FALSE`.
        #' @param line If `TRUE`, a column named `line` is prepended indicating
        #'        the line numbers where data occur in the actual EPW file.
        #'        Default: `FALSE`.
        #'
        #' @return A [data.table::data.table()] of 36 columns.
        #'
        #' @examples
        #' \dontrun{
        #' # get weather data
        #' str(epw$data())
        #'
        #' # get weather data but change the year to 2018
        #' # the year column is not changed by default, only the returned datetime column
        #' head(epw$data(start_year = 2018)$datetime)
        #' str(epw$data(start_year = 2018)$year)
        #' # you can update the year column too
        #' head(epw$data(start_year = 2018, update = TRUE)$year)
        #'
        #' # change the time zone of datetime column in the returned weather data
        #' attributes(epw$data()$datetime)
        #' attributes(epw$data(tz = "Etc/GMT+8")$datetime)
        #' }
        #'
        data = function (period = 1L, start_year = NULL, align_wday = TRUE, tz = "UTC", update = FALSE, line = FALSE)
            epw_data(self, private, period, start_year, align_wday, tz, update, line),
        # }}}

        # abnormal_data {{{
        #' @description
        #' Get abnormal weather data
        #'
        #' @details
        #' `$abnormal_data()` returns abnormal data of specific data period.
        #' Basically, there are 2 types of abnormal data in `Epw`, i.e. missing
        #' values and out-of-range values. Sometimes, it may be useful to
        #' extract and inspect those data especially when inserting measured
        #' weather data. `$abnormal_data()` does this.
        #'
        #' In the returned [data.table::data.table()], a column named `line`
        #' is created indicating the line numbers where abnormal data occur in
        #' the actual EPW file.
        #'
        #' @param period A single positive integer identifying the data period
        #'        index. Data periods information can be obtained using
        #'        \href{../../eplusr/html/Epw.html#method-period}{\code{$period()}}
        #'        described above.
        #' @param cols A character vector identifying what data columns, i.e.
        #'        all columns except `datetime`, `year`, `month`, `day`, `hour`
        #'        `minute`, and character columns, to search abnormal values. If
        #'        `NULL`, all data columns are used. Default: `NULL`.
        #' @param keep_all If `TRUE`, all columns are returned. If `FALSE`, only
        #'        `line`, `datetime`, `year`, `month`, `day`, `hour` and
        #'        `minute`, together with columns specified in `cols` are
        #'        returned. Default: `TRUE`
        #' @param type What abnormal type of data to return. Should be one of
        #'        `"all"`, `"missing"` and `"out_of_range"`. Default: `"all"`.
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' epw$abnormal_data()
        #'
        #' # only check if there are any abnormal values in air temperature and
        #' # liquid precipitation rate
        #' epw$abnormal_data(cols = c("dry_bulb_temperature", "liquid_precip_rate"))
        #'
        #' # save as above, but only return date time columns plus those 2 columns
        #' epw$abnormal_data(cols = c("dry_bulb_temperature", "liquid_precip_rate"),
        #'     keep_all = FALSE
        #' )
        #'
        #' # same as above, but only check for missing values
        #' epw$abnormal_data(cols = c("dry_bulb_temperature", "liquid_precip_rate"),
        #'     type = "missing"
        #' )
        #'
        #' # same as above, but only check for out-of-range values
        #' epw$abnormal_data(cols = c("dry_bulb_temperature", "liquid_precip_rate"),
        #'     type = "out_of_range"
        #' )
        #' }
        #'
        abnormal_data = function (period = 1L, cols = NULL, keep_all = TRUE, type = c("both", "missing", "out_of_range"))
            epw_abnormal_data(self, private, period, cols, keep_all, type),
        # }}}

        # redundant_data {{{
        #' @description
        #' Get redundant weather data
        #'
        #' @details
        #' `$redundant_data()` returns weather data in `Epw` object that do not
        #' belong to any data period. This data can be further removed using
        #' \href{../../eplusr/html/Epw.html#method-purge}{\code{$purge()}}`
        #' method described below.
        #'
        #' In the returned [data.table::data.table()], a column named `line`
        #' is created indicating the line numbers where redundant data occur in
        #' the actual EPW file.
        #'
        #' @return A [data.table::data.table()] of 37 columns.
        #'
        #' @examples
        #' \dontrun{
        #' epw$redundant_data()
        #' }
        #'
        redundant_data = function ()
            epw_redundant_data(self, private),
        # }}}
        # }}}

        # MODIFY IN-PLACE {{{
        # make_na {{{
        #' @description
        #' Convert abnormal data into NAs
        #'
        #' @details
        #' `$make_na()` converts specified abnormal data into `NA`s in specified
        #' data period. This makes it easier to find abnormal data directly
        #' using `is.na()` instead of using
        #' \href{../../eplusr/html/Epw.html#method-missing_code}{\code{$missing_code()}}
        #'
        #' `$make_na()` and
        #' \href{../../eplusr/html/Epw.html#method-fill_abnormal}{\code{$fill_abnormal()}}
        #' are reversible, i.e.
        #' `$make_na()` can be used to counteract the effects introduced by
        #' \href{../../eplusr/html/Epw.html#method-make_na}{\code{$make_na()}},
        #' and vise a versa.
        #'
        #' **Note** that `$make_na` modify the weather data in-place, meaning
        #' that the returned data from
        #' \href{../../eplusr/html/Epw.html#method-data}{\code{$data()}}
        #' and
        #' \href{../../eplusr/html/Epw.html#method-abnormal_data}{\code{$abnormal_data()}}
        #' may be different after calling `$make_na()`.
        #'
        #' @param missing If `TRUE`, missing values are included. Default:
        #'        `FALSE`.
        #' @param out_of_range If `TRUE`, out-of-range values are included.
        #'        Default: `FALSE`.
        #'
        #' @return The modified `Epw` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # turn all missing values into NAs
        #' summary(epw$data()$liquid_precip_rate)
        #' epw$make_na(missing = TRUE)
        #' summary(epw$data()$liquid_precip_rate)
        #' }
        #'
        make_na = function (missing = FALSE, out_of_range = FALSE)
            epw_make_na(self, private, missing, out_of_range),
        # }}}

        # fill_abnormal {{{
        #' @description
        #' Fill abnormal data using prescribed pattern
        #'
        #' @details
        #' `$fill_abnormal()` fills specified abnormal data using corresponding
        #' actions listed in
        #' \href{../../eplusr/html/Epw.html#method-fill_action}{\code{$fill_action()}}.
        #' For what kinds of actions to be performed, please see
        #' \href{../../eplusr/html/Epw.html#method-fill_action}{\code{$fill_action()}}.
        #' method described above. Note that only if `special` is `TRUE`,
        #' special actions listed in `$fill_action()` is performed. If `special`
        #' is `FALSE`, all abnormal data, including both missing values and
        #' out-of-range values, are filled with corresponding missing codes.
        #'
        #' \href{../../eplusr/html/Epw.html#method-make_na}{\code{$make_na()}}
        #' and `$fill_abnormal()` are reversible, i.e.
        #' \href{../../eplusr/html/Epw.html#method-make_na}{\code{$make_na()}}
        #' can be used to counteract the effects introduced by
        #' `$fill_abnormal()`, and vise a versa.
        #'
        #' **Note** that `$fill_abnormal` modify the weather data in-place,
        #' meaning that the returned data from
        #' \href{../../eplusr/html/Epw.html#method-data}{\code{$data()}}
        #' and
        #' \href{../../eplusr/html/Epw.html#method-abnormal_data}{\code{$abnormal_data()}}
        #' may be different after calling `$fill_abnormal()`.
        #'
        #' @param missing If `TRUE`, missing values are included. Default:
        #'        `FALSE`.
        #' @param out_of_range If `TRUE`, out-of-range values are included.
        #'        Default: `FALSE`.
        #' @param special If `TRUE`, abnormal data are filled using
        #'        corresponding actions listed
        #'        \href{../../eplusr/html/Epw.html#method-fill_action}{\code{$fill_action()}}.
        #'        If `FALSE`, all abnormal data are fill with missing code
        #'        described in
        #'        \href{../../eplusr/html/Epw.html#method-missing_code}{\code{$missing_code()}}.
        #'
        #' @return The modified `Epw` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # turn all missing values into NAs
        #' summary(epw$data()$liquid_precip_rate)
        #' epw$fill_abnormal(missing = TRUE)
        #' summary(epw$data()$liquid_precip_rate)
        #' }
        #'
        fill_abnormal = function (missing = FALSE, out_of_range = FALSE, special = FALSE)
            epw_fill_abnormal(self, private, missing, out_of_range, special),
        # }}}

        # add_unit {{{
        #' @description
        #' Add units to weather data variables
        #'
        #' @details
        #' `$add_unit()` assigns units to numeric weather data using
        #' [units::set_units()] if applicable.
        #'
        #' `$add_unit()`
        #' and
        #' \href{../../eplusr/html/Epw.html#method-drop_unit}{\code{$drop_unit()}}
        #' are reversible, i.e.
        #' `$add_unit()`
        #' can be used to counteract the effects introduced by
        #' \href{../../eplusr/html/Epw.html#method-drop_unit}{\code{$drop_unit()}},
        #' and vise a versa.
        #'
        #' **Note** that `$add_unit` modify the weather data in-place,
        #' meaning that the returned data from
        #' \href{../../eplusr/html/Epw.html#method-data}{\code{$data()}}
        #' and
        #' \href{../../eplusr/html/Epw.html#method-abnormal_data}{\code{$abnormal_data()}}
        #' may be different after calling `$add_unit()`.
        #'
        #' @return The modified `Epw` object itself, invisibly.
        #' @examples
        #' \dontrun{
        #' # get weather data with units
        #' epw$add_unit()
        #' head(epw$data())
        #'
        #' # with units specified, you can easily perform unit conversion using units
        #' # package
        #' t_dry_bulb <- epw$data()$dry_bulb_temperature
        #' units(t_dry_bulb) <- with(units::ud_units, "kelvin")
        #'
        #' head(t_dry_bulb)
        #' }
        #'
        add_unit = function ()
            epw_add_unit(self, private),
        # }}}

        # drop_unit {{{
        #' @description
        #' Remove units in weather data variables
        #'
        #' @details
        #' `$drop_unit()` removes all units of numeric weather data.
        #'
        #' \href{../../eplusr/html/Epw.html#method-add_unit}{\code{$add_unit()}}
        #' and
        #' `$drop_unit()`
        #' are reversible, i.e.
        #' \href{../../eplusr/html/Epw.html#method-drop_unit}{\code{$add_unit()}}
        #' can be used to counteract the effects introduced by
        #' `$drop_unit()`,
        #' and vise a versa.
        #'
        #' **Note** that `$add_unit` modify the weather data in-place,
        #' meaning that the returned data from
        #' \href{../../eplusr/html/Epw.html#method-data}{\code{$data()}}
        #' and
        #' \href{../../eplusr/html/Epw.html#method-abnormal_data}{\code{$abnormal_data()}}
        #' may be different after calling `$add_unit()`.
        #'
        #' @return The modified `Epw` object itself, invisibly.
        #' @examples
        #' \dontrun{
        #' epw$drop_unit()
        #' epw$data()
        #' }
        #'
        drop_unit = function ()
            epw_drop_unit(self, private),
        # }}}

        # purge {{{
        #' @description
        #' Delete redundant weather data observations
        #'
        #' @details
        #' `$purge()` deletes weather data in `Epw` object that do not belong to
        #' any data period.
        #'
        #' @return The modified `Epw` object itself, invisibly.
        #' @examples
        #' \dontrun{
        #' epw$purge()
        #' }
        #'
        purge = function ()
            epw_purge(self, private),
        # }}}
        # }}}

        # SETTER {{{
        # add {{{
        #' @description
        #' Add a data period
        #'
        #' @details
        #' `$add()` adds a new data period into current `Epw` object at
        #' specified position.
        #'
        #' The validity of input data is checked before adding according to
        #' rules following:
        #'
        #' * Column `datetime` exists and has type of `POSIXct`. Note that time
        #'   zone of input date time will be reset to `UTC`.
        #' * It assumes that input data is already sorted, i.e. no further
        #'   sorting is made during validation. This is because when input data
        #'   is TMY data, there is no way to properly sort input data rows only
        #'   using `datetime` column.
        #' * Number of data records per hour should be consistent across input
        #'   data.
        #' * Input number of data records per hour should be the same as
        #'   existing data periods.
        #' * The date time of input data should not overlap with existing data
        #'   periods.
        #' * Input data should have all 29 weather data columns with correct
        #'   types. The `year`, `month`, `day`, and `minute` column are not
        #'   compulsory. They will be created according to values in the
        #'   `datetime` column. Existing values will be overwritten.
        #'
        #' @param data A [data.table::data.table()] of new weather data to add
        #'        or set. Validation is performed according to rules described
        #'        above.
        #' @param realyear Whether input data is AMY data. Default: `FALSE`.
        #' @param name A new string used as name of added or set data period.
        #'        Should not be the same as existing data period names. If
        #'        `NULL`, it is generated automatically in format `Data`,
        #'        `Data_1` and etc., based on existing data period names.
        #'        Default: `NULL`
        #' @param start_day_of_week A single integer or character specifying
        #'        start day of week of input data period. If `NULL`, Sunday is
        #'        used for TMY data and the actual start day of week is used for
        #'        AMY data.  Default: `NULL`.
        #' @param after A single integer identifying the index of data period
        #'        where input new data period to be inserted after. IF `0`,
        #'        input new data period will be the first data period. Default:
        #'        `0`.
        #'
        #' @return The modified `Epw` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # will fail since date time in input data has already been covered by
        #' # existing data period
        #' try(epw$add(epw$data()), silent = TRUE)
        #' }
        #'
        add = function (data, realyear = FALSE, name = NULL, start_day_of_week = NULL, after = 0L)
            epw_add(self, private, data, realyear, name, start_day_of_week, after),
        # }}}

        # set {{{
        #' @description
        #' Replace a data period
        #'
        #' @details
        #' `$set()` replaces existing data period using input new weather data.
        #'
        #' The validity of input data is checked before replacing according to
        #' rules following:
        #'
        #' * Column `datetime` exists and has type of `POSIXct`. Note that time
        #'   zone of input date time will be reset to `UTC`.
        #' * It assumes that input data is already sorted, i.e. no further
        #'   sorting is made during validation. This is because when input data
        #'   is TMY data, there is no way to properly sort input data rows only
        #'   using `datetime` column.
        #' * Number of data records per hour should be consistent across input
        #'   data.
        #' * Input number of data records per hour should be the same as
        #'   existing data periods.
        #' * The date time of input data should not overlap with existing data
        #'   periods.
        #' * Input data should have all 29 weather data columns with right
        #'   types. The `year`, `month`, `day`, and `minute` column are not
        #'   compulsory. They will be created according to values in the
        #'   `datetime` column. Existing values will be overwritten.
        #'
        #' @param data A [data.table::data.table()] of new weather data to add
        #'        or set. Validation is performed according to rules described
        #'        above.
        #' @param realyear Whether input data is AMY data. Default: `FALSE`.
        #' @param name A new string used as name of added or set data period.
        #'        Should not be the same as existing data period names. If
        #'        `NULL`, it is generated automatically in format `Data`,
        #'        `Data_1` and etc., based on existing data period names.
        #'        Default: `NULL`
        #' @param start_day_of_week A single integer or character specifying
        #'        start day of week of input data period. If `NULL`, Sunday is
        #'        used for TMY data and the actual start day of week is used for
        #'        AMY data.  Default: `NULL`.
        #' @param period A single integer identifying the index of data period
        #'        to set.
        #'
        #' @return The modified `Epw` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # change the weather data
        #' epw$set(epw$data())
        #' }
        #'
        set = function (data, realyear = FALSE, name = NULL, start_day_of_week = NULL, period = 1L)
            epw_set(self, private, data, realyear, name, start_day_of_week, period),
        # }}}

        # del {{{
        #' @description
        #' Delete a data period
        #'
        #' @details
        #' `$del()` removes a specified data period. Note that an error will be
        #' given if current `Epw` only contains one data period.
        #'
        #' @param period A single integer identifying the index of data period
        #'        to set.
        #'
        #' @return The modified `Epw` object itself, invisibly.
        #'
        del = function (period)
            epw_del(self, private, period),
        # }}}
        # }}}
        # }}}

        # SAVE {{{
        # is_unsaved {{{
        #' @description
        #' Check if there are unsaved changes in current `Epw`
        #'
        #' @details
        #' `$is_unsaved()` returns `TRUE` if there are modifications on the
        #' `Epw` object since it was read or since last time it was saved, and
        #' returns `FALSE` otherwise.
        #'
        #' @return A single logical value of `TRUE` or `FALSE`.
        #'
        #' @examples
        #' \dontrun{
        #' epw$is_unsaved()
        #' }
        #'
        is_unsaved = function ()
            epw_is_unsaved(self, private),
        # }}}

        # save {{{
        #' @description
        #' Save `Epw` object as an EPW file
        #'
        #' @details
        #' `$save()` saves current `Epw` to an EPW file. Note that if missing
        #' values and out-of-range values are converted to `NA`s using
        #' \href{../../eplusr/html/Epw.html#method-make_na}{\code{$make_na()}},
        #' they will be filled with corresponding missing codes during saving.
        #'
        #' @param path A path where to save the weather file. If `NULL`, the
        #'        path of the weather file itself is used. Default: `NULL`.
        #' @param overwrite Whether to overwrite the file if it already exists.
        #'        Default is `FALSE`.
        #' @param purge Whether to remove redundant data when saving. Default:
        #'        `FALSE`.
        #' @param format_digit Whether to remove trailing digits in weather
        #'        data. Default: `TRUE`.
        #'
        #' @return A length-one character vector, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # save the weather file
        #' epw$save(file.path(tempdir(), "weather.epw"), overwrite = TRUE)
        #' }
        #'
        save = function (path = NULL, overwrite = FALSE, purge = FALSE, format_digit = TRUE)
            epw_save(self, private, path, overwrite, purge, format_digit),
        # }}}
        # }}}

        # print {{{
        #' @description
        #' Print `Idf` object
        #'
        #' @details
        #' `$print()` prints the `Epw` object, including location, elevation,
        #' data source, WMO station, leap year indicator, interval and data
        #' periods.
        #'
        #' @return The `Epw` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' epw$print()
        #' }
        #'
        print = function ()
            epw_print(self, private)
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_path = NULL,
        m_idd = NULL,
        m_idf_env = NULL,
        m_data = NULL,
        m_log = NULL,
        # }}}

        # PRIVATE FUNCTIONS {{{
        uuid = function () private$m_log$uuid,
        log_new_uuid = function () log_new_uuid(private$m_log),

        log_saved = function () log_saved(private$m_log),
        log_unsaved = function () log_unsaved(private$m_log),

        log_new_order = function (id) log_new_order(private$m_log, id),
        log_add_order = function (id) log_add_order(private$m_log, id),
        log_del_order = function (id) log_del_order(private$m_log, id),

        idd_env = function () get_priv_env(private$m_idd)$m_idd_env,
        idf_env = function () private$m_idf_env,

        update_idf_env = function (lst) {
            private$m_idf_env$object <- lst$object
            private$m_idf_env$value <- lst$value
            private$m_idf_env$reference <- lst$reference
        },

        deep_clone = function (name, value) epw_deep_clone(self, private, name, value)
        # }}}
    )
)

# set deep default value to `TRUE`
formals(Epw$clone_method)$deep <- TRUE
formals(Epw$public_methods$clone)$deep <- TRUE
# }}}

#' Read and Parse EnergyPlus Weather File (EPW)
#'
#' `read_epw()` parses an EPW file and returns an `Epw` object. The parsing
#' process is extremely inspired by \[EnergyPlus/WeatherManager.cc\] with some
#' simplifications. For more details on `Epw`, please see [Epw] class.
#'
#' @param path A path of an EnergyPlus `EPW` file.
#' @return An `Epw` object.
#' @examples
#' \dontrun{
#' # read an EPW file from EnergyPlus v8.8 installation folder
#' if (is_avail_eplus(8.8)) {
#'     path_epw <- file.path(
#'         eplus_config(8.8)$dir,
#'         "WeatherData",
#'         "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'    )
#'    read_epw(path_epw)
#' }
#'
#' # read an EPW file from EnergyPlus website
#' path_base <- "https://energyplus.net/weather-download"
#' path_region <- "north_and_central_america_wmo_region_4/USA/CA"
#' path_file <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#' path_epw <- file.path(path_base, path_region, path_file)
#' read_epw(path_epw)
#' }
#'
#' @seealso [Epw] class
#' @author Hongyuan Jia
#' @export
# read_epw {{{
read_epw <- function (path) {
    Epw$new(path)
}
# }}}

# epw_path {{{
epw_path <- function (self, private) {
    private$m_path
}
# }}}
# epw_definition {{{
epw_definition <- function (self, private, class) {
    IddObject$new(class, private$m_idd)
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

    if (length(l)) {
        idf_set(self, private, ..(EPW_CLASS$location) := l, .default = FALSE, .empty = TRUE)
    }

    parse_epw_header_location(private$idf_env())
}
# }}}
# epw_design_condition {{{
epw_design_condition <- function (self, private) {
    # short names {{{
    nm <- c(
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
    )
    # }}}
    val <- parse_epw_header_design(private$idf_env(), strict = TRUE)$value
    setattr(val, "names", nm)

    list(source = val$source,
        heating = val[5:19],
        cooling = val[21:52],
        extremes = val[53:69]
    )
}
# }}}
# epw_typical_extreme_period {{{
epw_typical_extreme_period <- function (self, private) {
    parse_epw_header_typical(private$idf_env(), strict = TRUE)
}
# }}}
# epw_ground_temperature {{{
epw_ground_temperature <- function (self, private) {
    parse_epw_header_ground(private$idf_env(), strict = TRUE)
}
# }}}
# epw_holiday {{{
epw_holiday <- function (self, private, leapyear, dst, holiday) {
    if (missing(leapyear) && missing(dst) && missing(holiday)) {
        return(parse_epw_header_holiday(private$idf_env()))
    }

    hol <- parse_epw_header_holiday(private$idf_env())
    l <- list()

    if (!missing(leapyear)) {
        assert_flag(leapyear)
        l$"..1" <- if (leapyear) "Yes" else "No"

        period <- parse_epw_header_period(private$idf_env())

        # note that parsed start and end day in data period can only be
        # either md or ymd type
        s <- period$period$start_day
        e <- period$period$end_day

        # current is leap year but want to change to non-leap year
        # for md type, it is ok to change only if that period does not cover
        # Feb 29, e.g. [01/02, 02/28]
        # for ymd type, if that period covers multiple years, e.g.
        # [2007-01-01, 2009-01-01], there is a need to check 2008-02-28
        if (hol$leapyear & !leapyear) {
            for (i in seq_along(s)) {
                # in case ymd format that spans multiple years
                feb29 <- lubridate::make_date(c(lubridate::year(s[i]) : lubridate::year(e[i])), 2, 29)
                # for case [2007-01-01, 2009-01-01]
                feb29 <- feb29[!is.na(feb29)]

                # if February exists in the data
                if (any(s[i] <= feb29 & feb29 <= e[i])) {
                    abort(paste0("Failed to change leap year indicator to ", leapyear, ", ",
                        "because data period ",
                        period$period[i, paste0("#", index, " ", surround(name))],
                        " contains weather data of February 29th [", s[i], ", ", e[i], "]."
                    ), "epw_header")
                }
            }

        # current is non-leap year but want to change to leap year
        # for md type, it is ok to change only if that period does not
        # across Feb, e.g. [01/02, 02/28], [03/01, 12/31]
        # for ymd type, it is always OK
        } else if (!hol$leapyear & leapyear) {
            is_md <- is_epwdate_type(s, "md")
            if (any(is_md)) {
                s_md <- s[is_md]
                e_md <- e[is_md]
                for (i in seq_along(s_md)) {
                    # in case ymd format that spans multiple years
                    feb28 <- lubridate::make_date(lubridate::year(s_md[i]), 2L, 28L)

                    if (!all(e_md[i] <= feb28 | feb28 <= s_md[i])) {
                        abort(paste0("Failed to change leap year indicator to ", leapyear, ", ",
                            "because data period ",
                            period$period[is_md][i, paste0("#", index, " ", surround(name))],
                            " contains weather data of February 29th [", s_md[i], ", ", e_md[i], "]."
                        ), "epw_header")
                    }
                }
            }
        }
    }

    if (!missing(dst)) {
        dst <- assert_vector(as.character(dst), len = 2L, .var.name = "Daylight saving time")
        dst <- epw_date(dst)

        # make it possible for directly giving Date-Time object
        if (any(is_epwdate_type(dst, "ymd"))) {
            is_ymd <- is_epwdate_type(dst, "ymd")
            dst[is_ymd] <- ymd_to_md(dst[is_ymd])
        }

        l$"Daylight Saving Start Day" <- format(dst[1])
        l$"Daylight Saving End Day" <- format(dst[2])
    }

    if (!missing(holiday)) {
        assert_list(holiday, len = 2L)
        assert_names(names(holiday), must.include = c("name", "day"))

        holiday <- as.list(unlist(data.table::transpose(as.data.table(holiday))))
        setattr(holiday, "names", paste0("..", 4 + seq_along(holiday)))
        l <- c(l, holiday)
        l$"Number of Holidays" <- length(holiday)/2L
    }

    # store current values in case error occur in later procedures
    obj <- get_idf_object(private$idd_env(), private$idf_env(), EPW_CLASS$holiday)
    val <- get_idf_value(private$idd_env(), private$idf_env(), EPW_CLASS$holiday)
    # store save status
    unsaved <- private$m_log$unsaved

    idf_set(self, private, ..(EPW_CLASS$holiday) := l, .default = FALSE, .empty = TRUE)

    withCallingHandlers(parse_epw_header_holiday(private$idf_env()),
        eplusr_warning_epw_header_num_field = function (w) invokeRestart("muffleWarning"),
        eplusr_error_parse_epw_header = function (e) {
            # restore header value
            env <- private$idf_env()
            env$object <- append_dt(env$object, obj, "object_id")
            env$value <- append_dt(env$value, val, "object_id")
            setorderv(env$object, "object_id")
            setorderv(env$value, "object_id")

            # restore save status
            private$m_log$unsaved <- unsaved
        }
    )
}
# }}}
# epw_comment {{{
#' @importFrom checkmate assert_string
epw_comment <- function (self, private, index = 1L, comment) {
    val <- get_idf_value(private$idd_env(), private$idf_env(), EPW_CLASS[[paste0("comment", index)]])

    if (missing(comment)) {
        if (is.na(val$value_chr)) return(NULL) else return(val$value_chr)
    }

    assert_string(comment, null.ok = TRUE)
    if (is.null(comment)) {
        comment <- NA_character_
    } else {
        comment <- stri_trim_right(comment)
        if (stri_isempty(comment)) {
            comment <- NA_character_
        }
    }
    private$idf_env()$value[J(val$value_id), on = "value_id", value_chr := comment]
    private$log_unsaved()
    private$log_new_uuid()

    if (is.na(comment)) return(NULL) else comment
}
# }}}
# epw_comment1 {{{
#' @importFrom checkmate assert_string
epw_comment1 <- function (self, private, comment) {
    epw_comment(self, private, 1L, comment)
}
# }}}
# epw_comment2 {{{
epw_comment2 <- function (self, private, comment) {
    epw_comment(self, private, 2L, comment)
}
# }}}
# epw_num_period {{{
epw_num_period <- function (self, private) {
    get_idf_value(private$idd_env(), private$idf_env(), EPW_CLASS$period, field = 1L)$value_num
}
# }}}
# epw_interval {{{
epw_interval <- function (self, private) {
    get_idf_value(private$idd_env(), private$idf_env(), EPW_CLASS$period, field = 2L)$value_num
}
# }}}
# epw_period {{{
epw_period <- function (self, private, period, name, start_day_of_week) {
    p <- parse_epw_header_period(private$idf_env())

    if (!missing(period)) {
        period <- assert_count(period, coerce = TRUE)
        if (period > nrow(p$period)) {
            abort(paste0("Invalid data period index found. EPW contains only ",
                nrow(p$period), " data period(s) but ", surround(period), " is specified."
                ), "epw_data_period_index"
            )
        }
    }

    l <- list()
    if (!missing(name)) l[sprintf("Data Period %i Name/Description", period)] <- name
    if (!missing(start_day_of_week)) {
        if (!is.na(wd <- get_epw_wday(start_day_of_week, TRUE))) start_day_of_week <- wd
        l[sprintf("Data Period %i Start Day of Week", period)] <- start_day_of_week
    }

    if (!length(l)) {
        if (missing(period)) return(p$period) else return(p$period[period])
    }

    # store current values in case error occur in later procedures
    obj <- get_idf_object(private$idd_env(), private$idf_env(), EPW_CLASS$period)
    val <- get_idf_value(private$idd_env(), private$idf_env(), EPW_CLASS$period)
    # store save status
    unsaved <- private$m_log$unsaved

    idf_set(self, private, ..(EPW_CLASS$period) := l, .default = FALSE, .empty = TRUE)

    withCallingHandlers(parse_epw_header_period(private$idf_env())$period[period],
        eplusr_warning_epw_header_num_field = function (w) invokeRestart("muffleWarning"),
        eplusr_error_parse_epw_header = function (e) {
            # restore header value
            env <- private$idf_env()
            env$object <- append_dt(env$object, obj, "object_id")
            env$value <- append_dt(env$value, val, "object_id")
            setorderv(env$object, "object_id")
            setorderv(env$value, "object_id")

            # restore save status
            private$m_log$unsaved <- unsaved
        }
    )
}
# }}}
# epw_missing_code {{{
epw_missing_code <- function (self, private) {
    get_epw_data_missing_code()[]
}
# }}}
# epw_initial_missing_value {{{
epw_initial_missing_value <- function (self, private) {
    get_epw_data_init_value()[]
}
# }}}
# epw_range_exist {{{
epw_range_exist <- function (self, private) {
    get_epw_data_range("exist")
}
# }}}
# epw_range_valid {{{
epw_range_valid <- function (self, private) {
    get_epw_data_range("valid")
}
# }}}
# epw_fill_action {{{
epw_fill_action <- function (self, private, type = c("missing", "out_of_range")) {
    get_epw_data_fill_action(match.arg(type))
}
# }}}
# epw_data {{{
epw_data <- function (self, private, period = 1L, start_year = NULL, align_wday = TRUE,
                      tz = "UTC", update = FALSE, line = FALSE) {
    d <- get_epw_data(private$m_data, private$idf_env(), private$m_log$matched,
        period, start_year, align_wday, tz, update)

    assert_flag(line)

    if (!line) set(d, NULL, "line", NULL)
    d[]
}
# }}}
# epw_abnormal_data {{{
epw_abnormal_data <- function (self, private, period = 1L, cols = NULL,
                               keep_all = TRUE, type = c("both", "missing", "out_of_range")) {
    get_epw_data_abnormal(private$m_data, private$idf_env(), private$m_log$matched,
        period, cols, keep_all, type)
}
# }}}
# epw_redundant_data {{{
epw_redundant_data <- function (self, private) {
    get_epw_data_redundant(private$m_data, private$idf_env(), private$m_log$matched)
}
# }}}
# epw_make_na {{{
#' @importFrom checkmate assert_flag
epw_make_na <- function (self, private, missing = FALSE, out_of_range = FALSE) {
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
    private$m_data <- make_epw_data_na(private$m_data, private$idf_env(), private$m_log$matched,
        period = NULL, missing = missing, out_of_range = out_of_range
    )
    invisible(self)
}
# }}}
# epw_fill_abnormal {{{
#' @importFrom checkmate assert_flag
epw_fill_abnormal <- function (self, private, missing = FALSE, out_of_range = FALSE, special = FALSE) {
    assert_flag(missing)
    assert_flag(out_of_range)
    assert_flag(special)

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

    private$m_data <- fill_epw_data_abnormal(private$m_data, private$idf_env(),
        private$m_log$matched, NULL, NULL, missing, out_of_range, special,
        private$m_log$miss_na, private$m_log$range_na
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
# epw_purge {{{
epw_purge <- function (self, private) {
    if (private$m_log$purged) {
        verbose_info("Redundant data has already been purged before. Skip...")
    } else {
        purged <- purge_epw_data_redundant(private$m_data, private$idf_env(), private$m_log$matched)
        if (nrow(purged$data) != nrow(private$m_data)) {
            private$log_unsaved()
            private$log_new_uuid()
        }
        private$m_data <- purged$data
        private$m_log$matched <- purged$matched
    }
    invisible(self)
}
# }}}
# epw_align_data_status {{{
epw_align_data_status <- function (self, private, data, period = NULL) {
    data <- make_epw_data_na(data, private$idf_env(), private$m_log$matched,
        period, missing = private$m_log$miss_na, out_of_range = private$m_log$range_na
    )

    data <- fill_epw_data_abnormal(data, private$idf_env(),
        private$m_log$matched, period, NULL,
        private$m_log$miss_filled,
        private$m_log$range_filled,
        private$m_log$miss_filled_special && private$m_log$range_filled_special,
        private$m_log$miss_na, private$m_log$range_na
    )

    if (private$m_log$unit) data <- add_epw_data_unit(data)

    data
}
# }}}
# epw_add {{{
epw_add <- function (self, private, data, realyear = FALSE, name = NULL,
                     start_day_of_week = NULL, after = 0L) {
    lst <- add_epw_data(private$m_data, private$idf_env(), private$m_log$matched,
        data, realyear, name, start_day_of_week, after)

    lst$data <- epw_align_data_status(self, private, lst$data, lst$period)
    private$m_data <- lst$data
    private$m_log$matched <- lst$matched
    private$update_idf_env(lst$header)

    if (in_verbose()) {
        cli::cat_rule("Info", col = "green")
        cat("New data period has been added successfully:\n\n")

        print(self$period()[lst$period][,
           list(
            " " = paste0(index, ": "),
            Name = name,
            `StartDayOfWeek` = start_day_of_week,
            `StartDay` = start_day, `EndDay` = end_day)],
            class = FALSE, row.names = FALSE
        )

        cli::cat_rule()
    }

    private$log_unsaved()
    private$log_new_uuid()
    private$m_log$purged <- FALSE
    invisible(self)
}
# }}}
# epw_set {{{
epw_set <- function (self, private, data, realyear = FALSE, name = NULL,
                     start_day_of_week = NULL, period = 1L) {
    lst <- set_epw_data(private$m_data, private$idf_env(), private$m_log$matched,
        data, realyear, name, start_day_of_week, period)

    lst$data <- epw_align_data_status(self, private, lst$data, lst$period)

    private$m_data <- lst$data
    private$m_log$matched <- lst$matched
    private$update_idf_env(lst$header)

    if (in_verbose()) {
        cli::cat_rule("Info", col = "green")
        cat("Data period", paste0("#", lst$period), "has been replaced with input data.\n\n")

        print(self$period()[lst$period][,
           list(
            " " = paste0(index, ": "),
            Name = name,
            `StartDayOfWeek` = start_day_of_week,
            `StartDay` = start_day, `EndDay` = end_day)],
            class = FALSE, row.names = FALSE
        )

        cli::cat_rule()
    }

    private$log_unsaved()
    private$log_new_uuid()
    invisible(self)
}
# }}}
# epw_del {{{
epw_del <- function (self, private, period) {
    lst <- del_epw_data(private$m_data, private$idf_env(), private$m_log$matched, period)

    if (in_verbose()) p <- self$period()

    private$m_data <- lst$data
    private$m_log$matched <- lst$matched
    private$update_idf_env(lst$header)

    if (in_verbose()) {
        cli::cat_rule("Info", col = "green")
        cat("Data period", paste0("#", lst$period), "has been successfully deleted:\n\n")

        print(p[lst$period][,
           list(
            " " = paste0(index, ": "),
            Name = name,
            `StartDayOfWeek` = start_day_of_week,
            `StartDay` = start_day, `EndDay` = end_day)],
            class = FALSE, row.names = FALSE
        )

        cli::cat_rule()
    }

    private$log_unsaved()
    private$log_new_uuid()
    invisible(self)
}
# }}}
# epw_is_unsaved {{{
epw_is_unsaved <- function (self, private) {
    private$m_log$unsaved
}
# }}}
# epw_save {{{
epw_save <- function (self, private, path = NULL, overwrite = FALSE, purge = FALSE, format_digit = TRUE) {
    if (is.null(path)) {
        if (is.null(private$m_path)) {
            abort("The Epw object is not created from local file. Please give the path to save.", "epw_not_local")
        } else {
            path <- private$m_path
        }
    }

    assert_string(path)
    if (!has_ext(path, "epw")) abort("'path' should have an file extension of 'epw'", "epw_save_ext")
    assert_flag(overwrite)
    assert_flag(purge)

    # fill all NAs with missing code
    fill <- if (!private$m_log$miss_filled || !private$m_log$range_filled) TRUE else FALSE

    p <- save_epw_file(private$m_data, private$idf_env(), private$m_log$matched,
        path, overwrite, fmt_digit = format_digit,
        fill = fill,
        missing = private$m_log$miss_filled,
        out_of_range = private$m_log$range_filled,
        miss_na = private$m_log$miss_na,
        range_na = private$m_log$range_na,
        purge = purge
    )

    # update path
    private$m_path <- path
    private$log_saved()
    invisible(path)
}
# }}}
# epw_print {{{
epw_print <- function (self, private) {
    cli::cat_rule("EnergyPlus Weather File", line = 2)

    cli::cat_line(format_epw_meta(private$idf_env()))

    cli::cat_line()

    cli::cat_rule("Data Periods")

    period <- parse_epw_header_period(private$idf_env())
    print(period$period[,
       list(Name = name,
        `StartDayOfWeek` = get_epw_wday(start_day_of_week, label = TRUE),
        `StartDay` = start_day, `EndDay` = end_day)],
        class = FALSE
    )

    cli::cat_line()

    cli::cat_rule()
}
# }}}
# epw_deep_clone {{{
epw_deep_clone <- idf_deep_clone
# }}}
# S3 Epw methods {{{
#' @export
str.Epw <- function (object, ...) {
    object$print()
}

#' @export
format.Epw <- function (x, ...) {
    utils::capture.output(x$print())
}

#' @export
`==.Epw` <- function (e1, e2) {
    if (!is_epw(e2)) return(FALSE)
    identical(get_priv_env(e1)$uuid(), get_priv_env(e2)$uuid())
}

#' @export
`!=.Epw` <- function (e1, e2) {
    Negate(`==.Epw`)(e1, e2)
}
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
#' download_weather("los angeles.*tmy3", "LosAngeles", tempdir(), ask = FALSE)
#' }
#' @author Hongyuan Jia
#' @export
# download_weather {{{
download_weather <- function (pattern, filename = NULL, dir = ".", type = c("all", "epw", "ddy"),
                              ask = TRUE, max_match = 3) {
    pattern <- gsub("\\s+", ".", pattern)
    d <- as.data.table(WEATHER_DB)
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
        verbose_info("No matched result found.")
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
                verbose_info(nrow(res), " matched results found. All of them will be downloaded:\n",
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
            verbose_info("One matched results found. Start downloading:\n",
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
        download_file(res$epw_url, res$epw_path)
        download_file(res$ddy_url, res$ddy_path)
        c(res$epw_path, res$ddy_path)
    } else if (type == "ddy") {
        download_file(res$ddy_url, res$ddy_path)
        res$ddy_path
    } else {
        download_file(res$epw_url, res$epw_path)
        res$epw_path
    }
}
# }}}
