#' @importFrom data.table as.data.table
#' @importFrom stringi stri_detect_regex
#' @importFrom cli rule cat_rule
#' @importFrom R6 R6Class
#' @importFrom utils menu download.file
#' @include impl-epw.R
NULL

#' Read, and modify an EnergyPlus Weather File (EPW)
#'
#' Reading an EPW file starts with function [read_epw()], which parses an EPW
#' file and returns an `Epw` object. The parsing process is basically as
#' \[EnergyPlus/WeatherManager.cc\] in EnergyPlus, with some simplifications.
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
#' @section Usage:
#' ```
#' epw <- read_epw(path)
#' epw$location(city, state_province, country, data_source, wmo_number, latitude, longitude, time_zone, elevation)
#' epw$design_condition()
#' epw$typical_extreme_period()
#' epw$ground_temperature()
#' epw$holiday(leapyear, dst, holiday)
#' epw$comment1(comment)
#' epw$comment2(comment)
#' epw$num_period()
#' epw$interval()
#' epw$period(period, name, start_day_of_week)
#' epw$missing_code()
#' epw$initial_missing_value()
#' epw$range_exist()
#' epw$range_valid()
#' epw$fill_action()
#' epw$data(period = 1L, start_year = NULL, tz = "UTC", update = FALSE)
#' epw$abnormal_data(period = 1L, cols = NULL, keep_all = TRUE, type = c("both", "missing", "out_of_range"))
#' epw$redundant_data()
#' epw$make_na(period = NULL, missing = FALSE, out_of_range = FALSE)
#' epw$fill_abnormal(period = NULL, missing = FALSE, out_of_range = FALSE, special = FALSE)
#' epw$add_unit()
#' epw$drop_unit()
#' epw$purge()
#' epw$add(data, realyear = FALSE, name = NULL, start_day_of_week = NULL, after = 0L, warning = TRUE)
#' epw$set(data, realyear = FALSE, name = NULL, start_day_of_week = NULL, period = 1L, warning = TRUE)
#' epw$delete(period)
#' epw$clone(deep = TRUE)
#' epw$is_unsaved()
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
#'
#' \subsection{LOCATION Header}{
#' ```
#' epw$location(city, state_province, country, data_source, wmo_number, latitude, longitude, time_zone, elevation)
#' ```
#'
#' `$location()` takes new values for `LOCATION` header fields and  returns the
#' parsed values of `LOCATION` header in a list format. If no input is given,
#' current `LOCATION` header value is returned.
#'
#' **Arguments**:
#'
#' * `city`: A string of city name recorded in the `LOCATION` header.
#' * `state_province`: A string of state or province name recorded in the
#'   `LOCATION` header.
#' * `country`: A string of country name recorded in the `LOCATION` header.
#' * `data_source`: A string of data source recorded in the `LOCATION` header.
#' * `wmo_number`: A string of WMO (World Meteorological Organization) number
#'   recorded in the `LOCATION` header.
#' * `latitude`: A number of latitude recorded in the `LOCATION` header. North
#'   latitude is positive and south latitude is negative. Should in range `[-90,
#'   +90]`.
#' * `longitude`: A number of longitude recorded in the `LOCATION` header. East
#'   longitude is positive and west longitude is negative. Should in range
#'   `[-180, +180]`.
#' * `time_zone`: A number of time zone recorded in the `LOCATION` header. Usually
#'   presented as the offset hours from UTC time. Should in range `[-12, +14]`.
#' * `elevation`: A number of elevation recorded in the `LOCATION` header.
#'   Should in range `[-1000, 9999.9)`.
#' }
#'
#' \subsection{DESIGN CONDITION Header}{
#' ```
#' epw$design_condition()
#' ```
#'
#' `$design_condition()` returns the parsed values of `DESIGN CONDITION` header in
#' a list format with 4 elements:
#'
#' * `source`: A string of source field
#' * `heating`: A list, usually of length 16, of the heading design conditions
#' * `cooling`: A list, usually of length 32, of the cooling design conditions
#' * `extreme`: A list, usually of length 16, of the extreme design conditions
#'
#' For the meaning of each element, please see ASHRAE Handbook of Fundamentals.
#' }
#'
#' \subsection{TYPICAL/EXTREME Header}{
#' ```
#' epw$typical_extreme_period()
#' ```
#'
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
#' }
#'
#' \subsection{GROUND TEMPERATURE Header}{
#' ```
#' epw$ground_temperature()
#' ```
#'
#' `$ground_temperature()` returns the parsed values of `GROUND TEMPERATURE`
#' header in a [data.table][data.table::data.table()] format with 7 columns:
#'
#' * `index`: Integer type. The index of ground temperature record
#' * `depth`: Numeric type. The depth of the ground temperature is measured
#' * `month`: Integer type. The month when the ground temperature is measured
#' * `soil_conductivity`: Numeric type. The soil conductivity at measured depth
#' * `soil_density`: Numeric type. The soil density at measured depth
#' * `soil_specific heat`: Numeric type. The soil specific heat at measured depth
#' * `temperature`: Numeric type. The measured group temperature
#' }
#'
#' \subsection{HOLIDAYS/DAYLIGHT SAVINGS Header}{
#' ```
#' epw$holiday(leapyear, dst, holiday)
#' ```
#'
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
#'  Validation process below is performed when changing the `leapyear`
#'  indicator:
#'
#'  * If current record of `leapyear` is `TRUE`, but new input is `FALSE`, the
#'    modification is only conducted when all data periods do not cover Feb 29.
#'  * If current record of `leapyear` is `FALSE`, but new input is `TRUE`, the
#'    modification is only conducted when TMY data periods do not across Feb,
#'    e.g. \[01/02, 02/28\], \[03/01, 12/31\]; for AMY data, it is always OK.
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
#' **Arguments**:
#'
#' * `leapyear`: Either `TRUE` or `FALSE`.
#' * `dst`: A length 2 EPW date specifications identifying the start and end of
#'   daylight saving time. For example, `c(3.10, 10.3)`.
#' * `holiday`: a list or a data.frame containing two elements (columns) `name`
#'   and `day` where `name` are the holiday names and `day` are valid EPW date
#'   specifications For example, `list(name = c("New Year's Day", "Christmas
#'   Day"), day = c("1.1", "25 Dec"))`.
#' }
#'
#' \subsection{COMMENT1 and COMMENT2 Header}{
#' ```
#' epw$comment1(comment)
#' epw$comment2(comment)
#' ```
#'
#' `$comment1()` and `$comment2()` both takes a single string of new comments
#' and replaces the old comment with input one. If no input is given, current
#' comment is returned.
#'
#' **Arguments**:
#'
#' * `comment`: A string of new comments.
#' }
#'
#' \subsection{DATA PERIODS Header}{
#' ```
#' epw$num_period()
#' epw$interval()
#' epw$period(period, name, start_day_of_week)
#' ```
#'
#' `$num_period()` returns a single positive integer of how many data periods
#' current `Epw` contains.
#'
#' `$interval()` returns a single positive integer of how many records of weather
#' data exist in one hour.
#'
#' `$period()` takes a data period index, a new period name and start day of
#' week specification, and uses that input to replace the data period's name and
#' start day of week. If no input is given, data periods in current `Epw` is
#' returned.
#'
#' `$period()` returns a [data.table][data.table::data.table()] with 5 columns:
#'
#' * `index`: Integer type. The index of data period.
#' * `name`: Character type. The name of data period.
#' * `start_day_of_week`: Integer type. The start day of week of data period.
#' * `start_day`: Date (EpwDate) type. The start day of data period.
#' * `end_day`: Date (EpwDate) type. The end day of data period.
#'
#' **Arguments**:
#'
#' * `index`: A positive integer vector identifying the data period indexes.
#' * `name`: A character vector used as new names for specified data periods.
#'   Should have the same length as `index`.
#' * `start_day_of_week`: A character vector or an integer vector used as the
#'   new start days of week of specified data periods. Should have the same
#'   length as `index`.
#' }
#'
#' @section Weather Data Specifications:
#' ```
#' epw$missing_code()
#' epw$initial_missing_value()
#' epw$range_exist()
#' epw$range_valid()
#' epw$fill_action(type = c("missing", "out_of_range"))
#' ```
#'
#' `$missing_code()` returns a list of 29 elements containing the value used as
#' missing value identifier for all weather data.
#'
#' `$initial_missing_value()` returns a list of 16 elements containing the
#' initial value used to replace missing values for corresponding weather data.
#'
#' `$range_exist()` returns a list of 28 elements containing the range each
#' numeric weather data should fall in. Any values out of this range are treated
#' as missing.
#'
#' `$range_valid()` returns a list of 28 elements containing the range each
#' numeric weather data should fall in. Any values out of this range are treated
#' as invalid.
#'
#' `$fill_action()` returns a list containing `action`s that EnergyPlus and also
#' `$fill_abnormal()` in `Epw` class will perform when certain abnormal data
#' found for corresponding weather data. There are 3 types of actions in total:
#'
#' * `do_nothing`: All abnormal values are left as they are.
#' * `use_zero`: All abnormal values are reset to zeros.
#' * `use_previous`: The first abnormal values of variables will be set to the
#'   initial missing values. All after are set to previous valid one.
#'
#' **Arguments**:
#'
#' * `type`: What abnormal type of actions to return. Should be one of `"missing"`
#'   and `"out_of_range"`. Default: `"missing"`.
#'
#' @section Query Weather Data:
#' ```
#' epw$data(period = 1L, start_year = NULL, tz = "UTC", update = FALSE)
#' epw$abnormal_data(period = 1L, cols = NULL, keep_all = TRUE, type = c("both", "missing", "out_of_range"))
#' epw$redundant_data()
#' ```
#'
#' `$data()` returns weather data of specific data period.
#'
#' Usually, EPW file downloaded from [EnergyPlus website](https://energyplus.net/)
#' contains TMY weather data. As years of weather data is not consecutive, it may
#' be more convenient to align the year values to be consecutive, which will
#' makes it possible to direct analyze and plot weather data. The `start_year`
#' argument in `$data()` method can help to achieve this. However, randomly
#' setting the `year` may result in a date time series that does not have the
#' same start day of week as specified in the `DATA PERIODS` header. eplusr
#' provides a simple solution for this. By setting `year` to `NULL` and
#' `align_wday` to `TRUE`, eplusr will calculate a year value (from current year
#' backwards) for each data period that compliance with the start day of week
#' restriction.
#'
#' Note that if current data period contains AMY data and `start_year` is given,
#' a warning is given because the actual year values will be overwritten by
#' input `start_year`. Also, an error is given if using input `start_year`
#' introduces invalid date time. This may happen when weather data contains leap
#' year but input `start_year` is not a leap year. An error will also be issued
#' if applying specified time zone specified using `tz` introduces invalid date
#' time.
#'
#' `$abnormal_data()` returns abnormal data of specific data period. Basically,
#' there are 2 types of abnormal data in `Epw`, i.e. missing values and
#' out-of-range values. Sometimes, it may be useful to extract and inspect those
#' data especially when inserting measured weather data. `$abnormal_data()`
#' does this.
#'
#' `$redundant_data()` returns weather data in `Epw` object that do not belong
#' to any data period. This data can be further removed using `$pruge()` method
#' described below.
#'
#' For `$abnormal_data()` and `$redundant_data()`, a new column named `line` is
#' created indicating the line numbers where abnormal data and redundant data
#' occur in the actual EPW file.
#'
#' **Arguments**:
#'
#' * `period`: A single positive integer identifying the data period index. Data
#'   periods information can be obtained using `$period()` described above.
#' * `start_year`: A positive integer identifying the year of first date time in
#'   specified data period. If `NULL`, the values in the `year` column are used
#'   as years of `datetime` column. Default: `NULL`.
#' * `align_wday`: Only applicable when `start_year` is `NULL`. If `TRUE`, a
#'   year value is automatically calculated for specified data period that
#'   compliance with the start day of week value specified in `DATA PERIODS`
#'   header.
#' * `tz`: A valid time zone to be assigned to the `datetime` column. All valid
#'   time zone names can be obtained using [base::OlsonNames()]. Default:`"UTC"`.
#' * `update`: If `TRUE`, the `year` column are updated according to the newly
#'   created `datetime` column using `start_year`. If `FALSE`, original year
#'   data in the `Epw` object is kept. Default: `FALSE`.
#' * `cols`: A character vector identifying what data columns, i.e. all columns
#'   except `datetime`, `year`, `month`, `day`, `hour` and `minute`, to search
#'   abnormal values. If `NULL`, all data columns are used. Default: `NULL`.
#' * `keep_all`: If `TRUE`, all columns are returned. If `FALSE`, only `line`,
#'   `datetime`, `year`, `month`, `day`, `hour` and `minute`, together with
#'   columns specified in `cols` are returned. Default: `TRUE`
#' * `type`: What abnormal type of data to return. Should be one of `"all"`,
#'   `"missing"` and `"out_of_range"`. Default: `"all"`.
#'
#' @section Modify Weather Data In-Place:
#' ```
#' epw$make_na(period = NULL, missing = FALSE, out_of_range = FALSE)
#' epw$fill_abnormal(period = NULL, missing = FALSE, out_of_range = FALSE, special = FALSE)
#' epw$add_unit()
#' epw$drop_unit()
#' epw$purge()
#' ```
#'
#' **Note** that all these 5 methods modify the weather data in-place, meaning
#' that the returned data from `$data()` and `$abnormal_data()` may be different
#' after calling these methods.
#'
#' `$make_na()` converts specified abnormal data into `NA`s in specified data
#' period. This makes it easier to find abnormal data directly using
#' [base::is.na()] instead of using `$missing_code()`.
#'
#' `$fill_abnormal()` fills specified abnormal data using corresponding actions
#' listed in `$fill_action()`. For what kinds of actions to be performed, please
#' see `$fill_action()` method described above. Note that only if `special` is
#' `TRUE`, special actions listed in `$fill_action()` is performed. If `special`
#' is `FALSE`, all abnormal data, including both missing values and out-of-range
#' values, are filled with corresponding missing codes.
#'
#' `$make_na()` and `$fill_abnormal()` are reversible, i.e. `$make_na()` can be
#' used to counteract the effects introduced by `$fill_abnormal()`, and vise a
#' versa.
#'
#' `$add_unit()` assigns units to numeric weather data using
#' [units::set_units()] if applicable.
#'
#' `$drop_unit()` removes all units of numeric weather data.
#'
#' Similarly, `$add_unit()` and `$drop_unit()` are reversible, i.e.
#' `$add_unit()` can be used to counteract the effects introduced by
#' `$drop_unit()`, and vise a versa.
#'
#' `$purge()` deletes weather data in `Epw` object that do not belong to any
#' data period.
#'
#' **Arguments**:
#' * `period`: A positive integer vector identifying the data period indexes.
#'   Data periods information can be obtained using `$period()` described
#'   above. If `NULL`, all data periods are included. Default: `NULL`.
#' * `missing`: If `TRUE`, missing values are included. Default: `FALSE`.
#' * `out_of_range`: If `TRUE`, out-of-range values are included. Default:
#'   `FALSE`.
#' * `special`: If `TRUE`, abnormal data are filled using corresponding actions
#'   listed `$fill_action()`. If `FALSE`, all abnormal data are fill with
#'   missing code described in `$missing_code()`.
#
#' @section Set Weather Data:
#' ```
#' epw$add(data, realyear = FALSE, name = NULL, start_day_of_week = NULL, after = 0L, warning = TRUE)
#' epw$set(data, realyear = FALSE, name = NULL, start_day_of_week = NULL, period = 1L, warning = TRUE)
#' epw$delete(period)
#' ```
#'
#' `$add()` adds a new data period into current `Epw` object at specified
#' position.
#'
#' `$set()` replaces existing data period using input new weather data.
#'
#' The validity of input data is checked before adding or setting according to
#' rules following:
#'
#' * Column `datetime` exists and has type of `POSIXct`. Note that time zone of
#'   input date time will be reset to `UTC`.
#' * It assumes that input data is already sorted, i.e. no further sorting is
#'   made during validation. This is because when input data is TMY data, there
#'   is no way to properly sort input data rows only using `datetime` column.
#' * Number of data records per hour should be consistent across input data.
#' * Input number of data records per hour should be the same as
#'   existing data periods.
#' * The date time of input data should not overlap with existing data periods.
#' * Input data should have all 29 weather data columns with right types. The
#'   `year`, `month`, `day`, and `minute` column are not compulsory. They will
#'   be created according to values in the `datetime` column. Existing values
#'   will be overwritten.
#'
#' `$delete(period)` removes one specified data period.
#'
#' **Arguments**:
#'
#' * `data`: A [data.table][data.table::data.table()] of new weather data to add
#'   or set.
#'   Validation is performed according to rules described above.
#' * `realyear`: Whether input data is AMY data. Default: `FALSE`.
#' * `name`: A new string used as name of added or set data period. Should not
#'   be the same as existing data period names. If `NULL`, it is generated
#'   automatically in format `Data`, `Data_1` and etc., based on existing data
#'   period names. Default: `NULL`
#' * `start_day_of_week`: A single integer or character specifying start day of
#'   week of input data period. If `NULL`, Sunday is used for TMY data and the
#'   actual start day of week is used for AMY data. Default: `NULL`.
#' * `after`: A single integer identifying the index of data period where input new
#'   data period to be inserted after. IF `0`, input new data period will be
#'   the first data period. Default: `0`.
#' * `period`: A single integer identifying the index of data period to set.
#' * `warning`: If `TRUE`, warnings are given if any missing data, out-of-range
#'   data are found. Default: `TRUE`.
#'
#' @section Save:
#' ```
#' epw$is_unsaved()
#' epw$save(path, overwrite = FALSE, purge = FALSE)
#' ```
#' `$is_unsaved()` returns `TRUE` if there are any modifications to the `Epw`
#' object since it was saved or since it was created if not saved before.
#'
#' `$save()` saves current `Epw` to an EPW file. Note that if missing values and
#' out-of-range values are converted to `NA`s using `$make_na()`, they will be
#' filled with corresponding missing codes during saving.
#'
#' **Arguments**
#'
#' * `path`: A path where to save the weather file. If `NULL`, the path of the
#'   weather file itself is used. Default: `NULL`.
#' * `overwrite`: Whether to overwrite the file if it already exists. Default is
#'   `FALSE`.
#' * `purge`: Whether to remove redundant data when saving. Default: `FALSE`.
#'
#' @section Clone:
#'
#' ```
#' epw$clone(deep = TRUE)
#' ```
#'
#' `$clone()` copies and returns the cloned `Epw` object. Because `Epw` uses
#' `R6Class` under the hook which has "modify-in-place" semantics, `epw_2 <-
#' epw_1` does not copy `epw_1` at all but only create a new binding to `epw_1`.
#' Modify `epw_1` will also affect `epw_2` as well, as these two are exactly the
#' same thing underneath. In order to create a complete cloned copy, please use
#' `$clone(deep = TRUE)`.
#'
#' **Arguments**
#'
#' * `deep`: Has to be `TRUE` if a complete cloned copy is desired. Default:
#'   `TRUE`.
#'
#' @section Print:
#' ```
#' epw$print()
#' print(epw)
#' ```
#'
#' `$print()` prints the `Epw` object, including location, elevation, data
#' source, WMO station, leap year indicator, interval and data periods.
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
#' # get header data
#' epw$location()
#' epw$location()
#' epw$design_condition()
#' epw$typical_extreme_period()
#' epw$ground_temperature()
#' epw$holiday()
#' epw$comment1()
#' epw$comment2()
#' epw$num_period()
#' epw$interval()
#' epw$period()
#'
#' # modify location data
#' epw$location(city = "MyCity")
#'
#' # add daylight saving time
#' epw$holiday(dst = c(3.10, 11.3))
#'
#' # modify data period name
#' epw$period(1, name = "test")
#'
#' # change start day of week
#' epw$(1, start_day_of_week = 3)
#'
#' # get data specifications
#' epw$missing_code()
#' epw$initial_missing_value()
#' epw$range_exist()
#' epw$range_valid()
#' epw$fill_action()
#'
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
#' # get weather data with units
#' epw$add_unit()
#' head(epw$data())
#' # with units specified, you can easily perform unit conversion using units
#' # package
#' t_dry_bulb <- epw$data()$dry_bulb_temperature
#' units(t_dry_bulb) <- with(units::ud_units, "kelvin")
#' head(t_dry_bulb)
#'
#' # change the time zone of datetime column in the returned weather data
#' attributes(epw$data()$datetime)
#' attributes(epw$data(tz = "Etc/GMT+8")$datetime)
#'
#' # change the weather data
#' epw$set(epw$data())
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
#' process is extremely inspired by \[EnergyPlus/WeatherManager.cc\] with some
#' simplifications. For more details on `Epw`, please see [Epw] class.
#'
#' @param path A path of an EnergyPlus `EPW` file.
#' @param warning If `TRUE`, warnings are given if any missing data, out of
#' range data and redundant data is found. Default: `FALSE`. All these data can
#' be also retrieved using methods in [Epw] class.
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
            if (is_string(path) && file.exists(path)) {
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
            private$m_log$unsaved <- FALSE
            private$m_log$uuid <- unique_id()
        },
        # }}}

        path = function ()
            epw_path(self, private),

        # HEADER getter and setter {{{
        location = function (city, state_province, country, data_source, wmo_number,
                             latitude, longitude, time_zone, elevation)
            epw_location(self, private, city, state_province, country, data_source,
                         wmo_number, latitude, longitude, time_zone, elevation),

        design_condition = function ()
            epw_design_condition(self, private),

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

        fill_action = function (type = c("missing", "out_of_range"))
            epw_fill_action(self, private, type = type),
        # }}}
        # CORE DATA {{{
        # GETTER (COPY RETURNED) {{{
        get_data = function (year = NULL, unit = FALSE, tz = "UTC", update = FALSE)
            epw_get_data(self, private, year, unit, tz, update),

        data = function (period = 1L, start_year = NULL, align_wday = TRUE, tz = "UTC", update = FALSE)
            epw_data(self, private, period, start_year, align_wday, tz, update),

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

        is_unsaved = function ()
            epw_is_unsaved(self, private),

        save = function (path = NULL, overwrite = FALSE)
            epw_save(self, private, path, overwrite),

        print = function ()
            epw_print(self, private)
    ),

    private = list(
        m_path = NULL,
        m_header = NULL,
        m_data = NULL,
        m_log = NULL,

        deep_clone = function (name, value) {
            epw_deep_clone(self, private, name, value)
        }
    )
)

# set deep default value to `TRUE`
formals(Epw$clone_method)$deep <- TRUE
formals(Epw$public_methods$clone)$deep <- TRUE
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
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)
    private$m_header$location
}
# }}}
# epw_design_condition {{{
epw_design_condition <- function (self, private) {
    copy(private$m_header$design)
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
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)
    copy(private$m_header$holiday)
}
# }}}
# epw_comment1 {{{
epw_comment1 <- function (self, private, comment) {
    if (missing(comment)) {
        return(private$m_header$comment1)
    } else {
        assert(is_string(comment))
        log_unsaved(private$m_log)
        log_new_uuid(private$m_log)
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
        log_unsaved(private$m_log)
        log_new_uuid(private$m_log)
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
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)
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
epw_data <- function (self, private, period = 1L, start_year = NULL, align_wday = TRUE,
                      tz = "UTC", update = FALSE) {
    get_epw_data(private$m_data, private$m_header, period, start_year, align_wday, tz, update)
}
# }}}
# epw_get_data {{{
epw_get_data <- function (self, private, year = NULL, unit = FALSE, tz = "UTC", update = FALSE) {
    warn("warning_eplusr_deprecated_fun",
        paste(
            "`$get_data()` in Epw class is deprecated. Please use `$data() instead.",
            "Note that `unit` argument is equal to call `$add_unit()`.",
            "Please see documentation of `$add_unit()` for more details."
        )
    )

    self$data(1L, year, FALSE, tz, update)
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
        if (nrow(lst$data) != nrow(private$m_data)) {
            log_unsaved(private$m_log)
            log_new_uuid(private$m_log)
        }
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

        cli::cat_rule("Info", col = "green")
        cat("New data period has been added successfully:\n")

        print(private$m_header$period$period[after + 1L,
           list(" " = paste0(index, ": "), Name = name,
            `StartDayOfWeek` = get_epw_wday(start_day_of_week, label = TRUE),
            `StartDay` = start_day, `EndDay` = end_day)],
            row.names = FALSE
        )
    }

    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)
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
        cli::cat_rule("Info", col = "green")
        cat("Data period", paste0("#", period), "has been replaced with input data.\n")

        print(private$m_header$period$period[period,
           list(" " = paste0(index, ": "), Name = name,
            `StartDayOfWeek` = get_epw_wday(start_day_of_week, label = TRUE),
            `StartDay` = start_day, `EndDay` = end_day)],
            row.names = FALSE
        )
    }

    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)
    invisible(self)
}
# }}}
# epw_set_data {{{
epw_set_data <- function (self, private, data) {
    .deprecated_fun("$set_data()", "$set()", "Epw", "0.10.0")
    self$set(data)
}
# }}}
# epw_delete {{{
epw_delete <- function (self, private, period) {
    l <- delete_epw_data(private$m_data, private$m_header, period)
    private$m_header <- l$header
    private$m_data <- l$data
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)
    invisible(self)
}
# }}}
# epw_is_unsaved {{{
epw_is_unsaved <- function (self, private) {
    private$m_log$unsaved
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
    log_saved(private$m_log)
    invisible(self)
}
# }}}
# epw_print {{{
epw_print <- function (self, private) {
    print_epw_header(private$m_header)
}
# }}}
# epw_deep_clone {{{
epw_deep_clone <- function (self, private, name, value) {
    if (is_idd(value)) {
        value
    } else if (is.environment(value)) {
        list2env(as.list.environment(value))
    } else {
        value
    }
}
# }}}
# S3 Epw methods {{{
#' @export
str.Epw <- function (object, ...) {
    object$print()
}

#' @export
format.Epw <- function (x, ...) {
    paste0(utils::capture.output(x$print()), collapse = "\n")
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
