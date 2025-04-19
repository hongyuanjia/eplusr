#' @importFrom stringi stri_isempty stri_match_first_regex stri_replace_all_charclass
#' @importFrom stringi stri_split_charclass stri_split_fixed stri_sub "stri_sub<-"
#' @importFrom stringi stri_trans_tolower stri_trans_totitle stri_trans_toupper
#' @importFrom lubridate as_datetime make_date force_tz tz
#' @importFrom lubridate days_in_month minutes hours days years leap_year
#' @importFrom lubridate mday month yday year "year<-" parse_date_time
#' @importFrom data.table "%chin%" chmatch copy fread fwrite as.data.table
#' @importFrom data.table data.table melt.data.table set setattr setcolorder
#' @importFrom data.table setnames setorderv
#' @importFrom units set_units drop_units
#' @importFrom cli cat_rule cat_line
#' @importFrom utils combn
#' @importFrom stats na.omit
#' @include parse.R
#' @include utils.R
#' @include assert.R

# HELPER
# combine_date {{{
combine_date <- function(year = NULL, month, day, hour, minute = NULL) {
    y <- if (!is.null(year)) paste0(year, "/") else ""
    m <- if (!is.null(minute)) minite else "XX"
    paste0(y, month, "/", day, " ", lpad(hour, "0", 2L), ":", m)
}
# }}}
# std_atm_press {{{
std_atm_press <- function(elevation) 101325 * (1 - 2.25577e-05 * elevation)^5.2559
# }}}
# as_date {{{
as_date <- function(x, ...) {
    as.Date(lubridate::as_date(x, ...))
}
# }}}

# CONSTANTS
# EPW_CLASS {{{
EPW_CLASS <- list(
    location = "LOCATION",
    design = "DESIGN CONDITIONS",
    typical = "TYPICAL/EXTREME PERIODS",
    ground = "GROUND TEMPERATURES",
    holiday = "HOLIDAYS/DAYLIGHT SAVINGS",
    comment1 = "COMMENTS 1",
    comment2 = "COMMENTS 2",
    period = "DATA PERIODS",
    data = "WEATHER DATA"
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

# IDD
# get_epw_idd {{{
get_epw_idd <- function(version = "before_2021") {
    if (!length(.globals$epw) || !length(.globals$epw[[version]])) {
        EpwIdd$new(
            system.file("extdata/epw.idd", package = "eplusr"),
            version = version
        )
    }

    .globals$epw[[version]]
}
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
parse_epw_file <- function(path, encoding = "unknown") {
    # get the correct EPW specs
    idd <- get_epw_idd(get_epw_ver_specs(path))
    idd_env <- get_priv_env(idd)$idd_env()

    # read and parse header
    epw_header <- parse_epw_header(idd_env, path, encoding = encoding)
    attr(epw_header, "version_specs") <- idd$version_specs()

    # read core weather data
    epw_data <- parse_epw_data(idd_env, path, encoding = encoding)

    # add line indicator
    set(epw_data, NULL, "line", seq_len(nrow(epw_data)))

    # parse date time
    epw_data[, datetime := stringi::stri_datetime_create(year, month, day, hour, tz = "UTC", lenient = TRUE)]

    # match date time
    matched <- match_epw_data(idd_env, epw_header, epw_data)

    # clean and set column order
    set(epw_data, NULL, "line", NULL)
    setcolorder(epw_data, "datetime")

    list(header = epw_header, data = epw_data, matched = matched)
}
# }}}
## HEADER
# get_epw_ver_specs {{{
# guess the version of DESIGN CONDITIONS specs
get_epw_ver_specs <- function(path, encoding = "unknown") {
    dt_in <- read_lines(path, nrows = 8L, encoding = encoding)

    # in case header does not any fields, e.g. "LOCATION\n"
    dt_in[!stri_detect_fixed(string, ","), string := paste0(string, ",")]

    str <- stri_split_fixed(dt_in$string, ",")
    cls <- stri_trans_toupper(vapply(str, .subset2, "", 1L))
    ind <- match(EPW_CLASS$design, cls)
    val <- stri_trans_toupper(str[[ind]][-1L])

    # in case of invalid EPW file, use the original one by default
    if (is.na(ind) || length(val) < 20L) return("before_2021")

    # Handle design conditions in ASHRAE HOF 2021, which adds a new field named
    # 'Wind shelter factor' in heating conditions and removes an existing field
    # named 'Condition 1 Extreme Maximum Wet-Bulb Temperature'.
    # Newer EPW files from climate.building.org use those design conditions.
    # See #571
    # this is design conditions from ASHRAE HOF 2021
    if (stri_trim_both(val[[20L]]) != "COOLING") {
        "after_2021"
        # this is design conditions before ASHRAE HOF 2021
    } else {
        "before_2021"
    }
}
# }}}
# parse_epw_header {{{
parse_epw_header <- function(idd_env, path, strict = FALSE, encoding = "unknown") {
    dt_in <- read_lines(path, nrows = 8L, encoding = encoding)

    # in case header does not any fields, e.g. "LOCATION\n"
    dt_in[!stri_detect_fixed(string, ","), string := paste0(string, ",")]

    # type enum
    type_enum <- list(unknown = 0L, special = 1L, macro = 2L, comment = 3L,
        object = 4L, object_value = 5L, value = 6L, value_last = 7L
    )

    # separate lines into bodies, and comments
    set(dt_in, NULL, "body", paste0(dt_in$string, ";"))
    set(dt_in, NULL, "comment", "")

    # mark lines
    set(dt_in, NULL, "type", type_enum$value_last)

    # object table
    dt <- tryCatch(sep_object_table(dt_in, type_enum, idd_env),
        eplusr_error_parse_idf_class = function(e) {
            parse_error("epw", "Invalid header name", e$data, subtype = "header_name")
        }
    )
    dt_object <- dt$object
    dt <- dt$left

    add_class_name(idd_env, dt)
    # handling comments to make sure they are parsed as a single string.
    # See #318
    ln_cmt <- dt[J(c(EPW_CLASS$comment1, EPW_CLASS$comment2)), on = "class_name", nomatch = NULL, which = TRUE]
    if (length(ln_cmt)) {
        set(dt, ln_cmt, "body", "[EPLUSRPLACEHOLDER];")
    }
    # detect invalid lines with multiple semicolon (;)
    # in case there are multiple semicolon in one line
    if (any(stri_count_fixed(dt$body, ";") > 1L)) {
        parse_error("epw", "Invalid header line found",
            dt[stri_count_fixed(body, ";") > 1L],
            subtype = "header_line"
        )
    }
    set(dt, NULL, "class_name", NULL)

    # value table
    # now it is save to set escape to TRUE since only comments can have multiple
    # semicolons
    dt_value <- tryCatch(get_value_table(dt, idd_env, escape = TRUE),
        eplusr_error_parse_idf_field = function(e) {
            d <- e$data[, list(field_index = max(field_index)), by = c("line", "string", "class_id")]
            add_rleid(d)
            add_class_property(idd_env, d, c("class_name", "min_fields", "num_fields"))
            msg <- gsub("Class", "Header", errormsg_field_index(d), fixed = TRUE)
            parse_error("epw", "Invalid header field number found", d, post = msg, subtype = "header_field")
        }
    )
    if (length(ln_cmt)) {
        s <- dt$string[ln_cmt]
        comma_loc <- stri_locate_first_fixed(s, ",")[, 1L]
        s <- stri_sub(s, comma_loc + 1L)
        s[stri_isempty(s)] <- NA_character_
        dt_value[J(c(EPW_CLASS$comment1, EPW_CLASS$comment2)), on = "class_name", value_chr := s]
    }

    # update object name
    dt_object <- update_object_name(dt_object, dt_value)

    # remove unuseful columns
    set(dt_value, NULL, setdiff(names(dt_value),
        c("value_id", "value_chr", "value_num", "object_id", "field_id")), NULL
    )

    # column order
    setcolorder(dt_object, c("object_id", "object_name", "object_name_lower", "comment", "class_id"))
    setcolorder(dt_value, c("value_id", "value_chr", "value_num", "object_id", "field_id"))

    dt_reference <- data.table(
            object_id = integer(0L),     value_id = integer(0L),
        src_object_id = integer(0L), src_value_id = integer(0L),
        src_enum = integer(0L)
    )

    header <- list(object = dt_object, value = dt_value, reference = dt_reference)

    # auto fill "0" for some empty headers
    if (!strict) {
        cls_nm <- c(EPW_CLASS$design, EPW_CLASS$typical, EPW_CLASS$ground)
        cls_id <- idd_env$class[J(cls_nm), on = "class_name", class_id]
        cls_id <- cls_id[cls_id %in% header$object$class_id]

        if (length(cls_id)) {
            val <- get_idf_value(idd_env, header, cls_id)

            id <- val[, list(value_chr = value_chr[[1L]], num = max(field_index)), by = "object_id"][
                num == 1L & is.na(value_chr), object_id]
            if (length(id)) {
                header$value[J(id), on = "object_id", `:=`(value_chr = "0", value_num = 0)]
            }
        }
    }

    validate_epw_header(idd_env, header, strict = strict)
    header
}
# }}}
# parse_epw_header_location {{{
parse_epw_header_location <- function(idd_env, header, strict = FALSE, transform = TRUE) {
    if (!transform) return(header)
    val <- get_idf_value(idd_env, header, EPW_CLASS$location,
        property = c("type_enum", "field_name_us")
    )
    nm <- val$field_name_us
    val <- get_value_list(val)
    setattr(val, "names", nm)
    val
}
# }}}
# parse_epw_header_design {{{
parse_epw_header_design <- function(idd_env, header, strict = FALSE, transform = TRUE) {
    obj <- get_idf_object(idd_env, header, EPW_CLASS$design, property = "num_extensible_group")
    val <- get_idf_value(idd_env, header, EPW_CLASS$design, property = c("extensible_group", "type_enum"))

    update_epw_header_num_field(header, obj, val, strict = strict)

    if (!transform) return(header)

    if (nrow(val) == 1L) return(list())
    get_idf_table(idd_env, header, EPW_CLASS$design, string_value = FALSE)[
        , list(index, field, value)]
}
# }}}
# parse_epw_header_typical {{{
parse_epw_header_typical <- function(idd_env, header, strict = FALSE, transform = TRUE) {
    obj <- get_idf_object(idd_env, header, EPW_CLASS$typical, property = "num_extensible_group")
    val <- get_idf_value(idd_env,  header, EPW_CLASS$typical, property = "extensible_group")

    update_epw_header_num_field(header, obj, val, strict = strict)

    if (max(val$extensible_group) == 0) {
        if (!transform) return(header) else return(data.table())
    }

    val[extensible_group > 0, extensible_field_index := seq_len(.N), by = c("object_id", "extensible_group")]
    start_day <- val[J(3L), on = "extensible_field_index", epw_date(value_chr)]
    end_day <- val[J(4L), on = "extensible_field_index", epw_date(value_chr)]

    if (checkmate::anyMissing(start_day)) {
        i <- which(is.na(start_day))
        invld <- val[J(i, 3L), on = c("extensible_group", "extensible_field_index")]
        issue_epw_header_parse_error_single(obj, invld, i)
    }

    if (checkmate::anyMissing(end_day)) {
        i <- which(is.na(end_day))
        invld <- val[J(i, 4L), on = c("extensible_group", "extensible_field_index")]
        issue_epw_header_parse_error_single(obj, invld, i)
    }

    if (!transform) return(header)

    data.table(
        index = seq_along(start_day),
        name = val[J(1L), on = "extensible_field_index", value_chr],
        type = val[J(2L), on = "extensible_field_index", value_chr],
        start_day = start_day,
        end_day = end_day
    )
}
# }}}
# parse_epw_header_ground {{{
parse_epw_header_ground <- function(idd_env, header, strict = FALSE, transform = TRUE) {
    obj <- get_idf_object(idd_env, header, EPW_CLASS$ground, property = "num_extensible_group")
    val <- get_idf_value(idd_env,  header, EPW_CLASS$ground, property = "extensible_group")

    update_epw_header_num_field(header, obj, val, strict = strict)

    if (!transform) return(header)

    if (nrow(val) == 1L) return(data.table())

    val <- get_idf_table(idd_env, header, EPW_CLASS$ground, group_ext = "index", wide = TRUE)
    set(val, NULL, 2:4, NULL)
    setnames(val, c("index", "depth", "soil_conductivity", "soil_density",
            "soil_specific_heat", MONTH))
    val <- val[, by = "index", lapply(.SD, unlist)]
    val[, index := .I]
    val
}
# }}}
# parse_epw_header_holiday {{{
parse_epw_header_holiday <- function(idd_env, header, strict = FALSE, transform = TRUE) {
    obj <- get_idf_object(idd_env, header, EPW_CLASS$holiday, property = "num_extensible_group")
    val <- get_idf_value(idd_env,  header, EPW_CLASS$holiday, property = "extensible_group")

    update_epw_header_num_field(header, obj, val, 4L, strict = strict)

    dst <- val[J(c(2L, 3L)), on = "field_index", value_chr]
    setattr(dst, "names", c("start_day", "end_day"))

    # if dst start date and end date should have same existence status
    if (any((dst["start_day"] != "0" && dst["end_day"] == "0") ||
            (dst["start_day"] == "0" && dst["end_day"] != "0"))
    ) {
        parse_error("epw", paste("Invalid", obj$class_name[[1L]], "header"), num = 1,
            post = sprintf("Invalid Daylight Saving Start/End Day pair found: ('%s', '%s'). %s",
                dst["start_day"], dst["end_day"], "Should both be '0' or neither be '0'."
            ),
            subtype = "header"
        )
    }

    leapyear <- tolower(val$value_chr[[1L]]) == "yes"
    dst <- epw_date(dst)

    if (nrow(val) <= 4L) {
        if (!transform) {
            return(header)
        } else {
            return(list(leapyear = leapyear, dst = dst, holiday = data.table()))
        }
    }

    val[extensible_group > 0, extensible_field_index := seq_len(.N), by = c("object_id", "extensible_group")]
    holiday <- val[J(2L), on = "extensible_field_index", epw_date(value_chr)]

    if (any(invld <- is.na(holiday))) {
        i <- which(invld)
        invld <- val[J(i, 2L), on = c("extensible_group", "extensible_field_index")]
        issue_epw_header_parse_error_single(obj, invld, i)
    }
    if (any(realyr <- is_epwdate_type(holiday, "ymd"))) {
        i <- which(realyr)
        invld <- val[J(i, 2L), on = c("extensible_group", "extensible_field_index")]
        issue_epw_header_parse_error_single(obj, invld, i, ". Can not contain year specification.")
    }

    # check if duplicated names
    name <- val[J(1L), on = "extensible_field_index", value_chr]
    if (anyDuplicated(tolower(name))) {
        i <- which(duplicated(tolower(name)))
        invld <- val[J(i, 1L), on = c("extensible_group", "extensible_field_index")]
        issue_epw_header_parse_error_single(obj, invld, NULL, ". Cannot be the same as existing period names.")
    }

    if (!transform) return(header)

    holiday <- data.table(index = seq_along(holiday), name = name, day = holiday)

    list(leapyear = leapyear, dst = dst, holiday = holiday)
}
# }}}
# parse_epw_header_period {{{
parse_epw_header_period <- function(idd_env, header, strict = FALSE, transform = TRUE) {
    obj <- get_idf_object(idd_env, header, EPW_CLASS$period, property = "num_extensible_group")
    val <- get_idf_value(idd_env,  header, EPW_CLASS$period, property = "extensible_group")

    update_epw_header_num_field(header, obj, val, strict = strict)

    interval <- val$value_num[[2L]]
    # check interval {{{
    if (60L %% interval != 0L) {
        issue_epw_header_parse_error_single(obj, val[2L], 1L,
            " does not result in integral number of minutes between records."
        )
    }
    # }}}

    val[extensible_group > 0, extensible_field_index := seq_len(.N), by = c("object_id", "extensible_group")]
    name <- val[J(1L), on = "extensible_field_index", value_chr]
    start_day_of_week <- val[J(2L), on = "extensible_field_index", get_epw_wday(value_chr)]
    start_day <- val[J(3L), on = "extensible_field_index", epw_date(value_chr)]
    end_day <- val[J(4L), on = "extensible_field_index", epw_date(value_chr)]

    # check if duplicated names {{{
    if (anyDuplicated(tolower(name))) {
        i <- which(duplicated(tolower(name)))
        invld <- val[J(i, 1L), on = c("extensible_group", "extensible_field_index")]
        issue_epw_header_parse_error_single(obj, invld, NULL, ". Cannot be the same as existing period names.")
    }
    # }}}

    # check start day and end day {{{
    if (any(wd <- !is_epwdate_type(start_day, c("md", "ymd")))) {
        i <- which(wd)
        invld <- val[J(i, 3L), on = c("extensible_group", "extensible_field_index")]
        issue_epw_header_parse_error_single(obj, invld, i)
    }
    if (any(wd <- !is_epwdate_type(end_day, c("md", "ymd")))) {
        i <- which(wd)
        invld <- val[J(i, 4L), on = c("extensible_group", "extensible_field_index")]
        issue_epw_header_parse_error_single(obj, invld, i)
    }

    # update year value according to leapyear element in HOLIDAYS header
    hol <- get_idf_value(idd_env, header, EPW_CLASS$holiday, field = "LeapYear Observed")
    # in case holiday header is broken
    if (is.na(hol$value_chr)) issue_epw_header_parse_error_single(
        get_idf_object(idd_env, header, EPW_CLASS$holiday), hol
    )
    ly <- if (tolower(hol$value_chr) == "yes") TRUE else FALSE

    start_day <- reset_epwdate_year(start_day, ly)
    end_day <- reset_epwdate_year(end_day, ly)

    if (any(mism <- is_epwdate_type(start_day, "ymd") & !is_epwdate_type(end_day, "ymd"))) {
        i <- which(mism)
        invld <- val[J(i), on = "extensible_group"]
        end_day[i] <- set_epwdate_year(end_day[i], lubridate::year(start_day[i]))

        if (any(is.na(end_day[i]))) {
            issue_epw_header_parse_error_conn(obj, invld, i, 4L, 3L,
                msg_pre = paste0(
                    "Start day contains year but end day does not. ",
                    "Assuming same year for those data periods introduces invalid date. ",
                    "Usually this means that the year is not a leap year but end day occurs on Feb 29."
                )
            )
        } else {
            issue_epw_header_parse_error_conn(obj, invld, i, 4L, 3L, stop = FALSE,
                msg_pre = paste0(
                    "Start day of data period contains year but end day does not. ",
                    "Assuming same year for those data periods."
                )
            )
            invld[J(i, 4L), on = c("extensible_group", "extensible_field_index"),
                value_chr := format(end_day[i])]
            header$value[invld, on = "value_id", value_chr := i.value_chr]
        }
    }
    if (any(mism <- !is_epwdate_type(start_day, "ymd") & is_epwdate_type(end_day, "ymd"))) {
        i <- which(mism)
        invld <- val[J(i), on = "extensible_group"]
        end_day[i] <- set_epwdate_year(end_day[i], lubridate::year(start_day[i]))

        issue_epw_header_parse_error_conn(obj, invld, i, 4L, 3L, stop = FALSE,
            msg_pre = paste0(
                "End day of data period contains year but start day does not. ",
                "Assuming non-real-year for those data periods."
            )
        )
        invld[J(i, 4L), on = c("extensible_group", "extensible_field_index"),
            value_chr := format(end_day[i])]
        header$value[invld, on = "value_id", value_chr := i.value_chr]
    }

    if (any(mism <- is_epwdate_type(start_day, "ymd") & start_day_of_week != wday(start_day))) {
        i <- which(mism)
        invld <- val[J(i), on = "extensible_group"]

        parse_warn("epw", paste("Invalid", obj$class_name, "header"), num = length(i),
            post = paste0(
                "Actual start day of week mismatches with specified.\n",
                paste0(collapse = "\n", sprintf(
                    " #%s| Invalid %s found: '%s', with actual day of week being '%s' for %s ('%s')",
                    lpad(seq_along(i), "0"),
                    invld[J(2L), on = "extensible_field_index", field_name],
                    invld[J(2L), on = "extensible_field_index", value_chr],
                    wday(start_day, TRUE)[i],
                    invld[J(3L), on = "extensible_field_index", field_name],
                    invld[J(3L), on = "extensible_field_index", value_chr]
                ))
            )
        )
    }

    # check if not real year and end day smaller than start day
    if (any(mism <- as_date(start_day) > as_date(align_epwdate_type(end_day, start_day)))) {
        i <- which(mism)
        invld <- val[J(i), on = "extensible_group"]
        issue_epw_header_parse_error_conn(obj, invld, i, 4L, 3L, ". Should be equal as or later than %s ('%s').")
    }
    # }}}

    # check if leap day is found in period but leap year is not allowed in the header {{{
    if (!ly &&
        any(ld <- format(as.Date.EpwDate(start_day), "%m-%d") == "02-29" |
                  format(as.Date.EpwDate(end_day), "%m-%d") == "02-29")
    ) {
        i <- which(ld)
        invld <- val[J(i), on = "extensible_group"]
        parse_error("epw", paste("Invalid", obj$class_name[[1L]], "header"), num = sum(i),
            post = paste0(
                "EPW file header '", EPW_CLASS$holiday, "' indicates no leap year ",
                "but start/end day on Feb 29 found.\n",
                paste0(collapse = "\n", sprintf(
                    " #%s| %s '%s' & %s '%s'",
                    lpad(seq_along(i), "0"),
                    invld[J(3L), on = "extensible_field_index", field_name],
                    invld[J(3L), on = "extensible_field_index", value_chr],
                    invld[J(4L), on = "extensible_field_index", field_name],
                    invld[J(4L), on = "extensible_field_index", value_chr]
                ))
            ),
            subtype = "header"
        )
    }
    # }}}

    # check each period does not overlap {{{
    n <- val$value_num[[1]]
    if (n > 1) {
        comb <- utils::combn(n, 2L, simplify = FALSE)

        for (i in comb) {
            overlapped <- !(as_date(start_day[i[1L]]) > as_date(end_day[i[2L]]) ||
                            as_date(end_day[i[1L]]) < as_date(start_day[i[2L]]))
            if (overlapped) {
                parse_error("epw", paste("Invalid", obj$class_name[[1L]], "header"), num = 1L,
                    post = paste0(
                        "Each data period should not have overlapped with each other.\n",
                        paste0(collapse = "\n", sprintf(
                            "Data Period %i [%s, %s] overlapped with Data Period %i [%s, %s]",
                            i[2L], start_day[i[2L]], end_day[i[2L]],
                            i[1L], start_day[i[1L]], end_day[i[1L]]
                        ))
                    )
                )
            }
        }
    }
    # }}}

    # update format
    val[J(3L), on = "extensible_field_index", value_chr := format(start_day)]
    val[J(4L), on = "extensible_field_index", value_chr := format(end_day)]
    header$value[val, on = "value_id", value_chr := i.value_chr]

    if (!transform) return(header)

    list(
        interval = interval,
        period = data.table(
            index = seq_along(name),
            name = name,
            start_day_of_week = get_epw_wday(start_day_of_week, TRUE),
            start_day = start_day,
            end_day = end_day
        )
    )
}
# }}}
# validate_epw_header {{{
validate_epw_header <- function(idd_env, header, strict = FALSE) {
    # validation against IDD_env
    valid <- validate_epw_header_basic(idd_env, header)
    assert_valid(valid, epw = TRUE)

    parse_epw_header_design(idd_env,  header, strict = strict, transform = FALSE)
    parse_epw_header_typical(idd_env, header, strict = strict, transform = FALSE)
    parse_epw_header_ground(idd_env,  header, strict = strict, transform = FALSE)
    parse_epw_header_holiday(idd_env, header, strict = strict, transform = FALSE)
    parse_epw_header_period(idd_env,  header, strict = strict, transform = FALSE)

    header
}
# }}}
# validate_epw_header_basic {{{
validate_epw_header_basic <- function(idd_env, header, class = NULL, field = NULL) {
    chk <- level_checks()
    chk$auto_field <- FALSE
    chk$reference <- FALSE

    if (is.null(class)) {
        valid <- validate_on_level(idd_env, header, level = chk)
    } else {
        dt_object <- get_idf_object(idd_env, header, class)
        dt_value <- get_idf_value(idd_env, header, class, field = field)
        valid <- validate_on_level(idd_env, header, dt_object, dt_value, level = chk)
    }

    # exclude incomplete extensible group for soil properties fields in 'GROUND
    # TEMPERATURES'
    if (nrow(valid$incomplete_extensible) && EPW_CLASS$ground %chin% valid$incomplete_extensible$class_name) {
        add_field_property(idd_env, valid$incomplete_extensible, "extensible_group")
        valid$incomplete_extensible[extensible_group > 0,
            extensible_field_index := seq_len(.N), by = c("object_id", "extensible_group")]

        ext_grp <- valid$incomplete_extensible[J(EPW_CLASS$ground), on = "class_name",
            by = "extensible_group", list(incomplete = anyNA(value_chr[-(2:4)]))
        ][J(FALSE), on = "incomplete", nomatch = NULL, extensible_group]

        if (length(ext_grp)) {
            valid$incomplete_extensible <- valid$incomplete_extensible[
                !J(EPW_CLASS$ground, ext_grp),
                on = c("class_name", "extensible_group")]
        }

        set(valid$incomplete_extensible, NULL, c("extensible_group", "extensible_field_index"), NULL)
    }

    setattr(valid, "class", c("EpwValidity", class(valid)))
    valid
}
# }}}
# update_epw_header_num_field {{{
update_epw_header_num_field <- function(header, dt_object, dt_value, i = 1L, strict = FALSE) {
    if ((num <- max(dt_value$extensible_group)) > 0 && !is.na(dt_value$value_num[i]) && dt_value$value_num[i] != num) {
        if (strict) {
            parse_error("epw", paste("Invalid", dt_object$class_name, "header"), num = 1,
                post = sprintf(
                    "%s ('%s') did not match the actual number ('%s').",
                    dt_value$field_name[i],
                    dt_value$value_num[i],
                    num
                ),
                subtype = "header_num_field"
            )
        }

        parse_warn("epw", paste("Invalid", dt_object$class_name, "header"), num = 1,
            post = sprintf(
                "%s ('%s') did not match the actual number ('%s'). The later will be used.",
                dt_value$field_name[i],
                dt_value$value_num[i],
                num
            ),
            subtype = "header_num_field"
        )

        set(dt_value, i, "value_num", as.double(num))
        set(dt_value, i, "value_chr", as.character(num))

        # update the value table
        header$value[dt_value[i], on = "value_id", `:=`(
            value_chr = i.value_chr, value_num = i.value_num)]
    }

    header
}
# }}}
# issue_epw_header_parse_error_single {{{
issue_epw_header_parse_error_single <- function(obj, val, i = NULL, msg_post = "") {
    if (is.null(i)) i <- seq_len(nrow(val))
    parse_error("epw", paste("Invalid", obj$class_name[[1L]], "header"), num = length(i),
        post = sprintf(" #%s| Invalid %s found: '%s'%s",
            lpad(seq_along(i), "0"), val$field_name[i], val$value_chr[i], msg_post
        ),
        subtype = "header"
    )
}
# }}}
# issue_epw_header_parse_error_conn {{{
issue_epw_header_parse_error_conn <- function(obj, val, i, index1, index2,
                                              fmt_conn = ", with %s being '%s'",
                                              msg_pre = NULL, stop = TRUE) {
    title <- paste("Invalid", obj$class_name[[1L]], "header")
    fmt <- paste0(" #%s| Invalid %s found: '%s'", fmt_conn)
    post <- sprintf(fmt, lpad(seq_along(i), "0"),
        val[J(index1), on = "extensible_field_index", field_name],
        val[J(index1), on = "extensible_field_index", value_chr],
        val[J(index2), on = "extensible_field_index", field_name],
        val[J(index2), on = "extensible_field_index", value_chr]
    )
    if (!is.null(msg_pre)) {
        post <- paste0(msg_pre, "\n", paste0(post, collapse = "\n"))
    }

    if (stop) {
        parse_error("epw", title, num = length(i), post = post, subtype = "header")
    } else {
        parse_warn("epw", title, num = length(i), post = post, subtype = "header")
    }
}
# }}}
# get_epw_wday {{{
DAYOFWEEK <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
get_epw_wday <- function(x, label = FALSE, abbr = FALSE, monday_start = TRUE){
    wd <- if (monday_start) DAYOFWEEK else c(DAYOFWEEK[7L], DAYOFWEEK[-7L])

    res <- if (label) rep(NA_character_, length(x)) else rep(NA_integer_, length(x))

    if (is.numeric(x)) {
        is_ok <- x == trunc(x) & x >= 1L & x <= 7L
        if (!label) {
            res[is_ok] <- as.integer(x[is_ok])
        } else {
            if (abbr) {
                res[is_ok] <- stri_sub(wd[x[is_ok]], to = 3L)
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
MONTH <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
get_epw_month <- function(x, label = FALSE){
    match_in_vec(x, MONTH, label = label)
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
epw_date <- function(x, leapyear = TRUE) {
    as_EpwDate(x, leapyear = leapyear)
}
init_epwdate_vctr <- function(len, init = NA) {
    structure(rep(as.Date(init), len), class = c("EpwDate", "Date"))
}
assign_epwdate <- function(x) {
    setattr(x, "class", c("EpwDate", "Date"))
}
get_epwdate_type <- function(x) {
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
is_epwdate_type <- function(x, type) {
    get_epwdate_type(x) %in% unlist(EPWDATE_TYPE[type], use.names = FALSE)
}
set_epwdate_year <- function(x, year) {
    tmp <- as_date(x)
    lubridate::year(tmp) <- year
    assign_epwdate(tmp)
}
align_epwdate_type <- function(x, to) {
    if (length(to) != 1L) assert_same_len(x, to)
    t <- get_epwdate_type(x)
    # only for julian and month day
    can_align <- t >= EPWDATE_TYPE$jul & t <= EPWDATE_TYPE$md
    x[can_align] <- set_epwdate_year(x[can_align], lubridate::year(to[can_align]))
    x
}
reset_epwdate_year <- function(x, leapyear) {
    # expect empty and real year
    t <- get_epwdate_type(x)
    if (any(t == EPWDATE_TYPE$nth | t == EPWDATE_TYPE$last)) warning("Cannot reset year of nth or last format date.")
    x[t == EPWDATE_TYPE$jul] <- set_epwdate_year(x[t == EPWDATE_TYPE$jul], if (leapyear) EPWDATE_YEAR$leap$jul else EPWDATE_YEAR$noleap$jul)
    x[t == EPWDATE_TYPE$md] <- set_epwdate_year(x[t == EPWDATE_TYPE$md], if (leapyear) EPWDATE_YEAR$leap$md else EPWDATE_YEAR$noleap$md)
    x
}
ymd_to_md <- function(x) {
    is_leap <- lubridate::leap_year(as_date(x))
    x[is_leap] <- set_epwdate_year(x[is_leap], EPWDATE_YEAR$leap$md)
    x[!is_leap] <- set_epwdate_year(x[!is_leap], EPWDATE_YEAR$noleap$md)
    x
}
# }}}
# as_EpwDate {{{

#' Convert to EnergyPlus Weather File date
#'
#' `as_EpwDate()` converts inputs to EnergyPlus Weather File (EPW) dates.
#'
#' @details
#' EnergyPlus supports multiple formats of date specification
#' Reference: Table 2.14, Chap 2 Weather Converter Program, Auxiliary Program
#'
#' Those formats include:
#'
#' 1. Julian day of year
#' 2. num_Month/num_Day
#' 3. num_Month/num_Day/num_Year (only for DataPeriod)
#' 4. num_Day alpha_Month
#' 5. alpha_Month num_Day
#' 6. num Weekday In Month (only for Holiday/DaylightSavingPeriod)
#' 7. last Weekday In Month (only for Holiday/DaylightSavingPeriod)
#'
#' @param x An integer vector or a character vector.
#' @param leapyear Whether support leap year. Default: `TRUE`
#' @export
#' @keywords internal
as_EpwDate <- function(x, leapyear = TRUE) {
    UseMethod("as_EpwDate")
}
#' @export
#' @keywords internal
as_EpwDate.default <- function(x, leapyear = TRUE) {
    stop("Missing method to convert <", class(x)[1L], "> object to <EpwDate>.")
}
#' @export
#' @keywords internal
# as_EpwDate.integer {{{
as_EpwDate.integer <- function(x, leapyear = TRUE) {
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
#' @export
#' @keywords internal
as_EpwDate.numeric <- function(x, leapyear = TRUE) {
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
#' @export
#' @keywords internal
as_EpwDate.character <- function(x, leapyear = TRUE) {
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
#' @export
#' @keywords internal
as_EpwDate.logical <- as_EpwDate.integer
# }}}
# as_EpwDate.Date {{{
#' @export
#' @keywords internal
as_EpwDate.Date <- function(x, ...) {
    # treat as default "yyyy-mm-dd" format
    assign_epwdate(copy(x))
}
# }}}
# as_EpwDate.POSIXt{{{
#' @export
#' @keywords internal
as_EpwDate.POSIXt <- function(x, ...) {
    # treat as default "yyyy-mm-dd" format
    assign_epwdate(as_date(x))
}
# }}}
# as_EpwDate.EpwDate {{{
#' @export
#' @keywords internal
as_EpwDate.EpwDate <- function(x, ...) x
# }}}
# parse_epwdate_md {{{
parse_epwdate_md <- function(x, leapyear = TRUE) {
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
parse_epwdate_ymd <- function(year, month, day, leapyear = TRUE) {
    res <- as_date(lubridate::make_date(year, month, day))
    res[is.na(res)] <- as_date(lubridate::make_date(day, year, month))
    res
}
# }}}
# parse_epwdate_wday {{{
parse_epwdate_wday <- function(x, leapyear = TRUE) {
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
    locate_wkd <- function(wkd, ref_day, wkd1, n) {
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
is_EpwDate <- function(x) {
    inherits(x, "EpwDate")
}
# }}}
#' @export
# format.EpwDate {{{
format.EpwDate <- function(x, m_spc = TRUE, ...) {
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
format_epwdate_julian <- function(x) {
    suffix <- rep("th", length(x))
    suffix[x == 1L] <- "st"
    suffix[x == 2L] <- "nd"
    suffix[x == 3L] <- "rd"
    paste0(x, suffix, " day")
}
format_epwdate_nthwkd <- function(x, last = FALSE) {
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
# as.character.EpwDate {{{
as.character.EpwDate <- format.EpwDate
# }}}
#' @export
# print.EpwDate {{{
print.EpwDate <- function(x, ...) {
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
`[.EpwDate` <- function(x, i) {
    NextMethod("[")
}
# }}}
#' @export
# [[.EpwDate {{{
`[[.EpwDate` <- function(x, i) {
    NextMethod("[[")
}
# }}}
#' @export
# [<-.EpwDate {{{
`[<-.EpwDate` <- function(x, ..., value) {
    assign_epwdate(NextMethod("[<-.Date", value = value, ...))
}
# }}}
#' @export
# [[<-.EpwDate {{{
`[[<-.EpwDate` <- function(x, ..., value) {
    assign_epwdate(NextMethod("[[", value = value, ...))
}
# }}}
#' @export
# c.EpwDate {{{
c.EpwDate <- function(...) {
    assign_epwdate(c.Date(...))
}
# }}}
#' @export
# as.Date.EpwDate {{{
as.Date.EpwDate <- function(x, ...) {
    class(x) <- "Date"
    x
}
# }}}
#' @export
# as.POSIXct.EpwDate {{{
as.POSIXct.EpwDate <- function(x, ...) {
    lubridate::force_tz(lubridate::as_datetime(as.Date.EpwDate(x)), tzone = "UTC")
}
# }}}
# }}}
## DATA
# parse_epw_data {{{
parse_epw_data <- function(idd_env, path, encoding = "unknown") {
    num_header <- 8L

    cls <- idd_env$class[J(EPW_CLASS$data), on = "class_name"]
    type <- unlist(get_epw_data_type(idd_env))

    # parse the rest of file {{{
    # colnames refers to column "Long Name" in Table 2.8 in
    # "AuxiliaryPrograms.pdf" of EnergyPlus 8.6
    # TODO: fread will directly skip those few abnormal rows
    header_epw_data <- fread(path, sep = ",", skip = num_header, nrows = 0L, header = FALSE, encoding = encoding)
    if (ncol(header_epw_data) != cls$min_fields) {
        parse_error("epw", "Invalid weather data column", num = 1L,
            post = sprintf("Expected %i fields in EPW weather data instead of '%i' in current file",
                cls$min_fields, ncol(header_epw_data)),
            subtype = "data_column"
        )
    }

    # As documented, fread will only promote a column to a higher type if
    # colClasses requests it. It won't downgrade a column to a lower type since
    # NAs would result.

    # This means that even if a column is specified as integer in colClasses, it
    # still could be resulted as character or double.
    epw_data <- suppressWarnings(fread(path, skip = num_header, col.names = names(type), colClasses = type))
    epw_data <- check_epw_data_type(idd_env, epw_data, type)
    # }}}

    # handle abnormal values of present weather codes
    epw_data[, present_weather_codes := {
        # delete single quote, e.g. "'999999999'", as fread() will read then as it is
        stri_sub(present_weather_codes[stri_sub(present_weather_codes, 1L, 1L) == "'"], 1L, 1L) <- ""
        stri_sub(present_weather_codes[stri_sub(present_weather_codes, -1L, -1L) == "'"], -1L, -1L) <- ""
        # if not a 9-length string, including empty string "", replace with default missing code
        present_weather_codes[nchar(present_weather_codes) != 9L] <-
            get_idd_field(idd_env, EPW_CLASS$data, "present_weather_codes",
                underscore = TRUE, property = "missing_chr")$missing_chr
        # replace non-digits with "9"
        stri_replace_all_charclass(present_weather_codes, "[^0-9]", "9")
    }]

    epw_data
}
# }}}
# match_epw_data {{{
match_epw_data <- function(idd_env, epw_header, epw_data, period = NULL, tz = "UTC") {
    dp <- parse_epw_header_period(idd_env, epw_header)
    holiday <- parse_epw_header_holiday(idd_env, epw_header)
    data_period <- match_epw_data_period(dp$period, period)

    # check if real year
    realyear <- get_epwdate_type(data_period$start_day) == EPWDATE_TYPE$ymd

    # get datetime range for each data period
    data_period[, by = "index", c("year", "month", "day", "hour", "minute", "step", "num") :=
        get_epw_datetime_range(start_day, end_day, dp$interval, holiday$leapyear)]

    col_on <- c("year", "month", "day", "hour")

    # extract date time
    if (!has_names(epw_data, "line")) {
        set(epw_data, NULL, "line", seq_len(nrow(epw_data)))
        on.exit(set(epw_data, NULL, "line", NULL))
    }
    dt <- epw_data[, .SD, .SDcols = c("line", "datetime", col_on)]

    # find first match
    if (length(unique(realyear)) == 1L) {
        if (!realyear[[1L]]) col_on <- setdiff(col_on, "year")
        matched <- dt[data_period, on = col_on][, by = "index", .SD[1L]]
        if (!realyear[[1L]]) {
            set(matched, NULL, "year", NULL)
            setnames(matched, "i.year", "year")
        }
    } else {
        matched <- rbindlist(use.names = TRUE,
            lapply(split(data_period, by = "index"), function(dp) {
                on <- col_on
                if (!realyear[dp$index]) on <- setdiff(on, "year")
                m <- dt[dp, on = col_on, mult = "first"]
                if (!realyear[dp$index]) {
                    set(m, NULL, "year", NULL)
                    setnames(m, "i.year", "year")
                }
                m
            })
        )
    }

    # stop if first match was not found
    if (any(i <- is.na(matched$line))) {
        invld <- matched[i]
        set(invld, NULL, "string", do.call(combine_date, invld[, .SD, .SDcols = col_on]))
        set(invld, NULL, "line", seq_len(nrow(invld)))
        set(invld, NULL, "suffix", sprintf(" is missing for data period #%i '%s'", invld$index, invld$name))

        parse_error("epw", "Invalid WEATHER DATA", invld, subtype = "data", loc_name = "DateTime")
    }

    # check core weather data range on the row of first day, just as EnergyPlus does
    range <- get_epw_data_range(idd_env, "valid", unlist(EPW_REPORT_RANGE, FALSE, FALSE))
    line_range <- check_epw_data_range(epw_data[matched$line], range)
    if (length(line_range)) {
        # only show the first invalid for each data period
        m <- matched[J(line_range), on = "line", mult = "first", nomatch = NULL]
        invld <- epw_data[m, on = "line"]

        # get the first invalid variable
        invld[, by = "line", variable := {
            names(range)[which(!apply2_lgl(mget(names(range)), range, in_range))]
        }]

        # get field name
        nm <- get_idd_field(idd_env, EPW_CLASS$data, invld$variable, underscore = TRUE)$field_name
        set(invld, NULL, "field_name", nm)

        # construct message
        invld <- invld[, string := sprintf(
            "Line %i: First '%s' ('%s') is out of prescribed range %s for Data Period #%i '%s'.",
            line, field_name, get(variable), format(range[[variable]]), index, name)
        ]
        parse_error("epw", "Invalid WEATHER DATA", num = nrow(invld), subtype = "data",
            post = paste0(invld$string, collapse = "\n")
        )
    }

    # validate_datetime_range {{{
    validate_datetime_range <- function(dt, matched, realyear) {
        datetime <- dt$datetime
        if (length(datetime) - matched$line + 1L < matched$num) {
            parse_error("epw", paste("Invalid WEATHER DATA"), subtype = "data",
                post = sprintf("%i rows of weather data (starting from row %i) are expected for data period #%i '%s', but only '%i' were found.",
                    matched$num, matched$line, matched$index, matched$name, length(datetime) - matched$line + 1L
                )
            )
        }
        index <- seq(matched$line, length.out = matched$num)
        datetime <- datetime[index]

        if (any(i <- is.na(datetime))) return(which(i)[[1L]])

        lubridate::year(datetime) <- get_epw_datetime_year(matched$year, matched$start_day, matched$end_day, matched$num, matched$step)

        # handle sub-hourly
        if (matched$step != 60) {
            mins <- seq(matched$step, 60, matched$step)
            mins <- rep(mins, matched$num / length(mins))
            datetime <- datetime + lubridate::minutes(mins) - lubridate::hours(1L)
        }

        steps <- as.numeric(difftime(datetime[-1L], datetime[-matched$num], units = "mins"))

        # update corrected datetime in case sub-hourly data
        if (matched$step != 60) {
            set(dt, matched$line - 1L + index, "datetime", datetime)
            set(dt, matched$line - 1L + index,
                c("month", "day", "hour", "minute"),
                create_epw_datetime_components(
                    matched$start_day, matched$end_day,
                    60 / matched$step, tz, holiday$leapyear
                )[, -"year"]
            )
        }

        which(steps != matched$step)
    }
    # }}}

    # validate time step for each data period
    for (i in seq_len(nrow(matched))) {
        # NOTE: In case of sub-hourly data, datetime in dt will be updated in
        # this function
        invld <- validate_datetime_range(dt, matched[i], realyear)

        if (length(invld)) {
            # expected time
            dtime <- dt$datetime[invld[[1L]]] + lubridate::minutes(matched[i]$step)

            # first actual index
            invld <- invld[[1L]] + 1L
            invld <- epw_data[invld]

            set(invld, NULL, "string", paste(do.call(combine_date, invld[, .SD, .SDcols = col_on]), "..."))
            set(invld, NULL, "suffix", sprintf(" is found but date time '%s' is expected for data period #%i '%s'",
                format(dtime, "%m/%d %H:XX"), matched$index, matched$name
            ))
            parse_error("epw", paste("Invalid WEATHER DATA"), invld, suffix = invld$suffix, subtype = "data")
        }

        # if no error found and sub-hourly data found, update input epw_data
        # with correct datetime
        if (matched$step[i] != 60) {
            set(epw_data, NULL, c("datetime", "month", "day", "hour", "minute"),
                dt[, .SD, .SDcols = c("datetime", "month", "day", "hour", "minute")]
            )
        }
    }

    set(matched, NULL, setdiff(names(matched), c("index", "line", "num")), NULL)
    setnames(matched, "line", "row")
}
# }}}
# get_epw_datetime_range {{{
get_epw_datetime_range <- function(start, end, interval, leapyear = FALSE, realyear = FALSE) {
    if (is_epwdate(start)) start <- reset_epwdate_year(start, leapyear)
    if (is_epwdate(end)) end <- reset_epwdate_year(end, leapyear)

    start <- as_date(start)
    end <- as_date(end)

    step <- 60L / interval
    if (!test_integerish(step, len = 1L)) abort("Invalid interval")

    num <- as.numeric(difftime(end + lubridate::days(1L), start, units = "hours")) * interval

    list(
        year = lubridate::year(start),
        month = lubridate::month(start),
        day = lubridate::mday(start),
        hour = 1L,
        minute = if (interval == 1L) 0L else step,
        step = step,
        num = as.integer(num)
    )
}
# }}}
# get_epw_datetime_year {{{
get_epw_datetime_year <- function(start_year, start_day, end_day, num, step) {
    # update year value
    #  year value does not change
    if (lubridate::year(start_day) == lubridate::year(end_day)) {
        # need to change the year value for the last day
        if (format(as_date(end_day), "%m-%d") == "12-31") {
            c(rep(start_year, num - 60 / step), rep(start_year + 1L, 60 / step))
        } else {
            rep(start_year, num)
        }
    # if real year, it is possible that multiple years exist
    } else {
        lubridate::year(seq(
            as_datetime(start_day) + lubridate::minutes(step),
            as_datetime(end_day) + lubridate::hours(24L),
            by = paste(step, "mins")
        ))
    }
}
# }}}
# get_epw_data_range {{{
get_epw_data_range <- function(idd_env, type = c("valid", "exist"), field = NULL) {
    type <- match.arg(type)
    prop <- c("type_enum", "field_name_us")

    if (type == "valid") {
        prop <- c(prop, "has_range", "minimum", "lower_incbounds", "maximum", "upper_incbounds")
    } else {
        prop <- c(prop, "has_exist", "exist_minimum", "exist_lower_incbounds", "exist_maximum", "exist_upper_incbounds")
    }

    fld <- get_idd_field(idd_env, EPW_CLASS$data, field, prop, underscore = TRUE)

    if (type == "exist") {
        setnames(fld,
            c("has_exist", "exist_minimum", "exist_lower_incbounds", "exist_maximum", "exist_upper_incbounds"),
            c("has_range", "minimum", "lower_incbounds", "maximum", "upper_incbounds")
        )
    }

    # set limits to Inf for numeric values that do not have ranges
    fld[J(c(IDDFIELD_TYPE$integer, IDDFIELD_TYPE$real), FALSE), on = c("type_enum", "has_range"), `:=`(maximum = Inf, minimum = -Inf)]
    fld[J(TRUE, NA_real_), on = c("has_range", "maximum"), `:=`(maximum = Inf)]
    fld[J(TRUE, NA_real_), on = c("has_range", "minimum"), `:=`(minimum = -Inf)]
    fld[, `:=`(range = list(ranger(minimum, lower_incbounds, maximum, upper_incbounds))), by = field_id]

    range <- fld$range
    setattr(range, "names", fld$field_name_us)
    # exclude non-applicable
    range[!names(range) %chin% c("year", "month", "day", "hour", "minute", "data_source", "present_weather_codes")]
}
# }}}
# get_epw_data_missing_code {{{
get_epw_data_missing_code <- function(idd_env) {
    fld <- get_idd_field(idd_env, EPW_CLASS$data,
        property = c("missing_chr", "missing_num", "field_name_us", "type_enum"))[
        !J(NA_character_), on = "missing_chr"]

    setnames(fld, c("missing_chr", "missing_num"), c("value_chr", "value_num"))

    setattr(get_value_list(fld), "names", fld$field_name_us)
}
# }}}
# get_epw_data_init_value {{{
get_epw_data_init_value <- function(idd_env) {
    fld <- get_idd_field(idd_env, EPW_CLASS$data,
        property = c("default_chr", "default_num", "field_name_us", "type_enum"))[
        !J(NA_character_), on = "default_chr"]

    setnames(fld, c("default_chr", "default_num"), c("value_chr", "value_num"))
    setattr(get_value_list(fld), "names", fld$field_name_us)
}
# }}}
# get_epw_data_fill_action {{{
get_epw_data_fill_action <- function(type = c("missing", "out_of_range")) {
    type <- match.arg(type)
    if (type == "missing") {
        EPW_REPORT_MISSING
    } else {
        EPW_REPORT_RANGE
    }
}
# }}}
# get_epw_data_unit {{{
get_epw_data_unit <- function(idd_env, field = NULL) {
    fld <- get_idd_field(idd_env, EPW_CLASS$data, field, c("units", "field_name_us"), underscore = TRUE)[
        !J(NA_character_), on = "units"]

    setattr(as.list(fld$units), "names", fld$field_name_us)
}
# }}}
# get_epw_data_type {{{
get_epw_data_type <- function(idd_env, field = NULL) {
    fld <- get_idd_field(idd_env, EPW_CLASS$data, field,
        c("type", "field_name_us"), underscore = TRUE
    )

    fld[J("real"), on = "type", type := "double"]
    fld[J("alpha"), on = "type", type := "character"]
    setattr(as.list(fld$type), "names", fld$field_name_us)
}
# }}}
# check_epw_data_range{{{
check_epw_data_range <- function(epw_data, range, merge = TRUE) {
    m <- epw_data[, apply2(.SD, range, function(x, y) !is.na(x) & in_range(x, y)), .SDcols = names(range)]

    if (!merge) return(lapply(m, function(x) which(!x)))

    assert_names(names(epw_data), must.include = "line")
    m[, c(names(range)) := lapply(.SD, function(x) {x[x == FALSE] <- NA;x}), .SDcols = names(range)]
    set(m, NULL, "line", epw_data$line)

    # store abnormal variables. See #326
    abnormal <- na.omit(m, invert = TRUE)
    nm <- names(which(unlist(abnormal[, lapply(.SD, anyNA), .SDcols = -"line"])))
    setattr(abnormal$line, "variable", nm)
}
# }}}
# check_epw_data_type{{{
check_epw_data_type <- function(idd_env, epw_data, type = NULL) {
    if (is.null(type)) type <- unlist(get_epw_data_type(idd_env))
    assert_names(names(type))
    assert_data_table(epw_data)
    assert_names(names(epw_data), must.include = names(type))
    setcolorder(epw_data, names(type))

    type_detected <- epw_data[, vcapply(.SD, typeof)]
    for (j in seq_along(type)) {
        if (type[[j]] == "integer") {
            # handle integerish
            if (
                type_detected[[j]] == "integer" ||
                (
                    type_detected[[j]] == "double" && checkmate::test_integerish(epw_data[[j]])
                )
            ) {
                # remove all derived S3 class
                set(epw_data, NULL, j, as.integer(epw_data[[j]]))
            } else {
                parse_error("epw", "Failed to parse variables as integer", num = 1L,
                    post = paste0("Failed variables: ",
                        get_idd_field(idd_env, EPW_CLASS$data, names(type)[[j]], underscore = TRUE)$field_name),
                    subtype = "data_type"
                )
            }
        } else if (type[[j]] == "double") {
            # it is ok to coerce integer to double
            if (type_detected[[j]] %chin% c("integer", "double")) {
                # remove all derived S3 class
                set(epw_data, NULL, j, as.double(epw_data[[j]]))
            } else {
                parse_error("epw", "Failed to parse variables as double", num = 1L,
                    post = paste0("Failed variables: ",
                        get_idd_field(idd_env, EPW_CLASS$data, names(type)[[j]], underscore = TRUE)$field_name),
                    subtype = "data_type"
                )
            }
        } else {
            # remove all derived S3 class
            set(epw_data, NULL, j, as.character(epw_data[[j]]))
        }
    }

    epw_data
}
# }}}

# DATA
# get_epw_data {{{
#' @importFrom checkmate assert_flag assert_scalar assert_count
get_epw_data <- function(idd_env, epw_header, epw_data, matched, period = 1L, start_year = NULL,
                         align_wday = FALSE, tz = "UTC", update = FALSE) {
    assert_count(period)
    assert_count(start_year, null.ok = TRUE)
    assert_flag(align_wday)
    assert_scalar(tz)
    assert_flag(update)

    # get data periods
    dp <- parse_epw_header_period(idd_env, epw_header, TRUE)
    if (period > nrow(dp$period)) {
        abort(paste0("Invalid data period index found. EPW contains only ",
            nrow(dp$period), " data period(s) but ", surround(period), " is specified."
            ), "epw_data_period_index"
        )
    }
    interval <- dp$interval
    p <- dp$period[period]

    # leap year
    leapyear <- parse_epw_header_holiday(idd_env, epw_header, TRUE)$leapyear

    # get match info
    m <- matched[period]

    # get data
    i <- seq(matched$row[period], length.out = matched$num[period])
    d <- epw_data[i]

    can_update <- FALSE

    # check if real year
    realyear <- get_epwdate_type(p$start_day) == EPWDATE_TYPE$ymd

    datetime <- d$datetime
    year <- lubridate::year(datetime)
    # use the year column
    if (is.null(start_year)) {
        if (realyear) {
            mism <- NULL
            if (wday(p$start_day, TRUE) != p$start_day_of_week) {
                mism <- paste0(" Actual start day of week (", wday(p$start_day, TRUE), ") ",
                    "mismatches with the value specified in the header (", p$start_day_of_week, "). ",
                    "The later will be used."
                )
            }

            # issue a warning if trying to align day of week for a real year
            if (align_wday) {
                warn(paste0("Data period #", period, " ", surround(p$name),
                        " seems like a real-year data starting from ",
                        format(p$start_day), " to ",
                        format(p$end_day), ".", mism,
                        if (align_wday) " No day of week alignment is performed."
                    ),
                    "warning_rewrite_epw_acutal_year"
                )
            }
        # calculate new start year based on start day of week
        } else if (align_wday) {
            can_update <- TRUE

            # align start day of week
            start_year <- find_nearst_wday_year(p$start_day, p$start_day_of_week,
                lubridate::year(Sys.Date()), leapyear)
            year <- get_epw_datetime_year(start_year, p$start_day, p$end_day, m$num, 60 / interval)
            start_year <- year[1L]
            # If Feb 28 is from a leap year, the datetime here will fall on Feb
            # 29th instead of Mar 1st. Here reset the date to Mar 1st manually
            # to avoid invalid date if derived start year is a non-leap year
            # See #552
            if (!leapyear && length(is_feb28 <- which(d$month == 2L & d$day == 28L)) && mday(datetime[max(is_feb28)]) == 29L) {
                lubridate::month(datetime[max(is_feb28)]) <- 3L
                lubridate::mday(datetime[max(is_feb28)]) <- 1L
            }
            lubridate::year(datetime) <- year
        }

    } else {
        can_update <- TRUE

        if (leapyear != lubridate::leap_year(start_year)) {
            # warning if leap year status mismatches
            msg <- if (leapyear) {
                "The original starting date falls in a leap year, however input 'start_year' is not a leap year."
            } else {
                "The original start year is not a leap year, however input 'start_year' is."
            }
            warn(paste0("Invalid 'start_year' found for Data period #", period, " ", surround(p$name),
                    " starting from ", format(p$start_day), " to ", format(p$end_day), ". ",
                    msg, " Invalid date time may occur."
                )
            )
        }

        # if real year and start_year argument is given, issue an warning
        if (realyear) {
            s <- as_date(p$start_day)
            lubridate::year(s) <- start_year
            s <- epw_date(s)

            warn(paste0("Data period #", period, " ", surround(p$name),
                    " seems like a real-year data starting from ",
                    format(p$start_day), " to ",
                    format(p$end_day), ". ",
                    "The starting date will be overwriten as ",
                    format(s), "."
                )
            )

            e <- as_date(p$end_day)
            lubridate::year(e) <- lubridate::year(e) - (lubridate::year(as_date(p$start_day)) - start_year)
            set(p, NULL, "start_day", s)
            set(p, NULL, "end_day", e)
        }

        year <- get_epw_datetime_year(start_year, p$start_day, p$end_day, m$num, 60 / interval)
        lubridate::year(datetime) <- year
    }

    if (tz != lubridate::tz(datetime[[1L]])) {
        can_update <- TRUE
        datetime <- lubridate::force_tz(datetime, tz)
        start_year <- lubridate::year(datetime[[1L]])
    }

    # original data should not have any NA as this has been checked when
    # parsing. NA will be introduced in cases when input year is a leap year:
    # "2016-02-29" + lubridate::years(1)
    if (can_update && any(j <- is.na(datetime))) {
        j <- which(j)
        invld <- if (length(j) > 10L) {
            set(d[j[1:10]], NULL, "new_year", year[j[1:10]])
        } else {
            set(d[j], NULL, "new_year", year[j])
        }

        mes <- invld[, paste0("Original: ", datetime, " --> New year: ", new_year)]

        if (length(j) > 10L) mes <- c(mes, "...[truncated. First 10 are shown.]")

        abort(paste0("Invalid date introduced with input new start year (", start_year, ") and time zone (", tz, "):\n",
            paste0(mes, collapse = "\n")),
            "epw_data"
        )
    }

    set(d, NULL, "datetime", datetime)

    if (update && can_update) set(d, NULL, "year", year(d$datetime))

    # add line
    set(d, NULL, "line", i + 8L)
    setcolorder(d, "line")

    # for sub-hourly data, correct hour value from 1:24 to 0:23
    if (as.integer(interval) != 1L) set(d, NULL, "hour", d$hour - 1L)
    d
}
# }}}
# get_epw_data_abnormal {{{
#' @importFrom checkmate assert_count assert_flag
get_epw_data_abnormal <- function(idd_env, epw_header, epw_data, matched, period = 1L, cols = NULL,
                                  keep_all = TRUE, type = c("both", "missing", "out_of_range")) {
    assert_count(period)
    assert_flag(keep_all)
    assert_character(cols, null.ok = TRUE, any.missing = FALSE)
    type <- match.arg(type)

    d <- get_epw_data(idd_env, epw_header, epw_data, matched, period)
    set(d, NULL, "line", seq(matched[period]$row + 8L, length.out = matched[period]$num))

    if (type == "both") type <- c("missing", "out_of_range")

    ln <- locate_epw_data_abnormal(idd_env, d, cols, "missing" %chin% type, "out_of_range" %chin% type, merge = TRUE)
    # get abnormal variables. See #326
    if (is.null(cols)) {
        cols <- unique(c(attr(ln$missing, "variable"), attr(ln$out_of_range, "variable")))
        cols <- names(epw_data)[names(epw_data) %chin% cols]
    }
    ln <- sort(unique(c(ln$missing, ln$out_of_range)))

    if (!length(ln)) verbose_info("No abnormal data found.")

    if (keep_all) {
        d <- d[J(ln), on = "line"]
    } else {
        d <- d[J(ln), on = "line", .SD, .SDcols = c("line", "datetime", "year", "month", "day", "hour", "minute", cols)]
    }

    setcolorder(d, "line")
    d
}
# }}}
# get_epw_data_redundant {{{
get_epw_data_redundant <- function(idd_env, epw_header, epw_data, matched, line = FALSE, revert = FALSE) {
    add_rleid(epw_data)
    rleid <- matched[, list(rleid = seq(row, length.out = num)), by = "index"]$rleid

    line_redundant <- setdiff(epw_data$rleid, rleid)
    if (!length(line_redundant)) verbose_info("No redundant data found.")

    if (revert) line_redundant <- setdiff(epw_data$rleid, line_redundant)

    if (line) {
        set(epw_data, NULL, "rleid", NULL)
        return(line_redudant)
    }

    d <- epw_data[line_redundant]

    # clean
    set(epw_data, NULL, "rleid", NULL)

    set(d, NULL, "rleid", rleid + 8L)
    setnames(d, "rleid", "line")
    setcolorder(d, "line")
    d[]
}
# }}}
# locate_epw_data_abnormal {{{
# Logic directly derived from WeatherManager.cc in EnergyPlus source code
locate_epw_data_abnormal <- function(idd_env, epw_data, field = NULL, missing = FALSE, out_of_range = FALSE, merge = FALSE) {
    if (merge) {
        line_miss <- integer()
        line_range <- integer()
    } else {
        line_miss <- list()
        line_range <- list()
    }

    if (missing) {
        exist <- get_epw_data_range(idd_env, "exist", field = field)
        line_miss <- check_epw_data_range(epw_data, exist, merge = merge)
    }

    if (out_of_range) {
        valid <- get_epw_data_range(idd_env, "valid", field = field)
        line_range <- check_epw_data_range(epw_data, valid, merge = merge)
    }

    list(missing = line_miss, out_of_range = line_range)
}
# }}}
# match_epw_data_period {{{
match_epw_data_period <- function(matched, period = NULL) {
    if (is.null(period)) return(matched)

    assert_integerish(period, lower = 1L, any.missing = FALSE)
    if (period > nrow(matched)) {
        abort(paste0("Invalid data period index found. EPW contains only ",
            nrow(matched), " data period(s) but ", surround(period), " is specified."
            ), "epw_data_period_index"
        )
    }
    matched[period]
}
# }}}
# make_epw_data_na {{{
# Logic directly derived from WeatherManager.cc in EnergyPlus source code
make_epw_data_na <- function(idd_env, epw_header, epw_data, matched, period = NULL,
                             field = NULL, missing = FALSE, out_of_range = FALSE) {
    if (!missing && !out_of_range) return(epw_data)

    matched <- match_epw_data_period(matched, period)
    rleid <- matched[, list(rleid = seq(row, length.out = num)), by = "index"]$rleid
    d <- epw_data[rleid]

    line <- locate_epw_data_abnormal(idd_env, d, field, missing, out_of_range, merge = FALSE)
    cols <- if (missing) names(line$missing) else names(line$out_of_range)

    for (name in cols) {
        i <- c(line$missing[[name]], line$out_of_range[[name]])
        if (length(i)) set(epw_data, rleid[i], name, NA)
    }

    epw_data
}
# }}}
# fill_epw_data_abnormal {{{
fill_epw_data_abnormal <- function(idd_env, epw_header, epw_data, matched, period = NULL,
                                   field = NULL, missing = TRUE, out_of_range = TRUE,
                                   special = FALSE, miss_na = FALSE, range_na = FALSE) {
    if (!missing && !out_of_range) return(epw_data)

    # get data
    matched <- match_epw_data_period(matched, period)
    rleid <- matched[, list(rleid = seq(row, length.out = num)), by = "index"]$rleid
    d <- epw_data[rleid]

    # get missing code
    code <- get_epw_data_missing_code(idd_env)

    # get all abnormal row indices
    ln <- locate_epw_data_abnormal(idd_env, d, field, missing, out_of_range, merge = FALSE)

    if (!special) {
        for (name in names(code)) {
            i <- c(ln$missing[[name]], ln$out_of_range[[name]])
            if (length(i)) set(epw_data, rleid[i], name, code[[name]])
        }

        return(epw_data)
    }

    # get initial value for first missing value
    init <- get_epw_data_init_value(idd_env)

    # get atmospheric pressure at current elevation
    elev <- parse_epw_header_location(idd_env, epw_header, EPW_CLASS$location)$elevation
    if (is.na(elev)) {
        valid <- validate_epw_header_basic(idd_env, epw_header, EPW_CLASS$location, field = "Elevation")
        assert_valid(valid, epw = TRUE)
    }
    atpres <- std_atm_press(epw_header$location$elevation)

    m <- ln$missing
    r <- ln$out_of_range
    # just in case
    if (missing && out_of_range) assert_same_len(m, r)

    # add previous valid line index {{{
    if (missing) {
        for (nm in EPW_REPORT_MISSING$use_previous) {
            if (length(m[[nm]])) {
                if (length(r[[nm]])) {
                    comb <- sort(unique(c(m[[nm]], r[[nm]])))
                } else {
                    comb <- m[[nm]]
                }
                bound <- c(0L, diff(comb)) != 1L
                m[[nm]] <- sort(unique(c(m[[nm]], comb[bound] - 1L)))
            }
        }
    }

    if (out_of_range) {
        for (nm in EPW_REPORT_RANGE$use_previous) {
            if (length(r[[nm]])) {
                if (length(m[[nm]])) {
                    comb <- sort(unique(c(m[[nm]], r[[nm]])))
                } else {
                    comb <- r[[nm]]
                }
                bound <- c(0L, diff(comb)) != 1L
                r[[nm]] <- sort(unique(c(r[[nm]], comb[bound] - 1L)))
            }
        }
    }
    # }}}

    if (missing) {
        action <- get_epw_data_fill_action("missing")
        fill_epw_data_abnormal_special(d, m, action, init, code, miss_na)
    }
    if (out_of_range) {
        action <- get_epw_data_fill_action("out_of_range")
        fill_epw_data_abnormal_special(d, m, action, init, code, range_na)
    }

    set(epw_data, rleid, names(d), d)
    epw_data
}
# }}}
# fill_epw_data_abnormal_special {{{
fill_epw_data_abnormal_special <- function(epw_data, loc, action, init_value, code, na_made = FALSE) {
    # for each variable
    for (name in names(loc)) {
        if (!length(loc[[name]])) next

        # keep that column as it is if requested
        if (name %chin% action$do_nothing) {
            # <DO NOTHING>

        # set to 0 if applicable
        } else if (name %chin% action$use_zero) {
            set(epw_data, loc[[name]], name, 0)

        # set to previous value if applicable
        } else if (name %chin% action$use_previous) {
            l <- loc[[name]]

            # if there is no previous valid line, set the first
            # missing value to initial missing value
            if (l[1L] == 0L) {
                set(epw_data, l[2L], name, init_value[[name]])
                l <- l[-1L]
            }

            # already change missing to NAs
            if (na_made) {
                setnafill(epw_data, "locf", cols = name)

            # still is presented as missing code
            } else {
                epw_data[l, c(name) := get(name)[1L], by = list(cumsum(get(name) != code[[name]]))]
            }

        # for others set to missing code
        } else {
            set(epw_data, loc[[name]], name, code[[name]])
        }
    }

    epw_data
}
# }}}
# add_epw_data_unit {{{
add_epw_data_unit <- function(idd_env, epw_data) {
    unit <- get_epw_data_unit(idd_env)

    # change to standard SI units
    u <- FIELD_UNIT_TABLE[J(unlist(unit)), on = "si_name", mult = "first"][
        !is.na(si_standard_name), si_name := si_standard_name]$si_name
    unit <- setattr(as.list(u), "names", names(unit))

    for (nm in names(unit)) {
        set(epw_data, NULL, nm, units::set_units(epw_data[[nm]], unit[[nm]], mode = "standard"))
    }
    epw_data
}
# }}}
# drop_epw_data_unit {{{
drop_epw_data_unit <- function(idd_env, epw_data) {
    unit <- get_epw_data_unit(idd_env)
    for (nm in names(unit)) {
        if (inherits(epw_data[[nm]], "units")) {
            set(epw_data, NULL, nm, units::drop_units(epw_data[[nm]]))
        }
    }
    epw_data
}
# }}}
# purge_epw_data_redundant {{{
purge_epw_data_redundant <- function(epw_header, epw_data, matched) {
    add_rleid(epw_data)
    ln <- matched[, list(rleid = seq(row, length.out = num)), by = "index"]

    line_redundant <- setdiff(epw_data$rleid, ln$rleid)
    if (!length(line_redundant)) {
        verbose_info("No redundant data found. Nothing to purge.")

        # clean
        set(epw_data, NULL, "rleid", NULL)

        return(list(data = epw_data, matched = matched))
    }

    if (in_verbose()) {
        if (length(line_redundant) >= 10L) {
            msg <- paste0(paste0("#", line_redundant[1L:10L], collapse = ", "), " and etc.")
        } else {
            msg <- paste0("#", line_redundant)
        }
        verbose_info("Deleting lines ", msg, " that are not used in any data period.")
    }

    data <- epw_data[ln, on = "rleid", nomatch = NULL]
    data[, row := .I]
    matched <- data[, list(row = row[[1L]], num = .N), by = "index"]

    # clean
    set(epw_data, NULL, "rleid", NULL)
    set(data, NULL, c("index", "row", "rleid"), NULL)

    list(data = data, matched = matched)
}
# }}}
# add_epw_data {{{
#' @importFrom checkmate assert_data_frame assert_names assert_flag
add_epw_data <- function(idd_env, epw_header, epw_data, matched, data, realyear = FALSE,
                         name = NULL, start_day_of_week = NULL, after = 0L) {
    merge_epw_new_data(idd_env, epw_header, epw_data, matched, data, after,
        reset = FALSE, realyear = realyear, name = name,
        start_day_of_week = start_day_of_week
    )
}
# }}}
# set_epw_data {{{
#' @importFrom checkmate assert_data_frame assert_names assert_flag
#' @importFrom checkmate assert_string assert_count
set_epw_data <- function(idd_env, epw_header, epw_data, matched, data, realyear = FALSE,
                         name = NULL, start_day_of_week = NULL, period = 1L) {
    merge_epw_new_data(idd_env, epw_header, epw_data, matched, data, period,
        reset = TRUE, realyear = realyear, name = name,
        start_day_of_week = start_day_of_week
    )
}
# }}}
# del_epw_data {{{
#' @importFrom checkmate assert_count
del_epw_data <- function(idd_env, epw_header, epw_data, matched, period) {
    assert_count(period, positive = TRUE)
    dp <- parse_epw_header_period(idd_env, epw_header)
    m <- match_epw_data_period(matched, period)

    # check if this is the only data period.
    # If so, stop. Since it makes no sense to create an EPW without any data
    # in it.
    if (nrow(matched) == 1L) {
        abort(paste0("The EPW file contains only one data period named ",
            surround(dp$period$name[period]), ". It cannot be deleted ",
            "since each EPW file should contain at least one data period."
        ))
    }

    val <- get_idf_table(idd_env, epw_header, EPW_CLASS$period)
    prev <- (period - 1L) * 4L + 2L
    val[J(1L), on = "index", value := as.character(nrow(matched) - 1L)]
    val[index > prev, value := c(value[-(1:4)], rep(NA_character_, 4L))]

    lst <- expand_idf_dots_literal(idd_env, epw_header, val, .default = FALSE)
    epw_header <- set_idf_object(idd_env, epw_header, lst$object, lst$value, level = "final")

    epw_data <- epw_data[-seq(m$row, length.out = m$num)]
    matched <- matched[-period][, index := .I]

    verbose_info("Data period #", period, " ", surround(dp$period$name[period]),
        " has been successfully deleted from the EPW file.")

    list(data = data, header = epw_header, matched = matched, period = m$index)
}
# }}}
# merge_epw_new_data {{{
#' @importFrom checkmate assert_posixct
merge_epw_new_data <- function(idd_env, epw_header, epw_data, matched, data, target_period,
                               reset = FALSE, realyear = FALSE, name = NULL,
                               start_day_of_week = NULL) {
    # drop units
    epw_data <- drop_epw_data_unit(idd_env, epw_data)

    assert_data_frame(data)
    assert_names(names(data), must.include = setdiff(names(epw_data), c("year", "month", "day", "hour", "minute")))
    assert_flag(reset)
    assert_flag(realyear)

    # copy the original header in case error occurs
    header <- list()
    header$object <- copy(epw_header$object)
    header$value <- copy(epw_header$value)
    header$reference <- copy(epw_header$reference)

    holiday <- parse_epw_header_holiday(idd_env, header)
    period <- parse_epw_header_period(idd_env, header)

    # get current data period and other periods {{{
    if (reset) {
        p <- match_epw_data_period(period$period, target_period)
        p_other <- period$period[-target_period]
    } else {
        target_period <- assert_count(target_period, coerce = TRUE)
        n <- nrow(period$period)

        if (target_period > n) {
            target_period <- n + 1L
            p <- period$period[n][, index := index + 1L]
            p_other <- period$period
        } else if (target_period == 0L) {
            target_period <- 1L
            p <- period$period[1L]
            p_other <- period$period[, index := index + 1L]
        } else {
            p <- period$period[target_period][, index := index + 1L]
            p_other <- period$period[index > target_period, index := index + 1L]
        }
    }
    # }}}

    # get new name {{{
    if (is.null(name)) {
        if (reset) {
            name <- p$name
        } else {
            all_nm <- period$period$name
            num <- stri_match_first_regex(all_nm, "^data(\\d{0,})$", case_insensitive = TRUE)[, 2L]
            num <- num[!is.na(num)]
            if (!length(num)) {
                name <- "Data"
            } else {
                num[stri_isempty(num)] <- "0"
                name <- paste0("Data", max(as.integer(num)) + 1L)
            }
        }
    # make sure input new name is not the same as others
    } else {
        if (stri_trans_tolower(name) %in% stri_trans_tolower(p_other$name)) {
            abort(paste0("Input data period name cannot be the same as existing ones, i.e. ",
                collapse(p_other$name)
            ))
        }
        set(p, NULL, "name", name)
    }
    # }}}

    # coerce input data into a data.table
    data <- as.data.table(data)
    # remove additional columns
    if (length(cols_del <- setdiff(names(data), names(epw_data)))) {
        set(data, NULL, cols_del, NULL)
    }

    # check datetime column type first, then others
    assert_posixct(data$datetime, any.missing = FALSE)

    # change time zone of input datetime to "UTC"
    set(data, NULL, "datetime", force_tz(data$datetime, "UTC"))

    # check other column types
    type <- get_epw_data_type(idd_env, setdiff(names(epw_data), c("datetime", "year", "month", "day", "hour", "minute")))
    data <- check_epw_data_type(idd_env, data, unlist(type))

    # get start and end day
    # assume that datetime is sorted
    start <- data$datetime[1L]
    end <- data$datetime[nrow(data) - 1L]

    # get time step and interval using first two rows {{{
    step <- difftime(data$datetime[2L], start, units = "mins")
    if (60L %% as.numeric(step) != 0L){
        abort(paste0("Invalid number of records per hour in input data. The difference ",
            "between second and first datetime is ", format(step), ", leading to ",
            "non-integral number of records per hour."
        ))
    }
    step <- as.numeric(step)
    if (reset) {
        interval <- as.integer(60L / step)
    } else {
        interval <- period$interval
        if (interval != 60 / step) {
            abort(paste0("Invalid number of records per hour in input data. Value ",
                "calculated between second and first datetime is ",
                as.integer(60/step), " which is different from value ",
                interval, " in the EPW file."
            ))
        }
    }
    # }}}

    # check if the datetime is valid
    expect_start <- stringi::stri_datetime_create(
        lubridate::year(start), lubridate::month(start), lubridate::mday(start),
        # hour # minute
        0L,    step,
        tz = "UTC", lenient = TRUE
    )
    if (start != expect_start) {
        abort(paste0("Invalid starting date time found in input data. ",
            "Expecting ", surround(expect_start), " but ", surround(start), " was found."
        ))
    }

    # after cal interval, change to md format if not real year
    if (!realyear) {
        start <- ymd_to_md(epw_date(start))
        end <- ymd_to_md(epw_date(end))
    }
    # set start and and day
    set(p, NULL, c("start_day", "end_day"), list(epw_date(start), epw_date(end)))

    # get leap year {{{
    if (!reset) {
        leapyear <- holiday$leapyear
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
        id <- get_idf_value(idd_env, header, EPW_CLASS$holiday, field = 1L)$value_id
        header$value[J(id), on = "value_id", value_chr := ifelse(leapyear, "Yes", "No")]
    }
    # }}}

    # if AMY data given, use the day of week of the first day if not explicitly specified {{{
    if (is.null(start_day_of_week)) {
        if (realyear) {
            start_day_of_week <- as.character(wday(start, TRUE))
        } else {
            # if reset, use the original one
            if (reset) {
                start_day_of_week <- p$start_day_of_week
            } else {
                start_day_of_week <- "Sunday"
            }
        }
    } else {
        start_day_of_week <- get_epw_wday(start_day_of_week, TRUE)
    }
    # }}}

    # update period data for futher checking {{{
    lst <- list()
    lst[sprintf("Data Period %i Name/Description", p$index)] <- name
    lst[sprintf("Data Period %i Start Day of Week", p$index)] <- start_day_of_week
    lst[sprintf("Data Period %i Start Day", p$index)] <- format(epw_date(start))
    lst[sprintf("Data Period %i End Day", p$index)] <- format(epw_date(end))
    lst["Number of Records per Hour"] <- interval

    # shift other values
    if (!reset) {
        lst["Number of Data Periods"] <- n + 1L
        lst[sprintf("Data Period %i Name/Description", p_other$index)] <- p_other$name
        lst[sprintf("Data Period %i Start Day of Week", p_other$index)] <- p_other$start_day_of_week
        lst[sprintf("Data Period %i Start Day", p_other$index)] <- format(p_other$start_day)
        lst[sprintf("Data Period %i End Day", p_other$index)] <- format(p_other$end_day)
    }
    lst <- expand_idf_dots_value(idd_env, header,
        ..(EPW_CLASS$period) := lst, .default = FALSE, .type = "object"
    )
    header <- set_idf_object(idd_env, header, lst$object, lst$value, empty = TRUE, level = "final")
    parse_epw_header_period(idd_env, header)
    # }}}

    # match datetime {{{
    # preserve input year if not AMY, see #320
    if (!realyear) year_ori <- data$year

    set(data, NULL, c("year", "month", "day"),
        list(year = lubridate::year(data$datetime),
             month = lubridate::month(data$datetime),
             day = lubridate::mday(data$datetime)
        )
    )

    if (step == 60) {
        # construct hour values
        hour <- as.integer(lubridate::hour(data$datetime))
        hour[hour == 0L] <- 24L
        set(data, NULL, "hour", hour)
        set(data, NULL, "minute", 0L)
    } else {
        # construct minute values
        minute <- as.integer(lubridate::minute(data$datetime))
        minute[minute == 0L] <- 60L
        set(data, NULL, "hour", lubridate::hour(data$datetime))
        set(data, NULL, "minute", minute)
        data[J(60L), on = "minute", c("year", "month", "day", "hour") := {
            dtime <- datetime - lubridate::minutes(step)
            list(lubridate::year(dtime), lubridate::month(dtime), lubridate::mday(dtime), lubridate::hour(dtime))
        }]
        set(data, NULL, "hour", data$hour + 1L)

        # ignore minute when validation
        set(data, NULL, "datetime", stringi::stri_datetime_create(data$year, data$month, data$day, data$hour, tz = lubridate::tz(data$datetime), lenient = TRUE))
    }

    m <- match_epw_data(idd_env, header, data, target_period)

    # update datetime components
    set(data, NULL, c("year", "month", "day", "hour", "minute"),
        create_epw_datetime_components(start, end, interval, leapyear = leapyear)
    )
    if (!realyear) set(data, NULL, "year", year_ori)
    # }}}

    # set column order
    setcolorder(data, names(epw_data))

    # update table {{{
    # drop units
    data <- drop_epw_data_unit(idd_env, data)

    if (reset) {
        m_prev <- matched[index == target_period]
        data <- rbindlist(list(
            # before the first line of previous data to reset
            if (m_prev$row == 1L) data.table() else epw_data[1:(m_prev$row - 1L)],
            data,
            # after the last line of previous data to reset
            if (nrow(epw_data) == (m_prev$row + m_prev$num - 1L)) data.table() else epw_data[(m_prev$row + m_prev$num):.N]
        ))

        matched <- copy(matched)[J(p$index), on = "index", `:=`(row = m$row, num = m$num)]
    } else {
        # after 0L
        if (target_period == 1L) {
            data <- rbindlist(list(data, epw_data))
            matched <- rbindlist(list(m, copy(matched)[, `:=`(index = index + 1L, row = row + m$num)]))
        # after n
        } else if (target_period == (n + 1L)) {
            data <- rbindlist(list(epw_data, data))
            matched <- rbindlist(list(matched, m[, `:=`(index = target_period, row = matched[.N, row + num])]))
        } else {
            m_prev <- matched[index == target_period]
            data <- rbindlist(list(
                # before the last line of target period
                epw_data[1:(m_prev$row + m_prev$num - 1L)],
                data,
                # before the first line of next period
                epw_data[(m_prev$row + m_prev$num):.N]
            ))
            matched <- rbindlist(list(
                matched[index <= target_period],
                m[, `:=`(index = target_period + 1L, row = m_prev$row + m_prev$num)],
                matched[index > target_period][, `:=`(index = index + 1L, row = row + m$num)]
            ))
        }
    }
    # }}}

    list(data = data, header = header, matched = matched, period = p$index)
}
# }}}
# find_nearst_wday_year {{{
find_nearst_wday_year <- function(date, week_day, year = NULL, leap_year = FALSE) {
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
# create_epw_datetime_components {{{
create_epw_datetime_components <- function(start, end, interval, tz = "UTC", leapyear = FALSE) {
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

# FORMAT
# format_epw {{{
format_epw <- function(idd_env, epw_header, epw_data, fmt_digit = TRUE, fill = FALSE, purge = FALSE, ...) {
    list(
        header = format_epw_header(idd_env, epw_header),
        data = format_epw_data(idd_env, epw_header, epw_data, fmt_digit = TRUE, fill = FALSE, purge = FALSE, ...)
    )
}
# }}}
# format_epw_header {{{
format_epw_header <- function(idd_env, header) {
    val <- get_idf_value(idd_env, header, property = c("choice", "type", "extensible_group"))
    header$value <- standardize_idf_value(idd_env, header, val, type = "choice")

    # store original numeric values
    val_num <- val$value_num

    # format location header
    header$value[J(EPW_CLASS$location, c("Latitude", "Longitude")), on = c("class_name", "field_name"),
        value_chr := fmt_dbl(value_num)]
    header$value[J(EPW_CLASS$location, c("Time Zone", "Elevation")), on = c("class_name", "field_name"),
        value_chr := fmt_dbl(value_num, 1L)]

    # format design condition
    idx_int <- c(2L, 9L, 16L, 18L, 33L, 47L, 49L)
    header$value[class_name == EPW_CLASS$design & extensible_group > 0L, by = "extensible_group",
        value_chr := {
            value_chr[!is.na(value_num)] <- round(value_num[!is.na(value_num)], 1L)
            value_chr[idx_int] <- as.character(value_num[idx_int])
            value_chr
        }
    ]

    # format typical periods
    header$value[class_name == EPW_CLASS$typical & extensible_group > 0L, by = "extensible_group",
        value_chr := {
            value_chr[c(3L, 4L)] <- format(epw_date(value_chr[c(3L, 4L)]), m_spc = FALSE)
            value_chr
        }
    ]

    # format ground temp
    header$value[class_name == EPW_CLASS$ground & extensible_group > 0L & !is.na(value_num), by = "extensible_group",
        value_chr := {
            num <- round(value_num, 2L)
            idx <- c(5L:16L)[!is.na(num[5L:16L])]
            value_chr[idx] <- fmt_dbl(num[idx])
            idx <- c(1L:4L)[!is.na(num[1L:4L])]
            value_chr[idx] <- as.character(num[idx])
            value_chr
        }
    ]

    # format data period
    header$value[class_name == EPW_CLASS$period & extensible_group > 0L, by = "extensible_group",
        value_chr := {
            value_chr[c(3L, 4L)] <- format(epw_date(value_chr[c(3L, 4L)]), m_spc = TRUE)
            value_chr
        }
    ]

    set(header$value, NULL, "value_num", NULL)

    fmt <- get_idf_string(idd_env, header, header = FALSE, comment = FALSE,
        format = "new_top", leading = 0, sep_at = -1, flat = FALSE
    )
    fmt <- lapply(fmt$format$fmt, "[[", 2L)

    # assign numeric value back
    set(header$value, NULL, "value_num", val_num)
    cols <- c("value_id", "value_chr", "value_num", "object_id", "field_id")
    set(header$value, NULL, setdiff(names(header$value), cols), NULL)
    setcolorder(header$value, cols)

    vcapply(fmt, function(s) {
        # remove trailing semicolon
        s[length(s)] <- stri_sub(s[length(s)], to = -2L)

        paste0(s, collapse = "")
    })
}
# }}}
# format_epw_data {{{
format_epw_data <- function(idd_env, epw_header, epw_data, fmt_digit = FALSE, fill = FALSE, purge = FALSE, ...) {
    if (purge) epw_data <- purge_epw_data_redundant(epw_header, epw_data, matched)

    d <- epw_data[, -"datetime"]

    if (fill) d <- fill_epw_data_abnormal(idd_env, d, epw_header, matched, ...)

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
        aerosol_optical_depth = function(x) fmt_dbl(x, 4L),
        snow_depth = as.integer,
        albedo = function(x) fmt_dbl(x, 3L),
        liquid_precip_depth = fmt_int,
        liquid_precip_rate = fmt_int
    )
    # }}}

    # round digits as WeatherConvertor
    if (fmt_digit) {
        for (nm in names(EPW_FORMAT)) {
            set(d, NULL, nm, EPW_FORMAT[[nm]](d[[nm]]))
        }
    }

    d
}
# }}}
# format_epw_meta {{{
format_epw_meta <- function(idd_env, header) {
    loc <- get_idf_value(idd_env, header, EPW_CLASS$location, property = c("field_name_us", "type_enum"))
    loc <- setattr(get_value_list(loc), "names", loc$field_name_us)
    leapyear <- get_idf_value(idd_env, header, EPW_CLASS$holiday, field = 1L)$value_chr
    interval <- get_idf_value(idd_env, header, EPW_CLASS$period, field = 2L)$value_num

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

    # format location
    c(
        sprintf("[Location ]: %s, %s, %s", loc$city, loc$state_province, loc$country),
        # format time zone into UTC offset
        {
            tz <- loc$time_zone
            h <- abs(trunc(tz))
            m <- round((abs(tz) - h) * 60)
            sprintf("             {%s}, {%s}, {UTC%s}",
                lat_lon(loc$latitude),
                lat_lon(loc$longitude, TRUE),
                paste0(if (tz >= 0) "+" else "-", lpad(h, "0", 2L), ":", lpad(m, "0", 2L))
            )
        },
        sprintf("[Elevation]: %.fm %s see level", abs(loc$elevation), if (loc$elevation >= 0) "above" else "below"),
        sprintf("[Data Src ]: %s", loc$data_source),
        sprintf("[WMO Stat ]: %s", loc$wmo_number),
        sprintf("[Leap Year]: %s", if (is.na(leapyear)) "Unknown" else if (tolower(leapyear) == "yes") "Yes" else "No"),
        sprintf("[Interval ]: %s mins", 60 / interval)
    )
}
# }}}

# SAVE
# save_epw_file {{{
save_epw_file <- function(idd_env, epw_header, epw_data, matched, path, overwrite = FALSE,
                          fmt_digit = TRUE, fill = FALSE, purge = FALSE, ...) {
    if (!file.exists(path)) {
        new_file <- TRUE
    } else {
        new_file <- FALSE
        if (!overwrite) {
            abort("Target EPW file already exists. Please set 'overwrite' to 'TRUE' if you want to replace it.")
        }
    }

    l <- format_epw(idd_env, epw_header, epw_data, fmt_digit = fmt_digit, fill = fill, purge = FALSE, ...)
    write_lines(l$header, path)
    fwrite(l$data, path, append = TRUE)

    if (!new_file && overwrite) {
        verbose_info("Replace the existing EPW file located at ", normalizePath(path), ".")
    }

    normalizePath(path)
}
# }}}

# vim: set fdm=marker:
