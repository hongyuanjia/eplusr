context("Epw Class")

eplusr_option(verbose_info = FALSE)

# IDD {{{
test_that("IDD", {
    expect_is(idd <- EpwIdd$new(system.file("extdata/epw.idd", package = "eplusr")), "Idd")
    expect_output(idd$print())
})
# }}}

# META {{{
test_that("Meta info", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    expect_is(epw <- read_epw(path_epw), "Epw")

    expect_is(epw <- Epw$new(path_epw), "Epw")

    # can update the path after saved
    expect_equal(epw$path(), normalizePath(path_epw))

    # can get definition
    expect_is(epw$definition("LOCATION"), "IddObject")
})
# }}}

# HEADER {{{
test_that("Header getter and setter", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    expect_silent(epw <- Epw$new(path_epw))

    # $location() {{{
    expect_equal(
        epw$location(city = "Chongqing", state_province = "Chongqing", country = "China",
            data_source = "TMY", wmo_number = "724944", latitude = 20.0,
            longitude = -120.0, time_zone = 8L, elevation = 100
        ),
        list(city = "Chongqing",
             state_province = "Chongqing",
             country = "China",
             data_source = "TMY",
             wmo_number = "724944",
             latitude = 20.0,
             longitude = -120.0,
             time_zone = 8L,
             elevation = 100
        )
    )
    expect_equal(epw$location(city = "chongqing")$city, "chongqing")
    expect_error(epw$location(city = 1))
    # }}}

    # $design_condition() {{{
    expect_is(epw$design_condition(), "list")
    expect_equal(names(epw$design_condition()), c("source", "heating", "cooling", "extremes"))
    # }}}

    # $typical_extreme_period() {{{
    expect_is(epw$typical_extreme_period(), "data.table")
    expect_equal(names(epw$typical_extreme_period()), c("index", "name", "type", "start_day", "end_day"))
    expect_equal(nrow(epw$typical_extreme_period()), 6L)
    # }}}

    # $ground_temperature {{{
    expect_is(epw$ground_temperature(), "data.table")
    expect_equal(names(epw$ground_temperature()), c(
        "index",              "depth",
        "soil_conductivity",  "soil_density",  "soil_specific_heat",
        "January",            "February",      "March",
        "April",              "May",           "June",
        "July",               "August",        "September",
        "October",            "November",      "December"
    ))
    expect_equal(nrow(epw$ground_temperature()), 3L)
    # }}}

    # $ground_temperature {{{
    expect_is(epw$ground_temperature(), "data.table")
    expect_equal(names(epw$ground_temperature()), c(
        "index",              "depth",
        "soil_conductivity",  "soil_density",  "soil_specific_heat",
        "January",            "February",      "March",
        "April",              "May",           "June",
        "July",               "August",        "September",
        "October",            "November",      "December"
    ))
    expect_equal(nrow(epw$ground_temperature()), 3L)
    # }}}

    # $holiday {{{
    expect_silent(epw <- Epw$new(path_epw))
    expect_is(epw$holiday(), "list")
    expect_equal(names(epw$holiday()), c("leapyear", "dst", "holiday"))

    # leapyear
    expect_is(epw <- Epw$new(path_epw), "Epw")
    expect_equal(epw$holiday()$leapyear, FALSE)
    expect_error(epw$holiday(TRUE), class = "eplusr_error_epw_header")

    # change to leapyear
    expect_is(epw <- Epw$new(path_epw), "Epw")
    expect_warning(d <- epw$data(1, start_year = 2016, align_wday = FALSE))
    feb29 <- d[month == 2 & day == 28][, day := 29L]
    d <- rbindlist(list(d, feb29))[order(month, day)]
    get_priv_env(epw)$idf_env()$value[object_id == 5, value_chr := {value_chr[1] <- "Yes";value_chr}]
    epw$.__enclos_env__$private$m_data <- d
    expect_true(epw$holiday()$leapyear)
    expect_error(epw$holiday(FALSE))

    # dst
    expect_is(epw <- Epw$new(path_epw), "Epw")
    expect_equal(epw$holiday(dst = c(1, 2))$dst, epw_date(1:2))
    expect_equal(epw$holiday(dst = c(as.Date("2008-01-01"), as.Date("2008-02-01")))$dst, epw_date(c("Jan 01", "Feb 01")))

    # holiday
    expect_is(epw <- Epw$new(path_epw), "Epw")
    expect_is(hol <- epw$holiday(holiday = list(name = "New Year", day = "Jan 01")), "list")
    expect_equal(hol$holiday,
        data.table(index = 1L, name = "New Year", day = epw_date("1/1"))
    )

    # can restore the original data
    expect_error(epw$holiday(holiday = list(name = "New Year", day = "Jan 41")))
    expect_is(hol <- epw$holiday(), "list")
    expect_equal(hol$holiday,
        data.table(index = 1L, name = "New Year", day = epw_date("1/1"))
    )
    # }}}

    # $comment() {{{
    expect_is(epw$comment1(), "character")
    expect_equal(epw$comment1("comment1"), "comment1")
    expect_equal(epw$comment1(), "comment1")
    expect_is(epw$comment2(), "character")
    expect_equal(epw$comment2("comment2"), "comment2")
    expect_equal(epw$comment2(), "comment2")
    expect_null(epw$comment2(""))
    expect_null(epw$comment2())
    expect_null(epw$comment1(NULL))
    expect_null(epw$comment1())
    # }}}

    # $num_period {{{
    expect_equal(epw$num_period(), 1L)
    # }}}

    # $interval {{{
    expect_equal(epw$interval(), 1L)
    # }}}

    # $period {{{
    expect_is(epw$period(), "data.table")
    expect_is(epw$period(1), "data.table")
    expect_error(epw$period(2), class = "eplusr_error_epw_data_period_index")
    expect_equal(epw$period(1, name = "test")$name, "test")
    expect_error(epw$period(1, start_day_of_week = "test"), class = "eplusr_error_validity_check")
    expect_equal(epw$period(1, start_day_of_week = 3)$start_day_of_week, "Wednesday")
    expect_equal(epw$period(1, start_day_of_week = "Wed")$start_day_of_week, "Wednesday")

    expect_error(epw$period(1, start_day_of_week = "NoDay"))
    expect_equal(epw$period(1)$start_day_of_week, "Wednesday")
    # }}}
})
# }}}

# CONSTANTS {{{
test_that("Constant data", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    expect_silent(epw <- Epw$new(path_epw))

    expect_is(epw$missing_code(), "list")
    expect_equal(length(epw$missing_code()), 29L)
    expect_is(epw$initial_missing_value(), "list")
    expect_equal(length(epw$initial_missing_value()), 14L)
    expect_is(epw$range_exist(), "list")
    expect_equal(length(epw$range_exist()), 28L)
    expect_is(epw$range_valid(), "list")
    expect_equal(length(epw$range_valid()), 28L)
    expect_is(epw$fill_action(), "list")
    expect_equal(names(epw$fill_action()), c("use_previous", "use_zero"))
})
# }}}

# SAVE {{{
test_that("$save() & $is_unsaved()", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    expect_is(epw <- Epw$new(path_epw), "Epw")
    expect_is(d_ori <- epw$data(), "data.table")

    # $is_unsaved() {{{
    expect_false(epw$is_unsaved())
    # }}}

    # $save() {{{
    expect_error(epw$save(".idf"))
    unlink(file.path(tempdir(), "test_save.epw"), force = TRUE)
    expect_is(epw$save(file.path(tempdir(), "test_save.epw")), "character")
    expect_error(epw$save(file.path(tempdir(), "test_save.epw")), class = "eplusr_error")
    expect_is(epw$save(overwrite = TRUE), "character")
    expect_is(epw1 <- Epw$new(file.path(tempdir(), "test_save.epw")), "Epw")
    expect_equal(epw1$data(), d_ori)
    # }}}
})
# }}}

# DATA GETTER {{{
test_that("Data Getter", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    expect_is(epw <- Epw$new(path_epw), "Epw")

    # $data() {{{
    # can get weather data
    expect_error(epw$data(2), class = "eplusr_error_epw_data_period_index")
    expect_equal(ncol(epw$data()), 36L)
    expect_equal(nrow(epw$data()), 8760L)

    # can use the origial datetime
    expect_equal(year(epw$data(align_wday = FALSE)$datetime[8760]), 1998)

    # can change year in datetime column
    expect_equal(
        epw$data(start_year = 2018, tz = "GMT")$datetime,
        seq(as.POSIXct("2018-01-01 01:00:00", tz = "GMT"),
            as.POSIXct("2019-01-01 00:00:00", tz = "GMT"),
            by = "1 hour"
        )
    )
    # can change the year column
    expect_equal(epw$data(start_year = 2018, update = TRUE)$year, c(rep(2018L, times = 8759), 2019L))

    # can detect if leap year mismatch found
    expect_warning(epw$data(start_year = 2016))
    expect_warning(epw$data(start_year = 2016, align_wday = FALSE))

    # can change the time zone of datetime column in the returned weather data
    expect_error(attr(epw$data(tz = "America/Chicago")$datetime, "tzone"), class = "eplusr_error_epw_data")
    expect_equal(attr(epw$data(start_year = 2019, tz = "Etc/GMT+8")$datetime, "tzone"), "Etc/GMT+8")
    # }}}

    # $abnormal_data() {{{
    expect_equal(nrow(epw$abnormal_data()), 8760)
    expect_equal(nrow(epw$abnormal_data(type = "missing")), 8760)
    expect_equal(nrow(epw$abnormal_data(type = "out_of_range")), 0L)
    expect_true("line" %in% names(epw$abnormal_data()))
    expect_equal(ncol(epw$abnormal_data()), 37L)
    expect_equal(ncol(epw$abnormal_data(keep_all = FALSE)), 12L)
    expect_equal(nrow(epw$abnormal_data(cols = "albedo")), 2160L)
    expect_equal(ncol(epw$abnormal_data(cols = "albedo", keep_all = FALSE)), 8L)
    expect_equal(nrow(epw$abnormal_data(cols = "albedo", type = "out_of_range")), 0L)
    # }}}

    # $redudant_data() {{{
    expect_equal(nrow(epw$redundant_data()), 0L)
    # }}}
})
# }}}

# DATA TAGGER {{{
test_that("Data Tagger", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    expect_is(epw <- Epw$new(path_epw), "Epw")

    # $make_na() {{{
    expect_true({
        epw$make_na(missing = TRUE, out_of_range = TRUE)
        all(is.na(epw$abnormal_data(cols = "albedo", keep_all = FALSE, type = "missing")$albedo))
    })
    expect_message(with_verbose(epw$make_na(missing = TRUE, out_of_range = TRUE)), "already")
    # }}}

    # $fill_abnormal() {{{
    expect_equal(
        {
            epw$fill_abnormal(missing = TRUE, out_of_range = TRUE, special = TRUE)
            epw$abnormal_data(cols = "albedo", keep_all = FALSE, type = "missing")$albedo
        }, rep(999, 2160)
    )
    expect_message(with_verbose(epw$fill_abnormal(missing = TRUE, out_of_range = TRUE)), "already")
    # }}}

    # $add_unit() & $drop_unit() {{{
    expect_is(class = "units",
        {
            epw$add_unit()
            rad <- epw$data()$direct_normal_radiation
        }
    )
    expect_true(all(units(rad)$numerator %in% c("W", "h")))
    expect_equal(units(rad)$denominator, c("m", "m"))
    expect_message(with_verbose(epw$add_unit()), "already")

    expect_is(epw$drop_unit()$data()$dry_bulb_temperature, "numeric")
    expect_message(with_verbose(epw$drop_unit()), "already")
    # }}}

    # $purge() {{{
    expect_is(epw <- Epw$new(path_epw), "Epw")
    expect_is(epw$purge(), "Epw")
    epw$.__enclos_env__$private$m_data <- rbindlist(list(get_priv_env(epw)$m_data, epw$data()))
    expect_message(with_verbose(epw$purge()))
    # }}}
})
# }}}

# DATA SETTER {{{
test_that("Data Setter", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    expect_is(epw <- read_epw(path_epw), "Epw")

    # $set() {{{
    expect_is(d <- epw$data(), "data.table")
    expect_output(with_verbose(epw$set(d, realyear = TRUE)))
    expect_equal(epw$period(),
        data.table(index = 1L, name = "Data", start_day_of_week = "Sunday",
            start_day = epw_date("2017/1/1"), end_day = epw_date("2017/12/31")
        )
    )

    expect_warning(epw$set(d, realyear = TRUE, start_day_of_week = "Monday"))
    expect_warning(epw$set(d, realyear = TRUE))
    expect_equal(epw$period()$start_day_of_week, "Sunday")

    expect_is(epw$set(d[1:48]), "Epw")
    expect_equal(epw$period(),
        data.table(index = 1L, name = "Data", start_day_of_week = "Sunday",
            start_day = epw_date("1/1", F), end_day = epw_date("1/2", F)
        )
    )
    expect_equal(nrow(epw$data()), 48L)

    # can remove extra columns
    set(d, NULL, "extra_column", 1)
    expect_is(epw$set(d), "Epw")
    expect_equal(ncol(epw$data()), 36)

    expect_error({
        epw <- read_epw(path_epw)
        suppressWarnings(d <- epw$data(start_year = 2020))
        epw$set(d, TRUE)
    })

    expect_error({
        epw <- read_epw(path_epw)
        suppressWarnings(d <- epw$data(start_year = 2020))
        d[100L, datetime := lubridate::as_datetime("2020-01-01 00:00:00")]
        epw$set(d)
    })
    # }}}

    # $add() {{{
    expect_is(epw <- Epw$new(path_epw), "Epw")

    expect_error(epw$add(epw$data()), class = "eplusr_error_parse_epw")

    # after 0L
    expect_output(with_verbose(epw$add(epw$data(start_year = 2017), realyear = TRUE)))
    expect_equal(epw$period()$name, c("Data1", "Data"))
    expect_equal(lubridate::year(epw$data(1, align_wday = FALSE)$datetime[1]), 2017)
    expect_equal(get_priv_env(epw)$m_log$matched,
        data.table(index = 1:2, row = c(1L, 8761L), num = rep(8760L, 2))
    )

    # after N
    expect_warning(d <- epw$data(start_year = 2014, align_wday = FALSE))
    expect_is(epw$add(d, after = 10, realyear = TRUE), "Epw")
    expect_equal(epw$period()$name, c("Data1", "Data", "Data2"))
    expect_equal(lubridate::year(epw$data(3, align_wday = FALSE)$datetime[1]), 2014)
    expect_equal(get_priv_env(epw)$m_log$matched,
        data.table(index = 1:3, row = c(1L, 8761L, 17521L), num = rep(8760L, 3))
    )

    # between
    expect_warning(d <- epw$data(1, start_year = 2013, align_wday = FALSE))
    expect_is(epw$add(d, after = 2, realyear = TRUE), "Epw")
    expect_equal(lubridate::year(epw$data(3, align_wday = FALSE)$datetime[1]), 2013)
    expect_equal(get_priv_env(epw)$m_log$matched,
        data.table(index = 1:4, row = c(1L, 8761L, 17521L, 26281L), num = rep(8760L, 4))
    )

    # unit + no unit
    expect_is(epw <- Epw$new(path_epw), "Epw")
    expect_is(d <- epw$data(start_year = 2017), "data.table")
    expect_is(epw$add_unit(), "Epw")
    expect_is(epw$add(d, realyear = TRUE), "Epw")
    expect_warning(u <- units(epw$data()$liquid_precip_rate)$numerator)
    expect_equal(u, "h")

    # unit + unit
    expect_is(epw <- Epw$new(path_epw), "Epw")
    expect_is(epw$add_unit(), "Epw")
    expect_is(d <- epw$data(start_year = 2017), "data.table")
    expect_is(epw$add(d, realyear = TRUE), "Epw")
    expect_warning(u <- units(epw$data()$liquid_precip_rate)$numerator)
    expect_equal(u, "h")

    # no unit + unit
    expect_is(epw <- Epw$new(path_epw), "Epw")
    expect_is(epw$add_unit(), "Epw")
    expect_is(d <- epw$data(start_year = 2017), "data.table")
    expect_is(epw$drop_unit(), "Epw")
    expect_is(epw$add(d, realyear = TRUE), "Epw")
    expect_warning(u <- epw$data()$liquid_precip_rate)
    expect_is(u, "numeric")

    # no na + na
    expect_is(epw <- Epw$new(path_epw), "Epw")
    expect_is(epw$make_na(TRUE, TRUE), "Epw")
    expect_is(d <- epw$data(start_year = 2017), "data.table")
    expect_is(epw$fill_abnormal(TRUE, TRUE), "Epw")
    expect_is(epw$add(d, realyear = TRUE), "Epw")
    expect_true(all(!is.na(epw$abnormal_data(cols = "albedo", keep_all = FALSE, type = "missing")$albedo)))

    # na + no na
    expect_is(epw <- Epw$new(path_epw), "Epw")
    expect_is(d <- epw$data(start_year = 2017), "data.table")
    expect_is(epw$make_na(TRUE, TRUE), "Epw")
    expect_is(epw$add(d, realyear = TRUE), "Epw")
    expect_true(all(is.na(epw$abnormal_data(cols = "albedo", keep_all = FALSE, type = "missing")$albedo)))

    # can remove extra columns
    expect_is(epw <- Epw$new(path_epw), "Epw")
    expect_is(d <- epw$data(start_year = 2017), "data.table")
    set(d, NULL, "extra_column", 1)
    expect_is(epw$add(d, realyear = TRUE), "Epw")
    expect_warning(d <- epw$data())
    expect_equal(ncol(d), 36)
    # }}}

    # $del() {{{
    expect_is(epw <- Epw$new(path_epw), "Epw")

    expect_error(epw$del())
    expect_error(epw$del(1))

    expect_is(epw$add(epw$data(start_year = 2017), realyear = TRUE), "Epw")
    expect_message(with_verbose(epw$del(1)))
    # }}}
})
# }}}

# CLONE {{{
test_that("$clone()", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")
    expect_is(epw1 <- Epw$new(path_epw), "Epw")

    epw2 <- epw1$clone()
    epw2$period(1, name = "Data2")
    expect_equal(epw1$period()$name, "Data")
    expect_equal(epw2$period()$name, "Data2")
})
# }}}

# PRINT {{{
test_that("$print()", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    expect_silent(epw <- Epw$new(path_epw))

    # $print() {{{
    expect_output(epw$print())
    # }}}
})
# }}}

# S3 FORMAT {{{
test_that("str.Epw & format.Epw", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    expect_silent(epw <- Epw$new(path_epw))
    expect_output(str(epw))
    expect_is(format(epw), "character")
})
# }}}

# S3 EQUALITY {{{
test_that("==.Epw & !=.Epw", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    expect_silent(epw <- Epw$new(path_epw))

    expect_true(epw == epw)
    expect_false(epw == Epw$new(path_epw))
    expect_false(epw == 1)
    expect_false(epw != epw)
    expect_true(epw != Epw$new(path_epw))
})
# }}}

# DOWNLOAD_WEATHER {{{
test_that("download_weather()", {
    skip_on_cran()

    # download weather
    expect_message({path_epw <- with_verbose(
        download_weather("USA_CA_San.Francisco.Intl.AP.724940_TMY3", ask = FALSE, type = "epw", dir = tempdir()))}
    )
    expect_message({path_epw <- with_verbose(
        download_weather("USA_CA_San.Francisco.Intl.AP.724940_TMY3", ask = FALSE, type = "all", dir = tempdir()))}
    )
    expect_message({path_epw <- with_verbose(
        download_weather("USA_CA_San.Francisco.Intl.AP.724940_TMY3", ask = FALSE, type = "ddy", dir = tempdir()))}
    )
})
# }}}
