test_that("Epw class", {
    # clean temp dir
    clean_tempdir()
    eplusr_option(verbose_info = FALSE)

    skip_if_not(is_avail_eplus(8.8))
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    expect_silent(epw <- read_epw(path_epw))

    # can save the file
    expect_silent(epw$save(file.path(tempdir(), "weather.epw")))

    # can update the path after saved
    expect_equal(epw$path(), file.path(tempdir(), "weather.epw"))

    # can read local EPW file
    expect_silent(epw <- read_epw(file.path(tempdir(), "weather.epw")))

    # can get basic info
    expect_warning(city <- epw$city, "deprecated")
    expect_warning(state_province <- epw$state_province, "deprecated")
    expect_warning(country <- epw$country, "deprecated")
    expect_warning(data_source <- epw$data_source, "deprecated")
    expect_warning(wmo_number <- epw$wmo_number, "deprecated")
    expect_warning(latitude <- epw$latitude, "deprecated")
    expect_warning(longitude <- epw$longitude, "deprecated")
    expect_warning(time_zone <- epw$time_zone, "deprecated")
    expect_warning(elevation <- epw$elevation, "deprecated")
    expect_warning(time_step <- epw$time_step, "deprecated")
    expect_warning(start_day_of_week <- epw$start_day_of_week, "deprecated")
    expect_equal(city, "San Francisco Intl Ap")
    expect_equal(state_province, "CA")
    expect_equal(country, "USA")
    expect_equal(data_source, "TMY3")
    expect_equal(wmo_number, "724940")
    expect_equal(latitude, 37.62)
    expect_equal(longitude, -122.4)
    expect_equal(time_zone, -8)
    expect_equal(elevation, 2)
    expect_equal(time_step, 1)
    expect_equal(start_day_of_week, "Sunday")

    # can set basic info
    expect_warning({epw$city <- "Chongqing"; city <- epw$city}, "deprecated")
    expect_warning({epw$state_province <- "Chongqing"; state_province <- epw$state_province}, "deprecated")
    expect_warning({epw$country <- "China"; country <- epw$country}, "deprecated")
    expect_warning({epw$data_source <- "TMY"; data_source <- epw$data_source}, "deprecated")
    expect_warning({epw$wmo_number <- "724944"; wmo_number <- epw$wmo_number}, "deprecated")
    expect_warning({epw$latitude <- 20.0; latitude <- epw$latitude}, "deprecated")
    expect_warning({epw$longitude <- -120.0; longitude <- epw$longitude}, "deprecated")
    expect_warning({epw$time_zone <- 8; time_zone <- epw$time_zone}, "deprecated")
    expect_warning({epw$elevation <- 100; elevation <- epw$elevation}, "deprecated")
    expect_error({epw$time_step <- 2}, class = "error_eplusr_deprecated_time_step")
    expect_warning({epw$start_day_of_week <- "Monday"; start_day_of_week <- epw$start_day_of_week}, "deprecated")
    expect_equal(city, "Chongqing")
    expect_equal(state_province, "Chongqing")
    expect_equal(country, "China")
    expect_equal(data_source, "TMY")
    expect_equal(wmo_number, "724944")
    expect_equal(latitude, 20.0)
    expect_equal(longitude, -120.0)
    expect_equal(time_zone, 8)
    expect_equal(elevation, 100)
    expect_equal(start_day_of_week, "Monday")

    expect_equal(epw$location(),
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
    expect_error(epw$location(city = 1), class = "error_invalid_epw_header_location_type")

    expect_is(epw$design_condition(), "list")
    expect_equal(names(epw$design_condition()), c("source", "heating", "cooling", "extremes"))
    epw$typical_extreme_period()
    epw$ground_temperature()
    expect_is(epw$holiday(), "list")
    expect_error(epw$holiday(TRUE), class = "error_invalid_epw_header_leapyear")
    expect_equal(epw$holiday(dst = c(1, 2))$dst, epw_date(1:2))
    expect_equal(epw$holiday(dst = c(as.Date("2008-01-01"), as.Date("2008-02-01")))$dst, epw_date(c("Jan 01", "Feb 01")))
    expect_is(epw$comment1(), "character")
    expect_equal(epw$comment1("comment1"), "comment1")
    expect_equal(epw$comment1(), "comment1")
    expect_is(epw$comment2(), "character")
    expect_equal(epw$comment2("comment2"), "comment2")
    expect_equal(epw$comment2(), "comment2")
    expect_equal(epw$num_period(), 1L)
    expect_equal(epw$interval(), 1L)
    expect_is(epw$period(), "data.table")
    expect_is(epw$period(1), "data.table")
    expect_error(epw$period(2), class = "error_invalid_data_period_index")
    expect_equal(epw$period(1, name = "test")$name, "test")
    expect_error(epw$period(1, name = "test"), class = "error_invalid_epw_data_period_name")
    expect_equal(epw$period(1, name = "Data")$name, "Data")
    expect_error(epw$period(1, start_day_of_week = "test"), class = "error_not_wday")
    expect_equal(epw$period(1, start_day_of_week = 3)$start_day_of_week, 3L)
    expect_equal(epw$period(1, start_day_of_week = "Wed")$start_day_of_week, 3L)
    expect_equal(epw$period(1)$start_day_of_week, 3L)

    # constant
    expect_is(epw$missing_code(), "list")
    expect_is(epw$initial_missing_value(), "list")
    expect_is(epw$range_exist(), "list")
    expect_is(epw$range_valid(), "list")
    expect_is(epw$fill_action(), "list")

    # can get weather data
    expect_warning(epw$get_data(), "deprecated")
    expect_is(epw$data(), "data.table")
    expect_error(epw$data(2), class = "error_invalid_data_period_index")
    expect_equal(ncol(epw$data()), 36L)
    expect_equal(nrow(epw$data()), 8760L)
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
    # can change the time zone of datetime column in the returned weather data
    expect_error(attr(epw$data(tz = "America/Chicago")$datetime, "tzone"), class = "error_invalid_epw_date_introduced")
    expect_equal(attr(epw$data(start_year = 2019, tz = "Etc/GMT+8")$datetime, "tzone"), "Etc/GMT+8")

    expect_equal(nrow(epw$abnormal_data()), 2170L)
    expect_true("line" %in% names(epw$abnormal_data()))
    expect_equal(ncol(epw$abnormal_data()), 37L)
    expect_equal(nrow(epw$abnormal_data(cols = "albedo")), 2160L)
    expect_equal(ncol(epw$abnormal_data(cols = "albedo", keep_all = FALSE)), 8L)
    expect_equal(nrow(epw$abnormal_data(cols = "albedo", type = "out_of_range")), 0L)

    expect_equal(nrow(epw$redundant_data()), 0L)

    expect_true(all(is.na(epw$make_na(missing = TRUE)$abnormal_data(cols = "albedo", type = "missing")$albedo)))

    expect_true(all(!is.na(epw$fill_abnormal(missing = TRUE, special = TRUE)$abnormal_data(cols = "albedo", type = "missing")$albedo)))

    expect_is(epw$add_unit()$data()$dry_bulb_temperature, "units")

    expect_is(epw$drop_unit()$data()$dry_bulb_temperature, "numeric")

    expect_silent(epw$purge())

    epw <- read_epw(path_epw)
    # can change weather data
    expect_silent(epw$set(epw$data(), warning = FALSE))
    expect_error(epw$add(epw$data(), warning = FALSE), class = "error_epw_data_overlap")
    expect_error(epw$add(epw$data(start_year = 2016L), realyear = TRUE, warning = FALSE), class = "error_invalid_epw_data_date")
    expect_silent(epw$add(epw$data(start_year = 2018L), realyear = TRUE, warning = FALSE))
    expect_equal(epw$period()$index, 1L:2L)
    expect_equal(epw$period()$name, c("Data1", "Data"))
    expect_warning(epw$add(epw$data(start_year = 2019L), after = 5L, realyear = TRUE, warning = FALSE),
        "starting date will be overwriten"
    )
    expect_equal(epw$period()$index, 1L:3L)
    expect_equal(epw$period()$name, c("Data1", "Data", "Data2"))
    expect_equal(epw$period()$start_day, c(c(epw_date("2018/1/1"), epw_date("1/1", FALSE)), epw_date("2019/1/1")))

    expect_equal(find_nearst_wday_year(make_date(2019, 1, 14), 1, 2019), 2019)
    expect_equal(find_nearst_wday_year(make_date(2019, 1, 14), 2, 2019), 2014)

    # EpwDate Class {{{
    expect_error(epw_date(list()), "Missing method to convert")
    expect_equal(epw_date(""), init_epwdate_vctr(1))
    expect_equal(format(epw_date("")), NA_character_)
    expect_output(print(epw_date("")), "NA")

    expect_equal(epw_date(0L), init_epwdate_vctr(1, "0-01-01"))
    expect_equal(epw_date("0"), init_epwdate_vctr(1, "0-01-01"))

    expect_equal(epw_date(367), init_epwdate_vctr(1))
    expect_equal(format(epw_date(367)), NA_character_)
    expect_output(print(epw_date(367)), "NA")

    expect_equal(epw_date(366), init_epwdate_vctr(1, "4-12-31"))
    expect_equal(format(epw_date(366)), "366")
    expect_output(print(epw_date(366)), "366th day")

    expect_equal(epw_date(3), init_epwdate_vctr(1, "4-01-03"))
    expect_equal(format(epw_date(3)), "3")
    expect_output(print(epw_date(3)), "3rd day")

    expect_equal(epw_date("01/03"), init_epwdate_vctr(1, "8-01-03"))
    expect_equal(format(epw_date("Apr-01")), "4/ 1")
    expect_output(print(epw_date("Apr-01")), "Apr 01")

    expect_equal(epw_date("01-Apr"), init_epwdate_vctr(1, "8-04-01"))
    expect_equal(format(epw_date("01-Apr")), "4/ 1")
    expect_output(print(epw_date("01-Apr")), "Apr 01")

    expect_equal(epw_date("2019-01-Apr"), init_epwdate_vctr(1))
    expect_equal(format(epw_date("2019-01-Apr")), NA_character_)
    expect_output(print(epw_date("2019-01-Apr")), "NA")

    expect_equal(epw_date("2019-Apr-01"), init_epwdate_vctr(1))
    expect_equal(format(epw_date("2019-Apr-01")), NA_character_)
    expect_output(print(epw_date("2019-Apr-01")), "NA")

    expect_equal(epw_date("4-01-2019"), init_epwdate_vctr(1, "2019-04-01"))
    expect_equal(format(epw_date("4-01-2019")), "2019/4/ 1")
    expect_output(print(epw_date("4-01-2019")), "2019-04-01")

    expect_equal(epw_date("last Mon in Jan"), init_epwdate_vctr(1, "16-01-25"))
    expect_equal(format(epw_date("last Mon in Jan")), "Last Monday in January")
    expect_output(print(epw_date("last Mon in Jan")), "Last Monday in January")

    expect_equal(epw_date("1st Mon in Jan"), init_epwdate_vctr(1, "12-01-02"))
    expect_equal(format(epw_date("1st Mon in Jan")), "1st Monday in January")
    expect_output(print(epw_date("1st Mon in Jan")), "1st Monday in January")

    expect_equal(epw_date("6 Mon in Jan"), init_epwdate_vctr(1))
    expect_equal(format(epw_date("6 Mon in Jan")), NA_character_)
    expect_output(print(epw_date("6 Mon in Jan")), "NA")

    expect_equal(c(epw_date("1/3"), epw_date("3")), epw_date(c("1/3", "3")))

    expect_true(is_EpwDate(epw_date("1")))
    expect_false(is_EpwDate(Sys.Date()))
    expect_equal(epw_date(1), as_EpwDate("1"))
    expect_true(is.na(epw_date("")))
    expect_false(is.na(epw_date(1)))
    expect_equal(length(epw_date(1:5)), 5L)
    expect_equal(epw_date(1:5)[2L], epw_date(2))
    expect_equal(epw_date(1:5)[[3L]], epw_date(3))
    expect_equal({d <- epw_date(1:2);d[1] <- epw_date(3);d}, epw_date(c(3, 2)))
    expect_equal({d <- epw_date(1:2);d[[1]] <- epw_date(3);d}, epw_date(c(3, 2)))
    # }}}

    # do not test on CRAN
    skip_on_cran()
    # download weather
    expect_message({path_epw <- download_weather("USA_CA_San.Francisco.Intl.AP.724940_TMY3",
        ask = FALSE, type = "epw", dir = tempdir())}
    )
})
