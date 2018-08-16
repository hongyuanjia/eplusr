test_that("Epw class", {
    # read an EPW file from EnergyPlus website
    path_base <- "https://energyplus.net/weather-download"
    path_region <- "north_and_central_america_wmo_region_4/USA/CA"
    path_file <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
    path_epw <- file.path(path_base, path_region, path_file)

    # can read an EPW file from EnergyPlus website
    expect_silent(epw <- read_epw(path_epw))

    # can save the file
    expect_silent(epw$save(file.path(tempdir(), "weather.epw"), overwrite = TRUE))

    # can update the path after saved
    expect_equal(epw$path(), file.path(tempdir(), "weather.epw"))

    # can read local EPW file
    expect_silent(epw <- read_epw(file.path(tempdir(), "weather.epw")))

    # can get basic info
    expect_equal(epw$city, "San Francisco Intl Ap")
    expect_equal(epw$state_province, "CA")
    expect_equal(epw$country, "USA")
    expect_equal(epw$data_source, "TMY3")
    expect_equal(epw$wmo_number, "724940")
    expect_equal(epw$latitude, 37.62)
    expect_equal(epw$longitude, -122.4)
    expect_equal(epw$time_zone, -8)
    expect_equal(epw$elevation, 2)
    expect_equal(epw$time_step, 1)
    expect_equal(epw$start_day_of_week, "Sunday")

    # can set basic info
    expect_equal({epw$city <- "Chongqing";epw$city}, "Chongqing")
    expect_equal({epw$state_province <- "Chongqing";epw$state_province}, "Chongqing")
    expect_equal({epw$country <- "China";epw$country}, "China")
    expect_equal({epw$data_source <- "TMY";epw$data_source}, "TMY")
    expect_equal({epw$wmo_number <- "724944";epw$wmo_number}, "724944")
    expect_equal({epw$latitude <- 20.0;epw$latitude}, 20.0)
    expect_equal({epw$longitude <- -120.0;epw$longitude}, -120.0)
    expect_equal({epw$time_zone <- 8;epw$time_zone}, 8)
    expect_equal({epw$elevation <- 100;epw$elevation}, 100)
    expect_equal({epw$time_step <- 2;epw$time_step}, 2)
    expect_equal({epw$start_day_of_week <- "Monday";epw$start_day_of_week}, "Monday")

    # can read from local file
    expect_silent(epw <- read_epw(file.path(tempdir(), "weather.epw")))

    # can get weather data
    expect_is(epw$get_data(), "data.table")
    expect_equal(ncol(epw$get_data()), 36L)
    expect_equal(nrow(epw$get_data()), 8760L)
    # can change year in datetime column
    expect_equal(
        epw$get_data(year = 2018, tz = "GMT")$datetime,
        seq(as.POSIXct("2018-01-01 01:00:00", tz = "GMT"),
            as.POSIXct("2019-01-01 00:00:00", tz = "GMT"),
            by = "1 hour"
        )
    )
    # can change the year column
    expect_equal(epw$get_data(year = 2018, update = TRUE)$year, c(rep(2018L, times = 8759), 2019L))

    # can get weather data with units
    expect_is(epw$get_data(unit = TRUE)$dry_bulb_temperature, "units")

    # can change the time zone of datetime column in the returned weather data
    expect_equal(attr(epw$get_data(tz = "America/Chicago")$datetime, "tzone"), "America/Chicago")

    # can change weather data
    expect_silent(epw$set_data(epw$get_data()))
})
