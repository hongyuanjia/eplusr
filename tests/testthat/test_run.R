context("Run method")

test_that("Run helper: eplus_path workds", {
    # eplus_path
    expect_equal(
        eplus_path(8.8),
        switch(Sys.info()["sysname"],
        Windows = c(home = "C:/EnergyPlusV8-8-0",
                    eplus = "C:/EnergyPlusV8-8-0/energyplus.exe",
                    idd = "C:/EnergyPlusV8-8-0/Energy+.idd",
                    epw = "C:/EnergyPlusV8-8-0/WeatherData/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw"),
        Linux = c(home = "/usr/local/EnergyPlus-8-8-0",
                  eplus ="/usr/local/EnergyPlus-8-8-0/energyplus",
                  idd = "/usr/local/EnergyPlus-8-8-0/Energy+.idd",
                  epw = "/usr/local/EnergyPlus-8-8-0/WeatherData/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw"),
        Darwin = c(home = "/Applications/EnergyPlus-8-8-0",
                   eplus = "/Applications/EnergyPlus-8-8-0/energyplus",
                   idd = "/Applications/EnergyPlus-8-8-0/Energy+.idd",
                   epw = "/Applications/EnergyPlus-8-8-0/WeatherData/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw")
    ))
    expect_error(eplus_path(path = NULL))
    expect_error(eplus_path(path = "~"))

    home_8.7 <- switch(osname(),
        Windows = "C:/EnergyPlusV8-7-0",
        Linux = "/usr/local/EnergyPlus-8-7-0",
        Darwin = "/Applications/EnergyPlus-8-7-0")
    names(home_8.7) <- "home"
    expect_equal(eplus_path("8.8", path = path_8.7)["home"], home_8.7)
})

test_that("Run helper: parse_runperiod works", {

    expect_equal(days_in_month(as.Date("2017-2-04")), 28L)
    expect_equal(days_in_month(as.Date("2017-4-04")), 30L)

    expect_equal(parse_runperiod(~.), list(start = "asis", end = "asis"))
    expect_equal(parse_runperiod(.~.), list(start =  "asis", end = "asis"))
    expect_error(parse_runperiod("design_day"~.))
    expect_equal(parse_runperiod(~"design_day"),
                 list(start = "design_day", end = "design_day"))
    expect_equal(parse_runperiod(~"annual"),
                 list(start = "annual", end = "annual"))
    expect_equal(parse_runperiod(~4),
                 list(start = as.Date("2017-04-01", tz = Sys.timezone()),
                      end = as.Date("2017-04-30", tz = Sys.timezone())))
    expect_equal(parse_runperiod(~"4"),
                 list(start = as.Date("2017-04-01", tz = Sys.timezone()),
                      end = as.Date("2017-04-30", tz = Sys.timezone())))
    expect_equal(parse_runperiod(~"April"),
                 list(start = as.Date("2017-04-01", tz = Sys.timezone()),
                      end = as.Date("2017-04-30", tz = Sys.timezone())))
    expect_equal(parse_runperiod(~"Apr"),
                 list(start = as.Date("2017-04-01", tz = Sys.timezone()),
                      end = as.Date("2017-04-30", tz = Sys.timezone())))
    expect_equal(parse_runperiod(3~4),
                 list(start = as.Date("2017-03-01", tz = Sys.timezone()),
                      end = as.Date("2017-04-30", tz = Sys.timezone())))
    expect_equal(parse_runperiod(3.2~4.5),
                 list(start = as.Date("2017-03-02", tz = Sys.timezone()),
                      end = as.Date("2017-04-05", tz = Sys.timezone())))
    expect_equal(parse_runperiod("3.2"~4.5),
                 list(start = as.Date("2017-03-02", tz = Sys.timezone()),
                      end = as.Date("2017-04-05", tz = Sys.timezone())))
})

test_that("Run IDF: set runperiod and set table output works", {
    model <- eplus_model$new("files/5Zone_Transformer_8.8.idf")
    idd <- .get(model, "idd")

    idf <- .get(model, "model")
    expect_warning(idf <- set_runperiod(idf, ~3, idd, TRUE))
    expect_equal(attr(idf, "runperiod"), parse_runperiod(~3))
    expect_equal(unique(get_value(idf, 11)[, edited]), -1L)
    expect_equal(unique(get_value(idf, 12)[, edited]), -1L)
    expect_equal(get_value(idf, max_id(idf))[, value],
                 c("run_period_eplusr", "3", "1", "3", "31", "UseWeatherFile",
                   "Yes", "Yes", "No", "Yes", "Yes"))

    model$set(302, "Comma")
    idf <- .get(model, "model")
    expect_warning(idf <- set_output_table_style(idf, idd))
    expect_equal(get_value(idf, 302)[, value], "CommaAndHTML")

    model$set(302, "XML")
    idf <- .get(model, "model")
    expect_warning(idf <- set_output_table_style(idf, idd))
    expect_equal(get_value(idf, 302)[, value], "XMLAndHTML")

    model$set(302, "All")
    idf <- .get(model, "model")
    expect_silent(idf <- set_output_table_style(idf, idd))
    expect_equal(get_value(idf, 302)[, value], "All")
})

test_that("Run IDF works", {

    # for CI testing
    ver <- Sys.getenv("ENERGYPLUS_VERSION")
    # for local testing
    if (ver != "") {
        ver <- substr(ver, 1, 3)
        path_eplus <- eplus_path(ver)
    # local test
    } else {
        path_eplus <- eplus_path(8.8)
    }
    eplus_home <- path_eplus["home"]
    weather <- path_eplus["epw"]
    path_idd <- path_eplus["idd"]

    example_folder <- "ExampleFiles"
    testfile <- "1ZoneUncontrolled.idf"
    output_folder <- "test_output"

    expect_silent(model <- eplus_model$new(file.path(eplus_home, example_folder, testfile), idd = path_idd))
    expect_output(model$run(weather = weather,  dir = file.path(output_folder), echo = TRUE))
    expect_message(model$run(~"design_day", weather,  dir = file.path(output_folder), echo = TRUE))
    expect_warning(model$run(eplus_home = eplus_home, echo = TRUE))

    # error when model has been modified without saving
    model$notes(1, "test_run")
    expect_error(model$run())

    unlink(output_folder, recursive = TRUE)
})
