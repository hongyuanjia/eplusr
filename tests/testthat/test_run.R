context("Run method")

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
    expect_output(model$run(weather = weather,  dir = file.path("~", output_folder)))
    expect_message(model$run(~"design_day", weather,  dir = file.path("~", output_folder)))
    expect_warning(model$run(eplus_home = eplus_home))
    model <- eplus_model$new(file.path(eplus_home, example_folder, testfile), idd = path_idd)
    model$notes(1, "test_run")
    expect_error(model$run())
})
