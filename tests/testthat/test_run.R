context("Run method")

test_that("Run IDF works", {

    # for CI testing
    ver <- Sys.getenv("ENERGYPLUS_INSTALL_VERSION")
    # for local testing
    if (ver != "") {
        path_8.8 <- eplus_path(8.8)
        path_8.7 <- eplus_path(8.7)
        path_8.6 <- eplus_path(8.6)
        path_8.5 <- eplus_path(8.5)
        path_8.4 <- eplus_path(8.4)
        path_8.3 <- eplus_path(8.3)
        path_8.2 <- eplus_path(8.2)
        path_eplus <- list(path_8.8, path_8.7, path_8.6, path_8.5, path_8.4,
                           path_8.3, path_8.2)
    } else {
        path_8.8 <- eplus_path(8.8)
        path_8.7 <- eplus_path(8.7)
        path_8.6 <- eplus_path(8.6)
        path_eplus <- list(path_8.8, path_8.7, path_8.6)
    }

    example_folder <- "ExampleFiles"
    testfile <- "1ZoneUncontrolled.idf"
    output_folder <- "test_output"

    for (i in seq_along(path_eplus)) {
        eplus_home <- path_eplus[[i]]["home"]
        weather <- path_eplus[[i]]["epw"]
        path_idd <- path_eplus[[i]]["idd"]
        expect_silent(model <- eplus_model$new(file.path(eplus_home, example_folder, testfile), idd = path_idd))
        expect_output(model$run(weather = weather,  dir = file.path("~", output_folder)))
        expect_message(model$run(~"design_day", weather,  dir = file.path("~", output_folder)))
        expect_warning(model$run(eplus_home = path_eplus[[i]]["home"]))
        model <- eplus_model$new(file.path(eplus_home, example_folder, testfile), idd = path_idd)
        model$notes(1, "test_run")
        expect_error(model$run())
    }
})
