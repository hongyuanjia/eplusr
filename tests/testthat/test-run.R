test_that("clean_wd()", {
    expect_true(file.create(f <- tempfile()))
    expect_true({clean_wd(f); file.exists(f)})
    unlink(f)

    expect_true(file.create(f <- tempfile(fileext = ".idf")))
    expect_silent({clean_wd(f); file.exists(f)})
    unlink(f)

    expect_true(file.create(f <- file.path(tempdir(), "in.idf")))
    expect_silent({clean_wd(f); file.exists(f)})
    unlink(f)
})

test_that("utilities", {
    .globals$eplus$"8.1.0"$version <- numeric_version("8.1.0")
    .globals$eplus$"8.1.0"$dir <- tempdir()
    .globals$eplus$"8.1.0"$exe <- "energyplus"
    get(".globals", envir = asNamespace("eplusr"))$eplus
    expect_error(eplus_exe(8.1), class = "eplusr_error_eplus_ver_not_supported")
    .globals$eplus$"8.1.0" <- NULL
})

test_that("run_idf()", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    path_idf <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    # can run ddy simulation
    expect_silent(res <- run_idf(path_idf, NULL, output_dir = tempdir(), echo = FALSE))
    # can specify EnergyPlus version
    expect_silent(res <- run_idf(path_idf, NULL, output_dir = tempdir(), echo = FALSE, eplus = 8.8))
    expect_null(res$epw)
    # can stop if failed to find version
    expect_error({
        f <- tempfile(fileext = ".idf")
        write_lines(read_lines(path_idf)[-91], f)
        run_idf(f, NULL, output_dir = tempdir(), echo = FALSE)
    }, "Missing version field", class = "eplusr_error_miss_idf_ver")
    # can use input file directory
    expect_silent({
        f <- tempfile(fileext = ".idf")
        file.copy(path_idf, f, overwrite = TRUE)
        res <- run_idf(f, NULL, output_dir = NULL, echo = FALSE)
    })

    # can run simulation with weather
    expect_silent(res <- run_idf(path_idf, path_epw, output_dir = tempdir(), echo = FALSE))

    expect_equal(res$idf, normalizePath(path_idf))
    expect_equal(res$epw, normalizePath(path_epw))
    expect_equal(res$version, "8.8.0")
    expect_equal(res$exit_status, 0L)
    expect_is(res$start_time, "POSIXct")
    expect_is(res$end_time, "POSIXct")
    expect_equal(res$output_dir, normalizePath(tempdir(), mustWork = FALSE))
    expect_equal(res$energyplus, normalizePath(file.path(eplus_config(8.8)$dir, eplus_config(8.8)$exe), mustWork = TRUE))
    expect_is(res$stdout, "character")
    expect_true("stderr" %in% names(res))
    expect_is(res$process, "process")
    expect_true(file.exists(file.path(tempdir(), basename(res$idf))))
    expect_true(file.exists(file.path(tempdir(), basename(res$epw))))

    # can run in the background
    expect_silent(res <- run_idf(path_idf, NULL, output_dir = tempdir(), wait = FALSE))
    expect_equal(res$idf, normalizePath(path_idf))
    expect_null(res$epw)
    expect_equal(res$version, "8.8.0")
    expect_null(res$exit_status)
    expect_is(res$start_time, "POSIXct")
    expect_null(res$end_time)
    expect_equal(res$output_dir, normalizePath(tempdir(), mustWork = FALSE))
    expect_equal(res$energyplus, normalizePath(file.path(eplus_config(8.8)$dir, eplus_config(8.8)$exe), mustWork = TRUE))
    expect_null(res$stdout)
    expect_null(res$stderr)
    expect_is(res$process, "process")
    expect_silent({res$process$wait(); res_post <- res$process$get_result()})
    expect_is(res_post$stdout, "character")
    expect_is(res_post$stderr, "character")

    # can run model with HVACTemplate objects
    path <- file.path(eplus_config(8.8)$dir, "ExampleFiles/HVACTemplate-5ZoneVAVWaterCooled.idf")
    expect_is(run_idf(path, NULL, tempdir(), design_day = TRUE, echo = FALSE), "list")
    expect_true(file.exists(file.path(tempdir(), "HVACTemplate-5ZoneVAVWaterCooled.expidf")))

    expect_is(res <- run_idf(path, NULL, tempdir(), design_day = TRUE, wait = FALSE), "list")
    while(res$process$is_alive()) Sys.sleep(0.2)
    expect_is(res$process$get_result(), "list")
    expect_true(file.exists(file.path(tempdir(), "HVACTemplate-5ZoneVAVWaterCooled.expidf")))
})

test_that("run_multi()", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    path_idf <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    # can stop if idf and epw does not have the same length
    expect_error(run_multi(rep(path_idf, 2), rep(path_epw, 3)), "Must have same length")
    # can stop if idf and eplus does not have the same length
    expect_error(run_multi(rep(path_idf, 2), NULL, eplus = rep(8.8, 3)), "Must have same length")
    # can stop if idf and design does not have the same length
    expect_error(run_multi(rep(path_idf, 2), NULL, design_day = rep(FALSE, 3)), "Must have same length")
    # can stop if idf and annual does not have the same length
    expect_error(run_multi(rep(path_idf, 2), NULL, annual = rep(FALSE, 3)), "Must have same length")
    # can stop if both design and annual is TRUE
    expect_error(run_multi(path_idf, NULL, annual = TRUE, design_day = TRUE), "both design-day-only", class = "eplusr_error_both_ddy_annual")
    # can stop if model does not exist
    expect_error(run_multi(tempfile(), NULL))
    # can stop if model does not contain version
    expect_error({
        f <- tempfile(fileext = ".idf")
        write_lines(read_lines(path_idf)[-91], f)
        run_multi(f, NULL, output_dir = tempdir())
    }, "Failed to determine the version of EnergyPlus", class = "eplusr_error_miss_idf_ver")
    # can stop if target EnergyPlus is not found
    expect_error(run_multi(path_idf, NULL, eplus = 8.0), "Cannot locate EnergyPlus", class = "eplusr_error_locate_eplus")
    # can stop if input idf contain duplications
    expect_error(run_multi(rep(path_idf, 2L), NULL, output_dir = NULL), class = "eplusr_error_duplicated_sim")
    # can stop if idf and output directory does not have the same length
    expect_error(run_multi(rep(path_idf, 2L), NULL, output_dir = tempdir()), "have same length")
    # can stop if idf and output directory combines same job
    expect_error(run_multi(rep(path_idf, 2L), NULL, output_dir = rep(tempdir(), 2L)), "Duplication found")

    expect_message(res <- run_multi(rep(path_idf, 2L), NULL, output_dir = c(file.path(tempdir(), "a"), file.path(tempdir(), "b"))),
        "FAILED"
    )
    expect_is(res, "data.table")
    expect_equal(names(res), c("index", "status", "idf", "epw", "version",
            "exit_status", "start_time", "end_time", "output_dir", "energyplus",
            "stdout", "stderr"))
    expect_equal(res$index, 1:2)
    expect_equal(res$status, rep("failed", 2))
    expect_equal(res$idf, rep(normalizePath(path_idf), 2))
    expect_equal(res$epw, rep(NA_character_, 2))
    expect_equal(res$version, rep("8.8.0", 2))
    expect_equal(res$exit_status > 0, rep(TRUE, 2))
    expect_is(res$start_time, "POSIXct")
    expect_is(res$end_time, "POSIXct")
    expect_equal(res$energyplus, rep(normalizePath(file.path(eplus_config(8.8)$dir, eplus_config(8.8)$exe), mustWork = TRUE), 2L))
    checkmate::expect_list(res$stdout, "character")
    checkmate::expect_list(res$stderr, "character")

    expect_silent(res <- run_multi(rep(path_idf, 2L), NULL, output_dir = c(file.path(tempdir(), "a"), file.path(tempdir(), "b")), wait = FALSE))
    expect_is(res, "r_process")
    expect_equal({res$wait(); res$get_exit_status()}, 0L)
    expect_silent(res <- res$get_result())
    expect_is(res, "data.table")
    expect_equal(names(res), c("index", "status", "idf", "epw", "version",
            "exit_status", "start_time", "end_time", "output_dir", "energyplus",
            "stdout", "stderr"))
    expect_equal(res$index, 1:2)
    expect_equal(res$status, rep("failed", 2))
    expect_equal(res$idf, rep(normalizePath(path_idf, mustWork = FALSE), 2))
    expect_equal(res$epw, rep(NA_character_, 2))
    expect_equal(res$version, rep("8.8.0", 2))
    expect_equal(res$exit_status > 0, rep(TRUE, 2))
    expect_is(res$start_time, "POSIXct")
    expect_is(res$end_time, "POSIXct")
    expect_equal(res$output_dir, normalizePath(c(file.path(tempdir(), "a"), file.path(tempdir(), "b")), mustWork = FALSE))
    expect_equal(res$energyplus, rep(normalizePath(file.path(eplus_config(8.8)$dir, eplus_config(8.8)$exe), mustWork = TRUE), 2L))
    checkmate::expect_list(res$stdout, "character")
    checkmate::expect_list(res$stderr, "character")
    expect_null(get_run_time(NULL))
    expect_null(get_run_time("a"))
})
