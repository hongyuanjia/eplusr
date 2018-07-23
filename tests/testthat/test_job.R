context("Job methods")

if (!is_avail_eplus(8.8)) install_eplus(8.8)

cfg <- eplus_config(8.8)

example_name <- "5Zone_Transformer.idf"
weather_name <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
ddy_name <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3.ddy"

path_example <- file.path(cfg$dir, "ExampleFiles", example_name)
path_idf <- file.path(getwd(), example_name)
file.copy(path_example, path_idf, overwrite = TRUE)

path_weather <- file.path(cfg$dir, "WeatherData", weather_name)
path_epw <- file.path(getwd(), weather_name)

test_that("can create a `EplusJob` object", {
    expect_silent(job <- eplus_job(path_idf, path_weather))
})

job <- eplus_job(path_idf, path_weather)

test_that("can run the simulation", {
    expect_true({
        proc <- job$run(wait = FALSE)
        Sys.sleep(1)
        job$kill()
    })
    expect_output(job$run())
})

test_that("can get status of simulation", {
    expect_equal(job$status(),
        list(run_before = TRUE, changed_after = FALSE, terminated = FALSE,
            successful = TRUE, alive = FALSE, wait = TRUE)
    )
})

test_that("can return the output directory", {
    expect_equal(job$output_dir(), dirname(path_idf))
})

test_that("can return simulation errors", {
    expect_is(job$errors(), "ErrFile")
})

# clean
clean_wd(path_idf)
unlink(c(path_idf, path_epw))
