context("Job methods")

test_that("Job methods", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    example <- copy_example()

    expect_silent(job <- eplus_job(example$idf, example$epw))

    # can get job status
    expect_equal(
        job$status(),
        list(run_before = FALSE, alive = FALSE, terminated = NA,
            successful = NA, changed_after = NA)
    )

    # can run job in waiting mode
    expect_output(job$run(wait = TRUE))

    # can refresh job status
    expect_equal(job$status(),
        list(run_before = TRUE, alive = FALSE, terminated = FALSE,
            successful = TRUE, changed_after = FALSE)
    )

    # can kill job
    expect_message(job$kill(), "job is not running")

    # # can kill backgroun R process
    # add a full-year run period to lengthen simulation time
    # idf <- read_idf(example$idf)
    # idf$dup_object("WinterDay", "FullYear")
    # full <- idf$RunPeriod$FullYear
    # full$End_Month <- 12
    # idf$save(example$idf, overwrite = TRUE)
    # job <- eplus_job(example$idf, example$epw)
    # expect_true({job$run(wait = FALSE);Sys.sleep(0.4);job$kill()})
    # # can update the status after job was killed
    # # can stop retreiving data when simulation was killed
    # expect_true(job$status()$terminated)
    # expect_message(job$kill(), "job is not running")
    # expect_error(job$errors(), "Simulation was terminated before")
    # expect_error(job$locate_output(), "Simulation was terminated before")

    example <- copy_example()
    job <- eplus_job(example$idf, example$epw)
    expect_is({job$run();job$errors()}, "ErrFile")
    expect_is(job$errors(info = TRUE), "ErrFile")
    expect_silent({err <- job$errors()})
    expect_equal(names(err), c("index", "environment_index", "environment",
        "level_index", "level", "message"
    ))
    expect_equal(attr(err, "eplus_version"), numeric_version("8.8.0"))
    expect_equal(attr(err, "eplus_build"), "7c3bbe4830")
    expect_equal(attr(err, "idd_version"), numeric_version("8.8.0"))
    expect_equal(attr(err, "successful"), TRUE)
    expect_equal(attr(err, "terminated"), FALSE)

    # can retrieve simulation data
    idf <- read_idf(example$idf)
    job <- idf$run(example$epw, dir = NULL)
    # can get all table names
    expect_equal(length(job$list_table()), 44L)

    # can read table
    expect_error(job$read_table("a"), "no such table")
    expect_is(job$read_table("Zones"), "data.table")

    # can read report data dictionary
    expect_is(job$report_data_dict(), "data.table")

    # can read report data
    expect_equal(nrow(job$report_data()), 3840L)
    expect_equal(nrow(job$report_data("")), 1344L)
    expect_equal(nrow(job$report_data(
        "TRANSFORMER 1", "Transformer Load Loss Rate")),
        192L
    )
    expect_equal(nrow(job$report_data(
        "TRANSFORMER 1", "Transformer Load Loss Rate")),
        192L
    )
    expect_equal(year(job$report_data(
        "TRANSFORMER 1", "Transformer Load Loss Rate", year = 2010)$datetime),
        rep(2010, 192)
    )
    expect_equal(lubridate::tz(job$report_data(tz = "Asia/Shanghai")$datetime),
        "Asia/Shanghai"
    )
    expect_equal(job$report_data(case = "test")$case, rep("test", 3840))
    expect_equal(names(job$report_data(all = TRUE)),
        c("case", "datetime", "month", "day", "hour", "minute", "dst", "interval",
          "simulation_days", "day_type", "environment_name", "is_meter", "type",
          "index_group", "timestep_type", "key_value", "name", "reporting_frequency",
          "schedule_name", "units", "value"
        )
    )
    expect_equal(nrow(job$report_data(period = seq(
        lubridate::ymd_hms("2019-01-14 0:0:0"), lubridate::ymd_hms("2019-01-15 0:0:0"), "15 min")
    )), 1900)
    expect_equal(nrow(job$report_data(month = 1)), 1920)
    expect_equal(nrow(job$report_data(month = 1, hour = 1)), 80)
    expect_equal(nrow(job$report_data(minute = 0)), 960)
    expect_equal(nrow(job$report_data(interval = 15)), 3840)
    expect_equal(nrow(job$report_data(simulation_days = 1)), 3840)
    expect_equal(nrow(job$report_data(day_type = "Tuesday")), 3840)
    expect_equal(nrow(job$report_data(environment_name = "WINTERDAY")), 1920)

    skip_on_os("mac")
    # can get path
    expect_equal(job$path(), c(idf = example$idf, epw = example$epw))
    expect_equal(job$path("idf"), c(example$idf))
    expect_equal(job$path("epw"), c(example$epw))

    # can get output dir
    expect_equal(job$output_dir(), dirname(example$idf))

    # can get output file path
    expect_equal(
        job$locate_output(".err"),
        normalizePath(file.path(tempdir(), "5Zone_Transformer.err"))
    )

    clean_wd(example$idf)
    unlink(c(example$idf, example$epw))
})
