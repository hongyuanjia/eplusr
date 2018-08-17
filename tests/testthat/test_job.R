context("Job methods")

test_that("Job methods", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    example <- copy_example()

    expect_silent(job <- eplus_job(example$idf, example$epw))

    # # can get path
    # expect_equal(job$path(), c(example$idf, example$epw))
    # expect_equal(job$path("idf"), c(example$idf))
    # expect_equal(job$path("epw"), c(example$epw))

    # can get job status
    expect_equal(
        job$status(),
        list(run_before = FALSE, changed_after = FALSE, terminated = FALSE,
            successful = FALSE, alive = FALSE, wait = FALSE)
    )

    # can run job in waiting mode
    expect_output(job$run())

    # can refresh job status
    expect_equal(job$status(),
        list(run_before = TRUE, changed_after = FALSE, terminated = FALSE,
            successful = TRUE, alive = FALSE, wait = TRUE)
    )

    # can kill job
    expect_message(job$kill(), "job was run in waiting mode")
    expect_true({job$run(wait = FALSE);Sys.sleep(0.2);job$kill()})
    # can update the status after job was killed
    expect_true(job$status()$terminated)
    expect_message(job$kill(), "job is not running")
    expect_error(job$errors(), "Simulation was terminated before")
    expect_error(job$locate_output(), "Simulation was terminated before")

    # # can get output dir
    # expect_equal(job$output_dir(), dirname(example$idf))

    # can retreive simulation data
    expect_is({job$run();job$errors()}, "ErrFile")
    expect_is(job$errors(info = TRUE), "ErrFile")
    expect_equal(names(job$errors()), c("completed", "successful", "data"))

    expect_error(job$locate_output(".exe"), "Path.*does not exist")
    expect_error(job$report_data_dict(), "Simulation SQL output does not exists")

    idf <- read_idf(example$idf)
    job <- idf$run(example$epw, dir = NULL)
    expect_is(job$report_data_dict(), "data.table")
    expect_is(job$report_data(), "data.table")
    expect_is(job$tabular_data(), "data.table")

    expect_false(has_name(job$report_data(name = "EnergyTransfer:Building", case = NULL), "Case"))
    expect_true(has_name(job$report_data(name = "EnergyTransfer:Building"), "Case"))
    expect_equal(unique(job$report_data(name = "EnergyTransfer:Building", case = "test")$Case), "test")
    expect_equal(
        unique(format(job$report_data(name = "EnergyTransfer:Building", year = 2016L)$DateTime, "%Y")),
        "2016"
    )
    expect_equal(
        attr(job$report_data(name = "EnergyTransfer:Building", year = 2016L, tz = "America/Chicago")$DateTime, "tzone"),
        "America/Chicago"
    )

    # clean
    clean_wd(example$idf)
    unlink(c(example$idf, example$epw))
})
