test_that("Job methods", {
    skip_on_cran()
    skip_if_not_integration()

    path_idf <- copy_eplus_example(LATEST_EPLUS_VER, "5Zone_Transformer.idf")
    path_epw <- path_eplus_weather(LATEST_EPLUS_VER, "USA_CO_Golden-NREL.724666_TMY3.epw")

    expect_s3_class(job <- eplus_job(path_idf, path_epw), "EplusJob")

    expect_equal(job$version(), numeric_version(LATEST_EPLUS_VER))
    expect_output(job$print())

    # can get job status
    expect_equal(
        job$status(),
        list(run_before = FALSE, alive = FALSE, terminated = NA,
            successful = NA, changed_after = NA)
    )

    # can run job in waiting mode
    expect_s3_class(job$run(wait = TRUE, echo = FALSE), "EplusJob")

    # can refresh job status
    expect_equal(job$status(),
        list(run_before = TRUE, alive = FALSE, terminated = FALSE,
            successful = TRUE, changed_after = FALSE)
    )

    # can kill job
    expect_false(job$kill())

    # can read ERR file
    path_idf <- copy_eplus_example(LATEST_EPLUS_VER, "5Zone_Transformer.idf")
    job <- eplus_job(path_idf, path_epw)
    expect_s3_class({job$run(echo = FALSE);job$errors()}, "ErrFile")
    expect_s3_class(job$errors(info = TRUE), "ErrFile")
    expect_silent({err <- job$errors()})
    expect_equal(names(err), c("index", "envir_index", "envir",
        "level_index", "level", "message"
    ))
    expect_equal(attr(err, "eplus_version"), numeric_version(LATEST_EPLUS_VER))
    expect_equal(attr(err, "eplus_build"), ALL_EPLUS_RELEASE_COMMIT$commit[1])
    # New EnergyPlus version has removed the IDD version in ERR file
    expect_equal(attr(err, "idd_version"), NA)
    expect_equal(attr(err, "successful"), TRUE)
    expect_equal(attr(err, "terminated"), FALSE)

    # can retrieve simulation data
    idf <- read_idf(path_idf)
    job <- idf$run(path_epw, dir = NULL, echo = FALSE)
    # can get all table names
    expect_equal(length(job$list_table()), 44L)

    # can read table
    expect_error(job$read_table("a"), "no such table")
    expect_s3_class(job$read_table("Zones"), "data.table")

    # can read report data dictionary
    expect_s3_class(job$report_data_dict(), "data.table")

    # can read report data
    expect_equal(nrow(job$report_data()), 3840L)
    expect_equal(nrow(job$report_data(name = job$report_data_dict()[is.na(key_value), name])), 1344L)
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
          "simulation_days", "day_type", "environment_name",
          "environment_period_index", "is_meter", "type", "index_group",
          "timestep_type", "key_value", "name", "reporting_frequency",
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

    expect_true(job == job)
    expect_false(job != job)

    skip_on_os("mac")
    # can get path
    expect_equal(job$path(), c(idf = path_idf, epw = path_epw))
    expect_equal(job$path("idf"), c(path_idf))
    expect_equal(job$path("epw"), c(path_epw))

    # can get output dir
    expect_equal(job$output_dir(), normalizePath(dirname(path_idf)))

    # can get output file path
    expect_equal(
        job$locate_output(".err"),
        normalizePath(file.path(tempdir(), "5Zone_Transformer.err"))
    )

    # can list all output files
    expect_equal(length(job$list_files()), 57L)
    expect_equal(length(job$list_files(full = TRUE)), 57L)
    expect_equal(length(job$list_files(simplify = TRUE)), 23L)
    expect_equal(length(job$list_files(simplify = TRUE, full = TRUE)), 23L)
    expect_equal(normalizePath(unique(dirname(job$list_files(simplify = TRUE, full = TRUE)))), job$output_dir())

    clean_wd(path_idf)
    unlink(c(path_idf, file.path(tempdir(), basename(path_epw))))
})

# vim: set fdm=marker:

test_that("stock API jobs preserve SQL results and separate consecutive worker lifecycles", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    cli <- eplus_job(fixture$model, fixture$weather)
    cli$run(
        dir = file.path(fixture$directory, "cli"),
        readvars = FALSE,
        echo = FALSE,
        wait = FALSE
    )
    cli_process <- cli$.__enclos_env__$private$m_job
    on.exit(if (cli_process$is_alive()) cli$kill(), add = TRUE)
    api <- read_idf(fixture$model)$run(
        fixture$weather,
        dir = file.path(fixture$directory, "api"),
        readvars = FALSE,
        echo = FALSE,
        backend = "api"
    )
    expect_true(
        api$status()$successful,
        info = jsonlite::toJSON(
            api$status()$api,
            auto_unbox = TRUE,
            null = "null"
        )
    )
    expect_identical(api$status()$backend, "api")
    expect_match(
        api$status()$api$version,
        paste0(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"), ".0"),
        fixed = TRUE
    )
    expect_false(identical(api$status()$api$pid, Sys.getpid()))
    api_data <- api$read_table("ReportData")
    cli_process$wait(60000)
    expect_false(cli_process$is_alive())
    expect_identical(
        cli$status(),
        list(
            run_before = TRUE,
            alive = FALSE,
            terminated = FALSE,
            successful = TRUE,
            changed_after = FALSE
        )
    )
    cli_data <- cli$read_table("ReportData")
    expect_gt(nrow(api_data), 0L)
    expect_identical(nrow(api_data), nrow(cli_data))
    expect_equal(api_data$value, cli_data$value, tolerance = 1e-10)
    first_pid <- api$status()$api$pid
    api$run(
        dir = file.path(fixture$directory, "api-second"),
        readvars = TRUE,
        echo = FALSE,
        backend = "api"
    )
    expect_true(api$status()$successful)
    expect_false(identical(api$status()$api$pid, first_pid))
    expect_true(any(grepl("\\.csv$", api$list_files(simplify = TRUE))))
})

test_that("killing a background API job reaps its native worker and returns a terminated status", {
    skip_if_not_installed("ps")
    fixture <- testrun__api_fixture()
    marker <- tempfile()
    setup <- local({
        path <- marker
        function(api, state) {
            api$runtime$callback_begin_new_environment(state, function(state) {
                writeLines(as.character(Sys.getpid()), path)
                Sys.sleep(60)
            })
            writeLines("started", paste0(path, ".started"))
        }
    })
    job <- read_idf(fixture$model)$run(
        fixture$weather,
        dir = file.path(tempdir(), "api-kill"),
        wait = FALSE,
        echo = FALSE,
        readvars = FALSE,
        backend = setup
    )
    on.exit(job$kill(), add = TRUE)
    testrun__api_started(
        function() file.exists(paste0(marker, ".started")),
        function() job$status()$alive
    )
    deadline <- Sys.time() + 30
    while (!file.exists(marker) && Sys.time() < deadline) {
        Sys.sleep(0.1)
    }
    expect_true(file.exists(marker))
    pid <- as.integer(readLines(marker))
    handle <- ps::ps_handle(pid)
    expect_true(ps::ps_is_running(handle))
    expect_true(job$kill())
    status <- job$status()
    expect_false(status$alive)
    expect_false(status$successful)
    expect_true(status$terminated)
    expect_identical(status$api$stop_reason, "process_terminated")
    deadline <- Sys.time() + 10
    while (ps::ps_is_running(handle) && Sys.time() < deadline) {
        Sys.sleep(0.1)
    }
    expect_false(ps::ps_is_running(handle))
})

test_that("foreground API Job conditions replace previous success before rethrow", {
    fixture <- testrun__api_fixture()
    condition <- NULL
    local_mocked_bindings(energyplus = function(...) {
        if (!is.null(condition)) {
            stop(condition)
        }
        list(exit_status = 0L, backend = "api", api = list(phase = "completed"))
    })
    job <- eplus_job(fixture$model, fixture$weather)
    directory <- file.path(fixture$directory, "foreground-failure")
    job$run(dir = directory, backend = "api", echo = FALSE, readvars = FALSE)
    expect_true(job$status()$successful)
    for (cause in list(
        simpleError("foreground failure"),
        structure(
            list(message = "foreground interrupt", call = NULL),
            class = c("interrupt", "condition")
        )
    )) {
        condition <- cause
        caught <- tryCatch(
            job$run(
                dir = directory,
                backend = "api",
                echo = FALSE,
                readvars = FALSE
            ),
            error = identity,
            interrupt = identity
        )
        expect_identical(caught, cause)
        status <- job$status()
        expect_true(status$run_before)
        expect_false(status$successful)
        expect_identical(status$terminated, inherits(cause, "interrupt"))
        expect_identical(
            status$api$stop_reason,
            if (inherits(cause, "interrupt")) "interrupt" else "pipeline_error"
        )
        expect_identical(status$api$error$class, class(cause))
    }
})


test_that("CLI job cancellation failures do not claim successful termination", {
    private <- new.env(parent = emptyenv())
    private$m_job <- testrun__denied_process()
    private$m_log <- list(backend = "cli")
    expect_false(job_kill(NULL, private))
    expect_false(isTRUE(private$m_log$killed))
})
