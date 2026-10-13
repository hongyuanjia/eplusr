test_that("Group methods", {
    skip_on_cran()
    skip_if_not_integration()

    eplusr_option(verbose_info = FALSE)

    path_idfs <- path_eplus_example(LATEST_EPLUS_VER,
        c("1ZoneEvapCooler.idf",
          "1ZoneParameterAspect.idf"
        )
    )
    path_epws <- path_eplus_weather(LATEST_EPLUS_VER,
        c("USA_CO_Golden-NREL.724666_TMY3.epw",
          "USA_FL_Tampa.Intl.AP.722110_TMY3.epw"
        )
    )

    expect_error(group_job(empty_idf(LATEST_EPLUS_VER)), "local", class = "eplusr_error")
    # can stop if input model is not saved after modification
    expect_error(
        group_job(
            list(
                {idf <- read_idf(path_idfs[[1]]); idf$RunPeriod <- NULL; idf},
                path_idfs[1]
            ),
            NULL
        ),
        "save",
        class = "eplusr_error"
    )
    expect_silent(group_job(path_idfs, path_epws[1L]))
    expect_silent(group_job(path_idfs[1], path_epws))
    expect_silent(grp <- group_job(path_idfs, NULL))
    expect_equal(grp$status(),
        list(run_before = FALSE, alive = FALSE, terminated = NA,
            successful = NA, changed_after = NA,
            job_status = data.table(index = 1:2, status = "idle",
                idf = path_idfs, epw = NA_character_
            )
        )
    )

    # Run and Status {{{
    # can run in the same folder
    expect_equal({
        grp$run(dir = file.path(tempdir(), "test"), separate = FALSE)
        basename(grp$status()$job_status$output_dir)
    }, rep("test", 2L))
    # can run in different folders
    expect_equal({
        grp$run(dir = file.path(tempdir(), "test"), separate = TRUE, echo = FALSE)
        status <- grp$status()
        basename(status$job_status$output_dir)
    }, tools::file_path_sans_ext(basename(path_idfs)))

    # Reuse the final real run for status and output getters instead of running
    # the same two-model group a third time.
    expect_equal(names(status),
        c("run_before", "alive", "terminated", "successful", "changed_after", "job_status")
    )
    expect_equal(status[c("run_before", "alive", "terminated", "successful", "changed_after")],
        list(run_before = TRUE, alive = FALSE, terminated = FALSE,
            successful = FALSE, changed_after = FALSE
        )
    )
    expect_equal(names(status$job_status),
        c("index", "status", "idf", "epw", "version", "exit_status", "start_time", "end_time",
          "output_dir", "energyplus", "stdout", "stderr"
        )
    )
    expect_equal(status$job_status$exit_status, c(0L, 1L))
    # }}}

    # Errors {{{
    expect_silent(grp$errors(1))
    expect_warning(grp$errors(2), class = "eplusr_warning_job_error")
    # }}}

    # Output Dir{{{
    expect_silent(grp$output_dir(1))
    expect_warning(grp$output_dir(2), class = "eplusr_warning_job_error")
    # }}}

    # Table {{{
    expect_error(grp$list_table())
    expect_silent(lsts <- grp$list_table(1))
    expect_type(lsts, "list")
    expect_equal(length(lsts), 1L)

    expect_error(grp$read_table())
    expect_silent(tables <- grp$read_table(1, "Zones"))
    expect_equal(names(tables)[1:2], c("index", "case"))
    # }}}

    # RDD & MDD {{{
    expect_error(grp$read_rdd(2))
    expect_silent(rdds <- grp$read_rdd(1))
    expect_s3_class(rdds, "data.table")
    expect_error(grp$read_mdd(2))
    expect_silent(mdds <- grp$read_mdd(1))
    expect_s3_class(mdds, "data.table")
    # }}}

    # Report Data Dict {{{
    expect_error(grp$report_data_dict(), class = "eplusr_error_job_error")
    expect_s3_class(grp$report_data_dict(1), "data.table")
    expect_true(has_names(grp$report_data_dict(1), "index"))
    expect_true(has_names(grp$report_data_dict(1), "case"))
    expect_equal(nrow(grp$report_data_dict(1)), 23)
    expect_equal(nrow(grp$report_data_dict("1zoneevapcooler")), 23)
    # }}}

    # Tabular Data {{{
    expect_equal(nrow(grp$tabular_data(1)), 4768L)
    expect_equal(nrow(grp$tabular_data(1,
        report_name = c(
            "AnnualBuildingUtilityPerformanceSummary",
            "Initialization Summary"
        ))),
        829L
    )
    expect_equal(nrow(grp$tabular_data(1, table_name = "Site and Source Energy")), 12)
    expect_equal(nrow(grp$tabular_data(1, column_name = "Total Energy")), 4)
    expect_equal(nrow(grp$tabular_data(1, row_name = "Total Site Energy")), 3)
    expect_equal(nrow(grp$tabular_data(1)), 4768)
    expect_equal(nrow(grp$tabular_data(1,
        report_name = c(
            "AnnualBuildingUtilityPerformanceSummary",
            "Initialization Summary"
        ))),
        829L
    )
    expect_equal(nrow(grp$tabular_data("1zoneevapcooler", table_name = "Site and Source Energy")), 12)
    expect_equal(nrow(grp$tabular_data("1zoneevapcooler", column_name = "Total Energy")), 4)
    expect_equal(nrow(grp$tabular_data("1zoneevapcooler", row_name = "Total Site Energy")), 3)
    # can convert to wide table
    expect_silent(tab <- grp$tabular_data("1zoneevapcooler", row_name = "Total Site Energy", wide = TRUE))
    expect_equal(names(tab), "AnnualBuildingUtilityPerformanceSummary.Entire Facility.Site and Source Energy")
    expect_equal(
        ignore_attr = TRUE,
        tab[[1L]][, lapply(.SD, class)],
        data.table(
            index = "integer",
            case = "character",
            report_name = "character",
            report_for = "character",
            table_name = "character",
            row_name = "character",
            `Total Energy [GJ]` = "numeric",
            `Energy Per Total Building Area [MJ/m2]` = "numeric",
            `Energy Per Conditioned Building Area [MJ/m2]` = "numeric"
        )
    )
    # }}}

    # Report Data {{{
    expect_equal(nrow(grp$report_data(1, grp$report_data_dict(1))), 920)
    expect_equal(nrow(grp$report_data(1)), 920)
    expect_equal(nrow(grp$report_data(1, name = c("Electricity:Facility", "Electricity:HVAC"))), 8)
    expect_equal(lubridate::tz(grp$report_data(1, tz = "Asia/Shanghai")$datetime),
        "Asia/Shanghai"
    )
    expect_equal(names(grp$report_data(1, all = TRUE)),
        c("index", "case", "datetime", "month", "day", "hour", "minute", "dst", "interval",
          "simulation_days", "day_type", "environment_name",
          "environment_period_index", "is_meter", "type", "index_group",
          "timestep_type", "key_value", "name", "reporting_frequency",
          "schedule_name", "units", "value"
        )
    )

    expect_equal(nrow(grp$report_data(1, period = seq(
        lubridate::ymd_hms("2019-12-21 1:0:0"), lubridate::ymd_hms("2019-12-22 0:0:0"), "1 hour")
    )), 437)
    expect_equal(nrow(grp$report_data(1, month = 12)), 460)
    expect_equal(nrow(grp$report_data(1, month = 12, hour = 1)), 19)
    expect_equal(nrow(grp$report_data(1, minute = 0)), 920)
    # See https://github.com/NREL/EnergyPlus/issues/8367
    expect_equal(nrow(grp$report_data(1, interval = 60)), 920)
    expect_equal(nrow(grp$report_data(1, simulation_days = 1)), 920)
    expect_equal(nrow(grp$report_data(1, day_type = "WinterDesignDay")), 460)
    expect_equal(nrow(grp$report_data(1, environment_name = "DENVER CENTENNIAL ANN HTG 99.6% CONDNS DB")), 460)
    # }}}

    # S3 {{{
    expect_output(grp$print())
    expect_true(grp == grp)
    expect_false(grp != grp)
    # }}}

    skip_on_os("mac")
    # Locate Output {{{
    expect_error(grp$locate_output(suffix = ".sql"))
    expect_equal(grp$locate_output(1, suffix = ".sql"),
        normalizePath(file.path(tempdir(), "test",
            tools::file_path_sans_ext(basename(path_idfs[1])),
            paste0(tools::file_path_sans_ext(basename(path_idfs[1])), ".sql")
        ))
    )
    # }}}

    # List files {{{
    expect_type(files <- grp$list_files(1, simplify = TRUE), "list")
    expect_equal(length(files), 1L)
    expect_equal(length(files[[1]]), 21L)

    expect_type(files <- grp$list_files(1, simplify = TRUE, full = TRUE), "list")
    expect_equal(length(files), 1L)
    expect_equal(normalizePath(dirname(files[[1]])), rep(grp$output_dir(1), 21L))

    expect_s3_class(files <- grp$list_files(1, simplify = FALSE), "data.table")
    expect_equal(names(files), c("index", "type", "file"))
    expect_equal(nrow(files), 57L)

    expect_s3_class(files <- grp$list_files(1, simplify = FALSE, full = TRUE), "data.table")
    expect_equal(names(files), c("index", "type", "file"))
    expect_equal(nrow(files), 57L)
    # }}}
})

# vim: set fdm=marker:

test_that("stock API group jobs keep parallel worker identity", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    previous <- eplusr_option(num_parallel = 2L)
    on.exit(do.call(eplusr_option, previous), add = TRUE)
    group <- group_job(rep(fixture$model, 2L), fixture$weather)
    group$run(
        dir = file.path(fixture$directory, c("case-a", "case-b")),
        backend = "api",
        echo = FALSE,
        readvars = FALSE
    )
    statuses <- group$status()$job_status
    expect_true(all(statuses$status == "completed"))
    expect_identical(
        length(unique(vapply(statuses$api, "[[", integer(1L), "pid"))),
        2L
    )
})
test_that("killing an API group terminates its worker tree and preserves every case status", {
    skip_if_not_installed("ps")
    fixture <- testrun__api_fixture()
    marker <- tempfile()
    dir.create(marker)
    setup <- local({
        directory <- marker
        function(api, state) {
            api$runtime$callback_begin_new_environment(state, function(state) {
                writeLines(
                    as.character(Sys.getpid()),
                    file.path(directory, paste0(Sys.getpid(), ".ready"))
                )
                Sys.sleep(60)
            })
            writeLines(
                "started",
                file.path(directory, paste0(Sys.getpid(), ".started"))
            )
        }
    })
    old <- eplusr_option(num_parallel = 2L)
    on.exit(do.call(eplusr_option, old), add = TRUE)
    group <- group_job(rep(fixture$model, 2L), fixture$weather)
    group$run(
        dir = file.path(fixture$directory, c("kill-a", "kill-b")),
        backend = "api",
        api_setup = setup,
        wait = FALSE,
        echo = FALSE,
        readvars = FALSE
    )
    on.exit(group$kill(), add = TRUE)
    testrun__api_started(
        function() length(list.files(marker, pattern = "[.]started$")) == 2L,
        function() group$status()$alive
    )
    deadline <- Sys.time() + 30
    while (
        length(list.files(marker, pattern = "[.]ready$")) < 2L &&
            Sys.time() < deadline
    ) {
        Sys.sleep(0.1)
    }
    expect_length(list.files(marker, pattern = "[.]ready$"), 2L)
    handles <- lapply(
        list.files(marker, pattern = "[.]ready$", full.names = TRUE),
        function(path) {
            ps::ps_handle(as.integer(readLines(path)))
        }
    )
    expect_true(group$kill())
    status <- group$status()
    expect_true(status$terminated)
    expect_false(status$alive)
    expect_false(status$successful)
    expect_true(all(status$job_status$status == "terminated"))
    expect_true(all(
        vapply(status$job_status$api, "[[", character(1L), "stop_reason") ==
            "process_terminated"
    ))
    deadline <- Sys.time() + 10
    while (
        any(vapply(handles, ps::ps_is_running, logical(1L))) &&
            Sys.time() < deadline
    ) {
        Sys.sleep(0.1)
    }
    expect_false(any(vapply(handles, ps::ps_is_running, logical(1L))))
})

test_that("foreground API Group conditions retain every failed case before rethrow", {
    fixture <- testrun__api_fixture()
    condition <- NULL
    local_mocked_bindings(run_sim_event_loop = function(state) stop(condition))
    group <- group_job(rep(fixture$model, 2L), fixture$weather)
    for (cause in list(
        simpleError("foreground group failure"),
        structure(
            list(message = "foreground group interrupt", call = NULL),
            class = c("interrupt", "condition")
        )
    )) {
        condition <- cause
        caught <- tryCatch(
            group$run(
                dir = file.path(fixture$directory, c("failed-a", "failed-b")),
                backend = "api",
                echo = FALSE,
                readvars = FALSE
            ),
            error = identity,
            interrupt = identity
        )
        expect_identical(caught, cause)
        status <- group$status()
        expect_true(status$run_before)
        expect_false(status$successful)
        expect_identical(status$terminated, inherits(cause, "interrupt"))
        expect_equal(nrow(status$job_status), 2L)
        expect_true(all(
            vapply(status$job_status$api, "[[", character(1L), "stop_reason") ==
                if (inherits(cause, "interrupt")) {
                    "interrupt"
                } else {
                    "pipeline_error"
                }
        ))
    }
})


test_that("CLI group cancellation failures retain their previous status", {
    private <- new.env(parent = emptyenv())
    private$m_job <- testrun__denied_process()
    private$m_log <- list()
    expect_false(epgroup_kill(NULL, private))
    expect_false(isTRUE(private$m_log$killed))
})
