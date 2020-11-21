context("Group metiods")

test_that("Group methods", {
    skip_on_cran()
    eplusr_option(verbose_info = FALSE)

    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    path_idfs <- normalizePath(file.path(eplus_config(8.8)$dir, "ExampleFiles",
        c("1ZoneDataCenterCRAC_wPumpedDXCoolingCoil.idf",
          "1ZoneEvapCooler.idf",
          "1ZoneParameterAspect.idf",
          "1ZoneUncontrolled_DD2009.idf",
          "1ZoneUncontrolled_DDChanges.idf"
        )
    ))
    path_epws <- normalizePath(list.files(file.path(eplus_config(8.8)$dir, "WeatherData"),
        "\\.epw", full.names = TRUE)[1:5])

    expect_error(group_job(empty_idf(8.8)), "local", class = "eplusr_error")
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
            job_status = data.table(index = 1:5, status = "idle",
                idf = path_idfs, epw = NA_character_
            )
        )
    )

    # Run and Status {{{
    # can run in the same folder
    expect_equal({
        grp$run(dir = file.path(tempdir(), "test"), separate = FALSE)
        basename(grp$status()$job_status$output_dir)
    }, rep("test", 5L))
    # can run in different folders
    expect_equal({
        grp$run(dir = file.path(tempdir(), "test"), separate = TRUE)
        basename(grp$status()$job_status$output_dir)
    }, tools::file_path_sans_ext(basename(path_idfs)))

    # can run the simulation and get status of simulation
    expect_equal({grp$run(dir = file.path(tempdir(), "test"), echo = FALSE); status <- grp$status(); names(status)},
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
    expect_equal(status$job_status$exit_status, c(0L, 0L, 1L, 0L, 0L))
    # }}}

    # Errors {{{
    expect_silent(grp$errors(2))
    expect_warning(grp$errors(3), class = "eplusr_warning_job_error")
    # }}}

    # Output Dir{{{
    expect_silent(grp$output_dir(1))
    expect_warning(grp$output_dir(3), class = "eplusr_warning_job_error")
    # }}}

    # Table {{{
    expect_error(grp$list_table())
    expect_silent(lsts <- grp$list_table(c(1,2,4)))
    expect_is(lsts, "list")
    expect_equal(length(lsts), 3L)

    expect_error(grp$read_table())
    expect_silent(tables <- grp$read_table(c(1, 2, 4), "Zones"))
    expect_equal(names(tables)[1:2], c("index", "case"))
    # }}}

    # RDD & MDD {{{
    expect_error(grp$read_rdd(3))
    expect_silent(rdds <- grp$read_rdd(c(1,2,4)))
    expect_is(rdds, "data.table")
    expect_error(grp$read_mdd(3))
    expect_silent(mdds <- grp$read_mdd(c(1,2,4)))
    expect_is(mdds, "data.table")
    # }}}

    # Report Data Dict {{{
    expect_error(grp$report_data_dict(), class = "eplusr_error_job_error")
    expect_is(grp$report_data_dict(c(1,2,4,5)), "data.table")
    expect_true(has_names(grp$report_data_dict(c(1,2,4,5)), "case"))
    expect_equal(nrow(grp$report_data_dict(2)), 22)
    expect_equal(nrow(grp$report_data_dict("1zoneevapcooler")), 22)
    # }}}

    # Tabular Data {{{
    expect_equal(nrow(grp$tabular_data(c(1,2,4,5))), 14668)
    expect_equal(nrow(grp$tabular_data(c(1,2,4,5),
        report_name = c(
            "AnnualBuildingUtilityPerformanceSummary",
            "Initialization Summary"
        ))),
        9032
    )
    expect_equal(nrow(grp$tabular_data(c(1,2,4,5), table_name = "Site and Source Energy")), 12 * 4)
    expect_equal(nrow(grp$tabular_data(c(1,2,4,5), column_name = "Total Energy")), 4 * 4)
    expect_equal(nrow(grp$tabular_data(c(1,2,4,5), row_name = "Total Site Energy")), 3 * 4)
    expect_equal(nrow(grp$tabular_data(2)), 2172)
    expect_equal(nrow(grp$tabular_data(2,
        report_name = c(
            "AnnualBuildingUtilityPerformanceSummary",
            "Initialization Summary"
        ))),
        769
    )
    expect_equal(nrow(grp$tabular_data("1zoneevapcooler", table_name = "Site and Source Energy")), 12)
    expect_equal(nrow(grp$tabular_data("1zoneevapcooler" ,column_name = "Total Energy")), 4)
    expect_equal(nrow(grp$tabular_data("1zoneevapcooler", row_name = "Total Site Energy")), 3)
    # can convert to wide table
    expect_silent(tab <- grp$tabular_data("1zoneevapcooler", row_name = "Total Site Energy", wide = TRUE))
    expect_equal(names(tab), "AnnualBuildingUtilityPerformanceSummary.Entire Facility.Site and Source Energy")
    expect_equivalent(tab[[1L]][, lapply(.SD, class)],
        data.table(
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
    expect_equal(nrow(grp$report_data(2, grp$report_data_dict(2))), 872)
    expect_equal(nrow(grp$report_data(2)), 872)
    expect_equal(nrow(grp$report_data(2, "")), 8)
    expect_equal(lubridate::tz(grp$report_data(2, tz = "Asia/Shanghai")$datetime),
        "Asia/Shanghai"
    )
    expect_equal(names(grp$report_data(2, all = TRUE)),
        c("index", "case", "datetime", "month", "day", "hour", "minute", "dst", "interval",
          "simulation_days", "day_type", "environment_name",
          "environment_period_index", "is_meter", "type", "index_group",
          "timestep_type", "key_value", "name", "reporting_frequency",
          "schedule_name", "units", "value"
        )
    )

    expect_equal(nrow(grp$report_data(2, period = seq(
        lubridate::ymd_hms("2019-12-21 1:0:0"), lubridate::ymd_hms("2019-12-22 0:0:0"), "1 hour")
    )), 414)
    expect_equal(nrow(grp$report_data(2, month = 12)), 436)
    expect_equal(nrow(grp$report_data(2, month = 12, hour = 1)), 18)
    expect_equal(nrow(grp$report_data(2, minute = 0)), 872)
    # See https://github.com/NREL/EnergyPlus/issues/8367
    expect_equal(nrow(grp$report_data(2, interval = 60)), 872)
    expect_equal(nrow(grp$report_data(2, simulation_days = 1)), 872)
    expect_equal(nrow(grp$report_data(2, day_type = "WinterDesignDay")), 436)
    expect_equal(nrow(grp$report_data(2, environment_name = "DENVER CENTENNIAL ANN HTG 99.6% CONDNS DB")), 436)
    # }}}

    # S3 {{{
    expect_output(grp$print())
    expect_true(grp == grp)
    expect_false(grp != grp)
    # }}}

    skip_on_os("mac")
    # Locate Output {{{
    expect_error(grp$locate_output(suffix = ".sql"))
    expect_equal(grp$locate_output(2, suffix = ".sql"),
        normalizePath(file.path(tempdir(), "test",
            tools::file_path_sans_ext(basename(path_idfs[2])),
            paste0(tools::file_path_sans_ext(basename(path_idfs[2])), ".sql")
        ))
    )
    # }}}
})
