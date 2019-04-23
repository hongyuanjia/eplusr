context("Parametric metiods")

test_that("Parametric methods", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    example <- copy_example()

    param <- param_job(example$idf, example$epw)

    priv <- ._get_private(param)

    # Seed and Weather {{{
    expect_is(param$seed(), "Idf")
    expect_is(param$weather(), "Epw")
    # }}}

    # Measure {{{
    # set_infil_rate {{{
    set_infil_rate <- function (idf, infil_rate) {

        # validate input value
        # this is optional, as validations will be made when setting values to `Idf`
        stopifnot(is.numeric(infil_rate), infil_rate >= 0)

        if (!idf$is_valid_class("ZoneInfiltration:DesignFlowRate"))
          stop("Input model does not have any object in class `ZoneInfiltration:DesignFlowRate`")

        ids <- idf$object_id("ZoneInfiltration:DesignFlowRate", simplify = TRUE)
        val <- rep(list(list(design_flow_rate_calculation_method = "AirChanges/Hour", air_changes_per_hour = infil_rate)), length(ids))
        setattr(val, "names", paste0("..", ids))
        idf$set(val)

        idf
    }
    # }}}
    param$apply_measure(set_infil_rate, seq(0, 4, by = 1), .names = NULL)
    expect_equal(length(priv$m_param), 5)
    expect_equal(unname(vlapply(priv$m_param, is_idf)), rep(TRUE, times = 5))
    # }}}

    # # can kill the master background R process
    # expect_message(param$kill(), "job is not running")
    # expect_true({param$run(wait = FALSE);Sys.sleep(0.2);param$kill()})
    # # can update the status after job was killed
    # expect_true(param$status()$terminated)
    # expect_message(param$kill(), "job is not running")
    # expect_error(param$errors(), "job was terminated before")
    # expect_error(param$locate_output(), "job was terminated before")

    # Run and Status {{{
    dir_nms <- paste0("set_infil_rate_", 1:5)
    # can run the simulation and get status of simulation
    expect_equal(
        {param$run(dir = NULL); param$status()},
        list(run_before = TRUE, alive = FALSE, terminated = FALSE,
            successful = TRUE, changed_after = FALSE
        )
    )
    # }}}

    # Report Data Dict {{{
    expect_is(param$report_data_dict(), "data.table")
    expect_true(has_name(param$report_data_dict(), "case"))
    expect_equal(nrow(param$report_data_dict(2)), 20)
    expect_equal(nrow(param$report_data_dict("set_infil_rate_2")), 20)
    # }}}

    # Tabular Data {{{
    expect_equal(nrow(param$tabular_data()), 6662 * 5)
    expect_equal(nrow(param$tabular_data(
        report_name = c(
            "AnnualBuildingUtilityPerformanceSummary",
            "Initialization Summary"
        ))),
        3774 * 5
    )
    expect_equal(nrow(param$tabular_data(table_name = "Site and Source Energy")), 12 * 5)
    expect_equal(nrow(param$tabular_data(column_name = "Total Energy")), 4 * 5)
    expect_equal(nrow(param$tabular_data(row_name = "Total Site Energy")), 3 * 5)
    expect_equal(nrow(param$tabular_data(2)), 6662)
    expect_equal(nrow(param$tabular_data(2,
        report_name = c(
            "AnnualBuildingUtilityPerformanceSummary",
            "Initialization Summary"
        ))),
        3774
    )
    expect_equal(nrow(param$tabular_data(2, table_name = "Site and Source Energy")), 12)
    expect_equal(nrow(param$tabular_data(2, column_name = "Total Energy")), 4)
    expect_equal(nrow(param$tabular_data(2, row_name = "Total Site Energy")), 3)
    expect_equal(nrow(param$tabular_data("set_infil_rate_2")), 6662)
    expect_equal(nrow(param$tabular_data("set_infil_rate_2",
        report_name = c(
            "AnnualBuildingUtilityPerformanceSummary",
            "Initialization Summary"
        ))),
        3774
    )
    expect_equal(nrow(param$tabular_data("set_infil_rate_2", table_name = "Site and Source Energy")), 12)
    expect_equal(nrow(param$tabular_data("set_infil_rate_2" ,column_name = "Total Energy")), 4)
    expect_equal(nrow(param$tabular_data("set_infil_rate_2", row_name = "Total Site Energy")), 3)
    # }}}

    # Report Data {{{
    expect_equal(nrow(param$report_data(2, param$report_data_dict())), 3840)
    expect_equal(nrow(param$report_data(2)), 3840)
    expect_equal(nrow(param$report_data(2, "")), 1344L)
    expect_equal(nrow(param$report_data(2,
        "TRANSFORMER 1", "Transformer Load Loss Rate")),
        192L
    )
    expect_equal(nrow(param$report_data(2,
        "TRANSFORMER 1", "Transformer Load Loss Rate")),
        192L
    )
    expect_equal(year(param$report_data(2,
        "TRANSFORMER 1", "Transformer Load Loss Rate", year = 2010)$datetime),
        rep(2010, 192)
    )
    expect_equal(lubridate::tz(param$report_data(2, tz = "Asia/Shanghai")$datetime),
        "Asia/Shanghai"
    )
    expect_equal(names(param$report_data(2, all = TRUE)),
        c("case", "datetime", "month", "day", "hour", "minute", "dst", "interval",
          "simulation_days", "day_type", "environment_name", "is_meter", "type",
          "index_group", "timestep_type", "key_value", "name", "reporting_frequency",
          "schedule_name", "units", "value"
        )
    )
    expect_equal(nrow(param$report_data(2, period = seq(
        lubridate::ymd_hms("2019-01-14 0:0:0"), lubridate::ymd_hms("2019-01-15 0:0:0"), "15 min")
    )), 1900)
    expect_equal(nrow(param$report_data(2, month = 1)), 1920)
    expect_equal(nrow(param$report_data(2, month = 1, hour = 1)), 80)
    expect_equal(nrow(param$report_data(2, minute = 0)), 960)
    expect_equal(nrow(param$report_data(2, interval = 15)), 3840)
    expect_equal(nrow(param$report_data(2, simulation_days = 1)), 3840)
    expect_equal(nrow(param$report_data(2, day_type = "Tuesday")), 3840)
    expect_equal(nrow(param$report_data(2, environment_name = "WINTERDAY")), 1920)

    expect_equal(nrow(param$report_data(NULL, param$report_data_dict())), 3840 * 5)
    expect_equal(nrow(param$report_data()), 3840 * 5)
    expect_equal(nrow(param$report_data(NULL, "")), 1344L * 5)
    expect_equal(nrow(param$report_data(NULL,
        "TRANSFORMER 1", "Transformer Load Loss Rate")),
        192L * 5
    )
    expect_equal(nrow(param$report_data(NULL,
        "TRANSFORMER 1", "Transformer Load Loss Rate")),
        192L * 5
    )
    expect_equal(year(param$report_data(NULL,
        "TRANSFORMER 1", "Transformer Load Loss Rate", year = 2010)$datetime),
        rep(2010, 192 * 5)
    )
    expect_equal(lubridate::tz(param$report_data(NULL, tz = "Asia/Shanghai")$datetime),
        "Asia/Shanghai"
    )
    expect_equal(names(param$report_data(all = TRUE)),
        c("case", "datetime", "month", "day", "hour", "minute", "dst", "interval",
          "simulation_days", "day_type", "environment_name", "is_meter", "type",
          "index_group", "timestep_type", "key_value", "name", "reporting_frequency",
          "schedule_name", "units", "value"
        )
    )
    expect_equal(nrow(param$report_data(period = seq(
        lubridate::ymd_hms("2019-01-14 0:0:0"), lubridate::ymd_hms("2019-01-15 0:0:0"), "15 min")
    )), 1900 * 5)
    expect_equal(nrow(param$report_data(month = 1)), 1920 * 5)
    expect_equal(nrow(param$report_data(month = 1, hour = 1)), 80 * 5)
    expect_equal(nrow(param$report_data(minute = 0)), 960 * 5)
    expect_equal(nrow(param$report_data(interval = 15)), 3840 * 5)
    expect_equal(nrow(param$report_data(simulation_days = 1)), 3840 * 5)
    expect_equal(nrow(param$report_data(day_type = "Tuesday")), 3840 * 5)
    expect_equal(nrow(param$report_data(environment_name = "WINTERDAY")), 1920 * 5)
    # }}}

    skip_on_os("mac")
    # Locate Output {{{
    expect_equal(param$locate_output(suffix = ".sql"),
        normalizePath(file.path(dirname(example$idf), dir_nms, paste0(dir_nms, ".sql"))))
    expect_equal(param$locate_output(2, suffix = ".sql"),
        normalizePath(file.path(dirname(example$idf), dir_nms[2], paste0(dir_nms[2], ".sql"))))
    expect_equal(param$locate_output("set_infil_rate_2", suffix = ".sql"),
        normalizePath(file.path(dirname(example$idf), dir_nms[2], paste0(dir_nms[2], ".sql"))))
    # }}}

    # Output Dir {{{
    expect_equal(param$output_dir(),
        normalizePath(file.path(dirname(example$idf), dir_nms)))
    expect_equal(param$output_dir(2),
        normalizePath(file.path(dirname(example$idf), dir_nms[2])))
    expect_equal(param$output_dir("set_infil_rate_2"),
        normalizePath(file.path(dirname(example$idf), dir_nms[2])))
    # }}}

    # clean
    lapply(dir_nms, unlink, recursive = TRUE, force = TRUE)
    unlink(c(example$idf, example$epw))
})
