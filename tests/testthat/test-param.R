context("Parametric metiods")

test_that("Parametric methods", {
    skip_on_cran()
    eplusr_option(verbose_info = FALSE)

    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    expect_error(param_job(empty_idf(8.8), NULL), class = "eplusr_error_idf_not_local")

    example <- copy_example()

    param <- param_job(example$idf, example$epw)

    priv <- get_priv_env(param)

    expect_equal(param$version(), numeric_version("8.8.0"))
    expect_output(param$print())
    expect_null(param_job(example$idf, NULL)$weather())

    # Seed and Weather {{{
    expect_is(param$seed(), "Idf")
    expect_is(param$weather(), "Epw")
    expect_null(param$models())
    # }}}

    # Measure {{{
    pa <- param_job(example$idf, NULL)
    test <- function(x, y) x
    param$apply_measure(test, 1:5)
    expect_equal(names(param$models()), sprintf("test_%i", 1:5))
    param$apply_measure(function (x, y) x, 1:5)
    expect_equal(names(param$models()), sprintf("case_%i", 1:5))

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
    # names are unique
    param$apply_measure(set_infil_rate, seq(0, 4, by = 1), .names = rep("A", 5))
    expect_equal(names(priv$m_idfs), c("A", paste0("A_", 1:4)))

    # auto assign name
    param$apply_measure(set_infil_rate, seq(0, 4, by = 1), .names = NULL)
    expect_equal(length(priv$m_idfs), 5)
    expect_equal(names(priv$m_idfs), paste0("set_infil_rate_", 1:5))
    expect_equal(unname(vlapply(priv$m_idfs, is_idf)), rep(TRUE, times = 5))
    # }}}

    # Models {{{
    expect_is(param$models(), "list")
    expect_equal(length(param$models()), 5)
    expect_equal(names(param$models()), paste0("set_infil_rate_", 1:5))
    expect_equal(unname(vlapply(priv$m_idfs, is_idf)), rep(TRUE, times = 5))
    # }}}

    # Save {{{
    # can preserve name
    param$apply_measure(set_infil_rate, seq(0, 4, by = 1), .names = 1:5)
    expect_equal(names(param$models()), as.character(1:5))
    expect_silent(paths <- param$save())
    expect_equal(paths,
        data.table::data.table(
            model = normalizePath(file.path(tempdir(), 1:5, paste0(1:5, ".idf"))),
            weather = normalizePath(file.path(tempdir(), 1:5, basename(param$weather()$path())))
        )
    )

    param$apply_measure(set_infil_rate, seq(0, 4, by = 1), .names = NULL)
    expect_silent(paths <- param$save())
    expect_equal(paths,
        data.table::data.table(
            model = normalizePath(file.path(tempdir(), paste0("set_infil_rate_", 1:5), paste0("set_infil_rate_", 1:5, ".idf"))),
            weather = normalizePath(file.path(tempdir(), paste0("set_infil_rate_", 1:5), basename(param$weather()$path())))
        )
    )
    expect_silent(paths <- param$save(separate = FALSE))
    expect_equal(paths,
        data.table::data.table(
            model = normalizePath(file.path(tempdir(), paste0("set_infil_rate_", 1:5, ".idf"))),
            weather = normalizePath(file.path(tempdir(), basename(param$weather()$path())))
        )
    )
    # can save when no weather are provided
    expect_silent(paths <- {
        empty <- empty_idf(8.8)
        empty$save(tempfile(fileext = ".idf"))
        par <- param_job(empty, NULL)
        par$apply_measure(function (idf, x) idf, 1:2, .names = 1:2)
        par$save()
    })
    expect_equal(paths,
        data.table::data.table(
            model = normalizePath(file.path(tempdir(), 1:2, paste0(1:2, ".idf"))),
            weather = NA_character_
        )
    )
    # }}}

    # Run and Status {{{

    # Can detect if models are modified before running
    model2 <- param$models()$set_infil_rate_2
    model2$Output_Variable <- NULL
    expect_warning(param$run(echo = FALSE), class = "warn_param_modified")

    dir_nms <- paste0("set_infil_rate_", 1:5)
    param$apply_measure(set_infil_rate, seq(0, 4, by = 1), .names = NULL)
    # can run the simulation and get status of simulation
    expect_equal({param$run(dir = NULL, echo = FALSE); status <- param$status(); names(status)},
        c("run_before", "alive", "terminated", "successful", "changed_after", "job_status")
    )
    expect_equal(status[c("run_before", "alive", "terminated", "successful", "changed_after")],
        list(run_before = TRUE, alive = FALSE, terminated = FALSE,
            successful = TRUE, changed_after = FALSE
        )
    )
    expect_equal(names(status$job_status),
        c("index", "status", "idf", "epw", "version", "exit_status", "start_time", "end_time",
          "output_dir", "energyplus", "stdout", "stderr"
        )
    )
    # }}}

    # Report Data Dict {{{
    expect_is(param$report_data_dict(), "data.table")
    expect_true(has_names(param$report_data_dict(), "case"))
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
        c("index", "case", "datetime", "month", "day", "hour", "minute", "dst", "interval",
          "simulation_days", "day_type", "environment_name",
          "environment_period_index", "is_meter", "type", "index_group",
          "timestep_type", "key_value", "name", "reporting_frequency",
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
        c("index", "case", "datetime", "month", "day", "hour", "minute", "dst", "interval",
          "simulation_days", "day_type", "environment_name",
          "environment_period_index", "is_meter", "type", "index_group",
          "timestep_type", "key_value", "name", "reporting_frequency",
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

    # S3 {{{
    expect_false(param == 1)
    expect_true(param == param)
    expect_false(param != param)
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
