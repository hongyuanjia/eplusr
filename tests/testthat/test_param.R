context("Parametric metiods")

test_that("Parametric methods", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    example <- copy_example()

    param <- param_job(example$idf, example$epw)

    priv <- ._get_private(param)

    expect_is(param$seed(), "Idf")

    expect_is(param$weather(), "Epw")

    # can apply measure
    # set_infil_rate {{{
    set_infil_rate <- function (idf, infil_rate) {

        # validate input value
        # this is optional, as validations will be made when setting values to `Idf`
        stopifnot(is.numeric(infil_rate), infil_rate >= 0)

        if (!idf$is_valid_class("ZoneInfiltration:DesignFlowRate"))
          stop("Input model does not have any object in class `ZoneInfiltration:DesignFlowRate`")

        ids <- idf$object_id("ZoneInfiltration:DesignFlowRate", simplify = TRUE)

        idf$set_object(ids,
            value = rep(list(list(
                design_flow_rate_calculation_method = "AirChanges/Hour",
                air_changes_per_hour = infil_rate)),
                times = length(ids))
            )

        idf
    }
    # }}}
    param$apply_measure(set_infil_rate, seq(0, 4, by = 1), .names = NULL)
    expect_equal(length(priv$m_param), 5)
    expect_equal(unname(vapply(priv$m_param, is_idf, logical(1))), rep(TRUE, times = 5))

    # # can kill the master background R process
    # expect_message(param$kill(), "job is not running")
    # expect_true({param$run(wait = FALSE);Sys.sleep(1);param$kill()})
    # # can update the status after job was killed
    # expect_true(param$status()$terminated)
    # expect_message(param$kill(), "job is not running")
    # expect_error(param$errors(), "job was terminated before")
    # expect_error(param$locate_output(), "job was terminated before")

    dir_nms <- paste0("set_infil_rate_", 1:5)
    # can run the simulation and get status of simulation
    expect_equal(
        {param$run(dir = NULL); param$status()},
        list(run_before = TRUE, alive = FALSE, terminated = FALSE,
            successful = TRUE, changed_after = FALSE
        )
    )

    # can get report data dictionary
    expect_is(param$report_data_dict(), "data.table")
    expect_true(has_name(param$report_data_dict(), "Case"))
    expect_is(param$report_data_dict(2), "data.table")
    expect_true(has_name(param$report_data_dict(2), "Case"))

    # can get tabular data
    expect_is(param$tabular_data(), "data.table")
    expect_true(has_name(param$tabular_data(), "Case"))
    expect_is(param$tabular_data(2), "data.table")
    expect_true(has_name(param$tabular_data(2), "Case"))

    # can get tabular data
    expect_is(param$tabular_data(), "data.table")
    expect_true(has_name(param$tabular_data(), "Case"))
    expect_is(param$tabular_data(2), "data.table")
    expect_true(has_name(param$tabular_data(2), "Case"))

    # can get report data
    expect_true(has_name(param$report_data(name = "EnergyTransfer:Building"), "Case"))
    expect_equal(unique(param$report_data(name = "EnergyTransfer:Building")$Case), dir_nms)
    expect_equal(
        unique(format(param$report_data(name = "EnergyTransfer:Building", year = 2016L)$DateTime, "%Y")),
        "2016"
    )
    expect_equal(
        attr(param$report_data(name = "EnergyTransfer:Building", year = 2016L, tz = "America/Chicago")$DateTime, "tzone"),
        "America/Chicago"
    )
    expect_true(has_name(param$report_data(2, name = "EnergyTransfer:Building"), "Case"))
    expect_equal(unique(param$report_data(2, name = "EnergyTransfer:Building")$Case), dir_nms[2])
    expect_equal(
        unique(format(param$report_data(2, name = "EnergyTransfer:Building", year = 2016L)$DateTime, "%Y")),
        "2016"
    )
    expect_equal(
        attr(param$report_data(2, name = "EnergyTransfer:Building", year = 2016L, tz = "America/Chicago")$DateTime, "tzone"),
        "America/Chicago"
    )
    expect_true(has_name(param$report_data("set_infil_rate_2", name = "EnergyTransfer:Building"), "Case"))
    expect_equal(unique(param$report_data("set_infil_rate_2", name = "EnergyTransfer:Building")$Case), dir_nms[2])
    expect_equal(
        unique(format(param$report_data("set_infil_rate_2", name = "EnergyTransfer:Building", year = 2016L)$DateTime, "%Y")),
        "2016"
    )
    expect_equal(
        attr(param$report_data("set_infil_rate_2", name = "EnergyTransfer:Building", year = 2016L, tz = "America/Chicago")$DateTime, "tzone"),
        "America/Chicago"
    )

    skip_on_os("mac")
    # can return simulation output path
    expect_equal(param$locate_output(suffix = ".sql"),
        normalizePath(file.path(dirname(example$idf), dir_nms, paste0(dir_nms, ".sql"))))
    expect_equal(param$locate_output(2, suffix = ".sql"),
        normalizePath(file.path(dirname(example$idf), dir_nms[2], paste0(dir_nms[2], ".sql"))))
    expect_equal(param$locate_output("set_infil_rate_2", suffix = ".sql"),
        normalizePath(file.path(dirname(example$idf), dir_nms[2], paste0(dir_nms[2], ".sql"))))

    # can return the output directory
    expect_equal(param$output_dir(),
        normalizePath(file.path(dirname(example$idf), dir_nms)))
    expect_equal(param$output_dir(2),
        normalizePath(file.path(dirname(example$idf), dir_nms[2])))
    expect_equal(param$output_dir("set_infil_rate_2"),
        normalizePath(file.path(dirname(example$idf), dir_nms[2])))

    # clean
    lapply(dir_nms, unlink, recursive = TRUE, force = TRUE)
    unlink(c(example$idf, example$epw))
})
