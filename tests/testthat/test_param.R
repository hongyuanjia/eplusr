context("Parametric metiods")

test_that("Parametric methods", {
    skip_on_cran()

    install_ep88()
    example <- copy_example()

    param <- param_job(example$idf, example$epw)

    priv <- ._get_private(param)

    expect_is(param$seed(), "Idf")

    expect_is(param$weather(), "Epw")

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

    dir_nms <- paste0("set_infil_rate_", 1:5)

    # can run the simulation
    expect_output(param$run())

    # can get status of simulation
    res <- replicate(5, list(run_before = TRUE, changed_after = FALSE, terminated = FALSE,
            successful = TRUE, alive = FALSE, wait = TRUE), simplify = FALSE)
    names(res) <- dir_nms
    expect_equal(param$status(), res)

    # can return the output directory
    expect_equal(unname(param$output_dir()), file.path(dirname(example$idf), dir_nms))

    # can return simulation errors
    expect_equal(unname(param$locate_output(suffix = ".sql")),
        normalizePath(file.path(dirname(example$idf), dir_nms, paste0(dir_nms, ".sql"))))

    # clean
    lapply(dir_nms, unlink, recursive = TRUE, force = TRUE)
    unlink(c(example$idf, example$epw))
})
