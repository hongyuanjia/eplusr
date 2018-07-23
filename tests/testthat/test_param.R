context("Parametric methods")

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

test_that("can create a `ParametricJob` object", {
    expect_silent(param <- param_job(path_idf, path_weather))
})

param <- param_job(path_idf, path_weather)

priv <- ._get_private(param)

test_that("can return the seed model", {
    expect_is(param$seed(), "Idf")
})

test_that("can return the weather", {
    expect_is(param$weather(), "Epw")
})

test_that("can apply a measure", {
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
})

dir_nms <- paste0("set_infil_rate_", 1:5)

test_that("can run the simulation", {
    expect_output(param$run())
})

test_that("can get status of simulation", {
    res <- replicate(5, list(run_before = TRUE, changed_after = FALSE, terminated = FALSE,
            successful = TRUE, alive = FALSE, wait = TRUE), simplify = FALSE)
    names(res) <- dir_nms
    expect_equal(param$status(), res)
})

test_that("can return the output directory", {
    expect_equal(unname(param$output_dir()), file.path(dirname(path_idf), dir_nms))
})

test_that("can return simulation errors", {
    expect_equal(unname(param$locate_output(suffix = ".sql")),
        normalizePath(file.path(dirname(path_idf), dir_nms, paste0(dir_nms, ".sql"))))
})

# clean
lapply(dir_nms, unlink, recursive = TRUE, force = TRUE)
unlink(path_idf)
