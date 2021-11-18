test_that("Parametric methods", {
    skip_on_cran()

    # can stop if idf is not saved
    expect_error(param_job(empty_idf(8.8), NULL), class = "eplusr_error_idf_not_local")

    path_idf <- copy_eplus_example(8.8, "5Zone_Transformer.idf")
    path_epw <- path_eplus_weather(8.8, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    param <- param_job(path_idf, path_epw)

    # $version()
    expect_equal(param$version(), numeric_version("8.8.0"))

    # $seed()
    expect_is(param$seed(), "Idf")

    # $weather()
    expect_is(param$weather(), "Epw")
    expect_null(param_job(path_idf, NULL)$weather())
})

test_that("$apply_measure()", {
    skip_on_cran()

    path_idf <- copy_eplus_example(8.8, "5Zone_Transformer.idf")
    param <- param_job(path_idf, NULL)

    mea <- function(idf, num) idf

    expect_error(param$apply_measure(function(idf) idf))
    expect_error(param$apply_measure(function(idf, ...) 1, a = 1:5))
    expect_is(param$apply_measure(mea, num = 1:2, .names = "case"), "ParametricJob")
    expect_equal(names(param$models()), c("case_1", "case_2"))
    expect_error(param$apply_measure(mea, num = 1:2, .names = c("a", "b", "c")))

    eplusr_option(verbose_info = FALSE)
    param <- param_job(path_idf, NULL)
    eplusr_option(verbose_info = TRUE)
    expect_message(param$apply_measure(function(idf, num) idf, num = 1:2), "function")
    expect_message(param$apply_measure(mea, num = 1:2), "mea")
})

test_that("$param()", {
    skip_on_cran()

    path <- copy_eplus_example(8.8, "5Zone_Transformer.idf")
    param <- param_job(path, NULL)

    expect_is(class = "ParametricJob",
        param$param("Supply Fan 1" = list(fan_total_efficiency = c(0.1, 0.5, 0.8)), .cross = TRUE)
    )
    expect_equal(length(param$models()), 3L)

    expect_is(class = "ParametricJob",
        param$param(
            Material := list(Thickness = seq(0.1, 1, length.out = 3), Conductivity = seq(0.1, 0.6, length.out = 3)),
            "Supply Fan 1" = list(fan_total_efficiency = c(0.1, 0.5, 0.8))
        )
    )
    expect_equal(length(param$models()), 3L)

    expect_is(class = "ParametricJob",
        param$param(
            Material := list(Thickness = seq(0.1, 1, length.out = 3), Conductivity = seq(0.1, 0.6, length.out = 3)),
            "Supply Fan 1" = list(fan_total_efficiency = c(0.1, 0.5, 0.8)),
            .cross = TRUE
        )
    )
    expect_equal(length(param$models()), 27L)

    expect_error(
        param$param(
            "Supply Fan 1" = list(fan_total_efficiency = c(0.1, 0.5, 0.8)),
            .names = c("p1", "p2")
        )
    )
    expect_is(class = "ParametricJob",
        param$param(
            "Supply Fan 1" = list(fan_total_efficiency = c(0.1, 0.5, 0.8)),
            .names = "fan_eff"
        )
    )
})

test_that("$models()", {
    skip_on_cran()

    eplusr_option(verbose_info = FALSE)
    path_idf <- copy_eplus_example(8.8, "5Zone_Transformer.idf")
    param <- param_job(path_idf, NULL)

    expect_null(param$models())
    expect_null(param$models("new name"))

    param$apply_measure(function(idf, num) idf, num = 1:2)

    expect_is(param$models(), "list")
    expect_equal(length(param$models()), 2)
    expect_equal(names(param$models()), c("case_1", "case_2"))

    expect_equal(names(param$models(names = "model")), c("model_1", "model_2"))
    expect_error(names(param$models(names = c("a", "b", "c"))), "names")
})

test_that("$cases()", {
    skip_on_cran()

    path <- copy_eplus_example(8.8, "5Zone_Transformer.idf")

    param <- param_job(path, NULL)

    expect_null(param$cases())

    param$param(
        "Supply Fan 1" = list(
            fan_total_efficiency = c(0.1, 0.2),
            availability_schedule_name = c("FanAvailSched", "Always On")
        ),
        c("GP01", "GP02") := .(thickness = c(0.01, 0.02))
    )
    expect_equivalent(param$cases(),
        data.table(
            index = 1:2, case = c("case_1", "case_2"),
            param_1 = c(0.1, 0.2),
            param_2 = c("FanAvailSched", "Always On"),
            param_3 = c(0.01, 0.02)
        )
    )

    param$apply_measure(
        function(idf, eff, sch) {
            idf$set("Supply Fan 1" = list(fan_total_efficiency = eff, availability_schedule_name = sch))
            idf
        },
        c("FanAvailSched", "Always On"),
        eff = c(0.1, 0.2)
    )
    expect_equivalent(param$cases(),
        data.table(index = 1:2, case = c("case_1", "case_2"),
            eff = c(0.1, 0.2), sch = c("FanAvailSched", "Always On")
        )
    )
})

test_that("$run()", {
    skip_on_cran()

    path <- copy_eplus_example(8.8, "5Zone_Transformer.idf")
    param <- param_job(path, NULL)

    expect_error(param$run(), "No measure")

    param$apply_measure(function(idf, num) idf, num = 1:2)
    param$models()[[1L]]$set(Material := list(thickness = 0.05))
    expect_warning(param$run())

    path_epw <- path_eplus_weather(8.8, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")
    param <- param_job(path, path_epw)
    param$apply_measure(function(idf, num) idf, num = 1:2)
    expect_is(param$run(), "ParametricJob")
})

test_that("$save()", {
    skip_on_cran()

    path <- copy_eplus_example(8.8, "5Zone_Transformer.idf")
    param <- param_job(path, NULL)
    expect_error(param$save())

    param$apply_measure(function(idf, num) idf, num = 1:2)
    expect_equivalent(param$save(),
        data.table(
            model = normalizePath(file.path(tempdir(), c("case_1", "case_2"), c("case_1.idf", "case_2.idf"))),
            weather = NA_character_
        )
    )

    param <- param_job(path, path_eplus_weather(8.8, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"))
    param$apply_measure(function(idf, num) idf, num = 1:2)
    expect_equivalent(param$save(separate = FALSE),
        data.table(
            model = normalizePath(file.path(tempdir(), c("case_1.idf", "case_2.idf"))),
            weather = normalizePath(file.path(tempdir(), "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"))
        )
    )
})

test_that("$print()", {
    skip_on_cran()

    path <- copy_eplus_example(8.8, "5Zone_Transformer.idf")
    param <- param_job(path, NULL)
    expect_output(param$print())

    mea <- function(idf, num) idf
    param$apply_measure(mea, num = 1:2)
    expect_output(param$print())
})

test_that("==.ParametricJob and !=.ParametricJob", {
    skip_on_cran()

    path <- copy_eplus_example(8.8, "5Zone_Transformer.idf")
    param <- param_job(path, NULL)

    expect_false(param == 1)
    expect_true(param == param)
    expect_false(param != param)
})
