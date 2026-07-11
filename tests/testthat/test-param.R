test_that("Parametric methods", {
    skip_on_cran()

    # can stop if idf is not saved
    expect_error(param_job(empty_idf(LATEST_EPLUS_VER), NULL), class = "eplusr_error_idf_not_local")

    path_idf <- copy_eplus_example(LATEST_EPLUS_VER, "5Zone_Transformer.idf")
    path_epw <- path_eplus_weather(LATEST_EPLUS_VER, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    param <- param_job(path_idf, path_epw)

    # $version()
    expect_equal(param$version(), numeric_version(LATEST_EPLUS_VER))

    # $seed()
    expect_s3_class(param$seed(), "Idf")
    expect_s3_class(param$seed(new = path_idf), "ParametricJob")

    # $weather()
    expect_s3_class(param$weather(), "Epw")
    expect_null(param_job(path_idf, NULL)$weather())

    # $weathers()
    path_epws <- path_eplus_weather(LATEST_EPLUS_VER,
        c("USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw",
          "USA_CO_Golden-NREL.724666_TMY3.epw")
    )
    param <- param_job(path_idf, NULL)
    expect_s3_class(param$weathers(path_epws, names = c("sf", "golden")), "ParametricJob")
    expect_equal(
        ignore_attr = TRUE,
        param$weathers(),
        data.table(weather_index = 1:2, weather_case = c("sf", "golden"),
            epw = normalizePath(path_epws)
        )
    )
    expect_type(param$weather(), "list")
})

test_that("$apply_measure()", {
    skip_on_cran()

    path_idf <- copy_eplus_example(LATEST_EPLUS_VER, "5Zone_Transformer.idf")
    param <- param_job(path_idf, NULL)

    mea <- function(idf, num) idf

    expect_error(param$apply_measure(function(idf) idf))
    expect_error(param$apply_measure(function(idf, ...) 1, a = 1:5))
    expect_s3_class(param$apply_measure(mea, num = 1:2, .names = "case"), "ParametricJob")
    expect_equal(names(param$models()), c("case_1", "case_2"))
    expect_error(param$apply_measure(mea, num = 1:2, .names = c("a", "b", "c")))

    eplusr_option(verbose_info = FALSE)
    param <- param_job(path_idf, NULL)
    eplusr_option(verbose_info = TRUE)
    expect_message(param$apply_measure(function(idf, num) idf, num = 1:2), "function")
    expect_message(param$apply_measure(mea, num = 1:2), "mea")

    param <- param_job(path_idf, NULL)
    msg <- catch_cnd(param$apply_measure(mea, num = 1:27))
    expect_s3_class(msg, "eplusr_message_param_measure")
    expect_match(conditionMessage(msg), "... and 7 more", fixed = TRUE)
    expect_match(conditionMessage(msg), "[20]: mea_20", fixed = TRUE)
    expect_no_match(conditionMessage(msg), "[21]: mea_21", fixed = TRUE)
})

test_that("$param()", {
    skip_on_cran()

    path <- copy_eplus_example(LATEST_EPLUS_VER, "5Zone_Transformer.idf")
    param <- param_job(path, NULL)

    expect_s3_class(class = "ParametricJob",
        param$param("Supply Fan 1" = list(fan_total_efficiency = c(0.1, 0.5, 0.8)), .cross = TRUE)
    )
    expect_equal(length(param$models()), 3L)

    expect_s3_class(class = "ParametricJob",
        param$param(
            Material := list(Thickness = seq(0.1, 1, length.out = 3), Conductivity = seq(0.1, 0.6, length.out = 3)),
            "Supply Fan 1" = list(fan_total_efficiency = c(0.1, 0.5, 0.8))
        )
    )
    expect_equal(length(param$models()), 3L)

    expect_s3_class(class = "ParametricJob",
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
    expect_s3_class(class = "ParametricJob",
        param$param(
            "Supply Fan 1" = list(fan_total_efficiency = c(0.1, 0.5, 0.8)),
            .names = "fan_eff"
        )
    )
})

test_that("$models()", {
    skip_on_cran()

    eplusr_option(verbose_info = FALSE)
    path_idf <- copy_eplus_example(LATEST_EPLUS_VER, "5Zone_Transformer.idf")
    param <- param_job(path_idf, NULL)

    expect_null(param$models())
    expect_null(param$models("new name"))

    param$apply_measure(function(idf, num) idf, num = 1:2)

    expect_type(param$models(), "list")
    expect_equal(length(param$models()), 2)
    expect_equal(names(param$models()), c("case_1", "case_2"))

    expect_equal(names(param$models(names = "model")), c("model_1", "model_2"))
    expect_error(names(param$models(names = c("a", "b", "c"))), "names")

    models <- param$models()
    models[[1L]]$set(Material := list(thickness = 0.05))
    expect_false(param$models()[[1L]]$is_unsaved())
})

test_that("$cases()", {
    skip_on_cran()

    path <- copy_eplus_example(LATEST_EPLUS_VER, "5Zone_Transformer.idf")

    param <- param_job(path, NULL)

    expect_null(param$cases())

    param$param(
        "Supply Fan 1" = list(
            fan_total_efficiency = c(0.1, 0.2),
            availability_schedule_name = c("FanAvailSched", "Always On")
        ),
        c("GP01", "GP02") := .(thickness = c(0.01, 0.02))
    )
    expect_equal(
        ignore_attr = TRUE,
        param$cases(),
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
    expect_equal(
        ignore_attr = TRUE,
        param$cases(),
        data.table(index = 1:2, case = c("case_1", "case_2"),
            eff = c(0.1, 0.2), sch = c("FanAvailSched", "Always On")
        )
    )

    path_epws <- path_eplus_weather(LATEST_EPLUS_VER,
        c("USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw",
          "USA_CO_Golden-NREL.724666_TMY3.epw")
    )
    param$weathers(path_epws, names = c("sf", "golden"))
    expect_equal(
        ignore_attr = TRUE,
        param$cases("run"),
        data.table(
            index = 1:4,
            case = c("case_1__sf", "case_1__golden", "case_2__sf", "case_2__golden"),
            model_index = c(1L, 1L, 2L, 2L),
            model_case = c("case_1", "case_1", "case_2", "case_2"),
            weather_index = c(1L, 2L, 1L, 2L),
            weather_case = c("sf", "golden", "sf", "golden"),
            epw = normalizePath(path_epws[c(1L, 2L, 1L, 2L)]),
            eff = c(0.1, 0.1, 0.2, 0.2),
            sch = c("FanAvailSched", "FanAvailSched", "Always On", "Always On")
        )
    )

    param <- param_job(path, path_epws)
    expect_null(param$cases())
    expect_equal(
        ignore_attr = TRUE,
        param$cases("run"),
        data.table(
            index = 1:2,
            case = tools::file_path_sans_ext(basename(path_epws)),
            model_index = c(1L, 1L),
            model_case = c("seed", "seed"),
            weather_index = 1:2,
            weather_case = tools::file_path_sans_ext(basename(path_epws)),
            epw = normalizePath(path_epws)
        )
    )
})

test_that("$run()", {
    skip_on_cran()
    skip_if_not_integration()

    path <- copy_eplus_example(LATEST_EPLUS_VER, "5Zone_Transformer.idf")
    param <- param_job(path, NULL)

    expect_error(param$run(), "No measure")

    param$apply_measure(function(idf, num) idf, num = 1:2)
    param$models()[[1L]]$set(Material := list(thickness = 0.05))
    expect_s3_class(param$run(), "ParametricJob")

    path_epw <- path_eplus_weather(LATEST_EPLUS_VER, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")
    param <- param_job(path, path_epw)
    param$apply_measure(function(idf, num) idf, num = 1:2)
    expect_s3_class(param$run(), "ParametricJob")
})

test_that("$seed(new=) invalidates generated parametric models", {
    skip_on_cran()

    path <- copy_eplus_example(LATEST_EPLUS_VER, "5Zone_Transformer.idf")
    param <- param_job(path, NULL)
    param$apply_measure(function(idf, num) idf, num = 1:2)

    expect_s3_class(param$seed(new = path), "ParametricJob")
    expect_error(param$models(), class = "eplusr_error_param_models_stale")
    expect_error(param$cases(), class = "eplusr_error_param_models_stale")
    expect_error(param$save(), class = "eplusr_error_param_models_stale")
    expect_error(param$run(), class = "eplusr_error_param_models_stale")

    expect_s3_class(param$apply_measure(function(idf, num) idf, num = 1:2), "ParametricJob")
    expect_equal(names(param$models()), c("case_1", "case_2"))
})

test_that("$save()", {
    skip_on_cran()

    path <- copy_eplus_example(LATEST_EPLUS_VER, "5Zone_Transformer.idf")
    param <- param_job(path, NULL)
    expect_error(param$save())

    param$apply_measure(function(idf, num) idf, num = 1:2)
    expect_equal(
        ignore_attr = TRUE,
        param$save(),
        data.table(
            model = normalizePath(file.path(tempdir(), c("case_1", "case_2"), c("case_1.idf", "case_2.idf"))),
            weather = NA_character_
        )
    )

    param <- param_job(path, path_eplus_weather(LATEST_EPLUS_VER, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"))
    param$apply_measure(function(idf, num) idf, num = 1:2)
    expect_equal(
        ignore_attr = TRUE,
        param$save(separate = FALSE),
        data.table(
            model = normalizePath(file.path(tempdir(), c("case_1.idf", "case_2.idf"))),
            weather = normalizePath(file.path(tempdir(), "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"))
        )
    )

    path_epws <- path_eplus_weather(LATEST_EPLUS_VER,
        c("USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw",
          "USA_CO_Golden-NREL.724666_TMY3.epw")
    )
    param <- param_job(path, NULL)
    param$weathers(path_epws, names = c("sf", "golden"))
    param$apply_measure(function(idf, num) idf, num = 1:2)
    expect_equal(
        ignore_attr = TRUE,
        param$save(separate = FALSE),
        data.table(
            model = normalizePath(file.path(tempdir(),
                c("case_1__sf.idf", "case_1__golden.idf", "case_2__sf.idf", "case_2__golden.idf")
            )),
            weather = normalizePath(file.path(tempdir(),
                basename(path_epws[c(1L, 2L, 1L, 2L)])
            ))
        )
    )
})

test_that("$print()", {
    skip_on_cran()

    path <- copy_eplus_example(LATEST_EPLUS_VER, "5Zone_Transformer.idf")
    param <- param_job(path, NULL)
    expect_output(param$print())

    mea <- function(idf, num) idf
    param$apply_measure(mea, num = 1:2)
    expect_output(param$print())

    param <- param_job(path, NULL)
    suppressMessages(param$apply_measure(mea, num = 1:27))
    out <- paste0(utils::capture.output(param$print()), collapse = "\n")
    expect_match(out, "Simulation Status Summary")
    expect_match(out, "idle: 27")
    expect_match(out, "... and 7 more", fixed = TRUE)
    expect_match(out, "Use $cases() to inspect all cases", fixed = TRUE)
    expect_no_match(out, "mea_21", fixed = TRUE)
})

test_that("==.ParametricJob and !=.ParametricJob", {
    skip_on_cran()

    path <- copy_eplus_example(LATEST_EPLUS_VER, "5Zone_Transformer.idf")
    param <- param_job(path, NULL)

    expect_false(param == 1)
    expect_true(param == param)
    expect_false(param != param)
})

# vim: set fdm=marker:
