test_that("clean_wd()", {
    expect_true(file.create(f <- tempfile()))
    expect_true({clean_wd(f); file.exists(f)})
    unlink(f)

    expect_true(file.create(f <- tempfile(fileext = ".idf")))
    expect_silent({clean_wd(f); file.exists(f)})
    unlink(f)

    expect_true(file.create(f <- file.path(tempdir(), "in.idf")))
    expect_silent({clean_wd(f); file.exists(f)})
    unlink(f)
})

test_that("utilities", {
    .globals$eplus$"8.1.0"$version <- numeric_version("8.1.0")
    .globals$eplus$"8.1.0"$dir <- tempdir()
    .globals$eplus$"8.1.0"$exe <- "energyplus"
    get(".globals", envir = asNamespace("eplusr"))$eplus
    expect_error(eplus_exe(8.1), class = "eplusr_error_eplus_ver_not_supported")
    .globals$eplus$"8.1.0" <- NULL
})

test_that("run_idf()", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    path_idf <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    # can run ddy simulation
    expect_silent(res <- run_idf(path_idf, NULL, output_dir = tempdir(), echo = FALSE))
    # can specify EnergyPlus version
    expect_silent(res <- run_idf(path_idf, NULL, output_dir = tempdir(), echo = FALSE, eplus = 8.8))
    expect_null(res$epw)
    # can stop if failed to find version
    expect_error({
        f <- tempfile(fileext = ".idf")
        write_lines(read_lines(path_idf)[-91], f)
        run_idf(f, NULL, output_dir = tempdir(), echo = FALSE)
    }, "Missing version field", class = "eplusr_error_miss_idf_ver")
    # can use input file directory
    expect_silent({
        f <- tempfile(fileext = ".idf")
        file.copy(path_idf, f, overwrite = TRUE)
        res <- run_idf(f, NULL, output_dir = NULL, echo = FALSE)
    })

    # can run simulation with weather
    expect_silent(res <- run_idf(path_idf, path_epw, output_dir = tempdir(), echo = FALSE))

    expect_equal(res$idf, normalizePath(path_idf))
    expect_equal(res$epw, normalizePath(path_epw))
    expect_equal(res$version, "8.8.0")
    expect_equal(res$exit_status, 0L)
    expect_is(res$start_time, "POSIXct")
    expect_is(res$end_time, "POSIXct")
    expect_equal(res$output_dir, normalizePath(tempdir(), mustWork = FALSE))
    expect_equal(res$energyplus, normalizePath(file.path(eplus_config(8.8)$dir, eplus_config(8.8)$exe), mustWork = TRUE))
    expect_is(res$stdout, "character")
    expect_true("stderr" %in% names(res))
    expect_is(res$process, "process")
    expect_true(file.exists(file.path(tempdir(), basename(res$idf))))
    expect_true(file.exists(file.path(tempdir(), basename(res$epw))))

    # can run in the background
    expect_silent(res <- run_idf(path_idf, NULL, output_dir = tempdir(), wait = FALSE))
    expect_equal(res$idf, normalizePath(path_idf))
    expect_null(res$epw)
    expect_equal(res$version, "8.8.0")
    expect_null(res$exit_status)
    expect_is(res$start_time, "POSIXct")
    expect_null(res$end_time)
    expect_equal(res$output_dir, normalizePath(tempdir(), mustWork = FALSE))
    expect_equal(res$energyplus, normalizePath(file.path(eplus_config(8.8)$dir, eplus_config(8.8)$exe), mustWork = TRUE))
    expect_null(res$stdout)
    expect_null(res$stderr)
    expect_is(res$process, "process")
    expect_silent({res$process$wait(); res_post <- res$process$get_result()})
    expect_is(res_post$stdout, "character")
    expect_is(res_post$stderr, "character")

    # can run model with HVACTemplate objects
    path <- file.path(eplus_config(8.8)$dir, "ExampleFiles/HVACTemplate-5ZoneVAVWaterCooled.idf")
    expect_is(run_idf(path, NULL, tempdir(), design_day = TRUE, echo = FALSE), "list")
    expect_true(file.exists(file.path(tempdir(), "HVACTemplate-5ZoneVAVWaterCooled.expidf")))

    expect_is(res <- run_idf(path, NULL, tempdir(), design_day = TRUE, wait = FALSE), "list")
    while(res$process$is_alive()) Sys.sleep(0.2)
    expect_is(res$process$get_result(), "list")
    expect_true(file.exists(file.path(tempdir(), "HVACTemplate-5ZoneVAVWaterCooled.expidf")))
})

test_that("run_multi()", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    path_idf <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    # can stop if idf and epw does not have the same length
    expect_error(run_multi(rep(path_idf, 2), rep(path_epw, 3)), "Must have same length")
    # can stop if idf and eplus does not have the same length
    expect_error(run_multi(rep(path_idf, 2), NULL, eplus = rep(8.8, 3)), "Must have same length")
    # can stop if idf and design does not have the same length
    expect_error(run_multi(rep(path_idf, 2), NULL, design_day = rep(FALSE, 3)), "Must have same length")
    # can stop if idf and annual does not have the same length
    expect_error(run_multi(rep(path_idf, 2), NULL, annual = rep(FALSE, 3)), "Must have same length")
    # can stop if both design and annual is TRUE
    expect_error(run_multi(path_idf, NULL, annual = TRUE, design_day = TRUE), "both design-day-only", class = "eplusr_error_both_ddy_annual")
    # can stop if model does not exist
    expect_error(run_multi(tempfile(), NULL))
    # can stop if model does not contain version
    expect_error({
        f <- tempfile(fileext = ".idf")
        write_lines(read_lines(path_idf)[-91], f)
        run_multi(f, NULL, output_dir = tempdir())
    }, "Failed to determine the version of EnergyPlus", class = "eplusr_error_miss_idf_ver")
    # can stop if target EnergyPlus is not found
    expect_error(run_multi(path_idf, NULL, eplus = 8.0), "Cannot locate EnergyPlus", class = "eplusr_error_locate_eplus")
    # can stop if input idf contain duplications
    expect_error(run_multi(rep(path_idf, 2L), NULL, output_dir = NULL), class = "eplusr_error_duplicated_sim")
    # can stop if idf and output directory does not have the same length
    expect_error(run_multi(rep(path_idf, 2L), NULL, output_dir = tempdir()), "have same length")
    # can stop if idf and output directory combines same job
    expect_error(run_multi(rep(path_idf, 2L), NULL, output_dir = rep(tempdir(), 2L)), "Duplication found")

    expect_message(res <- run_multi(rep(path_idf, 2L), NULL, output_dir = c(file.path(tempdir(), "a"), file.path(tempdir(), "b"))),
        "FAILED"
    )
    expect_is(res, "data.table")
    expect_equal(names(res), c("index", "status", "idf", "epw", "version",
            "exit_status", "start_time", "end_time", "output_dir", "energyplus",
            "stdout", "stderr"))
    expect_equal(res$index, 1:2)
    expect_equal(res$status, rep("failed", 2))
    expect_equal(res$idf, rep(normalizePath(path_idf), 2))
    expect_equal(res$epw, rep(NA_character_, 2))
    expect_equal(res$version, rep("8.8.0", 2))
    expect_equal(res$exit_status > 0, rep(TRUE, 2))
    expect_is(res$start_time, "POSIXct")
    expect_is(res$end_time, "POSIXct")
    expect_equal(res$energyplus, rep(normalizePath(file.path(eplus_config(8.8)$dir, eplus_config(8.8)$exe), mustWork = TRUE), 2L))
    checkmate::expect_list(res$stdout, "character")
    checkmate::expect_list(res$stderr, "character")

    expect_silent(res <- run_multi(rep(path_idf, 2L), NULL, output_dir = c(file.path(tempdir(), "a"), file.path(tempdir(), "b")), wait = FALSE))
    expect_is(res, "r_process")
    expect_equal({res$wait(); res$get_exit_status()}, 0L)
    expect_silent(res <- res$get_result())
    expect_is(res, "data.table")
    expect_equal(names(res), c("index", "status", "idf", "epw", "version",
            "exit_status", "start_time", "end_time", "output_dir", "energyplus",
            "stdout", "stderr"))
    expect_equal(res$index, 1:2)
    expect_equal(res$status, rep("failed", 2))
    expect_equal(res$idf, rep(normalizePath(path_idf, mustWork = FALSE), 2))
    expect_equal(res$epw, rep(NA_character_, 2))
    expect_equal(res$version, rep("8.8.0", 2))
    expect_equal(res$exit_status > 0, rep(TRUE, 2))
    expect_is(res$start_time, "POSIXct")
    expect_is(res$end_time, "POSIXct")
    expect_equal(res$output_dir, normalizePath(c(file.path(tempdir(), "a"), file.path(tempdir(), "b")), mustWork = FALSE))
    expect_equal(res$energyplus, rep(normalizePath(file.path(eplus_config(8.8)$dir, eplus_config(8.8)$exe), mustWork = TRUE), 2L))
    checkmate::expect_list(res$stdout, "character")
    checkmate::expect_list(res$stderr, "character")
    expect_null(get_run_time(NULL))
    expect_null(get_run_time("a"))
})

test_that("EPMacro()", {
    main <- test_path("file/macro.imf")
    part <- test_path("file/part.idf")
    write_lines("##include part.idf\nVersion,8.8;", main)
    write_lines("Building,\nBldg;", part)

    res <- EPMacro(part, echo = FALSE, eplus = 8.8)
    expect_equal(names(res), c("file", "run"))
    expect_equal(res$file, NULL)
    expect_equal(res$run, NULL)

    res <- EPMacro(main, echo = FALSE, eplus = 8.8)
    expect_equal(names(res), c("file", "run"))
    expect_true(file.exists(res$file$imf))
    expect_true(file.exists(res$file$epmidf))
    expect_true(file.exists(res$file$epmdet))
    expect_equal(names(res$file), c("imf", "epmidf", "epmdet"))
    expect_equal(names(res$run), c("process", "exit_status", "stdout",
        "stderr", "start_time", "end_time"))
    expect_equal(res$run$stdout, NULL)
    expect_equal(res$run$stderr, NULL)
    unlink(c(res$file$epmidf, res$file$epmdet))

    res <- EPMacro(main, tempdir())
    expect_equal(dirname(res$file$imf), normalizePath(tempdir(), "/"))
    expect_equal(dirname(res$file$epmidf), normalizePath(tempdir(), "/"))
    expect_equal(dirname(res$file$epmdet), normalizePath(tempdir(), "/"))
    unlink(unlist(res$file))

    res <- EPMacro(main, output_prefix = "test")
    expect_equal(basename(res$file$epmidf), "test.epmidf")
    expect_equal(basename(res$file$epmdet), "test.epmdet")
    unlink(c(res$file$epmidf, res$file$epmdet))

    res <- EPMacro(main, wait = FALSE, echo = FALSE)
    expect_equal(res$file, NULL)
    expect_equal(res$run$exit_status, NULL)
    expect_equal(res$run$stdout, NULL)
    expect_equal(res$run$stderr, NULL)
    expect_equal(res$run$end_time, NULL)
    while(res$run$process$is_alive()) Sys.sleep(0.1)
    post <- res$run$process$get_result()
    expect_equal(names(post), c("file", "run"))
    res <- modifyList(res, post)
    expect_equal(basename(res$file$imf), "macro.imf")
    expect_equal(basename(res$file$epmidf), "macro.epmidf")
    expect_equal(basename(res$file$epmdet), "macro.epmdet")
    expect_equal(res$run$stdout, NULL)
    expect_equal(res$run$stderr, NULL)
    expect_equal(res$run$process$get_exit_status(), 0L)
    unlink(unlist(res$file))

    unlink(c(main, part))
})

test_that("ExpandObjects()", {
    path <- test_path("file/expandobj.idf")
    write_lines(
        "Version, 8.8;
        HVACTemplate:Thermostat,
            All Zones,
            ,
            20,
            ,
            26;
        ",
        path
    )

    res <- ExpandObjects(path, echo = FALSE)
    expect_equal(names(res), c("file", "run"))
    expect_equal(names(res$file), c("idf", "expidf", "basement", "slab", "experr"))
    expect_true(file.exists(res$file$idf))
    expect_true(file.exists(res$file$expidf))
    expect_equal(res$file$basement, NA_character_)
    expect_equal(res$file$slab, NA_character_)
    expect_equal(res$file$experr, NA_character_)
    expect_equal(names(res$run), c("process", "exit_status", "stdout",
        "stderr", "start_time", "end_time"))
    expect_false(is.null(res$run$stdout))
    expect_null(res$run$stderr)
    unlink(res$file$expidf)

    res <- ExpandObjects(path, tempdir(), echo = FALSE)
    expect_equal(dirname(res$file$idf), normalizePath(tempdir(), "/"))
    expect_equal(dirname(res$file$expidf), normalizePath(tempdir(), "/"))
    expect_equal(res$file$basement, NA_character_)
    expect_equal(res$file$slab, NA_character_)
    expect_equal(res$file$experr, NA_character_)
    unlink(c(res$file$idf, res$file$expidf))

    res <- ExpandObjects(path, output_prefix = "test", echo = FALSE)
    expect_equal(basename(res$file$expidf), "test.expidf")
    expect_equal(res$file$basement, NA_character_)
    expect_equal(res$file$slab, NA_character_)
    expect_equal(res$file$experr, NA_character_)
    unlink(res$file$expidf)

    res <- ExpandObjects(path, wait = FALSE, echo = FALSE)
    expect_equal(res$file, NULL)
    expect_equal(res$run$exit_status, NULL)
    expect_equal(res$run$stdout, NULL)
    expect_equal(res$run$stderr, NULL)
    expect_equal(res$run$end_time, NULL)
    while(res$run$process$is_alive()) Sys.sleep(0.1)
    post <- res$run$process$get_result()
    expect_equal(names(post), c("file", "run"))
    res <- modifyList(res, post)
    expect_equal(basename(res$file$expidf), "expandobj.expidf")
    expect_false(is.null(res$run$stdout))
    expect_null(res$run$stderr)
    expect_equal(res$file$experr, NA_character_)
    expect_equal(res$run$process$get_exit_status(), 0L)
    unlink(res$file$expidf)

    unlink(path)
})

test_that("Basement()", {
    path <- test_path("file/basement.idf")
    write_lines(
        "SimParameters,
           0.1,
           1;

         MatlProps,
           6,
           2242.6,
           2242.6,
           311.66,
           1500.0,
           2000.0,
           448.5,
           880.0,
           880.0,
           1513.0,
           840.0,
           720.0,
           1630.0,
           1.402,
           1.402,
           0.093,
           0.5,
           1.9,
           0.119;

         Insulation,
           5.0,
           TRUE;

         SurfaceProps,
           0.16,
           0.40,
           0.94,
           0.86,
           6.0,
           0.25,
           TRUE;

         BldgData,
           0.2,
           0.1,
           0.3,
           0.2,
           0.1;

         Interior,
           TRUE,
           0.92,
           4.04,
           3.08,
           6.13,
           9.26,
           8.29;

         ComBldg,
           20.,
           20.,
           20.,
           20.,
           20.,
           20.,
           20.,
           20.,
           20.,
           20.,
           20.,
           20.,
           0.0;

         EquivSlab,
           15.0,
           TRUE;

         EquivAutoGrid,
           15,
           0.1,
           2.4;
        ",
        path
    )
    weather <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CO_Golden-NREL.724666_TMY3.epw")

    res <- Basement(path, weather, echo = FALSE, eplus = 8.8)
    expect_equal(names(res), c("file", "run"))
    expect_equal(names(res$file), c("idf", "epw", "bsmt_idf", "bsmt_csv", "bsmt_out", "bsmt_audit"))
    expect_true(file.exists(res$file$idf))
    expect_true(file.exists(res$file$epw))
    expect_equal(basename(res$file$bsmt_idf), "basement_bsmt.idf")
    expect_equal(basename(res$file$bsmt_csv), "basement_bsmt.csv")
    expect_equal(basename(res$file$bsmt_out), "basement_bsmt.out")
    expect_equal(basename(res$file$bsmt_audit), "basement_bsmt.audit")
    expect_equal(names(res$run), c("process", "exit_status", "stdout",
        "stderr", "start_time", "end_time"))
    expect_false(is.null(res$run$stdout))
    expect_false(is.null(res$run$stderr))
    unlink(unlist(res$file))

    unlink(path)
})

test_that("Slab()", {
    path <- test_path("file/slab.idf")
    write_lines(
        "Materials,
             2,
             0.158,
             0.379,
             0.9,
             0.9,
             0.75,
             0.03,
             6.13,
             9.26;

         MatlProps,
             2300,
             1200,
             653,
             1200,
             0.93,
             1;

         BoundConds,
             TRUE,
             TRUE,
             10,
             False,
             ;

         BldgProps,
             1,
             0,
             4,
             18,
             18,
             18,
             20,
             20,
             20,
             22,
             22,
             22,
             22,
             20,
             20,
             0,
             0.10;

         Insulation,
             0.,
             0.,
             2.0,
             2.0,
             1;

         EquivalentSlab,
             10,
             0.1,
             15,
             10.;
        ",
        path
    )
    weather <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CO_Golden-NREL.724666_TMY3.epw")

    res <- Slab(path, weather, eplus = 8.8, echo = FALSE)
    expect_equal(names(res), c("file", "run"))
    expect_equal(names(res$file), c("idf", "epw", "slab_gtp", "slab_out", "slab_ger"))
    expect_true(file.exists(res$file$idf))
    expect_true(file.exists(res$file$epw))
    expect_equal(basename(res$file$slab_gtp), "slab_slab.gtp")
    expect_equal(basename(res$file$slab_out), "slab_slab.out")
    expect_equal(basename(res$file$slab_ger), "slab_slab.ger")
    expect_equal(names(res$run), c("process", "exit_status", "stdout",
        "stderr", "start_time", "end_time"))
    expect_false(is.null(res$run$stdout))
    expect_false(is.null(res$run$stderr))
    unlink(unlist(res$file))

    unlink(path)
})

test_that("EnergyPlus()", {
    path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", "1ZoneUncontrolled.idf")
    file.copy(path, tempdir())
    path <- file.path(tempdir(), basename(path))

    weather <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CO_Golden-NREL.724666_TMY3.epw")

    res <- EnergyPlus(path, weather, file.path(tempdir(), "EnergyPlus"), eplus = 8.8, echo = FALSE)
    expect_equal(names(res), c("file", "run"))
    expect_equal(length(res$file), 42L)
    expect_equal(
        sort(names(which(vapply(res$file, function(x) !anyNA(x), logical(1))))),
        c("audit", "bnd", "dxf", "eio", "end", "epw", "err", "eso", "idf",
            "mdd", "mtd", "mtr", "rdd", "shd", "sqlite_err", "table")
    )
    expect_equal(names(res$run), c("process", "exit_status", "stdout",
        "stderr", "start_time", "end_time"))
    expect_false(is.null(res$run$stdout))
    expect_false(is.null(res$run$stderr))
    unlink(unlist(res$file))

    unlink(path)
})

test_that("convertESOMTR()", {
    path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", "1ZoneUncontrolled.idf")
    file.copy(path, tempdir())
    path <- file.path(tempdir(), basename(path))

    weather <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CO_Golden-NREL.724666_TMY3.epw")

    res <- EnergyPlus(path, weather, file.path(tempdir(), "EnergyPlus"), eplus = 8.8, echo = FALSE)

    eso <- convertESOMTR(res$file$eso, eplus = 8.8, echo = FALSE)
    expect_equal(names(eso), c("file", "run"))
    expect_equal(names(eso$file), c("eso", "mtr", "ipeso", "ipmtr", "iperr"))
    expect_true(file.exists(eso$file$eso))
    expect_true(file.exists(eso$file$ipeso))
    expect_true(is.na(eso$file$mtr))
    expect_true(is.na(eso$file$ipmtr))
    expect_true(is.na(eso$file$iperr))
    expect_equal(names(eso$run), c("process", "exit_status", "stdout",
        "stderr", "start_time", "end_time"))
    expect_false(is.null(eso$run$stdout))
    expect_true(is.null(eso$run$stderr))
    unlink(unlist(eso$file))

    mtr <- convertESOMTR(res$file$mtr, eplus = 8.8, echo = FALSE)
    expect_equal(names(mtr), c("file", "run"))
    expect_equal(names(mtr$file), c("eso", "mtr", "ipeso", "ipmtr", "iperr"))
    expect_true(file.exists(mtr$file$mtr))
    expect_true(file.exists(mtr$file$ipmtr))
    expect_true(is.na(mtr$file$eso))
    expect_true(is.na(mtr$file$ipeso))
    expect_true(is.na(mtr$file$iperr))
    expect_equal(names(mtr$run), c("process", "exit_status", "stdout",
        "stderr", "start_time", "end_time"))
    expect_false(is.null(mtr$run$stdout))
    expect_true(is.null(mtr$run$stderr))
    unlink(unlist(mtr$file))

    unlink(unlist(res$file))
    unlink(path)
})

test_that("ReadVarsESO()", {
    path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", "1ZoneUncontrolled.idf")
    file.copy(path, tempdir())
    path <- file.path(tempdir(), basename(path))

    weather <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CO_Golden-NREL.724666_TMY3.epw")

    res <- EnergyPlus(path, weather, file.path(tempdir(), "EnergyPlus"), eplus = 8.8, echo = FALSE)

    eso <- ReadVarsESO(res$file$eso, eplus = 8.8, echo = FALSE)
    expect_equal(names(eso), c("file", "run"))
    expect_equal(names(eso$file), c("eso", "mtr", "variable", "meter", "rvaudit"))
    expect_true(file.exists(eso$file$eso))
    expect_true(file.exists(eso$file$variable))
    expect_true(file.exists(eso$file$rvaudit))
    expect_true(is.na(eso$file$mtr))
    expect_true(is.na(eso$file$meter))
    expect_equal(names(eso$run), c("process", "exit_status", "stdout",
        "stderr", "start_time", "end_time"))
    expect_true(!is.na(eso$file$variable))
    expect_true(is.na(eso$file$meter))
    expect_true(!is.na(eso$file$rvaudit))
    unlink(unlist(eso$file))

    mtr <- ReadVarsESO(res$file$mtr, eplus = 8.8, echo = FALSE)
    expect_equal(names(mtr$file), c("eso", "mtr", "variable", "meter", "rvaudit"))
    expect_true(file.exists(mtr$file$mtr))
    expect_true(file.exists(mtr$file$meter))
    expect_true(file.exists(mtr$file$rvaudit))
    expect_true(is.na(mtr$file$eso))
    expect_true(is.na(mtr$file$variable))
    expect_equal(names(mtr$run), c("process", "exit_status", "stdout",
        "stderr", "start_time", "end_time"))
    expect_true(is.na(mtr$file$variable))
    expect_true(!is.na(mtr$file$meter))
    expect_true(!is.na(mtr$file$rvaudit))
    unlink(unlist(mtr$file))

    unlink(unlist(res$file))
    unlink(path)
})

test_that("HVAC_Diagram()", {
    path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", "1ZoneUncontrolled.idf")
    file.copy(path, tempdir())
    path <- file.path(tempdir(), basename(path))

    weather <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CO_Golden-NREL.724666_TMY3.epw")

    res <- EnergyPlus(path, weather, file.path(tempdir(), "EnergyPlus"), eplus = 8.8, echo = FALSE)

    svg <- HVAC_Diagram(res$file$bnd, echo = FALSE)
    expect_equal(names(svg), c("file", "run"))
    expect_equal(names(svg$file), c("bnd", "svg"))
    expect_true(file.exists(svg$file$bnd))
    expect_true(file.exists(svg$file$bnd))
    expect_equal(names(svg$run), c("process", "exit_status", "stdout",
        "stderr", "start_time", "end_time"))
    expect_true(!is.na(svg$file$svg))
    unlink(unlist(svg$file))

    unlink(unlist(res$file))
    unlink(path)
})
    path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", "1ZoneUncontrolled.idf")
    file.copy(path, tempdir())
    path <- file.path(tempdir(), basename(path))

    weather <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CO_Golden-NREL.724666_TMY3.epw")

    res <- EnergyPlus(path, weather, file.path(tempdir(), "EnergyPlus"), eplus = 8.8)
    res$file
    expect_equal(names(res), c("file", "run"))
    expect_equal(names(res$file), c("idf", "out", "audit"))
    expect_equal(basename(res$file$idf), "slab_slab.gtp")
    expect_equal(basename(res$file$out), "slab_slab.out")
    expect_equal(basename(res$file$audit), "slab_slab.ger")
    expect_equal(names(res$run), c("process", "exit_status", "stdout",
        "stderr", "start_time", "end_time"))
    expect_false(is.null(res$run$stdout))
    expect_false(is.null(res$run$stderr))
    unlink(unlist(res$file))

    unlink(path)
})
