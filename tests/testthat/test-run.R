test_that("clean_wd()", {
    skip_on_cran()

    expect_true(file.create(f <- tempfile()))
    expect_true({clean_wd(f); file.exists(f)})
    unlink(f)

    expect_true(file.create(f <- tempfile(fileext = ".idf")))
    expect_silent({clean_wd(f); file.exists(f)})
    unlink(f)

    expect_true(file.create(f <- file.path(tempdir(), "in.idf")))
    expect_true({clean_wd(f); file.exists(f)})

    expect_true(file.create(fidf <- file.path(tempdir(), "test.idf")))
    expect_true(file.create(fcsv <- file.path(tempdir(), "test.csv")))
    expect_true({.clean_wd(fidf, "D", basename(fcsv)); file.exists(fcsv)})
    unlink(c(fidf, fcsv))
})

test_that("path_eplus()", {
    skip_on_cran()

    expect_equal(basename(path_eplus(8.8, "a", "b")), "b")
    expect_error(basename(path_eplus(8.8, "a", "b", strict = TRUE)))

    expect_true(is.character(path_eplus_processor(8.8, "EPMacro", strict = TRUE)))
    expect_true(is.character(path_eplus_processor(8.8, "PreProcess", "GrndTempCalc", "Slab", strict = TRUE)))

    expect_true(is.character(path_eplus_example(8.8, "1ZoneUncontrolled.idf")))
    expect_true(is.character(path_eplus_example(8.8, "BasicFiles/Exercise1A.idf")))

    expect_true(is.character(path_eplus_weather(8.8, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.ddy")))

    expect_true(is.character(path_eplus_dataset(8.8, "Boilers.idf")))
    expect_true(is.character(path_eplus_dataset(8.8, "FMUs/MoistAir.fmu")))
})

test_that("copy_energyplus_idd()", {
    skip_on_cran()

    expect_silent(res <- copy_energyplus_idd(path_eplus(8.8, "energyplus"), tempdir()))
    expect_true(file.exists(file.path(tempdir(), "Energy+.idd")))
    expect_true(attr(res, "copied"))

    expect_silent(res <- copy_energyplus_idd(NULL, tempdir(), res))
    expect_true(file.exists(file.path(tempdir(), "Energy+.idd")))
    expect_false(attr(res, "copied"))

    unlink(file.path(tempdir(), "Energy+.idd"))

    expect_silent(res <- copy_energyplus_idd(NULL, tempdir(), path_eplus(8.8, "Energy+.idd")))
    expect_true(file.exists(file.path(tempdir(), "Energy+.idd")))
    unlink(file.path(tempdir(), "Energy+.idd"))
})

test_that("EPMacro()", {
    skip_on_cran()

    out_dir <- file.path(tempdir(), "EPMacro")
    if (!dir.exists(out_dir)) dir.create(out_dir)

    main <- file.path(out_dir, "macro.imf")
    part <- file.path(out_dir, "part.idf")
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
    expect_null(res$run$stdout)
    expect_null(res$run$stderr)
    expect_equal(res$run$process$get_exit_status(), 0L)
    unlink(unlist(res$file))

    unlink(out_dir, recursive = TRUE)
    unlink(c(main, part))
})

test_that("ExpandObjects()", {
    skip_on_cran()

    out_dir <- file.path(tempdir(), "ExpandObjects")
    if (!dir.exists(out_dir)) dir.create(out_dir)

    path <- file.path(out_dir, "expandobj.idf")
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

    res <- ExpandObjects(path, echo = FALSE, idd = path_eplus(8.8, "Energy+.idd"))
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

    unlink(out_dir, recursive = TRUE)
    unlink(path)
})

test_that("Basement()", {
    skip_on_cran()

    out_dir <- file.path(tempdir(), "Basement")
    if (!dir.exists(out_dir)) dir.create(out_dir)

    path <- file.path(out_dir, "basement.idf")
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
    weather <- path_eplus_weather(8.8, "USA_CO_Golden-NREL.724666_TMY3.epw")

    res <- Basement(path, weather, echo = FALSE, eplus = 8.8,
        idd = path_eplus(8.8, "PreProcess", "GrndTempCalc", "BasementGHT.idd")
    )
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

    unlink(out_dir, recursive = TRUE)
    unlink(path)
})

test_that("Slab()", {
    skip_on_cran()

    out_dir <- file.path(tempdir(), "Basement")
    if (!dir.exists(out_dir)) dir.create(out_dir)

    path <- file.path(out_dir, "slab.idf")
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
    weather <- path_eplus_weather(8.8, "USA_CO_Golden-NREL.724666_TMY3.epw")

    res <- Slab(path, weather, eplus = 8.8, echo = FALSE,
        idd = path_eplus(8.8, "PreProcess", "GrndTempCalc", "SlabGHT.idd")
    )
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

    unlink(out_dir, recursive = TRUE)
    unlink(path)
})

test_that("EnergyPlus()", {
    skip_on_cran()

    out_dir <- file.path(tempdir(), "Energyplus")
    if (!dir.exists(out_dir)) dir.create(out_dir)

    path <- copy_eplus_example(8.8, "1ZoneUncontrolled.idf")
    weather <- path_eplus_weather(8.8, "USA_CO_Golden-NREL.724666_TMY3.epw")

    res <- EnergyPlus(path, weather, out_dir, eplus = 8.8, echo = FALSE)
    expect_equal(names(res), c("file", "run"))
    expect_equal(length(res$file), 51L)
    expect_equal(
        sort(names(which(vapply(res$file, function(x) !anyNA(x), logical(1))))),
        c("audit", "bnd", "dxf", "eio", "end", "epw", "err", "eso", "idf",
            "mdd", "mtd", "mtr", "rdd", "shd", "sqlite_err", "table")
    )
    expect_equal(names(res$run), c("process", "exit_status", "stdout",
        "stderr", "start_time", "end_time"))
    expect_false(is.null(res$run$stdout))

    # can use legacy output name style
    res_legacy <- EnergyPlus(path, weather, out_dir, output_suffix = "L", eplus = 8.8, echo = FALSE)
    files <- basename(unlist(res_legacy$file, FALSE, FALSE))
    files <- files[!is.na(files)]
    expect_equal(setdiff(
        names(tbl <- table(tools::file_path_sans_ext(files))),
        c("1ZoneUncontrolled", "eplusout", "eplustbl", "sqlite",
          "USA_CO_Golden-NREL.724666_TMY3")
    ), character())
    expect_equal(sort(as.integer(tbl)), c(1L, 1L, 1L, 5L, 12L))

    # can run annual simulation with custom IDD
    expect_silent(
        EnergyPlus(path, weather, out_dir, annual = TRUE, eplus = 8.8,
            echo = FALSE, idd = path_eplus(8.8, "Energy+.idd")
        )
    )

    unlink(out_dir, recursive = TRUE)
    unlink(path)
})

test_that("convertESOMTR()", {
    skip_on_cran()

    out_dir <- file.path(tempdir(), "convertESOMTR")
    if (!dir.exists(out_dir)) dir.create(out_dir)

    path <- copy_eplus_example(8.8, "1ZoneUncontrolled.idf")
    weather <- path_eplus_weather(8.8, "USA_CO_Golden-NREL.724666_TMY3.epw")

    res <- EnergyPlus(path, weather, out_dir, eplus = 8.8, echo = FALSE)

    eso <- convertESOMTR(res$file$eso, eplus = 8.8, echo = FALSE,
        rules = path_eplus(8.8, "PostProcess", "convertESOMTRpgm", "convert.txt")
    )
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

    unlink(out_dir, recursive = TRUE)
    unlink(path)
})

test_that("ReadVarsESO()", {
    skip_on_cran()

    out_dir <- file.path(tempdir(), "ReadVarsESO")
    if (!dir.exists(out_dir)) dir.create(out_dir)

    path <- copy_eplus_example(8.8, "1ZoneUncontrolled.idf")
    weather <- path_eplus_weather(8.8, "USA_CO_Golden-NREL.724666_TMY3.epw")

    res <- EnergyPlus(path, weather, out_dir, eplus = 8.8, echo = FALSE)

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

    unlink(out_dir, recursive = TRUE)
    unlink(path)
})

test_that("HVAC_Diagram()", {
    skip_on_cran()

    out_dir <- file.path(tempdir(), "HVAC_Diagram")
    if (!dir.exists(out_dir)) dir.create(out_dir)

    model <- copy_eplus_example(8.8, "1ZoneUncontrolled.idf")
    weather <- path_eplus_weather(8.8, "USA_CO_Golden-NREL.724666_TMY3.epw")

    res <- EnergyPlus(model, weather, out_dir, eplus = 8.8, echo = FALSE)

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

    unlink(out_dir, recursive = TRUE)
    unlink(model)
})

test_that("energyplus()", {
    skip_on_cran()

    out_dir <- file.path(tempdir(), "run-energyplus")
    if (!dir.exists(out_dir)) dir.create(out_dir)

    weather <- path_eplus_weather(8.8, "USA_CO_Golden-NREL.724666_TMY3.epw")

    # can run EPMacro
    path_imf <- copy_eplus_example(8.8, "AbsorptionChiller_Macro.imf")
    path_resources <- c(
        copy_eplus_example(8.8, "HVAC3ZoneChillerSpec.imf"),
        copy_eplus_example(8.8, "HVAC3ZoneGeometry.imf"),
        copy_eplus_example(8.8, "HVAC3Zone-IntGains-Def.imf"),
        copy_eplus_example(8.8, "HVAC3ZoneMat-Const.imf")
    )
    res_imf <- energyplus(path_imf, weather, out_dir, echo = FALSE, resources = path_resources)
    expect_equal(names(res_imf), c("version", "energyplus", "start_time", "end_time", "exit_status", "output_dir", "file", "run"))
    expect_equal(as.character(res_imf$version), "8.8.0")
    expect_equal(res_imf$energyplus, eplus_config(8.8)$dir)
    expect_is(res_imf$start_time, "POSIXct")
    expect_is(res_imf$end_time, "POSIXct")
    expect_equal(res_imf$output_dir, normalizePath(out_dir))
    expect_equal(length(res_imf$file), 57L)
    expect_equal({files <- unlist(res_imf$file); files <- files[!is.na(files)]; length(files)}, 24L)
    expect_equal(sum(!file.exists(file.path(out_dir, files))), 0L)
    expect_equal(sort(unname(files[names(files) != "epw"])),
        sort(c(list.files(out_dir, "AbsorptionChiller_Macro"), basename(path_resources)))
    )
    expect_is(res_imf$run, "data.table")
    expect_equal(names(res_imf$run),
        c("program", "exit_status", "start_time", "end_time", "stdout", "stderr")
    )
    expect_equal(res_imf$run$program,
        c("EPMacro", "ExpandObjects", "Basement", "Slab", "EnergyPlus",
          "convertESOMTR", "ReadVarsESO_MTR", "ReadVarsESO_ESO", "HVAC_Diagram")
    )
    expect_equal(res_imf$run[program == "EPMacro", exit_status], list(0L))
    expect_true(file.exists(file.path(out_dir, files["epmidf"])))
    expect_true(file.exists(file.path(out_dir, files["epmdet"])))
    expect_true(file.exists(file.path(out_dir, files["eso"])))
    expect_true(file.exists(file.path(out_dir, files["mtr"])))
    expect_true(file.exists(file.path(out_dir, files["bnd"])))
    expect_true(file.exists(file.path(out_dir, files["svg"])))
    unlink(c(path_imf, path_resources, out_dir), recursive = TRUE)

    # can run ExpandObjects
    path_exp <- copy_eplus_example(8.8, "HVACTemplate-5ZoneFanCoil.idf")
    res_exp <- energyplus(path_exp, weather, out_dir, echo = FALSE)
    expect_equal(length(res_exp$file), 57L)
    expect_equal({files <- unlist(res_exp$file); files <- files[!is.na(files)]; length(files)}, 23L)
    expect_equal(sum(!file.exists(file.path(out_dir, files))), 0L)
    expect_equal(sort(unname(files[names(files) != "epw"])), list.files(out_dir, "HVACTemplate-5ZoneFanCoil"))
    expect_equal(res_exp$run[program == "ExpandObjects", exit_status], list(0L))
    expect_true(file.exists(file.path(out_dir, files["expidf"])))
    expect_true(is.na(files["experr"]))
    unlink(c(path_exp, out_dir), recursive = TRUE)

    # can run Basement
    path_base <- copy_eplus_example(8.8, "LgOffVAVusingBasement.idf")
    expect_error(energyplus(path_base, NULL, out_dir, echo = FALSE))
    # modify the input in order to reduce the simulation time
    l <- read_lines(path_base)
    l[grepl("IYRS: Maximum number of yearly iterations", string), string := "1;"]
    write_lines(l, path_base)
    res_base <- energyplus(path_base, weather, out_dir, design_day = TRUE, echo = FALSE)
    expect_equal(length(res_base$file), 57L)
    expect_equal({files <- unlist(res_base$file); files <- files[!is.na(files)]; length(files)}, 27L)
    expect_equal(sum(!file.exists(file.path(out_dir, files))), 0L)
    expect_equal(sort(unname(files[names(files) != "epw"])), list.files(out_dir, "LgOffVAVusingBasement"))
    expect_equal(res_base$run[program == "Basement", exit_status], list(0L))
    expect_true(file.exists(file.path(out_dir, files["expidf"])))
    expect_true(file.exists(file.path(out_dir, files["bsmt_idf"])))
    expect_true(file.exists(file.path(out_dir, files["bsmt_csv"])))
    expect_true(file.exists(file.path(out_dir, files["bsmt_out"])))
    expect_true(file.exists(file.path(out_dir, files["bsmt_audit"])))
    unlink(c(path_base, out_dir), recursive = TRUE)

    # can run Slab
    path_slab <- copy_eplus_example(8.8, "5ZoneAirCooledWithSlab.idf")
    expect_error(energyplus(path_slab, NULL, out_dir, echo = FALSE))
    # modify the input in order to reduce the simulation time
    l <- read_lines(path_slab)
    l[grepl("IYRS: Number of years to iterate", string), string := "1,"]
    l[grepl("ConvTol: Convergence Tolerance", string), string := "1;"]
    write_lines(l, path_slab)
    res_slab <- energyplus(path_slab, weather, out_dir, echo = FALSE)
    expect_equal(length(res_slab$file), 57L)
    expect_equal({files <- unlist(res_slab$file); files <- files[!is.na(files)]; length(files)}, 26L)
    expect_equal(sum(!file.exists(file.path(out_dir, files))), 0L)
    expect_equal(sort(unname(files[names(files) != "epw"])), list.files(out_dir, "5ZoneAirCooledWithSlab"))
    expect_equal(res_slab$run[program == "Slab", exit_status], list(0L))
    expect_true(file.exists(file.path(out_dir, files["slab_ger"])))
    expect_true(file.exists(file.path(out_dir, files["slab_gtp"])))
    expect_true(file.exists(file.path(out_dir, files["slab_out"])))
    unlink(c(path_slab, out_dir), recursive = TRUE)

    # can convert eso to IP
    path_ip <- copy_eplus_example(8.8, "1ZoneUncontrolled.idf")
    res_ip <- energyplus(path_ip, weather, out_dir, eso_to_ip = TRUE, echo = FALSE)
    expect_equal(length(res_ip$file), 57L)
    expect_equal({files <- unlist(res_ip$file); files <- files[!is.na(files)]; length(files)}, 26L)
    expect_equal(sum(!file.exists(file.path(out_dir, files))), 0L)
    expect_equal(sort(unname(files[names(files) != "epw"])), list.files(out_dir, "1ZoneUncontrolled"))
    expect_equal(res_ip$run[program == "convertESOMTR", exit_status], list(0L))
    expect_true(file.exists(file.path(out_dir, files["ipeso"])))
    expect_true(file.exists(file.path(out_dir, files["ipmtr"])))
    expect_true(is.na(files["iperr"]))
    unlink(out_dir, recursive = TRUE)
    res_ip2 <- energyplus(path_ip, weather, out_dir, eso_to_ip = TRUE, readvars = FALSE, echo = FALSE)
    expect_equal(length(res_ip2$file), 57L)
    expect_equal({files2 <- unlist(res_ip2$file); files2 <- files2[!is.na(files2)]; length(files2)}, 23L)
    expect_equal(sum(!file.exists(file.path(out_dir, files2))), 0L)
    expect_true(file.exists(file.path(out_dir, files2["ipeso"])))
    expect_true(file.exists(file.path(out_dir, files2["ipmtr"])))
    expect_true(is.na(files2["iperr"]))
    unlink(out_dir, recursive = TRUE)

    # can run with EnergyPlus < 8.3
    options("eplusr.eplus_legacy" = TRUE)
    path_legacy <- copy_eplus_example(8.8, "1ZoneUncontrolled.idf")
    expect_error(energyplus(path_legacy, weather, out_dir, design_day = TRUE, echo = FALSE))
    res_legacy <- energyplus(path_legacy, weather, out_dir, echo = FALSE)
    expect_equal(length(res_legacy$file), 57L)
    expect_equal({files <- unlist(res_legacy$file); files <- files[!is.na(files)]; length(files)}, 24L)
    expect_equal(sum(!file.exists(file.path(out_dir, files))), 0L)
    expect_equal(sort(unname(files[names(files) != "epw"])), list.files(out_dir, "1ZoneUncontrolled"))
    unlink(out_dir, recursive = TRUE)
    options("eplusr.eplus_legacy" = NULL)
    expect_error(energyplus(path_legacy, weather, out_dir, design_day = TRUE, annual = TRUE, echo = FALSE))
    res_norm <- energyplus(path_legacy, weather, out_dir, echo = FALSE)
    expect_equal(res_legacy$file, res_norm$file)

    unlink(out_dir, recursive = TRUE)
})

test_that("run_idf()", {
    skip_on_cran()

    path_idf <- copy_eplus_example(8.8, "1ZoneUncontrolled.idf")
    path_epw <- path_eplus_weather(8.8, "USA_CO_Golden-NREL.724666_TMY3.epw")

    # can run ddy simulation
    expect_is(res <- run_idf(path_idf, NULL, output_dir = tempdir(), echo = FALSE), "list")
    # can specify EnergyPlus version
    expect_is(res <- run_idf(path_idf, NULL, output_dir = tempdir(), echo = FALSE, eplus = 8.8), "list")
    expect_null(res$epw)
    # can stop if failed to find version
    expect_error({
        f <- tempfile(fileext = ".idf")
        write_lines(read_lines(path_idf)[-91], f)
        run_idf(f, NULL, output_dir = tempdir(), echo = FALSE)
    }, "Missing version field")
    # can use input file directory
    expect_is({
        f <- tempfile(fileext = ".idf")
        file.copy(path_idf, f, overwrite = TRUE)
        res <- run_idf(f, NULL, output_dir = NULL, echo = FALSE)
    }, "list")

    # can run simulation with weather
    expect_is(res <- run_idf(path_idf, path_epw, output_dir = tempdir(), echo = FALSE), "list")
    expect_equal(res$idf, normalizePath(path_idf))
    expect_equal(res$epw, normalizePath(path_epw))
    expect_equal(res$version, numeric_version("8.8.0"))
    expect_equal(res$exit_status, 0L)
    expect_is(res$start_time, "POSIXct")
    expect_is(res$end_time, "POSIXct")
    expect_equal(res$output_dir, normalizePath(tempdir(), mustWork = FALSE))
    expect_equal(res$energyplus, normalizePath(file.path(eplus_config(8.8)$dir, eplus_config(8.8)$exe), mustWork = TRUE))
    expect_is(res$stdout, "character")
    expect_true("stderr" %in% names(res))
    expect_is(res$process, "process")
    expect_true(file.exists(res$idf))
    expect_true(file.exists(res$epw))

    # can run in the background
    expect_is(proc <- run_idf(path_idf, NULL, output_dir = tempdir(), wait = FALSE), "r_process")
    expect_is({proc$wait(); res <- proc$get_result()}, "list")
    expect_equal(res$idf, normalizePath(path_idf))
    expect_null(res$epw)
    expect_equal(res$version, numeric_version("8.8.0"))
    expect_equal(res$exit_status, 0L)
    expect_is(res$start_time, "POSIXct")
    expect_is(res$end_time, "POSIXct")
    expect_equal(res$output_dir, normalizePath(tempdir(), mustWork = FALSE))
    expect_equal(res$energyplus, normalizePath(file.path(eplus_config(8.8)$dir, eplus_config(8.8)$exe), mustWork = TRUE))
    expect_is(res$stdout, "character")
    expect_null(res$process)
})

test_that("run_multi()", {
    skip_on_cran()

    path_idf <- copy_eplus_example(8.8, c("1ZoneUncontrolled.idf", "5Zone_Transformer.idf"))
    path_epw <- path_eplus_weather(8.8, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    # can stop if idf and epw does not have the same length
    expect_error(run_multi(path_idf, rep(path_epw, 3), NULL), "Must have same length")
    # can stop if idf and eplus does not have the same length
    expect_error(run_multi(path_idf, NULL, NULL, eplus = rep(8.8, 3)), "Must have same length")
    # can stop if idf and design does not have the same length
    expect_error(run_multi(path_idf, NULL, NULL, design_day = rep(FALSE, 3)), "Must have same length")
    # can stop if idf and annual does not have the same length
    expect_error(run_multi(path_idf, NULL, NULL, annual = rep(FALSE, 3)), "Must have same length")
    # can stop if both design and annual is TRUE
    expect_error(run_multi(path_idf, NULL, NULL, annual = TRUE, design_day = TRUE), "both design-day-only", class = "eplusr_error_both_ddy_annual")
    # can stop if model does not exist
    expect_error(run_multi(tempfile(), NULL))
    # can stop if model does not contain version
    expect_error({
        f <- tempfile(fileext = ".idf")
        write_lines(read_lines(path_idf[1])[-91], f)
        run_multi(f, NULL, output_dir = tempdir())
    }, "Failed to determine the version of EnergyPlus", class = "eplusr_error_miss_idf_ver")
    # can stop if target EnergyPlus is not found
    expect_error(run_multi(path_idf, NULL, NULL, eplus = 8.0), "Cannot locate EnergyPlus", class = "eplusr_error_locate_eplus")
    # can stop if input idf contain duplications
    expect_error(run_multi(rep(path_idf, 2), NULL, NULL), class = "eplusr_error_duplicated_sim")
    # can stop if idf and output directory does not have the same length
    expect_error(run_multi(path_idf, NULL, output_dir = tempdir()), "Must have same length")
    # can stop if idf and output directory combines same job
    expect_error(run_multi(rep(path_idf[1], 2), NULL, rep(tempdir(), 2L)), "Duplication found")

    expect_message(
        res <- run_multi(path_idf, NULL, file.path(tempdir(), c("a", "b"))),
        "FAILED"
    )
    expect_is(res, "data.table")
    expect_equal(names(res), c("index", "status", "idf", "epw", "version",
            "exit_status", "start_time", "end_time", "output_dir", "energyplus",
            "stdout", "stderr"))
    expect_equal(res$index, 1:2)
    expect_equal(res$status, rep("failed", 2))
    expect_equal(res$idf, normalizePath(path_idf))
    expect_equal(res$epw, rep(NA_character_, 2))
    expect_equal(res$version, rep("8.8.0", 2))
    expect_equal(res$exit_status > 0, rep(TRUE, 2))
    expect_is(res$start_time, "POSIXct")
    expect_is(res$end_time, "POSIXct")
    expect_equal(res$energyplus, rep(normalizePath(file.path(eplus_config(8.8)$dir, eplus_config(8.8)$exe), mustWork = TRUE), 2L))
    checkmate::expect_list(res$stdout, "character")

    expect_silent(res <- run_multi(path_idf, NULL, file.path(tempdir(), c("a", "b")), wait = FALSE))
    expect_is(res, "r_process")
    expect_equal({res$wait(); res$get_exit_status()}, 0L)
    expect_silent(res <- res$get_result())
    expect_is(res, "data.table")
    expect_equal(names(res), c("index", "status", "idf", "epw", "version",
            "exit_status", "start_time", "end_time", "output_dir", "energyplus",
            "stdout", "stderr"))
    expect_equal(res$index, 1:2)
    expect_equal(res$status, rep("failed", 2))
    expect_equal(res$idf, normalizePath(path_idf, mustWork = FALSE))
    expect_equal(res$epw, rep(NA_character_, 2))
    expect_equal(res$version, rep("8.8.0", 2))
    expect_equal(res$exit_status > 0, rep(TRUE, 2))
    expect_is(res$start_time, "POSIXct")
    expect_is(res$end_time, "POSIXct")
    expect_equal(res$output_dir, normalizePath(file.path(tempdir(), c("a", "b")), mustWork = FALSE))
    expect_equal(res$energyplus, rep(normalizePath(file.path(eplus_config(8.8)$dir, eplus_config(8.8)$exe), mustWork = TRUE), 2L))
    checkmate::expect_list(res$stdout, "character")
})
