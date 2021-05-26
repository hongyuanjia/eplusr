test_that("Assertion functions", {
    expect_equal(convert_to_eplus_ver(8), numeric_version("8.0.0"))
    expect_equal(convert_to_eplus_ver(c(8, 9), max = TRUE), numeric_version(c("8.0.0", "9.0.1")))
    expect_equal(convert_to_eplus_ver(c(8, 9), max = FALSE), list(numeric_version(c("8.0.0")), numeric_version(c("9.0.0", "9.0.1"))))

    expect_equal(convert_to_idd_ver(8), numeric_version("8.0.0"))
    expect_equal(convert_to_idd_ver(c(8, 9), max = TRUE), numeric_version(c("8.0.0", "9.0.1")))
    expect_equal(convert_to_idd_ver(c(8, 9), max = FALSE), list(numeric_version(c("8.0.0")), numeric_version(c("9.0.0", "9.0.1"))))

    expect_false(is_eplus_ver("a"))
    expect_true(is_eplus_ver(8))
    expect_true(is_eplus_ver(8.5))
    expect_true(is_eplus_ver("latest"))
    expect_false(is_eplus_ver("latest", strict = TRUE))
    expect_false(is_eplus_ver("8.8.8"))

    expect_false(is_idd_ver("a"))
    expect_true(is_idd_ver(8))
    expect_true(is_idd_ver(8.5))
    expect_true(is_idd_ver("latest"))
    expect_false(is_idd_ver("latest", strict = TRUE))
    expect_false(is_idd_ver("8.8.8"))

    expect_false(is_eplus_path(tempfile()))
    expect_true({
        file.create(file.path(tempdir(), "Energy+.idd"))
        file.create(file.path(tempdir(), "energyplus"))
        file.create(file.path(tempdir(), "energyplus.exe"))
        is_eplus_path(tempdir())
    })

    expect_false(is_idd(1))
    expect_true(is_idd(use_idd(8.8, download = "auto")))

    expect_false(is_idf(1))
    expect_true(is_idf(read_idf(example())))

    expect_false(is_iddobject(1))
    expect_true(is_iddobject(use_idd(8.8)$Version))

    expect_false(is_idfobject(1))
    expect_true(is_idfobject(read_idf(example())$Version))

    expect_false(is_epw(1))

    expect_true(is_rdd(structure(data.table(), class = "RddFile")))
    expect_true(is_mdd(structure(data.table(), class = "MddFile")))

    expect_error(assert_same_len(1:5, 1))
    expect_equal(assert_same_len(1:5, 1:5), 1:5)

    expect_true(in_range(1, ranger(1, TRUE, 2, FALSE)))
    expect_false(in_range(1, ranger(1, FALSE, 2, FALSE)))
    expect_false(in_range(1, ranger(1, FALSE, 2, TRUE)))

    expect_true(has_names(c(a = 1), "a"))
    expect_false(has_names(c(a = 1), "b"))
    expect_true(has_names(c(a = 1, b = 2), "a"))
    expect_equal(has_names(c(a = 1, b = 2), c("b", "c")), c(TRUE, FALSE))

    expect_true(has_ext(tempfile(fileext = ".idf"), "idf"))
    expect_false(has_ext(tempfile(fileext = ".idf"), "epw"))

    expect_true(is_epwdate(epw_date(1)))
    expect_false(is_epwdate(epw_date(-1)))

    expect_is(is_windows(), "logical")
    expect_is(is_linux(), 'logical')
    expect_is(is_macos(), 'logical')
})
