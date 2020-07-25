context("Assertions")

test_that("list checking", {
    expect_equal(convert_to_eplus_ver(8), numeric_version("8.0.0"))
    expect_equal(convert_to_eplus_ver(c(8, 8.1), max = TRUE), numeric_version(c("8.0.0", "8.1.0")))

    expect_equal(convert_to_idd_ver(8), numeric_version("8.0.0"))
    expect_equal(convert_to_idd_ver(c(8, 8.1), max = TRUE), numeric_version(c("8.0.0", "8.1.0")))

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
    expect_true(is_idd(use_idd(8.8)))

    expect_false(is_idf(1))
    expect_true(is_idf(read_idf(example())))

    expect_false(is_iddobject(1))
    expect_true(is_iddobject(use_idd(8.8)$Version))

    expect_false(is_idfobject(1))
    expect_true(is_idfobject(read_idf(example())$Version))

    expect_false(is_epw(1))

    expect_true(is_rdd(structure(data.table(), class = "RddFile")))
    expect_true(is_mdd(structure(data.table(), class = "MddFile")))

    expect_true(is_range(ranger()))

    expect_error(assert_strint(1))
    expect_error(assert_strint("a"))
    expect_equal(assert_strint("1"), "1")
    expect_equal(assert_strint("1", coerce = TRUE), 1L)

    expect_true(has_len(1:2, 2))
    expect_true(has_len(1:2, ranger(0)))
    expect_true(has_len(1:2, c(1,2)))
    expect_true(has_len(1:2, 1, 1))
    expect_false(has_len(1:2, 3))
    expect_false(has_len(1:2, c(0,4)))
    expect_false(has_len(1:2, 1, 2))
    expect_true(have_same_len(1:2, 3:4))
    expect_true(have_same_len(mtcars, seq_len(nrow(mtcars))))
    expect_false(have_same_len(1, 1:2))

    expect_true(in_range(1, ranger(1, TRUE, 2, FALSE)))
    expect_false(in_range(1, ranger(1, FALSE, 2, FALSE)))

    expect_true(is_named(list(a = 1)))
    expect_false(is_named(list(1)))

    expect_true(is_choice("yes", c("Yes", "no")))
    expect_true(is_choice("yes", c("Yes", "no")))

    expect_true(has_name(c(a = 1), "a"))
    expect_false(has_name(c(a = 1), "b"))
    expect_true(has_name(c(a = 1, b = 2), "a"))
    expect_false(has_name(c(a = 1, b = 2), c("b", "c")))

    expect_true(has_ext(tempfile(fileext = ".idf"), "idf"))
    expect_false(has_ext(tempfile(fileext = ".idf"), "epw"))

    expect_true(has_ext(tempfile(fileext = ".idf"), c("idf", "imf")))
    expect_false(has_ext(tempfile(fileext = ".idf"), c("epw", "imf")))

    expect_true(is_epwdate(epw_date(1)))
    expect_false(is_epwdate(epw_date(-1)))

    expect_true(are_epwdate(epw_date(0:5)))
    expect_false(are_epwdate(epw_date(-1:5)))

    expect_true(assert(8 > 5, 2 > 1))
    expect_is(tryCatch(assert(is_scalar(1:2)), error = identity), "error_not_scalar")
    expect_error(assert(is_scalar(1:2)), class = "error_not_scalar")
    expect_error(assert(is_scalar(1:2), msg = "a"), "a", class = "error_not_scalar")
    expect_error(assert(is_scalar(1:2), prefix = "input"), "input is not a scalar", class = "error_not_scalar")
})
