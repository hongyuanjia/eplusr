context("Assertions")

test_that("list checking", {
    expect_true(is_version(8))
    expect_true(is_version(8.8))
    expect_true(is_version("8.8"))
    expect_false(is_version("8.a"))

    expect_false(is_eplus_ver("a"))
    expect_false(is_eplus_ver(8))
    expect_true(is_eplus_ver(8.5))
    expect_true(is_eplus_ver("latest"))
    expect_false(is_eplus_ver("8.8.8"))

    expect_false(is_eplus_path(tempfile()))
    expect_true({
        file.create(file.path(tempdir(), "Energy+.idd"))
        file.create(file.path(tempdir(), "energyplus"))
        file.create(file.path(tempdir(), "energyplus.exe"))
        is_eplus_path(tempdir())
    })

    expect_false(not_empty(NULL))
    expect_false(not_empty(data.frame()))
    expect_true(is_empty(NULL))
    expect_true(is_empty(data.frame()))

    expect_true(is_integer(8))
    expect_false(is_integer(8.1))

    expect_equal(are_integer(c(8, 0, NA_integer_)), FALSE)
    expect_true(is_count(8))
    expect_true(is_count(8.0))
    expect_false(is_count(8.1))
    expect_false(is_count(0))
    expect_true(is_count(0, zero = TRUE))
    expect_false(is_count(c(8, 8)))
    expect_false(is_count(NA_integer_))

    expect_equal(are_count(8.0), TRUE)
    expect_equal(are_count(c(0, 1, 8, NA_integer_)), FALSE)
    expect_equal(are_count(c(0, 8), zero = TRUE), TRUE)

    expect_true(is_string("a"))
    expect_false(is_string(1L))
    expect_false(is_string(NA))
    expect_false(is_string(NA_character_))

    expect_true(is_flag(TRUE))
    expect_false(is_flag(NA))
    expect_false(is_flag(0))
    expect_false(is_flag(c(TRUE, TRUE)))

    expect_true(is_scalar("a"))
    expect_false(is_scalar(NULL))
    expect_false(is_scalar(list()))
    expect_false(is_scalar(1:2))

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
