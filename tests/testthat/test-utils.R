test_that("Utility functions", {
    expect_equal(NULL %||% 1, 1)
    expect_equal(1 %||% 2, 1)

    expect_equal(collapse(1:3), "'1', '2' and '3'")
    expect_equal(collapse(1:3, out = NULL), "1, 2 and 3")
    expect_equal(collapse(1, out = NULL), "1")
    expect_equal(collapse(1:3, or = NULL), "'1', '2', '3'")
    expect_equal(collapse(1:3, or = TRUE), "'1', '2' or '3'")

    expect_equal(surround(1:3), c("'1'", "'2'", "'3'"))
    expect_equal(surround(1:3, out = NULL), as.character(1:3))

    expect_null(rpad(NULL))
    expect_equal(rpad(c(1, 10)), c("1 ", "10"))
    expect_equal(rpad(c(1, 10), width = 3), c("1  ", "10 "))

    expect_null(lpad(NULL))
    expect_equal(lpad(c(1, 10)), c(" 1", "10"))
    expect_equal(lpad(c(1, 10), width = 3), c("  1", " 10"))

    expect_error(read_lines(NULL), "Failed to read input file", "eplusr_error_read_lines")
    expect_equal(read_lines("a\n b \n c \n"), data.table(line = 1:3, string = c("a", "b", "c")))

    f <- tempfile()
    expect_silent(write_lines(read_lines("a\nb"), f))
    expect_equal(read_lines(f), data.table(line = 1:2, string = c("a", "b")))
    expect_silent(write_lines(c("a", "b"), f))
    expect_equal(read_lines(f), data.table(line = 1:2, string = c("a", "b")))
    expect_silent(write_lines(c("c", "d"), f, append = TRUE))
    expect_equal(read_lines(f), data.table(line = 1:4, string = c("a", "b", "c", "d")))
    expect_error(write_lines(1:3, f), "Must be of type 'character'")

    expect_equal(read_lines(test_path("file/latin1"))$string[1], "N13, \\field Young’s modulus")
    expect_equal(underscore_name(read_lines(test_path("file/latin1"), F)$string)[1], "N13_field_Young_s_modulus")

    expect_equal(standardize_ver("latest"), numeric_version("9.4.0"))
    expect_equal(standardize_ver("latest", strict = TRUE), numeric_version(NA, strict = FALSE))
    expect_equal(standardize_ver(c(1, 1.1)), numeric_version(c("1.0.0", "1.1.0")))
    expect_equal(standardize_ver(c(1, 1.1), complete = FALSE), numeric_version(c("1.0", "1.1")))
    expect_equal(standardize_ver(c("1.0", "1.1.0.01")), numeric_version(c("1.0.0", "1.1.0")))
    expect_equal(standardize_ver(c("1.0", "1.1.0.01"), complete = FALSE), numeric_version(c("1.0", "1.1.0")))

    expect_error(match_minor_ver(1), "numeric_version")
    expect_error(match_minor_ver(numeric_version(1:2)), "length 1")
    expect_equal(match_minor_ver(numeric_version("0.0"), NULL), numeric_version(NA, strict = FALSE))
    expect_equal(match_minor_ver(numeric_version("0.0"), ALL_IDD_VER), numeric_version(NA, strict = FALSE))
    expect_equal(match_minor_ver(numeric_version("9.1"), ALL_IDD_VER), numeric_version("9.1.0"))
    expect_equal(match_minor_ver(numeric_version("9.0"), ALL_IDD_VER), numeric_version("9.0.1"))
    expect_equal(match_minor_ver(numeric_version("9.0"), ALL_EPLUS_VER, max = FALSE, verbose = FALSE),
        numeric_version(c("9.0.0", "9.0.1")))
    expect_equal(match_minor_ver(numeric_version("9.0.1"), ALL_EPLUS_VER), numeric_version("9.0.1"))

    expect_equal(vec_depth(NULL), 0L)
    expect_equal(vec_depth(character()), 1L)
    expect_equal(vec_depth(list()), 1L)
    expect_equal(vec_depth(list(list())), 2L)
    expect_equal(vec_depth(list(list(NULL))), 2L)
    expect_equal(vec_depth(list(list(1))), 3L)
    expect_error(vec_depth(environment()), "must be a vector")

    expect_equal(vlapply(1:3, is.integer), rep(TRUE, 3L))
    expect_equal(vlapply(setNames(1:3, c("a", "b", "c")), is.integer), setNames(rep(TRUE, 3L), c("a", "b", "c")))
    expect_equal(vlapply(setNames(1:3, c("a", "b", "c")), is.integer, use.names = FALSE), rep(TRUE, 3L))

    expect_equal(viapply(1:3, length), rep(1L, 3L))
    expect_equal(viapply(setNames(1:3, c("a", "b", "c")), length), setNames(rep(1L, 3L), c("a", "b", "c")))
    expect_equal(viapply(setNames(1:3, c("a", "b", "c")), length, use.names = FALSE), rep(1L, 3L))

    expect_equal(vcapply(1:3, paste), as.character(1:3))
    expect_equal(vcapply(setNames(1:3, c("a", "b", "c")), paste), setNames(as.character(1:3), c("a", "b", "c")))
    expect_equal(vcapply(setNames(1:3, c("a", "b", "c")), paste, use.names = FALSE), as.character(1:3))

    expect_equal(apply2(1:3, 4:6, "+"), list(5L, 7L, 9L))
    expect_equal(apply2_int(1:3, 4:6, "+"), c(5L, 7L, 9L))
    expect_equal(apply2_lgl(1:3, 4:6, ">"), rep(FALSE, 3L))
    expect_equal(apply2_chr(1:3, 4:6, paste0), c("14", "25", "36"))

    expect_equal(underscore_name("Class Name"), "Class_Name")
    expect_equal(underscore_name("Class:Name"), "Class_Name")
    expect_equal(lower_name("Class:Name"), "class_name")
    expect_equal(lower_name("Class Name"), "class_name")

    expect_equal(make_filename(c("<a?:\\>", "<a?:\\>")), c("_a____", "_a_____1"))
    expect_equal(make_filename(c("<a?:\\>", "<a?:\\>"), unique = FALSE), c("_a____", "_a____"))

    expect_equal(names2(1:3), rep(NA_character_, 3))
    expect_equal(names2(c(a = 1, 2)), c("a", NA_character_))

    expect_equal(each_length(list(1, 2:3)), c(1L, 2L))

    expect_equal(ranger()[], list(minimum = -Inf, lower_incbounds = FALSE, maximum = Inf, upper_incbounds = FALSE))
    expect_equal(format(ranger()), "(-Inf, Inf)")
    expect_equal(ranger(1, TRUE)[], list(minimum = 1, lower_incbounds = TRUE, maximum = Inf, upper_incbounds = FALSE))
    expect_equal(format(ranger(1, TRUE)), "[1, Inf)")

    expect_equal(fmt_dbl(1.111), "1.11")
    expect_equal(fmt_dbl(1.111, 1), "1.1")
    expect_equal(fmt_int(1), "1.0")
    expect_equal(fmt_int(1, 0), "1")

    expect_equal(wday(as.Date("2020-01-01")), 3L)
    expect_equal(as.character(wday(as.Date("2020-01-01"), label = TRUE)), "Wednesday")

    expect_equal(str_trunc("abcdefghij", 10), "abcde...")

    expect_equal(match_in_vec("a", LETTERS), 1L)
    expect_equal(match_in_vec("a", LETTERS, "aa"), 1L)
    expect_equal(match_in_vec("a", LETTERS, "aa", label = TRUE), "A")
    expect_equal(match_in_vec("aa", LETTERS, "aa", label = TRUE), "A")

    expect_is(catch_cnd(warn("")), "eplusr_warning")
    expect_is(catch_cnd(warn("", class = "empty")), "eplusr_warning_empty")
})
