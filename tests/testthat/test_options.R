test_that("eplusr_option()", {
    expect_error(eplusr_option(validate = TRUE), "Invalid option name found")

    expect_error(eplusr_option(validate_level = "wrong"))

    ops <- list(validate_level = "draft",
        view_in_ip = FALSE,
        save_format = "sorted")

    expect_equal(eplusr_option(ops), ops)

    expect_error(eplusr_option(view_in_ip = 1),
        "`view_in_ip` should be one of either `TRUE` or `FALSE`")

    expect_error(eplusr_option(num_digits = "a"), "is not a count")

    expect_equal(eplusr_option(validate_level = "final"), list(validate_level = "final"))
    expect_equal(eplusr_option(view_in_ip = FALSE), list(view_in_ip = FALSE))
    expect_equal(eplusr_option(save_format = "asis"), list(save_format = "asis"))
    expect_equal(eplusr_option(num_parallel = 8L), list(num_parallel = 8L))
    expect_equal(eplusr_option(verbose_info = TRUE), list(verbose_info = TRUE))
    expect_equal(eplusr_option(num_digits = 8L), list(num_digits = 8L))

    expect_equal(eplusr_option(),
        list(validate_level = "final",
             view_in_ip = FALSE,
             save_format = "asis",
             num_parallel = 8L,
             verbose_info = TRUE,
             num_digits = 8L))
})

