test_that("eplusr_option()", {
    expect_error(eplusr_option(validate = TRUE), class = "error_not_%in%")

    expect_error(eplusr_option(validate_level = "wrong"))

    expect_error(eplusr_option(view_in_ip = 1), class = "error_not_flag")

    expect_warning(eplusr_option(num_digits = "a"))

    expect_equal(eplusr_option(validate_level = custom_validate(required_object = TRUE)),
        list(validate_level = custom_validate(required_object = TRUE)))
    expect_equal(eplusr_option(validate_level = "final"), list(validate_level = "final"))
    expect_equal(eplusr_option(view_in_ip = FALSE), list(view_in_ip = FALSE))
    expect_equal(eplusr_option(save_format = "asis"), list(save_format = "asis"))
    expect_equal(eplusr_option(num_parallel = 8L), list(num_parallel = 8L))
    expect_equal(eplusr_option(verbose_info = TRUE), list(verbose_info = TRUE))

    expect_equal(eplusr_option(),
        list(validate_level = "final",
             view_in_ip = FALSE,
             save_format = "asis",
             num_parallel = 8L,
             verbose_info = TRUE))
})

