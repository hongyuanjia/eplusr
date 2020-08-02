test_that("eplusr_option()", {
    expect_error(eplusr_option(validate = TRUE), "Must be a subset")
    expect_error(eplusr_option(validate_level = "wrong"), "Must be element")
    expect_error(eplusr_option(view_in_ip = 1), "Must be of type")

    expect_equal(eplusr_option(validate_level = custom_validate(required_object = TRUE)),
        list(validate_level = custom_validate(required_object = TRUE)))
    expect_equal(eplusr_option(validate_level = "final"), list(validate_level = "final"))
    expect_equal(eplusr_option(view_in_ip = FALSE), list(view_in_ip = FALSE))
    expect_equal(eplusr_option(save_format = "asis"), list(save_format = "asis"))
    expect_equal(eplusr_option(num_parallel = 8L), list(num_parallel = 8L))
    expect_equal(eplusr_option(verbose_info = TRUE), list(verbose_info = TRUE))
    expect_equal(eplusr_option(autocomplete = TRUE), list(autocomplete = TRUE))

    expect_equal(eplusr_option(),
        list(autocomplete = TRUE,
             num_parallel = 8L,
             save_format = "asis",
             validate_level = "final",
             verbose_info = TRUE,
             view_in_ip = FALSE
        )
    )

    expect_false(with_option(list(verbose_info = FALSE), eplusr_option("verbose_info")))
    expect_false(with_silent(eplusr_option("verbose_info")))
    expect_message(with_verbose(verbose_info("a")), "a")
    expect_equal(without_checking(eplusr_option("validate_level")), "none")
    expect_equal(with_speed(c(eplusr_option("validate_level"), eplusr_option("autocomplete"))), c("none", "FALSE"))
})

