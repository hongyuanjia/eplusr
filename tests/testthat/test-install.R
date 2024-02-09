test_that("Install EnergyPlus v9.0 and below", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_INSTALL_OLD_") != "")
    skip_if_not(testthat:::on_ci())

    expect_equal(sort(as.character(avail_eplus())), sort(names(.globals$eplus)))

    if (is_avail_eplus("8.8")) {
        expect_error(install_eplus("8.8", local = TRUE))
    } else {
        install_eplus("8.8", local = TRUE)
    }
})

test_that("Install EnergyPlus v9.1 and above", {
    skip_on_cran()
    skip_if_not(testthat:::on_ci())

    expect_equal(sort(as.character(avail_eplus())), sort(names(.globals$eplus)))

    # test if patch on EnergyPlus v9.1 and above works
    if (!is_avail_eplus(LATEST_EPLUS_VER)) {
        expect_equal(ignore_attr = TRUE,
            res <- install_eplus(LATEST_EPLUS_VER, local = TRUE),
            0L
        )
        installer <- attr(res, "installer")

        # can update EnergyPlus config
        expect_true(is_avail_eplus(LATEST_EPLUS_VER))

        # can uninstall EnergyPlus
        expect_equal(ignore_attr = TRUE, uninstall_eplus(LATEST_EPLUS_VER), 0L)

        # can remove EnergyPlus config
        expect_false(LATEST_EPLUS_VER %in% as.character(avail_eplus()))

        # still need latest EnergyPlus for testing transitions
        install_eplus_from_file(LATEST_EPLUS_VER, installer, TRUE)
        # refresh config database
        expect_s3_class(locate_eplus(), "numeric_version")
    }
})

# vim: set fdm=marker:
