test_that("Install", {
    skip_on_cran()

    expect_equal(as.character(avail_eplus()), names(.globals$eplus))

    if (is_avail_eplus(8.8)) {
        expect_error(install_eplus(8.8, local = TRUE))
    } else {
        install_eplus(8.8, local = TRUE)
    }

    # test if patch on EnergyPlus v9.1 and above works
    if (!is_avail_eplus(LATEST_EPLUS_VER)) {
        expect_equivalent(res <- install_eplus(LATEST_EPLUS_VER, local = TRUE), 0L)
        installer <- attr(res, "installer")

        # can update EnergyPlus config
        expect_true(is_avail_eplus(LATEST_EPLUS_VER))

        # can uninstall EnergyPlus
        expect_equivalent(uninstall_eplus(LATEST_EPLUS_VER), 0L)

        # can remove EnergyPlus config
        expect_false(LATEST_EPLUS_VER %in% as.character(avail_eplus()))

        # still need latest EnergyPlus for testing transitions
        install_eplus_from_file(LATEST_EPLUS_VER, installer, TRUE)
        # refresh config database
        expect_is(locate_eplus(), "numeric_version")
    }
})
