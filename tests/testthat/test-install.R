test_that("Install", {
    skip_on_cran()
    expect_equal(as.character(avail_eplus()), names(.globals$eplus))

    if (is_avail_eplus(8.8)) expect_error(install_eplus(8.8, local = TRUE))
    else install_eplus(8.8, local = TRUE)

    # test if patch on EnergyPlus v9.1 and above works
    if (!is_avail_eplus(9.5)) {
        expect_equivalent(res <- install_eplus(9.5, local = TRUE), 0L)
        installer <- attr(res, "installer")

        # can update EnergyPlus config
        expect_true(is_avail_eplus(9.5))

        # can uninstall EnergyPlus
        expect_equivalent(uninstall_eplus(9.5), 0L)

        # can remove EnergyPlus config
        expect_false("9.5.0" %in% as.character(avail_eplus()))

        # still need EnergyPlus v9.5 for testing transitions
        install_eplus_from_file(9.5, installer, TRUE)
        # refresh config database
        expect_is(locate_eplus(), "numeric_version")
    }
})
