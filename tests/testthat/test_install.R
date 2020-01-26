test_that("Install", {
    skip_on_cran()
    expect_equal(as.character(avail_eplus()), names(.globals$eplus_config))
    if (is_avail_eplus(8.8)) expect_error(install_eplus(8.8, local = TRUE))
    if (is_macos()) expect_error(install_eplus(8.8, local = TRUE))
    else install_eplus(8.8, local = TRUE, force = TRUE)

    # test if patch on EnergyPlus v9.1 and above works
    if (!is_avail_eplus(9.1)) install_eplus(9.1, local = TRUE)
    if (!is_avail_eplus(9.2)) install_eplus(9.2, local = TRUE)
    expect_true(is_avail_eplus(9.1))
    expect_true(is_avail_eplus(9.2))
})
