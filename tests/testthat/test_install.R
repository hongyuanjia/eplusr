test_that("Install", {
    skip_on_cran()
    expect_equal(as.character(avail_eplus()), names(.globals$eplus_config))
    if (is_avail_eplus(8.8)) expect_error(install_eplus(8.8, local = TRUE))
    if (is_macos()) expect_error(install_eplus(8.8, local = TRUE))
    else install_eplus(8.8, local = TRUE, force = TRUE)
})
