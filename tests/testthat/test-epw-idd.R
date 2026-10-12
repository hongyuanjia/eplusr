# A fresh weather dictionary must follow the non-cloneable Idd inheritance contract.
test_that("weather dictionaries instantiate without cloneability diagnostics", {
    expect_false(EpwIdd$cloneable)
    for (version in c("before_2021", "after_2021")) {
        expect_silent(
            idd <- EpwIdd$new(
                system.file("extdata/epw.idd", package = "eplusr"),
                version = version
            )
        )
        expect_s3_class(idd, "Idd")
        expect_null(idd$clone)
        expect_identical(idd$version_specs(), version)
    }
})
