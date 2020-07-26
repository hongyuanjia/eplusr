test_that("Units conversion", {
    expect_silent(reg_custom_units())

    expect_equal(units::set_units(1, "person") + units::set_units(1, "person"),
        units::set_units(2, "person")
    )
    expect_equal(units::set_units(1, "dollar") + units::set_units(1, "dollar"),
        units::set_units(2, "dollar")
    )
    expect_equal(units::set_units(units::set_units(1, "inH2O"), "inch_H2O_39F"),
        units::set_units(1, "inch_H2O_39F")
    )
})
