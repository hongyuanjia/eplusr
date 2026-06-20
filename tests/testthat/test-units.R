test_that("units package is optional until units support is requested", {
    expect_false(with_mocked_bindings(
        reg_custom_units(),
        has_units = function() FALSE
    ))
    expect_error(
        with_mocked_bindings(
            check_units("add units to data"),
            has_units = function() FALSE
        ),
        class = "eplusr_error_missing_units"
    )

    dt_value <- data.table(
        value_id = 1L,
        value_chr = "NoUnits",
        value_num = NA_real_,
        type_enum = IDDFIELD_TYPE$alpha,
        units = NA_character_,
        ip_units = NA_character_,
        field_name = "Name"
    )

    expect_equal(with_mocked_bindings(
        get_value_list(dt_value, unit = TRUE),
        check_units = function(...) abort("Should not need units.", "called")
    ), list(Name = "NoUnits"))

    expect_equal(with_mocked_bindings(
        convert_value_unit(NULL, dt_value, "si", "ip"),
        check_units = function(...) abort("Should not need units.", "called")
    ), dt_value)
})

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

# vim: set fdm=marker:
