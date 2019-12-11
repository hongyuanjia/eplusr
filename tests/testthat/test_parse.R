context("parse IDD and IDF files")

# parse_idd_file() {{{
test_that("parse_idd_file()", {
    expect_silent(idd_parsed <- parse_idd_file(text("idd", "9.9.9")))

    # can parse Idd from string
    expect_equal(
        names(idd_parsed),
        c("version", "build", "group", "class", "field", "reference")
    )

    # can get Idd version
    expect_equal(idd_parsed$version, as.numeric_version("9.9.9"))

    # can get Idd build
    expect_equal(idd_parsed$build, "7c3bbe4830")

    # can parse group data
    expect_equal(idd_parsed$group$group_id, 1:2)
    expect_equal(idd_parsed$group$group_name, c("TestGroup1", "TestGroup2"))

    # can parse class index data
    expect_equal(idd_parsed$class$class_id, 1:2)
    expect_equal(idd_parsed$class$class_name, c("TestSimple", "TestSlash"))
    expect_equal(idd_parsed$class$group_id, 1:2)

    # can parse class property data
    expect_equal(idd_parsed$class$format, c("standard", "singleLine"))
    expect_equal(idd_parsed$class$min_fields, c(0, 3))
    expect_equal(idd_parsed$class$num_fields, c(1, 4))
    expect_equal(idd_parsed$class$last_required, c(0, 1))
    expect_equal(idd_parsed$class$has_name, c(TRUE, FALSE))
    expect_equal(idd_parsed$class$required_object, c(FALSE, TRUE))
    expect_equal(idd_parsed$class$unique_object, c(FALSE, TRUE))
    expect_equal(idd_parsed$class$num_extensible, c(0, 4))
    expect_equal(idd_parsed$class$first_extensible, c(0, 1))
    expect_equal(idd_parsed$class$num_extensible_group, c(0, 1))
    expect_equal(idd_parsed$class$memo, list(NULL, "This is just a test"))

    # can parse field index data
    expect_equal(idd_parsed$field$field_id, 1:5)
    expect_equal(idd_parsed$field$class_id, c(1, rep(2,4)))
    expect_equal(idd_parsed$field$field_index, c(1, 1:4))

    # can parse field property data
    expect_equal(idd_parsed$field$field_anid, c("A1", "A1", "N1", "N2", "A2"))
    nms <- c("Test Field",
             "Test Character Field 1",
             "Test Numeric Field 1",
             "Test Numeric Field 2",
             "Test Character Field 2")
    expect_equal(idd_parsed$field$units, c(NA_character_, NA_character_, "m", NA_character_, NA_character_))
    expect_equal(idd_parsed$field$ip_units, c(NA_character_, NA_character_, "in", NA_character_, NA_character_))
    expect_equal(idd_parsed$field$is_name, c(TRUE, FALSE, FALSE, FALSE, FALSE))
    expect_equal(idd_parsed$field$required_field, c(FALSE, TRUE, FALSE, FALSE, FALSE))
    expect_equal(idd_parsed$field$extensible_group, c(0L, 1L, 1L, 1L, 1L))
    expect_equal(idd_parsed$field$type_enum, c(4L, 5L, 2L, 2L, 3L))
    expect_equal(idd_parsed$field$autosizable, c(FALSE, FALSE, TRUE, FALSE, FALSE))
    expect_equal(idd_parsed$field$autocalculatable, c(FALSE, FALSE, FALSE, TRUE, FALSE))
    expect_equal(idd_parsed$field$default_chr, c(NA_character_, NA_character_, "2", NA_character_, NA_character_))
    expect_equal(idd_parsed$field$default_num, c(NA_real_, NA_real_, 2, NA_real_, NA_real_))
    expect_equal(idd_parsed$field$choice, list(NULL, NULL, NULL, NULL, c("Key1", "Key2")))
    expect_equal(idd_parsed$field$note, list(NULL, "Test Note Parsing", NULL, NULL, NULL))
    expect_equal(idd_parsed$field$has_range, c(FALSE, FALSE, TRUE, FALSE, FALSE))
    expect_equal(idd_parsed$field$maximum, c(NA, NA, 10, NA, NA))
    expect_equal(idd_parsed$field$minimum, c(NA, NA, 1, NA, NA))
    expect_equal(idd_parsed$field$lower_incbounds, c(FALSE, FALSE, TRUE, FALSE, FALSE))
    expect_equal(idd_parsed$field$upper_incbounds, c(FALSE, FALSE, FALSE, FALSE, FALSE))
    expect_equal(idd_parsed$field$src_enum, c(2L, 1L, 0L, 0L, 0L))

    # can parse field reference data
    expect_equal(idd_parsed$reference$field_id, 2L)
    expect_equal(idd_parsed$reference$src_field_id, 1L)
    expect_equal(idd_parsed$reference$src_enum, 2L)

    # can detect error of missing IDD version
    idd_wrong <- c(
        "\\group TestGroup

        Test,
        A1 ; \\note something"
    )
    expect_error(parse_idd_file(idd_wrong), class = "error_miss_idd_ver")

    # can detect error of multiple IDD versions
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_Version 9.9.8
         \\group TestGroup

         Test,
         A1 ; \\note something"
    )
    expect_error(parse_idd_file(idd_wrong), class = "error_multi_idd_ver")

    # can warn about missing IDD build tag
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         \\group TestGroup

         Test,
         A1 ; \\note something"
    )
    expect_warning(idd_parsed <- parse_idd_file(idd_wrong), class = "warning_miss_idd_build")
    expect_equal(idd_parsed$build, NA_character_)

    # can warn about multiple IDD build tags
    idd_wrong <- c(
        "!IDD_Version 9.9.9
        !IDD_BUILD abc
        !IDD_BUILD def
        \\group TestGroup

        Test,
        A1 ; \\note something"
    )
    expect_error(parse_idd_file(idd_wrong), class = "error_multi_idd_build")

    # can detect error of invalid line
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830
         \\group TestGroup

         Test,
         A1 ; \\note something

         Some Mess Here"
    )
    expect_error(parse_idd_file(idd_wrong), class = "error_unknown_line")

    # can detect missing group lines
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830

         Test,
         A1 ; \\note something

         \\group TestGroup

         Test1,
         A1 ; \\note something
         "
    )
    expect_warning(idd_parsed <- parse_idd_file(idd_wrong), class = "warning_missing_group")
    expect_equal(idd_parsed$group$group_id, 1L:2L)
    expect_equal(idd_parsed$group$group_name, c("Default Group", "TestGroup"))

    # can detect duplicated class names
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830

         \\group TestGroup

         Test,
         A1 ; \\note something

         Test,
         A1 ; \\note something
         "
    )
    expect_error(parse_idd_file(idd_wrong), class = "error_duplicated_class")

    # can detect incomplete class
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830

         \\group TestGroup

         Test,
         A1 ; \\note something

         A1 , \\note something
         A1 ; \\note something
         A1 , \\note something
         "
    )
    expect_error(parse_idd_file(idd_wrong), class = "error_missing_class")

    # can detect missing class names
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830

         \\group TestGroup

         Test,
         A1 ; \\note something

         A1 , \\note something
         A1 ; \\note something
         "
    )
    expect_error(parse_idd_file(idd_wrong), class = "error_missing_class")

    # can manually insert class slash
    idd_cls <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830

         \\group TestGroup

         Test,
         A1 ; \\note something

         Test1,
         A1 , \\note something
         A1 ; \\note something
         "
    )
    expect_silent(parse_idd_file(idd_cls))

    # can detect error of invalid slash key
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830
         \\group TestGroup

         TestInvalidSlash,
         A1 ; \\invalid-slash-key")
    expect_error(parse_idd_file(idd_wrong), class = "error_slash_key")

    # can detect error of invaid type key
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830
         \\group TestGroup

         TestInvalidSlash,
         A1 ; \\type invalid"
    )
    expect_error(parse_idd_file(idd_wrong), class = "error_type_value")

    # can detect error of invaid external list key
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830
         \\group TestGroup

         TestInvalidSlash,
         A1 ; \\external-list invalid"
    )
    expect_error(parse_idd_file(idd_wrong), class = "error_external_list_value")

    # can detect error of invalid format key
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830
         \\group TestGroup

         TestInvalidSlash,
         A1 ; \\format invalid"
    )
    expect_error(parse_idd_file(idd_wrong), class = "error_format_value")
})
# }}}

# parse_idf_file() {{{
test_that("parse_idf_file()", {
    # get version {{{
    # Normal formatted
    expect_equal(
        get_idf_ver(data.table(string = c("Version,", "8.6;"), line = 1:2)),
        numeric_version(8.6)
    )
    # One line formatted
    expect_equal(
        get_idf_ver(data.table(string = "Version, 8.6;", line = 1)),
        numeric_version(8.6)
    )
    expect_equal(
        get_idf_ver(data.table(string = "Version, 8.6; !- Version", line = 1)),
        numeric_version(8.6)
    )
    # }}}

    expect_warning(idf_parsed <- parse_idf_file(text("idf"), 8.8),
        "Missing version field in input IDF"
    )

    # can parse Idf stored in strings
    expect_equal(names(idf_parsed),
        c("version", "options", "object", "value", "reference"))

    # can add version according to input Idd object
    expect_equal(idf_parsed$version, as.numeric_version("8.8.0"))

    # can parse options data
    expect_equal(idf_parsed$options,
        list(idf_editor = FALSE, special_format = FALSE, view_in_ip = FALSE, save_format = "sorted")
    )

    # can parse object data
    expect_equal(idf_parsed$object$object_id, 1:5)
    expect_equal(idf_parsed$object$class_id, c(55, 90, 103, 55, 1))

    # can parse value reference data
    expect_equal(idf_parsed$reference$src_value_id, c(1, NA, NA, NA, 10, NA))
    expect_equal(idf_parsed$reference$value_id, c(11:14, 17:18))

    # can parse comment data
    expect_equal(idf_parsed$object$comment, list(" this is a test comment for WD01", NULL, NULL, NULL, NULL))

    # can detect EpMacro lines
    expect_warning({x <- parse_idf_file("Version, 8.8;\n##include abc")}, "IMF is not fully supported")

    # can parse value data
    text_object <- c(
        "Material,
            WD01,                    !- Name
            MediumSmooth,            !- Roughness
            0.2000000,               !- Thickness {m}
            0.1150000,               !- Conductivity {W/m-K}
            513.0000,                !- Density {kg/m3}
            1381.000,                !- Specific Heat {J/kg-K}
            0.9000000,               !- Thermal Absorptance
            0.7800000,               !- Solar Absorptance
            0.7800000;               !- Visible Absorptance
        ")
    val <- c("WD01", "MediumSmooth", "0.2", "0.115", "513", "1381", "0.9",
        "0.78", "0.78", "8.8")
    num <- suppressWarnings(as.numeric(val))
    num[10] <- NA_real_

    # can parse one-line empty object
    expect_silent(idf_parsed <- parse_idf_file("Version,8.8;\nOutput:Surfaces:List,,;"))
    expect_equivalent(idf_parsed$object,
        data.table(object_id = 1:2, class_id = c(1L, 764L),
        comment = list(), object_name = rep(NA_character_, 2), object_name_lower = rep(NA_character_, 2))
    )
    expect_equivalent(idf_parsed$value,
        data.table(object_id = 1:3, value_chr = c("8.8", NA_character_, NA_character_),
        value_num = rep(NA_real_, 3), object_id = c(1L, 2L, 2L), field_id = c(1L, 58822L, 58823L))
    )

    expect_silent(parse_idf_file("Version,8.8;\nOutput:Surfaces:List,,;"))
    expect_warning(idf_value <- parse_idf_file(text_object, 8.8))
    expect_equal(names(idf_value$value),
        c("value_id", "value_chr", "value_num", "object_id", "field_id"))
    expect_equal(idf_value$value$value_id, 1:10)
    expect_equivalent(idf_value$value$value_num, num)
    expect_equal(idf_value$value$object_id, c(rep(1, 9), 2))

    # can detect invalid lines
    idf_wrong <- c(
        "Version,8.8;
         WrongClass<
            WD01,                    !- Name
            MediumSmooth,            !- Roughness
            1.9099999E-02,           !- Thickness {m}
            0.1150000,               !- Conductivity {W/m-K}
            513.0000,                !- Density {kg/m3}
            1381.000,                !- Specific Heat {J/kg-K}
            0.9000000,               !- Thermal Absorptance
            0.7800000,               !- Solar Absorptance
            0.7800000;               !- Visible Absorptance
        ")
    expect_error(parse_idf_file(idf_wrong, 8.8), class = "error_unknown_line")

    # can detect incomplete object
    idf_wrong <- c(
        "Version,8.8;
         WrongClass,
            WD01,                    !- Name
            MediumSmooth,            !- Roughness
            1.9099999E-02,           !- Thickness {m}
            0.1150000,               !- Conductivity {W/m-K}
            513.0000,                !- Density {kg/m3}
            1381.000,                !- Specific Heat {J/kg-K}
            0.9000000,               !- Thermal Absorptance
            0.7800000,               !- Solar Absorptance
            0.7800000,               !- Visible Absorptance
        ")
    expect_error(parse_idf_file(idf_wrong, 8.8), class = "error_incomplete_object")

    # can detect error of invalid class name
    idf_wrong <- c(
        "Version,8.8;
        ! comment
         WrongClass,
            WD01;                    !- Name
        ! comment
         WrongClass,
            WD01;                    !- Name
        ")
    expect_error(parse_idf_file(idf_wrong, 8.8), class = "error_invalid_class")
    idf_wrong <- c(
        "Version,8.8;
         WrongClass, WD01;
        ")
    expect_error(parse_idf_file(idf_wrong, 8.8), class = "error_invalid_class")

    # can detect error of multiple version
    idf_wrong <- "Version, 8.8;\nVersion, 8.9;"
    expect_error(parse_idf_file(idf_wrong, 8.8), class = "error_multiple_version")

    # can detect error of invalid field number
    idf_wrong <- "
        Version, 8.8;
        SurfaceConvectionAlgorithm:Inside,
            Simple, !- Algorithm
            Simple, !- Algorithm
            TARP; !- Algorithm"
    expect_error(parse_idf_file(idf_wrong, 8.8), class = "error_invalid_field_number")
})
# }}}
