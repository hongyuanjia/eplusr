context("Idd and IddObject")

# idd_text {{{
idd_text <- c(
    "!IDD_Version 9.9.9
     !IDD_BUILD 7c3bbe4830
     \\group TestGroup1

     TestSimple,
     A1 ; \\field Test Field
       \\reference RefTestSimpleA1

     \\group TestGroup2
     TestSlash,
       \\memo This is just a test
       \\required-object
       \\unique-object
       \\min-fields 3
       \\format singleLine
       \\reference-class-name RefTestSlash
       \\extensible 4 !all fields are extensible
     A1 , \\field Test Character Field 1
       \\note Test Note Parsing
       \\required-field
       \\begin-extensible
       \\external-list autoRDDvariable
     N1 , \\field Test Numeric Field 1
       \\units m
       \\ip-units inch
       \\unitsbasedonfield A2
       \\minimum 1
       \\maximum< 10
       \\default 2
       \\autosizable
       \\type integer
     N2 , \\field Test Numeric Field 2
       \\autocalculatable
       \\type real
     A2 ; \\field Test Character Field 2
       \\type choice
       \\key Key1
       \\key Key2
       \\object-list RefTestSimpleA1")
# }}}

# download_idd() {{{
test_that("can download IDD from EnergyPlus repo", {
    skip_on_cran()
    expect_message(download_idd(8.8, tempdir()))
    expect_true(file.exists(file.path(tempdir(), "V8-8-0-Energy+.idd")))

    expect_message(download_idd("latest", tempdir()))
    expect_true(file.exists(file.path(tempdir(), "V9-0-0-Energy+.idd")))
})
# }}}

# use_idd() {{{
test_that("can read IDD", {
    skip_on_cran()
    expect_error(is_avail_idd("latest"))
    expect_equal(avail_idd(), NULL)
    expect_error(use_idd(8.5))

    expect_message(use_idd(8.5, download = TRUE))
    expect_true(file.exists(file.path(tempdir(), "V8-5-0-Energy+.idd")))
    expect_is(use_idd("8.5.0"), "Idd")
    expect_equal(avail_idd(), "8.5.0")
    expect_true(is_avail_idd(8.5))
    expect_true(is_avail_idd("8.5"))
    expect_true(is_avail_idd("8.5.0"))

    expect_message(use_idd(8.7, download = "auto"))
    expect_equal(avail_idd(), c("8.5.0", "8.7.0"))

    expect_message(use_idd(idd_text))
    expect_true(is_avail_idd("9.9.9"))
})
# }}}

# parse_idd_file() {{{
test_that("parse_idd_file()", {
    idd_parsed <- parse_idd_file(idd_text)

    # can read Idd from string
    expect_silent(idd_str <- read_idd_str(idd_text))

    # can parse Idd from string
    expect_equal(names(idd_parsed), c(
        "version", "build",
        "group",
        "class", "class_memo", "class_reference",
        "field", "field_note", "field_reference",
        "field_default", "field_choice", "field_range",
        "field_object_list", "field_external_list"
    ))

    # can get Idd version
    expect_equal(idd_parsed$version, as.numeric_version("9.9.9"))

    # can get Idd build
    expect_equal(idd_parsed$build, "7c3bbe4830")

    # can parse group data
    expect_equal(idd_parsed$group$group_id, 1:2)
    expect_equal(idd_parsed$group$group_name, c("TestGroup1", "TestGroup2"))

    # can parse class property data
    expect_equal(idd_parsed$class$class_id, 1:2)
    expect_equal(idd_parsed$class$class_name, c("TestSimple", "TestSlash"))
    expect_equal(idd_parsed$class$group_id, 1:2)
    expect_equal(idd_parsed$class$class_format, c("standard", "singleLine"))
    expect_equal(idd_parsed$class$min_fields, c(0, 3))
    expect_equal(idd_parsed$class$num_fields, c(1, 4))
    expect_equal(idd_parsed$class$required_object, c(FALSE, TRUE))
    expect_equal(idd_parsed$class$unique_object, c(FALSE, TRUE))
    expect_equal(idd_parsed$class$has_name, c(TRUE, FALSE))
    expect_equal(idd_parsed$class$last_required, c(0, 1))
    expect_equal(idd_parsed$class$num_extensible, c(0, 4))
    expect_equal(idd_parsed$class$first_extensible, c(0, 1))
    expect_equal(idd_parsed$class$num_extensible_group, c(0, 1))

    # can parse class memo data
    expect_equal(idd_parsed$class_memo$memo, c(NA_character_, "This is just a test"))

    # can parse class reference data
    expect_equal(idd_parsed$class_reference$reference_id, 1)
    expect_equal(idd_parsed$class_reference$reference, "RefTestSlash")
    expect_equal(idd_parsed$class_reference$class_id, 2)

    # can parse field property data
    expect_equal(idd_parsed$field$field_id, 1:5)
    expect_equal(idd_parsed$field$class_id, c(1, rep(2,4)))
    expect_equal(idd_parsed$field$field_index, c(1, 1:4))
    nms <- c("Test Field",
             "Test Character Field 1",
             "Test Numeric Field 1",
             "Test Numeric Field 2",
             "Test Character Field 2")
    expect_equal(idd_parsed$field$field_name, nms)
    expect_equal(idd_parsed$field$full_name,
                 paste0(nms, c("", "", " {m}", "", "")))
    expect_equal(idd_parsed$field$full_ipname,
                 paste0(nms, c("", "", " {ft}", "", "")))
    expect_equal(idd_parsed$field$units,
                 c(NA_character_, NA_character_, "m", NA_character_, NA_character_))
    expect_equal(idd_parsed$field$ip_units,
                 c(NA_character_, NA_character_, "ft", NA_character_, NA_character_))
    expect_equal(idd_parsed$field$required_field,
                 c(FALSE, TRUE, FALSE, FALSE, FALSE))
    expect_equal(idd_parsed$field$type,
                 c("alpha", "alpha", "integer", "real", "choice"))
    expect_equal(idd_parsed$field$autosizable,
                 c(FALSE, FALSE, TRUE, FALSE, FALSE))
    expect_equal(idd_parsed$field$autocalculatable,
                 c(FALSE, FALSE, FALSE, TRUE, FALSE))
    expect_equal(idd_parsed$field$is_name,
                 c(TRUE, FALSE, FALSE, FALSE, FALSE))
    expect_equal(idd_parsed$field$is_extensible,
                 c(FALSE, TRUE, TRUE, TRUE, TRUE))
    expect_equal(idd_parsed$field$has_default,
                 c(FALSE, FALSE, TRUE, FALSE, FALSE))
    expect_equal(idd_parsed$field$has_range,
                 c(FALSE, FALSE, TRUE, FALSE, FALSE))
    expect_equal(idd_parsed$field$has_reference,
                 c(TRUE, FALSE, FALSE, FALSE, FALSE))
    expect_equal(idd_parsed$field$has_object_list,
                 c(FALSE, FALSE, FALSE, FALSE, TRUE))
    expect_equal(idd_parsed$field$has_external_list,
                 c(FALSE, TRUE, FALSE, FALSE, FALSE))

    # can parse field note data
    expect_equal(idd_parsed$field_note$note,
                 c(NA_character_, "Test Note Parsing", NA_character_, NA_character_, NA_character_))

    # can parse field reference data
        expect_equal(idd_parsed$field_reference$reference_id, 1)
        expect_equal(idd_parsed$field_reference$reference, "RefTestSimpleA1")
        expect_equal(idd_parsed$field_reference$field_id, 1)

    # can parse field default data
    expect_equal(idd_parsed$field_default$field_id, 3)
    expect_equal(idd_parsed$field_default$default, "2")
    expect_equal(idd_parsed$field_default$default_upper, "2")
    expect_equal(idd_parsed$field_default$default_num, 2)
    expect_equivalent(idd_parsed$field_default$default_ipnum, 6.56168,
        tolerance = 0.0001)

    # can parse field choice data
    expect_equal(idd_parsed$field_choice$choice_id, 1:2)
    expect_equal(idd_parsed$field_choice$choice, c("Key1", "Key2"))
    expect_equal(idd_parsed$field_choice$choice_upper, c("KEY1", "KEY2"))
    expect_equal(idd_parsed$field_choice$field_id, c(5, 5))

    # can parse field range data
    expect_equal(idd_parsed$field_range$range_id, 1)
    expect_equal(idd_parsed$field_range$minimum, 1)
    expect_equal(idd_parsed$field_range$lower_incbounds, TRUE)
    expect_equal(idd_parsed$field_range$maximum, 10)
    expect_equal(idd_parsed$field_range$upper_incbounds, FALSE)
    expect_equal(idd_parsed$field_range$field_id, 3)

    # can parse field object list data
    expect_equal(idd_parsed$field_object_list$object_list_id, 1)
    expect_equal(idd_parsed$field_object_list$object_list, "RefTestSimpleA1")
    expect_equal(idd_parsed$field_object_list$field_id, 5)

    # can parse field external list data
    expect_equal(idd_parsed$field_external_list$external_list_id, 1)
    expect_equal(idd_parsed$field_external_list$external_list, "autoRDDvariable")
    expect_equal(idd_parsed$field_external_list$field_id, 2)

    # can detect error of missing IDD version
    idd_wrong <- c(
        "\\group TestGroup

        TestInvalidSlash,
        A1 ; \\note something

        Some Mess Here"
    )
    expect_error(parse_idd_file(idd_wrong), "No IDD version found")

    # can detect error of multiple IDD versions
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_Version 9.9.8
         \\group TestGroup

         TestInvalidSlash,
         A1 ; \\note something

         Some Mess Here"
    )
    expect_error(parse_idd_file(idd_wrong), "Multiple IDD version found")

    # can warn about missing IDD build tag
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         \\group TestGroup

         TestInvalidSlash,
         A1 ; \\note something"
    )
    expect_warning(parse_idd_file(idd_wrong))

    # can warn about multiple IDD build tags
    idd_wrong <- c(
        "!IDD_Version 9.9.9
        !IDD_BUILD abc
        !IDD_BUILD def
        \\group TestGroup

        TestInvalidSlash,
        A1 ; \\note something"
    )
    expect_warning(parse_idd_file(idd_wrong))

    # can detect error of invaid line
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830
         \\group TestGroup

         TestInvalidSlash,
         A1 ; \\note something

         Some Mess Here"
    )
    expect_error(parse_idd_file(idd_wrong))

    # can detect error of invalid slash key
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830
         \\group TestGroup

         TestInvalidSlash,
         A1 ; \\invalid-slash-key")
    expect_error(parse_idd_file(idd_wrong))

    # can detect error of invaid type key
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830
         \\group TestGroup

         TestInvalidSlash,
         A1 ; \\type invalid"
    )
    expect_error(parse_idd_file(idd_wrong))

    # can detect error of invaid external list key
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830
         \\group TestGroup

         TestInvalidSlash,
         A1 ; \\external-list invalid"
    )
    expect_error(parse_idd_file(idd_wrong))

    # can detect error of invalid format key
    idd_wrong <- c(
        "!IDD_Version 9.9.9
         !IDD_BUILD 7c3bbe4830
         \\group TestGroup

         TestInvalidSlash,
         A1 ; \\format invalid"
    )
    expect_error(parse_idd_file(idd_wrong))
})
# }}}

# Idd class {{{
test_that("Idd class", {
    idd <- Idd$new(idd_text)
    # can create an Idd object from string
    expect_silent(idd <- eplusr:::Idd$new(idd_text))

    # can get Idd version
    expect_equal(idd$version(), as.numeric_version("9.9.9"))

    # can get Idd build
    expect_equal(idd$build(), "7c3bbe4830")

    # can get all group names
    expect_equal(idd$group_name(), c("TestGroup1", "TestGroup2"))

    # can get group name of one class
    expect_equal(idd$from_group("TestSimple"), "TestGroup1")

    # can return when multiple class names are given
    expect_equal(idd$from_group(c("TestSlash", "TestSimple")),
        c("TestGroup2", "TestGroup1"))

    # can stop when invalid class name is given
    expect_error(idd$from_group("WrongClass"), "Invalid class name found")

    # can return all class names
    expect_equal(idd$class_name(), c("TestSimple", "TestSlash"))

    # can return an index of a single group
    expect_equal(idd$group_index("TestGroup1"), 1)

    # can return multiple group indexes
    expect_equal(idd$group_index(c("TestGroup2", "TestGroup1", "TestGroup2")),
        c(2L, 1L, 2L))

    # can stop when invalid group names are given
    expect_error(idd$group_index("WrongGroup"), "Invalid group name found")

    # can return an index of a single class
    expect_equal(idd$class_index("TestSlash"), 2L)

    # can return multiple class indexes
    expect_equal(idd$class_index(c("TestSlash", "TestSimple", "TestSimple")),
        c(2L, 1L, 1L))

    # can stop when invalid class names are given
    expect_error(idd$class_index("WrongClass"), "Invalid class name found")

    # can return names of all required classes
    expect_equal(idd$required_class_name(), "TestSlash")

    # can return names of all unique classes
    expect_equal(idd$unique_class_name(), "TestSlash")

    # can return names of all extensible classes
    expect_equal(idd$extensible_class_name(), "TestSlash")

    # can return a single IddObject using class name
    expect_is(idd$object("TestSimple")$TestSimple, "IddObject")

    # can stop when invalid class names are given
    expect_error(idd$object("WrongClass"), "Invalid class name found: `WrongClass`.")

    # can return when multiple class names are given
    expect_equal(idd$object(c("TestSimple", "TestSlash")),
        list(TestSimple = idd$object("TestSimple")$TestSimple,
            TestSlash = idd$object("TestSlash")$TestSlash))

    # can return all IddObjects in a group
    expect_is(idd$object_in_group("TestGroup1"), "list")
    expect_equal(idd$object_in_group("TestGroup1"), list(TestSimple = idd$object("TestSimple")$TestSimple))

    # can stop when invalid group names are given
    expect_error(idd$object_in_group("WrongGroup"), "Invalid group name found")

    # can stop when multiple group names are given
    expect_error(idd$object_in_group(c("TestGroup1", "TestGroup2")),
        "group is not a string")

    # can check if input is a valid group
    expect_false(idd$is_valid_group("WrongGroup"))
    expect_true(idd$is_valid_group("TestGroup1"))

    # can check if input is a valid class
    expect_false(idd$is_valid_class("WrongClass"))
    expect_true(idd$is_valid_class("TestSlash"))

    # can print without error
    expect_output(idd$print())

    # can get single object using S3 method
    expect_equal(idd$TestSlash, idd$object("TestSlash")[[1]])
    expect_equal(idd[["TestSlash"]], idd$object("TestSlash")[[1]])
})
# }}}

# IddObject class {{{
test_that("IddObject class", {
    idd <- eplusr:::Idd$new(idd_text)

    expect_error(IddObject$new(),
        "IddObject can only be created after a parent Idd object"
    )

    slash <- idd$object("TestSlash")$TestSlash

    # can use $group_name()
    expect_equal(slash$group_name(), "TestGroup2")

    # can use $group_index()
    expect_equal(slash$group_index(), 2L)

    # can use $class_name()
    expect_equal(slash$class_name(), "TestSlash")

    # can use $class_index()
    expect_equal(slash$class_index(), 2L)

    # can use $class_format()
    expect_equal(slash$class_format(), "singleLine")

    # can use $min_fields()
    expect_equal(slash$min_fields(), 3L)

    # can use $num_fields()
    expect_equal(slash$num_fields(), 4L)

    # can use $memo()
    expect_match(slash$memo(), "This is just a test")

    # can use $num_extensible()
    expect_equal(slash$num_extensible(), 4L)

    # can use $first_extensible_index()
    expect_equal(slash$first_extensible_index(), 1L)

    # can use $extensible_group_num()
    expect_equal(slash$extensible_group_num(), 1L)

    # can use $add_extensible_groups()
    expect_equal(slash$add_extensible_group(1)$num_fields(), 8L)

    # can use $del_extensible_groups()
    expect_equal(slash$del_extensible_group(1)$num_fields(), 4L)

    # can use $has_name()
    expect_false(slash$has_name())

    # can use $is_required()
    expect_true(slash$is_required())

    # can use $is_unique()
    expect_true(slash$is_unique())

    # can use $is_extensible()
    expect_true(slash$is_extensible())

    # can use $field_name()
    expect_error(slash$field_name(slash$num_fields() + 1), "Invalid field index")
    expect_equal(slash$field_name(c(2, 1)),
        c("Test Numeric Field 1", "Test Character Field 1"))

    # can use $field_index()
    expect_error(slash$field_index("WrongName"), "Invalid field name")
    expect_equal(slash$field_index(
            c("Test Numeric Field 1", "Test Character Field 1")), c(2L, 1L))
    # can use $field_type()
    expect_equivalent(slash$field_type(c(4, 2)), c("choice", "integer"))

    # can use $field_note()
    expect_equivalent(slash$field_note(c(2, 1)), c(NA_character_, "Test Note Parsing"))

    # can use $field_unit()
    expect_equivalent(slash$field_unit(c(4, 2)), c(NA_character_, "m"))

    # can use $field_default()
    expect_equivalent(slash$field_default(c(4, 2)), list(NA_character_, 2L))

    # can use $field_choice()
    expect_equivalent(slash$field_choice(c(4, 2)), list(c("Key1", "Key2"), NA_character_))

    # can use $field_range()
    expect_equivalent(slash$field_range(c(4, 2)),
        list(list(NA_real_, NA, NA_real_, NA), list(1L, TRUE, 10, FALSE)))

    # can use $field_reference()
    expect_error(slash$field_reference(c(4, 2)),
        "Function can only be used in IddObjects that are created inside an Idf")

    # can use $field_possible()
    expect_error(slash$field_possible(c(4, 2)),
        "Function can only be used in IddObjects that are created inside an Idf")

    # can use $is_valid_field_num()
    expect_equal(slash$is_valid_field_num(c(1, 4, 6, 12)), c(FALSE, TRUE, FALSE, TRUE))

    # can use $is_extensible_field_index()
    expect_equal(slash$is_extensible_index(c(1, 4, 6, 12)), rep(TRUE, time = 4L))

    # can use $is_valid_field_name()
    expect_true(slash$is_valid_field_name("Test Character Field 1"))
    expect_true(slash$is_valid_field_name("Test Character Field 2"))
    expect_true(slash$is_valid_field_name("Test Numeric Field 1"))
    expect_true(slash$is_valid_field_name("Test Numeric Field 2"))
    expect_true(slash$is_valid_field_name("test_character_field_1"))
    expect_true(slash$is_valid_field_name("test_numeric_field_1"))
    expect_false(slash$is_valid_field_name(1))
    expect_false(slash$is_valid_field_name("wrong"))

    # can use $is_valid_field_index()
    expect_true(slash$is_valid_field_index(1))
    expect_true(slash$is_valid_field_index(2))
    expect_true(slash$is_valid_field_index(3))
    expect_true(slash$is_valid_field_index(4))
    expect_false(slash$is_valid_field_index("wrong"))
    expect_false(slash$is_valid_field_index(5))

    # can use $is_autosizable_field()
    expect_false(slash$is_autosizable_field(1))
    expect_true(slash$is_autosizable_field(2))
    expect_false(slash$is_autosizable_field(3))
    expect_false(slash$is_autosizable_field(4))
    expect_error(slash$is_autosizable_field(5))

    # can use $is_autocalculatable_field()
    expect_false(slash$is_autocalculatable_field(1))
    expect_false(slash$is_autocalculatable_field(2))
    expect_true(slash$is_autocalculatable_field(3))
    expect_false(slash$is_autocalculatable_field(4))
    expect_error(slash$is_autocalculatable_field(5))

    # can use $is_numeric_field()
    expect_false(slash$is_numeric_field(1))
    expect_true(slash$is_numeric_field(2))
    expect_true(slash$is_numeric_field(3))
    expect_false(slash$is_numeric_field(4))
    expect_error(slash$is_numeric_field(5))

    # can use $is_integer_field()
    expect_false(slash$is_integer_field(1))
    expect_true(slash$is_integer_field(2))
    expect_false(slash$is_integer_field(3))
    expect_false(slash$is_integer_field(4))
    expect_error(slash$is_integer_field(5))

    # can use $is_required_field()
    expect_true(slash$is_required_field(1))
    expect_false(slash$is_required_field(2))
    expect_false(slash$is_required_field(3))
    expect_false(slash$is_required_field(4))
    expect_error(slash$is_required_field(5))

    expect_output(slash$print())
})
# }}}
