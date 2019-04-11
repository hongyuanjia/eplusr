context("Idd and IddObject")

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

    expect_message(use_idd(text("idd", "9.9.9")))
    expect_true(is_avail_idd("9.9.9"))
})
# }}}

# Idd class {{{
test_that("Idd class", {
    # can create an Idd object from string
    expect_silent(idd <- Idd$new(text("idd", "9.9.9")))

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
    expect_is(idd$object("TestSimple"), "IddObject")

    # can stop when invalid class names are given
    expect_error(idd$object("WrongClass"), "Invalid class name found: `WrongClass`")

    # can return when multiple class names are given
    expect_equal(idd$objects(c("TestSimple", "TestSlash")),
        list(TestSimple = idd$object("TestSimple"),
            TestSlash = idd$object("TestSlash")))

    # can return all IddObjects in a group
    expect_is(idd$objects_in_group("TestGroup1"), "list")
    expect_equal(idd$objects_in_group("TestGroup1"), list(TestSimple = idd$object("TestSimple")))

    # can stop when invalid group names are given
    expect_error(idd$objects_in_group("WrongGroup"), "Invalid group name found")

    # can stop when multiple group names are given
    expect_error(idd$objects_in_group(c("TestGroup1", "TestGroup2")),
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
    expect_equal(idd$TestSlash, idd$object("TestSlash"))
    expect_equal(idd[["TestSlash"]], idd$object("TestSlash"))

    expect_is(idd$object("TestSlash"), "IddObject")
    expect_is(idd$objects_in_group("TestGroup1")[[1L]], "IddObject")
})
# }}}

# IddObject class {{{
test_that("IddObject class", {

    expect_silent(idd <- Idd$new(text("idd", 9.9)))
    expect_silent(slash <- IddObject$new("TestSlash", idd))

    expect_error(IddObject$new(), "IddObject can only be created based on a parent Idd object")

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
    expect_s3_class(catch_cnd(slash$del_extensible_group(1)), "error_del_extensible")

    # can use $has_name()
    expect_false(slash$has_name())

    # can use $is_required()
    expect_true(slash$is_required())

    # can use $is_unique()
    expect_true(slash$is_unique())

    # can use $is_extensible()
    expect_true(slash$is_extensible())

    # can use $field_name()
    expect_error(slash$field_name(slash$num_fields() + 30), "Invalid field index")
    expect_equal(slash$field_name(c(2, 1)), c("Test Numeric Field 1", "Test Character Field 1"))
    expect_warning({nm <- slash$field_name(c(2, 1), lower = TRUE)},
        "Parameter `lower`.*has been deprecated"
    )

    # can use $field_index()
    expect_equal(slash$field_index(), 1L:4L)
    expect_error(slash$field_index("WrongName"), "Invalid field name")
    expect_equal(slash$field_index(
            c("Test Numeric Field 1", "Test Character Field 1")), c(2L, 1L))
    # can use $field_type()
    expect_equivalent(slash$field_type(c(4, 2)), c("choice", "real"))

    # can use $field_note()
    expect_equivalent(slash$field_note(c(2, 1)), list(NULL, "Test Note Parsing"))

    # can use $field_unit()
    expect_equivalent(slash$field_unit(c(4, 2)), c(NA_character_, "m"))
    expect_equivalent(slash$field_unit(c(4, 2), in_ip = TRUE), c(NA_character_, "in"))

    # can use $field_default()
    expect_equivalent(slash$field_default(c(4, 2)), list(NA_character_, 2L))
    expect_warning({val <- slash$field_default(c(4, 2), in_ip = TRUE)},
        "Parameter `in_ip`.* has been deprecated."
    )
    expect_equivalent(unname(val), list(NA_character_, 78.74016), tolerance = 0.001)

    # can use $field_choice()
    expect_equivalent(slash$field_choice(c(4, 2)), list(c("Key1", "Key2"), NULL))

    # can use $field_range()
    expect_equivalent(slash$field_range(c(4, 2)),
        list(ranger(NA_real_, FALSE, NA_real_, FALSE), ranger(1L, TRUE, 10, FALSE)))

    # can use $field_reference()
    expect_is(slash$field_relation(c(4, 2)), "list")
    expect_null(slash$field_relation(c(4, 2), "ref_by")$ref_to)
    expect_equal(nrow(slash$field_relation(c(4, 2))$ref_by), 0L)
    expect_equivalent(slash$field_relation(c(1, 3))$ref_to,
        data.table(
            class_id = 2L, class_name = "TestSlash",
            field_index = 1L, field_name = "Test Character Field 1",
            src_class_id = 1L, src_class_name = "TestSimple",
            src_field_index = 1L, src_field_name = "Test Field",
            src_enum = 2L, dep = 0L
        )
    )

    # can use $field_possible()
    expect_is(slash$field_possible(c(4, 2)), "list")
    expect_equal(names(slash$field_possible(c(4, 2))), c("possible", "relation"))
    expect_silent(slash$field_possible(c(4, 2)))

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
    expect_error(slash$is_valid_field_index("wrong"), "not counts")
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
    expect_false(slash$is_integer_field(2))
    expect_false(slash$is_integer_field(3))
    expect_false(slash$is_integer_field(4))
    expect_error(slash$is_integer_field(5))

    # can use $is_integer_field()
    expect_false(slash$is_real_field(1))
    expect_true(slash$is_real_field(2))
    expect_true(slash$is_real_field(3))
    expect_false(slash$is_real_field(4))
    expect_error(slash$is_real_field(5))

    # can use $is_required_field()
    expect_true(slash$is_required_field(1))
    expect_false(slash$is_required_field(2))
    expect_false(slash$is_required_field(3))
    expect_false(slash$is_required_field(4))
    expect_error(slash$is_required_field(5))

    # can detect if fields have relation with others
    expect_true(slash$has_relation("Test Character Field 1"))
    expect_false(slash$has_relation("Test Numeric Field 1"))
    expect_false(slash$has_ref_by("Test Character Field 1"))
    expect_false(slash$has_ref_by("Test Numeric Field 1"))
    expect_true(slash$has_ref_to("Test Character Field 1"))
    expect_false(slash$has_ref_to("Test Numeric Field 1"))

    # can detect if fields have relation with others
    expect_true(slash$has_relation())
    expect_false(slash$has_ref_by())
    expect_true(slash$has_ref_to())

    slash$to_table(all = TRUE)
    # print
    expect_output(slash$print())
})
# }}}
