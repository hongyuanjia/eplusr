context("IddObject methods")

# idd_text {{{
idd_text <- c(
    "!IDD_Version 8.8.0
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

idd <- eplusr:::Idd$new(idd_text)
slash <- idd$object("TestSlash")$TestSlash

describe("methods from IddObject", {
    # {{{
    it("can use $group_name()",
       expect_equal(slash$group_name(), "TestGroup2")
    )

    it("can use $group_index()",
       expect_equal(slash$group_index(), 2L)
    )
    it("can use $class_name()",
       expect_equal(slash$class_name(), "TestSlash")
    )
    it("can use $class_index()",
       expect_equal(slash$class_index(), 2L)
    )
    it("can use $class_format()",
       expect_equal(slash$class_format(), "singleLine")
    )
    it("can use $min_fields()",
       expect_equal(slash$min_fields(), 3L)
    )
    it("can use $num_fields()",
       expect_equal(slash$num_fields(), 4L)
    )
    it("can use $memo()",
       expect_match(slash$memo(), "This is just a test")
    )
    it("can use $num_extensible()",
       expect_equal(slash$num_extensible(), 4L)
    )
    # it("can use $reference_class_name()",
    #    expect_equal(slash$reference_class_name(), "RefTestSlash")
    # )
    it("can use $first_extensible_index()",
       expect_equal(slash$first_extensible_index(), 1L)
    )
    it("can use $extensible_group_num()",
       expect_equal(slash$extensible_group_num(), 1L)
    )
    it("can use $add_extensible_groups()",
       expect_equal(slash$add_extensible_group(1)$num_fields(), 8L)
    )
    it("can use $del_extensible_groups()",
       expect_equal(slash$del_extensible_group(1)$num_fields(), 4L)
    )
    it("can use $has_name()",
       expect_false(slash$has_name())
    )
    it("can use $is_required()",
       expect_true(slash$is_required())
    )
    it("can use $is_unique()",
       expect_true(slash$is_unique())
    )
    it("can use $is_extensible()",
       expect_true(slash$is_extensible())
    )
    it("can use $field_name()", {
       expect_error(slash$field_name(slash$num_fields() + 1), "Invalid field index")
       expect_equal(slash$field_name(c(2, 1)),
           c("Test Numeric Field 1", "Test Character Field 1"))
    })
    it("can use $field_index()", {
       expect_error(slash$field_index("WrongName"), "Invalid field name")
       expect_equal(slash$field_index(
           c("Test Numeric Field 1", "Test Character Field 1")), c(2L, 1L))
    })
    it("can use $field_type()",
       expect_equivalent(slash$field_type(c(4, 2)), c("choice", "integer"))
    )
    it("can use $field_note()",
       expect_equivalent(slash$field_note(c(2, 1)), c(NA_character_, "Test Note Parsing"))
    )
    it("can use $field_unit()",
       expect_equivalent(slash$field_unit(c(4, 2)), c(NA_character_, "m"))
    )
    it("can use $field_default()",
       expect_equivalent(slash$field_default(c(4, 2)), list(NA_character_, 2L))
    )
    it("can use $field_choice()",
       expect_equivalent(slash$field_choice(c(4, 2)), list(c("Key1", "Key2"), NA_character_))
    )
    it("can use $field_range()",
       expect_equivalent(slash$field_range(c(4, 2)),
           list(list(NA_real_, NA, NA_real_, NA), list(1L, TRUE, 10, FALSE)))
    )
    it("can use $is_valid_field_num()", {
       expect_false(slash$is_valid_field_num(1))
       expect_true(slash$is_valid_field_num(4))
       expect_false(slash$is_valid_field_num(6))
       expect_true(slash$is_valid_field_num(12))
    })
    it("can use $is_extensible_field_index()", {
       expect_true(slash$is_extensible_index(1))
       expect_true(slash$is_extensible_index(4))
       expect_true(slash$is_extensible_index(6))
       expect_true(slash$is_extensible_index(12))
    })
    it("can use $is_valid_field_name()", {
       expect_true(slash$is_valid_field_name("Test Character Field 1"))
       expect_true(slash$is_valid_field_name("Test Character Field 2"))
       expect_true(slash$is_valid_field_name("Test Numeric Field 1"))
       expect_true(slash$is_valid_field_name("Test Numeric Field 2"))
       expect_false(slash$is_valid_field_name(1))
       expect_false(slash$is_valid_field_name("wrong"))
    })
    it("can use $is_valid_field_index()", {
       expect_true(slash$is_valid_field_index(1))
       expect_true(slash$is_valid_field_index(2))
       expect_true(slash$is_valid_field_index(3))
       expect_true(slash$is_valid_field_index(4))
       expect_false(slash$is_valid_field_index("wrong"))
       expect_false(slash$is_valid_field_index(5))
    })
    it("can use $is_autosizable_field()", {
       expect_false(slash$is_autosizable_field(1))
       expect_true(slash$is_autosizable_field(2))
       expect_false(slash$is_autosizable_field(3))
       expect_false(slash$is_autosizable_field(4))
       expect_error(slash$is_autosizable_field(5))
    })
    it("can use $is_autocalculatable_field()", {
       expect_false(slash$is_autocalculatable_field(1))
       expect_false(slash$is_autocalculatable_field(2))
       expect_true(slash$is_autocalculatable_field(3))
       expect_false(slash$is_autocalculatable_field(4))
       expect_error(slash$is_autocalculatable_field(5))
    })
    it("can use $is_numeric_field()", {
       expect_false(slash$is_numeric_field(1))
       expect_true(slash$is_numeric_field(2))
       expect_true(slash$is_numeric_field(3))
       expect_false(slash$is_numeric_field(4))
       expect_error(slash$is_numeric_field(5))
    })
    it("can use $is_integer_field()", {
       expect_false(slash$is_integer_field(1))
       expect_true(slash$is_integer_field(2))
       expect_false(slash$is_integer_field(3))
       expect_false(slash$is_integer_field(4))
       expect_error(slash$is_integer_field(5))
    })
    it("can use $is_required_field()", {
       expect_true(slash$is_required_field(1))
       expect_false(slash$is_required_field(2))
       expect_false(slash$is_required_field(3))
       expect_false(slash$is_required_field(4))
       expect_error(slash$is_required_field(5))
    })
    # }}}
})
