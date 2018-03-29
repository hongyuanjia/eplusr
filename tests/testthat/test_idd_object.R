context("IDDObject method")

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
       \\type alpha
     A2 ; \\field Test Character Field 2
       \\key Key1
       \\key Key2
       \\object-list RefTestSimpleA1")

idd <- IDD$new(idd_text)
slash <- idd$object("TestSlash")

describe("methods from IddObject", {
    # {{{
    it("can use $group_name()",  expect_equal(slash$group_name(), "TestGroup2"))
    it("can use $group_order()", expect_equal(slash$group_order(), 2L))
    it("can use $class_name()", expect_equal(slash$class_name(), "TestSlash"))
    it("can use $class_order()", expect_equal(slash$class_order(), 2L))
    it("can use $class_format()", expect_equal(slash$class_format(), "singleLine"))
    it("can use $min_fields()", expect_equal(slash$min_fields(), 3L))
    it("can use $num_fields()", expect_equal(slash$num_fields(), 4L))
    it("can use $memo()", expect_match(slash$memo(), "This is just a test"))
    it("can use $num_extensible()", expect_equal(slash$num_extensible(), 4L))
    it("can use $reference_class_name()", expect_equal(slash$reference_class_name(), list("RefTestSlash")))
    it("can use $first_extensible()", expect_equal(slash$first_extensible(), 1L))
    it("can use $add_extensible_groups()", expect_equal(slash$add_extensible_groups()$num_fields(), 8L))
    it("can use $del_extensible_groups()", expect_equal(slash$del_extensible_groups()$num_fields(), 4L))
    it("can use $is_version()", expect_false(slash$is_version()))
    it("can use $is_required()", expect_true(slash$is_required()))
    it("can use $is_unique()", expect_true(slash$is_unique()))
    it("can use $is_extensible()", expect_true(slash$is_extensible()))
    it("can use $has_name()", expect_false(slash$has_name()))
    it("can use $field_name()", expect_match(slash$field_name(2), "Test Numeric Field 1"))
    it("can use $field_index()", expect_equal(slash$field_index("Test Numeric Field 1"), 2L))
    it("can use $field_type()", expect_equal(slash$field_type(2), "integer"))
    it("can use $field_unit()", expect_equal(slash$field_unit(2), "m"))
    it("can use $field_reference()", expect_equal(slash$field_reference(2), list(NA_character_)))
    it("can use $field_object_list()", expect_equal(slash$field_object_list(4), list("RefTestSimpleA1")))
    it("can use $field_default()", expect_equal(slash$field_default(c(1, 2)), list(NA_character_, 2L)))

    it("can use $field_choice()", {
       expect_equal(slash$field_choice(c(2, 4)),
            list(NA_character_, c("Key1", "Key2")))
    })

    it("can use $field_note()", expect_equal(slash$field_note(1), "Test Note Parsing"))
    # }}}
})
