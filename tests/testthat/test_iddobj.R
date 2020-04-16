context("IddObject")

test_that("IddObject class", {

    expect_silent(idd <- Idd$new(text("idd", 9.9)))
    expect_silent(simple <- IddObject$new("TestSimple", idd))
    expect_silent(slash <- IddObject$new("TestSlash", idd))
    expect_silent(slash <- idd_object(use_idd(text("idd", 9.9)), "TestSlash"))

    expect_error(idd_object(), "based on a parent Idd object", class = "eplusr_error")

    expect_equal(slash$version(), idd$version())

    expect_is(slash$parent(), "Idd")

    # Group {{{
    # can use $group_name()
    expect_equal(slash$group_name(), "TestGroup2")

    # can use $group_index()
    expect_equal(slash$group_index(), 2L)
    # }}}

    # Class {{{
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

    # can use $has_name()
    expect_false(slash$has_name())

    # can use $is_required()
    expect_true(slash$is_required())

    # can use $is_unique()
    expect_true(slash$is_unique())

    # can use $is_extensible()
    expect_true(slash$is_extensible())
    # }}}

    # Extensible Group {{{
    # can use $add_extensible_groups()
    expect_equal(slash$add_extensible_group(1)$num_fields(), 8L)

    # can use $del_extensible_groups()
    expect_equal(slash$del_extensible_group(1)$num_fields(), 4L)
    expect_s3_class(catch_cnd(slash$del_extensible_group(1)), "eplusr_error")
    # }}}

    # Field {{{
    # can use $field_name()
    expect_error(slash$field_name(slash$num_fields() + 30), class = "eplusr_error_invalid_field_index")
    expect_equal(slash$field_name(c(2, 1)), c("Test Numeric Field 1", "Test Character Field 1"))
    expect_equal(slash$field_name(c(2, 1), unit = TRUE), c("Test Numeric Field 1 {m}", "Test Character Field 1"))
    expect_equal(slash$field_name(c(2, 1), unit = TRUE, in_ip = TRUE), c("Test Numeric Field 1 {in}", "Test Character Field 1"))

    # can use $field_index()
    expect_equal(slash$field_index(), 1L:4L)
    expect_error(slash$field_index("WrongName"), class = "eplusr_error_invalid_field_name")
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
    expect_silent({val <- slash$field_default(c(4, 2), in_ip = TRUE)})
    expect_equivalent(unname(val), list(NA_character_, 78.74016), tolerance = 0.001)

    # can use $field_choice()
    expect_equivalent(slash$field_choice(c(4, 2)), list(c("Key1", "Key2"), NULL))

    # can use $field_range()
    expect_equivalent(slash$field_range(c(4, 2)),
        list(ranger(NA_real_, FALSE, NA_real_, FALSE), ranger(1L, TRUE, 10, FALSE)))

    # can use $field_relation()
    expect_is(slash$field_relation(), "list")
    expect_is(slash$field_relation(c(4, 2)), "list")
    expect_null(slash$field_relation(c(4, 2), "ref_by")$ref_to)
    expect_equal(nrow(slash$field_relation(c(4, 2), keep = TRUE)$ref_by), 2L)
    expect_equivalent(slash$field_relation(c(1, 3), keep = TRUE)$ref_to,
        data.table(
            class_id = 2L, class_name = "TestSlash",
            field_id = c(2L, 4L), field_index = c(1L, 3L),
            field_name = c("Test Character Field 1", "Test Numeric Field 2"),
            src_class_id = c(1L, NA_integer_), src_class_name = c("TestSimple", NA_character_),
            src_field_id = c(1L, NA_integer_), src_field_index = c(1L, NA_integer_),
            src_field_name = c("Test Field", NA_integer_),
            src_enum = c(2L, NA_integer_), dep = 0L
        )
    )

    # can use $field_possible()
    expect_equivalent(slash$field_possible(c(4, 2)),
        data.table(class_id = 2L, class_name = "TestSlash",
            field_id = c(5L, 3L), field_index = c(4L, 2L),
            field_name = c("Test Character Field 2", "Test Numeric Field 1"),
            auto = c(NA_character_, "Autosize"),
            default = list(NA_character_, 2),
            choice = list(c("Key1", "Key2"), NULL),
            range = list(
                ranger(NA_real_, FALSE, NA_real_, FALSE),
                ranger(1, TRUE, 10, FALSE)
            )
        )
    )
    # }}}

    # Assertion {{{
    # can use $is_valid_field_num()
    expect_equal(slash$is_valid_field_num(c(1, 4, 6, 12)), c(FALSE, TRUE, FALSE, TRUE))

    # can use $is_extensible_field_index()
    expect_equal(simple$is_extensible_index(1:2), rep(FALSE, 2L))
    expect_equal(slash$is_extensible_index(c(1, 4, 6, 12)), rep(TRUE, times = 4L))

    # can use $is_valid_field_name()
    expect_true(slash$is_valid_field_name("Test Character Field 1"))
    expect_true(slash$is_valid_field_name("Test Character Field 2"))
    expect_true(slash$is_valid_field_name("Test Numeric Field 1"))
    expect_true(slash$is_valid_field_name("Test Numeric Field 2"))
    expect_true(slash$is_valid_field_name("test_character_field_1"))
    expect_false(slash$is_valid_field_name("test_character_field_1", strict = TRUE))
    expect_true(slash$is_valid_field_name("test_numeric_field_1"))
    expect_false(slash$is_valid_field_name(1))
    expect_false(slash$is_valid_field_name("wrong"))

    # can use $is_valid_field_index()
    expect_true(slash$is_valid_field_index(1))
    expect_true(slash$is_valid_field_index(2))
    expect_true(slash$is_valid_field_index(3))
    expect_true(slash$is_valid_field_index(4))
    expect_error(slash$is_valid_field_index("wrong"), "integerish")
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
    expect_true(slash$has_ref("Test Character Field 1"))
    expect_false(slash$has_ref("Test Numeric Field 1"))
    expect_false(slash$has_ref_by("Test Character Field 1"))
    expect_false(slash$has_ref_by("Test Numeric Field 1"))
    expect_true(slash$has_ref_to("Test Character Field 1"))
    expect_false(slash$has_ref_to("Test Numeric Field 1"))

    # can detect if fields have relation with others
    expect_equal(slash$has_ref(), c(TRUE, FALSE, FALSE, FALSE))
    expect_equal(slash$has_ref_by(), rep(FALSE, 4L))
    expect_equal(slash$has_ref_to(), c(TRUE, FALSE, FALSE, FALSE))
    # }}}

    # Table {{{
    # can extract class and field info into a data.table
    expect_equivalent(slash$to_table(),
        data.table(
            class = rep("TestSlash", 3L),
            index = 1L:3L,
            field = c("Test Character Field 1", "Test Numeric Field 1", "Test Numeric Field 2")
        )
    )
    # }}}

    # String {{{
    # can convert to a character vector
    expect_equivalent(slash$to_string(),
        c("TestSlash,",
          "    ,                        !- Test Character Field 1",
          "    ,                        !- Test Numeric Field 1 {m}",
          "    ;                        !- Test Numeric Field 2"
        )
    )
    expect_equivalent(slash$to_string(c("comment1", "comment2"), leading = 0L, sep_at = 0L),
        c("!comment1",
          "!comment2",
          "",
          "TestSlash,",
          ",!- Test Character Field 1",
          ",!- Test Numeric Field 1 {m}",
          ";!- Test Numeric Field 2"
        )
    )
    # }}}

    # S3 {{{
    expect_equal(format(slash), "<IddObject: 'TestSlash' v9.9.0>")
    expect_equal(format(slash, ver = FALSE), "<IddObject: 'TestSlash'>")
    expect_output(str(slash))
    expect_equal(as.character(slash), slash$to_string())
    # }}}

    # can check equality
    expect_true(slash == slash)
    expect_false(slash == "a")
    expect_false(slash == IddObject$new("TestSlash", idd))
    expect_true(slash != IddObject$new("TestSlash", idd))

    # print
    expect_output(slash$print(brief = TRUE))
    expect_output(slash$print())
    expect_output(simple$print())
})
