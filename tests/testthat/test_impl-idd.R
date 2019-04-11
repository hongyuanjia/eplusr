context("IDD Implementation")

# IDD {{{
test_that("table manipulation", {
    expect_silent(idd_parsed <- parse_idd_file(text("idd", "9.9.9")))

    # GROUP {{{
    expect_equal(get_idd_group_index(idd_parsed), 1L:2L)
    expect_equal(get_idd_group_index(idd_parsed, "TestGroup2"), 2L)
    expect_error(get_idd_group_index(idd_parsed, "Wrong"), "Invalid group name")
    expect_equal(get_idd_group_name(idd_parsed), c("TestGroup1", "TestGroup2"))
    expect_equal(get_idd_group_name(idd_parsed, 2L), "TestGroup2")
    expect_error(get_idd_group_name(idd_parsed, 3), "Invalid group index")
    # }}}

    # CLASS {{{
    expect_equal(get_idd_class(idd_parsed), idd_parsed$class)
    expect_error(get_idd_class(idd_parsed, ""), "Invalid class name")
    expect_error(get_idd_class(idd_parsed, 10L), "Invalid class index")

    expect_equal(
        get_idd_class(idd_parsed, c(2L, 1L))[, -"rleid"],
        idd_parsed$class[c(2L, 1L), -"class_name_us"]
    )
    expect_equal(
        get_idd_class(idd_parsed, c(2L, 1L), "group_name")[, -"rleid"],
        add_group_name(idd_parsed, idd_parsed$class[c(2L, 1L), .SD,.SDcols = CLASS_COLS$index])[, -"class_name_us"]
    )
    expect_equal(
        get_idd_class(idd_parsed, c("TestSlash", "TestSimple"))[, -"rleid"],
        idd_parsed$class[c(2L, 1L), -"class_name_us"]
    )
    expect_equal(
        get_idd_class(idd_parsed, c("TestSlash", "TestSimple"), "min_fields")[, -"rleid"],
        idd_parsed$class[c(2L, 1L), .SD, .SDcols = c(CLASS_COLS$index, "min_fields")][, -"class_name_us"]
    )
    # }}}

    # EXTENSIBLE GROUP {{{
    # ADD {{{
    expect_equal(add_idd_extensible_group(idd_parsed, "TestSimple", 1)$field, idd_parsed$field)
    expect_error(add_idd_extensible_group(idd_parsed, "TestSimple", 1, strict = TRUE), "Non-extensible class")
    expect_equal(
        nrow(idd_added <- add_idd_extensible_group(idd_parsed,
                get_idd_class(idd_parsed, "TestSlash")[, num := 2L]
            )$field), 13L
    )
    expect_equal(nrow((idd_added <- add_idd_extensible_group(idd_parsed, "TestSlash", 1))$field), 9L)
    expect_equal(idd_added$class$num_fields[2L], 8L)
    expect_equal(idd_added$class$num_extensible_group[2L], 2L)
    expect_equal(idd_added$field$field_id[6:9], 6L:9L)
    expect_equal(idd_added$field$class_id[6:9], rep(2L, 4L))
    expect_equal(idd_added$field$field_index[6:9], 5L:8L)
    expect_equal(idd_added$field$field_name[6:9],
        c("Test Character Field 3", "Test Numeric Field 3",
          "Test Numeric Field 4", "Test Character Field 4")
    )
    expect_equal(idd_added$field$field_name_us[6:9],
        c("test_character_field_3", "test_numeric_field_3",
          "test_numeric_field_4", "test_character_field_4")
    )
    cols <- c("field_index", "field_id", "field_anid", "required_field",
        "field_name", "field_name_us", "extensible_group"
    )
    expect_equal(idd_added$field[6:9, -..cols], idd_added$field[2:5, -..cols])
    expect_equal(idd_added$field$field_id[6:9], 6L:9L)
    expect_equal(idd_added$field$field_anid[6:9], c("A3", "N3", "N4", "A4"))
    expect_equal(idd_added$field$required_field[6:9], rep(FALSE, 4L))
    expect_equal(idd_added$field$extensible_group[6:9], rep(2L, 4L))
    # }}}
    # DEL {{{
    expect_equivalent((idd_del <- del_idd_extensible_group(idd_added, "TestSlash", 1))$field, idd_parsed$field)
    expect_equal(idd_del$class$num_fields[2L], 4L)
    expect_equal(idd_del$class$num_extensible_group[2L], 1L)
    expect_error(del_idd_extensible_group(idd_del, "TestSlash", 4), "left less than required")
    # }}}
    # }}}

    # FIELD {{{
    ## USING CLASS {{{
    expect_error(get_idd_field(idd_parsed, 10), "Invalid class index")
    expect_error(get_idd_field(idd_parsed, ""), "Invalid class name")
    expect_equal(get_idd_field(idd_parsed, c("TestSimple", "TestSlash"))$field_id, 1L:4L)
    expect_equivalent(get_idd_field(idd_parsed, c("TestSimple", "TestSlash"), all = TRUE)$field_id, 1L:5L)
    expect_equivalent(get_idd_field(idd_parsed, c("TestSimple", "TestSlash"), property = "type_enum")$type_enum,
        c(4L, 5L, 2L, 2L)
    )
    # }}}
    ## USING FIELD INDEX {{{
    expect_error(get_idd_field(idd_parsed, c("TestSimple", "TestSlash"), c(2, 2)), "Invalid field index")
    expect_error(get_idd_field(idd_parsed, c("TestSimple", "TestSlash"), c(2, 2, 3)),
        "class and field do not have the same length"
    )
    expect_error(get_idd_field(idd_parsed, c("TestSimple", "TestSlash"), c(1, 10), no_ext = TRUE),
        "should be no less than 3 and no more than 4")
    expect_equal(get_idd_field(idd_parsed, "TestSlash", 100)[, .SD, .SDcols = c(
            "field_id", "class_id", "field_index", "field_name", "class_name", "field_in"
            )],
        data.table(field_id = 101L, class_id = 2L, field_index = 100L,
            field_name = "Test Character Field 50", class_name = "TestSlash",
            field_in = 100L
        )
    )
    expect_equal(get_idd_field(idd_parsed, "TestSlash", 20, complete = TRUE)$field_id, 2L:21L)
    # }}}
    ## USING FIELD NAME {{{
    expect_error(get_idd_field(idd_parsed, "TestSlash", ""), "Invalid field name")
    expect_error(get_idd_field(idd_parsed, "TestSlash", "", no_ext = TRUE), "Invalid field name")
    expect_equal(
        get_idd_field(idd_parsed, c(1L, 2L), c("test_field", "test_numeric_field_2"))$field_id,
        c(1L, 4L)
    )
    expect_equal(
        get_idd_field(idd_parsed, c(1L, 2L), c("test_field", "test_numeric_field_3"))$field_id,
        c(1L, 7L)
    )
    expect_equal(
        get_idd_field(idd_parsed, c(1L, 2L), c("test_field", "test_numeric_field_3"), complete = TRUE)$field_id,
        1L:7L
    )
    expect_error(
        get_idd_field(idd_parsed, c(1L, 2L), c("test_field", "test_numeric_field_5"))$field_id,
        "test_numeric_field_5"
    )
    expect_error(
        get_idd_field(idd_parsed, c(1L, 2L), c("test_field", "test_numeric_field_5"))$field_id,
        "test_numeric_field_5"
    )
    # }}}
    # }}}

    # REFERENCES {{{
    expect_equivalent(
        get_idd_relation(idd_parsed, name = TRUE),
        data.table(
            class_id = 2L, class_name = "TestSlash", field_index = 1L, field_name = "Test Character Field 1",
            src_class_id = 1L, src_class_name = "TestSimple", src_field_index = 1L, src_field_name = "Test Field",
            src_enum = 2L, dep = 0L
        )
    )
    expect_equivalent(
        get_idd_relation(idd_parsed, name = TRUE, direction = "ref_by"),
        data.table(
            class_id = 2L, class_name = "TestSlash", field_index = 1L, field_name = "Test Character Field 1",
            src_class_id = 1L, src_class_name = "TestSimple", src_field_index = 1L, src_field_name = "Test Field",
            src_enum = 2L, dep = 0L
        )
    )
    # }}}

    # TABLE {{{
    expect_equal(get_iddobj_table(idd_parsed, 1),
        data.table(class = "TestSimple", index = 1L, field = "Test Field", unit = NA_character_)
    )
    # }}}

    # STRING {{{
    expect_equal(get_iddobj_string(idd_parsed, 2, leading = 0L, sep_at = 0L),
        c("TestSlash,",
          ",!- Test Character Field 1",
          ",!- Test Numeric Field 1 {m}",
          ";!- Test Numeric Field 2"
        )

    )
    expect_equal(get_iddobj_string(idd_parsed, 2, comment = c("This is", "a comment"), leading = 0L, sep_at = 0L),
        c("!This is",
          "!a comment",
          "TestSlash,",
          ",!- Test Character Field 1",
          ",!- Test Numeric Field 1 {m}",
          ";!- Test Numeric Field 2"
        )

    )
    # }}}

})
# }}}
