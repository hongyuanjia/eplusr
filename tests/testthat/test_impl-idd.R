context("IDD Implementation")

# IDD {{{
test_that("table manipulation", {
    expect_silent(idd_parsed <- parse_idd_file(text("idd", "9.9.9")))

    # GROUP {{{
    expect_equal(get_idd_group_index(idd_parsed), 1L:2L)
    expect_equal(get_idd_group_index(idd_parsed, "TestGroup2"), 2L)
    expect_error(get_idd_group_index(idd_parsed, "Wrong"), class = "error_group_name")
    expect_equal(get_idd_group_name(idd_parsed), c("TestGroup1", "TestGroup2"))
    expect_equal(get_idd_group_name(idd_parsed, 2L), "TestGroup2")
    expect_error(get_idd_group_name(idd_parsed, 3), class = "error_group_id")
    # }}}

    # CLASS {{{
    expect_equal(get_idd_class(idd_parsed),
        idd_parsed$class[, .SD, .SDcols = c("class_id", "class_name", "group_id")]
    )
    expect_error(get_idd_class(idd_parsed, ""), class = "error_class_name")
    expect_error(get_idd_class(idd_parsed, 10L), class = "error_class_id")

    expect_equal(
        get_idd_class(idd_parsed, c(2L, 1L)),
        data.table(rleid = 1:2, class_id = c(2L, 1L),
            class_name = c("TestSlash", "TestSimple"), group_id = c(2L, 1L))
    )
    expect_equal(
        get_idd_class(idd_parsed, c(2L, 1L), "group_name"),
        data.table(rleid = 1:2, class_id = c(2L, 1L),
            class_name = c("TestSlash", "TestSimple"), group_id = c(2L, 1L),
            group_name = c("TestGroup2", "TestGroup1")
        )
    )
    expect_equal(
        get_idd_class(idd_parsed, c("TestSlash", "TestSimple")),
        data.table(rleid = 1:2, class_id = c(2L, 1L),
            class_name = c("TestSlash", "TestSimple"), group_id = c(2L, 1L))
    )
    expect_equal(
        get_idd_class(idd_parsed, c("TestSlash", "TestSimple"), "min_fields"),
        data.table(rleid = 1:2, class_id = c(2L, 1L),
            class_name = c("TestSlash", "TestSimple"), group_id = c(2L, 1L),
            min_fields = c(3L, 0L)
        )
    )
    expect_equivalent(
        get_idd_class(idd_parsed, NULL, "min_fields"),
        data.table(class_id = c(1L, 2L),
            class_name = c("TestSimple", "TestSlash"), group_id = c(1L, 2L),
            min_fields = c(0L, 3L)
        )
    )
    # }}}

    # EXTENSIBLE GROUP {{{
    # ADD {{{
    expect_equal(add_idd_extensible_group(idd_parsed, "TestSimple", 1)$field, idd_parsed$field)
    expect_error(add_idd_extensible_group(idd_parsed, "TestSimple", 1, strict = TRUE), class = "error_nonextensible_class")
    expect_equal(nrow(idd_added <- add_idd_extensible_group(idd_parsed, "TestSlash", 2)$field), 13L)
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

    # references of extensible fields should be automatically generated
    expect_silent(idd_added <- add_idd_extensible_group(idd_parsed, "TestSlash", 1))
    expect_equal(idd_added$reference,
        data.table(class_id = 2L, field_id = c(2L, 6L), src_class_id = 1L, src_field_id = 1L, src_enum = 2L)
    )
    # }}}
    # DEL {{{
    expect_equivalent((idd_del <- del_idd_extensible_group(idd_added, "TestSlash", 1))$field, idd_parsed$field)
    expect_equal(idd_del$class$num_fields[2L], 4L)
    expect_equal(idd_del$class$num_extensible_group[2L], 1L)
    expect_error(del_idd_extensible_group(idd_del, "TestSlash", 4), class = "error_del_extensible")
    # }}}
    # }}}

    # FIELD {{{
    ## USING CLASS {{{
    expect_error(get_idd_field(idd_parsed, 10), class = "error_class_id")
    expect_error(get_idd_field(idd_parsed, ""), class = "error_class_name_us")
    expect_equal(get_idd_field(idd_parsed, c("TestSimple", "TestSlash")),
        data.table(field_id = 1:4, class_id = c(1L, rep(2L, 3)),
            field_index = c(1L, 1:3),
            field_name = c("Test Field", "Test Character Field 1",
                paste("Test Numeric Field", 1:2)),
            rleid = c(1L, rep(2L, 3)), class_name = c("TestSimple", rep("TestSlash", 3))
        )
    )
    expect_equivalent(get_idd_field(idd_parsed, c("TestSimple", "TestSlash"), all = TRUE),
        data.table(field_id = 1:5, class_id = c(1L, rep(2L, 4)),
            field_index = c(1L, 1:4),
            field_name = c("Test Field", "Test Character Field 1",
                paste("Test Numeric Field", 1:2), "Test Character Field 2"),
            rleid = c(1L, rep(2L, 4)), class_name = c("TestSimple", rep("TestSlash", 4))
        )
    )
    expect_equivalent(get_idd_field(idd_parsed, c("TestSimple", "TestSlash"), property = "type_enum"),
        data.table(field_id = 1:4, class_id = c(1L, rep(2L, 3)),
            field_index = c(1L, 1:3),
            field_name = c("Test Field", "Test Character Field 1",
                paste("Test Numeric Field", 1:2)),
            type_enum = c(4L, 5L, 2L, 2L), rleid = c(1L, rep(2L, 3)),
            class_name = c("TestSimple", rep("TestSlash", 3))
        )
    )
    # }}}
    ## USING FIELD INDEX {{{
    expect_error(get_idd_field(idd_parsed, c("TestSimple", "TestSlash"), c(2, 2)), class = "error_bad_field_index")
    expect_error(get_idd_field(idd_parsed, c("TestSimple", "TestSlash"), c(2, 2, 3)), class = "error_not_have_same_len")
    expect_error(get_idd_field(idd_parsed, c("TestSimple", "TestSlash"), c(1, 10), no_ext = TRUE), class = "error_bad_field_index")
    expect_equal(get_idd_field(idd_parsed, c("TestSimple", "TestSlash", "TestSlash"), c(1, 3, 99)),
        data.table(field_id = c(1L, 4L, 100L), class_id = c(1L, 2L, 2L),
            field_index = c(1L, 3L, 99L),
            field_name = c("Test Field", "Test Numeric Field 2", "Test Numeric Field 50"),
            rleid = c(1L, 2L, 3L), class_name = c("TestSimple", "TestSlash", "TestSlash"),
            field_in = c(1L, 3L, 99L)
        )
    )
    expect_silent({fld <- get_idd_field(idd_parsed, c("TestSlash", "TestSlash"), c(3, 19), complete = TRUE)})
    expect_equal(fld,
        data.table(field_id = c(2:5, 2:21), class_id = rep(2L, 24), field_index = c(1:4, 1:20),
            field_name = paste0(
                rep(c("Test Character Field ", "Test Numeric Field ", "Test Numeric Field ", "Test Character Field "), times = 6),
                c(rep(1:2, each = 2), rep(1:10, each = 2))
            ),
            rleid = c(rep(1L, 4), rep(2L, 20)), class_name = rep("TestSlash", 24),
            field_in = c(rep(NA_real_, 2), 3L, rep(NA_real_, 19), 19L, NA_real_)
        )
    )
    # }}}
    ## USING FIELD NAME {{{
    expect_equal(get_idd_field(idd_parsed, c(1L, 2L), c("test_field", "test_numeric_field_2")),
        data.table(field_id = c(1L, 4L), class_id = c(1L, 2L), field_index = c(1L, 3L),
            field_name = c("Test Field", "Test Numeric Field 2"),
            rleid = c(1L, 2L), class_name = c("TestSimple", "TestSlash"),
            field_in = c("test_field", "test_numeric_field_2")
        )
    )
    expect_equal(get_idd_field(idd_parsed, c(1L, 2L), c("test_field", "test_numeric_field_2"), complete = TRUE),
        data.table(field_id = 1:5, class_id = c(1L, rep(2L, 4)), field_index = c(1L, 1:4),
            field_name = c("Test Field", "Test Character Field 1", "Test Numeric Field 1",
                "Test Numeric Field 2", "Test Character Field 2"
            ),
            rleid = c(1L, rep(2L, 4)), class_name = c("TestSimple", rep("TestSlash", 4)),
            field_in = c("test_field", rep(NA_character_, 2),
                "test_numeric_field_2", NA_character_
            )
        )
    )
    expect_error(get_idd_field(idd_parsed, "TestSimple", ""), class = "error_bad_field_name")
    expect_error(get_idd_field(idd_parsed, "TestSlash", ""), class = "error_bad_field_name")
    expect_error(get_idd_field(idd_parsed, "TestSlash", "", no_ext = TRUE), class = "error_bad_field_name")
    expect_equal(get_idd_field(idd_parsed, c(1L, 2L), c("test_field", "test_numeric_field_3")),
        data.table(field_id = c(1L, 7L), class_id = c(1L, 2L), field_index = c(1L, 6L),
            field_name = c("Test Field", "Test Numeric Field 3"),
            rleid = c(1L, 2L), class_name = c("TestSimple", "TestSlash"),
            field_in = c("test_field", "test_numeric_field_3")
        )
    )
    expect_equal(get_idd_field(idd_parsed, c(1L, 2L), c("test_field", "test_numeric_field_3"), complete = TRUE),
        data.table(field_id = 1:9, class_id = c(1L, rep(2L, 8)), field_index = c(1L, 1:8),
            field_name = c("Test Field",
                paste0(
                    rep(c("Test Character Field ", "Test Numeric Field ", "Test Numeric Field ", "Test Character Field "), 2),
                    rep(1:4, each = 2)
                )),
            rleid = c(1L, rep(2L, 8)), class_name = c("TestSimple", rep("TestSlash", 8)),
            field_in = c("test_field", rep(NA_character_, 5),
                "test_numeric_field_3", rep(NA_character_, 2)
            )
        )
    )
    # }}}
    # }}}

    # RELATION {{{
    expect_equivalent(
        get_idd_relation(idd_parsed, name = TRUE),
        data.table(
            class_id = 2L, class_name = "TestSlash",
            field_id = 2L, field_index = 1L, field_name = "Test Character Field 1",
            src_class_id = 1L, src_class_name = "TestSimple",
            sec_field_id = 1L, src_field_index = 1L, src_field_name = "Test Field",
            src_enum = 2L, dep = 0L
        )
    )
    expect_equivalent(
        get_idd_relation(idd_parsed, name = TRUE, direction = "ref_by"),
        data.table(
            class_id = 2L, class_name = "TestSlash",
            field_id = 2L, field_index = 1L, field_name = "Test Character Field 1",
            src_class_id = 1L, src_class_name = "TestSimple",
            src_field_id = 1L, src_field_index = 1L, src_field_name = "Test Field",
            src_enum = 2L, dep = 0L
        )
    )
    # }}}

    # TABLE {{{
    expect_equal(get_iddobj_table(idd_parsed, 1),
        data.table(class = "TestSimple", index = 1L, field = "Test Field")
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
          "",
          "TestSlash,",
          ",!- Test Character Field 1",
          ",!- Test Numeric Field 1 {m}",
          ";!- Test Numeric Field 2"
        )

    )
    # }}}

})
# }}}
