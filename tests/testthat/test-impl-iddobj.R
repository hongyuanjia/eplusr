context("IddObject Implementation")

# IddObject {{{
test_that("IddObject implementation", {
    expect_silent(idd_parsed <- parse_idd_file(text("idd", "9.9.9")))

    # RELATION {{{
    expect_silent(rel <- get_iddobj_relation(idd_parsed, 2L, direction = "ref_to", name = TRUE, keep_all = TRUE))
    expect_equal(names(rel), c("ref_to", "ref_by"))
    expect_equivalent(
        rel$ref_to,
        data.table(
            class_id = 2L, class_name = "TestSlash",
            field_id = 2:5, field_index = 1:4,
            field_name = c("Test Character Field 1", "Test Numeric Field 1", "Test Numeric Field 2", "Test Character Field 2"),
            src_class_id = c(1L, rep(NA_integer_, 3)),
            src_class_name = c("TestSimple", rep(NA_character_, 3)),
            sec_field_id = c(1L, rep(NA_integer_, 3)),
            src_field_index = c(1L, rep(NA_integer_, 3)),
            src_field_name = c("Test Field", rep(NA_character_, 3L)),
            src_enum = c(2L, rep(NA_integer_, 3L)), dep = 0L
        )
    )
    expect_null(rel$ref_by)

    expect_silent(rel <- get_iddobj_relation(idd_parsed, 2L, direction = "ref_to", name = TRUE, keep_all = FALSE))
    expect_equal(names(rel), c("ref_to", "ref_by"))
    expect_equivalent(
        rel$ref_to,
        data.table(
            class_id = 2L, class_name = "TestSlash",
            field_id = 2L, field_index = 1L, field_name = "Test Character Field 1",
            src_class_id = 1L, src_class_name = "TestSimple",
            sec_field_id = 1L, src_field_index = 1L, src_field_name = "Test Field",
            src_enum = 2L, dep = 0L
        )
    )
    expect_null(rel$ref_by)

    expect_silent(rel <- get_iddobj_relation(idd_parsed, 2L, direction = "ref_by", name = TRUE, keep_all = FALSE))
    expect_equal(names(rel), c("ref_to", "ref_by"))
    expect_equivalent(
        rel$ref_by,
        data.table(
            class_id = integer(), class_name = character(),
            field_id = integer(), field_index = integer(), field_name = character(),
            src_class_id = integer(), src_class_name = character(),
            sec_field_id = integer(), src_field_index = integer(), src_field_name = character(),
            src_enum = integer(), dep = integer()
        )
    )
    expect_null(rel$ref_to)

    expect_silent(rel <- get_iddobj_relation(idd_parsed, 2L, direction = "all", name = FALSE, keep_all = FALSE))
    expect_equal(names(rel), c("ref_to", "ref_by"))
    expect_equivalent(
        rel$ref_to,
        data.table(
            class_id = 2L, field_id = 2L, src_class_id = 1L, sec_field_id = 1L,
            src_enum = 2L, dep = 0L
        )
    )
    expect_equal(rel$ref_by,
        data.table(class_id = integer(), field_id = integer(), src_class_id = integer(), src_field_id = integer(), src_enum = integer(), dep = integer())
    )
    # }}}

    # POSSIBLE {{{
    expect_equivalent(
        get_iddobj_possible(idd_parsed, 1L),
        data.table(class_id = 1L, class_name = "TestSimple",
            field_id = 1L, field_index = 1L,
            field_name = "Test Field",
            auto = NA_character_,
            default = list(NA_character_),
            choice = list(NULL),
            range = list(
                ranger(NA_real_, FALSE, NA_real_, FALSE)
            )
        )
    )
    expect_equivalent(
        get_iddobj_possible(idd_parsed, field_id = c(5L, 3L)),
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

    expect_equal(get_iddobj_string(idd_parsed, 2, comment = c(1, 2), leading = 0L, sep_at = 0L),
        c("!1", "!2", "",
          "TestSlash,",
          ",!- Test Character Field 1",
          ",!- Test Numeric Field 1 {m}",
          ";!- Test Numeric Field 2"
        )

    )
    # }}}
})
# }}}
