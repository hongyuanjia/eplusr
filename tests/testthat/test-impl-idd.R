test_that("Idd implementation", {
    expect_silent(idd_parsed <- parse_idd_file(idftext("idd", "9.9.9")))

    # GROUP {{{
    expect_equal(get_idd_group_index(idd_parsed), 1L:2L)
    expect_equal(get_idd_group_index(idd_parsed, "TestGroup2"), 2L)
    expect_error(get_idd_group_index(idd_parsed, "Wrong"), class = "eplusr_error_invalid_group_name")
    expect_equal(get_idd_group_name(idd_parsed), c("TestGroup1", "TestGroup2"))
    expect_equal(get_idd_group_name(idd_parsed, 2L), "TestGroup2")
    expect_error(get_idd_group_name(idd_parsed, 3), class = "eplusr_error_invalid_group_index")
    # }}}

    # CLASS {{{
    expect_equivalent(get_idd_class(idd_parsed),
        idd_parsed$class[, .SD, .SDcols = c("class_id", "class_name", "group_id")]
    )
    expect_equivalent(d <- get_idd_class(idd_parsed, property = c("group_name", "group_id"))[],
        data.table(
            class_id = 1:2, class_name = c("TestSimple", "TestSlash"),
            group_id = 1:2, group_name = c("TestGroup1", "TestGroup2")
        )
    )
    expect_equivalent(get_idd_class(idd_parsed, property = "group_name"),
        set(idd_parsed$class[, .SD, .SDcols = c("class_id", "class_name", "group_id")],
            NULL, "group_name", c("TestGroup1", "TestGroup2")
        )
    )
    expect_error(get_idd_class(idd_parsed, ""), class = "eplusr_error_invalid_class_name")
    expect_error(get_idd_class(idd_parsed, 10L), class = "eplusr_error_invalid_class_index")

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

    expect_equivalent(get_idd_class_field_num(copy(idd_parsed$class)),
        set(copy(idd_parsed$class), NULL, c("input_num", "acceptable_num"), list(0L, c(0L, 3L)))
    )
    expect_equivalent(names(get_idd_class_field_num(idd_parsed$class[0L])),
        names(set(idd_parsed$class[0L], NULL, c("input_num", "acceptable_num"), integer(0))[])
    )

    expect_equal(get_class_component_name("Material"), "Material")
    expect_equal(get_class_component_name("Material:NoMass"), "Material")
    expect_equal(get_class_component_name("BuildingSurface:Detailed"), "BuildingSurface")
    # }}}

    # EXTENSIBLE GROUP {{{
    # ADD {{{
    expect_equal(
        {
            cls <- get_idd_class(idd_parsed, "TestSimple", property = c("min_fields", "num_fields", "num_extensible", "last_required", "num_extensible_group"))
            add_idd_extensible_group(idd_parsed, cls, 1)$field
        },
        idd_parsed$field
    )
    expect_equal(add_idd_extensible_group(idd_parsed, "TestSimple", 1)$field, idd_parsed$field)
    expect_error(add_idd_extensible_group(idd_parsed, "TestSimple", 1, strict = TRUE), "Non-extensible class", class = "eplusr_error_non_extensible_class")
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
    expect_error(del_idd_extensible_group(idd_parsed, "TestSimple", 1, strict = TRUE), "Non-extensible class", class = "eplusr_error_non_extensible_class")
    expect_equivalent((idd_del <- del_idd_extensible_group(idd_added, "TestSlash", 1))$field, idd_parsed$field)
    expect_equal(idd_del$class$num_fields[2L], 4L)
    expect_equal(idd_del$class$num_extensible_group[2L], 1L)
    expect_error(del_idd_extensible_group(idd_del, "TestSlash", 4), "0 left with 1 required", class = "eplusr_error")
    # }}}
    # }}}

    # FIELD {{{
    ## USING CLASS {{{
    expect_error(get_idd_field(idd_parsed, 10), class = "eplusr_error_invalid_class_index")
    expect_error(get_idd_field(idd_parsed, ""), class = "eplusr_error_invalid_class_name")
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
    expect_error(get_idd_field(idd_parsed, c("TestSimple", "TestSlash"), c(2, 2)), class = "eplusr_error_invalid_field_index")
    expect_error(get_idd_field(idd_parsed, c("TestSimple", "TestSlash"), c(2, 2, 3)), "Must have same length")
    expect_error(get_idd_field(idd_parsed, c("TestSimple", "TestSlash"), c(1, 10), no_ext = TRUE), class = "eplusr_error_invalid_field_index")
    expect_equal(get_idd_field(idd_parsed, c("TestSimple", "TestSlash", "TestSlash"), c(1, 3, 99)),
        data.table(field_id = c(1L, 4L, 100L), class_id = c(1L, 2L, 2L),
            field_index = c(1L, 3L, 99L),
            field_name = c("Test Field", "Test Numeric Field 2", "Test Numeric Field 50"),
            rleid = c(1L, 2L, 3L), class_name = c("TestSimple", "TestSlash", "TestSlash"),
            field_in = c(1L, 3L, 99L)
        )
    )
    expect_silent({fld <- get_idd_field(idd_parsed, c("TestSlash", "TestSlash"), c(3, 19), all = TRUE)})
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
    expect_error(get_idd_field(idd_parsed, "TestSimple", ""), class = "eplusr_error_invalid_field_name")
    expect_error(get_idd_field(idd_parsed, "TestSlash", ""), class = "eplusr_error_invalid_field_name")
    expect_error(get_idd_field(idd_parsed, "TestSlash", "", no_ext = TRUE), class = "eplusr_error_invalid_field_name")
    expect_equal(get_idd_field(idd_parsed, 1L, "Test Field", underscore = FALSE),
        data.table(field_id = 1L, class_id = 1L, field_index = 1L,
            field_name = "Test Field", rleid = 1L, class_name = "TestSimple",
            field_in = "Test Field"
        )
    )
    expect_equal(get_idd_field(idd_parsed, c(1L, 2L), c("test_field", "test_numeric_field_3")),
        data.table(field_id = c(1L, 7L), class_id = c(1L, 2L), field_index = c(1L, 6L),
            field_name = c("Test Field", "Test Numeric Field 3"),
            rleid = c(1L, 2L), class_name = c("TestSimple", "TestSlash"),
            field_in = c("test_field", "test_numeric_field_3")
        )
    )
    expect_equal(get_idd_field(idd_parsed, c(1L, 2L), c("test_field", "test_numeric_field_3"), all = TRUE),
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
    expect_equal(nrow(get_idd_field(idd_parsed, 2L, "test_numeric_field_3", all = TRUE)), 8L)

    expect_equal(get_idd_field(idd_parsed, 2, "test_numeric_field_2", all = TRUE)$field_index, 1:4)
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

    idd <- use_idd(8.8, "auto")
    idd_env <- get_priv_env(idd)$idd_env()
    fld <- get_idd_field(idd_env, "Construction")
    expect_equal(nrow(get_idd_relation(idd_env, field_id = fld$field_id, direction = "ref_to", keep_all = TRUE, depth = 0L)), 15L)

    fld <- get_idd_field(idd_env, "Material")
    expect_equal(nrow(get_idd_relation(idd_env, field_id = fld$field_id, direction = "ref_to", depth = NULL)), 0L)
    expect_equal(nrow(get_idd_relation(idd_env, class_id = fld$class_id, direction = "ref_to", depth = NULL)), 0L)
    expect_error(get_idd_relation(idd_env, class_id = fld$class_id, field_id = fld$field_id), class = "eplusr_error_idd_relation")

    fld <- get_idd_field(idd_env, "Construction")
    expect_equal(nrow(get_idd_relation(idd_env, field_id = fld$field_id, direction = "ref_by", keep_all = TRUE, depth = 2L)), 29697L)

    fld <- get_idd_field(idd_env, "Construction", 2L)
    expect_equal(nrow(get_idd_relation(idd_env, field_id = fld$field_id, direction = "ref_to", depth = 0L)), 14L)

    fld <- get_idd_field(idd_env, "Construction", 1L)
    expect_equal(nrow(get_idd_relation(idd_env, field_id = fld$field_id, direction = "ref_by", depth = 3L, class = "PlantEquipmentOperationSchemes")), 212L)

    fld <- get_idd_field(idd_env, "Branch", 3:4)
    expect_equal(nrow(get_idd_relation(idd_env, field_id = fld$field_id, direction = "ref_to", depth = 0L)), 123L)

    fld <- get_idd_field(idd_env, "Pump:ConstantSpeed")
    expect_equal(nrow(get_idd_relation(idd_env, field_id = fld$field_id, direction = "ref_by",
            group = "Node-Branch Management", depth = 2L)), 11L)
    fld <- get_idd_field(idd_env, "Branch", 1:4, property = "type_enum")
    expect_equal(nrow(get_idd_relation(idd_env, field_id = fld$field_id, direction = "ref_to",
            class_ref = "none", group = "Node-Branch Management", depth = 0L)), 7L)
    expect_equal(nrow(get_idd_relation(idd_env, field_id = fld$field_id, direction = "ref_to",
            class_ref = "all", group = "Node-Branch Management", depth = 0L)), 14L)

    expect_equal(nrow(get_idd_relation(idd_env, field_id = fld$field_id, class = "Version")), 0L)
    # }}}

    # PROPERTY COLUMNS {{{
    dt_in <- idd_env$class[1:5, .(class_name)]
    expect_equal(add_class_id(idd_env, dt_in), set(dt_in, NULL, "class_id", list(1:5)))

    dt_in <- idd_env$class[1:3, .(class_id)]
    expect_equal(add_class_name(idd_env, dt_in),
        set(dt_in, NULL, "class_name", list(c("Version", "SimulationControl", "Building"))))

    dt_in <- idd_env$class[1:3, .(class_id)]
    expect_equal(add_class_property(idd_env, dt_in, c("group_name", "num_fields")),
        set(dt_in, NULL, c("num_fields", "group_name"), list(c(1L, 7:8), "Simulation Parameters")))
    dt_in <- idd_env$class[1:3, .(class_id, group_id)]
    expect_equal(add_class_property(idd_env, dt_in, c("group_name", "num_fields")),
        set(dt_in, NULL, c("num_fields", "group_name"), list(c(1L, 7:8), "Simulation Parameters")))

    dt_in <- idd_env$field[1:3, .(field_id)]
    expect_equal(add_field_property(idd_env, dt_in, "type_enum"),
        set(dt_in, NULL, "type_enum", list(c(4L, 3L, 3L))))
    # }}}

    # UNIT CONVERSION {{{
    fld <- get_idd_field(idd_env, "WindowMaterial:Glazing:RefractionExtinctionMethod", 9, property = c("units", "ip_units"))
    expect_equal(field_default_to_unit(idd_env, fld, "si", "ip")$default_num,
        drop_units(set_units(set_units(0.9, "W/m/K"), "Btu*in/h/ft^2/degF"))
    )

    # can keep input value_id
    fld <- get_idd_field(idd_env, "Material", 1:6)
    set(fld, NULL, "value_id", 1:6)
    expect_equal(field_default_to_unit(idd_env, fld, "si", "ip")$value_id, 1:6)
    # }}}

    # TABLE {{{
    expect_equal(get_idd_table(idd_parsed, 1),
        data.table(class = "TestSimple", index = 1L, field = "Test Field")
    )
    # }}}

    # STRING {{{
    expect_equal(get_idd_string(idd_parsed, 2, leading = 0L, sep_at = 0L),
        c("TestSlash,",
          ",!- Test Character Field 1",
          ",!- Test Numeric Field 1 {m}",
          ";!- Test Numeric Field 2"
        )

    )
    # }}}
})
