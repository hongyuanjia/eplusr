use_idd(8.8, "auto")

# VALUE {{{
test_that("get_idfobj_value()", {
    idf_env <- parse_idf_file(idftext("idf", 8.8))
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    expect_equivalent(get_idfobj_value(idd_env, idf_env, 1), tolerance = 1e-5,
        list(Name = "WD01",
             Roughness = "MediumSmooth",
             Thickness = 0.0191,
             Conductivity = 0.115,
             Density = 513,
             `Specific Heat` = 1381,
             `Thermal Absorptance` = 0.9,
             `Solar Absorptance` = 0.78,
             `Visible Absorptance` = 0.78
        )
    )

    expect_equivalent(get_idfobj_value(idd_env, idf_env, 1, unit = TRUE), tolerance = 1e-5,
        list(Name = "WD01",
             Roughness = "MediumSmooth",
             Thickness = units::set_units(0.0191, "m"),
             Conductivity = units::set_units(0.115, "W/K/m"),
             Density = units::set_units(513, "kg/m^3"),
             `Specific Heat` = units::set_units(1381, "J/K/kg"),
             `Thermal Absorptance` = 0.9,
             `Solar Absorptance` = 0.78,
             `Visible Absorptance` = 0.78
        )
    )

    expect_equal(get_idfobj_value(idd_env, idf_env, 1, simplify = TRUE),
        c("WD01", "MediumSmooth", "0.019099999", "0.115", "513", "1381", "0.9", "0.78", "0.78")
    )

    val <- get_idf_value(idd_env, idf_env, object = 1, property = "type_enum")
    expect_silent(with_option(list(view_in_ip = TRUE), get_value_list(val, TRUE)))

    idf_env <- parse_idf_file(example())
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    val <- get_idf_value(idd_env, idf_env, object = 8, property = "type_enum")
    val[2, `:=`(value_chr = "4.5", value_num = 4.5)]
    expect_warning(l <- get_value_list(val), "Truncated error")
    expect_equal(l[[2]], 4L)
})
# }}}

# VALUE_POSSIBLE {{{
test_that("get_idfobj_possible()", {
    idf_env <- parse_idf_file(idftext("idf", 8.8))
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    expect_equivalent(get_idfobj_possible(idd_env, idf_env, 2),
        data.table(class_id = 90L, class_name = "Construction", object_id = 2L,
            object_name = "WALL-1", field_id = 11006:11010, field_index = 1:5,
            field_name = c("Name", "Outside Layer", paste("Layer", 2:4)),
            value_id = 10:14, value_chr = c("WALL-1", "WD01", "PW03", "IN02", "GP01"),
            value_num = rep(NA_real_, 5),
            auto = NA_character_, default = rep(list(NA_character_), 5),
            choice = list(), range = rep(list(ranger(NA_real_, FALSE, NA_real_, FALSE)), 5),
            source = c(list(NULL), rep(list(c("WD01", "WD02")), 4))
        )
    )

    expect_equivalent(get_idfobj_possible(idd_env, idf_env, 2, 2),
        data.table(class_id = 90L, class_name = "Construction", object_id = 2L,
            object_name = "WALL-1", field_id = 11007, field_index = 2,
            field_name = "Outside Layer",
            value_id = 11, value_chr = "WD01", value_num = NA_real_,
            auto = NA_character_, default = list(NA_character_),
            choice = list(), range = list(ranger(NA_real_, FALSE, NA_real_, FALSE)),
            source = list(c("WD01", "WD02"))
        )
    )

    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    idf_env <- parse_idf_file(file.path(eplus_config(8.8)$dir, "ExampleFiles/5Zone_Transformer.idf"))
    expect_equal(length(get_idfobj_possible(idd_env, idf_env, object = 278, 11)$source[[1]]), 88)
})
# }}}

# VALUE_RELATION {{{
test_that("get_idfobj_relation()", {
    idf_env <- parse_idf_file(idftext("idf", 8.8))
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    expect_equivalent(get_idfobj_relation(idd_env, idf_env, 2, 10),
        list(
            ref_to = data.table(
                class_id = integer(), class_name = character(),
                object_id = integer(), object_name = character(),
                field_id = integer(), field_index = integer(), field_name = character(),
                value_id = integer(), value_chr = character(), value_num = double(), type_enum = integer(),
                src_class_id = integer(), src_class_name = character(),
                src_object_id = integer(), src_object_name = character(),
                src_field_id = integer(), src_field_index = integer(), src_field_name = character(),
                src_value_id = integer(), src_value_chr = character(), src_value_num = double(), src_type_enum = integer(),
                src_enum = integer(), dep = integer()
            ),
            ref_by = data.table(
                class_id = 103L, class_name = "BuildingSurface:Detailed",
                object_id = 3L, object_name = "WALL-1PF",
                field_id = 11624L, field_index = 3L, field_name = "Construction Name",
                value_id = 17L, value_chr = "WALL-1", value_num = NA_integer_, type_enum = 5L,
                src_class_id = 90L, src_class_name = "Construction",
                src_object_id = 2L, src_object_name = "WALL-1",
                src_field_id = 11006L, src_field_index = 1L, src_field_name = "Name",
                src_value_id = 10L, src_value_chr = "WALL-1", src_value_num = NA_integer_, src_type_enum = 4L,
                src_enum = 2L, dep = 0L
            ),
            node = data.table(
                class_id = integer(), class_name = character(),
                object_id = integer(), object_name = character(),
                field_id = integer(), field_index = integer(), field_name = character(),
                value_id = integer(), value_chr = character(), value_num = double(), type_enum = integer(),
                src_class_id = integer(), src_class_name = character(),
                src_object_id = integer(), src_object_name = character(),
                src_field_id = integer(), src_field_index = integer(), src_field_name = character(),
                src_value_id = integer(), src_value_chr = character(), src_value_num = double(), src_type_enum = integer(),
                src_enum = integer(), dep = integer()
            )
        )
    )
})
# }}}

# TABLE {{{
test_that("get_idfobj_table()", {
    idf_env <- parse_idf_file(idftext("idf", 8.8))
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    expect_equal(
        get_idfobj_table(idd_env, idf_env, 2),
        data.table(id = 2L, name = "WALL-1", class = "Construction", index = 1:5,
            field = c("Name", "Outside Layer", paste("Layer", 2:4)),
            value = c("WALL-1", "WD01", "PW03", "IN02", "GP01")
        )
    )
})
# }}}

# STRING {{{
test_that("get_idfobj_string()", {
    idf_env <- parse_idf_file(idftext("idf", 8.8))
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    expect_equal(get_idfobj_string(idd_env, idf_env, 2, leading = 0, sep_at = 10, comment = FALSE),
        c("Construction,",
          "WALL-1,   !- Name",
          "WD01,     !- Outside Layer",
          "PW03,     !- Layer 2",
          "IN02,     !- Layer 3",
          "GP01;     !- Layer 4")
    )

    expect_equal(get_idfobj_string(idd_env, idf_env, 1, leading = 0, sep_at = 10),
        c("! this is a test comment for WD01",
          "",
          "Material,",
          "WD01,     !- Name",
          "MediumSmooth,  !- Roughness",
          "0.019099999,  !- Thickness {m}",
          "0.115,    !- Conductivity {W/m-K}",
          "513,      !- Density {kg/m3}",
          "1381,     !- Specific Heat {J/kg-K}",
          "0.9,      !- Thermal Absorptance",
          "0.78,     !- Solar Absorptance",
          "0.78;     !- Visible Absorptance")
    )
})
# }}}

# COMMENT {{{
test_that("set_idfobj_comment", {
    idf_env <- parse_idf_file(idftext("idf", 8.8))
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    # can delete comments
    expect_equal(set_idfobj_comment(idd_env, idf_env, 1, comment = NULL)$comment, list(NULL))

    # can append comments
    expect_equal(set_idfobj_comment(idd_env, idf_env, 1, comment = "a")$comment[[1]][2], "a")

    # can prepend comments
    expect_equal(set_idfobj_comment(idd_env, idf_env, 1, comment = "a", append = FALSE)$comment[[1]][1], "a")

    # can reset comments
    expect_equal(set_idfobj_comment(idd_env, idf_env, 1, comment = "a", append = NULL)$comment, list("a"))

    # can detect invalid `append` value
    expect_error(set_idfobj_comment(idd_env, idf_env, 1, comment = "a", append = 1:2)$comment, class = "eplusr_error")

    # can wrap comment at specified `width`
    expect_equal(set_idfobj_comment(idd_env, idf_env, 1, comment = c("a", "bb ccc"), append = NULL, width = 1L)$comment[[1]], c("a", "bb", "ccc"))
})
# }}}
