context("IdfObject")

# IdfObject class{{{
test_that("IdfObject class", {
    idf <- read_idf(text("idf", 8.8), use_idd(8.8, "auto"))
    ver <- idf$Version
    mat <- idf$Material$WD01
    surf <- idf$BuildingSurface_Detailed[["WALL-1PF"]]
    con <- idf$Construction[["WALL-1"]]

    # Basic {{{
    # get group name
    expect_equal(con$group_name(), "Surface Construction Elements")

    # get class name
    expect_equal(con$class_name(), "Construction")

    # get object ID
    expect_equal(mat$id(), 1L)
    # }}}

    # Comment {{{
    expect_equal(mat$comment(), " this is a test comment for WD01")

    # can handle invalid input types of comment
    expect_error(mat$comment(comment = list("a")), class = "error_not_is.atomic")

    # can delete comments
    expect_equal(mat$comment(comment = NULL)$comment(), NULL)

    # can add comments when comment is NA before
    expect_equal(mat$comment(comment = c("a"))$comment(), "a")

    # can append comments
    expect_equal(mat$comment(comment = c("b"))$comment(), c("a", "b"))

    # can prepend comments
    expect_equal(mat$comment(comment = c("c"), append = FALSE)$comment(),
        c("c", "a", "b"))

    # can reset comments
    expect_equal(mat$comment(comment = c("d"), append = NULL)$comment(), "d")

    # can detect invalid `append` value
    expect_error(mat$comment(comment = c("b"), append = 1:2),
        class = "error_not_flag")

    # can wrap comment at specified `width`
    expect_equal(mat$comment(comment = c("a", "bb ccc"), append = NULL, width = 1L)$comment(),
        c("a", "bb", "ccc"))

    # can detect invalid `width` value
    expect_error(mat$comment(comment = c("a"), append = NULL, width = "a"))
    # }}}

    # Get Values {{{
    # can handle cases when both `index` and `name` are NULL
    expect_equivalent(mat$value(), tolerance = 1e-5,
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

    expect_equivalent(mat$value(unit = TRUE), tolerance = 1e-5,
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

    expect_equal(mat$value(simplify = TRUE),
        c("WD01", "MediumSmooth", "0.019099999", "0.115", "513", "1381", "0.9", "0.78", "0.78")
    )

    # can detect invaid `index` values
    expect_error(mat$value("1"), class = "error_bad_field_name")
    expect_error(mat$value(c(1, 10:11)), class = "error_bad_field_index")

    # can return subset of values in a object using `index`
    expect_equivalent(mat$value(c(3, 1, 5)), tolerance = 1e-5,
        list(Thickness = 0.0191, Name = "WD01", Density = 513)
    )
    expect_equal(mat[[2]], "MediumSmooth")
    expect_equal(mat[["Roughness"]], "MediumSmooth")
    expect_equal(mat[c(2,1)], list(Roughness = "MediumSmooth", Name = "WD01"))

    # can return subset of values in a object using `name`
    expect_equivalent(mat$value("Roughness"), list(Roughness = "MediumSmooth"))
    expect_equivalent(mat$value("Roughness", simplify = TRUE), "MediumSmooth")
    expect_equal(mat$Roughness, "MediumSmooth")

    # can detect invalid `name` values
    expect_error(mat$value(c("Thickness", "Wrong", "Name")), class = "error_bad_field_name")
    # }}}

    # Set Values {{{
    # can stop when trying to directly modify `Version` object
    expect_error(ver$set(8.8), class = "error_set_version")

    # can stop when no values are given
    expect_error(con$set(), class = "error_dot_empty")

    expect_error(con$set(name = "named", "unnamed"),
        class = "error_validity")

    # can stop when duplicated names are given
    expect_error(con$set(name = "first", name = "second"),
        class = "error_dot_dup_field_name")

    # can stop when invalid names are given for a non-extensible class
    expect_error(mat$set(wrong = "something"),
        class = "error_bad_field_name")

    # can stop when invalid names are given for an extensible class
    expect_error(con$set(name = "first", wrong = "second"),
        class = "error_bad_field_name")

    # can stop when valid names are given, but total field values are not accepatable for an extensible class
    idf$add(Zone = list("PLENUM-1"))
    expect_error(surf$set(vertex_5_x_coordinate = 1, vertex_5_y_coordinate = 2),
        class = "error_validity"
    )

    # can stop when total field values are acceptable but invalid names are given for an extensible class
    expect_error(
        surf$set(vertex_5_x_coordinate = 1, vertex_5_y_coordinate = 2, vertex_5_z_wrong = 3),
        class = "error_bad_field_name"
    )

    # can add new values for extensible fields
    expect_silent(surf$set(vertex_5_x_coordinate = 1, vertex_5_y_coordinate = 2, vertex_5_z_coordinate = 3))
    expect_equal(surf$value()[23:25],
        list(`Vertex 5 X-coordinate` = 1, `Vertex 5 Y-coordinate` = 2, `Vertex 5 Z-coordinate` = 3)
    )
    # can change referenced values accordingly
    expect_equal(mat$set(name = "NewMaterialName")$value("Name")[[1]], "NewMaterialName")
    expect_equal(con$value("Outside Layer")[[1]], "NewMaterialName")

    # can stop when there are invalid references in the input
    expect_error(con$set(layer_2 = "missing"), class = "error_validity")

    # works using `[[<-.IdfObject`
    expect_silent(mat$Name <- "NewMaterial")
    expect_equal(mat$name(), "NewMaterial")
    expect_silent(mat[["Name"]] <- "NewMaterialName1")
    expect_equal(mat$name(), "NewMaterialName1")
    expect_silent(mat[[1]] <- "NewMaterialName")
    expect_equal(mat$name(), "NewMaterialName")
    # }}}

    # Possible {{{
    expect_equivalent(con$value_possible(),
        data.table(class_id = 90L, class_name = "Construction", object_id = 2L,
            object_name = "WALL-1", field_id = 11006:11010, field_index = 1:5,
            field_name = c("Name", "Outside Layer", paste("Layer", 2:4)),
            value_id = 10:14, value_chr = c("WALL-1", "NewMaterialName", "PW03", "IN02", "GP01"),
            value_num = rep(NA_real_, 5),
            auto = NA_character_, default = rep(list(NA_character_), 5),
            choice = list(), range = rep(list(ranger(NA_real_, FALSE, NA_real_, FALSE)), 5),
            source = c(list(NULL), rep(list(c("NewMaterialName", "WD02")), 4))
        )
    )
    expect_equivalent(con$value_possible(6),
        data.table(class_id = 90L, class_name = "Construction", object_id = 2L,
            object_name = "WALL-1", field_id = 11011, field_index = 6L,
            field_name = "Layer 5", value_id = -1L, value_chr = NA_character_, value_num = NA_real_,
            auto = NA_character_, default = list(NA_character_),
            choice = list(), range = list(ranger(NA_real_, FALSE, NA_real_, FALSE)),
            source = list(c("NewMaterialName", "WD02"))
        )
    )
    # }}}

    # Relation {{{
    expect_equivalent(con$value_relation(1),
        list(
            ref_to = data.table(
                class_id = 90L, class_name = "Construction",
                object_id = 2L, object_name = "WALL-1",
                field_id = 11006L, field_index = 1L, field_name = "Name",
                value_id = 10L, value_chr = "WALL-1", value_num = NA_real_, type_enum = 4L,
                src_class_id = NA_integer_, src_class_name = NA_character_,
                src_object_id = NA_integer_, src_object_name = NA_character_,
                src_field_id = NA_integer_, src_field_index = NA_integer_, src_field_name = NA_character_,
                src_value_id = NA_integer_, src_value_chr = NA_character_, src_value_num = NA_real_, src_type_enum = NA_integer_,
                src_enum = NA_integer_, dep = 0L
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
                class_id = NA_integer_, class_name = NA_character_,
                object_id = NA_integer_, object_name = NA_character_,
                field_id = NA_integer_, field_index = NA_integer_, field_name = NA_character_,
                value_id = NA_integer_, value_chr = NA_character_, value_num = NA_real_, type_enum = NA_integer_,
                src_class_id = 90L, src_class_name = "Construction",
                src_object_id = 2L, src_object_name = "WALL-1",
                src_field_id = 11006L, src_field_index = 1L, src_field_name = "Name",
                src_value_id = 10L, src_value_chr = "WALL-1", src_value_num = NA_real_, src_type_enum = 4L,
                src_enum = 2L, dep = 0L
            )
        )
    )

    expect_equal(names(con$ref_to_object()), "NewMaterialName")
    expect_equal(names(con$ref_by_object()), "WALL-1PF")

    expect_equal(con$has_ref(), c(TRUE, TRUE, FALSE, FALSE, FALSE))
    expect_true(con$has_ref(1))
    expect_equal(con$has_ref_to(), c(FALSE, TRUE, FALSE, FALSE, FALSE))
    expect_false(con$has_ref_to(1))
    expect_true(con$has_ref_to(2))
    expect_equal(con$has_ref_by(), c(TRUE, rep(FALSE, 4)))
    expect_true(con$has_ref_by(1))
    expect_false(con$has_ref_by(2))
    # }}}

    # Validate {{{
    expect_equal(con$validate()$invalid_reference,
        data.table(object_id = 2L, object_name = "WALL-1", class_id = 90L,
            class_name = "Construction", field_id = 11008:11010,
            field_index = 3:5, field_name = paste("Layer", 2:4),
            units = rep(NA_character_, 3L), ip_units = rep(NA_character_, 3L),
            type_enum = 5L, value_id = 12:14,
            value_chr = c("PW03", "IN02", "GP01"),
            value_num = rep(NA_real_, 3L)
        )
    )
    expect_true(ver$is_valid())
    expect_true(mat$is_valid())
    expect_false(con$is_valid())
    expect_true(surf$is_valid())
    # }}}

    # Table {{{
    expect_equal(
        con$to_table(all = FALSE, unit = TRUE, wide = FALSE, string_value = TRUE),
        data.table(id = 2L, name = "WALL-1", class = "Construction", index = 1:5,
            field = c("Name", "Outside Layer", paste("Layer", 2:4)),
            value = c("WALL-1", "NewMaterialName", "PW03", "IN02", "GP01")
        )
    )
    expect_equal(
        con$to_table(all = TRUE, unit = TRUE, wide = FALSE, string_value = TRUE),
        data.table(id = 2L, name = "WALL-1", class = "Construction", index = 1:11,
            field = c("Name", "Outside Layer", paste("Layer", 2:10)),
            value = c("WALL-1", "NewMaterialName", "PW03", "IN02", "GP01", rep(NA_character_, 6))
        )
    )
    expect_equal(
        con$to_table(all = TRUE, unit = FALSE, wide = FALSE, string_value = FALSE),
        data.table(id = 2L, name = "WALL-1", class = "Construction", index = 1:11,
            field = c("Name", "Outside Layer", paste("Layer", 2:10)),
            value = as.list(c("WALL-1", "NewMaterialName", "PW03", "IN02", "GP01", rep(NA_character_, 6)))
        )
    )
    expect_equivalent(
        con$to_table(all = TRUE, unit = TRUE, wide = TRUE, string_value = FALSE),
        data.table(id = 2L, name = "WALL-1", class = "Construction",
            Name = "WALL-1", `Outside Layer` = "NewMaterialName", `Layer 2` = "PW03",
            `Layer 3` = "IN02", `Layer 4` = "GP01", `Layer 5` = NA_character_,
            `Layer 6` = NA_character_, `Layer 7` = NA_character_, `Layer 8` = NA_character_,
            `Layer 9` = NA_character_, `Layer 10` = NA_character_
        )
    )
    expect_equivalent(tolerance = 1e-5,
        mat$to_table(string_value = FALSE),
        data.table(id = 1L, name = "NewMaterialName", class = "Material", index = 1:9,
            field = mat$definition()$field_name(),
            value = list("NewMaterialName", "MediumSmooth", units::set_units(0.0191, m),
                units::set_units(0.115, W/K/m), units::set_units(513, kg/m^3),
                units::set_units(1381, J/K/kg), 0.9, 0.78, 0.78
            )
        )
    )
    expect_equivalent(tolerance = 1e-5,
        mat$to_table(wide = TRUE, string_value = FALSE),
        data.table(id = 1L, name = "NewMaterialName", class = "Material", Name = "NewMaterialName",
            `Roughness` = "MediumSmooth",
            `Thickness` = units::set_units(0.0191, m),
            `Conductivity` = units::set_units(0.115, W/K/m),
            `Density` = units::set_units(513, kg/m^3),
            `Specific Heat` = units::set_units(1381, J/K/kg),
            `Thermal Absorptance` = 0.9, `Solar Absorptance` = 0.78,
            `Visible Absorptance` = 0.78
        )
    )
    # }}}

    # String {{{
    expect_equal(con$to_string(leading = 0, sep_at = 10),
        c("Construction,",
          "WALL-1,   !- Name",
          "NewMaterialName,  !- Outside Layer",
          "PW03,     !- Layer 2",
          "IN02,     !- Layer 3",
          "GP01;     !- Layer 4")
    )
    expect_equal(con$to_string(leading = 0, sep_at = 10, all = TRUE),
        c("Construction,",
          "WALL-1,   !- Name",
          "NewMaterialName,  !- Outside Layer",
          "PW03,     !- Layer 2",
          "IN02,     !- Layer 3",
          "GP01,     !- Layer 4",
          ",         !- Layer 5",
          ",         !- Layer 6",
          ",         !- Layer 7",
          ",         !- Layer 8",
          ",         !- Layer 9",
          ";         !- Layer 10"
        )
    )
    # }}}

    # S3 subsetting works {{{
    expect_equal(mat$Roughness, "MediumSmooth")
    expect_null(mat$rOuGhness)
    expect_equal(mat[["Roughness"]], "MediumSmooth")
    expect_null(mat[["roughness"]])
    expect_silent(mat$Roughness <- "Smooth")
    expect_equal(mat$Roughness, "Smooth")
    expect_silent(mat[["Roughness"]] <- "MediumSmooth")
    expect_equal(mat[["Roughness"]], "MediumSmooth")

    expect_error(mat$roughness <- "Smooth", "cannot add bindings to")
    expect_error(mat[["roughness"]] <- "Smooth", "cannot add bindings to")
    expect_equal(mat$Visible_Absorptance, 0.78, tolerance = 1e-5)
    expect_silent(mat$Visible_Absorptance <- 0.8)
    expect_equal(mat$Visible_Absorptance, 0.8, tolerance = 1e-5)
    expect_silent(mat[["Visible Absorptance"]] <- 0.8)
    expect_equal(mat[["Visible Absorptance"]], 0.8, tolerance = 1e-5)
    expect_error(mat[["Visible_Absorptance"]] <- 0.8, "cannot add bindings to")
    # }}}

    expect_output(con$print())
})
# }}}
