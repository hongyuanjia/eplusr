context("IDF Implementation")

eplusr_option(validate_level = "final")

# TABLE {{{
test_that("table", {
    idf_env <- parse_idf_file(text("idf", 8.8))
    idd_env <- ._get_private(use_idd(8.8))$m_idd_env

    # OBJECT {{{
    expect_equal(get_idf_object(idd_env, idf_env, 1),
        data.table(class_id = 1L, object_id = 5L, comment = list(),
            object_name = NA_character_, object_name_lower = NA_character_,
            rleid = 1L, class_name = "Version"
        )
    )
    expect_equal(get_idf_object(idd_env, idf_env, "Version"),
        data.table(class_id = 1L, object_id = 5L, comment = list(),
            object_name = NA_character_, object_name_lower = NA_character_,
            rleid = 1L, class_name = "Version"
        )
    )
    expect_equal(get_idf_object(idd_env, idf_env, "Version", 5),
        data.table(object_id = 5L, class_id = 1L, comment = list(),
            object_name = NA_character_, object_name_lower = NA_character_,
            class_name = "Version", rleid = 1L
        )
    )
    expect_equal(get_idf_object(idd_env, idf_env, "Version", 5, c("num_fields")),
        data.table(object_id = 5L, class_id = 1L, comment = list(),
            object_name = NA_character_, object_name_lower = NA_character_,
            class_name = "Version", num_fields = 1L, rleid = 1L
        )
    )
    expect_equal(get_idf_object(idd_env, idf_env), add_class_name(idd_env, copy(idf_env$object)))
    expect_equal(get_idf_object(idd_env, idf_env, 55)$object_id, c(1L, 4L))
    expect_equal(get_idf_object(idd_env, idf_env, 55, c("WD02", "WD01"))$object_id, c(4L, 1L))
    expect_equal(get_idf_object(idd_env, idf_env, "Material")$object_id, c(1L, 4L))
    expect_equal(get_idf_object(idd_env, idf_env, "Material", c("WD02", "WD01"))$object_id, c(4L, 1L))
    expect_error(get_idf_object(idd_env, idf_env, 2), "Invalid class index")
    expect_error(get_idf_object(idd_env, idf_env, "Branch"), "Invalid class name")
    expect_error(get_idf_object(idd_env, idf_env, "Material", "wrong"), "Invalid object name")
    expect_error(get_idf_object(idd_env, idf_env, "Material", 15), "Invalid object ID")
    expect_equal(get_idf_object(idd_env, idf_env, 55, c("wd02", "wd01"), ignore_case = TRUE)$object_id, c(4L, 1L))

    expect_error(get_idf_object_id(idd_env, idf_env, 10000), "Invalid class index")
    expect_error(get_idf_object_id(idd_env, idf_env, "Branch"), "Invalid class name")
    expect_equal(get_idf_object_id(idd_env, idf_env),
        list(Version = 5L, Material = c(1L, 4L), Construction = 2L, `BuildingSurface:Detailed` = 3L)
    )
    expect_equal(get_idf_object_id(idd_env, idf_env, simplify = TRUE), 1L:5L)
    expect_equal(get_idf_object_id(idd_env, idf_env, "Material"), list(Material = c(1L, 4L)))
    expect_equal(get_idf_object_id(idd_env, idf_env, 55), list(Material = c(1L, 4L)))
    expect_equal(get_idf_object_id(idd_env, idf_env, 55, simplify = TRUE), c(1L, 4L))
    expect_equal(get_idf_object_id(idd_env, idf_env, "Material", simplify = TRUE), c(1L, 4L))

    expect_equal(get_idf_object_name(idd_env, idf_env),
        list(Version = NA_character_, Material = c("WD01", "WD02"),
            Construction = "WALL-1", `BuildingSurface:Detailed` = "WALL-1PF")
    )
    expect_equal(get_idf_object_name(idd_env, idf_env, simplify = TRUE),
        c("WD01", "WALL-1", "WALL-1PF", "WD02", NA_character_)
    )
    expect_equal(get_idf_object_name(idd_env, idf_env, "Material"), list(Material = c("WD01", "WD02")))
    expect_equal(get_idf_object_name(idd_env, idf_env, 55), list(Material = c("WD01", "WD02")))
    expect_equal(get_idf_object_name(idd_env, idf_env, 55, simplify = TRUE), c("WD01", "WD02"))
    expect_equal(get_idf_object_name(idd_env, idf_env, "Material", simplify = TRUE), c("WD01", "WD02"))

    expect_equal(get_idf_object_num(idd_env, idf_env), 5L)
    expect_equal(get_idf_object_num(idd_env, idf_env, c(55, 55, 100)), c(2L, 2L, 0L))
    expect_error(get_idf_object_num(idd_env, idf_env, c(55, 55, 10000)), "Invalid class index")
    expect_equal(get_idf_object_num(idd_env, idf_env, c("Material", "Material")), c(2L, 2L))
    expect_equal(get_idf_object_num(idd_env, idf_env, c("Material", "Material", "Branch")), c(2L, 2L, 0L))

    expect_equal(get_idf_object_id(idd_env, idf_env, 1), list(Version = 5L))
    expect_equal(get_idf_object_id(idd_env, idf_env, "Version"), list(Version = 5L))
    expect_equal(get_idf_object_id(idd_env, idf_env, 1, simplify = TRUE), 5L)
    expect_equal(get_idf_object_id(idd_env, idf_env, "Version", simplify = TRUE), 5L)
    expect_equal(get_idf_object_name(idd_env, idf_env, c("Version", "Material")), list(Version = NA_character_, Material = c("WD01", "WD02")))
    expect_equal(get_idf_object_name(idd_env, idf_env, c("Version", "Material"), simplify = TRUE), c(NA_character_, c("WD01", "WD02")))
    expect_equal(get_idf_object_num(idd_env, idf_env, c("Version", "Material")), c(1L, 2L))
    expect_equal(get_object_info(add_class_name(idd_env, idf_env$object[1])), " #1| Object ID [1] (name `WD01`) in class Material")
    expect_equal(get_object_info(add_class_name(idd_env, idf_env$object[5])), " #1| Object ID [5] in class Version")
    expect_equal(get_object_info(idf_env$object[1], c("id", "name")), " #1| Object ID [1] (name `WD01`)")
    expect_equal(get_object_info(idf_env$object[1], c("name")), " #1| Object name `WD01`")
    # }}}

    # VALUE {{{
    # get all value from current idf {{{
    expect_equivalent(nrow(get_idf_value(idd_env, idf_env)), 44L)
    expect_equivalent(names(get_idf_value(idd_env, idf_env)),
        c("value_id", "value_chr", "value_num", "object_id", "field_id",
        "class_id", "object_name", "class_name", "field_index", "field_name"
        )
    )
    # }}}
    # get value from class {{{
    # get values from certain class {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, "Material")})
    expect_equivalent(val$value_id, c(1:9, 40:43))
    expect_equivalent(val$object_id, c(rep(1L, 9), rep(4L, 4)))
    expect_equivalent(val$field_id, c(7081:7089, 7081:7084))
    expect_equivalent(val$class_id, rep(55L, 13))
    expect_equivalent(val$field_index, c(1:9, 1:4))
    expect_equivalent(val$field_name,
       c(
          c("Name", "Roughness", "Thickness", "Conductivity", "Density",
            "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
            "Visible Absorptance"),
          c("Name", "Roughness", "Thickness", "Conductivity")
       )
    )
    expect_equivalent(val$rleid, rep(1L, 13))
    expect_equivalent(val$class_name, rep("Material", 13))
    expect_equivalent(val$object_name, c(rep("WD01", 9), rep("WD02", 4)))
    # }}}
    # get values from class but ensure all objects have same field {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, "Material", align = TRUE)})
    expect_equivalent(val$value_id, c(1:9, 40:43, -1:-5))
    expect_equivalent(val$object_id, rep(c(1L, 4L), each = 9))
    expect_equivalent(val$field_id, rep(7081:7089, 2))
    expect_equivalent(val$class_id, rep(55L, 18))
    expect_equivalent(val$field_index, rep(1:9, 2))
    expect_equivalent(val$field_name,
       rep(
          c("Name", "Roughness", "Thickness", "Conductivity", "Density",
            "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
            "Visible Absorptance"),
         2
       )
    )
    expect_equivalent(val$rleid, rep(1L, 18))
    expect_equivalent(val$class_name, rep("Material", 18))
    expect_equivalent(val$object_name, rep(c("WD01", "WD02"), each = 9))
    # }}}
    # get values from class and ensure all objects have min required fields {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, "Material", complete = TRUE)})
    expect_equivalent(val$value_id, c(1:9, 40:43, -1:-2))
    expect_equivalent(val$object_id, c(rep(1L, 9), rep(4L, 6)))
    expect_equivalent(val$field_id, c(7081:7089, 7081:7086))
    expect_equivalent(val$class_id, rep(55L, 15))
    expect_equivalent(val$field_index, c(1:9, 1:6))
    expect_equivalent(val$field_name,
       c(
          c("Name", "Roughness", "Thickness", "Conductivity", "Density",
            "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
            "Visible Absorptance"),
          c("Name", "Roughness", "Thickness", "Conductivity", "Density", "Specific Heat")
       )
    )
    expect_equivalent(val$rleid, rep(1L, 15))
    expect_equivalent(val$class_name, rep("Material", 15))
    expect_equivalent(val$object_name, c(rep("WD01", 9), rep("WD02", 6)))
    # }}}
    # get values from class and ensure all objects have min required fields and same field number {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, "Material", align = TRUE, complete = TRUE)})
    expect_equivalent(val$value_id, c(1:9, 40:43, -1:-5))
    expect_equivalent(val$object_id, rep(c(1L, 4L), each = 9))
    expect_equivalent(val$field_id, rep(7081:7089, 2))
    expect_equivalent(val$class_id, rep(55L, 18))
    expect_equivalent(val$field_index, rep(1:9, 2))
    expect_equivalent(val$field_name,
       rep(
          c("Name", "Roughness", "Thickness", "Conductivity", "Density",
            "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
            "Visible Absorptance"),
         2
       )
    )
    expect_equivalent(val$rleid, rep(1L, 18))
    expect_equivalent(val$class_name, rep("Material", 18))
    expect_equivalent(val$object_name, rep(c("WD01", "WD02"), each = 9))
    # }}}
    # get values from class and ensure all objects have all fields {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, "Material", all = TRUE)})
    expect_equivalent(val$value_id, c(1:9, 40:43, -1:-5))
    expect_equivalent(val$object_id, rep(c(1L, 4L), each = 9))
    expect_equivalent(val$field_id, rep(7081:7089, 2))
    expect_equivalent(val$class_id, rep(55L, 18))
    expect_equivalent(val$field_index, rep(1:9, 2))
    expect_equivalent(val$field_name,
       rep(
          c("Name", "Roughness", "Thickness", "Conductivity", "Density",
            "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
            "Visible Absorptance"),
         2
       )
    )
    expect_equivalent(val$rleid, rep(1L, 18))
    expect_equivalent(val$class_name, rep("Material", 18))
    expect_equivalent(val$object_name, rep(c("WD01", "WD02"), each = 9))
    expect_equivalent(
        get_idf_value(idd_env, idf_env, "Material", all = TRUE),
        get_idf_value(idd_env, idf_env, "Material", all = TRUE, align = TRUE)
    )
    # }}}
    # }}}
    # get value from object {{{
    # get values from certain class {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, object = c("WD01", "WD02"))})
    expect_equivalent(val$value_id, c(1:9, 40:43))
    expect_equivalent(val$object_id, c(rep(1L, 9), rep(4L, 4)))
    expect_equivalent(val$field_id, c(7081:7089, 7081:7084))
    expect_equivalent(val$class_id, rep(55L, 13))
    expect_equivalent(val$field_index, c(1:9, 1:4))
    expect_equivalent(val$field_name,
       c(
          c("Name", "Roughness", "Thickness", "Conductivity", "Density",
            "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
            "Visible Absorptance"),
          c("Name", "Roughness", "Thickness", "Conductivity")
       )
    )
    expect_equivalent(val$rleid, c(rep(1L, 9), rep(2L, 4)))
    expect_equivalent(val$class_name, rep("Material", 13))
    expect_equivalent(val$object_name, c(rep("WD01", 9), rep("WD02", 4)))
    # }}}
    # get values from class but ensure all objects have same field {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, object = c("WD01", "WD02"), align = TRUE)})
    expect_equivalent(val$value_id, c(1:9, 40:43, -1:-5))
    expect_equivalent(val$object_id, rep(c(1L, 4L), each = 9))
    expect_equivalent(val$field_id, rep(7081:7089, 2))
    expect_equivalent(val$class_id, rep(55L, 18))
    expect_equivalent(val$field_index, rep(1:9, 2))
    expect_equivalent(val$field_name,
       rep(
          c("Name", "Roughness", "Thickness", "Conductivity", "Density",
            "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
            "Visible Absorptance"),
         2
       )
    )
    expect_equivalent(val$rleid, rep(c(1L, 2L), each = 9))
    expect_equivalent(val$class_name, rep("Material", 18))
    expect_equivalent(val$object_name, rep(c("WD01", "WD02"), each = 9))
    # }}}
    # get values from class and ensure all objects have min required fields {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, object = c("WD01", "WD02"), complete = TRUE)})
    expect_equivalent(val$value_id, c(1:9, 40:43, -1:-2))
    expect_equivalent(val$object_id, c(rep(1L, 9), rep(4L, 6)))
    expect_equivalent(val$field_id, c(7081:7089, 7081:7086))
    expect_equivalent(val$class_id, rep(55L, 15))
    expect_equivalent(val$field_index, c(1:9, 1:6))
    expect_equivalent(val$field_name,
       c(
          c("Name", "Roughness", "Thickness", "Conductivity", "Density",
            "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
            "Visible Absorptance"),
          c("Name", "Roughness", "Thickness", "Conductivity", "Density", "Specific Heat")
       )
    )
    expect_equivalent(val$rleid, c(rep(1L, 9), rep(2L, 6)))
    expect_equivalent(val$class_name, rep("Material", 15))
    expect_equivalent(val$object_name, c(rep("WD01", 9), rep("WD02", 6)))
    # }}}
    # get values from class and ensure all objects have min required fields and same field number {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, object = c("WD01", "WD02"), align = TRUE, complete = TRUE)})
    expect_equivalent(val$value_id, c(1:9, 40:43, -1:-5))
    expect_equivalent(val$object_id, rep(c(1L, 4L), each = 9))
    expect_equivalent(val$field_id, rep(7081:7089, 2))
    expect_equivalent(val$class_id, rep(55L, 18))
    expect_equivalent(val$field_index, rep(1:9, 2))
    expect_equivalent(val$field_name,
       rep(
          c("Name", "Roughness", "Thickness", "Conductivity", "Density",
            "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
            "Visible Absorptance"),
         2
       )
    )
    expect_equivalent(val$rleid, rep(c(1L, 2L), each = 9))
    expect_equivalent(val$class_name, rep("Material", 18))
    expect_equivalent(val$object_name, rep(c("WD01", "WD02"), each = 9))
    # }}}
    # get values from class and ensure all objects have all fields {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, object = c("WD01", "WD02"), all = TRUE)})
    expect_equivalent(val$value_id, c(1:9, 40:43, -1:-5))
    expect_equivalent(val$object_id, rep(c(1L, 4L), each = 9))
    expect_equivalent(val$field_id, rep(7081:7089, 2))
    expect_equivalent(val$class_id, rep(55L, 18))
    expect_equivalent(val$field_index, rep(1:9, 2))
    expect_equivalent(val$field_name,
       rep(
          c("Name", "Roughness", "Thickness", "Conductivity", "Density",
            "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
            "Visible Absorptance"),
         2
       )
    )
    expect_equivalent(val$rleid, rep(c(1L, 2L), each = 9))
    expect_equivalent(val$class_name, rep("Material", 18))
    expect_equivalent(val$object_name, rep(c("WD01", "WD02"), each = 9))
    expect_equivalent(
        get_idf_value(idd_env, idf_env, object = c("WD01", "WD02"), all = TRUE),
        get_idf_value(idd_env, idf_env, object = c("WD01", "WD02"), all = TRUE, align = TRUE)
    )
    # }}}
    # }}}
    # get value from field {{{
    # one class, multiple fields {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, "BuildingSurface:Detailed", field = 1:24)})
    expect_equivalent(val$value_id, c(15:38))
    expect_equivalent(val$object_id, rep(3L, 24))
    expect_equivalent(val$field_id, 11622:11645)
    expect_equivalent(val$class_id, rep(103L, 24))
    expect_equivalent(val$field_index, 1:24)
    expect_equivalent(val$rleid, rep(1L, 24))
    expect_equivalent(val$class_name, rep("BuildingSurface:Detailed", 24))
    expect_equivalent(val$object_name, rep("WALL-1PF", 24))
    # }}}
    # one field for each class {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, c("Material", "BuildingSurface:Detailed"), field = c(4, 9))})
    expect_equivalent(val$value_id, c(4L, 43L, 23L))
    expect_equivalent(val$object_id, c(1L, 4L, 3L))
    expect_equivalent(val$field_id, c(rep(7084L, 2), 11630))
    expect_equivalent(val$class_id, c(rep(55L, 2), 103L))
    expect_equivalent(val$field_index, c(rep(4L, 2), 9L))
    expect_equivalent(val$field_name, c(rep("Conductivity", 2), "View Factor to Ground"))
    expect_equivalent(val$rleid, c(1L, 1L, 2L))
    expect_equivalent(val$class_name, c(rep("Material", 2), "BuildingSurface:Detailed"))
    expect_equivalent(val$object_name, c("WD01", "WD02", "WALL-1PF"))
    # }}}
    expect_silent({val <- get_idf_value(idd_env, idf_env, object = c("WD01", "WD02"), field = c(4, 9), complete = TRUE)})
    expect_silent({val <- get_idf_value(idd_env, idf_env, c("Material", "BuildingSurface:Detailed"), field = c(4, 9), complete = TRUE)})
    # }}}

    # misc
    expect_error(get_idf_value(idd_env, idf_env, 10000), "Invalid class index")
    expect_error(get_idf_value(idd_env, idf_env, ""), "Invalid class name")
    expect_error(get_idf_value(idd_env, idf_env, object = 10000), "Invalid object ID")
    expect_error(get_idf_value(idd_env, idf_env, object = ""), "Invalid object name")
    expect_error(get_idf_value(idd_env, idf_env, "Version", field = 2L), "Invalid field index")
    expect_error(get_idf_value(idd_env, idf_env, "Version", field = "Version"), "Invalid field name")

    expect_equal(get_idf_value(idd_env, idf_env, "Version")$value_id, 44L)
    expect_equal(get_idf_value(idd_env, idf_env, "Version", field = 1L)$value_id, 44L)
    expect_equal(get_idf_value(idd_env, idf_env, "Version", field = "Version Identifier")$value_id, 44L)
    expect_equal(get_idf_value(idd_env, idf_env, "Material")$value_id, c(1L:9L, 40L:43L))
    fld_nm <- c("Conductivity", "Visible Absorptance")
    expect_equal(get_idf_value(idd_env, idf_env, "Material", field = c(4L, 9L))$value_id, c(4L, 9L, 43L))
    expect_equal(get_idf_value(idd_env, idf_env, "Material", field = fld_nm)$value_id, c(4L, 9L, 43L))
    expect_equal(get_idf_value(idd_env, idf_env, "Material", field = c(4L, 9L), align = TRUE)$value_id, c(4L, 43L))
    expect_equal(get_idf_value(idd_env, idf_env, "Material", field = fld_nm, align = TRUE)$value_id, c(4L, 43L))
    expect_equal(get_idf_value(idd_env, idf_env, "Material", field = c(4L, 3L), complete = TRUE)$value_id, c(1:6, 40:43, -1:-2))
    fld_nm <- c("Layer 3", "Visible Absorptance")
    expect_equal(get_idf_value(idd_env, idf_env, c("Construction", "Material"), field = c(4L, 9L))$value_id, c(13L, 9L))
    expect_equal(get_idf_value(idd_env, idf_env, c("Construction", "Material"), field = fld_nm)$value_id, c(13L, 9L))
    expect_equal(get_idf_value(idd_env, idf_env, c("Construction", "Material"),
            field = c(4L, 9L), align = TRUE)$value_id, c(13L, 9L, -1L)
    )
    expect_equal(get_idf_value(idd_env, idf_env, c("Construction", "Material"),
            field = fld_nm, align = TRUE)$value_id, c(13L, 9L, -1L)
    )
    # }}}

    # RELATION {{{
    expect_equal(get_idf_relation(idd_env, idf_env),
        data.table(object_id = 2:3, value_id = c(11L, 17L), src_object_id = 1:2,
            src_value_id = c(1L, 10L), src_enum = 2L, dep = 0L
        )
    )
    # }}}
})
# }}}

# NAME DOTS {{{
test_that("NAME DOTS", {
    expect_error(sep_name_dots(), "Please give object\\(s\\) to modify")
    expect_error(sep_name_dots(NULL), "must be a character vector or a positive integer")
    expect_error(sep_name_dots(list()), "must be a character vector or a positive integer")
    expect_error(sep_name_dots(NA), "must be a character vector or a positive integer")
    expect_error(sep_name_dots(NA_character_), "must be a character vector or a positive integer")
    expect_error(sep_name_dots(TRUE), "must be a character vector or a positive integer")
    expect_error(sep_name_dots(NaN), "must be a character vector or a positive integer")
    expect_error(sep_name_dots(Inf), "must be a character vector or a positive integer")
    expect_error(sep_name_dots(0), "must be a character vector or a positive integer")
    expect_error(sep_name_dots(list(0)), "must be a character vector or a positive integer")
    expect_warning({
        x <- c("e", "f"); y <- c(5L, 6L); z <- c(z = "g")
        nm <- sep_name_dots(1:2, c("a", "b"), c = 1, d = "z", x, y, z = z)
    })
    expect_equal(nm$id,
        data.table(
            rleid = c(1L, 1L, 3L, 6L, 6L),
            object_rleid = c(1L, 2L, 1L, 1L, 2L),
            object_id = c(1L, 2L, 1L, 5L, 6L),
            new_object_name = c(rep(NA_character_, 2L), "c", rep(NA_character_, 2L))
        )
    )
    expect_equal(nm$name,
        data.table(
            rleid = c(2L, 2L, 4L, 5L, 5L, 7L),
            object_rleid = c(1L, 2L, 1L, 1L, 2L, 1L),
            object_name = c("a", "b", "z", "e", "f", "g"),
            new_object_name = c(rep(NA_character_, 2L), "d", rep(NA_character_, 2L), "z")
        )
    )
    expect_equal(nm$dot, data.table(rleid = 1L:7L,
            dot = list(c(1L, 2L), c("a", "b"), 1L, "z", c("e", "f"), c(5L, 6L), c(z = "g")),
            dot_nm = c(rep(NA_character_, 2L), "c", "d", rep(NA_character_, 2L), "z"),
            type = c(1L, 2L, 1L, 2L, 2L, 1L, 2L)
    ))
})
# }}}

# VALUE DOTS {{{
test_that("VALUE DOTS", {
    expect_error(sep_value_dots(NULL), "must be an empty list or a list where")
    expect_error(sep_value_dots(list()), "must be an empty list or a list where")
    expect_error(sep_value_dots(1), "must be an empty list or a list where")
    expect_error(sep_value_dots("a"), "must be an empty list or a list where")
    expect_error(sep_value_dots(NA), "must be an empty list or a list where")
    expect_error(sep_value_dots(NA_character_), "must be an empty list or a list where")
    expect_error(sep_value_dots(NA_integer_), "must be an empty list or a list where")
    expect_error(sep_value_dots(character()), "must be an empty list or a list where")
    expect_error(sep_value_dots(integer()), "must be an empty list or a list where")
    expect_error(sep_value_dots(double()), "must be an empty list or a list where")
    expect_error(sep_value_dots(logical()), "must be an empty list or a list where")
    expect_error(sep_value_dots(cls = NULL), "must be an empty list or a list where")
    expect_error(sep_value_dots(cls = "a"), "must be an empty list or a list where")
    expect_error(sep_value_dots(cls = NA_integer_), "must be an empty list or a list where")

    # can change empty string to NA
    expect_equal(sep_value_dots(cls = list("", "  ", " "))$value$value_chr, rep(NA_character_, 3L))

    # missing class name
    expect_error(sep_value_dots(list(NULL)), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(NULL, NULL)), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(NULL, NA)), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(NULL, c(NA, NA, NA))), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(NULL, 1)), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(NULL, c(1, 2))), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(NULL, "a")), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(NULL, NA_character_)), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(NULL, NA_integer_)), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(NA)), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(NA, "a")), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(list())), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(list(NULL))), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(list(NA))), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(list("a"))), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(list(NULL, NULL), list()), .empty = TRUE), "must be an empty list or a list where")
    expect_error(sep_value_dots(cls = list(NULL, NULL, list()), .empty = TRUE), "must be an empty list or a list where")

    # invalid list format
    expect_error(sep_value_dots(cls = list(list("a")), "must be an empty list or a list where"))
    expect_error(sep_value_dots(cls = list(1, list())), "must be an empty list or a list where")
    expect_error(sep_value_dots(cls = list(list())), "must be an empty list or a list where")
    expect_error(sep_value_dots(cls = list(list(NULL, NULL))), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(cls = list(list()))), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(cls = list(list(NULL, NULL)))), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(cls = list(1, list()))), "must be an empty list or a list where")
    expect_error(sep_value_dots(cls = list(1, list(NULL))), "must be an empty list or a list where")
    expect_error(sep_value_dots(cls = list(1, list("a"))), "must be an empty list or a list where")
    expect_error(sep_value_dots(list("Material" = list(), Construction = NULL)))
    expect_error(sep_value_dots(list("Material" = list(), NULL)))

    # contains NA
    expect_error(sep_value_dots(cls = list(NA)), "must be an empty list or a list where")
    expect_error(sep_value_dots(cls = list(NULL, NA)), "must be an empty list or a list where")
    expect_error(sep_value_dots(list(cls = list(NULL, NA))), "must be an empty list or a list where")

    # multiple .comment
    expect_error(sep_value_dots(cls = list(.comment = c("a"), .comment = NULL)), "only have one `.comment`")
    expect_error(sep_value_dots(list(cls = list(.comment = c("a"), .comment = NULL))), "only have one `.comment`")

    # duplicated field names
    expect_error(sep_value_dots(cls = list(Name = "const", Name = "const1")), "names must be unique")
    expect_error(sep_value_dots(list(cls = list(Name = "const", Name = "const1"))), "names must be unique")

    # empty objects
    expect_error(sep_value_dots(cls = list(), .empty = FALSE), "Empty input found")
    expect_error(sep_value_dots(list(cls = list()), .empty = FALSE), "Empty input found")
    expect_silent({l <- sep_value_dots(
        cls = list(), cls = list(NULL), list(cls = list(), cls = list(NULL)),
        .empty = TRUE)
    })
    expect_equivalent(l$object,
        data.table(rleid = c(1L, 2L, 3L, 3L), object_rleid = c(1L, 1L, 1L, 2L),
            name = rep("cls", 4L), comment = list(rep(NULL, 4L)),
            empty = rep(c(TRUE, FALSE), 2L)
        )
    )
    expect_equivalent(l$value,
        data.table(rleid = c(2L, 3L), object_rleid = c(1L, 2L),
            field_name = rep(NA_character_, 2L), value = rep(NA_character_, 2L),
            value_num = rep(NA_real_, 2L), defaulted = rep(TRUE, 2L)
        )
    )

    # comment-only object
    expect_silent({l <- sep_value_dots(cls = list(.comment = c("this is", "a comment")))})
    expect_equal(nrow(l$value), 0L)
    expect_equal(l$object, data.table(rleid = 1L, object_rleid = 1L, name = "cls",
            empty = TRUE, comment = list(c("this is", "a comment"))))

    # normal objects
    expect_silent(
        l <- sep_value_dots(
            # empty
            cls1 = list(),
            cls2 = list(.comment = c("a", "b")),
            cls3 = list(NULL, NULL, fld1 = NULL, .comment = c("a", "b")),
            cls4 = list(NULL, fld1 = "a", fld2 = 2L, fld3 = NULL, "a", 1L, .comment = c("a", "b")),
            list(cls5 = list(.comment = c("a", "b"))),
            list(cls6 = list(NULL, NULL, fld1 = NULL, .comment = c("a", "b"))),
            list(cls7 = list(fld1 = NULL, fld2 = "a", NULL, 2L, fld3 = NULL, .comment = c("a", "b"))),
            .empty = TRUE
        )
    )
    expect_equivalent(l$object,
        data.table(rleid = 1L:7L,
            object_rleid = rep(1L, 7L),
            name = paste0("cls", 1L:7L),
            comment = c(list(NULL), rep(list(c("a", "b")), 6L)),
            empty = c(rep(TRUE, 2L), rep(FALSE, 2L), TRUE, rep(FALSE, 2L))
        )
    )
    expect_equivalent(l$value,
        data.table(rleid = c(rep(3L, 3L), rep(4L, 6L), rep(6L, 3L), rep(7L, 5L)),
            object_rleid = rep(1L, 17L),
            field_name = c(rep(NA_character_, 2L), "fld1", NA_character_,
                paste0("fld", 1L:3L), rep(NA_character_, 4L), paste0("fld", c(1, 1, 2)),
                rep(NA_character_, 2L), "fld3"),
            value = c(rep(NA_character_, 4L), "a", "2", NA_character_, "a", "1",
                rep(NA_character_, 4L), "a", NA_character_, "2", NA_character_
            ),
            value_num = c(rep(NA_real_, 5L), 2, rep(NA_real_, 2L),
                1, rep(NA_real_, 6L), 2, NA_real_
            ),
            defaulted = c(rep(TRUE, 4L), FALSE, FALSE, TRUE, FALSE, FALSE,
                rep(TRUE, 4L), FALSE, TRUE, FALSE, TRUE
            )
        )
    )
})
# }}}

# DUP {{{
test_that("Dup", {
    # read idf
    idf <- read_idf(example(), 8.8)
    idf_env <- ._get_private(idf)$m_idf_env
    idd_env <- ._get_private(idf)$idd_env()

    expect_error(dup_idf_object(idd_env, idf_env), "Please give object\\(s\\) to modify")
    expect_error(dup_idf_object(idd_env, idf_env, 1), "Duplicating Version object")
    # unique object: SimulationControl
    expect_error(dup_idf_object(idd_env, idf_env, 7), "Existing unique object")

    expect_error(dup_idf_object(idd_env, idf_env, shit = 7, fuck = 7), "Existing unique object")

    expect_silent({dup <- dup_idf_object(idd_env, idf_env, `NewFloor` = "FLOOR")})
    expect_equivalent(dup$object, data.table(object_id = 54L, class_id = 90L,
        comment = list(), object_name = "NewFloor", object_name_lower = "newfloor"))
    expect_equivalent(dup$value, data.table(value_id = c(349L, 350L),
        value_chr = c("NewFloor", "C5 - 4 IN HW CONCRETE"),
        value_num = c(NA_real_, NA_real_), object_id = c(54L, 54L),
        field_id = c(11006L, 11007L)))
    expect_equal(dup$reference[.N], data.table(object_id = 54L, value_id = 350L,
        src_object_id = 14L, src_value_id = 99L, src_enum = 2L))

    expect_error(dup_idf_object(idd_env, idf_env, FLOOR = "FLOOR"), fixed = TRUE,
    "Input new name(s) cannot be the same as target object(s)")
    expect_silent({dup <- dup_idf_object(idd_env, idf_env, rep("FLOOR", 10))})
    expect_equal(dup$object$object_name, paste0("FLOOR_", 1:10))
})
# }}}

# ADD {{{
test_that("Add", {
    # read idf
    idf <- read_idf(example(), 8.8)
    idf_env <- ._get_private(idf)$m_idf_env
    idd_env <- ._get_private(idf)$idd_env()

    expect_error(add_idf_object(idd_env, idf_env), "Please give object\\(s\\) to add")
    expect_error(add_idf_object(idd_env, idf_env, Version = list(8.8)), "Adding Version object")
    expect_error(add_idf_object(idd_env, idf_env,
        SimulationControl = list()), "Adding new object in existing unique class")
    expect_silent({rp <- add_idf_object(idd_env, idf_env,
        RunPeriod = list("Test1", 1, 1, End_Month = 2, 1, "Monday", Apply_Weekend_Holiday_Rule = "No")
    )})
    expect_equivalent(rp$object, data.table(object_id = 54L, class_id = 22L,
        comment = list(), object_name = "Test1", object_name_lower = "test1"
    ))
    expect_equivalent(rp$value, data.table(value_id = 349L:359L,
        value_chr = c("Test1", "1", "1", "2", "1", "Monday", "Yes", "Yes", "No", "Yes", "Yes"),
        value_num = c(NA_real_, 1, 1, 2, 1, rep(NA_real_, 6L)),
        object_id = rep(54L, 11L), field_id = 104L:114L
    ))

    expect_silent(rp <- add_idf_object(idd_env, idf_env,
        RunPeriod = list("Test2", 1, 1, 2, 1), .default = TRUE, .all = TRUE)
    )
    expect_equivalent(rp$object, data.table(object_id = 54L, class_id = 22L,
        comment = list(), object_name = "Test2", object_name_lower = "test2"
    ))
    expect_equivalent(rp$value, data.table(value_id = 349L:362L,
        value = c("Test2", "1", "1", "2", "1", "UseWeatherFile", "Yes", "Yes", "No", "Yes", "Yes", "1", "Yes", NA_character_),
        value_num = c(NA_real_, 1, 1, 2, 1, rep(NA_real_, 6L), 1, rep(NA_real_, 2L)),
        object_id = rep(54L, 14L), field_id = 104L:117L
    ))

    expect_silent(const <- add_idf_object(idd_env, idf_env, Construction = list("TestConst", "R13LAYER")))
    expect_equal(const$reference[object_id == 54L],
        data.table(object_id = 54L, value_id = 350L, src_object_id = 12L, src_value_id = 87L, src_enum = 2L)
    )
})
# }}}

# SET {{{
test_that("Set", {
    # read idf
    idf <- read_idf(example(), 8.8)
    idf_env <- ._get_private(idf)$m_idf_env
    idd_env <- ._get_private(idf)$idd_env()

    expect_error(set_idf_object(idd_env, idf_env), "Please give object\\(s\\) to add")
    expect_error(set_idf_object(idd_env, idf_env, ..1 = list(8.8)), "Modifying Version object")
    expect_silent({rp <- set_idf_object(idd_env, idf_env, ..8 = list(Name = "Test"))})
    expect_equal(rp$object, data.table(object_id = 8L, class_id = 22L,
            comment = list(NULL), object_name = "Test", object_name_lower = "test")
    )
    expect_equivalent(rp$value,
        data.table(
            value_id = 19:29,
            value_chr = c("Test", "1", "1", "12", "31", "Tuesday", "Yes", "Yes", "No", "Yes", "Yes"),
            value_num = c(NA_real_, 1, 1, 12, 31, rep(NA_real_, 6)),
            object_id = rep(8L, 11),
            field_id = 104:114)
    )

    expect_silent({floor <- set_idf_object(idd_env, idf_env, FLOOR = list(Name = "Flr"))})
    expect_equal(floor$object$object_name, "Flr")
    expect_equal(idf_env$value[
        value_id == floor$reference[src_object_id == floor$object$object_id, value_id],
        value_chr], "Flr"
    )

    # delete fields
    expect_equal(nrow(set_idf_object(idd_env, idf_env,
        ..8 = list(name = "name", start_year = NULL), .default = FALSE)$value),
        11L)
})
# }}}

# DEL {{{
test_that("Del", {
    # read idf
    idf <- read_idf(example(), 8.8)
    idf_env <- ._get_private(idf)$m_idf_env
    idd_env <- ._get_private(idf)$idd_env()

    expect_error(del_idf_object(idd_env, idf_env), "Please give object\\(s\\) to modify")
    expect_error(del_idf_object(idd_env, idf_env, 1), "Deleting Version object")
    expect_error(del_idf_object(idd_env, idf_env, c(2, 2)), "Cannot modify same object multiple times")
    expect_error(del_idf_object(idd_env, idf_env, 3), "Deleting a required object")
    expect_error(
        del_idf_object(idd_env, idf_env, "R13WALL", "FLOOR", "ROOF31"),
        "Cannot delete object\\(s\\) that are referred by others"
    )
    expect_error(
        del_idf_object(idd_env, idf_env, 21:26, 14, .referenced = TRUE, .recursive = TRUE, .force = FALSE),
        "Cannot delete object\\(s\\) that are referred by others"
    )
    expect_silent({del <- del_idf_object(idd_env, idf_env, 21:26, 14, .referenced = TRUE, .recursive = TRUE, .force = TRUE)})
    expect_equivalent(setdiff(idf_env$object$object_id, del$object$object_id), c(14L, 16L, 21:26))
})
# }}}

# RENAME {{{
test_that("Rename", {
    # read idf
    idf <- read_idf(example(), 8.8)
    idf_env <- ._get_private(idf)$m_idf_env
    idd_env <- ._get_private(idf)$idd_env()

    expect_error(rename_idf_object(idd_env, idf_env), "Please give object\\(s\\) to modify")
    expect_error(rename_idf_object(idd_env, idf_env, 1), "Modifying Version object")
    expect_error(rename_idf_object(idd_env, idf_env, c(2, 2)), "Cannot modify same object multiple times")
    expect_error(rename_idf_object(idd_env, idf_env, 3), "Please give new object names")
    expect_error(
        rename_idf_object(idd_env, idf_env, "R13WALL", "FLOOR", "ROOF31"),
        "Please give new object names"
    )
    expect_silent(ren <- rename_idf_object(idd_env, idf_env,
        r13 = "R13WALL", flr = "FLOOR", roof = "ROOF31", r31 = "R31LAYER")
    )
    expect_equal(ren$object$object_name, c("r13", "flr", "roof", "r31"))
    expect_equal(ren$value$value_chr, c("r13", "flr", "roof", "r31"))
    expect_equal(get_idf_value(idd_env, idf_env, object = 21, field = "Construction Name")$value_chr, "r13")
    expect_equal(get_idf_value(idd_env, idf_env, object = 22, field = "Construction Name")$value_chr, "r13")
    expect_equal(get_idf_value(idd_env, idf_env, object = 23, field = "Construction Name")$value_chr, "r13")
    expect_equal(get_idf_value(idd_env, idf_env, object = 24, field = "Construction Name")$value_chr, "r13")
    expect_equal(get_idf_value(idd_env, idf_env, object = 25, field = "Construction Name")$value_chr, "flr")
    expect_equal(get_idf_value(idd_env, idf_env, object = 26, field = "Construction Name")$value_chr, "roof")
    expect_equal(get_idf_value(idd_env, idf_env, object = 17, field = "Outside Layer")$value_chr, "r31")
})
# }}}

# INSERT {{{
test_that("Insert", {
    # read idf
    idf <- read_idf(example(), 8.8)
    idf_env <- ._get_private(idf)$m_idf_env
    idd_env <- ._get_private(idf)$idd_env()

    expect_error(insert_idf_object(idd_env, idf_env), "Please give object\\(s\\) to insert")
    expect_error(insert_idf_object(idd_env, idf_env, version = idf$version(), 1),
        "must be an IdfObject or a list of IdfObjects"
    )
    expect_error(
        insert_idf_object(idd_env, idf_env, version = idf$version(), my_building = idf$Building),
        "Inserting new object in existing unique class"
    )
    expect_error(
        insert_idf_object(idd_env, idf_env, version = idf$version(), idf$Version),
        "Inserting Version object"
    )
    expect_error(
        insert_idf_object(idd_env, idf_env, version = numeric_version("8.7.0"), idf$Material),
        "should be IdfObjects with version `8.7.0`"
    )
    expect_error(
        insert_idf_object(idd_env, idf_env, version = idf$version(), idf$Material),
        "Conflicted Object Names"
    )
    expect_silent(
        ins <- insert_idf_object(idd_env, idf_env, version = idf$version(), new_mat = idf$Material[[1L]])
    )

    expect_equivalent(ins$object,
        data.table(object_id = 54L, class_id = 55L, comment = list(),
            object_name = "new_mat", object_name_lower = "new_mat"
        )
    )
    expect_equivalent(ins$value$value_id, 349:357)
    expect_equivalent(ins$value$value_chr[[1L]], "new_mat")
    expect_equivalent(ins$value$object_id, rep(54L, 9))
    expect_equivalent(ins$value$field_id, 7081:7089)
})
# }}}

# SAVE {{{
test_that("Save", {
    # read idf
    idf <- read_idf(example(), 8.8)
    idf_env <- ._get_private(idf)$m_idf_env
    idd_env <- ._get_private(idf)$idd_env()

    expect_silent(
        save_idf(idd_env, idf_env, idf_env$object[, list(object_id, object_order = 0)],
            tempfile(fileext = ".idf"), format = "sorted"
        )
    )
    expect_silent(
        save_idf(idd_env, idf_env, idf_env$object[, list(object_id, object_order = 0)],
            tempfile(fileext = ".idf"), format = "new_top"
        )
    )
    expect_silent(
        save_idf(idd_env, idf_env, idf_env$object[, list(object_id, object_order = 0)],
            tempfile(fileext = ".idf"), format = "new_bot"
        )
    )
})
# }}}
