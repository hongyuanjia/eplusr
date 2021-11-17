eplusr_option(validate_level = "final", verbose_info = FALSE)
use_idd(8.8, "auto")

# TABLE {{{
test_that("table", {
    idf_env <- parse_idf_file(idftext("idf", 8.8))
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    # OBJECT {{{
    expect_equal(get_idf_object(idd_env, idf_env, 1),
        data.table(rleid = 1L, class_id = 1L, class_name = "Version",
            object_id = 5L, object_name = NA_character_,
            object_name_lower = NA_character_, comment = list()
        )
    )
    expect_equal(get_idf_object(idd_env, idf_env, "Version"),
        data.table(rleid = 1L, class_id = 1L, class_name = "Version",
            object_id = 5L, object_name = NA_character_,
            object_name_lower = NA_character_, comment = list()
        )
    )
    expect_equal(get_idf_object(idd_env, idf_env, "Version", 5),
        data.table(rleid = 1L, class_id = 1L, class_name = "Version",
            object_id = 5L, object_name = NA_character_,
            object_name_lower = NA_character_, comment = list()
        )
    )
    expect_equal(get_idf_object(idd_env, idf_env, "Version", 5, c("num_fields")),
        data.table(rleid = 1L, class_id = 1L, class_name = "Version",
            object_id = 5L, object_name = NA_character_,
            object_name_lower = NA_character_, comment = list(), num_fields = 1L
        )
    )
    expect_equal(get_idf_object(idd_env, idf_env), setcolorder(add_rleid(add_class_name(idd_env, copy(idf_env$object))), c("rleid", "class_id", "class_name")))
    expect_equal(get_idf_object(idd_env, idf_env, property = "has_name"),
        setcolorder(add_rleid(add_class_property(idd_env, add_class_name(idd_env, copy(idf_env$object)), "has_name")),
            c("rleid", "class_id", "class_name")
        )
    )
    expect_equal(get_idf_object(idd_env, idf_env, 55, property = "has_name")$has_name, c(TRUE, TRUE))
    expect_equal(get_idf_object(idd_env, idf_env, 55)$object_id, c(1L, 4L))
    expect_equal(get_idf_object(idd_env, idf_env, 55, c("WD02", "WD01"))$object_id, c(4L, 1L))
    expect_equal(get_idf_object(idd_env, idf_env, "Material")$object_id, c(1L, 4L))
    expect_equal(get_idf_object(idd_env, idf_env, "Material", c("WD02", "WD01"))$object_id, c(4L, 1L))
    expect_error(get_idf_object(idd_env, idf_env, 2), class = "eplusr_error_invalid_class_index")
    expect_error(get_idf_object(idd_env, idf_env, "Branch"), class = "eplusr_error_invalid_class_name")
    expect_error(get_idf_object(idd_env, idf_env, "Material", "wrong"), class = "eplusr_error_invalid_object_name")
    expect_error(get_idf_object(idd_env, idf_env, "Material", 15), class = "eplusr_error_invalid_object_id")
    expect_equal(get_idf_object(idd_env, idf_env, 55, c("wd02", "wd01"), ignore_case = TRUE)$object_id, c(4L, 1L))

    expect_equal(get_idf_object_multi_scope(idd_env, idf_env)$object_id, 1:5)
    expect_equal(get_idf_object_multi_scope(idd_env, idf_env, 1, "Construction", "Thermal Zones and Surfaces"),
        data.table(rleid = 1:3, class_id = c(55L, 90L, 103L),
            class_name = c("Material", "Construction", "BuildingSurface:Detailed"),
            object_id = 1:3,
            object_name = c("WD01", "WALL-1", "WALL-1PF"),
            object_name_lower = c("wd01", "wall-1", "wall-1pf"),
            comment = list(" this is a test comment for WD01", NULL, NULL)
        )
    )

    # can stop if same names found in input class
    idf_env1 <- idf_env
    idf_env1$object <- rbindlist(list(idf_env1$object, idf_env1$object[1][, object_id := 6L]))
    expect_error(get_idf_object(idd_env, idf_env1, object = "WD01"), class = "eplusr_error_multi_match_by_name")

    expect_error(get_idf_object_id(idd_env, idf_env, 10000), class = "eplusr_error_invalid_class_index")
    expect_error(get_idf_object_id(idd_env, idf_env, "Branch"), class = "eplusr_error_invalid_class_name")
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

    expect_equal(get_idf_object_num(idd_env, idf_env), 5L)
    expect_equal(get_idf_object_num(idd_env, idf_env, c(55, 55, 100)), c(2L, 2L, 0L))
    expect_error(get_idf_object_num(idd_env, idf_env, c(55, 55, 10000)), class = "eplusr_error_invalid_class_index")
    expect_equal(get_idf_object_num(idd_env, idf_env, c("Material", "Material")), c(2L, 2L))
    expect_equal(get_idf_object_num(idd_env, idf_env, c("Material", "Material", "Branch")), c(2L, 2L, 0L))
    expect_equal(get_idf_object_num(idd_env, idf_env, c("Version", "Material")), c(1L, 2L))

    expect_equal(get_idf_object_name(idd_env, idf_env, "Material"), list(Material = c("WD01", "WD02")))
    expect_equal(get_idf_object_name(idd_env, idf_env, 55), list(Material = c("WD01", "WD02")))
    expect_equal(get_idf_object_name(idd_env, idf_env, 55, simplify = TRUE), c("WD01", "WD02"))
    expect_equal(get_idf_object_name(idd_env, idf_env, "Material", simplify = TRUE), c("WD01", "WD02"))
    expect_equal(get_idf_object_name(idd_env, idf_env, c("Version", "Material")), list(Version = NA_character_, Material = c("WD01", "WD02")))
    expect_equal(get_idf_object_name(idd_env, idf_env, c("Version", "Material"), simplify = TRUE), c(NA_character_, c("WD01", "WD02")))

    expect_equal(get_idf_object_id(idd_env, idf_env, 1), list(Version = 5L))
    expect_equal(get_idf_object_id(idd_env, idf_env, "Version"), list(Version = 5L))
    expect_equal(get_idf_object_id(idd_env, idf_env, 1, simplify = TRUE), 5L)
    expect_equal(get_idf_object_id(idd_env, idf_env, "Version", simplify = TRUE), 5L)

    expect_equal(get_object_info(add_class_name(idd_env, idf_env$object[1])), " #1| Object ID [1] (name 'WD01') in class 'Material'")
    expect_equal(get_object_info(add_class_name(idd_env, idf_env$object[5])), " #1| Object ID [5] in class 'Version'")
    expect_equal(get_object_info(add_class_name(idd_env, idf_env$object[1]), "class"), " #1| Class 'Material'")
    expect_equal(get_object_info(add_class_name(idd_env, idf_env$object[c(1, 4)]), c("id", "class"), by_class = TRUE), " #1| Object ID [1] and ID [4] in class 'Material'")
    expect_equal(get_object_info(add_class_name(idd_env, idf_env$object[c(1, 4)][, rleid := 5:6]), "class", by_class = TRUE), sprintf(" #%i| Class 'Material'", 5:6))
    expect_equal(get_object_info(idf_env$object[1], c("id", "name")), " #1| Object ID [1] (name 'WD01')")
    expect_equal(get_object_info(idf_env$object[1], c("name", "id")), " #1| Object name 'WD01'(ID [1])")
    expect_equal(get_object_info(idf_env$object[1], c("name")), " #1| Object name 'WD01'")
    expect_equal(get_object_info(idf_env$object[1], c("name"), name_prefix = FALSE), " #1| Object 'WD01'")

    # can init object table
    expect_equal(init_idf_object(idd_env, idf_env, c("Version", rep("Material", 2))),
        data.table(rleid = 1:3, class_id = c(1L, 55L, 55L),
            class_name = c("Version", "Material", "Material"),
            group_id = c(1L, 5L, 5L), object_id = 6:8,
            object_name = c(NA_character_, "Material", "Material 1"),
            object_name_lower = c(NA_character_, "material", "material 1"),
            comment = list()
        )
    )
    expect_equal(init_idf_object(idd_env, NULL, "Material", name = FALSE),
        data.table(rleid = 1L, class_id = 55L,
            class_name = "Material", group_id = 5L, object_id = 1L,
            object_name = NA_character_, object_name_lower = NA_character_,
            comment = list()
        )
    )
    # }}}

    # VALUE {{{
    # get all value from current idf {{{
    expect_equivalent(nrow(get_idf_value(idd_env, idf_env)), 46L)
    expect_equivalent(names(get_idf_value(idd_env, idf_env)),
        c("rleid", "class_id", "class_name", "object_id", "object_name",
        "field_id", "field_index", "field_name", "value_id", "value_chr", "value_num"
        )
    )
    # }}}
    # get value from class {{{
    # get values from certain class {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, "Material")})
    expect_equivalent(val$value_id, c(1:9, 40:43, 45:46))
    expect_equivalent(val$object_id, c(rep(1L, 9), rep(4L, 6)))
    expect_equivalent(val$field_id, c(7081:7089, 7081:7086))
    expect_equivalent(val$class_id, rep(55L, 15))
    expect_equivalent(val$field_index, c(1:9, 1:6))
    expect_equivalent(val$field_name,
       c(
          c("Name", "Roughness", "Thickness", "Conductivity", "Density",
            "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
            "Visible Absorptance"),
          c("Name", "Roughness", "Thickness", "Conductivity", "Density",
              "Specific Heat"
          )
       )
    )
    expect_equivalent(val$rleid, rep(1L, 15))
    expect_equivalent(val$class_name, rep("Material", 15))
    expect_equivalent(val$object_name, c(rep("WD01", 9), rep("WD02", 6)))
    # }}}
    # get values from class but ensure all objects have same field {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, "Material", align = TRUE)})
    expect_equivalent(val$value_id, c(1:9, 40:43, 45:46, -1:-3))
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
    expect_equivalent(val$value_id, c(1:9, 40:43, 45:46))
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
    expect_equivalent(val$value_id, c(1:9, 40:43, 45:46, -1:-3))
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
    expect_equivalent(val$value_id, c(1:9, 40:43, 45:46, -1:-3))
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
    expect_equivalent(val$value_id, c(1:9, 40:43, 45:46))
    expect_equivalent(val$object_id, c(rep(1L, 9), rep(4L, 6)))
    expect_equivalent(val$field_id, c(7081:7089, 7081:7086))
    expect_equivalent(val$class_id, rep(55L, 15))
    expect_equivalent(val$field_index, c(1:9, 1:6))
    expect_equivalent(val$field_name,
       c(
          c("Name", "Roughness", "Thickness", "Conductivity", "Density",
            "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
            "Visible Absorptance"),
          c("Name", "Roughness", "Thickness", "Conductivity", "Density",
            "Specific Heat")
       )
    )
    expect_equivalent(val$rleid, c(rep(1L, 9), rep(2L, 6)))
    expect_equivalent(val$class_name, rep("Material", 15))
    expect_equivalent(val$object_name, c(rep("WD01", 9), rep("WD02", 6)))
    # }}}
    # get values from class but ensure all objects have same field {{{
    expect_silent({val <- get_idf_value(idd_env, idf_env, object = c("WD01", "WD02"), align = TRUE)})
    expect_equivalent(val$value_id, c(1:9, 40:43, 45:46, -1:-3))
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
    expect_equivalent(val$value_id, c(1:9, 40:43, 45:46))
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
    expect_equivalent(val$value_id, c(1:9, 40:43, 45:46, -1:-3))
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
    expect_equivalent(val$value_id, c(1:9, 40:43, 45:46, -1:-3))
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
    expect_equal(nrow(get_idf_value(idd_env, idf_env, "Material", field = c(8, 9), align = TRUE)), 4L)
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
    expect_equal(nrow(get_idf_value(idd_env, idf_env, c("Material", "BuildingSurface:Detailed"), field = c(9, 24), align = TRUE)), 3)
    # }}}
    expect_equal(nrow(get_idf_value(idd_env, idf_env, object = c("WD01", "WD02"), field = c(4, 9), complete = TRUE)), 15)
    expect_equal(nrow(get_idf_value(idd_env, idf_env, c("Material", "BuildingSurface:Detailed"), field = c(4, 9), complete = TRUE)), 31)
    expect_equal(nrow(get_idf_value(idd_env, idf_env, object = c("WD01", "WD02"), field = c(4, 9), align = TRUE)), 2)
    expect_equal(nrow(get_idf_value(idd_env, idf_env, object = c("WD02"), field = c(4, 9), align = TRUE)), 2)
    expect_equal(nrow(get_idf_value(idd_env, idf_env, c("BuildingSurface:Detailed"), field = c(4, 9), align = TRUE)), 2)
    # }}}

    # misc
    expect_error(get_idf_value(idd_env, idf_env, 10000), class = "eplusr_error_invalid_class_index")
    expect_error(get_idf_value(idd_env, idf_env, ""), class = "eplusr_error_invalid_class_name")
    expect_error(get_idf_value(idd_env, idf_env, object = 10000), class = "eplusr_error_invalid_object_id")
    expect_error(get_idf_value(idd_env, idf_env, object = ""), class = "eplusr_error_invalid_object_name")
    expect_error(get_idf_value(idd_env, idf_env, "Version", field = 2L), class = "eplusr_error_invalid_field_index")
    expect_error(get_idf_value(idd_env, idf_env, "Version", field = "Version"), class = "eplusr_error_invalid_field_name")
    expect_error(get_idf_value(idd_env, idf_env, field = "Version"), class = "eplusr_error_missing_class_or_object")
    expect_error(get_idf_value(idd_env, idf_env, c("Material", "Construction"), field = 1), class = "eplusr_error_invalid_field_length")

    expect_equal(get_idf_value(idd_env, idf_env, "Version")$value_id, 44L)
    expect_equal(get_idf_value(idd_env, idf_env, "Version", field = 1L)$value_id, 44L)
    expect_equal(get_idf_value(idd_env, idf_env, "Version", field = "Version Identifier")$value_id, 44L)
    expect_equal(get_idf_value(idd_env, idf_env, "Material")$value_id, c(1L:9L, 40L:43L, 45:46))
    fld_nm <- c("Conductivity", "Visible Absorptance")
    expect_equal(get_idf_value(idd_env, idf_env, "Material", field = c(4L, 9L))$value_id, c(4L, 9L, 43L))
    expect_equal(get_idf_value(idd_env, idf_env, "Material", field = fld_nm)$value_id, c(4L, 9L, 43L))
    expect_equal(get_idf_value(idd_env, idf_env, "Material", field = c(4L, 9L), align = TRUE)$value_id, c(4L, 9L, 43L, -1L))
    expect_equal(get_idf_value(idd_env, idf_env, "Material", field = fld_nm, align = TRUE)$value_id, c(4L, 9L, 43L, -1L))
    expect_equal(get_idf_value(idd_env, idf_env, "Material", field = c(4L, 3L), complete = TRUE)$value_id, c(1:6, 40:43, 45:46))
    fld_nm <- c("Layer 3", "Visible Absorptance")
    expect_equal(get_idf_value(idd_env, idf_env, c("Construction", "Material"), field = c(4L, 9L))$value_id, c(13L, 9L))
    expect_equal(get_idf_value(idd_env, idf_env, c("Construction", "Material"), field = fld_nm)$value_id, c(13L, 9L))
    expect_equal(get_idf_value(idd_env, idf_env, c("Construction", "Material"),
            field = c(4L, 9L), align = TRUE)$value_id, c(13L, 9L, -1L)
    )
    expect_equal(get_idf_value(idd_env, idf_env, c("Construction", "Material"),
            field = fld_nm, align = TRUE)$value_id, c(13L, 9L, -1L)
    )

    # can init value table
    idf_env1 <- idf_env
    idf_env1$value <- idf_env1$value[0]
    expect_equal(init_idf_value(idd_env, idf_env1, "Material")$value_id, 1:6)

    expect_equivalent(init_idf_value(idd_env, idf_env, "Material"),
        data.table(rleid = 1L, class_id = 55L, class_name = "Material",
            object_id = NA_integer_, object_name = NA_character_,
            field_id = 7081:7086, field_index = 1:6,
            field_name = c("Name", "Roughness", "Thickness", "Conductivity", "Density", "Specific Heat"),
            value_id = 47:52, value_chr = NA_character_, value_num = NA_real_
        )
    )
    expect_equivalent(init_idf_value(idd_env, idf_env, "Material", property = "is_name"),
        data.table(rleid = 1L, class_id = 55L, class_name = "Material",
            object_id = NA_integer_, object_name = NA_character_,
            field_id = 7081:7086, field_index = 1:6,
            field_name = c("Name", "Roughness", "Thickness", "Conductivity", "Density", "Specific Heat"),
            value_id = 47:52, value_chr = NA_character_, value_num = NA_real_,
            is_name = c(TRUE, rep(FALSE, 5))
        )
    )
    # }}}

    # VALUE RELATION {{{
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    expect_is(rel <- get_idf_relation(idd_env, idf_env, direction = "ref_to"), "data.table")
    expect_equal(nrow(rel), 21L)

    expect_is(rel <- get_idf_relation(idd_env, idf_env, direction = "ref_by"), "data.table")
    expect_equal(nrow(rel), 21L)

    # can specify object id
    expect_equal(nrow(get_idf_relation(idd_env, idf_env, object_id = 15L, direction = "ref_to")), 1L)
    expect_equal(nrow(get_idf_relation(idd_env, idf_env, object_id = 15L, direction = "ref_by")), 4L)

    # can specify value id
    expect_equal(nrow(get_idf_relation(idd_env, idf_env, value_id = 109L, direction = "ref_to")), 1L)
    expect_equal(nrow(get_idf_relation(idd_env, idf_env, value_id = 114L, direction = "ref_by")), 8L)

    # can specify both object id and value id
    expect_equal(nrow(get_idf_relation(idd_env, idf_env, 15L, 109L, direction = "ref_to")), 1L)

    # can keep all input id
    expect_is(ref <- get_idf_relation(idd_env, idf_env, value_id = 100:110, direction = "ref_to", keep_all = TRUE), "data.table")
    expect_equal(ref$value_id, 100:110)
    expect_equal(ref$src_object_id, c(rep(NA, 9), 12L, NA))

    # can detect multiple depth
    idf_env <- parse_idf_file(system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr"), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()
    expect_equal(get_idf_relation(idd_env, idf_env, 21L, depth = NULL)$dep, c(0L, 0L, 1L))

    # can add format columns
    expect_is(rel <- get_idf_relation(idd_env, idf_env, 21L, depth = NULL, name = TRUE), "data.table")
    expect_equal(names(rel), c(
        "class_id", "class_name",
        "object_id", "object_name",
        "field_id", "field_index", "field_name",
        "value_id", "value_chr", "value_num", "type_enum",
        "src_class_id", "src_class_name",
        "src_object_id", "src_object_name",
        "src_field_id", "src_field_index", "src_field_name",
        "src_value_id", "src_value_chr", "src_value_num", "src_type_enum",
        "src_enum", "dep"
    ))

    # can specify target group
    expect_equal(get_idf_relation(idd_env, idf_env, 51L, depth = NULL, group = "Schedules", name = TRUE)$src_class_name, "Schedule:Constant")

    # can specify target class
    expect_equal(get_idf_relation(idd_env, idf_env, 51L, depth = NULL, class = "Schedule:Constant", name = TRUE)$src_class_name, "Schedule:Constant")

    # can specify non-existing class
    expect_equal(nrow(get_idf_relation(idd_env, idf_env, 51L, depth = NULL, class = "Window")), 0L)

    # can specify target object
    expect_equal(get_idf_relation(idd_env, idf_env, 51L, object = 53L, name = TRUE)$src_object_name, "AlwaysOn")

    # read a more complex model
    skip_on_cran()

    path_idf <- file.path(eplus_config(8.8)$dir, "ExampleFiles", "5Zone_Transformer.idf")
    idf_env <- parse_idf_file(path_idf, 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    # can handle class-name-references
    expect_equal(nrow(get_idf_relation(idd_env, idf_env, 217L, direction = "ref_to")), 8L)
    expect_equal(nrow(get_idf_relation(idd_env, idf_env, 217L, direction = "ref_to", class_ref = "none")), 4L)
    expect_equal(nrow(get_idf_relation(idd_env, idf_env, 217L, direction = "ref_to", class_ref = "all")), 15L)
    # }}}

    # NODE RELATION {{{
    # read idf
    path_idf <- file.path(eplus_config(8.8)$dir, "ExampleFiles", "5Zone_Transformer.idf")
    idf_env <- parse_idf_file(path_idf, 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    expect_error(get_idf_node_relation(idd_env, idf_env))

    val <- get_idf_value(idd_env, idf_env, object = 277L, field = 5)
    expect_equal(nrow(get_idf_node_relation(idd_env, idf_env, value_id = val$value_id, depth = NULL)), 10L)
    expect_equal(nrow(get_idf_node_relation(idd_env, idf_env, val$object_id, depth = NULL)), 12L)
    expect_equal(nrow(get_idf_node_relation(idd_env, idf_env, val$object_id, depth = NULL, keep_all = TRUE)), 26L)
    expect_equal(nrow(get_idf_node_relation(idd_env, idf_env, val$object_id, val$value_id, depth = NULL)), 10L)

    # can specify object id
    expect_equal(nrow(get_idf_node_relation(idd_env, idf_env, val$object_id, depth = NULL)), 12L)

    # can specify value id
    expect_equal(nrow(get_idf_node_relation(idd_env, idf_env, value_id = val$value_id, depth = NULL)), 10L)

    # can specify both object id and value id
    expect_equal(nrow(get_idf_node_relation(idd_env, idf_env, val$object_id, val$value_id, depth = NULL)), 10L)

    # can keep all input id
    expect_equal(nrow(get_idf_node_relation(idd_env, idf_env, val$object_id, depth = NULL, keep_all = TRUE)), 26L)

    # can add format columns
    expect_is(rel <- get_idf_node_relation(idd_env, idf_env, val$object_id, depth = NULL, name = TRUE), "data.table")
    expect_equal(names(rel), c(
        "class_id", "class_name",
        "object_id", "object_name",
        "field_id", "field_index", "field_name",
        "value_id", "value_chr", "value_num", "type_enum",
        "src_class_id", "src_class_name",
        "src_object_id", "src_object_name",
        "src_field_id", "src_field_index", "src_field_name",
        "src_value_id", "src_value_chr", "src_value_num", "src_type_enum",
        "src_enum", "dep"
    ))

    # can specify target group
    expect_equal(get_idf_node_relation(idd_env, idf_env, val$object_id, depth = NULL, group = "Node-Branch Management", name = TRUE)$class_name,
        c(rep("Branch", 5), rep("Pipe:Adiabatic", 4)))

    # can specify target class
    expect_equal(get_idf_node_relation(idd_env, idf_env, val$object_id, depth = NULL, class = "Branch", name = TRUE)$class_name, rep("Branch", 5))

    # can specify non-existing class
    expect_equal(nrow(get_idf_node_relation(idd_env, idf_env, val$object_id, depth = NULL, class = "Window")), 0L)

    # can specify target object
    expect_equal(get_idf_node_relation(idd_env, idf_env, val$object_id, object = 223, name = TRUE)$class_name, "Branch")
    # }}}
})
# }}}

# NAME DOTS {{{
test_that("NAME DOTS", {
    # read idf
    idf <- read_idf(example(), 8.8)
    idf_env <- get_priv_env(idf)$idf_env()
    idd_env <- get_priv_env(idf)$idd_env()

    # can stop if empty input
    expect_error(expand_idf_dots_name(idd_env, idf_env))
    # can stop if NULL
    expect_error(expand_idf_dots_name(idd_env, idf_env, NULL))
    # can stop if not integer or character
    expect_error(expand_idf_dots_name(idd_env, idf_env, list()))
    expect_error(expand_idf_dots_name(idd_env, idf_env, TRUE))
    expect_error(expand_idf_dots_name(idd_env, idf_env, NaN))
    expect_error(expand_idf_dots_name(idd_env, idf_env, Inf))
    expect_error(expand_idf_dots_name(idd_env, idf_env, list(0)))

    # can stop if contains NA
    expect_error(expand_idf_dots_name(idd_env, idf_env, NA))
    expect_error(expand_idf_dots_name(idd_env, idf_env, NA_character_))
    expect_error(expand_idf_dots_name(idd_env, idf_env, NA_integer_))

    # can work with only object ID inputs
    expect_equal(
        expand_idf_dots_name(idd_env, idf_env, 1:2, a = 3, .property = "has_name")[, -"comment"],
        data.table(rleid = 1:3,
            class_id = c(1L, 13L, 3L), class_name = c("Version", "Timestep", "Building"),
            object_id = 1:3,
            object_name = c(NA_character_, NA_character_, "Simple One Zone (Wireframe DXF)"),
            object_name_lower = c(NA_character_, NA_character_, "simple one zone (wireframe dxf)"),
            new_object_name = c(NA_character_, NA_character_, "a"),
            has_name = c(FALSE, FALSE, TRUE)
        )
    )

    # can exclude input names
    expect_equal(
        expand_idf_dots_name(idd_env, idf_env, 1:2, 3, .keep_name = FALSE)[, -"comment"],
        data.table(rleid = 1:3,
            class_id = c(1L, 13L, 3L),
            class_name = c("Version", "Timestep", "Building"),
            object_id = 1:3,
            object_name = c(NA_character_, NA_character_, "Simple One Zone (Wireframe DXF)"),
            object_name_lower = c(NA_character_, NA_character_, "simple one zone (wireframe dxf)")
        )
    )

    # can work with only object name inputs
    expect_equal(
        expand_idf_dots_name(idd_env, idf_env, Floor = "floor", c("zone one", l = "extlights"))[, -"comment"],
        data.table(rleid = 1:3,
            class_id = c(90L, 100L, 277L),
            class_name = c("Construction", "Zone", "Exterior:Lights"),
            object_id = c(16L, 18L, 49L),
            object_name = c("FLOOR", "ZONE ONE", "ExtLights"),
            object_name_lower = c("floor", "zone one", "extlights"),
            new_object_name = c("Floor", NA_character_, "l")
        )
    )

    # can exclude input names
    expect_equal(
        expand_idf_dots_name(idd_env, idf_env, Floor = "floor", c("zone one", l = "extlights"), .keep_name = FALSE)[, -"comment"],
        data.table(rleid = 1:3,
            class_id = c(90L, 100L, 277L),
            class_name = c("Construction", "Zone", "Exterior:Lights"),
            object_id = c(16L, 18L, 49L),
            object_name = c("FLOOR", "ZONE ONE", "ExtLights"),
            object_name_lower = c("floor", "zone one", "extlights")
        )
    )

    # can work with both object ID and name inputs
    expect_equal(
        expand_idf_dots_name(idd_env, idf_env, 1L, Floor = "floor")[, -"comment"],
        data.table(rleid = 1:2,
            class_id = c(1L, 90L),
            class_name = c("Version", "Construction"),
            object_id = c(1L, 16L),
            object_name = c(NA_character_, "FLOOR"),
            object_name_lower = c(NA_character_, "floor"),
            new_object_name = c(NA_character_, "Floor")
        )
    )
})
# }}}

# VALUE DOTS {{{
test_that("VALUE DOTS", {
    # parse_dots_value {{{
    # can stop if empty input
    expect_error(parse_dots_value(), "Must have length >= 1")
    expect_error(parse_dots_value(NULL), "missing value")

    # can stop if not named
    expect_error(parse_dots_value(list()), class = "eplusr_error_dots_no_name")
    expect_error({x <- list(1); parse_dots_value(x)}, class = "eplusr_error_dots_no_name")
    expect_error(parse_dots_value(1), class = "eplusr_error_dots_no_name")

    # can stop if not list
    expect_error(parse_dots_value(cls = "a"), "list")

    # can stop if missing value
    expect_error(parse_dots_value(cls = list(NA_character_)), "missing")

    # can stop if multiple value
    expect_error(parse_dots_value(cls = list(1:3)), "length")

    # can stop if nested list
    expect_error(parse_dots_value(cls = list(list())), "types")

    # can stop if duplicated field name
    expect_error(parse_dots_value(cls = list(..1 = "", ..1 = "")), "duplicated")

    # can stop if invalid LHS of ":="
    expect_error(parse_dots_value(f(x) := list(..1 = "name")), class = "eplusr_error_dots_ref_lhs")

    # can stop if LHS of ":=" is not allowed
    expect_error(parse_dots_value(a := list(..1 = "name"), .ref_assign = FALSE), class = "eplusr_error_dots_ref")

    # can stop if LHS of ":=" is not allowed
    expect_error(parse_dots_value(c(1, 2) := list(..1 = "name", ..2 = "sch", 1:3), .scalar = FALSE, .pair = TRUE), class = "eplusr_error_dots_pair_length")

    expect_equal(parse_dots_value(cls = list(..1 = "name", ..2 = 1L, NULL, NULL)),
        list(object = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "cls",
                comment = list(), is_ref = FALSE, lhs_sgl = FALSE, rhs_sgl = TRUE, is_empty = FALSE),
             value = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "cls",
                 field_index = c(1:2, rep(NA_integer_, 2)), field_name = NA_character_,
                 value_chr = c("name", "1", rep(NA_character_, 2)),
                 value_num = c(NA_real_, 1, NA_real_, NA_real_)
             )
        )
    )

    expect_equal(
        parse_dots_value(cls = .(..1 = "name", ..2 = 1L, NULL, NULL)),
        parse_dots_value(cls = list(..1 = "name", ..2 = 1L, NULL, NULL))
    )

    # can separate numeric and character value
    expect_equal(parse_dots_value(cls = list(..1 = "name", ..2 = 1L, NULL, NULL)),
        list(object = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "cls",
                comment = list(), is_ref = FALSE, lhs_sgl = FALSE, rhs_sgl = TRUE, is_empty = FALSE),
             value = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "cls",
                 field_index = c(1:2, rep(NA_integer_, 2)), field_name = NA_character_,
                 value_chr = c("name", "1", rep(NA_character_, 2)),
                 value_num = c(NA_real_, 1, NA_real_, NA_real_)
             )
        )
    )

    # can store multiple values
    expect_equal(parse_dots_value(cls = list(..1 = c("name1", "name2"), ..2 = 1:3, NULL, NULL), .scalar = FALSE),
        list(object = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "cls",
                comment = list(), is_ref = FALSE, lhs_sgl = FALSE, rhs_sgl = FALSE, is_empty = FALSE),
             value = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "cls",
                 field_index = c(1:2, rep(NA_integer_, 2)), field_name = NA_character_,
                 value_chr = list(c("name1", "name2"), c("1", "2", "3"), NA_character_, NA_character_),
                 value_num = list(rep(NA_real_, 2), 1:3, NA_real_, NA_real_)
             )
        )
    )

    # can convert empty string to NA
    expect_equal(parse_dots_value(cls = list(roughness = "", ..2 = "  ", name = " ", ..4 = NULL)),
        list(object = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "cls",
                comment = list(), is_ref = FALSE, lhs_sgl = FALSE, rhs_sgl = TRUE, is_empty = FALSE),
             value = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "cls",
                 field_index = c(NA_integer_, 2L, NA_integer_, 4L),
                 field_name = c("roughness", NA_character_, "name", NA_character_),
                 value_chr = NA_character_, value_num = NA_real_
             )
        )
    )

    # can detect empty object
    expect_equal(parse_dots_value(cls = list(), .empty = TRUE),
        list(object = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "cls",
                comment = list(), is_ref = FALSE, lhs_sgl = FALSE, rhs_sgl = TRUE, is_empty = TRUE),
             value = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "cls",
                 field_index = NA_integer_, field_name = NA_character_,
                 value_chr = NA_character_, value_num = NA_real_
             )
        )
    )

    # can use single name on LHS of ":="
    expect_equal(parse_dots_value(cls := list(..1 = "name")),
        list(object = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "cls",
                comment = list(), is_ref = TRUE, lhs_sgl = TRUE, rhs_sgl = TRUE, is_empty = FALSE),
             value = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "cls",
                 field_index = 1L, field_name = NA_character_,
                 value_chr = "name", value_num = NA_real_
             )
        )
    )

    # can use multiple inputs on LHS of ":="
    expect_equal(parse_dots_value(.(1:3) := list(..1 = "name")),
        list(object = data.table(rleid = 1L, each_rleid = 1:3, id = 1:3, name = NA_character_,
                comment = list(), is_ref = TRUE, lhs_sgl = FALSE, rhs_sgl = TRUE, is_empty = FALSE),
             value = data.table(rleid = 1L, each_rleid = 1:3, id = 1:3, name = NA_character_,
                 field_index = 1L, field_name = NA_character_,
                 value_chr = "name", value_num = NA_real_
             )
        )
    )
    expect_equal(parse_dots_value(c(1:3) := list(..1 = "name")),
        list(object = data.table(rleid = 1L, each_rleid = 1:3, id = 1:3, name = NA_character_,
                comment = list(), is_ref = TRUE, lhs_sgl = FALSE, rhs_sgl = TRUE, is_empty = FALSE),
             value = data.table(rleid = 1L, each_rleid = 1:3, id = 1:3, name = NA_character_,
                 field_index = 1L, field_name = NA_character_,
                 value_chr = "name", value_num = NA_real_
             )
        )
    )
    a <- "cls1"
    expect_equal(parse_dots_value(..(a) := list(), ..("cls2") := list(), .empty = TRUE),
        list(object = data.table(rleid = c(1L, 2L), each_rleid = c(1L, 1L),
                id = NA_integer_, name = paste0("cls", 1:2),
                comment = list(), is_ref = TRUE, lhs_sgl = TRUE, rhs_sgl = TRUE, is_empty = TRUE),
             value = data.table(rleid = c(1L, 2L), each_rleid = c(1L, 1L),
                 id = NA_integer_, name = paste0("cls", 1:2),
                 field_index = NA_integer_, field_name = NA_character_,
                 value_chr = NA_character_, value_num = NA_real_
             )
        )
    )

    expect_equal(
        parse_dots_value(cls = .(), .empty = TRUE),
        parse_dots_value(cls = list(), .empty = TRUE)
    )

    expect_equal(
        parse_dots_value(cls := .(..1 = "name")),
        parse_dots_value(cls := list(..1 = "name"))
    )

    expect_equal(
        parse_dots_value(.(1:3) := .(..1 = "name")),
        parse_dots_value(.(1:3) := list(..1 = "name"))
    )

    a <- "cls1"
    expect_equal(
        parse_dots_value(..(a) := .(), ..("cls2") := .(), .empty = TRUE),
        parse_dots_value(..(a) := list(), ..("cls2") := list(), .empty = TRUE)
    )

    # can stop if multiple value for normal list when .pair is TRUE
    expect_error(
        parse_dots_value(
            ..11 = list(1:2), # invalid
            # single id & multi field & multi value
            ..12 = list(1:2, 3:4), # invalid
            .scalar = FALSE, .pair = TRUE
        ), class = "eplusr_error_dots_pair_length"
    )

    # can match multiple id and single value input
    expect_equal(
        parse_dots_value(c(5:6) := list(1), .scalar = FALSE, .pair = FALSE),
        list(
            object = data.table(rleid = 1L, each_rleid = 1:2, id = 5:6,
                name = NA_character_, comment = list(),
                is_ref = TRUE, lhs_sgl = FALSE, rhs_sgl = TRUE, is_empty = FALSE
            ),
            value = data.table(rleid = 1L, each_rleid = 1:2, id = 5:6, name = NA_character_,
                field_index = NA_integer_, field_name = NA_character_,
                value_chr = list("1"),
                value_num = list(1)
            )
        )
    )

    # can pair multiple id and multiple value input
    expect_equal(
        parse_dots_value(
            # multi id & single field & multi value
            c(1:2) := list(..1 = c("name1", "name2")),
            # multi id & multi field & multi value
            c(3:4) := list(..1 = c("name1", "name2"), ..2 = 1:2, NULL, "a"),
            # multi id & single field & scalar value
            c(5:6) := list(1),
            # multi id & multi field & scalar value
            c(7:8) := list(1, 2),
            # single id & single field & scalar value
            ..9 = list(1),
            # single id & multi field & scalar value
            ..10 = list(1, 2),
            cls := list(1:2),
            .scalar = FALSE, .pair = TRUE
        ),

        list(
            object = data.table(
                rleid = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 6L, 7L, 7L),
                each_rleid = c(rep(1:2, 4), 1L, 1L, 1:2),
                id = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, NA, NA),
                name = c(rep(NA_character_, 10), "cls", "cls"), comment = list(),
                is_ref = c(rep(TRUE, 8), rep(FALSE, 2), TRUE, TRUE),
                lhs_sgl = c(rep(FALSE, 10), TRUE, TRUE),
                rhs_sgl = c(rep(FALSE, 4), rep(TRUE, 6), rep(FALSE, 2)),
                is_empty = FALSE
            ),
            value = data.table(
                rleid = c(rep(1L, 2), rep(2L, 8), rep(3L, 2), rep(4L, 4), 5L, rep(6L, 2), rep(7L, 2)),
                each_rleid = c(1:2, rep(1:2, each = 4), 1:2, rep(1:2, each = 2), 1L, rep(1L, 2), 1:2),
                id = c(1L, 2L, rep(3L, 4), rep(4L, 4), 5L, 6L, rep(7L, 2), rep(8L, 2), 9L, rep(10L, 2), NA, NA),
                name = c(rep(NA_character_, 19), "cls", "cls"),
                field_index = c(1L, 1L, 1L, 2L, rep(NA, 2), 1L, 2L, rep(NA, 13)),
                field_name = NA_character_,
                value_chr = c(
                    "name1", "name2",
                    "name1", "1", NA, "a", "name2", "2", NA, "a",
                    "1", "1",
                    "1", "2", "1", "2",
                    "1",
                    "1", "2",
                    "1", "2"),
                value_num = c(
                    NA, NA,
                    NA, 1, NA, NA, NA, 2, NA, NA,
                    1, 1,
                    1, 2, 1, 2,
                    1,
                    1, 2,
                    1, 2
                )
            )
        )
    )

    # can stop if id and value length is not the same
    expect_error(
        parse_dots_value(c(1:3) := list(..1 = c("name1", "name2"), ..2 = 1:3), .scalar = FALSE, .pair = TRUE),
        class = "eplusr_error_dots_pair_length"
    )

    # can use variable input on LHS of ":="
    expect_equal({x <- 1:3; parse_dots_value(c(x) := list(..1 = "name"))},
        list(object = data.table(rleid = 1L, each_rleid = 1:3, id = 1:3, name = NA_character_,
                comment = list(), is_ref = TRUE, lhs_sgl = FALSE, rhs_sgl = TRUE, is_empty = FALSE),
             value = data.table(rleid = 1L, each_rleid = 1:3, id = 1:3, name = NA_character_,
                 field_index = 1L, field_name = NA_character_,
                 value_chr = "name", value_num = NA_real_
             )
        )
    )
    expect_equal({x <- 1:3; parse_dots_value(.(x) := list(..1 = "name"))},
        list(object = data.table(rleid = 1L, each_rleid = 1:3, id = 1:3, name = NA_character_,
                comment = list(), is_ref = TRUE, lhs_sgl = FALSE, rhs_sgl = TRUE, is_empty = FALSE),
             value = data.table(rleid = 1L, each_rleid = 1:3, id = 1:3, name = NA_character_,
                 field_index = 1L, field_name = NA_character_,
                 value_chr = "name", value_num = NA_real_
             )
        )
    )

    # can accept quote input on LHS of ":="
    expect_equal({x <- quote(cls := list(..1 = "name")); parse_dots_value(x)},
        list(object = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "cls",
                comment = list(), is_ref = TRUE, lhs_sgl = TRUE, rhs_sgl = TRUE, is_empty = FALSE),
             value = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "cls",
                 field_index = 1L, field_name = NA_character_,
                 value_chr = "name", value_num = NA_real_
             )
        )
    )

    # can accept variable input
    expect_equal({x <- list(a = 1L, b = 2L); parse_dots_value(obj = x, .empty = TRUE)},
        list(object = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "obj",
                comment = list(), is_ref = FALSE, lhs_sgl = FALSE, rhs_sgl = TRUE, is_empty = FALSE),
             value = data.table(rleid = 1L, each_rleid = 1L, id = NA_integer_, name = "obj",
                 field_index = NA_integer_, field_name = c("a", "b"),
                 value_chr = c("1", "2"), value_num = 1:2
             )
        )
    )
    expect_equal({x <- list(a = list(1), b = list(), ..5 = list()); parse_dots_value(x, .empty = TRUE)},
        list(object = data.table(rleid = 1:3, each_rleid = 1L, id = c(NA, NA, 5L), name = c("a", "b", NA),
                comment = list(), is_ref = FALSE, lhs_sgl = FALSE, rhs_sgl = TRUE, is_empty = c(FALSE, TRUE, TRUE)),
             value = data.table(rleid = 1:3, each_rleid = 1L, id = c(NA, NA, 5L), name = c("a", "b", NA),
                 field_index = NA_integer_, field_name = NA_character_,
                 value_chr = c("1", NA, NA), value_num = c(1, NA, NA)
             )
        )
    )

    # whole game
    expect_equal(
        {
            x <- list(cls8 = list(fld1 = NULL, fld2 = "a", NULL, 2L, fld3 = NULL, .comment = c("a", "b")))
            parse_dots_value(
                # empty
                cls1 = list(),
                cls2 = list(.comment = c("a", "b")),
                cls3 = list(NULL, NULL, fld1 = NULL, .comment = c("a", "b")),
                cls4 = list(NULL, fld1 = "a", fld2 = 2L, fld3 = NULL, "a", 1L, .comment = c("a", "b")),
                cls5 := list(.comment = c("a", "b")),
                c("cls6", "cls7") := list(..1 = NULL, ..3 = NULL, fld1 = NULL, .comment = c("a", "b")),
                x,
                .empty = TRUE
            )
        },
        list(
            object = data.table(
                rleid = c(1:5, rep(6L, 2), 7), each_rleid = c(rep(1L, 6), 2L, 1L),
                id = NA_integer_,
                name = paste0("cls", 1:8),
                comment = c(list(NULL), rep(list(c("a", "b")), 7L)),
                is_ref = c(rep(FALSE, 4), rep(TRUE, 3), FALSE),
                lhs_sgl = c(rep(FALSE, 4), TRUE, rep(FALSE, 3)),
                rhs_sgl = TRUE,
                is_empty = c(rep(TRUE, 2), rep(FALSE, 2), TRUE, rep(FALSE, 3))
            ),
            value = data.table(
                rleid = c(1L, 2L, rep(3L, 3), rep(4L, 6), 5L, rep(6L, 2*3), rep(7L, 5)),
                each_rleid = c(rep(1L, 15), rep(2L, 3), rep(1L, 5)),
                id = NA_integer_,
                name = c("cls1", "cls2", rep("cls3", 3), rep("cls4", 6), "cls5", rep(c("cls6", "cls7"), each = 3), rep("cls8", 5)),
                field_index = c(rep(NA_integer_, 12), rep(c(1L, 3L, NA_integer_), 2), rep(NA_integer_, 5)),
                field_name = c(rep(NA_character_, 4), "fld1", NA_character_, paste0("fld", 1:3), rep(NA_character_, 5),
                    "fld1", rep(NA_character_, 2), paste0("fld", c(1, 1, 2)), rep(NA_character_, 2), "fld3"),
                value_chr = c(rep(NA_character_, 6), "a", "2", NA_character_, "a", "1", rep(NA_character_, 8), "a", NA_character_, "2", NA_character_),
                value_num = c(rep(NA_real_, 7), 2, rep(NA_real_, 2), 1, rep(NA_real_, 10), 2, NA_real_)
            )
        )
    )
    # }}}

    # expand_idf_dots_value {{{
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    # can stop if duplicated class names are given
    expect_error(expand_idf_dots_value(idd_env, idf_env, Site_Location = list(), `Site:Location` = list(), .unique = TRUE))

    # match by class {{{
    expect_error(res <- expand_idf_dots_value(idd_env, idf_env, c(1) := list(..1 = 8.8, 'Version Identifier' = 8.8)), class = "eplusr_error_dots_multi_match")

    # only class id
    expect_is(res <- expand_idf_dots_value(idd_env, idf_env, c(1) := list(8.8), .empty = FALSE), "list")
    expect_equal(names(res), c("object", "value"))
    expect_equal(res$object,
        data.table(rleid = 1L, class_id = 1L, class_name = "Version",
            object_id = NA_integer_, object_name = NA_character_,
            object_name_lower = NA_character_, comment = list()
        )
    )
    expect_equal(res$value,
        data.table(rleid = 1L, class_id = 1L, class_name = "Version",
            object_id = NA_integer_, object_name = NA_character_, field_id = 1L,
            field_index = 1L, field_name = "Version Identifier", value_id = NA_integer_,
            value_chr = "8.8", value_num = 8.8)
    )

    expect_is(
        res <- expand_idf_dots_value(idd_env, idf_env,
            RunPeriod = list("Test1", ..2 = 1, 1, End_Month = 2, 1, "Monday", Apply_Weekend_Holiday_Rule = "No"),
            RunPeriod = list("Test2", 1, 1, 2, 1),
            Material = list("Mat"),
            Construction = list("TestConst", "R13LAYER"),
            SimulationControl = list(),
            SimulationControl = list(..7 = "yes"),
            .empty = TRUE, .unique = FALSE, .default = TRUE
        ), "list"
    )
    expect_equal(names(res), c("object", "value"))
    expect_equivalent(res$object,
        data.table(rleid = 1:6, class_id = c(22L, 22L, 55L, 90L, 2L, 2L),
            class_name = c("RunPeriod", "RunPeriod", "Material", "Construction", "SimulationControl", "SimulationControl"),
            object_id = NA_integer_, object_name = NA_character_,
            object_name_lower = NA_character_, comment = list()
        )
    )

    expect_equivalent(res$value[, -"field_name"],
        data.table(
            rleid = c(rep(1L, 11), rep(2L, 11), rep(3L, 6), rep(4L, 2), rep(5L, 5), rep(6L, 7)),
            class_id = c(rep(22L, 22), rep(55, 6), rep(90L, 2), rep(2L, 12)),
            class_name = c(rep("RunPeriod", 22), rep("Material", 6), rep("Construction", 2), rep("SimulationControl", 12)),
            object_id = NA_integer_, object_name = NA_character_,
            field_id = c(104:114, 104:114, 7081:7086, 11006:11007, 2:6, 2:8),
            field_index = c(1:11, 1:11, 1:6, 1:2, 1:5, 1:7),
            value_id = NA_integer_,
            value_chr = c(
                "Test1", "1", "1", "2", "1", "Monday", "Yes", "Yes", "No", "Yes", "Yes",
                "Test2", "1", "1", "2", "1", "UseWeatherFile", "Yes", "Yes", "No", "Yes", "Yes",
                "Mat", NA, NA, NA, NA, NA,
                "TestConst", "R13LAYER",
                "No", "No", "No", "Yes", "Yes",
                "No", "No", "No", "Yes", "Yes", "No", "yes"
            ),
            value_num = c(
                NA, 1, 1, 2, 1, NA, NA, NA, NA, NA, NA,
                NA, 1, 1, 2, 1, NA, NA, NA, NA, NA, NA,
                NA, NA, NA, NA, NA, NA,
                NA, NA,
                NA, NA, NA, NA, NA,
                NA, NA, NA, NA, NA, NA, NA
            )
        )
    )
    # }}}

    # match by object {{{
    # can stop if value number is not the same as object number in that class
    expect_error(expand_idf_dots_value(idd_env, idf_env, Output_Variable := list(key_value = c("*", "*")),
            .type = "object", .scalar = FALSE, .pair = TRUE, .unique = FALSE),
        class = "eplusr_error_dots_pair_length"
    )

    # can work for empty objects
    expect_is(res <- expand_idf_dots_value(idd_env, idf_env, Output_Variable := list(), .type = "object"), "list")
    expect_equal(names(res), c("object", "value"))
    expect_equal(res$object,
        data.table(rleid = 1L, class_id = 776L, class_name = "Output:Variable",
            object_id = 27:40, object_name = NA_character_,
            object_name_lower = NA_character_, comment = list()
        )
    )
    expect_equal(res$value[, -"value_chr"],
        data.table(rleid = 1L, class_id = 776L, class_name = "Output:Variable",
            object_id = rep(27:40, each = 3), object_name = NA_character_,
            field_id = rep(59058:59060, 14), field_index = rep(1:3, 14),
            field_name = rep(c("Key Value", "Variable Name", "Reporting Frequency"), 14),
            value_id = 262:303, value_num = NA_real_
        )
    )

    ## Class := list()
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            Output_Variable := list(),
            .scalar = FALSE, .pair = TRUE
        )
    )
    expect_equal(res$object$object_id, 27:40)
    expect_equal(res$value$field_index, rep(1:3, 14))
    cls <- "Output_Variable"
    expect_is(class = "list",
        res1 <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            ..(cls) := list(),
            .scalar = FALSE, .pair = TRUE
        )
    )
    expect_equal(res, res1)
    ## Class := list(), dup
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            Output_Variable := list(), Output_Variable := list(),
            .scalar = FALSE, .pair = TRUE, .unique = FALSE
        )
    )
    expect_equal(res$object$object_id, rep(27:40, 2))
    expect_equal(res$value$field_index, rep(1:3, 14 * 2))

    ## Class := list(Fld = Val)
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            Output_Variable := list(key_value = "*"),
            .scalar = FALSE, .pair = TRUE
        )
    )
    expect_equal(res$object$object_id, 27:40)
    expect_equal(res$value$field_index, rep(1L, 14))
    ## Class := list(Fld = Val), dup
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            Output_Variable := list(key_value = "*"), Output_Variable := list(key_value = "*"),
            .scalar = FALSE, .pair = TRUE, .unique = FALSE
        )
    )
    expect_equal(res$object$object_id, rep(27:40, 2))
    expect_equal(res$value$field_index, rep(1, 14 * 2))

    ## Class := list(Fld1 = Val1, Fld = Val2)
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            Output_Variable := list(key_value = "*", variable_name = NULL),
            .scalar = FALSE, .pair = TRUE
        )
    )
    expect_equal(res$object$object_id, 27:40)
    expect_equal(res$value$field_index, rep(1:2, 14))

    ## Class := list(Val1, Val2)
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env,
            SimulationControl := list("No", "No", "No", "No", "Yes"),
            .type = "object", .complete = TRUE, .all = FALSE,
            .scalar = FALSE, .pair = TRUE, .ref_assign = TRUE,
            .unique = TRUE, .empty  = TRUE, .default = TRUE
        )
    )
    expect_equal(res$value$field_index, 1:5)
    expect_equal(res$value$value_chr, c("No", "No", "No", "No", "Yes"))

    ## Class := list(Fld2 = Val2, Val1)
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env,
            SimulationControl := list(do_zone_sizing_calculation = "No", "No", "No", "No", "Yes"),
            .type = "object", .complete = TRUE, .all = FALSE,
            .scalar = FALSE, .pair = TRUE, .ref_assign = TRUE,
            .unique = TRUE, .empty  = TRUE, .default = TRUE
        )
    )
    expect_equal(res$value$field_index, 1:5)
    expect_equal(res$value$value_chr, c("No", "No", "No", "No", "Yes"))

    ## Class := list(Fld1 = Val1, Fld = Val2), dup
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            Output_Variable := list(key_value = "*", variable_name = NULL),
            Output_Variable := list(key_value = "*", variable_name = NULL),
            .scalar = FALSE, .pair = TRUE, .unique = FALSE, .empty = FALSE
        )
    )
    expect_equal(res$object$object_id, rep(27:40, 2))
    expect_equal(res$value$field_index, rep(1:2, 14 * 2))

    ## Class := list(Fld1 = c(Val1, Val2, Val3, ...))
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            Output_Variable := list(key_value = rep("*", 14)),
            .scalar = FALSE, .pair = TRUE
        )
    )
    expect_equal(res$object$object_id, 27:40)
    expect_equal(res$value$field_index, rep(1, 14))
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            Output_Variable := list(rep("*", 14), "Temp"),
            .scalar = FALSE, .pair = TRUE
        )
    )
    expect_equal(res$object$object_id, 27:40)
    expect_equal(res$value$field_index, rep(1:2, 14))
    ## Class := list(Fld1 = c(Val1, Val2, Val3, ...)), dup
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            Output_Variable := list(key_value = rep("*", 14)),
            Output_Variable := list(key_value = rep("*", 14)),
            .scalar = FALSE, .pair = TRUE, .unique = FALSE
        )
    )
    expect_equal(res$object$object_id, rep(27:40, 2))
    expect_equal(res$value$field_index, rep(1, 14 * 2))

    ## Class := list(Fld1 = c(Val1, Val2, Val3, ...), Fld2 = c(Val4, Val5, Val6, ...))
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            Output_Variable := list(key_value = rep("*", 14), variable_name = rep("", 14)),
            .scalar = FALSE, .pair = TRUE
        )
    )
    expect_equal(res$object$object_id, 27:40)
    expect_equal(res$value$field_index, rep(1:2, 14))
    ## Class := list(Fld1 = c(Val1, Val2, Val3, ...), Fld2 = c(Val4, Val5, Val6, ...)), dup
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            Output_Variable := list(key_value = rep("*", 14), variable_name = rep("", 14)),
            Output_Variable := list(key_value = rep("*", 14), variable_name = rep("", 14)),
            .scalar = FALSE, .pair = TRUE, .unique = FALSE
        )
    )
    expect_equal(res$object$object_id, rep(27:40, 2))
    expect_equal(res$value$field_index, rep(1:2, 14 * 2))

    ## Obj = list()
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            ..27 = list(),
            .scalar = FALSE, .pair = TRUE
        )
    )
    expect_equal(res$object$object_id, 27)
    expect_equal(res$value$field_index, 1:3)
    ## Obj = list(), dup
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            ..27 = list(), ..27 = list(),
            .scalar = FALSE, .pair = TRUE, .unique = FALSE
        )
    )
    expect_equal(res$object$object_id, rep(27, 2))
    expect_equal(res$value$field_index, rep(1:3, 2))

    ## Obj = list(Fld = Val)
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            ..27 = list(key_value = "*"),
            .scalar = FALSE, .pair = TRUE
        )
    )
    expect_equal(res$object$object_id, 27)
    expect_equal(res$value$field_index, 1)
    ## Obj = list(Fld = Val), dup
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            ..27 = list(key_value = "*"), ..27 = list(key_value = "*"),
            .scalar = FALSE, .pair = TRUE, .unique = FALSE
        )
    )
    expect_equal(res$object$object_id, rep(27, 2))
    expect_equal(res$value$field_index, rep(1, 2))

    ## Obj = list(Fld1 = Val1, Fld2 = Val2)
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            ..27 = list(key_value = "*", variable_name = NULL),
            .scalar = FALSE, .pair = TRUE
        )
    )
    expect_equal(res$object$object_id, 27)
    expect_equal(res$value$field_index, 1:2)
    ## Obj = list(Fld1 = Val1, Fld2 = Val2), dup
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            ..27 = list(key_value = "*", variable_name = NULL),
            ..27 = list(key_value = "*", variable_name = NULL),
            .scalar = FALSE, .pair = TRUE, .unique = FALSE
        )
    )
    expect_equal(res$object$object_id, rep(27, 2))
    expect_equal(res$value$field_index, rep(1:2, 2))

    ## c(Obj1, Obj2) := list(Fld = Val)
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            c(27, 28) := list(key_value = "*"),
            .scalar = FALSE, .pair = TRUE
        )
    )
    expect_equal(res$object$object_id, 27:28)
    expect_equal(res$value$field_index, rep(1, 2))
    ## c(Obj1, Obj2) := list(Fld = Val), dup
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            c(27, 28) := list(key_value = "*"), c(27, 28) := list(key_value = "*"),
            .scalar = FALSE, .pair = TRUE, .unique = FALSE
        )
    )
    expect_equal(res$object$object_id, rep(27:28, 2))
    expect_equal(res$value$field_index, rep(1, 2 * 2))

    ## c(Obj1, Obj2) := list(Fld1 = Val1, Fld2 = Val2)
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            c(27, 28) := list(key_value = "*", variable_name = NULL),
            .scalar = FALSE, .pair = TRUE
        )
    )
    expect_equal(res$object$object_id, 27:28)
    expect_equal(res$value$field_index, rep(1:2, 2))
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            c(27, 28) := list(key_value = rep("*", 3), variable_name = NULL),
            .scalar = FALSE, .pair = FALSE
        )
    )
    expect_equal(res$object$object_id, 27:28)
    expect_equal(res$value$field_index, rep(1:2, 2))
    expect_equal(res$value$value_chr, list(rep("*", 3), NA_character_, rep("*", 3), NA_character_))
    ## c(Obj1, Obj2) := list(Fld1 = Val1, Fld2 = Val2), dup
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            c(27, 28) := list(key_value = "*", variable_name = NULL),
            c(27, 28) := list(key_value = "*", variable_name = NULL),
            .scalar = FALSE, .pair = TRUE, .unique = FALSE
        )
    )
    expect_equal(res$object$object_id, rep(27:28, 2))
    expect_equal(res$value$field_index, rep(1:2, 2 * 2))

    ## c(Obj1, Obj2) := list(Fld1 = c(Val1, Val2), Fld2 = c(Val3, Val4))
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            c(27, 28) := list(key_value = c("*", "*"), variable_name = c("", "")),
            .scalar = FALSE, .pair = TRUE
        )
    )
    expect_equal(res$object$object_id, 27:28)
    expect_equal(res$value$field_index, rep(1:2, 2))
    ## c(Obj1, Obj2) := list(Fld1 = c(Val1, Val2), Fld2 = c(Val3, Val4)), dup
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env, .type = "object", .complete = FALSE,
            c(27, 28) := list(key_value = c("*", "*"), variable_name = c("", ""), "hourly"),
            c(27, 28) := list(key_value = c("*", "*"), variable_name = c("", "")),
            .scalar = FALSE, .pair = TRUE, .unique = FALSE
        )
    )
    expect_equal(res$object$object_id, rep(27:28, 2))
    expect_equal(res$value$field_index, c(rep(1:3, 2), rep(1:2, 2)))

    # whole game
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env,
            # Class := list() # extract all data from a class
            BuildingSurface_Detailed := list(),
            # Class := list(Fld = Val) # set field values in all class objects
            Material_NoMass := list(roughness = "smooth", thermal_absorptance = 0.8),
            # Class := list(Fld = c(Val1, Val2, Val3)) # set field values individually in a class
            BuildingSurface_Detailed := list(outside_boundary_condition = rep("Adiabatic", 6)),
            # Object = list() # extract object data with new comments
            R13LAYER = list(.comment = c("new", "comment")),
            # object = list(Fld1 = Val1, Fld2 = Val2) # set object field values
            ..8 = list(name = "name", start_year = NULL),
            # .(Obj1, Obj2, Obj3) := list(Fld = c(Val1, Val2, Val3)) # set field values individually
            c("r13wall", "floor", "roof31") := list(paste("Const", 1:3), "r13layer", c("r13layer", "r31layer", "r13layer")),
            .type = "object", .complete = TRUE, .scalar = FALSE, .pair = FALSE, .empty = TRUE,
            .unique = FALSE
        )
    )
    expect_equal(res$object$object_id, c(21:26, 12:13, 21:26, 12, 8, 15:17))
    expect_equal(nrow(res$value), 283)
    expect_is(res$value$value_chr, "list")
    expect_is(res$value$value_num, "list")
    expect_equal(res$value$value_chr[c(282:283)], list("r13layer", c("r13layer", "r31layer", "r13layer")))

    # whole game
    expect_is(class = "list",
        res <- expand_idf_dots_value(idd_env, idf_env,
            # Class := list() # extract all data from a class
            BuildingSurface_Detailed := list(),
            # Class := list(Fld = Val) # set field values in all class objects
            Material_NoMass := list(roughness = "smooth", thermal_absorptance = 0.8),
            # Class := list(Fld = c(Val1, Val2, Val3)) # set field values individually in a class
            BuildingSurface_Detailed := list(outside_boundary_condition = rep("Adiabatic", 6)),
            # Object = list() # extract object data with new comments
            R13LAYER = list(.comment = c("new", "comment")),
            # object = list(Fld1 = Val1, Fld2 = Val2) # set object field values
            ..8 = list(name = "name", start_year = NULL),
            # .(Obj1, Obj2, Obj3) := list(Fld = c(Val1, Val2, Val3)) # set field values individually
            c("r13wall", "floor", "roof31") := list(paste("Const", 1:3), "r13layer", c("r13layer", "r31layer", "r13layer")),
            .type = "object", .complete = TRUE, .scalar = FALSE, .pair = TRUE, .empty = TRUE,
            .unique = FALSE
        )
    )
    expect_equal(res$object$object_id, c(21:26, 12:13, 21:26, 12, 8, 15:17))
    expect_equal(nrow(res$value), 283)

    # cannot modify same object multiple times at the same time
    expect_error(expand_idf_dots_value(idd_env, idf_env, Construction := list(), Floor = list(), .type = "object"))
    # }}}
    # }}}
})
# }}}

# OBJECT DOTS {{{
test_that("OBJECT DOTS", {
    # read idf
    idf <- read_idf(example(), 8.8)
    idf_env <- get_priv_env(idf)$idf_env()
    idd_env <- get_priv_env(idf)$idd_env()

    # can stop if empty input
    expect_error(expand_idf_dots_object(idd_env, idf_env), class = "eplusr_error_dots_empty")
    # can stop if NULL
    expect_error(expand_idf_dots_object(idd_env, idf_env, NULL), class = "eplusr_error_dots_format")
    # can stop if duplicates
    expect_error(expand_idf_dots_object(idd_env, idf_env, idf, idf), class = "eplusr_error_dots_format")
    expect_error(expand_idf_dots_object(idd_env, idf_env, idf, list(idf)), class = "eplusr_error_dots_format")
    expect_error(expand_idf_dots_object(idd_env, idf_env, list(idf), list(idf)), class = "eplusr_error_dots_format")
    expect_error(expand_idf_dots_object(idd_env, idf_env, idf$Version, idf$Version), class = "eplusr_error_dots_format")
    expect_error(expand_idf_dots_object(idd_env, idf_env, idf$Version, list(idf$Version)), class = "eplusr_error_dots_format")
    expect_error(expand_idf_dots_object(idd_env, idf_env, list(idf$Version), list(idf$Version)), class = "eplusr_error_dots_format")

    # can remove duplicates
    expect_is(l <- expand_idf_dots_object(idd_env, idf_env, list(idf$Version), list(idf$Version), .unique = NULL), class = "list")
    expect_equal(names(l), c("meta", "object", "value"))
    expect_equal(names(l$meta), c("rleid", "version", "uuid", "object_id", "idd_env", "idf_env"))
    expect_equal(names(l$object), c("rleid", "class_id", "class_name", "object_id", "object_name", "object_name_lower", "comment"))
    expect_equal(names(l$value),
        c("rleid", "class_id", "class_name", "object_id", "object_name",
          "field_id", "field_index", "field_name", "value_id", "value_chr", "value_num")
    )
    expect_equal(nrow(l$meta), 1L)
    expect_equal(nrow(l$object), 1L)
    expect_equal(nrow(l$value), 1L)

    # can keep duplicates
    expect_is(l <- expand_idf_dots_object(idd_env, idf_env, list(idf$Version), list(idf$Version), .unique = FALSE), class = "list")
    expect_equal(names(l), c("meta", "object", "value"))
    expect_equal(names(l$meta), c("rleid", "version", "uuid", "object_id", "idd_env", "idf_env"))
    expect_equal(names(l$object), c("rleid", "class_id", "class_name", "object_id", "object_name", "object_name_lower", "comment"))
    expect_equal(names(l$value),
        c("rleid", "class_id", "class_name", "object_id", "object_name",
          "field_id", "field_index", "field_name", "value_id", "value_chr", "value_num")
    )
    expect_equal(nrow(l$meta), 2L)
    expect_equal(nrow(l$object), 2L)
    expect_equal(nrow(l$value), 2L)

    # can stop if version is not the same
    skip_on_cran()

    expect_error(expand_idf_dots_object(idd_env, idf_env, empty_idf(8.7)), class = "eplusr_error_dots_format")

    # can proceed if version is not the same
    expect_is(expand_idf_dots_object(idd_env, idf_env, empty_idf(8.7), .strict = FALSE), "list")

    expect_silent(expand_idf_dots_value(idd_env, idf_env, ..53 = list("sch"), .type = "object", .empty = FALSE))
})
# }}}

# LITERAL DOTS {{{
test_that("LITERAL DOTS", {
    # read idf
    idf <- read_idf(example(), 8.8)
    idf_env <- get_priv_env(idf)$idf_env()
    idd_env <- get_priv_env(idf)$idd_env()

    expect_error(expand_idf_dots_literal(idd_env, idf_env))
    expect_error(expand_idf_dots_literal(idd_env, idf_env, NULL))
    expect_error(expand_idf_dots_literal(idd_env, idf_env, list()))
    expect_error(expand_idf_dots_literal(idd_env, idf_env, c("a", NA_character_)))
    expect_error(expand_idf_dots_literal(idd_env, idf_env, data.table()))
    expect_error(expand_idf_dots_literal(idd_env, idf_env, data.table(id = NA, index = NA, value = NA)))

    # can stop if trying to add Version
    expect_error(expand_idf_dots_literal(idd_env, idf_env, "Version,8.7;\n"))

    # can stop if trying to match objects without name
    expect_error(expand_idf_dots_literal(idd_env, idf_env, "SimulationControl,no;\n", .exact = TRUE))

    # can stop if concatenated line
    expect_error(expand_idf_dots_literal(idd_env, idf_env, "Construction, const1, mat; Construction, const2;\n"), class = "eplusr_error_parse_idf_line")

    # can stop if invalid object names
    expect_error(expand_idf_dots_literal(idd_env, idf_env, .exact = TRUE, "Construction, const, mat;\n"), class = "eplusr_error_dots_format")

    mat <- get_idd_table(idd_env, "Material")
    mat1 <- set(copy(mat), NULL, "value", c("", " ", "  ", rep(NA_character_, 3)))
    mat2 <- set(copy(mat), NULL, "value", list(list(" ")))
    mat3 <- get_idf_table(idd_env, idf_env, "Material:NoMass")

    # can stop if duplicates in combinations of class, index and field
    expect_error(expand_idf_dots_literal(idd_env, idf_env, rbindlist(list(mat1, mat1))))

    # can stop if missing id column
    expect_error(expand_idf_dots_literal(idd_env, idf_env, copy(mat3)[, id := NULL], .exact = TRUE))

    # can stop if duplicates in combinations of id, class, index and field
    expect_error(expand_idf_dots_literal(idd_env, idf_env, rbindlist(list(mat1, mat1))[, id := 1L]))
    expect_error(expand_idf_dots_literal(idd_env, idf_env, copy(mat3)[, id := 1L], .exact = TRUE))

    # can stop if invalid class name
    expect_error(expand_idf_dots_literal(idd_env, idf_env, copy(mat1)[, class := "mat"]))
    expect_error(expand_idf_dots_literal(idd_env, idf_env, copy(mat3)[, class := "mat"], .exact = TRUE))

    # can stop if invalid object id
    expect_error(expand_idf_dots_literal(idd_env, idf_env, copy(mat1)[, id := 1e5L], .exact = TRUE))

    # can stop if invalid field index
    expect_error(expand_idf_dots_literal(idd_env, idf_env, rbindlist(list(mat1, mat1))[, index := .I]))
    expect_error(expand_idf_dots_literal(idd_env, idf_env, rbindlist(list(mat3, mat3))[, index := .I], .exact = TRUE))

    # whole game
    expect_is(class = "list",
        l <- expand_idf_dots_literal(idd_env, idf_env, mat1, mat2,
            c("! some comments;",
              "Material,",
              "    mat,                     !- Name",
              "    MediumSmooth,            !- Roughness",
              "    0.667;                   !- Thickness {m}",
              "Construction, const, mat;"
            ), mat3
        )
    )
    expect_equal(names(l), c("object", "value"))
    expect_equal(l$object,
        data.table(
            rleid = 1:6, class_id = c(55L, 90L, 55L, 55L, 56L, 56L),
            class_name = c("Material", "Construction", "Material", "Material", "Material:NoMass", "Material:NoMass"),
            object_id = NA_integer_, object_name = NA_character_, object_name_lower = NA_character_,
            comment = c(list(" some comments;"), rep(list(NULL), 5L))
        )
    )
    expect_equal(l$value$rleid, c(rep(1L, 6), rep(2L, 2), rep(3:6, each = 6)))
    expect_equal(l$value$class_id, c(rep(55L, 6), rep(90L, 2), rep(c(55L, 55L, 56L, 56L), each = 6)))
    expect_equal(l$value$object_id, rep(NA_integer_, 32))
    expect_equal(l$value$object_name, rep(NA_character_, 32))
    expect_equal(l$value$value_id, rep(NA_integer_, 32))
    expect_equal(l$value$value_num, c(NA, NA, 0.667, rep(NA, 19), 2.290965, 0.9, 0.75, 0.75, NA, NA, 5.456, 0.9, 0.75, 0.75))

    # whole game
    expect_is(class = "list",
        l <- expand_idf_dots_literal(idd_env, idf_env, .exact = TRUE, mat3,
            c("! some comments;",
              "Material,",
              "    C5 - 4 IN HW CONCRETE,   !- Name",
              "    MediumSmooth,            !- Roughness",
              "    0.20;                    !- Thickness {m}"
            ), mat3,
            c("Material,",
              "    C5 - 4 IN HW CONCRETE,   !- Name",
              "    MediumSmooth,            !- Roughness",
              "    0.20;                    !- Thickness {m}"
            )
        )
    )

    expect_equal(names(l), c("object", "value"))
    expect_equal(l$object,
        data.table(
            rleid = 1:6, class_id = c(rep(55L, 2), rep(56L, 4)),
            class_name = c(rep("Material", 2), rep("Material:NoMass", 4)),
            object_id = c(rep(14L, 2), 12L, 13L, 12L, 13L),
            object_name = c(rep("C5 - 4 IN HW CONCRETE", 2), rep(c("R13LAYER", "R31LAYER"), 2)),
            object_name_lower = c(rep("c5 - 4 in hw concrete", 2), rep(c("r13layer", "r31layer"), 2)),
            comment = c(list(" some comments;"), rep(list(NULL), 5L))
        )
    )
    expect_equal(l$value$rleid, c(rep(1L, 6), rep(2:6, each = 6)))
    expect_equal(l$value$class_id, c(rep(55L, 12), rep(56L, 24)))
    expect_equal(l$value$object_id, c(rep(14L, 12), rep(c(12L, 13L, 12L, 13L), each = 6)))
    expect_equal(l$value$object_name, c(rep("C5 - 4 IN HW CONCRETE", 12), rep(rep(c("R13LAYER", "R31LAYER"), 2), each = 6)))
    expect_equal(l$value$value_id, c(rep(99:104, 2), rep(87:98, 2)))
    expect_equal(l$value$value_num, c(rep(c(NA, NA, 0.2, NA, NA, NA), 2), rep(c(NA, NA, 2.290965, 0.9, 0.75, 0.75, NA, NA, 5.456, 0.9, 0.75, 0.75), 2)))
})
# }}}

# REGEX {{{
test_that("regex", {
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    # can stop if class contains duplications
    expect_error(expand_idf_regex(idd_env, idf_env, "", class = c("a", "a")))

    expect_is(l <- expand_idf_regex(idd_env, idf_env, "ABC"), "list")
    expect_equal(nrow(l$object), 0L)
    expect_equal(nrow(l$value), 0L)

    expect_is(l <- expand_idf_regex(idd_env, idf_env, "zn", "Zone", ignore.case = TRUE), "list")
    expect_equal(nrow(l$object), 6)
    expect_equal(nrow(l$value), 132)
    expect_equal(l$value$value_id, 130:261)
})
# }}}

# NEW OBJECT NAME {{{
test_that("make_idf_object_name", {
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    # can stop if trying to assign names to objects that do not have name attribute
    expect_error(
        make_idf_object_name(idd_env, idf_env, expand_idf_dots_name(idd_env, idf_env, a = 4)),
        class = "eplusr_error_cannot_name"
    )

    # can stop if there are duplications in new names
    expect_error(
        make_idf_object_name(idd_env, idf_env, expand_idf_dots_name(idd_env, idf_env, rp = 8, rp = 8)),
        class = "eplusr_error_duplicated_name"
    )

    # can stop if input new names are the same as existing ones
    expect_error(
        make_idf_object_name(idd_env, idf_env, expand_idf_dots_name(idd_env, idf_env, "floor" = "floor")),
        class = "eplusr_error_conflict_name"
    )

    # can use additional columns as prefixes
    expect_equal(
        {
            obj <- init_idf_object(idd_env, idf_env, rep("Construction", 2), name = FALSE)
            set(obj, 1L, "object_name", "Construction")
            set(obj, 1L, "object_name_lower", "construction")
            set(obj, NULL, "prefix1", "Con")
            set(obj, NULL, "prefix2", "Const")
            make_idf_object_name(idd_env, idf_env, obj, prefix_col = c("prefix1", "prefix2"), prefix_sep = "-", keep_na = FALSE)[]
        },
        data.table(rleid = 1:2, class_id = 90L, class_name = "Construction",
            group_id = 5L, object_id = 54:55,
            object_name = c("Construction", NA), object_name_lower = c("construction", NA),
            comment = list(),
            prefix1 = "Con", prefix2 = "Const",
            new_object_name = paste0("Con-Const-Construction", c("", " 1")),
            new_object_name_lower = paste0("con-const-construction", c("", " 1"))
        )
    )

    # can use additional columns as prefixes and keep empty names
    expect_equal(
        {
            obj <- init_idf_object(idd_env, idf_env, rep("Construction", 2), name = FALSE)
            set(obj, 1L, "object_name", "Construction")
            set(obj, 1L, "object_name_lower", "construction")
            set(obj, NULL, "prefix1", "Con")
            set(obj, NULL, "prefix2", "Const")
            make_idf_object_name(idd_env, idf_env, obj, prefix_col = c("prefix1", "prefix2"), prefix_sep = "-", keep_na = TRUE)[]
        },
        data.table(rleid = 1:2, class_id = 90L, class_name = "Construction",
            group_id = 5L, object_id = 54:55,
            object_name = c("Construction", NA), object_name_lower = c("construction", NA),
            comment = list(),
            prefix1 = "Con", prefix2 = "Const",
            new_object_name = c("Con-Const-Construction", NA),
            new_object_name_lower = c("con-const-construction", NA)
        )
    )

    # can use additional columns as prefixes and keep empty names
    expect_equal(
        {
            obj <- init_idf_object(idd_env, idf_env, rep("Construction", 2), name = FALSE)
            set(obj, NULL, "prefix1", "1")
            set(obj, NULL, "prefix2", "2")
            make_idf_object_name(idd_env, idf_env, obj, prefix_col = c("prefix1", "prefix2"), prefix_sep = "-", use_old = FALSE)
        },
        data.table(rleid = 1:2, class_id = 90L, class_name = "Construction",
            group_id = 5L, object_id = 54:55,
            object_name = NA_character_, object_name_lower = NA_character_,
            comment = list(), prefix1 = "1", prefix2 = "2",
            new_object_name = c("1-2-Construction", "1-2-Construction 1"),
            new_object_name_lower = c("1-2-construction", "1-2-construction 1")
        )
    )

    # can keep existing new names
    expect_equal(
        {
            obj <- init_idf_object(idd_env, idf_env, rep("Construction", 2), name = FALSE)
            set(obj, 1L, "object_name", "Construction")
            set(obj, 1L, "object_name_lower", "construction")
            set(obj, 1L, "new_object_name", "Const")
            set(obj, 1L, "new_object_name_lower", "const")
            make_idf_object_name(idd_env, idf_env, obj, include_ori = FALSE)
        },
        data.table(rleid = 1:2, class_id = 90L, class_name = "Construction",
            group_id = 5L, object_id = 54:55,
            object_name = c("Construction", NA), object_name_lower = c("construction", NA),
            comment = list(),
            new_object_name = c("Const", NA),
            new_object_name_lower = c("const", NA)
        )
    )

    # can auto name and keep empty name
    expect_equal(
        {
            obj <- init_idf_object(idd_env, idf_env, c(rep("Construction", 3), "Coil:Cooling:Water"), name = FALSE)
            set(obj, 1L, "object_name", "Const")
            set(obj, 1L, "object_name_lower", "const")
            make_idf_object_name(idd_env, idf_env, obj, keep_na = FALSE)
        },
        data.table(rleid = 1:4, class_id = c(rep(90L, 3), 390L),
            class_name = c(rep("Construction", 3), "Coil:Cooling:Water"),
            group_id = c(rep(5L, 3), 23L), object_id = 54:57,
            object_name = c("Const", rep(NA_character_, 3)),
            object_name_lower = c("const", rep(NA_character_, 3)),
            comment = list(),
            new_object_name = c("Const", "Construction", "Construction 1", "Coil"),
            new_object_name_lower = c("const", "construction", "construction 1", "coil")
        )
    )
})
# }}}

# DUP {{{
test_that("Dup", {
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    # can stop if version object
    expect_error(dup_idf_object(idd_env, idf_env, expand_idf_dots_name(idd_env, idf_env, 1)), class = "eplusr_error_dup_version")
    # can stop if duplicate unique object
    expect_error(dup_idf_object(idd_env, idf_env, expand_idf_dots_name(idd_env, idf_env, 3)), class = "eplusr_error_dup_unique")

    expect_message(with_verbose(
        dup <- dup_idf_object(idd_env, idf_env, expand_idf_dots_name(idd_env, idf_env, 8, Annual = 8, nomass = 13, 13))),
        "RunPeriod.*R31LAYER 1"
    )
    expect_is(dup, "list")
    expect_equal(names(dup), c("object", "value", "reference", "changed", "updated"))
    expect_equal(nrow(dup$object), 57)
    expect_equal(dup$object[54:57],
        data.table(
            object_id = 54:57,
            object_name = c("RunPeriod", "Annual", "nomass", "R31LAYER 1"),
            object_name_lower = c("runperiod", "annual", "nomass", "r31layer 1"),
            comment = list(),
            class_id = c(22L, 22L, 56L, 56L)
        )
    )
    expect_equal(nrow(dup$value), 382)
    expect_equal(dup$value[349:382],
        data.table(
          value_id = 349:382,
          value_chr = c(
              "RunPeriod", "1", "1", "12", "31", "Tuesday", "Yes", "Yes", "No", "Yes", "Yes",
              "Annual", "1", "1", "12", "31", "Tuesday", "Yes", "Yes", "No", "Yes", "Yes",
              "nomass", "Rough", "5.456", "0.9", "0.75", "0.75",
              "R31LAYER 1", "Rough", "5.456", "0.9", "0.75", "0.75"),
          value_num = c(
              NA, 1, 1, 12, 31, NA, NA, NA, NA, NA, NA,
              NA, 1, 1, 12, 31, NA, NA, NA, NA, NA, NA,
              NA, NA, 5.456, 0.9, 0.75, 0.75,
              NA, NA, 5.456, 0.9, 0.75, 0.75),
            object_id = c(rep(54L, 11), rep(55L, 11), rep(56L, 6), rep(57L, 6)),
            field_id = c(104:114, 104:114, 7090:7095, 7090:7095)
        )
    )
    expect_equal(nrow(dup$reference), 21)
    expect_equal(dup$changed, 54:57)
    expect_equal(dup$updated, integer())
})
# }}}

# ADD {{{
test_that("Add", {
    # read idf
    idf <- read_idf(example(), 8.8)
    idf_env <- get_priv_env(idf)$m_idf_env
    idd_env <- get_priv_env(idf)$idd_env()

    # can stop if adding version
    expect_error(
        {
            l <- expand_idf_dots_value(idd_env, idf_env, Version = list())
            add_idf_object(idd_env, idf_env, l$object, l$value)
        }, class = "eplusr_error_add_version")

    # can stop if adding existing unique object
    expect_error(
        {
            l <- expand_idf_dots_value(idd_env, idf_env, Building = list())
            add_idf_object(idd_env, idf_env, l$object, l$value)
        }, class = "eplusr_error_add_unique")

    # can stop if adding existing unique object
    expect_error(
        {
            l <- expand_idf_dots_value(idd_env, idf_env, c(rep("Output:SQLite", 2)) := list(), .unique = FALSE)
            add_idf_object(idd_env, idf_env, l$object, l$value)
        }, class = "eplusr_error_add_unique")

    # can stop if malformed field values
    expect_error(
        {
            l <- expand_idf_dots_value(idd_env, idf_env, Material := list(1), .unique = FALSE)
            add_idf_object(idd_env, idf_env, l$object, l$value)
        }, class = "eplusr_error_validity_check")

    # can remove input objects that are the same as existing ones
    expect_is(class = "list",
        {
            l <- expand_idf_dots_value(idd_env, idf_env, floor = list(), .type = "object")
            l <- add_idf_object(idd_env, idf_env, l$object, l$value, level = "none", unique = TRUE)
        }
    )
    expect_equal(nrow(l$object), 53)
    expect_equal(nrow(l$value), 348)
    expect_equal(nrow(l$reference), 21)
    expect_equal(l$changed, integer())
    expect_equal(l$updated, integer())

    # can handle references
    expect_equal(
        {
            l <- expand_idf_dots_value(idd_env, idf_env,
                Construction = list("ROOF13", "R13LAYER"),
                Construction = list("NewConst", "NewMat"),
                Material = list("NewMat"), .unique = FALSE
            )
            add_idf_object(idd_env, idf_env, l$object, l$value, level = custom_validate(reference = TRUE))$reference[22:23]
        },
        data.table(object_id = 54:55, value_id = c(350L, 352L),
            src_object_id = c(12L, 56L), src_value_id = c(87L, 353L), src_enum = 2L)
    )

    # whole game
    expect_is(class = "list",
        {
            l <- expand_idf_dots_value(idd_env, idf_env,
                Material := list(paste("Mat", 1:3)),
                Construction = list("Const", "Mat1", "Mat2", "Mat3"),
                BuildingSurface_Detailed = list("Surf", "Floor", "Const", "Zone"),
                Zone = list("Zone"),
                .scalar = FALSE, .pair = TRUE, .empty = TRUE, .unique = FALSE
            )
            l <- add_idf_object(idd_env, idf_env, l$object, l$value, level = "none", unique = TRUE)
        }
    )
    expect_equal(l$object[54:59],
        object = data.table(
            object_id = 54:59,
            object_name = c("Mat 1", "Mat 2", "Mat 3", "Const", "Surf", "Zone"),
            object_name_lower = c("mat 1", "mat 2", "mat 3", "const", "surf", "zone"),
            comment = list(),
            class_id = c(55L, 55L, 55L, 90L, 103L, 100L)
        )
    )
    expect_equal(l$value[349:390],
        data.table(
            value_id = 349:390,
            value_chr = c(
                "Mat 1", NA, NA, NA, NA, NA,
                "Mat 2", NA, NA, NA, NA, NA,
                "Mat 3", NA, NA, NA, NA, NA,
                "Const", "Mat1", "Mat2", "Mat3",
                "Surf", "Floor", "Const", "Zone", NA, NA, "SunExposed",
                "WindExposed", "autocalculate", "autocalculate",
                NA, NA, NA, NA, NA, NA, NA, NA, NA, "Zone"),
            value_num = NA_real_,
            object_id = c(rep(54L, 6), rep(55L, 6), rep(56L, 6), rep(57L, 4), rep(58L, 19), 59L),
            field_id = c(rep(7081:7086, 3), 11006:11009, 11622:11640, 11105L)
        )
    )
    expect_equal(l$reference[22:26],
        data.table(
            object_id = c(57L, 57L, 57L, 58L, 58L),
            value_id = c(368L, 369L, 370L, 373L, 374L),
            src_object_id = c(NA, NA, NA, 57L, 59L),
            src_value_id = c(NA, NA, NA, 367L, 390L),
            src_enum = c(NA, NA, NA, 2L, 2L)
        )
    )
    expect_equal(l$changed, 54:59)
    expect_equal(l$updated, integer())
})
# }}}

# SET {{{
test_that("Set", {
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    # can stop if modifying version
    expect_error(
        {
            l <- expand_idf_dots_value(idd_env, idf_env, ..1 = list(), .type = "object")
            set_idf_object(idd_env, idf_env, l$object, l$value)
        },
        class = "eplusr_error_set_version"
    )

    # can stop if modifying multiple times
    expect_error(
        {
            l <- expand_idf_dots_value(idd_env, idf_env, Zone := list(), `zone one` = list(), .type = "object", .unique = FALSE)
            set_idf_object(idd_env, idf_env, l$object, l$value)
        },
        class = "eplusr_error_set_same"
    )

    expect_is(class = "list",
        {
            l <- expand_idf_dots_value(idd_env, idf_env, ..8 = list(Name = "Test"), .type = "object")
            rp <- set_idf_object(idd_env, idf_env, l$object, l$value)
        }
    )
    expect_equal(nrow(rp$object), 53L)
    expect_equal(rp$object$object_id[8], 8L)
    expect_equal(rp$object$object_name[8], "Test")
    expect_equal(rp$object$object_name_lower[8], "test")
    expect_equal(nrow(rp$value), 348L)
    expect_equal(rp$value$value_chr[19L], "Test")
    expect_equal(nrow(rp$reference), 21L)
    expect_equal(rp$changed, 8L)
    expect_equal(rp$updated, integer())

    expect_is(class = "list",
        {
            l <- expand_idf_dots_value(idd_env, idf_env, FLOOR = list(Name = "Flr"), .type = "object")
            floor <- set_idf_object(idd_env, idf_env, l$object, l$value)
        }
    )
    expect_equal(nrow(floor$object), 53L)
    expect_equal(floor$object$object_id[16], 16)
    expect_equal(floor$object$object_name[16], "Flr")
    expect_equal(floor$object$object_name_lower[16], "flr")
    expect_equal(nrow(floor$value), 348)
    expect_equal(floor$value$value_chr[220L], "Flr")
    expect_equal(floor$reference[20:21],
        data.table(object_id = c(16L, 25L), value_id = c(111L, 220L),
            src_object_id = c(14L, 16L), src_value_id = c(99L, 110L),
            src_enum = 2L
        )
    )
    expect_equal(floor$changed, 16L)
    expect_equal(floor$updated, 25L)

    # delete fields
    expect_is(class = "list",
        {
            l <- expand_idf_dots_value(idd_env, idf_env, ..8 = list(name = "name", start_year = NULL), .type = "object", .default = FALSE)
            rp <- set_idf_object(idd_env, idf_env, l$object, l$value)
        }
    )
    expect_equal(nrow(rp$object), 53)
    expect_equal(rp$object$object_id[8], 8L)
    expect_equal(rp$object$object_name[8], "name")
    expect_equal(rp$object$object_name_lower[8], "name")
    expect_equal(nrow(rp$value), 348L)
    expect_equal(rp$value$value_chr[19L], "name")
    expect_equal(nrow(rp$reference), 21)
    expect_equal(rp$changed, 8L)
    expect_equal(rp$updated, integer())

    expect_is(class = "list",
        {
            l <- expand_idf_dots_value(idd_env, idf_env, ..14 = list(visible_absorptance = NULL), .type = "object", .default = FALSE)
            mat <- set_idf_object(idd_env, idf_env, l$object, l$value)
        }
    )
    expect_equal(nrow(get_idf_value(idd_env, mat, object = 14)), 8)

    # can set whole class
    expect_is(class = "list",
        {
            l <- expand_idf_dots_value(idd_env, idf_env, .type = "object",
                Material_NoMass := list(roughness = "smooth", thermal_absorptance = 0.8)
            )
            mat <- set_idf_object(idd_env, idf_env, l$object, l$value)
        }
    )
    expect_equal(nrow(mat$object), 53L)
    expect_equal(mat$object$object_id[12:13], 12:13)
    expect_equal(mat$object$object_name[12:13], c("R13LAYER", "R31LAYER"))
    expect_equal(mat$object$object_name_lower[12:13], c("r13layer", "r31layer"))
    expect_equal(get_idf_value(idd_env, mat, "Material:NoMass", field = "roughness")$value_chr, rep("smooth", 2))
    expect_equal(get_idf_value(idd_env, mat, "Material:NoMass", field = "thermal_absorptance")$value_num, rep(0.8, 2))
    expect_equal(mat$reference[20:21],
        data.table(object_id = c(15L, 17L), value_id = c(109L, 113L),
            src_object_id = c(12L, 13L), src_value_id = c(87L, 93L),
            src_enum = 2L
        )
    )
    expect_equal(mat$changed, 12:13)
    expect_equal(mat$updated, c(15L, 17L))

    # can handle references
    expect_is(class = "list",
        {
            l <- expand_idf_dots_value(idd_env, idf_env, .type = "object",
                ROOF31 = list(outside_layer = "R13LAYER"),
                FLOOR = list(outside_layer = "NoSuchMaterial")
            )
            l <- set_idf_object(idd_env, idf_env, l$object, l$value, level = "none")
        }
    )
    expect_equal(l$reference[19:21],
        data.table(
            object_id = c(17L, 16L, 26L),
            value_id = c(113L, 111L, 242L),
            src_object_id = c(12L, NA, 17L),
            src_value_id = c(87L, NA, 112L),
            src_enum = c(2L, NA, 2L)
        )
    )
    expect_equal(l$changed, c(17L, 16L))
    expect_equal(l$updated, 26L)
})
# }}}

# DEL {{{
test_that("Del", {
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    expect_error(del_idf_object(idd_env, idf_env, get_idf_object(idd_env, idf_env, "Version")), class = "eplusr_error_del_version")
    expect_error(del_idf_object(idd_env, idf_env, get_idf_object(idd_env, idf_env, object = 3)), class = "eplusr_error_del_required")
    expect_error(del_idf_object(idd_env, idf_env, get_idf_object(idd_env, idf_env, object = 7)), class = "eplusr_error_del_unique")
    expect_error(del_idf_object(idd_env, idf_env, get_idf_object(idd_env, idf_env, object = rep(53, 2))), class = "eplusr_error_del_same")
    expect_error(del_idf_object(idd_env, idf_env, get_idf_object(idd_env, idf_env, object = c("R13WALL", "FLOOR", "ROOF31"))), class = "eplusr_error_del_referenced")
    expect_message({del <- with_verbose(del_idf_object(idd_env, idf_env, get_idf_object(idd_env, idf_env, object = c(21:26, 14)), ref_to = TRUE, ref_by = TRUE, recursive = TRUE))}, "relation")
    expect_equal(setdiff(idf_env$object$object_id, del$object$object_id), c(14:17, 21:26))
    expect_equal(del$changed, c(21:26, 14:17))
    expect_equal(del$updated, integer())

    expect_message({
        obj <- get_idf_object(idd_env, idf_env, object = "R13LAYER")
        with_verbose(del_idf_object(idd_env, idf_env, obj, ref_by = TRUE))
    }, "Skipping")

    expect_message({
        env <- list2env(idf_env)
        l <- expand_idf_dots_value(idd_env, env, Construction = list("Const", "R13LAYER"))
        add <- add_idf_object(idd_env, env, l$object, l$value, level = "final")
        obj <- get_idf_object(idd_env, add, object = "R13LAYER")
        with_verbose(del_idf_object(idd_env, add, obj, ref_by = TRUE))
    }, "Including")

    expect_message({
        env <- list2env(idf_env)
        l <- expand_idf_dots_value(idd_env, env, Construction = list("Const", "R13LAYER"))
        add <- add_idf_object(idd_env, env, l$object, l$value, level = "final")
        obj <- get_idf_object(idd_env, add, object = "R13LAYER")
        with_verbose(del_idf_object(idd_env, add, obj, ref_by = TRUE, force = TRUE))
    }, "Including")

    expect_message({
        obj <- expand_idf_dots_name(idd_env, env, mat = "R13LAYER")
        dup <- list2env(dup_idf_object(idd_env, idf_env, obj, "final"))
        l <- expand_idf_dots_value(idd_env, dup, Construction = list("Const", "mat"))
        add <- add_idf_object(idd_env, dup, l$object, l$value, level = "final")
        obj <- get_idf_object(idd_env, add, object = "Const")
        with_verbose(del_idf_object(idd_env, add, obj, ref_to = TRUE))
    }, "Including")

    expect_message({
        obj <- expand_idf_dots_name(idd_env, env, mat = "R13LAYER")
        dup <- list2env(dup_idf_object(idd_env, env, obj, "final"))
        l <- expand_idf_dots_value(idd_env, dup, Construction = list("Const", "mat"))
        add <- add_idf_object(idd_env, dup, l$object, l$value, level = "final")
        obj <- get_idf_object(idd_env, add, object = "Const")
        with_verbose(del_idf_object(idd_env, add, obj, ref_to = TRUE, force = TRUE))
    }, "Including")
})
# }}}

# PURGE {{{
test_that("Purge", {
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    expect_message(pu <- with_verbose(purge_idf_object(idd_env, idf_env, get_idf_object(idd_env, idf_env, "SimulationControl"))), "ignored")
    expect_equal(pu$object, idf_env$object)
    expect_equal(pu$value, idf_env$value)
    expect_equal(pu$reference, idf_env$reference)
    expect_equal(pu$changed, integer())
    expect_equal(pu$updated, integer())

    expect_is(pu <- purge_idf_object(idd_env, idf_env, get_idf_object(idd_env, idf_env, "Material:NoMass")), "list")
    expect_equal(pu$object, idf_env$object)
    expect_equal(pu$value, idf_env$value)
    expect_equal(pu$reference, idf_env$reference)
    expect_equal(pu$changed, integer())
    expect_equal(pu$updated, integer())

    expect_is(pu <- purge_idf_object(idd_env, idf_env, get_idf_object(idd_env, idf_env, "RunPeriod")), "list")
    expect_equal(setdiff(idf_env$object$object_id, pu$object$object_id), 8L)
    expect_equal(nrow(pu$value), 337L)
    expect_equal(pu$reference, idf_env$reference)
    expect_equal(pu$changed, 8L)
    expect_equal(pu$updated, integer())
})
# }}}

# DUPLICATED {{{
test_that("Duplicated", {
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    l <- dup_idf_object(idd_env, idf_env, get_idf_object(idd_env, idf_env, "SimulationControl"), "none")
    expect_equal(duplicated_idf_object(idd_env, l, get_idf_object(idd_env, l))$unique_object_id, c(rep(NA, 53), 7L))
})
# }}}

# UNIQUE {{{
test_that("Unique", {
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    expect_message(with_verbose(unique_idf_object(idd_env, idf_env, get_idf_object(idd_env, idf_env))), "Skip")

    # change references from the original ones to duplicated ones
    ori_obj <- get_idf_object(idd_env, idf_env, "Material:NoMass")
    ori_val <- get_idf_value(idd_env, idf_env, "Material:NoMass")
    l <- dup_idf_object(idd_env, idf_env, ori_obj)
    new_val <- data.table::fsetdiff(l$value, idf_env$value)
    ref <- set(ori_val[, list(object_id, value_id)], NULL,
        c("new_object_id", "new_value_id"), new_val[, list(object_id, value_id)]
    )
    l$reference[ref, on = c("src_value_id" = "value_id"),
        `:=`(src_object_id = i.new_object_id, src_value_id = i.new_value_id)]

    expect_message(l <- with_verbose(unique_idf_object(idd_env, l, get_idf_object(idd_env, l))),
        "have been removed"
    )
    expect_equivalent(l$object, idf_env$object)
    expect_equivalent(l$value, idf_env$value)
    expect_equivalent(l$reference, idf_env$reference)
    expect_equivalent(l$changed, 54:55)
    expect_equivalent(l$updated, c(15L, 17L))
})
# }}}

# RENAME {{{
test_that("Rename", {
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    # can stop if try to rename same object multiple times
    expect_error(
        {
            obj <- expand_idf_dots_name(idd_env, idf_env, Floor = "floor", Floo1 = "floor")
            rename_idf_object(idd_env, idf_env, obj)
        },
        class = "eplusr_error_rename_same"
    )

    # can stop if no new names are given
    expect_error(
        {
            obj <- expand_idf_dots_name(idd_env, idf_env, "floor", "zone one", .keep_name = FALSE)
            rename_idf_object(idd_env, idf_env, obj)
        },
        class = "eplusr_error_rename_no_new_name"
    )
    expect_error(
        {
            obj <- expand_idf_dots_name(idd_env, idf_env, "floor", "zone one")
            rename_idf_object(idd_env, idf_env, obj)
        },
        class = "eplusr_error_rename_no_new_name"
    )

    # can stop if try to assign names to objects without name attribute
    expect_error(
        {
            obj <- expand_idf_dots_name(idd_env, idf_env, version = 1)
            rename_idf_object(idd_env, idf_env, obj)
        },
        class = "eplusr_error_cannot_name"
    )

    # can stop if new name has been used by other objects in the same class
    expect_error(
        {
            obj <- expand_idf_dots_name(idd_env, idf_env, Floor = "floor")
            rename_idf_object(idd_env, idf_env, obj)
        },
        class = "eplusr_error_conflict_name"
    )

    expect_is(class = "list",
        {
            obj <- expand_idf_dots_name(idd_env, idf_env, r13 = "R13WALL", flr = "FLOOR", roof = "ROOF31", r31 = "R31LAYER")
            l <- rename_idf_object(idd_env, idf_env, obj)
        }
    )
    expect_is(get_idf_object(idd_env, l, object = c("r13", "flr", "roof", "r31")), "data.table")
    expect_equal(get_idf_value(idd_env, l, object = c("r13", "flr", "roof", "r31"), field = rep(1, 4))$value_chr,
        c("r13", "flr", "roof", "r31"))
    expect_equal(nrow(data.table::fsetdiff(l$reference, idf_env$reference)), 0)
    expect_equal(
        {
            id <- get_idf_value(idd_env, l, object = c("r13", "flr", "roof", "r31"), field = rep(1, 4))$value_id
            id <- l$reference[J(id), on = "src_value_id", value_id]
            idf_env$value[J(id), on = "value_id", value_chr]
        },
        c(rep("r13", 4), "flr", "roof", "r31")
    )
    expect_equal(l$changed, c(15:17, 13L))
    expect_equal(l$updated, 21:26)
})
# }}}

# REMOVE {{{
test_that("Remove", {
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    # REMOVE FIELDS
    # can work if no trailing empty fields are found
    expect_equal(nrow(remove_empty_fields(idd_env, idf_env, get_idf_value(idd_env, idf_env, "SimulationControl"))), 5L)
    # can work for non-extensible fields
    val <- get_idf_value(idd_env, idf_env, "Material")[field_index > 7L,
        `:=`(value_chr = NA_character_, value_num = NA_real_)]
    expect_equal(nrow(remove_empty_fields(idd_env, idf_env, val)), 7L)
    # can work for extensible fields
    val <- get_idf_value(idd_env, idf_env, object = "Zn001:Wall001", field = 24, complete = TRUE)
    ## (a) can skip if extensible group is incomplete
    expect_equal(nrow(remove_empty_fields(idd_env, idf_env, val[field_index <= 24L])), 24L)
    ## (b) can remove if all extensible fields in a extensible group are empty
    expect_equal(nrow(remove_empty_fields(idd_env, idf_env, val)), 22L)
    ## (c) can skip if not all extensible fields in a group are empty
    expect_equal(nrow(remove_empty_fields(idd_env, idf_env, val[field_index == 24L, value_chr := "1"])), 25L)

    # REMOVE OBJECTS
    l <- list()
    l1 <- expand_idf_dots_value(idd_env, idf_env, "Site:WeatherStation" = list())
    expect_is(rev <- remove_duplicated_objects(idd_env, idf_env, l1$object, l1$value), "list")
    expect_equal(l1$object, rev$object)
    expect_equal(l1$value, rev$value)
    l2 <- dup_idf_object(idd_env, idf_env, get_idf_object(idd_env, idf_env, object = rep("Zn001:Wall001", 2L)))
    l$object <- rbindlist(list(
        l1$object,
        get_idf_object(idd_env, l2, object = 54:55)[,
            `:=`(object_name = stri_sub(object_name, to = -3),
                 object_name_lower = stri_sub(object_name_lower, to = -3))]
    ))
    l$value <- rbindlist(list(
        l1$value,
        get_idf_value(idd_env, l2, object = 54:55)[,
            `:=`(object_name = stri_sub(object_name, to = -3))][
            field_index == 1L, value_chr := stri_sub(value_chr, to = -3)]
    ))
    expect_message(with_verbose(rev <- remove_duplicated_objects(idd_env, idf_env, l$object, l$value)), "removed")
    expect_equal(rev$object$class_name, "Site:WeatherStation")
    expect_equal(nrow(rev$value), 4L)
})
# }}}

# IDF EDITOR {{{
test_that("Parsing IDF EDITOR Copy Contents", {
    skip_if_not(is_windows())

    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    text <- "IDF,BuildingSurface:Detailed,Surface,Wall,R13WALL,ZONE ONE,Outdoors,,SunExposed,WindExposed,0.5000000,4,0,0,4.572000,0,0,0,15.24000,0,0,15.24000,0,4.572000,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,;"
    writeClipboard(text)
    expect_is(l <- read_idfeditor_copy(idd_env, idf_env), "list")
    expect_equal(l$object,
        data.table(rleid = 1L, class_id = 103L, class_name = "BuildingSurface:Detailed",
            object_id = 1L, object_name = "Surface", object_name_lower = "surface",
            comment = list(NULL)
        )
    )
    expect_equal(l$value,
        data.table(
            rleid = 1L, class_id = 103L, class_name = "BuildingSurface:Detailed",
            object_id = 1L, object_name = "Surface", field_id = 11622:11643,
            field_index = 1:22,
            field_name = c("Name", "Surface Type", "Construction Name", "Zone Name",
                "Outside Boundary Condition", "Outside Boundary Condition Object",
                "Sun Exposure", "Wind Exposure", "View Factor to Ground", "Number of Vertices",
                "Vertex 1 X-coordinate", "Vertex 1 Y-coordinate", "Vertex 1 Z-coordinate",
                "Vertex 2 X-coordinate", "Vertex 2 Y-coordinate", "Vertex 2 Z-coordinate",
                "Vertex 3 X-coordinate", "Vertex 3 Y-coordinate", "Vertex 3 Z-coordinate",
                "Vertex 4 X-coordinate", "Vertex 4 Y-coordinate", "Vertex 4 Z-coordinate"),
            value_id = 1:22,
            value_chr = c("Surface", "Wall", "R13WALL", "ZONE ONE", "Outdoors",
                NA, "SunExposed", "WindExposed", "0.5", "4", "0", "0", "4.572",
                "0", "0", "0", "15.24", "0", "0", "15.24", "0", "4.572"),
            value_num = c(NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 4, 0, 0, 4.572,
                0, 0, 0, 15.24, 0, 0, 15.24, 0, 4.572)
        )
    )
    expect_equal(l$reference, idf_env$reference[0L])
})
# }}}

# TO_TABLE {{{
test_that("to_table", {
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    expect_equivalent(get_idf_table(idd_env, idf_env, "Material"),
        data.table(id = 14L, name = "C5 - 4 IN HW CONCRETE", class = "Material",
            index = 1:9,
            field = c(
                "Name", "Roughness", "Thickness", "Conductivity", "Density",
                "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
                "Visible Absorptance"
            ),
            value = c(
                "C5 - 4 IN HW CONCRETE", "MediumRough", "0.1014984", "1.729577",
                "2242.585", "836.8", "0.9", "0.65", "0.65"
            )
        )
    )
    expect_equivalent(get_idf_table(idd_env, idf_env, "Material", string_value = FALSE),
        data.table(id = 14L, name = "C5 - 4 IN HW CONCRETE", class = "Material",
            index = 1:9,
            field = c(
                "Name", "Roughness", "Thickness", "Conductivity", "Density",
                "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
                "Visible Absorptance"
            ),
            value = list(
                "C5 - 4 IN HW CONCRETE", "MediumRough", 0.1014984, 1.729577,
                2242.585, 836.8, 0.9, 0.65, 0.65
            )
        ), tolerance = 1e-5
    )
    expect_equivalent(get_idf_table(idd_env, idf_env, "Material", string_value = FALSE, unit = TRUE),
        data.table(id = 14L, name = "C5 - 4 IN HW CONCRETE", class = "Material",
            index = 1:9,
            field = c(
                "Name", "Roughness", "Thickness", "Conductivity", "Density",
                "Specific Heat", "Thermal Absorptance", "Solar Absorptance",
                "Visible Absorptance"
            ),
            value = list(
                "C5 - 4 IN HW CONCRETE", "MediumRough",
                units::set_units(0.1014984, "m"),
                units::set_units(1.729577, "W/K/m"),
                units::set_units(2242.585, "kg/m^3"),
                units::set_units(836.8, "J/K/kg"),
                0.9, 0.65, 0.65
            )
        ), tolerance = 1e-5
    )
    expect_equivalent(get_idf_table(idd_env, idf_env, "Material", string_value = FALSE, unit = TRUE, wide = TRUE),
        data.table(id = 14L, name = "C5 - 4 IN HW CONCRETE", class = "Material",
            "Name" = "C5 - 4 IN HW CONCRETE",
            "Roughness" = "MediumRough",
            "Thickness" = units::set_units(0.1014984, "m"),
            "Conductivity" = units::set_units(1.729577, "W/K/m"),
            "Density" = units::set_units(2242.585, "kg/m^3"),
            "Specific Heat" = units::set_units(836.8, "J/K/kg"),
            "Thermal Absorptance" = 0.9,
            "Solar Absorptance" = 0.65,
            "Visible Absorptance" = 0.65
        ), tolerance = 1e-5
    )
    expect_equivalent(get_idf_table(idd_env, idf_env, "Material", string_value = FALSE, unit = TRUE, wide = TRUE, group_ext = "group"),
        data.table(id = 14L, name = "C5 - 4 IN HW CONCRETE", class = "Material",
            "Name" = "C5 - 4 IN HW CONCRETE",
            "Roughness" = "MediumRough",
            "Thickness" = units::set_units(0.1014984, "m"),
            "Conductivity" = units::set_units(1.729577, "W/K/m"),
            "Density" = units::set_units(2242.585, "kg/m^3"),
            "Specific Heat" = units::set_units(836.8, "J/K/kg"),
            "Thermal Absorptance" = 0.9,
            "Solar Absorptance" = 0.65,
            "Visible Absorptance" = 0.65
        ), tolerance = 1e-5
    )

    expect_error(get_idf_table(idd_env, idf_env, wide = TRUE), class = "eplusr_error")
    expect_error(get_idf_table(idd_env, idf_env, idf_env$object[, unique(class_id)][1:4], wide = TRUE), class = "eplusr_error")
    expect_equal(get_idf_table(idd_env, idf_env, 1, string_value = FALSE)$value, list("8.8"))

    expect_is(val <- get_idf_table(idd_env, idf_env, object = get_idf_object(idd_env, idf_env, "BuildingSurface:Detailed")$object_id[1:2],
            string_value = FALSE, wide = TRUE, group_ext = "group"),
        "data.table"
    )
    expect_equal(names(val)[14:ncol(val)], sprintf("Vrtx%sX-crd|Vrtx%sY-crd|Vrtx%sZ-crd", 1:4, 1:4, 1:4))
    expect_equal(val[["Vrtx3X-crd|Vrtx3Y-crd|Vrtx3Z-crd"]], list(list(15.24, 0., 0.), list(15.24, 15.24, 0.)))

    expect_is(val <- get_idf_table(idd_env, idf_env, object = get_idf_object(idd_env, idf_env, "BuildingSurface:Detailed")$object_id[1:2],
            string_value = FALSE, wide = TRUE, group_ext = "index"),
        "data.table"
    )
    expect_equal(names(val)[14:ncol(val)], sprintf("Vertex %s-coordinate", c("X", "Y", "Z")))
    expect_equal(val[["Vertex X-coordinate"]], list(c(0., 0., 15.24, 15.24), c(15.24, 15.24, 15.24, 15.24)))

    # can init object value table
    expect_is(val <- get_idf_table(idd_env, idf_env, "Material", init = TRUE, all = TRUE), "data.table")
    expect_equal(nrow(val), 9)
    expect_equal(val$value, c(rep(NA, 6), ".9", ".7", ".7"))
})
# }}}

# DT_TO_LOAD {{{
test_that("dt_to_load", {
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    dt_long <- get_idf_table(idd_env, idf_env, "Material")
    dt_wide <- get_idf_table(idd_env, idf_env, "Material", wide = TRUE)
    expect_equivalent(dt_to_load(dt_wide), dt_long)

    dt_long <- get_idf_table(idd_env, idf_env, "Material", string_value = FALSE)
    dt_wide <- get_idf_table(idd_env, idf_env, "Material", string_value = FALSE, wide = TRUE)
    expect_equivalent(dt_to_load(dt_wide, FALSE), dt_long)
})
# }}}

# TO_STRING {{{
test_that("to_string", {
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    expect_equal(length(get_idf_string(idd_env, idf_env)), 634)
    expect_equal(length(get_idf_string(idd_env, idf_env, comment = FALSE)), 541)

    expect_equal(length(get_idf_string(idd_env, idf_env, idf_env$object[0, list(object_id, object_order = integer())], format = "new_top")), 553)
    expect_equal(length(get_idf_string(idd_env, idf_env, idf_env$object[0, list(object_id, object_order = integer())], format = "new_top", comment = FALSE)), 460)

    expect_equal(length(get_idf_string(idd_env, idf_env, class = "Version")), 97)
    expect_equal(length(get_idf_string(idd_env, idf_env, class = "Version", comment = FALSE)), 12)
    expect_equal(length(get_idf_string(idd_env, idf_env, class = "Version", comment = FALSE, header = FALSE)), 5)
    expect_equal(length(get_idf_string(idd_env, idf_env, class = "Material", header = FALSE, in_ip = TRUE)), 13L)
    expect_equal(length(get_idf_string(idd_env, idf_env, idf_env$object[0, list(object_id, object_order = integer())], class = "Material", header = FALSE, format = "new_top")), 11)
})
# }}}

# SAVE {{{
test_that("Save", {
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    expect_error(class = "eplusr_error_idf_save_ext",
        save_idf(idd_env, idf_env, idf_env$object[, list(object_id, object_order = 0)],
            tempfile(fileext = ".txt")
        )
    )
    f <- tempfile(fileext = ".idf")
    expect_silent(
        save_idf(idd_env, idf_env, idf_env$object[, list(object_id, object_order = 0)],
            f, format = "sorted"
        )
    )
    expect_error(class = "eplusr_error_idf_save_exist",
        save_idf(idd_env, idf_env, idf_env$object[, list(object_id, object_order = 0)],
            f, format = "sorted"
        )
    )
    expect_message(
        with_verbose(save_idf(idd_env, idf_env,
            idf_env$object[, list(object_id, object_order = 0)],
            f, format = "sorted", overwrite = TRUE
        )),
        "Replace the existing"
    )
    expect_silent(
        save_idf(idd_env, idf_env, idf_env$object[, list(object_id, object_order = 0)],
            file.path(tempdir(), basename(tempfile()), basename(tempfile(fileext = ".idf"))), format = "new_top"
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

# RESOLVE_EXTERNAL {{{
test_that("resolve external link", {
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    expect_false(resolve_idf_external_link(idd_env, idf_env))

    # add a Schedule:File object
    f <- tempfile(fileext = ".csv")
    l <- expand_idf_dots_value(idd_env, idf_env, `Schedule:File` = list("sch_file", NULL, f, 1, 0))
    l <- add_idf_object(idd_env, idf_env, l$object, l$value)

    # can give warnings if links are broken
    dir <- tempfile()
    dir.create(dir, FALSE)
    path <- file.path(dir, "test.idf")
    empty_idf(8.8)$save(path)
    expect_warning(flg <- resolve_idf_external_link(idd_env, l, path, tempfile(fileext = ".idf")), "Broken")
    expect_false(flg)

    # can keep the original link if copy is not required
    writeLines(",\n", f)
    expect_is(resolve_idf_external_link(idd_env, l, tempfile(fileext = ".idf"), path, copy = FALSE), "logical")
    expect_equal(l$value[field_id == 7074, normalizePath(value_chr)], normalizePath(f))

    expect_true(resolve_idf_external_link(idd_env, l, tempfile(fileext = ".idf"), path, copy = TRUE))
    expect_true(file.exists(file.path(dir, basename(f))))
    expect_equal(l$value[field_id == 7074, value_chr], basename(f))

    unlink(file.path(dir, basename(f)), force = TRUE)
})
# }}}

# UTILITIES {{{
test_that("utilities", {
    # read idf
    idf_env <- parse_idf_file(example(), 8.8)
    idd_env <- get_priv_env(use_idd(8.8))$idd_env()

    l <- expand_idf_dots_value(idd_env, idf_env, Building := list())
    obj <- l$object
    val <- l$value
    obj[1, object_id := 1L]
    val[3:4, value_id := 1:2]

    expect_equal(assign_new_id(idf_env, obj, "object", keep = TRUE)$object_id, 1L)
    expect_equal(assign_new_id(idf_env, obj, "object", keep = FALSE)$object_id, 54L)

    expect_equal(assign_new_id(idf_env, val, "value", keep = TRUE)$value_id, c(349:350, 1:2, 351:354))
    expect_equal(assign_new_id(idf_env, val, "value", keep = FALSE)$value_id, 349:356)

    expect_is(class = "data.table",
        def <- with_option(
            list(view_in_ip = TRUE),
            assign_idf_value_default(idd_env, idf_env,
                l$value[, `:=`(value_chr = NA_character_, value_num = NA_real_)]
            )
        )
    )
    expect_true(all(!is.na(def$value_chr)))
    expect_equal(sum(!is.na(def$value_num)), 5)

    id_ref <- get_idf_value(idd_env, idf_env, "Construction", field = 2)$value_id
    idf_env$value[J(id_ref), on = "value_id", value_chr := tolower(value_chr)]
    id_choice <- get_idf_value(idd_env, idf_env, "Material", field = "Roughness")$value_id
    idf_env$value[J(id_choice), on = "value_id", value_chr := tolower(value_chr)]

    val <- get_idf_value(idd_env, idf_env, "Construction", field = 2, property = "type_enum")
    expect_equal(standardize_idf_value(idd_env, idf_env, val)$value_chr, c("R13LAYER", "C5 - 4 IN HW CONCRETE", "R31LAYER"))

    val <- get_idf_value(idd_env, idf_env, "Material", field = 2)
    expect_equal(standardize_idf_value(idd_env, idf_env, val, type = "choice")$value_chr, "MediumRough")
})
# }}}
