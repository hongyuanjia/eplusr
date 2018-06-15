context("IDFObject method")

# idf_text {{{
idf_text <- "
    ! this is a test comment for WD01
    Material,
        WD01,                    !- Name
        MediumSmooth,            !- Roughness
        1.9099999E-02,           !- Thickness {m}
        0.1150000,               !- Conductivity {W/m-K}
        513.0000,                !- Density {kg/m3}
        1381.000,                !- Specific Heat {J/kg-K}
        0.9000000,               !- Thermal Absorptance
        0.7800000,               !- Solar Absorptance
        0.7800000;               !- Visible Absorptance

    Construction,
        WALL-1,                  !- Name
        WD01,                    !- Outside Layer
        PW03,                    !- Layer 2
        IN02,                    !- Layer 3
        GP01;                    !- Layer 4

    BuildingSurface:Detailed,
        WALL-1PF,                !- Name
        WALL,                    !- Surface Type
        WALL-1,                  !- Construction Name
        PLENUM-1,                !- Zone Name
        Outdoors,                !- Outside Boundary Condition
        ,                        !- Outside Boundary Condition Object
        SunExposed,              !- Sun Exposure
        WindExposed,             !- Wind Exposure
        0.50000,                 !- View Factor to Ground
        4,                       !- Number of Vertices
        0.0,                     !- Vertex 1 X-coordinate {m}
        0.0,                     !- Vertex 1 Y-coordinate {m}
        3.0,                     !- Vertex 1 Z-coordinate {m}
        0.0,                     !- Vertex 2 X-coordinate {m}
        0.0,                     !- Vertex 2 Y-coordinate {m}
        2.4,                     !- Vertex 2 Z-coordinate {m}
        30.5,                    !- Vertex 3 X-coordinate {m}
        0.0,                     !- Vertex 3 Y-coordinate {m}
        2.4,                     !- Vertex 3 Z-coordinate {m}
        30.5,                    !- Vertex 4 X-coordinate {m}
        0.0,                     !- Vertex 4 Y-coordinate {m}
        3.0;                     !- Vertex 4 Z-coordinate {m}
    "
# }}}
idd <- use_idd(8.8)

idf <- Idf$new(idf_text, 8.8)
ver <- idf$object_in_class("Version")
mat <- idf$object_in_class("Material")
surf <- idf$object_in_class("BuildingSurface:Detailed")
con <- idf$object_in_class("Construction")

describe("$id()", {
    it("can return right object id", expect_equal(mat$id(), c(Material = 1L)))
})

describe("$get_comment()", {
    # {{{
    it("can return comment", {
        expect_equal(mat$get_comment(), " this is a test comment for WD01")
    })
    # }}}
})

describe("$set_comment()", {
    # {{{
    it("can handle invalid input types of comment", {
        expect_error(mat$set_comment(comment = 1),
                     "comment is not a character vector")
        expect_error(mat$set_comment(comment = list("a")),
                     "comment is not a character vector")
    })

    it("can delete comments", {
        expect_equal(mat$set_comment(comment = NULL)$get_comment(), character(0))
    })

    it("can add comments when comment is NA before", {
        expect_equal(mat$set_comment(comment = c("a"))$get_comment(), "a")
        c
    })

    it("can append comments", {
        expect_equal(mat$set_comment(comment = c("b"))$get_comment(), c("a", "b"))
    })

    it("can prepend comments", {
        expect_equal(mat$set_comment(comment = c("c"), append = FALSE)$get_comment(),
                     c("c", "a", "b"))
    })

    it("can reset comments", {
        expect_equal(mat$set_comment(comment = c("d"), append = NULL)$get_comment(), "d")
    })

    it("can detect invalid `append` value", {
        expect_error(mat$set_comment(comment = c("b"), append = 1:2),
                     "`append` should be NULL or a single logical value.")
    })

    it("can wrap comment at specified `width`", {
        expect_equal(mat$set_comment(comment = c("a", "bb ccc"), append = NULL, width = 1L)$get_comment(),
                     c("a", "bb", "ccc"))
    })

    it("can detect invalid `width` value", {
        expect_error(mat$set_comment(comment = c("a"), append = NULL, width = "a"))
    })
    # }}}
})

describe("$get_value()", {
    # {{{
    index <- c(3, 1, 5)
    name <- c("Thickness", "Name")
    value <- list(Name = "WD01",
                  Roughness = "MediumSmooth",
                  Thickness = 0.0191,
                  Conductivity = 0.115,
                  Density = 513,
                  `Specific Heat` = 1381,
                  `Thermal Absorptance` = 0.9,
                  `Solar Absorptance` = 0.78,
                  `Visible Absorptance` = 0.78)

    it("can handle cases when both `index` and `name` are NULL", {
        expect_is(mat$get_value(), "list")
        expect_equivalent(mat$get_value(), value, tolerance = 1e-5)
    })

    it("can detect invaid `index` values", {
        expect_error(mat$get_value("1"),
                     "index is not a numeric or integer vector")
        expect_error(mat$get_value(c(1, 10:11)),
                     "Invalid field index found for class Material: `10` and `11`.")
    })

    it("can return subset of values in a object using `index`", {
        expect_is(mat$get_value(index), "list")
        expect_equivalent(mat$get_value(index), value[index], tolerance = 1e-5)
    })

    it("can detect invalid `name` values", {
        expect_error(mat$get_value(name = c("Thickness", "Wrong", "Name")),
                     "Invalid field name found for class Material: `Wrong`.")
    })

    it("can handle cases when both `index` and `name` are given", {
        expect_warning(mat$get_value(index = index, name = name),
                       "Both `index` and `name` are given. `name` will be ignored.")
    })

    it("can return subset of values in a object using `name`", {
        expect_equivalent(mat$get_value(name = name), value[name], tolerance = 1e-5)
    })
    # }}}
})

describe("$set_value()", {
    # {{{
    it("can stop when trying to directly modify `Version` object", {
        expect_error(ver$set_value(),
                     "Cannot modify `Version` object directly.")
    })

    it("can stop when no values are given", {
        expect_error(con$set_value(), "Please give values to set.")
    })

    it("can stop when both named values and unnamed values are given", {
        expect_error(con$set_value(name = "named", "unnamed"),
                     "Values should be either all unnamed or all named.")
    })

    it("can stop when duplicated names are given", {
        expect_error(con$set_value(name = "first", name = "second"),
                     "Duplicated field names found: `name`.")
    })

    it("can stop when invalid names are given for a non-extensible class", {
        expect_error(mat$set_value(wrong = "something"),
                     "Invalid field names found for class Material: `wrong`.")
    })

    it("can stop when invalid names are given for an extensible class", {
        expect_error(con$set_value(name = "first", wrong = "second"),
                     "Invalid field names found for class Construction: `wrong`.")
    })

    it("can stop when valid names are given, but total field values are not accepatable for an extensible class", {
        expect_error(surf$set_value(vertex_5_x_coordinate = 1,
                                    vertex_5_y_coordinate = 2))
    })

    it("can stop when total field values are acceptable but invalid names are given for an extensible class", {
        num_ori <- surf$num_fields()
        obj_ori <- surf$clone(deep = TRUE)
        expect_error(surf$set_value(vertex_5_x_coordinate = 1,
                                      vertex_5_y_coordinate = 2,
                                      vertex_5_z_wrong = 3),
                     paste0("Invalid field names found for class BuildingSurface:Detailed: ",
                            "`vertex_5_z_wrong`."))
        expect_equal(surf$num_fields(), num_ori)
        expect_equal(surf, obj_ori)
    })

    it("can add new values for extensible fields", {
        expect_silent(idf$set_options(validate_level = "draft"))
        expect_equal(idf$get_options("validate_level"), list(validate_level = "draft"))
        expect_silent(surf$set_value(vertex_5_x_coordinate = 1,
                                     vertex_5_y_coordinate = 2,
                                     vertex_5_z_coordinate = 3))
        expect_equal(surf$get_value()[23:25],
                     list(`Vertex 5 X-coordinate` = 1,
                          `Vertex 5 Y-coordinate` = 2,
                          `Vertex 5 Z-coordinate` = 3))
    })

    it("can change referenced values accordingly", {
        expect_equal(con$set_value(name = "NewWallName")$get_value(name = "Name")[[1]], "NewWallName")
        expect_equal(surf$get_value(name = "Construction Name")[[1]], "NewWallName")

        expect_equal(mat$set_value(name = "NewMaterialName")$get_value(name = "Name")[[1]], "NewMaterialName")
        expect_equal(con$get_value(name = "Outside Layer")[[1]], "NewMaterialName")
    })

    it("can stop when there are invalid references in the input", {
        expect_silent(idf$set_options(validate_level = "final"))
        expect_equal(idf$get_options("validate_level"), list(validate_level = "final"))
        expect_error(con$set_value(layer_6 = "missing"))
    })
    # }}}
})
describe("$validate() & $is_valid()", {
    # {{{
    it("can find errors of invalid reference", {
        expect_true(ver$is_valid())
        expect_true(mat$is_valid())
        expect_false(surf$is_valid())
        expect_false(con$is_valid())
    })
    # }}}
})
describe("$reference_map()", {
    # {{{
    it("can successfully print object", {
        expect_output(con$reference_map())
    })
    # }}}
})
describe("$string()", {
    # {{{
    idf$set_options(save_format = "new_bottom")
    idf$string(header = FALSE)
    mat_out <- c(
        "Material,",
        "    NewMaterialName,         !- Name",
        "    MediumSmooth,            !- Roughness",
        "    0.0191,                  !- Thickness {m}",
        "    0.115,                   !- Conductivity {W/m-K}",
        "    513,                     !- Density {kg/m3}",
        "    1381,                    !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.78,                    !- Solar Absorptance",
        "    0.78;                    !- Visible Absorptance")

    it("can return correctly formatted IDF string output", {
        expect_equal(mat$string(comment = FALSE), mat_out)
    })
    # }}}
})
describe("$save()", {
    # {{{
    it("can successfully save object", {
           idf$string()
           idd <- Idd$new("../../idd/V8-6-0-Energy+.idd")
           Idf$new("C:/Users/hongy/Desktop/rad/case1/wSun.imf", idd)$save("C:/Users/hongy/Desktop/try_save.idf", overwrite = TRUE)
        expect_output(idf$save(path = "C:/Users/hongy/Desktop/try_save.idf", overwrite = TRUE))
    })
    # }}}
})
describe("$print()", {
    # {{{
    it("can successfully print object", {
        expect_output(ver$print())
    })
    # }}}
})

# NOTE: uncomment this block when API is stable
# describe("methods inherited from IddObject", {
#     # {{{
#     it("can use $group_name()",  expect_equal(mat$group_name(), "Surface Construction Elements"))
#     it("can use $group_order()", expect_equal(mat$group_order(), 5L))
#     it("can use $class_name()", expect_equal(mat$class_name(), "Material"))
#     it("can use $class_order()", expect_equal(mat$class_order(), 55L))
#     it("can use $class_format()", expect_equal(mat$class_format(), "standard"))
#     it("can use $min_fields()", expect_equal(mat$min_fields(), 6L))
#     it("can use $num_fields()", expect_equal(mat$num_fields(), 9L))
#     it("can use $memo()", expect_match(mat$memo(), "Regular materials described.*"))
#     it("can use $num_extensible()", expect_equal(surf$num_extensible(), 3L))
#     it("can use $reference_class_name()", expect_equal(surf$reference_class_name(), NULL))
#     it("can use $first_extensible()", expect_equal(surf$first_extensible(), 11L))
#     it("can use $add_extensible_groups()", expect_equal(surf$add_extensible_groups()$num_fields(), 373L))
#     it("can use $del_extensible_groups()", expect_equal(surf$del_extensible_groups()$num_fields(), 370L))
#     it("can use $is_version()", expect_true(ver$is_version()))
#     it("can use $is_required()", expect_false(ver$is_required()))
#     it("can use $is_unique()", expect_true(ver$is_unique()))
#     it("can use $is_extensible()", expect_true(surf$is_extensible()))
#     it("can use $has_name()", expect_true(surf$has_name()))
#     it("can use $field_name()", expect_match(surf$field_name(2), "Surface Type"))
#     it("can use $field_index()", expect_equal(surf$field_index("Surface Type"), 2L))
#     it("can use $field_type()", expect_equal(surf$field_type(2), "choice"))
#     it("can use $field_unit()", expect_equal(mat$field_unit(3), "m"))
#     it("can use $field_reference()", expect_equal(con$field_reference(1), list("ConstructionNames")))
#     it("can use $field_object_list()", expect_equal(con$field_object_list(2), list("MaterialName")))
#     it("can use $field_default()", expect_equal(mat$field_default(c(1, 2)), list(NA_character_, "VeryRough")))

#     it("can use $field_choice()", {
#        expect_equal(mat$field_choice(c(1, 2)),
#             list(NA_character_, c("VeryRough", "Rough", "MediumRough",
#                                   "MediumSmooth", "Smooth", "VerySmooth")))
#     })

#     it("can use $field_note()", expect_equal(mat$field_note(1), NA_character_))
#     # }}}
# })
