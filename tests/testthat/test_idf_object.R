context("IdfObject methods")

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

if (is_avail_eplus(8.8)) {
    idd <- suppressMessages(use_idd(8.8))
} else {
    idd_path <- "https://raw.githubusercontent.com/NREL/EnergyPlus/v8.9.0/idd/V8-8-0-Energy%2B.idd"
    idd <- read_idd(idd_path)
}

suppressWarnings(idf <- Idf$new(idf_text, idd))
ver <- idf$Version[[1]]
mat <- idf$Material[[1]]
surf <- idf$BuildingSurface_Detailed[[1]]
con <- idf$Construction[[1]]

describe("$id()", {
    it("can return right object id", expect_equal(mat$id(), 1L))
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
        expect_equal(mat$get_value(simplify = TRUE), unname(as.character(value)))
    })

    it("can detect invaid `index` values", {
        expect_error(mat$get_value("1"),
                     "Invalid field name found for class `Material`")
        expect_error(mat$get_value(c(1, 10:11)),
                     "Invalid field index found for class `Material`: `10` and `11`.")
    })

    it("can return subset of values in a object using `index`", {
        expect_is(mat$get_value(index), "list")
        expect_equivalent(mat$get_value(index), value[index], tolerance = 1e-5)
        expect_equal(mat[[2]], "MediumSmooth")
        expect_equal(mat[["Roughness"]], "MediumSmooth")
        expect_equal(mat[c(2,1)], list(Roughness = "MediumSmooth", Name = "WD01"))
    })

    it("can return subset of values in a object using `name`", {
        expect_equivalent(mat$get_value("Roughness"), value["Roughness"])
        expect_equivalent(mat$get_value("Roughness", simplify = TRUE), "MediumSmooth")
        expect_equal(mat$Roughness, "MediumSmooth")
    })

    it("can detect invalid `name` values", {
        expect_error(mat$get_value(c("Thickness", "Wrong", "Name")),
                     "Invalid field name found for class `Material`: `Wrong`.")
    })
    # }}}
})

describe("$set_value()", {
    # {{{
    it("can stop when trying to directly modify `Version` object", {
        expect_error(ver$set_value(8.8),
                     "Modify `Version` object directly is prohibited")
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
                     "Duplicated field names found")
    })

    it("can stop when invalid names are given for a non-extensible class", {
        expect_error(mat$set_value(wrong = "something"),
                     "Invalid field name found")
    })

    it("can stop when invalid names are given for an extensible class", {
        expect_error(con$set_value(name = "first", wrong = "second"),
                     "Invalid field name found")
    })

    it("can stop when valid names are given, but total field values are not accepatable for an extensible class", {
        expect_error(surf$set_value(vertex_5_x_coordinate = 1,
                                    vertex_5_y_coordinate = 2))
    })

    it("can stop when total field values are acceptable but invalid names are given for an extensible class", {
        expect_error(surf$set_value(
            vertex_5_x_coordinate = 1, vertex_5_y_coordinate = 2, vertex_5_z_wrong = 3),
            paste0("Incomplete extensible group or invalid field names"))
    })

    it("can add new values for extensible fields", {
        eplusr_option(validate_level = "draft")
        expect_silent(surf$set_value(vertex_5_x_coordinate = 1,
                                     vertex_5_y_coordinate = 2,
                                     vertex_5_z_coordinate = 3))
        expect_equal(surf$get_value()[23:25],
                     list(`Vertex_5_X_coordinate` = 1,
                          `Vertex_5_Y_coordinate` = 2,
                          `Vertex_5_Z_coordinate` = 3))
    })

    it("can change referenced values accordingly", {
        expect_equal(con$set_value(name = "NewWallName")$get_value("Name")[[1]], "NewWallName")
        expect_equal(surf$get_value("Construction Name")[[1]], "NewWallName")

        expect_equal(mat$set_value(name = "NewMaterialName")$get_value("Name")[[1]], "NewMaterialName")
        expect_equal(con$get_value("Outside Layer")[[1]], "NewMaterialName")
    })

    it("can stop when there are invalid references in the input", {
        eplusr_option(validate_level = "final")
        expect_error(con$set_value(layer_6 = "missing"))
    })

    it("works using `[[<-.IdfObject`", {
        expect_silent(mat$Name <- "NewMaterial")
        expect_equal(mat$name(), "NewMaterial")
        expect_silent(mat[["Name"]] <- "NewMaterialName1")
        expect_equal(mat$name(), "NewMaterialName1")
        expect_silent(mat[[1]] <- "NewMaterialName")
        expect_equal(mat$name(), "NewMaterialName")
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

describe("$string()", {
    # {{{
    eplusr_option(save_format = "new_bot")
    mat_out <- c(
        "",
        "Material,",
        "    NewMaterialName,         !- Name",
        "    MediumSmooth,            !- Roughness",
        "    0.0191,                  !- Thickness {m}",
        "    0.115,                   !- Conductivity {W/m-K}",
        "    513,                     !- Density {kg/m3}",
        "    1381,                    !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.78,                    !- Solar Absorptance",
        "    0.78;                    !- Visible Absorptance",
        "")

    it("can return correctly formatted IDF string output", {
        expect_equal(mat$string(comment = FALSE), mat_out)
    })
    # }}}
})

test_that("S3 subsetting works", {
    expect_equal(mat$Roughness, "MediumSmooth")
    expect_equal(mat[["Roughness"]], "MediumSmooth")
})

test_that("S3 assigning works", {
    expect_silent(mat$Roughness <- "Rough")
    expect_equal(mat$Roughness, "Rough")
    expect_silent(mat[["Roughness"]] <- "MediumSmooth")
    expect_equal(mat$Roughness, "MediumSmooth")
})

test_that("$possible_value() works", {
    pos <- con$possible_value(c(3,1))
    expect_is(pos, "IdfFieldPossible")
    expect_equal(pos$field_index, c(3L, 1L))
    expect_equal(pos$field_name, c("Layer 2", "Name"))
    expect_equal(pos$auto, rep(NA_character_, 2L))
    expect_equal(pos$default, rep(list(NA_character_), 2L))
    expect_equal(pos$choice, rep(list(NA_character_), 2L))
    expect_equal(pos$reference, list("NewMaterialName", NULL))
})
