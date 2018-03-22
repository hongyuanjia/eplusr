# Test all methods inherited from `IDDObject`, especially when number of fields
# of `IDFObject` is less than that in `IDDObject`.
# idfobj$field_name(index = NULL)

context("IDFObject method")

# helpers
# {{{
get_attr <- function (x, attr, type) {
    vapply(x, function (x) attr(x, attr), type)
}

remove_attr <- function (x) {
    lapply(x, function (x) {attributes(x) <- NULL; x})
}
# }}}

idf_text <- "
    Material,
        WD10,                    !- Name
        MediumSmooth,            !- Roughness
        0.667,                   !- Thickness {m}
        0.115,                   !- Conductivity {W/m-K}
        513,                     !- Density {kg/m3}
        1381,                    !- Specific Heat {J/kg-K}
        0.9,                     !- Thermal Absorptance
        0.78,                    !- Solar Absorptance
        0.78;                    !- Visible Absorptance

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

idd <- IDD$new("files/V8-8-0-Energy+.idd")
IDF$debug("initialize")
idf <- IDF$new(idf_text, idd)
idf <- IDF$new("files/5Zone_Transformer_8.8.idf", idd)
idf$objects(1:2)
idf_file$value[, object_id]
idf_file$value[, class]
idf_file$value[, fuck := list(list(Fuck$new(row_id, class))), by = row_id][, fuck]
idf_file$value[, row_id := .I][, list(list(Fuck$new(object_id))), by = row_id]$V1

Fuck <- R6::R6Class(classname = "Fuck",
            public = list (
                           initialize = function (object_id, class) {
                               private$m_class <- class
                               private$m_id <- object_id
                           }
                           ),
            private = list(
                           m_id = NA_integer_,
                           m_class = NA_character_
                           ))


idf_file$value[, fuck := list(list(IDFObject$new(object_id, class, comment, value, idd))), by = list(object_id, class_order)][, fuck]
idf_file$value[, purrr::map(object_id)]
purrr::map(purrr::transpose(idf_file$value), ~IDFObject$new(.x[[1]], .x[[2]], .x[[3]], .x[[4]], idd))

private$m_id
private$m_value
# get `Material`
idfobj <-

parse_idf(idf_text, idd)$value[,
    object := list(list(IDFObject$new(object_id, class, comment, value, idd))),
    by = object_id][, object]
private$m_objects[, object]
`._get_private`(`._get_private`(idf)$m_objects[1, object][[1]])$m_id
describe("$id()", {
    it("can return right object id", {
        expect_equal(idfobj$id(), 1L)
    })
})

describe("$get_comment()", {
    it("can return valid `IDFObject_Comment`", {
        expect_equal(idfobj$get_comment(),
                     structure(NA_character_, class = "IDFObject_Comment"))
    })
})

describe("$set_comment()", {
    it("can handle invalid input types of comment", {
        expect_error(idfobj$set_comment(comment = 1),
                     "comment is not a character vector")
        expect_error(idfobj$set_comment(comment = list("a")),
                     "comment is not a character vector")
    })

    it("can delete comments", {
        expect_equal(idfobj$set_comment(comment = NULL),
                     structure(NA_character_, class = "IDFObject_Comment"))
    })

    it("can add comments when comment is NA before", {
        expect_equal(idfobj$set_comment(comment = c("a")),
                     structure(c("a"), class = "IDFObject_Comment"))
    })

    it("can append comments", {
        expect_equal(idfobj$set_comment(comment = c("b")),
                     structure(c("a", "b"), class = "IDFObject_Comment"))
    })

    it("can prepend comments", {
        expect_equal(idfobj$set_comment(comment = c("c"), append = FALSE),
                     structure(c("c", "a", "b"), class = "IDFObject_Comment"))
    })

    it("can reset comments", {
        expect_equal(idfobj$set_comment(comment = c("d"), append = NULL),
                     structure(c("d"), class = "IDFObject_Comment"))
    })

    it("can detect invalid `append` value", {
        expect_error(idfobj$set_comment(comment = c("b"), append = 1:2),
                     "`append` should be NULL or a single logical value.")
    })

    it("can wrap comment at specified `width`", {
        expect_equal(idfobj$set_comment(comment = c("a", "bb ccc"), append = NULL, width = 1L),
                     structure(c("a", "bb", "ccc"), class = "IDFObject_Comment"))
    })

    it("can detect invalid `width` value", {
        expect_error(idfobj$set_comment(comment = c("a"), append = NULL, width = "a"))
    })

    it("can retain class when subsetting", {
        expect_is(idfobj$get_comment(), "IDFObject_Comment")
        expect_identical(class(idfobj$get_comment()), class(idfobj$get_comment()[2]))
    })
})

describe("$get_value()", {
    index <- c(3, 1, 5)
    name <- c("Thickness", "Name")
    it("can handle cases when both `index` and `name` are NULL", {
        expect_is(idfobj$get_value(), "IDFObject_Value")
        expect_equal(remove_attr(idfobj$get_value()),
                     list("WD10", "MediumSmooth", 0.667, 0.115, 513, 1381,
                          0.9, 0.78, 0.78))
    })

    it("can detect invaid `index` values", {
        expect_error(idfobj$get_value("1"),
                     "index is not a numeric or integer vector")
        expect_error(idfobj$get_value(c(1, 10:11)),
                     "Invalid field index found for class Material: `10` and `11`.")
    })

    it("can return subset of values in a object using `index`", {
        expect_is(idfobj$get_value(index), "IDFObject_Value")
        expect_equal(remove_attr(idfobj$get_value(index)), list(0.667, "WD10", 513))
        expect_equal(get_attr(idfobj$get_value(index), "index", 1L), c(3, 1, 5))
        expect_equal(get_attr(idfobj$get_value(index), "field", ""),
                     c("Thickness {m}", "Name", "Density {kg/m3}"))
    })

    it("can detect invalid `name` values", {
        expect_error(idfobj$get_value(name = c("Thickness", "Wrong", "Name")),
                     "Invalid field name found for class Material: `Wrong`.")
    })

    it("can handle cases when both `index` and `name` are given", {
        expect_warning(idfobj$get_value(index = index, name = name),
                       "Both `index` and `name` are given. `name` will be ignored.")
        expect_equal(remove_attr(idfobj$get_value(index, name)), list(0.667, "WD10", 513))
        expect_equal(get_attr(idfobj$get_value(index, name), "index", 1L), c(3, 1, 5))
        expect_equal(get_attr(idfobj$get_value(index, name), "field", ""),
                     c("Thickness {m}", "Name", "Density {kg/m3}"))
    })

    it("can return subset of values in a object using `name`", {
        expect_equal(remove_attr(idfobj$get_value(name = name)),
                     list(0.667, "WD10"))
        expect_equal(get_attr(idfobj$get_value(name = name), "index", 1L), c(3, 1))
        expect_equal(get_attr(idfobj$get_value(name = name), "field", ""),
                     c("Thickness {m}", "Name"))
    })
})

# get first object in class `Material`
idfobj <- idf$objects_in_class("BuildingSurface:Detailed", 1L)[[1]]

describe("$set_value()", {
    it("can stop when trying to directly modify `Version` object", {
        expect_error(idf$objects_in_class("Version")[[1]]$set_value(),
                     "Cannot modify `Version` object directly.")
    })

    it("can stop when no values are given", {
        expect_error(idfobj$set_value(), "Please give values to set.")
    })

    it("can stop when both named values and unnamed values are given", {
        expect_error(idfobj$set_value(name = "named", "unnamed"),
                     "Values should be either all unnamed or all named.")
    })

    it("can stop when duplicated names are given", {
        expect_error(idfobj$set_value(name = "first", name = "second"),
                     "Duplicated field nameds found: `name`.")
    })

    it("can stop when invalid names are given for a non-extensible class", {
        expect_error(idf$objects_in_class("SimulationControl")[[1]]$set_value(name = "first"),
                     "Invalid field names found for class SimulationControl: `name`.")
    })

    it("can stop when invalid names are given for an extensible class", {
        expect_error(idfobj$set_value(name = "first", wrong = "second"),
                     "Invalid field names found for class Material: `wrong`.")
    })

    it("can stop when valid names are given, but total field values are not accepatable for an extensible class", {
        expect_error(idfobj$set_value(vertex_5_x_coordinate = 1,
                                      vertex_5_y_corrdinate = 2))
    })

    it("can stop when total field values are acceptable but invalid names are given for an extensible class", {
        num_ori <- idfobj$num_fields()
        obj_ori <- idfobj$clone(deep = TRUE)
        expect_error(idfobj$set_value(vertex_5_x_coordinate = 1,
                                      vertex_5_y_coordinate = 2,
                                      vertex_5_z_wrong = 3),
                     paste0("Failed to add extensible fields. Invalid field ",
                            "names found for class BuildingSurface:Detailed: ",
                            "`vertex_5_z_wrong`."))
        expect_equal(idfobj$num_fields(), num_ori)
        expect_identical(iddobj, obj_ori)
    })

    it("can add new values for extensible fields", {
        expect_silent(idfobj$set_value(vertex_5_x_coordinate = 1,
                                      vertex_5_y_coordinate = 2,
                                      vertex_5_z_coordinate = 3))
        expect_equal(remove_attr(idfobj$get_value()[23:25]), list(1, 2, 3))
        expect_equal(get_attr(idfobj$get_value()[23:25], "index", 1), 23:25)
        expect_equal(get_attr(idfobj$get_value()[23:25], "field", ""),
                     c("Vertex 5 X-coordinate {m}",
                       "Vertex 5 Y-coordinate {m}",
                       "Vertex 5 Z-coordinate {m}"))
    })
idd$extensible_class_name()


    it("", {

    })
})
