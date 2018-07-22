context("Idf methods")

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
    idd <- suppressMessages(use_idd(8.8, download = TRUE))
}

idf_parsed <- suppressWarnings(parse_idf_file(idf_text, idd))

describe("parse_idf_file()", {
    it("can parse Idf stored in strings", {
        # {{{
        expect_equal(names(idf_parsed),
            c("version", "options", "object", "value", "value_reference", "comment"))
        # }}}
    })

    it("can add version according to input Idd object", {
        # {{{
        expect_equal(idf_parsed$version, as.numeric_version("8.8"))
        expect_true(idf_parsed$version == idd$version())
        # }}}
    })
    it("can parse options data", {
        # {{{
        expect_equal(idf_parsed$options,
            list(save_format = "sorted", special_format = FALSE, view_in_ip = FALSE,
                 num_digits = 8L))
        # }}}
    })
    it("can parse object data", {
        # {{{
        expect_equal(idf_parsed$object$object_id, 1:4)
        expect_equal(idf_parsed$object$class_id, c(55, 90, 103, 1))
        # }}}
    })
    it("can parse value reference data", {
        # {{{
        expect_equal(idf_parsed$value_reference$reference_value_id, c(1, 10))
        expect_equal(idf_parsed$value_reference$value_id, c(11, 17))
        # }}}
    })
    it("can parse comment data", {
        # {{{
        expect_equal(idf_parsed$comment$comment_id, 1)
        expect_equal(idf_parsed$comment$comment, " this is a test comment for WD01")
        expect_equal(idf_parsed$comment$type, 0)
        expect_equal(idf_parsed$comment$object_id, 1)
        # }}}
    })
    it("can parse value data", {
        # {{{
        text_object <- c(
            "Material,
                WD01,                    !- Name
                MediumSmooth,            !- Roughness
                0.2000000,               !- Thickness {m}
                0.1150000,               !- Conductivity {W/m-K}
                513.0000,                !- Density {kg/m3}
                1381.000,                !- Specific Heat {J/kg-K}
                0.9000000,               !- Thermal Absorptance
                0.7800000,               !- Solar Absorptance
                0.7800000;               !- Visible Absorptance
            ")

        val <- c("WD01", "MediumSmooth", "0.2", "0.115", "513", "1381", "0.9",
            "0.78", "0.78", "8.8")
        num <- suppressWarnings(as.numeric(val))
        expect_warning(idf_value <- parse_idf_file(text_object, idd))
        expect_equal(names(idf_value$value),
            c("value_id", "value", "value_upper", "value_num", "value_ipnum", "object_id", "field_id"))
        expect_equal(idf_value$value$value_id, 1:10)
        expect_equal(idf_value$value$value, val)
        expect_equal(idf_value$value$value_upper, toupper(val))
        expect_equivalent(idf_value$value$value_num, num)
        expect_equal(idf_value$value$object_id, c(rep(1, 9), 2))
        # }}}
    })
    it("can detect error of invalid class name", {
        # {{{
        idf_wrong <- c(
            "WrongClass,
                WD01,                    !- Name
                MediumSmooth,            !- Roughness
                1.9099999E-02,           !- Thickness {m}
                0.1150000,               !- Conductivity {W/m-K}
                513.0000,                !- Density {kg/m3}
                1381.000,                !- Specific Heat {J/kg-K}
                0.9000000,               !- Thermal Absorptance
                0.7800000,               !- Solar Absorptance
                0.7800000;               !- Visible Absorptance
            ")
        expect_error(suppressWarnings(parse_idf_file(idf_wrong, idd)))
        # }}}
    })
})

describe("Idf$new()",
    # {{{
    it("can create new Idf object from string", {
        expect_warning(idf <- eplusr:::Idf$new(idf_text, idd))
    })
    # }}}
)

suppressWarnings(idf <- eplusr:::Idf$new(idf_text, idd))

describe("$version()", {
    # {{{
    it("can get Idf version", {
       expect_equal(idf$version(), as.numeric_version("8.8"))
    })
    # }}}
})
describe("$group_name()", {
    # {{{
    it("can get group names in Idf", {
        expect_equal(idf$group_name(),
                     c("Simulation Parameters",
                       "Surface Construction Elements",
                       "Thermal Zones and Surfaces"))
    })
    it("can get group names in Idd", {
        expect_equal(idf$group_name(all = TRUE), idd$group_name())
    })
    # }}}
})
describe("$class_name()", {
    # {{{
    it("can get class names in Idf", {
        expect_equal(idf$class_name(),
            c("Version", "Material", "Construction", "BuildingSurface:Detailed"))
    })
    it("can get class names in Idd", {
        expect_equal(idf$class_name(all = TRUE), idd$class_name())
    })
    # }}}
})
describe("$object_id()", {
    # {{{
    it("can get all object ids", {
        expect_equal(idf$object_id(),
            list(Version = 4L, Material = 1L, Construction = 2L,
                `BuildingSurface:Detailed` = 3L))
        expect_equal(idf$object_id(simplify = TRUE), 1L:4L)
    })
    it("can get all object ids of a single class", {
        expect_equal(idf$object_id("Version"), list(Version = 4L))
        expect_equal(idf$object_id("Version", simplify = TRUE), 4L)
    })
    # }}}
})
describe("$is_valid_class()", {
    # {{{
    it("works", {
        expect_true(idf$is_valid_class("Version"))
        expect_false(idf$is_valid_class("Wrong"))
    })
    # }}}
})
describe("$is_valid_id()", {
    # {{{
    it("works", {
        expect_true(idf$is_valid_id(1L))
        expect_false(idf$is_valid_id(5L))
        expect_error(idf$is_valid_id(1L:4L), "id is not a count")
    })
    # }}}
})
describe("$object()", {
    # {{{
    it("works", {
        expect_is(idf$object(1)[[1]], c("IdfObject", "IddObject", "R6"))
        expect_silent(idf$object(1))
        expect_silent(idf$object(1:2))
        expect_error(idf$object("a"))
        expect_error(idf$object(5))
        expect_equal(class(idf$object(1:2)), "list")
        expect_equal(length(idf$object(1:2)), 2L)
        expect_error(idf$object(1:5), "Invalid object ID found")
    })
    # }}}
})
describe("$object_in_class()", {
    # {{{
    it("works", {
        expect_equal(idf$object_in_class("Version"), idf$object(4))
        expect_error(idf$object_in_class("wrong"))
    })
    # }}}
})
describe("$dup_object()", {
    # {{{
    it("works", {
        expect_equal(idf$dup_object(1)[[1]]$get_value(2:5),
                     idf$object(1)[[1]]$get_value(2:5))
        expect_equal(idf$object(5)[[1]]$name(), "WD01_1")
    })
    # }}}
})
describe("$add_object()", {
    # {{{
    it("can add multiple objects", {
        expect_silent(
            idf$add_object(rep("RunPeriod", 2),
                value = list(
                    list("rp_test_1", 1, 1, 2, 1),

                    list(name = "rp_test_2",
                        begin_month = 3,
                        begin_day_of_month = 1,
                        end_month = 4,
                        end_day_of_month = 1)
                    ),
                comment = list(
                    list("Comment for new object 1", "Another comment"),
                    list("Comment for new object 2")),
                default = TRUE
            )
        )
        expect_equal(unname(unlist(idf$object(6)[[1]]$get_value())),
            c("rp_test_1", "1", "1", "2", "1", "UseWeatherFile", "Yes", "Yes",
                "No", "Yes", "Yes")
        )
    })
    it("can stop if trying to add a object with same name", {
        expect_silent(
            idf$add_object("RunPeriod",
                value = list("rp_test_4", 1, 1, 2, 1), default = TRUE, all = TRUE)
        )
        expect_error(
            idf$add_object("RunPeriod",
                value = list("rp_test_4", 1, 1, 2, 1), default = TRUE, all = TRUE)
        )
    })
    # }}}
})
describe("$set_object()", {
    # {{{
    it("set new values and comments", {
        expect_silent(
            idf$set_object("rp_test_1", list(name = "rp_test_3", begin_day_of_month = 2),
                comment = list(format(Sys.Date()), "begin day has been changed."))
        )
    })
    it("can delete fields", {
        expect_silent(
            idf$set_object("rp_test_3",
                list(use_weather_file_rain_indicators = NA,
                     use_weather_file_snow_indicators = NA))
        )
        expect_silent(idf$set_object("rp_test_4", list(start_year = NA)))
        expect_equal(length(idf$object("rp_test_4")[[1]]$get_value()), 13)
    })
    # }}}
})
test_that("S3 subset", {
    expect_equal(idf$Material, idf$object_in_class("Material"))
    expect_equal(idf[["Material"]], idf$object_in_class("Material"))
})
