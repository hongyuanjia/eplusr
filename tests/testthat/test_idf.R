context("Idf and IdfObject")

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

idd_8_8 <- use_idd(8.8, download = "auto")

# parse_idf_file() {{{
test_that("parse_idf_file()", {
    idf_parsed <- suppressWarnings(parse_idf_file(idf_text, idd_8_8))

    # can parse Idf stored in strings
    expect_equal(names(idf_parsed),
        c("version", "options", "object", "value", "value_reference", "comment"))

    # can add version according to input Idd object
    expect_equal(idf_parsed$version, as.numeric_version("8.8"))

    # can parse options data
    expect_equal(idf_parsed$options,
        list(save_format = "sorted", special_format = FALSE, view_in_ip = FALSE,
            num_digits = 8L))

    # can parse object data
    expect_equal(idf_parsed$object$object_id, 1:4)
    expect_equal(idf_parsed$object$class_id, c(55, 90, 103, 1))

    # can parse value reference data
    expect_equal(idf_parsed$value_reference$reference_value_id, c(1, 10))
    expect_equal(idf_parsed$value_reference$value_id, c(11, 17))

    # can parse comment data
    expect_equal(idf_parsed$comment$comment_id, 1)
    expect_equal(idf_parsed$comment$comment, " this is a test comment for WD01")
    expect_equal(idf_parsed$comment$type, 0)
    expect_equal(idf_parsed$comment$object_id, 1)

    # can detect EpMacro lines
    text_object <- paste0("Version, 8.8;\n##include ", tempfile())
    expect_equal(parse_idf_file(text_object)$comment$type, -1L)
    expect_true(attr(parse_idf_file(text_object), "is_imf"))
    expect_warning(Idf$new(text_object), "Currently, Imf file is not fully supported")

    # can parse value data
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
    num[10] <- NA_real_

    expect_warning(idf_value <- parse_idf_file(text_object, idd_8_8))
    expect_equal(names(idf_value$value),
        c("value_id", "value", "value_upper", "value_num", "value_ipnum", "object_id", "field_id"))
    expect_equal(idf_value$value$value_id, 1:10)
    expect_equal(idf_value$value$value, val)
    expect_equal(idf_value$value$value_upper, toupper(val))
    expect_equivalent(idf_value$value$value_num, num)
    expect_equal(idf_value$value$object_id, c(rep(1, 9), 2))

    # can detect error of invalid class name
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
    expect_error(suppressWarnings(parse_idf_file(idf_wrong, idd_8_8)))

    # can detect error of multiple version
    idf_wrong <- "Version, 8.8;\nVersion, 8.9;"
    expect_error(parse_idf_file(idf_wrong, idd_8_8))

    # can detect error of invalid field number
    idf_wrong <- "
        Version, 8.8;
        SurfaceConvectionAlgorithm:Inside,
            Simple, !- Algorithm
            TARP; !- Algorithm"
    expect_error(parse_idf_file(idf_wrong))
})
# }}}

# Idf class{{{
test_that("Idf class", {
    # add version string
    idf_text_ver <- paste0(idf_text, "\nVersion, 8.8;")

    # can create new Idf object from string
    expect_silent(idf <- read_idf(idf_text_ver, idd_8_8))

    # can get version
    expect_equal(idf$version(), as.numeric_version("8.8"))

    # can get path
    expect_equal(idf$path(), NULL)

    # can get group names in Idf
    expect_equal(idf$group_name(), c("Simulation Parameters",
        "Surface Construction Elements", "Thermal Zones and Surfaces"))

    # can get group names in Idd
    expect_equal(idf$group_name(all = TRUE), idd_8_8$group_name())

    # can get class names in Idf
    expect_equal(idf$class_name(),
        c("Version", "Material", "Construction", "BuildingSurface:Detailed"))

    # can get class names in Idd
    expect_equal(idf$class_name(all = TRUE), idd_8_8$class_name())

    # can get all object ids
    expect_equal(idf$object_id(),
        list(Version = 4L, Material = 1L, Construction = 2L,
            `BuildingSurface:Detailed` = 3L))
    expect_equal(idf$object_id(simplify = TRUE), 1L:4L)

    # can get all object ids of a single class
    expect_equal(idf$object_id("Version"), list(Version = 4L))
    expect_equal(idf$object_id("Version", simplify = TRUE), 4L)

    # can get object names
    expect_equal(idf$object_name(), list(Version = NA_character_, Material = "WD01",
        Construction = "WALL-1", `BuildingSurface:Detailed` = "WALL-1PF"))
    expect_equal(idf$object_name(simplify = TRUE), c("WD01", "WALL-1", "WALL-1PF", NA_character_))
    expect_equal(idf$object_name(c("Material", "Construction")),
        list(Material = "WD01", Construction = "WALL-1"))
    expect_equal(idf$object_name(c("Material", "Construction"), simplify = TRUE),
        c("WD01", "WALL-1"))

    # can get object num
    expect_equal(idf$object_num(), 4L)
    expect_equal(idf$object_num(c("Version", "Construction")), c(1L, 1L))
    expect_error(idf$object_num(1), "Invalid class name found")

    # can check invalid group name
    expect_true(idf$is_valid_group("Simulation Parameters"))
    expect_false(idf$is_valid_group("Simulation_Parameters"))

    # can check invalid class name
    expect_true(idf$is_valid_class("Version"))
    expect_false(idf$is_valid_class("version"))

    # can check invalid object ID
    expect_true(idf$is_valid_id(1L))
    expect_false(idf$is_valid_id(5L))
    expect_equal(idf$is_valid_id(1L:4L), rep(TRUE, times = 4L))
    expect_error(idf$is_valid_id("1"))

    # can check invalid object name
    expect_true(idf$is_valid_name("WD01"))
    expect_true(idf$is_valid_name("wd01"))
    expect_false(idf$is_valid_name(NA_character_))
    expect_equal(idf$is_valid_name(c("wd01", "WALL-1")), c(TRUE, TRUE))

    # can check if model has been changed since read
    expect_false(idf$is_unsaved())

    # can get IddObject
    expect_is(idf$definition(c("Version"))[[1]], "IddObject")
    expect_is(._get_private(idf$definition(c("Version"))[[1]])$m_idf_tbl, "environment")

    # can get IdfObject
    expect_is(idf$object(1)[[1]], "IdfObject")
    expect_equal(names(idf$object("WD01")), "WD01")
    expect_equal(names(idf$object("wall-1")), "WALL_1")
    expect_equal(idf$object(1)[[1]]$name(), "WD01")
    expect_is(idf$object(1:2), "list")
    expect_error(idf$object("a"), "Invalid object name")
    expect_error(idf$object(1:5), "Invalid object ID")

    # can get all objects in a class
    expect_equal(idf$object_in_class("Version"), idf$object(4))
    expect_error(idf$object_in_class("version"), "Invalid class name")

    # can get objects using "[" and "$"
    expect_equal(idf$Material, idf$object_in_class("Material"))
    expect_equal(idf[["Material"]], idf$object_in_class("Material"))

    # can search object
    expect_equal(names(idf$search_object("W")), c("WD01", "WALL_1", "WALL_1PF"))
    expect_message(idf$search_object("ma;ldk"), "No matched result found")
    expect_equal(suppressMessages(idf$search_object("ma;ldk")), NULL)

    # DUPLICATE {{{
    # can duplicate objects and assign new names
    expect_equal(names(idf$dup_object("wd01", "wd01-dup")), "wd01_dup")
    expect_equal(idf$object(5)[[1]]$name(), "wd01-dup")
    expect_equal(idf$object("wd01-dup")[[1]]$get_value(2:5), idf$object("wd01")[[1]]$get_value(2:5))
    expect_equal(idf$dup_object("wd01")[[1]]$name(), "WD01_1")
    expect_error(idf$dup_object("wd01", "wd01-dup"))
    expect_equal(names(idf$dup_object(rep("wd01", times = 10L))),
        paste0("WD01_", 2:11))
    # }}}

    # ADD {{{
    # invalid value input format
    expect_error(
        idf$add_object(c("Material", "Construction"), list(name = "mat")),
        "`value` and `comment` should have the same length as `class`"
    )
    expect_error(
        idf$add_object(c("Material", "Construction"), list(name = "mat", name = "const", thickness = 1)),
        "`value` and `comment` should have the same length as `class`"
    )
    expect_error(
        idf$add_object("Material", character(0)),
        "Invalid `value` or `comment` format found"
    )
    expect_error(
        idf$add_object("Material", list()),
        "Invalid `value` or `comment` format found"
    )
    expect_error(
        idf$add_object("Material", list(list())),
        "Invalid `value` or `comment` format found"
    )
    expect_error(
        idf$add_object("Material", list(list(name = "mat"))),
        "Invalid `value` or `comment` format found"
    )
    expect_error(
        idf$add_object(c("Material", "Construction"), list(name = "mat", name = "const")),
        "Invalid `value` or `comment` format found"
    )
    expect_error(
        idf$add_object(c("Material", "Construction"), list(list(), NULL)),
        "Invalid `value` or `comment` format found"
    )
    expect_error(
        idf$add_object(c("Material", "Construction"), list(character(), NULL)),
        "Invalid `value` or `comment` format found"
    )
    expect_error(
        idf$add_object(c("Material", "Construction"), list(list(list(1)), NULL)),
        "Invalid `value` or `comment` format found"
    )
    # invalid comment input format
    expect_error(
        idf$add_object("Material", NULL, list(list("comment"))),
        "Invalid `value` or `comment` format found"
    )
    expect_error(
        idf$add_object("Material", NULL, list(character(0))),
        "Invalid `value` or `comment` format found"
    )
    expect_error(
        idf$add_object(c("Material", "Construction"), NULL, list(NULL)),
        "`value` and `comment` should have the same length as `class`"
    )
    expect_error(
        idf$add_object(c("Material", "Construction"), NULL, list(list())),
        "`value` and `comment` should have the same length as `class`"
    )
    expect_error(
        idf$add_object(c("Material", "Construction"), NULL, list(list(1), list())),
        "Invalid `value` or `comment` format found"
    )
    # mixed named and unnamed
    expect_error(
        idf$add_object(c("Material", "Construction"),
            list(list(name = "Rough", "Rough"),
                 list(name = "const", "const"))),
        "Values should be either all unnamed or all named"
    )
    # duplicated field names
    expect_error(
        idf$add_object(c("Material", "Construction"),
            list(list(name = "Rough", name = "Rough"),
                 list(name = "const", name = "const"))),
        "Duplicated field names found"
    )
    # adding existing unique
    expect_error(idf$add_object("Version", "Add `Version` object directly is prohibited"))
    idf_path <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
    expect_silent(idf_full <- read_idf(idf_path, idd_8_8))
    expect_error(
        idf_full$add_object("Building"),
        "existing unique objects cannot be duplicated"
    )

    # adding empty object
    expect_error(
        idf$add_object("Material", NULL),
        "Adding empty objects is prohibited"
    )
    expect_error(
        idf$add_object(c("Material", "Construction"), list(NULL, NULL)),
        "Adding empty objects is prohibited"
    )

    # invalid field number
    expect_error(
        idf$add_object("Output:Variable", value = list("a", "b", "c", "d", "e")),
        "Invalid field number"
    )
    expect_error(
        idf$add_object(c("Output:Variable", "RunPeriod"),
            list(list("a", "b", "c", "d", "e"), list("a", "b", "c", "d")),
            default = TRUE, all = TRUE),
        "Invalid field number"
    )

    # invalid field names
    # (a) non-extensible classes
    expect_error(
        idf$add_object(c("Material", "Construction"), list(list(wrong = "Rough"), list(name = "const"))),
        "Invalid field name found"
    )
    # (b) extensible classes
    expect_error(
        idf$add_object(c("Schedule:Week:Compact"), list(daytype_list_6 = "day6")),
        "Incomplete extensible group or invalid field name"
    )
    expect_error(
        idf$add_object(
            c("SurfaceProperty:HeatTransferAlgorithm:SurfaceList"),
            list(name = "algo", wrong = "Rough")),
        "Incomplete extensible group or invalid field name"
    )
    expect_error(
        idf$add_object(
            c("SurfaceProperty:HeatTransferAlgorithm:SurfaceList"),
            list(name = "algo", surface_name_8 = "Rough")),
        "Incomplete extensible group or invalid field name"
    )
    expect_error(
        idf$add_object(
            c("Schedule:Week:Compact"),
             list(daytype_list_7 = "day7", schedule_day_name_6 = "sch6")),
        "Incomplete extensible group or invalid field name"
    )
    expect_error(
        idf$add_object(
            c("SurfaceProperty:HeatTransferAlgorithm:SurfaceList"),
            list(surface_name_8 = "surf8", surface_name_20 = "surf20")),
        "Incomplete extensible group or invalid field name"
    )
    expect_error(
        idf$add_object(
              c("Schedule:Week:Compact"),
              list(daytype_list_8 = "day8", schedule_day_name_8 = "sch8")),
        "Incomplete extensible group or invalid field name"
    )
    expect_equal(idd_8_8$SurfaceProperty_HeatTransferAlgorithm_SurfaceList$num_fields(), 8L)
    expect_equal(idd_8_8$Schedule_Week_Compact$num_fields(), 11L)

    # incomplete extensible group
    expect_error(
        idf$add_object(
            c("Schedule:Week:Compact"),
            list(daytype_list_1 = NA, schedule_day_name_1 = "sch1"),
        )
    )
    expect_error(
        idf$add_object(
            c("SurfaceProperty:HeatTransferAlgorithm:SurfaceList"),
            list(name = "surf_prop", surface_name_1 = "surf2", surface_name_2 = NA)
        )
    )
    expect_error(
        idf$add_object(
            c("Schedule:Week:Compact"),
            list(daytype_list_1 = NA, schedule_day_name_1 = NA,
                 daytype_list_2 = "day2", schedule_day_name_2 = "sch2")
        )
    )
    expect_error(
        idf$add_object(
            c("SurfaceProperty:HeatTransferAlgorithm:SurfaceList"),
            list(name = "surf_prop", surface_name_1 = "", surface_name_2 = "surf2")
        )
    )
    expect_error(
        idf$add_object(
              c("Schedule:Week:Compact"),
              list(daytype_list_5 = "", schedule_day_name_5 = ""))
    )
    expect_error(
        idf$add_object(
              c("Schedule:Week:Compact", "Construction"),
              list(list(daytype_list_6 = "day8", schedule_day_name_6 = "sch8"),
                   list(name = "const", outside_layer = "mat")))
    )

    # add all fields with defaults
    expect_equal(
        idf$add_object("RunPeriod", value = list("rp_test_1", 1, 1, 2, 1),
            default = TRUE, all = TRUE)$rp_test_1$get_value(),
        list(Name = "rp_test_1",
             Begin_Month = 1L,
             Begin_Day_of_Month = 1L,
             End_Month = 2L,
             End_Day_of_Month = 1L,
             Day_of_Week_for_Start_Day = "UseWeatherFile",
             Use_Weather_File_Holidays_and_Special_Days = "Yes",
             Use_Weather_File_Daylight_Saving_Period = "Yes",
             Apply_Weekend_Holiday_Rule = "No",
             Use_Weather_File_Rain_Indicators = "Yes",
             Use_Weather_File_Snow_Indicators = "Yes",
             Number_of_Times_Runperiod_to_be_Repeated = 1L,
             Increment_Day_of_Week_on_repeat = "Yes",
             Start_Year = NA_integer_)
    )
    expect_silent(
        idf$add_object(rep("RunPeriod", 2),
            value = list(
                list("rp_test_2", 1, 1, 2, 1),

                list(name = "rp_test_3",
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
    expect_equal(idf$object("rp_test_2")[[1]]$get_value(simplify = TRUE),
        c("rp_test_2", "1", "1", "2", "1", "UseWeatherFile", "Yes", "Yes",
            "No", "Yes", "Yes")
    )
    expect_equal(idf$object("rp_test_3")[[1]]$get_value(simplify = TRUE),
        c("rp_test_3", "3", "1", "4", "1", "UseWeatherFile", "Yes", "Yes",
            "No", "Yes", "Yes")
    )

    # can stop adding objects if trying to add a object with same name
    expect_error(
        idf$add_object("RunPeriod",
            value = list("rp_test_1", 1, 1, 2, 1), default = TRUE, all = TRUE)
    )
    # }}}

    # SET {{{
    # set new values and comments
    # name conflict
    expect_error(
        idf$set_object("rp_test_1", list(name = "rp_test_3", begin_day_of_month = 2),
            comment = list(format(Sys.Date()), "begin day has been changed."))
    )
    expect_silent(
        idf$set_object("rp_test_1", list(name = "rp_test_4", begin_day_of_month = 2),
            comment = list("begin day has been changed."))
    )
    expect_equal(idf$RunPeriod$rp_test_4$Begin_Day_of_Month, 2L)
    expect_equal(idf$RunPeriod$rp_test_4$get_comment(), "begin day has been changed.")
    # can delete fields
    expect_silent(
        idf$set_object("rp_test_4",
            list(use_weather_file_rain_indicators = NA,
                use_weather_file_snow_indicators = NA))
    )
    expect_equal(idf$RunPeriod$rp_test_4$Use_Weather_File_Rain_Indicators, NA_character_)
    expect_equal(idf$RunPeriod$rp_test_4$Use_Weather_File_Snow_Indicators, NA_character_)
    expect_silent(idf$set_object("rp_test_4", list(Number_of_Times_Runperiod_to_be_Repeated = NA, Increment_Day_of_Week_on_repeat = NA)))
    expect_equal(length(idf$RunPeriod$rp_test_4$get_value()), 11)
    # }}}

    # INSERT {{{
    expect_error(idf$ins_object(list()),
        "Input should be an IdfObject, or a list of IdfObjects"
    )
    expect_silent(idf$ins_object(idf_full$Material_NoMass$R13LAYER))
    expect_equal(idf$object_name("Material:NoMass", simplify = TRUE), "R13LAYER")
    expect_equal(idf_full$Material_NoMass$R13LAYER$get_value(simplify = TRUE),
        c("R13LAYER", "Rough", "2.290965", "0.9", "0.75", "0.75")
    )
    expect_error(idf$ins_object(idf_full$Version[[1]]),
        "Could not insert a `Version` object"
    )
    expect_output(idf$ins_object(idf$Material_NoMass$R13LAYER))
    # }}}

    # DELETE {{{
    expect_error(idf$del_object(4L), "Deleting `Version` object is prohibited")
    expect_error(idf$del_object(c(1, 2, 1)), "`object` should not contain any duplication")
    expect_silent(idf$add_object("Building", list("building")))
    expect_error(
        idf$del_object(idf$Building[[1]]$id()),
        "deleting an required object is prohibited"
    )
    expect_error(
        idf_full$del_object(idf_full$Material_NoMass[[1]]$id()),
        "Deleting an object that is referenced by others is prohibited"
    )
    expect_equal(eplusr_option(validate_level = "none"), list(validate_level = "none"))
    expect_output(idf_full$del_object(12, referenced = TRUE))
    expect_equal(idf_full$is_valid_id(c(12, 15)), c(FALSE, FALSE))
    expect_equal(eplusr_option(validate_level = "final"), list(validate_level = "final"))
    # }}}

    # SEARCH AND REPLACE {{{
    idf$print(plain = TRUE)
    expect_equal(
        vapply(idf$search_value("WALL"), function (x) x$id(), integer(1)),
        c(WALL_1 = 2L, WALL_1PF = 3L)
    )
    expect_equal(
        vapply(idf$replace_value("WALL-1", "WALL-2"), function (x) x$id(), integer(1)),
        c(WALL_2 = 2L, WALL_2PF = 3L)
    )
    # }}}

    # can get idf in string format
    idf_string <- c(
        "!-Generator eplusr",
        "!-Option OriginalOrderTop",
        "",
        "!-NOTE: All comments with '!-' are ignored by the IDFEditor and are generated automatically.",
        "!-      Use '!' comments if they need to be retained when using the IDFEditor.",
        "",
        "Construction,",
        "    WALL-1,                  !- Name",
        "    WD01;                    !- Outside Layer",
        "",
        "Version,",
        "    8.8;                     !- Version Identifier",
        ""
    )
    expect_silent(idf_1 <- read_idf(paste0(idf_string, collapse = "\n")))
    expect_equal(idf_1$string(format = "new_top"), idf_string)

    # can validate
    expect_is(idf$validate(), "IdfValidity")
    expect_false(idf$is_valid())

    # can save model
    expect_silent(idf$save(file.path(tempdir(), "test_save.idf")))
    expect_error(idf$save(file.path(tempdir(), "test_save.idf")))
})
# }}}

# IdfObject class{{{
test_that("IdfObject class", {
    # add version string
    idf_text_ver <- paste0(idf_text, "\nVersion, 8.8;")

    # can create new Idf object from string
    expect_silent(idf <- read_idf(idf_text_ver, idd_8_8))

    ver <- idf$Version[[1]]
    mat <- idf$Material[[1]]
    surf <- idf$BuildingSurface_Detailed[[1]]
    con <- idf$Construction[[1]]

    # get group name
    expect_equal(con$group_name(), "Surface Construction Elements")

    # get class name
    expect_equal(con$class_name(), "Construction")

    # get object ID
    expect_equal(mat$id(), 1L)
    expect_equal(mat$get_comment(), " this is a test comment for WD01")

    # can handle invalid input types of comment
    expect_error(mat$set_comment(comment = 1), "comment is not a character vector")
    expect_error(mat$set_comment(comment = list("a")), "comment is not a character vector")

    # can delete comments
    expect_equal(mat$set_comment(comment = NULL)$get_comment(), character(0))

    # can add comments when comment is NA before
    expect_equal(mat$set_comment(comment = c("a"))$get_comment(), "a")

    # can append comments
    expect_equal(mat$set_comment(comment = c("b"))$get_comment(), c("a", "b"))

    # can prepend comments
    expect_equal(mat$set_comment(comment = c("c"), append = FALSE)$get_comment(),
        c("c", "a", "b"))

    # can reset comments
    expect_equal(mat$set_comment(comment = c("d"), append = NULL)$get_comment(), "d")

    # can detect invalid `append` value
    expect_error(mat$set_comment(comment = c("b"), append = 1:2),
        "`append` should be NULL or a single logical value.")

    # can wrap comment at specified `width`
    expect_equal(mat$set_comment(comment = c("a", "bb ccc"), append = NULL, width = 1L)$get_comment(),
        c("a", "bb", "ccc"))

    # can detect invalid `width` value
    expect_error(mat$set_comment(comment = c("a"), append = NULL, width = "a"))

    # can get field values
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

    # can handle cases when both `index` and `name` are NULL
    expect_is(mat$get_value(), "list")
    expect_equivalent(mat$get_value(), value, tolerance = 1e-5)
    expect_equal(mat$get_value(simplify = TRUE), unname(as.character(value)))

    # can detect invaid `index` values
    expect_error(mat$get_value("1"),
        "Invalid field name found for class `Material`")
    expect_error(mat$get_value(c(1, 10:11)),
        "Invalid field index found for class `Material`: `10` and `11`.")

    # can return subset of values in a object using `index`
    expect_is(mat$get_value(index), "list")
    expect_equivalent(mat$get_value(index), value[index], tolerance = 1e-5)
    expect_equal(mat[[2]], "MediumSmooth")
    expect_equal(mat[["Roughness"]], "MediumSmooth")
    expect_equal(mat[c(2,1)], list(Roughness = "MediumSmooth", Name = "WD01"))

    # can return subset of values in a object using `name`
    expect_equivalent(mat$get_value("Roughness"), value["Roughness"])
    expect_equivalent(mat$get_value("Roughness", simplify = TRUE), "MediumSmooth")
    expect_equal(mat$Roughness, "MediumSmooth")

    # can detect invalid `name` values
    expect_error(mat$get_value(c("Thickness", "Wrong", "Name")),
        "Invalid field name found for class `Material`: `Wrong`.")

    # can stop when trying to directly modify `Version` object
    expect_error(ver$set_value(8.8),
        "Modify `Version` object directly is prohibited")

    # can stop when no values are given
    expect_error(con$set_value(), "Please give values to set.")

    # can stop when both named values and unnamed values are given
    expect_error(con$set_value(name = "named", "unnamed"),
        "Values should be either all unnamed or all named.")

    # can stop when duplicated names are given
    expect_error(con$set_value(name = "first", name = "second"),
        "Duplicated field names found")

    # can stop when invalid names are given for a non-extensible class
    expect_error(mat$set_value(wrong = "something"),
        "Invalid field name found")

    # can stop when invalid names are given for an extensible class
    expect_error(con$set_value(name = "first", wrong = "second"),
        "Invalid field name found")

    # can stop when valid names are given, but total field values are not accepatable for an extensible class
    expect_error(surf$set_value(vertex_5_x_coordinate = 1,
            vertex_5_y_coordinate = 2))

    # can stop when total field values are acceptable but invalid names are given for an extensible class
    expect_error(surf$set_value(
            vertex_5_x_coordinate = 1, vertex_5_y_coordinate = 2, vertex_5_z_wrong = 3),
        paste0("Incomplete extensible group or invalid field name"))

    # can add new values for extensible fields
    eplusr_option(validate_level = "draft")
    expect_silent(surf$set_value(vertex_5_x_coordinate = 1,
            vertex_5_y_coordinate = 2,
            vertex_5_z_coordinate = 3))
    expect_equal(surf$get_value()[23:25],
        list(`Vertex_5_X_coordinate` = 1,
            `Vertex_5_Y_coordinate` = 2,
            `Vertex_5_Z_coordinate` = 3))

    # can change referenced values accordingly
    expect_equal(con$set_value(name = "NewWallName")$get_value("Name")[[1]], "NewWallName")
    expect_equal(surf$get_value("Construction Name")[[1]], "NewWallName")

    expect_equal(mat$set_value(name = "NewMaterialName")$get_value("Name")[[1]], "NewMaterialName")
    expect_equal(con$get_value("Outside Layer")[[1]], "NewMaterialName")

    # can stop when there are invalid references in the input
    eplusr_option(validate_level = "final")
    expect_error(con$set_value(layer_6 = "missing"))

    # works using `[[<-.IdfObject`
    expect_silent(mat$Name <- "NewMaterial")
    expect_equal(mat$name(), "NewMaterial")
    expect_silent(mat[["Name"]] <- "NewMaterialName1")
    expect_equal(mat$name(), "NewMaterialName1")
    expect_silent(mat[[1]] <- "NewMaterialName")
    expect_equal(mat$name(), "NewMaterialName")

    # can find errors of invalid reference
    expect_is(con$validate(), "IdfValidity")
    expect_true(ver$is_valid())
    expect_true(mat$is_valid())
    expect_false(surf$is_valid())
    expect_false(con$is_valid())

    # can return correctly formatted IDF string output
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
    expect_equal(mat$string(comment = FALSE), mat_out)
    eplusr_option(save_format = "asis")

    # references
    idf_exp <- read_idf(system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr"))
    # get the IdfObject of material named "C5 - 4 IN HW CONCRETE"
    mat_exp <- idf_exp$Material$C5_4_IN_HW_CONCRETE
    expect_null(mat_exp$ref_from_object())
    expect_false(mat_exp$has_ref_from())
    expect_equal(names(mat_exp$ref_by_object()), "FLOOR")
    expect_true(mat_exp$has_ref_by())
    expect_true(mat_exp$has_ref())

    # table
    expect_equal(
        ver$table(),
        data.table::data.table(index = 1L, name = "Version Identifier", value = "8.8")
    )
    expect_equal(
        ver$table(string_value = FALSE),
        data.table::data.table(index = 1L, name = "Version Identifier", value = list("8.8"))
    )
    expect_equal(
        ver$table(wide = TRUE),
        data.table::data.table(`Version Identifier` = "8.8")
    )
    expect_equal(
        ver$table(wide = TRUE, string_value = FALSE),
        data.table::data.table(`Version Identifier` = "8.8")
    )

    # S3 subsetting works
    expect_equal(mat$Roughness, "MediumSmooth")
    expect_equal(mat[["Roughness"]], "MediumSmooth")

    # $possible_value() works
    pos <- con$possible_value(c(3,1))
    expect_is(pos, "IddFieldPossible")
    expect_equal(pos$field_index, c(3L, 1L))
    expect_equal(pos$field_name, c("Layer 2", "Name"))
    expect_equal(pos$auto, rep(NA_character_, 2L))
    expect_equal(pos$default, rep(list(NA_character_), 2L))
    expect_equal(pos$choice, rep(list(NA_character_), 2L))
    expect_equal(pos$reference, list("NewMaterialName", NULL))
    expect_equal(con$definition()$field_reference(c(7, 8)), list("NewMaterialName", "NewMaterialName"))
    expect_equal(con$definition()$field_possible(c(7, 8)), con$possible_value(c(7, 8)))

    expect_output(con$print())
})
# }}}
