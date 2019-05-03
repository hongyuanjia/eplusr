context("Idf")

# Idf class{{{
test_that("Idf class", {
    eplusr_option(verbose_info = FALSE)
    use_idd(8.8, "auto")

    # can create new Idf object from string
    expect_silent(idf <- read_idf(text("idf", 8.8)))

    # Basic {{{
    # can get version
    expect_equal(idf$version(), as.numeric_version("8.8.0"))

    # can get path
    expect_equal(idf$path(), NULL)

    # can get group names in Idf
    expect_equal(idf$group_name(), c("Simulation Parameters",
        "Surface Construction Elements", "Thermal Zones and Surfaces"))

    # can get group names in Idd
    expect_equal(idf$group_name(sorted = FALSE),
        c("Surface Construction Elements",
          "Surface Construction Elements",
          "Thermal Zones and Surfaces",
          "Surface Construction Elements",
          "Simulation Parameters")
    )

    # can get group names in Idd
    expect_equal(idf$group_name(all = TRUE), use_idd(8.8)$group_name())

    # can get class names in Idf
    expect_equal(idf$class_name(sorted = FALSE),
        c("Material", "Construction", "BuildingSurface:Detailed", "Material", "Version"))

    # can get class names in Idf
    expect_equal(idf$class_name(sorted = TRUE),
        c("Version", "Material", "Construction", "BuildingSurface:Detailed"))

    # can get class names in Idd
    expect_equal(idf$class_name(all = TRUE), use_idd(8.8)$class_name())

    # can get all object ids
    expect_equal(idf$object_id(),
        list(Version = 5L, Material = c(1L, 4L), Construction = 2L,
            `BuildingSurface:Detailed` = 3L))
    expect_equal(idf$object_id(simplify = TRUE), 1L:5L)

    # can get all object ids of a single class
    expect_equal(idf$object_id("Version"), list(Version = 5L))
    expect_equal(idf$object_id("Version", simplify = TRUE), 5L)

    # can get object names
    expect_equal(idf$object_name(), list(Version = NA_character_, Material = c("WD01", "WD02"),
        Construction = "WALL-1", `BuildingSurface:Detailed` = "WALL-1PF"))
    expect_equal(idf$object_name(simplify = TRUE), c("WD01", "WALL-1", "WALL-1PF", "WD02", NA_character_))
    expect_equal(idf$object_name(c("Material", "Construction")),
        list(Material = c("WD01", "WD02"), Construction = "WALL-1"))
    expect_equal(idf$object_name(c("Material", "Construction"), simplify = TRUE),
        c("WD01", "WD02", "WALL-1"))

    # can get object num
    expect_equal(idf$object_num(), 5L)
    expect_equal(idf$object_num(c("Version", "Construction")), c(1L, 1L))
    expect_equal(idf$object_num(1), 1L)

    expect_is(idf$object_relation(2), "IdfRelation")
    # }}}

    # ASSERTION {{{
    # can check invalid group name
    expect_true(idf$is_valid_group("Simulation Parameters"))
    expect_false(idf$is_valid_group("Simulation_Parameters"))

    # can check invalid class name
    expect_true(idf$is_valid_class("Version"))
    expect_false(idf$is_valid_class("version"))

    # can check invalid object ID
    expect_true(idf$is_valid_id(1L))
    expect_false(idf$is_valid_id(6L))
    expect_equal(idf$is_valid_id(1L:4L), rep(TRUE, times = 4L))
    expect_error(idf$is_valid_id("1"))

    # can check invalid object name
    expect_true(idf$is_valid_name("WD01"))
    expect_true(idf$is_valid_name("wd01"))
    expect_false(idf$is_valid_name(NA_character_))
    expect_equal(idf$is_valid_name(c("wd01", "WALL-1")), c(TRUE, TRUE))

    # can check if model has been changed since read
    expect_false(idf$is_unsaved())
    # }}}

    # OBJECT {{{
    # can get IddObject
    expect_is(idf$definition("Version"), "IddObject")

    # can get IdfObject
    expect_is(idf$object(1), "IdfObject")
    expect_error(idf$object(1:2), class = "error_not_scalar")
    expect_equal(names(idf$objects("WD01")), "WD01")
    expect_equal(names(idf$objects("WALL-1")), "WALL-1")
    expect_is(idf$objects(1:2), "list")
    expect_error(idf$objects("a"), class = "error_object_name_lower")
    expect_error(idf$objects(1:6), class = "error_object_id")

    # can get all objects in a class
    expect_warning(obj <- idf$object_in_class("Version"), "deprecated")
    expect_equal(names(obj), NA_character_)
    expect_error(idf$objects_in_class("version"), class = "error_class_name")

    # can get all objects in relation
    expect_is(idf$objects_in_relation(2), "list")
    expect_equal(names(idf$objects_in_relation(2)), c("WALL-1", "WD01"))
    expect_is(idf$objects_in_relation("WALL-1", "ref_by"), "list")
    expect_equal(names(idf$objects_in_relation("WALL-1", "ref_by")), c("WALL-1", "WALL-1PF"))

    # can get objects using "[" and "$"
    expect_is(idf$Version, "IdfObject")
    expect_equal(names(idf$Material), c("WD01", "WD02"))
    expect_equal(names(idf[["Material"]]), c("WD01", "WD02"))

    # can search object
    expect_silent(nm <- names(idf$search_object("W")))
    expect_equal(nm, c("WD01", "WALL-1", "WALL-1PF", "WD02"))
    expect_equal(names(idf$search_object("W")), c("WD01", "WALL-1", "WALL-1PF", "WD02"))
    expect_equal(idf$search_object("ma;ldk"), NULL)

    # }}}

    # DUPLICATE {{{
    # can duplicate objects and assign new names
    expect_equal(names(idf$dup("WD01-DUP" = "WD01")), "WD01-DUP")
    expect_equal(idf$object(6)$name(), "WD01-DUP")
    expect_equal(idf$object("WD01-DUP")$value(2:5), idf$object("WD01")$value(2:5))
    expect_equal(idf$dup("WD01")[[1L]]$name(), "WD01_1")
    expect_error(idf$dup("WD01" = "WD01-DUP"), class = "error_validity")
    expect_equal(names(idf$dup(rep("WD01", times = 10L))),
        paste0("WD01_", 2:11))
    # }}}

    # ADD {{{
    # invalid value input format
    expect_error(idf$add("Material" = list(name = "mat"), "Construction"), class = "error_dot_invalid_format")
    expect_error(idf$add("Material" = character(0)), class = "error_dot_invalid_format")
    expect_error(idf$add("Material" = list()), class = "error_validity")
    expect_error(idf$add(Material =  list(list())), class = "error_dot_invalid_format")
    expect_error(idf$add("Material" = list(list(name = "mat"))), class = "error_dot_invalid_format")
    expect_error(idf$add(list("Material" = list(), Construction = NULL)), class = "error_dot_invalid_format")
    expect_error(idf$add("Material" = character(), "Construction" = list()), class = "error_dot_invalid_format")
    expect_error(idf$add("Material" = list(list(1))), class = "error_dot_invalid_format")
    # invalid comment input format
    expect_error(idf$add("Material" = list(.comment = "a")), class = "error_validity")
    expect_error(idf$add("Material" = list(.comment = character(0))), class = "error_validity")
    # mixed named and unnamed
    expect_error(idf$add("Material" = list(name = "rough", "rough")), class = "error_validity")
    # duplicated field names
    expect_error(idf$add("Material" = list(name = "a", name = "a")), class = "error_dot_dup_field_name")
    # adding existing unique
    expect_error(idf$add("Version" = list(8)), class = "error_add_version")
    expect_silent(idf_full <- read_idf(example()))
    expect_error(idf_full$add("Building" = list()), class = "error_add_unique")

    # adding empty object
    expect_silent(idf$add("Building" = list()))

    # invalid field number
    expect_error(idf$add("Output:Variable" = list("a", "b", "c", "d", "e")), class = "error_bad_field_index")

    # invalid field names
    # (a) non-extensible classes
    expect_error(idf$add("Material" = list(wrong = "Rough")), class = "error_bad_field_name")

    # (b) extensible classes
    expect_error(idf$add("Schedule:Week:Compact" = list(DayType_List_6 = "day6")), class = "error_validity")
    expect_error(idf$add("SurfaceProperty:HeatTransferAlgorithm:SurfaceList" = list(Name = "algo", Wrong = "Rough")), class = "error_bad_field_name")
    expect_error(idf$add("SurfaceProperty:HeatTransferAlgorithm:SurfaceList" = list(Name = "algo", Surface_Name_8 = "Rough")), class = "error_bad_field_name")
    expect_error(idf$add("Schedule:Week:Compact" =  list(DayType_List_7 = "day7", Schedule_Day_Name_6 = "sch6")), class = "error_validity")
    expect_error(idf$add("SurfaceProperty:HeatTransferAlgorithm:SurfaceList" = list(Surface_Name_8 = "surf8", Surface_Name_20 = "surf20")), class = "error_bad_field_name")
    expect_error(idf$add("Schedule:Week:Compact" = list(DayType_List_8 = "day8", Schedule_Day_Name_8 = "sch8")), class = "error_validity")
    expect_equal(use_idd(8.8)$SurfaceProperty_HeatTransferAlgorithm_SurfaceList$num_fields(), 8L)
    expect_equal(use_idd(8.8)$Schedule_Week_Compact$num_fields(), 17L)

    # incomplete extensible group
    # add all fields with defaults
    expect_equal(
        idf$add("RunPeriod" = list("rp_test_1", 1, 1, 2, 1),
            .default = TRUE, .all = TRUE)$rp_test_1$value(),
        list(Name = "rp_test_1",
             `Begin Month` = 1L,
             `Begin Day of Month` = 1L,
             `End Month` = 2L,
             `End Day of Month` = 1L,
             `Day of Week for Start Day` = "UseWeatherFile",
             `Use Weather File Holidays and Special Days` = "Yes",
             `Use Weather File Daylight Saving Period` = "Yes",
             `Apply Weekend Holiday Rule` = "No",
             `Use Weather File Rain Indicators` = "Yes",
             `Use Weather File Snow Indicators` = "Yes",
             `Number of Times Runperiod to be Repeated` = 1L,
             `Increment Day of Week on repeat` = "Yes",
             `Start Year` = NA_integer_
        )
    )
    expect_silent(
        idf$add(
            RunPeriod = list("rp_test_2", 1, 1, 2, 1,
                .comment = c("Comment for new object 1", "Another comment")
            ),
            RunPeriod = list(name = "rp_test_3", begin_month = 3, begin_day_of_month = 1, end_month = 4,
                end_day_of_month = 1, .comment = c("Comment for new object 2")
            )
        )
    )
    expect_equal(idf$object("rp_test_2")$value(simplify = TRUE),
        c("rp_test_2", "1", "1", "2", "1", "UseWeatherFile", "Yes", "Yes",
            "No", "Yes", "Yes")
    )
    expect_equal(idf$objects("rp_test_3")[[1]]$value(simplify = TRUE),
        c("rp_test_3", "3", "1", "4", "1", "UseWeatherFile", "Yes", "Yes",
            "No", "Yes", "Yes")
    )

    # can stop adding objects if trying to add a object with same name
    expect_error(idf$add(RunPeriod = list("rp_test_1", 1, 1, 2, 1)), class = "error_validity")
    # }}}

    # SET {{{
    # set new values and comments
    # name conflict
    expect_error(
        idf$set("rp_test_1" = list(name = "rp_test_3", begin_day_of_month = 2,
            .comment = c(format(Sys.Date()), "begin day has been changed."))
        ),
        class = "error_validity"
    )
    expect_silent(
        idf$set("rp_test_1" = list(name = "rp_test_4", begin_day_of_month = 2,
                .comment = c("begin day has been changed."))
        )
    )
    expect_equal(idf$RunPeriod$rp_test_4$Begin_Day_of_Month, 2L)
    expect_equal(idf$RunPeriod$rp_test_4$comment(), "begin day has been changed.")
    # can delete fields
    expect_silent(
        idf$set(rp_test_4 = list(
            use_weather_file_rain_indicators = NULL, use_weather_file_snow_indicators = NULL
        ))
    )
    expect_equal(idf$RunPeriod$rp_test_4$Use_Weather_File_Rain_Indicators, "Yes")
    expect_equal(idf$RunPeriod$rp_test_4$Use_Weather_File_Snow_Indicators, "Yes")
    expect_error(
        idf$set(rp_test_4 = list(
            Number_of_Times_Runperiod_to_be_Repeated = NULL,
            Number_of_Times_Runperiod_to_be_Repeated = NULL,
            Increment_Day_of_Week_on_repeat = NULL
        ))
    )
    expect_silent(
        idf$set(rp_test_4 = list(
            Number_of_Times_Runperiod_to_be_Repeated = NULL,
            Increment_Day_of_Week_on_repeat = NULL
        ), .default = FALSE)
    )
    expect_equal(length(idf$RunPeriod$rp_test_4$value()), 11)
    # }}}

    # INSERT {{{
    expect_error(idf$insert(list()), class = "error_wrong_type")
    expect_silent(idf$insert(idf_full$Material_NoMass$R13LAYER))
    expect_equal(idf$object_name("Material:NoMass", simplify = TRUE), "R13LAYER")
    expect_equal(idf_full$Material_NoMass$R13LAYER$value(simplify = TRUE),
        c("R13LAYER", "Rough", "2.290965", "0.9", "0.75", "0.75")
    )
    expect_error(idf$insert(idf_full$Version), class = "error_insert_version")
    expect_error(idf$insert(idf$Material_NoMass$R13LAYER, .unique = FALSE), class = "error_validity")
    expect_null(idf$insert(idf$Material_NoMass$R13LAYER))
    # }}}

    # DELETE {{{
    expect_error(idf$del(5L), class = "error_del_version")
    expect_error(idf$del(c(1, 2, 1)), class = "error_del_multi_time")
    expect_error(idf$del(idf$Building$id()), class = "error_del_required")
    expect_error(fixed = TRUE,
        idf_full$del(idf_full$Material_NoMass[[1]]$id()),
        class = "error_del_referenced"
    )
    expect_equal(eplusr_option(validate_level = "none"), list(validate_level = "none"))
    expect_silent(idf_full$del(12, .ref_by = TRUE, .force = TRUE))
    expect_equal(idf_full$is_valid_id(c(12, 15)), c(FALSE, TRUE))
    expect_equal(eplusr_option(validate_level = "final"), list(validate_level = "final"))
    # }}}

    # RENAME {{{
    idf <- read_idf(example())
    idf$rename(test = "C5 - 4 IN HW CONCRETE")
    expect_equal(idf$object_name("Material"), list(Material = "test"))
    expect_equal(idf$Construction$FLOOR$Outside_Layer, "test")
    # }}}

    # SEARCH AND REPLACE {{{
    # can create new Idf object from string
    expect_silent(idf <- read_idf(text("idf", 8.8)))

    expect_equal(
        vapply(idf$search_value("WALL"), function (x) x$id(), integer(1)),
        c(`WALL-1` = 2L, `WALL-1PF` = 3L)
    )
    expect_equal(
        vapply(idf$replace_value("WALL-1", "WALL-2"), function (x) x$id(), integer(1)),
        c(`WALL-2` = 2L, `WALL-2PF` = 3L)
    )
    # }}}

    # STRING {{{
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
    expect_equal(idf_1$to_string(format = "new_top"), idf_string)
    # }}}

    # TABLE {{{
    # can get idf in table format
    expect_silent(idf <- read_idf(text("idf", 8.8)))
    expect_equal(
        idf$to_table(2, unit = TRUE, string_value = TRUE),
        data.table(id = 2L, name = "WALL-1", class = "Construction", index = 1:5,
            field = c("Name", "Outside Layer", paste("Layer", 2:4)),
            value = c("WALL-1", "WD01", "PW03", "IN02", "GP01")
        )
    )
    expect_equal(
        idf$to_table(2, unit = FALSE, string_value = FALSE),
        data.table(id = 2L, name = "WALL-1", class = "Construction", index = 1:5,
            field = c("Name", "Outside Layer", paste("Layer", 2:4)),
            value = as.list(c("WALL-1", "WD01", "PW03", "IN02", "GP01"))
        )
    )
    expect_equivalent(tolerance = 1e-5,
        idf$to_table(1, unit = TRUE, string_value = FALSE),
        data.table(id = 1L, name = "WD01", class = "Material", index = 1:9,
            field = idf$definition("Material")$field_name(),
            value = list("WD01", "MediumSmooth", units::set_units(0.0191, m),
                units::set_units(0.115, W/K/m), units::set_units(513, kg/m^3),
                units::set_units(1381, J/K/kg), 0.9, 0.78, 0.78
            )
        )
    )
    # }}}

    # VALIDATE {{{
    expect_is(idf$validate(), "IdfValidity")
    expect_false(idf$is_valid())
    # }}}

    # SAVE {{{
    unlink(file.path(tempdir(), "test_save.idf"), force = TRUE)
    expect_silent(idf$save(file.path(tempdir(), "test_save.idf")))
    expect_error(idf$save(file.path(tempdir(), "test_save.idf")))
    # }}}

    # PRINT {{{
    skip_on_os("windows")
    idf <- read_idf(example())
    expect_output(idf$print("group"), "[06<C>] Group: <Surface Construction Elements>", fixed = TRUE)
    expect_output(idf$print("group", order = FALSE), "[06<C>] Group: <Surface Construction Elements>", fixed = TRUE)
    expect_output(idf$print("class"), fixed = TRUE,
        "Group: <Schedules>\n├─ [02<O>] Class: <ScheduleTypeLimits>\n└─ [01<O>] Class: <Schedule:Constant>"
    )
    expect_output(idf$print("object"), fixed = TRUE,
        "Class: <Schedule:Constant>\n└─ Object [ID:53] <AlwaysOn>"
    )
    expect_output(idf$print("field"), fixed = TRUE,
        "Class: <Schedule:Constant>\n└─ Object [ID:53] <AlwaysOn>"
    )
    # }}}

})
# }}}

# PASTE {{{
test_that("$paste() method in Idf class", {
    skip_if_not(is_windows())
    idf <- read_idf(example())
    text <- "IDF,BuildingSurface:Detailed,Surface,Wall,R13WALL,ZONE ONE,Outdoors,,SunExposed,WindExposed,0.5000000,4,0,0,4.572000,0,0,0,15.24000,0,0,15.24000,0,4.572000,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,;"
    writeClipboard(text)
    expect_is(idf$paste()[[1L]], "IdfObject")
    writeClipboard(text)
    expect_null(idf$paste())
})
# }}}
