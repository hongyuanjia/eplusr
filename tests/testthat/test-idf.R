context("Idf class")

eplusr_option(verbose_info = FALSE)
use_idd(8.8, "auto")

# NEW {{{
test_that("$new()", {
    # can create new Idf object from string
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    priv <- get_priv_env(idf)

    expect_null(priv$m_path)
    expect_is(priv$m_version, "numeric_version")
    expect_is(priv$m_idd, "Idd")
    expect_is(priv$m_idf_env, "environment")
    expect_is(priv$m_log, "environment")

    expect_is(priv$m_log$uuid, "character")
    expect_false(priv$m_log$unsaved)
    expect_false(priv$m_log$view_in_ip)
    expect_equal(priv$m_log$save_format, "sorted")
})
# }}}

# VERSION {{{
test_that("$version()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    expect_equal(idf$version(), as.numeric_version("8.8.0"))
})
# }}}

# PATH {{{
test_that("$path()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")
    expect_equal(idf$path(), NULL)

    expect_is(idf <- read_idf(example()), "Idf")
    expect_equal(basename(idf$path()), "1ZoneUncontrolled.idf")
})
# }}}

# GROUP_NAME {{{
test_that("$group_name()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

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
})
# }}}

# CLASS_NAME {{{
test_that("$class_name()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can get class names in Idf
    expect_equal(idf$class_name(sorted = FALSE),
        c("Material", "Construction", "BuildingSurface:Detailed", "Material", "Version"))

    # can get class names in Idf
    expect_equal(idf$class_name(sorted = TRUE),
        c("Version", "Material", "Construction", "BuildingSurface:Detailed"))

    # can get class names in Idd
    expect_equal(idf$class_name(all = TRUE), use_idd(8.8)$class_name())

    # can get class names by group
    expect_equal(length(idf$class_name(all = TRUE, by_group = TRUE)), 58)

    # can get class names by group
    expect_equal(idf$class_name(by_group = TRUE),
        list(`Simulation Parameters` = "Version",
             `Surface Construction Elements` = c("Material", "Construction"),
             `Thermal Zones and Surfaces` = "BuildingSurface:Detailed")
    )

    # by_group only works when sorted is TRUE
    expect_equal(idf$class_name(sorted = FALSE, by_group = TRUE),
        c("Material", "Construction", "BuildingSurface:Detailed", "Material", "Version"))
})
# }}}

# OBJECT_ID {{{
test_that("$object_id()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can get all object ids
    expect_equal(idf$object_id(),
        list(Version = 5L, Material = c(1L, 4L), Construction = 2L,
            `BuildingSurface:Detailed` = 3L))
    expect_equal(idf$object_id(simplify = TRUE), 1L:5L)

    # can get all object ids of a single class
    expect_equal(idf$object_id("Version"), list(Version = 5L))
    expect_equal(idf$object_id("Version", simplify = TRUE), 5L)
})
# }}}

# OBJECT_NAME {{{
test_that("$object_name()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can get object names
    expect_equal(idf$object_name(), list(Version = NA_character_, Material = c("WD01", "WD02"),
        Construction = "WALL-1", `BuildingSurface:Detailed` = "WALL-1PF"))
    expect_equal(idf$object_name(simplify = TRUE), c("WD01", "WALL-1", "WALL-1PF", "WD02", NA_character_))
    expect_equal(idf$object_name(c("Material", "Construction")),
        list(Material = c("WD01", "WD02"), Construction = "WALL-1"))
    expect_equal(idf$object_name(c("Material", "Construction"), simplify = TRUE),
        c("WD01", "WD02", "WALL-1"))
})
# }}}

# OBJECT_NUM {{{
test_that("$object_num()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can get object num
    expect_equal(idf$object_num(), 5L)
    expect_equal(idf$object_num(c("Version", "Construction")), c(1L, 1L))
    expect_equal(idf$object_num(1), 1L)
})
# }}}

# IS_VALID_GROUP {{{
test_that("$is_valid_group()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can check invalid group name
    expect_true(idf$is_valid_group("Simulation Parameters"))
    expect_false(idf$is_valid_group("Simulation_Parameters"))
})
# }}}

# IS_VALID_CLASS {{{
test_that("$is_valid_class()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can check invalid class name
    expect_true(idf$is_valid_class("Version"))
    expect_false(idf$is_valid_class("version"))
})
# }}}

# IS_VALID_ID {{{
test_that("$is_valid_id()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can check invalid object ID
    expect_true(idf$is_valid_id(1L))
    expect_false(idf$is_valid_id(6L))
    expect_equal(idf$is_valid_id(1L:4L), rep(TRUE, times = 4L))
    expect_error(idf$is_valid_id("1"))
})
# }}}

# IS_VALID_NAME {{{
test_that("$is_valid_name()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can check invalid object name
    expect_true(idf$is_valid_name("WD01"))
    expect_true(idf$is_valid_name("wd01"))
    expect_error(idf$is_valid_name(NA_character_))
    expect_equal(idf$is_valid_name(c("wd01", "WALL-1")), c(TRUE, TRUE))
})
# }}}

# IS_UNSAVED {{{
test_that("$is_unsaved()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can check if model has been changed since read
    expect_false(idf$is_unsaved())
})
# }}}

# DEFINITION {{{
test_that("$definition()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can get Idd
    expect_is(idf$definition(), "Idd")

    # can get IddObject
    expect_is(idf$definition("Version"), "IddObject")
})
# }}}

# OBJECT {{{
test_that("$object()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can get IdfObject
    expect_is(idf$object(1), "IdfObject")

    # can stop if multiple inputs
    expect_error(idf$object(1:2), "Assertion")
})
# }}}

# OBJECT_UNIQUE {{{
test_that("$object_unique()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can stop if not a unique-class name
    expect_error(idf$object_unique("Material"))

    # can stop if multiple objects in unique class found
    expect_silent(
        idf <- with_option(
            list(validate_level = "none", verbose_info = FALSE),
            {
                idf <- empty_idf(8.8)
                idf$add(Building = list(), Building = list())
                idf
            }
        )
    )
    expect_error(idf$object_unique("Building"), class = "eplusr_error")
})
# }}}

# OBJECTS {{{
test_that("$objects()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    expect_equal(names(idf$objects("WD01")), "WD01")

    # can ignore cases
    expect_equal(names(idf$objects("wall-1")), "WALL-1")

    expect_is(idf$objects(1:2), "list")
    expect_error(idf$objects("a"), class = "eplusr_error_invalid_object_name")
    expect_error(idf$objects(1:6), class = "eplusr_error_invalid_object_id")
})
# }}}

# OBJECTS_IN_CLASS {{{
test_that("$objects_in_class()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can get all objects in a class
    expect_error(idf$objects_in_class("version"), class = "eplusr_error_invalid_class_name")
    expect_is(idf$objects_in_class("Version"), "list")
})
# }}}

# OBJECTS_IN_GROUP {{{
test_that("$objects_in_group()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can get all objects in a group
    expect_error(idf$objects_in_group("Schedules"), class = "eplusr_error_invalid_group_name")
    expect_is(idf$objects_in_group("Simulation Parameters"), "list")
})
# }}}

# OBJECT_RELATION {{{
test_that("$object_relation()", {
    skip_on_cran()
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")
    expect_is(idf$object_relation(2), "IdfRelation")

    idf_1 <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/5Zone_Transformer.idf"))

    # default only include both objects that are both referenced by their field
    # value and class names
    ref <- idf_1$object_relation(idf_1$Branch[[1]]$id(), direction = "ref_to")
    expect_equal(nrow(ref$ref_to), 8L)
    expect_equal(unique(ref$ref_to$src_object_name),
        c("OA Sys 1", "Main Cooling Coil 1", "Main Heating Coil 1", "Supply Fan 1")
    )

    # can exclude all class-name-reference
    ref <- idf_1$object_relation(idf_1$Branch[[1]]$id(), direction = "ref_to", class_ref = "none")
    expect_equal(nrow(ref$ref_to), 4L)
    expect_equal(unique(ref$ref_to$src_object_name),
        c("OA Sys 1", "Main Cooling Coil 1", "Main Heating Coil 1", "Supply Fan 1")
    )

    # can include all possible objects that are class-name-referenced
    ref <- idf_1$object_relation(idf_1$Branch[[1]]$id(), direction = "ref_to", class_ref = "all")
    expect_equal(nrow(ref$ref_to), 15L)
    expect_equal(unique(ref$ref_to$src_object_name),
        c(
        "OA Sys 1",
        "OA Cooling Coil 1",
        "Main Cooling Coil 1",
        "SPACE1-1 Zone Coil",
        "SPACE2-1 Zone Coil",
        "SPACE3-1 Zone Coil",
        "SPACE4-1 Zone Coil",
        "SPACE5-1 Zone Coil",
        "OA Heating Coil 1",
        "Main Heating Coil 1",
        "Supply Fan 1"
        )
    )
})
# }}}

# OBJECTS_IN_RELATION {{{
test_that("$objects_in_relation()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    expect_is(obj <- idf$objects_in_relation(2), "list")
    expect_equal(length(obj), 2L)
    expect_equal(names(obj), c("WALL-1", "WD01"))

    expect_message(with_verbose(obj <- idf$objects_in_relation(1)), "does not refer to")
    expect_message(with_verbose(obj <- idf$objects_in_relation(1, class = "Material")), "does not refer to")
    expect_equal(length(obj), 1L)
    expect_equal(names(obj), "WD01")

    expect_is(obj <- idf$objects_in_relation("WALL-1", "ref_by"), "list")
    expect_equal(names(obj), c("WALL-1", "WALL-1PF"))
})
# }}}

# SEARCH_OBJECT {{{
test_that("$search_object()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    expect_error(idf$search_object("W", class = rep("Version", 2)))

    expect_is(obj <- idf$search_object("W"), "list")
    expect_equal(names(obj), c("WD01", "WALL-1", "WALL-1PF", "WD02"))

    expect_equal(names(idf$search_object("W", class = "Material")), c("WD01", "WD02"))

    expect_equal(idf$search_object("AAA"), NULL)
})
# }}}

# DUP {{{
test_that("$dup()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can duplicate objects and assign new names
    expect_equal(names(idf$dup("WD01-DUP" = "WD01")), "WD01-DUP")
    expect_equal(idf$object(6)$name(), "WD01-DUP")
    expect_equal(idf$object("WD01-DUP")$value(2:5), idf$object("WD01")$value(2:5))
    expect_equal(idf$dup("WD01")[[1L]]$name(), "WD01 1")
    expect_error(idf$dup("WD01" = "WD01-DUP"), class = "eplusr_error_conflict_name")
    expect_equal(names(idf$dup(rep("WD01", times = 10L))), paste0("WD01 ", 2:11))
})
# }}}

# ADD {{{
test_that("$add()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # invalid value input format
    expect_error(idf$add("Material" = list(name = "mat"), "Construction"))
    expect_error(idf$add("Material" = character(0)))
    expect_error(idf$add("Material" = list()))
    expect_error(idf$add(Material =  list(list())))
    expect_error(idf$add("Material" = list(list(name = "mat"))))
    expect_error(idf$add(list("Material" = list(), Construction = NULL)))
    expect_error(idf$add("Material" = character(), "Construction" = list()))
    expect_error(idf$add("Material" = list(list(1))))
    # invalid comment input format
    expect_error(idf$add("Material" = list(.comment = "a")))
    expect_error(idf$add("Material" = list(.comment = character(0))))
    # mixed named and unnamed
    expect_error(idf$add("Material" = list(name = "rough", "rough")))
    # duplicated field names
    expect_error(idf$add("Material" = list(name = "a", name = "a")))
    # adding existing unique
    expect_error(idf$add("Version" = list(8)))
    expect_silent(idf_full <- read_idf(example()))
    expect_error(idf_full$add("Building" = list()))

    # adding empty object
    expect_is(idf$add("Building" = list())[[1L]], "IdfObject")

    # invalid field number
    expect_error(idf$add("Output:Variable" = list("a", "b", "c", "d", "e")), class = "eplusr_error_invalid_field_index")

    # invalid field names
    # (a) non-extensible classes
    expect_error(idf$add("Material" = list(wrong = "Rough")), class = "eplusr_error_invalid_field_name")

    # (b) extensible classes
    expect_error(idf$add("Schedule:Week:Compact" = list(DayType_List_6 = "day6")), class = "eplusr_error_validity_check")
    expect_error(idf$add("SurfaceProperty:HeatTransferAlgorithm:SurfaceList" = list(Name = "algo", Wrong = "Rough")), class = "eplusr_error_invalid_field_name")
    expect_error(idf$add("SurfaceProperty:HeatTransferAlgorithm:SurfaceList" = list(Name = "algo", Surface_Name_8 = "Rough")), class = "eplusr_error_invalid_field_name")
    expect_error(idf$add("Schedule:Week:Compact" = list(DayType_List_7 = "day7", Schedule_Day_Name_6 = "sch6")), class = "eplusr_error_validity_check")
    expect_error(idf$add("SurfaceProperty:HeatTransferAlgorithm:SurfaceList" = list(Surface_Name_8 = "surf8", Surface_Name_20 = "surf20")), class = "eplusr_error_invalid_field_name")
    expect_error(idf$add("Schedule:Week:Compact" = list(DayType_List_8 = "day8", Schedule_Day_Name_8 = "sch8")), class = "eplusr_error_validity_check")
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
    expect_error(idf$add(RunPeriod = list("rp_test_1", 1, 1, 2, 1)), class = "eplusr_error_validity_check")
})
# }}}

# SET {{{
test_that("$set()", {
    expect_is(idf <- read_idf(example()), "Idf")

    # set new values and comments
    expect_is(class = "list",
        idf$set(..8 = list(name = "rp_test", begin_day_of_month = 2,
                use_weather_file_rain_indicators = "no",
                .comment = c("begin day has been changed."))
        )
    )
    expect_equal(idf$RunPeriod$rp_test$Begin_Day_of_Month, 2L)
    expect_equal(idf$RunPeriod$rp_test$Use_Weather_File_Rain_Indicators, "no")
    expect_equal(idf$RunPeriod$rp_test$comment(), "begin day has been changed.")

    # can set default values
    expect_is(class = "list",
        idf$set(rp_test = list(
            use_weather_file_rain_indicators = NULL, use_weather_file_snow_indicators = NULL
        ))
    )
    expect_equal(length(idf$RunPeriod$rp_test$value()), 11)
    expect_equal(idf$RunPeriod$rp_test$Use_Weather_File_Rain_Indicators, "Yes")

    # can remove trailing empty fields
    expect_is(class = "list",
        idf$set(rp_test = list(
            Number_of_Times_Runperiod_to_be_Repeated = NULL,
            Increment_Day_of_Week_on_repeat = NULL
        ), .default = FALSE)
    )
    expect_equal(length(idf$RunPeriod$rp_test$value()), 11)

    # can keep trailing empty fields
    expect_is(class = "list",
        idf$set(rp_test = list(start_year = NULL), .default = FALSE, .empty = TRUE)
    )
    expect_equal(length(idf$RunPeriod$rp_test$value()), 14)

    # can set all values in a class
    expect_is(class = "list",
        idf$set(
            RunPeriod := list(start_year = NULL),
            Material_NoMass := list(roughness = "Rough")
        )
    )
    expect_equal(idf$Material_NoMass$R13LAYER$Roughness, "Rough")
    expect_equal(idf$Material_NoMass$R31LAYER$Roughness, "Rough")

    # can set multiple objects
    expect_is(class = "list",
        idf$set(c(12, 13) := list(roughness = c("VeryRough", "Smooth")))
    )
    expect_equal(idf$Material_NoMass$R13LAYER$Roughness, "VeryRough")
    expect_equal(idf$Material_NoMass$R31LAYER$Roughness, "Smooth")

    # can handle references
    skip_on_cran()
    expect_is(idf <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/5Zone_Transformer.idf")), "Idf")
    expect_is(idf$set("Pump:VariableSpeed" := list(c("pump1", "pump2"))), "list")
    expect_equal(idf$"Pump:VariableSpeed"[[1]]$ref_by_object(class = "Branch")[[1L]]$Component_1_Object_Type, "Pump:VariableSpeed")
})
# }}}

# DEL {{{
test_that("$del()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    expect_error(idf$del(5L), class = "eplusr_error_del_version")
    expect_error(idf$del(c(1, 2, 1)), class = "eplusr_error_del_same")

    expect_is(idf <- read_idf(example()), "Idf")
    expect_error(idf$del(idf$Material_NoMass[[1]]$id()), class = "eplusr_error_del_referenced")
    expect_error(idf$del(idf$Building$id()), class = "eplusr_error_del_required")

    expect_is(without_checking(idf$del(12, .ref_by = TRUE)), "Idf")
    expect_equal(idf$is_valid_id(c(12, 15)), c(FALSE, TRUE))
    expect_false(idf$object("R13WALL")$is_valid())

    expect_is(idf <- read_idf(example()), "Idf")
    expect_is(idf$del(12, .ref_by = TRUE, .force = TRUE), "Idf")
    expect_equal(idf$is_valid_id(c(12, 15)), c(FALSE, FALSE))
})
# }}}

# PURGE {{{
test_that("$purge()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    expect_error(idf$purge(1000), class = "eplusr_error_invalid_object_id")
    expect_error(idf$purge(class = "AirLoopHVAC"), class = "eplusr_error_invalid_class_name")
    expect_error(idf$purge(group = "Schedules"), class = "eplusr_error_invalid_group_name")

    # can skip non-resource object
    expect_true(idf$purge(5)$is_valid_id(5))

    # can purge not used resource object
    expect_false(idf$purge(4)$is_valid_id(4))

    # can skip if object is still referenced by others
    expect_true(idf$purge(1)$is_valid_id(1))
    expect_true(idf$purge(2)$is_valid_id(2))
    expect_true(all(idf$purge(c(1, 2))$is_valid_id(c(1, 2))))

    # can purge considering input relations
    expect_false(all(idf$purge(c(1, 2, 3))$is_valid_id(c(1, 2, 3))))

    # can purge using input class names
    idf <- read_idf(text("idf", 8.8))
    expect_equal(idf$purge(class = "Material")$is_valid_id(c(1, 4)), c(TRUE, FALSE))
    idf <- read_idf(text("idf", 8.8))
    expect_equal(idf$purge(class = c("Material", "Construction"))$is_valid_id(c(1, 4)), c(TRUE, FALSE))

    # can purge using input class names
    idf <- read_idf(text("idf", 8.8))
    expect_equal(idf$purge(group = "Surface Construction Elements")$is_valid_id(c(1, 4)), c(TRUE, FALSE))

    # can purge using various specifications
    idf <- read_idf(text("idf", 8.8))
    expect_equal(idf$purge(1:5, c("Material", "Construction"), idf$group_name())$is_valid_id(1:5), c(rep(FALSE, 4), TRUE))
})
# }}}

# DUPLICATED {{{
test_that("$duplicated()", {
    skip_on_cran()
    idf_1 <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/5Zone_Transformer.idf"))
    expect_silent(dup <- idf_1$duplicated())
    expect_equal(nrow(dup), 322)
    expect_equal(names(dup), c("class", "id", "name", "duplicate"))
    expect_equal(dup$class, idf_1$class_name(sorted = FALSE))
    expect_equal(dup$id, 1:322)
    expect_equal(dup$name, idf_1$object_name(simplify = TRUE))
    expect_equal(dup[!is.na(duplicate), id], c(35L, 38L, 42L, 77L, 78L, 152L, 154L, 156L, 158L))
    expect_equal(idf_1$duplicated(class = "Schedule:Compact")[!is.na(duplicate), id], c(35L, 38L, 42L))
    expect_equal(idf_1$duplicated(group = "Schedules")[!is.na(duplicate), id], c(35L, 38L, 42L))
})
# }}}

# UNIQUE {{{
test_that("$unique()", {
    skip_on_cran()
    idf_1 <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/5Zone_Transformer.idf"))
    expect_is(idf_1$unique(1, group = "Schedules"), "Idf")
    expect_false(all(idf_1$is_valid_id(c(35, 38, 42))))

    id <- idf_1$object_id(c("Material", "Output:Meter:MeterFileOnly"))
    expect_is(idf_1$unique(class = c("Material", "Output:Meter:MeterFileOnly")), "Idf")
    expect_equal(idf_1$object_id(c("Material", "Output:Meter:MeterFileOnly")), id)
})
# }}}

# RENAME {{{
test_that("$rename()", {
    idf <- read_idf(example())
    expect_is(idf$rename(test = "C5 - 4 IN HW CONCRETE"), "list")
    expect_equal(idf$object_name("Material"), list(Material = "test"))
    expect_equal(idf$Construction$FLOOR$Outside_Layer, "test")
})
# }}}

# INSERT {{{
test_that("$insert()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")
    expect_is(idf_full <- read_idf(example()), "Idf")

    expect_error(idf$insert(list()), class = "eplusr_error_dots_format")

    expect_is(idf$insert(idf_full$Material_NoMass$R13LAYER), "list")
    expect_equal(idf$object_name("Material:NoMass", simplify = TRUE), "R13LAYER")
    expect_equal(idf_full$Material_NoMass$R13LAYER$value(simplify = TRUE),
        c("R13LAYER", "Rough", "2.290965", "0.9", "0.75", "0.75")
    )

    # can skip Version object
    expect_message(with_verbose(idf$insert(idf_full$Version)), "skipped")

    # can remove same object
    expect_error(idf$insert(idf$Material_NoMass$R13LAYER, .unique = FALSE), class = "eplusr_error_validity_check")
    expect_null(idf$insert(idf$Material_NoMass$R13LAYER))
    expect_null(idf$insert(idf$Material_NoMass$R13LAYER, idf$Material_NoMass$R13LAYER))

    idf1 <- empty_idf(8.8)
    idf2 <- empty_idf(8.8)
    expect_is(idf1$add(ScheduleTypeLimits = list("Fraction", 0, 1, "continuous")), "list")
    expect_is(idf2$add(ScheduleTypeLimits = list("Fraction", 0, 1, "Continuous")), "list")
    expect_null(idf1$insert(idf2$ScheduleTypeLimits$Fraction))
    expect_equal(idf1$object_id()$ScheduleTypeLimits, 2L)
    # can directly insert an Idf
    expect_null(idf1$insert(idf2))
    expect_equal(idf1$object_id(), list(Version = 1L, ScheduleTypeLimits = 2L))
})
# }}}

# SEARCH_VALUE {{{
test_that("$search_value()", {
    # can create new Idf object from string
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    expect_null(idf$search_value("AAA"))
    expect_equal(
        vapply(idf$search_value("WALL"), function (x) x$id(), integer(1)),
        c(`WALL-1` = 2L, `WALL-1PF` = 3L)
    )
})
# }}}

# REPLACE_VALUE {{{
test_that("$replace_value()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    expect_equal(
        vapply(idf$replace_value("WALL-1", "WALL-2"), function (x) x$id(), integer(1)),
        c(`WALL-2` = 2L, `WALL-2PF` = 3L)
    )
})
# }}}

# PASTE {{{
test_that("$paste()", {
    idf <- read_idf(example())
    if (!is_windows()) expect_error(idf$paste())

    skip_if_not(is_windows())
    text <- "IDF,BuildingSurface:Detailed,Surface,Wall,R13WALL,ZONE ONE,Outdoors,,SunExposed,WindExposed,0.5000000,4,0,0,4.572000,0,0,0,15.24000,0,0,15.24000,0,4.572000,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,;"
    writeClipboard(text)
    expect_is(idf$paste()[[1L]], "IdfObject")
    writeClipboard(text)
    expect_null(idf$paste())
})
# }}}

# LOAD {{{
test_that("$load()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    # can error if trying to add Version
    expect_error(idf$load("Version,8.7;\n"))

    expect_is(idf$load("SimulationControl,no;\n"), "list")
    expect_is(idf$SimulationControl, "IdfObject")

    expect_is(class = "list",
        idf$load(
            c("! some comments;",
              "Material,",
              "    mat,                     !- Name",
              "    MediumSmooth,            !- Roughness",
              "    0.667,                   !- Thickness {m}",
              "    0.5,",
              "    800,",
              "    300;",
              "Construction, const, mat;"
            )
        )
    )
    expect_is(idf$Material$mat, "IdfObject")

    expect_is(class = "list",
        {
            dt <- idf$to_table(class = rep("Material:NoMass", 2), init = TRUE)[
                ,by = "id", value := c("mat", "Smooth", "0.5")
            ][index == 1L, value := paste(value, 1:2, sep = "_")]
            obj <- idf$load(dt)
        }
    )
    expect_equal(idf$Material_NoMass$mat_1$Roughness, "Smooth")
    expect_equal(idf$Material_NoMass$mat_2$Roughness, "Smooth")
})
# }}}

# UPDATE {{{
test_that("$update()", {
    expect_is(idf <- read_idf(example()), "Idf")

    # can stop if trying to update non-named objects using string
    expect_error(idf$update("SimulationControl, no;\n"))

    expect_is(idf$update("Material:NoMass, R13LAYER, Smooth, 2;\n"), "list")
    expect_equal(idf$Material_NoMass$R13LAYER$Roughness, "Smooth")

    expect_is(class = "list",
        {
            idf$update(idf$to_table("r13layer")[2][, value := "Rough"])
        }
    )
    expect_equal(idf$Material_NoMass$R13LAYER$Roughness, "Rough")
})
# }}}

# VALIDATE {{{
test_that("$validate()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    expect_is(val <- idf$validate(), "IdfValidity")
    expect_equal(val$missing_object, c("Building", "GlobalGeometryRules"))
    expect_equal(nrow(val$duplicate_object), 0)
    expect_equal(nrow(val$conflict_name), 0)
    expect_equal(nrow(val$incomplete_extensible), 0)
    expect_equal(nrow(val$missing_value), 2)
    expect_equal(nrow(val$invalid_autosize), 0)
    expect_equal(nrow(val$invalid_autocalculate), 0)
    expect_equal(nrow(val$invalid_character), 0)
    expect_equal(nrow(val$invalid_numeric), 0)
    expect_equal(nrow(val$invalid_integer), 0)
    expect_equal(nrow(val$invalid_choice), 0)
    expect_equal(nrow(val$invalid_range), 0)
    expect_equal(nrow(val$invalid_reference), 4)
    expect_equal(val$invalid_reference,
        data.table(object_id = c(2L, 2L, 2L, 3L),
            object_name = c("WALL-1", "WALL-1", "WALL-1", "WALL-1PF"),
            class_id = c(90L, 90L, 90L, 103L),
            class_name = c("Construction", "Construction", "Construction", "BuildingSurface:Detailed"),
            field_id = c(11008L, 11009L, 11010L, 11625L),
            field_index = c(3L, 4L, 5L, 4L),
            field_name = c("Layer 2", "Layer 3", "Layer 4", "Zone Name"),
            units = c(NA_character_, NA_character_, NA_character_, NA_character_),
            ip_units = c(NA_character_, NA_character_, NA_character_, NA_character_),
            type_enum = c(5L, 5L, 5L, 5L),
            value_id = c(12L, 13L, 14L, 18L),
            value_chr = c("PW03", "IN02", "GP01", "PLENUM-1"),
            value_num = c(NA_real_, NA_real_, NA_real_, NA_real_)
        )
    )
})
# }}}

# IS_VALID {{{
test_that("$is_valid()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    expect_false(idf$is_valid())
})
# }}}

# TO_STRING {{{
test_that("$to_string()", {
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

    expect_is(idf <- read_idf(example()), "Idf")
    expect_equal(idf$to_string()[2], "!-Option OriginalOrderTop")
})
# }}}

# TO_TABLE {{{
test_that("$to_table()", {
    # can get idf in table format
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")
    expect_is(idf$to_table(), "data.table")
    expect_is(idf$to_string(), "character")
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
            value = list("WD01", "MediumSmooth", set_units(0.0191, m),
                set_units(0.115, W/K/m), set_units(513, kg/m^3),
                set_units(1381, J/K/kg), 0.9, 0.78, 0.78
            )
        )
    )
    expect_equal(
        idf$to_table(3, unit = TRUE, string_value = TRUE, group_ext = "group"),
        data.table(id = 3L, name = "WALL-1PF", class = "BuildingSurface:Detailed",
            index = 1:15,
            field = c(
                "Name",
                "Surface Type",
                "Construction Name",
                "Zone Name",
                "Outside Boundary Condition",
                "Outside Boundary Condition Object",
                "Sun Exposure",
                "Wind Exposure",
                "View Factor to Ground",
                "Number of Vertices",
                "Vrtx1X-crd|Vrtx1Y-crd|Vrtx1Z-crd",
                "Vrtx2X-crd|Vrtx2Y-crd|Vrtx2Z-crd",
                "Vrtx3X-crd|Vrtx3Y-crd|Vrtx3Z-crd",
                "Vrtx4X-crd|Vrtx4Y-crd|Vrtx4Z-crd",
                "Vrtx5X-crd|Vrtx5Y-crd|Vrtx5Z-crd"
            ),
            value = list(
                "WALL-1PF",
                "WALL",
                "WALL-1",
                "PLENUM-1",
                "Outdoors",
                NA_character_,
                "SunExposed",
                "WindExposed",
                "0.5",
                "4",
                c("0", "0", "3"),
                c("0", "0", "2.4"),
                c("30.5", "0", "2.4"),
                c("30.5", "0", "3"),
                rep(NA_character_, 3L)
            )
        )
    )
    expect_equal(
        idf$to_table(3, unit = TRUE, string_value = TRUE, group_ext = "index"),
        data.table(id = 3L, name = "WALL-1PF", class = "BuildingSurface:Detailed",
            index = 1:13,
            field = c(
                "Name",
                "Surface Type",
                "Construction Name",
                "Zone Name",
                "Outside Boundary Condition",
                "Outside Boundary Condition Object",
                "Sun Exposure",
                "Wind Exposure",
                "View Factor to Ground",
                "Number of Vertices",
                "Vertex X-coordinate",
                "Vertex Y-coordinate",
                "Vertex Z-coordinate"
            ),
            value = list(
                "WALL-1PF",
                "WALL",
                "WALL-1",
                "PLENUM-1",
                "Outdoors",
                NA_character_,
                "SunExposed",
                "WindExposed",
                "0.5",
                "4",
                c("0", "0", "30.5", "30.5", NA_character_),
                c("0", "0", "0", "0", NA_character_),
                c("3", "2.4", "2.4", "3", NA_character_)
            )
        )
    )
    expect_equivalent(tolerance = 1e-5,
        idf$to_table(3, unit = TRUE, string_value = FALSE, group_ext = "group"),
        data.table(id = 3L, name = "WALL-1PF", class = "BuildingSurface:Detailed",
            index = 1:15,
            field = c(
                "Name",
                "Surface Type",
                "Construction Name",
                "Zone Name",
                "Outside Boundary Condition",
                "Outside Boundary Condition Object",
                "Sun Exposure",
                "Wind Exposure",
                "View Factor to Ground",
                "Number of Vertices",
                "Vrtx1X-crd|Vrtx1Y-crd|Vrtx1Z-crd",
                "Vrtx2X-crd|Vrtx2Y-crd|Vrtx2Z-crd",
                "Vrtx3X-crd|Vrtx3Y-crd|Vrtx3Z-crd",
                "Vrtx4X-crd|Vrtx4Y-crd|Vrtx4Z-crd",
                "Vrtx5X-crd|Vrtx5Y-crd|Vrtx5Z-crd"
            ),
            value = list(
                "WALL-1PF",
                "WALL",
                "WALL-1",
                "PLENUM-1",
                "Outdoors",
                NA_character_,
                "SunExposed",
                "WindExposed",
                0.5,
                4,
                list(set_units(0., "m"), set_units(0., "m"), set_units(3., "m")),
                list(set_units(0., "m"), set_units(0., "m"), set_units(2.4, "m")),
                list(set_units(30.5, "m"), set_units(0., "m"), set_units(2.4, "m")),
                list(set_units(30.5, "m"), set_units(0., "m"), set_units(3., "m")),
                rep(list(set_units(NA_real_, "m")), 3L)
            )
        )
    )
    expect_equivalent(tolerance = 1e-5,
        idf$to_table(3, unit = TRUE, string_value = FALSE, group_ext = "index"),
        data.table(id = 3L, name = "WALL-1PF", class = "BuildingSurface:Detailed",
            index = 1:13,
            field = c(
                "Name",
                "Surface Type",
                "Construction Name",
                "Zone Name",
                "Outside Boundary Condition",
                "Outside Boundary Condition Object",
                "Sun Exposure",
                "Wind Exposure",
                "View Factor to Ground",
                "Number of Vertices",
                "Vertex X-coordinate",
                "Vertex Y-coordinate",
                "Vertex Z-coordinate"
            ),
            value = list(
                "WALL-1PF",
                "WALL",
                "WALL-1",
                "PLENUM-1",
                "Outdoors",
                NA_character_,
                "SunExposed",
                "WindExposed",
                0.5,
                4,
                set_units(c(0., 0., 30.5, 30.5, NA_real_), "m"),
                set_units(c(0., 0., 0., 0., NA_real_), "m"),
                set_units(c(3., 2.4, 2.4, 3., NA_real_), "m")
            )
        )
    )
    expect_equivalent(tolerance = 1e-5,
        idf$to_table(3, unit = TRUE, string_value = FALSE, group_ext = "group", wide = TRUE),
        data.table(id = 3L, name = "WALL-1PF", class = "BuildingSurface:Detailed",
            "Name" = "WALL-1PF",
            "Surface Type" = "WALL",
            "Construction Name" = "WALL-1",
            "Zone Name" = "PLENUM-1",
            "Outside Boundary Condition" = "Outdoors",
            "Outside Boundary Condition Object" = NA_character_,
            "Sun Exposure" = "SunExposed",
            "Wind Exposure" = "WindExposed",
            "View Factor to Ground" = 0.5,
            "Number of Vertices" = 4.,
            "Vrtx1X-crd|Vrtx1Y-crd|Vrtx1Z-crd" = list(list(set_units(0., "m"), set_units(0., "m"), set_units(3., "m"))),
            "Vrtx2X-crd|Vrtx2Y-crd|Vrtx2Z-crd" = list(list(set_units(0., "m"), set_units(0., "m"), set_units(2.4, "m"))),
            "Vrtx3X-crd|Vrtx3Y-crd|Vrtx3Z-crd" = list(list(set_units(30.5, "m"), set_units(0., "m"), set_units(2.4, "m"))),
            "Vrtx4X-crd|Vrtx4Y-crd|Vrtx4Z-crd" = list(list(set_units(30.5, "m"), set_units(0., "m"), set_units(3., "m"))),
            "Vrtx5X-crd|Vrtx5Y-crd|Vrtx5Z-crd" = list(rep(list(set_units(NA_real_, "m")), 3L))
        )
    )
    expect_equivalent(tolerance = 1e-5,
        idf$to_table(3, unit = TRUE, string_value = FALSE, group_ext = "index", wide = TRUE),
        data.table(id = 3L, name = "WALL-1PF", class = "BuildingSurface:Detailed",
            "Name" = "WALL-1PF",
            "Surface Type" = "WALL",
            "Construction Name" = "WALL-1",
            "Zone Name" = "PLENUM-1",
            "Outside Boundary Condition" = "Outdoors",
            "Outside Boundary Condition Object" = NA_character_,
            "Sun Exposure" = "SunExposed",
            "Wind Exposure" = "WindExposed",
            "View Factor to Ground" = 0.5,
            "Number of Vertices" = 4.,
            "Vertex X-coordinate" = list(set_units(c(0., 0., 30.5, 30.5, NA_real_), "m")),
            "Vertex Y-coordinate" = list(set_units(c(0., 0., 0., 0., NA_real_), "m")),
            "Vertex Z-coordinate" = list(set_units(c(3., 2.4, 2.4, 3., NA_real_), "m"))
        )
    )
})
# }}}

# SAVE {{{
test_that("$save()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")
    expect_error(idf$save(), "not created from local", "eplusr_error")

    unlink(file.path(tempdir(), "test_save.idf"), force = TRUE)
    expect_silent(idf$save(file.path(tempdir(), "test_save.idf")))
    expect_error(idf$save(file.path(tempdir(), "test_save.idf")))
    expect_is(idf$save(overwrite = TRUE), "character")
})
# }}}

# RUN {{{
test_that("$run()", {
    skip_on_cran()
    expect_error(read_idf(text("idf", 8.8))$save(), class = "eplusr_error")
    expect_is(idf <- read_idf(example()), "Idf")
    expect_is(job <- idf$run(NULL, tempdir(), echo = FALSE), "EplusJob")

    expect_silent(idf$set(..12 = list(roughness = "smooth")))
    expect_error(idf$run())
    expect_error({idf$save(tempfile(fileext = ".idf"));idf$run(echo = FALSE)})
})
# }}}

# LAST_JOB {{{
test_that("$last_job()", {
    skip_on_cran()
    expect_is(idf <- read_idf(example()), "Idf")
    expect_null(idf$last_job())
    expect_is({idf$run(NULL, tempdir(), echo = FALSE); idf$last_job()}, "EplusJob")
})
# }}}

# GEOMETRY {{{
test_that("$geometry()", {
    expect_warning(empty_idf(8.8)$geometry())
    expect_is(read_idf(example())$geometry(), "IdfGeometry")
})
# }}}

# VIEW {{{
test_that("$view()", {
    skip_on_cran()
    expect_warning(empty_idf(8.8)$view())
    expect_is(read_idf(example())$view(), "IdfViewer")

    expect_is(v <- plot(read_idf(example())), "IdfViewer")
    v$close()
})
# }}}

# CLONE {{{
test_that("$clone()", {
    idf1 <- read_idf(example())
    idf2 <- idf1$clone()
    idf1$set(c(idf1$Zone[[1]]$name()) := list(name = "zone"))
    expect_equal(idf1$Zone[[1]]$Name, "zone")
    expect_equal(idf2$Zone[[1]]$Name, "ZONE ONE")
})
# }}}

# PRINT {{{
test_that("$print()", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")
    expect_output(idf$print())

    # only test on UTF-8 supported platform
    skip_if_not(cli::is_utf8_output())
    idf <- read_idf(example())
    expect_output(idf$print("group"))
    expect_output(idf$print("group", order = FALSE))
    expect_output(idf$print("class"))
    expect_output(idf$print("class", order = FALSE))
    expect_output(idf$print("object"))
    expect_output(idf$print("object", order = FALSE))
    expect_output(idf$print("field"))
    expect_output(idf$print("field", order = FALSE))
})
# }}}

# ADD_OUTPUT {{{
test_that("idf_add_output_*", {
    expect_true(idf_add_output_sqlite(example()))

    expect_is(idf <- read_idf(example()), "Idf")
    expect_true(idf_add_output_sqlite(idf))
    expect_is(idf$set(`Output:SQLite` := list("Simple")), "list")
    expect_true(idf_add_output_sqlite(idf))

    idf1 <- idf$clone()
    idf$set(c(idf$Zone[[1]]$name()) := list(name = "zone"))
    expect_equal(idf$Zone[[1]]$Name, "zone")
    expect_equal(idf1$Zone[[1]]$Name, "ZONE ONE")

    expect_false(idf_add_output_vardict(example()))

    expect_is(idf <- read_idf(example()), "Idf")
    expect_silent(without_checking(idf$Output_VariableDictionary[[1L]]$Key_Field <- "wrong"))
    expect_true(idf_add_output_vardict(idf))
    expect_null(idf$Output_VariableDictionary <- NULL)
    expect_true(idf_add_output_vardict(idf))
})
# }}}

# ACTIVE BINDING {{{
test_that("add_idd_class_bindings", {
    expect_is(with_option(list(autocomplete = FALSE), idf <- read_idf(example())), "Idf")
    expect_false("Version" %in% ls(idf))

    expect_is(with_option(list(autocomplete = TRUE), idf <- read_idf(example())), "Idf")
    expect_true(all(idf$class_name() %in% ls(idf)))

    expect_null(without_checking(with_option(list(autocomplete = TRUE), idf$Timestep <- NULL)))
    expect_output(with_option(list(autocomplete = TRUE), print(idf)))
    expect_false("Timestep" %in% ls(idf))
})
# }}}

# EMPTY IDF {{{
test_that("empty_idf()", {
    expect_is(idf <- empty_idf(8.8), "Idf")
})
# }}}

# S3 FORMATTING {{{
test_that("format.Idf, as.character.Idf and etc", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    expect_output(print(idf))

    expect_output(str(idf))

    expect_equal(format(idf, comment = FALSE, header = FALSE),
        paste0(idf$to_string(comment = FALSE, header = FALSE), collapse = "\n")
    )

    expect_equal(as.character(idf, comment = FALSE, header = FALSE),
        idf$to_string(comment = FALSE, header = FALSE)
    )
})
# }}}

# S3 EQUALITY {{{
test_that("==.Idf and !=.Idf", {
    expect_is(idf_1 <- read_idf(text("idf", 8.8)), "Idf")
    expect_is(idf_2 <- read_idf(text("idf", 8.8)), "Idf")

    # can check equality
    expect_false(idf_1 == TRUE)
    expect_true(idf_1 == idf_1)
    expect_false(idf_1 == idf_2)
    expect_false(idf_1 != idf_1)
    expect_true(idf_1 != idf_2)
})
# }}}

# S3 SUBSET {{{
test_that("[[.Idf and $.Idf", {
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")

    expect_is(idf$Version, "IdfObject")

    expect_equal(names(idf[["Material"]]), c("WD01", "WD02"))
    expect_equal(names(idf$Material), c("WD01", "WD02"))

    expect_null(idf$Wrong)
    expect_null(idf[["Wrong"]])
    expect_error(idf[[1:2]])
    expect_null(idf$Timestep)
    expect_null(idf[["Timestep"]])
})
# }}}

# S3 ASSIGN {{{
test_that("[[<-.Idf and $<-.Idf", {
    skip_on_cran()
    expect_is(idf <- read_idf(text("idf", 8.8)), "Idf")
    expect_error(idf$version() <- NULL)
    expect_error(idf$VERSION <- NULL)
    expect_error(idf[[1:2]] <- NULL)
    expect_error(idf[["VERSION"]] <- NULL)

    expect_is(idf_1 <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/5Zone_Transformer.idf")), "Idf")
    expect_is(idf_2 <- read_idf(idf_1$path()), "Idf")
    expect_silent(without_checking(idf_1$BuildingSurface_Detailed <- idf_2$BuildingSurface_Detailed))
    expect_silent(without_checking(idf_1[["BuildingSurface:Detailed"]] <- idf_2[["BuildingSurface:Detailed"]]))

    expect_null(without_checking(idf_1$BuildingSurface_Detailed <- NULL))
    expect_silent(without_checking(idf_1$BuildingSurface_Detailed <- idf_2$BuildingSurface_Detailed))
    expect_silent(without_checking(idf_1[["BuildingSurface:Detailed"]] <- NULL))
    expect_silent(without_checking(idf_1[["BuildingSurface:Detailed"]] <- idf_2[["BuildingSurface:Detailed"]]))

    expect_is(with_option(list(autocomplete = TRUE), idf <- read_idf(example())), "Idf")

    expect_error(idf$SimulationControl <- idf$Timestep)
    expect_error(idf$SimulationControl <- "Timestep, 6;\n")
    expect_error(idf$SimulationControl <- FALSE)

    # UNIQUE-OBJECT CLASS {{{
    expect_true("SimulationControl" %in% names(idf))

    expect_silent(idf$Material_NoMass$R13LAYER$Thermal_Absorptance <- 0.5)
    expect_equal(idf$Material_NoMass$R13LAYER$Thermal_Absorptance, 0.5)
    expect_silent(idf$Material_NoMass$R13LAYER[["Thermal Absorptance"]] <- 0.6)
    expect_equal(idf$Material_NoMass$R13LAYER[["Thermal Absorptance"]], 0.6)
    expect_silent(idf$SimulationControl$Do_Zone_Sizing_Calculation <- "Yes")
    expect_equal(idf$SimulationControl$Do_Zone_Sizing_Calculation, "Yes")
    expect_silent(idf$SimulationControl[["Do Zone Sizing Calculation"]] <- "No")
    expect_equal(idf$SimulationControl[["Do Zone Sizing Calculation"]], "No")

    # get data.frame input
    tbl <- idf$SimulationControl$to_table()
    # get string input
    str <- idf$SimulationControl$to_string()
    tbl[5, value := "No"]

    # can replace unique-object class
    expect_is(idf$SimulationControl <- idf$SimulationControl, "IdfObject")
    expect_is(idf$SimulationControl <- tbl, "data.table")
    expect_equal(idf$SimulationControl$to_table()$value[[5]], "No")
    expect_is(idf$SimulationControl <- str, "character")
    expect_equal(idf$SimulationControl$to_table()$value[[5]], "Yes")

    # can remove unique-object class
    expect_error(idf$SimulationControl <- NULL)
    expect_null(without_checking(idf$SimulationControl <- NULL))
    expect_false(idf$is_valid_class("SimulationControl"))
    expect_null(idf$SimulationControl)
    expect_false({capture.output(with_option(list(autocomplete = TRUE), print(idf))); "SimulationControl" %in% names(idf)})

    # can insert unique-object class
    expect_silent(with_option(list(autocomplete = TRUE), idf$SimulationControl <- tbl))
    expect_true(idf$is_valid_class("SimulationControl"))
    expect_silent(with_option(list(autocomplete = TRUE), idf$SimulationControl <- str))
    expect_true("SimulationControl" %in% names(idf))
    # }}}

    # NORMAL CLASS {{{
    expect_true("Material" %in% names(idf))

    # get data.frame input
    tbl <- idf$to_table(class = "Material")
    tbl[3, value := "0.2"]
    # get string input
    str <- idf$to_string(class = "Material", header = FALSE)

    # can replace class
    expect_silent(idf$Material <- tbl)
    expect_equal(idf$to_table(class = "Material")$value[[3]], "0.2")
    expect_silent(idf$Material <- str)
    expect_equal(idf$to_table(class = "Material")$value[[3]], "0.1014984")

    # can remove class
    expect_error(idf$Material <- NULL, class = "eplusr_error_del_referenced")
    eplusr_option(validate_level = {chk <- level_checks("final"); chk$reference <- FALSE; chk})
    expect_silent(idf$Material <- NULL)
    expect_false(idf$is_valid_class("Material"))
    expect_null(idf$Material)
    # TODO: dynamically modify active bindings
    # expect_false("Material" %in% names(idf))

    # can insert class
    expect_silent(idf$Material <- tbl)
    expect_true(idf$is_valid_class("Material"))
    expect_silent(idf$Material <- NULL)
    expect_silent(idf$Material <- str)
    # TODO: dynamically modify active bindings
    # expect_true("Material" %in% names(idf))
    eplusr_option(validate_level = "final")

    # can directly insert objects from other idf
    idf_1 <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/5Zone_Transformer.idf"))
    expect_silent(without_checking(idf$BuildingSurface_Detailed <- idf_1$BuildingSurface_Detailed))
    idf_1 <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/5Zone_Transformer.idf"))
    idf_2 <- read_idf(idf_1$path())
    expect_silent(without_checking(idf_1$BuildingSurface_Detailed <- idf_2$BuildingSurface_Detailed))
    # }}}
})
# }}}
