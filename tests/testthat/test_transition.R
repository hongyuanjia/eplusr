context("Transition")

test_that("Transition Helper", {
    eplusr_option(verbose_info = FALSE)
    idf <- read_idf(example(), use_idd(8.8, "auto"))

    # transition action {{{
    expect_equivalent(
        trans_action(idf, "Construction",
            offset = list(2L, 4L),
            add = list(2L:3L, "No")
        ),
        data.table(
            id = rep(15:17, each = 4L),
            name = rep(c("R13WALL", "FLOOR", "ROOF31"), each = 4L),
            class = rep("Construction", 12L),
            index = rep(c(1:4), 3L),
            field = rep(c("Name", NA_character_, NA_character_, "Outside Layer"), 3L),
            value = c(
                "R13WALL", "No", "No", "R13LAYER",
                "FLOOR", "No", "No", "C5 - 4 IN HW CONCRETE",
                "ROOF31", "No", "No", "R31LAYER"
            )
        )
    )

    # can insert new extensible fields
    expect_equivalent(
        trans_action(idf, "Construction",
            insert = list(2:3, NA, step = 1)
        ),
        data.table(
            id = rep(15:17, each = 6L),
            name = rep(c("R13WALL", "FLOOR", "ROOF31"), each = 6L),
            class = rep("Construction", 18L),
            index = rep(c(1:6), 3L),
            field = rep(c("Name", NA_character_, NA_character_, "Outside Layer", NA_character_, NA_character_), 3L),
            value = c(
                "R13WALL", NA_character_, NA_character_, "R13LAYER", NA_character_, NA_character_,
                "FLOOR", NA_character_, NA_character_, "C5 - 4 IN HW CONCRETE", NA_character_, NA_character_,
                "ROOF31", NA_character_, NA_character_, "R31LAYER", NA_character_, NA_character_
            )
        )
    )

    # can insert multiple times
    expect_equivalent(
        trans_action(idf, "RunPeriod",
            insert = list(c(`Start Year` = 4)),
            insert = list(c(`End Year` = 7))
        ),
        data.table(
            id = rep(8L, 13),
            name = rep(NA_character_, 13),
            class = rep("RunPeriod", 13),
            index = 1:13,
            field = c("Name", "Begin Month", "Begin Day of Month", "Start Year",
                      "End Month", "End Day of Month", "End Year",
                      "Day of Week for Start Day", "Use Weather File Holidays and Special Days",
                      "Use Weather File Daylight Saving Period", "Apply Weekend Holiday Rule",
                      "Use Weather File Rain Indicators", "Use Weather File Snow Indicators"
            ),
            value = c(NA_character_, "1", "1", NA_character_, "12", "31", NA_character_,
                "Tuesday", "Yes", "Yes", "No", "Yes", "Yes"
            )
        )
    )
    # }}}

    # preprocess {{{
    expect_silent(new_idf <- trans_preprocess(idf, 8.9, "Construction"))
    expect_equivalent(new_idf$version(), numeric_version("8.9.0"))
    expect_false(new_idf$is_valid_class("Construction"))
    expect_false(._get_private(new_idf)$m_log$uuid == ._get_private(idf)$m_log$uuid)
    # }}}

    # versions {{{
    expect_equivalent(trans_upper_versions(idf, 9.1, patch = TRUE),
        numeric_version(c("8.8.0", "8.9.0", "9.0.0", "9.0.1", "9.1.0"))
    )
    expect_equivalent(trans_upper_versions(idf, 9.1),
        numeric_version(c("8.8.0", "8.9.0", "9.0.0", "9.1.0"))
    )
    # }}}

    # transition functions {{{
    expect_equivalent(
        trans_fun_names(c("8.8.0", "8.9.0", "9.0.1", "9.1.0")),
        c("f880t890", "f890t900", "f900t910")
    )
    # }}}
})

test_that("Transition", {
    skip_on_cran()
    # EnergyPlus v9.1 failed to install on Linux
    skip_on_travis()
    if (!is_avail_eplus(9.1)) install_eplus(9.1)
    eplusr_option(verbose_info = FALSE)

    # suppress build tag missing warnings
    suppressWarnings(use_idd(7.2))
    suppressWarnings(use_idd(8.0))
    suppressWarnings(use_idd(8.1))

    # basic workflow {{{
    idf <- temp_idf(7.2)
    expect_error(transition(idf, 7.3), class = "error_not_idd_ver")
    expect_silent(res <- transition(idf, 8.0))
    expect_silent(res <- transition(idf, 8.0, keep_all = TRUE))
    expect_equal(names(res), c("7.2", "8.0"))
    expect_silent(res <- transition(idf, 8.0, save = TRUE, dir = file.path(tempdir(), "eplusr"), keep_all = TRUE))
    expect_equal(sapply(res, function (idf) basename(idf$path())),
        c("7.2" = paste0(prefix(idf), "V720.idf"),
          "8.0" = paste0(prefix(idf), "V800.idf"))
    )
    expect_true(all(file.exists(file.path(tempdir(), "eplusr", paste0(tools::file_path_sans_ext(basename(idf$path())), c("V720.idf", "V800.idf"))))))
    expect_silent(res_eplus <- version_updater(idf, 8.0, dir = file.path(tempdir(), "ep"), keep_all = TRUE))
    expect_identical(lapply(res, content), lapply(res_eplus, content))

    # can handle newly added extensible fields
    expect_silent(idf <- temp_idf(7.2, "Refrigeration:CompressorList" = list("a", "b", "c", "d", "e", "f")))
    expect_silent(transition(idf, 8.0))

    # can handle newly added extensible fields that are added during transition
    expect_silent(idf <- temp_idf(7.2))
    expect_silent(without_checking(idf$add(Branch = as.list(seq.int(60)), .all = TRUE)))
    expect_silent(idf$save(overwrite = TRUE))
    expect_length(without_checking(transition(idf, 8.0))$Branch$`1`$value(), 63)

    # can preserve object comment
    expect_silent(idf <- temp_idf(8.5,
        "ZoneHVAC:EquipmentList" = list("a", .comment = "comment"),
        "Daylighting:Controls" = list(.comment = "comment2"),
        "Output:Variable" = list(.comment = "comment1")
    ))
    expect_silent(idf <- transition(idf, 8.6))
    expect_equal(lapply(idf$object_id(), function (id) idf$object(id)$comment()),
        list(
            Version = NULL,
            `Daylighting:Controls` = "comment2",
            `Daylighting:ReferencePoint` = "comment2",
            `ZoneHVAC:EquipmentList` = "comment",
            `Output:Variable` = "comment1"
        )
    )

    # can remove empty lines
    expect_silent(
        idf <- transition(temp_idf(7.2,
            Branch = list("a", 0, "a", "a", "a", "a", "a", "a", "a"),
            Branch = list("b", 0, "b", "b")
        ), 8.0)
    )
    expect_equal(nrow(idf$object("a")$to_table()), 13L)
    expect_equal(nrow(idf$object("b")$to_table()), 8L)
    # }}}
    # v7.2 --> v8.0 {{{
    # can handle forkeq variables
    expect_warning(idf <- transition(ver = 8,
        idf = temp_idf(7.2,
            Chiller_Electric_EIR = list("chiller"),
            ChillerHeater_Absorption_DirectFired = list("heater"),
            Output_Variable = list("*", "Chiller Diesel Consumption")
        )
    ), class = "warning_trans_720_800")
    expect_equal(
        idf$to_table(class = "Output:Variable")[index == 2L, value],
        c("Chiller Diesel Energy", "Chiller Heater Diesel Energy")
    )

    # can handle confd nodal temperature from v7.2 to v8.0
    idf <- transition(temp_idf(7.2, Output_Variable = list("*", "condfd nodal temperature")), 8)
    expect_equal(
        idf$to_table(class = "Output:Variable")[index == 2L, value],
        paste("CondFD Surface Temperature Node", 1:10)
    )

    expect_identical_transition(7.2, 8.0,
        .exclude = list(class = c(
            # IDFVersionUpdater reports one less vertex which is incorrect
            "BuildingSurface:Detailed",
            # IDFVersionUpdater reports different order if include this
            "HeatExchanger:WatersideEconomizer"
        )),
        .report_vars = FALSE
    )

    expect_identical_transition(7.2, 8.0,
        "BuildingSurface:Detailed" = list(
            "surf", "wall", "const", "zone",
            "outdoors", NULL, "sunexposed", "windexposed", 0.5,
            3, 1, 1, 1, 2, 2, 2, 3, 3, 3
        ),
        "HeatExchanger:WatersideEconomizer" = list(),
        .report_vars = FALSE
    )
    # }}}
    # v8.0 --> v8.1 {{{
    expect_identical_transition(8.0, 8.1,
        # IDFVersionUpdater will create one `Any Number` schedule type for every
        # HVACTemplate:* instead of only creating only once
        .exclude = list(post_class = "ScheduleTypeLimits"),
        # IDFVersionUpdater only reports fields that meet the \min-fields
        # requirement for v8.0 but not v8.1
        .less_length = c(
            "HVACTemplate:Zone:PTAC",
            "HVACTemplate:Zone:PTHP",
            "HVACTemplate:Zone:WaterToAirHeatPump",
            "HVACTemplate:System:Unitary",
            "HVACTemplate:System:UnitaryHeatPump:AirToAir"
        )
    )

    # can add Any Number ScheduleTypeLimits
    trans <- get_both_trans(8.0, 8.1, "HVACTemplate:System:UnitaryHeatPump:AirToAir" = list())
    expect_equal(trans$eplusr$class_name(), trans$energyplus$class_name())
    expect_equal(
        trans$eplusr$to_string(class = c("ScheduleTypeLimits", "Schedule:Constant")),
        trans$energyplus$to_string(class = c("ScheduleTypeLimits", "Schedule:Constant"))
    )
    # }}}
    # v8.1 --> v8.2 {{{
    # IDFVersionUpdater fails to update variable names
    expect_identical_transition(8.1, 8.2, .report_vars = FALSE)
    # }}}
    # v8.2 --> v8.3 {{{
    # IDFVersionUpdater truncates number at 5 digits but R round at 5 digits
    expect_identical_transition(8.2, 8.3)

    # can hanle min-fields requirement update
    expect_silent(idf <- transition(temp_idf(8.2, `Sizing:System` = list()), 8.3))
    expect_length(idf$Sizing_System[[1L]]$value(), use_idd(8.3)$Sizing_System$min_fields())
    # }}}
    # v8.3 --> v8.4 {{{
    expect_identical_transition(8.3, 8.4,
        .exclude = list(
            class = c(
                # will change the object order if include this
                "PipingSystem:Underground:Domain",
                # IDFVersionUpdater fails to update variable names for class
                # "Meter:CustomDecrement"
                "Meter:CustomDecrement"
            )
        ),
        .ignore_case = list(
            # IDFVersionUpdater changes this field into upper case unnecessarily
            "WaterHeater:HeatPump:PumpedCondenser" = 21L
        ),
        .less_length = list(
            # IDFVersionUpdater did not output all fields
            "WaterHeater:HeatPump:PumpedCondenser" = 36L
        ),
        .report_vars = FALSE
    )
    # }}}
    # v8.4 --> v8.5 {{{
    # IDFVersionUpdater fails to update variable names
    expect_identical_transition(8.4, 8.5, .report_vars = FALSE)
    # }}}
    # v8.5 --> v8.6 {{{
    # IDFVersionUpdater fails to update variable names
    expect_identical_transition(8.5, 8.6,
        .exclude = list(
            # IDFVersionUpdater fails to delete variable names
            class = c("Output:Table:Monthly", "Meter:Custom", "Meter:CustomDecrement")
        ),
        .ignore_field = list(
            # IDFVersionUpdater alway adds defaults for `Fraction of Zone
            # Controlled by Reference Point 1` and `Illuminance Setpoint at
            # Reference Point
            # 1 {lux}`, even there is no matched reference point
            "Daylighting:Controls" = c(15L, 16L)
        ),
        .report_vars = FALSE, .tolerance = 1e-5
    )
    expect_silent(idf <- transition(temp_idf(8.5, Branch = list("branch", 0, "curve", "type", "name", "in", "out")), 8.6))
    expect_equivalent(idf$Branch$branch$value(), list("branch", "curve", "type", "name", "in", "out"))

    # can replace all Coil:Heating:Gas to Coil:Heating:Fuel
    expect_silent(idf <- transition(temp_idf(8.5, "Branch" = list("branch", 0, "curve", "Coil:Heating:Gas", "gas")), 8.6))
    expect_equal(idf$Branch$branch$value(3, simplify = TRUE), "Coil:Heating:Fuel")

    # can handle 2 ref points
    expect_silent(idf <- transition(temp_idf(8.5, "Daylighting:Controls" = list("zone", 2)), 8.6))
    expect_equal(idf$object_num("Daylighting:ReferencePoint"), 2L)
    # }}}
    # v8.6 --> 8.7{{{
    expect_identical_transition(8.6, 8.7)
    # }}}
    # v8.7 --> 8.8 {{{
    expect_identical_transition(8.7, 8.8)
    # }}}
    # v8.7 --> v8.8 {{{
    expect_identical_transition(8.7, 8.8)
    # }}}
    # v8.8 --> v8.9 {{{
    expect_identical_transition(8.8, 8.9,
        # IDFVersionUpdater fails to rename variable names
        .report_vars = FALSE
    )
    # }}}
    # v8.9 --> v9.0 {{{
    # include both `RunPeriod` and `RunPeriod:CustomRange` will change the
    # output object order
    expect_identical_transition(8.9, 9.0, .exclude = list(class = "RunPeriod:CustomRange"))
    expect_identical_transition(8.9, 9.0, "RunPeriod" = list(), .report_vars = FALSE)
    # can remove shading control that is not used by any zone
    expect_identical_transition(8.9, 9.0, "WindowProperty:ShadingControl" = list(), .report_vars = FALSE)
    # }}}
    # v9.0 --> v9.1 {{{
    expect_identical_transition(9.0, 9.1)
    # }}}
})
