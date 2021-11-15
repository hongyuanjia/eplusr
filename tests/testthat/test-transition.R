# HELPER {{{
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
            field = c(
                "Name", "Begin Month", "Begin Day of Month", "Start Year",
                "End Month", "End Day of Month", "End Year",
                "Day of Week for Start Day", "Use Weather File Holidays and Special Days",
                "Use Weather File Daylight Saving Period", "Apply Weekend Holiday Rule",
                "Use Weather File Rain Indicators", "Use Weather File Snow Indicators"
            ),
            value = c(
                NA_character_, "1", "1", NA_character_, "12", "31", NA_character_,
                "Tuesday", "Yes", "Yes", "No", "Yes", "Yes"
            )
        )
    )
    # }}}

    # preprocess {{{
    expect_silent(new_idf <- trans_preprocess(idf, 8.9, "Construction"))
    expect_equivalent(new_idf$version(), numeric_version("8.9.0"))
    expect_false(new_idf$is_valid_class("Construction"))
    expect_false(get_priv_env(new_idf)$uuid() == get_priv_env(idf)$uuid())
    # }}}

    # versions {{{
    expect_equivalent(
        trans_upper_versions(idf, 9.1, patch = TRUE),
        numeric_version(c("8.8.0", "8.9.0", "9.0.0", "9.0.1", "9.1.0"))
    )
    expect_equivalent(
        trans_upper_versions(idf, 9.1),
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
# }}}

# v7.2 --> v8.0 {{{
test_that("Transition v7.2 --> v8.0", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 7.2
    to <- 8.0
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(7.2,
            ShadowCalculation = list(),
            "Coil:Heating:DX:MultiSpeed" = list("Coil"),
            "EnergyManagementSystem:OutputVariable" = list("Variable [m]"),
            "EnergyManagementSystem:MeteredOutputVariable" = list("Variable [m]"),
            "Branch" = list("Branch", ..4 = "HeatExchanger:WatersideEconomizer"),
            "PlantEquipmentList" = list("PlantEquip", ..2 = "HeatExchanger:WatersideEconomizer"),
            "CondenserEquipmentList" = list("CondenserEquip", ..2 = "HeatExchanger:WatersideEconomizer"),
            "HeatExchanger:WatersideEconomizer" = list("HE1", ..2 = "PlateFrame"),
            "HeatExchanger:Hydronic" = list("HE2", ..8 = "UFactorTimesAreaEffectiveness"),
            "HeatExchanger:Plate" = list("HE3", ..8 = "UFactorTimesAreaEffectiveness"),
            "BuildingSurface:Detailed" = list("Surf"),
            "Wall:Detailed" = list("Wall"),
            "RoofCeiling:Detailed" = list("Roof"),
            "Floor:Detailed" = list("Floor"),
            "FenestrationSurface:Detailed" = list("Fene"),
            "Shading:Site:Detailed" = list("SiteShade"),
            "Shading:Building:Detailed" = list("BldgShade"),
            "Shading:Zone:Detailed" = list("ZoneShade"),
            "AirflowNetwork:Distribution:Component:ConstantVolumeFan" = list("Fan"),
            "ZoneHVAC:HighTemperatureRadiant" := list(c("Rad1", "Rad2"), ..5 = c("Electric", "Gas")),
            "AirConditioner:VariableRefrigerantFlow" = list("VRF", ..48 = 0, ..67 = "Electric"),
            "ZoneHVAC:WaterToAirHeatPump" = list("HP"),
            "AirLoopHVAC:UnitaryHeatPump:WaterToAir" = list("UHP"),
            "Boiler:HotWater" = list("Boiler", ..15 = "VariableFlow"),
            "Chiller:Electric" = list("Chiller1", ..27 = "VariableFlow"),
            "Chiller:ConstantCOP" = list("Chiller2", ..11 = "VariableFlow"),
            "Chiller:EngineDriven" = list("Chiller3", ..41 = "VariableFlow"),
            "Chiller:CombustionTurbine" = list("Chiller4", ..54 = "VariableFlow"),
            "Chiller:Electric:EIR" = list("Chiller5", ..23 = "VariableFlow"),
            "Chiller:Absorption" = list("Chiller6", ..23 = "VariableFlow"),
            "Chiller:Absorption:Indirect" = list("Chiller7", ..16 = "VariableFlow"),
            .all = TRUE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_warning(idfTR <- transition(idfOri, to))

    expect_equal(
        idfVU$"Coil:Heating:DX:MultiSpeed"$Coil$value()[1:40],
        idfTR$"Coil:Heating:DX:MultiSpeed"$Coil$value()
    )

    expect_equal(
        idfVU$"EnergyManagementSystem:OutputVariable"$Variable$value(),
        idfTR$"EnergyManagementSystem:OutputVariable"$Variable$value()
    )

    expect_equal(
        idfVU$"EnergyManagementSystem:MeteredOutputVariable"$Variable$value(),
        idfTR$"EnergyManagementSystem:MeteredOutputVariable"$Variable$value()
    )

    expect_equal(
        idfVU$"Branch"$Branch$value()[1:8],
        idfTR$"Branch"$Branch$value()
    )

    expect_equal(
        idfVU$"PlantEquipmentList"$PlantEquip$value()[1:2],
        idfTR$"PlantEquipmentList"$PlantEquip$value()
    )

    expect_equal(
        idfVU$"CondenserEquipmentList"$CondenserEquip$value()[1:2],
        idfTR$"CondenserEquipmentList"$CondenserEquip$value()
    )

    expect_equal(
        idfVU$"HeatExchanger:FluidToFluid"$HE1$value(),
        idfTR$"HeatExchanger:FluidToFluid"$HE1$value()
    )

    expect_equal(
        idfVU$"HeatExchanger:FluidToFluid"$HE2$value(),
        idfTR$"HeatExchanger:FluidToFluid"$HE2$value()
    )
    expect_equal(
        idfVU$"SetpointManager:Scheduled"$HE2$value()[-1],
        idfTR$"SetpointManager:Scheduled"$"HE2 Setpoint Manager"$value()[-1]
    )

    expect_equal(
        idfVU$"HeatExchanger:FluidToFluid"$HE3$value()[1:14],
        idfTR$"HeatExchanger:FluidToFluid"$HE3$value()
    )

    expect_equal(
        # VersionUpdater failed to change '11: Vertex 1 X-coordinate' to '0.0'
        idfVU$"BuildingSurface:Detailed"$Surf$value()[-11],
        idfTR$"BuildingSurface:Detailed"$Surf$value()[-11]
    )

    expect_equal(
        idfVU$"Wall:Detailed"$Wall$value(),
        idfTR$"Wall:Detailed"$Wall$value()
    )

    expect_equal(
        idfVU$"RoofCeiling:Detailed"$Roof$value(),
        idfTR$"RoofCeiling:Detailed"$Roof$value()
    )

    expect_equal(
        idfVU$"Floor:Detailed"$Floor$value(),
        idfTR$"Floor:Detailed"$Floor$value()
    )

    expect_equal(
        idfVU$"FenestrationSurface:Detailed"$Fene$value(),
        idfTR$"FenestrationSurface:Detailed"$Fene$value()
    )

    expect_equal(
        idfVU$"Shading:Site:Detailed"$SiteShade$value(),
        idfTR$"Shading:Site:Detailed"$SiteShade$value()
    )

    expect_equal(
        idfVU$"Shading:Building:Detailed"$BldgShade$value(),
        idfTR$"Shading:Building:Detailed"$BldgShade$value()
    )

    expect_equal(
        idfVU$"Shading:Zone:Detailed"$ZoneShade$value(),
        idfTR$"Shading:Zone:Detailed"$ZoneShade$value()
    )

    expect_equal(
        idfVU$"AirflowNetwork:Distribution:Component:Fan"[[1]]$value(),
        idfTR$"AirflowNetwork:Distribution:Component:Fan"[[1]]$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:HighTemperatureRadiant"$Rad1$value()[1:11],
        idfTR$"ZoneHVAC:HighTemperatureRadiant"$Rad1$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:HighTemperatureRadiant"$Rad2$value()[1:11],
        idfTR$"ZoneHVAC:HighTemperatureRadiant"$Rad2$value()
    )

    expect_equal(
        idfVU$"AirConditioner:VariableRefrigerantFlow"[[1]]$value(),
        idfTR$"AirConditioner:VariableRefrigerantFlow"[[1]]$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:WaterToAirHeatPump"$HP$value()[1:28],
        idfTR$"ZoneHVAC:WaterToAirHeatPump"$HP$value()
    )

    expect_equal(
        idfVU$"AirLoopHVAC:UnitaryHeatPump:WaterToAir"$UHP$value(),
        idfTR$"AirLoopHVAC:UnitaryHeatPump:WaterToAir"$UHP$value()
    )

    expect_equal(
        idfVU$"Boiler:HotWater"$Boiler$value(),
        idfTR$"Boiler:HotWater"$Boiler$value()
    )

    expect_equal(
        idfVU$"Chiller:Electric"$Chiller1$value()[1:33],
        idfTR$"Chiller:Electric"$Chiller1$value()
    )

    expect_equal(
        idfVU$"Chiller:ConstantCOP"$Chiller2$value()[1:14],
        idfTR$"Chiller:ConstantCOP"$Chiller2$value()
    )

    expect_equal(
        idfVU$"Chiller:EngineDriven"$Chiller3$value()[1:45],
        idfTR$"Chiller:EngineDriven"$Chiller3$value()
    )

    expect_equal(
        idfVU$"Chiller:CombustionTurbine"$Chiller4$value()[1:59],
        idfTR$"Chiller:CombustionTurbine"$Chiller4$value()
    )

    expect_equal(
        idfVU$"Chiller:Electric:EIR"$Chiller5$value()[1:29],
        idfTR$"Chiller:Electric:EIR"$Chiller5$value()
    )

    expect_equal(
        idfVU$"Chiller:Absorption"$Chiller6$value(),
        idfTR$"Chiller:Absorption"$Chiller6$value()
    )

    expect_equal(
        idfVU$"Chiller:Absorption:Indirect"$Chiller7$value(),
        idfTR$"Chiller:Absorption:Indirect"$Chiller7$value()
    )

    # can handle forkeq variables
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(
            7.2,
            "Chiller:Electric:EIR" := list(paste0("Chiller", 1:2)),
            "ChillerHeater:Absorption:DirectFired" := list(paste0("Heater", 1:2)),
            "Output:Variable" := list(
                "*",
                c(
                    "Chiller Diesel Consumption",
                    "CondFD Nodal Temperature"
                )
            )
        )
    )

    # can handle forkeq variables
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(
            7.2,
            "Chiller:Electric:EIR" := list(paste0("Chiller", 1:2)),
            "ChillerHeater:Absorption:DirectFired" := list(paste0("Heater", 1:2)),
            "Output:Variable" := list("*", c(
                "Chiller Diesel Consumption",
                "Chiller Diesel Consumption Rate",
                "CondFD Nodal Temperature",
                "Inside Surface Relative Humidity"
            ))
        )
    )

    # VersionUpdater does not create "Diesel" output variables for both Chiller
    # and ChillerHeater
    expect_warning(idfTR <- transition(idfOri, 8), "Default values")
    expect_equal(
        idfTR$to_table(class = "Output:Variable")[index == 2, sort(value)],
        c(
            "Chiller Diesel Energy",
            "Chiller Diesel Rate",
            "Chiller Heater Diesel Energy",
            "Chiller Heater Diesel Rate",
            "CondFD Surface Temperature Node 1",
            "CondFD Surface Temperature Node 10",
            "CondFD Surface Temperature Node 2",
            "CondFD Surface Temperature Node 3",
            "CondFD Surface Temperature Node 4",
            "CondFD Surface Temperature Node 5",
            "CondFD Surface Temperature Node 6",
            "CondFD Surface Temperature Node 7",
            "CondFD Surface Temperature Node 8",
            "CondFD Surface Temperature Node 9",
            "EMPD Surface Inside Face Relative Humidity",
            "HAMT Surface Inside Face Relative Humidity"
        )
    )
})
# }}}
# v8.0 --> v8.1 {{{
test_that("Transition v8.0 --> v8.1", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 8.0
    to <- 8.1
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "People" = list("People"),
            "CoolingTower:SingleSpeed" = list("CT1", ..8 = "autosize", ..9 = "autosize"),
            "CoolingTower:TwoSpeed" = list("CT2", ..8 = "autosize", ..9 = "autosize", ..11 = "autosize", ..12 = "autosize"),
            "EvaporativeFluidCooler:SingleSpeed" = list("EFC1"),
            "EvaporativeFluidCooler:TwoSpeed" = list("EFC2", ..6 = "autosize", ..7 = "autosize", ..14 = "autosize"),
            "FluidCooler:TwoSpeed" = list("FluidCooler", ..6 = "autosize", ..15 = "autosize", ..16 = "autosize"),
            "HeatPump:WaterToWater:EquationFit:Heating" = list("HP1"),
            "HeatPump:WaterToWater:EquationFit:Cooling" = list("HP2"),
            "HeatPump:WaterToWater:ParameterEstimation:Heating" = list("HP3"),
            "HeatPump:WaterToWater:ParameterEstimation:Cooling" = list("HP4"),
            "HVACTemplate:Zone:PTAC" = list(..13 = "Cycling"),
            "HVACTemplate:Zone:PTAC" = list(..13 = "Continuous"),
            "HVACTemplate:Zone:PTHP" = list(..13 = "Cycling"),
            "HVACTemplate:Zone:PTHP" = list(..13 = "Continuous"),
            "HVACTemplate:Zone:WaterToAirHeatPump" = list(..13 = "Cycling"),
            "HVACTemplate:Zone:WaterToAirHeatPump" = list(..13 = "Continuous"),
            "HVACTemplate:System:Unitary" = list("Sys1", ..5 = "Cycling"),
            "HVACTemplate:System:Unitary" = list("Sys2", ..5 = "Continuous"),
            "HVACTemplate:System:UnitaryHeatPump:AirToAir" = list("Sys3", ..7 = "Cycling"),
            "HVACTemplate:System:UnitaryHeatPump:AirToAir" = list("Sys4", ..7 = "Continuous"),
            .all = TRUE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_is(idfTR <- transition(idfOri, to), "Idf")

    expect_equal(
        idfVU$"People"[[1]]$value()[1:16],
        idfTR$"People"[[1]]$value()
    )

    # VersionUpdater gives "autocalculate" instead of "Autocalculate"
    lower_autocal <- function(l) lapply(l, function(x) if (identical(x, "Autocalculate")) "autocalculate" else x)

    expect_equal(
        idfVU$"CoolingTower:SingleSpeed"$CT1$value(),
        lower_autocal(idfTR$"CoolingTower:SingleSpeed"$CT1$value())
    )

    expect_equal(
        idfVU$"CoolingTower:TwoSpeed"$CT2$value(),
        lower_autocal(idfTR$"CoolingTower:TwoSpeed"$CT2$value())
    )

    expect_equal(
        idfVU$"EvaporativeFluidCooler:SingleSpeed"$EFC1$value()[1:23],
        idfTR$"EvaporativeFluidCooler:SingleSpeed"$EFC1$value()
    )

    expect_equal(
        idfVU$"EvaporativeFluidCooler:TwoSpeed"$EFC2$value()[1:32],
        lower_autocal(idfTR$"EvaporativeFluidCooler:TwoSpeed"$EFC2$value())
    )

    expect_equal(
        idfVU$"FluidCooler:TwoSpeed"$FluidCooler$value()[1:20],
        lower_autocal(idfTR$"FluidCooler:TwoSpeed"$FluidCooler$value())
    )

    expect_equal(
        idfVU$"HeatPump:WaterToWater:EquationFit:Heating"$HP1$value(),
        idfTR$"HeatPump:WaterToWater:EquationFit:Heating"$HP1$value()
    )

    expect_equal(
        idfVU$"HeatPump:WaterToWater:EquationFit:Cooling"$HP2$value(),
        idfTR$"HeatPump:WaterToWater:EquationFit:Cooling"$HP2$value()
    )

    expect_equal(
        idfVU$"HeatPump:WaterToWater:ParameterEstimation:Heating"$HP3$value(),
        idfTR$"HeatPump:WaterToWater:ParameterEstimation:Heating"$HP3$value()
    )

    expect_equal(
        idfVU$"HeatPump:WaterToWater:ParameterEstimation:Cooling"$HP4$value(),
        idfTR$"HeatPump:WaterToWater:ParameterEstimation:Cooling"$HP4$value()
    )

    # NOTE: VersionUpdater adds one "ScheduleTypeLimits" for each HVACTemplate
    expect_equal(idfTR$"ScheduleTypeLimits"$"Any Number"$Name, "Any Number")

    expect_equal(
        idfVU$"HVACTemplate:Zone:PTAC"[[1]]$value(),
        idfTR$"HVACTemplate:Zone:PTAC"[[1]]$value()
    )
    expect_equal(
        idfVU$"HVACTemplate:Zone:PTAC"[[2]]$value(),
        idfTR$"HVACTemplate:Zone:PTAC"[[2]]$value()
    )

    expect_equal(
        idfVU$"HVACTemplate:Zone:PTHP"[[1]]$value(),
        idfTR$"HVACTemplate:Zone:PTHP"[[1]]$value()
    )
    expect_equal(
        idfVU$"HVACTemplate:Zone:PTHP"[[2]]$value(),
        idfTR$"HVACTemplate:Zone:PTHP"[[2]]$value()
    )

    expect_equal(
        idfVU$"HVACTemplate:Zone:WaterToAirHeatPump"[[1]]$value(),
        idfTR$"HVACTemplate:Zone:WaterToAirHeatPump"[[1]]$value()
    )
    expect_equal(
        idfVU$"HVACTemplate:Zone:WaterToAirHeatPump"[[2]]$value(),
        idfTR$"HVACTemplate:Zone:WaterToAirHeatPump"[[2]]$value()
    )

    expect_equal(
        idfVU$"HVACTemplate:System:Unitary"$Sys1$value(),
        idfTR$"HVACTemplate:System:Unitary"$Sys1$value()
    )
    expect_equal(
        idfVU$"HVACTemplate:System:Unitary"$Sys2$value(),
        idfTR$"HVACTemplate:System:Unitary"$Sys2$value()
    )

    expect_equal(
        idfVU$"HVACTemplate:System:UnitaryHeatPump:AirToAir"$Sys3$value(),
        idfTR$"HVACTemplate:System:UnitaryHeatPump:AirToAir"$Sys3$value()
    )
    expect_equal(
        idfVU$"HVACTemplate:System:UnitaryHeatPump:AirToAir"$Sys4$value(),
        idfTR$"HVACTemplate:System:UnitaryHeatPump:AirToAir"$Sys4$value()
    )
})
# }}}
# v8.1 --> v8.2 {{{
test_that("Transition v8.1 --> v8.2", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 8.1
    to <- 8.2
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "ZoneHVAC:UnitVentilator" = list("UV"),
            "ZoneHVAC:UnitHeater" := list(paste0("UH", 1:2), ..8 = c("onoff", "continuous")),
            "PlantLoop" := list(paste0("PL", 1:2), ..19 = c("Sequential", "Uniform")),
            "CondenserLoop" = list("CL", ..19 = "Sequential"),
            "HVACTemplate:Plant:ChilledWaterLoop" := list(paste0("CWL", 1:2), ..32 = c("Sequential", "Uniform"), ..33 = c("Sequential", "Uniform")),
            "HVACTemplate:Plant:HotWaterLoop" := list(paste0("HWL", 1:2), ..21 = c("Sequential", "Uniform")),
            "HVACTemplate:Plant:MixedWaterLoop" := list(paste0("MWL", 1:2), ..17 = c("Sequential", "Uniform")),
            "Sizing:System" = list(),
            "ZoneHVAC:Baseboard:RadiantConvective:Water" = list(),
            "ZoneHVAC:HighTemperatureRadiant" = list(),
            "ZoneHVAC:Baseboard:RadiantConvective:Steam" = list(),
            "ZoneHVAC:Baseboard:RadiantConvective:Electric" = list(),
            "ZoneHVAC:Baseboard:Convective:Water" = list(),
            "ZoneHVAC:Baseboard:Convective:Electric" = list(),
            "ZoneHVAC:LowTemperatureRadiant:VariableFlow" = list(),
            "ZoneHVAC:LowTemperatureRadiant:Electric" = list(),
            .all = TRUE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_is(idfTR <- transition(idfOri, to), "Idf")

    expect_equal(
        idfVU$"ZoneHVAC:UnitVentilator"$UV$value(1:23),
        idfTR$"ZoneHVAC:UnitVentilator"$UV$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:UnitHeater"$UH1$value(1:14),
        idfTR$"ZoneHVAC:UnitHeater"$UH1$value()
    )
    expect_equal(
        idfVU$"ZoneHVAC:UnitHeater"$UH2$value(1:14),
        idfTR$"ZoneHVAC:UnitHeater"$UH2$value()
    )

    expect_equal(
        idfVU$"PlantLoop"$PL1$value(),
        idfTR$"PlantLoop"$PL1$value()
    )

    expect_equal(
        idfVU$"CondenserLoop"$CL$value(),
        idfTR$"CondenserLoop"$CL$value()
    )

    expect_equal(
        idfVU$object("CWL1", "HVACTemplate:Plant:ChilledWaterLoop")$value(),
        idfTR$object("CWL1", "HVACTemplate:Plant:ChilledWaterLoop")$value()
    )
    expect_equal(
        idfVU$object("CWL2", "HVACTemplate:Plant:ChilledWaterLoop")$value(),
        idfTR$object("CWL2", "HVACTemplate:Plant:ChilledWaterLoop")$value()
    )

    expect_equal(
        idfVU$object("HWL1", "HVACTemplate:Plant:HotWaterLoop")$value(),
        idfTR$object("HWL1", "HVACTemplate:Plant:HotWaterLoop")$value()
    )
    expect_equal(
        idfVU$object("HWL2", "HVACTemplate:Plant:HotWaterLoop")$value(),
        idfTR$object("HWL2", "HVACTemplate:Plant:HotWaterLoop")$value()
    )

    expect_equal(
        idfVU$object("MWL1", "HVACTemplate:Plant:MixedWaterLoop")$value(),
        idfTR$object("MWL1", "HVACTemplate:Plant:MixedWaterLoop")$value()
    )
    expect_equal(
        idfVU$object("MWL2", "HVACTemplate:Plant:MixedWaterLoop")$value(),
        idfTR$object("MWL2", "HVACTemplate:Plant:MixedWaterLoop")$value()
    )

    expect_equal(
        idfVU$"Sizing:System"[[1]]$value(),
        idfTR$"Sizing:System"[[1]]$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:Baseboard:RadiantConvective:Water"[[1]]$value(1:14),
        idfTR$"ZoneHVAC:Baseboard:RadiantConvective:Water"[[1]]$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:HighTemperatureRadiant"[[1]]$value(1:14),
        idfTR$"ZoneHVAC:HighTemperatureRadiant"[[1]]$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:Baseboard:RadiantConvective:Steam"[[1]]$value(1:13),
        idfTR$"ZoneHVAC:Baseboard:RadiantConvective:Steam"[[1]]$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:Baseboard:RadiantConvective:Electric"[[1]]$value(1:9),
        idfTR$"ZoneHVAC:Baseboard:RadiantConvective:Electric"[[1]]$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:Baseboard:Convective:Water"[[1]]$value(),
        idfTR$"ZoneHVAC:Baseboard:Convective:Water"[[1]]$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:Baseboard:Convective:Electric"[[1]]$value(),
        idfTR$"ZoneHVAC:Baseboard:Convective:Electric"[[1]]$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:LowTemperatureRadiant:VariableFlow"[[1]]$value(),
        idfTR$"ZoneHVAC:LowTemperatureRadiant:VariableFlow"[[1]]$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:LowTemperatureRadiant:Electric"[[1]]$value(1:10),
        idfTR$"ZoneHVAC:LowTemperatureRadiant:Electric"[[1]]$value()
    )
})
# }}}
# v8.2 --> v8.3 {{{
test_that("Transition v8.2 --> v8.3", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 8.2
    to <- 8.3
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "Chiller:Electric:ReformulatedEIR" = list(),
            "Site:GroundDomain" = list(),
            "GroundHeatExchanger:Vertical" = list(),
            "EvaporativeCooler:Indirect:ResearchSpecial" = list(
                ..3 = 0.75, ..4 = 0.5, ..5 = 30.0, ..6 = "autosize",
                ..7 = 0.8, ..8 = 1000, ..12 = 0.9, ..17 = 0.2, ..18 = 3
            ),
            "EvaporativeCooler:Direct:ResearchSpecial" = list(
                ..3 = 0.7, ..4 = 30.0, ..9 = 0.0, ..10 = 3
            ),
            .all = TRUE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_is(idfTR <- transition(idfOri, to), "Idf")

    expect_equal(
        idfVU$"Chiller:Electric:ReformulatedEIR"[[1]]$value(1:26),
        idfTR$"Chiller:Electric:ReformulatedEIR"[[1]]$value()
    )

    expect_equal(
        idfVU$"Site:GroundDomain:Slab"[[1]]$value(),
        idfTR$"Site:GroundDomain:Slab"[[1]]$value()
    )

    expect_equal(
        idfVU$"EvaporativeCooler:Indirect:ResearchSpecial"[[1]]$value(),
        idfTR$"EvaporativeCooler:Indirect:ResearchSpecial"[[1]]$value()
    )

    expect_equal(
        idfVU$"EvaporativeCooler:Direct:ResearchSpecial"[[1]]$value(),
        idfTR$"EvaporativeCooler:Direct:ResearchSpecial"[[1]]$value()
    )
})
# }}}
# v8.3 --> v8.4 {{{
test_that("Transition v8.3 --> v8.4", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 8.3
    to <- 8.4
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "Coil:WaterHeating:AirToWaterHeatPump" = list(),
            "WaterHeater:Stratified" := list(paste0("Stratified", 1:2)),
            "WaterHeater:HeatPump" := list(paste0("HP", 1:2),
                ..17 = "WaterHeater:Stratified",
                ..18 = paste0("Stratified", 1:2),
                ..21 = "Coil:WaterHeating:AirToWaterHeatPump",
                ..35 = c("HEATER1", "HEATER2")
            ),
            "Branch" = list(..4 = "WaterHeater:HeatPump", ..9 = "WaterHeater:HeatPump"),
            "ZoneHVAC:EquipmentList" = list(..2 = "WaterHeater:HeatPump", ..6 = "WaterHeater:HeatPump"),
            "PlantEquipmentList" = list(..2 = "WaterHeater:HeatPump", ..4 = "WaterHeater:HeatPump"),
            "EvaporativeCooler:Direct:ResearchSpecial" = list(),
            "Controller:MechanicalVentilation" = list(..4 = "ProportionalControl"),
            "Site:GroundDomain:Slab" := list(paste0("Slab", 1:2)),
            "Site:GroundDomain:Basement" := list(paste0("Base", 1:2)),
            "PipingSystem:Underground:Domain" := list(paste0("PipeSys", 1:2)),
            "Pipe:Underground" := list(paste0("Pipe", 1:2)),
            "GroundHeatExchanger:HorizontalTrench" := list(paste0("HEH", 1:2)),
            "GroundHeatExchanger:Slinky" := list(paste0("HES", 1:2)),
            "HVACTemplate:Plant:ChilledWaterLoop" := list(
                ..32 = c("Sequential", "Uniform"),
                ..33 = c("Sequential", "Uniform")
            ),
            "HVACTemplate:Plant:HotWaterLoop" := list(..21 = c("Sequential", "Uniform")),
            "HVACTemplate:Plant:MixedWaterLoop" := list(..17 = c("Sequential", "Uniform")),
            "ZoneAirMassFlowConservation" = list("No"),
            .all = TRUE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_is(idfTR <- transition(idfOri, to), "Idf")

    expect_equal(
        idfVU$"Coil:WaterHeating:AirToWaterHeatPump:Pumped"[[1]]$value(1:21),
        idfTR$"Coil:WaterHeating:AirToWaterHeatPump:Pumped"[[1]]$value()
    )

    expect_equal(
        idfVU$"WaterHeater:Stratified"$Stratified1$value(1:66),
        idfTR$"WaterHeater:Stratified"$Stratified1$value()
    )
    expect_equal(
        idfVU$"WaterHeater:Stratified"$Stratified2$value(1:66),
        idfTR$"WaterHeater:Stratified"$Stratified2$value()
    )

    expect_equal(
        idfVU$"WaterHeater:HeatPump:PumpedCondenser"$HP1$value(1:32),
        {
            val <- idfTR$"WaterHeater:HeatPump:PumpedCondenser"$HP1$value()
            val[[21]] <- toupper(val[[21]])
            val
        }
    )
    expect_equal(
        idfVU$"WaterHeater:HeatPump:PumpedCondenser"$HP2$value(1:32),
        {
            val <- idfTR$"WaterHeater:HeatPump:PumpedCondenser"$HP2$value()
            val[[21]] <- toupper(val[[21]])
            val
        }
    )

    expect_equal(
        idfVU$"Branch"[[1]]$value(1:13),
        idfTR$"Branch"[[1]]$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:EquipmentList"[[1]]$value(1:9),
        idfTR$"ZoneHVAC:EquipmentList"[[1]]$value()
    )

    expect_equal(
        idfVU$"PlantEquipmentList"[[1]]$value(1:4),
        idfTR$"PlantEquipmentList"[[1]]$value()
    )

    expect_equal(
        idfVU$"EvaporativeCooler:Direct:ResearchSpecial"[[1]]$value(1:11),
        idfTR$"EvaporativeCooler:Direct:ResearchSpecial"[[1]]$value()
    )

    expect_equal(
        idfVU$"Controller:MechanicalVentilation"[[1]]$value(1:8),
        idfTR$"Controller:MechanicalVentilation"[[1]]$value()
    )

    expect_equal(
        idfVU$"Site:GroundDomain:Slab"$Slab1$value(1:23),
        idfTR$"Site:GroundDomain:Slab"$Slab1$value()
    )
    expect_equal(
        idfVU$"Site:GroundDomain:Slab"$Slab2$value(1:23),
        idfTR$"Site:GroundDomain:Slab"$Slab2$value()
    )

    expect_equal(
        idfVU$"Site:GroundDomain:Basement"$Base1$value(),
        idfTR$"Site:GroundDomain:Basement"$Base1$value()
    )
    expect_equal(
        idfVU$"Site:GroundDomain:Basement"$Base2$value(),
        idfTR$"Site:GroundDomain:Basement"$Base2$value()
    )

    # VersionUpdater does not follow the actual class order
    expect_equal(
        idfVU$"PipingSystem:Underground:Domain"$PipeSys1$value(1:36)[-20],
        idfTR$"PipingSystem:Underground:Domain"$PipeSys1$value()[-20]
    )
    expect_equal(
        idfVU$"PipingSystem:Underground:Domain"$PipeSys2$value(1:36)[-20],
        idfTR$"PipingSystem:Underground:Domain"$PipeSys2$value()[-20]
    )
    expect_equal(
        idfTR$"PipingSystem:Underground:Domain"$PipeSys1$value("Undisturbed Ground Temperature Model Name")[[1]],
        "KATemp 5"
    )
    expect_equal(
        idfTR$"PipingSystem:Underground:Domain"$PipeSys2$value("Undisturbed Ground Temperature Model Name")[[1]],
        "KATemp 6"
    )

    expect_equal(
        idfVU$"Pipe:Underground"$Pipe1$value()[-10],
        idfTR$"Pipe:Underground"$Pipe1$value()[-10]
    )
    expect_equal(
        idfVU$"Pipe:Underground"$Pipe2$value()[-10],
        idfTR$"Pipe:Underground"$Pipe2$value()[-10]
    )
    expect_equal(idfTR$"Pipe:Underground"$Pipe1$value(10)[[1]], "KATemp 7")
    expect_equal(idfTR$"Pipe:Underground"$Pipe2$value(10)[[1]], "KATemp 8")

    expect_equal(
        idfVU$"GroundHeatExchanger:HorizontalTrench"$HEH1$value()[-20],
        idfTR$"GroundHeatExchanger:HorizontalTrench"$HEH1$value()[-20]
    )
    expect_equal(
        idfVU$"GroundHeatExchanger:HorizontalTrench"$HEH2$value()[-20],
        idfTR$"GroundHeatExchanger:HorizontalTrench"$HEH2$value()[-20]
    )
    expect_equal(idfTR$"GroundHeatExchanger:HorizontalTrench"$HEH1$value(20)[[1]], "KATemp 9")
    expect_equal(idfTR$"GroundHeatExchanger:HorizontalTrench"$HEH2$value(20)[[1]], "KATemp 10")

    expect_equal(
        idfVU$"GroundHeatExchanger:Slinky"$HES1$value()[-21],
        idfTR$"GroundHeatExchanger:Slinky"$HES1$value()[-21]
    )
    expect_equal(
        idfVU$"GroundHeatExchanger:Slinky"$HES2$value()[-21],
        idfTR$"GroundHeatExchanger:Slinky"$HES2$value()[-21]
    )
    expect_equal(idfTR$"GroundHeatExchanger:Slinky"$HES1$value(21)[[1]], "KATemp 11")
    expect_equal(idfTR$"GroundHeatExchanger:Slinky"$HES2$value(21)[[1]], "KATemp 12")

    expect_equal(idfVU$object(11)$value(), idfTR$object(36)$value())
    expect_equal(idfVU$object(12)$value(), idfTR$object(37)$value())

    expect_equal(idfVU$object(13)$value(), idfTR$object(38)$value())
    expect_equal(idfVU$object(14)$value(), idfTR$object(39)$value())

    expect_equal(idfVU$object(15)$value(), idfTR$object(40)$value())
    expect_equal(idfVU$object(16)$value(), idfTR$object(41)$value())

    expect_equal(
        idfVU$"ZoneAirMassFlowConservation"$value(),
        idfTR$"ZoneAirMassFlowConservation"$value()
    )
})
# }}}
# v8.4 --> v8.5 {{{
test_that("Transition v8.4 --> v8.5", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 8.4
    to <- 8.5
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "EnergyManagementSystem:Actuator" := list(
                ..4 = sprintf("outdoor air %sblub temperature", c("dry", "wet"))
            ),
            .all = TRUE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_is(idfTR <- transition(idfOri, to), "Idf")

    expect_equal(
        idfVU$"EnergyManagementSystem:Actuator"[[1]]$value(),
        idfTR$"EnergyManagementSystem:Actuator"[[1]]$value()
    )
    expect_equal(
        idfVU$"EnergyManagementSystem:Actuator"[[2]]$value(),
        idfTR$"EnergyManagementSystem:Actuator"[[2]]$value()
    )
})
# }}}
# v8.5 --> v8.6 {{{
test_that("Transition v8.5 --> v8.6", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 8.5
    to <- 8.6
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "Building" = list(),
            "GlobalGeometryRules" = list("UpperLeftCorner", "Counterclockwise", "Relative", "Relative", "Relative"),
            "Exterior:FuelEquipment" = list("Equip"),
            "HVACTemplate:System:UnitarySystem" = list("Uni"),
            "HVACTemplate:System:Unitary" = list("Sys", "On", "Zone1", ..16 = "Gas"),
            "ChillerHeater:Absorption:DirectFired" = list("Chiller", "Autosize",
                ..8 = "In", ..9 = "Out", ..10 = "CInlet", ..11 = "COut",
                ..12 = "HIn", ..13 = "Hout", ..20 = "Autosize"
            ),
            "SetpointManager:SingleZone:Humidity:Minimum" = list("SP1", ..4 = "Node", ..5 = "Zone1"),
            "SetpointManager:SingleZone:Humidity:Maximum" = list("SP2", ..4 = "Node", ..5 = "Zone1"),
            "AirTerminal:SingleDuct:VAV:Reheat" = list("VAV",
                ..3 = "Damper", ..4 = "Inlet", ..5 = "Autosize", ..6 = "Constant",
                ..10 = "Coil:Heating:Gas", ..11 = "HtgCoil",
                ..14 = "Outlet", ..16 = "Reverse", ..17 = "Autocalculate",
                ..18 = "Autocalculate"
            ),
            "Branch" = list("Branch", "autosize", NULL, "Type", "Name", "In", "Out", "Passive"),
            "AirTerminal:SingleDuct:InletSideMixer" = list("Term1", "ZoneHVAC:FourPipeFanCoil", "FanCoil", "Outlet", "Primary", "Second"),
            "AirTerminal:SingleDuct:SupplySideMixer" = list("Term2", "ZoneHVAC:FourPipeFanCoil", "FanCoil", "Outlet", "Primary", "Second"),
            "OtherEquipment" = list("Equip", "Zone1", "On"),
            "Coil:Heating:Gas" = list("Coil", ..5 = "Inlet", ..6 = "Outlet"),
            "Zone" := list(paste0("Zone", 1:2)),
            "Daylighting:Controls" := list(paste0("Zone", 1:3),
                c(1:2, 2), 1:3, 1:3, 4:6, 4:6, 4:6,
                ..11 = 300, ..12 = 500, ..13 = 1:3, ..14 = 90
            ),
            "Daylighting:DELight:Controls" := list(
                paste0("DELight", 1:3), paste0("Zone", 1:3),
                1:3, 0.3, 0.2, 2, 1.0, 0.5
            ),
            "Daylighting:DELight:ReferencePoint" := list(
                paste0("RefPt", 1:2), paste0("DELight", 1:2), 1:2, 1:2, 1:2, 0.5, 400
            ),
            "MaterialProperty:MoisturePenetrationDepth:Settings" := list(
                "Mat1", 0.004, 0.07, 0.40, 0.07, 10.0
            ),
            "Material" = list("Mat1", "Rough", 0.12, 0.16, 800, 800, 0.9, 0.6, 0.6),
            "EnergyManagementSystem:Actuator" := list(c("Act1", "Act2"), "Name", "Type",
                ..4 = sprintf("outdoor air %sblub temperature", c("dry", "wet"))
            ),
            "Output:Variable" := list(
                paste0("Zone", 1:3), "Daylighting Reference Point 1 Illuminance"
            ),
            "Output:Variable" := list(
                paste0("Zone", 1:3), "Daylighting Lighting Power Multiplier"
            ),
            .all = TRUE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_is(idfTR <- transition(idfOri, to), "Idf")

    use_idd(8.5)$"Daylighting:DELight:Controls"
    use_idd(8.6)$"Daylighting:Controls"

    use_idd(8.5)$"Daylighting:DELight:ReferencePoint"
    use_idd(8.6)$"Daylighting:DELight:ReferencePoint"

    expect_equal(
        idfVU$"Building"$value(),
        idfTR$"Building"$value()
    )
    expect_equal(
        idfVU$"GlobalGeometryRules"$value(),
        idfTR$"GlobalGeometryRules"$value()
    )

    expect_equal(
        idfVU$"Exterior:FuelEquipment"[[1]]$value(),
        idfTR$"Exterior:FuelEquipment"[[1]]$value()
    )

    expect_equal(
        idfVU$"HVACTemplate:System:UnitarySystem"[[1]]$value(),
        idfTR$"HVACTemplate:System:UnitarySystem"[[1]]$value()
    )

    expect_equal(
        idfVU$"HVACTemplate:System:Unitary"[[1]]$value(),
        idfTR$"HVACTemplate:System:Unitary"[[1]]$value()
    )

    expect_equal(
        idfVU$"ChillerHeater:Absorption:DirectFired"[[1]]$value(),
        idfTR$"ChillerHeater:Absorption:DirectFired"[[1]]$value()
    )

    expect_equal(
        idfVU$"SetpointManager:SingleZone:Humidity:Minimum"[[1]]$value(),
        idfTR$"SetpointManager:SingleZone:Humidity:Minimum"[[1]]$value()
    )

    expect_equal(
        idfVU$"SetpointManager:SingleZone:Humidity:Maximum"[[1]]$value(),
        idfTR$"SetpointManager:SingleZone:Humidity:Maximum"[[1]]$value()
    )

    expect_equal(
        idfVU$"AirTerminal:SingleDuct:VAV:Reheat"[[1]]$value(1:18),
        idfTR$"AirTerminal:SingleDuct:VAV:Reheat"[[1]]$value()
    )

    expect_equal(
        idfVU$"Branch"[[1]]$value(1:6),
        idfTR$"Branch"[[1]]$value()
    )

    expect_equal(
        idfVU$"AirTerminal:SingleDuct:Mixer"$Term1$value(),
        idfTR$"AirTerminal:SingleDuct:Mixer"$Term1$value()
    )

    expect_equal(
        idfVU$"AirTerminal:SingleDuct:Mixer"$Term2$value(),
        idfTR$"AirTerminal:SingleDuct:Mixer"$Term2$value()
    )

    expect_equal(
        idfVU$"OtherEquipment"[[1]]$value(),
        idfTR$"OtherEquipment"[[1]]$value()
    )

    expect_equal(
        idfVU$"Coil:Heating:Fuel"[[1]]$value(1:7),
        idfTR$"Coil:Heating:Fuel"[[1]]$value()
    )

    expect_equal(
        idfVU$"Daylighting:Controls"$Zone1_DaylCtrl$value(),
        idfTR$"Daylighting:Controls"$Zone1_DaylCtrl$value()
    )
    expect_equal(
        idfVU$"Daylighting:Controls"$Zone2_DaylCtrl$value(),
        idfTR$"Daylighting:Controls"$Zone2_DaylCtrl$value()
    )
    expect_equal(
        idfVU$"Daylighting:Controls"$Zone3_DaylCtrl$value(),
        idfTR$"Daylighting:Controls"$Zone3_DaylCtrl$value()
    )
    expect_equal(
        idfVU$"Daylighting:Controls"$DELight1$value(),
        idfTR$"Daylighting:Controls"$DELight1$value()
    )
    expect_equal(
        # NOTE: VersionUpdater failed to update `Lighting Control Type` and always
        # returned "Continuous"
        idfVU$"Daylighting:Controls"$DELight2$value()[-5],
        idfTR$"Daylighting:Controls"$DELight2$value()[-5]
    )
    expect_equal(
        idfTR$"Daylighting:Controls"$DELight2$value("Lighting Control Type")[[1]],
        "Stepped"
    )
    expect_equal(
        # NOTE: VersionUpdater failed to update `Lighting Control Type` and always
        # returned "Continuous"
        idfVU$"Daylighting:Controls"$DELight3$value()[-5],
        idfTR$"Daylighting:Controls"$DELight3$value()[-5]
    )
    expect_equal(
        idfTR$"Daylighting:Controls"$DELight3$value("Lighting Control Type")[[1]],
        "ContinuousOff"
    )

    expect_equal(
        idfVU$"Daylighting:ReferencePoint"$Zone1_DaylRefPt1$value(),
        idfTR$"Daylighting:ReferencePoint"$Zone1_DaylRefPt1$value()
    )
    expect_equal(
        idfVU$"Daylighting:ReferencePoint"$Zone2_DaylRefPt1$value(),
        idfTR$"Daylighting:ReferencePoint"$Zone2_DaylRefPt1$value()
    )
    expect_equal(
        idfVU$"Daylighting:ReferencePoint"$Zone2_DaylRefPt2$value(),
        idfTR$"Daylighting:ReferencePoint"$Zone2_DaylRefPt2$value()
    )
    expect_equal(
        idfVU$"Daylighting:ReferencePoint"$Zone3_DaylRefPt1$value(),
        idfTR$"Daylighting:ReferencePoint"$Zone3_DaylRefPt1$value()
    )
    expect_equal(
        idfVU$"Daylighting:ReferencePoint"$Zone3_DaylRefPt2$value(),
        idfTR$"Daylighting:ReferencePoint"$Zone3_DaylRefPt2$value()
    )
    expect_equal(
        idfVU$"Daylighting:ReferencePoint"$RefPt1$value(),
        idfTR$"Daylighting:ReferencePoint"$RefPt1$value()
    )
    expect_equal(
        idfVU$"Daylighting:ReferencePoint"$RefPt2$value(),
        idfTR$"Daylighting:ReferencePoint"$RefPt2$value()
    )

    expect_equal(
        idfVU$"EnergyManagementSystem:Actuator"[[1]]$value(),
        idfTR$"EnergyManagementSystem:Actuator"[[1]]$value()
    )

    expect_equal(
        tolerance = 1e-4,
        idfVU$"MaterialProperty:MoisturePenetrationDepth:Settings"[[1]]$value(),
        idfTR$"MaterialProperty:MoisturePenetrationDepth:Settings"[[1]]$value()
    )

    # NOTE: VersionUpdater will crash if no matched material found for
    # "MaterialProperty:MoisturePenetrationDepth:Settings"
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "MaterialProperty:MoisturePenetrationDepth:Settings" := list(
                paste0("Mat", 1:2), 0.004, 0.07, 0.40, 0.07, 10.0
            ),
            "Material" = list("Mat1", "Rough", 0.12, 0.16, 800, 800, 0.9, 0.6, 0.6),
            .all = TRUE
        )
    )
    expect_warning(idfTR <- transition(idfOri, to), "Material match issue")
})
# }}}
# v8.6 --> v8.7 {{{
test_that("Transition v8.6 --> v8.7", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 8.6
    to <- 8.7
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "Coil:Cooling:DX:MultiSpeed" = list("ClgCoil1", ..16 = NULL),
            "Coil:Cooling:DX:MultiSpeed" = list("ClgCoil2", ..16 = "PropaneGas"),
            "Coil:Heating:DX:MultiSpeed" = list("HtgCoil1", ..16 = NULL),
            "Coil:Heating:DX:MultiSpeed" = list("HtgCoil2", ..16 = "PropaneGas"),
            "CoolingTower:SingleSpeed" = list("Tower1"),
            "CoolingTower:TwoSpeed" = list("Tower2"),
            "CoolingTower:VariableSpeed:Merkel" = list("Tower3"),
            "AirflowNetwork:SimulationControl" = list("Ctrl"),
            "ZoneCapacitanceMultiplier:ResearchSpecial" = list(),
            "WaterHeater:HeatPump:WrappedCondenser" = list("HP", ..35 = "MutuallyExlcusive"),
            "AirflowNetwork:Distribution:Component:Duct" = list("Duct", ..7 = 0.772, ..8 = 0.0001),
            .all = TRUE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_is(idfTR <- transition(idfOri, to), "Idf")

    expect_equal(
        idfVU$"Coil:Cooling:DX:MultiSpeed"$ClgCoil1$value(1:91),
        idfTR$"Coil:Cooling:DX:MultiSpeed"$ClgCoil1$value()
    )
    expect_equal(
        idfVU$"Coil:Cooling:DX:MultiSpeed"$ClgCoil2$value(1:91),
        idfTR$"Coil:Cooling:DX:MultiSpeed"$ClgCoil2$value()
    )

    expect_equal(
        idfVU$"Coil:Heating:DX:MultiSpeed"$HtgCoil1$value(1:80),
        idfTR$"Coil:Heating:DX:MultiSpeed"$HtgCoil1$value()
    )
    expect_equal(
        idfVU$"Coil:Heating:DX:MultiSpeed"$HtgCoil2$value(1:80),
        idfTR$"Coil:Heating:DX:MultiSpeed"$HtgCoil2$value()
    )

    expect_equal(
        idfVU$"CoolingTower:SingleSpeed"$Tower1$value(),
        idfTR$"CoolingTower:SingleSpeed"$Tower1$value()
    )

    expect_equal(
        idfVU$"CoolingTower:TwoSpeed"$Tower2$value(),
        idfTR$"CoolingTower:TwoSpeed"$Tower2$value()
    )

    expect_equal(
        idfVU$"CoolingTower:VariableSpeed:Merkel"$Tower3$value(),
        idfTR$"CoolingTower:VariableSpeed:Merkel"$Tower3$value()
    )

    expect_equal(
        idfVU$"AirflowNetwork:SimulationControl"$value(),
        idfTR$"AirflowNetwork:SimulationControl"$value()
    )

    expect_equal(
        idfVU$"ZoneCapacitanceMultiplier:ResearchSpecial"[[1]]$value(),
        idfTR$"ZoneCapacitanceMultiplier:ResearchSpecial"[[1]]$value()
    )

    expect_equal(
        idfVU$"WaterHeater:HeatPump:WrappedCondenser"[[1]]$value(1:37),
        idfTR$"WaterHeater:HeatPump:WrappedCondenser"[[1]]$value()
    )

    expect_equal(
        idfVU$"AirflowNetwork:Distribution:Component:Duct"$Duct$value(),
        idfTR$"AirflowNetwork:Distribution:Component:Duct"$Duct$value()
    )
})
# }}}
# v8.7 --> v8.8 {{{
test_that("Transition v8.7 --> v8.8", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 8.7
    to <- 8.8
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "Output:Surfaces:List" = list("DecayCurvesfromZoneComponentLoads"),
            "Table:TwoIndependentVariables" = list("Table"),
            "BuildingSurface:Detailed" = list(
                "Surf1", "Floor", "Const", "Zone",
                "Foundation", "Slab Foundation", "NoSun", "NoWind", "AutoCalculate",
                4,
                45, 28, 0,
                45, 4, 0,
                4, 4, 0,
                4, 28, 0
            ),
            "Floor:Detailed" = list(
                "Surf2", "Const", "Zone",
                "Foundation", "Slab Foundation", "NoSun", "NoWind", "AutoCalculate",
                4,
                45, 28, 0,
                45, 4, 0,
                4, 4, 0,
                4, 28, 0
            ),
            "UnitarySystemPerformance:Multispeed" = list("Unitary"),
            "Coil:Cooling:DX:SingleSpeed" = list("Coil1"),
            "Coil:Cooling:DX:TwoSpeed" = list("Coil2"),
            "Coil:Cooling:DX:MultiSpeed" = list("Coil3"),
            "Coil:Cooling:DX:VariableSpeed" = list("Coil4"),
            "Coil:Cooling:DX:TwoStageWithHumidityControlMode" = list("Coil5"),
            "ZoneHVAC:PackagedTerminalHeatPump" = list("HP"),
            "ZoneHVAC:IdealLoadsAirSystem" = list("Ideal"),
            "ZoneControl:ContaminantController" = list("Cont"),
            "AvailabilityManager:NightCycle" = list("Night"),
            .all = TRUE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_warning(idfTR <- transition(idfOri, to), "'SurfaceProperty:ExposedFoundationPerimeter'")

    expect_equal(
        idfVU$"Output:Surfaces:List"[[1]]$value(1),
        idfTR$"Output:Surfaces:List"[[1]]$value()
    )

    expect_equal(
        idfVU$"Table:TwoIndependentVariables"$Table$value(1:14),
        idfTR$"Table:TwoIndependentVariables"$Table$value()
    )

    expect_equal(
        idfVU$"BuildingSurface:Detailed"$Surf1$value(),
        idfTR$"BuildingSurface:Detailed"$Surf1$value(1:22)
    )

    expect_equal(
        idfVU$"Floor:Detailed"$Surf2$value(),
        idfTR$"Floor:Detailed"$Surf2$value(1:21)
    )

    expect_equal(
        idfVU$"SurfaceProperty:ExposedFoundationPerimeter"[[1]]$value(1:14),
        idfTR$"SurfaceProperty:ExposedFoundationPerimeter"[[1]]$value(1:14)
    )
    expect_equal(
        idfVU$"SurfaceProperty:ExposedFoundationPerimeter"[[2]]$value(1:14),
        idfTR$"SurfaceProperty:ExposedFoundationPerimeter"[[2]]$value(1:14)
    )

    expect_equal(
        idfVU$"UnitarySystemPerformance:Multispeed"$Unitary$value(1:7),
        idfTR$"UnitarySystemPerformance:Multispeed"$Unitary$value()
    )

    expect_equal(
        idfVU$"Coil:Cooling:DX:SingleSpeed"$Coil1$value(1:34),
        idfTR$"Coil:Cooling:DX:SingleSpeed"$Coil1$value()
    )

    expect_equal(
        idfVU$"Coil:Cooling:DX:TwoSpeed"$Coil2$value(1:33),
        idfTR$"Coil:Cooling:DX:TwoSpeed"$Coil2$value()
    )

    expect_equal(
        idfVU$"Coil:Cooling:DX:MultiSpeed"$Coil3$value(1:92),
        idfTR$"Coil:Cooling:DX:MultiSpeed"$Coil3$value()
    )

    expect_equal(
        idfVU$"Coil:Cooling:DX:VariableSpeed"$Coil4$value(1:31),
        idfTR$"Coil:Cooling:DX:VariableSpeed"$Coil4$value()
    )

    expect_equal(
        idfVU$"Coil:Cooling:DX:TwoStageWithHumidityControlMode"$Coil5$value(1:21),
        idfTR$"Coil:Cooling:DX:TwoStageWithHumidityControlMode"$Coil5$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:PackagedTerminalHeatPump"$HP$value(1:26),
        idfTR$"ZoneHVAC:PackagedTerminalHeatPump"$HP$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:IdealLoadsAirSystem"$Ideal$value(1:27),
        idfTR$"ZoneHVAC:IdealLoadsAirSystem"$Ideal$value()
    )

    expect_equal(
        idfVU$"ZoneControl:ContaminantController"$Cont$value(1:4),
        idfTR$"ZoneControl:ContaminantController"$Cont$value()
    )

    expect_equal(
        idfVU$"AvailabilityManager:NightCycle"$Night$value(1:7),
        idfTR$"AvailabilityManager:NightCycle"$Night$value()
    )
})
# }}}
# v8.8 --> v8.9 {{{
test_that("Transition v8.8 --> v8.9", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 8.8
    to <- 8.9
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "ZoneHVAC:EquipmentList" = list("Equip"),
            "GroundHeatExchanger:Vertical" = list(
                "GHP", "Inlet", "Outlet",
                3.3e-4, 24,
                76.2, 0.06, 0.70, 2.34e6, 13.40, 0.70,
                0.39, 0.03, 0.02, 0.002, 2, 0.0005,
                3, -15.2996, -0.348322, -14.201, 0.022208, -13.2202, 0.412345
            ),
            "Branch" = list("Branch", NULL, "GroundHeatExchanger:Vertical", "GHP", "Inlet", "Outlet"),
            "CondenserEquipmentList" = list("CondEquip", "GroundHeatExchanger:Vertical", "GHP"),
            "ElectricEquipment:ITE:AirCooled" = list(),
            "Schedule:Day:Interval" = list(..3 = "yes"),
            "Schedule:Day:List" = list(..3 = "yes"),
            "Schedule:Compact" = list(..3 = "Interpolate: yes"),
            .all = TRUE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_is(idfTR <- transition(idfOri, to), "Idf")

    expect_equal(
        idfVU$"ZoneHVAC:EquipmentList"[[1]]$value(1:6),
        idfTR$"ZoneHVAC:EquipmentList"[[1]]$value()
    )

    expect_equal(
        idfVU$"GroundHeatExchanger:System"[[1]]$value(),
        idfTR$"GroundHeatExchanger:System"[[1]]$value()
    )

    expect_equal(
        tolerance = 1e-5,
        idfVU$"GroundHeatExchanger:Vertical:Properties"[[1]]$value(),
        idfTR$"GroundHeatExchanger:Vertical:Properties"[[1]]$value()
    )
    expect_equal(
        tolerance = 1e-5,
        idfVU$"GroundHeatExchanger:ResponseFactors"[[1]]$value(1:74),
        idfTR$"GroundHeatExchanger:ResponseFactors"[[1]]$value(1:74)
    )

    expect_equal(
        idfVU$"Branch"[[1]]$value(1:6),
        idfTR$"Branch"[[1]]$value()
    )

    expect_equal(
        idfVU$"CondenserEquipmentList"[[1]]$value(1:3),
        idfTR$"CondenserEquipmentList"[[1]]$value()
    )

    expect_equal(
        idfVU$"ElectricEquipment:ITE:AirCooled"[[1]]$value(),
        idfTR$"ElectricEquipment:ITE:AirCooled"[[1]]$value()
    )

    expect_equal(
        idfVU$"Schedule:Day:Interval"[[1]]$value(1:5),
        idfTR$"Schedule:Day:Interval"[[1]]$value()
    )

    expect_equal(
        idfVU$"Schedule:Day:List"[[1]]$value(1:5),
        idfTR$"Schedule:Day:List"[[1]]$value()
    )

    expect_equal(
        idfVU$"Schedule:Compact"[[1]]$value(1:5),
        idfTR$"Schedule:Compact"[[1]]$value()
    )
})
# }}}
# v8.9 --> v9.0 {{{
test_that("Transition v8.9 --> v9.0", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 8.9
    to <- 9.0
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "OutdoorAir:Mixer" = list("OAMixer"),
            "AirflowNetwork:Distribution:Component:OutdoorAirFlow" = list("OA"),
            "AirflowNetwork:Distribution:Component:ReliefAirFlow" = list("RA"),
            "Boiler:HotWater" = list("Boiler"),
            "GlazedDoor" = list("GD"),
            "RunPeriod:CustomRange" = list("RP1", 1, 1, 2020, 1, 2, 2020, "UseWeatherFile"),
            "RunPeriod" = list("RP2", 1, 1, 1, 2, "UseWeatherFile", ..12 = 3, ..14 = 2020),
            "RunPeriod" = list("RP3", 1, 1, 1, 2, "Monday", ..12 = 2),
            "RunPeriod" = list("RP4", 1, 1, 1, 2, "Sunday", ..12 = 2),
            "Table:OneIndependentVariable" = list("Table", "Exponent"),
            "WindowMaterial:ComplexShade" = list("Mat", "Venetian"),
            "FenestrationSurface:Detailed" = list("Fene", "Window", "ConstNoShade", "Surf", ..7 = "Ctrl"),
            "BuildingSurface:Detailed" = list("Surf", "Wall", "Wall", "Zone", "Outdoors"),
            "Zone" = list("Zone"),
            "Window" = list("Win"),
            "WindowProperty:ShadingControl" = list(
                "Ctrl", "ExteriorScreen",
                "Const", "OnIfScheduleAllows", "ScreenSchedule", 20, "Yes", "No"
            ),
            .all = TRUE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_warning(idfTR <- transition(idfOri, to), "UseWeatherFile")

    expect_equal(
        idfVU$"AirflowNetwork:Distribution:Component:OutdoorAirFlow"[[1]]$value(1:4),
        idfTR$"AirflowNetwork:Distribution:Component:OutdoorAirFlow"[[1]]$value()
    )

    expect_equal(
        idfVU$"AirflowNetwork:Distribution:Component:ReliefAirFlow"[[1]]$value(1:4),
        idfTR$"AirflowNetwork:Distribution:Component:ReliefAirFlow"[[1]]$value()
    )

    expect_equal(
        idfVU$"Boiler:HotWater"[[1]]$value(),
        idfTR$"Boiler:HotWater"[[1]]$value()
    )

    expect_equal(
        idfVU$"FenestrationSurface:Detailed"[[1]]$value(1:18),
        idfTR$"FenestrationSurface:Detailed"[[1]]$value()
    )

    expect_equal(
        idfVU$"GlazedDoor"[[1]]$value(1:5),
        idfTR$"GlazedDoor"[[1]]$value()
    )

    expect_equal(
        idfVU$"RunPeriod"$RP1$value(1:13),
        idfTR$"RunPeriod"$RP1$value()
    )
    expect_equal(
        idfVU$"RunPeriod"$RP2$value(1:13),
        idfTR$"RunPeriod"$RP2$value()
    )
    expect_equal(
        idfVU$"RunPeriod"$RP3$value(1:13),
        idfTR$"RunPeriod"$RP3$value()
    )
    expect_equal(
        idfVU$"RunPeriod"$RP4$value(1:13),
        idfTR$"RunPeriod"$RP4$value()
    )

    expect_equal(
        idfVU$"Table:OneIndependentVariable"[[1]]$value(1:14),
        idfTR$"Table:OneIndependentVariable"[[1]]$value()
    )

    expect_equal(
        idfVU$"WindowMaterial:ComplexShade"[[1]]$value(),
        idfTR$"WindowMaterial:ComplexShade"[[1]]$value()
    )

    expect_equal(
        idfVU$"Window"[[1]]$value(1:5),
        idfTR$"Window"[[1]]$value()
    )

    expect_equal(
        idfVU$"WindowShadingControl"[[1]]$value(),
        idfTR$"WindowShadingControl"[[1]]$value()
    )
})
# }}}
# v9.0 --> v9.1 {{{
test_that("Transition v9.0 --> v9.1", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 9.0
    to <- 9.1
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "HybridModel:Zone" = list("HM", "Zone", "no", "no", "sch", 1, 1, 12, 31),
            "ZoneHVAC:EquipmentList" = list("EquipList", NULL, "ZoneHVAC:IdealLoadsAirSystem", "Ideal", 1, 1),
            .all = TRUE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_is(idfTR <- transition(idfOri, to), "Idf")

    expect_equal(
        idfVU$"HybridModel:Zone"[[1]]$value(),
        idfTR$"HybridModel:Zone"[[1]]$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:EquipmentList"[[1]]$value(1:8),
        idfTR$"ZoneHVAC:EquipmentList"[[1]]$value()
    )
})
# }}}
# v9.1 --> v9.2 {{{
test_that("Transition v9.1 --> v9.2", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 9.1
    to <- 9.2
    unlink(file.path(tempdir(), "test.csv"))
    writeLines("", file.path(tempdir(), "test.csv"))
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "Foundation:Kiva" = list(),
            "RunPeriod" := list(c(paste0("runperiod", c("", 1:5), "rp"))),
            "RunPeriod" := list(..2 = rep(1:5)),
            "Schedule:File" = list(..3 = file.path(tempdir(), "test.csv"), ..7 = "fixed"),
            "Table:OneIndependentVariable" = list(
                "One", "Quadratic", "EvaluateCurveToLimits", 0.0, 1.0, 0.85,
                1.0, "Dimensionless", "Dimensionless", NULL, 0.0, 0.85, 0.5,
                0.925, 1.0, 1.0
            ),
            "Table:TwoIndependentVariables" = list(
                "Two1", "BiQuadratic", "LagrangeInterpolationLinearExtrapolation",
                12, 24, 18, 47, 0, 40000, "Temperature", "Temperature",
                "Dimensionless", 25000, NULL,
                12.77778, 36, 19524.15032,
                12.77778, 41, 18178.81244,
                12.77778, 46.11111, 16810.36004,
                15, 18, 25997.3589,
                15, 30, 22716.4017,
                12.77778, 30, 21147.21662,
                12.77778, 35, 19794.00525,
                15, 24, 24352.1562,
                12.77778, 18, 24421.69383,
                12.77778, 24, 22779.73113,
                15, 35, 21360.49033,
                15, 36, 21090.0954,
                15, 41, 19742.05753,
                15, 46.11111, 18370.84513,
                18, 18, 28392.31868,
                23.88889, 41, 27683.36592,
                23.88889, 46.11111, 26301.11353,
                18, 24, 26742.74198,
                18, 30, 25102.61348,
                23.88889, 35, 29314.75872,
                23.88889, 36, 29042.2038,
                19.44448943, 24, 28003.546,
                19.44448943, 30, 26361.31143,
                18, 35, 23743.0571,
                18, 36, 23471.93318,
                18, 41, 22120.2503,
                18, 46.11111, 20745.3119,
                21, 18, 31094.97495,
                21, 24, 29441.02425,
                19.44448943, 18, 29655.22876,
                21, 30, 27796.52175,
                21, 35, 26433.32038,
                21, 36, 26161.46745,
                21, 41, 24806.13958,
                21, 46.11111, 23427.47518,
                23.88889, 18, 33988.3473,
                23.88889, 24, 32330.1846,
                23.88889, 30, 30681.4701,
                19.44448943, 35, 25000,
                19.44448943, 36, 24728.52506,
                19.44448943, 41, 23375.08713,
                19.44448943, 46.11111, 21998.35468
            ),
            "Table:TwoIndependentVariables" = list(
                "Two2", "BiQuadratic", "LagrangeinterpolationLinearExtrapolation",
                12, 24, 18, 47, NULL, NULL, "Temperature", "Temperature",
                "Dimensionless", NULL, NULL,
                12.77778, 36, 19524.15032,
                12.77778, 41, 18178.81244,
                12.77778, 46.11111, 16810.36004,
                15, 18, 25997.3589,
                15, 30, 22716.4017,
                12.77778, 30, 21147.21662,
                12.77778, 35, 19794.00525,
                15, 24, 24352.1562,
                12.77778, 18, 24421.69383,
                12.77778, 24, 22779.73113,
                15, 35, 21360.49033,
                15, 36, 21090.0954,
                15, 41, 19742.05753,
                15, 46.11111, 18370.84513,
                18, 18, 28392.31868,
                23.88889, 41, 27683.36592,
                23.88889, 46.11111, 26301.11353,
                18, 24, 26742.74198,
                18, 30, 25102.61348,
                23.88889, 35, 29314.75872,
                23.88889, 36, 29042.2038,
                19.44448943, 24, 28003.546,
                19.44448943, 30, 26361.31143,
                18, 35, 23743.0571,
                18, 36, 23471.93318,
                18, 41, 22120.2503,
                18, 46.11111, 20745.3119,
                21, 18, 31094.97495,
                21, 24, 29441.02425,
                19.44448943, 18, 29655.22876,
                21, 30, 27796.52175,
                21, 35, 26433.32038,
                21, 36, 26161.46745,
                21, 41, 24806.13958,
                21, 46.11111, 23427.47518,
                23.88889, 18, 33988.3473,
                23.88889, 24, 32330.1846,
                23.88889, 30, 30681.4701,
                19.44448943, 35, 25000,
                19.44448943, 36, 24728.52506,
                19.44448943, 41, 23375.08713,
                19.44448943, 46.11111, 21998.35468
            ),
            "Table:MultiVariableLookup" = list(
                "Multi1", "EvaluateCurveToLimits", 3, "Quadratic",
                "SingleLineIndependentVariableWithMatrix", NULL, "ASCENDING",
                NULL, NULL, 0.5, 1.5, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                NULL, NULL, NULL, 0.8, 1.5, "Dimensionless", NULL, NULL, NULL,
                NULL, NULL, "Dimensionless", 1, 3, 0.0, 1.0, 1.5, 0.8, 1.0, 1.1
            ),
            "Table:MultiVariableLookup" = list(
                "Multi2", "EvaluateCurveToLimits", 3, "Quadratic",
                "SingleLineIndependentVariableWithMatrix", NULL, "ASCENDING",
                NULL, NULL, 0.5, 1.5, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                NULL, NULL, NULL, 0.0, 1.5, "Dimensionless", NULL, NULL, NULL,
                NULL, NULL, "Dimensionless", 1, 4, 0.0, 0.5, 1.0, 1.5, 1.1552,
                1.0712, 1.0, 0.9416
            ),
            "Coil:Cooling:DX:SingleSpeed" = list(
                "Coil", "Sch", "autosize",
                "autosize", 3.0, "autosize", NULL, "Inlet", "Outlet",
                "Two1", "Multi1", "Two2", "Multi2", "One"
            ),
            "ThermalStorage:Ice:Detailed" := list(c("Ice1", "Ice2"), ..6 = c("quadraticlinear", "cubiclinear"), ..8 = c("quadraticlinear", "cubiclinear")),
            "ZoneHVAC:EquipmentList" = list("Equip1"),
            "ZoneHVAC:EquipmentList" = list("Equip2", NULL, "ZoneHVAC:PackagedTerminalHeatPump", "HP", 1, 1),
            "ZoneHVAC:EquipmentList" = list(
                "Equip3", NULL,
                "ZoneHVAC:PackagedTerminalHeatPump", "HP1", 1, 1, 1, 1,
                "ZoneHVAC:PackagedTerminalHeatPump", "HP2", 1, 1, 1, 1,
                "ZoneHVAC:PackagedTerminalHeatPump", "HP3", 1, 1, 1, 1
            ),
            .all = FALSE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_warning(idfTR <- transition(idfOri, to), "comments")

    expect_equal(
        idfVU$"Foundation:Kiva"[[1]]$value(1),
        idfTR$"Foundation:Kiva"[[1]]$value()
    )

    expect_equal(
        idfVU$"RunPeriod"[[1]]$value(),
        idfTR$"RunPeriod"[[1]]$value()
    )

    expect_equal(
        idfVU$"Schedule:File"[[1]]$value()[-3],
        idfTR$"Schedule:File"[[1]]$value()[-3]
    )

    expect_equal(
        idfVU$"Table:IndependentVariable"$One_IndependentVariable1$value(),
        idfTR$"Table:IndependentVariable"$One_IndependentVariable1$value()
    )

    expect_equal(
        tolerance = 1e-5,
        idfVU$"Table:IndependentVariable"$Two1_IndependentVariable1$value(),
        idfTR$"Table:IndependentVariable"$Two1_IndependentVariable1$value()
    )
    expect_equal(
        tolerance = 1e-5,
        idfVU$"Table:IndependentVariable"$Two1_IndependentVariable2$value(),
        idfTR$"Table:IndependentVariable"$Two1_IndependentVariable2$value()
    )
    expect_equal(
        tolerance = 1e-5,
        idfVU$"Table:IndependentVariable"$Two2_IndependentVariable1$value(),
        idfTR$"Table:IndependentVariable"$Two2_IndependentVariable1$value()
    )
    expect_equal(
        tolerance = 1e-5,
        idfVU$"Table:IndependentVariable"$Two2_IndependentVariable2$value(),
        idfTR$"Table:IndependentVariable"$Two2_IndependentVariable2$value()
    )
    expect_equal(
        tolerance = 1e-5,
        idfVU$"Table:IndependentVariable"$Multi1_IndependentVariable1$value(),
        idfTR$"Table:IndependentVariable"$Multi1_IndependentVariable1$value()
    )
    expect_equal(
        tolerance = 1e-5,
        idfVU$"Table:IndependentVariable"$Multi2_IndependentVariable1$value(),
        idfTR$"Table:IndependentVariable"$Multi2_IndependentVariable1$value()
    )

    expect_equal(
        idfVU$"Table:IndependentVariableList"$One_IndependentVariableList$value(),
        idfTR$"Table:IndependentVariableList"$One_IndependentVariableList$value()
    )
    expect_equal(
        idfVU$"Table:IndependentVariableList"$Two1_IndependentVariableList$value(),
        idfTR$"Table:IndependentVariableList"$Two1_IndependentVariableList$value()
    )
    expect_equal(
        idfVU$"Table:IndependentVariableList"$Two2_IndependentVariableList$value(),
        idfTR$"Table:IndependentVariableList"$Two2_IndependentVariableList$value()
    )
    expect_equal(
        idfVU$"Table:IndependentVariableList"$Multi1_IndependentVariableList$value(),
        idfTR$"Table:IndependentVariableList"$Multi1_IndependentVariableList$value()
    )
    expect_equal(
        idfVU$"Table:IndependentVariableList"$Multi2_IndependentVariableList$value(),
        idfTR$"Table:IndependentVariableList"$Multi2_IndependentVariableList$value()
    )

    expect_equal(
        tolerance = 1e-5,
        idfVU$"Table:Lookup"$One$value(),
        idfTR$"Table:Lookup"$One$value()
    )
    expect_equal(
        tolerance = 1e-5,
        idfVU$"Table:Lookup"$Two1$value(),
        idfTR$"Table:Lookup"$Two1$value()
    )
    expect_equal(
        tolerance = 1e-5,
        idfVU$"Table:Lookup"$Two2$value(),
        idfTR$"Table:Lookup"$Two2$value()
    )
    expect_equal(
        tolerance = 1e-5,
        idfVU$"Table:Lookup"$Multi1$value(),
        idfTR$"Table:Lookup"$Multi1$value()
    )
    expect_equal(
        tolerance = 1e-5,
        idfVU$"Table:Lookup"$Multi2$value(),
        idfTR$"Table:Lookup"$Multi2$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:EquipmentList"$Equip1$value(),
        idfTR$"ZoneHVAC:EquipmentList"$Equip1$value()
    )
    expect_equal(
        idfVU$"ZoneHVAC:EquipmentList"$Equip2$value(),
        idfTR$"ZoneHVAC:EquipmentList"$Equip2$value()
    )
    expect_equal(
        idfVU$"ZoneHVAC:EquipmentList"$Equip3$value(),
        idfTR$"ZoneHVAC:EquipmentList"$Equip3$value()
    )

    expect_equal(
        idfVU$"ScheduleTypeLimits"[[1]]$value()[-1],
        idfTR$"ScheduleTypeLimits"[[1]]$value()[-1]
    )

    expect_equal(
        {
            dt_vu <- idfVU$to_table(class = "Schedule:Constant", wide = TRUE, string_value = FALSE)[, -"id"]
        },
        {
            dt_tr <- idfTR$to_table(class = "Schedule:Constant", wide = TRUE, string_value = FALSE)[, -"id"]
            dt_tr[J(unique(dt_vu$name)), on = "name"][, `Schedule Type Limits Name` := gsub("Limits", "Limts", `Schedule Type Limits Name`)]
        }
    )
})
# }}}
# v9.2 --> v9.3 {{{
test_that("Transition v9.2 --> v9.3", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 9.2
    to <- 9.3
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "AirConditioner:VariableRefrigerantFlow" = list("VRF", ..67 = "propanegas"),
            "AirTerminal:SingleDuct:Uncontrolled" := list(
                c("ATDUC1", "ATDUC2"), NULL, c("Node1", "Node2"), "Autosize"
            ),
            "ZoneHVAC:EquipmentList" := list(
                c("EquipList1", "EquipList2"), NULL,
                "AirTerminal:SingleDuct:Uncontrolled",
                c("ATDUC1", "ATDUC2"), 1, 1
            ),
            "AirLoopHVAC:ZoneSplitter" = list("ZS", "ZS Node", "Node1"),
            "AirLoopHVAC:SupplyPlenum" = list("SP", "Zone", "Zone Node", ..5 = "NodeList"),
            "NodeList" = list("NodeList", "Node2"),
            "AirLoopHVAC" := list(c("Loop1", "Loop2"), ..9 = c("Node1", "NodeList")),
            # VersionUpdater cannot handle
            # 'RoomAir:Node:AirflowNetwork:HVACEquipment' correctly
            # See: https://github.com/NREL/EnergyPlus/issues/8372
            # "RoomAir:Node:AirflowNetwork:HVACEquipment" := list(
            #     "AN",
            #     ..2 = "AirTerminal:SingleDuct:Uncontrolled",
            #     ..3 = "ATDUC1",
            #     ..6 = "AirTerminal:SingleDuct:Uncontrolled",
            #     ..7 = "ATDUC2"
            # ),
            "AirflowNetwork:Distribution:Node" := list(
                c("ANDN1", "ANDN2"), c("Node1", "Node2")
            ),
            "AirflowNetwork:Distribution:Linkage" := list(
                c("ANDL1", "ANDL2"), ..3 = c("ANDN1", "ANDN2"),
                ..4 = c("ANDCD1", "ANDCD2")
            ),
            "AirflowNetwork:Distribution:Component:Duct" = list("ANDCD1"),
            "Boiler:HotWater" = list("Boiler", "FuelOil#1"),
            "GlobalGeometryRules" = list(..3 = "Absolute"),
            "HeatPump:WaterToWater:EIR:Heating" = list("HPH"),
            "HeatPump:WaterToWater:EIR:Cooling" = list("HPC"),
            "ShadowCalculation" = list(
                "averageoverdaysinfrequency",
                ..6 = "scheduledshading"
            ),
            .all = TRUE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_warning(idfTR <- transition(idfOri, to), "ANDCD2")

    expect_equal(
        idfVU$"AirConditioner:VariableRefrigerantFlow"$VRF$value(),
        idfTR$"AirConditioner:VariableRefrigerantFlow"$VRF$value()
    )

    expect_equal(
        idfVU$"AirTerminal:SingleDuct:ConstantVolume:NoReheat"$ATDUC1$value(1:5),
        idfTR$"AirTerminal:SingleDuct:ConstantVolume:NoReheat"$ATDUC1$value()
    )
    expect_equal(
        idfVU$"AirTerminal:SingleDuct:ConstantVolume:NoReheat"$ATDUC2$value(1:5),
        idfTR$"AirTerminal:SingleDuct:ConstantVolume:NoReheat"$ATDUC2$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:AirDistributionUnit"$"ATDUC1 ADU"$value(),
        idfTR$"ZoneHVAC:AirDistributionUnit"$"ATDUC1 ADU"$value()
    )
    expect_equal(
        idfVU$"ZoneHVAC:AirDistributionUnit"$"ATDUC2 ADU"$value(),
        idfTR$"ZoneHVAC:AirDistributionUnit"$"ATDUC2 ADU"$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:EquipmentList"$EquipList1$value(1:8),
        idfTR$"ZoneHVAC:EquipmentList"$EquipList1$value()
    )
    expect_equal(
        idfVU$"ZoneHVAC:EquipmentList"$EquipList2$value(1:8),
        idfTR$"ZoneHVAC:EquipmentList"$EquipList2$value()
    )

    expect_equal(
        idfVU$"AirLoopHVAC:ZoneSplitter"$ZS$value(1:3),
        idfTR$"AirLoopHVAC:ZoneSplitter"$ZS$value()
    )

    expect_equal(
        idfVU$"AirLoopHVAC:SupplyPlenum"$SP$value(1:5),
        idfTR$"AirLoopHVAC:SupplyPlenum"$SP$value()
    )

    expect_equal(
        idfVU$NodeList$NodeList$value(1:2),
        idfTR$NodeList$NodeList$value(1:2)
    )
    expect_equal(
        idfVU$NodeList$"NodeList ATInlet"$value(1:2),
        idfTR$NodeList$"NodeList ATInlet"$value(1:2)
    )

    expect_equal(
        idfVU$AirLoopHVAC$Loop1$value(),
        idfTR$AirLoopHVAC$Loop1$value()
    )
    expect_equal(
        idfVU$AirLoopHVAC$Loop2$value(),
        idfTR$AirLoopHVAC$Loop2$value()
    )

    expect_equal(
        idfVU$"AirflowNetwork:Distribution:Node"$ANDN1$value(),
        idfTR$"AirflowNetwork:Distribution:Node"$ANDN1$value()
    )
    expect_equal(
        idfVU$"AirflowNetwork:Distribution:Node"$ANDN2$value(),
        idfTR$"AirflowNetwork:Distribution:Node"$ANDN2$value()
    )
    expect_equal(
        idfVU$"AirflowNetwork:Distribution:Node"$"ANDN1 ATInlet"$value(),
        idfTR$"AirflowNetwork:Distribution:Node"$"ANDN1 ATInlet"$value()
    )
    expect_equal(
        idfVU$"AirflowNetwork:Distribution:Node"$"ANDN2 ATInlet"$value(),
        idfTR$"AirflowNetwork:Distribution:Node"$"ANDN2 ATInlet"$value()
    )

    expect_equal(
        idfVU$"AirflowNetwork:Distribution:Linkage"$ANDL1$value(1:4),
        idfTR$"AirflowNetwork:Distribution:Linkage"$ANDL1$value()
    )
    expect_equal(
        idfVU$"AirflowNetwork:Distribution:Linkage"$ANDL2$value(1:4),
        idfTR$"AirflowNetwork:Distribution:Linkage"$ANDL2$value()
    )
    expect_equal(
        idfVU$"AirflowNetwork:Distribution:Linkage"$"ANDL1 ATInlet"$value(1:4),
        idfTR$"AirflowNetwork:Distribution:Linkage"$"ANDL1 ATInlet"$value()
    )
    # NOTE: VersionUpdater always creates a new duct even the original one
    # references an non-existing distribution duct
    expect_equal(
        idfVU$"AirflowNetwork:Distribution:Linkage"$"ANDL2 ATInlet"$value(1:3),
        idfTR$"AirflowNetwork:Distribution:Linkage"$"ANDL2 ATInlet"$value(1:3)
    )

    expect_equal(
        idfVU$"AirflowNetwork:Distribution:Component:Duct"$ANDCD1$value(),
        idfTR$"AirflowNetwork:Distribution:Component:Duct"$ANDCD1$value()
    )
    expect_equal(
        idfVU$"AirflowNetwork:Distribution:Component:Duct"$"ANDL1 ATInlet Duct"$value(1:8),
        idfTR$"AirflowNetwork:Distribution:Component:Duct"$"ANDL1 ATInlet Duct"$value()
    )
    # NOTE: VersionUpdater always creates a new duct even the original one
    # references an non-existing distribution duct
    expect_equal(
        # idfVU$"AirflowNetwork:Distribution:Component:Duct"$"ANDL2 ATInlet Duct",
        idfTR$"AirflowNetwork:Distribution:Component:Duct"$"ANDL2 ATInlet Duct",
        NULL
    )

    expect_equal(
        idfVU$"Boiler:HotWater"$Boiler$value(),
        idfTR$"Boiler:HotWater"$Boiler$value()
    )

    expect_equal(
        idfVU$GlobalGeometryRules$value(),
        idfTR$GlobalGeometryRules$value()
    )

    expect_equal(
        idfVU$"HeatPump:PlantLoop:EIR:Heating"$HPH$value(),
        idfTR$"HeatPump:PlantLoop:EIR:Heating"$HPH$value()
    )

    expect_equal(
        idfVU$"HeatPump:PlantLoop:EIR:Cooling"$HPC$value(),
        idfTR$"HeatPump:PlantLoop:EIR:Cooling"$HPC$value()
    )

    expect_equal(
        idfVU$ShadowCalculation$value(1:10),
        idfTR$ShadowCalculation$value()
    )

})
# }}}
# v9.3 --> v9.4 {{{
test_that("Transition v9.3 --> v9.4", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 9.3
    to <- 9.4
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "Construction:InternalSource" = list("IS", ..6 = "Mat"),
            "EnergyManagementSystem:Actuator" := list(
                c("Act1", "Act2"),
                ..4 = c("Electric Power Level", "Gas Power Level")
            ),
            "Output:DebuggingData" = list(-1, 1),
            "Output:Diagnostics" := list(
                c("DisplayAllWarnings", "DisplayExtraWarnings"),
                "DisplayUnusedObjects"
            ),
            "PerformancePrecisionTradeoffs" = list(..3 = "Mode05"),
            "ZoneHVAC:LowTemperatureRadiant:VariableFlow" = list(
                "VF", ..5 = 0.013, ..8 = "HeatingDesignCapacity",
                ..10 = 30, ..12 = 1.0),
            "ZoneHVAC:LowTemperatureRadiant:Electric" = list("Elec", ..10 = 2),
            "ZoneHVAC:LowTemperatureRadiant:ConstantFlow" = list(
                "CF", ..5 = 0.013, ..8 = 1.0, ..10 = 170000, ..12 = 0.9),
            "ZoneHVAC:HybridUnitaryHVAC" = list("HU", ..17 = 10),
            "PythonPlugin:Instance" = list("Py"),
            .all = FALSE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_warning(idfTR <- transition(idfOri, to), "Python")

    expect_equal(
        idfVU$"Construction:InternalSource"$IS$value(),
        idfTR$"Construction:InternalSource"$IS$value()
    )

    expect_equal(
        idfVU$"EnergyManagementSystem:Actuator"$Act1$value(),
        idfTR$"EnergyManagementSystem:Actuator"$Act1$value()
    )
    expect_equal(
        idfVU$"EnergyManagementSystem:Actuator"$Act2$value(),
        idfTR$"EnergyManagementSystem:Actuator"$Act2$value()
    )

    expect_equal(
        idfVU$"Output:DebuggingData"$value(),
        idfTR$"Output:DebuggingData"$value()
    )

    # NOTE: VersionUpdater converted all options to upper case
    expect_equal(
        lapply(idfVU$"Output:Diagnostics"$value(), tolower),
        lapply(idfTR$"Output:Diagnostics"$value(), tolower)
    )

    expect_equal(
        idfVU$"PerformancePrecisionTradeoffs"$value(),
        idfTR$"PerformancePrecisionTradeoffs"$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:LowTemperatureRadiant:VariableFlow"$VF$value(),
        idfTR$"ZoneHVAC:LowTemperatureRadiant:VariableFlow"$VF$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:LowTemperatureRadiant:Electric"$Elec$value(),
        idfTR$"ZoneHVAC:LowTemperatureRadiant:Electric"$Elec$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:LowTemperatureRadiant:ConstantFlow"$CF$value(),
        idfTR$"ZoneHVAC:LowTemperatureRadiant:ConstantFlow"$CF$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:HybridUnitaryHVAC"$HU$value(),
        idfTR$"ZoneHVAC:HybridUnitaryHVAC"$HU$value()
    )
})
# }}}
# v9.4 --> v9.5 {{{
test_that("Transition v9.4 --> v9.5", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")
    from <- 9.4
    to <- 9.5
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "Construction:AirBoundary" := list(
                c("CAB1", "CAB2"),
                c("InteriorWindow", "GroupedZones"),
                c("GroupedZones", "IRTSurface")
            ),
            "Coil:Cooling:WaterToAirHeatPump:EquationFit" = list(
                "ClgEF1", "WaterInlet", "WaterOutlet", "AirInlet", "AirOutlet",
                "Autosize", "Autosize", "Autosize", "Autosize", 5.0,
                1, 2, 3, 4, 5,
                1, 2, 3, 4, 5, 6,
                1, 2, 3, 4, 5,
                0, 0
            ),
            "Coil:Heating:WaterToAirHeatPump:EquationFit" := list(
                "HtgEF1", "WaterInlet", "WaterOutlet", "AirInlet", "AirOutlet",
                "Autosize", "Autosize", "Autosize", 5.0,
                1, 2, 3, 4, 5,
                1, 2, 3, 4, 5
            ),
            "Coil:Cooling:WaterToAirHeatPump:EquationFit" = list("ClgEF2"),
            "Coil:Heating:WaterToAirHeatPump:EquationFit" = list("HtgEF2"),
            "Construction:InternalSource" = list(
                "CIS", 2, 2, 1, 0.1, 0, "layer1", "layer2"
            ),
            "Construction:InternalSource" = list("CIS2"),
            "HeatPump:WaterToWater:EquationFit:Cooling" = list(
                "WWClgEF1", "Inlet1", "Outlet1", "Inlet2", "Outlet2",
                "Autosize", "Autosize", "Autosize", "Autosize",
                1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 8.0, 1.0, "Pump"
            ),
            "HeatPump:WaterToWater:EquationFit:Heating" = list(
                "WWHtgEF1", "Inlet1", "Outlet1", "Inlet2", "Outlet2",
                "Autosize", "Autosize", "Autosize", "Autosize",
                1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 8.0, 1.0, "Pump"
            ),
            "HeatPump:WaterToWater:EquationFit:Cooling" = list("WWClgEF2"),
            "HeatPump:WaterToWater:EquationFit:Heating" = list("WWHtgEF2"),
            "ZoneAirMassFlowConservation" = list("Yes"),
            "ZoneHVAC:LowTemperatureRadiant:VariableFlow" = list(
                "RadVF1", "Avail", "Zone", "Surface", "ConvectionOnly",
                0.01, 0.01, "Autosize", 0.35, "MeanAirTemperature", "HalfFlowPower",
                "HeatingDesignCapacity", "Autosize", NULL, NULL, "Autosize", "Inlet1", "Outlet1", 2.0, "HtgSp",
                "CoolingDesignCapacity", "Autosize", NULL, NULL, "Autosize", "Inlet2", "Outlet2", 2.0, "ClgSp",
                "Off", NULL, NULL, NULL
            ),
            "ZoneHVAC:LowTemperatureRadiant:VariableFlow" = list("RadVF2"),
            "ZoneHVAC:LowTemperatureRadiant:ConstantFlow" = list(
                "RadCF1", "Avail", "Zone", "Surface", "ConvectionOnly",
                0.01, 0.01, 400, 0.35, "MeanAirTemperature", 0.8, 8e-05, NULL, 8e5, 10, 0.8, 0.1,
                "Inlet1", "Outlet1", "HighWSch", "LowWSch", "HighCSch", "LowCSch"
            ),
            "ZoneHVAC:LowTemperatureRadiant:ConstantFlow" = list("RadCF2"),
            "ZoneHVAC:Baseboard:RadiantConvective:Water" = list(
                "BoardC1", "Avial", "Inlet", "Outlet", 80, 0.01, "HeatingDesignCapacity",
                "Autosize", NULL, NULL, "Autosize", 0.001, 0.3, 0.3,
                "Surface1", 0.4, "Surface2", 0.2, "Surface3", 0.1
            ),
            "ZoneHVAC:Baseboard:RadiantConvective:Water" = list("BoardC2"),
            "ZoneHVAC:Baseboard:RadiantConvective:Steam" = list(
                "Steam1", "Avail", "Inlet", "Outlet", "HeatingDesignCapacity",
                "Autosize", NULL, NULL, 5, "Autosize", 0.001, 0.3, 0.3,
                "Surface1", 0.4, "Surface2", 0.2, "Surface3", 0.1
            ),
            "ZoneHVAC:Baseboard:RadiantConvective:Steam" = list("Steam2"),
            .all = FALSE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_warning(idfTR <- transition(idfOri, to), "Construction:AirBoundary")

    expect_equal(
        idfVU$"Construction:AirBoundary"$CAB1$value(),
        idfTR$"Construction:AirBoundary"$CAB1$value()
    )
    expect_equal(
        idfVU$"Construction:AirBoundary"$CAB2$value(),
        idfTR$"Construction:AirBoundary"$CAB2$value()
    )

    expect_equal(
        idfVU$"Coil:Cooling:WaterToAirHeatPump:EquationFit"$ClgEF1$value(),
        idfTR$"Coil:Cooling:WaterToAirHeatPump:EquationFit"$ClgEF1$value()
    )
    # version updater remove trailing empty fields
    expect_equal(
        idfVU$"Coil:Cooling:WaterToAirHeatPump:EquationFit"$ClgEF2$value(),
        idfTR$"Coil:Cooling:WaterToAirHeatPump:EquationFit"$ClgEF2$value()[1:13]
    )

    expect_equal(
        idfVU$"Coil:Heating:WaterToAirHeatPump:EquationFit"$HtgEF1$value(),
        idfTR$"Coil:Heating:WaterToAirHeatPump:EquationFit"$HtgEF1$value()
    )
    # version updater removes trailing empty fields
    expect_equal(
        idfVU$"Coil:Heating:WaterToAirHeatPump:EquationFit"$HtgEF2$value(),
        idfTR$"Coil:Heating:WaterToAirHeatPump:EquationFit"$HtgEF2$value()[1:11]
    )

    expect_equal(
        idfVU$"ConstructionProperty:InternalHeatSource"$"CIS Heat Source"$value(),
        idfTR$"ConstructionProperty:InternalHeatSource"$"CIS Heat Source"$value()
    )
    expect_equal(
        idfVU$"ConstructionProperty:InternalHeatSource"$"CIS2 Heat Source"$value(),
        idfTR$"ConstructionProperty:InternalHeatSource"$"CIS2 Heat Source"$value()
    )

    expect_equal(
        idfVU$"HeatPump:WaterToWater:EquationFit:Cooling"$"WWClgEF1"$value(),
        idfTR$"HeatPump:WaterToWater:EquationFit:Cooling"$"WWClgEF1"$value()
    )
    # version updater removes trailing empty fields
    expect_equal(
        idfVU$"HeatPump:WaterToWater:EquationFit:Cooling"$"WWClgEF2"$value(),
        idfTR$"HeatPump:WaterToWater:EquationFit:Cooling"$"WWClgEF2"$value()[1:11]
    )

    expect_equal(
        idfVU$"HeatPump:WaterToWater:EquationFit:Heating"$"WWHtgEF1"$value(),
        idfTR$"HeatPump:WaterToWater:EquationFit:Heating"$"WWHtgEF1"$value()
    )
    # version updater removes trailing empty fields
    expect_equal(
        idfVU$"HeatPump:WaterToWater:EquationFit:Heating"$"WWHtgEF2"$value(),
        idfTR$"HeatPump:WaterToWater:EquationFit:Heating"$"WWHtgEF2"$value()[1:11]
    )

    expect_equal(
        idfVU$"ZoneAirMassFlowConservation"$value(),
        idfTR$"ZoneAirMassFlowConservation"$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:LowTemperatureRadiant:VariableFlow"$RadVF1$value(),
        idfTR$"ZoneHVAC:LowTemperatureRadiant:VariableFlow"$RadVF1$value()
    )
    expect_equal(
        idfVU$"ZoneHVAC:LowTemperatureRadiant:VariableFlow"$RadVF2$value(),
        idfTR$"ZoneHVAC:LowTemperatureRadiant:VariableFlow"$RadVF2$value()
    )

    # version updater fails to keep the "Hydronnic Tubing Length"
    # see: https://github.com/NREL/EnergyPlus/issues/9171
    expect_equal(
        idfVU$"ZoneHVAC:LowTemperatureRadiant:ConstantFlow"$RadCF1$value()[-6],
        idfTR$"ZoneHVAC:LowTemperatureRadiant:ConstantFlow"$RadCF1$value()[-6]
    )
    expect_equal(
        idfVU$"ZoneHVAC:LowTemperatureRadiant:ConstantFlow"$RadCF2$value()[-6],
        idfTR$"ZoneHVAC:LowTemperatureRadiant:ConstantFlow"$RadCF2$value()[-6]
    )

    expect_equal(
        idfVU$"ZoneHVAC:Baseboard:RadiantConvective:Water"$BoardC1$value(),
        idfTR$"ZoneHVAC:Baseboard:RadiantConvective:Water"$BoardC1$value()
    )
    expect_equal(
        idfVU$"ZoneHVAC:Baseboard:RadiantConvective:Water"$BoardC2$value(),
        idfTR$"ZoneHVAC:Baseboard:RadiantConvective:Water"$BoardC2$value()
    )

    expect_equal(
        idfVU$"ZoneHVAC:Baseboard:RadiantConvective:Steam"$Steam1$value(),
        idfTR$"ZoneHVAC:Baseboard:RadiantConvective:Steam"$Steam1$value()
    )
    expect_equal(
        idfVU$"ZoneHVAC:Baseboard:RadiantConvective:Steam"$Steam2$value(),
        idfTR$"ZoneHVAC:Baseboard:RadiantConvective:Steam"$Steam2$value()
    )
})
# }}}
# v9.5 --> v9.6 {{{
test_that("Transition v9.5 --> v9.6", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_TRANSITION_") != "")

    # Only install EnergyPlus v9.6 for testing v9.5 to v9.6 transition, because
    # this version of transition breaks `Wall:Detailed`, `Floor:Detailed`, etc.
    # See: https://github.com/NREL/EnergyPlus/issues/9172
    if (!is_avail_eplus(9.6)) install_eplus(9.6, local = TRUE)
    expect_true(is_avail_eplus(9.6))

    from <- 9.5
    to <- 9.6
    expect_is(
        class = "Idf",
        idfOri <- temp_idf(from,
            "AirflowNetwork:MultiZone:ReferenceCrackConditions" = list("AN", NULL),
            "AirLoopHVAC:OutdoorAirSystem" = list(
                "OAS", "OAS Ctrl", "OAS Equip", "OAS Avail"
            ),
            "BuildingSurface:Detailed" = list(
                "Surface", "Wall", "Const", "Zone", "Outdoors", NULL,
                "SunExposed", "WindExposed", 0.5, 3,
                0, 0, 0, 1, 1, 1, 2, 2, 2
            ),
            "Ceiling:Interzone" = list(
                "CI", "Const", "Zone", "Outdoors", 0, 90, 0, 0, 0, 0.5, 0.5
            ),
            "Ceiling:Adiabatic" = list(
                "CA", "Const", "Zone", 0, 90, 0, 0, 0, 0.5, 0.5
            ),
            "Controller:MechanicalVentilation" = list(
                "CMV", "Avail", "Yes", "VentilationRateProcedure", 1.0, "Zone", "OASpec", "AirDistSpec"
            ),
            "Floor:Detailed" = list(
                "FD", "Const", "Zone", "Outdoors", NULL,
                "SunExposed", "WindExposed", 0.5, 3,
                0, 0, 0, 1, 1, 1, 2, 2, 2
            ),
            "Floor:GroundContact" = list(
                "FG", "Const", "Zone", 0, 90, 0, 0, 0, 0.5, 0.5
            ),
            "Floor:Adiabatic" = list(
                "FA", "Const", "Zone", 0, 90, 0, 0, 0, 0.5, 0.5
            ),
            "Floor:Interzone" = list(
                "FI", "Const", "Zone", "Outdoors", 0, 90, 0, 0, 0, 0.5, 0.5
            ),
            "GroundHeatExchanger:System" = list(
                "GHES1", "Inlet", "Outlet", 0.0003,
                "Site:GroundTemperature:Undisturbed:KusudaAchenbach",
                "Vertical Ground Heat Exchanger Ground Temps",
                0.7, 2347000,
                "Vertical Ground Heat Exchanger g-functions"
            ),
            "GroundHeatExchanger:System" = list(
                "GHES2", "Inlet", "Outlet", 0.0003,
                "Site:GroundTemperature:Undisturbed:KusudaAchenbach",
                "Vertical Ground Heat Exchanger Ground Temps",
                0.7, 2347000, NULL,
                "Array1", "Single1", "Array2", "Single2"
            ),
            "InternalMass" = list("IM", "Const", "Zone", 50),
            "RoofCeiling:Detailed" = list(
                "RD", "Const", "Zone", "Outdoors", NULL,
                "SunExposed", "WindExposed", 0.5, 3,
                0, 0, 0, 1, 1, 1, 2, 2, 2
            ),
            "Sizing:System" = list("SS", ..27 = "VentilationRateProcedure"),
            "Roof" = list(
                "CI", "Const", "Zone", 0, 90, 0, 0, 0, 0.5, 0.5
            ),
            "PerformancePrecisionTradeoffs" = list("Yes", "ScriptF", "Mode06"),
            "Wall:Detailed" = list(
                "WD", "Const", "Zone", "Outdoors", NULL,
                "SunExposed", "WindExposed", 0.5, 3,
                0, 0, 0, 1, 1, 1, 2, 2, 2
            ),
            "Wall:Exterior" = list(
                "WE", "Const", "Zone", 0, 90, 0, 0, 0, 0.5, 0.5
            ),
            "Wall:Adiabatic" = list(
                "WA", "Const", "Zone", 0, 90, 0, 0, 0, 0.5, 0.5
            ),
            "Wall:Underground" = list(
                "WU", "Const", "Zone", 0, 90, 0, 0, 0, 0.5, 0.5
            ),
            "Wall:Interzone" = list(
                "WI", "Const", "Zone", "Outdoors", 0, 90, 0, 0, 0, 0.5, 0.5
            ),
            .all = FALSE
        )
    )

    expect_is(idfVU <- version_updater(idfOri, to), "Idf")
    expect_is(idfTR <- transition(idfOri, to), "Idf")

    expect_equal(
        idfVU$"AirflowNetwork:MultiZone:ReferenceCrackConditions"$AN$value(),
        idfTR$"AirflowNetwork:MultiZone:ReferenceCrackConditions"$AN$value()
    )

    expect_equal(
        idfVU$"AirLoopHVAC:OutdoorAirSystem"$OAS$value(),
        idfTR$"AirLoopHVAC:OutdoorAirSystem"$OAS$value()
    )

    expect_equal(
        idfVU$"BuildingSurface:Detailed"$Surface$value(),
        idfTR$"BuildingSurface:Detailed"$Surface$value()
    )

    expect_equal(
        idfVU$"Ceiling:Interzone"$CI$value(),
        idfTR$"Ceiling:Interzone"$CI$value()
    )

    expect_equal(
        idfVU$"Ceiling:Adiabatic"$CA$value(),
        idfTR$"Ceiling:Adiabatic"$CA$value()
    )

    expect_equal(
        idfVU$"Controller:MechanicalVentilation"$CMV$value(),
        idfTR$"Controller:MechanicalVentilation"$CMV$value()
    )

    expect_equal(
        idfVU$"Floor:Detailed"$FD$value(),
        idfTR$"Floor:Detailed"$FD$value()
    )

    expect_equal(
        idfVU$"Floor:GroundContact"$FG$value(),
        idfTR$"Floor:GroundContact"$FG$value()
    )

    expect_equal(
        idfVU$"Floor:Adiabatic"$FA$value(),
        idfTR$"Floor:Adiabatic"$FA$value()
    )

    expect_equal(
        idfVU$"Floor:Interzone"$FI$value(),
        idfTR$"Floor:Interzone"$FI$value()
    )

    expect_equal(
        idfVU$"GroundHeatExchanger:System"$GHES1$value(),
        idfTR$"GroundHeatExchanger:System"$GHES1$value()
    )
    expect_equal(
        tolower(idfVU$"GroundHeatExchanger:System"$GHES2$value()),
        tolower(idfTR$"GroundHeatExchanger:System"$GHES2$value())
    )

    expect_equal(
        idfVU$"InternalMass"$IM$value(),
        idfTR$"InternalMass"$IM$value()
    )

    expect_equal(
        idfVU$"RoofCeiling:Detailed"$RD$value(),
        idfTR$"RoofCeiling:Detailed"$RD$value()
    )

    expect_equal(
        idfVU$"Sizing:System"[[1]]$value(),
        idfTR$"Sizing:System"[[1]]$value()
    )

    expect_equal(
        idfVU$"PerformancePrecisionTradeoffs"$value(),
        idfTR$"PerformancePrecisionTradeoffs"$value()
    )

    expect_equal(
        idfVU$"Wall:Detailed"$WD$value(),
        idfTR$"Wall:Detailed"$WD$value()
    )

    expect_equal(
        idfVU$"Wall:Exterior"$WE$value(),
        idfTR$"Wall:Exterior"$WE$value()
    )

    expect_equal(
        idfVU$"Wall:Adiabatic"$WA$value(),
        idfTR$"Wall:Adiabatic"$WA$value()
    )

    expect_equal(
        idfVU$"Wall:Underground"$WU$value(),
        idfTR$"Wall:Underground"$WU$value()
    )

    expect_equal(
        idfVU$"Wall:Interzone"$WI$value(),
        idfTR$"Wall:Interzone"$WI$value()
    )
})
# }}}
