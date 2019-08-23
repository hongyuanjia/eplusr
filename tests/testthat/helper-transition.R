# temp_idf {{{
temp_idf <- function (ver, ...) {
    idf <- empty_idf(ver)

    l <- list(...)
    if (length(l)) with_option(list(validate_level = "draft"), idf$add(...))

    idf$save(tempfile(fileext = ".idf"))
    idf
}
# }}}

prefix <- function (idf) tools::file_path_sans_ext(basename(idf$path()))

content <- function (idf) idf$to_string(header = FALSE, comment = FALSE)

# build_trans_test_idf {{{
build_trans_test_idf <- function (ver, ..., .exclude = NULL, .report_vars = TRUE) {
    verbose <- eplusr_option("verbose_info")
    level <- eplusr_option("validate_level")
    comp <- eplusr_option("autocomplete")

    eplusr_option(validate_level = "draft", verbose_info = FALSE, autocomplete = FALSE)
    on.exit(eplusr_option(validate_level = level, verbose_info = verbose, autocomplete = comp), add = TRUE)

    ver <- standardize_ver(ver)
    # For 8.2. Otherwise IDFVersionUpdater will fail {{{
    if (ver == 8.2) {
        # use a defined file
        idf <- read_idf("files/v8.2.idf")
        idf$save(tempfile(fileext = ".idf"))
    # }}}
    # For 8.8. Otherwise IDFVersionUpdater will fail
    } else if (ver == 8.8) {
        # use a defined file
        idf <- read_idf("files/v8.8.idf")
        idf$save(tempfile(fileext = ".idf"))
    } else {
        idf <- temp_idf(ver, ...)
        if (!length(list(...)) && !is.character(.report_vars)) {
            cls <- class_updated[[as.character(ver[, 1:2])]]

            lst <- rep(list(list()), length(cls))
            names(lst) <- cls
            idf$add(lst, .default = TRUE, .all = TRUE)

            idf$save(overwrite = TRUE)
        }

        # for 8.5
        if (ver == 8.5) {
            # add a material to test moisture penetration depth
            prop <- idf$objects_in_class("MaterialProperty:MoisturePenetrationDepth:Settings")[[1L]]
            prop$set(name = "GP01", 0.004, 0.07, 0.39, 0.01, 11.71)
            idf$add(Material = list(name = "GP01", "MediumSmooth", 0.013, 0.16, 801, 837, 0.9, 0.75, 0.75))
            idf$save(overwrite = TRUE)
        # for 8.9
        } else if (ver == 8.9) {
            # add necessary input for the RunPeriod object
            rp <- idf$objects_in_class("RunPeriod")[[1L]]
            rp$set("name", 1, 2, 3, 4, .default = FALSE)
            idf$save(overwrite = TRUE)
        }
    }

    # report variable {{{
    if (isTRUE(.report_vars) || is.character(.report_vars)) {
        # add output variables
        f <- as.double(as.character(idf$version()[, 1:2]))
        rep_vars <- REPORTVAR_RULES[J(f), on = "from", nomatch = 0L]

        if (nrow(rep_vars)) {
            rep_vars[, id := .I]
            data.table::setnames(rep_vars, "old", "value")

            if (length(.exclude$report_var)) rep_vars <- rep_vars[!J(.exclude$report_var), on = "value"]

            position <- list(
                list(class = "Output:Variable", index = 2L),
                list(class = "Output:Meter", index = 1L),
                list(class = "Output:Meter:MeterFileOnly", index = 1L),
                list(class = "Output:Meter:Cumulative", index = 1L),
                list(class = "Output:Meter:Cumulative:MeteringFileOnly", index = 1L),
                list(class = "Output:Table:TimeBins", index = 2L),
                list(class = "ExternalInterface:FunctionalMockupUnitImport:From:Variable", index = 2L),
                list(class = "ExternalInterface:FunctionalMockupUnitExport:From:Variable", index = 2L),
                list(class = "EnergyManagementSystem:Sensor", index = 3L),
                list(class = "Output:Table:Monthly", index = 3L),
                list(class = "Meter:Custom", index = 4L),
                list(class = "Meter:CustomDecrement", index = 3L)
            )
            position <- rbindlist(position)[idf$is_valid_class(class, all = TRUE)]

            if (is.character(.report_vars)) position <- position[J(.report_vars), on = "class"]

            dt <- rbindlist(apply2(position$class, position$index, more_args = list(rep_vars = rep_vars),
                function (rep_vars, class, index) {
                    set(copy(rep_vars), NULL, c("class", "index"), list(class, index))
                }
            ))

            idf$load(dt)
            idf$save(overwrite = TRUE)
        }
    }
    # }}}

    if (!is.null(.exclude$class)) {
        cls <- .exclude$class[idf$is_valid_class(.exclude$class)]
        if (length(cls)) {
            idf$del(idf$object_id(cls, simplify = TRUE))
            idf$save(overwrite = TRUE)
        }
    }

    idf
}
# }}}

# get_both_trans {{{
get_both_trans <- function (from, to, ..., .exclude = NULL, .report_vars = TRUE) {
    verbose <- eplusr_option("verbose_info")
    level <- eplusr_option("validate_level")
    comp <- eplusr_option("autocomplete")

    eplusr_option(validate_level = "draft", verbose_info = FALSE, autocomplete = FALSE)
    on.exit(eplusr_option(validate_level = level, verbose_info = verbose, autocomplete = comp), add = TRUE)

    # build test idf
    idf <- suppressWarnings(build_trans_test_idf(from, ..., .exclude = .exclude, .report_vars = .report_vars))

    # eplusr transition
    eplusr <- suppressWarnings(transition(idf, to, save = TRUE, dir = file.path(tempdir(), "eplusr")))

    # EnergyPlus preprocessor IDFVersionUpdater
    ep <- version_updater(idf, to, dir = file.path(tempdir(), "ep"))
    ep$`Output:PreprocessorMessage` <- NULL

    if (!is.null(.exclude$post_class)) {
        cls <- .exclude$post_class[eplusr$is_valid_class(.exclude$post_class)]
        if (length(cls)) {
            eplusr$del(eplusr$object_id(cls, simplify = TRUE))
            eplusr$save(overwrite = TRUE)
        }

        cls <- .exclude$post_class[ep$is_valid_class(.exclude$post_class)]
        if (length(cls)) {
            ep$del(ep$object_id(cls, simplify = TRUE))
            ep$save(overwrite = TRUE)
        }
    }

    list(eplusr = eplusr, energyplus = ep)
}
# }}}

# expect_identical_transition {{{
expect_identical_transition <- function (from, to, ..., .exclude = NULL, .report_vars = TRUE,
                                         .skip_equal = NULL, .skip_after = NULL) {
    trans <- get_both_trans(from, to, ..., .exclude = .exclude, .report_vars = .report_vars)

    # check output version
    expect_equal(trans$eplusr$version(), trans$energyplus$version(),
        label = "eplusr transition output version",
        expected.label = "IDFVersionUpdater output",
        info = paste0("Transition ", from, " --> ", to)
    )

    # check output class
    expect_equal(trans$eplusr$class_name(), trans$energyplus$class_name(),
        label = "eplusr transition output classes",
        expected.label = "IDFVersionUpdater output",
        info = paste0("Transition ", from, " --> ", to)
    )

    # check object contents
    for (cls in trans$eplusr$class_name()) {
        str_eplusr <- trans$eplusr$to_string(class = cls, header = FALSE, format = "new_top", comment = FALSE)
        str_energyplus <- trans$energyplus$to_string(class = cls, header = FALSE, format = "new_top", comment = FALSE)

        if (cls %in% names(.skip_after)) {
            if (length(str_eplusr) >= .skip_after[[cls]]) {
                str_eplusr <- c(
                    str_eplusr[1L],
                    str_eplusr[-1L][setdiff(1:(.skip_after[[cls]] - 1L), .skip_equal[[cls]])]
                )
            }
            if (length(str_energyplus) >= .skip_after[[cls]]) {
                str_energyplus <- c(
                    str_energyplus[1L],
                    str_energyplus[-1L][setdiff(1:(.skip_after[[cls]] - 1L), .skip_equal[[cls]])])
            }
        } else if (cls %in% names(.skip_equal)) {
            if (any(length(str_eplusr) >= (.skip_equal[[cls]] + 1L))) {
                str_eplusr <- str_eplusr[-(.skip_equal[[cls]] + 1L)]
            }
            if (any(length(str_energyplus) >= (.skip_equal[[cls]] + 1L))) {
                str_energyplus <- str_energyplus[-(.skip_equal[[cls]] + 1L)]
            }
        }

        expect_equal(str_eplusr, str_energyplus,
            label = paste0("eplusr transition output objects of class ", surround(cls)),
            expected.label = paste0("IDFVersionUpdater transition output"),
            info = paste0("Transition ", from, " --> ", to)
        )
    }
}
# }}}

# class_updated {{{
class_updated <- list(
    `7.2` = c(
        "ShadowCalculation",                                       # 1
        "Coil:Heating:DX:MultiSpeed",                              # 2
        "EnergyManagementSystem:OutputVariable",                   # 3
        "EnergyManagementSystem:MeteredOutputVariable",            # 4
        "Branch",                                                  # 5
        "PlantEquipmentList",                                      # 6
        "CondenserEquipmentList",                                  # 7
        "HeatExchanger:WatersideEconomizer",                       # 8
        "HeatExchanger:Hydronic",                                  # 9
        "HeatExchanger:Plate",                                     # 10
        "BuildingSurface:Detailed",                                # 11
        "Wall:Detailed",                                           # 12
        "RoofCeiling:Detailed",                                    # 13
        "Floor:Detailed",                                          # 14
        "FenestrationSurface:Detailed",                            # 15
        "Shading:Site:Detailed",                                   # 16
        "Shading:Building:Detailed",                               # 17
        "Shading:Zone:Detailed",                                   # 18
        "AirflowNetwork:Distribution:Component:ConstantVolumeFan", # 19
        "ZoneHVAC:HighTemperatureRadiant",                         # 20
        "AirConditioner:VariableRefrigerantFlow",                  # 21
        "ZoneHVAC:WaterToAirHeatPump",                             # 22
        "AirLoopHVAC:UnitaryHeatPump:WaterToAir",                  # 23
        "Boiler:HotWater",                                         # 24
        "Chiller:Electric",                                        # 25
        "Chiller:ConstantCOP",                                     # 26
        "Chiller:EngineDriven",                                    # 27
        "Chiller:CombustionTurbine",                               # 28
        "Chiller:Electric:EIR",                                    # 29
        "Chiller:Electric:ReformulatedEIR",                        # 30
        "Chiller:Absorption",                                      # 31
        "Chiller:Absorption:Indirect"                              # 32
    ),

    `8.0` = c(
        "People",                                            # 1
        "CoolingTower:SingleSpeed",                          # 2
        "CoolingTower:TwoSpeed",                             # 3
        "EvaporativeFluidCooler:SingleSpeed",                # 4
        "EvaporativeFluidCooler:TwoSpeed",                   # 5
        "FluidCooler:TwoSpeed",                              # 6
        "HeatPump:WaterToWater:EquationFit:Heating",         # 7
        "HeatPump:WaterToWater:EquationFit:Cooling",         # 8
        "HeatPump:WaterToWater:ParameterEstimation:Heating", # 9
        "HeatPump:WaterToWater:ParameterEstimation:Cooling", # 10
        "HVACTemplate:Zone:PTAC",                            # 11
        "HVACTemplate:Zone:PTHP",                            # 12
        "HVACTemplate:Zone:WaterToAirHeatPump",              # 13
        "HVACTemplate:System:Unitary",                       # 14
        "HVACTemplate:System:UnitaryHeatPump:AirToAir"       # 15
    ),

    `8.1` = c(
        "ZoneHVAC:UnitVentilator",                       # 1
        "ZoneHVAC:UnitHeater",                           # 2
        "PlantLoop",                                     # 3
        "CondenserLoop",                                 # 4
        "HVACTemplate:Plant:ChilledWaterLoop",           # 5
        "HVACTemplate:Plant:HotWaterLoop",               # 6
        "HVACTemplate:Plant:MixedWaterLoop",             # 7
        "Sizing:System",                                 # 8
        "ZoneHVAC:Baseboard:RadiantConvective:Water",    # 9
        "ZoneHVAC:HighTemperatureRadiant",               # 10
        "ZoneHVAC:Baseboard:RadiantConvective:Steam",    # 11
        "ZoneHVAC:Baseboard:RadiantConvective:Electric", # 12
        "ZoneHVAC:Baseboard:Convective:Water",           # 13
        "ZoneHVAC:Baseboard:Convective:Electric",        # 14
        "ZoneHVAC:LowTemperatureRadiant:VariableFlow",   # 15
        "ZoneHVAC:LowTemperatureRadiant:Electric"        # 16
    ),

    `8.2` = c(
        "Chiller:Electric:ReformulatedEIR",           # 1
        "Site:GroundDomain",                          # 2
        "GroundHeatExchanger:Vertical",               # 3
        "EvaporativeCooler:Indirect:ResearchSpecial", # 4
        "EvaporativeCooler:Direct:ResearchSpecial"    # 5
    ),

    `8.3` = c(
        "Coil:WaterHeating:AirToWaterHeatPump",     # 1
        "WaterHeater:Stratified",                   # 2
        "WaterHeater:HeatPump",                     # 3
        "Branch",                                   # 4
        "ZoneHVAC:EquipmentList",                   # 5
        "PlantEquipmentList",                       # 6
        "EvaporativeCooler:Direct:ResearchSpecial", # 7
        "Controller:MechanicalVentilation",         # 8
        "Site:GroundDomain:Slab",                   # 9
        "Site:GroundDomain:Basement",               # 10
        "PipingSystem:Underground:Domain",          # 11
        "Pipe:Underground",                         # 12
        "GroundHeatExchanger:HorizontalTrench",     # 13
        "GroundHeatExchanger:Slinky",               # 14
        "HVACTemplate:Plant:ChilledWaterLoop",      # 15
        "HVACTemplate:Plant:HotWaterLoop",          # 16
        "HVACTemplate:Plant:MixedWaterLoop",        # 17
        "ZoneAirMassFlowConservation"               # 18
    ),

    `8.4` = c(
        "EnergyManagementSystem:Actuator" # 1
    ),

    `8.5` = c(
        "Exterior:FuelEquipment",                             # 1
        "HVACTemplate:System:UnitarySystem",                  # 2
        "HVACTemplate:System:Unitary",                        # 3
        "ChillerHeater:Absorption:DirectFired",               # 4
        "SetpointManager:SingleZone:Humidity:Minimum",        # 5
        "SetpointManager:SingleZone:Humidity:Maximum",        # 6
        "AirTerminal:SingleDuct:VAV:Reheat",                  # 7
        "Branch",                                             # 8
        "AirTerminal:SingleDuct:InletSideMixer",              # 9
        "AirTerminal:SingleDuct:SupplySideMixer",             # 10
        "ZoneHVAC:AirDistributionUnit",                       # 11
        "OtherEquipment",                                     # 12
        "Coil:Heating:Gas",                                   # 13
        "Daylighting:Controls",                               # 14
        "Daylighting:DELight:ReferencePoint",                 # 15
        "Daylighting:DELight:Controls",                       # 16
        "MaterialProperty:MoisturePenetrationDepth:Settings", # 17
        "EnergyManagementSystem:Actuator"                     # 18
    ),

    `8.6` = c(
        "Coil:Cooling:DX:MultiSpeed",                # 1
        "Coil:Heating:DX:MultiSpeed",                # 2
        "CoolingTower:SingleSpeed",                  # 3
        "CoolingTower:TwoSpeed",                     # 4
        "CoolingTower:VariableSpeed:Merkel",         # 5
        "AirflowNetwork:SimulationControl",           # 6
        "ZoneCapacitanceMultiplier:ResearchSpecial", # 7
        "WaterHeater:HeatPump:WrappedCondenser",     # 8
        "AirflowNetwork:Distribution:Component:Duct" # 9
    ),

    `8.7` = c(
        "Output:Surfaces:List",                           # 1
        "Table:TwoIndependentVariables",                  # 2
       # Only used to check corresponding perimeter object
        "BuildingSurface:Detailed",                       # 3
       # Only used to check corresponding perimeter object
        "Floor:Detailed",                                 # 4
        "SurfaceProperty:ExposedFoundationPerimeter",     # 5
        "Foundation:Kiva:Settings",                       # 6
        "UnitarySystemPerformance:Multispeed",            # 7
        "Coil:Cooling:DX:SingleSpeed",                    # 8
        "Coil:Cooling:DX:TwoSpeed",                       # 9
        "Coil:Cooling:DX:MultiSpeed",                     # 10
        "Coil:Cooling:DX:VariableSpeed",                  # 11
        "Coil:Cooling:DX:TwoStageWithHumidityControlMode",# 12
        "ZoneHVAC:PackagedTerminalHeatPump",              # 13
        "ZoneHVAC:IdealLoadsAirSystem",                   # 14
        "ZoneControl:ContaminantController",              # 15
        "AvailabilityManager:NightCycle"                  # 16
    ),

    `8.8` = c(
        "ZoneHVAC:EquipmentList",          # 1
        "GroundHeatExchanger:Vertical",    # 2
        "Branch",                          # 3
        "CondenserEquipmentList",          # 4
        "ElectricEquipment:ITE:AirCooled", # 5
        "Schedule:Day:Interval",           # 6
        "Schedule:Day:List",               # 7
        "Schedule:Compact"                 # 8
    ),

    `8.9` = c(
        "AirflowNetwork:Distribution:Component:OutdoorAirFlow", # 1
        "AirflowNetwork:Distribution:Component:ReliefAirFlow",  # 2
        "Boiler:HotWater",                                      # 3
        "FenestrationSurface:Detailed",                         # 4
        "GlazedDoor",                                           # 5
        "RunPeriod:CustomRange",                                # 6
        "RunPeriod",                                            # 7
        "Table:OneIndependentVariable",                         # 8
        "WindowMaterial:ComplexShade",                          # 9
        "Window",                                               # 10
        "WindowProperty:ShadingControl"                         # 11
    ),

    `9.0` = c(
        "HybridModel:Zone",      # 1
        "ZoneHVAC:EquipmentList" # 2
    )
)
# }}}
