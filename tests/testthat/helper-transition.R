temp_idf <- function (ver, ...) {
    idf <- empty_idf(ver)

    l <- list(...)
    if (length(l)) idf$add(...)

    idf$save(tempfile(fileext = ".idf"))
    idf
}

prefix <- function (idf) tools::file_path_sans_ext(basename(idf$path()))

content <- function (idf) idf$to_string(header = FALSE)

# expect_identical_trans {{{
expect_identical_trans <- function (from, to, ...) {
    verbose <- eplusr_option("verbose_info")
    level <- eplusr_option("validate_level")

    eplusr_option(validate_level = "draft", verbose_info = FALSE)
    on.exit(eplusr_option(validate_level = level, verbose_info = verbose), add = TRUE)

    idf <- temp_idf(from, ...)
    if (!length(list(...))) {
        cls <- class_updated[[as.character(from)]]

        lst <- rep(list(list()), length(cls))
        names(lst) <- cls
        idf$add(lst, .default = TRUE, .all = TRUE)

        idf$save(overwrite = TRUE)
    }

    eplusr <- suppressWarnings(transition(idf, to, save = TRUE, dir = file.path(tempdir(), "eplusr")))
    ep <- version_updater(idf, to, dir = file.path(tempdir(), "ep"))
    ep$`Output:PreprocessorMessage` <- NULL

    expect_identical(content(eplusr), content(ep))
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
        "EvaporativeCooler:SingleSpeed",                     # 4
        "EvaporativeCooler:TwoSpeed",                        # 5
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
        "Sizing:System",                                 # 7
        "ZoneHVAC:Baseboard:RadiantConvective:Water",    # 8
        "ZoneHVAC:HighTemperatureRadiant",               # 9
        "ZoneHVAC:Baseboard:RadiantConvective:Steam",    # 10
        "ZoneHVAC:Baseboard:RadiantConvective:Electric", # 11
        "ZoneHVAC:Baseboard:Convective:Water",           # 12
        "ZoneHVAC:Baseboard:Convective:Electric",        # 13
        "ZoneHVAC:LowTemperatureRadiant:VariableFlow",   # 14
        "ZoneHVAC:LowTemperatureRadiant:Electric"        # 15
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
        "AirflowNetwork:SimulationContrl",           # 6
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
        "UnitarySystemPerformance:MultiSpeed",            # 7
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
