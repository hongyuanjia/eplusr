# CLASS_DEL_COMMON {{{
CLASS_DEL_COMMON <- c(
    "PROGRAMCONTROL",
    "SKY RADIANCE DISTRIBUTION",
    "AIRFLOW MODEL",
    "GENERATOR:FC:BATTERY DATA",
    "AIRFLOWNETWORK:MULTIZONE:SITEWINDCONDITIONS",
    "WATER HEATER:SIMPLE"
)
# }}}

# REPORTVAR_RULES{{{
REPORTVAR_RULES <- fread("
    from , to  , old                                                       , new
    8.3  , 8.4 , Zone Supply Air Mass Flow Rate                            , Zone Air Mass Balance Supply Mass Flow Rate
    8.3  , 8.4 , Zone Exhaust Air Mass Flow Rate                           , Zone Air Mass Balance Exhaust Mass Flow Rate
    8.3  , 8.4 , Zone Return Air Mass Flow Rate                            , Zone Air Mass Balance Return Mass Flow Rate
    8.3  , 8.4 , Zone Mixing Receiving Air Mass Flow Rate                  , Zone Air Mass Balance Mixing Receiving Mass Flow Rate
    8.3  , 8.4 , Zone Mixing Source Air Mass Flow Rate                     , Zone Air Mass Balance Mixing Source Mass Flow Rate
    8.3  , 8.4 , Zone Infiltration Air Mass Flow Balance Status            , Zone Air Mass Balance Infiltration Status
    8.3  , 8.4 , Zone Mass Balance Infiltration Air Mass Flow Rate         , Zone Air Mass Balance Infiltration Mass Flow Rate
    8.4  , 8.5 , Wall Interface Heat Flux                                  , GroundDomain Basement Wall Interface Heat Flux
    8.4  , 8.5 , Wall Interface Temperature                                , GroundDomain Basement Wall Interface Temperature
    8.4  , 8.5 , Floor Interface Heat Flux                                 , GroundDomain Basement Floor Interface Heat Flux
    8.4  , 8.5 , Floor Interface Temperature                               , GroundDomain Basement Floor Interface Temperature
    8.4  , 8.5 , Zone Coupled Surface Heat Flux                            , GroundDomain Slab Zone Coupled Surface Heat Flux
    8.4  , 8.5 , Zone Coupled Surface Temperature                          , GroundDomain Slab Zone Coupled Surface Temperature
    8.4  , 8.5 , Availability Manager Optimum Start Hours Before Occupancy , Availability Manager Optimum Start Time Before Occupancy
    8.4  , 8.5 , Electric Storage Charge State                             , Electric Storage Simple Charge State
    8.4  , 8.5 , Electric Storage Charge State                             , Electric Storage Battery Charge Sate
    8.5  , 8.6 , EMPD Surface Inside Face Humidity Ratio                   ,
    8.5  , 8.6 , EMPD Surface Inside Face Relative Humidity                ,
    8.5  , 8.6 , Zone Mixing Current Density Air Volume Flow Rate          , Zone Mixing Current Density Volume Flow Rate
    8.5  , 8.6 , Zone Mixing Standard Density Air Volume Flow Rate         , Zone Mixing Standard Density Volume Flow Rate
    8.5  , 8.6 , Zone Mixing Current Density Volumetric Flow Rate          , Zone Mixing Current Density Volume Flow Rate
    8.5  , 8.6 , Zone Mixing Standard Density Volumetric Flow Rate         , Zone Mixing Standard Density Volume Flow Rate
    8.8  , 8.9 , Heating Coil Total Heating Rate                           , Heating Coil Heating Rate
    8.8  , 8.9 , Heating Coil Total Heating Energy                         , Heating Coil Heating Energy
    8.8  , 8.9 , Heating Coil Air Heating Rate                             , Heating Coil Heating Rate
    8.8  , 8.9 , Heating Coil Air Heating Energy                           , Heating Coil Heating Energy
")
setindexv(REPORTVAR_RULES, c("from", "to"))
# }}}

#' Perform version transition of EnergyPlus model
#'
#' `transition()` takes an [Idf] object or a path of IDF file and a target
#' version, performs version transitions and returns an [Idf] object of
#' specified version.
#'
#' @param idf An [Idf] object or a path of IDF file.
#' @param ver A valid EnergyPlus version, e.g. `9`, `8.8`, or `"8.8.0"`.
#' @param keep_all If `TRUE`, a list will be return which contains all
#' [Idf] objects of intermediate versions. The list will be named using first
#' two number of that version, e.g. `8.1`, `8.2`.
#' @return An [Idf] object if `keep_all` is `FALSE` or a list of [Idf] objects
#' if `keep_all` is `TRUE`.
#' @export
# transition {{{
transition <- function (idf, ver, keep_all = FALSE) {
    if (!is_idf(idf)) idf <- read_idf(idf)

    assert(is_idd_ver(ver))
    ver <- standardize_ver(ver)[, 1L:2L]

    if (idf$version() < 8.2) {
        abort("error_trans_not_supported",
            paste0(
                "Input IDF has version ", surround(idf$version()), ". ",
                "Currently only EnergyPlus v8.2 and above are suppored."
            )
        )
    }

    # only compare Major and Mversioninor version, skip patch version
    if (idf$version()[, 1L:2L] == ver) {
        verbose_info("IDF is already at latest version ", ver, ". No transition needed.")
        if (keep_all) {
            res <- list(idf)
            setattr(res, "names", as.character(idf$version()[, 1L:2L]))
            return(res)
        } else {
            return(idf)
        }
    # cannot go reverse
    } else if (idf$version()[, 1L:2L] > ver) {
        abort("error_trans_reverse", "Only version update is supported. Downgrade is not supported.")
    }

    with_option(
        list(verbose_info = FALSE, validate_level = "none"),
        trans_apply(idf, ver, keep_all)
    )
}
# }}}

# trans_apply {{{
trans_apply <- function (idf, ver, keep_all) {
    # get all versions needed to handle
    vers <- trans_upper_versions(idf, ver)

    # get corresponding transition function names
    funs <- trans_fun_names(vers)

    # apply transition functions
    if (!keep_all) {
        for (i in funs)  idf <- trans_funs[[i]](idf)
        idf
    } else {
        res <- vector("list", length(funs) + 1L)
        res[[1L]] <- idf

        for (i in seq_along(res)) {
            if (i == length(res)) break
            res[[i + 1L]] <- trans_funs[[funs[[i]]]](res[[i]])
        }
        nms <- paste0(stri_sub(funs, 5L, 5L), ".", stri_sub(funs, 6L, 6L))
        setattr(res, "names", c(as.character(idf$version()[, 1L:2L]), nms))

        res
    }
}
# }}}

trans_funs <- new.env(parent = emptyenv())
# trans_820_830 {{{
trans_funs$f820t830 <- function (idf) {
    assert(idf$version()[, 1:2] == 8.2)

    target_cls <- c(
        "Chiller:Electric:ReformulatedEIR",           # 1
        "Site:GroundDomain",                          # 2
        "GroundHeatExchanger:Vertical",               # 3
        "EvaporativeCooler:Indirect:ResearchSpecial", # 4
        "EvaporativeCooler:Direct:ResearchSpecial"    # 5
    )

    new_idf <- trans_preprocess(idf, 8.3, target_cls)

    # 1: Chiller:Electric:ReformulatedEIR {{{
    dt1 <- trans_action(idf, "Chiller:Electric:ReformulatedEIR", min_fields = 10L,
        insert = list(10L, "LeavingCondenserWaterTemperature")
    )
    # }}}
    # 2: Site:GroundDomain {{{
    dt2 <- trans_action(idf, class = c("Site:GroundDomain:Slab" = "Site:GroundDomain"))
    # }}}
    # 3: GroundHeatExchanger:Vertical {{{
    dt3 <- trans_action(idf, "GroundHeatExchanger:Vertical", offset = list(11L, 4L))
    # }}}
    # 4: EvaporativeCooler:Indirect:ResearchSpecial {{{
    dt4 <- trans_action(idf, "EvaporativeCooler:Indirect:ResearchSpecial", all = TRUE,
        reset = list(4L, NA_character_),
        insert = list(5L:6L),
        insert = list(8L:9L),
        insert = list(11L, "1.0"),
        insert = list(12L, "Autosize"),
        reset = list(17L, "Autosize"),
        insert = list(20L)
    )
    if (nrow(dt4)) {
        dt[J(c(13L:14L)), on = "index", value := {
            val <- suppressWarnings(as.double(value))
            value[1L] <- as.character(val[[2L]]/ val[[1L]])
            value[2L] <- NA_character_
            value
        }, by = "id"]
    }
    # }}}
    # 5: EvaporativeCooler:Direct:ResearchSpecial {{{
    dt5 <- trans_action(idf, "EvaporativeCooler:Direct:ResearchSpecial",
        insert = list(4L),
        insert = list(6L:7L)
    )
    # }}}

    dt <- rbindlist(mget(paste0("dt", 1:5)))
    if (nrow(dt)) new_idf$load(dt, .default = FALSE)

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_830_840 {{{
trans_funs$f830t840 <- function (idf) {
    assert(idf$version()[, 1:2] == 8.3)

    target_cls <- c(
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
    )

    new_idf <- trans_preprocess(idf, 8.4, target_cls)

    # 1: Coil:WaterHeating:AirToWaterHeatPump {{{
    dt1 <- trans_action(idf, class = c("Coil:WaterHeating:AirToWaterHeatPump:Pumped" = "Coil:WaterHeating:AirToWaterHeatPump"))
    # }}}
    # 2: WaterHeater:Stratified {{{
    dt2 <- trans_action(idf, "WaterHeater:Stratified", all = TRUE, insert = list(64L:65L))
    # }}}
    # 3: WaterHeater:HeatPump {{{
    dt3 <- trans_action(idf, all = TRUE,
        class = c("WaterHeater:HeatPump:PumpedCondenser" = "WaterHeater:HeatPump"),
        reset = list(21L, "Coil:WaterHeating:AirToWaterHeatPump", "Coil:WaterHeating:AirToWaterHeatPump:Pumped"),
        insert = list(24L),
        add = list(37L:39L)
    )
    if (nrow(dt3)) {
        dt3[, value := {
            if (is.na(value[17L])) {
                heater_num <- 0L
            } else if (stri_trans_tolower(value[17L]) == "waterheater:stratified") {
                heater_num <- 0L
            } else if (stri_trans_tolower(value[36L]) == "heater1") {
                heater_num <- 1L
            } else if (stri_trans_tolower(value[36L]) == "heater2") {
                heater_num <- 2L
            } else {
                heater_num <- 0L
            }
            value[36L] <- NA_character_

            if (heater_num > 0L) {
                tank <- idf$object(.BY$id)$ref_to_object(18L)[[1L]]
                if (length(tank)) {
                    if (stri_trans_tolower(tank$class_name()) == "waterheater:stratified") {
                        if (heater_num == 1L) {
                            value[37L] <- as.character(tank$Heater_1_Height)
                        } else {
                            value[37L] <- as.character(tank$Heater_2_Height)
                        }
                    }
                }
            }

        }, by = "id"]
    }
    # }}}
    # 4: Branch {{{
    dt4 <- trans_action(idf, "Branch")
    if (nrow(dt4)) {
        dt4[(index - 4L) %% 5L == 0L & stri_trans_tolower(value) == "waterheater:heatpump",
            value := "WaterHeater:HeatPump:PumpedCondenser"
        ]
    }
    # }}}
    # 5: ZoneHVAC:EquipmentList {{{
    dt5 <- trans_action(idf, "ZoneHVAC:EquipmentList")
    if (nrow(dt5)) {
        dt5[(index - 2L) %% 4L == 0L & stri_trans_tolower(value) == "waterheater:heatpump",
            value := "WaterHeater:HeatPump:PumpedCondenser"
        ]
    }
    # }}}
    # 6: PlantEquipmentList {{{
    dt6 <- trans_action(idf, "PlantEquipmentList")
    if (nrow(dt6)) {
        dt6[(index - 2L) %% 2L == 0L & stri_trans_tolower(value) == "waterheater:heatpump",
            value := "WaterHeater:HeatPump:PumpedCondenser"
        ]
    }
    # }}}
    # 7: EvaporativeCooler:Direct:ResearchSpecial {{{
    dt7 <- trans_action(idf, "EvaporativeCooler:Direct:ResearchSpecial", min_fields = 5L,
        insert = list(5L, "Autosize")
    )
    # }}}
    # 8: Controller:MechanicalVentilation {{{
    dt8 <- trans_action(idf, "Controller:MechanicalVentilation",
        reset = list(4L, "ProportionalControl", "ProportionalControlBasedonOccupancySchedule")
    )
    # }}}
    ka_num <- 0L
    # 9: Site:GroundDomain:Slab {{{
    dt9 <- trans_action(idf, "Site:GroundDomain:Slab", all = TRUE)
    if (nrow(dt9)) {
        dt9_2 <- dt9[J(c(1L, 5L:7L, 10L:12L)), on = "index"]
        set(dt9_2, NULL, "index", rowidv(dt9_2, "id"))
        set(dt9_2, NULL, "class", "Site:GroundTemperature:Undisturbed:KusudaAchenbach")
        dt9_2[J(1L), on = "index", value := paste0("KATemp", seq.int(.N) + ka_num)]

        dt9_1 <- dt9[!J(12L), on = "index"][
            J(10L), on = "index", value := "Site:GroundTemperature:Undisturbed:KusudaAchenbach"][
            J(11L), on = "index", value := paste0("KATemp", seq.int(.N) + ka_num)]
        set(dt9_1, NULL, "index", rowidv(dt9_1, "id"))

        dt9 <- rbindlist(list(dt9_1, dt9_2))
        ka_num <- ka_num + length(unique(dt9_1$id))
    }
    # }}}
    # 10: Site:GroundDomain:Basement {{{
    dt10 <- trans_action(idf, "Site:GroundDomain:Basement", all = TRUE)
    if (nrow(dt10)) {
        dt10_2 <- dt10[J(c(1L, 5L:7L, 10L:12L)), on = "index"]
        set(dt10_2, NULL, "index", rowidv(dt10_2, "id"))
        set(dt10_2, NULL, "class", "Site:GroundTemperature:Undisturbed:KusudaAchenbach")
        dt10_2[J(1L), on = "index", value := paste0("KATemp", seq.int(.N) + ka_num)]

        dt10_1 <- dt10[!J(12L), on = "index"][
            J(10L), on = "index", value := "Site:GroundTemperature:Undisturbed:KusudaAchenbach"][
            J(11L), on = "index", value := paste0("KATemp", seq.int(.N) + ka_num)]
        set(dt10_1, NULL, "index", rowidv(dt10_1, "id"))

        dt10 <- rbindlist(list(dt10_1, dt10_2))
        ka_num <- ka_num + length(unique(dt10_1$id))
    }
    # }}}
    # 11: PipingSystem:Underground:Domain {{{
    dt11 <- trans_action(idf, "PipingSystem:Underground:Domain", all = TRUE)
    if (nrow(dt11)) {
        dt11_2 <- dt11[J(c(1L, 14L:16L, 19L:21L)), on = "index"]
        set(dt11_2, NULL, "index", rowidv(dt11_2, "id"))
        set(dt11_2, NULL, "class", "Site:GroundTemperature:Undisturbed:KusudaAchenbach")
        dt11_2[J(1L), on = "index", value := paste0("KATemp", seq.int(.N) + ka_num)]

        dt11_1 <- dt11[!J(21L), on = "index"][
            J(19L), on = "index", value := "Site:GroundTemperature:Undisturbed:KusudaAchenbach"][
            J(20L), on = "index", value := paste0("KATemp", seq.int(.N) + ka_num)]
        set(dt11_1, NULL, "index", rowidv(dt11_1, "id"))

        dt11 <- rbindlist(list(dt11_1, dt11_2))
        ka_num <- ka_num + length(unique(dt11_1$id))
    }
    # }}}
    # 12: Pipe:Underground {{{
    dt12 <- trans_action(idf, "Pipe:Underground", all = TRUE)
    if (nrow(dt12)) {
        dt12_2 <- dt12[J(c(1L:3L, 8L:11L)), on = "index"]
        set(dt12_2, NULL, "index", rowidv(dt12_2, "id"))
        set(dt12_2, NULL, "class", "Site:GroundTemperature:Undisturbed:KusudaAchenbach")
        dt12_2[J(1L), on = "index", value := paste0("KATemp", seq.int(.N) + ka_num)]
        dt12_2[J(c(2L:4L)), value := {
            # get material properties
            mat <- idf$object(.BY$id)$ref_to_object(8L, "Material")[[1L]]
            if (length(mat)) {
                value <- mat$value(simplify = TRUE)[2L:4L]
            }
            value
        }, by = "id"]

        dt12_1 <- dt12[!J(11L), on = "index"][
            J(9L), on = "index", value := "Site:GroundTemperature:Undisturbed:KusudaAchenbach"][
            J(10L), on = "index", value := paste0("KATemp", seq.int(.N) + ka_num)]
        set(dt12_1, NULL, "index", rowidv(dt12_1, "id"))

        dt12 <- rbindlist(list(dt12_1, dt12_2))
        ka_num <- ka_num + length(unique(dt12_1$id))
    }
    # }}}
    # 13: GroundHeatExchanger:HorizontalTrench {{{
    dt13 <- trans_action(idf, "GroundHeatExchanger:HorizontalTrench", all = TRUE)
    if (nrow(dt13)) {
        dt13_2 <- dt13[J(c(1L, 11L:13L, 19L:21L)), on = "index"]
        set(dt13_2, NULL, "index", rowidv(dt13_2, "id"))
        set(dt13_2, NULL, "class", "Site:GroundTemperature:Undisturbed:KusudaAchenbach")
        dt13_2[J(1L), on = "index", value := paste0("KATemp", seq.int(.N) + ka_num)]

        dt13_1 <- dt13[!J(21L), on = "index"][
            J(19L), on = "index", value := "Site:GroundTemperature:Undisturbed:KusudaAchenbach"][
            J(20L), on = "index", value := paste0("KATemp", seq.int(.N) + ka_num)]
        set(dt13_1, NULL, "index", rowidv(dt13_1, "id"))

        dt13 <- rbindlist(list(dt13_1, dt13_2))
        ka_num <- ka_num + length(unique(dt13_1$id))
    }
    # }}}
    # 14: GroundHeatExchanger:Slinky {{{
    dt14 <- trans_action(idf, "GroundHeatExchanger:Slinky", all = TRUE)
    if (nrow(dt14)) {
        dt14_2 <- dt14[J(c(1L, 5L:7L, 20L:22L)), on = "index"]
        set(dt14_2, NULL, "index", rowidv(dt14_2, "id"))
        set(dt14_2, NULL, "class", "Site:GroundTemperature:Undisturbed:KusudaAchenbach")
        dt14_2[J(1L), on = "index", value := paste0("KATemp", seq.int(.N) + ka_num)]

        dt14_1 <- dt14[!J(22L), on = "index"][
            J(20L), on = "index", value := "Site:GroundTemperature:Undisturbed:KusudaAchenbach"][
            J(21L), on = "index", value := paste0("KATemp", seq.int(.N) + ka_num)]
        set(dt14_1, NULL, "index", rowidv(dt14_1, "id"))

        dt14 <- rbindlist(list(dt14_1, dt14_2))
        ka_num <- ka_num + length(unique(dt14_1$id))
    }
    # }}}
    # 15: HVACTemplate:Plant:ChilledWaterLoop {{{
    dt15 <- trans_action(idf, "HVACTemplate:Plant:ChilledWaterLoop",
        reset = list(32L, "Sequential", "SequentialLoad"),
        reset = list(32L, "Uniform", "UniformLoad"),
        reset = list(33L, "Sequential", "SequentialLoad"),
        reset = list(33L, "Uniform", "UniformLoad")
    )
    # }}}
    # 16: HVACTemplate:Plant:HotWaterLoop {{{
    dt16 <- trans_action(idf, "HVACTemplate:Plant:HotWaterLoop",
        reset = list(21L, "Sequential", "SequentialLoad"),
        reset = list(21L, "Uniform", "UniformLoad")
    )
    # }}}
    # 17: HVACTemplate:Plant:MixedWaterLoop {{{
    dt17 <- trans_action(idf, "HVACTemplate:Plant:MixedWaterLoop",
        reset = list(17L, "Sequential", "SequentialLoad"),
        reset = list(17L, "Uniform", "UniformLoad")
    )
    # }}}
    # 18: ZoneAirMassFlowConservation {{{
    dt18 <- trans_action(idf, "ZoneAirMassFlowConservation",
        add = list(3L, "MixingSourceZonesOnly")
    )
    if (nrow(dt18)) {
        dt18[, value := {
            if (stri_trans_tolower(value[1L]) != "yes") {
                value[2L] <- "None"
            }
            value
        }, by = "id"]
    }
    # }}}

    dt <- rbindlist(mget(paste0("dt", 1:18)))
    if (nrow(dt)) new_idf$load(dt, .default = FALSE)

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_840_850 {{{
trans_funs$f840t850 <- function (idf) {
    assert(idf$version()[, 1:2] == 8.4)

    target_cls <- c(
        "EnergyManagementSystem:Actuator" # 1
    )

    new_idf <- trans_preprocess(idf, 8.5, target_cls)

    # 1: EnergyManagementSystem:Actuator {{{
    dt <- trans_action(idf, "EnergyManagementSystem:Actuator",
        reset = list(4L, "outdoor air dryblub temperature", "Outdoor Air Drybulb Temperature"),
        reset = list(4L, "outdoor air wetblub temperature", "Outdoor Air Wetbulb Temperature")
    )
    # }}}

    if (nrow(dt)) new_idf$load(dt, .default = FALSE)

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_850_860 {{{
trans_funs$f850t860 <- function (idf) {
    assert(idf$version()[, 1:2] == 8.5)

    target_cls <- c(
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
    )

    new_idf <- trans_preprocess(idf, 8.6, target_cls)

    # 1: Exterior:FuelEquipment {{{
    dt1 <- trans_action(idf, "Exterior:FuelEquipment",
        reset = list(2L, "Gas", "NaturalGas"),
        reset = list(2L, "LPG", "PropaneGas")
    )
    # }}}
    # 2: HVACTemplate:System:UnitarySystem {{{
    dt2 <- trans_action(idf, "HVACTemplate:System:UnitarySystem", all = TRUE, delete = list(57))
    # }}}
    # 3: HVACTemplate:System:Unitary {{{
    dt3 <- trans_action(idf, "HVACTemplate:System:Unitary", all = TRUE, delete = list(40L))
    # }}}
    # 4: ChillerHeater:Absorption:DirectFired {{{
    dt4 <- trans_action(idf, "ChillerHeater:Absorption:DirectFired", delete = list(33L))
    # }}}
    # 5: SetpointManager:SingleZone:Humidity:Minimum {{{
    dt5 <- trans_action(idf, "SetpointManager:SingleZone:Humidity:Minimum", delete = list(2:3))
    # }}}
    # 6: SetpointManager:SingleZone:Humidity:Maximum {{{
    dt6 <- trans_action(idf, "SetpointManager:SingleZone:Humidity:Maximum", delete = list(2:3))
    # }}}
    # 7: AirTerminal:SingleDuct:VAV:Reheat {{{
    dt7 <- trans_action(idf, "AirTerminal:SingleDuct:VAV:Reheat", all = TRUE)
    if (nrow(dt7)) {
        dt[, value := {
            if (!anyNA(value[16L:18L]) & stri_trans_tolower(value[16]) == "reverse") {
                value[16] <- "ReverseWithLimits"
            }
            value
        }, by = "id"]
    }
    # }}}
    # 8: Branch {{{
    dt8 <- trans_action(idf, "Branch", delete = list(2), delete = list(8, step = 5))
    # }}}
    # 9: AirTerminal:SingleDuct:InletSideMixer {{{
    dt9 <- trans_action(idf, all = TRUE,
        class = c("AirTerminal:SingleDuct:Mixer" = "AirTerminal:SingleDuct:InletSideMixer"),
        add = list(7L, "InletSide")
    )
    # }}}
    # 10: AirTerminal:SingleDuct:SupplySideMixer {{{
    dt10 <- trans_action(idf, all = TRUE,
        class = c("AirTerminal:SingleDuct:Mixer" = "AirTerminal:SingleDuct:SupplySideMixer"),
        add = list(7L, "SupplySide")
    )
    # }}}
    # 11: ZoneHVAC:AirDistributionUnit {{{
    dt11 <- trans_action(idf, "ZoneHVAC:AirDistributionUnit", all = TRUE,
        reset = list(3L, "AirTerminal:SingleDuct:InletSideMixer", "AirTerminal:SingleDuct:InletSideMixer"),
        reset = list(3L, "AirTerminal:SingleDuct:SupplySideMixer", "AirTerminal:SingleDuct:InletSideMixer")
    )
    # }}}
    # 12: OtherEquipment {{{
    dt12 <- trans_action(idf, "OtherEquipment", insert = list(2L, "None"))
    # }}}
    # 13: Coil:Heating:Gas {{{
    dt13 <- trans_action(idf, min_fields = 2L,
        class = c("Coil:Heating:Fuel" = "Coil:Heating:Gas"),
        insert = list(3L, "NaturalGas"))
    # }}}
    # 14: Daylighting:Controls {{{
    dt14_1 <- trans_action(idf, "Daylighting:Controls", all = TRUE,
        offset = list(
            c(20L, 16:19, 13L, 14:15, 9L,  11L, 10L, 12L),
            c(4L,  6:9,   5L,  11:12, 15L, 16L, 18L, 19L)
        ),
        reset = list(3, "SplitFlux"),
        reset = list(8, "0", NA_character_),
        reset = list(13, NA_character_)
    )
    if (!nrow(dt14_1)) {
        dt14_2 <- data.table()
        dt14_3 <- data.table()
    } else {
        dt14_1 <- dt14_1[index <=19L]
        dt14_1[, value := {
            nm <- if (is.na(value[1L])) "" else value[1L]
            value[10L] <- paste0(nm, "_DaylRefPt1")
            value[14L] <- paste0(nm, "_DaylRefPt1")
            if (value[2L] == "2") {
                value[17L] <- paste0(nm, "_DaylRefPt2")
            } else {
                value[17L:19L] <- NA_character_
            }
            value[1L] <- paste0(nm, "_DaylCtrl")
            value[2L] <- value[1L]

            value
        }, by = "id"]

        dt14_1[J(5L), on = "index", value := {
            if (is.na(value)) {
                "Continuous"
            } else if (value == "1") {
                "Continuous"
            } else if (value == "2") {
                "Stepped"
            } else if (value == "3") {
                "ContinuousOff"
            } else {
                "Continuous"
            }
        }, by = "id"]

        # add reference points
        dt14_2 <- trans_action(idf, all = TRUE,
            class = c("Daylighting:ReferencePoint" = "Daylighting:Controls")
        )
        dt14_3 <- dt14_2[J(c(1L, 2L, 6L:8L))]

        dt14_2 <- dt14_2[index <= 5L]
        dt14_2[, value := {
            value[2L] <- value[1L]
            value[1L] <- if (is.na(value[1L])) "_DaylRefPt1" else paste0(value[1L], "_DaylRefPt1")
            value
        }, by = "id"]

        obj_id <- dt14_3[J(2L, "2"), on = c("index", "value"), id, nomatch = 0L]
        if (!length(id)) {
            dt14_3 <- data.table()
        } else {
            dt14_3 <- dt14_3[J(obj_id), on = "id"][
                J(c(6L:8L)), on = "index", value := c(3L:5L), by = "id"
            ][, value := {
                value[2L] <- value[1L]
                value[1L] <- if (is.na(value[1L])) "_DaylRefPt2" else paste0(value[1L], "_DaylRefPt2")
                value
            }, by = "id"]
        }
    }
    dt14 <- rbindlist(list(dt14_1, dt14_2, dt14_3))
    # }}}
    # 15: Daylighting:DELight:ReferencePoint {{{
    dt15 <- trans_action(idf, min_fields = 5L,
        class = c("Daylighting:ReferencePoint" = "Daylighting:DELight:ReferencePoint")
    )
    if (nrow(dt15)) {
        dt15 <- dt15[index <= 5L]
        nm_zone <- vcapply(unique(dt15$id),
            function (id) {
                zone <- idf$object(id)$ref_to_object(2L, class = "Zone", recursive = TRUE)[[1L]]
                if (!length(zone)) NA_character_ else zone$name()
            }
        )
        dt15[J(2L), on = "index", value := nm_zone]
    }
    # }}}
    # 16: Daylighting:Delight:Controls {{{
    dt16 <- trans_action(idf, all = TRUE,
        class = c("Daylighting:Controls" = "Daylighting:DELight:Controls"),
        insert = list(3L, "DElight"),
        insert = list(4L, NA_character_),
        add = list(10L:13L, c(NA_character_, "0", NA_character_, NA_character_))
    )
    if (nrow(dt16)) {
        dt16_1 <- dt16[index <= 13L][, value := {
            if (is.na(value[5L])) {
                value[5L] <- "Continuous"
            } else if (value[5L] == "1") {
                value[5L] <- "Continuous"
            } else if (value[5L] == "2") {
                value[5L] <- "Stepped"
            } else if (value[5L] == "3") {
                value[5L] <- "ContinuousOff"
            } else {
                value[5L] <- "Continuous"
            }

            val <- value[8L]
            if (!is.na(value[8L]) && value[8L] == "0") value[8L] <- NA_character_
            value[13] <- val

            value
        }, by = "id"]

        # extract reference point name, zone name, and illuminance setpoint
        dt16_2 <- lapply(unique(dt16_1$id),
            function (id) {
                refp <- idf$object(id)$ref_by_object(1L, "Daylighting:DELight:ReferencePoint")
                if (!length(refp)) {
                    data.table()
                } else {
                    dt <- rbindlist(lapply(refp, function (x) x$to_table()))[index %in% c(1L, 6L, 7L)]
                    # update object id
                    set(dt, NULL, "id", id)
                    set(dt, NULL, "class", "Daylighting:Controls")
                    set(dt, NULL, "index", seq.int(nrow(dt)) + 13L)
                }
            }
        )

        dt16 <- rbindlist(list(dt16_1, dt16_2))
    }
    # }}}
    # 17: MaterialProperty:MoisturePenetrationDepth:Settings {{{
    dt17 <- trans_action(idf,
        class = "MaterialProperty:MoisturePenetrationDepth:Settings",
        insert = list(8L:10L, "0")
    )
    if (nrow(dt17)) {
        # cal_mu_empd {{{
        # reference: CalculateMuEMPD() in UtilityRoutines.f90 under Transition
        cal_mu_empd <- function (a, b, c, d, d_empd, density) {
            TEMP <- 24
            RH <- 0.45
            P_ambient <- 101325
            Seconds <- 24L * 60L * 60L

            slope_MC <- a * b * RH ** (b - 1) + c * d * RH ** (d - 1)
            PV_sat = psat_fn_tdb(TEMP) * 1000
            diffusivity_EMPD = d_empd ** 2 * 3.1415926535 * slope_MC * density / (Seconds * PV_sat)
            diffusivity_air = 2.0e-7 * (TEMP + 273.15) ** 0.81 / P_ambient
            diffusivity_air / diffusivity_EMPD
        }
        # }}}
        # psat_fn_tdb {{{
        # reference: GetSatVapPressFromDryBulb() in UtilityRoutines.f90 under Transition
        psat_fn_tdb <- function (tdb) {
            tk <- tdb + 273.15
            C1 <- -5674.5359
            C2 <- 6.3925247
            C3 <- -0.009677843
            C4 <- 0.00000062215701
            C5 <- 2.0747825E-09
            C6 <- -9.484024E-13
            C7 <- 4.1635019
            C8 <- -5800.2206
            C9 <- 1.3914993
            C10 <- -0.048640239
            C11 <- 0.000041764768
            C12 <- -0.000000014452093
            C13 <- 6.5459673
            psat <- double(length(tdb))
            psat[tdb <= 0] = exp(C1/tk + C2 + C3*tk + C4*tk**2 + C5*tk**3 + C6*tk**4 + C7*log(tk)) / 1000
            psat[tdb  > 0] = exp(C8/tk + C9 + C10*tk + C11*tk**2 + C12*tk**3 + C13*log(tk)) / 1000
            psat
        }
        # }}}
        dt17 <- dt17[index <= 10L][,
            value := {
                value[7L] <- value[2L]

                # get material density
                mat <- idf$object(.BY$id)$ref_to_object(1L, "Material")[[1L]]
                if (!length(mat)) {
                    abort("error_tran_850_860",
                        paste0(
                            "Material match issue:\n",
                            "Did not find a material component match for name `",
                            idf$object(.BY$id)$name(), "`."
                        )
                    )
                }
                den <- mat$Density

                # calculate
                coeffs <- suppressWarnings(as.double(value[3L:6L]))
                d_empd <- suppressWarnings(as.double(value[2L]))
                value[2L] <- cal_mu_empd(coeffs[1L], coeffs[2L], coeffs[3L], coeffs[4L], d_empd, den)
                value
            },
            by = "id"
        ]
    }
    # }}}
    # 18:EnergyManagementSystem:Actuator {{{
    dt18 <- trans_action(idf, "EnergyManagementSystem:Actuator",
        reset = list(4L, "outdoor air dryblub temperature", "Outdoor Air Drybulb Temperature"),
        reset = list(4L, "outdoor air wetblub temperature", "Outdoor Air Wetbulb Temperature")
    )
    # }}}

    dt <- rbindlist(mget(paste0("dt", 1:18)))
    if (nrow(dt)) new_idf$load(dt, .default = FALSE)

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_860_870 {{{
trans_funs$f860t870 <- function (idf) {
    assert(idf$version()[, 1:2] == 8.6)

    target_cls <- c(
        "Coil:Cooling:DX:MultiSpeed",                # 1
        "Coil:Heating:DX:MultiSpeed",                # 2
        "CoolingTower:SingleSpeed",                  # 3
        "CoolingTower:TwoSpeed",                     # 4
        "CoolingTower:VariableSpeed:Merkel",         # 5
        "AirflowNetwork:SimulationContrl",           # 6
        "ZoneCapacitanceMultiplier:ResearchSpecial", # 7
        "WaterHeater:HeatPump:WrappedCondenser",     # 8
        "AirflowNetwork:Distribution:Component:Duct" # 9
    )

    new_idf <- trans_preprocess(idf, 8.7, target_cls)

    # 1: Coil:Cooling:DX:MultiSpeed {{{
    dt1 <- trans_action(idf, "Coil:Cooling:DX:MultiSpeed",
        reset = list(16, NA_character_, "NaturalGas"),
        reset = list(16, "PropaneGas", "Propane")
    )
    # }}}
    # 2: Coil:Heating:DX:MultiSpeed {{{
    dt2 <- trans_action(idf, "Coil:Heating:DX:MultiSpeed",
        reset = list(16, NA_character_, "NaturalGas"),
        reset = list(16, "PropaneGas", "Propane")
    )
    # }}}
    # 3: CoolingTower:SingleSpeed {{{
    dt3 <- trans_action(idf, "CoolingTower:SingleSpeed", insert = list(17:20))
    # }}}
    # 4: CoolingTower:TwoSpeed {{{
    dt4 <- trans_action(idf, "CoolingTower:TwoSpeed", insert = list(25:28))
    # }}}
    # 5: CoolingTower:VariableSpeed:Merkel {{{
    dt5 <- trans_action(idf, "CoolingTower:VariableSpeed:Merkel", insert = list(25:28))
    # }}}
    # 6: AirflowNetwork:SimulationContrl {{{
    dt6 <- trans_action(idf, "AirflowNetwork:SimulationContrl", delete = list(4L))
    # }}}
    # 7: ZoneCapacitanceMultiplier:ResearchSpecial {{{
    dt7 <- trans_action(idf, "ZoneCapacitanceMultiplier:ResearchSpecial",
        insert = list(1, "Multiplier"),
        insert = list(2)
    )
    # }}}
    # 8: WaterHeater:HeatPump:WrappedCondenser {{{
    dt8 <- trans_action(idf, "WaterHeater:HeatPump:WrappedCondenser",
        reset = list(35, "MutuallyExlcusive", "MutuallyExclusive"))
    # }}}
    # 9: AirflowNetwork:Distribution:Component:Duct {{{
    dt9 <- trans_action(idf, "AirflowNetwork:Distribution:Component:Duct", all = TRUE,
        insert(9:10)
    )
    if (nrow(dt9)) {
        dt9[index >= 7L, value := {
            AFNDuctFracRcond <- 0.815384615
            AFNDuctFracRout <- 0.153846154
            AFNDuctFracRin <- 0.030769231

            val <- suppressWarnings(as.double(value))
            # 7
            value[1L] <- as.character(val[1L] / AFNDuctFracRcond)
            # 9
            value[3L] <- as.character(val[3L] / AFNDuctFracRout)
            # 10
            value[4L] <- as.character(val[4L] / AFNDuctFracRin)

            value
        }, by = "id"]
    }
    # }}}

    dt <- rbindlist(mget(paste0("dt", 1:9)))
    if (nrow(dt)) new_idf$load(dt, .default = FALSE)

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_870_880 {{{
trans_funs$f870t880 <- function (idf) {
    assert(idf$version()[, 1:2] == 8.7)

    target_cls <- c(
        "Output:Surfaces:List",                           # 1
        "Table:TwoIndependentVariables",                  # 2
       # Only used to check corresponding perimeter object
       #"BuildingSurface:Detailed",                       # 3
       # Only used to check corresponding perimeter object
       #"Floor:Detailed",                                 # 4
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
    )

    new_idf <- trans_preprocess(idf, 8.8, target_cls)

    # 1: Output:Surfaces:List {{{
    dt1 <- trans_action(idf, "Output:Surfaces:List",
        reset = list(1, "DecayCurvesfromZoneComponentLoads", "DecayCurvesFromComponentLoadsSummary")
    )
    # }}}
    # 2: Table:TwoIndependentVariables {{{
    dt2 <- trans_action(idf, "Table:TwoIndependentVariables", insert = list(14))
    # }}}
    # 3: BuildingSurface:Detailed {{{
    dt3 <- trans_action(idf, "BuildingSurface:Detailed", min_fields = 5L)
    if (nrow(dt3)) {
        targets <- dt3[index == 5L & tolower(value) == "foundation"] &
                   dt3[index == 2L & tolower(value) == "floor"]
        obj_id <- dt3[index == 1L, id](targets)

        if (!length(obj_id)) {
            dt3 <- data.table()
        } else {
            # check if surface has a corresponding perimeter object
            # if not, give a warning and add one
            dt3 <- rbindlist(lapply(obj_id,
                function (i) {
                    perim <- idf$object(i)$ref_by_object(1L, "SurfaceProperty:ExposedFoundationPerimeter")[[1L]]

                    if (!length(perim)) {
                        dt <- dt3[J(i), on = "index"]
                        n <- ceiling((nrow(dt) - 10L) / 3L)
                        dt <- dt[index <= 4L + n]
                        dt[J(2L), on = "index", value := "BySegment"]
                        dt[J(c(3L, 4L)), on = "index", value := NA_character_]
                        dt[index > 4L, value := "Yes"]
                        warn("warn_trans_870_880",
                            paste0(
                                "Foundation floors now require a ",
                                "`SurfaceProperty:ExposedFoundationPerimeter` ",
                                "object. One was added with each segment of the ",
                                "floor surface exposed (`",idf$object(i)$name(),"`). ",
                                "Please check your inputs to make sure this ",
                                "reflects your foundation."
                            )
                        )
                    } else {
                        data.table()
                    }
                }
            ))
        }
    }
    # }}}
    # 4: Floor:Detailed {{{
    dt4 <- trans_action(idf, "Floor:Detailed", min_fields = 4L)
    if (nrow(dt4)) {
        targets <- dt4[index == 4L & tolower(value) == "foundation"]
        obj_id <- dt4[index == 1L, id](targets)

        if (!length(obj_id)) {
            dt4 <- data.table()
        } else {
            # check if surface has a corresponding perimeter object
            # if not, give a warning and add one
            dt4 <- rbindlist(lapply(obj_id,
                function (i) {
                    perim <- idf$object(i)$ref_by_object(1L, "SurfaceProperty:ExposedFoundationPerimeter")[[1L]]

                    if (!length(perim)) {
                        dt <- dt4[J(i), on = "index"]
                        n <- ceiling((nrow(dt) - 9L) / 3L)
                        dt <- dt[index <= 4L + n]
                        dt[J(2L), on = "index", value := "BySegment"]
                        dt[J(c(3L, 4L)), on = "index", value := NA_character_]
                        dt[index > 4L, value := "Yes"]
                        warn("warn_trans_870_880",
                            paste0(
                                "Foundation floors now require a ",
                                "`SurfaceProperty:ExposedFoundationPerimeter` ",
                                "object. One was added with each segment of the ",
                                "floor surface exposed (`",idf$object(i)$name(),"`). ",
                                "Please check your inputs to make sure this ",
                                "reflects your foundation."
                            )
                        )
                    } else {
                        data.table()
                    }
                }
            ))
        }
    }
    # }}}
    # 5: SurfaceProperty:ExposedFoundationPerimeter {{{
    dt5 <- trans_action(idf, "SurfaceProperty:ExposedFoundationPerimeter",
        insert = list(2L, "BySegment")
    )
    # }}}
    # 6: Foundation:Kiva:Settings {{{
    dt6 <- trans_action(idf, "Foundation:Kiva:Settings",
        reset = list(8L, "autocalculate", "autoselect")
    )
    # }}}
    # 7: UnitarySystemPerformance:MultiSpeed {{{
    dt7 <- trans_action(idf, "UnitarySystemPerformance:MultiSpeed", insert = list(5))
    # }}}
    # 8: Coil:Cooling:DX:SingleSpeed {{{
    dt8 <- trans_action(idf, "Coil:Cooling:DX:SingleSpeed", insert = list(15))
    # }}}
    # 9: Coil:Cooling:DX:TwoSpeed {{{
    dt9 <- trans_action(idf, "Coil:Cooling:DX:TwoSpeed", insert = list(23))
    # }}}
    # 10: Coil:Cooling:DX:MultiSpeed {{{
    dt10 <- trans_action(idf, "Coil:Cooling:DX:MultiSpeed", insert = list(7))
    # }}}
    # 11: Coil:Cooling:DX:VariableSpeed {{{
    dt11 <- trans_action(idf, "Coil:Cooling:DX:VariableSpeed", insert = list(16))
    # }}}
    # 12: Coil:Cooling:DX:TwoStageWithHumidityControlMode {{{
    dt12 <- trans_action(idf, "Coil:Cooling:DX:TwoStageWithHumidityControlMode", insert = list(19))
    # }}}
    # 13: ZoneHVAC:PackagedTerminalHeatPump {{{
    dt13 <- trans_action(idf, "ZoneHVAC:PackagedTerminalHeatPump", delete = list(18))
    # }}}
    # 14: ZoneHVAC:IdealLoadsAirSystem {{{
    dt14 <- trans_action(idf, "ZoneHVAC:IdealLoadsAirSystem", insert = list(5))
    # }}}
    # 15: ZoneControl:ContaminantController{{{
    dt15 <- trans_action(idf, "ZoneControl:ContaminantController", insert = list(6))
    # }}}
    # 16: AvailabilityManager:NightCycle {{{
    dt16 <- trans_action(idf, "AvailabilityManager:NightCycle", insert = list(6, "FixedRunTime"))
    # }}}

    dt <- rbindlist(mget(paste0("dt", 1:16)))
    if (nrow(dt)) new_idf$load(dt, .default = FALSE)

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_880_890 {{{
trans_funs$f880t890 <- function (idf) {
    assert(idf$version()[, 1:2] == 8.8)

    target_cls <- c(
        "ZoneHVAC:EquipmentList",          # 1
        "GroundHeatExchanger:Vertical",    # 2
        "Branch",                          # 3
        "CondenserEquipmentList",          # 4
        "ElectricEquipment:ITE:AirCooled", # 5
        "Schedule:Day:Interval",           # 6
        "Schedule:Day:List",               # 7
        "Schedule:Compact"                 # 8
    )

    new_idf <- trans_preprocess(idf, 8.9, target_cls)

    # 1: ZoneHVAC:EquipmentList {{{
    dt1 <- trans_action(idf, "ZoneHVAC:EquipmentList", insert = list(2, "SequentialLoad"))
    # }}}
    # 2: GroundHeatExchanger:Vertical {{{
    # GroundHeatExchanger:System {{{
    dt2_1 <- trans_action(idf, min_fields = 9,
        class = c("GroundHeatExchanger:System" = "GroundHeatExchanger:Vertical"),
        offset = list(8:9, 7:8),
        insert = list(5, "Site:GroundTemperature:Undisturbed:KusudaAchenbach"),
        insert = list(6)
    )
    if (nrow(dt2_1)) {
        dt2_1 <- dt2_1[index <= 9L]
        dt2_1[, value := {
            name <- if (is.na(value[1L])) "" else value[1L]
            value[6L] <- paste(value[1L], "Ground Temps")
            value[9L] <- paste(value[1L], "Response Factors")
            value
        }, by = "id"]
    }
    # }}}
    # GroundHeatExchanger:Vertical:Properties {{{
    dt2_2 <- trans_action(idf, min_fields = 15L,
        class = c("GroundHeatExchanger:Vertical:Properties" = "GroundHeatExchanger:Vertical"),
        offset = list(c(6L, 7L, 11L, 12L, 13L, 15L, 14L),
                      c(3L, 4L,  5L,  7L,  9L, 10L, 11L)
        ),
        reset = list(2L, "1"),
        reset = list(6L, "3.90e+6"),
        reset = list(8L, "1.77e+6")
    )
    if (nrow(dt2_2)) {
        dt2_2 <- dt2_2[index <= 11L]
        dt2_2[J(4L), on = "index", value := as.character(as.double(value) * 2)]
    }
    # }}}
    # Site:GroundTemperature:Undisturbed:KusudaAchenbach {{{
    dt2_3 <- trans_action(idf, min_fields = 10L,
        class = c("Site:GroundTemperature:Undisturbed:KusudaAchenbach" = "GroundHeatExchanger:Vertical"),
        offset = list(c(8L, 9L, 10L),
                      c(2L, 4L,  5L)
        ),
        reset = list(3L, "920"),
        reset = list(6L, "3.2"),
        reset = list(7L, "8")
    )
    if (nrow(dt2_3)) {
        dt2_3 <- dt2_3[index <= 7L]
        dt2_3[J(4L), on = "index", value := {
            val <- suppressWarnings(as.double(value))
            val[is.na(val)] <- 0.0
            val <- val / 920
            as.character(val)
        }, by = "id"]
    }
    # }}}
    # GroundHeatExchanger:ResponseFactors {{{
    dt2_4 <- trans_action(idf, min_fields = 17L,
        class = c("GroundHeatExchanger:ResponseFactors" = "GroundHeatExchanger:Vertical"),
        offset = list(c(5L, 17L),
                      c(3L,  4L)
        ),
        delete = list(c(6:18))
    )
    if (nrow(dt2_4)) {
        dt2_4[index >= 19L, index := index - 14L]
    }
    # }}}
    dt2 <- rbindlist(list(dt2_1, dt2_2, dt2_3, dt2_4))
    # }}}
    # 3: Branch {{{
    dt3 <- trans_action(idf, "Branch")
    if (nrow(dt3)) {
        dt3[(index - 2L) %% 4L == 1, value := gsub("GroundHeatExchanger:Vertical", "GroundHeatExchanger:System", value, ignore.case = TRUE)]
    }
    # }}}
    # 4: CondenserEquipmentList {{{
    dt4 <- trans_action(idf, "CondenserEquipmentList", delete = list(7L))
    if (nrow(dt4)) {
        dt4[(index - 2L) %% 2L == 1, value := gsub("GroundHeatExchanger:Vertical", "GroundHeatExchanger:System", value, ignore.case = TRUE)]
    }
    # }}}
    # 5: ElectricEquipment:ITE:AirCooled {{{
    dt5 <- trans_action(idf, "ElectricEquipment:ITE:AirCooled", insert = list(3L, "FlowFromSystem"))
    # }}}
    # 6: Schedule:Day:Interval {{{
    dt6 <- trans_action(idf, "Schedule:Day:Interval", reset = list(6L, "yes", "Average"))
    # }}}
    # 7: Schedule:Day:List {{{
    dt7 <- trans_action(idf, "Schedule:Day:List", reset = list(6L, "yes", "Average"))
    # }}}
    # 8: Schedule:Compact {{{
    dt8 <- trans_action(idf, "Schedule:Compact")
    if (nrow(dt8)) {
        dt8[index >= 3L & grep("interpolate.+average", value, ignore.case = TRUE), value := "Interpolate:Average"]
    }
    # }}}

    dt <- rbindlist(mget(paste0("dt", 1:8)))
    if (nrow(dt)) new_idf$load(dt, .default = FALSE)

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_890_900 {{{
trans_funs$f890t900 <- function (idf) {
    assert(idf$version()[, 1:2] == 8.9)

    target_cls <- c(
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
    )

    new_idf <- trans_preprocess(idf, 9.0, target_cls)

    # 1: AirflowNetwork:Distribution:Component:OutdoorAirFlow {{{
    dt1 <- trans_action(idf,
        class = "AirflowNetwork:Distribution:Component:OutdoorAirFlow",
        insert = list(2,
            tryCatch(idf$object_name("OutdoorAir:Mixer", simplify = TRUE)[[1L]],
                error_class_name = function (e) NA_character_
            )
        )
    )
    if (nrow(dt1) && idf$object_num("OutdoorAir:Mixer") > 1L) {
        warn("Multiple `OutdoorAir:Mixer` object found.")
    }
    # }}}
    # 2: AirflowNetwork:Distribution:Component:ReliefAirFlow {{{
    dt2 <- trans_action(idf, "AirflowNetwork:Distribution:Component:ReliefAirFlow",
        insert = list(2,
            tryCatch(idf$object_name("OutdoorAir:Mixer", simplify = TRUE)[[1L]],
                error_class_name = function (e) NA_character_
            )
        )
    )
    if (nrow(dt2) && idf$object_num("OutdoorAir:Mixer") > 1L) {
        warn("Multiple `OutdoorAir:Mixer` object found.")
    }
    # }}}
    # 3: Boiler:HotWater {{{
    dt3 <- trans_action(idf, "Boiler:HotWater", delete = list(7L))
    # }}}
    # 4: FenestrationSurface:Detailed {{{
    dt4 <- trans_action(idf, "FenestrationSurface:Detailed", delete = list(7L))
    # }}}
    # 5: GlazedDoor {{{
    dt5 <- trans_action(idf, "GlazedDoor", delete = list(4L))
    # }}}
    # 6: RunPeriod:CustomRange {{{
    dt6 <- trans_action(idf, c("RunPeriod" = "RunPeriod:CustomRange"))
    if (nrow(dt6)) {
        dt6[index == 8L, {
            if (any(tolower(value) == "useweatherfile")) {
                warn("warn_trans_890_900",
                    paste0("Run period start day of week `UseWeatherFile` option ",
                        "has been removed, start week day is set by the input start date."
                    )
                )
            }
        }]
    }
    # }}}
    # 7: RunPeriod {{{
    dt7 <- trans_action(idf, "RunPeriod", all = TRUE,
        insert = list(c(`Begin Year` = 4L)),
        insert = list(c(`End Year` = 7L))
    )

    # calculate start year and do some checkings
    if (nrow(dt7)) {
        dt7[, value :=
            {
                start_year <- value[14L + 2L]
                num_rep <- value[12L + 2L]
                if (!is.na(start_year)) {
                    assert(is_strint(start_year), prefix = "`Start Year`")
                    value[4L] <- start_year

                    # remove Day of Week for Start Day
                    value[8L] <- NA_character_
                } else if (!is.na(num_rep)) {
                    assert(is_strint(num_rep), prefix = "`Number of Times Runperiod to be Repeated`")
                    # in case of leap year
                    start_date <- lubridate::make_date(year = 2018L, month = value[2L], day = value[3L])
                    if (is.na(start_date)) {
                        abort("error_trans_890_900", msg = paste0(
                            "Invalid `Start Month` or `Start Day of Month` in `RunPeriod` found."
                        ))
                    }

                    weekday <- if (is.na(value[8L])) "Sunday" else value[8L]
                    start_year <- find_nearst_wday_year(start_date, weekday, 2017,
                        start_date == lubridate::as_date("2018-02-29")
                    )
                    value[4L] <- as.character(start_year)

                    # end year
                    end_year <- start_year + as.integer(num_rep)
                    value[7L] <- as.character(end_year)

                    assert(is_strint(value[5L]), prefix = "End Month")
                    assert(is_strint(value[6L]), prefix = "End Day of Month")
                    # check if leap day of end date is specified in an non-leap year
                    if ((!leap_year(end_year)) && as.integer(value[5L]) == 2L && as.integer(value[6L]) == 29L) {
                        warn("error_trans_890_900", msg = paste0(
                            "With `Number of Times Runperiod to be Repeated` being ", num_rep,
                            "the end year will be ", end_year, ", which is not a leap year. ",
                            "The end date will be reset to Feb 28th."
                        ))
                        value[6L] <- "28"
                    }
                }

                if (!is.na(value[8L]) && tolower(value[8L]) == "useweatherfile") {
                    warn("warn_trans_890_900",
                        paste0("Run period start day of week `UseWeatherFile` option ",
                            "has been removed, start week day is set by the input start date."
                        )
                    )
                    value[8L] <- NA_character_
                }

                value
            },
            by = "id"
        ]
        dt7 <- dt7[!J(c(15L:16L)), on = "index"][J(14L), on = "index", value := NA_character_]
    }
    # }}}
    # 8: Table:OneIndependentVariable {{{
    dt8 <- trans_action(idf, "Table:OneIndependentVariable", reset = list(2L, "exponent", NA_character_))
    # }}}
    # 9: WindowMaterial:ComplexShade {{{
    dt9 <- trans_action(idf, "WindowMaterial:ComplexShade",
        reset = list(2L, "venetian", "VenetianHorizontal")
    )
    # }}}
    # 10: Window {{{
    dt10 <- trans_action(idf, "Window", delete = list(4L))
    # }}}
    # 11: WindowProperty:ShadingControl {{{
    if (!idf$is_valid_class("WindowProperty:ShadingControl")) {
        dt11 <- data.table()
    } else {
        # get all old shading control objects
        shadctrl <- idf$objects_in_class("WindowProperty:ShadingControl")

        # get zone that is being controled {{{
        fene_daylight_zone <- lapply(shadctrl,
            function (ctrl) {
                # get all fenestrations that uses this control
                fene <- ctrl$ref_by_object("Name",
                    class = c("FenestrationSurface:Detailed", "Window", "GlazedDoor")
                )

                # get zone name this fenestration belongs to
                zone <- vcapply(fene, function (fenestration) {
                    fenestration$ref_to_object(
                        "Building Surface Name",
                        class = "Zone",
                        recursive = TRUE, depth = NULL
                    )[[1L]]$name()
                }, use.names = FALSE)
                assert(length(unique(tolower(zone))) == 1L)
                zone <- zone[[1L]]

                # get daylighting control of this zone if exists
                daylgt <- idf$object(zone)$ref_by_object("Name", class = "Daylighting:Controls")
                daylgt <- if (length(daylgt)) daylgh[[1L]]$name() else NA_character_

                list(fenestration = names(fene), zone = zone, daylighting = daylgt)
            }
        )
        # }}}

        dt11 <- trans_action(idf, all = TRUE,
            class = c("WindowShadingControl" = "WindowProperty:ShadingControl"),
            insert = list(c(`Zone Name` = 2L, `Shading Control Sequence Number` = 3L)),
            add = list(c(`Daylighting Control Object Name` = 15L,
                         `Multiple Surface Control Type` = 16L
            ))
        )

        # update name with zone name suffix
        setindexv(dt11, "index")
        dt11[J(1L), on = "index", value := paste(value, "-", vcapply(fene_daylight_zone, "[[", "zone"))]
        # update zone name
        dt11[J(2L), on = "index", value := vcapply(fene_daylight_zone, "[[", "zone")]
        # update control sequence
        dt11[J(3L), on = "index", value := as.character(seq_along(fene_daylight_zone))]
        # update multiple surface control type name
        dt11[, value := {
            if (tolower(value[[4L]]) == "switchableglazing" &&
                tolower(value[[6L]]) == "meetdaylightilluminancesetpoint"
            ) {
                value[[16]] <- "Group"
            } else {
                value[[16]] <- "Sequential"
            }
            value
        }, by = "id"]
        # update daylighting control name
        dt11[J(15L), on = "index", value := vcapply(fene_daylight_zone, "[[", "daylighting")]
        # add fenestration surface names
        fene <- lapply(fene_daylight_zone, "[[", "fenestration")
        # extract rows
        ka_num <- each_length(fene)
        new_fene <- data.table(id = rep(unique(dt11$id), ka_num), name = rep(names(fene), ka_num),
            class = "WindowShadingControl",
            index = unlist(lapply(ka_num, seq_len)) + 16L,
            field = NA_character_, value = unlist(fene)
        )
        dt11 <- rbindlist(list(dt11, new_fene), use.names = TRUE)
        setorderv(dt11, c("id", "index"))
    }
    # }}}

    dt <- rbindlist(mget(paste0("dt", 1:11)))
    if (nrow(dt)) new_idf$load(dt, .default = FALSE)

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_900_910 {{{
trans_funs$f900t910 <- function (idf) {
    assert(idf$version()[, 1:2] == 9.0)

    target_cls <- c(
        "HybridModel:Zone",      # 1
        "ZoneHVAC:EquipmentList" # 2
    )

    new_idf <- trans_preprocess(idf, 9.1, target_cls)

    # 1: HybridModel:Zone {{{
    dt1 <- trans_action(idf, "HybridModel:Zone", min_fields = 9L,
        # Old [6:9] --> New [17:20]
        offset = list(6:9, 17:20),
        # Old NA    --> New [7:16]
        add = list(7:16),
        # Old [5]   --> New [6]
        offset = list(5, 6),
        # Old NA    --> New [5]
        add = list(5, "No")
    )
    # }}}
    # 2: ZoneHVAC:EquipmentList {{{
    dt2 <- trans_action(idf, "ZoneHVAC:EquipmentList", min_fields = 6L,
        # Old NA    --> New [7:8] with step 4
        insert = list(7:8, NA, 4)
    )
    # }}}

    dt <- rbindlist(mget(paste0("dt", 1:2)))
    if (nrow(dt)) new_idf$load(dt, .default = FALSE)

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}

# trans_preprocess {{{
# 1. delete objects in deprecated class
# 2. delete all old objects
# 3. update IDD data
# 4. update version
# 5. assign new uuid
trans_preprocess <- function (idf, version, class) {
    # clone old IDF
    new_idf <- idf$clone(deep = TRUE)
    # get new IDD
    new_idd <- use_idd(version, "auto")

    # delete deprecated classes
    class_del <- setdiff(use_idd(idf$version())$class_name(), new_idd$class_name())

    # get all classes to be deleted
    class <- c(CLASS_DEL_COMMON, class_del, class)

    # del all related class
    class <- class[idf$is_valid_class(class)]

    if (length(class)) {
        new_idf$del(new_idf$object_id(class, simplify = TRUE), .force = TRUE)
    }

    priv <- ._get_private(new_idf)

    # use old class name in object and value table
    add_joined_cols(priv$idd_env()$class, priv$idf_env()$object, "class_id", "class_name")
    add_joined_cols(priv$idf_env()$object, priv$idf_env()$value, "object_id", "class_name")

    # add field index in value table
    set(priv$idf_env()$value, NULL, "field_index", rowidv(priv$idf_env()$value, "object_id"))

    # update IDD
    priv$m_idd <- new_idd

    # update class id in object table
    add_joined_cols(priv$idd_env()$class, priv$idf_env()$object, "class_name", "class_id")
    set(priv$idf_env()$object, NULL, "class_name", NULL)

    # add class names in field table
    add_joined_cols(priv$idd_env()$class, priv$idd_env()$field, "class_id", "class_name")
    # update field id in value table
    set(priv$idf_env()$value, NULL, "field_id",
        priv$idd_env()$field[priv$idf_env()$value, on = c("class_name", "field_index"), "field_id"]
    )
    set(priv$idf_env()$value, NULL, c("class_name", "field_index"), NULL)

    # check if there are newly added extensible groups
    if (anyNA(priv$idf_env()$value$field_id)) {
        stop()
        add_idd_extensible_group()
    }

    # update version
    priv$m_version <- priv$m_idd$version()
    priv$idf_env()$value[field_id == 1L, `:=`(
        value_chr = as.character(priv$m_version[, 1:2]),
        value_num = NA_real_
    )]

    # remove path
    priv$m_path <- NULL

    # reset log
    priv$m_log$unsaved <- TRUE
    priv$m_log$uuid <- unique_id()
    priv$m_log$job <- NULL

    new_idf
}
# }}}
# trans_postprocess {{{
# 1. If first argument in Output:Variable, Output:Table:TimeBins and others is
#    blank, replace it with "*".
# 2. Update variable names if necessary
# 3. Delete the whole object if corresponding output variable has been removed.
trans_postprocess <- function (idf, from, to) {
    # reset_key {{{
    reset_key <- function (dt, field_index = 1L) {
        if (!nrow(dt)) return(dt)
        dt[J(field_index, NA_character_), on = c("index", "value"), value := "*"]
    }
    # }}}
    # update_var {{{
    update_var <- function (dt, mapping, field_index, step = NULL) {
        if (!nrow(dt)) return(dt)
        if (!nrow(mapping)) return(dt)

        if (!has_name(dt, "value_lower")) {
            set(dt, NULL, "value_lower", stri_trans_tolower(dt$value))
        }

        new <- NULL # eliminate check warning of no visible binding
        # delete deprecatd variable first
        dt <- dt[!mapping[is.na(new)], on = c(value_lower = "old")]

        if (nrow(dt)) {
            if (!is.null(step)) {
                n <- (nrow(dt) - field_index) %/% step
                field_index <- as.integer(c(field_index, seq.int(n) * step + field_index))
            }

            # update variable names
            dt[J(field_index), on = "index", value := {
                mapping[!is.na(new)][J(value_lower), on = "old", {
                    updated <- new
                    updated[is.na(new)] <- value[is.na(new)]
                    updated
                }]
            }]
        }

        set(dt, NULL, "value_lower", NULL)
    }
    # }}}

    f <- as.double(as.character(standardize_ver(from)[, 1:2]))
    t <- as.double(as.character(standardize_ver(to)[, 1:2]))
    rep_vars <- REPORTVAR_RULES[J(f, t), on = c("from", "to")]
    if (nrow(rep_vars)) set(rep_vars, NULL, "old", stri_trans_tolower(rep_vars$old))

    # Output:Variable {{{
    dt1 <- trans_action(idf, "Output:Variable")
    dt1 <- reset_key(dt1, 1L)
    dt1 <- update_var(dt1, rep_vars, 2L)
    # }}}
    # Output:Meter:* {{{
    dt2 <- rbindlist(lapply(
        c("Output:Meter",
          "Output:Meter:MeterFileOnly",
          "Output:Meter:Cumulative",
          "Output:Meter:Cumulative:MeterFileOnly"
        ),
        trans_action, idf = idf
    ))
    dt2 <- update_var(dt2, rep_vars, 1L)
    # }}}
    # Output:Table:TimeBins {{{
    dt3 <- trans_action(idf, "Output:Table:TimeBins")
    dt3 <- reset_key(dt3, 1L)
    dt3 <- update_var(dt3, rep_vars, 2L)
    # }}}
    # ExternalInterface:FunctionalMockupUnitImport:From:Variable & ExternalInterface:FunctionalMockupUnitExport:From:Variable {{{
    dt3_1 <- trans_action(idf, "ExternalInterface:FunctionalMockupUnitImport:From:Variable")
    dt3_2 <- trans_action(idf, "ExternalInterface:FunctionalMockupUnitExport:From:Variable")
    dt3 <- rbindlist(list(dt3_1, dt3_2))
    dt3 <- reset_key(dt3, 1L)
    dt3 <- update_var(dt3, rep_vars, 2L)
    # }}}
    # EnergyManagementSystem:Sensor {{{
    dt4 <- trans_action(idf, "EnergyManagementSystem:Sensor")
    dt4 <- reset_key(dt4, 1L)
    dt4 <- update_var(dt4, rep_vars, 3L)
    # }}}
    # Output:Table:Monthly {{{
    dt5 <- trans_action(idf, "Output:Table:Monthly")
    dt5 <- update_var(dt4, rep_vars, 3L, step = 2L)
    # }}}
    # Meter:Custom {{{
    dt6 <- trans_action(idf, "Meter:Custom")
    dt6 <- update_var(dt4, rep_vars, 4L, step = 2L)
    # }}}
    # Meter:CustomDecrement {{{
    dt7 <- trans_action(idf, "Meter:CustomDecrement")
    dt7 <- update_var(dt4, rep_vars, 3L, 2L)
    # }}}

    dt <- rbindlist(mget(paste0("dt", 1:7)))
    if (nrow(dt)) idf$update(dt, .default = FALSE)

    idf
}
# }}}

# trans_action {{{
trans_action <- function (idf, class, min_fields = 1L, all = FALSE, ...) {
    assert(idf$is_valid_class(class, all = TRUE))
    if (!idf$is_valid_class(class)) return(data.table())

    dt <- idf$to_table(class = class, align = TRUE, all = all)
    # make sure min fields are returned
    if (!all && min_fields > max(dt$index)) {
        cur_max <- max(dt$index)
        num_obj <- length(unique(dt$id))
        miss <- dt[rep(1L, min_fields - cur_max), on = "index", allow.cartesian = TRUE][
            , index := seq(cur_max + 1L, min_fields), by = "id"]
        set(miss, NULL, c("field", "value"), NA_character_)
        dt <- rbindlist(list(dt, miss))
    }

    setindexv(dt, c("index"))
    setindexv(dt, c("id"))
    setindexv(dt, c("id", "index"))
    if (is_named(class)) set(dt, NULL, "class", names(class))

    act <- list(...)
    if (!length(act)) return(dt)

    new <- data.table()

    for (i in seq_along(act)) {
        action <- names(act)[[i]]
        content <- act[[i]]

        new1 <- data.table()
        if (action == "reset") {
            if (length(content) == 2L) {
                dt[J(content[[1L]]), on = "index", `:=`(value = content[[2L]])]
            } else if (length(content) >= 3L) {
                set(dt, NULL, "value_lower", stri_trans_tolower(dt$value))
                dt[J(content[[1L]], stri_trans_tolower(content[[2L]])), on = c("index", "value_lower"),
                    `:=`(value = content[[3L]])
                ]
                set(dt, NULL, "value_lower", NULL)
            }
        } else if (action == "offset") {
            # delete original first
            dt <- dt[!J(setdiff(content[[2L]], content[[1L]])), on = "index"]
            dt[J(content[[1L]]), on = "index", `:=`(index = content[[2L]]), by = "id"]
        } else if (action == "add") {
            # if no new value is given, assign NA
            if (length(content) == 1L) content[[2L]] <- rep(NA_character_, length(content[[1L]]))
            content[[2L]] <- rep(content[[2L]], length.out = length(content[[1L]]))

            new1 <- dt[J(rep(1L, length(content[[1L]]))), on = "index", allow.cartesian = TRUE][
                , `:=`(index = content[[1L]], field = names2(content[[1L]]), value = content[[2L]]), by = "id"
            ]
        } else if (action == "insert") {
            # if no new value is given, assign NA
            if (length(content) == 1L) content[[2]] <- rep(NA_character_, length(content[[1L]]))
            # if no step value is given, assgin zero
            if (length(content) == 2L) content[[3L]] <- 0L

            # calculate new index of group rows to insert
            num_grp <- dt[index >= content[[1L]][[1L]],
                {
                    if (content[[3L]] == 0L) {
                        num <- 1L
                    } else {
                        num <- .N / content[[3L]] + 1L
                        assert(is_integer(num), msg = "Invalid step for row insertion.")
                    }
                    index <- content[[1L]] + rep((length(content[[1L]]) + content[[3L]]) * (seq_len(num) - 1L), each = length(content[[1L]]))
                    value <- rep(rep(content[[2L]], length.out = length(content[[1L]])), num)
                    field <- rep(rep(names2(content[[1L]]), length.out = length(content[[1L]])), num)

                    list(index = 1L, new_index = as.integer(index),
                        new_value = value, new_field = field
                    )
                },
                by = "id"
            ]

            # extract group of groups
            new1 <- dt[num_grp, on = c("id", "index")]
            set(new1, NULL, c("index", "value", "field"), NULL)
            setnames(new1, c("new_index", "new_field", "new_value"), c("index", "field", "value"))

            # update index of field left
            dt[index >= content[[1L]][[1L]], index := {
                if (content[[3L]] == 0L) {
                    num <- 1L
                } else {
                    num <- .N / content[[3L]]
                }

                as.integer(index + length(content[[1L]]) * seq_len(num))

            }, by = "id"]
        } else if (action == "delete") {
            if (length(content) < 2L) {
                dt <- dt[!J(content[[1L]]), on = "index"]
            } else {
                dt <- dt[(index - content[[2L]]) %% content[[1L]] != 0L]
            }
        }

        new <- rbindlist(list(new, new1), use.names = TRUE)
    }

    new_dt <- rbindlist(list(dt, new), use.names = TRUE)
    setindexv(new_dt, c("index"))
    setindexv(new_dt, c("id"))
    setindexv(new_dt, c("id", "index"))
    setorderv(new_dt, c("id", "index"))
    new_dt
}
# }}}
# trans_upper_versions {{{
trans_upper_versions <- function (idf, ver) {
    # get all versions needed to handle
    all_vers <- standardize_ver(ALL_IDD_VER)
    all_vers[all_vers >= idf$version() & all_vers <= standardize_ver(ver)]
}
# }}}
# trans_fun_names {{{
trans_fun_names <- function (vers) {
    # get corresponding transition function names
    vers <- gsub(".", "", as.character(vers), fixed = TRUE)
    vers[vers == "901"] <- "900"
    vers <- unique(vers)
    funs <- paste0("f", vers[-length(vers)], "t", vers[-1L])
    # only include transition functions available
    funs[funs %in% names(trans_funs)]
}
# }}}
# with_option {{{
with_option <- function (opts, expr) {
    # get options
    ori <- eplusr_option()

    if (!is.list(opts) || is.null(names(opts))) {
        stop("`opts` should be a named list.")
    }

    if (any(!names(opts) %in% names(ori))) {
        stop("Invalid eplusr option found: ", sQuote(names(opts)[!names(opts) %in% names(ori)]))
    }

    # set new option values
    on.exit(do.call(eplusr_option, ori), add = TRUE)
    do.call(eplusr_option, opts)

    force(expr)
}
# }}}
