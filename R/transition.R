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

#' Perform version transition of EnergyPlus model
#'
#' `transition()` takes an [Idf] object or a path of IDF file and a target
#' version, performs version transitions and returns an [Idf] object of
#' specified version.
#'
#' @param idf An [Idf] object or a path of IDF file.
#' @param ver A valid EnergyPlus IDD version, e.g. `9`, `8.8`, or `"8.8.0"`.
#' @param save If `TRUE`, the models will be saved into specified directory.
#' Default: `FALSE`.
#' @param dir Only applicable when `save` is `TRUE`. The directory to save the
#' new IDF files. If the directory does not exist, it will be created before
#' save. If `NULL`, the directory of input [Idf] object or IDF file will be
#' used. Default: `NULL`.
#' @param keep_all If `TRUE`, a list will be return which contains all
#' [Idf] objects of intermediate versions. The list will be named using first
#' two number of that version, e.g. `8.1`, `8.2`. If `FALSE`, only the [Idf]
#' object of the version specified by `ver` will be returned. Default: `FALSE`.
#' @return An [Idf] object if `keep_all` is `FALSE` or a list of [Idf] objects
#' if `keep_all` is `TRUE`.
#' @seealso See [version_updater()] which directly call EnergyPlus preprocessor
#' `IDFVersionUpdater` to perform the version transitions.
#' @author Hongyuan Jia
#' @examples
#' \dontrun{
#' if (any(avail_eplus()) > 7.2) {
#'     # create an empty IDF
#'     idf <- empty_idf(7.2)
#'
#'     # convert it from v7.2 to the latest EnergyPlus installed
#'     transition(idf, max(avail_eplus()))
#'
#'     # convert it from v7.2 to the latest EnergyPlus installed and keep all
#'     # intermediate versions
#'     transition(idf, max(avail_eplus()), keep_all = TRUE)
#'
#'     # convert it from v7.2 to the latest EnergyPlus installed and keep all
#'     # intermediate versions and save all them
#'     idf$save(tempfile(fileext = ".idf"))
#'     transition(idf, max(avail_eplus()), keep_all = TRUE,
#'         save = TRUE, dir = tempdir()
#'     )
#' }
#' }
#' @export
# transition {{{
#' @importFrom checkmate assert_vector
# TODO: how to give the names of saved files
transition <- function (idf, ver, keep_all = FALSE, save = FALSE, dir = NULL) {
    if (!is_idf(idf)) idf <- read_idf(idf)

    if (length(ver) != 1L || is.na(ver <- convert_to_idd_ver(ver))) {
        abort("'ver' must be a valid EnergyPlus IDD version")
    }
    ver <- ver[, 1:2]

    if (idf$version() < 7.2) {
        abort(paste0("Input IDF has version ", surround(idf$version()), ". ",
            "Currently only EnergyPlus v7.2 and above are suppored."
        ))
    }

    # only compare Major and Mversioninor version, skip patch version
    if (idf$version()[, 1L:2L] == ver) {
        verbose_info("IDF is already at latest version ", ver, ". No transition is needed.")
        if (keep_all) {
            res <- list(idf)
            setattr(res, "names", as.character(idf$version()[, 1L:2L]))
            return(res)
        } else {
            return(idf)
        }
    # cannot go reverse
    } else if (idf$version()[, 1L:2L] > ver) {
        abort("Only version updating is supported. Downgrading is not supported.")
    }

    # stop if unsaved
    if (idf$is_unsaved()) {
        abort("Idf has been modified since read or last saved. Please save Idf using '$save()' before transition.")
    }

    # clone original input
    idf <- idf$clone(TRUE)

    # perform transition
    res <- trans_apply(idf, ver, keep_all)

    # directly return if no saving is required
    if (!save) return(res)

    # check if original file exists
    if (is.null(idf$path())) {
        abort("The Idf object is not created from local file. Please save Idf using '$save()' before transition.", "idf_not_local")
    }

    if (is.null(dir)) {
        dir <- dirname(idf$path())
    } else if (!dir.exists(dir)){
        dir.create(dir, recursive = TRUE)
    }

    save_new <- function (idf, dir, path) {
        nm <- paste0(tools::file_path_sans_ext(basename(path)), "V", idf$version()[, 1L], idf$version()[, 2L], "0.idf")
        idf$save(file.path(dir, nm), overwrite = TRUE)
    }

    if (!keep_all) {
        save_new(res, dir, path = idf$path())
    } else {
        lapply(res, save_new, dir = dir, path = idf$path())
    }

    res
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
        for (i in seq_along(funs))  {
            verbose_info(
                " From  Ver: ", vers[i], "\n",
                "Toward Ver: ", vers[i + 1L]
            )
            idf <- with_speed(trans_funs[[funs[i]]](idf))
            verbose_info("[", vers[i], " --> ", vers[i + 1L], "] SUCCEEDED.\n")
        }
        idf
    } else {
        res <- vector("list", length(funs) + 1L)
        res[[1L]] <- idf

        for (i in seq_along(res)) {
            if (i == length(res)) break
            verbose_info(
                " From  Ver: ", vers[i], "\n",
                "Toward Ver: ", vers[i + 1L]
            )
            res[[i + 1L]] <- with_speed(trans_funs[[funs[[i]]]](res[[i]]))
            verbose_info("[", vers[i], " --> ", vers[i + 1L], "] SUCCEEDED.\n")
        }
        nms <- paste0(stri_sub(funs, 6L, 6L), ".", stri_sub(funs, 7L, 7L))
        setattr(res, "names", c(as.character(idf$version()[, 1L:2L]), nms))

        res
    }
}
# }}}

trans_funs <- new.env(parent = emptyenv())
# trans_720_800 {{{
#' @importFrom checkmate assert_true
trans_funs$f720t800 <- function (idf) {
    assert_true(idf$version()[, 1:2] == 7.2)

    target_cls <- c(
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
    )

    new_idf <- trans_preprocess(idf, 8.0, target_cls)

    # 1: ShadowCalculation {{{
    dt1 <- trans_action(idf, "ShadowCalculation", insert = list(1L, "AverageOverDaysInFrequency"))
    # }}}
    # 2: Coil:Heating:DX:MultiSpeed {{{
    dt2 <- trans_action(idf, class = "Coil:Heating:DX:MultiSpeed",
        insert = list(6L),
        insert = list(17L),
        insert = list(22L),
        insert = list(33L),
        insert = list(44L),
        insert = list(55L)
    )
    # }}}
    # 3: EnergyManagementSystem:OutputVariable {{{
    dt3 <- trans_action(idf, "EnergyManagementSystem:OutputVariable", add = list(6L))
    if (nrow(dt3)) {
        # add unit if applicable
        units <- dt3[J(1L), on = "index"][, c("value", "unit") := {
            as.data.table(stri_match_first_regex(value, "^(.+)\\s+\\[(.*)\\]$"))[, 2:3]
        }][!is.na(value)]

        # update key value
        dt3[units, on = c("id", "index"), value := i.value]
        # update unit
        set(units, NULL, "index", 6L)
        dt3[units, on = c("id", "index"), value := i.unit]
    }
    # }}}
    # 4: EnergyManagementSystem:MeteredOutputVariable {{{
    dt4 <- trans_action(idf, "EnergyManagementSystem:MeteredOutputVariable", add = list(9L))
    if (nrow(dt4)) {
        # add unit if applicable
        units <- dt4[J(1L), on = "index"][, c("value", "unit") := {
            as.data.table(stri_match_first_regex(value, "^(.+)\\s+\\[(.*)\\]$"))[, 2:3]
        }][!is.na(value)]

        # update key value
        dt4[units, on = c("id", "index"), value := i.value]
        # update unit
        set(units, NULL, "index", 9L)
        dt4[units, on = c("id", "index"), value := i.unit]
    }
    # }}}
    # 5: Branch {{{
    dt5 <- trans_action(idf, "Branch")
    if (nrow(dt5)) {
        dt5[(index - 4L) %% 5L == 0L & stri_trans_tolower(value) %in% paste0("heatexchanger:", c("watersideeconomizer", "hydronic", "plate")),
            value := "HeatExchanger:FluidToFluid"]
    }
    # }}}
    # 6: PlantEquipmentList {{{
    dt6 <- trans_action(idf, "PlantEquipmentList")
    if (nrow(dt6)) {
        dt6[(index - 2L) %% 2L == 0L & stri_trans_tolower(value) %in% paste0("heatexchanger:", c("watersideeconomizer", "hydronic", "plate")),
            value := "HeatExchanger:FluidToFluid"]
    }
    # }}}
    # 7: CondenserEquipmentList {{{
    dt7 <- trans_action(idf, "CondenserEquipmentList")
    if (nrow(dt7)) {
        dt7[(index - 2L) %% 2L == 0L & stri_trans_tolower(value) %in% paste0("heatexchanger:", c("watersideeconomizer", "hydronic", "plate")),
            value := "HeatExchanger:FluidToFluid"]
    }
    # }}}
    # 8: HeatExchanger:WatersideEconomizer {{{
    dt8 <- trans_action(idf, c("HeatExchanger:FluidToFluid" = "HeatExchanger:WatersideEconomizer"), all = TRUE,
        offset = list(
            c(3L, 6L, 7L, 9L, 4L, 5L, 8L, 2L),
            c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L)
        ),
        reset = list(9L, "PlateFrame", "CrossFlowBothUnMixed"),
        insert = list(11L, "CoolingDifferentialOnOff"),
        insert = list(12L),
        add = list(14L, "FreeCooling")
    )
    # }}}
    # 9: HeatExchanger:Hydronic {{{
    dt9 <- trans_action(idf, c("HeatExchanger:FluidToFluid" = "HeatExchanger:Hydronic"), all = TRUE,
        delete = list(18L),
        offset = list(
            c(5L, 6L, 13L, 7L, 8L, 14L, 11L, 12L, 17L, 15L,  9L),
            c(3L, 4L,  5L, 6L, 7L,  8L,  9L, 10L, 15L, 16L, 17L)
        ),
        reset = list(2L, NA_character_),
        reset = list(9L, "UFactorTimesAreaEffectiveness", "CrossFlowBothUnMixed"),
        add = list(11L, "CoolingSetpointOnOffWithComponentOverride"),
        add = list(12L),
        add = list(13L, "0.0"),
        add = list(14L, "FreeCooling")
    )
    if (nrow(dt9)) {
        dt9[, value := {value[12L] <- value[7L]; value}, by = "id"]

        # create corresponding SetpointManager:Schedule objects
        dt9_spm <- trans_action(idf, c("SetpointManager:Scheduled" = "HeatExchanger:Hydronic"), min_fields = 8L)[
            J(c(1L, 2L, 4L, 8L)), on = "index"]
        dt9_spm[J(1L), on = "index", value := {
            if (any(!is.na(value))) {
                value[!is.na(value)] <- paste0(value[!is.na(value)], " Setpoint Manager")
            }
            value
        }]
        dt9_spm[J(2L), on = "index", value := "Temperature"]
        dt9_spm[, index := seq_len(.N), by = "id"]
        new_idf$load(dt9_spm, .unique = FALSE, .default = FALSE)
    }
    # }}}
    # 10: HeatExchanger:Plate {{{
    dt10 <- trans_action(idf, c("HeatExchanger:FluidToFluid" = "HeatExchanger:Plate"), all = TRUE,
        offset = list(
            c(4L, 5L, 10L, 11L,  8L,  9L),
            c(3L, 4L,  5L,  8L,  9L, 10L)
        ),
        reset = list(2L, NA_character_),
        reset = list(9L, "UFactorTimesAreaEffectiveness", "CrossFlowBothUnMixed"),
        add = list(11L, "UncontrolledOn"),
        add = list(12L:13L),
        add = list(14L, "LoopToLoop"),
        add = list(15L:17L)
    )
    # }}}

    # standardize_vertices {{{
    standardize_vertices <- function (idf, class, start) {
        dt <- trans_action(idf, class)
        if (nrow(dt)) dt[index >= start & is.na(value), value := "0.0"]
        dt
    }
    # }}}
    # 11: BuildingSurface:Detailed {{{
    dt11 <- standardize_vertices(idf, "BuildingSurface:Detailed", 11L)
    # }}}
    # 12: Wall:Detailed {{{
    dt12 <- standardize_vertices(idf, "Wall:Detailed", 10L)
    # }}}
    # 13: RoofCeiling:Detailed {{{
    dt13 <- standardize_vertices(idf, "RoofCeiling:Detailed", 10L)
    # }}}
    # 14: Floor:Detailed {{{
    dt14 <- standardize_vertices(idf, "Floor:Detailed", 10L)
    # }}}
    # 15: FenestrationSurface:Detailed {{{
    dt15 <- standardize_vertices(idf, "FenestrationSurface:Detailed", 11L)
    # }}}
    # 16: Shading:Site:Detailed {{{
    dt16 <- standardize_vertices(idf, "Shading:Site:Detailed", 4L)
    # }}}
    # 17: Shading:Building:Detailed {{{
    dt17 <- standardize_vertices(idf, "Shading:Building:Detailed", 4L)
    # }}}
    # 18: Shading:Zone:Detailed {{{
    dt18 <- standardize_vertices(idf, "Shading:Zone:Detailed", 5L)
    # }}}
    # 19: AirflowNetwork:Distribution:Component:ConstantVolumeFan {{{
    dt19 <- trans_action(idf, c("AirflowNetwork:Distribution:Component:Fan" = "AirflowNetwork:Distribution:Component:ConstantVolumeFan"))
    # }}}
    # 20: ZoneHVAC:HighTemperatureRadiant {{{
    dt20 <- trans_action(idf, "ZoneHVAC:HighTemperatureRadiant",
        reset = list(5L, "electric", "Electricity"),
        reset = list(5L, "gas", "NaturalGas")
    )
    # }}}
    # 21: AirConditioner:VariableRefrigerantFlow {{{
    dt21 <- trans_action(idf, "AirConditioner:VariableRefrigerantFlow",
        reset = list(67L, "electric", "Electricity")
    )
    # }}}

    # warning_reset {{{
    warning_reset <- function (idf, class, index = NULL, old = NULL, new = NULL) {
        if (is.null(index)) return(trans_action(idf, class))

        dt <- trans_action(idf, class, reset = list(index, old, new))

        if (nrow(dt)) {
            warn(paste0("Default values for some fields in class ", surround(class),
                    " have been changed from v7.2 to v8.0. ",
                    "Results may be different than previous. ",
                    "See InputOutputReference document for details."
                ),
                "warning_trans_720_800"
            )
        }

        dt
    }
    # }}}
    # 22: ZoneHVAC:WaterToAirHeatPump {{{
    dt22 <- warning_reset(idf, "ZoneHVAC:WaterToAirHeatPump")
    # }}}
    # 23: AirLoopHVAC:UnitaryHeatPump:WaterToAir {{{
    dt23 <- warning_reset(idf, "AirLoopHVAC:UnitaryHeatPump:WaterToAir")
    # }}}
    # 24: Boiler:HotWater {{{
    dt24 <- warning_reset(idf, "Boiler:HotWater", 15L, "VariableFlow", "LeavingSetpointModulated")
    # }}}
    # 25: Chiller:Electric {{{
    dt25 <- warning_reset(idf, "Chiller:Electric", 27L, "VariableFlow", "LeavingSetpointModulated")
    # }}}
    # 26: Chiller:ConstantCOP {{{
    dt26 <- warning_reset(idf, "Chiller:ConstantCOP", 11L, "VariableFlow", "LeavingSetpointModulated")
    # }}}
    # 27: Chiller:EngineDriven {{{
    dt27 <- warning_reset(idf, "Chiller:EngineDriven", 41L, "VariableFlow", "LeavingSetpointModulated")
    # }}}
    # 28: Chiller:CombustionTurbine {{{
    dt28 <- warning_reset(idf, "Chiller:CombustionTurbine", 54L, "VariableFlow", "LeavingSetpointModulated")
    # }}}
    # 29: Chiller:Electric:EIR {{{
    dt29 <- warning_reset(idf, "Chiller:Electric:EIR", 23L, "VariableFlow", "LeavingSetpointModulated")
    # }}}
    # 30: Chiller:Electric:ReformulatedEIR {{{
    dt30 <- warning_reset(idf, "Chiller:Electric:ReformulatedEIR", 21L,"VariableFlow", "LeavingSetpointModulated")
    # }}}
    # 31: Chiller:Absorption {{{
    dt31 <- warning_reset(idf, "Chiller:Absorption", 23L,"VariableFlow", "LeavingSetpointModulated")
    # }}}
    # 32: Chiller:Absorption:Indirect {{{
    dt32 <- warning_reset(idf, "Chiller:Absorption:Indirect", 16L,"VariableFlow", "LeavingSetpointModulated")
    # }}}

    trans_process(new_idf, idf, rbindlist(mget(paste0("dt", 1:32))))

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_800_810 {{{
#' @importFrom checkmate assert_true
trans_funs$f800t810 <- function (idf) {
    assert_true(idf$version()[, 1:2] == 8.0)

    target_cls <- c(
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
    )

    new_idf <- trans_preprocess(idf, 8.1, target_cls)

    # 1: People {{{
    dt1 <- trans_action(idf, "People",
        insert = list(16L, "ClothingInsulationSchedule"),
        insert = list(17L)
    )
    # }}}
    # 2: CoolingTower:SingleSpeed {{{
    dt2 <- trans_action(idf, class = "CoolingTower:SingleSpeed", min_fields = 12L,
        reset = list(8L, "autosize", "Autocalculate"),
        insert = list(9L),
        reset = list(10L, "autosize", "Autocalculate"),
        insert = list(11L),
        insert = list(13L),
        insert = list(16L)
    )
    # }}}
    # 3: CoolingTower:TwoSpeed {{{
    dt3 <- trans_action(idf, "CoolingTower:TwoSpeed", min_fields = 16L,
        reset = list(8L, "autosize", "Autocalculate"),
        insert = list(9L),
        reset = list(10L, "autosize", "Autocalculate"),
        insert = list(11L),
        insert = list(13L),
        reset = list(14L, "autosize", "Autocalculate"),
        insert = list(15L),
        reset = list(16L, "autosize", "Autocalculate"),
        insert = list(17L),
        insert = list(19L),
        insert = list(22L),
        insert = list(24L)
    )
    # }}}
    # 4: EvaporativeFluidCooler:SingleSpeed {{{
    dt4 <- trans_action(idf, "EvaporativeFluidCooler:SingleSpeed", min_fields = 9L,
        insert = list(9L)
    )
    # }}}
    # 5: EvaporativeFluidCooler:TwoSpeed {{{
    dt5 <- trans_action(idf, "EvaporativeFluidCooler:TwoSpeed", min_fields = 17L,
        reset = list(6L, "autosize", "Autocalculate"),
        insert = list(7L),
        reset = list(8L, "autosize", "Autocalculate"),
        insert = list(9L),
        insert = list(13L),
        insert = list(16L),
        reset = list(18L, "autosize", "Autocalculate"),
        insert = list(19L),
        insert = list(23L)
    )
    # }}}
    # 6: FluidCooler:TwoSpeed {{{
    dt6 <- trans_action(idf, "FluidCooler:TwoSpeed", min_fields = 16L,
        reset = list(6L, "autosize", "Autocalculate"),
        insert = list(7L),
        insert = list(10L),
        reset = list(17L, "autosize", "Autocalculate"),
        insert = list(18L),
        reset = list(19L, "autosize", "Autocalculate"),
        insert = list(20L)
    )
    # }}}
    # 7: HeatPump:WaterToWater:EquationFit:Heating {{{
    dt7 <- trans_action(idf, "HeatPump:WaterToWater:EquationFit:Heating", min_fields = 19L,
        delete = list(20L)
    )
    # }}}
    # 8: HeatPump:WaterToWater:EquationFit:Cooling {{{
    dt8 <- trans_action(idf, "HeatPump:WaterToWater:EquationFit:Cooling", min_fields = 19L,
        delete = list(20L)
    )
    # }}}
    # 9: HeatPump:WaterToWater:ParameterEstimation:Heating {{{
    dt9 <- trans_action(idf, "HeatPump:WaterToWater:ParameterEstimation:Heating", min_fields = 20L,
        delete = list(23L)
    )
    # }}}
    # 10: HeatPump:WaterToWater:ParameterEstimation:Cooling {{{
    dt10 <- trans_action(idf, "HeatPump:WaterToWater:ParameterEstimation:Cooling", min_fields = 20L,
        delete = list(23L)
    )
    # }}}

    # Add `Any Number` ScheduleTypeLimits {{{
    if (any(idf$is_valid_class(c(
        "HVACTemplate:Zone:PTAC", "HVACTemplate:Zone:PTHP",
        "HVACTemplate:Zone:WaterToAirHeatPump", "HVACTemplate:System:Unitary",
        "HVACTemplate:System:UnitaryHeatPump:AirToAir"
    )))) {
        # check if there are any `Any Number` ScheduleTypeLimits objects
        if (idf$is_valid_class("ScheduleTypeLimits")) {
            nm_schtype <- idf$object_name(class = "ScheduleTypeLimits", simplify = TRUE)
            if (!any(stri_trans_tolower(nm_schtype) == "any number")) {
                new_idf$add(ScheduleTypeLimits = list("Any Number"))
            }
        } else {
            new_idf$add(ScheduleTypeLimits = list("Any Number"))
        }
    }
    # }}}
    # update_hvactemplate_fan {{{
    update_hvactemplate_fan <- function (new_idf, idf, class, min_fields) {
        dt <- trans_action(idf, class = class, min_fields = min_fields)

        if (!nrow(dt)) return(dt)

        dt[, value := {
            if (!is.na(value[min_fields])) {
                if (stri_trans_tolower(value[min_fields]) == "cycling") {
                    sch <- "CyclingFanSchedule"
                    len <- 100L - nchar(paste0(class[[1L]], sch)) - 1L
                    nm <- stri_sub(value[1L], to = len)
                    if (is.na(nm)) nm <- ""
                    value[min_fields] <- paste0(class[[1L]], nm, sch)
                    new_idf$add(`Schedule:Constant` = list(value[[min_fields]], "Any Number", 0))
                } else if (stri_trans_tolower(value[min_fields]) == "continuous") {
                    sch <- "ContinuousFanSchedule"
                    len <- 100L - nchar(paste0(class[[1L]], sch)) - 1L
                    nm <- stri_sub(value[1L], to = len)
                    if (is.na(nm)) nm <- ""
                    value[min_fields] <- paste0(class[[1L]], nm, sch)
                    new_idf$add(`Schedule:Constant` = list(value[[min_fields]], "Any Number", 1))
                }
            }
            value
        }, by = "id"]

        dt
    }
    # }}}
    # 11: HVACTemplate:Zone:PTAC {{{
    dt11 <- update_hvactemplate_fan(new_idf, idf, "HVACTemplate:Zone:PTAC", min_fields = 13L)
    # }}}
    # 12: HVACTemplate:Zone:PTHP {{{
    dt12 <- update_hvactemplate_fan(new_idf, idf, "HVACTemplate:Zone:PTHP", min_fields = 13L)
    # }}}
    # 13: HVACTemplate:Zone:WaterToAirHeatPump {{{
    dt13 <- update_hvactemplate_fan(new_idf, idf, "HVACTemplate:Zone:WaterToAirHeatPump", min_fields = 13L)
    # }}}
    # 14: HVACTemplate:System:Unitary {{{
    dt14 <- update_hvactemplate_fan(new_idf, idf, "HVACTemplate:System:Unitary", min_fields = 5L)
    # }}}
    # 15: HVACTemplate:System:UnitaryHeatPump:AirToAir {{{
    dt15 <- update_hvactemplate_fan(new_idf, idf, "HVACTemplate:System:UnitaryHeatPump:AirToAir", min_fields = 7L)
    # }}}

    trans_process(new_idf, idf, rbindlist(mget(paste0("dt", 1:15))))

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_810_820 {{{
#' @importFrom checkmate assert_true
trans_funs$f810t820 <- function (idf) {
    assert_true(idf$version()[, 1:2] == 8.1)

    target_cls <- c(
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
    )

    new_idf <- trans_preprocess(idf, 8.2, target_cls)

    # 1: ZoneHVAC:UnitVentilator {{{
    dt1 <- trans_action(idf, "ZoneHVAC:UnitVentilator", min_fields = 16L,
        insert = list(17L)
    )
    # }}}
    # 2: ZoneHVAC:UnitHeater {{{
    dt2 <- trans_action(idf, class = "ZoneHVAC:UnitHeater", all = TRUE, add = list(15L))
    if (nrow(dt2)) {
        dt2[, by = "id", value := {
            fantype <- value[[8L]]
            fantype[stri_trans_tolower(fantype) == "onoff"] <- "No"
            fantype[stri_trans_tolower(fantype) == "continuous"] <- "Yes"
            if (any(!fantype %in% c("No", "Yes"))) {
                warn(paste0("Invalid 'Fan Control Type' value for object [ID:", id, "] ",
                    "in class 'ZoneHVAC:UnitHeater' in original v8.1 IDF. ",
                    "Expected 'OnOff' or 'Continuous'. Assuming 'OnOff'..."),
                    "warning_trans_810_820"
                )
                fantype[!fantype %in% c("No", "Yes")] <- "No"
            }
            c(value[c(1:7, 9:10)], value[15], fantype, value[11:14])
        }]
    }
    # }}}
    # 3: PlantLoop {{{
    dt3 <- trans_action(idf, "PlantLoop", min_fields = 19L,
        reset = list(19L, "Sequential", "SequentialLoad"),
        reset = list(19L, "Uniform", "UniformLoad")
    )
    # }}}
    # 4: CondenserLoop {{{
    dt4 <- trans_action(idf, "CondenserLoop", min_fields = 19L,
        reset = list(19L, "Sequential", "SequentialLoad"),
        reset = list(19L, "Uniform", "UniformLoad")
    )
    # }}}
    # 5: HVACTemplate:Plant:ChilledWaterLoop {{{
    dt5 <- trans_action(idf, "HVACTemplate:Plant:ChilledWaterLoop", min_fields = 33L,
        reset = list(32L, "Sequential", "SequentialLoad"),
        reset = list(32L, "Uniform", "UniformLoad"),
        reset = list(33L, "Sequential", "SequentialLoad"),
        reset = list(33L, "Uniform", "UniformLoad")
    )
    # }}}
    # 6: HVACTemplate:Plant:HotWaterLoop {{{
    dt6 <- trans_action(idf, "HVACTemplate:Plant:HotWaterLoop", min_fields = 21L,
        reset = list(21L, "Sequential", "SequentialLoad"),
        reset = list(21L, "Uniform", "UniformLoad")
    )
    # }}}
    # 7: HVACTemplate:Plant:MixedWaterLoop {{{
    dt7 <- trans_action(idf, "HVACTemplate:Plant:MixedWaterLoop", min_fields = 17L,
        reset = list(17L, "Sequential", "SequentialLoad"),
        reset = list(17L, "Uniform", "UniformLoad")
    )
    # }}}
    # 8: Sizing:System {{{
    dt8 <- trans_action(idf, "Sizing:System", min_fields = 21L,
        insert = list(18:20),
        insert = list(23:26)
    )
    # }}}
    # 9: ZoneHVAC:Baseboard:RadiantConvective:Water {{{
    dt9 <- trans_action(idf, "ZoneHVAC:Baseboard:RadiantConvective:Water", min_fields = 8L,
        insert = list(7L, "HeatingDesignCapacity"),
        insert = list(9:10)
    )
    # }}}
    # 10: ZoneHVAC:HighTemperatureRadiant {{{
    dt10 <- trans_action(idf, "ZoneHVAC:HighTemperatureRadiant", min_fields = 4L,
        insert = list(4L, "HeatingDesignCapacity"),
        insert = list(6:7)
    )
    # }}}
    # 11: ZoneHVAC:Baseboard:RadiantConvective:Steam {{{
    dt11 <- trans_action(idf, "ZoneHVAC:Baseboard:RadiantConvective:Steam", min_fields = 4L,
        insert = list(5L, "HeatingDesignCapacity"),
        insert = list(6L, "Autosize"),
        insert = list(7:8)
    )
    # }}}
    # 12: ZoneHVAC:Baseboard:RadiantConvective:Electric {{{
    dt12 <- trans_action(idf, "ZoneHVAC:Baseboard:RadiantConvective:Electric", min_fields = 3L,
        insert = list(3L, "HeatingDesignCapacity"),
        insert = list(5:6)
    )
    # }}}
    # 13: ZoneHVAC:Baseboard:Convective:Water {{{
    dt13 <- trans_action(idf, "ZoneHVAC:Baseboard:Convective:Water", min_fields = 4L,
        insert = list(5L, "HeatingDesignCapacity"),
        insert = list(6L, "Autosize"),
        insert = list(7:8)
    )
    # }}}
    # 14: ZoneHVAC:Baseboard:Convective:Electric {{{
    dt14 <- trans_action(idf, "ZoneHVAC:Baseboard:Convective:Electric", min_fields = 4L,
        insert = list(3L, "HeatingDesignCapacity"),
        insert = list(5:6)
    )
    # }}}
    # 15: ZoneHVAC:LowTemperatureRadiant:VariableFlow {{{
    dt15 <- trans_action(idf, "ZoneHVAC:LowTemperatureRadiant:VariableFlow", min_fields = 12L,
        insert = list(8L, "HeatingDesignCapacity"),
        insert = list(9L, "Autosize"),
        insert = list(10:11),
        insert = list(17L, "CoolingDesignCapacity"),
        insert = list(18L, "Autosize"),
        insert = list(19:20)
    )
    # }}}
    # 16: ZoneHVAC:LowTemperatureRadiant:Electric {{{
    dt16 <- trans_action(idf, "ZoneHVAC:LowTemperatureRadiant:Electric", min_fields = 5L,
        insert = list(5L, "HeatingDesignCapacity"),
        insert = list(7:8)
    )
    # }}}

    trans_process(new_idf, idf, rbindlist(mget(paste0("dt", 1:16))))

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_820_830 {{{
#' @importFrom checkmate assert_true
trans_funs$f820t830 <- function (idf) {
    assert_true(idf$version()[, 1:2] == 8.2)

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
    if (nrow(dt3)) dt3[index > 10L, index := index - 1L]
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
        dt4[J(c(13L:14L)), on = "index", value := {
            val <- suppressWarnings(as.double(value))
            value[1L] <- sprintf("%.5f", val[[2L]] / val[[1L]])
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

    trans_process(new_idf, idf, rbindlist(mget(paste0("dt", 1:5))))

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_830_840 {{{
#' @importFrom checkmate assert_true
trans_funs$f830t840 <- function (idf) {
    assert_true(idf$version()[, 1:2] == 8.3)

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
                tank <- with_silent(idf$object(.BY$id)$ref_to_object(18L)[[1L]])
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
            value

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
        dt9_2[J(1L), on = "index", value := paste("KATemp", seq.int(.N) + ka_num)]

        dt9_1 <- dt9[!J(12L), on = "index"][
            J(10L), on = "index", value := "Site:GroundTemperature:Undisturbed:KusudaAchenbach"][
            J(11L), on = "index", value := paste("KATemp", seq.int(.N) + ka_num)]
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
        dt10_2[J(1L), on = "index", value := paste("KATemp", seq.int(.N) + ka_num)]

        dt10_1 <- dt10[!J(12L), on = "index"][
            J(10L), on = "index", value := "Site:GroundTemperature:Undisturbed:KusudaAchenbach"][
            J(11L), on = "index", value := paste("KATemp", seq.int(.N) + ka_num)]
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
        dt11_2[J(1L), on = "index", value := paste("KATemp", seq.int(.N) + ka_num)]

        dt11_1 <- dt11[!J(21L), on = "index"][
            J(19L), on = "index", value := "Site:GroundTemperature:Undisturbed:KusudaAchenbach"][
            J(20L), on = "index", value := paste("KATemp", seq.int(.N) + ka_num)]
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
        dt12_2[J(1L), on = "index", value := paste("KATemp", seq.int(.N) + ka_num)]
        dt12_2[J(c(2L:4L)), on = "index", value := {
            # get material properties
            mat <- with_silent(idf$object(.BY$id)$ref_to_object(8L, "Material")[[1L]])
            if (length(mat)) {
                value <- mat$value(simplify = TRUE)[2L:4L]
            }
            value
        }, by = "id"]

        dt12_1 <- dt12[!J(11L), on = "index"][
            J(9L), on = "index", value := "Site:GroundTemperature:Undisturbed:KusudaAchenbach"][
            J(10L), on = "index", value := paste("KATemp", seq.int(.N) + ka_num)]
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
        dt13_2[J(1L), on = "index", value := paste("KATemp", seq.int(.N) + ka_num)]

        dt13_1 <- dt13[!J(21L), on = "index"][
            J(19L), on = "index", value := "Site:GroundTemperature:Undisturbed:KusudaAchenbach"][
            J(20L), on = "index", value := paste("KATemp", seq.int(.N) + ka_num)]
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
        dt14_2[J(1L), on = "index", value := paste("KATemp", seq.int(.N) + ka_num)]

        dt14_1 <- dt14[!J(22L), on = "index"][
            J(20L), on = "index", value := "Site:GroundTemperature:Undisturbed:KusudaAchenbach"][
            J(21L), on = "index", value := paste("KATemp", seq.int(.N) + ka_num)]
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

    trans_process(new_idf, idf, rbindlist(mget(paste0("dt", 1:18))))

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_840_850 {{{
#' @importFrom checkmate assert_true
trans_funs$f840t850 <- function (idf) {
    assert_true(idf$version()[, 1:2] == 8.4)

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

    trans_process(new_idf, idf, dt)

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_850_860 {{{
#' @importFrom checkmate assert_true
trans_funs$f850t860 <- function (idf) {
    assert_true(idf$version()[, 1:2] == 8.5)

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

    # SPECIAL
    # replace all "Coil:Heating:Gas" with "Coil:Heating:Fuel"
    get_priv_env(idf)$idf_env()$value[
        stri_detect_fixed(value_chr, "coil:heating:gas", case_insensitive = TRUE),
        value_chr := "Coil:Heating:Fuel"
    ]

    new_idf <- trans_preprocess(idf, 8.6, target_cls)

    # 1: Exterior:FuelEquipment {{{
    dt1 <- trans_action(idf, "Exterior:FuelEquipment",
        reset = list(2L, "Gas", "NaturalGas"),
        reset = list(2L, "LPG", "PropaneGas")
    )
    # }}}
    # 2: HVACTemplate:System:UnitarySystem {{{
    dt2 <- trans_action(idf, "HVACTemplate:System:UnitarySystem", all = TRUE, delete = list(57))
    if (nrow(dt2)) dt2[, index := seq_len(.N), by = "id"]
    # }}}
    # 3: HVACTemplate:System:Unitary {{{
    dt3 <- trans_action(idf, "HVACTemplate:System:Unitary", all = TRUE, delete = list(40L))
    if (nrow(dt3)) dt3[, index := seq_len(.N), by = "id"]
    # }}}
    # 4: ChillerHeater:Absorption:DirectFired {{{
    dt4 <- trans_action(idf, "ChillerHeater:Absorption:DirectFired", delete = list(33L))
    if (nrow(dt4)) dt4[, index := seq_len(.N), by = "id"]
    # }}}
    # 5: SetpointManager:SingleZone:Humidity:Minimum {{{
    dt5 <- trans_action(idf, "SetpointManager:SingleZone:Humidity:Minimum", delete = list(2:3))
    if (nrow(dt5)) dt5[, index := seq_len(.N), by = "id"]
    # }}}
    # 6: SetpointManager:SingleZone:Humidity:Maximum {{{
    dt6 <- trans_action(idf, "SetpointManager:SingleZone:Humidity:Maximum", delete = list(2:3))
    if (nrow(dt2)) dt6[, index := seq_len(.N), by = "id"]
    # }}}
    # 7: AirTerminal:SingleDuct:VAV:Reheat {{{
    dt7 <- trans_action(idf, "AirTerminal:SingleDuct:VAV:Reheat", all = TRUE)
    if (nrow(dt7)) {
        dt7[, value := {
            if (!anyNA(value[16L:18L]) & stri_trans_tolower(value[16]) == "reverse") {
                value[16] <- "ReverseWithLimits"
            }
            value
        }, by = "id"]
    }
    # }}}
    # 8: Branch {{{
    dt8 <- trans_action(idf, "Branch", delete = list(2), delete = list(8, step = 5))
    if (nrow(dt8)) dt8[, index := seq_len(.N), by = "id"]
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
        add = list(c(10L, 13L, 14L, 17L)),
        reset = list(3L, "SplitFlux"),
        reset = list(8L, "0", NA_character_),
        reset = list(13L, NA_character_)
    )

    if (!nrow(dt14_1)) {
        dt14_2 <- data.table()
        dt14_3 <- data.table()
    } else {
        dt14_1[, value := {
            nm <- if (is.na(value[1L])) "" else value[1L]
            value[10L] <- paste0(nm, "_DaylRefPt1")
            value[14L] <- paste0(nm, "_DaylRefPt1")
            if (value[2L] == "2") {
                value[17L] <- paste0(nm, "_DaylRefPt2")
            } else {
                value[17L:19L] <- NA_character_
            }
            value[2L] <- value[1L]
            value[1L] <- paste0(nm, "_DaylCtrl")

            value
        }, by = "id"]

        # remove #2 ref point if applicable
        obj_id <- dt14_1[J(17L:19L), on = "index", list(all(is.na(value))), by = "id"][V1 == TRUE, id]
        if (length(obj_id)) {
            dt14_1 <- dt14_1[!data.table::CJ(id = obj_id, index = 17L:19L), on = c("id", "index")]
        }

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
        dt14_3 <- dt14_2[J(c(1L, 2L, 6L:8L)), on = "index"]

        dt14_2 <- dt14_2[index <= 5L]
        dt14_2[, value := {
            value[2L] <- value[1L]
            value[1L] <- if (is.na(value[1L])) "_DaylRefPt1" else paste0(value[1L], "_DaylRefPt1")
            value
        }, by = "id"]

        obj_id <- dt14_3[J(2L, "2"), on = c("index", "value"), id, nomatch = 0L]
        if (!length(obj_id)) {
            dt14_3 <- data.table()
        } else {
            dt14_3 <- dt14_3[J(obj_id), on = "id"][
                J(c(6L:8L)), on = "index", index := c(3L:5L), by = "id"
            ][, value := {
                value[2L] <- value[1L]
                value[1L] <- if (is.na(value[1L])) "_DaylRefPt2" else paste0(value[1L], "_DaylRefPt2")
                value
            }, by = "id"]
            # in order to distinguish from first ref point's id
            dt14_3[, id := -id]
        }
    }
    dt14 <- rbindlist(list(dt14_1, dt14_2, dt14_3))
    # }}}
    # 15: Daylighting:DELight:ReferencePoint {{{
    dt15 <- trans_action(idf, all = TRUE,
        class = c("Daylighting:ReferencePoint" = "Daylighting:DELight:ReferencePoint")
    )
    if (nrow(dt15)) {
        dt15 <- dt15[index <= 5L]
        nm_zone <- vcapply(unique(dt15$id),
            function (id) {
                zone <- with_silent(idf$object(id)$ref_to_object(2L, class = "Zone", depth = NULL)[[1L]])
                if (!length(zone)) NA_character_ else zone$value(2)[[1L]]
            }
        )
        dt15[J(2L), on = "index", value := nm_zone]
    }
    # }}}
    # 16: Daylighting:DElight:Controls {{{
    dt16 <- trans_action(idf, all = TRUE,
        class = c("Daylighting:Controls" = "Daylighting:DELight:Controls"),
        insert = list(3L, "DElight"),
        insert = list(4L, NA_character_),
        add = list(11L:13L, c("0", NA_character_, NA_character_))
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
            value[13L] <- value[10L]
            value[10L] <- NA_character_

            value
        }, by = "id"]

        # extract reference point name, zone name, and illuminance setpoint
        dt16_2 <- lapply(unique(dt16_1$id),
            function (id) {
                refp <- with_silent(idf$object(id)$ref_by_object(1L, class = "Daylighting:DELight:ReferencePoint"))
                if (!length(refp)) {
                    data.table()
                } else {
                    dt <- rbindlist(lapply(refp, function (x) x$to_table(all = TRUE)))[index %in% c(1L, 6L, 7L)]
                    # update object id
                    set(dt, NULL, "id", id)
                    set(dt, NULL, "class", "Daylighting:Controls")
                    set(dt, NULL, "index", seq.int(nrow(dt)) + 13L)
                }
            }
        )

        dt16 <- rbindlist(c(list(dt16_1), dt16_2))
        setorderv(dt16, c("id", "index"))
    }
    # }}}
    # 17: MaterialProperty:MoisturePenetrationDepth:Settings {{{
    dt17 <- trans_action(idf, all = TRUE,
        class = "MaterialProperty:MoisturePenetrationDepth:Settings",
        add = list(7L:10L, "0")
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
            psat <- rep(NA_real_, length(tdb))
            psat[tdb <= 0] = exp(C1/tk + C2 + C3*tk + C4*tk**2 + C5*tk**3 + C6*tk**4 + C7*log(tk)) / 1000
            psat[tdb  > 0] = exp(C8/tk + C9 + C10*tk + C11*tk**2 + C12*tk**3 + C13*log(tk)) / 1000
            psat
        }
        # }}}
        dt17 <- dt17[, by = "id",
            value := {
                value[7L] <- value[2L]

                # get material density
                mat <- with_silent(idf$object(.BY$id)$ref_to_object(1L, "Material")[[1L]])
                if (length(mat)) {
                    den <- mat$Density
                } else {
                    warn(paste0(
                            "Material match issue:\n",
                            "Did not find a matched material component for name '",
                            value[[1L]], "' referenced in class ",
                            "'MaterialProperty:MoisturePenetrationDepth:Settings'."
                        ),
                        "warning_tran_850_860"
                    )
                    den <- NA_real_
                }

                # calculate
                coeffs <- suppressWarnings(as.double(value[3L:6L]))
                d_empd <- suppressWarnings(as.double(value[2L]))
                mu <- cal_mu_empd(coeffs[1L], coeffs[2L], coeffs[3L], coeffs[4L], d_empd, den)
                if (is.na(mu)) {
                    value[2L] <- paste0("Could not find Material Match for ",
                        if (is.na(value[1L])) "" else value[1L]
                    )
                } else {
                    value[2L] <- sprintf("%.7f", mu)
                }
                value
            }
        ]
    }
    # }}}
    # 18: EnergyManagementSystem:Actuator {{{
    dt18 <- trans_action(idf, "EnergyManagementSystem:Actuator",
        reset = list(4L, "outdoor air dryblub temperature", "Outdoor Air Drybulb Temperature"),
        reset = list(4L, "outdoor air wetblub temperature", "Outdoor Air Wetbulb Temperature")
    )
    # }}}
    # 19: Output:Variable (DIRECTLY UPDATE) {{{
    # For Output:Variable that reference a specific Daylighting:Controls object
    # need update to the new reference point name
    if (new_idf$is_valid_class("Output:Variable")) {
        dt19 <- new_idf$to_table(class = "Output:Variable", wide = TRUE)
        set(dt19, NULL, "key_value", stri_trans_tolower(stri_trim_both(dt19$`Key Value`)))
        set(dt19, NULL, "variable_name", stri_trans_tolower(stri_trim_both(dt19$`Variable Name`)))

        dt19_1 <- dt19[key_value != "*" & stri_sub(variable_name, to = 27L) == "daylighting reference point"]
        if (nrow(dt19_1)) {
            # DELight Reference Point
            if (nrow(dt15)) {
                refpt <- dt15[index == 1L, list(id, key_value = stri_trans_tolower(stri_trim_both(value)))]
                dt19_1[!refpt, on = "key_value", list(id, class, index = 1L, value = paste0(stri_trim_both(`Key Value`), "_DaylCtrl"))]
            } else {
                dt19_1 <- dt19_1[, list(id, class = class, index = 1L, value = paste0(stri_trim_both(`Key Value`), "_DaylCtrl"))]
            }
            set(dt19_1, NULL, c("key_value", "variable_name"), NULL)
            dt19_1 <- dt_to_load(dt19_1)
        } else {
            dt19_1 <- data.table()
        }

        dt19_2 <- dt19[key_value != "*" & variable_name == "daylighting lighting power multiplier"]
        if (nrow(dt19_2)) {
            # DELight Reference Point
            if (nrow(dt15)) {
                refpt <- dcast.data.table(dt15[J(c(1:2)), on = "index"], id + class ~ field, value.var = "value")
                # add control name
                refpt[, `:=`(ctrlname = idf$object(id[[1L]])$value(2L)[[1L]]), by = "id"]
                dt19_2 <- dt19_2[refpt, on = c("key_value" = "DElight Name"), list(id, class, index = 1L, value = ctrlname), nomatch = 0L]
            } else {
                dt19_2 <- dt19_2[, list(id, class = class, index = 1L, value = paste0(stri_trim_both(`Key Value`), "_DaylCtrl"))]
            }
        } else {
            dt19_2 <- data.table()
        }

        dt19 <- rbindlist(list(dt19_1, dt19_2), fill = TRUE)

        if (nrow(dt19)) new_idf$update(dt19, .default = FALSE)
    }
    # }}}

    trans_process(new_idf, idf, rbindlist(mget(paste0("dt", 1:18))))

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_860_870 {{{
#' @importFrom checkmate assert_true
trans_funs$f860t870 <- function (idf) {
    assert_true(idf$version()[, 1:2] == 8.6)

    target_cls <- c(
        "Coil:Cooling:DX:MultiSpeed",                # 1
        "Coil:Heating:DX:MultiSpeed",                # 2
        "CoolingTower:SingleSpeed",                  # 3
        "CoolingTower:TwoSpeed",                     # 4
        "CoolingTower:VariableSpeed:Merkel",         # 5
        "AirflowNetwork:SimulationControl",          # 6
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
    # 6: AirflowNetwork:SimulationControl {{{
    dt6 <- trans_action(idf, "AirflowNetwork:SimulationControl", delete = list(4L))
    if (nrow(dt6)) dt6[, index := seq_len(.N), by = "id"]
    # }}}
    # 7: ZoneCapacitanceMultiplier:ResearchSpecial {{{
    dt7 <- trans_action(idf, "ZoneCapacitanceMultiplier:ResearchSpecial",
        insert = list(1:2, c("Multiplier", NA_character_))
    )
    # }}}
    # 8: WaterHeater:HeatPump:WrappedCondenser {{{
    dt8 <- trans_action(idf, "WaterHeater:HeatPump:WrappedCondenser",
        reset = list(35, "MutuallyExlcusive", "MutuallyExclusive"))
    # }}}
    # 9: AirflowNetwork:Distribution:Component:Duct {{{
    dt9 <- trans_action(idf, "AirflowNetwork:Distribution:Component:Duct", all = TRUE,
        add = list(9:10)
    )
    if (nrow(dt9)) {
        dt9[index >= 7L, value := {
            AFNDuctFracRcond <- 0.815384615
            AFNDuctFracRout <- 0.153846154
            AFNDuctFracRin <- 0.030769231

            val <- suppressWarnings(as.double(value))
            # 7
            value[1L] <- sprintf("%.6f", val[1L] / AFNDuctFracRcond)
            # 9
            value[3L] <- sprintf("%.6f", val[1L] / AFNDuctFracRout)
            # 10
            value[4L] <- sprintf("%.6f", val[1L] / AFNDuctFracRin)

            value
        }, by = "id"]
    }
    # }}}

    trans_process(new_idf, idf, rbindlist(mget(paste0("dt", 1:9))))

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_870_880 {{{
#' @importFrom checkmate assert_true
trans_funs$f870t880 <- function (idf) {
    assert_true(idf$version()[, 1:2] == 8.7)

    target_cls <- c(
        "Output:Surfaces:List",                           # 1
        "Table:TwoIndependentVariables",                  # 2
       # Only used to check corresponding perimeter object
       #"BuildingSurface:Detailed",                       # 3
       # Only used to check corresponding perimeter object
       #"Floor:Detailed",                                 # 4
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
        targets <- dt3[index == 5L, !is.na(value) & tolower(value) == "foundation"] &
                   dt3[index == 2L, !is.na(value) & tolower(value) == "floor"]
        obj_id <- dt3[index == 1L, id][targets]

        if (!length(obj_id)) {
            dt3 <- data.table()
        } else {
            # check if surface has a corresponding perimeter object
            # if not, give a warning and add one
            dt3 <- rbindlist(lapply(obj_id,
                function (i) {
                    perim <- with_silent(idf$object(i)$ref_by_object(1L, class = "SurfaceProperty:ExposedFoundationPerimeter")[[1L]])

                    if (length(perim)) return(data.table())

                    dt <- dt3[J(i), on = "id"]
                    n <- ceiling((nrow(dt) - 10L) / 3L)
                    dt <- dt[index <= 4L + n]
                    set(dt, NULL, "class", "SurfaceProperty:ExposedFoundationPerimeter")

                    dt[J(2L), on = "index", value := "BySegment"]
                    dt[J(c(3L, 4L)), on = "index", value := NA_character_]
                    dt[index > 4L, value := "Yes"]

                    warn(paste0(
                            "Foundation floors now require a ",
                            "'SurfaceProperty:ExposedFoundationPerimeter' ",
                            "object. One was added with each segment of the ",
                            "floor surface exposed (", surround(idf$object(i)$name()), "). ",
                            "Please check your inputs to make sure this ",
                            "reflects your foundation."
                        ),
                        "warning_trans_870_880"
                    )

                    dt
                }
            ))
        }
    }
    # }}}
    # 4: Floor:Detailed {{{
    dt4 <- trans_action(idf, "Floor:Detailed", min_fields = 4L)
    if (nrow(dt4)) {
        targets <- dt4[index == 4L, !is.na(value) & tolower(value) == "foundation"]
        obj_id <- dt4[index == 1L, id][targets]

        if (!length(obj_id)) {
            dt4 <- data.table()
        } else {
            # check if surface has a corresponding perimeter object
            # if not, give a warning and add one
            dt4 <- rbindlist(lapply(obj_id,
                function (i) {
                    perim <- with_silent(idf$object(i)$ref_by_object(1L, class = "SurfaceProperty:ExposedFoundationPerimeter")[[1L]])

                    if (length(perim)) return(data.table())
                    dt <- dt4[J(i), on = "id"]
                    n <- ceiling((nrow(dt) - 9L) / 3L)
                    dt <- dt[index <= 4L + n]

                    set(dt, NULL, "class", "SurfaceProperty:ExposedFoundationPerimeter")

                    dt[J(2L), on = "index", value := "BySegment"]
                    dt[J(c(3L, 4L)), on = "index", value := NA_character_]
                    dt[index > 4L, value := "Yes"]
                    warn(paste0(
                            "Foundation floors now require a ",
                            "'SurfaceProperty:ExposedFoundationPerimeter' ",
                            "object. One was added with each segment of the ",
                            "floor surface exposed (", surround(idf$object(i)$name()), "). ",
                            "Please check your inputs to make sure this ",
                            "reflects your foundation."
                        ),
                        "warning_trans_870_880"
                    )

                    dt
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
    # 7: UnitarySystemPerformance:Multispeed {{{
    dt7 <- trans_action(idf, "UnitarySystemPerformance:Multispeed", insert = list(5))
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
    if (nrow(dt13)) dt13[, index := seq_len(.N), by = "id"]
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

    trans_process(new_idf, idf, rbindlist(mget(paste0("dt", 1:16))))

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_880_890 {{{
#' @importFrom checkmate assert_true
trans_funs$f880t890 <- function (idf) {
    assert_true(idf$version()[, 1:2] == 8.8)

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
        # this will remove #9
        offset = list(8:9, 7:8),
        reset = list(5L, "Site:GroundTemperature:Undisturbed:KusudaAchenbach"),
        add = list(9L)
    )
    if (nrow(dt2_1)) {
        dt2_1 <- dt2_1[index <= 9L]
        dt2_1[, value := {
            name <- if (is.na(value[1L])) "" else value[1L]
            value[6L] <- paste(name, "Ground Temps")
            value[9L] <- paste(name, "Response Factors")
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
        add = list(6L, "3.90e+6"),
        reset = list(8L, "1.77e+6")
    )

    if (nrow(dt2_2)) {
        dt2_2 <- dt2_2[index <= 11L]
        dt2_2[J(1L), on = "index", value := {
            paste(if (is.na(value)) "" else value, "Properties")
        }, by = "id"]
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
        dt2_3[J(1L), on = "index", value := {
            value[!is.na(value)] <- paste(value[!is.na(value)], "Ground Temps")
            value
        }]
        dt2_3[J(4L), on = "index", value := {
            val <- suppressWarnings(as.double(value))
            val[is.na(val)] <- 0.0
            val <- val / 920
            sprintf("%.5f", val)
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
        dt2_4[J(c(1L, 2L)), on = "index", value := {
            nm <- if (is.na(value[1L])) "" else value[1L]
            value[1L] <- paste(nm, "Response Factors")
            value[2L] <- paste(nm, "Properties")
            value
        }, by = "id"]
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
    dt4 <- trans_action(idf, "CondenserEquipmentList")
    if (nrow(dt4)) {
        dt4[(index - 2L) %% 2L == 0L, value := gsub("GroundHeatExchanger:Vertical",
            "GroundHeatExchanger:System", value, ignore.case = TRUE)
        ]
    }
    # }}}
    # 5: ElectricEquipment:ITE:AirCooled {{{
    dt5 <- trans_action(idf, "ElectricEquipment:ITE:AirCooled", insert = list(3L, "FlowFromSystem"))
    # }}}
    # 6: Schedule:Day:Interval {{{
    dt6 <- trans_action(idf, "Schedule:Day:Interval", reset = list(3L, "yes", "Average"))
    # }}}
    # 7: Schedule:Day:List {{{
    dt7 <- trans_action(idf, "Schedule:Day:List", reset = list(3L, "yes", "Average"))
    # }}}
    # 8: Schedule:Compact {{{
    dt8 <- trans_action(idf, "Schedule:Compact")
    if (nrow(dt8)) {
        dt8[index >= 3L & grepl("interpolate.+yes", value, ignore.case = TRUE), value := "Interpolate:Average"]
    }
    # }}}

    trans_process(new_idf, idf, rbindlist(mget(paste0("dt", 1:8))))

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_890_900 {{{
#' @importFrom checkmate assert_true
trans_funs$f890t900 <- function (idf) {
    assert_true(idf$version()[, 1:2] == 8.9)

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
                eplusr_error_invalid_class_name = function (e) NA_character_
            )
        )
    )
    if (nrow(dt1) && idf$object_num("OutdoorAir:Mixer") > 1L) {
        warn("Multiple 'OutdoorAir:Mixer' object found.", "warning_trans_890_900")
    }
    # }}}
    # 2: AirflowNetwork:Distribution:Component:ReliefAirFlow {{{
    dt2 <- trans_action(idf, "AirflowNetwork:Distribution:Component:ReliefAirFlow",
        insert = list(2,
            tryCatch(idf$object_name("OutdoorAir:Mixer", simplify = TRUE)[[1L]],
                eplusr_error_invalid_class_name = function (e) NA_character_
            )
        )
    )
    if (nrow(dt2) && idf$object_num("OutdoorAir:Mixer") > 1L) {
        warn("Multiple 'OutdoorAir:Mixer' object found.", "warning_trans_890_900")
    }
    # }}}
    # 3: Boiler:HotWater {{{
    dt3 <- trans_action(idf, "Boiler:HotWater", delete = list(7L))
    if (nrow(dt3)) dt3[, index := seq_len(.N), by = "id"]
    # }}}
    # 4: FenestrationSurface:Detailed {{{
    dt4 <- trans_action(idf, "FenestrationSurface:Detailed", delete = list(7L))
    if (nrow(dt4)) dt4[, index := seq_len(.N), by = "id"]
    # }}}
    # 5: GlazedDoor {{{
    dt5 <- trans_action(idf, "GlazedDoor", delete = list(4L))
    if (nrow(dt5)) dt5[, index := seq_len(.N), by = "id"]
    # }}}
    # 6: RunPeriod:CustomRange {{{
    dt6 <- trans_action(idf, c("RunPeriod" = "RunPeriod:CustomRange"))
    if (nrow(dt6)) {
        dt6[index == 8L, value := {
            if (any(usewthrfile <- tolower(value) == "useweatherfile")) {
                warn(paste0(
                        "For 'RunPeriod:CustomRange' ", surround(name[[1L]]), " [ID:", id[[1L]], "]:\n",
                        "Option 'UseWeatherFile' for 'Start Day of Week' has been removed, ",
                        "start week day is set by the input start date."
                    ),
                    "warning_trans_890_900"
                )
                value[usewthrfile] <- NA_character_
            }
            value
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
                    if (!checkmate::test_count(as.numeric(start_year))) {
                        abort(paste0(
                            "For 'RunPeriod' ", surround(name[[1L]]), " [ID:", id[[1L]], "]:\n",
                            "Invalid 'Start Year' value (", surround(start_year), ") found."
                        ))
                    }
                    # if start year is set in the old run period, use it
                    value[4L] <- start_year
                }
                start_year <- as.integer(start_year)

                # convert num of repeats to integer
                if (!is.na(num_rep)) {
                    if (!checkmate::test_count(as.numeric(num_rep))) {
                        abort(paste0(
                            "For 'RunPeriod' ", surround(name[[1L]]), " [ID:", id[[1L]], "]:\n",
                            "Invalid 'Number of Times Runperiod to be Repeated' value (",
                            surround(num_rep), ") found."
                        ))
                    }

                    num_rep <- as.integer(num_rep)
                } else {
                    num_rep <- 0L
                    if (!is.na(value[8L]) && tolower(value[8L]) == "useweatherfile") {
                        warn(paste0(
                                "For 'RunPeriod' ", surround(name[[1L]]), " [ID:", id[[1L]], "]:\n",
                                "Option 'UseWeatherFile' for 'Start Day of Week' has been removed, ",
                                "start week day is set by the input start date."
                            ),
                            "warning_trans_890_900"
                        )
                        value[8L] <- NA_character_
                    }
                }

                if (num_rep > 1L) {
                    # for case when start year is given
                    if (!is.na(start_year)) {
                        if (!is.na(value[8L]) && tolower(value[8L]) == "useweatherfile") {
                            warn(paste0(
                                    "For 'RunPeriod' ", surround(name[[1L]]), " [ID:", id[[1L]], "]:\n",
                                    "Option 'UseWeatherFile' for 'Start Day of Week' has been removed, ",
                                    "start week day is set by the input start date."
                                ),
                                "warning_trans_890_900"
                            )
                            value[8L] <- NA_character_
                        }
                    # if start year is not set but repeat times larger than 1
                    # need to calculate start year
                    } else {
                        # in case of leap year
                        start_date <- lubridate::make_date(year = 2016L, month = value[2L], day = value[3L])
                        # validate start month and day
                        if (is.na(start_date)) {
                            abort(paste0(
                                "For 'RunPeriod' ", surround(name[[1L]]), " [ID:", id[[1L]], "]:\n",
                                "Invalid 'Start Month' (", surround(value[2]), ") or ",
                                "'Start Day of Month' (", surround(value[3]), ") found."
                            ))
                        }

                        # validate day of week
                        if (is.na(value[8L])) {
                            # Sunday
                            weekday <- 1L
                        } else if (tolower(value[8L]) == "useweatherfile") {
                            warn(paste0(
                                    "For 'RunPeriod' ", surround(name[[1L]]), " [ID:", id[[1L]], "]:\n",
                                    "Option 'UseWeatherFile' for 'Start Day of Week' has been removed, ",
                                    "start week day is set by the input start date."
                                ),
                                "warning_trans_890_900"
                            )
                            value[8L] <- NA_character_
                            # Sunday
                            weekday <- 0L
                        } else {
                            weekday <- get_epw_wday(value[8L], monday_start = FALSE)
                            if (is.na(weekday)) {
                                warn(paste0(
                                        "For 'RunPeriod' ", surround(name[[1L]]), " [ID:", id[[1L]], "]:\n",
                                        "Invalid 'Start Day of Week' (", surround(value[8L]), ") found. ",
                                        "Assuming 'Sunday'."
                                    ),
                                    "warning_trans_890_900"
                                )
                            }
                        }

                        # NOTE: just to make sure the year calculation results are
                        # the same as VersionUpdater
                        YEARS <- c(2013, 2014, 2015, 2010, 2011, 2017, 2007, 2013, 2014, 2015, 2010, 2011, 2017)
                        LYEARS <- c(2008, 1992, 2004, 2016, 2000, 2012, 1996, 2008, 1992, 2004, 2016, 2000, 2012)

                        if (lubridate::month(start_date) == 2 && lubridate::mday(start_date) == 29) {
                            leap <- TRUE
                        } else {
                            leap <- FALSE
                            lubridate::year(start_date) <- 2017L
                        }
                        ord <- lubridate::yday(start_date)
                        rem <- ord %% 7L

                        # start year
                        if (leap) {
                            start_year <- LYEARS[weekday - rem + 6]
                        } else {
                            start_year <- YEARS[weekday - rem + 6]
                        }
                        value[4L] <- as.character(start_year)
                    }

                    # end year
                    end_year <- start_year + num_rep
                    value[7L] <- as.character(end_year)

                    end_month <- assert_integerish(as.numeric(value[5L]), len = 1L, lower = 1L, upper = 12L, any.missing = FALSE, coerce = TRUE, .var.name = "End Month")
                    end_day <- assert_integerish(as.numeric(value[6L]), len = 1L, lower = 1L, upper = 31L, any.missing = FALSE, coerce = TRUE, .var.name = "End Day of Month")

                    # check if leap day of end date is specified in an non-leap year
                    if ((!leap_year(end_year)) && end_month == 2L && end_day == 29L) {
                        warn(paste0(
                            "For 'RunPeriod' ", surround(name[[1L]]), " [ID:", id[[1L]], "]:\n",
                            "With 'Number of Times Runperiod to be Repeated' being ", num_rep,
                            "the end year will be ", end_year, ", which is not a leap year. ",
                            "The end date will be reset to Feb 28th."
                        ), "error_trans_890_900")
                        value[6L] <- "28"
                    }

                    # validate end month and day
                    end_date <- lubridate::make_date(year = end_year, month = end_month, day = end_day)
                    # validate start month and day
                    if (is.na(end_date)) {
                        abort(paste0(
                            "For 'RunPeriod' ", surround(name[[1L]]), " [ID:", id[[1L]], "]:\n",
                            "Invalid 'End Month' (", surround(value[2]), ") or ",
                            "'End Day of Month' (", surround(value[3]), ") found."
                        ))
                    }
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
    if (nrow(dt10)) dt10[, index := seq_len(.N), by = "id"]
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
                fene <- with_silent(ctrl$ref_by_object("Name",
                    class = c("FenestrationSurface:Detailed", "Window", "GlazedDoor")
                ))

                if (!length(fene)) {
                    data.table(id_ctrl = ctrl$id(), name_ctrl = ctrl$name(),
                        id_fene = NA_integer_, name_fene = NA_character_,
                        id_zone = NA_integer_, name_zone = NA_character_,
                        id_daylgt = NA_integer_, name_daylgt = NA_character_
                    )
                } else {
                    # use low-level API to speed up
                    fene <- data.table(id_fene = viapply(fene, function (f) f$id()), name_fene = names(fene))

                    surf <- get_idf_value(
                        get_priv_env(idf)$idd_env(),
                        get_priv_env(idf)$idf_env(),
                        object = fene$id_fene, field = rep("Building Surface Name", nrow(fene)),
                        align = TRUE
                    )
                    # combine
                    fene[surf, on = c("id_fene" = "object_id"), `:=`(id_surf = i.value_id, name_surf = i.value_chr)]

                    # get zone name this fenestration belongs to
                    zone <- get_idf_relation(
                        get_priv_env(idf)$idd_env(),
                        get_priv_env(idf)$idf_env(),
                        value_id = unique(fene$id_surf), name = TRUE,
                        direction = "ref_to", keep_all = TRUE, depth = 1
                    )
                    # get surface name
                    surf <- zone[dep == 0L, list(id_surf = src_object_id, name_surf = value_chr)]
                    # get zone name
                    zone <- zone[J(1L, "Zone"), on = c("dep", "src_class_name")][
                        surf, on = c("object_id" = "id_surf"), list(id_zone = src_object_id, name_zone = value_chr)]

                    # combine
                    set(fene, NULL, c("id_zone", "name_zone"), zone)

                    # get daylighting control for each zone
                    daylgt <- get_idf_relation(
                        get_priv_env(idf)$idd_env(),
                        get_priv_env(idf)$idf_env(),
                        object_id = unique(zone$id_zone), name = TRUE,
                        direction = "ref_by", keep_all = TRUE
                    )[class_name == "Daylighting:Controls",
                        list(id_zone = src_object_id, id_daylgt = object_id)]
                    # get daylighting control name
                    if (!nrow(daylgt)) {
                        set(daylgt, NULL, "name_daylgt", character())
                    } else {
                        daylgt[get_priv_env(idf)$idf_env()$object, on = c("id_daylgt" = "object_id"),
                            name_daylgt := i.object_name
                        ]
                    }

                    # combine
                    fene[daylgt, on = "id_zone", `:=`(id_daylgt = i.id_daylgt, name_daylgt = i.name_daylgt)]

                    # add control id and name
                    set(fene, NULL, c("id_ctrl", "name_ctrl"), list(ctrl$id(), ctrl$name()))

                    # clean
                    set(fene, NULL, c("id_surf", "name_surf"), NULL)
                }
            }
        )
        # }}}
        fene_daylight_zone <- rbindlist(fene_daylight_zone, use.names = TRUE)[,
            list(rleid = .GRP, name_fene = list(name_fene)),
            by = c("id_ctrl", "id_zone", "name_ctrl", "name_zone", "name_daylgt")
        ]

        dt11 <- trans_action(idf, all = TRUE,
            class = c("WindowShadingControl" = "WindowProperty:ShadingControl"),
            insert = list(c(`Zone Name` = 2L, `Shading Control Sequence Number` = 3L)),
            add = list(c(`Daylighting Control Object Name` = 15L,
                         `Multiple Surface Control Type` = 16L
            ))
        )

        # duplicate
        dt11 <- rbindlist(lapply(seq.int(nrow(fene_daylight_zone)), function (i) {
            set(copy(dt11), NULL, "rleid", i)
        }))

        # update name with zone name suffix
        set(fene_daylight_zone, NULL, "index", 1L)
        dt11[fene_daylight_zone, on = c("rleid", "id" = "id_ctrl", "index"), value := {
            found <- !is.na(value) & !is.na(i.name_zone)
            value[found] <- paste0(value[found], "-", i.name_zone[found])
            value
        }]

        # update zone name
        set(fene_daylight_zone, NULL, "index", 2L)
        dt11[fene_daylight_zone, on = c("rleid", "id" = "id_ctrl", "index"), value := i.name_zone]

        # assign new object ID
        fene_daylight_zone[, id_ctrl := new_id(get_priv_env(idf)$idf_env()$object, "object_id", .N)]
        # update dt
        dt11[fene_daylight_zone, on = "rleid", id := i.id_ctrl]

        # if unused, remove and throw a warning
        if (nrow(empty <- dt11[J(2L, NA_character_), on = c("index", "value"), nomatch = 0L])) {
            warn(paste0(
                "WindowProperty:ShadingControl = ",
                collapse(unique(empty[, {ifelse(is.na(name), "", (name))}])),
                " was not used by any surfaces, so it has been deleted.",
                collpase = "\n"
            ), "warning_trans_890_900")
            dt11 <- dt11[!empty, on = c("id", "name")]
            fene_daylight_zone <- fene_daylight_zone[!empty, on = c("id_ctrl" = "id", "name_ctrl" = "name")]
        }

        if (!nrow(dt11)) {
            dt11 <- data.table()
        } else {
            # update control sequence
            set(fene_daylight_zone, NULL, "index", 3L)
            fene_daylight_zone[, ctrl_seq := seq_along(.N), by = "id_zone"]
            dt11[fene_daylight_zone, on = c("rleid", "id" = "id_ctrl", "index"), value := as.character(ctrl_seq)]

            # update multiple surface control type name
            dt11[, value := {
                if (!is.na(value[[4L]]) & tolower(value[[4L]]) == "switchableglazing" &&
                    !is.na(value[[6L]]) & tolower(value[[6L]]) == "meetdaylightilluminancesetpoint"
                ) {
                    value[[16]] <- "Group"
                } else {
                    value[[16]] <- "Sequential"
                }
                value
            }, by = "id"]

            # update daylighting control name
            set(fene_daylight_zone, NULL, "index", 15L)
            dt11[fene_daylight_zone, on = c("rleid", "id" = "id_ctrl", "index"), value := i.name_daylgt]

            # add fenestration surface names
            fene_fld <- fene_daylight_zone[, list(
                name = NA_character_,
                class = "WindowShadingControl",
                index = seq_along(unlist(name_fene)) + 16L,
                field = NA_character_,
                value = unlist(name_fene)
            ), by = "rleid"]
            fene_fld[dt11, on = "rleid", id := i.id]

            dt11 <- rbindlist(list(dt11, fene_fld), use.names = TRUE)
            setorderv(dt11, c("id", "index"))
            set(dt11, NULL, "rleid", NULL)
        }
    }
    # }}}

    trans_process(new_idf, idf, rbindlist(mget(paste0("dt", 1:11))))

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_900_910 {{{
#' @importFrom checkmate assert_true
trans_funs$f900t910 <- function (idf) {
    assert_true(idf$version()[, 1:2] == 9.0)

    target_cls <- c(
        "HybridModel:Zone",      # 1
        "ZoneHVAC:EquipmentList" # 2
    )

    new_idf <- trans_preprocess(idf, 9.1, target_cls)

    # 1: HybridModel:Zone {{{
    dt1 <- trans_action(idf, "HybridModel:Zone", all = 9L,
        # Old [6:9] --> New [17:20]
        offset = list(6:9, 17:20),
        # Old NA    --> New [7:16]
        add = list(7:16),
        # Old [5]   --> New [6]
        offset = list(5L, 6L),
        # Old NA    --> New [5]
        add = list(5L, "No")
    )
    # }}}
    # 2: ZoneHVAC:EquipmentList {{{
    dt2 <- trans_action(idf, "ZoneHVAC:EquipmentList", min_fields = 6L,
        # Old NA    --> New [7:8] with step 4
        insert = list(7:8, NA, 4L)
    )
    # }}}

    trans_process(new_idf, idf, rbindlist(mget(paste0("dt", 1:2))))

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}
# trans_910_920 {{{
#' @importFrom checkmate assert_true
trans_funs$f910t920 <- function (idf) {
    assert_true(idf$version()[, 1:2] == 9.1)

    target_cls <- c(
        "Foundation:Kiva",               # 1
        "RunPeriod",                     # 2
        "Schedule:File",                 # 3
        "Table:OneIndependentVariable",  # 4
        "Table:TwoIndependentVariables", # 5
        "Table:MultiVariableLookup",     # 6
        "ThermalStorage:Ice:Detailed",   # 7
        "ZoneHVAC:EquipmentList"         # 8
    )

    new_idf <- trans_preprocess(idf, 9.2, target_cls)

    # 1: Foundation:Kiva {{{
    dt1 <- trans_action(idf, "Foundation:Kiva", insert = list(2L))
    # }}}
    # 2: RunPeriod {{{
    dt2 <- trans_action(idf, "RunPeriod")
    if (nrow(dt2)) {
        dt2[J(1L), on = "index", value := {
            # get exiting run period number
            existing <- stri_match_first_regex(stri_trans_tolower(value), "^runperiod (\\d+)$")[, 2L]
            existing <- as.integer(existing)
            # get number of run period without names
            n <- sum(is.na(value))

            # valid index left
            valid <- setdiff(seq.int(.N), existing)
            if (n >= length(valid)) {
                new <- c(valid, seq(max(valid) + 1L, length.out = n - length(valid)))
            } else {
                new <- valid[seq.int(n)]
            }

            value[is.na(value)] <- paste("RUNPERIOD", new)
            value
        }]
    }
    # }}}
    # 3: Schedule:File {{{
    dt3 <- trans_action(idf, "Schedule:File", reset = list(7, "fixed", "SPACE"))
    # }}}

    id_max <- 0L
    # init_var_dt {{{
    init_var_dt <- function (dt, i_expr = NULL, min_max, type) {
        dt <- if (is.null(i_expr)) copy(dt) else dt[eval(i_expr)]

        # calculate fields should be added
        to_add <- setdiff(c(min_max, type), c(4L:5L, 7L))
        # only add fields with index no more than 10
        if (length(to_add)) to_add <- to_add[to_add <= 10L]

        trans_action_dt(dt,
            offset = list(min_max, 4L:5L),
            offset = list(type, 7L),
            add = list(to_add, NA_character_),
            reset = list(6L, NA_character_),
            reset = list(8L:10L, NA_character_)
        )
    }
    # }}}
    # init_var_type {{{
    init_var_type <- function (dt, index_cond = 3L) {
        set(dt, NULL, "value_lower", stri_trans_tolower(dt$value))
        id_const <- dt[index == index_cond & value_lower == "linearinterpolationoftable", id]
        id_linear <- dt[index == index_cond & value_lower == "lagrangeinterpolationlinearextrapolation", id]
        set(dt, NULL, "value_lower", NULL)
        dt[J(id_const), on = "id", value := {
            value[2] <- "Linear"
            value[3] <- "Constant"
            value
        }, by = "id"]
        dt[J(id_linear), on = "id", value := {
            value[2] <- "Cubic"
            value[3] <- "Linear"
            value
        }, by = "id"]
        dt[!J(c(id_const, id_linear)), on = "id", value := {
            value[2] <- "Cubic"
            value[3] <- "Constant"
            value
        }, by = "id"]
        dt[J(1L, NA_character_), on = c("index", "value"), value := ""]
        dt[, index := seq.int(.N), by = "id"]
    }
    # }}}
    # init_list_dt {{{
    init_list_dt <- function (dt, num) {
        if (is.integer(num)) {
            dt <- dt[J(rep(1L, num + 1L)), on = "index"]
        } else if (is.data.frame(num)) {
            dt <- dt[num, on = c("id", "index")]
            dt[dt[J(1L), on = "index"], on = "id", value := i.value]
        }
        dt[, index := seq.int(.N), by = "id"]

        set(dt, NULL, "class", "Table:IndependentVariableList")
        dt[is.na(value), value := ""]
        dt[J(1L), on = "index", value := paste0(value, "_IndependentVariableList")]

        dt[index > 1L, value := paste0(value, "_IndependentVariable", seq.int(.N)), by = "id"]
        setorderv(dt, "id")

        dt
    }
    # }}}
    # init_lookup_dt {{{
    init_lookup_dt <- function (dt, i_expr = NULL, ref = 10L, min_max = 6L:7L, type = 9L, del = NULL) {
        dt <- if (is.null(i_expr)) copy(dt) else dt[eval(i_expr)]
        dt[, class := "Table:Lookup"]

        dt[J(1L, NA_character_), on = c("index", "value"), value := ""]
        dt[J(c(1L:2L)), on = "index", value := {
            value[2L] <- paste0(value[1L], "_IndependentVariableList")
            value
        }, by = "id"]

        # calculate fields should be added
        to_add <- setdiff(c(ref, min_max, type), c(4L:7L))
        # only add fields with index no more than 10
        if (length(to_add)) to_add <- to_add[to_add <= 10L]

        dt <- trans_action_dt(dt,
            offset = list(ref, 4L),
            offset = list(min_max, 5L:6L),
            offset = list(type, 7L),
            add = list(to_add),
            reset = list(8L:10L, NA_character_)
        )

        if (!is.null(del)) dt <- dt[!J(del), on = "index"]

        dt[J(c(3L, 4L)), on = "index", value := {
            value[[1L]] <- if (is.na(value[[2L]])) NA_character_ else "DivisorOnly"
            value
        }, by = "id"]

        dt[, index := seq.int(.N), by = "id"]
        dt
    }
    # }}}
    # warn_removed_as_comment {{{
    warn_removed_as_comment <- function (idf, class) {
        warn(paste0("Class '", class, "' has been removed in EnergyPlus v9.2. ",
            "Objects in that class will be listed as comments in the new output file."
        ), "warning_trans_910_920")

        # separate by objects
        cmt <- idf$to_string(class = class, header = FALSE, format = "new_top")
        sep <- which(stri_isempty(cmt))
        cmt <- apply2(c(1L, sep[-length(sep)] + 1L), sep - 1L,
            function (start, end) cmt[start:end]
        )

        # assign to object table
        ids <- idf$object_id(class = class, simplify = TRUE)
        get_priv_env(idf)$idf_env()$object[J(ids), on = "object_id", comment := list(cmt)]
    }
    # }}}
    # warn_table_convert {{{
    warn_table_convert <- function (dt, class, idx, ascending = NULL) {
        dt_file <- dt[index == idx & !is.na(value)]
        if (!nrow(dt_file)) return(dt)

        # rename column for formatting object info
        setnames(dt_file, c("id", "name", "class"), c("object_id", "object_name", "class_name"))
        obj <- get_object_info(dt_file, c("name", "id"), numbered = TRUE)

        # get file path
        files <- dt_file$value
        warn(paste0("Objects in Class '", class, "' references external ",
            "file. External files must be converted to the new format and saved ",
            "to CSV with a name suffix '-New':\n",
            paste0(obj, ": ", surround(files))
        ), "warning_trans_910_920")

        # convert
        if (is.null(ascending)) {
            tables <- lapply(files, trans_table_convert)
        } else {
            assert_same_len(files, ascending)
            tables <- apply2(files, ascending, trans_table_convert)
        }

        # save
        files <- paste0(tools::file_path_sans_ext(basename(files)), "-New.csv")
        apply2(tables, files, function (x, file) fwrite(x, file))

        # update field value
        dt[index == idx & !is.na(value), value := normalizePath(files)]

        dt
    }
    # }}}
    # assert_integer {{{
    assert_integer <- function (dt, idx = NULL, col = "value", name) {
        if (!is.null(idx)) dt <- dt[J(idx), on = "index"]

        set(dt, NULL, col, suppressWarnings(as.integer(dt[[col]])))

        if (!anyNA(dt[[col]])) return(dt)

        abort(paste0("Failed to get ", name, "objects below:\n", obj_info(dt)))
    }
    # }}}
    # obj_info {{{
    obj_info <- function (dt, numbered = TRUE, collapse = "\n") {
        setnames(dt, c("id", "name", "class"), c("object_id", "object_name", "class_name"))
        get_object_info(dt, c("name", "id"), numbered = numbered, collapse = collapse)
    }
    # }}}
    # 4: Table:OneIndependentVariable {{{
    dt4 <- trans_action(idf, min_fields = 10L, c("Table:IndependentVariable" = "Table:OneIndependentVariable"))
    if (nrow(dt4)) {
        warn_removed_as_comment(idf, "Table:OneIndependentVariable")

        # independent variable
        dt4_1 <- init_var_dt(dt4,
            quote(index <= 10L | (index > 10L & (index - 11) %% 2 == 0)),
            min_max = 4L:5L, type = 8L
        )
        dt4_1 <- init_var_type(dt4_1, 3L)
        dt4_1[J(1L), on = "index", value := paste0(value, "_IndependentVariable1")]

        # variable list
        dt4_2 <- init_list_dt(dt4, 1L)
        dt4_2[, id := -.GRP + id_max, by = "id"]
        id_max <- min(dt4_2$id)

        # lookup
        dt4_3 <- init_lookup_dt(dt4,
            quote(index <= 10L | (index > 10L & (index - 12) %% 2 == 0)),
            ref = 10L, min_max = 6L:7L, type = 9L
        )
        dt4_3[, id := -.GRP + id_max, by = "id"]
        id_max <- min(dt4_3$id)

        dt4 <- rbindlist(list(dt4_1, dt4_2, dt4_3))
    }
    # }}}
    # 5: Table:TwoIndependentVariables {{{
    dt5 <- trans_action(idf, min_fields = 14L, align = FALSE,
        c("Table:IndependentVariable" = "Table:TwoIndependentVariables")
    )
    if (nrow(dt5)) {
        warn_removed_as_comment(idf, "Table:TwoIndependentVariables")
        dt5 <- warn_table_convert(dt5, "Table:TwoIndependentVariables", 14L)

        val <- dt5[index > 14, by = "id", {
            val <- matrix(suppressWarnings(as.numeric(value)), ncol = 3L, byrow = TRUE)
            val <- setnames(as.data.table(val), c("x", "y", "out"))
            setorderv(val, c("x", "y"))
            list(x = list(unique(val$x)), y = list(unique(val$y)), out = list(val$out)
            )
        }]

        # independent variable
        ## X
        dt5_11 <- init_var_dt(dt5, quote(index <= 10L), min_max = 4L:5L, type = 10L)
        dt5_11 <- init_var_type(dt5_11, 3L)
        dt5_11[J(1L), on = "index", value := paste0(value, "_IndependentVariable1")]
        dt5_11 <- rbindlist(fill = TRUE, list(
            dt5_11,
            val[, by = "id", list(class = "Table:IndependentVariable",
                index = 10L + seq_along((x[[1L]])), value = as.character(x[[1L]]))]
        ))

        ## Y
        dt5_12 <- init_var_dt(dt5, quote(index <= 11L), min_max = 6L:7L, type = 11L)
        dt5_12 <- init_var_type(dt5_12, 3L)
        dt5_12[J(1L), on = "index", value := paste0(value, "_IndependentVariable2")]
        dt5_12 <- rbindlist(fill = TRUE, list(
            dt5_12,
            val[, by = "id", list(class = "Table:IndependentVariable",
                index = 10L + seq_along((y[[1L]])), value = as.character(y[[1L]]))]
        ))
        dt5_12[, id := -.GRP + id_max, by = "id"]
        id_max <- min(dt5_12$id)

        ## merge
        dt5_1 <- rbindlist(list(dt5_11, dt5_12))

        # variable list
        dt5_2 <- init_list_dt(dt5, 2L)
        dt5_2[, id := -.GRP + id_max, by = "id"]
        id_max <- min(dt5_2$id)

        # lookup
        dt5_3 <- init_lookup_dt(dt5, quote(index <= 13L), ref = 13L, min_max = 8L:9L, type = 12L, del = 11L)
        dt5_3 <- rbindlist(fill = TRUE, list(
            dt5_3,
            val[, by = "id", list(class = "Table:Lookup",
                index = 10L + seq_along((out[[1L]])), value = as.character(out[[1L]]))]
        ))
        dt5_3[, id := -.GRP + id_max, by = "id"]
        id_max <- min(dt5_3$id)

        dt5 <- rbindlist(list(dt5_1, dt5_2, dt5_3))
        setorderv(dt5, c("id", "index"))
    }
    # }}}
    # 6: Table:MultiVariableLookup {{{
    dt6 <- trans_action(idf, min_fields = 31L, align = FALSE,
        c("Table:IndependentVariable" = "Table:MultiVariableLookup")
    )
    if (nrow(dt6)) {
        warn_removed_as_comment(idf, "Table:MultiVariableLookup")

        # get sort order
        asc <- dt6[J(c(7L, 8L)), on = "index",
            list(asc = list(is.na(value) | stri_trans_tolower(value) == c("ascending", "ascending"))),
            by = "id"]$asc

        # convert external file if possible
        dt6 <- warn_table_convert(dt6, "Table:MultiVariableLookup", 6L, asc)

        # independent variable {{{
        # get independent variable number
        num_vars <- dt6[J(31L), on = "index"]
        set(num_vars, NULL, "value", suppressWarnings(as.integer(num_vars$value)))
        if (anyNA(num_vars$value)) {
            abort(paste0("Failed to get number of independent variables for 'Table:MultiVariableLookup' objects ",
                "objects below:\n", obj_info(num_vars[is.na(value)])
            ))
        }

        # get indices for different fields
        meta <- num_vars[, {
            idx <- seq.int(value)
            list(idx_var = idx,
                 min = as.integer(10L + 2 * (idx - 1L)),
                 max = as.integer(11L + 2 * (idx - 1L)),
                 type = 23L + idx,
                 number = 31L + idx
            )
        }, by = "id"]

        # get value number
        meta[dt6, on = c("id", "number" = "index"), val_number := suppressWarnings(as.integer(i.value))]
        if (anyNA(meta$val_number)) {
            invld <- meta[is.na(val_number), list(id, idx_var)]
            invld <- num_vars[invld, on = "id"]
            obj <- obj_info(invld, collapse = NULL)
            mes <- paste0(obj, " for independent variable #", invld$idx_var)

            abort(paste0("Failed to get value number of independent variables for 'Table:MultiVariableLookup' objects below:\n", mes))
        }

        meta[, cum := cumsum(data.table::shift(val_number, fill = 0L)), by = "id"]
        meta[, c("start", "end") := {
            start <- number + max(idx_var) - idx_var + 1L + cum
            list(start = start, end = start + val_number - 1L)
        }, by = "id"]

        # assign id
        meta[, object_id := id]
        meta[idx_var > 1L, object_id := -.I + id_max]
        id_max <- min(meta$object_id)

        # reusable fields
        dt6_1 <- init_var_type(dt6[index <= 10L], 2L)
        meta_1 <- data.table::CJ(
            object_id = meta$object_id, index = c(1L:3L, 6L, 8L:10L))[
            meta[, list(id, object_id)], on = "object_id"]
        dt6_11 <- dt6_1[meta_1, on = c("id", "index")][
            index %in% 8L:10L, value := NA_character_]
        dt6_11[J(1L), on = "index", value := paste0(value, "_IndependentVariable", seq.int(.N)), by = "id"]

        # melt
        meta_2 <- melt.data.table(meta, id.vars = c("id", "idx_var", "object_id"),
            measure.vars = c("min", "max", "type"), value.name = "index"
        )
        meta_2[J("min"), on = "variable", field_index := 4L]
        meta_2[J("max"), on = "variable", field_index := 5L]
        meta_2[J("type"), on = "variable", field_index := 7L]
        dt6_12 <- dt6[meta_2[, list(id, index, object_id, field_index)], on = c("id", "index")]
        set(dt6_12, NULL, "index", NULL)
        setnames(dt6_12, "field_index", "index")

        # variable values
        meta_3 <- meta[,
            list(id = rep(id, val_number),
                 index = seq(start, end),
                 field_index = seq.int(val_number) + 10L
            ),
            by = "object_id"
        ]
        dt6_13 <- dt6[meta_3, on = c("id", "index")]
        set(dt6_13, NULL, "index", NULL)
        setnames(dt6_13, "field_index", "index")

        dt6_1 <- rbindlist(list(dt6_11, dt6_12, dt6_13), use.names = TRUE)
        # retain object order
        setorderv(dt6_1, c("id", "index"))
        dt6_1 <- dt6_1[J(meta$object_id), on = "object_id"]

        # update object id
        set(dt6_1, NULL, "id", NULL)
        setnames(dt6_1, "object_id", "id")
        setcolorder(dt6_1, "id")
        # }}}

        # variable list
        dt6_2 <- init_list_dt(dt6,
            rbindlist(list(
                num_vars[, list(id, index = 1L)],
                meta[, list(id, index = idx_var)]
            ))
        )
        dt6_2[, id := -.GRP + id_max, by = "id"]
        id_max <- min(dt6_2$id)

        # lookup object {{{
        dt6_31 <- init_lookup_dt(dt6, quote(index <= 30L),
            ref = 9L, min_max = 22L:23L, type = 30L
        )[index <= 10L]

        meta_out <- meta[
            , list(# where to start
                   start = max(end) + 1L,
                   # variable #3 above should be listed before output data
                   step = if (.N <= 2L) 0L else .N - 2L,
                   # number of output data per variable #1
                   num = if (.N <= 2L) 0L else val_number[[1L]] * val_number[[2L]]
            ), by = "id"][
            dt6[, list(end = max(which(!is.na(value)))), by = "id"], on = "id"]

        # get exact field index for each value
        meta_out <- meta_out[, list(index = {
            # for case when variable number no more than 2
            if (step == 0L) {
                seq(start, end)
            } else {
                # for case when variable number more than 2
                unlist(lapply(seq(start + step, end, by = step + num), seq, length.out = num))
            }
        }), by = "id"]

        dt6_32 <- dt6[meta_out, on = c("id", "index")]
        dt6_32[, index := seq.int(.N) + 10L, by = "id"]
        dt6_32[, class := "Table:Lookup"]

        # split by independent variables in each object
        vars <- split(dt6_13[, list(id, object_id, value = suppressWarnings(as.numeric(value)))],
            by = c("id", "object_id"), flatten = FALSE, keep.by = FALSE
        )
        # reverse the order of independent variables and unlist
        vars <- apply2(vars, asc, function (var, ascending) {
            var <- lapply(var, unlist, use.names = FALSE)
            if (!ascending[1L] && length(var) >= 1L) var[[1L]] <- rev(var[[1L]])
            if (!ascending[2L] && length(var) >= 2L) var[[2L]] <- rev(var[[2L]])
            var
        })
        vars <- lapply(vars, rev)

        # get combination
        cj <- function (...) data.table::CJ(..., sorted = FALSE, unique = FALSE)
        vars <- lapply(vars, function (x) do.call(cj, x))

        # get output
        outs <- split(dt6_32[, list(id, value)], by = "id", keep.by = FALSE)
        outs <- lapply(outs, unlist, use.names = FALSE)

        # combine value and output
        vals <- apply2(vars, outs, function (var, out) set(var, NULL, "value", out))
        # change order
        vals <- lapply(vals, function (dt) setorderv(dt, rev(setdiff(names(dt), "value"))))
        # replace with ordered values
        set(dt6_32, NULL, "value", as.character(unlist(lapply(vals, "[[", "value"))))
        dt6_3 <- rbindlist(list(dt6_31, dt6_32))
        dt6_3[, id := -.GRP + id_max, by = "id"]
        # }}}

        dt6 <- rbindlist(list(dt6_1, dt6_2, dt6_3))
        setorderv(dt6, c("id", "index"))
    }
    # }}}

    # 7: ThermalStorage:Ice:Detailed {{{
    dt7 <- trans_action(idf, "ThermalStorage:Ice:Detailed",
        reset = list(6L, "quadraticlinear", "FractionDischargedLMTD"),
        reset = list(6L, "cubiclinear", "LMTDMassFlow"),
        reset = list(8L, "quadraticlinear", "FractionChargedLMTD"),
        reset = list(8L, "cubiclinear", "LMTDMassFlow")
    )
    # }}}
    # 8: ZoneHVAC:EquipmentList {{{
    dt8 <- trans_action(idf, "ZoneHVAC:EquipmentList")
    if (nrow(dt8)) {
        clg <- dt8[index > 2L & ((index - 2L) - 5L) %% 6 == 0L & !is.na(value)]
        if (!nrow(clg)) {
            dt8_1 <- data.table()
        } else {
            clg[, object_id := -.I + id_max]
            clg[is.na(name), name := ""]
            dt8_1 <- clg[, list(id, class = "Schedule:Constant", index = 1L:3L,
                value = c(
                    paste0(name, " CoolingFrac", as.integer((index - 3L) / 6L + 1L)),
                    "ZoneEqList ScheduleTypeLimits",
                    value
                )
            ), by = "object_id"]
            id_max <- min(clg$object_id)

            # update value
            set(clg, NULL, "value", dt8_1[index == 1L, value])
            dt8[clg, on = c("id", "index"), value := i.value]

            # clean
            set(dt8_1, NULL, "id", NULL)
            setnames(dt8_1, "object_id", "id")
        }

        htg <- dt8[index > 2L & ((index - 2L) - 6L) %% 6 == 0L & !is.na(value)]
        if (!nrow(htg)) {
            dt8_2 <- data.table()
        } else {
            htg[, object_id := -.I + id_max]
            htg[is.na(name), name := ""]
            dt8_2 <- htg[, list(id, class = "Schedule:Constant", index = 1L:3L,
                value = c(
                    paste0(name, " HeatingFrac", as.integer((index - 3L) / 6L + 1L)),
                    "ZoneEqList ScheduleTypeLimits",
                    value
                )
            ), by = "object_id"]
            id_max <- min(htg$object_id)

            # update value
            set(htg, NULL, "value", dt8_2[index == 1L, value])
            dt8[htg, on = c("id", "index"), value := i.value]

            # clean
            set(dt8_2, NULL, "id", NULL)
            setnames(dt8_2, "object_id", "id")
        }

        # add a schedule type object for sequential clg/htg fraction
        if ((nrow(dt8_1) || nrow(dt8_2)) &&
            (
                !idf$is_valid_class("ScheduleTypeLimits") ||
                !idf$is_valid_name("ZoneEqList ScheduleTypeLimits")
            )
        ) {
            new_idf$add(ScheduleTypeLimits = list("ZoneEqList ScheduleTypeLimits", 0.0, 1.0, "Continuous"))
        }

        dt8 <- rbindlist(list(dt8, dt8_1, dt8_2), fill = TRUE)
    }
    # }}}

    trans_process(new_idf, idf, rbindlist(mget(paste0("dt", 1:8))))

    trans_postprocess(new_idf, idf$version(), new_idf$version())
}
# }}}

# trans_preprocess {{{
# 1. delete objects in deprecated class
# 2. delete all old objects
# 3. update IDD data
# 4. update version
# 5. assign new uuid
trans_preprocess <- function (idf, version, class = NULL) {
    # clone old IDF
    new_idf <- idf$clone(deep = TRUE)
    # get new IDD
    new_idd <- use_idd(version, "auto")

    # delete deprecated classes
    class_del <- setdiff(use_idd(idf$version())$class_name(), new_idd$class_name())

    # get all classes to be deleted
    class <- unique(c(CLASS_DEL_COMMON, class_del, class))

    # del all related class
    class <- class[idf$is_valid_class(class)]

    if (length(class)) {
        with_silent(new_idf$del(new_idf$object_id(class, simplify = TRUE), .force = TRUE))
    }

    priv <- get_priv_env(new_idf)

    # use old class name in object and value table
    add_joined_cols(priv$idd_env()$class, priv$idf_env()$object, "class_id", "class_name")
    add_joined_cols(priv$idf_env()$object, priv$idf_env()$value, "object_id", "class_name")

    # add field index in value table
    set(priv$idf_env()$value, NULL, "field_index", rowidv(priv$idf_env()$value, "object_id"))

    # store old idd table for comparison on min fields requirements
    old_class <- priv$idd_env()$class

    # update IDD
    priv$m_idd <- new_idd

    # update class id in object table
    add_joined_cols(priv$idd_env()$class, priv$idf_env()$object, "class_name", "class_id")

    # add class names in field table
    add_joined_cols(priv$idd_env()$class, priv$idd_env()$field, "class_id", "class_name")
    # update field id in value table
    set(priv$idf_env()$value, NULL, "field_id",
        priv$idd_env()$field[priv$idf_env()$value, on = c("class_name", "field_index"), "field_id"]
    )

    # check if there are newly added extensible groups
    if (anyNA(priv$idf_env()$value$field_id)) {
        # get id of object that has new extensible fields
        id_obj <- priv$idf_env()$value[is.na(field_id), unique(object_id)]
        # get field number per object
        dt_obj <- priv$idf_env()$value[J(id_obj), on = "object_id", list(field_num = .N), by = c("class_name", "object_id")]

        # merge with class table
        dt_cls <- priv$idd_env()$class[dt_obj, on = "class_name"]
        # calculate num of group to add
        dt_cls[, `:=`(num = as.integer((field_num - num_fields) / num_extensible))]
        # add extensible groups
        add_idd_extensible_group(priv$idd_env(), dt_cls)

        # update field id in value table
        set(priv$idf_env()$value, NULL, "field_id",
            priv$idd_env()$field[priv$idf_env()$value, on = c("class_name", "field_index"), "field_id"]
        )
    }

    # should remove class_name and field_index column before calling append_dt
    set(priv$idf_env()$value, NULL, c("class_name", "field_index"), NULL)

    # if min-fields increased, should add new fields
    priv$idf_env()$object[old_class, on = "class_name", old_min_fields := i.min_fields]
    priv$idf_env()$object[priv$idd_env()$class, on = "class_name", min_fields := i.min_fields]
    # get objects in classes whose min-fields requirements increased
    obj <- priv$idf_env()$object[min_fields > old_min_fields]
    if (nrow(obj)) {
        # get current field num
        obj[, field_num := priv$idf_env()$value[J(obj$object_id), on = "object_id", .N, by = "object_id"]$N]

        # check if there is a need to insert new fields
        if (nrow(obj <- obj[field_num < min_fields])) {
            obj <- obj[, list(
                field_index = seq(field_num + 1L, min_fields, 1L),
                class_id = class_id[[1L]]
            ), by = "object_id"]

            add_rleid(obj)

            val <- get_idd_field(priv$idd_env(), obj$class_id, obj$field_index,
                property = c("units", "ip_units", "default_chr", "default_num",
                    "required_field", "src_enum", "type_enum"
                )
            )
            # assign default values
            val <- assign_idf_value_default(priv$idd_env(), priv$idf_env(), val)

            # assign old object id
            val[obj, on = "rleid", object_id := i.object_id]

            # assign new value id
            set(val, NULL, "value_id", new_id(priv$idf_env()$value, "value_id", nrow(val)))

            # merge data
            idd_env <- priv$idd_env()
            idf_env <- priv$idf_env()
            idf_env$value <- append_dt(idf_env$value, val, "value_id")

            # add necessary columns used for getting references
            add_field_property(idd_env, idf_env$value, "src_enum")
            add_joined_cols(idf_env$object, idf_env$value, "object_id", "class_id")
            add_class_name(idd_env, idf_env$value)
            ref <- get_value_reference_map(idd_env, idf_env$value, idf_env$value)
            set(idf_env$value, NULL, c("src_enum", "class_id", "class_name"), NULL)
            idf_env$reference <- ref
        }
    }

    set(priv$idf_env()$object, NULL, c("class_name", "old_min_fields", "min_fields"), NULL)
    set(priv$idd_env()$field, NULL, c("class_name"), NULL)

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
# trans_process {{{
trans_process <- function (new_idf, old_idf, dt) {
    if (!nrow(dt))  return(new_idf)

    # remove redundant empty fields
    dt[get_priv_env(new_idf)$idd_env()$class, on = c("class" = "class_name"),
        `:=`(class_id = i.class_id, min_fields = i.min_fields, num_extensible = i.num_extensible)
        ]
    dt[get_priv_env(new_idf)$idd_env()$field, on = c("class_id", index = "field_index"),
        `:=`(extensible_group = i.extensible_group, required_field = i.required_field)
    ]

    # check if there are newly added extensible groups
    # if detected, set extensible_group to a random number
    # since extensible_group is only used to detect if current field is
    # extensible, it will be enough to assign newly-added extensible fields with
    # a non-zero integer
    dt[J(NA_integer_), on = "extensible_group", `:=`(required_field = FALSE, extensible_group = -1L)]

    # add fake value id
    dt[, value_id := .I]
    setnames(dt, c("id", "index", "value"), c("object_id", "field_index", "value_chr"))
    dt <- remove_empty_fields(get_priv_env(new_idf)$idd_env(), get_priv_env(new_idf)$idf_env(), dt)
    setnames(dt, c("object_id", "field_index", "value_chr"), c("id", "index", "value"))

    trans_process_load(new_idf, old_idf, dt)
}
# }}}
# trans_postprocess {{{
trans_postprocess <- function (idf, from, to) {
    # reset_key {{{
    reset_key <- function (dt, field_index = 1L) {
        if (!nrow(dt)) return(dt)
        dt[J(field_index, NA_character_), on = c("index", "value"), value := "*"]
    }
    # }}}
    id_del <- NULL
    # update_var {{{
    update_var <- function (dt, mapping, field_index, step = NULL, is_meter = FALSE, idf = NULL) {
        if (!nrow(dt)) return(dt)
        if (!nrow(mapping)) return(dt)

        if (!has_names(dt, "value_lower")) {
            set(dt, NULL, "value_lower", stri_trans_tolower(dt$value))
        }

        new <- NULL # eliminate check warning of no visible binding

        # delete deprecatd variable first
        id_obj <- dt[mapping[!is.na(old) & is.na(new)], on = c(value_lower = "old"), unique(id)]
        id_del <<- c(id_del, id_obj[!is.na(id_obj)])
        dt <- dt[!J(id_obj), on = "id"]

        # remove delete rules
        mapping <- mapping[!is.na(new)]

        if (!nrow(dt) || !nrow(mapping)) return(set(dt, NULL, "value_lower", NULL))

        # calculate field index
        if (!is.null(step)) {
            n <- (nrow(dt) - field_index) %/% step
            field_index <- as.integer(c(field_index, seq.int(n) * step + field_index))
        }

        # update variable names
        dt[J(field_index), on = "index", value := {
            # from v7.2 to v8.0 there are duplicated old vars
            # use the first and then handle the other
            mapping[J(value_lower), on = "old", mult = "first", {
                updated <- new
                # if no match, keep the old
                updated[is.na(new)] <- value[is.na(new)]
                updated
            }]
        }]

        # special case from v7.2 to v8.0 {{{
        if (nrow(mapping) && unique(mapping$from) == 7.2) {
            stopifnot(is_idf(idf))

            has_chiller <- any(idf$is_valid_class(c(
                "Chiller:Electric:EIR",
                "Chiller:Electric:ReformulatedEIR",
                "Chiller:Electric",
                "Chiller:Absorption:Indirect",
                "Chiller:Absorption",
                "Chiller:ConstantCOP",
                "Chiller:EngineDriven",
                "Chiller:CombustionTurbine"
            )))

            has_heater <- any(idf$is_valid_class(c(
                "ChillerHeater:Absorption:DirectFired",
                "ChillerHeater:Absorption:DoubleEffect"
            )))

            # there are duplicated old vars should add new rows {{{
            # remove the first case
            if (has_chiller & has_heater) {
                dup <- mapping[duplicated(old)]

                # update variable names
                sec <- dt[J(field_index), on = "index", nomatch = 0L]

                if (nrow(sec)) {
                    sec <- sec[, value := {
                        # in this case, no matched will return NA
                        dup[J(value_lower), on = "old", new]
                    }][!is.na(value)]

                    # get other fields
                    sec_1 <- dt[J(sec$id), on = "id"][!J(sec$index), on = "index"]
                    # combine and change old id to negative
                    sec <- rbindlist(list(sec, sec_1))[, id := -id]
                    setorderv(sec, c("id", "index"))

                    # combine them all
                    dt <- rbindlist(list(dt, sec))
                }
            }
            # }}}
            # handle CondFD Nodal Temperature {{{
            if (!is_meter) {
                # can still use the old name as value_lower has not been changed
                nodal <- dt[J(field_index, "condfd nodal temperature"),
                    on = c("index", "value_lower"), nomatch = 0L
                ]

                if (nrow(nodal)) {
                    key <- dt[J(nodal$index - 1L, nodal$id), on = c("index", "id")][,
                        value_lower := stri_trim_both(value_lower)
                    ][stri_isempty(value_lower), value_lower := NA_character_]

                    # if key value is "*" or NA
                    id_wild <- key[is.na(value_lower) | value_lower == "*", id]

                    # duplicate 10 times
                    if (length(id_wild)) {
                        obj_wild <- dt[J(id_wild), on = "id"]
                        # remove the original
                        id_del <<- c(id_del, id_wild[!is.na(id_wild)])
                        dt <- dt[!J(id_wild), on = "id"]

                        # get the smallest negative id in case processes above
                        # also assign negative id for distinguishing purpose
                        id_ne <- if (!nrow(dt)) 0L else min(c(dt$id, 0L))
                        obj_wild <- rbindlist(lapply(id_ne - (1L:10L),
                            function (dt, id) set(copy(dt), NULL, "id", id),
                            dt = obj_wild
                        ))

                        obj_wild[J("condfd nodal temperature"), on = "value_lower",
                            value := paste0("CondFD Surface Temperature Node ", rep(1L:10L, each = length(id_wild)))
                        ]

                        dt <- rbindlist(list(dt, obj_wild))
                    }

                    nodal <- nodal[!J(id_wild), on = "id"]
                    key <- key[!J(id_wild), on = "id"]

                    # use the key if given
                    key[, c("key_value", "variable") := as.data.table(stri_match_first_regex(value, "(.*)#(.*)"))[, 1L:2L]]
                    key <- key[!is.na(key_value)][, key_value := stri_sub(key_value, to = -5L)]
                    nodal <- nodal[J(key$id), on = "id"]

                    # update the original input
                    dt[key, on = c("id", "index"), value := i.key_value]
                    dt[nodal, on = c("id", "index"), value := i.value]
                }
            }
            # }}}
        }
        # }}}

        set(dt, NULL, "value_lower", NULL)
    }
    # }}}

    f <- as.double(as.character(standardize_ver(from)[, 1:2]))
    t <- as.double(as.character(standardize_ver(to)[, 1:2]))
    rep_vars <- REPORTVAR_RULES[J(f, t), on = c("from", "to")]
    if (nrow(rep_vars)) set(rep_vars, NULL, "old", stri_trans_tolower(rep_vars$old))

    # 1: Output:Variable {{{
    dt1 <- trans_action(idf, "Output:Variable")
    dt1 <- reset_key(dt1, 1L)
    dt1 <- update_var(dt1, rep_vars, 2L, idf = idf)
    # }}}
    # 2: Output:Meter:* {{{
    dt2 <- rbindlist(lapply(
        c("Output:Meter",
          "Output:Meter:MeterFileOnly",
          "Output:Meter:Cumulative",
          "Output:Meter:Cumulative:MeterFileOnly"
        ),
        trans_action, idf = idf
    ))
    dt2 <- update_var(dt2, rep_vars, 1L, is_meter = TRUE, idf = idf)
    # }}}
    # 3: Output:Table:TimeBins {{{
    dt3 <- trans_action(idf, "Output:Table:TimeBins")
    dt3 <- reset_key(dt3, 1L)
    dt3 <- update_var(dt3, rep_vars, 2L, idf = idf)
    # }}}
    # 4: ExternalInterface:FunctionalMockupUnitImport:From:Variable & ExternalInterface:FunctionalMockupUnitExport:From:Variable {{{
    dt4_1 <- trans_action(idf, "ExternalInterface:FunctionalMockupUnitImport:From:Variable")
    dt4_2 <- trans_action(idf, "ExternalInterface:FunctionalMockupUnitExport:From:Variable")
    dt4 <- rbindlist(list(dt4_1, dt4_2))
    dt4 <- reset_key(dt4, 1L)
    dt4 <- update_var(dt4, rep_vars, 2L, idf = idf)
    # }}}
    # 5: EnergyManagementSystem:Sensor {{{
    dt5 <- trans_action(idf, "EnergyManagementSystem:Sensor")
    dt5 <- update_var(dt5, rep_vars, 3L, idf = idf)
    # }}}
    # 6: Output:Table:Monthly {{{
    dt6 <- trans_action(idf, "Output:Table:Monthly", min_fields = 4L)
    dt6 <- update_var(dt6, rep_vars, 3L, step = 2, idf = idf)
    # }}}
    # 7: Meter:Custom {{{
    dt7 <- trans_action(idf, "Meter:Custom")
    dt7 <- update_var(dt7, rep_vars, 4L, step = 2L, is_meter = TRUE, idf = idf)
    # }}}
    # 8: Meter:CustomDecrement {{{
    dt8 <- trans_action(idf, "Meter:CustomDecrement")
    dt8 <- update_var(dt8, rep_vars, 3L, 2L, is_meter = TRUE, idf = idf)
    # }}}

    dt <- rbindlist(mget(paste0("dt", 1:8)))
    if (!nrow(dt)) return(idf)

    if (length(id_del <- unique(c(id_del, dt[id > 0, id])))) {
        trans_process_load(idf$clone()$del(id_del), idf, dt, empty = FALSE)
    } else {
        trans_process_load(idf$clone(), idf, dt, empty = FALSE)
    }
}
# }}}

# trans_action {{{
#' @importFrom checkmate assert_true test_names
trans_action <- function (idf, class, min_fields = 1L, all = FALSE, align = TRUE, ...) {
    assert_true(idf$is_valid_class(class, all = TRUE))
    if (!idf$is_valid_class(class)) return(data.table())

    dt <- idf$to_table(class = class, align = align, all = all)

    # make sure min fields are returned
    if (!all && min_fields > max(dt$index)) {
        cur_max <- max(dt$index)
        num_obj <- length(unique(dt$id))
        miss <- dt[J(rep(1L, min_fields - cur_max)), on = "index", allow.cartesian = TRUE][
            , index := seq(cur_max + 1L, min_fields), by = "id"]
        set(miss, NULL, c("field", "value"), NA_character_)
        dt <- rbindlist(list(dt, miss))
    }

    setindexv(dt, c("index"))
    setindexv(dt, c("id"))
    setindexv(dt, c("id", "index"))
    if (!is.null(names(class))) set(dt, NULL, "class", names(class))

    trans_action_dt(dt, ...)
}
# }}}
# trans_action_dt {{{
trans_action_dt <- function (dt, ...) {
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
                        assert_count(num, .var.name = "step for row insertion")
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

                as.integer(index + length(content[[1L]]) *
                    rep(seq_len(num), each = if (content[[3L]]) content[[3L]] else 1L)
                )

            }, by = "id"]
        } else if (action == "delete") {
            if (length(content) < 2L) {
                dt <- dt[!J(content[[1L]]), on = "index"]
            } else {
                dt <- dt[index < content[[1L]] | (index - content[[1L]]) %% content[[2L]] != 0L]
            }
        }

        new <- rbindlist(list(new, new1), use.names = TRUE)
    }

    new_dt <- rbindlist(list(dt, new), use.names = TRUE)
    setorderv(new_dt, c("id", "index"))
    setindexv(new_dt, c("index"))
    setindexv(new_dt, c("id"))
    setindexv(new_dt, c("id", "index"))
    setorderv(new_dt, c("id", "index"))
    new_dt
}
# }}}
# trans_upper_versions {{{
trans_upper_versions <- function (idf, ver, patch = FALSE) {
    # get all versions needed to handle
    all_vers <- standardize_ver(ALL_IDD_VER)
    vers <- all_vers[all_vers >= idf$version() & all_vers <= standardize_ver(ver)]

    # remove patch version
    if (length(vers) & !patch) {
        vers <- vers[!duplicated(as.character(vers[, 1:2]))]
    }

    vers
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
# trans_process_load {{{
trans_process_load <- function (new_idf, old_idf, dt, empty = TRUE) {
    if (!nrow(dt)) return(new_idf)

    # get object table from old input
    old <- get_priv_env(old_idf)$idf_env()$object[J(unique(dt$id)), on = "object_id", nomatch = 0L]

    # get object table before inserting new objects
    new_before <- get_priv_env(new_idf)$idf_env()$object

    # insert new objects
    new_idf$load(dt, .unique = FALSE, .default = FALSE, .empty = empty)

    if (is.null(unlist(old$comment, use.names = FALSE))) return(new_idf)

    # update
    input <- dt[, list(rleid = .GRP), by = list(object_id = id, class)]
    input[old, on = "object_id", comment := i.comment]

    get_priv_env(new_idf)$idf_env()$object[
        !new_before, on = "object_id", `:=`(comment = {
            if (.N == nrow(input)) {
                if (.N == 1L) {
                    list(input$comment)
                } else {
                    input$comment
                }
            } else {
                warn(
                    paste0("Failed to preserve comments of objects involved during transition ",
                        "from ", old_idf$version()[, 1:2], " to ", new_idf$version()[, 1:2], ". ",
                        "Comments of objects below will be removed:\n",
                        get_object_info(.SD, c("name", "id"), collapse = "\n")
                    ),
                    paste0("warning_trans_",
                        gsub(".", "", as.character(old_idf$version()), fixed = TRUE), "_",
                        gsub(".", "", as.character(new_idf$version()), fixed = TRUE))

                )
                list(comment)
            }
    })]

    new_idf
}
# }}}
# trans_table_convert {{{
trans_table_convert <- function (path, ascending = c(TRUE, TRUE)) {
    vars <- fread(path, nrows = 1, header = FALSE)
    val_vars <- lapply(seq.int(vars$V1), function (row) unlist(fread(path, nrows = 1, skip = row)))

    # reorder if necessary
    if (any(!ascending) && vars$V1 >= 2L) {
        val_vars <- apply2(val_vars, ascending, function (val, asc) if (!asc) rev(val) else val)
    }

    # check consistency between value number specified and found
    num_vals <- viapply(val_vars, length)
    if (any(mismatch <- (num_vals != unlist(vars[, -"V1"])))) {
        invld <- unlist(vars[, .SD, .SDcols = setdiff(names(vars), "V1")[mismatch]])
        abort(paste0("Number of independent variable values found mismatches with description in header:\n",
            "  #", which(mismatch), "| ", invld, " specified in header but ",
            num_vals[mismatch], " values found"
        ))
    }

    # get cross joined table
    dt_vars <- do.call(data.table::CJ, rev(val_vars))
    # change column order
    setcolorder(dt_vars, rev(names(dt_vars)))
    # rename columns
    setnames(dt_vars, paste0("var", seq.int(ncol(dt_vars))))

    # read tabular data and change it to one row matrix
    table <- matrix(t(as.matrix(fread(path, skip = 1 + num_vals, header = FALSE))), nrow = 1L)
    # add value column
    dt_vars[, value := as.vector(table)]

    # change row order
    setorderv(dt_vars, setdiff(names(dt_vars), "value"))
}
# }}}

#' Run IDFVersionUpdater to Update Model Versions
#'
#' `version_updater()` is a wrapper of IDFVersionUpdater preprocessor
#' distributed with EnergyPlus. It takes a path of IDF file or an [Idf] object,
#' a target version to update to and a directory to save the new models.
#'
#' An attribute named `errors` is attached which is a list of
#' [ErrFiles][read_err()] that contain all error messages from transition error
#' (.VCpErr) files.
#'
#' @inheritParams transition
#' @param dir The directory to save the new IDF files. If the directory does not
#' exist, it will be created before save. If `NULL`, the directory of input
#' [Idf] object or IDF file will be used. Default: `NULL`.
#' @return An [Idf] object if `keep_all` is `FALSE` or a list of [Idf] objects
#' if `keep_all` is `TRUE`. An attribute named `errors` is attached which
#' contains all error messages from transition error (.VCpErr) files.
#' @author Hongyuan Jia
#' @examples
#' \dontrun{
#' if (any(avail_eplus()) > 7.2) {
#'     # create an empty IDF
#'     idf <- empty_idf(7.2)
#'     idf$save(tempfile(fileext = ".idf"))
#'
#'     # convert it from v7.2 to the latest EnergyPlus installed
#'     updated <- version_updater(idf, max(avail_eplus()))
#'
#'     # convert it from v7.2 to the latest EnergyPlus installed and keep all
#'     # intermediate versions
#'     updated <- version_updater(idf, max(avail_eplus()), keep_all = TRUE)
#'
#'     # see transition error messages
#'     attr(updated, "errors")
#' }
#' }
#' @export
# version_updater {{{
version_updater <- function (idf, ver, dir = NULL, keep_all = FALSE) {
    # parse file
    if (!is_idf(idf)) idf <- read_idf(idf)

    if (length(ver) != 1L || is.na(ver <- convert_to_idd_ver(ver))) {
        abort("'ver' must be a valid EnergyPlus IDD version")
    }

    # save the model to the output dir if necessary
    if (is.null(idf$path()) || !utils::file_test("-f", idf$path())) {
        abort(paste0("The Idf object is not created from local file or local file has ",
                "been deleted from disk. Please save Idf using '$save()' before transition."
        ), "idf_not_local")
    }

    # stop if unsaved
    if (idf$is_unsaved()) {
        abort("Idf has been modified since read or last saved. Please save Idf using '$save()' before transition.")
    }

    if (is.null(dir)) {
        dir <- dirname(idf$path())
    } else if (!dir.exists(dir)){
        dir.create(dir, recursive = TRUE)
    }

    # skip if input is already at the specified version
    if (idf$version()[, 1:2] == ver[, 1:2]) {
        verbose_info("IDF is already at latest version ", ver, ". No transition needed.")
        if (keep_all) {
            res <- list(idf)
            setattr(res, "names", as.character(idf$version()[, 1L:2L]))
            return(res)
        } else {
            return(idf)
        }
    }

    latest_ver <- avail_eplus()[avail_eplus()[, 1:2] >= ver[, 1:2]]
    if (!length(latest_ver)) {
        abort(paste0("EnergyPlus v", ver, " or newer are not installed."))
    }

    # save the original file with trailing version number
    original <- paste0(tools::file_path_sans_ext(basename(idf$path())), "V", idf$version()[, 1L], idf$version()[, 2L], "0.idf")
    # clone original
    idf <- idf$clone(TRUE)
    idf$save(file.path(dir, original), overwrite = TRUE)

    # get the directory of IDFVersionUpdater
    # avoid to use IDFVersionUpdater v9.0 as there are fital errors
    if (length(latest_ver[latest_ver[, 1:2] != 9.0])) latest_ver <- latest_ver[latest_ver[, 1:2] != 9.0]
    path_updater <- file.path(eplus_config(max(latest_ver))$dir, "PreProcess/IDFVersionUpdater")
    verbose_info("IDFVersionUpdater: ", normalizePath(path_updater, mustWork = FALSE))

    # get upper versions toward target version
    vers <- trans_upper_versions(idf, ver)

    # get fun names
    exe <- if (is_windows()) ".exe" else NULL
    from <- vers[-length(vers)]
    to <- vers[-1L]
    trans_exe <- paste0("Transition-",
        "V", from[, 1L], "-", from[, 2L], "-0", "-to-",
        "V",   to[, 1L], "-",   to[, 2L], "-0", exe
    )

    # results
    models <- vector("list", 1L + length(to))
    names(models) <- as.character(c(idf$version()[, 1:2], to[, 1:2]))

    # paths
    paths <- character(1L + length(to))
    names(paths) <- names(models)

    # errors
    errors <- vector("list", 1L + length(to))
    names(errors) <- names(models)

    while (idf$version()[, 1:2] != max(to)[, 1:2]) {
        # restore paths
        paths[names(paths) == as.character(idf$version()[, 1:2])] <- idf$path()

        # restore models
        models[names(models) == as.character(idf$version()[, 1:2])] <- list(idf)

        current_exe <- trans_exe[from[, 1:2] == idf$version()[, 1:2]]
        toward <- to[from[, 1:2] == idf$version()[, 1:2]]

        verbose_info(
            "Input file: ", idf$path(), "\n",
            " From  Ver: ", idf$version(), "\n",
            "Toward Ver: ", toward
        )

        trans_path <- file.path(path_updater, current_exe)

        if (!file.exists(trans_path)) {
            abort(paste0("Transition executable ", surround(trans_path), " does not exist."))
        }

        job <- tryCatch(processx::run(trans_path, idf$path(), wd = path_updater),
            error = function (e) {
                if (grepl("System command error", conditionMessage(e))) {
                    abort(paste0("Failed to update file ", idf$path()," from V", idf$version(), " to V", toward, ":\n", conditionMessage(e)))
                } else {
                    stop(e)
                }
            }
        )

        if (job$status != 0L) {
            abort(paste0("Failed to update file ", idf$path()," from V", idf$version(), " to V", toward, "."))
        }

        verbose_info("[", idf$version(), " --> ", toward, "] SUCCEEDED.\n")

        # delete the new IDF file with old name since there is another new IDF file
        # with ".idfold" extenstion
        unlink(idf$path(), force = TRUE)

        # read error file
        path_err <- paste0(tools::file_path_sans_ext(idf$path()), ".VCpErr")
        # in case VersionUpdater crashed
        if (!file.exists(path_err)) {
            err <- data.table()
        } else {
            err <- read_err(path_err)
            # remove VCpErr file generated
            unlink(path_err, force = TRUE)
        }

        # rename the old file
        file.rename(paste0(tools::file_path_sans_ext(idf$path()), ".idfold"), idf$path())

        # name of the new file
        path_new <- paste0(tools::file_path_sans_ext(idf$path()), ".idfnew")
        # replace the old Idf object
        idf <- read_idf(path_new)

        # resave using eplusr
        new_name <- paste0(stri_sub(tools::file_path_sans_ext(path_new), to = -4L), toward[, 1L], toward[, 2L], "0.idf")
        idf$save(new_name, overwrite = TRUE)
        # rename the orignal new file
        unlink(path_new, force = TRUE)

        # remove log file generated
        unlink(file.path(path_updater, c("fort.6", "Energy+.ini", "Transition.audit")), force = TRUE)

        # restore paths
        paths[names(paths) == as.character(idf$version()[, 1:2])] <- idf$path()

        # restore models
        models[names(models) == as.character(idf$version()[, 1:2])] <- list(idf)

        # restore errors
        errors[names(errors) == as.character(idf$version()[, 1:2])] <- list(err)
    }

    if (!keep_all) {
        unlink(paths[-length(paths)], force = TRUE)
        models <- models[[length(models)]]
    }

    attr(models, "errors") <- errors
    models
}
# }}}
