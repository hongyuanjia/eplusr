#' @importFrom purrr pmap

# trans_idf {{{
trans_idf <- function (idf, ver) {
    assert_that(is_model(idf))
    cur_ver <- as.character(idf$version)

    targ_ver <- as.character(ver)
    assert_that(is_eplus_ver(ver))
    # Currently only support pre-parsed version
    assert_that(is_pre_parsed(ver))

    if (identical(targ_ver, cur_ver)) {
        stop(msg(sprintf("File is already at the same version of %s. No trans
                         made.", targ_ver)), call. = FALSE)
    }

    # if input is an imf file, give a warning
    if (inherits(idf, "IMF")) {
        warning(msg("Note: IMF file being processed.  No guarantee of
                    perfection. Please check new file carefully."),
                    call. = FALSE)
    }

    rules <- get_trans_rule(cur_ver, targ_ver)

    new_idf <- idf

    for (i in seq_along(rules)) {
        rule <- rules[[i]]
        new_idd <- link_idd(rule$to)
        new_idf <- trans_idf_from_rule(new_idf, new_idd, rule)
        new_idf <- update_idd_data(new_idf, new_idd)
    }

    return(new_idf)
}
# }}}
# trans_idf_from_rule {{{
trans_idf_from_rule <- function (idf, new_idd, rule) {
    new_idf <- delete_obsolete_obj(idf, rule$dict_delete_obsolete)

    for (i in seq_along(rule$dict_change_value)) {
        arg <- rule$dict_change_value[[i]]
        new_idf <- change_field_value(new_idf,
            class = arg$class, field_order = arg$field_order,
            old = arg$old, new = arg$new)
    }

    for (i in seq_along(rule$dict_insert_field)) {
        arg <- rule$dict_insert_field[[i]]
        new_idf <- insert_obj_field(new_idf,
            class = arg$class, field_order = arg$field_order, value = arg$value)
    }

    for (i in seq_along(rule$dict_remove_field)) {
        arg <- rule$dict_remove_field[[i]]
        new_idf <- remove_obj_field(new_idf, class = arg$class, field_order = arg$field_order)
    }

    for (i in seq_along(rule$dict_rename_class)) {
        arg <- rule$dict_rename_class[[i]]
        new_idf <- rename_class_name(new_idf, old = arg$old, new = arg$new)
    }

    for (i in seq_along(rule$special)) {
        new_idf <- rule$special[[i]](new_idf, new_idd)
    }

    return(new_idf)
}
# }}}

# append_trans_log {{{
append_trans_log <- function (idf, trans_log) {
    setattr(idf, "trans_log", c(attr(idf, "trans_log"), trans_log))

    return(idf)
}
# }}}
# delete_obsolete_obj {{{
delete_obsolete_obj <- function (idf, dict) {

    class_all <- idf$class[, unique(class)]
    class_deleted <- dict[dict %in% class_all]

    if (not_empty(class_deleted)) {
        idf$class <- idf$class[!class %in% class_deleted]
        idf$value <- idf$value[!class %in% class_deleted]
        ids <- idf$class[class %in% class_deleted, object_id]
        idf$comment <- idf$comment[!object_id %in% ids]
        idf <- append_id_del(idf, ids)
    }

    return(idf)
}
# }}}
# change_field_value {{{
change_field_value <- function (idf, class, field_order, old = NULL, new, append = FALSE) {
    if (!is_valid_class(class, idf)) return(idf)
    targ_class_name <- class
    targ_field_order <- field_order

    if (is_empty(old)) {
        if (append) {
            idf$value[class == targ_class_name & field_order ==  targ_field_order,
                      value := paste0(value, new)]
        } else {
            idf$value[class == targ_class_name & field_order ==  targ_field_order,
                      value := new]
        }
    } else {
        idf$value[class == targ_class_name & field_order ==  targ_field_order &
                  toupper(value) == toupper(old), value := new]
    }

    return(idf)
}
# }}}
# insert_obj_field {{{
insert_obj_field <- function (idf, class, field_order, value = "") {
    if (!is_valid_class(class, idf)) return(idf)
    targ_class_name <- class
    targ_field_order <- field_order
    new_value <- value
    num_fields <- idf$value[class == targ_class_name, list(num_fields = .N), by = list(object_id)]

    # get objects that have equal or more fields than target field order
    targ_id <- num_fields[num_fields >= targ_field_order, object_id]

    # add the new field
    new_field <- idf$value[class == targ_class_name & object_id %in% targ_id & field_order == 1L][,
        `:=`(field_order = targ_field_order, value = new_value, edited = 2L)]
    # update field order
    idf$value[class == targ_class_name & object_id %in% targ_id & field_order >= targ_field_order,
        field_order := field_order + 1L]
    # combine
    idf$value <- rbindlist(list(idf$value, new_field))

    return(idf)
}
# }}}
# remove_obj_field {{{
remove_obj_field <- function (idf, class, field_order) {
    if (!is_valid_class(class, idf)) return(idf)
    targ_class_name <- class
    targ_field_order <- field_order

    idf$value[field_order != targ_field_order][
        field_order >= targ_field_order, field_order := field_order - 1L]

    return(idf)
}
# }}}
# rename_class_name {{{
rename_class_name <- function (idf, old, new) {
    idf$class <- idf$class[class == old, class := new]
    idf$value <- idf$value[toupper(value) == toupper(old), value := new]

    return(idf)
}
# }}}
# delete_obsolete_repvar {{{
remove_obsolete_repvar <- function (idf, variable) {
    if (is.null(variable)) return(idf)

    targ_class_name_1 <- c(
        "Output:Meter", "Output:Meter:MeterFileOnly",
        "Output:Meter:Cumulative", "Output:Meter:Cumulative:MeterFileOnly")
    targ_id_1 <- idf$class[class %in% targ_class_name &
        field_order == 1L & toupper(value) == toupper(variable), object_id]

    targ_class_name_2 <- c(
        "Output:Table:TimeBins",
        "ExternalInterface:FunctionalMockupUnitImport:From:Variable",
        "ExternalInterface:FunctionalMockupUnitExport:From:Variable")
    targ_id_2 <- idf$class[class %in% targ_class_name &
        field_order == 2L & toupper(value) == toupper(variable), object_id]

    targ_class_name_3 <- c(
        "EnergyManagementSystem:Sensor")
    targ_id_3 <- idf$class[class %in% targ_class_name &
        field_order == 3L & toupper(value) == toupper(variable), object_id]

    targ_id <- unique(c(targ_id_1, targ_id_2, targ_id_3))

    if (is_empty(targ_id)) return(idf)

    idf$class <- idf$class[!object_id %in% targ_id]
    idf$value <- idf$value[!object_id %in% targ_id]
    idf$comment <- idf$comment[!object_id %in% targ_id]
    idf <- append_id_del(idf, targ_id)

    return(idf)
}
# }}}
# update_idd_data {{{
update_idd_data <- function (idf, new_idd) {
    # only idf data columns and key columns
    keys_class <- c("group", "class")
    keys_value <- c("class", "field_order")
    cols_class <- c(keys_class, "object_id", "edited")
    cols_value <- c(keys_value, "object_id", "value", "edited")
    cols_class_all <- names(idf$class)
    cols_value_all <- names(idf$value)

    targ_class <- idf$class[, .SD, .SDcol = cols_class]
    targ_value <- idf$value[, .SD, .SDcol = cols_value]

    # merge
    idf$class <- new_idd$class[targ_class, on = c(keys_class)][
        , .SD, .SDcol = names(idf$class)]
    idf$value <- new_idd$field[targ_value, on = c(keys_value)][
        , .SD, .SDcol = names(idf$value)]

    # combine
    setcolorder(idf$class, cols_class_all)
    setcolorder(idf$value, cols_value_all)
    setorder(idf$class, group_order, class_order, object_id)
    setorder(idf$value, class_order, object_id, field_order)

    idf$ref <- get_obj_ref(idf$value, new_idd)
    idf$version <- substr(new_idd$version, 1L, 3L)

    setattr(idf, "id", idf$class[, unique(object_id)])

    return(idf)
}
# }}}

    refpt <- "Daylighting:DELight:ReferencePoint"
    delgt_ctrl <- "Daylighting:DELight:Controls"
# special_8.5_8.6_sglductvav {{{
special_8.5_8.6_sglductvav <- function (idf, new_idd) {
    targ_id <- idf$value[class == "AirTerminal:SingleDuct:VAV:Reheat" &
        field_order == 16L & toupper(value) == "REVERSE", object_id]

    if (is_empty(targ_id)) return(idf)

    id_cond_1 <- idf$value[object_id %in% targ_id & field_order == 17 & value != "", object_id]
    id_cond_2 <- idf$value[object_id %in% targ_id & field_order == 18 & value != "", object_id]

    ids <- unique(id_cond_1, id_cond_2)

    idf$value <- idf$value[object_id %in% ids & field_order == 16L , value := "ReverseWithLimits"]

    return(idf)
}
# }}}
# special_8.5_8.6_refpt {{{
special_8.5_8.6_refpt <- function (idf, new_idd) {
    lgt_ctrl <- "Daylighting:Controls"
    if (is_valid_class(lgt_ctrl, idf)) return(idf)

    # set the second value to the first value
    idf$value[class == lgt_ctrl & field_order == 2L,
        value := {idf$value[class == lgt_ctrl & field_order == 1L, value]}]

    # add a suffix to the value of first field
    idf$value[class == lgt_ctrl & field_order == 1L, value := paste0(value, "_DaylCtrl")]

    # change the third value
    idf$value[class == lgt_ctrl & field_order == 3L, value := "SplitFlux"]

    # set the forth value to the 20th value
    idf$value[class == lgt_ctrl & field_order == 4L,
        value := {idf$value[class == lgt_ctrl & field_order == 20L, value]}]

    # set the fifth value according to the value of 13th value
    value_13 <- idf$value[class == lgt_ctrl & field_order == 13L, list(value)][
        , list(out_5 = switch(value, "1" = "Continuous", "2" = "Stepped", "3" = "ContinuousOff", ""))]

    idf$value[class == lgt_ctrl & field_order == 5L,
        value := {
              }]
    value_13 <- idf$value[]
    idf <- insert_obj_field(idf, class = lgt_ctrl, )

    idd_8.5$field[class == "Daylighting:Controls"]
    idd_8.6$field[class == "Daylighting:Controls"]
    # get all "Daylighting:Controls" objects
    targ_obj <- idf$value[class == lgt_ctrl]

    if (is_valid_class(refpt, idf)) return(idf)

    targ_id <- get_id(idf, targ_class_name)

    return(idf)
}
# }}}
# special_8.5_8.6_branch {{{
special_8.5_8.6_branch <- function (idf, new_idd) {
    idf$value[, row_id := .I]
    targ_row <- idf$value[class == "Branch"][
        field_order == 2L | ((field_order - 3L) %% 5L == 0L), row_id]
    idf$value[, row_id := NULL]

    if (is_empty(targ_row)) return(idf)

    idf$value <- idf$value[!row_id %in% targ_row][
        class == "Branch", field_order := seq_along(.I), by = list(object_id)]

    return(idf)
}
# }}}

# special_8.6_8.7_airflowduct {{{
special_8.6_8.7_airflowduct <- function (idf, new_idd) {
    targ_class_name <- "AirflowNetwork:Distribution:Component:Duct"

    if (!is_valid_class(targ_class_name, idf)) return(idf)

    targ_id <- get_id(idf, targ_class_name)

    AFNDuctFracRcond <- 0.815384615
    AFNDuctFracRout <- 0.153846154
    AFNDuctFracRin <- 0.030769231

    AFNDuctUVal <- idf$value[object_id %in% targ_id & field_order == 7L, as.numeric(value)]

    idf$value <- idf$value[
        object_id %in% targ_id & field_order == 7L,
        value := sprintf(AFNDuctUVal / AFNDuctFracRcond, "%f")][

        object_id %in% targ_id & field_order == 9L,
        value := sprintf(AFNDuctUVal / AFNDuctFracRout, "%f")][

        object_id %in% targ_id & field_order == 10L,
        value := sprintf(AFNDuctUVal / AFNDuctFracRin, "%f")]

    return(idf)
}
# }}}

# special_8.7_8.8_foundation {{{
special_8.7_8.8_foundation <- function (idf, new_idd) {

    targ_id_surf <- idf$value[class == "BuildingSurface:Detailed"][
        field_order == 5L & toupper(value) == "FOUNDATION"][
        field_order == 2L & toupper(value) == "FLOOR", object_id]

    targ_id_flr <- idf$value[class == "Floor:Detailed"][
        field_order == 4L & toupper(value) == "FOUNDATION", object_id]

    targ_id <- c(targ_id_surf, targ_id_flr)

    if (is_empty(targ_id)) return(idf)

    nms <- idf$value[object_id %in% targ_id & field_order == 1L,
        value, by = list(object_id)]

    vert <- idf$value[object_id %in% targ_id,
        list(num_vert = (max(field_order) - ifelse(class == "BuildingSurface:Detailed", 10L, 9L) / 3L)), by = list(object_id)]

    comb_vert <- nms[vert, on = "object_id"]

    found <- idf$value[class == "SurfaceProperty:ExposedFoundationPerimeter" &
        field_order == 1L, value]

    mis_found <- comb_vert[!tolower(value) %in% found]

    if (is_empty(mis_found)) {
        return(idf)
    } else {
        mis_found[, `:=`(object_id = NULL)][, `:=`(new_id = .I + max_id(idf))]

        # TODO: handle \extensible:<#>
        new_class <- extract_class(class = "SurfaceProperty:ExposedFoundationPerimeter", idd = new_idd)
        new_value <- extract_object(class = "SurfaceProperty:ExposedFoundationPerimeter", min = FALSE, idd = new_idd)[
           field_order <= 4L, `:=`(value = c("", "BySegment", "", ""))]

        data_value <- rbindlist(pmap(mis_found,
            function (floor, num, new_id) {
                new_value[field_order <= num + 4L][
                    field_order == 1L, value := floor][
                    field_order > 4L, value := "Yes"][
                    , `:=`(object_id = new_id, edited = 2L)]
            })
        )[, .SD, .SDcol = names(idf$value)]

        data_class <- rbindlist(replicate(mis_found[, .N], new_class, FALSE))[
            , `:=`(object_id = mis_found$new_id, edited = 2L)][
            , .SD, .SDcol = names(idf$class)]

        idf$class <- rbindlist(list(idf$class, data_class))
        idf$value <- rbindlist(list(idf$value, data_value))
        setorder(idf$class, group_order, class_order, object_id)
        setorder(idf$value, class_order, object_id, field_order)

        idf$ref <- get_obj_ref(idf$value, new_idd)
    }

    return(idf)
}
# }}}
# special_8.7_8.8_winblind {{{
special_8.7_8.8_winblind <- function (idf, new_idd) {
    idf$value <- idf$value[class == "WindowMaterial:Blind:EquivalentLayer" &
        field_order == 6L,
        value := ifelse(as.numeric(value) >= 90L, as.character(90L - as.numeric(value)), value)]

    return(idf)
}
# }}}

# dict_delete_obsolete {{{
dict_delete_obsolete = c(
    "ProgramControl", "Sky Radiance Distribution", "Airflow Model",
    "Generator:FC:Battery Data", "AirflowNetwork:MultiZone:SiteWindConditions",
    "Water Heater:Simple")
# }}}
# rule_8.5_8.6 {{{
rule_8.5_8.6 <- list(
    from = "8.5", to = "8.6",

    dict_delete_obsolete = dict_delete_obsolete,

    dict_insert_field = list(
        list(class = "AirTerminal:SingleDuct:InletSideMixer", field_order = 7L, value = "InletSide"),
        list(class = "AirTerminal:SingleDuct:SupplySideMixer", field_order = 7L, value = "SupplySide"),
        list(class = "OtherEquipment", field_order = 2L, value = "None"),
        list(class = "Coil:Heating:Gas", field_order = 3L, value = "NaturalGas")
    ),

    dict_remove_field = list(
        list(class = "HVACTemplate:System:UnitarySystem", field_order = 57L),
        list(class = "HVACTemplate:System:Unitary", field_order = 40L),
        list(class = "ChillerHeater:Absorption:DirectFired", field_order = 33L),
        list(class = "SetpointManager:SingleZone:Humidity:Minimum", field_order = 2L),
        list(class = "SetpointManager:SingleZone:Humidity:Minimum", field_order = 3L),
        list(class = "SetpointManager:SingleZone:Humidity:Maximum", field_order = 2L),
        list(class = "SetpointManager:SingleZone:Humidity:Maximum", field_order = 3L)
    ),

    dict_change_value = list(
        list(class = "Version", field_order = 1L, old = "8.5", new = "8.6"),
        list(class = "Output:Variable", field_order = 1L, old = "", new = "*"),
        list(class = "Output:Table:TimeBins", field_order = 1L, old = "", new = "*"),
        list(class = "ExternalInterface:FunctionalMockupUnitImport:From:Variable", field_order = 1L, old = "", new = "*"),
        list(class = "ExternalInterface:FunctionalMockupUnitExport:From:Variable", field_order = 1L, old = "", new = "*"),
        list(class = "Exterior:FuelEquipment", field_order = 2L, old = "Gas", new = "NaturalGas"),
        list(class = "Exterior:FuelEquipment", field_order = 2L, old = "LPG", new = "PropaneGas"),
        list(class = "ZoneHVAC:AirDistributionUnit", field_order = 3L,
             old = "AirTerminal:SingleDuct:InletSideMixer", new = "AirTerminal:SingleDuct:Mixer"),
        list(class = "ZoneHVAC:AirDistributionUnit", field_order = 3L,
             old = "AirTerminal:SingleDuct:SupplySideMixer", new = "AirTerminal:SingleDuct:Mixer"),
        list(class = "EnergyManagementSystem:Actuator", field_order = 4L,
             old = "Outdoor Air Dryblub Temperature", new = "Outdoor Air Drybulb Temperature"),
        list(class = "EnergyManagementSystem:Actuator", field_order = 4L,
             old = "Outdoor Air Wetblub Temperature", new = "Outdoor Air Wetbulb Temperature")
    ),

    dict_rename_class = list(
        list(old = "Coil:Heating:Gas", new = "Coil:Heating:Fuel"),
        list(old = "AirTerminal:SingleDuct:InletSideMixer", new = "AirTerminal:SingleDuct:Mixer"),
        list(old = "AirTerminal:SingleDuct:SupplySideMixer", new = "AirTerminal:SingleDuct:Mixer")
    ),

    special = list(airflowduct = special_8.6_8.7_airflowduct)
)
# }}}
# rule_8.6_8.7 {{{
rule_8.6_8.7 <- list(
    from = "8.6", to = "8.7",

    dict_delete_obsolete = dict_delete_obsolete,

    dict_insert_field = list(
        list(class = "CoolingTower:SingleSpeed"                 , field_order = 17L, value = "")          ,
        list(class = "CoolingTower:SingleSpeed"                 , field_order = 18L, value = "")          ,
        list(class = "CoolingTower:SingleSpeed"                 , field_order = 19L, value = "")          ,
        list(class = "CoolingTower:SingleSpeed"                 , field_order = 20L, value = "")          ,
        list(class = "CoolingTower:TwoSpeed"                    , field_order = 25L, value = "")          ,
        list(class = "CoolingTower:TwoSpeed"                    , field_order = 26L, value = "")          ,
        list(class = "CoolingTower:TwoSpeed"                    , field_order = 27L, value = "")          ,
        list(class = "CoolingTower:TwoSpeed"                    , field_order = 28L, value = "")          ,
        list(class = "CoolingTower:VariableSpeed:Merkel"        , field_order = 25L, value = "")          ,
        list(class = "CoolingTower:VariableSpeed:Merkel"        , field_order = 26L, value = "")          ,
        list(class = "CoolingTower:VariableSpeed:Merkel"        , field_order = 27L, value = "")          ,
        list(class = "CoolingTower:VariableSpeed:Merkel"        , field_order = 28L, value = "")          ,
        list(class = "ZoneCapacitanceMultiplier:ResearchSpecial", field_order = 1L , value = "Multiplier"),
        list(class = "ZoneCapacitanceMultiplier:ResearchSpecial", field_order = 2L , value = "")
    ),

    dict_remove_field = list(
        list(class = "AirflowNetwork:SimulationControl", field_order = 4L)
    ),

    dict_change_value = list(
        list(class = "Version", field_order = 1L, old = "8.6", new = "8.7"),
        list(class = "Output:Variable", field_order = 1L, old = "", new = "*"),
        list(class = "Output:Table:TimeBins", field_order = 1L, old = "", new = "*"),
        list(class = "ExternalInterface:FunctionalMockupUnitImport:From:Variable", field_order = 1L, old = "", new = "*"),
        list(class = "ExternalInterface:FunctionalMockupUnitExport:From:Variable", field_order = 1L, old = "", new = "*"),
        list(class = "Coil:Cooling:DX:MultiSpeed", field_order = 16L, old = "", new = "NaturalGas"),
        list(class = "Coil:Cooling:DX:MultiSpeed", field_order = 16L, old = "PropaneGas", new = "Propane"),
        list(class = "WaterHeater:HeatPump:WrappedCondenser", field_order = 35L,
             old = "MutuallyExlcusive", new = "MutuallyExclusive")
    ),

    special = list(airflowduct = special_8.6_8.7_airflowduct)
)
# }}}
# rule_8.7_8.8 {{{
rule_8.7_8.8 <- list(
    from = "8.7", to = "8.8",

    dict_delete_obsolete = dict_delete_obsolete,

    dict_insert_field = list(
        list(class = "Table:TwoIndependentVariables"                   , field_order = 14L , value = "")             ,
        list(class = "SurfaceProperty:ExposedFoundationPerimeter"      , field_order = 2L  , value = "BySegment")    ,
        list(class = "UnitarySystemPerformance:Multispeed"             , field_order = 5L  , value = "")             ,
        list(class = "Coil:Cooling:DX:SingleSpeed"                     , field_order = 15L , value = "")             ,
        list(class = "Coil:Cooling:DX:TwoSpeed"                        , field_order = 23L , value = "")             ,
        list(class = "Coil:Cooling:DX:MultiSpeed"                      , field_order = 7L  , value = "")             ,
        list(class = "Coil:Cooling:DX:VariableSpeed"                   , field_order = 16L , value = "")             ,
        list(class = "Coil:Cooling:DX:TwoStageWithHumidityControlMode" , field_order = 19L , value = "")             ,
        list(class = "ZoneHVAC:IdealLoadsAirSystem"                    , field_order = 5L  , value = "")             ,
        list(class = "ZoneControl:ContaminantController"               , field_order = 6L  , value = "")             ,
        list(class = "AvailabilityManager:NightCycle"                  , field_order = 6L  , value = "FixedRunTime")
    ),

    dict_remove_field = list(
        list(class = "ZoneHVAC:PackagedTerminalHeatPump", field_order = 18L)
    ),

    dict_change_value = list(
        list(class = "Version", field_order = 1L, old = "8.7", new = "8.8"),
        list(class = "Output:Variable", field_order = 1L, old = "", new = "*"),
        list(class = "Output:Table:TimeBins", field_order = 1L, old = "", new = "*"),
        list(class = "ExternalInterface:FunctionalMockupUnitImport:From:Variable", field_order = 1L, old = "", new = "*"),
        list(class = "ExternalInterface:FunctionalMockupUnitExport:From:Variable", field_order = 1L, old = "", new = "*"),
        list(class = "Foundation:Kiva:Settings", field_order = 8L, old = "Autocalculate", new = "Autoselect"),
        list(class = "Output:Surfaces:List", field_order = 1L,
             old = "DecayCurvesfromZoneComponentLoads", new = "DecayCurvesFromComponentLoadsSummary")
    ),

    special = list(foundation = special_8.7_8.8_foundation,
                   winblind = special_8.7_8.8_winblind)
)
# }}}
# rules {{{
rules <- list(
    rule_8.5_8.6 = rule_8.5_8.6,
    rule_8.6_8.7 = rule_8.6_8.7,
    rule_8.7_8.8 = rule_8.7_8.8
)
# }}}
# get_trans_rule {{{
get_trans_rule <- function (from, to) {
    assert_that(is_eplus_ver(from), is_eplus_ver(to))
    step <- if (from < to) 0.1 else -0.1
    span <- seq(as.double(from), as.double(to), by = step)
    from <- span[-length(span)]
    to <- span[-1L]
    rule_names <- paste0("rule_", from, "_", to)

    rule_all <- names(rules)
    rule_avail <- rule_names[rule_names %in% rule_all]
    if (!identical(rule_names, rule_avail)) {
        rule_missing <- setdiff(rule_names, rule_avail)
        rule_missing_from <- substr(rule_missing, 6L, 8L)
        rule_missing_to <- substr(rule_missing, 10L, 12L)
        mes <- paste0("Missing trans rule from version '", rule_missing_from,
                      "' to '", rule_missing_to, "'.")
        stop(paste(mes, collapse = "\n"), call. = FALSE)
    }

    return(rules[rule_avail])
}
# }}}
