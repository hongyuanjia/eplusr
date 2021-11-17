# nocov start
# VERSIONS {{{
ALL_EPLUS_VER <- c(
    paste0("7.", 0:2, ".0"),
    paste0("8.", 0:9, ".0"), paste0("8.3.", 1:3),
    paste0("9.0.", 0:1), paste0("9.", 1:6, ".0")
)

LATEST_EPLUS_VER <- ALL_EPLUS_VER[length(ALL_EPLUS_VER)]

ALL_IDD_VER <- c(
    paste0("1.0.", 0:3),
    paste0("1.1.", 0:1),
    paste0("1.2.", 0:3),
    paste0("1.", 3:4, ".0"),
    paste0("2.", 0:2, ".0"),
    paste0("3.", 0:1, ".0"),
    paste0(4:6, ".0.0"),
    paste0("7.", 0:2, ".0"),
    paste0("8.", 0:9, ".0"),
    paste0("9.0.", 0:1),
    paste0("9.", 1:6, ".0")
)

ALL_EPLUS_RELEASE_COMMIT <- data.table::fread(
    "version , commit
     9.6.0   , f420c06a69
     9.5.0   , de239b2e5f
     9.4.0   , 998c4b761e
     9.3.0   , baff08990c
     9.2.0   , 921312fa1d
     9.1.0   , 08d2e308bb
     9.0.1   , bb7ca4f0da
     9.0.0   , 2ef880da82
     8.9.0   , 40101eaafd
     8.8.0   , 7c3bbe4830
     8.7.0   , 78a111df4a
     8.6.0   , 198c6a3cff
     8.5.0   , c87e61b44b
     8.4.0   , 832e4bb9cb
     8.3.0   , 6d97d074ea
    "
)
# }}}
# MACRO_DICT {{{
MACRO_DICT <-
      # Incorporating external files
    c("##include", "##fileprefix", "##includesilent", "##nosilent",
      # Selective accepting or skipping lins
      "##if", "##ifdef", "##ifndef", "##elseif", "##else", "##endif",
      # Defining blocks of input
      "##def", "##enddef", "##def1", "##set1",
      # Arithmetic operations
      "#eval", "#\\[",
      # Marco debugging and listing
      "##list", "##nolist", "##show", "##noshow", "##showdetail", "##noshowdetail",
      # Marco debugging and listing
      "##expandcomment", "##traceback", "##notraceback", "##write", "##nowrite",
      # Marco debugging and listing
      "##symboltable", "##clear", "##reverse", "##!")
# }}}
# init var{{{
`.` <- `..` <- `.GRP` <- `.I` <- `.N` <- `.SD` <- `.BY` <- `.EACHI` <- J <- N <- NULL

utils::globalVariables(c(
    "..", "J", "Date/Time", "Variable", "acceptable_num", "all_name_lower",
    "alpha", "annual", "autocalculatable", "autosizable", "azimuth_angle",
    "begin_extensible", "boundary_lower", "can_be_na", "check", "check_lower",
    "check_upper", "choice", "class_id", "class_name", "color", "color_int",
    "construction_name", "country", "data", "datetime", "day", "day_type",
    "ddy_name", "ddy_url", "default", "default_chr", "default_num",
    "design_day", "dir_relative_north", "each_rleid", "end", "end_day",
    "end_time", "envir", "envir_index", "epw", "epw_name", "epw_url",
    "excl_loc", "exist_maximum", "exist_maximum_l", "exist_minimum",
    "exist_minimum_u", "exit_status", "ext", "extensible",
    "extensible_field_index", "extensible_group", "field", "field_anid",
    "field_anid_an", "field_count", "field_id", "field_in", "field_index",
    "field_name", "field_name_noid", "field_num", "field_rleid",
    "first_extensible", "fmt", "found", "group", "group_id", "group_name",
    "has_any_na", "has_range", "header", "height", "height_width", "hour",
    "i.class_id", "i.color", "i.comment", "i.datetime", "i.day",
    "i.extensible_group", "i.field_id", "i.field_index", "i.field_name",
    "i.hour", "i.id", "i.key_value", "i.min_fields", "i.minute", "i.month",
    "i.name", "i.new_object_name_lower", "i.num_extensible", "i.object_id",
    "i.object_name", "i.object_name_lower", "i.object_order", "i.object_rleid",
    "i.origin_x", "i.origin_y", "i.origin_z", "i.outside_boundary_condition",
    "i.projection_axis", "i.required_field", "i.rleid", "i.src_value_chr",
    "i.src_value_id", "i.src_value_num", "i.subtype", "i.type",
    "i.unique_object_id", "i.value", "i.value_chr", "i.value_id", "i.value_num",
    "i.value_type", "i.x", "i.y", "i.z", "i.zone_name", "id", "id_list", "idf",
    "index", "index_str", "index_vertex", "info", "input_num", "ip", "ip_name",
    "ip_units", "is_all_na", "is_empty", "is_name", "is_num", "is_ref",
    "is_resource", "key_value", "last_extensible", "last_required", "latitude",
    "left_fields", "left_group", "level", "level_index", "level_num", "lhs_sgl",
    "line", "line_redudant", "line_s", "location", "longitude",
    "lower_incbounds", "matched", "max_fld", "maximum", "maximum_l",
    "mes_object", "min_fields", "min_required", "minimum", "minimum_u",
    "minite", "minute", "missing_num", "model", "msg", "n", "name",
    "new_object_name", "new_object_name_lower", "new_year", "num",
    "num_extensible", "num_extensible_group", "num_fields", "num_fld",
    "num_group", "obj_num", "object_id", "object_id_dup", "object_list",
    "object_name", "object_name_lower", "object_order", "object_rleid", "old",
    "old_exist", "old_min_fields", "origin_x", "origin_y", "origin_z", "out",
    "output_dir", "outside_boundary_condition",
    "outside_boundary_condition_object", "parent_surface_name", "patterns",
    "pointer", "prefix", "projected_x", "projected_y", "projection_axis",
    "reference", "reference_class_name", "removed", "reporting_frequency",
    "required_field", "required_object", "rev_field_rleid", "rhs_sgl",
    "row_index", "same_dir", "si", "si_name", "si_standard_name",
    "simulation_days", "slash", "slash_key", "slash_loc", "slash_value",
    "source_type", "space_loc", "spcl_loc", "src_class_id", "src_class_name",
    "src_enum", "src_field_id", "src_field_index", "src_field_name",
    "src_object_id", "src_object_name", "src_type_enum", "src_value_chr",
    "src_value_id", "src_value_num", "start", "start_day", "start_day_of_week",
    "starting_x_coordinate", "starting_y_coordinate", "starting_z_coordinate",
    "state_province", "status", "string", "sun_exposure", "sun_exposure_lower",
    "surface_type", "surface_type_int", "tilt_angle", "title", "type",
    "type_enum", "type_rleid", "unique_object", "unique_object_id",
    "update_value_reference", "upper_incbounds", "value", "value_chr",
    "value_count", "value_id", "value_lower", "value_num", "value_type",
    "variable", "weather", "wind_exposure", "wind_exposure_lower", "wmo_number",
    "x", "y", "z", "zone_name", "DAY_TYPE", "SIMULATION_DAYS", "daytype",
    "daytype_index", "i.daytype", "i.for", "i.interp", "i.interpolate",
    "i.pair_type", "i.through", "i.type_limits", "index_daytype",
    "index_schedule_value", "interpolate", "pair", "pair_type", "time",
    "time_rleid", "value_index", "year_day", "daytype_split", "i.daytype_split",
    "i.group_id", "int_idx", "base_surface_name", "building_surface_name",
    "category", "color_ext", "defaulted", "deg", "depth",
    "extensible_group_index", "first_vertex", "hole", "i.alpha", "i.area",
    "i.base_surface_name", "i.building_surface_name", "i.class", "i.color_ext",
    "i.color_int", "i.dir_relative_north", "i.ext", "i.int", "i.max_x",
    "i.max_y", "i.min_x", "i.min_y", "i.sun_exposure", "i.surface_type",
    "i.trans", "i.wind_exposure", "i.window_or_door_name", "left_depth",
    "left_distance_above", "left_distance_below", "left_extension",
    "left_tilt_angle", "max_x", "max_y", "min_x", "min_y", "misc", "new_class",
    "new_name", "rev_trans", "rev_x", "rev_y", "rev_z", "right_depth",
    "right_distance_above", "right_distance_below", "right_extension",
    "right_tilt_angle", "subtype", "trans", "window_or_door_name", "x0", "z0",
    "zone", "GlobalGeometryRules", "inv_x", "inv_y", "inv_z", paste0("V", 1:16),
    "area", "dt", "interval_type", "new_rleid", "time_index", "i.time_index",
    "i.environment_name", "i.environment_type", "i.simulation_index", "i.new",
    "value_lower_start", "expand_obj", "i.year", "provider", "energyplus_exe",
    "exist", "resources", "ver", "Name", "param_index"
))
# }}}
# nocov end
