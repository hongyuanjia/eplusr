# VERSIONS {{{
ALL_EPLUS_VER <- c(
    paste0("7.", 0:2, ".0"),
    paste0("8.", 0:9, ".0"), paste0("8.3.", 1:3),
    "9.0.0", "9.0.1", "9.1.0", "9.2.0"
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
    paste0("9.", 1:2, ".0")
)

ALL_EPLUS_RELEASE_COMMIT <- data.table::fread(
    "version , commit
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
`.` <- `.GRP` <- `.I` <- `.N` <- `.SD` <- `.BY` <- J <- N <- V1 <- V2 <- NULL

utils::globalVariables(c(
    "acceptable_num", "all_cmt", "all_name_lower", "annual", "auto_assigned",
    "autocalculatable", "autosizable", "begin_extensible",
    "can_be_na", "check", "check_lower", "check_upper", "choice", "class_id",
    "class_name", "colon_loc", "con", "contents", "copied", "country",
    "datetime", "datetime1", "day", "day_in", "ddy_name", "ddy_url", "default",
    "default_chr", "default_num", "defaulted", "dep", "depth", "design_day",
    "dis", "dot", "dot_nm", "dt", "dup_time", "empty", "end", "end_day",
    "end_time", "envir", "envir_index", "epw", "epw_name", "epw_url", "excl_loc",
    "exit_status", "ext", "extensible", "extensible_group", "field",
    "field_anid", "field_anid_an", "field_count", "field_id", "field_in",
    "field_index", "field_name", "field_name_noid", "field_rleid",
    "first_extensible", "fmt", "found", "from", "group", "group_id",
    "group_name", "has_any_na", "has_range", "header", "hour", "hour_in", "id",
    "id_list", "idf", "index", "index_str", "info", "input_num", "ip",
    "ip_units", "is_all_na", "is_name", "last_extensible", "last_required",
    "latitude", "left_fields", "left_group", "level", "level_index",
    "level_num", "line", "line_s", "ln_miss", "location", "longitude",
    "lower_incbounds", "max_suffix_num", "maximum", "maximum<", "mes_miss",
    "mes_object", "min_fields", "min_required", "minimum", "minimum>", "minute",
    "model", "month_in", "msg", "name", "new_comment", "new_object_name",
    "new_value", "new_value_num", "nm", "num", "num_extensible",
    "num_extensible_group", "num_fields", "num_group", "object_id",
    "object_list", "object_name", "object_name_lower", "object_order",
    "object_rleid", "old_exist", "old_object_name", "old_object_name_lower",
    "out", "out_of_range", "output_dir", "prefix", "ref_class", "ref_field",
    "ref_object", "ref_value", "reference", "reference_class_name", "reporting_frequency",
    "required_field", "required_object", "res", "rev_field_rleid", "same_dir",
    "si", "slash", "slash_key", "slash_loc", "slash_value", "slash_value_lower",
    "slash_value_rleid", "soil_conductivity", "soil_density",
    "soil_specific_heat", "source_type", "space_loc", "spcl_loc", "src_class",
    "src_class_id", "src_class_name", "src_enum", "src_field", "src_field_id",
    "src_field_index", "src_field_name", "src_object", "src_object_id",
    "src_value", "src_value_chr", "src_value_id", "start", "start_day",
    "start_day_of_week", "state_province", "status", "string", "temp",
    "temperature", "title", "to", "type", "type_enum", "type_exp",
    "unique_object", "upper_incbounds", "use_input_name", "value", "value_chr",
    "value_count", "value_id", "value_lower", "value_num", "weather",
    "wmo_number", "x", "year_in", "J", "num_fld", "max_fld", "Date/Time",
    "Variable", "i.datetime", "i.day", "i.hour", "i.minute", "i.month",
    "i.object_id", "i.rleid", "old", "key_value", "i.key_value", "i.value",
    "field_num", "i.class_id", "i.min_fields", "i.num_extensible",
    "i.extensible_group", "i.required_field", "i.comment", "old_min_fields",
    "new_rleid", "sgl_object_id", "sql", "day_type", "simulation_days",
    "row_index", "is_num"
))
# }}}
