# macro_dict {{{
macro_dict <-
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
`.` = `.GRP` = `.I` = `.N` = `.SD` = NULL
utils::globalVariables(c(
    "Case", "DateTime", "Day", "Hour", "J", "KeyValue", "Minute", "Month", "N",
    "Name", "V1", "all_name_upper", "arch", "assigned_new_name", "auto",
    "autocalculatable", "autosizable", "base", "begin_environment",
    "begin_extensible", "check_lower", "check_upper", "choice", "choice_id",
    "class_group", "class_id", "class_name", "class_name_in", "class_rleid",
    "class_upper_case", "colon_loc", "comment_id", "core_file", "datetime",
    "datetime_delta", "datetime_shifted", "day", "def", "default", "default_id",
    "default_ipnum", "default_num", "default_upper", "del_ext_num", "delete",
    "dt_minute", "dt_minute_cal", "duplicated_name", "empty",
    "environment_index", "explpt_loc", "ext", "ext_from", "ext_index",
    "ext_num", "extensible", "external_key", "external_list",
    "external_list_id", "field_an", "field_anid", "field_count", "field_id",
    "field_index", "field_name", "field_name_in", "field_name_lower",
    "file_name", "first_extensible", "found", "full_ipname", "full_name",
    "group", "group_id", "group_name", "grp", "has_default",
    "has_external_list", "has_object_list", "has_range", "has_reference",
    "header", "hour", "id_list", "idx", "index", "info", "input", "input_index",
    "ip_unit", "ip_units", "is_default", "is_extensible", "is_name", "key",
    "last_extensible", "last_index", "last_req_ext", "last_required",
    "leading_spaces", "left_fields", "level", "level_index", "level_num",
    "line", "loc_model", "loc_weather", "lower_incbounds", "macro_key",
    "max_suffix_num", "maximum", "maximum<", "memo", "min_fields", "minimum",
    "minimum>", "miss_idx", "msg_box", "msg_line", "new_ext_num",
    "new_field_id", "new_full_path", "new_object_name", "new_object_name_upper",
    "new_value", "num", "num_extensible", "num_extensible_group", "num_fields",
    "num_obj", "num_str", "num_to_add", "num_values", "object_id",
    "object_list", "object_list_id", "object_list_rleid", "object_name",
    "object_name_upper", "object_order", "object_rleid", "old_exist",
    "old_full_path", "os", "out", "possible_value", "possible_value_upper",
    "prerelease", "range_id", "reference", "reference_class_name",
    "reference_id", "reference_value_id", "referenced_by_object_id", "req",
    "required", "required_field", "required_object", "res", "res_ran", "row_id",
    "same_dir", "same_name_order", "seperate", "slash_key", "slash_key_value",
    "slash_loc", "slash_supported", "slash_value", "slash_value_upper",
    "space_loc", "special_key", "special_loc", "special_value", "string",
    "time", "type", "unique_object", "unit", "unitsbasedonfield",
    "upper_incbounds", "use_input_name", "value", "value_count", "value_id",
    "value_in", "value_ipnum", "value_num", "value_upper"))
# }}}
