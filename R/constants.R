# versions {{{
latest_eplus_ver <- function () as.numeric_version("8.9.0")

all_eplus_ver <- function () c(paste0("8.", 3:9, ".0"), paste0("8.3.", 1:3))

all_idd_ver <- function () {
    c(
        # 1.X.X
        paste0("1.0.", 0:3),
        paste0("1.1.", 0:1),
        paste0("1.2.", 0:3),
        paste0("1.", 3:4, ".0"),
        paste0("2.", 0:2, ".0"),
        paste0("3.", 0:1, ".0"),
        paste0(4:6, ".0.0"),
        paste0("7.", 0:2, ".0"),
        paste0("8.", 0:9, ".0")
    )
}

all_eplus_release_commit <- function () {
    data.table::fread("
        version,     commit
          8.9.0, 40101eaafd
          8.8.0, 7c3bbe4830
          8.7.0, 78a111df4a
          8.6.0, 198c6a3cff
          8.5.0, c87e61b44b
          8.4.0, 832e4bb9cb
          8.3.0, 6d97d074ea
    ")
}
# }}}
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
utils::globalVariables(c( "Case", "DateTime", "Day", "Hour", "J", "KeyValue",
        "Minute", "Month", "N", "Name", "V1", "all_cmt", "all_field_name_lower",
        "all_field_name_upper", "all_name_upper", "annual", "arch",
        "assigned_new_name", "auto", "autocalculatable", "autosizable", "base",
        "begin_environment", "begin_extensible", "can_be_na", "check_lower",
        "check_upper", "choice", "choice_id", "class_group", "class_id",
        "class_name", "class_name_in", "class_rleid", "class_upper_case",
        "colon_loc", "comment_id", "commit", "core_file", "datetime",
        "datetime_delta", "datetime_shifted", "day", "def", "default",
        "default_id", "default_ipnum", "default_num", "default_upper",
        "del_ext_num", "delete", "design_day", "dt_minute", "dt_minute_cal",
        "duplicated_name", "end_time", "empty", "environment_index", "epw",
        "exit_status", "explpt_loc", "ext", "ext_from", "ext_index", "ext_num",
        "extensible", "external_key", "external_list", "external_list_id",
        "field_an", "field_anid", "field_count", "field_id", "field_index",
        "field_name", "field_name_in", "field_name_lower", "file_name",
        "first_extensible", "found", "full_ipname", "full_name", "group",
        "group_id", "group_name", "grp", "has_any_na", "has_default",
        "has_external_list", "has_object_list", "has_range", "has_reference",
        "header", "hour", "id_list", "idf", "idx", "index", "index_str", "info",
        "input", "input_index", "install_eplus_macos", "invalid_ext_num",
        "ip_unit", "ip_units", "is_all_na", "is_complete", "is_default",
        "is_extensible", "is_name", "key", "last_extensible", "last_index",
        "last_req_ext", "last_required", "leading_spaces", "left_fields",
        "level", "level_index", "level_num", "line", "loc_model", "loc_weather",
        "lower_incbounds", "macro_key", "max_suffix_num", "maximum", "maximum<",
        "memo", "min_fields", "minimum", "minimum>", "mis_val_num", "miss_idx",
        "model", "msg", "msg_box", "msg_line", "new_ext_num", "new_field_id",
        "new_full_path", "new_object_name", "new_object_name_upper",
        "new_value", "new_value_num", "new_value_upper", "new_value_ipnum",
        "note", "num", "num_extensible", "num_extensible_group", "num_fields",
        "num_obj", "num_str", "num_to_add", "num_values", "object_id",
        "object_list", "object_list_id", "object_list_rleid", "object_name",
        "object_name_upper", "object_order", "object_rleid", "old_exist",
        "old_full_path", "os", "out", "output_dir", "possible_value",
        "possible_value_upper", "prerelease", "range_id", "reference",
        "reference_class_name", "reference_id", "reference_value_id",
        "referenced_by_object_id", "req", "required", "required_field",
        "required_object", "res", "res_ran", "row_id", "same_dir",
        "same_name_order", "seperate", "slash_key", "slash_key_value",
        "slash_loc", "slash_supported", "slash_value", "slash_value_upper",
        "space_loc", "special_key", "special_loc", "special_value", "status",
        "string", "time", "type", "unique_object", "unit", "unitsbasedonfield",
        "upper_incbounds", "use_input_name", "valid_ext_num", "value",
        "value_count", "value_ext_num", "value_id", "value_in", "value_ipnum",
        "value_num", "value_upper", "weather"
))
# }}}
