################################################################################
#                          Parse EnergyPlus IDD File                           #
################################################################################

#' @export
parse_idd <- function(filepath) {
    # The parsing process was basically the same as that was implemented in
    # IDFEditor distributed with EnergyPlus, but using the powerful 'data.table'
    # package to speed up the whole process and store the results.
    # The souce codes of IDFEditor can be found as below:
    # https://github.com/NREL/EnergyPlus/tree/develop/src/IDF_Editor

    # set progress bar
    pb <- progress::progress_bar$new(
           format = "  Parsing IDD (:what) [:bar] :percent in :elapsed",
           total = 100, clear = FALSE)

    # show progress bar
    pb$tick(0)

    pb$tick(tokens = list(what = "Initialize"))
    # read idd string, get idd version and build
    idd_str <- read_idd(filepath)
    idd_version <- attr(idd_str, "version")
    idd_build <- attr(idd_str, "build")
    idd_dt <- data.table(line = seq_along(idd_str), string = idd_str)
    setkeyv(idd_dt, c("line", "string"))

    pb$tick(tokens = list(what = "Parsing "))
    # parse basic info {{{
    pb$tick(tokens = list(what = "Parsing "))
    # Mark blank and comment lines
    # {{{
    idd_dt[, ignore := FALSE]
    idd_dt[stringr::str_detect(string, "^!|(^$)"), ignore := TRUE]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # FIELD: non-ignored, non-group, and non-slash lines with leading
    # characters "[AN][0-9]+"
    # handle condensed fields first {{{
    idd_dt[, `:=`(field_condensed = NA_integer_,
                  field_anid = NA_character_,
                  field_slash= NA_character_,
                  field_last = NA)]
    # get number of AN in condensed lines
    idd_dt[ignore == FALSE &
           stringr::str_detect(string, "^[AaNn]\\d+\\s*,\\s*[AaNn]\\d+"),
           field_condensed := get_condensed_field_num(string, sep = "\\")]
    # get number of AN in condensed lines
    idd_dt[ignore == FALSE &
           stringr::str_detect(string, "^[AaNn].*\\s*?,\\s*?(?=\\\\)"),
           c("field_anid", "field_slash", "field_last") :=
                data.table(stringr::str_split_fixed(string, "\\s*,\\s*(?=\\\\)", n = 2L),
                           field_last = FALSE)]
    idd_dt[ignore == FALSE &
           stringr::str_detect(string, "^[AnNn].*\\s*;\\s*(?=\\\\)"),
           c("field_anid", "field_slash", "field_last") :=
                data.table(stringr::str_split_fixed(string, "\\s*;\\s*(?=\\\\)", n = 2L),
                           field_last = TRUE)]
    # extract field AN and id
    idd_field_condensed <- idd_dt[
        !is.na(field_condensed),
        stringr::str_split_fixed(field_anid, "\\s*,\\s*", n = field_condensed),
        by = .(line)]
    setnames(idd_field_condensed, c("line", "field_anid"))
    # combine
    idd_dt <- idd_field_condensed[idd_dt, on = "line"][
        is.na(field_condensed), field_anid := i.field_anid][
        , i.field_anid := NULL]
    # }}}
    pb$tick(tokens = list(what = "Parsing "))
    # {{{
    # for not the last fields
    idd_dt[ignore == FALSE & is.na(field_condensed) &
           stringr::str_detect(string, "^[AaNn][0-9]+\\s*,"),
           c("field_anid", "field_slash", "field_last", "field_condensed") :=
               data.table(stringr::str_split_fixed(string, "\\s*,\\s*", n = 2L),
                          field_last = FALSE, field_condensed = 1L)]
    # for the last fields
    idd_dt[ignore == FALSE & is.na(field_condensed) &
           stringr::str_detect(string, "^[AaNn][0-9]+\\s*;"),
           c("field_anid", "field_slash", "field_last", "field_condensed") :=
               data.table(stringr::str_split_fixed(string, "\\s*;\\s*", n = 2L),
                          field_last = TRUE, field_condensed = 1L)]
    idd_dt[field_slash == "", field_slash := NA_character_]
    # FIELD ID: integers after "[AN]" before "[,;]" in field AN lines.
    # FIELD AN: leading "[AN]" in field AN lines.
    idd_dt[, `:=`(field_an = NA_character_, field_id = NA_integer_)]
    idd_dt[!is.na(field_anid), field_an := stringr::str_sub(field_anid, start = 1L, end = 1L)]
    idd_dt[!is.na(field_anid), field_id := as.integer(stringr::str_sub(field_anid, start = 2L))]
    idd_dt[, field_anid := NULL]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # SLASH: non-ignored, non-group lines with leading characters "\" or
    # string in field_slash column
    # NOTE: have to combine splited results into a data.table in advance.
    # {{{
    idd_dt[, c("slash_key", "slash_value") := NA_character_]
    ## for slash lines
    ### have to handle some informal slash keys such as '\ group' which should
    ### be '\group'
    idd_dt[ignore == FALSE & is.na(field_an) & stringr::str_detect(string, "^\\\\\\s+"),
           string := stringr::str_replace(string, "^\\\\\\s+", "\\\\")]
    ### have to handle some informal slash keys such as '\minimum >0' which should
    ### be '\minimum< 0'
    idd_dt[ignore == FALSE & is.na(field_an) &
           stringr::str_detect(string, stringr::regex("^\\\\minimum\\s+>", ignore_case = TRUE)),
           string := stringr::str_replace(string, "\\s+>", "> ")]
    idd_dt[ignore == FALSE & is.na(field_an) &
           stringr::str_detect(string, stringr::regex("^\\\\maximum\\s+<", ignore_case = TRUE)),
           string := stringr::str_replace(string, "\\s+<", "< ")]
    ## change "\extensible:[1-9]" to "\extensible [1-9]"
    idd_dt[ignore == FALSE & is.na(field_an) & stringr::str_detect(string, "^\\\\"),
               string := replace_slash_colon(string)]

    idd_dt[ignore == FALSE & is.na(field_an) & stringr::str_detect(string, "^\\\\"),
           c("slash_key", "slash_value") :=
               as.data.table(stringr::str_split_fixed(string, "\\s+", n = 2L))]
    idd_dt[!is.na(field_slash),
           c("slash_key", "slash_value") :=
               as.data.table(stringr::str_split_fixed(field_slash, "\\s+", n = 2L))]
    idd_dt[, slash_key := stringr::str_sub(slash_key, start = 2L)]
    idd_dt[, field_slash := NULL]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # CLASS: non-ignored, non-group, non-slash lines with trailing character ","
    # NOTE: there are some class lines that have comments
    # {{{
    idd_dt[, class := NA_character_]
    idd_dt[ignore == FALSE &
           is.na(field_an) &
           is.na(slash_key) &
           stringr::str_detect(string, ",(\\s*!.*)*$"),
           class := stringr::str_split_fixed(string, "\\s*,\\s*", n = 2)[,1]]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # SEPERATOR: non-ignored, non-group, non-class, non-slash lines with
    # trailing character ";". Currently there are treated just as comments
    # {{{
    idd_dt[ignore == FALSE &
           is.na(class) &
           is.na(field_an) &
           is.na(slash_key) &
           stringr::str_detect(string, ";$"),
           ignore := TRUE]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # check parsing error {{{
    idd_errors <- idd_dt[ignore == FALSE &
                         is.na(class) &
                         is.na(field_an) &
                         is.na(slash_key), .(line, string)]
    if (nrow(idd_errors) > 0) {
        parse_issue("Invalid line found", idd_errors)
    }
    # }}}
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # Special filtering for handling certain schedule objects that provide
    # multi-unit field support
    # {{{
    idd_dt[, class_special_multi_unit := NA]
    idd_dt[!is.na(class), class_special_multi_unit := FALSE]
    idd_dt[stringr::str_to_upper(class) == "SCHEDULETYPELIMITS", class_special_multi_unit := TRUE]
    idd_dt[stringr::str_to_upper(class) == "SCHEDULE:DAY:HOURLY", class_special_multi_unit := TRUE]
    idd_dt[stringr::str_to_upper(class) == "SCHEDULE:DAY:INTERVAL", class_special_multi_unit := TRUE]
    idd_dt[stringr::str_to_upper(class) == "SCHEDULE:DAY:LIST", class_special_multi_unit := TRUE]
    idd_dt[stringr::str_to_upper(class) == "SCHEDULE:COMPACT", class_special_multi_unit := TRUE]
    idd_dt[stringr::str_to_upper(class) == "SCHEDULE:CONSTANT", class_special_multi_unit := TRUE]
    # }}}

    pb$tick(30L, tokens = list(what = "Parsing "))
    # parse slash lines {{{
    idd_dt[, slash_supported := NA]
    idd_dt[!is.na(slash_key), slash_supported := FALSE]

    pb$tick(tokens = list(what = "Parsing "))
    # \Field {{{
    idd_dt[, `:=`(field_name = NA_character_,
                  field_autosizable = NA,
                  field_autocalculatable = NA,
                  field_deprecated = NA)]
    idd_dt[stringr::str_to_upper(slash_key) == "FIELD",
           `:=`(slash_supported = TRUE,
                field_name = slash_value,
                field_autosizable = FALSE,
                field_autocalculatable = FALSE,
                field_deprecated = FALSE)]
    # }}}
    pb$tick(tokens = list(what = "Parsing "))
    # \Units {{{
    idd_dt[, `:=`(field_units = NA_character_)]
    idd_dt[stringr::str_to_upper(slash_key) == "UNITS",
           `:=`(slash_supported = TRUE,
                field_units = slash_value)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Key {{{
    idd_dt[, `:=`(field_choice = NA_character_)]
    idd_dt[stringr::str_to_upper(slash_key) == "KEY",
           `:=`(slash_supported = TRUE,
                field_choice = slash_value)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Minimum & \Minimum> {{{
    idd_dt[, `:=`(field_minimum = NA_real_,
                  field_exclusive_min = NA,
                  field_min_specified = NA)]
    idd_dt[stringr::str_to_upper(slash_key) == "MINIMUM",
           `:=`(slash_supported = TRUE,
                field_minimum = as.double(slash_value),
                field_exclusive_min = FALSE,
                field_min_specified = TRUE)]
    idd_dt[stringr::str_to_upper(slash_key) == "MINIMUM>",
           `:=`(slash_supported = TRUE,
                field_minimum = as.double(slash_value),
                field_exclusive_min = TRUE,
                field_min_specified = TRUE)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Maximum & \Maximum< {{{
    idd_dt[, `:=`(field_maximum = NA_real_,
                  field_exclusive_max = NA,
                  field_max_specified = NA)]
    idd_dt[stringr::str_to_upper(slash_key) == "MAXIMUM",
           `:=`(slash_supported = TRUE,
                field_maximum = as.double(slash_value),
                field_exclusive_max = FALSE,
                field_max_specified = TRUE)]
    idd_dt[stringr::str_to_upper(slash_key) == "MAXIMUM<",
           `:=`(slash_supported = TRUE,
                field_maximum = as.double(slash_value),
                field_exclusive_max = TRUE,
                field_max_specified = TRUE)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Default {{{
    idd_dt[, `:=`(field_default_value = NA_real_,
                  field_default_autosize = NA,
                  field_default_autocalculate = NA,
                  field_default_specified = NA,
                  field_default_choice = NA_character_)]
    idd_dt[stringr::str_to_upper(slash_key) == "DEFAULT" &
           !is.na(suppressWarnings(as.double(slash_value))),
           `:=`(slash_supported = TRUE,
                field_default_value = as.double(slash_value),
                field_default_specified = TRUE)]
    idd_dt[stringr::str_to_upper(slash_key) == "DEFAULT" &
           stringr::str_to_upper(slash_value) == "AUTOSIZE",
           `:=`(slash_supported = TRUE,
                field_default_autosize = TRUE,
                field_default_specified = TRUE)]
    idd_dt[stringr::str_to_upper(slash_key) == "DEFAULT" &
           stringr::str_to_upper(slash_value) == "AUTOCALCULATE",
           `:=`(slash_supported = TRUE,
                field_default_autocalculate = TRUE,
                field_default_specified = TRUE)]
    idd_dt[stringr::str_to_upper(slash_key) == "DEFAULT" &
           is.na(field_default_value) &
           is.na(field_default_autosize) &
           is.na(field_default_autocalculate),
           `:=`(slash_supported = TRUE,
                field_default_choice = slash_value,
                field_default_specified = TRUE)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Note {{{
    idd_dt[, `:=`(field_note = NA_character_)]
    idd_dt[stringr::str_to_upper(slash_key) == "NOTE",
           `:=`(slash_supported = TRUE,
                field_note = slash_value)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Type {{{
    idd_dt[, `:=`(field_type = NA_character_)]
    idd_dt[stringr::str_to_upper(slash_key) == "TYPE",
           `:=`(slash_supported = TRUE,
                field_type = slash_value)]
    idd_errors_type <- idd_dt[stringr::str_to_upper(slash_key) == "TYPE" &
                              stringr::str_to_upper(slash_value) != "REAL" &
                              stringr::str_to_upper(slash_value) != "INTEGER" &
                              stringr::str_to_upper(slash_value) != "ALPHA" &
                              stringr::str_to_upper(slash_value) != "CHOICE" &
                              stringr::str_to_upper(slash_value) != "OBJECT-LIST" &
                              stringr::str_to_upper(slash_value) != "EXTERNAL-LIST" &
                              stringr::str_to_upper(slash_value) != "NODE",
                              .(line, string)]
    if (nrow(idd_errors_type) > 0) {
        parse_issue("Invalid \\type found", idd_errors_type)
    }
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Group {{{
    idd_dt[, `:=`(group = NA_character_)]
    idd_dt[stringr::str_to_upper(slash_key) == "GROUP",
           `:=`(slash_supported = TRUE,
                group = slash_value)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Memo {{{
    idd_dt[, `:=`(class_memo = NA_character_)]
    idd_dt[stringr::str_to_upper(slash_key) == "MEMO",
           `:=`(slash_supported = TRUE,
                class_memo = slash_value)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Object-List {{{
    idd_dt[, `:=`(field_object_list = NA_character_)]
    idd_dt[stringr::str_to_upper(slash_key) == "OBJECT-LIST",
           `:=`(slash_supported = TRUE,
                field_object_list = slash_value)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \External-List {{{
    idd_dt[, `:=`(field_external_list = NA_character_)]
    idd_dt[stringr::str_to_upper(slash_key) == "EXTERNAL-LIST",
           `:=`(slash_supported = TRUE)]
    idd_dt[stringr::str_to_upper(slash_key) == "EXTERNAL-LIST" &
           stringr::str_to_upper(slash_value) == "AUTORDDVARIABLE",
           `:=`(field_external_list = "Variable")]
    idd_dt[stringr::str_to_upper(slash_key) == "EXTERNAL-LIST" &
           stringr::str_to_upper(slash_value) == "AUTORDDMETER",
           `:=`(field_external_list = "Meter")]
    idd_dt[stringr::str_to_upper(slash_key) == "EXTERNAL-LIST" &
           stringr::str_to_upper(slash_value) == "AUTORDDVARIABLEMETER",
           `:=`(field_external_list = "VariableMeter")]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Reference {{{
    idd_dt[, `:=`(field_reference = NA_character_)]
    idd_dt[stringr::str_to_upper(slash_key) == "REFERENCE",
           `:=`(slash_supported = TRUE,
                field_reference = slash_value)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Reference-Class-Name {{{
    idd_dt[, `:=`(class_reference_name = NA_character_)]
    idd_dt[stringr::str_to_upper(slash_key) == "REFERENCE-CLASS-NAME",
           `:=`(slash_supported = TRUE,
                class_reference_name = slash_value)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Min-Fields {{{
    idd_dt[, `:=`(class_min_fields = NA_integer_)]
    idd_dt[stringr::str_to_upper(slash_key) == "MIN-FIELDS",
           `:=`(slash_supported = TRUE,
                class_min_fields = as.integer((slash_value)))]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \IP-Units {{{
    idd_dt[, `:=`(field_ip_units = NA_character_)]
    idd_dt[stringr::str_to_upper(slash_key) == "IP-UNITS",
           `:=`(slash_supported = TRUE,
                field_ip_units = slash_value)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Format {{{
    idd_dt[, `:=`(field_format = NA_character_)]
    idd_dt[stringr::str_to_upper(slash_key) == "FORMAT",
           `:=`(slash_supported = TRUE,
                class_format = slash_value)]
    idd_errors_format <- idd_dt[stringr::str_to_upper(slash_key) == "FORMAT" &
                                stringr::str_to_upper(slash_value) != "SINGLELINE" &
                                stringr::str_to_upper(slash_value) != "VERTICES" &
                                stringr::str_to_upper(slash_value) != "COMPACTSCHEDULE" &
                                stringr::str_to_upper(slash_value) != "FLUIDPROPERTY" &
                                stringr::str_to_upper(slash_value) != "VIEWFACTOR" &
                                stringr::str_to_upper(slash_value) != "SPECTRAL",
                              .(line, string)]
    if (nrow(idd_errors_format) > 0) {
        parse_issue("Invalid \\format found", idd_errors_format)
    }
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Deprecated {{{
    idd_dt[, `:=`(field_deprecated = NA)]
    idd_dt[stringr::str_to_upper(slash_key) == "DEPRECATED",
           `:=`(slash_supported = TRUE,
                field_deprecated = TRUE)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Required-Field {{{
    idd_dt[, `:=`(field_required = NA)]
    idd_dt[stringr::str_to_upper(slash_key) == "REQUIRED-FIELD",
           `:=`(slash_supported = TRUE,
                field_required = TRUE)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \Unique-Object {{{
    idd_dt[, `:=`(class_unique_object = NA)]
    idd_dt[stringr::str_to_upper(slash_key) == "UNIQUE-OBJECT",
           `:=`(slash_supported = TRUE,
                class_unique_object = TRUE)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \AutoSizable {{{
    idd_dt[, `:=`(field_autosizable = NA)]
    idd_dt[stringr::str_to_upper(slash_key) == "AUTOSIZABLE",
           `:=`(slash_supported = TRUE,
                field_autosizable = TRUE)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \AutoCalculatable {{{
    idd_dt[, `:=`(field_autocalculatable = NA)]
    idd_dt[stringr::str_to_upper(slash_key) == "AUTOCALCULATABLE",
           `:=`(slash_supported = TRUE,
                field_autocalculatable = TRUE)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \PreserveIndent {{{
    idd_dt[, `:=`(field_preserve_indent = NA)]
    idd_dt[stringr::str_to_upper(slash_key) == "PRESERVEINDENT",
           `:=`(slash_supported = TRUE,
                field_preserve_indent = TRUE)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # \UnitsBasedOnField {{{
    idd_dt[, `:=`(field_units = NA)]
    idd_dt[stringr::str_to_upper(slash_key) == "UNITSBASEDONFIELD",
           `:=`(slash_supported = TRUE,
                field_units_based_on_field = TRUE)]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # Just list supported slash keys in IDFEditor. These keys are treated just
    # as normal texts in IDFEditor.
    idd_dt[stringr::str_to_upper(slash_key) %in% c("REQUIRED-OBJECT", "EXTENSIBLE",
               "OBSOLETE", "RETAINCASE", "BEGIN-EXTENSIBLE", "FIELD-INDEX",
               "SEQUENCE-ID", "GROUP-INDEX", "HIDEINLIBRARYUI",
               "EMSUNIQUECOMPONENT", "HIDEINALLUI", "SIMERGYONLY", "NO-SEQUENCE",
               "SURROGATE-NAME-FOR-SEQUENCE"),
           `:=`(slash_supported = TRUE)]

    idd_errors_slash_key <- idd_dt[!is.na(slash_key) &
                                   slash_supported == FALSE,
                                   .(line, string)]
    if (nrow(idd_errors_slash_key) > 0L) {
        parse_issue("Invalid slash line found", idd_errors_slash_key)
    }
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # clean {{{
    idd_dt[, slash_supported := NULL]

    setcolorder(idd_dt,
                c("line", "string", "ignore", "slash_key", "slash_value",

                  "group",

                  "class", "class_format", "class_memo", "class_min_fields",
                  "class_reference_name", "class_special_multi_unit",
                  "class_unique_object",

                  "field_an", "field_id", "field_name", "field_last", "field_units",
                  "field_note", "field_ip_units", "field_units_based_on_field",
                  "field_required", "field_type", "field_format", "field_maximum",
                  "field_max_specified", "field_exclusive_max", "field_minimum",
                  "field_min_specified", "field_exclusive_min",
                  "field_autocalculatable", "field_autosizable", "field_choice",
                  "field_default_autocalculate", "field_default_autosize",
                  "field_default_choice", "field_default_specified",
                  "field_default_value", "field_external_list", "field_object_list",
                  "field_reference", "field_preserve_indent", "field_condensed",
                  "field_deprecated"
                  ))
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # fill na for group {{{
    idd_dt[ignore == FALSE, group := group[1], by = .(cumsum(!is.na(group)))]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # fill na for class {{{
    idd_dt[!is.na(group), class := class[1], by = .(cumsum(!is.na(class)))]
    setorder(idd_dt, line)
    idd_dt[!is.na(class), class_format:= class_format[1], by = .(cumsum(!is.na(class_format)), class)]
    # NOTE: leave NA as it is for class_memo
    # idd_dt[!is.na(class), class_memo:= class_memo[1], by = .(cumsum(!is.na(class_memo)), class)]
    idd_dt[!is.na(class), class_min_fields:= class_min_fields[1], by = .(cumsum(!is.na(class_min_fields)), class)]
    idd_dt[!is.na(class), class_reference_name:= class_reference_name[1], by = .(cumsum(!is.na(class_reference_name)), class)]
    idd_dt[!is.na(class), class_special_multi_unit:= class_special_multi_unit[1], by = .(cumsum(!is.na(class_special_multi_unit)), class)]
    idd_dt[!is.na(class), class_unique_object:= class_unique_object[1], by = .(cumsum(!is.na(class_unique_object)), class)]

    setorder(idd_dt, -line)
    idd_dt[!is.na(class), class_format:= class_format[1], by = .(cumsum(!is.na(class_format)), class)]
    # idd_dt[!is.na(class), class_memo:= class_memo[1], by = .(cumsum(!is.na(class_memo)), class)]
    idd_dt[!is.na(class), class_min_fields:= class_min_fields[1], by = .(cumsum(!is.na(class_min_fields)), class)]
    idd_dt[!is.na(class), class_reference_name:= class_reference_name[1], by = .(cumsum(!is.na(class_reference_name)), class)]
    idd_dt[!is.na(class), class_special_multi_unit:= class_special_multi_unit[1], by = .(cumsum(!is.na(class_special_multi_unit)), class)]
    idd_dt[!is.na(class), class_unique_object:= class_unique_object[1], by = .(cumsum(!is.na(class_unique_object)), class)]

    setorder(idd_dt, line)
    # NOTE: leave NA as it is for class_format
    # idd_dt[!is.na(class) & is.na(class_format), class_format := NA_character_]
    # NOTE: leave NA as it is for class_memo
    # idd_dt[!is.na(class) & is.na(class_memo), class_memo := NA_character_]
    idd_dt[!is.na(class) & is.na(class_min_fields), class_min_fields := -1L]
    # NOTE: leave NA as it is for class_reference_name
    # idd_dt[!is.na(class) & is.na(class_reference_name), class_reference_name := NA_character_]
    idd_dt[!is.na(class) & is.na(class_special_multi_unit), class_special_multi_unit := FALSE]
    idd_dt[!is.na(class) & is.na(class_unique_object), class_unique_object := FALSE]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # fill na for field {{{
    idd_dt[!is.na(class), field_an := field_an[1], by = .(cumsum(!is.na(field_an)), class)]
    idd_dt[!is.na(class), field_id := field_id[1], by = .(cumsum(!is.na(field_id)), class)]

    pb$tick(tokens = list(what = "Parsing "))
    setorder(idd_dt, line)
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_name := field_name[1],
           by = .(cumsum(!is.na(field_name)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_last := field_last[1],
           by = .(cumsum(!is.na(field_last)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_units := field_units[1],
           by = .(cumsum(!is.na(field_units)), class, field_an, field_id)]
    # NOTE: leave NA as it is for field_note
    # idd_dt[!is.na(field_an) & !is.na(field_id),
    #        field_note := field_note[1],
    #        by = .(cumsum(!is.na(field_note)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_ip_units := field_ip_units[1],
           by = .(cumsum(!is.na(field_ip_units)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_units_based_on_field := field_units_based_on_field[1],
           by = .(cumsum(!is.na(field_units_based_on_field)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_required := field_required[1],
           by = .(cumsum(!is.na(field_required)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_type := field_type[1],
           by = .(cumsum(!is.na(field_type)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_maximum := field_maximum[1],
           by = .(cumsum(!is.na(field_maximum)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_max_specified := field_max_specified[1],
           by = .(cumsum(!is.na(field_max_specified)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_exclusive_max := field_exclusive_max[1],
           by = .(cumsum(!is.na(field_exclusive_max)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_minimum := field_minimum[1],
           by = .(cumsum(!is.na(field_minimum)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_min_specified := field_min_specified[1],
           by = .(cumsum(!is.na(field_min_specified)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_exclusive_min := field_exclusive_min[1],
           by = .(cumsum(!is.na(field_exclusive_min)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_autocalculatable := field_autocalculatable[1],
           by = .(cumsum(!is.na(field_autocalculatable)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_autosizable := field_autosizable[1],
           by = .(cumsum(!is.na(field_autosizable)), class, field_an, field_id)]
    # NOTE: leave NA as it is for field_choice
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_choice := field_choice[1],
           by = .(cumsum(!is.na(field_choice)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_default_autocalculate := field_default_autocalculate[1],
           by = .(cumsum(!is.na(field_default_autocalculate)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_default_autosize := field_default_autosize[1],
           by = .(cumsum(!is.na(field_default_autosize)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_default_choice := field_default_choice[1],
           by = .(cumsum(!is.na(field_default_choice)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_default_specified := field_default_specified[1],
           by = .(cumsum(!is.na(field_default_specified)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_default_value := field_default_value[1],
           by = .(cumsum(!is.na(field_default_value)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_external_list := field_external_list[1],
           by = .(cumsum(!is.na(field_external_list)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_object_list := field_object_list[1],
           by = .(cumsum(!is.na(field_object_list)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_reference := field_reference[1],
           by = .(cumsum(!is.na(field_reference)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_preserve_indent := field_preserve_indent[1],
           by = .(cumsum(!is.na(field_preserve_indent)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_condensed := field_condensed[1],
           by = .(cumsum(!is.na(field_condensed)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_deprecated := field_deprecated[1],
           by = .(cumsum(!is.na(field_deprecated)), class, field_an, field_id)]

    pb$tick(tokens = list(what = "Parsing "))
    setorder(idd_dt, -line)
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_name := field_name[1],
           by = .(cumsum(!is.na(field_name)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_last := field_last[1],
           by = .(cumsum(!is.na(field_last)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_units := field_units[1],
           by = .(cumsum(!is.na(field_units)), class, field_an, field_id)]
    # NOTE: leave NA as it is for field_note
    # idd_dt[!is.na(field_an) & !is.na(field_id),
    #        field_note := field_note[1],
    #        by = .(cumsum(!is.na(field_note)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_ip_units := field_ip_units[1],
           by = .(cumsum(!is.na(field_ip_units)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_units_based_on_field := field_units_based_on_field[1],
           by = .(cumsum(!is.na(field_units_based_on_field)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_required := field_required[1],
           by = .(cumsum(!is.na(field_required)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_type := field_type[1],
           by = .(cumsum(!is.na(field_type)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_maximum := field_maximum[1],
           by = .(cumsum(!is.na(field_maximum)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_max_specified := field_max_specified[1],
           by = .(cumsum(!is.na(field_max_specified)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_exclusive_max := field_exclusive_max[1],
           by = .(cumsum(!is.na(field_exclusive_max)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_minimum := field_minimum[1],
           by = .(cumsum(!is.na(field_minimum)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_min_specified := field_min_specified[1],
           by = .(cumsum(!is.na(field_min_specified)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_exclusive_min := field_exclusive_min[1],
           by = .(cumsum(!is.na(field_exclusive_min)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_autocalculatable := field_autocalculatable[1],
           by = .(cumsum(!is.na(field_autocalculatable)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_autosizable := field_autosizable[1],
           by = .(cumsum(!is.na(field_autosizable)), class, field_an, field_id)]
    # NOTE: leave NA as it is for field_choice
    # idd_dt[!is.na(field_an) & !is.na(field_id),
    #        field_choice := field_choice[1],
    #        by = .(cumsum(!is.na(field_choice)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_default_autocalculate := field_default_autocalculate[1],
           by = .(cumsum(!is.na(field_default_autocalculate)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_default_autosize := field_default_autosize[1],
           by = .(cumsum(!is.na(field_default_autosize)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_default_choice := field_default_choice[1],
           by = .(cumsum(!is.na(field_default_choice)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_default_specified := field_default_specified[1],
           by = .(cumsum(!is.na(field_default_specified)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_default_value := field_default_value[1],
           by = .(cumsum(!is.na(field_default_value)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_external_list := field_external_list[1],
           by = .(cumsum(!is.na(field_external_list)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_object_list := field_object_list[1],
           by = .(cumsum(!is.na(field_object_list)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_reference := field_reference[1],
           by = .(cumsum(!is.na(field_reference)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_preserve_indent := field_preserve_indent[1],
           by = .(cumsum(!is.na(field_preserve_indent)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_condensed := field_condensed[1],
           by = .(cumsum(!is.na(field_condensed)), class, field_an, field_id)]
    idd_dt[!is.na(field_an) & !is.na(field_id),
           field_deprecated := field_deprecated[1],
           by = .(cumsum(!is.na(field_deprecated)), class, field_an, field_id)]

    pb$tick(tokens = list(what = "Parsing "))
    setorder(idd_dt, line)
    # NOTE: leave NA as it is for field_name
    # idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_name),
    #        field_name := NA_character_]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_last),
           field_last := FALSE]
    # NOTE: leave NA as it is for field_units
    # idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_units),
    #        field_units := NA_character_]
    # NOTE: leave NA as it is for field_note
    # idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_note),
    #        field_note := NA_character_]
    # NOTE: leave NA as it is for field_ip_units
    # idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_ip_units),
    #        field_ip_units := NA_character_]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_units_based_on_field),
           field_units_based_on_field := FALSE]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_required),
           field_required := FALSE]
    # NOTE: leave NA as it is for field_type
    # idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_type),
    #        field_type := NA_character_]
    # NOTE: leave NA as it is for field_maximum
    # idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_maximum),
    #        field_maximum := NA]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_max_specified),
           field_max_specified := FALSE]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_exclusive_max),
           field_exclusive_max := FALSE]
    # NOTE: leave NA as it is for field_minimum
    # idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_minimum),
    #        field_minimum := NA]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_min_specified),
           field_min_specified := FALSE]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_exclusive_min),
           field_exclusive_min := FALSE]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_autocalculatable),
           field_autocalculatable := FALSE]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_autosizable),
           field_autosizable := FALSE]
    # NOTE: leave NA as it is for field_choice
    # idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_choice),
    #        field_choice := NA_character_]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_default_autocalculate),
           field_default_autocalculate := FALSE]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_default_autosize),
           field_default_autosize := FALSE]
    # NOTE: leave NA as it is for field_default_choice
    # idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_default_choice),
    #        field_default_choice := NA_character_]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_default_specified),
           field_default_specified := FALSE]
    # NOTE: leave NA as it is for field_default_value
    # idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_default_value),
    #        field_default_value := NA_real_]
    # NOTE: leave NA as it is for field_external_list
    # idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_external_list),
    #        field_external_list := NA_character_]
    # NOTE: leave NA as it is for field_object_list
    # idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_object_list),
    #        field_object_list := NA_character_]
    # NOTE: leave NA as it is for field_reference
    # idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_reference),
    #        field_reference := NA_character_]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_preserve_indent),
           field_preserve_indent := FALSE]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_condensed),
           field_condensed := 1L]
    idd_dt[!is.na(field_an) & !is.na(field_id) & is.na(field_deprecated),
           field_deprecated := FALSE]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    idd_parsed <- idd_dt[
        stringr::str_to_upper(slash_key) != "GROUP"][
        !is.na(slash_key)]

    pb$tick(tokens = list(what = "Parsing "))
    # FIELD data.table {{{
    # get all field related data
    idd_field_full <- unique(
        idd_parsed[!is.na(field_an) & !is.na(field_id)][
                   , .SD, .SDcol = c("group", "class",
                                     stringr::str_subset(names(idd_dt), "^field"))])

    pb$tick(tokens = list(what = "Parsing "))
    # handle multiple values of \note, \reference, \object-list, \key {{{
    ## handle class with multiple \memo
    # extract only columns with single value
    idd_field <- unique(
        idd_field_full[
        , .SD, .SDcol = setdiff(names(idd_field_full),
                                c("field_note", "field_choice", "field_reference",
                                  "field_object_list"))
        ]
    )

    # combine note into one per field
    idd_field_note <-
        idd_field_full[!is.na(field_note), field_note, by = .(group, class, field_an, field_id)][
        , .(field_note = stringr::str_c(field_note, collapse = " ")), by = .(group, class, field_an, field_id)
    ]

    # merge memo into the main field data.table
    idd_field <- idd_field_note[idd_field, on = c("group", "class", "field_an", "field_id")]

    pb$tick(tokens = list(what = "Parsing "))
    ## handle class with multiple \field-choice
    # combine field choices into one vector per field
    idd_field_choice <- idd_field_full[
        !is.na(field_choice),
        .(field_choice = c(.SD)), .SDcol = "field_choice",
        by = c("group", "class", "field_an", "field_id")]

    # merge class reference names into the main field data.table
    idd_field <- idd_field_choice[idd_field, on = c("group", "class", "field_an", "field_id")]

    pb$tick(tokens = list(what = "Parsing "))
    ## handle class with multiple \field-reference
    # combine field references into one vector per field
    idd_field_reference <- idd_field_full[
        !is.na(field_reference),
        .(field_reference = c(.SD)), .SDcol = "field_reference",
        by = c("group", "class", "field_an", "field_id")]

    # merge field reference names into the main field data.table
    idd_field <- idd_field_reference[idd_field, on = c("group", "class", "field_an", "field_id")]

    pb$tick(tokens = list(what = "Parsing "))
    ## handle class with multiple \field-object-list
    # combine field object lists into one vector per field
    idd_field_object_list <- idd_field_full[
        !is.na(field_object_list),
        .(field_object_list = c(.SD)), .SDcol = "field_object_list",
        by = c("group", "class", "field_an", "field_id")]

    # merge field objectlist names into the main field data.table
    idd_field <- idd_field_object_list[idd_field, on = c("group", "class", "field_an", "field_id")]
    # }}}
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # CLASS data.table {{{
    # 1. if slash key is group, it has already been duplicated during NA-filling
    #    process.
    # 2. if an non-ignored non-group line with slash key being NA, it must be a
    #    class which has been duplicated during the NA-filling process

    # get all class related data
    idd_class_full <- unique(idd_parsed[, .SD,
        .SDcol = c("group", stringr::str_subset(names(idd_dt), "^class"))])

    pb$tick(tokens = list(what = "Parsing "))
    # handle multiple values of \memo, \class-reference-name {{{
    ## handle class with multiple \memo
    # extract only columns with single value
    idd_class <- unique(
        idd_class_full[
        , .SD, .SDcol = setdiff(names(idd_class_full),
                                c("class_memo", "class_reference_name"))
        ]
    )

    # combine memo into one per class
    idd_class_memo <-
        idd_class_full[!is.na(class_memo), class_memo, by = class][
        , .(class_memo = stringr::str_c(class_memo, collapse = " ")), by = class
    ]

    # merge memo into the main class data.table
    idd_class <- idd_class_memo[idd_class, on = "class"]

    pb$tick(tokens = list(what = "Parsing "))
    ## handle class with multiple \class-reference-name
    # combine class reference names into one vector per class
    idd_class_reference_name <- idd_class_full[
        !is.na(class_reference_name),
        .(class_reference_name = c(.SD)), .SDcol = "class_reference_name", by = class]

    # merge class reference names into the main class data.table
    idd_class <- idd_class_reference_name[idd_class, on = "class"]
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    # get max field number per class
    idd_class_max_field <- idd_field[!is.na(field_an) & !is.na(field_id),
        .(class_max_fields = .N), by = class]
    # merge class_max_fields into the main class data.table
    idd_class <- idd_class_max_field[idd_class, on = "class"]
    # add class id for object ordering in idf
    idd_class[, class_id := .I]

    pb$tick(tokens = list(what = "Parsing "))
    # re-arrange columns
    setcolorder(idd_class, c("group", "class_id", "class", "class_min_fields",
        "class_max_fields", "class_format", "class_unique_object",
        "class_special_multi_unit", "class_reference_name", "class_memo"))
    # }}}

    pb$tick(tokens = list(what = "Parsing "))
    idd <- list(version = idd_version,
                build = idd_build,
                class = idd_class,
                field = idd_field)

    pb$tick(100L, tokens = list(what = "Complete"))
    return(idd)
}

################################################################################
#                                   helpers                                    #
################################################################################
# read_idd {{{1
# reg {{{
reg_version_line <- "!IDD_Version"
reg_build_line <- "!IDD_BUILD"
reg_blank_line <- "^\\s*$"
reg_comment_line <- "^\\s*!.*$"
# }}}
get_idd_version <- function(idd_str) {
    point <- stringr::str_which(idd_str, stringr::fixed(reg_version_line))
    idd_version <- stringr::str_sub(idd_str[point], start = 14L)
    return(idd_version)
}

get_idd_build <- function (idd_str) {
    point <- stringr::str_which(idd_str, stringr::fixed(reg_build_line))
    idd_build <- stringr::str_sub(idd_str[point], start = 12L)
    return(idd_build)
}

read_idd <- function(filepath) {
    con = file(filepath, encoding = "UTF-8")
    idd_str <- readLines(con)
    close(con)

    # Get rid of leading and trailing spaces
    idd_str <- trimws(idd_str, which = "both")

    return(idd_str)
}
# }}}1
# parse_issue {{{
parse_issue <- function (type = "", data_errors, info = NULL, src = c("IDD", "IDF"),
                        stop = TRUE) {
    if (!is.null(info)) {
        sep <- paste0(rep("-", 60L), collapse = "")
    } else {
        sep <- NULL
    }

    src = match.arg(src)

    if (stop) {
        key_line <-  "[ Error Type ]"
    } else {
        key_line <-  "[Warning Type]"
    }

    mes <- c(
         glue::glue("

                    ============================================================
                    {src} PARSING ERROR for file {sQuote(filepath)}
                    {key_line}: {type}
                    [Total Number]: {nrow(data_errors)}
                    {if (nrow(data_errors) > 10L) '**Only first 10 errors are shown below**'}
                    ------------------------------------------------------------

                    "),
         glue::glue_data({if (nrow(data_errors) > 10L) data_errors[1:10] else data_errors},
                         "
                         Line {line[1:10]}: {sQuote(string[1:10])}\n
                         "),
         glue::glue("
                    {sep}

                    "),
         glue::glue("{info}"),
         glue::glue("

                    ============================================================
                    "))

    if (stop) {
        stop(glue::glue("{mes}"), call. = FALSE)
    } else {
        ori <- getOption("warning.length")
        options(warning.length = 8000)
        on.exit(option(warning.length = ori))
        warning(glue::glue("{mes}"), call. = FALSE)
    }
}
# }}}
# replace_slash_colon {{{
replace_slash_colon <- function (string) {
    slash_dt <- data.table(line = seq_along(string), char = string)

    slash_dt[, loc_space := stringr::str_locate(char, stringr::fixed(" "))[,1]]
    slash_dt[, loc_colon := stringr::str_locate(char, stringr::fixed(":"))[,1]]

    slash_dt[is.na(loc_space), loc_space := stringr::str_length(char)]
    slash_dt[is.na(loc_colon), loc_colon := stringr::str_length(char)]

    slash_dt[loc_colon < loc_space,
             char := stringr::str_replace(char, stringr::fixed(":"), stringr::fixed(" "))]

    return(slash_dt[, char])
}
# }}}
# get_condensed_field_num {{{
get_condensed_field_num <- function (string, sep) {
    # find all ",", ";" and "\"
    loc <- stringr::str_match_all(string, "[,;\\\\]")
    # transpose
    loc_t <- purrr::map(loc, t)
    if (stringr::str_trim(sep) == "\\") {
        offset = 1L
    } else {
        offset = -1L
    }
    # the field number equals line number of "\" minus 1
    n_field <- purrr::map_int(loc_t, ~max(which(.x == sep)) - offset)

    return(n_field)
}
# }}}
# conversion_units_record {{{
# SI names {{{
si_name <- character(length = 154)
si_name[1] = "m"
si_name[2] = "m"
si_name[3] = "W"
si_name[4] = "W"
si_name[5] = "m3/s"
si_name[6] = "m3/s"
si_name[7] = "C"
si_name[8] = "kg/J"
si_name[9] = "Pa"
si_name[10] = "Pa"
si_name[11] = "Pa"
si_name[12] = "Pa"
si_name[13] = "W/m-K"
si_name[14] = "W/K"
si_name[15] = "deltaC"
si_name[16] = "m2"
si_name[17] = "K"
si_name[18] = "(kg/s)/W"
si_name[19] = "J/kg"
si_name[20] = "kgWater/kgDryAir"
si_name[21] = "kJ/kg"
si_name[22] = "lux"
si_name[23] = "kg/m3"
si_name[24] = "kg/s"
si_name[25] = "kg/s-m"
si_name[26] = "m3"
si_name[27] = "m3"
si_name[28] = "W/m2-K"
si_name[29] = "1/m"
si_name[30] = "J/kg-K"
si_name[31] = "J/m3-K"
si_name[32] = "m/s"
si_name[33] = "m/s"
si_name[34] = "m2-K/W"
si_name[35] = "W/m2"
si_name[36] = "A/K"
si_name[37] = "g/kg"
si_name[38] = "g/m-s"
si_name[39] = "g/m-s-K"
si_name[40] = "J/K"
si_name[41] = "J/kg-K2"
si_name[42] = "J/m3"
si_name[43] = "kg/kg-K"
si_name[44] = "kPa"
si_name[45] = "kPa"
si_name[46] = "m2/s"
si_name[47] = "m3/kg"
si_name[48] = "m3/m3"
si_name[49] = "N-s/m2"
si_name[50] = "V/K"
si_name[51] = "W/m-K2"
si_name[52] = "m3/s-m"
si_name[53] = "deg"
si_name[54] = "hr"
si_name[55] = "A"
si_name[56] = "dimensionless"
si_name[57] = "V"
si_name[58] = "A/V"
si_name[59] = "eV"
si_name[60] = "percent"
si_name[61] = "percentage (as a real decimal)"
si_name[62] = "s"
si_name[63] = "W/m2 or deg C"
si_name[64] = "W/m2, W or deg C"
si_name[65] = "1/K"
si_name[66] = "J/m2-K"
si_name[67] = "ohms"
si_name[68] = "cycles/hr"
si_name[69] = "kg/kg"
si_name[70] = "J/J"
si_name[71] = "g/GJ"
si_name[72] = "L/GJ"
si_name[73] = "m3/GJ"
si_name[74] = "m3/s-m2"
si_name[75] = "m3/s-person"
si_name[76] = "W/m2-K2"
si_name[77] = "g/MJ"
si_name[78] = "L/MJ"
si_name[79] = "m3/MJ"
si_name[80] = "W/W"
si_name[81] = "$/m2"
si_name[82] = "$"
si_name[83] = "$/kW"
si_name[84] = "$/m3"
si_name[85] = "years"
si_name[86] = "$/(W/K)"
si_name[87] = "$/(m3/s)"
si_name[88] = "W/m"
si_name[89] = "minutes"
si_name[90] = "cm"
si_name[91] = "K/m"
si_name[92] = "W/s"
si_name[93] = "kmol"
si_name[94] = "J"
si_name[95] = "GJ"
si_name[96] = "days"
si_name[97] = "kg/m2"
si_name[98] = "kg"
si_name[99] = "kmol/s"
si_name[100] = "percent/K"
si_name[101] = "kg/s2"
si_name[102] = "g/mol"
si_name[103] = "deltaJ/kg"
si_name[104] = "person/m2"
si_name[105] = "m2/person"
si_name[106] = "W/person"
si_name[107] = "W/person"
si_name[108] = "W/m2"
si_name[109] = "m3/person"
si_name[110] = "m3/hr-person"
si_name[111] = "m3/m2"
si_name[112] = "m3/hr-m2"
si_name[113] = "m3/hr"
si_name[114] = "s/m"
si_name[115] = "W/m2"
si_name[116] = "m2/m"
si_name[117] = "L/day"
si_name[118] = "L/kWh"
si_name[119] = "kg/Pa-s-m2"
si_name[120] = "m/hr"
si_name[121] = "Mode"
si_name[122] = "Control"
si_name[123] = "Availability"
si_name[124] = "rev/min"
si_name[125] = "W/(m3/s)"
si_name[126] = "W/m-K"
si_name[127] = "VA"
si_name[128] = "N-m"
si_name[129] = "m3/s-W"
si_name[130] = "cm2"
si_name[131] = "kg/m"
si_name[132] = "Pa"
si_name[133] = "m/yr"
si_name[134] = "1/hr"
si_name[135] = "ppm"
si_name[136] = "W/m-K3"
si_name[137] = "kg/m-s"
si_name[138] = "kg/m-s-K"
si_name[139] = "kg/m-s-K2"
si_name[140] = "J/kg-K3"
si_name[141] = "ms"
si_name[142] = "Ah"
si_name[143] = "deltaC/hr"
si_name[144] = "micron"
si_name[145] = "W/(m3/s)"
si_name[146] = "W/((m3/s)-Pa)"
si_name[147] = "m3/s-m"
si_name[148] = "m3/s-W"
si_name[149] = "m3/person"
si_name[150] = "m3/hr-person"
si_name[151] = "m3/m2"
si_name[152] = "m3/hr-m2"
si_name[153] = "m3/hr"
si_name[154] = "W/((m3/s)-Pa)"
# }}}

# IP names {{{
ip_name <- character(length = 154)
ip_name[1] = "ft"
ip_name[2] = "in"
ip_name[3] = "Btu/h"
ip_name[4] = "W"
ip_name[5] = "ft3/min"
ip_name[6] = "gal/min"
ip_name[7] = "F"
ip_name[8] = "lb/Btu"
ip_name[9] = "psi"
ip_name[10] = "inHg"
ip_name[11] = "inH2O"
ip_name[12] = "ftH2O"
ip_name[13] = "Btu-in/h-ft2-F"
ip_name[14] = "Btu/h-F"
ip_name[15] = "deltaF"
ip_name[16] = "ft2"
ip_name[17] = "R"
ip_name[18] = "(lbm/sec)/(Btu/hr)"
ip_name[19] = "Btu/lb"
ip_name[20] = "lbWater/lbDryAir"
ip_name[21] = "Btu/lb"
ip_name[22] = "foot-candles"
ip_name[23] = "lb/ft3"
ip_name[24] = "lb/s"
ip_name[25] = "lb/s-ft"
ip_name[26] = "ft3"
ip_name[27] = "gal"
ip_name[28] = "Btu/h-ft2-F"
ip_name[29] = "1/ft"
ip_name[30] = "Btu/lb-F"
ip_name[31] = "Btu/ft3-F"
ip_name[32] = "ft/min"
ip_name[33] = "miles/hr"
ip_name[34] = "ft2-F-hr/Btu"
ip_name[35] = "Btu/h-ft2"
ip_name[36] = "A/F"
ip_name[37] = "grains/lb"
ip_name[38] = "lb/ft-s"
ip_name[39] = "lb/ft-s-F"
ip_name[40] = "Btu/F"
ip_name[41] = "Btu/lb-F2"
ip_name[42] = "Btu/ft3"
ip_name[43] = "lb/lb-F"
ip_name[44] = "psi"
ip_name[45] = "inHg"
ip_name[46] = "ft2/s"
ip_name[47] = "ft3/lb"
ip_name[48] = "ft3/ft3"
ip_name[49] = "lbf-s/ft2"
ip_name[50] = "V/F"
ip_name[51] = "Btu/h-F2-ft"
ip_name[52] = "ft3/min-ft"
ip_name[53] = "deg"
ip_name[54] = "hr"
ip_name[55] = "A"
ip_name[56] = "dimensionless"
ip_name[57] = "V"
ip_name[58] = "A/V"
ip_name[59] = "eV"
ip_name[60] = "percent"
ip_name[61] = "percentage (as a real decimal)"
ip_name[62] = "s"
ip_name[63] = "unknown"
ip_name[64] = "unknown"
ip_name[65] = "1/F"
ip_name[66] = "Btu/ft2-F"
ip_name[67] = "ohms"
ip_name[68] = "cycles/hr"
ip_name[69] = "lb/lb"
ip_name[70] = "Btu/Btu"
ip_name[71] = "lb/MWh"
ip_name[72] = "gal/kWh"
ip_name[73] = "ft3/MWh"
ip_name[74] = "ft3/min-ft2"
ip_name[75] = "ft3/min-person"
ip_name[76] = "Btu/h-ft2-F2"
ip_name[77] = "lb/MWh"
ip_name[78] = "gal/kWh"
ip_name[79] = "ft3/kWh"
ip_name[80] = "Btuh/Btuh"
ip_name[81] = "$/ft2"
ip_name[82] = "$"
ip_name[83] = "$/(kBtuh/h)"
ip_name[84] = "$/ft3"
ip_name[85] = "years"
ip_name[86] = "$/(Btu/h-F)"
ip_name[87] = "$/(ft3/min)"
ip_name[88] = "Btu/h-ft"
ip_name[89] = "minutes"
ip_name[90] = "in"
ip_name[91] = "F/ft"
ip_name[92] = "W/s"
ip_name[93] = "kmol"
ip_name[94] = "Wh"
ip_name[95] = "ton-hrs"
ip_name[96] = "days"
ip_name[97] = "lb/ft2"
ip_name[98] = "lb"
ip_name[99] = "kmol/s"
ip_name[100] = "percent/F"
ip_name[101] = "lb/s2"
ip_name[102] = "lb/mol"
ip_name[103] = "deltaBtu/lb"
ip_name[104] = "person/ft2"
ip_name[105] = "ft2/person"
ip_name[106] = "Btu/h-person"
ip_name[107] = "W/person"
ip_name[108] = "W/m2"
ip_name[109] = "ft3/person"
ip_name[110] = "ft3/hr-person"
ip_name[111] = "ft3/ft2"
ip_name[112] = "ft3/hr-ft2"
ip_name[113] = "ft3/hr"
ip_name[114] = "s/ft"
ip_name[115] = "W/ft2"
ip_name[116] = "ft2/ft"
ip_name[117] = "pint/day"
ip_name[118] = "pint/kWh"
ip_name[119] = "lb/psi-s-ft2"
ip_name[120] = "ft/hr"
ip_name[121] = "Mode"
ip_name[122] = "Control"
ip_name[123] = "Availability"
ip_name[124] = "rev/min"
ip_name[125] = "W/(ft3/min)"
ip_name[126] = "Btu/h-ft-F"
ip_name[127] = "VA"
ip_name[128] = "lbf-in"
ip_name[129] = "(ft3/min)/(Btu/h)"
ip_name[130] = "inch2"
ip_name[131] = "lb/ft"
ip_name[132] = "Pa"
ip_name[133] = "inch/yr"
ip_name[134] = "1/hr"
ip_name[135] = "ppm"
ip_name[136] = "Btu/h-F3-ft"
ip_name[137] = "kg/m-s"
ip_name[138] = "kg/m-s-F"
ip_name[139] = "kg/m-s-F2"
ip_name[140] = "J/kg-K3"
ip_name[141] = "ms"
ip_name[142] = "Ah"
ip_name[143] = "deltaF/hr"
ip_name[144] = "micron"
ip_name[145] = "W/(gal/min)"
ip_name[146] = "W/((gal/min)-ftH20)"
ip_name[147] = "gal/min-ft"
ip_name[148] = "(gal/min)/(Btu/h)"
ip_name[149] = "gal/person"
ip_name[150] = "gal/hr-person"
ip_name[151] = "gal/ft2"
ip_name[152] = "gal/hr-ft2"
ip_name[153] = "gal/hr"
ip_name[154] = "W/((ft3/min)-inH2O)"
# }}}

# mult {{{
mult <- double(length = 154)
mult[1] = 3.28083989501312
mult[2] = 39.3700787401575
mult[3] = 3.4121412858518
mult[4] = 1
mult[5] = 2118.88000328931
mult[6] = 15850.3222370511
mult[7] = 1.8
mult[8] = 2325.83774250441
mult[9] = 1.45037743897283E-04
mult[10] = 0.00029613
mult[11] = 0.00401463
mult[12] = 0.00033455
mult[13] = 6.93481276005548
mult[14] = 1.89563404769544
mult[15] = 1.8
mult[16] = 10.7639104167097
mult[17] = 1.8
mult[18] = 0.646078115385742
mult[19] = 0.00042986
mult[20] = 1
mult[21] = 0.429925
mult[22] = 0.092902267
mult[23] = 0.062428
mult[24] = 2.20462247603796
mult[25] = 0.67196893069637
mult[26] = 35.3146667214886
mult[27] = 264.172037284185
mult[28] = 0.176110194261872
mult[29] = 0.3048
mult[30] = 2.39005736137667E-04
mult[31] = 1.49237004739337E-05
mult[32] = 196.850393700787
mult[33] = 2.2369362920544
mult[34] = 5.678263
mult[35] = 0.316957210776545
mult[36] = 0.555555555555556
mult[37] = 7
mult[38] = 0.000671968949659
mult[39] = 3.73574867724868E-04
mult[40] = 526.565
mult[41] = 1.32889924714692E-04
mult[42] = 2.68096514745308E-05
mult[43] = 0.555555555555556
mult[44] = 0.145038
mult[45] = 0.29523
mult[46] = 10.7639104167097
mult[47] = 16.018
mult[48] = 1
mult[49] = 2.08857913669065E-02
mult[50] = 0.555555555555556
mult[51] = 0.321418310071648
mult[52] = 645.89
mult[53] = 1
mult[54] = 1
mult[55] = 1
mult[56] = 1
mult[57] = 1
mult[58] = 1
mult[59] = 1
mult[60] = 1
mult[61] = 1
mult[62] = 1
mult[63] = 1
mult[64] = 1
mult[65] = 0.555555555555556
mult[66] = 4.89224766847393E-05
mult[67] = 1
mult[68] = 1
mult[69] = 1
mult[70] = 1
mult[71] = 7.93664091373665E-03
mult[72] = 9.51022349025202E-04
mult[73] = 127.13292
mult[74] = 196.85
mult[75] = 2118.6438
mult[76] = 0.097826
mult[77] = 7.93664091373665
mult[78] = 0.951022349025202
mult[79] = 127.13292
mult[80] = 1
mult[81] = 9.28939733269818E-02
mult[82] = 1
mult[83] = 0.293083235638921
mult[84] = 2.83127014102352E-02
mult[85] = 1
mult[86] = 0.52667614683731
mult[87] = 4.72000059660808E-04
mult[88] = 1.04072
mult[89] = 1
mult[90] = 0.3937
mult[91] = 0.54861322767449
mult[92] = 1
mult[93] = 1
mult[94] = 2.77777777777778E-04
mult[95] = 78.9889415481832
mult[96] = 1
mult[97] = 0.204794053596664
mult[98] = 2.2046
mult[99] = 1
mult[100] = 0.555555555555556
mult[101] = 2.2046
mult[102] = 0.0022046
mult[103] = 0.0004299
mult[104] = 9.28939733269818E-02
mult[105] = 10.764961
mult[106] = 3.4121412858518
mult[107] = 1
mult[108] = 1
mult[109] = 35.3146667214886
mult[110] = 35.3146667214886
mult[111] = 3.28083989501312
mult[112] = 3.28083989501312
mult[113] = 35.3146667214886
mult[114] = 0.3048
mult[115] = 0.09290304
mult[116] = 3.28083989501312
mult[117] = 2.11337629827348
mult[118] = 2.11337629827348
mult[119] = 1412.00523459398
mult[120] = 3.28083989501312
mult[121] = 1
mult[122] = 1
mult[123] = 1
mult[124] = 1
mult[125] = 0.0004719475
mult[126] = 0.577796066000163
mult[127] = 1
mult[128] = 8.85074900525547
mult[129] = 621.099127332943
mult[130] = 0.15500031000062
mult[131] = 0.67196893069637
mult[132] = 1
mult[133] = 39.3700787401575
mult[134] = 1
mult[135] = 1
mult[136] = 0.178565727817582
mult[137] = 0.67196893069637
mult[138] = 0.373316072609094
mult[139] = 0.207397818116164
mult[140] = 7.38277359526066E-05
mult[141] = 1
mult[142] = 1
mult[143] = 1.8
mult[144] = 1
mult[145] = 0.0000630902
mult[146] = 0.188582274697355
mult[147] = 4831.17821785317
mult[148] = 4645.27137336702
mult[149] = 264.172037284185
mult[150] = 264.172037284185
mult[151] = 24.5423853466941
mult[152] = 24.5423853466941
mult[153] = 264.172037284185
mult[154] = 0.117556910599482
# }}}

# offset {{{
offset <- double(length = 154)
offset[7] = 32
offset[19] = 7.686
# }}}

# alt {{{
alt <- logical(length = 154)
alt[2] = TRUE
alt[4] = TRUE
alt[6] = TRUE
alt[10] = TRUE
alt[11] = TRUE
alt[12] = TRUE
alt[45] = TRUE
alt[107] = TRUE
alt[108] = TRUE
alt[126] = TRUE
alt[132] = TRUE
alt[145] = TRUE
alt[147] = TRUE
alt[148] = TRUE
alt[149] = TRUE
alt[150] = TRUE
alt[151] = TRUE
alt[152] = TRUE
alt[153] = TRUE
alt[154] = TRUE
# }}}

# multi_unit_name {{{
multi_unit_name <- character(length = 154)
multi_unit_name[1] = "Distance"
multi_unit_name[3] = "Capacity"
multi_unit_name[4] = "Power"
multi_unit_name[5] = "VolumetricFlow"
multi_unit_name[7] = "Temperature"
multi_unit_name[9] = "Pressure"
multi_unit_name[13] = "Conductivity"
multi_unit_name[15] = "DeltaTemperature"
multi_unit_name[19] = "Enthalpy"
multi_unit_name[23] = "Density"
multi_unit_name[24] = "MassFlow"
multi_unit_name[28] = "ConvectionCoefficient"
multi_unit_name[30] = "SpecificHeat"
multi_unit_name[32] = "Velocity"
multi_unit_name[49] = "Viscosity"
multi_unit_name[53] = "Angle"
multi_unit_name[56] = "Dimensionless"
multi_unit_name[60] = "Percent"
multi_unit_name[94] = "Energy"
multi_unit_name[107] = "ActivityLevel"
multi_unit_name[120] = "PrecipitationRate"
multi_unit_name[121] = "Mode"
multi_unit_name[122] = "Control"
multi_unit_name[123] = "Availability"
multi_unit_name[129] = "VolumetricFlowPerPower"
multi_unit_name[144] = "Wavelength"
# }}}

conversion_units_record <- data.table(si_name, ip_name, mult, offset, alt, multi_unit_name)
# }}}
