#' @importFrom progress progress_bar
#' @importFrom data.table data.table between setnames dcast.data.table setorder
#' @importFrom data.table setcolorder setattr
#' @importFrom stringr str_trim
#' @importFrom purrr map2
# parse_idd_file {{{
parse_idd_file <- function(path) {
    # set progress bar
    pb <- progress::progress_bar$new(
           format = "  Parsing IDD (:what) [:bar] :percent in :elapsed",
           total = 100, clear = FALSE)

    # show progress bar
    pb$tick(0)

    pb$update(0.1, tokens = list(what = "Initialize"))
    # read idd string, get idd version and build
    idd_str <- read_idd_str(path)
    idd_version <- get_idd_ver(idd_str)
    idd_build <- get_idd_build(idd_str)

    idd_dt <- data.table::data.table(
        line = seq_along(idd_str), string = idd_str, key = "line")

    pb$update(0.2, tokens = list(what = "Parsing "))
    # Delete all comment and blank lines
    line_comment <- idd_dt[startsWith(string, "!"), line]
    line_blank <- idd_dt[nchar(string) == 0L, line]
    idd_dt[!(line %in% c(line_comment, line_blank))]
    idd_dt <- idd_dt[-c(line_comment, line_blank)]
    # mark type{{{
    # -2, unknown
    type_unknown <- -2L
    # -1, clash
    type_slash <- -1L
    # 0 , group
    type_group <- 0L
    # 1 , class
    type_class <- 1L
    # 2 , class slash
    type_class_slash <- 2L
    # 3 , field
    type_field <- 3L
    # 4 , last field in class
    type_field_last <- 4L
    # 5 , field slash
    type_field_slash <- 5L
    idd_dt[, type := type_unknown]
    # trucate to characters left of ! in order to handle cases when there are
    # inline comments starting with "!", e.g.
    # "GrouhdHeatTransfer:Basement:EquivSlab,  ! Supplies ..."
    idd_dt[, explpt_loc := regexpr("!", string, fixed = TRUE)]
    idd_dt[explpt_loc > 1, string := stringr::str_trim(substr(string, 1L, explpt_loc - 1L), "right")]
    idd_dt[, explpt_loc := NULL]
    # categorize all slash lines
    idd_dt[startsWith(string, "\\"), type := type_slash]
    # categorize all lines with trailing comma into class. Lines that
    # have one slash can not be a class
    idd_dt[type == type_unknown & endsWith(string, ","), type := type_class]
    # categorize field lines
    idd_dt[grepl("\\", string, fixed = TRUE) & grepl("^[AaNn]", string), type := type_field]
    # ignore section if exists, e.g. "Simulation Data;"
    line_section <- idd_dt[type == type_unknown][endsWith(string, ";"), which = TRUE]
    if (not_empty(line_section)) {
        idd_dt <- idd_dt[-line_section]
    }
    # if there are still known lines, report an error
    line_error_invalid <- idd_dt[type == type_unknown, which = TRUE]
    if (not_empty(line_error_invalid)) {
        parse_error(type = "idd", error = "Invalid line found",
                    msg = idd_dt[line_error_invalid])
    }
    # }}}

    pb$update(0.3, tokens = list(what = "Parsing "))
    # get class names
    idd_dt[type == type_class, class_name := substr(string, 1L, nchar(string) - 1L)]

    pb$update(0.4, tokens = list(what = "Parsing "))
    # get field AN and id
    # {{{
    # get location of first slash
    idd_dt[, slash_loc := regexpr("\\", string, fixed = TRUE)]
    # get combined field AN and id
    idd_dt[type == type_field,
           `:=`(field_anid = stringr::str_trim(substr(string, 1L, slash_loc - 1L), "right"),
                slash_key_value = substr(string, slash_loc, nchar(string)))]
    idd_dt[endsWith(field_anid, ";"), `:=`(type = type_field_last)]
    # clean
    idd_dt[, slash_loc := NULL]

    # handle condensed fields
    # {{{
    idd_dt[, field_count := 0L]
    idd_dt[type %in% c(type_field, type_field_last),
           field_count := stringr::str_count(field_anid, "[,;]")]
    # hats off to Matt Dowle:
    # https://stackoverflow.com/questions/15673662/applying-a-function-to-each-row-of-a-data-table
    idd_dt <- idd_dt[data.table::between(type, type_field, type_field_last),
        {s <- strsplit(field_anid, "\\s*[,;]\\s*");
         list(line = rep(line, sapply(s, length)), V1 = unlist(s))}][
        idd_dt, on = "line"][field_count == 1L, V1 := field_anid][, field_anid := NULL]
    data.table::setnames(idd_dt, "V1", "field_anid")
    # get row numeber of last field per condensed field line in each class
    idd_dt[, row_id := .I]
    line_field_last <- idd_dt[field_count> 1L & type == type_field_last,
        row_id[.N], by = list(line, type)]$V1
    # set all type of condensed field lines to "field"
    idd_dt[field_count > 1L, type := type_field]
    idd_dt[line_field_last, type := type_field_last]
    idd_dt[, row_id := NULL]
    # }}}

    # seperate field AN and id {{{
    idd_dt[field_count == 1L,
        field_anid := stringr::str_trim(substr(field_anid, 1L, nchar(field_anid) - 1L), "right")]
    idd_dt[field_count >= 1L,
           `:=`(field_an = substr(field_anid, 1L, 1L),
                field_id = substr(field_anid, 2L, nchar(field_anid)))]
    # }}}

    # get slash keys and values {{{
    idd_dt[type == type_slash, slash_key_value := string]
    # Remove slash
    idd_dt[!is.na(slash_key_value),
        slash_key_value := stringr::str_trim(
            substr(slash_key_value, 2L, nchar(slash_key_value)), "left")]

    # handle informal field slash keys
    # {{{
    # have to handle some informal slash keys such as '\minimum >0' which should
    # be '\minimum> 0', and '\maximum <100' which should be `\maximum< 100`.
    line_bad_min_exclu <- idd_dt[!is.na(slash_key_value)][
        startsWith(slash_key_value, "minimum >"), line]
    if (not_empty(line_bad_min_exclu)) {
        idd_dt[line %in% line_bad_min_exclu,
        slash_key_value := paste0(
            "minimum> ",
            substr(slash_key_value, 10L, nchar(slash_key_value))
            )
        ]
    }

    line_bad_max_exclu <- idd_dt[!is.na(slash_key_value)][
        startsWith(slash_key_value, "maximum <"), line]
    if (not_empty(line_bad_max_exclu)) {
        idd_dt[line %in% line_bad_max_exclu,
        slash_key_value := paste0(
            "maximum< ",
            substr(slash_key_value, 10L, nchar(slash_key_value))
            )
        ]
    }
    # }}}

    # seperate slash key and value
    idd_dt[, space_loc := regexpr(" ", slash_key_value, fixed = TRUE)]
    # handle cases like "\extensible:2"
    idd_dt[, colon_loc := regexpr(":", slash_key_value, fixed = TRUE)]
    idd_dt[(colon_loc > 0L & space_loc > 0L & colon_loc < space_loc) |
           (space_loc == -1L & colon_loc > 0L), space_loc := colon_loc]
    idd_dt[, colon_loc := NULL]

    idd_dt[space_loc > 0L,
           `:=`(slash_key = toupper(substr(slash_key_value, 1L, space_loc - 1L)),
                slash_value = stringr::str_trim(substr(slash_key_value, space_loc + 1L, nchar(slash_key_value)), "left"))]
    # for "\extensible:<#>" with comments
    idd_dt[space_loc > 0L & slash_key == "EXTENSIBLE" & grepl("\\D$", slash_value),
           slash_value := substr(slash_value, 1L, regexpr("\\D", slash_value) - 1L)]
    idd_dt[space_loc < 0L, `:=`(slash_key = toupper(slash_key_value))]
    # remove trailing tabs in slash keys
    idd_dt[grepl("\t", slash_key), slash_key := gsub(slash_key, "\t", "")]

    # clean up
    idd_dt[, `:=`(space_loc = NULL)]
    # }}}
    # }}}

    pb$update(0.5, tokens = list(what = "Parsing "))
    # parse slash lines {{{
    idd_dt[!is.na(slash_key), slash_supported := FALSE]
    group_slash_key <- c("GROUP")
    class_slash_key <- c("MEMO", "UNIQUE-OBJECT", "REQUIRED-OBJECT",
                         "MIN-FIELDS", "FORMAT", "REFERENCE-CLASS-NAME",
                         "EXTENSIBLE")
    field_slash_key <- c("FIELD", "NOTE", "REQUIRED-FIELD", "UNITS", "IP-UNITS",
                         "UNITSBASEDONFIELD", "MINIMUM", "MINIMUM>", "MAXIMUM",
                         "MAXIMUM<", "DEFAULT", "DEPRECATED", "AUTOSIZABLE",
                         "AUTOCALCULATABLE", "TYPE", "KEY", "OBJECT-LIST",
                         "EXTERNAL-LIST", "REFERENCE", "BEGIN-EXTENSIBLE")
    ignored_slash_key <- c("RETAINCASE", "OBSOLETE")
    # mark group slash key and values
    idd_dt[slash_key %in% group_slash_key,
           `:=`(slash_supported = TRUE, type = type_group, group = slash_value)]
    # mark class slash key and values
    idd_dt[slash_key %in% class_slash_key,
           `:=`(slash_supported = TRUE, type = type_class_slash)]
    # mark field slash key and values
    idd_dt[slash_key %in% field_slash_key,
           `:=`(slash_supported = TRUE, type = type_field_slash)]
    # mark other ignored slash keys
    idd_dt[slash_key %in% ignored_slash_key, slash_supported := TRUE]
    # check for unsupported slash keys
    line_error_slash_key <- idd_dt[slash_supported == FALSE, which = TRUE]
    if (length(line_error_slash_key) > 0L) {
        parse_error(type = "idd", error = "Invalid slash key found.",
                    msg = idd_dt[line_error_slash_key])
    }
    # remove comments for "\extensible:<#>"
    # check for unsupported slash values
    idd_dt[, slash_value_upper := toupper(slash_value)]
    # \type {{{
    line_error_type <- idd_dt[slash_key == "TYPE" &
       !(slash_value_upper %in% c("REAL", "INTEGER", "ALPHA", "CHOICE",
            "OBJECT-LIST", "EXTERNAL-LIST", "NODE")), which = TRUE]
    if (length(line_error_type) > 0) {
        parse_error(type = "idd", error = "Invalid `\\type` found", msg = idd_dt[line_error_type])
    }
    # }}}
    # \external-List {{{
    line_error_external_list <- idd_dt[slash_key == "EXTERNAL-LIST" &
           !(slash_value_upper %in% c("AUTORDDVARIABLE", "AUTORDDMETER",
               "AUTORDDVARIABLEMETER")), which = TRUE]
    if (length(line_error_external_list) > 0) {
        parse_error(type = "idd", error = "Invalid `\\external-list` found",
                    msg = idd_dt[line_error_external_list])
    }
    # }}}
    # \format {{{
    line_error_format <- idd_dt[slash_key %in% "FORMAT" &
        !(slash_value_upper %in% c("SINGLELINE", "VERTICES", "COMPACTSCHEDULE",
            "FLUIDPROPERTY", "VIEWFACTOR", "SPECTRAL")), which = TRUE]
    if (length(line_error_format) > 0) {
        parse_error(type = "idd", error = "Invalid `\\format` found",
                    msg = idd_dt[line_error_format])
    }
    # }}}
    # }}}

    # fix duplicated values
    # {{{
    # fix duplicated fields in "ZoneHVAC:HighTemperatureRadiant" and
    # "Foundation:Kiva"
    # NOTE: there are errors in "ZoneHVAC:HighTemperatureRadiant" that
    # duplicated fields ANid are used for 'N76', 'N77', 'N87' and
    # "Foundation:Kiva" for "N16", have to fix it in advanced.
    # {{{
    # fill class downwards to make search easiser
    dup_field_anid <- idd_dt[data.table::between(type, type_class, type_field_slash)][
        , class_name := class_name[1L], by = list(cumsum(!is.na(class_name)))][
        type == type_field_slash][!is.na(field_anid), list(line, class_name, field_anid)]

    line_dup <- dup_field_anid[
        duplicated(dup_field_anid, by = c("class_name", "field_anid")), line]
    # add a suffix of 'd' to the duplicated field
    if (not_empty(line_dup)) {
        idd_dt[line %in% line_dup,
            `:=`(field_anid = paste0(field_anid, "_dup"),
                 field_id = paste0(field_id, "_dup"))]
    }
    # }}}

    # fix duplicated class slash lines such as "\min-fields 3" in such as
    # "SurfaceProperty:HeatTransferAlgorithm:SurfaceList"
    # {{{
    dup_class_slash <- idd_dt[type %in% c(type_class, type_class_slash)][
        , class_name := class_name[1L], by = list(cumsum(!is.na(class_name)))][
        type == type_class_slash, list(line, class_name, slash_key_value)]
    line_dup <- dup_class_slash[
        duplicated(dup_class_slash, by = c("class_name", "slash_key_value")), line]
    # remove duplicated class slash lines
    if (not_empty(line_dup)) {
        idd_dt <- idd_dt[!(line %in% line_dup)]
    }
    # }}}

    # fix duplicated field slash lines such as "\unit m" in such as
    # "HVACTemplate:Zone:WaterToAirHeatPump"
    # {{{
    dup_field_slash <- idd_dt[
        type %in% c(type_class, type_field, type_field_last, type_field_slash),
        list(type, line, class_name, field_anid, slash_key, slash_key_value)][
        , class_name := class_name[1L], by = list(cumsum(!is.na(class_name)))][
        slash_key != "NOTE"][, field_anid := field_anid[1L],
        by = list(class_name, cumsum(!is.na(field_anid)))][
        type == type_field_slash]

    line_dup <- dup_field_slash[
        duplicated(dup_field_slash, by = c("class_name", "field_anid", "slash_key_value")),
        line]
    # remove duplicated field slash lines
    if (not_empty(line_dup)) {
        idd_dt <- idd_dt[!(line %in% line_dup)]
    }
    # }}}
    # }}}
    # clean
    idd_dt[, `:=`(slash_key_value = NULL, field_anid = NULL)]

    pb$update(0.6, tokens = list(what = "Parsing "))
    idd_dt[, row_id := .I]
    # FIELD data
    # {{{
    # extract class data
    idd_field <- idd_dt[type %in% c(type_class, type_field, type_field_last, type_field_slash),
        .SD, .SDcol = c("row_id", "class_name", "field_an", "field_id", "slash_key", "slash_value")]
    # rolling fill downwards for class and field AN and id
    idd_field[, class_name := class_name[1L], by = list(cumsum(!is.na(class_name)))]
    idd_field[, field_an := field_an[1L], by = list(cumsum(!is.na(field_an)), class_name)]
    idd_field[, field_id := field_id[1L], by = list(cumsum(!is.na(field_id)), class_name)]
    # combine field AN and id again for easing distinguishing fields
    idd_field[, field_anid := paste0(field_an, field_id)][, field_id := NULL]
    # As the first line of each class is a class name which has been filled,
    # delete it
    idd_field <- idd_field[!is.na(slash_key)]
    # if slash key exists and slash value not, it must be a logical attribute,
    # such as "\\unique-object". Set it to TRUE
    idd_field <- idd_field[is.na(slash_value), slash_value := "TRUE"]
    # order class as the sequence the appears in IDD
    idd_field[, class_id := .GRP, by = list(class_name)][, class_name := NULL]
    # using dcast to cast all field attributes into seperated columns
    # get line of field AN and id
    idd_field_line <- idd_field[, list(class_id, field_anid, row_id)]
    idd_field_line <- idd_field_line[
        idd_field_line[, .I[1L], by = list(class_id, field_anid)]$V1]
    # dcast
    idd_field <- data.table::dcast.data.table(idd_field,
        class_id + field_anid + field_an ~ slash_key,
        value.var = "slash_value",
        fun.aggregate = list(function(x) paste0(x, collapse = "\n")), fill = NA)
    # merge line into dcasted table
    idd_field <- merge(idd_field, idd_field_line,
        by = c("class_id", "field_anid"), all.x = TRUE, sort = FALSE)
    # set order according to line
    data.table::setorder(idd_field, row_id)
    # delete line column
    idd_field[, row_id := NULL]

    # add field id
    idd_field[, field_id := .I]

    # set names
    new_nms <- gsub("-", "_", tolower(names(idd_field)), fixed = TRUE)
    data.table::setnames(idd_field, new_nms)

    # order fields per class
    idd_field[, field_index := seq_along(field_anid), by = list(class_id)]

    # add columns if there are no data in the IDD
    if (!has_name(idd_field, "autocalculatable")) idd_field[, autocalculatable := FALSE]
    if (!has_name(idd_field, "autosizable")) idd_field[, autosizable := FALSE]
    if (!has_name(idd_field, "required_field")) idd_field[, required_field := FALSE]
    if (!has_name(idd_field, "unitsbasedonfield")) idd_field[, unitsbasedonfield := FALSE]
    if (!has_name(idd_field, "begin_extensible")) idd_field[, begin_extensible := FALSE]
    if (!has_name(idd_field, "maximum")) idd_field[, maximum := NA_real_]
    if (!has_name(idd_field, "minimum")) idd_field[, minimum := NA_real_]
    if (!has_name(idd_field, "maximum<")) idd_field[, `maximum<` := NA_real_]
    if (!has_name(idd_field, "minimum>")) idd_field[, `minimum>` := NA_real_]
    if (!has_name(idd_field, "reference")) idd_field[, reference := NA_character_]
    if (!has_name(idd_field, "key")) idd_field[, key := NA_character_]
    if (!has_name(idd_field, "default")) idd_field[, default := NA_character_]
    if (!has_name(idd_field, "object_list")) idd_field[, object_list := NA_character_]
    if (!has_name(idd_field, "external_list")) idd_field[, external_list := NA_character_]
    if (!has_name(idd_field, "field")) idd_field[, field := NA_character_]
    if (!has_name(idd_field, "units")) idd_field[, units := NA_character_]
    if (!has_name(idd_field, "ip_units")) idd_field[, ip_units := NA_character_]
    if (!has_name(idd_field, "type")) idd_field[, type := NA_character_]

    # set column type and fill NA
    idd_field[, `:=`(autocalculatable = as.logical(autocalculatable),
                     autosizable = as.logical(autosizable),
                     required_field = as.logical(required_field),
                     unitsbasedonfield = as.logical(unitsbasedonfield),
                     begin_extensible = as.logical(begin_extensible),
                     maximum = as.double(maximum),
                     minimum = as.double(minimum),
                     `maximum<` = as.double(`maximum<`),
                     `minimum>` = as.double(`minimum>`))]

    idd_field[is.na(autocalculatable), autocalculatable := FALSE]
    idd_field[is.na(autosizable), autosizable := FALSE]
    idd_field[is.na(required_field), required_field := FALSE]
    idd_field[is.na(unitsbasedonfield), unitsbasedonfield := FALSE]
    idd_field[is.na(begin_extensible), begin_extensible := FALSE]
    idd_field[is.na(type) & field_an == "N", type := "real"]
    idd_field[is.na(type) & field_an == "A", type := "alpha"]
    idd_field[, `:=`(is_extensible = FALSE,
                     has_default = FALSE,
                     has_reference = FALSE,
                     has_object_list = FALSE,
                     has_external_list = FALSE)]
    first_ext <- idd_field[begin_extensible == TRUE,
        list(first_extensible = min(field_index)), by = class_id]
    idd_field <- first_ext[idd_field, on = "class_id"]
    idd_field[field_index >= first_extensible, is_extensible := TRUE]
    idd_field[!is.na(reference), has_reference := TRUE]
    idd_field[!is.na(object_list), has_object_list := TRUE]
    idd_field[!is.na(external_list), has_external_list := TRUE]
    idd_field[!is.na(default), has_default := TRUE]
    # just ignore the ip unit attributes and use the unit conversion table
    idd_field <- unit_conv_table[unit_conv_table[, .I[1], by = si_name]$V1][
        , units := si_name][idd_field[, ip_units := NULL], on = "units"]
    # add field names
    idd_field[is.na(field), field_name := field_anid]
    idd_field[!is.na(field), field_name := field]
    idd_field[is.na(units), unit := ""]
    idd_field[!is.na(units), unit := paste0("{", units, "}")]
    idd_field[is.na(units), full_name := field_name]
    idd_field[!is.na(units), full_name := paste0(field_name, " ", unit)]
    idd_field[is.na(ip_name), ip_unit := unit]
    idd_field[!is.na(ip_name), ip_unit := paste0("{", ip_name, "}")]
    idd_field[is.na(ip_name), full_ipname := field_name]
    idd_field[!is.na(ip_name), full_ipname := paste0(field_name, " ", ip_unit)]
    idd_field[, is_name := FALSE]
    idd_field[(has_reference == TRUE & has_object_list == FALSE) |
        (full_name == "Name" & type == "alpha"), is_name := TRUE]

    # parse default value
    field_default <- idd_field[has_default == TRUE, .SD, .SDcols = c(
        "field_id", "default", "type", "si_name", "ip_name", "mult", "offset")][
        , `:=`(default_upper = toupper(default),
               default_num = suppressWarnings(as.numeric(default)))][
        , `:=`(default_ipnum = default_num)]
    field_default <- update_value_num(field_default, digits = .options$num_digits,
        in_ip = FALSE, prefix = "default")[, default_id := .I][
        , .SD, .SDcols = c("default_id", "default", "default_upper",
            "default_num", "default_ipnum", "field_id")]

    # add range helper column
    idd_field[, `:=`(has_range = FALSE, lower_incbounds = FALSE, upper_incbounds = FALSE)]
    idd_field[!is.na(minimum), `:=`(has_range = TRUE, lower_incbounds = TRUE)]
    idd_field[!is.na(maximum), `:=`(has_range = TRUE, upper_incbounds = TRUE)]
    idd_field[!is.na(`minimum>`), `:=`(has_range = TRUE, minimum = `minimum>`)]
    idd_field[!is.na(`maximum<`), `:=`(has_range = TRUE, maximum = `maximum<`)]
    idd_field[, `:=`(`minimum>` = NULL, `maximum<` = NULL)]
    field_range <- idd_field[has_range == TRUE,
        list(field_id, minimum, lower_incbounds, maximum, upper_incbounds)][
        , range_id := .I]
    data.table::setcolorder(field_range, c("range_id", "minimum", "lower_incbounds",
                                           "maximum", "upper_incbounds", "field_id"))

    field <- idd_field[, .SD, .SDcols = c("field_id", "class_id", "field_index",
        "field_name", "full_name", "full_ipname", "units", "ip_name",
        "required_field", "type", "autosizable", "autocalculatable",
        "note", "is_name", "is_extensible", "has_default", "has_range",
        "has_reference", "has_object_list", "has_external_list")]
    data.table::setnames(field, "ip_name", "ip_units")

    # split choice
    target_choice <- idd_field[type == "choice"]
    if (is_empty(target_choice)) {
        field_choice <- data.table::data.table(
            choice_id = integer(0), choice = character(0),
            choice_upper = character(0), field_id = integer(0))
    } else {
        field_choice <- target_choice[,
            {s = strsplit(key, "\n", fixed = TRUE);
             list(field_id = rep(field_id, sapply(s, length)),
                  choice = unlist(s))
            }
        ][, `:=`(choice_id = .I, choice_upper = toupper(choice))]
        data.table::setcolorder(field_choice, c("choice_id", "choice", "choice_upper", "field_id"))
    }

    # split reference
    target_reference <- idd_field[has_reference == TRUE]
    if (is_empty(target_reference)) {
        field_reference <- data.table::data.table(
            reference_id = integer(0), reference = character(0),
            field_id = integer(0))
    } else {
        field_reference <- target_reference[,
            {s = strsplit(reference, "\n", fixed = TRUE);
             list(reference = unlist(s),
                  field_id = rep(field_id, sapply(s, length)))
            }
        ]
        data.table::setorder(field_reference, reference, field_id)
        field_reference[, reference_id := .I]
        data.table::setcolorder(field_reference, c("reference_id", "reference", "field_id"))
    }

    # split object-list
    target_object_list <- idd_field[has_object_list == TRUE]
    if (is_empty(target_object_list)) {
        field_object_list <- data.table::data.table(
            object_list_id = integer(0), object_list = character(0),
            field_id = integer(0))
    } else {
        field_object_list <- target_object_list[,
            {s = strsplit(object_list, "\n", fixed = TRUE);
             list(object_list = unlist(s),
                  field_id = rep(field_id, sapply(s, length)))
            }
        ][, object_list_id := .I]
        data.table::setcolorder(field_object_list, c("object_list_id", "object_list", "field_id"))
    }

    # split external-list
    target_external_list <- idd_field[has_external_list == TRUE]
    if (is_empty(target_external_list)) {
        field_external_list <- data.table::data.table(
            external_list_id = integer(0), external_list = character(0),
            field_id = integer(0))
    } else {
        field_external_list <- target_external_list[,
            {s = strsplit(external_list, "\n", fixed = TRUE);
             list(external_list = unlist(s),
                  field_id = rep(field_id, sapply(s, length)))
            }
        ][, external_list_id := .I]
        data.table::setcolorder(field_external_list, c("external_list_id", "external_list", "field_id"))
    }
    # }}}

    pb$update(0.8, tokens = list(what = "Parsing "))
    # CLASS data
    # {{{
    # extract class data
    idd_class <- idd_dt[data.table::between(type, type_group, type_class_slash),
        .SD, .SDcol = c("group", "class_name", "slash_key", "slash_value")]
    # rolling fill downwards for group and class
    idd_class[, group := group[1L], by = list(cumsum(!is.na(group)))]
    data.table::setnames(idd_class, "group", "group_name")
    idd_class[, class_name := class_name[1L], by = list(cumsum(!is.na(class_name)))]
    # As group has been add into a seperated column named "group"and also the
    # last class in one group has been mis-categorized into the next group by
    # the filling process, delete group slash_key
    idd_class <- idd_class[!(slash_key %in% "GROUP")]
    # as the first na in first group can not be replaced using downward filling
    idd_class <- idd_class[!is.na(class_name)]
    # first handle classes without slashes such as "SwimmingPool:Indoor"
    class_no_slash <- idd_class[, .N, by = list(class_name)][N == 1L, class_name]
    idd_class <- idd_class[!class_name %in% class_no_slash & !is.na(slash_key) |
        class_name %in% class_no_slash]
    # if slash key exists and slash value not, it must be a logical attribute,
    # such as "\\unique-object". Set it to TRUE
    idd_class <- idd_class[is.na(slash_value), slash_value := "TRUE"]
    # order group and class as the sequence the appears in IDD
    idd_class[, group_id := .GRP, by = list(group_name)]
    idd_class[, class_id := .GRP, by = list(class_name)]

    group <- unique(idd_class[, list(group_id, group_name)])
    class <- unique(idd_class[, list(class_id, class_name, group_id)])

    idd_class[, c("group_id", "group_name", "class_name") := NULL]
    # using dcast to cast all class attributes into seperated columns
    idd_class <- data.table::dcast.data.table(idd_class,
        class_id ~ slash_key,
        value.var = "slash_value",
        fun.aggregate = list(function(x) paste0(x, collapse = "\n")), fill = NA)
    # delete column "NA" caused by classes without slash such as
    # "SwimmingPool:Indoor"
    if (has_name(idd_class, "NA")) idd_class[, `:=`(`NA` = NULL)]
    # set names
    new_nms <- gsub("-", "_", tolower(names(idd_class)), fixed = TRUE)
    data.table::setnames(idd_class, new_nms)
    # add columns if there are no data in the IDD
    if (!has_name(idd_class, "min_fields")) idd_class[, min_fields := NA_integer_]
    if (!has_name(idd_class, "required_object")) idd_class[, required_object := FALSE]
    if (!has_name(idd_class, "unique_object")) idd_class[, unique_object := FALSE]
    if (!has_name(idd_class, "format")) idd_class[, format := NA_character_]
    if (!has_name(idd_class, "memo")) idd_class[, memo := NA_character_]
    if (!has_name(idd_class, "extensible")) idd_class[, extensible := NA_integer_]
    if (!has_name(idd_class, "reference_class_name")) idd_class[, reference_class_name := NA_character_]
    data.table::setnames(idd_class,
        c("extensible", "reference_class_name"), c("num_extensible", "reference")
    )
    # set column type
    idd_class[, `:=`(min_fields = as.integer(min_fields),
                     required_object = as.logical(required_object),
                     unique_object = as.logical(unique_object),
                     num_extensible = as.integer(num_extensible))]
    # fill na
    idd_class[is.na(format), format := "standard"]
    idd_class[is.na(min_fields), min_fields := 0L]
    idd_class[is.na(required_object), required_object := FALSE]
    idd_class[is.na(unique_object), unique_object := FALSE]
    idd_class[is.na(num_extensible), num_extensible := 0L]
    # get max field per class
    idd_class <- idd_field[, list(num_fields = .N), by = class_id][idd_class, on = "class_id"]

    # add `has_name`
    idd_class[, has_name := FALSE]
    idd_class[class_id %in% idd_field[is_name == TRUE, unique(class_id)], has_name := TRUE]
    class_property <- idd_class[, list(class_id, format, memo, min_fields,
        num_fields, required_object, unique_object, has_name, num_extensible)]
    data.table::setnames(class_property, "format", "class_format")

    # add info about the index of last required field
    last_req <- idd_field[required_field == TRUE,
        list(last_required = field_index[.N]), by = list(class_id)]
    class_property <- last_req[class_property, on = "class_id"][
        is.na(last_required), `:=`(last_required = 0L)]
    # add first extensible index
    class_property <- first_ext[class_property, on = "class_id"][
        is.na(first_extensible), `:=`(first_extensible = 0L)]
    # add num of extensible group
    class_property[, `:=`(num_extensible_group = 0)]
    class_property[num_extensible > 0, `:=`(num_extensible_group =
        (num_fields - first_extensible + 1L) / num_extensible)]

    class <- class[class_property, on = "class_id"][,
        .SD, .SDcols = c("class_id", "class_name", "group_id", "class_format",
            "memo", "min_fields", "num_fields", "required_object",
            "unique_object", "has_name", "last_required", "num_extensible",
            "first_extensible", "num_extensible_group")]

    # split reference_class_name
    class_reference <- idd_class[!is.na(reference),
        {
            s = strsplit(reference, "\n", fixed = TRUE);
            list(reference = unlist(s),
                 class_id = rep(class_id, sapply(s, length)))
        }
    ]
    if (not_empty(class_reference)) {
        data.table::setorder(class_reference, reference, class_id)
    }
    class_reference[, reference_id := .I]
    data.table::setcolorder(class_reference, c("reference_id", "reference", "class_id"))
    # }}}

    pb$update(0.95, tokens = list(what = "Parsing "))
    idd <- list(version = idd_version,
                build = idd_build,
                group = group,
                class = class,
                class_reference = class_reference,
                field = field,
                field_reference = field_reference,
                field_default = field_default,
                field_choice = field_choice,
                field_range = field_range,
                field_object_list = field_object_list,
                field_external_list = field_external_list)

    pb$tick(100L, tokens = list(what = "Complete"))
    idd
}
# }}}

#' @importFrom data.table data.table setorder setnames between last setattr
#' @importFrom data.table setcolorder setattr
#' @importFrom purrr map2
#' @importFrom cli cat_line cat_rule rule
# parse_idf_file {{{
parse_idf_file <- function (path, idd = NULL) {

    idf_str <- read_idd_str(path)

    # get idf version
    idf_ver <- get_idf_ver(idf_str)

    # if idd is missing, use preparsed Idd object
    if (is.null(idd)) {
        if (!is.null(idf_ver)) {
            idd <- use_idd(idf_ver)
        # if no version found, use the latest Idd object
        } else {
            if (!is_parsed_idd_ver(idf_ver))
                stop("Missing version filed in input IDF and none parsed IDD ",
                     "found to use.", call. = FALSE)

            latest_ver <- max(as.numeric_version(names(.globals$idd)))
            warning("Missing version field in input Idf file. The latest Idd ",
                "version ", latest_ver, " will be used. Parsing errors may ",
                "occur.", call. = FALSE)
            idd <- suppressMessages(use_idd(latest_ver))
        }
    } else {
        if (!is_idd(idd))
            idd <- use_idd(idd)

        if (is.null(idf_ver))
            warning("Missing version field in input Idf file. The given Idd ",
                "version ", idd$version(), " will be used. Parsing errors ",
                "may occur.", call. = FALSE)
    }

    # get idd internal environment for parsing
    idd_self <- ._get_self(idd)
    idd_private <- ._get_private(idd)

    idf_dt <- data.table(line = seq_along(idf_str), string = idf_str)

    # mark type {{{
    # -3, unknown
    type_unknown <- -3L
    # -2, speical comment
    type_special <- -2L
    # -1, macro
    type_macro <- -1L
    #  0, block comment
    type_comment <- 0L
    #  1, object
    type_object <- 1L
    #  2, field
    type_field <- 2L
    #  3, last field in an object
    type_field_last <- 3L
    idf_dt[, type := type_unknown]
    data.table::setorder(idf_dt, line, type)

    # delete blank lines
    idf_dt <- idf_dt[!(string %in% "")]
    idf_dt[startsWith(string, "##"), type := type_macro]
    # handle EP-Macro lines {{{
    idf_macro <- idf_dt[type == type_macro]
    idf_macro[, space_loc := regexpr(" ", string, fixed = TRUE)]
    idf_macro[space_loc > 0L,
        `:=`(macro_key = substr(string, 1L, space_loc - 1L),
             macro_value = substr(string, space_loc + 1L, nchar(string)))]
    idf_macro[space_loc < 0L, macro_key := substr(string, 1L, nchar(string))]
    # unknown marco key {{{
    idf_errors_unknown_macro <- idf_macro[!(macro_key %in% macro_dict), list(line, string)]
    if (not_empty(idf_errors_unknown_macro)) {
        parse_error(type = "idf", error = "Unknown macro found", msg = idf_errors_unknown_macro)
    }
    # }}}
    # mark macro values as macro {{{
    macro_value <- idf_macro[!is.na(macro_value), unique(macro_value)]
    is_imf <- ifelse(not_empty(macro_value), TRUE, FALSE)
    idf_dt[string %in% macro_value, type := type_macro]
    # }}}
    # }}}
    idf_dt[startsWith(string, "!"), type := type_comment]
    idf_dt[startsWith(string, "!-"), type := type_special]
    # mark location of "!" and "!-"
    idf_dt[, explpt_loc := regexpr("!", string, fixed = TRUE)]
    idf_dt[, special_loc := regexpr("!-", string, fixed = TRUE)]
    # lines ending with comma and without explaination symbol must be a object
    idf_dt[explpt_loc < 0L & endsWith(string, ","), type := type_object]
    # extract comments with leading spaces in order to preserve the indentation.
    idf_dt[special_loc > 0L,
           comment := substr(string, explpt_loc + 2L, nchar(string))]
    idf_dt[special_loc < 0L & explpt_loc > 0L,
           comment := substr(string, explpt_loc + 1L, nchar(string))]
    # for commented objects
    idf_dt[special_loc > 0L & explpt_loc > 0L,
           comment := substr(string, explpt_loc + 1L, nchar(string))]
    idf_dt[type == type_macro, comment := string]
    # get the number of leading spaces in comment
    idf_dt[, leading_spaces := regexpr("\\S", comment) - 1L]
    # get the value for lines that have comments
    idf_dt[explpt_loc > 1L, value := trimws(substr(string, 1L, explpt_loc - 1L), "right")]
    # get the value for lines without comments
    idf_dt[explpt_loc < 0L, value := string]
    # mark the last field in an object
    idf_dt[endsWith(value, ";"), type := type_field_last]
    # clean unused columns
    idf_dt[, `:=`(explpt_loc = NULL, special_loc = NULL)]
    # }}}

    # special comment key and value {{{
    option_idfeditor <- FALSE
    option_special_format <- FALSE
    option_view_in_ip_units <- FALSE
    option_save <- "sorted"

    idf_option <- idf_dt[type == type_special]
    idf_option[, space_loc := regexpr(" ", comment, fixed = TRUE)]
    idf_option[, `:=`(special_key = toupper(substr(comment, 1L, space_loc - 1L)),
                      special_value = toupper(trimws(substr(comment, space_loc + 1L, nchar(comment)))))]
    idf_option <- idf_option[special_key %in% c("GENERATOR", "OPTION")]
    if (not_empty(idf_option)) {
        idf_option <- idf_option[, strsplit(special_value, " ", fixed = TRUE)[[1]], by = list(line, string, special_key)]
        data.table::setnames(idf_option, "V1", "special_value")
        if (idf_option[special_key == "GENERATOR" & substr(special_value, 1L, 9L) == "IDFEDITOR",
            .N] > 1L) {
            option_idfeditor <- TRUE
        }
        if (idf_option[special_key == "OPTION" & special_value == "USESPECIALFORMAT",
                .N] == 1L) {
            option_special_format <- TRUE
        }
        if (idf_option[special_key == "OPTION" & special_value == "VIEWINIPUNITS",
            .N] == 1L) {
            option_view_in_ip_units <- TRUE
        }
        idf_option[special_key == "OPTION" & special_value == "SORTEDORDER",
                   option_save := "SortedOrder"]
        idf_option[special_key == "OPTION" & special_value == "ORIGINALORDERTOP",
                   option_save := "OriginalOrderTop"]
        idf_option[special_key == "OPTION" & special_value == "ORIGINALORDERBOTTOM",
                   option_save := "OriginalOrderBottom"]
        idf_option_save <- idf_option[!is.na(option_save), list(line, string, option_save)]
        option_save <- idf_option_save[, unique(option_save)]
        if (is_empty(option_save)) {
            option_save <- "sorted"
        } else if (!option_save %in% c("SortedOrder", "OriginalOrderBottom", "OriginalOrderTop")) {
            option_save <- "sorted"
        } else {
            option_save <- switch(option_save,
                SortedOrder = "sorted",
                OriginalOrderTop = "new_top",
                OriginalOrderBottom = "new_bottom")
        }
    }

    header_options = list(
        save_format = option_save,
        special_format = option_special_format,
        view_in_ip = option_view_in_ip_units,
        num_digits = 8L)
    # }}}

    # get rid of special comment lines
    idf_dt <- idf_dt[type != type_special]
    # handle condensed values {{{
    # if the sum of comma and semicolon > 2, then it must be a specially
    # formatted object or field. It may contain a class name, e.g.
    # 'Version,8.8;' and it may not, e.g. '0.0,0.0,0.0,' in
    # 'BuildingSurface:Detailed'.
    # get number of condensed values
    idf_dt[!is.na(value), `:=`(value_count = stringr::str_count(value, "[,;]"))]
    idf_dt[is.na(value), `:=`(value_count = 0L)]
    idf_dt <- idf_dt[!data.table::between(type, type_macro, type_comment)][,
        {s = strsplit(value, "\\s*[,;]\\s*");
         list(line  = rep(line, sapply(s, length)),
              V1 = unlist(s))}][
        idf_dt, on = "line"][value_count == 1L, V1 := value][, value := NULL]
    data.table::setnames(idf_dt, "V1", "value")
    # get row numeber of last field per condensed field line in each class
    line_value_last <- idf_dt[
        value_count > 1L & type == type_field_last,
        list(line_value_last = data.table::last(.I)),
        by = list(line, type)][, line_value_last]
    # set all type of condensed field lines to "field", including class names.
    idf_dt[value_count > 1L, type := type_field]
    # mark last field per object in condensed lines
    idf_dt[line_value_last, type := type_field_last]
    # make lines that only has one value as "field", excluding recognized class
    # names.
    idf_dt[type != type_object & value_count == 1L, type := type_field]
    # }}}

    # set row id
    idf_dt[, row_id := .I]
    # mark last field and remove trailing comma or semicolon in values {{{
    idf_dt[endsWith(value, ","), value := substr(value, 1L, nchar(value) - 1L)]
    idf_dt[endsWith(value, ";"),
           `:=`(type = type_field_last,
                value = substr(value, 1L, nchar(value) - 1L))]
    # }}}

    # set an id for last field per object {{{
    # if is the last field, then the line after last field line should be a
    # class name except the last field is the last non-blank and non-comment
    # line. Others are just normal fields.
    idf_dt[type == type_field_last, object_id := .GRP, by = list(row_id)]
    idf_dt <- idf_dt[!is.na(object_id), list(row_id, object_id)][
        idf_dt[, object_id := NULL], on = c("row_id"), roll = -Inf]
    # }}}

    # COMMENT (MACRO)
    # {{{
    idf_comment <- idf_dt[type %in% c(type_macro, type_comment), .SD,
        .SDcol = c("type", "object_id", "comment")]
    idf_comment[, comment_id := .I]
    comment <- idf_comment[, list(comment_id, comment, type, object_id)]

    # }}}

    # CLASS & FIELD
    # {{{
    # get idf without comments
    # {{{
    # NOTE: currently, inline comments are not supported.
    idf_dt <- idf_dt[!(type %in% c(type_macro, type_comment)), .SD,
         .SDcol = c("row_id", "object_id", "line", "type", "value", "string")]
    # }}}

    # class name should be the same of 'value' column for first line grouped by
    # object_id
    idf_dt[idf_dt[, .I[1], by = object_id]$V1,
           `:=`(type = type_object, class_upper_case = toupper(value))]

    idf_idd_all <- idd_private$m_idd_tbl$class[, class_upper_case := toupper(class_name)][
        idf_dt, on = "class_upper_case", nomatch = NA]
    data.table::setorder(idf_idd_all, object_id, class_id)

    # check for un-recognized class names {{{
    unknown_class <- idf_idd_all[type == type_object][
        !is.na(value)][is.na(class_id), list(line, string)]
    if (not_empty(unknown_class)) {
        parse_error(type = "idf", error = "Object type not recognized", msg = unknown_class)
    }
    # }}}

    # get object table
    object <- idf_idd_all[!is.na(class_upper_case) & type == type_object, list(object_id, class_id)]

    # get value table
    value <- idf_idd_all[!is.na(class_upper_case), list(row_id, class_id)][
        idf_dt, on = "row_id", roll = Inf][type > type_object][
        , field_index := seq_along(.I), by = list(object_id)][
        , value_id := .I][, list(value_id, value, object_id, class_id, field_index, line, string)]

    # handle `Version` object
    # {{{
    # TODO: put this block into `Idf$new()`
    ver_dt <- value[class_id == 1L]
    idd_version <- idd_private$m_version
    if (is_empty(ver_dt)) {
        # add a default version object at the end
        idf_version <- idd_version[,c(1,2)]
        # add a default version object at the end
        ver_obj <- data.table::data.table(
            object_id = max(object[["object_id"]]) + 1L, class_id = 1L
        )
        object <- data.table::rbindlist(list(object, ver_obj))
        ver_val <- data.table::data.table(
            value_id = max(value[["value_id"]]) + 1L,
            value = as.character(idf_version),
            object_id = ver_obj[["object_id"]],
            class_id = 1L,
            field_index = 1L
        )
        value <- data.table::rbindlist(list(value, ver_val), fill = TRUE)
    # check if there are multiple `Version` objects
    } else if (nrow(ver_dt) > 1L) {
        parse_error("idf", "Multiple `Version` objects found in the input.",
                    nrow(ver_dt), ver_dt)
    } else {
        # get version
        idf_version <- as.numeric_version(ver_dt[["value"]])
        if (idf_version != idd_version) {
            warning(msg("Version Mismatch. The file parsing is a differnet ",
                "version ", backtick(idf_version), " than the IDD file you ",
                "are using ", backtick(idd_version), ". Editing and saving ",
                "the file may make it incompatible with an older version of ",
                "EnergyPlus."), call. = FALSE)
        }
    }
    # }}}

    # handle extensible group
    # {{{
    # get field num per object
    num <- value[, list(num_values = .N), by = list(object_id, class_id)]
    # get classes needed to add extensible groups
    ext <- num[idd_private$m_idd_tbl$class, on = "class_id", nomatch = 0L][
        num_fields < num_values, list(object_id, class_name, num_fields, num_values, num_extensible)]
    if (not_empty(ext)) {
        # stop if errors were found
        error_num <- ext[num_extensible == 0L]
        if (not_empty(error_num)) {
            e_fld <- error_num[value, on = "object_id", nomatch = 0L][
                , idx := as.character(field_index), by = line][
                field_index > num_fields, idx := "X", by = line][
                , msg := paste0("  [", idx, "] -> Line ", line, ": ", string)]
            e_fld[e_fld[, .I[1], by = class_id]$V1,
                  msg := paste0("`", num_values, "` fields found for class ",
                backtick(class_name), " with only max ", num_fields, " fields allowed:\n", msg)]
            parse_error("idf", "Too many fields found for class", nrow(error_num), e_fld[["msg"]])
        }

        # add extensible group
        ext_add <- ext[, list(num_values = max(num_values)),
            by = list(class_id, num_extensible, num_fields)]
        ext_add[, num_to_add := ceiling(num_values - num_fields / num_extensible)]

        i_add_extensible_group(idd_self, idd_private, ext_add$class_id, ext_add$num_to_add)
    }
    # }}}
    value_tbl <- idd_private$m_idd_tbl$field[
        value, on = c("class_id", "field_index")]

    value_tbl[grepl("^\\s*$", value), `:=`(value = NA_character_)]

    value_tbl[ , `:=`(value_upper = toupper(value),
                      value_num = suppressWarnings(as.numeric(value)))]
    value_tbl[ , `:=`(value_ipnum = value_num)]
    value <- update_value_num(value_tbl, digits = header_options$num_digits,
                              in_ip = header_options$view_in_ip)[
        , list(value_id, value, value_upper, value_num, value_ipnum, object_id, field_id)]
    data.table::setorder(value, value_id)

    # add object name column
    object <- value_tbl[field_index == 1L][
        is_name == FALSE, `:=`(value = NA_character_, value_upper = NA_character_)][
    , list(object_id, class_id, value, value_upper)]
    data.table::setnames(object, c("value", "value_upper"), c("object_name",  "object_name_upper"))
    data.table::setorder(object, object_id)

    # value reference map
    obj <- value[idd_private$m_idd_tbl$field_object_list,
        on = "field_id", nomatch = 0L, list(value_id, value_upper, object_list)]
    ref <- value[idd_private$m_idd_tbl$field_reference,
        on = "field_id", nomatch = 0L, list(value_id, value_upper, reference)]
    data.table::setnames(ref, "value_id", "reference_value_id")
    value_reference <- unique(obj[ref, on = c(object_list = "reference", "value_upper"),
        nomatch = 0L, list(value_id, reference_value_id)])
    data.table::setorder(value_reference, value_id)
    # }}}

    idf <- list(version = idf_version,
                options = header_options,
                object = object,
                value = value,
                value_reference = value_reference,
                comment = comment)

    data.table::setattr(idf, "class", c("IdfFile", class(idf)))
    data.table::setattr(idf, "is_imf", is_imf)
    data.table::setattr(idf, "idd", idd)

    idf
}
# }}}

#' @importFrom readr read_lines
#' @importFrom data.table data.table ":=" setattr setcolorder
#' @importFrom stringr str_match str_detect
# parse_err_file {{{
parse_err_file <- function (path) {
    if (file.exists(path)) {
        res <- list(completed = FALSE, successful = FALSE, data = data.table::data.table())
        data.table::setattr(res, "class", "ErrFile")
        res
    }

    err_line <- readr::read_lines(path)
    err_dt <- data.table::data.table(string = err_line)

    reg_start <- "^\\s+\\*{5,}\\s+(.*)$"
    reg_w_or_e <- "^\\s*\\**\\s+\\*\\*\\s*([^~\\s\\*]+)\\s*\\*\\*\\s+(.*)$"
    reg_w_or_e_con <- "^\\s*\\**\\s+\\*\\*\\s*~~~\\s*\\*\\*\\s+(.*)$"
    reg_comp_success <- "^\\s*\\*+ EnergyPlus Completed Successfully.*"
    reg_ground_comp_success <- "^\\s*\\*+ GroundTempCalc\\S* Completed Successfully.*"
    reg_comp_unsuccess <- "^\\s*\\*+ EnergyPlus Terminated.*"

    err_dt[, `:=`(message = stringr::str_match(string, reg_start)[, 2])]
    err_dt[, `:=`(seperate = !is.na(message),
                  begin_environment = stringr::str_detect(message, "Beginning"))]
    err_dt[is.na(begin_environment), begin_environment := FALSE]
    err_dt[begin_environment == TRUE, environment_index := .GRP, by = list(message)]
    err_dt[is.na(seperate), seperate := FALSE]

    is_completed <- FALSE
    is_successful <- FALSE
    flg_success <- err_dt[seperate == TRUE,
        any(stringr::str_detect(string, reg_comp_success) |
            stringr::str_detect(string, reg_ground_comp_success))]
    if (flg_success) {
        is_completed <- TRUE
        is_successful <- TRUE
    }
    flg_unsuccess <- err_dt[seperate == TRUE,
        any(stringr::str_detect(string, reg_comp_unsuccess))]
    if (flg_unsuccess) is_completed <- TRUE

    l_final <- err_dt[seperate == TRUE & stringr::str_detect(message, "Final|Simulation Error Summary"),
           which = TRUE]
    if (is_empty(l_final)) l_final <- Inf
    l_warm <- err_dt[seperate == TRUE & stringr::str_detect(message, "EnergyPlus Warmup Error Summary"),
           which = TRUE]
    if (is_empty(l_warm)) l_warm <- Inf
    l_last_valid <- min(l_final, l_warm)

    if (l_last_valid > 0L) err_dt <- err_dt[-(l_last_valid:.N)]

    err_dt[is.na(message), c("level", "message") := {
        res <- stringr::str_match(string, reg_w_or_e)[, 2:3]
        list(res[,1], res[,2])}]
    err_dt[is.na(message),
           `:=`(message = stringr::str_match(string, reg_w_or_e_con)[, 2])]
    err_dt <- err_dt[!is.na(message)]

    err_dt[!is.na(level), `:=`(seperate = TRUE)]
    err_dt[is.na(level) & seperate == TRUE, level := "Info"]
    err_dt[is.na(level) & seperate == FALSE, `:=`(message = paste0("  ", message))]
    err_dt[seperate == TRUE, index := .I]
    err_dt[, `:=`(index = index[1L], level = level[1L]), by = list(cumsum(seperate == TRUE))]
    err_dt[, `:=`(environment_index = environment_index[1L]), by = list(cumsum(begin_environment == TRUE))]
    err_dt <- err_dt[!is.na(index)]

    err_dt[begin_environment == FALSE & level == "Info", `:=`(level_index = data.table::rleid(index))]
    err_dt[begin_environment == TRUE, `:=`(level_index = 0)]
    err_dt[level == "Warning", `:=`(level_index = data.table::rleid(index))]
    err_dt[level == "Severe", `:=`(level_index = data.table::rleid(index))]
    err_dt[level == "Fatal", `:=`(level_index = data.table::rleid(index))]
    err_dt[, `:=`(string = NULL)]
    data.table::setcolorder(err_dt,
        c("level", "message",
          "environment_index", "index", "level_index",
          "seperate", "begin_environment"))

    res <- list(completed = is_completed, successfull = is_successful, data = err_dt)
    data.table::setattr(res, "class", "ErrFile")
    res
}
# }}}

# parse_error {{{
parse_error <- function (type = c("idf", "idd", "err"), error, num, msg = NULL, stop = TRUE) {
    type <- match.arg(type)
    if (is.data.frame(msg)) {
        if (missing(num)) {
            num <- nrow(msg)
        }
        assert_that(has_name(msg, "line"))
        assert_that(has_name(msg, "string"))
        msg <- paste0("Line ", msg$line, ": ", msg$string)
    }

    start_rule <- cli::rule(line = 2L)
    err_type <- paste0("[ Error Type ]: ", error)
    err_num <- paste0("[Total Number]: ", num)

    if (!is.null(msg)) {
        msg_rule <- cli::rule(line = 1L)
        msg_line <- paste(msg, sep = "\n", collapse = "\n")
    } else {
        msg_rule <- NULL
        msg_line <- NULL
    }
    end_rule <- cli::rule(line = 2L)

    all_msg <- paste0(c(start_rule, err_type, err_num, msg_rule, msg_line, end_rule),
        collapse = "\n")

    ori <- getOption("warning.length")
    options(warning.length = 8170L)
    on.exit(options(warning.length = ori), add = TRUE)
    if (stop) {
        stop(paste0(toupper(type)," PARSING ERROR.\n"), all_msg, call. = FALSE)
    } else {
        warning(paste0(toupper(type), " PARSING ERROR.\n"), all_msg, call. = FALSE)
    }
}
# }}}

#' @importFrom stringr str_trim
#' @importFrom readr read_lines
# read_idd_str {{{
read_idd_str <- function(filepath) {
    idd_str <- readr::read_lines(filepath)
    # Have to fix encoding errors in version 8.3 and below
    idd_str <- gsub("\x92", "'", idd_str, useBytes = TRUE, fixed = TRUE)
    idd_str <- gsub("\x93", "\"", idd_str, useBytes = TRUE, fixed = TRUE)
    idd_str <- gsub("\x94", "\"", idd_str, useBytes = TRUE, fixed = TRUE)
    idd_str <- gsub("\xb0", "deg", idd_str, useBytes = TRUE, fixed = TRUE)
    idd_str <- gsub("\xd0", "D", idd_str, useBytes = TRUE, fixed = TRUE)
    # Get rid of leading and trailing spaces
    idd_str <- stringr::str_trim(idd_str, "both")

    return(idd_str)
}
# }}}

# get_idd_ver {{{
get_idd_ver <- function (idd_str) {
    ver_line <- idd_str[grepl("!IDD_Version", idd_str, fixed = TRUE)]

    if (length(ver_line) == 1L) {
        ver <- substr(ver_line, 14L, nchar(ver_line))
        return(standardize_ver(ver))
    } else if (length(ver_line > 1L)) {
        stop("Multiple IDD version found in the input:\n",
             paste0("  ", backtick(ver_line), collapse = "\n"), call. = FALSE)
    } else {
        stop("No IDD version found in the input.", call. = FALSE)
    }
}
# }}}

# get_idd_build {{{
get_idd_build <- function (idd_str) {
    build_line <- idd_str[grepl("!IDD_BUILD", idd_str, fixed = TRUE)]

    if (length(build_line) == 1L) {
        build <- substr(build_line, 12L, nchar(build_line))
        return(build)
    } else if (length(build_line > 1L)) {
        warning("Multiple IDD build tag found in the input:\n",
             paste0("  ", backtick(build_line), collapse = "\n"), call. = FALSE)
    } else {
        warning("No IDD build tag found in the input.", call. = FALSE)
    }
}
# }}}

# get_idf_ver {{{
get_idf_ver <- function (idf_str) {
    ver_normal_cand <- idf_str[endsWith(idf_str, "Version Identifier")]
    ver_normal <- ver_normal_cand[!startsWith(ver_normal_cand, "!")]
    ver_special_cand <- idf_str[startsWith(idf_str, "Version")]
    ver_special <- ver_special_cand[!startsWith(ver_special_cand, "!")]

    if (length(ver_normal) == 1L) {
        # for "8.6; !- Version Identifier"
        standardize_ver(trimws(strsplit(ver_normal, ";", fixed = TRUE)[[1]][1]))
    } else if (length(ver_special) == 1L){
        standardize_ver(trimws(strsplit(ver_special, "[,;]")[[1]][2]))
    } else {
        return(NULL)
    }
}
# }}}
