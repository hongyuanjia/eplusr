#' @importFrom cli rule
#' @importFrom checkmate assert_data_table assert_names
#' @importFrom data.table ":=" "%chin%"
#' @importFrom data.table between chmatch data.table dcast.data.table last
#' @importFrom data.table rbindlist rowidv rleid set setattr setcolorder
#' @importFrom data.table setnames setorder setindexv setnafill
#' @importFrom stringi stri_count_charclass stri_count_fixed stri_detect_fixed
#' @importFrom stringi stri_endswith_fixed stri_extract_first_regex stri_isempty
#' @importFrom stringi stri_length stri_locate_first_fixed stri_replace_all_fixed
#' @importFrom stringi stri_startswith_charclass stri_startswith_fixed
#' @importFrom stringi stri_split_charclass stri_split_fixed stri_sub stri_subset_fixed
#' @importFrom stringi stri_trans_tolower stri_trans_toupper stri_trim_both
#' @importFrom stringi stri_trim_left stri_trim_right stri_locate_first_charclass
#' @include impl.R
NULL

# IDD_SLASHKEY {{{
# nocov start
IDD_SLASHKEY <- list(
    class = list(
        flat = c("unique-object", "required-object", "min-fields", "format",
            "extensible"),
        nest = c("memo")
    ),

    field = list(
        flat = c("field", "required-field", "units", "ip-units",
            "unitsbasedonfield", "minimum", "minimum>", "maximum", "maximum<",
            "default", "autosizable", "autocalculatable", "type",
            "external-list", "begin-extensible",
            # EPW specific
            "missing", "exist-minimum", "exist-minimum>", "exist-maximum", "exist-maximum<"
        ),
        nest = c("note", "key", "object-list", "reference", "reference-class-name")
    ),

    type = list(
        lgl = c("unique-object", "required-object", "required-field",
            "unitsbasedonfield", "autosizable", "autocalculatable",
            "begin-extensible", "deprecated", "obsolete", "retaincase"),
        int = c("min-fields", "extensible"),
        dbl = c("minimum", "minimum>", "maximum", "maximum<",
            # EPW specific
            "exist-minimum", "exist-minimum>", "exist-maximum", "exist-maximum<"
        ),
        chr = c("group", "format", "field", "units", "ip-units", "default", "type",
            "external-list", "missing"),
        lst = c("memo", "reference-class-name", "note", "key", "object-list",
            "reference")
    )
)
# nocov end
# }}}

# IDDFIELD_TYPE {{{
# nocov start
IDDFIELD_TYPE <- list(
    integer = 1L, real = 2L, choice = 3L, alpha = 4L,
    object_list = 5L, node = 6L, external_list = 7L
)
# nocov end
# }}}

# IDDFIELD_SOURCE {{{
# nocov start
IDDFIELD_SOURCE <- list(none = 0L, class = 1L, field = 2L, mixed = 3L)
# nocov end
# }}}

# CLASS_COLS {{{
# nocov start
# names of class columns, mainly used for cleaning unuseful columns
CLASS_COLS <- list(
    index = c("class_id", "class_name", "class_name_us", "group_id"),
    property = c("format", "min_fields", "num_fields", "last_required",
        "unique_object", "required_object", "has_name", "memo",
        "num_extensible", "first_extensible", "num_extensible_group"
    )
)
# nocov end
# }}}

# FIELD_COLS {{{
# nocov start
# names of field columns
FIELD_COLS <- list(
    index = c("field_id", "class_id", "field_index", "field_name", "field_name_us"),
    property = c("field_anid", "units", "ip_units", "is_name",
        "required_field", "extensible_group",
        "type_enum", "src_enum", "type",
        "autosizable", "autocalculatable",
        "has_range", "maximum", "minimum", "lower_incbounds", "upper_incbounds",
        "default_chr", "default_num", "choice", "note",
        # EPW specific
        "missing_chr", "missing_num",
        "has_exist", "exist_maximum", "exist_minimum", "exist_lower_incbounds", "exist_upper_incbounds"
    )
)
# nocov end
# }}}

# parse_idd_file {{{
parse_idd_file <- function(path, epw = FALSE, encoding = "unknown") {
    # read idd string, get idd version and build
    idd_dt <- read_lines(path, encoding = encoding)

    idd_version <- get_idd_ver(idd_dt)
    idd_build <- get_idd_build(idd_dt)

    # delete all comment and blank lines
    idd_dt <- clean_idd_lines(idd_dt)

    # type enum
    type_enum <- list(unknown = 0L, slash = 1L, group = 2L, class = 3L, field = 4L, field_last = 5L)

    # idd_dt[slash_key == "exist-minimum>"]
    # separate lines into bodies, slash keys and slash values
    idd_dt <- sep_idd_lines(idd_dt)

    # mark line types
    idd_dt <- mark_idd_lines(idd_dt, type_enum)

    # group table
    dt <- sep_group_table(idd_dt, type_enum)
    dt_group <- dt$group
    idd_dt <- dt$left

    # class table
    dt <- sep_class_table(idd_dt, type_enum)
    dt_class <- dt$class
    idd_dt <- dt$left

    # field table
    dt_field <- get_field_table(idd_dt, type_enum)

    # dcast class and field tables
    # NOTE: "*_id" column should always put first
    dt_class <- dcast_slash(dt_class, "class_id",
        IDD_SLASHKEY$class, c("group_id", "class_name")
    )
    dt_field <- dcast_slash(dt_field, c("field_id", "field_anid"),
        IDD_SLASHKEY$field, c("class_id")
    )

    # complete property columns
    dt_field <- complete_property(dt_field, "field", dt_class, epw = epw)
    dt_class <- complete_property(dt_class, "class", dt_field, epw = epw)

    # ConnectorList references are missing until v9.1
    # See https://github.com/NREL/EnergyPlus/issues/7172
    if (idd_version < "9.1") {
        id_conlst <- dt_class[J("ConnectorList"), on = "class_name", class_id]
        dt_field[J(id_conlst, c(3L, 5L)), on = c("class_id", "field_index"), `:=`(
            object_list = list("plantconnectors")
        )]

        id_con <- dt_class[J(c("Connector:Mixer", "Connector:Splitter")), on = "class_name", class_id]
        dt_field[J(id_con, c(1L)), on = c("class_id", "field_index"), `:=`(
            reference = list("plantconnectors")
        )]
    }
    # Object-list property missing in HeatPump:PlantLoop:EIR:Cooling A9 since
    # v9.3
    # See https://github.com/NREL/EnergyPlus/issues/7837
    if (idd_version >= "9.3") {
        id_clg <- dt_class[J("HeatPump:PlantLoop:EIR:Cooling"), on = "class_name", class_id]
        dt_field[J(id_clg, 7L), on = c("class_id", "field_index"), `:=`(
            object_list = list("plhpheatingnames"),
            is_name = FALSE, reference = list(NULL)
        )]
    }

    # extract field reference map
    dt <- parse_field_reference_table(dt_field)
    dt_field <- dt$left
    dt_reference <- dt$reference

    list(version = idd_version, build = idd_build, group = dt_group,
        class = dt_class, field = dt_field,
        reference = dt_reference
    )
}
# }}}

# parse_idf_file {{{
parse_idf_file <- function(path, idd = NULL, ref = TRUE, encoding = "unknown") {
    # read IDF string and get version first to get corresponding IDD
    idf_dt <- read_lines(path, encoding = encoding)
    # delete blank lines
    idf_dt <- idf_dt[!stri_isempty(string)]

    idf_ver <- get_idf_ver(idf_dt)

    if (has_ext(path, "ddy")) {
        idd <- withCallingHandlers(get_idd_from_ver(idf_ver, idd),
            eplusr_warning = function(w) invokeRestart("muffleWarning")
        )
    } else {
        idd <- get_idd_from_ver(idf_ver, idd)
    }

    # get idd version and table
    idd_ver <- get_priv_env(idd)$m_version
    idd_env <- get_priv_env(idd)$m_idd_env

    # insert version line if necessary
    if (is.null(idf_ver)) idf_dt <- insert_version(idf_dt, idd_ver)

    # type enum
    type_enum <- list(unknown = 0L, special = 1L, macro = 2L, comment = 3L,
        object = 4L, object_value = 5L, value = 6L, value_last = 7L
    )

    # separate lines into bodies, and comments
    idf_dt <- sep_idf_lines(idf_dt, type_enum)

    # mark line types
    idf_dt <- mark_idf_lines(idf_dt, type_enum)

    # header options
    dt <- sep_header_options(idf_dt, type_enum)
    options <- dt$options
    idf_dt <- dt$left

    # object table
    dt <- sep_object_table(idf_dt, type_enum, idd_env)
    dt_object <- dt$object
    idf_dt <- dt$left

    # value table
    dt_value <- get_value_table(idf_dt, idd_env)

    # combine
    list_idf <- list(options = options, object = dt_object, value = dt_value)

    # update object name
    dt_object <- update_object_name(dt_object, dt_value)

    # IP - SI conversion
    from <- if(options$view_in_ip) "ip" else "si"
    to <- if(.options$view_in_ip) "ip" else "si"
    dt_value <- convert_value_unit(idd_env, dt_value, from, to)

    # value reference map
    if (ref) {
        dt_reference <- get_value_reference_map(idd_env, dt_value, dt_value)
    } else {
        dt_reference <- data.table(
                object_id = integer(0L),     value_id = integer(0L),
            src_object_id = integer(0L), src_value_id = integer(0L),
            src_enum = integer(0L)
        )
    }

    # remove unuseful columns
    set(dt_value, NULL, setdiff(names(dt_value),
        c("value_id", "value_chr", "value_num", "object_id", "field_id")), NULL
    )

    # column order
    setcolorder(dt_object, c("object_id", "object_name", "object_name_lower", "comment", "class_id"))
    setcolorder(dt_value, c("value_id", "value_chr", "value_num", "object_id", "field_id"))

    list(version = idd_ver, options = options,
         object = dt_object, value = dt_value, reference = dt_reference
    )
}
# }}}

# get_idd_ver {{{
get_idd_ver <- function(idd_dt) {
    ver_line <- idd_dt$string[[1L]]

    if (!stri_startswith_fixed(ver_line, "!IDD_Version")) {
        parse_error("idd", "No IDD version on 1st line", idd_dt[1L])
    } else {
        ver <- standardize_ver(stri_sub(ver_line, 14L))

        if (is.na(ver)) parse_error("idd", "Invalid IDD version on 1st line", idd_dt[1L])

        ver
    }
}
# }}}

# get_idd_build {{{
get_idd_build <- function(idd_dt) {
    build_line <- idd_dt$string[[2L]]

    if (!stri_startswith_fixed(build_line, "!IDD_BUILD")) {
        NA_character_
    } else {
        stri_sub(build_line, 12L)
    }
}
# }}}

# get_idf_ver {{{
get_idf_ver <- function(idf_dt) {
    is_ver <- stri_startswith_fixed(idf_dt$string, "Version",
        opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE)
    )

    ver_line_spe <- idf_dt[is_ver]
    ver_line_nor <- idf_dt[which(is_ver) + 1L]

    reg_ver <- "(\\d+\\.\\d+(?:\\.\\d+)*)"
    set(ver_line_spe, NULL, "version",
        stri_match_first_regex(
            ver_line_spe$string, paste0("Version\\s*,\\s*", reg_ver, "\\s*;(?:\\s*!.*)*$"),
            opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
        )[, 2L]
    )
    set(ver_line_nor, NULL, "version", stri_match_first_regex(ver_line_nor$string, paste0("^", reg_ver, "\\s*;"))[, 2L])
    ver_line <- rbindlist(list(ver_line_spe, ver_line_nor), use.names = FALSE)[!is.na(version)]

    if (!nrow(ver_line)) {
        NULL
    } else if (nrow(ver_line) == 1L) {
        ver <- standardize_ver(ver_line$version, complete = FALSE)
        if (is.na(ver)) parse_error("idf", "Invalid IDF version found", ver_line, subtype = "ver")
        attr(ver, "line") <- ver_line$line
        ver
    } else {
        parse_error("idf", "Multiple IDF versions found", ver_line, subtype = "ver")
    }
}
# }}}

# clean_idd_lines {{{
clean_idd_lines <- function(dt) {
    # trucate to characters left of ! in order to handle cases when there are
    # inline comments starting with "!", e.g.
    # "GrouhdHeatTransfer:Basement:EquivSlab,  ! Supplies ..."
    excl_loc <- stri_locate_first_fixed(dt[["string"]], "!")[, 1L]
    i <- which(!is.na(excl_loc))
    if (length(i)) set(dt, i, "string", stri_trim_right(stri_sub(dt[["string"]][i], to = excl_loc[i] - 1L)))

    # remove empty lines
    i <- which(!stri_isempty(dt[["string"]]))
    if (length(i)) dt <- dt[i]

    dt
}
# }}}

# sep_idd_lines {{{
sep_idd_lines <- function(dt, col = "string") {
    # mark first slash
    set(dt, NULL, "slash_loc", stri_locate_first_fixed(dt$string, "\\")[, 1L])

    setindexv(dt, "slash_loc")

    # separate field and slash
    dt[J(NA_integer_), on = "slash_loc", `:=`(body = string)]
    # also remove slash and delete extra spaces, like "\ group Hybrid Model"
    dt[!J(NA_integer_), on = "slash_loc", `:=`(
        body = stri_trim_right(stri_sub(string, to = slash_loc - 1L)),
        slash = stri_trim_left(stri_sub(string, slash_loc + 1L))
    )]

    # separate slash key and values
    set(dt, NULL, "space_loc", stri_locate_first_charclass(dt$slash, "[\\:\\ ]")[, 1L])
    data.table::setnafill(dt, fill = 0L, cols = "space_loc")
    dt[!J(NA_integer_), on = "slash_loc", `:=`(
        slash_key = stri_trans_tolower(stri_sub(slash, to = space_loc - 1L)),
        slash_value = stri_trim_left(stri_sub(slash, space_loc + 1L))
    )]

    setindexv(dt, "slash_key")

    # a) for logical slash key, e.g. "\required-field"
    dt[J(IDD_SLASHKEY$type$lgl), on = "slash_key", `:=`(slash_value = "TRUE")]
    # b) for numeric value slash with comments, e.g. "\extensible:<#> -some comments"
    # https://stackoverflow.com/questions/3575331/how-do-extract-decimal-number-from-string-in-c-sharp/3575807
    dt[J(c(IDD_SLASHKEY$type$int, IDD_SLASHKEY$type$dbl)), on = "slash_key",
        `:=`(slash_value = stri_extract_first_regex(slash_value, "[-+]?([0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?|Inf)"))
    ]

    # change all values of reference, object-list and refercne-class-name to
    # lower case
    dt[J(c("reference", "reference-class-name", "object-list")), on = "slash_key",
        `:=`(slash_value = stri_trans_tolower(slash_value))
    ]
    # check for mismatched object-list and reference
    refs <- dt[J(c("reference-class-name", "reference")), on = "slash_key", unique(slash_value), nomatch = 0L]
    invld_objlst <- which(dt$slash_key == "object-list" & !dt$slash_value %chin% refs)
    if (length(invld_objlst)) {
        parse_error("idd", "Invalid \\object-list value", dt[invld_objlst],
            post = "Neither paired \\reference nor \\reference-class-name exist for \\object-list above."
        )
    }

    # check invalid slash keys
    invld_key <- dt[!J(c(NA_character_, unlist(IDD_SLASHKEY$type))), on = "slash_key", which = TRUE]
    if (length(invld_key))
        parse_error("idd", "Invalid slash key", dt[invld_key])

    # check invalid slash value {{{
    set(dt, NULL, "slash_value_lower", stri_trans_tolower(dt[["slash_value"]]))

    # check invalid \format value
    invld_val <- which(dt$slash_key == "format" &
         !dt$slash_value_lower %chin% c("singleline", "vertices",
             "compactschedule", "fluidproperty", "viewfactor", "spectral")
    )
    if (length(invld_val))
        parse_error("idd", "Invalid format value", dt[invld_val])

    # check invalid \type value
    invld_val <- which(
        dt$slash_key == "type" &
        !dt$slash_value_lower %chin% c("integer", "real", "alpha", "choice",
            "object-list", "external-list", "node")
    )
    if (length(invld_val))
        parse_error("idd", "Invalid type value", dt[invld_val])

    # check invalid \external-list value
    invld_val <- which(
        dt$slash_key == "external-list" &
        !dt$slash_value_lower %chin% c("autorddvariable", "autorddmeter", "autorddvariablemeter")
    )
    if (length(invld_val))
        parse_error("idd", "Invalid external list value", dt[invld_val])
    # }}}

    set(dt, NULL, c("slash", "slash_loc", "space_loc", "slash_value_lower"), NULL)

    dt
}
# }}}

# mark_idd_lines {{{
mark_idd_lines <- function(dt, type_enum) {
    setindexv(dt, "slash_key")

    # add type indicator
    set(dt, NULL, "type", type_enum$unknown)

    set(dt, NULL, "semicolon", stri_endswith_fixed(dt$body, ";"))

    setindexv(dt, "slash_key")

    # mark slash lines
    dt[stri_isempty(body), `:=`(type = type_enum$slash)]

    # mark group
    dt[J("group"), on = "slash_key", `:=`(type = type_enum$group)]

    # mark class
    # cannot use empty slash_key as an indicator because in some versions, class
    # slash may in the same line as class names
    dt[stri_endswith_fixed(body, ","), `:=`(type = type_enum$class)]

    # mark field
    dt[stri_detect_regex(body, "^([AN]\\d+\\s*,\\s*)+|(([AN]\\d+\\s*,\\s*)*[AN]\\d+\\s*;)$"),
        `:=`(type = type_enum$field)
    ]

    # mark last field per class
    dt[J(type_enum$field, TRUE), on = c("type", "semicolon"), `:=`(type = type_enum$field_last)]

    # ignore section if exists, e.g. "Simulation Data;"
    dt <- dt[!J(TRUE, type_enum$unknown), on = c("semicolon", "type")]

    # if there are still known lines, throw an error
    if (any(dt$type == type_enum$unknown)) {
        parse_error("idd", "Invalid line", dt[type == type_enum$unknown])
    }

    set(dt, NULL, "semicolon", NULL)
}
# }}}

# sep_group_table {{{
sep_group_table <- function(dt, type_enum) {
    setindexv(dt, "type")

    is_group <- dt[J(type_enum$group), on = "type", which = TRUE, nomatch = 0L]

    dt[is_group, `:=`(group_id = seq_along(slash_key), group_name = slash_value)]
    dt_group <- dt[is_group, .SD, .SDcols = c("group_id", "group_name", "line")]

    # assign default group if necessary
    if (!nrow(dt_group)) {
        parse_warn("idd", "Missing group name", num = 1L,
            post = "No '\\group' key found. All classes will be assgined to a group named 'Default Group'."
        )

        set(dt, NULL, c("group_id", "group_name"), list(1L, "Default Group"))
        dt_group <- data.table(group_id = 1L, group_name = "Default Group", line = 0L)
    }

    # check missing group
    if (any(dt$line < dt_group$line[1L])) {
        invld_grp <- dt[line < dt_group$line[1L]]
        parse_warn("idd", "Missing group name",
            invld_grp, invld_grp[type == type_enum$class, .N],
            post = "Those classes will be assgined to a group named 'Default Group'.",
        )

        dt[invld_grp, on = "line", `:=`(group_id = 1L, group_name = "Default Group")]
        dt_group <- rbindlist(list(
            data.table(group_id = 1L, group_name = "Default Group", line = 0L),
            dt_group[, `:=`(group_id = group_id + 1L)]
        ))
    }
    set(dt_group, NULL, "line", NULL)

    # fill downwards
    dt[, `:=`(group_id = group_id[1L]), by = cumsum(!is.na(group_id))]

    # remove group lines
    dt <- dt[!J(type_enum$group), on = "type", .SD, .SDcols = !"group_name"]

    list(left = dt, group = dt_group)
}
# }}}

# sep_class_table {{{
sep_class_table <- function(dt, type_enum) {
    setindexv(dt, "type")
    dt[J(type_enum$class), on = "type", `:=`(
        class_id = seq_along(body),
        class_name = stri_trim_right(stri_sub(body, to = -2L))
    )]

    # check duplicated class names
    dup_cls <- dt[J(type_enum$class), on = "type", line[duplicated(class_name)], nomatch = 0L]
    if (length(dup_cls)) {
        invld_cls <- dt[class_name %in% dt[line %in% dup_cls, class_name]]
        parse_error("idd", "Duplicated class names found", invld_cls, length(dup_cls))
    }

    # fill downwards
    dt[, `:=`(class_id = class_id[1L], class_name = class_name[1L]), by = cumsum(!is.na(class_id))]

    # check missing class name
    if (anyNA(dt$class_id)) {
        invld_cls <- dt[is.na(class_id)]
        parse_error("idd", "Missing class name", invld_cls, invld_cls[type == type_enum$field_last, .N])
    }

    # add expected type indicator
    set(dt, NULL, "type_exp", dt$type)
    dt[J(c(type_enum$field, type_enum$field_last)), on = "type", `:=`(type_exp = type_enum$field)]
    l <- dt[J(c(type_enum$field, type_enum$field_last)), on = "type", line[.N], by = class_id, nomatch = 0L]$V1
    dt[J(l), on = "line", `:=`(type_exp = type_enum$field_last)]

    setindexv(dt, c("type", "type_exp"))
    # check missing class name
    mis_cls <- dt[J(type_enum$field_last, type_enum$field), on = c("type", "type_exp"), nomatch = 0L]
    if (nrow(mis_cls)) {
        invld_cls <- dt[class_id %in% mis_cls$class_id]
        invld_cls[, `:=`(line_s = line)]
        invld_cls <- invld_cls[
            invld_cls[, list(line_s = line[1L] + 1L), by = class_id],
            on = list(class_id, line_s > line_s)
        ]
        if (invld_cls[.N, type] == type_enum$field_last) {
            n <- invld_cls[type == type_enum$field_last, .N]
        } else {
            n <- invld_cls[type == type_enum$field_last, .N] + 1L
        }
        parse_error("idd", "Missing class name", invld_cls, n)
    }

    # check incomplete class
    incomp_cls <- dt[J(type_enum$field, type_enum$field_last), on = c("type", "type_exp"), class_id, nomatch = 0L]
    if (length(incomp_cls)) {
        invld_cls <- dt[class_id %in% incomp_cls]
        parse_error("idd", "Incomplete class", invld_cls, length(incomp_cls))
    }

    # after checking possible errors, resign type
    dt[J(type_enum$field_last), on = "type", `:=`(type = type_enum$field)]

    # line when class starts
    s <- dt[, list(start = line[1L]), by = class_id]
    # line when class slash ends
    e <- dt[J(type_enum$field), on = "type", list(end = line[1L] - 1L), by = class_id, nomatch = 0L]
    # join
    i <- merge(s, e, by = "class_id", all = TRUE)

    # manually add "\format" if there is no slash in class
    if (any(same <- i$start == i$end)) {
        setindexv(dt, "class_id")
        # in case that class slash is on the same line as clas name
        ins <- dt[J(i[same, class_id], NA_character_), on = c("class_id", "slash_key"), mult = "first", nomatch = 0L]
        ins[, `:=`(
            string = "\\format standard",
            body = "format standard",
            slash_key = "format",
            slash_value = "standard",
            type = type_enum$slash,
            line = line + 1L
        )]

        # insert
        dt <- rbindlist(list(ins, dt))
        setorderv(dt, "line")

        # change added line number
        dt[dt[J(ins$class_id), on = "class_id", .I[2L], by = class_id, nomatch = 0L]$V1,
            `:=`(line = line - 1L)]
    }

    setindexv(dt, "line")

    # get table
    dt_class <- i[dt, on = list(class_id, start <= line, end >= line),
        nomatch = 0L][!J(NA_character_), on = "slash_key", .SD,
        .SDcols = -c("string", "body", "type", "type_exp", "end")]

    dt <- dt[!J(c(dt_class$start, s$start)), on = "line", .SD,
        .SDcols = -c("line", "string", "type_exp", "group_id", "class_name")]

    set(dt_class, NULL, "start", NULL)

    list(left = dt, class = dt_class)
}
# }}}

# get_field_table {{{
get_field_table <- function(dt, type_enum) {
    # add row indicator
    set(dt, NULL, "row", seq_len(nrow(dt)))

    setindexv(dt, "type")

    # count field number per line
    dt[J(type_enum$field), on = "type", `:=`(field_count = stri_count_charclass(body, "[,;]"))]

    set(dt, NULL, "type", NULL)
    setindexv(dt, "field_count")

    # get all slash lines
    slsh <- dt[J(NA_integer_), on = "field_count", nomatch = 0L]
    set(slsh, NULL, "field_count", NULL)
    set(slsh, NULL, "field_anid", NA_character_)

    # get all single field lines
    sgl <- dt[J(1L), on = "field_count", nomatch = 0L]
    set(sgl, NULL, "field_count", NULL)
    sgl[, `:=`(field_anid = stri_trim_right(stri_sub(body, to = -2L)))]

    # get all condensed field lines
    con <- dt[field_count > 1L]
    set(con, NULL, "field_count", NULL)

    # split fields and slashes
    ## hats off to Matt Dowle:
    ## https://stackoverflow.com/questions/15673662/applying-a-function-to-each-row-of-a-data-table
    con <- con[, {
        s <- stri_split_charclass(body, "[,;]", omit_empty = TRUE)
        l <- vapply(s, length, integer(1L))
        list(
            row = rep(row, l),
            body = rep(body, l),
            class_id = rep(class_id, l),
            slash_key = rep(slash_key, l),
            slash_value = rep(slash_value, l),
            field_anid = stri_trim_both(unlist(s))
        )
    }]

    # combine
    dt_field <- setorderv(rbindlist(list(slsh, sgl, con), use.names = TRUE), "row")

    # fill downwards id and anid
    set(dt_field, NULL, "id", seq_len(nrow(dt_field)))
    left <- dt_field[!is.na(field_anid),
        list(id, field_id = .I, field_anid = stri_trans_toupper(field_anid))
    ]
    set(dt_field, NULL, "field_anid", NULL)
    dt_field <- left[dt_field, on = "id", roll = TRUE]
    set(dt_field, NULL, "id", NULL)

    dt_field
}
# }}}

# dcast_slash {{{
dcast_slash <- function(dt, id, keys, keep = NULL) {
    if (!is.null(keep)) assert_names(names(dt), must.include = keep)

    f <- stats::as.formula(paste0(paste0(id[[1L]], collapse = "+"), "~slash_key"))

    setindexv(dt, "slash_key")
    setindexv(dt, c(id[[1L]], "slash_key"))

    i <- unique(dt[, .SD, .SDcols = c(id, keep)], by = c(id[[1L]]))
    setindexv(i, id[[1L]])

    flat <- unique(dt[J(keys$flat), on = "slash_key", nomatch = 0L], by = c(id[[1L]], "slash_key"))
    if (nrow(flat)) flat <- dcast.data.table(flat, f, value.var = "slash_value")

    nest <- dt[J(keys$nest), on = "slash_key", nomatch = 0L,
        {list(slash_value = list(slash_value))}, by = c(id[[1L]], "slash_key")
    ]
    if (nrow(nest)) nest <- dcast.data.table(nest, f, value.var = "slash_value")

    # combine
    if (nrow(flat) && nrow(nest)) {
        set(flat[i, on = c(id[[1L]])], NULL, setdiff(names(nest), id[[1L]]),
            nest[J(i[[id[[1L]]]]), on = c(id[[1L]]), .SD, .SDcols = -id[[1L]]]
        )
    } else if (!nrow(flat)) {
        nest[i, on = c(id[[1L]])]
    } else if (!nrow(nest)) {
        flat[i, on = c(id[[1L]])]
    } else {
        dt[0L, .SD, .SDcols = c(id, keep)]
    }
}
# }}}

# complete_property {{{
complete_property <- function(dt, type, ref, epw = FALSE) {
    type <- match.arg(type, c("class", "field"))
    keys <- switch(type, class = IDD_SLASHKEY$class, field = IDD_SLASHKEY$field)

    # get slash type from slash key
    slash_type <- function(key) {
        types <- IDD_SLASHKEY$type
        chk <- vapply(types, function(type) key %in% type, logical(1L))
        names(types)[chk]
    }

    # get slash type checking function from slash key
    slash_is_type <- function(key) {
        switch(slash_type(key), lgl = is.logical, int = is.integer, dbl = is.double,
            chr = is.character, lst = is.list)
    }

    # get slash type conversion function from slash key
    slash_as_type <- function(key) {
        switch(slash_type(key), lgl = as.logical, int = as.integer, dbl = as.double,
            chr = as.character, lst = c)
    }

    # get slash initial value from slash key
    slash_init_value <- function(key) {
        res <- switch(slash_type(key), lgl = "FALSE", lst = list(), NA_character_)
        slash_as_type(key)(res)
    }

    # convert property column types
    types <- unlist(IDD_SLASHKEY$type, use.names = FALSE)
    for (key in intersect(names(dt), types)) {
        if (!slash_is_type(key)(dt[[key]])) {
            set(dt, NULL, key, slash_as_type(key)(dt[[key]]))
            set(dt, seq_len(nrow(dt))[is.na(dt[[key]])], key, slash_init_value(key))
        }
    }

    # add missing property columns if necessary
    cols <- unlist(keys, use.names = FALSE)
    if (!epw) cols <- setdiff(cols, c("missing", "exist-minimum", "exist-minimum>", "exist-maximum", "exist-maximum<"))
    for (key in cols) {
        if (!has_names(dt, key)) set(dt, NULL, key, slash_init_value(key))
    }

    dt <- switch(type,
        class = parse_class_property(dt, ref),
        field = parse_field_property(dt, ref, epw = epw)
    )

    dt
}
# }}}

# parse_class_property {{{
parse_class_property <- function(dt, ref) {
    # rename column names to lower case
    nms <- stri_replace_all_fixed(names(dt), "-", "_")
    setnames(dt, nms)

    dt[is.na(format), `:=`(format = "standard")]
    dt[is.na(min_fields), `:=`(min_fields = 0L)]
    dt[is.na(extensible), `:=`(extensible = 0L)]

    # rename
    setnames(dt, "extensible", "num_extensible")

    # get max field per class
    set(dt, NULL, "num_fields", ref[, .N, by = class_id]$N)

    # add `has_name`
    set(dt, NULL, "has_name", ref[, any(is_name), by = class_id]$V1)

    # add info about the index of last required field
    set(dt, NULL, "last_required",
        ref[required_field == TRUE, field_index[.N], by = list(class_id)][
            J(dt$class_id), on = "class_id"][is.na(V1), `:=`(V1 = 0L)]$V1
    )

    # add first extensible index
    set(dt, NULL, "first_extensible",
        ref[extensible_group == 1L, field_index[1L], by = list(class_id)][
            J(dt$class_id), on = "class_id"][is.na(V1), `:=`(V1 = 0L)]$V1
    )

    # add num of extensible group
    set(dt, NULL, "num_extensible_group", 0L)
    dt[num_extensible > 0L, `:=`(
        num_extensible_group = (num_fields - first_extensible + 1L) %/% num_extensible
    )]

    # add underscore class names
    set(dt, NULL, "class_name_us", underscore_name(dt$class_name))

    # only keep useful columns
    ignore <- setdiff(names(dt), unlist(CLASS_COLS, use.names = FALSE))
    if (length(ignore) > 0L) set(dt, NULL, ignore, NULL)
    setcolorder(dt, unlist(CLASS_COLS, use.names = FALSE))

    dt
}
# }}}

# parse_field_property {{{
parse_field_property <- function(dt, ref, epw = FALSE) {
    # rename column names to lower case
    nms <- stri_replace_all_fixed(names(dt), "-", "_")
    setnames(dt, nms)

    # complete types
    dt[is.na(type) & stri_startswith_fixed(field_anid, "A"), `:=`(type = "alpha")]
    dt[is.na(type) & stri_startswith_fixed(field_anid, "N"), `:=`(type = "real")]

    # add field index
    set(dt, NULL, "field_index", rowidv(dt, "class_id"))

    # transform all type values to lower-case
    set(dt, NULL, "type", stri_trans_tolower(dt[["type"]]))

    # add an integer-based field type column
    t <- stri_replace_all_fixed(names(IDDFIELD_TYPE), "_", "-")
    names(t) <- IDDFIELD_TYPE
    set(dt, NULL, "type_enum", as.integer(chmatch(dt$type, t)))

    # rename column `key` to `choice`
    setnames(dt, "key", "choice")

    # add extensible indicator
    dt <- parse_field_property_extensible_group(dt, ref)

    # parse field name
    dt <- parse_field_property_name(dt)

    # parse field range
    dt <- parse_field_property_range(dt)

    # parse field default
    dt <- parse_field_property_default(dt)

    # EPW specific
    if (epw) {
        # parse EPW missing field
        dt <- parse_field_property_missing(dt)
        # parse EPW range of existing field
        dt <- parse_field_property_exist(dt)
    }

    # add lower underscore name
    set(dt, NULL, "field_name_us", stri_trans_tolower(underscore_name(dt$field_name)))

    col_ref <- c("reference", "reference_class_name", "object_list")

    # only keep useful columns
    ignore <- setdiff(names(dt), c(unlist(FIELD_COLS, use.names = FALSE), col_ref))
    if (length(ignore) > 0L) set(dt, NULL, ignore, NULL)
    setcolorder(dt, intersect(unlist(FIELD_COLS, use.names = FALSE), names(dt)))

    dt
}
# }}}

# parse_field_property_extensible_group {{{
parse_field_property_extensible_group <- function(dt, ref) {
    ext <- dt[begin_extensible == TRUE, list(class_id, field_index)]
    # only count once
    ext <- ext[, list(first_extensible = field_index[1L]), by = class_id]

    # handle the case when there is no extensible fields
    if (!has_names(ref, "extensible") || !nrow(ext)) {
        set(dt, NULL, "extensible_group", 0L)
        return(dt)
    }

    # add extensible field number
    ext <- ref[, list(class_id, num_extensible = as.integer(extensible))][ext, on = "class_id"]
    # NOTE: few chances are classes not marked as extensible but with extensible fields
    ext <- ext[!J(NA_integer_), on = "num_extensible"]

    # add total field number
    set(ext, NULL, "num_fields", dt[ext, on = "class_id", .N, by = class_id]$N)

    # add total extensible group number
    set(ext, NULL, "num_group", 0L)
    # exclude incomplete groups
    ext[, `:=`(num_group = as.integer((num_fields - first_extensible + 1L) / num_extensible))]

    # add field extensible group number
    ext[, `:=`(extensible_group = list(
            c(rep(0L, first_extensible - 1L), rep(seq_len(num_group), each = num_extensible))
        )),
        by = class_id
    ]
    ext <- ext[, {
        n <- num_group * num_extensible + (first_extensible - 1L)
        group <- unlist(extensible_group)
        id <- rep(class_id, n)
        index <- unlist(lapply(n, seq_len))
        list(class_id = id, field_index = index, extensible_group = group)
    }]

    # insert into the main dt
    dt[ext, on = c("class_id", "field_index"), `:=`(extensible_group = ext$extensible_group)]
    dt[is.na(extensible_group), `:=`(extensible_group = 0L)]

    dt
}
# }}}

# parse_field_property_name {{{
parse_field_property_name <- function(dt) {
    # add name indicator
    set(dt, NULL, "is_name", FALSE)
    ## name fields:
    ## a) fields with name equal to "Name" with type being "alpha" or "node"
    ## b) fields can be referenced and does not reference others
    dt[
        (field == "Name" & type %chin% c("alpha", "node")) |
        (!vlapply(reference, is.null) & vlapply(object_list, is.null)),
        `:=`(is_name = TRUE)
    ]

    # fill missing ip units
    unit_dt <- FIELD_UNIT_TABLE[FIELD_UNIT_TABLE[, .I[1], by = si_name]$V1,
        .SD, .SDcols = c("si_name", "ip_name")
    ]
    dt <- unit_dt[dt, on = list(si_name = units)][is.na(ip_units), `:=`(ip_units = ip_name)]
    setnames(dt, "si_name", "units")
    set(dt, NULL, "ip_name", NULL)
    dt[is.na(ip_units) & ip_units == "unknown", `:=`(ip_units = units)]

    # add field names
    setnames(dt, "field", "field_name")
    dt[is.na(field_name), `:=`(field_name = field_anid)]

    dt
}
# }}}

# parse_field_property_default {{{
parse_field_property_default <- function(dt) {
    set(dt, NULL, "default_num", NA_real_)
    set(dt, NULL, "default_chr", dt$default)
    dt[type_enum <= IDDFIELD_TYPE$real, `:=`(default_num = suppressWarnings(as.double(default)))]
    dt
}
# }}}

# parse_field_property_missing {{{
parse_field_property_missing <- function(dt) {
    set(dt, NULL, "missing_num", NA_real_)
    set(dt, NULL, "missing_chr", dt$missing)
    dt[type_enum <= IDDFIELD_TYPE$real, `:=`(missing_num = suppressWarnings(as.double(missing)))]
    dt
}
# }}}

# parse_field_property_range {{{
parse_field_property_range <- function(dt) {
    set(dt, NULL, c("has_range", "lower_incbounds", "upper_incbounds"), FALSE)

    setnames(dt, c("minimum>", "maximum<"), c("minimum_u", "maximum_l"))
    dt[!J(NA_real_), on = "minimum", `:=`(has_range = TRUE, lower_incbounds = TRUE)]
    dt[!J(NA_real_), on = "maximum", `:=`(has_range = TRUE, upper_incbounds = TRUE)]
    dt[!J(NA_real_), on = "minimum_u", `:=`(has_range = TRUE, minimum = minimum_u)]
    dt[!J(NA_real_), on = "maximum_l", `:=`(has_range = TRUE, maximum = maximum_l)]

    set(dt, NULL, c("minimum_u", "maximum_l"), NULL)
    dt
}
# }}}

# parse_field_property_exist {{{
parse_field_property_exist <- function(dt) {
    set(dt, NULL, c("has_exist", "exist_lower_incbounds", "exist_upper_incbounds"), FALSE)

    setnames(dt, c("exist_minimum>", "exist_maximum<"), c("exist_minimum_u", "exist_maximum_l"))

    dt[!J(NA_real_), on = "exist_minimum", `:=`(has_exist = TRUE, exist_lower_incbounds = TRUE)]
    dt[!J(NA_real_), on = "exist_maximum", `:=`(has_exist = TRUE, exist_upper_incbounds = TRUE)]
    dt[!J(NA_real_), on = "exist_minimum_u", `:=`(has_exist = TRUE,
        exist_minimum = exist_minimum_u, exist_lower_incbounds = FALSE)]
    dt[!J(NA_real_), on = "exist_maximum_l", `:=`(has_exist = TRUE,
        exist_maximum = exist_maximum_l, exist_upper_incbounds = FALSE)]

    # by default use minimum and missing code as the lower and upper bound
    dt[!is.na(minimum) & is.na(exist_minimum), `:=`(
        has_exist = TRUE, exist_minimum = minimum, exist_lower_incbounds = lower_incbounds
    )]
    dt[!is.na(missing_num) & is.na(exist_maximum), `:=`(
        has_exist = TRUE, exist_maximum = missing_num, exist_upper_incbounds = FALSE
    )]

    set(dt, NULL, c("exist_minimum_u", "exist_maximum_l"), NULL)
    dt
}
# }}}

# parse_field_reference_table {{{
parse_field_reference_table <- function(dt) {
    # mark source type
    set(dt, NULL, "src_enum", IDDFIELD_SOURCE$none)

    setindexv(dt, "field_id")

    # for \object-list
    obj_fld <- dt[, {
        l <- vapply(object_list, length, integer(1L))
        # handle the case when there is no \object-list
        obj_lst <- if (all(l == 0L)) character(0) else unlist(object_list)
        list(
            class_id = rep(class_id[l > 0L], l[l > 0L]),
            field_id = rep(field_id[l > 0L], l[l > 0L]),
            object_list = obj_lst
        )
    }]
    # fix errors when object-list fields having an type of "alpha"
    dt[obj_fld, on = "field_id", `:=`(type = "object-list", type_enum = IDDFIELD_TYPE$object_list)]

    # for \reference-class-name
    ref_cls <- dt[, {
        l <- vapply(reference_class_name, length, integer(1L))
        # handle the case when there is no \reference-class-name
        enum <- {if (all(l == 0L)) integer(0) else IDDFIELD_SOURCE$class}
        dt[J(field_id[l > 0L]), on = "field_id", src_enum := IDDFIELD_SOURCE$class]
        list(
            reference = unlist(reference_class_name),
            src_field_id = rep(field_id[l > 0L], l[l > 0L]),
            src_class_id = rep(class_id[l > 0L], l[l > 0L]),
            src_enum = enum
        )
    }]

    # for \reference
    ref_fld <- dt[, {
        l <- vapply(reference, length, integer(1L))
        fld <- l > 0L
        mx <- fld & src_enum == IDDFIELD_SOURCE$class
        src_enum[fld] <- IDDFIELD_SOURCE$field
        dt[J(field_id[fld]), on = "field_id", src_enum := IDDFIELD_SOURCE$field]
        dt[J(field_id[mx]), on = "field_id", src_enum := IDDFIELD_SOURCE$mixed]
        list(
            reference = unlist(reference[fld]),
            src_field_id = rep(field_id[fld], l[fld]),
            src_class_id = rep(class_id[fld], l[fld]),
            src_enum = rep(src_enum[fld], l[fld])
        )
    }]

    # handle the case when there is neither no \reference nor \reference-class-name
    if (nrow(ref_fld) == 0L && nrow(ref_cls) == 0L) {
        return(list(
            left = dt,
            reference = data.table(
                class_id = integer(0), field_id = integer(0),
                src_class_id = integer(0), src_field_id = integer(0),
                src_enum = integer(0)
            )
        ))
    }

    # combine \reference and \reference-class-name
    refs <- rbindlist(list(ref_fld, ref_cls), fill = TRUE)

    # combine object list and reference
    obj_ref <- refs[obj_fld, on = list(reference = object_list), allow.cartesian = TRUE]

    set(obj_ref, NULL, "reference", NULL)
    setcolorder(obj_ref, c("class_id", "field_id", "src_class_id", "src_field_id", "src_enum"))

    # remove unuseful columns
    set(dt, NULL, c("object_list", "reference", "reference_class_name"), NULL)

    list(left = dt, reference = obj_ref)
}
# }}}

# sep_idf_lines {{{
sep_idf_lines <- function(dt, type_enum) {
    # mark location of first occurrence "!" and "!-"
    dt[, `:=`(excl_loc = stri_locate_first_fixed(string, "!")[, 1L])]
    dt[, `:=`(spcl_loc = stri_locate_first_fixed(string, "!-")[, 1L])]

    setindexv(dt, c("excl_loc", "spcl_loc"))

    # sep values and comments
    dt[is.na(excl_loc), `:=`(body = string, comment = "")]
    dt[!is.na(excl_loc), `:=`(
        body = stri_trim_right(stri_sub(string, to = excl_loc - 1L)),
        comment = stri_sub(string, excl_loc + 1L)
    )]
    dt[excl_loc <= spcl_loc, `:=`(excl_loc = NA_integer_)]
    dt[is.na(excl_loc) & !is.na(spcl_loc), `:=`(comment = stri_trim_left(stri_sub(comment, 2L)))]

    set(dt, NULL, c("excl_loc", "spcl_loc"), NULL)

    dt
}
# }}}

# mark_idf_lines {{{
mark_idf_lines <- function(dt, type_enum) {
    # add type indicator
    set(dt, NULL, "type", type_enum$unknown)

    setindexv(dt, "type")

    # macro line
    l_m <- dt[stri_startswith_fixed(string, "#"), which = TRUE]
    if (length(l_m)) {
        dt[l_m, c("type", "body", "comment") :=({
            macro <- stri_split_fixed(string, " ", n = 2L, omit_empty = TRUE, simplify = TRUE)[, 1L]
            is_m <- macro %in% MACRO_DICT
            if (any(is_m)) {
                type[is_m] <- type_enum$macro
                body[is_m] <- ""
                comment <- string
            }

            list(type, body, comment)
        })]

        if (nrow(dt[type == type_enum$macro])) {
            parse_warn("idf", "Marco lines found", dt[type == type_enum$macro],
                post = paste0(
                    "Currently, IMF is not fully supported. All ",
                    "EpMacro lines will be treated as normal comments of ",
                    "the nearest downwards object."
                )
            )
        }
    }

    # normal comments
    dt[stri_startswith_fixed(string, "!"), `:=`(type = type_enum$comment)]

    # special comments
    dt[stri_startswith_fixed(string, "!-"), `:=`(type = type_enum$special)]

    # mark values in object
    dt[stri_endswith_fixed(body, ","), `:=`(type = type_enum$value)]

    # mark last value in object
    dt[stri_endswith_fixed(body, ";"), `:=`(type = type_enum$value_last)]

    # if there are still known lines, throw an error
    if (nrow(dt[type == type_enum$unknown]) > 0L) {
        parse_error("idf", "Invalid line found", dt[type == type_enum$unknown], subtype = "line")
    }

    dt
}
# }}}

# sep_header_options {{{
sep_header_options <- function(dt, type_enum) {
    dt_opt <- dt[J(type_enum$special), on = "type"]
    dt <- dt[!dt_opt, on = "line"]

    s <- stri_split_fixed(dt_opt[['comment']], " ", n = 2, simplify = TRUE)
    set(dt_opt, NULL, "header", stri_trans_tolower(s[, 1L]))
    set(dt_opt, NULL, "value", stri_trans_tolower(s[, 2L]))

    # all available options
    opts <- list(
        generator = c(idf_editor = "idfeditor"),
        option = c(
            special_format = "usespecialformat",
            view_in_ip = "viewinipunits",
            sorted = "sortedorder",
            new_top = "originalordertop",
            new_bot = "originalorderbottom"
        )
    )

    # helper
    get_option <- function(header, value) {
        if (header %in% names(opts)) {
            opt <- opts[[header]]
            vals <- stri_split_fixed(value, " ", omit_empty = TRUE)[[1L]]
            names(opt)[opt %in% vals]
        }
    }

    # only parse lines with valid headers
    dt_opt <- dt_opt[header %in% names(opts)]

    opt <- list(idf_editor = FALSE, special_format = FALSE, view_in_ip = FALSE,
        save_format = "sorted")

    res <- list(left = dt, options = opt)

    if (!nrow(dt_opt)) return(res)

    out <- unlist(dt_opt[, `:=`(options = list(get_option(header, value))), by = line]$options)

    if (!length(out)) return(res)

    save_format <- c("sorted", "new_top", "new_bot")
    for (lgl in setdiff(out, save_format)) res$options[[lgl]] <- TRUE
    sf <- save_format[save_format %in% out][1L]
    if (!is.na(sf)) res$options$save_format <- sf

    res
}
# }}}

# sep_object_table {{{
sep_object_table <- function(dt, type_enum, idd) {
    # object id
    left <- dt[J(type_enum$value_last), on = "type", list(line, object_id = seq_along(line)), nomatch = 0L]
    dt <- left[dt, on = "line", roll = -Inf]

    # check incomplete object
    incomp_obj <- dt[is.na(object_id) & type >= type_enum$value]
    if (nrow(incomp_obj)) {
        parse_error("idf", "Incomplete object", dt[is.na(object_id)], 1L, subtype = "object")
    }

    # extract class names
    dt[dt[type >= type_enum$value, .I[1L], by = list(object_id)]$V1,
        c("type", "class_name_lower", "body") :=({
            n <- stri_locate_first_fixed(body, ",")[, 1L]
            # in case there are invalid class names ends with semicolon
            n[is.na(n)] <- 0L
            l <- stri_length(body)
            class_name_lower <- stri_trim_right(stri_trans_tolower(stri_sub(body, to = n - 1L)))
            body[n < l] <- stri_trim_left(stri_sub(body[n < l], n[n < l] + 1L))
            type[n <  l] <- type_enum$object_value
            type[n == l] <- type_enum$object
            list(type, class_name_lower, body)
        })
    ]

    # add class id and name
    set(idd$class, NULL, "class_name_lower", stri_trans_tolower(idd$class$class_name))
    dt[!is.na(class_name_lower), c("class_id", "class_name", "group_id") := ({
        nm_in <- class_name_lower
        cls <- idd$class[J(nm_in), on = "class_name_lower"]
        list(cls$class_id, cls$class_name, cls$group_id)
    })]
    set(idd$class, NULL, "class_name_lower", NULL)

    # check for version {{{
    id_ver <- dt[class_name == "Version", object_id]

    # if multiple version found, stop
    if (length(id_ver) > 1L) {
        parse_error("idf", "Multiple IDF versions found", dt[object_id %in% id_ver], length(id_ver), subtype = "ver")
    }
    # }}}

    # check invalid class name
    invld_obj <- dt[is.na(class_id) & !is.na(class_name_lower)]
    if (nrow(invld_obj)) {
        parse_error("idf", "Invalid class name", invld_obj, subtype = "class")
    }

    # fill class id and class name
    left <- dt[!is.na(class_id), list(line, class_id, class_name, group_id)]
    set(dt, NULL, c("class_id", "class_name", "group_id", "class_name_lower"), NULL)
    dt_cmt <- left[dt[type < type_enum$value], on = "line", roll = -Inf]
    dt_val <- left[dt[type > type_enum$comment], on = "line", roll = Inf]
    dt <- setorderv(rbindlist(list(dt_cmt, dt_val)), "line")

    # lines with type "object" and "object_value" are duplicated
    dt <- unique(dt)

    # get table
    dt_object <- dt[type <= type_enum$object_value, .SD, .SDcols = c("object_id", "class_id", "comment", "type")]

    # clean comment
    clean_comment <- function(x, type) {
        x <- x[type == type_enum$comment]
        if (!length(x)) NULL else x
    }

    dt_object <- dt_object[,
        list(class_id = class_id[1L], comment = list(clean_comment(comment, type))),
        by = object_id
    ]

    # if trailing comments are found, give message
    if (dt_object[.N, is.na(class_id)]) dt_object <- dt_object[-.N]

    dt <- dt[type > type_enum$object]
    # remove unuseful columns
    set(dt, NULL, c("class_name", "group_id", "type", "comment"), NULL)

    list(left = dt, object = dt_object)
}
# }}}

# get_value_table {{{
get_value_table <- function(dt, idd, escape = FALSE) {
    # count value number per line
    set(dt, NULL, "value_count", stri_count_fixed(dt$body, ",") + stri_endswith_fixed(dt$body, ";"))

    # in case there are multiple semicolon in one line
    if (any(stri_count_fixed(dt$body, ";") > 1L) && !escape) {
        parse_error("idf", "Invalid line found", dt[stri_count_fixed(body, ";") > 1L], subtype = "line")
    }

    setindexv(dt, "value_count")

    # get all comments and single value lines
    sgl <- dt[value_count < 2L]
    set(sgl, NULL, "value_count", NULL)
    set(sgl, NULL, "value_chr", stri_trim_right(stri_sub(sgl$body, to = -2L)))

    # get all condensed value lines
    con <- dt[value_count > 1L]

    # split values
    con <- con[, {
        s <- stri_split_charclass(body, "[,;]", n = value_count, omit_empty = NA, tokens_only = TRUE)
        l <- vapply(s, length, integer(1L))
        list(
            line = rep(line, l),
            body = rep(body, l),
            string = rep(string, l),
            object_id = rep(object_id, l),
            class_id = rep(class_id, l),
            value_chr = stri_trim_both(unlist(s))
        )
    }]

    # combine
    dt <- setorderv(rbindlist(list(sgl, con), use.names = TRUE), "line")

    # add value id
    set(dt, NULL, "value_id", seq_len(nrow(dt)))
    # add field index
    set(dt, NULL, "field_index", rowidv(dt, "object_id"))

    left <- idd$class[, list(class_id, num_extensible, num_fields)]
    ext <- dt[left, on = list(class_id, field_index > num_fields), nomatch = 0L]
    setnames(ext, "field_index", "num_fields")

    # replace empty value with NA
    dt[stri_isempty(value_chr), `:=`(value_chr = NA_character_)]

    # in order to get the object id with wrong field number
    dt_max <- dt[, list(class_id = class_id[[1L]], field_index = max(field_index)), by = "object_id"]

    # only use the max field index to speed up
    fld <- tryCatch(
        get_idd_field(idd, dt_max$class_id, dt_max$field_index,
            c("type_enum", "src_enum", "is_name", "units", "ip_units"),
            complete = TRUE
        ),
        eplusr_error_invalid_field_index = function(e) e
    )

    # issue parse error if invalid field number found
    if (inherits(fld, "eplusr_error_invalid_field_index")) {
        # get invalid class id and field number
        invld <- set(fld$data, NULL, "field_index", NULL)
        # find which object has invalid field number
        obj <- dt_max[invld, on = c("class_id", field_index = "field_in")]$object_id

        # modify message
        msg <- gsub(" *#\\d+\\|", "-->", gsub("index", "number", fld$message))
        parse_error("idf", "Invalid field number", dt[J(obj), on = "object_id"], post = msg, subtype = "field")
    }

    # bind columns
    # NOTE: make sure all necessary fields are there
    add_rleid(dt_max)
    add_joined_cols(dt_max, fld, "rleid", "object_id")
    set(fld, NULL, c("rleid", "field_in"), NULL)
    set(dt, NULL, "class_id", NULL)
    dt <- dt[fld, on = c("object_id", "field_index")]

    # fill data for missing fields
    dt[is.na(line), `:=`(value_id = new_id(dt, "value_id", length(value_id)))]

    # add numeric type values
    dt[type_enum <= IDDFIELD_TYPE$real, `:=`(value_num = suppressWarnings(as.numeric(value_chr)))]
    # update value_chr upon the numeric value
    dt[!is.na(value_num), `:=`(value_chr = as.character(value_num))]
    setnafill(dt, "locf", cols = "object_id")

    # only keep useful columns
    nms <- c("value_id", "value_chr", "value_num", "object_id", "field_id",
        "is_name", "type_enum", "src_enum", "class_name", "units", "ip_units"
    )
    ignore <- setdiff(names(dt), nms)
    if (length(ignore) > 0L) set(dt, NULL, ignore, NULL)
    setcolorder(dt, nms)

    dt
}
# }}}

# update_object_name {{{
update_object_name <- function(dt_object, dt_value) {
    if (!nrow(dt_value)) return(dt_object)
    dt_nm <- dt_value[is_name == TRUE,
        list(object_name = value_chr, object_name_lower = stri_trans_tolower(value_chr)),
        by = "object_id"]
    if (!nrow(dt_nm)) {
        if (!has_names(dt_object, "object_name")) {
            return(set(dt_object, NULL, c("object_name", "object_name_lower"), NA_character_))
        } else {
            return(dt_object)
        }
    }
    dt_object[dt_nm, on = "object_id", `:=`(object_name = i.object_name, object_name_lower = i.object_name_lower)]
    dt_object
}
# }}}

# convert_value_unit {{{
convert_value_unit <- function(idd_env, dt_value, from, to, type = "value") {
    from <- match.arg(from, c("si", "ip"))
    to <- match.arg(to, c("si", "ip"))

    if (identical(from, to)) return(dt_value)

    if (!has_names(dt_value, "units")) {
        add_field_property(idd_env, dt_value, "units")
        on.exit(set(dt_value, NULL, "units", NULL), add = TRUE)
    }
    if (!has_names(dt_value, "ip_units")) {
        add_field_property(idd_env, dt_value, "ip_units")
        on.exit(set(dt_value, NULL, "ip_units", NULL), add = TRUE)
    }
    val <- dt_value[!is.na(value_num) & !is.na(units), list(value_id, value_num, units, ip_units)]

    if (!nrow(val)) return(dt_value)

    val <- FIELD_UNIT_TABLE[val, on = c(si_name = "units", ip_name = "ip_units")]
    set(val, NULL, c("si_name", "ip_name"), NULL)
    setnames(val, c("si_standard_name", "ip_standard_name"), c("si", "ip"))

    val[, c("value_num") :=
        {
            s <- units::set_units(value_num, get(from)[1L], mode = "standard")
            s <- units::set_units(s, get(to)[1L], mode = "standard")
            units::drop_units(s)
        },
        by = list(si, ip)
    ]

    dt_value[val, on = "value_id", `:=`(value_num = i.value_num, value_chr = as.character(i.value_num))]

    dt_value
}
# }}}

# get_value_reference_map {{{
get_value_reference_map <- function(idd_env, src, value, all = TRUE) {
    empty <- data.table(
            object_id = integer(0L),     value_id = integer(0L),
        src_object_id = integer(0L), src_value_id = integer(0L),
        src_enum = integer(0L)
    )

    # get all values in lower case that are references {{{
    if (!has_names(value, "type_enum")) {
        add_field_property(idd_env, value, "type_enum")
        on.exit(set(value, NULL, "type_enum", NULL), add = TRUE)
    }
    val_ref <- value[!is.na(value_chr) & type_enum == IDDFIELD_TYPE$object_list,
        list(object_id, value_id, value_chr = stri_trans_tolower(value_chr), field_id)]
    if (!nrow(val_ref)) return(empty)
    # }}}

    # get field reference map in current IDF
    val_ref_map <- idd_env$reference[val_ref, on = "field_id", allow.cartesian = TRUE]

    # get all values in lower case that are sources {{{
    if (!has_names(src, "class_name")) {
        add_class_name(idd_env, src)
        on.exit(set(src, NULL, "class_name", NULL), add = TRUE)
    }
    if (!has_names(src, "src_enum")) {
        add_field_property(idd_env, src, "src_enum")
        on.exit(set(src, NULL, "src_enum", NULL), add = TRUE)
    }
    val_src <- src[!J(NA_character_), on = "value_chr", .SD,
        .SDcols = c("object_id", "field_id", "value_id", "value_chr", "src_enum", "class_name")
    ]

    setindexv(val_src, "src_enum")

    # a) reference class names
    cls_src <- val_src[J(IDDFIELD_SOURCE$class), on = "src_enum", nomatch = 0L,
        list(
            src_object_id = object_id,
            src_field_id = field_id,
            src_value_id = value_id,
            src_value_chr = class_name,
            src_enum
        )
    ]

    # b) reference field values
    fld_src <- val_src[J(IDDFIELD_SOURCE$field), on = "src_enum", nomatch = 0L,
        list(
            src_object_id = object_id,
            src_field_id = field_id,
            src_value_id = value_id,
            src_value_chr = value_chr,
            src_enum
        )
    ]

    # c) reference both class names and field values
    ## seperate source enum here
    mix_src <- val_src[J(IDDFIELD_SOURCE$mixed), on = "src_enum", nomatch = 0L,
        {
            list(
                src_object_id = c(object_id, object_id),
                src_field_id = c(field_id, field_id),
                src_value_id = c(value_id, value_id),
                src_value_chr = c(value_chr, class_name),
                src_enum = rep(c(IDDFIELD_SOURCE$field, IDDFIELD_SOURCE$class), each = .N)
            )
        }
    ]

    # combine
    val_src <- rbindlist(list(cls_src, fld_src, mix_src))
    set(val_src, NULL, "src_value_chr", stri_trans_tolower(val_src$src_value_chr))
    # }}}

    # match
    ref <- val_ref_map[val_src, on = c(value_chr = "src_value_chr", "src_enum", "src_field_id"),
        allow.cartesian = TRUE, list(object_id, value_id, src_object_id, src_value_id, src_enum)]

    # make sure every reference value has a corresponding source even NA
    if (!all || nrow(ref) == nrow(val_ref)) return(ref)
    ref[J(val_ref$object_id, val_ref$value_id), on = c("object_id", "value_id")]
}
# }}}

# parse_issue {{{
parse_warn <- function(type = c("idf", "idd", "err", "epw"), title, data = NULL,
                        num = NULL, prefix = NULL, suffix = NULL, post = NULL,
                        stop = TRUE, subtype = NULL, loc_name = "Line") {
    parse_issue(type, title, data, num, prefix, suffix, post, stop = FALSE, subtype, loc_name)
}
parse_error <- function(type = c("idf", "idd", "err", "epw"), title, data = NULL,
                         num = NULL, prefix = NULL, suffix = NULL, post = NULL,
                         stop = TRUE, subtype = NULL, loc_name = "Line") {
    parse_issue(type, title, data, num, prefix, suffix, post, stop = TRUE, subtype, loc_name)
}
parse_issue <- function(type = c("idf", "idd", "err", "epw"), title, data = NULL,
                         num = NULL, prefix = NULL, suffix = NULL, post = NULL,
                         stop = TRUE, subtype = NULL, loc_name = "Line") {

    start_rule <- cli::rule(line = 2L)

    mes <- NULL
    if (is.data.frame(data)) {
        if (is.null(num)) {
            num <- nrow(data)
        }
        assert_names(names(data), must.include = c("line", "string"))
        mes <- paste0(data$msg_each, loc_name, " ", lpad(data$line), ": ", data$string)
        if (!is.null(prefix)) mes <- paste0(prefix, mes)
        if (!is.null(suffix)) mes <- paste0(mes, suffix)

        # only show the first 15 message
        if (length(mes) > 10L) {
            mes <- c(mes[1L:10L], "...[truncated. First 10 are shown.]")
        }

        mes <- cli::ansi_strtrim(mes)
    }

    if (stop) {
        err_title <- paste0("[ Error Type ]: ", title)
        err_num  <- paste0("[Total Number]: ", num)
    } else {
        err_title <- paste0("[Warning Type]: ", title)
        err_num  <- paste0("[Total Number]: ", num)
    }

    if (!is.null(mes)) {
        mes_rule <- cli::rule("Location", line = 1L)
        mes_line <- paste(mes, sep = "\n", collapse = "\n")
    } else {
        mes_rule <- NULL
        mes_line <- NULL
    }
    end_rule <- cli::rule(line = 2L)

    if (!is.null(post)) {
        # only show the first 10
        if (length(post) > 10L) {
            post <- c(post[1L:10L], "...[truncated. First 10 are shown.]")
        }
        post <- c(cli::rule("Detail", line = 1L), post)
    }

    all_mes <- paste0(c(start_rule, err_title, err_num, mes_rule, mes_line, post, end_rule),
        collapse = "\n")

    type <- match.arg(type)
    key <- if(stop) "ERROR" else "WARNING"
    all_mes <- paste0(paste0(toupper(type)," PARSING ", key, ".\n"), all_mes)

    type <- paste0("parse_", type)
    subtype <- if (!is.null(subtype)) paste0(type, "_", subtype)

    if (stop) {
        abort(all_mes, c(subtype, type), data = data)
    } else {
        warn(all_mes, c(subtype, type), data = data)
    }
}
# }}}

# insert_version {{{
insert_version <- function(x, ver) {
    if (is.character(x)) {
        paste0(x, "Version, ", standardize_ver(ver)[, 1L:2L], ";")
    } else if (inherits(x, "data.table") && all(has_names(x, c("line", "string")))) {
        n <- if (!nrow(x)) 0L else max(x$line)
        append_dt(x, data.table(line = n + 1L, string = paste0("Version, ", standardize_ver(ver)[, 1L:2L], ";")))
    } else {
        x
    }
}
# }}}

# vim: set fdm=marker:
