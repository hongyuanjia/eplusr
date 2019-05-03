#' @importFrom cli rule
#' @importFrom data.table ":=" "%chin%"
#' @importFrom data.table between chmatch data.table dcast.data.table last
#' @importFrom data.table rbindlist rowidv rleid set setattr setcolorder
#' @importFrom data.table setnames setorder setindexv
#' @importFrom stringi stri_count_charclass stri_count_fixed stri_detect_fixed
#' @importFrom stringi stri_extract_first_regex stri_isempty stri_length
#' @importFrom stringi stri_locate_first_fixed stri_replace_all_fixed
#' @importFrom stringi stri_startswith_fixed stri_split_charclass
#' @importFrom stringi stri_split_fixed stri_sub stri_subset_fixed
#' @importFrom stringi stri_trans_tolower stri_trans_toupper stri_trim_both
#' @importFrom stringi stri_trim_left stri_trim_right
#' @include impl.R
NULL

# IDD_SLASHKEY {{{
IDD_SLASHKEY <- list (
    class = list(
        flat = c("unique-object", "required-object", "min-fields", "format",
            "extensible"),
        nest = c("memo")
    ),

    field = list(
        flat = c("field", "required-field", "units", "ip-units",
            "unitsbasedonfield", "minimum", "minimum>", "maximum", "maximum<",
            "default", "autosizable", "autocalculatable", "type",
            "external-list", "begin-extensible"),
        nest = c("note", "key", "object-list", "reference", "reference-class-name")
    ),

    type = list(
        lgl = c("unique-object", "required-object", "required-field",
            "unitsbasedonfield", "autosizable", "autocalculatable",
            "begin-extensible", "deprecated", "obsolete", "retaincase"),
        int = c("min-fields", "extensible"),
        dbl = c("minimum", "minimum>", "maximum", "maximum<"),
        chr = c("group", "format", "field", "units", "ip-units", "default", "type",
            "external-list"),
        lst = c("memo", "reference-class-name", "note", "key", "object-list",
            "reference")
    )
)
# }}}

# IDDFIELD_TYPE {{{
IDDFIELD_TYPE <- list(
    integer = 1L, real = 2L, choice = 3L, alpha = 4L,
    object_list = 5L, node = 6L, external_list = 7L
)
# }}}

# IDDFIELD_SOURCE {{{
IDDFIELD_SOURCE <- list(none = 0L, class = 1L, field = 2L, mixed = 3L)
# }}}

# CLASS_COLS {{{
# names of class columns, mainly used for cleaning unuseful columns
CLASS_COLS <- list(
    index = c("class_id", "class_name", "class_name_us", "group_id"),
    property = c("format", "min_fields", "num_fields", "last_required",
        "unique_object", "required_object", "has_name", "memo",
        "num_extensible", "first_extensible", "num_extensible_group"
    )
)
# }}}

# FIELD_COLS {{{
# names of field columns
FIELD_COLS <- list(
    index = c("field_id", "class_id", "field_index", "field_name", "field_name_us"),
    property = c("field_anid", "units", "ip_units", "is_name",
        "required_field", "extensible_group",
        "type_enum", "src_enum", "type",
        "autosizable", "autocalculatable",
        "has_range", "maximum", "minimum", "lower_incbounds", "upper_incbounds",
        "default_chr", "default_num", "choice", "note"
    )
)

# }}}

# parse_idd_file {{{
parse_idd_file <- function(path) {
    # read idd string, get idd version and build
    idd_dt <- read_lines(path)
    idd_version <- get_idd_ver(idd_dt)
    idd_build <- get_idd_build(idd_dt)

    # delete all comment and blank lines
    idd_dt <- clean_idd_lines(idd_dt)

    # type enum
    type_enum <- list(unknown = 0L, slash = 1L, group = 2L, class = 3L, field = 4L, field_last = 5L)

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
    dt_class <- dcast_slash(dt_class, "class_id",
        IDD_SLASHKEY$class, c("group_id", "class_name")
    )
    dt_field <- dcast_slash(dt_field, c("field_id", "field_anid"),
        IDD_SLASHKEY$field, c("class_id")
    )
    dt_field[, `:=`(field_id = .I)]

    # complete property columns
    dt_field <- complete_property(dt_field, "field", dt_class)
    dt_class <- complete_property(dt_class, "class", dt_field)

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
parse_idf_file <- function (path, idd = NULL, ref = TRUE) {
    # read IDF string and get version first to get corresponding IDD
    idf_dt <- read_lines(path)
    # delete blank lines
    idf_dt <- idf_dt[!J(""), on = "string"]

    idf_ver <- get_idf_ver(idf_dt, complete = FALSE)

    if (has_ext(path, "ddy")) {
        idd <- withCallingHandlers(get_idd_from_ver(idf_ver, idd),
            warn_given_idd_used = function (w) invokeRestart("muffleWarning"),
            warn_latest_idd_used = function (w) invokeRestart("muffleWarning")
        )
    } else {
        idd <- get_idd_from_ver(idf_ver, idd)
    }

    # get idd version and table
    idd_ver <- ._get_private(idd)$m_version
    idd_env <- ._get_private(idd)$m_idd_env

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
    dt <- sep_object_table(idf_dt, type_enum, idd_ver, idd_env)
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
    dt_value <- convert_value_unit(dt_value, from, to)

    # value reference map
    if (ref) {
        dt_reference <- get_value_reference_map(idd_env$reference, dt_value, dt_value)
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

    list(version = idd_ver, options = options,
        object = dt_object, value = dt_value, reference = dt_reference
    )
}
# }}}

# get_idd_ver {{{
get_idd_ver <- function (idd_dt) {
    assert(inherits(idd_dt, "data.table"), has_name(idd_dt, c("line", "string")))

    ver_line <- idd_dt[stringi::stri_startswith_fixed(string, "!IDD_Version")]

    if (!nrow(ver_line)) {
        abort("error_miss_idd_ver", "No version found in input IDD.")
    } else if (nrow(ver_line) == 1L) {
        ver <- tryCatch(standardize_ver(stri_sub(ver_line$string, 14L)),
            error = function (e) {
                m <- conditionMessage(e)
                if (stringi::stri_startswith_fixed(m, "invalid version specification")) {
                    parse_issue("error_invalid_idd_ver", "idd", "Invalid IDD version", ver_line)
                } else {
                    stop(e)
                }
            }
        )
        standardize_ver(ver)
    } else {
        parse_issue("error_multi_idd_ver", "idd", "Multiple versions found", ver_line)
    }
}
# }}}

# get_idd_build {{{
get_idd_build <- function (idd_dt) {
    assert(inherits(idd_dt, "data.table"), has_name(idd_dt, c("line", "string")))

    build_line <- idd_dt[stringi::stri_startswith_fixed(string, "!IDD_BUILD")]

    if (!nrow(build_line)) {
        abort("error_miss_idd_build", "No build tag found in input IDD.")
    } else if (nrow(build_line) == 1L) {
        build <- stri_sub(build_line$string, 12L)
    } else {
        parse_issue("error_multi_idd_build", "idd", "Multiple build tags found", build_line)
    }
}
# }}}

# get_idf_ver {{{
get_idf_ver <- function (idf_dt, empty_removed = TRUE, complete = TRUE) {
    assert(inherits(idf_dt, "data.table"), has_name(idf_dt, c("line", "string")))

    if (!empty_removed) idf_dt <- idf_dt[!stri_isempty(string)]

    is_ver <- stri_startswith_fixed(idf_dt$string, "Version",
        opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE)
    )

    ver_line_spe <- idf_dt[is_ver]
    ver_line_nor <- idf_dt[which(is_ver) + 1L]

    reg_ver <- "(\\d+\\.\\d+(?:\\.\\d+)*)"
    set(ver_line_spe, NULL, "version",
        stri_match_first_regex(
            ver_line_spe$string, paste0("Version\\s*,\\s*", reg_ver, "\\s*;$"),
            opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
        )[, 2L]
    )
    set(ver_line_nor, NULL, "version", stri_match_first_regex(ver_line_nor$string, paste0("^", reg_ver, "\\s*;"))[, 2L])
    ver_line <- rbindlist(list(ver_line_spe, ver_line_nor), use.names = FALSE)[!is.na(version)]

    if (!nrow(ver_line)) {
        NULL
    } else if (nrow(ver_line) == 1L) {
        standardize_ver(ver_line$version, complete = complete)
    } else {
        parse_issue("error_multiple_version", "idf", "Multiple versions found", ver_line)
    }
}
# }}}

# clean_idd_lines {{{
clean_idd_lines <- function (dt) {
    dt <- dt[!(stringi::stri_startswith_fixed(string, "!") | string == "")]

    # trucate to characters left of ! in order to handle cases when there are
    # inline comments starting with "!", e.g.
    # "GrouhdHeatTransfer:Basement:EquivSlab,  ! Supplies ..."
    dt[, `:=`(excl_loc = stri_locate_first_fixed(string, "!")[, 1L])]
    dt[!is.na(excl_loc), `:=`(
        string = stri_trim_right(
            stri_sub(string, to = excl_loc - 1L)
        )
    )]

    set(dt, NULL, "excl_loc", NULL)
    dt
}
# }}}

# sep_idd_lines {{{
sep_idd_lines <- function (dt, col = "string") {
    # mark first slash
    dt[, `:=`(slash_loc = stri_locate_first_fixed(string, "\\")[, 1L])]

    setindexv(dt, "slash_loc")

    # separate field and slash
    dt[is.na(slash_loc), `:=`(body = string)]
    # also remove slash and delete extra spaces, like "\ group Hybrid Model"
    dt[!is.na(slash_loc), `:=`(
        body = stri_trim_right(stri_sub(string, to = slash_loc - 1L)),
        slash = stri_trim_left(stri_sub(string, slash_loc + 1L))
    )]

    # locate first space and colon
    dt[, `:=`(
        space_loc = stri_locate_first_fixed(slash, " ")[, 1L],
        colon_loc = stri_locate_first_fixed(slash, ":")[, 1L]
    )]
    dt[(colon_loc < space_loc) | (is.na(space_loc) & !is.na(colon_loc)),
        `:=`(space_loc = colon_loc)
    ]
    dt[is.na(space_loc), `:=`(space_loc = 0L)]

    # separate slash key and values
    dt[!is.na(slash_loc), `:=`(
        slash_key = stri_trans_tolower(stri_sub(slash, to = space_loc - 1L)),
        slash_value = stri_trim_left(stri_sub(slash, space_loc + 1L))
    )]

    setindexv(dt, "slash_key")

    # a) for logical slash key, e.g. "\required-field"
    dt[slash_key %chin% IDD_SLASHKEY$type$lgl, `:=`(slash_value = "TRUE")]
    # b) for numeric value slash with comments, e.g. "\extensible:<#> -some comments"
    # https://stackoverflow.com/questions/3575331/how-do-extract-decimal-number-from-string-in-c-sharp/3575807
    dt[slash_key %chin% c(IDD_SLASHKEY$type$int, IDD_SLASHKEY$type$dbl), `:=`(
        slash_value = stri_extract_first_regex(slash_value, "[-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?")
    )]

    # change all values of reference, object-list and refercne-class-name to
    # lower case
    dt[slash_key %chin% c("reference", "reference-class-name", "object-list"),
        `:=`(slash_value = stri_trans_tolower(slash_value))]
    # check for mismatched object-list and reference
    refs <- dt[slash_key %in% c("reference-class-name", "reference"), unique(slash_value)]
    invld_objlst <- dt[slash_key == "object-list" & !slash_value %chin% refs]
    if (nrow(invld_objlst)) {
        parse_issue("error_object_list_value", "idd", "Invalid \\object-list value", invld_objlst)
    }

    # check invalid slash keys
    invld_key <- dt[!is.na(slash_key) & !slash_key %chin% unlist(IDD_SLASHKEY$type), which = TRUE]
    if (length(invld_key))
        parse_issue("error_slash_key", "idd", "Invalid slash key", dt[invld_key])

    # check invalid slash value {{{
    set(dt, NULL, "slash_value_lower", stri_trans_tolower(dt[["slash_value"]]))
    setindexv(dt, c("slash_key", "slash_value_lower"))

    # check invalid \format value
    invld_val <- dt[slash_key == "format" &
        !slash_value_lower %chin% c("singleline", "vertices", "compactschedule",
            "fluidproperty", "viewfactor", "spectral"),
        which = TRUE
    ]
    if (length(invld_val))
        parse_issue("error_format_value", "idd", "Invalid format value", dt[invld_val])

    # check invalid \type value
    invld_val <- dt[slash_key == "type" &
        !slash_value_lower %chin% c("integer", "real", "alpha", "choice",
            "object-list", "external-list", "node"),
        which = TRUE
    ]
    if (length(invld_val))
        parse_issue("error_type_value", "idd", "Invalid type value", dt[invld_val])

    # check invalid \external-list value
    invld_val <- dt[slash_key == "external-list" &
        !slash_value_lower %chin% c("autorddvariable", "autorddmeter", "autorddvariablemeter"),
        which = TRUE
    ]
    if (length(invld_val))
        parse_issue("error_external_list_value", "idd", "Invalid external list value", dt[invld_val])
    # }}}

    set(dt, NULL, c("slash", "slash_loc", "space_loc", "colon_loc", "slash_value_lower"), NULL)

    dt
}
# }}}

# mark_idd_lines {{{
mark_idd_lines <- function (dt, type_enum) {
    setindexv(dt, "slash_key")

    # add type indicator
    set(dt, NULL, "type", type_enum$unknown)

    # ignore section if exists, e.g. "Simulation Data;"
    dt <- dt[!(stringi::stri_endswith_fixed(body, ";") & is.na(slash_key))]

    # mark slash lines
    dt[body == "", `:=`(type = type_enum$slash)]

    # mark group
    dt[slash_key == "group", `:=`(type = type_enum$group)]

    # mark class
    dt[stringi::stri_endswith_fixed(body, ",") & is.na(slash_key), `:=`(type = type_enum$class)]

    # mark field
    dt[body != "" & !is.na(slash_key), `:=`(type = type_enum$field)]

    # mark last field per class
    dt[stringi::stri_endswith_fixed(body, ";"), `:=`(type = type_enum$field_last)]

    # if there are still known lines, throw an error
    if (nrow(dt[type == type_enum$unknown]) > 0L) {
        parse_issue("error_unknown_line", "idd", "Invalid line", dt[type == type_enum$unknown])
    }

    dt
}
# }}}

# sep_group_table {{{
sep_group_table <- function (dt, type_enum) {
    setindexv(dt, "type")

    dt[type == type_enum$group, `:=`(group_id = seq_along(slash_key), group_name = slash_value)]

    dt_group <- dt[type == type_enum$group, .SD, .SDcols = c("group_id", "group_name", "line")]

    # check missing group
    if (nrow(dt[line < dt_group$line[1L]])) {
        invld_grp <- dt[line < dt_group$line[1L]]
        parse_issue("error_missing_group", "idd", "Missing group name",
            invld_grp, invld_grp[type == type_enum$class, .N]
        )
    }
    set(dt_group, NULL, "line", NULL)

    # fill downwards
    dt[, `:=`(group_id = group_id[1L]), by = cumsum(!is.na(group_id))]

    # remove group lines
    dt <- dt[type != type_enum$group]

    set(dt, NULL, "group_name", NULL)

    list(left = dt, group = dt_group)
}
# }}}

# sep_class_table {{{
sep_class_table <- function (dt, type_enum) {
    setindexv(dt, "type")
    dt[type == type_enum$class, `:=`(
        class_id = seq_along(body),
        class_name = stri_trim_right(stri_sub(body, to = -2L))
    )]

    # check duplicated class names
    dup_cls <- dt[type == type_enum$class, line[duplicated(class_name)]]
    if (length(dup_cls)) {
        invld_cls <- dt[class_name %in% dt[line %in% dup_cls]$class_name]
        parse_issue("error_duplicated_class", "idd", "Duplicated class names found",
            invld_cls, length(dup_cls)
        )
    }

    # fill downwards
    dt[, `:=`(class_id = class_id[1L], class_name = class_name[1L]), by = cumsum(!is.na(class_id))]

    # check missing class name
    if (nrow(dt[is.na(class_id)])) {
        invld_cls <- dt[is.na(class_id)]
        parse_issue("error_missing_class", "idd", "Missing class name",
            invld_cls, invld_cls[type == type_enum$field_last, .N]
        )
    }

    # add expected type indicator
    dt[, `:=`(type_exp = type)]
    dt[type > type_enum$class, `:=`(type_exp = type_enum$field)]
    dt[line %in% dt[type > type_enum$class, line[.N], by = class_id]$V1,
        `:=`(type_exp = type_enum$field_last)
    ]

    # check missing class name
    mis_cls <- dt[type == type_enum$field_last & type_exp == type_enum$field]
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
        parse_issue("error_missing_class", "idd", "Missing class name", invld_cls, n)
    }

    # check incomplete class
    incomp_cls <- dt[type == type_enum$field & type_exp == type_enum$field_last, class_id]
    if (length(incomp_cls)) {
        invld_cls <- dt[class_id %in% incomp_cls]
        parse_issue("error_incomplete_class", "idd", "Incomplete class", invld_cls, length(incomp_cls))
    }

    # after checking possible errors, resign type
    dt[type == type_enum$field_last, `:=`(type = type_enum$field)]

    # line when class starts
    s <- dt[, list(start = line[1L]), by = class_id]
    # line when class slash ends
    e <- dt[type == type_enum$field, list(end = line[1L] - 1L), by = class_id]
    # join
    i <- merge(s, e, by = "class_id", all = TRUE)

    # manually add "\format" if there is no slash in class
    if (nrow(i[start == end])) {
        ins <- dt[i[start == end], on = "class_id", mult = "first"]
        ins[, `:=`(
            string = "\\format standard",
            body = "format standard",
            slash_key = "format",
            slash_value = "standard",
            type = type_enum$slash,
            line = line + 1L,
            start = NULL,
            end = NULL
        )]

        # insert
        dt <- rbindlist(list(ins, dt))
        setorderv(dt, "line")

        # change added line number
        dt[dt[class_id %in% ins$class_id, .I[2L], by = class_id]$V1, `:=`(
            line = line - 1L
        )]
    }

    # get table
    dt_class <- i[dt, on = list(class_id, start <= line, end >= line),
        nomatch = 0L][!is.na(slash_key)]

    setnames(dt_class, "start", "line")

    dt <- dt[!dt_class, on = "line"][!line %in% s$start]
    # remove unuseful columns
    set(dt, NULL, c("line", "string", "type_exp", "group_id", "class_name"), NULL)
    set(dt_class, NULL, c("line", "string", "body", "type", "end"), NULL)

    list(left = dt, class = dt_class)
}
# }}}

# get_field_table {{{
get_field_table <- function (dt, type_enum) {
    # add row indicator
    set(dt, NULL, "row", seq_len(nrow(dt)))

    setindexv(dt, "type")

    # count field number per line
    dt[type == type_enum$field, `:=`(field_count = stri_count_charclass(body, "[,;]"))]

    set(dt, NULL, "type", NULL)
    setindexv(dt, "field_count")

    # get all slash lines
    slsh <- dt[is.na(field_count)]
    set(slsh, NULL, "field_count", NULL)
    set(slsh, NULL, "field_anid", NA_character_)

    # get all single field lines
    sgl <- dt[field_count == 1L]
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
dcast_slash <- function (dt, id, keys, keep = NULL) {
    assert(has_name(dt, id))
    assert(has_name(keys, c("flat", "nest")))
    if (!is.null(keep)) assert(has_name(dt, keep))
    set(dt, NULL, "row", seq_len(nrow(dt)))

    # only use the first line of flat slash value
    dt[slash_key %chin% keys$flat, by = c(id, "slash_key"),
        `:=`(slash_value_rleid = seq_along(slash_value))
    ]
    dup_slsh <- dt[slash_value_rleid > 1L, row]
    if (length(dup_slsh)) dt <- dt[-dup_slsh]
    set(dt, NULL, c("row", "slash_value_rleid"), NULL)

    f <- stats::as.formula(paste0(paste0(id, collapse = "+"), "~slash_key"))

    dt_flat <- dt[slash_key %chin% keys$flat]
    if (nrow(dt_flat) == 0L) {
        flat <- dt_flat[, .SD, .SDcols = c(id)]
    } else {
        flat <- dcast.data.table(dt_flat, f, value.var = "slash_value")
    }

    dt_nest <- dt[slash_key %chin% keys$nest]
    if (nrow(dt_nest) == 0L) {
        nest <- dt_nest[, .SD, .SDcols = c(id)]
    } else {
        nest <- dcast.data.table(dt_nest, f, value.var = "slash_value",
            fun.aggregate = list(list)
        )
    }

    # combine
    i <- unique(dt[, .SD, .SDcols = c(id, keep)])
    flat <- merge(i, flat, by = c(id), all = TRUE)
    nest <- merge(i[, .SD, .SDcols = c(id)], nest, by = id, all = TRUE)

    # change empty character member in list to NULL
    idx <- if (stri_startswith_fixed(id[[1L]], "class")) "class_id" else "field_id"
    for (nm in intersect(names(nest), keys$nest)) {
        set(nest, nest[[idx]][vapply(nest[[nm]], function (x) length(x) == 0L, logical(1L))],
            nm, list(list(NULL))
        )
    }

    merge(flat, nest, by = id)
}
# }}}

# complete_property {{{
complete_property <- function (dt, type, ref) {
    type <- match.arg(type, c("class", "field"))
    keys <- switch(type, class = IDD_SLASHKEY$class, field = IDD_SLASHKEY$field)

    # get slash type from slash key
    slash_type <- function (key) {
        types <- IDD_SLASHKEY$type
        chk <- vapply(types, function (type) key %in% type, logical(1L))
        names(types)[chk]
    }

    # get slash type checking function from slash key
    slash_is_type <- function (key) {
        switch(slash_type(key), lgl = is.logical, int = is.integer, dbl = is.double,
            chr = is.character, lst = is.list)
    }

    # get slash type conversion function from slash key
    slash_as_type <- function (key) {
        switch(slash_type(key), lgl = as.logical, int = as.integer, dbl = as.double,
            chr = as.character, lst = c)
    }

    # get slash initial value from slash key
    slash_init_value <- function (key) {
        res <- switch(slash_type(key), lgl = "FALSE", lst = list(), NA_character_)
        slash_as_type(key)(res)
    }

    # convert proerty column types
    types <- unlist(IDD_SLASHKEY$type, use.names = FALSE)
    for (key in intersect(names(dt), types)) {
        if (!slash_is_type(key)(dt[[key]])) {
            set(dt, NULL, key, slash_as_type(key)(dt[[key]]))
            set(dt, seq_len(nrow(dt))[is.na(dt[[key]])], key, slash_init_value(key))
        }
    }

    # add missing property columns if necessary
    for (key in unlist(keys, use.names = FALSE)) {
        if (!has_name(dt, key)) set(dt, NULL, key, slash_init_value(key))
    }

    dt <- switch(type,
        class = parse_class_property(dt, ref),
        field = parse_field_property(dt, ref)
    )

    dt
}
# }}}

# parse_class_property {{{
parse_class_property <- function (dt, ref) {
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
parse_field_property <- function (dt, ref) {
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
parse_field_property_extensible_group <- function (dt, ref) {
    ext <- dt[begin_extensible == TRUE, list(class_id, field_index)]
    # only count once
    ext <- ext[, list(first_extensible = field_index[1L]), by = class_id]

    # handle the case when there is no extensible fields
    if (!has_name(ref, "extensible")) {
        set(dt, NULL, "extensible_group", 0L)
        return(dt)
    }

    # add extensible field number
    ext <- ref[, list(class_id, num_extensible = as.integer(extensible))][ext, on = "class_id"]
    # NOTE: few chances are classes not marked as extensible but with extensible fields
    ext <- ext[!is.na(num_extensible)]

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
parse_field_property_name <- function (dt) {
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
    unit_dt <- UNIT_CONV_TABLE[UNIT_CONV_TABLE[, .I[1], by = si_name]$V1,
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
parse_field_property_default <- function (dt) {
    set(dt, NULL, "default_num", NA_real_)
    set(dt, NULL, "default_chr", dt$default)
    dt[type_enum <= IDDFIELD_TYPE$real, `:=`(default_num = suppressWarnings(as.double(default)))]
    dt
}
# }}}

# parse_field_property_range {{{
parse_field_property_range <- function (dt) {
    set(dt, NULL, c("has_range", "lower_incbounds", "upper_incbounds"), FALSE)

    dt[!is.na(minimum), `:=`(has_range = TRUE, lower_incbounds = TRUE)]
    dt[!is.na(maximum), `:=`(has_range = TRUE, upper_incbounds = TRUE)]
    dt[!is.na(`minimum>`), `:=`(has_range = TRUE, minimum = `minimum>`)]
    dt[!is.na(`maximum<`), `:=`(has_range = TRUE, maximum = `maximum<`)]

    set(dt, NULL, c("minimum>", "maximum<"), NULL)
    dt
}
# }}}

# parse_field_reference_table {{{
parse_field_reference_table <- function (dt) {
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

    # check if \object-list does not have a corresponding \reference
    if (any(is.na(obj_ref$src_field_id))) {
        parse_issue("error_object_list_missing_reference", "idd",
            "\\object-list missing corresponding \\reference or \\reference-class-name",
            post = paste0(
                "Paired \\reference nor \\reference-class-name exist for \\object-list below:\n",
                collapse(obj_ref[is.na(src_field_id), reference])
            )
        )
    }

    set(obj_ref, NULL, "reference", NULL)
    setcolorder(obj_ref, c("class_id", "field_id", "src_class_id", "src_field_id", "src_enum"))

    # remove unuseful columns
    set(dt, NULL, c("object_list", "reference", "reference_class_name"), NULL)

    list(left = dt, reference = obj_ref)
}
# }}}

# sep_idf_lines {{{
sep_idf_lines <- function (dt, type_enum) {
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
    dt[is.na(excl_loc) & !is.na(spcl_loc), `:=`(comment = stri_trim_left(stri_sub(comment, 2L)))]

    set(dt, NULL, c("excl_loc", "spcl_loc"), NULL)

    dt
}
# }}}

# mark_idf_lines {{{
mark_idf_lines <- function (dt, type_enum) {
    # add type indicator
    set(dt, NULL, "type", type_enum$unknown)

    setindexv(dt, "type")

    # macro line
    l_m <- dt[stringi::stri_startswith_fixed(string, "#"), which = TRUE]
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
            parse_issue("warning_macro_line", "idf", "Marco lines found",
                dt[type == type_enum$macro], stop = FALSE,
                post = paste0(
                    "Currently, IMF is not fully supported. All ",
                    "EpMacro lines will be treated as normal comments of ",
                    "the nearest downwards object."
                )
            )
        }
    }

    # normal comments
    dt[stringi::stri_startswith_fixed(string, "!"), `:=`(type = type_enum$comment)]

    # special comments
    dt[stringi::stri_startswith_fixed(string, "!-"), `:=`(type = type_enum$special)]

    # mark values in object
    dt[stringi::stri_endswith_fixed(body, ","), `:=`(type = type_enum$value)]

    # mark last value in object
    dt[stringi::stri_endswith_fixed(body, ";"), `:=`(type = type_enum$value_last)]

    # if there are still known lines, throw an error
    if (nrow(dt[type == type_enum$unknown]) > 0L) {
        parse_issue("error_unknown_line", "idf", "Invalid line found", dt[type == type_enum$unknown])
    }

    dt
}
# }}}

# sep_header_options {{{
sep_header_options <- function (dt, type_enum) {
    dt_opt <- dt[type == type_enum$special]
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
    get_option <- function (header, value) {
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

    save_format <- c("sorted", "ori_top", "ori_bot")
    for (lgl in setdiff(out, save_format)) res$options[[lgl]] <- TRUE
    sf <- save_format[save_format %in% out][1L]
    if (!is.na(sf)) res$options$save_format <- sf

    res
}
# }}}

# sep_object_table {{{
sep_object_table <- function (dt, type_enum, version, idd) {
    # object id
    left <- dt[type == type_enum$value_last, list(line, object_id = seq_along(line))]
    dt <- left[dt, on = "line", roll = -Inf]

    # check incomplete object
    incomp_obj <- dt[is.na(object_id) & type >= type_enum$value]
    if (nrow(incomp_obj)) {
        parse_issue("error_incomplete_object", "idf", "Incomplete object", dt[is.na(object_id)], 1L)
    }

    # extract class names
    dt[dt[type >= type_enum$value, .I[1L], by = list(object_id)]$V1,
        c("type", "class_name_lower", "body") :=({
            n <- stri_locate_first_fixed(body, ",")[, 1L]
            l <- stri_length(body)
            class_name_lower <- stri_trans_tolower(stri_sub(body, to = n - 1L))
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
        parse_issue("error_multiple_version", "idf", "Multiple IDF Version found",
            dt[object_id %in% id_ver], length(id_ver)
        )
    }
    # }}}

    # check invalid class name
    invld_obj <- dt[is.na(class_id) & !is.na(class_name_lower)]
    if (nrow(invld_obj)) {
        parse_issue("error_invalid_class", "idf", "Invalid class name", invld_obj)
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
    clean_comment <- function (x, type) {
        x <- x[type == type_enum$comment]
        if (!length(x)) NULL else x
    }

    dt_object <- dt_object[,
        list(class_id = class_id[1L], comment = list(clean_comment(comment, type))),
        by = object_id
    ]

    dt <- dt[type > type_enum$object]
    # remove unuseful columns
    set(dt, NULL, c("class_name", "group_id", "type", "comment"), NULL)

    list(left = dt, object = dt_object)
}
# }}}

# get_value_table {{{
get_value_table <- function (dt, idd) {
    # count value number per line
    set(dt, NULL, "value_count", stri_count_charclass(dt$body, "[,;]"))

    setindexv(dt, "value_count")

    # get all comments and single value lines
    sgl <- dt[value_count < 2L]
    set(sgl, NULL, "value_count", NULL)
    set(sgl, NULL, "value_chr", stri_trim_right(stri_sub(sgl$body, to = -2L)))

    # get all condensed value lines
    con <- dt[value_count > 1L]
    set(con, NULL, "value_count", NULL)

    # split values
    con <- con[, {
        s <- stri_split_charclass(body, "[,;]", omit_empty = TRUE)
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
    dt_max <- dt[, list(field_index = max(field_index)), by = c("class_id", "object_id")]
    dt_uni <- dt_max[, list(field_index = unique(field_index)), by = "class_id"]

    # only use the max field index to speed up
    fld <- tryCatch(
        get_idd_field(idd, dt_uni$class_id, dt_uni$field_index,
            c("type_enum", "src_enum", "is_name", "units", "ip_units"),
            complete = TRUE
        ),
        error_bad_field_index = function (e) e
    )

    # issue parse error if invalid field number found
    if (inherits(fld, "error_bad_field_index")) {
        # get invalid class id and field number
        invld <- set(fld$data, NULL, "field_index", NULL)
        # find which object has invalid field number
        obj <- dt_max[invld, on = c("class_id", field_index = "field_in")]$object_id

        # modify message
        msg <- gsub(" *#\\d+\\|", "-->", gsub("index", "number", fld$message))
        parse_issue("error_invalid_field_number", "idf", "Invalid field number",
            dt[J(obj), on = "object_id"], post = msg)
    }

    # bind columns
    set(fld, NULL, c("rleid", "field_in"), NULL)
    dt <- unique(fld, by = "field_id")[dt, on = c("class_id", "field_index")]

    # fill data for missing fields
    dt[is.na(line), `:=`(value_id = new_id(dt, "value_id", length(value_id)))]

    # add numeric type values
    dt[type_enum <= IDDFIELD_TYPE$real, `:=`(value_num = suppressWarnings(as.numeric(value_chr)))]
    # update value_chr upon the numeric value
    dt[!is.na(value_num), `:=`(value_chr = as.character(value_num))]

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
update_object_name <- function (dt_object, dt_value) {
    if (!nrow(dt_value)) return(dt_object)
    dt_nm <- dt_value[is_name == TRUE,
        list(object_name = value_chr, object_name_lower = stri_trans_tolower(value_chr)),
        by = "object_id"]
    if (!nrow(dt_nm)) {
        if (!has_name(dt_object, "object_name")) {
            return(set(dt_object, NULL, c("object_name", "object_name_lower"), NA_character_))
        } else {
            return(dt_object)
        }
    }
    dt_object[dt_nm, on = "object_id", `:=`(object_name = dt_nm$object_name, object_name_lower = dt_nm$object_name_lower)]
    dt_object
}
# }}}

# convert_value_unit {{{
convert_value_unit <- function (dt_value, from, to, type = "value") {
    from <- match.arg(from, c("si", "ip"))
    to <- match.arg(to, c("si", "ip"))

    if (identical(from, to)) return(dt_value)

    val <- dt_value[!is.na(value_num) & !is.na(units), list(value_id, value_num, units, ip_units)]

    if (!nrow(val)) return(dt_value)

    val <- UNIT_CONV_TABLE[val, on = c(si_name = "units", ip_name = "ip_units")]
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

    dt_value[val, on = "value_id", `:=`(value_num = val$value_num)]

    dt_value
}
# }}}

# get_value_sources {{{
get_value_sources <- function (dt_value, lower = FALSE) {
    dt_val <- dt_value[!is.na(value_chr), list(object_id, field_id, value_id, value_chr, src_enum, class_name)]

    setindexv(dt_val, "src_enum")

    # a) reference class names
    cls_src <- dt_val[J(IDDFIELD_SOURCE$class), on = "src_enum", nomatch = 0L,
        list(
            src_object_id = object_id,
            src_field_id = field_id,
            src_value_id = value_id,
            src_value_chr = class_name,
            src_enum
        )
    ]

    # b) reference field values
    fld_src <- dt_val[J(IDDFIELD_SOURCE$field), on = "src_enum", nomatch = 0L,
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
    mix_src <- dt_val[J(IDDFIELD_SOURCE$mixed), on = "src_enum", nomatch = 0L,
        {
            list(
                src_object_id = c(object_id, object_id),
                src_field_id = c(field_id, field_id),
                src_value_id = c(value_id, value_id),
                src_value_chr = c(value_chr, class_name),
                src_enum = c(IDDFIELD_SOURCE$field, IDDFIELD_SOURCE$class)
            )
        }
    ]

    # combine
    val_src <- rbindlist(list(cls_src, fld_src, mix_src))

    if (lower) set(val_src, NULL, "src_value_chr", stri_trans_tolower(val_src$src_value_chr))

    val_src
}
# }}}

# get_value_references {{{
get_value_references <- function (dt_value, lower = FALSE) {
    val_ref <- dt_value[
        !is.na(value_chr) & type_enum == IDDFIELD_TYPE$object_list,
        list(object_id, value_id, value_chr, field_id)]

    if (lower) set(val_ref, NULL, "value_chr", stri_trans_tolower(val_ref$value_chr))

    val_ref
}
# }}}

# get_value_reference_map {{{
get_value_reference_map <- function (map, src, value, all = TRUE) {
    empty <- data.table(
            object_id = integer(0L),     value_id = integer(0L),
        src_object_id = integer(0L), src_value_id = integer(0L),
        src_enum = integer(0L)
    )

    # get all values in lower case that are references
    val_ref <- get_value_references(value, lower = TRUE)
    if (!nrow(val_ref)) return(empty)

    # get field reference map in current IDF
    val_ref_map <- val_ref[map, on = "field_id", nomatch = 0L]
    set(val_ref_map, NULL, "src_enum", NULL)

    # get all values in lower case that are sources
    val_src <- get_value_sources(src[J(unique(val_ref_map$src_field_id)), on = "field_id", nomatch = 0L], lower = TRUE)

    # match
    ref <- val_ref_map[val_src, on = "src_field_id", allow.cartesian = TRUE][
        value_chr == src_value_chr, .SD, .SDcols = names(empty)]

    # make sure every reference value has a corresponding source even NA
    if (!all || nrow(ref) == nrow(val_ref)) return(ref)
    ref[J(val_ref$value_id), on = "value_id"]
}
# }}}

# parse_issue {{{
parse_issue <- function (error_type, type = c("idf", "idd", "err", "epw"),
                         title, data = NULL, num = NULL, prefix = NULL, post = NULL,
                         stop = TRUE) {

    start_rule <- cli::rule(line = 2L)

    mes <- NULL
    if (is.data.frame(data)) {
        if (is.null(num)) {
            num <- nrow(data)
        }
        assert(has_name(data, c("line", "string")))
        mes <- paste0(data$msg_each, "Line ", data$line, ": ", data$string)
        if (!is.null(prefix)) {
            mes <- paste0(prefix, mes)
        }

        # only show the first 15 message
        if (length(mes) > 10L) {
            mes <- c(mes[1L:10L], "...[truncated. First 10 are shown.]")
        }

        mes <- str_trunc(mes)
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
    if (stop) {
        abort(c(error_type, paste0("error_parse_", type)), all_mes, NULL, data = data)
    } else {
        warn(c(error_type, paste0("warning_parse_", type)), all_mes, NULL, data = data)
    }
}
# }}}

# insert_version {{{
insert_version <- function (x, ver) {
    if (is.character(x)) {
        paste0(x, "Version, ", standardize_ver(ver)[, 1L:2L], ";")
    } else if (inherits(x, "data.table") && has_name(x, c("line", "string"))) {
        append_dt(x,
            data.table(
                line = max(x$line) + 1L,
                string = paste0("Version, ", standardize_ver(ver)[, 1L:2L], ";")
            )
        )
    } else {
        x
    }
}
# }}}
