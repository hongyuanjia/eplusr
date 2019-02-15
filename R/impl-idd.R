#' @importFrom cli cat_bullet cat_line cat_rule rule symbol
#' @importFrom crayon bold cyan red strip_style underline
#' @importFrom data.table copy data.table dcast rbindlist
#' @importFrom data.table setattr setcolorder setnames setorder setorderv
#' @importFrom stringi stri_locate_first_regex stri_replace_first_regex "stri_sub<-"
#' @importFrom stringi stri_subset_regex stri_match_first_regex stri_rand_strings
#' @include impl.R
NULL

# GROUP
# get_idd_group_index {{{
get_idd_group_index <- function (idd_env, group = NULL) {
    if (is.null(group)) return(idd_env$group$group_id)

    assert(is.character(group))

    res <- idd_env$group[J(group), on = "group_name", group_id]
    if (anyNA(res)) abort_bad_key("error_group_name", "group name", group)
    res
}
# }}}
# get_idd_group_name {{{
get_idd_group_name <- function (idd_env, group = NULL) {
    if (is.null(group)) return(idd_env$group$group_name)

    assert(are_count(group))

    res <- idd_env$group[J(group), on = "group_id", group_name]
    if (anyNA(res)) abort_bad_key("error_group_id", "group index", group)
    res
}
# }}}

# CLASS
# get_idd_class {{{
# Get class data
# @param idd_env An environment or list contains IDD tables including class,
#        field, and reference.
# @param class An integer vector of valid class indexes or a character vector
#        of valid class names or a data.table that contains column `class_id`
#        or `class_name`. If `NULL`, `dt_class` will be returned directly.
# @param cols A character vector of column names in class table to return. If
#        `NULL`, all columns from `dt_class` will be returned, plus column
#        `rleid`.
# @param underscore If `TRUE`, input class name will be converted into
#        underscore style name first and column `class_name_us` will be used
#        for matching.
# @return A data.table containing specified columns.
get_idd_class <- function (idd_env, class = NULL, cols = NULL, underscore = FALSE, group_name = FALSE) {
    if (is.null(class)) return(idd_env$class)
    cls_in <- recognize_input(class, "class", underscore)
    res <- join_from_input(idd_env$class, cls_in, "group_id")
    if (group_name) res <- idd_env$group[res, on = "group_id"]
    if (is.null(cols)) return(res)
    res[, .SD, .SDcols = cols]
}
# }}}

# FIELD
# get_idd_field {{{
get_idd_field <- function (idd_env, class, field = NULL, cols = NULL, all = FALSE,
                           underscore = TRUE, no_ext = FALSE, complete = FALSE) {
    if (is.null(field)) {
        get_idd_field_in_class(idd_env, class, cols, all, underscore)
    } else {
        get_idd_field_from_which(idd_env, class, field, cols, underscore, no_ext, complete)
    }
}
# }}}
# get_idd_field_in_class {{{
# Get all field data in a class
# @param idd_env An environment or list contains IDD tables including class,
#        field, and reference.
# @param class An integer vector of valid class indexes or a character vector
#        of valid class names or a data.table that contains column `class_id`
#        and `rleid`. If a data.table that contains a column `object_id`, that
#        column will be preserved.
# @param cols A character vector of column names in field table to return. If
#        `NULL`, all columns from IDD field table will be returned, plus column
#        `rleid`, and `object_id` (if applicable).
# @param underscore If `TRUE`, input class name will be converted into
#        underscore style name first and column `class_name_us` will be used for
#        matching.
# @param all If `TRUE` data of all fields will be returned. Default is `FALSE`,
#        which means that only minimum fields will be returned.
get_idd_field_in_class <- function (idd_env, class, cols = NULL, all = FALSE, underscore = TRUE) {
    if (is.null(cols)) cols <- c("rleid", names(idd_env$field))
    dt_in <- recognize_input(class, "class", underscore = underscore)
    dt_in[, min_required := pmax(min_fields, last_required)]
    dt_in[min_required == 0L, min_required := num_fields]
    col_on <- if (all) "num_fields" else "min_required"
    get_idd_field_from_idx(idd_env, dt_in, col_on)[, .SD, .SDcols = cols]
}
# }}}
# get_idd_field_from_which {{{
# Get specified field data
# @param idd_env An environment or list contains IDD tables including class,
#        field, and reference.
# @param class An integer vector of valid class indexes or a character vector
#        of valid class names or a data.table that contains column `class_id`
#        and `rleid`. If a data.table that contains a column `object_id`, that
#        column will be preserved.
# @param field An integer vector of valid field indexes or a character
#        vector of valid field names (can be in in underscore style).  `class`
#        and `field` should have the same length.
# @param cols A character vector of column names in field table to return. If
#        `NULL`, all columns from IDD field table will be returned, plus column
#        `rleid`, `object_id` (if applicable) and `matched_rleid` (if
#        `complete` is `TRUE`).
# @param underscore If `TRUE`, input class name and field names will be
#        converted into underscore style name first and column `class_name_us`
#        and `field_name_us` will be used for matching.
# @param no_ext If `TRUE`, no new extensible groups will be added even if there
#        are no matched input found and an error will be issued right away.
# @param complete If `TRUE`, at least fields till the current whole extensible
#        group will be returned. A new column named "matched_rleid" will be
#        created (when `cols` is NULL) indicating if given field has been
#        matched or not.
get_idd_field_from_which <- function (idd_env, class, field, cols = NULL,
                                      underscore = TRUE, no_ext = FALSE, complete = FALSE) {
    if (!is.character(field) && !all(are_count(field))) {
        abort_bad_which_type("error_field_which_type", "field")
    }

    dt_in <- recognize_input(class, "class", underscore = underscore)

    # make sure number of rows of dt_in and the length of input field is the same
    if (!inherits(class, "data.frame") && is_scalar(class) && !is.null(field)) dt_in <- dt_in[rep(1L, length(field))]

    if (is.null(cols)) {
        cols <- c(setdiff(names(dt_in), names(idd_env$field)), names(idd_env$field))
        if (complete) cols <- c(cols, "matched_rleid")
    }

    # get_index_dt: return an index dt for non-equi join {{{
    get_index_dt <- function (dt_in, cols = NULL) {
        # for using field names
        if (!nrow(dt_in) && !has_name(dt_in, "field_index")) {
            set(dt_in, NULL, "field_index", integer(0))
        }

        # select last matched field per input
        if (has_name(dt_in, "field_index")) {
            dt_last <- dt_in[dt_in[order(rleid, -field_index), .I[1L], by = list(rleid)]$V1]
        } else {
            dt_last <- dt_in[dt_in[order(rleid), .I[1L], by = list(rleid)]$V1]
        }

        # get acceptable field number
        get_idd_class_field_num(dt_last, dt_last$field_index)[
            , field_index := acceptable_num]
    }
    # }}}

    assert(have_same_len(dt_in, field))

    if (all(are_count(field))) {
        # from field index {{{
        col_on <- "field_index"
        set(dt_in, NULL, "field_index", field)

        dt_idx <- get_index_dt(dt_in, cols = c("input_num", "num_fields",
                "num_extensible", "num_extensible_group", "last_required")
        )

        # check invalid field index
        if (nrow(dt_idx[field_index < input_num])) {
            invld_idx <- dt_in[dt_idx[field_index < input_num, list(rleid)], on = "rleid"]
            abort_bad_field("error_bad_field_index", "index", invld_idx)
        }

        # handle extensible fields
        set(dt_idx, NULL, "num", 0L)
        dt_idx[field_index > num_fields, `:=`(num = as.integer((field_index - num_fields) / num_extensible))]

        if (no_ext && nrow(dt_idx[num > 0L])) {
            abort_bad_field("error_bad_field_index", "index", dt_in[dt_idx[num > 0L], on = "rleid"])
        }

        idd_env <- add_idd_extensible_group(idd_env, dt_idx)
        if (!complete) {
            fld <- idd_env$field[dt_in, on = c("class_id", col_on)]
        } else {
            fld <- get_idd_field_from_idx(idd_env, dt_idx, "field_index")[
                dt_in, on = c("rleid", "field_index"), matched_rleid := .I]
            fld[is.na(matched_rleid), matched_rleid := 0L]
        }
        # }}}
    } else {
        # from field name {{{
        # clean up dt for error message printing
        clean_errnm_dt <- function (dt, extensible = FALSE) {
            errnm <- dt[, list(rleid, field_rleid, class_id, class_name = i.class_name,
                field_name = field_name_in)]
            if (nrow(errnm)) {
                set(errnm, NULL, "extensible_field", extensible)
            } else {
                set(errnm, NULL, "extensible_field", logical(0L))
            }
            errnm
        }

        # add helper columns
        add_rleid(dt_in, "field")
        set(dt_in, NULL, "field_name_in", field)
        if (underscore) {
            col_on <- "field_name_us"
            set(dt_in, NULL, "field_name_us", underscore_name(field))
        } else {
            col_on <- "field_name"
            set(dt_in, NULL, "field_name", field)
        }

        # join
        dt_join <- idd_env$field[dt_in, on = c("class_id", col_on)]

        # get no matched
        dt_nom <- dt_join[is.na(field_id)]

        # invalid field names for non-extensible classes
        invld_nm_non_ext <- dt_nom[class_id %chin% get_idd_class_id_nonextensible(idd_env)]
        invld_nm_non_ext <- clean_errnm_dt(invld_nm_non_ext, FALSE)

        # fields for extensible classes
        dt_nom_ext <- dt_nom[!invld_nm_non_ext, on = "field_rleid"]

        # get matched
        dt_m <- dt_join[!dt_nom, on = "field_rleid"]

        if (complete) {
            dt_idx <- get_index_dt(dt_m)
            dt_m <- get_idd_field_from_idx(idd_env, dt_idx)[
                dt_m, on = list(rleid, field_index), matched_rleid := .I]
            dt_m[is.na(matched_rleid), matched_rleid := 0L]
        }

        # if checking completes and errors are found
        if (no_ext) {
            if (nrow(dt_nom_ext)) {
                abort_bad_field("error_bad_field_name", "name", clean_errnm_dt(dt_nom_ext))
            }

            return(dt_m[, .SD, .SDcols = cols])
        }

        if (!nrow(dt_nom_ext)) {
            if (nrow(invld_nm_non_ext)) {
                abort_bad_field("error_bad_field_name", "name", invld_nm_non_ext)
            }

            return(dt_m[, .SD, .SDcols = cols])
        }

        # if there is a need to extend classes or there is no error found
        # get number of field names to check per class
        dt_ext <- dt_nom_ext[, list(num = .N), by = class_id]
        # get extensible info per class
        dt_ext <- dt_nom_ext[dt_ext, on = "class_id", mult = "first"]
        # get extensible number to add per class
        dt_ext[, `:=`(num = ceiling(num / num_extensible))]

        idd_env <- add_idd_extensible_group(idd_env, dt_ext)
        dt_ext_join <- idd_env$field[
            dt_nom_ext[, .SD, .SDcols = c("class_id", col_on, setdiff(names(dt_nom_ext), names(idd_env$field)))],
            on = c("class_id", col_on)]

        # check invalid extensible field names
        invld_nm_ext <- clean_errnm_dt(dt_ext_join[is.na(field_id)], TRUE)
        invld_nm <- rbindlist(list(invld_nm_non_ext, invld_nm_ext))
        if (nrow(invld_nm)) {
            dt_ext[, num_fields := num_fields + num * num_extensible]
            idd_env <- del_idd_extensible_group(idd_env, dt_ext)
            abort_bad_field("error_bad_field_name", "name", invld_nm,
                "\n\nNOTE: For extensible fields, new one will be added only ",
                "when all previous extensible groups exist."
            )
        }

        fld <- rbindlist(list(dt_m, dt_ext_join), fill = TRUE)
        if (complete) {
            dt_ext_idx <- get_index_dt(dt_ext_join)

            # combine index data of non-extensible and extensible groups
            dt_idx <- rbindlist(list(dt_ext, dt_ext_idx))[
                , list(field_index = max(field_index)), by = c("rleid", col_obj_id, "class_id")]

            # mark those matched field
            fld <- get_idd_field_from_idx(idd_env, dt_idx)[fld, on = c("rleid", "field_index"),
                matched_rleid := .I]
            fld[is.na(matched_rleid), matched_rleid := 0L]
        }
        # }}}
    }

    fld[, .SD, .SDcols = cols]
}
# }}}
# get_idd_field_from_idx: return field data by using an index dt {{{
get_idd_field_from_idx <- function (idd_env, dt_idx, col = "field_index") {
    fld <- idd_env$field[dt_idx, on = c("class_id", paste0("field_index<=", col)), allow.cartesian = TRUE]
    set(fld, NULL, "field_index", rowidv(fld, c("rleid", "class_id")))

    fld
}
# }}}

# change_field_default_value_unit {{{
change_field_default_value_unit <- function (dt_field, from, to) {
    set(dt_field, NULL, "value_id", seq_along(dt_field$field_id))
    set(dt_field, NULL, "value_num", NA_real_)

    dt_field[vapply(default, is.numeric, logical(1L)), `:=`(value_num = unlist(default))]

    dt_field <- convert_value_unit(dt_field, from, to)

    dt_field[!is.na(value_num), `:=`(default = as.list(value_num))]

    int_trunc <- dt_field[type_enum == IDDFIELD_TYPE$integer & !are_integer(value_num)]
    if (nrow(int_trunc)) {
        mes <- paste0("Truncation errors introduced when converting integer ",
            "default values from ", toupper(from), " unit to ", toupper(to), " unit.")
        warn("warning_default_int_trunc", mes, int_trunc)
    }

    dt_field[type_enum == IDDFIELD_TYPE$integer, `:=`(default = as.list(as.integer(value_num)))]
    set(dt_field, NULL, c("value_id", "value_num"), NULL)

    dt_field
}
# }}}
# get_field_reference {{{
get_field_reference <- function (fld, dt_reference, dt_value) {
    # get all nodes
    fld[type_enum == IDDFIELD_TYPE$node, `:=`(node_value = list(t_value_all_node(dt_value)))]

    # get all ids of unique field references
    fld_id_ref <- fld[type_enum == IDDFIELD_TYPE$object_list, list(field_id = unique(field_id))]

    # get all ids of field sources
    fld_id_ref_src <- dt_reference[fld_id_ref, on = "field_id", nomatch = 0L, list(field_id, src_field_id)]

    # remove duplicates in source field ids and unuseful value
    dt_val <- dt_value[J(unique(fld_id_ref_src$src_field_id)), on = "field_id", nomatch = 0L]

    # get source values
    val_src <- get_value_sources(dt_val)[, list(src_value = list(src_value)), by = list(src_field_id)]

    # nest by field list
    fld_src <- fld_id_ref_src[val_src, on = "src_field_id", list(field_id, src_value)][,
        list(src_value = list(unlist(src_value))), by = field_id]

    # merge into orignial field table
    fld <- fld_src[fld, on = "field_id"][type_enum == IDDFIELD_TYPE$node, `:=`(src_value = node_value)]

    set(fld, NULL, "node_value", NULL)

    fld
}
# }}}

# EXTENSIBLE GROUP
# add_idd_extensible_group {{{
add_idd_extensible_group <- function (idd_env, class, num = NULL, strict = FALSE) {
    dt_cls <- get_input_class_data(idd_env$class, class, num)

    # stop if non-extensible class found
    if (strict && nrow(dt_cls[num_extensible == 0L])) {
        stop("Non-extensible class found: ",
            collapse(dt_cls[num_extensible == 0L, unique(class_name)]),
            call. = FALSE
        )
    }

    ext <- dt_cls[num_extensible > 0L & num > 0L]

    if (!nrow(ext)) return(idd_env)

    # get unique class data
    ext <- ext[,
        list(
            num = max(num),
            extensible_group = num_extensible_group[1L],
            num_extensible = num_extensible[1L],
            last_required = last_required[1L]
        ),
        by = class_id
    ]

    # get the last extensible group
    ext_fld <- idd_env$field[ext, on = c("class_id", "extensible_group")]
    set(ext_fld, NULL, "num", NULL)

    ext_fld[, `:=`(
        field_anid_an = stri_sub(field_anid, to = 1L),
        field_anid_id = as.integer(stri_sub(field_anid, from = 2L))
    )]

    # count field A and N per extensible group per class
    fld_an <- ext_fld[, list(field_anid_num = length(field_anid)),
        by = list(class_id, field_anid_an)]
    ext_fld <- fld_an[ext_fld, on = list(class_id, field_anid_an)]

    # extract field name
    ext_fld[, `:=`(field_name_id = as.integer(stri_extract_first_regex(field_name, "(?<=(#|A|N| ))\\d+")))]

    # count distinct field name patterns per extensible group per class
    ext_fld[, c("field_name_noid", "field_name_id") := (
        {
            nm <- field_name
            l <- stri_locate_first_regex(field_name,  "(?<=(#|A|N| ))\\d+")
            stri_sub(nm, l[,1L], l[,2L]) <- ""
            nm_id <- as.integer(stri_sub(field_name, l[,1L], l[,2L]))
            list(nm, nm_id)
        }
    )]
    fld_nm <- ext_fld[, list(field_name_num = length(field_name)),
        by = list(class_id, field_name_noid)]
    ext_fld <- fld_nm[ext_fld, on = list(class_id, field_name_noid)]

    setorderv(ext_fld, "field_id")

    # split by class
    lst_ext_fld <- split(ext_fld, by = "class_id")

    # combine
    rep_dt <- function(dt, time) rbindlist(replicate(dt, time, simplify = FALSE))
    new_ext <- rbindlist(apply2(ext$num, lst_ext_fld, rep_dt))

    # get extensible group index
    idx <- ext[, list(group = rep(seq_len(num), each = num_extensible)),
        by = list(class_id, extensible_group, num_extensible)
    ]
    idx[, `:=`(index = group * num_extensible, extensible_group = group + extensible_group), by = class_id]

    # fix properties
    new_ext[, `:=`(
        field_index = field_index + idx$index,
        field_anid = paste0(field_anid_an, field_anid_num * idx$group + field_anid_id),
        field_name = stri_replace_first_regex(field_name, "(?<=(#|A|N| ))\\d+", field_name_num * idx$group + field_name_id),
        extensible_group = idx$extensible_group
    )]
    set(new_ext, NULL, "field_name_us", underscore_name(new_ext$field_name))
    new_ext[is.na(units), `:=`(full_name = field_name)]
    new_ext[is.na(ip_units), `:=`(full_ipname = field_name)]
    new_ext[!is.na(units), `:=`(full_name = paste0(field_name, " {", units, "}"))]
    new_ext[!is.na(ip_units), `:=`(full_ipname = paste0(field_name, " {", ip_units, "}"))]
    new_ext[field_index > last_required, `:=`(required_field = FALSE)]

    # get new field id
    new_fld_id <- new_id(idd_env$field, "field_id", nrow(new_ext))

    # assign new field id
    set(new_ext, NULL, "field_id", new_fld_id)

    # get field reference data
    ref <- idd_env$reference[
        J(new_ext$field_id), on = "field_id"][
        , `:=`(field_id = new_fld_id)][
        !is.na(src_field_id)]
    src <- idd_env$reference[J(new_ext$field_id), on = "src_field_id"][
        , `:=`(src_field_id = new_fld_id)][
        !is.na(field_id)]
    new_ref <- rbindlist(list(ref, src))

    # combine into the main field table and name table
    idd_env$field <- append_dt(idd_env$field, new_ext)
    idd_env$reference <- append_dt(idd_env$reference, new_ref)

    # update class data
    idd_env$class <- copy(idd_env$class)[ext, on = "class_id",
        c("num_fields", "num_extensible_group") := {
            num_fields = num_fields + ext$num * ext$num_extensible
            num_extensible_group = num_extensible_group + ext$num
            list(num_fields, num_extensible_group)
        }
    ]

    idd_env
}
# }}}
# del_idd_extensible_group {{{
del_idd_extensible_group <- function (idd_env, class, num = NULL, strict = FALSE) {
    dt_cls <- get_input_class_data(idd_env$class, class, num)

    # stop if non-extensible class found
    if (strict && nrow(dt_cls[num_extensible == 0L])) {
        stop("Non-extensible class found: ",
            collapse(dt_cls[num_extensible == 0L, unique(class_name)]),
            call. = FALSE
        )
    }

    ext <- dt_cls[num_extensible > 0L]

    if (!nrow(ext)) return(idd_env)

    # get unique class data
    ext <- ext[,
        list(
            left_group = num_extensible_group[1L] - max(num),
            left_fields = num_fields[1L] - max(num) * num_extensible[1L],
            last_required = last_required[1L]
        ),
        by = list(class_id, class_name)
    ]

    # stop if number of left fields is less that minimum required
    if (nrow(ext[left_fields < last_required])) {
        less <- ext[left_fields < last_required][left_fields < 0L, `:=`(left_fields = 0L)]
        less <- errormsg_info(less)
        mes <- less[, paste0(info, ": ", left_fields, " left with ", last_required, " required.")]
        mes <- paste0("Failed to delete extensible groups. Number of field(s) left less than required:\n", mes)
        abort("error_del_extensible", mes, data = less)
    }

    # get field id to delete
    fld_id_del <- idd_env$field[ext, on = list(class_id, extensible_group > left_group), list(field_id)]

    idd_env$field <- idd_env$field[!fld_id_del, on = "field_id"]

    idd_env$reference <- idd_env$reference[
        !fld_id_del, on = "field_id"][
        !fld_id_del, on = c(src_field_id = "field_id")
    ]

    idd_env$class <- copy(idd_env$class)[ext, on = "class_id", `:=`(
        num_extensible_group = ext$left_group,
        num_fields = ext$left_fields
    )]

    idd_env
}
# }}}
# get_input_class_data {{{
get_input_class_data <- function (dt_class, class, num = NULL) {
    if (is.data.frame(class)) {
        dt_cls <- class

        if (is.null(num)) {
            assert(has_name(dt_cls, "num"))
            set(dt_cls, NULL, "num", as.integer(dt_cls$num))
        } else {
            assert(are_count(num))
            set(dt_cls, NULL, "num", as.integer(num))
        }

    } else {
        # get class data
        dt_cls <- t_class_data(dt_class, class,
            c(
                "class_id", "class_name", "min_fields", "last_required", "num_fields",
                "first_extensible", "num_extensible", "num_extensible_group"
            )
        )

        assert(are_count(num))
        assert(have_same_len(class, num))
        set(dt_cls, NULL, "num", as.integer(num))
    }
}
# }}}
