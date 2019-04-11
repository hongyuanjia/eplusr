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
# @param property A character vector of column names in class table to return. If
#        `NULL`, all columns from `dt_class` will be returned, plus column
#        `rleid`.
# @param underscore If `TRUE`, input class name will be converted into
#        underscore style name first and column `class_name_us` will be used
#        for matching.
# @return A data.table containing specified columns.
get_idd_class <- function (idd_env, class = NULL, property = NULL, underscore = FALSE) {
    if (is.null(class)) return(copy(idd_env$class))

    cls_in <- recognize_input(class, "class", underscore)
    cls_in <- del_redundant_cols(idd_env$class, cls_in)
    res <- join_from_input(idd_env$class, cls_in, "group_id")
    set(res, NULL, "class_name_us", NULL)

    if (!is.null(property)) {
        if ("group_name" %chin% property) {
            add_joined_cols(idd_env$group, res, "group_id", "group_name")
        }
        clean_class_property(res, property)
    }

    res
}
# }}}
# get_idd_class_field_num {{{
# Get the acceptable field number
# Acceptable number is determined in ways below:
#   * If input number is NULL:
#       - For class with no required fields and no minimum field number
#         requirement, the acceptable field number is the total existing
#         field number in that class.
#       - For class with required fields and minimum field number
#         requirement, the acceptable number is the bigger one between
#         required field number and minimum field number.
#   * If input number is smaller than the total existing field number:
#       - The acceptable field number is the maximum among the field
#         index of the last field in the last extensible group (if
#         applicable), the field index of the last required field and
#         minimum field number required.
#   * If input number is larger than the total existing field number:
#       - The acceptable field number will be the field index of the last
#         field in the last extensible group.
get_idd_class_field_num <- function (dt_class, num = NULL) {
    if (!is.null(num)) assert(are_count(num))

    dt_class <- add_rleid(dt_class, "class")

    # directly return num of fields in class
    assert(has_name(dt_class, c("num_fields", "min_fields", "last_required", "num_extensible", "first_extensible")))

    if (!nrow(dt_class)) {
        set(dt_class, NULL, "input_num", integer(0L))
        set(dt_class, NULL, "acceptable_num", integer(0L))
        return(dt_class)
    }

    if (is.null(num)) {
        set(dt_class, NULL, "input_num", 0L)
    } else {
        assert(have_same_len(dt_class, num))
        set(dt_class, NULL, "input_num", as.integer(num))
    }

    # get index of the last field in the last extensible group
    set(dt_class, NULL, "last_extensible", 0)
    if (nrow(dt_class[num_extensible > 0L])) {
        dt_class[
            num_extensible > 0L & input_num > first_extensible,
            last_extensible :=
                ceiling((input_num - first_extensible + 1L) / num_extensible) * num_extensible + first_extensible - 1L,
            by = "class_rleid"
        ]
    }

    if (nrow(dt_class[input_num > num_fields])) {
        dt_class[input_num > num_fields,
            acceptable_num := pmax(num_fields, min_fields, last_required, last_extensible)]
        dt_class[input_num <= num_fields,
            acceptable_num := pmax(input_num, min_fields, last_required, last_extensible)]
    } else {
        dt_class[,
            acceptable_num := pmax(input_num, min_fields, last_required, last_extensible)]
    }

    set(dt_class, NULL, c("class_rleid", "last_extensible"), NULL)
    set(dt_class, NULL, "acceptable_num", as.integer(dt_class$acceptable_num))
    dt_class
}
# }}}
# clean_class_property {{{
clean_class_property <- function (dt, property) {
    col_del <- setdiff(CLASS_COLS$property, property)
    if (length(col_del)) set(dt, NULL, col_del, NULL)
    dt
}
# }}}

# FIELD
# get_idd_field {{{
get_idd_field <- function (idd_env, class, field = NULL, property = NULL, all = FALSE,
                           underscore = TRUE, no_ext = FALSE, complete = FALSE) {
    if (is.null(field)) {
        res <- get_idd_field_in_class(idd_env, class, all, underscore)
    } else {
        res <- get_idd_field_from_which(idd_env, class, field, underscore, no_ext, complete, all)
    }
    if (!is.null(property)) clean_field_property(res, property)
    res
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
# @param property A character vector of column names in field table to return. If
#        `NULL`, all columns from IDD field table will be returned, plus column
#        `rleid`, and `object_id` (if applicable).
# @param underscore If `TRUE`, input class name will be converted into
#        underscore style name first and column `class_name_us` will be used for
#        matching.
# @param all If `TRUE` data of all fields will be returned. Default is `FALSE`,
#        which means that only minimum fields will be returned.
get_idd_field_in_class <- function (idd_env, class, all = FALSE, underscore = TRUE) {
    prop <- if (!all) c("num_fields", "min_fields", "last_required") else NULL
    cls <- get_idd_class(idd_env, class, prop, underscore)
    set(cls, NULL, "group_id", NULL)

    if (all) {
        cls <- del_redundant_cols(idd_env$field, cls, "class_id")
        idd_env$field[cls, on = "class_id"]
    } else {
        set(cls, NULL, "min_required", pmax(cls$min_fields, cls$last_required))
        cls[min_required == 0L, min_required := num_fields]
        set(get_idd_field_from_idx(idd_env, cls, "min_required"), NULL,
            c("num_fields", "min_fields", "last_required"), NULL
        )
    }
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
# @param property A character vector of column names in field table to return. If
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
#        created (when `property` is NULL) indicating if given field has been
#        matched or not.
get_idd_field_from_which <- function (idd_env, class, field, underscore = TRUE,
                                      no_ext = FALSE, complete = FALSE, all = FALSE) {
    assert_valid_type(field, "field")

    if (inherits(class, "data.frame")) {
        col_keep <- unique(c(names(class), "class_id", "class_name"))
    } else {
        col_keep <- c("rleid", "class_id", "class_name")
    }

    # class properties used for min required field num calculation
    col_prop <- c("num_fields", "min_fields", "last_required", "num_extensible",
        "first_extensible", "num_extensible_group"
    )
    dt_in <- get_idd_class(idd_env, class, col_prop, underscore)

    # make sure number of rows of dt_in and the length of input field is the same
    if (NROW(class) == 1L) dt_in <- dt_in[rep(1L, length(field))]

    # get_index_dt: return an index dt for non-equi join {{{
    get_index_dt <- function (dt_in, all = FALSE) {
        # for using field names
        if (!nrow(dt_in) && !has_name(dt_in, "field_index")) {
            set(dt_in, NULL, "field_index", integer(0))
        }

        # select last matched field per input
        if (has_name(dt_in, "field_index")) {
            dt_last <- dt_in[dt_in[order(rleid, -field_index), .I[1L], by = "rleid"]$V1]
        } else {
            dt_last <- dt_in[dt_in[order(rleid), .I[1L], by = "rleid"]$V1]
        }

        col_del <- setdiff(names(dt_last), c("rleid", "class_id", "class_name", col_prop, "field_index"))
        if (length(col_del)) set(dt_last, NULL, col_del, NULL)

        if (all) dt_last[field_index <= num_fields, field_index := num_fields]

        # get acceptable field number
        get_idd_class_field_num(dt_last, dt_last$field_index)
    }
    # }}}

    # join_field {{{
    join_field <- function (dt, idx) {
        no_roll <- c("field_in", "value_num", "value", "new_value", "new_value_num")
        no_roll <- no_roll[no_roll %chin% names(dt)]
        if (!length(no_roll)) {
            dt[get_idd_field_from_idx(idd_env, idx, "field_index"),
               on = c("rleid", "field_index"),
               roll = TRUE, rollends = c(TRUE, TRUE)
            ]
        } else {
            fld <- dt[, .SD, .SDcols = setdiff(names(dt), no_roll)][
                get_idd_field_from_idx(idd_env, idx, "field_index"),
                on = c("rleid", "field_index"),
                roll = TRUE, rollends = c(TRUE, TRUE)
            ]

            set(fld, NULL, no_roll,
                dt[fld, on = c("rleid", "field_index")][, .SD, .SDcols = no_roll]
            )
        }
    }
    # }}}

    assert(have_same_len(dt_in, field), prefix = "class and field")

    if (all(are_count(field))) {
        # from field index {{{
        set(dt_in, NULL, c("field_index", "field_in"), list(field, field))
        col_on <- "field_index"
        col_keep <- c(col_keep, col_on, "field_in")

        dt_idx <- get_index_dt(dt_in, all)

        # check invalid field index
        if (nrow(dt_idx[field_index > acceptable_num])) {
            invld_idx <- dt_in[dt_idx[field_index > acceptable_num, list(rleid)], on = "rleid"]
            abort_bad_field("error_bad_field_index", "index", invld_idx)
        }

        # handle extensible fields
        set(dt_idx, NULL, "num", 0L)
        dt_idx[field_index > num_fields, `:=`(num = ceiling((field_index - num_fields) / num_extensible))]

        if (no_ext && nrow(dt_idx[num > 0L])) {
            abort_bad_field("error_bad_field_index", "index", dt_in[dt_idx[num > 0L], on = "rleid"])
        }

        # add extensible groups
        idd_env <- add_idd_extensible_group(idd_env, dt_idx)
        set(dt_in, NULL, setdiff(names(dt_in), col_keep), NULL)
        # only return specified fields
        if (!all && !complete) {
            dt_in <- del_redundant_cols(idd_env$field, dt_in, c("class_id", col_on))
            fld <- idd_env$field[dt_in, on = c("class_id", col_on), nomatch = 0L]
        } else {
            dt_in <- del_redundant_cols(idd_env$field, dt_in, "field_index")
            set(dt_idx, NULL, setdiff(names(dt_idx), c("rleid", "class_id", "field_index")), NULL)
            fld <- join_field(dt_in, dt_idx)
        }
        # }}}
    } else {
        # from field name {{{
        # clean up dt for error message printing
        clean_errnm_dt <- function (dt, extensible = FALSE) {
            errnm <- dt[, list(rleid, field_rleid, class_id, class_name, field_name = field_in)]
            if (nrow(errnm)) {
                set(errnm, NULL, "extensible_field", extensible)
            } else {
                set(errnm, NULL, "extensible_field", logical(0L))
            }
            errnm
        }

        # add helper columns
        add_rleid(dt_in, "field")
        # for error printing
        set(dt_in, NULL, "field_in", field)
        if (underscore) {
            col_on <- "field_name_us"
            set(dt_in, NULL, "field_name_us", stri_trans_tolower(underscore_name(field)))
        } else {
            col_on <- "field_name"
            set(dt_in, NULL, "field_name", field)
        }
        col_keep <- c(col_keep, col_on, "field_in")

        # join
        dt_in <- del_redundant_cols(idd_env$field, dt_in, c("class_id", col_on))
        dt_join <- idd_env$field[dt_in, on = c("class_id", col_on)]

        # if all matched
        if (!anyNA(dt_join$field_id)) {
            if (!all && !complete) {
                fld <- dt_join
            } else {
                dt_idx <- get_index_dt(dt_join, all)
                set(dt_idx, NULL, setdiff(names(dt_idx), c("rleid", "class_id", "field_index")), NULL)
                dt_join <- del_redundant_cols(idd_env$field, dt_join, "field_index")

                fld <- join_field(dt_join, dt_idx)
            }
        } else {
            # get no matched
            dt_nom <- dt_join[is.na(field_id)]

            # invalid field names for non-extensible classes
            if (any(dt_nom$class_id %in% idd_env$class[J(0L), on = "num_extensible", class_id])) {
                invld_non_ext <- dt_nom[class_id %in% idd_env$class[J(0L), on = "num_extensible", class_id]]
                abort_bad_field("error_bad_field_name", "name", clean_errnm_dt(invld_non_ext))
            }

            # if all names not found are in extensible class
            if (no_ext) {
                abort_bad_field("error_bad_field_name", "name", clean_errnm_dt(dt_nom))
            }

            # get number of field names to check per class
            dt_ext <- dt_nom[, list(
                rleid = rleid[[1L]],
                class_name = class_name[[1L]],
                num_fields = num_fields[[1L]],
                min_fields = min_fields[[1L]],
                last_required = last_required[[1L]],
                num_extensible = num_extensible[[1L]],
                first_extensible = first_extensible[[1L]],
                num_extensible_group = num_extensible_group[[1L]],
                num = .N),
                by = class_id
            ]
            # get extensible number to add per class
            dt_ext[, `:=`(num = ceiling(num / num_extensible))]

            # try to match names in newly added extensible groups
            idd_env <- add_idd_extensible_group(idd_env, dt_ext)
            dt_nom <- del_redundant_cols(idd_env$field, dt_nom, c("class_id", col_on))
            dt_ext_join <- idd_env$field[dt_nom, on = c("class_id", col_on)]

            # check invalid extensible field names
            if (anyNA(dt_ext_join$field_id)) {
                invld_nm <- clean_errnm_dt(dt_ext_join[is.na(field_id)], TRUE)
                dt_ext[, `:=`(
                    num_fields = num_fields + num * num_extensible,
                    num_extensible_group = num_extensible_group + num
                    )]
                idd_env <- del_idd_extensible_group(idd_env, dt_ext)
                abort_bad_field("error_bad_field_name", "name", get_idd_class(idd_env, invld_nm),
                    "\n\nNOTE: For extensible fields, new one will be added only ",
                    "when all previous extensible groups exist."
                )
            }

            # if all matched
            if (!all && !complete) {
                # {{{
                col_del <- setdiff(names(dt_join), c("field_rleid", col_keep, names(idd_env$field)))

                fld <- setorderv(
                    rbindlist(
                        list(
                            set(dt_join[!is.na(field_id)], NULL, col_del, NULL),
                            set(dt_ext_join, NULL, col_del, NULL)
                        ),
                         use.names = TRUE
                    ),
                    "field_rleid"
                )
                # }}}
            } else {
                # {{{
                # combine index data of non-extensible and extensible groups
                dt_idx <- get_index_dt(dt_join[!is.na(field_id)], all)
                dt_ext_idx <- get_index_dt(dt_ext_join, all)
                dt_idx <- rbindlist(list(dt_idx, dt_ext_idx), use.names = TRUE)[
                    , list(field_index = max(field_index)), by = c("rleid", "class_id")]
                dt_join <- rbindlist(list(
                    dt_join[!is.na(field_id), .SD, .SDcols = setdiff(c("field_index", col_keep), col_on)],
                    dt_ext_join[, .SD, .SDcols = setdiff(c("field_index", col_keep), col_on)]
                ))
                dt_join <- del_redundant_cols(idd_env$field, dt_join, "field_index")

                fld <- join_field(dt_join, dt_idx)
                # }}}
            }
        }
        # }}}
    }

    set(fld, NULL, "field_name_us", NULL)
    fld
}
# }}}
# get_idd_field_from_idx: return field data by using an index dt {{{
get_idd_field_from_idx <- function (idd_env, dt_idx, on = "field_index") {
    dt_idx <- del_redundant_cols(idd_env$field, dt_idx, c("class_id", on))
    fld <- idd_env$field[dt_idx, on = c("class_id", paste0("field_index<=", on)), allow.cartesian = TRUE]
    if (has_name(fld, "object_id")) {
        set(fld, NULL, "field_index", rowidv(fld, c("rleid", "object_id", "class_id")))
    } else {
        set(fld, NULL, "field_index", rowidv(fld, c("rleid", "class_id")))
    }
}
# }}}
# clean_field_property {{{
clean_field_property <- function (dt, property) {
    col_del <- setdiff(FIELD_COLS$property, property)
    if (length(col_del)) set(dt, NULL, col_del, NULL)
    dt
}
# }}}

# REFERENCES
# get_idd_relation {{{
get_idd_relation <- function (idd_env, class = NULL, field = NULL, max_depth = NULL,
                              name = FALSE, direction = c("ref_to", "ref_by")) {
    assert(is.null(max_depth) || is_count(max_depth, TRUE))
    direction <- match.arg(direction)

    # get class reference
    if (is.null(field)) {
        if (is.null(class)) {
            id <- idd_env$class$class_id
        } else {
            id <- get_idd_class(idd_env, class)$class_id
        }
        col_on <- "class_id"
    } else {
        # if no class is given, assume field are valid field ids
        if (is.null(class)) {
            id <- field
        } else {
            id <- get_idd_field(idd_env, class, field)$field_id
        }
        col_on <- "field_id"
    }

    if (direction == "ref_to") {
        col_rec <- paste0("src_field_id")
    } else if (direction == "ref_by") {
        col_on <- paste0("src_", col_on)
        col_rec <- "field_id"
    }

    # TODO: for extensible groups, only use the first group
    dep <- 0L
    if (is.null(max_depth)) max_depth <- Inf
    ref <- idd_env$reference[0L]
    set(ref, NULL, "dep", integer())

    get_all_ref <- function (idd_env, id, depth = Inf) {
        cur_ref <- idd_env$reference[J(id), on = col_on, nomatch = 0L]
        set(cur_ref, NULL, "dep", dep)
        if (!nrow(cur_ref)) return(ref)
        ref <<- rbindlist(list(ref, cur_ref))
        if (depth == dep) return(ref)
        dep <<- dep + 1L
        if (is.null(field)) col_on <<- sub("class", "field", col_on)
        get_all_ref(idd_env, cur_ref[[col_rec]])
    }

    ref <- get_all_ref(idd_env, id, max_depth)

    if (!name) return(ref)

    # add all necessary columns for printing
    set(ref, NULL, "class_name", idd_env$class[J(ref$class_id), on = "class_id", class_name])
    set(ref, NULL, "src_class_name", idd_env$class[J(ref$src_class_id), on = "class_id", class_name])
    set(ref, NULL, c("field_index", "field_name"),
        idd_env$field[J(ref$field_id), on = "field_id", .SD,
            .SDcols = c("field_index", "field_name")
        ]
    )
    set(ref, NULL, c("src_field_index", "src_field_name"),
        idd_env$field[J(ref$src_field_id), on = "field_id", .SD,
            .SDcols = c("field_index", "field_name")
        ]
    )

    setcolorder(ref,
        c("class_id", "class_name", "field_index", "field_name",
          "src_class_id", "src_class_name", "src_field_index", "src_field_name",
          "src_enum", "dep"
        )
    )
    cls <- switch(direction , ref_by = "IddRelationBy", ref_to = "IddRelationTo")
    setattr(ref, "class", c(cls, class(ref)))
    ref
}
# }}}

# add_group_id {{{
add_group_id <- function (idd_env, dt) {
    add_joined_cols(idd_env$class, dt, "class_id", "group_id")
}
# }}}
# add_group_name {{{
add_group_name <- function (idd_env, dt) {
    add_joined_cols(idd_env$group, dt, "group_id", "group_name")
}
# }}}
# add_class_property {{{
add_class_property <- function (idd_env, dt, property) {
    add_joined_cols(idd_env$class, dt, "class_id", property)
}
# }}}
# add_class_name {{{
add_class_name <- function (idd_env, dt) {
    if (has_name(dt, "class_name")) return(dt)
    add_joined_cols(idd_env$class, dt, "class_id", "class_name")
}
# }}}
# add_class_id {{{
add_class_id <- function (idd_env, dt) {
    if (has_name(dt, "class_id")) return(dt)
    add_joined_cols(idd_env$class, dt, "class_name", "class_id")
}
# }}}
# add_field_property {{{
add_field_property <- function (idd_env, dt, property) {
    add_joined_cols(idd_env$field, dt, "field_id", property)
}
# }}}
# add_field_full_name {{{
add_field_full_name <- function (dt) {
    col_unit <- if (in_ip_mode()) "ip_units" else "units"

    dt[is.na(get(col_unit)), full_name := field_name]
    dt[!is.na(get(col_unit)), full_name := paste0(field_name, " {", get(col_unit), "}")]

    dt
}
# }}}

# field_default_to_unit {{{
field_default_to_unit <- function (dt_field, from, to) {
    set(dt_field, NULL, "value_id", seq_along(dt_field$field_id))
    setnames(dt_field, c("default", "default_num"), c("value", "value_num"))

    dt_field <- convert_value_unit(dt_field, from, to)

    set(dt_field, NULL, "value_id", NULL)
    setnames(dt_field, c("value", "value_num"), c("default", "default_num"))
    dt_field
}
# }}}

# EXTENSIBLE GROUP
# add_idd_extensible_group {{{
add_idd_extensible_group <- function (idd_env, class, num = NULL, strict = FALSE) {
    dt_cls <- get_input_class_data(idd_env, class, num)

    # stop if non-extensible class found
    if (strict && nrow(dt_cls[num_extensible == 0L])) {
        abort("error_nonextensible_class",
            paste0("Non-extensible class found: ",
                collapse(dt_cls[num_extensible == 0L, unique(class_name)])
            )
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
    rep_dt <- function(dt, time) rbindlist(lapply(seq_len(time), function (x) dt))
    new_ext <- rbindlist(apply2(lst_ext_fld, ext$num, rep_dt))

    # get extensible group index
    idx <- ext[, list(group = rep(seq_len(num), each = num_extensible)),
        by = list(class_id, extensible_group, num_extensible)
    ]
    idx[, `:=`(index = group * num_extensible, extensible_group = group + extensible_group), by = "class_id"]

    # fix properties
    set(new_ext, NULL, "field_index", new_ext$field_index + idx$index)
    set(new_ext, NULL, "field_anid",
        paste0(new_ext$field_anid_an,
            new_ext$field_anid_num * idx$group + new_ext$field_anid_id
        )
    )
    set(new_ext, NULL, "field_name",
        stri_replace_first_regex(new_ext$field_name,
            "(?<=(#|A|N| ))\\d+", new_ext$field_name_num * idx$group + new_ext$field_name_id
        )
    )
    set(new_ext, NULL, "extensible_group", idx$extensible_group)
    set(new_ext, NULL, "field_name_us", stri_trans_tolower(underscore_name(new_ext$field_name)))
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
    dt_cls <- get_input_class_data(idd_env, class, num)

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
get_input_class_data <- function (idd_env, class, num = NULL) {
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
        dt_cls <- get_idd_class(idd_env, class,
            c(
                "min_fields", "last_required", "num_fields",
                "first_extensible", "num_extensible", "num_extensible_group"
            )
        )

        assert(are_count(num))
        assert(have_same_len(class, num))
        set(dt_cls, NULL, "num", as.integer(num))
    }
}
# }}}
