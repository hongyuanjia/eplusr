#' @importFrom cli cat_bullet cat_line cat_rule rule symbol
#' @importFrom checkmate assert_count assert_names assert_integerish
#' @importFrom checkmate test_integerish assert_integer assert_character
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

    assert_character(group, any.missing = FALSE)

    res <- idd_env$group[J(group), on = "group_name", group_id]
    if (anyNA(res)) abort_bad_key("group name", group)
    res
}
# }}}
# get_idd_group_name {{{
get_idd_group_name <- function (idd_env, group = NULL) {
    if (is.null(group)) return(idd_env$group$group_name)

    assert_integerish(group, lower = 1L, any.missing = FALSE)

    res <- idd_env$group[J(group), on = "group_id", group_name]
    if (anyNA(res)) abort_bad_key("group index", group)
    res
}
# }}}

# CLASS
# get_idd_class {{{
#' Get class data
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param class An integer vector of valid class indexes or a character vector
#'        of valid class names. If `NULL`, all classes are returned.
#' @param property A character vector of column names in class table to return.
#'        If `NULL`, only class index columns are returned, plus column `rleid`.
#' @param underscore If `TRUE`, input class name will be converted into
#'        underscore style name first and column `class_name_us` will be used
#'        for matching.
#'
#' @return A data.table containing specified columns.
#' @keywords internal
#' @export
get_idd_class <- function (idd_env, class = NULL, property = NULL, underscore = FALSE) {
    cols <- setdiff(CLASS_COLS$index, "class_name_us")

    if (is.null(class)) {
        # very odd way to subset columns but is way faster that others
        # ref: https://github.com/Rdatatable/data.table/issues/3477
        if (is.null(property)) return(fast_subset(idd_env$class, cols))

        if ("group_name" %chin% property) {
            property <- setdiff(property, "group_name")
            add_group <- TRUE
        } else {
            add_group <- FALSE
        }

        # very odd way to subset columns but is way faster
        # ref: https://github.com/Rdatatable/data.table/issues/3477
        res <- fast_subset(idd_env$class, unique(c(cols, property)))

        if (add_group) add_joined_cols(idd_env$group, res, "group_id", "group_name")

        return(res)
    }

    cls_in <- recognize_input(class, "class", underscore)
    res <- join_from_input(idd_env$class, cls_in, "group_id")

    if (!is.null(property) && "group_name" %chin% property) {
        add_joined_cols(idd_env$group, res, "group_id", "group_name")
    }

    fast_subset(res, c("rleid", unique(c(cols, property))))
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
    assert_integer(num, lower = 1L, any.missing = FALSE, null.ok = TRUE)

    dt_class <- add_rleid(dt_class, "class")

    # directly return num of fields in class
    assert_names(names(dt_class), must.include = c("num_fields", "min_fields", "last_required", "num_extensible", "first_extensible"))

    if (!nrow(dt_class)) {
        set(dt_class, NULL, "class_rleid", NULL)
        set(dt_class, NULL, "input_num", integer(0L))
        set(dt_class, NULL, "acceptable_num", integer(0L))
        return(dt_class)
    }

    if (is.null(num)) {
        set(dt_class, NULL, "input_num", 0L)
    } else {
        assert_same_len(dt_class, num)
        set(dt_class, NULL, "input_num", as.integer(num))
    }

    # get index of the last field in the last extensible group
    set(dt_class, NULL, "last_extensible", 0)
    if (any(dt_class$num_extensible > 0L)) {
        dt_class[
            num_extensible > 0L & input_num > first_extensible,
            last_extensible :=
                ceiling((input_num - first_extensible + 1L) / num_extensible) * num_extensible + first_extensible - 1L,
            by = "class_rleid"
        ]
    }

    if (any(dt_class$input_num > dt_class$num_fields)) {
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
# get_idd_class_unique {{{
get_idd_class_unique <- function (idd_env) {
    idd_env$class[J(TRUE), on = "unique_object", nomatch = NULL]
}
# }}}
# get_class_component_name {{{
get_class_component_name <- function (class) {
    nm <- stri_extract_first_regex(class, "^.+?(?=:)")
    nm[is.na(nm)] <- class[is.na(nm)]
    nm
}
# }}}

# FIELD
# get_idd_field {{{
#' Get field data
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param class An integer vector of valid class indexes or a character vector
#'        of valid class names.
#' @param field An integer vector of valid field indexes or a character
#'        vector of valid field names (can be in in underscore style).  `class`
#'        and `field` should have the same length.
#' @param property A character vector of column names in field table to return.
#' @param underscore If `TRUE`, input class name and field names will be
#'        converted into underscore style name first and column `class_name_us`
#'        and `field_name_us` will be used for matching.
#' @param no_ext If `TRUE`, no new extensible groups will be added even if there
#'        are no matched input found and an error will be issued right away.
#' @param complete If `TRUE`, at least fields till the current whole extensible
#'        group will be returned. A new column named "matched_rleid" will be
#'        created (when `property` is NULL) indicating if given field has been
#'        matched or not.
#'
#' @return A data.table containing specified columns.
#' @keywords internal
#' @export
get_idd_field <- function (idd_env, class, field = NULL, property = NULL, all = FALSE,
                           underscore = TRUE, no_ext = FALSE, complete = FALSE) {
    if (is.null(field)) {
        res <- get_idd_field_in_class(idd_env, class, all, underscore)
    } else {
        res <- get_idd_field_from_which(idd_env, class, field, underscore, no_ext, complete, all)
    }

    cols <- c("rleid", "class_id", "class_name", "field_id", "field_index", "field_name", "field_in")
    if (length(col_del <- setdiff(names(res), c(cols, property)))) {
        set(res, NULL, col_del, NULL)
    }

    res
}
# }}}
# get_idd_field_in_class {{{
get_idd_field_in_class <- function (idd_env, class, all = FALSE, underscore = TRUE) {
    prop <- if (!all) c("num_fields", "min_fields", "last_required") else NULL
    cls <- get_idd_class(idd_env, class, prop, underscore)
    set(cls, NULL, "group_id", NULL)

    if (all) {
        idd_env$field[cls, on = "class_id"]
    } else {
        set(cls, NULL, "min_required", pmax(cls$min_fields, cls$last_required))
        cls[min_required == 0L, min_required := num_fields]
        set(cls, NULL, c("num_fields", "min_fields", "last_required"), NULL)
        fld <- idd_env$field[cls, on = c("class_id", "field_index<=min_required")]
        set(fld, NULL, "field_index", rowidv(fld, c("rleid", "class_id")))
        fld
    }
}
# }}}
# get_idd_field_from_which {{{
get_idd_field_from_which <- function (idd_env, class, field, underscore = TRUE,
                                      no_ext = FALSE, complete = FALSE, all = FALSE) {
    assert_valid_type(field, "Field Index|Name")

    # class properties used for min required field num calculation
    col_prop <- c("num_fields", "min_fields", "last_required", "num_extensible",
        "first_extensible", "num_extensible_group"
    )
    dt_in <- get_idd_class(idd_env, class, col_prop, underscore)
    set(dt_in, NULL, "group_id", NULL)

    # make sure number of rows of dt_in and the length of input field is the same
    if (nrow(dt_in) == 1L) dt_in <- dt_in[rep(1L, length(field))]

    # join_field {{{
    join_field <- function (idd_env, dt_in, field_index) {
        fld <- idd_env$field[
            dt_in[, .SD, .SDcols = c("rleid", "class_id", "class_name", "field_index")],
            on = c("class_id", "field_index<=field_index"), allow.cartesian = TRUE
        ]

        # recalculate field index
        set(fld, NULL, "field_index", rowidv(fld, c("rleid", "class_id", "field_index")))
        # add field in
        ## reset input field index in order to find right place to insert
        ## field input column
        set(dt_in, NULL, "field_index", field_index)
        set(fld, NULL, "field_in", dt_in[fld, on = c("rleid", "class_id", "field_index"), field_in])
    }
    # }}}

    assert_same_len(dt_in, field, "class and field")

    if (test_integerish(field, lower = 1L, any.missing = FALSE)) {
        # from field index {{{
        field <- as.integer(field)
        set(dt_in, NULL, c("field_index", "field_in"), list(field, field))

        # return all fields
        if (all) dt_in[field_index < num_fields, field_index := num_fields]

        # get acceptable field number for each input
        dt_in <- get_idd_class_field_num(dt_in, dt_in$field_index)

        # check invalid field index
        if (dt_in[field_in > acceptable_num, .N > 0L]) {
            invld_idx <- dt_in[field_in > acceptable_num]
            abort_bad_field("index", invld_idx)
        }

        # handle extensible fields
        # there should use dt_max for efficiency
        set(dt_in, NULL, "num", 0L)
        # calculate num of groups needed to be added
        dt_in[field_index > num_fields, `:=`(num = as.integer(ceiling((field_index - num_fields) / num_extensible)))]

        # stop if adding new extensible groups is not allowed
        if (no_ext && nrow(dt_in[num > 0L])) {
            abort_bad_field("index", dt_in[num > 0L])
        }

        # add extensible groups
        idd_env <- add_idd_extensible_group(idd_env, dt_in)

        # remove unuseful columns
        set(dt_in, NULL,
            setdiff(
                names(dt_in),
                c("rleid", "class_id", "class_name", "field_index", "field_in",
                  "acceptable_num"
                )
            ),
            NULL
        )

        # only return specified fields
        if (!all && !complete) {
            fld <- idd_env$field[dt_in, on = c("class_id", "field_index"), allow.cartesian = TRUE]
            set(fld, NULL, "acceptable_num", NULL)
        } else {
            set(dt_in, NULL, "field_index", dt_in$acceptable_num)

            fld <- join_field(idd_env, dt_in, dt_in$field_in)
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
        setindexv(dt_in, c("class_id", col_on))

        # join
        dt_join <- idd_env$field[dt_in, on = c("class_id", col_on), allow.cartesian = TRUE]

        # if all matched
        if (!anyNA(dt_join$field_id)) {
            if (!all && !complete) {
                set(dt_join, NULL, c(col_prop, "field_rleid"), NULL)
                fld <- dt_join
            } else {
                # restore field index in order to insert field name input at the
                # right place
                fld_idx <- dt_join$field_index

                # return all fields
                if (all) dt_join[field_index < num_fields, field_index := num_fields]

                dt_join <- get_idd_class_field_num(dt_join, dt_join$field_index)

                set(dt_join, NULL, "field_index", dt_join$acceptable_num)

                fld <- join_field(idd_env, dt_join, fld_idx)
            }
        } else {
            # get no matched
            dt_nom <- dt_join[is.na(field_id)]

            # invalid field names for non-extensible classes
            if (any(dt_nom$class_id %in% idd_env$class[J(0L), on = "num_extensible", class_id])) {
                invld_non_ext <- dt_nom[class_id %in% idd_env$class[J(0L), on = "num_extensible", class_id]]
                abort_bad_field("name", clean_errnm_dt(invld_non_ext))
            }

            # if all names not found are in extensible class
            if (no_ext) {
                abort_bad_field("name", clean_errnm_dt(dt_nom))
            }

            # get number of field names to check per class
            dt_ext <- dt_nom[, list(
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
            dt_ext[, `:=`(num = as.integer(ceiling(num / num_extensible)))]

            # try to match names in newly added extensible groups
            idd_env <- add_idd_extensible_group(idd_env, dt_ext)
            dt_ext_join <- idd_env$field[dt_nom[, .SD, .SDcols = names(dt_in)], on = c("class_id", col_on)]

            # check invalid extensible field names
            if (anyNA(dt_ext_join$field_id)) {
                invld_nm <- clean_errnm_dt(dt_ext_join[is.na(field_id)], TRUE)
                dt_ext[, `:=`(
                    num_fields = num_fields + num * num_extensible,
                    num_extensible_group = num_extensible_group + num
                    )]
                idd_env <- del_idd_extensible_group(idd_env, dt_ext)
                abort_bad_field("name",
                    add_class_property(idd_env, invld_nm, c("min_fields", "num_fields")),
                    "\n\nNOTE: For extensible fields, new one will be added only ",
                    "when all previous extensible groups exist."
                )
            }

            # if all matched
            if (!all && !complete) {
                # {{{
                set(dt_ext_join, NULL, col_prop, NULL)
                set(dt_join, NULL, col_prop, NULL)
                fld <- append_dt(dt_join[!is.na(field_id)], dt_ext_join)
                setorderv(fld, "field_rleid")
                set(fld, NULL, "field_rleid", NULL)
                # }}}
            } else {
                # {{{
                # combine index data of non-extensible and extensible groups
                dt_in <- append_dt(dt_join[!is.na(field_id)], dt_ext_join)

                fld_idx <- dt_in$field_index

                # return all fields
                if (all) dt_in[field_index < num_fields, field_index := num_fields]

                # get acceptable field number for each input
                dt_in <- get_idd_class_field_num(dt_in, dt_in$field_index)

                set(dt_in, NULL, "field_index", dt_in$acceptable_num)

                fld <- join_field(idd_env, dt_in, fld_idx)
                # }}}
            }
        }
        # }}}
    }

    fld
}
# }}}

# REFERENCES
# get_recursive_relation {{{
get_recursive_relation <- function (all_ref, init_ref, init_dep, max_dep,
                                    col_ref, col_rev, include = NULL,
                                    both = FALSE, match_all = FALSE) {
    if (both) {
        if (substring(col_ref, 1, 3) == "src") {
            col_fld <- "src_field_id"
            col_val <- "src_value_id"
        } else {
            col_fld <- "field_id"
            col_val <- "value_id"
        }
        if (!col_val %chin% names(init_ref)) col_val <- col_fld

        # this assume that one class-name-reference is always followed by one
        # field value reference
        redundant <- init_ref[, by = c(col_rev, col_fld), {
            # for field-reference only
            if (!IDDFIELD_SOURCE$class %in% src_enum) {
                id <- integer()
            } else {
                id <- setdiff(
                    get(col_val)[src_enum == IDDFIELD_SOURCE$class],
                    get(col_val)[src_enum == IDDFIELD_SOURCE$field]
                )
            }
            list(id)
        }]
        setnames(redundant, c(col_rev, col_fld, col_val))
        set(redundant, NULL, "src_enum", IDDFIELD_SOURCE$class)

        if (nrow(redundant)) init_ref <- init_ref[!redundant, on = c(col_rev, col_val, "src_enum")]
    }

    # parent class or object
    parent <- init_ref[[col_rev]]

    # store classes or objects needed to be removed later
    del <- list()

    # log depth
    dep <- init_dep

    ref <- init_ref
    cur_ref <- init_ref
    while (dep < max_dep && nrow(cur_ref)) {
        # skip if specified classes/objects are matched
        if (!match_all && !is.null(include)) {
            skip <- unique(cur_ref[J(include), on = col_ref, col_rev, with = FALSE][[1L]])
            if (length(skip)) {
                cur_ref <- cur_ref[!J(skip), on = col_rev]

                # should also remove all unmatched in the init match
                if (dep == 0L) {
                    exclu <- ref[J(unique(skip)), on = col_rev][!J(include), on = col_ref]
                    ref <- ref[!exclu, on = c(col_rev, col_ref)]
                }
            }
        }
        # if all are matched, stop
        if (!nrow(cur_ref)) break

        # get current indirectly ref
        new_ref <- all_ref[J(unique(cur_ref[[col_ref]])), on = col_rev, nomatch = 0L]

        if (both) {
            # this assume that one class-name-reference is always followed by one
            # field value reference
            redundant <- new_ref[, by = c(col_rev, col_fld), {
                # for field-reference only
                if (!IDDFIELD_SOURCE$class %in% src_enum) {
                    id <- integer()
                } else {
                    id <- setdiff(
                        get(col_val)[src_enum == IDDFIELD_SOURCE$class],
                        get(col_val)[src_enum == IDDFIELD_SOURCE$field]
                    )
                }
                list(id)
            }]
            setnames(redundant, c(col_rev, col_fld, col_val))
            set(redundant, NULL, "src_enum", IDDFIELD_SOURCE$class)

            if (nrow(redundant)) new_ref <- new_ref[!redundant, on = c(col_rev, col_val, "src_enum")]
        }

        # get classes that do not going any deeper
        # those classes should be removed
        if (!is.null(include)) {
            del <- c(del,
                list(setattr(setdiff(cur_ref[[col_ref]], c(include, new_ref[[col_rev]])), "dep", dep))
            )
        }

        cur_ref <- new_ref

        # add depth
        dep <- dep + 1L
        # set depth value
        set(cur_ref, NULL, "dep", dep)
        # merge into the main results
        ref <- rbindlist(list(ref, cur_ref))

        # remove self reference
        cur_ref <- cur_ref[!J(parent), on = col_ref]
    }

    # should search backwards to only include paths related to specified
    # classes/objects
    if (!is.null(include) && nrow(ref)) ref <- del_recursive_relation(ref, del, include, col_ref, col_rev)

    ref
}
# }}}
# del_recursive_relation {{{
del_recursive_relation <- function (ref, target, keep, col_ref, col_rev) {
    # split ref by depth
    ref <- split(ref, by = "dep")

    # get class/object indices needed to be removed in former level
    not_found <- unique(ref[[length(ref)]][!J(keep), on = col_ref, .SD, .SDcols = col_rev][[1L]])
    # only keep specified classes for current level
    ref[[length(ref)]] <- ref[[length(ref)]][J(keep), on = col_ref, nomatch = 0L]
    # NOTE: should include classes that contain valid ref
    not_found <- setdiff(not_found, ref[[length(ref)]][[col_rev]])

    for (i in rev(seq_along(ref[-length(ref)]))) {
        # include specified classes to be removed
        not_found <- c(not_found, target[[i]])
        # get class/object indices needed to be removed in former level
        next_not_found <- unique(ref[[i]][J(not_found), on = col_ref, .SD, .SDcols = col_rev][[1L]])
        # remove at current level
        if (length(not_found)) ref[[i]] <- ref[[i]][!J(not_found), on = col_ref]
        not_found <- next_not_found
    }

    rbindlist(ref)
}
# }}}
# combine_input_and_relation {{{
combine_input_and_relation <- function (input, ref, type, direction) {
    if (type == "idd") {
        col_main <- "class_id"
        col_sub <- "field_id"
    } else if (type == "idf") {
        col_main <- "object_id"
        col_sub <- "value_id"
    }

    if (direction == "ref_by") {
        col_main <- paste0("src_", col_main)
        col_sub <- paste0("src_", col_sub)
        setnames(input, c(col_sub, col_main))
    }

    if (!nrow(ref) || max(ref$dep) == 0L) {
        set(ref, NULL, col_main, NULL)
        ref <- ref[input, on = col_sub]
        set(ref, NULL, "dep", 0L)
    } else {
        ref0 <- ref[J(0L), on = "dep"]
        set(ref0, NULL, col_main, NULL)
        ref0 <- ref0[input, on = col_sub]
        set(ref0, NULL, "dep", 0L)
        ref <- append_dt(ref0, ref[!J(0L), on = "dep"])
    }

    ref
}
# }}}

# get_idd_relation {{{
#' Get field relation data
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param class_id An integer vector of valid class indexes. Should be `NULL` if
#'        `field_id` is given.
#' @param field_id An integer vector of valid field id. Should be `NULL` if
#'        `class_id` is given.
#' @param direction The relation direction to extract. Should be one of
#'        `"ref_to"` or `"ref_by"`.
#' @param underscore If `TRUE`, input class name and field names will be
#'        converted into underscore style name first and column `class_name_us`
#'        and `field_name_us` will be used for matching.
#' @param depth If > 0, the relation is searched recursively. If `NULL`,
#'        all possible recursive relations are returned. Default: `0`.
#' @param name If `TRUE`, additional formatting columns are added and an
#'        `IddRelation` object is returned. Default: `FALSE`.
#' @param class,group A character vector of group names used for searching
#'        relations. Default: `NULL`.
#' @param keep If `TRUE`, all inputs are returned regardless they have any
#'        relations with other fields or not. If `FALSE`, only input that have
#'        relations with other fields are returned. Default: `FALSE`.
#' @param class_ref Specify how to handle class-name-references. There are 3
#'        options in total, i.e. `"none"`, `"both"` and `"all"`, with `"both"`
#'        being the default.
#'     * `"none"`: just ignore class-name-references.
#'     * `"both"`: only include class-name-references if this object
#'       also reference field values of the same one. This is the default
#'       option.
#'     * `"all"`: include all class-name-references. This is the most aggressive
#'       option.
#' @param match_all If `TRUE`, relation search will continue even though one
#'        relation has been found. If `FALSE`, searching is stopped whenever one
#'        relation is found in specified classes/groups. Default: `FALSE`.
#'
#' @return A data.table.
#'
#' @keywords internal
#' @export
get_idd_relation <- function (idd_env, class_id = NULL, field_id = NULL,
                              direction = c("ref_to", "ref_by"), depth = 0L, name = FALSE,
                              class = NULL, group = NULL, keep_all = FALSE,
                              class_ref = c("both", "none", "all"), match_all = FALSE) {
    assert_count(depth, null.ok = TRUE)
    if (is.null(depth)) depth <- Inf
    direction <- match.arg(direction)
    class_ref <- match.arg(class_ref)

    # get class reference
    if (is.null(field_id)) {
        if (is.null(class_id)) {
            id <- idd_env$class$class_id
        } else {
            id <- get_idd_class(idd_env, class_id)$class_id
        }
        col_on <- "class_id"
    } else {
        # if no class is given, assume field are valid field ids
        if (is.null(class_id)) {
            id <- field_id
        } else {
            abort("Should not specify both class id and field id at the same time",
                "idd_relation"
            )
        }
        col_on <- "field_id"
    }

    if (keep_all) {
        # make sure all input IDs appear in the result
        fld <- idd_env$field[J(id), on = col_on, .SD, .SDcols = c("field_id", "class_id")]
    }

    if (direction == "ref_by") col_on <- paste0("src_", col_on)

    all_ref <- idd_env$reference

    if (class_ref == "none") {
        both <- FALSE
        all_ref <- all_ref[!J(IDDFIELD_SOURCE$class), on = "src_enum"]
    } else if (class_ref == "all") {
        both <- FALSE
    } else if (class_ref == "both") {
        both <- TRUE
    }

    # init depth
    dep <- 0L

    # get first directly ref
    cur_ref <- all_ref[J(id), on = col_on, nomatch = 0L]
    set(cur_ref, NULL, "dep", if (nrow(cur_ref)) dep else integer())

    if (direction == "ref_to") {
        col_ref <- "src_class_id"
        col_rev <- "class_id"
    } else if (direction == "ref_by") {
        col_ref <- "class_id"
        col_rev <- "src_class_id"
    }

    # restrict searching reference ranges
    # NOTE: should do this depending on depth value
    # This makes it possible to find recursive relations, e.g. how is the
    # AirLoopHVAC related to Schedule:Compact?
    cls_id <- NULL
    if (!is.null(group)) {
        grp_id <- get_idd_group_index(idd_env, group)
        cls_id <- idd_env$class[J(grp_id), on = "group_id"]$class_id
    }
    if (!is.null(class)) {
        cls_id <- c(cls_id, get_idd_class(idd_env, class)$class_id)
    }
    if (depth == 0L && !is.null(cls_id)) {
        cur_ref <- cur_ref[J(cls_id), on = col_ref, nomatch = 0L]
    }

    # no matched found for specified classes or groups
    if (!is.null(cls_id) && !length(cls_id)) all_ref <- all_ref[0L]

    # get recursive relation
    ref <- get_recursive_relation(all_ref, cur_ref, dep, depth, col_ref,
        col_rev, cls_id, both = both, match_all = match_all)

    # keep all input
    if (keep_all) ref <- combine_input_and_relation(fld, ref, "idd", direction)

    if (!name) return(ref)

    ref <- add_idd_relation_format_cols(idd_env, ref)

    cls <- switch(direction, ref_by = "IddRelationBy", ref_to = "IddRelationTo")
    setattr(ref, "class", c(cls, class(ref)))
    ref
}
# }}}
# add_idd_relation_format_cols {{{
add_idd_relation_format_cols <- function (idd_env, ref) {
    # add all necessary columns for printing
    ref <- add_joined_cols(idd_env$class, ref, "class_id", "class_name")
    ref <- add_joined_cols(idd_env$class, ref, c(src_class_id = "class_id"),
        c(src_class_name = "class_name"))
    ref <- add_joined_cols(idd_env$field, ref, "field_id", c("field_index", "field_name"))
    ref <- add_joined_cols(idd_env$field, ref, c(src_field_id = "field_id"),
        c(src_field_index = "field_index", src_field_name = "field_name"))

    setcolorder(ref,
        c("class_id", "class_name", "field_id", "field_index", "field_name",
          "src_class_id", "src_class_name", "src_field_id", "src_field_index", "src_field_name",
          "src_enum", "dep"
        )
    )

    ref
}
# }}}

# PROPERTY COLUMNS
# add_class_id {{{
add_class_id <- function (idd_env, dt) {
    add_joined_cols(idd_env$class, dt, "class_name", "class_id")
}
# }}}
# add_class_name {{{
add_class_name <- function (idd_env, dt) {
    add_joined_cols(idd_env$class, dt, "class_id", "class_name")
}
# }}}
# add_class_property {{{
add_class_property <- function (idd_env, dt, property) {
    add_group <- FALSE
    if ("group_name" %in% property) {
        if ("group_id" %in% property) {
            add_id <- FALSE
            property <- property[property != "group_name"]
        } else {
            property <- c(property[property != "group_name"], "group_id")
            add_id <- TRUE
        }
        add_group <- TRUE
    }

    add_joined_cols(idd_env$class, dt, "class_id", property)

    if (add_group) {
        add_joined_cols(idd_env$group, dt, "group_id", "group_name")
        if (add_id) set(dt, NULL, "group_id", NULL)
    }

    dt
}
# }}}
# add_field_property {{{
add_field_property <- function (idd_env, dt, property) {
    add_joined_cols(idd_env$field, dt, "field_id", property)
}
# }}}

# UNIT CONVERSION
# field_default_to_unit {{{
field_default_to_unit <- function (idd_env, dt_field, from, to) {
    if (has_names(dt_field, "value_id")) {
        value_id <- dt_field$value_id
    } else {
        value_id <- NULL
    }
    if (has_names(dt_field, "value_chr")) {
        setnames(dt_field, c("value_chr", "value_num"), paste0(c("value_chr", "value_num"), "-backup"))
    }
    set(dt_field, NULL, "value_id", seq_along(dt_field$field_id))

    cols_add <- NULL
    if (!has_names(dt_field, "default_chr")) cols_add <- "default_chr"
    if (!has_names(dt_field, "default_num")) cols_add <- c(cols_add, "default_num")
    if (!is.null(cols_add)) add_field_property(idd_env, dt_field, cols_add)

    setnames(dt_field, c("default_chr", "default_num"), c("value_chr", "value_num"))

    dt_field <- convert_value_unit(idd_env, dt_field, from, to)

    set(dt_field, NULL, "value_id", value_id)
    setnames(dt_field, c("value_chr", "value_num"), c("default_chr", "default_num"))

    if (has_names(dt_field, "value_chr-backup")) {
        setnames(dt_field, paste0(c("value_chr", "value_num"), "-backup"), c("value_chr", "value_num"))
    }

    dt_field
}
# }}}

# EXTENSIBLE GROUP
# add_idd_extensible_group {{{
add_idd_extensible_group <- function (idd_env, class, num = NULL, strict = FALSE) {
    dt_cls <- get_input_class_data(idd_env, class, num)

    # stop if non-extensible class found
    if (strict && nrow(dt_cls[num_extensible == 0L])) {
        abort(paste0("Non-extensible class found: ",
            collapse(dt_cls[num_extensible == 0L, unique(class_name)])
        ), "non_extensible_class")
    }

    ext <- dt_cls[num_extensible > 0L & num > 0L]

    if (!nrow(ext)) return(idd_env)

    # get unique class data
    ext <- ext[,
        list(
            num = max(num),
            extensible_group = num_extensible_group[[1L]],
            num_extensible = num_extensible[[1L]],
            last_required = last_required[[1L]]
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
    set(new_ext, NULL, "new_fld_id", new_fld_id)

    # get field reference data
    ref <- idd_env$reference[new_ext[, list(field_id, new_fld_id)], on = "field_id"][
        , `:=`(field_id = new_fld_id)][!is.na(src_field_id)]
    src <- idd_env$reference[new_ext[, list(src_field_id = field_id, new_fld_id)], on = "src_field_id"][
        , `:=`(src_field_id = new_fld_id)][!is.na(field_id)]
    new_ref <- rbindlist(list(ref, src))

    # combine into the main field table and name table
    idd_env$field <- append_dt(idd_env$field, new_ext[, field_id := new_fld_id])
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
        abort(paste0("Non-extensible class found: ",
            collapse(dt_cls[num_extensible == 0L, unique(class_name)])
        ), "non_extensible_class")
    }

    ext <- dt_cls[num_extensible > 0L & num > 0L]

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
        abort(mes, data = less)
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
        assert_names(names(dt_cls),
            must.include = c("min_fields", "num_fields", "num_extensible", "last_required", "num_extensible_group")
        )

        if (is.null(num)) {
            assert_names(names(dt_cls), must.include = "num")
            set(dt_cls, NULL, "num", as.integer(dt_cls$num))
        } else {
            num <- assert_integerish(num, lower = 1L, any.missing = FALSE, coerce = TRUE)
            set(dt_cls, NULL, "num", num)
        }

    } else {
        # get class data
        dt_cls <- get_idd_class(idd_env, class,
            c(
                "min_fields", "last_required", "num_fields",
                "num_extensible", "num_extensible_group"
            )
        )

        num <- assert_integerish(num, lower = 1L, any.missing = FALSE, coerce = TRUE, len = 1L)
        assert_same_len(class, num)
        set(dt_cls, NULL, "num", num)
    }
}
# }}}

# TABLE
# get_idd_table {{{
get_idd_table <- function (idd_env, class, all = FALSE) {
    assert_valid_type(class, "Class Index|Name")
    fld <- get_idd_field(idd_env, class, all = all)[
        , .SD, .SDcols = c("class_name", "field_index", "field_name")
    ]

    setnames(fld, c("class_name", "field_index", "field_name"), c("class", "index", "field"))

    fld
}
# }}}

# STRING
# get_idd_string {{{
get_idd_string <- function (idd_env, class, leading = 4L, sep_at = 29L, sep_each = 0L, all = FALSE) {
    assert_valid_type(class, "Class Index|Name")
    assert_count(sep_each)

    fld <- get_idd_field(idd_env, class, property = c("units", "ip_units"), all = all)

    # add fake value in order to correctly format
    set(fld, NULL, "value_chr", NA_character_)
    set(fld, NULL, "field", format_field(fld, leading = leading, sep_at = sep_at))

    sep <- rep("", sep_each)
    out <- fld[, list(out = list(c(paste0(class_name[[1L]], ","), field, sep))), by = "class_id"]

    unlist(out$out, use.names = FALSE)
}
# }}}
