#' @importFrom cli cat_bullet cat_line cat_rule rule symbol
#' @importFrom crayon bold cyan red strip_style underline
#' @importFrom data.table copy data.table dcast rbindlist
#' @importFrom data.table setattr setcolorder setnames setorder setorderv
#' @importFrom stringr str_detect str_match str_replace_all
#' @importFrom stringi stri_locate_first_regex stri_replace_first_regex "stri_sub<-"
#' @importFrom stringi stri_subset_regex stri_match_first_regex stri_rand_strings
#' @importFrom uuid UUIDgenerate
NULL

# GROUP
# t_group_name {{{
t_group_name <- function (dt_class, dt_group, class = NULL) {
    if (is.null(class)) {
        grp_id <- dt_class[order(group_id), unique(group_id)]
    } else {
        grp_id <- t_class_data(dt_class, class)$group_id
    }

    dt_group[J(grp_id), on = "group_id", group_name]
}
# }}}
# t_group_index {{{
t_group_index <- function (dt_group, group = NULL) {
    if (is.null(group)) return(dt_group$group_id)

    stopifnot(is.character(group))

    res <- dt_group[J(group), on = "group_name", group_id]
    if (anyNA(res)) abort_bad_key("error_group_name", "group name", group)
    res
}
# }}}

# CLASS
# t_class_data {{{
# Get class data
# @param dt_class A data.table containing all class data. Usually generated from
#        parsing an IDD.
#' @param class An integer vector of valid class indexes or a character vector
#'        of valid class names or a data.table that contains column `class_id`
#'        or `class_name`. If `NULL`, `dt_class` will be returned directly.
#' @param cols A character vector of column names in class table to return. If
#'        `NULL`, all columns from `dt_class` will be returned, plus column
#'        `rleid`.
#' @param underscore If `TRUE`, input class name will be converted into
#'        underscore style name first and column `class_name_us` will be used
#'        for matching.
# @return A data.table containing specified columns.
t_class_data <- function (dt_class, class = NULL, cols = NULL, underscore = FALSE) {
    if (is.null(class)) return(dt_class)

    if (inherits(class, "data.table")) {
        dt_in <- class
        if (!has_name(dt_in, "rleid")) add_rleid(dt_in)
        if (has_name(dt_in, "class_id")) {
            col_on <- "class_id"
            col_key <- "class index"
        } else {
            stopifnot(has_name(dt_in, "class_name"))
            if (underscore) {
                if (!has_name(dt_in, "class_name_us")) {
                    set(dt_in, NULL, "class_name_us", underscore_name(dt_in$class_name))
                }
                col_on <- "class_name_us"
            } else {
                col_on <- "class_name"
            }
            col_key <- "class name"
        }
    } else {
        if (is.character(class)) {
            if (underscore) {
                class <- underscore_name(class)
                col_on <- "class_name_us"
            } else {
                col_on <- "class_name"
            }
            col_key <- "class name"
        } else if (are_count(class)) {
            col_on <- "class_id"
            col_key <- "class index"
        } else {
            abort_bad_which_type("error_class_which_type", "class")
        }
        dt_in <- data.table(class = class, rleid = seq_along(class))
        setnames(dt_in, "class", col_on)
    }

    res <- dt_class[dt_in, on = col_on, allow.cartesian = TRUE]

    if (anyNA(res$group_id)) {
        invld_cls <- res[is.na(group_id)][[col_on]]
        abort_bad_key(paste0("error_", col_on), col_key, invld_cls)
    }

    if (is.null(cols)) return(res)

    res[, .SD, .SDcols = cols]
}
# }}}
# t_class_id_unique {{{
t_class_id_unique <- function (dt_class) {
    dt_class[unique_object == TRUE, class_id]
}
# }}}
# t_class_id_required {{{
t_class_id_required <- function (dt_class) {
    dt_class[required_object == TRUE, class_id]
}
# }}}
# t_class_id_extensible {{{
t_class_id_extensible <- function (dt_class) {
    dt_class[num_extensible > 0L, class_id]
}
# }}}
# t_class_id_nonextensible {{{
t_class_id_nonextensible <- function (dt_class) {
    dt_class[num_extensible == 0L, class_id]
}
# }}}
# t_class_name_unique {{{
t_class_name_unique <- function (dt_class) {
    dt_class[unique_object == TRUE, class_name]
}
# }}}
# t_class_name_required {{{
t_class_name_required <- function (dt_class) {
    dt_class[required_object == TRUE, class_name]
}
# }}}
# t_class_name_extensible {{{
t_class_name_extensible <- function (dt_class) {
    dt_class[num_extensible > 0L, class_name]
}
# }}}
# t_class_name_nonextensible {{{
t_class_name_nonextensible <- function (dt_class) {
    dt_class[num_extensible == 0L, class_name]
}
# }}}

# OBJECT
# t_object_data {{{
#' Get object data.
#'
#' @param dt_object A data.table containing all object data. Usually generated
#'        from parsing an IDF.
#' @param class An integer vector of valid class indexes or a character vector
#'        of valid class names or a data.table that contains column `class_id`
#'        or `class_name` to specify which classes should be considered when
#'        matching object ID or names. If `class` is a data.table with both
#'        `class_id` and `class_name` column, `class_id` will be used.
#' @param object An integer vector of valid object IDs or a character vector
#'        of valid object names or a data.table that contains column `object_id`
#'        or `object_name`. If `object` is a data.table with both `object_id`
#'        and `object_name` column, `object_id` will be used.
#' @param cols A character vector of column names in class table to return. If
#'        `NULL`, all columns from `dt_object` will be returned, plus column
#'        `rleid`.
#' @param underscore If `TRUE`, input class name will be converted into
#'        underscore style name first and column `class_name_us` will be used
#'        for matching.
#' @param ignore_case If TRUE, object name matching will be case-insensitive.
#'
#' If one input object matches multiple objects, an error will be issued.
#'
#' @return A data.table containing specified columns.
#' TODO: remove `for_value`
t_object_data <- function (dt_object, class = NULL, object = NULL, cols = NULL,
                           underscore = FALSE, ignore_case = FALSE, for_value = FALSE) {
    obj <- t_class_data(dt_object, class, cols, underscore)

    if (is.null(object)) return(obj)

    if (inherits(object, "data.table")) {
        dt_in <- object

        if (!has_name(dt_in, "rleid")) add_rleid(dt_in)

        if (has_name(dt_in, "object_id")) {
            col_on <- "object_id"
            col_key <- "object id"
        } else {
            stopifnot(has_name(dt_in, "object_name"))
            col_key <- "object name"
            if (ignore_case) {
                if (!has_name(dt_in, "object_name_lower")) {
                    set(dt_in, NULL, "object_name_lower", stri_trans_tolower(dt_in$object_name))
                }
                col_on <- "object_name_lower"
            } else {
                col_on <- "object_name"
            }
        }
    } else {
        if (is.character(object)) {
            if (ignore_case) {
                col_on <- "object_name_lower"
                object <- stri_trans_tolower(object)
            } else {
                col_on <- "object_name"
            }
            col_key <- "object name"
        } else if (are_count(object)){
            col_on <- "object_id"
            col_key <- "object id"
        } else {
            abort_bad_which_type("error_object_which_type", "object")
        }

        dt_in <- data.table(object = object, rleid = seq_along(object))
        setnames(dt_in, "object", col_on)
    }

    res <- obj[dt_in, on = col_on, allow.cartesian = TRUE]

    # stop if there are objects that have the same name
    if (!isTRUE(for_value) && !is_same_len(res, dt_in)) {
        mult_rleid <- res[, .N, by = rleid][N > 1L, rleid]
        mult <- res[J(mult_rleid), on = "rleid"]

        set(mult, NULL, "object",
            t_object_info(mult, c("id", "class"), numbered = FALSE, prefix = "")
        )

        m <- mult[, list(m = paste("Name", surround(object_name[1L]), "matches", collapse(object, NULL))),
            by = c("rleid", "object_name_lower")][, m := paste0(" #", rpad(rleid), "| ",m)]$m

        abort("error_multiple_matched",
            paste0(
                "Input object name matched multiple results. Please use object ID instead:\n",
                m
            ),
            data = res
        )
    }

    if (anyNA(res$group_id)) {
        invld_obj <- res[is.na(group_id)][[col_on]]
        abort_bad_key(paste0("error_", col_on), col_key, invld_obj)
    }

    res
}
# }}}
# t_object_id {{{
t_object_id <- function (dt_object, class = NULL, simplify = FALSE) {
    obj <- t_object_data(dt_object, class,
        cols = c("class_id", "class_name", "rleid", "object_id")
    )

    if (simplify) return(obj$object_id)

    if (is.null(class)) {
        col_by <- "class_id"
        setorderv(obj, "class_id")
        nm <- unique(obj$class_name)
    } else {
        col_by <- "rleid"
        nm <- unique(obj[, list(rleid, class_name)])$class_name
    }

    res <- lapply(split(obj, by = col_by, keep.by = FALSE), `[[`, "object_id")
    data.table::setattr(res, "names", nm)
    res
}
# }}}
# t_object_name {{{
t_object_name <- function (dt_object, class = NULL, simplify = FALSE, lower = FALSE) {
    obj <- t_object_data(dt_object, class,
        cols = c("class_id", "rleid", "object_name")
    )

    if (lower) set(obj, NULL, "object_name", stri_trans_tolower(obj$object_name))

    if (simplify) return(obj$object_name)

    if (is.null(class)) {
        col_by <- "class_id"
        setorderv(obj, "class_id")
        nm <- unique(obj$class_name)
    } else {
        col_by <- "rleid"
        nm <- class
    }

    res <- lapply(split(obj, by = col_by, keep.by = FALSE), `[[`, "object_name")
    setattr(res, "names", nm)
    res
}
# }}}
# t_object_num {{{
t_object_num <- function (dt_object, class = NULL) {
    if (is.null(class)) return(nrow(dt_object))

    col_on <- if(is.character(class)) "class_name" else "class_id"
    setindexv(dt_object, col_on)
    dt_object[
        J(unique(class)), on = col_on][
        , .N, by = list(class_id, class_name, found = !is.na(object_id))][
        found == FALSE, `:=`(N = 0L)][
        J(class), on = col_on]$N
}
# }}}
# t_object_info {{{
t_object_info <- function (dt_object, component = c("id", "name", "class"),
                           by_class = FALSE, numbered = TRUE, collapse = NULL,
                           prefix = NULL) {
    stopifnot(all(component %in% c("id", "name", "class")))

    if (is.null(prefix)) {
        key_obj <- "Object"
        key_cls <- "Class"
    } else {
        key_obj <- "object"
        key_cls <- "class"
    }

    order_id <- match("id", component, nomatch = 0L)
    order_nm <- match("name", component, nomatch = 0L)
    order_cls <- match("class", component, nomatch = 0L)

    mes <- NULL
    # if ID is required
    if (order_id != 0L) {
        mes_id <- dt_object[,
            ifelse(object_id < 0L,
                paste0("Input #", -object_id),
                paste0("ID [", object_id, "]")
            )
        ]
        mes <- mes_id
    }

    # if name is required
    if (order_nm != 0L) {
        mes_nm <- dt_object[, {
            mes <- paste0("name ", surround(object_name))
            mes[is.na(object_name)] <- ""
            mes
        }]
        # if name comes after ID
        if (order_nm > order_id) {
            # if ID is not required
            if (order_id == 0L) {
                mes <- mes_nm
            # if ID is required
            } else {
                # surround name with parenthesis
                mes_nm[!stri_isempty(mes_nm)] <- paste0(" (", mes_nm[!stri_isempty(mes_nm)], ")")
                mes <- paste0(mes, mes_nm)
            }
        # if name comes before ID
        } else {
            # surround ID with parenthesis
            mes <- paste0(mes_nm, "(", mes, ")")
        }
    }

    # If class is required
    if (order_cls != 0L) {
        # If none of ID or name is required
        if (is.null(mes)) {
            mes <- dt_object[, paste0(key_cls, " ", surround(class_name))]
        } else {
            set(dt_object, NULL, "mes_object", mes)
            if (by_class) {
                mes <- dt_object[, {
                    paste0(key_obj, " ", collapse(mes_object, NULL), " in class ", surround(class_name[1L]))
                }, by = class_name]$V1
            } else {
                mes <- dt_object[, {
                    paste0(key_obj, " ", mes_object, " in class ", surround(class_name))
                }]
            }
            set(dt_object, NULL, "mes_object", NULL)
        }
    } else {
        if (!is.null(mes)) {
            mes <- paste0(key_obj, " ", mes)
        }
    }

    mes <- paste0(prefix, mes)

    if (numbered) {
        if (has_name(dt_object, "rleid")) {
            if (by_class) {
                num <- rpad(paste0(" #", dt_object[, unique(rleid), by = class_name]$V1, "| "))
            } else {
                num <- rpad(paste0(" #", dt_object$rleid, "| "))
            }
        } else {
            num <- rpad(paste0(" #", seq_along(mes), "| "))
        }
        mes <- paste0(num, mes)
    }

    paste0(mes, collapse = collapse)
}
# }}}

# FIELD
# t_field_data {{{
# Get specified field data
#' @param dt_idd An environment contains IDD tables including class, fields, and
#'        references.
#' @param class An integer vector of valid class indexes or a character vector
#'        of valid class names or a data.table that contains column `class_id`
#'        and `rleid`. If a data.table that contains a column `object_id`, that
#'        column will be preserved.
#' @param field NULL or an integer vector of valid field indexes or a character
#'        vector of valid field names (in underscore style). If not `NULL`,
#'        `class` and `field` should have the same length.
#' @param cols A character vector of column names in field table to return. If
#'        `NULL`, all columns from IDD field table will be returned, plus column
#'        `rleid`, `object_id` (if applicable) and `matched_rleid` (if
#'        `complete` is `TRUE`).
#' @param underscore If `TRUE`, input class name and field names will be
#'        converted into underscore style name first and column `class_name_us`
#'        and `field_name_us` will be used for matching.
#' @param no_ext Only used when `field` is not `NULL`. If `TRUE`, no new
#'        extensible groups will be added even if there are no matched input
#'        found and an error will be issued right away.
#' @param all If `TRUE` and `field` is NULL or is smaller than the total
#'        existing field number in that class, data of all fields will be
#'        returned. Default is `FALSE`, which means that only minimum fields
#'        will be returned.
#' @param complete Only used when `field` is not `NULL`. If `TRUE`, at least
#'        fields till the current whole extensible group will be returned. A new
#'        column named "matched_rleid" will be created (when `cols` is NULL)
#'        indicating if given field has been matched or not.
t_field_data <- function (
    dt_idd, class, field = NULL, cols = NULL,
    underscore = TRUE, no_ext = FALSE, all = FALSE, complete = FALSE
)
{
    if (!is.null(field) && !is.character(field) && !are_count(field)) {
        abort_bad_which_type("error_field_which_type", "field")
    }

    cols_req <- c("rleid",
        "class_id", "class_name", "min_fields", "last_required", "num_fields",
        "first_extensible", "num_extensible", "num_extensible_group"
    )

    if (underscore) cols_req <- c(cols_req, "class_name_us")

    # get class data
    if (inherits(class, "data.table")) {
        dt_in <- class
        col_obj_id <- c(
            names(dt_in)[names(dt_in) == "object_id"],
            names(dt_in)[names(dt_in) == "object_rleid"]
        )
        cols_req <- c(cols_req, col_obj_id)
        dt_in <- dt_idd$class[ dt_in[, .SD, .SDcols = c("class_id", "rleid", col_obj_id)],
            on = "class_id"][
            , .SD, .SDcols = cols_req]
    } else {
        dt_in <- t_class_data(dt_idd$class, class, cols_req, underscore = underscore)
        # make sure number of rows of dt_in and the length of input field is the same
        if (is_scalar(class) && !is.null(field)) dt_in <- dt_in[rep(1L, length(field))]
        col_obj_id <- c(
            names(dt_in)[names(dt_in) == "object_id"],
            names(dt_in)[names(dt_in) == "object_rleid"]
        )
    }

    if (is.null(cols)) {
        cols <- c("rleid", names(dt_idd$field), col_obj_id)
        if (complete) cols <- c(cols, "matched_rleid")
    }

    # get_index_dt: return an index dt for non-equi join {{{
    get_index_dt <- function (dt_in, all = FALSE, cols = NULL) {
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
        add_accept_field_num(dt_last, num = dt_last$field_index, all = all, keep_input = TRUE)[
            , field_index := acceptable_num][
            , .SD, .SDcols = c("rleid", "class_id", "field_index", cols)]
    }
    # }}}

    # field_from_idx: return field data by using an index dt {{{
    fields_from_idx <- function (dt_idd, dt_idx, col = "field_index") {
        fld <- dt_idd$field[dt_idx, on = c("class_id", paste0("field_index<=", col)), allow.cartesian = TRUE]
        set(fld, NULL, "field_index", rowidv(fld, c("rleid", "class_id")))
        fld
    }
    # }}}

    if (is.null(field)) {
        return(
            fields_from_idx(dt_idd, get_index_dt(dt_in, all = all, cols = col_obj_id))[
                , .SD, .SDcols = cols]
        )
    }

    assert_that(is_same_len(dt_in, field))

    if (are_count(field)) {
        # from field index {{{
        col_on <- "field_index"
        set(dt_in, NULL, "field_index", field)

        dt_idx <- get_index_dt(dt_in, all = all, cols = c(col_obj_id,
            "input_num", "num_fields", "num_extensible", "num_extensible_group",
            "last_required")
        )

        # check invalid field index
        if (nrow(dt_idx[field_index == 0L])) {
            invld_idx <- dt_in[dt_idx[field_index == 0L, list(rleid)], on = "rleid"]
            abort_bad_field("error_bad_field_index", "index", invld_idx)
        }

        # handle extensible fields
        set(dt_idx, NULL, "num", 0L)
        dt_idx[field_index > num_fields, `:=`(num = as.integer((field_index - num_fields) / num_extensible))]

        if (no_ext && nrow(dt_idx[num > 0L])) {
            abort_bad_field("error_bad_field_index", "index", dt_in[num > 0L])
        }

        dt <- t_add_extensible_group(dt_idd, dt_idx)
        if (!complete) {
            fld <- dt_idd$field[dt_in, on = c("class_id", col_on)]
        } else {
            fld <- fields_from_idx(dt, dt_idx, "field_index")[dt_in, on = list(rleid, field_index),
                matched_rleid := .I]
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
        dt_join <- dt_idd$field[dt_in, on = c("class_id", col_on)]

        # get no matched
        dt_nom <- dt_join[is.na(field_id)]

        # invalid field names for non-extensible classes
        invld_nm_non_ext <- dt_nom[class_id %chin% t_class_id_nonextensible(dt_idd$class)]
        invld_nm_non_ext <- clean_errnm_dt(invld_nm_non_ext, FALSE)

        # fields for extensible classes
        dt_nom_ext <- dt_nom[!invld_nm_non_ext, on = "field_rleid"]

        # get matched
        dt_m <- dt_join[!dt_nom, on = "field_rleid"]

        if (complete) {
            dt_idx <- get_index_dt(dt_m, all = all, cols = col_obj_id)
            dt_m <- fields_from_idx(dt_idd, dt_idx)[dt_m, on = list(rleid, field_index),
                matched_rleid := .I]
            dt_m[is.na(matched_rleid), matched_rleid := 0L]
        }

        # if checking completes and errors are found
        if (no_ext) {
            if (nrow(dt_nom_ext)) {
                abort_bad_field("error_bad_field_name", "name", clean_errnm_dt(dt_na))
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

        dt <- t_add_extensible_group(dt_idd, dt_ext)
        dt_ext_join <- dt$field[
            dt_nom_ext[, .SD, .SDcols = c("class_id", col_on, setdiff(names(dt_nom_ext), names(dt$field)))],
            on = c("class_id", col_on)]

        # check invalid extensible field names
        invld_nm_ext <- clean_errnm_dt(dt_ext_join[is.na(field_id)], TRUE)
        invld_nm <- rbindlist(list(invld_nm_non_ext, invld_nm_ext))
        if (nrow(invld_nm)) {
            dt_ext[, num_fields := num_fields + num * num_extensible]
            dt <- t_del_extensible_group(dt, dt_ext)
            abort_bad_field("error_bad_field_name", "name", invld_nm,
                "\n\nNOTE: For extensible fields, new one will be added only ",
                "when all previous extensible groups exist."
            )
        }

        fld <- rbindlist(list(dt_m, dt_ext_join), fill = TRUE)
        if (complete) {
            dt_ext_idx <- get_index_dt(dt_ext_join, all = all, cols = col_obj_id)

            # combine index data of non-extensible and extensible groups
            dt_idx <- rbindlist(list(dt_ext, dt_ext_idx))[
                , list(field_index = max(field_index)), by = c("rleid", col_obj_id, "class_id")]

            # mark those matched field
            fld <- fields_from_idx(dt_idd, dt_idx)[fld, on = list(rleid, field_index),
                matched_rleid := .I]
            fld[is.na(matched_rleid), matched_rleid := 0L]
        }
        # }}}
    }

    if (!is_same_len(dt_idd$field, dt$field)) {
        warning("`dt_idd` is not an environment and IDD data has been changed during query")
    }

    fld[, .SD, .SDcols = cols]
}
# }}}
# t_field_default_to_unit {{{
t_field_default_to_unit <- function (dt_field, from, to) {
    from <- match.arg(from, c("si", "ip"))
    to <- match.arg(to, c("si", "ip"))

    if (identical(from, to)) return(dt_field)

    set(dt_field, NULL, "value_id", seq_along(dt_field$field_id))
    set(dt_field, NULL, "value_num", NA_real_)

    dt_field[vapply(default, is.numeric, logical(1L)), `:=`(value_num = unlist(default))]

    dt_field <- convert_value_unit(dt_field, from, to)

    dt_field[!is.na(value_num), `:=`(default = as.list(value_num))]

    int_trunc <- dt_field[type_enum == .globals$type$integer & !are_integerish(value_num)]
    if (nrow(int_trunc)) {
        mes <- paste0("Truncation errors introduced when converting integer ",
            "default values from ", toupper(from), " unit to ", toupper(to), " unit.")
        warn("warning_default_int_trunc", mes, int_trunc)
    }

    dt_field[type_enum == .globals$type$integer, `:=`(default = as.list(as.integer(value_num)))]
    set(dt_field, NULL, c("value_id", "value_num"), NULL)

    dt_field
}
# }}}
# t_field_reference {{{
t_field_reference <- function (fld, dt_reference, dt_value) {
    # get all nodes
    fld[type_enum == .globals$type$node, `:=`(node_value = list(t_value_all_node(dt_value)))]

    # get all ids of unique field references
    fld_id_ref <- fld[type_enum == .globals$type$object_list, list(field_id = unique(field_id))]

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
    fld <- fld_src[fld, on = "field_id"][type_enum == .globals$type$node, `:=`(src_value = node_value)]

    set(fld, NULL, "node_value", NULL)

    fld
}
# }}}

# VALUE
# t_value_data_in_class {{{
# Return all object value data in a class
#' @param dt_idd An environment contains IDD tables including class, field, and
#'        reference.
#' @param dt_idf An environment contains IDF tables including object, value, and
#'        reference.
#' @param class An integer vector of valid class indexes or a character vector
#'        of valid class names or a data.table that contains columns `class_id`
#'        or `class_name` and `class_name_us` (when `underscore` is TRUE). If
#'        both `class_id` and `class_name` exist, `class_id` will be used for
#'        matching.
#' @param field NULL or an integer vector of valid field indexes or a character
#'        vector of valid field names (in underscore style). If not `NULL`,
#'        `class` and `field` should have the same length.
#' @param cols A character vector of column names in value table to return. If
#'        `NULL`, all columns from object value table will be returned, plus
#'        column `rleid`.
#' @param underscore If `TRUE`, input class name will be converted into
#'        underscore style name first and column `class_name_us` in IDD class
#'        table will be used for matching.
#' @param fill If `TRUE`, all field number will be the same.
t_value_data_in_class <- function (dt_idd, dt_idf, class, field = NULL, cols = NULL,
                                   underscore = FALSE, fill = FALSE) {
    stopifnot(!is.null(class))

    browser()
    # get all fields
    fld <- t_field_data(dt_idd, class, field, underscore = underscore)
    val <- fld[dt_idf$value, on = "field_id"][, .SD, .SDcols = cols]

    if (fill) {
        browser()
    }

}
# }}}
# t_value_data_in_object {{{
# Return object value data
#' @param dt_idd An environment contains IDD tables including class, field, and
#'        reference.
#' @param dt_idf An environment contains IDF tables including object, value, and
#'        reference.
#' @param class An integer vector of valid class indexes or a character vector
#'        of valid class names or a data.table that contains column `class_id`
#'        or `class_name` to specify which classes should be considered when
#'        matching object ID or names. If `class` is a data.table with both
#'        `class_id` and `class_name` column, `class_id` will be used.
#' @param object An integer vector of valid object IDs or a character vector
#'        of valid object names or a data.table that contains column `object_id`
#'        or `object_name`. If `object` is a data.table with both `object_id`
#'        and `object_name` column, `object_id` will be used.
#' @param field NULL or an integer vector of valid field indexes or a character
#'        vector of valid field names (in underscore style). If not `NULL`,
#'        `class` and `field` should have the same length.
#' @param cols A character vector of column names in value table to return. If
#'        `NULL`, all columns from object value table will be returned, plus
#'        column `rleid`.
#' @param underscore If `TRUE`, input class name will be converted into
#'        underscore style name first and column `class_name_us` in IDD class
#'        table will be used for matching.
#' @param ignore_case If TRUE, object name matching will be case-insensitive.
t_value_data_in_object <- function (dt_idd, dt_idf, class = NULL, object, field = NULL, cols = NULL,
                                    underscore = FALSE, ignore_case = FALSE) {
    stopifnot(!is.null(object))
    if (is.null(cols)) cols <- unique(c(names(object), names(dt_idf$value)))

    obj <- t_object_data(dt_idf$object, class, object,
        ignore_case = ignore_case, underscore = underscore)

    if (is.null(field)) {
        return(dt_idf$value[obj, on = "object_id"][, .SD, .SDcols = cols])
    }

    fld <- t_field_data(dt_idd, obj, field, underscore = underscore,
        cols = intersect(names(dt_idd$field), names(dt_idf$value)))

    dt_idf$value[, .SD, .SDcols = c("object_id", "value_id", "value", "value_num", "field_id")][
        cbind(obj[, .SD, .SDcols = setdiff(names(obj), names(fld))], fld),
        on = c("object_id", "field_id")][, .SD, .SDcols = cols]
}
# }}}
# t_value_data {{{
# Get specified value data
#' @param dt_idd An environment contains IDD tables including class, field, and
#'        reference.
#' @param dt_idf An environment contains IDF tables including object, value, and
#'        reference.
#' @param class An integer vector of valid class indexes or a character vector
#'        of valid class names to specify classes to search for. If `NULL`, then
#'        all classes existing in current IDF wil be used.
#' @param object An integer vector of valid object IDs or a character vector
#'        of valid object names.
#' @param field NULL or an integer vector of valid field indexes or a character
#'        vector of valid field names (in underscore style). If not `NULL`,
#'        `class` and `field` should have the same length.
#' @param cols A character vector of column names in field tables to return. If
#'        `NULL`, all columns from IDD field table will be returned, plus column
#'        `rleid`, `object_id` (if applicable) and `matched_rleid` (if
#'        `complete` is `TRUE`).
#' @param no_ext Only used when `field` is not `NULL`. If `TRUE`, no new
#'        extensible groups will be added even if there are no matched input
#'        found and an error will be issued right away.
#' @param all If `TRUE` and `field` is NULL or is smaller than the total
#'        existing field number in that class, data of all fields will be
#'        returned. Default is `FALSE`, which means that only minimum fields
#'        will be returned.
#' @param complete Only used when `field` is not `NULL`. If `TRUE`, at least
#'        fields till the current whole extensible group will be returned. A new
#'        column named "matched_rleid" will be created indicating if given field
#'        has been matched or not.
#' @details
#' (a) To get all object value data in a specific class:
#'
#' t_value_data(dt_idd, dt_idf, class = "XXX", fill = FALSE)
#'
#' (b) To get all object value data in a specific class, and make sure returned
#'     field number is the same across all objects in that class:
#'
#' t_value_data(dt_idd, dt_idf, class = "XXX", fill = TRUE)
#'
#' (c) To search objects in a specific class:
#'
#' t_value_data(dt_idd, dt_idf, class = "XXX", object = c("obj1", "obj2"), fill = FALSE)
#'
#' (d) To get specific field values in a specific class, and issue an error if
#'     some objects do not have that field:
#'
#' t_value_data(dt_idd, dt_idf, class = "XXX", field = "Name", fill = FALSE)
#'
#' (e) To get specific field values in a specifc class, and automatically add
#'     those missing fields (temporary, not affacting the actual IDF data)
#'
#' t_value_data(dt_idd, dt_idf, class = "XXX", field = "Name", fill = TRUE)
#'
#' (f) `field` can also be a data.table that contains at least columns
#'     `field_id` or `field_name`.

t_value_data <- function (dt_value, class = NULL, object = NULL, field = NULL,
                          cols = NULL, ignore_case = FALSE, underscore = FALSE) {
    t_object_data(dt_value, class, object, cols, ignore_case, for_value = TRUE)
}
# }}}
# t_value_all_node {{{
t_value_all_node <- function (dt_value) {
    dt_value[type_enum == .globals$type$node & !is.na(value), unique(value)]
}
# }}}
# t_value_fill_attr {{{
t_value_fill_attr <- function (dt_idd, dt_value, attr_cols = NULL) {
    fld <- t_field_data(dt_idd, dt_value, dt_value$field_index)

    if (is.null(attr_cols)) attr_cols <- names(fld)
    # combine field attributes
    fld[dt_value, on = c("rleid", "field_index")][,
        .SD, .SDcols = unique(c(names(dt_value), attr_cols))]
}
# }}}

# EXTENSIBLE GROUP
# t_add_extensible_group {{{
t_add_extensible_group <- function (dt_idd, class, num = NULL, strict = FALSE) {
    dt_cls <- get_input_class_data(dt_idd$class, class, num)

    # stop if non-extensible class found
    if (strict && nrow(dt_cls[num_extensible == 0L])) {
        stop("Non-extensible class found: ",
            collapse(dt_cls[num_extensible == 0L, unique(class_name)]),
            call. = FALSE
        )
    }

    ext <- dt_cls[num_extensible > 0L & num > 0L]

    if (!nrow(ext)) return(dt_idd)

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
    ext_fld <- dt_idd$field[ext, on = c("class_id", "extensible_group")]
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
    new_ext[!is.na(ip_units), `:=`(full_name = paste0(field_name, " {", ip_units, "}"))]
    new_ext[field_index > last_required, `:=`(required_field = FALSE)]

    # get new field id
    new_fld_id <- t_new_id(dt_idd$field, "field_id", nrow(new_ext))

    # assign new field id
    set(new_ext, NULL, "field_id", new_fld_id)

    # get field reference data
    ref <- dt_idd$reference[
        J(new_ext$field_id), on = "field_id"][
        , `:=`(field_id = new_fld_id)][
        !is.na(src_field_id)]
    src <- dt_idd$reference[J(new_ext$field_id), on = "src_field_id"][
        , `:=`(src_field_id = new_fld_id)][
        !is.na(field_id)]
    new_ref <- rbindlist(list(ref, src))

    # combine into the main field table and name table
    dt_idd$field <- ins_dt(dt_idd$field, new_ext)
    dt_idd$reference <- ins_dt(dt_idd$reference, new_ref)

    # update class data
    dt_idd$class <- copy(dt_idd$class)[ext, on = "class_id",
        c("num_fields", "num_extensible_group") := {
            num_fields = num_fields + ext$num * ext$num_extensible
            num_extensible_group = num_extensible_group + ext$num
            list(num_fields, num_extensible_group)
        }
    ]

    dt_idd
}
# }}}
# t_del_extensible_group {{{
t_del_extensible_group <- function (dt_idd, class, num = NULL, strict = FALSE) {
    dt_cls <- get_input_class_data(dt_idd$class, class, num)

    # stop if non-extensible class found
    if (strict && nrow(dt_cls[num_extensible == 0L])) {
        stop("Non-extensible class found: ",
            collapse(dt_cls[num_extensible == 0L, unique(class_name)]),
            call. = FALSE
        )
    }

    ext <- dt_cls[num_extensible > 0L]

    if (!nrow(ext)) return(dt_idd)

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
    fld_id_del <- dt_idd$field[ext, on = list(class_id, extensible_group > left_group), list(field_id)]

    dt_idd$field <- dt_idd$field[!fld_id_del, on = "field_id"]

    dt_idd$reference <- dt_idd$reference[
        !fld_id_del, on = "field_id"][
        !fld_id_del, on = c(src_field_id = "field_id")
    ]

    dt_idd$class <- copy(dt_idd$class)[ext, on = "class_id", `:=`(
        num_extensible_group = ext$left_group,
        num_fields = ext$left_fields
    )]

    dt_idd
}
# }}}

# ASSERT
# t_assert_can_do {{{
t_assert_can_do <- function (dt_class, dt_object, dot, object,
                             action = c("add", "dup", "set", "del")) {
    find_dot <- function (dot, dt) dot[dt, on = "rleid", mult = "first"]

    # stop attempting to touch Version {{{
    if (nrow(object[class_name == "Version"])) {
        invld <- find_dot(dot, object[class_name == "Version"])

        m <- paste0(dot_string(invld, NULL), " | Class ", surround(invld$class_name), collpase = "\n")

        abort("error_add_version",
            paste0(
                switch(action, add = "Adding", dup = "Duplicating", set = "Modifying", del = "Deleting"),
                " `Version` object is prohibited. Invalid input:\n", m
            ),
            dot = dot, object = object
        )
    }
    # }}}

    if (level_checks()$unique_object && action %in% c("add", "dup", "del")) {
        uni <- object[class_id %in% t_class_id_unique(dt_class)]
        if (nrow(uni)) {
            # try to add or dup new unique object that already exists {{{
            if (action %in% c("add", "dup") && nrow(uni[t_object_num(dt_object, class_id) > 0L])) {
                invld <- find_dot(dot, uni[t_object_num(dt_object, class_id) > 0L])

                info <- t_object_info(invld, collapse = NULL)

                m <- paste0(dot_string(invld, NULL), " | ", info, collapse = "\n")

                abort(paste0("error_", action, "_unique"),
                    paste0(
                        switch(action,
                            add = "Adding new one to unique class that already has one object is prohibited.",
                            dup = "Existing object in unique class cannot be duplicated."
                        ),
                        " Invalid input:\n", m
                    ),
                    dot = dot, object = object
                )
            }
            # }}}
            # try to add multi objects in unique classes {{{
            if (action == "add" && nrow(uni[duplicated(class_id)])) {
                invld <- find_dot(dot, uni[duplicated(class_id)])

                m <- paste0(dot_string(invld, NULL), " | Class: ", surround(invld$class_name), collapse = "\n")

                abort("error_add_multi_unique",
                    paste0("Unique object can only be added once. Invalid input\n", m),
                    dot = dot, object = object
                )
            }
            # }}}
            # try do del unique object {{{
            if (action == "del" && nrow(uni[t_object_num(dt_object, class_id) == 0L])) {

                invld <- find_dot(dot, uni[t_object_num(dt_object, class_id) == 0L])

                info <- t_object_info(invld, collapse = NULL)

                m <- paste0(dot_string(invld, NULL), " | ", info, collapse = "\n")
            }
            # }}}
        }
    }

    # stop attempting to delete required objects {{{
    if (action == "del" && level_checks()$required_object && nrow(object[class_id %in% t_class_id_required(dt_class)])) {
        invld <- find_dot(dot, object[class_id %in% t_class_id_required(dt_class)])

        info <- t_object_info(invld, collapse = NULL)

        m <- paste0(dot_string(invld, NULL), " | ", info, collapse = "\n")

        abort("error_del_required",
            paste0("Deleting a required object is prohibited. Invalid input:\n", m),
            dot = dot, object = object
        )
    }
    # }}}

    # stop if modifying same object multiple times {{{
    if (action %in% c("set", "del") && anyDuplicated(object, by = "object_id")) {
        invld <- find_dot(dot, object[duplicated(class_id)])
        info <- t_object_info(invld, numbered = FALSE)
        m <- paste0(dot_string(invld, NULL), " | ", info, collapse = "\n")

        abort(paste0("error_", action, "_multi_time"),
            paste0("Cannot modify same object multiple times. Invalid input:\n", m),
            dot = dot, object = object
        )
    }
    # }}}

    TRUE
}
# }}}
# t_assert_valid {{{
t_assert_valid <- function (dt_idd, dt_idf, object, value, action = c("add", "set")) {
    action <- match.arg(action)
    # validate fields that do not use default values and all extensible fields
    val_chk <- value[required_field == TRUE | defaulted == FALSE | extensible_group > 0L]
    validity <- validate_on_level(dt_idd, dt_idf,
        copy(object)[, object_id := -rleid],
        val_chk[, object_id := -rleid],
        level = eplusr_option("validate_level")
    )

    if (count_check_error(validity)) {
        on.exit(options(warning.length = getOption("warning.length")), add = TRUE)
        options(warning.length = 8170)
        m <- paste0(capture.output(print_validity(validity)), collapse = "\n")
        abort("error_validity",  paste0("Failed to ", action, " object(s).\n\n", m))
    }

    TRUE
}
# }}}

# DOTS
# dot_string {{{
dot_string <- function (dt, collapse = "\n") {
    dt[, paste0(" #", lpad(rleid, "0"), "| ", dot, collapse = collapse)]
}
# }}}
# old_input {{{
old_input <- function (which, value = NULL, comment = NULL, type = c("add", "set")) {
    assert_valid_input_format(class, value, comment, default, type)

    input <- rep(list(list()), length(class))

    if (!is.null(value)) {
        if (is_scalar(class)) {
            input <- list(value)
        } else {
            null <- vapply(value, is.null, logical(1L))
            input[null] <- rep(list(list()), sum(null))
            input[!null] <- value[!null]
        }
    }

    setattr(input, "names", class)
}
# }}}
# sep_name_dots {{{
sep_name_dots <- function (..., .can_name = TRUE) {
    l <- list(...)

    # stop if empty input
    if (!length(l)) {
        abort("error_empty_input", "Please give object(s) to modify.")
    }

    # check depth of each element
    dep <- vapply(l, vec_depth, integer(1L))

    # check type of each element
    get_type <- function (x) {
        if (is.null(x)) {
            4L
        } else if (are_count(x)) {
            1L
        } else if (is.character(x)) {
            2L
        } else {
            3L
        }
    }
    type <- vapply(l, get_type, integer(1L))

    # stop if invalid depth or type found
    if (any(dep != 1L) || any(type > 2L)) {
        abort("error_wrong_type", "Each element must be a character vector or a positive integer vector.")
    }

    # put all data into a data.table
    dt_in <- data.table(rleid = seq_along(l), dot = l, dot_nm = names2(l), type = type)
    dt_in[, `:=`(nmd = is_named(dot[[1L]])), by = rleid]

    # warning if mix named
    row_mixnm <- dt_in[!is.na(dot_nm) & nmd == TRUE, which = TRUE]
    if (.can_name && length(row_mixnm)) {
        mes <- paste0(
            "Named vectors found in named input element. ",
            "Names of vectors will be used instead of element's name."
        )
        warn("warning_nest_named", mes)
        # remove names of elements that contain named vectors
        dt_in[row_mixnm, `:=`(dot_nm = NA_character_)]
    }

    # separate ids and names
    id <- dt_in[type == 1L]
    nm <- dt_in[type == 2L]

    unnest_dot <- function (rleid, dot, dot_nm, nmd) {
        # length
        l <- vapply(dot, length, integer(1L))

        # id with new names
        id <- unlist(dot)

        new_name <- rep(dot_nm, l)
        nmd <- rep(nmd, l)
        new_name[nmd] <- names2(id)[nmd]

        list(rleid = rep(rleid, l), id = unname(id), new_name = new_name)
    }

    id <- id[, unnest_dot(rleid, dot, dot_nm, nmd)]
    nm <- nm[, unnest_dot(rleid, dot, dot_nm, nmd)]

    list(id = id, name = nm, dot = dt_in)
}
# }}}
# sep_value_dots {{{
sep_value_dots <- function (..., .empty = !in_final_mode(), .duplicate = TRUE) {
    l <- list(...)

    # stop if empty input
    if (!length(l)) {
        abort("error_empty_input", "Please give object(s) to add or set")
    }

    dt_dot <- data.table(rleid = seq_along(l),
        dep = vapply(l, vec_depth, integer(1L)),
        dot = l, dot_nm = names2(l)
    )

    id_invalid <- integer(0L)

    is_single_value <- function (x) {
        is.null(x) ||
        ((is.character(x) || is.numeric(x)) && is_scalar(x) && !is.na(x))
    }

    empty_input <- data.table(rleid = integer(0), class_name = character(0),
        field_name = character(0), value_list = list(), empty = logical(0)
    )

    is_null_list <- function (x) is.null(unlist(x, use.names = FALSE))

    # flatten_input {{{
    flatten_input <- function (dt) {
        if (!nrow(dt)) return(empty_input)

        dep <- unique(dt$dep)

        # empty object, e.g. Construction = list(), Construction = list(NULL)
        if (dep == 1L) {
            id_nolist <- dt[!vapply(dot, is.list, logical(1L)) | is.na(dot_nm), rleid]
            id_invalid <<- c(id_invalid, id_nolist)

            dt <- dt[!rleid %in% id_nolist, {
                list(rleid = rleid,
                     class_name = dot_nm,
                     field_name = rep(NA_character_, .N),
                     value_list = rep(list(NULL), .N),
                     empty = TRUE
                )
            }]
        } else if (dep == 2L) {
            # for empty objects:
            # list(list()) # this is invalid
            # list(Construction = list())
            # list(Construction = list(NULL, NULL))
            id_nonm <- dt[is.na(dot_nm), unique(rleid)]
            id_empty <- dt[rleid %in% id_nonm][
                vapply(dot, is_named, logical(1L)) &
                vapply(dot, is_null_list, logical(1)),
                unique(rleid)]

            id_invalid <<- c(id_invalid, setdiff(id_nonm, id_empty))

            # for empty object
            dt_empty <- dt[rleid %in% id_empty, {
                list(rleid = rleid,
                     class_name = names(unlist(dot, recursive = FALSE)),
                     field_name = rep(NA_character_, .N),
                     value_list = rep(list(NULL), .N),
                     empty = TRUE
                )
            }]

            # for normal objects
            dt_norm <- dt[!rleid %in% id_nonm, {
                len <- each_length(dot)
                value_list <- unlist(dot, recursive = FALSE)
                list(rleid = rep(rleid, len),
                     class_name = rep(dot_nm, len),
                     field_name = names2(value_list),
                     value_list = value_list,
                     empty = FALSE
                )
            }]

            # only NULL, single number and character is acceptable
            id_type <- dt_norm[
                (is.na(field_name) | field_name != ".comment") &
                !vapply(value_list, is_single_value, logical(1L)),
                unique(rleid)
            ]
            id_invalid <<- c(id_invalid, id_type)
            dt_norm <- dt_norm[!rleid %in% id_type]

            dt <- rbindlist(list(dt_empty, dt_norm))
        } else if (dep == 3L){
            id_nm <- dt[!is.na(dot_nm), rleid]
            id_invalid <<- c(id_invalid, id_nm)

            dt <- dt[!rleid %in% id_nm, {
                len <- each_length(dot)
                dot <- unlist(dot, recursive = FALSE)
                list(rleid = rep(rleid, len),
                     dep = rep(dep - 1L, len),
                     dot = dot,
                     dot_nm = names2(dot)
                )
            }]
            dt <- flatten_input(dt)
        } else {
            id_invalid <<- c(id_invalid, dt[!is.na(dot_nm), rleid])
            dt <- empty_input
        }

        dt
    }
    # }}}

    flat <- rbindlist(lapply(split(dt_dot, by = "dep"), flatten_input))

    # stop if invalid input format {{{
    if (length(id_invalid) || !nrow(flat)) {
        abort("error_invalid_value_dot",
            paste0(
                "Each object must be an empty list or a list where ",
                "each element being a non-NA single string or number. Invalid input:\n",
                dot_string(dt_dot[rleid %in% id_invalid])
            )
        )
    }
    # }}}

    # stop if empty objects {{{
    if (!.empty && nrow(flat[empty == TRUE])) {
        abort("error_empty_object",
            paste0(
                "Empty input found. Please give field values to add or set. Invalid input:\n",
                dot_string(dt_dot[rleid %in% flat[empty == TRUE, rleid]])
            )
        )
    }
    # }}}

    # stop if duplicated object names {{{
    if (!.duplicate && anyDuplicated(unique(flat, by = "rleid"), by = "class_name")) {
        dup <- flat[duplicated(unique(flat, by = "rleid"), by = "class_name"), class_name]
        invld <- flat[J(dup), on = "class_name", unique(rleid)]
        abort("error_dup_name",
            paste0(
                "Input object name or ID cannot have any duplication. Invalid input:\n",
                dot_string(dt_dot[rleid %in% invld])
            )
        )
    }
    # }}}

    # stop if mix named {{{
    mix_nmd <- flat[is.na(field_name), list(named = .N), by = list(rleid)][
        flat[is.na(field_name) | field_name != ".comment", list(total = .N), by = list(rleid)], on = "rleid"]
    if (nrow(mix_nmd[named != 0 & named != total])) {
        abort("error_mix_named",
            paste0(
                "Elements in each object must be all named or all unnamed. Invalid input:\n",
                dot_string(dt_dot[rleid %in% mix_nmd[named != 0 & named != total, rleid]])
            )
        )
    }
    # }}}

    # stop if multiple .comment {{{
    if (nrow(flat[field_name == ".comment", list(num_cmt = .N), by = list(rleid)][num_cmt > 1L])) {
        id_m_cmt <- flat[field_name == ".comment", list(num_cmt = .N), by = list(rleid)][num_cmt > 1L, rleid]
        abort("error_multiple_comment",
            paste0(
                "Each object can only have one `.comment` element. Invalid input:\n",
                dot_string(dt_dot[rleid %in% id_m_cmt])
            )
        )
    }
    # }}}

    # stop if duplicated field names (in underscore style) {{{
    if (any(flat[!is.na(field_name), list(anyDuplicated(underscore_name(field_name)) > 0L), by = list(rleid)]$V1)) {
        id_dup_nm <- flat[!is.na(field_name),
            list(anyDuplicated(underscore_name(field_name)) > 0L),
            by = list(rleid)][
            V1 == TRUE, rleid]

        abort("error_dup_field_name",
            paste0(
                "Field names must be unique. Invalid input:\n",
                dot_string(dt_dot[rleid %in% id_dup_nm])
            )
        )
    }
    # }}}

    # stop if value starts with "!" {{{
    if (nrow(flat[vapply(value_list, function (x) is.character(x) && startsWith(x, "!"), logical(1L))])) {
        id_exc <- flat[vapply(value_list, function (x) is.character(x) && startsWith(x, "!"), logical(1L)), unique(rleid)]
        abort("error_exclamation_value",
            paste0(
                "Field value cannot starts with `!`. Invalid input:\n",
                dot_string(dt_dot[rleid %in% id_exc])
            )
        )
    }
    # }}}

    list(value = flat, dot = dt_dot)
}
# }}}
# object_from_l{{{
object_from_l <- function (dt_idd, dt_idf, l, keep_duplicate = TRUE) {
    setnames(l$id, "id", "object_id")
    setnames(l$name, "id", "object_name")
    # match
    if (nrow(l$id)) {
        obj_id <- t_object_data(dt_idf$object, object = l$id)
    } else {
        obj_id <- dt_idf$object[0L]
        set(obj_id, NULL, c("rleid", "new_name"), list(integer(0L), character(0L)))
    }
    if (nrow(l$name)) {
        obj_nm <- t_object_data(dt_idf$object, object = l$name)
    } else {
        obj_nm <- dt_idf$object[0L]
        set(obj_nm, NULL, c("rleid", "new_name"), list(integer(0L), character(0L)))
    }

    # remain the input order
    obj <- rbindlist(list(obj_id, obj_nm))
    setorderv(obj, "rleid")
    # in case when there are input object id or name vectors
    add_rleid(obj, "object")

    if (keep_duplicate) return(obj)

    unique(obj, by = "object_id")
}
# }}}

# OBJECT MUNIPULATION
# t_duplicate_object {{{
t_duplicate_object = function (dt_idd, dt_idf, ...) {
    l <- sep_name_dots(..., .can_name = TRUE)
    obj <- object_from_l(dt_idd, dt_idf, l, keep_duplicate = TRUE)

    # add has_name attribute
    obj <- dt_idd$class[, list(class_id, has_name)][obj, on = "class_id"]

    # stop if cannot add objects in specified classes
    t_assert_can_do(dt_idd$class, dt_idf$object, l$dot, obj, "dup")

    # check input new names {{{
    # stop if trying to assign names to objects that do not have name attribute
    if (nrow(obj[has_name == FALSE & !is.na(new_name)])) {
        cannot_name <- unique(obj[has_name == FALSE & !is.na(new_name)], by = "object_id")
        mes <- paste0("Target object(s) in class that does not have name attribute ",
            "cannot be renamed. Invalid input:\n",
            t_object_info(cannot_name, collapse = "\n")
        )
        abort(c("error_cannot_rename", "error_dup_object"), mes, data = cannot_name)
    }

    # get value data
    val <- t_value_data(dt_idf$value, object = obj[, list(rleid, object_rleid, object_id)])
    # NOTE:
    # (a) restore old value id for updating reference
    # (b) Assign new value id in order to correctly print validate message
    setnames(val, "value_id", "old_value_id")
    set(val, NULL, "value_id", t_new_id(dt_idf$value, "value_id", nrow(val)))

    # NOTE:
    # (a) Store old id and name for logging
    # (b) Change object names for validation
    setnames(obj, c("object_id", "object_name", "object_name_lower"),
        c("old_object_id", "old_object_name", "old_object_name_lower")
    )
    set(obj, NULL, c("object_name", "object_name_lower"),
        list(obj$new_name, stri_trans_tolower(obj$new_name)))

    validity <- validate_objects(dt_idd, dt_idf,
        copy(obj)[, object_id := paste0("Input #", rleid)],
        copy(val)[, object_id := paste0("Input #", rleid)],
        unique_name = TRUE
    )

    if (count_check_error(validity)) {
        print_validity(validity)
        abort("error_validity", paste0(
            "Failed to duplicate object(s). ",
            "Input new name(s) cannot be the same as target object(s) or ",
            "any existing object in the same class."
        ))
    }
    # }}}

    # assign new object id after validation
    obj <- assign_new_id(dt_idf, obj, "object")
    val <- correct_obj_id(obj, val)

    # get new object name {{{
    # get indicator of whether user input new names are used
    set(obj, NULL, "use_input_name", FALSE)
    obj[has_name == TRUE & !is.na(object_name_lower) &
        (object_name_lower != old_object_name_lower | is.na(old_object_name_lower)),
        `:=`(use_input_name = TRUE)
    ]

    # get all name in the same class
    obj[, `:=`(all_name_lower = t_object_name(dt_idf$object, class_name, lower = TRUE)), by = c("rleid")]

    # check if trying to duplicate same object several times
    set(obj, NULL, "dup_time", 0L)
    obj[use_input_name == FALSE, `:=`(dup_time = seq_along(object_id)),
        by = list(class_id, old_object_name_lower)]

    # get the duplicated times before
    obj[!is.na(old_object_name), `:=`(
        max_suffix_num = apply2_int(all_name_lower, old_object_name_lower,
            function (x, y) {
                num <- stri_match_first_regex(x, paste0("^", y, "_(\\d+)$"))[,2L]
                num[is.na(num)] <- "0"
                max(as.integer(num))
            }
        )
    )]

    # assign new object name
    set(obj, NULL, "auto_assigned", FALSE)
    obj[!is.na(old_object_name) & use_input_name == FALSE,
        `:=`(object_name = paste0(old_object_name, "_", max_suffix_num + dup_time),
             auto_assigned = TRUE
        )
    ]
    set(obj, NULL, "object_name_lower", stri_trans_tolower(obj$object_name))

    # logging
    if (nrow(obj[auto_assigned == TRUE])) {
        auto <- obj[auto_assigned == TRUE]
        id <- rpad(t_object_info(auto, "id"))
        name <- t_object_info(auto, "name", prefix = " --> New ", numbered = FALSE)
        verbose_info(
            "New names of duplicated objects not given are automatically generated:\n",
            paste0(id, name, collapse = "\n")
        )
    }
    # }}}

    # assign name field
    val[is_name == TRUE, `:=`(value = obj[has_name == TRUE, object_name])]

    # value reference
    ref <- dt_idf$reference[J(val$old_value_id)]
    set(ref, NULL, "value_id", val$value_id)

    list(object = obj[, .SD, .SDcols = names(dt_idf$object)],
         value = val[, .SD, .SDcols = names(dt_idf$value)],
         reference = ins_dt(dt_idf$reference, ref)
    )
}
# }}}
# t_add_object {{{
t_add_object <- function (dt_idd, dt_idf, ..., .default = TRUE, .all = FALSE) {
    l <- sep_value_dots(..., .empty = TRUE)

    obj <- l$value[J(unique(l$value$rleid)), on = "rleid", mult = "first"]

    # new object table
    obj <- t_class_data(dt_idd$class, obj,
        cols = c("rleid", "class_id", "class_name", "group_id"),
        underscore = TRUE
    )

    # stop if cannot add objects in specified classes
    t_assert_can_do(dt_idd$class, dt_idf$object, l$dot, obj, "add")

    # add object id
    obj <- assign_new_id(dt_idf, obj, "object")

    # get object comments
    obj <- l$value[field_name == ".comment", list(rleid, comment = value_list)][obj, on = "rleid"]

    # new value table
    val <- l$value[is.na(field_name) | field_name != ".comment",
        .SD, .SDcols = c("rleid", "field_name", "value_list", "empty")][
        obj[, .SD, .SDcols = c("rleid", "object_id", "class_id", "class_name")],
        on = "rleid"]

    # get empty objects {{{
    val_empty <- val[empty == TRUE]
    val_empty <- t_field_data(dt_idd, val_empty, all = .all)

    # add value and value in number
    set(val_empty, NULL, c("value", "value_num"), list(NA_character_, NA_real_))
    # }}}

    # get non-empty objects {{{
    val_nonempty <- val[empty == FALSE, -"empty"]

    setindexv(val_nonempty, "field_name")

    # get field names and attributes {{{
    val_unnm <- val_nonempty[is.na(field_name)]
    fld_unnm <- t_field_data(dt_idd, val_unnm, rowidv(val_unnm, "rleid"), all = .all, complete = TRUE)

    val_nm <- val_nonempty[!is.na(field_name)]
    fld_nm <- t_field_data(dt_idd, val_nm, val_nm$field_name, all = .all, complete = TRUE)

    # combine field attributes
    set(val_unnm, NULL, setdiff(names(val_unnm), "value_list"), NULL)
    set(val_nm, NULL, setdiff(names(val_nm), "value_list"), NULL)

    add_rleid(val_unnm, "matched")
    add_rleid(val_nm, "matched")

    val_matched <- rbindlist(list(
        val_unnm[fld_unnm, on = c("matched_rleid")],
        val_nm[fld_nm, on = c("matched_rleid")]
    ))
    set(val_matched, NULL, "matched_rleid", NULL)
    # }}}

    # find fields to be filled with default values
    set(val_empty, NULL, "defaulted", TRUE)
    set(val_matched, NULL, "defaulted", vapply(val_matched$value_list, is.null, logical(1L)))

    # add value and value in number
    val_matched[defaulted == FALSE, `:=`(value = as.character(value_list))]
    val_matched[vapply(value_list, is.numeric, logical(1L)), `:=`(value_num = as.double(value_list))]
    set(val_matched, NULL, "value_list", NULL)
    # }}}

    # combine empty and non-empty objects
    val <- rbindlist(list(val_empty, val_matched), use.names = TRUE)

    # assign default values if needed
    if (.default) val <- assign_default_value(val)

    # assign new value id
    val <- assign_new_id(dt_idf, val, "value")

    # update object name
    obj <- update_object_name(obj, val)
    # add lower name
    set(obj, NULL, "object_name_lower", stri_trans_tolower(obj$object_name))

    # validate
    t_assert_valid(dt_idd, dt_idf, obj, val, action = "add")

    # get field reference
    # include new values in order to find references across input new objects
    ref <- get_value_reference_map(dt_idd$reference,
        ins_dt(dt_idf$value, val, "value_id"),
        ins_dt(dt_idf$value, val, "value_id")
    )

    list(object = obj[, .SD, .SDcols = names(dt_idf$object)],
         value = val[, .SD, .SDcols = names(dt_idf$value)],
         reference = ref
    )
}
# }}}
# t_set_object {{{
t_set_object <- function (dt_idd, dt_idf, ..., .default = TRUE) {
    l <- sep_value_dots(..., .empty = TRUE, .duplicate = FALSE)

    # get object ID in `..X` format
    setnames(l$value, "class_name", "object_name")
    set(l$value, NULL, "object_id", as.integer(stri_match_first_regex(l$value$object_name, "^\\.\\.(\\d+)$")[, 2L]))

    obj <- l$value[J(unique(l$value$rleid)), on = "rleid", mult = "first"]

    # separate
    obj_id <- obj[!is.na(object_id)]
    set(obj_id, NULL, "object_name", NULL)
    obj_nm <- obj[is.na(object_id)]
    set(obj_nm, NULL, "object_id", NULL)

    # get object data
    obj_id <- t_object_data(dt_idf$object, object = obj_id)
    obj_nm <- t_object_data(dt_idf$object, object = obj_nm)

    # combine
    obj <- rbindlist(list(obj_id, obj_nm))

    # stop if cannot set objects
    t_assert_can_do(dt_idd$class, dt_idf$object, l$dot, obj, "set")

    # replace old comments with input comments
    cmt <- l$value[field_name == ".comment", list(rleid, comment = value_list)]
    if (nrow(cmt) == 1L) {
        obj[cmt, on = "rleid", comment := list(cmt$comment)]
    } else if (nrow(cmt) >= 2L) {
        obj[cmt, on = "rleid", comment := cmt$comment]
    }

    # new value table
    val <- l$value[is.na(field_name) | field_name != ".comment", list(rleid, field_name, value_list, empty)][
        obj[, .SD, .SDcols = c("rleid", "object_id", "class_id", "class_name")], on = "rleid"]

    # get objects with empty input {{{
    val_empty <- val[empty == TRUE]
    # only include empty fields
    val_empty <- t_value_data_in_object(dt_idd, dt_idf, object = val_empty)[is.na(value)]
    set(val_empty, NULL, "value_list", NULL)
    # }}}

    # get non-empty objects {{{
    val_nonempty <- val[empty == FALSE]
    val_nonempty <- t_value_data_in_object(dt_idd, dt_idf, object = val_nonempty,
        field = val_nonempty$field_name, underscore = TRUE)
    # mark empty fields
    set(val_nonempty, NULL, "empty", vapply(val_nonempty$value_list, is.null, logical(1L)))

    # set new values
    val_nonempty[empty == FALSE, value := as.character(value_list)]
    val_nonempty[empty == FALSE & vapply(value_list, is.numeric, logical(1L)),
        value_num := as.double(value_list)]
    set(val_nonempty, NULL, "value_list", NULL)
    # }}}

    val <- rbindlist(list(val_empty, val_nonempty), use.names = TRUE)

    # get field names and attributes
    val <- t_value_fill_attr(dt_idd, val)

    # assign default values if needed
    set(val, NULL, "defaulted", FALSE)
    val[is.na(value) & empty == TRUE, defaulted := TRUE]
    if (.default) {
        val <- assign_default_value(val)
    }

    # assign new value id to new fields
    val <- assign_new_id(dt_idf, val, "value", keep = TRUE)

    # update object name
    obj <- update_object_name(obj, val)

    # add lower name
    set(obj, NULL, "object_name_lower", stri_trans_tolower(obj$object_name))

    # validate
    t_assert_valid(dt_idd, dt_idf, obj, val, action = "set")

    # update referenced value in other objects if necessary
    new_val <- dt_idf$reference[src_enum == .globals$source$field][
        val[, list(value_id, value, value_num)],
        on = c(src_value_id = "value_id")]
    dt_idf$value[new_val, on = "value_id",
        `:=`(value = new_val$value, value_num = new_val$value_num)]

    # get field reference
    # NOTE: it may be more efficient to exclude fields whose references have
    # already been update in above
    ref <- get_value_reference_map(dt_idd$reference,
        ins_dt(dt_idf$value, val, "value_id"),
        ins_dt(dt_idf$value, val, "value_id")
    )

    list(object = obj[, .SD, .SDcols = names(dt_idf$object)],
         value = val[, .SD, .SDcols = names(dt_idf$value)],
         reference = ref
    )
}
# }}}
# t_delete_object {{{
t_delete_object <- function (dt_idd, dt_idf, ..., .referenced = FALSE) {
    l <- sep_name_dots(..., .can_name = TRUE)
    obj <- object_from_l(dt_idd, dt_idf, l, keep_duplicate = TRUE)

    t_assert_can_do(dt_idd$class, dt_idf$object, l$dot, obj, "del")

    ref <- dt_idf$reference[obj, on = c(src_object_id = "object_id"), nomatch = 0L]
    setnames(ref,
        c("object_id", "value_id", "src_object_id", "src_value_id"),
        c("ref_object_id", "ref_value_id", "object_id", "value_id")
    )

    # IDs of objects to be deleted
    id_del <- obj$object_id

    # check if target objects are referenced {{{
    if (nrow(ref)) {
        # TODO: what if invalid reference exists?
        ref_by <- dt_idf$object[, list(object_id, object_name)][
            dt_idf$value[ref[, list(src_object_id = object_id, src_value_id = value_id, value_id = ref_value_id)], on = "value_id"],
            on = "object_id"
        ]

        if (level_checks()$reference) {
            # dot message
            m_dot <- dot_string(l$dot[unique(ref, by = "object_id"), on = "rleid"], collapse = NULL)

            # src message
            m_ref <- t_object_info(unique(ref, by = "object_id"), numbered = FALSE)

            # ref message
            set(ref, NULL, "mes", t_object_info(ref_by, c("id", "name"), numbered = FALSE, prefix = ""))
            m_ref_by <- ref[, collapse(mes, NULL), by = "object_id"]$V1

            m <- paste0(m_dot, " | ", m_ref, " is referenced by ", m_ref_by, collapse = "\n")
            abort("error_del_referenced",
                paste0(
                    "Deleting an object referenced by others is prohibited. Invalid input:\n", m
                ),
                src = ref,
                ref = ref_by
            )
        }

        if (referenced) {
            verbose_info(
                "Delete object(s) ID [", collapse(id_del, NULL), "]",
                " and also object(s) ID [", collapse(unique(ref_by$object_id), NULL), "]",
                " that are referencing target object(s)."
            )
            id_del <- unique(c(id_del, ref_by$object_id))
        } else {
            verbose_info(
                "Delete object(s) ID [", collapse(id_del, NULL), "]",
                " which were referenced by object(s) ID [", collapse(unique(ref_by$object_id), NULL), "].",
                " Invalid reference(s) may cause simulation errors."
            )
        }
    }
    # }}}

    # delete rows in object table
    dt_object <- dt_idf$object[!J(id_del), on = "object_id"]

    # delete rows in value table and value reference table
    dt_value <- dt_idf$value[!J(id_del), on = "object_id"]

    # delete rows in value reference table
    dt_reference <- dt_idf$reference[!J(id_del), on = "object_id"][!J(id_del), on = "src_object_id"]

    list(object = dt_object[, .SD, .SDcols = names(dt_idf$object)],
         value = dt_value[, .SD, .SDcols = names(dt_idf$value)],
         reference = dt_reference
    )
}
# }}}
# t_paste_object {{{
t_paste_object <- function (dt_idd, dt_idf, ver, in_ip = FALSE, unique = TRUE) {
    parsed <- read_idfeditor_copy(ver, in_ip)

    # IP - SI conversion if necessary
    from <- if(in_ip) "ip" else "si"
    to <- if(.options$view_in_ip) "ip" else "si"
    parsed$value <- convert_value_unit(parsed$value, from, to)

    # remove version object
    obj_ver <- parsed$object[class_name == "Version", object_id]
    parsed$object <- parsed$object[!J(obj_ver), on = "object_id"]
    parsed$value <- parsed$value[!J(obj_ver), on = "object_id"]

    if (unique) {
        # get all class ids in input
        cls <- parsed$object[class_name != "Version", unique(class_id)]

        # NOTE: by default, IDF Editor only copies objects in a single class, which
        # makes it easy to remove duplicates
        stopifnot(is_scalar(cls))

        cols <- c("object_id", "class_id", "field_index", "value")

        # extract all object values in the same class
        # in order to distinguish input from original IDF, set id of objects
        # from IDF to negative
        # also note that dcast will automatically order object id, so this makes
        # that input objects are always in the bottom.
        val_idf <- dt_idf$value[J(cls), on = "class_id",
            list(object_id = -object_id, field_index, value)]

        # get all input value
        val_in <- parsed$value[, list(object_id, field_index, value)]

        # dcast to compare
        val_d <- dcast(rbindlist(list(val_idf, val_in), fill = TRUE),
            object_id ~ field_index, value.var = "value")

        # get indicator
        dup <- duplicated(val_d, by = setdiff(names(val_d), c("object_id", "class_id")))

        # only find duplicates in input
        obj <- val_d[dup & object_id > 0L, object_id]

        if (length(obj)) {
            # give info
            verbose_info(
                "Duplicated objects in input or objects in input that are the same in current IDF have been removed:\n",
                {
                    del <- parsed$object[J(obj), on = "object_id"]
                    t_object_info(del, "name", collapse = "\n")
                }
            )
            # NOTE: for references, as have to check gloally to get the latest
            # references, there is no need to exclude objects here
            parsed$object <- parsed$object[!J(obj), on = "object_id"]
            parsed$value <- parsed$value[!J(obj), on = "object_id"]
        }
        browser()
    }

    # add rleid for validation
    add_rleid(parsed$object)
    add_rleid(parsed$object, "object")

    # update id
    parsed$object <- assign_new_id(dt_idf, parsed$object, "object")
    parsed$value <- assign_new_id(dt_idf, parsed$value, "value")
    parsed$value <- correct_obj_id(parsed$object, parsed$value)
    set(parsed$value, NULL, "rleid", parsed$value$object_id)
    set(parsed$value, NULL, "object_rleid", parsed$value$object_id)

    # add field attributes for validation
    parsed$value <- t_value_fill_attr(dt_idd, parsed$value)
    set(parsed$value, NULL, "defaulted", FALSE)

    # validate
    t_assert_valid(dt_idd, dt_idf, parsed$object, parsed$value, action = "add")

    # get field reference in whole fields
    parsed$reference <- get_value_reference_map(dt_idd$reference,
        ins_dt(dt_idf$value, parsed$value, "value_id"),
        ins_dt(dt_idf$value, parsed$value, "value_id")
    )

    parsed
}
# }}}
# t_ins_object {{{
t_ins_object <- function (dt_idd, dt_idf, object) {

}
# }}}
# t_purge_object {{{
t_purge_object <- function (dt_idd, dt_idf, ...) {

}
# }}}

# IDF Editor Integration
# read_idfeditor_copy {{{
read_idfeditor_copy <- function (ver = NULL, in_ip = FALSE) {
    if (Sys.info()["sysname"] == "Darwin") {
        clp <- pipe("pbpaste")
    } else {
        clp <- "clipboard"
    }

    text <- readLines(clp, warn = FALSE)

    if (!startsWith(text, "IDF,")) {
        abort("error_clipboard_string", "Failed to find contents copied from IDF Editor.")
    }
    text <- gsub("([,;])", "\\1\n", stri_sub(text, 5L))

    if (isTRUE(in_ip)) {
        text <- paste0("!-Option SortedOrder ViewInIPunits\n", text)
    }

    # ignore the warning of using given IDD
    withCallingHandlers(read_idf_file(text, idd = ver),
        warn_given_idd_used = function (w) invokeRestart("muffleWarning")
    )
}
# }}}

# LOG
# log_new_uuid {{{
log_new_uuid <- function (log) {
    log$uuid <- unique_id()
}
# }}}
# log_new_order {{{
log_new_order <- function (log, id) {
    log$order <- ins_dt(log$order, data.table(object_id = id, object_order = 1L, "object_id"))
}
# }}}
# log_add_order {{{
log_add_order <- function (log, id) {
    log$order[J(id), on = "object_id", object_order := object_order + 1L]
}
# }}}
# log_del_order {{{
log_del_order <- function (log, id) {
    log$order <- log$order[!J(id), on = "object_id"]
}
# }}}
# log_unsaved {{{
log_unsaved <- function (log) {
    log$unsaved <- TRUE
}
# }}}
# log_saved {{{
log_saved <- function (log) {
    log$unsaved <- FALSE
}
# }}}

# UTILITIES
# in_final_mode {{{
in_final_mode <- function () {
    eplusr_option("validate_level") == "final"
}
# }}}
# in_ip_mode {{{
in_ip_mode <- function () {
    eplusr_option("view_in_ip")
}
# }}}
# verbose_info {{{
verbose_info <- function (...) {
    if (eplusr_option("verbose_info")) {
        cli::cat_rule(crayon::bold("Info"), col = "green")
        cat(crayon::green(paste0(...)), "\n", sep = "")
        cat("\n")
    }
}
# }}}

# abort_bad_key {{{
abort_bad_key <- function (error_type, key, value) {
    mes <- paste0("Invalid ", key, " found: ", collapse(value))
    abort(error_type, mes, value = value)
}
# }}}
# abort_bad_which_type {{{
abort_bad_which_type <- function (error_type, key, ...) {
    mes <- paste0(key, " should be either an integer vector or a character vector", ...)
    abort(error_type, mes)
}
# }}}
# abort_bad_field {{{
abort_bad_field <- function (error_type, key, dt, ...) {
    h <- paste0("Invalid field ", key, " found:\n")

    mes <- switch(key,
        index = errormsg_field_index(dt),
        name = errormsg_field_name(dt)
    )

    abort(error_type, paste0(h, mes, ...), data = dt)
}
# }}}
# errormsg_info {{{
errormsg_info <- function (dt) {
    if (!has_name(dt, "rleid")) add_rleid(dt)
    dt[, `:=`(info = paste0(" #", lpad(rleid), "| Class ", surround(class_name)))]
}
# }}}
# errormsg_field_index {{{
errormsg_field_index <- function (dt) {
    dt <- dt[field_index < min_fields | field_index > num_fields,
        list(field_index = collapse(field_index)),
        by = list(rleid, class_name, min_fields, num_fields)
    ]

    dt <- errormsg_info(dt)

    dt[, msg := paste0(info, ": ", rpad(field_index), ".")]

    dt[min_fields == 0L, msg := paste0(msg,
        " Field index should be no more than ", num_fields, ".")]
    dt[min_fields >  0L, msg := paste0(msg,
        " field index should be no less than ", min_fields,
        " and no more than ", num_fields, ".")]

    paste0(dt$msg, collapse = "\n")
}
# }}}
# errormsg_field_name {{{
errormsg_field_name <- function (dt) {
    dt <- dt[, list(field_name = collapse(field_name)),
        by = list(rleid, class_name)
    ]

    dt <- errormsg_info(dt)

    dt[, msg := paste0(info, ": ", rpad(field_name), ".")]
    paste0(dt$msg, collapse = "\n")
}
# }}}

# t_new_id {{{
t_new_id <- function (dt, name, num) {
    stopifnot(has_name(dt, name))
    max(dt[[name]], na.rm = TRUE) + seq_len(num)
}
# }}}
# add_rleid {{{
add_rleid <- function (dt, prefix = NULL) {
    if (!is.null(prefix)) prefix <- paste0(prefix, "_")
    set(dt, NULL, paste0(prefix, "rleid"), seq_len(nrow(dt)))
}
# }}}
# add_accept_field_num {{{
# Get the acceptable field number
#' @param dt_class A data.table contains class data. Columns below should exist:
#'        `class_id`, `min_fields`, `last_required`, `num_fields`,
#'        `num_extensible`, `first_extensible`.
#' @param num NULL or an positive integer vector indicating how many fields to
#'        check.
#' @param all If `TRUE`, the acceptable number returned will always be the total
#'        number of existing fields in those classes.
#' @param keep_input If `TRUE`, a column named `input_num` containing all input
#'        `num`. If `num` is `NULL`, `input_num` will be the same as the
#'        `num_fields` column in `dt_class`.
#' @details
#' Acceptable number is determined in ways below:
#'     * If `all` is `TRUE`, when `num` is NULL or smaller than the total
#'       existing field number, the accetable field number is the total existing
#'       field number in that class.
#'     * If `all` is `FALSE`:
#'         * If input number is NULL:
#'             - For class with no required fields and no minimum field number
#'               requirement, the acceptable field number is the total existing
#'               field number in that class.
#'             - For class with required fields and minimum field number
#'               requirement, the acceptable number is the bigger one between
#'               required field number and minimum field number.
#'         * If input number is smaller than the total existing field number:
#'             - The acceptable field number is the maximum among the field
#'               index of the last field in the last extensible group (if
#'               applicable), the field index of the last required field and
#'               minimum field number required.
#'         * If input number is larger than the total existing field number:
#'             - The acceptable field number will be the field index of the last
#'               field in the last extensible group.
add_accept_field_num <- function (dt_class, num = NULL, all = FALSE, keep_input = FALSE) {
    stopifnot(has_names(dt_class,
        c("min_fields", "last_required", "num_fields", "num_extensible", "first_extensible"))
    )

    stopifnot(is.null(num) || are_count(num))

    dt_class <- add_rleid(dt_class, "class")

    if (all) {
        if (keep_input) {
            set(dt_class, NULL, "input_num", dt_class$num_fields)
        }
        set(dt_class, NULL, "acceptable_num", dt_class$num_fields)
        return(dt_class)
    }

    if (is.null(num)) {
        if (all) {
            if (keep_input) {
                set(dt_class, NULL, "input_num", dt_class$num_fields)
            }
            set(dt_class, NULL, "acceptable_num", dt_class$num_fields)
            return(dt_class)
        }

        set(dt_class, NULL, "input_num", 0L)
        dt_class[min_fields == 0L & last_required == 0L, input_num := num_fields]
    } else {
        stopifnot(is_same_len(dt_class, num))
        stopifnot(are_count(num))
        set(dt_class, NULL, "input_num", as.integer(num))
        if (all) {
            dt_class[input_num <= num_fields, input_num := num_fields]
        }
    }

    # get index of the last field in the last extensible group
    set(dt_class, NULL, "last_extensible", 0L)
    dt_class[
        num_extensible > 0L & input_num > first_extensible,
        last_extensible := as.integer(
            ceiling((input_num - first_extensible + 1L) / num_extensible)
            * num_extensible + first_extensible - 1L
        ),
        by = list(class_rleid)
    ]

    if (!nrow(dt_class)) {
        set(dt_class, NULL, "acceptable_num", integer(0L))
    } else {
        dt_class[,
            acceptable_num := max(input_num, min_fields, last_required, last_extensible),
            by = c("class_rleid")]
    }

    if (keep_input) {
        set(dt_class, NULL, c("class_rleid", "last_extensible"), NULL)
    } else {
        set(dt_class, NULL, c("class_rleid", "last_extensible", "input_num"), NULL)
    }
    dt_class
}
# }}}
# get_input_class_data {{{
get_input_class_data <- function (dt_class, class, num = NULL) {
    if (is.data.frame(class)) {
        dt_cls <- class

        if (is.null(num)) {
            stopifnot(has_name(dt_cls, "num"))
            set(dt_cls, NULL, "num", as.integer(dt_cls$num))
        } else {
            stopifnot(are_count(num))
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

        # dt_cls <- add_accept_field_num(dt_cls)

        stopifnot(are_count(num))
        stopifnot(is_same_len(class, num))
        set(dt_cls, NULL, "num", as.integer(num))
    }
}
# }}}
# assign_new_id {{{
assign_new_id <- function (dt_idf, dt, type = c("object", "value"), keep = FALSE) {
    type <- match.arg(type)
    col <- paste0(type, "_id")
    if (!keep) {
        set(dt, NULL, col, t_new_id(dt_idf[[type]], col, nrow(dt)))
    } else {
        dt[is.na(get(col)), `:=`(value_id = t_new_id(dt_idf[[type]], col, .N))]
    }
}
# }}}
# correct_obj_id {{{
correct_obj_id <- function (dt_object, dt_value) {
    by_col <- if(has_name(dt_value, "object_rleid")) "object_rleid" else "object_id"
    set(dt_value, NULL, "object_id",
        rep(dt_object$object_id, times = dt_value[, .N, by = c(by_col)]$N))
}
# }}}
# assign_default_value {{{
assign_default_value <- function (dt_value) {
    if (in_ip_mode()) {
        dt_value <- t_field_default_to_unit(dt_value, "si", "ip")
    }
    dt_value[defaulted == TRUE, `:=`(value = as.character(unlist(default, use.names = FALSE)))]
    dt_value[defaulted == TRUE & vapply(default, is.numeric, logical(1L)),
        `:=`(value_num = as.double(unlist(default)))
    ]
    dt_value
}
# }}}
# make_field_range {{{
make_field_range <- function (minimum, lower_incbounds, maximum, upper_incbounds) {
    r <- list(
        minimum = minimum, lower_incbounds = lower_incbounds,
        maximum = maximum, upper_incbounds = upper_incbounds)
    data.table::setattr(r, "class", c("IddFieldRange", "list"))
    r
}
# }}}
# ins_dt {{{
ins_dt <- function (dt, new_dt, base_col = NULL) {
    stopifnot(has_names(new_dt, names(dt)))

    if (is.null(base_col)) {
        rbindlist(
            list(
                dt,
                new_dt[, .SD, .SDcols = names(dt)]
            )
        )
    } else {
        rbindlist(
            list(
                dt[!new_dt, on = base_col],
                new_dt[, .SD, .SDcols = names(dt)]
            )
        )
    }
}
# }}}
# merge_idf_data {{{
merge_idf_data <- function (dt_idf, dt) {
    stopifnot(is.environment(dt_idf))
    stopifnot(has_names(dt, c("object", "value", "reference")))
    dt_idf$object <- ins_dt(dt_idf$object, dt$object, "object_id")
    dt_idf$value <- ins_dt(dt_idf$value, dt$value, "value_id")
    dt_idf$reference <- dt$reference

    dt_idf
}
# }}}
# unique_id {{{
unique_id <- function () {
    paste0("id-", stri_rand_strings(1, 15L))
}
# }}}

# FOR BACK COMPATIBILITY
# SHOULD BE REMOVED IN NEXT RELEASE
# assert_valid_input_format {{{
assert_valid_input_format <- function (class_name, value, comment, default, type = c("add", "set")) {
    type <- match.arg(type)
    key <- switch(type, add = "class", set = "object")

    is_valid_input <- function (x) is.null(x) || is_normal_list(x)

    if (length(class_name) > 1L &&
        ((!is.null(value)   && !is_same_len(class_name, value)) ||
         (!is.null(comment) && !is_same_len(class_name, comment))))
        stop("`value` and `comment` should have the same length as ",
            surround(key), ".", call. = FALSE)

    if (is_scalar(class_name)) {
        if (!is_valid_input(value) || !is_valid_input(comment)) {
            stop("Invalid `value` or `comment` format found. Each value or ",
                "comment for a single object should be NULL or a list in format ",
                "'list(a = 1, b = 2)' or 'list(1, 2)'.", call. = FALSE)
        }
    } else {
        if (!all(vapply(value, is_valid_input, logical(1))) ||
            !all(vapply(comment, is_valid_input, logical(1)))) {
            stop("Invalid `value` or `comment` format found. Each value or ",
                "comment for a single object should be NULL or a list in format ",
                "'list(a = 1, b = 2)' or 'list(1, 2)'.", call. = FALSE)
        }
    }
}
# }}}

################################################################################
#                                  Basic Info                                  #
################################################################################

# i_id {{{
i_id <- function (self, private) {
    private$m_object_id
}
# }}}

# i_version {{{
i_version <- function (self, private) private$m_version
# }}}

# i_build {{{
i_build <- function (self, private) private$m_build
# }}}

# i_path {{{
i_path <- function (self, private) private$m_path
# }}}

# i_is_unsaved_idf {{{
i_is_unsaved_idf <- function (self, private) {
    private$m_log$unsaved %||% FALSE
}
# }}}

################################################################################
#                                    Group                                     #
################################################################################

# i_group_name {{{
i_group_name <- function (self, private, type = c("idd", "idf")) {
    type <- match.arg(type)
    if (type == "idd") {
        private$m_idd_tbl$group$group_name
    } else {
        cls <- unique(private$m_idf_tbl$object$class_id)
        private$m_idd_tbl$class[class_id %in% cls, list(group_id)][
            private$m_idd_tbl$group, on = "group_id", unique(group_name),
            nomatch = 0L]
    }
}
# }}}

# i_from_group {{{
i_from_group <- function (self, private, class) {
    cls_id <- i_class_index_from_which(self, private, class)
    grp_id <- private$m_idd_tbl$class[J(cls_id), on = "class_id", group_id]
    private$m_idd_tbl$group[J(grp_id), on = "group_id", group_name]
}
# }}}

# i_group_index {{{
i_group_index <- function (self, private, group = NULL) {
    if (is.null(group)) return(private$m_idd_tbl$group$group_id)
    i_assert_valid_group_name(self, private, group)
    private$m_idd_tbl$group[J(group), on = "group_name", group_id]
}
# }}}

# i_is_valid_group_name {{{
i_is_valid_group_name <- function (self, private, group, type = c("idd", "idf")) {
    group %in% i_group_name(self, private, type)
}
# }}}

# i_assert_valid_group_name {{{
i_assert_valid_group_name <- function (self, private, group, type = c("idd", "idf")) {
    type <- match.arg(type)

    valid <- group %in% i_group_name(self, private, type = type)
    key <- switch(type, idd = "Idd", idf = "Idf")

    if (!all(valid))
        stop("Invalid group name found in current ", key, ": ",
            collapse(group[!valid]), ".", call. = FALSE)
}
# }}}

################################################################################
#                                    Class                                     #
################################################################################

# i_class_tbl_from_which {{{
i_class_tbl_from_which <- function (self, private, which = NULL) {
    if (is.null(which)) private$m_idd_tbl$class

    cls_in <- i_in_tbl_from_which(self, private, "class", which)

    private$m_idd_tbl$class[cls_in, on = "class_id"]
}
# }}}

# i_class_memo_from_which {{{
i_class_memo_from_which <- function (self, private, which) {
    cls_in <- i_in_tbl_from_which(self, private, "class", which)

    private$m_idd_tbl$class_memo[cls_in, on = "class_id", memo]
}
# }}}

# i_class_index {{{
i_class_index <- function (self, private, name = NULL, type = c("idd", "idf")) {
    type <- match.arg(type)
    if (is.null(name)) {
        if (type == "idd")
            private$m_idd_tbl$class$class_id
        else sort(unique(private$m_idf_tbl$object$class_id))
    } else {
        i_assert_valid_class_name(self, private, name, type)
        private$m_idd_tbl$class[J(name), on = "class_name", class_id]
    }
}
# }}}

# i_class_name {{{
i_class_name <- function (self, private, index = NULL, type = c("idd", "idf")) {
    type <- match.arg(type)
    if (is.null(index)) {
        if (type == "idd") {
            private$m_idd_tbl$class$class_name
        } else {
            cls_id <- unique(private$m_idf_tbl$object$class_id)
            private$m_idd_tbl$class[J(cls_id), on = "class_id"][order(class_id), class_name]
        }
    } else {
        i_assert_valid_class_index(self, private, index, type)
        if (type == "idd")
            private$m_idd_tbl$class[J(index), on = "class_id", class_name]
        else
            i_object_tbl_from_class(self, private, index)[order(class_id), class_name]
    }
}
# }}}

# i_is_valid_class_name {{{
i_is_valid_class_name <- function (self, private, name, type = c("idd", "idf")) {
    name %in% i_class_name(self, private, type = type)
}
# }}}

# i_is_valid_class_index {{{
i_is_valid_class_index <- function (self, private, index, type = c("idd", "idf")) {
    index %in% i_class_index(self, private, type = type)
}
# }}}

# i_assert_valid_class_index {{{
i_assert_valid_class_index <- function (self, private, index, type = c("idd", "idf")) {
    type <- match.arg(type)
    valid <- index %in% i_class_index(self, private, type = type)
    key <- switch(type, idd = "Idd", idf = "Idf")
    if (any(!valid))
        stop("Invalid class index found in current ", key, ": ",
            collapse(index[!valid]), ".", call. = FALSE)
}
# }}}

# i_assert_valid_class_name {{{
i_assert_valid_class_name <- function (self, private, name, type = c("idd", "idf")) {
    type <- match.arg(type)
    valid <- name %in% i_class_name(self, private, type = type)
    key <- switch(type, idd = "Idd", idf = "Idf")
    if (any(!valid))
        stop("Invalid class name found in current ", key, ": ",
            collapse(name[!valid]), ".", call. = FALSE)

}
# }}}

# i_class_index_from_which {{{
i_class_index_from_which <- function (self, private, which, type = c("idd", "idf")) {
    if (is.numeric(which)) {
        i_assert_valid_class_index(self, private, which, type)
        which
    } else if (is.character(which)) {
        i_class_index(self, private, which, type)
    } else {
        stop("`which` should be either a numeric vector or a character vector.",
            call. = FALSE)
    }
}
# }}}

# i_required_class_name {{{
i_required_class_name <- function (self, private) {
    private$m_idd_tbl$class[required_object == TRUE, class_name]
}
# }}}

# i_unique_class_name {{{
i_unique_class_name <- function (self, private) {
    private$m_idd_tbl$class[unique_object == TRUE, class_name]
}
# }}}

# i_extensible_class_name {{{
i_extensible_class_name <- function (self, private) {
    private$m_idd_tbl$class[num_extensible > 0L, class_name]
}
# }}}

# i_assert_extensible_class {{{
i_assert_extensible_class <- function (self, private, class) {
    cls_tbl <- i_class_tbl_from_which(self, private, class)

    valid <- cls_tbl$class_name %in% i_extensible_class_name(self, private)
    if (any(!valid))
        stop("Non-extensible class found: ", collapse(class[!valid]),
            ".", call. = FALSE)
}
# }}}

# i_class_reference {{{
i_class_reference <- function (self, private, class = NULL) {
    cls_ref <- private$m_idd_tbl$class_reference[private$m_idd_tbl$class,
        on = "class_id"][, .SD, .SDcols = c("class_id", "class_name", "reference")]

    if (is.null(class)) {
        bycol <- "class_id"
        nm <- cls_ref$class_name
    } else {
        bycol <- "class_rleid"
        nm <- class
        cls_ref <- cls_ref[J(nm), on = "class_id"]
    }

    lapply(split(cls_ref, by = "class_id", keep.by = FALSE), `[[`, "reference")
    data.table::setattr(res, "names", nm)
    res
}
# }}}

# i_reference_map {{{
i_reference_map <- function (self, private, class) {
    assert_that(i_is_valid_class_name(self, private, class))
    # check if this class has \reference-class-name
    if (is_empty(private$m_idd_tbl$class_reference)) {
        ref_class <- character(0)
    } else {
        ref_class <- private$m_idd_tbl$class[class_name == class][
            private$m_idd_tbl$class_reference, on = "class_id", nomatch = 0L, list(reference)][
            private$m_idd_tbl$field_object_list, on = c(reference = "object_list"), nomatch = 0L, list(field_id)][
            private$m_idd_tbl$field, on = "field_id", nomatch = 0L, list(class_id)][
            private$m_idd_tbl$class, on = "class_id", unique(class_name), nomatch = 0L]
    }

    # check if fields in this class has \reference attributes
    ref_field <- private$m_idd_tbl$class[class_name == class][
        private$m_idd_tbl$field, on = "class_id", nomatch = 0L, list(field_id)][
        private$m_idd_tbl$field_reference, on = "field_id", nomatch = 0L, list(reference)][
        private$m_idd_tbl$field_object_list, on = c(reference = "object_list"), nomatch = 0L, list(field_id)][
        private$m_idd_tbl$field, on = "field_id", nomatch = 0L][
        private$m_idd_tbl$class, on = "class_id", nomatch = 0L, unique(class_name)]

    # check if fields in this class has \object-list attributes
    object_list <- private$m_idd_tbl$class[class_name == class][
        private$m_idd_tbl$field, on = "class_id", nomatch = 0L, list(field_id)][
        private$m_idd_tbl$field_object_list, on = "field_id", nomatch = 0L, list(object_list)][
        private$m_idd_tbl$field_reference, on = c(object_list = "reference"), nomatch = 0L, list(field_id)][
        private$m_idd_tbl$field, on = "field_id", nomatch = 0L, list(class_id)][
        private$m_idd_tbl$class, on = "class_id", nomatch = 0L, unique(class_name)]

    # check if fields in this class has \external-list attributes
    external_list <- private$m_idd_tbl$class[class_name == class][
        private$m_idd_tbl$field, on = "class_id", nomatch = 0L, list(field_id)][
        private$m_idd_tbl$field_external_list, on = "field_id", nomatch = 0L, list(external_key = unique(external_list))][
        external_key == "autoRDDvariable", external_list := list("eplusout.rdd")][
        external_key == "autoRDDmeter", external_list := list("eplusout.mdd")][
        external_key == "autoRDDvariableMeter", external_list := list(c("eplusout.rdd", "eplusout.mdd"))][
        , unique(unlist(external_list))]

    list(reference_class = ref_class,
         reference_field = ref_field,
         object_list = object_list,
         external_list = external_list)
}
# }}}

################################################################################
#                                    Field                                     #
################################################################################

# i_field_tbl: return all existing fields {{{
i_field_tbl <- function (self, private, class = NULL) {
    if (is.null(class)) return(private$m_idd_tbl$field)

    cls_in <- i_in_tbl_from_which(self, private, "class", class)

    private$m_idd_tbl$field[cls_in, on = "class_id", allow.cartesian = TRUE]
}
# }}}

# i_field_tbl_from_num: return acceptable number of fields {{{
i_field_tbl_from_num <- function (self, private, class, num = 0L) {
    assert_that(are_count(num))
    assert_that(is_same_len(class, num))

    num <- i_field_num_from_index(self, private, class, num)
    cls_tbl <- i_class_tbl_from_which(self, private, class)[, `:=`(num = num)]

    # add extensible group if necessary
    cls_tbl[, `:=`(new_ext_num = 0)]
    cls_tbl[num > num_fields, `:=`(new_ext_num = (num - num_fields) / num_extensible)]
    cls_in <- cls_tbl[new_ext_num > 0L]
    if (not_empty(cls_in)) {
        cls_in <- cls_in[, list(new_ext_num = max(new_ext_num)), by = class_id]
        i_add_extensible_group(self, private, cls_in$class_id, cls_in$new_ext_num)
    }

    fld_tbl <- i_field_tbl(self, private, class)

    fld_tbl[cls_tbl[, list(class_rleid, num)], on = c("class_rleid"),
        roll = TRUE, rollends = c(TRUE, TRUE)][
        field_index <= num][, `:=`(num = NULL)]
}
# }}}

# i_is_valid_field_index_ext {{{
i_is_valid_field_index_ext <- function (self, private, class, index) {
    assert_that(are_count(index))
    assert_that(is_same_len(class, index))

    cls_tbl <- i_class_tbl_from_which(self, private, class)

    cls_tbl$num_extensible > 0L | index <= cls_tbl$num_field
}
# }}}

# i_assert_valid_field_index_ext {{{
i_assert_valid_field_index_ext <- function (self, private, class, index) {
    assert_that(are_count(index))
    assert_that(is_same_len(class, index))

    cls_tbl <- i_class_tbl_from_which(self, private, class)

    valid <- cls_tbl$num_extensible > 0L | index <= cls_tbl$num_field
    if (!all(valid))
        stop("Invalid field index found:\n",
            paste0("  ", lpad(cls_tbl$class_rleid[!valid]), "| Class ",
                surround(cls_tbl$class_name[!valid]),
                ": ", surround(index[!valid]), ".", collapse = "\n"), call. = FALSE)
}
# }}}

# i_is_valid_field_num {{{
i_is_valid_field_num <- function (self, private, class, num) {
    assert_that(are_count(num))
    assert_that(is_same_len(class, num))

    cls_tbl <- i_class_tbl_from_which(self, private, class)

    !(
        # it should be FALSE when num is
        # 1. less than min-fields OR
        cls_tbl$min_fields > num |
        # 2. larger than num-fields but not extensible OR
        (cls_tbl$num_extensible == 0L & num > cls_tbl$num_fields) |
        # 3. larger than num-fields and is extensible but not have full
        #    extensible groups
        (cls_tbl$num_extensible >  0L &
            ((num - cls_tbl$num_fields) %% cls_tbl$num_extensible) != 0L
        )
    )
}
# }}}

# i_assert_valid_field_num {{{
i_assert_valid_field_num <- function (self, private, class, num) {
    cls_tbl <- i_class_tbl_from_which(self, private, class)
    valid <- i_is_valid_field_num(self, private, cls_tbl$class_id, num)

    if (!all(valid))
        stop("Invalid field number found:\n",
            paste0("  ", lpad(cls_tbl$class_rleid), "| Class ",
                surround(cls_tbl$class_name[!valid]),
                ": ", surround(num[!valid]), "."), call. = FALSE)
}
# }}}

# i_field_num_from_index {{{
i_field_num_from_index <- function (self, private, class, index = 0L) {
    cls_tbl <- i_class_tbl_from_which(self, private, class)

    cls_tbl[min_fields == 0L & last_required == 0L,
        num := as.integer(num_fields), by = list(class_rleid)]
    cls_tbl[min_fields >  0L | last_required >  0L,
        num := as.integer(max(min_fields, last_required)), by = list(class_rleid)]

    if (all(index == 0L)) return(cls_tbl$num)

    i_assert_valid_field_index_ext(self, private, class, index)

    cls_tbl[, `:=`(input_index = index, last_extensible = 0L)]
    cls_tbl[is.na(input_index), input_index := 0L]
    cls_tbl[num_extensible > 0L & input_index > first_extensible,
        `:=`(last_extensible = as.integer(ceiling(
                (input_index - first_extensible + 1L) / num_extensible) * num_extensible + first_extensible - 1L)),
        by = list(class_rleid)
    ]

    cls_tbl[input_index >  num_fields,
        num := last_extensible, by = list(class_rleid)]
    cls_tbl[input_index <= num_fields,
        num := max(num, last_extensible, input_index), by = list(class_rleid)]
    cls_tbl$num
}
# }}}

# i_class_field_name {{{
i_class_field_name <- function (self, private, class, lower = FALSE) {
    if (is.null(class)) return(private$m_idd_tbl$field)

    cls_in <- i_in_tbl_from_which(self, private, "class", class)

    fld_tbl <- private$m_idd_tbl$field[cls_in, on = "class_id", allow.cartesian = TRUE]

    if (isTRUE(lower)) fld_tbl[, `:=`(field_name = lower_name(field_name))]

    fld_tbl[, list(class_rleid, field_name)][, lapply(.SD, list), by = list(class_rleid)]$field_name
}
# }}}

# <========      FUNCTION BELOW ONLY WORK FOR A SINGLE CLASS         ========> #
################################################################################
# i_field_tbl_from_which {{{
i_field_tbl_from_which <- function (self, private, class, which = NULL) {
    assert_that(is_scalar(class))

    if (is.null(which)) return(i_field_tbl(self, private, class))

    fld_tbl <- i_field_tbl(self, private, class)

    if (is.null(which)) return(fld_tbl)

    if (is.numeric(which)) {
        i_assert_valid_field_index(self, private, class, which)
        fld_idx <- which
    } else if (is.character(which)) {
        fld_idx <- i_field_index(self, private, class, which)
    } else {
        stop("Index should be either a numeric vector or a character vector.",
            call. = FALSE)
    }

    fld_tbl[J(fld_idx), on = "field_index"]
}
# }}}

# i_field_index {{{
i_field_index <- function (self, private, class, name = NULL) {
    assert_that(is_scalar(class))
    fld_tbl <- i_field_tbl(self, private, class)

    if (is.null(name)) return(fld_tbl$field_index)

    name_std <- fld_tbl$field_name
    name_lc <- lower_name(fld_tbl$field_name)

    res_std <- match(name, name_std)
    res_lc <- match(name, name_lc)

    invalid <- is.na(res_std) & is.na(res_lc)
    if (any(invalid)) {
        stop("Invalid field name found for class ",
            surround(i_class_name(self, private, unique(fld_tbl$class_id))),
            ": ", collapse(name[invalid]), ".", call. = FALSE)
    }

    res_std[is.na(res_std)] <- res_lc[is.na(res_std)]
    res_std
}
# }}}

# i_field_index_from_which {{{
i_field_index_from_which <- function (self, private, class, which = NULL) {
    assert_that(is_scalar(class))
    if (is.null(which)) return(i_field_index(self, private, class))

    if (is.numeric(which)) {
        i_assert_valid_field_index(self, private, class, which)
        which
    } else if (is.character(which)) {
        i_field_index(self, private, class, which)
    } else {
        stop("`which` should be either a numeric vector or a character vector.",
            call. = FALSE)
    }
}
# }}}

# i_field_name {{{
i_field_name <- function (self, private, class, index = NULL, lower = FALSE,
                          unit = FALSE, in_ip = eplusr_option("view_in_ip")) {
    assert_that(is_scalar(class))

    fld_tbl <- i_field_tbl(self, private, class)

    if (!is.null(index)) {
        i_assert_valid_field_index(self, private, class, index)

        fld_tbl <- fld_tbl[J(index), on = "field_index"]
    }

    if (unit) {
        if (in_ip)
            res <- fld_tbl$full_ipname
        else
            res <- fld_tbl$full_name
    } else {
        res <- fld_tbl$field_name
    }

    if (lower)
        lower_name(res)
    else
        res
}
# }}}

# i_is_valid_field_name {{{
i_is_valid_field_name <- function (self, private, class, name) {
    assert_that(is_scalar(class))

    fld_tbl <- i_field_tbl(self, private, class)

    fld_nm <- fld_tbl$field_name

    name %in% fld_nm | name %in% lower_name(fld_nm)
}
# }}}

# i_is_valid_field_index {{{
i_is_valid_field_index <- function (self, private, class, index) {
    assert_that(is_scalar(class))
    index <= i_class_tbl_from_which(self, private, class)$num_fields
}
# }}}

# i_assert_valid_field_index {{{
i_assert_valid_field_index <- function (self, private, class, index) {
    assert_that(is_scalar(class))

    cls_tbl <- i_class_tbl_from_which(self, private, class)
    valid <- index <= cls_tbl$num_fields
    if (!all(valid))
        stop("Invalid field index found for class ", surround(cls_tbl$class_name),
            ": ", collapse(index[!valid]), ".", call. = FALSE)
}
# }}}

# i_assert_valid_field_name {{{
i_assert_valid_field_name <- function (self, private, class, name) {
    valid <- i_is_valid_field_name(self, private, class, name)
    if (is_string(class))
        cls_nm <- class
    else cls_nm <- i_class_name(self, private, class)
    if (!all(valid))
        stop("Invalid field name found for class ", surround(cls_nm),
            ": ", collapse(name[!valid]), ".", call. = FALSE)
}
# }}}

# i_field_note {{{
i_field_note <- function (self, private, class, which = NULL) {
    assert_that(is_scalar(class))

    cls_in <- i_in_tbl_from_which(self, private, "class", class)

    fld_note <- private$m_idd_tbl$field_note[cls_in, on = "class_id", allow.cartesian = TRUE]

    if (is.null(which)) return(fld_note$note)

    fld_idx <- i_field_index_from_which(self, private, cls_in$class_id, which)

    fld_note[J(fld_idx), on = "field_index", note]
}
# }}}

# i_field_unit {{{
i_field_unit <- function (self, private, class, which = NULL, in_ip = eplusr_option("view_in_ip")) {
    fld_tbl <- i_field_tbl_from_which(self, private, class, which)

    if (in_ip)
        fld_tbl$ip_units
    else
        fld_tbl$units
}
# }}}

# i_field_default {{{
i_field_default <- function (self, private, class, which = NULL, in_ip = eplusr_option("view_in_ip")) {
    fld_tbl <- i_field_tbl_from_which(self, private, class, which)

    fld_def <- private$m_idd_tbl$field_default[fld_tbl, on = "field_id"]

    fld_def[, `:=`(default = as.list(default))]

    if (in_ip)
        fld_def[type %in% c("integer", "real"), `:=`(default = as.list(default_ipnum))]
    else
        fld_def[type %in% c("integer", "real"), `:=`(default = as.list(default_num))]

    fld_def$default
}
# }}}

# i_field_choice {{{
i_field_choice <- function (self, private, class, which = NULL) {
    fld_tbl <- i_field_tbl_from_which(self, private, class, which)

    fld_cho <- private$m_idd_tbl$field_choice[fld_tbl,
        on = "field_id"][, lapply(.SD, list), .SDcol = "choice",
        by = list(field_id, field_name)]

    fld_cho$choice
}
# }}}

# i_field_range {{{
i_field_range <- function (self, private, class, which = NULL) {
    fld_tbl <- i_field_tbl_from_which(self, private, class, which)

    fld_ran <- private$m_idd_tbl$field_range[fld_tbl,
        on = "field_id"][, `:=`(range = list({
            r <- list(
                minimum = minimum, lower_incbounds = lower_incbounds,
                maximum = maximum, upper_incbounds = upper_incbounds)
            data.table::setattr(r, "class", c("IddFieldRange", "list"))
            r
        })
        ), by = "field_id"]

    fld_ran$range
}
# }}}

# i_field_reference {{{
i_field_reference <- function (self, private, class, which = NULL) {
    if (is.null(private$m_idf_tbl))
        stop("Function can only be used in IddObjects that are created inside ",
            "an Idf or IdfObject using `$definition()`.", call. = FALSE)

    fld_tbl <- i_field_tbl_from_which(self, private, class, which)[, list(field_id, type)]

    fld_tbl[, reference := list(list(NULL))]
    fld_tbl[type == "node", reference := list(list(i_all_node_value(self, private)))]

    fld_tbl_obj_list <- private$m_idd_tbl$field_object_list[
        fld_tbl, on = "field_id", nomatch = 0L, list(object_list, field_id)]

    if (is_empty(fld_tbl_obj_list)) return(fld_tbl$reference)

    uni_obj_list <- fld_tbl_obj_list[, unique(object_list)]

    # get possible values
    fld_tbl_ref <- i_value_tbl_from_object_list(self, private, uni_obj_list)

    ref <- fld_tbl_ref[fld_tbl_obj_list, on = "object_list"][,
        list(reference = list(unique(unlist(possible_value)))), by = field_id]

    fld_tbl <- ref[fld_tbl[, reference := NULL], on = "field_id"]

    fld_tbl$reference
}
# }}}

# i_field_possible {{{
i_field_possible <- function (self, private, class, which = NULL) {
    if (is.null(private$m_idf_tbl))
        stop("Function can only be used in IddObjects that are created inside ",
            "an Idf or IdfObject using `$definition()`.", call. = FALSE)

    fld_tbl <- i_field_tbl_from_which(self, private, class, which)[
        , list(class_id, field_index, field_name, autosizable, autocalculatable)]

    fld_tbl[, auto := NA_character_]
    fld_tbl[autosizable == TRUE, auto := "Autosize"]
    fld_tbl[autocalculatable == TRUE, auto := "Autocalculate"]

    fld_tbl[, default := list(i_field_default(self, private, class_id[1L], which))]
    fld_tbl[, choice := list(i_field_choice(self, private, class_id[1L], which))]
    fld_tbl[, range := list(i_field_range(self, private, class_id[1L], which))]
    fld_tbl[, reference := list(i_field_reference(self, private, class_id[1L], which))]

    res <- fld_tbl[, list(field_index, field_name, auto, default, choice, range, reference)]
    data.table::setattr(res, "class", c("IddFieldPossible", class(res)))
    res
}
# }}}

# i_field_external_list {{{
i_field_external_list <- function (self, private, class) {
    fld_tbl <- i_field_tbl_from_which(self, private, class, which)

    fld_ext <- private$m_idd_tbl$field_external_list[fld_tbl,
        on = "field_id"][, lapply(.SD, list), .SDcol = "external_list",
        by = list(field_id, field_name)]

    fld_ext$external_list
}
# }}}

# i_is_extensible_index {{{
i_is_extensible_index <- function (self, private, class, index) {
    assert_that(is_scalar(class))
    assert_that(are_count(index))
    cls_tbl <- i_class_tbl_from_which(self, private, class)
    if (!cls_tbl$num_extensible)
        return(rep(FALSE, length(index)))

    index >= cls_tbl$first_extensible
}
# }}}

################################################################################
#                                  Extensible                                  #
################################################################################

# i_last_required_extensible_field_index {{{
i_last_required_extensible_field_index <- function (self, private, class = NULL) {
    cls_in <- i_in_tbl_from_which(self, private, "class", class)
    i_assert_extensible_class(self, private, cls_in$class_id)

    private$m_idd_tbl$field[required_field == TRUE & is_extensible == TRUE,
        list(last_required = max(field_index)), by = list(class_id)][
        cls_in, on = "class_id"][
        is.na(last_required), `:=`(last_required = 0L)]$last_required
}
# }}}

# i_field_index_from_extensible_index {{{
i_field_index_from_extensible_index <- function (self, private, class, index) {
    assert_that(is_same_len(class, index))

    cls_tbl <- i_class_tbl_from_which(self, private, class)
    i_assert_extensible_class(self, private, cls_tbl$class_name)

    cls_tbl[, `:=`(ext_index = index)]

    cls_tbl[, `:=`(field_index =
        list(seq(first_extensible + (ext_index - 1L) * num_extensible,
                length.out = num_extensible))),
       by = list(class_rleid)]
    cls_tbl$field_index
}
# }}}

# i_extensible_group_tbl_from_range {{{
i_extensible_group_tbl_from_range <- function (self, private, class, from, to) {
    if (!(length(class) == length(from) & length(from) == length(to)))
        stop("`class`, `from` and `to` should have the same length.", call. = FALSE)

    assert_that(are_count(from))
    assert_that(are_count(to))
    assert_that(all(from <= to))

    cls_tbl <- i_class_tbl_from_which(self, private, class)
    fld_tbl <- i_field_tbl(self, private, class)

    i_assert_extensible_class(self, private, cls_tbl$class_name)

    cls_tbl[, `:=`(ext_from = from, ext_to = to, ext_num = to - from + 1L)]
    cls_tbl[, `:=`(new_ext_num = ext_num - num_extensible_group)][
        new_ext_num < 0L, `:=`(new_ext_num = 0L)]

    # use the existing if possible
    ext_tbl <- cls_tbl[fld_tbl, on = c("class_rleid", "class_id"), roll = TRUE][
        field_index >= first_extensible &
        field_index < first_extensible +
            num_extensible * ifelse(new_ext_num == 0, ext_num, num_extensible_group)]

    new_ext_cls_tbl <- cls_tbl[new_ext_num > 0L]
    if (not_empty(new_ext_cls_tbl)) {
        first_ext <- new_ext_cls_tbl[fld_tbl,
            on = c("class_rleid", "class_id"), roll = TRUE][
            field_index >= first_extensible &
                field_index < first_extensible + num_extensible]
        first_ext_per_cls <- split(first_ext, by = "class_rleid")
        new_ext_cls_tbl[, `:=`(first_ext = first_ext_per_cls)]

        new_ext <- data.table::rbindlist(lapply(new_ext_cls_tbl,
            function (new_ext_num, first_ext, ...) {
                data.table::rbindlist(replicate(
                    new_ext_num, data.table::copy(first_ext), simplify = FALSE
                ))
            }
        ))

        ext_tbl <- data.table::rbindlist(list(
                ext_tbl[, .SD, .SDcols = names(new_ext)],
                new_ext
        ), use.names = TRUE)
    }

    data.table::setorderv(ext_tbl, "class_rleid")
    ext_tbl[, ext_index :=
        rep(seq(unique(ext_from), length.out = unique(ext_num)),
            each = unique(num_extensible)),
        by = class_rleid]

    ext_idx_tbl <- unique(ext_tbl[, list(class_id, ext_index)])
    fld_idx <- unlist(i_field_index_from_extensible_index(self, private,
            ext_idx_tbl$class_id, ext_idx_tbl$ext_index))

    # TODO: if extensible field name is "A123", then field name should be
    ext_tbl[, `:=`(
        field_index = fld_idx,
        last_req_ext = i_last_required_extensible_field_index(self, private, class_id),
        field_name = stringr::str_replace_all(field_name, "(?<=(A|N| ))\\d+", as.character(ext_index)),
        full_name = stringr::str_replace_all(full_name, "(?<=(A|N| ))\\d+", as.character(ext_index)),
        full_ipname = stringr::str_replace_all(full_ipname, "(?<=(A|N| ))\\d+", as.character(ext_index)))]

    ext_tbl[field_index > last_req_ext, `:=`(required_field = FALSE)]
    ext_tbl[, `:=`(last_req_ext = NULL)]

    ext_tbl
}
# }}}

# i_extensible_group_tbl_from_num {{{
i_extensible_group_tbl_from_num <- function (self, private, class, num) {
    i_extensible_group_tbl_from_range(self, private, class, from = 1L, to = num)
}
# }}}

# i_extensible_group_tbl_from_index {{{
i_extensible_group_tbl_from_index <- function (self, private, class, index) {
    i_extensible_group_tbl_from_range(self, private, class, from = index, to = index)
}
# }}}

# i_add_extensible_group {{{
i_add_extensible_group <- function (self, private, class, num) {
    if (anyDuplicated(class))
        stop("`class` should not contain any duplications", call. = FALSE)

    cls_tbl <- i_class_tbl_from_which(self, private, class)

    if (not_empty(cls_tbl[num_extensible == 0]))
        stop("Failed to add extensible groups. Non-extensible class found:\n",
            cls_tbl[num_extensible == 0L, paste0("  ", lpad(class_rleid),
                "| Class ", surround(class_name), ".", collapse = "\n")], call. = FALSE)

    assert_that(is_same_len(class, num))

    new_ext_fld <- i_extensible_group_tbl_from_range(self, private, class,
        from = cls_tbl$num_extensible_group + 1, to = cls_tbl$num_extensible_group + num)
    old_fld_id <- new_ext_fld$field_id
    new_fld_id <- i_new_id(self, private, "field", nrow(new_ext_fld))
    new_ext_fld[, `:=`(field_id = new_fld_id)]

    id_tbl <- data.table::data.table(field_id = old_fld_id, new_field_id = new_fld_id)

    # FIELD
    private$m_idd_tbl$field <- data.table::rbindlist(list(
            private$m_idd_tbl$field,
            new_ext_fld[, .SD, .SDcols = names(private$m_idd_tbl$field)]),
        use.names = TRUE)

    # i_add_attr_tbl: helper to append field attribute tables {{{
    i_add_attr_tbl <- function (self, private, name, id_tbl) {
        tbl_name <- paste0("field_", name)
        assert_that(has_name(private$m_idd_tbl, tbl_name))

        attr_tbl_full <- private$m_idd_tbl[[tbl_name]][id_tbl, on = "field_id",
            allow.cartesian = TRUE]

        attr_tbl <- attr_tbl_full[!is.na(get(paste0(name, "_id")))][, `:=`(
            field_id = new_field_id, new_field_id = NULL)]

        if (is_empty(attr_tbl)) return(invisible())

        new_attr_id <- i_new_id(self, private, tbl_name, nrow(attr_tbl))
        attr_tbl[[paste0(name, "_id")]] <- new_attr_id

        private$m_idd_tbl[[tbl_name]] <- data.table::rbindlist(list(
                private$m_idd_tbl[[tbl_name]], attr_tbl), use.names = TRUE)
    }
    # }}}
    i_add_attr_tbl(self, private, "range", id_tbl)
    i_add_attr_tbl(self, private, "choice", id_tbl)
    i_add_attr_tbl(self, private, "default", id_tbl)
    i_add_attr_tbl(self, private, "reference", id_tbl)
    i_add_attr_tbl(self, private, "object_list", id_tbl)
    i_add_attr_tbl(self, private, "external_list", id_tbl)

    # CLASS
    cls_tbl[, `:=`(
        num_extensible_group = num_extensible_group + num,
        num_fields = num_fields + num * num_extensible)]
    private$m_idd_tbl$class <- data.table::rbindlist(list(
        private$m_idd_tbl$class[!class_id %in% cls_tbl$class_id],
        cls_tbl[, .SD, .SDcols = names(private$m_idd_tbl$class)]),
        use.names = TRUE)
    data.table::setorderv(private$m_idd_tbl$class, "class_id")
    self
}
# }}}

# i_del_extensible_group {{{
i_del_extensible_group <- function (self, private, class, num) {
    if (anyDuplicated(class))
        stop("`class` should not contain any duplications", call. = FALSE)

    cls_tbl <- i_class_tbl_from_which(self, private, class)

    if (not_empty(cls_tbl[num_extensible == 0]))
        stop("Non-extensible class found:\n",
            cls_tbl[num_extensible == 0L, paste0("  ", lpad(class_rleid),
                "| Class ", surround(class_name), ": ", surround(new_ext_num),
                ".", collapse = "\n")], call. = FALSE)

    assert_that(is_same_len(class, num))

    cls_tbl[, `:=`(del_ext_num = num)]
    cls_tbl[, `:=`(left_fields = num_fields - del_ext_num * num_extensible)]
    if (not_empty(cls_tbl[left_fields < last_required])) {
        stop("Failed to delete extensible groups. Number of fields left less ",
            "than required:\n",
            cls_tbl[left_fields < last_required, paste0("  ", lpad(class_rleid),
                "| Class ", surround(class_name), ": ", surround(left_fields),
                "(Required: ", surround(last_required), ").", collapse = "\n")],
            call. = FALSE)
    }

    fld_tbl <- i_field_tbl(self, private, class)[cls_tbl,
        on = c("class_rleid", "class_id"), allow.cartesian = TRUE]
    del_id <- fld_tbl[field_index > left_fields, field_id,
        by = list(class_rleid, class_id)]$field_id

    # FIELD
    nm_fld_tbl <- names(private$m_idd_tbl)[startsWith(names(private$m_idd_tbl), "field")]
    for (tbl in nm_fld_tbl) {
        private$m_idd_tbl[[tbl]] <- private$m_idd_tbl[[tbl]][!field_id %in% del_id]
    }

    # CLASS
    cls_tbl[, `:=`(
        num_extensible_group = num_extensible_group - del_ext_num,
        num_fields = left_fields)]
    private$m_idd_tbl$class <- data.table::rbindlist(list(
        private$m_idd_tbl$class[!class_id %in% cls_tbl$class_id],
        cls_tbl[, .SD, .SDcols = names(private$m_idd_tbl$class)]),
        use.names = TRUE)
    data.table::setorderv(private$m_idd_tbl$class, "class_id")
    self
}
# }}}

################################################################################
#                                    Object                                    #
################################################################################

# i_object_tbl_from_class {{{
i_object_tbl_from_class <- function (self, private, class = NULL) {
    if (is.null(class))
        return(private$m_idd_tbl$class[private$m_idf_tbl$object, on = "class_id"])

    cls_in <- i_in_tbl_from_which(self, private, "class", class, "idf")

    private$m_idd_tbl$class[
        private$m_idf_tbl$object[cls_in, on = "class_id"], on = "class_id"]
}
# }}}

# i_object_tbl_from_which {{{
i_object_tbl_from_which <- function (self, private, which, class = TRUE) {
    if (is.numeric(which)) {
        i_assert_valid_object_id(self, private, which)
        obj_in <- data.table::data.table(object_id = which,
            object_rleid = seq_along(which))
        obj_tbl <- private$m_idf_tbl$object[obj_in, on = "object_id"]
    } else if (is.character(which)) {
        i_assert_valid_object_name(self, private, which)

        obj_in <- data.table::data.table(object_name_upper = toupper(which),
            object_rleid = seq_along(which))
        obj_tbl <- private$m_idf_tbl$object[obj_in, on = "object_name_upper"]

        dup <- obj_tbl[, list(num = .N), by = list(object_rleid, object_name)][num > 1]
        if (not_empty(dup)) {
            dup <- unique(dup)
            stop("Multiple objects found with the same name: \n",
                paste0(surround(dup$object_name), "(", dup$num, ")", collapse = ", "),
                ". Please specify objects using object IDs", call. = FALSE)
        }
    } else {
        stop("Index should be either a numeric vector or a character vector.",
            call. = FALSE)
    }

    if (!class) return(obj_tbl)
    private$m_idd_tbl$class[obj_tbl, on = "class_id"]
}
# }}}

# i_new_object_tbl {{{
i_new_object_tbl <- function (self, private, object_id, class_id, object_name = NULL) {
    if (is.null(object_name)) object_name <- NA_character_

    data.table::data.table(object_id = object_id, class_id = class_id,
        object_name = object_name, object_name_upper = toupper(object_name))
}
# }}}

# i_object_num {{{
i_object_num <- function (self, private, class = NULL) {
    if (is.null(class)) return(nrow(private$m_idf_tbl$object))

    cls_id <- i_class_index(self, private, class)

    private$m_idf_tbl$object[J(cls_id), on = "class_id"][,
        .N, by = list(class_id, found = !is.na(object_id))][
        found == FALSE, `:=`(N = 0L)]$N
}
# }}}

# i_object_id {{{
i_object_id <- function (self, private, class = NULL, simplify = FALSE) {
    obj_tbl <- i_object_tbl_from_class(self, private, class)

    if (simplify) return(obj_tbl$object_id)

    if (is.null(class)) {
        bycol <- "class_id"
        data.table::setorderv(obj_tbl, "class_id")
        nm <- unique(obj_tbl$class_name)
    } else {
        bycol <- "class_rleid"
        nm <- class
    }

    res <- lapply(split(obj_tbl, by = bycol, keep.by = FALSE), `[[`, "object_id")
    data.table::setattr(res, "names", nm)
    res
}
# }}}

# i_object_id_from_which {{{
i_object_id_from_which <- function (self, private, which) {
    if (is.numeric(which)) {
        i_assert_valid_object_id(self, private, which)
        which
    } else if (is.character(which)) {
        i_assert_valid_object_name(self, private, which)

        obj_in <- data.table::data.table(object_name_upper = toupper(which),
            object_rleid = seq_along(which))
        res <- private$m_idf_tbl$object[obj_in, on = "object_name_upper"]

        dup <- res[, list(num = .N), by = list(object_rleid, object_name)][num > 1]
        if (not_empty(dup)) {
            dup <- unique(dup)
            stop("Multiple objects found with the same name: \n",
                paste0(surround(dup$object_name), "(", dup$num, ")", collapse = ", "),
                ". Please specify objects using object IDs", call. = FALSE)
        }

        res$object_id
    } else {
        stop("`which` should be either a numeric vector or a character vector.",
            call. = FALSE)
    }
}
# }}}

# i_is_valid_object_id {{{
i_is_valid_object_id <- function (self, private, id) {
    assert_that(are_count(id))
    id %in% private$m_idf_tbl$object$object_id
}
# }}}

# i_assert_valid_object_id {{{
i_assert_valid_object_id <- function (self, private, id) {
    valid <- id %in% private$m_idf_tbl$object$object_id

    if (!all(valid))
        stop("Invalid object ID found for current Idf: ",
            collapse(id[!valid]), ".", call. = FALSE)
}
# }}}

# i_object_name {{{
i_object_name <- function (self, private, class = NULL, simplify = FALSE, upper = FALSE) {
    obj_tbl <- i_object_tbl_from_class(self, private, class)
    col <- ifelse(upper, "object_name_upper", "object_name")

    if (simplify) return(obj_tbl[[col]])

    if (is.null(class)) {
        bycol <- "class_id"
        data.table::setorderv(obj_tbl, "class_id")
        nm <- unique(obj_tbl$class_name)
    } else {
        bycol <- "class_rleid"
        nm <- class
    }

    res <- lapply(split(obj_tbl, by = bycol, keep.by = FALSE), `[[`, col)
    data.table::setattr(res, "names", nm)
    res
}
# }}}

# i_is_valid_object_name {{{
i_is_valid_object_name <- function (self, private, name) {
    assert_that(is.character(name))
    toupper(name) %in% private$m_idf_tbl$object[!is.na(object_name), object_name_upper]
}
# }}}

# i_assert_valid_object_name {{{
i_assert_valid_object_name <- function (self, private, name) {
    valid <- toupper(name) %in% private$m_idf_tbl$object[
        !is.na(object_name), object_name_upper]

    if (!all(valid))
        stop("Invalid object name found for current Idf: ",
            collapse(name[!valid]), ".", call. = FALSE)
}
# }}}

# i_assert_can_add_object {{{
i_assert_can_add_object <- function (self, private, class) {
    is_version <- class == "Version"
    if (any(is_version))
        stop("Add `Version` object directly is prohibited.", call. = FALSE)

    if (eplusr_option("validate_level") != "final") return()

    is_unique <- class %in% i_unique_class_name(self, private)
    if (!all(is_unique)) return()

    uni_cls <- class[is_unique]

    obj_num <- i_object_num(self, private, uni_cls)
    if (all(obj_num == 0L)) return()

    dup_cls <- uni_cls[obj_num > 0L]

    if (not_empty(dup_cls))
        stop("In `final` validation level, existing unique objects cannot be ",
            "duplicated. Class ", backtick(dup_cls), " is existing unique object ",
            "that can ", "not be added or duplicated.", call. = FALSE)

    dup_in <- unique(uni_cls[duplicated(uni_cls)])

    if (not_empty(dup_in))
        stop("In `final` validation level, existing unique objects can only be ",
            "added onece. Class ", backtick(dup_in), " is unique object that can ",
            "only be added once.", call. = FALSE)
}
# }}}

# i_assert_can_set_object {{{
i_assert_can_set_object <- function (self, private, object) {
    obj_tbl <- i_object_tbl_from_which(self, private, object)

    is_version <- obj_tbl$class_name == "Version"

    if (any(is_version))
        stop("Modify `Version` object directly is prohibited.", call. = FALSE)

    if (anyDuplicated(obj_tbl$object_id))
        stop("`object` should not contain any duplication.", call. = FALSE)
}
# }}}

# i_assert_can_del_object {{{
i_assert_can_del_object <- function (self, private, object) {
    obj_tbl <- i_object_tbl_from_which(self, private, object)

    is_version <- obj_tbl$class_name == "Version"

    if (any(is_version))
        stop("Deleting `Version` object is prohibited.", call. = FALSE)

    if (anyDuplicated(obj_tbl$object_id))
        stop("`object` should not contain any duplication.", call. = FALSE)

    if (eplusr_option("validate_level") != "final") return()

    class_name <- obj_tbl$class_name

    is_required <- class_name %in% i_required_class_name(self, private)
    if (any(is_required))
        stop("In `final` validation level, deleting an required object is prohibited ",
            "deleted. Class ", surround(unique(class_name[is_required])),
            " is required object that can not be deleted.", call. = FALSE)

    is_unique <- class_name %in% i_unique_class_name(self, private)
    if (!all(is_unique)) return()

    uni_cls <- class_name[is_unique]

    obj_num <- i_object_num(self, private, uni_cls)
    if (all(obj_num == 0L)) return()

    dup_cls <- uni_cls[obj_num > 0L]

    if (not_empty(dup_cls))
        stop("In `final` validation level, existing unique objects cannot be ",
            "deleted. Class ", surround(dup_cls), " is existing unique object ",
            "that can ", "not be deleted.", call. = FALSE)
}
# }}}

# create_iddobj_generator {{{
create_iddobj_generator <- function (self, private, IddObject) {
    # clone the IddObject R6ClassGenerator
    own_iddobject <- clone_generator(IddObject)

    # assign shared data to IddObject R6Class Generator
    assign_idd_shared_data(self, private, own_iddobject)
}
# }}}
# assign_idd_shared_data {{{
assign_idd_shared_data <- function (self, private, iddobj_gen) {
    # assign shared data to IddObject R6Class Generator
    shared <- c("m_uuid", "m_version", "m_idd_tbl")
    for (nm in shared) {
        iddobj_gen$self$private_fields[[nm]] <- private[[nm]]
        iddobj_gen$private_fields[[nm]] <- private[[nm]]
    }

    # self reference
    iddobj_gen$self$private_fields$m_iddobj_gen <- iddobj_gen
    iddobj_gen$self$self$private_fields$m_iddobj_gen <- iddobj_gen

    iddobj_gen
}
# }}}

# create_idfobj_generator {{{
create_idfobj_generator <- function (self, private, IdfObject) {
    # clone the IdfObject R6ClassGenerator
    own_idfobject <- clone_generator(IdfObject)

    # assign shared data to IdfObject R6ClassGenerator
    assign_idf_shared_data(self, private, own_idfobject)
}
# }}}
# assign_idf_shared_data {{{
assign_idf_shared_data <- function (self, private, idfobj_gen) {
    # assign shared data to IddObject R6Class Generator
    shared <- c("m_version", "m_idf_tbl", "m_idd_tbl", "m_log",
        "m_iddobj_gen", "m_idfobj_gen")
    for (nm in shared) {
        idfobj_gen$self$private_fields[[nm]] <- private[[nm]]
        idfobj_gen$private_fields[[nm]] <- private[[nm]]
    }

    # self reference
    idfobj_gen$self$private_fields$m_idfobj_gen <- idfobj_gen
    idfobj_gen$self$self$private_fields$m_idfobj_gen <- idfobj_gen

    idfobj_gen
}
# }}}

# i_definition {{{
i_definition <- function (self, private, class) {
    res <- i_iddobject(self, private, class)

    assign_idf_tbl <- function (iddobj, idf_tbl) {
        priv <- ._get_private(iddobj)
        priv$m_idf_tbl <- idf_tbl
        iddobj
    }

    lapply(res, assign_idf_tbl, idf_tbl = private$m_idf_tbl)
}
# }}}

# i_iddobject {{{
i_iddobject <- function (self, private, class) {
    res <- lapply(class, private$m_iddobj_gen$new)
    data.table::setattr(res, "names", class)
    res
}
# }}}

# i_iddobject_in_group {{{
i_iddobject_in_group <- function (self, private, group) {
    assert_that(is_string(group))
    i_assert_valid_group_name(self, private, group)

    cls <- private$m_idd_tbl$group[group_name == group][
        private$m_idd_tbl$class, on = "group_id", nomatch = 0L, class_name]

    res <- lapply(cls, private$m_iddobj_gen$new)
    data.table::setattr(res, "names", cls)
    res
}
# }}}

# i_idfobject {{{
i_idfobject <- function (self, private, which) {
    obj_id <- i_object_id_from_which(self, private, which)

    obj_nm <- private$m_idf_tbl$object[J(obj_id), on = "object_id", object_name]

    res <- lapply(obj_id, private$m_idfobj_gen$new)
    data.table::setattr(res, "names", underscore_name(obj_nm))
    res
}
# }}}

# i_idfobject_in_class {{{
i_idfobject_in_class <- function (self, private, class) {
    obj_id <- i_object_id(self, private, class, simplify = TRUE)

    obj_nm <- private$m_idf_tbl$object[J(obj_id), on = "object_id", object_name]

    res <- lapply(obj_id, private$m_idfobj_gen$new)
    data.table::setattr(res, "names", underscore_name(obj_nm))
    res
}
# }}}

# i_search_object {{{
i_search_object <- function (self, private, pattern, class = NULL) {
    obj_tbl <- private$m_idf_tbl$object

    if (not_empty(class)) {
        if (anyDuplicated(class)) {
            stop("`class` should not contain any duplication.", call. = FALSE)
        }
        cls_id <- i_class_index_from_which(self, private, class, type = "idf")
        obj_tbl <- obj_tbl[J(cls_id), on = "class_id"]
    }


    obj_tbl <- obj_tbl[stringr::str_detect(object_name, pattern)]

    if (is_empty(obj_tbl)) {
        message("No matched result found.")
        return(invisible())
    }

    res <- lapply(obj_tbl$object_id, private$m_idfobj_gen$new)
    data.table::setattr(res, "names", underscore_name(obj_tbl$object_name))
    res
}
# }}}

# i_dup_object {{{
i_dup_object = function (self, private, which, new_name = NULL) {
    obj_tbl <- i_object_tbl_from_which(self, private, which)

    # check if adding is allowed
    i_assert_can_add_object(self, private, unique(obj_tbl$class_name))

    # check input new names {{{
    if (!is.null(new_name)) {
        # check length of object and new_name
        if (length(new_name) != nrow(obj_tbl)) {
            stop("`new_name` (", length(new_name), ") should be a character ",
                "vector with the same length as the number of target objects (",
                nrow(obj_tbl), ").", call. = FALSE)
        }
    }

    # add new object columns
    obj_tbl[, `:=`(new_object_name = as.character(new_name),
        new_object_name_upper = toupper(as.character(new_name)))]

    if (!is.null(new_name)) {
        # check if trying to assign names to objects that do not have name
        # attribute
        can_not_name <- obj_tbl[has_name == FALSE & !is.na(new_object_name)]
        if (not_empty(can_not_name)) {
            stop("Target object(s) (ID: ", collapse(can_not_name$object_id),
                ") does not have name attribute.", call. = FALSE)
        }

        # check if trying to assign the same name
        if (eplusr_option("validate_level") == "final") {
            same_name <- obj_tbl[has_name == TRUE & object_name_upper == new_object_name_upper]
            if (not_empty(same_name)) {
                stop("Duplicate objects without new names is prohibited in `final` ",
                    "validation level. `new_name` should not be the same as the ",
                    "target object.", call. = FALSE)
            }
        }
    }
    # }}}

    # get new object name {{{
    # store old name for logging
    obj_tbl[, `:=`(old_object_name = object_name)]

    # replace object names with input new names
    obj_tbl[, `:=`(use_input_name = FALSE)]
    obj_tbl[has_name == TRUE & !is.na(new_object_name_upper) &
        object_name_upper != new_object_name_upper,
        `:=`(object_name = new_object_name, object_name_upper = new_object_name_upper,
             use_input_name = TRUE)]

    # get all name in the same class
    obj_tbl[, `:=`(all_name_upper = i_object_name(self, private, class_name, upper = TRUE)), by = list(object_rleid)]

    # check if trying to duplicate same object several times
    obj_tbl[, `:=`(same_name_order = seq_along(object_name)), by = object_name_upper]

    # get the duplicated times before
    obj_tbl[!is.na(object_name), `:=`(
        max_suffix_num = apply2_int(all_name_upper, object_name_upper, function (x, y) {
            num <- stringr::str_match(x, paste0("^", y, "_(\\d+)$"))[,2]
            num[is.na(num)] <- "0"
            max(as.integer(num))
        })
    )]

    # get new object name
    set(obj, NULL, "use_input_name", FALSE)
    obj_tbl[!is.na(object_name), `:=`(object_name = paste0(
        object_name, "_", max_suffix_num + same_name_order))]
    obj_tbl[!is.na(object_name) & same_name_order == 1L &
            use_input_name == TRUE & max_suffix_num == 0L,
        `:=`(object_name = new_object_name, assigned_new_name = TRUE)]
    obj_tbl[, `:=`(object_name_upper = toupper(object_name))]
    obj_tbl[is.na(assigned_new_name), `:=`(assigned_new_name = FALSE)]

    # check if user input name is one of existing names
    if (eplusr_option("validate_level") == "final") {
        existing_name <- obj_tbl[has_name == TRUE &
            apply2_lgl(object_name_upper, all_name_upper, `%in%`)]
        if (not_empty(existing_name)) {
            stop("Duplicate objects with existing names is prohibited in `final` ",
                "validation level. Input new name ",
                collapse(existing_name$object_name),
                ifelse(nrow(existing_name) == 1L, " is ", " are "),
                "one of existing object names in the same class." , call. = FALSE)
        }
    }

    # logging
    obj_auto_nm <- obj_tbl[!is.na(object_name) & assigned_new_name == FALSE]
    if (not_empty(obj_auto_nm))
        i_verbose_info(self, private,
            "New names of duplicated objects were not given. Automatically ",
            "generated names were assigned:\n",
            paste0("  * Target Object [ID: ", obj_auto_nm$object_id, "] Name ",
                rpad(surround(obj_auto_nm$old_object_name)),
                "--> ", "Auto Assigned Name: ",
                surround(obj_auto_nm$object_name), collapse = "\n"))
    # }}}

    # get value and comment
    # allow to duplicate same object multiple times
    val_tbl <- i_value_tbl_from_which(self, private, obj_tbl$object_id, allow_cartesian = TRUE)
    cmt_tbl <- i_comment_tbl_from_which(self, private, obj_tbl$object_id, nomatch = 0L)
    old_val_id <- val_tbl$value_id

    # get new ids
    new_obj_id <- i_new_id(self, private, "object", length(which))
    new_val_id <- i_new_id(self, private, "value", nrow(val_tbl))
    new_cmt_id <- i_new_id(self, private, "comment", nrow(cmt_tbl))
    new_obj_id_val_tbl <- rep(new_obj_id, times = i_value_num(self, private, obj_tbl$object_id))
    new_obj_id_cmt_tbl <- rep(new_obj_id, times = i_comment_num(self, private, obj_tbl$object_id))

    # assign name
    val_tbl[is_name == TRUE, `:=`(
        value = obj_tbl$object_name,
        value_upper = obj_tbl$object_name_upper)]

    # assign id
    obj_tbl[, `:=`(object_id = new_obj_id)]
    val_tbl[, `:=`(value_id = new_val_id, object_id = new_obj_id_val_tbl)]
    cmt_tbl[, `:=`(comment_id = new_cmt_id, object_id = new_obj_id_cmt_tbl)]

    # assign tbl
    i_assign_object_tbl(self, private, obj_tbl)
    i_assign_value_tbl(self, private, val_tbl)
    i_assign_comment_tbl(self, private, cmt_tbl)

    # update value reference
    i_update_value_reference_from_id(self, private, old_val_id, new_val_id)

    # log order
    i_log_new_object_order(self, private, new_obj_id)

    # log unsaved
    i_log_unsaved_idf(self, private)

    # log uuid
    i_log_new_uuid(self, private)

    i_idfobject(self, private, new_obj_id)
}
# }}}

# i_add_object {{{
i_add_object = function (self, private, class, value = NULL, comment = NULL, default = TRUE, all = FALSE) {
    # get class tbl
    cls_tbl <- i_class_tbl_from_which(self, private, class)
    cls_id <- cls_tbl$class_id

    i_assert_can_add_object(self, private, cls_tbl$class_name)
    i_assert_valid_input_format(class, value, comment, default, type = "add")

    # OBJECT
    new_obj_id <- i_new_id(self, private, "object", length(class))
    obj_tbl <- i_new_object_tbl(self, private, new_obj_id, cls_id)
    obj_tbl[, `:=`(object_rleid = .I)]
    # combine class property with new object tbl
    obj_tbl <- obj_tbl[cls_tbl, on = c(object_rleid = "class_rleid", "class_id")]

    # VALUE
    val_tbl <- i_valid_value_input(self, private, obj_tbl, value, default, all, "add")
    new_val_id <- i_new_id(self, private, "value", nrow(val_tbl))
    val_tbl[, `:=`(value_id = new_val_id)]

    # UPDATE OBJECT NAME
    obj_tbl <- val_tbl[is_name == TRUE, list(object_id, value)][
        obj_tbl, on = "object_id"][!is.na(value), `:=`(
        object_name = value, object_name_upper = toupper(value))][,
        `:=`(value = NULL)]

    # VALIDATE
    # only validate fields that do not use default values and extensible fields
    val_tbl_chk <- val_tbl[is_default == FALSE | is_extensible == TRUE]

    i_validate_in_object(self, private, obj_tbl, val_tbl_chk)
    if (any(vapply(private$m_log$validate, not_empty, logical(1)))) {
        i_print_validate(private$m_log$validate)
        stop("Failed to add objects in class ",
            collapse(unique(cls_tbl$class_name)), ".", call. = FALSE)
    }
    on.exit({private$m_log$validate <- NULL}, add = TRUE)

    # assign tbl
    i_assign_object_tbl(self, private, obj_tbl)
    i_assign_value_tbl(self, private, val_tbl)

    # update value reference
    i_update_value_reference_from_tbl(self, private, val_tbl)

    # COMMENT {{{
    if (!is.null(comment)) {
        cmt_tbl <- i_new_comment_tbl(self, private, new_obj_id, comment)
        new_cmt_id <- i_new_id(self, private, "comment", nrow(cmt_tbl))
        cmt_tbl[, `:=`(comment_id = new_cmt_id)]
        i_assign_comment_tbl(self, private, cmt_tbl)
    }
    # }}}

    # log order
    i_log_new_object_order(self, private, new_obj_id)

    # log unsaved
    i_log_unsaved_idf(self, private)

    # log uuid
    i_log_new_uuid(self, private)

    # return new objects
    i_idfobject(self, private, new_obj_id)
}
# }}}

# i_ins_object {{{
i_ins_object = function (self, private, object) {
    cur_ver <- i_version(self, private)

    if (is_idfobject(object)) {
        i_insert_single_object(self, private, object)
    } else if (is.list(object) && not_empty(object)) {
        len <- length(object)
        # every component should be an IdfObject
        valid <- vapply(object, is_idfobject, logical(1))
        if (!all(valid)) {
            stop("When input is a list, every component should be an ",
                 "IdfObject.", call. = FALSE)
        }
        lapply(object, function (x) i_insert_single_object(self, private, x))
    } else {
        stop("Input should be an IdfObject, or a list of IdfObjects.", call. = FALSE)
    }
}
# }}}

# i_insert_single_object {{{
i_insert_single_object <- function (self, private, object) {
    cur_ver <- i_version(self, private)

    in_self <- ._get_self(object)
    in_priv <- ._get_private(object)
    in_ver <- i_version(in_self, in_priv)

    # check if it is a version object
    if (object$class_name() == "Version") {
        stop("Could not insert a `Version` object.", call. = FALSE)
    }

    # input version which should be the same version as this model
    if (cur_ver != in_ver) {
        stop("Input object has a different version ", surround(in_ver),
             " than current Idf object (", surround(cur_ver), ").",
             call. = FALSE)
    }

    # get the uuid to see if it comes from the same object
    in_uuid <- in_priv$m_log$uuid
    if (in_uuid == private$m_log$uuid) {
        i_verbose_info(self, private, "Object (ID:", surround(object$id()),
            ") to insert is an object from current Idf. The target object ",
            "will be directly duplicated instead of creating a new one with ",
            "same values.")
        self$dup_object(object$id())[[1]]
    } else {
        cls <- object$class_name()
        # get all value
        val <- object$get_value()
        self$add_object(cls, val)[[1]]
    }
}
# }}}

# i_set_object {{{
i_set_object = function (self, private, object, value = NULL, comment = NULL, default = FALSE) {
    # get object tbl
    obj_tbl <- i_object_tbl_from_which(self, private, object)

    i_assert_can_set_object(self, private, obj_tbl$object_id)
    i_assert_valid_input_format(object, value, comment, default, type = "set")

    # VALUE
    val_tbl <- i_valid_value_input(self, private, obj_tbl, value, default, type = "set")

    # add value id
    num_empty_id <- val_tbl[is.na(value_id), .N]
    if (num_empty_id)
        val_tbl[is.na(value_id),
            value_id := i_new_id(self, private, "value", num_empty_id)]

    # UPDATE OBJECT NAME
    obj_tbl <- val_tbl[is_name == TRUE, list(object_id, value)][
        obj_tbl, on = "object_id"][!is.na(value), `:=`(
        object_name = value, object_name_upper = toupper(value))][,
        `:=`(value = NULL)]

    # VALIDATE
    # only validate fields that do not use default values
    val_tbl_chk <- val_tbl[is_default == FALSE]
    i_validate_in_object(self, private, obj_tbl, val_tbl_chk, temp = FALSE)
    if (any(vapply(private$m_log$validate, not_empty, logical(1)))) {
        i_print_validate(private$m_log$validate)
        stop("Failed to set new value to object ID ", collapse(obj_tbl$object_id), ".", call. = FALSE)
    }
    on.exit({private$m_log$validate <- NULL}, add = TRUE)

    # remove empty values
    last_val <- val_tbl[!is.na(value), list(last_index = max(field_index)),
        by = list(object_rleid, class_id)]

    last_val[, last_index := as.integer(i_field_num_from_index(
        self, private, class_id, last_index), by = list(object_rleid))]

    val_tbl_set <- val_tbl[last_val, on = list(object_rleid, class_id, field_index <= last_index)]

    # assign tbl
    i_assign_object_tbl(self, private, obj_tbl)
    i_assign_value_tbl(self, private, val_tbl_set)

    # update value reference
    i_update_value_reference_from_tbl(self, private, val_tbl)

    # COMMENT {{{
    if (!is.null(comment)) {
        cmt_tbl <- i_new_comment_tbl(self, private, obj_tbl$object_id, comment)
        new_cmt_id <- i_new_id(self, private, "comment", nrow(cmt_tbl))
        cmt_tbl[, `:=`(comment_id = new_cmt_id)]
        i_assign_comment_tbl(self, private, cmt_tbl)
    }
    # }}}

    # log order
    i_log_add_object_order(self, private, obj_tbl$object_id)

    # log unsaved
    i_log_unsaved_idf(self, private)

    # log uuid
    i_log_new_uuid(self, private)

    # return new objects
    i_idfobject(self, private, obj_tbl$object_id)
}
# }}}

# i_del_object {{{
i_del_object <- function (self, private, object, referenced = FALSE) {
    i_assert_can_del_object(self, private, object)

    obj_tbl <- i_object_tbl_from_which(self, private, object)
    val_tbl <- i_value_tbl_from_which(self, private, object, field = FALSE)
    obj_id <- obj_tbl$object_id

    ref_by_tbl <- i_val_ref_by_tbl(self, private, val_tbl)

    # check if target objects are referenced {{{
    if (not_empty(ref_by_tbl)) {
        if (eplusr_option("validate_level") == "final") {
            ref <- ref_by_tbl[, list(referenced_by = collapse(referenced_by_object_id)),
                by = list(object_rleid, object_id)]
            stop("Deleting an object that is referenced by others is prohibited ",
                "in `final` validation level. Failed to delete target object ",
                "[ID:", collapse(obj_tbl$object_id), "]:\n",
                paste0(paste0(ref$object_rleid, ": Object [ID:",surround(ref$object_id),"] was ",
                        "referenced by other objects [ID:", ref$referenced_by, "]."),
                    collapse = "\n"),
                call. = FALSE)
        }

        by_id <- unique(ref_by_tbl$referenced_by_object_id)

        if (referenced) {
            i_verbose_info(self, private, "Delete target object [ID:",
                collapse(obj_id), "] and also objects [ID:",
                collapse(by_id), "] that are referencing target object.")
            obj_id <- c(obj_id, by_id)
        } else {
            i_verbose_info(self, private, "Delete target object [ID:",
                collapse(obj_id), "] which was referenced by objects ",
                "[ID: ", collapse(by_id), "]. Error may occur during ",
                "simulation.")
        }
    }
    # }}}

    # delete rows in object table
    private$m_idf_tbl$object <- private$m_idf_tbl$object[!object_id %in% obj_id]

    # delete rows in value table and value reference table
    private$m_idf_tbl$value <- private$m_idf_tbl$value[!object_id %in% obj_id]

    # delete rows in value reference table
    private$m_idf_tbl$value_reference <- private$m_idf_tbl$value_reference[
        !val_tbl, on = "value_id"][
        !ref_by_tbl, on = c(reference_value_id = "referenced_by_value_id")]

    # log order
    i_log_del_object_order(self, private, obj_id)

    # log unsaved
    i_log_unsaved_idf(self, private)

    # log uuid
    i_log_new_uuid(self, private)

    self
}
# }}}

# i_search_value {{{
i_search_value = function (self, private, pattern, class = NULL) {
    val <- private$m_idf_tbl$value[stringr::str_detect(value, pattern)]

    if (!is.null(class)) {
        i_assert_valid_class_name(self, private, class, "idf")
        cls_in <- i_in_tbl_from_which(self, private, "class", class, "idf")
        val <- val[cls_in, on = "class_id", nomatch = 0L]
    }

    if (is_empty(val)) {
        message("No matched result found.")
        return(invisible())
    }

    value_tbl <- val[
        private$m_idf_tbl$object, on = "object_id", nomatch = 0L][
        private$m_idd_tbl$class, on = "class_id", nomatch = 0L][
        private$m_idd_tbl$field, on = "field_id", nomatch = 0L]

    i_verbose_info(self, private, nrow(val), " results found in class",
        collapse(unique(value_tbl$class_name)), ".")

    cli::cat_line(format_objects(value_tbl, in_ip = eplusr_option("view_in_ip")))

    return(invisible(i_idfobject(self, private, unique(val$object_id))))
}
# }}}

# i_replace_value {{{
i_replace_value = function (self, private, pattern, replacement, class = NULL) {
    val_before <- private$m_idf_tbl$value[stringr::str_detect(value, pattern)]

    if (!is.null(class)) {
        i_assert_valid_class_name(self, private, class, "idf")
        cls_in <- i_in_tbl_from_which(self, private, "class", class, "idf")
        val_before <- val_before[cls_in, on = "class_id", nomatch = 0L]
    }

    if (is_empty(val_before)) {
        message("No matched result found.")
        return(invisible())
    }

    val_after <- data.table::copy(val_before)[,
        value := stringr::str_replace_all(value, pattern, replacement)]

    value_tbl_before <- val_before[
        private$m_idf_tbl$object, on = "object_id", nomatch = 0L][
        private$m_idd_tbl$class, on = "class_id", nomatch = 0L][
        private$m_idd_tbl$field, on = "field_id", nomatch = 0L][,
        `:=`(field_index = paste0(lpad(field_index, "0"), "(before)"))]

    value_tbl_after <- val_after[
        private$m_idf_tbl$object, on = "object_id", nomatch = 0L][
        private$m_idd_tbl$class, on = "class_id", nomatch = 0L][
        private$m_idd_tbl$field, on = "field_id", nomatch = 0L][,
        `:=`(field_index = paste0(lpad(field_index, "0"), "(after) "))]

    value_tbl <- data.table::rbindlist(list(value_tbl_before, value_tbl_after))

    value_tbl_after[, `:=`(
        value_upper = toupper(value),
        value_num = suppressWarnings(as.numeric(value)),
        value_ipnum = suppressWarnings(as.numeric(value)))]

    value_tbl_new <- update_value_num(
        value_tbl_after,
        eplusr_option("num_digits"),
        eplusr_option("view_in_ip"))

    # update object name if necessary
    if (not_empty(value_tbl_new[is_name == TRUE])) {
        obj_tbl_new <- value_tbl_new[is_name == TRUE, list(object_id, value, value_upper)]
        private$m_idf_tbl$object <- obj_tbl_new[private$m_idf_tbl$object, on = "object_id"][
            !is.na(value), `:=`(object_name = value, object_name_upper = value_upper)][
            , `:=`(value = NULL, value_upper = NULL)]
    }

    # remove unuseful columns and rename column
    value_tbl_rep <- value_tbl_new[,
        list(value_id, new_value = value, new_value_upper = value_upper,
            new_value_num = value_num, new_value_ipnum = value_ipnum)]

    private$m_idf_tbl$value <- value_tbl_rep[private$m_idf_tbl$value, on = "value_id"][
        !is.na(new_value), `:=`(value = new_value, value_upper = new_value_upper,
            value_num = new_value_num, value_ipnum = new_value_ipnum)][
        , `:=`(new_value = NULL, new_value_upper = NULL, new_value_num = NULL, new_value_ipnum = NULL)]

    private$m_log$order[object_id %in% value_tbl_rep$object_id,
                        object_order := object_order + 1L]

    # log uuid
    i_log_new_uuid(self, private)

    cli::cat_line(format_objects(value_tbl, in_ip = eplusr_option("view_in_ip")))

    return(invisible(i_idfobject(self, private, unique(value_tbl_new$object_id))))
}
# }}}

# i_object_string {{{
i_object_string <- function (self, private, object = NULL, comment = TRUE, ...) {
    # get value tbl
    val_tbl <- i_value_tbl_from_which(self, private, object)

    # add class name column
    val_tbl[, `:=`(class_name = i_class_name(self, private, class_id))]

    # add order column
    val_tbl <- val_tbl[private$m_log$order, on = "object_id", nomatch = 0L]

    # get comment tbl
    cmt_tbl <- NULL
    if (comment)
        cmt_tbl <- i_comment_tbl_from_which(self, private)

    main <- format_output(val_tbl, cmt_tbl, ...)
    # keep empty lines
    main[main == ""] <- "\n"
    main <- unlist(strsplit(main, "\n", fixed = TRUE))

    # add a blank line at the end like IDFEditor
    crayon::strip_style(c(main, ""))
}
# }}}

# i_deep_clone {{{
i_deep_clone <- function (self, private, name, value, type) {
    env_shared <- name_env_shared(type)

    # normal_copy {{{
    normal_copy <- function (name, value) {
        if (inherits(value, "R6")) {
            value$clone(deep = TRUE)
        } else if (is.environment(value)) {
            list2env(as.list.environment(value, all.names = TRUE), parent = emptyenv())
        } else {
            value
        }
    }
    # }}}

    # copy_to_globals {{{
    copy_to_globals <- function (name, value) {
        if (name %in% env_shared) {
            if (!.globals$is_env_cloned[[type]][[name]]) {
                val <- normal_copy(name, value)
                .globals$env_cloned[[type]][[name]] <- val
                .globals$is_env_cloned[[type]][[name]] <- TRUE
            } else {
                val <- .globals$env_cloned[[type]][[name]]
            }
            val
        }
    }
    # }}}

    # assign_shared {{{
    assign_shared <- function (name, gen) {
        for (nm in env_shared) {
            gen$self$private_fields[[nm]] <- .globals$env_cloned[[type]][[nm]]
            gen$private_fields[[nm]] <- .globals$env_cloned[[type]][[nm]]
        }

        # self reference
        gen$self$private_fields[[name]] <- gen
        gen$self$self$private_fields[[name]] <- gen

        gen
    }
    # }}}

    # done_copy {{{
    done_copy <- function () {
        all(unlist(.globals$is_env_cloned[[type]]), unlist(.globals$is_gen_cloned[[type]]))
    }
    # }}}

    # done_assign {{{
    done_assign <- function () {
        all(unlist(.globals$is_env_assigned[[type]]))
    }
    # }}}

    if (inherits(value, "R6ClassGenerator")) {
        copied <- unlist(.globals$is_env_cloned[[type]], use.names = TRUE)
        if (any(!copied)) {
            for (nm in names(!copied)) {
                copy_to_globals(nm, private[[nm]])
            }
            i_deep_clone(self, private, name, value, type)
        } else {
            # clone the R6ClassGenerator
            value <- clone_generator(value)
            value <- assign_shared(name, value)
            .globals$is_gen_cloned[[type]][[name]] <- TRUE
            if (done_copy() && done_assign()) reset_clone_indicator()
            value
        }
    } else {
        if (name %in% env_shared) {
            value <- copy_to_globals(name, value)
            .globals$is_env_assigned[[type]][[name]] <- TRUE
            if (done_copy() && done_assign()) reset_clone_indicator()
            value
        } else {
            normal_copy(name, value)
        }
    }
}
# }}}

# i_assign_object_tbl {{{
i_assign_object_tbl <- function (self, private, new_tbl) {
    assert_that(has_names(new_tbl, names(private$m_idf_tbl$object)))

    private$m_idf_tbl$object <- data.table::rbindlist(list(
        private$m_idf_tbl$object[!object_id %in% new_tbl$object_id],
        new_tbl[, .SD, .SDcols = names(private$m_idf_tbl$object)]),
        use.names = TRUE)
}
# }}}

################################################################################
#                                    Value                                     #
################################################################################

# i_assert_valid_input_format {{{
i_assert_valid_input_format <- function (class_name, value, comment, default, type = c("add", "set")) {
    type <- match.arg(type)
    key <- switch(type, add = "class", set = "object")

    is_valid_input <- function (x) is.null(x) || is_normal_list(x)

    if (length(class_name) > 1L &&
        ((not_empty(value)   && !is_same_len(class_name, value)) ||
         (not_empty(comment) && !is_same_len(class_name, comment))))
        stop("`value` and `comment` should have the same length as ",
            surround(key), ".", call. = FALSE)

    if (is_scalar(class_name)) {
        if (!is_valid_input(value) || !is_valid_input(comment)) {
            stop("Invalid `value` or `comment` format found. Each value or ",
                "comment for a single object should be NULL or a list in format ",
                "'list(a = 1, b = 2)' or 'list(1, 2)'.", call. = FALSE)
        }
    } else {
        if (!all(vapply(value, is_valid_input, logical(1))) ||
            !all(vapply(comment, is_valid_input, logical(1)))) {
            stop("Invalid `value` or `comment` format found. Each value or ",
                "comment for a single object should be NULL or a list in format ",
                "'list(a = 1, b = 2)' or 'list(1, 2)'.", call. = FALSE)
        }
    }
}
# }}}

# i_valid_value_input {{{
i_valid_value_input <- function (self, private, object_tbl, value, default = TRUE, all = FALSE, type = c("add", "set")) {
    type <- match.arg(type)

    val_in <- i_val_in_tbl(self, private, object_tbl$object_id, object_tbl$class_id, value)

    # divide input into empty, named and unnamed
    val_in_empty <- val_in[empty == TRUE]
    val_in_unnamed <- val_in[empty == FALSE & is.na(field_name_in)]
    val_in_named <- val_in[empty == FALSE & !is.na(field_name_in)]

    # direct return if null
    no_empty <- no_unnamed <- no_named <- FALSE
    if (is_empty(val_in_empty))   {no_empty <- TRUE;   val_tbl_empty <- list()}
    if (is_empty(val_in_unnamed)) {no_unnamed <- TRUE; val_tbl_unnamed <- list()}
    if (is_empty(val_in_named))   {no_named <- TRUE;   val_tbl_named <- list()}

    # i_count_value_in_num: helper to count value in number {{{
    i_count_value_in_num <- function (object_tbl, value_in_tbl) {
       value_in_tbl[, list(value_num = .N),
            by = list(object_rleid, object_id, class_id, class_name_in)][
            object_tbl, on = c("object_rleid", "object_id", "class_id"), nomatch = 0L]
    }
    # }}}

    # i_value_tbl_ori_from_num_tbl: helper to get value tbl ori {{{
    i_value_tbl_ori_from_num_tbl <- function (self, private, value_num_tbl, type = c("add", "set"), default, all = FALSE) {
        if (type == "add") {

            if (all)
                val_tbl_ori <- i_empty_value_tbl(self, private,
                    object_id = value_num_tbl$object_id,
                    class = value_num_tbl$class_id,
                    default = default,
                    num = value_num_tbl[, ifelse(num_fields > value_num, num_fields, value_num)])
            else
                val_tbl_ori <- i_empty_value_tbl(self, private,
                    object_id = value_num_tbl$object_id,
                    class = value_num_tbl$class_id,
                    default = default,
                    num = value_num_tbl$value_num)

        } else {

            if (all)
                val_tbl_ori <- i_value_tbl_from_num(self, private,
                    object = value_num_tbl$object_id,
                    num = value_num_tbl[, ifelse(num_fields > value_num, num_fields, value_num)])
            else
                val_tbl_ori <- i_value_tbl_from_num(self, private,
                    object = value_num_tbl$object_id,
                    num = value_num_tbl$value_num)

        }

        val_tbl_ori
    }
    # }}}

    # stop adding empty objects in `final` validation level {{{
    if (type == "add" && eplusr_option("validate_level") == "final" && !no_empty) {
        msg <- val_in_empty[, paste0("  ", lpad(object_rleid), "| Class ",
            surround(class_name_in), ".", collapse = "\n")]
        stop("Adding empty objects is prohibited in `final` validation ",
            "level. Empty value found in input `value`:\n", msg, call. = FALSE)
    }
    # TODO: stop when both value and comment is empty in `$set_object()`
    # if (type == "set" && !default && !no_empty) {
    #     msg <- val_in_empty[, paste0("  ", lpad(class_rleid), "| Class ",
    #         surround(class_name_in), ".", collapse = "\n")]
    #     stop("Adding empty objects is prohibited in `final` validation ",
    #         "level. Missing values found in input `value`:\n", msg, call. = FALSE)
    # }
    # }}}

    # EMPTY CASE {{{
    if (!no_empty) {
        val_num_empty <- i_count_value_in_num(object_tbl, val_in_empty)

        if (type == "add") {

            val_tbl_empty_ori <- i_value_tbl_ori_from_num_tbl(self, private,
                val_num_empty, type, default = FALSE, all)

        } else if (type == "set") {

            # only make sense when `default` is TRUE
            if (default) {

                # get all existing values
                val_tbl_empty_ori <- i_value_tbl_from_which(self, private,
                    object = val_num_empty$object_id)
            }
        }

        # combine
        val_tbl_empty_ori <- i_del_rleid_column(val_tbl_empty_ori)
        val_tbl_empty <- val_tbl_empty_ori[val_in_empty, on = c("object_id", "class_id")]
    }
    # }}}

    # UNNAMED CASE {{{
    if (!no_unnamed) {
        val_num_unnamed <- i_count_value_in_num(object_tbl, val_in_unnamed)

        # get input file index
        val_in_unnamed[, `:=`(field_name_in = NULL)]
        val_in_unnamed[, `:=`(field_index = seq_along(value_in)), by = object_rleid]

        if (!default) {
            invalid_num_unnamed <- val_num_unnamed[
                !i_is_valid_field_num(self, private, class = class_name, num = value_num)
            ]
        } else {
            invalid_num_unnamed_more <- val_num_unnamed[value_num >= min_fields &
                !i_is_valid_field_num(self, private, class = class_name, num = value_num)
            ]

            less_num_unnamed <- val_num_unnamed[value_num < min_fields][, class_rleid := .I]

            if (type == "add") {
                invalid_num_obj_rleid <- less_num_unnamed[,
                    list(object_rleid, class_rleid, value_count = value_num, min_fields)][
                    i_value_tbl_ori_from_num_tbl(self, private, less_num_unnamed,
                        type, default = FALSE, all),
                    on = list(class_rleid, value_count < field_index, min_fields >= field_index),
                    nomatch = 0L][has_default == FALSE, unique(object_rleid)]
            } else {
                invalid_num_obj_rleid <- less_num_unnamed[,
                    list(object_rleid, class_rleid, value_count = value_num, min_fields)][
                    i_value_tbl_ori_from_num_tbl(self, private, less_num_unnamed,
                        type, default = FALSE, all),
                    on = list(object_rleid, value_count < field_index, min_fields >= field_index),
                    nomatch = 0L][has_default == FALSE, unique(object_rleid)]
            }

            invalid_num_unnamed_less <- val_num_unnamed[object_rleid %in% invalid_num_obj_rleid]

            invalid_num_unnamed <- data.table::rbindlist(list(
                invalid_num_unnamed_more, invalid_num_unnamed_less))

        }

        if (not_empty(invalid_num_unnamed)) {
            msg <- i_msg_invalid_value_num(invalid_num_unnamed, type)
            stop("Invalid field number found:\n", msg, call. = FALSE)
        }

        val_tbl_unnamed_ori <- i_value_tbl_ori_from_num_tbl(self, private,
            val_num_unnamed, type, default = FALSE, all)

        # combine
        val_tbl_unnamed_ori <- i_del_rleid_column(val_tbl_unnamed_ori)
        val_tbl_unnamed <- val_in_unnamed[val_tbl_unnamed_ori, on = c("object_id", "class_id", "field_index")]
    }
    # }}}

    # NAMED CASE {{{
    if (!no_named) {
        val_num_named <- i_count_value_in_num(object_tbl, val_in_named)

        val_in_named[, `:=`(all_field_name_lower = i_class_field_name(self, private, class_id, lower = TRUE))]

        val_in_named[, `:=`(field_index = apply2_int(field_name_lower, all_field_name_lower, match))]

        # check field names
        mis_val_in <- val_in_named[is.na(field_index)]
        if (not_empty(mis_val_in)) {

            # check non-extensible class
            mis_val_in_nonext <- mis_val_in[!class_name_in %in% i_extensible_class_name(self, private)]
            if (not_empty(mis_val_in_nonext)) {
                data.table::setnames(mis_val_in_nonext, "class_name_in", "class_name")
                msg <- i_msg_invalid_value_name(mis_val_in_nonext, "add")
                stop("Invalid field name found:\n", msg, call. = FALSE)
            }

            # get object tbl for mismatched
            mis_obj_tbl <- val_num_named[J(unique(mis_val_in$object_rleid)), on = "object_rleid"]

            # get possible-extensible field value number per object
            mis_obj_tbl <- mis_obj_tbl[
                mis_val_in[, list(mis_val_num = .N), by = list(object_rleid)],
                on = "object_rleid"]

            # check if value number is acceptable
            mis_obj_tbl[, `:=`(new_ext_num = mis_val_num %/% num_extensible,
                               is_complete = (mis_val_num %% num_extensible) == 0L)]

            if (not_empty(mis_obj_tbl[is_complete == FALSE])) {
                incomp_ext_grp <- mis_val_in[J(mis_obj_tbl[is_complete == FALSE, object_rleid]), on = "object_rleid"]
                data.table::setnames(incomp_ext_grp, "class_name_in", "class_name")
                msg <- i_msg_invalid_ext(incomp_ext_grp, type)
                stop("Incomplete extensible group or invalid field name of ",
                    "extensible group found:\n", msg, call. = FALSE)
            }

            # add extensible group
            ext_in <- mis_obj_tbl[, list(new_ext_num = max(new_ext_num)), by = list(class_id)]

            apply2(ext_in$class_id, ext_in$new_ext_num, i_add_extensible_group,
                list(self = self, private = private))

            # match field names again
            val_in_named[is.na(field_index), `:=`(all_field_name_lower =
                i_class_field_name(self, private, class_id, lower = TRUE))]
            val_in_named[is.na(field_index), `:=`(field_index =
                apply2_int(field_name_lower, all_field_name_lower, match))]
            invalid_ext <- val_in_named[is.na(field_index)]
            if (not_empty(invalid_ext)) {
                # delete extensible groups for invalid input
                valid_obj_id <- val_in_named[!is.na(field_index),
                    setdiff(object_rleid, invalid_ext$object_rleid)]
                if (not_empty(valid_obj_id)) {
                    valid_ext_in <- mis_obj_tbl[object_rleid %in% valid_obj_id, list(class_id, new_ext_num)][
                        , list(valid_ext_num = max(new_ext_num)), by = list(class_id)]
                    invalid_ext_in <- valid_ext_in[ext_in, on = "class_id"][
                        , `:=`(invalid_ext_num = new_ext_num - valid_ext_num)][
                        invalid_ext_num > 0L]
                } else {
                    invalid_ext_in <- ext_in[, `:=`(invalid_ext_num = new_ext_num)]
                }
                apply2(invalid_ext_in$class_id, invalid_ext_in$invalid_ext_num,
                    i_del_extensible_group, list(self = self, private = private))

                data.table::setnames(invalid_ext, "class_name_in", "class_name")
                msg <- i_msg_invalid_ext(invalid_ext, type)
                stop("Incomplete extensible group or invalid field name of ",
                     "extensible group found:\n", msg, call. = FALSE)
            }
        }

        val_in_named[, `:=`(all_field_name_lower = NULL)]

        if (type == "set") val_num_named[, `:=`(value_num = i_value_num(self, private, object_id))]

        val_num_named <- val_num_named[val_in_named[, list(last_index = max(field_index)), by = list(object_rleid)],
            on = "object_rleid"]

        val_num_named[value_num < last_index, `:=`(value_num = last_index)]

        val_tbl_named_ori <- i_value_tbl_ori_from_num_tbl(self, private,
            val_num_named, type, default = FALSE, all)

        val_tbl_named_ori <- i_del_rleid_column(val_tbl_named_ori)

        val_tbl_named <- val_in_named[val_tbl_named_ori, on = c("object_id", "class_id", "field_index")]
    }
    # }}}

    # combine all
    val_tbl <- data.table::rbindlist(list(
        val_tbl_empty, val_tbl_unnamed, val_tbl_named), fill = TRUE)

    # fill indicators for nomatch fields
    val_tbl[is.na(empty), `:=`(empty = FALSE, delete = FALSE)]

    # assign values and defaults
    val_tbl[delete == FALSE & !is.na(value_in) &
        vapply(value_list, is.numeric, logical(1)),
        `:=`(value_num = as.numeric(unlist(value_list)))]

    val_tbl[delete == FALSE & !is.na(value_in),
        `:=`(value = value_in, value_upper = toupper(value_in),
             value_ipnum = value_num)]

    val_tbl[delete == TRUE & is.na(value_in),
        `:=`(value = NA_character_, value_upper = NA_character_,
             value_num = NA_real_, value_ipnum = NA_real_)]

    val_tbl[, `:=`(is_default = FALSE)]
    if (default) {
        if (type == "add") val_tbl[, `:=`(delete = TRUE)]

        val_tbl <- i_value_tbl_assign_default(self, private, val_tbl)
    }

    # update value table number
    # fill object rleid
    data.table::setorder(val_tbl, -object_id, -field_index)
    val_tbl[order(object_id), `:=`(object_rleid = object_rleid[1L]),
        by = list(cumsum(!is.na(object_rleid)), object_id)]
    data.table::setorder(val_tbl, object_id, field_index)
    val_tbl[order(object_id), `:=`(object_rleid = object_rleid[1L]),
        by = list(cumsum(!is.na(object_rleid)), object_id)]

    val_tbl <- update_value_num(val_tbl, digits = eplusr_option("num_digits"),
        in_ip = eplusr_option("view_in_ip"), prefix = "value")
    data.table::setorder(val_tbl, object_rleid, field_index)

    # add class name
    val_tbl <- object_tbl[, .SD, .SDcols = c("object_rleid", "class_name")][
        val_tbl, on = "object_rleid"]

    # clean up
    fld_cols <- intersect(names(val_tbl), names(private$m_idd_tbl$field))
    val_cols <- intersect(names(val_tbl), names(private$m_idf_tbl$value))
    misc_cols <- c("object_rleid", "is_default", "class_name")
    val_tbl[, .SD, .SDcols = unique(c(misc_cols, val_cols, fld_cols))]
}
# }}}

# i_value_tbl_from_which {{{
i_value_tbl_from_which <- function (self, private, object = NULL, field = TRUE, allow_cartesian = FALSE) {
    if (is.null(object)) {
        if (field)
            return(private$m_idd_tbl$field[private$m_idf_tbl$value, on = "field_id"])
        else
            return(private$m_idf_tbl$value)
    }

    in_tbl <- i_in_tbl_from_which(self, private, "object", object)

    val_tbl <- private$m_idf_tbl$value[in_tbl, on = "object_id", allow.cartesian = allow_cartesian]

    if (!field) return(val_tbl)

    fld_tbl <- i_field_tbl(self, private, in_tbl$class_id)
    data.table::setnames(fld_tbl, "class_rleid", "object_rleid")

    tbl <- fld_tbl[val_tbl, on = c("object_rleid", "class_id", "field_id")]

    data.table::setorderv(tbl, c("object_rleid", "field_id"))

    tbl
}
# }}}

# i_value_tbl_from_num {{{
i_value_tbl_from_num <- function (self, private, object, num = 0L) {
    assert_that(are_count(num))
    assert_that(is_same_len(object, num))

    val_tbl <- i_value_tbl_from_which(self, private, object, field = FALSE)

    val_num_tbl <- val_tbl[, list(value_num = .N),
        by = c("object_rleid", "object_id", "class_id")][, `:=`(num = num)]
    val_num_tbl[value_num > num, `:=`(num = value_num)]

    fld_tbl <- i_field_tbl_from_num(self, private, val_num_tbl$class_id, val_num_tbl$num)

    tbl <- val_tbl[fld_tbl, on = c(object_rleid = "class_rleid", "class_id", "field_id")]
    tbl[, `:=`(object_id = object_id[1]), by = list(cumsum(!is.na(object_id)))]
    tbl
}
# }}}

# i_value_tbl_assign_default {{{
i_value_tbl_assign_default <- function (self, private, value_tbl) {
    if (!has_name(value_tbl, "delete")) return(value_tbl)

    if (!has_name(value_tbl, "is_default"))
        value_tbl[, `:=`(is_default = FALSE)]

    if (!has_name(value_tbl, "value_in"))
        value_tbl[, `:=`(value_in = NA_character_)]

    tbl <- private$m_idd_tbl$field_default[value_tbl, on = "field_id"]

    tbl[is.na(value) & is.na(value_in) & delete == TRUE & !is.na(default_id), `:=`(
        is_default = TRUE,
        value = default, value_upper = default_upper,
        value_num = default_num, value_ipnum = default_ipnum)]

    tbl[, c("default_id", "default", "default_upper", "default_num",
        "default_ipnum") := NULL]

    tbl
}
# }}}

# i_empty_value_tbl {{{
i_empty_value_tbl <- function (self, private, object_id, class, default = TRUE, num = 0L) {
    fld_tbl <- i_field_tbl_from_num(self, private, class, num)

    # add object id
    obj_cls <- data.table::data.table(object_id = object_id, class_rleid = seq_along(class))
    fld_tbl <- fld_tbl[obj_cls, on = "class_rleid"]

    # mark fields that use default values
    fld_tbl[, is_default := FALSE]
    if (default) {
        fld_tbl <- private$m_idd_tbl$field_default[fld_tbl, on = "field_id"]
        fld_tbl[!is.na(default_id), is_default := TRUE]
        fld_tbl[, `:=`(default_id = NULL)]
        data.table::setnames(fld_tbl,
            paste0("default", c("", "_upper", "_num", "_ipnum")),
            paste0("value",   c("", "_upper", "_num", "_ipnum")))
    } else {
        fld_tbl[, `:=`(value = NA_character_, value_upper = NA_character_,
                       value_num = NA_real_, value_ipnum = NA_real_)]
    }

    fld_tbl
}
# }}}

# i_val_in_tbl {{{
i_val_in_tbl <- function (self, private, object_id, class_id, value) {
    replace_null <- function (val) if (is.null(val)) NA_character_ else val
    has_empty_name <- function (name) any(name == "")
    has_duplicated <- function (name) as.logical(anyDuplicated(name))

    # get input class names
    cls_id <- class_id
    class_name_in <- i_class_name(self, private, cls_id)

    if (is.null(value)) {
        empty <- TRUE
        val_list <- NA_character_
        val_chr <- NA_character_
        field_name_in <- NA_character_
        field_name_lower <- NA_character_
        delete <- FALSE
    } else {
        # get input value list
        val_list <- value
        if (vec_depth(value) == 3L) val_list <- Reduce(c, val_list)

        if (is_scalar(object_id)) {
            # check if there are empty values
            empty <- list(is_empty(value))

            field_name_in <- list(replace_null(names(value)))
            field_name_lower <- list(lower_name(unlist(field_name_in)))

            val_chr <- list(vapply(value, as.character, character(1)))

            delete <- list(vapply(value, is.na, logical(1)))
        } else {
            # check if there are empty values
            empty <- vapply(value, is_empty, logical(1))

            field_name_in <- lapply(value, function (val) replace_null(names(val)))
            field_name_lower <- lapply(field_name_in, lower_name)

            val_chr <- apply2(value, empty, function (x, y) {
                if (!isTRUE(y)) {
                    lapply(x, function (x) as.character(replace_null(x)))
                } else {
                    NA_character_
                }
            })

            delete <- apply2(value, empty, function (x, y) {
                if (!isTRUE(y)) {
                    vapply(x, is.na, logical(1))
                } else {
                    FALSE
                }
            })
        }

    }

    # combine all
    tbl_nest <- data.table::data.table(value_in = val_chr, object_id = object_id,
        class_rleid = seq_along(class_id), class_id = class_id,
        class_name_in = class_name_in, field_name_in = field_name_in,
        field_name_lower = field_name_lower, empty = empty, delete = delete)

    # check if there are non-empty value list with empty name
    mix_named <- tbl_nest[
        vapply(value_in, not_empty, logical(1)) &
        vapply(field_name_in, has_empty_name, logical(1)), .SD, by = object_id]
    if (not_empty(mix_named))
        stop("Values should be either all unnamed or all named.", call. = FALSE)

    # check duplication of names
    dup_name <- tbl_nest[vapply(field_name_lower, has_duplicated, logical(1)),
        .SD, by = object_id]
    if (not_empty(dup_name)) {
        dup_name[, `:=`(duplicated_name =
            lapply(field_name_in, function (nm) nm[duplicated(nm)]))]
        msg <- dup_name[, paste0("  ", lpad(class_rleid),
            "| Class ", surround(class_name_in), ":",
            collapse(as.character(duplicated_name)), ".", collapse = "\n")]
        stop("Duplicated field names found in input:\n", msg, call. = FALSE)
    }

    tbl <- tbl_nest[, lapply(.SD, unlist), by = object_id]
    tbl[is.na(delete), `:=`(delete = FALSE)]

    # add columns of lower format field name and value list
    tbl[, `:=`(field_name_lower = tolower(gsub("[- :]", "_", field_name_in)),
               value_list = val_list)]

    # replace empty string, i.e. "''" with NA
    tbl[grepl("^\\s*$", value_in), `:=`(value_in = NA_character_)]

    data.table::setnames(tbl, "class_rleid", "object_rleid")

    tbl
}
# }}}

# i_value_num {{{
i_value_num <- function (self, private, which = NULL) {
    if (is.null(which)) return(nrow(private$m_idf_tbl$value))

    obj_in <- i_in_tbl_from_which(self, private, "object", which)

    private$m_idf_tbl$value[obj_in, on = "object_id", allow.cartesian = TRUE][,
        .N, by = list(object_rleid, found = !is.na(value_id))][
        found == FALSE, `:=`(N = 0L)]$N
}
# }}}

# i_assign_value_tbl {{{
i_assign_value_tbl <- function (self, private, new_tbl) {
    assert_that(has_names(new_tbl, names(private$m_idf_tbl$value)))

    private$m_idf_tbl$value <- data.table::rbindlist(list(
        private$m_idf_tbl$value[!object_id %in% unique(new_tbl$object_id)],
        new_tbl[, .SD, .SDcols = names(private$m_idf_tbl$value)]), use.names = TRUE)
}
# }}}

# i_update_value_reference_from_id {{{
i_update_value_reference_from_id <- function (self, private, old_id, new_id) {
    d <- data.table::data.table(value_id = old_id, new_id = new_id)

    # get old reference table and assign new value id
    new_val_ref <- private$m_idf_tbl$value_reference[d, on = "value_id", nomatch = 0L][
        !is.na(reference_value_id), `:=`(value_id = new_id)][, `:=`(new_id = NULL)]

    if (is_empty(new_val_ref)) return()
    private$m_idf_tbl$value_reference <- data.table::rbindlist(list(
        private$m_idf_tbl$value_reference, new_val_ref), use.names = TRUE)
}
# }}}

# i_update_value_reference_from_tbl {{{
i_update_value_reference_from_tbl <- function (self, private, value_tbl) {
    # delete references to empty fields
    empty_id <- value_tbl[is.na(value), value_id]
    if (not_empty(empty_id)) {
        private$m_idf_tbl$value_reference <- private$m_idf_tbl$value_reference[
            !(reference_value_id %in% empty_id | value_id %in% empty_id)]
    }

    value_tbl <- value_tbl[!value_id %in% empty_id]

    # get object-list fields
    val_tbl_obj <- value_tbl[has_object_list == TRUE]

    # get reference fields
    val_tbl_ref <- value_tbl[has_reference == TRUE]

    if (is_empty(val_tbl_obj) && is_empty(val_tbl_ref)) return()

    # update value reference table
    if (not_empty(val_tbl_obj)) {
        # get values that may possiblely reference other fields
        val_obj_tbl <- value_tbl[private$m_idd_tbl$field_object_list,
            on = "field_id", nomatch = 0L, list(value_id, value_upper, object_list)]

        # get all possible values to reference
        all_val_ref <- private$m_idf_tbl$value[private$m_idd_tbl$field_reference,
            on = "field_id", nomatch = 0L, list(value_id, value_upper, reference)]
        data.table::setnames(all_val_ref, "value_id", "reference_value_id")

        # find references
        val_ref_new <- unique(val_obj_tbl[all_val_ref,
            on = c(object_list = "reference", "value_upper"),
            nomatch = 0L, list(value_id, reference_value_id)])
        data.table::setorder(val_ref_new, value_id)

        if (not_empty(val_ref_new)) {
            private$m_idf_tbl$value_reference <- data.table::rbindlist(list(
                private$m_idf_tbl$value_reference[!value_id %in% val_ref_new$value_id],
                val_ref_new), use.names = TRUE)

            data.table::setorderv(private$m_idf_tbl$value_reference, "value_id")
        }
    }

    # update values that reference the input
    if (not_empty(val_tbl_ref)) {
        val_ref_tbl <- unique(value_tbl[private$m_idd_tbl$field_reference,
            on = "field_id", nomatch = 0L,
            list(value_id, value, value_upper, value_num ,value_ipnum)])

        # find values that reference input before
        new_ref_tbl <- val_ref_tbl[private$m_idf_tbl$value_reference,
            on = c(value_id = "reference_value_id"), nomatch = 0L]

        data.table::setnames(new_ref_tbl, c("ref_value_id", "ref_value",
            "ref_value_upper", "ref_value_num", "ref_value_ipnum", "value_id"))

        new_val_tbl <- new_ref_tbl[private$m_idf_tbl$value, on = "value_id", nomatch = 0L][
            , c("ref_value_id", "value", "value_upper", "value_num", "value_ipnum") := NULL]

        data.table::setnames(new_val_tbl,
            c("ref_value", "ref_value_upper", "ref_value_num", "ref_value_ipnum"),
            c("value", "value_upper", "value_num", "value_ipnum"))

        # assign new values
        private$m_idf_tbl$value <- data.table::rbindlist(list(
            private$m_idf_tbl$value[!new_val_tbl, on = "value_id"], new_val_tbl),
            use.names = TRUE)
        data.table::setorder(private$m_idf_tbl$value, object_id, field_id)

        # log new order
        i_log_add_object_order(self, private, unique(new_val_tbl$object_id))
    }
}
# }}}

# i_value_tbl_from_object_list {{{
i_value_tbl_from_object_list <- function (self, private, object_list) {
    i_assert_valid_object_list(self, private, object_list)

    obj_list_in <- data.table::data.table(
        object_list = object_list, object_list_rleid = seq_along(object_list))

    if (is_empty(private$m_idd_tbl$class_reference)) {
        cls_val <- list()
    } else {
        cls_val <- private$m_idd_tbl$class_reference[obj_list_in,
            on = c(reference = "object_list")][
            i_is_valid_class_index(self, private, class_id, type = "idf")][,
            `:=`(value = i_class_name(self, private, class_id))][,
            `:=`(value_upper = toupper(value))][,
            lapply(.SD, list), .SDcols = c("value", "value_upper"),
            by = list(object_list_rleid, reference)]
    }

    fld_val <- private$m_idd_tbl$field_reference[obj_list_in,
        on = c(reference = "object_list")][private$m_idf_tbl$value,
        on = "field_id", nomatch = 0L][,
        lapply(.SD, list), .SDcols = c("value", "value_upper"),
        by = list(object_list_rleid, reference)]

    val <- data.table::rbindlist(list(cls_val, fld_val), use.names = TRUE)

    # in case there are some object-list that points to both class names and
    # field values
    bi_ref <- intersect(cls_val$object_list_rleid, fld_val$object_list_rleid)
    if (not_empty(bi_ref)) {
        val <- val[
            , lapply(.SD, unlist), by = list(object_list_rleid, reference)][
            , lapply(.SD, list), by = list(object_list_rleid, reference)
        ]
    }

    val <- val[obj_list_in, on = "object_list_rleid"][, `:=`(reference = NULL)]
    data.table::setcolorder(val, c("object_list_rleid", "object_list", "value", "value_upper"))
    data.table::setnames(val, c("object_list_rleid", "object_list",
        "possible_value", "possible_value_upper"))

    val
}
# }}}

# i_assert_valid_object_list {{{
i_assert_valid_object_list <- function (self, private, object_list) {
    valid <- object_list %in% private$m_idd_tbl$field_object_list$object_list
    if (any(!valid))
        stop("Invalid object-list string found:",
            collapse(reference[!valid]), ".", call. = FALSE)
}
# }}}

# i_assert_valid_reference {{{
i_assert_valid_reference <- function (self, private, reference) {
    # check if valid class reference
    valid_cls_ref <- reference %in% private$m_idd_tbl$class_reference$reference

    # check if valid field reference
    valid_fld_ref <- reference %in% private$m_idd_tbl$field_reference$reference

    valid <- valid_cls_ref | valid_fld_ref

    if (any(!valid))
        stop("Invalid reference string found:",
            collapse(reference[!valid]), ".", call. = FALSE)
}
# }}}

# i_val_ref_by_tbl {{{
i_val_ref_by_tbl <- function (self, private, value_tbl) {
    ref_val_tbl <- private$m_idf_tbl$value_reference[value_tbl,
        on = c(reference_value_id = "value_id"), nomatch = 0L]

    data.table::setnames(ref_val_tbl, "reference_value_id", "i.value_id")

    ref_by_val_tbl <- private$m_idf_tbl$value[ref_val_tbl, on = "value_id"]

    nms <- names(ref_by_val_tbl)
    ori_src_name <- nms[startsWith(nms, "i.")]
    new_src_name <- gsub("i.", "", ori_src_name, fixed = TRUE)
    ori_ref_by_name <- nms[!nms %in% c(ori_src_name, "class_id", "object_rleid")]
    new_ref_by_name <- paste0("referenced_by_", ori_ref_by_name)
    data.table::setnames(ref_by_val_tbl, ori_ref_by_name, new_ref_by_name)
    data.table::setnames(ref_by_val_tbl, ori_src_name, new_src_name)

    data.table::setcolorder(ref_by_val_tbl,
        c(new_src_name, "class_id", "object_rleid", new_ref_by_name))

    ref_by_val_tbl
}
# }}}

# i_val_ref_from_tbl {{{
i_val_ref_from_tbl <- function (self, private, value_tbl) {
    ref_val_tbl <- private$m_idf_tbl$value_reference[value_tbl,
        on = "value_id", nomatch = 0L]

    ref_from_val_tbl <- private$m_idf_tbl$value[ref_val_tbl,
        on = c(value_id = "reference_value_id")]

    nms <- names(ref_from_val_tbl)
    ori_src_name <- nms[startsWith(nms, "i.")]
    new_src_name <- gsub("^i.", "", ori_src_name)
    ori_ref_from_name <- nms[!nms %in% c(ori_src_name, "class_id", "object_rleid")]
    new_ref_from_name <- paste0("referencing_from_", ori_ref_from_name)

    data.table::setnames(ref_from_val_tbl, ori_ref_from_name, new_ref_from_name)
    data.table::setnames(ref_from_val_tbl, ori_src_name, new_src_name)

    data.table::setcolorder(ref_from_val_tbl,
        c(new_src_name, "class_id", "object_rleid", new_ref_from_name))

    ref_from_val_tbl
}
# }}}

# i_del_rleid_column {{{
i_del_rleid_column <- function (tbl) {
    if (has_name(tbl, "object_rleid")) tbl[, `:=`(object_rleid = NULL)]
    if (has_name(tbl, "class_rleid")) tbl[, `:=`(class_rleid = NULL)]
    tbl
}
# }}}

# i_msg_info {{{
i_msg_info <- function (msg_tbl, type = c("add", "set")) {
    msg_tbl[, idx := paste0(" ", lpad(object_rleid))]

    msg_tbl[, which := paste0("Class ", surround(class_name))]

    if (has_name(msg_tbl, "object_id")) msg_tbl[, object_id := as.character(object_id)]

    if (type == "add")
        msg_tbl[, `:=`(object_id = "[Temporary]")]
    else
        msg_tbl[, `:=`(object_id = surround(object_id))]

    msg_tbl[, which := paste0("Object ", object_id, " (", which, ")")]

    msg_tbl[, info := paste0(idx, "| ", which)]

    msg_tbl[, `:=`(idx = NULL, which = NULL)]

    msg_tbl
}
# }}}

# i_msg_invalid_value_name {{{
i_msg_invalid_value_name <- function (value_tbl, type = c("add", "set")) {
    assert_that(has_name(value_tbl, "field_name_in"))

    by_col <- c("object_rleid", "class_name")
    if (type == "set") by_col <- c(by_col, "object_id")

    msg_tbl <- value_tbl[, list(invalid_name = collapse(field_name_in)),
        by = c(by_col)]

    msg_tbl <- i_msg_info(msg_tbl, type)

    paste0(msg_tbl$info, ": ", msg_tbl$invalid_name, ".", collapse = "\n")
}
# }}}

# i_msg_invalid_value_num {{{
i_msg_invalid_value_num <- function (value_tbl, type = c("add", "set")) {
    assert_that(has_names(value_tbl, c("object_rleid", "class_name", "min_fields",
        "num_fields", "value_num", "num_extensible", "first_extensible")))

    msg_tbl <- i_msg_info(value_tbl, type)

    msg_tbl[, msg := paste0(info, ": ", rpad(value_num), ".")]

    msg_tbl[min_fields == 0L & num_extensible == 0L, msg := paste0(msg,
        " Total field number should be less than ", num_fields, ".")]
    msg_tbl[min_fields >  0L & num_extensible == 0L, msg := paste0(msg,
        " Total field number should be no less than ", min_fields,
        " and less than ", num_fields, ".")]
    msg_tbl[num_extensible >  0L, msg := paste0(msg,
        " Total field number should be ", first_extensible - 1L, " + N * ",
        num_extensible, ".")]

    paste0(msg_tbl$msg, collapse = "\n")
}
# }}}

# i_msg_invalid_ext {{{
i_msg_invalid_ext <- function (value_tbl, type = c("add", "set")) {
    assert_that(has_names(value_tbl, c("field_name_in")))

    by_col <- c("object_rleid", "class_name")
    if (type == "set") by_col <- c(by_col, "object_id")

    msg_tbl <- value_tbl[, list(invalid_ext = collapse(field_name_in)),
        by = c(by_col)]

    msg_tbl <- i_msg_info(msg_tbl, type)

    paste0(msg_tbl$info, ": ", msg_tbl$invalid_ext, ".", collapse = "\n")
}
# }}}

# i_value_tbl_from_field_which {{{
i_value_tbl_from_field_which <- function (self, private, object, which = NULL) {
    assert_that(is_scalar(object))

    obj_in <- i_in_tbl_from_which(self, private, "object", object)

    fld_tbl <- i_field_tbl_from_which(self, private, obj_in$class_id, which)

    val_tbl <- i_value_tbl_from_which(self, private, obj_in$object_id, field = FALSE)

    val_tbl[fld_tbl, on = c("class_id", "field_id")]
}
# }}}

# i_all_node_value {{{
i_all_node_value <- function (self, private) {
    i_value_tbl_from_which(self, private)[type == "node" & !is.na(value), unique(value)]
}
# }}}

################################################################################
#                                   Comment                                    #
################################################################################

# i_comment_tbl_from_which {{{
i_comment_tbl_from_which <- function (self, private, which = NULL, nomatch = NA) {
    if (is.null(which)) {
        # copy here
        cmt_tbl <- data.table::copy(private$m_idf_tbl$comment)
    } else {
        obj_in <- i_in_tbl_from_which(self, private, "object", which)
        cmt_tbl <- private$m_idf_tbl$comment[obj_in,
            on = "object_id", allow.cartesian = TRUE, nomatch = nomatch]
    }

    cmt_tbl
}
# }}}

# i_new_comment_tbl {{{
i_new_comment_tbl <- function (self, private, object_id, comment) {
    tbl_nest <- data.table::data.table(comment = comment, object_id = object_id)
    tbl <- tbl_nest[, lapply(.SD, unlist), .SDcols = "comment", by = object_id]
    tbl[, `:=`(comment_id = .I, type = 0L)]
    data.table::setcolorder(tbl, names(private$m_idf_tbl$comment))
}
# }}}

# i_comment_num {{{
i_comment_num <- function (self, private, which = NULL) {
    if (is.null(which)) return(nrow(private$m_idf_tbl$comment))

    obj_in <- i_in_tbl_from_which(self, private, "object", which)

    private$m_idf_tbl$comment[obj_in, on = "object_id", allow.cartesian = TRUE][,
        .N, by = list(object_rleid, found = !is.na(comment_id))][
        found == FALSE, `:=`(N = 0L)]$N
}
# }}}

# i_assign_comment_tbl {{{
i_assign_comment_tbl <- function (self, private, new_tbl, exclude = TRUE) {
    assert_that(has_names(new_tbl, names(private$m_idf_tbl$comment)))

    if (exclude)
        private$m_idf_tbl$comment <- data.table::rbindlist(list(
                private$m_idf_tbl$comment[!object_id %in% unique(new_tbl$object_id)],
                new_tbl[, .SD, .SDcols = names(private$m_idf_tbl$comment)]),
            use.names = TRUE)
    else
        private$m_idf_tbl$comment <- data.table::rbindlist(list(
                private$m_idf_tbl$comment,
                new_tbl[, .SD, .SDcols = names(private$m_idf_tbl$comment)]),
            use.names = TRUE)
}
# }}}

# i_remove_comment_from_which {{{
i_remove_comment_from_which <- function (self, private, object) {
    obj_id <- i_object_id_from_which(self, private, object)
    private$m_idf_tbl$comment <- private$m_idf_tbl$comment[
        !J(obj_id), on = "object_id"]
}
# }}}

################################################################################
#                                     Misc                                     #
################################################################################

# i_in_tbl_from_which {{{
i_in_tbl_from_which <- function (self, private, type = c("object", "class"), which,
                                 where = c("idd", "idf")) {
    type <- match.arg(type)
    where <- match.arg(where)

    if (is.null(which))
        id <- switch(type,
            object = private$m_idf_tbl$object$object_id,
            class = private$m_idd_tbl$class$class_id)
    else
        id <- switch(type,
            object = i_object_id_from_which(self, private, which),
            class = i_class_index_from_which(self, private, which, where))

    tbl <- data.table::data.table(id = id, rleid = seq_along(id))
    data.table::setnames(tbl, paste0(type, "_", names(tbl)))

    if (type == "object")
        tbl <- private$m_idf_tbl$object[, list(object_id, class_id)][
            tbl, on = "object_id"]

    tbl
}
# }}}

# i_new_id {{{
i_new_id <- function (self, private, name, num) {
    id_col <- paste0(sub("(object|value|comment|class|field)_", "", name), "_id")
    if (name %in% c(names(private$m_idf_tbl))) {
        current <- private$m_idf_tbl[[name]][[id_col]]
    } else if (name %in% c(names(private$m_idd_tbl))) {
        current <- private$m_idd_tbl[[name]][[id_col]]
    } else {
        stop("Invalid `name` found: ", surround(name), ".", call. = FALSE)
    }

    if (is_empty(current)) current <- 0L else current <- max(current)
    seq.int(current + 1L, length.out = num)
}
# }}}

# i_log_new_uuid {{{
i_log_new_uuid <- function (self, private) {
    private$m_log$uuid <- uuid::UUIDgenerate(use.time = TRUE)
}
# }}}

# i_log_new_object_order {{{
i_log_new_object_order <- function (self, private, id) {
    private$m_log$order <- data.table::rbindlist(
        list(private$m_log$order,
            data.table::data.table(object_id = id, object_order = 1L)),
        use.names = TRUE)
}
# }}}

# i_log_add_object_order {{{
i_log_add_object_order <- function (self, private, id) {
    private$m_log$order[object_id %in% id, `:=`(object_order = object_order + 1L)]
}
# }}}

# i_log_del_object_order {{{
i_log_del_object_order <- function (self, private, id) {
    private$m_log$order <- private$m_log$order[!object_id %in% id]
}
# }}}

# i_log_unsaved_idf {{{
i_log_unsaved_idf <- function (self, private) {
    private$m_log$unsaved <- TRUE
}
# }}}

# i_log_saved_idf {{{
i_log_saved_idf <- function (self, private) {
    private$m_log$unsaved <- FALSE
}
# }}}

# i_verbose_info {{{
i_verbose_info <- function (self, private, ...) {
    if (eplusr_option("verbose_info")) {
        cli::cat_rule(crayon::bold("Info"), col = "green")
        cat(crayon::green(paste0(...)), "\n", sep = "")
        cat("\n")
    }
}
# }}}

# i_need_update_num {{{
i_need_update_num <- function (self, private,
                               view_in_ip = eplusr_option("view_in_ip"),
                               num_digits = eplusr_option("num_digits")) {
    if (private$m_log$view_in_ip != view_in_ip) {
        private$m_log$view_in_ip <- view_in_ip
        return(TRUE)
    }

    if (private$m_log$num_digits != num_digits) {
        private$m_log$num_digits <- num_digits
        return(TRUE)
    }

    FALSE
}
# }}}

################################################################################
#                                 Idf Specific                                 #
################################################################################

# can_locate_idf_file {{{
can_locate_idf_file <- function (self, private) {
    if (is.null(private$m_path)) return(FALSE)
    if (!utils::file_test("-f", private$m_path)) return(FALSE)
    TRUE
}
# }}}

# TODO: set value or add object using a data.table
# i_idf_save {{{
i_idf_save <- function (self, private, path = NULL, format = eplusr_option("save_format"),
                        overwrite = FALSE, copy_external = TRUE) {
    if (is.null(path)) {
        if (is.null(private$m_path)) {
            stop("The Idf object is not created from local file. ",
                 "Please give the path to save.", call. = FALSE)
        } else {
            path <- private$m_path
        }
    } else {
        assert_that(is_string(path))
    }

    if (private$m_is_imf & !has_ext(path, "imf")) {
        warning("The Idf object contains EpMacro lines. Saving it to a ",
            "file other than `imf` file may cause errors during simulation.",
            call. = FALSE)
    } else {
        assert_that(has_exts(path, c("idf", "imf")),
            msg = paste0("`path` should have an extension of `idf` or `imf`."))
    }

    format <- match.arg(format, c("asis", "sorted", "new_top", "new_bot"))
    if (format == "asis") format <- private$m_log$save_format

    if (file.exists(path)) {
        if (!overwrite) {
            stop("Target already exists. Please set `overwrite` to ",
                 "TRUE if you want to replace it.", call. = FALSE)
        } else {
            i_verbose_info(self, private, "Replace the existing file located ",
                " at ", normalizePath(path), ".")
        }
    } else {
        d <- dirname(path)
        if (!dir.exists(d)) {
            tryCatch(dir.create(d, recursive = TRUE),
                warning = function (w) {
                    stop("Failed to creat directory ",
                        surround(d), ".", call. = FALSE)
                }
            )
        }
    }

    # copy external files into the same directories if necessary
    i_idf_resolve_external_link(self, private,
        old = private$m_path, new = path, copy = copy_external)

    str <- i_object_string(self, private, header = TRUE, comment = TRUE, save_format = format)
    write_lines_eol(str, path)

    # log saved
    i_log_saved_idf(self, private)

    # change path
    private$m_path <- path
}
# }}}

# i_idf_add_output_sqlite {{{
i_idf_add_output_sqlite <- function (self, private) {
    added <- FALSE
    if (i_is_valid_class_name(self, private, "Output:SQLite", type = "idf")) {
        sql <- self$object_in_class("Output:SQLite")[[1]]
        type <- toupper(sql$get_value()[[1]])
        if (type != "SIMPLEANDTABULAR") {
            invisible(sql$set_value("SimpleAndTabular"))
            i_verbose_info(self, private, "Setting `Option Type` in ",
                "`Output:SQLite` to from", surround(type), " to `SimpleAndTabular`.")
            added <- TRUE
        }
    } else {
        invisible(self$add_object("Output:SQLite", list("SimpleAndTabular")))
        i_verbose_info(self, private, "Adding object `Output:SQLite` and setting ",
            "`Option Type` to `SimpleAndTabular`.")
        added <- TRUE
    }
    added
}
# }}}

# i_idf_run {{{
i_idf_run <- function (self, private, epw, dir = NULL, wait = TRUE,
                       force = FALSE, copy_external = FALSE) {
    if (private$m_version < 8.3)
        stop("Currently, `$run()` only supports EnergyPlus V8.3 or higher.",
             call. = FALSE)

    # stop if unsaved
    if (self$is_unsaved())
        stop("Idf has been modified since read or last saved. Please save Idf ",
            "using $save() before run.", call. = FALSE)

    # check if the model is still running
    old <- private$m_log$job
    if (!is.null(old)) {
        proc <- ._get_private(old)$m_process$process
        if (inherits(proc, "process") && proc$is_alive()) {
            pid <- proc$get_pid()
            if (force) {
                old$kill()
                message("Force to kill current running simulation (PID: ", pid,
                    ") and start a new simulation...")
            } else {
                stop("The simulation of current Idf is still running (PID: ",
                    pid, "). Please set `force` to TRUE if you want ",
                    "to kill the running process and start a new simulation.",
                    call. = FALSE)
            }
        }
    }
    # add Output:SQLite if necessary
    add_sql <- i_idf_add_output_sqlite(self, private)

    # save the model to the output dir if necessary
    if (!can_locate_idf_file(self, private))
        stop("The Idf object is not created from local file or local file has ",
            "been deleted from disk. Please save Idf using $save() before run.", call. = FALSE)

    path_idf <- private$m_path
    if (is.null(dir))
        run_dir <- dirname(path_idf)
    else {
        run_dir <- dir
        path_idf <- normalizePath(file.path(run_dir, basename(path_idf)), mustWork = FALSE)
    }

    # if necessary, resave the model
    if (add_sql || !is.null(dir))
        i_idf_save(self, private, path_idf, overwrite = TRUE, copy_external = copy_external)

    job <- EplusJob$new(path_idf, epw, private$m_version)

    job$run(wait = wait)

    private$m_log$job <- job

    job
}
# }}}

# i_idf_resolve_external_link: auto change full file path in `Schedule:File` to
# relative path and copy those files into the same directory of the model {{{
i_idf_resolve_external_link <- function (self, private, old, new, copy = TRUE) {
    # Currently, only `Schedule:File` class is supported
    if (!i_is_valid_class_name(self, private, "Schedule:File", "idf")) return(FALSE)

    # restore current working directory
    ori <- getwd()
    on.exit(setwd(ori), add = TRUE)

    # set validate level to `none` to speed up
    ori_level <- eplusr_option("validate_level")
    eplusr_option(validate_level = "none")
    on.exit(eplusr_option(validate_level = ori_level), add = TRUE)

    # get full path of old and new
    old_dir <- normalizePath(dirname(old), mustWork = FALSE)
    new_dir <- normalizePath(dirname(new), mustWork = FALSE)

    # get object table and value table
    obj_tbl <- i_object_tbl_from_class(self, private, "Schedule:File")
    val_tbl <- i_value_tbl_from_which(self, private, object = obj_tbl$object_id)[
        full_name == "File Name"]

    # check existence of old files
    setwd(old_dir)
    val_tbl[, old_full_path := normalizePath(value, mustWork = FALSE)]
    val_tbl[, old_exist := file.exists(old_full_path)]

    if (not_empty(val_tbl[old_exist == FALSE]))
        warning(paste0("Broken external file link found in Idf: ",
            surround(val_tbl[old_exist == FALSE, old_full_path]), collapse = "\n"),
        call. = FALSE)

    val_tbl[, same_dir := normalizePath(dirname(old_full_path)) == new_dir]

    targ <- val_tbl[old_exist == TRUE & same_dir == FALSE]

    if (is_empty(targ)) return(FALSE)

    # copy external files and change values to relative paths
    if (copy) {
        targ[, file_name := basename(value)]
        targ[, new_full_path := normalizePath(file.path(new_dir, file_name), mustWork = FALSE)]
        targ[, new_value := file_name]

        # copy files
        to_copy <- unique(targ[, list(old_full_path, new_full_path)])
        flgs <- logical(nrow(to_copy))
        for (i in seq_len(nrow(to_copy))) {
            flgs[i] <- file.copy(to_copy$old_full_path[i], to_copy$new_full_path[i], overwrite = TRUE, copy.date = TRUE)
        }
        if (any(!flgs)) {
            stop(paste0("Failed to copy external file into the ",
                "output directory: ", surround(targ[!flgs]), collapse = "\n"),
                call. = FALSE)
        }
    # change all paths to full paths
    } else {
        targ[, new_value := old_full_path]
    }

    targ[, input := {
        l <- as.list(new_value); names(l) <- field_name; list(list(l))},
        by = list(object_id)]

    i_set_object(self, private, targ$object_id, targ$input)

    TRUE
}
# }}}

################################################################################
#                             IdfObject Sepecific                              #
################################################################################

# i_idfobj_set_comment {{{
i_idfobj_set_comment <- function (self, private, object, comment, append = TRUE, width = 0L) {
    assert_that(is_scalar(object))
    obj_tbl <- i_object_tbl_from_which(self, private, object)

    if (is.null(comment)) {
        i_remove_comment_from_which(self, private, obj_tbl$object_id)
    } else {
        assert_that(is.character(comment))

        comment <- strsplit(comment, "\n", fixed = TRUE)

        if (width != 0L) {
            assert_that(is_count(width))
            comment <- strwrap(comment, width = width)
        }

        new_cmt_tbl <- i_new_comment_tbl(self, private, obj_tbl$object_id, comment)
        new_cmt_id <- i_new_id(self, private, "comment", nrow(new_cmt_tbl))
        new_cmt_tbl[, `:=`(comment_id = new_cmt_id)]

        if (is.null(append)) {
            # add new
            i_assign_comment_tbl(self, private, new_cmt_tbl, exclude = TRUE)
        } else {
            assert_that(is_scalar(append),
                msg = "`append` should be NULL or a single logical value."
            )
            if (append) {
                # add new
                i_assign_comment_tbl(self, private, new_cmt_tbl, exclude = FALSE)
            } else {
                old <- i_comment_tbl_from_which(self, private, obj_tbl$object_id, nomatch = 0L)$comment
                comment <- c(comment, old)
                new_cmt_tbl <- i_new_comment_tbl(self, private, obj_tbl$object_id, comment)
                new_cmt_id <- i_new_id(self, private, "comment", nrow(new_cmt_tbl))
                new_cmt_tbl[, `:=`(comment_id = new_cmt_id)]
                # add new
                i_assign_comment_tbl(self, private, new_cmt_tbl, exclude = TRUE)
            }

        }
    }

    # log unsaved
    i_log_unsaved_idf(self, private)

    # log order change
    i_log_add_object_order(self, private, obj_tbl$object_id)

    # log uuid
    i_log_new_uuid(self, private)

    self
}
# }}}

# i_idfobj_get_value {{{
i_idfobj_get_value <- function (self, private, object, which = NULL, all = FALSE,
                                simplify = FALSE, in_ip = eplusr_option("view_in_ip")) {
    val_tbl <- i_value_tbl_from_field_which(self, private, object, which)

    if(!all) val_tbl <- val_tbl[!is.na(value_id)]

    if (i_need_update_num(self, private, view_in_ip = in_ip))
        val_tbl <- update_value_num(val_tbl, in_ip = in_ip)

    if (simplify) return(val_tbl$value)

    val <- value_list(val_tbl, in_ip = in_ip)

    nm <- underscore_name(val_tbl$field_name)

    data.table::setattr(val, "names", nm)

    val
}
# }}}

# i_idfobj_set_value {{{
i_idfobj_set_value <- function (self, private, object, ..., default = TRUE) {
    # capture all arguments in dots and flatten into a list
    value <- list(...)

    # splice for `[[<-.IdfObject` and `$<-.IdfObject`
    if (length(value) == 1L && vec_depth(value) == 3L) value <- Reduce(c, value)

    if (is_empty(value))
        stop("Please give values to set.", call. = FALSE)
    i_set_object(self, private, object, value, comment = NULL, default)[[1]]
}
# }}}

# i_idfobj_value_table {{{
i_idfobj_value_table <- function (self, private, object, all = FALSE, unit = TRUE,
                                  wide = FALSE, string_value = TRUE,
                                  in_ip = eplusr_option("view_in_ip")) {
    val_tbl_full <- i_value_tbl_from_field_which(self, private, object, NULL)

    if(!all) val_tbl_full <- val_tbl_full[!is.na(value_id)]

    # change to IP numbers and names if necessary
    if (i_need_update_num(self, private, view_in_ip = in_ip))
        val_tbl_full <- update_value_num(val_tbl_full, eplusr_option("num_digits"), in_ip)

    ip <- if(in_ip) "ip" else ""

    if (unit) fld_nm <- paste0("full_", ip, "name")
    else fld_nm <- "field_name"

    cols <- c("field_index", fld_nm, "value", paste0("value_", ip, "num"))

    val_tbl <- val_tbl_full[, .SD, .SDcols = cols]

    if (in_ip) {
        data.table::setnames(val_tbl,
            c("full_ipname", "value_ipnum"),
            c("full_name", "value_num"))
    }

    if (!string_value) {
        val_tbl[, value := list(as.list(value))]
        # NOTE: if a numeric field is set to "autosize", then
        # inconsistency will be introduced into return value classes, as
        # values will be return as a string "autosize" instead of a
        # number.
        val_tbl[!is.na(value_num), value := list(as.list(value_num))]
    }

    val_tbl[, value_num := NULL]

    data.table::setnames(val_tbl, c("index", "name", "value"))

    if (wide) {
        val_wide_tbl <- data.table::dcast(val_tbl, . ~ name,
            value.var = c("value"))[, `.` := NULL]
        data.table::setcolorder(val_wide_tbl, val_tbl[["name"]])
        if (!string_value) {
            val_wide_tbl <- val_wide_tbl[, lapply(.SD, unlist)]
        }

        val_wide_tbl[]
    } else {
        val_tbl[]
    }
}
# }}}

# i_idfobj_ref_by {{{
i_idfobj_ref_by <- function (self, private, object) {
    val_tbl <- i_value_tbl_from_which(self, private, object)
    ref_by <- i_val_ref_by_tbl(self, private, val_tbl)
    obj_id <- unique(ref_by$referenced_by_object_id)

    if (is_empty(obj_id)) {
        message("Object is not referenced by any other object.")
    } else {
        message(length(obj_id), " object found that reference the target object [ID: ",
            unique(ref_by$object_id), "].\n")
        i_idfobject(self, private, obj_id)
    }

}
# }}}

# i_idfobj_ref_from {{{
i_idfobj_ref_from <- function (self, private, object) {
    val_tbl <- i_value_tbl_from_which(self, private, object)
    ref_from <- i_val_ref_from_tbl(self, private, val_tbl)
    obj_id <- unique(ref_from$referencing_from_object_id)
    if (is_empty(obj_id)) {
        message("Object does not reference from any other object.")
    } else {
        message(length(obj_id), " object found that are referenced by the target object [ID: ",
            unique(ref_from$object_id), "].\n")
        i_idfobject(self, private, obj_id)
    }
}
# }}}

# i_idfobj_has_ref_by {{{
i_idfobj_has_ref_by <- function (self, private, object) {
    val_tbl <- i_value_tbl_from_which(self, private, object)
    ref_by <- i_val_ref_by_tbl(self, private, val_tbl)
    not_empty(ref_by)
}
# }}}

# i_idfobj_has_ref_from {{{
i_idfobj_has_ref_from <- function (self, private, object) {
    val_tbl <- i_value_tbl_from_which(self, private, object)
    ref_from <- i_val_ref_from_tbl(self, private, val_tbl)
    not_empty(ref_from)
}
# }}}

# i_idfobj_possible_values {{{
i_idfobj_possible_values <- function (self, private, object, which = NULL) {
    val_tbl <- i_value_tbl_from_field_which(self, private, object, which)[
        , list(class_id, value_id, field_index, field_name, autosizable, autocalculatable)]

    # delete extra fields if no field is given
    if (is.null(which)) {
        val_tbl <- val_tbl[!is.na(value_id)]
        which <- seq.int(nrow(val_tbl))
    }

    val_tbl[, auto := NA_character_]
    val_tbl[autosizable == TRUE, auto := "Autosize"]
    val_tbl[autocalculatable == TRUE, auto := "Autocalculate"]

    val_tbl[, default := list(i_field_default(self, private, class_id[1L], which))]
    val_tbl[, choice := list(i_field_choice(self, private, class_id[1L], which))]
    val_tbl[, range := list(i_field_range(self, private, class_id[1L], which))]
    val_tbl[, reference := list(i_field_reference(self, private, class_id[1L], which))]

    res <- val_tbl[, list(field_index, field_name, auto, default, choice, range, reference)]
    data.table::setattr(res, "class", c("IddFieldPossible", class(res)))
    res
}
# }}}
