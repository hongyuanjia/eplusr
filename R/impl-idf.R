#' @importFrom cli cat_bullet cat_line cat_rule rule symbol
#' @importFrom crayon bold cyan red strip_style underline
#' @importFrom data.table copy data.table dcast rbindlist
#' @importFrom data.table setattr setcolorder setnames setorder setorderv
#' @importFrom stringi stri_locate_first_regex stri_replace_first_regex "stri_sub<-"
#' @importFrom stringi stri_subset_regex stri_match_first_regex stri_rand_strings
#' @include parse.R
#' @include clone.R
NULL

# DOTS
# ...elt(n) to get the nth element in dots (after evaluation)
# ...length() to get the total number of element in dots (without evaluation)
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
        } else if (all(are_count(x))) {
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

    is_null_list <- function (x) is.list(x) && length(x) == 0L
    # is_null_list <- function (x) is.null(unlist(x, use.names = FALSE))

    # flatten_input {{{
    flatten_input <- function (dt) {
        if (!nrow(dt)) return(empty_input)

        dep <- unique(dt$dep)

        # empty object, e.g. Construction = list()
        if (dep == 1L) {
            id_nolist <- dt[!vapply(dot, is.list, logical(1L)) | is.na(dot_nm), rleid]
            id_invalid <<- c(id_invalid, id_nolist)
            dt <- dt[!rleid %in% id_nolist, {
                list(rleid = rleid,
                     class_name = dot_nm,
                     field_name = rep(NA_character_, .N),
                     value_list = rep(list(NULL), .N),
                     empty = vapply(dot, is_null_list, logical(1L))
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
    if (nrow(flat[vapply(value_list, function (x) is.character(x) && stringi::stri_startswith_fixed(x, "!"), logical(1L))])) {
        id_exc <- flat[vapply(value_list, function (x) is.character(x) && stringi::stri_startswith_fixed(x, "!"), logical(1L)), unique(rleid)]
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

# OBJECT
# get_idf_object {{{
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
#' @noRd
get_idf_object <- function (idf_env, class = NULL, object = NULL, cols = NULL,
                            underscore = FALSE, ignore_case = FALSE) {
    if (is.null(class)) {
        obj <- add_rleid(copy(idf_env$object))
    } else {
        cls_in <- recognize_input(class, "class", underscore)
        obj <- join_from_input(idf_env$object, cls_in, "group_id")
    }

    if (is.null(object)) return(if (is.null(cols)) obj else obj[, .SD, .SDcols = cols])

    obj_in <- recognize_input(object, "object", lower = ignore_case)
    res <- join_from_input(obj, obj_in, "group_id")

    # stop if there are objects that have the same name {{{
    if (!have_same_len(res, obj_in)) {
        mult_rleid <- res[, .N, by = rleid][N > 1L, rleid]
        mult <- res[J(mult_rleid), on = "rleid"]

        set(mult, NULL, "object",
            get_object_info(mult, c("id", "class"), numbered = FALSE, prefix = "")
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
    # }}}

    if (is.null(cols)) return(res)
    res[, .SD, .SDcols = cols]
}
# }}}
# get_idf_object_id {{{
get_idf_object_id <- function (idf_env, class = NULL, simplify = FALSE) {
    if (simplify) {
        return(get_idf_object(idf_env, class, cols = "object_id")$object_id)
    }

    obj <- get_idf_object(idf_env, class,
        cols = c("rleid", "class_id", "class_name", "object_id")
    )

    if (is.null(class)) {
        col_by <- "class_id"
        setorderv(obj, "class_id")
        nm <- unique(obj$class_name)
    } else {
        col_by <- "rleid"
        nm <- unique(obj[, .SD, .SDcols = c("rleid", "class_name")])$class_name
    }

    res <- lapply(split(obj, by = col_by, keep.by = FALSE), `[[`, "object_id")
    setattr(res, "names", nm)
    res
}
# }}}
# get_idf_object_name {{{
get_idf_object_name <- function (idf_env, class = NULL, simplify = FALSE, lower = FALSE) {
    if (simplify) {
        trans <- if (lower) stri_trans_tolower else identity
        return(trans(get_idf_object(idf_env, class, cols = "object_name")$object_name))
    }

    col <- if (lower) "object_name_lower" else "object_name"
    obj <- get_idf_object(idf_env, class,
        cols = c("rleid", "class_id", "class_name", col)
    )

    if (is.null(class)) {
        col_by <- "class_id"
        setorderv(obj, "class_id")
        nm <- unique(obj$class_name)
    } else {
        col_by <- "rleid"
        nm <- unique(obj[, .SD, .SDcols = c("rleid", "class_name")])$class_name
    }

    res <- lapply(split(obj, by = col_by, keep.by = FALSE), `[[`, col)
    setattr(res, "names", nm)
    res
}
# }}}
# get_idf_object_num {{{
get_idf_object_num <- function (idf_env, class = NULL) {
    if (is.null(class)) return(nrow(idf_env$object))

    if (is.character(class)) {
        col_on <- "class_name"
    } else if (all(are_count(class))) {
        col_on <- "class_id"
    } else {
        abort_bad_which_type("error_class_which_type", "class")
    }

    setindexv(idf_env$object, col_on)
    idf_env$object[
        J(unique(class)), on = col_on][
        , .N, by = list(class_id, class_name, found = !is.na(object_id))][
        found == FALSE, `:=`(N = 0L)][
        J(class), on = col_on]$N
}
# }}}
# get_object_info {{{
get_object_info <- function (dt_object, component = c("id", "name", "class"),
                             by_class = FALSE, numbered = TRUE, collapse = NULL,
                             prefix = NULL, name_prefix = TRUE) {
    assert(component %in% c("id", "name", "class"))

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
            if (name_prefix) {
                mes <- paste0("name ", surround(object_name))
            } else {
                mes <- surround(object_name)
            }
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
            mes <- dt_object[, paste0(key_cls, ": ", class_name)]
        } else {
            set(dt_object, NULL, "mes_object", mes)
            if (by_class) {
                mes <- dt_object[, {
                    paste0(key_obj, " ", collapse(mes_object, NULL), " in class ", class_name[1L])
                }, by = class_name]$V1
            } else {
                mes <- dt_object[, {
                    paste0(key_obj, " ", mes_object, " in class ", class_name)
                }]
            }
            set(dt_object, NULL, "mes_object", NULL)
        }
    } else {
        if (!is.null(mes)) {
            mes <- paste0(key_obj, ": ", mes)
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

# VALUE
# get_idf_value_in_class {{{
# Return all object value data in a class
# @param idd_env An environment contains IDD tables including class, field, and
#        reference.
# @param idf_env An environment contains IDF tables including object, value, and
#        reference.
# @param class An integer vector of valid class indexes or a character vector
#        of valid class names or a data.table that contains columns `class_id`
#        or `class_name` and `class_name_us` (when `underscore` is TRUE). If
#        both `class_id` and `class_name` exist, `class_id` will be used for
#        matching.
# @param field NULL or an integer vector of valid field indexes or a character
#        vector of valid field names (in underscore style). If not `NULL`,
#        `class` and `field` should have the same length.
# @param cols A character vector of column names in value table to return. If
#        `NULL`, all columns from object value table will be returned, plus
#        column `rleid`.
# @param underscore If `TRUE`, input class name will be converted into
#        underscore style name first and column `class_name_us` in IDD class
#        table will be used for matching.
# @param fill If `TRUE`, all field number will be the same.
get_idf_value_in_class <- function (idd_env, idf_env, class, field = NULL, cols = NULL,
                                    underscore = FALSE, fill = FALSE, name = TRUE) {
    # avoid duplications in class in order to make sure there will also no
    # duplications in object id
    assert(!is.null(class))
    if (!is.null(field)) assert(have_same_len(class, field))

    if (is.null(cols)) cols <- names(idf_env$value)
    if (name) cols <- unique(c(cols, "object_name"))

    # valid class field name and get all field data in a class
    # here adding new extensible fields are not allowed
    fld <- get_idd_field(idd_env, class, field, underscore = underscore, all = TRUE)

    # validate class input
    if (!any(unique(fld$class_id) %in% unique(idf_env$object$class_id))) {
        abort_bad_key("error_class_name", "class name",
            fld[class_id %in% idf_env$object$class_id, unique(class_name)]
        )
    }

    # make sure number of fields in each object is same
    fld <- idf_env$object[, list(class_id, object_id, object_name)][
        fld, on = "class_id", allow.cartesian = TRUE]

    val <- idf_env$value[, list(value_id, value, value_num, object_id, field_id)][
        fld, on = c("object_id", "field_id"), allow.cartesian = TRUE]

    setorderv(val, c("rleid", "object_id", "field_index"))

    if (!fill) {
        return(val[!is.na(value_id), .SD, .SDcols = cols])
    }

    if (!is.null(field)) {
        # assign ids new filled values to negative values
        val[is.na(value_id), value_id := -seq_len(.N)]

        return(val[, .SD, .SDcols = cols])
    }

    count <- val[!is.na(value_id), list(field_index = max(field_index)), by = c("rleid", "object_id")][
        , list(field_index = rep(max(field_index), .N), object_id = object_id), by = "rleid"]

    val <- val[count, on = c("rleid", "object_id", "field_index<=field_index")][
        , field_index := seq_len(.N), by = c("rleid", "object_id")]

    # assign ids new filled values to negative values
    val[is.na(value_id), value_id := -seq_len(.N)][, .SD, .SDcols = cols]
}
# }}}
# get_idf_value {{{
# Return all object value data in a object
get_idf_value <- function (idd_env, idf_env, class = NULL, object = NULL, field = NULL, cols = NULL,
                           underscore = FALSE, ignore_case = FALSE, fill = FALSE, name = TRUE) {
    if (is.null(cols)) cols <- names(idf_env$value)
    if (name) cols <- unique(c(cols, "object_name"))

    obj <- get_idf_object(idf_env, class, object, NULL, underscore, ignore_case)

    # valid class field name and get all field data in a class
    fld <- get_idd_field(idd_env, obj, field, underscore = underscore, no_ext = TRUE, all = TRUE)

    # make sure number of fields in each object is same
    fld <- obj[, list(class_id, object_id, object_name)][
        fld, on = "class_id", allow.cartesian = TRUE]

    val <- idf_env$value[, list(value_id, value, value_num, object_id, field_id)][
        fld, on = c("object_id", "field_id"), allow.cartesian = TRUE]

    setorderv(val, c("rleid", "object_id", "field_index"))

    if (!fill) {
        return(val[!is.na(value_id), .SD, .SDcols = cols])
    }

    if (!is.null(field)) {
        # assign ids new filled values to negative values
        val[is.na(value_id), value_id := -seq_len(.N)]

        return(val[, .SD, .SDcols = cols])
    }

    count <- val[!is.na(value_id), list(field_index = max(field_index)), by = c("rleid", "object_id")][
        , list(field_index = rep(max(field_index), .N), object_id = object_id), by = "rleid"]

    val <- val[count, on = c("rleid", "object_id", "field_index<=field_index")][
        , field_index := seq_len(.N), by = c("rleid", "object_id")]

    # assign ids new filled values to negative values
    val[is.na(value_id), value_id := -seq_len(.N)][, .SD, .SDcols = cols]
}
# }}}
# get_idf_value_from_which {{{
# Return object value data
# @param idd_env An environment contains IDD tables including class, field, and
#        reference.
# @param idf_env An environment contains IDF tables including object, value, and
#        reference.
# @param class An integer vector of valid class indexes or a character vector
#        of valid class names or a data.table that contains column `class_id`
#        or `class_name` to specify which classes should be considered when
#        matching object ID or names. If `class` is a data.table with both
#        `class_id` and `class_name` column, `class_id` will be used.
# @param object An integer vector of valid object IDs or a character vector
#        of valid object names or a data.table that contains column `object_id`
#        or `object_name`. If `object` is a data.table with both `object_id`
#        and `object_name` column, `object_id` will be used.
# @param field NULL or an integer vector of valid field indexes or a character
#        vector of valid field names (in underscore style). If not `NULL`,
#        `class` and `field` should have the same length.
# @param cols A character vector of column names in value table to return. If
#        `NULL`, all columns from object value table will be returned, plus
#        column `rleid`.
# @param underscore If `TRUE`, input class name will be converted into
#        underscore style name first and column `class_name_us` in IDD class
#        table will be used for matching.
# @param ignore_case If TRUE, object name matching will be case-insensitive.
get_idf_value_from_which <- function (idd_env, idf_env, class = NULL, object,
                                      field = NULL, cols = NULL,
                                      underscore = FALSE, ignore_case = FALSE) {
    assert(!is.null(object))
    if (is.null(cols)) cols <- unique(c(names(object), names(idf_env$value)))

    obj <- get_idf_object(idf_env, class, object,
        underscore = underscore, ignore_case = ignore_case)

    if (is.null(field)) {
        return(idf_env$value[obj, on = "object_id"][, .SD, .SDcols = cols])
    }

    fld <- get_idd_field(idd_env, obj, field, underscore = underscore,
        cols = intersect(names(idd_env$field), names(idf_env$value)))

    idf_env$value[, .SD, .SDcols = c("object_id", "value_id", "value", "value_num", "field_id")][
        cbind(obj[, .SD, .SDcols = setdiff(names(obj), names(fld))], fld),
        on = c("object_id", "field_id")][, .SD, .SDcols = cols]
}
# }}}
# get_idf_value_all_node {{{
get_idf_value_all_node <- function (idf_env) {
    idf_env$value[type_enum == IDDFIELD_TYPE$node & !is.na(value), unique(value)]
}
# }}}

# ASSERT
# FOR BACK COMPATIBILITY
# SHOULD BE REMOVED IN NEXT RELEASE
# assert_valid_input_format {{{
assert_valid_input_format <- function (class_name, value, comment, default, type = c("add", "set")) {
    type <- match.arg(type)
    key <- switch(type, add = "class", set = "object")

    is_valid_input <- function (x) is.null(x) || is_normal_list(x)

    if (length(class_name) > 1L &&
        ((!is.null(value)   && !have_same_len(class_name, value)) ||
         (!is.null(comment) && !have_same_len(class_name, comment))))
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
# assert_can_do {{{
assert_can_do <- function (dt_class, dt_object, dot, object,
                           action = c("add", "dup", "set", "del")) {
    find_dot <- function (dot, dt) dt[dot, on = "rleid", mult = "first", nomatch = 0L]

    # stop attempting to touch Version {{{
    if (nrow(object[class_name == "Version"])) {
        invld <- find_dot(dot, object[class_name == "Version"])

        m <- paste0(dot_string(invld, NULL), " --> Class ", surround(invld$class_name), collpase = "\n")

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
                info <- get_object_info(invld, collapse = NULL, numbered = FALSE)

                m <- paste0(dot_string(invld, NULL), " --> ", info, collapse = "\n")
                invld[, paste0(" #", lpad(rleid, "0"), "| ", dot, collapse = "\n")]

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

                m <- paste0(dot_string(invld, NULL), " --> Class: ", surround(invld$class_name), collapse = "\n")

                abort("error_add_multi_unique",
                    paste0("Unique object can only be added once. Invalid input\n", m),
                    dot = dot, object = object
                )
            }
            # }}}
            # try do del unique object {{{
            if (action == "del" && nrow(uni[t_object_num(dt_object, class_id) == 0L])) {

                invld <- find_dot(dot, uni[t_object_num(dt_object, class_id) == 0L])

                info <- geget_object_info(invld, collapse = NULL)

                m <- paste0(dot_string(invld, NULL), " --> ", info, collapse = "\n")
            }
            # }}}
        }
    }

    # stop attempting to delete required objects {{{
    if (action == "del" && level_checks()$required_object && nrow(object[class_id %in% t_class_id_required(dt_class)])) {
        invld <- find_dot(dot, object[class_id %in% t_class_id_required(dt_class)])

        info <- get_object_info(invld, collapse = NULL)

        m <- paste0(dot_string(invld, NULL), " --> ", info, collapse = "\n")

        abort("error_del_required",
            paste0("Deleting a required object is prohibited. Invalid input:\n", m),
            dot = dot, object = object
        )
    }
    # }}}

    # stop if modifying same object multiple times {{{
    if (action %in% c("set", "del") && anyDuplicated(object, by = "object_id")) {
        invld <- find_dot(dot, object[duplicated(class_id)])
        info <- get_object_info(invld, numbered = FALSE)
        m <- paste0(dot_string(invld, NULL), " --> ", info, collapse = "\n")

        abort(paste0("error_", action, "_multi_time"),
            paste0("Cannot modify same object multiple times. Invalid input:\n", m),
            dot = dot, object = object
        )
    }
    # }}}

    TRUE
}
# }}}
# assert_valid {{{
assert_valid <- function (idd_env, idf_env, object, value, action = c("add", "set")) {
    action <- match.arg(action)
    # validate fields that do not use default values and all extensible fields
    val_chk <- value[required_field == TRUE | defaulted == FALSE | extensible_group > 0L]
    validity <- validate_on_level(idd_env, idf_env,
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

# OBJECT MUNIPULATION
# duplicate_idf_object {{{
duplicate_idf_object = function (idd_env, idf_env, ...) {
    l <- sep_name_dots(..., .can_name = TRUE)
    obj <- get_object_input(idd_env, idf_env, l, keep_duplicate = TRUE)

    # add has_name attribute
    obj <- idd_env$class[, list(class_id, has_name)][obj, on = "class_id"]

    # stop if cannot add objects in specified classes
    assert_can_do(idd_env$class, idf_env$object, l$dot, obj, "dup")

    # check input new names {{{
    # stop if trying to assign names to objects that do not have name attribute
    if (nrow(obj[has_name == FALSE & !is.na(new_name)])) {
        cannot_name <- unique(obj[has_name == FALSE & !is.na(new_name)], by = "object_id")
        mes <- paste0("Target object(s) in class that does not have name attribute ",
            "cannot be renamed. Invalid input:\n",
            get_object_info(cannot_name, collapse = "\n")
        )
        abort(c("error_cannot_rename", "error_dup_object"), mes, data = cannot_name)
    }

    # get value data
    val <- get_idf_value(idd_env, idf_env, object = obj[, list(rleid, object_rleid, object_id)])
    # NOTE:
    # (a) restore old value id for updating reference
    # (b) Assign new value id in order to correctly print validate message
    setnames(val, "value_id", "old_value_id")
    set(val, NULL, "value_id", new_id(idf_env$value, "value_id", nrow(val)))

    # NOTE:
    # (a) Store old id and name for logging
    # (b) Change object names for validation
    setnames(obj, c("object_id", "object_name", "object_name_lower"),
        c("old_object_id", "old_object_name", "old_object_name_lower")
    )
    set(obj, NULL, c("object_name", "object_name_lower"),
        list(obj$new_name, stri_trans_tolower(obj$new_name)))

    validity <- validate_objects(idd_env, idf_env,
        copy(obj)[, object_id := -rleid],
        copy(val)[, object_id := -rleid],
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
    obj <- assign_new_id(idf_env, obj, "object")
    val <- correct_obj_id(obj, val)

    # get new object name {{{
    # get indicator of whether user input new names are used
    set(obj, NULL, "use_input_name", FALSE)
    obj[has_name == TRUE & !is.na(object_name_lower) &
        (object_name_lower != old_object_name_lower | is.na(old_object_name_lower)),
        `:=`(use_input_name = TRUE)
    ]

    # get all name in the same class
    obj[, `:=`(all_name_lower = get_idf_object_name(idf_env, class_name, lower = TRUE)), by = "rleid"]

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
        id <- rpad(get_object_info(auto, "id"))
        name <- get_object_info(auto, "name", prefix = " --> New ", numbered = FALSE)
        verbose_info(
            "New names of duplicated objects not given are automatically generated:\n",
            paste0(id, name, collapse = "\n")
        )
    }
    # }}}

    # assign name field
    val[is_name == TRUE, `:=`(value = obj[has_name == TRUE, object_name])]

    # value reference
    ref <- idf_env$reference[J(val$old_value_id), on = "value_id", nomatch = 0L]
    set(ref, NULL, "value_id", val$value_id)

    list(object = obj[, .SD, .SDcols = names(idf_env$object)],
         value = val[, .SD, .SDcols = names(idf_env$value)],
         reference = append_dt(idf_env$reference, ref)
    )
}
# }}}
# add_idf_object {{{
add_idf_object <- function (idd_env, idf_env, ..., .default = TRUE, .all = FALSE) {
    l <- sep_value_dots(..., .empty = TRUE)

    obj <- l$value[J(unique(l$value$rleid)), on = "rleid", mult = "first"]

    # new object table
    obj <- get_idd_class(idd_env, obj,
        cols = c("rleid", "class_id", "class_name", "group_id"),
        underscore = TRUE
    )

    # stop if cannot add objects in specified classes
    assert_can_do(idd_env$class, idf_env$object, l$dot, obj, "add")

    # add object id
    obj <- assign_new_id(idf_env, obj, "object")

    # get object comments
    obj <- l$value[field_name == ".comment", list(rleid, comment = value_list)][obj, on = "rleid"]

    # new value table
    val <- l$value[is.na(field_name) | field_name != ".comment",
        .SD, .SDcols = c("rleid", "field_name", "value_list", "empty")][
        obj[, .SD, .SDcols = c("rleid", "object_id", "class_id", "class_name")],
        on = "rleid"]

    # get empty objects {{{
    val_empty <- val[empty == TRUE]
    val_empty <- get_idd_field(idd_env, val_empty, all = .all)

    # add value and value in number
    set(val_empty, NULL, c("value", "value_num"), list(NA_character_, NA_real_))
    # }}}

    # get non-empty objects {{{
    val_nonempty <- val[empty == FALSE, -"empty"]

    setindexv(val_nonempty, "field_name")

    # get field names and attributes {{{
    val_unnm <- val_nonempty[is.na(field_name)]
    fld_unnm <- get_idd_field(idd_env, val_unnm, rowidv(val_unnm, "rleid"), all = .all, complete = TRUE)

    val_nm <- val_nonempty[!is.na(field_name)]
    fld_nm <- get_idd_field(idd_env, val_nm, val_nm$field_name, all = .all, complete = TRUE)

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
    # make sure all empty values are set to NA
    val_matched[stri_detect_regex(value, "^\\s*$"), `:=`(value = NA_character_)]
    val_matched[vapply(value_list, is.numeric, logical(1L)), `:=`(value_num = as.double(value_list))]
    set(val_matched, NULL, "value_list", NULL)
    # }}}

    # combine empty and non-empty objects
    val <- rbindlist(list(val_empty, val_matched[, .SD, .SDcols = names(val_empty)]), use.names = TRUE)

    # assign default values if needed
    if (.default) val <- assign_default_value(val)

    # assign new value id
    val <- assign_new_id(idf_env, val, "value")
    val <- val[obj[, list(object_id, rleid)], on = "rleid"]

    # update object name
    obj <- update_object_name(obj, val)
    # add lower name
    set(obj, NULL, "object_name_lower", stri_trans_tolower(obj$object_name))

    # validate
    assert_valid(idd_env, idf_env, obj, val, action = "add")

    # get field reference
    # include new values in order to find references across input new objects
    ref <- get_value_reference_map(idd_env$reference,
        append_dt(idf_env$value, val, "value_id"),
        append_dt(idf_env$value, val, "value_id")
    )

    list(object = obj[, .SD, .SDcols = names(idf_env$object)],
         value = val[, .SD, .SDcols = names(idf_env$value)],
         reference = ref
    )
}
# }}}
# set_idf_object {{{
set_idf_object <- function (idd_env, idf_env, ..., .default = TRUE) {
    l <- sep_value_dots(..., .empty = FALSE, .duplicate = FALSE)

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
    obj_id <- get_idf_object(idf_env, object = obj_id)
    obj_nm <- get_idf_object(idf_env, object = obj_nm)

    # combine
    obj <- rbindlist(list(obj_id, obj_nm))

    # stop if cannot set objects
    assert_can_do(idd_env$class, idf_env$object, l$dot, obj, "set")

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
    val_empty <- t_value_data_in_object(idd_env, idf_env, object = val_empty)[is.na(value)]
    set(val_empty, NULL, "value_list", NULL)
    # }}}

    # get non-empty objects {{{
    val_nonempty <- val[empty == FALSE]
    val_nonempty <- t_value_data_in_object(idd_env, idf_env, object = val_nonempty,
        field = val_nonempty$field_name, underscore = TRUE)
    # mark empty fields
    set(val_nonempty, NULL, "empty", vapply(val_nonempty$value_list, is.null, logical(1L)))

    # set new values
    val_nonempty[empty == FALSE, value := as.character(value_list)]
    # make sure all empty values are set to NA
    val_nonempty[stri_detect_regex(value, "^\\s*$"), `:=`(value = NA_character_)]
    val_nonempty[empty == FALSE & vapply(value_list, is.numeric, logical(1L)),
        value_num := as.double(value_list)]
    set(val_nonempty, NULL, "value_list", NULL)
    # }}}

    val <- rbindlist(list(val_empty, val_nonempty), use.names = TRUE)

    # get field names and attributes
    val <- t_value_fill_attr(idd_env, val)

    # assign default values if needed
    set(val, NULL, "defaulted", FALSE)
    val[is.na(value) & empty == TRUE, defaulted := TRUE]
    if (.default) {
        val <- assign_default_value(val)
    }

    # assign new value id to new fields
    val <- assign_new_id(idf_env, val, "value", keep = TRUE)

    # update object name
    obj <- update_object_name(obj, val)

    # add lower name
    set(obj, NULL, "object_name_lower", stri_trans_tolower(obj$object_name))

    # validate
    assert_valid(idd_env, idf_env, obj, val, action = "set")

    # update referenced value in other objects if necessary
    new_val <- idf_env$reference[src_enum == IDDFIELD_SOURCE$field][
        val[, list(value_id, value, value_num)],
        on = c(src_value_id = "value_id")]
    idf_env$value[new_val, on = "value_id",
        `:=`(value = new_val$value, value_num = new_val$value_num)]

    # get field reference
    # NOTE: it may be more efficient to exclude fields whose references have
    # already been update in above
    ref <- get_value_reference_map(idd_env$reference,
        append_dt(idf_env$value, val, "value_id"),
        append_dt(idf_env$value, val, "value_id")
    )

    list(object = obj[, .SD, .SDcols = names(idf_env$object)],
         value = val[, .SD, .SDcols = names(idf_env$value)],
         reference = ref
    )
}
# }}}
# delete_idf_object {{{
delete_idf_object <- function (idd_env, idf_env, ..., .referenced = FALSE) {
    l <- sep_name_dots(..., .can_name = TRUE)
    obj <- get_object_input(idd_env, idf_env, l, keep_duplicate = TRUE)

    assert_can_do(idd_env$class, idf_env$object, l$dot, obj, "del")

    ref <- idf_env$reference[obj, on = c(src_object_id = "object_id"), nomatch = 0L]
    setnames(ref,
        c("object_id", "value_id", "src_object_id", "src_value_id"),
        c("ref_object_id", "ref_value_id", "object_id", "value_id")
    )

    # IDs of objects to be deleted
    id_del <- obj$object_id

    # check if target objects are referenced {{{
    if (nrow(ref)) {
        # TODO: what if invalid reference exists?
        ref_by <- idf_env$object[, list(object_id, object_name)][
            idf_env$value[ref[, list(src_object_id = object_id, src_value_id = value_id, value_id = ref_value_id)], on = "value_id"],
            on = "object_id"
        ]

        if (level_checks()$reference) {
            # dot message
            m_dot <- dot_string(l$dot[unique(ref, by = "object_id"), on = "rleid"], collapse = NULL)

            # src message
            m_ref <- get_object_info(unique(ref, by = "object_id"), numbered = FALSE)

            # ref message
            set(ref, NULL, "mes", get_object_info(ref_by, c("id", "name"), numbered = FALSE, prefix = ""))
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
    dt_object <- idf_env$object[!J(id_del), on = "object_id"]

    # delete rows in value table and value reference table
    dt_value <- idf_env$value[!J(id_del), on = "object_id"]

    # delete rows in value reference table
    dt_reference <- idf_env$reference[!J(id_del), on = "object_id"][!J(id_del), on = "src_object_id"]

    list(object = dt_object[, .SD, .SDcols = names(idf_env$object)],
         value = dt_value[, .SD, .SDcols = names(idf_env$value)],
         reference = dt_reference
    )
}
# }}}
# paste_idf_object {{{
paste_idf_object <- function (idd_env, idf_env, ver, in_ip = FALSE, unique = TRUE) {
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
        assert(is_scalar(cls))

        cols <- c("object_id", "class_id", "field_index", "value")

        # extract all object values in the same class
        # in order to distinguish input from original IDF, set id of objects
        # from IDF to negative
        # also note that dcast will automatically order object id, so this makes
        # that input objects are always in the bottom.
        val_idf <- idf_env$value[J(cls), on = "class_id",
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
                    get_object_info(del, "name", collapse = "\n")
                }
            )
            # NOTE: for references, as have to check gloally to get the latest
            # references, there is no need to exclude objects here
            parsed$object <- parsed$object[!J(obj), on = "object_id"]
            parsed$value <- parsed$value[!J(obj), on = "object_id"]
        }
    }

    # add rleid for validation
    add_rleid(parsed$object)
    add_rleid(parsed$object, "object")

    # update id
    parsed$object <- assign_new_id(idf_env, parsed$object, "object")
    parsed$value <- assign_new_id(idf_env, parsed$value, "value")
    parsed$value <- correct_obj_id(parsed$object, parsed$value)
    set(parsed$value, NULL, "rleid", parsed$value$object_id)
    set(parsed$value, NULL, "object_rleid", parsed$value$object_id)

    # add field attributes for validation
    parsed$value <- t_value_fill_attr(idd_env, parsed$value)
    set(parsed$value, NULL, "defaulted", FALSE)

    # validate
    assert_valid(idd_env, idf_env, parsed$object, parsed$value, action = "add")

    # get field reference in whole fields
    parsed$reference <- get_value_reference_map(idd_env$reference,
        append_dt(idf_env$value, parsed$value, "value_id"),
        append_dt(idf_env$value, parsed$value, "value_id")
    )

    parsed
}
# }}}
# insert_idf_object {{{
insert_idf_object <- function (idd_env, idf_env, object) {

}
# }}}
# purge_idf_object {{{
purge_idf_object <- function (idd_env, idf_env, ...) {

}
# }}}
# get_object_input {{{
get_object_input <- function (idd_env, idf_env, l, keep_duplicate = TRUE) {
    setnames(l$id, "id", "object_id")
    setnames(l$name, "id", "object_name")
    # match
    if (nrow(l$id)) {
        obj_id <- get_idf_object(idf_env, object = l$id)
    } else {
        obj_id <- idf_env$object[0L]
        set(obj_id, NULL, c("rleid", "new_name"), list(integer(0L), character(0L)))
    }
    if (nrow(l$name)) {
        obj_nm <- get_idf_object(idf_env, object = l$name)
    } else {
        obj_nm <- idf_env$object[0L]
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

# IDF Editor Integration
# read_idfeditor_copy {{{
read_idfeditor_copy <- function (ver = NULL, in_ip = FALSE) {
    if (Sys.info()["sysname"] == "Darwin") {
        clp <- pipe("pbpaste")
    } else {
        clp <- "clipboard"
    }

    text <- readLines(clp, warn = FALSE)

    if (!stringi::stri_startswith_fixed(text, "IDF,")) {
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

# SAVE
# save_idf {{{
save_idf <- function (idd_env, idf_env, dt_order, path, in_ip = FALSE,
                      format = c("sorted", "new_top", "new_bot"),
                      overwrite = FALSE, copy_external = TRUE) {
    format <- match.arg(format)

    assert(has_ext(path, "idf"))

    if (file.exists(path)) {
        new_file <- FALSE
        if (!overwrite) {
            abort("error_not_overwrite_idf",
                paste0(
                    "Target IDF file already exists. Please set `overwrite` to ",
                    "TRUE if you want to replace it."
                )
            )
        } else {
            verbose_info("Replace the existing IDF located at ", normalizePath(path), ".")
        }
    } else {
        d <- dirname(path)
        if (!dir.exists(d)) {
            tryCatch(dir.create(d, recursive = TRUE),
                warning = function (w) {
                    abort("error_create_idf_dir", paste0("Failed to create directory ", surround(d), "."))
                }
            )
        }
        new_file <- TRUE
    }

    # inorder to skip color control sequence
    clr <- .globals$color
    on.exit({.globals$color <- clr}, add = TRUE)
    .globals$color <- FALSE

    str <- format_output(idf_env$value, idf_env$object, dt_order, in_ip = in_ip,
        header = TRUE, comment = TRUE, save_format = format)

    write_lines(str, path)

    if (!new_file && overwrite) {
        verbose_info("Replace the existing EPW file located at ", normalizePath(path), ".")
    }
    path
}
# }}}
# resolve_idf_external_link {{{
#  auto change full file path in `Schedule:File` to relative path and copy those
#  files into the same directory of the model
resolve_idf_external_link <- function (idd_env, idf_env, old, new, copy = TRUE) {
    # Currently, only `Schedule:File` class is supported
    if (!"Schedule:File" %in% idf_env$object$class_name) return(FALSE)

    # restore current working directory
    ori <- getwd()
    on.exit(setwd(ori), add = TRUE)

    # get full path of old and new
    old_dir <- normalizePath(dirname(old), mustWork = FALSE)
    new_dir <- normalizePath(dirname(new), mustWork = FALSE)

    # get object table and value table
    val <- get_idf_value_in_class(idd_env, idf_env, "Schedule:File", "File Name", fill = TRUE)

    # check existence of old files
    setwd(old_dir)
    val[, old_full_path := normalizePath(value, mustWork = FALSE)]
    val[, old_exist := file.exists(old_full_path)]

    # stop if old file does not exist
    if (nrow(val[old_exist == FALSE])) {
        on.exit(options(warning.length = getOption("warning.length")), add = TRUE)
        options(warning.length = 8170)

        m <- paste0(format_objects(val, "field", leading = 4L), collapse = "\n")

        warn("warning_broken_file_link",
            paste0("Broken external file link found in IDF:\n\n", m)
        )
    }

    val[, same_dir := normalizePath(dirname(old_full_path)) == new_dir]

    # find files to copy
    val <- val[old_exist == TRUE & same_dir == FALSE]

    if (!nrow(val)) return(FALSE)

    # copy external files and change values to relative paths
    if (!copy) {
        val[, new_value := old_full_path]
    # change all paths to full paths
    } else {
        val[, file_name := basename(value)]
        val[, new_full_path := normalizePath(file.path(new_dir, file_name), mustWork = FALSE)]
        val[, new_value := file_name]

        # copy files
        val[, copied := file.copy(old_full_path, new_full_path, overwrite = TRUE, copy.date = TRUE)]
        if (nrow(val[copied == FALSE])) {
            on.exit(options(warning.length = getOption("warning.length")), add = TRUE)
            options(warning.length = 8170)

            m <- paste0(format_objects(val[copied == FALSE], "field", leading = 4L), collapse = "\n")

            abort("error_failed_to_copy",
                paste0("Failed to copy external file into the output directory ",
                    surround(new_dir), ":\n", m, collapse = "\n"
                )
            )
        }
    }

    # update object value table
    idf_env$value[val, on = "value_id", value := val$new_value]

    # update field reference
    if (nrow(val[type_enum == IDDFIELD_TYPE$object_list | src_enum > IDDFIELD_SOURCE$none])) {
        idf_env$reference <- get_value_reference_map(idd_env$reference,
            append_dt(idf_env$value, val, "value_id"),
            append_dt(idf_env$value, val, "value_id")
        )
    }

    TRUE
}
# }}}

# MISC
# assign_new_id {{{
assign_new_id <- function (dt_idf, dt, type = c("object", "value"), keep = FALSE) {
    type <- match.arg(type)
    col <- paste0(type, "_id")
    if (!keep) {
        set(dt, NULL, col, new_id(dt_idf[[type]], col, nrow(dt)))
    } else {
        dt[is.na(get(col)), `:=`(value_id = new_id(dt_idf[[type]], col, .N))]
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
# merge_idf_data {{{
merge_idf_data <- function (dt_idf, dt) {
    assert(is.environment(dt_idf))
    assert(has_name(dt, c("object", "value", "reference")))
    dt_idf$object <- append_dt(dt_idf$object, dt$object, "object_id")
    dt_idf$value <- append_dt(dt_idf$value, dt$value, "value_id")
    dt_idf$reference <- dt$reference

    dt_idf
}
# }}}
