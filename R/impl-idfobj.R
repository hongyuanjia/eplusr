#' @importFrom cli cat_bullet cat_line cat_rule rule symbol
#' @importFrom data.table copy data.table dcast rbindlist
#' @importFrom data.table setattr setcolorder setnames setorder setorderv
#' @importFrom stringi stri_locate_first_regex stri_replace_first_regex "stri_sub<-"
#' @importFrom stringi stri_subset_regex stri_match_first_regex stri_rand_strings
#' @include parse.R
NULL

# IDF OBJECT
# get_idfobj_value {{{
get_idfobj_value <- function (idd_env, idf_env, object, which = NULL, all = FALSE,
                              simplify = FALSE, unit = FALSE, underscore = FALSE) {
    val <- get_idf_value(idd_env, idf_env, NULL, object, which, NULL, all = all, align = TRUE, underscore = underscore)
    if (simplify) return(val$value_chr)
    add_field_property(idd_env, val, "type_enum")

    if (unit) add_field_property(idd_env, val, c("units", "ip_units"))

    get_value_list(val, unit)
}
# }}}
# get_value_list {{{
#' @importFrom checkmate assert_names
get_value_list <- function (dt_value, unit = FALSE) {
    assert_names(names(dt_value), must.include = c("value_chr", "value_num", "type_enum"))

    res <- as.list(dt_value[["value_chr"]])
    num <- dt_value$type_enum <= IDDFIELD_TYPE$real &
        !stri_trans_tolower(dt_value$value_chr) %chin% c("autosize", "autocalculate")

    val <- dt_value$value_num
    is_double <- dt_value$type_enum == IDDFIELD_TYPE$real
    exists <- !is.na(val)
    res[num & is_double] <- val[num & is_double]
    res[num & !is_double] <- as.integer(val[num & !is_double])

    if (any(unlist(res[exists & num & !is_double]) != val[exists & num & !is_double])) {
        warn(paste0("Truncated error introduced when converting value of field ",
                collapse(dt_value[["field_name"]][res[num & !is_double] != val[num & !is_double]]),
                " to integer. This often indicates that integer fields have ",
                "decimal numbers. Please see '$validate()' for details."
            ), "warning_value_int_trunc"
        )
    }

    if (unit) {
        if (in_ip_mode()) {
            prefix <- "ip"
            input <- "ip_units"
        } else {
            prefix <- "si"
            input <- "units"
        }

        u <- !is.na(dt_value[[input]]) & num

        if (any(u)) {
            col <- paste0(prefix, "_standard_name")

            unit <- FIELD_UNIT_TABLE[J(dt_value[[input]][u]), on = c(paste0(prefix, "_name")),
                mult = "first", .SD, .SDcols = c(col)]

            res[u] <- apply2(res[u], unit[[col]], function (val, unit) {
                units::set_units(val, unit, mode = "standard")
            })
        }
    }
    setattr(res, "names", dt_value$field_name)
    res
}
# }}}
# get_idfobj_possible {{{
#' @importFrom checkmate assert_subset
get_idfobj_possible <- function (idd_env, idf_env, object, field = NULL,
                                 type = c("auto", "default", "choice", "range", "source")) {
    assert_subset(type, c("auto", "default", "choice", "range", "source"), FALSE)

    prop <- c("units", "ip_units", "type_enum")
    if ("auto" %chin% type) prop <- c(prop, "autosizable", "autocalculatable")
    if ("default" %chin% type) prop <- c(prop, "default_chr", "default_num")
    if ("choice" %chin% type) prop <- c(prop, "choice")
    if ("range" %chin% type) prop <- c(prop, "minimum", "lower_incbounds", "maximum", "upper_incbounds")

    val <- get_idf_value(idd_env, idf_env, NULL, object, field, property = prop, align = TRUE)

    # auto fields
    if ("auto" %chin% type) {
        val[J(TRUE), on = "autosizable", `:=`(auto = "Autosize")]
        val[J(TRUE), on = "autocalculatable", `:=`(auto = "Autocalculate")]
        set(val, NULL, c("autosizable", "autocalculatable"), NULL)
    }

    # default
    if ("default" %chin% type) {
        setnames(val, c("value_id", "value_chr", "value_num"),
            c("ori_value_id", "ori_value_chr", "ori_value_num"))

        val <- field_default_to_unit(idd_env, val, "si", if (in_ip_mode()) "ip" else "si")
        setnames(val, c("default_chr", "default_num"), c("value_chr", "value_num"))
        # make sure default is a list
        if (nrow(val) == 1L) {
            set(val, NULL, "default", list(get_value_list(val)))
        } else {
            set(val, NULL, "default", get_value_list(val))
        }

        setnames(val, c("ori_value_id", "ori_value_chr", "ori_value_num"),
            c("value_id", "value_chr", "value_num"))
    }

    # range
    if ("range" %chin% type) {
        val[, `:=`(range = list(ranger(minimum, lower_incbounds, maximum, upper_incbounds))), by = c("value_id")]
    }

    # source
    if ("source" %chin% type) {
        # handle NODE
        node <- val[J(IDDFIELD_TYPE$node), on = "type_enum", nomatch = 0L, list(field_id)]
        if (nrow(node)) {
            add_field_property(idd_env, idf_env$value, "type_enum")
            nodes <- idf_env$value[J(IDDFIELD_TYPE$node), on = "type_enum", unique(value_chr)]
            set(idf_env$value, NULL, "type_enum", NULL)
            set(node, NULL, "source", list(list(nodes)))
        }

        setnames(idf_env$value, "field_id", "src_field_id")
        src <- get_idd_relation(idd_env, NULL, val$field_id, depth = 0L, direction = "ref_to")
        src <- idf_env$value[src, on = c("src_field_id"), allow.cartesian = TRUE, nomatch = 0L]
        # add class name in case of class-name reference
        add_joined_cols(idd_env$class, src, c(src_class_id = "class_id"), c(src_class_name = "class_name"))
        src[J(1L), on = "src_enum", `:=`(value_chr = src_class_name)]
        src <- src[, list(source = list(value_chr)), by = "field_id"]
        setnames(idf_env$value, "src_field_id", "field_id")

        add_joined_cols(rbindlist(list(node, src), fill = TRUE), val, "field_id", "source")
    }

    res <- val[, .SD, .SDcols = c(
        "class_id", "class_name", "object_id", "object_name",
        "field_id", "field_index", "field_name",
        "value_id", "value_chr", "value_num",
        type
    )]

    setattr(res, "class", c("IdfValuePossible", class(res)))
    res
}
# }}}
# get_idfobj_relation {{{
get_idfobj_relation <- function (idd_env, idf_env, object_id = NULL, value_id = NULL,
                                 name = TRUE, direction = c("ref_to", "ref_by", "node", "all"),
                                 object = NULL, class = NULL, group = NULL,
                                 keep_all = FALSE, depth = 0L, class_ref = c("both", "none", "all")) {
    all_dir <- c("ref_to", "ref_by", "node", "all")
    checkmate::assert_subset(direction, all_dir, FALSE)

    rel <- list(ref_to = NULL, ref_by = NULL, node = NULL)

    if ("all" %in% direction) direction <- unique(c(direction, all_dir))

    if ("ref_to" %in% direction) {
        rel$ref_to <- get_idf_relation(idd_env, idf_env, object_id = object_id, value_id,
            depth = depth, name = name, direction = "ref_to", keep_all = keep_all,
            object = object, class = class, group = group, class_ref = match.arg(class_ref)
        )
    }

    if ("ref_by" %in% direction) {
        rel$ref_by <- get_idf_relation(idd_env, idf_env, object_id = object_id, value_id,
            depth = depth, name = name, direction = "ref_by", keep_all = keep_all,
            object = object, class = class, group = group, class_ref = match.arg(class_ref)
        )
    }

    if ("node" %in% direction) {
        rel$node <- get_idf_node_relation(idd_env, idf_env, object_id = object_id, value_id,
            name = name, keep_all = keep_all, depth = depth,
            object = object, class = class, group = group
        )
    }

    setattr(rel, "class", c("IdfRelation", class(rel)))

    rel
}
# }}}
# get_idfobj_table {{{
get_idfobj_table <- function (idd_env, idf_env, object_id, all = FALSE,
                              unit = TRUE, wide = FALSE, string_value = TRUE, group_ext = "none") {
    get_idf_table(idd_env, idf_env, NULL, object_id, all = all, unit = unit,
        wide = wide, string_value = string_value, group_ext = group_ext
    )
}
# }}}
# get_idfobj_string {{{
get_idfobj_string <- function (idd_env, idf_env, object_id, comment = TRUE, leading = 4L, sep_at = 29L, all = FALSE) {
    val <- get_idf_value(idd_env, idf_env, object = object_id, property = c("units", "ip_units"), all = all)

    cls <- paste0(val$class_name[[1L]], ",")

    # get field value
    fld <- format_field(val, leading = leading, sep_at = sep_at,
        index = FALSE, blank = FALSE, end = TRUE, required = FALSE)

    if (comment) {
        id <- object_id
        cmt <- idf_env$object[J(id), on = "object_id", comment][[1L]]
    } else {
        cmt <- NULL
    }

    if (!is.null(cmt)) cmt <- c(paste0("!", cmt), "")

    c(cmt, cls, fld)
}
# }}}
# set_idfobj_comment {{{
#' @importFrom checkmate assert_count test_flag
set_idfobj_comment <- function (idd_env, idf_env, object_id, comment, append = TRUE, width = 0L) {
    obj <- get_idf_object(idd_env, idf_env, object = object_id)

    if (is.null(comment)) {
        set(obj, NULL, "comment", list(list(NULL)))
    } else {
        comment <- as.character(comment)
        assert_count(width)

        cmt <- unlist(stri_split_fixed(comment, "\n", omit_empty = FALSE), use.names = FALSE)

        if (width != 0L) {
            cmt <- strwrap(cmt, width = width)
        }

        if (is.null(append)) {
            # reset
            set(obj, NULL, "comment", list(list(cmt)))
        } else if (test_flag(append)){
            if (append) {
                # add new
                set(obj, NULL, "comment", list(list(c(obj$comment[[1L]], cmt))))
            } else {
                set(obj, NULL, "comment", list(list(c(cmt, obj$comment[[1L]]))))
            }

        } else {
            abort("'append' should be NULL or a single logical value.")
        }
    }

    obj
}
# }}}
