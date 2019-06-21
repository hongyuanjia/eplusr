#' @importFrom cli cat_bullet cat_line cat_rule rule symbol
#' @importFrom data.table copy data.table dcast rbindlist
#' @importFrom data.table setattr setcolorder setnames setorder setorderv
#' @importFrom stringi stri_locate_first_regex stri_replace_first_regex "stri_sub<-"
#' @importFrom stringi stri_subset_regex stri_match_first_regex stri_rand_strings
#' @include parse.R
NULL

# IDF OBJECT
# set_idfobj_comment {{{
set_idfobj_comment <- function (idd_env, idf_env, object_id, comment, append = TRUE, width = 0L) {
    obj <- get_idf_object(idd_env, idf_env, object = object_id)

    if (is.null(comment)) {
        set(obj, NULL, "comment", list(list(NULL)))
    } else {
        comment <- as.character(comment)
        assert(is_count(width, TRUE))

        cmt <- unlist(strsplit(comment, "\n", fixed = TRUE), use.names = FALSE)

        if (width != 0L) {
            cmt <- strwrap(cmt, width = width)
        }

        if (is.null(append)) {
            # reset
            set(obj, NULL, "comment", list(list(cmt)))
        } else {
            assert(is_flag(append), msg = "`append` should be NULL or a single logical value.")
            if (append) {
                # add new
                set(obj, NULL, "comment", list(list(c(obj$comment[[1L]], cmt))))
            } else {
                set(obj, NULL, "comment", list(list(c(cmt, obj$comment[[1L]]))))
            }

        }
    }

    obj
}
# }}}
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
# get_value_list: return a list of field values with correct types {{{
get_value_list <- function (dt_value, unit = FALSE) {
    assert(has_name(dt_value, c("value_chr", "value_num", "type_enum")))

    res <- as.list(dt_value[["value_chr"]])
    num <- dt_value$type_enum <= IDDFIELD_TYPE$real &
        !stri_trans_tolower(dt_value$value_chr) %chin% c("autosize", "autocalculate")

    val <- dt_value$value_num
    is_double <- dt_value$type_enum == IDDFIELD_TYPE$real
    exists <- !is.na(val)
    res[num & is_double] <- val[num & is_double]
    res[num & !is_double] <- as.integer(val[num & !is_double])

    if (any(unlist(res[exists & num & !is_double]) != val[exists & num & !is_double])) {
        warn("warning_value_int_trunc",
            paste0("Truncated error introduced when converting value of field ",
                collapse(dt_value[["field_name"]][res[num & !is_double] != val[num & !is_double]]),
                " to integer. This often indicates that integer fields have ",
                "decimal numbers. Please see `$validate()` for details."
            )
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

            unit <- UNIT_CONV_TABLE[J(dt_value[[input]][u]), on = c(paste0(prefix, "_name")),
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
# set_idfobj_value {{{
set_idfobj_value <- function (idd_env, idf_env, object, ..., .default = TRUE) {
    nm <- if (is_count(object)) paste0("..", object) else object

    input <- list(...)

    # if single input
    if (length(input) == 1L) {
        # if input is an atomic
        if (is.atomic(input[[1L]])) {
            # for `<-.IdfObject`
            if (length(input[[1L]]) == 1L && is_named(input[[1L]])) {
                input <- list(as.list(input[[1L]]))
            # for `$set(field = value)`
            } else {
                input <- list(input)
            }
        # for `$set(list(field1 = value1, field2 = value2))`
        } else {
            # check for .comment
            if (has_name(input[[1L]], ".comment")) {
                abort("error_idfobj_dotcomment",
                    paste0(
                        "Using `.comment` to set object comments is prohibited in ",
                        "`IdfObject` class. Please use `$comment()` method instead."
                    )
                )
            }
        }
    # for `$set(field1 = value1, field2 = value2)`
    } else {
        # check for .comment
        if (has_name(input, ".comment")) {
            abort("error_idfobj_dotcomment",
                paste0(
                    "Using `.comment` to set object comments is prohibited in ",
                    "`IdfObject` class. Please use `$comment()` method instead."
                )
            )
        }
        input <- list(input)
    }

    setattr(input, "names", nm)

    set_idf_object(idd_env, idf_env, input, .default = .default)
}
# }}}
# get_idfobj_possible {{{
get_idfobj_possible <- function (idd_env, idf_env, object, field,
                                 type = c("auto", "default", "choice", "range", "source")) {
    all_type <- c("auto", "default", "choice", "range", "source")
    assert(no_na(chmatch(type, all_type)), msg = paste0("`type` should be one or some of ", collapse(all_type)))

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

        val <- field_default_to_unit(val, "si", if (in_ip_mode()) "ip" else "si")
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
        src <- get_idd_relation(idd_env, NULL, val$field_id, max_depth = 0L, direction = "ref_to")
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
                                 keep_all = FALSE, by_value = FALSE, max_depth = 0L,
                                 recursive = FALSE, recursive_depth = 1L) {
    all_dir <- c("ref_to", "ref_by", "node", "all")
    direction <- all_dir[sort(chmatch(direction, all_dir))]
    assert(no_na(direction), msg = paste0("`direction` should be one or some of ", collapse(all_dir)))

    rel <- list(ref_to = NULL, ref_by = NULL, node = NULL)

    if ("all" %in% direction) direction <- unique(c(direction, all_dir))

    if ("ref_to" %in% direction) {
        rel$ref_to <- get_idf_relation(idd_env, idf_env, object_id, value_id,
            max_depth = max_depth, name = name, direction = "ref_to",
            keep_all = keep_all, recursive = recursive, recursive_depth = recursive_depth
        )
        setattr(rel$ref_to, "by_value", by_value)
    }

    if ("ref_by" %in% direction) {
        rel$ref_by <- get_idf_relation(idd_env, idf_env, object_id, value_id,
            max_depth = max_depth, name = name, direction = "ref_by",
            keep_all = keep_all, recursive = recursive, recursive_depth = recursive_depth
        )
        setattr(rel$ref_by, "by_value", by_value)
    }

    if ("node" %in% direction) {
        rel$node <- get_idf_node_relation(idd_env, idf_env, object_id, value_id,
            name = name, keep_all = keep_all, recursive = recursive, recursive_depth = recursive_depth
        )
        setattr(rel$node, "by_value", by_value)
    }


    setattr(rel, "class", c("IdfRelation", class(rel)))

    rel
}
# }}}

# get_idfobj_table {{{
get_idfobj_table <- function (idd_env, idf_env, object_id, all = FALSE,
                              unit = TRUE, wide = FALSE, string_value = TRUE) {
    get_idf_table(idd_env, idf_env, NULL, object_id, all = all, unit = unit,
        wide = wide, string_value = string_value
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

# helper
# merge_idfobj_data {{{
merge_idfobj_data <- function (idf_env, dt, type = c("object", "value", "reference")) {
    type <- match.arg(type)
    if (type != "reference") {
        idf_env[[type]] <- append_dt(idf_env[[type]], dt, paste0(type, "_id"))
    } else {
        idf_env[[type]] <- dt
    }
    idf_env
}
# }}}
