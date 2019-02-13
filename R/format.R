#' @importFrom cli boxx rule cat_line symbol
#' @importFrom crayon red green cyan yellow magenta bold
#' @importFrom utils capture.output
NULL

# format_header: return header of an Idf output {{{
format_header <- function (save_format = c("sorted", "new_top", "new_bot"),
                           view_in_ip = FALSE, special_format = FALSE) {
    save_format <- switch(match.arg(save_format),
        sorted = "SortedOrder",
        new_top = "OriginalOrderTop",
        new_bot = "OriginalOrderBottom")

    header_generator <- "!-Generator eplusr"

    header_option <- paste0("!-Option ", save_format)

    special_format <- NULL
    if (special_format) {
        warn("warning_special_format",
            paste0("Currently, special format for classes such as ",
                "single line formating for vertices are not support. ",
                "All objects will be formatted in standard way."
            )
        )
    }
    if (view_in_ip) in_ip <- "ViewInIPunits" else in_ip <- NULL

    header_option <- stri_trim_right(paste0(header_option, " ", special_format, " ", in_ip))

    # TODO: Add "UseSpecialFormat" support
    c(
        header_generator,
        header_option,
        "",
        "!-NOTE: All comments with '!-' are ignored by the IDFEditor and are generated automatically.",
        "!-      Use '!' comments if they need to be retained when using the IDFEditor."
    )
}
# }}}

# format_output: return whole Idf output {{{
format_output <- function (
    dt_value, dt_object = NULL, dt_order = NULL,
    header = TRUE, comment = TRUE, save_format = c("sorted", "new_top", "new_bot"),
    special_format = FALSE, leading = 4L, in_ip = FALSE, sep_at = 29L, index = FALSE,
    blank = FALSE, end = TRUE, required = FALSE
)
{
    assert(has_name(dt_value, c("object_id", "class_id", "class_name", "field_index")))

    save_format <- match.arg(save_format)
    assert(
        is_count(leading),
        is_flag(in_ip),
        is_count(sep_at),
        is_flag(index),
        is_flag(blank),
        is_flag(end),
        is_flag(required)
    )

    # object order {{{
    if (save_format == "sorted") {
        setorderv(dt_value, c("class_id", "object_id", "field_index"))
    } else {
        assert(!is.null(dt_order))
        dt_value <- dt_value[dt_order, on = "object_id"]
        if (save_format == "new_top") {
            setorderv(dt_value,
                c("object_order", "object_id", "class_id", "field_index"),
                c(-1L, 1L, 1L, 1L)
            )
        } else {
            setorderv(dt_value,
                c("object_order", "object_id", "class_id", "field_index"),
                c(1L, 1L, 1L, 1L)
            )
        }
        set(dt_value, NULL, "object_order", NULL)
    }
    # }}}

    # get field value
    fld <- format_field(dt_value, leading = leading, sep_at = sep_at,
        index = index, blank = blank, end = end, required = required)

    # init output as field values
    set(dt_value, NULL, "output", fld)
    on.exit({set(dt_value, NULL, "output", NULL);invisible()}, add = TRUE)

    # add class name per object
    dt_value[field_index == 1L, output := paste0(class_name, ",\n", output)]

    # add comments
    if (comment) {
        assert(!is.null(dt_object))
        cmt <- format_comment(dt_object)
        dt_value[cmt, on = "object_id", mult = "first",
            output := paste0(cmt$comment, "\n", output)
        ]
    }

    # add class heading for sorted format
    if (save_format == "sorted") {
        dt_value[dt_value[, .I[1L], by = list(class_id)]$V1,
            output := paste0(
                "\n",
                "!-   ===========  ALL OBJECTS IN CLASS: ", stri_trans_toupper(class_name), " ===========",
                "\n\n",
                output
            )
        ]
    } else {
        dt_value[field_index == 1L, output := paste0("\n", output)]
    }

    # add blank line after each object
    dt_value[dt_value[, .I[.N], by = "object_id"]$V1, output := paste0(output, "\n")]

    if (header)
        h <- format_header(save_format = save_format, view_in_ip = in_ip, special_format = special_format)
    else h <- NULL

    unlist(stringi::stri_split_lines(c(h, dt_value$output), omit_empty = FALSE),
        recursive = FALSE, use.names = FALSE
    )
}
# }}}

# format_refmap: return pretty formatted tree string for IdfObjectRefMap {{{
format_refmap <- function (ref_map, in_ip = FALSE) {
    assert(inherits(ref_map, "IdfObjectRefMap"))

    ref_by <- format_refmap_sgl(ref_map$reference_by, "by", in_ip)
    ref_from <- format_refmap_sgl(ref_map$reference_from, "from", in_ip)

    out <- c(ref_by, ref_from)
    return(out)
}
# }}}

# format_refmap_sgl: return pretty formatted tree string for IdfObjectRefMap for one direction {{{
format_refmap_sgl <- function (ref_sgl, type = c("by", "from"), in_ip = FALSE) {
    type <- match.arg(type)
    if (not_empty(ref_sgl)) {

        nms <- names(ref_sgl)
        prefix <- switch(type, by = "target_", from = "reference_")
        is_other <- setdiff(grep(prefix, nms, fixed = TRUE), paste0(prefix, "object"))
        self <- ref_sgl[, .SD, .SDcols = nms[-is_other]]
        other <- ref_sgl[, .SD, .SDcols = c("value_id", nms[is_other])]
        setnames(other,
            c("ori_value_id", gsub(prefix, "", nms[is_other], fixed = TRUE)))
        sp_self <- split(unique(self), by = "value_id")
        sp_other <- split(other, by = "ori_value_id")
        fld_self <- lapply(sp_self, format_objects, in_ip = in_ip)
        fld_other <- lapply(sp_other, function (x) {
            c(paste0(format_objects(x, in_ip = in_ip), "  "), "")
        })
        fld_list <- mapply(function (x, y) {
            c(x, cli::boxx(y, padding = 0L, float = "left", align = "left"))
        }, fld_self, fld_other, SIMPLIFY = TRUE)
        fld <- unlist(fld_list, use.names = FALSE)
    } else {
        fld <- "  <NONE>"
    }
    header_str <- switch(type,
        by = "* Fields feferenced by other objects *",
        from = "* Fields referencing other objects *")
    header <- cli::rule(center = header_str, line = 1)
    out <- c("", header, fld)
    return(out)
}
# }}}

# format_objects: return pretty formatted tree string for mutiple IdfObjects {{{
format_objects <- function (dt_value, zoom = c("group", "class", "object", "field"), leading = 0L) {
    zoom <- match.arg(zoom)

    spcs <- stringi::stri_dup(" ", leading)

    # tree_chars {{{
    # reference: https://github.com/r-lib/cli/blob/master/R/tree.R#L111
    tree_chars <- function() {
        if (l10n_info()$`UTF-8`) {
            list("v" = "\u2502",
                 "h" = "\u2500",
                 "p" = "\u251C",
                 "l" = "\u2514",
                 "j" = "\u251C"
            )
        } else {
            list("v" = "|",
                 "h" = "-",
                 "p" = "+",
                 "l" = "\\",
                 "j" = "|"
            )
        }
    }
    # }}}
    # tree {{{
    tree <- function (dt, leaf, branch, trunk = NULL, sep = FALSE) {
        char <- tree_chars()

        if (sep) {
            set(dt, NULL, "prefix", paste0(char$v, " \n", stringi::stri_dup(" ", leading), char$v, char$h))
        } else {
            set(dt, NULL, "prefix", paste0(char$v, char$h))
        }

        dt[dt[, .I[1L], by = c(branch[1L])]$V1, prefix := paste0(char$p, char$h)]
        dt[dt[, .I[.N], by = c(branch[1L])]$V1, prefix := paste0(char$l, char$h)]

        if (sep) {
            dt[setdiff(
                dt[, .I[.N], by = c(branch[1L])]$V1,
                dt[, .I[1L], by = c(branch[1L])]$V1),
            prefix := paste0(char$v, " \n", stringi::stri_dup(" ", leading), prefix)]
        }

        if (!is.null(trunk)) {
            set(dt, NULL, "branch_prefix", paste0(char$v, " "))
            dt[J(dt[, get(branch[1L])[.N], by = c(trunk[1L])]$V1), on = c(branch[1L]), branch_prefix := "  "]
            set(dt, NULL, "prefix", paste(dt$branch_prefix, dt$prefix))
            set(dt, NULL, "branch_prefix", NULL)
        }

        set(dt, NULL, "tree", paste0(stringi::stri_dup(" ", leading), dt$prefix, " ", dt[[leaf]]))
        set(dt, NULL, "prefix", NULL)

        dt
    }
    # }}}

    if (zoom %in% c("group", "class")) assert(has_name(dt_value, "group_name"))
    if (zoom %in% c("object", "field")) assert(has_name(dt_value, "object_name"))

    if (zoom == "group") {
        # {{{
        dt <- unique(dt_value[, .SD, .SDcols = c("group_id", "group_name", "class_id")])
        setorderv(dt, c("group_id"))
        dt <- dt[, list(group_name = group_name[1L], num = length(class_id)), by = c("group_id")][
            , tree := paste0(spcs, "[", lpad(num, "0"), "] Group: ", group_name)]
        # }}}
    } else if (zoom == "class") {
        # {{{
        dt <- unique(dt_value[, .SD, .SDcols = c("group_id", "group_name", "class_id", "class_name", "object_id")])
        setorderv(dt, c("group_id", "class_id"))
        dt <- dt[,
            list(group_id = group_id[1L], group_name = group_name[1L],
                 class_name = class_name[1L], num = length(object_id)
            ), by = c("class_id")
        ][, class := paste0(spcs, "[", lpad(num, "0"), "] Class: ", class_name)]

        dt <- tree(dt, "class", "group_id")

        dt <- dt[, list(tree = paste0(spcs, "Group: ", group_name[1L], "\n", paste0(tree, collapse = "\n"))), by = "group_id"]
        dt[-1L, tree := paste0("\n", tree)]
        # }}}
    } else if (zoom == "object") {
        # {{{
        dt <- unique(dt_value[, .SD, .SDcols = c("group_id", "group_name", "class_id", "class_name", "object_id", "object_name")])
        setorderv(dt, c("group_id", "class_id", "object_id"))
        set(dt, NULL, "object", t_object_info(dt, c("name", "id"), numbered = FALSE, name_prefix = FALSE))
        dt <- tree(dt, "object", "class_id", "group_id", leading = 0L)

        dt <- dt[,
            list(group_name = group_name[1L],
                 group_id = group_id[1L],
                 tree = paste0(spcs, "Class: ", class_name[1L], "\n", paste0(tree, collapse = "\n"))
            ),
            by = "class_id"
        ]

        dt <- tree(dt, "tree", branch = "group_id", sep = TRUE)
        dt <- dt[, list(tree = paste0(spcs, "Group: ", group_name, "\n", paste0(tree, collapse = "\n"))), by = "group_id"]
        dt[-1L, tree := paste0("\n", tree)]
        # }}}
    } else {
        # {{{
        setorderv(dt_value, c("class_id", "object_id", "field_index"))
        set(dt_value, NULL, "field",
            format_field(dt_value, leading = 1L, sep_at = 20L, index = TRUE, blank = TRUE)
        )
        on.exit({set(dt_value, NULL, "field", NULL);invisible()}, add = TRUE)

        dt <- tree(dt_value, "field", "object_id", "class_id")

        dt <- dt[,
            list(class_name = class_name[1L],
                 class_id = class_id[1L],
                 object_name = object_name[1L],
                 tree = paste0(tree, collapse = "\n")
            ),
            by = "object_id"
        ]

        set(dt, NULL, "object",
            paste0(
                t_object_info(dt, c("name", "id"), numbered = FALSE, name_prefix = FALSE),
                "\n",
                dt$tree
            )
        )

        dt <- tree(dt, "object", "class_id", sep = TRUE)

        dt <- dt[,
            list(tree = paste0(
                spcs, "Class: ", class_name[1L], "\n", paste0(tree, collapse = "\n")
            )),
            by = "class_id"
        ]

        dt[-1L, tree := paste0("\n", tree)]
        # }}}
    }

    dt$tree
}
# }}}

# format_field: return Idf format field {{{
format_field <- function (dt_value, leading = 4L, sep_at = 29L,
                          index = FALSE, blank = FALSE, end = TRUE, required = FALSE) {

    idx <- NULL

    if (index) {
        idx <- paste0(format_index(dt_value, required = required), ":")
    }

    val <- format_value(dt_value, leading = leading, length = sep_at, blank = blank, end = end)
    nm <- format_name(dt_value)

    paste0(idx, val, nm)
}
# }}}

# format_index: return right aligned field index {{{
format_index <- function (dt_value, required = FALSE) {
    if (required) assert(has_name(dt_value, "required_field"))

    idx <- lpad(dt_value$field_index)

    if (required) {
        req <- rep(" ", nrow(dt_value))
        req[dt_value$required_field] <- "*"
        idx <- paste0(idx, req)
    }

    idx
}
# }}}

# format_value: return Idf format value strings {{{
format_value <- function (dt_value, leading = 4L, length = 29L, blank = FALSE,
                          end = TRUE) {
    values <- dt_value$value
    if (blank) {
        values[is.na(values)] <- s_blk("<Blank>")
    } else {
        values[is.na(values)] <- ""
    }

    if (!end) {
        res <- paste0(values, ",")
    } else {
        if (has_name(dt_value, "object_id")) {
            is_end <- dt_value[, .I[.N], by = object_id]$V1
        } else {
            is_end <- length(values)
        }
        res <- character(length(values))
        res[is_end] <- paste0(values[is_end], ";")
        res[-is_end] <- paste0(values[-is_end], ",")
    }

    res <- paste0(stringi::stri_dup(" ", leading), res)

    res <- rpad(res, width = length)
    res[nchar(res) > length] <- paste0(res[nchar(res) > length], "  ")

    res
}
# }}}

# format_name: return Idf format field names {{{
format_name <- function (field_tbl) {
    s_nm(paste0("!- ", field_tbl[["full_name"]]))
}
# }}}

# format_comment: return Idf format comments and macros {{{
# NOTE: return a data.table
format_comment <- function (dt_object) {
    if (is.null(dt_object) || !nrow(dt_object)) return(NULL)

    dt_object[!vapply(comment, function (x) all(stri_isempty(x)), logical(1L))][,
        list(comment = paste0("!", unlist(comment, use.names = FALSE), collapse = "\n")),
        by = "object_id"
    ]
}
# }}}

# update_value_num: update value string and digits {{{
update_value_num <- function (value_tbl, digits = 8L, in_ip = FALSE, prefix = "value") {
    val_suffix <- c("", "_upper", "_num", "_ipnum")
    req_val <- paste0(prefix, val_suffix)
    assert(has_name(value_tbl, req_val))

    # add unit conversion data if necessary
    joined_before <- TRUE
    if (!has_name(value_tbl, c("si_name", "mult", "offset"))) {
        joined_before <- FALSE
        assert(has_name(value_tbl, c("type", "units", "ip_units")))
        value_tbl <- unit_conv_table[value_tbl, on = c(si_name = "units", ip_name = "ip_units")]
    }
    setnames(value_tbl,
        c("si_name", "ip_name", req_val),
        c("units", "ip_units", paste0("value", val_suffix)))

    if (in_ip) {
        value_tbl[
            !is.na(value_ipnum) & type == "real",
            `:=`(value = as.character(round(value_ipnum, digits = digits)))][
            !is.na(value_ipnum) & type == "integer" & is_integer(value_ipnum),
            `:=`(value = as.character(round(value_ipnum)))][
            !is.na(value_ipnum) & !is.na(units),
            `:=`(value_num = value_ipnum / mult - offset)]
    } else {
        value_tbl[
            !is.na(value_num) & type == "real",
            `:=`(value = as.character(round(value_num, digits = digits)))][
            !is.na(value_num) & type == "integer" & is_integer(value_num),
            `:=`(value = as.character(round(value_num)))][
            !is.na(value_num) & !is.na(units),
            `:=`(value_ipnum = value_num * mult + offset)]
    }

    value_tbl[, `:=`(value_upper = toupper(value))]

    if (prefix != "value")
        setnames(value_tbl, paste0("value", val_suffix), req_val)

    if (joined_before)
        setnames(value_tbl, c("units", "ip_units"), c("si_name", "ip_name"))

    value_tbl
}
# }}}

# value_list: return a list of field values with correct types {{{
value_list <- function (value_tbl, in_ip = FALSE) {
    assert(has_name(value_tbl, c("value", "value_num", "value_ipnum", "type")))

    num_col <- ifelse(in_ip, "value_ipnum", "value_num")

    value_tbl[, out := as.list(value)]
    value_tbl[type %in% c("integer", "real") & !value_upper %in% c("AUTOSIZE", "AUTOCALCULATE"),
        out := list(as.list(get(num_col)))]

    res <- value_tbl$out

    if (is_scalar(res)) return(as.list(res))

    res
}
# }}}

# print.IdfObjectRefMap {{{
print.IdfObjectRefMap <- function (x, ...) {
    cli::cat_line(format_refmap(x, ...))
}
# }}}

#' @export
# print.IddFieldPossible {{{
print.IddFieldPossible <- function (x, ...) {
    dt <- data.table::copy(x)

    dt[, header := cli::rule(crayon::bold(paste0(
        field_index, ": Field ", surround(field_name))), col = "green"),
        by = field_index
    ]

    dt[-1L, header := paste0("\n", header)]

    dt[, res := header]

    dt[!is.na(auto),
        res := paste0(res, "\n", crayon::cyan(paste0(
            cli::symbol$bullet, " ", crayon::bold("Auto value"), ": ", surround(auto))))]

    dt[!vapply(default, is.na, logical(1)),
        res := paste0(res, "\n", crayon::cyan(paste0(
            cli::symbol$bullet, " ", crayon::bold("Default"), ": ",
            ifelse(is.character(default), surround(default), default)))
        )
    ]

    dt[!vapply(choice, function (x) all(is.na(x)), logical(1)),
        res := paste0(res, "\n", crayon::cyan(paste0(
            cli::symbol$bullet, " ", crayon::bold("Choice"), ":\n" ,
            paste0("  - ", surround(unlist(choice)), collapse = "\n")))),
        by = field_index
    ]

    dt[, res_ran := paste0(vapply(range, function (x) capture.output(print.IddRanger(x)), character(1)))]
    dt[res_ran == "<Not Applicable>", res_ran := NA_character_]
    dt[!is.na(res_ran), res := paste0(res, "\n", crayon::cyan(paste0(
            cli::symbol$bullet, " ", crayon::bold("Range"), ": ", res_ran
        ))
    )]

    dt[!vapply(reference, is.null, logical(1)), res := paste0(res, "\n",
        crayon::cyan(paste0(cli::symbol$bullet, " ", crayon::bold("References"), ":\n",
            paste0(paste0("  - ", surround(unlist(reference))), collapse = "\n"))
        )),
        by = field_index
    ]

    dt[res == header, res := paste0(res, "\n", crayon::magenta("<Not Applicable>"))]
    cli::cat_line(dt$res)
}
# }}}

#' @export
# format.Range {{{
format.Range <- function (x, ...) {
    if (is.na(x$minimum) && is.na(x$maximum)) {
        return("<Not Applicable>")
    }

    if (!is.na(x$minimum)) {
        if (x$lower_incbounds) {
            left <- paste0("[", x$minimum)
        } else {
            left <- paste0("(", x$minimum)
        }
    } else {
        left <- "(-Inf"
    }

    if (!is.na(x$maximum)) {
        if (x$upper_incbounds) {
            right <- paste0(x$maximum, "]")
        } else {
            right <- paste0(x$maximum, ")")
        }
    } else {
        right <- "Inf)"
    }

    paste0(left, ", ", right)
}
# }}}
#' @export
# print.Range {{{
print.Range <- function (x, ...) {
    format.Range(x, ...)
    if (is.na(x$minimum) && is.na(x$maximum)) {
        cat("<Not Applicable>")
        return(invisible(x))
    }

    if (!is.na(x$minimum)) {
        if (x$lower_incbounds) {
            left <- paste0("[", x$minimum)
        } else {
            left <- paste0("(", x$minimum)
        }
    } else {
        left <- "(-Inf"
    }

    if (!is.na(x$maximum)) {
        if (x$upper_incbounds) {
            right <- paste0(x$maximum, "]")
        } else {
            right <- paste0(x$maximum, ")")
        }
    } else {
        right <- "Inf)"
    }

    cli::cat_line(paste0(left, ", ", right))
    invisible(x)
}
# }}}
#' @export
# as.character.Range{{{
as.character.Range <- function (x, ...) {
    format.Range(x, ...)
}
# }}}

# STYLE
# has_color {{{
has_color <- function () {
    requireNamespace("crayon") && crayon::has_color()
}
# }}}
# s_req: style for indices of required fields {{{
s_req <- function (...) if (.globals$color) crayon::red$bold(...) else c(...)
# }}}
# s_nm: style for field names {{{
s_nm <- function (...) if (.globals$color) crayon::silver(...) else c(...)
# }}}
# s_blk: style for blank {{{
s_blk <- function (...) if (.globals$color) crayon::underline(...) else c(...)
# }}}
