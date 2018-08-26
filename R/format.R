#' @importFrom stringr str_trim str_replace
#' @importFrom data.table setorder rleid setnames copy
#' @importFrom cli boxx rule cat_line symbol
#' @importFrom data.table setorder
#' @importFrom crayon red green cyan yellow magenta bold
#' @importFrom utils capture.output
NULL

# format_header: return header of an Idf output {{{
format_header <- function (format = c("sorted", "new_top", "new_bottom"), view_in_ip = FALSE) {
    format <- switch(format,
        sorted = "SortedOrder",
        new_top = "OriginalOrderTop",
        new_bottom = "OriginalOrderBottom")

    header_generator <- "!-Generator eplusr"

    header_option <- paste0("!-Option ", format)

    special_format <- NULL
    if (view_in_ip) in_ip <- "ViewInIPunits" else in_ip <- NULL

    header_option <- paste0(header_option, " ", special_format, " ", in_ip)
    header_option <- stringr::str_trim(header_option, "right")

    # TODO: Add "UseSpecialFormat" support
    header <- c(
        header_generator,
        header_option,
        "",
        "!-NOTE: All comments with '!-' are ignored by the IDFEditor and are generated automatically.",
        "!-      Use '!' comments if they need to be retained when using the IDFEditor."
    )

    return(header)
}
# }}}

# format_output: return whole Idf output {{{
format_output <- function (value_tbl, comment_tbl, ...) {
    assert_that(has_names(value_tbl, c("object_id", "class_id", "class_name", "field_index")))

    dots <- list(...)
    header <- dots$header %||% TRUE
    sav_fmt <- dots$format %||% eplusr_option("save_format")
    leading <- dots$leading %||% 4L
    in_ip <- dots$in_ip %||% eplusr_option("view_in_ip")
    sep_at <- dots$sep_at %||% 29L
    index <- dots$index %||% FALSE
    blank <- dots$blank %||% FALSE
    end <- dots$end %||% TRUE
    required <- dots$required %||% FALSE

    if (sav_fmt == "sorted") {
        data.table::setorder(value_tbl, class_id, object_id, field_index)
    } else if (sav_fmt == "new_top") {
        data.table::setorder(value_tbl, -object_order, object_id, class_id, field_index)
    } else {
        data.table::setorder(value_tbl, object_order, object_id, class_id, field_index)
    }

    # add field output
    fld <- format_field(value_tbl, leading = leading, in_ip = in_ip,
        sep_at = sep_at, index = index, blank = blank, end = end, required = required)

    # add comment output
    if (not_empty(comment_tbl)) {
        has_comment <- TRUE
        comment_tbl[type == 0L, `:=`(comment = paste0("!", comment))]
        comment_tbl <- comment_tbl[, lapply(.SD, paste0, collapse = "\n"),
            .SDcols = "comment", by = object_id]
        tbl <- comment_tbl[value_tbl, on = "object_id"]
    } else {
        has_comment <- FALSE
        tbl <- value_tbl
    }

    tbl[, out := fld]
    tbl[field_index == 1L, out := paste0(class_name, ",\n", out)]
    if (has_comment) {
        tbl[field_index == 1L & !is.na(comment), out := paste0(comment, "\n", out)]
    }

    # add class output
    cls_1 <- tbl[, class_group := .GRP, by = list(data.table::rleid(class_id))][
        , .I[1L], by = list(class_group)]$V1
    if (sav_fmt == "sorted") {
        tbl[cls_1, out := paste0(
            "\n",
            "!-   ===========  ALL OBJECTS IN CLASS: ", toupper(class_name), " ===========",
            "\n\n",
            out)][
            field_index == 1L, out := paste0("\n", out)]
    } else {
        tbl[field_index == 1L, out := paste0("\n", out)]
    }

    if (header)
        h <- format_header(format = sav_fmt, view_in_ip = in_ip)
    else h <- NULL

    c(h, tbl[["out"]])
}
# }}}

# format_refmap: return pretty formatted tree string for IdfObjectRefMap {{{
format_refmap <- function (ref_map, in_ip = FALSE) {
    assert_that(inherits(ref_map, "IdfObjectRefMap"))

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
        data.table::setnames(other,
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
format_objects <- function (value_tbl, in_ip = FALSE) {
    assert_that(has_names(value_tbl, c("class_id", "object_id", "field_index")))
    data.table::setorder(value_tbl, class_id, object_id, field_index)

    fmt_fld <- format_field(value_tbl, leading = 1L, in_ip = in_ip,
                            sep_at = 20L, index = TRUE, blank = TRUE)

    value_tbl[, `:=`(out = as.list(fmt_fld), row_id = .I), by = .I]

    # tree_chars {{{
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
    char <- tree_chars()

    # add field char
    last_field <- value_tbl[, row_id[.N], by = list(class_name, object_id)]$V1
    first_field <- setdiff(value_tbl[, row_id[1L], by = list(class_name, object_id)]$V1, last_field)
    last_object <- value_tbl[object_id %in% value_tbl[, max(object_id), by = list(class_id)]$V1, unique(object_id)]
    value_tbl[!object_id %in% last_object & row_id %in% last_field,
           out := list(list(paste0("  ", crayon::green(char$v), "  ", crayon::green(char$l), crayon::green(char$h), " ", out[[1L]]))), by = row_id]
    value_tbl[!object_id %in% last_object & row_id %in% first_field,
           out := list(list(paste0("  ", crayon::green(char$v), "  ", crayon::green(char$p), crayon::green(char$h), " ", out[[1L]]))), by = row_id]
    value_tbl[!object_id %in% last_object & !row_id %in% c(first_field, last_field),
           out := list(list(paste0("  ", crayon::green(char$v), "  ", crayon::green(char$j), crayon::green(char$h), " ", out[[1L]]))), by = row_id]
    value_tbl[object_id %in% last_object & row_id %in% last_field,
           out := list(list(paste0("     ", crayon::green(char$l), crayon::green(char$h), " ", out[[1L]]))), by = row_id]
    value_tbl[object_id %in% last_object & row_id %in% first_field,
           out := list(list(paste0("     ", crayon::green(char$p), crayon::green(char$h), " ", out[[1L]]))), by = row_id]
    value_tbl[object_id %in% last_object & !row_id %in% c(first_field, last_field),
           out := list(list(paste0("     ", crayon::green(char$j), crayon::green(char$h)," ", out[[1L]]))), by = row_id]

    # add object char
    first_row_per_object <- value_tbl[, row_id[1L], by = list(class_id, object_id)]$V1
    first_row_per_last_object <- value_tbl[object_id %in% last_object, row_id[1], by = object_id]$V1
    value_tbl[setdiff(first_row_per_object, first_row_per_last_object),
           out := list(list(
                c(crayon::green(paste0("  ", char$p, char$h, " Object [ID:", object_id,"]")), out[[1L]]))),
           by = list(row_id)]
    value_tbl[first_row_per_last_object,
           out := list(list(
                c(crayon::green(paste0("  ", char$l, char$h, " Object [ID:", object_id,"]")), out[[1L]]))),
           by = list(row_id)]

    # add class char
    each_class <- value_tbl[, row_id[1L], by = class_id]$V1
    value_tbl[each_class,
           out := list(list(
                c("", crayon::green(paste0("  Class ", backtick(class_name))), out[[1L]]))),
           by = row_id]

    res <- unlist(value_tbl[["out"]])

    return(res)
}
# }}}

# format_field: return Idf format field {{{
format_field <- function (value_tbl, leading = 4L, in_ip = FALSE, sep_at = 29L,
                          index = FALSE, blank = FALSE, end = TRUE, required = FALSE) {

    idx <- NULL
    if (index) {
        value_tbl[, `:=`(idx = lpad(field_index))]

        if (has_name(value_tbl, "required_field")) {
            value_tbl[required_field == TRUE, `:=`(
                idx = crayon::red$bold(idx),
                req = crayon::red$bold(cli::symbol$bullet)
            )]

            value_tbl[required_field == FALSE, `:=`(
                idx = crayon::cyan(idx),
                req = crayon::cyan(strrep(" ", nchar(cli::symbol$bullet)))
            )]
        } else {
            value_tbl[, `:=`(
                idx = crayon::cyan(idx),
                req = crayon::cyan(strrep(" ", nchar(cli::symbol$bullet)))
            )]
        }

        idx <- value_tbl$idx
        if (required) idx <- paste0(value_tbl$req, idx)
        idx <- paste0(idx, ":")
    }

    val <- format_value(value_tbl, leading = leading, length = sep_at, blank = blank, end = end)
    nm <- format_name(value_tbl, in_ip = in_ip)

    if (has_name(value_tbl, "required_field")) {
        nm[value_tbl$required_field] <- crayon::red(nm[value_tbl$required_field])
        nm[!value_tbl$required_field] <- crayon::cyan(nm[!value_tbl$required_field])
    } else {
        nm <- crayon::cyan(nm)
    }

    paste0(idx, crayon::yellow$bold(val), nm)
}
# }}}

# format_index: return right aligned field index {{{
format_index <- function (field_tbl) {
    lpad(field_tbl[["field_index"]])
}
# }}}

# format_value: return Idf format value strings {{{
format_value <- function (value_tbl, leading = 4L, length = 29L, blank = FALSE,
                          end = TRUE) {
    value_tbl[is.na(value), value := ""]
    values <- value_tbl[["value"]]

    if (blank) {
        values[values == ""] <- rep("<Blank>", sum(values == ""))
    }

    if (end) {
        if (has_names(value_tbl, c("object_id", "field_index"))) {
            is_end <- value_tbl[, .I[max(seq_along(field_index))], by = object_id]$V1
        } else {
            is_end <- length(values)
        }
        res <- values
        res[is_end] <- paste0(res[is_end], ";")
        res[-is_end] <- paste0(res[-is_end], ",")
    } else {
        res <- paste0(values, ",")
    }

    res <- paste0(strrep(" ", leading), res)

    long <- nchar(res) > length
    res[long] <- paste0(res[long], "  ")
    res[!long] <- rpad(res[!long], width = length)

    return(res)
}
# }}}

# format_name: return Idf format field names {{{
format_name <- function (field_tbl, in_ip = FALSE) {
    assert_that(has_names(field_tbl, c("full_name", "full_ipname")))
    if (in_ip) {
        paste0("!- ", field_tbl[["full_ipname"]])
    } else {
        paste0("!- ", field_tbl[["full_name"]])
    }
}
# }}}

# format_comment: return Idf format comments and macros {{{
format_comment <- function (comment_tbl) {
    if (is_empty(comment_tbl)) return(NULL)

    assert_that(has_names(comment_tbl, c("comment", "type")))
    out <- comment_tbl[, out := comment][
        type == 0L, out := paste0("!", comment)][["out"]]
    comment_tbl[, out := NULL]

    return(out)
}
# }}}

# update_value_num: update value string and digits {{{
update_value_num <- function (value_tbl, digits = 8L, in_ip = FALSE, prefix = "value") {
    val_suffix <- c("", "_upper", "_num", "_ipnum")
    req_val <- paste0(prefix, val_suffix)
    assert_that(has_names(value_tbl, req_val))

    # add unit conversion data if necessary
    joined_before <- TRUE
    if (!has_names(value_tbl, c("si_name", "mult", "offset"))) {
        joined_before <- FALSE
        assert_that(has_names(value_tbl, c("type", "units", "ip_units")))
        value_tbl <- unit_conv_table[value_tbl, on = c(si_name = "units", ip_name = "ip_units")]
    }
    data.table::setnames(value_tbl,
        c("si_name", "ip_name", req_val),
        c("units", "ip_units", paste0("value", val_suffix)))

    if (in_ip) {
        value_tbl[
            !is.na(value_ipnum) & type == "real",
            `:=`(value = as.character(round(value_ipnum, digits = digits)))][
            !is.na(value_ipnum) & type == "integer" & is_integerish(value_ipnum),
            `:=`(value = as.character(round(value_ipnum)))][
            !is.na(value_ipnum) & !is.na(units),
            `:=`(value_num = value_ipnum / mult - offset)]
    } else {
        value_tbl[
            !is.na(value_num) & type == "real",
            `:=`(value = as.character(round(value_num, digits = digits)))][
            !is.na(value_num) & type == "integer" & is_integerish(value_num),
            `:=`(value = as.character(round(value_num)))][
            !is.na(value_num) & !is.na(units),
            `:=`(value_ipnum = value_num * mult + offset)]
    }

    value_tbl[, `:=`(value_upper = toupper(value))]

    if (prefix != "value")
        data.table::setnames(value_tbl, paste0("value", val_suffix), req_val)

    if (joined_before)
        data.table::setnames(value_tbl, c("units", "ip_units"), c("si_name", "ip_name"))

    value_tbl
}
# }}}

# value_list: return a list of field values with correct types {{{
value_list <- function (value_tbl, in_ip = FALSE) {
    assert_that(has_names(value_tbl, c("value", "value_num", "value_ipnum", "type")))

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
# print.ErrFile {{{
print.ErrFile <- function (x, ...) {
    if (x$completed) {
        if (x$successful) {
            sum_line <- "EnergyPlus completed successfully"
        } else {
            sum_line <- "EnergyPlus completed unsuccessfully"
        }
    } else {
        sum_line <- "EnergyPlus terminated"
    }
    err_dt <- data.table::copy(x$data)

    err_dt[, line := .I]
    num_sum <- err_dt[, list(num = max(level_index)), by = list(level)][level != "Info"]
    if (not_empty(num_sum)) {
        num_line <- num_sum[, paste0(paste0(num, " ", level), collapse = ", ")]
        head <- crayon::bold(paste0("\n", sum_line, " with ", num_line, "."))
        if (x$successful) head <- crayon::green(head) else head <- crayon::red(head)
    } else {
        head <- crayon::bold$red(paste0("\n", sum_line, " or is still running."))
    }

    dt_num <- err_dt[begin_environment == FALSE, .N, by = list(environment_index, index)]

    if (is_empty(dt_num)) {
        cat(head, "\n")
    } else {
        index_last_env <- dt_num[, index[.N], by = list(environment_index)]$V1
        l_last <- err_dt[begin_environment == FALSE & !index %in% index_last_env,
                         line[.N], by = index]$V1

        err_dt[, level_num := max(level_index), by = list(level)]
        err_dt[, out := message]
        err_dt[begin_environment == TRUE, out := stringr::str_replace(out, "^Beginning ", "During ")]
        err_dt[begin_environment == TRUE, out := cli::rule(out, line = 2L, col = "green"), by = line]

        err_dt[begin_environment == FALSE & seperate == TRUE,
               out := paste0(level, "[", level_index, "/", level_num, "] ", out)]

        err_dt[, out := as.list(out)]
        err_dt[line %in% l_last, `:=`(out = {lapply(out, function (x) c(x, ""))})]
        err_dt[begin_environment == FALSE,
            `:=`(out = list(strwrap(out[[1]], width = getOption("width")))),
            by = line]

        err_dt[begin_environment == FALSE & level == "Info",
            `:=`(out = list(vapply(out[[1]], crayon::cyan, character(1)))),
            by = line]
        err_dt[begin_environment == FALSE & level == "Warning",
            `:=`(out = list(vapply(out[[1]], crayon::magenta, character(1)))),
            by = line]
        err_dt[begin_environment == FALSE & !level %in% c("Info", "Warning"),
            `:=`(out = list(vapply(out[[1]], crayon::red$bold, character(1)))),
            by = line]

        if (all(is.na(err_dt$environment_index))) {
            cat(unlist(err_dt$out), head, sep = "\n")
        } else {
            err_dt[is.na(environment_index), environment_index := 0L]
            err_box_dt <- err_dt[begin_environment == FALSE,
                list(msg_box = cli::boxx(unlist(out), padding = 0L, col = "green")), by = environment_index]
            err_line_dt <- err_dt[begin_environment == TRUE, list(msg_line = unlist(out)), by = environment_index]
            msg_dt <- err_line_dt[err_box_dt, on = "environment_index"]
            msg_dt[is.na(msg_line), msg_line := ""]
            msg_dt[environment_index == environment_index[1L], `:=`(msg = paste0(msg_line, "\n", msg_box))]
            msg_dt[environment_index != environment_index[1L], `:=`(msg = paste0("\n", msg_line, "\n", msg_box))]

            cat(c(msg_dt$msg, head), sep = "\n")
        }
    }

}
# }}}

#' @export
# print.IddFieldPossible {{{
print.IddFieldPossible <- function (x, ...) {
    dt <- data.table::copy(x)

    dt[, header := cli::rule(crayon::bold(paste0(
        field_index, ": Field ", backtick(field_name))), col = "green"),
        by = field_index
    ]

    dt[-1L, header := paste0("\n", header)]

    dt[, res := header]

    dt[!is.na(auto),
        res := paste0(res, "\n", crayon::cyan(paste0(
            cli::symbol$bullet, " ", crayon::bold("Auto value"), ": ", backtick(auto))))]

    dt[!vapply(default, is.na, logical(1)),
        res := paste0(res, "\n", crayon::cyan(paste0(
            cli::symbol$bullet, " ", crayon::bold("Default"), ": ",
            ifelse(is.character(default), backtick(default), default)))
        )
    ]

    dt[!vapply(choice, function (x) all(is.na(x)), logical(1)),
        res := paste0(res, "\n", crayon::cyan(paste0(
            cli::symbol$bullet, " ", crayon::bold("Choice"), ":\n" ,
            paste0("  - ", backtick(unlist(choice)), collapse = "\n")))),
        by = field_index
    ]

    dt[, res_ran := paste0(vapply(range, function (x) utils::capture.output(print.IddFieldRange(x)), character(1)))]
    dt[res_ran == "<Not Applicable>", res_ran := NA_character_]
    dt[!is.na(res_ran), res := paste0(res, "\n", crayon::cyan(paste0(
            cli::symbol$bullet, " ", crayon::bold("Range"), ": ", res_ran
        ))
    )]

    dt[!vapply(reference, is.null, logical(1)), res := paste0(res, "\n",
        crayon::cyan(paste0(cli::symbol$bullet, " ", crayon::bold("References"), ":\n",
            paste0(paste0("  - ", backtick(unlist(reference))), collapse = "\n"))
        )),
        by = field_index
    ]

    dt[res == header, res := paste0(res, "\n", crayon::magenta("<Not Applicable>"))]
    cli::cat_line(dt$res)
}
# }}}

#' @export
# print.IddFieldRange {{{
print.IddFieldRange <- function (x, ...) {
    if (is.na(x$lower_incbounds) & is.na(x$upper_incbounds)) {
        cat("<Not Applicable>")
        return(invisible(NULL))
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
}
# }}}
