#' @importFrom cli boxx rule cat_line symbol
#' @importFrom utils capture.output
#' @importFrom stringi stri_dup
#' @include impl.R
NULL

# tree_chars {{{
# reference: https://github.com/r-lib/cli/blob/master/R/tree.R#L111
tree_chars <- function() {
    if (l10n_info()$`UTF-8`) {
        list("v" = "\u2502",
             "h" = "\u2500",
             "p" = "\u251C",
             "l" = "\u2514",
             "j" = "\u251C",
             "u" = "^",
             "d" = "v"
        )
    } else {
        list("v" = "|",
             "h" = "-",
             "p" = "+",
             "l" = "\\",
             "j" = "|",
             "u" = "^",
             "d" = "v"
        )
    }
}
# }}}

# tree_prefix {{{
tree_prefix <- function() {
    list(
        start = paste0(tree_chars()$p, tree_chars()$h),
        start_e = "  ",
        end = paste0(tree_chars()$l, tree_chars()$h),
        end_e = "  ",
        mid = paste0(tree_chars()$v, tree_chars()$h),
        mid2 = paste0(tree_chars()$v, " "),
        mid_e = "  "
    )
}
# }}}

# add_prefix {{{
add_prefix <- function(lst) {
    combine_sub <- function(x, first_prefix, other_prefix = NULL) {
        apply_fun <- if (is.list(x)) lapply else vcapply
        if (is.list(x)) {
            before <- function(x) unlist(x, use.names = FALSE)
            after <- list
        } else {
            before <- identity
            after <- identity
        }
        if (length(x) == 1L) {
            x <- apply_fun(x, function(x) paste(first_prefix, unlist(x, use.names = FALSE)))
        } else if (length(x) > 1L) {
            x[1L] <- apply_fun(x[1L], function(x) paste(first_prefix, unlist(x, use.names = FALSE)))
            x[-1L] <- apply_fun(x[-1L], function(x) paste(other_prefix, unlist(x, use.names = FALSE)))
        }
        x
    }

    l <- length(lst)
    apply_fun <- if (is.list(lst)) lapply else vcapply
    if (l == 1L) {
        # \- X: Name
        #    \- Class
        #       |- X: Field1
        #       \- X: Field2
        lst[[1L]] <- combine_sub(lst[[1L]], tree_prefix()$end, tree_prefix()$mid_e)
    } else {
        # |- X: Name
        # |  \- Class
        # |     |- X: Field1
        # |     \- X: Field2
        lst[[1L]] <- combine_sub(lst[[1L]], tree_prefix()$start, tree_prefix()$mid2)
        # |- X: Name
        # |  \- Class
        # |     |- X: Field1
        # |     \- X: Field2
        lst[-c(1L, l)] <- apply_fun(lst[-c(1L, l)], combine_sub, first_prefix = tree_prefix()$mid, tree_prefix()$mid2)
        # \- X: Name
        #    \- Class
        #       |- X: Field1
        #       \- X: Field2
        lst[[l]] <- combine_sub(lst[[l]], tree_prefix()$end, tree_prefix()$mid_e)
    }
    lst
}
# }}}

# format_header: return header of an Idf output {{{
format_header <- function(save_format = c("sorted", "new_top", "new_bot"),
                           view_in_ip = FALSE, special_format = FALSE) {
    save_format <- switch(match.arg(save_format),
        sorted = "SortedOrder",
        new_top = "OriginalOrderTop",
        new_bot = "OriginalOrderBottom")

    header_generator <- "!-Generator eplusr"

    header_option <- paste0("!-Option ", save_format)

    if (special_format) {
        warn(paste0("Currently, special format for classes such as ",
                "single line formating for vertices are not support. ",
                "All objects will be formatted in standard way."
            ),
            "warning_special_format"
        )
    }
    special_format <- NULL

    if (view_in_ip) in_ip <- " ViewInIPunits" else in_ip <- NULL

    header_option <- stri_trim_right(paste0(header_option, special_format, in_ip))

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

# format_idf: return whole Idf output {{{
#' @importFrom checkmate assert_names assert_count assert_flag assert_int
format_idf <- function(
    dt_value, dt_object = NULL, dt_order = NULL,
    header = TRUE, comment = TRUE, save_format = c("sorted", "new_top", "new_bot"),
    special_format = FALSE, leading = 4L, sep_at = 29L, index = FALSE,
    blank = FALSE, end = TRUE, required = FALSE
)
{
    assert_names(names(dt_value), must.include = c("object_id", "class_id", "class_name", "field_index"))

    save_format <- match.arg(save_format)
    assert_count(leading)
    assert_int(sep_at, lower = -1L)
    assert_flag(index)
    assert_flag(blank)
    assert_flag(end)
    assert_flag(required)

    setorderv(dt_value, c("object_id", "field_index"))

    # get field value
    fld <- format_field(dt_value, leading = leading, sep_at = sep_at,
        index = index, blank = blank, end = end, required = required)

    # init output as field values
    set(dt_value, NULL, "fmt", fld)
    on.exit(set(dt_value, NULL, "fmt", NULL), add = TRUE)

    # format objects
    out <- dt_value[, list(
        class_id = class_id[[1L]], class_name = class_name[[1L]],
        fmt = list(object = c(paste0(class_name[[1L]], ","), fmt))),
        by = c("object_id")
    ]

    # add comments
    if (!comment) {
        out[, `:=`(fmt = list(c(list(NULL), fmt))), by = c("object_id")]
    } else {
        out[dt_object, on = "object_id",
            `:=`(fmt = list(
                {
                    if (is.null(dt_object$comment[[.GRP]])) {
                        c(list(NULL), fmt)
                    } else {
                        c(list(paste0("!", dt_object$comment[[.GRP]])), fmt)
                    }
                }
            )),
            by = c("object_id")
        ]
    }

    # object order {{{
    if (save_format == "sorted") {
        setorderv(out, c("class_id", "object_id"))
        # nest by class
        out <- out[, list(class_name = class_name[[1L]],
            fmt = list(c(format_class_header(class_name[[1L]]), fmt))),
            by = c("class_id")
        ]
    } else {
        if (!is.null(dt_order)) {
            assert_data_frame(dt_order, any.missing = FALSE, min.cols = 2)
            assert_names(names(dt_order), must.include = c("object_id", "object_order"))

            out[dt_order, on = "object_id", object_order := i.object_order]
            if (save_format == "new_top") {
                setorderv(out, c("object_order", "object_id"), c(-1L, 1L))
            } else {
                setorderv(out, c("object_order", "object_id"), c(1L, 1L))
            }
            set(out, NULL, "object_order", NULL)
        }
    }
    # }}}

    if (header)
        h <- format_header(save_format = save_format, view_in_ip = eplusr_option("view_in_ip"), special_format = special_format)
    else h <- NULL

    list(header = h, format = out)
}
# }}}

# format_class_header {{{
format_class_header <- function(class) {
    paste0("!-   ===========  ALL OBJECTS IN CLASS: ", stri_trans_toupper(class), " ===========")
}
# }}}

# switch_ref_src {{{
switch_ref_src <- function(dt, types, invert = FALSE) {
    pair <- function(type, src = FALSE) {
        if (type == "value") {
            suffix <- c("_id", "_chr", "_num")
        } else if (type == "field") {
            suffix <- c("_id", "_name", "_index")
        } else if (type == "type_enum") {
            suffix <- c("")
        } else {
            suffix <- c("_id", "_name")
        }
        if (src) {
            paste0(type, suffix)
        } else {
            paste0("src_", type, suffix)
        }
    }
    old <- c(pair("class"), pair("object"), pair("field"), pair("value"), pair("type_enum"),
        pair("class", TRUE), pair("object", TRUE), pair("field", TRUE), pair("value", TRUE), pair("type_enum", TRUE)
    )
    new <- c(pair("class", TRUE), pair("object", TRUE), pair("field", TRUE), pair("value", TRUE), pair("type_enum", TRUE),
        pair("class"), pair("object"), pair("field"), pair("value"), pair("type_enum")
    )
    if (invert) {
        setnames(dt, new[new %chin% names(dt)], old[new %chin% names(dt)])
    } else {
        setnames(dt, old[old %chin% names(dt)], new[old %chin% names(dt)])
    }
}
# }}}

# format_idd_relation: return pretty formatted tree string for Relation {{{
format_idd_relation <- function(ref, direction = c("ref_to", "ref_by")) {
    if (!nrow(ref)) return(data.table(class_id = integer(), fmt = list()))

    # copy original
    ref <- copy(ref)

    direction <- match.arg(direction)

    tc <- tree_chars()

    cls <- format_class
    fld <- function(dt) {
        paste0("Field: <",
            format_field(dt, leading = 0L, sep_at = 15L, index = TRUE, prefix = FALSE, pad_char = "0"), ">")
    }

    # simple switch column names
    if (direction == "ref_by") switch_ref_src(ref)

    # add formatted string for class and fields
    set(ref, NULL, "class_name", cls(ref))
    set(ref, NULL, "field_name", fld(ref))
    set(ref, NULL, "src_class_name", cls(ref[, list(class_name = src_class_name)]))
    set(ref, NULL, "src_field_name",
        as.list(fld(ref[, list(class_id = src_class_id, field_index = src_field_index, field_name = src_field_name)]))
    )

    ref[!is.na(src_enum), by = c("src_enum", "field_name"), pointer := {
        if (!length(src_enum)) {
            prefix <- NA_character_
        } else if (.BY$src_enum == IDDFIELD_SOURCE$class) {
            prefix <- switch(direction, ref_by = "b", ref_to = "p")
        } else {
            prefix <- switch(direction, ref_by = tc$u, ref_to = tc$d)
        }
        paste0(prefix, stri_dup("~", stri_length(.BY$field_name) - 1L))
    }]

    # clean
    set(ref, NULL, c("src_enum", "field_index", "src_field_index"), NULL)

    # helper function to add tree structure prefix
    add_pre <- function(x, end = TRUE, indent = "") {
        p <- character(length(x))
        if (end) {
            p[[1L]] <- paste0(tc$l, tc$h, " ")
            if (length(p) > 1L) p[-1L] <- "   "
        } else {
            p[[1L]] <- paste0(tc$j, tc$h, " ")
            if (length(p) > 1L) p[-1L] <- paste0(tc$v, "  ")
        }
        paste0(indent, p, x)
    }

    # split by depth
    ref <- split(ref, by = "dep")

    for (i in rev(seq(ref))) {
        ref[[i]] <- ref[[i]][, by = c("class_id", "class_name", "field_id", "field_name"), {
            tar <- c(class_name[[1L]], add_pre(c(field_name[[1L]], pointer[[1L]])))

            if (is.na(pointer[[1L]])) {
                list(src_field_name = list(tar[1:2]))
            } else {
                if (.N == 1L) {
                    src <- src_field_name[[1L]]
                    blank <- if (stri_isempty(stri_trim_left(src[length(src)]))) NULL else ""
                    src <- c(
                        paste0("   ",    add_pre(src_class_name)),
                        paste0("      ", add_pre(src)),
                        blank
                    )
                } else {
                    # group by source class name
                    id <- rleid(src_class_name)

                    src <- lapply(unique(id), function(i) {
                        i <- which(id == i)

                        if (length(i) == 1L) {
                            # the deepest
                            c(unlist(src_class_name[[i]], FALSE, FALSE),
                              add_pre(src_field_name[[i]])
                            )
                        } else {
                            # remove class and field prefix
                            srci <- c(
                                src_field_name[i[[1L]]],
                                lapply(src_field_name[i[-1L]],
                                    function(s) if (length(s) > 1L) stri_sub(s[-1L], 4L) else s
                                )
                            )

                            l <- length(srci)
                            srci[[l]] <- add_pre(srci[[l]])
                            srci[-l] <- lapply(srci[-l], add_pre, FALSE)

                            c(src_class_name[[i[[1L]]]], unlist(srci, FALSE, FALSE))
                        }
                    })

                    l <- length(src)
                    blank <- if (stri_isempty(stri_trim_left(src[[l]][length(src[[l]])]))) NULL else ""
                    src[[l]] <- add_pre(c(src[[l]], blank), indent = "   ")
                    src[-l] <- lapply(src[-l], function(s) {
                        blank <- if (stri_isempty(stri_trim_left(s[length(s)]))) NULL else ""
                        add_pre(c(s, blank), FALSE, "   ")
                    })
                    src <- unlist(src, FALSE, FALSE)
                }

                list(src_field_name = list(c(tar, src)))
            }
        }]

        # add recursively referred classes into previous depth
        if (i != 1L) {
            cur <- ref[[i]]
            pre <- ref[[i - 1L]]

            ref[[i - 1L]] <- rbindlist(list(
                pre,
                unique(pre[, .SD, .SDcols = c("class_id", "class_name", "field_id", "field_name", "dep", "pointer", "src_class_id")])[
                cur[, list(src_class_id = class_id, src_class_name = class_name, src_field_id = field_id, src_field_name)],
                on = "src_class_id", allow.cartesian = TRUE]
            ), fill = TRUE)

            setorderv(ref[[i - 1L]], c("class_id", "field_id", "src_class_id", "src_field_id"))
        } else  {
            ref[[1L]] <- ref[[1L]][, by = "class_id", {
                if (.N == 1L) {
                    list(src_field_name = c(class_name[[1L]], src_field_name[[1L]][-1L]))
                } else {
                    # remove class and field prefix
                    src <- lapply(src_field_name,
                        function(s) if (length(s) > 1L) stri_sub(s[-1L], 4L) else s
                    )

                    src[[.N]] <- add_pre(src[[.N]])
                    src[-.N] <- lapply(src[-.N], add_pre, FALSE)

                    list(src_field_name = c(class_name[[1L]], unlist(src, FALSE, FALSE)))
                }
            }]
        }
    }

    out <- ref[[1L]]
    setnames(out, "src_field_name", "fmt")
}
# }}}

# format_idf_relation: return pretty formatted tree string for Relation {{{
format_idf_relation <- function(ref, direction = c("ref_to", "ref_by")) {
    if (!nrow(ref)) return(data.table(class_id = integer(), object_id = integer(), value_id = integer(), fmt = list()))

    # copy original
    ref <- copy(ref)

    direction <- match.arg(direction)

    tc <- tree_chars()

    cls <- format_class
    fld <- function(dt) {
        paste0("Field: <",
            format_field(dt, leading = 0L, sep_at = 15L, index = TRUE, prefix = FALSE, pad_char = "0"), ">")
    }

    # simple switch column names
    if (direction == "ref_by") switch_ref_src(ref)

    # add formatted string for object, class, fields and values
    set(ref, NULL, "class_name", format_class(ref))
    set(ref, NULL, "object_name", format_object(ref))
    switch_ref_src(ref)
    set(ref, NULL, "class_name", format_class(ref))
    set(ref, NULL, "object_name", format_object(ref))
    switch_ref_src(ref, invert = TRUE)
    # format values {{{
    val <- ref[, by = c("object_id"), {
        dup <- duplicated(value_id)
        l <- list(
            field_index = field_index[!dup],
            field_name = field_name[!dup],
            value_chr = value_chr[!dup],
            value_num = value_num[!dup],
            type_enum = type_enum[!dup]
        )
        val <- format_field(setDT(l), leading = 0L, sep_at = 15L, pad_char = "0",
            value = TRUE, quote = TRUE, blank = TRUE, index = TRUE, required = FALSE, prefix = TRUE
        )
        list(value_id = value_id[!dup], value_chr = val)
    }]
    ref[val, on = c("object_id", "value_id"), value_chr := i.value_chr]
    val <- ref[, by = c("src_object_id"), {
        dup <- duplicated(src_value_id)
        l <- list(
            field_index = src_field_index[!dup],
            field_name = src_field_name[!dup],
            value_chr = src_value_chr[!dup],
            value_num = src_value_num[!dup],
            type_enum = src_type_enum[!dup]
        )
        val <- format_field(setDT(l), leading = 0L, sep_at = 15L, pad_char = "0",
            value = TRUE, quote = TRUE, blank = TRUE, index = TRUE, required = FALSE, prefix = TRUE
        )
        list(src_value_id = src_value_id[!dup], src_value_chr = val)
    }]
    ref[val, on = c("src_object_id", "src_value_id"), src_value_chr := i.src_value_chr]
    set(ref, NULL, "src_value_chr", as.list(ref$src_value_chr))
    # }}}

    ref[!is.na(src_enum), by = c("src_enum", "value_chr"), pointer := {
        if (!length(src_enum)) {
            prefix <- NA_character_
        } else if (.BY$src_enum == IDDFIELD_SOURCE$class) {
            prefix <- switch(direction, ref_by = "b", ref_to = "p")
        } else {
            prefix <- switch(direction, ref_by = tc$u, ref_to = tc$d)
        }
        paste0(prefix, stri_dup("~", stri_length(.BY$value_chr) - 1L))
    }]

    # clean
    set(ref, NULL, c("src_enum", "type_enum", "src_type_enum",
        "value_num", "src_value_num", "value_id", "src_value_id",
        "field_name", "src_field_name", "field_index", "src_field_index"), NULL)

    # helper function to add tree structure prefix
    add_pre <- function(x, end = TRUE, indent = "") {
        p <- character(length(x))
        if (end) {
            p[[1L]] <- paste0(tc$l, tc$h, " ")
            if (length(p) > 1L) p[-1L] <- "   "
        } else {
            p[[1L]] <- paste0(tc$j, tc$h, " ")
            if (length(p) > 1L) p[-1L] <- paste0(tc$v, "  ")
        }
        paste0(indent, p, x)
    }

    # split by depth
    ref <- split(ref, by = "dep")

    for (i in rev(seq_along(ref))) {
        ref[[i]] <- ref[[i]][,
            by = c("class_id", "class_name", "object_id", "object_name", "field_id", "value_chr"), {
            tar <- c(class_name[[1L]], add_pre(object_name[[1L]]),
                     add_pre(c(value_chr[[1L]], pointer[[1L]]), indent = "   ")
            )

            if (is.na(pointer[[1L]])) {
                list(src_value_chr = list(tar[1:3]))
            } else {
                if (.N == 1L) {
                    src <- src_value_chr[[1L]]
                    blank <- if (stri_isempty(stri_trim_left(src[length(src)]))) NULL else ""
                    src <- c(
                        paste0("      ", add_pre(src_class_name)),
                        paste0("         ", add_pre(src_object_name)),
                        paste0("            ", add_pre(src)),
                        blank
                    )
                } else {
                    # group by source class name
                    id <- rleid(src_class_name, src_object_name)

                    src <- lapply(unique(id), function(i) {
                        i <- which(id == i)

                        if (length(i) == 1L) {
                            # the deepest
                            c(src_class_name[[i]][[1L]],
                              add_pre(src_object_name[[i]][[1L]]),
                              paste0("   ", add_pre(unlist(src_value_chr[[i]][[1L]], FALSE, FALSE)))
                            )
                        } else {
                            # remove class and field prefix
                            srci <- lapply(src_value_chr[i],
                                function(s) if (length(s) > 1L) stri_sub(s[-(1L:2L)], 7L) else s
                            )

                            l <- length(srci)
                            srci[[l]] <- add_pre(srci[[l]])
                            srci[-l] <- lapply(srci[-l], add_pre, FALSE)

                            c(src_class_name[[i[[1L]]]],
                              add_pre(src_object_name[[i[[1L]]]]),
                              paste0("   ", unlist(srci, FALSE, FALSE))
                            )
                        }
                    })

                    l <- length(src)
                    blank <- if (stri_isempty(stri_trim_left(src[[l]][length(src[[l]])]))) NULL else ""
                    src[[l]] <- add_pre(c(src[[l]], blank), indent = "      ")
                    src[-l] <- lapply(src[-l], function(s) {
                        blank <- if (stri_isempty(stri_trim_left(s[length(s)]))) NULL else ""
                        add_pre(c(s, blank), FALSE, "      ")
                    })
                    src <- unlist(src, FALSE, FALSE)
                }

                list(src_value_chr = list(c(tar, src)))
            }
        }]

        # add recursively referred classes into previous depth
        if (i != 1L) {
            cur <- ref[[i]]
            pre <- ref[[i - 1L]]

            ref[[i - 1L]] <- rbindlist(list(
                pre,
                unique(pre[, .SD, .SDcols = c("class_id", "class_name", "object_id", "object_name", "field_id", "value_chr", "dep", "pointer", "src_class_id", "src_object_id")])[
                    cur[, list(src_class_id = class_id, src_class_name = class_name,
                               src_object_id = object_id, src_object_name = object_name,
                               src_field_id = field_id, src_value_chr)],
                    on = c("src_class_id", "src_object_id"), allow.cartesian = TRUE]
            ), fill = TRUE)

            setorderv(ref[[i - 1L]], c("class_id", "object_id", "field_id", "src_class_id", "src_object_id", "src_field_id"))
        } else  {
            ref[[1L]] <- ref[[1L]][, by = c("class_id", "object_id"), {
                if (.N == 1L) {
                    list(src_value_chr = c(
                        class_name[[1L]],
                        add_pre(object_name[[1L]]),
                        paste0("   ", stri_sub(src_value_chr[[1L]][-(1:2)], 3L))
                    ))
                } else {
                    # remove class and field prefix
                    src <- lapply(src_value_chr,
                        function(s) if (length(s) > 1L) stri_sub(s[-(1:2)], 7L) else s
                    )

                    src[[.N]] <- add_pre(src[[.N]])
                    src[-.N] <- lapply(src[-.N], add_pre, FALSE)

                    list(src_value_chr = c(
                        class_name[[1L]],
                        add_pre(object_name[[1L]]),
                        paste0("   ", unlist(src, FALSE, FALSE))
                    ))
                }
            }]
        }
    }

    out <- ref[[1L]]
    setnames(out, "src_value_chr", "fmt")
    out
}
# }}}

# format_possible: return pretty formatted list of possible field values {{{
format_possible <- function(x) {
    set(x, NULL, "field", paste0(x$field_index, ": ", x$field_name))
    on.exit(set(x, NULL, "field", NULL), add = TRUE)

    cols <- NULL

    # auto {{{
    if (has_names(x, "auto")) {
        set(x, NULL, "fmt_auto", paste0("* Auto value: ",
            {
                tmp <- paste0("\"", x$auto, "\"")
                tmp[is.na(x$auto)] <- "<NA>"
                tmp
            }
        ))
        on.exit(set(x, NULL, "fmt_auto", NULL), add = TRUE)
        cols <- c(cols, "fmt_auto")
    }
    # }}}

    # default {{{
    if (has_names(x, "default")) {
        set(x, NULL, "fmt_default", paste0("* Default: ",
            vcapply(x$default, function(def) {
                if (is.numeric(def)) {
                    as.character(def)
                } else if (is.na(def)) {
                    "<NA>"
                } else {
                    paste0("\"", def, "\"")
                }
            })
        ))
        on.exit(set(x, NULL, "fmt_default", NULL), add = TRUE)
        cols <- c(cols, "fmt_default")
    }
    # }}}

    # choice {{{
    if (has_names(x, "choice")) {
        set(x, NULL, "fmt_choice", paste0("* Choice:",
            vcapply(x$choice, function(cho) {
                if (!length(cho)) return(" <NA>")
                if (length(cho) > 5L) cho <- c(cho[1L:5L], "......")
                cho <- surround(cho, "\"")
                paste0("\n", paste0("  - ", cho, collapse = "\n"))
            })
        ))
        on.exit(set(x, NULL, "fmt_choice", NULL), add = TRUE)
        cols <- c(cols, "fmt_choice")
    }
    # }}}

    # range {{{
    if (has_names(x, "ranger")) {
        set(x, NULL, "range", paste0("* Range: ", vcapply(x$range, format.Range)))
        on.exit(set(x, NULL, "fmt_range", NULL), add = TRUE)
        cols <- c(cols, "fmt_range")
    }
    # }}}

    # source {{{
    if (has_names(x, "source")) {
        set(x, NULL, "fmt_source", paste0("* Source: ",
            vcapply(x$source, function(src) {
                if (!length(src)) return("<NA>")
                if (length(src) > 5L) src <- c(src[1L:5L], "......")
                paste0("\n", paste0("  - ", surround(src, "\""), collapse = "\n"))
            })
        ))
        on.exit(set(x, NULL, "fmt_source", NULL), add = TRUE)
        cols <- c(cols, "fmt_source")
    }
    # }}}

    if (has_names(x, "value_id")) {
        res <- x[, .SD, .SDcols = c("object_id", "value_id", "field", cols)]
    } else {
        res <- x[, .SD, .SDcols = c("class_id", "field_id", "field", cols)]
    }

    res
}
# }}}

# format_group {{{
format_group <- function(dt) {
    paste0("Group: <", dt$group_name, ">")
}
# }}}

# format_class {{{
format_class <- function(dt) {
    paste0("Class: <", dt$class_name, ">")
}
# }}}

# format_object {{{
format_object <- function(dt) {
    obj_nm <- paste0(" <", dt$object_name, ">")
    obj_nm[is.na(dt$object_name)] <- ""
    id <- as.character(dt$object_id)
    id[!is.na(dt$object_id) & dt$object_id < 0L] <- paste0(
        "Input #", -dt$object_id[!is.na(dt$object_id) & dt$object_id < 0L])
    paste0("Object [ID:", id, "]", obj_nm)
}
# }}}

# format_field_by_parent {{{
format_field_by_parent <- function(dt, col = "value", sep_at = 15L, required = FALSE) {
    val <- col == "value"
    # in order to keep index more tidy, have to format them based on
    # parent index
    if (has_names(dt, "object_id")) {
        col_parent <- "object_id"
    } else if (has_names(dt, "class_id")) {
        col_parent <- "class_id"
    } else if (has_names(dt, "group_id")) {
        col_parent <- "group_id"
    } else {
        col_parent <- NULL
    }

    if (is.null(col_parent)) {
        format_field(dt, leading = 0L, sep_at = sep_at, pad_char = "0",
            value = val, quote = TRUE, blank = TRUE, index = TRUE, required = required, prefix = val)
    } else {
        l <- dt[, list(l = list(.I)), by = c(col_parent)]$l
        out <- character(nrow(dt))
        for (i in seq_along(l)) {
            out[l[[i]]] <- format_field(dt[l[[i]]],
                leading = 0L, sep_at = sep_at, pad_char = "0",
                value = val, quote = TRUE, blank = TRUE, index = TRUE, required = required, prefix = val
            )
        }
        out
    }
}
# }}}

# format_objects: return pretty formatted tree string for mutiple IdfObjects {{{
#' @importFrom checkmate assert_subset assert_names
format_objects <- function(dt, component = c("group", "class", "object", "field", "value"),
                            brief = TRUE, merge = TRUE, sep_at = 15L, nest = TRUE,
                            order = FALSE, required = FALSE) {
    choices <- c("group", "class", "object", "field", "value")
    assert_subset(component, choices, FALSE)
    component <- choices[choices %in% component]

    # create each component {{{
    if ("group" %chin% component) {
        assert_names(names(dt), must.include = c("group_id", "group_name"))
        set(dt, NULL, "group", format_group(dt))
    }

    if ("class" %chin% component) {
        assert_names(names(dt), must.include = c("class_id", "class_name"))
        set(dt, NULL, "class", format_class(dt))
    }

    if ("object" %chin% component) {
        assert_names(names(dt), must.include = c("object_id", "object_name"))
        set(dt, NULL, "object", format_object(dt))
    }

    if ("value" %chin% component) {
        assert_names(names(dt), must.include = c("value_id", "value_chr", "value_num"))
        old_value <- dt[["value_chr"]]
        if (merge) {
            assert_names(names(dt), must.include = c("field_id", "field_index", "field_name", "units", "ip_units"))
            set(dt, NULL, "value", format_field_by_parent(dt, "value", sep_at = sep_at, required = required))
            component <- component[component != "field"]
        } else {
            set(dt, NULL, "value",
                paste0("Value: <",
                format_value(dt, leading = 0L, length = 0L, quote = TRUE, end = NULL, blank = TRUE),
                ">"
                )
            )
        }
    }

    # should format "field" after "value" as if merge is TRUE, then formatting
    # field is not necessary
    if ("field" %chin% component) {
        assert_names(names(dt), must.include = c("field_id", "field_index", "field_name", "units", "ip_units"))
        if ((!"value" %chin% component) || ("value" %chin% component & !merge)) {
            set(dt, NULL, "field", paste0("Field: <", format_field_by_parent(dt, "field", sep_at = sep_at, required = required), ">")
            )
        }
    }
    # }}}

    col_id <- paste0(component, "_id")

    # only one component is required
    if (length(component) == 1L) {
        return(setnames(dt[, .SD, .SDcols = c(col_id, component)], component, "out")[])
    }

    if (brief) {
        # only the last component will be simplified
        key_child <- stri_trans_toupper(stri_sub(component[[length(component)]], to = 1L))

        setorderv(dt, col_id[-length(component)])

        col_id <- col_id[-length(col_id)]
        component <- component[-length(component)]

        dt <- dt[, list(num = .N), by = c(col_id, component)]

        set(dt, NULL, component[[length(component)]],
            paste0("[", lpad(dt$num, "0"), "<", key_child, ">] ", dt[[component[[length(component)]]]]))
    }

    out <- dt
    i <- length(component)
    n <- i
    while (i > 1L) {
        parents <- NULL
        for (j in seq.int(i-1L)) {
            if (j == 1L) next
            parents <- c(
                parents,
                paste0(component[[i-j]], " = ", component[[i-j]], "[[1L]]")
            )
            j <- j - 1L
        }

        empty <- if (n == i) ", ''" else NULL
        childen <- paste0(
            component[[i-1L]], " = list(",
            "c(", component[[i-1L]], "[[1L]], add_prefix(", component[[i]], ")", empty, "))"
        )

        if (is.null(parents) || all(stri_isempty(parents))) {
            code <- paste0("list(", childen, ")")
        } else {
            code <- paste0("list(", paste0(parents, collapse = ","), ", ", childen, ")")
        }

        if (nest || i != 2L) col_id <- col_id[-i]
        out <- out[, eval(parse(text = code)), by = c(col_id)]
        i <- i - 1L
    }
    setnames(out, component[[1L]], "out")

    col_del <- intersect(names(dt), c("group", "class", "object", "field", "value"))
    if (length(col_del)) set(dt, NULL, col_del, NULL)
    if (has_names(dt, "value_chr")) set(dt, NULL, "value_chr", old_value)

    if (nest & order) setorderv(out, col_id)
    out
}
# }}}

# format_field: return Idf format field {{{
format_field <- function(dt,
                          # index
                          leading = 4L, sep_at = 29L, index = FALSE, pad_char = " ",
                          # value
                          value = TRUE, quote = FALSE, blank = FALSE, end = TRUE, required = FALSE,
                          # field
                          prefix = TRUE) {
    idx <- NULL

    if (index) {
        idx <- paste0(format_index(dt, required = required, pad_char = pad_char), ": ")
    }

    if (value) {
        val <- format_value(dt, leading = leading, length = sep_at, quote = quote, blank = blank, end = end)
    } else {
        val <- NULL
    }

    nm <- if (sep_at < 0L) "" else format_name(dt, prefix)

    paste0(idx, val, nm)
}
# }}}

# format_index: return right aligned field index {{{
format_index <- function(dt, required = FALSE, pad_char = " ") {
    if (required) assert_names(names(dt), must.include = "required_field")

    if (any(!is.na(dt$field_index))) {
        idx <- lpad(dt$field_index, pad_char, width = max(nchar(dt$field_index[!is.na(dt$field_index)], "width")))
    } else {
        idx <- as.character(dt$field_index)
    }

    if (required) {
        if (all(!dt$required_field)) return(idx)
        req <- rep(" ", nrow(dt))
        req[dt$required_field] <- "*"
        idx <- paste0(idx, req)
    }

    idx
}
# }}}

# format_value: return Idf format value strings {{{
#' @importFrom checkmate assert_names
format_value <- function(dt, leading = 4L, length = 29L, quote = FALSE, blank = FALSE, end = TRUE) {
    length <- max(length, 0L)
    if (is.null(dt$value_chr)) return(paste0(stri_dup(" ", leading), character(nrow(dt))))
    set(dt, NULL, "value_out", dt$value_chr)
    set(dt, NULL, "width", leading + nchar(dt$value_out, "width") + 1L) # 1 for comma(,)

    if (has_names(dt, "value_num")) {
        dt[!is.na(value_num), `:=`(
            value_out = as.character(value_num),
            width = leading + nchar(value_num, "width") + 1L)
        ]
    }

    # get blank character and width {{{
    if (blank) {
        blk_chr <- if (quote) s_blk("<\"Blank\">") else s_blk("<Blank>")
        blk_chr_w <- nchar("<\"Blank\">", "width")
        blk_num <- s_blk("<Blank>")
        blk_num_w <- nchar("<Blank>", "width")
    } else {
        blk_chr <- if (quote) s_blk("\"\"") else ""
        blk_chr_w <- nchar("", "width")
        blk_num <- ""
        blk_num_w <- blk_chr_w
    }
    # }}}

    # format value according to type {{{
    if (!quote) {
        dt[is.na(value_out), `:=`(value_out = blk_chr, width = leading + blk_chr_w + 1L)]
    } else {
        assert_names(names(dt), must.include = "type_enum")

        # character value
        dt[type_enum > IDDFIELD_TYPE$real,
            c("value_out", "width") := ({
                na <- is.na(value_out)
                width[na] <- leading + blk_chr_w + 1L
                width[!na] <- width[!na] + 2L
                value_out[na] <- blk_chr
                value_out[!na] <- paste0("\"", value_out[!na], "\"")
                list(value_out, width)
            })
        ]
        # numeric value
        dt[type_enum <= IDDFIELD_TYPE$real,
            c("value_out", "width") := ({
                na <- is.na(value_out)
                value_out[na] <- blk_num
                width[na] <- leading + blk_num_w + 1L
                list(value_out, width)
            })
        ]
    }
    # }}}

    if (length <= 0L) {
        len <- 0L
    } else {
        len <- length - dt$width
        len[len < 0L] <- 2L
    }
    pad <- stringi::stri_dup(" ", len)

    values <- dt$value_out
    if (is.null(end)) {
        res <- values
    } else if (!end) {
        res <- paste0(values, ",")
    } else {
        if (has_names(dt, "object_id")) {
            is_end <- dt[, .I[.N], by = object_id]$V1
        } else {
            is_end <- length(values)
        }
        res <- character(length(values))
        res[is_end] <- paste0(values[is_end], ";")
        res[-is_end] <- paste0(values[-is_end], ",")
    }

    on.exit(set(dt, NULL, c("value_out", "width"), NULL), add = TRUE)

    paste0(stringi::stri_dup(" ", leading), res, pad)
}
# }}}

# format_name: return Idf format field names {{{
format_name <- function(dt, prefix = TRUE) {
    col_unit <- if (in_ip_mode()) "ip_units" else "units"
    pre <- if (prefix) "!- " else NULL
    u <- character(nrow(dt))
    u[!is.na(dt[[col_unit]])] <- paste0(" {", dt[[col_unit]][!is.na(dt[[col_unit]])], "}")
    paste0(pre, dt[["field_name"]], u)
}
# }}}

# format_comment: return Idf format comments and macros {{{
format_comment <- function(dt) {
    vcapply(dt$comment,
        function(cmt) {
            if (length(cmt)) {
                paste0("!", unlist(cmt, use.names = FALSE), collapse = "\n")
            } else {
                NA_character_
            }
        }
    )
}
# }}}

# Relatioin
#' @export
# print.IddRelationBy {{{
print.IddRelationBy <- function(x, ...) {
    cli::cat_rule("Referred by Others")
    if (!nrow(x)) {
        cli::cat_line("Target(s) is not referred by any other field.")
    } else {
        s <- paste0(" ", unlist(format_idd_relation(x, "ref_by")$fmt, use.names = FALSE))
        cli::cat_line(cli::ansi_strtrim(s))
    }
    invisible(x)
}
# }}}
#' @export
# print.IddRelationTo {{{
print.IddRelationTo <- function(x, ...) {
    cli::cat_rule("Refer to Others")
    if (!nrow(x)) {
        cli::cat_line("Target(s) does not refer to any other field.")
    } else {
        s <- paste0(" ", unlist(format_idd_relation(x, "ref_to")$fmt, use.names = FALSE))
        cli::cat_line(cli::ansi_strtrim(s))
    }
    invisible(x)
}
# }}}
#' @export
# print.IddRelation {{{
print.IddRelation <- function(x, ...) {
    if (!is.null(x[["ref_to"]])) {print.IddRelationTo(x[["ref_to"]]); cli::cat_line()}
    if (!is.null(x[["ref_by"]])) print.IddRelationBy(x[["ref_by"]])
    invisible(x)
}
# }}}
#' @export
# print.IdfRelationBy {{{
print.IdfRelationBy <- function(x, ...) {
    cli::cat_rule("Referred by Others")
    if (!all(has_names(x, c("class_name", "object_name", "field_name", "src_class_name", "src_object_name", "src_field_name")))) {
        NextMethod("print")
        return(invisible(x))
    }

    if (!nrow(x)) {
        cli::cat_line("Target(s) is not referred by any other field.")
    } else {
        s <- paste0(" ", unlist(format_idf_relation(x, "ref_by")$fmt, use.names = FALSE))
        cli::cat_line(cli::ansi_strtrim(s))
    }
    invisible(x)
}
# }}}
#' @export
# print.IdfRelationTo {{{
print.IdfRelationTo <- function(x, ...) {
    cli::cat_rule("Refer to Others")

    if (!all(has_names(x, c("class_name", "object_name", "field_name", "src_class_name", "src_object_name", "src_field_name")))) {
        NextMethod("print")
        return(invisible(x))
    }

    if (!nrow(x)) {
        cli::cat_line("Target(s) does not refer to any other field.")
    } else {
        s <- paste0(" ", unlist(format_idf_relation(x, "ref_to")$fmt, use.names = FALSE))
        cli::cat_line(cli::ansi_strtrim(s))
    }
    invisible(x)
}
# }}}
#' @export
# print.IdfRelationNode {{{
print.IdfRelationNode <- function(x, ...) {
    cli::cat_rule("Node Relation")
    if (!all(has_names(x, c("class_name", "object_name", "field_name", "src_class_name", "src_object_name", "src_field_name")))) {
        NextMethod("print")
        return(invisible(x))
    }

    if (!nrow(x)) {
        cli::cat_line("Target(s) has no node or their nodes have no reference to other object.")
    } else {
        s <- paste0(" ", unlist(format_idf_relation(x, "ref_by")$fmt, use.names = FALSE))
        cli::cat_line(cli::ansi_strtrim(s))
    }
    invisible(x)
}
# }}}
#' @export
# print.IdfRelation {{{
print.IdfRelation <- function(x, ...) {
    if (!is.null(x[["ref_to"]])) {print.IdfRelationTo(x[["ref_to"]]); cli::cat_line()}
    if (!is.null(x[["ref_by"]])) {print.IdfRelationBy(x[["ref_by"]]); cli::cat_line()}
    if (!is.null(x[["node"]])) print.IdfRelationNode(x[["node"]])
    invisible(x)
}
# }}}

# Possible
#' @export
# print.IddFieldPossible {{{
print.IddFieldPossible <- function(x, ...) {
    fmt <- format_possible(x)
    fmt[, field := rule(field), by = c(names(fmt)[1L:2L])]

    cli::cat_line(
        fmt[, list(out = paste0(Reduce(function(...) paste(..., sep = "\n"), .SD), "\n")),
            .SDcols = names(fmt)[-c(1L:2L)]
        ]$out
    )

    invisible(x)
}
# }}}
#' @export
# print.IdfValuePossible {{{
print.IdfValuePossible <- print.IddFieldPossible
# }}}

# Range
#' @export
# format.Range {{{
format.Range <- function(x, ...) {
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
print.Range <- function(x, ...) {
    cat(format.Range(x, ...), "\n")
    invisible(x)
}
# }}}
#' @export
# as.character.Range{{{
as.character.Range <- function(x, ...) {
    format.Range(x, ...)
}
# }}}

# STYLE
# with_nocolor {{{
with_nocolor <- function(...) {
    # inorder to skip color control sequence
    clr <- .globals$color
    on.exit({.globals$color <- clr; invisible()}, add = TRUE)
    .globals$color <- FALSE
    force(...)
}
# }}}
# has_color {{{
has_color <- function() {
    cli::num_ansi_colors() > 1L
}
# }}}
# s_nm: style for field names {{{
s_nm <- function(...) if (.globals$color) cli::style_italic(...) else c(...)
# }}}
# s_blk: style for blank {{{
s_blk <- function(...) if (.globals$color) cli::style_underline(...) else c(...)
# }}}

# vim: set fdm=marker:
