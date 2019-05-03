#' @importFrom cli cat_bullet cat_line cat_rule rule symbol
#' @importFrom data.table copy data.table dcast rbindlist
#' @importFrom data.table setattr setcolorder setnames setorder setorderv
#' @importFrom stringi stri_locate_first_regex stri_replace_first_regex "stri_sub<-"
#' @importFrom stringi stri_subset_regex stri_match_first_regex stri_rand_strings
#' @include impl-idd.R
NULL

# DOTS
# ...elt(n) to get the nth element in dots (after evaluation)
# ...length() to get the total number of element in dots (without evaluation)
# dot_string {{{
dot_string <- function (dt, collapse = "\n") {
    dt[, string := paste0(" #", lpad(rleid, "0"), "| ", dot)]
    dt[!is.na(dot_nm), string := paste0(" #", lpad(rleid, "0"), "| ", dot_nm, " = ", dot, collapse = collapse)]
    on.exit(set(dt, NULL, "string", NULL), add = TRUE)
    str_trunc(stri_replace_all_fixed(paste0(dt$string, collapse = collapse), "<environment>", "<IdfObject>"))
}
# }}}
# find_dot {{{
find_dot <- function (dot, dt) dt[dot, on = "rleid", mult = "first", nomatch = 0L]
# }}}
# old_input {{{
old_input <- function (which, value = NULL, comment = NULL, type = c("add", "set")) {
    assert_valid_input_format(class, value, comment, default, type)

    input <- rep(list(list()), length(which))

    if (!is.null(value)) {
        if (is_scalar(which)) {
            input <- list(value)
        } else {
            null <- vapply(value, is.null, logical(1L))
            input[null] <- rep(list(list()), sum(null))
            input[!null] <- value[!null]
        }
    }

    setattr(input, "names", which)
}
# }}}
# sep_name_dots {{{
sep_name_dots <- function (..., .can_name = TRUE) {
    l <- list(...)

    # stop if empty input
    if (!length(l)) abort("error_empty_input", "Please give object(s) to modify.")

    # check type of each element
    get_type <- function (x) {
        if (is.null(x)) {
            3L
        } else if (all(are_count(x)) && all(is.finite(x))) {
            1L
        } else if (is.character(x) && !anyNA(x)) {
            2L
        } else {
            3L
        }
    }
    type <- viapply(l, get_type)

    # put all data into a data.table
    dt_dot <- data.table(rleid = seq_along(l), dot = l, dot_nm = names2(l), type = type)

    # stop if invalid depth or type found
    if (any(type == 3L)) {
        abort("error_wrong_type",
            paste0("Each element must be a character vector or a positive integer",
                " vector. Invalid input:\n", dot_string(dt_dot[J(3L), on = "type"])
            )
        )
    }

    empty_input <- list(
        id = data.table(rleid = integer(), object_rleid = integer(),
            object_id = integer(), new_object_name = character()
        ),
        name = data.table(rleid = integer(), object_rleid = integer(),
            object_name = character(), new_object_name = character()
        )
    )

    nest_nmd <- integer()

    flatten <- function (dt, type) {
        t <- type
        empty <- if (type == 1L) "id" else "name"

        dt_dot[J(t), on = "type", {
            if (all(is.na(rleid))) return(empty_input[[empty]])

            len <- each_length(dot)
            rleid <- rep(rleid, len)
            obj <- unlist(dot, use.names = TRUE)
            if (.can_name) {
                new_nms <- names2(obj)
                dot_nm <- rep(dot_nm, len)
                nest_nmd <<- c(nest_nmd, unique(rleid[!is.na(dot_nm) & !is.na(new_nms)]))
                new_nms[is.na(new_nms)] <- dot_nm[is.na(new_nms)]
            } else {
                new_nms <- rep(NA_character_, sum(len))
            }

            res <- list(rleid = rleid,
                 object_rleid = unlist(lapply(len, seq.int), use.names = FALSE),
                 object = obj,
                 new_object_name = new_nms
            )

            setattr(res, "names", names(empty_input[[empty]]))
        }]
    }

    res <- list(id = flatten(dt, 1L), name = flatten(dt, 2L), dot = dt_dot)

    if (length(nest_nmd)) {
        warn("warning_nest_named",
            paste0(
                "Named vectors found in named input element. ",
                "Names of vectors will be used instead of element's name for:\n",
                dot_string(dt_dot[nest_nmd])
            )
        )
    }

    res
}
# }}}
# sep_value_dots {{{
sep_value_dots <- function (..., .empty = !in_final_mode(), .duplicate = FALSE) {
    l <- list(...)

    # stop if empty input
    if (!length(l)) {
        abort("error_empty_input", "Please give object(s) to add or set")
    }

    dt_dot <- data.table(rleid = seq_along(l),
        object_rleid = as.list(rep(1L, length(l))),
        dep = vapply(l, vec_depth, integer(1L)),
        dot = l, dot_nm = names2(l)
    )

    empty_input <- list(
        object = data.table(rleid = integer(), object_rleid = integer(),
            name = character(), comment = list(), empty = logical()
        ),
        value = data.table(rleid = integer(), object_rleid = integer(),
            field_name = character(), value_chr = character(), value_num = double(),
            defaulted = logical()
        )
    )

    is_empty_list <- function (x) is.list(x) && length(x) == 0L

    # abort invalid {{{
    abort_invalid_input <- function (id, err_type, mes) {
        abort(paste0("error_dot_", err_type),
            paste0(paste0(mes, collapse = ""), " Invalid input:\n",
                dot_string(dt_dot[J(id), on = "rleid"])
            )
        )
    }

    abort_invalid_format <- function (id) {
        abort_invalid_input(id, "invalid_format",
            c("Each object must be an empty list or a list where ",
              "each element being a non-NA single string or number.")
        )
    }

    abort_empty_dot <- function (id) {
        abort_invalid_input(id, "empty",
            "Empty input found. Please give field values to add or set."
        )
    }

    abort_multi_comment <- function (id) {
        abort_invalid_input(id, "multi_comment",
            "Each object can only have one `.comment` element."
        )
    }

    abort_duplicated_name <- function (id) {
        abort_invalid_input(id, "dup_field_name", "Field names must be unique.")
    }

    abort_comment_value <- function (id) {
        abort_invalid_input(id, "comment_value", "Field value cannot start with `!`.")
    }
    # }}}

    # flatten {{{
    flatten <- function (dt, minus = 0L) {
        dt[, {
            len <- viapply(dot, length, use.names = FALSE)
            dot <- unlist(dot, recursive = FALSE)
            # in case list()
            if (!length(dot)) dot <- rep(list(), len)
            list(rleid = rep(rleid, len),
                 object_rleid = as.list(unlist(object_rleid, use.names = FALSE)),
                 dep = rep(dep - minus, len),
                 dot = dot,
                 dot_nm = names2(dot)
            )
        }]
    }
    # }}}

    # flatten_input {{{
    flatten_input <- function (dt) {
        if (!nrow(dt)) return(empty_input)

        dep <- unique(dt$dep)

        # empty object, e.g. Construction = list()
        if (dep == 1L) {
            # {{{
            if (any(!vlapply(dt$dot, is.list) | is.na(dt$dot_nm))) {
                abort_invalid_format(dt[!vlapply(dot, is.list) | is.na(dot_nm), rleid])
            }

            set(dt, NULL, "object_rleid", unlist(dt$object_rleid, use.names = FALSE))
            set(dt, NULL, "dep", NULL)
            setnames(dt, "dot_nm", "name")

            if (nrow(dt) == 1L) {
                set(dt, NULL, "comment", list(list(NULL)))
            } else {
                set(dt, NULL, "comment", rep(list(NULL), nrow(dt)))
            }
            set(dt, NULL, "empty", vlapply(dt$dot, is_empty_list))

            if (!.empty && any(dt$empty)) abort_empty_dot(dt$rleid[dt$empty])

            if (all(dt$empty)) {
                val <- empty_input$value
            } else {
                val <- dt[empty == FALSE, {
                    # field number of each object
                    len <- each_length(dot)

                    # init field name
                    field_name <- rep("", sum(len))

                    # get field name and value
                    fld_val <- unlist(dot, recursive = FALSE, use.names = TRUE)

                    # set field name and value
                    field_name <- names2(fld_val)

                    # duplicated field name: "cls = list(a = 1, a = 2)"
                    if (anyDuplicated(stri_trans_tolower(field_name))) {
                        abort_duplicated_name(rleid)
                    }

                    value_chr <- rep(NA_character_, len)
                    value_num <- rep(NA_real_, len)

                    list(rleid = rep(rleid, len),
                         object_rleid = rep(object_rleid, len),
                         field_name = field_name,
                         value_chr = value_chr,
                         value_num = value_num,
                         defaulted = rep(TRUE, .N)
                    )
                }]
            }

            res <- list(object = set(dt, NULL, "dot", NULL), value = val)
            # }}}
        } else if (dep == 2L) {
            # {{{
            # check if is format "list(cls = list(NULL), cls = list())"
            dot_nmd <- !is.na(dt$dot_nm)

            # correct object rleid
            dt[!dot_nmd, `:=`(object_rleid = list(seq_len(each_length(dot)))), by = "rleid"]

            # flatten format: "list(cls = list(), cls = list(NULL, NULL))"
            dt_unnmd <- flatten(dt[!dot_nmd])

            # stop if object without names: "list(list(), list(NULL), list(NULL, NULL))"
            if (anyNA(dt_unnmd$dot_nm) | any(vlapply(dt_unnmd$dot, is.null))) {
                abort_invalid_format(dt_unnmd[is.na(dot_nm) | vlapply(dt_unnmd$dot, is.null), unique(rleid)])
            }

            # combine
            obj <- rbindlist(list(dt_unnmd, dt[dot_nmd]))

            # check if empty object
            set(obj, NULL, "empty", vlapply(obj$dot, is_empty_list))

            # stop if empty object is not allowed
            if (!.empty && any(obj$empty)) abort_empty_dot(unique(obj$rleid[obj$empty]))

            # change object_rleid into integer vector
            set(obj, NULL, "object_rleid", unlist(obj$object_rleid, use.names = FALSE))

            # reorder
            setorderv(obj, c("rleid", "object_rleid"))

            set(obj, NULL, "dep", NULL)
            set(obj, NULL, "comment", list(rep(list(NULL), nrow(obj))))
            setnames(obj, "dot_nm", "name")

            if (all(obj$empty)) {
                val <- empty_input$value
            } else {
                val <- obj[empty == FALSE, {
                    # field number of each object
                    len <- each_length(dot)

                    # get an unique id for each object
                    uni_id <- .I

                    # init rleid and object_rleid
                    rleid <- rep(rleid, len)
                    object_rleid <- rep(object_rleid, len)
                    uni_id <- rep(uni_id, len)

                    # set all values to NULL by default
                    value_list <- rep(list(NULL), sum(len))

                    # init field name
                    field_name <- rep("", sum(len))

                    # get field name and value
                    fld_val <- unlist(dot, recursive = FALSE, use.names = TRUE)

                    # set field name and value
                    field_name <- names2(fld_val, "")
                    value_list <- unname(fld_val)

                    # init value in character and numeric format
                    value_chr <- rep(NA_character_, sum(len))
                    value_num <- rep(NA_real_, sum(len))

                    # init default value indicator
                    defaulted <- each_length(value_list) == 0L

                    # contains NA: "cls = list(NA), list(cls = list(NA))" {{{
                    # put this before check comment to make sure there is no NA
                    # in comments
                    if (anyNA(value_list, recursive = TRUE)) {
                        abort_invalid_format(unique(rleid[vlapply(value_list, anyNA)]))
                    }
                    # }}}

                    # duplicated comment: "cls = list(.comment = c("a"), .comment = NULL)" {{{
                    is_cmt <- field_name == ".comment"
                    if (any(is_cmt)) {
                        cmt_id <- uni_id[is_cmt]
                        # stop if multiple .comments found
                        if (anyDuplicated(cmt_id)) {
                            abort_multi_comment(unique(rleid[duplicated(uni_id[is_cmt])]))
                        }

                        # check if there is only comment in the input
                        len[cmt_id] <- len[cmt_id] - 1L

                        if (any(len == 0L)) set(obj, .I[len == 0L], "empty", TRUE)

                        # update comment, coerce to character
                        set(obj, cmt_id, "comment",
                            list(lapply(value_list[is_cmt],
                                    function (x) {
                                        x <- as.character(x);
                                        if (length(x)) x else NULL
                                    }
                            ))
                        )

                        # if all are comments, no value
                        if (all(len == 0L)) {return(empty_input$value)}

                        rleid <- rleid[!is_cmt]
                        object_rleid <- object_rleid[!is_cmt]
                        uni_id <- uni_id[!is_cmt]
                        field_name <- field_name[!is_cmt]
                        value_list <- value_list[!is_cmt]
                        value_chr <- value_chr[!is_cmt]
                        value_num <- value_num[!is_cmt]
                        defaulted <- defaulted[!is_cmt]
                    }
                    # }}}

                    # not scalar: "cls = list(1:5)" {{{
                    if (any(each_length(value_list) > 1L)) {
                        el <- each_length(value_list)
                        abort_invalid_format(unique(rep(rleid, el)[el > 1L]))
                    }
                    # }}}

                    # not NULL, character or numeric {{{
                    # this will find out "list()"
                    if (any(vlapply(value_list, function (x) !is.null(x) && !is.character(x) && !is.numeric(x)))) {
                        id <- rleid[vlapply(value_list, function (x) !is.null(x) && !is.character(x) && !is.numeric(x))]
                        abort_invalid_format(unique(id))
                    }
                    # }}}

                    # check if field names are given
                    no_nm <- stri_isempty(field_name)

                    # duplicated field name: "cls = list(a = 1, a = 2)" {{{
                    if (any(!no_nm) && anyDuplicated(data.table(uni_id[!no_nm], stri_trans_tolower(underscore_name(field_name[!no_nm]))))) {
                        dt <- data.table(id = uni_id[!no_nm], field_name = stri_trans_tolower(underscore_name(field_name[!no_nm])))
                        abort_duplicated_name(obj$rleid[dt$id[duplicated(dt)]])
                    }
                    # }}}

                    # comment value: "cls = list(a = "!b")" {{{
                    if (any(stri_startswith_fixed(unlist(value_list, use.names = FALSE), "!"))) {
                        abort_comment_value(
                            unique(rleid[!defaulted][
                                vlapply(value_list[!defaulted], stri_startswith_fixed, "!")
                            ])
                        )
                    }
                    # }}}

                    # get value in both character and numeric format
                    value_chr[!defaulted] <- as.character(unlist(value_list[!defaulted], use.names = FALSE))
                    # change empty strings to NA
                    s <- value_chr[!defaulted]
                    s <- stri_trim_both(s)
                    s[stri_isempty(s)] <- NA_character_
                    value_chr[!defaulted] <- s

                    value_num[!defaulted] <- suppressWarnings(as.double(s))

                    # change empty field names to NA
                    field_name[no_nm] <- NA_character_

                    list(rleid = rleid,
                         object_rleid = object_rleid,
                         field_name = field_name,
                         value_chr = value_chr,
                         value_num = value_num,
                         defaulted = defaulted
                    )
                }]
            }

            set(obj, NULL, "dot", NULL)
            res <- list(object = obj, value = val)
            # }}}
        } else if (dep == 3L) {
            if (any(!is.na(dt$dot_nm))) abort_invalid_format(dt[!is.na(dot_nm), rleid])
            dt[, `:=`(object_rleid = list(seq_len(each_length(dot)))), by = "rleid"]
            res <- flatten_input(flatten(dt, 1L))
        } else {
            abort_invalid_format(dt$rleid)
        }

        res
    }
    # }}}

    flat <- lapply(split(dt_dot, by = "dep"), flatten_input)

    list(object = rbindlist(lapply(flat, .subset2, "object"), use.names = TRUE),
         value = rbindlist(lapply(flat, .subset2, "value"), use.names = TRUE),
         dot = dt_dot
    )
}
# }}}
# sep_object_dots {{{
sep_object_dots <- function (...) {
    l <- list(...)

    # stop if empty input
    if (!length(l)) abort("error_empty_input", "Please give object(s) to insert.")

    # check type of each element
    get_depth <- function (x) {
        if (is_idfobject(x)) {
            1L
        } else if (is.list(x) & length(x) > 0) {
            2L
        } else {
            0L
        }
    }
    depth <- viapply(l, get_depth)

    # put all data into a data.table
    dt_dot <- data.table(rleid = seq_along(l), dot = l, dot_nm = names2(l), dep = depth)

    if (any(depth == 0L)) {
        abort("error_wrong_type",
            paste0("Each element must be an IdfObject or a list of IdfObjects. ",
                "Invalid input:\n", dot_string(dt_dot[J(0L), on = "dep"])
            )
        )
    }

    if (any(depth == 1L)) {
        dt_1 <- dt_dot[J(1L), on = "dep"]
    } else {
        dt_1 <- data.table()
    }

    if (any(depth == 2L)){
        dt_2 <- dt_dot[J(2L), on = "dep",
            {
                len <- each_length(dot)
                lst <- unlist(dot, recursive = FALSE, use.names = TRUE)
                list(dot = lst, dot_nm = names2(lst), dep = 1L)
            }, by = "rleid"]
    } else {
        dt_2 <- data.table()
    }

    dt <- rbindlist(list(dt_1, dt_2), use.names = TRUE)
    setorderv(dt, "rleid")
    add_rleid(dt, "object")

    dt[, c("version", "uuid", "object_id", "idd_env", "idf_env") := {
        if (!is_idfobject(dot[[1L]])) {
            abort("error_wrong_type",
                paste0("Each element must be an IdfObject or a list of IdfObjects. ",
                    "Invalid input:\n", dot_string(copy(.SD))
                )
            )
        }

        list(._get_private(._get_private(dot[[1L]])$m_parent)$m_version,
             ._get_private(._get_private(dot[[1L]])$m_parent)$m_log$uuid,
             ._get_private(dot[[1L]])$m_object_id,
             list(._get_private(dot[[1L]])$idd_env()),
             list(._get_private(dot[[1L]])$idf_env())
        )
    }, by = "object_rleid"]

    list(data = dt, dot = dt_dot)
}
# }}}
# sep_definition_dots {{{
sep_definition_dots <- function (..., .version = NULL) {
    l <- list(...)

    # stop if empty input
    if (!length(l)) abort("error_empty_input", "Please give object(s) to load.")

    # check type of each element
    get_depth <- function (x) {
        if (is.character(x)) {
            if (anyNA(x)) 0L else 1L
        } else if (is.data.frame(x)) {
            if (has_name(x, c("class", "index", "value"))) 2L else 0L
        } else {
            0L
        }
    }
    depth <- viapply(l, get_depth)

    # put all data into a data.table
    dt_dot <- data.table(rleid = seq_along(l), dot = l, dot_nm = names2(l), depth = depth)

    if (any(depth == 0L)) {
        abort("error_wrong_type",
            paste0("Each element must be a character vector with no NA or ",
                "a data frame with column `class`, `index` and `value`. ",
                "Invalid input:\n", str_trunc(dot_string(dt_dot[J(0L), on = "depth"]))
            )
        )
    }

    if (!any(depth == 1L)) {
        parsed <- list()
    } else {
        str_in <- unlist(dt_dot[J(1L), on = "depth", dot], use.names = FALSE)

        # get total line number in each dot
        l <- dt_dot[J(1L), on = "depth",
            {
                list(line_rleid = seq.int(length(dot[[1L]]) + stri_count_fixed(dot[[1L]], "\n")))
            },
            by = "rleid"
        ]

        # here insert an version string
        # if there is version definition in the input, there will be an
        # `error_multi_idf_ver` error
        str_in <- c(str_in, paste0("Version,", .version, ";"))

        parsed <- withCallingHandlers(
            parse_idf_file(paste0(str_in, collapse = "\n"), ref = FALSE),

            # ignore the warning of using given IDD
            warn_given_idd_used = function (w) invokeRestart("muffleWarning"),

            # modify messages if any error occurs
            error_parse_idf = function (e) {
                set(l, NULL, "line", seq.int(nrow(l)))

                data <- e$data

                # remove inserted version
                if (class(e)[[1L]] == "error_multiple_version") {
                    data <- data[!J(max(line)), on = "line"]
                }

                add_joined_cols(l, data, "line", c("rleid", "line_rleid"))
                set(data, NULL, "msg_each", paste0("[Input #", data$rleid, "]"))

                # use line in each input
                set(data, NULL, "line", data$line_rleid)

                # get line number in each object
                t <- switch(class(e)[[1L]],
                    error_multiple_version = "Adding Version object is prohibited",
                    error_unknown_line = "Invalid line found",
                    error_incomplete_object = "Incomplete object",
                    error_invalid_class = "Invalid class name",
                    error_invalid_field_number = "Invalid field number"
                )

                parse_issue(class(e)[[1L]], "idf", t, data)
            }
        )

        # remove inserted version object
        parsed$object <- parsed$object[!J(max(object_id)), on = "object_id"]
        parsed$value <- parsed$value[!J(max(object_id)), on = "object_id"]

        # add rleid for latter error printing
        set(l, NULL, "line_rleid", NULL)
        l <- unique(l)[, object_id := .I]
        set(l, NULL, "object_id", rleid(l$rleid))
        add_joined_cols(l, parsed$object, "object_id", "rleid")
        add_joined_cols(l, parsed$value, "object_id", "rleid")
    }

    # extract_dt_input {{{
    extract_dt_input <- function (dt, id) {
        dt <- as.data.table(dt)
        # check field index duplication
        if (has_name(dt, "id")) {
            if (anyDuplicated(dt, by = c("id", "class", "index"))) {
                abort("error_dot_def_index_dup",
                    paste0("When input is a data.frame, `id`, `class` and `index` ",
                        " column combined should not contain any duplication. ",
                        "Invalid input:\n",
                        str_trunc(dot_string(dt_dot[J(id), on = "rleid"]))
                    )
                )
            }
            set(dt, NULL, "object_id", rleid(dt$id, dt$class))
        } else if (anyDuplicated(dt, by = c("class", "index"))) {
            abort("error_dot_def_index_dup",
                paste0("When input is a data.frame, `class` and `index` column ",
                    "combined should not contain any duplication. Invalid input:\n",
                    str_trunc(dot_string(dt_dot[J(id), on = "rleid"]))
                )
            )
        } else {
            set(dt, NULL, "object_id", rleid(dt$class))
        }

        # value column should be either character or list
        if (!is.character(dt$value) && !is.list(dt$value)) {
            abort("error_dot_def_value_type",
                paste0("When input is a data.frame, `value` column should be ",
                    "either a character vector or a list. Invalid input:\n",
                    str_trunc(dot_string(dt_dot[J(id), on = "rleid"]))
                )
            )
        }

        defaulted <- rep(FALSE, nrow(dt))

        # if value is character, trim spaces and convert them into NAs
        if (is.character(dt$value)) {
            value_chr <- stri_trim_both(dt$value)
            value_chr[stri_isempty(value_chr)] <- NA_character_
            value_num <- suppressWarnings(as.double(value_chr))

            # an indicator of input value type. 1: character, 2: list
            type = 1L

            # mark NA input as defaulted
            defaulted[is.na(value_chr)] <- TRUE
        # if value is a list, each element should be a single character or a
        # number.
        } else {
            get_type <- function (x) {
                if (!is_scalar(x)) return(0L)
                if (is.numeric(x)) 1L else if (is.character(x)) 2L else 0L
            }

            type <- viapply(dt$value, get_type)

            if (any(type == 0L)) {
                abort("error_dot_def_value_type",
                    paste0("When input is a data.frame and `value` column is a list, ",
                        "each element in that list should be a single string or number. ",
                        "Invalid input:\n",
                        str_trunc(dot_string(dt_dot[J(id), on = "rleid"]))
                    )
                )
            }

            # change empty strings to NA
            value_chr <- unlist(dt$value, use.names = FALSE)
            value_chr <- stri_trim_both(value_chr)
            value_chr[stri_isempty(value_chr)] <- NA_character_

            value_num <- rep(NA_real_, nrow(dt))
            value_num[type == 1L] <- unlist(dt$value[type == 1L], use.names = FALSE)

            defaulted[is.na(value_chr)] <- TRUE

            # value cannot start with "!"
            if (any(stri_startswith_fixed(value_chr[!defaulted], "!"))) {
                abort("error_dot_def_value_type",
                    paste0("When input is a data.frame and `value` column is a list, ",
                        "string value should not be IDF comment, i.e. starts with `!`. ",
                        "Invalid input:\n",
                        str_trunc(dot_string(dt_dot[J(id), on = "rleid"]))
                    )
                )
            }

            # an indicator of input value type. 1: character, 2: list
            type = 2L
        }

        list(object_id = dt$object_id, class_name = dt$class,
             field_index = dt$index, value_chr = value_chr,
             value_num = value_num, defaulted = defaulted, type = type
        )
    }
    # }}}

    if (!any(depth == 2L)) {
        dt_dt <- data.table()
    } else  {
        # copy them before convert them to data.table
        dt_dt <- dt_dot[J(2L), on = "depth", extract_dt_input(dot[[1L]], rleid), by = "rleid"]
    }

    list(parsed = parsed, value = dt_dt, dot = dt_dot)
}
# }}}

# OBJECT
# get_idf_object {{{
get_idf_object <- function (idd_env, idf_env, class = NULL, object = NULL, property = NULL,
                            underscore = FALSE, ignore_case = FALSE) {
    # if no object is specified
    if (is.null(object)) {
        # if no class is specified
        if (is.null(class)) {
            obj <- add_class_name(idd_env, copy(idf_env$object))
            if (!is.null(property)) {
                obj <- add_class_property(idd_env, idf_env$object, property)
            }
        # if class is specified
        } else {
            cls_in <- recognize_input(class, "class", underscore)
            col_on <- names(cls_in)[[1L]]
            col_key <- col_on
            if (col_on == "class_id") {
                cls_in <- add_class_name(idd_env, cls_in)
            } else {
                # get class id
                cls_in <- join_from_input(
                    idd_env$class[, .SD, .SDcols = c("class_id", unique(c("class_name", col_on)))],
                    cls_in, "group_id"
                )
                col_on <- "class_id"
            }

            col_add <- setdiff(names(idf_env$object), names(cls_in))

            if (!length(col_add)) {
                obj <- cls_in
            } else {
                obj <- idf_env$object[, .SD, .SDcols = c(col_on, col_add)][, check := .I][cls_in, on = col_on]
                check_bad_key(obj, "check", col_key)
                set(obj, NULL, "check", NULL)
            }

            if (!is.null(property)) obj <- add_class_property(idd_env, obj, property)
        }
    # if object is specified
    } else {
        obj_in <- recognize_input(object, "object", lower = ignore_case)
        obj_in <- del_redundant_cols(idf_env$object, obj_in)
        # if no class is specified
        if (is.null(class)) {
            obj <- join_from_input(idf_env$object, obj_in, "class_id")
            obj <- add_class_name(idd_env, obj)
            # add property if necessary
            if (!is.null(property)) obj <- add_class_property(idd_env, obj, property)
        # if class is specified
        } else {
            cls_in <- recognize_input(class, "class", underscore)
            if (names(cls_in)[[1L]] == "class_id") {
                cls_in <- add_class_name(idd_env, cls_in)
            } else {
                # get class id
                cls_in <- join_from_input(
                    idd_env$class[, .SD, .SDcols = c("class_id", unique(c("class_name", names(cls_in)[[1L]])))],
                    cls_in, "group_id"
                )
            }

            # add property if necessary
            if (!is.null(property)) cls_in <- add_class_property(idd_env, cls_in, property)
            # delete rleid in class
            set(cls_in, NULL, "rleid", NULL)

            # if only one class is specified, recycle
            if (nrow(cls_in) == 1L) cls_in <- cls_in[rep(1L, nrow(obj_in))]
            assert(have_same_len(cls_in, obj_in), prefix = "class and object")

            col_on <- names(obj_in)[[1L]]

            # combine all input
            obj_in <- cbind(cls_in, obj_in)

            # add an indicator column to check if bad key is found
            set(idf_env$object, NULL, "ind", 0L)
            on.exit(set(idf_env$object, NULL, "ind", NULL))

            obj <- idf_env$object[obj_in, on = c("class_id", col_on), allow.cartesian = TRUE]

            check_bad_key(obj, "ind", col_on)
            set(obj, NULL, "ind", NULL)
        }

        # stop if there are objects that have the same name {{{
        if (!have_same_len(obj, object)) {
            mult_rleid <- obj[, .N, by = rleid][N > 1L, rleid]
            mult <- obj[J(mult_rleid), on = "rleid"]

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
                data = obj
            )
        }
        # }}}
    }

    obj
}
# }}}
# get_idf_object_id {{{
get_idf_object_id <- function (idd_env, idf_env, class = NULL, simplify = FALSE) {
    obj <- get_idf_object(idd_env, idf_env, class)
    if (simplify) return(obj$object_id)

    obj <- add_class_name(idd_env, obj)

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
get_idf_object_name <- function (idd_env, idf_env, class = NULL, simplify = FALSE, lower = FALSE) {
    obj <- get_idf_object(idd_env, idf_env, class)
    col <- if (lower) "object_name_lower" else "object_name"
    if (simplify) return(obj[[col]])

    obj <- add_class_name(idd_env, obj)

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
get_idf_object_num <- function (idd_env, idf_env, class = NULL) {
    if (is.null(class)) return(nrow(idf_env$object))

    cls_in <- recognize_input(class, "class")
    col_on <- names(cls_in)[[1L]]
    if (any(!cls_in[[col_on]] %in% idd_env$class[[col_on]])) {
        col_key <- if (col_on == "class_id") "class index" else "class name"
        abort_bad_key("error_invalid_class", col_key,
            idd_env$class[cls_in, on = col_on][is.na(group_id), .SD, .SDcols = col_on][[col_on]]
        )
    }

    if (col_on == "class_name") cls_in <- add_class_id(idd_env, cls_in)

    idf_env$object[cls_in, on = "class_id", allow.cartesian = TRUE][
        , .N, by = list(rleid, found = !is.na(object_id))][found == FALSE, N := 0L]$N
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
                paste0("Input #", lpad(-object_id, "0")),
                paste0("ID [", lpad(object_id, "0"), "]")
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
            mes <- dt_object[, paste0(key_cls, ": ", surround(class_name))]
        } else {
            set(dt_object, NULL, "mes_object", mes)
            dt_object[!stri_isempty(mes_object), mes_object := paste0(" ", mes_object)]
            if (by_class) {
                mes <- dt_object[, {
                    paste0(key_obj, collapse(mes_object, NULL), " in class ", surround(class_name[1L]))
                }, by = class_name]$V1
            } else {
                mes <- dt_object[, {
                    paste0(key_obj, mes_object, " in class ", surround(class_name))
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
                num <- paste0(" #", lpad(dt_object[, unique(rleid), by = class_name]$V1, "0"), "| ")
            } else {
                num <- paste0(" #", lpad(dt_object$rleid, "0"), "| ")
            }
        } else {
            num <- paste0(" #", lpad(seq_along(mes), "0"), "| ")
        }
        mes <- paste0(num, mes)
    }

    paste0(mes, collapse = collapse)
}
# }}}

# VALUE
# get_idf_value {{{
# Return all object value data in a object
get_idf_value <- function (idd_env, idf_env, class = NULL, object = NULL, field = NULL,
                           property = NULL, underscore = FALSE, ignore_case = FALSE,
                           align = FALSE, complete = FALSE, all = FALSE) {
    obj <- get_idf_object(idd_env, idf_env, class, object, NULL, underscore, ignore_case)
    set(obj, NULL, c("object_name_lower", "comment"), NULL)

    # if field is not speicifed
    if (is.null(field)) {
        val <- idf_env$value[obj, on = "object_id", allow.cartesian = TRUE]

        # if just want to get the value, no special treatment is required
        if (!(all || complete || align)) {
            add_joined_cols(idd_env$field, val, "field_id", c("field_index", "field_name", property))
            return(val)
        }

        # as class name already exist in fld
        set(obj, NULL, "class_name", NULL)

        # find field number per object
        set(obj, NULL, "num", val[, .N, by = c("rleid", "object_id")]$N)

        # if align is TRUE, make sure all objects in one class have same field number
        if (align) {
            obj[, num := rep(max(num), .N), by = "class_id"]

            # if neither all nor complete is TRUE, then have to manually create
            # field index, otherwise only the last field will be returned
            if (!(all || complete)) {
                obj <- obj[, {
                    num <- seq.int(num)
                    list(rleid = rep(rleid, length(num)),
                         object_name = rep(object_name, length(num)),
                         num = num
                    )
                }, by = c("class_id", "object_id")]
            }
        }
    # if field is specified
    } else {
        assert(!is.null(class) || !is.null(object),
            msg = "When `field` is not NULL, either `class` or `object` should be not NULL."
        )

        # as class name already exist in fld
        set(obj, NULL, "class_name", NULL)

        # if class or object is a scalar, then this means that field should be
        # applied to every target object
        if (is_scalar(class) || is_scalar(object)) {
            obj <- obj[,
                list(rleid = rep(rleid, length(field)),
                    object_name = rep(object_name, length(field)),
                    num = field
                ),
                by = c("class_id", "object_id")
            ]
        } else {
            assert(have_same_len(class, field) || have_same_len(object, field))
            obj[, num := field[[.GRP]], by = "rleid"]
        }
    }

    fld <- get_idd_field(idd_env, obj$class_id, obj$num,
        property = property, all = all, complete = complete)
    set(fld, NULL, "field_in", NULL)

    # get the actual field number per input class
    set(obj, NULL, "num_fld", fld[, .N, by = "rleid"]$N)
    set(obj, NULL, "max_fld", fld[, max(field_index), by = "rleid"]$V1)

    # correct rleid and add object id and name in fld
    set(fld, NULL, c("object_id", "object_name"),
        obj[, list(rep(object_id, num_fld), rep(object_name, num_fld))]
    )

    # merge value and field
    ## just for efficiency
    if (is.null(field)) {
        set(fld, NULL, "rleid", obj$rleid[fld$rleid])

        # remove these columns as them already exist in fld
        set(val, NULL, c("rleid", "class_id", "class_name", "object_name"), NULL)

        val <- val[fld, on = c("object_id", "field_id")]
    } else {
        # if special treatment is required, use the max field number
        if (align) {
            nom <- NA
            set(fld, NULL, "rleid", obj[, list(rep(rleid, num_fld))])
        } else if (all || complete) {
            nom <- NA
            id <- obj[, .I[which.max(max_fld)], by = c("rleid", "object_id")]$V1
            fld <- fld[J(id), on = c("rleid")]
            set(fld, NULL, "rleid", obj[id, list(rep(rleid, num_fld))])
        } else {
            nom <- 0L
            set(fld, NULL, "rleid", obj[, list(rep(rleid, num_fld))])
        }
        val <- idf_env$value[fld, on = c("object_id", "field_id"), nomatch = nom]
    }

    val[is.na(value_id), value_id := -.I]

    val
}
# }}}
# get_idf_value_all_node {{{
get_idf_value_all_node <- function (idf_env) {
    idf_env$value[type_enum == IDDFIELD_TYPE$node & !is.na(value_chr), unique(value_chr)]
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
assert_can_do <- function (idd_env, idf_env, dot, object,
                           action = c("add", "dup", "set", "del", "rename", "insert")) {
    # stop attempting to touch Version {{{
    if (any(object$class_id == 1L)) {
        invld <- find_dot(dot, object[class_id == 1L])

        m <- paste0(dot_string(invld, NULL), " --> Class ", invld$class_name, collapse = "\n")

        abort(paste0("error_", action, "_version"),
            paste0(
                switch(action,
                    add = "Adding",
                    dup = "Duplicating",
                    set = "Modifying",
                    del = "Deleting",
                    rename = "Modifying",
                    insert = "Inserting"),
                " Version object is prohibited. Invalid input:\n", m
            ),
            dot = dot, object = object
        )
    }
    # }}}

    if (level_checks()$unique_object && action %in% c("add", "dup", "del", "insert")) {
        uni <- object[class_id %in% idd_env$class[unique_object == TRUE, class_id]]
        if (nrow(uni)) {
            # try to add or dup new unique object that already exists {{{
            if (action %in% c("add", "dup", "insert") && any(get_idf_object_num(idd_env, idf_env, uni$class_id) > 0L)) {
                invld <- find_dot(dot, uni[get_idf_object_num(idd_env, idf_env, class_id) > 0L])
                # add object id and name
                if (action %in% c("add", "insert")) {
                    invld <- idf_env$object[, list(class_id, object_id, object_name)][
                        invld, on = c("class_id")]
                }
                info <- get_object_info(invld, collapse = NULL, numbered = FALSE)

                m <- paste0(dot_string(invld, NULL), " --> ", info, collapse = "\n")
                invld[, paste0(" #", lpad(rleid, "0"), "| ", dot, collapse = "\n")]

                abort(paste0("error_", action, "_unique"),
                    paste0(
                        switch(action,
                            add = "Adding new object in existing unique-object class is prohibited.",
                            dup = "Existing unique object cannot be duplicated.",
                            insert = "Inserting new object in existing unique-object class is prohibited."
                        ),
                        " Invalid input:\n", m
                    ),
                    dot = dot, object = object
                )
            }
            # }}}
            # try to add multi objects in unique classes {{{
            if (action %in% c("add", "insert") && anyDuplicated(uni$class_id)) {
                invld <- find_dot(dot, uni[duplicated(class_id)])

                m <- paste0(dot_string(invld, NULL), " --> Class: ", invld$class_name, collapse = "\n")
                act <- switch(action, add = "added", insert = "inserted")

                abort("error_add_multi_unique",
                    paste0("Unique object can only be ",act," once. Invalid input\n", m),
                    dot = dot, object = object
                )
            }
            # }}}
            # try do del unique object {{{
            if (action == "del" && any(get_idf_object_num(idd_env, idf_env, uni$class_id) == 0L)) {

                invld <- find_dot(dot, uni[get_idf_object_num(idd_env, idf_env, class_id) == 0L])

                info <- get_object_info(invld, collapse = NULL)

                m <- paste0(dot_string(invld, NULL), " --> ", info, collapse = "\n")

                abort("error_del_exist_unique",
                    paste0("Existing unique object can not be deleted. Invalid input\n", m),
                    dot = dot, object = object
                )
            }
            # }}}
        }
    }

    # stop attempting to delete required objects {{{
    if (action == "del" && level_checks()$required_object &&
        any(object$class_id %in% idd_env$class[required_object == TRUE, class_id])) {
        invld <- find_dot(dot, object[class_id %in% idd_env$class[required_object == TRUE, class_id]])

        info <- get_object_info(invld, numbered = FALSE, collapse = NULL)

        m <- paste0(dot_string(invld, NULL), " --> ", info, collapse = "\n")

        abort("error_del_required",
            paste0("Deleting a required object is prohibited. Invalid input:\n", m),
            dot = dot, object = object
        )
    }
    # }}}

    # stop if modifying same object multiple times {{{
    if (action %in% c("set", "del", "rename") && anyDuplicated(object$object_id)) {
        invld <- find_dot(dot, object[duplicated(class_id)])
        info <- get_object_info(invld, numbered = FALSE)
        m <- paste0(dot_string(invld, NULL), " --> ", info, collapse = "\n")

        abort(paste0("error_", action, "_multi_time"),
            paste0("Cannot modify same object multiple times. Invalid input:\n", m),
            dot = dot, object = object
        )
    }
    # }}}

    # stop if no new name is provided when renaming {{{
    if (action == "rename" && anyNA(object$new_object_name)) {
        invld <- find_dot(dot, object[is.na(new_object_name)])
        m <- paste0(dot_string(invld, NULL),
            " --> ",
            get_object_info(invld, collapse = NULL, numbered = FALSE),
            collapse = "\n"
        )

        mes <- paste0("Please give new object names. Invalid input:\n", m)
        abort(paste0("error_", action, "_no_new_name"), mes, data = invld)
    }
    # }}}

    # stop if tring to assign names to objects that do not have name attribute {{{
    if (action %chin% c("dup", "rename") && object[has_name == FALSE & !is.na(new_object_name), .N]) {
        invld <- find_dot(dot, object[has_name == FALSE & !is.na(new_object_name)])
        m <- paste0(dot_string(invld, NULL),
            " --> ",
            get_object_info(invld, c("id", "class"), collapse = NULL, numbered = FALSE),
            collapse = "\n"
        )

        mes <- paste0("Target object(s) in class that does not have name attribute ",
            "cannot be renamed. Invalid input:\n", m
        )
        abort(paste0("error_", action, "_cannot_rename"), mes, data = invld)
    }
    # }}}

    TRUE
}
# }}}
# assert_valid {{{
assert_valid <- function (idd_env, idf_env, object, value, action = c("dup", "add", "set", "rename", "insert")) {
    action <- match.arg(action)
    if (action %chin% c("dup", "rename")) {
        validity <- validate_objects(idd_env, idf_env,
            copy(object)[, object_id := -rleid],
            # copy needed here to enable object id correction based on object id
            copy(value)[, object_id := -rleid],
            unique_name = level_checks()$unique_name
        )
    } else {
        # validate fields that do not use default values and all extensible fields
        value <- value[required_field == TRUE | defaulted == FALSE | extensible_group > 0L]
        if (action %in% c("add", "insert")) {
            validity <- validate_on_level(idd_env, idf_env,
                object[J(unique(value$object_id)), on = "object_id"][, object_id := -rleid],
                value[, object_id := -rleid],
                level = eplusr_option("validate_level")
            )
        } else if (action == "set") {
            validity <- validate_on_level(idd_env, idf_env,
                object[J(unique(value$object_id)), on = "object_id"],
                value,
                level = eplusr_option("validate_level")
            )
        }
    }

    if (count_check_error(validity)) {
        on.exit(options(warning.length = getOption("warning.length")), add = TRUE)
        options(warning.length = 8170)
        m <- paste0(capture.output(print_validity(validity)), collapse = "\n")
        if (action == "dup") {
            t <- paste0(
                "Failed to duplicate object(s). ",
                "Input new name(s) cannot be the same as target object(s) or ",
                "any existing object in the same class."
            )
        } else {
            t <- paste0("Failed to ",action," object(s).")
        }
        abort("error_validity",  paste0(t, "\n\n", m))
    }

    TRUE
}
# }}}

# OBJECT MUNIPULATION
# dup_idf_object {{{
dup_idf_object <- function (idd_env, idf_env, ...) {
    l <- sep_name_dots(..., .can_name = TRUE)
    obj <- get_object_input(idd_env, idf_env, l, property = "has_name", keep_duplicate = TRUE)

    # stop if cannot add objects in specified classes
    assert_can_do(idd_env, idf_env, l$dot, obj, "dup")

    # make sure rleid column as the unique id
    set(obj, NULL, "rleid", rleid(obj$rleid, obj$object_rleid))
    set(obj, NULL, "object_rleid", NULL)

    # check input new names {{{
    # get value data
    val <- get_idf_value(idd_env, idf_env, object = obj$object_id,
        property = c("is_name", "type_enum", "src_enum")
    )
    set(val, NULL, "rleid", obj[val$rleid, rleid])

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
        list(obj$new_object_name, stri_trans_tolower(obj$new_object_name)))

    # assign name field in order to make sure new object name is used during
    # error printing
    val[is_name == TRUE, `:=`(value_chr = obj[has_name == TRUE, object_name])]

    assert_valid(idd_env, idf_env, obj, val, "dup")
    # }}}

    # assign new object id after validation
    obj <- assign_new_id(idf_env, obj, "object")
    set(val, NULL, "object_id", obj[J(val$rleid), on = "rleid", object_id])

    # get new object name {{{
    # get indicator of whether user input new names are used
    set(obj, NULL, "use_input_name", FALSE)
    obj[has_name == TRUE & !is.na(object_name_lower) &
        (object_name_lower != old_object_name_lower | is.na(old_object_name_lower)),
        `:=`(use_input_name = TRUE)
    ]

    # get all name in the same class
    obj[, `:=`(all_name_lower = get_idf_object_name(idd_env, idf_env, class_id, lower = TRUE)), by = "rleid"]

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
    val[is_name == TRUE, `:=`(value_chr = obj[has_name == TRUE, object_name])]

    # logging
    if (nrow(obj[auto_assigned == TRUE])) {
        auto <- obj[auto_assigned == TRUE]
        id <- get_object_info(auto, "id")
        name <- get_object_info(auto, "name", prefix = " --> New ", numbered = FALSE)
        verbose_info(
            "New names of duplicated objects not given are automatically generated:\n",
            paste0(id, name, collapse = "\n")
        )
    }
    # }}}

    # value reference
    ## directly copy old field references excepting the name field
    ref <- idf_env$reference[J(val$old_value_id[!val$is_name]), on = "value_id", nomatch = 0L]
    set(ref, NULL, c("object_id", "value_id"),
        val[match(ref$value_id, val$old_value_id), .SD, .SDcols = c("object_id", "value_id")])
    ## for original objects whose fields are referred by others, just keep the
    ## original relation and no new relation needs to be created as one value
    ## can only refer to one other value
    ## however, it is possible that new input object names can be referred by
    ## other existing objects
    src <- get_value_reference_map(idd_env$reference,
        append_dt(idf_env$value, val), val[is_name == TRUE]
    )
    new_ref <- rbindlist(list(ref, src))

    list(object = del_unuseful_cols(idf_env$object, obj),
         value = del_unuseful_cols(idf_env$value, val),
         reference = append_dt(idf_env$reference, new_ref)
    )
}
# }}}
# add_idf_object {{{
add_idf_object <- function (idd_env, idf_env, ..., .default = TRUE, .all = FALSE) {
    l <- sep_value_dots(..., .empty = TRUE)

    # new object table
    obj <- get_idd_class(idd_env, setnames(l$object, "name", "class_name")$class_name, underscore = TRUE)
    set(obj, NULL, c("object_rleid", "comment", "empty"),
        l$object[obj$rleid, .SD, .SDcols = c("object_rleid", "comment", "empty")]
    )

    # stop if cannot add objects in specified classes
    assert_can_do(idd_env, idf_env, l$dot, obj, "add")

    # add object id
    obj <- assign_new_id(idf_env, obj, "object")

    # make sure rleid column as the unique id
    set(obj, NULL, "new_rleid", rleid(obj$rleid, obj$object_rleid))

    # new value table
    val <- l$value[obj[, -c("empty", "comment", "group_id")],
        on = c("rleid", "object_rleid"), nomatch = 0L
    ]

    # clean old rleid
    set(obj, NULL, c("rleid", "object_rleid"), NULL)
    set(val, NULL, c("rleid", "object_rleid"), NULL)
    setnames(obj, "new_rleid", "rleid")
    setnames(val, "new_rleid", "rleid")

    prop <- c("units", "ip_units", "default_chr", "default_num", "is_name",
        "required_field", "src_enum", "type_enum", "extensible_group"
    )

    # get empty objects {{{
    val_empty <- obj[J(TRUE), on = "empty", nomatch = 0L]
    if (!nrow(val_empty)) {
        val_empty <- data.table()
    } else {
        set(val_empty, NULL, c("empty", "comment", "group_id"), NULL)
        val_empty_fld <- get_idd_field(idd_env, val_empty$class_id, all = .all, underscore = FALSE, property = prop)
        # insert object id back
        val_empty <- add_joined_cols(val_empty, val_empty_fld, "rleid", "object_id")

        # add input field index indicator
        set(val_empty, NULL, "field_in", NA_integer_)

        # add value and value in number
        set(val_empty, NULL, c("value_chr", "value_num"), list(NA_character_, NA_real_))

        # find fields to be filled with default values
        set(val_empty, NULL, "defaulted", TRUE)
    }
    # }}}

    # get non-empty objects {{{
    if (!nrow(val)) {
        val <- val_empty[0L]
    } else {
        if (any(is.na(val$field_name))) {
            val <- fill_unnamed_field_index(idd_env, idf_env, val)
        # all named
        } else {
            # just to verify field names
            fld_out <- get_idd_field(idd_env, class = val$class_id, field = val$field_name)
            # set matched field index
            set(val, NULL, "field_index", fld_out$field_index)
            # remove input field name
            set(val, NULL, "field_name", NULL)
        }

        # now all field indices have been detected
        fld_in <- val[, list(class_id = class_id[[1L]], num = max(field_index)), by = c("rleid", "object_id")]
        fld_out <- get_idd_field(idd_env, class = fld_in$class_id, field = fld_in$num,
            all = .all, complete = TRUE, property = prop
        )
        # reset rleid in fld_out
        set(fld_out, NULL, c("rleid", "object_id"), fld_in[fld_out$rleid, list(rleid, object_id)])

        # remove duplicated columns
        set(val, NULL, c("class_id", "class_name"), NULL)
        val <- val[fld_out, on = c("rleid", "object_id", "field_index")]

        if (.default) val[is.na(defaulted), defaulted := TRUE]
    }
    # }}}

    # combine empty and non-empty objects
    val <- rbindlist(list(val_empty, val), use.names = TRUE)

    # order
    setorderv(val, c("rleid", "field_index"))

    # assign default values if needed
    if (.default) val <- assign_default_value(val)
    set(val, NULL, c("default_chr", "default_num"), NULL)

    # assign new value id
    val <- assign_new_id(idf_env, val, "value")

    # update object name
    obj <- update_object_name(obj, val)
    set(val, NULL, "is_name", NULL)
    # add lower name
    set(obj, NULL, "object_name_lower", stri_trans_tolower(obj$object_name))

    # validate
    assert_valid(idd_env, idf_env, obj, val, action = "add")

    list(object = obj[, .SD, .SDcols = names(idf_env$object)],
         value = val[, .SD, .SDcols = names(idf_env$value)],
         reference = update_value_reference(idd_env, idf_env, obj, val, "add")
    )
}
# }}}
# set_idf_object {{{
set_idf_object <- function (idd_env, idf_env, ..., .default = TRUE) {
    l <- sep_value_dots(..., .empty = FALSE)

    # get object ID in `..X` format
    setnames(l$object, c("name", "comment"), c("object_name", "new_comment"))
    set(l$object, NULL, "object_id", as.integer(stri_match_first_regex(l$object$object_name, "^\\.\\.(\\d+)$")[, 2L]))

    # separate
    obj_id_in <- l$object[!is.na(object_id)]
    set(obj_id_in, NULL, "object_name", NULL)
    obj_nm_in <- l$object[is.na(object_id)]
    set(obj_nm_in, NULL, "object_id", NULL)

    # get object data
    obj_id <- get_idf_object(idd_env, idf_env, object = obj_id_in$object_id)
    set(obj_id, NULL, names(obj_id_in), obj_id_in)
    obj_nm <- get_idf_object(idd_env, idf_env, object = obj_nm_in$object_name, ignore_case = TRUE)
    set(obj_nm, NULL, names(obj_nm_in), obj_nm_in)

    # combine
    obj <- rbindlist(list(obj_id, obj_nm))

    # update comment
    # NOTE: have to use `:=` format here as comment is a list
    obj[!vlapply(new_comment, is.null), `:=`(comment = new_comment)]
    set(obj, NULL, c("new_comment", "empty"), NULL)

    # stop if cannot set objects
    assert_can_do(idd_env, idf_env, l$dot, obj, "set")

    # make sure rleid column as the unique id
    set(obj, NULL, "new_rleid", rleid(obj$rleid, obj$object_rleid))

    # new value table
    val <- l$value[obj[, -c("comment")], on = c("rleid", "object_rleid"), nomatch = 0L ]

    # clean old rleid
    set(obj, NULL, c("rleid", "object_rleid"), NULL)
    set(val, NULL, c("rleid", "object_rleid"), NULL)
    setnames(obj, "new_rleid", "rleid")
    setnames(val, "new_rleid", "rleid")

    prop <- c("units", "ip_units", "default_chr", "default_num", "is_name",
        "required_field", "src_enum", "type_enum", "extensible_group"
    )

    setnames(val, c("value_chr", "value_num"), c("new_value", "new_value_num"))

    if (any(is.na(val$field_name))) {
        val <- fill_unnamed_field_index(idd_env, idf_env, val)
        idx <- val$field_index
    } else {
        # just to verify field names
        fld_out <- get_idd_field(idd_env, class = val$class_id, field = val$field_name)
        # set matched field index
        set(val, NULL, "field_index", fld_out$field_index)
        # remove input field name
        set(val, NULL, "field_name", NULL)
    }

    # in order to delete field values, here get all value numbers in current class
    fld_in <- val[, list(num = max(field_index)), by = c("rleid", "object_id")]
    fld_cur <- idf_env$value[J(fld_in$object_id), on = "object_id",
        list(object_id = object_id[[1L]], num = .N), by = "object_id"
    ]
    # get the max field number
    fld_in$num <- pmax(fld_in$num, fld_cur$num)

    val_out <- get_idf_value(idd_env, idf_env, object = fld_in$object_id, field = fld_in$num,
        complete = TRUE, property = prop
    )
    # reset rleid in val_out
    set(val_out, NULL, c("rleid"), val$rleid[val_out$rleid])
    set(val_out, NULL, c("new_value", "new_value_num", "defaulted"),
        val[val_out, on = c("rleid", "field_index"),
            .SD, .SDcols = c("new_value", "new_value_num", "defaulted")
        ]
    )
    val <- val_out

    if (.default) val[is.na(value_chr) & is.na(defaulted), defaulted := TRUE]
    val[is.na(defaulted), defaulted := FALSE]

    # exclude name field if it has been already set before in order to
    # prevent name conflict checking error
    val[is_name == TRUE & !is.na(value_chr) & is.na(new_value),
        `:=`(required_field = FALSE)
    ]

    # order
    setorderv(val, c("rleid", "field_index"))

    # assign default values if needed
    if (.default) {
        val <- assign_default_value(val)
    } else {
        # remove
        val[defaulted == TRUE, `:=`(value_chr = NA_character_, value_num = NA_real_)]
    }
    set(val, NULL, c("default_chr", "default_num"), NULL)

    # assign new values
    val[!is.na(new_value), `:=`(value_chr = new_value, value_num = new_value_num)]
    set(val, NULL, c("new_value", "new_value_num"), NULL)

    # assign new value id
    val[value_id < 0L, value_id := new_id(idf_env$value, "value_id", .N)]

    # update object name
    obj <- update_object_name(obj, val)
    set(val, NULL, "is_name", NULL)
    # add lower name
    set(obj, NULL, "object_name_lower", stri_trans_tolower(obj$object_name))

    # delete fields
    add_joined_cols(idd_env$class, val, "class_id", c("min_fields", "num_extensible"))
    val <- remove_empty_fields(val)

    # validate
    assert_valid(idd_env, idf_env, obj, val, action = "set")

    list(object = obj[, .SD, .SDcols = names(idf_env$object)],
         value = val[, .SD, .SDcols = names(idf_env$value)],
         reference = update_value_reference(idd_env, idf_env, obj, val, "set")
    )
}
# }}}
# del_idf_object {{{
del_idf_object <- function (idd_env, idf_env, ..., .ref_to = FALSE, .ref_by = FALSE,
                            .recursive = FALSE, .force = FALSE) {
    l <- sep_name_dots(..., .can_name = TRUE)
    obj <- get_object_input(idd_env, idf_env, l, keep_duplicate = TRUE)
    set(obj, NULL, c("object_name_lower", "comment", "new_object_name"), NULL)

    assert_can_do(idd_env, idf_env, l$dot, obj, "del")

    # get objects to be deleted
    id_del <- obj$object_id

    # always check if targets objects are referred by others
    dir <- if (.ref_to) "all" else "ref_by"

    rel <- get_idfobj_relation(idd_env, idf_env, id_del, direction = dir,
        max_depth = NULL, recursive = .recursive, name = eplusr_option("verbose_info")
    )

    if (eplusr_option("verbose_info")) {
        msg <- paste0("Deleting object(s) [ID: ", paste(id_del, sep = ", ", collapse = ", "), "]")
    }

    id_ref_by <- c()

    if (nrow(rel$ref_by)) {
        # stop if objects are referred {{{
        if (!.ref_by && !.force) {
            if (!eplusr_option("verbose_info")) {
                rel$ref_by <- add_idf_relation_format_cols(idd_env, idf_env, rel$ref_by)
            }
            abort("error_del_referenced",
                paste0(
                    "Cannot delete object(s) that are referred by others:\n",
                    "\n",
                    paste0("  ", unlist(format_idf_relation(rel$ref_by, "ref_by")$fmt, use.names = FALSE), collapse = "\n")
                )
            )
        }
        # }}}

        if (.ref_by) {
            id_ref_by <- setdiff(unique(rel$ref_by$object_id), id_del)

            # check if objects that refer to targets are also referred by other
            # objects
            id_src <- id_ref_by[id_ref_by %in% idf_env$reference$src_object_id]
            if (length(id_src)) {
                id_ref_by <- setdiff(id_ref_by, id_src)
                if (eplusr_option("verbose_info")) {
                    if (length(id_ref_by)) {
                        msg <- c(msg,
                            paste0(
                                "Including object(s) [ID:", paste(id_ref_by, collapse = ", "), "] that refer to it, ",
                                "skipping object(s) [ID: ", paste0(id_src, collapse = ","), "] that is referred by other objects."
                            )
                        )
                    } else {
                        msg <- c(msg,
                            paste0("Skipping object(s) [ID: ", paste0(id_src, collapse = ","), "] that is referred by other objects.")
                        )
                    }
                }
            } else {
                if (eplusr_option("verbose_info")) {
                    msg <- c(msg, "",
                        paste0("Including object(s) [ID:", paste(id_ref_by, collapse = ", "), "] that refer to it.")
                    )
                }
            }
        }
    }

    # if .ref_to is TRUE and rel$ref_to has contents
    if (NROW(rel$ref_to)) {
        id_ref_to <- setdiff(unique(rel$ref_to$src_object_id))

        # check if objects that target refers to are also referred by other
        # objects
        id_src <- idf_env$reference[!J(id_del), on = "object_id"][
            J(rel$ref_to$src_object_id), on = "src_object_id", nomatch = 0L, src_object_id
        ]
        id_src <- setdiff(id_src, id_del)
        if (length(id_src)) {
            id_ref_to <- setdiff(id_ref_to, id_src)
            if (eplusr_option("verbose_info")) {
                if (length(id_ref_to)) {
                    msg <- c(msg, "",
                        paste0(
                            "Including object(s) [ID:", paste(id_ref_to, collapse = ", "), "] that is referred by it, ",
                            "skipping object(s) [ID: ", paste0(id_src, collapse = ","), "] that is also referred by other objects."
                        )
                    )
                } else {
                    msg <- c(msg, "",
                        paste0("Skipping object(s) [ID: ", paste0(id_src, collapse = ","), "] that is also referred by other objects.")
                    )
                }
            }
        } else {
            if (eplusr_option("verbose_info")) {
                msg <- c(msg, "",
                    paste0("Including object(s) [ID:", paste(id_ref_by, collapse = ", "), "] that is referred by it.")
                )
            }
        }
    }

    if (eplusr_option("verbose_info")) {
        msg <- paste0(c(msg, "", "Object relation is shown below:", ""), collapse = "\n")
        msg_rel <- paste0(" ", capture.output(print.IdfRelation(rel)), collapse = "\n")
        verbose_info(paste0(msg, msg_rel, collapse = "\n"))
    }

    id_del <- if (.ref_to) c(id_del, id_ref_by, id_ref_to) else c(id_del, id_ref_by)

    # delete rows in object table
    dt_object <- idf_env$object[!J(id_del), on = "object_id"]
    dt_value <- idf_env$value[!J(id_del), on = "object_id"]
    # keep invalid reference
    dt_reference <- idf_env$reference[!J(id_del), on = "object_id"][
        J(id_del), on = "src_object_id",
        `:=`(src_object_id = NA_integer_, src_value_id = NA_integer_)
    ]

    list(object = dt_object[, .SD, .SDcols = names(idf_env$object)],
         value = dt_value[, .SD, .SDcols = names(idf_env$value)],
         reference = dt_reference
    )
}
# }}}
# rename_idf_object {{{
rename_idf_object <- function (idd_env, idf_env, ...) {
    l <- sep_name_dots(..., .can_name = TRUE)
    obj <- get_object_input(idd_env, idf_env, l, property = "has_name", keep_duplicate = TRUE)

    # stop if input object does not have name attribute
    assert_can_do(idd_env, idf_env, l$dot, obj, "rename")

    # make sure rleid column as the unique id
    set(obj, NULL, "rleid", rleid(obj$rleid, obj$object_rleid))
    set(obj, NULL, "object_rleid", NULL)

    # check input new names {{{
    # get value data
    val <- get_idf_value(idd_env, idf_env, object = obj$object_id,
        property = c("is_name", "type_enum", "src_enum")
    )
    val <- val[is_name == TRUE]
    set(val, NULL, "rleid", obj$rleid[val$rleid])

    # assign new object name
    set(obj, NULL, c("object_name", "object_name_lower"),
        list(obj$new_object_name, stri_trans_tolower(obj$new_object_name)))

    # assign name field in order to make sure new object name is used during
    # error printing
    set(val, NULL, "value_chr", obj$object_name)

    assert_valid(idd_env, idf_env, obj, val, "rename")
    # }}}

    # value reference
    # if name is referred by other objects, update others
    ref_by <- get_idf_relation(idd_env, idf_env, value_id = val$value_id, direction = "ref_by")
    add_joined_cols(val, ref_by, c(src_value_id = "value_id"), c(src_value_chr = "value_chr"))

    # if name itself is a reference, remove that relation for that depth
    ref <- idf_env$reference[!J(ref_by$src_value_id), on = "value_id"]

    # update values in main table
    if (nrow(ref_by)) {
        idf_env$value[J(ref_by$value_id), on = "value_id", `:=`(value_chr = ref_by$src_value_chr)]
        idf_env$value[J(ref_by$src_value_id), on = "value_id", `:=`(value_chr = ref_by$src_value_chr)]
    }

    list(object = obj[, .SD, .SDcols = names(idf_env$object)],
         value = val[, .SD, .SDcols = names(idf_env$value)],
         reference = ref
    )
}
# }}}
# insert_idf_object {{{
insert_idf_object <- function (idd_env, idf_env, version, ..., .unique = TRUE) {
    l <- sep_object_dots(...)
    ver <- version
    input <- l$data

    # stop if version is different
    if (input[version != ver, .N]) {
        abort("error_not_same_version",
            paste0(
                "Input object(s) should be IdfObjects with version ", surround(ver), ". ",
                "Invalid input:\n",
                paste0(dot_string(l$dot[J(input[version != ver, unique(rleid)]), on = "rleid"], NULL), collapse = "\n")
            )
        )
    }

    # get object table
    obj <- input[, list(list(
        set(get_idf_object(idd_env[[1L]], idf_env[[1L]], object = object_id, property = "has_name"),
            NULL, c("rleid"), list(object_rleid)
        )
        )), by = "object_rleid"]$V1
    obj <- rbindlist(obj)

    # stop of trying to add Version object
    if (any(obj$class_id == 1L)) {
        invld <- find_dot(l$dot, obj[class_id == 1L])
        m <- paste0(dot_string(invld, NULL), " --> Class ", invld$class_name, collapse = "\n")
        abort("error_insert_version",
            paste0("Inserting Version object is prohibited. Invalid input:\n", m)
        )
    }

    prop <- c("units", "ip_units", "default_chr", "default_num", "is_name",
        "required_field", "src_enum", "type_enum", "extensible_group"
    )
    val <- input[, list(list(
        set(get_idf_value(idd_env[[1L]], idf_env[[1L]], object = object_id, complete = TRUE, property = prop),
            NULL, "rleid", object_rleid
        )
        )), by = "object_rleid"]$V1
    val <- rbindlist(val)
    # update name field
    val[is_name == TRUE, `:=`(value_chr = obj$object_name[obj$has_name], value_num = NA_real_)]
    # set newly added fields to default value if possible
    set(val, NULL, "defaulted", FALSE)
    val[value_id < 0L, defaulted := TRUE]
    val <- assign_default_value(val)

    # update object id
    obj <- assign_new_id(idf_env, obj, "object")
    add_joined_cols(obj, val, "rleid", "object_id")
    # update value id
    val <- assign_new_id(idf_env, val, "value")

    # remove empty fields
    add_class_property(idd_env, val, c("min_fields", "num_extensible"))
    val <- remove_empty_fields(val)

    # remove duplicated objects
    if (.unique) {
        obj_val <- remove_duplicated_objects(idd_env, idf_env, obj, val)
        obj <- obj_val$object
        val <- obj_val$value
    }

    # stop if cannot insert objects in specified classes
    assert_can_do(idd_env, idf_env, l$dot, obj, "insert")

    # if all inputs are duplications
    if (!nrow(obj)) {
        return(list(object = idf_env$object[0L], value = idf_env$value[0L], reference = idf_env$reference))
    }

    # validate
    assert_valid(idd_env, idf_env, obj, val, action = "insert")

    list(object = obj[, .SD, .SDcols = names(idf_env$object)],
         value = val[, .SD, .SDcols = names(idf_env$value)],
         reference = update_value_reference(idd_env, idf_env, obj, val, "add")
    )
}
# }}}
# paste_idf_object {{{
paste_idf_object <- function (idd_env, idf_env, version, in_ip = FALSE, unique = TRUE, default = TRUE) {
    parsed <- read_idfeditor_copy(version, in_ip)

    # add class name
    add_class_name(idd_env, parsed$object)
    # add class id and field index
    add_joined_cols(parsed$object, parsed$value, "object_id", c("class_id", "class_name"))
    add_joined_cols(idd_env$field, parsed$value, "field_id", "field_index")

    # remove version object
    obj_ver <- parsed$object[class_name == "Version", object_id]
    parsed$object <- parsed$object[!J(obj_ver), on = "object_id"]
    parsed$value <- parsed$value[!J(obj_ver), on = "object_id"]

    # delete empty fields
    add_joined_cols(idd_env$class, parsed$value, "class_id", c("min_fields", "num_extensible"))
    add_field_property(idd_env, parsed$value, "required_field")
    parsed$value <- remove_empty_fields(parsed$value)

    # remove duplicated objects
    if (unique) {
        parsed <- remove_duplicated_objects(idd_env, idf_env, parsed$object, parsed$value)
    }

    # if all inputs are duplications
    if (!nrow(parsed$object)) {
        return(list(object = idf_env$object[0L], value = idf_env$value[0L], reference = idf_env$reference))
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

    # add field defaults if possible
    set(parsed$value, NULL, "defaulted", FALSE)
    if (default) {
        parsed$value[is.na(value_chr), defaulted := TRUE]
        add_field_property(idd_env, parsed$value, c("default_chr", "default_num", "units", "ip_units"))
        parsed$value <- assign_default_value(parsed$value)
    }

    # validate
    # add necessary columns for validation
    add_field_property(idd_env, parsed$value, c("extensible_group", "type_enum", "src_enum"))
    assert_valid(idd_env, idf_env, parsed$object, parsed$value, action = "add")

    # get field reference in whole fields
    add_joined_cols(idf_env$object, idf_env$value, "object_id", c("class_id"))
    add_class_name(idd_env, idf_env$value)
    add_field_property(idd_env, idf_env$value, c("type_enum", "src_enum"))
    parsed$reference <- get_value_reference_map(idd_env$reference,
        append_dt(idf_env$value, parsed$value, "value_id"),
        append_dt(idf_env$value, parsed$value, "value_id")
    )
    set(idf_env$value, NULL, c("class_id", "class_name", "type_enum", "src_enum"), NULL)

    list(object = parsed$object[, .SD, .SDcols = names(idf_env$object)],
         value = parsed$value[, .SD, .SDcols = names(idf_env$value)],
         reference = parsed$reference
    )
}
# }}}
# load_idf_object {{{
load_idf_object <- function (idd_env, idf_env, version, ..., .unique = TRUE, .default = TRUE) {
    l <- sep_definition_dots(..., .version = version)

    prop <- c("is_name", "required_field", "src_enum", "type_enum", "extensible_group")
    if (.default) prop <- c(prop, c("units", "ip_units", "default_chr", "default_num"))

    # get object and value from data.frame input {{{
    if (!nrow(l$value)) {
        obj_dt <- data.table()
        val_dt <- data.table()
    } else {
        # verify class name and add class id
        cls <- tryCatch(get_idd_class(idd_env, l$value$class_name, property = c("has_name")),
            error_class_name = function (e) {
                # get input with invalid class name
                id <- l$value[J(e$value), on = "class_name", rleid]
                abort("error_class_name",
                    paste0("Invalid class name ", collapse(unique(e$value)), " found in input:\n",
                        dot_string(l$dot[J(id), on = "rleid"])
                    )
                )
            }
        )

        set(l$value, NULL, c("class_id", "class_name", "has_name"),
            list(cls$class_id, cls$class_name, cls$has_name)
        )

        obj_dt <- l$value[,
            list(rleid =  rleid[[1L]], class_id = class_id[[1L]],
                 class_name = class_name[[1L]], has_name = has_name[[1L]],
                 num = max(field_index)
            ),
            by = "object_id"
        ]

        # stop of trying to add Version object
        if (any(obj_dt$class_id == 1L)) {
            invld <- find_dot(l$dot, obj_dt[class_id == 1L])
            m <- paste0(dot_string(invld, NULL), " --> Class ", invld$class_name, collapse = "\n")
            abort("error_add_version",
                paste0("Adding Version object is prohibited. Invalid input:\n", m)
            )
        }

        # verify class name and add class id
        fld_out <- tryCatch(
            get_idd_field(idd_env, class = obj_dt$class_id, field = obj_dt$num, complete = TRUE, property = prop),
            error_bad_field_index = function (e) {
                # update rleid
                e$data[, rleid := obj_dt$rleid[e$data$rleid]]
                abort_bad_field("error_bad_field_index", "index", e$data)
            }
        )

        # reset rleid in fld_out
        set(fld_out, NULL, c("rleid", "object_id"), obj_dt[fld_out$rleid, list(rleid, object_id)])

        # remove unuseful column
        set(fld_out, NULL, "field_in", NULL)

        # remove duplicated columns
        set(l$value, NULL, c("class_id", "class_name", "has_name"), NULL)
        val_dt <- l$value[fld_out, on = c("rleid", "object_id", "field_index")]

        # if input is character vectors, need to reset values since all of them
        # are coerced regardless of field types
        val_dt[type == 1L & type_enum > IDDFIELD_TYPE$real, `:=`(value_num = NA_real_)]
        set(val_dt, NULL, "type", NULL)

        if (.default) val_dt[is.na(defaulted), defaulted := TRUE]

        # order
        setorderv(val_dt, c("rleid", "field_index"))

        # remove unuseful columns
        set(obj_dt, NULL, "num", NULL)

        # add comment column
        set(obj_dt, NULL, "comment", list(list(NULL)))
    }
    # }}}

    # get object and value from character input {{{
    if (!length(l$parsed)) {
        obj_chr <- data.table()
        val_chr <- data.table()
    } else {
        obj_chr <- l$parsed$object
        val_chr <- l$parsed$value

        # add class name
        add_class_name(idd_env, obj_chr)
        add_class_property(idd_env, obj_chr, "has_name")
        # add class id and field index
        add_joined_cols(obj_chr, val_chr, "object_id", c("class_id", "class_name"))
        add_joined_cols(idd_env$field, val_chr, "field_id", c("field_index", "field_name"))
        # add field property
        add_field_property(idd_env, val_chr, prop)

        # add field defaults if possible
        set(val_chr, NULL, "defaulted", FALSE)
        if (.default) val_chr[is.na(value_chr), defaulted := TRUE]
    }
    # }}}

    obj <- rbindlist(list(obj_chr, obj_dt), fill = TRUE)
    val <- rbindlist(list(val_chr, val_dt), fill = TRUE)
    setorderv(val, c("rleid", "object_id", "field_id"))

    # assign object id and value id
    obj <- assign_new_id(idf_env, obj, "object")
    # make sure rleid is unique
    set(obj, NULL, "rleid", seq.int(nrow(obj)))
    val <- assign_new_id(idf_env, val, "value")
    set(val, NULL, "object_id", rleid(val$rleid, val$object_id))
    set(val, NULL, "rleid", val$object_id)
    val <- correct_obj_id(obj, val)
    # update object name
    obj <- update_object_name(obj, val)

    # assign default
    if (.default) val <- assign_default_value(val)

    # remove duplicated objects
    if (.unique) {
        parsed <- remove_duplicated_objects(idd_env, idf_env, obj, val)
    } else {
        parsed <- list(object = obj, value = val)
    }

    # if all inputs are duplications
    if (!nrow(parsed$object)) {
        return(list(object = idf_env$object[0L], value = idf_env$value[0L], reference = idf_env$reference))
    }

    # check if adding objects in specific class is allowed
    assert_can_do(idd_env, idf_env, l$dot, parsed$object, action = "add")

    # validate
    assert_valid(idd_env, idf_env, parsed$object, parsed$value, action = "add")

    # get field reference in whole fields
    add_joined_cols(idf_env$object, idf_env$value, "object_id", "class_id")
    add_class_name(idd_env, idf_env$value)
    add_field_property(idd_env, idf_env$value, c("type_enum", "src_enum"))
    parsed$reference <- get_value_reference_map(idd_env$reference,
        append_dt(idf_env$value, parsed$value, "value_id"),
        append_dt(idf_env$value, parsed$value, "value_id")
    )
    set(idf_env$value, NULL, c("class_id", "class_name", "type_enum", "src_enum"), NULL)

    # update object name
    parsed$object <- update_object_name(parsed$object, parsed$value)
    # add lower name
    set(parsed$object, NULL, "object_name_lower", stri_trans_tolower(parsed$object$object_name))
    set(parsed$object, NULL, "has_name", NULL)

    list(object = parsed$object[, .SD, .SDcols = names(idf_env$object)],
         value = parsed$value[, .SD, .SDcols = names(idf_env$value)],
         reference = parsed$reference
    )
}
# }}}
# purge_idf_object {{{
purge_idf_object <- function (idd_env, idf_env, class = c("Schedule", "Construction", "Material")) {
    stop("Not implemented")
}
# }}}
# search_idf_value {{{
search_idf_value <- function (idd_env, idf_env, pattern, class = NULL, ignore.case = FALSE,
                              perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    if (!is.null(class) && anyDuplicated(class)) {
        abort("error_search_object_dup_class",
              "Class should not contain any duplication.", class = class
        )
    }

    val <- get_idf_value(idd_env, idf_env, class)

    val <- val[grepl(pattern, value_chr, ignore.case = ignore.case, perl = perl,
        fixed = fixed, useBytes = useBytes)
    ]

    if (!nrow(val)) {
        verbose_info("No matched result found.")
        return(invisible())
    }

    val
}
# }}}
# replace_idf_value {{{
replace_idf_value <- function (idd_env, idf_env, pattern, replacement,
                               class = NULL, ignore.case = FALSE,
                               perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    if (!is.null(class) && anyDuplicated(class)) {
        abort("error_search_object_dup_class",
              "Class should not contain any duplication.", class = class
        )
    }

    prop <- c("units", "ip_units", "is_name", "required_field", "src_enum", "type_enum", "extensible_group")

    val <- get_idf_value(idd_env, idf_env, class, property = prop)

    val <- val[grepl(pattern, value_chr, ignore.case = ignore.case, perl = perl,
        fixed = fixed, useBytes = useBytes)
    ]

    if (!nrow(val)) {
        verbose_info("No matched result found.")
        return(invisible())
    }

    set(val, NULL, "value_chr",
        gsub(pattern, replacement, val$value_chr, ignore.case = ignore.case,
            perl = perl, fixed = fixed, useBytes = useBytes
        )
    )
    set(val, NULL, "value_num", suppressWarnings(as.numeric(val$value_chr)))
    set(val, NULL, "defaulted", FALSE)

    obj <- get_idf_object(idd_env, idf_env, object = unique(val$object_id))
    # update object name
    obj <- update_object_name(obj, val)

    assert_valid(idd_env, idf_env, obj, val, action = "set")

    list(object = obj, value = val,
         reference = update_value_reference(idd_env, idf_env, obj, val, "set")
    )
}
# }}}
# get_object_input {{{
get_object_input <- function (idd_env, idf_env, l, property = NULL, keep_duplicate = TRUE) {
    # match
    if (nrow(l$id)) {
        obj_id <- get_idf_object(idd_env, idf_env, object = l$id$object_id, property = property)
        obj_id <- cbind(
            set(obj_id, NULL, "rleid", NULL),
            l$id[, .SD, .SDcols = c("rleid", "object_rleid", "new_object_name")]
        )
    } else {
        obj_id <- idf_env$object[0L]
    }
    if (nrow(l$name)) {
        obj_nm <- get_idf_object(idd_env, idf_env, object = l$name$object_name,
            property = property, ignore_case = TRUE
        )
        obj_nm <- cbind(
            set(obj_nm, NULL, "rleid", NULL),
            l$name[, .SD, .SDcols = c("rleid", "object_rleid", "new_object_name")]
        )
    } else {
        obj_nm <- idf_env$object[0L]
    }

    # remain the input order
    obj <- rbindlist(list(obj_id, obj_nm), fill = TRUE)
    setorderv(obj, "rleid")

    if (keep_duplicate) return(obj)

    unique(obj, by = "object_id")
}
# }}}
# fill_unnamed_field_index {{{
fill_unnamed_field_index <- function (idd_env, idf_env, val) {
    # match field names and get field index
    val_nm <- val[!is.na(field_name)]
    val_nm_fld <- get_idd_field(idd_env, val_nm$class_id, val_nm$field_name)
    set(val_nm_fld, NULL, "rleid", val_nm$rleid)
    set(val, NULL, "field_index",
        val_nm_fld[val, on = c("rleid", field_in = "field_name"), field_index]
    )
    val[, field_index := {
        # whether field index is detected by using field name
        no_nm <- is.na(field_index)
        # how many field indices
        s <- seq_along(field_index)
        # no field name is given
        if (all(no_nm)) {
            s
        # some field name is given
        } else {
            # what are left after excluding detected field indices
            idx <- setdiff(s, field_index[!no_nm])
            field_index[no_nm] <- idx[seq.int(sum(no_nm))]
            field_index
        }
    }, by = "rleid"]

    set(val, NULL, "field_name", NULL)

    val
}
# }}}
# remove_empty_fields {{{
remove_empty_fields <- function (val) {
    if (!val[required_field == FALSE & is.na(value_chr) & min_fields < field_index, .N]) return(val)

    # fields that can be deleted:
    # 1. not required
    # 2. do not have value
    # 3. field index should be consecutive from the end
    # 4. should be a whole extensible group
    val[, rev_field_rleid := rev(field_index), by = "object_id"]
    id_del <- val[required_field == FALSE & is.na(value_chr) & field_index > min_fields,
        {
            if (!.N) {
                list(value_id = NA_integer_)
            } else {
                whole_ext <- TRUE
                if (num_extensible[[1L]] > 0) {
                    whole_ext <- .N %% num_extensible[[1L]] == 0L
                }

                if (!whole_ext) {
                    list(value_id = NA_integer_)
                } else {
                    if (all(rev_field_rleid == rev(seq.int(.N)))) {
                        list(value_id = value_id)
                    } else {
                        list(vlaue_id = NA_integer_)
                    }
                }
            }
        },
        by = c("object_id")
    ]

    if (any(!is.na(id_del$value_id))) val <- val[!id_del, on = "value_id"]

    val
}
# }}}
# remove_duplicated_objects {{{
remove_duplicated_objects <- function (idd_env, idf_env, obj, val) {
    # extract all object values in the same class
    # in order to distinguish input from original IDF, set id of objects
    # from IDF Editor to negative also note that dcast will automatically
    # order object id, so this makes that input objects are always in the
    # bottom.
    add_joined_cols(idd_env$field, idf_env$value, "field_id", "field_index")
    add_joined_cols(idf_env$object, idf_env$value, "object_id", "class_id")
    val_idf <- idf_env$value[J(unique(obj$class_id)), on = "class_id",
        list(class_id, object_id = -object_id, field_index, value_chr), nomatch = 0L]
    set(idf_env$value, NULL, c("class_id", "field_index"), NULL)

    # if there are no objects in the same class
    if (!nrow(val_idf)) return(list(object = obj, value = val))

    # get all input value
    val_in <- val[, list(class_id, object_id, field_index, value_chr)]

    # dcast to compare
    val_d <- dcast(rbindlist(list(val_idf, val_in), fill = TRUE),
        class_id + object_id ~ field_index, value.var = "value_chr")

    # get indicator
    dup <- duplicated(val_d, by = setdiff(names(val_d), c("object_id", "class_id")))

    # only find duplicates in input
    id_dup <- val_d[dup & object_id > 0L, object_id]

    if (length(id_dup)) {
        # give info
        verbose_info(
            "Duplicated objects in input or objects in input that are the same in current IDF have been removed:\n",
            {
                del <- obj[J(id_dup), on = "object_id"]
                setorderv(del, "rleid")
                get_object_info(del, c("name", "class"), collapse = "\n", name_prefix = FALSE)
            }
        )

        obj <- obj[!J(id_dup), on = "object_id"]
        val <- val[!J(id_dup), on = "object_id"]
    }

    list(object = obj, value = val)
}
# }}}

# REFERENCES
# get_idf_relation {{{
get_idf_relation <- function (idd_env, idf_env, object_id = NULL, value_id = NULL,
                              max_depth = NULL, name = FALSE,
                              direction = c("ref_to", "ref_by"), keep_all = FALSE,
                              recursive = FALSE) {
    direction <- match.arg(direction)

    ref <- get_idf_relation_at_depth(idd_env, idf_env, object_id, value_id,
        max_depth, direction, keep_all
    )

    if (recursive && nrow(ref)) {
        rec_in <- switch(direction, ref_to = "src_object_id", ref_by = "object_id")
        new_id <- unique(ref[[rec_in]][!is.na(ref[[rec_in]])])

        get_rec_ref <- function (recref, md, new_id) {
            if (!length(new_id)) return(ref)

            rec_ref <- get_idf_relation_at_depth(idd_env, idf_env, new_id,
                NULL, max_depth, direction, keep_all = FALSE
            )

            # increase the depth
            set(rec_ref, NULL, "dep", rec_ref$dep + md + 1L)

            # exclude self reference to avoid infinite loop
            # `Name` <--> `Outside Boundary Condition Object` in `BuildingSurface:Detaild`
            rec_ref <- rec_ref[!ref, on = c("object_id", "value_id", "src_object_id", "src_value_id")]

            ref <<- rbindlist(list(ref, rec_ref), use.names = TRUE)

            new_id <- unique(rec_ref[[rec_in]][!is.na(rec_ref[[rec_in]])])

            get_rec_ref(rec_ref, max(rec_ref$dep), new_id)

            ref
        }

        ref <- get_rec_ref(ref, max(ref$dep), new_id)
    }

    if (!name) return(ref)

    ref <- add_idf_relation_format_cols(idd_env, idf_env, ref)

    cls <- switch(direction, ref_by = "IdfRelationBy", ref_to = "IdfRelationTo")
    setattr(ref, "class", c(cls, class(ref)))
    ref
}
# }}}
# get_idf_relation_at_depth {{{
get_idf_relation_at_depth <- function (idd_env, idf_env, object_id = NULL, value_id = NULL,
                                       max_depth = NULL, direction = c("ref_to", "ref_by"),
                                       keep_all = FALSE) {
    assert(is.null(max_depth) || is_count(max_depth, TRUE))
    direction <- match.arg(direction)

    # if no value id is given
    if (is.null(value_id)) {
        # if no object id is given
        if (is.null(object_id)) {
            # use all objects in current IDF
            id <- idf_env$object$object_id
        } else {
            # use specified object IDs
            id <- object_id
        }
        col_on <- c("object_id", "value_id")
    # if value ids are given
    } else {
        # if no object id is given
        if (is.null(object_id)) {
            # use specified value id
            id <- value_id
        # if object IDs are given
        } else {
            # make sure object IDs and value ids have the same length
            assert(have_same_len(object_id, value_id))
            obj_id <- object_id
            val_id <- value_id

            # find value ids
            id <- idf_env[J(obj_id, val_id), on = c("object_id", "value_id"), value_id]
        }
        col_on <- "value_id"
    }

    if (keep_all) {
        # make sure all input IDs appear in the result
        val <- idf_env$value[J(id), on = col_on[[1L]], .SD, .SDcols = c("value_id", "object_id")]
    }

    if (direction == "ref_to") {
        col_rec <- "src_value_id"
    } else if (direction == "ref_by") {
        col_on <- paste0("src_", col_on)
        col_rec <- "value_id"
    }

    dep <- 0L
    if (is.null(max_depth)) max_depth <- Inf

    ref <- get_recref(idf_env$reference, id, col_on, col_rec, dep, max_depth)

    # keep all input {{{
    if (keep_all) {
        if (direction == "ref_to") {
            set(ref, NULL, "object_id", NULL)
            if (!nrow(ref) || max(ref$dep) == 0L) {
                ref <- ref[val, on = "value_id"]
                set(ref, NULL, "dep", 0L)
            } else {
                ref0 <- ref[J(0L), on = "dep"]
                ref0 <- ref0[val, on = "value_id"]
                set(ref0, NULL, "dep", 0L)
                ref <- append_dt(ref0, ref[!J(0L), on = "dep"])
            }
        } else {
            set(ref, NULL, "src_object_id", NULL)
            setnames(val, c("src_value_id", "src_object_id"))
            if (!nrow(ref) || max(ref$dep) == 0L) {
                ref <- ref[val, on = "src_value_id"]
                set(ref, NULL, "dep", 0L)
            } else {
                ref0 <- ref[J(0L), on = "dep"]
                ref0 <- ref0[val, on = "src_value_id"]
                set(ref0, NULL, "dep", 0L)
                ref <- append_dt(ref0, ref[!J(0L), on = "dep"])
            }
        }
    }
    # }}}

    ref
}
# }}}
# add_idf_relation_format_cols {{{
add_idf_relation_format_cols <- function (idd_env, idf_env, ref) {
    # add all necessary columns for printing
    ref <- add_joined_cols(idf_env$object, ref, "object_id", c("class_id", "object_name"))
    ref <- add_joined_cols(idf_env$value, ref, "value_id",
        c("field_id", "value_chr", "value_num"))
    ref <- add_joined_cols(idf_env$object, ref, c(src_object_id = "object_id"),
        c(src_class_id = "class_id", src_object_name = "object_name"))
    ref <- add_joined_cols(idf_env$value, ref, c(src_value_id = "value_id"),
        c(src_field_id = "field_id", src_value_chr = "value_chr", src_value_num = "value_num"))
    ref <- add_idd_relation_format_cols(idd_env, ref)
    ref <- add_joined_cols(idd_env$field, ref, "field_id", "type_enum")
    ref <- add_joined_cols(idd_env$field, ref, c(src_field_id = "field_id"), c(src_type_enum = "type_enum"))

    setcolorder(ref,
        c("class_id", "class_name",
          "object_id", "object_name",
          "field_id", "field_index", "field_name",
          "value_id", "value_chr", "value_num", "type_enum",
          "src_class_id", "src_class_name",
          "src_object_id", "src_object_name",
          "src_field_id", "src_field_index", "src_field_name",
          "src_value_id", "src_value_chr", "src_value_num", "src_type_enum",
          "src_enum", "dep"
        )
    )

    ref
}
# }}}
# update_value_reference {{{
update_value_reference <- function (idd_env, idf_env, object, value, action = c("add", "set")) {
    # If field reference has been handled and updated during validation, only
    # check sources
    if (level_checks()$reference) {
        set(object, NULL, "rleid", -object$rleid)

        # update object id as new object id during validation
        input_ref <- idf_env$reference[object_id < 0L, which = TRUE]
        if (length(input_ref)) {
            set(idf_env$reference, input_ref, "object_id",
                object[J(idf_env$reference$object_id[input_ref]), on = "rleid", object_id]
            )
        }

        input_src <- idf_env$reference[src_object_id < 0L, which = TRUE]
        if (length(input_src)) {
            set(idf_env$reference, input_src, "src_object_id",
                object[J(idf_env$reference$src_object_id[input_src]), on = "rleid", object_id]
            )
        }

        # if have sources
        if (any(value$src_enum > IDDFIELD_SOURCE$none)) {
            idf_env <- update_referenced_value(idd_env, idf_env, value, action)
            idf_env$value <- add_field_property(idd_env, idf_env$value, "type_enum")
            val <- value
            new_ref <- get_value_reference_map(idd_env$reference,
                value[!J(idf_env$value$value_id), on = "value_id"],
                idf_env$value[!J(val$value_id), on = "value_id"], all = FALSE
            )
            set(idf_env$value, NULL, "type_enum", NULL)
            if (nrow(new_ref)) {
                ref <- rbindlist(list(idf_env$reference, new_ref))
            } else {
                ref <- idf_env$reference
            }
        } else {
            ref <- idf_env$reference
        }
    } else {
        idf_env$value <- add_field_property(idd_env, idf_env$value, c("src_enum", "type_enum"))
        if (any(value$type_enum == IDDFIELD_TYPE$object_list)) {
            new_ref <- TRUE
            val_ref <- append_dt(idf_env$value, value, "value_id")
        } else {
            new_ref <- FALSE
            val_ref <- idf_env$value
        }

        # add class name
        set(idf_env$value, NULL, "class_id", idf_env$object[J(idf_env$value$object_id), on = "object_id", class_id])
        idf_env$value <- add_class_name(idd_env, idf_env$value)
        if (any(value$src_enum > IDDFIELD_SOURCE$none)) {
            idf_env <- update_referenced_value(idd_env, idf_env, value, action)

            new_src <- TRUE
            val_src <- append_dt(idf_env$value, value, "value_id")
        } else {
            new_src <- FALSE
            val_src <- idf_env$value
        }

        if (!new_ref && !new_src) {
            ref <- idf_env$reference
        } else {
            ref <- get_value_reference_map(idd_env$reference, val_src, val_ref)
        }
        set(idf_env$value, NULL, c("class_id", "class_name", "src_enum", "type_enum"), NULL)
    }

    ref
}
# }}}
# update_referenced_value {{{
update_referenced_value <- function (idd_env, idf_env, value, action = c("add", "set")) {
    # get old values that refer to current input recursively
    # inter-references in the input should be excluded
    depth <- if (action == "add") 0L else NULL
    ref <- get_idf_relation(idd_env, idf_env,
        value_id = value[src_enum > IDDFIELD_SOURCE$none, value_id],
        direction = "ref_by", max_depth = depth
    )

    if (!nrow(ref)) return(idf_env)

    depth <- 0L
    max_depth <- max(ref$dep)

    setindexv(ref, "dep")

    while (depth <= max_depth) {
        ref_at_depth <- ref[J(depth), on = "dep", nomatch = 0L]

        if (!nrow(ref_at_depth)) break

        if (depth == 0L) {
            src_val <- value[J(ref_at_depth$src_value_id), on = "value_id",
                .SD, .SDcols = c("value_chr", "class_name")
            ]
            src_val[ref_at_depth$src_enum == 1L, value_chr := class_name]

            set(ref_at_depth, NULL, "src_value_chr", src_val$value_chr)
        } else {
            set(ref_at_depth, NULL, "src_value_chr",
                idf_env$value[J(ref_at_depth$src_value_id), .SD, .SDcols = "value_chr"]
            )
        }

        idf_env$value[ref_at_depth, on = "value_id",
            `:=`(value_chr = ref_at_depth$src_value_chr, value_num = NA_real_)
        ]

        depth <- depth + 1L
    }

    idf_env
}
# }}}

# IDF Editor Integration
# read_idfeditor_copy {{{
read_idfeditor_copy <- function (version = NULL, in_ip = FALSE) {
    if (!is_windows()) {
        abort("error_not_on_windows", "Currently $paste() can only work on Windows platform.")
    }

    text <- readLines("clipboard", warn = FALSE)

    if (!stringi::stri_startswith_fixed(text, "IDF,")) {
        abort("error_clipboard_string", "Failed to find contents copied from IDF Editor.")
    }
    text <- gsub("([,;])", "\\1\n", stri_sub(text, 5L))

    if (isTRUE(in_ip)) {
        text <- paste0("!-Option SortedOrder ViewInIPunits\n", text)
    }

    # ignore the warning of using given IDD
    withCallingHandlers(parse_idf_file(text, idd = version, ref = FALSE),
        warn_given_idd_used = function (w) invokeRestart("muffleWarning")
    )
}
# }}}

# TABLE
# get_idf_table {{{
get_idf_table <- function (idd_env, idf_env, class = NULL, object = NULL, string_value = TRUE, unit = TRUE) {
    cols <- c("object_id", "object_name", "class_name",
              "field_index", "field_name", "units", "ip_units", "type_enum",
              "value_chr", "value_num")

    val <- get_idf_value(idd_env, idf_env, class = class, object = object,
        property = c("units", "ip_units", "type_enum"))[, .SD, .SDcols = cols]

    setnames(val,
        c("object_id", "object_name", "class_name", "field_index", "field_name"),
        c("id", "name", "class", "index", "field"))

    if (string_value) {
        setnames(val, "value_chr", "value")
        val[, .SD, .SDcols = c("id", "name", "class", "index", "field", "value")]
    } else {
        lst <- get_value_list(val, unit = unit)
        if (nrow(val) == 1L) {
            set(val, NULL, "value", list(lst))
        } else {
            set(val, NULL, "value", lst)
        }

        val[, .SD, .SDcols = c("id", "name", "class", "index", "field", "value")]
    }
}
# }}}

# STRING
# get_idf_string {{{
get_idf_string <- function (idd_env, idf_env, dt_order = NULL, class = NULL, object = NULL,
                            in_ip = FALSE, comment = TRUE, header = TRUE,
                            format = c("sorted", "new_top", "new_bot"),
                            leading = 4L, sep_at = 29L) {
    format <- match.arg(format)

    if (any(!is.null(class), !is.null(object))) {
        obj <- get_idf_object(idd_env, idf_env, class, object, ignore_case = TRUE)
        fmt <- with_nocolor(with_format_cols(idd_env, idf_env,
            format_idf(
                idf_env$value[J(obj$object_id), on = "object_id"],
                idf_env$object[J(obj$object_id), on = "object_id"],
                dt_order, in_ip = in_ip, header = header, comment = comment,
                save_format = format
            )
        ))
    } else {
        fmt <- with_nocolor(with_format_cols(idd_env, idf_env,
            format_idf(idf_env$value, idf_env$object, dt_order, in_ip = in_ip,
            header = header, comment = comment, save_format = format)
        ))
    }

    if (format == "sorted") {
        combine_fmt <- function (lst) {
            head <- if (is.null(lst[[1L]])) "" else c("", lst[[1L]], "")
            c(head, unlist(lapply(lst[-1L], function (x) c(unlist(x, use.names = FALSE), "")), use.names = FALSE))
        }

        body <- unlist(lapply(fmt$format$fmt, combine_fmt), use.names = FALSE)
    } else {
        combine_fmt <- function (lst) c(unlist(lst, use.names = FALSE), "")

        body <- unlist(lapply(fmt$format$fmt, combine_fmt), use.names = FALSE)
    }

    if (header) {
        c(fmt$header, "", body)
    } else if (format == "sorted") {
        body[-1L]
    } else {
        body
    }
}
# }}}

# SAVE
# save_idf {{{
save_idf <- function (idd_env, idf_env, dt_order = NULL, path, in_ip = FALSE,
                      format = c("sorted", "new_top", "new_bot"),
                      overwrite = FALSE, copy_external = TRUE, oldpath = path) {
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

    resolve_idf_external_link(idd_env, idf_env, oldpath, path, copy_external)

    str <- get_idf_string(idd_env, idf_env, dt_order, comment = TRUE, header = TRUE, format = format)

    path <- normalizePath(path, mustWork = FALSE)
    write_lines(str, path)

    path
}
# }}}
# resolve_idf_external_link {{{
#  auto change full file path in `Schedule:File` to relative path and copy those
#  files into the same directory of the model
resolve_idf_external_link <- function (idd_env, idf_env, old, new, copy = TRUE) {
    if (!has_name(idf_env$object, "class_name")) {
        added <- TRUE
        add_class_name(idd_env, idf_env$object)
        on.exit(set(idf_env$object, NULL, "class_name", NULL), add = TRUE)
    }

    # Currently, only `Schedule:File` class is supported
    if (!"Schedule:File" %in% idf_env$object$class_name) return(FALSE)

    # restore current working directory
    ori <- getwd()
    on.exit(setwd(ori), add = TRUE)

    # get full path of old and new
    old_dir <- normalizePath(dirname(old), mustWork = FALSE)
    new_dir <- normalizePath(dirname(new), mustWork = FALSE)

    # get object table and value table
    val <- get_idf_value(idd_env, idf_env, class = "Schedule:File", field = "File Name")

    # check existence of old files
    setwd(old_dir)
    set(val, NULL, "old_full_path", normalizePath(val$value_chr, mustWork = FALSE))
    set(val, NULL, "old_exist", file.exists(val$old_full_path))

    # stop if old file does not exist
    if (nrow(val[old_exist == FALSE])) {
        on.exit(options(warning.length = getOption("warning.length")), add = TRUE)
        options(warning.length = 8170)

        m <- paste0(format_objects(val, "field"), collapse = "\n")

        warn("warning_broken_file_link",
            paste0("Broken external file link found in IDF:\n\n", m)
        )
    }

    set(val, NULL, "same_dir", normalizePath(dirname(dir$old_full_path)) == new_dir)

    # find files to copy
    val <- val[old_exist == TRUE & same_dir == FALSE]

    if (!nrow(val)) return(FALSE)

    # copy external files and change values to relative paths
    if (!copy) {
        set(val, NULL, "new_value", val$old_full_path)
    # change all paths to full paths
    } else {
        set(val, NULL, "file_name", basename(val$value_chr))
        set(val, NULL, "new_full_path", normalizePath(file.path(val$new_dir, val$file_name), mustWork = FALSE))
        set(val, NULL, "new_value", val$file_name)

        # copy files
        set(val, NULL, "copied", file.copy(val$old_full_path, val$new_full_path, overwrite = TRUE, copy.date = TRUE))
        if (val[copied == FALSE, .N]) {
            on.exit(options(warning.length = getOption("warning.length")), add = TRUE)
            options(warning.length = 8170)

            m <- paste0(format_objects(val[copied == FALSE], "field"), collapse = "\n")

            abort("error_failed_to_copy",
                paste0("Failed to copy external file into the output directory ",
                    surround(new_dir), ":\n", m, collapse = "\n"
                )
            )
        }
    }

    # update object value table
    idf_env$value[J(val$value_id), on = "value_id", value_chr := val$new_value]

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
    set(dt_value, NULL, "object_id",
        rep(dt_object$object_id, times = dt_value[, .N, by = "object_id"]$N)
    )
}
# }}}
# assign_default_value {{{
assign_default_value <- function (dt_value) {
    if (in_ip_mode()) {
        dt_value <- field_default_to_unit(dt_value, "si", "ip")
    }
    dt_value[defaulted == TRUE, `:=`(value_chr = default_chr, value_num = default_num)]
    dt_value
}
# }}}
# merge_idf_data {{{
merge_idf_data <- function (idf_env, dt, by_object = FALSE) {
    assert(is.environment(idf_env))
    assert(has_name(dt, c("object", "value", "reference")))

    idf_env$object <- append_dt(idf_env$object, dt$object, "object_id")
    if (by_object) {
        idf_env$value <- append_dt(idf_env$value, dt$value, "object_id")
    } else {
        idf_env$value <- append_dt(idf_env$value, dt$value, "value_id")
    }
    idf_env$reference <- dt$reference

    setorderv(idf_env$object, c("object_id"))
    setorderv(idf_env$value, c("object_id", "field_id"))

    idf_env
}
# }}}
# add_idf_format_cols {{{
add_idf_format_cols <- function (idd_env, idf_env) {
    # add class_id, class_name and field_index columns
    add_joined_cols(idf_env$object, idf_env$value, "object_id", "class_id")
    add_joined_cols(idd_env$class, idf_env$value, "class_id", "class_name")
    add_joined_cols(idd_env$field, idf_env$value, "field_id", c("field_index", "units", "ip_units", "field_name"))

    idf_env
}
# }}}
# del_idf_format_cols {{{
del_idf_format_cols <- function (idd_env, idf_env) {
    # del class_id, class_name and field_index columns
    set(idf_env$value, NULL,
        c("class_id", "class_name", "field_index", "field_name", "units", "ip_units"),
        NULL
    )

    idf_env
}
# }}}
# with_format_cols {{{
with_format_cols <- function (idd_env, idf_env, ...) {
    add_idf_format_cols(idd_env, idf_env)
    on.exit(del_idf_format_cols(idd_env, idf_env), add = TRUE)
    force(...)
}
# }}}
