#' @importFrom cli cat_bullet cat_line cat_rule rule symbol
#' @importFrom checkmate assert assert_names assert_string assert_data_frame
#' @importFrom checkmate assert_count assert_character assert_subset
#' @importFrom checkmate test_integerish check_integerish test_character
#' @importFrom checkmate qassert qassertr qtestr
#' @importFrom data.table copy data.table dcast rbindlist transpose
#' @importFrom data.table setattr setcolorder setnames setorder setorderv
#' @importFrom stringi stri_locate_first_regex stri_replace_first_regex "stri_sub<-"
#' @importFrom stringi stri_subset_regex stri_match_first_regex stri_rand_strings
#' @include impl-idd.R
NULL

# OBJECT
# get_idf_object {{{
#' Get object data
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#' @param class An integer vector of valid class indexes or a character vector
#'        of valid class names. Default: `NULL`.
#' @param object An integer vector of valid object IDs or a character vector
#'        of valid object names. Default: `NULL`.
#' @param property A character vector of column names in class table to return.
#' @param underscore If `TRUE`, input class name will be converted into
#'        underscore style name first and column `class_name_us` will be used
#'        for matching. Default: `FALSE`.
#' @param ignore_case If `TRUE`, input object name will be converted into lower
#'        case and column `object_name_lower` will be used for matching.
#'        converted into underscore style name first and column `class_name_us`
#'        and `field_name_us` will be used for matching. Default: `FALSE`.
#'
#' @return A data.table.
#'
#' @keywords internal
#' @export
get_idf_object <- function (idd_env, idf_env, class = NULL, object = NULL, property = NULL,
                            underscore = FALSE, ignore_case = FALSE) {
    # if no object is specified
    if (is.null(object)) {
        # if no class is specified
        if (is.null(class)) {
            obj <- add_class_name(idd_env, copy(idf_env$object))
            if (!is.null(property)) {
                obj <- add_class_property(idd_env, obj, property)
            }
            # add rleid
            add_rleid(obj)
        # if class is specified
        } else {
            cls_in <- recognize_input(class, "class", underscore)
            col_on <- names(cls_in)[[1L]]
            col_key <- col_on
            if (col_on == "class_id") {
                cls_in <- add_class_name(idd_env, cls_in)
                set(cls_in, NULL, "original", NULL)
            } else {
                # get class id
                cls_in <- join_from_input(
                    idd_env$class[, .SD, .SDcols = c("group_id", "class_id", unique(c("class_name", col_on)))],
                    cls_in, "group_id"
                )
                set(cls_in, NULL, "group_id", NULL)
                col_on <- "class_id"
            }

            col_add <- setdiff(names(idf_env$object), names(cls_in))

            obj <- idf_env$object[, .SD, .SDcols = c(col_on, col_add)][, check := .I][cls_in, on = col_on]
            check_bad_key(obj, "check", col_key)
            set(obj, NULL, "check", NULL)

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
            set(obj_in, NULL, "original", NULL)
            cls_in <- recognize_input(class, "class", underscore)
            if (names(cls_in)[[1L]] == "class_id") {
                cls_in <- add_class_name(idd_env, cls_in)
                set(cls_in, NULL, "original", NULL)
            } else {
                # get class id
                cls_in <- join_from_input(
                    idd_env$class[, .SD, .SDcols = c("group_id", "class_id", unique(c("class_name", names(cls_in)[[1L]])))],
                    cls_in, "group_id"
                )
                set(cls_in, NULL, "group_id", NULL)
            }

            # add property if necessary
            if (!is.null(property)) cls_in <- add_class_property(idd_env, cls_in, property)
            # delete rleid in class
            set(cls_in, NULL, "rleid", NULL)

            # if only one class is specified, recycle
            if (nrow(cls_in) == 1L) cls_in <- cls_in[rep(1L, nrow(obj_in))]
            assert_same_len(cls_in, obj_in, .var.name = "class and object")

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
        if (!test_same_len(obj, object)) {
            mult_rleid <- obj[, .N, by = rleid][N > 1L, rleid]
            mult <- obj[J(mult_rleid), on = "rleid"]

            set(mult, NULL, "object",
                get_object_info(mult, c("id", "class"), numbered = FALSE, prefix = "")
            )

            m <- mult[, list(m = paste("Name", surround(object_name[1L]), "matches", collapse(object, NULL))),
                by = c("rleid", "object_name_lower")][, m := paste0(" #", rpad(rleid), "| ",m)]$m

            abort(paste0("Input object name matched multiple results. Please use object ID instead:\n",
                paste0(m, collapse = "\n")), "multi_match_by_name")
        }
        # }}}
    }

    setcolorder(obj, c("rleid", "class_id", "class_name", "object_id", "object_name", "object_name_lower", "comment"))
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
        abort_bad_key(col_key, idd_env$class[cls_in, on = col_on][is.na(group_id), .SD, .SDcols = col_on][[col_on]])
    }

    if (col_on == "class_name") cls_in <- add_class_id(idd_env, cls_in)

    idf_env$object[cls_in, on = "class_id", allow.cartesian = TRUE][
        , .N, by = list(rleid, found = !is.na(object_id))][found == FALSE, N := 0L]$N
}
# }}}
# get_object_info {{{
#' Format object information string
#'
#' @param dt_object A [data.table::data.table()] of object data
#' @param component A character vector specifying what information to be
#'        formatted. Should be a subset of `"id"`, `"name"` and `"class"`.
#'        Defaults are all of them.
#' @param by_class If `TRUE`, multiple objects in the same class will be
#'        concatenated. Default: `FALSE`.
#' @param numbered If `TRUE`, a index number will be prepended. If `rleid`
#'        column exists in `dt_object`, its values will be used as the index
#'        numbers.
#' @param collapse A single string used to collapse the results into a single
#'        string. Default: `NULL`.
#' @param prefix A character vector used to add at the beginning of object
#'        information. Default: `NULL`.
#' @param name_prefix If `TRUE`, Default: `TRUE`.
#'
#' @return A character vector of the same length as the row number of input
#' `dt_object` if `collapse` is `NULL`. Otherwise a single string.
#'
#' @keywords internal
#' @export
get_object_info <- function (dt_object, component = c("id", "name", "class"),
                             by_class = FALSE, numbered = TRUE, collapse = NULL,
                             prefix = NULL, name_prefix = TRUE) {
    assert_subset(component, c("id", "name", "class"))

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
            mes <- dt_object[, paste(key_cls, surround(class_name))]
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
        if (has_names(dt_object, "rleid")) {
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
# init_idf_object {{{
#' Initialize object data
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#' @param class An integer vector of valid class indexes or a character vector
#'        of valid class names. Default: `NULL`.
#' @param property A character vector of column names in class table to return.
#' @param underscore If `TRUE`, input class name will be converted into
#'        underscore style name first and column `class_name_us` will be used
#'        for matching. Default: `FALSE`.
#' @param id If `TRUE`, new object IDs will be added in column `object_id` based
#'        on current existing objects found in `idf_env`. Default: `TRUE`.
#' @param name If `TRUE`, column `object_name` and `object_name_lower` will be
#'        filled using [make_idf_object_name()]. Default: `TRUE`.
#'
#' @return A [data.table::data.table()]
#'
#' @keywords internal
#' @export
init_idf_object <- function (idd_env, idf_env, class, property = NULL, underscore = FALSE, id = TRUE, name = TRUE) {
    obj <- get_idd_class(idd_env, class, underscore = underscore, property = property)
    set(obj, NULL, c("object_name", "object_name_lower", "comment"),
        list(NA_character_, NA_character_, list())
    )

    id <- if (id) {
        if (NROW(idf_env$object)) {
            obj$rleid + max(idf_env$object$object_id)
        } else {
            obj$rleid
        }
    } else NA_integer_

    set(obj, NULL, "object_id", id)

    if (name) {
        obj <- make_idf_object_name(idd_env, idf_env, obj, use_old = FALSE, keep_na = FALSE, include_ori = TRUE)
        set(obj, NULL, c("object_name", "object_name_lower"), NULL)
        setnames(obj, c("new_object_name", "new_object_name_lower"), c("object_name", "object_name_lower"))
    }

    setcolorder(obj,
        c(setdiff(names(obj), c("object_id", "object_name", "object_name_lower", "comment")),
          c("object_id", "object_name", "object_name_lower", "comment")
        )
    )
}
# }}}
# make_idf_object_name {{{
#' Initialize object data
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#' @param dt_object A [data.table::data.table()] containing object data.
#' @param use_old If `TRUE`, new object names are based on the original object
#'        names in column `object_name`. If `FALSE`, new object names are
#'        created based on the class name it belongs to. Default: `TRUE`.
#' @param prefix_col An character vector of column names in input `dt_object`
#'        whose values will be combined together as the prefix of the new object
#'        names. Default: `NULL`.
#' @param prefix_sep A single string specifying the separation character among
#'        prefix columns. Default: `NULL`.
#' @param keep_na If `TRUE`, new object names will be `NA` if the original
#'        object names in column `object_name` are `NA`s. Default: `TRUE`.
#' @param include_ori If `TRUE`, make sure new object names are not the same as
#'        the original object names in the `object_name` column. Default: `FALSE`.
#'
#' @return A [data.table::data.table()]
#'
#' @keywords internal
#' @export
make_idf_object_name <- function (idd_env, idf_env, dt_object, use_old = TRUE,
                                  prefix_col = NULL, prefix_sep = " ",
                                  keep_na = TRUE, include_ori = FALSE) {
    add_hasname <- FALSE
    if (!has_names(dt_object, "has_name")) {
        add_class_property(idd_env, dt_object, "has_name")
        add_hasname <- TRUE
    }

    if (!has_names(dt_object, "new_object_name")) {
        set(dt_object, NULL, "new_object_name", NA_character_)
    }

    if (!has_names(dt_object, "new_object_name_lower")) {
        set(dt_object, NULL, "new_object_name_lower", stri_trans_tolower(dt_object[["new_object_name"]]))
    }

    # sep objects with/without name attr
    can_nm <- dt_object$has_name
    dt_obj_nm <- dt_object[which(can_nm)]
    dt_obj_no <- dt_object[which(!can_nm)]

    # stop if trying to assign names to objects that do not have name attribute
    if (any(!is.na(dt_obj_no$new_object_name))) {
        invld <- dt_obj_no[!is.na(new_object_name)]
        abort(paste0("Object in class that does not have name attribute cannot be renamed. Invalid input:\n",
            get_object_info(invld, numbered = TRUE, collapse = "\n")),
            "cannot_name")
    }

    # check duplications in new names
    if (any(invld <- duplicated(dt_obj_nm[!J(NA_character_), on = "new_object_name_lower"],
                by = c("class_id", "new_object_name_lower")))) {
        abort(paste0("Input new object names cannot contain duplications. Duplicated names:\n",
            paste0(dt_obj_nm[invld, sprintf(" #%s| '%s'", lpad(rleid, "0"), new_object_name)], collapse = "\n")),
            "duplicated_name")
    }

    dt_all <- fast_subset(idf_env$object, c("class_id", "object_id", "object_name", "object_name_lower"))

    # auto-generate object names if necessary
    autoname <- is.na(dt_obj_nm$new_object_name)
    dt_obj_nm_auto <- dt_obj_nm[which(autoname)]
    dt_obj_nm_input <- dt_obj_nm[which(!autoname)]

    # check if input new names are the same as existing ones
    if (nrow(invld <- dt_all[dt_obj_nm_input, on = c("class_id", object_name_lower = "new_object_name_lower"), nomatch = 0L])) {
        obj <- get_object_info(invld, numbered = FALSE)
        abort(paste0("Input new object names cannot be the same as existing object. Conflicting object names:\n",
            paste0(sprintf(" #%s| '%s' --> %s", lpad(invld$rleid), invld$new_object_name, obj), collapse = "\n")),
            "conflict_name")
    }

    # auto generate names and append integer suffix for auto-names if necessary
    if (nrow(dt_obj_nm_auto)) {
        # extract component names from class names
        if (!use_old) {
            set(dt_obj_nm_auto, NULL, "new_object_name", get_class_component_name(dt_obj_nm_auto$class_name))

            if (!is.null(prefix_col)) {
                dt_obj_nm_auto[, new_object_name := do.call(paste, c(.SD, sep = prefix_sep)), .SDcols = c(prefix_col, "new_object_name")]
            }
        # use the original object name if possible
        } else {
            dt_obj_nm_auto[, "new_object_name" := {
                # fill missing object name with extracted component name
                # from class name
                if (!keep_na) {
                    object_name[is.na(object_name)] <- get_class_component_name(class_name[is.na(object_name)])
                }
                object_name
            }]

            if (!is.null(prefix_col)) {
                if (keep_na) {
                    dt_obj_nm_auto[!J(NA_character_), on = "object_name",
                        new_object_name := do.call(paste, c(.SD, sep = prefix_sep)), .SDcols = c(prefix_col, "new_object_name")]
                } else {
                    dt_obj_nm_auto[, new_object_name := do.call(paste, c(.SD, sep = prefix_sep)), .SDcols = c(prefix_col, "new_object_name")]
                }
            }
        }

        set(dt_obj_nm_auto, NULL, "new_object_name_lower", stri_trans_tolower(dt_obj_nm_auto$new_object_name))

        # get all names in existing objects
        dt_nm <- dt_all[J(unique(dt_obj_nm_auto$class_id)), on = "class_id", nomatch = 0L][
            !J(NA_character_), on = "object_name", by = "class_id",
            list(all_name_lower = list(object_name_lower))
        ]

        if (nrow(dt_obj_nm_input)) {
            # NOTE:
            # (a) When include_ori is FALSE, old object name should be excluded while
            #     the new names should be included
            # (b) Otherwise, both old object names and new names should be
            #     considered

            # user specified new names and original names
            inclu <- dt_obj_nm_input[J(unique(dt_obj_nm_auto$class_id)), on = "class_id", nomatch = 0L,
                .SD, .SDcols = c("class_id", "object_name_lower", "new_object_name_lower")]

            if (include_ori) {
                dt_nm[inclu, on = "class_id", by = .EACHI, all_name_lower := {
                    list(list(c(all_name_lower[[1L]], i.new_object_name_lower)))
                }]
            } else {
                dt_nm[inclu, on = "class_id", by = .EACHI, all_name_lower := {
                    list(list(c(setdiff(all_name_lower[[1L]], i.object_name_lower), i.new_object_name_lower)))
                }]
            }
        }

        add_joined_cols(dt_nm, dt_obj_nm_auto, "class_id", "all_name_lower")

        dt_obj_nm_auto[!J(NA_character_), on = "new_object_name", by = c("class_id", "new_object_name_lower"),
            c("new_object_name", "new_object_name_lower") := {

                # check if trying to duplicate same object several times
                time <- seq_len(.N)

                # get the duplicated times before
                if (is.null(all_name_lower[[1L]])) {
                    num <- 0L
                } else {
                    num <- apply2_int(all_name_lower, new_object_name_lower,
                        function (all, new) {
                            same <- sum(all == new)
                            num <- stri_match_first_regex(all, paste0("^", new, " (\\d+)$"))[, 2L]
                            num[is.na(num)] <- "0"
                            max(as.integer(num)) + same
                        }
                    )
                }

                n <- time + num - 1L
                if (.N == 1L && n == 0L) {
                    list(new_object_name, new_object_name_lower)
                } else {
                    suffix <- character(.N)
                    suffix[n > 0L] <- paste("", n[n > 0L])
                    list(paste0(new_object_name, suffix),
                         paste0(new_object_name_lower, suffix)
                    )
                }
        }]

        set(dt_obj_nm_auto, NULL, "all_name_lower", NULL)
    }

    dt <- rbindlist(list(dt_obj_no, dt_obj_nm_input, dt_obj_nm_auto), use.names = TRUE)
    setorderv(dt, "rleid")
    if (add_hasname) set(dt, NULL, "has_name", NULL)
    setcolorder(dt, setdiff(names(dt), c("new_object_name", "new_object_name_lower")))
}
# }}}
# get_idf_object_multi_scope {{{
get_idf_object_multi_scope <- function (idd_env, idf_env, object = NULL, class = NULL, group = NULL) {
    obj <- data.table()

    if (is.null(object) && is.null(class) && is.null(group)) {
        return(setcolorder(get_idf_object(idd_env, idf_env),
            c("rleid", "class_id", "class_name", "object_id", "object_name", "object_name_lower", "comment")
        ))
    }

    if (!is.null(object)) {
        obj <- get_idf_object(idd_env, idf_env, object = object, ignore_case = TRUE)
    }
    if (!is.null(class)) {
        obj <- rbindlist(list(obj, get_idf_object(idd_env, idf_env, class)), use.names = TRUE)
    }
    if (!is.null(group)) {
        assert_valid_type(group, "Group Name", type = "name")

        add_class_property(idd_env, idf_env$object, "group_name")

        grp_in <- recognize_input(group, "group")
        obj_grp <- join_from_input(idf_env$object, grp_in, "object_id")

        # clean
        set(idf_env$object, NULL, "group_name", NULL)

        # add class name to make sure results have same columns as 'get_idf_object()'
        set(obj_grp, NULL, "group_name", NULL)
        add_class_property(idd_env, obj_grp, "class_name")

        obj <- rbindlist(list(obj, obj_grp), use.names = TRUE)
    }

    obj <- unique(obj, by = "object_id")

    # reset rleid
    add_rleid(obj)

    setcolorder(obj, c("rleid", "class_id", "class_name", "object_id", "object_name", "object_name_lower", "comment"))

    obj
}
# }}}

# VALUE
# get_idf_value {{{
#' Get value data
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#' @param class An integer vector of valid class indexes or a character vector
#'        of valid class names. Default: `NULL`.
#' @param object An integer vector of valid object IDs or a character vector
#'        of valid object names. Default: `NULL`.
#' @param field An integer vector of valid field indexes or a character
#'        vector of valid field names (can be in in underscore style). `class`
#'        and `field` should have the same length.
#' @param property A character vector of column names in field table to return.
#' @param underscore If `TRUE`, input class name will be converted into
#'        underscore style name first and column `class_name_us` will be used
#'        for matching. Default: `FALSE`.
#' @param ignore_case If `TRUE`, input object name will be converted into lower
#'        case and column `object_name_lower` will be used for matching.
#'        converted into underscore style name first and column `class_name_us`
#'        and `field_name_us` will be used for matching. Default: `FALSE`.
#' @param align If `TRUE`, all objects in the same class will have the same
#'        field number. The number of fields is the same as the object that have
#'        the most fields among objects specified.  Default: `FALSE`.
#' @param complete If `TRUE`, at least fields till the current whole extensible
#'        group will be returned. A new column named "matched_rleid" will be
#'        created (when `property` is NULL) indicating if given field has been
#'        matched or not.
#' @param all If `TRUE`, all available fields defined in IDD for the class that
#'        objects belong to will be returned. Default: `FALSE`.
#'
#' @return A data.table containing specified columns.
#' @keywords internal
#' @export
get_idf_value <- function (idd_env, idf_env, class = NULL, object = NULL, field = NULL,
                           property = NULL, underscore = FALSE, ignore_case = FALSE,
                           align = FALSE, complete = FALSE, all = FALSE) {
    obj <- get_idf_object(idd_env, idf_env, class, object, NULL, underscore, ignore_case)
    set(obj, NULL, c("object_name_lower", "comment"), NULL)

    # if field is not specified
    if (is.null(field)) {
        val <- idf_env$value[obj, on = "object_id", allow.cartesian = TRUE]

        # if just want to get the value, no special treatment is required
        if (!(all || complete || align)) {
            add_joined_cols(idd_env$field, val, "field_id", c("field_index", "field_name", property))
            setcolorder(val, c("rleid", "class_id", "class_name", "object_id",
                "object_name", "field_id", "field_index", "field_name", "value_id",
                "value_chr", "value_num"))
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
        if (is.null(class) && is.null(object)) {
            abort("When 'field' is specified, either 'class' or 'object' should also be specified",
                "missing_class_or_object"
            )
        }

        # as class name already exist in fld
        set(obj, NULL, "class_name", NULL)

        # if class or object is a scalar, then this means that field should be
        # applied to every target object
        if (length(class) == 1L || length(object) == 1L) {
            obj <- obj[,
                list(rleid = rep(rleid, length(field)),
                    object_name = rep(object_name, length(field)),
                    num = field
                ),
                by = c("class_id", "object_id")
            ]
        } else {
            if (!test_same_len(class, field) && !test_same_len(object, field)) {
                abort("'field' should have same length as 'class' or 'object'",
                    "invalid_field_length"
                )
            }
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
        set(val, NULL, c("class_id", "class_name", "object_name"), NULL)

        val <- val[fld, on = c("rleid", "object_id", "field_id")]
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

    val[J(NA_integer_), on = "value_id", value_id := -.I]
    setcolorder(val, c("rleid", "class_id", "class_name", "object_id",
        "object_name", "field_id", "field_index", "field_name", "value_id",
        "value_chr", "value_num"))

    val
}
# }}}
# init_idf_value {{{
#' Initialize value data
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#' @param class An integer vector of valid class indexes or a character vector
#'        of valid class names. Default: `NULL`.
#' @param object An integer vector of valid object IDs or a character vector
#'        of valid object names. Default: `NULL`.
#' @param field An integer vector of valid field indexes or a character
#'        vector of valid field names (can be in in underscore style). `class`
#'        and `field` should have the same length.
#' @param property A character vector of column names in field table to return.
#' @param underscore If `TRUE`, input class name will be converted into
#'        underscore style name first and column `class_name_us` will be used
#'        for matching. Default: `FALSE`.
#' @param complete If `TRUE`, at least fields till the current whole extensible
#'        group will be returned. A new column named "matched_rleid" will be
#'        created (when `property` is NULL) indicating if given field has been
#'        matched or not. Default: `FALSE`.
#' @param all If `TRUE`, all available fields defined in IDD for the class that
#'        objects belong to will be returned. Default: `FALSE`.
#' @param default If `TRUE`, column `value_chr` and `value_num` will be filled
#'        with default values. Default: `TRUE`.
#' @param id If `TRUE`, new value id will be added in column `value_id` based
#'        on current existing value ids found in `idf_env`. Default: `TRUE`.
#'
#' @note 'object_id' and 'object_name' are added as all `NA`s.
#'
#' @return A data.table containing specified columns.
#' @keywords internal
#' @export
init_idf_value <- function (idd_env, idf_env, class, field = NULL, property = NULL,
                            underscore = FALSE, complete = FALSE, all = FALSE, default = TRUE,
                            id = TRUE) {
    prop <- c("type_enum", "units", "ip_units", "default_chr", "default_num")
    # get empty object
    val <- get_idd_field(idd_env, class, field, underscore = underscore,
        complete = complete, all = all, property = unique(c(prop, property)))

    # get default value
    set(val, NULL, "defaulted", TRUE)
    val <- assign_idf_value_default(idd_env, idf_env, val)
    set(val, NULL, "defaulted", NULL)

    # clean
    if (!is.null(property)) prop <- setdiff(prop, property)
    if (length(prop)) set(val, NULL, prop, NULL)

    id <- if (id) {
        if (NROW(idf_env$value)) {
            seq_len(nrow(val)) + max(idf_env$value$value_id)
        } else {
            seq_len(nrow(val))
        }
    } else NA_integer_
    set(val, NULL, "value_id", id)

    set(val, NULL, c("object_id", "object_name"), list(NA_integer_, NA_character_))

    setcolorder(val, c("rleid", "class_id", "class_name",
            "object_id", "object_name",
            "field_id", "field_index", "field_name",
            "value_id", "value_chr", "value_num"))
}
# }}}
# standardize_idf_value {{{
#' Standardize Value Data
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#' @param dt_value An data.table of object field values.
#' @param type A character vector to specify what type of values to be
#'        standardized. Should be a subset of `c("choice", "reference")`.
#'        Default: `c("choice", "reference")`.
#' @param keep If `TRUE`, the original value will be kept even if it is invalid.
#'        If `FALSE`, invalid values will be converted into NAs. Default: `TRUE`.
#'
#' @return A data.table
#' @keywords internal
#' @export
standardize_idf_value <- function (idd_env, idf_env, dt_value, type = c("choice", "reference"), keep = TRUE) {
    type <- assert_subset(type, c("choice", "reference"), empty.ok = FALSE)

    prop <- "type_enum"
    if ("choice" %chin% type) prop <- c(prop, "choice")

    if (any(miss <- !prop %chin% names(dt_value))) {
        add_field_property(idd_env, dt_value, prop[miss])
    } else {
        prop <- NULL
    }

    if ("choice" %chin% type && any(i <- dt_value$type_enum == IDDFIELD_TYPE$choice)) {
        dt_value[i, value_chr := {
            i <- apply2_int(stri_trans_tolower(value_chr), lapply(choice, stri_trans_tolower), chmatch)
            value_chr[!is.na(i)] <- apply2_chr(choice[!is.na(i)], i[!is.na(i)], .subset2)
            if (!keep) value_chr[is.na(i)] <- NA_character_
            value_chr
        }]
    }

    if ("reference" %chin% type && any(i <- dt_value$type_enum == IDDFIELD_TYPE$object_list) && nrow(idf_env$reference)) {
        ref <- idf_env$reference[J(dt_value$value_id[i]), on = "value_id", nomatch = NULL]
        ref[idf_env$value, on = c("src_value_id" = "value_id"),
            `:=`(src_value_chr = i.value_chr, src_value_num = i.value_num)]
        if (keep) {
            dt_value[ref[!J(NA_character_), on = "src_value_chr"], on = "value_id",
                `:=`(value_chr = i.src_value_chr, value_num = i.src_value_num)]
        } else {
            dt_value[ref, on = "value_id", `:=`(value_chr = i.src_value_chr, value_num = i.src_value_num)]
        }
    }

    if (!is.null(prop)) set(dt_value, NULL, prop, NULL)

    dt_value
}
# }}}

# DOTS EXPANSION
# expand_idf_dots_name {{{
#' Parse object ID or name specifications given in list format
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#'
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#'
#' @param ... Lists of object ID or name pair, e.g. `c(Name1, Name2)`, `c(ID1,
#'        ID2)`, `NewName = OldName` and `NewName = ID`. `NewName` is optional.
#'
#' @param .keep_name If `TRUE`, input new names will be kept in a column named
#'        `new_object_name`, otherwise they will be dropped. Default: `TRUE`.
#'
#' @param .property A character vector of column names in class table to return.
#'        Default: `NULL`.
#'
#' @return A [data.table::data.table()] containing extracted object data.
#'
#' @keywords internal
#' @export
expand_idf_dots_name <- function (idd_env, idf_env, ..., .keep_name = TRUE, .property = NULL) {
    l <- list(...)

    # see https://github.com/mllg/checkmate/issues/146
    # For list, only NULL is treated as "missing" value
    assert_list(l, c("character", "integerish"), any.missing = FALSE, all.missing = FALSE, .var.name = "Input", min.len = 1L)
    qassertr(l, "V", "Input")

    is_nm <- vlapply(l, is.character, use.names = FALSE)
    rleid <- seq_along(l)
    # object name
    l_nm <- l[is_nm]
    rleid_nm <- rleid[is_nm]
    # object ID
    l_id <- l[!is_nm]
    rleid_id <- rleid[!is_nm]

    if (!length(l_nm)) {
        obj_nm <- data.table()
    } else {
        # in order to keep input order
        rleid_nm <- rep(rleid_nm, each_length(l_nm))

        if (.keep_name) {
            l_nm <- unlist(l_nm, FALSE, TRUE)
            nm_nm <- names2(l_nm)
        } else {
            l_nm <- unlist(l_nm, FALSE, FALSE)
        }

        obj_nm <- get_idf_object(idd_env, idf_env, object = l_nm, ignore_case = TRUE, property = .property)
        setnames(obj_nm, "rleid", "object_rleid")

        if (.keep_name) set(obj_nm, NULL, "new_object_name", stri_trim_both(nm_nm))
    }

    if (!length(l_id)) {
        obj_id <- data.table()
    } else {
        # in order to keep input order
        rleid_id <- rep(rleid_id, each_length(l_id))

        if (.keep_name) {
            l_id <- unlist(l_id, FALSE, TRUE)
            nm_id <- names2(l_id)
        } else {
            l_id <- unlist(l_id, FALSE, FALSE)
        }
        storage.mode(l_id) <- "integer"

        obj_id <- get_idf_object(idd_env, idf_env, object = l_id, property = .property)
        setnames(obj_id, "rleid", "object_rleid")

        if (.keep_name) set(obj_id, NULL, "new_object_name", stri_trim_both(nm_id))
    }

    # remain the input order
    obj <- rbindlist(list(obj_id, obj_nm))
    set(obj, NULL, "input_rleid", list(c(rleid_id, rleid_nm)))
    setorderv(obj, "input_rleid")
    set(obj, NULL, "rleid", rleid(obj$input_rleid, obj$object_rleid))
    set(obj, NULL, c("input_rleid", "object_rleid"), NULL)
    setcolorder(obj, "rleid")
    if (!is.null(.property)) setcolorder(obj, setdiff(names(obj), .property))
    obj
}
# }}}
# parse_dots_value {{{
#' @inherit expand_idf_dots_value
#' @export
parse_dots_value <- function (..., .scalar = TRUE, .pair = FALSE,
                              .ref_assign = TRUE, .unique = FALSE,
                              .empty = FALSE, .env = parent.frame()) {
    l <- eval(substitute(alist(...)))
    rules <- if (.scalar) "V1" else "V"

    assert_list(l, any.missing = FALSE, all.missing = FALSE, .var.name = "Input", min.len = 1L)

    nm <- name <- names2(l)
    ll <- vector("list", length(l))

    i <- j <- 1L
    # if is a variable, directly evaluate it
    while (i <= length(l)) {
        if (!is.symbol(l[[i]])) {
            ll[[j]] <- l[[i]]
            nm[[j]] <- name[[i]]
            i <- i + 1L
            j <- j + 1L
            next
        }

        val <- eval(l[[i]], .env)

        # in case 'x <- quote(cls := list()); parse_dots_value(x)'
        if (is.call(val)) {
            ll[[j]] <- val
            nm[[j]] <- name[[i]]
            i <- i + 1L
            j <- j + 1L
            next
        }

        # only one level:
        # 'x <- list(Name = "name"); parse_dots_value(obj = x)'
        if (qtestr(val, rules, depth = 1L)) {
            if (is.na(name[[i]])) {
                abort("Assertion on 'Input' failed: Must be named.", "dots_no_name")
            }

            ll[[j]] <- val
            nm[[j]] <- name[[i]]
            i <- i + 1L
            j <- j + 1L
        # in case 'x <- list(Name = list()); parse_dots_value(x)'
        } else {
            assert_list(val, "list", .var.name = "Input", names = if (.unique) "unique" else "named")

            len <- length(val)
            ll[j:(j+len-1L)] <- val
            nm[j:(j+len-1L)] <- names2(val)
            j <- j + len
            i <- i + 1L
        }
    }

    dt_in <- data.table(
        rleid = seq_along(ll),
        id = list(NA_integer_), name = as.list(nm), comment = list(),
        field_index = list(NA_integer_), field_name = list(NA_character_),
        value_chr = list(NA_character_), value_num = list(NA_real_),
        is_empty = FALSE, is_ref = FALSE, lhs_sgl = FALSE, rhs_sgl = TRUE, is_id = FALSE
    )

    i <- 1L
    for (i in dt_in$rleid) {
        li <- ll[[i]]
        # symbol has already evaluated in previous step
        if (!is.call(li)) {
            evaluated <- TRUE
            # stop if not named
            if (is.na(dt_in$name[[i]])) {
                abort("Assertion on 'Input' failed: Must be named.", "dots_no_name")
            }
            val <- li

            # for '..ID = list()'
            name <- nm[[i]]
            if (stri_detect_regex(name, "^\\.\\.\\d+$")) {
                id <- stri_sub(name, 3L)
                storage.mode(id) <- "integer"
                set(dt_in, i, "id", list(id))
                set(dt_in, i, "name", list(NA_character_))
            }
        # if `-`, `{`, `(` and other special function calls, len will be 2
        } else {
            evaluated <- FALSE
            # 'Name = list()', '..ID = list()'
            if (li[[1L]] == "list") {
                # stop if not named
                if (is.na(nm[[i]])) {
                    abort("Assertion on 'Input' failed: Must be named.", "dots_no_name")
                }

                # for '..ID = list()'
                name <- nm[[i]]
                if (stri_detect_regex(name, "^\\.\\.\\d+$")) {
                    id <- stri_sub(name, 3L)
                    storage.mode(id) <- "integer"
                    set(dt_in, i, "id", list(id))
                    set(dt_in, i, "name", list(NA_character_))
                # for 'Name = list()'
                } else {
                    set(dt_in, i, "name", list(name))
                }
            } else if (li[[1L]] == ":=") {
                if (!.ref_assign) abort("Assertion on 'Input' failed: ':=' is not allowed in this context", "dots_ref")
                # for 'ClassName := list()'
                if (length(li[[2L]]) == 1L) {
                    set(dt_in, i, "name", list(as.character(li[[2L]])))
                    # indicate that LHS is a single name
                    set(dt_in, i, "lhs_sgl", TRUE)
                # for 'c(Obj, Obj) := list()'
                } else if (as.character(li[[2L]][[1L]]) %chin% c("c", ".")) {
                    li[[2L]][[1L]] <- as.name("c")
                    name <- eval(li[[2L]], .env)
                    name <- assert_valid_type(name, "ID | Name | Index")
                    if (is.character(name)) {
                        set(dt_in, i, "name", list(name))
                        set(dt_in, i, "id", list(rep(NA_integer_, length(name))))
                    } else {
                        set(dt_in, i, "id", list(name))
                        set(dt_in, i, "name", list(rep(NA_character_, length(name))))
                    }
                # for '..(Cls) := list()'
                } else if (as.character(li[[2L]][[1L]]) == "..") {
                    li[[2L]][[1L]] <- as.name("c")
                    name <- eval(li[[2L]], .env)
                    name <- assert_valid_type(name, "Name", len = 1L, type = "name")
                    set(dt_in, i, "name", list(name))
                    # indicate that LHS is a single name
                    set(dt_in, i, "lhs_sgl", TRUE)
                } else {
                    abort("Assertion on 'Input' failed: LHS of ':=' must start with '.()', 'c()', or '..()'", "dots_ref_lhs")
                }

                li <- li[[3L]]
                set(dt_in, i, "is_ref", TRUE)
            }
        }

        if (!evaluated) val <- eval(li, .env)
        assert_list(val, c("character", "integer", "double", "null"), .var.name = "Input",
            all.missing = .empty
        )

        # check if empty list: 'list()'
        if (identical(val, list())) {
            set(dt_in, i, "is_empty", TRUE)
            next
        }

        fld_nm <- names(val)
        if (is.null(fld_nm)) {
            fld_nm <- character(length(val))
        } else {
            assert_character(fld_nm[!stri_isempty(fld_nm)], unique = TRUE, .var.name = "Field Name")
        }

        # handle '.comment'
        iscmt <- which(fld_nm == ".comment")
        if (length(iscmt)) {
            set(dt_in, i, "comment", list(val[iscmt]))

            val <- val[-iscmt]
            fld_nm <- fld_nm[-iscmt]

            # check if .comment only
            if (identical(unname(val), list())) {
                set(dt_in, i, "is_empty", TRUE)
                next
            }
        }

        fld_idx <- rep(NA_integer_, length(fld_nm))

        # check if ".." notation
        isidx <- stri_detect_regex(fld_nm, "^\\.\\.\\d+$")
        fld_idx[isidx] <- {id <- stri_sub(fld_nm[isidx], 3L);storage.mode(id) <- "integer";id}
        fld_nm[stri_isempty(fld_nm)] <- NA_character_
        fld_nm[isidx] <- NA_character_

        set(dt_in, i, "field_name", list(fld_nm))
        set(dt_in, i, "field_index", list(fld_idx))

        # check if NULL
        isnull <- vlapply(val, is.null)

        # make sure no NA and scalar if necessary
        qassertr(val[!isnull], rules, .var.name = "Field Value")

        # separate character and numeric value
        if (.scalar) {
            val[isnull] <- list(NA_character_)
            isnum <- vlapply(val, is.numeric)

            val_chr <- stri_trim_both(unlist(val, FALSE, FALSE))
            val_chr[stri_isempty(val_chr)] <- NA_character_
            val_num <- rep(NA_real_, length(val_chr))
            val_num[isnum] <- unlist(val[isnum], FALSE, FALSE)

            set(dt_in, i, "value_chr", val_chr)
            set(dt_in, i, "value_num", val_num)

        } else if (!.pair) {
            val[isnull] <- list(NA_character_)
            isnum <- vlapply(val, is.numeric)

            len <- each_length(val)

            # indicate if vector value input
            if (any(len > 1L)) set(dt_in, i, "rhs_sgl", FALSE)

            val_chr <- lapply(val, function (x) {x <- stri_trim_both(x); x[stri_isempty(x)] <- NA_character_;x})
            val_num <- lapply(len, function (n) rep(NA_real_, n))
            val_num[isnum] <- lapply(val[isnum], as.double)

            set(dt_in, i, "value_chr", list(list(val_chr)))
            set(dt_in, i, "value_num", list(list(val_num)))

        # make sure id/name are paired with field values
        } else {
            len_obj <- length(.subset2(dt_in$name, i))
            len_val <- each_length(val)
            len <- max(len_obj, len_val)

            # indicate if vector value input
            if (any(len_val > 1L)) set(dt_in, i, "rhs_sgl", FALSE)

            # check the length of objects
            if (len_obj != len) {
                if (len_obj != 1L) {
                    abort(paste0("Assertion on 'Field Value' failed, element ",
                        i, " at position ", which(len_val > 1L & len_obj != len)[[1L]], ": ",
                        "Length of field value {", len_val[len_val > 1L & len_obj != len_val][[1L]], "} ",
                        "must be the same as the ", "length of ID/Name {", len_obj, "}."),
                        "dots_pair_length"
                    )
                # 'Object = list(Fld = c(Val1, Val2))'
                } else if (!.subset2(dt_in$is_ref, i)) {
                    abort(paste0("Assertion on 'Field Value' failed, element ",
                        i, " at position ", which(len_val > 1L)[[1L]], ". ",
                        "Must be of length == 1, but has length ", len_val[len_val > 1L][[1L]], "."),
                        "dots_pair_length"
                    )
                }
                set(dt_in, i, "id", list(rep(dt_in$id[[i]], len)))
                set(dt_in, i, "name", list(rep(dt_in$name[[i]], len)))
            }

            # check the length of values
            val_lst <- apply2(val, len_val, function (v, l) {
                if (is.null(v)) {
                    chr <- rep(NA_character_, len)
                    num <- rep(NA_real_, len)
                } else if (l == 1L) {
                    if (is.character(v)) {
                        v <- stri_trim_both(v)
                        v[stri_isempty(v)] <- NA_character_
                        chr <- rep(v, len)
                        num <- rep(NA_real_, len)
                    } else {
                        chr <- rep(as.character(v), len)
                        num <- rep(as.double(v), len)
                    }
                } else if (l == len) {
                    if (is.character(v)) {
                        v <- stri_trim_both(v)
                        v[stri_isempty(v)] <- NA_character_
                        chr <- v
                        num <- rep(NA_real_, len)
                    } else {
                        chr <- as.character(v)
                        num <- as.double(v)
                    }
                } else {
                    abort(paste0("Assertion on 'Field Value' failed, element ",
                        i, " at position ", which(len_val == l)[1L], ": ",
                        "Length of field value {", l, "} ",
                        "must be the same as the ", "length of ID/Name {", len, "}."),
                        "dots_pair_length"
                    )
                }
                list(chr = chr, num = num)
            })

            # only one field
            if (length(len_val) == 1L) {
                set(dt_in, i, "value_chr", list(list(as.list(val_lst[[1L]]$chr))))
                set(dt_in, i, "value_num", list(list(as.list(val_lst[[1L]]$num))))
            } else {
                set(dt_in, i, "value_chr", list(list(transpose(lapply(val_lst, .subset2, "chr")))))
                set(dt_in, i, "value_num", list(list(transpose(lapply(val_lst, .subset2, "num")))))
            }
        }
    }

    # make sure id/name is unique
    if (.unique) {
        id <- viapply(dt_in$id, .subset2, 1L)
        assert_integer(id[!is.na(id)], unique = TRUE, .var.name = "Input ID")

        nm <- vcapply(dt_in$name, .subset2, 1L)
        assert_character(nm[!is.na(nm)], unique = TRUE, .var.name = "Input Name")
    }

    len_obj <- each_length(dt_in$id)
    len_fld <- each_length(dt_in$field_index)
    rep_each <- function (x, len) rep(x, each = len)

    obj <- dt_in[, list(
        rleid = rep(rleid, len_obj),
        each_rleid = unlist(lapply(len_obj, seq_len), FALSE, FALSE),
        id = unlist(id, FALSE, FALSE),
        name = unlist(name, FALSE, FALSE),
        comment = rep(comment, len_obj),
        is_ref = rep(is_ref, len_obj),
        lhs_sgl = rep(lhs_sgl, len_obj),
        rhs_sgl = rep(rhs_sgl, len_obj),
        is_empty = rep(is_empty, len_obj)
    )]

    each_rleid <- unlist(apply2(len_obj, len_fld, function (lo, lf) rep_each(seq_len(lo), lf)), FALSE, FALSE)
    if (.scalar) {
        val <- dt_in[, list(
            rleid = unlist(rep(rleid, len_obj * len_fld), FALSE, FALSE),
            each_rleid = each_rleid,
            id = unlist(apply2(id, len_fld, rep_each), FALSE, FALSE),
            name = unlist(apply2(name, len_fld, rep_each), FALSE, FALSE),
            field_index = unlist(rep(field_index, len_obj), FALSE, FALSE),
            field_name = unlist(rep(field_name, len_obj), FALSE, FALSE),
            value_chr = unlist(rep(value_chr, len_obj), FALSE, FALSE),
            value_num = unlist(rep(value_num, len_obj), FALSE, FALSE)
        )]
    } else if (!.pair) {
        # Should treat one-row input specially. Otherwise, vector field value input
        # will be unlisted
        if (length(len_fld) == 1L && len_fld == 1L) {
            val <- dt_in[, list(
                rleid = unlist(rep(rleid, len_obj * len_fld), FALSE, FALSE),
                each_rleid = each_rleid,
                id = unlist(apply2(id, len_fld, rep_each), FALSE, FALSE),
                name = unlist(apply2(name, len_fld, rep_each), FALSE, FALSE),
                field_index = unlist(rep(field_index, len_obj), FALSE, FALSE),
                field_name = unlist(rep(field_name, len_obj), FALSE, FALSE),
                value_chr = rep(unlist(value_chr, FALSE, FALSE), len_obj),
                value_num = rep(unlist(value_num, FALSE, FALSE), len_obj)
            )]
        } else {
            val <- dt_in[, list(
                rleid = unlist(rep(rleid, len_obj * len_fld), FALSE, FALSE),
                each_rleid = each_rleid,
                id = unlist(apply2(id, len_fld, rep_each), FALSE, FALSE),
                name = unlist(apply2(name, len_fld, rep_each), FALSE, FALSE),
                field_index = unlist(rep(field_index, len_obj), FALSE, FALSE),
                field_name = unlist(rep(field_name, len_obj), FALSE, FALSE),
                value_chr = unlist(rep(value_chr, len_obj), FALSE, FALSE),
                value_num = unlist(rep(value_num, len_obj), FALSE, FALSE)
            )]
        }
    } else {
        val <- dt_in[, list(
            rleid = unlist(rep(rleid, len_obj * len_fld), FALSE, FALSE),
            each_rleid = each_rleid,
            id = unlist(apply2(id, len_fld, rep_each), FALSE, FALSE),
            name = unlist(apply2(name, len_fld, rep_each), FALSE, FALSE),
            field_index = unlist(rep(field_index, len_obj), FALSE, FALSE),
            field_name = unlist(rep(field_name, len_obj), FALSE, FALSE),
            value_chr = unlist(value_chr, TRUE, FALSE),
            value_num = unlist(value_num, TRUE, FALSE)
        )]
    }

    list(object = obj, value = val)
}
# }}}
# expand_idf_dots_value {{{
#' Parse object field values given in list format
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#'
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#'
#' @param ... Lists of object definitions. Each list should be named
#'        with a valid class/object id/name. ID should be denoted in style
#'        `..ID`. There is a special element `.comment` in each list, which will
#'        be used as new comments of the object. If `.ref_assign` is `TRUE`,
#'        `:=` can be used to group ids/names:
#'
#' * When `.type` equals `"class"`, LHS multiple class indices/names should be
#'   wrapped by `.()`, `c()`.
#' * When `.type` equals `"object"`, LHS multiple object ids/names should be
#'   wrapped by `.()` or `c()`. LHS **SINGLE** class name should be
#'   wrapped by `..()`.
#'
#' @param .type Should be either `"class"` or `"object"`. If `"class"`,
#'        id/name of each input will be treated as class index/name. If `"object"`,
#'        id/name of each input will be treated as object id/name.
#'
#' @param .complete If `TRUE`, make sure the returned field number meets the
#'        `\min-fields` requirement. Default: `TRUE`
#'
#' @param .all If `TRUE`, make sure the all possible fields are returned.
#'        Default: `FALSE`.
#'
#' @param .scalar If `TRUE`, make sure the value of each field in the object is a
#'        scalar value. If `FALSE`, `value_chr` and `value_num` column will be
#'        list type. Default: `TRUE`.
#'
#' @param .pair Only works when `.scalar` is `FALSE`. If `.pair` is `TRUE`,
#'        vector field values will be paired to each id/name on the LHS. In this
#'        case, `value_chr` and `value_num` will be character type and double
#'        type, respectively. When there is only one id/name on the LHS, it will
#'        be replicated to match the length of the value vector. Default: `FALSE`.
#'
#' @param .ref_assign If `TRUE`, allow using `:=` to gather multiple
#'        classes/objects on the LHS when defining the objects. Default: `TRUE`.
#'
#' @param .unique If `TRUE`, make sure there are no duplicated classes/objects in
#'        the input. Default: `FALSE`.
#'
#' @param .empty If `TRUE`, allow using an empty list, i.e. `list()` to define an
#'        object with all default values. Default: `TRUE`.
#'
#' @param .default If `TRUE`, all empty fields will be filled with default
#'        values if possible. Default: `TRUE`.
#'
#' @param .env An environment specifying the environment to evaluate the `...`.
#'        Default: [parent.frame()].
#'
#' @return A named list of 2 element `object` and `value` which is a
#' [data.table::data.table()] with object data and value data respectively.
#'
#' @keywords internal
#' @export
expand_idf_dots_value <- function (idd_env, idf_env, ...,
                                   .type = "class", .complete = TRUE, .all = FALSE,
                                   .scalar = TRUE, .pair = FALSE, .ref_assign = TRUE,
                                   .unique = TRUE, .empty = TRUE, .default = TRUE,
                                   .env = parent.frame()) {
    l <- parse_dots_value(...,
        .scalar = .scalar, .pair = .pair, .ref_assign = .ref_assign,
        .unique = .unique, .empty = .empty, .env = .env)

    .type <- match.arg(.type, c("class", "object"))
    # indicate if single field value
    .sgl <- .scalar || (!.scalar && .pair)

    obj <- l$object
    val <- l$value

    # add new objects in specified classes {{{
    if (.type == "class") {
        # update rleid
        set(obj, NULL, "rleid", rleid(obj$rleid, obj$each_rleid))
        set(val, NULL, "rleid", rleid(val$rleid, val$each_rleid))
        set(obj, NULL, "each_rleid", NULL)
        set(val, NULL, "each_rleid", NULL)

        setnames(obj, c("id", "name"), c("class_id", "class_name"))
        setnames(val, c("id", "name"), c("class_id", "class_name"))

        # verify class index and name
        if (!length(i <- which(!is.na(obj$class_name)))) {
            add_class_name(idd_env, obj)
            check_bad_key(obj, "class_name", "class_id")
        } else {
            set(obj, i, c("class_id", "class_name"),
                fast_subset(get_idd_class(idd_env, obj$class_name[i], underscore = TRUE),
                    c("class_id", "class_name")
                )
            )

            i <- setdiff(seq_len(nrow(obj)), i)

            set(obj, i, "class_name",
                get_idd_class(idd_env, obj$class_id[i], underscore = TRUE)$class_name
            )
        }

        # if unique, should compare after class name has been matched
        if (.unique && anyDuplicated(obj$class_id)) {
            abort(paste0("Assertion on 'Input' failed: Must have unique class names, but element ",
                which(duplicated(obj$class_id))[[1L]], " {", obj$class_name[duplicated(obj$class_id)][[1L]],
                "} is duplicated."),
                "dots_dup_name"
            )
        }

        add_joined_cols(obj, val, "rleid", c("class_id", "class_name"))

        # handle empty objects
        if (!.empty) {
            val_emp <- data.table()
        } else if (!length(i <- which(obj$is_empty))){
            val_emp <- data.table()
        } else {
            val_emp <- init_idf_value(idd_env, idf_env, obj$class_name[i],
                underscore = TRUE, complete = .complete, all = .all, default = .default,
                id = FALSE
            )
            # keep original rleid
            add_joined_cols(data.table(rleid = i, object_rleid = seq_along(i)),
                val_emp, c(rleid = "object_rleid"), "rleid"
            )
            val <- val[!J(i), on = "rleid"]
        }

        if (nrow(val)) {
            val <- match_idd_field(idd_env, val)

            # complete fields if necessary {{{
            if (.all || .complete) {
                # get maximum field index per object
                fld_in <- val[, list(class_id = class_id[[1L]], num = max(field_index)), by = "rleid"]

                # get all necessary fields
                fld_out <- get_idd_field(idd_env, class = fld_in$class_id, field = fld_in$num,
                    all = .all, complete = TRUE
                )
                set(fld_out, NULL, "field_in", NULL)

                # add a temp rleid for matching field data
                add_rleid(fld_in, "object")

                # restore the original rleid
                add_joined_cols(fld_in, fld_out, c("rleid" = "object_rleid"), "rleid")

                # match
                val <- fld_out[val, on = c("rleid", "field_index"), `:=`(
                    value_chr = i.value_chr, value_num = i.value_num
                )]
            }
            # }}}

            # assign default value if necessary
            # only possible for scalar field value
            if (.default && .sgl) {
                val <- assign_idf_value_default(idd_env, idf_env, val)
            }

            # complete id column
            set(val, NULL, c("object_id", "object_name", "value_id"), list(NA_integer_, NA_character_, NA_integer_))
        }

        # combine
        if (nrow(val_emp)) {
            if (nrow(val)) {
                val <- rbindlist(list(val, val_emp), use.names = TRUE)
                # keep input order
                setorderv(val, "rleid")
            } else {
                val <- val_emp
            }
        }

        # complete object table
        set(obj, NULL, c("is_ref", "lhs_sgl", "rhs_sgl", "is_empty"), NULL)
        set(obj, NULL, c("object_id", "object_name", "object_name_lower"), list(NA_integer_, NA_character_, NA_character_))

        # keep column order
        setcolorder(obj, c("rleid", "class_id", "class_name", "object_id", "object_name", "object_name_lower", "comment"))
        setcolorder(val, c("rleid", "class_id", "class_name", "object_id", "object_name",
            "field_id", "field_index", "field_name", "value_id", "value_chr", "value_num"))
    # }}}
    # modify existing objects {{{
    } else if (.type == "object") {
        # separate class input namd object input
        # for class input {{{
        if (!(.ref_assign && any(obj$lhs_sgl))) {
            cls_obj <- data.table()
            cls_val <- data.table()
            obj_val <- val
        } else {
            # separate class input and object input
            cls <- obj[lhs_sgl == TRUE]
            obj <- obj[lhs_sgl == FALSE]

            # match class name
            cls_in <- cls[, list(class_name = name[[1L]], is_empty = is_empty[[1L]], rhs_sgl = rhs_sgl[[1L]], num = .N), by = "rleid"]
            add_rleid(cls_in, "class")
            cls_obj <- get_idf_object(idd_env, idf_env, cls_in$class_name, underscore = TRUE)
            add_joined_cols(cls_in, cls_obj, c("rleid" = "class_rleid"), "rleid")
            set(cls_obj, NULL, "class_name_us", NULL)

            # update class id and class name
            add_joined_cols(
                cls_obj[, list(class_id = class_id[[1L]], class_name = class_name[[1L]], num = .N), by = "rleid"],
                cls_in,
                "rleid",
                c("class_id" = "class_id", "class_name" = "class_name", "obj_num" = "num")
            )

            # when paired, if multiple field values, the length of field value
            # vector should be the same as number of objects in that class
            if (!.scalar && .pair && nrow(invld <- cls_in[num > 1L & num != obj_num])) {
                abort(paste0("Assertion on 'Field Value' failed on element ",
                    invld$rleid[[1L]], ". When LHS of ':=' is a class name, ",
                    "the length of each field value vector {", invld$num[[1L]],
                    "} must be the same as number of objects in that class {", invld$obj_num[[1L]], "}. "),
                    "dots_pair_length"
                )
            }

            # separate class value input and object value input
            cls_val <- val[rleid %in% cls_in$rleid]
            obj_val <- val[!rleid %in% cls_in$rleid]

            # extract values for empty input
            # empty here means to extract all objects in that class
            if (!.empty) {
                cls_obj_emp <- data.table()
                cls_val_emp <- data.table()
            } else if (!any(cls_in$is_empty)) {
                cls_obj_emp <- data.table()
                cls_val_emp <- data.table()
            } else {
                cls_obj_emp <- cls_obj[rleid %in% cls_in$rleid[cls_in$is_empty]]
                cls_obj <- cls_obj[!rleid %in% cls_in$rleid[cls_in$is_empty]]

                cls_val_emp <- get_idf_value(idd_env, idf_env, cls_in$class_id[cls_in$is_empty],
                    complete = .complete, all = .all
                )
                set(cls_val_emp, NULL, "rleid", rep(cls_in$rleid[cls_in$is_empty], table(cls_val_emp$rleid)))

                # assign default value if necessary
                if (.sgl) {
                    if (.default) {
                        cls_val_emp <- assign_idf_value_default(idd_env, idf_env, cls_val_emp)
                    }
                } else {
                    set(cls_val_emp, NULL, c("value_chr", "value_num"),
                        list(as.list(cls_val_emp$value_chr), as.list(cls_val_emp$value_num))
                    )
                }

                # exclude empty value input
                cls_val <- cls_val[!rleid %in% cls_in$rleid[cls_in$is_empty]]
            }

            if (!nrow(cls_val)) {
                cls_obj <- data.table()
                cls_val <- data.table()
            } else {
                # update id and name column
                add_joined_cols(cls_in, cls_val, "rleid", c("class_id", "class_name"))

                # match field id
                cls_sgl <- cls_val[, by = "rleid", list(each_rleid = max(each_rleid), n = length(unique(each_rleid)))]
                cls_sgl <- match_idd_field(idd_env, cls_val[cls_sgl, on = c("rleid", "each_rleid")])

                cls_sgl <- cls_sgl[, by = "rleid",
                    list(field_id = rep(field_id, n[[1L]]),
                         field_index = rep(field_index, n[[1L]]),
                         field_name = rep(field_name, n[[1L]])
                    )
                ]

                set(cls_val, NULL, "each_rleid", NULL)

                set(cls_val, NULL, c("field_id", "field_index", "field_name"),
                    set(cls_sgl, NULL, "rleid", NULL)
                )

                # assign default value if necessary
                if (.default && .sgl) {
                    cls_val <- assign_idf_value_default(idd_env, idf_env, cls_val)
                }

                # add object number
                add_joined_cols(cls_in, cls_val, "rleid", "obj_num")

                # get object id
                obj_id <- unlist(apply2(
                    split(cls_obj$object_id, cls_obj$rleid),
                    cls_val[, by = "rleid", data.table::uniqueN(field_id)]$V1, rep
                ), FALSE, FALSE)

                add_joined_cols(cls_in, cls_val, "rleid", "rhs_sgl")

                # each field should be replicated by object number
                if (!.sgl) {
                    cls_val <- cls_val[, lapply(.SD, function (x) rep(x, obj_num)),
                        .SDcols = -c("obj_num", "rhs_sgl"), by = "rleid"]
                } else {
                    cls_val <- rbindlist(list(
                        # if Class := list(Fld1 = Val1, Fld2 = Val2), each field should
                        # be replicated by object number
                        cls_val[J(TRUE), on = "rhs_sgl", nomatch = 0L, lapply(.SD, function (x) rep(x, obj_num)),
                            .SDcols = -c("obj_num", "rhs_sgl"), by = "rleid"],

                        # if Class := list(Fld1 = c(Val1, Val2, Val3, ...)), no
                        # replication is needed
                        cls_val[J(FALSE), on = "rhs_sgl", nomatch = 0L, .SD, .SDcols = -c("obj_num", "rhs_sgl")]
                    ))
                }

                setnames(cls_val, "id", "object_id")
                setorderv(cls_val, c("rleid", "field_id"))
                set(cls_val, NULL, "object_id", obj_id)

                # complete fields if necessary
                if (.all || .complete) {
                    fld_in <- cls_val[, by = "rleid", list(class_id = class_id[[1L]], field_index = max(field_index))]
                    cls_val_out <- get_idf_value(idd_env, idf_env,
                        class = fld_in$class_id, field = fld_in$field_index,
                        complete = .complete, all = .all
                    )
                    # add a temp rleid for matching field data
                    add_rleid(fld_in, "field")

                    # restore the original rleid
                    add_joined_cols(fld_in, cls_val_out, c("rleid" = "field_rleid"), "rleid")

                    # make the original value as a list if necessary
                    if (!.sgl) {
                        set(cls_val_out, NULL, c("value_chr", "value_num"),
                            list(as.list(cls_val_out$value_chr), as.list(cls_val_out$value_num))
                        )
                    }

                    # assign input value
                    cls_val_out[cls_val, on = c("rleid", "object_id", "field_index"),
                        `:=`(value_chr = i.value_chr, value_num = i.value_num)]
                    cls_val <- cls_val_out
                } else {
                    setnames(cls_val, "name", "object_name")
                    # add object name
                    add_joined_cols(idf_env$object, cls_val, "object_id", "object_name")
                    cls_val[idf_env$value, on = c("object_id", "field_id"), value_id := i.value_id]
                }
                setorderv(cls_val, c("rleid", "object_id", "field_id"))
            }

            # combine empty
            cls_obj <- rbindlist(list(cls_obj_emp, cls_obj), use.names = TRUE)
            cls_val <- rbindlist(list(cls_val_emp, cls_val), use.names = TRUE)
        }
        # }}}

        # for object input {{{
        if (!nrow(obj)) {
            obj <- data.table()
            obj_val <- data.table()
        } else {
            # columns not used
            set(obj, NULL, c("is_ref", "lhs_sgl", "rhs_sgl"), NULL)

            # add a new unique rleid
            set(obj, NULL, "each_rleid", rleid(obj$rleid, obj$each_rleid))
            set(obj_val, NULL, "each_rleid", rleid(obj_val$rleid, obj_val$each_rleid))

            setnames(obj, c("id", "name"), c("object_id", "object_name"))
            setnames(obj_val, c("id", "name"), c("object_id", "object_name"))

            # verify object id and name
            if (!length(i <- which(!is.na(obj$object_name)))) {
                set(obj, NULL, c("class_id", "class_name", "object_name", "object_name_lower"),
                    fast_subset(get_idf_object(idd_env, idf_env, object = obj$object_id),
                        c("class_id", "class_name", "object_name", "object_name_lower")
                    )
                )
            } else {
                set(obj, i, c("class_id", "class_name", "object_id", "object_name", "object_name_lower"),
                    fast_subset(get_idf_object(idd_env, idf_env, object = obj$object_name[i], ignore_case = TRUE),
                        c("class_id", "class_name", "object_id", "object_name", "object_name_lower")
                    )
                )

                i <- setdiff(seq_len(nrow(obj)), i)

                set(obj, i, c("class_id", "class_name", "object_name", "object_name_lower"),
                    fast_subset(get_idf_object(idd_env, idf_env, object = obj$object_id[i]),
                        c("class_id", "class_name", "object_name", "object_name_lower")
                    )
                )
            }

            # stop if trying to modifying same object multiple times
            if (.unique && anyDuplicated(c(cls_obj$object_id, obj$object_id))) {
                invld <- setorderv(rbindlist(list(cls_obj[, list(rleid, object_id)], obj[, list(rleid, object_id)]), use.names = TRUE),
                    "rleid")[duplicated(object_id)][1L]
                abort(paste0("Assertion on 'Object ID' failed: Contains duplicated values, position ",
                    invld$rleid, " {ID: ", invld$object_id, "}."),
                    "dots_dup_name"
                )
            }

            # extract values for empty input
            # empty here means to extract all objects in that class
            if (!.empty) {
                obj_emp <- data.table()
                val_emp <- data.table()
            } else if (!any(obj$is_empty)) {
                obj_emp <- data.table()
                val_emp <- data.table()
            } else {
                obj_emp <- obj[rleid %in% obj$rleid[obj$is_empty]]
                obj <- obj[!rleid %in% obj$rleid[obj$is_empty]]

                set(obj_emp, NULL, c("is_empty", "each_rleid"), NULL)

                val_emp <- get_idf_value(idd_env, idf_env, object = obj_emp$object_id,
                    complete = .complete, all = .all
                )
                set(val_emp, NULL, "rleid", rep(obj_emp$rleid, table(val_emp$rleid)))

                # assign default value if necessary
                if (.sgl) {
                    if (.default) {
                        val_emp <- assign_idf_value_default(idd_env, idf_env, val_emp)
                    }
                } else {
                    set(cls_val_emp, NULL, c("value_chr", "value_num"),
                        list(as.list(cls_val_emp$value_chr), as.list(cls_val_emp$value_num))
                    )
                }

                # exclude empty value input
                obj_val <- obj_val[rleid %in% obj$rleid]
            }

            if (!nrow(obj_val)) {
                obj <- data.table()
                obj_val <- data.table()
            } else {
                set(obj, NULL, "is_empty", NULL)

                add_joined_cols(obj, obj_val, "each_rleid", c("object_id", "object_name", "class_id", "class_name"))
                # after this, each_rleid is not needed
                set(obj, NULL, "each_rleid", NULL)

                # match field id
                obj_sgl <- obj_val[, by = "rleid", list(each_rleid = max(each_rleid), n = length(unique(each_rleid)))]
                obj_sgl <- match_idd_field(idd_env, obj_val[obj_sgl, on = c("rleid", "each_rleid")])

                obj_sgl <- obj_sgl[, by = "rleid",
                    list(field_id = rep(field_id, n[[1L]]),
                         field_index = rep(field_index, n[[1L]]),
                         field_name = rep(field_name, n[[1L]])
                    )
                ]

                set(obj_val, NULL, c("field_id", "field_index", "field_name"),
                    set(obj_sgl, NULL, "rleid", NULL)
                )

                # assign default value if necessary
                if (.default && .sgl) {
                    obj_val <- assign_idf_value_default(idd_env, idf_env, obj_val)
                }

                # complete fields if necessary
                if (.all || .complete) {

                    fld_in <- obj_val[, by = c("rleid", "each_rleid"), list(object_id = object_id[[1L]], field_index = max(field_index))]

                    # complete fields if necessary
                    obj_val_out <- get_idf_value(idd_env, idf_env,
                        object = fld_in$object_id, field = fld_in$field_index,
                        complete = .complete, all = .all
                    )
                    add_rleid(fld_in, "field")
                    add_joined_cols(fld_in, obj_val_out, c("rleid" = "field_rleid"), c("rleid", "each_rleid"))

                    # make the original value as a list if necessary
                    if (!.sgl) {
                        set(obj_val_out, NULL, c("value_chr", "value_num"),
                            list(as.list(obj_val_out$value_chr), as.list(obj_val_out$value_num))
                        )
                    }

                    # assign input value
                    obj_val_out[obj_val, on = c("each_rleid", "object_id", "field_id"),
                        `:=`(value_chr = i.value_chr, value_num = i.value_num)]
                    obj_val <- obj_val_out
                } else {
                    # add object name
                    obj_val[idf_env$value, on = c("object_id", "field_id"), value_id := i.value_id]
                }

                setorderv(obj_val, "each_rleid")
                set(obj_val, NULL, "each_rleid", NULL)
            }

            # combine empty
            obj <- rbindlist(list(obj_emp, obj), use.names = TRUE)
            obj_val <- rbindlist(list(val_emp, obj_val), use.names = TRUE)
        }
        # }}}

        # combine all
        obj <- rbindlist(list(cls_obj, obj), use.names = TRUE)
        val <- rbindlist(list(cls_val, obj_val), use.names = TRUE)
        setorderv(obj, "rleid")
        setorderv(val, "rleid")

        # keep column order
        setcolorder(obj, c("rleid", "class_id", "class_name", "object_id", "object_name", "object_name_lower", "comment"))
        setcolorder(val, c("rleid", "class_id", "class_name", "object_id", "object_name",
            "field_id", "field_index", "field_name", "value_id", "value_chr", "value_num"))
    }
    # }}}

    list(object = obj, value = val)
}
# }}}
# match_idd_field {{{
match_idd_field <- function (idd_env, dt_field) {
    # need to verify field name
    i <- which(!is.na(dt_field$field_name))
    # need to verify field index
    j <- which(!is.na(dt_field$field_index))

    # verify field name
    if (length(i)) {
        set(dt_field, i, c("field_id", "field_index", "field_name"),
            fast_subset(get_idd_field(idd_env, dt_field$class_id[i], dt_field$field_name[i], underscore = TRUE),
                c("field_id", "field_index", "field_name")
            )
        )
    }

    # verify field index
    if (length(j)) {
        set(dt_field, j, c("field_id", "field_name"),
            fast_subset(get_idd_field(idd_env, dt_field$class_id[j], dt_field$field_index[j]),
                c("field_id", "field_name")
            )
        )
    }

    # need to fill field index
    k <- which(is.na(dt_field$field_index))
    dt_field[, by = "rleid", field_index := {
        # stop if trying to modify same field multiple times
        matched <- !is.na(field_index)

        if (anyDuplicated(field_index[matched])) {
            wh <- which(duplicated(field_index[matched]))
            idx <- field_index[matched][wh]
            nm <- field_name[matched][wh]
            abort(paste0("Assertion on 'Field Index & Name' failed, element ", .BY$rleid, ": ",
                "Field index must not match an input field name: ",
                paste0(sprintf("{%i --> %s} at position %i", idx, nm, wh), collapse = "\n")),
                "dots_multi_match")
        }

        # all matched
        if (all(matched)) {
            field_index
        } else {
            # what are left after excluding detected field indices
            idx <- setdiff(seq_len(.N), field_index[matched])
            field_index[!matched] <- idx[seq.int(sum(!matched))]
            field_index
        }
    }]
    if (length(k)) {
        set(dt_field, k, c("field_id", "field_name"),
            fast_subset(get_idd_field(idd_env, dt_field$class_id[k], dt_field$field_index[k]),
                c("field_id", "field_name")
            )
        )
    }

    dt_field
}
# }}}
# expand_idf_dots_object {{{
#' Parse object values given in a list of Idf or IdfObject format
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#'
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#'
#' @param ... Lists of [Idf]s or [IdfObject]s.
#'
#' @param .unique If `TRUE`, make sure there are no duplicated objects in the
#'        input. If `FALSE`, duplicates are kept. If `NULL`, duplicates are
#'        removed. Default: `TRUE`.
#'
#' @param .complete If `TRUE`, make sure the returned field number meets the
#'        `\min-fields` requirement. Default: `TRUE`
#'
#' @param .all If `TRUE`, make sure the all possible fields are returned.
#'        Default: `FALSE`.
#'
#' @param .strict If `TRUE`, make sure all input objects come from the same
#'        version as that from `idf_env`. Default: `TRUE`.
#'
#' @return A named list of 3 [data.table::data.table()]: `meta`, `object` and
#' `value`.
#'
#' @keywords internal
#' @export
expand_idf_dots_object <- function (idd_env, idf_env, ..., .unique = TRUE, .strict = TRUE, .complete = TRUE, .all = FALSE) {
    l <- list(...)

    # stop if empty input
    if (!length(l)) abort("Assertion on 'Input' failed: Contains only missing values.", "dots_empty")

    # extract Idf meta
    extract_idf <- function (x) {
        list(version = get_priv_env(x)$m_version,
             uuid = get_priv_env(x)$uuid(),
             object_id = NA_integer_,
             idd_env = list(get_priv_env(x)$idd_env()),
             idf_env = list(get_priv_env(x)$idf_env())
        )
    }
    # extract IdfObject meta
    extract_idfobj <- function (x) {
        list(version = get_priv_env(get_priv_env(x)$m_parent)$m_version,
             uuid = get_priv_env(get_priv_env(x)$m_parent)$uuid(),
             object_id = get_priv_env(x)$m_object_id,
             idd_env = list(get_priv_env(x)$idd_env()),
             idf_env = list(get_priv_env(x)$idf_env())
        )
    }
    extract_data <- function (x) {
        if (is_idf(x)) extract_idf(x) else if (is_idfobject(x)) extract_idfobj(x)
    }

    len <- rep(1L, length(l))
    is_nest <- logical(length(l))
    d <- lapply(seq_along(l), function (i) {
        ll <- .subset2(l, i)
        if (is_idf(ll)) {
            extract_idf(ll)
        } else if (is_idfobject(ll)) {
            extract_idfobj(ll)
        } else {
            if (!test_list(ll, c("Idf", "IdfObject"), any.missing = FALSE, all.missing = FALSE)) {
                abort(paste0("Assertion on 'Input' failed, element ", i, ": ",
                    "Must be an 'Idf' or 'IdfObject', or a list of them."), "dots_format")
            }
            d <- lapply(ll, extract_data)
            # update actual object number
            len[[i]] <<- length(d)
            is_nest[[i]] <<- TRUE
            d
        }
    })

    # store all meta data in a table
    meta <- c()
    for (i in seq_along(d)) {
        if (is_nest[[i]]) meta <- c(meta, d[[i]]) else meta <- c(meta, d[i])
    }
    meta <- rbindlist(meta)
    set(meta, NULL, "rleid", rep(seq_along(d), len))
    add_rleid(meta, "object")

    if (.strict) {
        # get current version
        ver <- standardize_ver(get_idf_value(idd_env, idf_env, "Version")$value_chr)
        same_ver <- Reduce(c, meta$version)[, c(1:2)] == ver[, c(1:2)]
        if (!any(same_ver)) {
            abort(paste0("Assertion on 'Input' failed, element ", meta$rleid[!same_ver][1L], ": ",
                "Must have a version of ", surround(ver[, c(1:2)]), "."), "dots_format")
        }
    }

    # stop if duplicates
    has_dup <- FALSE
    if (anyDuplicated(meta, by = c("uuid", "object_id"))) {
        has_dup <- TRUE
        if (is.null(.unique)) {
            meta <- unique(meta, by = c("uuid", "object_id"))
        } else if (isTRUE(.unique)) {
            i <- meta[duplicated(meta, by = c("uuid", "object_id")), rleid[1L]]
            abort(paste0("Assertion on 'Input' failed, element ", i, ": Input must be all unique."), "dots_format")
        }
    }

    # hand Idf and IdfObject differently
    set(meta, NULL, "type_rleid", meta$object_id)
    meta[J(NA_integer_), on = "type_rleid", type_rleid := -seq_len(.N), by = "uuid"]

    obj_val <- meta[, by = c("uuid", "type_rleid"), {
        # Idf object
        if (type_rleid[[1L]] < 0L) {
            obj_per <- get_idf_object(idd_env[[1L]], idf_env[[1L]])
            val_per <- get_idf_value(idd_env[[1L]], idf_env[[1L]], complete = .complete, all = .all)

            set(obj_per, NULL, "rleid", object_rleid)
            set(val_per, NULL, "rleid", object_rleid)
        } else {
            obj_per <- get_idf_object(idd_env[[1L]], idf_env[[1L]], object = object_id)
            val_per <- get_idf_value(idd_env[[1L]], idf_env[[1L]], object = object_id, complete = .complete, all = .all)

            set(obj_per, NULL, "rleid", rep(object_rleid, table(obj_per$rleid)))
            set(val_per, NULL, "rleid", rep(object_rleid, table(val_per$rleid)))
        }

        list(object = list(obj_per), value = list(val_per))
    }]

    obj <- rbindlist(obj_val$object, use.names = TRUE)
    val <- rbindlist(obj_val$value, use.names = TRUE)

    # clean
    set(meta, NULL, c("rleid", "type_rleid"), NULL)
    setnames(meta, "object_rleid", "rleid")

    # keep column order
    setcolorder(meta, c("rleid", "version", "uuid", "object_id", "idd_env", "idf_env"))
    setcolorder(obj, c("rleid", "class_id", "class_name", "object_id", "object_name", "object_name_lower", "comment"))
    setcolorder(val, c("rleid", "class_id", "class_name", "object_id", "object_name",
        "field_id", "field_index", "field_name", "value_id", "value_chr", "value_num"))

    list(meta = meta, object = obj, value = val)
}
# }}}
# expand_idf_dots_literal {{{
#' Parse object values given in literal character vectors or data.frames
#'
#' @details
#' For object definitions in character vector format, they follow the
#' same rules as a normal IDF file:
#'
#' * Each object starts with a class name and a comma (`,`);
#' * Separates each values with a comma (`,`);
#' * Ends an object with a semicolon (`;`) for the last value.
#'
#' Each character vector can contain:
#'
#' * One single object, e.g. `c("Building,", "MyBuilding;")`, or "Building, MyBuilding;".
#' * Multiple objects, e.g. `c("Building, MyBuilding;", "SimulationControl, Yes")`.
#'
#' You can also provide an option header to indicate if input objects are
#' presented in IP units, using `!-Option ViewInIPunits`. If this header does
#' not exist, then all values are treated as in SI units.
#'
#' For object definitions in data.frame format, a valid definition requires at
#' least three columns described below. Note that column order does not matter.
#'
#' * `class`:Character type. Valid class names in the underlying
#'   [Idd] object.
#' * `index`:Integer type. Valid field indices for each class.
#' * `value`:Character type or list type. Value for each field
#'   to be added.
#'   - If character type, each value should be given as a string even if the
#'     corresponding field is a numeric type.
#'   - If list type, each value should have the right type as the corresponding
#'     field definition.
#' * `id`: **Optional** when `.exact` is `FALSE`. Integer type.
#'   If input data.frame includes multiple object definitions in a same class,
#'   values in `id` column will be used to distinguish each definition. If `id`
#'   column does not exists, it assumes that each definition is separated by
#'   `class` column and will issue an error if there is any duplication in the
#'   `index` column.
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#'
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#'
#' @param ... Character vectors or data.frames.
#'
#' @param .default If `TRUE`, all empty fields will be filled with default
#'        values if possible. Default: `TRUE`.
#'
#' @param .exact If `TRUE`, all inputs should match existing objects in the
#'        [Idf]. In this case, `id` column is require for data.frame input.
#'        Default: `FALSE`.
#'
#' @return A named list of 2 element `object` and `value` which is a
#' [data.table::data.table()] with object data and value data respectively.
#'
#' @note
#' Objects from character vectors will always be at the top of each table.
#'
#' @keywords internal
#' @export
expand_idf_dots_literal <- function (idd_env, idf_env, ..., .default = TRUE, .exact = FALSE) {
    l <- list(...)

    assert_list(l, c("character", "data.frame"), .var.name = "Input", min.len = 1L)

    is_chr <- vlapply(l, is.character)

    # character input {{{
    if (!any(is_chr)) {
        obj_chr <- data.table()
        val_chr <- data.table()
    } else {
        # parse {{{
        chr <- l[is_chr]
        qassertr(chr, "S", .var.name = "Character Input")

        # get total line number
        ln_chr <- vector("list", length(chr))
        for (i in seq_along(chr)) {
            if (i == 1L) {
                ln_chr[[i]] <- seq_along(chr[[i]]) + stri_count_fixed(chr[[i]], "\n")
            } else {
                ln_chr[[i]] <- seq_along(chr[[i]]) + stri_count_fixed(chr[[i]], "\n") + max(ln_chr[[i - 1L]])
            }
        }

        chr <- unlist(chr, FALSE, FALSE)
        chr_one <- paste0(chr, collapse = "\n")

        # indicate whether trying to add different Version object
        same_ver <- TRUE

        # parse as an IDF file
        ver <- standardize_ver(get_idf_value(idd_env, idf_env, "Version")$value_chr)
        parsed <- withCallingHandlers(
            parse_idf_file(chr_one, idd = ver, ref = FALSE),

            eplusr_warning_use_hard_coded_idd = function (w) invokeRestart("muffleWarning"),
            eplusr_warning_use_mismatch_idd = function (w) {same_ver <<- FALSE; invokeRestart("muffleWarning")},

            # modify messages if any error occurs
            eplusr_error_parse_idf = function (e) {
                data <- e$data

                # get the input number
                rle <- rep(seq_along(ln_chr), each_length(ln_chr))
                rle <- rle[unlist(ln_chr, FALSE, FALSE) %in% data$line]

                set(data, NULL, "msg_each", paste0("[Character Input #", rle, "] "))

                # get line number in each object
                title <- switch(class(e)[[1L]],
                    eplusr_error_parse_idf_ver = "Invalid IDF version found",
                    eplusr_error_parse_idf_line = "Invalid line found",
                    eplusr_error_parse_idf_object = "Incomplete object",
                    eplusr_error_parse_idf_class = "Invalid class name",
                    eplusr_error_parse_idf_field = "Invalid field number"
                )

                parse_error("idf", title, data, subtype = gsub("eplusr_error_parse_idf_", "", class(e)[[1L]], fixed = TRUE))
            }
        )

        if (!same_ver) {
            # locate the Version line
            data <- read_lines(chr)
            set(data, NULL, "rleid", rep(seq_along(ln_chr), each_length(ln_chr)))
            data <- data[J(c(attr(get_idf_ver(data), "line"))), on = "line"]
            set(data, NULL, "msg_each", paste0("[Character Input #", data$rleid, "] "))
            parse_error("idf", "Adding a different Version object is prohibited", data, subtype = "ver")
        }

        get_idf_value(idd_env, parsed, complete = )
        # remove inserted version object
        id <- parsed$object[J(1L), on = "class_id", object_id]
        obj_chr <- parsed$object[!J(id), on = "object_id"]
        val_chr <- parsed$value[!J(id), on = "object_id"]

        # after parsing all input character as a whole, there is no way to know
        # how many objects are extracted from each input
        # object ID should be sufficent for distinguishing all objects
        # add rleid for latter error printing
        set(obj_chr, NULL, "rleid", obj_chr$object_id)
        set(val_chr, NULL, "rleid", val_chr$object_id)
        # }}}
        # match {{{
        add_class_name(idd_env, obj_chr)
        add_joined_cols(obj_chr, val_chr, "object_id", c("class_id", "class_name"))
        add_field_property(idd_env, val_chr, c("field_index", "field_name"))

        if (.exact) {
            # if all class are valid, each object in class that has name attribute
            # should has a valid name
            if (anyNA(obj_chr$object_name)) {
                cls <- obj_chr[J(NA_character_), on = "object_name", paste0("'", class_name, "'", collapse = "\n")]
                abort(paste0("Assertion on 'Character Input' failed: Must be objects with names, ",
                    "but specified class with no name attribute {", cls, "}."),
                    "dots_format")
            }

            # verify object name
            obj_chr_match <- tryCatch(
                get_idf_object(idd_env, idf_env, obj_chr$class_id, obj_chr$object_name_lower, "has_name", ignore_case = TRUE),
                eplusr_error_invalid_object_name = function (e) {
                    nm <- obj_chr[J(e$value), on = "object_name_lower", paste0("'", object_name[1], "' (Class '", class_name[1], "')")]
                    abort(paste0("Assertion on 'Character Input' failed: ",
                        "Must be valid object names in current IDF, but unable to match name ", nm, "."),
                        "dots_format")
                }
            )

            # update object id
            setnames(obj_chr, "object_id", "input_object_id")
            set(obj_chr, NULL, "object_id", obj_chr_match$object_id)
            add_joined_cols(obj_chr, val_chr, c("object_id" = "input_object_id"), "object_id")
            set(obj_chr, NULL, "input_object_id", NULL)

            # update value id
            set(val_chr, NULL, "value_id", NA_integer_)
            val_chr[idf_env$value, on = c("object_id", "field_id"), value_id := i.value_id]
            val_chr[J(NA_integer_), on = "value_id", value_id := -.I]
        }
        # }}}

        # set rleid to negative in order to distinguish from data.frame input
        set(obj_chr, NULL, "rleid", -obj_chr$rleid)
        set(val_chr, NULL, "rleid", -val_chr$rleid)

        if (!.exact) {
            set(obj_chr, NULL, c("object_id", "object_name", "object_name_lower"), list(NA_integer_, NA_character_, NA_character_))
            set(val_chr, NULL, c("value_id", "object_id", "object_name"), list(NA_integer_, NA_integer_, NA_character_))
        } else {
            val_chr[obj_chr, on = c("rleid", "object_id"), object_name := i.object_name]
        }
    }
    # }}}
    # data.frame input {{{
    if (all(is_chr)) {
        obj_dt <- data.table()
        val_dt <- data.table()
    } else {
        # parse {{{
        # check types
        if (.exact) {
            n_col <- 4L
            nm_col <- c("id", "class", "index", "value")
        } else {
            n_col <- 3L
            nm_col <- c("class", "index", "value")
        }

        df <- l[!is_chr]
        dt <- lapply(seq_along(df), function (i) {
            dt <- as.data.table(.subset2(df, i))
            assert_names(names(dt), must.include = nm_col, .var.name = paste0("DataFrame Input #", i))

            # check types
            qassert(.subset2(dt, "class"), "S", paste0("class column in DataFrame Input #", i))
            qassert(.subset2(dt, "index"), "I", paste0("index column in DataFrame Input #", i))
            if (has_names(dt, "id")) {
                qassert(.subset2(dt, "id"), "I", paste0("id column in DataFrame Input #", i))
                if (anyDuplicated(dt, by = c("id", "class", "index"))) {
                    abort(paste0("Assertion on 'DataFrame Input #", i, "' failed: ",
                        "Must no duplicates among value combinations of column 'id', 'class' and 'index'"),
                        "dots_format"
                    )
                }
            } else {
                if (anyDuplicated(dt, by = c("class", "index"))) {
                    abort(paste0("Assertion on 'DataFrame Input #", i, "' failed: ",
                        "Must no duplicates among value combinations of column 'class' and 'index'"),
                        "dots_format"
                    )
                }
                set(dt, NULL, "id", NA_integer_)
            }

            # if value is character, trim spaces and convert them into NAs
            if (is.character(value <- .subset2(dt, "value"))) {
                # indicates it's a character vector
                type <- 1L

                value_chr <- stri_trim_both(.subset2(dt, "value"))
                value_chr[stri_isempty(value_chr)] <- NA_character_
                value_num <- suppressWarnings(as.double(value_chr))
            # if value is a list, each element should be a single character or a
            # number.
            } else {
                # indicates it's a list
                type <- 2L

                if (!is.list(value)) value <- as.list(value)

                # check if NULL
                isnull <- vlapply(value, is.null)

                # make sure no NA and scalar
                qassertr(value[!isnull], "V1", paste0("value column in DataFrame Input #", i))

                # change NULL to NA
                value[isnull] <- NA_character_

                # change empty strings to NA
                value_chr <- stri_trim_both(unlist(value, FALSE, FALSE))
                value_chr[stri_isempty(value_chr)] <- NA_character_

                isnum <- vlapply(value, is.numeric)
                value_num <- rep(NA_real_, length(value))
                value_num[isnum] <- unlist(value[isnum], FALSE, FALSE)
            }

            set(dt, NULL, c("rleid", "value_type", "value_chr", "value_num"), list(i, type, value_chr, value_num))
            set(dt, NULL, "value", NULL)

            if (length(extra_cols <- setdiff(names(dt), c("id", "class", "index", "rleid", "value_type", "value_chr", "value_num")))) {
                set(dt, NULL, extra_cols, NULL)
            }

            dt
        })

        dt <- rbindlist(dt, use.names = TRUE)
        # }}}
        # match {{{
        # rename
        setnames(dt, c("id", "class", "index"), c("object_id", "class_name", "field_index"))
        dt[, by = c("rleid", "object_id"), id := .GRP]
        setorderv(dt, "id")
        set(dt, NULL, "id", NULL)

        # in this case, object_rleid is the unique identifier
        set(dt, NULL, "object_rleid", rleid(dt$rleid, dt$class_name, dt$object_id))

        # extract object table
        obj_dt <- dt[, by = c("object_rleid"),
            list(rleid = rleid[[1L]], object_id = object_id[[1L]],
                 class_name = class_name[[1L]], num = max(field_index),
                 value_type = value_type[[1L]]
            )
        ]

        # extract all necessary fields
        if (!.exact) {
            val_dt <- tryCatch(
                get_idd_field(idd_env, obj_dt$class_name, obj_dt$num, complete = TRUE, property = "type_enum"),
                eplusr_error_invalid_class_name = function (e) {
                    invld <- obj_dt[J(e$value[[1L]]), on = "class_name", mult = "first"]
                    abort(paste0("Assertion on 'class column in DataFrame Input #", invld$rleid, "' failed: ",
                        "Must contain valid class names, but invalid one found {'", invld$class_name, "'}."),
                        "dots_format")
                },
                eplusr_error_invalid_field_index = function (e) {
                    invld <- obj_dt[J(e$data$class_name[1L], e$data$field_in[1L]), on = c("class_name", "num"), mult = "first"]
                    abort(paste0("Assertion on 'index column in DataFrame Input #", invld$rleid, "' failed: ",
                        "Must contain valid field indices, but invalid one found {'", invld$num, "' (Class: '", invld$class_name, "')}. ",
                        stri_replace_first_regex(errormsg_field_index(e$data[1L]), ".*\\. ", "")),
                        "dots_format")
                }
            )
            set(val_dt, NULL, "field_in", NULL)
            set(val_dt, NULL, "value_id", NA_integer_)
        } else {
            val_dt <- tryCatch(
                get_idf_value(idd_env, idf_env, obj_dt$class_name, obj_dt$object_id, obj_dt$num, property = "type_enum", complete = TRUE),
                eplusr_error_invalid_class_name = function (e) {
                    invld <- obj_dt[J(e$value[[1L]]), on = "class_name", mult = "first"]
                    abort(paste0("Assertion on 'class column in DataFrame Input #", invld$rleid, "' failed: ",
                        "Must contain valid class names, but invalid one found {'", invld$class_name, "'}."),
                        "dots_format")
                },
                eplusr_error_invalid_object_id = function (e) {
                    invld <- obj_dt[J(e$value[[1L]]), on = "object_id", mult = "first"]
                    abort(paste0("Assertion on 'id column in DataFrame Input #", invld$rleid, "' failed: ",
                        "Must contain valid object IDs, but invalid one found {'", invld$object_id, "' (Class: '", invld$class_name, "')}."),
                        "dots_format")
                },
                eplusr_error_invalid_field_index = function (e) {
                    invld <- obj_dt[J(e$data$class_name[1L], e$data$field_in[1L]), on = c("class_name", "num"), mult = "first"]
                    abort(paste0("Assertion on 'index column in DataFrame Input #", invld$rleid, "' failed: ",
                        "Must contain valid field indices, but invalid one found {'", invld$num, "' (Class: '", invld$class_name, "')}. ",
                        stri_replace_first_regex(errormsg_field_index(e$data[1L]), ".*\\. ", "")),
                        "dots_format")
                }
            )
        }

        # update class id
        obj_dt[val_dt, on = c("object_rleid" = "rleid"), class_id := i.class_id]

        # assign input value
        val_dt[dt, on = c(rleid = "object_rleid", "field_index"),
            `:=`(value_chr = i.value_chr, value_num = i.value_num)
        ]
        # update rleid related
        val_dt[obj_dt, on = c("rleid" = "object_rleid"),
            `:=`(rleid = i.rleid, object_rleid = i.object_rleid, value_type = i.value_type)
        ]
        set(obj_dt, NULL, c("num", "value_type"), NULL)
        # if value column is a character vector, need to reset values since
        # all of them are coerced regardless of field types
        val_dt[value_type == 1L & type_enum > IDDFIELD_TYPE$real, value_num := NA_real_]

        # add comment column
        if (.exact) {
            add_joined_cols(idf_env$object, obj_dt, "object_id", c("object_name", "object_name_lower", "comment"))
        } else {
            set(obj_dt, NULL, "comment", list(list(NULL)))
        }

        # clean
        # at this point, rleid is not useful
        set(obj_dt, NULL, "rleid", obj_dt$object_rleid)
        set(obj_dt, NULL, "object_rleid", NULL)
        set(val_dt, NULL, "rleid", val_dt$object_rleid)
        set(val_dt, NULL, c("type_enum", "value_type", "object_rleid"), NULL)
        if (!.exact) {
            set(obj_dt, NULL, c("object_id", "object_name", "object_name_lower"), list(NA_integer_, NA_character_, NA_character_))
            set(val_dt, NULL, c("object_id", "object_name"), list(NA_integer_, NA_character_))
        }
        # }}}
    }
    # }}}

    obj <- rbindlist(list(obj_chr, obj_dt), use.names = TRUE)
    val <- rbindlist(list(val_chr, val_dt), use.names = TRUE)

    # reset rleid
    set(obj, NULL, "rleid", rleid(obj$rleid))
    set(val, NULL, "rleid", rleid(val$rleid))

    # assign default value if necessary
    if (.default) val <- assign_idf_value_default(idd_env, idf_env, val)

    # keep column order
    setcolorder(obj, c("rleid", "class_id", "class_name", "object_id", "object_name", "object_name_lower", "comment"))
    setcolorder(val, c("rleid", "class_id", "class_name", "object_id", "object_name",
        "field_id", "field_index", "field_name", "value_id", "value_chr", "value_num"))

    list(object = obj, value = val)
}
# }}}
# expand_idf_regex {{{
#' Parse regular expression of object field values
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#'
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#'
#' @param pattern A single string of regular expression used to match field
#'        values
#'
#' @param class A character vector specifying the target class names
#'
#' @param pattern,ignore.case,perl,fixed,useBytes All of them are
#'        directly passed to [base::grepl][base::grep()] and
#'        [base::gsub][base::grep()] with the same default values.
#'
#' @return A named list of 2 [data.table::data.table()]: `object` and `value`.
#'
#' @keywords internal
#' @export
expand_idf_regex <- function (idd_env, idf_env, pattern, replacement = NULL,
                              class = NULL, ignore.case = FALSE, perl = FALSE,
                              fixed = FALSE, useBytes = FALSE) {
    assert_string(pattern)

    if (!is.null(class) && anyDuplicated(class)) {
        abort("Class should not contain any duplication.")
    }

    val <- get_idf_value(idd_env, idf_env, class, property = "type_enum")[,
        matched := grepl(pattern, value_chr, ignore.case = ignore.case, perl = perl,
            fixed = fixed, useBytes = useBytes)
    ]
    val <- val[J(val[matched == TRUE, unique(object_id)]), on = "object_id"]

    # add object rleid
    set(val, NULL, "rleid", rleid(val$object_id))

    # get object data
    if (!nrow(val)) {
        obj <- get_idf_object(idd_env, idf_env, 1L)[0L]
    } else {
        obj <- get_idf_object(idd_env, idf_env, object = unique(val[, by = "rleid", object_id]$object_id))
    }

    if (!is.null(replacement)) {
        assert_string(replacement)

        set(val, NULL, "value_chr",
            gsub(pattern, replacement, val$value_chr, ignore.case = ignore.case,
                perl = perl, fixed = fixed, useBytes = useBytes
            )
        )
        set(val, NULL, "value_num", suppressWarnings(as.double(val$value_chr)))
        val[type_enum > IDDFIELD_TYPE$real, value_num := NA_real_]
    }

    # keep column order
    setcolorder(obj, c("rleid", "class_id", "class_name", "object_id", "object_name", "object_name_lower", "comment"))
    setcolorder(val, c("rleid", "class_id", "class_name", "object_id", "object_name",
        "field_id", "field_index", "field_name", "value_id", "value_chr", "value_num"))

    list(object = obj, value = val)
}
# }}}

# OBJECT MUNIPULATION
# dup_idf_object {{{
#' Duplicate existing objects
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#' @param dt_object A [data.table::data.table()] that contains object data.
#' @param level Validate level. Default: `eplusr_option("validate_level")`.
#'
#' @return The modified [Idf] data in a named list of 5 elements, i.e. `object`,
#' `value`, `reference`, `changed` and `updated`. First 3 elements are
#' [data.table::data.table()]s containing the actual updated [Idf] data while
#' `changed` and `updated` are integer vectors containing IDs of objects that
#' have been directly changed and indirectly updated due to references,
#' respectively.
#'
#' @keywords internal
#' @export
dup_idf_object <- function (idd_env, idf_env, dt_object, level = eplusr_option("validate_level")) {
    chk <- level_checks(level)
    # transform input names to lower case
    set(dt_object, NULL, "new_object_name_lower", stri_trans_tolower(dt_object$new_object_name))

    # stop if try to dup version
    if (any(invld <- dt_object$class_name == "Version")) {
        abort(paste0("Duplicating 'Version' object is prohibited.\n",
            paste0(dt_object[invld, sprintf(" #%s| Object ID [%i] --> Class 'Version'",
                lpad(rleid, "0"), object_id)], collapse = "\n")),
            "dup_version")
    }
    # stop if trying to duplicate unique object
    if (chk$unique_object && nrow(invld <- dt_object[J(get_idd_class_unique(idd_env)$class_id), on = "class_id", nomatch = 0L])) {
        abort(paste0("Existing unique object cannot be duplicated. Invalid input:\n",
            get_object_info(invld, numbered = TRUE, collapse = "\n")),
            "dup_unique")
    }

    add_class_property(idd_env, dt_object, "has_name")
    obj <- make_idf_object_name(idd_env, idf_env, dt_object, use_old = TRUE, include_ori = TRUE, keep_na = FALSE)

    # update object name
    set(obj, NULL, c("object_name", "object_name_lower"), NULL)
    setnames(obj, c("new_object_name", "new_object_name_lower"), c("object_name", "object_name_lower"))

    # get new object ID
    id_obj <- new_id(idf_env$object, "object_id", nrow(obj))

    # logging
    if (in_verbose() && any(auto <- dt_object$has_name & is.na(dt_object$new_object_name))) {
        auto <- set(copy(obj), NULL, "object_id", id_obj)[auto]
        id <- get_object_info(auto, c("id", "class"))
        name <- get_object_info(auto, "name", prefix = " --> New ", numbered = FALSE)
        verbose_info(
            "New names of duplicated objects not given are automatically generated:\n",
            paste0(id, name, collapse = "\n")
        )
    }

    # assign new rleid
    obj <- add_rleid(obj)

    # extract value table
    val <- get_idf_value(idd_env, idf_env, object = obj$object_id, property = "is_name")
    # assign new object id
    obj <- assign_new_id(idf_env, obj, "object")
    add_joined_cols(obj, val, "rleid", "object_id")

    # assign new object name
    val[obj, on = "object_id", object_name := i.object_name]
    val[obj[J(TRUE), on = "has_name", nomatch = NULL], on = c("object_id", is_name = "has_name"), value_chr := i.object_name]

    # assign new value id
    set(val, NULL, "new_value_id", new_id(idf_env$value, "value_id", nrow(val)))

    # value reference
    # extract value reference
    # directly copy old field references excepting the name field
    dt_id <- fast_subset(val, c("object_id", "value_id", "new_value_id"))
    setnames(dt_id, "object_id", "new_object_id")
    ref <- idf_env$reference[dt_id, on = "value_id", nomatch = 0L]
    set(ref, NULL, c("object_id", "value_id"), NULL)
    setnames(ref, c("new_object_id", "new_value_id"), c("object_id", "value_id"))
    setcolorder(ref, names(idf_env$reference))

    # remove original ids
    set(val, NULL, "value_id", NULL)
    setnames(val, "new_value_id", "value_id")

    # NOTE: For original objects whose fields are referred by others, just keep
    # the original relation and no new relation needs to be created as one value
    # can only refer to one other value. However, it is possible that new input
    # object names can be referred by other existing objects
    add_field_property(idd_env, val, "src_enum")
    if (nrow(src_val <- val[src_enum > IDDFIELD_SOURCE$none])) {
        add_field_property(idd_env, idf_env$value, "type_enum")
        src <- get_value_reference_map(idd_env, src_val, idf_env$value, all = FALSE)[
            !J(NA_integer_), on = "value_id"]
        set(idf_env$value, NULL, "type_enum", NULL)
        ref <- rbindlist(list(ref, src))
    }

    list(object = append_dt(idf_env$object, obj),
         value = append_dt(idf_env$value, val),
         reference = append_dt(idf_env$reference, ref),
         changed = obj$object_id,
         updated = integer()
    )
}
# }}}
# add_idf_object {{{
#' Add new objects
#'
#' @inherit dup_idf_object
#' @param dt_value A [data.table::data.table()] that contains value data.
#' @param default If `TRUE`, default values are used for those blank
#'        fields if possible. If `FALSE`, empty fields are kept blank.
#'        Default: `TRUE`.
#' @param unique If `TRUE`, there are same objects in current [Idf] as input,
#'        duplications in input are removed. Default: `FALSE`.
#' @param empty If `TRUE`, trailing empty fields are kept. Default: `FALSE`.
#' @param level Validate level. Default: `eplusr_option("validate_level")`.
#'
#' @keywords internal
#' @export
add_idf_object <- function (idd_env, idf_env, dt_object, dt_value,
                            default = TRUE, unique = FALSE, empty = FALSE,
                            level = eplusr_option("validate_level")) {
    chk <- level_checks(level)
    # stop if try to add version
    if (any(invld <- dt_object$class_name == "Version")) {
        abort(paste0("Adding 'Version' object is prohibited. Invalid input:\n",
            paste0(sprintf(" #%s| Class 'Version'", lpad(dt_object$rleid[invld], "0")), collapse = "\n")),
            "add_version")
    }
    # stop if trying to add another unique object
    if (chk$unique_object && nrow(uni <- dt_object[J(get_idd_class_unique(idd_env)$class_id), on = "class_id", nomatch = 0L])) {

        # try to add multi objects in unique classes
        if (anyDuplicated(uni$class_id)) {
            abort(paste0("Unique object can only be added only once. Invalid input:\n",
                paste0(uni[duplicated(class_id), sprintf(" #%s| Class '%s'", lpad(rleid, "0"), class_name)], collapse = "\n")),
                "add_unique"
            )
        }

        if (nrow(invld <- idf_env$object[fast_subset(uni, c("rleid", "class_id", "class_name")), on = "class_id", nomatch = 0L])) {
            abort(paste0("Adding new object in existing unique-object class is prohibited. Invalid input:\n",
                get_object_info(invld[, object_id := -rleid], numbered = TRUE, collapse = "\n")),
                "add_unique"
            )

        }
    }

    # assign object id
    dt_object <- assign_new_id(idf_env, dt_object, "object")
    add_joined_cols(dt_object, dt_value, "rleid", "object_id")

    # assign value id
    dt_value <- assign_new_id(idf_env, dt_value, "value")

    # update object name
    if (!has_names(dt_value, "is_name")) add_field_property(idd_env, dt_value, "is_name")
    dt_object[dt_value[J(TRUE), on = "is_name", nomatch = 0L], on = c("rleid", "object_id"), object_name := i.value_chr]
    set(dt_object, NULL, "object_name_lower", stri_trans_tolower(dt_object$object_name))
    set(dt_value, NULL, "is_name", NULL)
    dt_value[dt_object, on = c("rleid", "object_id"), object_name := i.object_name]

    # assign default values
    if (default) dt_value <- assign_idf_value_default(idd_env, idf_env, dt_value)

    # delete empty fields
    if (!empty) dt_value <- remove_empty_fields(idd_env, idf_env, dt_value)

    # remove duplications
    if (unique) {
        l <- remove_duplicated_objects(idd_env, idf_env, dt_object, dt_value)
        dt_object <- l$object
        dt_value <- l$value
    }
    # if all inputs are duplications
    if (!nrow(dt_object)) {
        if (unique) verbose_info("After removing duplications, nothing to add.")
        return(list(
            object = idf_env$object, value = idf_env$value, reference = idf_env$reference,
            changed = integer(), updated = integer()))
    }

    # validate {{{
    # skip unique object checking
    chk$unique_object <- FALSE

    # temporarily change object id for error printing
    id_obj <- dt_object$object_id
    id_val <- dt_value$object_id
    set(dt_object, NULL, "object_id", -dt_object$rleid)
    set(dt_value, NULL, "object_id", -dt_value$rleid)

    # validate
    validity <- validate_on_level(idd_env, idf_env, dt_object, dt_value, level = chk)
    assert_valid(validity, "add")

    set(dt_object, NULL, "object_id", id_obj)
    set(dt_value, NULL, "object_id", id_val)
    # }}}

    # add reference {{{
    # Since 'check_invalid_reference()' will add new field references in idf_env$reference
    # only check new sources
    if (chk$reference) {
        i <- idf_env$reference[object_id < 0L, which = TRUE]
        if (length(i)) {
            rle <- idf_env$reference$object_id[i]
            set(idf_env$reference, i, "object_id", dt_object$object_id[-rle])
        }

        j <- idf_env$reference[src_object_id < 0L, which = TRUE]
        if (length(j)) {
            rle <- idf_env$reference$src_object_id[j]
            set(idf_env$reference, j, "src_object_id", dt_object$object_id[-rle])
        }

        # extract new reference
        k <- unique(c(i, j))
        if (!length(k)) {
            ref <- idf_env$reference[0L]
        } else {
            ref <- idf_env$reference[k]
            # remove from the original IDF reference table
            idf_env$reference <- idf_env$reference[-k]
        }
    # manually check new reference
    } else {
        # add necessary columns used for getting references
        add_field_property(idd_env, dt_value, "src_enum")

        add_field_property(idd_env, idf_env$value, "src_enum")
        add_joined_cols(idf_env$object, idf_env$value, "object_id", "class_id")
        add_class_name(idd_env, idf_env$value)

        ref <- get_value_reference_map(idd_env, append_dt(idf_env$value, dt_value), dt_value)
        set(idf_env$value, NULL, c("src_enum", "class_id", "class_name"), NULL)
    }

    # here should only find if any values in the original IDF reference input
    # values. references among input values have been handled previously
    src <- get_value_reference_map(idd_env, dt_value, idf_env$value, all = FALSE)[!J(NA_integer_), on = "object_id"]
    ref <- unique(rbindlist(list(ref, src)))
    # }}}

    list(object = append_dt(idf_env$object, dt_object),
         value = append_dt(idf_env$value, dt_value),
         reference = append_dt(idf_env$reference, ref, "value_id"),
         changed = dt_object$object_id,
         updated = integer()
    )
}
# }}}
# set_idf_object {{{
#' Modifying existing objects
#'
#' @inherit add_idf_object
#'
#' @keywords internal
#' @export
set_idf_object <- function (idd_env, idf_env, dt_object, dt_value, empty = FALSE, level = eplusr_option("validate_level"), replace = FALSE) {
    chk <- level_checks(level)
    # stop if try to modify version
    if (any(invld <- dt_object$class_name == "Version")) {
        abort(paste0("Modifying 'Version' object is prohibited. Invalid input:\n",
            paste0(sprintf(" #%s| Class 'Version'", lpad(dt_object$rleid[invld], "0")), collapse = "\n")),
            "set_version")
    }
    # stop if modifying same object multiple times
    if (anyDuplicated(dt_object$object_id)) {
        abort(paste0("Cannot modify same object multiple times. Invalid input:\n",
            get_object_info(dt_object[duplicated(object_id)], collapse = "\n")),
            "set_same"
        )
    }

    # add new value id in case there are new fields added
    dt_value[value_id < 0L, value_id := new_id(idf_env$value, "value_id", .N)]

    # update object name
    if (!has_names(dt_value, "is_name")) add_field_property(idd_env, dt_value, "is_name")
    dt_object[dt_value[J(TRUE), on = "is_name", nomatch = 0L], on = c("rleid", "object_id"), object_name := i.value_chr]
    set(dt_object, NULL, "object_name_lower", stri_trans_tolower(dt_object$object_name))
    set(dt_value, NULL, "is_name", NULL)
    dt_value[dt_object, on = c("rleid", "object_id"), object_name := i.object_name]

    # delete empty fields
    id_del <- integer()
    if (!empty) {
        dt_chk <- dt_value

        # append fields before checking. See #310
        fld_num <- idf_env$value[J(unique(dt_value$object_id)), on = "object_id",
            by = "object_id", list(num = .N)]
        set(fld_num, NULL, "field_index", fld_num$num)
        fld_in <- dt_value[, list(num = .N), by = c("rleid", "object_id")]
        fld_append <- fld_num[fld_in, on = list(num > num, object_id), nomatch = NULL]
        if (nrow(fld_append)) {
            fld_append <- fld_append[, by = c("rleid", "object_id"), list(field_index = seq(num + 1L, field_index))]
            val_append <- get_idf_value(idd_env, idf_env, object = fld_append$object_id, field = fld_append$field_index)
            set(val_append, NULL, "rleid", fld_append$rleid)
            dt_chk <- rbindlist(list(dt_value, val_append), fill = TRUE)
        }

        id_all <- dt_chk$value_id
        dt_value <- remove_empty_fields(idd_env, idf_env, dt_chk)
        id_del <- setdiff(id_all, dt_value$value_id)
    }

    # validate
    validity <- validate_on_level(idd_env, idf_env, dt_object, dt_value, level = chk)
    assert_valid(validity, "set")

    # remove existing references whose fields have been set to empty. see #355
    all_ref <- idf_env$reference[!dt_value[J(NA_character_), on = "value_chr", nomatch = NULL], on = "value_id"]

    # extract reference {{{
    # Since 'check_invalid_reference()' will add new field references in idf_env$reference
    # only check new sources
    if (chk$reference) {
        # extract new reference
        ref <- rbindlist(list(
            all_ref[J(dt_object$object_id), on = "object_id", nomatch = 0L],
            all_ref[J(dt_object$object_id), on = "src_object_id", nomatch = 0L]
        ))
    # manually check new reference
    } else {
        # add necessary columns used for getting references
        add_field_property(idd_env, dt_value, "src_enum")

        add_field_property(idd_env, idf_env$value, "src_enum")
        add_joined_cols(idf_env$object, idf_env$value, "object_id", "class_id")
        add_class_name(idd_env, idf_env$value)

        ref <- get_value_reference_map(idd_env, append_dt(idf_env$value, dt_value), dt_value)

        set(idf_env$value, NULL, c("src_enum", "class_id", "class_name"), NULL)
        set(dt_value, NULL, "src_enum", NULL)
    }

    # here should only find if any values in the original IDF reference input
    # values. references among input values have been handled previously
    src <- get_value_reference_map(idd_env, dt_value, idf_env$value, all = FALSE)[!J(NA_integer_), on = "object_id"]
    ref <- unique(rbindlist(list(ref, src)))
    # }}}

    # update referenced values
    add_joined_cols(dt_value, ref, c("src_value_id" = "value_id"), c("value_chr", "value_num"))
    # handle class-references #271
    if (any(i <- !is.na(ref$src_enum) & ref$src_enum == IDDFIELD_SOURCE$class)) {
        add_joined_cols(dt_object, ref, c("src_object_id" = "object_id"), "class_name")
        ref[i, value_chr := class_name]
    }
    # only update valid reference
    idf_env$value[ref[!J(NA_character_), on = "value_chr"], on = "value_id", `:=`(value_chr = i.value_chr, value_num = i.value_num)]

    if (length(id_del)) {
        if (replace) {
            value <- append_dt(idf_env$value[!J(id_del), on = "object_id"], dt_value, "object_id")
        } else {
            value <- append_dt(idf_env$value[!J(id_del), on = "value_id"], dt_value, "value_id")
        }

        # remove existing references whose fields have been set to empty. see #355
        all_ref <- all_ref[!J(id_del), on = "value_id"][!J(id_del), on = "src_value_id"]
        ref <- ref[!J(id_del), on = "value_id"][!J(id_del), on = "src_value_id"]
    } else {
        if (replace) {
            value <- append_dt(idf_env$value, dt_value, "object_id")
        } else {
            value <- append_dt(idf_env$value, dt_value, "value_id")
        }
    }

    order_idf_data(list(
        object = append_dt(idf_env$object, dt_object, "object_id"),
        value = value,
        reference = append_dt(all_ref, ref, "value_id"),
        changed = c(dt_object$object_id),
        updated = setdiff(ref$object_id, dt_object$object_id)
    ))
}
# }}}
# del_idf_object {{{
#' Delete existing objects
#'
#' @inherit add_idf_object
#' @param ref_by If `TRUE`, objects whose fields refer to input objects
#'        will also be deleted. Default: `FALSE`.
#' @param ref_to If `TRUE`, objects whose fields are referred by input
#'        objects will also be deleted. Default: `FALSE`.
#' @param recursive If `TRUE`, relation searching is performed
#'        recursively, in case that objects whose fields refer to target
#'        object are also referred by another object, and also objects
#'        whose fields are referred by target object are also referred
#'        by another object. Default: `FALSE`.
#' @param force If `TRUE`, objects are deleted even if they are
#'        referred by other objects.
#'
#' @keywords internal
#' @export
del_idf_object <- function (idd_env, idf_env, dt_object, ref_to = FALSE, ref_by = FALSE,
                            recursive = FALSE, force = FALSE, level = eplusr_option("validate_level")) {
    chk <- level_checks(level)

    # stop if try to delete version
    if (any(invld <- dt_object$class_name == "Version")) {
        abort(paste0("Deleting 'Version' object is prohibited.\n",
            paste0(dt_object[invld, sprintf(" #%s| Object ID [%i] --> Class 'Version'",
                lpad(rleid, "0"), object_id)], collapse = "\n")),
            "del_version")
    }
    if (!force) {
        # stop attempting to delete required objects
        if (chk$required_object && any(invld <- dt_object$class_id %in% idd_env$class[required_object == TRUE, class_id])) {
            abort(paste0("Deleting a required object is prohibited. Invalid input:\n",
                get_object_info(dt_object[invld], numbered = TRUE, collapse = "\n")),
                "del_required")
        }
        # stop if trying to delete unique object
        if (chk$unique_object && nrow(invld <- dt_object[J(get_idd_class_unique(idd_env)$class_id), on = "class_id", nomatch = 0L])) {
            abort(paste0("Existing unique object cannot be deleted. Invalid input:\n",
                get_object_info(invld, numbered = TRUE, collapse = "\n")),
                "del_unique")
        }
    }
    # stop if modifying same object multiple times
    if (anyDuplicated(dt_object$object_id)) {
        abort(paste0("Cannot delete same object multiple times. Invalid input:\n",
            get_object_info(dt_object[duplicated(object_id)], collapse = "\n")),
            "del_same"
        )
    }

    # get objects to be deleted
    id_del <- dt_object$object_id

    # always check if target objects are referred by others
    dir <- if (ref_to) "all" else "ref_by"

    depth <- if (recursive) NULL else 0L
    rel <- get_idfobj_relation(idd_env, idf_env, id_del, direction = dir,
        depth = depth, name = in_verbose(), class_ref = "both"
    )

    if (in_verbose()) {
        msg <- paste0("Deleting object(s) [ID: ", paste(id_del, sep = ", ", collapse = ", "), "]")
    }

    id_ref_by <- c()

    # ref by {{{
    # exclude invalid reference
    if (nrow(rel$ref_by)) {
        rel$ref_by <- rel$ref_by[!J(NA_integer_), on = "object_id"]

        # stop if objects are referred {{{
        # should be able to delete targets objects in at least one condition:
        # 1. current validate level does not includ reference checking
        # 2. want to delete both targets and referees
        # 3. `force` is TRUE
        if (chk$reference && !ref_by && !force && nrow(rel$ref_by)) {
            rel$ref_by <- rel$ref_by[!J(id_del), on = "object_id"]

            if (!eplusr_option("verbose_info")) {
                rel$ref_by <- add_idf_relation_format_cols(idd_env, idf_env, rel$ref_by)
            }
            abort(paste0("Cannot delete object(s) that are referred by others:\n",
                "\n",
                paste0("  ", unlist(format_idf_relation(rel$ref_by, "ref_by")$fmt, use.names = FALSE), collapse = "\n")
            ), "del_referenced")
        }
        # }}}

        if (ref_by && nrow(rel$ref_by)) {
            # check if objects that refer to targets are also referred by other
            # objects
            id_ref_by <- setdiff(unique(rel$ref_by$object_id), id_del)
            id_src <- id_ref_by[id_ref_by %in% idf_env$reference$src_object_id]
            if (!force && length(id_src)) {
                id_ref_by <- setdiff(id_ref_by, id_src)
                if (in_verbose()) {
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
                if (in_verbose()) {
                    msg <- c(msg,
                        paste0("Including object(s) [ID:", paste(id_ref_by, collapse = ", "), "] that refer to it.")
                    )
                }
            }
        }
    }
    # }}}

    # if ref_to is TRUE and rel$ref_to has contents
    # ref to {{{
    if (NROW(rel$ref_to)) {
        # exclude invalid reference
        rel$ref_to <- rel$ref_to[!J(NA_integer_), on = "src_object_id"]

        id_ref_to <- setdiff(unique(rel$ref_to$src_object_id), id_del)

        # check if objects that target refers to are also referred by other
        # objects
        id_src <- idf_env$reference[!J(id_del), on = "object_id"][
            J(unique(rel$ref_to$src_object_id)), on = "src_object_id", nomatch = 0L, unique(src_object_id)
        ]
        id_src <- setdiff(id_src, id_del)
        if (!force && length(id_src)) {
            id_ref_to <- setdiff(id_ref_to, id_src)
            if (in_verbose()) {
                if (length(id_ref_to)) {
                    msg <- c(msg,
                        paste0(
                            "Including object(s) [ID:", paste(id_ref_to, collapse = ", "), "] that is referred by it, ",
                            "skipping object(s) [ID: ", paste0(id_src, collapse = ","), "] that is also referred by other objects."
                        )
                    )
                } else {
                    msg <- c(msg,
                        paste0("Skipping object(s) [ID: ", paste0(id_src, collapse = ","), "] that is also referred by other objects.")
                    )
                }
            }
        } else {
            if (in_verbose()) {
                msg <- c(msg,
                    paste0("Including object(s) [ID:", paste(id_ref_by, collapse = ", "), "] that is referred by it.")
                )
            }
        }
    }
    # }}}

    if (in_verbose() &&
        ((ref_to && NROW(rel$ref_to)) || (ref_by && NROW(rel$ref_by)) ||
            (force && (NROW(rel$ref_to) || NROW(rel$ref_by))))) {
        msg <- paste0(c(msg, "", "Object relation is shown below:", ""), collapse = "\n")
        msg_rel <- paste0(" ", capture.output(print.IdfRelation(rel)), collapse = "\n")
        verbose_info(paste0(msg, msg_rel, collapse = "\n"))
    }

    id_del <- if (NROW(rel$ref_to)) c(id_del, id_ref_by, id_ref_to) else c(id_del, id_ref_by)

    list(object = idf_env$object[!J(id_del), on = "object_id"],
         value = idf_env$value[!J(id_del), on = "object_id"],
         reference = idf_env$reference[!J(id_del), on = "object_id"][
                J(id_del), on = "src_object_id",
                `:=`(src_object_id = NA_integer_, src_value_id = NA_integer_)],
         changed = id_del, updated = integer()
    )
}
# }}}
# purge_idf_object {{{
#' Purge not-used resource objects
#'
#' @inherit add_idf_object
#'
#' @keywords internal
#' @export
purge_idf_object <- function (idd_env, idf_env, dt_object) {
    # exclude objects that cannot be resources
    src <- dt_object[J(unique(idd_env$reference$src_class_id)), on = "class_id", nomatch = 0L]

    if (in_verbose()) {
        norm <- dt_object[!J(src$class_id), on = "class_id"]
        if (nrow(norm)) {
            verbose_info("Non-resource objects are ignored:\n", get_object_info(norm, collapse = "\n"))
        }
    }

    if (!nrow(src)) {
        verbose_info("None of specified object(s) can be purged. Skip.")
        return(list(
            object = idf_env$object, value = idf_env$value, reference = idf_env$reference,
            changed = integer(), updated = integer()))
    }

    # get references
    ref <- get_idf_relation(idd_env, idf_env, src$object_id, depth = 0L,
        direction = "ref_by", class_ref = "both")

    # get objects that can be removed directly
    id_del <- setdiff(src$object_id, ref$src_object_id)

    # take into account references inside inputs, i.e. an resource object can
    # be purged if all objects referencing it can be purged and have already
    # been captured in 'id_del'
    id_rec <- setdiff(
        # resources that are used by objects to be purged
        ref[J(id_del), on = "object_id", nomatch = 0L, src_object_id],
        # resources that are not used by objects to be purged
        ref[!J(id_del), on = "object_id", src_object_id]
    )

    # should do above step again to catch the deepest resources
    id_del <- c(id_del, id_rec)
    id_rec <- setdiff(
        # resources that are used by objects to be purged
        ref[J(id_del), on = "object_id", nomatch = 0L, src_object_id],
        # resources that are not used by objects to be purged
        ref[!J(id_del), on = "object_id", src_object_id]
    )

    id <- unique(c(id_del, id_rec))

    if (!length(id)) {
        verbose_info("None of specified object(s) can be purged. Skip.")
        obj <- idf_env$object
        val <- idf_env$value
        ref <- idf_env$reference
    } else {
        verbose_info("Object(s) below have been purged:\n",
            get_object_info(add_rleid(dt_object[J(id), on = "object_id"]), collapse = "\n"))

        # delete rows in object table
        obj <- idf_env$object[!J(id), on = "object_id"]
        val <- idf_env$value[!J(id), on = "object_id"]
        ref <- idf_env$reference[!J(id), on = "object_id"]
    }

    list(object = obj, value = val, reference = ref, changed = id, updated = integer())
}
# }}}
# duplicated_idf_object {{{
#' Determine duplicate objects
#'
#' @inherit add_idf_object
#'
#' @return A same [data.table::data.table()] as input `dt_object` (updated by
#' reference) with appended integer column `unique_object_id` indicating the
#' object is a duplicated one of that object.
#'
#' @keywords internal
#' @export
duplicated_idf_object <- function (idd_env, idf_env, dt_object) {
    dt_value <- idf_env$value[J(dt_object$object_id), on = "object_id"]
    add_field_property(idd_env, dt_value, c("field_index", "src_enum"))
    add_joined_cols(dt_object, dt_value, "object_id", "class_id")

    # change to lower case for comparison
    set(dt_value, NULL, "value_chr_lower", tolower(dt_value$value_chr))

    # should seperate resource objects and non-resource objects
    set(dt_value, NULL, "is_resource", FALSE)
    cls_rsrc <- dt_value[field_index == 1L & src_enum > IDDFIELD_SOURCE$none, unique(class_id)]
    if (length(cls_rsrc)) dt_value[J(cls_rsrc), on = "class_id", is_resource := TRUE]

    # get ID of objects to keep and delete
    dup <- rbindlist(lapply(
        split(dt_value[, .SD, .SDcols = c("class_id", "object_id", "field_index",
                "value_chr_lower", "is_resource")], by = "class_id"),
        function(d) {
            # dcast to compare
            dd <- data.table::dcast(d, class_id + object_id ~ field_index, value.var = "value_chr_lower")

            dd[, list(class_id = class_id[[1L]], object_id = object_id[[1L]], object_id_dup = list(object_id[-1L])),
                by = c(setdiff(names(dd), c("class_id", "object_id", "1"[d$is_resource[[1L]]])))][
              , list(class_id, object_id, object_id_dup)]
        }
    ))[, lapply(.SD, unlist), by = c("class_id", "object_id")]

    dt_object[dup, on = c("object_id" = "object_id_dup"), unique_object_id := i.object_id]

    dt_object
}
# }}}
# unique_idf_object {{{
#' Remove duplicate objects
#'
#' @inherit add_idf_object
#'
#' @keywords internal
#' @export
unique_idf_object <- function (idd_env, idf_env, dt_object) {
    dup <- duplicated_idf_object(idd_env, idf_env, dt_object)

    if (checkmate::allMissing(dup$unique_object_id)) {
        verbose_info("None duplicated objects found. Skip.")
        return(list(
            object = idf_env$object, value = idf_env$value, reference = idf_env$reference,
            changed = integer(), updated = integer()))
    }

    obj <- dup[!J(NA_integer_), on = "unique_object_id"]

    # remove reference rows of duplicated objects
    ref <- idf_env$reference[!J(obj$object_id), on = "object_id"]

    # get referenced field index of object to be deleted
    src <- ref[J(obj$object_id), on = "src_object_id", nomatch = NULL]

    if (in_verbose()) {
        setnames(obj, c("object_id", "object_name", "unique_object_id"), c("removed_object_id", "removed_object_name", "object_id"))
        obj[dup, on = "object_id", `:=`(object_name = i.object_name)]
        set(obj, NULL, "unique", get_object_info(obj, numbered = FALSE, prefix = ""))

        setnames(obj,
            c("removed_object_id", "removed_object_name", "object_id", "object_name"),
            c("object_id", "object_name", "unique_object_id", "unique_object_name")
        )

        obj[, rleid := seq_len(.N), by = c("class_id", "unique_object_id")]
        obj[, by = c("class_id", "unique_object_id"),
            removed := get_object_info(.SD, c("id", "name"), numbered = TRUE)
        ]

        msg <- obj[, by = c("class_id", "unique_object_id"), list(list(
            sprintf("Duplications for %s have been removed:\n %s",
                unique[[1L]], paste0(removed, collapse = "\n ")
            )
        ))]$V1
        verbose_info(paste0(unlist(msg), collapse = "\n\n"))
    }

    src[idf_env$value, on = c("src_object_id" = "object_id", "src_value_id" = "value_id"),
        `:=`(src_field_id = i.field_id)]

    # get unique object data
    src[obj, on = c("src_object_id" = "object_id"), unique_object_id := i.unique_object_id]
    src[idf_env$value, on = c("unique_object_id" = "object_id", "src_field_id" = "field_id"),
        `:=`(src_value_id = i.value_id, src_value_chr = i.value_chr, src_value_num = i.value_num)]

    # update referenced value
    idf_env$value[src, on = c("object_id", "value_id"), `:=`(
        value_chr = i.src_value_chr, value_num = i.src_value_num
    )]
    # update reference dict
    ref[src, on = c("object_id", "value_id"), `:=`(
        src_object_id = i.unique_object_id, src_value_id = i.src_value_id
    )]

    list(object = idf_env$object[!J(obj$object_id), on = "object_id"],
         value = idf_env$value[!J(obj$object_id), on = "object_id"],
         reference = ref, changed = obj$object_id, updated = setdiff(src$object_id, obj$object_id)
    )
}
# }}}
# rename_idf_object {{{
#' Rename existing objects
#'
#' @inherit add_idf_object
#'
#' @keywords internal
#' @export
rename_idf_object <- function (idd_env, idf_env, dt_object, level = eplusr_option("validate_level")) {
    chk <- level_checks(level)
    # stop if modifying same object multiple times
    if (anyDuplicated(dt_object$object_id)) {
        abort(paste0("Cannot modify same object multiple times. Invalid input:\n",
            get_object_info(dt_object[duplicated(object_id)], collapse = "\n")),
            "rename_same"
        )
    }
    # stop if no new name is provided when renaming
    if (!has_names(dt_object, "new_object_name")) {
        abort(paste0("Please give new object names. Invalid input:\n",
            get_object_info(dt_object, collapse = "\n")),
            "rename_no_new_name"
        )
    }
    if (anyNA(dt_object$new_object_name)) {
        abort(paste0("Please give new object names. Invalid input:\n",
            get_object_info(dt_object[is.na(new_object_name)], collapse = "\n")),
            "rename_no_new_name"
        )
    }

    obj <- make_idf_object_name(idd_env, idf_env, dt_object)
    set(obj, NULL, c("object_name", "object_name_lower"), NULL)
    setnames(obj, c("new_object_name", "new_object_name_lower"), c("object_name", "object_name_lower"))

    # extract value table
    val <- get_idf_value(idd_env, idf_env, object = obj$object_id, property = "is_name")[
        J(TRUE), on = "is_name", nomatch = NULL]

    # assign new object name
    set(obj, NULL, "has_name", TRUE)
    val[obj, on = c("object_id", is_name = "has_name"), value_chr := i.object_name]

    # validate
    # There are some special fields that could both reference to other objects
    # and be referenced by other objects.
    # For instance, `1: Zone Name` in `AirflowNetwork:MultiZone:Zone`. It
    # references to values from `Zone` names and also can be referenced by
    # `3: Zone Name` in `AirflowNetwork:IntraZone:Node`.
    # In this case, if these fields are names, it is also needed to check if new
    # names are valid.
    ref_to <- idf_env$reference[J(val$value_id, IDDFIELD_SOURCE$field), on = c("value_id", "src_enum"), nomatch = 0L]
    if (nrow(ref_to) && chk$reference) {
        validity <- validate_on_level(idd_env, idf_env, obj, val, level = chk)
        assert_valid(validity, "rename")
    }

    # value reference
    # extract value reference and update other objects using its name
    # only consider value reference
    ref_by <- idf_env$reference[J(val$value_id, IDDFIELD_SOURCE$field), on = c("src_value_id", "src_enum"), nomatch = 0L]

    # update values in main table
    if (nrow(ref_by)) {
        add_joined_cols(val, ref_by, c(src_value_id = "value_id"), c(src_value_chr = "value_chr"))
        idf_env$value[ref_by, on = "value_id", `:=`(value_chr = i.src_value_chr)]
        set(ref_by, NULL, "src_value_chr", NULL)
    }

    list(object = idf_env$object[obj, on = "object_id", `:=`(object_name = i.object_name, object_name_lower = i.object_name_lower)],
         value = idf_env$value[val, on = "value_id", `:=`(value_chr = i.value_chr, value_num = i.value_num)],
         reference = idf_env$reference,
         changed = obj$object_id,
         updated = setdiff(ref_by$object_id, obj$object_id)
    )
}
# }}}

# remove_empty_fields {{{
#' Remove trailing empty object fields
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#' @param dt_value A [data.table::data.table()] that contains value data.
#'
#' @return A [data.table::data.table()]
#'
#' @keywords internal
#' @export
remove_empty_fields <- function (idd_env, idf_env, dt_value) {
    if (!has_names(dt_value, "required_field")) {
        add_field_property(idd_env, dt_value, "required_field")
        on.exit(set(dt_value, NULL, "required_field", NULL), add = TRUE)
    }
    if (!has_names(dt_value, "min_fields")) {
        add_class_property(idd_env, dt_value, "min_fields")
        on.exit(set(dt_value, NULL, "min_fields", NULL), add = TRUE)
    }

    if (!dt_value[required_field == FALSE & is.na(value_chr) & min_fields < field_index, .N]) return(dt_value)

    # fields that can be deleted:
    # 1. not required
    # 2. do not have value
    # 3. field index should be consecutive from the end
    # 4. should be a whole extensible group
    dt_value[, rev_field_rleid := rev(field_index), by = "object_id"]
    on.exit(set(dt_value, NULL, "rev_field_rleid", NULL), add = TRUE)

    if (!has_names(dt_value, "extensible_group")) {
        add_field_property(idd_env, dt_value, "extensible_group")
        on.exit(set(dt_value, NULL, "extensible_group", NULL), add = TRUE)
    }
    if (!has_names(dt_value, "num_extensible")) {
        add_class_property(idd_env, dt_value, "num_extensible")
        on.exit(set(dt_value, NULL, "num_extensible", NULL), add = TRUE)
    }

    id_del <- dt_value[required_field == FALSE & is.na(value_chr) & field_index > min_fields,
        {
            # skip if no field found or field index not consecutive
            if (!.N || !length(idx <- rev_field_rleid[rev_field_rleid == rev(seq_len(.N))])) {
                list(value_id = NA_integer_)
            # all are non-extensible fields
            } else if (num_extensible[[1L]] == 0L) {
                list(value_id = rev(value_id)[idx])
            # handle extensible fields
            } else {
                # get extensible group numbers
                ext_num <- length(idx) %/% num_extensible[[1L]]

                # skip if not contain even one whole group
                if (!ext_num) {
                    list(value_id = NA_integer_)
                } else {
                    # extensible field index
                    idx_ext <- seq_len(ext_num * num_extensible[[1L]])

                    # if all rest are non-extensible fields, save to remove them
                    # all
                    if (length(idx[-idx_ext]) && rev(extensible_group)[idx[-idx_ext]][1L] == 0L) {
                        list(value_id = rev(value_id)[idx])
                    } else {
                        list(value_id = rev(value_id)[idx_ext])
                    }
                }
            }
        },
        by = c("object_id")
    ]

    if (any(!is.na(id_del$value_id))) dt_value <- dt_value[!id_del, on = "value_id"]

    dt_value
}
# }}}
# remove_duplicated_objects {{{
#' Remove duplicated objects in inputs
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#' @param dt_object A [data.table::data.table()] that contains object data.
#' @param dt_value A [data.table::data.table()] that contains value data.
#'
#' @return The modified input data in a named list of 2
#' [data.table::data.table()]s, i.e. `object` and `value`.
#'
#' @keywords internal
#' @export
remove_duplicated_objects <- function (idd_env, idf_env, dt_object, dt_value) {
    # extract all object values in the same class
    # in order to distinguish input from original IDF, set id of objects
    # from IDF Editor to negative also note that dcast will automatically
    # order object id, so this makes that input objects are always in the
    # bottom.
    add_joined_cols(idd_env$field, idf_env$value, "field_id", "field_index")
    add_joined_cols(idf_env$object, idf_env$value, "object_id", "class_id")
    val_idf <- idf_env$value[J(unique(dt_object$class_id)), on = "class_id",
        list(class_id, object_id = -object_id, field_index, value_chr), nomatch = 0L]
    set(idf_env$value, NULL, c("class_id", "field_index"), NULL)

    # get all input value
    val_in <- dt_value[, list(class_id, object_id, field_index, value_chr)]

    # compare in case-insensitive way
    if (!nrow(val_idf)) {
        # if there are no objects in the same class, only consider input
        val_d <- val_in
    } else {
        val_d <- rbindlist(list(val_idf, val_in), fill = TRUE)
    }
    set(val_d, NULL, "value_chr", stri_trans_tolower(val_d$value_chr))

    # dcast to compare
    val_d <- dcast(val_d, class_id + object_id ~ field_index, value.var = "value_chr")

    # get indicator
    dup <- duplicated(val_d, by = setdiff(names(val_d), "object_id"))

    # only find duplicates in input
    id_dup <- val_d[dup & object_id > 0L, object_id]

    if (length(id_dup)) {
        # give info
        verbose_info(
            "Duplicated objects in input, or objects in input that are the same in current IDF have been removed:\n",
            {
                del <- dt_object[J(id_dup), on = "object_id"]
                setorderv(del, "rleid")
                get_object_info(del, c("name", "class"), collapse = "\n", name_prefix = FALSE)
            }
        )

        dt_object <- dt_object[!J(id_dup), on = "object_id"]
        dt_value <- dt_value[!J(id_dup), on = "object_id"]
    }

    list(object = dt_object, value = dt_value)
}
# }}}

# REFERENCES
# get_idf_relation {{{
#' Extract object and value reference relations
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#' @param object_id An integer vector of valid object IDs. If `NULL`, all object
#'        IDs in current IDF will be used.
#' @param value_id An integer vector of valid value IDs. If `NULL`, all value
#'        IDs in current IDF will be used.
#' @param direction Reference relation direction. Should be one of `"ref_to"`
#'        and `"ref_by"`. Default: `"ref_to"`.
#' @param depth Recursive reference relation depth. `NULL` means infinite.
#'        Default: `0L`.
#' @param name If `TRUE`, all class, object, field value ID and name columns
#'        will be added and a `IdfRelationTo` or `IdfRelationBy` object is
#'        returned with customized printing method. Default: `FALSE`.
#' @param object An integer vector of valid object IDs or a character vector
#'        of valid object names to specify the targeting relation objects.
#'        Default: `NULL`.
#' @param class An integer vector of valid class indexes or a character vector
#'        of valid class names to specify the targeting relation classes.
#'        Default: `NULL`.
#' @param group A character vector of valid group names to specify the targeting
#'        relation groups. Default: `NULL`.
#' @param keep_all If `TRUE`, all input ID are kept. Otherwise, only input IDs
#'        that have relations are kept. Default: `FALSE`.
#' @param class_ref Specify how to handle class-name-references. There are 3
#'        options in total, i.e. `"none"`, `"both"` and `"all"`, with `"both"`
#'        being the default.
#'     * `"none"`: just ignore class-name-references.
#'     * `"both"`: only include class-name-references if this object also
#'       reference field values of the same one. This is the default option.
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
get_idf_relation <- function (idd_env, idf_env, object_id = NULL, value_id = NULL,
                              direction = c("ref_to", "ref_by"), depth = 0L, name = FALSE,
                              object = NULL, class = NULL, group = NULL, keep_all = FALSE,
                              class_ref = c("both", "none", "all"), match_all = FALSE) {
    assert_count(depth, null.ok = TRUE)
    if (is.null(depth)) depth <- Inf
    direction <- match.arg(direction)
    class_ref <- match.arg(class_ref)

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
        col_on <- "object_id"
    # if value ids are given
    } else {
        # if no object id is given
        if (is.null(object_id)) {
            # use specified value id
            id <- value_id
        # if object IDs are given
        } else {
            # make sure object IDs and value ids have the same length
            assert_same_len(object_id, value_id)
            obj_id <- object_id
            val_id <- value_id

            # find value ids
            id <- idf_env$value[J(obj_id, val_id), on = c("object_id", "value_id"), value_id]
        }
        col_on <- "value_id"
    }

    if (keep_all) {
        # make sure all input IDs appear in the result
        val <- idf_env$value[J(id), on = col_on, .SD, .SDcols = c("value_id", "object_id")]
    }

    if (direction == "ref_by") col_on <- paste0("src_", col_on)

    all_ref <- idf_env$reference

    if (class_ref == "none") {
        both <- FALSE
        all_ref <- all_ref[!J(IDDFIELD_SOURCE$class), on = "src_enum"]
    } else if (class_ref == "all") {
        both <- FALSE
    } else if (class_ref == "both") {
        both <- TRUE
        all_ref[idf_env$value, on = "value_id", field_id := i.field_id]
        all_ref[idf_env$value, on = c("src_value_id" = "value_id"), src_field_id := i.field_id]
        on.exit(set(all_ref, NULL, c("field_id", "src_field_id"), NULL), add = TRUE)
    }

    # init depth
    dep <- 0L

    # get first directly ref
    cur_ref <- all_ref[J(id), on = col_on, nomatch = NULL]
    set(cur_ref, NULL, "dep", if (nrow(cur_ref)) dep else integer())

    if (direction == "ref_to") {
        col_ref <- "src_object_id"
        col_rev <- "object_id"
    } else if (direction == "ref_by") {
        col_ref <- "object_id"
        col_rev <- "src_object_id"
    }

    # restrict searching reference ranges
    # NOTE: should do this depending on depth value
    # This makes it possible to find recursive relations, e.g. how is the
    # one AirLoopHVAC related to an Schedule:Compact object?
    cls_id <- NULL
    if (!is.null(group)) {
        grp_id <- get_idd_group_index(idd_env, group)
        cls_id <- idd_env$class[J(grp_id), on = "group_id"]$class_id
    }
    if (!is.null(class)) {
        cls_id <- unique(c(cls_id, get_idd_class(idd_env, class)$class_id))
    }
    obj_id <- NULL
    if (!is.null(cls_id)) {
        obj_id <- idf_env$object[J(cls_id), on = "class_id", nomatch = 0L, object_id]
    }
    if (!is.null(object)) {
        obj_id <- unique(c(obj_id, get_idf_object(idd_env, idf_env, object = object)$object_id))
    }
    if (depth == 0L && !is.null(obj_id)) {
        cur_ref <- cur_ref[J(obj_id), on = col_ref, nomatch = 0L]
    }

    # no matched objects found for specified classes or groups
    if (!is.null(obj_id) && !length(obj_id)) all_ref <- all_ref[0L]

    ref <- get_recursive_relation(all_ref, cur_ref, dep, depth, col_ref,
        col_rev, obj_id, both = both, match_all = match_all)

    # remove redundant columns
    if (both) set(ref, NULL, c("field_id", "src_field_id"), NULL)

    # keep all input
    if (keep_all) ref <- combine_input_and_relation(val, ref, "idf", direction)

    setcolorder(ref, c("object_id", "value_id", "src_object_id", "src_value_id", "src_enum", "dep"))

    if (!name) return(ref)

    ref <- add_idf_relation_format_cols(idd_env, idf_env, ref)

    cls <- switch(direction, ref_by = "IdfRelationBy", ref_to = "IdfRelationTo")
    setattr(ref, "class", c(cls, class(ref)))
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

# NODES
# get_idf_node_relation {{{
#' Extract node relations
#'
#' @inheritParams get_idf_relation
#'
#' @return A data.table.
#'
#' @keywords internal
#' @export
get_idf_node_relation <- function (idd_env, idf_env, object_id = NULL, value_id = NULL,
                                   object = NULL, class = NULL, group = NULL,
                                   name = FALSE, keep_all = FALSE, depth = 0L) {
    assert(
        check_integerish(object_id, any.missing = FALSE, null.ok = TRUE),
        check_integerish(value_id, any.missing = FALSE, null.ok = TRUE)
    )
    assert_count(depth, null.ok = TRUE)
    if (is.null(depth)) depth <- Inf

    # extract all node data
    all_nodes <- add_field_property(idd_env, property = "type_enum",
        idf_env$value[!J(NA_character_), on = "value_chr", .SD,
            .SDcols = c("object_id", "field_id", "value_id", "value_chr")])
    all_nodes <- all_nodes[J(IDDFIELD_TYPE$node), on = "type_enum", nomatch = 0L]
    set(all_nodes, NULL, c("type_enum", "field_id"), NULL)
    set(all_nodes, NULL, "value_chr", stri_trans_tolower(all_nodes$value_chr))

    # if no value id is given
    if (is.null(value_id)) {
        # if no object id is given
        if (is.null(object_id)) {
            # node search needs a start point
            abort("A start point is needed for searching for node relation. Either 'object_id' or 'value_id' should not be NULL.")
        } else {
            # use specified object IDs
            id <- object_id
        }
        col_on <- "object_id"
    # if value ids are given
    } else {
        # if no object id is given
        if (is.null(object_id)) {
            # use specified value id
            id <- value_id
        # if object IDs are given
        } else {
            # make sure object IDs and value ids have the same length
            assert_same_len(object_id, value_id)
            obj_id <- object_id
            val_id <- value_id

            # find value ids
            id <- idf_env$value[J(obj_id, val_id), on = c("object_id", "value_id"), value_id]
        }
        col_on <- "value_id"
    }

    if (keep_all) {
        # make sure all input IDs appear in the result
        val <- idf_env$value[J(id), on = col_on, .SD, .SDcols = c("value_id", "object_id")]
    }

    # init depth
    dep <- 0L

    cur_nodes <- all_nodes[J(id), on = col_on, nomatch = NULL]
    setnames(cur_nodes, c("object_id", "value_id"), c("src_object_id", "src_value_id"))
    # excluding already matched nodes
    all_nodes <- all_nodes[!J(cur_nodes$src_value_id), on = "value_id"]
    cur_nodes <- all_nodes[cur_nodes, on = "value_chr"]
    set(cur_nodes, NULL, "dep", dep)

    col_ref <- "object_id"
    col_rev <- "src_object_id"

    # restrict searching reference ranges
    # NOTE: should do this depending on depth value
    # This makes it possible to find recursive relations, e.g. how is the
    # one AirLoopHVAC related to an Schedule:Compact object?
    cls_id <- NULL
    if (!is.null(group)) {
        grp_id <- get_idd_group_index(idd_env, group)
        cls_id <- idd_env$class[J(grp_id), on = "group_id"]$class_id
    }
    if (!is.null(class)) {
        cls_id <- unique(c(cls_id, get_idd_class(idd_env, class)$class_id))
    }
    obj_id <- NULL
    if (!is.null(cls_id)) {
        obj_id <- idf_env$object[J(cls_id), on = "class_id", nomatch = 0L, object_id]
    }
    if (!is.null(object)) {
        obj_id <- unique(c(obj_id, get_idf_object(idd_env, idf_env, object = object)$object_id))
    }
    if (depth == 0L && !is.null(obj_id)) {
        cur_nodes <- cur_nodes[J(obj_id), on = col_ref, nomatch = 0L]
    }

    # no matched objects found for specified classes or groups
    if (!is.null(obj_id) && !length(obj_id)) {
        all_nodes <- all_nodes[0L]
        cur_nodes <- cur_nodes[0L]
    }

    # store classes or objects needed to be removed later
    del <- list()

    # parent class or object
    parent <- cur_nodes[[col_rev]]

    ref <- cur_nodes
    # for node should reduce one level
    while (dep < (depth - 1L) && nrow(cur_nodes)) {
        # skip if specified classes/objects are matched
        if (length(obj_id)) {
            skip <- cur_nodes[J(obj_id), on = col_ref, .SD, .SDcols = col_rev, nomatch = 0L][[1L]]
            if (length(skip)) {
                cur_nodes <- cur_nodes[!J(skip), on = col_rev]
            }
        }
        # if all are matched, stop
        if (!nrow(cur_nodes)) break

        # excluding already matched values
        all_nodes <- all_nodes[!J(cur_nodes$value_id), on = "value_id"]
        # match new nodes
        new_nodes <- all_nodes[J(unique(cur_nodes$object_id)), on = col_ref, nomatch = NULL]
        # excluding already matched objects
        all_nodes <- all_nodes[!J(new_nodes$value_id), on = "value_id"]
        setnames(new_nodes, c("object_id", "value_id"), c("src_object_id", "src_value_id"))
        new_nodes <- all_nodes[new_nodes, on = "value_chr", nomatch = NULL]

        # get objects that do not going any deeper
        # those objects should be removed
        if (length(obj_id)) {
            del <- c(del,
                list(setattr(setdiff(cur_nodes[[col_ref]], c(obj_id, new_nodes[[col_rev]])), "dep", dep))
            )
            cur_nodes
        }

        cur_nodes <- new_nodes

        # add depth
        dep <- dep + 1L
        # set depth value
        set(cur_nodes, NULL, "dep", dep)
        # merge into the main results
        ref <- rbindlist(list(ref, cur_nodes))

        # remove self reference
        cur_nodes <- cur_nodes[!J(parent), on = col_ref]
    }

    set(ref, NULL, "value_chr", NULL)
    set(ref, NULL, "src_enum", IDDFIELD_SOURCE$field)
    setcolorder(ref, names(idf_env$reference))

    # should search backwards to only include paths related to specified
    # classes/objects
    if (length(obj_id) && nrow(ref)) ref <- del_recursive_relation(ref, del, obj_id, "object_id", "src_object_id")

    # keep all input
    if (keep_all) ref <- combine_input_and_relation(val, ref, "idf", "ref_by")

    if (!name) return(ref)

    ref <- add_idf_relation_format_cols(idd_env, idf_env, ref)
    setattr(ref, "class", c("IdfRelationNode", class(ref)))
    ref
}
# }}}

# IDF Editor Integration
# read_idfeditor_copy {{{
#' Parse objects from IDF Editor
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#' @param version The version of IDF file open by IDF Editor, e.g. `8.6`,
#'        `"8.8.0"`. If `NULL`, assume that the file has the same
#'        version as current Idf object. Default: `NULL`.
#' @param in_ip Set to `TRUE` if the IDF file is open with `Inch-Pound`
#'        view option toggled. Numeric values will automatically
#'        converted to SI units if necessary. Default: `FALSE`.
#'
#' @note
#' References in the input is not parsed and `reference` in the returned list is
#' always a zero-row table.
#'
#' @return The copyied object data from IDF Editor in a named list of 3
#' [data.table::data.table()]s, i.e. `object`, `value` and `reference`.
#'
#' @keywords internal
#' @export
read_idfeditor_copy <- function (idd_env, idf_env, version = NULL, in_ip = FALSE) { # nocov start
    if (!is_windows()) {
        abort("Currently 'read_idfeditor_copy()' can only work on Windows platform.")
    }

    text <- readLines("clipboard", warn = FALSE)

    if (length(text) != 1L || !stringi::stri_startswith_fixed(text, "IDF,")) {
        abort("Failed to find contents copied from IDF Editor.")
    }
    text <- stringi::stri_replace_all_regex(stri_sub(text, 5L), "([,;])", "$1\n")

    if (isTRUE(in_ip)) {
        text <- paste0("!-Option SortedOrder ViewInIPunits\n", text)
    }

    if (is.null(version)) {
        version <- get_idf_value(idd_env, idf_env, "Version")$value_chr
    }

    # ignore the warning of using given IDD
    parsed <- withCallingHandlers(parse_idf_file(text, idd = version, ref = FALSE),
        eplusr_warning = function (w) invokeRestart("muffleWarning")
    )

    # add class name
    add_class_name(idd_env, parsed$object)
    # add class id and field index
    add_joined_cols(parsed$object, parsed$value, "object_id", c("class_id", "class_name"))
    add_joined_cols(idd_env$field, parsed$value, "field_id", c("field_index", "field_name"))

    # remove version object
    obj_ver <- parsed$object[class_name == "Version", object_id]
    parsed$object <- parsed$object[!J(obj_ver), on = "object_id"]
    parsed$value <- parsed$value[!J(obj_ver), on = "object_id"]

    # add rleid for validation and message printing
    add_rleid(parsed$object)
    add_joined_cols(parsed$object, parsed$value, "object_id", c("rleid", "object_name"))

    # remove empty fields
    parsed$value <- remove_empty_fields(idd_env, idf_env, parsed$value)

    setcolorder(parsed$object, c("rleid", "class_id", "class_name", "object_id", "object_name", "object_name_lower", "comment"))
    setcolorder(parsed$value, c("rleid", "class_id", "class_name", "object_id", "object_name", "field_id", "field_index", "field_name", "value_id", "value_chr"))

    parsed
} # nocov end
# }}}

# TABLE
# get_idf_table {{{
#' Extract value data in a data.table
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#' @param class An integer vector of valid class indexes or a character vector
#'        of valid class names. Default: `NULL`.
#' @param object An integer vector of valid object IDs or a character vector
#'        of valid object names. Default: `NULL`.
#' @param string_value If `TRUE`, all field values are returned as
#'        character. If `FALSE`, `value` column in returned
#'        [data.table][data.table::data.table()] is a list column with
#'        each value stored as corresponding type. Note that if the
#'        value of numeric field is set to `"Autosize"` or
#'        `"Autocalculate"`, it is left as it is, leaving the returned
#'        type being a string instead of a number. Default: `TRUE`.
#' @param unit Only applicable when `string_value` is `FALSE`. If
#'        `TRUE`, values of numeric fields are assigned with units using
#'        [units::set_units()] if applicable. Default: `FALSE`.
#' @param wide Only applicable if target objects belong to a same class.
#'        If `TRUE`, a wide table will be returned, i.e. first three
#'        columns are always `id`, `name` and `class`, and then every
#'        field in a separate column. Note that this requires all
#'        objects specified must from the same class.
#'        Default: `FALSE`.
#' @param align If `TRUE`, all objects in the same class will have the
#'        same field number. The number of fields is the same as the
#'        object that have the most fields among objects specified.
#'        Default: `FALSE`.
#' @param all If `TRUE`, all available fields defined in IDD for the
#'        class that objects belong to will be returned. Default:
#'        `FALSE`.
#' @param group_ext Should be one of `"none"`, `"group"` or `"index"`.
#'        If not `"none"`, `value` column in returned
#'        [data.table::data.table()] will be converted into a list.
#'        If `"group"`, values from extensible fields will be grouped by the
#'        extensible group they belong to. For example, coordinate
#'        values of each vertex in class `BuildingSurface:Detailed` will
#'        be put into a list. If `"index"`, values from extensible fields
#'        will be grouped by the extensible field indice they belong to.
#'        For example, coordinate values of all x coordinates will be
#'        put into a list. If `"none"`, nothing special will be done.
#'        Default: `"none"`.
#' @param force If `TRUE`, `wide` can be `TRUE` even though there are
#'        multiple classes in input. This can result in a data.table
#'        with lots of columns. But may be useful when you know that
#'        target classes have the exact same fields, e.g.
#'        `Ceiling:Adiabatic` and `Floor:Adiabatic`. Default: `FALSE`.
#' @param init If `TRUE`, a table for new object input will be returned
#'        with all values filled with defaults. In this case, `object`
#'        input will be ignored. The `id` column will be filled with
#'        possible new object IDs. Default: `FALSE`.
#'
#' @return A [data.table][data.table::data.table()] with 6 columns (if
#' `wide` is `FALSE`) or at least 5 columns (if `wide` is `TRUE`).
#'
#' When `wide` is `FALSE`, the 5 columns are:
#'
#' * `id`: Integer type. Object IDs.
#' * `name`: Character type. Object names.
#' * `class`: Character type. Current class name.
#' * `index`: Integer type. Field indexes.
#' * `field`: Character type. Field names.
#' * `value`: Character type if `string_value` is `TRUE` or list type if
#'   `string_value` is `FALSE` or `group_ext` is not `"none"`. Field values.
#'
#' @keywords internal
#' @export
get_idf_table <- function (idd_env, idf_env, class = NULL, object = NULL,
                           string_value = TRUE, unit = FALSE, wide = FALSE,
                           align = FALSE, all = FALSE, group_ext = c("none", "group", "index"),
                           force = FALSE, init = FALSE) {
    group_ext <- match.arg(group_ext)

    cols <- c("object_id", "object_name", "class_name",
              "field_index", "field_name",
              "units", "ip_units", "type_enum", "extensible_group",
              "value_chr", "value_num")

    if (init) {
        if (!is.null(object)) warn("'object' is ignored when 'init' is set to 'TRUE'.")

        val <- init_idf_value(idd_env, idf_env, class, complete = TRUE, all = all, id = TRUE,
            property = c("units", "ip_units", "type_enum", "extensible_group")
        )

        # assign new object id
        set(val, NULL, "object_id", val$rleid + max(idf_env$object$object_id))
    } else {
        val <- get_idf_value(idd_env, idf_env, class = class, object = object,
            property = c("units", "ip_units", "type_enum", "extensible_group"),
            align = align, complete = TRUE, all = all, ignore_case = TRUE)[
            , .SD, .SDcols = c("rleid", cols)]
    }

    if (wide && length(cls <- unique(val$class_name)) != 1L && !force) {
        if (length(cls) <= 5L) {
            cls <- collapse(cls)
        } else {
            cls <- paste0(c(surround(cls[1:5]), "..."), collapse = ", ")
        }
        abort(paste0("Target objects should belong to a same class when 'wide' is TRUE. ",
            "Multiple classes found: ", cls, "."
        ))
    }

    setnames(val,
        c("object_id", "object_name", "class_name", "field_index", "field_name"),
        c("id", "name", "class", "index", "field"))

    if (string_value) {
        set(val, NULL, c("units", "ip_units", "type_enum", "value_num"), NULL)
        setnames(val, "value_chr", "value")
    } else {
        lst <- get_value_list(val, unit = unit)
        if (nrow(val) == 1L) {
            set(val, NULL, "value", list(lst))
        } else {
            set(val, NULL, "value", lst)
        }
    }

    if (group_ext != "none") {
        non_ext <- val[extensible_group == 0L][, `:=`(value = as.list(value))]

        if (group_ext == "group") {
            ext <- val[extensible_group > 0L][,
                list(index = NA_integer_,
                     field = paste(abbreviate(field, 10), collapse = "|"),
                     value = list(value)),
                by = c("rleid", "id", "name", "class", "extensible_group")
            ]
        } else if (group_ext == "index") {
            fun <- if (string_value) function (x) unlist(x, FALSE, FALSE) else function (x) do.call(c, x)
            ext <- val[extensible_group > 0L][,
                extensible_group := data.table::rowid(rleid, id, extensible_group)][,
                list(index = NA_integer_, field = field[1L], value = list(fun(value))),
                by = c("rleid", "id", "name", "class", "extensible_group")][,
                `:=`(field = stri_replace_first_regex(field, "(?<=(#|A|N| ))\\d+ ", ""))
            ]
        }

        val <- rbindlist(list(non_ext[, .SD, .SDcols = names(ext)], ext))
        setorderv(val, c("rleid", "id"))
        set(val, NULL, "index", rowidv(val, c("rleid", "id")))

        # store extensible names
        cols_ext <- unique(ext$field)
    }

    if (string_value) {
        if (wide) {
            res <- setcolorder(
                dcast(val, rleid + id + name + class ~ field, value.var = "value"),
                c("id", "name", "class", unique(val$field))
            )
            set(res, NULL, "rleid", NULL)[]
        } else {
            val[, .SD, .SDcols = c("id", "name", "class", "index", "field", "value")]
        }
    } else {
        if (wide) {
            val <- setcolorder(
                dcast(val, rleid + id + name + class ~ field, value.var = "value", fill = NA),
                c("id", "name", "class", unique(val$field))
            )
            set(val, NULL, "rleid", NULL)

            cols <- setdiff(names(val), c("id", "name", "class"))
            if (group_ext == "none") cols_ext <- character()
            if (!unit) {
                val[, c(setdiff(cols, cols_ext)) := lapply(.SD, unlist, recursive = FALSE, use.names = FALSE),
                    .SDcols = setdiff(cols, cols_ext)]
            } else {
                # get unit attributes
                unit <- val[, lapply(.SD, function (x) list(attr(x[[1]], "units"))), .SDcols = cols]

                val[, c(setdiff(cols, cols_ext)) := lapply(.SD, unlist, recursive = FALSE, use.names = FALSE),
                    .SDcols = setdiff(cols, cols_ext)]

                # only need to handle non-extensible groups
                for (nm in setdiff(names(unit), cols_ext)) {
                    if (!is.null(unit[[nm]][[1L]])) {
                        set(val, NULL, nm, setattr(setattr(val[[nm]], "units", unit[[nm]][[1L]]), "class", "units"))
                    }
                }
            }
            val[]
        } else {
            val[, .SD, .SDcols = c("id", "name", "class", "index", "field", "value")]
        }
    }
}
# }}}

#' Format Long Table to Standard Input for `Idf$load()` Method
#'
#' `dt_to_load()` takes a [data.table][data.table::data.table()], usually
#' created from [`Idf$to_table()`][Idf] or [`IdfObject$to_table()`][IdfObject]
#' with `wide` being `TRUE`, and format it into a
#' [data.table][data.table::data.table()] in acceptable format for `$load()`
#' method in [Idf] class.
#'
#' @param dt A data.table created using `Idf$to_table()` and
#'        `IdfObject$to_table()`. `dt` should at least contain column `id`
#'        (indicator used to distinguish object definitions), `class` (class
#'        names). If a `name` column exists, it will be preserved.
#' @param string_value If `TRUE`, all value will be coerced into character and
#'        the `value` column of returned [datat.table][data.table::data.table()]
#'        will be character type. If `FALSE`, the original value will be
#'        preserved and the `value` column of returned
#'        [data.table][data.table::data.table()] will be list type.
#'
#' @return
#' A [data.table][data.table::data.table()] with 5 or 6 columns:
#'
#' * `id`: Integer type. Used to distinguish each object definition.
#' * `name`: Character type. Only exists when input `dt` has a `name` column.
#' * `class`: Character type.
#' * `index`: Integer type. Field indices.
#' * `field`: Character type. Field names.
#' * `value`: Character or list type. The value of each field to be added.
#'
#' @examples
#' \dontrun{
#' # read an example distributed with eplusr
#' path_idf <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
#' idf <- read_idf(path_idf)
#'
#' # extract all material object data and return it as a wide table
#' dt <- idf$to_table(class = "Material", wide = TRUE)
#'
#' dt_to_load(dt)
#' }
#'
#' @export
# dt_to_load {{{
dt_to_load <- function (dt, string_value = TRUE) {
    assert_data_frame(dt)
    assert_names(names(dt), must.include = c("id", "class"))
    assert_flag(string_value)
    has_nm <- has_names(dt, "name")

    dt <- copy(dt)[, rleid := .I]
    id_cols <- if (has_names(dt, "name")) c("rleid", "id", "name", "class") else c("rleid", "id", "class")
    val_cols <- setdiff(names(dt), id_cols)

    if (string_value && length(val_cols)) {
        dt[, c(val_cols) := lapply(.SD, as.character), .SDcols = val_cols]
    } else if (!string_value && length(val_cols)) {
        dt[, c(val_cols) := lapply(.SD, as.list), .SDcols = val_cols]
    }

    dt <- melt.data.table(copy(dt)[, rleid := .I],
        id.vars = id_cols,
        variable.name = "field", variable.factor = FALSE
    )

    setorderv(dt, "rleid")
    dt[, index := seq.int(.N), by = "rleid"]
    set(dt, NULL, "rleid", NULL)
    setcolorder(dt, c(setdiff(id_cols, "rleid"), "index"))[]
}
# }}}

# STRING
# get_idf_string {{{
get_idf_string <- function (idd_env, idf_env, dt_order = NULL, class = NULL, object = NULL,
                            in_ip = FALSE, comment = TRUE, header = TRUE,
                            format = c("sorted", "new_top", "new_bot"),
                            leading = 4L, sep_at = 29L, flat = TRUE) {
    format <- match.arg(format)

    # IP - SI conversion
    from <- if (eplusr_option("view_in_ip")) "ip" else "si"
    to <- if (in_ip) "ip" else "si"
    temp_ip <- FALSE
    if (from != to) {
        if (in_ip) temp_ip <- TRUE

        value <- copy(idf_env$value)
        idf_env$value <- convert_value_unit(idd_env, idf_env$value, from, to)
    }

    if (any(!is.null(class), !is.null(object))) {
        obj <- get_idf_object(idd_env, idf_env, class, object, ignore_case = TRUE)
        fmt <- with_nocolor(with_format_cols(idd_env, idf_env,
            with_option(list(view_in_ip = temp_ip),
                format_idf(
                    idf_env$value[J(obj$object_id), on = "object_id"],
                    idf_env$object[J(obj$object_id), on = "object_id"],
                    dt_order, header = header, comment = comment,
                    save_format = format, leading = leading, sep_at = sep_at
                )
            )
        ))
    } else {
        fmt <- with_nocolor(with_format_cols(idd_env, idf_env,
            with_option(list(view_in_ip = temp_ip),
                format_idf(idf_env$value, idf_env$object, dt_order,
                    header = header, comment = comment, save_format = format,
                    leading = leading, sep_at = sep_at
                )
            )
        ))
    }

    if (from != to) idf_env$value <- value

    if (!flat) return(fmt)

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

    assert_string(path)
    if (!has_ext(path, "idf")) abort("'path' should have the extension of 'idf'", "idf_save_ext")

    if (file.exists(path)) {
        new_file <- FALSE
        if (!overwrite) {
            abort("Target IDF file already exists. Please set 'overwrite' to TRUE if you want to replace it.", "idf_save_exist")
        } else {
            verbose_info("Replace the existing IDF located at ", normalizePath(path), ".")
        }
    } else {
        d <- dirname(path)
        if (!dir.exists(d)) {
            tryCatch(dir.create(d, recursive = TRUE),
                warning = function (w) stop("Failed to create directory ", surround(d))
            )
        }
        new_file <- TRUE
    }

    modified <- resolve_idf_external_link(idd_env, idf_env, oldpath, path, copy_external)

    str <- get_idf_string(idd_env, idf_env, dt_order, comment = TRUE, header = TRUE, format = format)

    path <- normalizePath(path, mustWork = FALSE)
    write_lines(str, path)

    setattr(path, "path_updated", modified)
}
# }}}
# resolve_idf_external_link {{{
#  auto change full file path in `Schedule:File` and other classes to relative
#  path and copy those files into the same directory of the model
resolve_idf_external_link <- function (idd_env, idf_env, old, new, copy = TRUE) {
    if (!has_names(idf_env$object, "class_name")) {
        added <- TRUE
        add_class_name(idd_env, idf_env$object)
        on.exit(set(idf_env$object, NULL, "class_name", NULL), add = TRUE)
    }

    map <- data.table(
        class_name = c(
            "Schedule:File:Shading",
            "Schedule:File",
            "Construction:WindowDataFile",
            "ExternalInterface:FunctionalMockupUnitImport",
            "ExternalInterface:FunctionalMockupUnitImport:From:Variable",
            "ExternalInterface:FunctionalMockupUnitImport:To:Schedule",
            "ExternalInterface:FunctionalMockupUnitImport:To:Actuator",
            "ExternalInterface:FunctionalMockupUnitImport:To:Variable",
            "Table:IndependentVariable",
            "Table:Lookup"
        ),
        field_name = c(
            "File Name",
            "File Name",
            "File Name",
            "FMU File Name",
            "FMU File Name",
            "FMU File Name",
            "FMU File Name",
            "FMU File Name",
            "External File Name",
            "External File Name"
        )
    )

    if (!nrow(map <- map[class_name %chin% idf_env$object$class_name])) return(FALSE)

    # get full path of old and new
    old_dir <- normalizePath(dirname(old), mustWork = FALSE)
    new_dir <- normalizePath(dirname(new), mustWork = FALSE)

    # restore current working directory
    ori <- getwd()
    on.exit(setwd(ori), add = TRUE)
    setwd(old_dir)

    # get object table and value table
    val <- get_idf_value(idd_env, idf_env, class = map$class_name, field = map$field_name,
        property = c("units", "ip_units", "type_enum")
    )[!J(NA_character_), on = "value_chr"] # remove empty fields. See #366

    # remove empty fields. See #366
    if (!nrow(val)) return(FALSE)

    # check existence of old files
    set(val, NULL, "old_full_path", normalizePath(val$value_chr, mustWork = FALSE))
    set(val, NULL, "old_exist", file.exists(val$old_full_path))

    # stop if old file does not exist
    if (nrow(val[old_exist == FALSE])) {
        on.exit(options(warning.length = getOption("warning.length")), add = TRUE)
        options(warning.length = 8170)

        m <- paste0("  ", unlist(format_objects(val, c("class", "object", "value"), brief = FALSE)$out), collapse = "\n")

        warn(paste0("Broken external file link found in IDF:\n\n", m),
            "warning_broken_file_link"
        )
    }

    set(val, NULL, "same_dir", normalizePath(dirname(val$old_full_path), mustWork = FALSE) == new_dir)

    # find files to copy
    val <- val[old_exist == TRUE & same_dir == FALSE]

    if (!nrow(val)) return(FALSE)

    # copy external files and change values to relative paths
    if (!copy) {
        set(val, NULL, "new_value", val$old_full_path)
    # change all paths to full paths
    } else {
        set(val, NULL, "file_name", basename(val$value_chr))
        set(val, NULL, "new_value", val$file_name)

        # copy files
        to_copy <- unique(val$old_full_path)
        flag <- file.copy(to_copy, new_dir, copy.date = TRUE, overwrite = TRUE)
        if (any(!flag)) {
            invld <- val[J(to_copy[!flag]), on = c("old_full_path")]
            m <- paste0("  ", unlist(format_objects(invld, c("class", "object", "value"), brief = FALSE)$out), collapse = "\n")

            abort(paste0("Failed to copy external file into the output directory ",
                surround(new_dir), ":\n", m, collapse = "\n"
            ))
        }
    }

    # update object value table
    if (all(val$value_chr == val$new_value)) {
        FALSE
    } else {
        idf_env$value[J(val$value_id), on = "value_id", value_chr := val$new_value]
        TRUE
    }
}
# }}}

# MISC
# assign_new_id {{{
assign_new_id <- function (idf_env, dt, type = c("object", "value"), keep = FALSE) {
    type <- match.arg(type)
    col <- paste0(type, "_id")
    if (!keep) {
        set(dt, NULL, col, new_id(idf_env[[type]], col, nrow(dt)))
    } else {
        dt[is.na(get(col)), `:=`(value_id = new_id(idf_env[[type]], col, .N))]
    }
}
# }}}
# assign_idf_value_default {{{
#' Assign default field values
#'
#' @param idd_env An environment or list contains IDD tables including class,
#'        field, and reference.
#' @param idf_env An environment or list contains IDF tables including object,
#'        value, and reference.
#' @param dt_value A [data.table::data.table()] that contains object value data.
#'
#' @return The updated version of [data.table::data.table()].
#'
#' @keywords internal
#' @export
assign_idf_value_default <- function (idd_env, idf_env, dt_value) {
    cols_add <- NULL
    if (!has_names(dt_value, "default_chr")) cols_add <- "default_chr"
    if (!has_names(dt_value, "default_num")) cols_add <- c(cols_add, "default_num")
    if (!is.null(cols_add)) add_field_property(idd_env, dt_value, cols_add)

    if (in_ip_mode()) {
        dt_value <- field_default_to_unit(idd_env, dt_value, "si", "ip")
    }

    if (!has_names(dt_value, "value_chr")) set(dt_value, NULL, "value_chr", NA_character_)

    if (has_names(dt_value, "defaulted")) {
        dt_value[J(TRUE), on = "defaulted", `:=`(value_chr = default_chr, value_num = default_num)]
    } else {
        dt_value[J(NA_character_), on = "value_chr", `:=`(value_chr = default_chr, value_num = default_num)]
    }

    if (!is.null(cols_add)) set(dt_value, NULL, cols_add, NULL)

    dt_value
}
# }}}
# order_idf_data {{{
order_idf_data <- function (lst) {
    setorderv(lst$object, "object_id")
    setorderv(lst$value, c("object_id", "field_id"))

    lst
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
