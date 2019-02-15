#' @importFrom cli cat_bullet cat_line cat_rule rule symbol
#' @importFrom crayon bold cyan red strip_style underline
#' @importFrom data.table copy data.table dcast rbindlist
#' @importFrom data.table setattr setcolorder setnames setorder setorderv
#' @importFrom stringi stri_locate_first_regex stri_replace_first_regex "stri_sub<-"
#' @importFrom stringi stri_subset_regex stri_match_first_regex stri_rand_strings
#' @include parse.R
#' @include clone.R
NULL

# IDF OBJECT
# set_idfobj_comment {{{
set_idfobj_comment <- function (idf_env, object, comment, append = TRUE, width = 0L) {
    assert(is_scalar(object))
    obj <- get_idf_object(idf_env, object = object)

    if (is.null(comment)) {
        set(obj, NULL, "comment", list(list(NULL)))
    } else {
        assert(is.character(comment))
        assert(is_count(width))

        cmt <- strsplit(comment, "\n", fixed = TRUE)

        if (width != 0L) {
            cmt <- strwrap(cmt, width = width)
        }

        if (is.null(append)) {
            # reset
            set(obj, NULL, "comment", list(list(cmt)))
        } else {
            assert(is_flag(append))
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
                              simplify = FALSE, in_ip = FALSE) {
    val <- get_idf_value(idd_env, idf_env, NULL, object, which, NULL, fill = all)

    browser()
    if (i_need_update_num(self, private, view_in_ip = in_ip))
        val_tbl <- update_value_num(val_tbl, in_ip = in_ip)

    if (simplify) return(val_tbl$value)

    val <- value_list(val_tbl, in_ip = in_ip)

    nm <- underscore_name(val_tbl$field_name)

    data.table::setattr(val, "names", nm)

    val
}
# }}}
# set_idfobj_value {{{
set_idfobj_value <- function (self, private, object, ..., default = TRUE) {
    nm <- if (is_count(object)) paste0("..", object) else object
    input <- list(list(...))
    setattr(input, "names", nm)
    set_idf_object(idd_env, idf_env, input, .default = default)
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
