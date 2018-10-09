#' @importFrom cli cat_bullet cat_line cat_rule rule symbol
#' @importFrom crayon bold cyan red strip_style underline
#' @importFrom data.table copy data.table dcast rbindlist
#' @importFrom data.table setattr setcolorder setnames setorder setorderv
#' @importFrom stringr str_detect str_match str_replace_all
#' @importFrom uuid UUIDgenerate
NULL

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
            backtick_collapse(group[!valid]), ".", call. = FALSE)
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
            backtick_collapse(index[!valid]), ".", call. = FALSE)
}
# }}}

# i_assert_valid_class_name {{{
i_assert_valid_class_name <- function (self, private, name, type = c("idd", "idf")) {
    type <- match.arg(type)
    valid <- name %in% i_class_name(self, private, type = type)
    key <- switch(type, idd = "Idd", idf = "Idf")
    if (any(!valid))
        stop("Invalid class name found in current ", key, ": ",
            backtick_collapse(name[!valid]), ".", call. = FALSE)

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
        stop("Non-extensible class found: ", backtick_collapse(class[!valid]),
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
                backtick(cls_tbl$class_name[!valid]),
                ": ", backtick(index[!valid]), ".", collapse = "\n"), call. = FALSE)
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
                backtick(cls_tbl$class_name[!valid]),
                ": ", backtick(num[!valid]), "."), call. = FALSE)
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

    if (isTRUE(lower)) fld_tbl[, `:=`(field_name = i_lower_name(field_name))]

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
    name_lc <- i_lower_name(fld_tbl$field_name)

    res_std <- match(name, name_std)
    res_lc <- match(name, name_lc)

    invalid <- is.na(res_std) & is.na(res_lc)
    if (any(invalid)) {
        stop("Invalid field name found for class ",
            backtick(i_class_name(self, private, unique(fld_tbl$class_id))),
            ": ", backtick_collapse(name[invalid]), ".", call. = FALSE)
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
        i_lower_name(res)
    else
        res
}
# }}}

# i_is_valid_field_name {{{
i_is_valid_field_name <- function (self, private, class, name) {
    assert_that(is_scalar(class))

    fld_tbl <- i_field_tbl(self, private, class)

    fld_nm <- fld_tbl$field_name

    name %in% fld_nm | name %in% i_lower_name(fld_nm)
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
        stop("Invalid field index found for class ", backtick(cls_tbl$class_name),
            ": ", backtick_collapse(index[!valid]), ".", call. = FALSE)
}
# }}}

# i_assert_valid_field_name {{{
i_assert_valid_field_name <- function (self, private, class, name) {
    valid <- i_is_valid_field_name(self, private, class, name)
    if (is_string(class))
        cls_nm <- class
    else cls_nm <- i_class_name(self, private, class)
    if (!all(valid))
        stop("Invalid field name found for class ", backtick(cls_nm),
            ": ", backtick_collapse(name[!valid]), ".", call. = FALSE)
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
                "| Class ", backtick(class_name), ".", collapse = "\n")], call. = FALSE)

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
                "| Class ", backtick(class_name), ": ", backtick(new_ext_num),
                ".", collapse = "\n")], call. = FALSE)

    assert_that(is_same_len(class, num))

    cls_tbl[, `:=`(del_ext_num = num)]
    cls_tbl[, `:=`(left_fields = num_fields - del_ext_num * num_extensible)]
    if (not_empty(cls_tbl[left_fields < last_required])) {
        stop("Failed to delete extensible groups. Number of fields left less ",
            "than required:\n",
            cls_tbl[left_fields < last_required, paste0("  ", lpad(class_rleid),
                "| Class ", backtick(class_name), ": ", backtick(left_fields),
                "(Required: ", backtick(last_required), ").", collapse = "\n")],
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
                paste0(backtick(dup$object_name), "(", dup$num, ")", collapse = ", "),
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
                paste0(backtick(dup$object_name), "(", dup$num, ")", collapse = ", "),
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
            backtick_collapse(id[!valid]), ".", call. = FALSE)
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
            backtick_collapse(name[!valid]), ".", call. = FALSE)
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
            "deleted. Class ", backtick(unique(class_name[is_required])),
            " is required object that can not be deleted.", call. = FALSE)

    is_unique <- class_name %in% i_unique_class_name(self, private)
    if (!all(is_unique)) return()

    uni_cls <- class_name[is_unique]

    obj_num <- i_object_num(self, private, uni_cls)
    if (all(obj_num == 0L)) return()

    dup_cls <- uni_cls[obj_num > 0L]

    if (not_empty(dup_cls))
        stop("In `final` validation level, existing unique objects cannot be ",
            "deleted. Class ", backtick(dup_cls), " is existing unique object ",
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
    iddobj_gen$self$private_fields$m_iddobj_generator <- iddobj_gen
    iddobj_gen$self$self$private_fields$m_iddobj_generator <- iddobj_gen

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
        "m_iddobj_generator", "m_idfobj_generator")
    for (nm in shared) {
        idfobj_gen$self$private_fields[[nm]] <- private[[nm]]
        idfobj_gen$private_fields[[nm]] <- private[[nm]]
    }

    # self reference
    idfobj_gen$self$private_fields$m_idfobj_generator <- idfobj_gen
    idfobj_gen$self$self$private_fields$m_idfobj_generator <- idfobj_gen

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
    res <- lapply(class, private$m_iddobj_generator$new)
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

    res <- lapply(cls, private$m_iddobj_generator$new)
    data.table::setattr(res, "names", cls)
    res
}
# }}}

# i_idfobject {{{
i_idfobject <- function (self, private, which) {
    obj_id <- i_object_id_from_which(self, private, which)

    obj_nm <- private$m_idf_tbl$object[J(obj_id), on = "object_id", object_name]

    res <- lapply(obj_id, private$m_idfobj_generator$new)
    data.table::setattr(res, "names", i_underscore_name(obj_nm))
    res
}
# }}}

# i_idfobject_in_class {{{
i_idfobject_in_class <- function (self, private, class) {
    obj_id <- i_object_id(self, private, class, simplify = TRUE)

    obj_nm <- private$m_idf_tbl$object[J(obj_id), on = "object_id", object_name]

    res <- lapply(obj_id, private$m_idfobj_generator$new)
    data.table::setattr(res, "names", i_underscore_name(obj_nm))
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

    res <- lapply(obj_tbl$object_id, private$m_idfobj_generator$new)
    data.table::setattr(res, "names", i_underscore_name(obj_tbl$object_name))
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
            stop("Target object(s) (ID: ", backtick_collapse(can_not_name$object_id),
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
                backtick_collapse(existing_name$object_name),
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
                rpad(backtick(obj_auto_nm$old_object_name)),
                "--> ", "Auto Assigned Name: ",
                backtick(obj_auto_nm$object_name), collapse = "\n"))
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
            backtick_collapse(unique(cls_tbl$class_name)), ".", call. = FALSE)
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
        stop("Input object has a different version ", backtick(in_ver),
             " than current Idf object (", backtick(cur_ver), ").",
             call. = FALSE)
    }

    # get the uuid to see if it comes from the same object
    in_uuid <- in_priv$m_log$uuid
    if (in_uuid == private$m_log$uuid) {
        i_verbose_info(self, private, "Object (ID:", backtick(object$id()),
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
        stop("Failed to set new value to object ID ", backtick_collapse(obj_tbl$object_id), ".", call. = FALSE)
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
            ref <- ref_by_tbl[, list(referenced_by = backtick_collapse(referenced_by_object_id)),
                by = list(object_rleid, object_id)]
            stop("Deleting an object that is referenced by others is prohibited ",
                "in `final` validation level. Failed to delete target object ",
                "[ID:", backtick_collapse(obj_tbl$object_id), "]:\n",
                paste0(paste0(ref$object_rleid, ": Object [ID:",backtick(ref$object_id),"] was ",
                        "referenced by other objects [ID:", ref$referenced_by, "]."),
                    collapse = "\n"),
                call. = FALSE)
        }

        by_id <- unique(ref_by_tbl$referenced_by_object_id)

        if (referenced) {
            i_verbose_info(self, private, "Delete target object [ID:",
                backtick_collapse(obj_id), "] and also objects [ID:",
                backtick_collapse(by_id), "] that are referencing target object.")
            obj_id <- c(obj_id, by_id)
        } else {
            i_verbose_info(self, private, "Delete target object [ID:",
                backtick_collapse(obj_id), "] which was referenced by objects ",
                "[ID: ", backtick_collapse(by_id), "]. Error may occur during ",
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
        backtick_collapse(unique(value_tbl$class_name)), ".")

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
    main <- unlist(strsplit(main, "\n", fixed = TRUE))

    # add a blank line at the end like IDFEditor
    crayon::strip_style(c(main, ""))
}
# }}}

# i_deep_clone {{{
i_deep_clone <- function (self, private, name, value) {
    if (inherits(value, "R6ClassGenerator")) {
        # clone the R6ClassGenerator
        clone_generator(value)
        if (identical(value$classname, "IdfObject")) {
            assign_idf_shared_data(self, private, value)
        } else if (identical(value$classname, "IddObject")) {
            assign_idd_shared_data(self, private, value)
        }
    } else if (inherits(value, "R6")) {
        value$clone(deep = TRUE)
    } else if (is.environment(value)) {
        list2env(as.list.environment(value, all.names = TRUE),
                 parent = emptyenv())
    } else {
        value
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
            backtick(key), ".", call. = FALSE)

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
            backtick(class_name_in), ".", collpase = "\n")]
        stop("Adding empty objects is prohibited in `final` validation ",
            "level. Empty value found in input `value`:\n", msg, call. = FALSE)
    }
    # TODO: stop when both value and comment is empty in `$set_object()`
    # if (type == "set" && !default && !no_empty) {
    #     msg <- val_in_empty[, paste0("  ", lpad(class_rleid), "| Class ",
    #         backtick(class_name_in), ".", collpase = "\n")]
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
            field_name_lower <- list(i_lower_name(unlist(field_name_in)))

            val_chr <- list(vapply(value, as.character, character(1)))

            delete <- list(vapply(value, is.na, logical(1)))
        } else {
            # check if there are empty values
            empty <- vapply(value, is_empty, logical(1))

            field_name_in <- lapply(value, function (val) replace_null(names(val)))
            field_name_lower <- lapply(field_name_in, i_lower_name)

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
            "| Class ", backtick(class_name_in), ":",
            backtick_collapse(as.character(duplicated_name)), ".", collapse = "\n")]
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
            backtick_collapse(reference[!valid]), ".", call. = FALSE)
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
            backtick_collapse(reference[!valid]), ".", call. = FALSE)
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

    msg_tbl[, which := paste0("Class ", backtick(class_name))]

    if (has_name(msg_tbl, "object_id")) msg_tbl[, object_id := as.character(object_id)]

    if (type == "add")
        msg_tbl[, `:=`(object_id = "[Temporary]")]
    else
        msg_tbl[, `:=`(object_id = backtick(object_id))]

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

    msg_tbl <- value_tbl[, list(invalid_name = backtick_collapse(field_name_in)),
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

    msg_tbl <- value_tbl[, list(invalid_ext = backtick_collapse(field_name_in)),
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
        stop("Invalid `name` found: ", backtick(name), ".", call. = FALSE)
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

# i_lower_name {{{
i_lower_name <- function (name) {
    tolower(i_underscore_name(name))
}
# }}}

# i_underscore_name {{{
i_underscore_name <- function (name) {
    nm <- gsub("[^[:alnum:]]", "_", name)
    gsub("_{2,}", "_", nm)
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
                        backtick(d), ".", call. = FALSE)
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
                "`Output:SQLite` to from", backtick(type), " to `SimpleAndTabular`.")
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
            backtick(val_tbl[old_exist == FALSE, old_full_path]), collapse = "\n"),
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
        flgs <- file.copy(to_copy$old_full_path, to_copy$new_full_path, overwrite = TRUE, copy.date = TRUE)
        if (any(!flgs)) {
            stop(paste0("Failed to copy external file into the ",
                "output directory: ", backtick(targ[!flgs]), collapse = "\n"),
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

    nm <- i_underscore_name(val_tbl$field_name)

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

    ip <- ifelse(in_ip, "ip", "")

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

################################################################################
#                                Print methods                                 #
################################################################################

# i_print_idf {{{
i_print_idf <- function (self, private, plain = FALSE) {
    if (plain) {
        cli::cat_line(i_object_string(self, private), col = "green")
    } else {
        cli::cat_rule(crayon::bold("EnergPlus Input Data File"), col = "green", line = 2)

        if (is.null(private$m_path)) path <- crayon::bold$bgRed("NOT LOCAL") else path <- backtick(private$m_path)

        cli::cat_bullet(c(
            paste0(crayon::bold("Path"), ": ", path),
            paste0(crayon::bold("Version"), ": ", backtick(private$m_version))
        ), col = "cyan", bullet_col = "cyan")

        count <- private$m_idf_tbl$object[, list(num_obj = .N), by = class_id][
            private$m_idd_tbl$class, on = "class_id", nomatch = 0L][
            private$m_idd_tbl$group, on = "group_id", nomatch = 0L][
            , list(group_name, class_name, num_obj)]

        max_num <- count[, max(num_obj)]
        count[, num_str := paste0("[", lpad(num_obj, "0"), "]")]
        count[, grp := ""]
        count[count[, .I[1L], by = list(group_name)]$V1,
            grp := crayon::bold$green(paste0("\nGroup: ", backtick((group_name)), "\n", cli::rule(), "\n"))]
        out <- count[, paste0(grp, crayon::cyan(num_str), " ", crayon::cyan(class_name))]

        cli::cat_line(out)
    }
}
# }}}

# i_print_idd {{{
i_print_idd <- function (self, private) {
    cli::cat_rule(crayon::bold("EnergyPlus Input Data Dictionary"), col = "green")
    cli::cat_bullet(c(
        paste0(crayon::bold("Version"), ": ", backtick(private$m_version)),
        paste0(crayon::bold("Build"), ": ", backtick(private$m_build)),
        paste0(crayon::bold("Total Class"), ": ", nrow(private$m_idd_tbl$class))
    ), col = "cyan", bullet_col = "cyan")
}
# }}}

# i_print_idfobj {{{
i_print_idfobj <- function (self, private, object, comment = TRUE, auto_sep = FALSE) {
    assert_that(is_scalar(object))

    obj_tbl <- i_object_tbl_from_which(self, private, object)
    val_tbl <- i_value_tbl_from_which(self, private, object)

    if (is.na(obj_tbl$object_name)) {
        cli::cat_line(
            crayon::bold$underline(paste0(
                "IdfObject <<[ID:", obj_tbl$object_id, "]>>",
                backtick(obj_tbl$class_name)
            )),
            col = "inverse"
        )
    } else {
        cli::cat_line(
            crayon::bold$underline(paste0(
                "IdfObject <<[ID:", obj_tbl$object_id, "] ", backtick(obj_tbl$object_name), ">>",
                backtick(obj_tbl$class_name)
            )),
            col = "inverse"
        )
    }

    # comment
    if (comment) {
        cmt_tbl <- i_comment_tbl_from_which(self, private, object, nomatch = 0L)
        if (not_empty(cmt_tbl)) {
            cli::cat_rule(center = crayon::bold("* COMMENTS *"), col = "green")
            cli::cat_line(crayon::italic(format_comment(cmt_tbl)), col = "cyan")
        }
    }

    if (auto_sep)
        sep_at <- max(nchar(val_tbl$value, keepNA = FALSE)) + 4L
    else
        sep_at <- 20L

    # value
    fld <- format_field(val_tbl, leading = 1L, in_ip = eplusr_option("view_in_ip"),
        sep_at = sep_at, index = TRUE, blank = TRUE, required = TRUE)

    # remove class line
    cli::cat_rule(center = crayon::green$bold("* VALUES *"), col = "green")
    cli::cat_line(fld)
    cli::cat_rule(col = "green")
}
# }}}

# i_print_iddobj {{{
i_print_iddobj <- function (self, private) {
    cls_tbl <- i_class_tbl_from_which(self, private, private$m_class_id)
    cls_tbl <- private$m_idd_tbl$group[cls_tbl, on = "group_id"]

    fld_tbl <- private$m_idd_tbl$field[class_id == private$m_class_id]

    cli::cat_line(crayon::bold$underline(paste0(
            "IddObject <<Class: ", backtick(cls_tbl$class_name), ">>")),
        col = "inverse")

    # memo
    cli::cat_rule(center = crayon::bold("* MEMO *"), col = "green")
    memo <- private$m_idd_tbl$class_memo[class_id == private$m_class_id, memo]
    if (is.na(memo)) {
        cli::cat_line("  ", crayon::italic("<No Memo>"), col = "cyan")
    } else {
        cli::cat_line("  \"", crayon::italic(memo), "\"", col = "cyan")
    }

    # property
    cli::cat_rule(center = crayon::bold("* PROPERTIES *"), col = "green")

    cli::cat_line("   ", cli::symbol$bullet, " ", c(
        paste0(crayon::bold("Group: "), backtick(cls_tbl$group_name)),
        paste0(crayon::bold("Unique: "), cls_tbl$unique_object),
        paste0(crayon::bold("Required: "), cls_tbl$required_object),
        paste0(crayon::bold("Total fields: "), cls_tbl$num_fields)
    ), col = "cyan")

    # field
    cli::cat_rule(center = crayon::bold("* FIELDS *"), col = "green")

    if (cls_tbl$num_extensible) {
        cls_tbl[, `:=`(last_extensible = first_extensible + num_extensible - 1L)]
        cls_tbl[, `:=`(num_print = max(last_required, last_extensible))]
    } else {
        cls_tbl[, `:=`(num_print = num_fields, last_extensible = 0L)]
    }

    fld_tbl <- fld_tbl[field_index <= cls_tbl$num_print]

    fld_tbl[, `:=`(idx = lpad(field_index), ext = "")]

    fld_tbl[is_extensible & field_index <= cls_tbl$last_extensible,
        `:=`(ext = crayon::bold$yellow(paste0(" <", cli::symbol$arrow_down, ">")))]

    fld_tbl[required_field == TRUE, `:=`(
        idx = crayon::red$bold(idx),
        req = crayon::red$bold(cli::symbol$bullet),
        full_name = crayon::red$bold(full_name),
        full_ipname = crayon::red$bold(full_ipname)
    )]
    fld_tbl[required_field == FALSE, `:=`(
        idx = crayon::cyan(idx),
        req = crayon::cyan(strrep(" ", nchar(cli::symbol$bullet))),
        full_name = crayon::cyan(full_name),
        full_ipname = crayon::cyan(full_ipname)
    )]

    if (eplusr_option("view_in_ip"))
        fld_tbl[, cli::cat_line("  ", req, idx, ": ", full_ipname, ext)]
    else
        fld_tbl[, cli::cat_line("  ", req, idx, ": ", full_name, ext)]

    if (cls_tbl$num_extensible) cli::cat_line("  ......", col = "cyan")
    cli::cat_rule(col = "green")
}
# }}}
