#' @importFrom data.table data.table rbindlist setattr setnames
#' @importFrom purrr map_int map2_lgl map_lgl iwalk
#' @importFrom cli cat_line cat_bullet
#' @importFrom clisymbols symbol
# i_collect_validate: collect_results validate results {{{
i_collect_validate <- function (private) {
    private$m_validate <- list()

    # for "none" level, nothing will be checked
    if (private$m_options$validate_level == "none") {
        return(private$m_validate)
    }

    # prepare check input data
    input <- new.env(parent = emptyenv(), size = 5L)

    input$cols <- c("value_id", "value", "object_id", "class_id", "class_name",
                    "field_order", "full_name", "full_ipname")
    input$is_idfobj <- ifelse(not_empty(private$m_object_id), TRUE, FALSE)
    input$is_set <- ifelse(input$is_idfobj && not_empty(private$m_temp$value_to_set), TRUE, FALSE)

    if (private$m_options$validate_level == "draft") {
        # get input
        if (input$is_idfobj) {
            if (input$is_set) {
                input$value_tbl <- private$m_temp$value_to_set
            } else {
                input$value_tbl <- private$value_tbl(with_field = TRUE)
            }
        } else {
            input$value_tbl <- private$value_tbl()
        }

        i_exclu_empty(private, input)
        i_check_autosize(private, input)
        i_check_autocalculate(private, input)
        i_exclu_auto(private, input)
        i_check_numeric(private, input)
        i_check_integer(private, input)
        i_check_choice(private, input)
    } else if (private$m_options$validate_level == "final"){
        # get input
        if (input$is_idfobj) {
            input$object_tbl <- private$object_tbl()
            if (input$is_set) {
                input$value_tbl <- private$m_temp$value_to_set
            } else {
                input$value_tbl <- private$value_tbl(with_field = TRUE)
            }
        } else {
            input$object_tbl <- private$object_tbl(all = TRUE)
            input$value_tbl <- private$value_tbl()
        }

        # only for Idf object
        if (!input$is_idfobj) {
            i_check_missing_object(private, input)
            i_check_duplicate_object(private, input)
            i_exclu_class(private, input)
        }

        i_check_conflict_name(private, input)

        i_check_missing(private, input)
        i_exclu_empty(private, input)
        i_check_autosize(private, input)
        i_check_autocalculate(private, input)
        i_exclu_auto(private, input)
        i_check_numeric(private, input)
        i_check_integer(private, input)
        i_check_choice(private, input)
        i_check_reference(private, input)

        data.table::setattr(private$m_validate, "class", c("IdfValidity", "list"))
    }
}
# }}}
# i_is_valid: return true if no validity error found {{{
i_is_valid <- function (private) {
    i_collect_validate(private)
    all(purrr::map_int(private$m_validate, is_empty))
}
# }}}
# i_check_missing_object: check missing required objects {{{
i_check_missing_object <- function (private, input) {
    private$m_validate$missing_object <- input$object_tbl[
        required_object == TRUE & is.na(object_id), list(class_name)]

}
# }}}
# i_exclu_class: exclude non-useful class data {{{
i_exclu_class <- function (private, input) {
    input$object_tbl <- input$object_tbl[!is.na(object_id)]
}
# }}}
# i_check_duplicate_object: check duplicated unique objects {{{
i_check_duplicate_object <- function (private, input) {
    ids <- input$object_tbl[unique_object == TRUE,
        list(num = .N, object_id = list(object_id)),
        by = list(class_id)][num > 1L, object_id]
    if (is_empty(ids)) private$m_validate$duplicate_object <- data.table::data.table()
    private$m_validate$duplicate_object <- input$value_tbl[object_id %in% unlist(ids),
       .SD, .SDcols = input$cols]
}
# }}}
# i_check_conflict_name: objects in the same class have exact the same name {{{
i_check_conflict_name <- function (private, input) {
    cls_id <- input$object_tbl[has_name == TRUE, class_id]
    if (not_empty(cls_id)) {
        all_ids <- private$m_idf_tbl$object[class_id %in% cls_id, object_id]
        fld_id <- private$m_idd_tbl$field[
            class_id %in% cls_id & field_order == 1L, field_id]
        obj_id <- private$m_idf_tbl$value[field_id %in% fld_id][
            private$m_idf_tbl$object, on = "object_id", nomatch = 0L][,
            list(num = .N, object_id = list(object_id)),
            by = list(class_id, value_upper)][num > 1L, object_id]
        if (not_empty(obj_id)) {
            private$m_validate$conflict_name <- input$value_tbl[object_id %in% unlist(ids),
                .SD, .SDcols = input$cols]
        }
    }
}
# }}}
# i_check_missing: missing required fields {{{
i_check_missing <- function (private, input) {
    private$m_validate$missing <- input$value_tbl[
        required_field == TRUE & (is.na(value) | value == ""),
        .SD, .SDcols = input$cols]

    if (not_empty(private$m_validate$missing)) {
        input$value_tbl <- input$value_tbl[
            !value_id %in% private$m_validate$missing[["value_id"]]]
    }
}
# }}}
# i_exclu_empty: exclude non-required empty fields {{{
i_exclu_empty <- function (private, input) {
    input$value_tbl <- input$value_tbl[!(required_field == FALSE & value == "")]
}
# }}}
# i_check_autosize: invalid autosize fields {{{
i_check_autosize <- function (private, input) {
    private$m_validate$autosize <- input$value_tbl[
        value_upper == "AUTOSIZE" & autosizable == FALSE,
        .SD, .SDcol = input$cols]
    if (not_empty(private$m_validate$autosize)) {
        input$value_tbl <- input$value_tbl[
            !value_id %in% private$m_validate$autosize[["value_id"]]]
    }
}
# }}}
# i_check_autocalculate: invalid autocalculate fields {{{
i_check_autocalculate <- function (private, input) {
    private$m_validate$autocalculate <- input$value_tbl[
        value_upper == "AUTOCALCULATE" & autocalculatable == FALSE,
        .SD, .SDcol = input$cols]
    if (not_empty(private$m_validate$autocalculate)) {
        input$value_tbl <- input$value_tbl[
            !value_id %in% private$m_validate$autocalculate[["value_id"]]]
    }
}
# }}}
# i_exclu_auto: exclude valid autosize and autocalculate fields {{{
i_exclu_auto <- function (private, input) {
    input$value_tbl <- input$value_tbl[
        !(value_upper == "AUTOSIZE" & autosizable == TRUE) &
        !(value_upper == "AUTOCALCULATE" & autocalculatable == TRUE)]
}
# }}}
# i_check_numeric: invalid numeric fields {{{
i_check_numeric <- function (private, input) {
    private$m_validate$numeric <- input$value_tbl[
        type %in% c("real", "integer") & is.na(value_num),
        .SD, .SDcols = input$cols]
    if (not_empty(private$m_validate$numeric)) {
        input$value_tbl <- input$value_tbl[
            !value_id %in% private$m_validate$numeric[["value_id"]]]
    }
}
# }}}
# i_check_integer: invalid integer fields {{{
i_check_integer <- function (private, input) {
    targ <- input$value_tbl[type == "integer"]
    if (private$m_options$view_in_ip) {
        private$m_validate$integer <- targ[!is_integerish(value_ipnum),
            .SD, .SDcols = input$cols, by = value_id]
    } else {
        private$m_validate$integer <- targ[!is_integerish(value_num),
            .SD, .SDcols = input$cols, by = value_id]
    }
    if (not_empty(private$m_validate$integer)) {
        input$value_tbl <- input$value_tbl[!value_id %in% private$m_validate$integer[["value_id"]]]
    }
}
# }}}
# i_check_choice: invalid choice fields {{{
i_check_choice <- function (private, input) {
    private$m_validate$choice <- input$value_tbl[type == "choice"][
        private$m_idd_tbl$field_choice[
            , lapply(.SD, list), .SDcol = c("choice_upper", "choice"), by = field_id
        ], on = "field_id", nomatch = 0L][
        !purrr::map2_lgl(value_upper, choice_upper, `%in%`),
        .SD, .SDcol = c(input$cols, "choice")]
    if (not_empty(private$m_validate$choice)) {
        input$value_tbl <- input$value_tbl[
            !value_id %in% private$m_validate$choice[["value_id"]]]
    }
}
# }}}
# i_check_range: invalid range fields {{{
i_check_range <- function (private, input) {
    private$m_validate$range <- input$value_tbl[type %in% c("integer", "real")][
        private$m_idd_tbl$field_range, on = "field_id"][
        lower_incbounds == TRUE,  check_lower := value_num >= minimum][
        lower_incbounds == FALSE, check_lower := value_num > minimum][
        upper_incbounds == TRUE,  check_upper := value_num <= maximum][
        upper_incbounds == FALSE, check_upper := value_num < maximum][
        check_lower == FALSE | check_upper == FALSE,
        .SD, .SDcol = c(input$cols,
            c("minimum", "lower_incbounds", "maximum", "upper_incbounds"))
        ]
    if (not_empty(private$m_validate$range)) {
        input$value_tbl <- input$value_tbl[
            !value_id %in% private$m_validate$range[["value_id"]]]
    }
}
# }}}
# i_check_reference: invlaid reference fields {{{
i_check_reference <- function (private, input) {
    val_obj_list <- input$value_tbl[private$m_idd_tbl$field_object_list,
        on = "field_id", nomatch = 0L][
        , .SD, .SDcols = c(input$cols, "value_upper", "object_list")]
    if (not_empty(val_obj_list)) {
        val_fld_ref <- private$m_idd_tbl$field_reference[
            reference %in% val_obj_list$object_list][
            private$m_idf_tbl$value, on = "field_id", nomatch = 0L][,
            lapply(.SD, list), .SDcols = c("value", "value_upper"),
            by = reference]

        if (not_empty(private$m_idd_tbl$class_reference)) {
            val_cls_ref <- private$m_idd_tbl$class_reference[
                reference %in% val_obj_list$object_list][
                private$m_idd_tbl$class, on = "class_id", nomatch = 0L][,
                value_upper := toupper(class_name)][,
                lapply(.SD, list), .SDcols = c("class_name", "value_upper"),
                by = reference]
            data.table::setnames(val_cls_ref, "class_name", "value")
            val_ref <- data.table::rbindlist(list(val_fld_ref, val_cls_ref), fill = TRUE)
        } else {
            val_ref <- val_fld_ref
        }

        data.table::setnames(val_ref,
            c("value", "value_upper"),
            c("value_reference", "value_reference_upper"))
        private$m_validate$reference <- val_ref[
            val_obj_list, on = c(reference = "object_list")][
            , lapply(.SD, function (x) list(unlist(x))),
            .SDcol = c("value_reference", "value_reference_upper"),
            by = c(input$cols, "value_upper")][
            !purrr::map2_lgl(value_upper, value_reference_upper, `%in%`),
            .SD, .SDcols = c(input$cols, "value_reference")]

        if (not_empty(private$m_validate$reference)) {
            input$value_tbl <- input$value_tbl[
                !value_id %in% private$m_validate$reference[["value_id"]]]
        }
    }
}
# }}}
# i_print_validate: print all validate results {{{
i_print_validate <- function (validate) {
    empty <- purrr::map_lgl(validate, is_empty)
    if (all(empty)) {
        cli::cat_line(" ", clisymbols::symbol$tick, " ", "No error found.")
    } else {
        error_num <- sum(purrr::map_int(validate[!empty], nrow))
        cli::cat_line(" ", clisymbols::symbol$cross, " [", error_num, "] ",
                      "Errors found during validation.")
        cli::cat_rule(line = 2)
        purrr::iwalk(validate[!empty], ~i_print_single_validate(.x, .y))
        cli::cat_line()
    }
}
# }}}
# i_print_single_validate: print a single validate result {{{
i_print_single_validate <- function (res, type) {
    error_num <- nrow(res)
    index <- res[["field_order"]]

    title <- switch(type,
        missing_object = "Missing Required Object",
        duplicate_object = "Duplicated Unique Object",
        conflict_name = "Conflicted Object Names",
        missing = "Missing Required Field",
        autosize = "Invalid Autosize Field",
        autocalculate = "Invalid Autocalculate Field",
        numeric = "Invalid Number",
        character = "Invalid Character",
        integer = "Invalid Integer",
        choice = "Invalid Choice",
        range = "Range Exceeding",
        reference = "Invalid Reference")

    bullet <- switch(type,
        missing_object = "Objects below are required but not exist.",
        duplicate_object = "Objects should be unique but have multiple instances.",
        conflict_name = "Objects below have the same name.",
        missing = "Fields below are required but values are not given.",
        autosize = "Fields below cannot be `autosize`.",
        autocalculate = "Fields below cannot be `autocalculate`.",
        numeric = "Fields below should be numbers but are not.",
        character = "Fields below should be characters but are not.",
        integer = "Fields below are not or cannot be coerced into integers.",
        choice = "Fields below are not one of prescribed choices.",
        range = "Fields below exceed prescibed ranges.",
        reference = "Fields below are not one of valid references.")

    cli::cat_line()
    cli::cat_rule(paste0("[", error_num, "] ", title))
    cli::cat_bullet(bullet, bullet = "circle_cross")
    if (type == "missing_object") {
        cli::cat_bullet(backtick(res[["class_name"]]))
    } else {
        cli::cat_line(format_objects(res))
    }
}
# }}}
# print.IdfValidity {{{
print.IdfValidity <- function (x, ...) {
    i_print_validate(x)
}
# }}}
