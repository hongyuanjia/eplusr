#' @importFrom data.table data.table rbindlist setattr setnames
#' @importFrom purrr map_int map2_lgl map_lgl iwalk
#' @importFrom cli cat_line cat_bullet
#' @importFrom clisymbols symbol
NULL

# i_init_validate{{{
i_init_validate <- function (self, private) {
    private$m_log$validate <- list()
    data.table::setattr(private$m_log$validate, "class", c("IdfValidity", "list"))
}
# }}}
# i_validate_in_object {{{
i_validate_in_object <- function (self, private, object_tbl, value_tbl, temp = TRUE) {
    i_init_validate(self, private)

    if (eplusr_option("validate_level") == "none") return(private$m_log$validate)

    input <- new.env(parent = emptyenv(), size = 3L)
    input$type <- ifelse(temp, "add_object", "set_object")
    input$object_tbl <- data.table::copy(object_tbl)
    input$value_tbl <- data.table::copy(value_tbl)

    if (temp) {
        input$object_tbl[, `:=`(object_id = paste0(" (Temporary ", object_rleid, ")"))]
        input$value_tbl[, `:=`(object_id = paste0(" (Temporary ", object_rleid, ")"))]
    }

    i_check_conflict_name(self, private, input)
    i_check_missing(self, private, input)
    i_check_autosize(self, private, input)
    i_check_autocalculate(self, private, input)
    i_exclude_valid_auto(self, private, input)
    i_exclude_empty_field(self, private, input)
    i_check_numeric(self, private, input)
    i_check_integer(self, private, input)
    i_check_choice(self, private, input)
    i_check_range(self, private, input)
    i_check_reference(self, private, input)

    data.table::setattr(private$m_log$validate, "class", c("IdfValidity", "list"))
}
# }}}
# i_validate_idfobject {{{
i_validate_idfobject <- function (self, private, object) {
    i_init_validate(self, private)

    if (eplusr_option("validate_level") == "none") return(private$m_log$validate)

    input <- new.env(parent = emptyenv(), size = 3L)
    input$type <- "idfobject"
    input$object_tbl <- i_object_tbl_from_which(self, private, object)
    input$value_tbl <- i_value_tbl_from_which(self, private, object)
    input$value_tbl[, `:=`(class_name = i_class_name(self, private, class_id))]

    i_check_conflict_name(self, private, input)
    i_check_missing(self, private, input)
    i_check_autosize(self, private, input)
    i_check_autocalculate(self, private, input)
    i_exclude_valid_auto(self, private, input)
    i_exclude_empty_field(self, private, input)
    i_check_numeric(self, private, input)
    i_check_integer(self, private, input)
    i_check_choice(self, private, input)
    i_check_range(self, private, input)
    i_check_reference(self, private, input)

    data.table::setattr(private$m_log$validate, "class", c("IdfValidity", "list"))

    private$m_log$validate
}
# }}}
# i_validate_idf {{{
i_validate_idf <- function (self, private) {
    private$m_log$validate <- list()
    data.table::setattr(private$m_log$validate, "class", c("IdfValidity", "list"))

    if (eplusr_option("validate_level") == "none")
        return(private$m_log$validate)

    input <- new.env(parent = emptyenv(), size = 3L)
    input$type <- "idf"
    input$object_tbl <- i_object_tbl_from_class(self, private)
    input$value_tbl <- i_value_tbl_from_which(self, private)
    input$value_tbl[, `:=`(class_name = i_class_name(self, private, class_id))]

    i_check_missing_object(self, private, input)
    i_check_duplicate_object(self, private, input)
    i_check_conflict_name(self, private, input)
    i_check_missing(self, private, input)
    i_check_autosize(self, private, input)
    i_check_autocalculate(self, private, input)
    i_exclude_valid_auto(self, private, input)
    i_exclude_empty_field(self, private, input)
    i_check_numeric(self, private, input)
    i_check_integer(self, private, input)
    i_check_choice(self, private, input)
    i_check_range(self, private, input)
    i_check_reference(self, private, input)

    data.table::setattr(private$m_log$validate, "class", c("IdfValidity", "list"))

    private$m_log$validate
}
# }}}

# i_is_valid_idf: return true if no validity error found {{{
i_is_valid_idf <- function (self, private) {
    i_validate_idf(self, private)
    all(purrr::map_int(private$m_log$validate, is_empty))
}
# }}}
# i_is_valid_idfobject: return true if no validity error found {{{
i_is_valid_idfobject <- function (self, private, object) {
    i_validate_idfobject(self, private, object)
    all(purrr::map_int(private$m_log$validate, is_empty))
}
# }}}

# i_check_missing_object: check missing required objects {{{
i_check_missing_object <- function (self, private, input) {
    if (eplusr_option("validate_level") != "final")
        return(private$m_log$validate$missing_object <- list())

    exist <- unique(i_class_name(self, private, type = "idf"))
    required <- i_required_class_name(self, private)
    private$m_log$validate$missing_object <- required[!required %in% exist]
}
# }}}
# i_check_duplicate_object: check duplicated unique objects {{{
i_check_duplicate_object <- function (self, private, input) {
    if (eplusr_option("validate_level") != "final")
        return(private$m_log$validate$duplicate_object <- list())

    dup_uni <- input$object_tbl[unique_object == TRUE, list(num = .N),
        by = list(class_id)][num > 1L]

    if (not_empty(dup_uni))
        private$m_log$validate$duplicate_object <-
            input$value_tbl[dup_uni, on = "class_id"]
}
# }}}
# i_check_conflict_name: objects in the same class have exact the same name {{{
i_check_conflict_name <- function (self, private, input) {
    if (eplusr_option("validate_level") != "final")
        return(private$m_log$validate$conflict_name <- list())

    ori_obj_tbl <- input$object_tbl[!is.na(object_name)]
    obj_tbl <- ori_obj_tbl

    if (is_empty(obj_tbl)) return()

    if (input$type != "idf") {
        cls_id <- unique(obj_tbl$class_id)
        exist <- i_is_valid_class_index(self, private, cls_id, type = "idf")
        # add existing object table
        if (any(exist)) {
            obj_tbl_other <- i_object_tbl_from_class(self, private, cls_id[exist])

            if (input$type != "add_object") {
                obj_tbl_other <- obj_tbl_other[!J(obj_tbl$object_id), on = "object_id"]
            }
            obj_tbl <- data.table::rbindlist(list(obj_tbl, obj_tbl_other), fill = TRUE)
        }
    }

    conf_id <- obj_tbl[, list(num = .N, id_list = list(object_id)),
        by = list(class_id, object_name_upper)][num > 1L, unlist(id_list)]

    if (is_empty(conf_id)) return()

    other_id <- conf_id[!conf_id %in% ori_obj_tbl$object_id]

    val_tbl <- data.table::rbindlist(list(
        i_value_tbl_from_which(self, private, as.integer(other_id))[,
            `:=`(class_name = i_class_name(self, private, class_id))],
        input$value_tbl[J(conf_id), on = "object_id", nomatch = 0L]),
    fill = TRUE)

    private$m_log$validate$conflict_name <- val_tbl
}
# }}}
# i_check_missing: missing required fields {{{
i_check_missing <- function (self, private, input) {
    missing <- input$value_tbl[required_field == TRUE & is.na(value)]

    if (not_empty(missing)) {
        private$m_log$validate$missing <- missing
        input$value_tbl <- input$value_tbl[!missing, on = "value_id"]
    }
}
# }}}
# i_exclude_empty_field: exclude non-required empty fields {{{
i_exclude_empty_field <- function (self, private, input) {
    input$value_tbl <- input$value_tbl[!(required_field == FALSE & is.na(value))]
}
# }}}
# i_check_autosize: invalid autosize fields {{{
i_check_autosize <- function (self, private, input) {
    invalid_autosize <- input$value_tbl[
        value_upper == "AUTOSIZE" & autosizable == FALSE]

    if (is_empty(invalid_autosize))
        return(private$m_log$validate$autosize <- list())

    private$m_log$validate$autosize <- invalid_autosize
    input$value_tbl <- input$value_tbl[!invalid_autosize, on = "value_id"]
}
# }}}
# i_check_autocalculate: invalid autocalculate fields {{{
i_check_autocalculate <- function (self, private, input) {
    invalid_autocalculate <- input$value_tbl[
        value_upper == "AUTOCALCULATE" & autocalculatable == FALSE]

    if (is_empty(invalid_autocalculate))
        return(private$m_log$validate$autocalculate <- list())

    private$m_log$validate$autocalculate <- invalid_autocalculate
    input$value_tbl <- input$value_tbl[!invalid_autocalculate, on = "value_id"]
}
# }}}
# i_exclude_valid_auto: exclude valid autosize and autocalculate fields {{{
i_exclude_valid_auto <- function (self, private, input) {
    input$value_tbl <- input$value_tbl[
        !(value_upper == "AUTOSIZE" & autosizable == TRUE) &
        !(value_upper == "AUTOCALCULATE" & autocalculatable == TRUE)]
}
# }}}
# i_check_numeric: invalid numeric fields {{{
i_check_numeric <- function (self, private, input) {
    invalid_numeric <- input$value_tbl[
        type %in% c("real", "integer") & is.na(value_num)]

    if (is_empty(invalid_numeric))
        return(private$m_log$validate$numeric <- list())

    private$m_log$validate$numeric <- invalid_numeric
    input$value_tbl <- input$value_tbl[!invalid_numeric, on = "value_id"]
}
# }}}
# i_check_integer: invalid integer fields {{{
i_check_integer <- function (self, private, input) {
    invalid_integer <- input$value_tbl[
        type %in% c("integer") & is.na(value_num)]

    if (is_empty(invalid_integer))
        return(private$m_log$validate$integer <- list())

    private$m_log$validate$integer <- invalid_integer
    input$value_tbl <- input$value_tbl[!invalid_integer, on = "value_id"]
}
# }}}
# i_check_choice: invalid choice fields {{{
i_check_choice <- function (self, private, input) {
    val_tbl <- input$value_tbl[type == "choice"]

    if (is_empty(val_tbl))
        return(private$m_log$validate$choice <- list())

    # have to handle class "Schedule:Week:Compact" seperately
    val_tbl[class_name == "Schedule:Week:Compact",
        `:=`(value_upper = gsub("^FOR\\s*[:]{0,1}\\s*", "", value_upper))]

    invalid_choice <- private$m_idd_tbl$field_choice[val_tbl,
        on = c("field_id", choice_upper = "value_upper")][is.na(choice_id)]

    if (is_empty(invalid_choice))
        return(private$m_log$validate$choice <- list())

    private$m_log$validate$choice <- invalid_choice
    input$value_tbl <- input$value_tbl[!invalid_choice, on = "value_id"]
}
# }}}
# i_check_range: invalid range fields {{{
i_check_range <- function (self, private, input) {
    val_tbl <- input$value_tbl[type %in% c("integer", "real") & has_range == TRUE]

    if (is_empty(val_tbl))
        return(private$m_log$validate$range <- list())

    invalid_range <- private$m_idd_tbl$field_range[val_tbl, on = "field_id"][
        lower_incbounds == TRUE,  check_lower := value_num >= minimum][
        lower_incbounds == FALSE, check_lower := value_num > minimum][
        upper_incbounds == TRUE,  check_upper := value_num <= maximum][
        upper_incbounds == FALSE, check_upper := value_num < maximum][
        check_lower == FALSE | check_upper == FALSE]

    if (is_empty(invalid_range))
        return(private$m_log$validate$range <- list())

    private$m_log$validate$range <- invalid_range
    input$value_tbl <- input$value_tbl[!invalid_range, on = "value_id"]
}
# }}}
# i_check_reference: invalid reference fields {{{
i_check_reference <- function (self, private, input) {
    if (eplusr_option("validate_level") != "final") return()

    val_tbl <- input$value_tbl[has_object_list == TRUE]

    if (is_empty(val_tbl))
        return(private$m_log$validate$reference <- list())

    # TODO: empty values should not be present
    invalid_ref_empty <- val_tbl[is.na(value)]

    # only check non-empty values
    val_tbl_obj_list <- val_tbl[!is.na(value)][
        private$m_idd_tbl$field_object_list, on = "field_id", nomatch = 0L]

    # get unique object-list
    uni_obj_list <- val_tbl_obj_list[, unique(object_list)]

    # get possible values
    val_tbl_ref <- i_value_tbl_from_object_list(self, private, uni_obj_list)[
        val_tbl_obj_list, on = "object_list"][,
        lapply(.SD, function (x) list(unlist(x))),
        .SDcols = c("possible_value_upper"),
        by = list(value_id, value_upper)]

    invalid_ref_id <- val_tbl_ref[
        !purrr::map2_lgl(value_upper, possible_value_upper, `%in%`), value_id]
    invalid_ref_non_empty <- input$value_tbl[J(invalid_ref_id), on = "value_id"]

    # combine
    invalid_ref <- data.table::rbindlist(list(invalid_ref_empty, invalid_ref_non_empty))

    if (is_empty(invalid_ref))
        return(private$m_log$validate$reference <- list())

    private$m_log$validate$reference <- invalid_ref
    input$value_tbl <- input$value_tbl[!invalid_ref, on = "value_id"]
}
# }}}

# i_print_validate: print all validate results {{{
i_print_validate <- function (validate) {
    empty <- vapply(validate, is_empty, logical(1))
    if (all(empty)) {
        cli::cat_line(" ", clisymbols::symbol$tick, " ", "No error found.")
    } else {
        error_num <- sum(purrr::imap_int(validate[!empty],
                ~{
                    if (.y == "conflict_name") length(unique(.x$object_id))
                    else nrow(.x)
                }))
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
    if (type == "conflict_name") {
        error_num <- length(unique(res$object_id))
    } else {
        error_num <- nrow(res)
    }
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

#' @export
# print.IdfValidity {{{
print.IdfValidity <- function (x, ...) {
    i_print_validate(x)
}
# }}}
