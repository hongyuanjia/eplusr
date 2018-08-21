#' @importFrom cli cat_bullet cat_line cat_rule symbol
#' @importFrom data.table copy data.table rbindlist setattr setnames
NULL

# i_empty_check_dt {{{
i_empty_check_dt <- function () {
    data.table::data.table(
        object_id = integer(),
        class_id = integer(),
        class_name = character(),
        field_index = integer(),
        field_name = character(),
        full_name = character(),
        full_ipname = character(),
        type = character(),
        value_id = character(),
        value = character(),
        value_upper = character(),
        value_num = character(),
        value_ipnum = character()
    )
}
# }}}
# i_init_validate{{{
i_init_validate <- function (self, private) {
    private$m_log$validate <- list()

    private$m_log$validate$missing_object <- character()

    chks <- c("duplicate_object", "conflict_name",
      "missing_value", "invalid_autosize", "invalid_autocalculate",
      "invaid_character", "invalid_numeric", "invalid_integer",
      "invalid_choice", "invalid_range", "invalid_reference")

    for (chk in chks) {
        private$m_log$validate[[chk]] <- i_empty_check_dt()
    }

    data.table::setattr(private$m_log$validate, "class", c("IdfValidity", "list"))
}
# }}}
# i_count_check_error {{{
i_count_check_error <- function (validate, type) {
    if (identical(type, "missing_object"))
        return(length(validate[[type]]))

    if (identical(type, "duplicate_object") || identical(type, "conflict_name"))
        return(length(unique(validate[[type]]$object_id)))

    if (identical(type, "incomplete_extensible"))
        return(validate[[type]][is.na(value), .N])

    nrow(validate[[type]])
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
    i_check_incomplete_extensible(self, private, input)
    i_check_missing_field(self, private, input)
    i_check_invalid_autosize(self, private, input)
    i_check_invalid_autocalculate(self, private, input)
    i_exclude_valid_auto(self, private, input)
    i_exclude_empty_field(self, private, input)
    i_check_invalid_character(self, private, input)
    i_check_invalid_numeric(self, private, input)
    i_check_invalid_integer(self, private, input)
    i_check_invalid_choice(self, private, input)
    i_check_invalid_range(self, private, input)
    i_check_invalid_reference(self, private, input)

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
    i_check_incomplete_extensible(self, private, input)
    i_check_missing_field(self, private, input)
    i_check_invalid_autosize(self, private, input)
    i_check_invalid_autocalculate(self, private, input)
    i_exclude_valid_auto(self, private, input)
    i_exclude_empty_field(self, private, input)
    i_check_invalid_character(self, private, input)
    i_check_invalid_numeric(self, private, input)
    i_check_invalid_integer(self, private, input)
    i_check_invalid_choice(self, private, input)
    i_check_invalid_range(self, private, input)
    i_check_invalid_reference(self, private, input)

    data.table::setattr(private$m_log$validate, "class", c("IdfValidity", "list"))

    private$m_log$validate
}
# }}}
# i_validate_idf {{{
i_validate_idf <- function (self, private) {
    i_init_validate(self, private)

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
    i_check_incomplete_extensible(self, private, input)
    i_check_missing_field(self, private, input)
    i_check_invalid_autosize(self, private, input)
    i_check_invalid_autocalculate(self, private, input)
    i_exclude_valid_auto(self, private, input)
    i_exclude_empty_field(self, private, input)
    i_check_invalid_character(self, private, input)
    i_check_invalid_numeric(self, private, input)
    i_check_invalid_integer(self, private, input)
    i_check_invalid_choice(self, private, input)
    i_check_invalid_range(self, private, input)
    i_check_invalid_reference(self, private, input)

    data.table::setattr(private$m_log$validate, "class", c("IdfValidity", "list"))

    private$m_log$validate
}
# }}}

# i_is_valid_idf: return true if no validity error found {{{
i_is_valid_idf <- function (self, private) {
    i_validate_idf(self, private)
    all(vapply(private$m_log$validate, is_empty, logical(1)))
}
# }}}
# i_is_valid_idfobject: return true if no validity error found {{{
i_is_valid_idfobject <- function (self, private, object) {
    i_validate_idfobject(self, private, object)
    all(vapply(private$m_log$validate, is_empty, logical(1)))
}
# }}}

# i_check_missing_object: check missing required objects {{{
i_check_missing_object <- function (self, private, input) {
    if (eplusr_option("validate_level") != "final") return()

    exist <- unique(i_class_name(self, private, type = "idf"))
    required <- i_required_class_name(self, private)
    private$m_log$validate$missing_object <- required[!required %in% exist]
}
# }}}
# i_check_duplicate_object: check duplicated unique objects {{{
i_check_duplicate_object <- function (self, private, input) {
    if (eplusr_option("validate_level") != "final") return()

    dup_uni <- input$object_tbl[unique_object == TRUE, list(num = .N),
        by = list(class_id)][num > 1L]

    if (not_empty(dup_uni))
        private$m_log$validate$duplicate_object <-
            input$value_tbl[dup_uni, on = "class_id", .SD, .SDcols = names(i_empty_check_dt())]
}
# }}}
# i_check_conflict_name: objects in the same class have exact the same name {{{
i_check_conflict_name <- function (self, private, input) {
    if (eplusr_option("validate_level") != "final") return()

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

    private$m_log$validate$conflict_name <- val_tbl[, .SD, .SDcols = names(i_empty_check_dt())]
}
# }}}
# i_check_incomplete_extensible: incomplete extensible group {{{
i_check_incomplete_extensible <- function (self, private, input) {
    if (eplusr_option("validate_level") != "final") return()

    # get all extensible groups
    ext_tbl <- input$value_tbl[is_extensible == TRUE]
    if (is_empty(ext_tbl)) return()

    if (input$type == "add_object") {
        invalid_obj <- ext_tbl[is.na(value), unique(object_rleid)]
    } else {
        # get extensible group num per object
        ext_info <- input$object_tbl[
            ext_tbl[, list(last_index = max(field_index)), by = list(object_id)],
            on = list(object_id),
            list(object_id, first_extensible, num_extensible, last_index)][,
            `:=`(num_extensible_group = ((last_index - first_extensible) + 1L) %/% num_extensible)
        ]

        # get extensible group index and field index
        ext_index <- ext_info[,
            list(ext_index = rep(
                    seq(unique(first_extensible - 1L), length.out = num_extensible_group),
                    each = unique(num_extensible)
                 ),
                 field_index = seq(first_extensible, last_index)
            ),
            by = list(object_id)
        ]

        ext_tbl <- ext_tbl[ext_index, on = c("object_id", "field_index")]

        ext_na <- ext_tbl[order(object_id, -ext_index, -field_index),
            list(object_id, field_index, ext_index, value)][
            , list(has_any_na = any(is.na(value)), is_all_na = all(is.na(value))),
            by = list(object_id, ext_index)]

        ext_na[ext_na[is_all_na == FALSE, .I[1L], by = list(object_id)]$V1, can_be_na := is_all_na]
        ext_na[can_be_na == FALSE | is.na(can_be_na), can_be_na := can_be_na[1L],
            by = list(cumsum(!is.na(can_be_na)), object_id)]
        ext_na[is.na(can_be_na), can_be_na := TRUE]

        invalid_obj <- ext_na[has_any_na == TRUE & can_be_na == FALSE, unique(object_id)]
    }

    if (not_empty(invalid_obj)) {
        # exclude field that are empty extensible fields that not required
        input$value_tbl <- input$value_tbl[is_extensible == TRUE & required_field == FALSE & is.na(value)]

        if (type == "add_object") {
            incomp_ext <- ext_tbl[object_rleid %in% invalid_obj]
        } else {
            incomp_ext <- ext_tbl[object_id %in% invalid_obj]
        }

        private$m_log$validate$incomplete_extensible <- incomp_ext[
            , .SD, .SDcols = names(i_empty_check_dt())]
    }
}
# }}}
# i_check_missing_field: missing required fields {{{
i_check_missing_field <- function (self, private, input) {
    if (eplusr_option("validate_level") != "final") return()
    missing_value <- input$value_tbl[required_field == TRUE & is.na(value) & is_extensible == FALSE]

    if (not_empty(missing_value)) {
        private$m_log$validate$missing_value <- missing_value[
            , .SD, .SDcols = names(i_empty_check_dt())]
        input$value_tbl <- input$value_tbl[!missing_value, on = "value_id"]
    }
}
# }}}
# i_exclude_empty_field: exclude non-required empty fields {{{
i_exclude_empty_field <- function (self, private, input) {
    input$value_tbl <- input$value_tbl[!(required_field == FALSE & is.na(value))]
}
# }}}
# i_check_invalid_autosize: invalid autosize fields {{{
i_check_invalid_autosize <- function (self, private, input) {
    invalid_autosize <- input$value_tbl[
        value_upper == "AUTOSIZE" & autosizable == FALSE]

    if (is_empty(invalid_autosize)) return()

    private$m_log$validate$invalid_autosize <- invalid_autosize[
        , .SD, .SDcols = names(i_empty_check_dt())]
    input$value_tbl <- input$value_tbl[!invalid_autosize, on = "value_id"]
}
# }}}
# i_check_invalid_autocalculate: invalid autocalculate fields {{{
i_check_invalid_autocalculate <- function (self, private, input) {
    invalid_autocalculate <- input$value_tbl[
        value_upper == "AUTOCALCULATE" & autocalculatable == FALSE]

    if (is_empty(invalid_autocalculate)) return()

    private$m_log$validate$invalid_autocalculate <- invalid_autocalculate[
        , .SD, .SDcols = names(i_empty_check_dt())]
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
# i_check_invalid_character: invalid numeric fields {{{
i_check_invalid_character <- function (self, private, input) {
    invalid_character <- input$value_tbl[
        !type %in% c("real", "integer") & !is.na(value_num)]

    if (is_empty(invalid_character)) return()

    private$m_log$validate$invalid_character <- invalid_character[
        , .SD, .SDcols = names(i_empty_check_dt())]
    input$value_tbl <- input$value_tbl[!invalid_character, on = "value_id"]
}
# }}}
# i_check_invalid_numeric: invalid numeric fields {{{
i_check_invalid_numeric <- function (self, private, input) {
    invalid_numeric <- input$value_tbl[
        type %in% c("real", "integer") & is.na(value_num)]

    if (is_empty(invalid_numeric)) return()

    private$m_log$validate$invalid_numeric <- invalid_numeric[
        , .SD, .SDcols = names(i_empty_check_dt())]
    input$value_tbl <- input$value_tbl[!invalid_numeric, on = "value_id"]
}
# }}}
# i_check_invalid_integer: invalid integer fields {{{
i_check_invalid_integer <- function (self, private, input) {
    invalid_integer <- input$value_tbl[
        type %in% c("integer") & is.na(value_num)]

    if (is_empty(invalid_integer)) return()

    private$m_log$validate$invalid_integer <- invalid_integer[
        , .SD, .SDcols = names(i_empty_check_dt())]
    input$value_tbl <- input$value_tbl[!invalid_integer, on = "value_id"]
}
# }}}
# i_check_invalid_choice: invalid choice fields {{{
i_check_invalid_choice <- function (self, private, input) {
    val_tbl <- input$value_tbl[type == "choice"]

    if (is_empty(val_tbl)) return()

    # have to handle class "Schedule:Week:Compact" seperately
    val_tbl[class_name == "Schedule:Week:Compact",
        `:=`(value_upper = gsub("^FOR\\s*[:]{0,1}\\s*", "", value_upper))]

    invalid_choice <- private$m_idd_tbl$field_choice[val_tbl,
        on = c("field_id", choice_upper = "value_upper")][is.na(choice_id)]

    if (is_empty(invalid_choice)) return()

    data.table::setnames(invalid_choice, "choice_upper", "value_upper")
    private$m_log$validate$invalid_choice <- invalid_choice[
        , .SD, .SDcols = names(i_empty_check_dt())]
    input$value_tbl <- input$value_tbl[!invalid_choice, on = "value_id"]
}
# }}}
# i_check_invalid_range: invalid range fields {{{
i_check_invalid_range <- function (self, private, input) {
    val_tbl <- input$value_tbl[type %in% c("integer", "real") & has_range == TRUE]

    if (is_empty(val_tbl)) return()

    invalid_range <- private$m_idd_tbl$field_range[val_tbl, on = "field_id"][
        lower_incbounds == TRUE,  check_lower := value_num >= minimum][
        lower_incbounds == FALSE, check_lower := value_num > minimum][
        upper_incbounds == TRUE,  check_upper := value_num <= maximum][
        upper_incbounds == FALSE, check_upper := value_num < maximum][
        check_lower == FALSE | check_upper == FALSE]

    if (is_empty(invalid_range)) return()

    private$m_log$validate$invalid_range <- invalid_range[
        , .SD, .SDcols = names(i_empty_check_dt())]
    input$value_tbl <- input$value_tbl[!invalid_range, on = "value_id"]
}
# }}}
# i_check_invalid_reference: invalid reference fields {{{
i_check_invalid_reference <- function (self, private, input) {
    if (eplusr_option("validate_level") != "final") return()

    val_tbl <- input$value_tbl[has_object_list == TRUE]

    if (is_empty(val_tbl)) return()

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
        !apply2_lgl(value_upper, possible_value_upper, `%in%`), value_id]
    invalid_ref_non_empty <- input$value_tbl[J(invalid_ref_id), on = "value_id"]

    # combine
    invalid_ref <- data.table::rbindlist(list(invalid_ref_empty, invalid_ref_non_empty))

    if (is_empty(invalid_ref)) return()

    private$m_log$validate$invalid_reference <- invalid_ref[
        , .SD, .SDcols = names(i_empty_check_dt())]
    input$value_tbl <- input$value_tbl[!invalid_ref, on = "value_id"]
}
# }}}

# i_print_validate: print all validate results {{{
i_print_validate <- function (validate) {
    error_num_per <- unlist(mapply(i_count_check_error, type = names(validate),
        MoreArgs = list(validate = validate), SIMPLIFY = FALSE))

    error_num <- sum(error_num_per)
    error_type <- names(which(error_num_per > 0L))

    if (error_num == 0L) {
        cli::cat_line(" ", cli::symbol$tick, " ", "No error found.")
        return()
    }

    cli::cat_line(" ", cli::symbol$cross, " [", error_num, "] ",
        "Errors found during validation.")
    cli::cat_rule(line = 2)

    mapply(i_print_single_validate, type = error_type,
        MoreArgs = list(res = validate[error_type]))

    cli::cat_line()
}
# }}}
# i_print_single_validate: print a single validate result {{{
i_print_single_validate <- function (res, type) {
    error_num <- i_count_check_error(res, type)

    title <- switch(type,
        missing_object = "Missing Required Object",
        duplicate_object = "Duplicated Unique Object",
        conflict_name = "Conflicted Object Names",
        incomplete_extensible = "Incomplete Extensible Group",
        missing_value = "Missing Required Field",
        invalid_autosize = "Invalid Autosize Field",
        invalid_autocalculate = "Invalid Autocalculate Field",
        invalid_numeric = "Invalid Number",
        invalid_character = "Invalid Character",
        invalid_integer = "Invalid Integer",
        invalid_choice = "Invalid Choice",
        invalid_range = "Range Exceeding",
        invalid_reference = "Invalid Reference")

    bullet <- switch(type,
        missing_object = "Objects below are required but not exist.",
        duplicate_object = "Objects should be unique but have multiple instances.",
        conflict_name = "Objects below have the same name.",
        incomplete_extensible = "Fields in each extensible group cannot contain any empty.",
        missing_value = "Fields below are required but values are not given.",
        invalid_autosize = "Fields below cannot be `autosize`.",
        invalid_autocalculate = "Fields below cannot be `autocalculate`.",
        invalid_numeric = "Fields below should be numbers but are not.",
        invalid_character = "Fields below should be characters but are not.",
        invalid_integer = "Fields below are not or cannot be coerced into integers.",
        invalid_choice = "Fields below are not one of prescribed choices.",
        invalid_range = "Fields below exceed prescibed ranges.",
        invalid_reference = "Fields below are not one of valid references.")

    cli::cat_line()
    cli::cat_rule(paste0("[", error_num, "] ", title))
    cli::cat_bullet(bullet, bullet = "circle_cross")

    if (type == "missing_object") {
        cli::cat_bullet(backtick(res[[type]]))
    } else {
        cli::cat_line(format_objects(res[[type]]))
    }
}
# }}}

#' @export
# print.IdfValidity {{{
print.IdfValidity <- function (x, ...) {
    i_print_validate(x)
}
# }}}
