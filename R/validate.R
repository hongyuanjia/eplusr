#' @importFrom cli cat_bullet cat_line cat_rule symbol
#' @importFrom data.table copy data.table setattr
NULL

# empty_validity {{{
# An empty IDF Validity report
empty_validity <- function () {
    validity <- vector("list", 13L)

    chks <- c("missing_object", "duplicate_object", "conflict_name",
        "incomplete_extensible", "missing_value", "invalid_autosize",
        "invalid_autocalculate", "invaid_character", "invalid_numeric",
        "invalid_integer", "invalid_choice", "invalid_range",
        "invalid_reference"
    )

    setattr(validity, "names", chks)

    validity$missing_object <- character()

    empty_check <- data.table(
        object_id = integer(0),
        object_name = character(0),
        class_id = integer(0),
        class_name = character(0),
        field_index = integer(0),
        field_name = character(0),
        full_name = character(0),
        value_id = character(0),
        value = character(0),
        value_num = character(0)
    )

    for (chk in setdiff(chks, "missing_object")) {
        validity[[chk]] <- empty_check
    }

    setattr(validity, "class", c("IdfValidity", "list"))

    validity
}
# }}}
# add_validity {{{
# Add a single check result into IdfValidity
# @param env_in An environment that contains IdfValidity in `validity` element
#               and input value data in `value` element.
# @param check A data.table that contains objects invalid value data that can
#        be used to extract full info from IDF value table.
# @param type A valid IdfValidity type name.
# @param on Passed to `[.data.table` in order to extract value data.
# @return NULL
add_validity <- function (env_in, check, type, on) {
    if (inherits(check, "data.table")) {
        env_in[["validity"]][[type]] <- env_in$object[, .SD, .SDcols = c("object_id", "object_name")][
            env_in$value, on = "object_id"][check, on = on,
            .SD, .SDcols = names(env_in[["validity"]][[type]])]
    } else {
        env_in[["validity"]][[type]] <- env_in$object[, .SD, .SDcols = c("object_id", "object_name")][
            env_in$value, on = "object_id"][J(check), on = on,
            .SD, .SDcols = names(env_in[["validity"]][[type]])]
    }
}
# }}}
# exclude_invalid {{{
# Exclude invalid value data from input
# @param env_in An environment that contains input value data in `value` element.
# @param invalid A data.table that contains objects invalid value data that can
#        be used to extract full info from IDF value table.
# @param on Passed to `[.data.table` in order to exclude invalid value data.
# @return NULL
exclude_invalid <- function (env_in, invalid, on) {
    env_in[["value"]] <- env_in[["value"]][!invalid, on = on]
}
# }}}

# custom_validate {{{
custom_validate <- function (
    required_object = FALSE, unique_object = FALSE, unique_name = FALSE,
    extensible = FALSE, required_field = FALSE, autofield = FALSE,
    type = FALSE, choice = FALSE, range = FALSE, reference = FALSE
)
{
    list(
        required_object = required_object,
        unique_object = unique_object,
        unique_name = unique_name,
        extensible = extensible,
        required_field = required_field,
        autofield = autofield,
        type = type,
        choice = choice,
        range = range,
        reference = reference
    )
}
# }}}
# level_checks {{{
level_checks <- function (level = eplusr_option("validate_level")) {
    level <- match.arg(level, c("none", "draft", "final"))

    if (level == "none") {
        custom_validate(
            autofield = TRUE, type = TRUE
        )
    } else if (level == "draft") {
        custom_validate(
            autofield = TRUE, type = TRUE, unique_name = TRUE, choice = TRUE,
            range = TRUE
        )
    } else {
        custom_validate(
            required_object = TRUE, unique_object = TRUE, unique_name = TRUE,
            extensible = TRUE, required_field = TRUE, autofield = TRUE,
            type = TRUE, choice = TRUE, range = TRUE, reference = TRUE
        )
    }
}
# }}}
# validate_on_level {{{
# Validate input IDF data on different level
# @param dt_idd An environment that contains IDD data
# @param dt_idf An environment that contains IDF data
# @param dt_object A data.table that contains object data to validate
# @param dt_value A data.table that contains value data to validate
# @param level Strictness level to validate. Should be one of `"none"`,
#        `"draft"` and `"final"`.
# @return An IdfValidity object.
validate_on_level <- function (dt_idd, dt_idf, dt_object = NULL, dt_value = NULL, level) {

    if (is.character(level)) {
        level <- level_checks(level)
    } else {
        stopifnot(is.list(level), has_names(level, names(custom_validate())))
    }

    validate_objects(dt_idd, dt_idf, dt_object, dt_value,
        required_object = level$required_object,
        unique_object = level$unique_object,
        unique_name = level$unique_name,
        extensible = level$extensible,
        required_field = level$required_field,
        autofield = level$autofield,
        type = level$type,
        choice = level$choice,
        range = level$range,
        reference = level$reference
    )
}
# }}}
# validate_objects {{{
# Validate input IDF data in terms of various aspects
# @param dt_idd An environment that contains IDD data
# @param dt_idf An environment that contains IDF data
# @param dt_object A data.table that contains object data to validate. If
#        `NULL`, the object data from `dt_idf` will be used, which means to
#        validate the whole IDF.
# @param dt_object A data.table that contains value data to validate. If
#        `NULL`, the value data from `dt_idf` will be used, which means to
#        validate the whole IDF.
# @param required_object Whether to check if required objects are missing. This
#        will only be applied when checking the whole IDF.
# @param unique_object Whether to check if there are multiple instances of
#        unique object.
# @param unique_name Whether to check if there are objects having the same name
#        in same class.
# @param extensible Whether to check if there are incomplete extensible.
# @param required_field Whether to check if there are missing value for
#        required fields.
# @param autofield Whether to check if there are non-autosizable or
#        non-autocalculatable fields that are assigned "autosize" or
#        "autocalculate".
# @param type Whether to check if there are input values whose type are not
#        consistent with definitions in IDD.
# @param choice Whether to check if there are invalid choice values.
# @param range Whether to check if there are numeric values that are out of
#        ranges specified in IDD.
# @param reference Whether to check if there are values that have invalid
#        references.
# @return An IdfValidity object.
validate_objects <- function
(
    dt_idd, dt_idf, dt_object = NULL, dt_value = NULL,
    required_object = FALSE, unique_object = FALSE, unique_name = FALSE,
    extensible = FALSE, required_field = FALSE, autofield = FALSE,
    type = FALSE, choice = FALSE, range = FALSE, reference = FALSE
)
{

    stopifnot(
        is.logical(required_object),
        is.logical(unique_object),
        is.logical(unique_name),
        is.logical(extensible),
        is.logical(required_field),
        is.logical(autofield),
        is.logical(type),
        is.logical(choice),
        is.logical(range),
        is.logical(reference)
    )

    # if object and value dt are not provided, then this means to validate the
    # whole IDF
    if (is.null(dt_object) && is.null(dt_value)) {
        dt_object <- dt_idf$object
        dt_value <- dt_idf$value
        check_whole <- TRUE
    } else {
        check_whole <- FALSE
    }

    # copy in order to leave original dt unaffected
    dt_object <- copy(dt_object)
    dt_value <- copy(dt_value)

    # add field attributes used for validating {{{
    cols_add <- character(0)
    if (isTRUE(extensible)) cols_add <- c(cols_add, "extensible_group")
    if (isTRUE(required_field)) cols_add <- c(cols_add, "required_field")
    if (isTRUE(autofield)) cols_add <- c(cols_add, "autosizable", "autocalculatable")
    if (isTRUE(choice)) cols_add <- c(cols_add, "choice")
    if (isTRUE(range)) cols_add <- c(cols_add, "has_range", "maximum", "minimum", "lower_incbounds", "upper_incbounds")
    # to exclude empty fields
    if (isTRUE(autofield) || isTRUE(type) || isTRUE(choice) || isTRUE(range) || isTRUE(reference)) {
        cols_add <- c(cols_add, "required_field")
    }
    # to exclude auto fields
    if (isTRUE(type) || isTRUE(choice) || isTRUE(range) || isTRUE(reference)) {
        cols_add <- c(cols_add, "autosizable", "autocalculatable")
    }

    if (length(setdiff(cols_add, names(dt_value)))) {
        dt_value <- dt_idd$field[, .SD, .SDcols = c("field_id", setdiff(cols_add, names(dt_value)))][
            dt_value, on = "field_id"]
    }
    # }}}

    # add lower-case value
    set(dt_value, NULL, "value_lower", stri_trans_tolower(dt_value$value))

    # put all input into an environment
    env_in <- new.env(parent = emptyenv(), size = 4L)
    env_in$check_whole <- check_whole
    env_in$object <- dt_object
    env_in$value <- dt_value
    env_in$validity <- empty_validity()

    if (isTRUE(required_object) && check_whole) check_missing_object(dt_idd, dt_idf, env_in)
    if (isTRUE(unique_object)) check_duplicate_object(dt_idd, dt_idf, env_in)
    if (isTRUE(unique_name)) check_conflict_name(dt_idd, dt_idf, env_in)
    if (isTRUE(extensible)) check_incomplete_extensible(dt_idd, dt_idf, env_in)
    if (isTRUE(required_field)) check_missing_value(dt_idd, dt_idf, env_in)

    # exclude unrequired empty fields
    if (isTRUE(autofield) || isTRUE(type) || isTRUE(choice) || isTRUE(range) || isTRUE(reference)) {
        exclude_empty_field(dt_idd, dt_idf, env_in)
    }

    if (isTRUE(autofield)) {
        check_invalid_autosize(dt_idd, dt_idf, env_in)
        check_invalid_autocalculate(dt_idd, dt_idf, env_in)
    }

    # exclude autosize and autocalculate fields when checking types, ranges and
    # references
    if (isTRUE(type) || isTRUE(choice) || isTRUE(range) || isTRUE(reference)) {
        exclude_auto_field(dt_idd, dt_idf, env_in)
    }

    if (isTRUE(type)) {
        check_invalid_character(dt_idd, dt_idf, env_in)
        check_invalid_numeric(dt_idd, dt_idf, env_in)
        check_invalid_integer(dt_idd, dt_idf, env_in)
    }

    if (isTRUE(choice)) check_invalid_choice(dt_idd, dt_idf, env_in)
    if (isTRUE(range)) check_invalid_range(dt_idd, dt_idf, env_in)
    if (isTRUE(reference)) check_invalid_reference(dt_idd, dt_idf, env_in)

    env_in$validity
}
# }}}

# check_missing_object: check missing required objects {{{
check_missing_object <- function (dt_idd, dt_idf, env_in) {
    required <- t_class_name_required(dt_idd$class)
    env_in$validity$missing_object <- required[!required %in% unique(dt_idf$object$class_name)]
    env_in
}
# }}}
# check_duplicate_object: check duplicated unique objects {{{
check_duplicate_object <- function (dt_idd, dt_idf, env_in) {
    dup_uni <- env_in$object[class_name %in% t_class_name_unique(dt_idd$class),
        list(num = .N), by = list(class_id)][num > 1L]

    if (nrow(dup_uni)) {
        add_validity(env_in, duplicate_object, "duplicate_object", "class_id")
    }

    env_in
}
# }}}
# check_conflict_name: objects in the same class have exact the same name {{{
check_conflict_name <- function (dt_idd, dt_idf, env_in) {
    # only check objects that have names
    if (env_in$check_whole) {
        obj <- env_in$object[!is.na(object_name)]
    } else {
        exist <- dt_idf$object[class_id %in% env_in$object$class_id]
        # add existing object
        obj <- ins_dt(exist, env_in$object, "object_id")[!is.na(object_name)]
    }

    if (!nrow(obj)) return(env_in)

    conf_id <- obj[, list(num = .N, id_list = list(object_id)),
        by = list(class_id, object_name_lower)][num > 1L, unlist(id_list, use.names = FALSE)]

    if (is.null(conf_id)) return(env_in)

    # get object name in order to correctly print error messages
    obj <- obj[J(conf_id), on = "object_id", .SD, .SDcols = c("object_id", "object_name")]
    # only show the first 3 fields
    env_in$validity$conflict_name <- ins_dt(dt_idf$value, env_in$value, "value_id")[
        obj, on = "object_id"][
        field_index <= 3L, .SD, .SDcols = names(env_in$validity$conflict_name)
    ]

    env_in
}
# }}}
# check_incomplete_extensible: incomplete extensible group {{{
check_incomplete_extensible <- function (dt_idd, dt_idf, env_in) {
    # extensible groups in input
    ext <- env_in$value[extensible_group > 0L,
        list(object_id, field_index, field_id, extensible_group, value)]

    if (!nrow(ext)) return(env_in)

    # check incomplete extensible fields
    # check if fields in an extensible group have any NA or are all NAs
    empty_info <- ext[order(object_id, -extensible_group, -field_index),
        list(has_any_na = any(is.na(value)), is_all_na = all(is.na(value))),
        by = list(object_id, extensible_group)]
    # if fields in one extensible group only have some NAs or do not have any
    # NA, then extensible groups below that group cannot have any NA
    empty_info[empty_info[is_all_na == FALSE, .I[1L], by = list(object_id)]$V1, can_be_na := is_all_na]
    empty_info[can_be_na == FALSE | is.na(can_be_na), can_be_na := can_be_na[1L],
        by = list(cumsum(!is.na(can_be_na)), object_id)]
    empty_info[is.na(can_be_na), can_be_na := TRUE]
    incomplete <- empty_info[has_any_na == TRUE & can_be_na == FALSE, list(object_id, extensible_group)]

    if (nrow(incomplete)) {
        add_validity(env_in, incomplete, "incomplete_extensible", c("object_id", "extensible_group"))
    }

    env_in
}
# }}}
# check_missing_value: missing required fields {{{
check_missing_value <- function (dt_idd, dt_idf, env_in) {
    missing_value <- env_in$value[required_field == TRUE & is.na(value)]

    if (!nrow(missing_value)) return(env_in)

    add_validity(env_in, missing_value, "missing_value", "value_id")
    exclude_invalid(env_in, missing_value, on = "value_id")
}
# }}}
# check_invalid_autosize: invalid autosize fields {{{
check_invalid_autosize <- function (dt_idd, dt_idf, env_in) {
    invalid_autosize <- env_in$value[value_lower == "autosize" & autosizable == FALSE]

    if (!nrow(invalid_autosize)) return(env_in)

    add_validity(env_in, invalid_autosize, "invalid_autosize", "value_id")
    exclude_invalid(env_in, invalid_autosize, "value_id")
}
# }}}
# check_invalid_autocalculate: invalid autocalculate fields {{{
check_invalid_autocalculate <- function (dt_idd, dt_idf, env_in) {
    invalid_autocalculate <- env_in$value[value_lower == "autocalculate" & autocalculatable == FALSE]

    if (!nrow(invalid_autocalculate)) return(env_in)

    add_validity(env_in, invalid_autocalculate, "invalid_autocalculate", "value_id")
    exclude_invalid(env_in, invalid_autocalculate, "value_id")
}
# }}}
# check_invalid_character: invalid numeric fields {{{
check_invalid_character <- function (dt_idd, dt_idf, env_in) {
    invalid_character <- env_in$value[type_enum > .globals$type$real & !is.na(value_num)]

    if (!nrow(invalid_character)) return(env_in)

    add_validity(env_in, invalid_character, "invalid_character", "value_id")
    exclude_invalid(env_in, invalid_character, "value_id")
}
# }}}
# check_invalid_numeric: invalid numeric fields {{{
check_invalid_numeric <- function (dt_idd, dt_idf, env_in) {
    invalid_numeric <- env_in$value[type_enum <= .globals$type$real & is.na(value_num)]

    if (!nrow(invalid_numeric)) return(env_in)

    add_validity(env_in, invalid_numeric, "invalid_numeric", "value_id")
    exclude_invalid(env_in, invalid_numeric, "value_id")
}
# }}}
# check_invalid_integer: invalid integer fields {{{
check_invalid_integer <- function (dt_idd, dt_idf, env_in) {
    invalid_integer <- env_in$value[type_enum == .globals$type$integer & is.na(value_num)]

    if (!nrow(invalid_integer)) return(env_in)

    add_validity(env_in, invalid_integer, "invalid_integer", "value_id")
    exclude_invalid(env_in, invalid_integer, "value_id")
}
# }}}
# check_invalid_choice: invalid choice fields {{{
check_invalid_choice <- function (dt_idd, dt_idf, env_in) {
    cho <- env_in$value[type_enum == .globals$type$choice]

    if (!nrow(cho)) return(env_in)

    # have to handle class "Schedule:Week:Compact" seperately
    cho[class_name == "Schedule:Week:Compact",
        `:=`(value_lower = gsub("^for\\s*[:]{0,1}\\s*", "", value_lower))]

    set(cho, NULL, "choice", list(lapply(cho$choice, stri_trans_tolower)))
    invalid_choice <- cho[!apply2_lgl(value_lower, choice, "%in%")]

    if (!nrow(invalid_choice)) return(env_in)

    add_validity(env_in, invalid_choice, "invalid_choice", "value_id")
    exclude_invalid(env_in, invalid_choice, "value_id")
}
# }}}
# check_invalid_range: invalid range fields {{{
check_invalid_range <- function (dt_idd, dt_idf, env_in) {
    val <- env_in$value[type_enum <= .globals$type$real & has_range == TRUE]

    if (!nrow(val)) return(env_in)

    invalid_range <- val[
        lower_incbounds == TRUE,  check_lower := value_num >= minimum][
        lower_incbounds == FALSE, check_lower := value_num > minimum][
        upper_incbounds == TRUE,  check_upper := value_num <= maximum][
        upper_incbounds == FALSE, check_upper := value_num < maximum][
        check_lower == FALSE | check_upper == FALSE]

    if (!nrow(invalid_range)) return(env_in)

    add_validity(env_in, invalid_range, "invalid_range", "value_id")
    exclude_invalid(env_in, invalid_range, "value_id")
}
# }}}
# check_invalid_reference: invalid reference fields {{{
check_invalid_reference <- function (dt_idd, dt_idf, env_in) {
    val <- env_in$value[type_enum == .globals$type$object_list,
        list(object_id, value_id, value, field_id, type_enum)]

    if (!nrow(val)) return(env_in)

    if (env_in$check_whole) {
        ref_map <- get_value_reference_map(dt_idd$reference, env_in$value, val)
    } else {
        ref_map <- get_value_reference_map(dt_idd$reference,
            ins_dt(dt_idf$value, env_in$value, "value_id"), val)
    }

    invalid_ref <- ref_map[val, on = list(value_id)][is.na(src_value_id)]

    if (!nrow(invalid_ref)) return(env_in)

    add_validity(env_in, invalid_ref, "invalid_reference", "value_id")
    exclude_invalid(env_in, invalid_ref, "value_id")
}
# }}}

# exclude_empty_field: exclude non-required empty fields {{{
exclude_empty_field <- function (dt_idd, dt_idf, env_in) {
    env_in$value <- env_in$value[!(required_field == FALSE & is.na(value))]
    env_in
}
# }}}
# exclude_auto_field: exclude valid autosize and autocalculate fields {{{
exclude_auto_field <- function (dt_idd, dt_idf, env_in) {
    env_in$value <- env_in$value[
        !(value_lower == "autosize" & autosizable == TRUE) &
        !(value_lower == "autocalculate" & autocalculatable == TRUE)
    ]
    env_in
}
# }}}

# count_check_type_error {{{
# Count total error in a single validity type
count_check_type_error <- function (validity, type) {
    if (identical(type, "missing_object"))
        return(length(validity[[type]]))

    if (identical(type, "duplicate_object") || identical(type, "conflict_name"))
        return(length(unique(validity[[type]]$object_id)))

    if (identical(type, "incomplete_extensible"))
        return(validity[[type]][is.na(value), .N])

    nrow(validity[[type]])
}
# }}}
# count_check_error {{{
# Count total error in validity
count_check_error <- function (validity) {
    sum(vapply(names(validity), count_check_type_error, integer(1L), validity = validity, USE.NAMES = FALSE))

}
# }}}
# print_validity: print all validity results {{{
print_validity <- function (validity) {
    error_num_per <- vapply(names(validity), count_check_type_error,
        integer(1L), validity = validity)

    error_num <- sum(error_num_per)
    error_type <- names(which(error_num_per > 0L))

    if (error_num == 0L) {
        cli::cat_line(" ", cli::symbol$tick, " ", "No error found.")
        return()
    }

    cli::cat_line(" ", cli::symbol$cross, " [", error_num, "] ",
        "Errors found during validation.")
    cli::cat_rule(line = 2)

    mapply(print_single_validity, type = error_type,
        MoreArgs = list(single_validity = validity[error_type]))

    cli::cat_line()
}
# }}}
# print_single_validity: print a single validity result {{{
print_single_validity <- function (single_validity, type) {
    error_num <- count_check_type_error(single_validity, type)

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
        invalid_reference = "Invalid Reference"
    )

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
        cli::cat_bullet(surround(single_validity[[type]]))
    } else {
        cli::cat_line(format_objects(single_validity[[type]]))
    }
}
# }}}

#' @export
# print.IdfValidity {{{
print.IdfValidity <- function (x, ...) {
    print_validity(x, ...)
}
# }}}
