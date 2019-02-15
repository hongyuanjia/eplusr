#' @importFrom cli cat_bullet cat_line cat_rule rule symbol
#' @importFrom crayon bold cyan red strip_style underline
#' @importFrom data.table copy data.table dcast rbindlist
#' @importFrom data.table setattr setcolorder setnames setorder setorderv
#' @importFrom stringi stri_locate_first_regex stri_replace_first_regex "stri_sub<-"
#' @importFrom stringi stri_subset_regex stri_match_first_regex stri_rand_strings
#' @include parse.R
#' @include clone.R
NULL

# COMMON
# recognize_input {{{
recognize_input <- function (input, type = "class", underscore = FALSE, lower = FALSE) {
    if (underscore && lower) stop("underscore and lower cannot all be TRUE.")

    if (inherits(input, "data.table")) {
        dt_in <- input
        if (!has_name(dt_in, "rleid")) add_rleid(dt_in)
        if (has_name(dt_in, paste0(type, "_id"))) {
            col_on <- paste0(type, "_id")
            col_key <- paste0(type, " index")
        } else {
            assert(has_name(dt_in, paste0(type, "_name")))
            if (underscore) {
                if (!has_name(dt_in, paste0(type, "_name_us"))) {
                    set(dt_in, NULL, paste0(type, "_name_us"), underscore_name(dt_in[[paste0(type, "_name")]]))
                }
                col_on <- paste0(type, "_name_us")
            } else if (lower) {
                if (!has_name(dt_in, paste0(type, "_name_lower"))) {
                    set(dt_in, NULL, paste0(type, "_name_lower"), stri_trans_tolower(dt_in[[paste0(type, "_name")]]))
                }
                col_on <- paste0(type, "_name_lower")
            }else {
                col_on <- paste0(type, "_name")
            }
            col_key <- paste0(type, " name")
        }
    } else {
        if (is.character(input)) {
            if (underscore) {
                input <- underscore_name(input)
                col_on <- paste0(type, "_name_us")
            } else if (lower) {
                input <- stri_trans_tolower(input)
                col_on <- paste0(type, "_name_lower")
            } else {
                col_on <- paste0(type, "_name")
            }
            col_key <- paste0(type, " name")
        } else if (all(are_count(input))) {
            col_on <- paste0(type, "_id")
            col_key <- paste0(type, " index")
        } else {
            abort_bad_which_type(paste0("error_", type, "_which_type"), type)
        }
        dt_in <- data.table(input = input, rleid = seq_along(input))
        setnames(dt_in, "input", col_on)
    }

    # make sure the first column is the column used for joinning
    setcolorder(dt_in, c(col_on, setdiff(names(dt_in), col_on)))
    dt_in
}
# }}}
# join_from_input {{{
join_from_input <- function (dt, input, check = "group_id") {
    col_on <- names(input)[[1L]]

    res <- dt[input, on = col_on, allow.cartesian = TRUE]

    if (anyNA(res[[check]])) {
        invld_cls <- res[is.na(get(check))][[col_on]]
        if (stri_endswith_fixed(col_on, "id")) {
            if (stri_startswith_fixed(col_on, "object")) {
                col_key <- "ID"
            } else {
                col_key <- "index"
            }
        } else {
            col_key <- "name"
        }
        col_key <- paste(stri_replace_first_regex(col_on, "_.*", ""), col_key)
        abort_bad_key(paste0("error_", col_on), col_key, invld_cls)
    }

    setcolorder(res, c("rleid", setdiff(names(res), "rleid")))
    res
}
# }}}

# LOG
# log_new_uuid {{{
log_new_uuid <- function (log) {
    log$uuid <- unique_id()
}
# }}}
# log_new_order {{{
log_new_order <- function (log, id) {
    log$order <- append_dt(log$order, data.table(object_id = id, object_order = 1L, "object_id"))
}
# }}}
# log_add_order {{{
log_add_order <- function (log, id) {
    log$order[J(id), on = "object_id", object_order := object_order + 1L]
}
# }}}
# log_del_order {{{
log_del_order <- function (log, id) {
    log$order <- log$order[!J(id), on = "object_id"]
}
# }}}
# log_unsaved {{{
log_unsaved <- function (log) {
    log$unsaved <- TRUE
}
# }}}
# log_saved {{{
log_saved <- function (log) {
    log$unsaved <- FALSE
}
# }}}

# UTILITIES
# in_final_mode {{{
in_final_mode <- function () {
    eplusr_option("validate_level") == "final"
}
# }}}
# in_ip_mode {{{
in_ip_mode <- function () {
    eplusr_option("view_in_ip")
}
# }}}
# verbose_info {{{
verbose_info <- function (...) {
    if (eplusr_option("verbose_info")) {
        cli::cat_rule(crayon::bold("Info"), col = "green")
        cat(crayon::green(paste0(...)), "\n", sep = "")
        cat("\n")
    }
}
# }}}

# abort_bad_key {{{
abort_bad_key <- function (error_type, key, value) {
    mes <- paste0("Invalid ", key, " found: ", collapse(value))
    abort(error_type, mes, value = value)
}
# }}}
# abort_bad_which_type {{{
abort_bad_which_type <- function (error_type, key, ...) {
    mes <- paste0(key, " should be either an integer vector or a character vector", ...)
    abort(error_type, mes)
}
# }}}
# abort_bad_field {{{
abort_bad_field <- function (error_type, key, dt, ...) {
    h <- paste0("Invalid field ", key, " found:\n")

    mes <- switch(key,
        index = errormsg_field_index(dt),
        name = errormsg_field_name(dt)
    )

    abort(error_type, paste0(h, mes, ...), data = dt)
}
# }}}
# errormsg_info {{{
errormsg_info <- function (dt) {
    if (!has_name(dt, "rleid")) add_rleid(dt)
    dt[, `:=`(info = paste0(" #", lpad(rleid), "| Class ", surround(class_name)))]
}
# }}}
# errormsg_field_index {{{
errormsg_field_index <- function (dt) {
    dt <- dt[field_index < min_fields | field_index > num_fields,
        list(field_index = collapse(field_index)),
        by = list(rleid, class_name, min_fields, num_fields)
    ]

    dt <- errormsg_info(dt)

    dt[, msg := paste0(info, ": ", rpad(field_index), ".")]

    dt[min_fields == 0L, msg := paste0(msg,
        " Field index should be no more than ", num_fields, ".")]
    dt[min_fields >  0L, msg := paste0(msg,
        " field index should be no less than ", min_fields,
        " and no more than ", num_fields, ".")]

    paste0(dt$msg, collapse = "\n")
}
# }}}
# errormsg_field_name {{{
errormsg_field_name <- function (dt) {
    dt <- dt[, list(field_name = collapse(field_name)),
        by = list(rleid, class_name)
    ]

    dt <- errormsg_info(dt)

    dt[, msg := paste0(info, ": ", rpad(field_name), ".")]
    paste0(dt$msg, collapse = "\n")
}
# }}}

# new_id {{{
new_id <- function (dt, name, num) {
    assert(has_name(dt, name))
    max(dt[[name]], na.rm = TRUE) + seq_len(num)
}
# }}}
# add_rleid {{{
add_rleid <- function (dt, prefix = NULL) {
    if (!is.null(prefix)) prefix <- paste0(prefix, "_")
    set(dt, NULL, paste0(prefix, "rleid"), seq_len(nrow(dt)))
}
# }}}
# append_dt {{{
append_dt <- function (dt, new_dt, base_col = NULL) {
    assert(has_name(new_dt, names(dt)))

    if (is.null(base_col)) {
        rbindlist(list(dt, new_dt[, .SD, .SDcols = names(dt)]))
    } else {
        rbindlist(list(dt[!new_dt, on = base_col], new_dt[, .SD, .SDcols = names(dt)]))
    }
}
# }}}
# unique_id {{{
unique_id <- function () {
    paste0("id-", stri_rand_strings(1, 15L))
}
# }}}
