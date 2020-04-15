#' @importFrom cli cat_bullet cat_line cat_rule rule symbol
#' @importFrom data.table copy data.table dcast rbindlist
#' @importFrom data.table setattr setcolorder setnames setorder setorderv
#' @importFrom stringi stri_locate_first_regex stri_replace_first_regex "stri_sub<-"
#' @importFrom stringi stri_subset_regex stri_match_first_regex stri_rand_strings
#' @include parse.R
NULL

# COMMON
# recognize_input {{{
recognize_input <- function (input, type = "class", underscore = FALSE, lower = FALSE) {
    if (underscore && lower) stop("underscore and lower cannot all be TRUE.")
    input <- assert_valid_type(input, name = type)

    # store the original input
    ori <- input

    if (is.integer(input)) {
        col_on <- paste0(type, "_id")
        col_key <- paste0(type, " index")
    } else {
        if (underscore) {
            input <- underscore_name(input)
            col_on <- paste0(type, "_name_us")
            # always trans to lower case for field names
            if (type == "field") {
                input <- stri_trans_tolower(input)
            }
        } else if (lower) {
            input <- stri_trans_tolower(input)
            col_on <- paste0(type, "_name_lower")
        } else {
            col_on <- paste0(type, "_name")
        }
        col_key <- paste0(type, " name")
    }

    dt_in <- data.table(input = input, rleid = seq_along(input), original = ori)
    setnames(dt_in, "input", col_on)

    # make sure the first column is the column used for joinning
    setcolorder(dt_in, c(col_on, setdiff(names(dt_in), col_on)))

    dt_in
}
# }}}
# join_from_input {{{
join_from_input <- function (dt, input, check = "group_id", allow.cartesian = TRUE) {
    col_on <- names(input)[[1L]]

    res <- dt[input, on = col_on, allow.cartesian = allow.cartesian]

    if (length(check)) check_bad_key(res, check, col_on)

    if (has_names(res, "original")) on.exit(set(res, NULL, "original", NULL), add = TRUE)

    setcolorder(res, "rleid")
    res
}
# }}}
# check_bad_key {{{
check_bad_key <- function (res, col_check, col_on) {
    if (anyNA(res[[col_check]])) {
        if (has_names(res, "original")) {
            invld_cls <- res[is.na(get(col_check))][["original"]]
        } else {
            invld_cls <- res[is.na(get(col_check))][[col_on]]
        }
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
        abort_bad_key(col_key, invld_cls)
    }
    res
}
# }}}
# add_joined_cols {{{
add_joined_cols <- function (base, dt, on, cols) {
    on_dt <- if (!is.null(names(on))) names(on) else on
    nm <- if (!is.null(names(cols))) names(cols) else cols
    on <- unname(on)
    cols <- unname(cols)
    set(dt, NULL, nm, base[J(dt[[on_dt]]), on = on, .SD, .SDcols = cols])
}
# }}}
# del_redundant_cols {{{
del_redundant_cols <- function (base, dt, col_on = names(dt)[[1L]]) {
    col_del <- setdiff(intersect(names(dt), names(base)), col_on)
    if (length(col_del)) set(dt, NULL, col_del, NULL)
    dt
}
# }}}
# keep_same_cols {{{
keep_same_cols <- function (base, dt) {
    col_del <- setdiff(names(dt), intersect(names(dt), names(base)))
    if (length(col_del)) set(dt, NULL, col_del, NULL)
    setcolorder(dt, names(base))
    dt
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
# in_verbose {{{
in_verbose <- function () {
    eplusr_option("verbose_info")
}
# }}}
# verbose_info {{{
verbose_info <- function (...) {
    if (eplusr_option("verbose_info")) message(...)
}
# }}}

# abort_bad_key {{{
abort_bad_key <- function (key, value) {
    mes <- paste0("Invalid ", key, " found: ", collapse(value))
    abort(mes, value = value, class = paste0("invalid_", gsub(" ", "_", tolower(key))))
}
# }}}
# abort_bad_field {{{
abort_bad_field <- function (key, dt, ...) {
    h <- paste0("Invalid field ", key, " found:\n")

    mes <- switch(key,
        index = errormsg_field_index(dt),
        name = errormsg_field_name(dt)
    )

    abort(paste0(h, mes, ...), class = paste0("invalid_field_", key))
}
# }}}
# errormsg_info {{{
errormsg_info <- function (dt) {
    if (!has_names(dt, "rleid")) add_rleid(dt)
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
        " Field index should be no less than ", min_fields,
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
#' @importFrom checkmate assert_names
new_id <- function (dt, name, num) {
    assert_names(names(dt), must.include = name)
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
#' @importFrom checkmate assert_names
append_dt <- function (dt, new_dt, base_col = NULL) {
    assert_names(names(new_dt), must.include = names(dt))

    if (is.null(base_col)) {
        rbindlist(list(dt, new_dt[, .SD, .SDcols = names(dt)]))
    } else {
        rbindlist(list(dt[!new_dt, on = base_col], new_dt[, .SD, .SDcols = names(dt)]))
    }
}
# }}}
# unique_id {{{
unique_id <- function () {
    paste0("id-", stri_rand_strings(1, 10L), "-", as.integer(Sys.time()))
}
# }}}

# assert_valid_type {{{
#' @importFrom checkmate assert_character assert_integerish check_character
#' @importFrom checkmate check_integerish
assert_valid_type <- function (x, name = NULL, len = NULL, null.ok = FALSE, lower = -Inf, type = c("both", "id", "name")) {
    if (is.null(name)) name <- checkmate::vname(x)
    type <- match.arg(type)

    if (type == "name") {
        x <- assert_character(x, any.missing = FALSE, len = len, null.ok = null.ok, .var.name = name)
    } else if (type == "id") {
        x <- assert_integerish(x, any.missing = FALSE, len = len, null.ok = null.ok, lower = lower, coerce = TRUE, .var.name = name)
    } else {
        assert(
            check_character(x, any.missing = FALSE, len = len, null.ok = null.ok),
            check_integerish(x, any.missing = FALSE, len = len, null.ok = null.ok, lower = lower),
            .var.name = name
        )
        if (is.numeric(x)) storage.mode(x) <- "integer"
    }
    x
}
# }}}
