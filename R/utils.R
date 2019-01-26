#' @importFrom readr write_lines
#' @importFrom stringi stri_replace_all_charclass stri_trans_tolower
NULL

# `%||%` {{{
`%||%` <- function (x, y) {
    if (is.null(x)) {
        y
    } else {
        x
    }
}
# }}}

# collapse {{{
collapse <- function (x, out = "`") {
    if (is.null(out)) {
        s <- x
    } else {
        out <- as.character(out)
        if (is_scalar(out)) {
            out <- c(out, out)
        }
        s <- paste0(out[1L], x, out[2L])
    }
    if (length(s) == 1L) return (s)

    b <- paste0(s[-length(s)], collapse = ", ")
    e <- s[length(s)]
    paste0(b, " and ", e)
}
# }}}

# surround {{{
surround <- function (x, out = "`") {
    if (is.null(out)) return(x)
    out <- as.character(out)
    if (is_scalar(out)) {
        out <- c(out, out)
    }
    paste0(out[1L], x, out[2L])
}
# }}}

# `._get_self`{{{
`._get_self` <- function (x) {
    .subset2(.subset2(x, ".__enclos_env__"), "self")
}
# }}}

# `._get_private`{{{
`._get_private` <- function (x) {
    .subset2(.subset2(x, ".__enclos_env__"), "private")
}
# }}}

# pad: borrowed from `r-lib/cli` {{{
rpad <- function(x, char = " ", width = NULL) {
    if (!length(x)) return(x)
    w <- nchar(x, type = "width")
    if (is.null(width)) width <- max(w)
    paste0(x, strrep(char, pmax(width - w, 0)))
}

lpad <- function(x, char = " ", width = NULL) {
    if (!length(x)) return(x)
    w <- nchar(x, type = "width")
    if (is.null(width)) width <- max(w)
    paste0(strrep(char, pmax(width - w, 0)), x)
}
# }}}

# clone_generator {{{
clone_generator <- function (x) {
    # create a new environment with the R6:::capsule environment being its
    # parent
    new <- new.env(parent = parent.env(x))

    # set enclosing environments of all generator funs to the new environment
    new_funs <- lapply(as.list.environment(x, all.names = TRUE), function(x) {
        if (is.function(x)) environment(x) <- new
        x
    })

    # add generator funs to the new environment
    list2env(new_funs, new)
    # set self ref
    new$self <- new

    # add attributes
    class(new) <- "R6ClassGenerator"
    attr(new, "name") <- paste0(deparse(substitute(x)), "_generator")

    new
}
# }}}

# write_lines {{{
# NOTE: IDFEditor will crash if a large IDF file was saved with LF eol on
#       Windows.
write_lines <- function (x, file = "") {
    if (inherits(x, "data.table")) {
        assert(has_name(x, "string"))
        fwrite(x[, list(string)], file = file, col.names = FALSE, quote = FALSE)
    } else {
        assert(is.character(x))
        fwrite(data.table(x), file = file, col.names = FALSE, quote = FALSE)
    }
}
# }}}

# os_type: Return operation system type {{{
os_type <- function () {
    if (.Platform$OS.type == 'windows') {
        "windows"
    } else if (Sys.info()[['sysname']] == 'Darwin') {
        "macos"
    } else if (Sys.info()[['sysname']] == 'Linux') {
        "linux"
    } else {
        "unknown"
    }
}
# }}}

# standardize_ver {{{
standardize_ver <- function (ver, strict = FALSE) {
    assert(is_scalar(ver))
    if (!strict && identical(ver, "latest")) ver <- LATEST_EPLUS_VER
    if (is_integer(ver)) ver <- paste0(ver, ".0")
    ver <- numeric_version(ver, strict = FALSE)
    if (is.na(ver)) return(ver)
    if (is.na(ver[1L, 3L])) ver[1L, 3L] <- 0L
    ver
}
# }}}

# is_normal_list {{{
is_normal_list <- function (x) {
    is.list(x) && vec_depth(x) == 2L && all(vapply(x, not_empty, logical(1)))
}
# }}}

# vec_depth {{{
vec_depth <- function (x) {
    if (is.null(x)) {
        0L
    } else if (is.atomic(x)) {
        1L
    } else if (is.list(x)) {
        depths <- vapply(x, vec_depth, integer(1))
        1L + max(depths, 0L)
    } else {
        stop("`x` must be a vector")
    }
}
# }}}

# appply2 {{{
apply2 <- function (x, y, fun, more_args = NULL) {
    mapply(FUN = fun, x, y, MoreArgs = more_args, SIMPLIFY = FALSE)
}
# }}}

# apply2_int {{{
apply2_int <- function (x, y, fun, more_args = NULL) {
    as.integer(unlist(apply2(x, y, fun, more_args)))
}
# }}}

# apply2_lgl {{{
apply2_lgl <- function (x, y, fun, more_args = NULL) {
    as.logical(unlist(apply2(x, y, fun, more_args)))
}
# }}}

# lower_name {{{
lower_name <- function (name) {
    stri_trans_tolower(underscore_name(name))
}
# }}}

# underscore_name {{{
underscore_name <- function (name, merge = TRUE) {
    stri_replace_all_charclass(name, "[^[:alnum:]]", "_", merge = merge)
}
# }}}

# cnd {{{
cnd <- function (type = c("error", "warning", "message"), subclass, message, call = NULL, ...) {
    type <- match.arg(type)
    structure(
        list(message = message, call = call, ...),
        class = c(subclass, type, "condition")
    )
}
# }}}

# abort {{{
# reference: https://adv-r.hadley.nz/conditions.html#custom-conditions
abort <- function (subclass, message, call = NULL, ...) {
    err <- cnd(type = "error", subclass =  subclass, message = message, call = call, ...)
    stop(err)
}
# }}}

# warn {{{
# reference: https://adv-r.hadley.nz/conditions.html#custom-conditions
warn <- function (subclass, message, call = NULL, ...) {
    w <- cnd(type = "warning", subclass =  subclass, message = message, call = call, ...)
    warning(w)
}
# }}}

# names2 {{{
names2 <- function (x, default = NA_character_) {
    nm <- names(x)
    if (is.null(nm)) {
        return(rep(default, length(x)))
    }

    nm[stri_isempty(nm)] <- default
    nm
}
# }}}

# each_length {{{
each_length <- function (x) {
    vapply(x, length, integer(1L))
}
# }}}

# in_range {{{
in_range <- function (x, range) {
    if (range$lower_incbounds == range$upper_incbounds) {
        between(x, range$minimum, range$maximum, range$lower_incbounds)
    } else {
        if (range$lower_incbounds) {
            x >= range$minimum & x < range$maximum
        } else {
            x > range$minimum & x <= range$maximum
        }
    }
}
# }}}

# make_field_range {{{
make_field_range <- function (minimum, lower_incbounds, maximum, upper_incbounds) {
    assert(is_number(minimum), is_number(maximum), is_falg(lower_incbounds), is_flag(upper_incbounds))
    setattr(
        list(
            minimum = minimum, lower_incbounds = lower_incbounds,
            maximum = maximum, upper_incbounds = upper_incbounds
        ),
        "class", c("FieldRange", "list")
    )
}
# }}}

# ins_dt {{{
ins_dt <- function (dt, new_dt, base_col = NULL) {
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
