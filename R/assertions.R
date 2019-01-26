#' @importFrom tools file_ext
#' @include constants.R
NULL

# on_fail {{{
# a tailored version of assertthat::`on_failure<-`
# should be compatible with assertthat::assert_that
"on_fail<-" <- function (x, value) {
    stopifnot(is.function(x), identical(names(formals(value)), c("call", "env")))
    setattr(x, "fail", value)
}
# }}}
# assert {{{
# a tailored version of assertthat::assert_that
assert <- function(..., msg = NULL, err_type = NULL, env = parent.frame()) {
    assertions <- eval(substitute(alist(...)))

    for (assertion in assertions) {
        val <- eval(assertion, env)
        # all TRUE
        if (length(val) && !any(is.na(val)) && all(val)) next

        # get error type
        if (is.null(err_type)) {
            fnm <- deparse(assertion[[1L]])
            if (startsWith(fnm, "is") || startsWith(fnm, "are")) {
                err_type <- paste0("error_not_", sub("[a-z]+_", "", fnm))
            } else {
                err_type <- paste0("error_not_", fnm)
            }
        }

        # use msg if given
        if (!is.null(msg)) abort(err_type, msg)

        # get function
        f <- eval(assertion[[1L]], env)

        # get msg_fun
        msg_fun <- attr(f, "fail")

        # if no msg_fun defined, use default message
        if (is.null(msg_fun)) {
            abort(err_type,
                sprintf(ngettext(
                    length(val), "%s is not TRUE.", "%s is not all TRUE."),
                    surround(deparse(assertion, width.cutoff = 60L))
                )
            )
        }

        # match.call does not do well with primitive functions
        if (!is.primitive(f)) assertion <- match.call(f, assertion)
        abort(err_type, msg_fun(assertion, env))
    }
    TRUE
}
# }}}

# is_version {{{
is_version <- function (ver) {
    !is.na(numeric_version(ver, strict = FALSE))
}
# }}}
# is_eplus_ver {{{
is_eplus_ver <- function (ver, strict = FALSE) {
    ver <- standardize_ver(ver, strict)
    if (is.na(ver)) return(FALSE)
    as.character(ver) %in% ALL_EPLUS_VER
}
on_fail(is_eplus_ver) <- function (call, env) {
    paste0(deparse(call$ver), " is not a valid or supported EnergyPlus version. ",
        "Only EnergyPlus v8.3.0 and after are supported."
    )
}
# }}}
# is_idd_ver {{{
is_idd_ver <- function (ver, strict = FALSE, only_released = TRUE) {
    assert(is_scalar(ver))

    if (isTRUE(strict) && !is_version(ver)) return(FALSE)

    is_ver <- identical(ver, "latest") || is_version(ver)

    if (only_released)
        is_ver && as.character(standardize_ver(ver)) %in% ALL_IDD_VER
    else is_ver
}
on_fail(is_idd_ver) <- function (call, env) {
    paste0(deparse(call$ver), " is not a valid Idd version.")
}
# }}}
# is_eplus_path {{{
is_eplus_path <- function (path) {
    assert(is_scalar(path))
    eplus <- paste0("energyplus", if(is_windows()) ".exe" else "")
    all(dir.exists(path), file.exists(file.path(path, c(eplus, "Energy+.idd"))))
}
on_fail(is_eplus_path) <- function (call, env) {
    paste(deparse(call$path), "is not a valid EnergyPlus installation path",
        "where `energyplus` executable and `Energy+.idd` should exist."
    )
}
# }}}
# is_idd {{{
is_idd <- function (x) inherits(x, "Idd")
on_fail(is_idd) <- function (call, env) {
    paste(deparse(call$x), "is not an Idd object.")
}
# }}}
# is_idf {{{
is_idf <- function (x) inherits(x, "Idf")
on_fail(is_idf) <- function (call, env) {
    paste(deparse(call$x), "is not an Idf object.")
}
# }}}
# is_idfobject {{{
is_idfobject <- function (x) inherits(x, "IdfObject")
on_fail(is_idfobject) <- function (call, env) {
    paste(deparse(call$x), "is not an IdfObject object.")
}
# }}}
# is_epw {{{
is_epw <- function (x) inherits(x, "Epw")
on_fail(is_epw) <- function (call, env) {
    paste(deparse(call$x), "is not an Epw object.")
}
# }}}

# not_empty {{{
not_empty <- function (x) {
    all((dim(x) %||% length(x)) != 0)
}
on_fail(not_empty) <- function (call, env) {
    paste(deparse(call$x), "is empty.")
}
# }}}
# is_empty {{{
is_empty <- function (x) {
    !not_empty(x)
}
on_fail(is_empty) <- function (call, env) {
    paste(deparse(call$x), "is not empty.")
}
# }}}
# is_number {{{
is_number <- function (x) {
    length(x) == 1L && is.numeric(x) && !is.na(x)
}
on_fail(is_number) <- function (call, env) {
    paste(deparse(call$x), "is not a number (length one numeric vector).")
}
# }}}
# is_integer {{{
is_integer <- function(x) {
    length(x) == 1L && !is.na(x) && (is.integer(x) || (is.numeric(x) && all(x == trunc(x))))
}
on_fail(is_integer) <- function (call, env) {
    paste(deparse(call$x), "is neither a length one integer nor can be converted into one.")
}
# }}}
# is_count {{{
is_count <- function (x, zero = FALSE) {
    if (!is_integer(x)) return(FALSE)
    if (zero) x >= 0L else x > 0L
}
on_fail(is_epw) <- function (call, env) {
    info <- if (eval(call$zero, env)) "non-nagitive integer" else "positive integer"
    paste(deparse(call$x), "is not a count (a length one", info, "vector).")
}
# }}}
# are_integer {{{
are_integer <- function(x) {
    rep(is.numeric(x), length(x)) & !is.na(x) & (x == trunc(x))
}
on_fail(are_integer) <- function (call, env) {
    paste(deparse(call$x), "is neither an integer vector or can be converted into one.")
}
# }}}
# are_count {{{
are_count <- function (x, zero = FALSE) {
    are_integer(x) & if (zero) x >= 0L else x > 0L
}
on_fail(are_count) <- function (call, env) {
    info <- if (eval(call$zero, env)) "non-nagitive integer" else "positive integer"
    paste(deparse(call$x), "are not counts (a", info, "vector).")
}
# }}}
# is_string {{{
is_string <- function(x) is.character(x) && length(x) == 1 && !is.na(x)
on_fail(is_string) <- function (call, env) {
    paste(deparse(call$x), "is not a string (length one character vector).")
}
# }}}
# is_flag {{{
is_flag <- function(x) is.logical(x) && length(x) == 1 && !is.na(x)
on_fail(is_flag) <- function (call, env) {
    paste(deparse(call$x), "is neither `TRUE` nor `FALSE`.")
}
# }}}
# is_scalar {{{
is_scalar <- function(x) length(x) == 1L
on_fail(is_scalar) <- function (call, env) {
    paste(deparse(call$x), "is not a scalar (length one vector).")
}
# }}}
# have_same_len {{{
have_same_len <- function (x, y) {
    x_len_fun <- if(is.data.frame(x)) nrow else length
    y_len_fun <- if(is.data.frame(y)) nrow else length
    x_len_fun(x) == y_len_fun(y)
}
on_fail(have_same_len) <- function (call, env) {
    paste(deparse(call$x), "and", deparse(call$y), "do not have the same length.")
}
# }}}
# is_in_range {{{
is_in_range <- function (x, minimum, lower_incbounds, maximum, upper_incbounds) {
    in_range(x, make_field_range(minimum, lower_incbounds, maximum, upper_incbounds))
}
on_fail(is_in_range) <- function (call, env) {
    r <- make_field_range(
        eval(call$minimum, env), eval(call$lower_incbounds, env),
        eval(call$maximum, env), eval(call$upper_incbounds, env)
    )
    paste(deparse(call$x), "is not in range", r)
}
# }}}
# is_named {{{
is_named <- function (x) !is.null(names(x))
# }}}

# has_name {{{
has_name <- function(x, which) {
    assert(is.character(which), msg = "Non-character `which` in has_name().")
    all(which %in% names(x))
}
on_fail(has_name) <- function (call, env) {
    nms <- eval(call$which, env)
    paste(deparse(call$x), "does not have",
        sprintf(
            ngettext(length(nms),
                "name %s.", "all required names %s."
            ),
            collapse(nms)
        )
    )
}
# }}}
# has_ext {{{
has_ext <- function (path, exts) tolower(tools::file_ext(path)) %in% tolower(exts)
on_fail(has_ext) <- function (call, env) {
    ext <- eval(call$which, env)
    paste(deparse(call$path),
        sprintf(
            ngettext(length(ext),
                "should have the extension of %s.",
                "should have one of the extensions %s."
            ),
            collapse(ext)
        )
    )
}
# }}}
# is_windows {{{
is_windows <- function () .Platform$OS.type == 'windows'
# }}}
# is_linux {{{
is_linux <- function () Sys.info()["sysname"] == "Linux"
# }}}
# is_macos {{{
is_macos <- function () Sys.info()["sysname"] == "Darwin"
# }}}
