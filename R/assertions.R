#' @importFrom assertthat assert_that "on_failure<-"
#' @importFrom tools file_ext

# is_eplus_ver {{{
is_eplus_ver <- function (ver) {
    assert_that(is_scalar(ver))
    if (is.numeric_version(ver)) TRUE
    ver_fmt <- "^[78]\\.[0-9](\\.[0-9]){0,1}$"
    if (is_integerish(ver)) ver <- paste0(ver, ".0")
    grepl(ver_fmt, as.character(ver))
}

on_failure(is_eplus_ver) <- function (call, env) {
    paste0(backtick(eval(call$ver, env)), " is not a valid EnergyPlus version (which should be a number or string with format '[78].[0-9]').")
}
# }}}
# is_supported_ver {{{
is_supported_ver <- function (ver) {
    if (!is_eplus_ver(ver)) return(FALSE)
    ver <- as.numeric_version(ver)
    if (!is.na(ver[, 3L])) {
        if (ver[, 3L] != 0L) {
            return(FALSE)
        } else {
            ver > 8.0
        }
    } else {
        ver > 8.0
    }
}

on_failure(is_supported_ver) <- function (call, env) {
    paste0("Currently EnergyPlus v8.1 to v8.9 are supported.")
}
# }}}
# is_parsed_idd_ver {{{
is_parsed_idd_ver <- function (ver) {
    as.character(standardize_ver(ver)) %in% names(.globals$idd)
}
# }}}

# is_idd {{{
is_idd <- function (x) inherits(x, "Idd")

on_failure(is_idd) <- function (call, env) {
    paste0(deparse(call$x), " is not an Idd object")
}
# }}}
# is_idf {{{
is_idf <- function (x) inherits(x, "Idf")

on_failure(is_idf) <- function (call, env) {
    paste0(deparse(call$x), " is not an Idf object")
}
# }}}
# is_idfobject {{{
is_idfobject <- function (x) inherits(x, "IdfObject")

on_failure(is_idfobject) <- function (call, env) {
    paste0(deparse(call$x), " is not an IdfObject object")
}
# }}}
# is_epw {{{
is_epw <- function (x) inherits(x, "Epw")

on_failure(is_epw) <- function (call, env) {
    paste0(deparse(call$x), " is not an Epw object")
}
# }}}
# not_empty {{{
not_empty <- function (x) {
    all((dim(x) %||% length(x)) != 0)
}
on_failure(not_empty) <- function (call, env) {
    paste0(deparse(call$x), " is empty")
}
# }}}
# is_empty {{{
is_empty <- function (x) {
    !not_empty(x)
}
on_failure(is_empty) <- function (call, env) {
    paste0(deparse(call$x), " is not empty")
}
# }}}
# is_count {{{
is_count <- function (x) {
    if (length(x) != 1) return(FALSE)
    if (!is_integerish(x)) return(FALSE)
    x > 0
}
on_failure(is_count) <- function(call, env) {
  paste0(deparse(call$x), " is not a count (a single positive integer)")
}
# }}}
# are_count {{{
are_count <- function (x) {
    if (!is_integerish(x)) return(FALSE)
    all(x > 0)
}
on_failure(are_count) <- function(call, env) {
  paste0(deparse(call$x), " is not a positive integer vector.")
}
# }}}
# is_string {{{
is_string <- function(x) is.character(x) && length(x) == 1
on_failure(is_string) <- function(call, env) {
    paste0(deparse(call$x), " is not a string (a length one character vector).")
}
# }}}
# is_scalar {{{
is_scalar <- function(x) {
    length(x) == 1L
}
on_failure(is_scalar) <- function(call, env) {
  paste0(deparse(call$x), " is not a scalar.")
}
# }}}
# is_integerish {{{
is_integerish <- function(x) {
    # is.integer(x) || (is.numeric(x) && all(x == as.integer(x)))
    if (!is.numeric(x)) return (FALSE)
    all(abs(x - round(x)) < .Machine$double.eps^0.5)
}
on_failure(is_integerish) <- function(call, env) {
  paste0(deparse(call$x), " is neither an integer nor can be converted into an integer")
}
# }}}
# are_integerish {{{
are_integerish <- function(x) {
    if (!is.numeric(x)) return (rep(FALSE, length(x)))
    abs(x - round(x)) < .Machine$double.eps^0.5
}
# }}}
# is_same_len {{{
is_same_len <- function (x, y) {
    length(x) == length(y)
}
on_failure(is_same_len) <- function (call, env) {
    paste0(backtick(deparse(call$x)), " and ", backtick(deparse(call$y)),
      " does not have the same length.")
}
# }}}

# has_name {{{
has_name <- function(x, which) which %in% names(x)
on_failure(has_name) <- function(call, env) {
    paste0(deparse(call$x), " does not have name ", eval(call$which, env))
}
# }}}
# has_names {{{
has_names <- function(x, which) all(which %in% names(x))
on_failure(has_names) <- function(call, env) {
    paste0(deparse(call$x), " does not have all required names: ",
           backtick_collapse(eval(call$which, env)), ".")
}
# }}}
# has_ext {{{
has_ext <- function (path, ext) {
    ext == tolower(tools::file_ext(path))
}

on_failure(has_ext) <- function (call, env = parent.env) {
    path <- eval(call$path, env)
    ext <- eval(call$ext, env)
    msg("File ", backtick(basename(path)), " does not have extension ", backtick(ext), ".")
}
# }}}
# has_exts {{{
has_exts <- function (path, exts) {
    tolower(tools::file_ext(path)) %in% exts
}

on_failure(has_exts) <- function (call, env = parent.env) {
    path <- eval(call$path, env)
    ext <- eval(call$ext, env)
    msg("File ", backtick(basename(path)), " should have one of extensions ", backtick_collapse(ext), ".")
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
