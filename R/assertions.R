#' @importFrom assertthat assert_that "on_failure<-"
#' @importFrom tools file_ext

# is_eplus_ver {{{
is_eplus_ver <- function (ver) {
    if (length(ver) != 1L) return(FALSE)
    if (is.numeric_version(ver)) TRUE
    ver_fmt <- "^[78]\\.[0-9]$"
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
# is_pre_parsed {{{
is_pre_parsed <- function (ver) {
    is_supported_ver(ver) && as.numeric_version(ver) > 8.5
}
# }}}
# }}}
# is_eplus_exists {{{
is_eplus_exists <- function (eplus_exe) {
    file.exists(eplus_exe)
}

on_failure(is_eplus_exists) <- function (call, env) {
    paste0("EnergyPlus does not exist")
}
# }}}
# has_macro {{{1
has_macro <- function (str) {
    any(sapply(macro_dict, function(x) any(startsWith(str, x))))
}
# }}}1
# has_hvac_template {{{
has_hvac_template <- function (idf) {
    idf$class[startsWith(class, "HVACTemplate"), list(unique(class))][, .N] > 0L
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

on_failure(is_idf) <- function (call, env) {
    paste0(deparse(call$x), " is not an IdfObject object")
}
# }}}
# is_imf {{{
is_imf <- function (x) inherits(x, "Imf")

on_failure(is_imf) <- function (call, env) {
    paste0(deparse(call$x), " is not an Imf object")
}
# }}}
# is_model {{{
is_model <- function (x) inherits(x, "IMF") || inherits(x, "IDF")

on_failure(is_model) <- function (call, env) {
    paste0(deparse(call$x), " is neither an IDF nor IMF object")
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
    all(abs(x - round(x)) < .Machine$double.eps^0.5)
}
on_failure(is_integerish) <- function(call, env) {
  paste0(deparse(call$x), " is neither an integer nor can be converted into an integer")
}
# }}}
# is_flag {{{
is_flag <- function(x) is.logical(x) && length(x) == 1
on_failure(is_flag) <- function(call, env) {
  paste0(deparse(call$x), " is not a flag (a length one logical vector).")
}
# }}}
# is_writeable {{{
is_writeable <- function(path) {
    assert_that(is_string(path), file.exists(path))
    file.access(path, mode = 2)[[1]] == 0
}
on_failure(is_writeable) <- function (call, env) {
    paste0(eval(call$path, env), " is not writeable")
}
# }}}
# is_readable {{{
is_readable <- function(path) {
    assert_that(is_string(path), file.exists(path))
    file.access(path, mode = 4)[[1]] == 0
}
on_failure(is_readable) <- function (call, env) {
    paste0(eval(call$path, env), " is not readable")
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
# has_output_ext {{{
has_output_ext <- function (x) {
    has_ext(x, "csv") || has_ext(x, "txt") || has_ext(x, "tab")
}

on_failure(has_output_ext) <- function (call, env = parent.env) {
    path <- eval(call$path, env)
    ext <- eval(call$ext, env)
    msg("File ", backtick(basename(path)), " is not an EnergyPlus output data file.")
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
