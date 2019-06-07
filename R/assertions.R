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
assert <- function(..., msg = NULL, prefix = NULL, err_type = NULL, env = parent.frame()) {
    assertions <- eval(substitute(alist(...)))

    for (assertion in assertions) {
        val <- eval(assertion, env)
        # all TRUE
        if (length(val) && !any(is.na(val)) && all(val)) next

        # get error type
        if (is.null(err_type)) {
            fnm <- deparse(assertion[[1L]])
            if (stringi::stri_startswith_fixed(fnm, "is") || stringi::stri_startswith_fixed(fnm, "are")) {
                err_type <- c(paste0("error_not_", sub("[a-z]+_", "", fnm)), "error_assertion")
            } else {
                err_type <- c(paste0("error_not_", fnm), "error_assertion")
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
                    length(val), "%s is not TRUE.", "%s are not all TRUE."),
                    surround(deparse(assertion, width.cutoff = 60L))
                )
            )
        }

        # match.call does not do well with primitive functions
        if (!is.primitive(f)) assertion <- match.call(f, assertion)

        if (is.null(prefix)) {
            abort(err_type, msg_fun(assertion, env))
        } else {
            # change message prefix
            stopifnot(is_string(prefix))
            abort(err_type,
                paste0(prefix, sub(".*? (is|are|should|does|do|must|can|contains)", " \\1", msg_fun(assertion, env)))
            )
        }
    }
    TRUE
}
# }}}

# is_version {{{
is_version <- function (ver) {
    !is.na(standardize_ver(ver))
}
# }}}
#' Check for Idd, Idf and Epw objects
#'
#' These functions test if input is a valid object of Idd, Idf, Epw and other
#' main classes.
#'
#' `is_eplus_ver()` returns `TRUE` if input is a valid EnergyPlus version.
#'
#' `is_idd_ver()` returns `TRUE` if input is a valid EnergyPlus IDD version.
#'
#' `is_eplus_path()` returns `TRUE` if input path is a valid EnergyPlus path,
#' i.e. a path where there is an `energyplus` executable and an `Energy+.idd`
#' file.
#'
#' `is_idd()` returns `TRUE` if input is an Idd object.
#'
#' `is_idf()` returns `TRUE` if input is an Idf object.
#'
#' `is_iddobject()` returns `TRUE` if input is an IddObject object.
#'
#' `is_idfobject()` returns `TRUE` if input is an IdfObject object.
#'
#' `is_epw()` returns `TRUE` if input is an Epw object.
#'
#' @param ver A character or numeric vector with suitable numeric version
#' strings.
#' @param strict If `FALSE`, `ver` can be a special string "latest" which
#' represents the latest version.
#' @return A logical vector.
#' @rdname assertion
#' @export
#' @examples
#' is_eplus_ver(8.8)
#' is_eplus_ver(8.0)
#' is_eplus_ver("latest", strict = FALSE)
#'
#' is_idd_ver("9.0.1")
#' is_idd_ver("8.0.1")
#'
#' is_eplus_path("C:/EnergyPlusV9-0-0")
#' is_eplus_path("/usr/local/EnergyPlus-9-0-1")
#'
#' is_idd(use_idd(8.8, download = "auto"))
#'
#' idf <- read_idf(system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr"),
#'     idd = use_idd(8.8, download = "auto"))
#' is_idf(idf)
#'
#' is_iddobject(idd_object(8.8, "Version"))
#'
#' is_idfobject(idf_object(idf, 1))
#'
#' \dontrun{
#' is_epw(read_epw(download_weather("los angeles.*tmy3", type = "epw", ask = FALSE, max_match = 1)))
#' }
# is_eplus_ver {{{
is_eplus_ver <- function (ver, strict = FALSE) {
    ver <- standardize_ver(ver, strict = strict, complete = FALSE)
    ver <- lapply(ver, match_minor_ver, all_ver = c(ALL_EPLUS_VER, names(.globals$eplus_config)), verbose = FALSE)
    !viapply(ver, is.na)
}
on_fail(is_eplus_ver) <- function (call, env) {
    paste0(deparse(call$ver), " is not a valid or supported EnergyPlus version. ",
        "Only EnergyPlus v8.3.0 and after are supported."
    )
}
# }}}

#' @rdname assertion
#' @export
# is_idd_ver {{{
is_idd_ver <- function (ver, strict = FALSE) {
    ver <- standardize_ver(ver, strict = strict, complete = FALSE)
    !is.na(match_minor_ver(ver, c(ALL_IDD_VER, names(.globals$idd)), verbose = FALSE))
}
on_fail(is_idd_ver) <- function (call, env) {
    paste0(deparse(call$ver), " is not a valid Idd version.")
}
# }}}

#' @param path A path to test.
#' @rdname assertion
#' @export
# is_eplus_path {{{
is_eplus_path <- function (path) {
    eplus <- paste0("energyplus", if (is_windows()) ".exe" else "")
    # in case input is a numeric version
    path <- as.character(path)
    dir.exists(path) & file.exists(file.path(path, eplus)) & file.exists(file.path(path, "Energy+.idd"))
}
on_fail(is_eplus_path) <- function (call, env) {
    paste(deparse(call$path), "is not a valid EnergyPlus installation path",
        "where `energyplus` executable and `Energy+.idd` should exist."
    )
}
# }}}

#' @param x An object to test.
#' @rdname assertion
#' @export
# is_idd {{{
is_idd <- function (x) inherits(x, "Idd")
on_fail(is_idd) <- function (call, env) {
    paste(deparse(call$x), "is not an Idd object.")
}
# }}}

#' @rdname assertion
#' @export
# is_idf {{{
is_idf <- function (x) inherits(x, "Idf")
on_fail(is_idf) <- function (call, env) {
    paste(deparse(call$x), "is not an Idf object.")
}
# }}}

#' @rdname assertion
#' @export
# is_iddobject {{{
is_iddobject <- function (x) inherits(x, "IddObject")
on_fail(is_iddobject) <- function (call, env) {
    paste(deparse(call$x), "is not an IddObject object.")
}
# }}}

#' @rdname assertion
#' @export
# is_idfobject {{{
is_idfobject <- function (x) inherits(x, "IdfObject")
on_fail(is_idfobject) <- function (call, env) {
    paste(deparse(call$x), "is not an IdfObject object.")
}
# }}}

#' @rdname assertion
#' @export
# is_epw {{{
is_epw <- function (x) inherits(x, "Epw")
on_fail(is_epw) <- function (call, env) {
    paste(deparse(call$x), "is not an Epw object.")
}
# }}}

# is_range {{{
is_range <- function (x) {
    inherits(x, "Range")
}
# }}}

# no_na {{{
no_na <- function (x, coerce = FALSE) {
    all(!is.na(x))
}
on_fail(no_na) <- function (call, env) {
    end <- if (eval(call$coerce, env)) " after coercion." else " ."
    paste0(deparse(call$x), "should not contain any NA", end)
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
# is_strnum {{{
is_strnum <- function(x) {
    length(x) == 1L && is.character(x) && all(!is.na(suppressWarnings(as.double(x))))
}
on_fail(is_strnum) <- function (call, env) {
    paste(deparse(call$x), "is not a coercible number.")
}
# }}}
# is_integer {{{
is_integer <- function(x) {
    length(x) == 1L && !is.na(x) && (is.integer(x) || (is.numeric(x) && all(x == trunc(x))))
}
on_fail(is_integer) <- function (call, env) {
    paste(deparse(call$x), "is neither a length one integer nor can be coerced into one.")
}
# }}}
# is_strint {{{
is_strint <- function(x) {
    is_integer(suppressWarnings(as.double(x)))
}
on_fail(is_strint) <- function (call, env) {
    paste(deparse(call$x), "is not an integer-coercible format.")
}
# }}}
# is_count {{{
is_count <- function (x, zero = FALSE) {
    if (!is_integer(x)) return(FALSE)
    if (zero) x >= 0L else x > 0L
}
on_fail(is_count) <- function (call, env) {
    zero <- eval(call$zero, env)
    info <- if (!is.null(zero) && zero) "non-nagitive integer" else "positive integer"
    paste(deparse(call$x), "is not a count (a length one", info, "vector).")
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
# are_string {{{
are_string <- function(x) is.character(x) && all(!is.na(x))
on_fail(are_string) <- function (call, env) {
    paste(deparse(call$x), "is not a character vector.")
}
# }}}
# are_strint {{{
are_strint <- function(x) {
    are_integer(suppressWarnings(as.double(x)))
}
on_fail(are_strint) <- function (call, env) {
    paste(deparse(call$x), "is not an integer-coercible vector.")
}
# }}}
# are_integer {{{
are_integer <- function(x) {
    is.numeric(x) && all(!is.na(x)) && all(x == trunc(x))
}
on_fail(are_integer) <- function (call, env) {
    paste(deparse(call$x), "is neither an integer vector or can be converted into one.")
}
# }}}
# are_number {{{
are_number <- function (x) {
    is.numeric(x) && all(!is.na(x))
}
on_fail(are_number) <- function (call, env) {
    paste(deparse(call$x), "is not a number vector.")
}
# }}}
# are_strnum {{{
are_strnum <- function(x) {
    is.character(x) & all(!is.na(suppressWarnings(as.double(x))))
}
on_fail(are_strnum) <- function (call, env) {
    paste(deparse(call$x), "is not a number-coercible vector.")
}
# }}}
# are_count {{{
are_count <- function (x, zero = FALSE) {
    are_integer(x) && if (zero) all(x >= 0L) else all(x > 0L)
}
on_fail(are_count) <- function (call, env) {
    zero <- eval(call$zero, env)
    info <- if (!is.null(zero) && zero) "non-nagitive integer" else "positive integer"
    paste(deparse(call$x), "are not counts (a", info, "vector).")
}
# }}}
# has_len {{{
has_len <- function (x, len, step = NULL) {
    if (is_range(len)) {
        in_range(length(x), len)
    } else if (is_integer(len)){
        if (is.null(step)) {
            length(x) == len
        } else {
            if (!is_integer(step)) stop("`step` should be either NULL or an integer.")
            length(x) >= len && ((length(x) - len) %% step == 0L)
        }
    } else if (are_integer(len)) {
        if (!is.null(step)) {
            stop("`step` should not be provided when `len` is an integer vector.")
        }
        length(x) %in% len
    } else {
        stop("`len` should be either a range or an integer vector.")
    }
}
on_fail(has_len) <- function (call, env) {
    len <- eval(call$len, env)
    step <- eval(call$step, env)

    if (is_range(len)) {
        paste0(deparse(call$x), " does not meet the required length range of ", len, ".")
    } else {
        if (is.null(step)) {
            paste0(deparse(call$x), " does not have the required length of ", collapse(len, or = TRUE), ".")
        } else {
            paste0(deparse(call$x), " does not have the required length pattern `", len, " + " , step, " x N`.")
        }
    }
}
# }}}
# have_same_len {{{
have_same_len <- function (x, y) {
    NROW(x) == NROW(y)
}
on_fail(have_same_len) <- function (call, env) {
    paste(deparse(call$x), "and", deparse(call$y), "do not have the same length.")
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
on_fail(in_range) <- function (call, env) {
    paste(deparse(call$x), "is not in range", eval(call$range, env))
}
# }}}
# is_named {{{
is_named <- function (x) !is.null(names(x))
on_fail(is_named) <- function (call, env) {
    paste(deparse(call$x), "must be named.")
}
# }}}
# is_choice {{{
is_choice <- function (x, choices) {
    is.character(x) & stri_trans_tolower(x) %in% stri_trans_tolower(choices)
}
on_fail(is_choice) <- function (call, env) {
    paste0(deparse(call$x), " should be one of ", collapse(eval(call$choices, env)))
}
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
has_ext <- function (path, ext) tolower(tools::file_ext(path)) %in% tolower(ext)
on_fail(has_ext) <- function (call, env) {
    ext <- eval(call$ext, env)
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

# is_epwdate {{{
is_epwdate <- function (x) {
    length(x) == 1L && !is.na(epw_date(x))
}
on_fail(is_epwdate) <- function (call, env) {
    paste0(deparse(call$x), " is not a valid EPW date specification.")
}
# }}}
# are_epwdate {{{
are_epwdate <- function (x) {
    all(!is.na(epw_date(x)))
}
on_fail(are_epwdate) <- function (call, env) {
    paste0(deparse(call$x), " contains invalid EPW date specification.")
}
# }}}
# not_epwdate_realyear {{{
not_epwdate_realyear <- function (x, scalar = FALSE, zero = TRUE) {
    d <- epw_date(x)
    r <- !is.na(d) & get_epwdate_type(d) != 3L
    if (!zero) r <- r & get_epwdate_type(d) != 0L
    if (scalar) {
        length(x) == 1L && all(r)
    } else {
        r
    }
}
on_fail(not_epwdate_realyear) <- function (call, env) {
    s <- eval(call$scalar, env)
    if (!is.null(s) && s) {
        paste0(deparse(call$x), " should not be EPW real-year date specification.")
    } else {
        paste0(deparse(call$x), " should not contain any EPW real-year date specification.")
    }
}
# }}}
# not_epwdate_weekday {{{
not_epwdate_weekday <- function (x, scalar = FALSE, zero = TRUE) {
    d <- epw_date(x)
    r <- !is.na(d) & get_epwdate_type(d) != 5L
    if (!zero) r <- r & get_epwdate_type(d) != 0L
    if (scalar) {
        length(x) == 1L && all(r)
    } else {
        r
    }
}
on_fail(not_epwdate_weekday) <- function (call, env) {
    s <- eval(call$scalar, env)
    if (!is.null(s) && s) {
        paste0(deparse(call$x), " is not valid EPW Julian day or Month/Day date specification.")
    } else {
        paste0(deparse(call$x), " contains invalid EPW Julian day or Month/Day date specification.")
    }
}
# }}}
# is_unique {{{
is_unique <- function (x) {
    anyDuplicated(x) == 0L
}
on_fail(is_unique) <- function (call, env) {
    paste0(deparse(call$x), " should not contain any duplication.")
}
# }}}
# is_wday {{{
is_wday <- function (x) {
    length(x) == 1L && !is.na(get_epw_wday(x))
}
on_fail(is_wday) <- function (call, env) {
    paste0(deparse(call$x), " is not a valid day of week format.")
}
# }}}
# are_wday {{{
are_wday <- function (x) {
    all(!is.na(get_epw_wday(x)))
}
on_fail(is_wday) <- function (call, env) {
    paste0(deparse(call$x), " contains invalid day of week format.")
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
