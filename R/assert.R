#' @importFrom tools file_ext
#' @importFrom checkmate test_class test_r6
#' @include constants.R
NULL

# convert_to_ver {{{
convert_to_eplus_ver <- function (ver, strict = FALSE, all_ver = c(ALL_EPLUS_VER, names(.globals$eplus)), max = TRUE, verbose = FALSE) {
    ver <- standardize_ver(ver, strict = strict, complete = FALSE)
    res <- lapply(ver, match_minor_ver, all_ver = all_ver, type = "eplus", max = max, verbose = verbose)
    if (max) do.call(c, res) else res
}
convert_to_idd_ver <- function (ver, strict = FALSE, all_ver = c(ALL_IDD_VER, names(.globals$idd)), max = TRUE, verbose = FALSE) {
    ver <- standardize_ver(ver, strict = strict, complete = FALSE)
    res <- lapply(ver, match_minor_ver, all_ver = all_ver, type = "idd", max = max, verbose = verbose)
    if (max) do.call(c, res) else res
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
    !is.na(convert_to_eplus_ver(ver, strict))
}
# }}}

#' @rdname assertion
#' @export
# is_idd_ver {{{
is_idd_ver <- function (ver, strict = FALSE) {
    !is.na(convert_to_eplus_ver(ver, strict))
}
# }}}

#' @param path A path to test.
#' @rdname assertion
#' @export
# is_eplus_path {{{
is_eplus_path <- function (path) {
    eplus <- paste0("energyplus", if (is_windows()) ".exe" else "")
    eplus1 <- paste0("EnergyPlus", if (is_windows()) ".exe" else "")
    # in case input is a numeric version
    path <- as.character(path)
    dir.exists(path) &
    (file.exists(file.path(path, eplus)) | file.exists(file.path(path, eplus1))) &
    file.exists(file.path(path, "Energy+.idd"))
}
# }}}

#' @param x An object to test.
#' @rdname assertion
#' @export
# is_idd {{{
is_idd <- function (x) test_r6(x, "Idd")
# }}}

#' @rdname assertion
#' @export
# is_idf {{{
is_idf <- function (x) test_r6(x, "Idf")
# }}}

#' @rdname assertion
#' @export
# is_iddobject {{{
is_iddobject <- function (x) test_r6(x, "IddObject")
# }}}

#' @rdname assertion
#' @export
# is_idfobject {{{
is_idfobject <- function (x) test_r6(x, "IdfObject")
# }}}

#' @rdname assertion
#' @export
# is_epw {{{
is_epw <- function (x) test_r6(x, "Epw")
# }}}

# is_rdd {{{
is_rdd <- function (x) checkmate::test_class(x, "RddFile")
is_mdd <- function (x) checkmate::test_class(x, "MddFile")
# }}}

# is_range {{{
is_range <- function (x) {
    checkmate::test_list(x, len = 4L) && checkmate::test_class(x, "Range")
}
# }}}

# assert_strint {{{
check_strint <- function (x, len = NULL, min.len = NULL, max.len = NULL, names = NULL, null.ok = FALSE) {
    chk <- checkmate::check_character(x, any.missing = FALSE, len = len, min.len = max.len, names = names, null.ok = null.ok)
    if (isTRUE(chk)) TRUE else chk
    num <- suppressWarnings(as.double(x))
    chk <- checkmate::check_integerish(num, any.missing = FALSE)
    if (isTRUE(chk)) TRUE
    else "Must be a vector with integer-coercible format"
}
test_strint <- checkmate::makeTestFunction(check_strint)
assert_strint <- function (x, len = NULL, coerce = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    res <- check_strint(x)
    checkmate::makeAssertion(x, res, .var.name, add)
    if (isTRUE(coerce)) storage.mode(x) <- "integer"
    x
}
# }}}
# assert_length {{{
check_length <- function (x, len, step = NULL) {
    if (is_range(len)) {
        res <- if (in_range(length(x), len)) TRUE
            else paste0("Must have length in range ", len, ", but has length ", length(x))
    } else if (checkmate::test_count(len)){
        if (is.null(step)) {
            length(x) == len
            res <- if (length(x) == len) TRUE
            else paste0("Must have length ", len, ", but has length ", length(x))
        } else {
            if (!checkmate::test_count(step, positive = TRUE)) stop("'step' should be either NULL or an integer")
            res <- if (length(x) >= len && ((length(x) - len) %% step == 0L)) TRUE
            else paste0("Must have length of pattern '", len, " + " , step, " x N'")
        }
    } else if (checkmate::test_integerish(len, lower = 0L)) {
        if (!is.null(step)) {
            stop("'step' should not be provided when 'len' is an integer vector")
        }
        res <- if (length(x) %in% len) TRUE
            else paste0("Must have length ", collapse(len, or = TRUE), ", but has length ", length(x))
    } else {
        stop("'len' should be either a range or an integer vector")
    }
}
assert_length <- checkmate::makeAssertionFunction(check_length)
# }}}
# assert_same_len {{{
check_same_len <- function (x, y) {
    if (NROW(x) == NROW(y)) TRUE else "Must have same length"
}
test_same_len <- checkmate::makeTestFunction(check_same_len)
assert_same_len <- function(x, y, .var.name = paste(checkmate::vname(x), "and", checkmate::vname(y)), add = NULL) {
    res <- check_same_len(x, y)
    checkmate::makeAssertion(x, res, .var.name, add)
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
# check_range {{{
check_range <- function (x, range) {
    res <- in_range(x, range)
    if (all(res)) TRUE
    else paste("Must in range", range)
}
# }}}
# is_choice {{{
is_choice <- function (x, choices) {
    is.character(x) & stri_trans_tolower(x) %chin% stri_trans_tolower(choices)
}
# on_fail(is_choice) <- function (call, env) {
#     paste0(deparse(call$x), " should be one of ", collapse(eval(call$choices, env)))
# }
# }}}

# has_names {{{
has_names <- function(x, names) names %chin% names(x)
# }}}
# has_ext {{{
has_ext <- function (path, ext) tolower(tools::file_ext(path)) %chin% ext
# }}}

assert_epwdate <- function (x, len = NULL, null.ok = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    x <- assert_vector(x, len = len, null.ok = null.ok)
    if (is.null(x)) return(x)
    x <- epw_data(x)
    res <- if (!checkmate::anyMissing(x)) TRUE
    else "Must be a vector of valid EPW date specifications"
    makeAssertion(x, res, .var.name, add)
}
assert_wday <- function (x, len = NULL, null.ok = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    x <- assert_vector(x, len = len, null.ok = null.ok)
    if (is.null(x)) return(x)
    x <- get_epw_wday(x)
    res <- if (!checkmate::anyMissing(x)) TRUE
    else "Must be a vector of valid EPW day of week specifications"
    makeAssertion(x, res, .var.name, add)
}

# is_epwdate {{{
is_epwdate <- function (x) {
    length(x) == 1L && !is.na(epw_date(x))
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
# on_fail(not_epwdate_realyear) <- function (call, env) {
#     s <- eval(call$scalar, env)
#     if (!is.null(s) && s) {
#         paste0(deparse(call$x), " should not be EPW real-year date specification.")
#     } else {
#         paste0(deparse(call$x), " should not contain any EPW real-year date specification.")
#     }
# }
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
# on_fail(not_epwdate_weekday) <- function (call, env) {
#     s <- eval(call$scalar, env)
#     if (!is.null(s) && s) {
#         paste0(deparse(call$x), " is not valid EPW Julian day or Month/Day date specification.")
#     } else {
#         paste0(deparse(call$x), " contains invalid EPW Julian day or Month/Day date specification.")
#     }
# }
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
