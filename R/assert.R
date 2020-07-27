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

# has_names {{{
has_names <- function(x, names) names %chin% names(x)
# }}}

# has_ext {{{
has_ext <- function (path, ext) tolower(tools::file_ext(path)) %chin% ext
# }}}

# is_epwdate {{{
is_epwdate <- function (x) {
    length(x) == 1L && !is.na(epw_date(x))
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
