#' @importFrom stringi stri_enc_toutf8 stri_replace_all_charclass stri_trans_tolower
#' @importFrom checkmate assert_number assert_flag assert_class assert_vector
#' @importFrom checkmate assert_character assert_names
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
collapse <- function (x, out = "'", or = FALSE, max_num = 5L) {
    if (is.null(out)) {
        s <- as.character(x)
    } else {
        out <- as.character(out)
        if (length(out) == 1L) {
            out <- c(out, out)
        }
        s <- paste0(out[1L], x, out[2L])
    }
    if (length(s) == 1L) return (s)

    if (!is.null(max_num)) {
        assert_count(max_num)
        l <- length(s)
        if (max_num + 1L <= l) {
            s <- s[1:(max_num + 1L)]
            s[length(s)] <- "etc"
        }
    }
    b <- paste0(s[-length(s)], collapse = ", ")
    e <- s[length(s)]
    if (is.null(or)) {
        paste0(b, ", ", e)
    } else if (or) {
        paste0(b, " or ", e)
    } else {
        paste0(b, " and ", e)
    }
}
# }}}

# surround {{{
surround <- function (x, out = "'") {
    if (is.null(out)) return(as.character(x))
    out <- as.character(out)
    if (length(out) == 1L) {
        out <- c(out, out)
    }
    paste0(out[1L], x, out[2L])
}
# }}}

# get_self_env{{{
#' Get the enclosed environment of an R6 object
#'
#' @details
#'
#' `get_super_env()` returns the `super` enclosed environment of an [R6::R6Class()]
#' object.
#'
#' `get_self_env()` returns the `self` enclosed environment of an [R6::R6Class()]
#' object.
#'
#' `get_priv_env()` returns the `private` enclosed environment of an [R6::R6Class()]
#' object.
#'
#' @param x An R6 object.
#'
#' @return An environment.
#'
#' @keywords internal
#' @export
#' @name get_env
get_self_env <- function (x) {
    .subset2(.subset2(x, ".__enclos_env__"), "self")
}
# }}}

# get_priv_env{{{
#' @keywords internal
#' @export
#' @name get_env
get_priv_env <- function (x) {
    .subset2(.subset2(x, ".__enclos_env__"), "private")
}
# }}}

# get_super_env{{{
#' @keywords internal
#' @export
#' @name get_env
get_super_env <- function (x) {
    .subset2(.subset2(x, ".__enclos_env__"), "super")
}
# }}}

# pad: borrowed from `r-lib/cli` {{{
rpad <- function(x, char = " ", width = NULL) {
    if (!length(x)) return(x)
    w <- nchar(x, type = "width")
    if (is.null(width)) width <- max(w)
    paste0(x, stringi::stri_dup(char, pmax(width - w, 0)))
}

lpad <- function(x, char = " ", width = NULL) {
    if (!length(x)) return(x)
    w <- nchar(x, type = "width")
    if (is.null(width)) width <- max(w)
    paste0(stringi::stri_dup(char, pmax(width - w, 0)), x)
}
# }}}

# read_lines {{{
read_lines <- function(input, trim = TRUE, encoding = "unknown", ...) {
    dt <- tryCatch(
        fread(input = input, sep = NULL, header = FALSE, col.names = "string",
            encoding = encoding, strip.white = FALSE, ...),
        warning = function (w) if (grepl("has size 0", conditionMessage(w))) data.table() else warning(w),
        error = function (e) abort(paste0("Failed to read input file. ", conditionMessage(e)), "read_lines")
    )
    if (!nrow(dt)) return(data.table(string = character(0L), line = integer(0L)))
    set(dt, j = "line", value = seq_along(dt[["string"]]))

    if (trim) set(dt, j = "string", value = stri_trim_both(dt[["string"]]))

    setcolorder(dt, c("line", "string"))

    dt
}
# }}}

# write_lines {{{
# NOTE: IDFEditor will crash if a large IDF file was saved with LF eol on
#       Windows.
write_lines <- function (x, file = "", append = FALSE) {
    if (inherits(x, "data.table")) {
        assert_names(names(x), must.include = "string")
        fwrite(x[, list(string)], file = file, col.names = FALSE, quote = FALSE, append = append)
    } else {
        assert_character(x)
        fwrite(data.table(x), file = file, col.names = FALSE, quote = FALSE, append = append)
    }
}
# }}}

# os_type: Return operation system type {{{
# nocov start
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
# nocov end
# }}}

# standardize_ver {{{
standardize_ver <- function (ver, strict = FALSE, complete = TRUE) {
    if (!strict && is.character(ver)) {
        ver[ver == "latest"] <- LATEST_EPLUS_VER
    }

    if (is.numeric(ver)) {
        int <- (!is.na(ver)) & (is.integer(ver) | (is.numeric(ver) & (ver == trunc(ver))))
        if (any(int)) ver[int] <- paste0(ver[int], ".0")
    }

    if (!inherits(ver, "numeric_version")) ver <- numeric_version(ver, strict = FALSE)

    # only keep major.minor.patch, and remove others
    has_trail <- suppressWarnings(!is.na(ver[, 4L]))
    ver[has_trail] <- ver[has_trail, 1L:3L]

    # complete patch version to 0 if not exist
    if (complete && any(!is.na(ver) & suppressWarnings(is.na(ver[, 3L])))) {
        ver[!is.na(ver) & suppressWarnings(is.na(ver[, 3L])), 3L] <- 0L
    }

    ver
}
# }}}

# match_minor_ver {{{
match_minor_ver <- function (ver, all_ver, type = c("idd", "eplus"), max = TRUE, verbose = TRUE) {
    checkmate::assert_class(ver, "numeric_version")
    checkmate::assert_vector(ver, len = 1L)
    if (!length(all_ver)) return(numeric_version(NA, strict = FALSE))
    all_ver <- unique(all_ver)
    ori_ver <- ver

    if (is.na(ver[, 3L])) {
        ver <- numeric_version(all_ver[ver[, 1L:2L] == numeric_version(all_ver)[, 1L:2L]], strict = FALSE)
    } else {
        ver <- numeric_version(all_ver[as.character(ver) == all_ver], strict = FALSE)
    }

    if (!length(ver)) {
        ver <- numeric_version(NA, strict = FALSE)
    } else if (length(ver) > 1L) {
        if (max) {
            ver <- max(ver)

            if (verbose) {
                type <- match.arg(type)
                key <- switch(type, idd = "IDD", eplus = "EnergyPlus")

                verbose_info("Multiple versions found for ", key, " v", ori_ver, ": ",
                    collapse(paste0("v", ver)), ". ",
                    "The last patched version v", max(ver), " will be used. ",
                    "Please explicitly give the full version if you want to use the other versions."
                )
            }
        }
    }

    ver
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
        stop("'x' must be a vector")
    }
}
# }}}

# vlapply {{{
vlapply <- function (x, fun, ..., use.names = TRUE) {
    vapply(x, FUN = fun, FUN.VALUE = logical(1L), ..., USE.NAMES = use.names)
}
# }}}

# viapply {{{
viapply <- function (x, fun, ..., use.names = TRUE) {
    vapply(x, FUN = fun, FUN.VALUE = integer(1L), ..., USE.NAMES = use.names)
}
# }}}

# vcapply {{{
vcapply <- function (x, fun, ..., use.names = TRUE) {
    vapply(x, FUN = fun, FUN.VALUE = character(1L), ..., USE.NAMES = use.names)
}
# }}}

# apply2 {{{
apply2 <- function (x, y, fun, more_args = NULL, simplify = FALSE, use.names = TRUE) {
    mapply(FUN = fun, x, y, MoreArgs = more_args, SIMPLIFY = simplify, USE.NAMES = use.names)
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

# apply2_chr {{{
apply2_chr <- function (x, y, fun, more_args = NULL) {
    as.character(unlist(apply2(x, y, fun, more_args)))
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

# make_filename {{{
make_filename <- function (x, len = 100, unique = TRUE) {
    # reference: https://stackoverflow.com/questions/6730009/validate-a-file-name-on-windows/6804755#6804755
    x <- stri_replace_all_charclass(x, "[<>:\"/\\\\|?*\\x00-\\x1F]", "_")
    # cannot start with "."
    x <- stri_replace_first_regex(x, "^\\.", "_")

    x[nchar(x) > 100] <- substring(x[nchar(x) > 100], 1L, 100L)

    # make unique
    if (!unique) return(x)

    make.unique(x, "_")
}
# }}}

# abort {{{
# reference: https://adv-r.hadley.nz/conditions.html#custom-conditions
abort <- function (message, class = NULL, call = NULL, ...) {
    ori <- getOption("warning.length")
    options(warning.length = 8170L)
    on.exit(options(warning.length = ori), add = TRUE)
    if (is.null(class)) {
        stop(errorCondition(message, ..., class = "eplusr_error", call = call))
    } else {
        stop(errorCondition(message, ..., class = unique(c(paste0("eplusr_error_", class), "eplusr_error")), call = call))
    }
}
# }}}

# warn {{{
# reference: https://adv-r.hadley.nz/conditions.html#custom-conditions
warn <- function (message, class = NULL, call = NULL, ...) {
    ori <- getOption("warning.length")
    options(warning.length = 8170L)
    on.exit(options(warning.length = ori), add = TRUE)
    if (is.null(class)) {
        warning(warningCondition(message, ..., class = "eplusr_warning", call = call))
    } else {
        warning(warningCondition(message, ..., class = unique(c(paste0("eplusr_warning_", class), "eplusr_warning")), call = call))
    }
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
    viapply(x, length)
}
# }}}

# ranger {{{
ranger <- function (minimum = -Inf, lower_incbounds = FALSE, maximum = Inf, upper_incbounds = FALSE) {
    assert_number(minimum, na.ok = TRUE)
    assert_number(maximum, na.ok = TRUE)
    assert_flag(lower_incbounds)
    assert_flag(upper_incbounds)
    setattr(
        list(
            minimum = minimum, lower_incbounds = lower_incbounds,
            maximum = maximum, upper_incbounds = upper_incbounds
        ),
        "class", c("Range", "list")
    )
}
# }}}

# fmt_* {{{
fmt_dbl <- function (x, digits = 2L) sprintf(paste0("%.", digits, "f"), x)
fmt_int <- function (x, digits = 1L) sprintf(paste0("%.", digits, "f"), x)
# }}}

# wday {{{
wday <- function (x, label = FALSE) {
    lubridate::wday(x, label = label, abbr = FALSE, week_start = 1L, locale = "C")
}
# }}}

# str_trunc {{{
#' @importFrom cli console_width
str_trunc <- function (x, width = cli::console_width()) {
    # in case invalid UTF-8 character in IDF
    x <- stringi::stri_encode(x)
    tr <- nchar(crayon::strip_style(x), "width") > (0.95 * width)
    x[tr] <- paste0(stri_sub(x[tr], to = width - 5L), "...")
    x
}
# }}}

# match_in_vec {{{
match_in_vec <- function (x, vec, abbr = NULL, label = FALSE) {
    x <- stri_trans_tolower(x)
    vecl <- stri_trans_tolower(vec)
    if (is.null(abbr)) abbr <- stri_sub(vecl, to = 3L)

    m <- chmatch(x, vecl, nomatch = 0L)
    m[m == 0L] <- chmatch(x[m == 0L], abbr, nomatch = 0L)

    if (!label) {
        m[m == 0L] <- NA_integer_
        m
    } else {
        res <- rep(NA_character_, length(x))
        res[m != 0L] <- vec[m[m != 0L]]
        res
    }
}
# }}}

# copy_list {{{
copy_list <- function(x) {
    if (data.table::is.data.table(x)) {
        x <- copy(x)
    } else if (is.list(x)) {
        x[] <- lapply(x, copy_list)
    }
    x
}
# }}}

file_copy <- function(from, to, copy.date = TRUE, copy.mode = TRUE, err_title = NULL) {
    from <- normalizePath(from, mustWork = TRUE)
    to <- normalizePath(to, mustWork = FALSE)

    # remove duplications
    same <- from == to
    from <- from[!same]

    if (!length(from)) return(to)

    to <- to[!same]

    flag <- file.copy(from, to, copy.date = copy.date, copy.mode = copy.mode, overwrite = TRUE)

    # nocov start
    if (any(!flag)) {
        failed_from <- normalizePath(from[!flag], mustWork = FALSE)
        failed_to <- normalizePath(to[!flag], mustWork = FALSE)
        if (is.null(err_title)) {
            err_title <- "Failed to copy file"
        } else {
            assert_string(err_title)
        }
        abort(sprintf(
            "%s:\n%s",
            err_title,
            paste0(collapse = "\n", sprintf(
                "#%s | From '%s' to '%s'",
                seq_along(failed_from), failed_from, failed_to
            ))
        ))
    }
    # nocov end

    to
}

file_rename <- function(from, to, err_title = NULL) {
    from <- normalizePath(from, mustWork = TRUE)
    to <- normalizePath(to, mustWork = FALSE)

    # remove duplications
    same <- from == to
    from <- from[!same]

    if (!length(from)) return(to)

    to <- to[!same]

    flag <- file.rename(from, to)

    # nocov start
    if (any(!flag)) {
        failed_from <- normalizePath(from[!flag], mustWork = FALSE)
        failed_to <- normalizePath(to[!flag], mustWork = FALSE)
        if (is.null(err_title)) {
            err_title <- "Failed to move file"
        } else {
            assert_string(err_title)
        }
        abort(sprintf(
            "%s:\n%s",
            err_title,
            paste0(collapse = "\n", sprintf(
                "#%s | From '%s' to '%s'",
                seq_along(failed_from), failed_from, failed_to
            ))
        ))
    }
    # nocov end

    to
}

file_rename_if_exist <- function(from, to, err_title = NULL) {
    from <- normalizePath(from, mustWork = FALSE)
    to <- normalizePath(to, mustWork = FALSE)
    res <- rep(NA_character_, length(from))

    exist <- which(file.exists(from))
    if (length(exist)) res[exist] <- file_rename(from[exist], to[exist])

    res
}
