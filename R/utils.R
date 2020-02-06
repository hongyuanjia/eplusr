#' @importFrom stringi stri_enc_toutf8 stri_replace_all_charclass stri_trans_tolower
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
collapse <- function (x, out = "'", or = FALSE) {
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
    if (or) {
        paste0(b, " or ", e)
    } else {
        paste0(b, " and ", e)
    }
}
# }}}

# surround {{{
surround <- function (x, out = "'") {
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
    paste0(x, stringi::stri_dup(char, pmax(width - w, 0)))
}

lpad <- function(x, char = " ", width = NULL) {
    if (!length(x)) return(x)
    w <- nchar(x, type = "width")
    if (is.null(width)) width <- max(w)
    paste0(stringi::stri_dup(char, pmax(width - w, 0)), x)
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

# read_lines {{{
read_lines <- function(input, trim = TRUE, ...) {
    dt <- tryCatch(
        fread(input = input, sep = NULL, header = FALSE, col.names = "string", ...),
        error = function (e) {
            abort("error_read_file",
                paste0("Failed to read input file. ", conditionMessage(e))
            )
        }
    )
    if (!nrow(dt)) return(data.table(string = character(0L), line = integer(0L)))
    set(dt, j = "line", value = seq_along(dt[["string"]]))

    if (trim) {
        tryCatch(set(dt, j = "string", value = stri_trim_both(dt[["string"]])),
            error = function (e) {
                if (!grepl("invalid UTF-8 byte sequence detected", conditionMessage(e), fixed = TRUE)) {
                    signalCondition(e)
                }

                # fix encoding issue in older versions of IDD files
                dt[!stringi::stri_enc_isutf8(string), string :=
                    stringi::stri_encode(string, "windows-1252", "UTF-8")
                ]

                set(dt, j = "string", value = stri_trim_both(dt[["string"]]))
            }
        )
    }

    setcolorder(dt, c("line", "string"))

    dt
}
# }}}

# write_lines {{{
# NOTE: IDFEditor will crash if a large IDF file was saved with LF eol on
#       Windows.
write_lines <- function (x, file = "", append = FALSE) {
    if (inherits(x, "data.table")) {
        assert(has_name(x, "string"))
        fwrite(x[, list(string)], file = file, col.names = FALSE, quote = FALSE, append = append)
    } else {
        assert(is.character(x))
        fwrite(data.table(x), file = file, col.names = FALSE, quote = FALSE, append = append)
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
standardize_ver <- function (ver, strict = FALSE, complete = TRUE) {
    if (!strict && is.character(ver)) {
        ver[ver == "latest"] <- LATEST_EPLUS_VER
    }

    if (is.numeric(ver)) {
        int <- (!is.na(ver)) & (is.integer(ver) | (is.numeric(ver) & (ver == trunc(ver))))
        if (any(int)) ver[int] <- paste0(ver[int], ".0")
    }

    ver <- numeric_version(ver, strict = FALSE)

    # only keep major.minor.patch, and remove others
    has_trail <- suppressWarnings(!is.na(ver[, 4L]))
    ver[has_trail] <- ver[has_trail, 1L:3L]

    # complete patch version to 0 if not exist
    if (complete && any(!is.na(ver) & is.na(ver[, 3L]))) {
        ver[!is.na(ver) & is.na(ver[, 3L]), 3L] <- 0L
    }

    ver
}
# }}}

# complete_patch_ver {{{
complete_patch_ver <- function (ver) {
    if (any(!is.na(ver) & is.na(ver[, 3L]))) {
        ver[!is.na(ver) & is.na(ver[, 3L]), 3L] <- 0L
    }
    ver
}
# }}}

# match_minor_ver {{{
match_minor_ver <- function (ver, all_ver, type = c("idd", "eplus"), verbose = TRUE) {
    assert(is_scalar(ver))
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
        if (verbose) {
            type <- match.arg(type)
            key <- switch(type, idd = "IDD", eplus = "EnergyPlus")

            verbose_info("Multiple versions found for ", key, " v", ori_ver, ": ",
                collapse(paste0("v", ver)), ". ",
                "The last patched version v", max(ver), " will be used. ",
                "Please explicitly give the full version if you want to use the other versions."
            )
        }
        ver <- max(ver)
    }

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
    tryCatch(
        stri_replace_all_charclass(name, "[^[:alnum:]]", "_", merge = merge),
        error = function (e) {
            if (!grepl("invalid UTF-8 byte sequence detected", conditionMessage(e), fixed = TRUE)) {
                signalCondition(e)
            }

            # fix encoding issue in older versions of IDD files
            name[!stringi::stri_enc_isutf8(name)] <-
                stringi::stri_encode(name[!stringi::stri_enc_isutf8(name)], "windows-1252", "UTF-8")

            stri_replace_all_charclass(name, "[^[:alnum:]]", "_", merge = merge)
        }
    )
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
    ori <- getOption("warning.length")
    options(warning.length = 8170L)
    on.exit(options(warning.length = ori), add = TRUE)
    err <- cnd(type = "error", subclass =  subclass, message = message, call = call, ...)
    stop(err)
}
# }}}

# warn {{{
# reference: https://adv-r.hadley.nz/conditions.html#custom-conditions
warn <- function (subclass, message, call = NULL, ...) {
    ori <- getOption("warning.length")
    options(warning.length = 8170L)
    on.exit(options(warning.length = ori), add = TRUE)
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

# ranger {{{
ranger <- function (minimum = -Inf, lower_incbounds = FALSE, maximum = Inf, upper_incbounds = FALSE) {
    assert(is_scalar(minimum) && is.numeric(minimum),
           is_scalar(maximum) && is.numeric(maximum),
           is_flag(lower_incbounds), is_flag(upper_incbounds)
    )
    setattr(
        list(
            minimum = minimum, lower_incbounds = lower_incbounds,
            maximum = maximum, upper_incbounds = upper_incbounds
        ),
        "class", c("Range", "list")
    )
}
# as_Range.character <- function (x) {
# "([\\(\\[])\\s*(\\d+)\\s*,\\s*(\\d+|Inf)\\s*([\\)\\]])"
# }
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

# as_integer {{{
as_integer <- function (x) {
    x <- as.double(x)
    if (any(x[!is.na(x)] != trunc(x[!is.na(x)]))) {
        x[!is.na(x) & x != trunc(x)] <- NA_integer_
        warning("NAs introduced by coercion")
    }
    x
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

# .deprecated_fun {{{
# adopted from tidyverse/lubridate/R/deprecated.R
.deprecated_fun <- function(name, replacement, class = NULL, version) {
    class <- if (is.null(class)) "" else paste0(" in ", class, " class")
    msg <- paste0(sprintf("`%s` is deprecated%s in eplusr version `%s`. Please use `%s` instead.",
        name, class, version, replacement)
    )
    .deprecated(msg, version)
}
# }}}

# .deprecated_arg {{{
.deprecated_arg <- function(arg, version, class = NULL, n_call = 1) {
    name <- paste0(as.character(sys.call(-n_call)[[1]]), "()")
    if (!is.null(class)) {
        name <- sub(".*?_", "$", name)
        cls <- paste0(" in class '", class, "'")
    } else {
        cls <- ""
    }
    mes <- sprintf("Parameter `%s` of `%s`%s has been deprecated in eplusr version %s.",
        arg, name, cls, version)
    .deprecated(mes, version)
}
# }}}

# .deprecated {{{
.deprecated <- function(msg, version) {
    v <- as.package_version(version)
    cv <- utils::packageVersion("eplusr")

    # If current major number is greater than last-good major number, or if
    # current minor number is more than 2 greater than last-good minor number,
    # give error.
    if (cv[[1, 1]] > v[[1, 1]]  ||  cv[[1, 2]] > v[[1, 2]] + 2) {
        abort("error_eplusr_deprecated", msg)
    } else {
        warn("warning_eplusr_deprecated", msg)
    }
    invisible()
}
# }}}

# str_trunc {{{
str_trunc <- function (x, width = getOption("width", 60L)) {
    # in case invalid UTF-8 character in IDF
    x <- stringi::stri_encode(x)
    tr <- nchar(x, "width") > (0.95 * width)
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
