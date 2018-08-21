#' @importFrom readr write_lines
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

# backtick_collapse {{{
backtick_collapse <- function (x) {
    s <- paste0("`", x, "`")
    if (length(s) == 1L) {
        return (s)
    } else {
        b <- paste0(s[-length(s)], collapse = ", ")
        e <- s[length(s)]
        out <- paste0(b, " and ", e)
    }
    return(out)
}
# }}}

# backtick {{{
backtick <- function (x) {
    paste0("`", x, "`")
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

# write_lines_eol {{{
# NOTE: IDFEditor will crash if a large IDF file was saved with LF eol on
#       Windows.
write_lines_eol <- function (x, path) {
    if (is_windows())
        readr::write_lines(paste0(x, "\r"), path)
    else
        readr::write_lines(x, path)
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
standardize_ver <- function (ver) {
    if (identical(ver, "latest")) ver <- latest_eplus_ver()
    if (is_integerish(ver)) ver <- paste0(ver, ".0")
    ver <- as.numeric_version(ver)
    if (is.na(ver[1,3])) ver[1,3] <- 0
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
