# `%||%` {{{
`%||%` <- function (x, y) {
    if (is.null(x)) {
        y
    } else {
        x
    }
}
# }}}
# msg {{{
msg <- function (...) {
    paste(strwrap(paste0(...)), collapse = "\n")
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
