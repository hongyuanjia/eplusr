# `%||%` {{{
`%||%` <- function (x, y) {
    if (is.null(x)) {
        y
    } else {
        x
    }
}
# }}}
# char_count {{{
char_count <- function (x, pattern, ...) {
    nchar(as.character(x)) - nchar(gsub(pattern, "", x, ...))
}
# }}}
# msg {{{
msg <- function (..., prefix = " ", initial = "") {
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
