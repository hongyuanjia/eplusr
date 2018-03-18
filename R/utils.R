# console_width {{{
# Reference: `cli` (https://github.com/r-lib/cli)
console_width <- function() {
    width <- getOption(
        "cli.width",
        Sys.getenv("RSTUDIO_CONSOLE_WIDTH",
                   getOption("width", 80)
        )
    )

    return(as.integer(width))
}
# }}}
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
        return (x)
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
