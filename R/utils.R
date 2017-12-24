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
# get_avail_cols {{{
get_avail_cols <- function (x, table) {
    names(x)[match(table, names(x))]
}
# }}}
# sep_line {{{
sep_line <- function (char = "-", length = console_width()) {
    strrep(char, length)
}
# }}}
