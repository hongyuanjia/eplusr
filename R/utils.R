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
# slash_exists {{{
slash_exists <- function (idd_data, slash) {
    any(grepl(slash, names(idd_data), fixed = TRUE))
}
# }}}
# sep_line {{{
sep_line <- function (char = "-", length = console_width()) {
    strrep(char, length)
}
# }}}
