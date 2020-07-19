# temp_idf {{{
temp_idf <- function (ver, ...) {
    idf <- empty_idf(ver)

    l <- tryCatch(list(...), error = function (e) 1L)

    if (length(l)) with_option(list(validate_level = "draft"), idf$add(...))

    idf$save(tempfile(fileext = ".idf"))
    idf
}
# }}}
