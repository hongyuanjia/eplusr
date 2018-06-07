#' @importFrom purrr map map_lgl
# get_date_colname: A helper function to get the column contains date and time in a
# data.frame. It will return a name vector of column that has a calss of
# 'POSIXt' or 'Date'. If none is found, it will return a vector with a length of
# zero.
# get_date_colname {{{
get_date_colname <- function(data){
    assert_that(is.data.frame(data))

    # Get classes for all columns.
    classes <- purrr::map(data, class)
    # Find the column with a class of "POSIXt" or "Date".
    date_classes <- (purrr::map_lgl(classes, function(classes) 'POSIXt' %in% classes)|
                     purrr::map_lgl(classes, function(classes) 'Date' %in% classes))

    # Find the name of date column.
    date_col <- names(which(date_classes))

    return(date_col)
}
# }}}

#' @importFrom dygraphs dygraph
#' @export
# plot_dygraph {{{
plot_dygraph <- function (dt, cols = NULL, main = NULL, xlab = NULL, ylab = NULL) {
    date_col <- get_date_colname(dt)

    if (!is.null(cols)) dt <- dt[, .SD, .SDcols = unique(c(date_col, cols))]

    data.table::as.xts.data.table(dt) %>%
        dygraphs::dygraph(group = "dygraphs", main = main, xlab = xlab, ylab = ylab) %>%
        dygraphs::dyRangeSelector() %>%
        dygraphs::dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
        dygraphs::dyOptions(useDataTimezone = TRUE, axisLineWidth = 1.5) %>%
        dygraphs::dyLegend(width = 400, show = "follow", labelsSeparateLines = TRUE) %>%
        dygraphs::dyCrosshair(direction = "vertical") %>%
        dygraphs::dyCSS(system.file("css", "dygraphs.css", package = "eplusr"))
}
# }}}
