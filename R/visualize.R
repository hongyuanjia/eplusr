#' @importFrom dygraphs dygraph
#' @export
# plot_dygraph {{{
plot_dygraph <- function (dt, cols = NULL, main = NULL, xlab = NULL, ylab = NULL) {
    if (!is.null(cols)) dt <- dt[, .SD, .SDcols = unique(c("datetime", cols))]
    dt %>% data.table::as.xts.data.table() %>%

    dygraphs::dygraph(group = "dygraphs", main = main, xlab = xlab, ylab = ylab) %>%
    dygraphs::dyRangeSelector() %>%
    dygraphs::dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dygraphs::dyOptions(useDataTimezone = TRUE, axisLineWidth = 1.5) %>%
    dygraphs::dyLegend(width = 400, show = "follow", labelsSeparateLines = TRUE) %>%
    dygraphs::dyCrosshair(direction = "vertical") %>%
    dygraphs::dyCSS(system.file("css", "dygraphs.css", package = "eplusr"))
}
# }}}
