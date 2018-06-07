#' @aliases NULL tibble-package
#'
#' @details EnergyPlus has been widely used for XXXX. `eplusr`
#'
#' @section Methods:
#'
#' `tbl_df` implements four important base methods:
#'
#' \describe{
#' \item{print}{By default only prints the first 10 rows (at most 20), and the
#'   columns that fit on screen; see [print.tbl()]}
#' \item{\code{[}}{Does not simplify (drop) by default, returns a data frame}
#' \item{\code{[[}, `$`}{Calls [.subset2()] directly,
#'   so is considerably faster. Returns `NULL` if column does not exist,
#'   `$` warns.}
#' }
#'
#' @section Important functions:
#'
#' [tibble()] and [tribble()] for construction,
#' [as_tibble()] for coercion,
#' and [print.tbl()] and [glimpse()] for display.
#' @importFrom data.table ":="
#' @examples
#' tibble(a = 1:26, b = letters)
#' as_tibble(iris)
"_PACKAGE"

#' Package options
#'
#' Display options for `tbl_df`, used by [trunc_mat()] and
#' (indirectly) by [print.tbl()].
#'
#' @name eplusr-package
#' @section Package options:
# https://github.com/Rdatatable/data.table/blob/master/R/onLoad.R
# https://github.com/Rdatatable/data.table/blob/master/R/onAttach.R
# https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html
# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
# https://github.com/tidyverse/tibble/blob/782c8904353ad3de878d2b07ac910fbbab04e8a1/DESCRIPTION
# https://github.com/tidyverse/dplyr/blob/master/R/dplyr.r
# https://github.com/tidyverse/dplyr/blob/master/R/zzz.r
# https://github.com/tidyverse/dplyr/blob/master/DESCRIPTION
eplusr_opt <- function(x) op.eplusr[[paste0("eplusr.", x)]]

# package level global constant
.globals <- new.env(parent = emptyenv())
.globals$latest_parsed_ver <- as.numeric_version("8.9.0")
.globals$pre_parsed_ver <- paste0(seq(8.5, 8.9, by = 0.1), ".0")
.globals$eplus_config <- list()

# package level mutable global options
.options <- new.env(parent = emptyenv())
.options$num_digits <- 8L
.options$view_in_ip <- FALSE
.options$validate_level <- "final"
.options$verbose_info <- TRUE
.options$save_format <- "sorted"
.options$num_parallel <- parallel::detectCores(logical = FALSE)
