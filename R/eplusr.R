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
(op.eplusr <- list(
      #' - `tibble.print_max`: Row number threshold: Maximum number of rows
      #'     printed. Set to `Inf` to always print all rows.  Default: 20.
      eplusr.num_digits = 20L,

      #' - `tibble.print_min`: Number of rows printed if row number
      #'     threshold is exceeded. Default: 10.
      eplusr.view_in_ip = FALSE,

      #' - `tibble.width`: Output width. Default: `NULL` (use
      #'     `width` option).
      eplusr.validate_level = NULL,

      #' - `tibble.max_extra_cols`: Number of extra columns
      #'     printed in reduced form. Default: 100.
      eplusr.verbose_info = 100L,

      eplusr.save_format = "sorted"
))
# https://github.com/Rdatatable/data.table/blob/master/R/onLoad.R
# https://github.com/Rdatatable/data.table/blob/master/R/onAttach.R
# https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html
# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
# https://github.com/tidyverse/tibble/blob/782c8904353ad3de878d2b07ac910fbbab04e8a1/DESCRIPTION
# https://github.com/tidyverse/dplyr/blob/master/R/dplyr.r
# https://github.com/tidyverse/dplyr/blob/master/R/zzz.r
# https://github.com/tidyverse/dplyr/blob/master/DESCRIPTION
eplusr_opt <- function(x) op.eplusr[[paste0("eplusr.", x)]]
