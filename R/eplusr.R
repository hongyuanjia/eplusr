#' eplusr: A Toolkit for Using EnergyPlus in R
#'
#' @details eplusr provides a richable toolkit of using EnergyPlus directly in
#' R, which enables programmatical navigation, modification of EnergyPlus models
#' and makes it less painful to do parametric simulations and analysis.
#'
#' With eplusr, you can do:
#'
#' * Read and parse EnergyPlus `IDF` files
#' * Query on models, including classes, objects and fields
#' * Directly add, modify, duplicate, and delete objects of `IDF` in R.
#' * Automatically change referred fields when modifying objects.
#' * Save the changed models into standard formats in the same way as IDFEditor
#'   distributed along with EnergyPlus.
#'   * Run your models directly and collect the simulation output of EnergyPlus
#'   in R.
#'
#' To learn more about dplyr, start with the vignettes:
#' `browseVignettes(package = "eplusr")`
#'
#' @section Package options:
#'
#' * `eplusr.num_digits`: Integer indicating the number of decimal places for
#' numeric fields. Default: `8L`
#'
#' * `eplusr.view_in_ip`: Whether models should be presented in IP units.
#' Default: `FALSE`
#'
#' * `eplusr.validate_level`: The strictness level of validation during field
#' value modification and model error checking. Possible value: `"none"`,
#' `"draft"` and `"final"`. Default: `"final"`. Detailed description:
#'     - For `"none"`, none validation will be done;
#'     - For `"draft"`, checking of invalid autosize, autocalculate, numeric,
#'       integer, and choice field values will be done;
#'     - For `"final"`, besides above, checking of missing required objects,
#'     duplicated unique objects, object name confliction, missing required
#'     fields and invalid field value reference will also be done.
#'
#' * `eplusr.verbose_info`: Whether to show information messages. Default: `TRUE`
#'
#' * `eplusr.save_format`: The format to use when saving Idf objects to `.idf`
#' files. Possible values: `"sorted"`, `"new_top"` and `"new_bottom"`, which
#' have the same effect as `Save Options` settings `"Sorted"`, `"Original with
#' New at Top"` and `"Original with New at Bottom"` in IDF Editor, respectively.
#' Default: `"sorted"`.
#'
#' * `eplusr.num_parallel`: Maximum number of parallel simulations to run.
#' Default: `parallel::detectCores()`.
#'
#' @name eplusr-package
"_PACKAGE"

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
.options$verbose_info <- FALSE
.options$save_format <- "sorted"
.options$num_parallel <- parallel::detectCores()
