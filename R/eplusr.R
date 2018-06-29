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
#' * `eplusr.valid_before_run`: Whether to validate the model at the level
#' "final" before run the simulation. Default: TRUE.
#'
#' @name eplusr-package
"_PACKAGE"

# package level global constant
.globals <- new.env(parent = emptyenv())
.globals$latest_parsed_ver <- as.numeric_version("8.9.0")
.globals$pre_parsed_ver <- paste0(seq(8.5, 8.9, by = 0.1), ".0")
.globals$eplus_config <- list()

# package level mutable global options
.options <- list()
.options$num_digits <- 8L
.options$view_in_ip <- FALSE
.options$validate_level <- "draft"
.options$verbose_info <- TRUE
.options$save_format <- "sorted"
.options$num_parallel <- parallel::detectCores()
.options$valid_before_run <- TRUE

# a helper to check if input option is valid
# get_option {{{
get_option <- function (option, internal = FALSE) {
    if (internal) {
        opt_nm <- names(option)
        opt_val <- unname(option)
    } else {
        opt_nm <- paste0("eplusr.", option)
        opt_val <- getOption(opt_nm)
    }

    key <- ifelse(internal, opt_nm, option)

    is_valid <- switch(key,
        num_digits = is_count(opt_val),
        view_in_ip = is.logical(opt_val),
        validate_level = opt_val %in% c("none", "draft", "final"),
        verbose_info = is.logical(opt_val),
        save_format = opt_val %in% c("sorted", "new_top", "new_bottom"),
        num_parallel = is_count(opt_val),
        valid_before_run = is.logical(opt_val)
    )

    if (!is_valid) {
        msg <- switch(key,
            num_digits = paste0(backtick(opt_nm), " should be a single positive integer."),
            num_parallel = paste0(backtick(opt_nm), " should be a single positive integer."),
            validate_level = paste0(backtick(opt_nm), " should be one of `none`, `draft` or `final`."),
            save_format = paste0(backtick(opt_nm), " should be one of `sorted`, `new_top` or `new_bottom`."),
            paste0(backtick(opt_nm), " should be either TRUE or FALSE.")
        )
        stop(msg, call. = FALSE)
    }

    opt_val
}
# }}}
