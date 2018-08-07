#' eplusr: A Toolkit for Using EnergyPlus in R
#'
#' @details eplusr provides a rich toolkit of using EnergyPlus directly in
#' R, which enables programmatic navigation, modification of EnergyPlus models
#' and makes it less painful to do parametric simulations and analysis.
#'
#' With eplusr, you can do:
#'
#' * Read, parse and modify EnergyPlus Weather File (EPW).
#' * Read and parse EnergyPlus IDF files.
#' * Query on models, including classes, objects and fields
#' * Directly add, modify, duplicate, and delete objects of IDF in R.
#' * Automatically change referred fields when modifying objects.
#' * Save the changed models into standard formats in the same way as IDFEditor
#'   distributed along with EnergyPlus.
#' * Run your models directly and collect the simulation output of EnergyPlus
#'   in R.
#' * Run parametric analysis in parallel and collect results in one go.
#'
#' @name eplusr-package
"_PACKAGE"

# package level global constant
.globals <- new.env(parent = emptyenv())
.globals$eplus_config <- list()
.globals$idd <- list()

# package level mutable global options
.options <- new.env(parent = emptyenv())
.options$num_digits <- 8L
.options$view_in_ip <- FALSE
.options$validate_level <- "final"
.options$verbose_info <- TRUE
.options$save_format <- "asis"
.options$num_parallel <- parallel::detectCores()

#' Get and Set eplusr options
#'
#' Get and set eplusr options which affect the way in which eplusr computes and
#' displays its results.
#'
#' @param ... Any options can be defined, using `name = value`. All available
#'     options are below. If no options are given, then all values of current
#'     options are returned.
#'
#' @details
#' * `num_digits`: Integer indicating the number of decimal places for numeric
#'     fields. Default: `8L`
#'
#' * `view_in_ip`: Whether models should be presented in IP units.  Default:
#'     `FALSE`
#'
#' * `validate_level`: The strictness level of validation during field value
#'     modification and model error checking. Possible value: `"none"`,
#'     `"draft"` and `"final"`. Default: `"final"`. Detailed description:
#'   - For `"none"`, none validation will be done;
#'   - For `"draft"`, checking of invalid autosize, autocalculate, numeric,
#'     integer, and choice field values will be done;
#'   - For `"final"`, besides above, checking of missing required objects,
#'     duplicated unique objects, object name conflicts, missing required fields
#'     and invalid field value reference will also be done.
#'
#' * `verbose_info`: Whether to show information messages. Default: `TRUE`.
#'
#' * `save_format`: The format to use when saving Idf objects to `.idf` files.
#'     Possible values: `"asis"`, `"sorted"`, `"new_top"` and `"new_bottom"`.
#'     The later three have the same effect as `Save Options` settings
#'     `"Sorted"`, `"Original with New at Top"` and `"Original with New at
#'     Bottom"` in IDF Editor, respectively. For `"asis"`, the saving format
#'     will be set according to the header of IDF file. If no header found,
#'     `"sorted"` is used. "Default: `"asis"`.
#'
#' * `num_parallel`: Maximum number of parallel simulations to run. Default:
#'     `parallel::detectCores()`.
#'
#' @export
# eplusr_option {{{
eplusr_option <- function (...) {
    opt <- purrr::splice(...)
    if (is_empty(opt)) return(as.list.environment(.options))

    nm <- names(opt)

    if (is_empty(nm)) {
        nm <- unlist(opt)
        assert_that(is_string(nm))
        return(.options[[nm]])
    }

    valid <- nm %in% c("num_digits", "view_in_ip", "validate_level",
        "verbose_info", "save_format", "num_parallel")

    if (any(!valid))
        stop("Invalid option name found: ", backtick_collapse(nm[!valid]), ".",
            call. = FALSE)

    choice_opt <- c("validate_level", "save_format")
    choice_list <- list(
        validate_level = c("none", "draft", "final"),
        save_format = c("asis", "sorted", "new_top", "new_bot")
    )

    onoff_opt <- c("view_in_ip", "verbose_info")

    count_opt <- c("num_digits", "num_parallel")

    # assign_onoff_opt {{{
    assign_onoff_opt <- function (input, name) {
        if (not_empty(input[[name]])) {
            assert_that(is_scalar(input[[name]]))

            invalid <- !is.logical(input[[name]])

            if (invalid)
                stop(backtick(name), " should be one of either `TRUE` or `FALSE`.",
                    call. = FALSE)

            .options[[name]] <- input[[name]]
        }
    }
    # }}}
    # assign_choice_opt {{{
    assign_choice_opt <- function (input, name) {
        if (not_empty(input[[name]])) {
            assert_that(is_string(input[[name]]))

            invalid <- !input[[name]] %in% choice_list[[name]]

            if (invalid)
                stop(backtick(name), " should be one of ",
                    backtick_collapse(choice_list[[name]]), ".", call. = FALSE)

            .options[[name]] <- input[[name]]
        }
    }
    # }}}
    # assign_count_opt {{{
    assign_count_opt <- function (input, name) {
        if (not_empty(input[[name]])) {
            assert_that(is_count(input[[name]]))
            .options[[name]] <- input[[name]]
        }
    }
    # }}}

    for (nm_opt in choice_opt) assign_choice_opt(opt, nm_opt)
    for (nm_opt in onoff_opt) assign_onoff_opt(opt, nm_opt)
    for (nm_opt in count_opt) assign_count_opt(opt, nm_opt)

    as.list.environment(.options)[nm]
}
# }}}
