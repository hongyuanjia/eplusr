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
#' * Check any possible errors whenever modifications are made.
#' * Save the changed models into standard formats in the same way as IDFEditor
#'   distributed along with EnergyPlus.
#' * Run your models directly and collect the simulation output of EnergyPlus
#'   in R.
#' * Run parametric analysis in parallel and collect results in one go.
#'
#' @name eplusr-package
#' @author Hongyuan Jia
"_PACKAGE"

# check_color {{{
check_color <- function () {
    (.globals$color <- has_color())
}
# }}}

# package level global constant {{{
.globals <- new.env(parent = emptyenv())

# for storing internal data
.globals$eplus_config <- list()
.globals$idd <- list()
# }}}

# package level mutable global options
.options <- new.env(parent = emptyenv())
.options$verbose_info <- TRUE
.options$validate_level <- "final"
.options$view_in_ip <- FALSE
.options$save_format <- "asis"
.options$num_parallel <- parallel::detectCores()

#' Get and Set eplusr options
#'
#' Get and set eplusr options which affect the way in which eplusr computes and
#' displays its results.
#'
#' @param ... Any available options to define, using `name = value`. All
#' available options are shown below. If no options are given, all values of
#' current options are returned. If a single option name, its value is returned.
#'
#' @details
#' * `validate_level`: The strictness level of validation during field value
#'   modification and model error checking. Possible value: `"none"`,
#'   `"draft"` and `"final"` or a custom validation level using
#'   [custom_validate()]. Default: `"final"`. For what validation
#'   components each level contains, see [level_checks()].
#'
#' * `view_in_ip`: Whether models should be presented in IP units. Default:
#'   `FALSE`. It is not recommended to set this option to `TRUE` as currently
#'   IP-units support in eplusr is not fully tested.
#'
#' * `save_format`: The default format to use when saving Idf objects to `.idf` files.
#'   Possible values: `"asis"`, `"sorted"`, `"new_top"` and `"new_bottom"`.
#'   The later three have the same effect as `Save Options` settings
#'   `"Sorted"`, `"Original with New at Top"` and `"Original with New at
#'   Bottom"` in IDF Editor, respectively. For `"asis"`, the saving format
#'   will be set according to the header of IDF file. If no header found,
#'   `"sorted"` is used. Default: `"asis"`.
#'
#' * `num_parallel`: Maximum number of parallel simulations to run. Default:
#'     `parallel::detectCores()`.
#'
#' * `verbose_info`: Whether to show information messages. Default: `TRUE`.
#'
#' @return If called directly, a named list of input option values. If input is
#'     a single option name, a length-one vector whose type is determined by
#'     that option. If input is new option values, a named list of newly set
#'     option values.
#' @examples
#' # list all current options
#' eplusr_option() # a named list
#'
#' # get a specific option value
#' eplusr_option("verbose_info")
#'
#' # set options
#' eplusr_option(verbose_info = TRUE, view_in_ip = FALSE)
#' @export
#' @author Hongyuan Jia
# eplusr_option {{{
eplusr_option <- function (...) {
    opt <- list(...)

    if (is_empty(opt)) return(as.list.environment(.options))

    nm <- names(opt)

    if (is_empty(nm)) {
        nm <- unlist(opt)
        assert(is_string(nm), prefix = "option")
        return(.options[[nm]])
    }

    if ("num_digits" %in% nm) {
        warn("warning_eplusr_deprecated_opt",
            paste0("Option `num_digits` has been deprecated. ",
                "The formatting of numeric fields are not handled by R itself."
            )
        )
        nm <- nm[nm != "num_digits"]
        opt <- opt[nm != "num_digits"]
        if (!length(nm)) return(as.list.environment(.options))
    }

    assert(nm %in% names(.options),
        msg = paste0("Invalid option name found: ", collapse(nm[!nm %in% names(.options)]), ".")
    )

    choice_opt <- c("save_format")
    choice_list <- list(
        save_format = c("asis", "sorted", "new_top", "new_bot")
    )

    onoff_opt <- c("view_in_ip", "verbose_info")

    count_opt <- c("num_parallel")

    # assign_onoff_opt {{{
    assign_onoff_opt <- function (input, name) {
        if (length(input[[name]])) {
            assert(is_scalar(input[[name]]), prefix = name)
            assert(is_flag(input[[name]]), prefix = name)
            .options[[name]] <- input[[name]]
        }
    }
    # }}}
    # assign_choice_opt {{{
    assign_choice_opt <- function (input, name) {
        if (not_empty(input[[name]])) {
            assert(is_string(input[[name]]), prefix = name)
            assert(input[[name]] %in% choice_list[[name]],
                msg = paste0(surround(name), " should be one of ",
                    collapse(choice_list[[name]])
                )
            )

            .options[[name]] <- input[[name]]
        }
    }
    # }}}
    # assign_count_opt {{{
    assign_count_opt <- function (input, name) {
        if (not_empty(input[[name]])) {
            assert(is_count(input[[name]]), prefix = name)
            .options[[name]] <- as.integer(input[[name]])
        }
    }
    # }}}

    for (nm_opt in choice_opt) assign_choice_opt(opt, nm_opt)
    for (nm_opt in onoff_opt) assign_onoff_opt(opt, nm_opt)
    for (nm_opt in count_opt) assign_count_opt(opt, nm_opt)

    # validate level
    if ("validate_level" %in% nm) {
        level <- opt[["validate_level"]]
        if (is_string(level) && level %in% c("none", "draft", "final")) {
            .options[["validate_level"]] <- level
        } else {
            .options[["validate_level"]] <- level_checks(level)
        }
    }

    as.list.environment(.options)[nm]
}
# }}}
