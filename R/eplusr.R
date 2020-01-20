#' eplusr: A Toolkit for Using EnergyPlus in R
#'
#' @details eplusr provides a rich toolkit of using EnergyPlus directly in
#' R, which enables programmatic navigation, modification of EnergyPlus models
#' and makes it less painful to do parametric simulations and analysis.
#'
#' @section Features:
#'
#' - Download and install EnergyPlus in R
#' - Read, parse and modify EnergyPlus:
#'     - Input Data File (IDF)
#'     - Weather File (EPW)
#'     - Report Data Dictionary (RDD) & Meter Data Dictionary (MDD)
#'     - Error File (ERR)
#' - Modify multiple versions of IDFs and run corresponding EnergyPlus
#'   both in the background and in the front
#' - Rich-featured interfaces to query and modify IDFs
#' - Automatically handle referenced fields and validate input during
#'   modification
#' - Take fully advantage of most common used data structure for data
#'   science in R – data.frame
#'     - Extract model, weather data into data.frames
#'     - Modify multiple objects via data.frames input
#'     - Query output via SQL in Tidy format which is much better for
#'       data analysis and visualization
#' - Provide a simple yet extensible prototype of conducting parametric
#'   simulations and collect all results in one go
#' - A pure R-based version updater [transition()] which is much faster than
#'   VersionUpdater distributed with EnergyPlus
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
.options$autocomplete <- interactive()

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
#' * `autocomplete`: Whether to turn on autocompletion on class and field names.
#'   Underneath, [makeActiveBinding()] is used to add or move active bindings in
#'   [Idf] and [IdfObject]s to directly return objects in class or field values.
#'   This will make it possible to dynamically show current class and field
#'   names in both RStudio and in the terminal. However, this process does have
#'   a penalty on the performance. It can make adding or modifying large mounts
#'   of [Idf] and [IdfObject]s extremely slower. Default: `interactive()`.
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

    if (is_empty(opt)) return(as.list.environment(.options, sorted = TRUE))

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

    onoff_opt <- c("view_in_ip", "verbose_info", "autocomplete")

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
# with_option {{{
with_option <- function (opts, expr) {
    # get options
    ori <- eplusr_option()

    if (!is.list(opts) || is.null(names(opts))) {
        stop("`opts` should be a named list.")
    }

    if (any(!names(opts) %in% names(ori))) {
        stop("Invalid eplusr option found: ", sQuote(names(opts)[!names(opts) %in% names(ori)]))
    }

    # set new option values
    on.exit(do.call(eplusr_option, ori), add = TRUE)
    do.call(eplusr_option, opts)

    force(expr)
}
# }}}
# with_silent {{{
with_silent <- function (expr) {
    with_option(list(verbose_info = FALSE), expr)
}
# }}}
# with_speed {{{
with_speed <- function (expr) {
    with_option(list(validate_level = "none", autocomplete = FALSE), expr)
}
# }}}
# without_checking {{{
without_checking <- function (expr) {
    with_option(list(validate_level = "none"), expr)
}
# }}}
