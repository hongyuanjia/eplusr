#' @importFrom checkmate assert_count assert_choice assert_flag assert_subset
#' @importFrom checkmate assert_string test_choice
NULL

# package level global constant {{{
.globals <- new.env(parent = emptyenv())

# for storing internal data
.globals$eplus <- list()
.globals$idd <- list()
.globals$epw <- list()
.globals$color <- has_color()
# }}}

# package level mutable global options {{{
.options <- new.env(parent = emptyenv())
.options$verbose_info <- TRUE
.options$validate_level <- "final"
.options$view_in_ip <- FALSE
.options$save_format <- "asis"
.options$num_parallel <- parallel::detectCores()
.options$autocomplete <- interactive()
# }}}

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
#'   Possible values: `"asis"`, `"sorted"`, `"new_top"` and `"new_bot"`.
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

    if (!length(opt)) return(as.list.environment(.options, sorted = TRUE))

    nm <- names(opt)

    if (!length(nm)) {
        nm <- unlist(opt)
        assert_string(nm, .var.name = "option")
        return(.options[[nm]])
    }

    assert_subset(nm, names(.options), FALSE, .var.name = "option")

    choice_opt <- c("save_format")
    choice_list <- list(
        save_format = c("asis", "sorted", "new_top", "new_bot")
    )

    onoff_opt <- c("view_in_ip", "verbose_info", "autocomplete")

    count_opt <- c("num_parallel")

    # assign_onoff_opt {{{
    assign_onoff_opt <- function (input, name) {
        if (length(input[[name]])) {
            assert_flag(input[[name]], .var.name = name)
            .options[[name]] <- input[[name]]
        }
    }
    # }}}
    # assign_choice_opt {{{
    assign_choice_opt <- function (input, name) {
        if (length(input[[name]])) {
            assert_choice(input[[name]], choice_list[[name]], .var.name = name)
            .options[[name]] <- input[[name]]
        }
    }
    # }}}
    # assign_count_opt {{{
    assign_count_opt <- function (input, name) {
        if (length(input[[name]])) {
            assert_count(input[[name]], positive = TRUE, .var.name = name)
            .options[[name]] <- as.integer(input[[name]])
        }
    }
    # }}}

    for (nm_opt in choice_opt) assign_choice_opt(opt, nm_opt)
    for (nm_opt in onoff_opt) assign_onoff_opt(opt, nm_opt)
    for (nm_opt in count_opt) assign_count_opt(opt, nm_opt)

    # validate level
    if ("validate_level" %chin% nm) {
        level <- opt[["validate_level"]]
        if (test_choice(level, c("none", "draft", "final"))) {
            .options[["validate_level"]] <- level
        } else {
            .options[["validate_level"]] <- level_checks(level)
        }
    }

    as.list.environment(.options)[nm]
}
# }}}

#' Evaluate an expression with temporary eplusr options
#'
#' These functions evaluate an expression with temporary eplusr options
#'
#' `with_option` evaluates an expression with specified eplusr options.
#'
#' `with_silent` evaluates an expression with no verbose messages.
#'
#' `with_verbose` evaluates an expression with verbose messages.
#'
#' `without_checking` evaluates an expression with no checkings.
#'
#' `with_speed` evaluates an expression with no checkings and autocompletion
#' functionality.
#'
#' @param opts A list of valid input for `eplusr::eplusr_option()`.
#' @param expr An expression to be evaluated.
#' @name with_option
#' @export
#' @examples
#' \dontrun{
#' path_idf <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
#'
#' # temporarily disable verbose messages
#' idf <- with_silent(read_idf(path_idf, use_idd(8.8, download = "auto")))
#'
#' # temporarily disable checkings
#' without_checking(idf$'BuildingSurface:Detailed' <- NULL)
#' # OR
#' with_option(list(validate_level = "none"), idf$'BuildingSurface:Detailed' <- NULL)
#' }
#'
# with_option {{{
with_option <- function (opts, expr) {
    assert_list(opts, names = "named")
    # get options
    ori <- eplusr_option()
    assert_names(names(opts), subset.of = names(ori))

    # set new option values
    on.exit(do.call(eplusr_option, ori), add = TRUE)
    do.call(eplusr_option, opts)

    force(expr)
}
# }}}

#' @name with_option
#' @export
# with_silent {{{
with_silent <- function (expr) {
    with_option(list(verbose_info = FALSE), expr)
}
# }}}

#' @name with_option
#' @export
# with_verbose {{{
with_verbose <- function (expr) {
    with_option(list(verbose_info = TRUE), expr)
}
# }}}

#' @name with_option
#' @export
# with_speed {{{
with_speed <- function (expr) {
    with_option(list(validate_level = "none", autocomplete = FALSE), expr)
}
# }}}

#' @name with_option
#' @export
# without_checking {{{
without_checking <- function (expr) {
    with_option(list(validate_level = "none"), expr)
}
# }}}
