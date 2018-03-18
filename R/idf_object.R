#' EnergyPlus IDF objects
#'
#' \code{IDFObject} is a R6 class used internally as members in \code{IDF} R6
#' class. \code{IDFObject} inherits \code{\link{IDDObject}}, so all methods of
#' \code{IDDObject} are available for \code{IDFobject}.
#'
#' @section Usage:
#' ```
#'
#' idfobj <- IDFObject$new(list, idd)
#'
#' idfobj$get_comment()
#' idfobj$set_comment(comment, append = TRUE, wrap = 0L)
#' idfobj$get_value(index = NULL, name = NULL)
#' idfobj$set_value(...)
#'
#' idfobj$check()
#'
#' idfobj$out(comment = TRUE, leading = 4L, ip_units = FALSE, sep_at = 29L,
#'            add_blank = FALSE)
#' idfobj$out_lines(index = NULL, ip_units = FALSE)
#'
#' idfobj$print(comment = TRUE)
#'
#' print(iddobj)
#' ```
#'
#' For other methods inherited from `IDDObject`, please see
#' \code{\link{IDDObject}}.
#'
#' @section Detail:
#'
#' \subsection{Create}{
#' ```
#' idfobj <- IDFObject$new(list, idd)
#' ```
#'
#' `IDFObject$new(list, idd)` creates an IDFObject using parsed IDF object data
#' stored in a list and an IDD object.
#'
#' **Arguments**:
#'
#' * `list`: A list. The list contains three named elements:
#' `object_id`, `value` and `comment`, where:
#'     * `object_id`: a unique integer to reference this IDFObject.
#'     * `value`: a list contains all values of this IDFObject. All values can
#'     be totally strings, but they will be converted to corresponding types
#'     during initialization.
#'     * `comment`: a character vector. Elements will be put line by line when
#'     saved to `*.idf` files.
#' * `idd`: An IDD object created by `IDD$new()`.
#' }
#'
#' \subsection{Value}{
#' ```
#' idfobj$get_value(index = NULL, name = NULL)
#' ```
#'
#' `$get_value(index, name)` return values of certain fields specified by
#' `index` or `name`. The values are returned as a list and returned invisibly.
#'
#' `$set_value(...)` sets values given in `...` to fields.
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom rlang dots_splice
#' @importFrom cli cat_line cat_rule cat_bullet
#' @importFrom stringr str_pad
#' @importFrom clisymbols symbol
#' @importFrom purrr map map_chr map_lgl map2_lgl iwalk
#' @importFrom assertthat assert_that
#' @return An IDFObject object
#' @docType class
#' @seealso \code{\link{IDDObject}}
#' @name IDFObject
#' @author Hongyuan Jia
NULL

#' @export
# IDFObject {{{
IDFObject <- R6::R6Class(classname = "IDFObject",
    inherit = IDDObject,

    public = list(

        initialize = function (list, idd) {
            # {{{
            private$m_id <- list$object_id
            private$m_value <- as.list(list$value)
            private$m_comment <- list$comment

            .iddobj <- idd$object(list$class)
            len <- length(private$m_value)
            assert_that(.iddobj$is_valid_field_num(len),
                        msg = paste0("Object [ID:", private$m_id, "] in class ",
                                     .iddobj$class_name(), " have ", len,
                                     " fields, which is not a valid field number."))
            # handle extensible fields
            num <- .iddobj$num_fields()
            if (len > num) {
                # no need to check if the class is extensible as it has been
                # done by .iddobj$is_valid_field_num
                num_to_add <- (len - num) / .iddobj$num_extensible()
                .iddobj <- .iddobj$clone(deep = TRUE)
                .iddobj$add_extensible_groups(num_to_add)
            }
            private$m_fields <- .get(.iddobj, "m_fields")
            private$m_properties <- .get(.iddobj, "m_properties")
            private$enforce_value_type()
            # }}}
        },

        # PUBLIC FUNCTIONS
        # {{{
        get_comment = function () {
            # return object comments
            # {{{
            cli::cat_line("! ", private$m_comment)
            return(invisible(private$m_comment))
            # }}}
        },

        set_comment = function (comment, append = TRUE, wrap = 0L) {
            # set object comment
            # {{{
            if (is_empty(comment)) {
                private$m_comment <- NULL
            } else {
                assert_that(is.character(comment))
                if (wrap > 0L) comment <- strwrap(comment, width = wrap)

                if (append) {
                    private$m_comment <- c(private$m_comment, comment)
                } else {
                    private$m_comment <- c(comment, private$m_comment)
                }
            }

            cli::cat_line("! ", private$m_comment)
            # }}}
        },

        get_value = function (index = NULL, name = NULL) {
            # return object values
            # {{{
            if (all(is.null(index), is.null(name))) {
                index <- index %||% seq_len(length(private$m_value))
            }

            index <- private$fields(index, name)[, field_order]
            cli::cat_line(self$out_lines(index))
            return(invisible(private$m_value[index]))
            # }}}
        },

        set_value = function (...) {
            # set field value
            # {{{
            assert_that(!self$is_version(), msg = "Cannot modify `Version` object directly.")
            # capture all arguments in dots and flatten into a list
            dots <- rlang::dots_splice(...)
            assert_that(not_empty(dots), msg = "Please give values to set.")
            # check if the dots have names
            nms <- names(dots)
            named <- all(nms != "")
            any_named <- any(nms != "")
            if (!named & any_named) {
                stop("Values should be either all unnamed or all named.", call. = FALSE)
            }

            # get current field number
            num <- self$num_fields()
            # if all named, using named index
            # {{{
            if (named) {
                # check duplication of names
                if (anyDuplicated(nms)) {
                    stop("Duplicated field names found: ",
                         backtick_collapse(nms[duplicated(nms)]),
                         call. = FALSE)
                }

                # check if all inputs have valid names
                valid <- self$is_valid_field_name(nms)
                if (!all(valid)) {
                    # check if there are newly added extensible fields
                    if (!self$is_extensible()) {
                        stop("Invalid field names found: ",
                             backtick_collapse(nms[!valid]),
                             call. = FALSE)
                    } else {
                        # get new field names
                        new_fields <- nms[!valid]
                        new_num <- length(new_fields)
                        new_nms <- names(new_fields)
                        # check if the number of new fields is acceptable
                        if (!self$is_valid_field_num(num + new_num)) {
                            stop("Invalid field number found: ", backtick(len), ". ",
                                 "Should be ", backtick(paste0(self$num_fields(), " + ",
                                                             self$num_extensible(), " * X.")))
                        }
                        # add new extensible groups
                        num_to_add <- new_num / self$num_extensible()
                        private$add_extensible_groups(num_to_add)
                        # check if new field names are valid
                        new_valid <- new_nms %in%
                            c(self$field_name(seq(num + 1, length.out = new_num)))
                        if (!all(new_valid)) {
                            stop("Invalid field names found: ",
                                 backtick_collapse(new_nms[!new_valid]),
                                 call. = FALSE)
                        }
                    }
                }

                index <- self$field_index(nms)
            # }}}

            # if none named, using position index
            # {{{
            } else {
                # get the number of values
                len <- length(dots)
                # if need to add new extensible groups
                if (len > num) {
                    if (!self$is_extensible()) {
                        stop("Invalid field number found: ", backtick(len), ". ",
                             "Should be ", backtick(paste0(self$num_fields(), " + ",
                                                         self$num_extensible(), " * X.")))
                    }

                    if (private$get_extensible_field_index(len) != 0) {
                        stop("Invalid field number found: ", backtick(len), ". ",
                             "Should be ", backtick(paste0(self$num_fields(), " + ",
                                                         self$num_extensible(), " * X.")))
                    }

                    num_to_add <- (len - num) / self$num_extensible()
                    private$add_extensible_groups(num_to_add)
                }

                index <- seq_len(len)
            }
            # }}}

            # TODO: check if below is ok for single field objects such
            # `Version`
            check_data <- private$fields(index)[, value := dots]
            errors <- private$check_object(.data = check_data, print = FALSE)

            # get current valid value number
            num_value <- length(private$m_value)
            # get indice of newly added fields
            new_index <- index[index > num_value]
            # get all valid field indice
            full_index <- seq_len(num)
            # get index that in the input but not in the current private$m_value
            mis_index <- full_index[full_index > num_value & full_index < max(index)]

            # if no error
            if (all(purrr::map_lgl(errors, is_empty))) {
                # if there are newly added non-required fields
                if (not_empty(mis_index)) {
                    # for named case
                    if (named) {
                        default <- self$field_default(mis_index)
                        # get the type of missing new fields
                        cli::cat_rule("Info", line = 1)
                        cli::cat_bullet(
                            paste0("| Values for field ",
                                backtick_collapse(mis_index),
                                " are missing. Default values are added."),
                            bullet = "info")
                        cli::cat_line()

                        # assign default values to new fields
                        private$m_value[mis_index] <- default
                    }
                }
                private$m_value[index] <- dots
                self$print()
            } else {
                # store original field values
                ori_value <- private$m_value
                on.exit({private$m_value <- ori_value}, add = TRUE)
                private$m_value[index] <- dots
                private$print_check(errors)
                stop("Failed to set field values.", call. = FALSE)
            }
            # }}}
        },

        check = function () {
            # validate field in terms of all creteria
            # {{{
            private$check_object(print = TRUE)
            # }}}
        },

        out = function (comment = TRUE, leading = 4L, ip_units = FALSE,
                        sep_at = 29L, add_blank = FALSE) {
            # return IDF format output
            # {{{
            .class <- paste0(self$class_name(), ",")
            .value <- private$out_values(leading = leading, length = sep_at, blank = add_blank)
            .name <- private$out_names(ip_units = ip_units)[seq_along(.value)]
            out <- c(.class, paste0(.value, .name))
            if (comment) {
                out <- c(private$out_comments(), out)
            }
            return(out)
            # }}}
        },

        out_lines = function (index = NULL, ip_units = FALSE) {
            # return IDF format output
            # {{{
            .value <- private$out_values(index = index, leading = 1L, length = 20L, blank = TRUE)
            .name <- private$out_names(index = index, ip_units = ip_units)
            index <- index %||% seq_along(private$m_value)
            .index <- private$out_index(index)
            out <- paste0(.index, ":", .value, .name)
            return(out)
            # }}}
        },

        print = function (comment = TRUE) {
            # TODO: change unit according to IDF$options$view_in_ip
            # {{{
            id_class <- paste0("[ID: ", private$m_id, "] Class ", backtick(self$class_name()))
            cli::cat_line(clisymbols::symbol$star, clisymbols::symbol$star, id_class, clisymbols::symbol$star, clisymbols::symbol$star)

            # comment
            if (comment) {
                if (not_empty(private$m_comment)) {
                    cli::cat_rule(center = "* NOTES *", line = 1)
                    cli::cat_line(private$m_comment)
                }
            }

            # value
            # remove class line
            out <- self$out(comment = FALSE, leading = 1L, sep_at = 20L,
                            add_blank = TRUE)[-1]
            index <- stringr::str_pad(seq_along(private$m_value),
                                      nchar(length(private$m_value)), "left")
            cli::cat_rule(center = "* FIELDS *")
            cli::cat_line(index, ":", out)
            cli::cat_rule()
            # }}}
        }
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS
        # {{{
        m_id = NA_integer_,
        m_value = list(),
        m_comment = character(),
        m_iddobj = NULL,
        m_properties = NULL,
        m_fields = NULL,
        m_check = list(),
        m_check_data = data.table(),
        # }}}

        # PRIVATE FUNCTIONS
        # {{{
        enforce_value_type = function () {
        # make sure that numeric strings are converted to numeric values and
        # all empty values are converted to corresponding NAs
            # {{{
            .value_dt <- private$m_fields[field_order <= length(private$m_value)]
            # NOTE: In `data.table`, if 'private$m_value' is a list of length
            # 1, then 'value' here will be a character string, not a list. Have
            # to fix this first.
            if (is_scalar(private$m_value)) {
                .value_dt[, value := list(private$m_value)]
            } else {
                .value_dt[, value := list(private$m_value)]
            }

            is_empty <- purrr::map_lgl(private$m_value, ~.x == "")
            is_auto <- purrr::map_lgl(private$m_value, ~tolower(.x) %in% c("autosize", "autocalculate"))
            .value_dt[field_an == "A" & is_empty,
                      value := list(list(NA_character_)), by = c("field_order")]
            .value_dt[field_an == "N" & is_empty,
                      value := list(list(NA_real_)), by = c("field_order")]
            .value_dt[field_an == "N" & !is_auto,
                      value := list(list(tryCatch(as.numeric(value),
                                                  warning = function (c) unlist(value)))),
                      by = c("field_order")]

            private$m_value <- .value_dt[, value]
            # }}}
        },

        check_object = function (.data = NULL, print = TRUE) {
        # check if there are errors in current object
            # {{{
            private$m_check <- NULL
            if (is.null(.data)) {
                private$m_check_data <- private$m_fields[
                    field_order <= length(private$m_value)][
                    , value := private$m_value]
            } else {
                private$m_check_data <- .data
            }
            on.exit({private$m_check_data <- NULL})

            # check scalar
            private$check_scalar()
            # check missing required fields
            private$check_missing()
            # check invalid auto
            private$check_auto()
            # exclude auto fields below during checking
            private$m_check_data <- private$m_check_data[
                !purrr::map_lgl(value, ~tolower(.x) %in% c("autosize", "autocalculate"))]
            # check invalid type
            private$check_type()
            # check invalid integer
            private$check_integer()
            # check invalid choice
            private$check_choice()
            # check range exceeding
            private$check_range()

            if (print) {
                private$print_check()
                return(invisible(private$m_check))
            } else {
                return(private$m_check)
            }
            # }}}
        },

        check_scalar = function () {
        # check if every value is a scalar
            # {{{
            res_scalar <- private$m_check_data[!is.na(value)][
                !purrr::map_lgl(value, is_scalar),
                list(field_order, `_field`, `_field_ip`, value)]
            private$m_check$scalar <- res_scalar
            if (not_empty(res_scalar)) {
                private$m_check_data <- private$m_check_data[
                    field_order != res_scalar[, field_order]]
            }
            # }}}
        },

        check_missing = function () {
        # check if there are missing required values
            # {{{
            res_missing <- private$m_check_data[required_field == TRUE][
                purrr::map_lgl(value, `==`, "") |
                purrr::map_lgl(value, is.na) |
                purrr::map_lgl(value, is.null),
                list(field_order, `_field`, `_field_ip`, value)]
            private$m_check$missing <- res_missing
            if (not_empty(res_missing)) {
                private$m_check_data <- private$m_check_data[
                    field_order != res_missing[, field_order]]
            }
            # }}}
        },

        check_auto = function () {
        # check if there are invalid autosize and autocalculate fields
            # {{{
            # invalid autosize fields
            res_autosize <- private$m_check_data[autosizable == FALSE][
                tolower(unlist(value)) == "autosize",
                list(field_order, `_field`, `_field_ip`, value)]
            private$m_check$autosize <- res_autosize
            if (not_empty(res_autosize)) {
                private$m_check_data <- private$m_check_data[
                    field_order != res_autosize[, field_order]]
            }

            # invalid autocalculate fields
            res_autocalculate <- private$m_check_data[autocalculatable == FALSE][
                tolower(unlist(value)) == "autocalculate",
                list(field_order, `_field`, `_field_ip`, value)]
            private$m_check$autocalculate <- res_autocalculate
            if (not_empty(res_autocalculate)) {
                private$m_check_data <- private$m_check_data[
                    field_order != res_autocalculate[, field_order]]
            }
            # }}}
        },

        check_type = function () {
        # check if there are character values which should be numeric or vice
        # versa
            # {{{
            # numeric type mismatch
            res_numeric <- private$m_check_data[field_an == "N"][
                !purrr::map_lgl(value, is.numeric),
                list(field_order, `_field`, `_field_ip`, value)]
            private$m_check$numeric <- res_numeric
            if (not_empty(res_numeric)) {
                private$m_check_data <- private$m_check_data[
                    field_order != res_numeric[, field_order]]
            }

            # character type mismatch
            res_character <- private$m_check_data[field_an == "A"][
                !purrr::map_lgl(value, is.character),
                list(field_order, `_field`, `_field_ip`, value)]
            private$m_check$character <- res_character
            if (not_empty(res_character)) {
                private$m_check_data <- private$m_check_data[
                    field_order != res_character[, field_order]]
            }
            # }}}
        },

        check_integer = function () {
        # check if there are invalid integer
            # {{{
            res_integer <- private$m_check_data[!is.na(value)][type == "integer"][
                !is_integerish(unlist(value)),
                list(field_order, `_field`, `_field_ip`, value)]
            private$m_check$integer <- res_integer
            if (not_empty(res_integer)) {
                private$m_check_data <- private$m_check_data[
                    field_order != res_integer[, field_order]]
            }
            # }}}
        },

        check_choice = function () {
        # check if there are invalid choices
            # {{{
            res_choice <- private$m_check_data[!is.na(value)][type == "choice"][
                !purrr::map2_lgl(value, key, ~tolower(.x) %in% tolower(.y)),
                list(field_order, `_field`, `_field_ip`, value, key)]
            private$m_check$choice <- res_choice
            if (not_empty(res_choice)) {
                private$m_check_data <- private$m_check_data[
                    field_order != res_choice[, field_order]]
            }
            # }}}
        },

        check_range = function () {
        # check if the value exceeds range
            # {{{
            res_range <- private$m_check_data[!is.na(value)][have_range == TRUE][
                !purrr::map2_lgl(value, range, in_range),
                list(field_order, `_field`, `_field_ip`, value, range)]
            private$m_check$range <- res_range
            if (not_empty(res_range)) {
                private$m_check_data <- private$m_check_data[
                    field_order != res_range[, field_order]]
            }
            # }}}
        },

        print_check = function (success = TRUE) {
        # print pretty check results
            # {{{
            if (all(purrr::map_lgl(private$m_check, is_empty))) {
                cli::cat_line(" ", clisymbols::symbol$tick, " ",
                              "No error found.")
            } else {
                error_num <- sum(purrr::map_int(private$m_check, nrow))
                cli::cat_line(" ", clisymbols::symbol$cross, " [", error_num, "] ",
                              "Errors found during value validation.")
                cli::cat_rule(line = 2)
                purrr::iwalk(private$m_check, ~if(not_empty(.x)) private$cat_errors(.y))
                cli::cat_line()
            }
            # }}}
        },

        cat_errors = function (type = c("scalar", "missing", "autosize",
                                        "autocalculate", "numeric", "character",
                                        "integer", "choice", "range")) {
        # cat detailed error messages of a single type
            # {{{
            type <- match.arg(type)

            .data <- private$m_check[[type]]
            error_num <- nrow(.data)
            index <- .data[, field_order]

            title <- switch(type,
                scalar = "Not Scalar",
                missing = "Missing Required Field",
                autosize = "Invalid Autosize Field",
                autocalculate = "Invalid Autocalculate Field",
                numeric = "Invalid Number",
                character = "Invalid Character",
                integer = "Invalid Integer",
                choice = "Invalid Choice",
                range = "Range Exceeding")

            bullet <- switch(type,
                scalar = "Fields below have multiple values.",
                missing = "Fields below are required but values are not given.",
                autosize = "Fields below cannot be `Autosize`.",
                autocalculate = "Fields below cannot be `Autocalculate`.",
                numeric = "Fields below should be numbers but are not.",
                character = "Fields below should be characters but are not.",
                integer = "Fields below are not or cannot be coerced into integers.",
                choice = "Fields below are not one of prescribed choices.",
                range = "Fields below exceed prescibed ranges.")

            cli::cat_line()
            cli::cat_rule(paste0("[", error_num, "] ", title))
            cli::cat_bullet(bullet, bullet = "circle_cross")
            ori_value <- private$m_value[index]
            cli::cat_line(self$out_lines(index))
            # }}}
        },

        no_error = function () {
        # return TRUE if there is no error in current object
            # {{{
            purrr::map_lgl(private$m_check)
            # }}}
        },

        str_range = function (range) {
        # print pretty value range
            # {{{
            if (is.null(range)) return("")
            if (range$lower_incbounds) {
                out <- paste0("[", range$lower)
            } else {
                out <- paste0("(", range$lower)
            }

            if (range$upper_incbounds) {
                out <- paste0(out, ", ", range$upper, "]")
            } else {
                out <- paste0(out, ", ", range$upper, ")")
            }

            return(out)
            # }}}
        },

        values_to_str = function (index = NULL, blank = FALSE) {
        # convert all NAs to "" and change all values to character
            # {{{
            if (is.null(index)) {
                .value <- private$m_value
            } else {
                valid <- self$is_valid_field_index(index)
                assert_that(all(valid), msg = paste0("Invalid field index found: ",
                    backtick_collapse(index[!valid])))
                .value <- private$m_value[index]
            }

            if (blank) {
                res <- purrr::map(.value, ~ifelse(is.na(.x), "<Blank>", .x))
            } else {
                res <- purrr::map(.value, ~ifelse(is.na(.x), "", .x))
            }

            return(as.character(res))
            # }}}
        },

        out_index = function (index) {
        # return formated field index
            # {{{
            out <- stringr::str_pad(index, nchar(max(index)), "left")
            # }}}
        },

        out_values = function (index = NULL, leading = 4L, length = 29L, blank = FALSE) {
        # return IDF format value output
            # {{{
            values <- private$values_to_str(index, blank)

            if (length(values) == length(private$m_value)) {
                if (length(values) > 1L) {
                    res <- c(paste0(values[-length(values)], ","),
                             paste0(values[length(values)], ";"))
                } else {
                    res <- paste0(values, ";")
                }
            } else {
                res <- paste0(values, ",")
            }

            spcs <- strrep(" ", leading)
            res <- paste0(spcs, res)

            res <- purrr::map_chr(res, ~
                ifelse(nchar(.x) > length,
                       paste0(.x, "  "),
                       stringr::str_pad(.x, length, "right")))

            return(res)
            # }}}
        },

        out_names = function (index = NULL, ip_units = FALSE) {
        # return IDF format field names
            # {{{
            nms <- self$field_name(index = index, unit = TRUE, in_ip = ip_units)
            paste0("!- ", nms)
            # }}}
        },

        out_comments = function () {
        # return formated comments
            # {{{
            if (is_empty(private$m_comment)) return(invisible(NULL))

            out <- private$m_comment
            # TODO: add color to macro lines using `crayon` package
            is_macro <- startsWith(out, "#")
            out[!is_macro] <- paste0("!", out[!is_macro])

            return(out)
            # }}}
        }
        # }}}
    )
)
# }}}

# in_range{{{
in_range <- function (x, range) {
    if (range$lower_incbounds) {
        l <- x >= range$lower
    } else {
        l <- x > range$lower
    }

    if(!l) return(FALSE)

    if (range$upper_incbounds) {
        u <- x <= range$upper
    } else {
        u <- x < range$upper
    }

    return(u)
}
# }}}
