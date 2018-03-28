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
#' idfobj$set_comment(comment, append = TRUE, width = 0L)
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

        initialize = function (object_id, class, comment, value, idd, ...) {
            # {{{
            private$m_object_id <- object_id
            private$m_value <- as.list(value)
            private$m_comment <- unlist(comment)

            .iddobj <- idd$object(class)
            len <- length(private$m_value)
            assert_that(.iddobj$is_valid_field_num(len),
                        msg = paste0("Object [ID:", private$m_object_id, "] in class ",
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
            private$m_fields <- `._get_private`(.iddobj)$m_fields
            private$m_properties <- `._get_private`(.iddobj)$m_properties
            private$enforce_value_type()
            # }}}
        },

        # PUBLIC FUNCTIONS
        # {{{
        id = function () {
            # return object id
            # {{{
            private$m_object_id
            # }}}
        },

        get_comment = function () {
            # return object comments
            # {{{
            private$m_comment
            # }}}
        },

        set_comment = function (comment, append = TRUE, width = 0L) {
            # set object comment
            # {{{
            if (is_empty(comment)) {
                private$m_comment <- NULL
            } else {
                assert_that(is.character(comment))
                if (width != 0L) {
                    assert_that(is_count(width))
                    comment <- strwrap(comment, width = width)
                }

                if (is.null(append)) {
                    private$m_comment <- comment
                } else {
                    assert_that(is_scalar(append),
                                msg = "`append` should be NULL or a single logical value.")
                    if (append) {
                        private$m_comment <- c(private$m_comment, comment)
                    } else {
                        private$m_comment <- c(comment, private$m_comment)
                    }
                }
            }

            self
            # }}}
        },

        get_value = function (index = NULL, name = NULL) {
            # return object values
            # {{{
            if (all(is.null(index), is.null(name))) return(flatten_ref(private$m_value))
            index <- private$fields(index, name)[, field_order]
            flatten_ref(private$m_value[index])
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
            # TODO: check other $num_fields() in IDFObject
            num <- length(private$m_value)
            # before checking, set num_to_add to NULL
            num_to_add <- NULL
            # if all named, using named index
            # {{{
            if (named) {
                # check duplication of names
                if (anyDuplicated(nms)) {
                    stop("Duplicated field names found: ",
                         backtick_collapse(nms[duplicated(nms)]), ".",
                         call. = FALSE)
                }

                # check if all inputs have valid field names in current
                # IDDObject
                valid <- nms %in% private$field_name_std() |
                         nms %in% private$field_name_lcase()
                # if not, only make sense when this is an extensible object
                if (!all(valid)) {
                    # stop if it is not an extensible object
                    if (!self$is_extensible()) {
                        stop("Invalid field names found for class ",
                             self$class_name(), ": ",
                             backtick_collapse(nms[!valid]), ".",
                             call. = FALSE)
                    # if it is an extensible object
                    } else {
                        # but the number of values is less than that in
                        # IddObject
                        if (num < nrow(private$m_fields)) {
                            stop("Invalid field names found for class ",
                                 self$class_name(), ": ",
                                 backtick_collapse(nms[!valid]), ".",
                                 call. = FALSE)
                        }
                        new_nms <- nms[!valid]
                        new_num <- length(new_nms)
                        # Add new extensible groups to validate field names.
                        # It should be safe as added extensible group will be
                        # deleted if invalid field names found
                        num_to_add <- round(new_num / self$num_extensible())
                        num_to_add <- ifelse(num_to_add == 0, 1L, num_to_add)
                        self$add_extensible_groups(num_to_add)
                        # check if new field names are valid
                        new_valid <- new_nms %in%
                            self$field_name(seq(num + 1, length.out = new_num))
                        if (!all(new_valid)) {
                            # remove added extensible groups
                            self$del_extensible_groups(num_to_add)
                            stop("Invalid field names found for class ",
                                 self$class_name(), ": ",
                                 backtick_collapse(new_nms[!new_valid]), ".",
                                 call. = FALSE)
                        }
                        # check if the number of new fields is acceptable
                        if (!self$is_valid_field_num(num + new_num)) {
                            # remove added extensible groups
                            self$del_extensible_groups(num_to_add)
                            stop("Invalid field number found for class ",
                                 self$class_name(), ": ", backtick(num + new_num),
                                 ". Should be ",
                                 backtick(paste0(self$num_fields(), " + ",
                                                 self$num_extensible(), " * X")),
                                 ".", call. = FALSE)
                        }
                    }
                } else {
                    # check if all inputs have valid field names in current
                    # IDFObject
                    valid_cur <- nms %in% private$field_name_std(seq_len(num)) |
                                 nms %in% private$field_name_lcase(seq_len(num))
                    # there are new fields to set
                    if (!all(valid_cur)) {
                        index <- self$field_index(nms)
                        # check if the total field number is acceptable
                        if (!self$is_valid_field_num(max(index))) {
                            stop("Failed to add new fields named: ",
                                 backtick_collapse(nms[!valid_cur]), ". ",
                                 "Invalid field number found for class ",
                                 self$class_name(), ": ", backtick(num + sum(!valid_cur)),
                                 ". Should be ",
                                 backtick(paste0(num, " + ",
                                                 self$num_extensible(), " * X")),
                                 ".", call. = FALSE)
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
                        stop("Invalid field number found for class ",
                             self$class_name(), ": ", backtick(len), ". ",
                             "Should be less than ", self$num_fields(), ".",
                             call. = FALSE)
                    } else if (private$get_extensible_field_index(len) != 0) {
                        stop("Invalid field number found for class ",
                             self$class_name(), ": ", backtick(len), ". ",
                             "Should be ",
                             backtick(paste0(self$num_fields(), " + ",
                                             self$num_extensible(), " * X")), ".",
                             call. = FALSE)
                    }

                    num_to_add <- (len - num) / self$num_extensible()
                    private$add_extensible_groups(num_to_add)
                }

                index <- seq_len(len)
            }
            # }}}

            # TODO: check if below is ok for single field objects such
            # `Version`
            # check if there are reference fields in the input
            fields <- private$fields(index)
            # check if there are object-list fields in the input
            private$m_check_field <- data.table::copy(fields)[, value := flatten_ref(dots)]
            self$validate()

            # get indice of newly added fields
            new_index <- index[index > num]
            # get all valid field indice
            full_index <- seq_len(num)
            # get index that in the input but not in the current private$m_value
            mis_index <- full_index[full_index > num & full_index < max(index)]

            # if no error
            if (all(purrr::map_lgl(private$m_check, is_empty))) {
                # if there are reference fields, only need to modify the
                # reference map
                ref_field <- fields[has_reference == TRUE]
                ref_ord <- ref_field$field_order
                if (not_empty(ref_field)) {
                    line_ref <- private$m_refmap$field[
                        object_id == private$m_object_id & src_field_order %in% ref_ord,
                        which = TRUE]
                    if (not_empty(line_ref)) {
                        private$m_refmap$field[line_ref,
                            `:=`(value = unlist(unname(dots[ref_ord])),
                                 refvalue = purrr::map2(refvalue, dots[ref_ord], set_ref_value))]
                        # replace the value
                        dots[ref_ord] <- private$m_refmap$field[line_ref, refvalue]
                    # this is the case when the input value is a new reference
                    # add it to the field reference map
                    } else {
                        new_ref <- ref_field[, list(reference, class_order, class, field_order)][
                            , `:=`(object_id = private$m_object_id,
                                   value = unlist(unname(dots[ref_ord])),
                                   refvalue = purrr::map(unname(dots[ref_ord]), create_ref))]
                        data.table::setnames(new_ref,
                             c("class_order", "class", "field_order"),
                             c("src_class_order", "src_class", "src_field_order")
                        )
                        private$m_refmap$field <- data.table::rbindlist(
                            list(private$m_refmap$field, new_ref), use.names = TRUE)
                        dots[ref_ord] <- new_ref$refvalue
                    }

                }

                # if there are object-list fields, set to the new reference
                # value
                obj <- fields[has_object_list == TRUE, field_order]
                if (not_empty(obj)) {
                    dots[obj] <- purrr::map2(dots[obj], fields[obj],
                        ~{
                            private$m_refmap$field[
                                reference %in% .y[["reference"]] & value == .x, refvalue]
                         }
                    )
                }

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
                return(self)
            } else {
                # remove added extensible groups
                if (not_empty(num_to_add)) self$del_extensible_groups(num_to_add)
                # print checking results before stop
                i_print_check(private$m_check, ip_unit = FALSE)
                stop("Failed to set field values.", call. = FALSE)
            }
            # }}}
        },

        validate = function () {
            # validate field in terms of all creteria
            # {{{
            if (is_empty(private$m_check_field)) {
                private$m_check_field <- private$m_fields[
                    field_order <= length(private$m_value)][
                    , value := flatten_ref(private$m_value)]
            }
            i_check(private, type = "idfobject")
            # }}}
        },

        is_valid = function () {
            # return TRUE if there are no errors after `$check()`
            # {{{
            res <- self$validate()
            if (all(purrr::map_lgl(res, is_empty))) TRUE else FALSE
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
            id_class <- paste0("[ID: ", private$m_object_id, "] Class ", backtick(self$class_name()))
            cli::cat_line(clisymbols::symbol$star, clisymbols::symbol$star, id_class, clisymbols::symbol$star, clisymbols::symbol$star)

            # comment
            if (comment) {
                if (not_empty(private$m_comment)) {
                    cli::cat_rule(center = "* COMMENTS *", line = 1)
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
        m_object_id = NA_integer_,
        m_value = list(),
        m_comment = character(),
        m_iddobj = NULL,
        m_properties = NULL,
        m_fields = NULL,
        m_check = list(),
        m_check_field = list(),
        m_order = new.env(),
        m_ref_map = new.env(),
        # }}}

        # PRIVATE FUNCTIONS
        # {{{
        enforce_value_type = function () {
            # make sure that numeric strings are converted to numeric values and
            # all empty values are converted to corresponding NAs
            # {{{
            fields <- private$m_fields[seq_along(private$m_value)]
            an_a <- fields[["field_an"]] == "A"
            empty <- private$m_value == ""
            empty[is.na(empty)] <- TRUE
            is_auto <- tolower(private$m_value) %in% c("autosize", "autocalculate")
            na_chr <- an_a & empty
            na_dbl <- !an_a & empty
            an_n <- !an_a & !is_auto
            if (any(na_chr)) private$m_value[na_chr] <- rep(NA_character_, sum(na_chr))
            if (any(na_dbl)) private$m_value[na_dbl] <- rep(NA_real_, sum(na_dbl))
            num <- suppressWarnings(as.numeric(unlist(private$m_value)))
            private$m_value[an_n & !is.na(num)] <- num[an_n & !is.na(num)]

            # only check for non-empty, non-already-auto character fields
            tgt <- !empty & !is_auto & an_a
            if (!any(tgt)) return(invisible(NULL))
            # for fields that have \reference
            if (any(fields[["has_reference"]])) {
                ref_val <- private$m_refmap$field[object_id == private$m_object_id,
                    list(src_field_order, refvalue)]
                if (not_empty(ref_val)) {
                    # There are some fields that have more than one reference
                    # names. As the environment is the same, it will be ok to
                    # only use the first one
                    ref_val <- ref_val[, data.table::first(.SD), by = src_field_order]
                    # replace the value
                    private$m_value[ref_val[["src_field_order"]]] <- ref_val[["refvalue"]]
                }
            }

            # for fields that have \object-list
            obj_l <- fields[tgt & has_object_list == TRUE, list(field_order, object_list)]
            if (not_empty(obj_l)) {
                src_val <- purrr::map2(obj_l[["field_order"]], obj_l[["object_list"]], ~{
                    # value
                    val <- private$m_value[[.x]]
                    # sources
                    refval <- private$m_refmap$field[reference %in% .y & value == val, refvalue]
                    # no match
                    if (not_empty(refval)) {
                        refval[[1]]
                    } else {
                        val
                    }
                })
                private$m_value[obj_l[["field_order"]]] <- src_val
            }
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
                values <- private$m_value
            } else {
                private$assert_valid_field_index(index)
                values <- private$m_value[index]
            }
            i_values_to_str(values, blank)
            # }}}
        },

        out_index = function (index) {
        # return formated field index
            # {{{
            i_out_index(index)
            # }}}
        },

        out_values = function (index = NULL, leading = 4L, length = 29L, blank = FALSE) {
        # return IDF format value output
            # {{{
            values <- private$values_to_str(index, blank)
            end <- length(values) == length(private$m_value)
            i_out_values(values, end, leading, length, blank)
            # }}}
        },

        out_names = function (index = NULL, ip_units = FALSE) {
        # return IDF format field names
            # {{{
            if (!is.null(index)) {
                valid <- index <= length(private$m_value)
                if (!all(valid)) {
                    warning("Indice larger than total field number have been remove.")
                    index <- index[valid]
                }
            }
            i_out_names(private$fields(index), ip_units)
            # }}}
        },

        out_comments = function () {
        # return formated comments
            # {{{
            i_out_comments(private$m_comment)
            # }}}
        }
        # }}}
    )
)
# }}}

# in_range: check if input is in a specified range
# {{{
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

# i_check_scalar: check if every value is a scalar
# {{{
i_check_scalar <- function (private, cols = c("field_order", "_field",
                                              "_field_ip", "value")) {
    res_scalar <- private$m_check_field[!is.na(value)][
        !purrr::map_lgl(value, is_scalar), ..cols]
    private$m_check$scalar <- res_scalar
    if (not_empty(res_scalar)) {
        private$m_check_field <- private$m_check_field[
            !field_order %in% res_scalar[, field_order]]
    }
}
# }}}

# i_check_missing: check if there are missing required values
# {{{
i_check_missing <- function (private, cols = c("field_order", "_field",
                                               "_field_ip", "value")) {
    res_missing <- private$m_check_field[required_field == TRUE][
        purrr::map_lgl(value, `==`, "") |
        purrr::map_lgl(value, is.na) |
        purrr::map_lgl(value, is.null), ..cols]
    private$m_check$missing <- res_missing
    if (not_empty(res_missing)) {
        private$m_check_field <- private$m_check_field[
            !field_order %in% res_missing[, field_order]]
    }
}
# }}}

# i_check_auto: check if there are invalid autosize and autocalculate fields
# {{{
i_check_auto <- function (private, cols = c("field_order", "_field",
                                            "_field_ip", "value")) {
    # invalid autosize fields
    res_autosize <- private$m_check_field[autosizable == FALSE][
        tolower(unlist(value)) == "autosize", ..cols]
    private$m_check$autosize <- res_autosize
    if (not_empty(res_autosize)) {
        private$m_check_field <- private$m_check_field[
            !field_order %in% res_autosize[, field_order]]
    }

    # invalid autocalculate fields
    res_autocalculate <- private$m_check_field[autocalculatable == FALSE][
        tolower(unlist(value)) == "autocalculate", ..cols]
    private$m_check$autocalculate <- res_autocalculate
    if (not_empty(res_autocalculate)) {
        private$m_check_field <- private$m_check_field[
            !field_order %in% res_autocalculate[, field_order]]
    }
}
# }}}

# i_check_type: check if there are character values which should be numeric or vice versa
# {{{
i_check_type <- function (private, cols = c("field_order", "_field",
                                            "_field_ip", "value")) {
    # numeric type mismatch
    res_numeric <- private$m_check_field[field_an == "N"][
        !purrr::map_lgl(value, is.numeric), ..cols]
    private$m_check$numeric <- res_numeric
    if (not_empty(res_numeric)) {
        private$m_check_field <- private$m_check_field[
            !field_order %in% res_numeric[, field_order]]
    }

    # character type mismatch
    res_character <- private$m_check_field[field_an == "A"][
        !purrr::map_lgl(value, is.character), ..cols]
    private$m_check$character <- res_character
    if (not_empty(res_character)) {
        private$m_check_field <- private$m_check_field[
            !field_order %in% res_character[, field_order]]
    }
}
# }}}

# i_check_integer: check if there are invalid integers
# {{{
i_check_integer <- function (private, cols = c("field_order", "_field",
                                               "_field_ip", "value")) {
    res_integer <- private$m_check_field[!is.na(value)][type == "integer"][
        !is_integerish(unlist(value)), ..cols]
    private$m_check$integer <- res_integer
    if (not_empty(res_integer)) {
        private$m_check_field <- private$m_check_field[
            !field_order %in% res_integer[, field_order]]
    }
}
# }}}

# i_check_choice: check if there are invalid choices
# {{{
i_check_choice <- function (private, cols = c("field_order", "_field",
                                              "_field_ip", "value", "key")) {
    res_choice <- private$m_check_field[!is.na(value)][type == "choice"][
        !purrr::map2_lgl(value, key, ~tolower(.x) %in% tolower(.y)), ..cols]
    private$m_check$choice <- res_choice
    if (not_empty(res_choice)) {
        private$m_check_field <- private$m_check_field[
            !field_order %in% res_choice[, field_order]]
    }
}
# }}}

# i_check_range: check if the value exceeds range
# {{{
i_check_range <- function (private, cols = c("field_order", "_field",
                                             "_field_ip", "value", "range")) {
    res_range <- private$m_check_field[!is.na(value)][have_range == TRUE][
        !purrr::map2_lgl(value, range, in_range), ..cols]
    private$m_check$range <- res_range
    if (not_empty(res_range)) {
        private$m_check_field <- private$m_check_field[
            !field_order %in% res_range[, field_order]]
    }
}
# }}}

# i_check_reference: check if there are invalid references
# {{{
i_check_reference <- function (private, cols) {
    # get fields that have \object-list
    targ <- setdiff(c(cols, "object_list"), "source")
    ref_field <- private$m_check_field[has_object_list == TRUE, ..targ]
    if (is_empty(ref_field)) return(data.table::data.table())
    ref_field <- ref_field[
        , lapply(.SD, unlist), by = setdiff(targ, c("value", "object_list"))][
        !is.na(value)]

    src <- data.table::rbindlist(
        list(private$m_refmap$class, private$m_refmap$field), fill = TRUE)[
        , list(reference, value)][, list(source = list(value)), by = reference]

    private$m_check$reference <- merge(ref_field, src,
        by.x = "object_list", by.y = "reference", all.x = TRUE, sort = FALSE)[
        , row_id := .I][
        !purrr::map2_lgl(value, source, ~toupper(.x) %in% toupper(.y)),
        ..cols, by = row_id]
}
# }}}

# i_out_lines: return output lines for certain fields
# {{{
i_out_lines <- function (values, fields, ip_units = FALSE) {
    .value <- i_out_values(values, leading = 1L, length = 20L, blank = TRUE)
    .name <- i_out_names(fields, ip_units = ip_units)
    .index <- i_out_index(fields[["field_order"]])
    res <- paste0(.index, ":", .value, .name)
    # check if there are class names in the fields
    if (has_name(fields, "class") && has_name(fields, "object_id")) {
        data.table::setorder(fields, class_order, object_id, field_order)
        fields[, `:=`(out = as.list(res), row_id = .I), by = .I]

        # add field char
        last_field <- fields[, row_id[.N], by = list(class, object_id)]$V1
        first_field <- setdiff(fields[, row_id[1L], by = list(class, object_id)]$V1, last_field)
        last_object <- fields[object_id %in% fields[, max(object_id), by = list(class_order)]$V1, unique(object_id)]
        fields[!object_id %in% last_object & row_id %in% last_field,
               out := list(list(paste0("  |  \\- ", out[[1]]))), by = row_id]
        fields[!object_id %in% last_object & row_id %in% first_field,
               out := list(list(paste0("  |  +- ", out[[1]]))), by = row_id]
        fields[!object_id %in% last_object & !row_id %in% c(first_field, last_field),
               out := list(list(paste0("  |  |- ", out[[1]]))), by = row_id]
        fields[object_id %in% last_object & row_id %in% last_field,
               out := list(list(paste0("     \\- ", out[[1]]))), by = row_id]
        fields[object_id %in% last_object & row_id %in% first_field,
               out := list(list(paste0("     +- ", out[[1]]))), by = row_id]
        fields[object_id %in% last_object & !row_id %in% c(first_field, last_field),
               out := list(list(paste0("     |- ", out[[1]]))), by = row_id]

        # add object char
        first_row_per_object <- fields[, row_id[1], by = list(class_order, object_id)]$V1
        first_row_per_last_object <- fields[object_id %in% last_object, row_id[1], by = object_id]$V1
        fields[setdiff(first_row_per_object, first_row_per_last_object),
               out := list(list(
                    c(paste0("  +- Object [ID:", object_id,"]"),
                      out[[1]]))),
               by = list(row_id)]
        fields[first_row_per_last_object,
               out := list(list(
                    c(paste0("  \\- Object [ID:", object_id,"]"),
                      out[[1]]))),
               by = list(row_id)]

        # add class char
        each_class <- fields[, row_id[1], by = class_order]$V1
        fields[each_class,
               out := list(list(
                    c("",
                      paste0("  Class ", backtick(class)),
                      out[[1]]))),
               by = row_id]

        res <- unlist(fields[["out"]])
        # clean
        fields[, c("out", "row_id") := NULL]
    } else {
        res <- c("", paste0("  ", res))
    }

    return(res)
}
# }}}

# i_out_names: return IDF format field names
# {{{
i_out_names <- function (fields, ip_units = FALSE) {
    nms <- i_field_name_std(fields, unit = TRUE, in_ip = ip_units)
    paste0("!- ", nms)
}
# }}}

# i_field_name_std: return standard field name
# {{{
i_field_name_std <- function (fields, unit = FALSE, in_ip = FALSE) {
    if (unit) {
        if (in_ip) {
            res <- fields[["_field_ip"]]
        } else {
            res <- fields[["_field"]]
        }
    } else {
        res <- fields[["_field_name"]]
    }

    return(res)
}
# }}}

# i_out_index: return right aligned field index
# {{{
i_out_index <- function (index) {
    paste0(" ", stringr::str_pad(index, nchar(max(index)), "left"))
}
# }}}

# i_out_values: return IDF format value output
# {{{
i_out_values <- function (values, end = FALSE, leading = 4L, length = 29L, blank = FALSE) {
    values <- i_values_to_str(values, blank)

    if (end) {
        if (length(values) > 1L) {
            res <- c(paste0(values[-length(values)], ","),
                     paste0(values[length(values)], ";"))
        } else {
            res <- paste0(values, ";")
        }
    } else {
        res <- paste0(values, ",")
    }

    res <- paste0(strrep(" ", leading), res)

    long <- nchar(res) > length
    res[long] <- paste0(res[long], " ")
    res[!long] <- stringr::str_pad(res[!long], length, "right")

    return(res)
}
# }}}

# i_out_comments: return comments
# {{{
i_out_comments <- function (comments) {
    if (is_empty(comments)) return(invisible(NULL))

    out <- comments
    # TODO: add color to macro lines using `crayon` package
    is_macro <- startsWith(out, "#")
    out[!is_macro] <- paste0("!", out[!is_macro])

    return(out)
}
# }}}

# i_values_to_str: convert all NAs to "" and change all values to character
# {{{
i_values_to_str <- function (values, blank = FALSE) {
    values <- flatten_ref(values)
    if (blank) {
        values[is.na(values)] <- as.list(rep("<Blank>", sum(is.na(values))))
    } else {
        values[is.na(values)] <- as.list(rep("", sum(is.na(values))))
    }

    return(as.character(values))
}
# }}}

# flatten_ref
# {{{
flatten_ref <- function (x) {
    purrr::map_if(x, is.environment, ~unlist(as.list(.x), use.names = FALSE))
}
# }}}

# create_ref
# {{{
create_ref <- function (value) {
    env <- new.env(parent = emptyenv(), size = 1L)
    env$value <- value
    env
}
# }}}

# print.IDFCheckRes: S3 method for printing IDF and IDFObject check results
# {{{
print.IDFCheckRes <- function (x, ip_unit = FALSE, ...) {
    i_print_check(x, ip_unit = FALSE)
}
# }}}

# i_print_check: print pretty check results
# {{{
i_print_check = function (check, ip_unit = FALSE) {
    empty <- purrr::map_lgl(check, is_empty)
    if (all(empty)) {
        cli::cat_line(" ", clisymbols::symbol$tick, " ", "No error found.")
    } else {
        error_num <- sum(purrr::map_int(check[!empty], nrow))
        cli::cat_line(" ", clisymbols::symbol$cross, " [", error_num, "] ",
                      "Errors found during value validation.")
        cli::cat_rule(line = 2)
        purrr::iwalk(check[!empty], ~i_cat_errors(.x, .y, ip_unit))
        cli::cat_line()
    }
}
# }}}

# i_cat_errors: cat detailed error messages of a single type
# {{{
i_cat_errors = function (check_data,
                         type = c("missing_object", "duplicate_object", "scalar",
                                  "missing", "autosize", "autocalculate",
                                  "numeric", "character", "integer", "choice",
                                  "range", "reference"),
                         ip_units = FALSE) {
    type <- match.arg(type)

    error_num <- nrow(check_data)
    index <- check_data[, field_order]

    title <- switch(type,
        missing_object = "Missing Required Object",
        duplicate_object = "Duplicated Unique Object",
        scalar = "Not Scalar",
        missing = "Missing Required Field",
        autosize = "Invalid Autosize Field",
        autocalculate = "Invalid Autocalculate Field",
        numeric = "Invalid Number",
        character = "Invalid Character",
        integer = "Invalid Integer",
        choice = "Invalid Choice",
        range = "Range Exceeding",
        reference = "Invalid Reference")

    bullet <- switch(type,
        missing_object = "Objects below are required but not exist.",
        duplicate_object = "Objects should be unique but have multiple instances.",
        scalar = "Fields below have multiple values.",
        missing = "Fields below are required but values are not given.",
        autosize = "Fields below cannot be `Autosize`.",
        autocalculate = "Fields below cannot be `Autocalculate`.",
        numeric = "Fields below should be numbers but are not.",
        character = "Fields below should be characters but are not.",
        integer = "Fields below are not or cannot be coerced into integers.",
        choice = "Fields below are not one of prescribed choices.",
        range = "Fields below exceed prescibed ranges.",
        reference = "Fields below are not one of valid references.")

    cli::cat_line()
    cli::cat_rule(paste0("[", error_num, "] ", title))
    cli::cat_bullet(bullet, bullet = "circle_cross")
    if (type == "missing_object") {
        cli::cat_bullet(backtick(check_data$class))
    } else {
        cli::cat_line(i_out_lines(check_data$value, check_data, ip_units))
    }
}
# }}}
