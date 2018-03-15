#' @export
# IDF {{{
IDF <- R6::R6Class(classname = "IDF",

    public = list(

        initialize = function (path, idd) {
            # {{{
            idf_file <- parse_idf_(path, idd)

            private$m_path <- path
            private$m_idd <- idd

            private$m_data<- private$m_idd$orders()[idf_file$value, on = "class"]

            private$m_version <- idf_file$version
            private$m_options <- idf_file$options
            p <- progress::progress_bar$new(total = nrow(idf_file$value))
            private$m_objects <- purrr::map(purrr::transpose(idf_file$value),
                                             ~IDFObject$new(.x, idd))
            # }}}
        },

        # PUBLIC FUNCTIONS
        # {{{
        id_of_class = function (class) {
        # return all valid id in one class or in current IDF
            # {{{
            assert_that(self$is_valid_class(class))
            cls <- class
            private$m_data[class == cls, object_id]
            # }}}
        },

        class_of_id = function (id) {
        # return the class name of given object id in current IDF
            # {{{
            assert_that(self$is_valid_id(id))
            private$m_data[object_id == id, class]
            # }}}
        },

        all_class = function (scope = "idf") {
        # return all valid class in current IDF
            # {{{
            scope <- match.arg(scope, choices = c("idf", "idd"))
            switch(scope,
                   idf = unique(private$m_data$class),
                   idd = private$m_idd$class_name())
            # }}}
        },

        all_id = function () {
        # return all object ids in current IDF
            # {{{
            private$m_data$object_id
            # }}}
        },

        is_valid_class = function (name, scope = "idf") {
        # check if the input string is a valid class name in current IDF
            # {{{
            assert_that(is_string(name))
            name %in% self$all_class(scope)
            # }}}
        },

        is_valid_id = function (id) {
        # check if the input number is a valid object id in current IDF
            # {{{
            assert_that(is_integerish(id))
            id %in% self$all_id()
            # }}}
        },

        object = function (id) {
        # return a single object
            # {{{
            assert_that(self$is_valid_id(id))
            private$m_objects[id][[1]]
            # }}}
        },

        objects = function (id) {
        # return a list which contains all objects with input object ids
            # {{{
            purrr::walk(id, ~assert_that(self$is_valid_id(.x)))
            private$m_objects[id]
            # }}}
        },

        objects_in_class = function (class) {
        # return a list which contails all objects in the target class
            # {{{
            ids <- self$id_of_class(class)
            private$m_objects[ids]
            # TODO: Return a IDFObjectInClass object and add methods to
            # manipulate then in a go
            # }}}
        },

        dup = function (id) {
        # duplicate an Object
            # {{{

            # }}}
        },

        add = function (class) {
        # add a new object in class
            # {{{

            # }}}
        },

        set = function (id) {
        # set values in an object
            # {{{

            # }}}
        },

        del = function (id) {
        # delete an object
            # {{{

            # }}}
        },

        diff = function (id) {
        # diff the values
            # {{{

            # }}}
        },

        check = function () {
        # check if there are errors in current model
            # {{{

            # }}}
        },

        out = function (format = c("asis", "sorted", "ori_bot", "ori_top"),
                        comment = TRUE, in_ip = FALSE) {
        # return save-ready format string
            # {{{
            format <- private$out_format(format)
            header <- private$out_header(format)
            lines <- private$out_line(format = format, comment = comment)
            c(header, lines)
            # }}}
        },

        print = function () {
            # {{{
            # get object count
            res <- private$m_idd$orders()[private$m_data, on = "class"][
                , list(num = .N), by = c("group", "class")]

            max_count <- res[, max(num)]

            res <- res[, count := paste0(
                "[", stringr::str_pad(num, nchar(max_count), "left", "0"), "]")]

            res[, grp := ""]

            res[res[, .I[1L], by = list(group)]$V1,
                grp := paste0("\nGroup: ", sQuote(group), "\n", cli::rule(), "\n")]

            out <- res[, paste0(grp, count, " ", class)]

            cat(out, sep = "\n")
            # }}}
        }
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS
        # {{{
        m_path = NULL,
        m_version = NULL,
        m_options = list(),
        m_objects = list(),
        m_data = NULL,
        m_idd = NULL,
        # }}}

        # PRIVATE FUNCTIONS
        # {{{
        out_format = function (format = c("asis", "sorted", "ori_bot", "ori_top")) {
        # return save format
            format <- match.arg(format)
            # {{{
            # Default use "SortedOrder"
            format <- switch(format,
                asis = private$m_options$format,
                sorted = "SortedOrder",
                ori_bot = "OriginalOrderBottom",
                ori_top = "OriginalOrderTop")
            if (is.null(format)) format <- "SortedOrder"

            return(format)
            # }}}
        },

        out_header = function (format) {
        # return output header
            # {{{
            if (private$m_options$idfeditor) {
                header_generator <- "!-Generator IDFEditor"
            } else {
                header_generator <- "!-Generator eplusr"
            }

            header_option <- paste0("!-Option ", format)

            if (private$m_options$special_format) {
                warning("Currently option 'UseSpecialFormat' is not supported. Standard foramt will be used.",
                        call. = FALSE)
            }

            if (private$m_options$view_in_ip) {
                warning("Currently option 'ViewInIPunits' is not supported. SI format will be used.",
                        call. = FALSE)
            }
            # currently, "UseSpecialFormat" and "ViewInIPunits" options are not supported.
            special_format <- NULL
            ip_unit <- NULL

            header_option <- paste0(header_option, " ", special_format, " ", ip_unit)
            header_option <- stringr::str_trim(header_option, "right")

            # TODO: Add "UseSpecialFormat" and "ViewInIPunits" support
            header <- c(
                header_generator,
                header_option,
                "",
                "!-NOTE: All comments with '!-' are ignored by the IDFEditor and are generated automatically.",
                "!-      Use '!' comments if they need to be retained when using the IDFEditor.",
                "")

            return(header)
            # }}}
        },

        out_line = function (comment = TRUE, format) {
        # return output lines according to the format
            # {{{
            setorder(private$m_data, class_order, object_id)
            private$m_data[, class_group := .GRP, by = list(rleid(class))]
            l_cls <- private$m_data[, .I[1], by = list(class_group)]$V1
            id_cls <- private$m_data[l_cls, object_id]
            max_id <- max(self$all_id())
            l_list <- purrr::imap(.get(idf, "m_objects"),
                ~{
                    out_str <- ""
                    if (.y %in% id_cls) {
                        out_cls <- paste0(
                            "!-   ===========  ALL OBJECTS IN CLASS: ",
                            toupper(.x$class_name()), " ===========")
                        out_str <- c("", out_cls, "")
                    }
                    out_str <- c(out_str, .x$out(comment = comment))
                    if (.y != max(idf$all_id())) {
                        out_str <- c(out_str, "")
                    }
                    return(out_str)
                })
            l_str <- purrr::simplify(l_list)
            return(l_str)
            # }}}
        },

        check_missing = function () {
        # check if there are missing required fields
            # {{{

            # }}}
        },

        check_numeric = function () {
        # check if there are fields should be numeric but cannot be converted
        # into numbers
            # {{{

            # }}}
        },

        check_integer = function () {
        # check if there are incorrect integer fields
            # {{{

            # }}}
        },

        check_choice = function () {
        # check if there are incorrect field choice values
            # {{{

            # }}}
        },

        check_objectlist = function () {
        # check if there are incorrect field references
            # {{{

            # }}}
        },

        check_range = function () {
        # check if there are incorrect field range
            # {{{

            # }}}
        },

        check_auto = function () {
        # check if there are fields with value 'autosize' or 'autocalculate'
        # but do not have attributes of 'autosizable' or 'autocalculatable'.
            # {{{

            # }}}
        }
        # }}}
    )
)
# }}}

#' @export
# IDFObject {{{
IDFObject <- R6::R6Class(classname = "IDFObject",
    inherit = IDDObject,

    public = list(

        initialize = function (idfobj_list, idd) {
            # {{{
            private$m_id <- idfobj_list$object_id
            private$m_value <- as.list(idfobj_list$value)
            private$m_comment <- idfobj_list$comment

            .iddobj <- idd$objects(idfobj_list$class)[[1]]
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
                         paste0(sQuote(nms[duplicated(nms)]), collapse = ", "),
                         call. = FALSE)
                }

                # check if all inputs have valid names
                valid <- self$is_valid_field_name(nms)
                if (!all(valid)) {
                    # check if there are newly added extensible fields
                    if (!self$is_extensible()) {
                        stop("Invalid field names found: ",
                             paste0(sQuote(nms[!valid]), collapse = ", "),
                             call. = FALSE)
                    } else {
                        # get new field names
                        new_fields <- nms[!valid]
                        new_num <- length(new_fields)
                        new_nms <- names(new_fields)
                        # check if the number of new fields is acceptable
                        if (!self$is_valid_field_num(num + new_num)) {
                            stop("Invalid field number found: ", sQuote(len), ". ",
                                 "Should be ", sQuote(paste0(self$num_fields(), " + ",
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
                                 paste0(sQuote(new_nms[!new_valid]), collapse = ", "),
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
                        stop("Invalid field number found: ", sQuote(len), ". ",
                             "Should be ", sQuote(paste0(self$num_fields(), " + ",
                                                         self$num_extensible(), " * X.")))
                    }

                    if (private$get_extensible_field_index(len) != 0) {
                        stop("Invalid field number found: ", sQuote(len), ". ",
                             "Should be ", sQuote(paste0(self$num_fields(), " + ",
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
                                paste0(sQuote(mis_index), collapse = ", "),
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
            id_class <- paste0("[ID: ", private$m_id, "] Class ", sQuote(self$class_name()))
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
                    paste0(sQuote(index[!valid]), collapse = ", ")))
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

#' @export
# parse_idf_ {{{
parse_idf_ <- function (path, idd) {
    assert_that(is_idd(idd))

    idf_str <- read_idf(path)

    idf_version <- get_idf_ver(idf_str)

    idf_dt <- data.table(line = seq_along(idf_str), string = idf_str)

    # idf and idd version mismatch {{{
    idd_version <- idd$version()
    if (not_empty(idf_version)) {
        if (!grepl(idf_version, idd_version, fixed = TRUE)) {
            warning(msg("Version Mismatch. The file parsing is a differnet version
                        '",idf_version,"' than the EnergyPlus program and IDD file
                        you are using '",substr(idd_version, 1L, 3L),"'. Editing and
                        saving the file may make it incompatible with an older
                        version of EnergyPlus."),
                call. = FALSE)
        }
    }
    # }}}

    # mark type {{{
    # -3, unknown
    type_unknown <- -3L
    # -2, speical comment
    type_special <- -2L
    # -1, macro
    type_macro <- -1L
    #  0, block comment
    type_comment <- 0L
    #  1, object
    type_object <- 1L
    #  2, field
    type_field <- 2L
    #  3, last field in an object
    type_field_last <- 3L
    idf_dt[, type := type_unknown]
    setorderv(idf_dt, c("line", "type"))

    # delete blank lines
    idf_dt <- idf_dt[!(string %chin% "")]
    idf_dt[startsWith(string, "##"), type := type_macro]
    # handle EP-Macro lines {{{
    idf_macro <- idf_dt[type == type_macro]
    idf_macro[, space_loc := regexpr(" ", string, fixed = TRUE)]
    idf_macro[space_loc > 0L,
        `:=`(macro_key = substr(string, 1L, space_loc - 1L),
             macro_value = substr(string, space_loc + 1L, nchar(string)))]
    idf_macro[space_loc < 0L, macro_key := substr(string, 1L, nchar(string))]
    # unknown marco key {{{
    idf_errors_unknown_macro <- idf_macro[!(macro_key %chin% macro_dict),
                                          list(line, string)]
    if (not_empty(idf_errors_unknown_macro)) {
        parse_issue(path, type = "Unknown macro found", src = "IDF",
                    data_errors = idf_errors_unknown_macro)
    }
    # }}}
    # mark macro values as macro {{{
    macro_value <- idf_macro[!is.na(macro_value), unique(macro_value)]
    idf_dt[string %chin% macro_value, type := type_macro]
    # }}}
    # }}}
    # treat macro lines as the same as comments
    # idf_dt[type == type_macro, type := type_comment]
    idf_dt[startsWith(string, "!"), type := type_comment]
    idf_dt[startsWith(string, "!-"), type := type_special]
    # mark location of "!" and "!-"
    idf_dt[, explpt_loc := regexpr("!", string, fixed = TRUE)]
    idf_dt[, special_loc := regexpr("!-", string, fixed = TRUE)]
    # lines ending with comma and without explaination symbol must be a object
    idf_dt[explpt_loc < 0L & endsWith(string, ","), type := type_object]
    # extract comments with leading spaces in order to preserve the indentation.
    idf_dt[special_loc > 0L,
           comment := substr(string, explpt_loc + 2L, nchar(string))]
    idf_dt[special_loc < 0L & explpt_loc > 0L,
           comment := substr(string, explpt_loc + 1L, nchar(string))]
    # for commented objects
    idf_dt[special_loc > 0L & explpt_loc > 0L,
           comment := substr(string, explpt_loc + 1L, nchar(string))]
    idf_dt[type == type_macro, comment := string]
    # get the number of leading spaces in comment
    idf_dt[, leading_spaces := regexpr("\\S", comment) - 1L]
    # get the value for lines that have comments
    idf_dt[explpt_loc > 1L, value := trimws(substr(string, 1L, explpt_loc - 1L), "right")]
    # get the value for lines without comments
    idf_dt[explpt_loc < 0L, value := string]
    # mark the last field in an object
    idf_dt[endsWith(value, ";"), type := type_field_last]
    # clean unused columns
    idf_dt[, `:=`(explpt_loc = NULL, special_loc = NULL)]
    # }}}

    # special comment key and value {{{
    option_idfeditor <- FALSE
    option_special_format <- FALSE
    option_view_in_ip_units <- FALSE
    option_save <- NULL

    idf_option <- idf_dt[type == type_special]
    idf_option[, space_loc := regexpr(" ", comment, fixed = TRUE)]
    idf_option[, `:=`(special_key = toupper(substr(comment, 1L, space_loc - 1L)),
                      special_value = toupper(trimws(substr(comment, space_loc + 1L, nchar(comment)))))]
    idf_option <- idf_option[special_key %chin% c("GENERATOR", "OPTION")]
    if (not_empty(idf_option)) {
        idf_option <- idf_option[, strsplit(special_value, " ", fixed = TRUE)[[1]], by = list(line, string, special_key)]
        setnames(idf_option, "V1", "special_value")
        if (idf_option[special_key == "GENERATOR" & substr(special_value, 1L, 9L) == "IDFEDITOR",
            .N] > 1L) {
            option_idfeditor <- TRUE
        }
        if (idf_option[special_key == "OPTION" & special_value == "USESPECIALFORMAT",
                .N] == 1L) {
            option_special_format <- TRUE
        }
        if (idf_option[special_key == "OPTION" & special_value == "VIEWINIPUNITS",
            .N] == 1L) {
            option_view_in_ip_units <- TRUE
        }
        idf_option[special_key == "OPTION" & special_value == "SORTEDORDER",
                   option_save := "SortedOrder"]
        idf_option[special_key == "OPTION" & special_value == "ORIGINALORDERTOP",
                   option_save := "OriginalOrderTop"]
        idf_option[special_key == "OPTION" & special_value == "ORIGINALORDERBOTTOM",
                   option_save := "OriginalOrderBottom"]
        idf_errors_option_save <- idf_option[!is.na(option_save), list(line, string, option_save)]
        option_save <- idf_errors_option_save[, unique(option_save)]
        if (is_empty(option_save)) {
            option_save <- NULL
        }
        if (nrow(idf_errors_option_save) > 1L) {
            parse_issue(path, type = "More than one save option found", idf_errors_option_save,
                        src = "IDF", stop = FALSE,
                        info = msg("Only the first option '",option_save[1],"'
                                   will be used."))
        }
    }

    heading_options = list(
        save_format = option_save,
        special_format = option_special_format,
        view_in_ip = option_view_in_ip_units,
        idfeditor = option_idfeditor)
    # }}}

    # get rid of special comment lines
    idf_dt <- idf_dt[type != type_special]
    # handle condensed values {{{
    # if the sum of comma and semicolon > 2, then it must be a specially
    # formatted object or field. It may contain a class name, e.g.
    # 'Version,8.8;' and it may not, e.g. '0.0,0.0,0.0,' in
    # 'BuildingSurface:Detailed'.
    # get number of condensed values
    idf_dt[!is.na(value), `:=`(value_count = char_count(value, "[,;]"))]
    idf_dt[is.na(value), `:=`(value_count = 0L)]
    idf_dt <- idf_dt[!(type %in% c(type_macro, type_comment)),
        strsplit(value, "\\s*[,;]\\s*"), by = list(line)][
        idf_dt, on = "line"][value_count == 1L, V1 := value][, value := NULL]
    setnames(idf_dt, "V1", "value")
    # get row numeber of last field per condensed field line in each class
    line_value_last <- idf_dt[
        value_count > 1L & type == type_field_last, list(line_value_last = last(.I)), by = list(line, type)][
        , line_value_last]
    # set all type of condensed field lines to "field", including class names.
    idf_dt[value_count > 1L, type := type_field]
    # mark last field per object in condensed lines
    idf_dt[line_value_last, type := type_field_last]
    # make lines that only has one value as "field", excluding recognized class
    # names.
    idf_dt[type != type_object & value_count == 1L, type := type_field]
    # }}}

    # set row id
    idf_dt[, row_id := .I]
    # mark last field and remove trailing comma or semicolon in values {{{
    idf_dt[endsWith(value, ","), value := substr(value, 1L, nchar(value) - 1L)]
    idf_dt[endsWith(value, ";"),
           `:=`(type = type_field_last,
                value = substr(value, 1L, nchar(value) - 1L))]
    # }}}

    # set an id for last field per object {{{
    # if is the last field, then the line after last field line should be a
    # class name except the last field is the last non-blank and non-comment
    # line. Others are just normal fields.
    idf_dt[type == type_field_last, object_id := .GRP, by = list(row_id)]
    idf_dt <- idf_dt[!is.na(object_id), list(row_id, object_id)][
        idf_dt[, object_id := NULL], on = c("row_id"), roll = -Inf]
    # }}}

    # COMMENT (MACRO)
    # {{{
    idf_comment <- idf_dt[type %in% c(type_macro, type_comment), .SD,
        .SDcol = c("type", "object_id", "comment")]
    idf_comment[, field_order := seq_along(.I), by = list(object_id)]
    idf_comment[, .SD, .SDcol = c("type", "object_id", "field_order", "comment")]
    # }}}

    # CLASS & FIELD
    # {{{
    # get idf without comments
    # {{{
    # NOTE: currently, inline comments are not supported.
    idf_dt <- idf_dt[!(type %in% c(type_macro, type_comment)), .SD,
         .SDcol = c("row_id", "object_id", "line", "type", "value", "string")]
    # }}}

    # get class name
    # {{{
    # class name should be the same of 'value' column for first line grouped by
    # object_id
    idf_dt[idf_dt[, .I[1], by = object_id]$V1,
           `:=`(type = type_object, class_upper_case = toupper(value))]

    idf_idd_all <- .get(idd, "m_orders")[
        , class_upper_case := toupper(class)][
        idf_dt, on = "class_upper_case"][
        order(object_id, class_order)]

    # check for un-recognized class names {{{
    unknown_class <- idf_idd_all[type == type_object][
        !is.na(value)][is.na(class), list(line, string)]
    if (not_empty(unknown_class)) {
        parse_issue(path, type = "Object type not recognized", src = "IDF",
                    data_errors = unknown_class,
                    info = "This error may be caused by a misspelled class name.")
    }
    # }}}

    # fill class downwards
    idf_field <- idf_idd_all[!is.na(class), list(row_id, class, class_order)][
        idf_dt, on = c("row_id"), roll = Inf][
        # get rid of class lines
        type > type_object]
    # order fields per object
    idf_field[, field_order := seq_along(.I), by = list(object_id)]
    # idf_field <- .get(idd, "m_data")[
    #     , .SD, .SDcol = c("class_order", "class", "field_order", "field_an")][
    #     idf_field, on = c("class_order", "class", "field_order")][
    #     order(object_id, class_order, field_order)]

    # # get the line number of autosized or autocalculated fields
    # line_auto <- idf_field[tolower(value) %in% c("autosize", "autocalculate"), which = TRUE]
    # idf_field[, row_id := .I][
    #           , value_list := list(list(value)), by = row_id][
    #           , value := NULL]
    # setnames(idf_field, "value_list", "value")
    # # change value into a list with each member has the right type according to
    # # field AN
    # # for empty numeric fields, change them into numeric NA
    # idf_field[-line_auto & field_an == "N" & unlist(value) == "",
    #     value := list(list(NA_real_)), by = c("class_order", "field_order")]
    # # for unempty numeric fields, change them into numeric types
    # idf_field[-line_auto & field_an == "N",
    #     value := list(list(as.numeric(value))), by = c("class_order", "field_order")]
    # # for empty character fields, change them into character NA
    # idf_field[-line_auto & field_an == "A" & unlist(value) == "",
    #     value := list(list(NA_character_)), by = c("class_order", "field_order")]

    # }}}
    # }}}

    # get splited idf values
    idf_value <- idf_field[, .SD, .SDcol = c("object_id", "class", "value")][
                           , list(value = list(value)), by = list(object_id, class)]

    # currently, ep-macro lines are not supported
    idf_comment <- idf_comment[, .SD, .SDcol = c("object_id", "comment")][
                               , list(comment = list(comment)), by = list(object_id)]

    idf_value <- idf_comment[idf_value, on = "object_id"]

    idf <- list(version = idf_version,
                options = heading_options,
                value = idf_value)

    return(idf)
}
# }}}

#######################################################################
#                             ASSERTIONS                              #
#######################################################################
on_failure(IDF$public_methods$is_valid_class) <- function (call, env) {
    paste0(sQuote(eval(call$name, env)), " is not a valid class name in current ",
           toupper(eval(call$scope, env)), ".")
}

on_failure(IDF$public_methods$is_valid_id) <- function (call, env) {
    paste0(sQuote(eval(call$id, env)), " is not a valid object ID in current IDF.")
}

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

# read_idf {{{
read_idf <- function (filepath) {
    assert_that(is_readable(filepath))

    con = file(filepath)
    idf_str <- readLines(con, encoding = "UTF-8", warn = FALSE)
    # Get rid of unparsable characters
    idf_str <- iconv(idf_str, to = "UTF-8", sub = " ")
    close(con)

    # Get rid of preceeding and trailing spaces
    idf_str <- trimws(idf_str, "both")

    assert_that(not_empty(idf_str))

    setattr(idf_str, "path", normalizePath(filepath, winslash = "/"))

    return(idf_str)
}
# }}}
# get_idf_ver {{{1
get_idf_ver <- function (idf_str) {
    ver_normal <- idf_str[endsWith(idf_str, "Version Identifier")]
    ver_special <- idf_str[startsWith(idf_str, "Version")]

    if (length(ver_normal) == 1L) {
        ver_line <- ver_normal
    } else if (length(ver_special) == 1L){
        ver_line <- ver_special
    } else {
        return(NULL)
    }

    ver_pt <- regexpr("\\d", ver_line)
    ver <- substr(ver_line, ver_pt, ver_pt + 2)

    return(ver)
}
# }}}1
