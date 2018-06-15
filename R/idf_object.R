#' EnergyPlus IDF objects
#'
#' \code{IDFObject} is a R6 class used internally as members in \code{IDF} R6
#' class. \code{IDFObject} inherits \code{\link{IDDObject}}, so all methods of
#' \code{IDDObject} are available for \code{IDFobject}.
#'
#' @section Usage:
#' ```
#'
#' idfobj <- IDFObject$new(object_id)
#'
#' idfobj$id()
#' idfobj$name()
#'
#' idfobj$get_comment()
#' idfobj$set_comment(comment, append = TRUE, width = 0L)
#' idfobj$get_value(index = NULL, name = NULL)
#' idfobj$set_value(..., defaults = TRUE)
#'
#' idfobj$validate()
#'
#' idfobj$reference_map()
#'
#' idfobj$value_table(all = FALSE, unit = TRUE, wide = FALSE, string_value =
#' TRUE)
#'
#' idfobj$is_valid()
#' idfobj$has_reference_by()
#' idfobj$has_reference_from()
#' idfobj$string(comment = TRUE, leading = 4L, sep_at = 29L)
#'
#' idfobj$print(comment = TRUE)
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
#' @importFrom glue glue
#' @importFrom data.table data.table rbindlist setattr setorder setnames
#' @importFrom purrr splice map_lgl
#' @importFrom cli cat_line cat_rule cat_bullet

#' @importFrom rlang dots_splice
#' @importFrom stringr str_pad
#' @importFrom clisymbols symbol
#' @importFrom purrr map map_chr map_lgl map2_lgl iwalk
#' @importFrom assertthat assert_that
#' @return An IdfObject object
#' @docType class
#' @seealso \code{\link{IddObject}}
#' @name idf_object
#' @author Hongyuan Jia
NULL

# IdfObject {{{
IdfObject <- R6::R6Class(classname = "IdfObject",
    inherit = IddObject,
    public = list(
        initialize = function (object_id) {
            # {{{
            assert_that(!is.null(private$m_idf_tbl), !is.null(private$m_idd_tbl),
                        !is.null(private$m_options), !is.null(private$m_log),
                msg = glue::glue("IdfObject can be created only after a parent \\
                    Idf object has been initialized.")
            )

            assert_that(is_count(object_id))
            assert_that(object_id %in% private$m_idf_tbl$object[["object_id"]],
                msg = glue::glue("Failed to create IdfObject. Invalid object \\
                id found: `{object_id}`.")
            )
            private$m_object_id <- object_id
            private$m_class_name <- private$m_idf_tbl$object[
                object_id == private$m_object_id][
                private$m_idd_tbl$class, on = "class_id", nomatch = 0L, class_name]
            private$m_object_name <- private$m_idf_tbl$object[
                object_id == private$m_object_id, object_name]
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

        name = function () {
            # return object name or NULL if not exists
            # {{{
            private$m_object_name
            # }}}
        },

        get_comment = function () {
            # return object comments
            # {{{
            private$m_idf_tbl$comment[object_id == private$m_object_id, comment]
            # }}}
        },

        set_comment = function (comment, append = TRUE, width = 0L) {
            # set object comment
            # {{{
            if (is.null(comment)) {
                private$m_idf_tbl$comment <- private$m_idf_tbl$comment[
                    object_id != private$m_object_id]
            } else {
                assert_that(is.character(comment))

                if (width != 0L) {
                    assert_that(is_count(width))
                    comment <- strwrap(comment, width = width)
                }

                old <- private$m_idf_tbl$comment[object_id == private$m_object_id, comment]

                if (is.null(append)) {
                    # delete the old
                    private$m_idf_tbl$comment <- private$m_idf_tbl$comment[
                        object_id != private$m_object_id]
                    # add new
                    private$m_idf_tbl$comment <- data.table::rbindlist(
                        list(private$m_idf_tbl$comment,
                             data.table::data.table(
                                comment = comment,
                                type = 0L,
                                object_id = private$m_object_id
                             )
                        ), fill = TRUE)[order(object_id), comment_id := .I]
                } else {
                    assert_that(is_scalar(append),
                        msg = "`append` should be NULL or a single logical value."
                    )
                    if (append) {
                        private$m_idf_tbl$comment <- data.table::rbindlist(
                            list(private$m_idf_tbl$comment,
                                 data.table::data.table(
                                    comment = comment,
                                    type = 0L,
                                    object_id = private$m_object_id
                                 )
                            ), fill = TRUE)[order(object_id), comment_id := .I]
                    } else {
                        comment <- c(comment, old)
                        # delete the old and add new
                        private$m_idf_tbl$comment <- private$m_idf_tbl$comment[
                            object_id != private$m_object_id]

                        private$m_idf_tbl$comment <- data.table::rbindlist(
                            list(private$m_idf_tbl$comment,
                                 data.table::data.table(
                                    comment = comment,
                                    type = 0L,
                                    object_id = private$m_object_id
                                 )
                            ), fill = TRUE)[order(object_id), comment_id := .I]
                    }

                }
            }

            # log
            private$m_log$unsaved <- TRUE
            # log order change
            private$m_log$order[object_id == private$m_object_id,
                object_order := object_order + 1L]

            self
            # }}}
        },

        get_value = function (index = NULL, name = NULL) {
            # return object values
            # {{{
            value_tbl <- private$value_tbl(index, name, with_field = TRUE)
            res <- value_list(value_tbl, private$m_options$view_in_ip)
            data.table::setattr(res, "names", value_tbl[["field_name"]])
            res
            # }}}
        },

        set_value = function (..., defaults = TRUE) {
            # set field value
            # {{{
            assert_that(!self$is_version(), msg = "Cannot modify `Version` object directly.")
            # capture all arguments in dots and flatten into a list
            dots <- purrr::splice(...)
            if (private$m_options$validate_level == "final") {
                assert_that(not_empty(dots), msg = "Please give values to set.")
                depth <- purrr::vec_depth(dots)
                assert_that(depth == 2L, msg = "Nested list is not supported.")
            }
            # check if there are NA or empty string or "" in the dots
            # check if there are fields to delete
            is_null <- purrr::map_lgl(dots, is.null)
            is_valid <- purrr::map_lgl(dots[!is_null], ~any(!is.na(.x) & length(.x) == 1))
            if (any(!is_valid)) {
                stop("Each value should be an atomic vector or NULL.", call. = FALSE)
            }
            # check if the dots have names
            nms <- names(dots)
            named <- all(nms != "", !is.null(nms))
            any_named <- any(nms != "")
            if (!named & any_named) {
                stop("Values should be either all unnamed or all named.", call. = FALSE)
            }
            # get current field number
            value_tbl <- private$value_tbl()
            # init vars
            num <- nrow(value_tbl)
            num_fields <- self$num_fields()
            num_ext <- self$num_extensible()
            num_to_add <- NULL
            cls <- private$m_class_name
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
                # IddObject
                valid <- nms %in% private$field_name_std() |
                         nms %in% private$field_name_lcase()
                # if not, it only makes sense when this is an extensible object
                if (!all(valid)) {
                    # stop if it is not an extensible object
                    if (num_ext == 0L) {
                        stop("Invalid field names found for class ",
                             cls, ": ", backtick_collapse(nms[!valid]), ".",
                             call. = FALSE)
                    # if it is an extensible object
                    } else {
                        # but the number of values is less than that in
                        # IddObject
                        if (num < num_fields) {
                            stop("Invalid field names found for class ",
                                 cls, ": ", backtick_collapse(nms[!valid]), ".",
                                 call. = FALSE)
                        }
                        new_nms <- nms[!valid]
                        new_num <- length(new_nms)
                        # Add new extensible groups to validate field names.
                        # It should be safe as added extensible group will be
                        # deleted if invalid field names are found
                        num_to_add <- ceiling(new_num / num_ext)
                        self$add_extensible_groups(num_to_add)
                        # check if new field names are valid
                        new_valid <- new_nms %in%
                            self$field_name(seq(num + 1, length.out = new_num))
                        if (!all(new_valid)) {
                            # remove added extensible groups
                            self$del_extensible_groups(num_to_add)
                            stop("Invalid field names found for class ",
                                 cls, ": ", backtick_collapse(new_nms[!new_valid]), ".",
                                 call. = FALSE)
                        }
                        # check if the number of new fields is acceptable
                        if (!self$is_valid_field_num(num + new_num)) {
                            # remove added extensible groups
                            self$del_extensible_groups(num_to_add)
                            stop("Invalid field number found for class ",
                                 cls, ": ", backtick(num + new_num), ". Should be ",
                                 backtick(paste0(num_fields, " + ", num_ext, " * X")),
                                 ".", call. = FALSE)
                        }
                    }
                } else {
                    # check if all inputs have valid field names in current
                    # IdfObject
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
                                 cls, ": ", backtick(num + sum(!valid_cur)),
                                 ". Should be ", backtick(paste0(num, " + ", num_ext, " * X")),
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
                if (len > num & num > 0L) {
                    if (!self$is_extensible()) {
                        stop("Invalid field number found for class ",
                             cls, ": ", backtick(len), ". ",
                             "Should be less than ", num_fields, ".",
                             call. = FALSE)
                    } else if (private$get_extensible_field_index(len) != 0) {
                        stop("Invalid field number found for class ",
                             cls, ": ", backtick(len), ". ",
                             "Should be ", backtick(paste0(num_fields, " + ", num_ext, " * X")),
                             ".", call. = FALSE)
                    }

                    num_to_add <- (len - num) / self$num_extensible()
                    private$add_extensible_groups(num_to_add)
                }

                index <- seq_len(len)
            }
            # }}}

            # prepare values for validation
            # {{{
            value_tbl <- private$value_tbl(with_field = TRUE, all = TRUE)
            vals <- dots
            vals[is_null] <- rep("", sum(is_null))
            value_to_set <- value_tbl[index,
                `:=`(value = as.character(unlist(vals)),
                     object_id = private$m_object_id)][index,
                `:=`(value_upper = toupper(value),
                     value_num = {
                         is_num = purrr::map_lgl(vals, is.numeric);
                         value_num[is_num] = unlist(vals[is_num]);
                         value_num[!is_num] = NA_real_;
                         value_num})][,
                `:=`(value_ipnum = value_num)]
            value_to_set <- update_value_num(
                value_to_set,
                private$m_options$num_digits,
                private$m_options$view_in_ip)

            # get the last value index
            last_ord <- max(num, index)
            # if default is TRUE
            if (defaults) {
                # make sure all required fields with defaults will be set
                req <- value_to_set[required_field == TRUE]
                if (not_empty(req)) last_req <- req[, max(field_order)] else last_req <- 0L
                last_val <- max(last_req, last_ord)
                can_def <- value_to_set[
                    !field_order %in% index[is_null] & # exclude fields to be deleted
                    field_order <= last_val &
                    is.na(value) &
                    !is.na(field_default),
                    field_order]
                # have fields with defaults
                if (not_empty(can_def)) {
                    # only reset the last order if having defaults to set
                    last_ord <- last_val
                    value_to_set[
                        can_def,
                        `:=`(value = field_default,
                             value_upper = toupper(field_default),
                             object_id = private$m_object_id)][
                        can_def & type %in% c("integer", "real"),
                        `:=`(value_num = as.double(field_default))]
                    # print messages
                    if (private$m_options$verbose_info) {
                        cli::cat_rule("Info")
                        dft_flds <- backtick(value_to_set$field_name[can_def])
                        flds <- paste0(can_def, ":", dft_flds, sep = ", ")
                        cli::cat_bullet(
                            paste0("| Values for field ", flds,
                                " are missing. Default values are added."),
                            bullet = "info")
                        cli::cat_line()
                    }
                }
            }

            # fill missing value_id
            max_id <- private$m_idf_tbl$value[, max(value_id)]
            value_to_set[field_order <= last_ord & is.na(value_id),
                `:=`(value_id = max_id + seq_along(value_id))]
            # for non-required fields that have no defaults, set value to ""
            value_to_set <- value_to_set[is.na(value),
                `:=`(value = "", value_upper = "", object_id = private$m_object_id)]
            # set to m_temp for value validation
            private$m_temp$value_to_set <- value_to_set
            # clean up temp variables
            on.exit({private$m_temp$value_to_set <- NULL}, add = TRUE)
            # }}}

            # validate
            i_collect_validate(private)

            # if no error
            if (all(purrr::map_lgl(private$m_validate, is_empty))) {
                # assign value
                new_val_tbl <- update_value_num(private$m_temp$value_to_set)[
                    !field_order %in% index[is_null],
                    .SD, .SDcols = names(private$m_idf_tbl$value)]
                private$m_idf_tbl$value <- data.table::rbindlist(list(
                    private$m_idf_tbl$value[object_id != private$m_object_id],
                    new_val_tbl
                ))
                data.table::setorder(private$m_idf_tbl$value, value_id)

                # update object name if necessary
                new_obj_nm <- private$m_temp$value_to_set[
                    field_order == 1L & field_name == "Name",
                    list(object_id, value, value_upper)]
                if (not_empty(new_obj_nm)) {
                    private$m_idf_tbl$object[
                        object_id == new_obj_nm$object_id,
                        `:=`(object_name = new_obj_nm$value,
                             object_name_upper = new_obj_nm$value_upper)
                    ]
                }

                # log order change
                private$m_log$order[object_id == private$m_object_id,
                    object_order := object_order + 1L]

                # if there are fields referenced by other fields, update
                ref <- private$m_idf_tbl$value_reference[private$m_temp$value_to_set,
                    on = c(reference_value_id = "value_id"), nomatch = 0L,
                    list(value_id, value, value_upper, value_num)]
                if (not_empty(ref)) {
                    private$m_idf_tbl$value[value_id %in% ref[["value_id"]],
                        `:=`(value = ref[["value"]],
                             value_upper = ref[["value_upper"]],
                             value_num = ref[["value_num"]])]
                    # log order change
                    ids <- private$m_idf_tbl$value[value_id %in% ref[["value_id"]], object_id]
                    private$m_log$order[object_id %in% ids,
                                        object_order := object_order + 1L]
                }

                # if there are fields referencing other fields, update the value
                # reference tbl
                obj <- private$m_idd_tbl$field_object_list[new_val_tbl,
                    on = "field_id", nomatch = 0L,
                    list(value_id, value_upper, field_id, object_list)]
                if (not_empty(obj)) {
                    obj_res <- obj[private$m_idd_tbl$field_reference,
                        on = c(object_list = "reference"), nomatch = 0L,
                        list(value_id, value_upper, i.field_id)][
                        private$m_idf_tbl$value,
                        on = c(i.field_id = "field_id", "value_upper"),
                        nomatch = 0L, list(value_id, i.value_id)]
                    if (not_empty(obj_res)) {
                        data.table::setnames(obj_res, c("value_id", "reference_value_id"))
                        private$m_idf_tbl$value_reference <- data.table::rbindlist(list(
                            private$m_idf_tbl$value_reference[!value_id %in% obj_res],
                            obj_res
                        ))
                        data.table::setorder(private$m_idf_tbl$value_reference, value_id)
                    }
                }
                private$m_log$unsaved <- TRUE

                return(self)
            } else {
                # remove added extensible groups
                if (not_empty(num_to_add)) self$del_extensible_groups(num_to_add)
                # print checking results before stop
                i_print_validate(private$m_validate)
                stop("Failed to set field values.", call. = FALSE)
            }
            # }}}
        },

        validate = function () {
            # validate field in terms of all creteria
            # {{{
            i_collect_validate(private)
            private$m_validate
            # }}}
        },

        reference_map = function () {
            # return reference and referenced fields
            # {{{
            all_fld <- private$m_idd_tbl$field[
                private$m_idd_tbl$field_property, on = "field_id", nomatch = 0L][
                private$m_idd_tbl$class, on = "class_id", nomatch = 0L,
                list(class_id, field_id, field_order, class_name, full_name, full_ipname)]

            # check if this object has been referenced by others
            ref_ed <- private$field_ref_by(with_field = TRUE)
            targ <- ref_ed[
                private$m_idf_tbl$value[, list(value_id, value, object_id, field_id)],
                on = c(target_value_id = "value_id"), nomatch = 0L][
                all_fld, on = "field_id", nomatch = 0L][, field_id := NULL]
            nms <- names(targ)
            dup_nms <- nms[startsWith(nms, "i.")]
            data.table::setnames(targ, dup_nms, gsub("i\\.", "target_", dup_nms))
            data.table::setorder(targ, field_order)
            targ[, target_object := list(lapply(target_object_id, private$IdfObject$new))]

            # check if this object has referenced fields from others
            ref <- private$field_ref_from(with_field = TRUE)
            src <- ref[
                private$m_idf_tbl$value[, list(value_id, value, object_id, field_id)],
                on = c(reference_value_id = "value_id"), nomatch = 0L][
                all_fld, on = "field_id", nomatch = 0L][, field_id := NULL]
            nms <- names(src)
            dup_nms <- nms[startsWith(nms, "i.")]
            data.table::setnames(src, dup_nms, gsub("i\\.", "reference_", dup_nms))
            data.table::setorder(src, field_order)
            src[, reference_object := list(lapply(reference_object_id, private$IdfObject$new))]

            res <- list(reference_from = src, reference_by = targ)
            data.table::setattr(res, "class", c("IdfObjectRefMap", "list"))
            res
            # }}}
        },

        value_table = function (all = FALSE, unit = TRUE,
                                wide = FALSE, string_value = TRUE) {
            # return object data in spreadsheet format
            # {{{
            tbl <- private$value_tbl(with_field = TRUE, all = all)
            in_ip <- private$m_options$view_in_ip
            # change to IP numbers and names if necessary
            tbl <- update_value_num(tbl, private$m_options$num_digits, in_ip)

            if (!unit) fld_nm <- "field_name"

            if (in_ip) {
                if (unit) fld_nm <- "full_ipname"
                cols <- c("field_order", fld_nm, "value", "value_ipnum")
            } else {
                if (unit) fld_nm <- "full_name"
                cols <- c("field_order", fld_nm, "value", "value_num")
            }

            tbl_sub <- tbl[, .SD, .SDcols = cols]
            if (in_ip) {
                data.table::setnames(tbl_sub,
                    c("full_ipname", "value_ipnum"),
                    c("full_name", "value_num"))
            }

            if (!string_value) {
                tbl_sub[, value := list(as.list(value))]
                # NOTE: if a numeric field is set to "autosize", then
                # inconsistency will be introduced into return value classes, as
                # values will be return as a string "autosize" instead of a
                # number.
                tbl_sub[!is.na(value_num), value := list(as.list(value_num))]
            }

            tbl_sub[, value_num := NULL]

            data.table::setnames(tbl_sub, c("order", "name", "value"))

            if (wide) {
                tbl_wide <- data.table::dcast(tbl_sub, . ~ name,
                    value.var = c("value"))[, `.` := NULL]
                data.table::setcolorder(tbl_wide, tbl_sub[["name"]])
                if (!string_value) {
                    tbl_wide <- tbl_wide[, lapply(.SD, unlist)]
                }

                return(tbl_wide[])
            } else {
                return(tbl_sub[])
            }
            # }}}
        },

        is_valid = function () {
            # return TRUE if there are no errors after `$check()`
            # {{{
            i_is_valid(private)
            # }}}
        },

        has_reference_by = function () {
            # return TRUE if there are fields referenced by other objects
            # {{{
            not_empty(private$field_ref_by())
            # }}}
        },

        has_reference_from = function () {
            # return TRUE if there are fields referencing other objects
            # {{{
            not_empty(private$field_ref_from())
            # }}}
        },

        has_reference = function () {
            # return TRUE if fields in this objects referencing or referenced by
            # other objects
            # {{{
            any(self$has_by_reference(), self$has_from_reference())
            # }}}
        },

        string = function (comment = TRUE, leading = 4L, sep_at = 29L) {
            # return IDF format output
            # {{{
            cls <- paste0(private$m_class_name, ",")
            fld <- format_field(private$value_tbl(with_field = TRUE),
                                leading = leading,
                                in_ip = private$m_options$view_in_ip,
                                sep_at = sep_at, index = FALSE)
            res <- c(cls, fld)
            if (comment) {
                cmt <- format_comment(private$comment_tbl())
                res <- c(cmt, res)
            }

            return(res)
            # }}}
        },

        print = function (comment = TRUE) {
            # TODO: add the index of this object in the class and required mark
            # {{{
            if (is.na(private$m_object_name)) {
                cli::cat_line("<<[ID:", private$m_object_id, "]>> ",
                    private$m_class_name)
            } else {
                cli::cat_line("<<[ID:", private$m_object_id, "] ",
                    backtick(private$m_object_name), ">> ",
                    private$m_class_name)
            }

            value_tbl <- private$value_tbl(with_field = TRUE)
            if (is_empty(value_tbl)) {
                cli::cat_line(" * Object has been deleted * ")
                return(invisible())
            }

            # comment
            if (comment) {
                if (not_empty(private$comment_tbl())) {
                    cli::cat_rule(center = "* COMMENTS *", line = 1)
                    cli::cat_line(format_comment(private$comment_tbl()))
                }
            }

            # value
            fld <- format_field(value_tbl, leading = 1L,
                                in_ip = private$m_options$view_in_ip,
                                sep_at = 20L, index = TRUE, blank = TRUE)

            # remove class line
            cli::cat_rule(center = "* FIELDS *")
            cli::cat_line(fld)
            cli::cat_rule()
            # }}}
        }
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS
        # {{{
        # shared data from parent Idf object
        m_uuid = NULL,
        m_version = NULL,
        m_idf_tbl = NULL,
        m_options = NULL,
        m_log = NULL,

        m_object_id = integer(),
        m_object_name = character(),
        m_validate = list(),
        m_temp = NULL,

        IdfObject = NULL,
        IddObject = NULL,
        # }}}

        # PRIVATE FUNCTIONS
        # {{{
        object_tbl = function () {
            # return object tbl
            # {{{
            private$m_idf_tbl$object[object_id == private$m_object_id][
                private$class_tbl(), on = "class_id", nomatch = 0L]
            # }}}
        },

        value_tbl = function (index = NULL, name = NULL, with_field = FALSE, all = FALSE) {
            # return value tbl
            # {{{
            out_tbl <- private$m_idf_tbl$value[object_id == private$m_object_id]
            if (with_field) {
                if (all) {
                    out_tbl[private$field_tbl(index, name), on = "field_id"]
                } else {
                    out_tbl[private$field_tbl(index, name), on = "field_id", nomatch = 0L]
                }
            } else {
                out_tbl
            }
            # }}}
        },

        comment_tbl = function () {
            # return value tbl
            # {{{
            private$m_idf_tbl$comment[object_id == private$m_object_id]
            # }}}
        },

        field_ref_by = function (with_field = FALSE) {
            # return fields referenced by other objects
            # {{{
            ref_ed <- private$m_idf_tbl$value_reference[
                private$value_tbl(with_field = with_field),
                on = c(reference_value_id = "value_id"), nomatch = 0L]
            if (with_field) {
                ref_ed <- ref_ed[, list(object_id, reference_value_id, value,
                class_id, class_name, field_order, full_name, full_ipname, value_id)]
            }
            data.table::setnames(ref_ed,
                c("reference_value_id", "value_id"), c("value_id", "target_value_id"))
            return(ref_ed)
            # }}}
        },

        field_ref_from = function (with_field = FALSE) {
            # return fields referencing other objects
            # {{{
            ref <- private$m_idf_tbl$value_reference[
                private$value_tbl(with_field = with_field),
                on = "value_id", nomatch = 0L]
            if (with_field) {
                ref <- ref[, list(object_id, value_id, value, class_id,
                class_name, field_order, full_name, full_ipname, reference_value_id)]
            }
            return(ref)
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
        }
        # }}}
    )
)
# }}}

#' @export
# [.IdfObject {{{
'[.IdfObject' <- function(x, i, j, ...) {
    if (!missing(j)) {
        stop("incorrect number of dimensions.", call. = FALSE)
    }
    if (missing(i)) {
        x
    } else {
        if (is.character(i)) {
            .subset2(x, "get_value")(name = i)
        } else if (is.numeric(i)){
            .subset2(x, "get_value")(index = i)
        } else {
            stop("i should be an integer or string.", call. = FALSE)
        }
    }
}
# }}}

#' @export
# [<-.IdfObject {{{
'[<-.IdfObject' <- function(x, i, j, ..., value) {
    if (!missing(j)) {
        stop("incorrect number of dimensions.", call. = FALSE)
    }
    if (missing(i)) {
        stop("Missing field index or name.", call. = FALSE)
    } else {
        if (is.character(i)) {
            nms <- i
        } else if (is.numeric(i)){
            nms <- .subset2(x, "field_name")(index = i)
        } else {
            stop("i should either integers or strings.", call. = FALSE)
        }
    }
    names(value) <- nms
    .subset2(x, "set_value")(value)
}
# }}}

#' @export
# [[.IdfObject {{{
'[[.IdfObject' <- function(x, i, j, ..., drop = FALSE) {
    if (!missing(j)) {
        stop("incorrect number of dimensions.", call. = FALSE)
    }
    if (missing(i)) {
        stop("Missing field index or name.", call. = FALSE)
    } else {
        if (length(i) == 1L) {
            if (is.character(i)) {
                .subset2(x, "get_value")(name = i)[[1]]
            } else if (is.numeric(i)){
                .subset2(x, "get_value")(index = i)[[1]]
            } else {
                stop("i should either integers or strings.", call. = FALSE)
            }
        } else {
            stop("subscript out of bounds.", call. = FALSE)
        }
    }
}
# }}}

#' @export
# [[<-.IdfObject {{{
'[[<-.IdfObject' <- function(x, i, j, ..., value) {
    if (!missing(j)) {
        stop("incorrect number of dimensions.", call. = FALSE)
    }
    if (missing(i)) {
        stop("Missing field number or name.", call. = FALSE)
    } else {
        if (length(i) == 1L) {
            if (is.character(i)) {
                nm <- i
            } else if (is.numeric(i)){
                nm <- .subset2(x, "field_name")(i)
            } else {
                stop("i should either integers or strings.", call. = FALSE)
            }
            names(value) <- nm
            value <- as.list(value)
            .subset2(x, "set_value")(value)
        } else {
            stop("subscript out of bounds.", call. = FALSE)
        }
    }
}
# }}}
