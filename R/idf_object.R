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
    public = list(
        # INITIALIZE {{{
        initialize = function (object_id) {
            # check shared data
            var_shared <- c("m_uuid", "m_version", "m_idd_tbl", "m_idf_tbl", "m_log")
            is_null <- vapply(var_shared, function (var) is.null(private[[var]]), logical(1))
            if (any(is_null))
                stop("IdfObject can be created only after a parent Idf object ",
                    "has been initialized.", call. = FALSE)

            assert_that(is_count(object_id))

            obj_tbl <- i_object_tbl_from_which(self, private, object_id)
            private$m_object_id <- obj_tbl$object_id
            private$m_class_id <- obj_tbl$class_id
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        id = function ()
            i_id(self, private),

        name = function ()
            i_object_tbl_from_which(self, private, private$m_object_id, class = FALSE)$object_name,

        get_comment = function ()
            i_comment_tbl_from_which(self, private, private$m_object_id, nomatch = 0L)$comment,

        set_comment = function (comment, append = TRUE, width = 0L)
            i_idfobj_set_comment(self, private, private$m_object_id, comment, append, width),

        get_value = function (which = NULL, all = FALSE)
            i_idfobj_get_value(self, private, private$m_object_id, which, all),

        set_value = function (..., default = TRUE)
            i_idfobj_set_value(self, private, private$m_object_id, ..., default = default),

        validate = function ()
            i_validate_idfobject(self, private, private$m_object_id),

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

        table = function (all = FALSE, unit = TRUE, wide = FALSE, string_value = TRUE, in_ip = getOption("eplusr.view_in_ip"))
            i_idfobj_value_table(self, private, private$m_object_id, all, unit, wide, string_value, in_ip),

        is_valid = function ()
            i_is_valid_idfobject(self, private, private$m_object_id),

        ref_from_object = function ()
            i_idfobj_ref_from(self, private, private$m_object_id),

        ref_by_object = function ()
            i_idfobj_ref_by(self, private, private$m_object_id),

        has_ref_by = function ()
            i_idfobj_has_ref_by(self, private, private$m_object_id),

        has_ref_from = function ()
            i_idfobj_has_ref_from(self, private, private$m_object_id),

        has_ref = function ()
            i_idfobj_has_ref_from(self, private, private$m_object_id) ||
            i_idfobj_has_ref_by(self, private, private$m_object_id),

        string = function (comment = TRUE, leading = 4L, sep_at = 29L)
            i_object_string(self, private, private$m_object_id, header = FALSE,
                format = "new_top", comment = comment, leading = leading,
                sep_at = sep_at, index = FALSE),

        print = function (comment = TRUE, ...)
            i_print_idfobj(self, private, private$m_object_id, comment, ...)
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        # shared data from parent Idf object
        m_uuid = NULL,
        m_version = NULL,
        m_idf_tbl = NULL,
        m_idd_tbl = NULL,
        m_log = NULL,

        m_object_id = NULL,
        m_class_id = NULL,

        # self reference
        m_idfobj_generator = NULL,
        m_iddobj_generator = NULL
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
