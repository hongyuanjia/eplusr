#' EnergyPlus IDF object
#'
#' `IdfObject` is an abstraction of a single object in an `Idf`. It provides
#' more detail methods to modify objects. `IdfObject` can only be created from
#' the parent `Idf` object, using `$object`, `$object_in_class` and
#' `$search_object`. This is because that initialization of an `IdfObject` needs
#' some shared data from parent `Idf` object.
#'
#' @section Usage:
#' ```
#' # basic info
#' idfobj$id()
#' idfobj$name()
#'
#' # definition
#' idfobj$definition()
#'
#' # comment
#' idfobj$get_comment()
#' idfobj$set_comment(comment, append = TRUE, width = 0L)
#'
#' # value
#' idfobj$get_value(which = NULL, all = NULL)
#' idfobj$set_value(..., defaults = TRUE)
#'
#' # validation
#' idfobj$validate()
#' idfobj$is_valid()
#'
#' # object cross reference
#' idfobj$ref_from_object()
#' idfobj$ref_by_object()
#' idfobj$has_ref_by()
#' idfobj$has_ref_from()
#' idfobj$has_ref()
#'
#' # data extraction
#' idfobj$table(all = FALSE, unit = TRUE, wide = FALSE, string_value = TRUE, in_ip = eplusr_option("view_in_ip"))
#'
#' # formatting
#' idfobj$string(comment = TRUE, leading = 4L, sep_at = 29L)
#'
#' # print
#' idfobj$print(comment = TRUE, auto_sep = FALSE)
#' print(iddobj)
#' ```
#'
#' @section Basic Info:
#'
#' ```
#' idfobj$id()
#' idfobj$name()
#' ```
#'
#' `$id` will return the object ID.
#'
#' `$name` will return the object name. If the class does not have name
#'     attribute, then `NA` will returned.
#'
#' @section Definition:
#' ```
#' idfobj$definition()
#' ```
#'
#' `$definition` will return the definition, i.e. the `IddObject`, of current
#'     class. For details of `IddObject`, please see XXX.
#'
#' @section Comment:
#' ```
#' idfobj$get_comment()
#' idfobj$set_comment(comment, append = TRUE, width = 0L)
#' ```
#'
#' `$get_comment` will return the comments of current object.
#'
#' `$set_comment` will set the comments of current object.
#'
#' **Arguments**
#'
#' * `comment`: A character vector.
#' * `append`: If `TRUE`, comment will be appended to existing comments. If
#'     `FALSE`, comment will be prepended to existing currents. If `NULL`,
#'     existing comments will be deleted. Default: `FALSE`
#' * `width`: An integer to indicate where to break long comment lines. If `0`,
#'     no breaking will be made.
#'
#' @section Value:
#' ```
#' idfobj$get_value(which = NULL, all = FALSE)
#' idfobj$set_value(..., default = TRUE)
#' ```
#'
#' `$get_value` will return a named list containing values of specified fields.
#'
#' `$set_comment` will set the comments of current object.
#'
#' **Arguments**
#'
#' * `which`: An integer vector of field indexes or a character vector of field
#'     names. Field names can be given in "lower-style", e.g. `"Thermal
#'     Resistance"` can be given as `"thermal_resistance"`.
#' * `all`: If `TRUE`, values of all fields, including empty fields will be
#'     returned as well. Default: `FALSE`
#' * `...`: Values to set. Field names of value can be given. If not named, the
#'     input values will be set to fields according to their order of
#'     appearance.
#' * `default`: If `TRUE`, all empty fields will be filled with their default
#'     values if possible.
#'
#' @section Validation:
#'
#' ```
#' idfobj$validate()
#' idfobj$is_valid()
#' ```
#'
#' `$validate` will check if there are errors in current object under different
#'     strictness level.
#'
#' `$is_valid` will check if there are no errors in current object under
#'     different strictness level.
#'
#' The strictness level can be changed using [eplusr_option()]. Default is
#'     `"final". `There are three different validate levels, i.e. `"none"`,
#'     `"draft"` and `"final"`:
#'
#'   * For `"none"`, none validation will be done;
#'   * For `"draft"`, checking of invalid autosize, autocalculate, numeric,
#'     integer, and choice field values will be done;
#'   * For `"final"`, besides above, checking of missing required objects,
#'     duplicated unique objects, object name conflicts, missing required
#'     fields and invalid field value reference will also be done.
#'
#' @section Cross Reference:
#'
#' ```
#' idfobj$ref_from_object()
#' idfobj$ref_by_object()
#' idfobj$has_ref_from()
#' idfobj$has_ref_by()
#' idfobj$has_ref()
#' ```
#'
#' `$ref_from_object` will return other objects that current object references
#'     from.
#'
#' `$ref_by_object` will return other objects that reference current object.
#'
#' `$has_ref_from` and `$has_ref_by` will return `TRUE` if current object has
#'     referenced from other objects or has been referenced by other objects,
#'     respectively.
#'
#' `$has_ref` will return `TRUE` if current object has either referenced from
#'     other objects or has been referenced by other objects.
#'
#' @section Data Extraction:
#'
#' ```
#' idfobj$table(all = FALSE, unit = TRUE, wide = FALSE, string_value = TRUE, in_ip = eplusr_option("view_in_ip"))
#' ```
#'
#' `$table` will return a data.table that contains all data of current object.
#'
#' **Arguments**
#' * `all`: If `TRUE`, values of all fields, including empty fields will be
#'     returned as well. Default: `FALSE`
#' * `unit`: If `TRUE`, field names with units will be returned. Default:
#'     `TRUE`.
#' * `wide`: If `TRUE`, a wide table will be returned. Default: `FALSE`.
#' * `string_value`: If `TRUE`, all field values will be returned as character.
#'     Default: `TRUE`
#' * `in_ip`: If `TRUE`, IP units and values will be returned. Default: the
#'     value of `eplusr_option("view_in_ip")`.
#'
#' @section Formatting:
#'
#' ```
#' idfobj$string(comment = TRUE, leading = 4L, sep_at = 29L)
#' ```
#'
#' `$string` will return the text format of current object.
#'
#' **Arguments**
#'
#' * `comment`: If `FALSE`, all comments will not be included.
#' * `leading`: An integer to indicate the number of spaces before each fields.
#'     Default: `4`.
#' * `sep_at`: An integer to indicate the character width where to separate
#'     values and field names.  Default: `29`.
#'
#' @section Print:
#'
#' ```
#' idfobj$print(comment = TRUE, auto_sep = FALSE)
#' print(idfobj)
#' ```
#'
#' **Arguments**
#'
#' * `comment`: If `FALSE`, all comments will not be included.
#' * `auto_sep`: If `TRUE`, values and field names will be separate at the
#'     largest character length of values. Default: `FALSE`.
#'
#' @importFrom R6 R6Class
#' @docType class
#' @name idf_object
#' @author Hongyuan Jia
NULL

# IdfObject {{{
IdfObject <- R6::R6Class(classname = "IdfObject",
    public = list(
        # INITIALIZE {{{
        initialize = function (object_id) {
            # check shared data
            var_shared <- c("m_version", "m_idd_tbl", "m_idf_tbl", "m_log")
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

        definition = function ()
            i_iddobject(self, private, i_class_name(self, private, private$m_class_id))[[1]],

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

        table = function (all = FALSE, unit = TRUE, wide = FALSE, string_value = TRUE, in_ip = eplusr_option("view_in_ip"))
            i_idfobj_value_table(self, private, private$m_object_id, all, unit, wide, string_value, in_ip),

        string = function (comment = TRUE, leading = 4L, sep_at = 29L)
            i_object_string(self, private, private$m_object_id, header = FALSE,
                format = "new_top", comment = comment, leading = leading,
                sep_at = sep_at, index = FALSE),

        print = function (comment = TRUE, auto_sep = FALSE)
            i_print_idfobj(self, private, private$m_object_id, comment, auto_sep)
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
