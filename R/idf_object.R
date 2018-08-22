#' @importFrom R6 R6Class
NULL

#' EnergyPlus IDF object
#'
#' `IdfObject` is an abstraction of a single object in an `Idf`. It provides
#' more detail methods to modify objects. `IdfObject` can only be created from
#' the parent `Idf` object, using `$object`, `$object_in_class()` and
#' `$search_object()` or equivalent. This is because that initialization of an
#' `IdfObject` needs some shared data from parent `Idf` object.
#'
#' @section Usage:
#' \preformatted{
#' idfobj$id()
#' idfobj$name()
#' idfobj$definition()
#' idfobj$get_comment()
#' idfobj$set_comment(comment, append = TRUE, width = 0L)
#' idfobj$get_value(which = NULL, all = NULL, simplify = FALSE)
#' idfobj$set_value(..., defaults = TRUE)
#' idfobj$FieldName
#' idfobj[[Field]]
#' idfobj$FieldName <- Value
#' idfobj[[Field]] <- Value
#' idfobj$possible_value(which = NULL)
#' idfobj$validate()
#' idfobj$is_valid()
#' idfobj$ref_from_object()
#' idfobj$ref_by_object()
#' idfobj$has_ref_by()
#' idfobj$has_ref_from()
#' idfobj$has_ref()
#' idfobj$table(all = FALSE, unit = TRUE, wide = FALSE, string_value = TRUE, in_ip = eplusr_option("view_in_ip"))
#' idfobj$string(comment = TRUE, leading = 4L, sep_at = 29L)
#' idfobj$print(comment = TRUE, auto_sep = FALSE)
#' print(iddobj)
#' }
#'
#' @section Basic Info:
#'
#' ```
#' idfobj$id()
#' idfobj$name()
#' idfobj$group_name()
#' idfobj$class_name()
#' ```
#'
#' `$id()` returns the object ID.
#'
#' `$name()` returns the object name. If the class does not have name
#'     attribute, then `NA` will returned.
#'
#' `$group_name()` returns the group name of this object belongs to.
#'
#' `$class_name()` returns the class name of this object belongs to.
#'
#' @section Definition:
#' ```
#' idfobj$definition()
#' ```
#'
#' `$definition()` returns the definition, i.e. the `IddObject`, of current
#'     class. For details of `IddObject`, please see [IddObject] class.
#'
#' @section Comment:
#' ```
#' idfobj$get_comment()
#' idfobj$set_comment(comment, append = TRUE, width = 0L)
#' ```
#'
#' `$get_comment()` returns the comments of current object.
#'
#' `$set_comment()` sets comments of current object.
#'
#' **Arguments**
#'
#' * `comment`: A character vector. If `NULL`, all comments will be deleted.
#' * `append`: If `TRUE`, comment will be appended to existing comments. If
#'     `FALSE`, comment will be prepended to existing currents. If `NULL`,
#'     existing comments will be deleted before adding new comments. Default:
#'     `FALSE`.
#' * `width`: An integer of character number to indicate where to break long
#'   comment lines. If `0`, no breaking will be made. Default: `0`.
#'
#' @section Value:
#' \preformatted{
#' idfobj$get_value(which = NULL, all = FALSE, simplify = FALSE)
#' idfobj$set_value(..., default = TRUE)
#' idfobj$FieldName
#' idfobj[[Field]]
#' idfobj$FieldName <- Value
#' idfobj[[Field]] <- Value
#' idfobj$possible_value(which = NULL)
#' }
#'
#' `$get_value()` returns a named list containing values of specified fields.
#'     If simplify is `FALSE`, then all values will be converted into character
#'     and the converted character vector will be returned. Note that the field
#'     names will be converted into valid R names, i.e. all characters other
#'     than letters and numbers will be replaced by underscore `"_"`
#'     ("underscore-style").
#'
#' `$set_value()` sets values of current object. Field values should be given in
#' following either pattern below:
#'
#' * directly list all field values with no name. The values will be assigned to
#'   fields according to the appearance order
#' * give both field names *without units* and values in pair, e.g. `` Name =
#'   "Test", `Begin Month` = 1 ``. You can find all valid field names using
#'   `$definition()$field_name()`. Field names can also be given in
#'   underscore-style, e.g. `Name = "Test", begin_month = 1` (NOTE: matching is
#'   case-insensitive).
#'
#' eplusr also provides custom S3 method of `$`, \code{[[} and also `$<-` and
#' \code{[[<-} to make it more convenient to get and set a single value of an
#' `IdfObject`. Basically, `idfobj$FieldName` and \code{idfobj[[Field} is equivalent to \code{idfobj$get_value(Field)[[1]]};
#' `idfobj$FieldName <- Value` and \code{idfobj[[Field]] <- Value} is equivalent
#' to `idfobj$set_value(Field = Value)`,  where `FieldName` is a single valid
#' field name and `Field` is a single valid field index or name.
#'
#' `$possible_value()` return all possible values for specified fields,
#'      including auto-value (`autosize` and `autocalculate`), default value,
#'      value range, choices and references. Underneath, it returns a data.table
#'      with custom printing method. It is basically the same as
#'      `$field_possible()` in [IddObject] class.
#'
#' **Arguments**
#'
#' * `which`: An integer vector of field indexes or a character vector of field
#'     names. Field names can be given in "lower-style", e.g. `"Thermal
#'     Resistance"` can be given as `"thermal_resistance"`.
#' * `all`: If `TRUE`, values of all fields, including empty fields will be
#'     returned as well. Default: `FALSE`
#' * `simplify`: If `TRUE`, values of fields will be converted into characters
#'     and the converted character vector will be returned.
#' * `...`: Values to set. Field names of value can be given. If not named, the
#'     input values will be set to fields according to their order of
#'     appearance.
#' * `default`: If `TRUE`, all empty fields will be filled with their default
#'     values if possible.
#' * `FieldName`: A single length character vector of one valid field name where
#'     all characters except letters and numbers are replaced by underscores.
#' * `Field`: A single length character vector of one valid field name or a
#'     single length integer vector of one valid field index. Same as above,
#'     field names should be given in a style where all characters except
#'     letters and numbers are replaced by underscores.
#' * `Value`: A single length vector of value to set.
#'
#' @section Validation:
#'
#' ```
#' idfobj$validate()
#' idfobj$is_valid()
#' ```
#'
#' `$validate()` will check if there are errors in current object under different
#'     strictness level.
#'
#' `$is_valid()` will check if there are no errors in current object under
#'     different strictness level.
#'
#' The strictness level can be changed using [eplusr_option()]. Default is
#'     `"final". `There are three different validate levels, i.e. `"none"`,
#'     `"draft"` and `"final"`:
#'
#'   * For `"none"`, none validation will be done;
#'   * For `"draft"`, checking of invalid autosize, autocalculate, character,
#'     numeric, integer, and choice field values will be done;
#'   * For `"final"`, besides above, checking of incomplete extensible groups,
#'     missing required objects, duplicated unique objects, object name
#'     conflicts, missing required fields and invalid field value reference will
#'     also be done.
#'
#' For details about the underlying structure of returned value of
#' `$validate()`, please `$validate()` in [Idf] class.
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
#' `$ref_from_object()` will return other objects that current object references
#'     from.
#'
#' `$ref_by_object()` will return other objects that reference current object.
#'
#' `$has_ref_from()` and `$has_ref_by` will return `TRUE` if current object has
#'     referenced from other objects or has been referenced by other objects,
#'     respectively.
#'
#' `$has_ref()` will return `TRUE` if current object has either referenced from
#'     other objects or has been referenced by other objects.
#'
#' @section Data Extraction:
#'
#' ```
#' idfobj$table(all = FALSE, unit = TRUE, wide = FALSE, string_value = TRUE, in_ip = eplusr_option("view_in_ip"))
#' ```
#'
#' `$table()` will return a data.table that contains all data of current object.
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
#' `$string()` will return the text format of current object.
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
#' `$print()` prints the IdfObject. Basically, the print output can be divided
#'     into three parts:
#'
#'     * OBJECT: object id and name (if applicable) and IDD class name of
#'     * COMMENTS: object comments
#'     * VALUES: fields and values of current IDD class. Required fields are
#'       marked with bullet marks. Only the minimum fields are printed. E.g.,
#'       the last printed field is either the last required field or the last
#'       non-empty field.
#'
#' **Arguments**
#'
#' * `comment`: If `FALSE`, all comments will not be included.
#' * `auto_sep`: If `TRUE`, values and field names will be separate at the
#'     largest character length of values. Default: `FALSE`.
#'
#' @importFrom R6 R6Class
#' @examples
#' # read an IDF file
#' idf <- read_idf(system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr"),
#'     idd = use_idd(8.8, download = "auto"))
#'
#' # get the IdfObject of material named "C5 - 4 IN HW CONCRETE"
#' mat <- idf$Material$C5_4_IN_HW_CONCRETE
#'
#' # get object ID
#' mat$id()
#'
#' # get object name
#' mat$name()
#'
#' # NA will be returned if the class does not have name attribute. For example,
#' # "Version" class
#' idf$Version[[1]]$name()
#'
#' # get underlying IddObject of current class
#' mat$definition()
#'
#' # get object comments
#' mat$get_comment()
#'
#' # add new object comments
#' mat$set_comment(c("This is a material named `WD01`", "This object has an ID of 47"))
#' mat$get_comment()
#'
#' # append new comments
#' mat$set_comment("This is an appended comment")
#' mat$get_comment()
#'
#' # prepend new comments
#' mat$set_comment("This is a prepended comment", append = FALSE)
#' mat$get_comment()
#'
#' # wrap long comments
#' mat$set_comment("This is a very long comment that is needed to be wrapped.", width = 30)
#' mat$get_comment()
#'
#' # delete old comments and add new one
#' mat$set_comment("This is the only comment", append = NULL)
#' mat$get_comment()
#'
#' # delete all comments
#' mat$set_comment(NULL)
#' mat$get_comment()
#'
#' # get all existing field values
#' str(mat$get_value())
#'
#' # get values of field 1, 3, 5
#' str(mat$get_value(c(1, 3, 5)))
#'
#' # get character format values instead of a named list
#' mat$get_value(c(1, 3, 5), simplify = TRUE)
#'
#' # get values of all field even those that are not set
#' str(idf$Zone$ZONE_ONE$get_value())
#'
#' str(idf$Zone$ZONE_ONE$get_value(all = TRUE))
#'
#' # get field values using shortcuts
#' mat$Roughness
#' mat[["Specific_Heat"]]
#' mat[c(1,2)]
#' mat[c("Name", "Density")]
#'
#' # set field values
#' mat$set_value(name = "new_name", Thickness = 0.02)
#' mat[c("Name", "Thickness")]
#'
#' # When `default` argument is set to TRUE and input field values are empty, i.e.
#' # NA and NULL, the field values will be reset to defaults.
#' mat[c("Thermal Absorptance", "Solar Absorptance")]
#'
#' mat$set_value(visible_absorptance = NA, Solar_Absorptance = NA, default = TRUE)
#' mat[c("Visible Absorptance", "Solar Absorptance")]
#'
#' # set field values using shortcuts
#' mat$Name <- "another_name"
#' mat$Name
#' mat[["Thickness"]] <- 0.019
#' mat$Thickness
#'
#' # check validate
#' mat$validate()
#' mat$is_valid()
#'
#' # if we set density to a negative number
#' mat$definition()$field_range("Density")
#' eplusr_option(validate_level = "none") # have to set validate to "none" to do so
#' mat$Density <- -1
#' eplusr_option(validate_level = "final") # change back to "final" validate level
#' mat$is_valid()
#' # get other objects that this object refereces
#' mat$ref_from_object() # not referencing other objects
#' mat$has_ref_from()
#'
#'
#' # get other objects that reference this object
#' mat$ref_by_object() # referenced by construction "FLOOR"
#' names(mat$ref_by_object())
#'
#' mat$has_ref_by()
#'
#' # check if having any referenced objects or is referenced by other objects
#' mat$has_ref()
#'
#' # get all object data in a data.table format without field units
#' str(mat$table(unit = FALSE))
#'
#' # get all object data in a data.table format where all field values are put in a
#' # list column and field names without unit
#' str(mat$table(string_value = FALSE, unit = FALSE))
#'
#' # get all object data in a data.table format where all field values are put in a
#' # list column and all values are converted into IP units
#' str(mat$table(string_value = FALSE, in_ip = TRUE))
#'
#' # get all object data in a data.table format, including tailing empty fields
#' str(idf$Zone$ZONE_ONE$table(all = TRUE))
#'
#' # get all object data in a data.table format where each field becomes a column
#' str(mat$table(wide = TRUE))
#'
#' # get string format object
#' mat$string()
#'
#' # get string format of object, and decrease the space between field values and
#' # field names
#' mat$string(sep_at = 15)
#'
#' # get string format of object, and decrease the leading space of field values
#' mat$string(leading = 0)
#'
#' # print the object without comment
#' mat$print(comment = FALSE)
#'
#' # print the object, and auto separate field values and field names at the
#' # largetst character length of field values
#' mat$print(auto_sep = TRUE)
#' @docType class
#' @name IdfObject
#' @seealso [Idf] class
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

        group_name = function ()
            i_from_group(self, private, private$m_class_id),

        class_name = function ()
            i_class_tbl_from_which(self, private, private$m_class_id)$class_name,

        definition = function ()
            i_definition(self, private, i_class_name(self, private, private$m_class_id))[[1]],

        get_comment = function ()
            i_comment_tbl_from_which(self, private, private$m_object_id, nomatch = 0L)$comment,

        set_comment = function (comment, append = TRUE, width = 0L)
            i_idfobj_set_comment(self, private, private$m_object_id, comment, append, width),

        get_value = function (which = NULL, all = FALSE, simplify = FALSE)
            i_idfobj_get_value(self, private, private$m_object_id, which, all, simplify),

        set_value = function (..., default = TRUE)
            i_idfobj_set_value(self, private, private$m_object_id, ..., default = default),

        possible_value = function (which = NULL)
            i_idfobj_possible_values(self, private, private$m_object_id, which),

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
    .subset2(x, "get_value")(i)[j]
}
# }}}

#' @export
# $.IdfObject {{{
'$.IdfObject' <- function (x, name) {
    if (is_string(name)) {
        funs <- setdiff(ls(x), "initialize")
        if (name %in% funs) {
            NextMethod()
        } else {
            all_nm <- i_underscore_name(.subset2(.subset2(x, "definition")(), "field_name")())
            m <- match(i_underscore_name(name), all_nm)
            if (!is.na(m)) {
                .subset2(x, "get_value")(m)[[1]]
            } else {
                NextMethod()
            }
        }
    } else {
        NextMethod()
    }
}
# }}}

#' @export
# [[.IdfObject {{{
'[[.IdfObject' <- function(x, i) {
    if (is_string(i)) {
        funs <- setdiff(ls(x), "initialize")
        if (i %in% funs) {
            NextMethod()
        } else {
            all_nm <- i_underscore_name(.subset2(.subset2(x, "definition")(), "field_name")())
            m <- match(i_underscore_name(i), all_nm)
            if (!is.na(m)) {
                .subset2(x, "get_value")(m)[[1]]
            } else {
                NextMethod()
            }
        }
    } else if (is_integerish(i)) {
        .subset2(x, "get_value")(i)[[1]]
    } else {
        NextMethod()
    }
}
# }}}

#' @export
# $<-.IdfObject {{{
'$<-.IdfObject' <- function (x, name, value) {
    if (is_string(name)) {
        funs <- setdiff(ls(x), "initialize")
        if (name %in% funs) {
            NextMethod()
        } else {
            all_nm <- i_underscore_name(.subset2(.subset2(x, "definition")(), "field_name")())
            m <- match(i_underscore_name(name), all_nm)
            if (!is.na(m)) {
                value <- as.list(value)
                names(value) <- all_nm[m]
                .subset2(x, "set_value")(value)
            } else {
                NextMethod()
            }
        }
    } else {
        NextMethod()
    }
}
# }}}

#' @export
# [[<-.IdfObject {{{
'[[<-.IdfObject' <- function(x, i, value) {
    if (is_string(i)) {
        funs <- setdiff(ls(x), "initialize")
        if (i %in% funs) {
            NextMethod()
        } else {
            all_nm <- i_underscore_name(.subset2(.subset2(x, "definition")(), "field_name")())
            m <- match(i_underscore_name(i), all_nm)
            if (!is.na(m)) {
                value <- as.list(value)
                names(value) <- all_nm[m]
                .subset2(x, "set_value")(value)
            } else {
                NextMethod()
            }
        }
    } else if (is_integerish(i)) {
        nm <- .subset2(.subset2(x, "definition")(), "field_name")(i)
        value <- as.list(value)
        names(value) <- nm
        .subset2(x, "set_value")(value)
    } else {
        NextMethod()
    }
}
# }}}
