#' @importFrom R6 R6Class
#' @importFrom checkmate assert_count assert_vector assert_integerish
NULL

#' EnergyPlus IDD object
#'
#' `IddObject` is an abstraction of a single object in an [Idd] object. It
#' provides more detail methods to query field properties. `IddObject` can only
#' be created from the parent [Idd] object, using `$object()`,
#' `$object_in_group()` and other equivalent. This is because that
#' initialization of an `IddObject` needs some shared data from parent [Idd]
#' object.
#'
#' There are lots of properties for every class and field. For details on the
#' meaning of each property, please see the heading comments in the
#' `Energy+.idd` file in the EnergyPlus installation path.
#'
#' @docType class
#' @name IddObject
#' @seealso [Idd] Class
#' @author Hongyuan Jia
NULL

#' Create an `IddObject` object.
#'
#' `idd_object()` takes a parent `Idd` object, a class name, and returns a
#' corresponding [IddObject]. For details, see [IddObject].
#'
#' @param parent An [Idd] object or a valid input for [use_idd()].
#' @param class A valid class name (a string).
#' @return An [IddObject] object.
#' @export
#' @examples
#' \dontrun{
#' idd <- use_idd(8.8, download = "auto")
#'
#' # get an IddObject using class name
#' idd_object(idd, "Material")
#' idd_object(8.8, "Material")
#' }
#'
# idd_object {{{
idd_object <- function (parent, class) {
    IddObject$new(class, parent)
}
# }}}

#' @export
# IddObject {{{
IddObject <- R6::R6Class(classname = "IddObject", cloneable = FALSE,

    public = list(
        # INITIALIZE {{{
        #' @description
        #' Create an `IddObject` object
        #'
        #' @details
        #' Note that an `IddObject` can be created from the parent [Idd] object,
        #' using `$object()`, [idd_object] and other equivalent.
        #'
        #' @param class A single integer specifying the class index or a single
        #'        string specifying the class name.
        #' @param parent An [Idd] object or a valid input for [use_idd()].
        #'
        #' @return An `IddObject` object.
        #'
        #' @examples
        #' \dontrun{
        #' surf <- IddObject$new("BuildingSurface:Detailed", use_idd(8.8, download = "auto"))
        #' }
        #'
        initialize = function (class, parent) {
            if (missing(parent)) {
                abort(paste("IddObject can only be created based on a parent Idd object.",
                    "Please give 'parent', which should be either an IDD version or an 'Idd' object."
                ))
            } else {
                private$m_parent <- use_idd(parent)
            }

            assert_valid_type(class, len = 1L)
            private$m_class_id <- get_idd_class(private$idd_env(), class, underscore = TRUE)$class_id
        },
        # }}}

        # META {{{
        # version {{{
        #' @description
        #' Get the version of parent `Idd`
        #'
        #' @details
        #' `$version()` returns the version of parent `Idd` in a
        #' [base::numeric_version()] format. This makes it easy to direction
        #' compare versions of different `IddObject`s, e.g. `iddobj$version() > 8.6` or
        #' `iddobj1$version() > iddobj2$version()`.
        #'
        #' @return A [base::numeric_version()] object.
        #'
        #' @examples
        #' \dontrun{
        #' # get version
        #' surf$version()
        #' }
        #'
        version = function ()
            iddobj_version(self, private),
        # }}}

        # parent {{{
        #' @description
        #' Get parent [Idd]
        #'
        #' @details
        #' `$parent()` returns parent [Idd] object.
        #'
        #' @return A [Idd] object.
        #'
        #' @examples
        #' \dontrun{
        #' surf$parent()
        #' }
        #'
        parent = function ()
            iddobj_parent(self, private),
        # }}}
        # }}}

        # CLASS PROPERTY GETTERS {{{
        # group_name {{{
        #' @description
        #' Get the group name
        #'
        #' @details
        #' `$group_name()` returns the group name of current `IddObject`.
        #'
        #' @return A single string.
        #'
        #' @examples
        #' \dontrun{
        #' surf$group_name()
        #' }
        #'
        group_name = function ()
            iddobj_group_name(self, private),
        # }}}

        # group_index {{{
        #' @description
        #' Get the group index
        #'
        #' @details
        #' `$group_index()` returns the group index of current `IddObject`. A
        #' group index is just an integer indicating its appearance order in the
        #' [Idd].
        #'
        #' @return A single integer.
        #'
        #' @examples
        #' \dontrun{
        #' surf$group_index()
        #' }
        #'
        group_index = function ()
            iddobj_group_index(self, private),
        # }}}

        # class_name {{{
        #' @description
        #' Get the class name of current `IddObject`
        #'
        #' @details
        #' `$class_name()` returns the class name of current `IddObject`.
        #'
        #' @return A single string.
        #'
        #' @examples
        #' \dontrun{
        #' surf$class_name()
        #' }
        #'
        class_name = function ()
            iddobj_class_name(self, private),
        # }}}

        # class_index {{{
        #' @description
        #' Get the class index
        #'
        #' @details
        #' `$class_index()` returns the class index of current `IddObject`. A
        #' class index is just an integer indicating its appearance order in the
        #' [Idd].
        #'
        #' @return A single integer.
        #'
        #' @examples
        #' \dontrun{
        #' surf$class_index()
        #' }
        #'
        class_index = function ()
            iddobj_class_index(self, private),
        # }}}

        # class_foramt {{{
        #' @description
        #' Get the class format
        #'
        #' @details
        #' `$class_format()` returns the format of this IDD class. This format
        #' indicator is currently not used by eplusr.
        #'
        #' @note
        #' Some classes have special format when saved in the IDFEditor with the
        #' special format option enabled. Those special format includes
        #' "singleLine", "vertices", "compactSchedule", "fluidProperties",
        #' "viewFactors" and "spectral". eplusr can handle all those format when
        #' parsing IDF files. However, when saved, all classes are formatted in
        #' standard way.
        #'
        #' @return A single character.
        #'
        #' @examples
        #' \dontrun{
        #' surf$class_format()
        #' }
        #'
        class_format = function ()
            iddobj_class_format(self, private),
        # }}}

        # min_fields {{{
        #' @description
        #' Get the minimum field number of current class
        #'
        #' @details
        #' `$min_fields()` returns the minimum fields required for current class.
        #' If no required, `0` is returned.
        #'
        #' @return A single integer.
        #'
        #' @examples
        #' \dontrun{
        #' surf$min_fields()
        #' }
        #'
        min_fields = function ()
            iddobj_min_fields(self, private),
        # }}}

        # num_fields {{{
        #' @description
        #' Get the total field number of current class
        #'
        #' @details
        #' `$num_fields()` returns current total number of fields in current
        #' class.
        #'
        #' @note
        #' This number may change if the class is extensible and after
        #' `$add_extensible_group()` or `$del_extensible_group()`.
        #'
        #' @return A single integer.
        #'
        #' @examples
        #' \dontrun{
        #' surf$num_fields()
        #' }
        #'
        num_fields = function ()
            iddobj_num_fields(self, private),
        # }}}

        # memo {{{
        #' @description
        #' Get the memo string of current class
        #'
        #' @details
        #' `$memo()` returns memo of current class, usually a brief description
        #' of this class.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' surf$memo()
        #' }
        #'
        memo = function ()
            iddobj_memo(self, private),
        # }}}

        # num_extensible{{{
        #' @description
        #' Get the field number of the extensible group in current class
        #'
        #' @details
        #' `$num_extensible()` returns the field number of the extensible group
        #' in current class.
        #'
        #' An extensible group is a set of fields that should be treated as a
        #' whole, such like the X, Y and Z vertices of a building surfaces. An
        #' extensible group should be added or deleted together.
        #'
        #' If there is no extensible group in current class, `0` is returned.
        #'
        #' @return A single integer.
        #'
        #' @examples
        #' \dontrun{
        #' surf$num_extensible()
        #' }
        #'
        num_extensible = function ()
            iddobj_num_extensible(self, private),
        # }}}

        # first_extensible_index {{{
        #' @description
        #' Get the minimum field number of current class
        #'
        #' @details
        #' `$first_extensible_index()` returns the field index of first
        #' extensible field in current class.
        #'
        #' An extensible group is a set of fields that should be treated as a
        #' whole, such like the X, Y and Z vertices of a building surfaces. An
        #' extensible group should be added or deleted together.
        #'
        #' If there is no extensible group in current class, `0` is returned.
        #'
        #' @return A single integer.
        #'
        #' @examples
        #' \dontrun{
        #' surf$first_extensible_index()
        #' }
        #'
        first_extensible_index = function ()
            iddobj_first_extensible_index(self, private),
        # }}}

        # extensible_group_num{{{
        #' @description
        #' Get the number of extensible groups in current class
        #'
        #' @details
        #' `$extensible_group_num()` returns the number of extensible groups in
        #' current class.
        #'
        #' An extensible group is a set of fields that should be treated as a
        #' whole, such like the X, Y and Z vertices of a building surfaces. An
        #' extensible group should be added or deleted together.
        #'
        #' If there is no extensible group in current class, `0` is returned.
        #'
        #' @return A single integer.
        #'
        #' @examples
        #' \dontrun{
        #' surf$extensible_group_num()
        #' }
        #'
        extensible_group_num = function ()
            iddobj_extensible_group_num(self, private),
        # }}}
        # }}}

        # EXTENSIBLE GROUP {{{
        # add_extensible_group {{{
        #' @description
        #' Add extensible groups in current class
        #'
        #' @details
        #' `$add_extensible_groups()` adds extensible groups in this class.
        #'
        #' An extensible group is a set of fields that should be treated as a
        #' whole, such like the X, Y and Z vertices of a building surfaces. An
        #' extensible group should be added or deleted together.
        #'
        #' An error will be issued if current class contains no extensible
        #' group.
        #'
        #' @param num An integer indicating the number of extensible groups to
        #'        be added.
        #'
        #' @return The modified `IddObject` itself.
        #'
        #' @examples
        #' \dontrun{
        #' # field number before adding
        #' surf$num_fields()
        #' # extensible group number before adding
        #' surf$extensible_group_num()
        #'
        #' # add 2 more extensible groups
        #' surf$add_extensible_group(2)
        #'
        #' # field number after adding
        #' surf$num_fields()
        #' # extensible group number after adding
        #' surf$extensible_group_num()
        #' }
        #'
        add_extensible_group = function (num = 1L)
            iddobj_add_extensible_group(self, private, num),
        # }}}

        # del_extensible_group {{{
        #' @description
        #' Delete extensible groups in current class
        #'
        #' @details
        #' `$del_extensible_groups()` deletes extensible groups in this class.
        #'
        #' An extensible group is a set of fields that should be treated as a
        #' whole, such like the X, Y and Z vertices of a building surfaces. An
        #' extensible group should be added or deleted together.
        #'
        #' An error will be issued if current class contains no extensible
        #' group.
        #'
        #' @param num An integer indicating the number of extensible groups to
        #'        be deleted.
        #'
        #' @return The modified `IddObject` itself.
        #'
        #' @examples
        #' \dontrun{
        #' # field number before deleting
        #' surf$num_fields()
        #' # extensible group number before deleting
        #' surf$extensible_group_num()
        #'
        #' # delete 2 more extensible groups
        #' surf$del_extensible_group(2)
        #'
        #' # field number after deleting
        #' surf$num_fields()
        #' # extensible group number after deleting
        #' surf$extensible_group_num()
        #' }
        #'
        del_extensible_group = function (num = 1L)
            iddobj_del_extensible_group(self, private, num),
        # }}}
        # }}}

        # CLASS PROPERTY ASSERTIONS {{{
        # has_name {{{
        #' @description
        #' Check if current class has name attribute
        #'
        #' @details
        #' `$has_name()` return `TRUE` if current class has name attribute, and
        #' `FALSE` otherwise.
        #'
        #' A class with name attribute means that objects in this class can have
        #' names.
        #'
        #' @return A single logical value (`TRUE` or `FALSE`).
        #'
        #' @examples
        #' \dontrun{
        #' surf$has_name()
        #' }
        #'
        has_name = function ()
            iddobj_has_name(self, private),
        # }}}

        # is_required {{{
        #' @description
        #' Check if current class is required
        #'
        #' @details
        #' `$is_required()` returns `TRUE` if current class is required and
        #' `FALSE` otherwise.
        #'
        #' A required class means that for any model, there should be at least
        #' one object in this class. One example is `Building` class.
        #'
        #' @return A single logical value (`TRUE` or `FALSE`).
        #'
        #' @examples
        #' \dontrun{
        #' surf$is_required()
        #' }
        #'
        is_required = function ()
            iddobj_is_required(self, private),
        # }}}

        # is_unique {{{
        #' @description
        #' Check if current class is unique
        #'
        #' @details
        #' `$is_unique()` returns `TRUE` if current class is unique and
        #' `FALSE` otherwise.
        #'
        #' A unique class means that for any model, there should be at most
        #' one object in this class. One example is `Building` class.
        #'
        #' @return A single logical value (`TRUE` or `FALSE`).
        #'
        #' @examples
        #' \dontrun{
        #' surf$is_unique()
        #' }
        #'
        is_unique = function ()
            iddobj_is_unique(self, private),
        # }}}

        # is_extensible {{{
        #' @description
        #' Check if current class is extensible
        #'
        #' @details
        #' `$is_extensible()` returns `TRUE` if current class is extensible and
        #' `FALSE` otherwise.
        #'
        #' A extensible class means that for there are curtain number of fields
        #' in this class that can be dynamically added or deleted, such like the
        #' X, Y and Z vertices of a building surface.
        #'
        #' @return A single logical value (`TRUE` or `FALSE`).
        #'
        #' @examples
        #' \dontrun{
        #' surf$is_extensible()
        #' }
        #'
        is_extensible = function ()
            iddobj_is_extensible(self, private),
        # }}}
        # }}}

        # FIELD PROPERTY GETTERS {{{

        # field_name {{{
        #' @description
        #' Get field names
        #'
        #' @details
        #' `$field_name()` returns a character vector of names of fields
        #' specified by field indices in current class.
        #'
        #' @param index An integer vector of field indices. If `NULL`, names of
        #'        all fields in this class are returned. Default: `NULL`.
        #'
        #' @param unit If `TRUE`, the units of those fields are also returned.
        #'        Default: `FALSE`.
        #'
        #' @param in_ip If `in_ip`, corresponding imperial units are returned.
        #'        It only has effect when `unit` is `TRUE`. Default:
        #'        `eplusr_option("view_in_ip")`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get all field names
        #' surf$field_name()
        #'
        #' # get field units also
        #' surf$field_name(unit = TRUE)
        #'
        #' # get field units in IP
        #' surf$field_name(unit = TRUE)
        #'
        #' # change field name to lower-style
        #' surf$field_name(unit = TRUE, in_ip = TRUE)
        #' }
        #'
        field_name = function (index = NULL, unit = FALSE, in_ip = eplusr_option("view_in_ip"))
            iddobj_field_name(self, private, index, unit, in_ip),
        # }}}

        # field_index {{{
        #' @description
        #' Get field indices
        #'
        #' @details
        #' `$field_index()` returns an integer vector of names of fields
        #' specified by field names in current class.
        #'
        #' @param name A character vector of field names. Can be in
        #'        "lower-style", i.e. all spaces and dashes is replaced by
        #'        underscores. If `NULL`, indices of all fields in this class
        #'        are returned. Default: `NULL`.
        #'
        #' @return An integer vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get all field indices
        #' surf$field_index()
        #'
        #' # get field indices for specific fields
        #' surf$field_index(c("number of vertices", "vertex 10 z-coordinate"))
        #' }
        #'
        field_index = function (name = NULL)
            iddobj_field_index(self, private, name),
        # }}}

        # field_type {{{
        #' @description
        #' Get field types
        #'
        #' @details
        #' `$field_type()` returns a character vector of field types of
        #' specified fields in current class. All possible values are:
        #'
        #' * `"integer"`
        #' * `"real"`
        #' * `"alpha"` (arbitrary string)
        #' * `"choice"` (alpha with specific list of choices)
        #' * `"object-list"` (link to a list of objects defined elsewhere)
        #' * `"external-list"` (uses a special list from an external source)
        #' * `"node"` (name used in connecting HVAC components).
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get all field types
        #' surf$field_type()
        #'
        #' # get field types for specific fields
        #' surf$field_type(c("name", "zone name", "vertex 10 z-coordinate"))
        #' }
        #'
        field_type = function (which = NULL)
            iddobj_field_type(self, private, which = which),
        # }}}

        # field_note {{{
        #' @description
        #' Get field notes
        #'
        #' @details
        #' `$field_note()` returns a list of character vectors that contains
        #' field notes of specified fields in current class, usually serving as
        #' field descriptions. If no notes are found for current fields, `NULL`
        #' is returned.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @return A list of character vectors.
        #'
        #' @examples
        #' \dontrun{
        #' # get all field notes
        #' surf$field_note()
        #'
        #' # get field types for specific fields
        #' surf$field_note(c("name", "zone name", "vertex 10 z-coordinate"))
        #' }
        #'
        field_note = function (which = NULL)
            iddobj_field_note(self, private, which),
        # }}}

        # field_unit {{{
        #' @description
        #' Get field units
        #'
        #' @details
        #' `$field_unit()` returns a character vector that contains units of
        #' specified fields in current class. If there is no unit found for
        #' current field, `NA` is returned.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @param in_ip If `in_ip`, corresponding imperial units are returned.
        #'        Default: `eplusr_option("view_in_ip")`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get all field units
        #' surf$field_unit()
        #'
        #' # get field units for specific fields
        #' surf$field_unit(c("name", "zone name", "vertex 10 z-coordinate"))
        #' }
        #'
        field_unit = function (which = NULL, in_ip = eplusr_option("view_in_ip"))
            iddobj_field_unit(self, private, which, in_ip),
        # }}}

        # field_default {{{
        #' @description
        #' Get field default value
        #'
        #' @details
        #' `$field_default()` returns a list that contains default values of
        #' specified fields in current class. If there is no default value found
        #' for current field, `NA` is returned.
        #'
        #' @note
        #' The type of each default value will be consistent with field
        #' definition. However, for numeric fields with default values being
        #' `"autosize"` or `"autocalculate"`, the type of returned values will
        #' be character.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @param in_ip If `in_ip`, values in corresponding imperial units are
        #'        returned. Default: `eplusr_option("view_in_ip")`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get all field default values
        #' surf$field_default()
        #'
        #' # get default values for specific fields
        #' surf$field_default(c("name", "zone name", "vertex 10 z-coordinate"))
        #' }
        #'
        field_default = function (which = NULL, in_ip = eplusr_option("view_in_ip"))
            iddobj_field_default(self, private, which, in_ip),
        # }}}

        # field_choice {{{
        #' @description
        #' Get choices of field values
        #'
        #' @details
        #' `$field_value()` returns a list of character vectors that contains
        #' choices of specified field values in current class. If there is no
        #' choice found for current field, `NULL` is returned.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @return A list of character vectors.
        #'
        #' @examples
        #' \dontrun{
        #' # get all field value choices
        #' surf$field_choice()
        #'
        #' # get field value choices for specific fields
        #' surf$field_choice(c("name", "sun exposure", "wind exposure"))
        #' }
        #'
        field_choice = function (which = NULL)
            iddobj_field_choice(self, private, which),
        # }}}

        # field_range {{{
        #' @description
        #' Get field value ranges
        #'
        #' @details
        #' `$field_range()` returns a list of value ranges of specified fields
        #' in current class.
        #'
        #' Every range has four components:
        #'
        #' * `minimum`: lower limit
        #' * `lower_incbounds`: `TRUE` if the lower limit should be included
        #' * `maximum`: upper limit
        #' * `upper_incbounds`: `TRUE` if the upper limit should be included
        #'
        #' For fields of character type,
        #'
        #' * `minimum` and `maximum` are always set to `NA`
        #' * `lower_incbounds` and `upper_incbounds` are always set to `FALSE`
        #'
        #' For fields of numeric types with no specified ranges,
        #'
        #' * `minimum` is set to `-Inf`
        #' * `lower_incbounds` is set to `FALSE`
        #' * `upper` is set to `Inf`
        #' * `upper_incbounds` is set to `FALSE`
        #'
        #' The field range is printed in number interval denotation.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @return A list of ranges.
        #'
        #' @examples
        #' \dontrun{
        #' # get all field value ranges
        #' surf$field_range()
        #'
        #' # get value ranges for specific fields
        #' surf$field_range(c("name", "number of vertices", "vertex 10 z-coordinate"))
        #' }
        #'
        field_range = function (which = NULL)
            iddobj_field_range(self, private, which),
        # }}}

        # field_relation {{{
        #' @description
        #' Extract the relationship among fields
        #'
        #' @details
        #' Many fields in [Idd] can be referred by others. For example, the
        #' `Outside Layer` and other fields in `Construction` class refer to the
        #' `Name` field in `Material` class and other material related classes.
        #' Here it means that the `Outside Layer` field **refers to** the `Name`
        #' field and the `Name` field is **referred by** the `Outside Layer`.
        #'
        #' `$field_relation()` provides a simple interface to get this kind of
        #' relation. It takes a field specification and a relation
        #' direction, and returns an `IddRelation` object which contains data
        #' presenting such relation above.
        #'
        #' `$field_relation()` returns a list of references for those fields
        #' that have the `object-list` and/or `reference` and
        #' `reference-class-name` attribute. Basically, it is a list of two
        #' elements `ref_to` and `ref_by`. Underneath, `ref_to` and `ref_by`
        #' are [data.table][data.table::data.table()]s which contain source
        #' field data and reference field data with custom printing method. For
        #' instance, if `iddobj$field_relation(c(1, 2), "ref_to")` gives results
        #' below:
        #'
        #' ```
        #' -- Refer to Others ---------------------
        #'   +- Field: <1: Field 1>
        #'   |  v~~~~~~~~~~~~~~~~~~
        #'   |  \- Class: <Class 2>
        #'   |     \- Field: <2: Field 2>
        #'   |
        #'   \- Field: <2: Field 2>
        #' ```
        #'
        #' This means that `Field 2` in current class does not refer to any other fields.
        #' But `Field 1` in current class refers to `Field 2` in class named `Class 2`.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @param direction The relation direction to extract. Should be one of
        #'        `"all"`, `"ref_to"` or `"ref_by"`.
        #'
        #' @param class A character vector of class names used for searching
        #'        relations. Default: `NULL`.
        #'
        #' @param group A character vector of group names used for searching
        #'        relations. Default: `NULL`.
        #'
        #' @param depth If > 0, the relation is searched recursively. A
        #'        simple example of recursive reference: one material named
        #'        `mat` is referred by a construction named `const`, and `const`
        #'        is also referred by a surface named `surf`. If `NULL`,
        #'        all possible recursive relations are returned. Default: `0`.
        #'
        #' @param keep If `TRUE`, all input fields are returned regardless they
        #'        have any relations with other objects or not. If `FALSE`, only
        #'        fields in input that have relations with other objects are
        #'        returned. Default: `FALSE`.
        #'
        #' @return An `IddRelation` object.
        #'
        #' @examples
        #' \dontrun{
        #' # get field relation for specific fields
        #' surf$field_relation(c("name", "zone name", "vertex 10 z-coordinate"))
        #' }
        #'
        field_relation = function (which = NULL, direction = c("all", "ref_by", "ref_to"), class = NULL, group = NULL, depth = 0L, keep = FALSE)
            iddobj_field_relation(self, private, which, match.arg(direction), class = class, group = group, depth = depth, keep = keep),
        # }}}

        # field_possible {{{
        #' @description
        #' Get field possible values
        #'
        #' @details
        #' `$field_possible()` returns all possible values for specified fields,
        #' including auto-value (`Autosize`, `Autocalculate`, and `NA` if not
        #' applicable), and results from `$field_default()`, `$field_range()`,
        #' `$field_choice()`. Underneath, it returns a data.table with custom
        #' printing method. For instance, if `iddobj$field_possible(c(4, 2))`
        #' gives results below:
        #'
        #' ```
        #' -- 4: Field 4 ----------
        #' * Auto value: <NA>
        #' * Default: <NA>
        #' * Choice:
        #'   - "Key1"
        #'   - "Key2"
        #'
        #' -- 2: Field 2 ----------
        #' * Auto value: "Autosize"
        #' * Default: 2
        #' * Choice: <NA>
        #' ```
        #'
        #' This means that `Field 4` in current class cannot be "autosized" or
        #' "autocalculated", and it does not have any default value. Its value should be
        #' a choice from `"Key1"` or `"Key2"`. For `Field 2` in current class, it has a
        #' default value of `2` but can also be filled with value `"Autosize"`.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @return A `IddFieldPossible` object which is a
        #' [data.table::data.table()] with 9 columns.
        #'
        #' @examples
        #' \dontrun{
        #' # get field possible values for specific fields
        #' surf$field_possible(6:10)
        #' }
        #'
        field_possible = function (which = NULL)
            iddobj_field_possible(self, private, which),
        # }}}
        # }}}

        # FIELD PROPERTY ASSERTIONS {{{
        # is_valid_field_num {{{
        #' @description
        #' Check if input is a valid field number
        #'
        #' @details
        #' `$is_valid_field_num()` returns `TRUE` if input `num` is acceptable
        #' as a total number of fields in this class. Extensible property is
        #' considered.
        #'
        #' For instance, the total number of fields defined in IDD for class
        #' `BuildingSurfaces:Detailed` is 390. However, 396 is still a valid
        #' field number for this class as the number of field in the extensible
        #' group is 3.
        #'
        #' @param num An integer vector to test.
        #'
        #' @return A logical vector.
        #'
        #' @examples
        #' \dontrun{
        #' surf$is_valid_field_num(c(10, 14, 100))
        #' }
        #'
        is_valid_field_num = function (num)
            iddobj_is_valid_field_num(self, private, num),
        # }}}

        # is_extensible_index {{{
        #' @description
        #' Check if input field index indicates an extensible field
        #'
        #' @details
        #' `$is_extensible_index()` returns `TRUE` if input `index` indicates an
        #' index of extensible field in current class.
        #'
        #' Extensible fields mean that these fields can be dynamically added or
        #' deleted, such like the X, Y and Z vertices of a building surface.
        #'
        #' @param index An integer vector of field indices.
        #'
        #' @return A logical vector.
        #'
        #' @examples
        #' \dontrun{
        #' surf$is_extensible_index(c(10, 14, 100))
        #' }
        #'
        is_extensible_index = function (index)
            iddobj_is_extensible_index(self, private, index),
        # }}}

        # is_valid_field_name {{{
        #' @description
        #' Check if input character is a valid field name
        #'
        #' @details
        #' `$is_valid_field_name()` returns `TRUE` if `name` is a valid field
        #' name **WITHOUT** unit. Note `name` can be given in underscore style,
        #' e.g.  `"outside_layer"` is equivalent to `"Outside Layer"`.
        #'
        #' @param name A character vector to test.
        #'
        #' @param strict If `TRUE`, only exact match is accepted. Default:
        #'        `FALSE`.
        #'
        #' @return A logical vector.
        #'
        #' @examples
        #' \dontrun{
        #' surf$is_valid_field_name(c("name", "sun_exposure"))
        #'
        #' # exact match
        #' surf$is_valid_field_name(c("Name", "Sun_Exposure"), strict = TRUE)
        #' }
        #'
        is_valid_field_name = function (name, strict = FALSE)
            iddobj_is_valid_field_name(self, private, name, strict),
        # }}}

        # is_valid_field_index {{{
        #' @description
        #' Check if input integer is a valid field index
        #'
        #' @details
        #' `$is_valid_field_index()` returns `TRUE` if `index` is a valid field
        #' index. For extensible class, `TRUE` is always returned.
        #'
        #' @param index An integer vector to test.
        #'
        #' @return A logical vector.
        #'
        #' @examples
        #' \dontrun{
        #' surf$is_valid_field_index(1:10)
        #' }
        #'
        is_valid_field_index = function (index)
            iddobj_is_valid_field_index(self, private, index),
        # }}}

        # is_autosizable_field {{{
        #' @description
        #' Check if input field can be autosized
        #'
        #' @details
        #' `$is_autosizable_field()` returns `TRUE` if input field can be
        #' assigned to `autosize`.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @return A logical vector.
        #'
        #' @examples
        #' \dontrun{
        #' surf$is_autosizable_field()
        #'
        #' surf$is_autosizable_field(c("name", "sun_exposure"))
        #' }
        #'
        is_autosizable_field = function (which = NULL)
            iddobj_is_autosizable_field(self, private, which),
        # }}}

        # is_autocalculatable_field {{{
        #' @description
        #' Check if input field can be autocalculated
        #'
        #' @details
        #' `$is_autocalculatable_field()` returns `TRUE` if input field can be
        #' assigned to `autocalculate`.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @return A logical vector.
        #'
        #' @examples
        #' \dontrun{
        #' surf$is_autocalculatable_field()
        #'
        #' surf$is_autocalculatable_field(c("name", "sun_exposure"))
        #' }
        #'
        is_autocalculatable_field = function (which = NULL)
            iddobj_is_autocalculatable_field(self, private, which),
        # }}}

        # is_numeric_field {{{
        #' @description
        #' Check if input field value should be numeric
        #'
        #' @details
        #' `$is_numeric_field()` returns `TRUE` if the value of input field
        #' should be numeric ( an integer or a real number).
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @return A logical vector.
        #'
        #' @examples
        #' \dontrun{
        #' surf$is_numeric_field()
        #'
        #' surf$is_numeric_field(c("name", "sun_exposure"))
        #' }
        #'
        is_numeric_field = function (which = NULL)
            iddobj_is_numeric_field(self, private, which),
        # }}}

        # is_real_field {{{
        #' @description
        #' Check if input field value should be a real number
        #'
        #' @details
        #' `$is_real_field()` returns `TRUE` if the field value should be a real
        #' number but not an integer.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @return A logical vector.
        #'
        #' @examples
        #' \dontrun{
        #' surf$is_real_field()
        #'
        #' surf$is_real_field(c("name", "number of vertices"))
        #' }
        #'
        is_real_field = function (which = NULL)
            iddobj_is_real_field(self, private, which),
        # }}}

        # is_integer_field {{{
        #' @description
        #' Check if input field value should be an integer
        #'
        #' @details
        #' `$is_real_field()` returns `TRUE` if the field value should be an
        #' integer.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @return A logical vector.
        #'
        #' @examples
        #' \dontrun{
        #' surf$is_integer_field()
        #'
        #' surf$is_integer_field(c("name", "number of vertices"))
        #' }
        #'
        is_integer_field = function (which = NULL)
            iddobj_is_integer_field(self, private, which),
        # }}}

        # is_required_field {{{
        #' @description
        #' Check if input field is required
        #'
        #' @details
        #' `$is_required_field()` returns `TRUE` if the field is required.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @return A logical vector.
        #'
        #' @examples
        #' \dontrun{
        #' surf$is_required_field()
        #'
        #' surf$is_required_field(c("name", "number of vertices"))
        #' }
        #'
        is_required_field = function (which = NULL)
            iddobj_is_required_field(self, private, which),
        # }}}

        # has_ref {{{
        #' @description
        #' Check if input field can refer to or can be referred by other fields
        #'
        #' @details
        #' `$has_ref()` returns `TRUE` if input field refers to or can be referred
        #' by other fields.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @param class A character vector of class names used for searching
        #'        relations. Default: `NULL`.
        #'
        #' @param group A character vector of group names used for searching
        #'        relations. Default: `NULL`.
        #'
        #' @param depth If > 0, the relation is searched recursively. A
        #'        simple example of recursive reference: one material named
        #'        `mat` is referred by a construction named `const`, and `const`
        #'        is also referred by a surface named `surf`. If `NULL`,
        #'        all possible recursive relations are returned. Default: `0`.
        #'
        #' @return A logical vector.
        #'
        #' @examples
        #' \dontrun{
        #' surf$has_ref()
        #'
        #' surf$has_ref(c("name", "zone name"))
        #' }
        #'
        has_ref = function (which = NULL, class = NULL, group = NULL, depth = 0L)
            iddobj_has_ref(self, private, which, class = class, group = group, depth = depth),
        # }}}

        # has_ref_to {{{
        #' @description
        #' Check if input field can refer to other fields
        #'
        #' @details
        #' `$has_ref_to()` returns `TRUE` if input field can refer to other
        #' fields.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @param class A character vector of class names used for searching
        #'        relations. Default: `NULL`.
        #'
        #' @param group A character vector of group names used for searching
        #'        relations. Default: `NULL`.
        #'
        #' @param depth If > 0, the relation is searched recursively. A
        #'        simple example of recursive reference: one material named
        #'        `mat` is referred by a construction named `const`, and `const`
        #'        is also referred by a surface named `surf`. If `NULL`,
        #'        all possible recursive relations are returned. Default: `0`.
        #'
        #' @return A logical vector.
        #'
        #' @examples
        #' \dontrun{
        #' surf$has_ref_to()
        #'
        #' surf$has_ref_to(c("name", "zone name"))
        #' }
        #'
        has_ref_to = function (which = NULL, class = NULL, group = NULL, depth = 0L)
            iddobj_has_ref_to(self, private, which, class = class, group = group, depth = depth),
        # }}}

        # has_ref_by {{{
        #' @description
        #' Check if input field can be referred by other fields
        #'
        #' @details
        #' `$has_ref_by()` returns `TRUE` if input field can be referred by
        #' other fields.
        #'
        #' @param which An integer vector of field indices or a character vector
        #'        of field names in current class. If `NULL`, all fields in this
        #'        class are used. Default: `NULL`.
        #'
        #' @param class A character vector of class names used for searching
        #'        relations. Default: `NULL`.
        #'
        #' @param group A character vector of group names used for searching
        #'        relations. Default: `NULL`.
        #'
        #' @param depth If > 0, the relation is searched recursively. A
        #'        simple example of recursive reference: one material named
        #'        `mat` is referred by a construction named `const`, and `const`
        #'        is also referred by a surface named `surf`. If `NULL`,
        #'        all possible recursive relations are returned. Default: `0`.
        #'
        #' @return A logical vector.
        #'
        #' @examples
        #' \dontrun{
        #' surf$has_ref_by()
        #'
        #' surf$has_ref_by(c("name", "zone name"))
        #' }
        #'
        has_ref_by = function (which = NULL, class = NULL, group = NULL, depth = 0L)
            iddobj_has_ref_by(self, private, which, class = class, group = group, depth = depth),
        # }}}
        # }}}

        # DATA EXTRACTION {{{
        # outputs {{{
        #' @description
        #' Get possible output variables for current class
        #'
        #' @details
        #' `$outputs()` returns a [data.table][data.table::data.table()] that
        #' gives all possible outputs for current class.
        #' The returned [data.table][data.table::data.table()] has 6 columns:
        #'
        #' *`index`: Integer. Index of each variable.
        #' *`class`: Character. Name of current class.
        #' * `reported_time_step`: Character. Reported time step for the variables.
        #'   Possible value: `Zone` and `HVAC`.
        #' * `report_type`: Character. Report types. Possible value: `Average`,
        #'   `Sum`.
        #' * `variable`: Character. Report variable names.
        #' * `units`: Character. Units of reported values. `NA` if report values do not
        #'   have units.
        #'
        #' @note
        #' All outputs are extracted from the LaTeX source file of "Input Output
        #' Reference" for EnergyPlus v9.5.0 and later. So empty result will
        #' always be returned for [Idd] version lower than v9.5.
        #'
        #' It is possible that there are some mistakes introduced when
        #' extracting the output variables.
        #' Also, some outputs are only available if certain fields
        #' are set. Even they are listed in the results, it does not mean that
        #' the [Idf] can report all of them.
        #' It is strongly suggested to check the RDD and MDD file for
        #' correctness.
        #'
        #' @return A [data.table][data.table::data.table()] with 6 columns.
        #'
        #' @examples
        #' \dontrun{
        #' surf$outputs()
        #' }
        #'
        outputs = function ()
            iddobj_outputs(self, private),
        # }}}

        # to_table {{{
        #' @description
        #' Format an `IddObject` as a data.frame
        #'
        #' @details
        #' `$to_table()` returns a [data.table][data.table::data.table()] that
        #' contains basic data of current class.
        #' The returned [data.table][data.table::data.table()] has 3 columns:
        #'
        #' * `class`: Character type. Current class name.
        #' * `index`: Integer type. Field indexes.
        #' * `field`: Character type. Field names.
        #'
        #' @param all If `TRUE`, all available fields defined in IDD for
        #'        specified class will be returned. If `FALSE`, only the minimum
        #'        field number is returned. Default: `FALSE`.
        #'
        #' @return A [data.table][data.table::data.table()] with 3 columns.
        #'
        #' @examples
        #' \dontrun{
        #' surf$to_table()
        #'
        #' surf$to_table(TRUE)
        #' }
        #'
        to_table = function (all = FALSE)
            iddobj_to_table(self, private, all),
        # }}}

        # to_string {{{
        #' @description
        #' Format an `IdfObject` as a character vector
        #'
        #' @details
        #' `$to_string()` returns the text format of current class. The returned
        #' character vector can be pasted into an IDF file as an empty object of
        #' specified class.
        #'
        #' @param comment A character vector to be used as comments of returned
        #'        string format object.
        #' @param leading Leading spaces added to each field. Default: `4L`.
        #' @param sep_at The character width to separate value string and field
        #'        string. Default: `29L` which is the same as IDF Editor.
        #' @param all If `TRUE`, all available fields defined in IDD for
        #'        specified class will be returned. Default: `FALSE`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get text format of class BuildingSurface:Detailed
        #' surf$to_string()
        #'
        #' # tweak output formatting
        #' surf$to_string(leading = 0, sep_at = 0)
        #'
        #' # add comments
        #' surf$to_string(c("This", "will", "be", "comments"))
        #' }
        #'
        to_string = function (comment = NULL, leading = 4L, sep_at = 29L, all = FALSE)
            iddobj_to_string(self, private, comment, leading, sep_at = sep_at, all = all),
        # }}}
        # }}}

        # print {{{
        #' @description
        #' Print `IddObject` object
        #'
        #' @details
        #' `$print()` prints the `IddObject` object giving the information of
        #' class name, class properties, field indices and field names.
        #'
        #' `$print()` prints the IddObject. Basically, the print output can be
        #' divided into 4 parts:
        #'
        #' * CLASS: IDD class name of current object in format `<IddObject: CLASS>`.
        #' * MEMO: brief description of the IDD class.
        #' * PROPERTY: properties of the IDD class, including name of group it
        #'   belongs to, whether it is an unique or required class and current
        #'   total fields. The fields may increase if the IDD class is
        #'   extensible, such as `Branch`, `ZoneList` and etc.
        #' * FIELDS: fields of current IDD class. Required fields are marked
        #'   with stars (`*`). If the class is extensible, only the first
        #'   extensible group will be printed and two ellipses will be shown at
        #'   the bottom. Fields in the extensible group will be marked with an
        #'   arrow down surrounded by angle brackets (`<v>`).
        #'
        #' @param brief If `TRUE`, only class name part is printed. Default:
        #'        `FALSE`.
        #'
        #' @return The `IddObject` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' surf
        #'
        #' surf$print(brief = TRUE)
        #' }
        #'
        print = function (brief = FALSE)
            iddobj_print(self, private, brief)
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_parent = NULL,
        m_class_id = NULL,
        # }}}

        # PRIVATE FUNCTIONS {{{
        idd_env = function () .subset2(get_priv_env(private$m_parent), "m_idd_env")
        # }}}
    )
)
# }}}

# iddobj_version {{{
iddobj_version <- function (self, private) {
    private$m_parent$version()
}
# }}}
# iddobj_parent {{{
iddobj_parent <- function (self, private) {
    private$m_parent
}
# }}}
# iddobj_group_index {{{
iddobj_group_index <- function (self, private) {
    private$idd_env()$class[class_id == private$m_class_id, group_id]
}
# }}}
# iddobj_group_name {{{
iddobj_group_name <- function (self, private) {
    grp_id <- iddobj_group_index(self, private)
    private$idd_env()$group[J(grp_id), on = "group_id", group_name]
}
# }}}
# iddobj_class_index {{{
iddobj_class_index <- function (self, private) {
    private$m_class_id
}
# }}}
# iddobj_class_name {{{
iddobj_class_name <- function (self, private) {
    private$idd_env()$class[J(private$m_class_id), on = "class_id", class_name]
}
# }}}
# iddobj_class_data {{{
iddobj_class_data <- function (self, private) {
    private$idd_env()$class[J(private$m_class_id), on = "class_id"]
}
# }}}
# iddobj_class_format {{{
iddobj_class_format <- function (self, private) {
    iddobj_class_data(self, private)$format
}
# }}}
# iddobj_min_fields {{{
iddobj_min_fields <- function (self, private) {
    iddobj_class_data(self, private)$min_fields
}
# }}}
# iddobj_num_fields {{{
iddobj_num_fields <- function (self, private) {
    iddobj_class_data(self, private)$num_fields
}
# }}}
# iddobj_memo {{{
iddobj_memo <- function (self, private) {
    iddobj_class_data(self, private)$memo[[1L]]
}
# }}}
# iddobj_num_extensible {{{
iddobj_num_extensible <- function (self, private) {
    iddobj_class_data(self, private)$num_extensible
}
# }}}
# iddobj_first_extensible_index {{{
iddobj_first_extensible_index <- function (self, private) {
    iddobj_class_data(self, private)$first_extensible
}
# }}}
# iddobj_extensible_group_num {{{
iddobj_extensible_group_num <- function (self, private) {
    iddobj_class_data(self, private)$num_extensible_group
}
# }}}
# iddobj_add_extensible_group {{{
iddobj_add_extensible_group <- function (self, private, num) {
    assert_count(num, positive = TRUE)

    iddenv <- get_priv_env(private$m_parent)$m_idd_env
    iddenv <- add_idd_extensible_group(private$idd_env(), private$m_class_id, num, strict = TRUE)
    get_priv_env(private$m_parent)$log_new_uuid()

    verbose_info(num, " extensible group(s) added")

    self
}
# }}}
# iddobj_del_extensible_group {{{
iddobj_del_extensible_group <- function (self, private, num) {
    assert_count(num, positive = TRUE)

    iddenv <- get_priv_env(private$m_parent)$m_idd_env
    iddenv <- del_idd_extensible_group(private$idd_env(), private$m_class_id, num, strict = TRUE)
    get_priv_env(private$m_parent)$log_new_uuid()

    verbose_info(num, " extensible group(s) deleted")

    self
}
# }}}
# iddobj_has_name {{{
iddobj_has_name <- function (self, private) {
    iddobj_class_data(self, private)$has_name
}
# }}}
# iddobj_is_required {{{
iddobj_is_required <- function (self, private) {
    iddobj_class_data(self, private)$required_object
}
# }}}
# iddobj_is_unique {{{
iddobj_is_unique <- function (self, private) {
    iddobj_class_data(self, private)$unique_object
}
# }}}
# iddobj_is_extensible {{{
iddobj_is_extensible <- function (self, private) {
    iddobj_class_data(self, private)$num_extensible > 0L
}
# }}}
# iddobj_field_data {{{
iddobj_field_data <- function (self, private, which = NULL, property = NULL, underscore = FALSE) {
    all <- if (is.null(which)) TRUE else FALSE
    get_idd_field(private$idd_env(), private$m_class_id, which,
        property, all = all, underscore = underscore, no_ext = TRUE
    )
}
# }}}
# iddobj_field_name {{{
iddobj_field_name <- function (self, private, index = NULL, unit = FALSE, in_ip = eplusr_option("view_in_ip")) {
    index <- assert_integerish(index, lower = 1L, any.missing = FALSE, null.ok = TRUE, coerce = TRUE)

    if (unit) {
        if (eplusr_option("view_in_ip") != in_ip) {
            eplusr_option(view_in_ip = in_ip)
            on.exit(eplusr_option(view_in_ip = !in_ip), add = TRUE)
        }
        res <- format_name(iddobj_field_data(self, private, index, c("units", "ip_units")), prefix = FALSE)
    } else {
        res <- iddobj_field_data(self, private, index)$field_name
    }

    res
}
# }}}
# iddobj_field_index {{{
iddobj_field_index <- function (self, private, name = NULL) {
    assert_character(name, any.missing = FALSE, null.ok = TRUE)
    iddobj_field_data(self, private, name, underscore = TRUE)$field_index
}
# }}}
# iddobj_field_type {{{
iddobj_field_type <- function (self, private, which = NULL) {
    iddobj_field_data(self, private, which, "type", underscore = TRUE)$type
}
# }}}
# iddobj_field_note {{{
iddobj_field_note <- function (self, private, which = NULL) {
    iddobj_field_data(self, private, which, "note", underscore = TRUE)$note
}
# }}}
# iddobj_field_unit {{{
iddobj_field_unit <- function (self, private, which = NULL, in_ip = eplusr_option("view_in_ip")) {
    fld <- iddobj_field_data(self, private, which, c("units", "ip_units"), underscore = TRUE)

    if (in_ip) {
        fld$ip_units
    } else {
        fld$units
    }
}
# }}}
# iddobj_field_default {{{
iddobj_field_default <- function (self, private, which = NULL, in_ip = eplusr_option("view_in_ip")) {
    fld <- iddobj_field_data(self, private, which, underscore = TRUE,
        c("default_chr", "default_num", "units", "ip_units", "type_enum")
    )

    if (in_ip) fld <- field_default_to_unit(private$idd_env(), fld, "si", "ip")

    setnames(fld, c("default_chr", "default_num"), c("value_chr", "value_num"))
    get_value_list(fld)
}
# }}}
# iddobj_field_choice {{{
iddobj_field_choice <- function (self, private, which = NULL) {
    iddobj_field_data(self, private, which, "choice", underscore = TRUE)$choice
}
# }}}
# iddobj_field_range {{{
iddobj_field_range <- function (self, private, which = NULL) {
    fld <- iddobj_field_data(self, private, which, c("type_enum", "has_range",
            "minimum", "lower_incbounds", "maximum", "upper_incbounds"), underscore = TRUE)

    # set limits to Inf for numeric values that do not have ranges
    fld[J(c(IDDFIELD_TYPE$integer, IDDFIELD_TYPE$real), FALSE), on = c("type_enum", "has_range"),
        `:=`(maximum = Inf, minimum = -Inf)]

    fld[, `:=`(range = list(ranger(minimum, lower_incbounds, maximum, upper_incbounds))), by = field_id]

    fld$range
}
# }}}
# iddobj_field_relation {{{
iddobj_field_relation <- function (self, private, which = NULL, direction = c("all", "ref_to", "ref_by"),
                                   class = NULL, group = NULL, depth = 0L, keep = FALSE) {
    direction <- match.arg(direction)

    if (is.null(which)) {
        get_iddobj_relation(private$idd_env(), private$m_class_id, NULL, name = TRUE,
            direction = direction, depth = depth, keep_all = keep,
            class = class, group = group)
    } else {
        fld <- get_idd_field(private$idd_env(), private$m_class_id, which)

        get_iddobj_relation(private$idd_env(), NULL, fld$field_id, name = TRUE,
            direction = direction, depth = depth, keep_all = keep,
            class = class, group = group)
    }
}
# }}}
# iddobj_field_possible {{{
iddobj_field_possible <- function (self, private, which = NULL) {
    fld <- iddobj_field_data(self, private, which, FIELD_COLS$property, underscore = TRUE)
    get_iddobj_possible(private$idd_env(), field_id = fld$field_id)
}
# }}}
# iddobj_is_valid_field_num {{{
iddobj_is_valid_field_num <- function (self, private, num) {
    num <- assert_integerish(num, lower = 1L, any.missing = FALSE, coerce = TRUE)

    cls <- iddobj_class_data(self, private)

    !(
        # it should be FALSE when num is
        # 1. less than min-fields OR
        cls$min_fields > num |
        # 2. larger than num-fields but not extensible OR
        (cls$num_extensible == 0L & num > cls$num_fields) |
        # 3. larger than num-fields and is extensible but not have full
        #    extensible groups
        (cls$num_extensible >  0L &
            ((num - cls$num_fields) %% cls$num_extensible) != 0L
        )
    )
}
# }}}
# iddobj_is_extensible_index {{{
iddobj_is_extensible_index <- function (self, private, index) {
    index <- assert_integerish(index, lower = 1L, any.missing = FALSE, coerce = TRUE)

    cls <- iddobj_class_data(self, private)

    if (!cls$num_extensible) return(rep(FALSE, length(index)))

    index >= cls$first_extensible
}
# }}}
# iddobj_is_valid_field_name {{{
iddobj_is_valid_field_name <- function (self, private, name, strict = FALSE) {
    fld <- iddobj_field_data(self, private, underscore = TRUE)

    name <- as.character(name)

    if (isTRUE(strict)) {
        name %chin% fld$field_name
    } else {
        name %chin% fld$field_name | name %chin% lower_name(fld$field_name)
    }
}
# }}}
# iddobj_is_valid_field_index {{{
iddobj_is_valid_field_index <- function (self, private, index) {
    index <- assert_integerish(index, lower = 1L, any.missing = FALSE, coerce = TRUE)
    index <= iddobj_class_data(self, private)$num_fields
}
# }}}
# iddobj_is_autosizable_field {{{
iddobj_is_autosizable_field <- function (self, private, which) {
    iddobj_field_data(self, private, which, "autosizable", underscore = TRUE)$autosizable
}
# }}}
# iddobj_is_autocalculatable_field {{{
iddobj_is_autocalculatable_field <- function (self, private, which) {
    iddobj_field_data(self, private, which, "autocalculatable", underscore = TRUE)$autocalculatable
}
# }}}
# iddobj_is_numeric_field {{{
iddobj_is_numeric_field <- function (self, private, which) {
    iddobj_field_type(self, private, which) %chin% c("integer", "real")
}
# }}}
# iddobj_is_integer_field {{{
iddobj_is_integer_field <- function (self, private, which) {
    iddobj_field_type(self, private, which) == "integer"
}
# }}}
# iddobj_is_real_field {{{
iddobj_is_real_field <- function (self, private, which) {
    iddobj_field_type(self, private, which) == "real"
}
# }}}
# iddobj_is_required_field {{{
iddobj_is_required_field <- function (self, private, which) {
    iddobj_field_data(self, private, which, "required_field", underscore = TRUE)$required_field
}
# }}}
# iddobj_has_ref {{{
iddobj_has_ref <- function (self, private, which = NULL, class = NULL, group = NULL,
                            depth = 0L, type = c("all", "ref_to", "ref_by")) {
    type <- match.arg(type)

    if (is.null(which)) {
        rel <- get_iddobj_relation(private$idd_env(), private$m_class_id,
            class = class, group = group, depth = depth, direction = type,
            keep_all = TRUE
        )
    } else {
        fld <- get_idd_field(private$idd_env(), private$m_class_id, which)

        rel <- get_iddobj_relation(private$idd_env(), NULL, fld$field_id,
            class = class, group = group, depth = depth, direction = type,
            keep_all = TRUE
        )
    }

    if (type == "all") {
        rel$ref_to[, list(.N > 0 && any(!is.na(src_field_id))), by = "field_id"]$V1 |
        rel$ref_by[, list(.N > 0 && any(!is.na(field_id))), by = "src_field_id"]$V1
    } else if (type == "ref_to") {
        rel$ref_to[, list(.N > 0 && any(!is.na(src_field_id))), by = "field_id"]$V1
    } else {
        rel$ref_by[, list(.N > 0 && any(!is.na(field_id))), by = "src_field_id"]$V1
    }
}
# }}}
# iddobj_has_ref_by {{{
iddobj_has_ref_by <- function (self, private, which = NULL, class = NULL, group = NULL, depth = 0L) {
    iddobj_has_ref(self, private, which, class = class, group = group, depth = depth, type = "ref_by")
}
# }}}
# iddobj_has_ref_to {{{
iddobj_has_ref_to <- function (self, private, which = NULL, class = NULL, group = NULL, depth = 0L) {
    iddobj_has_ref(self, private, which, class = class, group = group, depth = depth, type = "ref_to")
}
# }}}
# iddobj_outputs {{{
iddobj_outputs <- function (self, private) {
    vars <- OUTPUT_VARS[[as.character(private$m_parent$version())]]

    if (!NROW(vars)) {
        res <- OUTPUT_VARS[[1]][0]
        set(res, NULL, "index", integer())
    } else {
        res <- vars[J(iddobj_class_name(self, private)), on = "class_name", nomatch = NULL]
        set(res, NULL, "index", seq_len(nrow(res)))
    }

    setnames(res, "class_name", "class")
    setcolorder(res, c("index", "class"))[]
}
# }}}
# iddobj_to_table {{{
iddobj_to_table <- function (self, private, all = FALSE) {
    get_iddobj_table(private$idd_env(), private$m_class_id, all)
}
# }}}
# iddobj_to_string {{{
iddobj_to_string <- function (self, private, comment = NULL, leading = 4L, sep_at = 29L, all = FALSE) {
    get_iddobj_string(private$idd_env(), private$m_class_id, comment = comment,
        leading = leading, sep_at = sep_at, all = all
    )
}
# }}}
# iddobj_print {{{
iddobj_print <- function (self, private, brief = FALSE) {
    # CLASS {{{
    cls <- iddobj_class_data(self, private)
    cli::cat_line(paste0("<IddObject: ", surround(cls$class_name), ">"))

    if (brief) return(invisible(self))

    # memo {{{
    cli::cat_rule("MEMO")
    if (is.null(cls$memo[[1L]])) {
        cli::cat_line("  <No Memo>\n")
    } else {
        cli::cat_line("  \"", paste0(cls$memo[[1L]], collapse = "\n"), "\"\n")
    }
    # }}}

    # property {{{
    cli::cat_rule("PROPERTIES")

    grp <- private$idd_env()$group[J(cls$group_id), on = "group_id", group_name]
    cli::cat_line("  * ", c(
        paste0("Group: ", surround(grp)),
        paste0("Unique: ", cls$unique_object),
        paste0("Required: ", cls$required_object),
        paste0("Total fields: ", cls$num_fields)
    ))
    cli::cat_line()
    # }}}
    # }}}

    # FIELD {{{
    cli::cat_rule("FIELDS")

    # calculate number of fields to print
    if (cls$num_extensible) {
        # only print the first extensible group
        set(cls, NULL, "num_print",
            pmax(cls$last_required, cls$first_extensible + cls$num_extensible - 1L)
        )
    } else {
        set(cls, NULL, "num_print", cls$num_fields)
    }

    fld <- iddobj_field_data(self, private, seq_len(cls$num_print), c("extensible_group", "required_field"))
    set(fld, NULL, "name", format_name(fld, prefix = FALSE))
    set(fld, NULL, "index", format_index(fld, required = TRUE, pad_char = "0"))

    set(fld, NULL, "ext", "")
    fld[extensible_group > 0L, ext := paste0(" <", cli::symbol$arrow_down, ">")]

    cli::cat_line("  ", fld$index, ": ", fld$name, fld$ext)

    if (cls$num_extensible) cli::cat_line("   ......")
    # }}}
}
# }}}

#' Format an IddObject
#'
#' Format an [IddObject] into a string. It is formatted the same way as
#' `IddObject$print(brief = TRUE)` but with a suffix of current IDD version.
#'
#' @param x An [IddObject] object.
#' @param ver If `TRUE`, a suffix of version string is added. Default: `TRUE`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A single length character vector.
#' @examples
#' \dontrun{
#' format(use_idd(8.8, download = "auto")$Material)
#' }
#'
#' @export
# format.IddObject {{{
format.IddObject <- function (x, ver = TRUE, ...) {
    nm <- get_idd_class(get_priv_env(x)$idd_env(), get_priv_env(x)$m_class_id)$class_name
    if (isTRUE(ver)) {
        paste0("<IddObject: ", surround(nm), " v", x$version(), ">")
    } else {
        paste0("<IddObject: ", surround(nm), ">")
    }
}
# }}}

#' Coerce an IddObject into a Character Vector
#'
#' Coerce an [IddObject] into an empty object of current class in a character
#' vector format. It is formatted exactly the same as in IDF Editor.
#'
#' @param x An [IddObject] object.
#' @param all If `TRUE`, all fields in current class are returned, otherwise
#'        only minimum fields are returned.
#' @param comment A character vector to be used as comments of returned string
#'        format object. If `NULL`, no comments are inserted. Default: `NULL`.
#' @param leading Leading spaces added to each field. Default: `4`.
#' @param sep_at The character width to separate value string and field string.
#'        Default: `29` which is the same as IDF Editor.'
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A character vector.
#' @examples
#' \dontrun{
#' as.character(use_idd(8.8, download = "auto")$Materal, leading = 0)
#' }
#'
#' @export
# as.character.IddObject {{{
as.character.IddObject <- function (x, comment = NULL, leading = 4L, sep_at = 29L, all = FALSE, ...) {
    x$to_string(comment = comment, leading = leading, sep_at = sep_at, all = all)
}
# }}}

#' @export
# str.IddObject {{{
str.IddObject <- function (object, brief = FALSE, ...) {
    object$print(brief)
}
# }}}

#' @export
# ==.IddObject {{{
`==.IddObject` <- function (e1, e2) {
    if (!is_iddobject(e2)) return(FALSE)
    identical(
        get_priv_env(get_priv_env(e1)$m_parent)$uuid(),
        get_priv_env(get_priv_env(e2)$m_parent)$uuid()
    ) &&
    identical(get_priv_env(e1)$m_class_id, get_priv_env(e2)$m_class_id)
}

#' @export
`!=.IddObject` <- function (e1, e2) {
    Negate(`==.IddObject`)(e1, e2)
}
# }}}

#' @export
# .DollarNames.IddObject {{{
.DollarNames.IddObject <- function (x, pattern = "") {
    grep(pattern, c(x$field_name(), names(x)), value = TRUE)
}
# }}}
