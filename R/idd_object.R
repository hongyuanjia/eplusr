#' @importFrom R6 R6Class
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
#' @section Usage:
#' ```
#' iddobj <- idd$object(class)
#' iddobj <- idd_object(idd, class)
#' iddobj$version()
#' iddobj$group_name()
#' iddobj$group_index()
#' iddobj$class_name()
#' iddobj$class_index()
#' iddobj$class_format()
#' iddobj$min_fields()
#' iddobj$num_fields()
#' iddobj$memo()
#' iddobj$has_name()
#' iddobj$is_required()
#' iddobj$is_unique()
#' iddobj$is_extensible()
#' iddobj$num_extensible()
#' iddobj$first_extensible_index()
#' iddobj$extensible_group_num()
#' iddobj$add_extensible_group(num = 1L)
#' iddobj$del_extensible_group(num = 1L)
#' iddobj$field_name(index = NULL, unit = FALSE, in_ip = eplusr_option("view_in_ip")
#' iddobj$field_index(name = NULL)
#' iddobj$field_type(which = NULL)
#' iddobj$field_note(which = NULL)
#' iddobj$field_unit(which = NULL, in_ip = eplusr_option("view_in_ip")
#' iddobj$field_default(which = NULL, in_ip = eplusr_option("view_in_ip")
#' iddobj$field_choice(which = NULL)
#' iddobj$field_range(which = NULL)
#' iddobj$field_relation(which = NULL, type = c("all", "ref_by", "ref_to"))
#' iddobj$field_possible(which = NULL)
#' iddobj$is_valid_field_num(num)
#' iddobj$is_extensible_index(index)
#' iddobj$is_valid_field_name(name)
#' iddobj$is_valid_field_index(which)
#' iddobj$is_autosizable_field(which = NULL)
#' iddobj$is_autocalculatable_field(which = NULL)
#' iddobj$is_numeric_field(which = NULL)
#' iddobj$is_integer_field(which = NULL)
#' iddobj$is_real_field(which = NULL)
#' iddobj$is_required_field(which = NULL)
#' iddobj$has_ref(which = NULL)
#' iddobj$has_ref_to(which = NULL)
#' iddobj$has_ref_by(which = NULL)
#' iddobj$to_table(all = FALSE)
#' iddobj$to_string(comment = NULL, leading = 4L, sep_at = 29L, all = FALSE)
#' iddobj$print(brief = FALSE)
#' print(iddobj)
#' ```
#'
#' @section Basic:
#' ```
#' iddobj <- idd$object(class)
#' iddobj <- idd_object(idd, class)
#' iddobj$version()
#' ```
#'
#' An `IddObject` can be created from the parent [Idd] object, using
#' `$object()`, [idd_object] and other equivalent.
#'
#' `$version()` returns the version of parent IDD current object belongs to.
#'
#' **Arguments**
#'
#' * `idd`: An [Idd] object.
#' * `class`: A valid class name (a string).
#' * `iddobj`: An IddObject object.
#'
#' @section Class Property:
#' ```
#' iddobj$group_name()
#' iddobj$group_index()
#' iddobj$class_name()
#' iddobj$class_index()
#' iddobj$class_format()
#' iddobj$min_fields()
#' iddobj$num_fields()
#' iddobj$memo()
#' iddobj$has_name()
#' iddobj$is_required()
#' iddobj$is_unique()
#' ```
#'
#' `$group_index()` returns the index of IDD group it belongs to.
#'
#' `$group_name()` returns the name of IDD group it belongs to.
#'
#' `$class_index()` returns the index of this IDD class.
#'
#' `$class_name()` returns the name of this IDD class.
#'
#' `$class_format()` returns the format of this IDD class. This format indicator
#' is currently not used by eplusr. **Note**: some classes have special format
#' when saved in the IDFEditor with the special format option enabled. Those
#' special format includes "singleLine", "vertices", "compactSchedule",
#' "fluidProperties", "viewFactors" and "spectral".  eplusr can handle all those
#' format when parsing IDF files. However, when saved, all classes are formatted
#' in standard way.
#'
#' `$min_fields()` returns the minimum fields required for this class.  If no
#' required, `0` is returned.
#'
#' `$num_fields()` returns current total number of fields in this class. This
#' number may change if the class is extensible and after
#' `$add_extensible_group()` or `$del_extensible_group()`.
#'
#' `$memo()` returns memo of this class. Usually a brief description of this
#' class.
#'
#' `$has_name()` return `TRUE` if this class has name attribute.
#'
#' `$is_unique()` return `TRUE` if this class is unique.
#'
#' `$is_required()` returns `TRUE` if this class is required.
#'
#' @section Extensible Group:
#' ```
#' iddobj$is_extensible()
#' iddobj$num_extensible()
#' iddobj$first_extensible_index()
#' iddobj$extensible_group_num()
#' iddobj$add_extensible_group(num = 1L)
#' iddobj$del_extensible_group(num = 1L)
#' ```
#'
#' `$is_extensible()` returns `TRUE` if this class is extensible.
#'
#' `$num_extensible()` returns the number of extensible fields in this class. If
#' not zero, it means that objects in this class is dynamically extensible.
#'
#' `$first_extensible_index()` returns the field index of the first extensible
#' field in this class. If this class is not extensible, `0` is return.
#'
#' `$extensible_group_num()` returns the number of extensible groups in this
#' class.
#'
#' `$add_extensible_groups()` adds extensible groups in this class.
#'
#' `$del_extensible_groups()` deletes extensible groups in this class.
#'
#' **Arguments**
#'
#' * `num`: A positive integer of how many extensible groups to add or delete.
#'   Default: `1`.
#'
#' @section Field Property:
#' ```
#' iddobj$field_name(index = NULL, unit = FALSE, in_ip = eplusr_option("view_in_ip")
#' iddobj$field_index(name = NULL)
#' iddobj$field_type(which = NULL)
#' iddobj$field_note(which = NULL)
#' iddobj$field_unit(which = NULL, in_ip = eplusr_option("view_in_ip")
#' iddobj$field_default(which = NULL, in_ip = eplusr_option("view_in_ip")
#' iddobj$field_choice(which = NULL)
#' iddobj$field_range(which = NULL)
#' iddobj$field_relation(which = NULL, type = c("all", "ref_by", "ref_to"))
#' iddobj$field_possible(which = NULL)
#' iddobj$is_valid_field_num(num)
#' iddobj$is_extensible_index(index)
#' iddobj$is_valid_field_name(name)
#' iddobj$is_valid_field_index(which)
#' iddobj$is_autosizable_field(which = NULL)
#' iddobj$is_autocalculatable_field(which = NULL)
#' iddobj$is_numeric_field(which = NULL)
#' iddobj$is_integer_field(which = NULL)
#' iddobj$is_real_field(which = NULL)
#' iddobj$is_required_field(which = NULL)
#' iddobj$has_ref(which = NULL)
#' iddobj$has_ref_to(which = NULL)
#' iddobj$has_ref_by(which = NULL)
#' ```
#'
#' `$field_name()` returns names of fields specified by field indexes. If
#' `index` is `NULL`, names of all fields in this class are returned. If `lower`
#' is `TRUE`, "lower-style" names are returned, i.e. all spaces and dashes is
#' replaced by underscores. "lower-style" names are useful when use them as
#' filed names in `$set_value()` in `IdfObject` class and `$set_object()` in
#' `Idf` class. If `unit` is `TRUE`, the units of those fields are also
#' returned. If `in_ip`, corresponding imperial units are returned. It only has
#' effect when `unit` is `TRUE`.
#'
#' `$field_index()` returns indexes of fields specified by field names. If
#' `name` is `NULL`, indexes of all fields in this class are returned.
#'
#' All other `$field_*()` returns specific field properties. If `which` is
#' `NULL`, properties of all fields in this class are returned.
#'
#' `$field_type`(): returns field types. All possible values are
#' `"integer"`, `"real"`, `"alpha"` (arbitrary string), `"choice"` (alpha
#' with specific list of choices), `"object-list"` (link to a list of
#' objects defined elsewhere), `"external-list"` (uses a special list from
#' an external source) and `"node"` (name used in connecting HVAC
#' components).
#'
#' `$field_unit()`: returns a character vector of field units. If `in_ip` is
#' `TRUE`, IP unites are returned.
#'
#' `$field_default()`: returns a list of default values of those fields. If
#' no defaults found, `NA`s are returned.
#'
#' `$field_choice()`: returns a list of all valid choices for those fields.
#' If no choices found, `NA`s are returned.
#'
#' `$field_range()`: returns a list of ranges for those fields. Every range
#' has four components: `minimum` (lower limit), `lower_incbounds` (`TRUE`
#' if the lower limit should be included), `maximum` (upper limit), and
#' `upper_incbounds` (`TRUE` if the upper limit should be included). For
#' fields of character type, empty lists are returned. For fields of
#' numeric types with no specified ranges, `minimum` is set to `-Inf`,
#' `lower_incbounds` is set to FALSE, `upper` is set to `Inf`, and
#' `upper_incbounds` is set to FALSE. The field range is printed in number
#' interval denotation.
#'
#' `$field_relation()`: returns a list of references for those fields that
#' have the `object-list` and/or `reference` and `reference-class-name`
#' attribute. Basically, it is a list of two elements `ref_to` and `ref_by`.
#' Underneath, `ref_to` and `ref_by` are [data.table][data.table::data.table()]s
#' which contain source field data and reference field data with custom printing
#' method. For instance, if `iddobj$field_relation(c(1, 2), "ref_to")` gives
#' results below:
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
#' `$field_possible()`: returns all possible values for specified fields,
#' including auto-value (`Autosize`, `Autocalculate`, and `NA` if not
#' applicable), and results from `$field_default()`, `$field_range()`,
#' `$field_choice()`. Underneath, it returns a data.table with custom
#' printing method. For instance, if `iddobj$field_possible(c(4, 2))` gives
#' results below:
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
#' `$is_valid_field_num()` returns `TRUE` if `num` is acceptable as a total
#' number of fields in this class. Extensible property is considered. For
#' instance, the total number of fields defined in IDD for class
#' `BuildingSurfaces:Detailed` is 390. However, 396 is still a valid field
#' number for this class as the number of field in the extensible group is 3.
#'
#' `$is_valid_field_name()` returns `TRUE` if `name` is a valid field name
#' **WITHOUT** unit. Note `name` can be given in underscore style, e.g.
#' `"outside_layer"` is equivalent to `"Outside Layer"`.
#'
#' `$is_valid_field_index()` returns `TRUE` if `index` is a valid field index.
#'
#' `$is_autosizable_field()` returns `TRUE` if the field can be assigned to
#' `autosize`.
#'
#' `$is_autocalculatable_field()` returns `TRUE` if the field can be assigned to
#' `autocalculate`.
#'
#' `$is_numeric_field()` returns `TRUE` if the field value should be numeric (
#' an integer or a real number).
#'
#' `$is_integer_field()` returns `TRUE` if the field value should be an integer.
#'
#' `$is_real_field()` returns `TRUE` if the field value should be a real number
#' but not an integer.
#'
#' `$is_required_field()` returns `TRUE` if the field is required.
#'
#' `$has_ref()` returns `TRUE` if the field refers to or can be referred by
#' other fields.
#'
#' `$has_ref_to()` returns `TRUE` if the field refers to other fields.
#'
#' `$has_ref_by()` returns `TRUE` if the field refers can be referred by other
#' fields.
#'
#' **Arguments**
#'
#' * `index`: An integer vector of field indexes.
#' * `name`: A character vector or field names. Can be given in underscore style,
#'   e.g. `"Thermal Resistance"` can be given in format `"thermal_resistance"`.
#' * `which`: An integer vector of field indexes or a character vector of field
#'   names. Field names can be given in underscore style.
#' * `unit`: If `TRUE`, field units will be pasted after field names, just like
#'   the way IDF Editor does. Default: `FALSE`.
#' * `in_ip`: If `TRUE`, field names or values will be returned in IP units.
#'   Default: the value of `eplusr_option("view_in_ip")`.
#' * `type`: The direction of relation to search. Should be one of `"all"`,
#'   `"ref_by"` and `"ref_to"`. If `"ref_by"`, the relation data of specified
#'   fields and fields that refer to specified fields is returned. If
#'   `"ref_to"`, the relation data of specified fields and fields that are
#'   referred by specified fields is returned. If `"all"`, both are returned.
#'
#' @section Data Extraction:
#' ```
#' iddobj$to_table(all = FALSE)
#' iddobj$to_string(comment = NULL, leading = 4L, sep_at = 29L, all = FALSE)
#' ```
#'
#' `$to_table()` returns a [data.table][data.table::data.table()] that contains
#' core data of current class. It has 3 columns:
#'
#' * `class`: Character type. Current class name.
#' * `index`: Integer type. Field indexes.
#' * `field`: Character type. Field names.
#'
#' `$to_string()` returns an empty object of current class in a character vector
#' format. It is formatted exactly the same as in IDF Editor.
#'
#' **Arguments**
#'
#' * `all`: If `TRUE`, all fields in current class are returned, otherwise only
#'   minimum fields are returned.
#' * `unit`: If `TRUE`, units are also returned. Default: `FALSE`.
#' * `comment`: A character vector to be used as comments of returned string
#'   format object. If `NULL`, no comments are inserted. Default: `NULL`.
#' * `leading`: Leading spaces added to each field. Default: `4L`.
#' * `sep_at`: The character width to separate value string and field string.
#'   Default: `29L` which is the same as IDF Editor.
#'
#' @section Print:
#'
#' `$print()` prints the IddObject. Basically, the print output can be divided
#' into 4 parts:
#'
#' * CLASS: IDD class name of current object in format `<IddObject: CLASS>`.
#' * MEMO: brief description of the IDD class
#' * PROPERTY: properties of the IDD class, including name of group it belongs
#'   to, whether it is an unique or required class and current total fields. The
#'   fields may increase if the IDD class is extensible, such as `Branch`,
#'   `ZoneList` and etc.
#' * FIELDS: fields of current IDD class. Required fields are marked with bullet
#'   marks. If the class is extensible, only the first extensible group will be
#'   printed and two ellipses will be shown at the bottom. Fields in the
#'   extensible group will be marked with an arrow down surrounded by angle
#'   brackets.
#'
#' **Argument**:
#'
#' * `brief`: If `TRUE`, only class name part is printed. Default: `FALSE`.
#'
#' @examples
#' \dontrun{
#' # ===== CREATE =====
#' # get a parent Idd object
#' idd <- use_idd(8.8, download = "auto")
#'
#' # get an IddObject of class "Material"
#' mat <- idd$Material
#' # OR
#' mat <- idd_object(idd, "Material")
#'
#' # ===== BASIC INFO =====
#' # get the version of parent IDD
#' mat$version()
#'
#' # ===== CLASS PROPERTY =====
#' # get name of IDD group it belongs to
#' mat$group_name()
#'
#' # get index of IDD group it belongs to
#' mat$group_index()
#'
#' # get name of current IDD class
#' mat$class_name()
#'
#' # get index of current IDD class
#' mat$class_index()
#'
#' # get the format of current IDD class
#' mat$class_format()
#'
#' # get minimum field number
#' mat$min_fields()
#'
#' # get total field number
#' mat$num_fields()
#'
#' # get memo of current class
#' mat$memo()
#'
#' # check if current class has name attribute or not
#' mat$has_name()
#'
#' # check if current class is required
#' mat$is_required()
#'
#' # check if current class is unique
#' mat$is_unique()
#'
#' # ===== EXTENSIBLE GROUP =====
#' # get an IddObject of extensible class "Branch"
#' bran <- idd$Branch
#'
#' # check if the class is extensible
#' bran$is_extensible()
#'
#' # get number of extensible fields, index of first extensible field and number of
#' # current extensible groups in "Branch" class
#' bran$num_extensible()
#'
#' bran$first_extensible_index()
#'
#' bran$extensible_group_num()
#'
#' # get current number of fields
#' bran$num_fields()
#'
#' # add ten extensible groups
#' bran$add_extensible_group(10)
#' # the number of fields has been increased by 10 * 4 (= 40)
#' bran$num_fields()
#'
#' # delete eight extensible groups
#' bran$del_extensible_group(8)
#' # the number of fields has been decreased by 8 * 4 (= 32)
#' bran$num_fields()
#'
#' # ===== FIELD PROPERTY =====
#' # list all field names without units
#' mat$field_name()
#'
#' # get field indexes
#' mat$field_index(c("thickness", "roughness", "name"))
#'
#' # get field types
#' mat$field_type(c("solar_absorptance", "Density", "Name"))
#'
#' # get field notes
#' bran$field_note(c(2, 4))
#'
#' # get field SI units
#' mat$field_unit(c(1,3,5), in_ip = FALSE)
#'
#' # get field IP units
#' mat$field_unit(c(1,3,5), in_ip = TRUE)
#'
#' # get field default values in SI units
#' str(mat$field_default(in_ip = FALSE))
#'
#' # get field choices
#' str(mat$field_choice(1:3))
#'
#' # get field ranges
#' mat$field_range(c("roughness", "thickness", "conductivity", "solar_absorptance"))
#'
#' # get field relation with other fields
#' mat$field_relation(type = "all")
#'
#' # get all possible values of fields
#' mat$field_possible()
#'
#' # check if input is a valid field number for current class
#' ## get required minimum field number
#' mat$min_fields()
#'
#' # (1) if less than required minimum field number
#' mat$is_valid_field_num(3)
#'
#' # (2) if larger than required minimum field number but less than total field
#' # number
#' mat$is_valid_field_num(7)
#'
#' # (3) if larger than total field number
#' mat$is_valid_field_num(10)
#' # [1] FALSE
#'
#' # for extensible class
#' bran$num_fields()
#' bran$num_extensible()
#' # if larger than required minimum field number
#' # (1) but cannot give whole extensible groups
#' bran$is_valid_field_num(c(55, 57, 60))
#'
#' # (2) and can give whole extensible groups
#' bran$is_valid_field_num(c(58, 62, 70))
#'
#' # check if input field index is an extensible field index
#' bran$is_extensible_index(1:4)
#'
#' # check if input is valid field name
#' # NOTE: lower-style names are treated as valid
#' mat$is_valid_field_name(c("nAmE", "specific heat", "Specific Heat", "specific_heat"))
#'
#' # check if input is valid field index
#' bran$is_valid_field_index(c(1, 4, 54, 57))
#'
#' # check if fields are autosizable, i.e. can be set to "Autosize"
#' mat$is_autosizable_field(1:4)
#'
#' # check if fields are autocalculatable, i.e. can be set to "Autocalculate"
#' mat$is_autocalculatable_field(1:4)
#'
#' # check if fields are numeric fields, i.e. field values should be either
#' # integers or float numbers
#' mat$is_numeric_field(c("roughness", "thickness", "density"))
#'
#' # check if fields are integer fields, i.e. field values should be integers
#' mat$is_integer_field(c("name", "specific_heat"))
#'
#' # check if fields are real fields, i.e. field values should be real numbers
#' # but not integers
#' mat$is_real_field(c("name", "specific_heat"))
#'
#' # check if fields are required, i.e. field values should not be empty
#' mat$is_required_field(c("name", "roughness", "solar_absorptance"))
#'
#' # check if fields refer to or can be referred by other fields
#' mat$has_ref()
#'
#' # check if fields refer to other fields
#' mat$has_ref_to()
#'
#' # check if fields can be referred by other fields
#' mat$has_ref_by(which = NULL)
#'
#' # ===== DATA EXTRACTION =====
#' # get core data of current class
#' mat$to_table()
#'
#' # get an empty string-foramt object of current class
#' mat$to_string()
#'
#' # ===== PRINT =====
#' mat$print()
#' }
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
# idd_object {{{
idd_object <- function (parent, class) {
    IddObject$new(class, parent)
}
# }}}

# IddObject {{{
IddObject <- R6::R6Class(classname = "IddObject", cloneable = FALSE,

    public = list(
        # INITIALIZE {{{
        initialize = function (class, parent) {
            if (missing(parent)) {
                abort("error_iddobject_missing_parent",
                    paste("IddObject can only be created based on a parent Idd object.",
                        "Please give `parent`, which should be either an IDD version or an `Idd` object."
                    )
                )
            } else {
                private$m_parent <- use_idd(parent)
            }

            assert(!is.null(class))
            private$m_class_id <- get_idd_class(private$idd_env(), class, underscore = TRUE)$class_id
        },
        # }}}

        version = function ()
            iddobj_version(self, private),

        # CLASS PROPERTY GETTERS {{{
        group_name = function ()
            iddobj_group_name(self, private),

        group_index = function ()
            iddobj_group_index(self, private),

        class_name = function ()
            iddobj_class_name(self, private),

        class_index = function ()
            iddobj_class_index(self, private),

        class_format = function ()
            iddobj_class_format(self, private),

        min_fields = function ()
            iddobj_min_fields(self, private),

        num_fields = function ()
            iddobj_num_fields(self, private),

        memo = function ()
            iddobj_memo(self, private),

        num_extensible = function ()
            iddobj_num_extensible(self, private),

        first_extensible_index = function ()
            iddobj_first_extensible_index(self, private),

        extensible_group_num = function ()
            iddobj_extensible_group_num(self, private),
        # }}}

        # EXTENSIBLE GROUP {{{
        add_extensible_group = function (num = 1L)
            iddobj_add_extensible_group(self, private, num),

        del_extensible_group = function (num = 1L)
            iddobj_del_extensible_group(self, private, num),
        # }}}

        # CLASS PROPERTY ASSERTIONS {{{
        has_name = function ()
            iddobj_has_name(self, private),

        is_required = function ()
            iddobj_is_required(self, private),

        is_unique = function ()
            iddobj_is_unique(self, private),

        is_extensible = function ()
            iddobj_is_extensible(self, private),
        # }}}

        # FIELD PROPERTY GETTERS {{{
        field_name = function (index = NULL, unit = FALSE, in_ip = eplusr_option("view_in_ip"), lower = FALSE)
            iddobj_field_name(self, private, index, unit, in_ip, lower),

        field_index = function (name = NULL)
            iddobj_field_index(self, private, name),

        field_type = function (which = NULL)
            iddobj_field_type(self, private, which = which),

        field_note = function (which = NULL)
            iddobj_field_note(self, private, which),

        field_unit = function (which = NULL, in_ip = eplusr_option("view_in_ip"))
            iddobj_field_unit(self, private, which, in_ip),

        field_default = function (which = NULL, in_ip = eplusr_option("view_in_ip"))
            iddobj_field_default(self, private, which, in_ip),

        field_choice = function (which = NULL)
            iddobj_field_choice(self, private, which),

        field_range = function (which = NULL)
            iddobj_field_range(self, private, which),

        field_relation = function (which = NULL, type = c("all", "ref_by", "ref_to"))
            iddobj_field_relation(self, private, which, match.arg(type)),

        field_reference = function (which = NULL)
            iddobj_field_reference(self, private, which),

        field_possible = function (which = NULL, in_ip)
            iddobj_field_possible(self, private, which, in_ip),
        # }}}

        # FIELD PROPERTY ASSERTIONS {{{
        is_valid_field_num = function (num)
            iddobj_is_valid_field_num(self, private, num),

        is_extensible_index = function (index)
            iddobj_is_extensible_index(self, private, index),

        is_valid_field_name = function (name, strict = FALSE)
            iddobj_is_valid_field_name(self, private, name, strict),

        is_valid_field_index = function (index)
            iddobj_is_valid_field_index(self, private, index),

        is_autosizable_field = function (which = NULL)
            iddobj_is_autosizable_field(self, private, which),

        is_autocalculatable_field = function (which = NULL)
            iddobj_is_autocalculatable_field(self, private, which),

        is_numeric_field = function (which = NULL)
            iddobj_is_numeric_field(self, private, which),

        is_real_field = function (which = NULL)
            iddobj_is_real_field(self, private, which),

        is_integer_field = function (which = NULL)
            iddobj_is_integer_field(self, private, which),

        is_required_field = function (which = NULL)
            iddobj_is_required_field(self, private, which),

        has_ref = function (which = NULL)
            iddobj_has_ref(self, private, which),

        has_ref_to = function (which = NULL)
            iddobj_has_ref_to(self, private, which),

        has_ref_by = function (which = NULL)
            iddobj_has_ref_by(self, private, which),
        # }}}

        to_table = function (all = FALSE)
            iddobj_to_table(self, private, all),

        to_string = function (comment = NULL, leading = 4L, sep_at = 29L, all = FALSE)
            iddobj_to_string(self, private, comment, leading, sep_at = sep_at, all = all),

        print = function (brief = FALSE)
            iddobj_print(self, private, brief)
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_parent = NULL,
        m_class_id = NULL,
        # }}}

        # PRIVATE FUNCTIONS {{{
        idd_priv = function () {
            ._get_private(private$m_parent)
        },

        idd_env = function () {
            .subset2(._get_private(private$m_parent), "m_idd_env")
        }
        # }}}
    )
)
# }}}

# iddobj_version {{{
iddobj_version <- function (self, private) {
    private$idd_priv()$m_version
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
    assert(is_count(num))

    iddenv <- ._get_private(private$m_parent)$m_idd_env
    iddenv <- add_idd_extensible_group(private$idd_env(), private$m_class_id, num, strict = TRUE)

    verbose_info(num, " extensible group(s) added")

    self
}
# }}}
# iddobj_del_extensible_group {{{
iddobj_del_extensible_group <- function (self, private, num) {
    assert(is_count(num))

    iddenv <- ._get_private(private$m_parent)$m_idd_env
    iddenv <- del_idd_extensible_group(private$idd_env(), private$m_class_id, num, strict = TRUE)

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
iddobj_field_name <- function (self, private, index = NULL, unit = FALSE, in_ip = eplusr_option("view_in_ip"), lower = FALSE) {
    if (!is.null(index)) assert(are_count(index))

    if (unit) {
        if (eplusr_option("view_in_ip") != in_ip) {
            eplusr_option(view_in_ip = in_ip)
            on.exit(eplusr_option(view_in_ip = !in_ip), add = TRUE)
        }
        res <- format_name(iddobj_field_data(self, private, index, c("units", "ip_units")))
    } else {
        res <- iddobj_field_data(self, private, index)$field_name
    }

    if (lower) .deprecated_arg("lower", "0.10.0", "IddObject")

    res
}
# }}}
# iddobj_field_index {{{
iddobj_field_index <- function (self, private, name = NULL) {
    if (!is.null(name)) assert(is.character(name))
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

    if (in_ip) fld <- field_default_to_unit(fld, "si", "ip")

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
    fld <- iddobj_field_data(self, private, which, c("minimum", "lower_incbounds", "maximum", "upper_incbounds"), underscore = TRUE)

    fld[, `:=`(range = list(ranger(minimum, lower_incbounds, maximum, upper_incbounds))), by = field_id]

    fld$range
}
# }}}
# iddobj_field_relation {{{
iddobj_field_relation <- function (self, private, which = NULL, direction = c("all", "ref_to", "ref_by")) {
    direction <- match.arg(direction)

    if (is.null(which)) {
        get_iddobj_relation(private$idd_env(), private$m_class_id, NULL, TRUE, direction, TRUE)
    } else {
        fld <- get_idd_field(private$idd_env(), private$m_class_id, which)

        get_iddobj_relation(private$idd_env(), NULL, fld$field_id, TRUE, direction, TRUE)
    }
}
# }}}
# iddobj_field_reference {{{
iddobj_field_reference <- function (self, private, which = NULL) {
    .deprecated_fun("$field_reference()", "$field_relation()", "Idd", "0.10.0")
    iddobj_field_relation(self, private, which, "ref_to")
}
# }}}
# iddobj_field_possible {{{
iddobj_field_possible <- function (self, private, which = NULL, in_ip) {
    if (!missing(in_ip)) .deprecated_arg("in_ip", "0.10.0", "IddObject")
    fld <- iddobj_field_data(self, private, which, FIELD_COLS$property, underscore = TRUE)
    get_iddobj_possible(private$idd_env(), field_id = fld$field_id)
}
# }}}
# iddobj_is_valid_field_num {{{
iddobj_is_valid_field_num <- function (self, private, num) {
    assert(are_count(num))

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
    assert(are_count(index))

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
    assert(are_count(index))
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
iddobj_has_ref <- function (self, private, which = NULL, type = c("all", "ref_to", "ref_by")) {
    type <- match.arg(type)

    if (is.null(which)) {
        rel <- get_iddobj_relation(private$idd_env(), private$m_class_id, direction = type)
    } else {
        fld <- get_idd_field(private$idd_env(), private$m_class_id, which)

        rel <- get_iddobj_relation(private$idd_env(), NULL, fld$field_id, direction = type)
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
iddobj_has_ref_by <- function (self, private, which = NULL) {
    iddobj_has_ref(self, private, which, "ref_by")
}
# }}}
# iddobj_has_ref_to {{{
iddobj_has_ref_to <- function (self, private, which = NULL) {
    iddobj_has_ref(self, private, which, "ref_to")
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
    set(fld, NULL, "index", format_index(fld, required = TRUE))

    set(fld, NULL, "ext", "")
    fld[extensible_group > 0L, ext := paste0(" <", cli::symbol$arrow_down, ">")]

    cli::cat_line("  ", fld$index, ": ", fld$name, fld$ext)

    if (cls$num_extensible) cli::cat_line("   ......")
    # }}}
}
# }}}

#' Format an IddObject
#'
#' Format an [IddObject] into a string of an empty object of current class.
#' It is formatted exactly the same as in IDF Editor.
#'
#' @param x An [IddObject] object.
#' @param all If `TRUE`, all fields in current class are returned, otherwise
#' only minimum fields are returned.
#' @param comment A character vector to be used as comments of returned string
#' format object. If `NULL`, no comments are inserted. Default: `NULL`.
#' @param leading Leading spaces added to each field. Default: `4`.
#' @param sep_at The character width to separate value string and field string.
#' Default: `29` which is the same as IDF Editor.
#' @param ... Further arguments passed to or from other methods.
#' @return A single length character vector.
#' @examples
#' \dontrun{
#' cat(format(use_idd(8.8, download = "auto")$Materal, leading = 0))
#' }
#' @export
# format.IddObject {{{
format.IddObject <- function (x, comment = NULL, leading = 4L, sep_at = 29L, all = FALSE, ...) {
    paste0(x$to_string(comment = comment, leading = leading, sep_at = sep_at, all = all),
        collapse = "\n"
    )
}
# }}}

#' Coerce an IddObject into a Character Vector
#'
#' Coerce an [IddObject] into an empty object of current class in a character
#' vector format. It is formatted exactly the same as in IDF Editor.
#'
#' @inheritParams format.IddObject
#' @return A character vector.
#' @examples
#' \dontrun{
#' as.character(use_idd(8.8, download = "auto")$Materal, leading = 0)
#' }
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
