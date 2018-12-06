#' @importFrom R6 R6Class
NULL

#' EnergyPlus IDD object
#'
#' `IddObject` is an abstraction of a single object in an `Idd` object. It
#' provides more detail methods to query field properties. `IddObject` can only
#' be created from the parent `Idd` object, using `$object()` and
#' `$object_in_group()`. This is because that initialization of an `IddObject`
#' needs some shared data from parent `Idd` object.
#'
#' There are lots of properties for every class and field. For details on the
#' meaning of each property, please see the heading comments in the
#' `Energy+.idd` file in the EnergyPlus installation path.
#'
#' @section Usage:
#' ```
#' iddobj$group_name()
#' iddobj$group_index()
#' iddobj$class_name()
#' iddobj$class_index()
#' iddobj$class_format()
#' iddobj$min_fields()
#' iddobj$num_fields()
#' iddobj$memo()
#' iddobj$num_extensible()
#' iddobj$first_extensible_index()
#' iddobj$extensible_group_num()
#' iddobj$add_extensible_group(num = 1L)
#' iddobj$del_extensible_group(num = 1L)
#' iddobj$has_name()
#' iddobj$is_required()
#' iddobj$is_unique()
#' iddobj$is_extensible()
#' iddobj$field_name(index = NULL, lower = FALSE, unit = FALSE, in_ip = eplusr_option("view_in_ip"))
#' iddobj$field_index(name = NULL)
#' iddobj$field_type(which = NULL)
#' iddobj$field_note(which = NULL)
#' iddobj$field_unit(which = NULL, in_ip = eplusr_option("view_in_ip")
#' iddobj$field_default(which = NULL, in_ip = eplusr_option("view_in_ip")
#' iddobj$field_choice(which = NULL)
#' iddobj$field_range(which = NULL)
#' iddobj$field_reference(which = NULL)
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
#' iddobj$print()
#' print(iddobj)
#' ```
#'
#' @section Arguments:
#'
#' * `iddobj`: An IddObject object.
#' * `num`: A positive integer.
#' * `index`: An integer vector of field indexes.
#' * `name`: A character vector or field names. Can be given in "lower-style".
#'     See below.
#' * `lower`: If `TRUE`, "lower-style" field names will be returned, e.g.
#'     `"Thermal Resistance"` will become `"thermal_resistance"`. Default:
#'     `FALSE`.
#' * `which`: An integer vector of field indexes or a character vector of field
#'     names. Field names can be given in "lower-style".
#' * `unit`: If `TRUE`, field units will be also returned. Default: `FALSE`.
#' * `in_ip`: If `TRUE`, field names or values will be returned in IP units.
#'     Default: eplusr_option("view_in_ip").
#'
#' @section Detail:
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
#'     is currently not used by eplusr. **NOTE**: some classes have special
#'     format when saved in the IDFEditor with the special format option
#'     enabled. Those special format includes "singleLine", "vertices",
#'     "compactSchedule", "fluidProperties", "viewFactors" and "spectral".
#'     eplusr can handle all those format when parsing IDF files. However, when
#'     saved, all classes are formatted in standard way.
#'
#' `$min_fields()` returns the minimum fields required for this class.
#'     If no required, `0` is returned.
#'
#' `$num_fields()` returns current total number of fields in this class. This
#'     number may change if the class is extensible and after
#'     `$add_extensible_group()` or `$del_extensible_group()`.
#'
#' `$memo()` returns memo of this class. Usually a brief description of this
#'     class.
#'
#' `$num_extensible()` returns the number of extensible fields in this class. If
#'     not zero, it means that objects in this class is dynamically extensible.
#'
#' `$first_extensible_index()` returns the field index of the first extensible
#'     field in this class. If this class is not extensible, `0` is return.
#'
#' `$extensible_group_num()` returns the number of extensible groups in this
#'     class.
#'
#' `$add_extensible_groups()` adds extensible groups in this class.
#'
#' `$del_extensible_groups()` deletes extensible groups in this class.
#'
#' `$has_name()` return `TRUE` if this class has name attribute.
#'
#' `$is_unique()` return `TRUE` if this class is unique.
#'
#' `$is_required()` returns `TRUE` if this class is required.
#'
#' `$is_extensible()` returns `TRUE` if this class is extensible.
#'
#' `$field_name()` returns names of fields specified by field indexes.  If `index`
#'     is `NULL`, names of all fields in this class are returned. If `lower` is
#'     `TRUE`, "lower-style" names are returned, i.e. all spaces and dashes is
#'     replaced by underscores. "lower-style" names are useful when use them as
#'     filed names in `$set_value()` in `IdfObject` class and `$set_object()` in
#'     `Idf` class. If `unit` is `TRUE`, the units of those fields are also
#'     returned.  If `in_ip`, corresponding imperial units are returned. It only
#'     has effect when `unit` is `TRUE`.
#'
#' `$field_index()` returns indexes of fields specified by field names. If `name`
#'     is `NULL`, indexes of all fields in this class are returned.
#'
#' All other `$field_*()` returns specific field properties. If `which` is `NULL`,
#'     properties of all fields in this class are returned.
#'
#'   * `$field_type`(): returns field types. All possible values are
#'     `"integer"`, `"real"`, `"alpha"` (arbitrary string), `"choice"` (alpha
#'     with specific list of choices), `"object-list"` (link to a list of
#'     objects defined elsewhere), `"external-list"` (uses a special list from
#'     an external source) and `"node"` (name used in connecting HVAC
#'     components).
#'   * `$field_unit()`: returns a character vector of field units. If `in_ip` is
#'     `TRUE`, IP unites are returned.
#'   * `$field_default()`: returns a list of default values of those fields. If
#'     no defaults found, `NA`s are returned.
#'   * `$field_choice()`: returns a list of all valid choices for those fields.
#'     If no choices found, `NA`s are returned.
#'   * `$field_range()`: returns a list of ranges for those fields. Every range
#'     has four components: `minimum` (lower limit), `lower_incbounds` (`TRUE`
#'     if the lower limit should be included), `maximum` (upper limit), and
#'     `upper_incbounds` (`TRUE` if the upper limit should be included). For
#'     fields of character type, empty lists are returned. For fields of
#'     numeric types with no specified ranges, `minimum` is set to `-Inf`,
#'     `lower_incbounds` is set to FALSE, `upper` is set to `Inf`, and
#'     `upper_incbounds` is set to FALSE. The field range is printed in number
#'     interval denotation.
#'   * `$field_reference()`: returns a list of references for those fields that
#'     have the `object-list` attribute. Basically, it is a list with all
#'     possible values collected from other object fields that those fields
#'     reference.
#'   * `$field_possible()`: returns all possible values for specified fields,
#'      including auto-value (`autosize` and `autocalculate`), and results from
#'      `$field_default()`, `$field_range()`, `$field_choice()` and
#'      `$field_reference()`. Underneath, it returns a data.table with custom
#'      printing method.
#'
#' **NOTE**: `$field_reference()` and `$field_possible()` can only be used in
#' `IddObject`s that are created using `$definition()` in [Idf] class and
#' [IdfObject] class, and cannot be used in `IddObject`s that are
#' created using `$object()` or equivalent in [Idd] class. This is
#' because both methods need shared Idf value data to collect all reference
#' values.
#'
#' `$is_valid_field_num()` returns `TRUE` if `num` is acceptable as a total number
#' of fields in this class. Extensible property is considered. For instance, the
#' total number of fields defined in IDD for class `BuildingSurfaces:Detailed`
#' is 390. However, 396 is still a valid field number for this class as the
#' number of field in the extensible group is 3.
#'
#' `$is_valid_field_name()` returns `TRUE` if `name` is a valid field name
#'     **WITHOUT** unit.
#'
#' `$is_valid_field_index()` returns `TRUE` if `index` is a valid field index.
#'
#' `$is_autosizable_field()` returns `TRUE` if the field can be assigned to
#'     `autosize`.
#'
#' `$is_autocalculatable_field()` returns `TRUE` if the field can be assigned to
#'     `autocalculate`.
#'
#' `$is_numeric_field()` returns `TRUE` if the field value should be numeric (
#'     an integer or a real number).
#'
#' `$is_integer_field()` returns `TRUE` if the field value should be an integer.
#'
#' `$is_real_field()` returns `TRUE` if the field value should be a real number.
#'
#' `$is_required_field()` returns `TRUE` if the field is required.
#'
#' `$print()` prints the IddObject. Basically, the print output can be divided
#'     into four parts:
#'
#'     * CLASS: IDD class name of current object
#'     * MEMO: brief description of the IDD class
#'     * PROPERTY: properties of the IDD class, including name of group it
#'       belongs to, whether it is an unique or required class and current total
#'       fields. The fields may increase if the IDD class is extensible, such as
#'       `Branch`, `ZoneList` and etc.
#'     * FIELDS: fields of current IDD class. Required fields are marked with
#'       bullet marks. If the class is extensible, only the first extensible
#'       group will be printed and two ellipses will be shown at the bottom.
#'       Fields in the extensible group will be marked with an arrow down
#'       surrounded by angle brackets.
#'
#' @examples
#' # get a parent Idd object
#' idd <- use_idd(8.8, download = "auto")
#'
#' # get an IddObject of class "Material"
#' mat <- idd$Material
#'
#' # get name of IDD group it belongs to
#' mat$group_name()
#'
#' # get index of IDD group it belongs to
#' mat$group_index()
#'
#' # get name of current IDD class
#' mat$class_name()
#'
#' # get index of class IDD class
#' mat$class_name()
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
#'
#' # get current number of fields
#' bran$num_fields()
#'
#' # add ten extensible groups
#' bran$add_extensible_group(10)
#' # the number of fields has been increased by 10 * 4 (= 46)
#' bran$num_fields()
#'
#' # delete eight extensible groups
#' bran$del_extensible_group(8)
#' # the number of fields has been decreased by 8 * 4 (= 32)
#' bran$num_fields()
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
#' # list all field names without units
#' mat$field_name()
#'
#' # list all field names in lower-style
#' # useful when used as field names in "$set_value()" in IdfObject class
#' # and "$set_object()" in Idf class.
#' mat$field_name(lower = TRUE)
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
#' # get all possible values of fields
#' \dontrun{mat$field_possible()}
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
#' # get all field referneces
#' \dontrun{bran$field_reference(1:4)}
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
#' # check if fields are required, i.e. field values should not be empty
#' mat$is_required_field(c("name", "roughness", "solar_absorptance"))
#'
#' @docType class
#' @name IddObject
#' @seealso [Idd] Class
#' @author Hongyuan Jia
NULL

# IddObject {{{
IddObject <- R6::R6Class(classname = "IddObject",

    public = list(
        # INITIALIZE {{{
        initialize = function (class) {
            if (is.null(private$m_uuid) || is.null(private$m_version) || is.null(private$m_idd_tbl)) {
                stop("IddObject can only be created after a parent Idd object ",
                    "has been initialized.", call. = FALSE)
            }

            assert_that(is_string(class))

            id <- private$m_idd_tbl$class[class_name == class, class_id]
            if (is_empty(id)) {
                stop("Failed to create IddObject. Invalid class name found: ",
                    surround(class), ".", call. = FALSE)
            }
            private$m_class_id <- id
        },
        # }}}

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
        field_name = function (index = NULL, lower = FALSE, unit = FALSE, in_ip = eplusr_option("view_in_ip"))
            iddobj_field_name(self, private, index, lower, unit, in_ip),

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

        field_reference = function (which = NULL)
            iddobj_field_reference(self, private, which),

        field_possible = function (which = NULL)
            iddobj_field_possible(self, private, which),
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
        # }}}

        print = function ()
            iddobj_print(self, private)
    ),

    private = list(
        # PRIVATE FIELDS {{{
        # shared data
        m_uuid = NULL,
        m_version = NULL,
        m_idd_tbl = NULL,
        m_idf_tbl = NULL,

        # self data
        m_class_id = NULL,

        # self reference
        m_iddobj_gen = NULL
        # }}}
    )
)
# }}}

# iddobj_group_index {{{
iddobj_group_index <- function (self, private) {
    private$m_idd_tbl$class[class_id == private$m_class_id, group_id]
}
# }}}
# iddobj_group_name {{{
iddobj_group_name <- function (self, private) {
    grp_id <- iddobj_group_index(self, private)
    private$m_idd_tbl$group[J(grp_id), on = "group_id", group_name]
}
# }}}
# iddobj_class_index {{{
iddobj_class_index <- function (self, private) {
    private$m_class_id
}
# }}}
# iddobj_class_name {{{
iddobj_class_name <- function (self, private) {
    private$m_idd_tbl$class[J(private$m_class_id), on = "class_id", class_name]
}
# }}}
# iddobj_class_data {{{
iddobj_class_data <- function (self, private) {
    private$m_idd_tbl$class[class_id == private$m_class_id]
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
    assert_that(is_count(num))

    private$m_idd_tbl <- t_add_extensible_group(private$m_idd_tbl, private$m_class_id, num, strict = TRUE)

    verbose_info(num, " extensible group(s) added")

    self
}
# }}}
# iddobj_del_extensible_group {{{
iddobj_del_extensible_group <- function (self, private, num) {
    assert_that(is_count(num))

    private$m_idd_tbl <- t_del_extensible_group(private$m_idd_tbl, private$m_class_id, num, strict = TRUE)

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
iddobj_field_data <- function (self, private, which = NULL, cols = NULL, min = FALSE, no_ext = TRUE) {
    t_field_data(private$m_idd_tbl, private$m_class_id, which, cols, min, no_ext)
}
# }}}
# iddobj_field_name {{{
iddobj_field_name <- function (self, private, index = NULL, lower = FALSE,
                               unit = FALSE, in_ip = eplusr_option("view_in_ip")) {
    fld <- iddobj_field_data(self, private, index, c("field_name", "full_name", "full_ipname"))

    if (unit) {
        if (in_ip)
            res <- fld$full_ipname
        else
            res <- fld$full_name
    } else {
        res <- fld$field_name
    }

    if (lower)
        lower_name(res)
    else
        res
}
# }}}
# iddobj_field_index {{{
iddobj_field_index <- function (self, private, name = NULL) {
    iddobj_field_data(self, private, name, "field_index")$field_index
}
# }}}
# iddobj_field_type {{{
iddobj_field_type <- function (self, private, which = NULL) {
    iddobj_field_data(self, private, which, "type")$type
}
# }}}
# iddobj_field_note {{{
iddobj_field_note <- function (self, private, which = NULL) {
    iddobj_field_data(self, private, which, "note")$note
}
# }}}
# iddobj_field_unit {{{
iddobj_field_unit <- function (self, private, which = NULL, in_ip = eplusr_option("view_in_ip")) {
    fld <- iddobj_field_data(self, private, which, c("units", "ip_units"))

    if (in_ip) {
        fld$ip_units
    } else {
        fld$units
    }
}
# }}}
# iddobj_field_default {{{
iddobj_field_default <- function (self, private, which = NULL, in_ip = eplusr_option("view_in_ip")) {
    fld <- iddobj_field_data(self, private, which,
        c("field_id", "type_enum", "default", "units", "ip_units")
    )

    unit_to <- ifelse(in_ip, "ip", "si")
    fld <- t_field_default_to_unit(fld, from = "si", to = unit_to)

    fld$default
}
# }}}
# iddobj_field_choice {{{
iddobj_field_choice <- function (self, private, which = NULL) {
    iddobj_field_data(self, private, which, "choice")$choice
}
# }}}
# iddobj_field_range {{{
iddobj_field_range <- function (self, private, which = NULL) {
    fld <- iddobj_field_data(self, private, which,
        c("field_id", "minimum", "lower_incbounds", "maximum", "upper_incbounds")
    )

    fld[, `:=`(range = list(make_field_range(minimum, lower_incbounds, maximum, upper_incbounds))), by = field_id]

    fld$range
}
# }}}
# iddobj_field_reference {{{
iddobj_field_reference <- function (self, private, which = NULL) {
    if (is.null(private$m_idf_tbl)) {
        mes <- paste0("Function can only be used in IddObjects that are created ",
            "inside an Idf or IdfObject using `$definition()`.")
        abort("error_field_reference", mes, class_id = private$m_class_id)
    }

    fld <- iddobj_field_data(self, private, which, c("field_id", "type_enum"))

    t_field_reference(fld, private$m_idd_tbl$reference, private$m_idf_tbl$value)$src_value
}
# }}}
# iddobj_field_possible {{{
iddobj_field_possible <- function (self, private, which = NULL, in_ip = eplusr_option("view_in_ip")) {
    if (is.null(private$m_idf_tbl)) {
        mes <- paste0("Function can only be used in IddObjects that are created ",
            "inside an Idf or IdfObject using `$definition()`.")
        abort("error_field_possible", mes, class_id = private$m_class_id)
    }

    fld <- iddobj_field_data(self, private, which,
        c("class_id", "class_name", "field_index",
          "field_id", "type_enum",
          "autosizable", "autocalculatable",
          "default", "units", "ip_units",
          "minimum", "lower_incbounds", "maximum", "upper_incbounds")
    )

    # auto fields
    set(fld, NULL, "auto", NA_character_)
    fld[autosizable == TRUE, `:=`(auto = "Autosize")]
    fld[autocalculatable == TRUE, `:=`(auto = "Autocalculate")]

    # default
    unit_to <- ifelse(in_ip, "ip", "si")
    fld <- t_field_default_to_unit(fld, from = "si", to = unit_to)

    # range
    fld[, `:=`(range = make_field_range(minimum, lower_incbounds, maximum, upper_incbounds)), by = field_id]

    # reference
    fld <- t_field_reference(fld, private$m_idd_tbl_reference, private$m_idf_tbl$value)

    res <- fld[, list(class_id, class_name, field_index, field_name, auto, default, choice, range, reference = src_value)]

    data.table::setattr(res, "class", c("IddFieldPossible", class(res)))
    res
}
# }}}
# iddobj_is_valid_field_num {{{
iddobj_is_valid_field_num <- function (self, private, num) {
    assert_that(are_count(num))

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
    assert_that(are_count(index))

    cls <- iddobj_class_data(self, private)

    if (!cls$num_extensible) return(rep(FALSE, length(index)))

    index >= cls$first_extensible
}
# }}}
# iddobj_is_valid_field_name {{{
iddobj_is_valid_field_name <- function (self, private, name, strict = FALSE) {
    fld <- iddobj_field_data(self, private, cols = "field_name")

    if (isTRUE(strict)) {
        name %in% fld$field_name
    } else {
        name %in% fld$field_name | name %in% lower_name(fld$field_name)
    }
}
# }}}
# iddobj_is_valid_field_index {{{
iddobj_is_valid_field_index <- function (self, private, index) {
    assert_that(are_count(index))
    index <= iddobj_class_data(self, private)$num_fields
}
# }}}
# iddobj_is_autosizable_field {{{
iddobj_is_autosizable_field <- function (self, private, which) {
    iddobj_field_data(self, private, which, "autosizable")$autosizable
}
# }}}
# iddobj_is_autocalculatable_field {{{
iddobj_is_autocalculatable_field <- function (self, private, which) {
    iddobj_field_data(self, private, which, "autocalculatable")$autocalculatable
}
# }}}
# iddobj_is_numeric_field {{{
iddobj_is_numeric_field <- function (self, private, which) {
    iddobj_field_type(self, private, which) %in% c("integer", "real")
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
    iddobj_field_data(self, private, which, "required_field")$required_field
}
# }}}
# iddobj_print {{{
iddobj_print <- function (self, private) {
    # CLASS {{{
    cls <- iddobj_class_data(self, private)
    cli::cat_line(crayon::bold$underline(paste0(
            "IddObject <<Class: ", surround(cls$class_name), ">>")),
        col = "inverse")

    # memo {{{
    cli::cat_rule(center = crayon::bold("* MEMO *"), col = "green")
    if (is.null(cls$memo[[1L]])) {
        cli::cat_line("  ", crayon::italic("<No Memo>"), col = "cyan")
    } else {
        cli::cat_line("  \"", crayon::italic(paste0(cls$memo[[1L]], collapse = "\n")), "\"", col = "cyan")
    }
    # }}}

    # property {{{
    cli::cat_rule(center = crayon::bold("* PROPERTIES *"), col = "green")

    grp <- private$m_idd_tbl$group[J(cls$group_id), on = "group_id", group_name]
    cli::cat_line("   ", cli::symbol$bullet, " ", c(
        paste0(crayon::bold("Group: "), surround(grp)),
        paste0(crayon::bold("Unique: "), cls$unique_object),
        paste0(crayon::bold("Required: "), cls$required_object),
        paste0(crayon::bold("Total fields: "), cls$num_fields)
    ), col = "cyan")
    # }}}
    # }}}

    # FIELD {{{
    cli::cat_rule(center = crayon::bold("* FIELDS *"), col = "green")

    # calculate number of fields to print
    if (cls$num_extensible) {
        cls[, `:=`(last_extensible = first_extensible + num_extensible - 1L)]
        cls[, `:=`(num_print = max(last_required, last_extensible))]
    } else {
        cls[, `:=`(num_print = num_fields, last_extensible = 0L)]
    }

    col_nm <- ifelse(eplusr_option("view_in_ip"), "full_ipname", "full_name")
    fld <- iddobj_field_data(self, private, seq_len(cls$num_print),
        c("field_index", "required_field", "extensible_group", col_nm)
    )
    setnames(fld, col_nm, "full_name")

    fld[, `:=`(idx = lpad(field_index), ext = "")]

    fld[extensible_group > 0L,
        `:=`(ext = crayon::bold$yellow(paste0(" <", cli::symbol$arrow_down, ">")))]

    fld[required_field == TRUE, `:=`(
        idx = crayon::red$bold(idx),
        req = crayon::red$bold(cli::symbol$bullet),
        full_name = crayon::red$bold(full_name)
    )]
    fld[required_field == FALSE, `:=`(
        idx = crayon::cyan(idx),
        req = crayon::cyan(strrep(" ", nchar(cli::symbol$bullet))),
        full_name = crayon::cyan(full_name)
    )]
    fld[, cli::cat_line("  ", req, idx, ": ", full_name, ext)]

    if (cls$num_extensible) cli::cat_line("  ......", col = "cyan")
    cli::cat_rule(col = "green")
    # }}}
}
# }}}
