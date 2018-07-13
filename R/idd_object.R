#' EnergyPlus IDD object
#'
#' `IddObject` is an abstraction of a single object in an `Idd`. It provides
#' more detail methods to query field properties. `IddObject` can only be
#' created from the parent `Idd` object, using `$object` and `$object_in_group`.
#' This is because that initialization of an `IddObject` needs
#' some shared data from parent `Idd` object.
#'
#' @section Usage:
#' ```
#' # class properties
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
#'
#' # extensible group
#' iddobj$add_extensible_group(num = 1L)
#' iddobj$del_extensible_group(num = 1L)
#'
#' # class property assertions
#' iddobj$has_name()
#' iddobj$is_required()
#' iddobj$is_unique()
#' iddobj$is_extensible()
#'
#' # field properties
#' iddobj$field_name(index = NULL, lower = FALSE, unit = FALSE, in_ip = getOption("eplusr.view_in_ip"))
#' iddobj$field_index(name = NULL)
#' iddobj$field_type(which = NULL)
#' iddobj$field_note(which = NULL)
#' iddobj$field_unit(which = NULL, in_ip = getOption("eplusr.view_in_ip")
#' iddobj$field_default(which = NULL, in_ip = getOption("eplusr.view_in_ip")
#' iddobj$field_choice(which = NULL)
#' iddobj$field_range(which = NULL)
#'
#' # field property assertions
#' iddobj$is_valid_field_num(num)
#' iddobj$is_extensible_index(index)
#' iddobj$is_valid_field_name(name)
#' iddobj$is_valid_field_index(which)
#' iddobj$is_autosizable_field(which = NULL)
#' iddobj$is_autocalculatable_field(which = NULL)
#' iddobj$is_numeric_field(which = NULL)
#' iddobj$is_integer_field(which = NULL)
#' iddobj$is_required_field(which = NULL)
#'
#' iddobj$print()
#' print(iddobj)
#' ```
#'
#' @section Arguments:
#'
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
#'
#' @section Detail:
#'
#' `$group_index` returns the index of group this class belongs to.
#'
#' `$group_name` returns the name of group this class belongs to.
#'
#' `$class_index` returns the index of this class.
#'
#' `$class_name` returns the name of this class.
#'
#' `$min_fields` returns the minimum fields required for this class.
#'     If no required, `0` is returned.
#'
#' `$num_fields` returns current total number of fields in this class. This
#'     number may change if the class is extensible and after
#'     `$add_extensible_group` or `$del_extensible_group`.
#'
#' `$memo` returns memo of this class.
#'
#' `$num_extensible` returns the number of extensible fields in this class.
#'
#' `$first_extensible_index` returns the field index of the first extensible
#'     field in this class. If this class is not extensible, `0` is return.
#'
#' `$extensible_group_num` returns the number of extensible groups in this
#'     class.
#'
#' `$add_extensible_groups` adds extensible groups in this class.
#'
#' `$del_extensible_groups` deletes extensible groups in this class.
#'
#' `$has_name` return `TRUE` if this class has name attribute.
#'
#' `$is_unique` return `TRUE` if this class is unique.
#'
#' `$is_required` returns `TRUE` if this class is required.
#'
#' `$is_extensible` returns `TRUE` if this class is extensible.
#'
#' `$field_name` returns names of fields specified by field indexes.  If `index`
#'     is `NULL`, names of all fields in this class are returned. If `lower` is
#'     `TRUE`, "lower-style" names are returned, i.e. all spaces and dashes is
#'     replaced by underscores. If `unit` is `TRUE`, the units of those fields
#'     are also returned.  If `in_ip`, corresponding imperial units are
#'     returned. It only has effect when `unit` is `TRUE`.
#'
#' `$field_index` returns indexes of fields specified by field names. If `name`
#'     is `NULL`, indexes of all fields in this class are returned.
#'
#' All other `$field_*` returns specific field properties. If `which` is `NULL`,
#'     properties of all fields in this class are returned.
#'
#'   * `$field_type`: returns field types. All possible values are
#'     `"integer"`, `"real"`, `"alpha"` (arbitrary string), `"choice"` (alpha
#'     with specific list of choices), `"object-list"` (link to a list of
#'     objects defined elsewhere), `"external-list"` (uses a special list from
#'     an external source) and `"node"` (name used in connecting HVAC
#'     components).
#'   * `$field_unit`: returns a character vector of field units. If `in_ip` is
#'     `TRUE`, IP unites are returned.
#'   * `$field_default`: returns a list of default values of those fields. If
#'     no defaults found, `NA`s are returned.
#'   * `$field_choice`: returns a list of all valid choices for those fields.
#'     If no choices found, `NA`s are returned.
#'   * `$field_range`: returns a list of ranges for those fields. Every range
#'     has four components: `minimum` (lower limit), `lower_incbounds` (`TRUE`
#'     if the lower limit should be included), `maximum` (upper limit), and
#'     `upper_incbounds` (`TRUE` if the upper limit should be included). For
#'     fields of character type, empty lists are returned. For fields of
#'     numeric types with no specified ranges, `minimum` is set to `-Inf`,
#'     `lower_incbounds` is set to FALSE, `upper` is set to `Inf`, and
#'     `upper_incbounds` is set to FALSE.
#'
#' `$is_valid_field_num` returns `TRUE` if `num` is acceptable as a total number
#' of fields in this class. Extensible property is considered. For instance, The
#' total number of fields defined in IDD for class `BuildingSurfaces:Detailed`
#' is 390. However, 396 is still a valid field number for this class as the
#' number of field in the extensible group is 3.
#'
#' `$is_valid_field_name` returns `TRUE` if `name` is a valid field name
#'     **WITHOUT** unit.
#'
#' `$is_valid_field_index` returns `TRUE` if `index` is a valid field index.
#'
#' `$is_autosizable_field` returns `TRUE` if the field can be assigned to
#'     `autosize`.
#'
#' `$is_autocalculatable_field` returns `TRUE` if the field can be assigned to
#'     `autocalculate`.
#'
#' `$is_numeric_field` returns `TRUE` if the field value should be numeric.
#'
#' `$is_integer_field` returns `TRUE` if the field value should be an integer.
#'
#' `$is_required_field` returns `TRUE` if the field is required.
#'
#' @docType class
#' @name idd_object
#' @seealso [Idd Class][idd]
#' @author Hongyuan Jia
NULL

#' @importFrom R6 R6Class
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
                    backtick(class), ".", call. = FALSE)
            }
            private$m_class_id <- id
        },
        # }}}

        # CLASS PROPERTY GETTERS {{{
        group_name = function ()
            i_from_group(self, private, private$m_class_id),

        group_index = function ()
            i_group_index(self, private, self$group_name()),

        class_name = function ()
            i_class_tbl_from_which(self, private, private$m_class_id)$class_name,

        class_index = function ()
            private$m_class_id,

        class_format = function ()
            i_class_tbl_from_which(self, private, private$m_class_id)$class_format,

        min_fields = function ()
            i_class_tbl_from_which(self, private, private$m_class_id)$min_fields,

        num_fields = function ()
            i_class_tbl_from_which(self, private, private$m_class_id)$num_fields,

        memo = function ()
            i_class_tbl_from_which(self, private, private$m_class_id)$memo,

        num_extensible = function ()
            i_class_tbl_from_which(self, private, private$m_class_id)$num_extensible,

        first_extensible_index = function ()
            i_class_tbl_from_which(self, private, private$m_class_id)$first_extensible,

        extensible_group_num = function ()
            i_class_tbl_from_which(self, private, private$m_class_id)$num_extensible_group,
        # }}}

        # EXTENSIBLE GROUP {{{
        add_extensible_group = function (num = 1L)
            i_add_extensible_group(self, private, private$m_class_id, num),

        del_extensible_group = function (num = 1L)
            i_del_extensible_group(self, private, private$m_class_id, num),
        # }}}

        # CLASS PROPERTY ASSERTIONS {{{
        has_name = function ()
            i_class_tbl_from_which(self, private, private$m_class_id)$has_name,

        is_required = function ()
            i_class_tbl_from_which(self, private, private$m_class_id)$required_object,

        is_unique = function ()
            i_class_tbl_from_which(self, private, private$m_class_id)$unique_object,

        is_extensible = function ()
            i_class_tbl_from_which(self, private, private$m_class_id)$num_extensible > 0,
        # }}}

        # FIELD PROPERTY GETTERS {{{
        field_name = function (index = NULL, lower = FALSE, unit = FALSE, in_ip = getOption("eplusr.view_in_ip"))
            i_field_name(self, private, private$m_class_id, index, lower, unit, in_ip),

        field_index = function (name = NULL)
            i_field_index(self, private, private$m_class_id, name),

        field_type = function (which = NULL)
            i_field_tbl_from_which(self, private, private$m_class_id, which = which)$type,

        field_note = function (which = NULL)
            i_field_tbl_from_which(self, private, private$m_class_id, which = which)$note,

        field_unit = function (which = NULL, in_ip = getOption("eplusr.view_in_ip"))
            i_field_unit(self, private, private$m_class_id, which = which),

        field_default = function (which = NULL, in_ip = getOption("eplusr.view_in_ip"))
            i_field_default(self, private, private$m_class_id, which = which),

        field_choice = function (which = NULL)
            i_field_choice(self, private, private$m_class_id, which = which),

        # TODO: add IP range
        field_range = function (which = NULL)
            i_field_range(self, private, private$m_class_id, which = which),
        # }}}

        # FIELD PROPERTY ASSERTIONS {{{
        is_valid_field_num = function (num)
            i_is_valid_field_num(self, private, private$m_class_id, num),

        is_extensible_index = function (index)
            i_is_extensible_index(self, private, private$m_class_id, index),

        is_valid_field_name = function (name)
            i_is_valid_field_name(self, private, private$m_class_id, name),

        is_valid_field_index = function (index)
            i_is_valid_field_index(self, private, private$m_class_id, index),

        is_autosizable_field = function (which = NULL)
            i_field_tbl_from_which(self, private, private$m_class_id, which = which)$autosizable,

        is_autocalculatable_field = function (which = NULL)
            i_field_tbl_from_which(self, private, private$m_class_id, which = which)$autocalculatable,

        is_numeric_field = function (which = NULL)
            i_field_tbl_from_which(self, private, private$m_class_id, which = which)$type %in% c("integer", "real"),

        is_integer_field = function (which = NULL)
            i_field_tbl_from_which(self, private, private$m_class_id, which = which)$type == "integer",

        is_required_field = function (which = NULL)
            i_field_tbl_from_which(self, private, private$m_class_id, which = which)$required_field,
        # }}}

        print = function ()
            i_print_iddobj(self, private)
    ),

    private = list(
        # PRIVATE FIELDS {{{
        # shared data
        m_uuid = NULL,
        m_version = NULL,
        m_idd_tbl = NULL,

        # self data
        m_class_id = NULL,

        # self reference
        m_iddobj_generator = NULL
        # }}}
    )
)
# }}}
