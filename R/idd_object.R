#' EnergyPlus IDD objects
#'
#' \code{IDDObject} is a R6 class used internally as members in \code{IDD} R6
#' class. \code{IDDObject} is not entended to be used seperately.
#'
#' @section Usage:
#' ```
#'
#' iddobj <- IDDObject$new(list)
#'
#' iddobj$group_name()
#' iddobj$group_index()
#' iddobj$class_name()
#' iddobj$class_index()
#' iddobj$class_format()
#' iddobj$min_fields()
#' iddobj$num_fields()
#' iddobj$memo()
#' iddobj$num_extensible()
#' iddobj$reference_class_name()
#' iddobj$first_extensible()
#' iddobj$extensible_group()
#' iddobj$add_extensible_groups(num = 1L)
#'
#' iddobj$is_version()
#' iddobj$is_required()
#' iddobj$is_unique()
#' iddobj$is_extensible()
#' iddobj$has_name()
#'
#' iddobj$field_name(which = NULL, lower = FALSE, unit = FALSE, in_ip = FALSE)
#' iddobj$field_which(name = NULL)
#' iddobj$field_type(which = NULL, name = NULL)
#' iddobj$field_unit(which = NULL, name = NULL, in_ip = FALSE)
#' iddobj$field_reference(which = NULL, name = NULL)
#' iddobj$field_object_list(which = NULL, name = NULL)
#' iddobj$field_default(which = NULL, name = NULL)
#' iddobj$field_choice(which = NULL, name = NULL)
#' iddobj$field_range(which = NULL, name = NULL)
#'
#' iddobj$is_valid_field_num(num)
#' iddobj$is_valid_field_name(name)
#' iddobj$is_valid_field_index(which)
#' iddobj$is_autosizable_field(which = NULL, name = NULL)
#' iddobj$is_autocalculatable_field(which = NULL, name = NULL)
#' iddobj$is_numeric_field(which = NULL, name = NULL)
#' iddobj$is_integer_field(which = NULL, name = NULL)
#' iddobj$is_required_field(which = NULL, name = NULL)
#'
#' iddobj$print()
#'
#' print(iddobj)
#'
#' ```
#'
#' @section Arguments:
#'
#' * `list`: A subset of list with two named members `class` and `field`.
#' Usually a subset of `data` list that created by `parse_idd` function.
#'
#' * `num`: A positive integer.
#'
#' * `which`: A valid field which.
#'
#' * `name`: A valid field name.
#'
#' @section Detail:
#'
#' `IDDObject$new()` creates an IDDObject using parsed EnergyPlus Input Data
#' Dictionary (IDD) data of a certain class.
#'
#' `$group_name()` returns the name of group this class belongs to.
#'
#' `$group_index()` returns the index of group this class belongs to.
#'
#' `$class_name()` returns the name of this class.
#'
#' `$class_index()` returns the index of this class.
#'
#' `$min_fields()` returns the minimum fields required for this class.
#' If no required, 0 is returned.
#'
#' `$num_fields()` returns current total number of fields in this class. This
#' number may change if the class is extensible and after
#' `$add_extensible_groups()` is run.
#'
#' `$memo()` returns memo of this class.
#'
#' `$num_extensible()` returns the number of extensible fields of this class.
#'
#' `$reference_class_name()` returns the name of this class that is referenced
#' by other class or objects.
#'
#' `$first_extensible()` returns the which of the first extensible field in this
#' class. If this class is not extensible, 0 is return.
#'
#' `$extensible_group()` returns the data of extensible group for this class.
#'
#' `$add_extensible_groups(num)` adds `num` of extensible groups in this class.
#'
#' `$is_version()` returns `TRUE` if this class is `Version`
#'
#' `$is_required()` returns `TRUE` if this class is required.
#'
#' `$is_extensible()` returns `TRUE` if this class is extensible.
#'
#' `$field_name(which, lower, unit, in_ip)` returns field names of that `which`
#' or those indice. If `which` is NULL, names of all fields in this class are
#' returned. If `lower` is `TRUE`, all spaces and dashes is replaced by
#' underscores. If `unit` is `TRUE`, the units of those fields are also
#' returned.  If `in_ip`, corresponding imperial units are returned. It only has
#' effect when `unit` is `TRUE`.
#'
#' `$field_which(name)` returns field which of that fields or indice of those
#' fields with `name`(s). If `name` is NULL, indice of all fields in this class
#' are returned.
#'
#' All other `$field_*(which, name)` returns specific field properties. If both
#' `which` and `name` are given, `name` is ignored. If both `which` and `name`
#' are NULL, properties of all fields in this class are returned.
#'
#' * `$field_type(which, name)`: returns types of those fields. All possible
#' values are `"integer"`, `"real"`, `"alpha"` (arbitrary string), `"choice"`
#' (alpha with specific list of choices), `"object-list"` (link to a list of
#' objects defined elsewhere), `"external-list"` (uses a special list from an
#' external source) and `"node"` (name used in connecting HVAC components).
#'
#' * `$field_unit(which, name)`: returns units of those fields.
#'
#' * `$field_reference(which, name)`: returns references of those fields which
#' are alternative field names that are referenced by other fields elsewhere.
#'
#' * `$field_object_list(which, name)`: returns a list of other field names that
#' this field referenced.
#'
#' * `$field_default(which, name)`: returns a list of default values of those
#' fields. If no defaults found, `NA`s are returned.
#'
#' * `$field_choice(which, name)`: returns a list of all valid choices for those
#' fields. If no choices found, `NA`s are returned.
#'
#' * `$field_range(which, name)`: returns a list of ranges for those fields.
#' Every range has four components: `lower` (lower limit), `lower_incbounds`
#' (`TRUE` if the lower limit should be included), `upper` (upper limit), and
#' `upper_incbounds` (`TRUE` if the upper limit should be included). For fields
#' of character type, empty lists are returned. For fields of numeric types with
#' no specified ranges, `lower` is set to `-Inf`, `lower_incbounds` is set to
#' FALSE, `upper` is set to `Inf`, and `upper_incbounds` is set to FALSE. The
#' names of each components of returned list are ranges of those fields
#' expressed in brackets, e.g. (-Inf, 100].
#'
#' `$is_valid_field_num(num)` returns `TRUE` if `num` is acceptable as a total
#' number of fields in this class. Extensible property is considered. For
#' instance, The total number of fields defined in IDD for class
#' `BuildingSurfaces:Detailed` is 390. However, 396 is still a valid field
#' number for this class as the number of field in the extensible group is 3.
#'
#' `$is_valid_field_name(name)` returns `TRUE` if `name` is a valid field name
#' **WITHOUT** unit.
#'
#' `$is_valid_field_index(which)` returns `TRUE` if `which` is a valid field
#' which.
#'
#' `$is_autosizable_field(which, name)` returns `TRUE` if the field can be
#' assigned to `autosize`.
#'
#' `$is_autocalculatable_field(which, name)` returns `TRUE` if the field can be
#' assigned to `autocalculate`.
#'
#' `$is_numeric_field(which, name)` returns `TRUE` if the field value should be
#' numeric.
#'
#' `$is_integer_field(which, name)` returns `TRUE` if the field value should be
#' an integer.
#'
#' `$is_required_field(which, name)` returns `TRUE` if the field is required.
#'
#' @return An IDDObject object
#' @docType class
#' @name idd_object
#' @author Hongyuan Jia
NULL

#' @importFrom R6 R6Class
#' @importFrom glue glue
#' @importFrom data.table data.table rbindlist copy set setattr between
#' @importFrom stringr str_replace_all
#' @importFrom cli cat_line cat_rule
#' @importFrom assertthat assert_that
#' @importFrom clisymbols symbol
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
