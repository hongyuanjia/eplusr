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
#' iddobj$group_order()
#' iddobj$class_name()
#' iddobj$class_order()
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
#' iddobj$field_name(index = NULL, lower = FALSE, unit = FALSE, in_ip = FALSE)
#' iddobj$field_index(name = NULL)
#' iddobj$field_type(index = NULL, name = NULL)
#' iddobj$field_unit(index = NULL, name = NULL, in_ip = FALSE)
#' iddobj$field_reference(index = NULL, name = NULL)
#' iddobj$field_object_list(index = NULL, name = NULL)
#' iddobj$field_default(index = NULL, name = NULL)
#' iddobj$field_choice(index = NULL, name = NULL)
#' iddobj$field_range(index = NULL, name = NULL)
#'
#' iddobj$is_valid_field_num(num)
#' iddobj$is_valid_field_name(name)
#' iddobj$is_valid_field_index(index)
#' iddobj$is_autosizable_field(index = NULL, name = NULL)
#' iddobj$is_autocalculatable_field(index = NULL, name = NULL)
#' iddobj$is_numeric_field(index = NULL, name = NULL)
#' iddobj$is_integer_field(index = NULL, name = NULL)
#' iddobj$is_required_field(index = NULL, name = NULL)
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
#' * `index`: A valid field index.
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
#' `$group_order()` returns the order of group this class belongs to.
#'
#' `$class_name()` returns the name of this class.
#'
#' `$class_order()` returns the order of this class.
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
#' `$first_extensible()` returns the index of the first extensible field in this
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
#' `$field_name(index, lower, unit, in_ip)` returns field names of that `index`
#' or those indice. If `index` is NULL, names of all fields in this class are
#' returned. If `lower` is `TRUE`, all spaces and dashes is replaced by
#' underscores. If `unit` is `TRUE`, the units of those fields are also
#' returned.  If `in_ip`, corresponding imperial units are returned. It only has
#' effect when `unit` is `TRUE`.
#'
#' `$field_index(name)` returns field index of that fields or indice of those
#' fields with `name`(s). If `name` is NULL, indice of all fields in this class
#' are returned.
#'
#' All other `$field_*(index, name)` returns specific field properties. If both
#' `index` and `name` are given, `name` is ignored. If both `index` and `name`
#' are NULL, properties of all fields in this class are returned.
#'
#' * `$field_type(index, name)`: returns types of those fields. All possible
#' values are `"integer"`, `"real"`, `"alpha"` (arbitrary string), `"choice"`
#' (alpha with specific list of choices), `"object-list"` (link to a list of
#' objects defined elsewhere), `"external-list"` (uses a special list from an
#' external source) and `"node"` (name used in connecting HVAC components).
#'
#' * `$field_unit(index, name)`: returns units of those fields.
#'
#' * `$field_reference(index, name)`: returns references of those fields which
#' are alternative field names that are referenced by other fields elsewhere.
#'
#' * `$field_object_list(index, name)`: returns a list of other field names that
#' this field referenced.
#'
#' * `$field_default(index, name)`: returns a list of default values of those
#' fields. If no defaults found, `NA`s are returned.
#'
#' * `$field_choice(index, name)`: returns a list of all valid choices for those
#' fields. If no choices found, `NA`s are returned.
#'
#' * `$field_range(index, name)`: returns a list of ranges for those fields.
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
#' `$is_valid_field_index(index)` returns `TRUE` if `index` is a valid field
#' index.
#'
#' `$is_autosizable_field(index, name)` returns `TRUE` if the field can be
#' assigned to `autosize`.
#'
#' `$is_autocalculatable_field(index, name)` returns `TRUE` if the field can be
#' assigned to `autocalculate`.
#'
#' `$is_numeric_field(index, name)` returns `TRUE` if the field value should be
#' numeric.
#'
#' `$is_integer_field(index, name)` returns `TRUE` if the field value should be
#' an integer.
#'
#' `$is_required_field(index, name)` returns `TRUE` if the field is required.
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
        initialize = function (class) {
            # {{{
            assert_that(!is.null(private$m_idd_tbl),
                msg = glue::glue("IddObject can only be created after a parent \\
                Idd object has been initialized.")
            )

            assert_that(is_string(class))
            assert_that(class %in% private$m_idd_tbl$class[["class_name"]],
                msg = glue::glue("Failed to create IddObject. Invalid class \\
                name found: `{class}`.")
            )

            private$m_class_name <- class
            # }}}
        },

        ##########################
        #  PROPERTIES FUNCTIONS  #
        ##########################
        # properties getters
        # {{{
        group_name = function () {
            # return group name
            # {{{
            private$m_idd_tbl$class[class_name == private$m_class_name][
                private$m_idd_tbl$group, on = "group_id", nomatch = 0L, group_name]
            # }}}
        },

        group_order = function () {
            # return group order
            # {{{
            private$m_idd_tbl$class[class_name == private$m_class_name, group_id]
            # }}}
        },

        class_name = function () {
            # return class name
            # {{{
            private$m_class_name
            # }}}
        },

        class_order = function () {
            # return class order
            # {{{
            private$m_idd_tbl$class[class_name == private$m_class_name, class_id]
            # }}}
        },

        class_format = function () {
            # return class format
            # {{{
            private$class_tbl()[["class_format"]]
            # }}}
        },

        min_fields = function () {
            # return minimum field number requirement
            # {{{
            private$class_tbl()[["min_fields"]]
            # }}}
        },

        num_fields = function () {
            # return total number of fields in definition
            # {{{
            private$class_tbl()[["num_fields"]]
            # }}}
        },

        memo = function () {
            # return memo
            # {{{
            private$class_tbl()[["memo"]]
            # }}}
        },

        num_extensible = function () {
            # return field number of extensible fields in a extensible field
            # group
            # {{{
            private$class_tbl()[["num_extensible"]]
            # }}}
        },

        reference_class_name = function () {
            # return reference class name
            # {{{
            private$m_idd_tbl$class[class_name == private$m_class_name][
                private$m_idd_tbl$class_reference, on = "class_id", nomatch = 0L, reference]
            # }}}
        },

        first_extensible_index = function () {
            # return index of the first extensible field
            # {{{
            res <- private$m_idd_tbl$class[class_name == private$m_class_name, list(class_id)][
                private$m_idd_tbl$field, on = "class_id", nomatch = 0L, list(field_id, field_order)][
                private$m_idd_tbl$field_property, on = "field_id", nomatch = 0L][
                begin_extensible == TRUE, field_order]
            ifelse(is_empty(res), 0L, res)
            # }}}
        },

        extensible_group_index = function () {
            # return field indexes of the extensible field group
            # {{{
            res <- private$m_idd_tbl$class[class_name == private$m_class_name, list(class_id)][
                private$m_idd_tbl$field, on = "class_id", nomatch = 0L, list(field_id, field_order)][
                private$m_idd_tbl$field_extensible, on = "field_id", nomatch = 0L, list(field_id, field_order)][
                private$m_idd_tbl$field_property, on = "field_id", nomatch = 0L][
                begin_extensible == TRUE, field_order]
            # }}}
        },

        add_extensible_group = function (num = 1L) {
            # add one or more extensible groups
            # {{{
            assert_that(is_count(num))
            assert_that(self$is_extensible(), msg = glue::glue("Failed to add \\
               extensible groups. Class `{private$m_class_name}` is not \\
               extensible.")
            )

            # get the max field id
            max_id <- private$m_idd_tbl$field[, max(field_id)]
            # get field ids of first extensible group
            ext_id <- private$class_tbl()[, list(class_id)][
                private$m_idd_tbl$field, on = "class_id", nomatch = 0L, list(field_id)][
                private$m_idd_tbl$field_extensible, on = "field_id", nomatch = 0L, field_id]

            # insert new `field_order`, `full_name` and `full_ipname` values
            # into field
            ext_field <- data.table::rbindlist(
                replicate(num, private$m_idd_tbl$field[field_id %in% ext_id], simplify = FALSE)
            )

            num_ext <- self$num_extensible()
            # get new added field id
            added_field_num <- seq_len(num_ext * num)
            # get new field id
            new_field_id <- max_id + added_field_num
            # get total fields
            num_field <- self$num_fields()
            # get new field orders
            new_field_order <- num_field + added_field_num
            # get total extensible group num
            num_ext_grp <- private$num_extensible_group()
            new_ext_ord <- rep(seq_len(num) + num_ext_grp, each = num_ext)
            # combine
            new_ext_field <- data.table::copy(ext_field)[,
                `:=`(field_id = new_field_id,
                     field_order = new_field_order,
                     field_name = stringr::str_replace_all(field_name, "1", as.character(new_ext_ord)),
                     full_name = stringr::str_replace_all(full_name, "1", as.character(new_ext_ord)),
                     full_ipname = stringr::str_replace_all(full_ipname, "1", as.character(new_ext_ord)))]
            private$m_idd_tbl$field <- data.table::rbindlist(list(private$m_idd_tbl$field, new_ext_field))

            # insert new values of field property
            ext_field_property <- private$m_idd_tbl$field_property[field_id %in% ext_id]
            new_ext_field_property <- data.table::rbindlist(
                replicate(num, ext_field_property, simplify = FALSE)
            )[, `:=`(field_id = new_field_id,
                     required_field = FALSE,
                     begin_extensible = FALSE)]
            private$m_idd_tbl$field_property <- data.table::rbindlist(
                list(private$m_idd_tbl$field_property, new_ext_field_property)
            )

            # append_ext_tbl: local function to append extensible fields
            # {{{
            append_ext_tbl <- function (tbl) {
                tbl <- deparse(substitute(tbl))
                tbl_name <- paste0("field_", tbl)
                id_name <- paste0(tbl, "_id")
                # insert new values of field choice
                ext_field <- private$m_idd_tbl[[tbl_name]][field_id %in% ext_id]
                if (not_empty(ext_field)) {
                    new_ref_id <- new_field_id[rep(ext_id %in% ext_field[["field_id"]], times = num)]
                    new_self_id <- max(private$m_idd_tbl[[tbl_name]][[id_name]]) + length(new_field_id)
                    new_ext_field <- data.table::rbindlist(
                        replicate(num, ext_field, simplify = FALSE)
                    )
                    for (i in 1:nrow(new_ext_field)) {
                        data.table::set(new_ext_field, i, c(id_name, "field_id"),
                            list(new_self_id[i], new_ref_id[i]))
                    }
                    private$m_idd_tbl[[tbl_name]] <- data.table::rbindlist(
                        list(private$m_idd_tbl[[tbl_name]], new_ext_field)
                    )
                }
            }
            # }}}
            append_ext_tbl(choice)
            append_ext_tbl(range)
            append_ext_tbl(reference)
            append_ext_tbl(object_list)
            append_ext_tbl(external_list)
            # update `num_fields` field in class property at the end
            private$m_idd_tbl$class_property[, num_fields := self$num_fields() + self$num_extensible() * num]
            return(self)
            # }}}
        },

        del_extensible_group = function (num = 1L) {
            # delete extensible groups
            # {{{
            assert_that(is_count(num))
            assert_that(self$is_extensible(), msg = glue::glue("Failed to delete \\
               extensible groups. Class `{private$m_class_name}` is not \\
               extensible.")
            )
            line_left <- self$num_fields() - num * self$num_extensible()
            last_req <- private$class_tbl()[
                private$m_idd_tbl$field, on = "class_id", nomatch = 0L][
                private$m_idd_tbl$field_property, on = "field_id", nomatch = 0L][
                required_field == TRUE, field_order]
             assert_that(line_left >= last_req, msg = glue::glue("Failed to \\
                 delete extensible groups. Number of fields left after \\
                 deletion will be `{line_left}` which is less than \\
                 the required field number `{last_req}`."))

            # get ids of fields to delete
            del_start <- self$num_fields() - num * self$num_extensible() + 1L
            del_end <- self$num_fields()
            del_id <- private$m_idd_tbl$field[class_id == self$class_order() &
                data.table::between(field_order, del_start, del_end), field_id]

            # delete fields
            private$m_idd_tbl$field <- private$m_idd_tbl$field[!field_id %in% del_id]
            private$m_idd_tbl$field_property <- private$m_idd_tbl$field_property[!field_id %in% del_id]
            private$m_idd_tbl$field_choice <- private$m_idd_tbl$field_choice[!field_id %in% del_id]
            private$m_idd_tbl$field_range <- private$m_idd_tbl$field_range[!field_id %in% del_id]
            private$m_idd_tbl$field_reference <- private$m_idd_tbl$field_reference[!field_id %in% del_id]
            private$m_idd_tbl$field_extensible <- private$m_idd_tbl$field_extensible[!field_id %in% del_id]
            private$m_idd_tbl$field_object_list <- private$m_idd_tbl$field_object_list[!field_id %in% del_id]
            private$m_idd_tbl$field_external_list <- private$m_idd_tbl$field_external_list[!field_id %in% del_id]
            # update field num
            private$m_idd_tbl$class[, num_fields := (del_start - 1L)]

            return(self)
            # }}}
        },
        # }}}

        # properties assertions
        # {{{
        is_version = function () {
            # return TRUE if this is the 'Version' object
            # {{{
            ifelse(private$class_tbl()[["class_name"]] == "Version", TRUE, FALSE)
            #}}}
        },

        is_required = function () {
            # return TRUE if this is a required object
            # {{{
            private$class_tbl()[["required_object"]]
            # }}}
        },

        is_unique = function () {
            # return TRUE if this is a unique object
            # {{{
            private$class_tbl()[["unique_object"]]
            # }}}
        },

        is_extensible = function () {
            # return TRUE if this object contains extensible fields
            # {{{
            private$class_tbl()[["num_extensible"]] > 0L
            # }}}
        },

        has_name = function () {
            # return TRUE if this object has a name field
            # {{{
            private$class_tbl()[["has_name"]]
            # }}}
        },
        # }}}

        ######################
        #  FIELDS FUNCTIONS  #
        ######################
        # fields getters
        # {{{
        field_name = function (index = NULL, lower = FALSE, unit = FALSE, in_ip = FALSE) {
            # return field name by field index
            # {{{
            if (lower) {
                private$field_name_lcase(index, unit = unit, in_ip = in_ip)
            } else {
                private$field_name_std(index, unit = unit, in_ip = in_ip)
            }
            # }}}
        },

        field_index = function (name = NULL) {
            # return field index by field name (in either standard or lower case
            # format)
            # {{{
            if (is.null(name)) return(private$all_indice())
            name_std <- private$field_name_std()
            name_lc <- private$field_name_lcase()
            res_std <- match(name, name_std)
            res_lc <- match(name, name_lc)
            invalid <- is.na(res_std) & is.na(res_lc)
            assert_that(!any(invalid),
                msg = paste0("Invalid field name found for class ",
                             self$class_name(),": ",
                             backtick_collapse(name[invalid]), "."))
            res_std[is.na(res_std)] <- res_lc[is.na(res_std)]
            return(res_std)
            # }}}
        },

        field_type = function (index = NULL, name = NULL) {
            # return field type
            # {{{
            res_tbl <- private$field_tbl(index, name)[, list(field_name, type)]
            data.table::setattr(as.list.default(res_tbl[["type"]]), "names", res_tbl[["field_name"]])[]
            # }}}
        },

        field_unit = function (index = NULL, name = NULL, in_ip = FALSE) {
            # return field unit in SI or IP format
            # {{{
            if (in_ip) {
                res_tbl <- private$field_tbl(index, name)[, list(field_name, ip_units)]
                data.table::setattr(as.list.default(res_tbl[["ip_units"]]),
                                    "names", res_tbl[["field_name"]])[]
            } else {
                res_tbl <- private$field_tbl(index, name)[, list(field_name, units)]
                data.table::setattr(as.list.default(res_tbl[["units"]]),
                                    "names", res_tbl[["field_name"]])[]
            }
            # }}}
        },

        field_default = function (index = NULL, name = NULL) {
            # return field default value by field index
            # {{{
            res_tbl <- private$field_tbl(index, name)[
                , list(field_name, type, field_default)][
                , `:=`(defaults = as.list(field_default))][
                !is.na(field_default) & type == "integer",
                `:=`(defaults = as.list(as.integer(field_default)))][
                !is.na(field_default) & type == "real",
                `:=`(defaults = as.list(as.double(field_default)))]
            data.table::setattr(res_tbl[["defaults"]], "names", res_tbl[["field_name"]])[]
            # }}}
        },

        field_choice = function (index = NULL, name = NULL) {
            # return field choices by field index
            # {{{
            res_tbl <- private$m_idd_tbl$field_choice[
                private$field_tbl(index, name)[, list(field_id, field_name)],
                on = "field_id"][, lapply(.SD, list), .SDcol = "choice",
                by = list(field_id, field_name)]
            data.table::setattr(res_tbl[["choice"]], "names", res_tbl[["field_name"]])[]
            # }}}
        },

        field_range = function (index = NULL, name = NULL) {
            # return field range value by field index
            # {{{
            res_tbl <- private$m_idd_tbl$field_range[
                private$field_tbl(index, name)[, list(field_id, field_name)]
                , on = "field_id"][, `:=`(range = list(list(
                    minimum = minimum, lower_incbounds = lower_incbounds,
                    maximum = maximum, upper_incbounds = upper_incbounds))
                ), by = "field_id"]
            data.table::setattr(res_tbl[["range"]], "names", res_tbl[["field_name"]])[]
            # }}}
        },

        field_note = function (index = NULL, name = NULL) {
            # return field note by field index
            # {{{
            res_tbl <- private$field_tbl(index, name)[, list(field_name, note)]
            data.table::setattr(as.list.default(res_tbl[["note"]]), "names", res_tbl[["field_name"]])[]
            # }}}
        },
        # }}}

        # ASSERTIONS
        # {{{
        is_valid_field_num = function (num) {
            # check if the input number is acceptable as the total number of fields
            # for this object
            # {{{
            assert_that(is_count(num))
            # if has \min-fields
            min_num <- self$min_fields()
            if (min_num > 0L & min_num > num) return(FALSE)
            # if no \extensible:<#>
            if (!self$is_extensible()) {
                if (self$num_fields() < num) return(FALSE) else return(TRUE)
            # if has \extensible:<#>
            } else {
                if (private$get_extensible_field_index(num) == 0L) {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            }
            # }}}
        },

        is_extensible_field_index = function (index) {
            # check if input index is a valid extensible field index
            # {{{
            assert_that(is_count(index))
            if (!self$is_extensible()) return(FALSE)
            ifelse(index >= self$first_extensible_index(), TRUE, FALSE)
            # }}}
        },

        is_valid_field_name = function (name) {
            # check if input name is a valid field name
            # {{{
            assert_that(is_string(name))
            name %in% private$field_name_std() ||
            name %in% private$field_name_lcase()
            # }}}
        },

        is_valid_field_index = function (index) {
            # check if input index is a valid field index
            # {{{
            assert_that(is_count(index))
            index <= self$num_fields()
            # }}}
        },

        is_autosizable_field = function (index) {
            # check if a field is autosizable or not
            # {{{
            assert_that(self$is_valid_field_index(index))
            private$field_tbl(index)[["autosizable"]]
            # }}}
        },

        is_autocalculatable_field = function (index) {
            # check if a field is autocalculatable or not
            # {{{
            assert_that(self$is_valid_field_index(index))
            private$field_tbl(index)[["autocalculatable"]]
            # }}}
        },

        is_numeric_field = function (index) {
            # check if a field is a numeric field or not
            # {{{
            assert_that(self$is_valid_field_index(index))
            private$field_tbl(index)[["type"]] %in% c("integer", "real")
            # }}}
        },

        is_integer_field = function (index) {
            # check if a field is an integer field or not
            # {{{
            assert_that(self$is_valid_field_index(index))
            private$field_tbl(index)[["type"]] == "integer"
            # }}}
        },

        is_required_field = function (index) {
            # return TURE if the index is an index of a required field
            # {{{
            assert_that(self$is_valid_field_index(index))
            private$field_tbl(index)[["required_field"]]
            # }}}
        },
        # }}}

        print = function (ip_unit = FALSE) {
            # {{{
            prop <- private$class_tbl()[private$m_idd_tbl$group, on = "group_id", nomatch = 0L][
                , `:=`(group =    paste0("  Group: ", backtick(group_name)),
                       unique =   paste0("  Unique: ", unique_object),
                       required = paste0("  Required: ", required_object),
                       num =      paste0("  Total fields: ", num_fields)
                       )]

            cli::cat_line("<< Class: ", backtick(prop[["class_name"]]), " >>")
            cli::cat_rule(center = "* MEMO *", line = 1)
            memo <- self$memo()
            if (is_empty(memo)) {
                cli::cat_line("  <No Memo>")
            } else {
                cli::cat_line("  \"", msg(memo), "\"")
            }
            cli::cat_rule(center = "* PROPERTIES *", line = 1)
            cli::cat_line(unlist(prop[, list(group, unique, required, num)]))
            cli::cat_rule(center = "* FIELDS *", line = 1)

            req <- private$field_tbl()[required_field == TRUE, field_order]
            if (self$is_extensible()) {
                first_ext <- self$first_extensible_index()
                last_ext <- first_ext + self$num_extensible() - 1L
                last_req <- ifelse(is_empty(req), 0L, max(req))
                num_print <- max(last_ext, last_req)
                mark_ext <- character(num_print)
                mark_ext[seq.int(first_ext, last_ext)] <- paste0(" <", clisymbols::symbol$arrow_down, ">")
            } else {
                num_print <- self$num_fields()
                mark_ext <- character(num_print)
            }
            lines <- seq_len(num_print)

            nms <- self$field_name(unit = TRUE, in_ip = ip_unit)[lines]
            index <- lpad(lines)
            required <- req[req < num_print]

            if (not_empty(required)) {
                mark_req <- character(length(nms))
                mark_req[required] <- paste("* ")
                mark_req[-required] <- "  "
            } else {
                mark_req <- " "
            }

            cli::cat_line("  ", index, ":", mark_req, nms, mark_ext)
            if (self$is_extensible()) cli::cat_line("  ......")
            cli::cat_rule(line = 1)
            # }}}
        }
    ),

    private = list(
        # PRIVATE FIELDS
        # {{{
        # class name
        m_class_name = character(),
        m_idd_tbl = NULL,
        # }}}

        # PRIVATE FUNCTIONS
        # {{{
        class_tbl = function () {
            # return a tbl which contains all class property data
            # {{{
            private$m_idd_tbl$class[class_name == private$m_class_name][
                private$m_idd_tbl$class_property, on = "class_id", nomatch = 0L]
            # }}}
        },

        field_tbl = function (index = NULL, name = NULL, min = FALSE) {
            # return a tbl which contains field property data excluding range,
            # choice, reference, object_list and external_list
            # {{{
            # either 'index' or 'name' should be used, not both
            if (all(not_empty(index), not_empty(name))) {
                warning("Both `index` and `name` are given. `name` will be ignored.",
                        call. = FALSE)
                name <- NULL
            }

            out_tbl <- private$class_tbl()[private$m_idd_tbl$field, on = "class_id", nomatch = 0L]
            if (not_empty(index)) {
                private$assert_valid_field_index(index)
                ord <- index
            } else if (not_empty(name)) {
                ord <- self$field_index(name = name)
            } else {
                # this makes sure that it is save to call $field_tbl() in Idfbject
                ord <- NULL
            }

            if (!is.null(ord)) {
                out_tbl <- out_tbl[ord]
            }

            private$m_idd_tbl$field_property[out_tbl, on = "field_id", nomatch = 0L]
            # }}}
        },

        field_name_std = function (index = NULL, unit = FALSE, in_ip = FALSE) {
        # return standard field name
            # {{{
            if (!is.null(index)) private$assert_valid_field_index(index)
            index <- index %||% private$all_indice()
            res_tbl <- private$class_tbl()[private$m_idd_tbl$field,
                on = "class_id", nomatch = 0L][index]

            if (unit) {
                if (in_ip) {
                    res_tbl[["full_ipname"]]
                } else {
                    res_tbl[["full_name"]]
                }
            } else {
                res_tbl[["field_name"]]
            }
            # }}}
        },

        field_name_lcase = function (index = NULL, unit = FALSE, in_ip = FALSE) {
        # return lower case field name
            # {{{
            std_res <- private$field_name_std(index, unit, in_ip)
            gsub("[ -]", "_", tolower(std_res))
            # }}}
        },

        get_extensible_field_index = function (index) {
        # change input index to the line number of extensible data
            # {{{
            (index - self$first_extensible_index() + 1L) %% self$num_extensible()
            # }}}
        },

        num_extensible_group = function () {
        # return the number of extensible groups in the original idd data
            # {{{
            (self$num_fields() - self$first_extensible_index() + 1) / self$num_extensible()
            # }}}
        },

        assert_valid_field_name = function (name) {
            # assert that all input are valid field names.
            # {{{
            assert_that(is.character(name))
            nm_std <- private$field_name_std()
            nm_lc <- private$field_name_lc()
            valid <- name %in% nm_std | name %in% nm_lc
            assert_that(all(valid),
                msg = paste0("Invalid field name found for class ",
                             self$class_name(),": ",
                             backtick_collapse(name[!valid]), "."))
            # }}}
        },

        assert_valid_field_index = function (index) {
            # assert that all input are valid field indice.
            # {{{
            assert_that(is.numeric(index))
            total <- self$num_fields()
            valid <- index <= total
            assert_that(all(valid),
                msg = paste0("Invalid field index found for class ",
                             self$class_name(),": ",
                             backtick_collapse(index[!valid]), "."))
            # }}}
        },

        all_indice = function () {
            # get default indice
            # {{{
            seq_len(self$num_fields())
            # }}}
        }
        # }}}
    )
)
# }}}
