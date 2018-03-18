# TODO: fields created by extensible fields should have the same
# reference data

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
#' @importFrom R6 R6Class
#' @importFrom data.table rbindlist
#' @importFrom purrr map map_lgl map_chr
#' @importFrom assertthat assert_that
#' @importFrom cli cat_rule cat_line
#' @importFrom clisymbols symbol
#' @return An IDDObject object
#' @docType class
#' @name IDDObject
#' @author Hongyuan Jia
NULL

#' @export
# IDDObject {{{
IDDObject <- R6::R6Class(classname = "IDDObject",

    public = list(
        initialize = function (list) {
            # {{{
            private$m_properties <- list$class
            private$m_fields <- list$field
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
            return(private$m_properties$group)
            # }}}
        },

        group_order = function () {
            # return group order
            # {{{
            return(private$m_properties$group_order)
            # }}}
        },

        class_name = function () {
            # return class name
            # {{{
            return(private$m_properties$class)
            # }}}
        },

        class_order = function () {
            # return class order
            # {{{
            return(private$m_properties$class_order)
            # }}}
        },

        class_format = function () {
            # return class format
            # {{{
            return(private$m_properties$format)
            # }}}
        },

        min_fields = function () {
            # return minimum field number requirement
            # {{{
            return(private$m_properties$min_fields)
            # }}}
        },

        num_fields = function () {
            # return total number of fields in definition
            # {{{
            return(private$m_properties$num_fields)
            # }}}
        },

        memo = function () {
            # return memo
            # {{{
            return(private$m_properties$memo)
            # }}}
        },

        num_extensible = function () {
            # return field number of extensible fields in a extensible field
            # group
            # {{{
            return(private$m_properties$extensible)
            # }}}
        },

        reference_class_name = function () {
            # return reference class name
            # {{{
            .data <- private$m_properties$reference_class_name
            if (is.na(.data)) return(.data)
            return(strsplit(.data, " ", fixed = TRUE)[[1]])
            # }}}
        },

        first_extensible = function () {
            # return index of the first extensible field
            # {{{
            .data <- private$m_fields[begin_extensible == TRUE, field_order]
            if (is_empty(.data)) return(0L)
            return(.data)
            # }}}
        },

        extensible_group = function () {
            # return data of the extensible field group
            # {{{
            # if the current object is not extensible, return an empty
            # data.table
            if (!self$is_extensible()) return(data.table())

            # get the line number of extensible fields
            first_ext <- self$first_extensible()
            num_ext <- self$num_extensible()
            lines_ext <- seq.int(first_ext, length.out = num_ext)
            .data <- private$m_fields[lines_ext][
                , `:=`(required_field = FALSE, begin_extensible = FALSE)]

            return(.data[])
            # }}}
        },

        add_extensible_groups = function (num = 1L) {
            # add one or more extensible groups
            # {{{
            assert_that(is_integerish(num), num > 0L,
                        msg = "`num` should be a positive integer.")
            private$assert_is_extensible()
            .data <- data.table::rbindlist(
                 purrr::map(seq_len(num),
                            ~private$append_extensible_numbers(.x)))
            private$m_fields <- data.table::rbindlist(list(private$m_fields, .data))
            private$m_properties[, num_fields := {private$m_fields[, .N]}]
            return(self)
            # }}}
        },
        # }}}

        # properties assertions
        # {{{
        is_version = function () {
            # return TRUE if this is the 'Version' object
            # {{{
            ifelse(self$class_name() == "Version", TRUE, FALSE)
            #}}}
        },

        is_required = function () {
            # return TRUE if this is a required object
            # {{{
            return(private$m_properties$required_object)
            # }}}
        },

        is_unique = function () {
            # return TRUE if this is a unique object
            # {{{
            return(private$m_properties$unique_object)
            # }}}
        },

        is_extensible = function () {
            # return TRUE if this object contains extensible fields
            # {{{
            ifelse(self$num_extensible(), TRUE, FALSE)
            # }}}
        },

        has_name = function () {
            # return TRUE if this object has a name field
            # {{{
            grepl("Name", private$m_fields[field_order == 1L, field], fixed = TRUE)
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
            if (is.null(name)) return(seq_len(self$num_fields()))
            private$assert_valid_field_index(name)
            res <- match(name, name_std)
            res_lc <- match(name, name_lc)
            res[is.na(res)] <- res_lc[is.na(res)]
            return(res)
            # }}}
        },

        field_type = function (index = NULL, name = NULL) {
            # return field type
            # {{{
            res <- private$fields(index, name)[, type]
            # if no type specified, using field_an
            input <- index %||% name
            mis <- input[is.na(res)]
            if (is_empty(mis)) return(res)
            an_mis <- private$fields(mis)[, field_an]
            type_mis <- ifelse(an_mis == "N", "real", "alpha")
            res[is.na(res)] <- type_mis
            return(res)
            # }}}
        },

        field_unit = function (index = NULL, name = NULL, in_ip = FALSE) {
            # return field unit in SI or IP format
            # {{{
            if (in_ip) {
                private$fields(index, name)[, ip_units]
            } else {
                private$fields(index, name)[, units]
            }
            # }}}
        },

        field_reference = function (index = NULL, name = NULL) {
            # return field references
            # {{{
            private$fields(index, name)[, reference]
            # }}}
        },

        field_object_list = function (index = NULL, name = NULL) {
            # return field reference key by field index
            # {{{
            private$fields(index, name)[, object_list]
            # }}}
        },

        field_default = function (index = NULL, name = NULL) {
            # return field default value by field index
            # {{{
            # if default exists, use it
            .field <- private$fields(index, name)
            .field[, .default := list(list(default)), by = "field_order"]

            # enforce value type
            .field[field_an == "N" & !tolower(default) %in% c("autosize", "autocalculate"),
                   .default := list(list(as.numeric(default))), by = "field_order"]
            # if no default, and choices exist, use the first one
            .field[is.na(default) & !is.na(key), .default := list(list(unlist(key)[1])),
                   by = "field_order"]
            # if no default and the type is not choice, use corresponding NA
            .field[purrr::map_lgl(.default, is.na) & field_an == "N",
                   .default := list(list(NA_real_)), by = "field_order"]
            .field[purrr::map_lgl(.default, is.na) & field_an == "A",
                   .default := list(list(NA_character_)), by = "field_order"]

            .field[, .default]
            # }}}
        },

        field_choice = function (index = NULL, name = NULL) {
            # return field choices by field index
            # {{{
            private$fields(index, name)[, key]
            # }}}
        },

        field_range = function (index = NULL, name = NULL) {
            # return field range value by field index
            # {{{
            res <- private$fields(index, name)[, range]
            out <- purrr::map_chr(res, private$str_range)
            names(res) <- out
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

        is_valid_field_name = function (name) {
            # check if input name is a valid field name
            # {{{
            assert_that(is_string(name))
            name %in% private$field_name_std() |
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

        is_autosizable_field = function (index = NULL, name = NULL) {
            # check if a field is autosizable or not
            # {{{
            assert_that(is_single_key(index, name))
            private$fields(index, name)[, autosizable]
            # }}}
        },

        is_autocalculatable_field = function (index = NULL, name = NULL) {
            # check if a field is autocalculatable or not
            # {{{
            i_iddobject_is_autocalculatable_field(self, private, index, name)
            # }}}
        },

        is_numeric_field = function (index = NULL, name = NULL) {
            # check if a field is a numeric field or not
            # {{{
            assert_that(is_single_key(index, name))
            private$fields(index, name)[, field_an] == "N"
            # }}}
        },

        is_integer_field = function (index = NULL, name = NULL) {
            # check if a field is an integer field or not
            # {{{
            assert_that(is_single_key(index, name))
            private$fields(index, name)[, type] == "integer"
            # }}}
        },

        is_required_field = function (index = NULL, name = NULL) {
            # return TURE if the index is an index of a required field
            # {{{
            assert_that(is_single_key(index, name))
            private$fields(index, name)[, required_field]
            # }}}
        },
        # }}}

        print = function (ip_unit = FALSE) {
            # {{{
            cli::cat_line(clisymbols::symbol$star, clisymbols::symbol$star,
                          " Class: ", backtick(self$class_name()), " ",
                          clisymbols::symbol$star, clisymbols::symbol$star)
            cli::cat_rule(center = "* MEMO* ", line = 1)
            cli::cat_line("  \"", msg(self$memo()), "\"")
            cli::cat_rule(center = "* PROPERTIES *", line = 1)
            cli::cat_line("  Group: ", self$group_name())
            cli::cat_line("  Unique object: ", self$is_unique())
            cli::cat_line("  Required: ", self$is_required())
            cli::cat_line("  Format: ", self$class_format())
            cli::cat_line("  Total fields: ", self$num_fields())
            cli::cat_line("  Minimum fields: ", self$min_fields())
            cli::cat_line("  Extensible field number: ", self$num_extensible())
            cli::cat_line("  First extensible field index: ", self$first_extensible())
            cli::cat_rule(center = "* FIELDS* ", line = 1)

            if (self$is_extensible()) {
                first_ext <- self$first_extensible()
                last_ext <- first_ext + self$num_extensible() - 1L
                last_req <- private$m_fields[required_field == TRUE, max(field_order)]
                num_print <- max(last_ext, last_req)
                mark_ext <- character(num_print)
                mark_ext[seq.int(first_ext, last_ext)] <- paste0(" <", clisymbols::symbol$arrow_down, ">")
            } else {
                num_print <- self$num_fields()
                mark_ext <- character(num_print)
            }
            lines <- seq_len(num_print)

            nms <- self$field_name(unit = TRUE, in_ip = ip_unit)[lines]
            index <- stringr::str_pad(lines, nchar(num_print), "left")
            required <- private$m_fields[field_order <= num_print][
                required_field == TRUE, field_order]

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
        # class property data
        m_properties = data.table(),
        # field data
        m_fields = data.table(),
        # }}}

        # PRIVATE FUNCTIONS
        # {{{
        fields = function (index = NULL, name = NULL) {
        # return single field data by field index or field name (either in
        # standard format or lower case format)
            # {{{
            # either 'index' or 'name' should be used, not both
            if (all(not_empty(index), not_empty(name))) {
                warning("Both 'index' and 'name' are given. 'name' will be ignored.",
                        call. = FALSE)
                name <- NULL
            }

            if (not_empty(index)) {
                valid <- purrr::map_lgl(index, self$is_valid_field_index)
                assert_that(all(valid),
                            msg = paste0("Invalid field index found for class ",
                                         backtick(self$class_name()), ": ",
                                         backtick_collapse(index[!valid]), "."))
                out <- private$m_fields[index]
            } else if (not_empty(name)) {
                index <- purrr::map_chr(name, self$field_index)
                out <- private$m_fields[index]
            } else {
                out <- private$m_fields
            }

            return(out)
            # }}}
        },

        field_name_std = function (index = NULL, unit = FALSE, in_ip = FALSE) {
        # return standard field name
            # {{{
            .data <- private$m_fields
            if (not_empty(index)) {
                assert_that(is.numeric(index))
                valid <- self$is_valid_field_index(index)
                assert_that(all(valid),
                            msg = paste0("Invalid field index found: ",
                                         paste0(backtick(index[valid]), collapse = ", ")))
                .data <- .data[index]
            }
            if (unit) {
                if (in_ip) {
                    res <- .data[, `_field_ip`]
                } else {
                    res <- .data[, `_field`]
                }
            } else {
                res <- .data[, `_field_name`]
            }

            return(res)
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
            (index - self$first_extensible() + 1L) %% self$num_extensible()
            # }}}
        },

        num_extensible_group = function () {
        # return the number of extensible groups in the original idd data
            # {{{
            (self$num_fields() - self$first_extensible() + 1) / self$num_extensible()
            # }}}
        },

        append_extensible_numbers = function (num = 1L) {
        # append numbers in field, field_id, field_anid, and field_order in
        # extensible groups
            # {{{
            assert_that(is_count(num))
            num_ext <- self$num_extensible()
            # get current number of extensible groups
            num_grp <- private$num_extensible_group()
            # get max field_id
            max_field_id <- private$m_fields[.N, as.integer(field_id)]
            # get new added field id
            added_field_id <- (num - 1L) * self$num_extensible() + seq_len(self$num_extensible())
            # get total fields
            num_field <- self$num_fields()
            # append numbers in field, field_id, field_anid, field_order
            .data <- self$extensible_group()
            .data[, field := gsub("1", num_grp + num, field)]
            .data[, `_field_name` := gsub("1", num_grp + num, `_field_name`)]
            .data[, `_field` := gsub("1", num_grp + num, `_field`)]
            .data[, `_field_ip` := gsub("1", num_grp + num, `_field_ip`)]
            .data[, field_id := as.character(max_field_id + added_field_id)]
            .data[, field_anid := paste0(field_an, field_id)]
            .data[, field_order := num_field + added_field_id]
            .data[, field_order := num_field + added_field_id]

            return(.data)
            # }}}
        }
        # }}}
    )
)
# }}}

#######################################################################
#                             ASSERTIONS                              #
#######################################################################
# is_single_key
# {{{
is_single_key <- function (index, name) {
    key <- index %||% name
    length(key) == 1L
}
on_failure(is_single_key) <- function (call, env) {
    paste0("`index` and `name` should have a length of one.")
}
# }}}
