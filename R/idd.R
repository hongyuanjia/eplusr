#' Parse EnergyPlus IDD files
#'
#' \code{eplusr} provides parsing of and programmatic access to EnergyPlus
#' Input Data Dictionary (IDD) files, and objects. It contains all data needed
#' to parse EnergyPlus models. IDD objects provide parsing and printing
#'
#' @details The IDD objects for EnergyPlus 8.5 to 8.8 have been pre-parsed and
#' stored internally and will automatically be used when parsing \code{IDF} and
#' \code{IMF} files. Internally, the powerful \code{data.table} package is used
#' to speed up the whole process and store the results. However, it will still
#' take about 5-6 sec to parse an IDD file.
#'
#' Normally, you may not need to parse any Energy+.idd file unless your model
#' is produced by EnergyPlus whose version is lower than 8.5. If so, it is
#' suggested to store the parsed IDD object and directly pass it to the
#' \code{idd} argument in \code{eplusr_model$new} in order to avoid the parsing
#' process whenever you read a model of that version.
#'
#' @section Usage:
#' ```
#'
#' idd <- IDD$new(path)
#'
#' idd$version()
#' idd$build()
#' idd$class_name(group = NULL)
#' idd$group_name(class = NULL)
#' idd$group_order(group = NULL)
#' idd$class_order(class = NULL)
#' idd$orders()
#'
#' idd$object(class)
#' idd$objects(class = NULL)
#' idd$objects_in_group(group)
#'
#' idd$is_valid_class(class)
#' idd$is_valid_group(group)
#'
#' idd$print()
#'
#' print(idd)
#'
#' ```
#'
#' @section Arguments:
#'
#' * `path`: Path to an EnergyPlus Input Data Dictionary (IDD) file, usually
#' named as `Energy+.idd`.
#'
#' * `group`: A valid group name or valid group names.
#'
#' * `class`: A valid class name or valid class names.
#'
#' @section Detail:
#'
#' `IDD$new()` parses an EnergyPlus Input Data Dictionary (IDD) file, and
#' returns an IDD object.
#'
#' `$version()` returns the version string of current idd file.
#'
#' `$build()` returns the build tag string of current idd file.
#'
#' `$group_name(class)` returns group name that that `class` belong to.
#'
#' `$class_name(group)` returns class names of that `group`. If `group` not
#' given, all class names in current IDD are returned.
#'
#' `$group_order(group)` returns integer orders (orders of name apperarance in
#' the IDD file) of that `group`.
#'
#' `$class_order(class)` returns integer orders (orders of name apperarance in
#' the IDD file) of that `class`.
#'
#' `$object(class)` returns an IDDObject of that `class`.
#'
#' `$objects(class)` returns a list of IDDObjects of `class`es. If `class` is
#' NULL, all IDDObjects in current IDD are returned.
#'
#' `$objects_in_group(group)` returns a list of IDDObjects in that `group`.
#'
#' `$is_valid_group(group)` return `TRUE` if the input is a valid `group` name.
#'
#' `$is_valid_class(class)` return `TRUE` if the input is a valid `class` name.
#'
#' @importFrom R6 R6Class
#' @importFrom data.table setattr
#' @importFrom purrr map map_lgl
#' @importFrom cli cat_rule cat_bullet
#' @importFrom assertthat assert_that
#' @return An Idd object
#' @docType class
#' @name idd
#' @author Hongyuan Jia
#' @references
#' \href{https://github.com/NREL/EnergyPlus/tree/develop/src/IDF_Editor}{IDFEditor
#' source code}
NULL

# Idd {{{
Idd <- R6::R6Class(classname = "Idd",

    public = list(
        # INITIALIZE {{{
        initialize = function (path) {
            idd_file <- parse_idd_file(path)
            private$m_version<- idd_file$version
            private$m_build <- idd_file$build

            private$m_idd_tbl <- list2env(
                idd_file[!names(idd_file) %in% c("version", "build")], parent = emptyenv()
            )
            # assign tbls to IddObject R6Class Generator
            private$create_iddobj_gen(IddObject)
        },
        # }}}

        # PROPERTIES GETTERS
        # {{{
        version = function () {
            return(private$m_version)
        },

        build = function () {
            return(private$m_build)
        },

        group_names = function (class = NULL) {
            if (is.null(class)) {
                private$m_idd_tbl$group[["group_name"]]
            } else {
                private$assert_valid_classes(class)
                res <- private$m_idd_tbl$group[
                    private$m_idd_tbl$class[J(class), on = "class_name", list(group_id)]
                    , on = "group_id", nomatch = 0L, group_name]
                data.table::setattr(res, "names", class)
                res
            }
        },

        class_names = function (group = NULL) {
            if (is.null(group)) {
                private$m_idd_tbl$class[["class_name"]]
            } else {
                private$assert_valid_groups(group)
                res <- private$m_idd_tbl$class[
                    private$m_idd_tbl$group[J(group), on = "group_name", list(group_id)]
                    , on = "group_id", nomatch = 0L, class_name]
                data.table::setattr(res, "names", group)
                res
            }
        },

        required_class_names = function () {
            # return names of all required classes
            # {{{
            private$m_idd_tbl$class[private$m_idd_tbl$class_property, on = "class_id"][
                required_object == TRUE, class_name]
            # }}}
        },

        unique_class_names = function () {
            # return names of all unique classes
            # {{{
            private$m_idd_tbl$class[private$m_idd_tbl$class_property, on = "class_id"][
                unique_object == TRUE, class_name]
            # }}}
        },

        extensible_class_names = function () {
            # return names of all unique classes
            # {{{
            private$m_idd_tbl$class[private$m_idd_tbl$class_property, on = "class_id"][
                num_extensible > 0L, class_name]
            # }}}
        },

        group_orders = function (group) {
            # return group order
            # {{{
            private$assert_valid_groups(group)
            res <- private$m_idd_tbl$group[J(group), on = "group_name", group_id]
            data.table::setattr(res, "names", group)
            res
            # }}}
        },

        class_orders = function (class) {
            # return class order
            # {{{
            private$assert_valid_classes(class)
            res <- private$m_idd_tbl$class[J(class), on = "class_name", class_id]
            data.table::setattr(res, "names", class)
            res
            # }}}
        },

        object = function (class) {
            # return a single object
            # {{{
            assert_that(self$is_valid_class(class))
            private$IddObject$new(class)
            # }}}
        },

        objects_in_group = function (group) {
            # return all objects in a group
            # {{{
            assert_that(self$is_valid_group(group))
            purrr::map(
                private$m_idd_tbl$group[group_name == group][
                    private$m_idd_tbl$class, on = "group_id", class_name],
                private$IddObject$new
            )
            # }}}
        },

        required_objects = function () {
            # return a list of all required IDDObjects
            # {{{
            purrr::map(self$required_class_names(), private$IddObject$new)
            # }}}
        },

        unique_objects = function () {
            # return a list of all unique IDDObjcts
            # {{{
            purrr::map(self$unique_class_names(), private$IddObject$new)
            # }}}
        },

        reference_map = function (class) {
            # return reference data
            # {{{
            assert_that(self$is_valid_class(class))
            # check if this class has \reference-class-name
            if (is_empty(private$m_idd_tbl$class_reference)) {
                ref_class <- character(0)
            } else {
                ref_class <- private$m_idd_tbl$class[class_name == class][
                    private$m_idd_tbl$class_reference, on = "class_id", nomatch = 0L, list(reference)][
                    private$m_idd_tbl$field_object_list, on = c(reference = "object_list"), nomatch = 0L, list(field_id)][
                    private$m_idd_tbl$field, on = "field_id", nomatch = 0L, list(class_id)][
                    private$m_idd_tbl$class, on = "class_id", unique(class_name), nomatch = 0L]
            }

            # check if fields in this class has \reference attributes
            ref_field <- private$m_idd_tbl$class[class_name == class][
                private$m_idd_tbl$field, on = "class_id", nomatch = 0L, list(field_id)][
                private$m_idd_tbl$field_reference, on = "field_id", nomatch = 0L, list(reference)][
                private$m_idd_tbl$field_object_list, on = c(reference = "object_list"), nomatch = 0L, list(field_id)][
                private$m_idd_tbl$field, on = "field_id", nomatch = 0L][
                private$m_idd_tbl$class, on = "class_id", nomatch = 0L, unique(class_name)]

            # check if fields in this class has \object-list attributes
            object_list <- private$m_idd_tbl$class[class_name == class][
                private$m_idd_tbl$field, on = "class_id", nomatch = 0L, list(field_id)][
                private$m_idd_tbl$field_object_list, on = "field_id", nomatch = 0L, list(object_list)][
                private$m_idd_tbl$field_reference, on = c(object_list = "reference"), nomatch = 0L, list(field_id)][
                private$m_idd_tbl$field, on = "field_id", nomatch = 0L, list(class_id)][
                private$m_idd_tbl$class, on = "class_id", nomatch = 0L, unique(class_name)]

            # check if fields in this class has \external-list attributes
            external_list <- private$m_idd_tbl$class[class_name == class][
                private$m_idd_tbl$field, on = "class_id", nomatch = 0L, list(field_id)][
                private$m_idd_tbl$field_external_list, on = "field_id", nomatch = 0L, list(external_key = unique(external_list))][
                external_key == "autoRDDvariable", external_list := list("eplusout.rdd")][
                external_key == "autoRDDmeter", external_list := list("eplusout.mdd")][
                external_key == "autoRDDvariableMeter", external_list := list(c("eplusout.rdd", "eplusout.mdd"))][
                , unique(unlist(external_list))]

            res <- list(reference_class = ref_class,
                        reference_field = ref_field,
                        object_list = object_list,
                        external_list = external_list)

            return(res)
            # }}}
        },
        # }}}

        # ASSERTIONS
        # {{{
        is_valid_group = function (group) {
            assert_that(is_string(group))
            group %in% private$m_idd_tbl$group[["group_name"]]
        },

        is_valid_class = function (class) {
            assert_that(is_string(class))
            class %in% private$m_idd_tbl$class[["class_name"]]
        },
        # }}}

        print = function () {
            ver <- paste0("Version: ", private$m_version)
            bld <- paste0("Build: ", private$m_build)
            cls <- paste0("Total Class: ", nrow(private$m_idd_tbl$class))
            cli::cat_rule(left = crayon::bold("EnergyPlus Input Data Dictionary"))
            cli::cat_bullet(ver)
            cli::cat_bullet(bld)
            cli::cat_bullet(cls)
        }
    ),

    private = list(
        # PRIVATE FIELDS
        # {{{
        m_version = character(),
        m_build = character(),
        m_idd_tbl = NULL,
        IddObject = NULL,
        # }}}

        # PRIVATE FUNCTIONS
        # {{{
        create_iddobj_gen = function (IddObject) {
            # create an IddObject R6Class Generator corresponding to this Idd
            # {{{
            # clone the IddObject R6Class Generator
            own_iddobject <- clone_generator(IddObject)
            # assign shared data to IddObject R6Class Generator
            own_iddobject$self$private_fields$m_version <- private$m_version
            own_iddobject$self$private_fields$m_idd_tbl <- private$m_idd_tbl
            private$IddObject <- own_iddobject
            # }}}
        },

        assert_valid_groups = function (groups) {
            # assert that all members in group are valid group names.
            # {{{
            valid <- groups %in% private$m_idd_tbl$group[["group_name"]]
            assert_that(all(valid),
                msg = paste0("Invalid group name found for current IDD",
                             backtick_collapse(groups[!valid]), "."))
            # }}}
        },

        assert_valid_classes = function (classes) {
            # assert that all members in class are valid class names.
            # {{{
            valid <- classes %in% private$m_idd_tbl$class[["class_name"]]
            assert_that(all(valid),
                msg = paste0("Invalid class name found for current IDD",
                             backtick_collapse(classes[!valid]), "."))
            # }}}
        }
        # }}}
    )
)
# }}}

# ASSERTION ERROR MESSAGES

#' @importFrom assertthat "on_failure<-"
on_failure(Idd$public_methods$is_valid_class) <- function (call, env) {
    paste0("Invalid class name found: ", backtick(eval(call$class, env)), ".")
}

on_failure(Idd$public_methods$is_valid_group) <- function (call, env) {
    paste0("Invalid group name found: ", backtick(eval(call$group, env)), ".")
}

#' @importFrom assertthat assert_that
#' @export
# use_idd {{{
use_idd <- function (idd) {
    if (is_idd(idd)) return(idd)

    assert_that(is_scalar(idd))

    if (is_eplus_ver(idd)) {
        if (is_pre_parsed(idd)) {
            idd <- as.character(idd)
            res <- switch(idd,
                "8.5" = idd_8.5,
                "8.6" = idd_8.6,
                "8.7" = idd_8.7,
                "8.8" = idd_8.8,
                "8.9" = idd_8.9)
        } else {
            stop("Currently only Idd of EnergyPlus v8.5 to v8.9 have been pre-parsed. ",
                 "Please give a valid path to an `Energy+.idd` file of EnergyPlus version ",
                 backtick(idd), ".", call. = FALSE)
        }
    } else {
        res <- Idd$new(idd)
    }
    res
}
# }}}

'[.Idd' <- function(x, i, j, ..., drop = FALSE) {
    m_obj <- .subset2(x, "objects")()
    .subset2(m_obj, i)
}
