#' Parse EnergyPlus IDD files
#'
#' eplusr provides parsing of and programmatic access to EnergyPlus
#' Input Data Dictionary (IDD) files, and objects. It contains all data needed
#' to parse EnergyPlus models. `Idd` class provides parsing and printing while
#' `IddObject` provides detailed information of curtain class.
#'
#' @details The `Idd` objects for EnergyPlus 8.5 to 8.8 have been pre-parsed and
#' stored internally and will automatically be used when parsing IDF files and
#' strings.  Internally, the powerful \code{data.table} package is used to speed
#' up the whole process and store the results. However, it will still take about
#' 5-6 sec to parse an IDD file.
#'
#' Normally, you may not need to parse any Energy+.idd file unless your model
#' is produced by EnergyPlus whose version is lower than 8.5. If so, it is
#' suggested to store the parsed IDD object and directly pass it to the `idd`
#' argument in [read_idf()] in order to avoid the parsing process whenever you
#' read a model of that version.
#'
#' Under the hook, eplusr uses a SQL-like structure to store both IDF and IDD
#' data in `data.frame` format. Every IDD will be parsed and stored in four
#' tables:
#'
#' * `group`: contains group index and group names.
#' * `class`: contains class names and properties.
#' * `class_reference`: contains reference names of classes.
#' * `field`: contains field names and field properties.
#' * `field_reference`: contains reference names of fields.
#' * `field_default`: contains default values of fields.
#' * `field_choice`: contains choices of choice-type fields.
#' * `field_range`: contains range data of fields.
#' * `field_object_list`: contains object-list data of fields.
#' * `field_external_list`: contains external-list data of fields.
#'
#' @section Usage:
#' ```
#' # read
#' idd <- use_idd(idd)
#'
#' # basic info
#' idd$version()
#' idd$build()
#'
#' idd$group_index(group = NULL)
#' idd$group_name()
#' idd$is_valid_group(group)
#' idd$from_group(class)
#'
#' idd$class_index(class = NULL)
#' idd$class_name()
#' idd$is_valid_class(class)
#'
#' idd$required_class_name()
#' idd$unique_class_name()
#' idd$extenesible_class_name()
#'
#' # idd object
#' idd$object(class)
#' idd$object_in_group(group)
#'
#' idd$print()
#' print(idd)
#' ```
#'
#' @section Arguments:
#'
#' * `idd`: Path to an EnergyPlus Input Data Dictionary (IDD) file, usually
#'     named as `Energy+.idd` or a valid version of pre-parsed IDD (8.3 - 8.9).
#' * `group`: A valid group name or valid group names.
#' * `class`: A valid class name or valid class names.
#'
#' @section Detail:
#'
#' `use_idd` will parses an EnergyPlus Input Data Dictionary (IDD) file, and
#' returns an `Idd` object. If `idd` is a valid version of pre-parsed IDD file,
#' then the pre-parsed `Idd` object will be returned.
#'
#' `$version` returns the version string.
#'
#' `$build` returns the build tag string.
#'
#' `$group_index` returns integer indexes (indexes of name appearance in
#'     the IDD file) of specified groups.
#'
#' `$group_name` returns all group names.
#'
#' `$from_group` returns the names of group that specified classes belongs to.
#'
#' `$is_valid_group` return `TRUE` if the input is a valid group name.
#'
#' `$class_index` returns integer indexes (indexes of name appearance in
#' the IDD file) of specified classes.
#'
#' `$class_name` returns all class names.
#'
#' `$required_class_name` returns the names of all required classes.
#'
#' `$unique_class_name` returns the names of all unique classes.
#'
#' `$extensible_class_name` returns the names of all extensible classes.
#'
#' `$is_valid_class` return `TRUE` if the input is a valid class name.
#'
#' `$object` returns a list of `IddObject`s of specified classes.
#'
#' `$object_in_group` returns a list of `IddObject`s in that group.
#'
#' For details about `IddObject`, please see [idd_object].
#'
#' @importFrom R6 R6Class
#' @importFrom data.table setattr
#' @importFrom cli cat_rule cat_bullet
#' @importFrom assertthat assert_that
#' @return For `use_idd`, an Idd object.
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

            # add a uuid
            private$m_uuid <- uuid::UUIDgenerate(use.time = TRUE)

            idd_file <- parse_idd_file(path)
            private$m_version<- idd_file$version
            private$m_build <- idd_file$build

            private$m_idd_tbl <- list2env(
                idd_file[!names(idd_file) %in% c("version", "build")], parent = emptyenv()
            )
            # assign tbls to IddObject R6Class Generator
            private$m_iddobj_generator <- create_iddobj_generator(self, private, IddObject)
        },
        # }}}

        # PROPERTY GETTERS {{{
        version = function ()
            i_version(self, private),

        build = function ()
            i_build(self, private),

        group_name = function ()
            i_group_name(self, private, type = "idd"),

        from_group = function (class)
            i_from_group(self, private, class = class),

        class_name = function ()
            i_class_name(self, private, type = "idd"),

        required_class_name = function ()
            i_required_class_name(self, private),

        unique_class_name = function ()
            i_unique_class_name(self, private),

        extensible_class_name = function ()
            i_extensible_class_name(self, private),

        group_index = function (group = NULL)
            i_group_index(self, private, group = group),

        class_index = function (class = NULL)
            i_class_index(self, private, name = class, type = "idd"),
        # }}}

        # OBJECT GETTERS {{{
        object = function (class)
            i_iddobject(self, private, class = class),

        object_in_group = function (group)
            i_iddobject_in_group(self, private, group = group),
        # }}}

        # ASSERTIONS {{{
        is_valid_group = function (group)
            i_is_valid_group_name(self, private, group = group),

        is_valid_class = function (class)
            i_is_valid_class_name(self, private, name = class, type = "idd"),
        # }}}

        print = function ()
            i_print_idd(self, private)
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_uuid = NULL,
        m_version = NULL,
        m_build = NULL,
        m_idd_tbl = NULL,
        m_iddobj_generator = NULL,
        # }}}

        deep_clone = function (name, value)
            i_deep_clone(self, private, name, value)
    )
)
# }}}

# read_idd {{{
read_idd <- function (path) {
    # substitute the clone method
    clone_method <- Idd$clone_method
    # `deep` arg will be ignored
    full_clone <- function (deep = TRUE) {
        deep_cloned <- clone_method(deep = TRUE)
        enclos_env <- deep_cloned$.__enclos_env__
        enclos_env$private$m_iddobj_generator$self$private_fields$m_version <-
            enclos_env$private$m_version
        enclos_env$private$m_iddobj_generator$self$private_fields$m_idd_tbl <-
            enclos_env$private$m_idd_tbl
        deep_cloned
    }
    Idd$clone_method <- full_clone
    Idd$public_methods$clone <- full_clone

    Idd$new(path)
}
# }}}

#' @importFrom assertthat assert_that
#' @export
# use_idd {{{
use_idd <- function (idd) {
    if (is_idd(idd)) return(idd)

    assert_that(is_scalar(idd))

    if (is_eplus_ver(idd)) {
        ver <- standerize_ver(idd)
        if (!is_pre_parsed(idd)) {
            stop("Currently only Idd of EnergyPlus v8.5.0 to v8.9.0 have been pre-parsed. ",
                 "Please give a valid path to an `Energy+.idd` file of EnergyPlus version ",
                 backtick(ver), ".", call. = FALSE)
        } else {
            nm <- paste0("idd_", as.character(ver[1,1:2]))
            res <- get(nm)
        }
    } else {
        res <- read_idd(idd)
    }

    res
}
# }}}

# [.Idd {{{
#' @export
'[.Idd' <- function(x, i, j, ..., drop = FALSE) {
    m_obj <- .subset2(x, "object")()
    .subset2(m_obj, i)
}
# }}}
