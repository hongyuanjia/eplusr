#' @importFrom R6 R6Class
#' @importFrom cli cat_line cat_rule
#' @importFrom crayon bold
#' @include impl-idf.R
NULL

#' Read, Modify, and Run an EnergyPlus Model
#'
#' eplusr provides parsing EnergyPlus Input Data File (IDF) files and strings
#' in a hierarchical structure, which was extremely inspired by
#' [OpenStudio utilities library](https://openstudio-sdk-documentation.s3.amazonaws.com/cpp/OpenStudio-2.5.1-doc/utilities/html/idf_page.html),
#' but with total different data structure under the hook.
#'
#' eplusr uses `Idf` class to present the whole IDF file and use [IdfObject]
#' to present a single object in IDF. Both `Idf` and [IdfObject] contain member
#' functions for helping modify the data in IDF so it complies with the
#' underlying IDD (EnergyPlus Input Data Dictionary).
#'
#' Under the hook, eplusr uses a SQL-like structure to store both IDF and IDD
#' data in different [data.table::data.table]s. So to modify an EnergyPlus model
#' in eplusr is equal to change the data in those IDF tables accordingly, in the
#' context of specific IDD data. This means that a corresponding [Idd] object is
#' needed whenever creating an `Idf` object. eplusr provides several
#' [helpers][use_idd()] to easily download IDD files and create [Idd] objects.
#'
#' All IDF reading process starts with function [read_idf()] which returns an
#' `Idf` object. `Idf` class provides lots of methods to programmatically query
#' and modify EnergyPlus models.
#'
#' Internally, the powerful [data.table](https://cran.r-project.org/package=data.table)
#' package is used to speed up the whole IDF parsing process and store the
#' results. Under the hook, eplusr uses a SQL-like structure to store both IDF
#' and IDD data in [data.table::data.table] format. Every IDF will be parsed and
#' stored in three tables:
#'
#' * `object`: contains object IDs, names and comments.
#' * `value`: contains field values
#' * `reference`: contains cross-reference data of field values.
#'
#' @seealso [IdfObject] class for a single object in an IDF.
#' @author Hongyuan Jia
#' @name Idf
#'
NULL

#' @export
# Idf {{{
Idf <- R6::R6Class(classname = "Idf", lock_objects = FALSE,

    public = list(

        # INITIALIZE {{{
        #' @description
        #' Create an `Idf` object
        #'
        #' @details
        #' It takes an EnergyPlus Input Data File (IDF) as input and returns an
        #' `Idf` object.
        #'
        #' Currently, Imf file is not fully supported. All EpMacro lines will be treated
        #' as normal comments of the nearest downwards object. If input is an Imf file,
        #' a warning will be given during parsing. It is recommended to convert the Imf
        #' file to an Idf file and use [ParametricJob] class to conduct
        #' parametric analysis.
        #'
        #' @param path Either a path, a connection, or literal data (either a single
        #'        string or a raw vector) to an EnergyPlus Input Data File
        #'        (IDF). If a file path, that file usually has a extension
        #'        `.idf`.
        #' @param idd Any acceptable input of [use_idd()]. If `NULL`, which is the
        #'        default, the version of IDF will be passed to [use_idd()]. If
        #'        the input is an `.ddy` file which does not have a version
        #'        field, the latest version of [Idf] cached will be used.
        #'
        #' @return An `Idf` object.
        #'
        #' @examples
        #' \dontrun{
        #' # example model shipped with eplusr from EnergyPlus v8.8
        #' path_idf <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr") # v8.8
        #'
        #' # If neither EnergyPlus v8.8 nor Idd v8.8 was found, error will
        #' # occur. If Idd v8.8 is found, it will be used automatically.
        #' idf <- Idf$new(path_idf)
        #'
        #' # argument `idd` can be specified explicitly using `use_idd()`
        #' idf <- Idf$new(path_idf, idd = use_idd(8.8))
        #'
        #' # you can set `download` arugment to "auto" in `use_idd()` if you
        #' # want to automatically download corresponding IDD file when
        #' # necessary
        #' idf <- Idf$new(path_idf, use_idd(8.8, download = "auto"))
        #'
        #' # Besides use a path to an IDF file, you can also provide IDF in literal
        #' # string format
        #' string_idf <-
        #'     "
        #'     Version, 8.8;
        #'     Building,
        #'         Building;                !- Name
        #'     "
        #'
        #' Idf$new(string_idf, use_idd(8.8, download = "auto"))
        #' }
        #'
        initialize = function (path, idd = NULL) {
            # only store if input is a path
            if (is.character(path) && length(path) == 1L) {
                if (file.exists(path)) private$m_path <- normalizePath(path)
            }

            idf_file <- parse_idf_file(path, idd)
            idd <- use_idd(idf_file$version)

            # in case there is no version field in input IDF
            private$m_version <- idf_file$version

            # init idd tbl
            private$m_idd <- idd

            # init idf tbl
            private$m_idf_env <- list2env(idf_file[c("object", "value", "reference")], parent = emptyenv())

            # init log data
            private$m_log <- new.env(hash = FALSE, parent = emptyenv())

            # add a uuid
            private$m_log$uuid <- unique_id()

            private$m_log$unsaved <- FALSE
            private$m_log$order <- private$m_idf_env$object[, list(object_id)][
                , object_order := 0L]

            private$m_log$view_in_ip <- eplusr_option("view_in_ip")
            private$m_log$num_digits <- eplusr_option("num_digits")
            private$m_log$save_format <- idf_file$options$save_format
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        # version {{{
        #' @description
        #' Get the version of current `Idf`
        #'
        #' @details
        #' `$version()` returns the version of current `Idf` in a
        #' [base::numeric_version()] format. This makes it easy to direction
        #' compare versions of different `Idf`s, e.g. `idf$version() > 8.6` or
        #' `idf1$version() > idf2$version()`.
        #'
        #' @return A [base::numeric_version()] object.
        #' @examples
        #' \dontrun{
        #' # get version
        #' idf$version()
        #' }
        #'
        version = function ()
            idf_version(self, private),
        # }}}

        # path {{{
        #' @description
        #' Get the file path of current `Idf`
        #'
        #' @details
        #' `$path()` returns the full path of current `Idf` or `NULL` if the
        #' `Idf` object is created using a character vector and not saved
        #' locally.
        #'
        #' @return `NULL` or a single string.
        #'
        #' @examples
        #' \dontrun{
        #' # get path
        #' idf$path()
        #'
        #' # return `NULL` if Idf is not created from a file
        #' Idf$new("Version, 8.8;\n")$path()
        #' }
        #'
        path = function ()
            idf_path(self, private),
        # }}}

        # group_name {{{
        #' @description
        #' Get names of groups
        #'
        #' @details
        #' `$group_name()` returns names of groups current `Idf` contains or
        #' the underlying [Idd] object contains.
        #'
        #' @param all If `FALSE`, only names of groups in current `Idf` object
        #'        will be returned. If `TRUE`, all group names in the underlying
        #'        [Idd] will be returned. Default: `FALSE`.
        #' @param sorted Only applicable when `all` is `FALSE`. If `TRUE`,
        #'        duplications in returned group or class names are removed, and
        #'        unique names are further sorted according to their occurrences
        #'        in the underlying [Idd]. Default: `TRUE`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get names of all groups Idf contains
        #' idf$group_name()
        #'
        #' # get group name of each object in Idf
        #' idf$group_name(sorted = FALSE)
        #'
        #' # get names of all available groups in underlying Idd
        #' idf$group_name(all = TRUE)
        #' }
        #'
        group_name = function (all = FALSE, sorted = TRUE)
            idf_group_name(self, private, all, sorted),
        # }}}

        # class_name {{{
        #' @description
        #' Get names of classes
        #'
        #' @details
        #' `$class_name()` returns names of classes current `Idf` contains or
        #' the underlying [Idd] object contains.
        #'
        #' @param all If `FALSE`, only names of classes in current `Idf` object
        #'        will be returned. If `TRUE`, all class names in the underlying
        #'        [Idd] will be returned. Default: `FALSE`.
        #' @param sorted Only applicable when `all` is `FALSE`. If `TRUE`,
        #'        duplications in returned group or class names are removed, and
        #'        unique names are further sorted according to their occurrences
        #'        in the underlying [Idd]. Default: `TRUE`.
        #' @param by_group Only applicable when `all` or `sorted` is `TRUE`. If
        #'        `TRUE`, a list is returned which separates class names by the
        #'        group they belong to.
        #'
        #' @return A character vector if `by_group` is `FALSE` and a list of
        #' character vectors when `by_group` is `TRUE`.
        #'
        #' @examples
        #' \dontrun{
        #' # get names of all classes in Idf
        #' idf$class_name()
        #'
        #' # get names of all classes grouped by group names in Idf
        #' idf$class_name(by_group = TRUE)
        #'
        #' # get class name of each object in Idf
        #' idf$class_name(sorted = FALSE)
        #'
        #' # get names of all available classes in underlying Idd
        #' idf$class_name(all = TRUE)
        #'
        #' # get names of all available classes grouped by group names in
        #' # underlying Idd
        #' idf$class_name(all = TRUE, by_group = TRUE)
        #' }
        #'
        class_name = function (all = FALSE, sorted = TRUE, by_group = FALSE)
            idf_class_name(self, private, all, sorted, by_group),
        # }}}

        # is_valid_group {{{
        #' @description
        #' Check if elements in input character vector are valid group names.
        #'
        #' @details
        #' `$is_valid_group()` returns `TRUE`s if given character vector
        #' contains valid group names in the context of current `Idf` (when
        #' `all` is `FALSE`) or current underlying [Idd] (when `all` is `TRUE`).
        #'
        #' Note that case-sensitive matching is performed, which means that
        #' `"Location and Climate"` is a valid group name but `"location and
        #' climate"` is not.
        #'
        #' @param group A character vector to check.
        #' @param all If `FALSE`, check if input characters are valid group names
        #'        for current `Idf`. If `TRUE`, check if input characters are
        #'        valid group names for underlying [Idd]. Default: FALSE
        #'
        #' @return A logical vector with the same length as input character
        #' vector.
        #'
        #' @examples
        #' \dontrun{
        #' # check if input is a valid group name in current Idf
        #' idf$is_valid_group(c("Schedules", "Compliance Objects"))
        #'
        #' # check if input is a valid group name in underlying Idd
        #' idf$is_valid_group(c("Schedules", "Compliance Objects"), all = TRUE)
        #' }
        #'
        is_valid_group = function (group, all = FALSE)
            idf_is_valid_group_name(self, private, group, all),
        # }}}

        # is_valid_class {{{
        #' @description
        #' Check if elements in input character vector are valid class names.
        #'
        #' @details
        #' `$is_valid_class()` returns `TRUE`s if given character vector
        #' contains valid class names in the context of current `Idf` (when
        #' `all` is `FALSE`) or current underlying [Idd] (when `all` is `TRUE`),
        #' and `FALSE`s otherwise.
        #'
        #' Note that case-sensitive matching is performed, which means that
        #' `"Version"` is a valid class name but `"version"` is not.
        #'
        #' @param class A character vector to check.
        #' @param all If `FALSE`, check if input characters are valid class names
        #'        for current `Idf`. If `TRUE`, check if input characters are
        #'        valid class names for underlying [Idd]. Default: FALSE
        #'
        #' @return A logical vector with the same length as input character
        #' vector.
        #'
        #' @examples
        #' \dontrun{
        #' # check if input is a valid class name in current Idf
        #' idf$is_valid_class(c("Building", "ShadowCalculation"))
        #'
        #' # check if input is a valid class name in underlying Idd
        #' idf$is_valid_class(c("Building", "ShadowCalculation"), all = TRUE)
        #' }
        #'
        is_valid_class = function (class, all = FALSE)
            idf_is_valid_class_name(self, private, class, all),
        # }}}

        # definition {{{
        #' @description
        #' Get the [IddObject] object for specified class.
        #'
        #' @details
        #' `$definition()` returns an [IddObject] of given class. [IddObject]
        #' contains all data used for parsing and creating an [IdfObject]. For
        #' details, please see [IddObject] class.
        #'
        #' @param class A **single** string of valid class name in current
        #'        [Idd].
        #'
        #' @return An [IddObject] object.
        #'
        #' @examples
        #' \dontrun{
        #' # get the IddObject object for specified class
        #' idf$definition("Version")
        #' }
        #'
        definition = function (class)
            idf_definition(self, private, class),
        # }}}

        # object_id {{{
        #' @description
        #' Get the unique ID for each object in specified classes in the `Idf`.
        #'
        #' @details
        #' In `Idf`, each object is assigned with an integer as an universally
        #' unique identifier (UUID) in the context of current `Idf`. UUID is
        #' not reused even if the object associated is deleted.
        #'
        #' `$object_id()` returns an integer vector (when `simplify` is `TRUE`)
        #' or a named list (when `simplify` is `FALSE`) of integer vectors that
        #' contain object IDs in each specified class. The returned list is
        #' named using specified class names.
        #'
        #' @param class A character vector that contains valid class names for
        #'        current `Idf` object. If `NULL`, all classes in current `Idf`
        #'        object are used. Default: `NULL`.
        #' @param simplify If `TRUE`, an integer vector contains object IDs of
        #'        all specified classes is returned. If `FALSE`, a named list
        #'        that contains object IDs for each specified class is returned.
        #'        Default: `FALSE`.
        #'
        #' @return An integer vector (when `simplify` is `TRUE`) or a named list
        #' of integer vectors (when `simplify` is `FALSE`).
        #'
        #' @examples
        #' \dontrun{
        #' # get IDs of all objects in current Idf object
        #' idf$object_id()
        #'
        #' # get IDs of all objects in current Idf object, and merge them into a
        #' # single integer vector
        #' idf$object_id(simplify = TRUE)
        #'
        #' # get IDs of objects in class Version and Zone
        #' idf$object_id(c("Version", "Zone"))
        #'
        #' # get IDs of objects in class Version and Zone, and merge them into a
        #' # single integer vector
        #' idf$object_id(c("Version", "Zone"), simplify = TRUE)
        #' }
        #'
        object_id = function (class = NULL, simplify = FALSE)
            idf_object_id(self, private, class, simplify),
        # }}}

        # object_name {{{
        #' @description
        #' Get names for objects in specified classes in the `Idf`.
        #'
        #' @details
        #' In `Idf`, each object is assigned with a single string as the name
        #' for it, if the class it belongs to has name attribute, e.g. class
        #' `RunPeriod`, `Material` and etc. That name should be unique among all
        #' objects in that class. EnergyPlus will fail with an error if
        #' duplications are found among object names in a class.
        #'
        #' `$object_name()` returns a character vector (when `simplify` is
        #' `TRUE`) or a named list (when `simplify` is `FALSE`) of character
        #' vectors that contain object IDs in each specified class. The returned
        #' list is named using specified class names. If specified class does
        #' not have name attribute, `NA`s are returned.
        #'
        #' @param class A character vector that contains valid class names for
        #'        current `Idf`. If `NULL`, all classes in current `Idf` are
        #'        used. Default: `NULL`.
        #' @param simplify If `TRUE`, a character vector contains object names
        #'        of all specified classes is returned. If `FALSE`, a named list
        #'        that contains a character vector for each specified class is
        #'        returned. Default: `FALSE`.
        #'
        #' @return A character vector (when `simplify` is `TRUE`) or a named
        #' list of character vectors (when `simplify` is `FALSE`).
        #'
        #' @examples
        #' \dontrun{
        #' # get names of all objects in current Idf object
        #' idf$object_name()
        #'
        #' # get names of all objects in current Idf object, and merge them into
        #' # a single character vector
        #' idf$object_name(simplify = TRUE)
        #'
        #' # get names of objects in class Version and Zone
        #' idf$object_name(c("Version", "Zone"))
        #'
        #' # get names of objects in class Version and Zone, and merge them into
        #' # a single character vector
        #' idf$object_name(c("Version", "Zone"), simplify = TRUE)
        #' }
        #'
        object_name = function (class = NULL, simplify = FALSE)
            idf_object_name(self, private, class, simplify),
        # }}}

        # object_num {{{
        #' @description
        #' Get number of objects in specified classes in the [Idf] object.
        #'
        #' @details
        #' `$object_num()` returns an integer vector of object number in
        #' specified classes. `0` is returned if there is no object in that
        #' class.
        #'
        #' @param class A character vector that contains valid class names for
        #'        underlying [Idd]. If `NULL`, all classes in current `Idf` are
        #'        used, and the total object number is returned. Default: `NULL`.
        #'
        #' @return An integer vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get total number of objects
        #' idf$object_num()
        #'
        #' # get number of objects in class Zone and Schedule:Compact
        #' idf$object_num(c("Zone", "Schedule:Compact"))
        #' }
        #'
        object_num = function (class = NULL)
            idf_object_num(self, private, class),
        # }}}

        # is_valid_id {{{
        #' @description
        #' Check if elements in input integer vector are valid object IDs.
        #'
        #' @details
        #' `$is_valid_id()` returns `TRUE`s if given integer vector
        #' contains valid object IDs in current `Idf` object.
        #'
        #' @param id An integer vector to check.
        #'
        #' @return A logical vector with the same length as input integer
        #' vector.
        #'
        #' @examples
        #' \dontrun{
        #' idf$is_valid_id(c(51, 1000))
        #' }
        #'
        is_valid_id = function (id)
            idf_is_valid_object_id(self, private, id),
        # }}}

        # is_valid_name {{{
        #' @description
        #' Check if elements in input character vector are valid object names.
        #'
        #' @details
        #' `$is_valid_name()` returns `TRUE`s if given character vector
        #' contains valid object names in current `Idf` object.
        #'
        #' Note that **case-insensitive** matching is performed, which means
        #' that `"rOoF"` is equivalent to `"roof"`. This behavior is consistent
        #' in all methods that take object name(s) as input.
        #'
        #' @param name A character vector to check.
        #'
        #' @return A logical vector with the same length as input character
        #' vector.
        #'
        #' @examples
        #' \dontrun{
        #' idf$is_valid_name(c("Simple One Zone (Wireframe DXF)", "ZONE ONE", "a"))
        #'
        #' # name matching is case-insensitive
        #' idf$is_valid_name(c("simple one zone (wireframe dxf)", "zone one", "a"))
        #' }
        #'
        is_valid_name = function (name)
            idf_is_valid_object_name(self, private, name),
        # }}}

        # object {{{
        #' @description
        #' Extract an [IdfObject] object using object ID or name.
        #'
        #' @details
        #' `$object()` returns an [IdfObject] object specified by an object ID
        #' or name.
        #'
        #' Note that unlike object ID, which is always unique across the whole
        #' `Idf` object, different objects can have the same name. If the name
        #' given matches multiple objects, an error is issued showing what
        #' objects are matched by the same name. This behavior is consistent in
        #' all methods that take object name(s) as input. In this case, it is
        #' suggested to directly use object ID instead of name.
        #'
        #' Note that **case-insensitive** matching is performed for object
        #' names, which means that `"rOoF"` is equivalent to `"roof"`. This
        #' behavior is consistent in all methods that take object name(s) as
        #' input.
        #'
        #' @param which A single integer specifying the object ID or a single
        #'        string specifying the object name.
        #'
        #' @return An [IdfObject] object.
        #'
        #' @examples
        #' \dontrun{
        #' # get an object whose ID is 3
        #' idf$object(3)
        #'
        #' # get an object whose name is "simple one zone (wireframe dxf)"
        #' # NOTE: object name matching is case-insensitive
        #' idf$object("simple one zone (wireframe dxf)")
        #' }
        #'
        object = function (which)
            idf_obj(self, private, which),
        # }}}

        # objects {{{
        #' @description
        #' Extract multiple [IdfObject] objects using object IDs or names.
        #'
        #' @details
        #' `$objects()` returns a named list of [IdfObject] objects using object
        #' IDS or names. The returned list is named using object names.
        #'
        #' Note that unlike object ID, which is always unique across the whole
        #' `Idf` object, different objects can have the same name. If the name
        #' given matches multiple objects, an error is issued showing what
        #' objects are matched by the same name. This behavior is consistent in
        #' all methods that take object name(s) as input. In this case, it is
        #' suggested to directly use object ID instead of name.
        #'
        #' Note that **case-insensitive** matching is performed for object
        #' names, which means that `"rOoF"` is equivalent to `"roof"`. This
        #' behavior is consistent in all methods that take object name(s) as
        #' input.
        #'
        #' @param which An integer vector specifying object IDs or a character
        #'        vector specifying object names.
        #'
        #' @return A named list of [IdfObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # get objects whose IDs are 3 and 10
        #' idf$objects(c(3,10))
        #'
        #' # get objects whose names are "Simple One Zone (Wireframe DXF)" and "ZONE ONE"
        #' # NOTE: object name matching is case-insensitive
        #' idf$objects(c("Simple One Zone (Wireframe DXF)", "zone one"))
        #' }
        #'
        objects = function (which)
            idf_objects(self, private, which),
        # }}}

        # object_unique {{{
        #' @description
        #' Extract the [IdfObject] in class with `unique-object` attribute.
        #'
        #' @details
        #' For each version of an `Idf` object, the corresponding underlying
        #' [Idd] describe how many objects can be defined in each class. Classes
        #' that have `unique-object` attribute can only hold a single object,
        #' e.g. `Version`, `SimulationControl` and etc. `$object_unique()` can
        #' be used to directly return the [IdfObject] in one `unique-object`
        #' class. An error will be issued if there are multiple objects in that
        #' class or input class is not an `unique-object` class. This makes sure
        #' that `$object_unique()` always returns a single [IdfObject].
        #'
        #' `Idf` class also provides custom S3 method of `$` and \code{[[} to
        #' make it more convenient to get the [IdfObject] in `unique-object`
        #' class. Basically, `idf$ClassName` and \code{idf[["ClassName"]]},
        #' where `ClassName` is a single valid class name, is equivalent to
        #' `idf$object_unique(ClassName)` if `ClassName` is an `unique-object`
        #' class. For convenience, underscore-style names are allowed when using
        #' `$`, e.g.  `Site_Location` is equivalent to `Site:Location`. For
        #' instance, `idf$Site_Location` and also `idf[["Site_Location"]]` will
        #' both return the [IdfObject]s in `Site:Location` class. Note that
        #' unlike `$object_unique()`, `idf$ClassName` and `idf[["ClassName"]]`
        #' will directly return `NULL` instead of giving an error when
        #' `ClassName` is not a valid class name in current `Idf` object. This
        #' makes it possible to use `is.null(idf$ClassName)` to check if
        #' `ClassName` is a valid class or not.
        #'
        #' @param class A single string of valid class name for current `Idf`
        #'        object.
        #'
        #' @return An [IdfObject] object.
        #'
        #' @examples
        #' \dontrun{
        #' # get the SimulationColtrol object
        #' idf$object_unique("SimulationControl")
        #'
        #' # S3 "[[" and "$" can also be used
        #' idf$SimulationControl
        #' idf[["SimulationControl"]]
        #' }
        #'
        object_unique = function (class)
            idf_object_unique(self, private, class),
        # }}}

        # objects_in_class {{{
        #' @description
        #' Extract all [IdfObject] objects in one class.
        #'
        #' @details
        #' `$objects_in_class()` returns a named list of all [IdfObject] objects
        #' in specified class. The returned list is named using object names.
        #'
        #' `Idf` class also provides custom S3 method of `$` and \code{[[} to
        #' make it more convenient to get all [IdfObject] objects in one class.
        #' Basically, `idf$ClassName` and \code{idf[["ClassName"]]}, where
        #' `ClassName` is a single valid class name, is equivalent to
        #' `idf$objects_in_class(ClassName)` if `ClassName` is not an
        #' `unique-object` class. For convenience, *underscore-style* names are
        #' allowed, e.g.  `BuildingSurface_Detailed` is equivalent to
        #' `BuildingSurface:Detailed` when using `$`. For instance,
        #' `idf$BuildingSurface_Detailed` and also
        #' `idf[["BuildingSurface:Detailed"]]` will both return all [IdfObject]
        #' objects in `BuildingSurface:Detailed` class. Note that
        #' unlike `$objects_in_class()`, `idf$ClassName` and
        #' `idf[["ClassName"]]` will directly return `NULL` instead of giving
        #' an error when `ClassName` is not a valid class name in current `Idf`
        #' object. This makes it possible to use `is.null(idf$ClassName)` to
        #' check if `ClassName` is a valid class or not.
        #'
        #' @param class A single string of valid class name for current `Idf`
        #'        object.
        #'
        #' @return A named list of [IdfObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # get all objects in Zone class
        #' idf$objects_in_class("Zone")
        #'
        #' # S3 "[[" and "$" can also be used
        #' idf$Zone
        #' idf[["Zone"]]
        #' }
        #'
        objects_in_class = function (class)
            idf_objects_in_class(self, private, class),
        # }}}

        # objects_in_group {{{
        #' @description
        #' Extract all [IdfObject] objects in one group.
        #'
        #' @details
        #' `$objects_in_group()` returns a named list of all [IdfObject] objects
        #' in specified group. The returned list is named using object names.
        #'
        #' @param group A single string of valid group name for current `Idf`
        #'        object.
        #'
        #' @return A named list of [IdfObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # get all objects in Schedules group
        #' idf$objects_in_group("Schedules")
        #' }
        #'
        objects_in_group = function (group)
            idf_objects_in_group(self, private, group),
        # }}}

        # object_relation {{{
        #' @description
        #' Extract the relationship between object field values.
        #'
        #' @details
        #' Many fields in [Idd] can be referred by others. For example, the
        #' `Outside Layer` and other fields in `Construction` class refer to the
        #' `Name` field in `Material` class and other material related classes.
        #' Here it means that the `Outside Layer` field **refers to** the `Name`
        #' field and the `Name` field is **referred by** the `Outside Layer`. In
        #' EnergyPlus, there is also a special type of field called `Node`,
        #' which together with `Branch`, `BranchList` and other classes define
        #' the topography of the HVAC connections. A outlet node of a component
        #' can be referred by another component as its inlet node, but can also
        #' exists independently, such as zone air node.
        #'
        #' `$object_relation()` provides a simple interface to get this kind of
        #' relation. It takes a single object ID or name and also a relation
        #' direction, and returns an `IdfRelation` object which contains data
        #' presenting such relation above. For instance, if
        #' `model$object_relation("WALL-1", "ref_to")` gives results below:
        #'
        #' ```
        #' -- Refer to Others ------------------------
        #'   Class: <Construction>
        #'   \- Object [ID:2] <WALL-1>
        #'      \- 2: "WD01";        !- Outside Layer
        #'         v~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #'         \- Class: <Material>
        #'            \- Object [ID:1] <WD01>
        #'               \- 1: "WD01";        !- Name
        #' ```
        #'
        #' This means that the value `"WD01"` of `Outside Layer` in a
        #' construction named `WALL-1` refers to a material named `WD01`. All
        #' those objects can be further easily extracted using
        #' `$objects_in_relation()` method described below.
        #'
        #' @param which A single integer specifying object ID or a single string
        #'        specifying object name.
        #' @param direction The relation direction to extract. Should be either
        #'        `"all"`, `"ref_to"`, `"ref_by"` and `"node"`.
        #' @param recursive If `TRUE`, the relation is searched recursively. A
        #'        simple example of recursive reference: one material named
        #'        `mat` is referred by a construction named `const`, and `const`
        #'        is also referred by a surface named `surf`. If `recursive` is
        #'        `TRUE`, all relations above will be extracted.
        #' @param depth Only applicable when `recursive` is `TRUE`. This is the
        #'        depth when searching value relations recursively. If `NULL`,
        #'        all possible recursive relations are returned. Default: `1`.
        #'
        #' @return An `IdfRelation` object, which is a list of 3
        #' [data.table::data.table()]s named `ref_to`, `ref_by` and `node`.
        #' Each [data.table::data.table()] contains 24 columns.
        #'
        #' @examples
        #' \dontrun{
        #' # check each layer's reference of a construction named FLOOR
        #' idf$object_relation("floor", "ref_to")
        #'
        #' # check where is this construction being used
        #' idf$object_relation("floor", "ref_by")
        #' }
        #'
        object_relation = function (which, direction = c("all", "ref_to", "ref_by", "node"), recursive = FALSE, depth = 1L)
            idf_object_relation(self, private, which, match.arg(direction), recursive = recursive, recursive_depth = depth),
        # }}}

        # objects_in_relation {{{
        #' @description
        #' Extract multiple [IdfObject] objects referencing each others.
        #'
        #' @details
        #' `$objects_in_relation()` returns a named list of [IdfObject] objects
        #' that have specified relationship with given object. The first element of
        #' returned list is always the specified object itself. If that
        #' object does not have specified relationship with other objects in
        #' specified `class`, a list that only contains specified object itself
        #' is returned.
        #'
        #' For instance, assuming that `const` is a valid object name in
        #' `Construction` class, `idf$objects_in_relation("const", "ref_by",
        #' "BuildingSurface:Detailed")`
        #' will return a named list of an [IdfObject] object named `const` and
        #' also all other [IdfObject] objects in `BuildingSurface:Detailed`
        #' class that refer to field values in `const`. Similarly,
        #' `idf$objects_in_relation("const", "ref_to", "Material")`
        #' will return a named list of an [IdfObject] object named `const` and
        #' also all other [IdfObject] objects in `Material` class that `const`
        #' refers to. This makes it easy to directly extract groups of related
        #' objects and then use `$insert()` method or other methods
        #' described below to insert them or extract data.
        #'
        #' There are lots of recursive references in a model. For instance, a
        #' material can be referred by a construction, that construction can be
        #' referred by a building surface, and that building surface can be
        #' referred by a window on that surface. These objects related
        #' recursively can be extracted by setting `recursive` to `TRUE`.
        #'
        #' @param which A single integer specifying object ID or a single string
        #'        specifying object name.
        #' @param direction The relation direction to extract. Should be one of
        #'        `"ref_to"`, `"ref_by"` or `"node"`.
        #' @param class A character vector of valid class names in the
        #'        underlying [Idd]. It is used to restrict the classes to be
        #'        returned. If `NULL`, all possible classes are considered and
        #'        corresponding [IdfObject] objects are returned if
        #'        relationships are found. Default: `NULL`.
        #' @param recursive If `TRUE`, the relation is searched recursively. A
        #'        simple example of recursive reference: one material named
        #'        `mat` is referred by a construction named `const`, and `const`
        #'        is also referred by a surface named `surf`. If `recursive` is
        #'        `TRUE`, all relations above will be extracted.
        #' @param depth Only applicable when `recursive` is `TRUE`. This is the
        #'        depth when searching value relations recursively. If `NULL`,
        #'        all possible recursive relations are returned. Default: `1`.
        #'
        #' @return An named list of [IdfObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # get a construction named FLOOR and all materials it uses
        #' idf$objects_in_relation("floor", "ref_to")
        #'
        #' # get a construction named FLOOR and all surfaces that uses it
        #' idf$objects_in_relation("floor", "ref_by", "BuildingSurface:Detailed")
        #' }
        #'
        objects_in_relation = function (which, direction = c("ref_to", "ref_by", "node"), class = NULL, recursive = FALSE, depth = 1L)
            idf_objects_in_relation(self, private, which, match.arg(direction), class, recursive = recursive, recursive_depth = depth),
        # }}}

        # search_object {{{
        #' @description
        #' Extract multiple [IdfObject] objects using regular expression on
        #' names.
        #'
        #' @details
        #' `$search_object()` returns a named list of [IdfObject] objects whose
        #' names meet the given regular expression in specified classes.
        #'
        #' @param pattern,ignore.case,perl,fixed,useBytes All are directly
        #'        passed to [base::grepl][base::grep()].
        #' @param class A character vector of valid class names in the
        #'        underlying [Idd]. It is used to restrict the classes to be
        #'        returned. If `NULL`, all possible classes are considered and
        #'        corresponding [IdfObject] objects are returned if
        #'        `pattern` is met Default: `NULL`.
        #'
        #' @return A named list of [IdfObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # get all objects whose names contains "floor"
        #' idf$search_object("floor", ignore.case = TRUE)
        #' }
        #'
        search_object = function (pattern, class = NULL, ignore.case = FALSE,
                                  perl = FALSE, fixed = FALSE, useBytes = FALSE)
            idf_search_object(self, private, pattern, class, ignore.case = ignore.case,
                perl = perl, fixed = fixed, useBytes = useBytes
            ),
        # }}}

        # dup {{{
        #' @description
        #' Duplicate existing objects.
        #'
        #' @details
        #' `$dup()` takes integer vectors of object IDs and character vectors of
        #' object names, duplicates objects specified, and returns a list of
        #' newly created [IdfObject] objects. The names of input are used as new
        #' names for created [IdfObject]s. If input is not named, new names are
        #' the names of duplicated objects with a suffix `"_1"`, `"_2"` and etc,
        #' depending on how many times that object has been duplicated. Note an
        #' error will be issued if trying to assign a new name to an object
        #' which belongs to a class that does not have name attribute.
        #'
        #' Assigning newly added objects with an existing name in current `Idf`
        #' object is prohibited if current validation level includes object name
        #' conflicting checking. For details, please see `level_checks()`.
        #'
        #' @param ... Integer vectors of object IDs and character vectors of
        #'        object names. If input is named, its name will be used as the
        #'        name of newly created objects.
        #'
        #' @return A named list of [IdfObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # duplicate an object named "FLOOR"
        #' idf$dup("floor") # New object name 'FLOOR_1' is auto-generated
        #'
        #' # duplicate that object again by specifing object ID
        #' idf$dup(16) # New object name 'FLOOR_2' is auto-generated
        #'
        #' # duplicate that object two times and giving new names
        #' idf$dup(new_floor = "floor", new_floor2 = 16)
        #'
        #' # duplicate that object multiple times using variable inputs
        #' floors_1 <- c(new_floor3 = "floor", new_floor4 = "floor")
        #' floors_2 <- setNames(rep(16, 5), paste0("flr", 1:5))
        #' idf$dup(floors_1, floors_2)
        #' }
        #'
        dup = function (...)
            idf_dup(self, private, ...),
        # }}}

        # add {{{
        #' @description
        #' Add new objects.
        #'
        #' @details
        #' `$add()` takes new object definitions in list format, adds
        #' corresponding objects in specified classes, returns a list of newly
        #' added [IdfObject] objects. The returned list will be named using
        #' newly added object names. Every list should be named using a valid
        #' class name.  Underscore-style class name is allowed for class name.
        #' Names in each list element are treated as field names. Values without
        #' names will be inserted according to their position. There is a
        #' special element named `.comment` in each list, which will be used as
        #' the comments of newly added object.
        #'
        #' Empty objects can be added using an empty list, e.g.
        #' `idf$add(Building = list())`. All empty fields will be filled with
        #' corresponding default value if `.default` is `TRUE`, leaving other
        #' fields as blanks. However, adding blank objects may not be allowed if
        #' there are required fields in that class and current validate level
        #' includes missing-required-field checking. For what kind of validation
        #' components will be performed during adding new objects, please see
        #' [level_checks()].
        #'
        #' Field name matching is **case-insensitive**. For convenience,
        #' underscore-style field names are also allowed, e.g. `eNd_MoNtH` is
        #' equivalent to `End Month`. This behavior is consistent among all
        #' methods that take field names as input.
        #'
        #' There is no need to give all field values if only specific fields are
        #' interested, as long as other fields are not required. For example, to
        #' define a new object in `RunPeriod` class, the following is enough (at
        #' least for EnergyPlus v8.8):
        #'
        #' ```
        #' idf$add(
        #'     RunPeriod = list(
        #'         "my run period",
        #'         begin_month = 1, begin_day_of_month = 1,
        #'         end_month = 1, end_day_of_month = 31
        #'     ),
        #'     .default = TRUE
        #' )
        #' ```
        #'
        #' If not all field names are given, positions of those values without
        #' field names are determined after those values with names. E.g. in
        #' `idf$add(Construction = list("out_layer", name = "name"))`,
        #' `"out_layer"` will be treated as the value for field `Outside Layer`
        #' in `Construction` class, since the value for field `Name` has been
        #' specified using explicit field name.
        #'
        #' @param ... Lists of object definitions. Each list should be named
        #'        with a valid class name. There is a special element `.comment`
        #'        in each list, which will be used as the comments of newly
        #'        added object.
        #' @param .default If `TRUE`, default values are used for those blank
        #'        fields if possible. If `FALSE`, empty fields are kept blank.
        #'        Default: `TRUE`.
        #' @param .all If `TRUE`, all fields are added. If `FALSE`, only minimum
        #'        required fields are added. Default: `FALSE`.
        #'
        #' @return A named list of [IdfObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # add a new Building object with all default values
        #' empty <- empty_idf(8.8) # create an empty Idf
        #' empty$add(Building = list())
        #'
        #' # add a new Building object with all default values and comments
        #' empty <- empty_idf(8.8) # create an empty Idf
        #' empty$add(Building = list(.comment = c("this is", "a new building")))
        #'
        #' # add a new RunPeriod object with all possible fields
        #' empty <- empty_idf(8.8) # create an empty Idf
        #' empty$add(Building = list(), RunPeriod = list("rp", 1, 1, 1, 31), .all = TRUE)
        #'
        #' # add objects using variable inputs
        #' empty <- empty_idf(8.8) # create an empty Idf
        #' objs1 <- list(Schedule_Constant = list("const"), Building = list())
        #' rp <- list(RunPeriod = list("rp", 2, 1, 2, 28))
        #' empty$add(objs1, rp)
        #' }
        #'
        add = function (..., .default = TRUE, .all = FALSE)
            idf_add(self, private, ..., .default = .default, .all = .all),
        # }}}

        # set {{{
        #' @description
        #' Set values of existing objects.
        #'
        #' @details
        #' `$set()` takes new field value definitions in list format, sets new
        #' values for fields in objects specified, and returns a list of
        #' modified [IdfObject]s. The returned list will be named using names of
        #' modified objects. Every list in `$set()` should be named with a
        #' valid object name. Object ID can also be used but have to be combined
        #' with prevailing two periods `..`, e.g. `..10` indicates the object
        #' with ID `10`. Similar to
        #' \href{../../eplusr/html/Idf.html#method-add}{\code{$add()}}, a
        #' special element `.comment` in each list will be used as the **new**
        #' comments for modified object, overwriting the old ones. Names in list
        #' element are treated as field names.
        #'
        #' There is two special syntax in `$set()`, which is inspired by the
        #' [data.table](https://cran.r-project.org/package=eplusr) package:
        #'
        #' * `class := list(field = value)`: Note the use of `:=` instead of
        #'   `=`. The main difference is that, unlike `=`, the left hand side of
        #'   `:=` should be a valid class name in current `Idf` object. It will
        #'   set the field of all objects in specified class to specified value.
        #' * `.(object, object) := list(field = value)`: Simimar like above, but
        #'   note the use of `.()` in the left hand side. You can put multiple
        #'   object ID or names in `.()`. It will set the field of all specified
        #'   objects to specified value.
        #'
        #' You can delete a field by assigning `NULL` to it, e.g. `list(fld =
        #' NULL)` means to delete the value of field `fld`, in the condition
        #' that `.default` is `FALSE`, `fld` is not a required field and the
        #' index of `fld` is larger than the number minimum fields required for
        #' that class. If those conditions are not required, `fld` will be left
        #' as blank if `.default` is `FALSE` or filled with default value if
        #' `.default` is `TRUE`.
        #'
        #' By default, trailing empty fields that are not required will be
        #' removed and only minimum required fields are kept. For example, if
        #' `rp` is an object in `RunPeriod` class in an `Idf` of version 8.8,
        #' by default empty field with index larger than 11 will be removed
        #' since they are all non-required fields. You can keep the trailing
        #' empty fields by setting `.empty` to `TRUE`.
        #'
        #' New fields that currently do not exist in that object can also be
        #' set. They will be automatically added on the fly.
        #'
        #' Field name matching is **case-insensitive**. For convenience,
        #' underscore-style field names are also allowed, e.g. `eNd_MoNtH` is
        #' equivalent to `End Month`.
        #'
        #' If not all field names are given, positions of those values without
        #' field names are determined after those values with names. E.g. in
        #' `idf$set(floor = list("out_layer", name = "name"))`, `"out_layer"`
        #' will be treated as the value for field `Outside Layer` in an object
        #' named `floor`, since the value for field `Name` has been specified
        #' using explicit field name.
        #'
        #' @param ... Lists of object definitions. Each list should be named
        #'        with a valid object name or ID denoted in style `..ID`. There
        #'        is a special element `.comment` in each list, which will be
        #'        used as new comments of modified object, overwriting existing
        #'        comments if any.
        #' @param .default If `TRUE`, default values are used for those blank
        #'        fields if possible. If `FALSE`, empty fields are kept blank.
        #'        Default: `TRUE`.
        #' @param .empty If `TRUE`, trailing empty fields are kept. Default:
        #'        `FALSE`.
        #'
        #' @return A named list of [IdfObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # modify an object by name (case-insensitive)
        #' idf$set(r13layer = list(roughness = "smooth"))
        #'
        #' # modify an object by ID
        #' idf$set(..12 = list(roughness = "rough"))
        #'
        #' # overwrite existing object comments
        #' idf$set(r13layer = list(.comment = c("New comment")))
        #'
        #' # assign default values to fields
        #' idf$set(r13layer = list(solar_absorptance = NULL), .default = TRUE)
        #'
        #' # set field values to blanks
        #' idf$set(r13layer = list(solar_absorptance = NULL), .default = FALSE)
        #'
        #' # set field values to blank and delete trailing fields
        #' idf$set(r13layer = list(visible_absorptance = NULL), .default = FALSE)
        #'
        #' # set field values to blank and keep blank fields
        #' idf$set(r13layer = list(visible_absorptance = NULL), .default = FALSE, .empty = TRUE)
        #'
        #' # set all fields in one class
        #' idf$set(Material_NoMass := list(visible_absorptance = 0.9))
        #'
        #' # set multiple objects in one class
        #' idf$set(.("r13layer", "r31layer") := list(solar_absorptance = 0.8))
        #' # above is equivalent to
        #' idf$set(r13layer = list(solar_absorptance = 0.8),
        #'         r31layer = list(solar_absorptance = 0.8)
        #' )
        #'
        #' # use variable input
        #' sets <- list(r13layer = list(roughness = "smooth"))
        #' idf$set(sets)
        #' }
        #'
        set = function (..., .default = TRUE, .empty = FALSE)
            idf_set(self, private, ..., .default = .default, .empty = .empty),
        # }}}

        # del {{{
        #' @description
        #' Delete existing objects
        #'
        #' @details
        #' `$del()` takes integer vectors of object IDs and character vectors of
        #' object names, and deletes objects specified.
        #'
        #' If current [validate level][level_checks()] includes reference
        #' checking, objects will not be allowed to be deleted if they are
        #' referred by other objects. For example, an error will be issued if
        #' you want to delete one material that is referred by other
        #' constructions, because doing so will result in invalid field value
        #' references. You may bypass this if you really want to by setting
        #' `.force` to `TRUE`.
        #'
        #' When `.ref_by` or `.ref_to` is `TRUE`, objects will be deleted
        #' only when they have and only have relation with input objects but not
        #' any other objects. For example, a construction `const` consist of 4
        #' different materials. If `.ref_to` is `TRUE`, that 4 materials will
        #' only be deleted when they are only used in `const`, but not used in
        #' any other objects.
        #'
        #' There are recursively reference relations in `Idf` object. For
        #' example, one material's name is referenced by one construction, and
        #' that construction's name can be referred by another surface. You can
        #' delete all of them by setting `.recursive` to `TRUE`.
        #'
        #' If `.ref_by` is `TRUE`, objects whose fields refer to input objects
        #' will also be deleted.
        #'
        #' IF `.ref_to` is `TRUE`, objects whose fields
        #' are referred by input objects will also be deleted.
        #'
        #'
        #' @param ... integer vectors of object IDs and character vectors of
        #'        object names in current `Idf` object.
        #' @param .ref_by If `TRUE`, objects whose fields refer to input objects
        #'        will also be deleted. Default: `FALSE`.
        #' @param .ref_to If `TRUE`, objects whose fields are referred by input
        #'        objects will also be deleted. Default: `FALSE`.
        #' @param .recursive If `TRUE`, relation searching is performed
        #'        recursively, in case that objects whose fields refer to target
        #'        object are also referred by another object, and also objects
        #'        whose fields are referred by target object are also referred
        #'        by another object. Default: `FALSE`.
        #' @param .force If `TRUE`, objects are deleted even if they are
        #'        referred by other objects.
        #'
        #' @return The modified `Idf` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # delete objects using names
        #' idf$object("Fraction") # ScheduleTypeLimits
        #' idf$del("Fraction")
        #'
        #' # delete objects using IDs
        #' idf$objects(c(39, 40)) # Output:Variable
        #' idf$del(39, 40)
        #'
        #' # cannot delete objects that are referred by others
        #' level_checks()$reference # reference-checking is enable by default
        #' idf$del("r13layer") # error
        #'
        #' # force to delete objects even thay are referred by others
        #' idf$del("r13layer", .force = TRUE)
        #'
        #' # delete objects and also objects that refer to them
        #' idf$del("r31layer", .ref_by = TRUE) # Construction 'ROOF31' will be kept
        #'
        #' # delete objects and also objects that they refer to
        #' idf$del("extlights", .ref_to = TRUE) # Schedule 'AlwaysOn' will be kept
        #'
        #' # delete objects and also other objects that refer to them recursively
        #' idf$del("roof31", .ref_by = TRUE, .recursive = TRUE)
        #'
        #' # delete objects using variable inputs
        #' ids <- idf$object_id("Output:Variable", simplify = TRUE)
        #' idf$del(ids)
        #' }
        #'
        del = function (..., .ref_by = FALSE, .ref_to = FALSE, .recursive = FALSE, .force = FALSE)
            idf_del(self, private, ..., .ref_by = .ref_by, .ref_to = .ref_to, .recursive = .recursive, .force = .force),
        # }}}

        # rename {{{
        #' @description
        #' Rename existing objects
        #'
        #' @details
        #' `$rename()` takes named character vectors of object names and named
        #' integer vectors of object IDs, renames specified objects to names of
        #' input vectors and returns a list of renamed [IdfObject]s. The
        #' returned list will be named using names of modified objects. An error
        #' will be issued if trying to "rename" an object which does not have
        #' name attribute. When renaming an object that is referred by other
        #' objects, corresponding fields that refer to that object's name will
        #' also be changed accordingly.
        #'
        #' @param ... Integer vectors of valid object IDs and character vectors
        #'        of valid object names in current `Idf` object. Each element
        #'        should be named. Names of input vectors are used as the new
        #'        object names
        #'
        #' @return A named list of renamed [IdfObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' idf$objects(c("on/off", "test 352a"))
        #' idf$rename(on_off = "on/off", test_352a = 51)
        #' }
        #'
        rename = function (...)
            idf_rename(self, private, ...),
        # }}}

        # insert {{{
        #' @description
        #' Insert new objects from [IdfObject]s
        #'
        #' @details
        #' `$insert()` takes [IdfObject]s or lists of [IdfObject]s as input,
        #' inserts them into current `Idf` objects, and returns a list of
        #' inserted [IdfObject]s. The returned list will be named using names of
        #' inserted objects.
        #'
        #' `$insert()` is quite useful to insert objects from other `Idf`
        #' objects. However, you cannot insert an [IdfObject] which comes from a
        #' different version than current `Idf` object.
        #'
        #' `$insert()` will skip [IdfObject]s that have exactly same fields in
        #' current `Idf` object. If input [IdfObject] has the same name as one
        #' [IdfObject] in current `Idf` object but field values are not equal,
        #' an error will be issued if current [validate level][level_checks()]
        #' includes conflicted-name checking.
        #'
        #' By default, trailing empty fields that are not required will be
        #' removed and only minimum required fields are kept. You can keep the
        #' trailing empty fields by setting `.empty` to `TRUE`.
        #'
        #' @param ... [IdfObject]s or lists of [IdfObject]s from same version as
        #'        current `Idf` object.
        #' @param .unique If there are duplications in input [IdfObject]s or
        #'        there is same object in current `Idf` object, duplications in
        #'        input are removed. Default: `TRUE`.
        #' @param .empty If `TRUE`, trailing empty fields are kept. Default:
        #'        `FALSE`.
        #'
        #' @return A named list of inserted [IdfObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # insert all material from another IDF
        #' path_idf2 <- file.path(eplus_config(8.8)$dir, "ExampleFiles/5ZoneTDV.idf")
        #' idf2 <- Idf$new(path_idf2)
        #' idf$insert(idf2$Material)
        #'
        #' # insert objects from same Idf is equivalent to using Idf$dup()
        #' idf$insert(idf$SizingPeriod_DesignDay)
        #' }
        #'
        insert = function (..., .unique = TRUE, .empty = FALSE)
            idf_insert(self, private, ..., .unique = .unique, .empty = .empty),
        # }}}

        # load {{{
        #' @description
        #' Load new objects from characters or data.frames
        #'
        #' @details
        #' `$load()` is similar to
        #' \href{../../eplusr/html/Idf.html#method-insert}{\code{$insert()}},
        #' except it takes directly character vectors or data.frames as
        #' [IdfObject] definitions, insert corresponding objects into current
        #' `Idf` object and returns a named list of newly added [IdfObject]s.
        #' The returned list will be named using names of added objects. This
        #' makes it easy to create objects using the output from`$to_string()`
        #' and `$to_table()` method from
        #' \href{../../eplusr/html/Idd.html#method-to_string}{\code{Idd}},
        #' \href{../../eplusr/html/IddObject.html#method-to_string}{\code{IddObject}},
        #' also from
        #' \href{../../eplusr/html/Idf.html#method-to_string}{\code{Idf}},
        #' and
        #' \href{../../eplusr/html/IdfObject.html#method-to_string}{\code{IdfObject}},
        #' class.
        #'
        #' For object definitions in character vector format, they follow the
        #' same rules as a normal IDF file:
        #'
        #' * Each object starts with a class name and a comma (`,`);
        #' * Separates each values with a comma (`,`);
        #' * Ends an object with a semicolon (`;`) for the last value.
        #'
        #' Each character vector can contain:
        #'
        #' * One single object, e.g. `c("Building,", "MyBuilding;")`, or
        #'   "Building, MyBuilding;".
        #' * Multiple objects, e.g. `c("Building, MyBuilding;",
        #' "SimulationControl, Yes")`.
        #'
        #' You can also provide an option header to indicate if input objects
        #' are presented in IP units, using `!-Option ViewInIPunits`. If this
        #' header does not exist, then all values are treated as in SI units.
        #'
        #' For object definitions in data.frame format, it is highly recommended
        #' to use `$to_table()` method in
        #' \href{../../eplusr/html/Idd.html#method-to_table}{\code{Idd}},
        #' [Idd],
        #' \href{../../eplusr/html/IddObject.html#method-to_table}{\code{IddObject}},
        #' [IddObject],
        #' \href{../../eplusr/html/Idf.html#method-to_table}{\code{Idf}},
        #' and
        #' \href{../../eplusr/html/IdfObject.html#method-to_table}{\code{IdfObject}},
        #' class to create an acceptable data.frame template. A
        #' valid definition requires at least three columns described below.
        #' Note that column order does not matter.
        #'
        #' * `class`:Character type. Valid class names in the underlying
        #'   [Idd] object.
        #' * `index`:Integer type. Valid field indices for each class.
        #' * `value`:Character type or list type. Value for each field
        #'   to be added.
        #'   - If character type, usually when `string_value` is `TRUE`
        #'     in method `$to_table()` in
        #'     \href{../../eplusr/html/Idf.html#method-to_table}{\code{Idf}}
        #'     and
        #'     \href{../../eplusr/html/IdfObject.html#method-to_table}{\code{IdfObject}}
        #'     class. Note that
        #'     each value should be given as a string even if the corresponding
        #'     field is a numeric type.
        #'   - If list type, usually when `string_value` is set to
        #'     `FALSE` in method`$to_table()` in
        #'     \href{../../eplusr/html/Idf.html#method-to_table}{\code{Idf}}
        #'     and
        #'     \href{../../eplusr/html/IdfObject.html#method-to_table}{\code{IdfObject}}
        #'     class.
        #'     Each value should have the right type as the corresponding field
        #'     definition.  Otherwise, errors will be issued if current
        #'     [validation level][level_checks()] includes invalid-type checking.
        #' * `id`: **Optional**. Integer type. If input data.frame includes
        #'   multiple object definitions in a same class, values in `id` column
        #'   will be used to distinguish each definition. If `id` column does
        #'   not exists, it assumes that each definition is separated by `class`
        #'   column and will issue an error if there is any duplication in the
        #'   `index` column.
        #'
        #' Note that `$load()` assumes all definitions are from the same version
        #' as current `Idf` object. If input definition is from different
        #' version, parsing error may occur.
        #'
        #' By default, trailing empty fields that are not required will be
        #' removed and only minimum required fields are kept. You can keep the
        #' trailing empty fields by setting `.empty` to `TRUE`.
        #'
        #' @param ... Character vectors or data.frames of object definitions.
        #' @param .unique If `TRUE`, and there are duplications in input
        #'        [IdfObject]s or there is same object in current `Idf` object,
        #'        duplications in input are removed. Default: `TRUE`.
        #' @param .default If `TRUE`, default values are filled for those blank
        #'        fields if possible. Default: `TRUE`.
        #' @param .empty If `TRUE`, trailing empty fields are kept. Default:
        #'        `FALSE`.
        #'
        #' @return A named list of loaded [IdfObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # load objects from character vectors
        #' idf$load(
        #'     c("Material,",
        #'       "    mat,                     !- Name",
        #'       "    MediumSmooth,            !- Roughness",
        #'       "    0.667,                   !- Thickness {m}",
        #'       "    0.115,                   !- Conductivity {W/m-K}",
        #'       "    513,                     !- Density {kg/m3}",
        #'       "    1381;                    !- Specific Heat {J/kg-K}"),
        #'
        #'     "Construction, const, mat;"
        #' )
        #'
        #' # load objects from data.frame definitions
        #' dt <- idf$to_table(class = "Material")
        #' dt[field == "Name", value := paste(value, 1)]
        #' dt[field == "Thickness", value := "0.5"]
        #' idf$load(dt)
        #'
        #' # by default, duplications are removed
        #' idf$load(idf$to_table(class = "Material"))
        #'
        #' # keep empty fields as they are
        #' idf$load("Material, mat1, smooth, 0.5, 0.2, 500, 1000,,, 0.5;", .default = FALSE)
        #'
        #' # keep trailing empty fields
        #' idf$load("Material, mat2, smooth, 0.5, 0.2, 500, 1000,,,;",
        #'     .default = FALSE, .empty = TRUE
        #' )
        #' }
        #'
        load = function (..., .unique = TRUE, .default = TRUE, .empty = FALSE)
            idf_load(self, private, ..., .unique = .unique, .default = .default, .empty = .empty),
        # }}}

        # update {{{
        #' @description
        #' Update existing object values from characters or data.frames
        #'
        #' @details
        #' `$update()` is similar to
        #' \href{../../eplusr/html/Idf.html#method-set}{\code{$set()}}, except
        #' it takes directly character vectors or data.frames as [IdfObject]
        #' definitions, updates new values for fields in objects specified, and
        #' returns a named list of modified [IdfObject]s. The returned list will
        #' be named using names of modified objects. This makes it easy to
        #' update object values using the output from `$to_string()` and
        #' `$to_table` method from
        #' \href{../../eplusr/html/Idf.html#method-to_string}{\code{Idf}},
        #' and
        #' \href{../../eplusr/html/IdfObject.html#method-to_string}{\code{IdfObject}},
        #' class.
        #'
        #' The format of object definitions is similar to `$load()`.
        #'
        #' For object definitions in character vector format, object names are
        #' used to locate which objects to update. Objects that have name
        #' attribute should have valid names. This means that there is no way to
        #' update object names using character vector format, but this can be
        #' achieved using data.frame format as it uses object IDs instead of
        #' object names to locate objects. The format of acceptable characters
        #' follows the same rules as a normal IDF file:
        #'
        #' * Each object starts with a class name and a comma (`,`);
        #' * Separates each values with a comma (`,`);
        #' * Ends an object with a semicolon (`;`) for the last value.
        #'
        #' Each character vector can contain:
        #'
        #' * One single object, e.g. `c("Building,", "MyBuilding;")`, or
        #'   "Building, MyBuilding;".
        #' * Multiple objects, e.g. `c("Building, MyBuilding;",
        #' "SimulationControl, Yes")`.
        #'
        #' You can also provide an option header to indicate if input objects
        #' are presented in IP units, using `!-Option ViewInIPunits`. If this
        #' header does not exist, then all values are treated as in SI units.
        #'
        #' For object definitions in data.frame format, it is highly recommended
        #' to use `$to_table()` method in
        #' \href{../../eplusr/html/Idf.html#method-to_table}{\code{Idf}},
        #' and
        #' \href{../../eplusr/html/IdfObject.html#method-to_table}{\code{IdfObject}},
        #' class to create an acceptable data.frame template. A valid definition
        #' requires three columns described below. Note that column order does
        #' not matter.
        #'
        #' * `id`: Integer type. Valid IDs of objects to update.
        #' * `index`:Integer type. Valid field indices for each object.
        #' * `value`:Character type or list type. Value for each field
        #'   to be added.
        #'   - If character type, usually when `string_value` is `TRUE`
        #'     in method `$to_table()` in
        #'     \href{../../eplusr/html/Idf.html#method-to_table}{\code{Idf}}
        #'     and
        #'     \href{../../eplusr/html/IdfObject.html#method-to_table}{\code{IdfObject}}
        #'     class. Note that
        #'     each value should be given as a string even if the corresponding
        #'     field is a numeric type.
        #'   - If list type, usually when `string_value` is set to
        #'     `FALSE` in method `$to_table()` in
        #'     \href{../../eplusr/html/Idf.html#method-to_table}{\code{Idf}}
        #'     and
        #'     \href{../../eplusr/html/IdfObject.html#method-to_table}{\code{IdfObject}}
        #'     class.
        #'     Each value should have the right type as the corresponding field
        #'     definition.  Otherwise, errors will be issued if current
        #'     [validation level][level_checks()] includes invalid-type checking.
        #'
        #' Note that `$update()` assumes all definitions are from the same version
        #' as current `Idf` object. If input definition is from different
        #' version, parsing error may occur.
        #'
        #' By default, trailing empty fields that are not required will be
        #' removed and only minimum required fields are kept. You can keep the
        #' trailing empty fields by setting `.empty` to `TRUE`.
        #'
        #' @param ... Character vectors or data.frames of object definitions.
        #' @param .default If `TRUE`, default values are filled for those blank
        #'        fields if possible. Default: `TRUE`.
        #' @param .empty If `TRUE`, trailing empty fields are kept. Default:
        #'        `FALSE`.
        #'
        #' @return A named list of updated [IdfObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # update objects from string definitions:
        #' str <- idf$to_string("zone one", header = FALSE, format = "new_top")
        #' str[8] <- "2," # Multiplier
        #' idf$update(str)
        #'
        #' # update objects from data.frame definitions:
        #' dt <- idf$to_table("zone one")
        #' dt[field == "Multiplier", value := "1"]
        #' idf$update(dt)
        #' }
        #'
        update = function (..., .default = TRUE, .empty = FALSE)
            idf_update(self, private, ..., .default = .default, .empty = .empty),
        # }}}

        # paste {{{
        #' @description
        #' Paste new objects from IDF Editor
        #'
        #' @details
        #' `$paste()` reads the contents (from clipboard) of copied objects from IDF
        #' Editor (after hitting `Copy Obj` button), inserts corresponding
        #' objects into current `Idf` object and returns a named list of newly
        #' added [IdfObject]s. The returned list will be named using names of
        #' added objects. As IDF Editor is only available on Windows platform,
        #' `$paste()` only works on Windows too.
        #'
        #' There is no version data copied to the clipboard when copying objects in
        #' IDF Editor. `$paste()` assumes the file open in IDF Editor has the
        #' same version as current `Idf` object. This may not be always true.
        #' Please check the version before running `$paste()`, or explicitly
        #' specify the version of file opened by IDF Editor using `ver`
        #' parameter. Parsing error may occur if there is a version mismatch.
        #'
        #' By default, trailing empty fields that are not required will be
        #' removed and only minimum required fields are kept. You can keep the
        #' trailing empty fields by setting `.empty` to `TRUE`.
        #'
        #' @param in_ip Set to `TRUE` if the IDF file is open with `Inch-Pound`
        #'        view option toggled. Numeric values will automatically
        #'        converted to SI units if necessary. Default: `FALSE`.
        #' @param ver The version of IDF file open by IDF Editor, e.g. `8.6`,
        #'        `"8.8.0"`. If `NULL`, assume that the file has the same
        #'        version as current Idf object. Default: `NULL`.
        #' @param unique If `TRUE`, and there are duplications in copied objects
        #'        from IDF Editor or there is same object in current Idf,
        #'        duplications in input are removed. Default: `TRUE`.
        #' @param empty If `TRUE`, trailing empty fields are kept. Default:
        #'        `FALSE`.
        #'
        #' @return A named list of loaded [IdfObject] objects.
        #'
        paste = function (in_ip = FALSE, ver = NULL, unique = TRUE, empty = FALSE)
            idf_paste(self, private, in_ip = in_ip, ver = ver, unique = unique, empty = empty),
        # }}}

        # search_value {{{
        #' @description
        #' Search objects by field values using regular expression
        #'
        #' @details
        #' `$search_value()` returns a list of [IdfObject]s that contain values
        #' which match the given pattern. If no matched found, `NULL` is
        #' returned invisibly. The returned list will be named using names of
        #' matched objects.
        #'
        #' Note that during matching, all values are treated as characters,
        #' including numeric values.
        #'
        #' @param pattern,ignore.case,perl,fixed,useBytes All of them are
        #'        directly passed to [base::grepl][base::grep()] and
        #'        [base::gsub][base::grep()].
        #' @param class A character vector of invalid class names in current
        #'        `Idf` object to search for values. If `NULL`, all classes are
        #'        used. Default: `NULL`.
        #'
        #' @return A named list of [IdfObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # search values that contains "floor"
        #' idf$search_value("floor", ignore.case = TRUE)
        #'
        #' # search values that contains "floor" in class Construction
        #' idf$search_value("floor", "Construction", ignore.case = TRUE)
        #' }
        #'
        search_value = function (pattern, class = NULL, ignore.case = FALSE,
                                 perl = FALSE, fixed = FALSE, useBytes = FALSE)
            idf_search_value(self, private, pattern, class, ignore.case = ignore.case,
                perl = perl, fixed = fixed, useBytes = useBytes
            ),
        # }}}

        # replace_value {{{
        #' @description
        #' Replace object field values using regular expression
        #'
        #' @details
        #' `$replace_value()` returns a list of [IdfObject]s whose values have
        #' been replaced using given pattern. If no matched found, `NULL` is
        #' returned invisibly. The returned list will be named using names of
        #' matched objects.
        #'
        #' Note that during matching, all values are treated as characters,
        #' including numeric values.
        #'
        #' Modifying object values using regular expression is not recommended.
        #' Consider to use
        #' \href{../../eplusr/html/Idf.html#method-set}{\code{$set()}}
        #' and
        #' \href{../../eplusr/html/Idf.html#method-update}{\code{$update()}}
        #' if possible.
        #' [Validation][level_checks()] rules also apply during replacing.
        #'
        #' @param pattern,replacement,ignore.case,perl,fixed,useBytes All of
        #'        them are directly passed to [base::grepl][base::grep()] and
        #'        [base::gsub][base::grep()].
        #' @param class A character vector of invalid class names in current
        #'        `Idf` object to search for values. If `NULL`, all classes are
        #'        used. Default: `NULL`.
        #'
        #' @return A named list of [IdfObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # search values that contains "win" and replace them with "windows"
        #' idf$replace_value("win", "windows")
        #' }
        #'
        replace_value = function (pattern, replacement, class = NULL, ignore.case = FALSE,
                                  perl = FALSE, fixed = FALSE, useBytes = FALSE)
            idf_replace_value(self, private, pattern, replacement, class = class,
                ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes
            ),
        # }}}

        # validate {{{
        #' @description
        #' Check possible object field value errors
        #'
        #' @details
        #' `$validate()` checks if there are errors in current `Idf` object
        #' under specified validation level and returns an `IdfValidity` object.
        #' `$validate()` is useful to help avoid some common errors before
        #' running the model. By default, validation is performed when calling
        #' all methods that modify objects, e.g.
        #' \href{../../eplusr/html/Idf.html#method-dup}{\code{$dup()}}
        #' \href{../../eplusr/html/Idf.html#method-add}{\code{$add()}},
        #' \href{../../eplusr/html/Idf.html#method-set}{\code{$set()}},
        #' \href{../../eplusr/html/Idf.html#method-del}{\code{$del()}},
        #' and etc.
        #'
        #' In total, there are 10 different validate checking components:
        #'
        #' * `required_object`: Check if required objects are missing in current
        #'   `Idf`.
        #' * `unique_object`: Check if there are multiple objects in one
        #'   unique-object class. An unique-object class means that there should
        #'   be at most only one object existing in that class.
        #' * `unique_name`: Check if all objects in each class have unique names.
        #' * `extensible`: Check if all fields in an extensible group have
        #'   values. An extensible group is a set of fields that should be
        #'   treated as a whole, such like the X, Y and Z vertices of a building
        #'   surfaces. An extensible group should be added or deleted together.
        #'   `extensible` component checks if there are some, but not all,
        #'   fields in an extensible group are empty.
        #' * `required_field`: Check if all required fields have values.
        #' * `auto_field`: Check if all fields filled with value `"Autosize"` and
        #'   `"Autocalculate"` are actual autosizable and autocalculatable
        #'   fields or not.
        #' * `type`: Check if all fields have value types complied with their
        #'   definitions, i.e. character, numeric and integer fields should be
        #'   filled with corresponding type of values.
        #' * `choice`: Check if all choice fields are filled with valid choice
        #'   values.
        #' * `range`: Check if all numeric fields have values within prescibed
        #'   ranges.
        #' * `reference`: Check if all fields whose values refer to other fields
        #'   are valid.
        #'
        #' The `level` argument controls what checkings should be performed.
        #' `level` here is just a list of 10 element which specify the toggle
        #' status of each component. You can use helper [custom_validate()] to
        #' get that list and pass it directly to `level`.
        #'
        #' There are 3 predefined validate level that indicates different
        #' combinations of checking components, i.e. `none`, `draft` and
        #' `final`. Basically, `none` level just does not perform any
        #' checkings; `draft` includes 5 components, i.e. `auto_field`, `type`,
        #' `unique_name`, `choice` and `range`; and `final` level includes all
        #' 10 components. You can always get what components each level contains
        #' using [level_checks()]. By default, the result from
        #' `eplusr_option("validate_level")` is passed to `level`. If not set,
        #' `final` level is used.
        #'
        #' Underneath, an `IdfValidity` object which `$validate()` returns is a
        #' list of 13 element as shown below. Each element or several elements
        #' represents the results from a single validation checking component.
        #'
        #' * `missing_object`: Result of `required_object` checking.
        #' * `duplicate_object`: Result of `unique_object` checking.
        #' * `conflict_name`: Result of `unique_name` checking.
        #' * `incomplete_extensible`: Result of `extensible` checking.
        #' * `missing_value`: Result of `required_field` checking.
        #' * `invalid_autosize`: Result of `auto_field` checking for invalid
        #'   `Autosize` field values.
        #' * `invalid_autocalculate`: Result of `auto_field` checking for
        #'   invalid `Autocalculate` field values.
        #' * `invalid_character`: Result of `type` checking for invalid
        #'   character field values.
        #' * `invalid_numeric`: Result of `type` checking for invalid
        #'   numeric field values.
        #' * `invalid_integer`: Result of `type` checking for invalid
        #'   integer field values.
        #' * `invalid_choice`: Result of `choice` checking.
        #' * `invalid_range`: Result of `range` checking.
        #' * `invalid_reference`: Result of `reference` checking.
        #'
        #' Except `missing_object`, which is a character vector of class names
        #' that are missing, all other elements are
        #' [data.table][data.table::data.table()] with 9 columns containing data
        #' of invalid field values:
        #'
        #' * `object_id`: IDs of objects that contain invalid values
        #' * `object_name`: names of objects that contain invalid values
        #' * `class_id`: indexes of classes that invalid objects belong to
        #' * `class_name`: names of classes that invalid objects belong to
        #' * `field_id`: indexes (at Idd level) of object fields that are invalid
        #' * `field_index`: indexes of object fields in corresponding that are invalid
        #' * `field_name`: names (without units) of object fields that are invalid
        #' * `units`: SI units of object fields that are invalid
        #' * `ip_units`: IP units of object fields that are invalid
        #' * `type_enum`: An integer vector indicates types of invalid fields
        #' * `value_id`: indexes (at Idf level) of object field values that are invalid
        #' * `value_chr`: values (converted to characters) of object fields that are
        #'   invalid
        #' * `value_num`: values (converted to numbers in SI units) of object fields
        #'    that are invalid
        #'
        #' Knowing the internal structure of `IdfValidity`, it is easy to extract
        #' invalid [IdfObject]s you interested in. For example, you can get all IDs of
        #' objects that contain invalid value references using
        #' `model$validate()$invalid_reference$object_id`. Then using
        #' \href{../../eplusr/html/Idf.html#method-set}{\code{$set()}}
        #' method to correct them.
        #'
        #' Different validate result examples are shown below:
        #'
        #' * No error is found:
        #'
        #'   ```
        #'   v No error found.
        #'   ```
        #'
        #'   Above result shows that there is no error found after conducting all
        #'   validate checks in specified validate level.
        #'
        #' * Errors are found:
        #'
        #'   ```
        #'    x [2] Errors found during validation.
        #'   =========================================================================
        #'
        #'   -- [2] Invalid Autocalculate Field --------------------------------------
        #'      Fields below cannot be `autocalculate`:
        #'
        #'       Class: <AirTerminal:SingleDuct:VAV:Reheat>
        #'       \- Object [ID:176] <SPACE5-1 VAV Reheat>
        #'          +- 17: AUTOCALCULATE, !- Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}
        #'          \- 18: AUTOCALCULATE; !- Maximum Flow Fraction During Reheat
        #'   ```
        #'
        #'   Above result shows that after all validate components
        #'   performed under current validate level, 2 invalid field values
        #'   are found. All of them are in a object named `SPACE5-1 VAV Reheat`
        #'   with ID `176`. They are invalid because those two fields do not
        #'   have an autocalculatable attribute but are given `AUTOCALCULATE`
        #'   value. Knowing this info, one simple way to fix the
        #'   error is to correct those two fields by doing:
        #'
        #'   ```
        #'   idf$set(..176 =
        #'       list(`Maximum Flow per Zone Floor Area During Reheat` = "autosize",
        #'            `Maximum Flow Fraction During Reheat` = "autosize"
        #'       )
        #'   )
        #'   ```
        #'
        #' @param level One of `"none"`, `"draft"`, `"final"` or a list of 10
        #'        elements with same format as [custom_validate()] output.
        #'
        #' @return An `IdfValidity` object.
        #'
        #' @examples
        #' \dontrun{
        #' idf$validate()
        #'
        #' # check at predefined validate level
        #' idf$validate("none")
        #' idf$validate("draft")
        #' idf$validate("final")
        #'
        #' # custom validate checking components
        #' idf$validate(custom_validate(auto_field = TRUE, choice = TRUE))
        #' }
        #'
        validate = function (level = eplusr_option("validate_level"))
            idf_validate(self, private, level),
        # }}}

        # is_valid {{{
        #' @description
        #' Check if there is any error in current `Idf`
        #'
        #' @details
        #' `$is_valid()` checks if there are errors in current `Idf` object
        #' under specified validation level and returns `TRUE` or `FALSE`
        #' accordingly. For detailed description on validate checking, see
        #' \href{../../eplusr/html/Idf.html#method-validate}{\code{$validate()}}
        #' documentation above.
        #'
        #' @param level One of `"none"`, `"draft"`, `"final"` or a list of 10
        #'        elements with same format as [custom_validate()] output.
        #'
        #' @return A single logical value of `TRUE` or `FALSE`.
        #'
        #' @examples
        #' \dontrun{
        #' idf$is_valid()
        #'
        #' # check at predefined validate level
        #' idf$is_valid("none")
        #' idf$is_valid("draft")
        #' idf$is_valid("final")
        #'
        #' # custom validate checking components
        #' idf$is_valid(custom_validate(auto_field = TRUE, choice = TRUE))
        #' }
        #'
        is_valid = function (level = eplusr_option("validate_level"))
            idf_is_valid(self, private, level),
        # }}}

        # to_string {{{
        #' @description
        #' Format `Idf` as a character vector
        #'
        #' @details
        #' `$to_string()` returns the text format of parts or whole `Idf`
        #' object.
        #'
        #' @param which Either an integer vector of valid object IDs or a
        #'        character vector of valid object names. If `NULL`, the whole
        #'        `Idf` object is converted. Default: `NULL`.
        #' @param class A character vector of class names. If `NULL`, all
        #'        classed in current `Idf` object is converted. Default: `NULL`.
        #' @param comment If `FALSE`, all comments will not be included.
        #'        Default: `TRUE`.
        #' @param header If `FALSE`, the header will not be included. Default:
        #'        `TRUE`.
        #' @param format Specific format used when formatting. Should be one of
        #'        `"asis"`, `"sorted"`, `"new_top"`, and `"new_bot"`.
        #'   * If `"asis"`, `Idf` object will be formatted in the same way as it
        #'     was when first read. If `Idf` object does not contain any format
        #'     saving option, which is typically the case when the model was not
        #'     saved using eplusr or IDFEditor, `"sorted"` will be used.
        #'   * `"sorted"`, `"new_top"` and `"new_bot"` are the same as the save
        #'     options `"Sorted"`, `"Original with New at Top"`, and `"Original
        #'     with New at Bottom"` in IDFEditor. Default:
        #'     `eplusr_option("save_format")`.
        #' @param leading Leading spaces added to each field. Default: `4L`.
        #' @param sep_at The character width to separate value string and field
        #'        string. Default: `29L` which is the same as IDF Editor.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get text format of the whole Idf
        #' head(idf$to_string())
        #'
        #' # get text format of the whole Idf, excluding the header and all comments
        #' head(idf$to_string(comment = FALSE, header = FALSE))
        #'
        #' # get text format of all objects in class Material
        #' head(idf$to_string(class = "Material", comment = FALSE, header = FALSE))
        #'
        #' # get text format of some objects
        #' head(idf$to_string(c("floor", "zone one")))
        #'
        #' # tweak output formatting
        #' head(idf$to_string("floor", leading = 0, sep_at = 0))
        #' }
        #'
        to_string = function (which = NULL, class = NULL, comment = TRUE,
                              header = TRUE, format = eplusr_option("save_format"),
                              leading = 4L, sep_at = 29L)
            idf_to_string(self, private, which, class, comment = comment,
                          header = header, format = format,
                          leading = leading, sep_at = sep_at),
        # }}}

        # to_table {{{
        #' @description
        #' Format `Idf` as a data.frame
        #'
        #' @details
        #' `$to_table()` returns a [data.table][data.table::data.table()] that
        #' contains core data of specified objects.
        #' The returned [data.table][data.table::data.table()] has 6 columns:
        #'
        #' * `id`: Integer type. Object IDs.
        #' * `name`: Character type. Object names.
        #' * `class`: Character type. Current class name.
        #' * `index`: Integer type. Field indexes.
        #' * `field`: Character type. Field names.
        #' * `value`: Character type if `string_value` is `TRUE` or list type if
        #'   `string_value` is `FALSE` or `group_ext` is not `"none"`. Field values.
        #'
        #' Note that when `group_ext` is not `"none"`, `index` and `field`
        #' values will not match the original field indices and names. In this
        #' case, `index` will only indicate the indices of sequences. For
        #' `field` column, specifically:
        #'
        #' * When `group_ext` is `"group"`, each field name in a extensible group
        #'   will be abbreviated using [abbreviate()] with `minlength` being
        #'   `10L` and all abbreviated names will be separated by `|` and
        #'   combined together. For example, field names in the extensible group
        #'   (`Vertex 1 X-coordinate`, `Vertex 1 Y-coordinate`, `Vertex 1
        #'   Z-coordinate`) in class `BuildiBuildingSurface:Detailed` will be
        #'   merged into one name `Vrtx1X-crd|Vrtx1Y-crd|Vrtx1Z-crd`.
        #' * When `group_ext` is `"index"`, the extensible group indicator in field
        #'   names will be removed. Take the same example as above, the
        #'   resulting field names will be `Vertex X-coordinate`, `Vertex
        #'   Y-coordinate`, and `Vertex Z-coordinate`.
        #'
        #' @param which Either an integer vector of valid object IDs or a
        #'        character vector of valid object names. If `NULL`, the whole
        #'        `Idf` object is converted. Default: `NULL`.
        #' @param class A character vector of class names. If `NULL`, all
        #'        classed in current `Idf` object is converted. Default: `NULL`.
        #' @param string_value If `TRUE`, all field values are returned as
        #'        character. If `FALSE`, `value` column in returned
        #'        [data.table][data.table::data.table()] is a list column with
        #'        each value stored as corresponding type. Note that if the
        #'        value of numeric field is set to `"Autosize"` or
        #'        `"Autocalculate"`, it is left as it is, leaving the returned
        #'        type being a string instead of a number. Default: `TRUE`.
        #' @param unit Only applicable when `string_value` is `FALSE`. If
        #'        `TRUE`, values of numeric fields are assigned with units using
        #'        [units::set_units()] if applicable. Default: `FALSE`.
        #' @param wide Only applicable if target objects belong to a same class.
        #'        If `TRUE`, a wide table will be returned, i.e. first three
        #'        columns are always `id`, `name` and `class`, and then every
        #'        field in a separate column. Note that this requires all
        #'        objects specified must from the same class.
        #'        Default: `FALSE`.
        #' @param align If `TRUE`, all objects in the same class will have the
        #'        same field number. The number of fields is the same as the
        #'        object that have the most fields among objects specified.
        #'        Default: `FALSE`.
        #' @param all If `TRUE`, all available fields defined in IDD for the
        #'        class that objects belong to will be returned. Default:
        #'        `FALSE`.
        #' @param group_ext Should be one of `"none"`, `"group"` or `"index"`.
        #'        If not `"none"`, `value` column in returned
        #'        [data.table::data.table()] will be converted into a list.
        #'        If `"group"`, values from extensible fields will be grouped by the
        #'        extensible group they belong to. For example, coordinate
        #'        values of each vertex in class `BuildingSurface:Detailed` will
        #'        be put into a list. If `"index"`, values from extensible fields
        #'        will be grouped by the extensible field indice they belong to.
        #'        For example, coordinate values of all x coordinates will be
        #'        put into a list. If `"none"`, nothing special will be done.
        #'        Default: `"none"`.
        #'
        #' @return A [data.table][data.table::data.table()] with 6 columns (if
        #' `wide` is `FALSE`) or at least 6 columns (if `wide` is `TRUE`).
        #'
        #' @examples
        #' \dontrun{
        #' # extract whole Idf data
        #' idf$to_table()
        #'
        #' # extract all data from class Material
        #' idf$to_table(class = "Material")
        #'
        #' # extract multiple object data
        #' idf$to_table(c("FLOOR", "ZONE ONE"))
        #'
        #' # keep value types and put actual values into a list column
        #' idf$to_table(c("FLOOR", "ZONE ONE"), string_value = FALSE)$value
        #'
        #' # add the unit to each value
        #' idf$to_table(c("FLOOR", "ZONE ONE"), string_value = FALSE, unit = TRUE)
        #'
        #' # get all possible fields
        #' idf$to_table("ZONE ONE", all = TRUE)
        #'
        #' # make sure all objects in same class have the same number of fields
        #' idf$to_table(class = "Construction", align = TRUE)
        #'
        #' # get a wide table with string values
        #' idf$to_table(class = "Construction", wide = TRUE)
        #'
        #' # get a wide table with actual values
        #' idf$to_table(class = "OtherEquipment", wide = TRUE, string_value = FALSE)
        #'
        #' # group extensible by extensible group number
        #' idf$to_table(class = "BuildingSurface:Detailed", group_ext = "group")
        #'
        #' # group extensible by extensible group number and convert into a wide table
        #' idf$to_table(class = "BuildingSurface:Detailed", group_ext = "group", wide = TRUE)
        #'
        #' # group extensible by extensible field index
        #' idf$to_table(class = "BuildingSurface:Detailed", group_ext = "index")
        #'
        #' # group extensible by extensible field index and convert into a wide table
        #' idf$to_table(class = "BuildingSurface:Detailed", group_ext = "index", wide = TRUE)
        #'
        #' # when grouping extensible, 'string_value' and 'unit' still take effect
        #' idf$to_table(class = "BuildingSurface:Detailed", group_ext = "index",
        #'     wide = TRUE, string_value = FALSE, unit = TRUE
        #' )
        #' }
        #'
        to_table = function (which = NULL, class = NULL, string_value = TRUE,
                             unit = FALSE, wide = FALSE, align = FALSE, all = FALSE, group_ext = c("none", "group", "index"))
            idf_to_table(self, private, which = which, class = class,
                string_value = string_value, unit = unit, wide = wide, align = align, all = all, group_ext = match.arg(group_ext)),
        # }}}

        # is_unsaved {{{
        #' @description
        #' Check if there are unsaved changes in current `Idf`
        #'
        #' @details
        #' `$is_unsaved()` returns `TRUE` if there are modifications on the
        #' model since it was read or since last time it was saved, and returns
        #' `FALSE` otherwise.
        #'
        #' @return A single logical value of `TRUE` or `FALSE`.
        #'
        #' @examples
        #' \dontrun{
        #' idf$is_unsaved()
        #' }
        #'
        is_unsaved = function ()
            idf_is_unsaved(self, private),
        # }}}

        # save {{{
        #' @description
        #' Save `Idf` object as an IDF file
        #'
        #' @details
        #' `$save()` formats current `Idf` object, saves it as an IDF file and
        #' returns the path of saved file invisibly. After saving,
        #' \href{../../eplusr/html/Idf.html#method-path}{\code{$path()}}
        #' will also be updated to return the path of saved file.
        #'
        #' @param path A path where to save the IDF file. If `NULL`, the path of
        #'        the `Idf` itself, i.e.
        #'        \href{../../eplusr/html/Idf.html#method-path}{\code{$path()}},
        #'        will be used.
        #' @param format Specific format used when formatting. Should be one of
        #'        `"asis"`, `"sorted"`, `"new_top"`, and `"new_bot"`.
        #'   * If `"asis"`, `Idf` object will be formatted in the same way as it
        #'     was when first read. If `Idf` object does not contain any format
        #'     saving option, which is typically the case when the model was not
        #'     saved using eplusr or IDFEditor, `"sorted"` will be used.
        #'   * `"sorted"`, `"new_top"` and `"new_bot"` are the same as the save
        #'     options `"Sorted"`, `"Original with New at Top"`, and `"Original
        #'     with New at Bottom"` in IDFEditor. Default:
        #'     `eplusr_option("save_format")`.
        #' @param overwrite Whether to overwrite the file if it already exists.
        #'        Default: `FALSE`.
        #' @param copy_external If `TRUE`, the external files that current `Idf`
        #'        object depends on will also be copied into the same directory.
        #'        The values of file paths in the `Idf` will be changed into
        #'        relative path automatically. This makes it possible to create
        #'        fully reproducible simulation conditions. Currently, only
        #'        `Schedule:File` class is supported. Default: `FALSE`.
        #'
        #' @return A length-one character vector, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # save Idf as a new file
        #' idf$save(tempfile(fileext = ".idf"))
        #'
        #' # save and overwrite current file
        #' idf$save(overwrite = TRUE)
        #'
        #' # save the model with newly created and modified objects at the top
        #' idf$save(overwrite = TRUE, format = "new_top")
        #'
        #' # save the model to a new file and copy all external csv files used in
        #' # "Schedule:File" class into the same folder
        #' idf$save(path = file.path(tempdir(), "test1.idf"), copy_external = TRUE)
        #' }
        #'
        save = function (path = NULL, format = eplusr_option("save_format"), overwrite = FALSE, copy_external = TRUE)
            idf_save(self, private, path, format = format, overwrite = overwrite, copy_external = copy_external),
        # }}}

        # run {{{
        #' @description
        #' Run simulation using EnergyPlus
        #'
        #' @details
        #' `$run()` calls corresponding version of EnergyPlus to run the current
        #' `Idf` object together with specified weather. The model and the
        #' weather used will be copied into the output directory. An [EplusJob]
        #' object is returned which provides detailed info of the simulation and
        #' methods to collect simulation results. Please see [EplusJob] for
        #' details.
        #'
        #' eplusr uses the EnergyPlus command line interface which was
        #' introduced since EnergyPlus 8.3.0. So `$run()` only supports models
        #' with version no lower than 8.3.0.
        #'
        #' When calling `$run()`, eplusr will do steps below to make sure the
        #' output collecting methods work as expected. Please note that this may
        #' result in an IDF file that may not be exactly same as your current
        #' `Idf` object.
        #'
        #' * eplusr uses EnergyPlus SQL output for extracting simulation
        #'   results. In order to do so, an object in `Output:SQLite` class with
        #'   `Option Type` value being `SimpleAndTabular` will be automatically
        #'   created if it does not exists.
        #' * In order to make sure `.rdd` (Report Data Dictionary) and `.mdd`
        #'   (Meter Data Dictionary) files are created during simulation, an
        #'   object in `Output:VariableDictionary` class with `Key Field` value
        #'   being `IDF` will be automatically created if it does not exists.
        #'
        #' @param weather A path to an `.epw` file or an [Epw] object. `weather`
        #'        can also be `NULL` which will force design-day-only
        #'        simulation. Note that this needs at least one
        #'        `Sizing:DesignDay` object exists in the `Idf`.
        #' @param dir The directory to save the simulation results. If `NULL`,
        #'        the folder of `Idf` path will be used. Default: `NULL`.
        #' @param wait Whether to wait until the simulation completes and print
        #'        the standard output and error of EnergyPlus. If `FALSE`, the
        #'        simulation will run in the background. Default is `TRUE`.
        #' @param force Only applicable when the last simulation runs with
        #'        `wait` equals to `FALSE` and is still running. If `TRUE`,
        #'        current running job is forced to stop and a new one will
        #'        start. Default: `FALSE`.
        #' @param copy_external If `TRUE`, the external files that current `Idf`
        #'        object depends on will also be copied into the simulation
        #'        output directory. The values of file paths in the Idf will be
        #'        changed automatically. Currently, only `Schedule:File` class
        #'        is supported.  This ensures that the output directory will
        #'        have all files needed for the model to run. Default is
        #'        `FALSE`.
        #' @param echo Only applicable when `wait` is `TRUE`. Whether to show
        #'        standard output and error from EnergyPlus. Default: same as
        #'        `wait`.
        #'
        #' @return An [EplusJob] object of current simulation.
        #'
        #' @examples
        #' \dontrun{
        #' idf <- Idf$new(path_idf)
        #' # save the model to tempdir()
        #' idf$save(file.path(tempdir(), "test_run.idf"))
        #'
        #' # use the first epw file in "WeatherData" folder in EnergyPlus v8.8
        #' # installation path
        #' epw <- list.files(file.path(eplus_config(8.8)$dir, "WeatherData"),
        #'     pattern = "\\.epw$", full.names = TRUE)[1]
        #'
        #' # if `dir` is NULL, the directory of IDF file will be used as simulation
        #' # output directory
        #' job <- idf$run(epw, dir = NULL)
        #'
        #' # run simulation in the background
        #' idf$run(epw, dir = tempdir(), wait = FALSE)
        #'
        #' # copy all external files into the directory run simulation
        #' idf$run(epw, dir = tempdir(), copy_external = TRUE)
        #'
        #' # check for simulation errors
        #' job$errors()
        #'
        #' # get simulation status
        #' job$status()
        #'
        #' # get output directory
        #' job$output_dir()
        #'
        #' # re-run the simulation
        #' job$run()
        #'
        #' # get simulation results
        #' job$report_data()
        #' }
        #'
        run = function (weather, dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait)
            idf_run(self, private, weather, dir, wait, force, copy_external = copy_external, echo),
        # }}}

        # last_job {{{
        #' @description
        #' Get the last simulation job
        #'
        #' @details
        #' `$last_job()` returns the last [EplusJob] object that was created
        #' using
        #' \href{../../eplusr/html/Idf.html#method-run}{\code{$run()}}. If the
        #' `Idf` hasn't been run yet, `NULL` is returned.
        #'
        #' @return `NULL` or an [EplusJob] object.
        #'
        #' @examples
        #' \dontrun{
        #' idf$last_job()
        #' }
        #'
        last_job = function ()
            idf_last_job(self, private),
        # }}}

        # print {{{
        #' @description
        #' Print `Idf` object
        #'
        #' @details
        #' `$print()` prints the `Idf` object according to different detail
        #' level specified using the `zoom` argument.
        #'
        #' With the default `zoom` level `object`, contents of the `Idf` object
        #' is printed in a similar style as you see in IDF Editor, with
        #' additional heading lines showing `Path`, `Version` of the `Idf`
        #' object. Class names of objects are ordered by group and the number of
        #' objects in classes are shown in square bracket.
        #'
        #' @param zoom Control how detailed of the Idf object should be printed.
        #'        Should be one of `"group"`, `"class"`, `"object"` and
        #'        `"field"`. Default: `"group"`.
        #'   * `"group"`: all group names current existing are shown with prevailing
        #'     square bracket showing how many \strong{C}lasses existing in that group.
        #'   * `"class"`: all class names are shown with prevailing square bracket
        #'     showing how many \strong{O}bjects existing in that class, together with
        #'     parent group name of each class.
        #'   * `"object"`: all object IDs and names are shown, together with parent
        #'     class name of each object.
        #'   * `"field"`: all object IDs and names, field names and values are shown,
        #'     together with parent class name of each object.
        #'
        #' @param order Only applicable when `zoom` is `"object"` or `"field"`.
        #'        If `TRUE`, objects are shown as the same order in the IDF. If
        #'        `FALSE`, objects are grouped and ordered by classes. Default:
        #'        `TRUE`.
        #'
        #' @return The `Idf` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' idf$print("group")
        #' idf$print("class")
        #' idf$print("object")
        #' idf$print("field")
        #'
        #' # order objects by there classes
        #' idf$print("object", order = FALSE)
        #' idf$print("field", order = FALSE)
        #' }
        #'
        print = function (zoom = "class", order = TRUE)
            idf_print(self, private, zoom, order)
        # }}}
        # }}}

    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_path = NULL,
        m_version = NULL,
        m_idd = NULL,
        m_idf_env = NULL,
        m_log = NULL,
        # }}}

        idd_env = function () {
            ._get_private(private$m_idd)$m_idd_env
        },

        idf_env = function () {
            private$m_idf_env
        },

        log_env = function () {
            private$m_log
        },

        deep_clone = function (name, value) {
            idf_deep_clone(self, private, name, value)
        }
    )
)

# set deep default value to `TRUE`
formals(Idf$clone_method)$deep <- TRUE
formals(Idf$public_methods$clone)$deep <- TRUE

# Manually remove IDF clss active bindings before cloning. See #164
b <- as.list(body(Idf$clone_method))
b <- as.call(c(
    list(b[[1]],
         quote(enclosing <- .subset2(self, ".__enclos_env__")),
         quote(rm(list = grep("^[A-Z]", names(enclosing$self), value = TRUE), envir = enclosing$self))
    ),
    b[-1]
))
body(Idf$clone_method) <- b
body(Idf$public_methods$clone) <- b
# }}}

# idf_version {{{
idf_version <- function (self, private) {
    private$m_version
}
# }}}
# idf_path {{{
idf_path <- function (self, private) {
    private$m_path
}
# }}}
# idf_group_name {{{
idf_group_name <- function (self, private, all = FALSE, sorted = TRUE) {
    if (all) {
        get_idd_group_name(private$idd_env())
    } else {
        grp <- private$idd_env()$class[private$idf_env()$object, on = "class_id", group_id]
        if (sorted) {
            get_idd_group_name(private$idd_env(), sort(unique(grp)))
        } else {
            get_idd_group_name(private$idd_env(), grp)
        }
    }
}
# }}}
# idf_class_name {{{
idf_class_name <- function (self, private, all = FALSE, sorted = TRUE, by_group = FALSE) {
    if (all) {
        if (!by_group) return(private$idd_env()$class$class_name)
        cls <- get_idd_class(private$idd_env(), property = "group_name")
        res <- cls[, list(class_name = list(class_name)), by = "group_name"]
        setattr(res$class_name, "names", res$group_name)[]
    } else {
        add_class_name(private$idd_env(), private$idf_env()$object)
        on.exit(set(private$idf_env()$object, NULL, "class_name", NULL), add = TRUE)
        if (sorted) {
            if (!by_group) {
                private$idf_env()$object[order(class_id), unique(class_name)]
            } else {
                cls <- get_idd_class(private$idd_env(), sort(unique(private$idf_env()$object$class_id)),
                    property = "group_name")
                res <- cls[, list(class_name = list(class_name)), by = "group_name"]
                setattr(res$class_name, "names", res$group_name)[]
            }
        } else {
            private$idf_env()$object$class_name
        }
    }
}
# }}}
# idf_object_id {{{
idf_object_id <- function (self, private, class = NULL, simplify = TRUE) {
    get_idf_object_id(private$idd_env(), private$idf_env(), class, simplify)
}
# }}}
# idf_object_name {{{
idf_object_name <- function (self, private, class = NULL, simplify = FALSE) {
    get_idf_object_name(private$idd_env(), private$idf_env(), class, simplify)
}
# }}}
# idf_object_num {{{
idf_object_num <- function (self, private, class = NULL) {
    get_idf_object_num(private$idd_env(), private$idf_env(), class)
}
# }}}
# idf_is_valid_group_name {{{
idf_is_valid_group_name <- function (self, private, group, all = FALSE) {
    assert(is.character(group), msg = "`group` should be a character vector.")
    group %in% idf_group_name(self, private, all, FALSE)
}
# }}}
# idf_is_valid_class_name {{{
idf_is_valid_class_name <- function (self, private, class, all = FALSE) {
    assert(is.character(class), msg = "`class` should be a character vector.")
    class %in% idf_class_name(self, private, all, FALSE)
}
# }}}
# idf_is_valid_object_id {{{
idf_is_valid_object_id <- function (self, private, id) {
    assert(are_count(id))
    id %in% idf_object_id(self, private, NULL, simplify = TRUE)
}
# }}}
# idf_is_valid_object_name {{{
idf_is_valid_object_name <- function (self, private, name) {
    assert(is.character(name), msg = "`name` should be a character vector.")
    stri_trans_tolower(name) %chin% private$idf_env()$object[!is.na(object_name), object_name_lower]
}
# }}}
# idf_is_unsaved {{{
idf_is_unsaved <- function (self, private) {
    private$m_log$unsaved
}
# }}}
# idf_definition {{{
idf_definition <- function (self, private, class) {
    IddObject$new(class, private$m_idd)
}
# }}}
# idf_obj {{{
idf_obj <- function (self, private, which) {
    assert(!is.null(which), is_scalar(which))
    obj <- get_idf_object(private$idd_env(), private$idf_env(), class = NULL, object = which,
        ignore_case = TRUE
    )

    add_idfobj_field_bindings(IdfObject$new(obj$object_id, obj$class_id, parent = self))
}
# }}}
# idf_object_unique {{{
idf_object_unique <- function (self, private, class) {
    assert(is_scalar(class))
    obj <- get_idf_object(private$idd_env(), private$idf_env(), class)

    if (!unique(obj$class_id) %in% private$idd_env()$class[unique_object == TRUE, class_id]) {
        abort("error_idf_not_unique_class",
            paste0(surround(unique(obj$class_name)), " is not a valid unique-object class index or name.")
        )
    }

    if (nrow(obj) > 1L) {
        abort("error_idf_dup_unique_class",
            paste0(surround(unique(obj$class_name)), " class have more than one ",
                "objects:\n",
                get_object_info(obj[, rleid := .I], c("id", "name"), collapse = "\n"),
                "\nPlease see `$validate()` for more details."
            )
        )
    }

    add_idfobj_field_bindings(IdfObject$new(obj$object_id, obj$class_id, parent = self))
}
# }}}
# idf_objects {{{
idf_objects <- function (self, private, which) {
    assert(!is.null(which))
    obj <- get_idf_object(private$idd_env(), private$idf_env(), class = NULL, object = which,
        ignore_case = TRUE
    )

    res <- apply2(obj$object_id, obj$class_id, IdfObject$new, list(parent = self))
    res <- lapply(res, add_idfobj_field_bindings)
    setattr(res, "names", obj$object_name)
    res
}
# }}}
# idf_object_in_class {{{
idf_object_in_class <- function (self, private, class) {
    .deprecated_fun("$object_in_class()", "$objects_in_class()", "Idf", "0.10.0")
    idf_objects_in_class(self, private, class)
}
# }}}
# idf_objects_in_class {{{
idf_objects_in_class <- function (self, private, class) {
    assert(is_string(class))
    obj <- get_idf_object(private$idd_env(), private$idf_env(), class)

    res <- apply2(obj$object_id, obj$class_id, IdfObject$new, list(parent = self))
    res <- lapply(res, add_idfobj_field_bindings)
    setattr(res, "names", obj$object_name)
    res
}
# }}}
# idf_objects_in_group {{{
idf_objects_in_group <- function (self, private, group) {
    assert(is_string(group))

    add_joined_cols(private$idd_env()$class, private$idf_env()$object, "class_id", "group_id")
    add_joined_cols(private$idd_env()$group, private$idf_env()$object, "group_id", "group_name")
    on.exit(set(private$idf_env()$object, NULL, c("group_id", "group_name"), NULL), add = TRUE)

    grp_in <- recognize_input(group, "group")

    obj <- join_from_input(private$idf_env()$object, grp_in, "group_id")

    res <- apply2(obj$object_id, obj$class_id, IdfObject$new, list(parent = self))
    res <- lapply(res, add_idfobj_field_bindings)
    setattr(res, "names", obj$object_name)
    res
}
# }}}
# idf_object_relation {{{
idf_object_relation <- function (self, private, which,
                                 direction = c("all", "ref_to", "ref_by", "node"),
                                 recursive = FALSE, recursive_depth = 1L) {
    assert(is_scalar(which))

    obj <- get_idf_object(private$idd_env(), private$idf_env(),
        object = which, ignore_case = TRUE
    )

    get_idfobj_relation(private$idd_env(), private$idf_env(),
        object_id = obj$object_id, name = TRUE, direction = direction,
        keep_all = FALSE, by_value = FALSE, max_depth = NULL,
        recursive = recursive, recursive_depth = recursive_depth
    )
}
# }}}
# idf_objects_in_relation {{{
idf_objects_in_relation <- function (self, private, which, direction = c("ref_to", "ref_by", "node"),
                                     class = NULL, recursive = FALSE, recursive_depth = 1L) {
    assert(is_scalar(which))
    direction <- match.arg(direction)

    obj <- get_idf_object(private$idd_env(), private$idf_env(), object = which, ignore_case = TRUE)
    rel <- get_idfobj_relation(private$idd_env(), private$idf_env(), obj$object_id,
        name = FALSE, max_depth = NULL, direction = direction,
        recursive = recursive, recursive_depth = recursive_depth
    )

    # only include specified class
    if (!is.null(class)) {
        cls <- get_idd_class(private$idd_env(), class)

        if (direction == "ref_to") {
            add_joined_cols(private$idf_env()$object, rel$ref_to, c(src_object_id = "object_id"), c(src_class_id = "class_id"))
            rel$ref_to <- rel$ref_to[J(cls$class_id), on = "src_class_id"]
        } else {
            add_joined_cols(private$idf_env()$object, rel[[direction]], "object_id", "class_id")
            rel[[direction]] <- rel[[direction]][J(cls$class_id), on = "class_id"]
        }
    }

    id_self <- obj$object_id
    if (direction == "ref_to") {
        id_ref <- unique(rel$ref_to$src_object_id[!is.na(rel$ref_to$src_object_id)])
    } else {
        id_ref <- unique(rel[[direction]]$object_id[!is.na(rel[[direction]]$object_id)])
    }

    obj_self <- list(IdfObject$new(id_self, obj$class_id, self))
    setattr(obj_self, "names", obj$object_name)

    if (!length(id_ref)) {
        dir <- switch(direction, ref_to = "does not refer to", ref_by = "is not referred by",
            node = "has no node or their nodes have no reference to"
        )
        msg <- paste0(get_object_info(obj, numbered = FALSE), " ", dir, " any other object")
        if (is.null(class)) {
            verbose_info(paste0(msg, "."))
        } else {
            verbose_info(paste0(msg, " in class ", collapse(cls$class_name), "."))
        }
        return(obj_self)
    }

    res <- c(obj_self, lapply(id_ref, IdfObject$new, parent = self))
    res <- lapply(res, add_idfobj_field_bindings)

    ref_nm <- private$idf_env()$object[J(id_ref), on = "object_id", object_name]

    setattr(res, "names", c(obj$object_name, ref_nm))

    res
}
# }}}
# idf_search_object {{{
idf_search_object <- function (self, private, pattern, class = NULL, ignore.case = FALSE,
                               perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    if (!is.null(class) && anyDuplicated(class)) {
        abort("error_search_object_dup_class",
            "Class should not contain any duplication.", class = class
        )
    }

    obj <- get_idf_object(private$idd_env(), private$idf_env(), class)

    obj <- obj[grepl(pattern, object_name, ignore.case = ignore.case, perl = perl,
        fixed = fixed, useBytes = useBytes)
    ]

    if (!nrow(obj)) {
        verbose_info("No matched result found.")
        return(invisible())
    }

    res <- apply2(obj$object_id, obj$class_id, IdfObject$new, list(parent = self))
    res <- lapply(res, add_idfobj_field_bindings)
    setattr(res, "names", obj$object_name)
    res
}
# }}}
# idf_return_modified {{{
idf_return_modified <- function (self, private, modified) {
    res <- apply2(modified$object$object_id, modified$object$class_id, IdfObject$new, list(parent = self))
    res <- lapply(res, add_idfobj_field_bindings)
    setattr(res, "names", modified$object$object_name)
    res
}
# }}}
# idf_dup {{{
idf_dup <- function (self, private, ...) {
    dup <- dup_idf_object(private$idd_env(), private$idf_env(), ...)
    merge_idf_data(private$idf_env(), dup)

    # log
    log_new_order(private$m_log, dup$object$object_id)
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)

    idf_return_modified(self, private, dup)
}
# }}}
# idf_dup_object {{{
idf_dup_object <- function (self, private, object, new_name = NULL) {
    .deprecated_fun("$dup_object()", "$dup()", "Idf", "0.10.0")
    idf_dup(self, private, setattr(object, "names", new_name))
}
# }}}
# idf_add {{{
idf_add <- function (self, private, ..., .default = TRUE, .all = FALSE, .env = parent.frame(2)) {
    add <- add_idf_object(private$idd_env(), private$idf_env(), ..., .default = .default, .all = .all, .env = .env)
    merge_idf_data(private$idf_env(), add)

    # log
    log_new_order(private$m_log, add$object$object_id)
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)

    idf_return_modified(self, private, add)
}
# }}}
# idf_add_object {{{
idf_add_object <- function (self, private, class, value = NULL, comment = NULL, default = TRUE, all = FALSE) {
    .deprecated_fun("$add_object()", "$add()", "Idf", "0.10.0")
    input <- old_input(class, value, comment, "add")
    idf_add(self, private, input, .default = default, .all = all)
}
# }}}
# idf_set {{{
idf_set <- function (self, private, ..., .default = TRUE, .empty = FALSE, .env = parent.frame(2)) {
    set <- set_idf_object(private$idd_env(), private$idf_env(), ..., .default = .default, .empty = .empty, .env = .env)
    merge_idf_data(private$idf_env(), set, by_object = TRUE)

    # log
    log_add_order(private$m_log, set$object$object_id)
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)

    idf_return_modified(self, private, set)
}
# }}}
# idf_set_object {{{
idf_set_object <- function (self, private, object, value, comment, default) {
    .deprecated_fun("$set_object()", "$set()", "Idf", "0.10.0")
    input <- old_input(class, value, comment, "set")
    idf_set(self, private, input, .default = default)
}
# }}}
# idf_del {{{
idf_del <- function (self, private, ..., .ref_by = FALSE, .ref_to = FALSE, .recursive = FALSE, .force = FALSE) {
    del <- del_idf_object(private$idd_env(), private$idf_env(), ...,
        .ref_by = .ref_by, .ref_to = .ref_to, .recursive = .recursive, .force = .force
    )

    private$m_idf_env$object <- del$object
    private$m_idf_env$value <- del$value
    private$m_idf_env$reference <- del$reference

    # log
    log_del_order(private$m_log, del$object$object_id)
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)

    invisible(self)
}
# }}}
# idf_del_object {{{
idf_del_object <- function (self, private, object, referenced = FALSE) {
    .deprecated_fun("$del_object()", "$delete()", "Idf", "0.10.0")
    idf_del(self, private, object, .ref_by = referenced)
}
# }}}
# idf_rename {{{
idf_rename <- function (self, private, ...) {
    ren <- rename_idf_object(private$idd_env(), private$idf_env(), ...)

    merge_idf_data(private$idf_env(), ren)

    # log
    log_add_order(private$m_log, ren$object$object_id)
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)

    res <- apply2(ren$object$object_id, ren$object$class_id, IdfObject$new, list(parent = self))
    setattr(res, "names", ren$object$object_name)
    res
}
# }}}
# idf_insert {{{
idf_insert <- function (self, private, ..., .unique = TRUE, .empty = FALSE) {
    ins <- insert_idf_object(private$idd_env(), private$idf_env(), private$m_version, ..., .unique = .unique, .empty = .empty)

    if (!nrow(ins$object)) {
        verbose_info("After deleting duplications, nothing to add.")
        return(invisible())
    }

    merge_idf_data(private$idf_env(), ins, by_object = TRUE)

    # log
    log_new_order(private$m_log, ins$object$object_id)
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)

    idf_return_modified(self, private, ins)
}
# }}}
# idf_ins_object {{{
idf_ins_object <- function (self, private, object) {
    warning("`$ins_object()` is deprecated. Please use `$ins()` instead.", call. = FALSE)
    idf_insert(self, private, object)
}
# }}}
# idf_search_value {{{
idf_search_value <- function (self, private, pattern, class = NULL, ignore.case = FALSE,
                              perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    val <- search_idf_value(private$idd_env(), private$idf_env(), pattern, class,
        ignore.case, perl, fixed, useBytes
    )

    if (is.null(val)) return(invisible())

    obj <- val[, list(object_name = object_name[[1L]]), by = c("class_id", "object_id")]
    idf_return_modified(self, private, list(object = obj))
}
# }}}
# idf_replace_value {{{
idf_replace_value <- function (self, private, pattern, replacement, class = NULL,
                               ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                               useBytes = FALSE) {
    rep <- replace_idf_value(private$idd_env(), private$idf_env(), pattern, replacement,
        class, ignore.case, perl, fixed, useBytes)

    if (is.null(rep)) return(invisible())

    merge_idf_data(private$idf_env(), rep)

    # log
    log_add_order(private$m_log, rep$object$object_id)
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)

    idf_return_modified(self, private, rep)
}
# }}}
# idf_paste {{{
idf_paste <- function (self, private, in_ip = FALSE, ver = NULL, unique = TRUE, empty = FALSE) {
    pas <- paste_idf_object(private$idd_env(), private$idf_env(),
        version = private$m_version, in_ip = in_ip, unique = unique, empty = empty
    )

    if (!nrow(pas$object)) {
        verbose_info("After deleting duplications, nothing to add.")
        return(invisible())
    }

    merge_idf_data(private$idf_env(), pas, by_object = TRUE)

    # log
    log_new_order(private$m_log, pas$object$object_id)
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)

    idf_return_modified(self, private, pas)
}
# }}}
# idf_load {{{
idf_load <- function (self, private, ..., .unique = TRUE, .default = TRUE, .empty = FALSE) {
    l <- load_idf_object(private$idd_env(), private$idf_env(), private$m_version,
        ..., .unique = .unique, .default = .default, .empty = .empty
    )

    if (!nrow(l$object)) {
        verbose_info("After deleting duplications, nothing to add.")
        return(invisible())
    }

    merge_idf_data(private$idf_env(), l, by_object = TRUE)

    # log
    log_new_order(private$m_log, l$object$object_id)
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)

    idf_return_modified(self, private, l)
}
# }}}
# idf_update {{{
idf_update <- function (self, private, ..., .default = TRUE, .empty = FALSE) {
    l <- update_idf_object(private$idd_env(), private$idf_env(), private$m_version,
        ..., .default = .default, .empty = .empty
    )

    merge_idf_data(private$idf_env(), l, by_object = TRUE)

    # log
    log_new_order(private$m_log, l$object$object_id)
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)

    idf_return_modified(self, private, l)
}
# }}}
# idf_validate {{{
idf_validate <- function (self, private, level = eplusr_option("validate_level")) {
    validate_on_level(private$idd_env(), private$idf_env(), level = level)
}
# }}}
# idf_is_valid {{{
idf_is_valid <- function (self, private, level = eplusr_option("validate_level")) {
    count_check_error(validate_on_level(private$idd_env(), private$idf_env(), level = level)) == 0L
}
# }}}
# idf_to_string {{{
idf_to_string <- function (self, private, which = NULL, class = NULL,
                           comment = TRUE, header = TRUE, format = eplusr_option("save_format"),
                           leading = 4L, sep_at = 29L) {
    if (format == "asis") format <- private$m_log$save_format

    get_idf_string(private$idd_env(), private$idf_env(), private$m_log$order,
        class, which, comment = comment, header = header, format = format,
        leading = leading, sep_at = sep_at
    )
}
# }}}
# idf_string {{{
idf_string <- function (self, private, ...) {
    .deprecated_fun("$string()", "$to_string()", "Idf", "0.10.0")
    idf_to_string(self, private, ...)
}
# }}}
# idf_to_table {{{
idf_to_table <- function (self, private, which = NULL, class = NULL, string_value = TRUE, unit = FALSE, wide = FALSE, align = FALSE, all = FALSE, group_ext = c("none", "group", "index")) {
    get_idf_table(private$idd_env(), private$idf_env(), class, which, string_value, unit, wide, align, all, group_ext)
}
# }}}
# idf_save {{{
idf_save <- function (self, private, path = NULL, format = eplusr_option("save_format"),
                      overwrite = FALSE, copy_external = TRUE) {
    if (is.null(path)) {
        if (is.null(private$m_path)) {
            abort("error_not_local",
                paste0(
                    "The Idf object is not created from local file. ",
                    "Please give the path to save."
                )
            )
        } else {
            path <- private$m_path
        }
    }

    if (format == "asis") format <- private$m_log$save_format

    oldpath <- private$m_path %||% path

    path <- save_idf(private$idd_env(), private$idf_env(), private$m_log$order,
        path = path, in_ip = private$m_log$view_in_ip, format = format,
        overwrite = overwrite, copy_external = copy_external, oldpath = oldpath)

    # if values are updated, assign new uuid
    if (attr(path, "path_updated")) log_new_uuid(private$m_log)
    attr(path, "path_updated") <- NULL

    # log saved
    log_saved(private$m_log)

    # change path
    private$m_path <- normalizePath(path)
    invisible(path)
}
# }}}
# idf_run {{{
idf_run <- function (self, private, epw, dir = NULL, wait = TRUE,
                     force = FALSE, copy_external = FALSE, echo = wait) {
    # check if the model is still running
    old <- private$m_log$job
    if (!inherits(old, "EplusJob")) {
        private$m_log$job <- EplusJob$new(self, epw)
    # recreate job if the model has been changed since last ran
    } else if (
        normalizePath(private$m_path, mustWork = FALSE) !=
        normalizePath(private$m_log$job$path("idf"), mustWork = FALSE)){
        private$m_log$job <- EplusJob$new(self, epw)
    }

    private$m_log$job$run(dir = dir, wait = wait, force = force, echo = echo,
        copy_external = copy_external
    )
}
# }}}
# idf_last_job {{{
idf_last_job <- function (self, private) {
    private$m_log$job
}
# }}}
# idf_print {{{
idf_print <- function (self, private, zoom = c("object", "class", "group", "field"), order = FALSE) {
    zoom <- match.arg(zoom)

    cli::cat_rule("EnergPlus Input Data File", line = 1)

    if (is.null(private$m_path)) path <- crayon::bold$bgRed("NOT LOCAL") else path <- surround(private$m_path)

    cli::cat_line(" * ", c(
        str_trunc(paste0("Path: ", path), width = getOption("width") - 3L),
        paste0("Version: ", surround(private$m_version))
    ))

    cli::cat_line()

    if (zoom == "group") {
        brief <- TRUE
        nest <- TRUE
        component <- c("group", "class")

        dt <- private$idd_env()$class[
            J(private$idf_env()$object$class_id), on = "class_id", mult = "first",
            .SD, .SDcols = c("class_id", "class_name", "group_id")]
        add_joined_cols(private$idd_env()$group, dt, "group_id", "group_name")
    } else if (zoom == "class") {
        brief <- TRUE
        nest <- TRUE
        component <- c("group", "class", "object")

        dt <- private$idf_env()$object[, .SD, .SDcols = c("class_id", "object_id", "object_name")]
        add_joined_cols(private$idd_env()$class, dt, "class_id", c("class_name", "group_id"))
        add_joined_cols(private$idd_env()$group, dt, "group_id", "group_name")
    } else if (zoom == "object") {
        brief <- FALSE
        nest <- if (order) FALSE else TRUE
        component <- c("class", "object")

        dt <- private$idf_env()$object[, .SD, .SDcols = c("class_id", "object_id", "object_name")]
        add_joined_cols(private$idd_env()$class, dt, "class_id", c("class_name"))
    } else {
        brief <- FALSE
        nest <- if (order) FALSE else TRUE
        component <- c("class", "object", "value")

        add_idf_format_cols(private$idd_env(), private$idf_env())
        on.exit(del_idf_format_cols(private$idd_env(), private$idf_env()), add = TRUE)

        add_joined_cols(private$idd_env()$field, private$idf_env()$value, "field_id", "type_enum")
        on.exit(set(private$idf_env()$value, NULL, c("type_enum"), NULL), add = TRUE)

        add_joined_cols(private$idf_env()$object, private$idf_env()$value, "object_id", "object_name")
        on.exit(set(private$idf_env()$value, NULL, c("object_name"), NULL), add = TRUE)

        dt <- private$idf_env()$value
    }

    out <- unlist(format_objects(dt, component, brief = brief, nest = nest, order = nest)$out, use.names = FALSE)

    # remove tailing space
    if (zoom != "group") out <- out[-length(out)]
    cli::cat_line(str_trunc(out))

    invisible(self)
}
# }}}
# idf_deep_clone {{{
idf_deep_clone <- function (self, private, name, value) {
    if (is_idd(value)) {
        value
    } else if (is.environment(value)) {
        l <- as.list.environment(value)
        # copy data.table is necessary here
        l <- lapply(l, function (x) if (inherits(x, "data.table")) copy(x) else x)
        list2env(l)
    } else {
        value
    }
}
# }}}
# idf_add_output_sqlite {{{
idf_add_output_sqlite <- function (idf) {
    if (!is_idf(idf)) idf <- read_idf(idf)
    added <- FALSE
    if (idf$is_valid_class("Output:SQLite")) {
        sql <- idf$objects_in_class("Output:SQLite")[[1L]]
        type <- toupper(sql$value()[[1]])
        if (type != "SIMPLEANDTABULAR") {
            sql$set("SimpleAndTabular")
            verbose_info("Setting `Option Type` in ",
                "`Output:SQLite` to from", surround(type), " to `SimpleAndTabular`.")
            added <- TRUE
        }
    } else {
        invisible(idf$add(Output_SQLite = list("SimpleAndTabular")))
        verbose_info("Adding an object in class `Output:SQLite` and setting its ",
            "`Option Type` to `SimpleAndTabular` in order to create SQLite output file.")
        added <- TRUE
    }
    added
}
# }}}
# idf_add_output_vardict {{{
idf_add_output_vardict <- function (idf) {
    if (!is_idf(idf)) idf <- read_idf(idf)
    added <- FALSE
    if (idf$is_valid_class("Output:VariableDictionary")) {
        dict <- idf$objects_in_class("Output:VariableDictionary")[[1L]]
        key <- toupper(dict$value()[[1]])
        if (!key %chin% c("IDF", "REGULAR")) {
            dict$set("IDF")
            verbose_info("Setting `Key Field` in ",
                "`Output:VariableDictionary` to from", surround(key), " to `IDF`.")
            added <- TRUE
        }
    } else {
        invisible(idf$add(Output_VariableDictionary = list("IDF")))
        verbose_info("Adding an object in class `Output:VariableDictionary` and setting its ",
            "`Key Field` to `IDF` in order to create RDD and MDD output file.")
        added <- TRUE
    }
    added
}
# }}}

#' Read an EnergyPlus Input Data File (IDF)
#'
#' `read_idf` takes an EnergyPlus Input Data File (IDF) as input and returns an
#' `Idf` object. For more details on `Idf` object, please see [Idf] class.
#'
#' @param path Either a path, a connection, or literal data (either a single
#' string or a raw vector) to an EnergyPlus Input Data File (IDF). If a file
#' path, that file usually has a extension `.idf`.
#' @param idd  Any acceptable input of [use_idd()]. If `NULL`, which is the
#' default, the version of IDF will be passed to [use_idd()]. If the input is an
#' `.ddy` file which does not have a version field, the latest version of [Idf]
#' cached will be used.
#'
#' @details
#' Currently, Imf file is not fully supported. All EpMacro lines will be treated
#' as normal comments of the nearest downwards object. If input is an Imf file,
#' a warning will be given during parsing. It is recommended to convert the Imf
#' file to an Idf file and use [ParametricJob] class to conduct
#' parametric analysis.
#'
#' @return An [Idf] object.
#' @examples
#' \dontrun{
#' # example model shipped with eplusr from EnergyPlus v8.8
#' idf_path <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr") # v8.8
#'
#' # if neither EnergyPlus v8.8 nor Idd v8.8 was found, error will occur
#' # if EnergyPlus v8.8 is found but Idd v8.8 was not, `Energy+.idd` in EnergyPlus
#' # installation folder will be used for pasing
#' # if Idd v8.8 is found, it will be used automatically
#' is_avail_eplus(8.8)
#' is_avail_idd(8.8)
#'
#' read_idf(idf_path)
#'
#' # argument `idd` can be specified explicitly using `use_idd()`
#' read_idf(idf_path, idd = use_idd(8.8))
#'
#' # you can set `download` arugment to "auto" in `use_idd()` if you want to
#' # automatically download corresponding IDD file when necessary
#' read_idf(idf_path, use_idd(8.8, download = "auto"))
#'
#' # Besides use a path to an IDF file, you can also provide IDF in literal
#' # string format
#' idf_string <-
#'     "
#'     Version, 8.8;
#'     Building,
#'         Building;                !- Name
#'     "
#'
#' read_idf(idf_string, use_idd(8.8, download = "auto"))
#' }
#' @seealso [Idf] class for modifying EnergyPlus model. [use_idd()] and
#' [download_idd()] for downloading and parsing EnergyPlus IDD file.
#' [use_eplus()] for configuring which version of EnergyPlus to use.
#' @export
#' @author Hongyuan Jia
# read_idf {{{
read_idf <- function (path, idd = NULL) {
    add_idf_class_bindings(Idf$new(path, idd))
}
# }}}

# add_idf_class_bindings {{{
add_idf_class_bindings <- function (idf, class_id = NULL, update = FALSE) {
    if (!.options$autocomplete) return(idf)

    # get all classes in current version IDD
    env <- .subset2(idf, ".__enclos_env__")
    self <- .subset2(env, "self")
    private <- .subset2(env, "private")

    if (is.null(class_id)) {
        ext <- unique(private$idf_env()$object$class_id)
    } else {
        ext <- class_id
    }
    cls <- private$idd_env()$class$class_name[ext]
    flg <- private$idd_env()$class$unique_object[ext]

    # move deleted class bindings
    if (update && length(setdiff(ls(idf, pattern = "^[A-Z]"), cls))) {
        rm(list = setdiff(ls(idf, pattern = "^[A-Z]"), cls), envir = idf)
    }

    # skip if nothing to add
    if (!length(setdiff(cls, ls(idf)))) return(idf)

    # see https://github.com/r-lib/covr/issues/398
    # unique classes
    b <- quote({
        if (missing(value)) {
            if (self$is_valid_class(class)) {
                return(self$object_unique(class))
            } else {
                return(NULL)
            }
        }

        if (is_idfobject(value)) value <- list(value)
        replace_objects_in_class(self, private, class, value, TRUE)
    })
    for (i in setdiff(cls[flg], ls(idf))) {
        b_ <- as.call(c(list(b[[1]], substitute(class <- nm, list(nm = i))), as.list(b[-1])))
        fun <- eval(call("function", as.pairlist(alist(value = )), b_), env)
        makeActiveBinding(i, fun, idf)
    }

    # see https://github.com/r-lib/covr/issues/398
    # other classes
    b <- quote({
        if (missing(value)) {
            if (self$is_valid_class(class)) {
                return(self$objects_in_class(class))
            } else {
                return(NULL)
            }
        }

        if (is_idfobject(value)) value <- list(value)
        replace_objects_in_class(self, private, class, value, TRUE)
    })
    for (i in setdiff(cls[!flg], ls(idf))) {
        b_ <- as.call(c(list(b[[1]], substitute(class <- nm, list(nm = i))), as.list(b[-1])))
        fun <- eval(call("function", as.pairlist(alist(value = )), b_), env)
        makeActiveBinding(i, fun, idf)
    }

    idf
}
# }}}

# replace_objects_in_class {{{
replace_objects_in_class <- function (self, private, class, value, unique_object = FALSE) {
    exist <- self$is_valid_class(class)

    # if NULL, delete all objects in class
    if (is.null(value)) {
        if (exist) self$del(self$object_id(class, simplify = TRUE))

    # if a character vector or a data.frame, use `$load()`
    } else if (is.character(value) || is.data.frame(value)) {
        if (exist) {
            # get current objects
            obj_main <- get_idf_object(private$idd_env(), private$idf_env(), class)

            # temporary rename objects to bypass unique name checking
            obj_dt <- private$idf_env()$object
            obj_dt[J(obj_main$object_id), on = "object_id",
                object_name_lower := paste0(object_name_lower, stri_rand_strings(length(object_name_lower), 15L))
            ]
        }

        # disable unique object checking
        if (exist && unique_object) {
            ori <- eplusr_option("validate_level")
            on.exit(eplusr_option(validate_level = ori), add = TRUE)

            chk <- level_checks(ori)
            chk$unique_object <- FALSE
            eplusr_option(validate_level = chk)
        }

        # get new object data
        l <- load_idf_object(private$idd_env(), private$idf_env(), version = private$m_version,
            value, .unique = FALSE)

        # stop if not from the same class
        cls_in <- private$idd_env()$class$class_name[l$object$class_id]
        if (any(cls_in != class)) {
            if (exist) {
                # get back original object names
                obj_dt[J(obj_main$object_id), on = "object_id",
                    object_name_lower := stri_trans_tolower(object_name)
                ]
            }

            invld_cls <- cls_in[cls_in != class]
            abort("error_invalid_input_object_class",
                paste0(
                    "Input IdfObjects should from class `", class, "`. ",
                    " Invalid input class: ", collapse(invld_cls)
                )
            )
        }

        # if everything looks good, add new objects
        merge_idf_data(private$idf_env(), l, by_object = TRUE)
        log_new_order(private$m_log, l$object$object_id)
        log_unsaved(private$m_log)
        log_new_uuid(private$m_log)

        # delete original objects
        if (exist) {
            invisible(self$del(obj_main$object_id, .force = TRUE))
        }

    # if a list of IdfObjects, use `$insert()`
    } else if (is.list(value) && all(vlapply(value, is_idfobject))) {
        # check if input is from the same model
        # get uuid if idf
        uuid_main <- private$m_log$uuid

        # get uuids of input
        uuid_in <- vcapply(value, function (obj) .subset2(.subset2(._get_private(obj), "log_env")(), "uuid"))
        # get id of input
        obj_id_in <- viapply(value, function (obj) .subset2(._get_private(obj), "m_object_id"))

        # ignore ones that is from the same idf
        if (exist) {
            obj_main <- get_idf_object(private$idd_env(), private$idf_env(), class)
            same_num <- length(value) == nrow(obj_main)
            same_id <- obj_id_in %in% obj_main$object_id
        } else {
            same_num <- FALSE
            same_id <- FALSE
        }

        # direct return the idf
        if (all(uuid_main == uuid_in) && same_num && all(same_id)) return(invisible(self))

        # stop if not from the same class
        cls_id_in <- viapply(value, function (obj) .subset2(._get_private(obj), "m_class_id"))
        cls_in <- private$idd_env()$class$class_name[cls_id_in]
        if (any(cls_in != class)) {
            invld_cls <- vcapply(value[cls_in != class], function (obj) .subset2(obj, "class_name")())
            msg <- paste0(" #", which(cls_in != class), "| <IdfObject> --> Class: ", surround(invld_cls),
                collapse = "\n"
            )
            abort("error_invalid_input_object_class",
                paste0(
                    "Input IdfObjects should from class `", class, "`. ",
                    " Invalid input:\n", msg

                )
            )
        }

        # ignore same objects and insert new ones
        if (any(!same_id)) .subset2(x, "insert")(value[!same_id], .unique = FALSE)

        # delete objects that are not included in input
        if (exist) {
            invisible(self$del(setdiff(obj_main$object_id, obj_id_in), .force = TRUE))
        }

    } else {
        mes <- if (unique_object) "an IdfObject" else "a list of IdfObjects"
        abort("error_invalid_active_binding_value",
            paste0(
                "Value should be ", mes, ", a character vector or a data.frame. ",
                "Input class: ", surround(class(value)[[1]]), "."
            )
        )
    }

    invisible(self)
}
# }}}

#' @export
# $.Idf {{{
`$.Idf` <- function (x, i) {
    if (i %chin% ls(x)) return(NextMethod())

    private <- ._get_private(x)

    cls_id <- chmatch(i, private$idd_env()$class$class_name_us)
    if (is.na(cls_id)) cls_id <- chmatch(i, private$idd_env()$class$class_name)

    # skip if not a valid IDD class name
    if (is.na(cls_id)) return(NextMethod())

    # skip if not an existing IDF class name
    if (!cls_id %in% private$idf_env()$object$class_id) return(NextMethod())

    cls_nm <- private$idd_env()$class$class_name[cls_id]
    if (private$idd_env()$class$unique_object[cls_id]) {
        .subset2(x, "object_unique")(cls_nm)
    } else {
        .subset2(x, "objects_in_class")(cls_nm)
    }
}
# }}}

#' @export
# [[.Idf {{{
`[[.Idf` <- function (x, i) {
    if (i %chin% ls(x)) return(NextMethod())

    private <- ._get_private(x)

    cls_id <- chmatch(i, private$idd_env()$class$class_name)

    # skip if not a valid IDD class name
    if (is.na(cls_id)) return(NextMethod())

    # skip if not an existing IDF class name
    if (!cls_id %in% private$idf_env()$object$class_id) return(NextMethod())

    cls_nm <- private$idd_env()$class$class_name[cls_id]
    if (private$idd_env()$class$unique_object[cls_id]) {
        .subset2(x, "object_unique")(cls_nm)
    } else {
        .subset2(x, "objects_in_class")(cls_nm)
    }
}
# }}}

#' @export
# $<-.Idf {{{
`$<-.Idf` <- function (x, name, value) {
    if (name %chin% ls(x)) return(NextMethod())

    self <- ._get_self(x)
    private <- ._get_private(x)

    # match both normal and underscore class names
    cls_id <- chmatch(name, private$idd_env()$class$class_name)
    if (is.na(cls_id)) cls_id <- chmatch(name, private$idd_env()$class$class_name_us)

    # skip if not a valid IDD class name
    # imitate error message of a locked environment
    if (is.na(cls_id)) stop("cannot add bindings to a locked environment")

    cls_nm <- private$idd_env()$class$class_name[cls_id]
    uni <- private$idd_env()$class$unique_object[cls_id]

    if (uni && is_idfobject(value)) value <- list(value)
    replace_objects_in_class(self, private, cls_nm, value, uni)

    # if not an existing IDF class name, add active bindings
    if (!cls_id %in% ls(x)) {
        add_idf_class_bindings(x, cls_id)
    }

    invisible(x)
}
# }}}

#' @export
# [[<-.Idf {{{
`[[<-.Idf` <- function (x, name, value) {
    if (length(name) != 1L) return(NextMethod())

    if (name %chin% ls(x)) {
        NextMethod()
    } else {
        self <- ._get_self(x)
        private <- ._get_private(x)

        # match only normal class names
        cls_id <- chmatch(name, private$idd_env()$class$class_name)

        # skip if not a valid IDD class name
        # imitate error message of a locked environment
        if (is.na(cls_id)) stop("cannot add bindings to a locked environment")

        cls_nm <- private$idd_env()$class$class_name[cls_id]
        uni <- private$idd_env()$class$unique_object[cls_id]

        replace_objects_in_class(self, private, cls_nm, value, uni)
        # if not an existing IDF class name, add active bindings
        if (!cls_id %in% private$idf_env()$object$class_id) {
            add_idf_class_bindings(x, cls_id)
        }
    }

    invisible(x)
}
# }}}

#' @export
# print.Idf {{{
print.Idf <- function (x, zoom = "class", order = TRUE, ...) {
    add_idf_class_bindings(x, update = TRUE)
    x$print(zoom = zoom, order = order)
    invisible(x)
}
# }}}

#' @export
# str.idf {{{
str.Idf <- function (object, zoom = "class", ...) {
    object$print(zoom)
}
# }}}

#' Format an Idf Object
#'
#' Format an [Idf] object.
#'
#' @param x An [Idf] object.
#' @param comment If `FALSE`, all comments will not be included. Default: `TRUE`.
#' @param header If `FALSE`, the header will not be included. Default: `TRUE`.
#' @param format Specific format used when formatting. For details, please see
#' `$save()`. Default: `eplusr_option("save_format")`
#' @param leading Leading spaces added to each field. Default: `4L`.
#' @param sep_at The character width to separate value string and field string.
#' Default: `29L` which is the same as IDF Editor.
#' @param ... Further arguments passed to or from other methods.
#' @return A single length string.
#' @examples
#' \dontrun{
#' idf_path <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
#' cat(format(read_idf(idf_path, use_idd(8.8, "auto")), leading = 0))
#' }
#' @export
#' @author Hongyuan Jia
# format.idf {{{
format.Idf <- function (x, comment = TRUE, header = TRUE,
                        format = eplusr_option("save_format"),
                        leading = 4L, sep_at = 29L, ...) {
    paste0(
        x$to_string(comment = comment, header = header, format = format,
            leading = leading, sep_at = sep_at, ...
        ),
        collapse = "\n"
    )
}
# }}}

#' Coerce an Idf object into a Character Vector
#'
#' Coerce an [Idf] object into a character vector.
#'
#' @inheritParams format.Idf
#' @return A character vector.
#' @examples
#' \dontrun{
#' idf_path <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
#' as.character(read_idf(idf_path, use_idd(8.8, "auto")), leading = 0)
#' }
#' @export
#' @author Hongyuan Jia
# as.character.Idf {{{
as.character.Idf <- function (x, comment = TRUE, header = TRUE,
                        format = eplusr_option("save_format"),
                        leading = 4L, sep_at = 29L, ...) {
    x$to_string(comment = comment, header = header, format = format,
        leading = leading, sep_at = sep_at, ...
    )
}
# }}}

#' Create an Empty Idf
#'
#' `empty_idf()` takes a valid IDD version and creates an empty [Idf] object
#' that only contains a Version object.
#'
#' @param ver Any acceptable input of [use_idd()]. If `latest`, which is the
#' default, the latest IDD released version is used.
#' @return An [Idf] object
#' @export
#' @examples
#' \dontrun{
#' if (is_avail_idd(8.8)) empty_idf(8.8)
#' }
#'
# empty_idf {{{
empty_idf <- function (ver = "latest") {
    ver <- standardize_ver(ver)
    text <- paste0("Version,", ver[, 1L:2L], ";\n")
    read_idf(text, ver)
}
# }}}
