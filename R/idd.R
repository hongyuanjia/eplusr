#' @importFrom R6 R6Class
#' @importFrom checkmate assert_vector assert_string assert_scalar
#' @include impl-idd.R
NULL

#' Parse, Query and Modify EnergyPlus Input Data Dictionary (IDD)
#'
#' eplusr provides parsing of and programmatic access to EnergyPlus
#' Input Data Dictionary (IDD) files, and objects. It contains all data needed
#' to parse EnergyPlus models. `Idd` class provides parsing and printing while
#' [IddObject] provides detailed information of curtain class.
#'
#' @section Overview:
#'
#' EnergyPlus operates off of text input files written in its own Input
#' Data File (IDF) format. IDF files are similar to XML files in that they are
#' intended to conform to a data schema written using similar syntax. For XML,
#' the schema format is XSD; for IDF, the schema format is IDD. For each release
#' of EnergyPlus, valid IDF files are defined by the "Energy+.idd" file shipped
#' with the release.
#'
#' eplusr tries to detect all installed EnergyPlus in default installation
#' locations when loading, i.e. `C:\\EnergyPlusVX-X-0` on Windows,
#' `/usr/local/EnergyPlus-X-Y-0` on Linux, and
#' `/Applications/EnergyPlus-X-Y-0` on macOS and stores all found locations
#' internally. This data is used to locate the distributed "Energy+.idd" file of
#' each EnergyPlus version. And also, every time an IDD file is parsed, an `Idd`
#' object is created and cached in an environment.
#'
#' Parsing an IDD file starts from [use_idd()]. When using [use_idd()], eplusr
#' will first try to find the cached `Idd` object of that version, if possible.
#' If failed, and EnergyPlus of that version is available (see [avail_eplus()]),
#' the `"Energy+.idd"` distributed with EnergyPlus will be parsed and cached. So
#' each IDD file only needs to be parsed once and can be used when parsing every
#' IDF file of that version.
#'
#' Internally, the powerful [data.table](https://cran.r-project.org/package=data.table)
#' package is used to speed up the whole IDD parsing process and store the
#' results. However, it will still take about 2-3 sec per IDD. Under the hook,
#' eplusr uses a SQL-like structure to store both IDF and IDD data in
#' [data.table::data.table] format. Every IDD will be parsed and stored in
#' four tables:
#'
#' * `group`: contains group index and group names.
#' * `class`: contains class names and properties.
#' * `field`: contains field names and field properties.
#' * `reference`: contains cross-reference data of fields.
#'
#' @docType class
#' @name Idd
#' @seealso [IddObject] class which provides detailed information of
#' curtain class
#' @author Hongyuan Jia
#' @references
#' [IDFEditor](https://github.com/NREL/EnergyPlus/tree/develop/src/IDF_Editor),
#' [OpenStudio utilities library](https://openstudio-sdk-documentation.s3.amazonaws.com/cpp/OpenStudio-2.5.1-doc/utilities/html/idf_page.html)
NULL

#' @export
# Idd {{{
Idd <- R6::R6Class(classname = "Idd", cloneable = FALSE,

    public = list(
        # INITIALIZE {{{
        #' @description
        #' Create an `Idd` object
        #'
        #' @details
        #' It takes an EnergyPlus Input Data Dictionary (IDD) as input and
        #' returns an `Idd` object.
        #'
        #' It is suggested to use helper [use_idd()] which supports to directly
        #' take a valid IDD version as input and search automatically the
        #' corresponding file path.
        #'
        #' @param path Either a path, a connection, or literal data (either a single
        #'        string or a raw vector) to an EnergyPlus Input Data Dictionary
        #'        (IDD). If a file path, that file usually has a extension
        #'        `.idd`.
        #' @param encoding The file encoding of input IDD. Should be one of
        #'        `"unknown"`, `"Latin-1" and `"UTF-8"`. The default is
        #'        `"unknown"` which means that the file is encoded in the native
        #'        encoding.
        #'
        #' @return An `Idd` object.
        #'
        #' @examples
        #' \dontrun{Idd$new(file.path(eplus_config(8.8)$dir, "Energy+.idd"))
        #'
        #' # Preferable way
        #' idd <- use_idd(8.8, download = "auto")
        #' }
        #'
        initialize = function(path, encoding = "unknown") {
            # only store if input is a path
            if (is.character(path) && length(path) == 1L) {
                if (file.exists(path)) private$m_path <- normalizePath(path)
            }

            # add a uuid
            private$m_log <- new.env(hash = FALSE, parent = emptyenv())
            private$m_log$uuid <- unique_id()

            idd_file <- parse_idd_file(path, encoding = encoding)
            private$m_version <- idd_file$version
            private$m_build <- idd_file$build

            private$m_idd_env <- list2env(
                idd_file[!names(idd_file) %in% c("version", "build")], parent = emptyenv()
            )

            # add current idd to .globals
            .globals$idd[[as.character(private$m_version)]] <- self
        },
        # }}}

        # PROPERTY GETTERS {{{
        # version {{{
        #' @description
        #' Get the version of current `Idd`
        #'
        #' @details
        #' `$version()` returns the version of current `Idd` in a
        #' [base::numeric_version()] format. This makes it easy to direction
        #' compare versions of different `Idd`s, e.g. `idd$version() > 8.6` or
        #' `idd1$version() > idd2$version()`.
        #'
        #' @return A [base::numeric_version()] object.
        #' @examples
        #' \dontrun{
        #' # get version
        #' idd$version()
        #' }
        #'
        version = function()
            idd_version(self, private),
        # }}}

        # build {{{
        #' @description
        #' Get the build tag of current `Idd`
        #'
        #' @details
        #' `$build()` returns the build tag of current `Idd`. If no build tag is
        #' found, `NA` is returned.
        #'
        #' @return A [base::numeric_version()] object.
        #' @examples
        #' \dontrun{
        #' # get build tag
        #' idd$build()
        #' }
        #'
        build = function()
            idd_build(self, private),
        # }}}

        # path {{{
        #' @description
        #' Get the file path of current `Idd`
        #'
        #' @details
        #' `$path()` returns the full path of current `Idd` or `NULL` if the
        #' `Idd` object is created using a character vector and not saved
        #' locally.
        #'
        #' @return `NULL` or a single string.
        #'
        #' @examples
        #' \dontrun{
        #' # get path
        #' idd$path()
        #' }
        #'
        path = function()
            idd_path(self, private),

        # group_name {{{
        #' @description
        #' Get names of groups
        #'
        #' @details
        #' `$group_name()` returns names of groups current `Idd` contains.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get names of all groups Idf contains
        #' idd$group_name()
        #' }
        #'
        group_name = function()
            idd_group_name(self, private),
        # }}}

        # from_group {{{
        #' @description
        #' Get the name of group that specified class belongs to
        #'
        #' @details
        #' `$from_group()` returns the name of group that specified class
        #' belongs to.
        #'
        #' @param class A character vector of valid class names in current
        #'        `Idd`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' idd$from_group(c("Version", "Schedule:Compact"))
        #' }
        #'
        from_group = function(class)
            idd_from_group(self, private, class),
        # }}}

        # class_name {{{
        #' @description
        #' Get names of classes
        #'
        #' @details
        #' `$class_name()` returns names of classes current `Idd` contains
        #'
        #' @param index An integer vector of class indices.
        #' @param by_group If `TRUE`, a list is returned which separates class
        #'        names by the group they belong to. Default: `FALSE`.
        #'
        #' @return A character vector if `by_group` is `FALSE` and a list of
        #' character vectors when `by_group` is `TRUE`.
        #'
        #' @examples
        #' \dontrun{
        #' # get names of the 10th to 20th class
        #' idd$class_name(10:20)
        #'
        #' # get names of all classes in Idf
        #' idd$class_name()
        #'
        #' # get names of all classes grouped by group names in Idf
        #' idd$class_name(by_group = TRUE)
        #' }
        #'
        class_name = function(index = NULL, by_group = FALSE)
            idd_class_name(self, private, index = index, by_group = by_group),
        # }}}

        # required_class_name {{{
        #' @description
        #' Get the names of required classes
        #'
        #' @details
        #' `$required_class_name()` returns the names of required classes in
        #' current `Idd`. "Require" means that for any [Idf] there should be at
        #' least one object.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' idd$required_class_name()
        #' }
        #'
        required_class_name = function()
            idd_required_class_name(self, private),
        # }}}

        # unique_class_name {{{
        #' @description
        #' Get the names of unique-object classes
        #'
        #' @details
        #' `$unique_class_name()` returns the names of unique-object classes in
        #' current `Idd`. "Unique-object" means that for any [Idf] there should
        #' be at most one object in those classes.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' idd$unique_class_name()
        #' }
        #'
        unique_class_name = function()
            idd_unique_class_name(self, private),
        # }}}

        # extensible_class_name {{{
        #' @description
        #' Get the names of classes with extensible fields
        #'
        #' @details
        #' `$extensible_class_name()` returns the names of classes with
        #' extensible fields in current `Idd`. "Extensible fields" indicate
        #' fields that can be added dynamically, such like the X, Y and Z
        #' vertices of a building surface.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' idd$extensible_class_name()
        #' }
        #'
        extensible_class_name = function()
            idd_extensible_class_name(self, private),
        # }}}

        # group_index {{{
        #' @description
        #' Get the indices of specified groups
        #'
        #' @details
        #' `$group_index()` returns the indices of specified groups in
        #' current `Idd`. A group index is just an integer indicating its
        #' appearance order in the `Idd`.
        #'
        #' @param group A character vector of valid group names.
        #'
        #' @return An integer vector.
        #'
        #' @examples
        #' \dontrun{
        #' idd$group_index()
        #' }
        #'
        group_index = function(group = NULL)
            idd_group_index(self, private, group),
        # }}}

        # class_index {{{
        #' @description
        #' Get the indices of specified classes
        #'
        #' @details
        #' `$class_index()` returns the indices of specified classes in
        #' current `Idd`. A class index is just an integer indicating its
        #' appearance order in the `Idd`.
        #'
        #' @param class A character vector of valid class names.
        #' @param by_group If `TRUE`, a list is returned which separates class
        #'        names by the group they belong to. Default: `FALSE`.
        #'
        #' @return An integer vector.
        #'
        #' @examples
        #' \dontrun{
        #' idd$class_index()
        #' }
        #'
        class_index = function(class = NULL, by_group = FALSE)
            idd_class_index(self, private, class, by_group = by_group),
        # }}}
        # }}}

        # ASSERTIONS {{{
        # is_valid_group {{{
        #' @description
        #' Check if elements in input character vector are valid group names.
        #'
        #' @details
        #' `$is_valid_group()` returns `TRUE`s if given character vector
        #' contains valid group names in the context of current `Idd`, and
        #' `FALSE`s otherwise.
        #'
        #' Note that case-sensitive matching is performed, which means that
        #' `"Location and Climate"` is a valid group name but `"location and
        #' climate"` is not.
        #'
        #' @param group A character vector to check.
        #'
        #' @return A logical vector with the same length as input character
        #' vector.
        #'
        #' @examples
        #' \dontrun{
        #' idd$is_valid_group(c("Schedules", "Compliance Objects"))
        #' }
        #'
        is_valid_group = function(group)
            idd_is_valid_group_name(self, private, group),
        # }}}

        # is_valid_class {{{
        #' @description
        #' Check if elements in input character vector are valid class names.
        #'
        #' @details
        #' `$is_valid_class()` returns `TRUE`s if given character vector
        #' contains valid class names in the context of current `Idd`, and
        #' `FALSE`s otherwise.
        #'
        #' Note that case-sensitive matching is performed, which means that
        #' `"Version"` is a valid class name but `"version"` is not.
        #'
        #' @param class A character vector to check.
        #'
        #' @return A logical vector with the same length as input character
        #' vector.
        #'
        #' @examples
        #' \dontrun{
        #' idd$is_valid_class(c("Building", "ShadowCalculation"))
        #' }
        #'
        is_valid_class = function(class)
            idd_is_valid_class_name(self, private, class),
        # }}}
        # }}}

        # OBJECT GETTERS {{{
        # object {{{
        #' @description
        #' Extract an [IddObject] object using class index or name.
        #'
        #' @details
        #' `$object()` returns an [IddObject] object specified by a class ID
        #' or name.
        #'
        #' Note that case-sensitive matching is performed, which means that
        #' `"Version"` is a valid class name but `"version"` is not.
        #'
        #' For convenience, underscore-style names are allowed, e.g.
        #' `Site_Location` is equivalent to `Site:Location`.
        #'
        #' @param class A single integer specifying the class index or a single
        #'        string specifying the class name.
        #'
        #' @return An [IddObject] object.
        #'
        #' @examples
        #' \dontrun{
        #' idd$object(3)
        #'
        #' idd$object("Building")
        #' }
        #'
        object = function(class)
            idd_obj(self, private, class),
        # }}}

        # objects {{{
        #' @description
        #' Extract multiple [IddObject] objects using class indices or names.
        #'
        #' @details
        #' `$objects()` returns a named list of [IddObject] objects using class
        #' indices or names. The returned list is named using class names.
        #'
        #' Note that case-sensitive matching is performed, which means that
        #' `"Version"` is a valid class name but `"version"` is not.
        #'
        #' For convenience, underscore-style names are allowed, e.g.
        #' `Site_Location` is equivalent to `Site:Location`.
        #'
        #' @param class An integer vector specifying class indices or a character
        #'        vector specifying class names.
        #'
        #' @return A named list of [IddObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' idd$objects(c(3,10))
        #'
        #' idd$objects(c("Version", "Material"))
        #' }
        #'
        objects = function(class)
            idd_objects(self, private, class),
        # }}}

        # object_relation {{{
        #' @description
        #' Extract the relationship between class fields.
        #'
        #' @details
        #' Many fields in [Idd] can be referred by others. For example, the
        #' `Outside Layer` and other fields in `Construction` class refer to the
        #' `Name` field in `Material` class and other material related classes.
        #' Here it means that the `Outside Layer` field **refers to** the `Name`
        #' field and the `Name` field is **referred by** the `Outside Layer`.
        #'
        #' `$object_relation()` provides a simple interface to get this kind of
        #' relation. It takes a single class index or name and also a relation
        #' direction, and returns an `IddRelation` object which contains data
        #' presenting such relation above. For instance, if
        #' `idd$object_relation("Construction", "ref_to")` gives results below:
        #'
        #' ```
        #' -- Refer to Others ---------------------------
        #'   Class: <Construction>
        #'   |- Field: <02: Outside Layer>
        #'   |  v~~~~~~~~~~~~~~~~~~~~~~~~~
        #'   |  |- Class: <Material>
        #'   |  |  \- Field: <1: Name>
        #'   |  |
        #'   |  |- Class: <Material:NoMass>
        #'   |  |  \- Field: <1: Name>
        #'   |  |
        #'   |  |- Class: <Material:InfraredTransparent>
        #'   |  |  \- Field: <1: Name>
        #'   |  |
        #'   ......
        #' ```
        #'
        #' This means that the value of field `Outside Layer` in class
        #' `Construction` can be one of values from field `Name` in class
        #' `Material`, field `Name` in class `Material:NoMass`, field `Name` in
        #' class `Material:InfraredTransparent` and etc. All those classes can
        #' be further easily extracted using `$objects_in_relation()` method
        #' described below.
        #'
        #' @param which A single integer specifying the class index or a single
        #'        string specifying the class name.
        #' @param direction The relation direction to extract. Should be one of
        #'        `"all"`, `"ref_to"` or `"ref_by"`.
        #' @param class A character vector of class names used for searching
        #'        relations. Default: `NULL`.
        #' @param group A character vector of group names used for searching
        #'        relations. Default: `NULL`.
        #' @param depth If > 0, the relation is searched recursively. A
        #'        simple example of recursive reference: one material named
        #'        `mat` is referred by a construction named `const`, and `const`
        #'        is also referred by a surface named `surf`. If `NULL`,
        #'        all possible recursive relations are returned. Default: `0`.
        #'
        #' @return An `IddRelation` object, which is a list of 3
        #' [data.table::data.table()]s named `ref_to` and `ref_by`.
        #' Each [data.table::data.table()] contains 12 columns.
        #'
        #' @examples
        #' \dontrun{
        #' # check each construction layer's possible references
        #' idd$object_relation("Construction", "ref_to")
        #'
        #' # check where construction being used
        #' idd$object_relation("Construction", "ref_by")
        #' }
        #'
        object_relation = function(which, direction = c("all", "ref_to", "ref_by"), class = NULL, group = NULL, depth = 0L)
            idd_object_relation(self, private, which, match.arg(direction), class = class, group = group, depth = depth),
        # }}}

        # objects_in_relation {{{
        #' @description
        #' Extract multiple [IddObject] objects referencing each others.
        #'
        #' @details
        #' `$objects_in_relation()` returns a named list of [IddObject] objects
        #' that have specified relationship with given class. The first element of
        #' returned list is always the specified class itself. If that
        #' class does not have specified relationship with other classes, a list
        #' that only contains specified class itself is returned.
        #'
        #' For instance, `idd$objects_in_relation("Construction", "ref_by")`
        #' will return a named list of an [IddObject] object named
        #' `Construction` and also all other [IddObject] objects that can refer
        #' to field values in class `Construction`. Similarly,
        #' `idd$objects_in_relation("Construction", "ref_to")` will return a
        #' named list of an [IddObject] object named `Construction` and also all
        #' other [IddObject] objects that `Construction` can refer to.
        #'
        #' @param which A single integer specifying the class index or a single
        #'        string specifying the class name.
        #' @param direction The relation direction to extract. Should be either
        #'        `"ref_to"` or `"ref_by"`.
        #' @param class A character vector of valid class names in the
        #'        current Idd. It is used to restrict the classes to be
        #'        returned. If `NULL`, all possible classes are considered and
        #'        corresponding [IddObject] objects are returned if
        #'        relationships are found. Default: `NULL`.
        #' @param group A character vector of valid group names in the
        #'        current Idd. It is used to restrict the groups to be
        #'        returned. If `NULL`, all possible groups are considered and
        #'        corresponding [IddObject] objects are returned if
        #'        relationships are found. Default: `NULL`.
        #' @param depth If > 0, the relation is searched recursively. A
        #'        simple example of recursive reference: one material named
        #'        `mat` is referred by a construction named `const`, and `const`
        #'        is also referred by a surface named `surf`. If `NULL`,
        #'        all possible recursive relations are returned. Default: `0`.
        #'
        #' @return An named list of [IddObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # get class Construction and all classes that it can refer to
        #' idd$objects_in_relation("Construction", "ref_to")
        #'
        #' # get class Construction and all classes that refer to it
        #' idd$objects_in_relation("Construction", "ref_by")
        #' }
        #'
        objects_in_relation = function(which, direction = c("ref_to", "ref_by"), class = NULL, group = NULL, depth = 0L)
            idd_objects_in_relation(self, private, which, match.arg(direction), class = class, group = group, depth = depth),
        # }}}

        # objects_in_group {{{
        #' @description
        #' Extract all [IddObject] objects in one group.
        #'
        #' @details
        #' `$objects_in_group()` returns a named list of all [IddObject] objects
        #' in specified group. The returned list is named using class names.
        #'
        #' @param group A single string of valid group name for current `Idd`
        #'        object.
        #'
        #' @return A named list of [IddObject] objects.
        #'
        #' @examples
        #' \dontrun{
        #' # get all classes in Schedules group
        #' idd$objects_in_group("Schedules")
        #' }
        #'
        objects_in_group = function(group)
            idd_objects_in_group(self, private, group = group),
        # }}}
        # }}}

        # DATA EXTRACTION {{{
        # to_table {{{
        #' @description
        #' Format `Idd` classes as a data.frame
        #'
        #' @details
        #' `$to_table()` returns a [data.table][data.table::data.table()] that
        #' contains basic data of specified classes.
        #' The returned [data.table][data.table::data.table()] has 3 columns:
        #'
        #' * `class`: Character type. Current class name.
        #' * `index`: Integer type. Field indexes.
        #' * `field`: Character type. Field names.
        #'
        #' @param class A character vector of class names.
        #' @param all If `TRUE`, all available fields defined in IDD for
        #'        specified class will be returned. Default: `FALSE`.
        #'
        #' @return A [data.table][data.table::data.table()] with 3 columns.
        #'
        #' @examples
        #' \dontrun{
        #' # extract data of class Material
        #' idd$to_table(class = "Material")
        #'
        #' # extract multiple class data
        #' idd$to_table(c("Construction", "Material"))
        #' }
        #'
        to_table = function(class, all = FALSE)
            idd_to_table(self, private, class, all),
        # }}}

        # to_string {{{
        #' @description
        #' Format `Idf` classes as a character vector
        #'
        #' @details
        #' `$to_string()` returns the text format of specified classes. The
        #' returned character vector can be pasted into an IDF file as empty
        #' objects of specified classes.
        #'
        #' @param class A character vector of class names.
        #' @param leading Leading spaces added to each field. Default: `4L`.
        #' @param sep_at The character width to separate value string and field
        #'        string. Default: `29L` which is the same as IDF Editor.
        #' @param sep_each A single integer of how many empty strings to insert
        #'        between different classes. Default: `0`.
        #' @param all If `TRUE`, all available fields defined in IDD for
        #'        specified class will be returned. Default: `FALSE`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get text format of class Material
        #' head(idd$to_string(class = "Material"))
        #'
        #' # get text format of multiple class
        #' idd$to_string(c("Material", "Construction"))
        #'
        #' # tweak output formatting
        #' idd$to_string(c("Material", "Construction"), leading = 0, sep_at = 0, sep_each = 5)
        #' }
        #'
        to_string = function(class, leading = 4L, sep_at = 29L, sep_each = 0L, all = FALSE)
            idd_to_string(self, private, class, leading, sep_at, sep_each, all),
        # }}}
        # }}}

        # print {{{
        #' @description
        #' Print `Idd` object
        #'
        #' @details
        #' `$print()` prints the `Idd` object giving the information of version,
        #' build tag and total class numbers.
        #'
        #' @return The `Idd` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' idd$print()
        #' }
        #'
        print = function()
            idd_print(self, private)
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_version = NULL,
        m_build = NULL,
        m_path = NULL,
        m_idd_env = NULL,
        m_log = NULL,
        # }}}

        # PRIVATE FUNCTIONS {{{
        uuid = function() private$m_log$uuid,
        log_new_uuid = function() log_new_uuid(private$m_log),

        idd_env = function() private$m_idd_env,
        log_env = function() private$m_log
        # }}}
    )
)
# }}}

# idd_path {{{
idd_path <- function(self, private) {
    private$m_path
}
# }}}
# idd_version {{{
idd_version <- function(self, private) {
    private$m_version
}
# }}}
# idd_build {{{
idd_build <- function(self, private) {
    private$m_build
}
# }}}
# idd_group_name {{{
idd_group_name <- function(self, private) {
    get_idd_group_name(private$m_idd_env)
}
# }}}
# idd_from_group {{{
idd_from_group <- function(self, private, class) {
    get_idd_class(private$m_idd_env, class, "group_name")$group_name
}
# }}}
# idd_group_index {{{
idd_group_index <- function(self, private, group = NULL) {
    get_idd_group_index(private$m_idd_env, group)
}
# }}}
# idd_class_name {{{
idd_class_name <- function(self, private, index = NULL, by_group = FALSE) {
    if (!by_group) return(get_idd_class(private$m_idd_env, index)$class_name)
    cls <- get_idd_class(private$m_idd_env, index, property = "group_name")
    res <- cls[, list(class_name = list(class_name)), by = "group_name"]
    setattr(res$class_name, "names", res$group_name)[]
}
# }}}
# idd_class_index {{{
idd_class_index <- function(self, private, class = NULL, by_group = FALSE) {
    if (!by_group) return(get_idd_class(private$m_idd_env, class)$class_id)
    cls <- get_idd_class(private$m_idd_env, class, property = "group_name")
    res <- cls[, list(class_id = list(class_id)), by = "group_name"]
    setattr(res$class_id, "names", res$group_name)[]
}
# }}}
# idd_required_class_name {{{
idd_required_class_name <- function(self, private) {
    private$m_idd_env$class[required_object == TRUE, class_name]
}
# }}}
# idd_unique_class_name {{{
idd_unique_class_name <- function(self, private) {
    private$m_idd_env$class[unique_object == TRUE, class_name]
}
# }}}
# idd_extensible_class_name {{{
idd_extensible_class_name <- function(self, private) {
    private$m_idd_env$class[num_extensible > 0L, class_name]
}
# }}}
# idd_is_valid_group_name {{{
idd_is_valid_group_name <- function(self, private, group) {
    group %chin% private$m_idd_env$group$group_name
}
# }}}
# idd_is_valid_class_name {{{
idd_is_valid_class_name <- function(self, private, class) {
    class %chin% private$m_idd_env$class$class_name
}
# }}}
# idd_obj {{{
idd_obj <- function(self, private, class) {
    IddObject$new(class, self)
}
# }}}
# idd_objects {{{
idd_objects <- function(self, private, class) {
    res <- lapply(class, IddObject$new, self)
    setattr(res, "names", class)
    res
}
# }}}
# idd_object_relation {{{
idd_object_relation <- function(self, private, which, direction = c("all", "ref_to", "ref_by"),
                                 class = NULL, group = NULL, depth = 0L) {
    assert_scalar(which)
    direction <- match.arg(direction)

    cls <- get_idd_class(private$m_idd_env, which)

    get_iddobj_relation(private$m_idd_env, cls$class_id, NULL, name = TRUE,
        direction = direction, depth = depth, keep_all = FALSE,
        class = class, group = group
    )
}
# }}}
# idd_objects_in_relation {{{
idd_objects_in_relation <- function(self, private, which, direction = c("ref_to", "ref_by"),
                                     class = NULL, group = NULL, depth = 0L) {
    assert_scalar(which)
    direction <- match.arg(direction)
    rel <- get_idd_relation(private$m_idd_env, which, depth = depth, direction = direction,
        class = class, group = group, keep_all = TRUE)

    if (direction == "ref_to") {
        id_self <- unique(rel$class_id)
        id_ref <- rel$src_class_id[!is.na(rel$src_class_id)]
    } else {
        id_self <- unique(rel$src_class_id)
        id_ref <- rel$class_id[!is.na(rel$class_id)]
    }

    cls_nm <- private$m_idd_env$class[J(id_self), on = "class_id", class_name]

    obj_self <- list(IddObject$new(id_self, self))
    setattr(obj_self, "names", cls_nm)

    if (!length(id_ref)) {
        dir <- switch(direction, ref_to = "does not refer to", ref_by = "is not referred by")
        verbose_info("Class ", surround(cls_nm), " ", dir, " any other class or group.")
        return(obj_self)
    }

    res <- c(obj_self, lapply(id_ref, IddObject$new, parent = self))

    ref_nm <- private$m_idd_env$class[J(id_ref), on = "class_id", class_name]
    setattr(res, "names", c(cls_nm, ref_nm))

    res
}
# }}}
# idd_objects_in_group {{{
idd_objects_in_group <- function(self, private, group) {
    assert_string(group)

    grp_id <- idd_group_index(self, private, group)

    cls <- private$m_idd_env$class[J(grp_id), on = "group_id", class_name]

    res <- lapply(cls, IddObject$new, self)
    setattr(res, "names", cls)
    res
}
# }}}
# idd_to_table {{{
idd_to_table <- function(self, private, class, all) {
    get_idd_table(private$m_idd_env, class, all)
}
# }}}
# idd_to_string {{{
idd_to_string <- function(self, private, class, leading = 4L, sep_at = 29L, sep_each = 0L, all = FALSE) {
    get_idd_string(private$m_idd_env, class, leading, sep_at, sep_each, all)
}
# }}}
# idd_print {{{
idd_print <- function(self, private) {
    cli::cat_rule("EnergyPlus Input Data Dictionary")
    cli::cat_line("* ", c(
        paste0("Version", ": ", private$m_version),
        paste0("Build", ": ", private$m_build),
        paste0("Total Class", ": ", nrow(private$m_idd_env$class))
    ))
}
# }}}

#' @export
# [[.Idd {{{
`[[.Idd` <- function(x, i) {
    if (i %chin% ls(x)) return(NextMethod())

    private <- get_priv_env(x)

    cls_id <- chmatch(i, private$m_idd_env$class$class_name)

    # skip if not a valid IDD class name
    if (is.na(cls_id)) return(NextMethod())

    cls_nm <- private$m_idd_env$class$class_name[cls_id]
    .subset2(x, "object")(cls_nm)
}
# }}}

#' @export
# $.Idd {{{
`$.Idd` <- function(x, i) {
    if (i %chin% ls(x)) return(NextMethod())

    private <- get_priv_env(x)

    cls_id <- chmatch(underscore_name(i), private$m_idd_env$class$class_name_us)

    # skip if not a valid IDD class name
    if (is.na(cls_id)) return(NextMethod())

    cls_nm <- private$m_idd_env$class$class_name[cls_id]
    .subset2(x, "object")(cls_nm)
}
# }}}

#' @export
# str.Idd {{{
str.Idd <- function(object, ...) {
    object$print()
}
# }}}

#' Format an Idd
#'
#' Format an [Idd] into a string.
#'
#' @param x An [Idd] object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A single length character vector.
#' @examples
#' \dontrun{
#' format(use_idd(8.8, download = "auto"))
#' }
#'
#' @export
# format.Idd {{{
format.Idd <- function(x, ...) {
    n <- length(x$class_index())

    if (is.na(x$build())) {
        sprintf("<EnergyPlus IDD v%s with %i %s>", x$version(), n,
           if (n <= 1L) "class" else "classes"
        )
    } else {
        sprintf("<EnergyPlus IDD v%s (%s) with %i %s>", x$version(), x$build(),
            n, if (n <= 1L) "class" else "classes"
        )
    }
}
# }}}

#' @export
# ==.Idd {{{
`==.Idd` <- function(e1, e2) {
    if (!is_idd(e2)) return(FALSE)
    identical(get_priv_env(e1)$uuid(), get_priv_env(e2)$uuid())
}
# }}}

#' @export
# !=.Idd {{{
`!=.Idd` <- function(e1, e2) {
    Negate(`==.Idd`)(e1, e2)
}
# }}}

#' @export
# .DollarNames.Idd {{{
.DollarNames.Idd <- function(x, pattern = "") {
    grep(pattern, c(x$class_name(), names(x)), value = TRUE)
}
# }}}

# read_idd {{{
read_idd <- function(path, encoding = "unknown") {
    Idd$new(path, encoding = encoding)
}
# }}}

#' Use a specific EnergyPlus Input Data Dictionary (IDD) file
#'
#' @param idd Either a path, a connection, or literal data (either a single
#'     string or a raw vector) to an EnergyPlus Input Data Dictionary (IDD)
#'     file, usually named as `Energy+.idd`, or a valid version of IDD, e.g.
#'     `"8.9"`, `"8.9.0"`.
#' @param download If `TRUE` and argument `idd`, the IDD file will be
#'     downloaded from [EnergyPlus GitHub Repository](https://github.com/NREL/EnergyPlus),
#'     and saved to [tempdir()]. It will be parsed after it is downloaded
#'     successfully. A special value of `"auto"` can be given, which will
#'     automatically download corresponding IDD file if the Idd object is
#'     currently not available. It is useful in case when you only want to edit
#'     an EnergyPlus Input Data File (IDF) directly but do not want to install
#'     whole EnergyPlus software. Default is `FALSE`.
#' @param ver A valid EnergyPlus version, e.g. `"8"`, `"8.7"` or `"8.7.0"`.
#'     For `download_idd()`, the special value `"latest"`, which is default,
#'     means the latest version.
#' @param dir A directory to indicate where to save the IDD file. Default:
#'     current working directory.
#' @param encoding The file encoding of input IDD. Should be one of `"unknown"`,
#'     `"Latin-1" and `"UTF-8"`. The default is `"unknown"` which means that the
#'     file is encoded in the native encoding.
#'
#' @details
#' `use_idd()` takes a valid version or a path of an EnergyPlus Input Data
#' Dictionary (IDD) file, usually named "Energy+.idd" and return an `Idd`
#'  object. For details on `Idd` class, please see [Idd].
#'
#' `download_idd()` downloads specified version of EnergyPlus IDD file from
#' [EnergyPlus GitHub Repository](https://github.com/NREL/EnergyPlus). It is
#' useful in case where you only want to edit an EnergyPlus Input Data File
#' (IDF) directly but do not want to install whole EnergyPlus software.
#'
#' `avail_idd()` returns versions of all cached `Idd` object.
#'
#' `is_avail_idd()` returns `TRUE` if input version of IDD file has been parsed
#' and cached.
#'
#' eplusr tries to detect all installed EnergyPlus in default installation
#' locations when loading. If argument `idd` is a version, eplusr will try the
#' follow ways sequentially to find corresponding IDD:
#'
#' * The cached `Idd` object of that version
#' * `"Energy+.idd"` file distributed with EnergyPlus of that version (see
#'   [avail_eplus()]).
#' * The `"VX-Y-Z-Energy+.idd"` file distributed along with IDFVersionUpdater
#'   from the latest EnergyPlus detected.
#'
#' @return
#' * `use_idd()` returns an `Idd` object
#' * `download_idd()` returns an invisible integer `0` if succeed. Also an
#'   attribute named `file` which is the full path of the downloaded IDD file;
#' * `avail_idd()` returns a [numeric_version][base::numeric_version()] vector
#'   or `NULL` if no available Idd object found.
#' * `is_avail_idd()` returns a single logical vector.
#'
#' @examples
#' \dontrun{
#' # get all available Idd version
#' avail_idd()
#'
#' # check if specific version of Idd is available
#' is_avail_idd("8.5")
#'
#' # download latest IDD file from EnergyPlus GitHub repo
#' str(download_idd("latest", tempdir()))
#'
#' # use specific version of Idd
#' # only works if EnergyPlus v8.8 has been found or Idd v8.8 exists
#' use_idd("8.8")
#'
#' # If Idd object is currently not avail_idd, automatically download IDD file
#' # from EnergyPlus GitHub repo and parse it
#' use_idd("8.8", download = "auto")
#'
#' # now Idd v8.8 should be available
#' is_avail_idd("8.8")
#'
#' # get specific version of parsed Idd object
#' use_idd("8.8")
#'
#' avail_idd() # should contain "8.8.0"
#' }
#' @seealso [Idd] Class for parsing, querying and making modifications to
#' EnergyPlus IDD file
#'
#' @export
#' @author Hongyuan Jia
# use_idd {{{
use_idd <- function(idd, download = FALSE, encoding = "unknown") {
    if (is_idd(idd)) return(idd)

    assert_vector(idd, len = 1L)

    # if input is a file path or literal IDD string
    if (!is_idd_ver(idd)) {
        return(tryCatch(read_idd(idd, encoding = encoding), eplusr_error_read_lines = function(e) {
            abort(paste0("Parameter 'idd' should be a valid version, a path, or ",
                "a single character string of an EnergyPlus Input Data ",
                "Dictionary (IDD) file (usually named 'Energy+.idd'). ",
                "Invalid input found: ", surround(idd), "."
            ), "read_lines")
        }))
    }

    # make sure to print multiple version message only once
    ver_in <- standardize_ver(idd, complete = FALSE)
    # first check if version already exists
    ver <- match_minor_ver(ver_in, c(names(.globals$idd), names(.globals$eplus)), "idd")
    # if not exists, try to find more
    if (is.na(ver)) ver <- match_minor_ver(ver_in, ALL_IDD_VER, "idd")

    # directly download if specified
    if (isTRUE(download)) {
        dl <- download_idd(ver, dir = tempdir())
        idd <- attr(dl, "file")
    } else {
        # if found in cache, return it directly
        if (is_avail_idd(ver)) return(.globals$idd[[as.character(ver)]])

        verbose_info("IDD v", ver, " has not been parsed before.\nTry to locate ",
            "'Energy+.idd' in EnergyPlus v", ver, " installation folder ",
            surround(eplus_default_path(ver)), ".")

        # if corresponding EnergyPlus folder not found
        if (!is_avail_eplus(ver)) {
            verbose_info("Failed to locate 'Energy+.idd' because EnergyPlus v",
                ver, " is not available.")

            # try to locate using latest IDFVersionUpdater
            idd <- find_idd_from_updater(ver)

            # if still failed
            if (is.null(idd)) {
                # download IDD if auto is specified
                if (identical(download, "auto")) {
                    verbose_info("Starting to download the IDD file from EnergyPlus GitHub repo...")
                    dl <- download_idd(ver, dir = tempdir())
                    idd <- attr(dl, "file")
                # else issue an error
                } else {
                    abort(paste0("Failed to locate IDD v", ver, ".\n",
                        "You may want to set 'download' to TRUE or ",
                        "\"auto\" to download the IDD file from EnregyPlus ",
                        "GitHub repo."
                    ), "locate_idd")
                }
            }
        # if corresponding EnergyPlus folder is found
        } else {
            config <- eplus_config(ver)
            idd <- normalizePath(file.path(config$dir, "Energy+.idd"), mustWork = FALSE)

            # but IDD file is missing
            if (!file.exists(idd)) {
                verbose_info("'Energy+.idd' file does not exist in EnergyPlus v",
                    config$version, " installation folder ", surround(config$dir), "."
                )

                # try to locate using latest IDFVersionUpdater
                idd <- find_idd_from_updater(ver)

                # if still failed
                if (is.null(idd)) {
                    # download IDD if auto is specified
                    if (identical(download, "auto")) {
                        verbose_info("Starting to download the IDD file from EnergyPlus GitHub repo...")
                        dl <- download_idd(ver, dir = tempdir())
                        idd <- attr(dl, "file")
                    # else issue an error
                    } else {
                        abort(paste0("Failed to locate IDD v", ver,
                            "You may want to set 'download' to TRUE or ",
                            "\"auto\" to download the IDD file from EnregyPlus GitHub repo."
                        ), "locate_idd")
                    }
                }
            }
        }

        if (ver <= "8.3") encoding <- "Latin-1"
    }

    verbose_info("IDD file found: ", surround(idd), ".")
    verbose_info("Start parsing...")
    idd <- read_idd(idd, encoding = encoding)
    verbose_info("Parsing completed.")
    idd
}
# }}}

#' @rdname use_idd
#' @export
# download_idd {{{
download_idd <- function(ver = "latest", dir = ".") {
    assert_vector(ver, len = 1L)
    ver <- standardize_ver(ver, complete = FALSE)

    if (!is_eplus_ver(ver)) abort("'ver' must be a valid EnergyPlus version", "invalid_eplus_ver")

    ori_ver <- ver
    # if no patch version is given
    if (is.na(ver[, 3L])) {
        ver <- match_minor_ver(ver, ALL_IDD_VER, "idd", TRUE)

        latest_ver <- LATEST_EPLUS_VER

        # If the input is 9 or 9.0, ver will be changed to 9.0.1 from above,
        # here change the file to download to "V9-0-0-Energy+.idd" as there is
        # no "V9-0-1-Energy+.idd" and only "V9-0-0-Energy+.idd", which does not
        # follow past conventions.

        # store the original version in order to rename downloaded file from
        # "V9-0-0-Energy+idd" to "V9-0-1-Energy+.idd", if applicable
        ori_ver <- ver

        if (ver == numeric_version("9.0.1")) ver <- numeric_version("9.0.0")

    # in case explicitly download IDD version "9.0.1"
    # change the file to download to "V9-0-0-Energy+.idd" as there is no
    # "V9-0-1-Energy+.idd" and only "V9-0-0-Energy+.idd", which does not follow
    # past conventions.
    } else if (ver == numeric_version("9.0.1")){
        ver <- numeric_version("9.0.0")
        latest_ver <- LATEST_EPLUS_VER
    } else {
        latest_ver <- LATEST_EPLUS_VER
    }

    base_url <- paste0("https://raw.githubusercontent.com/NREL/EnergyPlus/v", latest_ver, "/idd/")

    ver_dash <- paste0(ver[, 1L], "-", ver[, 2L], "-", ver[, 3L])

    ver <- as.character(ver)
    if (ver == latest_ver) {
        file_url <- "Energy%2B.idd.in"
    } else {
        file_url <- paste0("V", ver_dash, "-Energy%2B.idd")
    }

    url <- paste0(base_url, file_url)

    if (ori_ver == numeric_version("9.0.1")) {
        file <- paste0("V9-0-1-Energy+.idd")
    } else {
        file <- paste0("V", ver_dash, "-Energy+.idd")
    }
    dest <- normalizePath(file.path(dir, file), mustWork = FALSE)
    res <- download_file(url, dest)

    if (res != 0L) abort(sprintf("Failed to download EnergyPlus IDD v%s.", ver), "download_idd")

    if (ver == latest_ver) {
        cmt <- ALL_EPLUS_RELEASE_COMMIT[version == ver][["commit"]]

        l <- read_lines(dest, trim = FALSE)

        l[1L:2L, string := c(paste0("!IDD_Version ", ver), paste0("!IDD_BUILD ", cmt))]

        write_lines(l, dest)
    }

    verbose_info("EnergyPlus v", ver, " IDD file ", surround(file), " has been successfully ",
        "downloaded into ", normalizePath(dir), ".")

    attr(res, "file") <- dest
    invisible(res)
}
# }}}

#' @rdname use_idd
#' @export
# avail_idd {{{
avail_idd <- function() {
    res <- names(.globals$idd)
    if (!length(res)) return(NULL)
    sort(numeric_version(res))
}
# }}}

#' @rdname use_idd
#' @export
# is_avail_idd {{{
is_avail_idd <- function(ver) {
    if (is.character(ver) && "latest" %chin% ver) {
        abort("'latest' notation for IDD version is not allowed here. Please give specific versions.", "ambiguous_idd_ver")
    }
    !is.na(convert_to_idd_ver(ver, strict = TRUE, all_ver = names(.globals$idd)))
}
# }}}

# find_idd_from_updater {{{
find_idd_from_updater <- function(ver) {
    ver <- standardize_ver(ver, strict = TRUE)
    # check if there are any EnergyPlus detected whose version is
    # newer than specified version
    vers <- rev(avail_eplus()[avail_eplus() >= ver])

    if (!length(vers)) return(NULL)

    # use the IDD file distributed with IDFVersionUpdater
    file <- paste0("V", ver[, 1L], "-", ver[, 2L], "-0-Energy+.idd")
    idd <- NULL

    msg <- NULL
    for (i in seq_along(vers)) {
        line_break <- if (i == 1L) "" else "\n"
        dir <- file.path(eplus_config(vers[i])$dir, "PreProcess", "IDFVersionUpdater")
        idd_path <- normalizePath(file.path(dir, file), mustWork = FALSE)
        msg <- paste0(msg, line_break, "Try to locate ", surround(file), " in EnergyPlus v",
            vers[i], " IDFVersionUpdater folder ", surround(dir), "."
        )

        if (!file.exists(idd_path)) {
            msg <- paste0(msg, " --> Failed")
        } else {
            msg <- paste0(msg, " --> Succeeded")
            idd <- idd_path
            break
        }
    }

    verbose_info(msg)
    idd
}
# }}}
# get_idd_from_ver {{{
# Get Idd object from input IDF version
#
# @param idf_ver NULL or a valid IDF version
# @param idd NULL or valid input for [use_idd()]
# @param warn If `TRUE`, extra warning message will be shown
get_idd_from_ver <- function(idf_ver = NULL, idd = NULL, warn = TRUE) {
    if (!is.null(idf_ver)) {
        if (is.null(idd)) {
            # if input IDF has a version but neither that version of EnergyPlus
            # nor IDD is available, rewrite the message
            idd <- tryCatch(use_idd(idf_ver),
                eplusr_error_locate_idd = function(e) {
                    mes <- stri_replace_all_fixed(conditionMessage(e),
                        "You may want to set 'download'",
                        "You may want to use 'use_idd()' and set 'download'"
                    )
                    abort(mes, "locate_idd")
                }
            )
        } else {
            idd <- use_idd(idd)
            if (warn && idf_ver[, 1L:2L] != idd$version()[, 1L:2L]) {
                warn(paste0("Version mismatch. The IDF file parsing has a differnet ",
                    "version (", idf_ver, ") than the IDD file used (",
                    idd$version(), "). Parsing errors may occur."),
                    "use_mismatch_idd"
                )
            }
        }
    } else {
        mes <- "Missing version field in input IDF."

        if (!is.null(idd)) {
            idd <- use_idd(idd)
            if (warn) {
                warn(paste0(mes, " The given IDD version ", idd$version(),
                    " will be used. Parsing errors may occur."), "use_hard_coded_idd"
                )
            }
        } else {
            if (is.null(avail_idd())) {
                abort(paste(mes, "No parsed IDD was available to use."), "no_avail_idd")
            }

            # which.max does not work with numeric_version objects
            idd <- use_idd(avail_idd()[max(order(avail_idd()))])
            if (warn) {
                warn(paste0(mes, " The latest parsed IDD version ", idd$version(),
                        " will be used. Parsing errors may occur."
                    ), "use_latest_idd"
                )
            }
        }
    }
    idd
}
# }}}

# vim: set fdm=marker:
