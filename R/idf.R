#' Read, modify, and run an EnergyPlus model
#'
#' eplusr provides parsing EnergyPlus Input Data File (IDF) files and strings
#' in a hierarchical structure, which was extremely inspired by [OpenStudio utilities library](https://openstudio-sdk-documentation.s3.amazonaws.com/cpp/OpenStudio-2.5.1-doc/utilities/html/idf_page.html),
#' but with total different data structure under the hook.
#'
#' @section Overview:
#'
#' eplusr uses `Idf` class to present the whole IDF file and use `IdfObject`
#' to present a single object in IDF. Both `Idf` and `IdfObject` contain member
#' functions for helping modify the data in IDF so it complies with the
#' underlying IDD (EnergyPlus Input Data Dictionary).
#'
#' Under the hook, eplusr uses a SQL-like structure to store both IDF and IDD
#' data in `data.frame` format. To speed up the whole process, the
#' [data.table::data.table()] is used instead of the base `data.frame`. Every
#' IDF is parsed and stored in four tables:
#'
#' * `object`: contains object IDs and names.
#' * `value`: contains field values.
#' * `comment`: contains object comments.
#' * `value_reference`: contains cross-reference of field values.
#'
#' IDD file is parsed and stored in a similar structure. For details, please see
#' [Idd class][idd].
#'
#' So to modify an EnergyPlus model in eplusr is equal to change the data in
#' those four tables accordingly, in the context of specific IDD data.
#'
#' All IDF reading process starts with [read_idf()] which returns an `Idf`
#' object. The model will be printed in a similar style you see in IDFEditor,
#' with additional heading lines show the `Path`, `Version` of the model. The
#' classes of objects in the model are ordered by group and the number of
#' objects in classes are shown in square bracket.
#'
#' @section Usage:
#'
#' \preformatted{
#' model$version()
#' model$path()
#' model$group_name(all = FALSE)
#' model$class_name(all = FALSE)
#' model$is_valid_group(group, all = FALSE)
#' model$is_valid_class(class, all = FALSE)
#' model$definition(class)
#' model$object_id(class = NULL, simplify = FALSE)
#' model$object_name(class = NULL, simplify = FALSE)
#' model$object_num(class = NULL)
#' model$is_valid_id(id)
#' model$is_valid_name(name)
#' model$object(id)
#' model$object_in_class(class)
#' model$search_object(pattern, class = NULL)
#' model$ClassName
#' model[[ClassName]]
#' model$dup_object(object, new_name = NULL)
#' model$add_object(class, value = NULL, comment = NULL, default = TRUE, all = FALSE)
#' model$ins_object(object)
#' model$set_object(object)
#' model$del_object(object, referenced = FALSE)
#' model$search_value(pattern)
#' model$replace_value(pattern, replacement)
#' model$validate()
#' model$is_valid
#' model$string(comment = TRUE, header = TRUE, ...)
#' model$is_unsaved()
#' model$save(path = NULL, format = c("sorted", "new_top", "new_bot"), overwrite = FALSE, copy_external = TRUE)
#' model$clone()
#' model$run(weather = NULL, dir = NULL, wait = TRUE, force = FALSE)
#' model$print(plain = FALSE)
#' print(model)
#' }
#'
#' @section Basic Info:
#' ```
#' model$version()
#' model$path()
#' model$group_name(all = FALSE)
#' model$class_name(all = FALSE)
#' model$is_valid_group(group, all = FALSE)
#' model$is_valid_class(class, all = FALSE)
#' ```
#'
#' `$version()` will return the version of current model.
#'
#' `$path()` will return the path of current model or `NULL` if the model is
#'     created using a character vector.
#'
#' `$group_name()` will return all groups the model contains when `all` is `FALSE`
#'     or all groups the Idd contains when `all` is `TRUE`.
#'
#' `$class_name()` will return all classes the model contains when `all` is `FALSE`
#'     or all classes the Idd contains when `all` is `TRUE`.
#'
#' `$is_valid_group()` will return `TRUE`s if given group names are valid for
#'     current model (when `all` is `FALSE`) or current Idd (when `all` is
#'     `TRUE`).
#'
#' `$is_valid_class()` will return `TRUE`s if given class names are valid for
#'     current model (when `all` is `FALSE`) or current Idd (when `all` is
#'     `TRUE`).
#'
#' **Arguments**
#'
#' * `all`: If `FALSE`, only values in current model will be returned. If
#'     `TRUE`, all values in Idd will be returned. Default: `FALSE`.
#' * `group`: A character vector contains group names.
#' * `class`: A character vector contains class names.
#'
#' @section Definition:
#' ```
#' model$definition(class)
#' ```
#'
#' `$definition()` will return the definitions, i.e. the `IddObject`s, of given
#'     classes which contain all data used for parsing `IdfObject`s. For details
#'     of `IdfObject`, please see [IddObject class][idd_object].
#'
#' **Arguments**
#'
#' * `class`: A character vector contains class names.
#'
#' @section Object Info:
#'
#' ```
#' model$object_id(class = NULL, simplify = FALSE)
#' model$object_name(class = NULL, simplify = FALSE)
#' model$object_num(class = NULL)
#' model$is_valid_id(id)
#' model$is_valid_name(name)
#' ```
#'
#' `$object_id()` and `$object_name()` will return all object IDs and names
#'     in specified class respectively. For `$object_name()`, if the specified
#'     class does not have name attributes, such as `SimulationContrl`, `NA`
#'     will be returned.
#'
#' `$is_valid_id()` and `$is_valid_name()` will return `TRUE`s if given integers
#' or strings are valid object IDs or object names respectively.
#'
#' `$object_num()` will return the number of objects in specified classes.
#'
#' **Arguments**
#'
#' * `id`: An integer vector to check.
#' * `name`: A character vector to check.
#' * `class`: A character vector contains valid class names.
#' * `simplify`: If FALSE, a list with each member being the data per class will
#'     be returned. Otherwise, an integer vector (for `$object_id`) or a
#'     character vector (for `$object_name`) will be returned.
#'
#' @section Object Query:
#'
#' \preformatted{
#' model$object(which)
#' model$object_in_class(class)
#' model$search_object(pattern, class = NULL)
#' model$ClassName
#' model[[ClassName]]
#' }
#'
#' `$object()` will return a list of `IdfObject`s specified by object IDs or
#'     names.
#'
#' `$object_in_class()` will return a list of all `IdfObject`s in specified
#'     classes.
#'
#' `$search_object()` will return a list of `IdfObject`s whose names meet the
#'     given pattern in specified classes.
#'
#' eplusr also provides custom S3 method of `$` and \code{[[} to make it more
#' convenient to get `IdfObject`s in class. Basically, `model$ClassName` and
#' \code{model[[ClassName]]}, where `ClassName` is a single valid class name, is
#' equivalent to `model$object_in_class(ClassName)`.
#'
#' All above methods will return a named list of `IdfObject`s. If the class does
#'     not have name attribute, then `NA` will be used.
#'
#' `IdfObject` is a class that provides more detailed information methods to
#'     modify a single object in an `Idf` object. For detailed explanations,
#'     please see [IdfObject class][idf_object].
#'
#' **Arguments**
#'
#' * `object`: Either an integer vector of valid object IDs or a character vector
#'     of valid object names.
#' * `class`: A character vector of valid class names.
#' * `pattern`: A regular expression. It will be directly passed to
#'     `stringr::str_detect`.
#' * `ClassName`: A single length character vector of one valid class name,
#'     where all characters other than letters and numbers are replaced by a
#'     underscore `_`.
#'
#' @section Object Modification:
#' ```
#' model$dup_object(object, new_name = NULL)
#' model$add_object(class, value = NULL, comment = NULL, default = TRUE, all = FALSE)
#' model$ins_object(object)
#' model$set_object(object)
#' model$del_object(object, referenced = FALSE)
#' model$search_value(pattern)
#' model$replace_value(pattern, replacement)
#' ```
#'
#' `$dup_object()` will duplicate objects specified by object IDs or names. The
#'     newly created objects will be renamed automatically if new names are not
#'     given, with a suffix `"_1"`, `"_2"` and etc.
#'
#' `$add_object()` will add objects in the specified class.
#'
#' `$ins_object()` will insert objects from other IDF into current IDF.
#'
#' `$set_object()` will set the value of fields in the objects specified by object
#'     IDs or names.
#'
#' `$del_object()` will delete objects specified by object IDs or names.
#'
#' `$search_value()` will return values that match the given pattern.
#'
#' `$replace_value()` will return replace values that match the given pattern.
#'
#' **NOTE**: There is no field validation when using `$replace_value()` to
#'     change field values. `$replace_value()` should be treated as a low-level
#'     method which should be used with caution.
#'
#' **Arguments**
#'
#' * `object`: Either an integer vector of valid object IDs or a character vector
#'     of valid object names.
#' * `new_name`: A character vector with the same length as the number of
#'     objects to be duplicated.
#' * `value`: A list which contains field values to set to the newly created
#'     objects. The class of each field value should comply with the definition
#'     in corresponding IDD. Field names of value in each class can be given. If
#'     not named, the input values will be set to fields according to their
#'     order of appearance.
#' * `comment`: A list which contains comments to set to the newly created
#'     objects.
#' * `default`: If `TRUE`, all empty fields will be filled with their default
#'     values if possible.
#' * `all`: If `TRUE`, all fields in the class will be returned, even if there
#'     are no input values for them. If `FALSE`, only minimum fields will be
#'     returned.
#' * `referenced`: If `TRUE`, all objects that reference the targets to delete
#'     will also be deleted.
#' * `pattern`: A regular expression used to search for values.
#' * `replacement`: A regular expression used to replace values.
#'
#' @section Validation:
#'
#' ```
#' model$validate()
#' model$is_valid()
#' ```
#'
#' `$validate()` will check if there are errors in current model under different
#'     strictness level.
#'
#' `$is_valid()` will check if there are no errors in current model under different
#'     strictness level.
#'
#'
#' The strictness level can be changed using [eplusr_option()]. Default is
#'     `"final". `There are three different validate levels, i.e. `"none"`,
#'     `"draft"` and `"final"`:
#'
#'   * For `"none"`, none validation will be done;
#'   * For `"draft"`, checking of invalid autosize, autocalculate, numeric,
#'     integer, and choice field values will be done;
#'   * For `"final"`, besides above, checking of missing required objects,
#'     duplicated unique objects, object name conflicts, missing required fields
#'     and invalid field value reference will also be done.
#'
#' @section Format Output:
#'
#' ```
#' model$string(comment = TRUE, header = TRUE)
#' ```
#'
#' `$string()` will return the text format of an IDF file.
#'
#' **Arguments**
#'
#' * `comment`: If `FALSE`, all comments will not be included.
#' * `header`: If `FALSE`, the header will not be included.
#'
#' @section Save:
#'
#' ```
#' model$is_unsaved()
#' model$save(path = NULL, format = c("asis", "sorted", "new_top", "new_bot"), overwrite = FALSE, copy_external = TRUE)
#' ```
#'
#' `$is_unsaved()` will check if there are modifications on the model since it was
#'     read or since last time it was saved.
#'
#' `$save()` will save the model into local disk.
#'
#' **Arguments**
#'
#' * `path`: A path where to save the model. If `NULL`, the path of the model
#'     itself will be used.
#' * `format`: A string to specify the saving format. Should be one of `"asis"`,
#'     `"sorted"`, `"new_top"`, and `"new_bot"`. If `"asis"`, which is the default, the
#'     model will be saved in the same format as it is. If the model does not
#'     contain any format saving option, which is typically the case when the
#'     model was not saved using eplusr or IDFEditor, `"sorted"` will be used.
#'     `"sorted"`, `"new_top"` and `"new_bot"` are the same as the save options
#'     `"Sorted"`, `"Original with New at Top"`, and `"Original with New at Bottom"`
#'     in IDFEditor.
#' * `overwrite`: Whether to overwrite the file if it already exists. Default is
#'     `FALSE`.
#' * `copy_external`: If `TRUE`, the external files will also be copied into the
#'     same directory. Currently, only `Schedule:File` class is supported.
#'     Default is `FALSE`.
#'
#' @section Clone:
#'
#' ```
#' model$clone(deep = FALSE)
#' ```
#'
#' `$clone()` will copy and returned the cloned model. Because `Idf` use
#'     `R6Class` under the hook, `idf_2 <- idf_1` does not copy `idf_1` at all
#'     but only create a new binding to `idf_1`. Modify `idf_1` will also affect
#'     `idf_2` as well, as these two are exactly the same thing underneath.
#'
#' **Arguments**
#'
#' * `deep`: Not used. Keep it here just for compatible with the default clone
#'     method provided by `R6Class`.
#'
#' @section Run Model:
#'
#' ```
#' model$run(weather, dir = NULL, wait = TRUE, force = FALSE)
#' ```
#'
#' `$run()` will run the current model within specified weather using
#'     corresponding version of EnergyPlus. The model and the weather used will
#'     be copied to the output directory. An `EplusJob` will be returned which
#'     provides detailed info of the simulation and methods to collect
#'     simulation results. Please see [eplus_job()] for more detailed.
#'
#' eplusr uses the EnergyPlus command line interface which was introduced since
#'     EnergyPlus 8.3.0. So `$run` only supports models with version higher
#'     than 8.3.0.
#'
#' eplusr uses the EnergyPlus SQL output for extracting simulation results. In
#' order to do so, a object in `Output:SQLite` with `Option Type` value of
#' `SimpleAndTabular` will be automatically created if it does not exists.
#'
#' **Arguments**
#'
#' * `weather`: A path to an `.epw` file or an `Epw` object.
#' * `dir`: The directory to save the simulation results. If `NULL`, which is
#'     the default, the model folder will be used.
#' * `echo`: Whether to print the standard output and error of EnergyPlus to
#'           the screen. Default is `FALSE`.
#' * `force`: Whether to stop the background EnergyPlus process and start the
#'     simulation again.
#'
#' @section Print:
#' ```
#' model$print(plain = FALSE)
#' print(model)
#' ```
#'
#' `$print()` will print the model in the similar format as what you will see in
#'     IDFEditor.
#'
#' **Arguments**
#'
#' * `plain`: If `TRUE`, the model will be printed in plain text format.
#'
#' @docType class
#' @name idf
#' @seealso [IdfObject class][idf_object]
#' @author Hongyuan Jia
#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
NULL

#' Read an EnergyPlus Input Data File (IDF)
#'
#' `read_idf` takes an EnergyPlus Input Data File (IDF) as input and returns an
#' `Idf` object. For more details on `Idf` object, please see [Idf class][idf].
#'
#' @param path A path to an EnergyPlus IDF file or a string that can be parsed as
#'     an IDF. The file extension does not matter. So models stored in `TXT`
#'     format are still able to correctly be parsed.
#' @param idd  Any acceptable input of [use_idd()]. If `NULL`, which is the
#'     default, the version of IDF will be passed to [use_idd()]. If the input
#'     IDF does not have a version field (possible for ".ddy" files), then it
#'     will be parsed using the latest version of IDD cached, with a warning.
#' @return An `Idf` object.
#' @seealso [Idf class][idf]
#' @export
# read_idf {{{
read_idf <- function (path, idd = NULL) {
    # have to clone the generator first in order to leave the original Idf
    # generator untouched
    gen <- clone_generator(Idf)

    # get the clone method
    clone <- gen$clone_method

    # get function body
    ori_expr <- as.list(body(clone))

    # get the body length
    len <- length(ori_expr)

    # insert new expressions just at the beginning and also before the return
    # reference:
    # https://stackoverflow.com/questions/38732663/how-to-insert-expression-into-the-body-of-a-function-in-r
    new_expr <- append(ori_expr,
        as.list(
            expression(
                shared <- c("m_version", "m_idf_tbl", "m_idd_tbl", "m_log", "m_idfobj_generator", "m_iddobj_generator"),
                for (nm in shared) {
                    private_bind_env[["m_idfobj_generator"]][["self"]][["private_fields"]][[nm]] <- private_bind_env[[nm]]
                }
            )
        ), after = len-1)
    # always use deep clone
    new_expr <- append(new_expr, as.list(expression(deep <- TRUE)), after = 1L)

    # assign new body
    body(gen$clone_method) <- as.call(new_expr)
    body(gen$public_methods$clone) <- as.call(new_expr)

    gen$new(path, idd)
}
# }}}

# Idf {{{
Idf <- R6::R6Class(classname = "Idf",
    public = list(

        # INITIALIZE {{{
        initialize = function (path, idd = NULL) {

            # only store if input is a path
            if (length(path) == 1L) {
                if (file.exists(path)) private$m_path <- normalizePath(path)
            }

            idf_file <- parse_idf_file(path, idd)
            # warn if input is an imf file
            is_imf <- attr(idf_file, "is_imf")
            if (is_imf) {
                warning("Currently, Imf file is not fully supported. All ",
                        "EpMacro lines will be treated as normal comments of ",
                        "the nearest downwards object.", call. = FALSE)
            }
            private$m_is_imf <- is_imf
            private$m_version <- idf_file$version
            # init options

            idd <- attr(idf_file, "idd")
            # init idd tbl
            private$m_idd_tbl <- ._get_private(idd)$m_idd_tbl
            # get IddObject R6ClassGenerator
            private$m_iddobj_generator <- ._get_private(idd)$m_iddobj_generator

            # init idf tbl
            private$m_idf_tbl <- list2env  (
                idf_file[c("object", "value", "value_reference", "comment")],
                parent = emptyenv()
            )

            # init log data
            private$m_log <- new.env(hash = FALSE, parent = emptyenv())

            # add a uuid
            private$m_log$uuid <- uuid::UUIDgenerate(use.time = TRUE)

            private$m_log$unsaved <- FALSE
            private$m_log$order <- private$m_idf_tbl$object[, list(object_id)][
                , object_order := 0L]

            private$m_log$view_in_ip <- eplusr_option("view_in_ip")
            private$m_log$num_digits <- eplusr_option("num_digits")
            private$m_log$save_format <- idf_file$options$save_format

            # create the IdfObject R6ClassGenerator for this specific Idf
            private$m_idfobj_generator <- create_idfobj_generator(self, private, IdfObject)
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        version = function ()
            i_version(self, private),

        path = function ()
            i_path(self, private),

        group_name = function (all = FALSE)
            i_group_name(self, private, type = ifelse(all, "idd", "idf")),

        class_name = function (all = FALSE)
            i_class_name(self, private, type = ifelse(all, "idd", "idf")),

        object_id = function (class = NULL, simplify = FALSE)
            i_object_id(self, private, class, simplify),

        object_name = function (class = NULL, simplify = FALSE)
            i_object_name(self, private, class, simplify),

        object_num = function (class = NULL)
            i_object_num(self, private, class),

        is_valid_group = function (group, all = FALSE)
            i_is_valid_group_name(self, private, group, type = ifelse(all, "idd", "idf")),

        is_valid_class = function (class, all = FALSE)
            i_is_valid_class_name(self, private, class, type = ifelse(all, "idd", "idf")),

        is_valid_id = function (id)
            i_is_valid_object_id(self, private, id),

        is_valid_name = function (name)
            i_is_valid_object_name(self, private, name),

        is_unsaved = function ()
            i_is_unsaved_idf(self, private),

        definition = function (class)
            i_iddobject(self, private, class),

        object = function (object)
            i_idfobject(self, private, object),

        object_in_class = function (class)
            i_idfobject_in_class(self, private, class),

        search_object = function (pattern, class = NULL)
            i_search_object(self, private, pattern, class),

        dup_object = function (object, new_name = NULL)
            i_dup_object(self, private, object, new_name),

        add_object = function (class, value = NULL, comment = NULL, default = TRUE, all = FALSE)
            i_add_object(self, private, class, value, comment, default, all),

        ins_object = function (object)
            i_ins_object(self, private, object),

        set_object = function (object, value = NULL, comment = NULL, default = FALSE)
            i_set_object(self, private, object, value, comment, default),

        del_object = function (object, referenced = FALSE)
            i_del_object(self, private, object, referenced),

        search_value = function (pattern)
            i_search_value(self, private, pattern),

        replace_value = function (pattern, replacement)
            i_replace_value(self, private, pattern, replacement),

        validate = function ()
            i_validate_idf(self, private),

        is_valid = function ()
            i_is_valid_idf(self, private),

        string = function (comment = TRUE, header = TRUE, ...)
            i_object_string(self, private, comment = comment, header = header, ...),

        save = function (path = NULL, format = eplusr_option("save_format"), overwrite = FALSE, copy_external = TRUE)
            i_idf_save(self, private, path, format, overwrite, copy_external),

        run = function (weather = NULL, dir = NULL, wait = TRUE, force = FALSE)
            i_idf_run(self, private, weather, dir, wait, force),

        print = function (plain = FALSE)
            i_print_idf(self, private, plain)
        # }}}

    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_path = NULL,
        m_is_imf = NULL,
        m_version = NULL,
        m_idd_tbl = NULL,
        m_idf_tbl = NULL,
        m_log = NULL,
        m_idfobj_generator = NULL,
        m_iddobj_generator = NULL,
        # }}}

        deep_clone = function (name, value)
            i_deep_clone(self, private, name, value)
    )
)
# }}}

#' @export
# [[.Idf {{{
'[[.Idf' <- function(x, i) {
    if (is_string(i)) {
        funs <- setdiff(ls(x), "initialize")
        if (i %in% funs) {
            NextMethod()
        } else {
            in_nm <- i_underscore_name(i)

            self <- .subset2(.subset2(x, ".__enclos_env__"), "self")
            priv <- .subset2(.subset2(x, ".__enclos_env__"), "private")

            all_nm <- i_class_name(self, priv, type = "idf")

            all_nm_u <- i_underscore_name(all_nm)

            m <- match(in_nm, all_nm_u)

            if (is.na(m)) {
                NextMethod()
            } else {
                .subset2(x, "object_in_class")(all_nm[m])
            }
        }
    } else {
        NextMethod()
    }
}
# }}}

#' @export
# $.Idf {{{
'$.Idf' <- function (x, name) {
    if (is_string(name)) {
        funs <- setdiff(ls(x), "initialize")
        if (name %in% funs) {
            NextMethod()
        } else {
            in_nm <- i_underscore_name(name)

            self <- .subset2(.subset2(x, ".__enclos_env__"), "self")
            priv <- .subset2(.subset2(x, ".__enclos_env__"), "private")

            all_nm <- i_class_name(self, priv, type = "idf")

            all_nm_u <- i_underscore_name(all_nm)

            m <- match(in_nm, all_nm_u)

            if (is.na(m)) {
                NextMethod()
            } else {
                .subset2(x, "object_in_class")(all_nm[m])
            }
        }
    } else {
        NextMethod()
    }
}
# }}}
