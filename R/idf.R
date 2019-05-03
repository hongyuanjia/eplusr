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
#' @section Overview:
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
#' and modify EnergyPlus models. Below is the detailed documentation on each
#' method.
#'
#' @section Usage:
#'
#' \preformatted{
#' model <- read_idf(path)
#' model$version()
#' model$path()
#' model$group_name(all = FALSE, sorted = TRUE)
#' model$class_name(all = FALSE, sorted = TRUE)
#' model$is_valid_group(group, all = FALSE)
#' model$is_valid_class(class, all = FALSE)
#' model$definition(class)
#' model$object_id(class = NULL, simplify = FALSE)
#' model$object_name(class = NULL, simplify = FALSE)
#' model$is_valid_id(id)
#' model$is_valid_name(name)
#' model$object_num(class = NULL)
#' model$object(which)
#' model$object_unique(class)
#' model$objects(which)
#' model$objects_in_class(class)
#' model$objects_in_group(group)
#' model$object_relation(which, direction = c("all", "ref_to", "ref_by"))
#' model$objects_in_relation(which, direction = c("ref_to", "ref_by"), class = NULL, recursive = FALSE)
#' model$search_object(pattern, class = NULL, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
#' model$ClassName
#' model[[ClassName]]
#' model$dup(...)
#' model$add(..., .default = TRUE, .all = FALSE)
#' model$set(..., .default = TRUE)
#' model$del(..., .ref_by = FALSE, .ref_to = FALSE, .recursive = FALSE, .force = FALSE)
#' model$insert(..., .unique = TRUE)
#' model$load(..., .unique = TRUE, .default = TRUE)
#' model$rename(...)
#' model$paste(in_ip = FALSE, ver = NULL, unique = TRUE)
#' model$search_value(pattern, class = NULL, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
#' model$replace_value(pattern, class = NULL, replacement, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
#' model$is_unsaved()
#' model$validate(level = eplusr_option("validate_level"))
#' model$is_valid(level = eplusr_option("validate_level"))
#' model$to_table(which = NULL, class = NULL, string_value = TRUE, unit = FALSE)
#' model$to_string(which = NULL, class = NULL, comment = TRUE, header = TRUE, format = eplusr_option("save_format"), leading = 4L, sep_at = 29L)
#' model$save(path = NULL, format = eplusr_option("save_format"), overwrite = FALSE, copy_external = TRUE)
#' model$run(weather = NULL, dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE)
#' model$clone(deep = TRUE)
#' model$print(zoom = c("object", "class", "group", "field"), order = TRUE)
#' print(model)
#' }
#'
#' @section Basic Info:
#' ```
#' model <- read_idf(path)
#' model$version()
#' model$path()
#' model$group_name(all = FALSE, sorted = TRUE)
#' model$class_name(all = FALSE, sorted = TRUE)
#' model$is_valid_group(group, all = FALSE)
#' model$is_valid_class(class, all = FALSE)
#' ```
#'
#' `$version()` returns the version of current model in a
#' [numeric_version][base::numeric_version()] format. This makes it easy to
#' direction compare versions of different model, e.g. `model1$version() > 8.6`
#' or `model1$version() > model2$version()`.
#'
#' `$path()` returns the full path of current model or `NULL` if the `Idf`
#' object is created using a character vector and not saved locally.
#'
#' `$group_name()` returns all groups the model contains when `all` is `FALSE`
#' or all groups the underlying [Idd] object contains when `all` is `TRUE`.
#'
#' `$class_name()` returns all classes the model contains when `all` is `FALSE`
#' or all classes the underlying [Idd] object contains when `all` is `TRUE`.
#'
#' `$is_valid_group()` returns `TRUE`s if given group names are valid for
#' current model (when `all` is `FALSE`) or current underlying [Idd] object
#' (when `all` is `TRUE`).
#'
#' `$is_valid_class()` returns `TRUE`s if given class names are valid for
#' current model (when `all` is `FALSE`) or underlying [Idd] object (when `all`
#' is `TRUE`).
#'
#' **Arguments**:
#'
#' * `path`: Either a path, a connection, or literal data (either a single
#'   string or a raw vector) to an EnergyPlus Input Data File (IDF).
#' * `all`: If `FALSE`, only values in current `Idf` object will be returned. If
#'   `TRUE`, all values in the underlying [Idd] will be returned. For
#'   `$is_valid_group()` and `$is_valid_class()`, `all` equals to `TRUE` means
#'   that input group or class names are checked in all existing ones in the
#'   underlying [Idd] object. Default: `FALSE`.
#' * `sorted`: Only applicable when `all` is `FALSE`. If `TRUE`, duplications in
#'   returned group or class names are removed, and unique names are further
#'   sorted according to their occurrences in the underlying [Idd] object.
#'   Default: `TRUE`.
#' * `group`: A character vector of valid group names.
#' * `class`: A character vector of valid class names.
#'
#' @section Definition:
#' ```
#' model$definition(class)
#' ```
#'
#' `$definition()` returns an [IddObject] of given class. [IddObject] contains
#' all data used for parsing and creating an [IdfObject]. For details, please
#' see [IddObject] class.
#'
#' **Arguments**:
#'
#' * `class`: A **single** string of valid class name in current IDD.
#'
#' @section Object Info:
#'
#' ```
#' model$object_id(class = NULL, simplify = FALSE)
#' model$is_valid_id(id)
#' model$object_name(class = NULL, simplify = FALSE)
#' model$is_valid_name(name)
#' model$object_num(class = NULL)
#' ```
#'
#' `$object_id()` returns an integer vector (when `simplify` is `TRUE`) or a
#' named list (when `simplify` is `FALSE`) of all object IDs in specified
#' classes. The returned list is named using specified class names.
#'
#' `$object_name()` returns a character vector (when `simplify` is `TRUE`) or a
#' named list (when `simplify` is `FALSE`) of all object names in specified
#' classes. The returned list is named using specified class names.
#'
#' `$is_valid_id()` and `$is_valid_name()` returns a logical vector whether the
#' given integer vector or character vector contains valid object IDs or names
#' respectively. Note that for `$is_valid_name()`, object name matching is
#' **case-insensitive**.
#'
#' `$object_num()` returns an integer vector of object numbers in specified
#' classes.
#'
#' **Arguments**:
#'
#' * `id`: An integer vector to check.
#' * `name`: A character vector to check.
#' * `class`: A character vector that contains valid class names. If `NULL`, all
#'   classes in current `Idf` object are used. Default: `NULL`.
#' * `simplify`: If `TRUE`, an integer vector (for `$object_id()`) or a
#'   character vector (for `$object_name`()) is returned. If `FALSE`, a list
#'   with each element being the data per class is returned. If `class` is
#'   `NULL`, the order of classes returned is the same as that in the underlying
#'   [Idd] object. Default: `FALSE`.
#'
#' @section Object Relation:
#' ```
#' model$object_relation(which, direction = c("all", "ref_to", "ref_by"))
#' ```
#'
#' Many fields in [Idd] can be referred by others. For example, the `Outside
#' Layer` and other fields in `Construction` class refer to the `Name` field
#' in `Material` class and other material related classes. Here it means that
#' the `Outside Layer` field **refers to** the `Name` field and the `Name` field
#' is **referred by** the `Outside Layer`.
#'
#' `$object_relation()` provides a simple interface to get this kind of
#' relation. It takes a single object ID or name and also a relation direction,
#' and returns an `IdfRelation` object which contains data presenting such
#' relation above. For instance, if `model$object_relation("WALL-1", "ref_to")`
#' gives results below:
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
#' This means that the value `"WD01"` of `Outside Layer` in a construction named
#' `WALL-1` refers to a material named `WD01`. All those objects can be further
#' easily extracted using `$objects_in_relation()` method described below.
#'
#' **Arguments**:
#'
#' * `which`: Either a single integer of object ID or a string of object name.
#' * `direciton`: The relation direction to extract. Should be either `"all"`,
#'   `"ref_to"` or "ref_by".
#'
#' @section Object Query:
#'
#' \preformatted{
#' model$object(which)
#' model$objects(which)
#' model$object_unique(class)
#' model$objects_in_class(class)
#' model$objects_in_group(group)
#' model$objects_in_relation(which, direction = c("ref_to", "ref_by"), class = NULL, recursive = FALSE)
#' model$search_object(pattern, class = NULL, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
#' model$ClassName
#' model[[ClassName]]
#' }
#'
#' `$object()` returns an [IdfObject] specified by an object ID or name. Note
#' that unlike object ID, which is always unique across the whole `Idf` object,
#' sometimes different objects can have the same name. If the name given matches
#' multiple objects, an error is issued showing what objects are matched by the
#' same name. This behavior is consistent in all methods that take an object
#' name(s) as input.
#'
#' `$object_unique()` returns the [IdfObject] in unique-object class, e.g.
#' `SimulationControl` class. This makes it easy to directly extract and modify
#' those unique objects, e.g.
#' `model$object_unique("SimulationContrl")$set(...)`. Note that if there are
#' multiple objects in that unique-object class, an error is issued. This makes
#' sure that `$object_unique()` always returns a single [IdfObject].
#'
#' `$objects()` returns a named **list** of [IdfObject]s specified by object IDs
#' or names.
#'
#' `$objects_in_class()` returns a named **list** of all [IdfObject]s in
#' specified class.
#'
#' `$objects_in_group()` returns a named **list** of all [IdfObject]s in
#' specified group.
#'
#' `$objects_in_relation()` returns a named **list** of [IdfObject]s that have
#' specified relations with given object. The first element of returned list is
#' always the [IdfObject] of specified object. If that object does not have
#' specified relation with other objects in specified `class`, a list that only
#' contains that [IdfObject] is returned. For instance, assume that `const` is a
#' valid object name in `Construction` class,
#' `model$objects_in_relation("const", "ref_by", "BuildingSurface:Detailed")`
#' will return a named list of an [IdfObject] named `const` and also all other
#' [IdfObject]s in `BuildingSurface:Detailed` that refer to field values in
#' `const`. Similarly, `model$objects_in_relation("const", "ref_to", "Material")`
#' will return a named list of an [IdfObject] named `const` and also all other
#' [IdfObject]s in `Material` class that `const` refers to. This makes it easy
#' to directly extract groups of related objects and then use `$insert()` method
#' described below to insert them. For example, copy a construction named
#' `const` from an `Idf` object `model1` to another `Idf` object `model2` is
#' simply to do `model2$insert(model1$objects_in_relation("const", "ref_to"))`.
#'
#' There are lots of recursive references in a model. For instance, a material
#' can be referred by a construction, that construction can be referred by a
#' building surface, and that building surface can be referred by a window on
#' that surface. These objects related recursively can be extracted by setting
#' `recursive` to `TRUE`.
#'
#' `$search_object()` returns a named **list** of [IdfObject]s whose names meet
#' the given pattern in specified classes.
#'
#' eplusr also provides custom S3 method of `$` and \code{[[} to make it more
#' convenient to get [IdfObject]s in specified class. Basically, `model$ClassName` and
#' \code{model[[ClassName]]}, where `ClassName` is a single valid class name, is
#' equivalent to `model$objects_in_class(ClassName)` if `ClassName` is not an
#' unique-object class and `model$object_unique(ClassName)` if `ClassName` is an
#' unique-object class. For convenience, underscore-style names are allowed, e.g.
#' `BuildingSurface_Detailed` is equivalent to `BuildingSurface:Detailed`. For
#' instance, `model$BuildingSurface_Detailed` and also
#' `model[["BuildingSurface:Detailed"]]` will return all [IdfObject]s in
#' `BuildingSurface:Detailed` class; `model$Building` and also
#' `model[["Building"]]` will return the [IdfObject] in `Building` class which
#' is an unique-object class.
#'
#' **Note**: The returned list from `$objects()`, `$objects_in_class()` and
#' other methods is named using the names of returned [IdfObject]s in that list.
#' This will makes it easy to using object name to do further subsetting, e.g.
#' `model$objects_in_class("Material")$mat` will return an [IdfObject] named
#' `mat` in `Material` class, and `model$objects_in_class("Material")[[1]]` will
#' return the first material in `model`. If returned [IdfObject]s belongs to a
#' class that does not have name attribute, such like `Version`,
#' `SimulationControl` and etc., `NA` is assigned as the name.
#'
#' [IdfObject] class provides more detailed methods to modify a single object in
#' an `Idf`. For detailed explanations, please see [IdfObject] class.
#'
#' **Arguments**:
#'
#' * `which`: A single object ID or name for `$object()` and
#'   `$objects_in_relation()`; an integer vector of object IDs or a character
#'   vector of object names for `$objects()`.
#' * `class`: A single string of class name for `$object_unique()` and
#'   `$objects_in_class()`; a character vector of class names for
#'   `$objects_in_relation()` and `$search_object()`.
#' * `group`: A single string of group name.
#' * `pattern`, `ignore.case`, `perl`, `fixed` and `useBytes`: All of them are
#'   directly passed to [base::grepl()].
#' * `ClassName`: A single string of class name. For \code{[[}, `ClassName` can
#'   be an underscore-style class name, where all characters other than letters
#'   and numbers are replaced by underscores `_`.
#' * `direciton`: The relation direction to extract. Should be either `"ref_to"`
#'   or "ref_by".
#' * `recursive`: If `TRUE`, the relation is searched recursively, e.g. one
#'   material named `mat` is referred by a construction named `const`, and
#'   `const` is also referred by a surface named `surf`, all `mat`, `const` and
#'   `surf` are returned.
#' * `ignore.case`, `perl`, `fixed` and `useBytes`: All are directly passed to
#'   [base::grepl()].
#'
#' @section Object Modification:
#' \subsection{Duplicate Objects}{
#'
#' ```
#' model$dup(...)
#' ```
#'
#' `$dup()` takes integer vectors of object IDs and character vectors of object
#' names, duplicates objects specified, and returns a list of newly created
#' [IdfObject]s. The names of input are used as new names for created
#' [IdfObject]s. If input is not named, new names are the names of duplicated
#' objects with a suffix `"_1"`, `"_2"` and etc, depending on how many times
#' that object has been duplicated. Note an error will be issued if trying to
#' assign a new name to an object which does not have name attribute.
#'
#' **Note**:
#'
#' * Assign newly added objects with an existing name in current `Idf` object is
#'   prohibited if current validation level includes object name conflicting
#'   checking. For details, please see `level_checks()`.
#'
#' **Argument**:
#'
#' * `...`: Integer vectors of object IDs and character vectors of valid
#'   object names. If input has names, they will be used as the names of newly
#'   created objects.
#'
#' **Usage**:
#'
#' * Without new names: `model$dup(c("name1", "name2"), 6:10)`.
#' * With new names: `model$dup(c(new_name1 = "name1", new_name2 = "name2"), new_name3 = 6)`.
#' * Variable inputs: `a <- c("name1", new_name2 = "name2"); b <- 10:20; c <- c(new_name3 = 10); model$dup(a, b, c)`.
#' }
#'
#' \subsection{Add Objects}{
#'
#' ```
#' model$add(..., .default = TRUE, .all = FALSE)
#' ```
#'
#' `$add()` takes object definitions in list format, adds corresponding
#' objects in specified classes, returns a list of newly added [IdfObject]s.
#' Every list should be named with a valid class name. Underscore-style class
#' name is allowed. Names in list element are treated as field names. Values
#' without names will be inserted according to their position. There is a
#' special element named `.comment` in each list, which will be used as the
#' comments of newly added object.
#'
#' **Note**:
#'
#' * Empty objects can be added using an empty list, e.g. `model$add(building =
#'   list())`. All empty fields will be filled with corresponding default value
#'   if `.default` is `TRUE`, leaving other fields as blank. However, adding
#'   blank objects may not be successful if required fields are not valued and
#'   current validate level includes missing-required-field checking. For what
#'   kind of validation components to be performed during modifications, please
#'   see [level_checks()].
#' * Field name matching is **case-insensitive**. For convenience,
#'   underscore-style field names are also allowed, e.g. `eNd_MoNtH` is
#'   equivalent to `End Month`.
#' * There is no need to give all field values if only specific fields are
#'   interested, unless other fields are not required. For example, to define a
#'   new object in `RunPeriod` class, the following is enough:
#'   `model$add(RunPeriod = list(begin_month = 1, begin_day_of_month = 1,
#'   end_month = 1, end_day_of_month = 31), .default = TRUE)`.
#' * If not all field names are given, positions of those values without field
#'   names are determined after those values with names. E.g. in
#'   `model$add(Construction = list("out_layer", name = "name"))`, `"out_layer"`
#'   will be treated as the value of field `Outside Layer` in `Construction`
#'   class, as value of field `Name` has been given as `"name"`.
#'
#' **Arguments**:
#'
#' * `...`: Lists of object definitions. Each list should be named with a valid
#'   class name. There is a special element `.comment` in each list, which will
#'   be used as the comments of newly added object.
#' * `.default`: If `TRUE`, default values are used for those blank fields if
#'    possible. If `FALSE`, each required field in input object must have one
#'    value. Otherwise, an error will be issued during validation. Default:
#'    `TRUE`.
#' * `.all`: If `TRUE`, all fields are added, otherwise only minimum fields are
#'   added. Default: `FALSE`.
#'
#' **Usage**:
#'
#' * Empty object with default values: `model$add(Building = list(), .default = TRUE)`.
#' * Empty object with comments: `model$add(Building = list(.comment = c("This", "is", "a", "comment")))`.
#' * Empty object with all fields: `model$add(Building = list(), .all = TRUE)`.
#' * New objects: `model$add(RunPeriod = list("rp", 1, 1, end_month = 2, 1, "Monday"), list(Construction = list("const", "mat"), Material = list("mat")))`.
#' * New objects with comments: `model$add(RunPeriod = list("rp", 1, 1, 2, 1, .comment = "comment1"))`.
#' * Variable inputs: `x <- list(Construction = list("const"), Building = list()); model$add(x)`.
#' }
#'
#' \subsection{Set Values of Existing Objects}{
#'
#' ```
#' model$set(..., .default = TRUE)
#' ```
#'
#' `$set()` takes new field value definitions in list format, sets new
#' values for fields in objects specified, and returns a list of modified
#' [IdfObject]s. Every list in `$set()` should be named with a
#' valid object name. Object ID can also be used but have to be combined with
#' prevailing two periods `..`, e.g. `..10` indicates the object with ID `10`.
#' Similar to `$add()`, a special element `.comment` in each list will be used
#' as the **new** comments for modified object, overwriting the old ones. Names
#' in list element are treated as field names.
#'
#' **Note**:
#'
#' * You can delete a field by assigning `NULL` to it, e.g. `list(fld =
#'   NULL)` means to delete the value of field `fld`. If `.default` is FALSE,
#'   also `fld` is not a required field and the index of `fld` is larger than
#'   the number minimum fields required for that class, it will be deleted.
#'   Otherwise it will be left as blank. If `.default` is `TRUE`, that field
#'   will be filled with default value if applicable and left as blank if not.
#' * New fields that currently do not exist in that object can also be set. They
#'   will be automatically added on the fly.
#' * Field name matching is **case-insensitive**. For convenience,
#'   underscore-style field names are also allowed, e.g. `eNd_MoNtH` is
#'   equivalent to `End Month`.
#' * If not all field names are given, positions of those values without field
#'   names are determined after those values with names. E.g. in
#'   `model$set(Construction = list("out_layer", name = "name"))`, `"out_layer"`
#'   will be treated as the value of field `Outside Layer` in `Construction`, as
#'   value of field `Name` has been given as `"name"`.
#'
#' **Arguments**:
#'
#' * `...`: Lists of object definitions. Each list should be named with a valid
#'   object name or a valid object ID denoted in style `..1`, `..2` and etc.
#'   There is a special element `.comment` in each list, which will be used as
#'   new comments of modified object, overwriting existing ones.
#' * `.default`: If `TRUE`, default values are used for those blank fields if
#'    possible. Default: `TRUE`.
#'
#' **Usage**:
#'
#' * Specify object with name: `model$set(Object_Name = list(val1, val2, val3))`.
#' * Specify object with ID: `model$set(..8 = list(val1))`.
#' * Overwrite existing object comments: `model$set(..8 = list(.comment = c("new", "comment")))`.
#' * Delete field value: `model$set(Object_Name = list(Field_1 = NULL), .default = FALSE)`.
#' * Assign default field value: `model$set(Object_Name = list(Field_1 = NULL), .default = TRUE)`.
#' * Variable input: `a <- list(Object_Name = list(Field_1 = val1)); model$set(a, .default = TRUE)`.
#' * Set all values of field `fld` in a class `cls`:
#'
#' ```
#' ids <- model$object_id("cls", simplify = TRUE)
#' val <- rep(list(list(fld = val)), times = length(ids))
#' names(val) <- paste0("..", ids)
#' model$set(val)
#' ```
#' }
#'
#' \subsection{Deleting Existing Objects}{
#'
#' ```
#' model$del(..., .ref_by = FALSE, .ref_to = FALSE, .recursive = FALSE, .force = FALSE)
#' ```
#'
#' `$del()` takes integer vectors of object IDs and character vectors of object
#' names, and deletes objects specified. If `.ref_by` is `TRUE`, objects
#' whose fields refer to input objects will also be deleted. IF `.ref_to` is
#' `TRUE`, objects whose fields are referred by input objects will also be
#' deleted.
#'
#' **Note**:
#'
#' * If current [validate level][level_checks()] includes reference checking,
#'   objects will not be allowed to be deleted if they are referred by other
#'   objects. For example, an error will be issued if you want to delete one
#'   material that is referred by other constructions, because doing so will
#'   result in invalid field value references. You may bypass this if you really
#'   want to by setting `.force` to `TRUE`.
#' * When `.ref_by` or `.ref_to` is `TRUE`, objects are only deleted when they
#'   only have relation with input objects. For example, a construction `const`
#'   consist of 4 different materials. If `.ref_to` is `TRUE`, that 4 materials
#'   will only be deleted when they are only used in `const`, but not used in
#'   any other objects.
#' * There are recursively reference relations in `Idf` object. For example, one
#'   material's name is referenced by one construction, and that construction's
#'   name can be referred by another surface. You can delete all of them by
#'   setting `.recursive` to `TRUE`.
#'
#' **Arguments**:
#'
#' * `.ref_by`: If `TRUE`, objects whose fields refer to input objects will
#'     also be deleted. Default: `FALSE`.
#' * `.ref_to`: If `TRUE`, objects whose fields are referred by input objects
#'   will also be deleted. Default: `FALSE`.
#' * `.recursive`: If `TRUE`, relation searching is performed recursively, in
#'   case that objects whose fields refer to target object are also referred by
#'   another object, and also objects whose fields are referred by target object
#'   are also referred by another object. Default: `FALSE`.
#' * `.force`: If `TRUE`, objects are deleted even if they are referred by other
#'   objects.
#'
#' **Usage**:
#'
#' * Specify object with name: `model$del("Object_Name1", "Object_Name2")`.
#' * Specify object with ID: `model$del(1, 2, 10)`.
#' * Delete objects even they are referred by other objects: `model$del(1:5, .force = TRUE)`
#' * Delete objects and also other objects that refer to them: `model$del(2,
#' "Object_Name1", .ref_by = TRUE)`
#' * Delete objects and also other objects that refer to them recursively:
#' `model$del(1:5, .ref_by = TRUE, .recursive = TRUE)`
#' * Delete objects and also other objects that input objects refer to: `model$del(1:5, .ref_to = TRUE)`
#' * Variable input: `x <- c("Object_Name1", "Object_Name2"); y <- c(1:5); model$del(x, y)`.
#' }
#'
#' \subsection{Rename Objects}{
#'
#' ```
#' model$rename(...)
#' ```
#'
#' `$rename()` takes named character vectors of object names and named integer
#' vectors of object IDs, renames specified object to names of input vectors and
#' returns a list of renamed [IdfObject]s. An error will be issued if trying to
#' "rename" an object which does not have name attribute.
#'
#' **Argument**:
#'
#' * `...`: Integer vectors of valid object IDs and character vectors of valid
#'   object names. Each element should be named. That name is used at the new
#'   object name.
#'
#' **Usage**:
#'
#' * Rename objects: `model$rename(c(new_name1 = "name1", new_name2 = "name2"), new_name3 = 6)`.
#' * Variable inputs: `a <- c(new_name1 = "name1", new_name2 = "name2"); b <- c(new_name3 = 10); model$rename(a, b)`.
#' }
#'
#' \subsection{Insert Objects}{
#'
#' ```
#' model$insert(..., .unique = TRUE)
#' ```
#'
#' `$insert()` takes [IdfObject]s or lists of [IdfObject]s as input, inserts
#' them into current Idf, and returns a list of inserted [IdfObject]s.
#'
#' **Note**:
#'
#' * You cannot insert an [IdfObject] which comes from a different version than
#'   current `Idf` object.
#' * If input [IdfObject] has the same name as one [IdfObject] in current `Idf`
#'   object but field values are not equal, an error may be issued if current
#'   validation level includes conflicted-name checking. For what kind of
#'   validation components to be performed during modifications, please see
#'   [level_checks()].
#'
#' **Argument**:
#'
#' * `...`: [IdfObject]s or lists of [IdfObject]s.
#' * `.unique`: If there are duplications in input [IdfObject]s or there is same
#'   object in current `Idf` object, duplications in input are removed. Default:
#'   `TRUE`.
#'
#' **Usage**:
#'
#' * Insert objects without new names: `model1$insert(model2$Material)`.
#' * Insert an object without new name: `model1$insert(my_material = model2$Material[[1]])`.
#' * Insert objects but keep duplications: `model1$insert(model1$Output_Variable)`.
#' * Variable input: `mat <- model2$Material; names(mat) <- c("mat1", "mat2"); model1$insert(mat)`.
#' }
#'
#' \subsection{Load Objects from characters or data.frames}{
#'
#' ```
#' model$load(..., .unique = TRUE, .default = TRUE)
#' ```
#'
#' `$load()` is similar to `$insert()` except it takes directly character
#' vectors or data.frames of [IdfObject] definitions, insert corresponding
#' objects into current `Idf` object and returns a list of newly added
#' [IdfObject]s. This makes it easy to create objects using the output from
#' `$to_string()` and `$to_table` method from [Idd], [IddObject], also
#' Idf and [IdfObject] class.
#'
#' For object definitions in character vector format, they follow the same rules
#' as normal IDF file. Each object starts with a class name and a comma (`,`),
#' separates each values with a comma (`,`) and ends with a semicolon (`;`).
#' Noted that you can also provide headers to indicate if input objects are
#' presented in IP units, using `!-Option ViewInIPunits`. If this header does
#' not exist, then all values are treated as in SI units.
#'
#' For object definitions in data.frame format, it is highly recommended to use
#' `$to_table()` method in [Idd], [IddObject], `Idf` and [IdfObject] class. A
#' valid definition requires at least three columns described below. Note that
#' column order does not matter.
#'
#' * `class`: **Mandatory**. Character type. Valid class names in the underlying
#'   [Idd] object. You can get all valid class names using
#'   `use_idd(model$version())$class_name()`
#' * `index`: **Mandatory**. Integer type. Valid field indexes for each class.
#' * `value`: **Mandatory**. Character type or list type. The value of each field
#'   to be added.
#'   * If `value` is a character column, usually when `string_value` is `TRUE`
#'     in method `$to_table()` in `Idf` and [IdfObject] class. Each value should
#'     be given as a string even if the corresponding field is a numeric type.
#'   * If `value` is a list column, usually when `string_value` is set to
#'    `FALSE` in method `$to_table()` in `Idf` and [IdfObject] class. Each value
#'    should have the right type  as the corresponding field definition.
#'    Otherwise, errors will be issued during if current validation level
#'    includes invalid-type checking. For what kind of validation components to
#'    be performed during modifications, please see [level_checks()].
#' * `id`: **Optional**. Integer type. If input data.frame includes multiple
#'   object definitions in a same class, the value in `id` will be used to
#'   distinguish each definition. If `id` column does not exists, it assumes
#'   that each definition is separated by `class` column and will issue an error
#'   if there is any duplication in the `index` column.
#'
#' **Note**:
#'
#' * `$load()` assume all definitions are from the same version as current `Idf`
#'   object. If input definition is from different version, parsing error may
#'   occur.
#'
#' **Argument**:
#'
#' * `...`: Character vectors or data.frames of object definitions For details,
#'   see above.
#' * `.unique`: If there are duplications in input [IdfObject]s or there is same
#'   object in current Idf, duplications in input are removed. Default: `TRUE`.
#' * `.default`: If `TRUE`, default values are used for those blank fields if
#'    possible. Default: `TRUE`.
#'
#' **Usage**:
#'
#' * Load objects from string definitions:
#'
#'   ```
#'   model$load(c(
#'       "Material,",
#'       "    mat,                     !- Name",
#'       "    MediumSmooth,            !- Roughness",
#'       "    0.667,                   !- Thickness {m}",
#'       "    0.115,                   !- Conductivity {W/m-K}",
#'       "    513,                     !- Density {kg/m3}",
#'       "    1381;                    !- Specific Heat {J/kg-K}",
#'
#'       "Construction,
#'        const,
#'        mat;
#'       "
#'   ))
#'   ```
#'
#' * Load objects from data.frame definitions:
#'
#'   ```
#'   dt <- model1$to_table(class = "Material")
#'   dt[field == "thickness", value := "0.5"]
#'   model$load(dt)
#'   ```
#' }
#'
#' \subsection{Paste Objects from IDF Editor}{
#'
#' ```
#' model$paste(in_ip = FALSE, ver = NULL, unique = TRUE)
#' ```
#'
#' `$paste()` reads the contents (from clipboard) of copied objects from IDF
#' Editor (after hitting `Copy Obj` button), parses it and inserts corresponding
#' objects into current Idf. As IDF Editor only available on Windows platform,
#' `$paste()` only works on Windows too.
#'
#' **Note**:
#'
#' * There is no version data copied to the clipboard when copying objects in
#'   IDF Editor. It is possible that IDF Editor opens an IDF with different
#'   version than current IDF. Please check the version before running
#'   `$paste()`, or explicitly specify the version of file opened by IDF Editor
#'   using `ver` parameter. Parsing error may occur if there is a version
#'   mismatch.
#'
#' **Arguments**:
#'
#' * `in_ip`: Set to `TRUE` if the IDF file is open with `Inch-Pound`
#'   view option toggled. Numeric values will automatically converted to SI
#'   units if necessary. Default: `FALSE`.
#' * `ver`: The version of IDF file opened by IDF Editor, e.g. 8.6, "8.8.0". If
#'   `NULL`, assume that the file has the same version as current Idf object.
#'   Default: `NULL`.
#' * `unique`: If there are duplications in copied objects from IDF Editor or
#'   there is same object in current Idf, duplications in input are removed.
#'   Default: `TRUE`.
#'
#' **Usage**:
#'
#' * Paste objects from same version: `model$paste()`.
#' * Paste objects from different version: `model$paste(ver = "version")`.
#' * Paste objects that are viewed in IP units in IDF Editor: `model$paste(in_ip = TRUE)`.
#' * Paste objects but also keep duplications: `model$paste(unique = FALSE)`.
#' }
#'
#' \subsection{Search and Replace Values}{
#'
#' ```
#' model$search_value(pattern, class = NULL, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
#' model$replace_value(pattern, class = NULL, replacement, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
#' ```
#'
#' `$search_value()` returns a list of [IdfObject]s that contain values which
#' match the given pattern. If no matched found, `NULL` is returned invisibly.
#'
#' `$replace_value()` returns a list of [IdfObject]s whose values have been
#' replace with given pattern. If no matched found, `NULL` is returned
#' invisibly.
#'
#' **Note**:
#'
#' * During matching, all values are treated as characters, including numeric
#'   values.
#' * Replacing values using regular expression is not recommended, because it is
#'   error prone. Validation rules also apply during replacing.
#'
#' **Arguments**:
#'
#' * `class`: A character vector of invalid class names in current `Idf` object
#'   to search for values. If `NULL`, all classes are used.
#' * `pattern`, `replacement`, `ignore.case`, `perl`, `fixed` and `useBytes`:
#'   All of them are directly passed to [base::grepl()] and [base::gsub()].
#'
#' **Usage**:
#'
#' * Search values that contains `supply`: `model$search_value("supply")`
#' * Search values that contains `supply` or `demand` in class `Branch`: `model$search_value("supply|demand", "Branch")`
#' * Search values that contains `win` and replace them with `windows`: `model$replace_value("win", "windows")`
#' }
#'
#' @section Validation:
#'
#' ```
#' model$validate(level = eplusr_option("validate_level"))
#' model$is_valid(level = eplusr_option("validate_level"))
#' ```
#'
#' `$validate()` checks if there are errors in current `Idf` under specified
#' validation level and returns an `IdfValidity` object which contains data of
#' invalid field values. Different validation result examples are shown below:
#'
#' * No error is found:
#'
#'   ```
#'   v No error found.
#'   ```
#'
#'   Above result shows that there is no error found after conducting all
#'   validation checks in specified validation level.
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
#' Above validation results show that after all validation components performed
#' under current validation level, 2 invalid field values are found. All of them
#' are in object named `SPACE5-1 VAV Reheat` with ID `176`. They are invalid
#' because those two fields do not have an autocalculatable attribute but are
#' given `AUTOCALCULATE` value. Knowing this info, one simple way to fix the
#' error is to set those two fields to correct value by doing `idf$set(..176 =
#' list(`Maximum Flow per Zone Floor Area During Reheat` = "autosize",
#'      `Maximum Flow Fraction During Reheat` = "autosize"
#' ))`
#'
#' `$is_valid()` returns `TRUE` if there is no error in current `Idf` object
#' under specified validation level and `FALSE` otherwise.
#'
#' Underneath, an `IdfValidity` object which `$validate()` returns is a list of
#' 13 element as shown below. Each element or several elements represents the
#' results from a single validation checking component. In total, There are 10
#' different validation check components. To get the meaning of each component,
#' please see [level_checks()] and [custom_validate()].
#'
#' * `missing_object`
#' * `duplicate_object`
#' * `conflict_name`
#' * `incomplete_extensible`
#' * `missing_value`
#' * `invalid_autosize`
#' * `invalid_autocalculate`
#' * `invalid_character`
#' * `invalid_numeric`
#' * `invalid_integer`
#' * `invalid_choice`
#' * `invalid_range`
#' * `invalid_reference`
#'
#' Except `missing_object`, which is a character vector, all other elements
#' are [data.table][data.table::data.table()] with 9 columns containing data of
#' invalid field values:
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
#'  Knowing the internal structure of `IdfValidity`, it is easy to extract
#'  invalid [IdfObject]s you interested in. For example, you can get all IDs of
#'  objects that contain invalid value references using
#'  `model$validate()$invalid_reference$object_id`. Then using `$set()` method
#'  to correct them.
#'
#' @section Data Extraction:
#' ```
#' model$to_table(which = NULL, class = NULL, string_value = TRUE, unit = FALSE)
#' model$to_string(which = NULL, class = NULL, comment = TRUE, header = TRUE, format = eplusr_option("save_format"), leading = 4L, sep_at = 29L)
#' ```
#'
#' `$to_table()` returns a [data.table][data.table::data.table()] that contains
#' core data of specified objects. It has 6 columns:
#'
#' * `id`: Integer type. Object IDs.
#' * `name`: Character type. Object names.
#' * `class`: Character type. Current class name.
#' * `index`: Integer type. Field indexes.
#' * `field`: Character type. Field names.
#' * `value`: Character type if `string_value` is `TRUE` or list type if
#'   `string_value` is `FALSE`. Field values.
#'
#' `$to_string()` returns the text format of an IDF file.
#'
#' **Arguments**:
#'
#' * `which`: Either an integer vector of valid object IDs or a character vector
#'   of valid object names. If `NULL`, the whole `Idf` object is converted.
#'   Default: `NULL`.
#' * `class`: A character vector of class names. If `NULL`, all classed in
#'   current `Idf` object is converted. Default: `NULL`.
#' * `string_value`: If `TRUE`, all field values are returned as character. If
#'   `FALSE`, `value` column in returned [data.table][data.table::data.table()]
#'   is a list column with each value stored as corresponding type. Note that if
#'   the value of numeric field is set to `"Autosize"` or `"Autocalculate"`, it
#'   is left as it is, leaving the returned type being a string instead of a
#'   number.  Default: `TRUE`.
#' * `unit`: Only applicable when `string_value` is `FALSE`. If `TRUE`, values
#'   of numeric fields are assigned with units using [units::set_units()] if
#'   applicable. Default: `FALSE`.
#' * `comment`: If `FALSE`, all comments will not be included. Default: `TRUE`.
#' * `header`: If `FALSE`, the header will not be included. Default: `TRUE`.
#' * `format`: Specific format used when formatting. For details, please see
#'   `$save()`. Default: `eplusr_option("save_format")`
#' * `leading`: Leading spaces added to each field. Default: `4L`.
#' * `sep_at`: The character width to separate value string and field string.
#'   Default: `29L` which is the same as IDF Editor.
#'
#' @section Save:
#'
#' ```
#' model$is_unsaved()
#' model$save(path = NULL, format = eplusr_option("save_format"), overwrite = FALSE, copy_external = TRUE)
#' ```
#'
#' `$is_unsaved()` returns `TRUE` if there are modifications on the model since
#' it was read or since last time it was saved and `FALSE` otherwise.
#'
#' `$save()` saves the `Idf` object as an IDF file.
#'
#' **Arguments**:
#'
#' * `path`: A path where to save the model. If `NULL`, the path of the `Idf`
#'   itself, i.e. `model$path()`, will be used.
#' * `format`: A string to specify the saving format. Should be one of `"asis"`,
#'   `"sorted"`, `"new_top"`, and `"new_bot"`.
#'   * If `"asis"`, the model will be saved in the same format as it was when
#'   first read. If the model does not contain any format saving option, which
#'   is typically the case when the model was not saved using eplusr or
#'   IDFEditor, `"sorted"` will be used.
#'   * `"sorted"`, `"new_top"` and `"new_bot"` are the same as the save options
#'     `"Sorted"`, `"Original with New at Top"`, and `"Original with New at
#'     Bottom"` in IDFEditor. Default: `eplusr_option("save_format")`.
#'
#' * `overwrite`: Whether to overwrite the file if it already exists. Default:
#'    `FALSE`.
#' * `copy_external`: If `TRUE`, the external files that current Idf depends on
#'   will also be copied into the same directory. The values of file paths in
#'   the Idf will be changed into relative path automatically. This makes it
#'   possible to create fully reproducible simulation conditions. Currently,
#'   only `Schedule:File` class is supported. Default: `FALSE`.
#'
#' @section Clone:
#'
#' ```
#' model$clone(deep = TRUE)
#' ```
#'
#' `$clone()` returns the exactly the same cloned model. Because `Idf` uses
#' [R6::R6Class()] under the hook which has "modify-in-place" semantics, `idf_2
#' <- idf_1` does not copy `idf_1` at all but only create a new binding to
#' `idf_1`.  Modify `idf_1` will also affect `idf_2` as well, as these two are
#' exactly the same thing underneath. In order to create a complete cloned copy,
#' use `$clone(deep = TRUE)`.
#'
#' **Arguments**:
#'
#' * `deep`: Has to be `TRUE` if a complete cloned copy is desired. Default:
#'   `TRUE`.
#'
#' @section Run Model:
#'
#' ```
#' model$run(weather, dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE)
#' ```
#'
#' `$run()` calls corresponding version of EnergyPlus to run the current model
#' together with specified weather. The model and the weather used will be
#' copied into the output directory. An [EplusJob] object is returned which
#' provides detailed info of the simulation and methods to collect simulation
#' results. Please see [EplusJob] for details.
#'
#' **Note**:
#'
#' * eplusr uses the EnergyPlus command line interface which was introduced
#'   since EnergyPlus 8.3.0. So `$run()` only supports models with version no
#'   lower than 8.3.0.
#' * eplusr uses the EnergyPlus SQL output for extracting simulation results. In
#'   order to do so, an object in `Output:SQLite` class with `Option Type` value
#'   being `SimpleAndTabular` will be automatically created if it does not
#'   exists.
#'
#' **Arguments**:
#'
#' * `weather`: A path to an `.epw` file or an [Epw] object.
#' * `dir`: The directory to save the simulation results. If `NULL`, the model
#'    folder will be used. Default: NULL
#' * `wait`: Whether to wait until the simulation completes and print the
#'    standard output and error of EnergyPlus to the screen. If `FALSE`, the
#'    simulation will run in the background. Default is `TRUE`.
#' * `force`: Only applicable when the last simulation runs with `wait` equals
#'   to `FALSE` and is still running. If `TRUE`, current running job is
#'   forced to stop and a new one will start. Default: `FALSE`.
#' * `copy_external`: If `TRUE`, the external files that current `Idf` object
#'   depends on will also be copied into the simulation output directory. The
#'   values of file paths in the Idf will be changed automatically. Currently,
#'   only `Schedule:File` class is supported.  This ensures that the output
#'   directory will have all files needed for the model to run. Default is
#'   `FALSE`.
#'
#' @section Print:
#' ```
#' model$print(zoom = c("object", "class", "group", "field"), order = TRUE)
#' print(model)
#' ```
#'
#' `$print()` prints the `Idf` object according to different detail level
#' specified using the `zoom` argument.
#'
#' With the default `zoom` level `object`, contents of the `Idf` object is
#' printed in a similar style as you see in IDF Editor, with additional heading
#' lines showing `Path`, `Version` of the `Idf` object. Class names of objects
#' are ordered by group and the number of objects in classes are shown in square
#' bracket.
#'
#' **Arguments**:
#'
#' * `zoom`: Control how detailed of the Idf object should be printed. Should be
#'   one of `"group"`, `"class"`, `"object"` and `"field"`. Default: `"group"`.
#'
#'   * `"group"`: all group names current existing are shown with prevailing
#'     square bracket showing how many **<C>**lasses existing in that group.
#'   * `"class"`: all class names are shown with prevailing square bracket
#'     showing how many **<O>**bjects existing in that class, together with
#'     parent group name of each class.
#'   * `"object"`: all object IDs and names are shown, together with parent
#'     class name of each object.
#'   * `"field"`: all object IDs and names, field names and values are shown,
#'     together with parent class name of each object.
#'
#' * `order`: Only applicable when `zoom` is `"object"` or `"field"`. If `TRUE`,
#'   objects are shown as the same order in the IDF. If `FALSE`, objects are
#'   grouped and ordered by classes. Default: `TRUE`.
#'
#' @examples
#' # ===== CREATE =====
#' # read an IDF file
#' idf <- read_idf(system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr"),
#'     idd = use_idd(8.8, download = "auto"))
#'
#' # ===== MODEL BASIC INFO =====
#' # get version
#' idf$version()
#'
#' # get path
#' idf$path()
#'
#' # get names of all groups in current model
#' str(idf$group_name())
#'
#' # get names of all defined groups in the IDD
#' str(idf$group_name(all = TRUE))
#'
#' # get names of all classes in current model
#' str(idf$class_name())
#'
#' # get names of all defined classes in the IDD
#' str(idf$class_name(all = TRUE))
#'
#' # check if input is a valid group name in current model
#' idf$is_valid_group("Schedules")
#' idf$is_valid_group("Compliance Objects")
#'
#' # check if input is a valid group name in IDD
#' idf$is_valid_group("Compliance Objects", all = TRUE)
#'
#' # check if input is a valid class name in current model
#' idf$is_valid_class("Building")
#' idf$is_valid_class("ShadowCalculation")
#'
#' # check if input is a valid class name in IDD
#' idf$is_valid_class("ShadowCalculation", all = TRUE)
#'
#' # ===== OBJECT DEFINITION (IDDOBJECT) =====
#' # get the a list of underlying IddObjects
#' idf$definition("Version")
#'
#' # ===== OBJECT INFO =====
#' # get IDs of objects in classes
#' idf$object_id(c("Version", "Zone"))
#'
#' # when `simplify` is TRUE, an integer vector will be returned instead of a
#' # named list
#' idf$object_id(c("Version", "Zone"), simplify = TRUE)
#'
#' # get names of objects in classes
#' # NA will be returned if targeted class does not have a name attribute
#' idf$object_name(c("Building", "Zone", "Version"))
#'
#' # if `simplify` is TRUE, a character vector will be returned instead of a
#' # named list
#' idf$object_name(c("Building", "Zone", "Version"), simplify = TRUE)
#'
#' # get number of objects in classes
#' idf$object_num(c("Zone", "Schedule:Compact"))
#'
#' # check if input is a valid object ID, i.e. there is an object whose ID is
#' # the same with input integer
#' idf$is_valid_id(c(51, 1000))
#'
#' # check if input is a valid object name, i.e., there is an object whose name is
#' # the same with input string
#' idf$is_valid_name(c("Simple One Zone (Wireframe DXF)", "ZONE ONE"))
#'
#' # ===== OBJECT QUERY =====
#' # get single object using object ID or name
#' # NOTE: object name matching is case-insensitive
#' idf$object(3)
#' idf$object("simple one zone (wireframe dxf)")
#'
#' # get objects using object IDs or names
#' # NOTE: object name matching is case-insensitive
#' idf$objects(c(3,10))
#' idf$objects(c("Simple One Zone (Wireframe DXF)", "zone one"))
#'
#' # the names of returned list are object names
#' names(idf$objects(c("Simple One Zone (Wireframe DXF)", "zone one")))
#'
#' # get all objects in classes in a named list
#' idf$objects_in_class("Zone")
#' names(idf$objects_in_class("Zone"))
#'
#' # OR using shortcuts
#' idf$Zone
#' idf[["Zone"]]
#'
#' # get a single object in unique-object class
#' idf$object_unique("SimulationControl")
#' idf$SimulationControl
#' idf[["SimulationControl"]]
#'
#' # search objects using regular expression
#' length(idf$search_object("R13"))
#' names(idf$search_object("R13"))
#'
#' # search objects using regular expression in specifc class
#' length(idf$search_object("R13", class = "Construction"))
#' names(idf$search_object("R13", class = "Construction"))
#'
#' # get more controls on matching
#' length(idf$search_object("r\\d", ignore.case = TRUE, class = "Construction"))
#' names(idf$search_object("r\\d", ignore.case = TRUE, class = "Construction"))
#'
#' # ===== DUPLICATE OBJECTS =====
#' \dontrun{
#' # duplicate objects in "Construction" class
#' names(idf$Construction)
#' idf$dup("R13WALL")
#'
#' # new objects will have the same names as the duplicated objects but with a
#' # suffix "_1", "_2" and etc.
#' names(idf$Construction)
#'
#' # new names can also be explicitly specified
#' idf$dup(My_R13Wall = "R13WALL")
#'
#' # duplicate an object multiple times
#' idf$dup(rep("R13WALL", times = 10))
#' }
#'
#' # ===== ADD OBJECTS =====
#' \dontrun{
#' # add two new objects in "RunPeriod" class
#' idf$add(
#'     RunPeriod = list("rp_test_1", 1, 1, 2, 1,
#'         .comment = c("Comment for new object 1", "Another comment")
#'     ),
#'
#'     RunPeriod = list(name = "rp_test_2",
#'         begin_month = 3,
#'         begin_day_of_month = 1,
#'         end_month = 4,
#'         end_day_of_month = 1,
#'         .comment = "Comment for new object 2"
#'      )
#' )
#' }
#'
#' # ===== INSERT OBJECTS =====
#' \dontrun{
#' # insert objects from other Idf object
#' idf_1 <- read_idf(system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr"),
#'     idd = use_idd(8.8, download = "auto"))
#'
#' idf_1$object_name("Material")
#'
#' # rename material name from "C5 - 4 IN HW CONCRETE" to "test", otherwise
#' # insertion will be aborted as there will be two materials with the same name
#' # in the idf
#' idf_1$Material$`C5 - 4 IN HW CONCRETE`$set(name = "test")
#'
#' # insert the object
#' idf$insert(idf_1$Material$test)
#'
#' # check if material named "test" is there
#' idf$object_name("Material")
#'
#' # $insert() is useful when importing design days from a ".ddy" file
#' idf$insert(read_idf("foo.ddy"))
#' }
#'
#' # ===== SET OBJECTS =====
#' \dontrun{
#' # set the thickness of newly inserted material "test" to 0.2 m
#' idf$set(test = list(thickness = 0.2))
#' idf$Material$test$Thickness
#'
#' # set thermal absorptance of all material to 0.85
#' val <- rep(list(list(thermal_absorptance = 0.85)), idf$object_num("Material"))
#' names(val) <- idf$object_name("Material", simplify = TRUE)
#' idf$set(val)
#'
#' # check results
#' lapply(idf$Material, function (mat) mat$Thermal_Absorptance)
#'
#' # reset thermal absorptance of all material to the default
#' val <- rep(list(list(thermal_absorptance = NULL)), idf$object_num("Material"))
#' names(val) <- idf$object_name("Material", simplify = TRUE)
#' idf$set(val)
#'
#' # check results
#' lapply(idf$Material, function (mat) mat$Thermal_Absorptance)
#' }
#'
#' # ===== RENAME OBJECTS =====
#' \dontrun{
#' idf$rename(new_test = "test")
#' idf$object_name("Material")
#' }
#'
#' # ===== DELELTE OBJECTS =====
#' \dontrun{
#' # delete the added run period "rp_test_1", "rp_test_2" and "new_test" from above
#' idf$del(c("new_test", "rp_test_1", "rp_test_2"))
#' names(idf$Material)
#' names(idf$RunPeriod)
#'
#' # In "final" validate level, delete will be aborted if the target objects are
#' # referenced by other objects.
#' # get objects that referenced material "R13LAYER"
#' eplusr_option("validate_level")
#'
#' idf$Material_NoMass$R13LAYER$ref_by_object()
#' length(idf$Material_NoMass$R13LAYER$ref_by_object())
#'
#' idf$del("R13LAYER") # will give an error in "final" validate level
#'
#' # objects referencing target objects can also be deleted by setting
#' # `referenced` to TRUE
#' idf$del("R13LAYER", .ref_by = TRUE) # will give an error in "final" validate level
#'
#' # it is possible to force delete objects
#' idf$del("R13LAYER", .ref_by = TRUE, .force = TRUE)
#' }
#'
#' # ===== SEARCH ADN REPLACE OBJECT VALUES =====
#' # get objects whose field values contains both "VAV" and "Node"
#' idf$search_value("WALL")
#' length(idf$search_value("WALL"))
#' names(idf$search_value("WALL"))
#'
#' # replace values using regular expression
#' \dontrun{idf$replace_value("WALL", "A_WALL")}
#'
#' # ===== VALIDATE MODEL =====
#' \dontrun{
#' # check if there are errors in current model
#' idf$validate()
#' idf$is_valid()
#'
#' # change validate level to "none", which will enable invalid modifications
#' eplusr_option(validate_level = "none")
#'
#' # change the outside layer of floor to an invalid material
#' idf$set(FLOOR = list(outside_layer = "wrong_layer"))
#'
#' # change validate level back to "final" and validate the model again
#' eplusr_option(validate_level = "final")
#'
#' idf$validate()
#' idf$is_valid()
#'
#' # get IDs of all objects that contains invalid reference fields
#' idf$validate()$invalid_reference$object_id
#'
#' # fix the error
#' idf$set(..16 = list(outside_layer = idf$Material[[1]]$name()))
#' idf$validate()
#' idf$is_valid()
#' }
#' # ===== FORMAT MODEL =====
#' # get text format of the model
#' head(idf$to_string())
#'
#' # get text format of the model, excluding the header and all comments
#' head(idf$to_string(comment = FALSE, header = FALSE))
#'
#' # ===== SAVE MODEL =====
#' # check if the model has been modified since read or last saved
#' idf$is_unsaved()
#'
#' \dontrun{
#' # save and overwrite current model
#' idf$save(overwrite = TRUE)
#'
#' # save the model with newly created and modified objects at the top
#' idf$save(overwrite = TRUE, format = "new_top")
#'
#' # save the model to a new file
#' idf$save(path = file.path(tempdir(), "test.idf"))
#'
#' # save the model to a new file and copy all external csv files used in
#' # "Schedule:File" class into the same folder
#' idf$save(path = file.path(tempdir(), "test1.idf"), copy_external = TRUE)
#'
#' # the path of this model will be changed to the saved path
#' idf$path()
#' }
#'
#' # ===== CLONE MODEL =====
#' \dontrun{
#' # Idf object are modified in place and has reference semantic.
#' idf_2 <- idf
#' idf_2$object_name("Building")
#' idf$object_name("Building")
#'
#' # modify idf_2 will also affect idf as well
#' idf_2$Building$set(name = "Building_Name_Changed")
#' idf_2$object_name("Building")
#' idf$object_name("Building")
#'
#' # in order to make a copy of an Idf object, use $clone() method
#' idf_3 <- idf$clone(deep = TRUE)
#' idf_3$Building$set(name = "Building_Name_Changed_Again")
#' idf_3$object_name("Building")
#'
#' idf$object_name("Building")
#' }
#'
#' # ===== RUN MODEL =====
#' \dontrun{
#' if (is_avail_eplus(8.8)) {
#'
#'     # save the model to tempdir()
#'     idf$save(file.path(tempdir(), "test_run.idf"))
#'
#'     # use the first epw file in "WeatherData" folder in EnergyPlus v8.8
#'     # installation path
#'     epw <- list.files(file.path(eplus_config(8.8)$dir, "WeatherData"),
#'         pattern = "\\.epw$", full.names = TRUE)[1]
#'     basename(epw)
#'     # [1] "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'
#'     # if `dir` is NULL, the directory of IDF file will be used as simulation
#'     # output directory
#'     job <- idf$run(epw, dir = NULL)
#'
#'     # run simulation in the background
#'     idf$run(epw, dir = tempdir(), wait = FALSE)
#'
#'     # copy all external files into the directory run simulation
#'     idf$run(epw, dir = tempdir(), copy_external = TRUE)
#'
#'     # check for simulation errors
#'     job$errors()
#'
#'     # get simulation status
#'     job$status()
#'
#'     # get output directory
#'     job$output_dir()
#'
#'     # re-run the simulation
#'     job$run()
#'
#'     # get simulation results
#'     job$report_data()
#' }
#' }
#' # ===== PRINT MODEL =====
#' \dontrun{
#' idf$print("group")
#' idf$print("class")
#' idf$print("object")
#' idf$print("field")
#' }
#' @docType class
#' @name Idf
#' @seealso [IdfObject] class
#' @author Hongyuan Jia
NULL

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
        version = function ()
            idf_version(self, private),

        path = function ()
            idf_path(self, private),

        group_name = function (all = FALSE, sorted = TRUE)
            idf_group_name(self, private, all, sorted),

        class_name = function (all = FALSE, sorted = TRUE)
            idf_class_name(self, private, all, sorted),

        is_valid_group = function (group, all = FALSE)
            idf_is_valid_group_name(self, private, group, all),

        is_valid_class = function (class, all = FALSE)
            idf_is_valid_class_name(self, private, class, all),

        definition = function (class)
            idf_definition(self, private, class),

        object_id = function (class = NULL, simplify = FALSE)
            idf_object_id(self, private, class, simplify),

        object_name = function (class = NULL, simplify = FALSE)
            idf_object_name(self, private, class, simplify),

        is_valid_id = function (id)
            idf_is_valid_object_id(self, private, id),

        is_valid_name = function (name)
            idf_is_valid_object_name(self, private, name),

        object_num = function (class = NULL)
            idf_object_num(self, private, class),

        object = function (which)
            idf_obj(self, private, which),

        object_unique = function (class)
            idf_object_unique(self, private, class),

        objects = function (which)
            idf_objects(self, private, which),

        objects_in_class = function (class)
            idf_objects_in_class(self, private, class),

        objects_in_group = function (group)
            idf_objects_in_group(self, private, group),

        object_relation = function (which, direction = c("all", "ref_to", "ref_by"))
            idf_object_relation(self, private, which, match.arg(direction)),

        objects_in_relation = function (which, direction = c("ref_to", "ref_by"), class = NULL, recursive = FALSE)
            idf_objects_in_relation(self, private, which, match.arg(direction), class, recursive = recursive),

        object_in_class = function (class)
            idf_object_in_class(self, private, class),

        search_object = function (pattern, class = NULL, ignore.case = FALSE,
                                  perl = FALSE, fixed = FALSE, useBytes = FALSE)
            idf_search_object(self, private, pattern, class, ignore.case = ignore.case,
                perl = perl, fixed = fixed, useBytes = useBytes
            ),

        dup = function (...)
            idf_dup(self, private, ...),

        add = function (..., .default = TRUE, .all = FALSE)
            idf_add(self, private, ..., .default = .default, .all = .all),

        set = function (..., .default = TRUE)
            idf_set(self, private, ..., .default = .default),

        del = function (..., .ref_by = FALSE, .ref_to = FALSE, .recursive = FALSE, .force = FALSE)
            idf_del(self, private, ..., .ref_by = .ref_by, .ref_to = FALSE, .recursive = .recursive, .force = .force),

        rename = function (...)
            idf_rename(self, private, ...),

        insert = function (..., .unique = TRUE)
            idf_insert(self, private, ..., .unique = .unique),

        load = function (..., .unique = TRUE, .default = TRUE)
            idf_load(self, private, ..., .unique = .unique, .default = .default),

        paste = function (in_ip = FALSE, ver = NULL, unique = TRUE)
            idf_paste(self, private, in_ip = in_ip, ver = ver, unique = unique),

        dup_object = function (object, new_name = NULL)
            idf_dup_object(self, private, object, new_name),

        add_object = function (class, value = NULL, comment = NULL, default = TRUE, all = FALSE)
            idf_add_object(self, private, class, value, comment, default, all),

        set_object = function (object, value = NULL, comment = NULL, default = FALSE)
            idf_set_object(self, private, object, value, comment, default),

        ins_object = function (object)
            idf_ins_object(self, private, object),

        del_object = function (object, referenced = FALSE)
            idf_del_object(self, private, object, referenced),

        search_value = function (pattern, class = NULL, ignore.case = FALSE,
                                 perl = FALSE, fixed = FALSE, useBytes = FALSE)
            idf_search_value(self, private, pattern, class, ignore.case = ignore.case,
                perl = perl, fixed = fixed, useBytes = useBytes
            ),

        replace_value = function (pattern, replacement, class = NULL, ignore.case = FALSE,
                                  perl = FALSE, fixed = FALSE, useBytes = FALSE)
            idf_replace_value(self, private, pattern, replacement, class = class,
                ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes
            ),

        validate = function (level = eplusr_option("validate_level"))
            idf_validate(self, private, level),

        is_valid = function (level = eplusr_option("validate_level"))
            idf_is_valid(self, private, level),

        to_string = function (which = NULL, class = NULL, comment = TRUE,
                              header = TRUE, format = eplusr_option("save_format"),
                              leading = 4L, sep_at = 29L)
            idf_to_string(self, private, which, class, comment = comment,
                          header = header, format = format,
                          leading = leading, sep_at = sep_at),

        string = function (comment = TRUE, header = TRUE, format = eplusr_option("save_format"),
                           leading = 4L, sep_at = 29L)
            idf_string(self, private, comment = comment, header = header,
                       format = format, leading = leading, sep_at = sep_at),

        to_table = function (which = NULL, class = NULL, string_value = TRUE, unit = FALSE)
            idf_to_table(self, private, which = which, class = class, string_value = string_value, unit = unit),

        is_unsaved = function ()
            idf_is_unsaved(self, private),

        save = function (path = NULL, format = eplusr_option("save_format"), overwrite = FALSE, copy_external = TRUE)
            idf_save(self, private, path, format = format, overwrite = overwrite, copy_external = copy_external),

        run = function (weather = NULL, dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE)
            idf_run(self, private, weather, dir, wait, force, copy_external = copy_external),

        print = function (zoom = "class", order = TRUE)
            idf_print(self, private, zoom, order)
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

        deep_clone = function (name, value) {
            idf_deep_clone(self, private, name, value)
        }
    )
)

# set deep default value to `TRUE`
formals(Idf$clone_method)$deep <- TRUE
formals(Idf$public_methods$clone)$deep <- TRUE
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
idf_class_name <- function (self, private, all = FALSE, sorted = TRUE) {
    if (all) {
        private$idd_env()$class$class_name
    } else {
        add_class_name(private$idd_env(), private$idf_env()$object)
        on.exit(set(private$idf_env()$object, NULL, "class_name", NULL), add = TRUE)
        if (sorted) {
            private$idf_env()$object[order(class_id), unique(class_name)]
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

    IdfObject$new(obj$object_id, obj$class_id, parent = self)
}
# }}}
# idf_object_unique {{{
idf_object_unique <- function (self, private, class) {
    assert(is_scalar(class))
    obj <- get_idf_object(private$idd_env(), private$idf_env(), class)

    if (!obj$class_id %in% private$idd_env()$class[unique_object == TRUE, class_id]) {
        abort("error_idf_not_unique_class",
            paste0(surround(unique(obj$class_name)), " is not a valid unique-object class index or name.")
        )
    }

    if (nrow(obj) > 1L) {
        abort("error_idf_dup_unique_class",
            paste0(surround(unique(obj$class_name)), " class have more than one ",
                "objects: ", get_object_info(obj, c("id", "name"), collapse = "\n"),
                ". Please see `$validate()` for more details."
            )
        )
    }

    IdfObject$new(obj$object_id, obj$class_id, parent = self)
}
# }}}
# idf_objects {{{
idf_objects <- function (self, private, which) {
    assert(!is.null(which))
    obj <- get_idf_object(private$idd_env(), private$idf_env(), class = NULL, object = which,
        ignore_case = TRUE
    )

    res <- apply2(obj$object_id, obj$class_id, IdfObject$new, list(parent = self))
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
    setattr(res, "names", obj$object_name)
    res
}
# }}}
# idf_object_relation {{{
idf_object_relation <- function (self, private, which, direction = c("all", "ref_to", "ref_by")) {
    assert(is_scalar(which))
    direction <- match.arg(direction)

    obj <- get_idf_object(private$idd_env(), private$idf_env(),
        object = which, ignore_case = TRUE
    )

    get_idfobj_relation(private$idd_env(), private$idf_env(),
        object_id = obj$object_id, name = TRUE, direction = direction,
        keep_all = FALSE, by_value = FALSE, max_depth = NULL, recursive = FALSE
    )
}
# }}}
# idf_objects_in_relation {{{
idf_objects_in_relation <- function (self, private, which, direction = c("ref_to", "ref_by"),
                                     class = NULL, recursive = FALSE) {
    assert(is_scalar(which))
    direction <- match.arg(direction)

    obj <- get_idf_object(private$idd_env(), private$idf_env(), object = which, ignore_case = TRUE)
    rel <- get_idfobj_relation(private$idd_env(), private$idf_env(), obj$object_id,
        name = FALSE, max_depth = NULL, direction = direction, recursive = recursive
    )

    # only include specified class
    if (!is.null(class)) {
        cls <- get_idd_class(private$idd_env(), class)

        if (direction == "ref_to") {
            add_joined_cols(private$idf_env()$object, rel$ref_to, c(src_object_id = "object_id"), c(src_class_id = "class_id"))
            rel$ref_to <- rel$ref_to[J(cls$class_id), on = "src_class_id"]
        } else {
            add_joined_cols(private$idf_env()$object, rel$ref_by, "object_id", "class_id")
            rel$ref_by <- rel$ref_by[J(cls$class_id), on = "class_id"]
        }
    }

    id_self <- obj$object_id
    if (direction == "ref_to") {
        id_ref <- rel$ref_to$src_object_id[!is.na(rel$ref_to$src_object_id)]
    } else {
        id_ref <- rel$ref_by$object_id[!is.na(rel$ref_by$object_id)]
    }

    obj_self <- list(IdfObject$new(id_self, obj$class_id, self))
    setattr(obj_self, "names", obj$object_name)

    if (!length(id_ref)) {
        dir <- switch(direction, ref_to = "does not refer to", ref_by = "is not referred by")
        msg <- paste0(get_object_info(obj, numbered = FALSE), " ", dir, " any other object")
        if (is.null(class)) {
            message(paste0(msg, "."))
        } else {
            message(paste0(msg, " in class ", collapse(cls$class_name), "."))
        }
        return(obj_self)
    }

    res <- c(obj_self, lapply(id_ref, IdfObject$new, parent = self))

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
    setattr(res, "names", obj$object_name)
    res
}
# }}}
# idf_return_modified {{{
idf_return_modified <- function (self, private, modified) {
    res <- apply2(modified$object$object_id, modified$object$class_id, IdfObject$new, list(parent = self))
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
idf_add <- function (self, private, ..., .default = TRUE, .all = FALSE) {
    add <- add_idf_object(private$idd_env(), private$idf_env(), ..., .default = .default, .all = .all)
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
idf_set <- function (self, private, ..., .default = TRUE) {
    set <- set_idf_object(private$idd_env(), private$idf_env(), ..., .default = .default)
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
idf_insert <- function (self, private, ..., .unique = TRUE) {
    ins <- insert_idf_object(private$idd_env(), private$idf_env(), private$m_version, ..., .unique = .unique)

    if (!nrow(ins$object)) {
        message("After deleting duplications, nothing to add.")
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
idf_paste <- function (self, private, in_ip = FALSE, ver = NULL, unique = TRUE) {
    pas <- paste_idf_object(private$idd_env(), private$idf_env(),
        version = private$m_version, in_ip = in_ip, unique = unique
    )

    if (!nrow(pas$object)) {
        message("After deleting duplications, nothing to add.")
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
idf_load <- function (self, private, ..., .unique = TRUE, .default = TRUE) {
    l <- load_idf_object(private$idd_env(), private$idf_env(), private$m_version,
        ..., .unique = .unique, .default = .default
    )

    if (!nrow(l$object)) {
        message("After deleting duplications, nothing to add.")
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
idf_to_table <- function (self, private, which = NULL, class = NULL, string_value = TRUE, unit = FALSE) {
    get_idf_table(private$idd_env(), private$idf_env(), class, which, string_value, unit)
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

    # log saved
    log_saved(private$m_log)

    # change path
    private$m_path <- normalizePath(path)
    invisible(path)
}
# }}}
# idf_run {{{
idf_run <- function (self, private, epw, dir = NULL, wait = TRUE,
                     force = FALSE, copy_external = FALSE) {
    if (private$m_version < 8.3) {
        abort("error_eplus_lower_8.3",
            "Currently, `$run()` only supports EnergyPlus V8.3 or higher."
        )
    }

    # stop if unsaved
    if (self$is_unsaved())
        abort("error_idf_not_saved",
            paste0("Idf has been modified since read or last saved. ",
                "Please save Idf using $save() before run."
            )
        )

    # add Output:SQLite if necessary
    add_sql <- idf_add_output_sqlite(self)

    # save the model to the output dir if necessary
    if (is.null(private$m_path) || !utils::file_test("-f", private$m_path)) {
        stop("The Idf object is not created from local file or local file has ",
            "been deleted from disk. Please save Idf using $save() before run.", call. = FALSE)
    }

    path_idf <- private$m_path
    if (is.null(dir))
        run_dir <- dirname(path_idf)
    else {
        run_dir <- dir
        path_idf <- normalizePath(file.path(run_dir, basename(path_idf)), mustWork = FALSE)
    }

    # if necessary, resave the model
    if (add_sql || !is.null(dir)) {
        idf_save(self, private, path_idf, overwrite = TRUE, copy_external = copy_external)
    }

    # check if the model is still running
    old <- private$m_log$job
    if (!inherits(old, "EplusJob")) {
        private$m_log$job <- EplusJob$new(path_idf, epw, private$m_version)
    }

    private$m_log$job$run(wait = wait, force = force)
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
        list2env(as.list.environment(value))
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
            invisible(sql$set("SimpleAndTabular"))
            verbose_info("Setting `Option Type` in ",
                "`Output:SQLite` to from", surround(type), " to `SimpleAndTabular`.")
            added <- TRUE
        }
    } else {
        invisible(idf$add(Output_SQLite = list("SimpleAndTabular")))
        verbose_info("Adding object `Output:SQLite` and setting ",
            "`Option Type` to `SimpleAndTabular` in order to create SQLite output file.")
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
#' @details
#' Currently, Imf file is not fully supported. All EpMacro lines will be treated
#' as normal comments of the nearest downwards object. If input is an Imf file,
#' a warning will be given during parsing. It is recommended to convert the Imf
#' file to an Idf file and use [ParametricJob] class to conduct
#' parametric analysis.
#'
#' @return An [Idf] object.
#' @examples
#' # example model shipped with eplusr from EnergyPlus v8.8
#' idf_path <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr") # v8.8
#'
#' # if neither EnergyPlus v8.8 nor Idd v8.8 was found, error will occur
#' is_avail_eplus(8.8)
#'
#' is_avail_idd(8.8)
#'
#' \dontrun{(read_idf(idf_path))}
#'
#' # if EnergyPlus v8.8 is found but Idd v8.8 was not, `Energy+.idd` in EnergyPlus
#' # installation folder will be used for pasing
#' is_avail_eplus(8.8)
#' is_avail_idd(8.8)
#'
#' \dontrun{read_idf(idf_path)}
#'
#' # if Idd v8.8 is found, it will be used automatically
#' is_avail_idd(8.8)
#'
#' \dontrun{read_idf(idf_path)}
#'
#' # argument `idd` can be specified explicitly using `use_idd()`
#' \dontrun{read_idf(idf_path, idd = use_idd(8.8))}
#'
#' # you can set `download` arugment to "auto" in `use_idd()` if you want to
#' # automatically download corresponding IDD file when necessary
#' \dontrun{read_idf(idf_path, use_idd(8.8, download = "auto"))}
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
#' \dontrun{read_idf(idf_string, use_idd(8.8, download = "auto"))}
#' @seealso [Idf] class for modifying EnergyPlus model. [use_idd()] and
#' [download_idd()] for downloading and parsing EnergyPlus IDD file.
#' [use_eplus()] for configuring which version of EnergyPlus to use.
#' @export
#' @author Hongyuan Jia
# read_idf {{{
read_idf <- function (path, idd = NULL) {
    Idf$new(path, idd)
}
# }}}

#' @export
# $.Idf {{{
`$.Idf` <- function (x, i) {
    if (all(i %in% setdiff(ls(x), "initialize"))) {
        NextMethod()
    } else {
        assert(is_scalar(i))

        priv <- ._get_private(x)

        obj <- tryCatch(get_idf_object(priv$idd_env(), priv$idf_env(), i, underscore = TRUE),
            error = function (e) NULL
        )

        if (is.null(obj)) return(NULL)

        uni <- FALSE
        if (obj$class_id[[1L]] %in% priv$idd_env()$class[unique_object == TRUE, class_id]) {
            if (nrow(obj) == 1L) {
                uni <- TRUE
            } else {
                warn("warning_idf_dup_unique_class",
                    paste0("Unique object class ", surround(unique(obj$class_name)),
                        " has more than one objects: ",
                        get_object_info(obj, c("id", "name"), collapse = "\n"),
                        "\nAll objects will be returned. Please see `$validate()`",
                        " for more details."
                    )
                )
            }
        }

        if (uni) {
            IdfObject$new(obj$object_id, obj$class_id, parent = x)
        } else {
            out <- apply2(obj$object_id, obj$class_id, IdfObject$new, list(parent = x))
            setattr(out, "names", obj$object_name)
            out
        }
    }
}
# }}}

#' @export
# $<-.Idf {{{
`$<-.Idf` <- function (x, name, value) {
    idd_env <- .subset2(._get_private(x), "idd_env")()
    idf_env <- .subset2(._get_private(x), "idf_env")()

    cls_id <- chmatch(name, idd_env$class$class_name)

    if (is.na(cls_id)) abort_bad_key("error_invalid_class_name", "class name", name)

    # input should be a list of IdfObjects
    assert(is.list(value), vlapply(value, is_idfobject), msg = "Value should be a list of IdfObjects.")

    # check if input is from the same model
    # get uuid if idf
    uuid_main <- .subset2(.subset2(._get_private(x), "m_log"), "uuid")

    # get uuids of input
    uuid_in <- vcapply(value, function (obj) .subset2(.subset2(._get_private(obj), "log_env")(), "uuid"))
    # get id of input
    obj_id_in <- viapply(value, function (obj) .subset2(._get_private(obj), "m_object_id"))

    obj_main <- get_idf_object(idd_env, idf_env, cls_id)

    # ignore ones that is from the same idf
    same_idf <- uuid_main == uuid_in
    same_num <- length(value) == nrow(obj_main)
    same_id <- obj_id_in %in% obj_main$object_id

    # direct return the idf
    if (all(same_idf) && same_num && all(same_id)) return(invisible(x))

    # stop if not from the same class
    cls_id_in <- viapply(value, function (obj) .subset2(._get_private(obj), "m_class_id"))
    if (any(cls_id_in != cls_id)) {
        invld_cls <- vcapply(value[cls_id_in != cls_id], function (obj) .subset2(obj, "class_name")())
        msg <- paste0(" #", which(cls_id_in != cls_id), "| <IdfObject> --> Class: ", surround(invld_cls),
            collapse = "\n"
        )
        abort("error_invalid_input_object_class",
            paste0(
                "Input IdfObjects should all from class `", obj_main$class_name[[1L]]), "`. ",
                " Invalid input:\n", msg

        )
    }

    # ignore same objects and insert new ones
    invisible(.subset2(x, "insert")(value[!same_id], .unique = FALSE))

    # delete objects that are not included in input
    id_del <- setdiff(obj_main$object_id, obj_id_in)
    if (!length(id_del)) return(invisible(x))

    invisible(.subset2(x, "del")(id_del, .unique = FALSE))

    invisible(x)
}
# }}}

#' @export
# [[.Idf {{{
`[[.Idf` <- `$.Idf`
# }}}

#' @export
# [[<-.Idf {{{
`[[<-.Idf` <- `$<-.Idf`
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
#' if (is_avail_idd(8.8)) empty_idf(8.8)
# empty_idf {{{
empty_idf <- function (ver = "latest") {
    ver <- standardize_ver(ver)
    text <- paste0("Version,", ver[, 1L:2L], ";\n")
    read_idf(text, ver)
}
# }}}
