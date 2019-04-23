#' @importFrom R6 R6Class
#' @importFrom crayon bold
#' @include impl-idf.R
NULL

#' Read, modify, and run an EnergyPlus model
#'
#' eplusr provides parsing EnergyPlus Input Data File (IDF) files and strings
#' in a hierarchical structure, which was extremely inspired by [OpenStudio utilities library](https://openstudio-sdk-documentation.s3.amazonaws.com/cpp/OpenStudio-2.5.1-doc/utilities/html/idf_page.html),
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
#' data in [data.table::data.table] format. Every IDF is parsed and stored in
#' three tables:
#'
#' * `object`: contains object IDs, names and comments.
#' * `value`: contains field values.
#' * `reference`: contains cross-reference of field values.
#'
#' IDD file is parsed and stored in a similar structure. For details, please see
#' [Idd] class.
#'
#' So to modify an EnergyPlus model in eplusr is equal to change the data in
#' those three tables accordingly, in the context of specific IDD data.
#'
#' All IDF reading process starts with [read_idf()] which returns an `Idf`
#' object. The model will be printed in a similar style you see in IDF Editor,
#' with additional heading lines show the `Path`, `Version` of the model. The
#' classes of objects in the model are ordered by group and the number of
#' objects in classes are shown in square bracket. The printing style can be
#' further customized using the `zoom` parameter in `$print()`, helping you to
#' quickly get the IDF contents at different zoom scale.
#'
#' Below is the detailed documentation on each method in Idf class.
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
#' model$search_object(pattern, class = NULL, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
#' model$ClassName
#' model[[ClassName]]
#' model$dup(...)
#' model$add(..., .default = TRUE, .all = FALSE)
#' model$set(..., .default = TRUE)
#' model$del(..., .referenced = FALSE, .recursive = FALSE, .force = FALSE)
#' model$insert(...)
#' model$rename(...)
#' model$paste(in_ip = FALSE, ver = NULL)
#' model$search_value(pattern, class = NULL, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
#' model$replace_value(pattern, class = NULL, replacement, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
#' model$is_unsaved()
#' model$validate(level = eplusr_option("validate_level"))
#' model$is_valid(level = eplusr_option("validate_level"))
#' model$to_string(comment = TRUE, header = TRUE, format = eplusr_option("save_format"), leading = 4L, sep_at = 29L)
#' model$to_table(class = NULL, object)
#' model$save(path = NULL, format = eplusr_option("save_format"), overwrite = FALSE, copy_external = TRUE)
#' model$run(weather = NULL, dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE)
#' model$clone(deep = TRUE)
#' model$print(zoom = "class", order = TRUE)
#' print(model)
#' }
#'
#' @section Basic Info:
#' ```
#' model$version()
#' model$path()
#' model$group_name(all = FALSE, sorted = TRUE)
#' model$class_name(all = FALSE, sorted = TRUE)
#' model$is_valid_group(group, all = FALSE)
#' model$is_valid_class(class, all = FALSE)
#' ```
#'
#' `$version()` returns the version of current model in a
#' [base::numeric_version] format. This makes it easy to direction compare
#' versions of different Idfs, e.g. `model1$version() > 8.6` or
#' `model1$version() > model2$version()`.
#'
#' `$path()` returns the path of current model or `NULL` if the model is created
#' using a character vector and not saved locally.
#'
#' `$group_name()` returns all groups the model contains when `all` is `FALSE`
#' or all groups the Idd contains when `all` is `TRUE`.
#'
#' `$class_name()` returns all classes the model contains when `all` is `FALSE`
#' or all classes the Idd contains when `all` is `TRUE`.
#'
#' `$is_valid_group()` returns `TRUE`s if given group names are valid for
#' current model (when `all` is `FALSE`) or current Idd (when `all` is `TRUE`).
#'
#' `$is_valid_class()` returns `TRUE`s if given class names are valid for
#' current model (when `all` is `FALSE`) or current Idd (when `all` is `TRUE`).
#'
#' **Arguments**
#'
#' * `all`: If `FALSE`, only values in current model will be returned. If
#'   `TRUE`, all values in Idd will be returned. Default: `FALSE`.
#' * `sorted`: If `TRUE`, duplications in returned values are removed and values
#'   are also further sorted according to their occurances in IDD. Default:
#'   `TRUE`.
#' * `group`: A character vector contains group names.
#' * `class`: A character vector contains class names.
#'
#' @section Definition:
#' ```
#' model$definition(class)
#' ```
#'
#' `$definition()` returns an [IddObject] of given class. IddObject
#' contains all data used for parsing [IdfObject]. For details, please see
#' [IddObject] class.
#'
#' **Arguments**
#'
#' * `class`: A single string of valid class name in current IDD.
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
#' `$object_id()` and `$object_name()` returns all object IDs and names in
#' specified classes respectively. For `$object_name()`, if the specified class
#' does not have name attributes, such as `SimulationContrl`, `NA` will be
#' returned.
#'
#' `$is_valid_id()` and `$is_valid_name()` returns `TRUE`s if given integers or
#' characters are valid object IDs or object names respectively.
#'
#' `$object_num()` returns the number of objects in specified classes.
#'
#' **Arguments**
#'
#' * `id`: An integer vector to check.
#' * `name`: A character vector to check.
#' * `class`: A character vector that contains valid class names. If `NULL`, all
#'   classes in current IDF are used. Default: `NULL`.
#' * `simplify`: If `FALSE`, a list with each member being the data per class
#'   is returned. The order of classes are the same as it in IDD. If
#'   `TRUE`, an integer vector (for `$object_id()`) or a character vector (for
#'   `$object_name`()) is returned. The order of returned object IDs or
#'   names is the same order as objects in the IDF. Default: `FALSE`.
#'
#' @section Object Query:
#'
#' \preformatted{
#' model$object(which)
#' model$object_unique(class)
#' model$objects(which)
#' model$objects_in_class(class)
#' model$search_object(pattern, class = NULL, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
#' model$ClassName
#' model[[ClassName]]
#' }
#'
#' `$object()` returns an [IdfObject] specified by an object ID or name.
#'
#' `$object_unique()` returns an [IdfObject] in unique-object class, e.g.
#' `SimulaSimulationContrl`. This makes it easy to directly extract and modify
#' those objects, e.g. `model$object_unique("SimulationContrl")$set(...)`. If
#' there are multiple objects in that unique-object class, an error is issued.
#'
#' `$objects()` returns a named **list** of [IdfObject]s specified by object IDs
#' or names. The names are the same as returned [IdfObject]s. Thus, if returned
#' [IdfObject]s do not have names, `NA`s are assigned as the name of returned
#' list.
#'
#' `$objects_in_class()` returns a named list of all [IdfObject]s in specified
#' classes.
#'
#' `$search_object()` returns a named list of [IdfObject]s whose names meet the
#' given pattern in specified classes.
#'
#' eplusr also provides custom S3 method of `$` and \code{[[} to make it more
#' convenient to get [IdfObject]s in class. Basically, `model$ClassName` and
#' \code{model[[ClassName]]}, where `ClassName` is a single valid class name, is
#' equivalent to `model$objects_in_class(ClassName)` if `ClassName` is not an
#' unique-object class and `model$object_unique(ClassName)` if `ClassName` is an
#' unique-object class. For convenience, *underscore* names are allowed, e.g.
#' `BuildingSurface_Detailed` is equivalent to `BuildingSurface:Detailed`.
#'
#' `IdfObject` is a class that provides more detailed information methods to
#'     modify a single object in an `Idf` object. For detailed explanations,
#'     please see [IdfObject] class.
#'
#' **Arguments**
#'
#' * `which`: Either an integer vector of valid object IDs or a character vector
#'     of valid object names.
#' * `class`: A single string of valid class name for `$object_unique()` or a
#'    character vector of valid class names for `$objects_in_class()`.
#' * `pattern`, `ignore.case`, `perl`, `fixed` and `useBytes`: All of them are
#'   directly passed to [base::grepl()].
#' * `ClassName`: A single string of valid class name or *underscore* class name,
#'    where all characters other than letters and numbers are replaced by a
#'    underscore `_`.
#'
#' @section Object Modification:
#' \subsection{Duplicate Objects}{
#'
#' ```
#' model$dup(...)
#' ```
#'
#' `$dup()` takes object name and ID vectors and duplicates objects specified.
#'     The names of input vector are used as new names of duplicated object. The
#'     newly created objects will be renamed automatically if new names are not
#'     given, with a suffix `"_1"`, `"_2"` and etc. A list of resultant
#'     [IdfObject]s is returned.
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
#' model$add(...)
#' ```
#'
#' `$add()` takes object definitions in list format and adds objects in the
#'    specified class. Every list should be named with a valid class name. There
#'    is a special element `.comment` in each list, which will be used as the
#'    comments of newly added object. Names in list element are treated as
#'    field names. Values without names will be inserted according to their
#'    position. A list of resultant [IdfObject]s is returned.
#'
#' **NOTE**:
#'
#' * Empty objects can be added using an empty list, e.g. `model$add(building =
#'   list())`. All empty fields will be filled with corresponding default value
#'   if `.default` is `TRUE`, leaving other fields as blank. However, adding
#'   blank objects may not be successful if current [validate level][eplusr_option()] does not allow so.
#' * Field names are case insensitive and for convenience, *underscore_name* is
#'   also allowed, e.g. `end_month` is equivalent to `End Month`.
#' * If not all field names are given, positions of those values without field
#'   names are determined after those values with names. E.g. in
#'   `model$add(Construction = list("out_layer", name = "name"))`, `"out_layer"`
#'   will be treated as the value of field `Outside Layer` in `Construction`, as
#'   value of field `Name` has been given as `"name"`.
#'
#' **Usage**:
#'
#' * Empty object with default values: `model$add(Building = list(), .default = TRUE)`.
#' * Empty object with comments: `model$add(Building = list(.comment = c("This", "is", "a", "comment")))`.
#' * Empty object with all fields: `model$add(Building = list(), .all = TRUE)`.
#' * New objects: `model$add(RunPeriod = list("rp", 1, 1, end_month = 2, 1, "Monday"), list(Construction = list("const", "mat"), Material = list("mat")))`.
#' * New objects with comments: `model$add(RunPeriod = list("rp", 1, 1, 2, 1, .comment = "comment1"))`.
#' * Variable inputs: `x <- list(Construction = list("const"), Building = list()); model$add(x)`.
#'
#' **Arguments**:
#'
#' * `.default`: If `TRUE`, default values are used for those blank fields if
#'    possible. If `FALSE`, each required field in input object must have one
#'    value. Otherwise, an error will be issued during validation. Default:
#'    `TRUE`.
#' * `.all`: If `TRUE`, all fields are added, otherwise only minimum fields are
#'   added. Default: `FALSE`.
#' }
#'
#' \subsection{Set Existing Objects}{
#'
#' ```
#' model$set(..., .default = TRUE)
#' ```
#'
#' `$set()` takes object definitions in list format and sets field values in
#'    objects specified. Every list in `$set()` should be named with a valid
#'    object name. Object ID can also be used but have to be combined with two
#'    prevailing `..`, e.g. `..10` indicates the object with ID `10`. Similar to
#'    `$add()`, a special element `.comment` in each list will be used as the
#'    **new** comments for modifed object, overwriting the old ones. Names
#'    in list element are treated as field names. Values without names will be
#'    inserted according to their position. A list of resultant [IdfObject]s is
#'    returned.
#'
#' **NOTE**:
#'
#' * You can delete a field value by assigning `NULL` to it, e.g. `list(layer1 =
#'   NULL)` means to delete the value of field `layer1`. If `.default` is
#'   `TRUE`, that field will be filled with default value (if exists).
#' * Field names are case insensitive and for convenience, *underscore_name* is
#'   also allowed, e.g. `end_month` is equivalent to `End Month`.
#' * If not all field names are given, positions of those values without field
#'   names are determined after those values with names. E.g. in
#'   `model$set(Construction = list("out_layer", name = "name"))`, `"out_layer"`
#'   will be treated as the value of field `Outside Layer` in `Construction`, as
#'   value of field `Name` has been given as `"name"`.
#'
#' **Usage**:
#'
#' * Specify object with name: `model$set(Object_Name = list(val1, val2, val3, .comment = "new comment will overwrite the old one"))`.
#' * Specify object with ID: `model$set(..8 = list(val1))`.
#' * Delete field value: `model$set(Object_Name = list(Field_1 = NULL), .default = FALSE)`.
#' * Assign default field value: `model$set(Object_Name = list(Field_1 = NULL), .default = TRUE)`.
#' * Variable input: `a <- list(Object_Name = list(Field_1 = val1)); model$set(a, .default = TRUE)`.
#'
#' **Arguments**:
#'
#' * `.default`: If `TRUE`, default values are used for those blank fields if
#'    possible. If `FALSE`, each field in input object must have one value.
#'    Otherwise, an error will be issued. Default: `TRUE`.
#' }
#'
#' \subsection{Deleting Objects}{
#'
#' ```
#' model$del(..., .referenced = FALSE, .recursive = FALSE, .force = FALSE)
#' ```
#'
#' `$del()` deletes objects specified by valid names and IDs vectors. If
#'     `.referenced` is `TRUE`, objects whose fields refer to input objects will
#'     also be deleted.
#'
#' **NOTE**:
#'
#' * If current [validate level][eplusr_option()] includes reference checking,
#'   objects will not be able to be deleted if they are referred by other
#'   object. For example, an error will be issued if you want to delete one
#'   material that is referred by other constructions, because doing so will
#'   result in invalid field reference. You may still delete it if you really
#'   want to by setting `.force` to `TRUE`.
#'
#' **Usage**:
#'
#' * Specify object with name: `model$add(Object_Name = list(val1, val2, val3, .comment = "new comment will overwrite the old one"))`.
#' * Specify object with ID: `model$add(..8 = list(val1))`.
#' * Delete field value: `model$add(Object_Name = list(Field_1 = NULL), .default = FALSE)`.
#' * Assign default field value: `model$add(Object_Name = list(Field_1 = NULL), .default = TRUE)`.
#' * Variable input: `x <- list(Object_Name = list(Field_1 = NULL)); model$add(x)`.
#'
#' **Arguments**:
#'
#' * `.referenced`: If `TRUE`, objects whose fields refer to input objects will
#'     also be deleted. Default: `FALSE`.
#' * `.recursive`: If `TRUE`, the reference checking is performed recursively,
#'   in case that objects whose fields refer to others are also referred by
#'   another object. Default: `FALSE`.
#' }
#'
#' \subsection{Rename Objects}{
#'
#' ```
#' model$rename(...)
#' ```
#'
#' `$rename()` takes named character vectors of object names and named integer
#'     vectors of object ID, and rename them accordingly.  The names of input
#'     vector are used as new names. An error will be issued if trying to
#'     "rename" an object which does not have name attribute.
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
#' model$insert(...)
#' ```
#'
#' `$insert()` takes [IdfObject] or a list of [IdfObject]s as input and insert
#' them into current Idf. If input is a list of [IdfObject]s, the name of that
#' list will be used as new names of newly inserted objects.
#'
#' **NOTE**:
#'
#' * You cannot insert an [IdfObject] which comes from a different version than
#'   current Idf object.
#'
#' **Usage**:
#'
#' * Insert objects without new names: `model1$insert(model2$Material)`.
#' * Insert objects without new names: `model1$insert(my_material = model2$Material[[1]])`.
#' * Variable input: `mat <- model2$Material; names(mat) <- c("mat1", "mat2"); model1$insert(mat)`.
#' }
#'
#' \subsection{Paste Objects from IDF Editor}{
#'
#' ```
#' model$paste(in_ip = FALSE, ver = NULL)
#' ```
#'
#' `$paste()` reads the contents of IDF Editor from clipboard, parses it and
#'    inserts objects into current Idf. 
#'
#' **Note**:
#'
#' * There is no version data copied to the clipboard when copying objects in
#'   IDF Editor. It is possible that IDF Editor opens an IDF with different
#'   version than current IDF. Please check the version before running
#'   `$paste()`, or explicitly specify the version of file opened by IDF Editor
#'   using `ver` parameter. Parsing error may occur if there is a version
#'   mismatch.
#' * If there are exactly same objects as those copied from IDF Editor, all same
#'   objects copied will be automatically removed and not inserted.
#'
#' **Usage**:
#'
#' * Paste objects from same version: `model$paste()`.
#' * Paste objects from different version: `model$paste(ver = "version")`.
#' * Paste objects that are viewed in IP units: `model$paste(in_ip = TRUE)`.
#'
#' **Arguments**:
#'
#' * `in_ip`: Set to `TRUE` if the IDF file is open with `Inch-Pound`
#'   view option toggled. Numeric values will automatically converted to SI
#'   units if necessary. Default: `FALSE`.
#' * `ver`: The version of IDF file opened by IDF Editor, e.g. 8.6, "8.8.0".
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
#'     match the given pattern.
#'
#' `$replace_value()` returns a list of [IdfObject]s whose values have been
#'     replace with given pattern.
#'
#' **Arguments**
#'
#' * `pattern`, `replacement`, `ignore.case`, `perl`, `fixed` and `useBytes`: All of them are
#'   directly passed to [base::grepl()] and [base::gsub()].
#' }
#'
#' @section Validation:
#'
#' ```
#' model$validate(level = eplusr_option("validate_level"))
#' model$is_valid(level = eplusr_option("validate_level"))
#' ```
#'
#' `$validate()` checks if there are errors in current model under different
#'     strictness level.
#'
#' `$is_valid()` returns `TRUE` if there are no errors in current model under
#'     current strictness level and `FALSE` otherwise.
#'
#' The strictness level can be changed using [eplusr_option()]. Default is
#'     `"final". `There are three different validate levels, i.e. `"none"`,
#'     `"draft"` and `"final"`:
#'
#'   * For `"none"`, none validation will be done. In this case, `$is_valid()`
#'     will always return `TRUE`;
#'   * For `"draft"`, checking of invalid autosize, autocalculate, character,
#'     numeric, integer, and choice field values will be performed;
#'   * For `"final"`, besides above, checking of incomplete extensible groups,
#'     missing required objects, duplicated unique objects, object name
#'     conflicts, missing required fields and invalid field value references
#'     will also be performed.
#'
#'  Underlying, `$validate()` returned a list of thirteen components. Except
#'      `missing_object`, which is a character vector, all other components are
#'      [data.table::data.table()]s. The contents of each component
#'      are described blow:
#'
#'    * `missing_object`: A character vector that contains names of classes
#'      which are required but currently none object exists.
#'    * `duplicate_object`: A data.table that contains data of all objects in
#'      unique-object class which should only have one object but currently
#'      multiple objects exist.
#'    * `conflict_name`: A data.table that contains data of all objects
#'      that have the same name in the same class.
#'    * `incomplete_extensible`: A data.table that contains data of all object
#'      fields that are extensible but with empty value.
#'    * `missing_value`: A data.table that contains data of all object fields
#'      that are required but have empty value.
#'    * `invalid_autosize`: A data.table that contains data of all object
#'      fields which should not be "Autosize".
#'    * `autocalculate`: A data.table that contains data of object fields
#'       which should not be "Autocalculate".
#'    * `invalid_character`: A data.table that contains data of all object
#'      fields which should be character type, but currently are not.
#'    * `invalid_numeric`: A data.table that contains data of all object fields
#'       which should be numbers, but currently are not.
#'    * `invalid_integer`: A data.table that contains data of all object fields
#'      which should be integers, but currently are not.
#'    * `invalid_choice`: A data.table that contains data of all object fields
#'      whose values are not one of prescribed choices.
#'    * `invalid_range`: A data.table that contains data of all object fields
#'      whose values exceed prescribed ranges.
#'    * `invalid_reference`: A data.table that contains data of all object
#'      fields whose values are not one of available reference values.
#'
#'  All data.tables above contains nine columns:
#'
#'    * `object_id`: IDs of objects that contain invalid values
#'    * `object_name`: names of objects that contain invalid values
#'    * `class_id`: indices of classes that invalid objects belong to
#'    * `class_name`: names of classes that invalid objects belong to
#'    * `field_id`: indices (at IDD level) of object fields that are invalid
#'    * `field_index`: indices of object fields that are invalid
#'    * `field_name`: names (without units) of object fields that are invalid
#'    * `units`: SI units of object fields that are invalid
#'    * `ip_units`: IP units of object fields that are invalid
#'    * `type_enum`: An integer vector indicates types of invalid fields
#'    * `value_id`: indices of object field values that are invalid
#'    * `value_chr`: values (converted to characters) of object field that are invalid
#'    * `value_num`: values (converted to numbers in SI units) of object field
#'       that are invalid
#'
#'  Knowing the internal structure of returned data from `$validate()`, it is
#'      easy to extract data of invalid objects you interested in. For example,
#'      you can get all IDs of objects that contains invalid value references
#'      using `$validate()$invalid_reference$object_id`. Then using
#'      `$set()` to correct them.
#'
#' @section Format Output:
#'
#' ```
#' model$to_string(comment = TRUE, header = TRUE, format = eplusr_option("save_format"), leading = 4L, sep_at = 29L)
#' ```
#'
#' `$to_string()` returns the text format of an IDF file.
#'
#' **Arguments**
#'
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
#' `$is_unsaved()` checks if there are modifications on the model since it was
#'     read or since last time it was saved.
#'
#' `$save()` saves Idf object into local disk.
#'
#' **Arguments**
#'
#' * `path`: A path where to save the model. If `NULL`, the path of the model
#'   itself will be used.
#' * `format`: A string to specify the saving format. Should be one of `"asis"`,
#'   `"sorted"`, `"new_top"`, and `"new_bot"`. If `"asis"`, the
#'   model will be saved in the same format as it is. If the model does not
#'   contain any format saving option, which is typically the case when the
#'   model was not saved using eplusr or IDFEditor, `"sorted"` will be used.
#'   `"sorted"`, `"new_top"` and `"new_bot"` are the same as the save options
#'   `"Sorted"`, `"Original with New at Top"`, and `"Original with New at Bottom"`
#'    in IDFEditor. Default: `eplusr_option("save_format")`
#' * `overwrite`: Whether to overwrite the file if it already exists. Default:
#'    `FALSE`.
#' * `copy_external`: If `TRUE`, the external files will also be copied into the
#'   same directory. The values of file paths in the Idf will be changed
#'   automatically. Currently, only `Schedule:File` class is supported.
#'   Default: `FALSE`.
#'
#' @section Clone:
#'
#' ```
#' model$clone(deep = TRUE)
#' ```
#'
#' `$clone()` copies and returns the cloned model. Because `Idf` uses `R6Class`
#'     under the hook which has "modify-in-place" semantics, `idf_2 <- idf_1`
#'     does not copy `idf_1` at all but only create a new binding to `idf_1`.
#'     Modify `idf_1` will also affect `idf_2` as well, as these two are exactly
#'     the same thing underneath. In order to create a complete cloned copy,
#'     please use `$clone(deep = TRUE)`.
#'
#' **Arguments**
#'
#' * `deep`: Has to be `TRUE` if a complete cloned copy is desired.
#'
#' @section Run Model:
#'
#' ```
#' model$run(weather, dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE)
#' ```
#'
#' `$run()` will run the current model together with specified weather using
#'     corresponding version of EnergyPlus. The model and the weather used will
#'     be copied to the output directory. An `EplusJob` will be returned which
#'     provides detailed info of the simulation and methods to collect
#'     simulation results. Please see [eplus_job()] for more detailed.
#'
#' eplusr uses the EnergyPlus command line interface which was introduced since
#'     EnergyPlus 8.3.0. So `$run()` only supports models with version higher
#'     than 8.3.0.
#'
#' eplusr uses the EnergyPlus SQL output for extracting simulation results. In
#'    order to do so, a object in `Output:SQLite` with `Option Type` value of
#'    `SimpleAndTabular` will be automatically created if it does not exists.
#'
#' **Arguments**
#'
#' * `weather`: A path to an `.epw` file or an [Epw] object.
#' * `dir`: The directory to save the simulation results. If `NULL`, the model
#'    folder will be used. Default: NULL
#' * `wait`: Whether to wait until the simulation completes and print the
#'     standard output and error of EnergyPlus to the screen. Default is `TRUE`.
#' * `force`: Whether to force to stop the background EnergyPlus process and
#'   start the simulation again.
#' * `copy_external`: If `TRUE`, the external files will also be copied into the
#'     simulation output directory. The values of file paths in the Idf will be
#'     changed automatically. Currently, only `Schedule:File` class is supported.
#'     This ensures that the output directory will have all files needed for the
#'     model to run. Default is `FALSE`.
#'
#' @section Print:
#' ```
#' model$print(zoom = "class", order = TRUE)
#' print(model)
#' ```
#'
#' `$print()` will print the model in the similar format as what you will see in
#'     IDF Editor.
#'
#' **Arguments**
#'
#' * `zoom`: Control how detailed of Idf shoud be printed. Should be one of
#'   `"group"`, `"class"`, `"object"` and `"field"`. Default: `"class"`.
#'
#'   * `"group"`: all group names current existing are shown with prevailing
#'     square bracket showing how many **C**lasses existing in that group.
#'   * `"class"`: all class names are shown with prevailing square bracket
#'     showing how many **O**bjects existing in that class, together with
#'     parent group name of each class.
#'   * `"object"`: all object IDs and names are shown, together with parent
#'     class name of each object.
#'   * `"field"`: all object IDs and names, field name and values are shown, together with parent
#'     class name of each object.
#'
#' * `order`: Only applicable when `zoom` is `"object"` or `"field"`. If `TRUE`,
#'   objects are grouped by classes. If `FALSE`, objects are shown as the same
#'   order in the IDF. Default: `TRUE`
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
#' \dontrun{idf$dup(rep("R13WALL", times = 10))}
#'
#' # ===== ADD OBJECTS =====
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
#'
#' # ===== INSERT OBJECTS =====
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
#' # $ins_object() is useful when importing design days from a ".ddy" file
#' \dontrun{idf$ins_object(read_idf("foo.ddy"))}
#'
#' # ===== SET OBJECTS =====
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
#'
#' # ===== RENAME OBJECTS =====
#' idf$rename(new_test = "test")
#' idf$object_name("Material")
#'
#' # ===== DELELTE OBJECTS =====
#' # delete the added run period "rp_test_1", "rp_test_2" and "new_test" from above
#' idf$del(c("new_test", "rp_test_1", "rp_test_2"))
#' names(idf$Material)
#' names(idf$RunPeriod)
#'
#' # In "final" validate level, delete will be aborted if the target obejcts are
#' # referenced by other objects.
#' # get objects that referenced material "R13LAYER"
#' eplusr_option("validate_level")
#'
#' idf$Material_NoMass$R13LAYER$ref_by_object()
#' length(idf$Material_NoMass$R13LAYER$ref_by_object())
#'
#' \dontrun{idf$del("R13LAYER")} # will give an error in "final" validate level
#'
#' # objects referencing target objects can also be deleted by setting
#' # `referenced` to TRUE
#' \dontrun{idf$del("R13LAYER", .referenced = TRUE)} # will give an error in "final" validate level
#'
#' # it is possible to force delete objects
#' \dontrun{idf$del("R13LAYER", .referenced = TRUE, .force = TRUE)}
#'
#' # ===== SEARCH ADN REPLACE OBJECT VALUES =====
#' # get objects whose field values contains both "VAV" and "Node"
#' idf$search_value("WALL")
#' length(idf$search_value("WALL"))
#' names(idf$search_value("WALL"))
#'
#' # replace values using regular expression
#' idf$replace_value("WALL", "A_WALL")
#'
#' # ===== VALIDATE MODEL =====
#' # CRAN does not like long-time tests
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
#' str(idf$to_string())
#'
#' # get text format of the model, excluding the header and all comments
#' str(idf$to_string(comment = FALSE, header = FALSE))
#'
#' # ===== SAVE MODEL =====
#' # check if the model has been modified since read or last saved
#' idf$is_unsaved()
#'
#' # save and overwrite current model
#' \dontrun{idf$save(overwrite = TRUE)}
#'
#' # save the model with newly created and modified objects at the top
#' \dontrun{idf$save(overwrite = TRUE, format = "new_top")}
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
#'
#' # ===== CLONE MODEL =====
#' # Idf object are modified in place and has reference semantic.
#' idf_2 <- idf
#' idf_2$object_name("Building")
#' idf$object_name("Building")
#'
#' # modify idf_2 will also affect idf as well
#' idf_2$Building[[1]]$set_value(name = "Building_Name_Changed")
#' idf_2$object_name("Building")
#' idf$object_name("Building")
#'
#' # in order to make a copy of an Idf object, use $clone() method
#' idf_3 <- idf$clone(deep = TRUE)
#' idf_3$Building[[1]]$set_value(name = "Building_Name_Changed_Again")
#' idf_3$object_name("Building")
#'
#' idf$object_name("Building")
#'
#' # run the model
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
#' # print the text format of model
#' idf$print(plain = TRUE)
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

        object_in_class = function (class)
            idf_object_in_class(self, private, class),

        objects_in_class = function (class)
            idf_objects_in_class(self, private, class),

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

        del = function (..., .referenced = FALSE, .recursive = FALSE, .force = FALSE)
            idf_del(self, private, ..., .referenced = .referenced, .recursive = .recursive, .force = .force),

        rename = function (...)
            idf_rename(self, private, ...),

        insert = function (...)
            idf_insert(self, private, ...),

        paste = function (in_ip = FALSE, ver = NULL)
            idf_paste(self, private, in_ip = in_ip, ver = ver),

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

        is_unsaved = function ()
            idf_is_unsaved(self, private),

        validate = function (level = eplusr_option("validate_level"))
            idf_validate(self, private, level),

        is_valid = function (level = eplusr_option("validate_level"))
            idf_is_valid(self, private, level),

        to_string = function (comment = TRUE, header = TRUE, format = eplusr_option("save_format"),
                              leading = 4L, sep_at = 29L)
            idf_to_string(self, private, comment = comment, header = header,
                         format = format, leading = leading, sep_at = sep_at),

        string = function (comment = TRUE, header = TRUE, format = eplusr_option("save_format"),
                           leading = 4L, sep_at = 29L)
            idf_string(self, private, comment = comment, header = header,
                       format = format, leading = leading, sep_at = sep_at),

        to_table = function (class = NULL, which, string_value = TRUE, unit = FALSE)
            idf_to_table(self, private, class = NULL, object = which, string_value = string_value, unit = unit),

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
    obj <- get_idf_object(private$idd_env(), private$idf_env(), class)

    res <- apply2(obj$object_id, obj$class_id, IdfObject$new, list(parent = self))
    setattr(res, "names", obj$object_name)
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
# idf_set_in_class {{{
idf_set_in_class <- function (self, private, ..., .default = TRUE) {
    set <- set_idf_object_in_class(private$idd_env(), private$idf_env(), ..., .default = .default)
    merge_idf_data(private$idf_env(), set, by_object = TRUE)

    # log
    log_add_order(private$m_log, set$object$object_id)
    log_unsaved(private$m_log)
    log_new_uuid(private$m_log)

    idf_return_modified(self, private, set)
}
# }}}
# idf_del {{{
idf_del <- function (self, private, ..., .referenced = FALSE, .recursive = FALSE, .force = FALSE) {
    del <- del_idf_object(private$idd_env(), private$idf_env(), ...,
        .referenced = .referenced, .recursive = .recursive, .force = .force
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
    idf_del(self, private, object, .referenced = referenced)
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
idf_insert <- function (self, private, ...) {
    ins <- insert_idf_object(private$idd_env(), private$idf_env(), private$m_version, ...)
    merge_idf_data(private$idf_env(), ins)

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
idf_paste <- function (self, private, in_ip = FALSE, ver = NULL) {
    pas <- paste_idf_object(private$idd_env(), private$idf_env(), version = private$m_version, in_ip = in_ip)

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
idf_to_string <- function (self, private, comment = TRUE, header = TRUE,
                         format = eplusr_option("save_format"),
                         leading = 4L, sep_at = 29L, ...) {
    if (format == "asis") format <- private$m_log$save_format

    fmt <- with_nocolor(with_format_cols(private$idd_env(), private$idf_env(),
        format_idf(
            private$idf_env()$value, private$idf_env()$object, private$m_log$order,
            comment = comment, header = header, save_format = format,
            leading = leading, sep_at = sep_at, ...
        )
    ))

    if (format == "sorted") {
        combine_fmt <- function (lst) {
            head <- if (is.null(lst[[1L]])) "" else c("", lst[[1L]], "")
            c(head, lst[[2L]][[1L]], lst[[2L]][[2L]], "")
        }

        body <- unlist(lapply(fmt$format$fmt, combine_fmt), use.names = FALSE)
    } else {
        combine_fmt <- function (lst) c(unlist(lst, use.names = FALSE), "")

        body <- unlist(lapply(fmt$format$fmt, combine_fmt), use.names = FALSE)
    }

    if (header) c(fmt$header, "", body) else body
}
# }}}
# idf_string {{{
idf_string <- function (self, private, ...) {
    .deprecated_fun("$string()", "$to_string()", "Idf", "0.10.0")
    idf_to_string(self, private, ...)
}
# }}}
# idf_to_table {{{
idf_to_table <- function (self, private, class = NULL, object, string_value = TRUE, unit = FALSE) {
    get_idf_table(private$idd_env, private$idf_env, class, object, string_value, unit)
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

    # check if the model is still running
    old <- private$m_log$job
    if (!is.null(old)) {
        proc <- ._get_private(old)$m_process$process
        if (inherits(proc, "process") && proc$is_alive()) {
            pid <- proc$get_pid()
            if (force) {
                old$kill()
                message("Force to kill current running simulation (PID: ", pid,
                    ") and start a new simulation...")
            } else {
                stop("The simulation of current Idf is still running (PID: ",
                    pid, "). Please set `force` to TRUE if you want ",
                    "to kill the running process and start a new simulation.",
                    call. = FALSE)
            }
        }
    }

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

    job <- EplusJob$new(path_idf, epw, private$m_version)

    job$run(wait = wait)

    private$m_log$job <- job

    job
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
#'     string or a raw vector) to an EnergyPlus Input Data File (IDF), usually
#'     has a extension `.idf`.
#' @param idd  Any acceptable input of [use_idd()]. If `NULL`, which is the
#'     default, the version of IDF will be passed to [use_idd()]. If the input
#'     IDF does not have a version field (possible for ".ddy" files), then it
#'     will be parsed using the latest version of IDD cached, with a warning.
#' @details
#' Currently, Imf file is not fully supported. All EpMacro lines will be treated
#' as normal comments of the nearest downwards object. If input is an Imf file,
#' a warning will be given during parsing. It is recommended to convert the Imf
#' file to an Idf file and use [ParametricJob] class to conduct
#' parametric analysis.
#'
#' @return An `Idf` object.
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
# [[.Idf {{{
`[[.Idf` <- `$.Idf`
# }}}

# str.idf {{{
str.Idf <- function (x, zoom = "class", ...) {
    x$print(zoom)
}
# }}}

# format.idf {{{
format.Idf <- function (x, comment = TRUE, header = TRUE,
                        format = eplusr_option("save_format"),
                        leading = 4L, sep_at = 29L, index = FALSE, blank = FALSE,
                        end = TRUE, required = FALSE, ...) {
    x$to_string()
}
# }}}

# empty_idf {{{
empty_idf <- function (ver = "latest") {
    ver <- standardize_ver(ver)[, 1:2]
    text <- paste0("Version,", ver, ";\n")
    read_idf(text, ver)
}
# }}}

