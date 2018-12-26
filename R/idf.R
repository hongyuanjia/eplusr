#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
NULL

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
#' [Idd] class.
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
#' model$object(which)
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
#' model$clone(deep = FALSE)
#' model$run(weather = NULL, dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE)
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
#'     of `IdfObject`, please see [IddObject] class.
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
#' * `class`: A character vector that contains valid class names.
#' * `simplify`: If `FALSE`, a list with each member being the data per class
#'     will be returned. The order of classes are the same as it in Idd. If
#'     `TRUE`, an integer vector (for `$object_id()`) or a character vector (for
#'     `$object_name`()) will be returned. The order of returned object IDs or
#'     names will be the same order as objects in the IDF file.  Default:
#'     `FALSE`.
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
#' `$object()` will return a named list of `IdfObject`s specified by object IDs
#'     or names.
#'
#' `$object_in_class()` will return a named list of all `IdfObject`s in specified
#'     classes.
#'
#' `$search_object()` will return a named list of `IdfObject`s whose names meet the
#'     given pattern in specified classes.
#'
#' The names of returned list by `$object()`, `$object_in_class()` and
#'     `$search_object()` are the returned object names, except that all names
#'     are converted into valid R names, i.e.  all other characters except
#'     letters and numbers are replaced by underscore `_`.
#'
#' eplusr also provides custom S3 method of `$` and \code{[[} to make it more
#'     convenient to get `IdfObject`s in class. Basically, `model$ClassName` and
#'     \code{model[[ClassName]]}, where `ClassName` is a single valid class
#'     name, is equivalent to `model$object_in_class(ClassName)`.
#'
#' All above methods will return a named list of `IdfObject`s. If the class does
#'     not have name attribute, then `NA` will be used.
#'
#' `IdfObject` is a class that provides more detailed information methods to
#'     modify a single object in an `Idf` object. For detailed explanations,
#'     please see [IdfObject] class.
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
#' The strictness level can be changed using [eplusr_option()]. Default is
#'     `"final". `There are three different validate levels, i.e. `"none"`,
#'     `"draft"` and `"final"`:
#'
#'   * For `"none"`, none validation will be done;
#'   * For `"draft"`, checking of invalid autosize, autocalculate, character,
#'     numeric, integer, and choice field values will be done;
#'   * For `"final"`, besides above, checking of incomplete extensible groups,
#'     missing required objects, duplicated unique objects, object name
#'     conflicts, missing required fields and invalid field value reference will
#'     also be done.
#'
#'  Underlying, `$validate()` returned a list of thirteen components. Except
#'      `missing_object`, which is a character vector, all other components are
#'      [data.tables][data.table::data.table()]. The contents of each component
#'      are described blow:
#'
#'    * `missing_object`: A character vector that contains names of classes
#'      which are required but currently none object exists.
#'    * `duplicate_object`: A data.table that contains data of all objects in
#'      unique class which should only have one object but currently multiple
#'      objects exist.
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
#'  All data.tables above contains thirteen columns:
#'
#'    * `object_id`: IDs of objects that contain invalid fields
#'    * `class_id`: indexes of classes that invalid objects belong to
#'    * `class_name`: names of classes that invalid objects belong to
#'    * `field_index`: indexes of object fields that are invalid
#'    * `field_name`: names (without units) of object fields that are invalid
#'    * `full_name`: names (with SI units) of object fields that are invalid
#'    * `full_ipname`: names (with IP units) of object fields that are invalid
#'    * `type`: types of object fields that are invalid
#'    * `value_id`: indexes of object field values that are invalid
#'    * `value`: values (converted to characters) of object field that are invalid
#'    * `value_upper`: values (converted to upper-case characters) of object
#'       field that are invalid
#'    * `value_num`: values (converted to numbers in SI units) of object field
#'       that are invalid
#'    * `value_ipnum`: values (converted to numbers in IP units) of object field
#'       that are invalid
#'
#'  Knowing the internal structure of returned data from `$validate()`, it is
#'      easy to extract data of invalid objects you interested in. For example,
#'      you can get all IDs of objects that contains invalid value references
#'      using `$validate()$invalid_reference$object_id`. Then using
#'      `$set_object()` to correct them.
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
#'     same directory. The values of file paths in the Idf will be changed
#'     automatically. Currently, only `Schedule:File` class is supported.
#'     Default is `FALSE`.
#'
#' @section Clone:
#'
#' ```
#' model$clone(deep = FALSE)
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
#' `$run()` will run the current model within specified weather using
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
#' order to do so, a object in `Output:SQLite` with `Option Type` value of
#' `SimpleAndTabular` will be automatically created if it does not exists.
#'
#' **Arguments**
#'
#' * `weather`: A path to an `.epw` file or an `Epw` object.
#' * `dir`: The directory to save the simulation results. If `NULL`, the model
#'    folder will be used.
#' * `wait`: Whether to wait until the simulation completed and print the
#'     standard output and error of EnergyPlus to the screen. Default is `TRUE`.
#' * `force`: Whether to stop the background EnergyPlus process and start the
#'     simulation again.
#' * `copy_external`: If `TRUE`, the external files will also be copied into the
#'     simulation output directory. The values of file paths in the Idf will be
#'     changed automatically. Currently, only `Schedule:File` class is supported.
#'     This ensures that the output directory will have all files needed for the
#'     model to run. Default is `FALSE`.
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
#' * `plain`: If `TRUE`, the model will be printed in plain text format with
#'     newly added and modified objects at the bottom.
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
#'
#' idf$is_valid_group("Compliance Objects")
#'
#' # check if input is a valid group name in IDD
#' idf$is_valid_group("Compliance Objects", all = TRUE)
#'
#' # check if input is a valid class name in current model
#' idf$is_valid_class("Building")
#'
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
#' # get objects using object IDs or names
#' idf$object(c(3,10))
#' # NOTE: object name matching is case-insensitive
#' idf$object(c("Simple One Zone (Wireframe DXF)", "zone one"))
#'
#' # the names of returned list are "underscore-style" object names
#' names(idf$object(c("Simple One Zone (Wireframe DXF)", "zone one")))
#'
#' # get all objects in classes in a named list
#' idf$object_in_class("Zone")
#' names(idf$object_in_class("Zone"))
#'
#' # OR using shortcuts
#' idf$Zone
#' idf[["Zone"]]
#'
#' # search objects using regular expression
#' length(idf$search_object("R13"))
#'
#' names(idf$search_object("R13"))
#'
#' # search objects using regular expression in specifc class
#' length(idf$search_object("R13", class = "Construction"))
#'
#' # get more controls on matching using `stringr::regex()`
#' names(idf$search_object(stringr::regex("zn.*1.*wall", ignore_case = TRUE)))
#'
#' # ===== DUPLICATE OBJECTS =====
#' # duplicate objects in "Construction" class
#' names(idf$Construction)
#'
#' idf$dup_object("R13WALL")
#' # new objects will have the same names as the duplicated objects but with a
#' # suffix "_1", "_2" and etc.
#' names(idf$Construction)
#'
#' # new names can also be explicitly specified
#' idf$dup_object("R13WALL", new_name = "My-R13Wall")
#'
#' # duplicate an object multiple times
#' \dontrun{idf$dup_object(rep("R13WALL", time = 10))}
#'
#' # ===== ADD OBJECTS =====
#' # add two new objects in "RunPeriod" class
#' idf$add_object(rep("RunPeriod", 2),
#'     value = list(
#'         list("rp_test_1", 1, 1, 2, 1),
#'
#'         list(name = "rp_test_2",
#'             begin_month = 3,
#'             begin_day_of_month = 1,
#'             end_month = 4,
#'             end_day_of_month = 1)
#'     ),
#'     comment = list(
#'         list("Comment for new object 1", "Another comment"),
#'         list("Comment for new object 2")),
#'     default = TRUE
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
#' idf_1$Material$C5_4_IN_HW_CONCRETE$set_value(name = "test")
#'
#' # insert the object
#' idf$ins_object(idf_1$Material$test)
#'
#' # check if material named "test" is there
#' idf$object_name("Material")
#'
#' # $ins_object() is useful when importing design days from a ".ddy" file
#' \dontrun{idf$ins_object(read_idf("foo.ddy"))}
#'
#' # ===== SET OBJECTS =====
#' # set the thickness of newly inserted material "test" to 0.2 m
#' idf$set_object("test", value = list(thickness = 0.2))
#' idf$Material$test$Thickness
#'
#' # set thermal absorptance of all material to 0.85
#' id_mat <- idf$object_id("Material", simplify = TRUE)
#' idf$set_object(id_mat,
#'     value = rep(
#'         list(list(thermal_absorptance = 0.85)),
#'         times = length(id_mat)
#'     )
#' )
#'
#' # check results
#' lapply(idf$Material, function (mat) mat$Thermal_Absorptance)
#'
#' # reset thermal absorptance of all material to the default
#' idf$set_object(id_mat,
#'     value = rep(
#'         list(list(thermal_absorptance = NA)),
#'         times = length(id_mat)
#'     ),
#'     default = TRUE
#' )
#' # check results
#' lapply(idf$Material, function (mat) mat$Thermal_Absorptance)
#'
#' # ===== DELELTE OBJECTS =====
#' # delete the added run period "rp_test_1", "rp_test_2" and "test" from above
#' idf$del_object(c("test", "rp_test_1", "rp_test_2"))
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
#' \dontrun{idf$del_object("R13LAYER")} # will give an error in "final" validate level
#'
#' # objects referencing target objects can also be delted by setting `referenced`
#' # to TRUE
#' \dontrun{idf$del_object("R13LAYER", referenced = TRUE)} # will give an error in "final" validate level
#'
#' # ===== SEARCH ADN REPLACE OBJECT VALUES =====
#' # get objects whose field values contains both "VAV" and "Node"
#' idf$search_value("WALL")
#' length(idf$search_value("WALL"))
#' names(idf$search_value("WALL"))
#'
#' # replace values using regular expression
#' # NOTE: No field validation will be performed! Should be treated as a low-level
#' # method. Use with caution.
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
#' idf$set_object("FLOOR", list(outside_layer = "wrong_layer"))
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
#' idf$set_object(16, list(outside_layer = idf$Material[[1]]$name()))
#' idf$validate()
#' idf$is_valid()
#' }
#' # ===== FORMAT MODEL =====
#' # get text format of the model
#' str(idf$string())
#'
#' # get text format of the model, excluding the header and all comments
#' str(idf$string(comment = FALSE, header = FALSE))
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
#' idf_3 <- idf$clone()
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

            idf_file <- read_idf_file(path, idd)
            idd <- use_idd(idf_file$version)$clone(deep = TRUE)

            # in case there is no version field in input IDF
            private$m_version <- idf_file$version

            idd <- attr(idf_file, "idd")
            # init idd tbl
            private$m_idd_tbl <- ._get_private(idd)$m_idd_tbl

            # get IddObject R6ClassGenerator
            private$m_iddobj_gen <- ._get_private(idd)$m_iddobj_gen

            # init idf tbl
            private$m_idf_tbl <- list2env(idf_file[c("object", "value", "reference")], parent = emptyenv())

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
            private$m_idfobj_gen <- create_idfobj_generator(self, private, IdfObject)
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        version = function ()
            idf_version(self, private),

        path = function ()
            idf_path(self, private),

        group_name = function (all = FALSE)
            idf_group_name(self, private, all),

        class_name = function (all = FALSE)
            idf_class_name(self, private, all),

        object_id = function (class = NULL, simplify = FALSE)
            idf_object_id(self, private, class, simplify),

        object_name = function (class = NULL, simplify = FALSE)
            idf_object_name(self, private, class, simplify),

        object_num = function (class = NULL)
            idf_object_num(self, private, class),

        is_valid_group = function (group, all = FALSE)
            idf_is_valid_group_name(self, private, group, all),

        is_valid_class = function (class, all = FALSE)
            idf_is_valid_class_name(self, private, class, all),

        is_valid_id = function (id)
            idf_is_valid_object_id(self, private, id),

        is_valid_name = function (name)
            idf_is_valid_object_name(self, private, name),

        is_unsaved = function ()
            idf_is_unsaved(self, private),

        definition = function (class)
            idf_definition(self, private, class),

        object = function (which)
            idf_object(self, private, which),

        object_in_class = function (class)
            idf_object_in_class(self, private, class),

        search_object = function (pattern, class = NULL, ...)
            idf_search_object(self, private, pattern, class, ...),

        dup = function (...)
            idf_dup(self, private, ...),

        add = function (..., .default = TRUE, .all = FALSE)
            idf_add(self, private, ..., .default = .default, .all = .all),

        ins = function (...)
            idf_ins_object(self, private, ...),

        set = function (...)
            idf_set(self, private, ...),

        del = function (..., .purge = FALSE, .referenced = FALSE)
            idf_set(self, private, ...),

        dup_object = function (object, new_name = NULL)
            idf_dup_object(self, private, object, new_name),

        add_object = function (class, value = NULL, comment = NULL, default = TRUE, all = FALSE)
            idf_add_object(self, private, class, value, comment, default, all),

        ins_object = function (object)
            idf_ins_object(self, private, object),

        set_object = function (object, value = NULL, comment = NULL, default = FALSE)
            idf_set_object(self, private, object, value, comment, default),

        del_object = function (object, referenced = FALSE)
            i_del_object(self, private, object, referenced),

        search_value = function (pattern)
            i_search_value(self, private, pattern),

        replace_value = function (pattern, replacement)
            i_replace_value(self, private, pattern, replacement),

        validate = function ()
            idf_validate(self, private),

        is_valid = function ()
            i_is_valid_idf(self, private),

        string = function (comment = TRUE, header = TRUE, ...)
            i_object_string(self, private, comment = comment, header = header, ...),

        save = function (path = NULL, format = eplusr_option("save_format"), overwrite = FALSE, copy_external = TRUE)
            i_idf_save(self, private, path, format, overwrite, copy_external),

        run = function (weather = NULL, dir, wait = TRUE, force = FALSE, copy_external = FALSE)
            i_idf_run(self, private, weather, dir, wait, force, copy_external = copy_external),

        print = function (plain = FALSE)
            idf_print(self, private, plain)
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
        m_idfobj_gen = NULL,
        m_iddobj_gen = NULL,
        # }}}

        deep_clone = function (name, value) {
            i_deep_clone(self, private, name, value, "Idf")
        }
    )
)
# }}}

# read_idf_file {{{
read_idf_file <- function (path, idd = NULL) {
    # read IDF string and get version first to get corresponding IDD
    idf_dt <- read_lines_in_dt(path)
    idf_ver <- get_idf_ver(idf_dt$string)
    idd <- get_idd_from_ver(idf_ver, idd)

    # parse IDF
    # insert version line if necessary
    if (is.null(idf_ver)) {
        idf_dt <- rbindlist(
            list(idf_dt,
                data.table(
                    line = nrow(idf_dt) + 1L,
                    string = paste0("Version, ", ._get_private(idd)$m_version[, 1L:2L], ";")
                )
            ),
            use.names = TRUE
        )
    }

    setattr(idf_dt, "class", c("IdfDt", class(idf_dt)))
    setattr(idf_dt, "version", idf_ver)

    parse_idf_file(idf_dt, ._get_private(idd)$m_version, ._get_private(idd)$m_idd_tbl)
}
# }}}

# Get Idd object from input IDF version
#
# @param idf_ver NULL or a valid IDF version
# @param idd NULL or valid input for [use_idd()]
# @param warn If `TRUE`, extra warning message will be shown
# get_idd_from_ver {{{
get_idd_from_ver <- function (idf_ver = NULL, idd = NULL, warn = TRUE) {
    if (!is.null(idf_ver)) {
        # if input IDF has a version but neither that version of EnergyPlus nor
        # IDD is available, rewrite the message
        idd <- tryCatch(use_idd(idf_ver),
            error_no_matched_idd = function (e) {
                mes <- stri_replace_all_fixed(conditionMessage(e),
                    "You may want to set `download`",
                    "You may want to use `use_idd()` and set `download`"
                )
                stop(mes)
            }
        )
    } else {
        mes <- "Missing version field in input IDF."

        if (!is.null(idd)) {
            idd <- use_idd(idd)
            if (warn) {
                warn("warn_given_idd_used",
                    paste0(
                        mes, " The given IDD version ", idd$version(),
                        " will be used. Parsing errors may occur."
                    )
                )
            }
        } else {
            if (is.null(avail_idd())) {
                abort("error_no_avail_idd",
                    paste(mes, "No parsed IDD was available to use.")
                )
            }

            idd <- use_idd(avail_idd()[length(avail_idd())])
            if (warn) {
                warn("warn_latest_idd",
                    paste0(mes,
                        " The latest parsed IDD version ", idd$version(),
                        " will be used. Parsing errors may occur."
                    )
                )
            }
        }
    }

    idd$clone(deep = TRUE)
}
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
idf_group_name <- function (self, private, all = FALSE) {
    if (all) {
        t_group_name(private$m_idd_tbl$class, private$m_idd_tbl$group)
    } else {
        t_group_name(private$m_idf_tbl$object, private$m_idd_tbl$group)
    }
}
# }}}
# idf_class_name {{{
idf_class_name <- function (self, private, all = FALSE) {
    if (all) {
        private$m_idd_tbl$class$class_name
    } else {
        private$m_idf_tbl$object[order(class_id), unique(class_name)]
    }
}
# }}}
# idf_object_id {{{
idf_object_id <- function (self, private, class = NULL, simplify = FALSE) {
    t_object_id(private$m_idf_tbl$object, class, simplify)
}
# }}}
# idf_object_name {{{
idf_object_name <- function (self, private, class = NULL, simplify = FALSE) {
    t_object_name(private$m_idf_tbl$object, class, simplify)
}
# }}}
# idf_object_num {{{
idf_object_num <- function (self, private, class = NULL) {
    t_object_num(private$m_idf_tbl$object, class)
}
# }}}
# idf_is_valid_group_name {{{
idf_is_valid_group_name <- function (self, private, group, all = FALSE) {
    group %in% idf_group_name(self, private, all)
}
# }}}
# idf_is_valid_class_name {{{
idf_is_valid_class_name <- function (self, private, class, all = FALSE) {
    class %in% idf_class_name(self, private, all)
}
# }}}
# idf_is_valid_object_id {{{
idf_is_valid_object_id <- function (self, private, id) {
    assert_that(are_count(id))
    id %in% private$m_idf_tbl$object$object_id
}
# }}}
# idf_is_valid_object_name {{{
idf_is_valid_object_name <- function (self, private, name) {
    assert_that(is.character(name))
    stri_trans_tolower(name) %in% private$m_idf_tbl$object[!is.na(object_name), object_name_lower]
}
# }}}
# idf_is_unsaved {{{
idf_is_unsaved <- function (self, private) {
    private$m_log$unsaved
}
# }}}
# idf_definition {{{
idf_definition <- function (self, private, class) {
    idd_object(self, private, class)
}
# }}}
# idf_object {{{
idf_object <- function (self, private, which) {
    obj <- t_object_data(private$m_idf_tbl$object, class = NULL, object = which,
        cols = c("class_id", "object_id", "object_name")
    )

    res <- apply2(obj$object_id, obj$class_id, private$m_idfobj_gen$new)
    setattr(res, "names", underscore_name(obj$object_name))
    res
}
# }}}
# idf_object_in_class {{{
idf_object_in_class <- function (self, private, class) {
    obj <- t_object_data(private$m_idf_tbl$object, class,
        cols = c("class_id", "object_id", "object_name")
    )

    res <- apply2(obj$object_id, obj$class_id, private$m_idfobj_gen$new)
    setattr(res, "names", underscore_name(obj$object_name))
    res
}
# }}}
# idf_search_object {{{
idf_search_object <- function (self, private, pattern, class = NULL, ...) {
    if (!is.null(class) && anyDuplicated(class)) {
        abort("error_search_object_dup_class",
            "Class should not contain any duplication.", class = class
        )
    }

    obj <- t_object_data(private$m_idf_tbl$object, class,
        cols = c("class_id", "object_id", "object_name")
    )

    obj <- obj[grepl(pattern, object_name, ...)]

    if (!nrow(obj)) {
        message("No matched result found.")
        return(invisible())
    }

    res <- apply2(obj$object_id, obj$class_id, private$m_idfobj_gen$new)
    setattr(res, "names", underscore_name(obj$object_name))
    res
}
# }}}
# idf_dup {{{
idf_dup <- function (self, private, ...) {
    dup <- t_dup_object(private$m_idd_tbl, private$m_idf_tbl, ...)

    private$m_idf_tbl$object <- ins_dt(private$m_idf_tbl$object, dup$object, "object_id")
    private$m_idf_tbl$value <- ins_dt(private$m_idf_tbl$value, dup$value, "value_id")
    private$m_idf_tbl$reference <- ins_dt(private$m_idf_tbl$reference, dup$reference, "value_id")

    # log
    self
}
# }}}
# idf_dup_object {{{
idf_dup_object <- function (self, private, object, new_name = NULL) {
    warning("`$dup_object()` is deprecated. Please use `$dup()` instead.", call. = FALSE)
    idf_dup(self, private, setattr(object, "names", new_name))
}
# }}}
# idf_add {{{
idf_add <- function (self, private, ..., .default = TRUE, .all = FALSE) {
    add <- t_add_object(private$m_idd_tbl, private$m_idf_tbl, ..., .default = .default, .all = .all)

    private$m_idf_tbl$object <- ins_dt(private$m_idf_tbl$object, add$object, "object_id")
    private$m_idf_tbl$value <- ins_dt(private$m_idf_tbl$value, add$value, "value_id")
    private$m_idf_tbl$reference <- add$reference

    # log
    self
}
# }}}
# idf_add_object {{{
idf_add_object <- function (self, private, class, value = NULL, comment = NULL, default = TRUE, all = FALSE) {
    warning("`$add_object()` is deprecated. Please use `$add()` instead.", call. = FALSE)
    input <- old_input(class, value, comment, "add")
    idf_add(self, private, input, .default = default, .all = all)
}
# }}}
# idf_set {{{
idf_set <- function (self, private, ..., .default = TRUE) {
    set <- t_set_object(private$m_idd_tbl, private$m_idf_tbl, ..., .default = .default)

    private$m_idf_tbl$object <- ins_dt(private$m_idf_tbl$object, set$object, "object_id")
    private$m_idf_tbl$value <- ins_dt(private$m_idf_tbl$value, set$value, "value_id")
    private$m_idf_tbl$reference <- set$reference
    self
}
# }}}
# idf_set_object {{{
idf_set_object <- function (self, private, object, value, comment, default) {
    warning("`$set_object()` is deprecated. Please use `$set()` instead.", call. = FALSE)
    input <- old_input(class, value, comment, "set")
    idf_set(self, private, input, .default = default)
}
# }}}
# idf_validate {{{
idf_validate <- function (self, private, level = eplusr_option("validate_level")) {
    validate_on_level(private$m_idd_tbl, private$m_idf_tbl, level = level)
}
# }}}
# idf_print {{{
idf_print <- function (self, private, plain = FALSE) {
    if (plain) {
        cli::cat_line(i_object_string(self, private), col = "green")
    } else {
        cli::cat_rule(crayon::bold("EnergPlus Input Data File"), col = "green", line = 2)

        if (is.null(private$m_path)) path <- crayon::bold$bgRed("NOT LOCAL") else path <- surround(private$m_path)

        cli::cat_bullet(c(
            paste0(crayon::bold("Path"), ": ", path),
            paste0(crayon::bold("Version"), ": ", surround(private$m_version))
        ), col = "cyan", bullet_col = "cyan")

        count <- private$m_idf_tbl$object[, list(num_obj = .N), by = class_id][
            private$m_idd_tbl$class, on = "class_id", nomatch = 0L][
            private$m_idd_tbl$group, on = "group_id", nomatch = 0L][
            , list(group_name, class_name, num_obj)]

        max_num <- count[, max(num_obj)]
        count[, num_str := paste0("[", lpad(num_obj, "0"), "]")]
        count[, grp := ""]
        count[count[, .I[1L], by = list(group_name)]$V1,
            grp := crayon::bold$green(paste0("\nGroup: ", surround((group_name)), "\n", cli::rule(), "\n"))]
        out <- count[, paste0(grp, crayon::cyan(num_str), " ", crayon::cyan(class_name))]

        cli::cat_line(out)
    }
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
# [[.Idf {{{
'[[.Idf' <- function(x, i) {
    if (is_string(i)) {
        funs <- setdiff(ls(x), "initialize")
        if (i %in% funs) {
            NextMethod()
        } else {
            in_nm <- underscore_name(i)

            self <- .subset2(.subset2(x, ".__enclos_env__"), "self")
            priv <- .subset2(.subset2(x, ".__enclos_env__"), "private")

            all_nm <- i_class_name(self, priv, type = "idf")

            all_nm_u <- underscore_name(all_nm)

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
            in_nm <- underscore_name(name)

            self <- .subset2(.subset2(x, ".__enclos_env__"), "self")
            priv <- .subset2(.subset2(x, ".__enclos_env__"), "private")

            all_nm <- i_class_name(self, priv, type = "idf")

            all_nm_u <- underscore_name(all_nm)

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

# new_idf {{{
new_idf <- function (ver = "latest") {
    ver <- standardize_ver(ver)
    text <- paste0("Version,", ver, ";\n")
    read_idf(text, ver)
}
# }}}
