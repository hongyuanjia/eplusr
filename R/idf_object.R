#' @importFrom R6 R6Class
#' @include impl-idfobj.R
NULL

#' Create and Modify an EnergyPlus Object
#'
#' `IdfObject` is an abstraction of a single object in an [Idf]. It provides
#' more detail methods to modify object values and comments. An `IdfObject`
#' object can be created using function [idf_object()] or from methods of a
#' parent [Idf] object, using `$object()`, `$objects_in_class()` and equivalent.
#'
#' @section Usage:
#' \preformatted{
#' idfobj <- model$object(which)
#' idfobj <- idf_object(model, which, class = NULL)
#' idfobj$version()
#' idfobj$id()
#' idfobj$name()
#' idfobj$definition()
#' idfobj$comment(comment, append = TRUE, width = 0L)
#' idfobj$value(which = NULL, all = FALSE, simplify = FALSE, unit = FALSE)
#' idfobj$value_possible(which = NULL, type = c("auto", "default", "choice", "range", "source"))
#' idfobj$FieldName
#' idfobj[[Field]]
#' idfobj$set(..., .defaults = TRUE)
#' idfobj$FieldName <- Value
#' idfobj[[Field]] <- Value
#' idfobj$value_relation(which = NULL, direction = c("all", "ref_to", "ref_by", "node"), recursive = FALSE, depth = 1L)
#' idfobj$ref_to_object(which = NULL, class = NULL, recursive = FALSE, depth = 1L)
#' idfobj$ref_by_object(which = NULL, class = NULL, recursive = FALSE, depth = 1L)
#' idfobj$ref_to_node(which = NULL, class = NULL, recursive = FALSE, depth = 1L)
#' idfobj$has_ref_to(which = NULL, class = NULL)
#' idfobj$has_ref_by(which = NULL, class = NULL)
#' idfobj$has_ref_node(which = NULL, class = NULL)
#' idfobj$has_ref(which)
#' idfobj$validate(level = eplusr_option("validate_level"))
#' idfobj$is_valid(level = eplusr_option("validate_level"))
#' idfobj$to_table(all = FALSE, unit = TRUE, wide = FALSE, string_value = TRUE)
#' idfobj$to_string(comment = TRUE, leading = 4L, sep_at = 29L, all = FALSE)
#' idfobj$print(comment = TRUE, auto_sep = FALSE, brief = FALSE)
#' print(iddobj)
#' }
#'
#' @section Basic Info:
#'
#' ```
#' idfobj <- model$object(which)
#' idfobj <- idf_object(model, which, class = NULL)
#' idfobj$version()
#' idfobj$id()
#' idfobj$name()
#' idfobj$group_name()
#' idfobj$class_name()
#' ```
#'
#' `$version()` returns the version of parent [Idf] object in a
#' [numeric_version][base::numeric_version()] format.
#'
#' `$id()` returns current `IdfObject` ID.
#'
#' `$name()` returns current `IdfObject` name. If the class does not have name
#' attribute, `NA` is returned.
#'
#' `$group_name()` returns the group name of current [IdfObject] belongs to.
#'
#' `$class_name()` returns the class name of current [IdfObject] belongs to.
#'
#' **Arguments**:
#'
#' * `model`: An [Idf] object.
#' * `class`: A single string of valid class name.
#' * `which`: A valid object ID (an integer) or name (a string).
#'
#' @section Definition:
#' ```
#' idfobj$definition()
#' ```
#'
#' `$definition()` returns an [IddObject] of current class. [IddObject] contains
#' all data used for parsing and creating an [IdfObject]. For details, please
#' see [IddObject] class.
#'
#' @section Getting and Setting Comments:
#' ```
#' idfobj$comment(comment, append = TRUE, width = 0L)
#' ```
#' `$comment()` returns current `IdfObject` comment if `comment` is not given,
#' or modifies current `IdfObject` comment if `comment` is given.
#'
#' **Arguments**
#'
#' * `comment`: A character vector.
#'    * If missing, current comments are returned. If there is no comment in
#'    current `IdfObject`, `NULL` is returned.
#'    * If `NULL`, all comments in current `IdfObject` is deleted.
#'    * If a character vector, it is inserted as comments depending on the
#'    `append` value.
#' * `append`: Only applicable when `commment` is a character vector. Default: `FALSE`.
#'    * If `NULL`, existing comments is deleted before adding `comment`.
#'    * If `TRUE`, comment will be appended to existing comments.
#'    * If `FALSE`, `comment` is prepended to existing currents.
#' * `width`: A positive integer giving the target width for wrapping inserted
#'   `comment`.
#'
#' @section Get Field Values:
#' \preformatted{
#' idfobj$value(which = NULL, all = FALSE, simplify = FALSE, unit = FALSE)
#' idfobj$value_possible(which = NULL, type = c("auto", "default", "choice", "range", "source"))
#' idfobj$FieldName
#' idfobj[[Field]]
#' }
#'
#' `$value()` takes an integer vector of valid field indexes or a character
#' vector of valid field names, and returns a named list containing values of
#' specified fields when `simplify` is `FALSE` and a character vector when
#' `simplify` is `TRUE`.
#'
#' eplusr also provides custom S3 method of `$` and \code{[[} which make
#' it more convenient to get a single value of current `IdfObject`. Basically,
#' `idfobj$FieldName` and \code{idfobj[[Field]]} is equivalent to
#' \code{idfobj$value(FieldName)[[1]]} and \code{idfobj$value(Field)[[1]]}.
#'
#' `$possible_value()` takes an integer vector of valid field indexes or a character
#' vector of valid field names, and returns all possible values for specified
#' fields. For a specific field, there are 5 types of possible values:
#'
#' * `auto`: Whether the field can be filled with `Autosize` and
#'   `Autocalculate`. This field attribute can also be retrieved using
#'   `idfobj$definition()$is_autosizable()` and
#'   `idfobj$definition()$is_autosizable()`.
#' * `default`: The default value. This value can also be retrieved using
#'   `idfobj$defintion()$field_default()`.
#' * `choice`: The choices which the field can be set. This value can also be
#'   retrieved using `idfobj$definition()$field_choice()`.
#' * `range`: The range which the field value should fall in. This range can
#'   also be retrieved using `idfobj$definition()$field_range()`.
#' * `source`: All values from other objects that current field can refer to.
#'
#' `$value_possible()` returns an `IdfValuePossible` object which is a
#' [data.table][data.table::data.table()] with at most 15 columns:
#'
#' * `class_id`: index of class that current `IdfObject` belongs to
#' * `class_name`: name of class that current `IdfObject` belongs to
#' * `object_id`: ID of current `IdfObject`
#' * `object_name`: name of current `IdfObject`
#' * `field_id`: indexes (at Idd level) of object fields specified
#' * `field_index`: indexes of object fields specified
#' * `field_name`: names (without units) of object fields specified
#' * `value_id`: value indexes (at Idf level) of object fields specified
#' * `value_chr`: values (converted to characters) of object fields specified
#' * `value_num`: values (converted to numbers in SI units) of object fields
#'    specified.
#' * `auto`: Exists only when `"auto"` is one of `type`. Character type.
#'   Possible values are: `"Autosize"`, `"Autocalculate"` and `NA` (if current
#'   field is neither `autosizable` nor `autocalculatable`).
#' * `default`: Exists only when `"default"` is one of `type`. List type. The
#'   default value of current field. The value is converted into number if
#'   corresponding field type yells so. Note that if current field is a numeric
#'   field but the default value is `"Autosize"` or `"Autocalculate"`, it is
#'   left as it is, leaving the returned type being a string instead of a
#'   number.
#' * `range`: Exists only when `"range"` is one of `type`. List type. The range
#'   that field value should fall in. Every range has four components: `minimum`
#'   (lower limit), `lower_incbounds` (`TRUE` if the lower limit should be
#'   included), `maximum` (upper limit), and `upper_incbounds` (`TRUE` if the
#'   upper limit should be included). For fields of character type, empty lists
#'   are returned. For fields of numeric types with no specified ranges,
#'   `minimum` is set to `-Inf`, `lower_incbounds` is set to FALSE, `upper` is
#'   set to `Inf`, and `upper_incbounds` is set to FALSE. The field range is
#'   printed in number interval denotation.
#' * `source`: Exists only when `"source"` is one of `type`. List type. Each
#'   element is a character vector which includes all values from other objects
#'   that current field can use as sources and refers to.
#'
#' **Arguments**
#'
#' * `which`: An integer vector of field indexes or a character vector of field
#'   names.
#' * `all`: If `TRUE`, values of all possible fields in current class the
#'   `IdfObject` belongs to are returned. Default: `FALSE`
#' * `simplify`: If `TRUE`, values of fields are converted into characters
#'   and the converted character vector is returned.
#' * `FieldName`: A single length character vector of one valid field name where
#'     all characters except letters and numbers are replaced by underscores.
#' * `Field`: A single length character vector of one valid field name or a
#'     single length integer vector of one valid field index. Same as above,
#'     field names should be given in a style where all characters except
#'     letters and numbers are replaced by underscores.
#' * `type`: A character vector. What types of possible values should be
#'   returned. Should be one of or a combination of `"auto"`, `"default"`,
#'   `"choice"`, `"range"` and `"source"`. Default: All of those.
#'
#' @section Set Field Values:
#' \preformatted{
#' idfobj$set(..., .default = TRUE)
#' idfobj$FieldName <- Value
#' idfobj[[Field]] <- Value
#' }
#'
#' `$set()` takes new field value definitions in `field = value` format or in a
#' single list format, sets new values for fields specified, and returns the
#' modified [IdfObject]. Unlike `$set()` method in [Idf] class, the special
#' element `.comment` is **not allowed**. To modify object comments, please use
#' `$comment()`.
#'
#' **Note**:
#'
#' * Only one single list is allowed, e.g. `idfobj$set(lst1)` where `lst1 <-
#'   list(field1 = value1)` is allowed, but `idfobj$set(lst1, lst2)` is not.
#' * You can delete a field by assigning `NULL` to it, e.g. `iddobj$set(fld =
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
#' eplusr also provides custom S3 method of `$<-` and
#' \code{[[<-} which makes it more convenient to set a single field value of an
#' `IdfObject`. Basically, `idfobj$FieldName <- value` and \code{idfobj[[Field]]
#' <- value} is equivalent to `idfobj$set(FieldName = value)` and
#' `idfobjset(Field = value)`.
#'
#' **Arguments**:
#'
#' * `...`: New field value definitions in `field = value` format or a single
#'   list in format `list(field1 = value1, field2 = value2)`.
#' * `.default`: If `TRUE`, default values are used for those blank fields if
#'    possible. Default: `TRUE`.
#' * `FieldName`: A single length character vector of one valid field name where
#'     all characters except letters and numbers are replaced by underscores.
#' * `Field`: A single length character vector of one valid field name or a
#'     single length integer vector of one valid field index. Same as above,
#'     field names should be given in a style where all characters except
#'     letters and numbers are replaced by underscores.
#' * `Value`: A single length vector of value to set.
#'
#' @section Field Value Relation:
#' \preformatted{
#' idfobj$value_relation(which = NULL, direction = c("all", "ref_to", "ref_by", "node"), recursive = FALSE, depth = 1L)
#' idfobj$ref_to_object(which = NULL, class = NULL, recursive = FALSE, depth = 1L)
#' idfobj$ref_by_object(which = NULL, class = NULL, recursive = FALSE, depth = 1L)
#' idfobj$ref_to_node(which = NULL, class = NULL, recursive = FALSE, depth = 1L)
#' idfobj$has_ref_to(which = NULL, class = NULL)
#' idfobj$has_ref_by(which = NULL, class = NULL)
#' idfobj$has_ref_node(which = NULL, class = NULL)
#' idfobj$has_ref(which)
#' }
#'
#' Many fields in [Idd] can be referred by others. For example, the `Outside
#' Layer` and other fields in `Construction` class refer to the `Name` field
#' in `Material` class and other material related classes. Here it means that
#' the `Outside Layer` field **refers to** the `Name` field and the `Name` field
#' is **referred by** the `Outside Layer`. In EnergyPlus, there is also a
#' special type of field called `Node`, which together with `Branch` and
#' `BranchList` define the topography of the HVAC connections. A outlet node of
#' a component can be referred by another component as its inlet node, but can
#' also exists independently, such as zone air node.
#'
#' `$value_relation()` provides a simple interface to get this kind of
#' relation. It takes field indexes or field names, together a relation
#' direction, and returns an `IdfRelation` object which contains data presenting
#' such relation described above. For instance, if
#' `idfobj$value_relation("Name", "ref_by")` gives results below:
#'
#' ```
#' -- Referred by Others ------------------------
#'   \- 1: "WALL-1";      !- Name
#'      ^~~~~~~~~~~~~~~~~~~~~~~~~
#'      \- Class: <BuildingSurface:Detailed>
#'         \- Object [ID:3] <WALL-1PF>
#'            \- 3: "WALL-1";      !- Construction Name
#' ```
#'
#' This means that the value `"WALL-1"` of field `Name` is referred by field
#' `Construction Name` in a surface named `WALL-1PF`. All those objects can be
#' futher easily extracted using `$ref_by_object()` method.
#'
#' Note that `$value_relation()` shows all fields specified, even some of them
#' may do not have relation.
#'
#' `$ref_to_object()` takes an integer vector of field indexes or a character
#' vector of field names, and returns a list of `IdfObject`s that specified
#' fields refer to.
#'
#' `$ref_by_object()` takes an integer vector of field indexes or a character
#' vector of field names, and returns a list of `IdfObject`s that refer to
#' specified fields.
#'
#' `$ref_to_node()` takes an integer vector of field indexes or a character
#' vector of field names, and returns a list of `IdfObject`s whose nodes are
#' referred by specified fields.
#'
#' `$has_ref_to()` takes an integer vector of field indexes or a character
#' vector of field names, and returns a logical vector showing whether specified
#' fields refer to other object values or not.
#'
#' `$has_ref_by()` takes an integer vector of field indexes or a character
#' vector of field names, and returns a logical vector showing whether there are
#' other object values ref to specified fields.
#'
#' `$has_ref_node()` takes an integer vector of field indexes or a character
#' vector of field names, and returns a logical vector showing whether specified
#' fields refer to other objects' nodes.
#'
#' `$has_ref()` takes an integer vector of field indexes or a character
#' vector of field names, and returns a logical vector showing whether there are
#' other object values ref to specified field values or specified field values
#' refer to other object values or specified field values refer to other
#' objects' nodes.
#'
#' **Arguments**:
#'
#' * `which`: An integer vector of field indexes or a character vector of field
#'   names.
#' * `class`: A character vector of class names.
#' * `direciton`: The relation direction to extract. Should be either `"all"`,
#'   `"ref_to"` or "ref_by".
#' * `recursive`: If `TRUE`, the relation is searched recursively. A simple
#'   example of recursive reference: one material named `mat` is referred by a
#'   construction named `const`, and `const` is also referred by a surface named
#'   `surf`.
#' * `depth`: Only applicable when `recursive` is `TRUE`. This is a depth to
#'   when searching value relations recursively. If `NULL`, all recursive
#'   relations are returned. Default: `1`.
#'
#' @section Validation:
#'
#' ```
#' idfobj$validate(level = eplusr_option("validate_level"))
#' idfobj$is_valid(level = eplusr_option("validate_level"))
#' ```
#'
#' `$validate()` checks if there are errors in values in current `IdfObject`
#' under specified validation level and returns an `IdfValidity` object which
#' contains data of invalid field values. Different validation result examples
#' are shown below:
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
#' `$is_valid()` returns `TRUE` if there is no error in current `IdfObject`
#' object under specified validation level and `FALSE` otherwise.
#'
#' Underneath, an `IdfValidity` object which `$validate()` returns is a list of
#' 13 element. For details about the underlying structure of `IdfValidity`,
#' please `$validate()` in [Idf] class.
#'
#' @section Data Extraction:
#'
#' ```
#' idfobj$to_table(string_value = TRUE, unit = TRUE, wide = FALSE, all = FALSE)
#' idfobj$to_string(comment = TRUE, leading = 4L, sep_at = 29L, all = FALSE)
#' ```
#'
#' `$to_table()` returns a [data.table][data.table::data.table()] that contains
#' core data of current [IdfObject]. It has 6 columns:
#'
#' * `id`: Integer type. Object IDs.
#' * `name`: Character type. Object names.
#' * `class`: Character type. Current class name.
#' * `index`: Integer type. Field indexes.
#' * `field`: Character type. Field names.
#' * `value`: Character type if `string_value` is `TRUE` or list type if
#'   `string_value` is `FALSE.` Field values.
#'
#' `$to_string()` returns the text format of an `IdfObject`.
#'
#' **Arguments**:
#'
#' * `string_value`: If `TRUE`, all field values are returned as character. If
#'   `FALSE`, `value` column in returned [data.table][data.table::data.table()]
#'   is a list column with each value stored as corresponding type. Note that if
#'   the value of numeric field is set to `"Autosize"` or `"Autocalculate"`, it
#'   is left as it is, leaving the returned type being a string instead of a
#'   number.  Default: `TRUE`.
#' * `unit`: Only applicable when `string_value` is `FALSE`. If `TRUE`, values
#'   of numeric fields are assigned with units using [units::set_units()] if
#'   applicable. Default: `FALSE`.
#' * `wide`: If `TRUE`, a wide table will be returned, i.e. first three columns
#'   are always `id`, `name` and `class`, and then every field in a separate
#'   column. Default: `FALSE`.
#' * `comment`: If `FALSE`, all comments will not be included. Default: `TRUE`.
#' * `leading`: Leading spaces added to each field. Default: `4L`.
#' * `sep_at`: The character width to separate value string and field string.
#'   Default: `29L` which is the same as IDF Editor.
#' * `all`: If `TRUE`, values of all possible fields in current class the
#'   `IdfObject` belongs to are returned. Default: `FALSE`
#'
#' @section Print:
#'
#' ```
#' idfobj$print(comment = TRUE, auto_sep = FALSE)
#' print(idfobj)
#' ```
#'
#' `$print()` prints the `IdfObject`. Basically, the print output can be divided
#' into 3 parts:
#'
#' * OBJECT: Class name, object id and name (if applicable).
#' * COMMENTS: Object comments if exist.
#' * VALUES: fields and values of current `IdfObject`. Required fields are marked
#'   with start `*`. String values are quoted. Numeric values are printed as
#'   they are. Blank string values are printed as `<"Blank">` and blank number
#'   values are printed as `<Blank>`.
#'
#' **Arguments**
#'
#' * `comment`: If `FALSE`, all comments are not included.
#' * `auto_sep`: If `TRUE`, values and field names are separated at the
#'   largest character length of values. Default: `FALSE`.
#'
#' @importFrom R6 R6Class
#' @examples
#' \dontrun{
#' # read an IDF file
#' idf <- read_idf(system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr"),
#'     idd = use_idd(8.8, download = "auto"))
#'
#' # get the IdfObject of material named "C5 - 4 IN HW CONCRETE"
#' mat <- idf$Material[["C5 - 4 IN HW CONCRETE"]]
#'
#' # get object ID
#' mat$id()
#'
#' # get object name
#' mat$name()
#'
#' # NA will be returned if the class does not have name attribute. For example,
#' # "Version" class
#' idf$Version$name()
#'
#' # get underlying IddObject of current class
#' mat$definition()
#'
#' # get object comments
#' mat$comment()
#'
#' # add new object comments
#' mat$comment(c("This is a material named `WD01`", "This object has an ID of 47"))
#' mat$comment()
#'
#' # append new comments
#' mat$comment("This is an appended comment")
#' mat$comment()
#'
#' # prepend new comments
#' mat$comment("This is a prepended comment", append = FALSE)
#' mat$comment()
#'
#' # wrap long comments
#' mat$comment("This is a very long comment that is needed to be wrapped.", width = 30)
#' mat$comment()
#'
#' # delete old comments and add new one
#' mat$comment("This is the only comment", append = NULL)
#' mat$comment()
#'
#' # delete all comments
#' mat$comment(NULL)
#' mat$comment()
#'
#' # get all existing field values
#' str(mat$value())
#'
#' # get values of field 1, 3, 5
#' str(mat$value(c(1, 3, 5)))
#'
#' # get character format values instead of a named list
#' mat$value(c(1, 3, 5), simplify = TRUE)
#'
#' # get values of all field even those that are not set
#' str(idf$Zone$`ZONE ONE`$value())
#'
#' str(idf$Zone$`ZONE ONE`$value(all = TRUE))
#'
#' # get field values using shortcuts
#' mat$Roughness
#' mat[["Specific_Heat"]]
#' mat[c(1,2)]
#' mat[c("Name", "Density")]
#'
#' # set field values
#' mat$set(name = "new_name", Thickness = 0.02)
#' mat[c("Name", "Thickness")]
#'
#' # When `default` argument is set to TRUE and input field values are empty, i.e.
#' # NULL, the field values will be reset to defaults.
#' mat[c("Thermal Absorptance", "Solar Absorptance")]
#'
#' mat$set(visible_absorptance = NULL, Solar_Absorptance = NULL, .default = TRUE)
#' mat[c("Visible Absorptance", "Solar Absorptance")]
#'
#' # set field values using shortcuts
#' mat$Name <- "another_name"
#' mat$Name
#' mat[["Thickness"]] <- 0.019
#' mat$Thickness
#'
#' # check validate
#' mat$validate()
#' mat$is_valid()
#'
#' # if we set density to a negative number
#' mat$definition()$field_range("Density")
#' eplusr_option(validate_level = "none") # have to set validate to "none" to do so
#' mat$Density <- -1
#' eplusr_option(validate_level = "final") # change back to "final" validate level
#' mat$is_valid()
#' # get other objects that this object refereces
#' mat$ref_to_object() # not referencing other objects
#' mat$has_ref_to()
#'
#' # get other objects that reference this object
#' mat$ref_by_object() # referenced by construction "FLOOR"
#' names(mat$ref_by_object())
#'
#' mat$has_ref_by()
#'
#' # check if having any referenced objects or is referenced by other objects
#' mat$has_ref()
#'
#' # get all object data in a data.table format without field units
#' str(mat$to_table(unit = FALSE))
#'
#' # get all object data in a data.table format where all field values are put in a
#' # list column and field names without unit
#' str(mat$to_table(string_value = FALSE, unit = FALSE))
#'
#' # get all object data in a data.table format, including tailing empty fields
#' str(idf$Zone$`ZONE ONE`$to_table(all = TRUE))
#'
#' # get all object data in a data.table format where each field becomes a column
#' str(mat$to_table(wide = TRUE))
#'
#' # get string format object
#' mat$to_string()
#'
#' # get string format of object, and decrease the space between field values and
#' # field names
#' mat$to_string(sep_at = 15)
#'
#' # get string format of object, and decrease the leading space of field values
#' mat$to_string(leading = 0)
#'
#' # print the object without comment
#' mat$print(comment = FALSE)
#'
#' # print the object, and auto separate field values and field names at the
#' # largetst character length of field values
#' mat$print(auto_sep = TRUE)
#' }
#' @docType class
#' @name IdfObject
#' @seealso [Idf] class
#' @author Hongyuan Jia
NULL

#' Create an `IdfObject` object.
#'
#' `idf_object()` takes a parent `Idf` object, an object name or class name, and
#' returns a corresponding [IdfObject].
#'
#' If `object` is not given, an empty [IdfObject] of specified class is created,
#' with all field values filled with defaults, if possible.  Note that
#' validation is performed when creating, which means that an error may occur if
#' current [validate level][level_checks()] does not allow empty required fields.
#'
#' The empty [IdfObject] is directly added into the parent [Idf] object. It is
#' recommended to use `$validate()` method in [IdfObject] to see what kinds of
#' further modifications are needed for those empty fields and use `$set()`
#' method to set field values.
#'
#' @param parent An [Idf] object.
#' @param object A valid object ID (an integer) or name (a string). If `NULL`
#' and `class` is not `NULL`, an empty [IdfObject] is created with all fields
#' fill with default values if possible. Default: `NULL`.
#' @param class A valid class name (a string). If `object` is not `NULL`,
#' `class` is used to further specify what class is the target object belongs
#' to. If `object` is `NULL`, an empty [IdfObject] of `class` is created.
#' @return An [IdfObject] object.
#' @export
#' @examples
#' \dontrun{
#' model <- read_idf(system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr"))
#'
#' # get an IdfObject using object ID
#' idf_object(model, 14)
#'
#' # get an IdfObject using object name (case-insensitive)
#' idf_object(model, "zone one")
#'
#' # `class` argument is useful when there are objects with same name in
#' # different class
#' idf_object(model, "zone one", "Zone")
#'
#' # create a new zone
#' eplusr_option(validate_level = "draft")
#' zone <- idf_object(model, class = "Zone")
#' zone
#' eplusr_option(validate_level = "final")
#' zone$validate()
#' }
#' @export
# idf_object {{{
idf_object <- function (parent, object = NULL, class = NULL) {
    if (missing(parent) || !is_idf(parent)) {
        abort("error_idfobject_missing_parent",
            paste("IdfObject can only be created based a parent Idf object.",
                "Please give `parent`, which should be an Idf object.")
        )
    }

    idd_env <- ._get_private(parent)$idd_env()
    idf_env <- ._get_private(parent)$idf_env()

    # add an empty object
    if (is.null(object)) {
        assert(!is.null(class),
            msg = paste0("`class` must be given when `object` is not.")
        )

        assert(is_string(class))

        # add field property
        prop <- c("units", "ip_units", "default_chr", "default_num", "is_name",
            "required_field", "src_enum", "type_enum", "extensible_group"
        )

        cls <- get_idd_class(idd_env, class)
        fld <- get_idd_field(idd_env, cls$class_id, property = prop)

        obj <- set(cls, NULL, c("object_id", "object_name", "object_name_lower", "comment"),
            list(new_id(idf_env$object, "object_id", 1L), NA_character_, NA_character_, list())
        )

        dot <- data.table(rleid = 1L, object_rleid = 1L, dep = 1, dot = class, dot_nm = NA_character_)

        assert_can_do(idd_env, idf_env, dot, obj, action = "add")

        val <- set(fld, NULL, c("value_id", "value_chr", "value_num", "object_id", "defaulted"),
            list(new_id(idf_env$value, "value_id", nrow(fld)),
                 NA_character_, NA_real_, obj$object_id,
                 TRUE
            )
        )

        # assign default values
        val <- assign_default_value(val)

        # validate
        assert_valid(idd_env, idf_env, obj, val, action = "add")

        idf_env$object <- append_dt(idf_env$object, obj)
        idf_env$value <- append_dt(idf_env$value, val)

        object <- obj$object_id
        class <- obj$class_id

        # validate
        assert_valid(idd_env, idf_env, obj, val, action = "add")

        verbose_info(
            paste0("New empty object [ID:", obj$object_id, "] in class ",
                surround(obj$class_name), " created."
            )
        )
    } else {
        obj <- get_idf_object(idd_env, idf_env, class, object, ignore_case = TRUE)

        object <- obj$object_id
        class <- obj$class_id
    }

    obj <- IdfObject$new(object, class, parent)

    add_idfobj_field_bindings(obj)
}
# }}}

# add_idfobj_field_bindings {{{
add_idfobj_field_bindings <- function (obj, field_index = NULL, update = FALSE) {
    # create active bindings
    # get first 30 field names in current IDD class
    env <- .subset2(obj, ".__enclos_env__")
    self <- .subset2(env, "self")
    private <- .subset2(env, "private")

    fld_id <- private$idf_env()$value[J(private$m_object_id), on = "object_id", field_id]
    if (!is.null(field_index)) {
        fld_id <- fld_id[field_index]
    }
    fld_nm <- private$idd_env()$field[J(fld_id), on = "field_id", field_name]

    get_field_value <- function (env, self, private, field, value) {
        fun <- function (value) {
            field <- field
            if (missing(value)) {
                self$value(field)[[1L]]
            } else {
                names(value) <- field
                self$set(c(value))
                invisible(self)
            }
        }
        environment(fun) <- env
        body(fun)[[2]][[3]] <- field
        fun
    }

    # move deleted field bindings
    if (update && length(setdiff(ls(obj, pattern = "^[A-Z]"), fld_nm))) {
        rm(list = setdiff(ls(obj, pattern = "^[A-Z]"), fld_nm), envir = obj)
    }

    # skip if nothing to add
    if (!length(setdiff(fld_nm, ls(obj)))) return(obj)

    for (i in setdiff(fld_nm, ls(obj))) {
        makeActiveBinding(i, get_field_value(env, self, private, i, value), obj)
    }

    obj
}
# }}}

# IdfObject {{{
IdfObject <- R6::R6Class(classname = "IdfObject", lock_objects = FALSE,
    public = list(
        # INITIALIZE {{{
        initialize = function (object, class = NULL, parent) {
            if (missing(parent) || !is_idf(parent)) {
                abort("error_idfobject_missing_parent",
                    paste("IdfObject can only be created based a parent Idf object.",
                        "Please give `parent`, which should be an Idf object.")
                )
            } else {
                private$m_parent <- parent
            }
            assert(is_count(object))
            if (!is.null(class)) {
                assert(is_count(class))
            } else {
                class <- get_idf_object(private$idd_env(), private$idf_env(), NULL, object)$class_id
            }

            private$m_object_id <- object
            private$m_class_id <- class
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        version = function ()
            idfobj_version(self, private),

        id = function ()
            idfobj_id(self, private),

        name = function ()
            idfobj_name(self, private),

        group_name = function ()
            idfobj_group_name(self, private),

        class_name = function ()
            idfobj_class_name(self, private),

        definition = function ()
            idfobj_definition(self, private),

        comment = function (comment, append = TRUE, width = 0L)
            idfobj_comment(self, private, comment, append, width),

        get_comment = function ()
            idfobj_get_comment(self, private),

        set_comment = function (comment, append = TRUE, width = 0L)
            idfobj_set_comment(self, private, comment, append, width),

        value = function (which = NULL, all = FALSE, simplify = FALSE, unit = FALSE)
            idfobj_value(self, private, which, all, simplify, unit),

        get_value = function (which = NULL, all = FALSE, simplify = FALSE, unit = FALSE)
            idfobj_get_value(self, private, which, all, simplify, unit),

        set = function (..., .default = TRUE)
            idfobj_set(self, private, ..., .default = .default),

        set_value = function (..., .default = TRUE)
            idfobj_set_value(self, private, ..., .default = .default),

        value_possible = function (which = NULL, type = c("auto", "default", "choice", "range", "source"))
            idfobj_value_possible(self, private, which, type),

        possible_value = function (which = NULL)
            idfobj_possible_value(self, private, which),

        validate = function (level = eplusr_option("validate_level"))
            idfobj_validate(self, private, level),

        is_valid = function (level = eplusr_option("validate_level"))
            idfobj_is_valid(self, private, level),

        value_relation = function (which = NULL, direction = c("all", "ref_to", "ref_by", "node"), recursive = FALSE, depth = 1L)
            idfobj_value_relation(self, private, which, match.arg(direction), recursive, depth),

        ref_to_object = function (which = NULL, class = NULL, recursive = FALSE, depth = 1L)
            idfobj_ref_to_object(self, private, which, class, recursive, depth),

        ref_from_object = function ()
            idfobj_ref_from_object(self, private),

        ref_by_object = function (which = NULL, class = NULL, recursive = FALSE, depth = 1L)
            idfobj_ref_by_object(self, private, which, class, recursive, depth),

        ref_to_node = function (which = NULL, class = NULL, recursive = FALSE, depth = 1L)
            idfobj_ref_to_node(self, private, which, class, recursive, depth),

        has_ref_to = function (which = NULL, class = NULL)
            idfobj_has_ref_to(self, private, which, class),

        has_ref_from = function ()
            idfobj_has_ref_from(self, private),

        has_ref_by = function (which = NULL, class = NULL)
            idfobj_has_ref_by(self, private, which, class),

        has_ref_node = function (which = NULL, class = NULL)
            idfobj_has_ref_node(self, private, which, class),

        has_ref = function (which = NULL)
            idfobj_has_ref(self, private, which),

        to_table = function (string_value = TRUE, unit = TRUE, wide = FALSE, all = FALSE)
            idfobj_to_table(self, private, all, string_value, unit, wide),

        to_string = function (comment = TRUE, leading = 4L, sep_at = 29L, all = FALSE)
            idfobj_to_string(self, private, comment, leading, sep_at, all),

        table = function (all = FALSE, unit = TRUE, wide = FALSE, string_value = TRUE, in_ip)
            idfobj_table(self, private, all, unit, wide, string_value, in_ip),

        string = function (comment = TRUE, leading = 4L, sep_at = 29L)
            idfobj_string(self, private, comment, leading, sep_at),

        print = function (comment = TRUE, auto_sep = TRUE, brief = FALSE)
            idfobj_print(self, private, comment, auto_sep, brief)
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        # shared data from parent Idf object
        m_parent = NULL,
        m_object_id = NULL,
        m_class_id = NULL,
        # }}}

        idf_env = function () {
            ._get_private(private$m_parent)$m_idf_env
        },

        idd_env = function () {
            ._get_private(private$m_parent)$idd_env()
        },

        log_env = function () {
            ._get_private(private$m_parent)$m_log
        }
    )
)
# }}}

# idfobj_version {{{
idfobj_version <- function (self, private) {
    private$m_parent$version()
}
# }}}
# idfobj_id {{{
idfobj_id <- function (self, private) {
    private$m_object_id
}
# }}}
# idfobj_name {{{
idfobj_name <- function (self, private) {
    private$idf_env()$object[J(private$m_object_id), on = "object_id", object_name]
}
# }}}
# idfobj_group_name {{{
idfobj_group_name <- function (self, private) {
    private$idd_env()$group[
        J(private$idd_env()$class[J(private$m_class_id), on = "class_id", group_id]),
        on = "group_id",
        group_name
    ]
}
# }}}
# idfobj_class_name {{{
idfobj_class_name <- function (self, private) {
    private$idd_env()$class[J(private$m_class_id), on = "class_id", class_name]
}
# }}}
# idfobj_definition {{{
idfobj_definition <- function (self, private) {
    IddObject$new(private$m_class_id, ._get_private(private$m_parent)$m_idd)
}
# }}}
# idfobj_comment {{{
idfobj_comment <- function (self, private, comment, append = TRUE, width = 0L) {
    if (missing(comment)) {
        return(private$idf_env()$object[J(private$m_object_id), on = "object_id"]$comment[[1L]])
    }

    assert(is.atomic(comment), msg = "`comment` should be NULL or a character vector.")
    obj <- set_idfobj_comment(private$idd_env(), private$idf_env(), private$m_object_id,
        comment = comment, append = append, width = width
    )

    log_add_order(private$log_env(), obj$object_id)
    log_unsaved(private$log_env())
    log_new_uuid(private$log_env())

    # update object in parent
    merge_idfobj_data(private$idf_env(), obj, "object")

    self
}
# }}}
# idfobj_get_comment {{{
idfobj_get_comment <- function (self, private) {
    .deprecated_fun("$get_comment()", "$comment()", "IdfObject", "0.10.0")
    idfobj_comment(self, private)
}
# }}}
# idfobj_set_comment {{{
idfobj_set_comment <- function (self, private, comment, append = TRUE, width = 0L) {
    .deprecated_fun("$set_comment()", "$comment()", "IdfObject", "0.10.0")
    idfobj_comment(self, private, comment, append, width)
}
# }}}
# idfobj_value {{{
idfobj_value <- function (self, private, which = NULL, all = FALSE, simplify = FALSE, unit = FALSE) {
    get_idfobj_value(private$idd_env(), private$idf_env(), private$m_object_id, which, all, simplify, unit)
}
# }}}
# idfobj_get_value {{{
idfobj_get_value <- function (self, private, which = NULL, all = FALSE, simplify = FALSE, unit = FALSE) {
    .deprecated_fun("$get_value()", "$value()", "IdfObject", "0.10.0")
    idfobj_value(self, private, which, all, simplify, unit)
}
# }}}
# idfobj_set {{{
idfobj_set <- function (self, private, ..., .default = TRUE) {
    set <- set_idfobj_value(private$idd_env(), private$idf_env(),
        private$m_object_id, ..., .default = .default
    )
    merge_idf_data(private$idf_env(), set, by_object = TRUE)

    # log
    log_add_order(private$log_env(), set$object$object_id)
    log_unsaved(private$log_env())
    log_new_uuid(private$log_env())

    self
}
# }}}
# idfobj_set_value {{{
idfobj_set_value <- function (self, private, ..., .default = TRUE) {
    .deprecated_fun("$set_value()", "$set()", "IdfObject", "0.10.0")
    idfobj_set(self, private, ..., .default = TRUE)
}
# }}}
# idfobj_value_possible {{{
idfobj_value_possible <- function (self, private, which = NULL, type = c("auto", "default", "choice", "range", "source")) {

    get_idfobj_possible(private$idd_env(), private$idf_env(), private$m_object_id, which, type)
}
# }}}
# idfobj_possible_value {{{
idfobj_possible_value <- function (self, private, which = NULL) {
    .deprecated_fun("$possible_value()", "$value_possible()", "IdfObject", "0.10.0")
    idfobj_value_possible(self, private, which)
}
# }}}
# idfobj_validate {{{
idfobj_validate <- function (self, private, level = eplusr_option("validate_level")) {
    obj <- get_idf_object(private$idd_env(), private$idf_env(), object = private$m_object_id)
    val <- get_idf_value(private$idd_env(), private$idf_env(), object = private$m_object_id)
    validate_on_level(private$idd_env(), private$idf_env(), obj, val, level)
}
# }}}
# idfobj_is_valid {{{
idfobj_is_valid <- function (self, private, level = eplusr_option("validate_level")) {
    count_check_error(idfobj_validate(self, private, level)) == 0L
}
# }}}
# idfobj_value_relation {{{
idfobj_value_relation <- function (self, private, which = NULL,
                                   direction = c("all", "ref_to", "ref_by", "node"),
                                   recursive = FALSE, recursive_depth = 1L) {
    direction <- match.arg(direction)

    val <- get_idf_value(private$idd_env(), private$idf_env(),
        object = private$m_object_id, field = which
    )

    get_idfobj_relation(private$idd_env(), private$idf_env(),
        value_id = val$value_id, name = TRUE, direction = direction,
        keep_all = TRUE, by_value = TRUE, recursive = recursive)
}
# }}}
# idfobj_ref_to_object {{{
idfobj_ref_to_object <- function (self, private, which = NULL, class = NULL, recursive = FALSE, recursive_depth = 1L) {
    val <- get_idf_value(private$idd_env(), private$idf_env(),
        object = private$m_object_id, field = which
    )

    # exclude invalid references
    rel <- get_idf_relation(private$idd_env(), private$idf_env(),
        value_id = val$value_id, direction = "ref_to", recursive = recursive
    )[!is.na(src_value_id)]

    # only include specified class
    if (!is.null(class)) {
        add_joined_cols(private$idf_env()$object, rel, c(src_object_id = "object_id"), c(src_class_id = "class_id"))
        cls <- get_idd_class(private$idd_env(), class)
        rel <- rel[J(cls$class_id), on = "src_class_id"]
    }

    if (!nrow(rel)) {
        if (is.null(class)) {
            verbose_info("Target object does not refer to any other object.")
        } else {
            verbose_info("Target object does not refer to any other object in class ",
                collapse(cls$class_name), "."
            )
        }
        return(invisible())
    } else {
        rel <- rel[, list(src_object_id = unique(src_object_id)), by = "object_id"]
        verbose_info("Target object refers to ", nrow(rel), " object(s) [ID:",
            collapse(rel$src_object_id), "].\n"
        )
        res <- apply2(
            rel$src_object_id,
            private$idf_env()$object[J(rel$src_object_id), on = "object_id", class_id],
            IdfObject$new, list(parent = private$m_parent)
        )
        res <- lapply(res, add_idfobj_field_bindings)
        setattr(res, "names", private$idf_env()$object[J(rel$src_object_id), on = "object_id", object_name])
        res
    }
}
# }}}
# idfobj_ref_from_object {{{
idfobj_ref_from_object <- function (self, private) {
    .deprecated_fun("$ref_from_object()", "$ref_to_object()", "IdfObject", "0.10.0")
    idfobj_ref_to_object(self, private)
}
# }}}
# idfobj_ref_by_object {{{
idfobj_ref_by_object <- function (self, private, which = NULL, class = NULL, recursive = FALSE, recursive_depth = 1L) {
    val <- get_idf_value(private$idd_env(), private$idf_env(),
        object = private$m_object_id, field = which
    )

    # exclude invalid references
    rel <- get_idf_relation(private$idd_env(), private$idf_env(),
        value_id = val$value_id, direction = "ref_by", recursive = recursive
    )[!is.na(value_id)]

    # only include specified class
    if (!is.null(class)) {
        add_joined_cols(private$idf_env()$object, rel, "object_id", "class_id")
        cls <- get_idd_class(private$idd_env(), class)
        rel <- rel[J(cls$class_id), on = "class_id"]
    }

    if (!nrow(rel)) {
        if (is.null(class)) {
            verbose_info("Target object is not referred by any other object.")
        } else {
            verbose_info("Target object is not referred by any other object in class ",
                collapse(cls$class_name), "."
            )
        }
        return(invisible())
    } else {
        rel <- rel[, list(object_id = unique(object_id)), by = "src_object_id"]
        verbose_info("Target object is referred by ", nrow(rel), " object(s) [ID:",
            collapse(rel$object_id), "].\n")
        res <- apply2(
            rel$object_id,
            private$idf_env()$object[J(rel$object_id), on = "object_id", class_id],
            IdfObject$new, list(parent = private$m_parent)
        )
        res <- lapply(res, add_idfobj_field_bindings)
        setattr(res, "names", private$idf_env()$object[J(rel$object_id), on = "object_id", object_name])
        res
    }
}
# }}}
# idfobj_ref_to_node {{{
idfobj_ref_to_node <- function (self, private, which = NULL, class = NULL, recursive = FALSE, recursive_depth = 1L) {
    val <- get_idf_value(private$idd_env(), private$idf_env(),
        object = private$m_object_id, field = which
    )

    # exclude invalid references
    rel <- get_idf_node_relation(private$idd_env(), private$idf_env(),
        value_id = val$value_id, recursive = recursive, recursive_depth = recursive_depth
    )[!is.na(value_id)]

    # only include specified class
    if (!is.null(class)) {
        add_joined_cols(private$idf_env()$object, rel, "object_id", "class_id")
        cls <- get_idd_class(private$idd_env(), class)
        rel <- rel[J(cls$class_id), on = "class_id"]
    }

    if (!nrow(rel)) {
        if (is.null(class)) {
            verbose_info("Target object has no node or its nodes have no reference to other object.")
        } else {
            verbose_info("Target object has no node referring to any object in class ",
                collapse(cls$class_name), "."
            )
        }
        return(invisible())
    } else {
        rel <- rel[, list(object_id = unique(object_id)), by = "src_object_id"]
        verbose_info("Target object has node(s) referring to ", nrow(rel), " object(s) [ID:",
            collapse(rel$object_id), "].\n")
        res <- apply2(
            rel$object_id,
            private$idf_env()$object[J(rel$object_id), on = "object_id", class_id],
            IdfObject$new, list(parent = private$m_parent)
        )
        res <- lapply(res, add_idfobj_field_bindings)
        setattr(res, "names", private$idf_env()$object[J(rel$object_id), on = "object_id", object_name])
        res
    }
}
# }}}
# idfobj_has_ref {{{
idfobj_has_ref <- function (self, private, which = NULL, class = NULL, type = c("all", "ref_to", "ref_by", "node")) {
    type <- match.arg(type)
    if (is.null(which)) {
        rel <- get_idfobj_relation(private$idd_env(), private$idf_env(), private$m_object_id,
            NULL, FALSE, direction = type, keep_all = TRUE)
    } else {
        val <- get_idf_value(private$idd_env(), private$idf_env(),
            object = private$m_object_id, field = which
        )

        rel <- get_idfobj_relation(private$idd_env(), private$idf_env(),
            value_id = val$value_id, direction = type, keep_all = TRUE)
    }

    if (!is.null(class)) {
        cls <- get_idd_class(private$idd_env(), class)
        if (type %in% c("all", "ref_by")) {
            add_joined_cols(private$idf_env()$object, rel$ref_by, "object_id", "class_id")
            rel$ref_by <- rel$ref_by[J(cls$class_id), on = "class_id"]
        } else if (type %in% c("all", "ref_to")) {
            add_joined_cols(private$idf_env()$object, rel$ref_to, c(src_object_id = "object_id"), c(src_class_id = "class_id"))
            rel$ref_to <- rel$ref_to[J(cls$class_id), on = "src_class_id"]
        } else if (type %in% c("all", "node")) {
            add_joined_cols(private$idf_env()$object, rel$node, "object_id", "class_id")
            rel$node <- rel$node[J(cls$class_id), on = "class_id"]
        }
    }

    if (type == "all") {
        rel$ref_to[, list(.N > 0 && any(!is.na(src_value_id))), by = "value_id"]$V1 |
        rel$ref_by[, list(.N > 0 && any(!is.na(value_id))), by = "src_value_id"]$V1 |
        rel$node[, list(.N > 0 && any(!is.na(value_id))), by = "src_value_id"]$V1
    } else if (type == "ref_to") {
        rel$ref_to[, list(.N > 0 && any(!is.na(src_value_id))), by = "value_id"]$V1
    } else if (type == "ref_by") {
        rel$ref_by[, list(.N > 0 && any(!is.na(value_id))), by = "src_value_id"]$V1
    } else {
        rel$node[, list(.N > 0 && any(!is.na(value_id))), by = "src_value_id"]$V1
    }
}
# }}}
# idfobj_has_ref_to {{{
idfobj_has_ref_to <- function (self, private, which = NULL, class = NULL) {
    idfobj_has_ref(self, private, which, class, "ref_to")
}
# }}}
# idfobj_has_ref_from {{{
idfobj_has_ref_from <- function (self, private) {
    .deprecated_fun("$has_ref_from()", "$has_ref_to()", "IdfObject", "0.10.0")
    idfobj_has_ref_to(self, private)
}
# }}}
# idfobj_has_ref_by {{{
idfobj_has_ref_by <- function (self, private, which = NULL, class = NULL) {
    idfobj_has_ref(self, private, which, class, "ref_by")
}
# }}}
# idfobj_has_ref_node {{{
idfobj_has_ref_node <- function (self, private, which = NULL, class = NULL) {
    idfobj_has_ref(self, private, which, class, "node")
}
# }}}
# idfobj_to_table {{{
idfobj_to_table <- function (self, private, all = FALSE, string_value = TRUE,
                             unit = TRUE,wide = FALSE) {
    get_idfobj_table(private$idd_env(), private$idf_env(), private$m_object_id,
        all = all, unit = unit, wide = wide, string_value = string_value
    )
}
# }}}
# idfobj_table {{{
idfobj_table <- function (self, private, all = FALSE, unit = TRUE, wide = FALSE, string_value = TRUE, in_ip) {
    .deprecated_fun("$table()", "$to_table()", "IdfObject", "0.10.0")
    if (!missing(in_ip)) .deprecated_arg("in_ip", "0.10.0", "IdfObject")
    idfobj_to_table(self, private, all = all, unit = unit, wide = wide, string_value = string_value)
}
# }}}
# idfobj_to_string {{{
idfobj_to_string <- function (self, private, comment = TRUE, leading = 4L, sep_at = 29L, all = FALSE) {
    get_idfobj_string(private$idd_env(), private$idf_env(), private$m_object_id,
        comment = comment, leading = leading, sep_at = sep_at, all = all
    )
}
# }}}
# idfobj_string {{{
idfobj_string <- function (self, private, comment = TRUE, leading = 4L, sep_at = 29L) {
    .deprecated_fun("$string()", "$to_string()", "IdfObject", "0.10.0")
    idfobj_to_string(self, private, comment, leading, sep_at)
}
# }}}
# idfobj_print {{{
idfobj_print <- function (self, private, comment = TRUE, auto_sep = FALSE, brief = FALSE) {
    obj <- get_idf_object(private$idd_env(), private$idf_env(), object = private$m_object_id)

    if (is.na(obj$object_name)) {
        h <- paste0("<IdfObject: ", surround(obj$class_name), "> [ID:", obj$object_id, "]")
    } else {
        h <- paste0("<IdfObject: ", surround(obj$class_name), "> [ID:", obj$object_id, "] `", obj$object_name, "`")
    }

    cli::cat_line(h)

    if (brief) return(invisible(self))

    val <- get_idf_value(private$idd_env(), private$idf_env(), object = private$m_object_id)

    # comment
    if (comment && !is.null(obj$comment[[1L]])) {
        cli::cat_rule("COMMENTS")
        cli::cat_line(str_trunc(paste0("!", obj$comment[[1L]])))
        cli::cat_rule("VALUES")
    }

    if (auto_sep) {
        sep_at <- max(nchar(val$value_chr, "width", keepNA = FALSE)) + 4L
        if (sep_at > 20L) sep_at <- 20L
        if (sep_at < 12L) sep_at <- 12L
    } else {
        sep_at <- 20L
    }

    # value
    add_joined_cols(private$idd_env()$field, val, "field_id", c("units", "ip_units", "type_enum"))
    fmt <- format_objects(val, c("class", "value"), brief = FALSE, sep_at = sep_at)$out[[1L]]
    # remove trailing blank line
    cli::cat_line(str_trunc(fmt[-length(fmt)]))

    invisible(self)
}
# }}}

#' Format an IdfObject
#'
#' Format an [IddObject] into a character vector in the same way as in IDF Editor.
#'
#' @param x An [IdfObject] object.
#' @param comment If `FALSE`, all comments will not be included. Default: `TRUE`.
#' @param leading Leading spaces added to each field. Default: `4L`.
#' @param sep_at The character width to separate value string and field string.
#' Default: `29L` which is the same as IDF Editor.
#' @param all If `TRUE`, values of all possible fields in current class the
#'   [IdfObject] belongs to are returned. Default: `FALSE`
#' @param ... Further arguments passed to or from other methods.
#' @return A character vector.
#' @examples
#' \dontrun{
#' idf <- read_idf(system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr"),
#'     idd = use_idd(8.8, download = "auto"))
#'
#' # get the IdfObject of material named "C5 - 4 IN HW CONCRETE"
#' mat <- idf$Material[["C5 - 4 IN HW CONCRETE"]]
#'
#' cat(format(mat, leading = 0, sep_at = 10))
#' }
#' @export
# format.IdfObject {{{
format.IdfObject <- function (x, comment = TRUE, leading = 4L, sep_at = 29L, all = FALSE, ...) {
    paste0(x$to_string(comment = comment, leading = leading, sep_at = sep_at, all = all), collapse = "\n")
}
# }}}

#' Coerce an IdfObject into a Character Vector
#'
#' Coerce an [IdfObject] into a character vector in the same way as in IDF Editor.
#'
#' @inheritParams format.IddObject
#' @return A character vector.
#' @examples
#' \dontrun{
#' idf <- read_idf(system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr"),
#'     idd = use_idd(8.8, download = "auto"))
#'
#' # get the IdfObject of material named "C5 - 4 IN HW CONCRETE"
#' mat <- idf$Material[["C5 - 4 IN HW CONCRETE"]]
#'
#' as.character(mat, leading = 0, sep_at = 10)
#' }
#' @export
# as.character.IdfObject {{{
as.character.IdfObject <- function (x, comment = TRUE, leading = 4L, sep_at = 29L, all = FALSE, ...) {
    x$to_string(comment = comment, leading = leading, sep_at = sep_at, all = all)
}
# }}}

#' @export
# str.IdfObject {{{
str.IdfObject <- function (object, ...) {
    object <- object$value()
    NextMethod()
}
# }}}

#' @export
# print.IdfObject {{{
print.IdfObject <- function (x, comment = TRUE, auto_sep = TRUE, brief = FALSE, ...) {
    add_idfobj_field_bindings(x, update = TRUE)
    x$print(comment = comment, auto_sep = auto_sep, brief = brief)
}
# }}}

#' @export
# [.IdfObject {{{
'[.IdfObject' <- function(x, i, j, ...) {
    if (!missing(j)) stop("incorrect number of dimensions")
    .subset2(x, "value")(i)
}
# }}}

#' @export
# $.IdfObject {{{
'$.IdfObject' <- function (x, name) {
    if (name %in% ls(x)) return(NextMethod())

    self <- ._get_self(x)
    private <- ._get_private(x)

    # In order to make sure `idfobj$nAmE` is not acceptable
    fld_nm <- private$idd_env()$field[J(private$m_class_id), on = "class_id", field_name]
    fld_idx <- chmatch(name, underscore_name(fld_nm))
    if (!is.na(fld_idx)) {
        tryCatch(
            get_idfobj_value(private$idd_env(), private$idf_env(),
                private$m_object_id, which = name, underscore = TRUE
            )[[1L]],
            error_bad_field_name = function (e) NextMethod()
        )
    } else {
        NextMethod()
    }
}
# }}}

#' @export
# [[.IdfObject {{{
'[[.IdfObject' <- function(x, i) {
    if (length(i) != 1L) return(NextMethod())

    if (i %in% ls(x)) {
        NextMethod()
    } else {
        self <- ._get_self(x)
        private <- ._get_private(x)

        # In order to make sure `idfobj$nAmE` is not acceptable
        if (is_integer(i) || i %chin% private$idd_env()$field[J(private$m_class_id), on = "class_id", field_name]) {
            tryCatch(
                get_idfobj_value(private$idd_env(), private$idf_env(),
                    private$m_object_id, which = i
                )[[1L]],
                error_bad_field_name = function (e) NextMethod()
            )
        } else {
            NextMethod()
        }
    }
}
# }}}

#' @export
# $<-.IdfObject {{{
`$<-.IdfObject` <- function (x, name, value) {
    if (name %in% ls(x)) return(NextMethod())

    self <- ._get_self(x)
    private <- ._get_private(x)

    assert(is_scalar(value))

    # In order to make sure `idfobj$nAmE <- "a"` is not acceptable
    fld_nm <- private$idd_env()$field[J(private$m_class_id), on = "class_id", field_name]
    fld_idx <- chmatch(name, underscore_name(fld_nm))
    if (!is.na(fld_idx)) {
        names(value) <- name
        tryCatch(.subset2(x, "set")(c(value)),
            error_bad_field_name = function (e) NextMethod()
        )

        # add bindings
        add_idfobj_field_bindings(x, fld_idx)

        invisible(x)
    } else {
        stop("cannot add bindings to a locked environment")
    }
}
# }}}

#' @export
# [[<-.IdfObject {{{
'[[<-.IdfObject' <- function(x, i, value) {
    if (length(i) != 1) return(NextMethod())

    if (i %in% ls(x)) return(NextMethod())

    self <- ._get_self(x)
    private <- ._get_private(x)

    # In order to make sure only standard field name is not acceptable
    fld_nm <- private$idd_env()$field[J(private$m_class_id), on = "class_id", field_name]
    if (is_integer(i)) {
        fld_idx <- i
        i <- fld_nm[[i]]
    } else {
        fld_idx <- chmatch(i, fld_nm)
    }
    if (!is.na(fld_idx)) {
        names(value) <- i
        tryCatch(.subset2(x, "set")(c(value)),
            error_bad_field_name = function (e) NextMethod()
        )

        # add bindings
        add_idfobj_field_bindings(x, fld_idx)

        invisible(x)
    } else {
        stop("cannot add bindings to a locked environment")
    }
}
# }}}
