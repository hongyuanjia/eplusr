#' Read, modify, save, run and analyze EnergyPlus models
#'
#' IDFEditor distributed along with
#' \href{https://www.energyplus.net}{EnergyPlus} provides full support for
#' preparing EnergyPus IDF and IMF files for simulations. The parsing and
#' writing process of IDF and IDD files in \code{eplusr} is basically the same
#' as that in IDFEditor. But \code{eplusr} takes advantage of the powerful
#' \code{data.table} package to speed up the whole process and store the
#' results. The IDD files for EnergyPlus 8.5 to 8.8 have been pre-parsed and
#' stored internally and will automatically be used when parsing \code{IDF} and
#' \code{IMF} files. The souce codes of IDFEditor can be found on
#' \href{https://github.com/NREL/EnergyPlus/tree/develop/src/IDF_Editor}{GitHub}
#' . There is still an option to give an additional IDD file path to parse if
#' you want. However, it will still take about 3-4 sec to parse an IDD file
#' which is much slower than IDFEditor written in Visual Basic.
#'
#' Basically, all model data are stored as `data.table`s. And each object
#' in the model has an unique \strong{\code{ID}}. Once you have the object ID,
#' you can set fields (using \code{$set}) in the object, duplicate (using
#' \code{$dup}), delete (using \code{del}) the object.
#'
#' @section Usage:
#'
#' ```
#' model <- Idf$new(path, idd = NULL)
#'
#' model$version()
#' model$path()
#' model$group_name(where = c("idf", "idd"))
#' model$class_name(where = c("idf", "idd"))
#' model$object_id(class = NULL)
#'
#' model$get_options(options = NULL)
#' model$set_options(...)
#'
#' model$definition(class)
#'
#' model$object(id)
#' model$objects(ids)
#' model$object_in_class(class, index = NULL)
#' model$objects_in_class(class, indexes = NULL)
#' model$search_value(pattern)
#' model$replace_value(pattern, replacement)
#'
#' model$add_object(class, ..., default = TRUE)
#' model$dup_object(id, new_name = NULL)
#' model$ins_object(objects)
#' model$set_object(id, ...)
#' model$del_object(id, referenced = FALSE)
#'
#' model$validate()
#'
#' model$string(header = TRUE, comment = TRUE)
#' model$save(path = NULL, overwrite = FALSE)
#'
#' model$copy()
#'
#' model$is_valid_class(class, where = "idf")
#' model$is_valid_id(id)
#' model$is_unsaved()
#' model$is_valid()
#'
#' model$run(weather = NULL, dir = NULL, wait = TRUE)
#' model$errors(info = TRUE)
#' model$collect()
#' model$output_dir(open = FALSE)
#'
#' model$print(plain = FALSE)
#' ```
#'
#' @section Read:
#'
#' ```
#' model <- eplus_model$new(path, idd = NULL)
#' ```
#'
#' * `path`: Path to EnergyPlus `IDF` or `IMF` file. The file extension does not
#'     matter. So models stored in `TXT` file are still able to correctly be
#'     parsed.
#' * `idd`: Path to `Energy+.idd` file. If NULL, the pre-parsed `Energy+.idd`
#'     files stored internally from EnergyPlus v8.0 to 8.8 will be used.
#'
#' @section Query:
#'
#' ```
#' model$all(type, class = NULL)
#' model$contains(match, scale)
#' model$matches(match, ..., scale)
#' model$get(...)
#' ```
#'
#' `$all` will give you all valid components you specified using `type` in
#'   current model for type "id" and "class". You can find all available fields
#'   for all valid class in IDD using `$all(type = "field", class =
#'   "any_valid_class_in_IDD")` which makes it handy to be used along with
#'   `$add`.
#'
#' `$contains` and `$matches` will search and return objects that contain the
#'   string or match the regular expression you give.
#'
#' `$get` will return you the objects with valid IDs you give.
#'
#' **Arguments**
#'
#' * `model`: An `eplus_model` object.
#' * `type`: Should be one of "id", "class" and "field". "id" will give you all
#'           object IDs in current model. "class" will give you all classes
#'           existed in current model. "field" will give you all valid fields in
#'           the class with required fields marked with "*".
#' * `class`: An valid class name. Only required when `type` is set to "field".
#'            you can find all valid class names using `$all("class")`.
#' * `match`: A string for `$contains` and a regular expression for `$matches`
#'            you want to search or match. All `...` in `$matches` will be
#'            parsed to `grepl`. See \code{\link{grepl}}.
#' * `scale`: Where you want to search. Should be one of "class" and "field".
#'            If "class", only class names existing in current model will be
#'            searched. If "field", only fields in current model will be
#'            searched. This is a handy option when you want to see if an object
#'            e.g. one material, is referred by other objects e.g.
#'            constructions.
#' * `...` (in `$get`): Valid object IDs. You can find all valid object IDs
#'                      using `$all("id")`.
#'
#' @section Modify:
#'
#' ```
#' model$add(class, ..., min = TRUE)
#' model$set(id, ...)
#' model$dup(id, new_name = NULL)
#' model$del(id, force = FALSE)
#' model$hide(id, force = FALSE)
#' ```
#'
#' `$add` will add an object in the `class` you give. All fields will be set to
#'   their defaults if applicable.
#'
#' `$set` will set curtain fields in the objects specified by `id`.
#'
#' `$dup` will duplicate current object specified by `id`.
#'
#' `$del` will delete current object specified by `id`. If the object is
#'   referred by other object(s), an error will given showing the fields that
#'   were referred. You can still delete the object if you want by setting
#'   `force` to TRUE.
#'
#' `$hide` is the same as `$del`, except that `$hide` will comment out the
#' object instead of deleting it. This make if possible for you to get the
#' hidden objects back by uncomment it using any test editor.
#'
#' All newly added, modified, deleted and hidden fields will be marked with
#' "(+)", "(~)", "(-)" and "(!)" respectively. The valid IDs will be appended
#' after `$add` and `$dup`, and the newly added (duplicated) object will have
#' the max ID.  *Note* that the IDs of deleted and hidden objects are invalid
#' after `$del` and cannot be applied to methods `$set`, `$dup`, `$del` and
#' `$hide`, of course. However, unless you save the model, the deleted and
#' hidden objects are still there internally but with a special mark to prevent
#' them accessable. This is done by purpose, in order to provide a new method
#' call `$undo` in the future, which will enable you to un-delete or un-hide
#' the objects.
#'
#' **Arguments**
#'
#' * `model`: An `eplus_model` object.
#' * `class`: An valid class name. Only required when `type` is set to "field".
#'            you can find all valid class names using `$all("class")`.
#' * `id`: A valid object IDs. You can find all valid object IDs using
#'         `$all("id")`.
#' * `min`: If TRUE, only minimum fields will be created. Else,
#'          all valid fields will be created. Default is TRUE.
#' * `new_name`: The new name of the duplicated object if applicable. If NULL,
#'               the duplicated object will have the same name of the original
#'               object except with a suffix of "_1", "_2" and etc.
#' * `force`: Whether delete or hide the object even it has been referred by
#'            others. Default is FALSE.
#' * `...`: Field values you want to add or modify. Currently three types are
#'          acceptable: (a) directly list all field values with no name. The
#'          values will be assigned to fields according to the order of values;
#'          (b) give both field names and values in pair, e.g. Name = "Test",
#'          `Sepcific Heat` = 150. You can find all valid field names (with
#'          units) using `$all("field", class = "class_name_to_query")`; (c)
#'          some kind of the same as (b), but with all field names in lower
#'          cases and spaces replaced by "_". Note: All field names should be
#'          given without units. Error will occur when the type (character or
#'          numeric), and the value (e.g. range) are not valid.
#'
#' @section Notes:
#'
#' ```
#' model$notes(id, ..., append = FALSE, wrap = 0L)
#' ```
#'
#' `$notes` will show, add or delete notes(comments) for the object specified using `id`.
#'
#' **Arguments**
#'
#' * `model`: An `eplus_model` object.
#' * `id`: A valid object IDs. You can find all valid object IDs using
#'         `$all("id")`.
#' * `...`: Any character vectors you want to add as notes for the object. If
#'          empty, the objects with notes will be printed.
#' * `append`: If TRUE, add new notes to the end of existing ones, otherwise
#'             add notes to the beginning of existing ones. If NULL, the
#'             already existing notes will be deleted before add new ones.
#' * `wrap`: If greater than 0L,long notes will be wrap at the length of `wrap`.
#'
#' @section Diff:
#'
#' ```
#' model$diff(type)
#' ```
#'
#' `$diff` will show all modifications you made, including added (or
#'   duplicated), modified, deleted and hidden objects with markers "(+)",
#'   "(~)", "(-)" and "(!)" respectively.
#'
#' **Arguments**
#'
#' * `model`: An `eplus_model` object.
#' * `type`: What type of modifications to show. Should be one of "all", "add",
#'           "set", "del". Default is "all".
#'
#' @section Check:
#'
#' ```
#' model$check()
#' ```
#'
#' `$check` will check the validation of all fields in current model, including
#'   missing required objected and fields, wrong value types, choices,
#'   references, any value range exceeds, invalid autosizable and
#'   autocalculatable fields.
#'
#' **Arguments**
#'
#' * `model`: An `eplus_model` object.
#'
#' @section Save:
#'
#' ```
#' model$save(confirm = FALSE, format)
#' model$saveas(path, format, overwrite = FALSE)
#' ```
#'
#' `$save` is a shortcut of `$saveas(path = "the_original_model_path")` and will
#' overwrite the current file which has a risk of losing your original file and
#' data. So make sure you have a safe copy of you original model.
#'
#' `$saveas` will save the model as a new file.
#'
#' **Arguments**
#'
#' * `model`: An `eplus_model` object.
#' * `confirm`: Whether to save the model and overwrite the original file.
#'              Default is FALSE.
#' * `format`: The saving format. Should be one of "asis", "sorted", "ori_top",
#'             and "ori_bot". If "asis", which is the default, the model will be
#'             saved in the same format as it is. If the model does not contain
#'             any format saving option, which is typically the case when the
#'             model was not saved using `eplusr` or IDFEditor, the "sorted"
#'             will be used. "sorted", "ori_top" and "ori_bot" are the same as
#'             the save options "Sorted", "Original with New at Top", and
#'             "Original with New at Bottom" in IDFEditor.
#' * `path`: The path to save the model.
#' * `overwrite`: Whether to overwrite the file if it already exists. Default is
#'                FALSE.
#'
#' @section Reset:
#'
#' ```
#' model$reset(confirm = FALSE)
#' ```
#'
#' `$reset` will reset the model to the status when it was last saved using
#' `$save` or `$saveas` (if never saved, first read and parsed using
#' `eplus_model$new`) All your modifications will be lost, so use with
#' caution. It is pretty useful if you messed things up during modifications.
#'
#' **Arguments**
#'
#' * `model`: An `eplus_model` object.
#' * `confirm`: Whether to reset the model. Default is FALSE.
#'
#' @section Run Model and Collect Results:
#'
#' ```
#' model$run(period = ~., weather = NULL, echo = FALSE, dir = NULL, eplus_home = NULL)
#' model$collect(type = c("variable", "meter", long = FALSE))
#' model$table(report = NULL, key = NULL, table = NULL, nest = TRUE)
#' ```
#'
#' `$run` will run the current model within given period using corresponding
#'   version of EnergyPlus.
#'
#' `$collect` will collect the simulation variable (specified in
#'   `Output:Variable` class) and meter (specified in `Output:Meter*` classes)
#'   output of current model
#'
#' `$table` will extract tables from simulation table (specified in
#'   `Output:Table*` classes) output of current model.
#'
#' NOTE: The underlying functions in `$table` relies on the `HTML` format
#' output. If the `Column Separator` in `OutputControl:Table:Style` does not
#' contain `HTML` format, `eplusr` will automatically change it when running
#' the model. For example, `"Comma"` (which is the default value) will be
#' changed into `"CommaAndHTML"` and a warning message will be issued.
#'
#' **Arguments**
#'
#' * `model`: An `eplus_model` object.
#' * `period`: A formula specified in format `from ~ to` to determine what
#'             period should the model be run. It can be used to override the
#'             `RunPeriod` objects. The original objects in `RunPeriod` class
#'             will be commented out using `$hide`. Each side of a `period`
#'             formulais specified as a character in format `'MM-DD'`, but
#'             powerful shorthand is available:
#'    - `~.`: Use existing `RunPeriod` objects. This is the default.
#'    - `~"annual"`: Force to run annual simulation only.
#'    - `~"design_day"`: Force to run design day only.
#'    - `~4` or `~"4"` or `~"Apr"`: Force to run from April 1st to April 30th.
#'    - `2~4` or `"2"~"4"` or `"Feb"~"Apr"`: Force to run from February 1st to
#'        April 30th.
#'    - `"2-1"~"4-30"`: Same as above.
#' * `weather`: The weather file used to run simulation. If NULL, the chicago
#'              weather file ("USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw")
#'              in the distributed along with corresponding EnergyPlus will be
#'              used, and a warning message will be given.
#' * `echo`: Whether to print the standard output and error of EnergyPlus to
#'           the screen. Default is FALSE.
#' * `dir`: The directory to save the simulation results. If NULL, which is the
#'          default, the model folder will be used.
#' * `eplus_home`: The EnergyPlus installation folder path. If NULL, which is
#'                 the default, `eplusr` will try to find if corresponding
#'                 version of EnergyPlus that was installed in the standard
#'                 location, i.e.  "C:/EnergyPlusVX-X-X" on Windows,
#'                 "/usr/local/EnergyPlus-X-X-X" on Linux and
#'                 "/Applications/EnergyPlus-X-X-X" on MacOS.
#' * `type`: Should be one of "variabale" (default) and "meter". If "variable",
#'           results from `Output:Variable` will be collected. If "meter",
#'           results from `Output:Meter*` will be collected.
#' * `long`: Whether to change the collected data from wide format (which is
#'           the default format from EnergyPlus) to long format which is much
#'           easy for data analysis. In long table format, the wide table will
#'           be melten according to DateTime and the output names will be
#'           splited into four parts, i.e. key, variable, frequency and unit.
#'           For more information on "Tidy Data", please read the excellent
#'           paper of Hadley Wickham
#'           \href{http://vita.had.co.nz/papers/tidy-data.html}{here}.
#' * `report`, `key` and `table`: Specify what tables to extract from
#'                                EnergyPlus HTML table output. You can find
#'                                valid report values by looking at Table of
#'                                Contents of the file, valid key values by
#'                                looking at the "For" line after each report
#'                                name. For example, the first table in the
#'                                table output can be extract by specifying
#'                                `$table(report = "Annual Building Utility
#'                                Performance Summary", key = "Entire
#'                                Facility", table = "Site and Source Energy")`.
#' * `nest`: If TRUE, which is the default, `$table` will return a data.table
#'           with four columns named `"report"`, `"key"`, `"table"` and
#'           `"content"`. `"content"` is a list column which has the extracted
#'           tables. If FALSE, a list will return with each each number
#'           containing those data.
#'
#' @docType class
#' @name idf
#' @author Hongyuan Jia
#' @importFrom R6 R6Class
#' @importFrom data.table data.table setattr rbindlist copy
#' @importFrom purrr splice map
#' @importFrom glue glue
#' @importFrom cli cat_line rule
#' @importFrom readr write_lines
#' @importFrom assertthat assert_that
#' @importFrom uuid UUIDgenerate
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

            private$m_log$view_in_ip <- getOption("eplusr.view_in_ip")
            private$m_log$num_digits <- getOption("eplusr.num_digits")

            # TODO: give verbose info about different save format
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

        is_valid_class = function (class, all = FALSE)
            i_is_valid_class_name(self, private, class, type = ifelse(all, "idd", "idf")),

        is_valid_id = function (id)
            i_is_valid_object_id(self, private, id),

        is_valid_name = function (name)
            i_is_valid_object_name(self, private, name),

        is_unsaved = function ()
            i_is_unsaved_idf(self, private),

        # set_options = function (...) {
        #     # set options for current Idf
        #     # {{{
        #     # capture all arguments in dots and flatten into a list
        #     dots <- purrr::splice(...)
        #     assert_that(not_empty(dots), msg = "Please give options to set.")
        #     nms <- names(dots)
        #     assert_that(all(nms != ""), msg = "Please give option names to set.")
        #     private$assert_valid_options(nms)
        #     if (not_empty(dots$validate_level)) {
        #         private$set_validate_level(dots[["validate_level"]])
        #     }
        #     if (not_empty(dots$save_format)) {
        #         private$set_save_format(dots[["save_format"]])
        #     }
        #     if (not_empty(dots$special_format)) {
        #         private$set_flag_option("special_format", dots[["special_format"]])
        #     }
        #     if (not_empty(dots$view_in_ip)) {
        #         private$set_flag_option("view_in_ip", dots[["view_in_ip"]])
        #     }
        #     if (not_empty(dots$num_digits)) {
        #         private$set_num_digits(dots[["num_digits"]])
        #     }
        #     if (not_empty(dots$view_in_ip) | not_empty(dots$num_digits)) {
        #         private$update_value_tbl()
        #     }
        #     return(invisible(self))
        #     # }}}
        # },

        definition = function (class)
            i_iddobject(self, private, class),

        object = function (which)
            i_idfobject(self, private, which),

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

        save = function (path = NULL, format = c("sorted", "new_top", "new_bot"), overwrite = FALSE, copy_external = TRUE)
            i_idf_save(self, private, path, format, overwrite, copy_external),

        run = function (weather = NULL, dir = NULL, wait = TRUE, force = FALSE)
            i_idf_run(self, private, weather, dir, wait, force),

        print = function (plain = FALSE)
            i_print_idf(self, private, plain)
        # }}}

    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_uuid = NULL,
        m_path = NULL,
        m_is_imf = NULL,
        m_version = NULL,
        m_idd_tbl = NULL,
        m_idf_tbl = NULL,
        m_log = NULL,
        m_run = NULL,
        m_idfobj_generator = NULL,
        m_iddobj_generator = NULL,
        # }}}

        deep_clone = function (name, value)
            i_deep_clone(self, private, name, value)
    )
)
# }}}

#' @export
# [.Idf {{{
'[.Idf' <- function(x, i, j, ...) {
    obj <- .subset2(x, "object_in_class")(i)

    if (missing(j)) return(obj)

    .subset(obj, j)
}
# }}}

#' @export
# [[.Idf {{{
'[[.Idf' <- function(x, i, j, ..., drop = FALSE) {
    if (!is_scalar(i))
        stop("Please give a single object ID or object name.")

    if (!missing(j))
        stop("Incorrect number of subscripts.")

    .subset2(x, "object")(i)[[1]]
}
# }}}

#' @export
# $.Idf {{{
'$.Idf' <- function (x, i, ...) {
    funs <- setdiff(ls(x), "initialize")
    if (i %in% funs) return(.subset2(x, i))

    in_nm <- i_lower_field_name(i)
    cls_std <- .subset2(x, "class_name")()
    cls_lower <- i_lower_field_name(cls_std)

    idx <- match(in_nm, cls_lower)

    if (is.na(idx))
        stop("Invalid class name found in current Idf: ", backtick(i), ".")

    .subset2(x, "object_in_class")(.subset2(cls_std, idx))
}

# }}}
