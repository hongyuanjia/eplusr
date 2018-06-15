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
#' model$group_names(where = c("idf", "idd"))
#' model$class_names(where = c("idf", "idd"))
#' model$object_ids(class = NULL)
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
    # substitute the clone method
    clone_method <- Idf$clone_method
    # `deep` arg will be ignored
    full_clone <- function (deep = TRUE) {
        deep_cloned <- clone_method(deep = TRUE)
        enclos_env <- deep_cloned$.__enclos_env__
        enclos_env$private$IdfObject$self$private_fields$m_uuid <- enclos_env$private$m_uuid
        enclos_env$private$IdfObject$self$private_fields$m_version <- enclos_env$private$m_version
        enclos_env$private$IdfObject$self$private_fields$m_idf_tbl <- enclos_env$private$m_idf_tbl
        enclos_env$private$IdfObject$self$private_fields$m_idd_tbl <- enclos_env$private$m_idd_tbl
        enclos_env$private$IdfObject$self$private_fields$m_options <- enclos_env$private$m_options
        enclos_env$private$IdfObject$self$private_fields$m_log <- enclos_env$private$m_log
        enclos_env$private$IdfObject$self$private_fields$IdfObject <- enclos_env$private$IdfObject
        deep_cloned
    }
    Idf$clone_method <- full_clone
    Idf$public_methods$clone <- full_clone

    idf <- Idf$new(path, idd)

    # delete unnecessary methods inherited from parent Idd class
    methods_mask <- c("build", "group_orders", "class_orders",
        "objects_in_group", "required_objects", "unique_objects",
        "unique_class_names", "extensible_class_names", "required_class_names")

    mask_method <- function (sym, env = idf) {
        unlockBinding(sym, env)
        env[[sym]] <- NULL
        env$.__enclos_env__$self[[sym]] <- NULL
        lockBinding(sym, env)
    }
    lapply(methods_mask, mask_method)

    idf
}
# }}}

# Idf {{{
Idf <- R6::R6Class(classname = "Idf",
    inherit = Idd,

    public = list(

        # INITIALIZE {{{
        initialize = function (path, idd = NULL) {

            # add a uuid
            private$m_uuid <- uuid::UUIDgenerate(use.time = TRUE)

            # only store if input is a path
            if (length(path) == 1L) {
                if (file.exists(path)) private$m_path <- path
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

            private$m_options <- list2env(as.list.environment(.options), parent = emptyenv())
            private$m_options$save_format <- idf_file$options$save_format
            private$m_options$view_in_ip <- idf_file$options$view_in_ip
            private$m_options$special_format <- idf_file$options$special_format

            idd <- attr(idf_file, "idd")
            # init idd tbl
            private$m_idd_tbl <- list2env(as.list.environment(
                ._get_private(idd)$m_idd_tbl), parent = emptyenv())
            # get IddObject generator
            private$IddObject <- ._get_private(idd)$IddObject

            # init idf tbl
            private$m_idf_tbl <- list2env  (
                idf_file[c("object", "value", "value_reference", "comment")],
                parent = emptyenv()
            )

            # init log data
            private$m_log <- new.env(parent = emptyenv())
            private$m_log$unsaved <- FALSE
            private$m_log$order <- private$m_idf_tbl$object[, list(object_id)][
                , object_order := 0L]

            # create the IdfObject R6Class Generator for this specific Idf
            private$create_idfobj_gen(IdfObject)

            # add `Output:SQLite` for collecting simulaton results
            private$add_sql_output()
        },
        # }}}

        # PUBLIC FUNCTIONS
        # {{{
        version = function () {
            # return version of current IDF
            # {{{
            private$m_version
            # }}}
        },

        path = function () {
            # return the model path
            # {{{
            private$m_path
            # }}}
        },

        group_names = function (all = FALSE) {
            # return all group names
            # {{{
            if (!all) {
                private$m_idf_tbl$object[
                    private$m_idd_tbl$class, on = "class_id", nomatch = 0L, list(group_id)][
                    private$m_idd_tbl$group, on = "group_id", nomatch = 0L, unique(group_name)]
            } else {
                super$group_names()
            }
            # }}}
        },

        class_names = function (all = FALSE) {
            # return all class names
            # {{{
            if (!all) {
                private$m_idf_tbl$object[
                    private$m_idd_tbl$class, on = "class_id", nomatch = 0L,
                    unique(class_name)]
            } else {
                super$class_names()
            }
            # }}}
        },

        object_ids = function (class = NULL, simplify = FALSE) {
            # return all object ids in current IDF
            # {{{
            if (is.null(class)) {
                if (simplify) {
                    private$m_idf_tbl$object[order(object_id), object_id]
                } else {
                    res <- private$m_idd_tbl$class[private$m_idf_tbl$object,
                        on = "class_id", nomatch = 0L, list(object_id, class_name)]

                    lapply(split(res, by = "class_name", keep.by = FALSE),
                        function (x) x[["object_id"]])
                }
            } else {
                assert_that(self$is_valid_class(class))
                id <- super$class_orders(class)
                private$m_idd_tbl$class[
                    private$m_idf_tbl$object[class_id == id], on = "class_id",
                    object_id]
            }
            # }}}
        },

        object_names = function (class = NULL, simplify = FALSE, keep_na = FALSE) {
            # return all object names in current IDF
            # {{{
            if (is.null(class)) {
                if (simplify) {
                    if (keep_na) {
                        private$m_idf_tbl$object$object_name
                    } else {
                        private$m_idf_tbl$object[!is.na(object_name), object_name]
                    }
                } else {
                    res <- private$m_idd_tbl$class[private$m_idf_tbl$object,
                        on = "class_id", nomatch = 0L, list(object_name, class_name)]
                    if (!keep_na) res <- res[!is.na(object_name)]

                    lapply(split(res, by = "class_name", keep.by = FALSE),
                        function (x) x[["object_name"]])
                }
            } else {
                assert_that(self$is_valid_class(class))
                id <- super$class_orders(class)
                res <- private$m_idd_tbl$class[
                    private$m_idf_tbl$object[class_id == id], on = "class_id",
                    list(object_name, class_name)]

                if (!keep_na) {
                    res[!is.na(object_name), object_name]
                } else {
                    res$object_name
                }
            }
            # }}}
        },

        is_valid_class = function (class) {
            # check if the input string is a valid class name in current IDF
            # {{{
            assert_that(is_string(class))
            class %in% self$class_names()
            # }}}
        },

        is_valid_id = function (id) {
            # check if the input number is a valid object id in current IDF
            # {{{
            assert_that(is_count(id))
            id %in% self$object_ids()
            # }}}
        },

        is_valid_name = function (name) {
            # check if the input string is a valid object name in current IDF.
            # NOTE: checking is case insensitive
            # {{{
            assert_that(is_string(name))
            toupper(name) %in% private$m_idf_tbl$object[!is.na(object_name), object_name_upper]
            # }}}
        },

        is_unsaved = function () {
            # return TRUE if there are unsaved changes
            # {{{
            private$m_log$unsaved %||% FALSE
            # }}}
        },

        get_options = function (options = NULL) {
            # return current options for current Idf
            # {{{
            res <- as.list.environment(private$m_options)
            # exclude `special format`
            res$special_format <- NULL
            if (is.null(options)) {
                res
            } else {
                assert_that(is.character(options))
                private$assert_valid_options(options)
                res[options]
            }
            # }}}
        },

        set_options = function (...) {
            # set options for current Idf
            # {{{
            # capture all arguments in dots and flatten into a list
            dots <- purrr::splice(...)
            assert_that(not_empty(dots), msg = "Please give options to set.")
            nms <- names(dots)
            assert_that(all(nms != ""), msg = "Please give option names to set.")
            private$assert_valid_options(nms)
            if (not_empty(dots$validate_level)) {
                private$set_validate_level(dots[["validate_level"]])
            }
            if (not_empty(dots$save_format)) {
                private$set_save_format(dots[["save_format"]])
            }
            if (not_empty(dots$special_format)) {
                private$set_flag_option("special_format", dots[["special_format"]])
            }
            if (not_empty(dots$view_in_ip)) {
                private$set_flag_option("view_in_ip", dots[["view_in_ip"]])
            }
            if (not_empty(dots$num_digits)) {
                private$set_num_digits(dots[["num_digits"]])
            }
            if (not_empty(dots$view_in_ip) | not_empty(dots$num_digits)) {
                private$update_value_tbl()
            }
            return(invisible(self))
            # }}}
        },

        definition = function (class) {
            # return the IddObject of specific class
            # {{{
            super$object(class)
            # }}}
        },

        object = function (index) {
            # return a single object
            # {{{
            assert_that(is_scalar(index))
            id <- private$id_from_index(index)
            if (length(id) > 1L){
                warning("More than one objects [ID: ", backtick_collapse(id),
                    "] found with the name ", backtick(name),
                    ". Only the first object will be returned.", call. = FALSE)
                id <- id[1]
            }
            private$IdfObject$new(id)
            # }}}
        },

        objects = function (indexes) {
            # return a list which contains all objects with input object ids or
            # names
            # {{{
            ids <- private$id_from_index(indexes)
            lapply(ids, private$IdfObject$new)
            # }}}
        },

        object_in_class = function (class, index = NULL) {
            # return the IdfObject at index in a class
            # {{{
            ids <- self$object_ids(class)
            if (is.null(index)) {
                id <- ids[1]
            } else if (is_count(index)){
                total <- length(ids)
                assert_that(index <= length(ids),
                    msg = paste0("Invalid object order found for class ",
                        backtick(class), ": ",
                        backtick(index), ". Only ",
                        total, ifelse(total > 1L, " objects exist.", " object exists.")
                    )
                )
                id <- ids[index]
            } else if (is_string(index)) {
                nms <- self$object_names(class = class)
                if (is_empty(nms)) {
                    stop("Class ", backtick(class), " does not have name attribute.",
                        call. = FALSE)
                }
                valid <- toupper(index) %in% toupper(nms)
                if (all(!valid)) {
                    stop("Invalid object name found for class ", backtick(class),
                        ": ", backtick(index), ". Valid names: ",
                        backtick_collapse(nms), ".", call = FALSE)
                }
                id <- ids[valid]
                if (length(id) > 1) {
                    warning("More than one objects found with the name ",
                        backtick(index), " in class ", backtick(class),
                        ". Only the first object will be returned.", call. = FALSE)
                    id <- id[1]
                }
            }
            private$IdfObject$new(id)
            # }}}
        },

        objects_in_class = function (class, indexes = NULL) {
            # return a list which contails all objects in the target class
            # {{{
            ids <- self$object_ids(class)
            if (!is.null(indexes)) {
                if (all(is.numeric(indexes))){
                    valid_order <- vapply(indexes, is_count, logical(1))
                    if (!all(valid_order)) {
                        stop("Invalid object order found: ",
                            backtick_collapse(indexes[!valid_order]), ".",
                            call. = FALSE)
                    }
                    total <- length(ids)
                    assert_that(all(index <= total),
                        msg = paste0("Invalid object order found for class ",
                            backtick(class), ": ",
                            backtick(index[index > total]), ". Only ",
                            total, ifelse(total > 1L, " objects exist.", " object exists.")
                        )
                    )
                    ids <- ids[indexes]
                } else if (all(is.character(indexes))) {
                    nms <- self$object_names(class = class)
                    if (is_empty(nms)) {
                        stop("Class ", backtick(class), " does not have name attribute.",
                            call. = FALSE)
                    }
                    valid <- toupper(indexes) %in% toupper(nms)
                    if (!all(valid)) {
                        stop("Invalid object name found for class ", backtick(class),
                            ": ", backtick(indexes[!valid]), ". Valid names: ",
                            backtick_collapse(nms), ".", call = FALSE)
                    }
                    ids <- ids[valid]
                }
                lapply(ids, private$IdfObject$new)
            }
            # }}}
        },

        search_value = function (pattern) {
            # classs
            # {{{
            val <- private$m_idf_tbl$value[stringr::str_detect(value, pattern)]
            if (is_empty(val)) {
                message("No matched result found.")
                return(invisible())
            }

            value_tbl <- val[
                private$m_idf_tbl$object, on = "object_id", nomatch = 0L][
                private$m_idd_tbl$class, on = "class_id", nomatch = 0L][
                private$m_idd_tbl$field, on = "field_id", nomatch = 0L]

            cli::cat_line(format_objects(value_tbl, in_ip = private$m_options$view_in_ip))
            # }}}
        },

        replace_value = function (pattern, replacement) {
            # replace value using regex
            # {{{
            val_before <- private$m_idf_tbl$value[stringr::str_detect(value, pattern)]
            if (is_empty(val_before)) {
                message("No matched result found.", call. = FLASE)
                return(invisible())
            }

            val_after <- data.table::copy(val_before)[,
                value := stringr::str_replace_all(value, pattern, replacement)]

            value_tbl_before <- val_before[
                private$m_idf_tbl$object, on = "object_id", nomatch = 0L][
                private$m_idd_tbl$class, on = "class_id", nomatch = 0L][
                private$m_idd_tbl$field, on = "field_id", nomatch = 0L][,
                `:=`(field_order = paste0(lpad(field_order, "0"), "(before)"),
                     full_name = paste0(full_name, "\n"),
                     full_ipname = paste0(full_ipname, "\n"))]

            value_tbl_after <- val_after[
                private$m_idf_tbl$object, on = "object_id", nomatch = 0L][
                private$m_idd_tbl$class, on = "class_id", nomatch = 0L][
                private$m_idd_tbl$field, on = "field_id", nomatch = 0L][,
                `:=`(field_order = paste0(lpad(field_order, "0"), "(after) "))]

            value_tbl <- data.table::rbindlist(list(value_tbl_before, value_tbl_after))

            value_tbl_after[, `:=`(
                value_upper = toupper(value),
                value_num = suppressWarnings(as.numeric(value)),
                value_ipnum = suppressWarnings(as.numeric(value)))][,
                private$m_idd_tbl$field_property, on = "field_id", nomatch = 0L]

            value_tbl_new <- private$m_idd_tbl$field_property[value_tbl_after,
                on = "field_id", nomatch = 0L]
            value_tbl_new <- update_value_num(
                value_tbl_new,
                private$m_options$num_digits,
                private$m_options$view_in_ip)

            private$m_idf_tbl$value[value_id %in% value_tbl_after$value_id, `:=`(
                value = value_tbl_after$value,
                value_upper = value_tbl_after$value_upper,
                value_num = value_tbl_after$value_num,
                value_ipnum = value_tbl_after$value_ipnum)]

            private$m_log$order[object_id %in% value_tbl_after$object_id,
                                object_order := object_order + 1L]

            cli::cat_line(format_objects(value_tbl, in_ip = private$m_options$view_in_ip))
            # }}}
        },

        dup_object = function (index, new_name = NULL) {
            # duplicate an Object
            # {{{
            # first, copy the corresponding row in the private$m_objects
            target <- self$object(index)

            # can be duplicated?
            private$assert_can_add_object_in_class(target$class_name())

            can_name <- target$has_name()
            if (!is.null(new_name)) {
                if (can_name) {
                    # check name conflict
                    old_nm <- target$name()
                    if (toupper(new_name) == toupper(old_nm)) {
                        stop("New Name ", backtick(new_name) , " is the same ",
                            "as the name of target object.", call. = FALSE)
                    }
                } else {
                    warning("Target object does not have name attribute. ",
                        "`new_name` will be ignored.", call. = FALSE)
                }
            } else {
                if (can_name) {
                    old_nm <- target$name()
                    # get all names in the same class
                    nms <- self$object_names(class = class)
                    # get existing names that has a prefix of old name
                    nms_upper <- toupper(nms)
                    same <- nms_upper[startsWith(nms_upper, paste0(toupper(old_nm), "_"))]
                    if (length(same) == 0L) {
                        new_name <- paste0(old_nm, "_1")
                    } else {
                        # extract the max duplicated time
                        suffix <- unlist(purrr::map(strsplit(same, "_", fixed = TRUE), 2L))
                        max_dup <- max(suppressWarnings(as.integer(suffix)))
                        new_name <- paste0(old_nm, "_", max_dup + 1L)
                    }
                    private$verbose_info("New name of the new object is not \\
                        given. A name `{new_name}` is assigned to it.")
                }
            }

            max_obj_id <- private$m_idf_tbl$object[, max(object_id)]
            max_val_id <- private$m_idf_tbl$value[, max(value_id)]
            max_cmt_id <- private$m_idf_tbl$comment[, max(comment_id)]

            obj_tbl <- private$m_idf_tbl$object[object_id == id][
                , object_id := max_obj_id + 1L]
            val_tbl <- private$m_idf_tbl$value[object_id == id][
                , `:=`(value_id = max_val_id + seq_along(value_id),
                       object_id = rep(max_obj_id + 1L, length(value_id)))]
            if (can_name) {
                obj_tbl[, `:=`(
                    object_name = new_name, object_name_upper = toupper(new_name))]
                val_tbl[1, `:=`(
                    value = new_name, value_upper = toupper(new_name))]
            }
            cmt_tbl <- private$m_idf_tbl$comment[object_id == id][
                , `:=`(comment_id = max_cmt_id + seq_along(comment_id),
                       object_id = rep(max_obj_id + 1L, length(object_id)))]

            old_ids <- private$m_idf_tbl$value[object_id == id, value_id]
            new_ids <- val_tbl[["value_id"]]
            val_ref_tbl <- private$m_idf_tbl$value_reference[value_id %in% old_ids][
                , `:=`(value_id = new_ids[old_ids %in% value_id])]

            private$m_idf_tbl$object <- data.table::rbindlist(list(
                private$m_idf_tbl$object, obj_tbl
            ))
            private$m_idf_tbl$value <- data.table::rbindlist(list(
                private$m_idf_tbl$value, val_tbl
            ))
            private$m_idf_tbl$comment <- data.table::rbindlist(list(
                private$m_idf_tbl$comment, cmt_tbl
            ))
            private$m_idf_tbl$value_reference <- data.table::rbindlist(list(
                private$m_idf_tbl$value_reference, val_ref_tbl
            ))
            # log
            private$m_log$unsaved <- TRUE
            private$m_log$order <- data.table::rbindlist(list(
                private$m_log$order,
                private$m_log$order[object_id == id][
                    , `:=`(object_id = max_obj_id + 1L, object_order = 1L)]
            ))

            private$IdfObject$new(max_obj_id + 1L)
            # }}}
        },

        ins_object = function (objects) {
            # insert an object from other Idf or file or even clipboard
            # {{{
            # check if input is a object list
            if (is.list(objects)) {
                len <- length(objects)
                # every component should be an IdfObject
                valid <- vapply(objects, is_idfobject, logical(1))
                if (!all(valid)) {
                    stop("When input is a list, every component should be an ",
                         "IdfObject.", call. = FALSE)
                }
                lapply(objects, private$insert_object)
            # check if input is a string
            } else if (is_scalar(objects)) {
                if (is_idfobject(objects)) {
                    private$insert_object(objects)
                } else if (is_string(objects)) {
                    # try to parse the input using same idd
                    idd <- use_idd(private$m_version)
                    in_idf <- Idf$new(objects, idd)
                    # delete version object
                    tbl <- ._get_private(in_idf)$m_idf_tbl
                    id_ver <- tbl$object[class_id == 1L, object_id]
                    tbl$object <- tbl$object[object_id != id_ver]
                    tbl$value <-  tbl$value[object_id != id_ver]
                    ids <- in_idf$object_ids()
                    lapply(in_idf$objects(ids), private$insert_object)
                }
            } else {
                stop("Input should be an IdfObject, a list of IdfObjects, or ",
                     "any input acceptable for `Idf$new()`.", call. = FALSE)
            }
            # }}}
        },

        add_object = function (class, ..., defaults = TRUE) {
            # add a new object in class
            # {{{
            # can be added?
            private$assert_can_add_object_in_class(class)
            cls_id <- super$class_orders(class)
            obj_id <- private$m_idf_tbl$object[, max(object_id)] + 1L
            obj_tbl <- data.table::data.table(object_id = obj_id, class_id = cls_id)
            private$m_idf_tbl$object <- data.table::rbindlist(list(
                private$m_idf_tbl$object, obj_tbl), fill = TRUE)
            idfobj <- private$IdfObject$new(obj_id)

            idfobj <- tryCatch(idfobj$set_value(..., defaults = defaults),
                error = function(e) {
                    private$m_idf_tbl$object <- private$m_idf_tbl$object[
                       object_id != obj_id]
                    e
                }
            )
            # if failed
            if (inherits(idfobj, "error")) {
                stop(idfobj$message, call. = FALSE)
            } else {
                # log
                private$m_log$unsaved <- TRUE
                private$m_log$order <- data.table::rbindlist(list(
                    private$m_log$order,
                    data.table::data.table(object_id = obj_id, object_order = 1L)
                ))
                private$verbose_info("A new object [ID: {obj_id}] in class \\
                    `{class}` has been added.")
                idfobj
            }
            # }}}
        },

        set_object = function (index, ...) {
            # set values in an object
            # {{{
            obj <- self$object(index)
            obj$set_value(...)
            # log
            private$m_log$unsaved <- TRUE
            private$m_log$order[object_id == obj$id(), object_order := object_order + 1L]
            obj
            # }}}
        },

        del_object = function (index, referenced = FALSE) {
            # delete an object
            # {{{
            id <- private$id_from_index(index)
            if (length(id) > 1L){
                stop("More than one objects [ID: ", backtick_collapse(id),
                    "] found with the name ", backtick(name),
                    ". Please specify the object to delete using object id.",
                    call. = FALSE)
            }
            target <- self$object(index)
            cls <- target$class_name()
            # check
            # {{{
            # stop if target object is a `Version` object
            if (cls == "Version") {
                stop("Cannot delete `Version` object.", call. = FALSE)
            }
            # stop if target object is an required object
            if (cls %in% c(super$required_class_names())) {
                if (private$m_options$validate_level == "final") {
                    stop("Cannot delete an required object.", call. = FALSE)
                }
            }
            # }}}
            refby <- target$reference_map()$reference_by
            # message
            # {{{
            if (not_empty(refby)) {
                # stop if target object is refereced by others
                if (private$m_options$validate_level == "final") {
                    cli::cat_line(format_refmap_sgl(
                        refby, "by", in_ip = private$m_options$view_in_ip
                    ))
                    stop(glue::glue("Failed to delete target object [ID:{backtick(id)}] \\
                    at validation level `final`.  Target object [ID: {id}] was referenced \\
                    by other objects [ID: {backtick_collapse(refby[['target_object_id']])}]."),
                    call. = FALSE)
                }
                if (!referenced) {
                    ids <- id
                    private$verbose_info("Delete object [ID:{backtick(id)}] \\
                        which was referenced by objects \\
                        [ID: {backtick_collapse(refby[['target_object_id']])}].")
                } else {
                    ids <- c(id, refby[["target_object_id"]])
                    private$verbose_info("Delete target object \\
                        and also other objects [ID: {backtick_collapse(ids)}] \\
                        that are referencing target object.")
                }
            } else {
                ids <- id
                private$verbose_info("Delete target object [ID:{backtick_collapse(id)}].")
            }
            # }}}
            # delete
            private$m_idf_tbl$object <- private$m_idf_tbl$object[!object_id %in% ids]
            val_ids <- private$m_idf_tbl$value[object_id %in% ids, value_id]
            private$m_idf_tbl$value <- private$m_idf_tbl$value[!object_id %in% ids]
            private$m_idf_tbl$value_reference <- private$m_idf_tbl$value_reference[
                !(reference_value_id %in% ids | value_id %in% ids)]
            private$m_idf_tbl$comment <- private$m_idf_tbl$comment[!object_id %in% ids]
            # log
            private$m_log$unsaved <- TRUE
            private$m_log$order <- private$m_log$order[!object_id %in% ids]
            # }}}
        },

#         diff = function (id) {
#             # diff the values
#             # {{{

#             # }}}
#         },

        validate = function () {
            # validate field in terms of all creteria
            # {{{
            i_collect_validate(private)
            private$m_validate
            # }}}
        },

        is_valid = function () {
            # return TRUE if there are no errors after `$check()`
            # {{{
            i_is_valid(private)
            # }}}
        },

        string = function (header = TRUE) {
            # return save-ready format string
            # {{{
            val_tbl <- private$value_tbl()[private$m_log$order, on = "object_id",
                nomatch = 0L]
            cmt_tbl <- private$comment_tbl()
            main <- format_output(val_tbl, cmt_tbl, private$m_options)
            main <- unlist(strsplit(main, "\n", fixed = TRUE))

            h <- NULL
            if (header) {
                h <- format_header(private$m_options)
            } else {
                main <- main[-c(1,2)]
            }

            # add a blank line at the end like IDFEditor
            c(h, main, "")
            # }}}
        },

        save = function (path = NULL, overwrite = FALSE) {
            # {{{
            if (is.null(path)) {
                if (is.null(private$m_path)) {
                    stop("The Idf object is not created from local file. ",
                         "Please give the path to save.", call. = FALSE)
                } else {
                    path <- private$m_path
                }
            } else {
                assert_that(is_string(path))
            }

            if (private$m_is_imf & !has_ext(path, "imf")) {
                warning("The Idf object contains EpMacro lines. Saving it to a ",
                    "file other than `imf` file may cause errors during simulation.",
                    call. = FALSE)
            } else {
                assert_that(has_exts(path, c("idf", "imf")),
                    msg = paste0("`path` should have an extension of `idf` or `imf`."))
            }

            str <- self$string(header = TRUE)
            if (file.exists(path)) {
                if (!overwrite) {
                    stop("Target already exists. Please set `overwrite` to ",
                         "TRUE if you want to replace it.", call. = FALSE)
                } else {
                    private$verbose_info("Replace the existing file located \\
                        at `{normalizePath(path)}`.")
                    readr::write_lines(str, path)
                }
            } else {
                d <- dirname(path)
                if (!dir.exists(d)) {
                    tryCatch(dir.create(d, recursive = TRUE),
                             warning = function (w) {
                                 stop("Failed to creat directory ",
                                      backtick(d), ".", call. = FALSE)
                             })
                }
                readr::write_lines(str, path)
                private$verbose_info("The Idf has been successfully saved to\\
                    `{normalizePath(path)}`.")
            }

            # log
            private$m_log$unsaved <- FALSE
            # change path
            private$m_path <- path
            # }}}
        },

        run = function (weather = NULL, dir = NULL, wait = TRUE) {
            # {{{
            # eplus path
            ver <- private$m_version
            if (!eplus_available(ver)) {
                stop("Could not locate EnergyPlus v", private$m_version, " at ",
                     "the default installation path. Please set the path to use ",
                     "using `use_eplus()`.", call. = FALSE)
            }
            if (ver < 8.3) {
                stop("Currently, `$run()` only supports EnergyPlus V8.3 or higher.",
                     call. = FALSE)
            }

            if (is_epw(weather))  weather <- weather$path()

            private$run_info(weather, dir)
            config <- eplus_config(ver)
            eplus <- file.path(config$dir, config$exe)

            expand_obj <- ifelse(private$have_hvac_template(), TRUE, FALSE)
            proc <- run_idf(eplus,
                            private$m_run$path_idf,
                            private$m_run$path_epw,
                            private$m_run$out_dir,
                            echo = wait, expand_obj = expand_obj)
            private$m_run$proc <- proc
            private$m_run$wait <- wait
            proc
            # }}}
        },

        output_dir = function (open = FALSE) {
            # return or open the output directory
            # {{{
            dir <- dirname(private$locate_output(strict = FALSE))
            if (!open) return(dir)
            if (open) {
                if (is.null(dir)) {
                    message("No simulation has been run yet.")
                    return(invisible())
                }

                # Reference:
                # http://r.789695.n4.nabble.com/R-command-to-open-a-file-quot-browser-quot-on-Windows-and-Mac-td4710688.html
                if (is_windows()) {
                    shell.exec(dir)
                } else if (is_macos()) {
                    system2("open", dir)
                } else if (is_linux()) {
                    system(paste0("xdg-open ", dir))
                } else {
                    message("Current platform not supported.")
                }
            }
            dir
            # }}}
        },

        errors = function (info = TRUE) {
            # read simulation errors
            # {{{
            path_err <- private$locate_output(".err", strict = FALSE)

            err <- parse_err_file(path_err)

            if (!info) err$data <- err$data[!(level == "Info" & begin_environment == FALSE)]

            err
            # }}}
        },

        collect = function () {
            # check status
            # {{{
            private$m_run$sql <- private$locate_output(".sql")
            Sql$new(private$m_run$sql)
            # }}}
        },

        print = function (plain = FALSE) {
            # {{{
            if (plain) {
                cli::cat_line(self$string())
            } else {
                count <- private$m_idf_tbl$object[, list(num_obj = .N), by = class_id][
                    private$m_idd_tbl$class, on = "class_id", nomatch = 0L][
                    private$m_idd_tbl$group, on = "group_id", nomatch = 0L][
                    , list(group_name, class_name, num_obj)]

                max_num <- count[, max(num_obj)]
                count[, num_str := paste0("[", lpad(num_obj, "0"), "]")]
                count[, grp := ""]
                count[count[, .I[1L], by = list(group_name)]$V1,
                    grp := paste0("\nGroup: ", backtick(group_name), "\n", cli::rule(), "\n")]
                out <- count[, paste0(grp, num_str, " ", class_name)]

                path <- private$m_path %||% ""
                cli::cat_line("# Path: ", backtick(path))
                cli::cat_line("# Version: ", backtick(private$m_version))
                cli::cat_line(out)
            }
            # }}}
        }
        # }}}

    ),

    private = list(
        # PRIVATE FIELDS
        # {{{
        m_uuid = NULL,
        m_path = NULL,
        m_is_imf = NULL,
        m_version = NULL,
        m_options = NULL,
        m_idd_tbl = NULL,
        m_idf_tbl = NULL,
        m_validate = NULL,
        m_temp = NULL,
        m_log = NULL,
        m_run = NULL,
        IdfObject = NULL,
        IddObject = NULL,
        # }}}

        # PRIVATE FUNCTIONS
        # {{{
        create_idfobj_gen = function (IdfObject) {
            # create an IdfObject R6Class Generator corresponding to this Idf
            # {{{
            # clone the IdfObject R6Class Generator
            own_idfobject <- clone_generator(IdfObject)
            # assign shared data to IdfObject R6Class Generator
            own_idfobject$self$private_fields$m_uuid <- private$m_uuid
            own_idfobject$self$private_fields$m_version <- private$m_version
            own_idfobject$self$private_fields$m_idf_tbl <- private$m_idf_tbl
            own_idfobject$self$private_fields$m_idd_tbl <- private$m_idd_tbl
            own_idfobject$self$private_fields$m_options <- private$m_options
            own_idfobject$self$private_fields$m_log <- private$m_log
            private$IdfObject <- own_idfobject
            private$IdfObject$self$private_fields$IdfObject <- own_idfobject
            # }}}
        },

        id_from_index = function (index) {
            # get object id from object index
            # {{{
            if (!(all(is.numeric(index)) || all(is.character(index)))) {
                stop("Index should be either an object id or object name.", call. = FALSE)
            }

            if (all(is.numeric(index))) {
                private$assert_valid_ids(index)
                index
            } else {
                private$assert_valid_names(index)
                nm <- toupper(index)
                private$m_idf_tbl$object[J(nm), on = "object_name_upper", object_id]
            }
            # }}}
        },

        insert_object = function (object) {
            # insert objects from other Idf or file or even clipboard
            # {{{
            assert_that(is_idfobject(object))
            # check if it is a version object
            if (object$is_version()) {
                stop("Could not insert a `Version` object.", call. = FALSE)
            }
            # get version which should be the same version as this model
            ver_in <- ._get_private(object)$m_version
            if (ver_in != private$m_version) {
                stop("Input object has a different version ", backtick(ver_in),
                     " than current Idf object (", private$m_version, ").",
                     call. = FALSE)
            }
            # get the uuid to see if it comes from the same object
            uuid_in <- ._get_private(object)$m_uuid
            if (uuid_in == private$m_uuid) {
                private$verbose_info("Object (ID:{backtick(object$id())}) to \\
                    insert is an object from this model. The target object \\
                    will be directly duplicated instead of creating a new \\
                    one with same values.")
                self$dup_object(object$id())
            } else {
                cls <- object$class_name()
                # check if can add an object in the class
                private$assert_can_add_object_in_class(cls)
                # get all value
                val <- object$get_value()
                self$add_object(cls, val)
            }
            # }}}
        },

        object_tbl = function (all = FALSE) {
            # return a tbl contains all object info
            # {{{
            if (all) {
                private$m_idf_tbl$object[private$m_idd_tbl$class, on = "class_id"][
                    private$m_idd_tbl$class_property, on = "class_id", nomatch = 0L]
            } else {
                private$m_idf_tbl$object[private$m_idd_tbl$class, on = "class_id", nomatch = 0L][
                    private$m_idd_tbl$class_property, on = "class_id", nomatch = 0L]
            }
            # }}}
        },

        value_tbl = function (object = NULL, class = NULL, field = NULL) {
            # return a tbl contains all value info
            # {{{
            object_tbl <- private$m_idf_tbl$object
            class_tbl <- private$m_idd_tbl$class
            field_tbl <- private$m_idd_tbl$field
            if (!is.null(object)) {
                object_tbl <- object_tbl[object_id %in% object]
            }
            if (!is.null(class)) {
                private$assert_valid_classes(class)
                class_tbl <- class_tbl[class_name %in% class]
                if (!is.null(field)) {
                    field_tbl <- field_tbl[field_name == field]
                }
            } else {
                if (!is.null(field)) {
                    stop("`class` should be specified when `field` is given.",
                         call. = FALSE)
                }
            }
            private$m_idd_tbl$group[
                class_tbl, on = "group_id", nomatch = 0L][
                object_tbl, on = "class_id", nomatch = 0L, list(object_id, class_name)][
                private$m_idf_tbl$value, on = "object_id", nomatch = 0L][
                field_tbl, on = "field_id", nomatch = 0L][
                private$m_idd_tbl$field_property, on = "field_id", nomatch = 0L]
            # }}}
        },

        comment_tbl = function () {
            # return a tbl contains all comment info
            # {{{
            data.table::copy(private$m_idf_tbl$comment)[
                type == 0L, `:=`(comment = paste0("!", comment))][
                , lapply(.SD, paste0, collapse = "\n"),
                .SDcols = "comment", by = object_id]
            # }}}
        },

        update_value_tbl = function () {
            # update value tbl according num_digits and view_in_ip options
            # {{{
            private$m_idf_tbl$value <- update_value_num(private$value_tbl(),
                digits = private$m_options$num_digits,
                in_ip = private$m_options$view_in_ip)[
                , .SD, .SDcols = names(private$m_idf_tbl$value)]
            # }}}
        },

        assert_can_add_object_in_class = function (class) {
            # assert that all members in group are valid group names.
            # {{{
            if (private$m_options$validate_level == "none") {
                return(TRUE)
            }

            is_unique <- class %in% super$unique_class_names()
            if (!is_unique) {
                res <- TRUE
            } else {
                num <- private$m_idf_tbl$object[
                    class_id == super$class_orders(class), .N]
                if (num == 0L) {
                    res <- TRUE
                } else {
                    res <- FALSE
                }
            }
            assert_that(res,
                msg = paste0("Class ", backtick(class), " is an existing unique ",
                   "object that can not be added or duplicated."))
            # }}}
        },

        assert_valid_ids = function (ids) {
            # assert that all members in group are valid group names.
            # {{{
            valid <- ids %in% private$m_idf_tbl$object$object_id
            assert_that(all(valid),
                msg = paste0("Invalid object id found for current Idf:",
                             backtick_collapse(ids[!valid]), "."))
            # }}}
        },

        assert_valid_names = function (names) {
            # assert that all members in group are valid group names.
            # {{{
            valid <- toupper(names) %in% private$m_idf_tbl$object$object_name_upper
            assert_that(all(valid),
                msg = paste0("Invalid object names found for current Idf:",
                             backtick_collapse(names[!valid]), "."))
            # }}}
        },

        assert_valid_options = function (options) {
            # assert that all members in options are valid option names.
            # {{{
            valid <- options %in% setdiff(names(private$m_options), "special_format")
            assert_that(all(valid),
                msg = paste0("Invalid option name found for current Idf: ",
                             backtick_collapse(options[!valid]), "."))
            # }}}
        },

        set_flag_option = function (option, flag) {
            # set flag option
            # {{{
            assert_that(is_flag(flag))
            private$m_options[[option]] <- flag
            # }}}
        },

        set_validate_level = function (level = c("none", "draft", "final")) {
            # set validate strictness level
            # {{{
            level <- match.arg(level)
            private$m_options$validate_level <- level
            # }}}
        },

        set_save_format = function (format = c("sorted", "new_top", "new_bottom")) {
            # set validate strictness format
            # {{{
            format <- match.arg(format)
            private$m_options$save_format <- format
            # }}}
        },

        set_num_digits = function (digits) {
            # set validate strictness format
            # {{{
            assert_that(is_count(digits))
            private$m_options$num_digits <- digits
            # }}}
        },

        verbose_info = function (mes) {
            # return add verbose message
            # {{{
            if (private$m_options$verbose_info) {
                # cli::cat_line(msg(glue::glue(mes, .envir = parent.frame(1))), "\n")
                message(msg(glue::glue(mes, .envir = parent.frame(1))), "\n")
            }
            # }}}
        },

        add_sql_output = function () {
            # add `Output:SQLite` variable if not exists and set `Option Type`
            # to "SimpleAndTabular"
            # {{{
            if (self$is_valid_class("Output:SQLite")) {
                sql <- self$object_in_class("Output:SQLite")
                type <- sql$get_value()[[1]]
                if (type != "SimpleAndTabular") {
                    invisible(sql$set_value("SimpleAndTabular"))
                    private$verbose_info("Setting `Option Type` in \\
                        `Output:SQLite` to from `{type}` to `SimpleAndTabular`.")
                }
            } else {
                invisible(self$add_object("Output:SQLite", "SimpleAndTabular"))
                private$verbose_info("Adding object `Output:SQLite` and setting \\
                    `Option Type` to `SimpleAndTabular`.")
            }
            # }}}
        },

        have_hvac_template = function () {
            # return TRUE if the model has any "HVACTemplate"
            # {{{
            cls <- self$class_names()
            any(startsWith(cls, "HVACTemplate"))
            # }}}
        },

        resolve_external_links = function (dir) {
            # copy external files to local output dir and use relative paths in
            # objects such as `Schedule:File`
            # {{{
            dir <- normalizePath(dir, mustWork = TRUE)
            ori <- getwd()
            setwd(dir)
            on.exit(setwd(ori), add = TRUE)
            if (!self$is_valid_class("Schedule:File")) return(FALSE)
            # manually change the value instead of using `IdfObject$set_value()`
            # to speed up
            obj_ids <- self$object_ids(class = "Schedule:File")
            val_info <- private$value_tbl()[object_id %in% obj_ids][
                full_name == "File Name", list(value_id, value)]
            val_ids <- val_info$value_id
            vals <- val_info$value
            # get full path of external files
            val_paths <- normalizePath(vals, mustWork = FALSE)
            # get files that exist
            is_exist <- file.exists(val_paths)

            msg_exist <- NULL
            if (any(!is_exist)) {
                warning(paste0("Broken external file link found in Idf: ",
                    backtick(val_paths[!is_exist]), collapse = "\n"), call. = FALSE)
            }

            # get file directory that is not the same as target directory
            is_same_dir <- normalizePath(dirname(val_paths), mustWork = FALSE) == dir
            # get files that need copied
            to_copy <- is_exist & !is_same_dir
            targ <- val_paths[to_copy]
            # copy files into the target directory
            msg_copy <- NULL
            flgs <- TRUE
            if (not_empty(targ)) {
                flgs <- file.copy(targ, dir, overwrite = TRUE, copy.date = TRUE)
                if (any(!flgs)) {
                    stop(paste0("Failed to copy external file into the ",
                        "output directory: ", backtick(targ[!flgs]), collapse = "\n"),
                        call. = FALSEE)
                }
                # change value tbl
                targ_ids <- val_ids[to_copy][flgs]
                new_vals <- basename(targ[flgs])
                private$m_idf_tbl$value[value_id %in% targ_ids,
                    `:=`(value = new_vals, value_upper = toupper(new_vals))]
                targ_obj <- obj_ids[to_copy][flgs]
                private$m_log$order[object_id %in% targ_obj,
                    object_order := object_order + 1L]
                TRUE
            } else {
                FALSE
            }
            # }}}
        },

        run_info = function (weather, dir = NULL) {
            # get basic simulation run info
            # {{{
            # if the model is not created from a local file
            path_idf <- private$m_path
            msg <- NULL
            flg_sav <- FALSE
            if (is.null(path_idf)) {
                msg <- "The Idf was not created from local file."
                # and the output dir is not given
                if (is.null(dir)) {
                    # save it as a temp file and run it in temp dir
                    msg <- c(msg, "`dir` is not given.",
                    "The Idf will be saved as a temporary file and run in temporary directory.")
                    flg_sav <- TRUE
                    run_dir <- normalizePath(file.path(tempdir(), "eplusr", "idf"),
                        mustWork = FALSE)
                    path_idf <- normalizePath(tempfile(pattern = "idf_", tmpdir = run_dir, fileext = ".idf"),
                        mustWork = FALSE)
                # but output dir is given
                } else {
                    # save it to the given output dir with random name and run
                    # it in that dir
                    msg <- c(msg, "The Idf will be saved in the output",
                             "directory with a randon name.")
                    flg_sav <- TRUE
                    run_dir <- normalizePath(dir, mustWork = FALSE)
                    path_idf <- normalizePath(tempfile("model_", run_dir, ".idf"),
                        mustWork = FALSE)
                }
            # if the model is created from a local file
            } else {
                # but the output dir is not given
                if (is.null(dir)) {
                    # use the model path
                    run_dir <- dirname(path_idf)
                    path_idf <- normalizePath(file.path(run_dir, basename(path_idf)),
                        mustWork = FALSE)
                    if (self$is_unsaved()) {
                        msg <- c(msg, "The Idf has been modified before.",
                                 "It will be saved in the output directory",
                                 "before run.")
                        flg_sav <- TRUE
                    }
                # and the output dir is given
                } else {
                    # save it with same name in the output dir and run it there
                    run_dir <- normalizePath(dir, mustWork = FALSE)
                    path_idf <- normalizePath(file.path(run_dir, basename(path_idf)),
                        mustWork = FALSE)
                    if (!path_idf == private$m_path) flg_sav <- TRUE
                }
            }

            name_idf <- tools::file_path_sans_ext(basename(path_idf))
            # create output dir
            if (!dir.exists(run_dir)) {
                if (!flg_sav) {
                    warning("The Idf file has been deleted. It will be created ",
                            "using `Idf$save()` before run.", call. = FALSE)
                    flg_sav <- TRUE
                }
                tryCatch(dir.create(run_dir, recursive = TRUE),
                    warning = function (w) {
                        stop("Failed to create output directory: ",
                             backtick(run_dir), call. = FALSE)
                    }
                )
            }

            # resolve external file links
            flg_res <- private$resolve_external_links(run_dir)
            if (flg_res) flg_sav <- TRUE

            # save the model if necessary
            if (flg_sav) {
                readr::write_lines(self$string(), path_idf)
            }

            path_epw <- normalizePath(weather, mustWork = FALSE)
            name_epw <- tools::file_path_sans_ext(basename(weather))

            # save info
            private$m_run$out_dir <- run_dir
            private$m_run$path_idf <- path_idf
            private$m_run$path_epw <- path_epw
            private$m_run$name_idf <- name_idf
            private$m_run$name_epw <- name_epw
            # }}}
        },

        is_local_file = function () {
            # return TRUE if the model was created from a local file
            # {{{
            if (is.null(private$m_path)) return(FALSE)
            if (!file_test("-f", private$m_path)) return(FALSE)
            TRUE
            # }}}
        },

        sim_status = function (based_suffix = ".err") {
            # check simulation status
            # {{{
            # init
            local_file <- private$is_local_file() # if the model was created from local file
            status <- list(
                run_before = FALSE, # if the model has been run before or is still running
                changed_after = FALSE, # if the model has been changed after last simulation
                terminated = FALSE, # if last simulation was terminated
                successful = FALSE, # if last simulation was successful
                alive = FALSE, # if simulation is still running
                prefix = NULL # prefix of simulation output file
            )

            # if the model has not been run before
            if (is.null(private$m_run$proc)) {
                if (local_file) {
                    prefix <- tools::file_path_sans_ext(private$m_path)
                    basefile <- paste0(prefix, based_suffix)
                    if (!utils::file_test("-f", basefile)) return(status)
                    status$prefix <- prefix

                    # compare last changed time
                    err_ctime <- file.info(basefile)$ctime
                    idf_ctime <- file.info(private$m_path)$ctime
                    if (err_ctime < idf_ctime) status$changed_after <- TRUE
                }
            # if the model has been run before
            } else {
                status$run_before <- TRUE
                status$prefix <- tools::file_path_sans_ext(private$m_run$path_idf)
                # check if the model was run in waiting mode
                if (isTRUE(private$m_run$wait)) {
                    # check the exist status of last simulationa
                    exit_status <- private$m_run$proc$status
                    if (is.na(exit_status)) {
                        status$terminated <- TRUE
                    } else if (exit_status == 0) {
                        status$successful <- TRUE

                    }
                } else {
                    # check if the model is still running
                    if (private$m_run$proc$is_alive()) {
                        status$alive <- TRUE
                    } else if (private$m_run$proc$get_exit_status() == 0L) {
                        status$successful <- TRUE
                    }
                }
            }
            status
            # }}}
        },

        locate_output = function (suffix = ".err", strict = TRUE) {
            # locate output file
            # {{{
            status <- private$sim_status(suffix)
            if (is.null(status$prefix)) {
                if (!private$is_local_file()) {
                    stop("The Idf was not created from local file. Failed to ",
                         "locate simulation output.",
                         call. = FALSE)
                } else {
                    if (status$terminated) {
                        stop("Simulation was terminated before. Please solve ",
                             "the problems and re-run the simulation before collect ",
                             "output", call. = FALSE)
                    } else {
                        stop("Failed to locate simulation output in the Idf file folder", call. = FALSE)
                    }
                }
            } else {
                if (strict) {
                    if (status$terminated) {
                        stop("Simulation was terminated before. Please solve ",
                             "the problems and re-run the simulation before collect ",
                             "output", call. = FALSE)
                    }

                    if (status$alive) {
                        stop("Simulation is still running. Please wait simulation ",
                             "to finish before collecting results.", call. = FALSE)
                    }

                    if (status$changed_after) {
                        warning("The Idf has been changed since last simulation. ",
                                "The simulation output may not be correct.", call. = FALSE)
                    }

                    if (status$run_before & !status$successful) {
                        warning("Simulation ended with errors. Simulation results ",
                                "may not be correct.", call. = FALSE)
                    }
                }
            }
            paste0(status$prefix, suffix)
            # }}}
        },

        deep_clone = function (name, value) {
            # deep clone an Idf object
            # {{{
            if (name == "m_uuid") {
                uuid::UUIDgenerate(use.time = TRUE)
            } else if (name == "IdfObject") {
                # clone the IdfObject R6Class Generator
                clone_generator(value)
            } else if (is.environment(value) &  name != "IddObject") {
                list2env(as.list.environment(value, all.names = TRUE),
                         parent = emptyenv())
            } else {
                value
            }
            # }}}
        }
        # }}}
    )
)
# }}}

# ASSERTION ERROR MESSAGES

#' @importFrom assertthat "on_failure<-"
on_failure(Idf$public_methods$is_valid_class) <- function (call, env) {
    paste0("Invalid class name found for current ", eval(call$where, env),
           ": ", backtick(eval(call$class, env)), ".")
}

on_failure(Idf$public_methods$is_valid_id) <- function (call, env) {
    paste0("Invalid object id found for current Idf: ", backtick(eval(call$id, env)), ".")
}

on_failure(Idf$public_methods$is_valid_name) <- function (call, env) {
    paste0("Invalid object name found for current Idf: ", backtick(eval(call$name, env)), ".")
}

#' @export
# [.Idf {{{
'[.Idf' <- function(x, i, j, ..., drop = FALSE) {
    if (missing(i)) {
        stop("Missing object id or class name.", call. = FALSE)
    }
    if (missing(j)) {
        if (is.character(i)) {
            obj <- .subset2(x, "objects_in_class")(i)
        } else if (is.numeric(i)){
            obj <- .subset2(x, "objects")(i)
        }
    } else {
        if (is.character(i)) {
            obj <- .subset2(x, "objects_in_class")(i, j)
        } else if (is.numeric(i)){
            stop("j should not be given when i is object ids.", call. = FALSE)
        }
    }
    obj
}
# }}}

#' @export
# [[.Idf {{{
'[[.Idf' <- function(x, i, j, ..., drop = FALSE) {
    if (missing(i)) {
        stop("Missing object id or class name.", call. = FALSE)
    }
    if (missing(j)) {
        if (is.character(i)) {
            obj <- .subset2(x, "object_in_class")(i)
        } else if (is.numeric(i)){
            obj <- .subset2(x, "object")(i)
        }
    } else {
        if (is.character(i)) {
            obj <- .subset2(x, "object_in_class")(i, j)
        } else if (is.numeric(i)){
            stop("j should not be given when i is an object id.", call. = FALSE)
        }
    }
    obj
}
# }}}

#' @export
# length.Idf {{{
length.Idf <- function (x) {
    ids <- .subset2(x, "object_ids")()
    length(ids)
}
# }}}

#' @export
# format.Idf {{{
format.Idf <- function (x, header = TRUE, comment = TRUE,
                        format = c("sorted", "new_top", "new_bottom"),
                        digits = 8L, in_ip = FALSE, ...) {

    format <- match.arg(format)

    opt <- list(save_format = format,
                num_digits = digits,
                view_in_ip = in_ip,
                special_format = FALSE)

    priv <- .subset2(.subset2(model, ".__enclos_env__"), "private")

    val_tbl <- update_value_num(priv$value_tbl(), digits = digits, in_ip = in_ip)
    val_tbl <- val_tbl[priv$m_log$order, on = "object_id",
        nomatch = 0L]

    if (comment) {
        cmt_tbl <- priv$comment_tbl()
    } else {
        cmt_tbl <- NULL
    }

    main <- format_output(val_tbl, cmt_tbl, opt)
    main <- unlist(strsplit(main, "\n", fixed = TRUE))

    h <- NULL
    if (header) {
        h <- format_header(opt)
    } else {
        main <- main[-c(1,2)]
    }

    # add a blank line at the end like IDFEditor
    c(h, main, "")
}
# }}}

#' @export
# str.Idf {{{
str.Idf <- function (object, ...) {
    .subset2(object, "print")()
}
# }}}
