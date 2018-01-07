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
#' model <- eplus_model$new(path, idd = NULL)
#'
#' model$all(type, class = NULL)
#' model$contains(match, scale)
#' model$matches(match, ..., scale)
#' model$get(...)
#' model$add(class, ..., min = TRUE)
#' model$set(id, ...)
#' model$dup(id, new_name = NULL)
#' model$del(id, force = FALSE)
#' model$hide(id, force = FALSE)
#' model$notes(id, ..., append = FALSE, wrap = 0L)
#' model$diff(type)
#' model$check()
#' model$save(confirm = FALSE, format)
#' model$saveas(path, format, overwrite = FALSE)
#' model$print()
#' model$reset(confirm = FALSE)
#' model$run(period = ~., weather, echo = FALSE, dir = NULL, eplus_home = NULL)
#' model$collect(type = c("variable", "meter"), long = FALSE)
#' model$table(report = NULL, key = NULL, table = NULL)
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
#'   duplicated), modified and deleted objects with markers "(+)", "(~)", "(-)"
#'   respectively.
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
#' model$run(period = ~., weather, echo = FALSE, dir = NULL, eplus_home = NULL)
#' model$collect(type = c("variable", "meter", long = FALSE))
#' model$table(report = NULL, key = NULL, table = NULL)
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
#'    - `"2-1"~"4-30": Same as above.
#' * `weather`: The weather file used to run simulation. If missing, the
#'              chicago weather file
#'              ("USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw") in the
#'              distributed along with corresponding EnergyPlus will be used,
#'              and a warning message will be given.
#' * `echo`: Whether to print the standard output and error of EnergyPlus to
#'           the screen. Default is FALSE.
#' * `dir`: The directory to save the simulation results. If NULL, which is the
#'          default, the model folder will be used.
#' * `eplus_home`: The EnergyPlus installation folder path. If NULL, which is
#'                 the default, `eplusr` will try to find if corresponding
#'                 version of EnergyPlus was installed in the standard
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
#'
#' @importFrom R6 R6Class
#' @importFrom tools file_ext
#' @docType class
#' @export
eplus_model <- R6::R6Class(classname = "Energy+Model",
    # prevent modification
    lock_class = TRUE,

    public = list(
        initialize = function(path, idd = NULL) {
            private$path <- normalizePath(path, winslash = "/")
            private$str <- read_idf(path)
            private$ver <- get_idf_ver(private$str)
            private$idd <- get_idd(private$ver, idd)
            private$model <- parse_idf(private$str, idd = private$idd)
            private$type <- class(private$model)[1]
            private$time_read <- Sys.time()
            private$model$log <- data.table(step = 0, timestep = private$time_read,
                action = "init", id = 0L, new_id = 0L, active = TRUE)
        },

        all = function (type = c("id", "class", "field"), class = NULL)
            iall_idf(private, type, class),

        contains = function (match, scale = c("class", "field"))
            ifind_(private, pattern = match, scale = scale, fixed = TRUE),

        matches = function (match, ..., scale = c("class", "field"))
            ifind_(private, pattern = match, scale = scale, ...),

        get = function (...)
            iget_object(self, private, ...),

        add = function (class, ..., min = TRUE)
            iadd_object(self, private, class, min, ...),

        set = function (id, ...)
            iset_object(self, private, id, ...),

        dup = function (id, new_name = NULL)
            idup_object(self, private, id, new_name),

        del = function (id, force = FALSE)
            idel_object(self, private, id, force),

        hide = function (id, force = FALSE)
            ihide_object(self, private, id = id, force = force),

        notes = function (id, ..., append = FALSE, wrap = 0L)
            inotes(self, private, id, ..., append = append, wrap = wrap),

        diff = function (type = c("all", "add", "set", "del"))
            idiff_idf(self, private, type),

        check = function ()
            icheck_object(self, private),

        save = function (confirm = FALSE, format = c("asis", "sorted", "ori_bot", "ori_top"))
            isave_idf(private, format = format, confirm = confirm),

        saveas = function (path, format = c("asis", "sorted", "ori_bot", "ori_top"), overwrite = FALSE)
            isaveas_idf(private, path, format, overwrite),

        print = function ()
            iprint_idf(private),

        reset = function (confirm = FALSE)
            ireset_model(self, private, confirm),

        run = function (period = ~., weather = NULL, echo = FALSE, dir = NULL, eplus_home = NULL)
            irun_idf(self, private, period = period, weather, echo = echo,
                     dir = dir, eplus_home = eplus_home),

        collect = function (type = c("variable", "meter"), long = FALSE)
            icollect_varmeter(self, private, type = type, long = long),

        table = function (report = NULL, key = NULL, table = NULL)
            icollect_output(self, private, type = "table", report = report,
                            key = key, table = table)
    ),

    private = list(
        path = NULL,
        ver = NULL,
        type = NULL,
        model = NULL,
        str = NULL,
        idd = NULL,
        time_read = NULL,
        sim = list(),
        process = NULL
    )
)

# iall_idf {{{
iall_idf <- function (private, type = c("id", "class", "field"), class = NULL) {
    type <- match.arg(type)
    if(type == "field" && is.null(class)) {
        stop("'class' is required when type is 'field'.", call. = FALSE)
    }

    switch(type,
        id = valid_id(private$model),
        class = valid_class(private$model),
        field = valid_field(class = class, private$model, private$idd))
}
# }}}

# ifind_ {{{
ifind_ <- function (private, pattern, scale = c("class", "field"), ...) {
    scale <- match.arg(scale)

    if (scale == "class") {
        object = find_object(private$model, pattern, ...)
    } else {
        field = find_field(private$model, pattern, ...)
    }
}
# }}}

# iget_object {{{
iget_object <- function (self, private, ...) {
    private$model <- get_object(private$model, ...)

    return(self)
}
# }}}

# iadd_object {{{
iadd_object <- function (self, private, class, min, ...) {
    private$model <- add_object(private$model, class, ..., min = min, idd = private$idd)

    return(self)
}
# }}}

# iset_object {{{
iset_object <- function (self, private, id, ...) {
    private$model <- set_object(private$model, id, ..., idd = private$idd)

    return(self)
}
# }}}

# idup_object {{{
idup_object <- function (self, private, id, new_name = NULL) {
    private$model <- dup_object(private$model, id, new_name, private$idd)

    return(self)
}
# }}}

# idel_object {{{
idel_object <- function (self, private, id, force = FALSE) {
    private$model <- del_object(private$model, id, private$idd, force = force)

    return(self)
}
# }}}

# ihide_object {{{
ihide_object <- function (self, private, id, force = FALSE) {
    private$model <- del_object(private$model, id, private$idd, force = force, hide = TRUE)
    return(self)
}
# }}}

# inotes {{{
inotes <- function (self, private, id, ..., append = FALSE, wrap = 0L) {
    content <- unlist(c(...))

    if (is.null(append)) {
        private$model <- del_comment(private$model, id)
    }

    if (is_empty(content)) {
        private$model <- get_comment(private$model, id)
    } else {
        if (!is.null(append)) assertthat::assert_that(is.logical(append))
        private$model <- add_comment(private$model, id, append == append, type = 1L, content, log = TRUE, wrap = wrap)
    }

    return(self)
}
# }}}

# idiff_idf {{{
idiff_idf <- function (self, private, type = c("all", "add", "set", "del")) {
    type <- match.arg(type)

    private$model <- diff_idf(private$model, type)

    return(self)
}
# }}}

#' @importFrom tools file_ext
# isave_ {{{
isave_ <- function (private, path, format) {
    # check mismatch of file content and file extention.
    right_ext <- tolower(private$type)
    target_ext <- tolower(tools::file_ext(path))
    if (right_ext == "imf" && target_ext == "idf") {
        stop(msg(
            sprintf("The model has macro input and should be saved as an %s
                    file, not an %s file.",sQuote("imf"),sQuote("idf"))),
                 call. = FALSE)
    } else if (right_ext == "idf" && target_ext == "imf") {
        warning(msg(
            sprintf("The model has no macro input and should be saved as an %s
                    file. Saving it to %s will force to run Ep-Marco
                    preprocessor before simulation which is unnecessary.",
                    sQuote("idf"), sQuote("imf"))),
                    call. = FALSE)
    # other cases such as saving the model as a 'txt' file.
    } else if (right_ext != target_ext) {
        warning(msg(
            sprintf("The model should be saved as an %s file, but has been saved
                    with an extension %s which EnergyPlus may not able to
                    recognize.", sQuote(right_ext), sQuote(target_ext))), call. == FALSE)
    }

    save_idf(private$model, path, format)

    message(sprintf("Model has been successfully saved at %s.", sQuote(path)))

    pre_log <- private$model$log
    private$path <- normalizePath(path, winslash = "/")
    private$str <- read_idf(private$path)
    private$model <- parse_idf(private$str, idd = private$idd)
    private$model$log <- pre_log
    private$model <- add_log("save", id = 0L, new_id = 0L, private$model)
    return(invisible(NULL))
}
# }}}
# isave_idf {{{
isave_idf <- function (private, format = c("asis", "sorted", "ori_bot", "ori_top"),
                       confirm = FALSE) {
    if (!confirm) {
        stop(msg(
            sprintf("Saving will overwrite the original model located at %s.
                    This may have a risk of losing your original model. confirm
                    by setting 'confirm' to TRUE.", sQuote(private$path))),
                    call. = FALSE)
    }

    isave_(private, private$path, format)
}
# }}}
# isaveas_idf {{{
isaveas_idf <- function (private, path, format = c("asis", "sorted", "ori_bot", "ori_top"),
                         overwrite = FALSE) {
    if (file.exists(path) & !overwrite) {
        path <- normalizePath(path, winslash = "/")
        stop(msg(
            sprintf("Saving will replace an existing model file located at %s.
                    confirm by setting 'overwrite' to TRUE.", sQuote(path))),
                    call. = FALSE)
    }

    isave_(private, path, format)
}
# }}}

# icheck_object {{{
icheck_object <- function (self, private) {
    check_input <- private$model$value[!(required_field == FALSE & value == "")]

    suppressWarnings(check_object(check_input, idf = private$model))

    return(invisible(self))
}
# }}}

# iprint_idf {{{
iprint_idf <- function (private) {

    path <- paste0("[ Path  ]: ", private$path)
    ver  <- paste0("[Version]: ", private$ver)
    type <- paste0("[ Type  ]: ", private$type)
    info <- c(path, ver, type, sep_line("="))

    .print(private$model, info)
}
# }}}

# ireset_model {{{
ireset_model <- function (self, private, confirm = FALSE) {
    if (!confirm) {
        stop(msg(
            sprintf("Reset the model back to the status when it was first read
                    at %s. You will lose all modifications after that time and
                    resetting cannot be undone. confirm by setting 'confirm' to
                    TRUE.", sQuote(private$time_read))), call. = FALSE)
    }

    self$initialize(private$path)

    message(msg("The model has been reset to the status when it was first read
                at ", sQuote(private$time_read), "."))

    # Do not print
    return(invisible(self))
}
# }}}

# irun_idf {{{
irun_idf <- function (self, private, period, weather, echo = FALSE, dir = NULL,
                      eplus_home = NULL) {
    eplus_info <- eplus_path(private$ver, eplus_home)
    eplus_exe <- eplus_info["eplus"]

    # check if the model has been modified but not saved {{{
    log <- private$model$log
    acts <- log[, action]
    modify_act <- c("add", "set", "dup", "del", "hide", "notes")
    step_mod <- log[action %in% modify_act, step]
    step_save <- log[action == "save", step]
    if (not_empty(step_mod)) {
        if (is_empty(step_save)) step_save <- 0L
        assertthat::assert_that(max(step_mod) < max(step_save),
            msg = msg("The model has been changed without saving. Please save
                      or reset your model before run.")
        )
    }
    # }}}

    # set run period {{{
    idf <- set_runperiod(private$model, period, private$idd)

    annual <- design_day <- FALSE
    rp <- attr(idf, "runperiod")
    data.table::setattr(idf, "runperiod", NULL)

    if (as.character(rp[[1]]) == "annual") {
        annual <- TRUE
        rp_mes <- "Reset run period to 'Annual Simulation'"
    } else if (as.character(rp[[1]]) == "design_day") {
        design_day <- TRUE
        rp_mes <- "Reset run period to 'Design Day Simulation'"
    } else if (as.character(rp[[1]]) != "asis") {
        # Store the original time locale.
        ori_locale <- Sys.getlocale(category = "LC_TIME")
        # Change the time locale to the original value. In order to not print
        # the locale while run this function, store the output of
        # Sys.setlocale().
        on.exit(invisible(Sys.setlocale(category = "LC_TIME", locale = ori_locale)))
        # Change the time locale to "English_United States.1252".
        not_print <- Sys.setlocale(category = "LC_TIME", locale = "English_United States.1252")
        rp_mes <- sprintf("Reset run period to '%s - %s'",
                       format(rp[[1]], "%B %d"), format(rp[[2]], "%B %d"))
    } else {
        rp_mes <- NULL
    }
    # }}}

    # set default weather {{{
    if (is_empty(weather)) {
        weather <- eplus_info["epw"]
        if (is_empty(weather)) {
            stop(msg("Failed to use default weather file
                     'USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw'. Please
                     give 'weather' file path."), call. = FALSE)
        } else {
            warning(msg(
                "Missing weather input, weather file located at ",
                sQuote(weather), " will been used."), call. = FALSE)
        }
    }
    # }}}

    # auto correct table output style
    idf <- set_output_table_stype(idf, private$idd)

    # auto determine whether to expand objects
    if (has_hvac_template(private$model)) expand_obj <- TRUE else expand_obj <- FALSE
    if (not_empty(rp_mes)) message(rp_mes)

    # save the new model and update objects
    save_idf(idf, private$path)
    private$model <- idf

    data_run <- run_idf(eplus_exe, model = private$path, weather = weather,
                        output_dir = dir, design_day = design_day,
                        annual = annual, expand_obj = expand_obj, echo = echo)
    private$process <- data_run$process
    private$sim <- data_run$info
}
# }}}

# icollect_varmeter {{{
icollect_varmeter <- function (self, private, type = c("variable", "meter"), long = FALSE) {
    type <- match.arg(type)
    icollect_output(self, private, type = type, long = long)
}
# }}}
# icollect_output {{{
icollect_output <- function (self, private, type = c("variable", "meter", "table"),
                             long = FALSE, report = NULL, key = NULL, table = NULL) {
    type <- match.arg(type)

    # if the model has not been run
    if (is.null(private$process)) {
        path <- private$path
    } else {
        path <- private$sim$model
        if (private$process$is_alive()) {
            stop("Simulation is still running.", call. = FALSE)
            return(invisible(NULL))
        }
    }

    collect_eplus(path, output = type, long = long, report = report, key = key, table = table)
}
# }}}

# a helper to access private members of the `eplus_model` R6 class
.get <- function (model, x) {
    environment(model$initialize)$private[[x]]
}
