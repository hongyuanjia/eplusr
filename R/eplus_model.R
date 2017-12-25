#' @importFrom R6 R6Class
#' @docType class
#' @export
eplus_model <- R6::R6Class(classname = "Energy+Model",
    # prevent modification
    lock_class = TRUE,

    public = list(
        getPrivate = function () private,

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

        find = function (pattern, full = TRUE, ...)
            ifind_object(private, pattern, full, ...),

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

        diff = function (type = c("all", "add", "set", "del"))
            idiff_idf(self, private, type),

        save = function (comfirm = FALSE, format = c("asis", "sorted", "ori_bot", "ori_top"))
            isave_idf(private, format = format, comfirm = comfirm),

        saveas = function (path, format = c("asis", "sorted", "ori_bot", "ori_top"), overwrite = FALSE)
            isaveas_idf(private, path, format, overwrite),

        print = function ()
            iprint_idf(private),

        reset = function (comfirm = FALSE)
            ireset_model(self, private, comfirm)
    ),

    private = list(
        path = NULL,
        ver = NULL,
        type = NULL,
        model = NULL,
        str = NULL,
        idd = NULL,
        time_read = NULL
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

# ifind_object {{{
ifind_object <- function (private, pattern, full = TRUE, ...) {
    find_object(private$model, pattern, full, ...)
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
    private$model <- del_object(private$model, id, private$idd)

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
        stop(glue::glue("The model has macro input and should be saved as an \\
            'imf' file, not an 'idf' file."), call. = FALSE)
    } else if (right_ext == "idf" && target_ext == "imf") {
        warning(glue::glue("The model has no macro input and should be saved \\
            as an 'idf' file. Saving it to 'imf' will force to run Ep-Marco \\
            preprocessor before simulation which is unnecessary."), call. = FALSE)
    # other cases such as saving the model as a 'txt' file.
    } else if (right_ext != target_ext) {
        warning(glue::glue("The model should be saved as an '{right_ext}' file \\
            , but has been saved with an extension '{target_ext}' which \\
            EnergyPlus may not able to recognize."), call. == FALSE)
    }

    save_idf(private$model, path, format)

    message(glue::glue("Model has been successfully saved at {path}."))

    return(invisible(NULL))
}
# }}}
# isave_idf {{{
isave_idf <- function (private, format = c("asis", "sorted", "ori_bot", "ori_top"),
                       comfirm = FALSE) {
    if (!comfirm) {
        stop(glue::glue("Saving will overwrite the original model located \\
            at {private$path}. This may have a risk of losing your original \\
            model. Comfirm by setting 'comfirm' to TRUE."), call. = FALSE)
    }

    isave_(private, private$path, format)
}
# }}}
# isaveas_idf {{{
isaveas_idf <- function (private, path, format = c("asis", "sorted", "ori_bot", "ori_top"),
                         overwrite = FALSE) {
    if (file.exists(path) & !overwrite) {
        path <- normalizePath(path, winslash = "/")
        stop(glue::glue("Saving will replace an existing model file located at \\
            {path}. Comfirm by setting 'overwrite' to TRUE."), call. = FALSE)
    }

    isave_(private, path, format)
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
ireset_model <- function (self, private, comfirm = FALSE) {
    if (!comfirm) {
        stop(glue::glue("Reset the model back to the status when it was first \\
            read at {private$time_read}. You will lose all modifications after \\
            that time and resetting cannot be undone. Comfirm by setting \\
            'comfirm' to TRUE."), call. = FALSE)
    }

    self$initialize(private$path)

    message(glue::glue("The model has been reset to the status when it was \\
       first read at {private$time_read}."))

    # Do not print
    return(invisible(self))
}
# }}}
