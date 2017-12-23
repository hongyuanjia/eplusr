#' @importFrom R6 R6Class
#' @docType class
#' @export
eplus_model <- R6::R6Class(classname = "Energy+Model",
    # prevent modification
    lock_class = TRUE,

    public = list(
        path = NULL,
        ver = NULL,
        type = NULL,
        model = NULL,

        initialize = function(path, idd = NULL) {
            self$path <- normalizePath(path, winslash = "/")
            private$str <- read_idf(path)
            self$ver <- get_idf_ver(private$str)
            private$idd <- get_idd(self$ver, idd)
            self$model <- parse_idf(private$str, idd = private$idd)
            self$type <- class(self$model)[1]
            private$time_read <- Sys.time()
        },

        all = function (type = c("id", "class", "field"), class = NULL)
            iall_idf(self, private, type, class),

        find = function (pattern, full = TRUE, ...)
            ifind_object(self, pattern, full, ...),

        get = function (id, echo = TRUE)
            iget_object(self, id, echo),

        add = function (class, ..., min = TRUE, echo = TRUE)
            iadd_object(self, private, class, min, ..., echo = echo),

        set = function (id, ..., echo = TRUE)
            iset_object(self, private, id, ..., echo = echo),

        dup = function (id, new_name = NULL, echo = TRUE)
            idup_object(self, private, id, new_name, echo),

        del = function (id, echo = TRUE)
            idel_object(self, private, id, echo),

        save = function (path, format = c("asis", "sorted", "ori_bot", "ori_top"))
            isave_idf(self, path = path, format = format),

        print = function ()
            iprint_idf(self),

        reset = function (comfirm = FALSE)
            ireset_model(self, private, comfirm)
    ),

    private = list(
        str = NULL,
        idd = NULL,
        time_read = NULL
    )
)

# iprint_idf {{{
iprint_idf <- function (self) {
    count_obj <- setorder(self$model$class, group_order, class_order)[
        , .N, by = .(group, class, object_id)][
        , .(num = .N), by = .(group, class)][
        , num_obj := paste0("[", stringr::str_pad(num, 2, "left", "0"), "]")][
        , output := paste0(num_obj, " ", class)]

    output <- count_obj[count_obj[, .I[1], by = .(group)]$V1,
        output := paste0("\n", group, "\n", sep_line(), "\n", output)]

    cat("Path: ", self$path, "\n")
    cat("Version: ", self$ver, "\n")
    cat("Type: ", self$type, "\n")
    cat(sep_line("="), "\n")

    print_output(output)
}
# }}}

# isave_idf {{{
isave_idf <- function (self, path, format = c("asis", "sorted", "ori_bot", "ori_top"), overwrite = FALSE) {
    if (missing(path)) {
        path <- self$path
        if (!overwrite) {
            stop(glue::glue("Saving will overwrite the original model located \\
                at {self$path}. This may have a risk of losing your original \\
                model. Comfirm by setting 'overwrite' to TRUE."), call. = FALSE)
        }
    }
    save_idf(self$model, path = path, format = format)
}
# }}}

# iall_idf {{{
iall_idf <- function (self, private, type = c("id", "class", "field"), class = NULL) {
    type <- match.arg(type)
    if(type == "field" && is.null(class)) {
        stop("'class' is required when type is 'field'.", call. = FALSE)
    }

    switch(type,
        id = valid_id(self$model),
        class = valid_class(self$model),
        field = valid_field(class = class, self$model, private$idd))
}
# }}}

# ifind_object {{{
ifind_object <- function (self, pattern, full = TRUE, ...) {
    find_object(self$model, pattern, full, ...)
}
# }}}

# iget_object {{{
iget_object <- function (self, id, echo) {
    get_object(self$model, id, verbose = echo)
}
# }}}

# idup_object {{{
idup_object <- function (self, private, id, new_name = NULL, echo = TRUE) {
    self$model <- dup_object(self$model, id, new_name, private$idd, verbose = echo)

    return(self)
}
# }}}

# iadd_object {{{
iadd_object <- function (self, private, class, min, ..., echo = TRUE) {
    self$model <- add_object(self$model, class, ..., min = min, idd = private$idd, verbose = echo)

    return(self)
}
# }}}

# idel_object {{{
idel_object <- function (self, private, id, echo = TRUE) {
    self$model <- del_object(self$model, id, private$idd, verbose = echo)

    return(self)
}
# }}}

# iset_object {{{
iset_object <- function (self, private, id, ..., echo = TRUE) {
    self$model <- set_object(self$model, id, ..., idd = private$idd, verbose = echo)

    return(self)
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
    self$ver <- get_idf_ver(private$str)
    self$model <- parse_idf(private$str, idd = private$idd)
    self$type <- class(self$model)[1]

    return(self)
}
# }}}
