#' @importFrom R6 R6Class
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
        },

        all = function (type = c("id", "class", "field"), class = NULL)
            iall_idf(private, type, class),

        find = function (pattern, full = TRUE, ...)
            ifind_object(private, pattern, full, ...),

        get = function (id, echo = TRUE)
            iget_object(private, id, echo),

        add = function (class, ..., min = TRUE, echo = TRUE)
            iadd_object(self, private, class, min, ..., echo = echo),

        set = function (id, ..., echo = TRUE)
            iset_object(self, private, id, ..., echo = echo),

        dup = function (id, new_name = NULL, echo = TRUE)
            idup_object(self, private, id, new_name, echo),

        del = function (id, echo = TRUE)
            idel_object(self, private, id, echo),

        save = function (path, format = c("asis", "sorted", "ori_bot", "ori_top"))
            isave_idf(private, path = path, format = format),

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
iget_object <- function (private, id, echo) {
    get_object(private$model, id, verbose = echo)
}
# }}}

# iadd_object {{{
iadd_object <- function (self, private, class, min, ..., echo = TRUE) {
    private$model <- add_object(private$model, class, ..., min = min, idd = private$idd, verbose = echo)

    return(self)
}
# }}}

# iset_object {{{
iset_object <- function (self, private, id, ..., echo = TRUE) {
    private$model <- set_object(private$model, id, ..., idd = private$idd, verbose = echo)

    return(self)
}
# }}}

# idup_object {{{
idup_object <- function (self, private, id, new_name = NULL, echo = TRUE) {
    private$model <- dup_object(private$model, id, new_name, private$idd, verbose = echo)

    return(self)
}
# }}}

# idel_object {{{
idel_object <- function (self, private, id, echo = TRUE) {
    private$model <- del_object(private$model, id, private$idd, verbose = echo)

    return(self)
}
# }}}

# isave_idf {{{
isave_idf <- function (private, path, format = c("asis", "sorted", "ori_bot", "ori_top"), overwrite = FALSE) {
    if (missing(path)) {
        path <- private$path
        if (!overwrite) {
            stop(glue::glue("Saving will overwrite the original model located \\
                at {private$path}. This may have a risk of losing your original \\
                model. Comfirm by setting 'overwrite' to TRUE."), call. = FALSE)
        }
    }
    save_idf(private$model, path = path, format = format)
}
# }}}

# iprint_idf {{{
iprint_idf <- function (private) {
    count_obj <- setorder(private$model$class, group_order, class_order)[
        , .N, by = .(group, class, object_id)][
        , .(num = .N), by = .(group, class)][
        , num_obj := paste0("[", stringr::str_pad(num, 2, "left", "0"), "]")][
        , output := paste0(num_obj, " ", class)]

    output <- count_obj[count_obj[, .I[1], by = .(group)]$V1,
        output := paste0("\n", group, "\n", sep_line(), "\n", output)]

    cat("Path: ", private$path, "\n")
    cat("Version: ", private$ver, "\n")
    cat("Type: ", private$type, "\n")
    cat(sep_line("="), "\n")

    print_output(output)
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
    private$ver <- get_idf_ver(private$str)
    private$model <- parse_idf(private$str, idd = private$idd)
    private$type <- class(private$model)[1]

    return(self)
}
# }}}
