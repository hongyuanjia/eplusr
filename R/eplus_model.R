#' @importFrom R6 R6Class
#' @docType class
#' @export
eplus_model <- R6::R6Class(classname = "Energy+Model",
    public = list(
        path = NULL,
        ver = NULL,
        type = NULL,
        model = NULL,

        initialize = function(path, idd = NULL) {
            self$path <- normalizePath(path, winslash = "/")
            self$model <- parse_idf(path, idd = idd)
            self$ver <- self$model$ver
            self$type <- class(self$model)[1]
        },

        save = function(path, format = c("asis", "sorted", "ori_bot", "ori_top"))
            isave_idf(self, path = path, format = format),

        print = function()
            iprint_idf(self)
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
isave_idf <- function (self, path, format = c("asis", "sorted", "ori_bot", "ori_top")) {
    if (missing(path)) path <- self$path
    save_idf(self$idf, path = path, format = format)
}
# }}}

# ilist_idf {{{
ilist_idf <- function (self, type = c("id", "class", "")) {

}
# }}}
