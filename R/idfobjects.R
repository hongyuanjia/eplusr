#' @importFrom R6 R6Class
#' @include impl-idf.R
NULL

#' A Collection of EnergyPlus Objects
#'
#' `IdfObjects` is a lazy collection view for multiple [IdfObject]s in the same
#' parent [Idf]. It stores object IDs and the parent `Idf`, and only creates
#' individual [IdfObject] instances when requested.
#'
#' @importFrom R6 R6Class
#' @docType class
#' @name IdfObjects
#' @seealso [Idf] class and [IdfObject] class
#' @author Hongyuan Jia
NULL

#' @export
# IdfObjects {{{
IdfObjects <- R6::R6Class(classname = "IdfObjects", lock_objects = FALSE,
    public = list(
        # INITIALIZE {{{
        #' @description
        #' Create an `IdfObjects` object.
        #'
        #' @details
        #' It is not recommended to directly use `$new()` to create an
        #' `IdfObjects` object. Use [Idf] methods such as
        #' \href{../../eplusr/html/Idf.html#method-objects}{\code{Idf$objects()}},
        #' \href{../../eplusr/html/Idf.html#method-objects_in_class}{\code{Idf$objects_in_class()}},
        #' and equivalent helpers instead.
        #'
        #' @param object An integer vector of object IDs.
        #' @param class An integer vector of class indexes. If `NULL`, class
        #'        indexes are inferred from `object`.
        #' @param parent An [Idf] object.
        #'
        #' @return An `IdfObjects` object.
        initialize = function(object, class = NULL, parent) {
            if (missing(parent) || !is_idf(parent)) {
                abort(paste("IdfObjects can only be created based on a parent Idf object.",
                    "Please give 'parent', which should be an Idf object.")
                )
            }

            object <- checkmate::assert_integerish(object, lower = 1L,
                any.missing = FALSE, coerce = TRUE)

            if (is.null(class)) {
                if (length(object)) {
                    obj <- get_idf_object(get_priv_env(parent)$idd_env(),
                        get_priv_env(parent)$idf_env(), object = object)
                    class <- obj[J(object), on = "object_id", class_id]
                } else {
                    class <- integer()
                }
            } else {
                class <- checkmate::assert_integerish(class, lower = 1L,
                    any.missing = FALSE, len = length(object), coerce = TRUE)
            }

            private$m_parent <- parent
            private$m_object_id <- object
            private$m_class_id <- class

            idfobjects_add_object_bindings(self, private)
        },
        # }}}

        # version {{{
        #' @description
        #' Get the version of the parent [Idf].
        #'
        #' @return A [base::numeric_version()] object.
        version = function()
            idfobjects_version(self, private),
        # }}}

        # parent {{{
        #' @description
        #' Get the parent [Idf].
        #'
        #' @return An [Idf] object.
        parent = function()
            idfobjects_parent(self, private),
        # }}}

        # length {{{
        #' @description
        #' Get the number of objects in the collection.
        #'
        #' @return A single integer.
        length = function()
            idfobjects_length(self, private),
        # }}}

        # id {{{
        #' @description
        #' Get object IDs.
        #'
        #' @return An integer vector.
        id = function()
            idfobjects_object_id(self, private),
        # }}}

        # object_id {{{
        #' @description
        #' Get object IDs.
        #'
        #' @return An integer vector.
        object_id = function()
            idfobjects_object_id(self, private),
        # }}}

        # name {{{
        #' @description
        #' Get object names.
        #'
        #' @return A character vector.
        name = function()
            idfobjects_object_name(self, private),
        # }}}

        # object_name {{{
        #' @description
        #' Get object names.
        #'
        #' @return A character vector.
        object_name = function()
            idfobjects_object_name(self, private),
        # }}}

        # class_name {{{
        #' @description
        #' Get class names.
        #'
        #' @return A character vector.
        class_name = function()
            idfobjects_class_name(self, private),
        # }}}

        # group_name {{{
        #' @description
        #' Get group names.
        #'
        #' @return A character vector.
        group_name = function()
            idfobjects_group_name(self, private),
        # }}}

        # object {{{
        #' @description
        #' Extract one [IdfObject] from the collection.
        #'
        #' @param which A single integer position or object name.
        #'
        #' @return An [IdfObject] object.
        object = function(which)
            idfobjects_object(self, private, which),
        # }}}

        # objects {{{
        #' @description
        #' Materialize the collection as a list of [IdfObject] objects.
        #'
        #' @param which Optional integer positions, logical indexes, or object
        #'        names used to subset before materializing. Default: `NULL`.
        #'
        #' @return A named list of [IdfObject] objects.
        objects = function(which = NULL)
            idfobjects_objects(self, private, which),
        # }}}

        # slice {{{
        #' @description
        #' Extract a subset as another `IdfObjects` collection.
        #'
        #' @param which Integer positions, logical indexes, or object names.
        #'
        #' @return An `IdfObjects` object.
        slice = function(which)
            idfobjects_slice(self, private, which),
        # }}}

        # to_table {{{
        #' @description
        #' Format the collection as a data.frame.
        #'
        #' @param string_value If `TRUE`, all field values are returned as
        #'        character. If `FALSE`, `value` column in returned
        #'        [data.table][data.table::data.table()] is a list column with
        #'        each value stored as corresponding type. Note that if the
        #'        value of numeric field is set to `"Autosize"` or
        #'        `"Autocalculate"`, it is left as it is, leaving the returned
        #'        type being a string instead of a number. Default: `TRUE`.
        #' @param unit Only applicable when `string_value` is `FALSE`. If
        #'        `TRUE`, values of numeric fields are assigned with units using
        #'        [units::set_units()] if applicable. This requires the suggested
        #'        package `units`. Default: `TRUE`.
        #' @param wide Only applicable if target objects belong to a same class.
        #'        If `TRUE`, a wide table will be returned, i.e. first three
        #'        columns are always `id`, `name` and `class`, and then every
        #'        field in a separate column. Default: `FALSE`.
        #' @param align If `TRUE`, all objects in the same class will have the
        #'        same field number. Default: `FALSE`.
        #' @param all If `TRUE`, all available fields defined in IDD for the
        #'        class that objects belong to will be returned. Default:
        #'        `FALSE`.
        #' @param group_ext Should be one of `"none"`, `"group"` or `"index"`.
        #'        If not `"none"`, `value` column in returned
        #'        [data.table::data.table()] will be converted into a list.
        #'        If `"group"`, values from extensible fields will be grouped by the
        #'        extensible group they belong to. If `"index"`, values from
        #'        extensible fields will be grouped by the extensible field
        #'        indice they belong to. Default: `"none"`.
        #' @param force If `TRUE`, the object IDs are not checked against the
        #'        parent `Idf` before table generation. Default: `FALSE`.
        #'
        #' @return A [data.table][data.table::data.table()].
        to_table = function(string_value = TRUE, unit = TRUE, wide = FALSE,
                            align = FALSE, all = FALSE,
                            group_ext = c("none", "group", "index"),
                            force = FALSE)
            idfobjects_to_table(self, private, string_value, unit, wide,
                align, all, match.arg(group_ext), force),
        # }}}

        # to_string {{{
        #' @description
        #' Format the collection as IDF strings.
        #'
        #' @param comment If `FALSE`, all comments will not be included.
        #'        Default: `TRUE`.
        #' @param header If `TRUE`, the IDF header will be included. Default:
        #'        `FALSE`.
        #' @param format Specific format used when formatting. For details,
        #'        see \href{../../eplusr/html/Idf.html#method-save}{\code{Idf$save()}}.
        #' @param leading Leading spaces added to each field. Default: `4L`.
        #' @param sep_at The character width to separate value string and field
        #'        string. Default: `29L`, the same as IDF Editor.
        #'
        #' @return A character vector.
        to_string = function(comment = TRUE, header = FALSE,
                             format = eplusr_option("save_format"),
                             leading = 4L, sep_at = 29L)
            idfobjects_to_string(self, private, comment, header, format,
                leading, sep_at),
        # }}}

        # print {{{
        #' @description
        #' Print the collection.
        #'
        #' @param zoom Control how detailed the collection should be printed.
        #'        Should be one of `"object"` and `"field"`. Default:
        #'        `"object"`.
        #' @param order If `TRUE`, objects are printed in collection order. If
        #'        `FALSE`, objects are grouped and ordered by class. Default:
        #'        `TRUE`.
        #' @param n Maximum number of objects to print. `NULL` prints all
        #'        objects. Default: `NULL`.
        #'
        #' @return The `IdfObjects` object itself, invisibly.
        print = function(zoom = c("object", "field"), order = TRUE, n = NULL)
            idfobjects_print(self, private, match.arg(zoom), order, n)
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_parent = NULL,
        m_object_id = integer(),
        m_class_id = integer(),
        # }}}

        # PRIVATE FUNCTIONS {{{
        idf_env = function() get_priv_env(private$m_parent)$idf_env(),
        idd_env = function() get_priv_env(private$m_parent)$idd_env(),
        log_env = function() get_priv_env(private$m_parent)$m_log
        # }}}
    )
)
# }}}

# idfobjects_version {{{
idfobjects_version <- function(self, private) {
    private$m_parent$version()
}
# }}}
# idfobjects_parent {{{
idfobjects_parent <- function(self, private) {
    private$m_parent
}
# }}}
# idfobjects_length {{{
idfobjects_length <- function(self, private) {
    length(private$m_object_id)
}
# }}}
# idfobjects_object_id {{{
idfobjects_object_id <- function(self, private) {
    private$m_object_id
}
# }}}
# idfobjects_object_name {{{
idfobjects_object_name <- function(self, private) {
    if (!length(private$m_object_id)) return(character())
    private$idf_env()$object[J(private$m_object_id), on = "object_id", object_name]
}
# }}}
# idfobjects_class_name {{{
idfobjects_class_name <- function(self, private) {
    if (!length(private$m_class_id)) return(character())
    private$idd_env()$class[J(private$m_class_id), on = "class_id", class_name]
}
# }}}
# idfobjects_group_name {{{
idfobjects_group_name <- function(self, private) {
    if (!length(private$m_class_id)) return(character())
    cls <- private$idd_env()$class[J(private$m_class_id), on = "class_id", group_id]
    private$idd_env()$group[J(cls), on = "group_id", group_name]
}
# }}}
# idfobjects_match_index {{{
idfobjects_match_index <- function(self, private, which, single = FALSE) {
    if (missing(which) || is.null(which)) return(seq_along(private$m_object_id))

    n <- length(private$m_object_id)
    if (is.character(which)) {
        nm <- idfobjects_object_name(self, private)
        pos <- match(which, nm)
        if (anyNA(pos)) abort(paste0("Unknown object name: ", surround(which[is.na(pos)][[1L]]), "."))
    } else if (is.logical(which)) {
        pos <- seq_len(n)[which]
        if (anyNA(pos)) abort("Invalid object index.")
    } else if (is.numeric(which)) {
        which <- checkmate::assert_integerish(which, any.missing = FALSE, coerce = TRUE)
        pos <- seq_len(n)[which]
        if (anyNA(pos)) abort("Invalid object index.")
    } else {
        abort("'which' must be integer, logical, or character.")
    }

    if (single && length(pos) != 1L) abort("'which' must identify exactly one object.")
    pos
}
# }}}
# idfobjects_object {{{
idfobjects_object <- function(self, private, which) {
    pos <- idfobjects_match_index(self, private, which, single = TRUE)
    IdfObject$new(private$m_object_id[[pos]], private$m_class_id[[pos]], private$m_parent)
}
# }}}
# idfobjects_objects {{{
idfobjects_objects <- function(self, private, which = NULL) {
    pos <- idfobjects_match_index(self, private, which)
    res <- apply2(private$m_object_id[pos], private$m_class_id[pos],
        IdfObject$new, list(parent = private$m_parent))
    setattr(res, "names", idfobjects_object_name(self, private)[pos])[]
}
# }}}
# idfobjects_slice {{{
idfobjects_slice <- function(self, private, which) {
    pos <- idfobjects_match_index(self, private, which)
    IdfObjects$new(private$m_object_id[pos], private$m_class_id[pos], private$m_parent)
}
# }}}
# idfobjects_to_table {{{
idfobjects_to_table <- function(self, private, string_value = TRUE, unit = TRUE,
                                wide = FALSE, align = FALSE, all = FALSE,
                                group_ext = c("none", "group", "index"),
                                force = FALSE) {
    get_idf_table(private$idd_env(), private$idf_env(), object = private$m_object_id,
        string_value = string_value, unit = unit, wide = wide, align = align,
        all = all, group_ext = match.arg(group_ext), force = force)
}
# }}}
# idfobjects_to_string {{{
idfobjects_to_string <- function(self, private, comment = TRUE, header = FALSE,
                                 format = eplusr_option("save_format"),
                                 leading = 4L, sep_at = 29L) {
    if (format == "asis") format <- private$log_env()$save_format
    get_idf_string(private$idd_env(), private$idf_env(), private$log_env()$order,
        object = private$m_object_id, comment = comment, header = header,
        format = format, leading = leading, sep_at = sep_at)
}
# }}}
# idfobjects_print {{{
idfobjects_print <- function(self, private, zoom = c("object", "field"),
                             order = TRUE, n = NULL) {
    zoom <- match.arg(zoom)
    checkmate::assert_flag(order)
    checkmate::assert_count(n, null.ok = TRUE)

    total <- length(private$m_object_id)
    object_id <- private$m_object_id
    if (!is.null(n) && n < total) object_id <- object_id[seq_len(n)]

    cli::cat_rule("EnergyPlus IDF Objects", line = 1)
    cli::cat_line(cli::ansi_strtrim(c(
        paste0(" ", cli::symbol$bullet, " Version: ", surround(private$m_parent$version())),
        paste0(" ", cli::symbol$bullet, " Objects: ", total)
    )))
    if (!length(object_id)) return(invisible(self))

    cli::cat_line()

    if (zoom == "object") {
        brief <- FALSE
        nest <- !order
        component <- c("class", "object")

        dt <- private$idf_env()$object[J(object_id), on = "object_id",
            .SD, .SDcols = c("class_id", "object_id", "object_name")]
        add_joined_cols(private$idd_env()$class, dt, "class_id", "class_name")
    } else {
        brief <- FALSE
        nest <- !order
        component <- c("class", "object", "value")

        add_idf_format_cols(private$idd_env(), private$idf_env())
        on.exit(del_idf_format_cols(private$idd_env(), private$idf_env()), add = TRUE)

        add_joined_cols(private$idd_env()$field, private$idf_env()$value, "field_id", "type_enum")
        on.exit(set(private$idf_env()$value, NULL, "type_enum", NULL), add = TRUE)

        add_joined_cols(private$idf_env()$object, private$idf_env()$value, "object_id", "object_name")
        on.exit(set(private$idf_env()$value, NULL, "object_name", NULL), add = TRUE)

        dt <- private$idf_env()$value[J(object_id), on = "object_id"]
    }

    out <- unlist(format_objects(dt, component, brief = brief, nest = nest,
        order = nest)$out, use.names = FALSE)
    if (length(out)) out <- out[-length(out)]
    if (length(out)) cli::cat_line(cli::ansi_strtrim(out))

    if (!is.null(n) && n < total) {
        cli::cat_line()
        cli::cat_line(sprintf("... %i more object(s) not shown.", total - n))
    }

    invisible(self)
}
# }}}
# idfobjects_add_object_bindings {{{
idfobjects_add_object_bindings <- function(self, private) {
    nm <- idfobjects_object_name(self, private)
    if (!length(nm)) return(invisible())

    bind <- !is.na(nm) & !duplicated(nm) & !(nm %chin% ls(self, all.names = TRUE))
    if (!any(bind)) return(invisible())

    for (i in which(bind)) {
        makeActiveBinding(nm[[i]], local({
            id <- private$m_object_id[[i]]
            cls <- private$m_class_id[[i]]
            parent <- private$m_parent
            function(value) {
                if (!missing(value)) {
                    if (is_idfobject(value)) {
                        value_private <- get_priv_env(value)
                        if (identical(id, value_private$m_object_id) &&
                            identical(parent, value_private$m_parent)) {
                            return(invisible(value))
                        }
                    }
                    abort("Cannot assign to an IdfObjects object binding.")
                }
                IdfObject$new(id, cls, parent)
            }
        }), self)
    }

    invisible()
}
# }}}

#' @export
# length.IdfObjects {{{
length.IdfObjects <- function(x) {
    x$length()
}
# }}}

#' @export
# as.list.IdfObjects {{{
as.list.IdfObjects <- function(x, ...) {
    x$objects()
}
# }}}

#' @export
# format.IdfObjects {{{
format.IdfObjects <- function(x, comment = TRUE, header = FALSE,
                              format = eplusr_option("save_format"),
                              leading = 4L, sep_at = 29L, ...) {
    paste0(x$to_string(comment = comment, header = header, format = format,
        leading = leading, sep_at = sep_at), collapse = "\n")
}
# }}}

#' @export
# as.character.IdfObjects {{{
as.character.IdfObjects <- function(x, comment = TRUE, header = FALSE,
                                    format = eplusr_option("save_format"),
                                    leading = 4L, sep_at = 29L, ...) {
    x$to_string(comment = comment, header = header, format = format,
        leading = leading, sep_at = sep_at)
}
# }}}

#' @export
# str.IdfObjects {{{
str.IdfObjects <- function(object, ...) {
    object$print(...)
}
# }}}

#' @export
# print.IdfObjects {{{
print.IdfObjects <- function(x, zoom = c("object", "field"), order = TRUE,
                             n = NULL, ...) {
    x$print(zoom = match.arg(zoom), order = order, n = n)
}
# }}}

# vim: set fdm=marker:
