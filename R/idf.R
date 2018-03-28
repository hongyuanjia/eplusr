#' Parse an EnergyPlus IDF file
#'
#' \code{IDF} R6 class is the core class that enable fully access to and
#' manipulate EnergyPlus IDF models.
#'
#' @section Usage:
#' ```
#'
#' idf <- IDF$new(path, idd)
#'
#' idf$version()
#' idf$id_of_class(class)
#' idf$class_of_id(id)
#' idf$all_class(scope = "idf")
#' idf$all_id()
#'
#' idf$is_valid_class(class, scope = "idf")
#' idf$is_valid_id(id)
#'
#' idf$object(id)
#' idf$objects(id)
#' idf$objects_in_class(class)
#' idf$objects_in_group(group)
#'
#' idf$out(format = c("asis", "sorted", "ori_bot", "ori_top"), comment = TRUE,
#' in_ip = FALSE)
#'
#' idf$print()
#'
#' print(idf)
#'
#' ```
#'
#' @section Arguments:
#'
#' * `path`: Path to an EnergyPlus Input Data Dictionary (IDD) file, usually
#' named as `Energy+.idd`.
#'
#' * `group`: A valid group name or valid group names.
#'
#' * `class`: A valid class name or valid class names.
#'
#' @section Detail:
#'
#' `IDD$new()` parses an EnergyPlus Input Data Dictionary (IDD) file, and
#' returns an IDD object.
#'
#' `$version()` returns the version string of current idd file.
#'
#' `$build()` returns the build tag string of current idd file.
#'
#' `$group_name(class)` returns group name that that `class` belong to.
#'
#' `$class_name(group)` returns class names of that `group`. If `group` not
#' given, all class names in current IDD are returned.
#'
#' `$group_order(group)` returns integer orders (orders of name apperarance in
#' the IDD file) of that `group`.
#'
#' `$class_order(class)` returns integer orders (orders of name apperarance in
#' the IDD file) of that `class`.
#'
#' `$orders()` returns the all order data (stored as a data.table with four
#' columns, i.e. "group", "group_order", "class", "class_order").
#'
#' `$object(class)` returns an IDDObject of that `class`.
#'
#' `$objects(class)` returns a list of IDDObjects of `class`es. If `class` is
#' NULL, all IDDObjects in current IDD are returned.
#'
#' `$objects_in_group(group)` returns a list of IDDObjects in that `group`.
#'
#' `$is_valid_group(group)` return `TRUE` if the input is a valid `group` name.
#'
#' `$is_valid_class(class)` return `TRUE` if the input is a valid `class` name.
#'
#' @importFrom R6 R6Class
#' @importFrom purrr map walk
#' @importFrom assertthat assert_that
#' @importFrom cli cat_rule cat_line
#' @importFrom data.table rleid
#' @return An IDF object
#' @docType class
#' @name IDF
#' @author Hongyuan Jia
#' @references
#' \href{https://github.com/NREL/EnergyPlus/tree/develop/src/IDF_Editor}{IDFEditor
#' source code}
NULL

#' @export
# IDF {{{
IDF <- R6::R6Class(classname = "IDF",

    public = list(

        initialize = function (path, idd) {
            # {{{
            idf_file <- parse_idf(path, idd)
            if (length(path) == 1L) {
                if (file.exists(path)) private$m_path <- path
            }
            private$m_idd <- idd
            private$get_order_data(idf_file$value)

            private$m_reference <- idd$reference()
            private$m_external <- idd$external()
            private$get_refmap_class(idf_file$value)
            private$get_refmap_field(idf_file$value)

            # TODO: create a version object if it does not exists
            private$m_version <- idf_file$version
            private$m_options <- idf_file$options
            IDFObject$private_fields$m_refmap <- private$m_refmap
            IDFObject$private_fields$m_order <- private$m_order
            objects <- purrr::pmap(idf_file$value, IDFObject$new, idd = idd)
            private$m_objects <- idf_file$value[, `:=`(object = objects, value = NULL)]
            # }}}
        },

        # PUBLIC FUNCTIONS
        # {{{
        version = function () {
            # return version of current IDF
            # {{{
            private$m_version
            # }}}
        },

        id_of_class = function (class) {
            # return all valid id in one class or in current IDF
            # {{{
            assert_that(self$is_valid_class(class))
            cls <- class
            private$m_objects[class == cls, object_id]
            # }}}
        },

        class_of_id = function (id) {
            # return the class name of given object id in current IDF
            # {{{
            assert_that(self$is_valid_id(id))
            private$m_objects[object_id == id, class]
            # }}}
        },

        all_class = function (scope = "idf") {
            # return all valid class in current IDF
            # {{{
            scope <- match.arg(scope, choices = c("idf", "idd"))
            switch(scope,
                   idf = unique(private$m_objects$class),
                   idd = private$m_idd$class_name())
            # }}}
        },

        all_id = function () {
            # return all object ids in current IDF
            # {{{
            private$m_objects$object_id
            # }}}
        },

        is_valid_class = function (class, scope = "idf") {
            # check if the input string is a valid class name in current IDF
            # {{{
            assert_that(is_string(class))
            class %in% self$all_class(scope)
            # }}}
        },

        is_valid_id = function (id) {
            # check if the input number is a valid object id in current IDF
            # {{{
            assert_that(is_scalar(id), is_integerish(id))
            id %in% self$all_id()
            # }}}
        },

        object = function (id) {
            # return a single object
            # {{{
            assert_that(self$is_valid_id(id))
            private$m_objects[object_id == id, object][[1]]
            # }}}
        },

        objects = function (id) {
            # return a list which contains all objects with input object ids
            # {{{
            purrr::walk(id, ~assert_that(self$is_valid_id(.x)))
            private$m_objects[object_id %in% id, object]
            # }}}
        },

        object_in_class = function (class, index = NULL) {
            # return a list which contails all objects in the target class
            # {{{
            ids <- self$id_of_class(class)
            cls <- class
            res <- private$m_objects[class == cls, object]
            if (is_empty(index)) {
                return(res[[1]])
            } else {
                assert_that(is_count(index))
                n <- length(res)
                assert_that(index <= n,
                    msg = paste0("Invalid object index found for class ",
                                 backtick(class), ": ",
                                 backtick_collapse(index), ". Only ",
                                 n, ifelse(n > 1L, " objects exist.")))
                return(res[[index]])
            }
            # }}}
        },

        objects_in_class = function (class, index = NULL) {
            # return a list which contails all objects in the target class
            # {{{
            ids <- self$id_of_class(class)
            cls <- class
            res <- private$m_objects[class == cls, object]
            if (not_empty(index)) {
                purrr::walk(index, ~assert_that(is_count(.x)))
                n <- length(res)
                valid <- index <= n
                assert_that(all(valid),
                    msg = paste0("Invalid object index found for class ",
                                 backtick(class), ": ",
                                 backtick_collapse(index[!valid]), ". Only ",
                                 n, ifelse(n > 1L, " objects exist.", " object exists.")))
                return(res[index])
            } else {
                return(res)
            }
            # }}}
        },

        dup_object = function (id) {
            # duplicate an Object
            # {{{

            # }}}
        },

        add_object = function (class) {
            # add a new object in class
            # {{{

            # }}}
        },

        set_object = function (id, ..., comment = NULL) {
            # set values in an object
            # {{{
            assert_that(self$is_valid_id(id))
            self$object(id)$set_value(...)
            # private$m_objects[object_id == id, object := object$set_value(...)]
            # }}}
        },

        set_objects_in_class = function (class, index = NULL) {
            # set values across all or some objects in a class
            # {{{
            # }}}
        },

        del_object = function (id) {
            # delete an object
            # {{{

            # }}}
        },

        diff = function (id) {
            # diff the values
            # {{{

            # }}}
        },

        validate = function () {
            # check if there are errors in current model
            # {{{
            private$fetch_check_data()
            i_check(private, type = "idf")
            # }}}
        },

        out = function (format = c("asis", "sorted", "ori_bot", "ori_top"),
                        comment = TRUE, in_ip = FALSE) {
            # return save-ready format string
            # {{{
            format <- private$out_format(format)
            header <- private$out_header(format)
            lines <- private$out_line(format = format, comment = comment)
            c(header, lines)
            # }}}
        },

        print = function () {
            # {{{
            # get object count
            res <- private$m_objects[, list(num = .N), by = c("group", "class")]

            max_count <- res[, max(num)]

            res <- res[, count := paste0(
                "[", stringr::str_pad(num, nchar(max_count), "left", "0"), "]")]

            res[, grp := ""]

            res[res[, .I[1L], by = list(group)]$V1,
                grp := paste0("\nGroup: ", backtick(group), "\n", cli::rule(), "\n")]

            out <- res[, paste0(grp, count, " ", class)]

            cat(out, sep = "\n")
            # }}}
        }
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS
        # {{{
        m_path = NULL,
        m_version = NULL,
        m_options = list(),
        m_idd = NULL,
        m_objects = data.frame(),
        m_refmap = new.env(parent = emptyenv(), size = 2L),
        m_order = new.env(parent = emptyenv(), size = 1L),
        m_reference = list(),
        m_external = list(),
        m_check = list(),
        m_check_class = data.frame(),
        m_check_field = data.frame(),
        # }}}

        # PRIVATE FUNCTIONS
        # {{{
        fetch_cache = function () {
            # cache

        },

        fetch_check_data = function () {
            # fetch values from all objects
            # {{{
            # get class checking data
            # {{{
            class_info <- ._get_private(private$m_idd)$m_objects[
                , .SD, .SDcol = c("class_order", "class", "format",
                                  "min_fields", "num_fields", "required_object",
                                  "unique_object", "extensible")]
            private$m_check_class <- merge(class_info, private$m_objects,
                by = c("class_order", "class"), all = TRUE, sort = FALSE)[
                , row_id := .I]
            # }}}

            # get field checking data
            # {{{
            # gather values from all idf objects
            field_values <- private$m_objects[,
                list(value = `._get_private`(object[[1]])$m_value),
                by = c("object_id", "class_order", "class")][
                , value := flatten_ref(value)][
                , field_order := seq_along(.I), by = "object_id"]

            # gather fields from all idd objects
            def_fields <- data.table::rbindlist(._get_private(idd)$m_objects[, data_field])

            # combine all
            private$m_check_field <- def_fields[field_values,
                on = c("class_order", "field_order", "class")]
            # }}}
            # }}}
        },

        get_order_data = function (idf_value) {
            # {{{
            env <- new.env(parent = emptyenv(), size = 1L)
            env$order <- idf_value[, list(group_order, group, class_order, class, object_id)]
            data.table::setorder(env$order, group_order, class_order, object_id)
            private$m_order <- env
            # }}}
        },

        get_refmap_field = function (idf_value) {
            # set all field values that have reference to environments
            # {{{
            val_field <- idf_value[, list(class_order, object_id, class, value)][
                , lapply(.SD, unlist), by = list(class_order, object_id, class)][
                , field_order := seq_len(.N), by = object_id]

            target <- private$m_reference$reference_field[val_field,
                on = c(src_class_order = "class_order", src_class = "class",
                       src_field_order = "field_order"), nomatch = 0L]

            uni_ref <- unique(
                target[, list(src_class_order, src_field_order, object_id, value)]
                )[, refvalue := list(purrr::map(value, value_to_ref)),
                by = list(object_id, src_class_order, src_field_order)]

            ref_field <- uni_ref[target, on = setdiff(names(uni_ref), "refvalue")]

            private$m_refmap$field <- ref_field
            # }}}
        },

        get_refmap_class = function (idf_value) {
            # collect all classes that have reference
            # {{{
            val_class <- idf_value[, list(class_order, object_id, class)][
                , value := class]
            ref_class <- private$m_reference$reference_class[val_class,
                on = c(src_class_order = "class_order", src_class = "class"), nomatch = 0L]
            private$m_refmap$class <- ref_class
            # }}}
        },

        out_format = function (format = c("asis", "sorted", "ori_bot", "ori_top")) {
            # return save format
            # {{{
            # Default use "SortedOrder"
            format <- match.arg(format)
            format <- switch(format,
                asis = private$m_options$format,
                sorted = "SortedOrder",
                ori_bot = "OriginalOrderBottom",
                ori_top = "OriginalOrderTop")
            if (is.null(format)) format <- "SortedOrder"

            return(format)
            # }}}
        },

        out_header = function (format) {
            # return output header
            # {{{
            if (private$m_options$idfeditor) {
                header_generator <- "!-Generator IDFEditor"
            } else {
                header_generator <- "!-Generator eplusr"
            }

            header_option <- paste0("!-Option ", format)

            if (private$m_options$special_format) {
                warning("Currently option 'UseSpecialFormat' is not supported. Standard foramt will be used.",
                        call. = FALSE)
            }

            if (private$m_options$view_in_ip) {
                warning("Currently option 'ViewInIPunits' is not supported. SI format will be used.",
                        call. = FALSE)
            }
            # currently, "UseSpecialFormat" and "ViewInIPunits" options are not supported.
            special_format <- NULL
            ip_unit <- NULL

            header_option <- paste0(header_option, " ", special_format, " ", ip_unit)
            header_option <- stringr::str_trim(header_option, "right")

            # TODO: Add "UseSpecialFormat" and "ViewInIPunits" support
            header <- c(
                header_generator,
                header_option,
                "",
                "!-NOTE: All comments with '!-' are ignored by the IDFEditor and are generated automatically.",
                "!-      Use '!' comments if they need to be retained when using the IDFEditor."
                )

            return(header)
            # }}}
        },

        out_line = function (comment = TRUE, format) {
            # return output lines according to the format
            # {{{
            data.table::setorder(private$m_objects, class_order, object_id)
            private$m_objects[, class_group := .GRP, by = list(data.table::rleid(class))]
            l_cls <- private$m_objects[, .I[1], by = list(class_group)]$V1
            id_cls <- private$m_objects[l_cls, object_id]

            # collect all out from each object
            private$m_objects[, out_obj := list(list(c("", object[[1]]$out()))), by = object_id]
            # add headings
            private$m_objects[object_id %in% id_cls,
                out_obj := list(list(c(
                    "",
                    "",
                    paste0("!-   ===========  ALL OBJECTS IN CLASS: ",
                            toupper(class), " ==========="),
                    out_obj[[1]]
                ))), by = object_id]
            # combine
            unlist(private$m_objects[["out_obj"]])
            # }}}
        }
        # }}}
    )
)
# }}}

#' @importFrom assertthat "on_failure<-"
# assertion failure message {{{
on_failure(IDF$public_methods$is_valid_class) <- function (call, env) {
    paste0(backtick(eval(call$class, env)), " is not a valid class name in current IDF.")
}

on_failure(IDF$public_methods$is_valid_id) <- function (call, env) {
    paste0(backtick(eval(call$id, env)), " is not a valid object ID in current IDF.")
}
# }}}

#' @importFrom data.table setnames setorder last
# parse_idf {{{
parse_idf <- function (path, idd) {
    assert_that(is_idd(idd))

    idf_str <- read_idd(path)

    idf_version <- get_idf_ver(idf_str)

    idf_dt <- data.table(line = seq_along(idf_str), string = idf_str)

    # idf and idd version mismatch {{{
    idd_version <- idd$version()
    if (not_empty(idf_version)) {
        if (!grepl(idf_version, idd_version, fixed = TRUE)) {
            warning(msg("Version Mismatch. The file parsing is a differnet version
                        '",idf_version,"' than the EnergyPlus program and IDD file
                        you are using '",substr(idd_version, 1L, 3L),"'. Editing and
                        saving the file may make it incompatible with an older
                        version of EnergyPlus."),
                call. = FALSE)
        }
    }
    # }}}

    # mark type {{{
    # -3, unknown
    type_unknown <- -3L
    # -2, speical comment
    type_special <- -2L
    # -1, macro
    type_macro <- -1L
    #  0, block comment
    type_comment <- 0L
    #  1, object
    type_object <- 1L
    #  2, field
    type_field <- 2L
    #  3, last field in an object
    type_field_last <- 3L
    idf_dt[, type := type_unknown]
    data.table::setorder(idf_dt, line, type)

    # delete blank lines
    idf_dt <- idf_dt[!(string %in% "")]
    idf_dt[startsWith(string, "##"), type := type_macro]
    # handle EP-Macro lines {{{
    idf_macro <- idf_dt[type == type_macro]
    idf_macro[, space_loc := regexpr(" ", string, fixed = TRUE)]
    idf_macro[space_loc > 0L,
        `:=`(macro_key = substr(string, 1L, space_loc - 1L),
             macro_value = substr(string, space_loc + 1L, nchar(string)))]
    idf_macro[space_loc < 0L, macro_key := substr(string, 1L, nchar(string))]
    # unknown marco key {{{
    idf_errors_unknown_macro <- idf_macro[!(macro_key %in% macro_dict),
                                          list(line, string)]
    if (not_empty(idf_errors_unknown_macro)) {
        parse_issue(type = "Unknown macro found", src = "IDF",
                    data_errors = idf_errors_unknown_macro)
    }
    # }}}
    # mark macro values as macro {{{
    macro_value <- idf_macro[!is.na(macro_value), unique(macro_value)]
    idf_dt[string %in% macro_value, type := type_macro]
    # }}}
    # }}}
    # treat macro lines as the same as comments
    # idf_dt[type == type_macro, type := type_comment]
    idf_dt[startsWith(string, "!"), type := type_comment]
    idf_dt[startsWith(string, "!-"), type := type_special]
    # mark location of "!" and "!-"
    idf_dt[, explpt_loc := regexpr("!", string, fixed = TRUE)]
    idf_dt[, special_loc := regexpr("!-", string, fixed = TRUE)]
    # lines ending with comma and without explaination symbol must be a object
    idf_dt[explpt_loc < 0L & endsWith(string, ","), type := type_object]
    # extract comments with leading spaces in order to preserve the indentation.
    idf_dt[special_loc > 0L,
           comment := substr(string, explpt_loc + 2L, nchar(string))]
    idf_dt[special_loc < 0L & explpt_loc > 0L,
           comment := substr(string, explpt_loc + 1L, nchar(string))]
    # for commented objects
    idf_dt[special_loc > 0L & explpt_loc > 0L,
           comment := substr(string, explpt_loc + 1L, nchar(string))]
    idf_dt[type == type_macro, comment := string]
    # get the number of leading spaces in comment
    idf_dt[, leading_spaces := regexpr("\\S", comment) - 1L]
    # get the value for lines that have comments
    idf_dt[explpt_loc > 1L, value := trimws(substr(string, 1L, explpt_loc - 1L), "right")]
    # get the value for lines without comments
    idf_dt[explpt_loc < 0L, value := string]
    # mark the last field in an object
    idf_dt[endsWith(value, ";"), type := type_field_last]
    # clean unused columns
    idf_dt[, `:=`(explpt_loc = NULL, special_loc = NULL)]
    # }}}

    # special comment key and value {{{
    option_idfeditor <- FALSE
    option_special_format <- FALSE
    option_view_in_ip_units <- FALSE
    option_save <- NULL

    idf_option <- idf_dt[type == type_special]
    idf_option[, space_loc := regexpr(" ", comment, fixed = TRUE)]
    idf_option[, `:=`(special_key = toupper(substr(comment, 1L, space_loc - 1L)),
                      special_value = toupper(trimws(substr(comment, space_loc + 1L, nchar(comment)))))]
    idf_option <- idf_option[special_key %in% c("GENERATOR", "OPTION")]
    if (not_empty(idf_option)) {
        idf_option <- idf_option[, strsplit(special_value, " ", fixed = TRUE)[[1]], by = list(line, string, special_key)]
        data.table::setnames(idf_option, "V1", "special_value")
        if (idf_option[special_key == "GENERATOR" & substr(special_value, 1L, 9L) == "IDFEDITOR",
            .N] > 1L) {
            option_idfeditor <- TRUE
        }
        if (idf_option[special_key == "OPTION" & special_value == "USESPECIALFORMAT",
                .N] == 1L) {
            option_special_format <- TRUE
        }
        if (idf_option[special_key == "OPTION" & special_value == "VIEWINIPUNITS",
            .N] == 1L) {
            option_view_in_ip_units <- TRUE
        }
        idf_option[special_key == "OPTION" & special_value == "SORTEDORDER",
                   option_save := "SortedOrder"]
        idf_option[special_key == "OPTION" & special_value == "ORIGINALORDERTOP",
                   option_save := "OriginalOrderTop"]
        idf_option[special_key == "OPTION" & special_value == "ORIGINALORDERBOTTOM",
                   option_save := "OriginalOrderBottom"]
        idf_errors_option_save <- idf_option[!is.na(option_save), list(line, string, option_save)]
        option_save <- idf_errors_option_save[, unique(option_save)]
        if (is_empty(option_save)) {
            option_save <- NULL
        }
        if (nrow(idf_errors_option_save) > 1L) {
            parse_issue(type = "More than one save option found", idf_errors_option_save,
                        src = "IDF", stop = FALSE,
                        info = msg("Only the first option '",option_save[1],"'
                                   will be used."))
        }
    }

    heading_options = list(
        save_format = option_save,
        special_format = option_special_format,
        view_in_ip = option_view_in_ip_units,
        idfeditor = option_idfeditor)
    # }}}

    # get rid of special comment lines
    idf_dt <- idf_dt[type != type_special]
    # handle condensed values {{{
    # if the sum of comma and semicolon > 2, then it must be a specially
    # formatted object or field. It may contain a class name, e.g.
    # 'Version,8.8;' and it may not, e.g. '0.0,0.0,0.0,' in
    # 'BuildingSurface:Detailed'.
    # get number of condensed values
    idf_dt[!is.na(value), `:=`(value_count = char_count(value, "[,;]"))]
    idf_dt[is.na(value), `:=`(value_count = 0L)]
    # idf_dt <- idf_dt[!(type %in% c(type_macro, type_comment)),
    idf_dt <- idf_dt[data.table::between(type, type_macro, type_comment),
        strsplit(value, "\\s*[,;]\\s*"), by = list(line)][
        idf_dt, on = "line"][value_count == 1L, V1 := value][, value := NULL]
    data.table::setnames(idf_dt, "V1", "value")
    # get row numeber of last field per condensed field line in each class
    line_value_last <- idf_dt[
        value_count > 1L & type == type_field_last,
        list(line_value_last = data.table::last(.I)),
        by = list(line, type)][, line_value_last]
    # set all type of condensed field lines to "field", including class names.
    idf_dt[value_count > 1L, type := type_field]
    # mark last field per object in condensed lines
    idf_dt[line_value_last, type := type_field_last]
    # make lines that only has one value as "field", excluding recognized class
    # names.
    idf_dt[type != type_object & value_count == 1L, type := type_field]
    # }}}

    # set row id
    idf_dt[, row_id := .I]
    # mark last field and remove trailing comma or semicolon in values {{{
    idf_dt[endsWith(value, ","), value := substr(value, 1L, nchar(value) - 1L)]
    idf_dt[endsWith(value, ";"),
           `:=`(type = type_field_last,
                value = substr(value, 1L, nchar(value) - 1L))]
    # }}}

    # set an id for last field per object {{{
    # if is the last field, then the line after last field line should be a
    # class name except the last field is the last non-blank and non-comment
    # line. Others are just normal fields.
    idf_dt[type == type_field_last, object_id := .GRP, by = list(row_id)]
    idf_dt <- idf_dt[!is.na(object_id), list(row_id, object_id)][
        idf_dt[, object_id := NULL], on = c("row_id"), roll = -Inf]
    # }}}

    # COMMENT (MACRO)
    # {{{
    idf_comment <- idf_dt[type %in% c(type_macro, type_comment), .SD,
        .SDcol = c("type", "object_id", "comment")]
    idf_comment[, field_order := seq_along(.I), by = list(object_id)]
    idf_comment[, .SD, .SDcol = c("type", "object_id", "field_order", "comment")]
    # }}}

    # CLASS & FIELD
    # {{{
    # get idf without comments
    # {{{
    # NOTE: currently, inline comments are not supported.
    idf_dt <- idf_dt[!(type %in% c(type_macro, type_comment)), .SD,
         .SDcol = c("row_id", "object_id", "line", "type", "value", "string")]
    # }}}

    # get class name
    # {{{
    # class name should be the same of 'value' column for first line grouped by
    # object_id
    idf_dt[idf_dt[, .I[1], by = object_id]$V1,
           `:=`(type = type_object, class_upper_case = toupper(value))]

    idf_idd_all <- `._get_private`(idd)$m_objects[,
        list(group_order, group, class_order, class)][
        , class_upper_case := toupper(class)][
        idf_dt, on = "class_upper_case"][
        order(object_id, class_order)]

    # check for un-recognized class names {{{
    unknown_class <- idf_idd_all[type == type_object][
        !is.na(value)][is.na(class), list(line, string)]
    if (not_empty(unknown_class)) {
        parse_issue(type = "Object type not recognized", src = "IDF",
                    data_errors = unknown_class,
                    info = "This error may be caused by a misspelled class name.")
    }
    # }}}

    # fill class downwards
    idf_field <- idf_idd_all[!is.na(class),
        list(row_id, group_order, group, class_order, class)][
        idf_dt, on = "row_id", roll = Inf][
        # get rid of class lines
        type > type_object]
    # }}}
    # }}}

    # get splited idf values
    idf_value <- idf_field[, .SD,
        .SDcol = c("object_id", "group_order", "group", "class_order", "class", "value")][
        , list(value = list(value)), by = list(object_id, group_order, group, class_order, class)]

    # currently, ep-macro lines are not supported
    idf_comment <- idf_comment[, .SD, .SDcol = c("object_id", "comment")][
                               , list(comment = list(comment)), by = list(object_id)]

    idf_value <- idf_comment[idf_value, on = "object_id"]

    data.table::setcolorder(idf_value,
        c("object_id", "group_order", "group", "class_order", "class", "comment", "value"))

    idf <- list(version = idf_version,
                options = heading_options,
                value = idf_value,
                field = idf_field)

    return(idf)
}
# }}}
# get_idf_ver {{{1
get_idf_ver <- function (idf_str) {
    ver_normal <- idf_str[endsWith(idf_str, "Version Identifier")]
    ver_special <- idf_str[startsWith(idf_str, "Version")]

    if (length(ver_normal) == 1L) {
        ver_line <- ver_normal
    } else if (length(ver_special) == 1L){
        ver_line <- ver_special
    } else {
        return(NULL)
    }

    ver_pt <- regexpr("\\d", ver_line)
    ver <- substr(ver_line, ver_pt, ver_pt + 2)

    return(ver)
}
# }}}1

# i_check: check if there are errors in current object
# {{{
i_check = function (private, type = c("idf", "idfobject")) {
    private$m_check <- NULL
    type <- match.arg(type)

    # get returned columns according to `type`
    cols_idfobject <- c("field_order", "value", "_field", "_field_ip")
    cols <- switch(type,
        idf = c("class_order", "class", "object_id", cols_idfobject),
        idfobject = cols_idfobject)
    # clean up after checking
    on.exit({private$m_check_field <- NULL}, add = TRUE)

    if (type == "idf") {
        i_check_missing_object(private)
        i_check_duplicate_object(private, cols)
        # clean up after checking
        on.exit({private$m_check_class <- NULL}, add = TRUE)
    }

    # check scalar
    i_check_scalar(private, cols)
    # check missing required fields
    i_check_missing(private, cols)
    # check invalid auto
    i_check_auto(private, cols)
    # exclude auto fields below during checking
    private$m_check_field <- private$m_check_field[
        !purrr::map_lgl(value, ~tolower(.x) %in% c("autosize", "autocalculate"))]
    # check invalid type
    i_check_type(private, cols)
    # check invalid integer
    i_check_integer(private, cols)
    # check invalid choice
    i_check_choice(private, cols = c(cols, "key"))
    # check range exceeding
    i_check_range(private, cols = c(cols, "range"))
    # check invalid reference
    # do this at last in order to exclude all other errors before checking
    i_check_reference(private, cols = c(cols, "source"))

    data.table::setattr(private$m_check, "class", c("IDFCheckRes", "list"))
    private$m_check
}
# }}}

# i_check_missing_object
# {{{
i_check_missing_object <- function (private) {
    private$m_check$missing_object <- private$m_check_class[
        required_object == TRUE & is.na(object_id)][, field_order := 0L]
}
# }}}

# i_check_duplicate_object
# {{{
i_check_duplicate_object <- function (private, cols) {
    res <- private$m_check_class[unique_object == TRUE & !is.na(object_id),
        list(num = .N, object_id = list(object_id)), by = list(class_order, class)][
        num > 1L, list(class_order, class, object_id)]
    if (is_empty(res)) return(data.table())
    res <- res[, lapply(.SD, unlist), by = list(class_order, class)]
    private$m_check$duplicate_object <- private$m_check_field[res,
        on = c("class_order", "object_id")][, ..cols]
}
# }}}

# value_to_ref
# {{{
value_to_ref <- function (x) {
    env <- new.env(parent = emptyenv(), size = 1L)
    env$value <- x
    env
}
# }}}

# set_ref_value
# {{{
set_ref_value <- function (ref, value) {
    ref[["value"]] <- value
    ref
}
# }}}
