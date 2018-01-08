#' @importFrom data.table "%chin%" data.table rbindlist set setattr setkey
#' @importFrom data.table setorder setorderv copy rleid last setnames
#' @importFrom stringr str_pad
#' @importFrom purrr map_lgl map2_lgl
NULL

#' Parse EnergyPlus models
#'
#' @param idf_str A character vector created using \code{read_idf}.
#' @param idd An IDD object created using \code{parse_idd}.
#'
#' @return A list contains the IDF version, option data, class data, value data,
#' comment data and field reference data.
# parse_idf {{{
parse_idf <- function (idf_str, idd) {
    path <- attr(idf_str, "path")

    idf_version <- get_idf_ver(idf_str)
    # check if input file is an imf file.
    is_imf <- has_macro(idf_str)

    idf_dt <- data.table(line = seq_along(idf_str), string = idf_str)

    # idf and idd version mismatch {{{
    idd_version <- idd$version
    if (!grepl(idf_version, idd_version, fixed = TRUE)) {
        warning(msg("Version Mismatch. The file parsing is a differnet version
                    '",idf_version,"' than the EnergyPlus program and IDD file
                    you are using '",substr(idd_version, 1L, 3L),"'. Editing and
                    saving the file may make it incompatible with an older
                    version of EnergyPlus."),
            call. = FALSE)
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
    setorderv(idf_dt, c("line", "type"))

    # delete blank lines
    idf_dt <- idf_dt[!(string %chin% "")]
    idf_dt[startsWith(string, "##"), type := type_macro]
    # handle EP-Macro lines {{{
    idf_macro <- idf_dt[type == type_macro]
    idf_macro[, space_loc := regexpr(" ", string, fixed = TRUE)]
    idf_macro[space_loc > 0L,
        `:=`(macro_key = substr(string, 1L, space_loc - 1L),
             macro_value = substr(string, space_loc + 1L, nchar(string)))]
    idf_macro[space_loc < 0L, macro_key := substr(string, 1L, nchar(string))]
    # unknown marco key {{{
    idf_errors_unknown_macro <- idf_macro[!(macro_key %chin% macro_dict),
                                          list(line, string)]
    if (not_empty(idf_errors_unknown_macro)) {
        parse_issue(path, type = "Unknown macro found", src = "IDF",
                    data_errors = idf_errors_unknown_macro)
    }
    # }}}
    # mark macro values as macro {{{
    macro_value <- idf_macro[!is.na(macro_value), unique(macro_value)]
    idf_dt[string %chin% macro_value, type := type_macro]
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
    # get rid of leading spaces
    idf_dt[, comment := trimws(comment, "left")]
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
    idf_option <- idf_option[special_key %chin% c("GENERATOR", "OPTION")]
    if (not_empty(idf_option)) {
        idf_option <- idf_option[, strsplit(special_value, " ", fixed = TRUE), by = list(line, string, special_key)]
        setnames(idf_option, "V1", "special_value")
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
            parse_issue(path, type = "More than one save option found", idf_errors_option_save,
                        src = "IDF", stop = FALSE,
                        info = msg("Only the first option '",option_save[1],"'
                                   will be used."))
        }
    }

    # for imf, always use "OriginalOrderBottom"
    if (is_imf) {
        option_save <- "OriginalOrderBottom"
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
    idf_dt <- idf_dt[!(type %in% c(type_macro, type_comment)),
        strsplit(value, "\\s*[,;]\\s*"), by = list(line)][
        idf_dt, on = "line"][value_count == 1L, V1 := value][, value := NULL]
    setnames(idf_dt, "V1", "value")
    # get row numeber of last field per condensed field line in each class
    line_value_last <- idf_dt[
        value_count > 1L & type == type_field_last, list(line_value_last = last(.I)), by = list(line, type)][
        , line_value_last]
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
        .SDcol = c("type", "object_id", "comment", "leading_spaces")]
    idf_comment[is.na(leading_spaces), leading_spaces := 0L]
    idf_comment[leading_spaces < 0L, leading_spaces := 0L]
    idf_comment[, field_order := seq_along(.I), by = list(object_id)]
    idf_comment[, .SD, .SDcol = c("type", "object_id", "field_order", "comment", "leading_spaces")]
    idf_comment[, edited := 0L]
    # }}}
    # get idf without comments
    # NOTE: currently, inline comments are not supported.
    idf_dt <- idf_dt[!(type %in% c(type_macro, type_comment)), .SD,
         .SDcol = c("row_id", "object_id", "line", "type", "value", "string")]
    # class name should be the same of 'value' column for first line grouped by
    # object_id
    idf_dt[idf_dt[, .I[1], by = object_id]$V1,
           `:=`(type = type_object, class_upper_case = toupper(value))]

    # OBJECT
    # {{{
    # merge IDD class data
    idf_class_all <- merge(idf_dt, copy(idd$class)[, class_upper_case := toupper(class)],
        by = "class_upper_case", all = TRUE, sort = FALSE)

    # Error checking {{{
    # duplicated unqiue object {{{
    idf_errors_duplicated_unique <- idf_class_all[!is.na(line)][
        unique_object == TRUE, list(line, class)][
        , lapply(.SD, list), .SDcol = "line", by = class][
        map_lgl(line, ~length(.x) > 1L)][, string := class]
    if (not_empty(idf_errors_duplicated_unique)) {
        parse_issue(path, "Duplicated unique objects found",
            idf_errors_duplicated_unique, src = "IDF")
    }
    # }}}
    # un-recognized class names {{{
    idf_errors_unknown_class <- idf_class_all[type == type_object][
        !is.na(value)][is.na(class), list(line, string)]
    if (not_empty(idf_errors_unknown_class)) {
        parse_issue(path, type = "Object type not recognized", src = "IDF",
                    data_errors = idf_errors_unknown_class,
                    info = "This error may be caused by a misspelled object name.")
    }
    # }}}
    # Do not check missing required objects and fields for imf files.
    if (!is_imf) {
        # missing required object {{{
        idf_errors_missing_required <- idf_class_all[required_object == TRUE][
            is.na(line)]
        if (not_empty(idf_errors_missing_required)) {
            idf_errors_missing_required[, `:=`(line = "NA", string = class)]
            parse_issue(path, "Missing required object", src = "IDF",
                        data_errors = idf_errors_missing_required)
        }
        # }}}
    }
    # }}}
    idf_class <- idf_class_all[type == type_object]
    # add an indicator column to mark the class has been modified or not.
    idf_class[, edited := 0L]
    # select needed columns
    col_class_all <- names(idf_class)
    col_class_full <- c("object_id", "group_order", "group",
        "class_order", "class", "format", "min_fields", "max_fields",
        "required_object", "unique_object", "memo", "edited")
    col_class_avail <- col_class_all[!is.na(match(col_class_all, col_class_full))]
    idf_class <- idf_class[, .SD, .SDcol = col_class_avail]
    # }}}

    # FIELD
    # {{{
    # fill class downwards
    idf_field <- idf_class_all[!is.na(class), list(row_id, class_order, class)][
        idf_dt, on = c("row_id"), roll = Inf][
        # get rid of class lines
        type > type_object][, c("type", "class_upper_case") := NULL]
    # order fields per object
    idf_field[, field_order := seq_along(.I), by = list(object_id)]

    idf_value_all <- merge(idf_field, idd$field,
        by = c("class_order", "class", "field_order"), all = TRUE)
    # Error checking {{{
    # exceed max field {{{
    # get field number per class
    idf_num_fields <- idf_field[, list(num_fields = .N), by = list(class, object_id)]
    idf_errors_max_fields <- idf_class_all[type == type_object][idf_num_fields, on = c("class", "object_id")][
        num_fields > max_fields, list(line, class, max_fields, num_fields)][
        , string := sprintf("*%s* fields found for class %s with only *%s* allowed", num_fields, sQuote(class), max_fields)]
    if (not_empty(idf_errors_max_fields)) {
        parse_issue(path, "Too many fields in objects", idf_errors_max_fields, src = "IDF", quote = FALSE)
    }
    # }}}
    # Do not check missing required objects and fields for imf files.
    if (!is_imf) {
        # missing required field {{{
        idf_errors_missing_required_field <- idf_value_all[
            !is.na(object_id)][required_field == TRUE & is.na(value)][
            , string := paste0("Missing field ", field, "{", units, "}", "in class ", sQuote(class))]
        if (not_empty(idf_errors_missing_required_field)) {
            parse_issue(path, "Missing reqiured fields in objects",
                idf_errors_missing_required_field, src = "IDF", quote = FALSE)
        }
        # }}}
    }
    # }}}
    # clean up after error checking
    idf_value <- idf_value_all[!is.na(line)][, c("reference") := NULL]
    # add an indicator column to mark the fields has been modified or not.
    idf_value[, edited := 0L]
    # }}}

    # REF
    # {{{
    # wrong class and field references {{{
    idf_errors_wrong_references <- check_obj_ref(idf_value, idd)
    if (not_empty(idf_errors_wrong_references)) {
        parse_issue(path, type = "Wrong value references", src = "IDF",
                    idf_errors_wrong_references)
    }
    # }}}
    # select needed columns after ref checking
    col_value_all <- names(idf_value)
    col_value_full <- c("object_id", "class_order", "class", "field_order",
        "field", "field_anid", "field_an", "field_id", "value", "units",
        "ip_units", "type", "default", "key", "maximum", "maximum<", "minimum",
        "minimum>", "autosizable", "autocalculatable", "required_field",
        "object_list", "edited")
    col_value_avail <- col_value_all[!is.na(match(col_value_all, col_value_full))]
    idf_value <- idf_value[, .SD, .SDcol = col_value_avail]
    setkey(idf_value, object_id, class_order, field_order)
    idf_ref <- get_obj_ref(idf_value, idd)
    # }}}

    # set class
    setattr(idf_class, "class", c("IDF_Class", class(idf_class)))
    setattr(idf_value, "class", c("IDF_Value", class(idf_value)))
    setattr(idf_comment, "class", c("IDF_Comment", class(idf_comment)))
    setattr(idf_ref, "class", c("IDF_Ref", class(idf_ref)))

    idf <- list(version = idf_version,
                options = heading_options,
                class = idf_class,
                value = idf_value,
                comment = idf_comment,
                ref = idf_ref)

    if (is_imf) {
        setattr(idf, "class", c("IMF", class(idf)))
    } else {
        setattr(idf, "class", c("IDF", class(idf)))
    }

    ids <- sort(idf$class[, object_id])
    setattr(idf, "id", ids)

    return(idf)
}
# }}}
# read_idf {{{
read_idf <- function (filepath) {
    assert_that(is_readable(filepath))

    con = file(filepath)
    idf_str <- readLines(con, encoding = "UTF-8")
    # Get rid of unparsable characters
    idf_str <- iconv(idf_str, to = "UTF-8")
    close(con)

    # Get rid of preceeding and trailing spaces
    idf_str <- trimws(idf_str, "both")

    assert_that(not_empty(idf_str))

    setattr(idf_str, "path", normalizePath(filepath, winslash = "/"))

    return(idf_str)
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
# get_obj_ref {{{
get_obj_ref <- function (idf_value, idd) {
    # get field values that are referred
    idf_ref_value_field <- NULL
    if (!is.null(idd$ref_object$field)) {
        idf_ref_value_field <- idd$ref_object$field[idf_value[edited >= 0L],
            on = c("class_order", "field_order"), nomatch = 0L][
            , list(ref_key, value)][
            , list(ref_value = c(.SD)), by = list(ref_key)]
    }

    # get class values that are referred
    idf_ref_value_class <- NULL
    if (!is.null(idd$ref_object$class)) {
        idf_ref_value_class <- idd$ref_object$class[idf_value[edited >= 0L],
            on = c("class_order"), nomatch = 0L][
            field_order == 1L, list(ref_key, class)][
            , list(ref_value = c(.SD)), by = list(ref_key)]
    }

    idf_ref_value <- rbindlist(list(idf_ref_value_class, idf_ref_value_field))

    return(idf_ref_value)
}
# }}}
# check_obj_ref {{{
check_obj_ref <- function (idf, idd) {
    # get fields that have \object-list
    idf_ref_field <- data.table()
    if (has_name(idf$value, "object_list")) {
        idf_ref_field <- idf$value[!is.na(object_list),
            list(row_id, line, string,
              object_id, class_order, field_order,
              object_list, value)][
            # remove empty lines
            nchar(value) > 1L]
    }

    # if no '\object-list' exists
    if (is_empty(idf_ref_field)) {
        return(idf_ref_field)
    }

    # get referred value
    idf_ref_value <- get_obj_ref(idf$value, idd)

    idf_ref <- merge(idf_ref_field, idf_ref_value,
                     by.x = "object_list" , by.y = "ref_key")

    error_ref <- idf_ref[!(toupper(value) %chin% toupper(unlist(ref_value))),
        list(line, string)]

    return(error_ref)
}
# }}}
# print.IDF {{{
print.IDF <- function (idf, ...) {
    .print(idf)
}
# }}}
# print.IMF {{{
print.IMF <- function (imf, ...) {
    .print(imf)
}
# }}}
# link_idd {{{
link_idd <- function (ver) {
    if (is_pre_parsed(ver)) {
        switch(ver,
            "8.5" = idd_8.5,
            "8.6" = idd_8.6,
            "8.7" = idd_8.7,
            "8.8" = idd_8.8)
    } else {
        NULL
    }
}
# }}}
# get_idd {{{
get_idd <- function (ver = NULL, path = NULL) {
    if (is.null(path)) {
        assert_that(not_empty(ver), msg = "Both 'ver' and 'path' are NULL.")
        idd <- link_idd(ver)
        if (is.null(idd)) {
            stop(msg(
                sprintf("Input file has a version %s whose IDD file has not been
                        pre-parsed. 'idd' should be specified.", sQuote(ver))),
                call. = FALSE
            )
        }
    } else {
        idd <- parse_idd(idd)
    }

    return(idd)
}
# }}}

# save_idf {{{
save_idf <- function (idf, path, format = c("asis", "sorted", "ori_bot", "ori_top")) {
    format <- match.arg(format)
    save_format <- switch(format,
           asis = idf$options$save_format,
           sorted = "SortedOrder",
           ori_bot = "OriginalOrderBottom",
           ori_top = "OriginalOrderTop")
    if (is.null(save_format)) save_format <- "SortedOrder"

    header <- get_output_header(idf$options, format = save_format)
    comment <- get_output_comment(idf$comment[edited >= -1L])
    value <- get_output_line(idf$value[edited >= -1L], show_hide = TRUE)

    # combine comment and value {{{
    output_dt <- rbindlist(list(comment, value), fill = TRUE)[
        , list(edited, object_id, class_order, field_order, class, output)]
    output_dt <- output_dt[!is.na(class_order),
        list(edited, object_id, field_order, class_order, class)][
        output_dt[, `:=`(class_order = NULL, class = NULL)],
        on = c("edited", "object_id", "field_order"), roll = -Inf]
    # }}}

    # handle different save options {{{
    # "SortedOrder"
    if (save_format == "SortedOrder") {
        setorder(output_dt, class_order, object_id, field_order)
        # add class heading
        output_dt[, class_group := .GRP, by = list(rleid(class))]
        output_dt[output_dt[, .I[1], by = list(class_group)]$V1,
            output := paste0("\n!-   ===========  ALL OBJECTS IN CLASS: ",
                             toupper(class), " ===========\n\n", output)]
        setorder(output_dt, class_order, object_id, field_order)
    }
    # "OriginalOrderTop"
    if (save_format == "OriginalOrderTop") {
        header <- c(header, "")
        setorder(output_dt, edited, object_id, field_order)
    }
    # "OriginalOrderBottom"
    if (save_format == "OriginalOrderBottom") {
        header <- c(header, "")
        setorder(output_dt, -edited, object_id, field_order)
    }
    # }}}

    # # handle special format object {{{
    # if (idf$options$special_format) {

    # }
    # # }}}

    value_output <- output_dt[, output]

    str_output <- c(header, value_output)

    if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
    if (!file.exists(path)) file.create(path)
    assert_that(is_writeable(path))

    con <- file(path)
    writeLines(str_output, path)
    close(con)

    return(invisible())
}
# }}}
# get_output_header {{{
get_output_header <- function (idf_options, format = c("SortedOrder", "OriginalOrderBottom", "OriginalOrderTop")) {
    format <- match.arg(format)

    if (idf_options$idfeditor) {
        header_generator <- "!-Generator IDFEditor"
    } else {
        header_generator <- "!-Generator eplusr"
    }

    # Default use "SortedOrder"
    if (is.null(format)) {
        format <- "SortedOrder"
    }
    header_option <- paste0("!-Option ", format)

    special_format <- if (idf_options$special_format) "UseSpecialFormat" else NULL
    ip_unit <- if (idf_options$view_in_ip) "ViewInIPunits" else NULL
    # currently, "UseSpecialFormat" and "ViewInIPunits" options are not supported.
    special_format <- NULL
    ip_unit <- NULL

    header_option <- paste0(header_option, " ", special_format, " ", ip_unit)
    header_option <- trimws(header_option, "right")

    # TODO: Add "UseSpecialFormat" and "ViewInIPunits" support
    header <- c(
        header_generator,
        header_option,
        "",
        "!-NOTE: All comments with '!-' are ignored by the IDFEditor and are generated automatically.",
        "!-      Use '!' comments if they need to be retained when using the IDFEditor.",
        "")

    return(header)
}
# }}}
# get_output_comment {{{
get_output_comment <- function (idf_comment) {
    idf_comment <- copy(idf_comment)
    # init
    idf_comment[, output := ""]
    idf_comment[, output_space := strrep(" ", leading_spaces)]
    # for macro lines
    idf_comment[type ==-1L, output := paste0(output_space, comment)]
    # for normal comment lines
    idf_comment[type == 0L, output := paste0("!", output_space, comment)]
    idf_comment[type == 1L, output := paste0("!", output_space, comment)]
    idf_comment[type == 2L, output := paste0("!#", output_space, comment)]
    setorder(idf_comment, object_id, -field_order)
    idf_comment[, field_order := -seq_along(.I), by = list(object_id)]
    setorder(idf_comment, object_id, field_order)

    return(idf_comment)
}
# }}}
# get_output_line {{{
get_output_line <- function (idf_value, keep_all = FALSE,
                             show_id = FALSE, show_class = TRUE,
                             show_diff = FALSE, show_order = FALSE,
                             mark_required = FALSE, show_ref = FALSE,
                             show_hide = FALSE, fill_na = FALSE, indent = TRUE,
                             standard = TRUE, new_line = TRUE) {
    idf_value <- copy(idf_value)

    comb_list <- c("output_value", "output_field")
    # add ref
    if (show_ref) {
        idf_value <- add_output_ref(idf_value)
        comb_list <- c("output_ref", comb_list)
    }

    # add diff
    if (show_diff) {
        idf_value <- add_output_diff(idf_value)
        comb_list <- c("output_diff", comb_list)
    }

    # add field order
    if (show_order) {
        idf_value <- add_output_order(idf_value, mark_required = mark_required)
        comb_list <- c("output_order", comb_list)
    }

    # add class name
    if (show_class) {
        idf_value <- add_output_class(idf_value, show_hide = show_hide)
        comb_list <- c("output_class", comb_list)
    }

    # add object id
    if (show_id) {
        idf_value <- add_output_id(idf_value)
        comb_list <- c("output_id", comb_list)
    }

    # add field value
    idf_value <- add_output_value(idf_value, fill_na = fill_na, indent = indent,
                                  show_hide = show_hide)

    # add field name
    idf_value <- add_output_field(idf_value, keep_all = keep_all, standard = standard, new_line = new_line)

    idf_value[, output := do.call(paste0, .SD), .SDcol = comb_list]

    setorder(idf_value, class_order, object_id, field_order)

    if (!keep_all) {
        idf_value[, c(comb_list) := NULL]
    }

    return(idf_value)
}
# }}}
# add_output_order {{{
add_output_order <- function (idf_value, mark_required = FALSE) {
    max_id <- idf_value[, max(field_order)]
    output_dt <- copy(idf_value)[
        , output_order := paste0(stringr::str_pad(field_order, nchar(max_id), "left"), ":")]
    if (mark_required) {
        output_dt[
            required_field == TRUE, output_order := paste0("*", output_order)][
            required_field == FALSE, output_order := paste0(" ", output_order)]
    }

    return(output_dt)
}
# }}}
# add_output_ref {{{
add_output_ref <- function (idf_value_ref) {
    idf_value_ref[ref_value != value, output_ref := "   "]
    idf_value_ref[ref_value == value, output_ref := "($)"]
}
# }}}
# add_output_id {{{
add_output_id <- function (idf_value) {
    max_id <- idf_value[, max(object_id)]
    setorder(idf_value, class_order, object_id, field_order)
    idf_value[, output_id := ""]
    idf_value[idf_value[, .I[1], by = list(object_id)]$V1,
        output_id := paste0(
            "[ID:", stringr::str_pad(object_id, nchar(max_id), "left", " "), "] ")
    ]

    return(idf_value)
}
# }}}
# add_output_diff {{{
add_output_diff <- function (idf_object) {
    assert_that(has_name(idf_object, "edited"))
    idf_object[, output_diff := "   "]
    # for deleted fields
    idf_object[edited ==-2L, output_diff := "(-)"]
    # for hidden fields
    idf_object[edited ==-1L, output_diff := "(!)"]
    # for changed fields
    idf_object[edited == 1L, output_diff := "(~)"]
    # for added fields
    idf_object[edited == 2L, output_diff := "(+)"]

    return(idf_object)
}
# }}}
# add_output_class {{{
add_output_class <- function (idf_value, show_hide = FALSE) {
    idf_value[, output_class := ""]
    l_class <- idf_value[, .I[1], by = list(object_id)]$V1
    idf_value[, row_id_class := .I]
    if (show_hide) {
        # Support for hidden objects
        idf_value[row_id_class %in% l_class & edited == -1L,
                  output_class := paste0("! ", class, ",\n")][
                  row_id_class %in% l_class & edited != -1L,
                  output_class := paste0(class, ",\n")][
                  , row_id_class := NULL]
    } else {
        idf_value[row_id_class %in% l_class,
                  output_class := paste0(class, ",\n")][
                  , row_id_class := NULL]
    }

    return(idf_value)
}
# }}}
# add_output_value {{{
add_output_value <- function (idf_value, fill_na = FALSE, indent = TRUE, show_hide = FALSE) {
    idf_value[, value_fill := value]
    if (fill_na) {
        idf_value[is.na(value_fill), value_fill := ""]
    }
    idf_value[, output_value := paste0("    ", value_fill, ",")][
        , `:=`(value_fill = NULL)]
    idf_value[idf_value[, .I[.N], by = list(object_id)]$V1,
              output_value := sub(",$", ";", output_value)]

    if (indent) {
        idf_value[nchar(output_value) <= 29L,
                  output_value := stringr::str_pad(output_value, 29L, side = "right")]
        idf_value[nchar(output_value) > 29L,
                  output_value := paste0(output_value, "  ")]
    }

    if (show_hide) {
        # Support for hidden objects
        idf_value[edited == -1L, output_value := paste0("! ", output_value)]
    }

    return(idf_value)
}
# }}}
# add_output_field {{{
add_output_field <- function (idf_value, standard = TRUE, new_line = TRUE, keep_all = FALSE) {
    idf_value <- add_output_field_name(idf_value)
    idf_value <- add_output_field_unit(idf_value)
    comb_list <- c("output_name", "output_unit")

    if (standard) {
        idf_value[, output_name := paste0("!- ", output_name)]
    }

    if (new_line) {
        # add a new line after the last field per class
        idf_value[idf_value[, .I[.N], by = list(object_id)]$V1,
                  output_unit := paste0(output_unit, "\n")]
    }

    idf_value[, output_field := do.call(paste0, .SD), .SDcol = comb_list]

    if (!keep_all) {
        idf_value[, c(comb_list) := NULL]
    }

    return(idf_value)
}
# }}}
# add_output_field_name {{{
add_output_field_name <- function (idf_value) {
    idf_value[is.na(field), output_name := field_anid]
    idf_value[!is.na(field), output_name := field]

    return(idf_value)
}
# }}}
# add_output_field_unit {{{
add_output_field_unit <- function (idf_value) {
    idf_value[is.na(units), output_unit := ""]
    idf_value[!is.na(units), output_unit := paste0(" {", units, "}")]

    return(idf_value)
}
# }}}
# get_output_summary {{{
get_output_summary <- function (idf, diff = FALSE) {
    assert_that(is_model(idf))
    output_dt <- add_output_count(idf$class)
    output_dt <- add_output_group(output_dt)

    output_dt[, output := paste0(output_group, output_count, " ", class)]

    return(output_dt)
}
# }}}
# add_output_group {{{
add_output_group <- function (idf_class) {
    key_cols <- c("group_order", "class_order", "object_id")
    key_cols <- avail_cols(idf_class, key_cols)
    setorderv(idf_class, key_cols)

    output_dt <- idf_class[, output_group := ""][
        idf_class[, .I[1L], by = list(group_order)]$V1,
        output_group := paste0("\n", group, "\n", sep_line(), "\n")]

    return(output_dt)
}
# }}}
# add_output_count {{{
add_output_count <- function (idf_class) {
    assert_that(has_name(idf_class, "group_order"), has_name(idf_class, "class_order"))
    output_dt <- get_obj_count(idf_class)
    max_count <- output_dt[, max(num)]

    output_dt <- output_dt[, output_count := paste0(
        "[", stringr::str_pad(num, width = nchar(max_count), "left", "0"), "]")
        ][, num := NULL]

    setorder(output_dt, group_order, class_order)

    return(output_dt)
}
# }}}
# print_output {{{
print_output <- function (x, col = "output") {
    cat(x[[col]], sep = "\n")
}
# }}}
# .print {{{
.print <- function (idf, info = NULL) {
    target_id <- idf$log[.N, unlist(id)]
    new_id <- idf$log[.N, new_id]
    if (new_id > 0L) target_id <- unique(c(target_id, new_id)[c(target_id, new_id) > 0L])
    action <- idf$log[.N, action]
    active <- idf$log[.N, active]

    if (action == "save") return(invisible())
    # not active action
    if (!active) {
        # print the whole idf
        output_str <- get_output_summary(idf)[, output]
        output_str <- c(info, output_str)
    # init or reset
    } else if (all(target_id == 0L)) {
        # print the whole idf
        output_str <- get_output_summary(idf)[, output]
        output_str <- c(info, output_str)
    } else if (action == "diff") {
        if (all(target_id == 0L)) {
            idf$log[.N, active := FALSE]
            return(cat("No modification has been made yet\n"))
        } else {
            output_str <- get_output_str(idf, target_id, action)
        }
    } else {
        output_str <- get_output_str(idf, target_id, action)
    }

    cat(output_str, sep = "\n")

    idf$log[.N, active := FALSE]

    return(invisible())
}
# }}}

# valid_field {{{
valid_field <- function (class, idf, idd) {
    assert_that(is_valid_class(class, idd))
    class_name <- class

    all_fields <- add_output_order(idd$field[class == class_name], mark_required = TRUE)
    all_fields <- add_output_field(all_fields, standard = FALSE, new_line = FALSE, keep_all = TRUE)
    all_fields[, output := paste0(output_order, " ", output_field)]

    print_output(all_fields)

    return(invisible(all_fields[, output_name]))
}
# }}}
# valid_class {{{
valid_class <- function (idf) {
    key_avail <- key_col(idf$class, "class")
    id_del <- get_deleted_id(idf)
    idf_class <- copy(idf$class)
    if (not_empty(id_del)) idf_class <- idf_class[!object_id %in% id_del]
    return(setorderv(idf_class, key_avail)[, unique(class)])
}
# }}}
# valid_id {{{
valid_id <- function (idf) {
    idf_value <- copy(idf$value)
    id_del <- get_deleted_id(idf)
    if (not_empty(id_del)) {
        idf_value <- idf_value[!object_id %in% id_del]
    }
    idf_value <- idf_value[idf_value[, .I[1:3], by = list(object_id)]$V1][
        !is.na(class_order)]
    idf_value <- get_output_line(idf_value, show_id = TRUE)
    idf_value[field_order == 3L, output := "    ........\n"]

    setorder(idf_value, object_id, field_order)

    print_output(idf_value)

    return(invisible(idf_value[, unique(object_id)]))
}
# }}}

# pull_data {{{
pull_data <- function (idf, class, type = c("class", "value")) {
    assert_that(is_string(class))
    assert_that(is_valid_class(class, idf))
    class_name <- class

    type <- match.arg(type)

    switch(type,
           class = idf$class[class == class_name],
           value = idf$value[class == class_name]
    )
}
# }}}
# get_class {{{
get_class <- function (idf, id) {
    assert_that(is_model(idf), is_valid_id(id, idf))

    single_class <- idf$class[object_id %in% id]

    return(single_class)
}
# }}}
# get_value {{{
get_value <- function (idf, id, field = NULL) {
    assert_that(is_model(idf), is_valid_id(id, idf))

    object <- idf$value[object_id == id]
    if (!is.null(field)) {
        sapply(field, function (x) assert_that(is_integerish(x)))
        sapply(field, function (x) assert_that(x <= object[, max(field_order)]))
        object <- object[field]
    }

    return(object)
}
# }}}
# get_id {{{
get_id <- function (idf, class) {
    assert_that(is_valid_class(class, idf))

    class_name <- class
    ids <- idf$class[class == class_name, object_id]

    return(ids)
}
# }}}

# find_object {{{
find_object <- function (idf, pattern, ...) {

    idf_value <- idf$value[grepl(pattern, class, ...)]

    if (is_empty(idf_value)) {
        stop("No matched object found.", call. = FALSE)
    }

    object_count <- idf_value[, .N, by = list(class, object_id)][, .N, by = list(class)]
    idf_value <- get_output_line(idf_value, show_id = TRUE)
    idf_value <- object_count[idf_value, on = "class", roll = Inf]

    # add class heading
    setorder(idf_value, class_order, object_id, field_order)
    idf_value[, class_group := .GRP, by = list(rleid(class))]
    idf_value[idf_value[, .I[1], by = list(class_group)]$V1,
        output := paste0("\n",
            stringr::str_pad(paste0("== * ", N, " Objects Found in Class: ", class, " * "),
                             console_width(), side = "right", pad = "=" ),
            "\n\n", output)]

    setorder(idf_value, class_order, object_id, field_order)

    print_output(idf_value)

    return(invisible(idf))
}
# }}}
# find_field {{{
find_field <- function (idf, pattern, ..., all = FALSE) {

    if (!all) {
        idx <- idf$value[field_an == "A"][
            grepl(pattern, value, ...), list(object_id, field_order)]
    } else {
        idx <- idf$value[grepl(pattern, value, ...), list(object_id, field_order)]
    }

    total <- idx[, .N]
    if (is_empty(idx)) {
        stop("No matched field found.", call. = FALSE)
    }

    idf_value <- idf$value[object_id %in% idx$object_id]
    idf_value <- idx[, char := "(*)"][
        idf_value, on = c("object_id", "field_order")][
        !is.na(char), value := paste0(char, value)][
        is.na(char), value := paste0("   ", value)]

    idf_value <- get_output_line(idf_value, show_id = TRUE)

    setorder(idf_value, class_order, object_id, field_order)

    cat(stringr::str_pad(
            sprintf("== * %s Matched Fields Found * ", total),
            console_width(), "right", "="), "\n\n")
    print_output(idf_value)

    return(invisible(idf))
}
# }}}
# get_object {{{
get_object <- function (idf, ..., log = TRUE) {
    id <- list(...)
    assert_that(not_empty(id), msg = "'id' or 'class' is empty")
    check_char <- all(map_lgl(id, is.character))
    check_num <- all(map_lgl(id, is_integerish))
    assert_that(check_char || check_num,
        msg = "Input should be either all character types or integer types.")
    id <- unlist(id)
    if (check_char) {
        map_lgl(id, ~assert_that(is_valid_class(.x, idf)))
        class_names <- id
        ids <- idf$class[class %chin% class_names, object_id]
        ids <- ids[map_lgl(ids, ~not_deleted(.x, idf))]
    } else {
        map_lgl(id, ~assert_that(not_deleted(.x, idf), is_valid_id(.x, idf)))
        ids <- id
    }

    if (log) idf <- add_log("get", ids, 0L, idf)

    return(idf)
}
# }}}
# dup_object {{{
dup_object <- function (idf, id, new_name = NULL, idd, log = TRUE) {
    target_class <- get_class(idf, id = id)
    class_name <- get_class_name(target_class)
    # check if the object can be duplicated
    assert_that(can_be_duplicated(class_name, idf))
    assert_that(not_deleted(id, idf))

    target_object <- get_value(idf, id)

    # Give new name if applicable {{{
    if (target_object[field_order == 1L & grepl("Name", field, fixed = TRUE), .N]) {
        # get all names of objects in the same class
        all_names <- idf$value[class == class_name & field_order == 1L, value]
        # get all names of new or midified objects in the same class
        all_new_names <- idf$value[edited == 1L & class == class_name & field_order == 1L, value]
        # auto-create a new name if new name is not given and make sure that an
        # unique object name is created.
        if (is.null(new_name)) {
            n_new <- length(all_new_names)
            if (n_new > 0L) {
                target_object[field_order == 1L, value := paste0(value, "_", n_new)]
            } else {
                target_object[field_order == 1L, value := paste0(value, "_1")]
            }
        } else {
            if (new_name %chin% all_names) {
                used_id <- idf$value[class == class_name & field_order == 1L & field == new_name, object_id]
                stop("Given new name has been used in object [ID:", used_id, "]",
                     call. = FALSE)
            } else {
                used_id <- target_object[class == class_name & field_order == 1L, value := new_name]
            }
        }
    } else {
        if (!is.null(new_name)) {
            warning("'new_name' is ignored for class ", sQuote(class_name), call. = FALSE)
        }
    }
    # }}}

    # append all data
    new_idf <- append_data(id, target_class, target_object, action = "dup", idf, idd, log)

    return(new_idf)
}
# }}}
# add_object {{{
add_object <- function (idf, class, ..., min = TRUE, idd, log = TRUE) {
    class_name <- class
    assert_that(is_valid_class(class_name, idd))
    assert_that(can_be_duplicated(class_name, idf))

    new_class <- extract_class(class_name, idf, idd)
    new_object <- extract_object(class_name, min, idf, idd)

    # set default values first
    new_object <- set_default(new_object)

    fields <- list(...)
    assert_that(not_empty(fields), msg = "Field values are empty")

    # add suggestion of 'min' option
    num_max <- new_class[, max_fields]
    num_cur <- new_object[, .N]

    target_order <- get_target_order(new_object, index = class_name, fields, idf, idd)

    if (min && max(target_order) > num_cur) {
        stop(msg(sprintf("No.*%s* field found in input, which exceeded current
                         field number (*%s* in total) in object template for
                         class %s. Try to set %s to FALSE.", max(target_order),
                         num_cur, sQuote(class_name), sQuote("min")
                )
            ),
            call. = FALSE
        )
    }

    new_object <- add_extra_required(new_object, target_order, fields, idf, idd)
    new_object <- set_fields(new_object, target_order, fields, "add", idf, idd)

    # append all data
    new_idf <- append_data(id = NULL, new_class, new_object, action = "add", idf, idd, log)

    return(new_idf)
}
# }}}
# set_object {{{
set_object <- function (idf, id, ..., idd, log = TRUE) {
    assert_that(is_valid_id(id, idf))
    fields <- list(...)
    assert_that(not_empty(fields), msg = "Field values are empty")

    target_class <- get_class(idf, id = id)
    target_object <- get_value(idf, id)
    class_name <- get_class_name(target_class)
    assert_that(not_deleted(id, idf))
    assert_that(can_be_modified(class_name, idf))

    target_order <- get_target_order(target_object, index = id, fields, idf, idd)
    new_object <- set_fields(target_object, target_order, fields, "set", idf, idd)

    idf <- update_field_ref(target_object, new_object, idf, idd)

    new_idf <- append_data(id, target_class, new_object, action = "set", idf, idd, log)

    return(new_idf)
}
# }}}
# del_object {{{
del_object <- function (idf, id, idd, force = FALSE, hide = FALSE, log = TRUE) {

    target_class <- get_class(idf, id)
    class_name <- get_class_name(target_class)
    assert_that(not_deleted(id, idf))
    assert_that(can_be_deleted(class_name, idf))

    target_object <- get_value(idf, id = id)

    # check if the fields in the target object have been referred by other
    # objects
    field_referred <- get_referred(target_object, idf, idd)
    if (not_empty(field_referred)) {
        field_output <- get_output_line(field_referred, show_id = TRUE,
            show_class = TRUE, show_ref = TRUE)
        ref_ids <- field_referred[, unique(object_id)]
        if (force) {
            if (hide) act <- "hide" else act <- "delete"
            warning(msg(
                sprintf("Force to %s object (ID:%s) that has been referred
                        by other objects (ID:%s). Errors may occur during
                        simulations.", act, id,
                        paste0(ref_ids, collapse = ", "))),
                call. = FALSE)
        } else {
            stop(msg(
                sprintf("Some field(s) in current object (ID:%s) has been
                        referred by other object(s) (ID:%s) below. Comfirm by
                        setting 'force' to TRUE.",
                        id, paste0(ref_ids, collapse = ', '))), "\n",
                paste0(field_output[, output], collapse = '\n'),
                call. = FALSE)
        }
    }

    if (hide) action <- "hide" else action <- "del"
    new_idf <- append_data(id, target_class, target_object, action = action, idf, idd, log)

    return(new_idf)
}
# }}}
# get_comment {{{
get_comment <- function (idf, id) {
    assert_that(is_valid_id(id, idf))

    idf <- add_log("notes", id, 0L, idf)

    return(idf)
}
# }}}
# add_comment {{{
add_comment <- function (idf, id, append = TRUE, type = 1L, ..., wrap = 0L, log = TRUE) {
    assert_that(is_valid_id(id, idf), not_deleted(id, idf))
    assert_that(is_integerish(wrap))
    content <- unlist(c(...))
    if (wrap > 0L) content <- strwrap(content, width = wrap)
    assert_that(not_empty(content), msg = "Comment to be inserted is empty.")

    # make sure that 'edited' value is consistent between value and comment
    edited <- get_class(idf, id)[, edited]
    new_comment <- data.table(type = type, object_id = id, comment = content,
                              leading_spaces = 1L, edited = edited)

    # check if there are already comments in this object
    ori_comment <- setorder(idf$comment[object_id == id], field_order)
    if (not_empty(ori_comment)) {
        idf$comment <- idf$comment[object_id != id]
        if (append) {
            new_comment <- rbindlist(list(ori_comment, new_comment), fill = TRUE)
        } else {
            new_comment <- rbindlist(list(new_comment, ori_comment), fill = TRUE)
        }
    }

    new_comment[, field_order := .I]
    idf$comment <- rbindlist(list(idf$comment, new_comment), fill = TRUE)
    setorder(idf$comment, object_id, field_order)

    if (log) idf <- add_log(action = "notes", id = id, new_id = 0L, idf)

    return(idf)
}
# }}}
# del_comment {{{
del_comment <- function (idf, id) {
    assert_that(is_valid_id(id, idf))

    idf$comment <- idf$comment[object_id != id]

    idf <- add_log("notes", id, 0L, idf)

    return(idf)
}
# }}}
# diff_idf {{{
diff_idf <- function (idf, type = c("all", "add", "set", "del", "hide")) {
    idx <- switch(type,
        all = idf$class[edited != 0L, object_id],
        del = idf$class[edited ==-2L, object_id],
        hide = idf$class[edited ==-1L, object_id],
        set = idf$class[edited == 1L, object_id],
        add = idf$class[edited == 2L, object_id]
    )

    if (is_empty(idx)) idx <- 0L
    idf <- add_log("diff", idx, 0L, idf)

    return(idf)
}
# }}}

# extract_class {{{
extract_class <- function (class, idf, idd) {
    class_name <- class
    new_class <- idd$class[class == class_name]

    return(new_class)
}
# }}}
# extract_object {{{
extract_object <- function (class, min = TRUE, idf, idd) {
    class_name <- class
    if (min) {
        num_min_field <- idd$class[class == class_name, min_fields]
        if (num_min_field > 0L) {
            new_object <- idd$field[class == class_name & field_order <= num_min_field]
        # if no min field requirement, return all fields
        } else {
            new_object <- idd$field[class == class_name]
        }
    } else {
        new_object <- idd$field[class == class_name]
    }

    return(new_object)
}
# }}}
# append_data {{{
append_data <- function (id = NULL, class_data = NULL, object_data = NULL, action, idf, idd, log = TRUE) {

    action <- match.arg(action, c("add", "dup", "set", "del", "hide"))
    new_id <- 0L

    if (action %in% c("add", "dup")) {
        class_data <- append_id(class_data, base = "value", idf)
        object_data <- append_id(object_data, base = "value", idf)
        new_id <- max_id(idf) + 1L
        setattr(idf, "id", c(attr(idf, "id"), new_id))
        if (action == "add") {
            assert_that(is.null(id))
            id <- 0L
        }
    } else if (action == "set") {
        class_data[, edited := 1L]
        # only mark fields that have been changed.
        id <- class_data[, unique(object_id)]
        ori_object <- idf$value[object_id == id]
        field_order_changed <- get_field_changes(ori_object, object_data)[
            , field_order]
        object_data[field_order %in% field_order_changed, edited := 1L]

        idf$class <- idf$class[object_id != id]
        idf$value <- idf$value[object_id != id]
        idf$comment <- idf$comment[object_id == id, edited := 1L]
    # del and hide
    } else {
        if (action == "del") idx <- -2L else idx <- -1L
        idf$class[object_id == id, edited := idx]
        idf$value[object_id == id, edited := idx]
        idf$comment[object_id == id, edited := idx]
        # just mark hidden objects as deleted
        id_del <- sort(unique(c(attr(idf, "id_del"), id)))
        setattr(idf, "id_del", id_del)
    }

    if (!action %in% c("del", "hide")) {
        needed_cols_class <- names(idf$class)
        idf$class <- rbindlist(list(idf$class, class_data[, .SD, .SDcol = needed_cols_class]))
        setorder(idf$class, group_order, class_order, object_id)

        needed_cols_value <- names(idf$value)
        idf$value <- rbindlist(list(idf$value, object_data[, .SD, .SDcol = needed_cols_value]))
        setorder(idf$value, class_order, object_id, field_order)
    }

    # update ref
    idf$ref <- get_obj_ref(idf$value, idd)

    # add log
    if (log) idf <- add_log(action, id, new_id, idf)

    return(idf)
}
# }}}
# append_id {{{
append_id <- function (data, base = c("class", "value"), idf) {
    base <- match.arg(base)

    # set new indicator
    data[, edited := 2L]
    data[, object_id := max_id(idf) + 1L]

    return(data)
}
# }}}
# set_default {{{
set_default <- function (idf_value) {
    idf_value[!is.na(default), value := default]
    idf_value[is.na(default) & !is.na(key),
        value := strsplit(key, " ", fixed = TRUE)[[1]][1]]

    return(idf_value)
}
# }}}
# set_fields {{{
set_fields <- function (object, orders, fields, type = c("add", "set"), idf, idd) {
    type <- match.arg(type)
    # prepare object for checking
    object <- copy(object)
    object[orders, set_value := fields]
    # for $add
    if (type == "add") {
        # defaults was set and field input is missing. Do not check, just give a
        # message.
        if (is_scalar(fields)) {
            line_skip <- object[, row_id := .I][is.na(set_value)][
                !is.na(default)][value == default, row_id]
        } else {
            line_skip <- object[, row_id := .I][map_lgl(set_value, is.null)][
                !is.na(default)][value == default, row_id]
        }
        mes <- add_output_field(object[line_skip], standard = FALSE, new_line = FALSE)[
            !is.na(suppressWarnings(as.numeric(value))), value := as.character(as.numeric(value))][
            , mes := sprintf(
                "Value for field %s in class %s is missing. Default value %s is used.",
                sQuote(output_field), sQuote(class), sQuote(value))][, mes]
    # for $set
    } else {
        if (is_scalar(fields)) {
            # skip fields whose input values are NAs
            line_skip <- object[, row_id := .I][is.na(set_value), row_id]
        } else {
            # skip fields whose input values are NULLs
            line_skip <- object[, row_id := .I][map_lgl(set_value, is.null), row_id]
        }
        mes <- NULL
    }
    # check fields
    if (not_empty(line_skip)) check_input <- object[-line_skip] else check_input <- object
    suppressWarnings(check_object(check_input, idf = idf, stop = TRUE))
    if (not_empty(mes)) message(sep_line("~"), "\n", paste0(mes, collapse = "\n"), "\n", sep_line("~"), "\n")

    # NOTE: In data.table, if fields is a scalar, 'set_value' is not a list but
    # a vector and other missing values will be NAs; if not a scalar,
    # 'set_value' will be a list and other missing values will be lists of NULL
    # if set more than one field
    if (is_scalar(fields)) {
        object[!is.na(set_value), value := as.character(set_value)]
    } else {
        l_to_ins <- object[!map_lgl(set_value, is.null), which = TRUE]
        for (i in l_to_ins) {
            set(object, i = i, j = "value", value = as.character(unlist(object$set_value[i])))
        }
    }

    # clean
    object[, set_value := NULL]
    if (has_name(object, "extra_required")) object[, extra_required := NULL]

    return(object)
}
# }}}
# get_class_name {{{
get_class_name <- function (object) {
    class_name <- object[, unique(class)]
    assert_that(is_scalar(class_name), msg = "Input has more than one objects")
    class_name
}
# }}}
# get_field_changes {{{
get_field_changes <- function (ori_object, new_object) {
    # get the original object
    ori_object <- ori_object[, list(class_order, field_order, value)]
    setnames(ori_object, "value", "ori_value")
    # get the new object
    new_object <- new_object[, list(class_order, field_order, value)]
    setnames(new_object, "value", "new_value")

    modified_fields <- ori_object[new_object,
        on = c("class_order", "field_order")][ori_value != new_value]

    return(modified_fields)
}
# }}}
# get_target_order {{{
get_target_order <- function (idf_value, index, fields, idf, idd) {
    class_name <- NULL
    if (is_integerish(index)) {
        assert_that(is_valid_id(index, idf))
        id <- index
        class_name <- idf_value[object_id == id, unique(class)]
    } else {
        assert_that(is_valid_class(index, idd))
        class_name <- index
    }

    class_data <- extract_class(class_name, idf, idd)

    # get field info
    field_length <- length(fields)
    field_name <- names(fields)

    num_cur <- idf_value[, .N]
    num_max <- class_data[, max_fields]

    # 1. check num of field input
    # {{{
    if (field_length > num_max) {
        stop(msg("Only *",num_max,"* fields are allowed for class
                 ",sQuote(class_name)," but *",field_length,"* are given."),
                 call. = FALSE)
    }
    # }}}

    # 2. get field order
    # {{{
    if (is.null(field_name)) {
        orders <- seq_along(fields)
    } else {
        idd_field <- extract_object(class_name, min = FALSE, idf, idd)
        # add standard field name
        idd_field <- add_output_field_name(idd_field)
        # add lower case field_name
        idd_field[, output_name_lower := tolower(output_name)]
        idd_field[, output_name_lower := gsub(" ", "_", output_name_lower, fixed = TRUE)]
        idd_field[, output_name_lower := gsub("-", "_", output_name_lower, fixed = TRUE)]
        if (all(grepl("^[a-z]", field_name))) {
            all_names <- idd_field[order(field_order)][, output_name_lower]
        } else if (all(grepl("^[A-Z]", field_name))) {
            all_names <- idd_field[order(field_order)][, output_name]
        } else {
            stop("Field names should be either in title-case or lower-case",
                 call. = FALSE)
        }
        orders <- match(field_name, all_names)
    }
    # }}}

    # 3. check field names
    # {{{
    invalid_name <- field_name[is.na(orders)]
    if (not_empty(invalid_name)) {
        stop(msg("Invalid field names found: ", sQuote(invalid_name), ". You can
                 find all invalid field names using \"$all('field', class = '",
                 class_name, "')\"."),
             call. = FALSE)
    }
    # }}}

    return(orders)
}
# }}}
# add_extra_required {{{
add_extra_required <- function (object, orders, fields, idf, idd) {
    # check new fields in the input
    # add extra index column
    object[, extra_required := FALSE]
    class_name <- get_class_name(object)
    if (max(orders) > object[, max(field_order)]) {
        # get orders for new fields
        diff_field_order <- setdiff(target_order, object[, field_order])
        # get orders for fields to fill
        new_field_order <- seq(min(diff_field_order), max(diff_field_order), by = 1L)
        fill_field_order <- setdiff(new_field_order, diff_field_order)
        # get new field data from idd and set the filled fields as required
        new_field <- extract_object(class_name, min = FALSE, idf, idd)[
            field_order %in% new_field_order][fill_field_order, extra_required := TRUE]
        # set default values
        set_default(new_field)
        object <- rbindlist(list(object, new_field), fill = TRUE)
    }

    return(object)
}
# }}}
# get_ref_key {{{
get_ref_key <- function (idf_value, idd) {
    field_ref_value <- idd$ref_object$field[idf_value,
        on = c("class_order", "field_order"), nomatch = 0L]

    return(field_ref_value)
}
# }}}
# get_referred {{{
get_referred <- function (idf_value, idf, idd) {
    all_needed <- c("ref_key", "class", "field", "field_anid", "units", "value")
    field_ref_value <- get_ref_key(idf_value, idd)[, .SD, .SDcol = all_needed]
    new_names <- c("ref_key", paste0("ref_", setdiff(all_needed, "ref_key")))
    setnames(field_ref_value, all_needed, new_names)

    field_referred <- field_ref_value[
        # insert ref keys, referred values
        idd$ref_object$key[idf$value, on = c("class_order", "field_order")],
        on = "ref_key"][!is.na(ref_key) & ref_value == value,
        .SD, .SDcol = c(new_names, "object_id", "class_order", "field_order")]

    setnames(field_referred,
        c("class_order", "field_order"),
        c("target_class_order", "target_field_order"))

    comb <- idf$value[field_referred, on = "object_id"]

    return(comb)
}
# }}}
# update_field_ref {{{
update_field_ref <- function (ori_object, new_object, idf, idd) {

    field_changes <- get_field_changes(ori_object, new_object)
    # find fields that have ref keys in the object
    field_ref_value <- get_ref_key(field_changes, idd)[
        , list(ref_key, ori_value, new_value)]
    if (not_empty(field_ref_value)) {
        # update new values
        target_fields <- field_ref_value[
            # insert ref keys, original values, and new values
            idd$ref_object$key[idf$value, on = c("class_order", "field_order"),
                               nomatch = 0L],
            on = "ref_key", nomatch = 0L][
            # get field locations
            !is.na(ref_key) & ori_value == value,
            c(key_col(idf$value), "new_value"), with = FALSE]

        idf$value <- target_fields[idf$value, on = c(key_col(idf$value))][
            !is.na(new_value), `:=`(value = new_value, edited = 1L)][
            , c("new_value") := NULL]

        idf$class <- target_fields[, list(object_id, new_value)][
            idf$class, on = "object_id"][
            !is.na(new_value), `:=`(edited = 1L)][
            , c("new_value") := NULL]
    }

    return(idf)
}

# }}}
# get_obj_count {{{
get_obj_count <- function (idf_object) {
    key_cols <- c("group_order", "group", "class_order", "class", "object_id")
    key_cols <- avail_cols(idf_object, key_cols)

    setorderv(idf_object, setdiff(key_cols, c("group", "class")))
    count_obj <- idf_object[, .N, by = c(key_cols)][
        , list(num = .N), by = c(setdiff(key_cols, c("object_id")))]

    return(count_obj)
}
# }}}
# add_log {{{
add_log <- function (action, id, new_id = 0L, idf) {
    action <- match.arg(action,
        c("get", "add", "set", "dup", "del", "hide", "notes", "diff", "save")
    )

    pre_step <- max(idf$log$step)
    log_dt <- data.table(step = pre_step + 1L, timestep = Sys.time(),
        action = action, id = list(id), new_id = new_id, active = TRUE)

    idf$log <- rbindlist(list(idf$log[.N, active := FALSE], log_dt))
    setorder(idf$log, step)

    return(idf)
}
# }}}
# get_output_str {{{
get_output_str <- function (idf, ids, action, new_ids = NULL) {
    objects <- idf$value[object_id %in% ids]
    comments <- NULL
    if (action %in% c("notes", "hide")) {
        idf_comment <- setorder(idf$comment, object_id, field_order)[
            object_id %in% ids]
        comments <- get_output_comment(idf_comment)[, output]
    }
    output <- get_output_line(objects, show_id = TRUE, show_class = TRUE,
        show_order = TRUE, show_diff = TRUE)[, output]

    return(c(comments, output))
}
# }}}
# max_id {{{
max_id <- function (idf) {
    max(attr(idf, "id"))
}
# }}}
# key_col {{{
key_col <- function (idf_obj, type = c("field", "class")) {
    type <- match.arg(type)

    if (type == "field") key_cols <- c("object_id", "class_order", "field_order")
    if (type == "class") key_cols <- c("group_order", "object_id", "class_order")

    avail_cols(idf_obj, key_cols)
}
# }}}
# get_deleted_id {{{
get_deleted_id <- function (idf) attr(idf, "id_del")
# }}}
# get_comment_attach_id {{{
get_comment_attach_id <- function (idf, id) {
    obj <- get_value(idf, id)

    class_name <- get_class_name(obj)

    prev_id <- idf$class[class == class_name][object_id < id, object_id]
    next_id <- idf$class[class == class_name][object_id > id, object_id]

    targ_id <- NULL
    if (is_empty(next_id)) {
        if (!is_empty(prev_id)) {
            targ_id <- max(prev_id)
        }
    } else {
        tar_id <- min(next_id)
    }

    # case when there is only one object in the class
    if (is.null(targ_id)) {

        prev_id <- idf$class[edited >= 0L][object_id < id, max(object_id)]
        next_id <- idf$class[edited >= 0L][object_id > id, min(object_id)]

        if (is_empty(next_id)) {
            if (is_empty(prev_id)) {
                stop("The commented object is the only object existed in the model", call. = FALSE)
            } else {
                targ_id <- prev_id
            }
        } else {
            tar_id <- next_id
        }
    }

    return(tar_id)
}
# }}}

# check
# check_object {{{
check_object <- function (idf_value, idf, stop = FALSE) {
    ori <- getOption("warning.length")
    options(warning.length = 8000)
    on.exit(options(warning.length = ori))
    if (has_name(idf_value, "set_value")) {
        setnames(idf_value, c("value", "set_value"), c("ori_value", "value"))
    }

    # get key columns
    cols <- key_col(idf_value, type = "field")

    valid_checks <- c("missing", "scalar", "an", "integer", "choice",
                      "object-list", "range", "auto")

    # get rid of autosizable and autocalculatable fields
    line_auto <- get_field_auto_line(idf_value)
    if (not_empty(line_auto)) {
        input <- idf_value[-line_auto]
    } else {
        input <- copy(idf_value)
    }

    input <- add_output_field(input, standard = FALSE, new_line = FALSE)
    if (has_name(input, "object_id")) {
        input[, output := sprintf("field %s in class %s with ID %s",
            sQuote(output_field), sQuote(class), object_id)]
    } else {
        input[, output := sprintf("field %s in class %s",
            sQuote(output_field), sQuote(class))]
    }

    # always checking missing values first
    mis <- check_field_missing(input)
    # get rid of lines with missing required values
    if (not_empty(mis)) input <- input[!mis[, .SD, .SDcol = cols], on = c(cols)]

    scalar <- check_field_scalar(input)
    if (not_empty(scalar)) input <- input[!scalar[, .SD, .SDcol = cols], on = c(cols)]

    auto <- check_field_auto(input)
    if (not_empty(auto)) input <- input[!auto[, .SD, .SDcol = cols], on = c(cols)]

    an <- check_field_an(input)
    if (not_empty(an)) input <- input[!an[, .SD, .SDcol = cols], on = c(cols)]

    int <- check_field_int(input)
    if (not_empty(int)) input <- input[!int[, .SD, .SDcol = cols], on = c(cols)]

    choice <- check_field_choice(input)
    if (not_empty(choice)) input <- input[!choice[, .SD, .SDcol = cols], on = c(cols)]

    objlist <- check_field_objlist(input, idf)
    if (not_empty(objlist)) input <- input[!objlist[, .SD, .SDcol = cols], on = c(cols)]

    range <- check_field_range(input)
    # if (not_empty(range)) input <- input[!range[, .SD, .SDcol = cols], on = c(cols)]

    res <- rbindlist(list(mis, scalar, an, int, choice, objlist, range, auto), fill = TRUE)

    if (not_empty(res)) {
        res[res[, .I[1L], by = check_type]$V1,
            output := paste0("\n", sep_line("="), "\n",
                             "Errors found when checking ", sQuote(check_type), "\n",
                             sep_line("-"), "\n", output)]
        res[res[, .I[.N], by = check_type]$V1,
            output := paste0(output, "\n", sep_line("="), "\n")]

        str <- paste0(res[, output], collapse = "\n")

        if (stop){
            stop(str, call. = FALSE)
        } else {
            message(str)
        }
    }

    # set names back
    if (has_name(idf_value, "ori_value")) {
        setnames(idf_value, c("ori_value", "value"), c("value", "set_value"))
    }

    return(res)
}
# }}}
# check_field_missing {{{
check_field_missing <- function (object) {
    if (!has_name(object, "required_field")) return(data.table())
    if (has_name(object, "extra_required")) {
        mis <- object[required_field == TRUE | extra_required == TRUE]
    } else {
        mis <- object[required_field == TRUE]
    }

    mis_null <- mis[map_lgl(value, is_empty)][
        , `:=`(check_type = "Missing Value")][
        , output := sprintf("Missing value for required %s", output)]

    mis_na <- mis[!map_lgl(value, is_empty)][map_lgl(value, is.na)][
        , `:=`(check_type = "Missing Value")][
        , output := sprintf("Missing value for required %s", output)]

    rbindlist(list(mis_null, mis_na))
}
# }}}
# check_field_scalar {{{
check_field_scalar <- function (object) {
    object[!map_lgl(value, is_scalar)][
        , `:=`(check_type = "Scalar", wrong = value)][
        , output := sprintf("Value %s for %s is not a scalar",
                            sQuote(wrong), output)]
}
# }}}
# check_field_an {{{
check_field_an <- function (object) {
    object[
        # for $add and $set
        if (is.list(value)) {
            (field_an == "A" & !map_lgl(value, is.character)) |
            (field_an == "N" & !map_lgl(value, is.numeric))
        } else {
            field_an == "N" & is.na(as.numeric(value))
        }][
        , `:=`(check_type = "Type", wrong = class(unlist(value)), right = field_an)][
        , output := sprintf("Invalid value type %s (which should be %s) for %s",
                            sQuote(wrong),
                            sQuote(ifelse(right == "A", "character", "numeric")),
                            output)]
}
# }}}
# check_field_int {{{
check_field_int <- function (object) {
    object[type == "integer"][
        # for $add and $set
        if (is.list(value)) !map_lgl(value, is_integerish)
        else !(!is.na(as.integer(value)) && as.integer(value) == as.numeric(value))][
        , `:=`(check_type = "Integer", wrong = as.character(value), right = as.character(as.integer(value)))][
        , output := sprintf("Input value %s for %s will should be an integer",
                            sQuote(wrong), output)]
}
# }}}
# check_field_choice {{{
check_field_choice <- function (object) {
    if (!has_name(object, "key")) return(data.table())
    choice <- object[!is.na(key)]
    if (is_empty(choice)) return(data.table())

    key_cols <- c("object_id", "class_order", "class", "field_order")
    key_cols <- avail_cols(object, key_cols)
    choice <- choice[, list(strsplit(key, " ", fixed = TRUE)),
        by = c(key_cols)][
        choice[, key := NULL], on = c(key_cols)][
        type == "choice"][!map2_lgl(value, V1, ~tolower(.x) %in% tolower(.y))]
    setnames(choice, "V1", "key")

    choice[, `:=`(check_type = "Choice", wrong = value, right = key)][
        , output := sprintf("Invalid value %s found for %s which should be one of choices %s",
                            sQuote(wrong), output, sQuote(right))]
}
# }}}
# check_field_objlist {{{
check_field_objlist <- function (object, idf) {
    if (!has_name(object, "object_list")) return(data.table())
    if (is_empty(idf$ref)) return(data.table())
    obj_list <- object[!is.na(object_list)]
    if (is_empty(obj_list)) return(data.table())

    obj_list[, row_id := .I]
    ref_keys <- obj_list[, strsplit(object_list, " ", fixed = TRUE), by = c("row_id")]
    ref_values <- idf$ref[ref_keys, on = c("ref_key" = "V1"), nomatch = 0L]

    obj_list <- ref_values[obj_list, on = c("row_id"), nomatch = 0L][
        map2_lgl(value, ref_value,
            ~{if (is.null(.x)||is.na(.x)||.x == "") FALSE
              else !tolower(.x) %in% tolower(.y)
            })]
    setnames(obj_list[, `:=`(row_id = NULL, key = NULL, ref_key = NULL)], "ref_value", "key")

    obj_list[, `:=`(check_type = "Reference", wrong = value, right = key)][
        , output := sprintf("Invalid value %s found for %s which should be one of references %s",
                            sQuote(wrong), output, sQuote(right))]
}
# }}}
# check_field_range {{{
check_field_range <- function (object) {
    max_dt <- check_field_maximum(object)
    min_dt <- check_field_minimum(object)
    maxe_dt <- check_field_maximum_exclu(object)
    mine_dt <- check_field_minimum_exclu(object)

    rbindlist(list(max_dt, min_dt, maxe_dt, mine_dt), use.names = TRUE)
}
# }}}
# check_field_maximum {{{
check_field_maximum <- function (object) {
    if (!has_name(object, "maximum")) return(data.table())

    object[!is.na(maximum)][map2_lgl(value, maximum, ~as.numeric(.x) > .y)][
           , `:=`(check_type = "Maximum", wrong = as.character(value), right = as.character(maximum))][
        , output := sprintf("Invalid value %s for %s which should be no more than (<=) %s",
                            sQuote(wrong), output, sQuote(right))]
}
# }}}
# check_field_maximum_exclu {{{
check_field_maximum_exclu <- function (object) {
    if (!has_name(object, "maximum<")) return(data.table())

    object[!is.na(`maximum<`)][map2_lgl(value, `maximum<`, ~as.numeric(.x) >= .y)][
           , `:=`(check_type = "Maximum<", wrong = as.character(value), right = as.character(`maximum<`))][
        , output := sprintf("Invalid value %s for %s which should be less than (<) %s",
                            sQuote(wrong), output, sQuote(right))]
}
# }}}
# check_field_minimum {{{
check_field_minimum <- function (object) {
    if (!has_name(object, "minimum")) return(data.table())

    object[!is.na(minimum)][map2_lgl(value, minimum, ~as.numeric(.x) < .y)][
           , `:=`(check_type = "Minimum", wrong = as.character(value), right = as.character(`minimum`))][
        , output := sprintf("Invalid value %s for %s which should be no less than (>=) %s",
                            sQuote(wrong), output, sQuote(right))]
}
# }}}
# check_field_minimum_exclu {{{
check_field_minimum_exclu <- function (object) {
    if (!has_name(object, "minimum>")) return(data.table())

    object[!is.na(`minimum>`)][map2_lgl(value, `minimum>`, ~as.numeric(.x) <= .y)][
           , `:=`(check_type = "Minimum>", wrong = as.character(value), right = as.character(`minimum>`))][
        # , output := paste0("Invalid value ",sQuote(wrong)," for ",output," should be more than (>) ",sQuote(right))]
        , output := sprintf("Value %s for %s should be more than (>) %s",
                            sQuote(wrong), output, sQuote(right))]
}
# }}}
# check_field_auto {{{
check_field_auto <- function (object) {
    if (!any(has_name(object, "autosizable"), has_name(object, "autocalculatable"))) {
        return(data.table())
    }

    res_size <- object[autosizable == FALSE][
        map_lgl(value, ~tolower(.x) == "autosize")][
        , `:=`(check_type = "Autosizable", wrong = value)]
    res_cal <- object[autocalculatable == FALSE][
        map_lgl(value, ~tolower(.x) == "autocalculate")][
        , `:=`(check_type = "Autocalculatable", wrong = value)]

    rbindlist(list(res_size, res_cal), fill = TRUE)[
        , output := sprintf("Value for %s is not but was set to %s" ,
                            output, sQuote(wrong))]
}
# }}}
# get_field_auto_line {{{
get_field_auto_line <- function (object) {
    if (!any(has_name(object, "autosizable"), has_name(object, "autocalculatable"))) {
        return(data.table())
    }

    object[!map_lgl(value, is_empty)][
        (autosizable == TRUE & map_lgl(value, ~tolower(.x) == "autosize")) |
        (autocalculatable == TRUE & map_lgl(value, ~tolower(.x) == "autocalculatable")), which = TRUE]
}
# }}}
