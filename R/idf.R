#' @import data.table
#' @importFrom stringr str_pad
NULL

#' Parse EnergyPlus models
#'
#' @param path Path to EnergyPlus \code{IDF} or \code{IMF} file. The file
#' extension does not matter. So models stored in \code{TXT} file are still able
#' to correctly be parsed.
#' @param idd Path to \code{Energy+.idd} or an IDD object.
#'
#' @return A list contains the IDF version, option data, class data, value data,
#' comment data and field reference data.
#'
#' @export
# parse_idf {{{
parse_idf <- function (idf_str, idd) {

    idf_version <- get_idf_ver(idf_str)
    # check if input file is an imf file.
    is_imf <- has_macro(idf_str)

    idf_dt <- data.table(line = seq_along(idf_str), string = idf_str)

    # idf and idd version mismatch {{{
    idd_version <- idd$version
    if (!grepl(idf_version, idd_version, fixed = TRUE)) {
        warning(glue::glue("Version Mismatch. The file parsing is a differnet \\
            version '{idf_version}' than the EnergyPlus program and IDD file \\
            you are using '{substr(idd_version, 1L, 3L)}'. Editing and saving \\
            the file may make it incompatible with an older version of EnergyPlus."),
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
    setkey(idf_dt, line, type)

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
                                          .(line, string)]
    if (not_empty(idf_errors_unknown_macro)) {
        parse_issue(type = "Unknown macro found", src = "IDF",
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
    idf_dt[special_loc > 0L, comment := substr(string, explpt_loc + 2L, nchar(string))]
    idf_dt[special_loc < 0L & explpt_loc > 0L,
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
    if (not_empty(idf_option)) {
        idf_option[, space_loc := regexpr(" ", comment, fixed = TRUE)]
        idf_option[, `:=`(special_key = toupper(substr(comment, 1L, space_loc - 1L)),
                          special_value = toupper(trimws(substr(comment, space_loc + 1L, nchar(comment)))))]
        idf_option <- idf_option[special_key %chin% c("GENERATOR", "OPTION")]
        idf_option <- idf_option[, strsplit(special_value, " ", fixed = TRUE), by = .(line, string, special_key)]
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
        idf_errors_option_save <- idf_option[!is.na(option_save), .(line, string, option_save)]
        option_save <- idf_errors_option_save[, unique(option_save)]
        if (is_empty(option_save)) {
            option_save <- NULL
        }
        if (nrow(idf_errors_option_save) > 1L) {
            parse_issue(type = "More than one save option found", idf_errors_option_save,
                        src = "IDF", stop = FALSE,
                        info = glue::glue("Only the first option '{option_save[1]}' \\
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
        strsplit(value, "\\s*[,;]\\s*"), by = .(line)][
        idf_dt, on = "line"][value_count == 1L, V1 := value][, value := NULL]
    setnames(idf_dt, "V1", "value")
    # get row numeber of last field per condensed field line in each class
    line_value_last <- idf_dt[
        value_count > 1L & type == type_field_last, .(line_value_last = last(.I)), by = .(line, type)][
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
    idf_dt[type == type_field_last, object_id := .GRP, by = .(row_id)]
    idf_dt <- idf_dt[!is.na(object_id), .(row_id, object_id)][
        idf_dt[, object_id := NULL], on = c("row_id"), roll = -Inf]
    # }}}

    # COMMENT (MACRO)
    # {{{
    idf_comment <- idf_dt[type %in% c(type_macro, type_comment), .SD,
        .SDcol = c("type", "object_id", "comment", "leading_spaces")]
    idf_comment[is.na(leading_spaces), leading_spaces := 0L]
    idf_comment[leading_spaces < 0L, leading_spaces := 0L]
    idf_comment[, field_order := seq_along(.I), by = .(object_id)]
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
    idf_errors_duplicated_unique <- idf_class_all[unique_object == TRUE][duplicated(class), .(line, class)]
    if (not_empty(idf_errors_duplicated_unique)) {
        stop(glue::glue("Duplicated unique object found for class",
                        glue::collapse(idf_errors_duplicated_unique$class, sep = ",",
                                       last = " and ")),
             call. = FALSE)
    }
    # }}}
    # un-recognized class names {{{
    idf_errors_unknown_class <- idf_class_all[type == type_object][!is.na(value) & is.na(class), .(line, string)]
    if (not_empty(idf_errors_unknown_class)) {
        parse_issue(type = "Object type not recognized", idf_errors_unknown_class,
                    src = "IDF",
                    info = "This error may be caused by a misspelled object name.")
    }
    # }}}
    # Do not check missing required objects and fields for imf files.
    if (!is_imf) {
        # missing required object {{{
        idf_errors_missing_required <- idf_class_all[required_object == TRUE & is.na(line), class]
        if (not_empty(idf_errors_missing_required)) {
            stop(glue::glue("Missing required object ",
                            glue::collapse(idf_errors_missing_required)),
                 call. = FALSE)
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
    idf_field <- idf_class_all[!is.na(class), .(row_id, class_order, class)][
        idf_dt, on = c("row_id"), roll = Inf][
        # get rid of class lines
        type > type_object][, c("type", "class_upper_case") := NULL]
    # order fields per object
    idf_field[, field_order := seq_along(.I), by = .(object_id)]

    idf_value_all <- merge(idf_field, idd$field,
        by = c("class_order", "class", "field_order"), all = TRUE)
    # Error checking {{{
    # exceed max field {{{
    # get field number per class
    idf_num_fields <- idf_field[, .(num_fields = .N), by = .(object_id, class)]
    idf_errors_max_fields <- idf_class_all[type == type_object][idf_num_fields, on = c("object_id", "class")][
        num_fields > max_fields, .(line, class, max_fields, num_fields)]
    if (not_empty(idf_errors_max_fields)) {
        stop(glue::glue("Too many fields for objects \\
                        {glue::collapse(idf_errors_max_fields$class, sep = ',', last = ' and ')}.
                        {glue::collapse(rep('-', 60L))}
                        "),
             glue::glue_data("Line\tObject name\tField num.\tMax allowed
                             {line}\t{class}\t{num_fields}\t{max_fields}"),
             glue::collapse({rep('-', 60L)}),
             call. = FALSE)
    }
    # }}}
    # Do not check missing required objects and fields for imf files.
    if (!is_imf) {
        # missing required field {{{
        idf_errors_missing_required_field <- idf_value_all[!is.na(object_id)][required_field == TRUE & is.na(value)]
        if (not_empty(idf_errors_missing_required_field)) {
            stop(glue::glue("Missing required fields in objects \\
                            {glue::collapse(idf_errors_missing_required_field$class, sep = ',', last = ' and ')}.",
                            glue::collapse({rep('-', 60L)})),
                 glue::glue_data("Line\tObject name\tMissing
                                 {line}\t{class}\t{field}{{units}}
                                 "),
                 call. = FALSE)
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
        parse_issue(type = "Wrong value references", src = "IDF",
                    idf_errors_wrong_references)
    }
    # }}}
    # select needed columns after ref checking
    col_value_all <- names(idf_value)
    col_value_full <- c("object_id", "class_order", "class", "field_order",
        "field", "field_anid", "field_an", "field_id", "value", "units",
        "ip_units", "default", "key", "maximum", "maximum<", "minimum",
        "minimum>", "required_field", "object_list", "edited")
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
        idf_ref_value_field <- idd$ref_object$field[idf_value,
            on = c("class_order", "field_order"), nomatch = 0L][
            , .(ref_key, value)][
            , .(ref_value = c(.SD)), by = .(ref_key)]
    }

    # get class values that are referred
    idf_ref_value_class <- NULL
    if (!is.null(idd$ref_object$class)) {
        idf_ref_value_class <- idd$ref_object$class[idf_value,
            on = c("class_order"), nomatch = 0L][
            field_order == 1L, .(ref_key, class)][
            , .(ref_value = c(.SD)), by = .(ref_key)]
    }

    idf_ref_value <- rbindlist(list(idf_ref_value_class, idf_ref_value_field))

    return(idf_ref_value)
}
# }}}
# check_obj_ref {{{
check_obj_ref <- function (idf, idd) {
    # get fields that have \object-list
    idf_ref_field <- data.table()
    if (slash_exists(idf$value, "object_list")) {
        idf_ref_field <- idf$value[!is.na(object_list),
            .(row_id, line, string,
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
        .(line, string)]

    return(error_ref)
}
# }}}
# print.IDF {{{
print.IDF <- function (idf) {
    count_obj <- setorder(idf$class, group_order, class_order)[
        , .N, by = .(group, class, object_id)][
        , .(num = .N), by = .(group, class)][
        , num_obj := paste0("[", stringr::str_pad(num, 2, "left", "0"), "]")][
        , output := paste0(num_obj, " ", class)]

    output <- count_obj[count_obj[, .I[1], by = .(group)]$V1,
        output := paste0("\n", group, "\n", strrep("-", console_width()), "\n", output)]

    print_output(output)
}
# }}}
# link_idd {{{
link_idd <- function (ver) {
    if (is_pre_parsed(ver)) {
        switch(ver,
            "8.4" = idd_8.4,
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
        idd <- link_idd(ver)
        if (is.null(idd)) {
            stop(glue::glue("Input file has a version '{ver}' whose \\
                IDD file has not been pre-parsed. 'idd' should be specified."),
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
    comment <- get_output_comment(idf$comment)
    value <- get_output_line(idf$value)

    # combine comment and value {{{
    output_dt <- rbindlist(list(comment, value), fill = TRUE)[
        , .(edited, object_id, class_order, field_order, class, output)]
    output_dt <- output_dt[!is.na(class_order),
        .(edited, object_id, field_order, class_order, class)][
        output_dt[, `:=`(class_order = NULL, class = NULL)],
        on = c("edited", "object_id", "field_order"), roll = -Inf]
    # }}}

    # handle different save options {{{
    # "SortedOrder"
    if (save_format == "SortedOrder") {
        setorder(output_dt, class_order, object_id, field_order)
        # add class heading
        output_dt[, class_group := .GRP, by = .(rleid(class))]
        output_dt[output_dt[, .I[1], by = .(class_group)]$V1,
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
    idf_comment[type == -1L, output := paste0(output_space, comment)]
    # for normal comment lines
    idf_comment[type == 0L, output := paste0("!", output_space, comment)]
    setorder(idf_comment, object_id, -field_order)
    idf_comment[, field_order := -seq_along(.I), by = .(object_id)]
    setorder(idf_comment, object_id, field_order)

    return(idf_comment)
}
# }}}
# get_output_line {{{
get_output_line <- function (idf_value, keep_all = FALSE, show_id = FALSE,
                             show_class = TRUE, show_diff = FALSE,
                             show_ref = FALSE, fill_na = FALSE, indent = TRUE,
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

    # add class name
    if (show_class) {
        idf_value <- add_output_class(idf_value)
        comb_list <- c("output_class", comb_list)
    }

    # add object id
    if (show_id) {
        idf_value <- add_output_id(idf_value)
        comb_list <- c("output_id", comb_list)
    }

    # add field value
    idf_value <- add_output_value(idf_value, fill_na = fill_na, indent = indent)

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
    idf_value[idf_value[, .I[1], by = .(object_id)]$V1,
        output_id := paste0(
            "[ID:", stringr::str_pad(object_id, nchar(max_id), "left", " "), "] ")
    ]

    return(idf_value)
}
# }}}
# add_output_diff {{{
add_output_diff <- function (idf_value) {
    idf_value[, output_diff := "   "]
    # for deleted fields
    idf_value[edited ==-1L, output_diff := "(-)"]
    # for changed fields
    idf_value[edited == 1L, output_diff := "(~)"]
    # for added fields
    idf_value[edited == 2L, output_diff := "(+)"]

    return(idf_value)
}
# }}}
# add_output_class {{{
add_output_class <- function (idf_value) {
    idf_value[, output_class := ""]
    idf_value[idf_value[, .I[1], by = .(object_id)]$V1,
              output_class := paste0(class, ",\n")]

    return(idf_value)
}
# }}}
# add_output_value {{{
add_output_value <- function (idf_value, fill_na = FALSE, indent = TRUE) {
    idf_value[, value_fill := value]
    if (fill_na) {
        idf_value[is.na(value_fill), value_fill := ""]
    }
    idf_value[, output_value := paste0("    ", value_fill, ",")][
        , `:=`(value_fill = NULL)]
    idf_value[idf_value[, .I[.N], by = .(object_id)]$V1,
              output_value := sub(",$", ";", output_value)]

    if (indent) {
        idf_value[nchar(output_value) <= 29L,
                  output_value := stringr::str_pad(output_value, 29L, side = "right")]
        idf_value[nchar(output_value) > 29L,
                  output_value := paste0(output_value, "  ")]
    }

    return(idf_value)
}
# }}}
# add_output_field {{{
add_output_field <- function (idf_value, standard = TRUE, new_line = TRUE, keep_all = FALSE) {
    idf_value <- add_output_field_name(idf_value)
    idf_value <- add_output_field_unit(idf_value)

    if (standard) {
        idf_value[, output_name := paste0("!- ", output_name)]
    }

    if (new_line) {
        # add a new line after the last field per class
        idf_value[idf_value[, .I[.N], by = .(object_id)]$V1,
                  output_unit := paste0(output_unit, "\n")]
    }

    idf_value[, output_field := paste0(output_name, output_unit)]

    if (!keep_all) {
        idf_value[, `:=`(output_name = NULL, output_unit = NULL)]
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
# print_output {{{
print_output <- function (x, col = "output") {
    cat(x[[col]], sep = "\n")
}
# }}}

# valid_field {{{
valid_field <- function (class, idf, idd, verbose = TRUE) {
    class_name <- class
    assert_that(is_valid_class(class_name, idf))

    all_fields <- add_output_field(idd$field[class == class_name],
        standard = FALSE, new_line = FALSE, keep_all = TRUE)[
        , output_field := paste0(field_order, ": ", output_field)]

    if (verbose) {
        output <- copy(all_fields)[
            required_field == TRUE, output := paste0("*", output_field)][
            required_field == FALSE, output := paste0(" ", output_field)]
        print_output(output)
    }

    return(invisible(all_fields[, output_name]))
}
# }}}
# valid_class {{{
valid_class <- function (idf) {
    key_cols <- c("group_order", "class_order", "object_id")
    key_avail <- names(idf$class)[names(idf$class) %in% key_cols]
    setorderv(copy(idf$class), key_avail)[, unique(class)]
}
# }}}
# valid_id {{{
valid_id <- function (idf, verbose = TRUE) {
    idf_value <- idf$value[idf$value[, .I[1:3], by = .(object_id)]$V1][
        !is.na(class_order)]
    idf_value <- get_output_line(idf_value, show_id = TRUE)
    idf_value[field_order == 3L, output := "    ........\n"]

    setorder(idf_value, object_id, field_order)

    if (verbose) {
        print_output(idf_value)
    }

    return(invisible(idf$value[, unique(object_id)]))
}
# }}}

# get_class {{{
get_class <- function (idf, id) {
    assert_that(is_valid_id(id, idf))

    single_class <- idf$class[object_id %in% id]

    return(single_class)
}
# }}}

# find_object {{{
find_object <- function (idf, pattern, full = TRUE, ...) {

    if (full) {
        pattern = paste0("^(", pattern, ")$")
        idf_value <- idf$value[grepl(pattern, class)]
    } else {
        idf_value <- idf$value[grepl(pattern, class, ...)]
    }

    if (is_empty(idf_value)) {
        stop("No matched object found.", call. = FALSE)
    }

    object_count <- idf_value[, .N, by = .(class, object_id)][, .N, by = .(class)]
    idf_value <- get_output_line(idf_value, show_id = TRUE)
    idf_value <- object_count[idf_value, on = "class", roll = Inf]

    # add class heading
    setorder(idf_value, class_order, object_id, field_order)
    idf_value[, class_group := .GRP, by = .(rleid(class))]
    idf_value[idf_value[, .I[1], by = .(class_group)]$V1,
        output := paste0("\n",
            stringr::str_pad(paste0("== * ", N, " Objects Found in Class: ", class, " * "),
                             console_width(), side = "right", pad = "=" ),
            "\n\n", output)]

    setorder(idf_value, class_order, object_id, field_order)

    print_output(idf_value)

    return(invisible())
}
# }}}
# get_object {{{
get_object <- function (idf, id, verbose = TRUE) {
    lapply(id, function (x) assert_that(is_valid_id(x, idf)))

    single_object <- idf$value[object_id %in% id]

    single_output <- get_output_line(single_object, show_id = TRUE)

    if (verbose) {
        print_output(single_output)
        return(invisible(single_object))
    }

    return(single_object)
}
# }}}
# dup_object {{{
dup_object <- function (idf, id, new_name = NULL, idd, verbose = TRUE) {

    target_class <- get_class(idf, id = id)
    class_name <- target_class[, unique(class)]
    assert_that(can_be_duplicated(class_name, idf))

    target_object <- get_object(idf, id = id)

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
                stop("Given new name has been used for object [ID:", used_id, "]",
                     call. = FALSE)
            } else {
                used_id <- idf$value[class == class_name & field_order == 1L, value := new_name]
            }
        }
    } else {
        if (!is.null(new_name)) {
            warning("'new_name' is ignored for class ", class_name, call. = FALSE)
        }
    }
    # }}}

    # append all data
    new_idf <- append_data(target_class, target_object, type = "add", idf, idd)

    if (verbose) {
        print_output(get_output_line(target_object, show_class = TRUE, show_id = TRUE))
        return(invisible(new_idf))
    }

    return(new_idf)
}
# }}}
# add_object {{{
add_object <- function (idf, class, ..., min = TRUE, idd, verbose = TRUE) {
    class_name <- class
    assert_that(is_valid_class(class_name, idd))
    assert_that(can_be_duplicated(class_name, idf))

    new_class <- extract_class(class_name, idf, idd)
    new_object <- extract_object(class_name, min, idf, idd)

    # set default values first
    new_object <- set_default(new_object)

    # set specified values
    fields <- list(...)
    if (not_empty(fields)) {
        new_object <- set_fields(new_object, fields, idd = idd)
    }

    # check missing required fields
    missing_fields <- new_object[required_field == TRUE & is.na(value)]
    if (not_empty(missing_fields)) {
        new_object <- append_id(new_object, base = "value", idf)
        new_object[required_field == TRUE & is.na(value),
                   value := "[ ** missing ** ]"]
        missing_res <- get_output_line(new_object,
            show_class = FALSE, standard = TRUE, new_line = FALSE)[, output]
        stop("Missing required values for fields:\n",
             paste0(missing_res, collapse = "\n"), call. = FALSE)
    }

    # append all data
    new_idf <- append_data(new_class, new_object, type = "add", idf, idd)

    if (verbose) {
        print_output(get_output_line(new_object, show_class = TRUE, show_id = TRUE))
        return(invisible(new_idf))
    }

    return(new_idf)
}
# }}}
# set_object {{{
set_object <- function (idf, id, ..., idd, verbose = TRUE) {
    assert_that(is_valid_id(id, idf))
    fields <- list(...)
    assert_that(not_empty(fields), msg = "Field values are empty")

    target_class <- get_class(idf, id = id)

    ori_object <- get_object(idf, id, verbose = FALSE)
    new_object <- set_fields(ori_object, fields, idd)

    idf <- update_field_ref(ori_object, new_object, idf, idd)

    new_idf <- append_data(target_class, new_object, type = "change", idf, idd)

    if (verbose) {
        print_output(get_output_line(new_object, show_class = TRUE, show_id = TRUE))
        return(invisible(new_idf))
    }

    return(new_idf)
}
# }}}
# del_object {{{
del_object <- function (idf, id, idd, force = FALSE, verbose = TRUE) {

    target_class <- get_class(idf, id = id)
    class_name <- target_class[, unique(class)]
    assert_that(can_be_deleted(class_name, idf))

    target_object <- get_object(idf, id = id)

    # check if the fields in the target object have been referred by other
    # objects
    field_referred <- get_referred(target_object, idf, idd)
    if (not_empty(field_referred)) {
        field_output <- get_output_line(field_referred, show_id = TRUE,
            show_class = TRUE, show_ref = TRUE)
        ref_ids <- field_referred[, unique(object_id)]
        if (force) {
            warning(glue::glue("
                Force to delete object (ID:{id}) that has \\
                been referred. Errors may occur during simulations."),
                call. = FALSE)
        } else {
            stop(glue::glue("
                Some field(s) in current object (ID:{id}) has \\
                been referred by other object(s) \\
                (ID:{paste0(ref_ids, collapse = ', ')}) below. Comfirm by \\
                setting 'force' to TRUE."), "\n",
                paste0(field_output[, output], collapse = '\n'),
                call. = FALSE)
        }
    }

    idf$class <- idf$class[object_id != id]
    idf$value <- idf$value[object_id != id]
    idf$ref <- get_obj_ref(idf$value, idd)
    idf$del <- rbindlist(list(idf$del, copy(target_object)[, edited := -1L]))

    if (verbose) {
        print_output(get_output_line(target_object, show_class = TRUE, show_id = TRUE))
        return(invisible(idf))
    }

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
        new_object <- idd$field[class == class_name & field_order <= num_min_field]
    } else {
        new_object <- idd$field[class == class_name]
    }

    return(new_object)
}
# }}}
# append_data {{{
append_data <- function (class_data, object_data, type = c("add", "change"), idf, idd) {

    type <- match.arg(type)

    if (identical(type, "add")) {
        class_data <- append_id(class_data, base = "value", idf)
        object_data <- append_id(object_data, base = "value", idf)
    } else {
        class_data[, edited := 1L]
        # only mark fields that have been changed.
        id <- class_data[, unique(object_id)]
        assert_that(is_valid_id(id, idf))

        ori_object <- idf$value[object_id == id]
        field_order_changed <- get_field_changes(ori_object, object_data)[
            , field_order]
        object_data[field_order %in% field_order_changed, edited := 1L]

        idf$class <- idf$class[object_id != id]
        idf$value <- idf$value[object_id != id]
    }

    needed_cols_class <- names(idf$class)
    idf$class <- rbindlist(list(idf$class, class_data[, ..needed_cols_class]))
    setorder(idf$class, group_order, class_order, object_id)

    needed_cols_value <- names(idf$value)
    idf$value <- rbindlist(list(idf$value, object_data[, ..needed_cols_value]))
    setorder(idf$value, class_order, object_id, field_order)

    # update ref
    idf$ref <- get_obj_ref(idf$value, idd)

    return(idf)
}
# }}}
# append_id {{{
append_id <- function (data, base = c("class", "value"), idf) {
    base <- match.arg(base)

    # get max object id
    max_id <- idf[[base]][, max(object_id)]

    # set new indicator
    data[, edited := 2L]
    data[, object_id := max_id + 1L]

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
set_fields <- function (object, fields, idd) {

    class_name <- get_class_name(object)

    value_length <- length(fields)
    value_name <- names(fields)

    if (is.null(value_name)) {
        target_order <- seq_along(fields)
    } else {
        target_order <- get_field_order(object, value_name, idd = idd)
    }

    # TODO: check value types and min, max range

    # check value types {{{

    # }}}
    # check num of fields {{{
    max_fields <- idd$class[class == class_name, max_fields]
    if (value_length > max_fields) {
        stop("Only *", max_fields, "* fields are applicable for class",
             sQuote(class_name), ", but ", value_length, " are given.",
             call. = FALSE)
    }
    # }}}

    if (is.null(value_name)) {
        target_order <- seq_along(fields)
    } else {
        target_order <- get_field_order(object, value_name, idd = idd)

        # check invalid field names
        invalid_name <- value_name[is.na(target_order)]
        if (length(invalid_name) > 0L) {
            stop("Invalid field names found: ", sQuote(invalid_name),
                 ". You can find all invalid field names using \"$all('field', class = '", class_name, "')\".",
                 call. = FALSE)
        }
    }

    for (num in seq_along(fields)) {
        set(object, i = target_order[num], j = "value",
            value = as.character(fields[[num]]))
    }

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
# get_field_order {{{
get_field_order <- function (idf_value, field_name, id = NULL, idd) {

    if (is.null(id)) {
        if ("object_id" %in% names(idf_value)) {
            id <- idf_value[, unique(object_id)]
            if (length(id) > 1L) {
                stop("'id' required if more than one object exists in the input.", call. = FALSE)
            }
            target_class <- idf_value[object_id == id, unique(class)]
        } else {
            target_class <- idf_value[, unique(class)]
            assert_that(is_string(target_class), msg = "Multiple classes found in input. Cannot set fields.")
        }
    } else {
        assert_that(is_valid_id(id, idf))
        target_class <- idf_value[, unique(class)]
    }

    idd_field <- idd$field[class == target_class]
    # add standard field name
    idd_field <- add_output_field_name(idd_field)
    # add lower case field_name
    idd_field[, output_name_lower := tolower(output_name)]
    idd_field[, output_name_lower := gsub(" ", "_", output_name_lower, fixed = TRUE)]
    if (all(grepl("^[a-z]", field_name))) {
        all_names <- idd_field[order(field_order)][, output_name_lower]
    } else if (all(grepl("^[A-Z]", field_name))) {
        all_names <- idd_field[order(field_order)][, output_name]
    } else {
        stop("Field names should be either in title-case or lower-case",
             call. = FALSE)
    }

    orders <- match(field_name, all_names)

    return(orders)
}
# }}}
# get_field_changes {{{
get_field_changes <- function (ori_object, new_object) {
    # get the original object
    ori_object <- ori_object[, .(class_order, field_order, value)]
    setnames(ori_object, "value", "ori_value")
    # get the new object
    new_object <- new_object[, .(class_order, field_order, value)]
    data.table::setnames(new_object, "value", "new_value")

    modified_fields <- ori_object[new_object,
        on = c("class_order", "field_order")][ori_value != new_value]

    return(modified_fields)
}
# }}}
# get_ref_key {{{
get_ref_key <- function (idf_value, idd) {
    field_ref_value <- idd_8.8$ref_object$field[idf_value,
        on = c("class_order", "field_order"), nomatch = 0L]

    return(field_ref_value)
}
# }}}
# get_referred {{{
get_referred <- function (idf_value, idf, idd) {
    all_needed <- c("ref_key", "class", "field", "field_anid", "units", "value")
    field_ref_value <- get_ref_key(idf_value, idd)[, ..all_needed]
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
        , .(ref_key, ori_value, new_value)]

    # update new values
    idf$value <- field_ref_value[
        # insert ref keys, original values, and new values
        idd$ref_object$key[idf$value, on = c("class_order", "field_order")],
        on = "ref_key"][
        # udpate new values and mark changes
        !is.na(ref_key) & ori_value == value,
        `:=`(value = new_value, edited = 1L)][
        , c("ref_key", "ori_value", "new_value") := NULL]

    return(idf)
}

# }}}
# get_idf_diff {{{
get_idf_diff <- function (idf) {
    output_dt <- get_output_line(idf$value[edited > 0L],
        show_diff = TRUE, show_id = TRUE)[
        , .(object_id, class_order, field_order, output)]
    output_del <- get_output_line(idf$del,
        show_diff = TRUE, show_id = TRUE)[
        , .(object_id, class_order, field_order, output)]

    output_comb <- rbindlist(list(output_dt, output_del))
    setorder(output_comb, object_id, class_order, field_order)

    print_output(output_comb)

    return(invisible(output_comb))
}
# }}}
# get_obj_count {{{
get_obj_count <- function (idf_value) {
    count_obj <- setorder(idf_value, class_order, object_id, field_order)[
        , .N, by = .(class_order, class, object_id)][
        , .(num = .N), by = .(class_order, class)]

    return(count_obj)
}
# }}}
