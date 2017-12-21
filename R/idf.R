#################################################################################
##                       Parse EnergyPlus IDF/IMF File                          #
#################################################################################

##' @importFrom stringr str_interp str_length
## read_idf
## {{{1
#read_idf <- function(file, parse = TRUE, imf_to_idf = FALSE, verbose = FALSE) {

#    if (is_model_str(file)) {
#        idf_lines <- file
#    } else {
#        idf_lines <- read_idf_lines(file)
#        if (!is_model_str(idf_lines)) {
#            ori_error_len <- getOption("warning.length")
#            invalid_lines <- paste(is_model_str(idf_lines, lines = TRUE), collapse = "\n")
#            options("warning.length" = stringr::str_length(invalid_lines) + 1000)
#            stop(stringr::str_interp("Invalid lines found in the input:\n${invalid_lines}"), call. = FALSE)
#            on.exit(options("warning.length" = ori_error_len))
#        }
#    }

#    is_imf <- is_imf(idf_lines)

#    idf_ver <- get_idf_ver(idf_lines)

#    # If parametric fields exist, and parse is FALSE, give a warning.
#    if (is_param_exist(idf_lines)) {
#        if (parse) {
#            warning("Parametric fields found in the input file. ",
#                    "'parse' is forced to FALSE and only a string vector of input .idf/.imf file will be returned.",
#                    call. = FALSE)
#            parse <- FALSE
#        }
#    # NOTE: If parametric fields exist, the .imf file cannot directly converted
#    # to an .idf file.
#    # If no parametric fields in the input.
#    } else {
#        # If input is an .imf file
#        if (is_imf) {
#            # Convert the .imf file to a .idf file
#            if (imf_to_idf) {
#                idf_lines <- imf_to_idf(imf_lines = idf_lines, rename = FALSE, verbose = verbose, keep = FALSE)
#            # If imf_to_idf is FALSE,
#            } else {
#                # but 'parse' has been sepcified as FALSE, give a warning.
#                if (!parse) {
#                    warning("Unable to parse imf when 'imf_to_idf' is FALSE. ",
#                            "'parse' is forced to FALSE and only a string vector of input .idf/.imf file will be returned.",
#                            call. = FALSE)
#                    parse <- FALSE
#                }
#            }
#        }
#    }

#    # Return a string vector directly.
#    if (!parse) {
#        attrs <- list(ver = idf_ver,
#                      type = "string")
#        idf_lines <- add_attrs(idf_lines, attrs)
#        return(idf_lines)
#    # Return a parsed idf which is a list of data.tables.
#    } else {
#        attrs <- list(ver = idf_ver,
#                      type = "parsed")
#        idf <- get_idf_object(idf = idf_lines)
#        idf <- add_attrs(idf, attrs)
#        return(idf)
#    }
#}
## }}}1

##' @importFrom stringr str_which
## find_object
## {{{1
#find_object <- function (idf, pattern, ignore_case = TRUE, perl = TRUE, invert = FALSE) {
#    type <- get_idf_type(idf)

#    if (type == "string") {
#        macro_pt <- stringr::str_which(idf, "^##")
#        if (length(macro_pt) > 0) {
#            idf <- idf[-macro_pt]
#        }
#        object_ranges <- get_idf_object_range(idf)
#        object_names <- unique(object_ranges[["object_name"]])
#        objs <- grep(x = object_names, pattern = pattern, value = TRUE,
#                     ignore.case = ignore_case, perl = perl, invert = invert)
#        if (length(objs) == 0) {
#            stop("Could not find any matched objects.", call. = FALSE)
#        } else {
#            object_ranges_selected <- object_ranges[object_name %in% objs]
#            results <- map2(object_ranges_selected$object_start_row,
#                            object_ranges_selected$object_end_row,
#                            function (object_start, object_end) {
#                                object <- idf[object_start:object_end]
#                                object_fields <- get_idf_object_fields(object)
#                                return(object_fields)
#                            }) %>% set_names(make.unique(object_ranges_selected$object_name, sep = "_"))
#        }

#    } else if (type == "parsed") {
#        ori_names <- names(idf)
#        names(idf) <- make.unique(names(idf), sep = "_")
#        objs <- grep(x = names(idf), pattern = pattern, ignore.case = ignore_case, perl = perl, invert = invert)
#        if (length(objs) == 0) {
#            stop("Could not find any matched objects.", call. = FALSE)
#        } else {
#            results <- idf[c(objs)]
#        }
#        names(idf) <- ori_names
#    } else {
#        stop("Unknown input idf type.", call. = FALSE)
#    }
#    return(results)
#}
## }}}1

#######################
##  helper functions  #
#######################

##' @importFrom stringr str_detect
## is_param_exist
## {{{
#is_param_exist <- function (idf_lines) {
#    param_exist <- any(stringr::str_detect(idf_lines, "@@.*@@"))
#    return(param_exist)
#}
## }}}

##' @importFrom stringr str_extract_all
##' @importFrom purrr flatten_chr keep
## list_params
## {{{1
#list_params <- function (idf_lines) {
#    param_check <- stringr::str_extract_all(idf_lines, "@@.*@@")
#    params_all <- purrr::flatten_chr(purrr::keep(param_check, ~ length(.x) > 0))
#    params <- unique(params_all)
#    return(params)
#}
## }}}1

##' @importFrom tools file_path_sans_ext
##' @importFrom readr read_lines
##' @importFrom stringr str_which
##' @importFrom purrr map
## epmacro_exe
## {{{1
#epmacro_exe <- function (eplus_dir = find_eplus(), imf_path, rename = TRUE, verbose = TRUE) {
#    # In order to chain commands, this has to be used before commands.
#    cmd_head <- "cmd.exe /c"

#    # Use "energyplus.exe" to run EnergyPlus.
#    epmacro_exe <- file_path(eplus_dir, "EPMacro.exe")

#    # Get the working directory.
#    working_dir <- dirname(imf_path)

#    command <- paste(cmd_head, "cd", working_dir, "&&",
#                     epmacro_exe, imf_path, sep = " ")

#    epmacro_run <- system(command = command, wait = TRUE)

#    output <- file_path(working_dir, "out.idf")
#    new_output <- paste0(tools::file_path_sans_ext(imf_path), ".idf")
#    audit <- file_path(working_dir, "audit.out")
#    new_audit <- file_path(dirname(audit), paste0(file_prefix(imf_path), ".out"))

#    if (file.exists(output)) {

#        # Read error messages
#        audit_raw <- readr::read_lines(file_path(dirname(imf_path), "audit.out"))
#        errors_pt <- stringr::str_which(audit_raw, "ERROR")

#        if (rename) {
#            rename_flag <- file.copy(from = output, to = new_output,
#                                     copy.mode = TRUE, copy.date = TRUE,
#                                     overwrite = TRUE)

#            rename_flag_audit <- file.copy(from = audit, to = new_audit,
#                                           copy.mode = TRUE, copy.date = TRUE,
#                                           overwrite = TRUE)

#            if (rename_flag) {
#                # If renaming successed, delete the original output file.
#                unlink(output, force = TRUE)
#            } else {
#                warning("Could not rename the output idf file.", call. = FALSE)
#            }

#            if (rename_flag_audit) {
#                # If renaming successed, delete the original audit file.
#                unlink(audit, force = TRUE)
#            } else {
#                warning("Could not rename the audit file.", call. = FALSE)
#            }
#        }

#        if (verbose) {
#            if (rename) {
#                if (rename_flag) {
#                    message("File '", basename(imf_path), "' has been successfully converted to '",
#                            basename(new_output), "'.")
#                } else {
#                    message("File '", basename(imf_path), "' has been successfully converted to 'out.idf'.")
#                }

#                if (rename_flag_audit) {
#                    message("Please see '", basename(new_audit), "' for details.")
#                } else {
#                    message("Please see '", basename(audit), "' for details.")
#                }

#            } else {
#                message("File '", basename(imf_path), "' has been successfully converted to 'out.idf'.")
#                message("Please see '", basename(audit), "' for details.")
#            }

#            if (length(errors_pt) > 0) {
#                error_msg <-
#                    purrr::map(seq_along(errors_pt),
#                               ~{error_pt <- errors_pt[.x];
#                               msg <- c("Error message [", .x, "]:\n",
#                                        paste0(audit_raw[(error_pt-2):error_pt], collapse = "\n"))})
#                error_msg <- map_chr(error_msg, ~paste0(.x, collapse = ""))
#                error_msg <- paste0(error_msg, collapse = "\n\n")
#                warning("\nNOTE: ", length(errors_pt), " errors found during the conversion.\n", call. = FALSE)
#                warning(error_msg, call. = FALSE)
#            }
#        }

#    } else {
#        stop("Error occured when running 'EPMacro.exe'.", call. = FALSE)
#    }
#}
## }}}1

##' @importFrom stringi stri_rand_strings
##' @importFrom readr write_lines
## imf_to_idf
## {{{1
#imf_to_idf <- function(imf_lines, verbose = FALSE) {

#    # Get the input imf file version.
#    imf_ver <- get_idf_ver(imf_lines)

#    # Find the path of EnergyPlus with the same version.
#    eplus_dir <- find_eplus(ver = imf_ver, verbose = F)

#    imf_name <- paste0("imf_", stringi::stri_rand_strings(1, 8), ".imf")
#    imf_path <- file_path(normalizePath(tempdir()), imf_name)

#    readr::write_lines(imf_lines, path = imf_path)
#    # Convert the input imf file to a idf file
#    epmacro_exe(eplus_dir = eplus_dir, imf_path = imf_path,
#                verbose = verbose, rename = FALSE)

#    idf_path <- file_path(dirname(imf_path), "out.idf")
#    idf_lines <- read_idf_lines(idf_path)
#    unlink(imf_path, force = TRUE)

#    return(idf_lines)
#}
## }}}1

################################################################################
#                          Refact IDF parsing process                          #
################################################################################
# parse_idf {{{
parse_idf <- function (filepath, idd = NULL, eplus_dir = NULL) {

    # check idd {{{
    if (is.null(idd)) {
        if (is.null(eplus_dir)) {
            eplus_dir <- getOption("eplusr.eplus_dir")
        }
        idd_path <- normalizePath(file.path(eplus_dir, "Energy+.idd"))
        idd <- parse_idd(idd_path)
    } else if (is.character(idd) & length(idd) == 1L) {
        if (file.exists(idd)) {
            idd <- parse_idd(path)
        } else {
            stop("Input IDD file does not exist.", call. = FALSE)
        }
    } else if (!("IDD" %chin% class(idd))) {
        stop("'idd' should be a path of 'Energy+.idd' or an IDD object.",
             call. = FALSE)
    }
    # }}}

    idf_str <- read_idf(filepath)
    # check if input file is an imf file.
    is_imf <- is_imf_str(idf_str)
    idf_dt <- data.table(line = seq_along(idf_str), string = idf_str)

    # idf and idd version mismatch {{{
    idf_version <- get_idf_ver(idf_str)
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
    if (nrow(idf_errors_unknown_macro) > 0L) {
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
    # NOTE: treat macro lines as the same as comments
    idf_dt[type == type_macro, type := type_comment]
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
    if (length(option_save) == 0L) {
        option_save <- NULL
    }
    if (nrow(idf_errors_option_save) > 1L) {
        parse_issue(type = "More than one save option found", idf_errors_option_save,
                    src = "IDF", stop = FALSE,
                    info = glue::glue("Only the first option '{option_save[1]}' \\
                                       will be used."))
    }
    # for imf, always use "OriginalOrderBottom"
    if (is_imf) {
        option_save <- "OriginalOrderBottom"
    }
    # }}}

    # get rid of special comment lines
    idf_dt <- idf_dt[type != type_special]
    # handle condensed values {{{
    # if the sum of comma and semicolon > 2, then it must be a specially
    # formatted object or field. It may contain a class name, e.g.
    # 'Version,8.8;' and it may not, e.g. '0.0,0.0,0.0,' in
    # 'BuildingSurface:Detailed'.
    # get number of condensed values
    idf_dt[!is.na(value), `:=`(value_count = stringr::str_count(value, "[,;]"))]
    idf_dt[is.na(value), `:=`(value_count = 0L)]
    idf_dt <- idf_dt[type != type_comment, strsplit(value, "\\s*[,;]\\s*"), by = .(line)][
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
    # fill object_id backward for non-comment lines
    idf_dt[, is_comment := FALSE]
    idf_dt[type == type_comment, is_comment := TRUE]
    # link comments to object order
    idf_dt <- idf_dt[!is.na(object_id), .(row_id, object_id)][
        idf_dt[, object_id := NULL], on = c("row_id"), roll = -Inf]
    # }}}

    # get comments and macros
    idf_comment <- idf_dt[is_comment == TRUE, .SD,
        .SDcol = c("row_id", "object_id", "line", "comment", "string", "leading_spaces")]
    idf_comment[is.na(leading_spaces), leading_spaces := 0L]
    idf_comment[leading_spaces < 0L, leading_spaces := 0L]
    # get idf without comments
    idf_dt <- idf_dt[is_comment == FALSE, .SD,
         .SDcol = c("row_id", "object_id", "line", "type", "value", "comment", "string")]
    # class name should be the same of 'value' column for first line grouped by
    # object_id
    idf_dt[idf_dt[, .I[1], by = object_id]$V1,
           `:=`(type = type_object, class_upper_case = toupper(value))]

    # OBJECT
    # {{{
    # merge IDD class data
    idf_class_all <- merge(idf_dt, copy(idd$class)[, class_upper_case := toupper(class)],
        by = "class_upper_case", all = TRUE, sort = FALSE)
    idf_class <- idf_class_all[type == type_object]
    # add an indicator column to mark the class has been modified or not.
    idf_class[, edited := 0L]
    # select needed columns
    col_class_all <- names(idf_class)
    col_class_full <- c("object_id", "group_order", "group",
        "class_order", "class", "format", "min_fields", "max_fields",
        "required_object", "unique_object", "memo", "edited", "shit",
        # for error checking
        "line", "string", "value")
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
    idf_field[, field_order := seq_along(row_id), by = .(object_id)]

    idf_value_all <- merge(idf_field, idd$field,
        by = c("class_order", "class", "field_order"), all = TRUE)
    # }}}

    # Error checking
    # un-recognized class names {{{
    idf_errors_unknown_class <- idf_class[!is.na(value) & is.na(class), .(line, string)]
    if (nrow(idf_errors_unknown_class) > 0L) {
        parse_issue(type = "Object type not recognized", idf_errors_unknown_class,
                    src = "IDF",
                    info = "This error may be caused by a misspelled object name.")
    }
    # }}}
    # duplicated unqiue object {{{
    idf_errors_duplicated_unique <- idf_class_all[unique_object == TRUE][duplicated(class), .(line, class)]
    if (nrow(idf_errors_duplicated_unique) > 0L) {
        stop(glue::glue("Duplicated unique object found for class",
                        glue::collapse(idf_errors_duplicated_unique$class, sep = ",",
                                       last = " and ")),
             call. = FALSE)
    }
    # }}}
    # exceed max field {{{
    # get field number per class
    idf_num_fields <- idf_field[, .(num_fields = .N), by = .(object_id, class)]
    idf_errors_max_fields <- idf_class[idf_num_fields, on = c("object_id", "class")][
        num_fields > max_fields, .(line, class, max_fields, num_fields)]
    if (nrow(idf_errors_max_fields) > 0L) {
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
        # missing required object {{{
        idf_errors_missing_required <- idf_class_all[required_object == TRUE & is.na(line), class]
        if (length(idf_errors_missing_required) > 0L) {
            stop(glue::glue("Missing required object ",
                            glue::collapse(idf_errors_missing_required)),
                 call. = FALSE)
        }
        # }}}
        # missing required field {{{
        idf_errors_missing_required_field <- idf_value_all[!is.na(object_id)][required_field == TRUE & is.na(value)]
        if (nrow(idf_errors_missing_required_field) > 0L) {
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

    # clean up after error checking
    idf_class[, c("line", "string", "value") := NULL]
    idf_value <- idf_value_all[!is.na(line)][, c("reference", "comment") := NULL]
    # add an indicator column to mark the fields has been modified or not.
    idf_value[, edited := 0L]
    # select needed columns
    col_value_all <- names(idf_value)
    col_value_full <- c("row_id", "object_id", "class_order", "class",
        "field_order", "field", "field_anid", "field_an", "field_id", "value",
        "units", "ip_units", "default", "key", "maximum", "maximum<", "minimum",
        "minimum>", "required_field", "object_list", "edited")
    col_value_avail <- col_value_all[!is.na(match(col_value_all, col_value_full))]
    idf_value <- idf_value[, .SD, .SDcol = col_value_avail]

    # wrong class and field references {{{
    idf_errors_wrong_references <- check_obj_ref(idf_value, idd)
    if (nrow(idf_errors_wrong_references) > 0L) {
        parse_issue(type = "Wrong value references", src = "IDF",
                    idf_errors_wrong_references)
    }
    # }}}
    idf_ref <- get_obj_ref(idf_value, idd)

    idf <- list(version = idf_version,
                options = list(save_format = option_save,
                               special_format = option_special_format,
                               view_in_ip = option_view_in_ip_units,
                               idfeditor = option_idfeditor),
                class = idf_class,
                value = idf_value,
                comment = idf_comment,
                ref = idf_ref)

    class(idf) <- c("IDF", class(idf))
    return(idf)
}
# }}}
# read_idf {{{
read_idf <- function (filepath) {
    con = file(filepath)
    idf_str <- readLines(con, encoding = "UTF-8")
    # Get rid of unparsable characters
    idf_str <- iconv(idf_str, to = "UTF-8")
    close(con)

    # Get rid of preceeding and trailing spaces
    idf_str <- stringr::str_trim(idf_str)

    return(idf_str)
}
# }}}

# save_idf {{{
save_idf <- function (idf, format = c("asis", "sorted", "ori_bot", "ori_top")) {

    header <- get_output_header(idf$options, format = foramt)
    comment <- get_output_comment(idf$comment)
    value <- get_output_value(idf$value)

    # combine comment and value {{{
    output <- rbindlist(list(comment, value), fill = TRUE)[
        , .(row_id, object_id, class_order, field_order, class, output)]
    output <- output[!is.na(class_order), .(row_id, class_order, class)][
        output[, `:=`(class_order = NULL, class = NULL)],
        on = "row_id", roll = -Inf]
    # }}}

    # handle different save options {{{
    # "SortedOrder"
    if (save_format == "SortedOrder") {
        setorder(output, row_id)
        # add class heading
        output[, class_group :=  .GRP, by = .(rleid(class))]
        output[output[, .I[1], by = .(class_group)]$V1,
                  output := paste0("\n!-   ===========  ALL OBJECTS IN CLASS: ",
                                   toupper(class), " ===========\n\n", output)]
        setorder(output, class_order, object_id, field_order)
    }
    # "OriginalOrderTop"
    if (save_format == "OriginalOrderTop") {
        header <- c(header, "")
        setorder(output, row_id)
    }
    # "OriginalOrderBottom"
    if (save_format == "OriginalOrderBottom") {
        header <- c(header, "")
        setorder(output, row_id)
    }
    # }}}

    # # handle special format object {{{
    # if (idf$options$special_format) {

    # }
    # # }}}

    value_output <- output[, output]

    output <- c(header, value_output)

    return(output)
}
# }}}
# get_output_header {{{
get_output_header <- function (idf_options, format = c("asis", "sorted", "ori_bot", "ori_top")) {
    format <- match.arg(format)

    if (idf_options$idfeditor) {
        header_generator <- "!-Generator IDFEditor"
    } else {
        header_generator <- "!-Generator eplusr"
    }

    save_format <- switch(format,
           asis = idf_options$save_format,
           sorted = "SortedOrder",
           ori_bot = "OriginalOrderBottom",
           ori_top = "OriginalOrderTop")

    # Default use "SortedOrder"
    if (is.null(save_format)) {
        save_format <- "SortedOrder"
    }
    header_option <- paste0("!-Option ", save_format)

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
    idf_comment <- idf_comment[, .(row_id, line, object_id, comment, leading_spaces)]
    # init
    idf_comment[, output := ""]
    idf_comment[, output_space := strrep(" ", leading_spaces)]
    idf_comment[, output := paste0("!", output_space, comment)]
    idf_comment[, field_order := 0L]

    return(idf_comment)
}
# }}}
# get_output_value {{{
get_output_value <- function (idf_value, show_id = FALSE) {
    idf_value <- idf_value[, .(row_id, class_order, class, object_id, field_order,
                               value, field, field_anid,  units)]

    # add class name
    idf_value[, output_class := ""]
    idf_value[idf_value[, .I[1], by = .(object_id)]$V1,
              output_class := paste0(class, ",\n")]

    # for field name
    idf_value[is.na(field), output_name := paste0("!- ", field_anid)]
    idf_value[!is.na(field), output_name := paste0("!- ", field)]
    # for field unit
    idf_value[is.na(units), output_unit := ""]
    idf_value[!is.na(units), output_unit := paste0(" {", units, "}")]
    # add a new line after the last field per class
    idf_value[idf_value[, .I[.N], by = .(object_id)]$V1,
              output_unit := paste0(output_unit, "\n")]
    # for field value
    idf_value[, output_value := paste0("    ", value, ",")]
    idf_value[idf_value[, .I[.N], by = .(object_id)]$V1,
              output_value := sub(",$", ";", output_value)]
    # handle indentation
    idf_value[nchar(output_value) <= 29L,
              output_value := stringr::str_pad(output_value, 29L, side = "right")]
    idf_value[nchar(output_value) > 29L,
              output_value := paste0(output_value, "  ")]

    # combine
    if (show_id) {
        max_id <- idf_value[, max(object_id)]
        setorder(idf_value, class_order, object_id, field_order)
        idf_value[, output_id := ""]
        idf_value[idf_value[, .I[1], by = .(object_id)]$V1,
            output_id := paste0(
                "[ID:", stringr::str_pad(object_id, nchar(max_id), "left", " "), "] ")
        ]

        idf_value[, output := paste0(
            output_id, output_class, output_value, output_name, output_unit)]
    } else {
        idf_value[, output := paste0(
            output_class, output_value, output_name, output_unit)]
    }

    setorder(idf_value, class_order, object_id, field_order)

    return(idf_value)
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
# macro_dict {{{
macro_dict <-
      # Incorporating external files
    c("##include", "##fileprefix", "##includesilent", "##nosilent",
      # Selective accepting or skipping lins
      "##if", "##ifdef", "##ifndef", "##elseif", "##else", "##endif",
      # Defining blocks of input
      "##def", "##enddef", "##def1", "##set1",
      # Arithmetic operations
      "#eval", "#\\[",
      # Marco debugging and listing
      "##list", "##nolist", "##show", "##noshow", "##showdetail", "##noshowdetail",
      # Marco debugging and listing
      "##expandcomment", "##traceback", "##notraceback", "##write", "##nowrite",
      # Marco debugging and listing
      "##symboltable", "##clear", "##reverse", "##!")
# }}}
# is_imf_str {{{1
is_imf_str <- function (idf_lines) {
    is_imf_str <- any(purrr::map_lgl(macro_dict, ~any(startsWith(idf_lines, .x))))

    return(is_imf_str)
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
    if (nrow(idf_ref_field) == 0L) {
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
        output := paste0("\n", group, "\n---------------------------\n", output)]

    print_output(output)
}
# }}}
# find_object {{{
find_object <- function (idf, pattern, full = TRUE, ...) {

    if (full) {
        pattern = paste0(pattern, "$")
    }

    idf_value <- idf$value[grepl(pattern, class, ...)]
    if (nrow(idf_value) == 0L) {
        stop("No matched object found.", call. = FALSE)
    }

    object_count <- idf_value[, .N, by = .(class, object_id)][, .N, by = .(class)]
    idf_value <- get_output_value(idf_value, show_id = TRUE)
    idf_value <- object_count[idf_value, on = "class", roll = Inf]

    # add class heading
    setorder(idf_value, row_id)
    idf_value[, class_group := .GRP, by = .(rleid(class))]
    idf_value[idf_value[, .I[1], by = .(class_group)]$V1,
              output := paste0("\n",
                stringr::str_pad(paste0("== * ", N, " Objects Found in Class: ", class, " * "), console_width(), side = "right", pad = "=" ), "\n\n", output)]

    setorder(idf_value, class_order, object_id, field_order)

    print_output(idf_value)

    return(invisible())
}
# }}}
# valid_class {{{
valid_class <- function (idf) {
    setorder(copy(idf$class), group_order, class_order, object_id)[, unique(class)]
}
# }}}
# valid_id {{{
valid_id <- function (idf, verbose = TRUE) {
    idf_value <- idf$value[idf$value[, .I[1:3], by = .(class)]$V1][ !is.na(class_order)]
    idf_value <- get_output_value(idf_value, show_id = TRUE)
    idf_value[field_order == 3L, output := "    ........\n"]

    if (verbose) {
        print_output(idf_value)
    }

    return(invisible(idf$value[, unique(object_id)]))
}
# }}}
# print_output {{{
print_output <- function (x) {
    cat(x$output, sep = "\n")
}
# }}}
# console_width {{{
# Reference: `cli` (https://github.com/r-lib/cli)
console_width <- function() {
    width <- getOption(
        "cli.width",
        Sys.getenv("RSTUDIO_CONSOLE_WIDTH",
                   getOption("width", 80)
        )
    )

    return(as.integer(width))
}
# }}}
# get_object {{{
get_object <- function (idf, id) {
    if (!(id %in% valid_id(idf, verbose = FALSE))) {
        stop("Invalid object id. You can find all valid id using 'eplusr::valid_id'.", call. = FALSE)
    }

    single_object <- idf$value[object_id == id]

    single_output <- get_output_value(single_object, show_id = TRUE)

    print_output(single_output)

    return(invisible(NULL))
}
# }}}
# dup_object {{{
dup_object <- function (idf, id, new_name = NULL, idd) {

    max_id <- idf$value[, max(object_id)]
    max_row <- idf$value[, max(row_id)]
    max_line <- idf$value[, max(line)]

    target_object <- idf$value[object_id == id]
    target_class <- target_object[, unique(class)]
    # check if the target object is an unique object
    if (idd$class[class == target_class, unique_object]) {
        stop("The target object is an unique object which cannot be duplicated.",
             call. = FALSE)
    }

    # mark that this is a new object
    target_object[, edited := 2L]
    target_object[, object_id := max_id + 1L]
    target_object[, row_id := seq_along(max_row) + max_row]
    target_object[, line := seq_along(max_line) + max_line]

    # Give new name if applicable {{{
    if (target_object[field_order == 1L & grepl("Name", field, fixed = TRUE), .N]) {
        # get all names of objects in the same class
        all_names <- idf$value[class == target_class & field_order == 1L, value]
        # get all names of new or midified objects in the same class
        all_new_names <- idf$value[edited == 1L & class == target_class & field_order == 1L, value]
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
                used_id <- idf$value[class == target_class & field_order == 1L & field == new_name, object_id]
                stop("Given new name has been used for object [ID:", used_id, "]",
                     call. = FALSE)
            } else {
                used_id <- idf$value[class == target_class & field_order == 1L, value := new_name]
            }
        }
    } else {
        if (!is.null(new_name)) {
            warning("'new_name' is ignored for class ", target_class, call. = FALSE)
        }
    }
    # }}}

    idf$value <- rbindlist(list(idf$value, target_object))

    idf$ref <- get_obj_ref(idf$value, idd)

    return(idf)
}
# }}}
is.idf <- function (x) inherits(x, "IDF")
