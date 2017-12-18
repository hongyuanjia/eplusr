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

##' @importFrom purrr map flatten_chr
##' @importFrom stringr str_to_upper
##' @importFrom data.table data.table rbindlist
##' @importFrom readr write_lines
## write_idf
## {{{1
#write_idf <- function (idf, path) {
#    header <-
#        c("!-Generator IDFEditor 1.48",
#          "!-Option SortedOrder",
#          "!-NOTE: All comments with '!-' are ignored by the IDFEditor and are generated automatically.",
#          "!-      Use '!' comments if they need to be retained when using the IDFEditor.",
#          ""
#          )

#    obj_header_info <-
#        purrr::map(unique(names(idf)),
#                   function (name) {
#                       upper_name <- stringr::str_to_upper(name)
#                       index <- grep(x = names(idf), pattern = paste0("^", name, "$"))[[1]]
#                       data.table::data.table(name = upper_name, index = index)
#                   }) %>% data.table::rbindlist(.)

#    idf_lines <-
#        purrr::map(seq_along(idf),
#                   function (i) {
#                       lines <- format_idf_lines(idf[i])
#                       if (!is.na(match(i, obj_header_info[["index"]]))) {
#                           class <- obj_header_info[index == i, name]
#                           obj_header_str <- paste0( "!-   ===========  ALL OBJECTS IN CLASS: ", class, " ===========")
#                           lines <- c(obj_header_str, lines)
#                       }
#                       return(lines)
#                   }) %>% purrr::flatten_chr()
#    idf <- c(header, idf_lines)
#    readr::write_lines(idf, path = path)
#}
## }}}1

#######################
##  helper functions  #
#######################

##' @importFrom readr read_lines
## read_idf_lines
## {{{1
#read_idf_lines <- function (file) {
#    idf <- readr::read_lines(file) %>% iconv(to = "UTF-8")
#    idf <- clean_idf_lines(idf)
#    return(idf)
#}
## }}}1

#regex_object <- "^([A-Z][A-Za-z].*),$"
## get_idf_object_name
## {{{1
#get_idf_object_name <- function(idf) {
#    regex_object <- "^([A-Z][A-Za-z].*),$"
#    object_names <- find_field(idf, regex_object)
#    object_names <- replace_field(object_names, regex_object, "\\1")
#    return(object_names)
#}
## }}}1

##' @importFrom data.table data.table
## get_idf_object_range
## {{{1
#get_idf_object_range <- function(idf){
#    regex_object <- "^([A-Z][A-Za-z].*),$"
#    object_names <- get_idf_object_name(idf)

#    # If the group only contains one object
#    if (length(object_names) == 1) {
#        point_object_start <-
#            `+`(find_field(idf, regex_object, value = F), 1)
#        point_object_end <- length(idf)
#    } else {
#        point_object_start <-
#            `+`(find_field(idf, regex_object, value = F), 1)
#        point_object_end <-
#            find_field(idf, regex_object, value = F) %>%
#            .[-1] %>% `-`(., 1) %>% c(., length(idf))
#    }

#    idf_object_range <-
#        data.table(object_name = object_names,
#                   object_start_row = point_object_start,
#                   object_end_row = point_object_end)

#    return(idf_object_range)
#}
## }}}1

##' @importFrom stringr str_split str_replace_all str_extract_all
##' @importFrom data.table data.table
## get_idf_object_fields
## {{{1
#get_idf_object_fields <- function (object) {
#    object_field <- stringr::str_split(object, pattern = "[,;]\\s*!\\s*-\\s*", simplify = TRUE)
#    value <- object_field[, 1]
#    field_unit <- object_field[, 2]
#    field <- stringr::str_replace_all(field_unit, pattern = "(.*)\\s\\{.+\\}", replacement = "\\1")
#    unit <- stringr::str_extract_all(field_unit, pattern = "\\{.+\\}")
#    unit <- stringr::str_replace_all(unit, pattern = "[\\{\\}]", replacement = "")
#    unit <- ifelse(unit == "character(0)", NA_character_, unit)
#    object <- data.table::data.table(value = value, field = field, unit = unit)
#    return(object)
#}
## }}}1

##' @importFrom purrr map2
## get_idf_object
## {{{1
#get_idf_object <- function (idf) {
#    object_ranges <- get_idf_object_range(idf)

#    objects <- purrr::map2(object_ranges$object_start_row,
#                           object_ranges$object_end_row,
#                           function (object_start, object_end) {
#                               object <- idf[object_start:object_end]
#                               object_fields <- get_idf_object_fields(object)
#                               return(object_fields)
#                           }) %>% set_names(object_ranges$object_name)
#    return(objects)
#}
## }}}1

##' @importFrom purrr map_chr
##' @importFrom stringr str_pad
## format_idf_lines
## {{{1
#format_idf_lines <- function (object) {
#    object_name <- names(object)
#    object <- object[[1]]
#    value <- object[["value"]]
#    field <- object[["field"]]
#    unit <- object[["unit"]]

#    lines <-
#        purrr::map_chr(seq(1:nrow(object)),
#                       function (i) {
#                           sep <- ifelse(i != nrow(object), ",", ";")
#                           value <- stringr::str_pad(paste0("    ", value[i], sep),
#                                                     29, side = "right")
#                           field <- field[i]
#                           unit <- unit[i]
#                           if (is.na(unit)) {
#                               line <- paste0(value, "!- ", field)
#                           } else {
#                               line <- paste0(value, "!- ", field, " {", unit, "}")
#                           }
#                           # if (i == nrow(object)) {
#                           #     line <- paste0(line, "\n\n")
#                           # }
#                           return(line)
#                       })

#    header <- paste0(object_name, ",")
#    object_lines <- c(header, lines)

#    return(object_lines)
#}
## }}}1

## get_idf_ver {{{
#get_idf_ver <- function (idf_str) {
#    ver_normal <- idf_str[endsWith(idf_str, "Version Identifier")]
#    ver_special <- idf_str[startsWith(idf_str, "Version")]

#    if (length(ver_normal) == 1L) {
#        ver_line <- ver_normal
#    } else if (length(ver_special) == 1L){
#        ver_line <- ver_special
#    } else {
#        return(NULL)
#    }

#    ver_pt <- regexpr("\\d", ver_line)
#    ver <- substr(ver_special, ver_pt, ver_pt + 2)

#    return(ver)
#}
## }}}

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

## get_idf_type
## {{{1
#get_idf_type <- function (idf) {
#    type <- attr(idf, "type")

#    if (is.null(type)) {
#        type <- "string"
#    }

#    return(type)
#}
## }}}1

##' @importFrom stringr str_trim
##' @importFrom purrr flatten_int map2
## clean_idf_lines
## {{{1
#clean_idf_lines <- function (idf_lines) {
#    regex_blank_line <- "^\\s*$"
#    regex_comment_line <- "^\\s*!.*$"

#    # Get rid of blank lines
#    idf_lines <- find_field(idf_lines, regex_blank_line, invert = TRUE)
#    # Get rid of comment lines
#    idf_lines <- find_field(idf_lines, regex_comment_line, invert = TRUE)
#    # Get rid of leading and trailing spaces
#    idf_lines <- stringr::str_trim(idf_lines, side = "both")

#    # # Get rid of Output:PreprocessorMessage
#    # regex_preproc_msg <- "^Output:PreprocessorMessage,"
#    # preproc_msg_start_pt <- stringr::str_which(idf_lines, regex_preproc_msg)
#    # preproc_msg_end_pt <- preproc_msg_start_pt + (diff(c(preproc_msg_start_pt, (length(idf_lines) + 1))) - 1)
#    # preproc_msg_rows <- purrr::flatten_int(purrr::map2(preproc_msg_start_pt, preproc_msg_end_pt, seq))
#    # if (length(preproc_msg_rows) > 0) {
#    #     idf_lines <- idf_lines[-preproc_msg_rows]
#    # }

#    return(idf_lines)
#}
## }}}1

##' @importFrom purrr flatten_int map
##' @importFrom stringr str_which str_trim
## is_model_str
## {{{1
#is_model_str <- function (text, lines = FALSE) {
#    # Get rid of blank lines, comment lines and leading&trailing spaces
#    # {{{2
#    regex_blank <- "^\\s*$"
#    regex_comment <- "^\\s*!.*$"
#    row_blank <- stringr::str_which(text, regex_blank)
#    row_comment <- stringr::str_which(text, regex_comment)
#    row_rem <- c(row_blank, row_comment)
#    if (length(row_rem) == 0L) {
#        text_cleaned <- stringr::str_trim(text)
#    } else {
#        text_cleaned <- stringr::str_trim(text[-sort(row_rem)])
#    }
#    # }}}2

#    # Return FALSE if no lines left
#    # {{{2
#    if (length(text_cleaned) == 0L) {
#        return(FALSE)
#    }
#    # }}}2

#    # Get EpMacro row number
#    # {{{2
#    macro_dict <-
#        c("##include", "##fileprefix", "##includesilent", "##nosilent", # Incorporating external files
#          "##if", "##ifdef", "##ifndef", "##elseif", "##else", "##endif", # Selective accepting or skipping lins
#          "##def", "##enddef", "##def1", "##set1", # Defining blocks of input
#          "#eval", "#\\[", # Arithmetic operations
#          "##list", "##nolist", "##show", "##noshow", "##showdetail", "##noshowdetail", # Marco debugging and listing
#          "##expandcomment", "##traceback", "##notraceback", "##write", "##nowrite", # Marco debugging and listing
#          "##symboltable", "##clear", "##reverse", "##!") # Marco debugging and listing
#    macro_rows <- purrr::flatten_int(purrr::map(macro_dict, ~stringr::str_which(text_cleaned, paste0("^", .x))))
#    # }}}2

#    # Get IDF row number
#    # {{{2
#    regex_object_head <- "^([A-Z][A-Za-z].*),$"
#    regex_object_special <- '^\\w.*\\s*,\\s*.*;$'
#    regex_field <- "^(((\\w|\\d|-|\\*|@@|\\.).*\\s*[,;])|[,;])\\s*!\\s*-"

#    head_rows <- stringr::str_which(text_cleaned, regex_object_head)
#    special_rows <- stringr::str_which(text_cleaned, regex_object_special)
#    field_rows <- stringr::str_which(text_cleaned, regex_field)
#    # }}}2

#    valid_lines <- sort(c(macro_rows, head_rows, special_rows, field_rows))

#    if (!lines) {
#        invalid <- setdiff(seq_along(text_cleaned), valid_lines)
#        if (length(invalid) == 0L) {
#            return(TRUE)
#        } else {
#            return(FALSE)
#        }
#    } else {
#        return(text[invalid])
#    }
#}
## }}}1

##' @importFrom purrr map_lgl
##' @importFrom stringr str_detect
## is_imf
## {{{1
#is_imf <- function (idf_lines) {
#    macro_dict <-
#        c("##include", "##fileprefix", "##includesilent", "##nosilent", # Incorporating external files
#          "##if", "##ifdef", "##ifndef", "##elseif", "##else", "##endif", # Selective accepting or skipping lins
#          "##def", "##enddef", "##def1", "##set1", # Defining blocks of input
#          "#eval", "#\\[", # Arithmetic operations
#          "##list", "##nolist", "##show", "##noshow", "##showdetail", "##noshowdetail", # Marco debugging and listing
#          "##expandcomment", "##traceback", "##notraceback", "##write", "##nowrite", # Marco debugging and listing
#          "##symboltable", "##clear", "##reverse", "##!") # Marco debugging and listing

#    is_imf <- any(purrr::map_lgl(macro_dict, ~any(stringr::str_detect(idf_lines, paste0("^", .x)))))

#    return(is_imf)
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
    is_imf <- is_imf(idf_str)
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
    idf_value <- idf_value_all[!is.na(line)]

    # wrong class and field references {{{
    idf_errors_wrong_references <- check_obj_ref(idf_value, idd)
    if (nrow(idf_errors_wrong_references) > 0L) {
        parse_issue(type = "Wrong value references", src = "IDF",
                    idf_errors_wrong_references)
    }
    # }}}

    idf <- list(version = idf_version,
                options = list(save_format = option_save,
                               special_format = option_special_format,
                               view_in_ip = option_view_in_ip_units,
                               idfeditor = option_idfeditor),
                class = idf_class,
                value = idf_value,
                comment = idf_comment)

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

    format <- match.arg(format)

    if (idf$options$idfeditor) {
        header_generator <- "!-Generator IDFEditor"
    } else {
        header_generator <- "!-Generator eplusr"
    }

    save_format <- switch(format,
           asis = idf$options$save_format,
           sorted = "SortedOrder",
           ori_bot = "OriginalOrderBottom",
           ori_top = "OriginalOrderTop")

    # Default use "SortedOrder"
    if (is.null(save_format)) {
        save_format <- "SortedOrder"
    }
    header_option <- paste0("!-Option ", save_format)

    # TODO: Add "UseSpecialFormat" and "ViewInIPunits" support
    header <- c(
        header_generator,
        header_option,
        "",
        "!-NOTE: All comments with '!-' are ignored by the IDFEditor and are generated automatically.",
        "!-      Use '!' comments if they need to be retained when using the IDFEditor.",
        "")

    idf_comment <- idf$comment[, .(row_id, line, object_id, comment, leading_spaces)]
    # init
    idf_comment[, output := ""]
    idf_comment[, output_space := strrep(" ", leading_spaces)]
    idf_comment[, output := paste0("!", output_space, comment)]
    idf_comment[, field_order := 0L]

    idf_value <- idf$value[, .(row_id, class_order, class, object_id, field_order,
                               value, field, field_anid,  units)]

    # init
    idf_value[, output := ""]

    # add class name
    idf_value[idf_value[, .I[1], by = .(object_id)]$V1,
              output := paste0(output, class, ",\n")]

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
    idf_value[, output := paste0(output, output_value, output_name, output_unit)]

    # combine comment and value
    idf_output <- rbindlist(list(idf_comment, idf_value), fill = TRUE)[
        , .(row_id, object_id, class_order, field_order, class, output)]
    idf_output <- idf_output[!is.na(class_order), .(row_id, class_order, class)][
        idf_output[, `:=`(class_order = NULL, class = NULL)],
        on = "row_id", roll = -Inf]

    # handle different save options
    # "SortedOrder"
    if (save_format == "SortedOrder") {
        setorder(idf_output, row_id)
        # add class heading
        idf_output[, class_group :=  .GRP, by = .(rleid(class))]
        idf_output[idf_output[, .I[1], by = .(class_group)]$V1,
                  output := paste0("\n!-   ===========  ALL OBJECTS IN CLASS: ",
                                   toupper(class), " ===========\n\n", output)]
        setorder(idf_output, class_order, object_id, field_order)
    }
    # "OriginalOrderTop"
    if (save_format == "OriginalOrderTop") {
        setorder(idf_output, row_id)
    }
    # "OriginalOrderBottom"
    if (save_format == "OriginalOrderBottom") {
        setorder(idf_output, row_id)
    }

    value_output <- idf_output[, output]

    output <- c(header, value_output)

    return(output)
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
# is_imf {{{1
is_imf <- function (idf_lines) {
    is_imf <- any(purrr::map_lgl(macro_dict, ~any(startsWith(idf_lines, .x))))

    return(is_imf)
}
# }}}1
# update_obj_ref {{{
update_obj_ref <- function (idf_value, idd) {
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
check_obj_ref <- function (idf_value, idd) {
    # get fields that have \object-list
    idf_ref_field <- data.table()
    if (slash_exists(idf_value, "object_list")) {
        idf_ref_field <- idf_value[!is.na(object_list),
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
    idf_ref_value <- update_obj_ref(idf_value, idd)

    idf_ref <- merge(idf_ref_field, idf_ref_value,
                     by.x = "object_list" , by.y = "ref_key")

    error_ref <- idf_ref[!(toupper(value) %chin% toupper(unlist(ref_value))),
        .(line, string)]

    return(error_ref)
}
# }}}
