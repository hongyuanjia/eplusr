################################################################################
#                       Parse EnergyPlus IDF/IMF File                          #
################################################################################

#' @importFrom stringr str_interp str_length
# read_idf
# {{{1
read_idf <- function(file, parse = TRUE, imf_to_idf = FALSE, verbose = FALSE) {

    if (is_model_str(file)) {
        idf_lines <- file
    } else {
        idf_lines <- read_idf_lines(file)
        if (!is_model_str(idf_lines)) {
            ori_error_len <- getOption("warning.length")
            invalid_lines <- paste(is_model_str(idf_lines, lines = TRUE), collapse = "\n")
            options("warning.length" = stringr::str_length(invalid_lines) + 1000)
            stop(stringr::str_interp("Invalid lines found in the input:\n${invalid_lines}"), call. = FALSE)
            on.exit(options("warning.length" = ori_error_len))
        }
    }

    is_imf <- is_imf(idf_lines)

    idf_ver <- get_idf_ver(idf_lines)

    # If parametric fields exist, and parse is FALSE, give a warning.
    if (is_param_exist(idf_lines)) {
        if (parse) {
            warning("Parametric fields found in the input file. ",
                    "'parse' is forced to FALSE and only a string vector of input .idf/.imf file will be returned.",
                    call. = FALSE)
            parse <- FALSE
        }
    # NOTE: If parametric fields exist, the .imf file cannot directly converted
    # to an .idf file.
    # If no parametric fields in the input.
    } else {
        # If input is an .imf file
        if (is_imf) {
            # Convert the .imf file to a .idf file
            if (imf_to_idf) {
                idf_lines <- imf_to_idf(imf_lines = idf_lines, rename = FALSE, verbose = verbose, keep = FALSE)
            # If imf_to_idf is FALSE,
            } else {
                # but 'parse' has been sepcified as FALSE, give a warning.
                if (!parse) {
                    warning("Unable to parse imf when 'imf_to_idf' is FALSE. ",
                            "'parse' is forced to FALSE and only a string vector of input .idf/.imf file will be returned.",
                            call. = FALSE)
                    parse <- FALSE
                }
            }
        }
    }

    # Return a string vector directly.
    if (!parse) {
        attrs <- list(ver = idf_ver,
                      type = "string")
        idf_lines <- add_attrs(idf_lines, attrs)
        return(idf_lines)
    # Return a parsed idf which is a list of data.tables.
    } else {
        attrs <- list(ver = idf_ver,
                      type = "parsed")
        idf <- get_idf_object(idf = idf_lines)
        idf <- add_attrs(idf, attrs)
        return(idf)
    }
}
# }}}1

#' @importFrom stringr str_which
# find_object
# {{{1
find_object <- function (idf, pattern, ignore_case = TRUE, perl = TRUE, invert = FALSE) {
    type <- get_idf_type(idf)

    if (type == "string") {
        macro_pt <- stringr::str_which(idf, "^##")
        if (length(macro_pt) > 0) {
            idf <- idf[-macro_pt]
        }
        object_ranges <- get_idf_object_range(idf)
        object_names <- unique(object_ranges[["object_name"]])
        objs <- grep(x = object_names, pattern = pattern, value = TRUE,
                     ignore.case = ignore_case, perl = perl, invert = invert)
        if (length(objs) == 0) {
            stop("Could not find any matched objects.", call. = FALSE)
        } else {
            object_ranges_selected <- object_ranges[object_name %in% objs]
            results <- map2(object_ranges_selected$object_start_row,
                            object_ranges_selected$object_end_row,
                            function (object_start, object_end) {
                                object <- idf[object_start:object_end]
                                object_fields <- get_idf_object_fields(object)
                                return(object_fields)
                            }) %>% set_names(make.unique(object_ranges_selected$object_name, sep = "_"))
        }

    } else if (type == "parsed") {
        ori_names <- names(idf)
        names(idf) <- make.unique(names(idf), sep = "_")
        objs <- grep(x = names(idf), pattern = pattern, ignore.case = ignore_case, perl = perl, invert = invert)
        if (length(objs) == 0) {
            stop("Could not find any matched objects.", call. = FALSE)
        } else {
            results <- idf[c(objs)]
        }
        names(idf) <- ori_names
    } else {
        stop("Unknown input idf type.", call. = FALSE)
    }
    return(results)
}
# }}}1

#' @importFrom purrr map flatten_chr
#' @importFrom stringr str_to_upper
#' @importFrom data.table data.table rbindlist
#' @importFrom readr write_lines
# write_idf
# {{{1
write_idf <- function (idf, path) {
    header <-
        c("!-Generator IDFEditor 1.48",
          "!-Option SortedOrder",
          "!-NOTE: All comments with '!-' are ignored by the IDFEditor and are generated automatically.",
          "!-      Use '!' comments if they need to be retained when using the IDFEditor.",
          ""
          )

    obj_header_info <-
        purrr::map(unique(names(idf)),
                   function (name) {
                       upper_name <- stringr::str_to_upper(name)
                       index <- grep(x = names(idf), pattern = paste0("^", name, "$"))[[1]]
                       data.table::data.table(name = upper_name, index = index)
                   }) %>% data.table::rbindlist(.)

    idf_lines <-
        purrr::map(seq_along(idf),
                   function (i) {
                       lines <- format_idf_lines(idf[i])
                       if (!is.na(match(i, obj_header_info[["index"]]))) {
                           class <- obj_header_info[index == i, name]
                           obj_header_str <- paste0( "!-   ===========  ALL OBJECTS IN CLASS: ", class, " ===========")
                           lines <- c(obj_header_str, lines)
                       }
                       return(lines)
                   }) %>% purrr::flatten_chr()
    idf <- c(header, idf_lines)
    readr::write_lines(idf, path = path)
}
# }}}1

######################
#  helper functions  #
######################

#' @importFrom readr read_lines
# read_idf_lines
# {{{1
read_idf_lines <- function (file) {
    idf <- readr::read_lines(file) %>% iconv(to = "UTF-8")
    idf <- clean_idf_lines(idf)
    return(idf)
}
# }}}1

regex_object <- "^([A-Z][A-Za-z].*),$"
# get_idf_object_name
# {{{1
get_idf_object_name <- function(idf) {
    regex_object <- "^([A-Z][A-Za-z].*),$"
    object_names <- find_field(idf, regex_object)
    object_names <- replace_field(object_names, regex_object, "\\1")
    return(object_names)
}
# }}}1

#' @importFrom data.table data.table
# get_idf_object_range
# {{{1
get_idf_object_range <- function(idf){
    regex_object <- "^([A-Z][A-Za-z].*),$"
    object_names <- get_idf_object_name(idf)

    # If the group only contains one object
    if (length(object_names) == 1) {
        point_object_start <-
            `+`(find_field(idf, regex_object, value = F), 1)
        point_object_end <- length(idf)
    } else {
        point_object_start <-
            `+`(find_field(idf, regex_object, value = F), 1)
        point_object_end <-
            find_field(idf, regex_object, value = F) %>%
            .[-1] %>% `-`(., 1) %>% c(., length(idf))
    }

    idf_object_range <-
        data.table(object_name = object_names,
                   object_start_row = point_object_start,
                   object_end_row = point_object_end)

    return(idf_object_range)
}
# }}}1

#' @importFrom stringr str_split str_replace_all str_extract_all
#' @importFrom data.table data.table
# get_idf_object_fields
# {{{1
get_idf_object_fields <- function (object) {
    object_field <- stringr::str_split(object, pattern = "[,;]\\s*!\\s*-\\s*", simplify = TRUE)
    value <- object_field[, 1]
    field_unit <- object_field[, 2]
    field <- stringr::str_replace_all(field_unit, pattern = "(.*)\\s\\{.+\\}", replacement = "\\1")
    unit <- stringr::str_extract_all(field_unit, pattern = "\\{.+\\}")
    unit <- stringr::str_replace_all(unit, pattern = "[\\{\\}]", replacement = "")
    unit <- ifelse(unit == "character(0)", NA_character_, unit)
    object <- data.table::data.table(value = value, field = field, unit = unit)
    return(object)
}
# }}}1

#' @importFrom purrr map2
# get_idf_object
# {{{1
get_idf_object <- function (idf) {
    object_ranges <- get_idf_object_range(idf)

    objects <- purrr::map2(object_ranges$object_start_row,
                           object_ranges$object_end_row,
                           function (object_start, object_end) {
                               object <- idf[object_start:object_end]
                               object_fields <- get_idf_object_fields(object)
                               return(object_fields)
                           }) %>% set_names(object_ranges$object_name)
    return(objects)
}
# }}}1

#' @importFrom purrr map_chr
#' @importFrom stringr str_pad
# format_idf_lines
# {{{1
format_idf_lines <- function (object) {
    object_name <- names(object)
    object <- object[[1]]
    value <- object[["value"]]
    field <- object[["field"]]
    unit <- object[["unit"]]

    lines <-
        purrr::map_chr(seq(1:nrow(object)),
                       function (i) {
                           sep <- ifelse(i != nrow(object), ",", ";")
                           value <- stringr::str_pad(paste0("    ", value[i], sep),
                                                     29, side = "right")
                           field <- field[i]
                           unit <- unit[i]
                           if (is.na(unit)) {
                               line <- paste0(value, "!- ", field)
                           } else {
                               line <- paste0(value, "!- ", field, " {", unit, "}")
                           }
                           # if (i == nrow(object)) {
                           #     line <- paste0(line, "\n\n")
                           # }
                           return(line)
                       })

    header <- paste0(object_name, ",")
    object_lines <- c(header, lines)

    return(object_lines)
}
# }}}1

#' @importFrom stringr str_which str_extract
# get_idf_ver
# {{{1
get_idf_ver <- function (idf_lines) {
    ver_pt_normal <- stringr::str_which(idf_lines, "^\\d\\.\\d\\s*;\\s*\\!\\s*-\\s*Version Identifier$")
    ver_pt_special <- stringr::str_which(idf_lines, "^Version\\s*,\\s*\\d\\.\\d;$")

    if (length(ver_pt_normal) == 1) {
        idf_ver <- stringr::str_extract(idf_lines[ver_pt_normal], "\\d+\\.\\d+")
    } else if (length(ver_pt_special) == 1) {
        idf_ver <- stringr::str_extract(idf_lines[ver_pt_special], "\\d+\\.\\d+")
    } else {
        return(NULL)
    }

    return(idf_ver)
}
# }}}1

#' @importFrom stringr str_detect
# is_param_exist
# {{{
is_param_exist <- function (idf_lines) {
    param_exist <- any(stringr::str_detect(idf_lines, "@@.*@@"))
    return(param_exist)
}
# }}}

#' @importFrom stringr str_extract_all
#' @importFrom purrr flatten_chr keep
# list_params
# {{{1
list_params <- function (idf_lines) {
    param_check <- stringr::str_extract_all(idf_lines, "@@.*@@")
    params_all <- purrr::flatten_chr(purrr::keep(param_check, ~ length(.x) > 0))
    params <- unique(params_all)
    return(params)
}
# }}}1

#' @importFrom tools file_path_sans_ext
#' @importFrom readr read_lines
#' @importFrom stringr str_which
#' @importFrom purrr map
# epmacro_exe
# {{{1
epmacro_exe <- function (eplus_dir = find_eplus(), imf_path, rename = TRUE, verbose = TRUE) {
    # In order to chain commands, this has to be used before commands.
    cmd_head <- "cmd.exe /c"

    # Use "energyplus.exe" to run EnergyPlus.
    epmacro_exe <- file_path(eplus_dir, "EPMacro.exe")

    # Get the working directory.
    working_dir <- dirname(imf_path)

    command <- paste(cmd_head, "cd", working_dir, "&&",
                     epmacro_exe, imf_path, sep = " ")

    epmacro_run <- system(command = command, wait = TRUE)

    output <- file_path(working_dir, "out.idf")
    new_output <- paste0(tools::file_path_sans_ext(imf_path), ".idf")
    audit <- file_path(working_dir, "audit.out")
    new_audit <- file_path(dirname(audit), paste0(tools::file_path_sans_ext(basename(imf_path)), ".out"))

    if (file.exists(output)) {

        # Read error messages
        audit_raw <- readr::read_lines(file_path(dirname(imf_path), "audit.out"))
        errors_pt <- stringr::str_which(audit_raw, "ERROR")

        if (rename) {
            rename_flag <- file.copy(from = output, to = new_output,
                                     copy.mode = TRUE, copy.date = TRUE,
                                     overwrite = TRUE)

            rename_flag_audit <- file.copy(from = audit, to = new_audit,
                                           copy.mode = TRUE, copy.date = TRUE,
                                           overwrite = TRUE)

            if (rename_flag) {
                # If renaming successed, delete the original output file.
                unlink(output, force = TRUE)
            } else {
                warning("Could not rename the output idf file.", call. = FALSE)
            }

            if (rename_flag_audit) {
                # If renaming successed, delete the original audit file.
                unlink(audit, force = TRUE)
            } else {
                warning("Could not rename the audit file.", call. = FALSE)
            }
        }

        if (verbose) {
            if (rename) {
                if (rename_flag) {
                    message("File '", basename(imf_path), "' has been successfully converted to '",
                            basename(new_output), "'.")
                } else {
                    message("File '", basename(imf_path), "' has been successfully converted to 'out.idf'.")
                }

                if (rename_flag_audit) {
                    message("Please see '", basename(new_audit), "' for details.")
                } else {
                    message("Please see '", basename(audit), "' for details.")
                }

            } else {
                message("File '", basename(imf_path), "' has been successfully converted to 'out.idf'.")
                message("Please see '", basename(audit), "' for details.")
            }

            if (length(errors_pt) > 0) {
                error_msg <-
                    purrr::map(seq_along(errors_pt),
                               ~{error_pt <- errors_pt[.x];
                               msg <- c("Error message [", .x, "]:\n",
                                        paste0(audit_raw[(error_pt-2):error_pt], collapse = "\n"))})
                error_msg <- map_chr(error_msg, ~paste0(.x, collapse = ""))
                error_msg <- paste0(error_msg, collapse = "\n\n")
                warning("\nNOTE: ", length(errors_pt), " errors found during the conversion.\n", call. = FALSE)
                warning(error_msg, call. = FALSE)
            }
        }

    } else {
        stop("Error occured when running 'EPMacro.exe'.", call. = FALSE)
    }
}
# }}}1

#' @importFrom stringi stri_rand_strings
#' @importFrom readr write_lines
# imf_to_idf
# {{{1
imf_to_idf <- function(imf_lines, verbose = FALSE) {

    # Get the input imf file version.
    imf_ver <- get_idf_ver(imf_lines)

    # Find the path of EnergyPlus with the same version.
    eplus_dir <- find_eplus(ver = imf_ver, verbose = F)

    imf_name <- paste0("imf_", stringi::stri_rand_strings(1, 8), ".imf")
    imf_path <- file_path(normalizePath(tempdir()), imf_name)

    readr::write_lines(imf_lines, path = imf_path)
    # Convert the input imf file to a idf file
    epmacro_exe(eplus_dir = eplus_dir, imf_path = imf_path,
                verbose = verbose, rename = FALSE)

    idf_path <- file_path(dirname(imf_path), "out.idf")
    idf_lines <- read_idf_lines(idf_path)
    unlink(imf_path, force = TRUE)

    return(idf_lines)
}
# }}}1

# get_idf_type
# {{{1
get_idf_type <- function (idf) {
    type <- attr(idf, "type")

    if (is.null(type)) {
        type <- "string"
    }

    return(type)
}
# }}}1

#' @importFrom stringr str_trim
#' @importFrom purrr flatten_int map2
# clean_idf_lines
# {{{1
clean_idf_lines <- function (idf_lines) {
    regex_blank_line <- "^\\s*$"
    regex_comment_line <- "^\\s*!.*$"

    # Get rid of blank lines
    idf_lines <- find_field(idf_lines, regex_blank_line, invert = TRUE)
    # Get rid of comment lines
    idf_lines <- find_field(idf_lines, regex_comment_line, invert = TRUE)
    # Get rid of leading and trailing spaces
    idf_lines <- stringr::str_trim(idf_lines, side = "both")

    # # Get rid of Output:PreprocessorMessage
    # regex_preproc_msg <- "^Output:PreprocessorMessage,"
    # preproc_msg_start_pt <- stringr::str_which(idf_lines, regex_preproc_msg)
    # preproc_msg_end_pt <- preproc_msg_start_pt + (diff(c(preproc_msg_start_pt, (length(idf_lines) + 1))) - 1)
    # preproc_msg_rows <- purrr::flatten_int(purrr::map2(preproc_msg_start_pt, preproc_msg_end_pt, seq))
    # if (length(preproc_msg_rows) > 0) {
    #     idf_lines <- idf_lines[-preproc_msg_rows]
    # }

    return(idf_lines)
}
# }}}1

#' @importFrom purrr flatten_int map
#' @importFrom stringr str_which str_trim
# is_model_str
# {{{1
is_model_str <- function (text, lines = FALSE) {
    # Get rid of blank lines, comment lines and leading&trailing spaces
    # {{{2
    regex_blank <- "^\\s*$"
    regex_comment <- "^\\s*!.*$"
    row_blank <- stringr::str_which(text, regex_blank)
    row_comment <- stringr::str_which(text, regex_comment)
    row_rem <- c(row_blank, row_comment)
    if (length(row_rem) == 0L) {
        text_cleaned <- stringr::str_trim(text)
    } else {
        text_cleaned <- stringr::str_trim(text[-sort(row_rem)])
    }
    # }}}2

    # Return FALSE if no lines left
    # {{{2
    if (length(text_cleaned) == 0L) {
        return(FALSE)
    }
    # }}}2

    # Get EpMacro row number
    # {{{2
    macro_dict <-
        c("##include", "##fileprefix", "##includesilent", "##nosilent", # Incorporating external files
          "##if", "##ifdef", "##ifndef", "##elseif", "##else", "##endif", # Selective accepting or skipping lins
          "##def", "##enddef", "##def1", "##set1", # Defining blocks of input
          "#eval", "#\\[", # Arithmetic operations
          "##list", "##nolist", "##show", "##noshow", "##showdetail", "##noshowdetail", # Marco debugging and listing
          "##expandcomment", "##traceback", "##notraceback", "##write", "##nowrite", # Marco debugging and listing
          "##symboltable", "##clear", "##reverse", "##!") # Marco debugging and listing
    macro_rows <- purrr::flatten_int(purrr::map(macro_dict, ~stringr::str_which(text_cleaned, paste0("^", .x))))
    # }}}2

    # Get IDF row number
    # {{{2
    regex_object_head <- "^([A-Z][A-Za-z].*),$"
    regex_object_special <- '^\\w.*\\s*,\\s*.*;$'
    regex_field <- "^(((\\w|\\d|-|\\*|@@|\\.).*\\s*[,;])|[,;])\\s*!\\s*-"

    head_rows <- stringr::str_which(text_cleaned, regex_object_head)
    special_rows <- stringr::str_which(text_cleaned, regex_object_special)
    field_rows <- stringr::str_which(text_cleaned, regex_field)
    # }}}2

    valid_lines <- sort(c(macro_rows, head_rows, special_rows, field_rows))

    if (!lines) {
        invalid <- setdiff(seq_along(text_cleaned), valid_lines)
        if (length(invalid) == 0L) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    } else {
        return(text[invalid])
    }
}
# }}}1

#' @importFrom purrr map_lgl
#' @importFrom stringr str_detect
# is_imf
# {{{1
is_imf <- function (idf_lines) {
    macro_dict <-
        c("##include", "##fileprefix", "##includesilent", "##nosilent", # Incorporating external files
          "##if", "##ifdef", "##ifndef", "##elseif", "##else", "##endif", # Selective accepting or skipping lins
          "##def", "##enddef", "##def1", "##set1", # Defining blocks of input
          "#eval", "#\\[", # Arithmetic operations
          "##list", "##nolist", "##show", "##noshow", "##showdetail", "##noshowdetail", # Marco debugging and listing
          "##expandcomment", "##traceback", "##notraceback", "##write", "##nowrite", # Marco debugging and listing
          "##symboltable", "##clear", "##reverse", "##!") # Marco debugging and listing

    is_imf <- any(purrr::map_lgl(macro_dict, ~any(stringr::str_detect(idf_lines, paste0("^", .x)))))

    return(is_imf)
}
# }}}1
