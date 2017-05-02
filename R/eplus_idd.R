################################################################################
#                          Parse EnergyPlus IDD File                           #
################################################################################
# parse_idd: A function that takes the idd path as input and returns a list that
#            contains all the information of input idd with the structure below:
#            - Groups
#                  - Objects
#                        - Field
#                        - Field attributes
#                  - Object attributes
#            - Attributes
#                  - IDD version
#                  - IDD build
#            This process will take several minutes (6 mins in my computer). It
#            may be a good choice to store the result list as an external file.
# {{{1
parse_idd <- function (idd_path) {
    # For idd
    idd <- read_idd(idd_path)
    idd_attrs <- attributes(idd)

    # For groups
    idd_groups <- add_attrs(get_idd_group(idd), idd_attrs)

    # For fields
    idd_parsed <-
        purrr::map(seq_along(idd_groups),
                   function (i) {
                       group <- idd_groups[i]
                       idd_objects <- get_idd_object(group)
                       objects <- purrr::map(seq_along(idd_objects),
                                             function(j) {
                                                 object <- idd_objects[j]
                                                 fields <- get_idd_field(object)
                                                 return(fields)
                                             }) %>% purrr::set_names(names(idd_objects))
                       return(objects)
                   }) %>% purrr::set_names(names(idd_groups))

    return(idd_parsed)
}
# }}}1

# Helper functions
# {{{1
find_field <- function (x, regex, value = TRUE, invert = FALSE) {
    str <- grep(x = x, pattern = regex, value = value, invert = invert,
                perl = TRUE, ignore.case = FALSE)
    return(str)
}

replace_field <- function (x, regex, replacement, value = TRUE) {
    str <- gsub(x = x, pattern = regex, replacement = replacement,
                perl = TRUE, ignore.case = FALSE)
    return(str)
}

check_list <- function (x) {
    if (!is.list(x)) {
        stop("Input is not a list.")
    }
}

add_attrs <- function (x, attrs) {
    names <- names(attrs)
    expr <- purrr::map(seq_along(attrs),
                       ~paste0("attr(x, '", names[.x], "') <- attrs[[", .x, "]]\n")) %>%
            purrr::flatten_chr

    eval(parse(text = expr))

    return(x)
}
# }}}1

# For idd
# {{{1
get_idd_version <- function(idd) {
    regex_version_line <- "!IDD_Version (\\d\\.\\d\\.\\d)"
    point <- find_field(idd, regex_version_line, value = FALSE)
    idd_version <- replace_field(idd[point], regex_version_line, "\\1")
    return(idd_version)
}

get_idd_build <- function (idd) {
    regex_build_line <- "!IDD_BUILD ([0-9a-z]{10})"
    point <- find_field(idd, regex_build_line, value = FALSE)
    idd_build <- replace_field(idd[point], regex_build_line, "\\1")
    return(idd_build)
}

read_idd <- function(idd) {
    idd <- readr::read_lines(idd)
    idd <- iconv(idd, from = "UTF-8", to = "UTF-8", sub = "")

    version <- get_idd_version(idd)
    build <- get_idd_build(idd)

    regex_blank_line <- "^\\s*$"
    regex_comment_line <- "^\\s*!.*$"

    # Get rid of blank lines
    idd <- find_field(idd, regex_blank_line, invert = TRUE)
    # Get rid of comment lines
    idd <- find_field(idd, regex_comment_line, invert = TRUE)
    # Get rid of leading and trailing spaces
    idd <- str_trim(idd, side = "both")

    attr(idd, "version") <- version
    attr(idd, "build") <- build
    return(idd)
}
# }}}1

# For groups
# {{{1
regex_group <- "\\\\group (.*)$"

get_idd_group_name <- function (idd) {
    group_name <- find_field(idd, regex_group)
    group_name <- replace_field(group_name, regex_group, "\\1")
    return(group_name)
}

get_idd_group_range <- function (idd){
    group_name <- get_idd_group_name(idd)

    point_group_start <-
        `+`(find_field(idd, regex_group, value = F), 1)
    point_group_end <-
        find_field(idd, regex_group, value = F) %>% .[-1] %>%
        `-`(., 1) %>% c(., length(idd))

    idd_group_range <-
        data.table::data.table(group_name = group_name,
                               group_start_row = point_group_start,
                               group_end_row = point_group_end)

    return(idd_group_range)
}

get_idd_group <- function (idd) {
    idd_group_range <- get_idd_group_range(idd)
    idd_group <- purrr::map2(idd_group_range$group_start_row,
                             idd_group_range$group_end_row,
                             function (group_start, group_end) {
                                 group <- idd[group_start:group_end]
                                 return(group)
                             }) %>% purrr::set_names(idd_group_range$group_name)

    return(idd_group)
}
# }}}1

# For objects
# {{{1
regex_object <- "^([A-Z][A-Za-z].*),$"

# get_idd_object_name
# {{{2
get_idd_object_name <- function(idd_group) {
    check_list(idd_group)

    group_contents <- flatten_chr(idd_group)

    object_names <- find_field(group_contents, regex_object)
    object_names <- replace_field(object_names, regex_object, "\\1")
    return(object_names)
}
# }}}2

# get_idd_object_range
# {{{2
get_idd_object_range <- function(idd_group){
    check_list(idd_group)

    object_names <- get_idd_object_name(idd_group)

    group_contents <- flatten_chr(idd_group)

    # If the group only contains one object
    if (length(object_names) == 1) {
        point_object_start <-
            `+`(find_field(group_contents, regex_object, value = F), 1)
        point_object_end <- length(group_contents)
    } else {
        point_object_start <-
            `+`(find_field(group_contents, regex_object, value = F), 1)
        point_object_end <-
            find_field(group_contents, regex_object, value = F) %>%
            .[-1] %>% `-`(., 1) %>% c(., length(group_contents))
    }

    idd_object_range <-
        data.table::data.table(object_name = object_names,
                               object_start_row = point_object_start,
                               object_end_row = point_object_end)

    return(idd_object_range)
}
# }}}2

# get_idd_object_attr_str
# {{{2
get_idd_object_attr_str <- function (idd_object) {
    object_contents <- idd_object

    point_object_attrs_start <- 1L
    point_object_attrs_end <-
        `-`(purrr::detect_index(object_contents, ~grepl(x = .x, regex_field)), 1)
    object_attrs <- object_contents[point_object_attrs_start:point_object_attrs_end]

    return(object_attrs)
}
# }}}2

# get_idd_object_field_str
# {{{2
get_idd_object_field_str <- function (idd_object) {
    object_contents <- idd_object

    point_object_fields_start <-
        purrr::detect_index(object_contents, ~grepl(x = .x, regex_field))
    point_object_fields_end <- length(object_contents)
    object_fields <- object_contents[point_object_fields_start:point_object_fields_end]
    return(object_fields)
}
# }}}2

# get_idd_object_memo_attr
# {{{2
get_idd_object_memo_attr <- function (object_attrs) {
    regex <- "^\\\\memo "
    result <- find_field(object_attrs, regex)
    if (length(result) != 0) {
        result <- paste(replace_field(result, regex, ""), collapse = " ")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_object_unique_object_attr
# {{{2
get_idd_object_unique_object_attr <- function (object_attrs) {
    regex <- "^\\\\unique-object"
    result <- find_field(object_attrs, regex)
    if (length(result) != 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}2

# get_idd_object_required_object_attr
# {{{2
get_idd_object_required_object_attr <- function (object_attrs) {
    regex <- "^\\\\required-object"
    result <- find_field(object_attrs, regex)
    if (length(result) != 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}2

# get_idd_object_min_fields_attr
# {{{2
get_idd_object_min_fields_attr <- function (object_attrs) {
    regex <- "^\\\\min-fields"
    result <- find_field(object_attrs, regex)
    if (length(result) != 0) {
        result <- as.numeric(paste(replace_field(result, regex, ""), collapse = " "))
        return(result)
    } else {
        return(0)
    }
}
# }}}2

# get_idd_object_obsolete_attr
# {{{2
get_idd_object_obsolete_attr <- function (object_attrs) {
    regex <- "^\\\\obsolete"
    result <- find_field(object_attrs, regex)
    if (length(result) != 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}2

# get_idd_object_extensible_attr
# {{{2
get_idd_object_extensible_attr <- function (object_attrs) {
    regex <- "^\\\\extensible:(\\d{1})"
    result <- find_field(object_attrs, regex)
    if (length(result) != 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}2

# get_idd_object_begin_extensible_attr
# {{{2
get_idd_object_begin_extensible_attr <- function (object_attrs) {
    regex <- "^\\\\begin-extensible"
    result <- find_field(object_attrs, regex)
    if (length(result) != 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}2

# get_idd_object_format_attr
# {{{2
get_idd_object_format_attr <- function (object_attrs) {
    regex <- "^\\\\format "
    result <- find_field(object_attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_object_reference_class_name_attr
# {{{2
get_idd_object_reference_class_name_attr <- function (object_attrs) {
    regex <- "^\\\\reference-class-name "
    result <- find_field(object_attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_object_attrs
# {{{2
get_idd_object_attrs <- function (object_attrs_str) {
    attrs <-
        list("memo" = get_idd_object_memo_attr(object_attrs_str),
             "unique-object" = get_idd_object_unique_object_attr(object_attrs_str),
             "required-object" = get_idd_object_required_object_attr(object_attrs_str),
             "min-fields" = get_idd_object_min_fields_attr(object_attrs_str),
             "obsolete" = get_idd_object_obsolete_attr(object_attrs_str),
             "extensible" = get_idd_object_extensible_attr(object_attrs_str),
             "begin-extensible" = get_idd_object_begin_extensible_attr(object_attrs_str),
             "format" = get_idd_object_format_attr(object_attrs_str),
             "reference-class-name" = get_idd_object_reference_class_name_attr(object_attrs_str))
    return(attrs)
}
# }}}2

# get_idd_object
# {{{2
get_idd_object <- function (idd_group) {
    check_list(idd_group)

    object_ranges <- get_idd_object_range(idd_group)

    group_contents <- purrr::flatten_chr(idd_group)

    objects <- purrr::map2(object_ranges$object_start_row,
                           object_ranges$object_end_row,
                           function (object_start, object_end) {
                               object <- group_contents[object_start:object_end]
                               object_attrs <- get_idd_object_attr_str(object)
                               object_fields <- get_idd_object_field_str(object)
                               object <- list(contents = object_fields,
                                              attrs = object_attrs)
                               return(object)
                           }) %>% purrr::set_names(object_ranges$object_name)

    attrs <- purrr::map(objects, "attrs") %>% purrr::map(get_idd_object_attrs)
    idd_object <- purrr::map(objects, "contents") %>% purrr::map2(., attrs, add_attrs)

    return(idd_object)
}
# }}}2
# }}}1

# For fields
# {{{1
regex_field <- "((?:[AN][0-9]+\\s*[,;]\\s*)+)(\\\\(?:field|note)\\s(.*))$"

# get_idd_field_name
# {{{2
get_idd_field_name <- function(idd_object) {
    check_list(idd_object)

    object_contents <- purrr::flatten_chr(idd_object)

    field_name <- find_field(object_contents, regex_field)
    field_name <- replace_field(field_name, regex_field, "\\1")
    field_name <- replace_field(field_name, "\\s*", "")
    field_name <- replace_field(field_name, "[,;]$", "")

    return(field_name)
}
# }}}2

# get_idd_field_range
# {{{2
get_idd_field_range <- function(idd_object){
    check_list(idd_object)

    object_contents <- purrr::flatten_chr(idd_object)

    field_name <- get_idd_field_name(idd_object)

    if (length(field_name) == 1) {
        point_field_start <-
            find_field(object_contents, regex_field, value = F)
        point_field_end <- length(object_contents)
    } else {
        point_field_start <-
            find_field(object_contents, regex_field, value = F)
        point_field_end <-
            find_field(object_contents, regex_field, value = F) %>%
            .[-1] %>% `-`(., 1) %>% c(., length(object_contents))
    }

    field_ranges <-
        purrr::map(seq_along(field_name),
            function (i) {
                A_N_occurances <- length(flatten_chr(stringr::str_extract_all(field_name[i], "[AN]")))
                # If a line with multiple fields
                if (A_N_occurances > 1) {
                    field_name <- purrr::flatten_chr(stringr::str_split(field_name[i], "[,;]"))
                    point_field_start = rep(point_field_start[i], length(field_name))
                    point_field_end = rep(point_field_end[i], length(field_name))
                } else {
                    field_name = field_name[i]
                    point_field_start = point_field_start[i]
                    point_field_end = point_field_end[i]
                }
                field_ranges <-
                    data.table::data.table(field_name = field_name,
                                           field_start_row = point_field_start,
                                           field_end_row = point_field_end)
                return(field_ranges)
            }) %>% data.table::rbindlist

    return(field_ranges)
}
# }}}2

# get_idd_field_attr_str
# {{{2
get_idd_field_attr_str <- function (idd_field) {
    field_contents <- idd_field

    field_attrs <- replace_field(field_contents, regex_field, "\\2")

    return(field_attrs)
}
# }}}2

# get_idd_field_attrs
# {{{2
get_idd_field_attrs <- function (attrs) {
    attrs <-
        list("field" = get_idd_field_field_attr(attrs),
             "note" = get_idd_field_note_attr(attrs),
             "required-field" = get_idd_field_required_field_attr(attrs),
             "begin-extensible" = get_idd_field_begin_extensible_attr(attrs),
             "units" = get_idd_field_units_attr(attrs),
             "ip-units" = get_idd_field_ip_units_attr(attrs),
             "unitsBasedOnField" = get_idd_field_unitsBasedOnField_attr(attrs),
             "minimum" = get_idd_field_minimum_attr(attrs),
             "maximum" = get_idd_field_maximum_attr(attrs),
             "minimum_larger" = get_idd_field_minimum_larger_attr(attrs),
             "maximum_smaller" = get_idd_field_maximum_smaller_attr(attrs),
             "default" = get_idd_field_default_attr(attrs),
             "deprecated" = get_idd_field_deprecated_attr(attrs),
             "autosizable" = get_idd_field_autosizable_attr(attrs),
             "autocalculatable" = get_idd_field_autocalculatable_attr(attrs),
             "type" = get_idd_field_type_attr(attrs),
             "retaincase" = get_idd_field_retaincase_attr(attrs),
             "key" = get_idd_field_key_attr(attrs),
             "object-list" = get_idd_field_object_list_attr(attrs),
             "external-list" = get_idd_field_external_list_attr(attrs),
             "reference" = get_idd_field_reference_attr(attrs))
    return(attrs)
}
# }}}2

# get_idd_field_field_attr
# {{{2
get_idd_field_field_attr <- function (attrs) {
    regex <- "^\\\\field "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field_note_attr
# {{{2
get_idd_field_note_attr <- function (attrs) {
    regex <- "^\\\\note "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- paste(replace_field(result, regex, ""), collapse = " ")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field_required_field_attr
# {{{2
get_idd_field_required_field_attr <- function (attrs) {
    regex <- "^\\\\required-field"
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}2

# get_idd_field_begin_extensible_attr
# {{{2
get_idd_field_begin_extensible_attr <- function (attrs) {
    regex <- "^\\\\begin-extensible"
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}2

# get_idd_field_units_attr
# {{{2
get_idd_field_units_attr <- function (attrs) {
    regex <- "^\\\\units "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field_ip_units_attr
# {{{2
get_idd_field_ip_units_attr <- function (attrs) {
    regex <- "^\\\\ip-units "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field_unitsBasedOnField_attr
# {{{2
get_idd_field_unitsBasedOnField_attr <- function (attrs) {
    regex <- "^\\\\unitsBasedOnField "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field_minimum_attr
# {{{2
get_idd_field_minimum_attr <- function (attrs) {
    regex <- "^\\\\minimum "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field_maximum_attr
# {{{2
get_idd_field_maximum_attr <- function (attrs) {
    regex <- "^\\\\maximum "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field_minimum_larger_attr
# {{{2
get_idd_field_minimum_larger_attr <- function (attrs) {
    regex <- "^\\\\minimum> "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field_maximum_smaller_attr
# {{{2
get_idd_field_maximum_smaller_attr <- function (attrs) {
    regex <- "^\\\\maximum< "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field_default_attr
# {{{2
get_idd_field_default_attr <- function (attrs) {
    regex <- "^\\\\default "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field_deprecated_attr
# {{{2
get_idd_field_deprecated_attr <- function (attrs) {
    regex <- "^\\\\deprecated"
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}2

# get_idd_field_autosizable_attr
# {{{2
get_idd_field_autosizable_attr <- function (attrs) {
    regex <- "^\\\\autosizable"
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}2

# get_idd_field_autocalculatable_attr
# {{{2
get_idd_field_autocalculatable_attr <- function (attrs) {
    regex <- "^\\\\autocalculatable"
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}2

# get_idd_field_type_attr
# {{{2
get_idd_field_type_attr <- function (attrs) {
    regex <- "^\\\\type "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field_retaincase_attr
# {{{2
get_idd_field_retaincase_attr <- function (attrs) {
    regex <- "^\\\\retaincase"
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# }}}2

# get_idd_field_key_attr
# {{{2
get_idd_field_key_attr <- function (attrs) {
    regex <- "^\\\\key "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- c(replace_field(result, regex, ""))
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field_object_list_attr
# {{{2
get_idd_field_object_list_attr <- function (attrs) {
    regex <- "^\\\\object-list "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field_external_list_attr
# {{{2
get_idd_field_external_list_attr <- function (attrs) {
    regex <- "^\\\\external-list "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field_reference_attr
# {{{2
get_idd_field_reference_attr <- function (attrs) {
    regex <- "^\\\\reference "
    result <- find_field(attrs, regex)
    if (length(result) != 0) {
        result <- replace_field(result, regex, "")
        return(result)
    } else {
        return(NULL)
    }
}
# }}}2

# get_idd_field
# {{{2
get_idd_field <- function (idd_object) {
    check_list(idd_object)

    field_ranges <- get_idd_field_range(idd_object)

    object_contents <- purrr::flatten_chr(idd_object)

    fields <- purrr::map(1:nrow(field_ranges),
                         function (i) {
                             field_name <- field_ranges$field_name[i]
                             field_start <- field_ranges$field_start_row[i]
                             field_end <- field_ranges$field_end_row[i]
                             field <- object_contents[field_start:field_end]
                             field_attrs <- get_idd_field_attr_str(field)
                             field <- list(contents = field_name,
                                           attrs = field_attrs)
                             return(field)
                         }) %>% purrr::set_names(field_ranges$field_name)

    attrs <- purrr::map(fields, "attrs") %>% purrr::map(get_idd_field_attrs)
    idd_field <- purrr::map(fields, "contents") %>% purrr::flatten_chr %>% purrr::map2(., attrs, add_attrs)
    return(idd_field)
}
# }}}2

# }}}1
