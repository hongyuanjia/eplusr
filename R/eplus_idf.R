# read_idf
# {{{1
read_idf <- function (idf_path) {
    idf <- read_lines(idf_path) %>% iconv(to = "UTF-8")
    regex_blank_line <- "^\\s*$"
    regex_comment_line <- "^\\s*!.*$"

    # Get rid of blank lines
    idf <- find_field(idf, regex_blank_line, invert = TRUE)
    # Get rid of comment lines
    idf <- find_field(idf, regex_comment_line, invert = TRUE)
    # Get rid of leading and trailing spaces
    idf <- str_trim(idf, side = "both")

    return(idf)
}
# }}}1

regex_object <- "^([A-Z][A-Za-z].*),$"
# get_idf_object_name
# {{{1
get_idf_object_name <- function(idf) {
    object_names <- find_field(idf, regex_object)
    object_names <- replace_field(object_names, regex_object, "\\1")
    return(object_names)
}
# }}}1

# get_idf_object_range
# {{{1
get_idf_object_range <- function(idf){
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

# get_idd_object
# {{{1
get_idf_object <- function (idf) {
    object_ranges <- get_idf_object_range(idf)

    objects <- map2(object_ranges$object_start_row,
                    object_ranges$object_end_row,
                    function (object_start, object_end) {
                        object <- idf[object_start:object_end]
                        # object_attrs <- get_idf_object_attr_str(object)
                        # object_fields <- get_idf_object_field_str(object)
                        # object <- list(contents = object_fields,
                        #                attrs = object_attrs)
                        return(object)
                    }) %>% set_names(object_ranges$object_name)

    # attrs <- map(objects, "attrs") %>% map(get_idf_object_attrs)
    # idf_object <- map(objects, "contents") %>% map2(., attrs, add_attrs)
    #
    # return(idf_object)
    return(objects)
}
# }}}1

regex_field <- "(.*\\s*)[,;]\\s*!\\s*-\\s*(.*)$"
# get_idd_field_value
# {{{1
get_idf_field_value <- function (idf_object) {
    object_contents <- flatten_chr(idf_object)
    field_value <- replace_field(object_contents, regex_field, "\\1")
    field_name_unit <- replace_field(object_contents, regex_field, "\\2")
    field_name <- replace_field(field_name_unit, "(.*)\\s\\{(.*)\\}", "\\1")
    field_unit <- str_extract_all(field_name_unit, "\\{(.*)\\}$")
}
# }}}1
