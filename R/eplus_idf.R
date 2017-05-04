################################################################################
#                       Parse EnergyPlus IDF/IMF File                          #
################################################################################

# read_idf
# {{{1
read_idf <- function(idf_path) {
    idf <- read_idf_raw(idf_path)
    idf <- get_idf_object(idf)
    return(idf)
}
# }}}1

# find_object
# {{{1
find_object <- function (idf, obj_ptn, ignore_case = TRUE, perl = TRUE, invert = FALSE) {
    ori_names <- names(idf)
    names(idf) <- make.unique(names(idf), sep = "_")
    objs <- grep(x = names(idf), pattern = obj_ptn, ignore_case, perl = perl, invert = invert)
    if (length(objs) == 0) {
        stop("Could not find any matched objects.", call. = FALSE)
    } else {
        results <- idf[c(objs)]
    }
    names(idf) <- ori_names
    return(results)
}
# }}}1

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
        map(unique(names(idf)),
            function (name) {
                upper_name <- str_to_upper(name)
                index <- grep(x = names(idf), pattern = paste0("^", name, "$")) %>% .[[1]]
                data.table(name = upper_name, index = index)
            }) %>% rbindlist(.)

    idf_lines <- map(seq_along(idf),
                     function (i) {
                         lines <- create_idf_object_lines(idf[i])
                         if (!is.na(match(i, obj_header_info[["index"]]))) {
                             class <- obj_header_info[index == i, name]
                             obj_header_str <- paste0( "!-   ===========  ALL OBJECTS IN CLASS: ", class, " ===========")
                             lines <- c(obj_header_str, lines)
                         }
                         return(lines)
          }) %>% flatten_chr()
    idf <- c(header, idf_lines)
    write_lines(idf, path = path)
}
# }}}1

######################
#  helper functions  #
######################

# read_idf_raw
# {{{1
read_idf_raw <- function (idf_path) {
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
    regex_object <- "^([A-Z][A-Za-z].*),$"
    object_names <- find_field(idf, regex_object)
    object_names <- replace_field(object_names, regex_object, "\\1")
    return(object_names)
}
# }}}1

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

# get_idf_object_fields
# {{{1
# get_idf_object_fields <- function (object, object_start, object_end) {
get_idf_object_fields <- function (object) {
    # object <- idf[object_start:object_end]
    object_field <- str_split(object, pattern = "[,;]\\s*!\\s*-\\s*", simplify = TRUE)
    value <- object_field[, 1]
    field_unit <- object_field[, 2]
    field <- str_replace_all(field_unit, pattern = "(.*)\\s\\{.+\\}", replacement = "\\1")
    unit <- str_extract_all(field_unit, pattern = "\\{.+\\}")
    unit <- str_replace_all(unit, pattern = "[\\{\\}]", replacement = "")
    unit <- ifelse(unit == "character(0)", NA_character_, unit)
    object <- data.table(value = value, field = field, unit = unit)
    return(object)
}
# }}}1

# get_idf_object
# {{{1
get_idf_object <- function (idf) {
    object_ranges <- get_idf_object_range(idf)

    objects <- map2(object_ranges$object_start_row,
                    object_ranges$object_end_row,
                    function (object_start, object_end) {
                        object <- idf[object_start:object_end]
                        object_fields <- get_idf_object_fields(object)
                        return(object_fields)
                    }) %>% set_names(object_ranges$object_name)
    return(objects)
}
# }}}1

# create_idf_object_lines
# {{{1
create_idf_object_lines <- function (object) {
    object_name <- names(object)
    object <- object[[1]]
    value <- object[["value"]]
    field <- object[["field"]]
    unit <- object[["unit"]]

    lines <-
        map_chr(seq(1:nrow(object)),
                function (i) {
                    sep <- if_else(i != nrow(object), ",", ";")
                    value <- str_pad(paste0("    ", value[i], sep), 29, side = "right")
                    field <- field[i]
                    unit <- unit[i]
                    if (is.na(unit)) {
                        line <- paste0(value, "!- ", field)
                    } else {
                        line <- paste0(value, "!- ", field, "{", unit, "}")
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
