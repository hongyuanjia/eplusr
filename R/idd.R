################################################################################
#                          Parse EnergyPlus IDD File                           #
################################################################################

#' Parse Energy+.idd file
#'
#' @param filepath Path to 'Energy+.idd' file
#'
#' @details The parsing process was basically the same as that was implemented
#' in IDFEditor distributed with EnergyPlus, but using the powerful 'data.table'
#' package to speed up the whole process and store the results.  The souce codes
#' of IDFEditor can be found as below:
#' https://github.com/NREL/EnergyPlus/tree/develop/src/IDF_Editor
#'
#' @return A list contains the IDD version, build, parsed class data and parsed
#' field data. Both class data and field data are stored in data.tables
#' @export

parse_idd <- function(filepath) {

    # set progress bar
    pb <- progress::progress_bar$new(
           format = "  Parsing IDD (:what) [:bar] :percent in :elapsed",
           total = 100, clear = FALSE)

    # show progress bar
    pb$tick(0)

    pb$update(0.1, tokens = list(what = "Initialize"))
    # read idd string, get idd version and build
    idd_str <- read_idd(filepath)
    idd_dt <- data.table(line = seq_along(idd_str), string = idd_str, key = "line")
    idd_version <- idd_dt[grepl("!IDD_Version", string), substr(string, 14L, nchar(string))]
    idd_build <- idd_dt[grepl("!IDD_BUILD", string), substr(string, 12L, nchar(string))]

    pb$update(0.2, tokens = list(what = "Parsing "))
    # mark type{{{
    # -2, unknown
    type_unknown <- -2L
    # -1, clash
    type_slash <- -1L
    # 0 , group
    type_group <- 0L
    # 1 , class
    type_class <- 1L
    # 2 , class slash
    type_class_slash <- 2L
    # 3 , field
    type_field <- 3L
    # 4 , last field in class
    type_field_last <- 4L
    # 5 , field slash
    type_field_slash <- 5L
    idd_dt[, type := type_unknown]
    setkey(idd_dt, line, type)
    # Delete all comment and blank lines
    idd_dt <- idd_dt[grep("^!|(^$)", string, invert = TRUE)]
    # trucate to characters left of ! in order to handle cases when there are
    # inline comments starting with "!", e.g.
    # "GrouhdHeatTransfer:Basement:EquivSlab,  ! Supplies ..."
    idd_dt[, explpt_loc := regexpr("!", string, fixed = TRUE)]
    idd_dt[explpt_loc > 1, string := trimws(substr(string, 1L, explpt_loc - 1L), which = "right")]
    idd_dt[, explpt_loc := NULL]
    # categorize all slash lines
    idd_dt[startsWith(string, "\\"), type := type_slash]
    # categorize all lines with trailing comma into class. Lines that
    # have one slash can not be a class
    idd_dt[type < -1L & endsWith(string, ","), type := type_class]
    # categorize field lines
    idd_dt[grepl("\\", string, fixed = TRUE) & grepl("^[AaNn]", string), type := type_field]
    # ignore section if exists, e.g. "Simulation Data;"
    line_section <- idd_dt[type == -2L & endsWith(string, ";"), which = TRUE]
    if (length(line_section) > 0L) {
        idd_dt <- idd_dt[-line_section]
    }
    # if there are still known lines, report an error
    line_error_invalid <- idd_dt[type < -1L, which = TRUE]
    if (length(line_error_invalid) > 0L) {
        parse_issue(type = "Invalid line found", src = "IDD", stop = TRUE,
                    data_errors = idd_dt[line_error_invalid, .(line, string)])
    }
    # }}}

    pb$update(0.3, tokens = list(what = "Parsing "))
    # basic {{{
    # get class names
    idd_dt[type == type_class, class := substr(string, 1L, nchar(string) - 1L)]

    # get field AN and id
    # {{{
    # get location of first slash
    idd_dt[, slash_loc := regexpr("\\", string, fixed = TRUE)]
    # get combined field AN and id
    idd_dt[type == type_field,
           `:=`(field_anid = trimws(substr(string, 1L, slash_loc - 1L), which = "right"),
                slash_key_value = substr(string, slash_loc, nchar(string)))]
    idd_dt[endsWith(field_anid, ";"), `:=`(type = type_field_last)]
    # clean
    idd_dt[, slash_loc := NULL]

    # handle condensed fields
    # {{{
    idd_dt[, field_count := 0L]
    # TODO: find a better way to count occurrences in string without using
    # "stringr" package
    idd_dt[between(type, type_field, type_field_last), field_count := stringr::str_count(field_anid, "[,;]")]

    idd_dt <- idd_dt[
        between(type, type_field, type_field_last), strsplit(field_anid, "\\s*[,;]\\s*"), by = .(line)][
        idd_dt, on = "line"][field_count == 1L, V1 := field_anid][, field_anid := NULL]
    setnames(idd_dt, "V1", "field_anid")
    # get row numeber of last field per condensed field line in each class
    line_field_last <- idd_dt[
        field_count > 1L & type == type_field_last, .(line_field_last = last(.I)), by = .(line, type)][
        , line_field_last]
    # set all type of condensed field lines to "field"
    idd_dt[field_count > 1L, type := type_field]
    idd_dt[line_field_last, type := type_field_last]
    # }}}

    # fix duplicated fields in "ZoneHVAC:HighTemperatureRadiant" and
    # "Foundation:Kiva"
    # NOTE: there are errors in "ZoneHVAC:HighTemperatureRadiant" that
    # duplicated fields ANid are used for 'N76', 'N77', 'N87' and
    # "Foundation:Kiva" for "N16", have to fix it in advanced.
    # {{{
    # fill class downwards to make search easiser
    idd_dt_dup <- idd_dt[between(type, type_class, type_field)][
        , class := class[1], by = .(cumsum(!is.na(class)))][
        type == type_field, .(line, class, field_anid)]
    # found duplicated field in the whole data.table
    line_dup <- idd_dt_dup[,
        .SD[duplicated(field_anid)], .SDcol = "line",  by = .(class)][, line]
    # add a suffix of 'd' to the duplicated field
    if (length(line_dup) > 0L) {
        idd_dt[line %in% line_dup, `:=`(field_anid = gsub("([,;])$", "_dup\\1", field_anid))]
    }
    # }}}

    # seperate file AN and id {{{
    idd_dt[field_count == 1L, field_anid := trimws(substr(field_anid, 1L, nchar(field_anid) - 1L), "right")]
    idd_dt[field_count >= 1L,
           `:=`(field_an = substr(field_anid, 1L, 1L),
                field_id = substr(field_anid, 2L, nchar(field_anid)))]
    # }}}

    # get slash keys and values {{{
    idd_dt[type == type_slash, slash_key_value := string]
    # Remove slash
    idd_dt[!is.na(slash_key_value), slash_key_value := trimws(substr(slash_key_value, 2L, nchar(slash_key_value)), which = "left")]

    # handle informal slash keys
    # {{{
    # have to handle some informal slash keys such as '\minimum >0' which should
    # be '\minimum> 0', and '\maximum <100' which should be `\maximum< 100`.
    idd_dt[!is.na(slash_key_value) & grepl("^minimum\\s+>", slash_key_value, ignore.case = TRUE),
           slash_key_value := gsub("\\s+>", "> ", slash_key_value)]
    idd_dt[!is.na(slash_key_value) & grepl("^maximum\\s+<", slash_key_value, ignore.case = TRUE),
           slash_key_value := gsub("\\s+<", "< ", slash_key_value)]
    # }}}

    # seperate slash key and value
    idd_dt[, space_loc := regexpr(" ", slash_key_value, fixed = TRUE)]
    # handle cases like "\extensible:2"
    idd_dt[, colon_loc := regexpr(":", slash_key_value, fixed = TRUE)]
    idd_dt[(colon_loc > 0L & space_loc > 0L & colon_loc < space_loc) |
           (space_loc == -1L & colon_loc > 0L), space_loc := colon_loc]
    idd_dt[, colon_loc := NULL]

    idd_dt[space_loc > 0L,
           `:=`(slash_key = toupper(substr(slash_key_value, 1L, space_loc - 1L)),
                slash_value = trimws(substr(slash_key_value, space_loc + 1L, nchar(slash_key_value)), "left"))]
    idd_dt[space_loc < 0L, `:=`(slash_key = toupper(slash_key_value))]
    # remove trailing tabs in slash keys
    idd_dt[grepl("\t", slash_key), slash_key := gsub(slash_key, "\t", "")]

    # clean up
    idd_dt[, `:=`(space_loc = NULL, slash_key_value = NULL, field_anid = NULL)]
    # }}}
    # }}}

    pb$tick(10L, tokens = list(what = "Parsing "))
    # parse slash lines {{{
    idd_dt[!is.na(slash_key), slash_supported := FALSE]
    group_slash_key <- c("GROUP")
    class_slash_key <- c("MEMO", "UNIQUE-OBJECT", "REQUIRED-OBJECT",
                         "MIN-FIELDS", "FORMAT", "REFERENCE-CLASS-NAME")
    field_slash_key <- c("FIELD", "NOTE", "REQUIRED-FIELD", "UNITS", "IP-UNITS",
                         "UNITSBASEDONFIELD", "MINIMUM", "MINIMUM>", "MAXIMUM",
                         "MAXIMUM<", "DEFAULT", "DEPRECATED", "AUTOSIZABLE",
                         "AUTOCALCULATABLE", "TYPE", "KEY", "OBJECT-LIST",
                         "EXTERNAL-LIST", "REFERENCE")
    ignored_slash_key <- c("BEGIN-EXTENSIBLE", "RETAINCASE", "EXTENSIBLE")
    # mark group slash key and values
    idd_dt[slash_key %chin% group_slash_key,
           `:=`(slash_supported = TRUE, type = type_group, group = slash_value)]
    # mark class slash key and values
    idd_dt[slash_key %chin% class_slash_key,
           `:=`(slash_supported = TRUE, type = type_class_slash)]
    # mark field slash key and values
    idd_dt[slash_key %chin% field_slash_key,
           `:=`(slash_supported = TRUE, type = type_field_slash)]
    # mark other ignored slash keys
    idd_dt[slash_key %chin% ignored_slash_key, slash_supported := TRUE]
    # check for unsupported slash keys
    line_error_slash_key <- idd_dt[slash_supported == FALSE, .I]
    if (length(line_error_slash_key) > 0L) {
        parse_issue(type = "Invalid slash key found.", src = "IDD", stop = TRUE,
                    data_errors = idd_dt[line_error_slash_key, .(line, string)])
    }
    # check for unsupported slash values
    idd_dt[, slash_value_upper := toupper(slash_value)]
    # \type {{{
    line_error_type <- idd_dt[slash_key == "TYPE"][
       !(slash_value_upper %chin% c("REAL", "INTEGER", "ALPHA", "CHOICE",
            "OBJECT-LIST", "EXTERNAL-LIST", "NODE")), which = TRUE]
    if (length(line_error_type) > 0) {
        parse_issue("Invalid \\type found", idd_dt[line_error_type, .(line, string)])
    }
    # }}}
    # \external-List {{{
    line_error_external_list <- idd_dt[slash_key %chin% "EXTERNAL-LIST"][
           !(slash_value_upper %chin% c("AUTORDDVARIABLE", "AUTORDDMETER",
               "AUTORDDVARIABLEMETER")), which = TRUE]
    if (length(line_error_type) > 0) {
        parse_issue("Invalid \\type found", idd_dt[line_error_type, .(line, string)])
    }
    # }}}
    # \format {{{
    line_error_format <- idd_dt[slash_key %chin% "FORMAT"][
        !(slash_value_upper %chin% c("SINGLELINE", "VERTICES", "COMPACTSCHEDULE",
            "FLUIDPROPERTY", "VIEWFACTOR", "SPECTRAL")), which = TRUE]
    if (length(line_error_format) > 0) {
        parse_issue("Invalid \\format found", idd_dt[line_error_format, .(line, string)])
    }
    # }}}
    # }}}
    # }}}

    pb$update(0.6, tokens = list(what = "Parsing "))
    # FIELD data
    # {{{
    # extract class data
    idd_field <- idd_dt[type == type_class | between(type, type_field, type_field_slash),
        .SD, .SDcol = c("line", "class", "field_an", "field_id", "slash_key", "slash_value")]
    # rolling fill downwards for class and field AN and id
    idd_field[, class := class[1], by = .(cumsum(!is.na(class)))]
    idd_field[, field_an := field_an[1], by = .(cumsum(!is.na(field_an)), class)]
    idd_field[, field_id := field_id[1], by = .(cumsum(!is.na(field_id)), class)]
    # combine field AN and id again for easing distinguishing fields
    idd_field[, field_anid := paste0(field_an, field_id)]
    # As the first line of each class is a class name which has been filled,
    # delete it
    idd_field <- idd_field[!is.na(slash_key)]
    # if slash key exists and slash value not, it must be a logical attribute,
    # such as "\\unique-object". Set it to TRUE
    idd_field <- idd_field[is.na(slash_value), slash_value := "TRUE"]
    # order class as the sequence the appears in IDD
    idd_field[, class_order := .GRP, by = .(class)]
    # using dcast to cast all field attributes into seperated columns
    # get line of field AN and id
    idd_field_line <- idd_field[, .(class_order, field_anid, line)]
    idd_field_line <- idd_field_line[
        idd_field_line[, .I[1], by = .(class_order, field_anid)]$V1]
    # dcast
    idd_field <- dcast.data.table(idd_field,
        class_order + class + field_anid + field_an + field_id ~ slash_key,
        value.var = "slash_value",
        fun.aggregate = list(function(x) paste0(x, collapse = " ")), fill = NA)
    # merge line into dcasted table
    idd_field <- merge(idd_field, idd_field_line,
        by = c("class_order", "field_anid"), all.x = TRUE, sort = FALSE)
    # set order according to line
    setorder(idd_field, line)
    # delete line column
    idd_field[, line := NULL]
    # set names
    new_nms <- gsub("-", "_", tolower(names(idd_field)), fixed = TRUE)
    setnames(idd_field, new_nms)
    # set column type
    idd_field[, `:=`(autocalculatable = as.logical(autocalculatable),
                     autosizable= as.logical(autosizable),
                     maximum = as.double(maximum),
                     minimum = as.double(minimum),
                     `maximum<` = as.double(`maximum<`),
                     `minimum>` = as.double(`minimum>`),
                     unitsbasedonfield = as.logical(unitsbasedonfield))]
    # fill na
    idd_field[is.na(autocalculatable), autocalculatable := FALSE]
    idd_field[is.na(autosizable), autosizable := FALSE]
    idd_field[is.na(unitsbasedonfield), unitsbasedonfield := FALSE]

    # order fields per class
    idd_field[, field_order := seq_along(field_anid), by = .(class)]
    setorder(idd_field, class_order, field_order)
    # }}}

    pb$update(0.8, tokens = list(what = "Parsing "))
    # CLASS data
    # {{{
    # extract class data
    idd_class <- idd_dt[between(type, type_group, type_class_slash),
        .SD, .SDcol = c("group", "class", "slash_key", "slash_value")]
    # rolling fill downwards for group and class
    idd_class[, group := group[1], by = .(cumsum(!is.na(group)))]
    idd_class[, class := class[1], by = .(cumsum(!is.na(class)))]
    # As group has been add into a seperated column named "group"and also the
    # last class in one group has been mis-categorized into the next group by
    # the filing process, delete group slash_key
    idd_class <- idd_class[!(slash_key %chin% "GROUP")]
    # as the first na in first group can not be replaced using downward filling
    idd_class <- idd_class[!is.na(class)]
    # if slash key exists and slash value not, it must be a logical attribute,
    # such as "\\unique-object". Set it to TRUE
    idd_class <- idd_class[!is.na(slash_key)][is.na(slash_value), slash_value := "TRUE"]
    # order group and class as the sequence the appears in IDD
    idd_class[, group_order := .GRP, by = .(group)]
    idd_class[, class_order := .GRP, by = .(class)]
    # using dcast to cast all class attributes into seperated columns
    idd_class <- dcast.data.table(idd_class,
        group_order + group + class_order + class ~ slash_key,
        value.var = "slash_value",
        fun.aggregate = list(function(x) paste0(x, collapse = " ")), fill = NA)
    # set names
    new_nms <- gsub("-", "_", tolower(names(idd_class)), fixed = TRUE)
    setnames(idd_class, new_nms)
    # set column type
    idd_class[, `:=`(min_fields = as.integer(min_fields),
                     required_object = as.logical(required_object),
                     unique_object = as.logical(unique_object))]
    # fill na
    idd_class[is.na(format), format := "standard"]
    idd_class[is.na(min_fields), min_fields := 0L]
    idd_class[is.na(required_object), required_object := FALSE]
    idd_class[is.na(unique_object), unique_object := FALSE]
    # get max field per class
    idd_class <- idd_field[, .(max_fields = .N), by = class][idd_class, on = "class"]
    neworder <- c("group_order", "group", "class_order", "class", "format",
         "min_fields", "max_fields", "required_object", "unique_object")
    setcolorder(idd_class, c(neworder, setdiff(names(idd_class), neworder)))
    # }}}

    pb$update(0.85, tokens = list(what = "Parsing "))
    # Object-List reference data {{{
    # collect \object-list data
    idd_object_list <- idd_field[!is.na(object_list),
        .(class_order, field_order, object_list)][
        , strsplit(object_list, " ", fixed = TRUE),
        by = .(class_order, field_order)]
    setnames(idd_object_list, "V1", "ref_key")

    # collect \reference-class-name data
    idd_class_reference <- idd_class[!is.na(reference_class_name),
        .(class_order, reference_class_name)][
        , strsplit(reference_class_name, " ", fixed = TRUE), by = .(class_order)]
    setnames(idd_class_reference, "V1", "ref_key")
    setcolorder(idd_class_reference, c("ref_key", "class_order"))

    # collect \reference data
    idd_field_reference <- idd_field[!is.na(reference),
        .(class_order, field_order, reference)][
        , strsplit(reference, " ", fixed = TRUE), by = .(class_order, field_order)]
    setnames(idd_field_reference, c("class_order", "field_order", "ref_key"))
    setcolorder(idd_field_reference, c("ref_key", "class_order", "field_order"))

    # idd_field_reference <- idd_field[!is.na(reference),
    #     .(class, field_order, reference)][
    #     , strsplit(reference, " ", fixed = TRUE), by = .(class, field_order)][
    #     , .(ref_class = c(.SD[, 1]), ref_field_order = c(.SD[, 2])), by = .(V1)]
    # setnames(idd_field_reference, c("ref_key", "ref_class", "ref_field_order"))

    # # combine \reference-class-name and \reference data
    # idd_reference <- rbindlist(
    #     list(idd_field_reference, idd_class_reference), fill = TRUE
    # )
    # # add an indicator column for easy subsetting
    # idd_reference[, num_ref_field := length(unlist(ref_field_order)), by = ref_key]

    # # collect \object-list data
    # idd_object_list <- idd_field[!is.na(object_list),
    #     .(class_order, class, field_order, field, object_list)][
    #     , strsplit(object_list, " ", fixed = TRUE),
    #     by = .(class_order, class, field_order, field)]
    # setnames(idd_object_list, "V1", "object_list")

    # # combine all data
    # idd_object_list_ref <- merge(idd_object_list, idd_reference,
    #     by.x = "object_list", by.y = "ref_key", all.x = TRUE, sort = FALSE)
    # # remove object-lists that are not referred by any class or field
    # idd_object_list_ref <- idd_object_list_ref[!is.na(num_ref_field)]
    # }}}

    pb$update(0.90, tokens = list(what = "Parsing "))
    # External-List reference data {{{
    idd_external_list <- idd_field[!is.na(external_list),
        .(class_order, field_order, external_list)]
    setnames(idd_external_list, "external_list", "ref_key")
    setcolorder(idd_external_list, c("ref_key", "class_order", "field_order"))
    # }}}

    pb$update(0.95, tokens = list(what = "Parsing "))
    idd <- list(version = idd_version,
                build = idd_build,
                class = idd_class,
                field = idd_field,
                ref_object = list(key = idd_object_list,
                                  class = idd_class_reference,
                                  field = idd_field_reference),
                ref_external = idd_external_list)
    # set class to IDD
    class(idd) <- c("IDD", class(idd))
    pb$tick(100L, tokens = list(what = "Complete"))
    return(idd)
}

################################################################################
#                                   helpers                                    #
################################################################################
# read_idd {{{1
read_idd <- function(filepath) {
    con = file(filepath, encoding = "UTF-8")
    idd_str <- readLines(con)
    close(con)

    # Get rid of leading and trailing spaces
    idd_str <- trimws(idd_str, which = "both")

    return(idd_str)
}
# }}}1
# parse_issue {{{
parse_issue <- function (type = "", data_errors = NULL, info = NULL, src = c("IDD", "IDF"),
                        stop = TRUE) {
    if (!is.null(info)) {
        sep <- paste0(rep("-", 60L), collapse = "")
    } else {
        sep <- NULL
    }

    src = match.arg(src)

    if (stop) {
        key_line <-  "[ Error Type ]"
    } else {
        key_line <-  "[Warning Type]"
    }

    error_num <- NULL
    error_truncated <- NULL
    if (!is.null(data_errors)) {
        num_row <- nrow(data_errors)
        error_num <- num_row
        # Only use the first 10 lines.
        if (num_row > 10L) {
            data_errors <- data_errors[1:10]
            error_truncated <- "**Only first 10 errors are shown below**"
        }
    }

    mes <- c(
         glue::glue("

                    ============================================================
                    {src} PARSING ERROR for file {sQuote(filepath)}
                    {key_line}: {type}
                    "),
         glue::glue("

                    [Total Number]: {error_num}
                    "),
         glue::glue("

                    {error_truncated}
                    "),
         glue::glue("

                    {sep}

                    "),
         if (!is.null(data_errors)) {
             glue::glue_data(data_errors,
                    "
                    Line {line}: {sQuote(string)}\n
                    ")
         },
         glue::glue("
                    {sep}

                    "),
         glue::glue(info),
         glue::glue("

                    ============================================================
                    "))

    if (stop) {
        stop(glue::glue("{mes}"), call. = FALSE)
    } else {
        ori <- getOption("warning.length")
        options(warning.length = 8000)
        on.exit(option(warning.length = ori))
        warning(glue::glue("{mes}"), call. = FALSE)
    }
}
# }}}
# conversion_units_record {{{
# SI names {{{
si_name <- character(length = 154)
si_name[1] = "m"
si_name[2] = "m"
si_name[3] = "W"
si_name[4] = "W"
si_name[5] = "m3/s"
si_name[6] = "m3/s"
si_name[7] = "C"
si_name[8] = "kg/J"
si_name[9] = "Pa"
si_name[10] = "Pa"
si_name[11] = "Pa"
si_name[12] = "Pa"
si_name[13] = "W/m-K"
si_name[14] = "W/K"
si_name[15] = "deltaC"
si_name[16] = "m2"
si_name[17] = "K"
si_name[18] = "(kg/s)/W"
si_name[19] = "J/kg"
si_name[20] = "kgWater/kgDryAir"
si_name[21] = "kJ/kg"
si_name[22] = "lux"
si_name[23] = "kg/m3"
si_name[24] = "kg/s"
si_name[25] = "kg/s-m"
si_name[26] = "m3"
si_name[27] = "m3"
si_name[28] = "W/m2-K"
si_name[29] = "1/m"
si_name[30] = "J/kg-K"
si_name[31] = "J/m3-K"
si_name[32] = "m/s"
si_name[33] = "m/s"
si_name[34] = "m2-K/W"
si_name[35] = "W/m2"
si_name[36] = "A/K"
si_name[37] = "g/kg"
si_name[38] = "g/m-s"
si_name[39] = "g/m-s-K"
si_name[40] = "J/K"
si_name[41] = "J/kg-K2"
si_name[42] = "J/m3"
si_name[43] = "kg/kg-K"
si_name[44] = "kPa"
si_name[45] = "kPa"
si_name[46] = "m2/s"
si_name[47] = "m3/kg"
si_name[48] = "m3/m3"
si_name[49] = "N-s/m2"
si_name[50] = "V/K"
si_name[51] = "W/m-K2"
si_name[52] = "m3/s-m"
si_name[53] = "deg"
si_name[54] = "hr"
si_name[55] = "A"
si_name[56] = "dimensionless"
si_name[57] = "V"
si_name[58] = "A/V"
si_name[59] = "eV"
si_name[60] = "percent"
si_name[61] = "percentage (as a real decimal)"
si_name[62] = "s"
si_name[63] = "W/m2 or deg C"
si_name[64] = "W/m2, W or deg C"
si_name[65] = "1/K"
si_name[66] = "J/m2-K"
si_name[67] = "ohms"
si_name[68] = "cycles/hr"
si_name[69] = "kg/kg"
si_name[70] = "J/J"
si_name[71] = "g/GJ"
si_name[72] = "L/GJ"
si_name[73] = "m3/GJ"
si_name[74] = "m3/s-m2"
si_name[75] = "m3/s-person"
si_name[76] = "W/m2-K2"
si_name[77] = "g/MJ"
si_name[78] = "L/MJ"
si_name[79] = "m3/MJ"
si_name[80] = "W/W"
si_name[81] = "$/m2"
si_name[82] = "$"
si_name[83] = "$/kW"
si_name[84] = "$/m3"
si_name[85] = "years"
si_name[86] = "$/(W/K)"
si_name[87] = "$/(m3/s)"
si_name[88] = "W/m"
si_name[89] = "minutes"
si_name[90] = "cm"
si_name[91] = "K/m"
si_name[92] = "W/s"
si_name[93] = "kmol"
si_name[94] = "J"
si_name[95] = "GJ"
si_name[96] = "days"
si_name[97] = "kg/m2"
si_name[98] = "kg"
si_name[99] = "kmol/s"
si_name[100] = "percent/K"
si_name[101] = "kg/s2"
si_name[102] = "g/mol"
si_name[103] = "deltaJ/kg"
si_name[104] = "person/m2"
si_name[105] = "m2/person"
si_name[106] = "W/person"
si_name[107] = "W/person"
si_name[108] = "W/m2"
si_name[109] = "m3/person"
si_name[110] = "m3/hr-person"
si_name[111] = "m3/m2"
si_name[112] = "m3/hr-m2"
si_name[113] = "m3/hr"
si_name[114] = "s/m"
si_name[115] = "W/m2"
si_name[116] = "m2/m"
si_name[117] = "L/day"
si_name[118] = "L/kWh"
si_name[119] = "kg/Pa-s-m2"
si_name[120] = "m/hr"
si_name[121] = "Mode"
si_name[122] = "Control"
si_name[123] = "Availability"
si_name[124] = "rev/min"
si_name[125] = "W/(m3/s)"
si_name[126] = "W/m-K"
si_name[127] = "VA"
si_name[128] = "N-m"
si_name[129] = "m3/s-W"
si_name[130] = "cm2"
si_name[131] = "kg/m"
si_name[132] = "Pa"
si_name[133] = "m/yr"
si_name[134] = "1/hr"
si_name[135] = "ppm"
si_name[136] = "W/m-K3"
si_name[137] = "kg/m-s"
si_name[138] = "kg/m-s-K"
si_name[139] = "kg/m-s-K2"
si_name[140] = "J/kg-K3"
si_name[141] = "ms"
si_name[142] = "Ah"
si_name[143] = "deltaC/hr"
si_name[144] = "micron"
si_name[145] = "W/(m3/s)"
si_name[146] = "W/((m3/s)-Pa)"
si_name[147] = "m3/s-m"
si_name[148] = "m3/s-W"
si_name[149] = "m3/person"
si_name[150] = "m3/hr-person"
si_name[151] = "m3/m2"
si_name[152] = "m3/hr-m2"
si_name[153] = "m3/hr"
si_name[154] = "W/((m3/s)-Pa)"
# }}}

# IP names {{{
ip_name <- character(length = 154)
ip_name[1] = "ft"
ip_name[2] = "in"
ip_name[3] = "Btu/h"
ip_name[4] = "W"
ip_name[5] = "ft3/min"
ip_name[6] = "gal/min"
ip_name[7] = "F"
ip_name[8] = "lb/Btu"
ip_name[9] = "psi"
ip_name[10] = "inHg"
ip_name[11] = "inH2O"
ip_name[12] = "ftH2O"
ip_name[13] = "Btu-in/h-ft2-F"
ip_name[14] = "Btu/h-F"
ip_name[15] = "deltaF"
ip_name[16] = "ft2"
ip_name[17] = "R"
ip_name[18] = "(lbm/sec)/(Btu/hr)"
ip_name[19] = "Btu/lb"
ip_name[20] = "lbWater/lbDryAir"
ip_name[21] = "Btu/lb"
ip_name[22] = "foot-candles"
ip_name[23] = "lb/ft3"
ip_name[24] = "lb/s"
ip_name[25] = "lb/s-ft"
ip_name[26] = "ft3"
ip_name[27] = "gal"
ip_name[28] = "Btu/h-ft2-F"
ip_name[29] = "1/ft"
ip_name[30] = "Btu/lb-F"
ip_name[31] = "Btu/ft3-F"
ip_name[32] = "ft/min"
ip_name[33] = "miles/hr"
ip_name[34] = "ft2-F-hr/Btu"
ip_name[35] = "Btu/h-ft2"
ip_name[36] = "A/F"
ip_name[37] = "grains/lb"
ip_name[38] = "lb/ft-s"
ip_name[39] = "lb/ft-s-F"
ip_name[40] = "Btu/F"
ip_name[41] = "Btu/lb-F2"
ip_name[42] = "Btu/ft3"
ip_name[43] = "lb/lb-F"
ip_name[44] = "psi"
ip_name[45] = "inHg"
ip_name[46] = "ft2/s"
ip_name[47] = "ft3/lb"
ip_name[48] = "ft3/ft3"
ip_name[49] = "lbf-s/ft2"
ip_name[50] = "V/F"
ip_name[51] = "Btu/h-F2-ft"
ip_name[52] = "ft3/min-ft"
ip_name[53] = "deg"
ip_name[54] = "hr"
ip_name[55] = "A"
ip_name[56] = "dimensionless"
ip_name[57] = "V"
ip_name[58] = "A/V"
ip_name[59] = "eV"
ip_name[60] = "percent"
ip_name[61] = "percentage (as a real decimal)"
ip_name[62] = "s"
ip_name[63] = "unknown"
ip_name[64] = "unknown"
ip_name[65] = "1/F"
ip_name[66] = "Btu/ft2-F"
ip_name[67] = "ohms"
ip_name[68] = "cycles/hr"
ip_name[69] = "lb/lb"
ip_name[70] = "Btu/Btu"
ip_name[71] = "lb/MWh"
ip_name[72] = "gal/kWh"
ip_name[73] = "ft3/MWh"
ip_name[74] = "ft3/min-ft2"
ip_name[75] = "ft3/min-person"
ip_name[76] = "Btu/h-ft2-F2"
ip_name[77] = "lb/MWh"
ip_name[78] = "gal/kWh"
ip_name[79] = "ft3/kWh"
ip_name[80] = "Btuh/Btuh"
ip_name[81] = "$/ft2"
ip_name[82] = "$"
ip_name[83] = "$/(kBtuh/h)"
ip_name[84] = "$/ft3"
ip_name[85] = "years"
ip_name[86] = "$/(Btu/h-F)"
ip_name[87] = "$/(ft3/min)"
ip_name[88] = "Btu/h-ft"
ip_name[89] = "minutes"
ip_name[90] = "in"
ip_name[91] = "F/ft"
ip_name[92] = "W/s"
ip_name[93] = "kmol"
ip_name[94] = "Wh"
ip_name[95] = "ton-hrs"
ip_name[96] = "days"
ip_name[97] = "lb/ft2"
ip_name[98] = "lb"
ip_name[99] = "kmol/s"
ip_name[100] = "percent/F"
ip_name[101] = "lb/s2"
ip_name[102] = "lb/mol"
ip_name[103] = "deltaBtu/lb"
ip_name[104] = "person/ft2"
ip_name[105] = "ft2/person"
ip_name[106] = "Btu/h-person"
ip_name[107] = "W/person"
ip_name[108] = "W/m2"
ip_name[109] = "ft3/person"
ip_name[110] = "ft3/hr-person"
ip_name[111] = "ft3/ft2"
ip_name[112] = "ft3/hr-ft2"
ip_name[113] = "ft3/hr"
ip_name[114] = "s/ft"
ip_name[115] = "W/ft2"
ip_name[116] = "ft2/ft"
ip_name[117] = "pint/day"
ip_name[118] = "pint/kWh"
ip_name[119] = "lb/psi-s-ft2"
ip_name[120] = "ft/hr"
ip_name[121] = "Mode"
ip_name[122] = "Control"
ip_name[123] = "Availability"
ip_name[124] = "rev/min"
ip_name[125] = "W/(ft3/min)"
ip_name[126] = "Btu/h-ft-F"
ip_name[127] = "VA"
ip_name[128] = "lbf-in"
ip_name[129] = "(ft3/min)/(Btu/h)"
ip_name[130] = "inch2"
ip_name[131] = "lb/ft"
ip_name[132] = "Pa"
ip_name[133] = "inch/yr"
ip_name[134] = "1/hr"
ip_name[135] = "ppm"
ip_name[136] = "Btu/h-F3-ft"
ip_name[137] = "kg/m-s"
ip_name[138] = "kg/m-s-F"
ip_name[139] = "kg/m-s-F2"
ip_name[140] = "J/kg-K3"
ip_name[141] = "ms"
ip_name[142] = "Ah"
ip_name[143] = "deltaF/hr"
ip_name[144] = "micron"
ip_name[145] = "W/(gal/min)"
ip_name[146] = "W/((gal/min)-ftH20)"
ip_name[147] = "gal/min-ft"
ip_name[148] = "(gal/min)/(Btu/h)"
ip_name[149] = "gal/person"
ip_name[150] = "gal/hr-person"
ip_name[151] = "gal/ft2"
ip_name[152] = "gal/hr-ft2"
ip_name[153] = "gal/hr"
ip_name[154] = "W/((ft3/min)-inH2O)"
# }}}

# mult {{{
mult <- double(length = 154)
mult[1] = 3.28083989501312
mult[2] = 39.3700787401575
mult[3] = 3.4121412858518
mult[4] = 1
mult[5] = 2118.88000328931
mult[6] = 15850.3222370511
mult[7] = 1.8
mult[8] = 2325.83774250441
mult[9] = 1.45037743897283E-04
mult[10] = 0.00029613
mult[11] = 0.00401463
mult[12] = 0.00033455
mult[13] = 6.93481276005548
mult[14] = 1.89563404769544
mult[15] = 1.8
mult[16] = 10.7639104167097
mult[17] = 1.8
mult[18] = 0.646078115385742
mult[19] = 0.00042986
mult[20] = 1
mult[21] = 0.429925
mult[22] = 0.092902267
mult[23] = 0.062428
mult[24] = 2.20462247603796
mult[25] = 0.67196893069637
mult[26] = 35.3146667214886
mult[27] = 264.172037284185
mult[28] = 0.176110194261872
mult[29] = 0.3048
mult[30] = 2.39005736137667E-04
mult[31] = 1.49237004739337E-05
mult[32] = 196.850393700787
mult[33] = 2.2369362920544
mult[34] = 5.678263
mult[35] = 0.316957210776545
mult[36] = 0.555555555555556
mult[37] = 7
mult[38] = 0.000671968949659
mult[39] = 3.73574867724868E-04
mult[40] = 526.565
mult[41] = 1.32889924714692E-04
mult[42] = 2.68096514745308E-05
mult[43] = 0.555555555555556
mult[44] = 0.145038
mult[45] = 0.29523
mult[46] = 10.7639104167097
mult[47] = 16.018
mult[48] = 1
mult[49] = 2.08857913669065E-02
mult[50] = 0.555555555555556
mult[51] = 0.321418310071648
mult[52] = 645.89
mult[53] = 1
mult[54] = 1
mult[55] = 1
mult[56] = 1
mult[57] = 1
mult[58] = 1
mult[59] = 1
mult[60] = 1
mult[61] = 1
mult[62] = 1
mult[63] = 1
mult[64] = 1
mult[65] = 0.555555555555556
mult[66] = 4.89224766847393E-05
mult[67] = 1
mult[68] = 1
mult[69] = 1
mult[70] = 1
mult[71] = 7.93664091373665E-03
mult[72] = 9.51022349025202E-04
mult[73] = 127.13292
mult[74] = 196.85
mult[75] = 2118.6438
mult[76] = 0.097826
mult[77] = 7.93664091373665
mult[78] = 0.951022349025202
mult[79] = 127.13292
mult[80] = 1
mult[81] = 9.28939733269818E-02
mult[82] = 1
mult[83] = 0.293083235638921
mult[84] = 2.83127014102352E-02
mult[85] = 1
mult[86] = 0.52667614683731
mult[87] = 4.72000059660808E-04
mult[88] = 1.04072
mult[89] = 1
mult[90] = 0.3937
mult[91] = 0.54861322767449
mult[92] = 1
mult[93] = 1
mult[94] = 2.77777777777778E-04
mult[95] = 78.9889415481832
mult[96] = 1
mult[97] = 0.204794053596664
mult[98] = 2.2046
mult[99] = 1
mult[100] = 0.555555555555556
mult[101] = 2.2046
mult[102] = 0.0022046
mult[103] = 0.0004299
mult[104] = 9.28939733269818E-02
mult[105] = 10.764961
mult[106] = 3.4121412858518
mult[107] = 1
mult[108] = 1
mult[109] = 35.3146667214886
mult[110] = 35.3146667214886
mult[111] = 3.28083989501312
mult[112] = 3.28083989501312
mult[113] = 35.3146667214886
mult[114] = 0.3048
mult[115] = 0.09290304
mult[116] = 3.28083989501312
mult[117] = 2.11337629827348
mult[118] = 2.11337629827348
mult[119] = 1412.00523459398
mult[120] = 3.28083989501312
mult[121] = 1
mult[122] = 1
mult[123] = 1
mult[124] = 1
mult[125] = 0.0004719475
mult[126] = 0.577796066000163
mult[127] = 1
mult[128] = 8.85074900525547
mult[129] = 621.099127332943
mult[130] = 0.15500031000062
mult[131] = 0.67196893069637
mult[132] = 1
mult[133] = 39.3700787401575
mult[134] = 1
mult[135] = 1
mult[136] = 0.178565727817582
mult[137] = 0.67196893069637
mult[138] = 0.373316072609094
mult[139] = 0.207397818116164
mult[140] = 7.38277359526066E-05
mult[141] = 1
mult[142] = 1
mult[143] = 1.8
mult[144] = 1
mult[145] = 0.0000630902
mult[146] = 0.188582274697355
mult[147] = 4831.17821785317
mult[148] = 4645.27137336702
mult[149] = 264.172037284185
mult[150] = 264.172037284185
mult[151] = 24.5423853466941
mult[152] = 24.5423853466941
mult[153] = 264.172037284185
mult[154] = 0.117556910599482
# }}}

# offset {{{
offset <- double(length = 154)
offset[7] = 32
offset[19] = 7.686
# }}}

# alt {{{
alt <- logical(length = 154)
alt[2] = TRUE
alt[4] = TRUE
alt[6] = TRUE
alt[10] = TRUE
alt[11] = TRUE
alt[12] = TRUE
alt[45] = TRUE
alt[107] = TRUE
alt[108] = TRUE
alt[126] = TRUE
alt[132] = TRUE
alt[145] = TRUE
alt[147] = TRUE
alt[148] = TRUE
alt[149] = TRUE
alt[150] = TRUE
alt[151] = TRUE
alt[152] = TRUE
alt[153] = TRUE
alt[154] = TRUE
# }}}

# multi_unit_name {{{
multi_unit_name <- character(length = 154)
multi_unit_name[1] = "Distance"
multi_unit_name[3] = "Capacity"
multi_unit_name[4] = "Power"
multi_unit_name[5] = "VolumetricFlow"
multi_unit_name[7] = "Temperature"
multi_unit_name[9] = "Pressure"
multi_unit_name[13] = "Conductivity"
multi_unit_name[15] = "DeltaTemperature"
multi_unit_name[19] = "Enthalpy"
multi_unit_name[23] = "Density"
multi_unit_name[24] = "MassFlow"
multi_unit_name[28] = "ConvectionCoefficient"
multi_unit_name[30] = "SpecificHeat"
multi_unit_name[32] = "Velocity"
multi_unit_name[49] = "Viscosity"
multi_unit_name[53] = "Angle"
multi_unit_name[56] = "Dimensionless"
multi_unit_name[60] = "Percent"
multi_unit_name[94] = "Energy"
multi_unit_name[107] = "ActivityLevel"
multi_unit_name[120] = "PrecipitationRate"
multi_unit_name[121] = "Mode"
multi_unit_name[122] = "Control"
multi_unit_name[123] = "Availability"
multi_unit_name[129] = "VolumetricFlowPerPower"
multi_unit_name[144] = "Wavelength"
# }}}

conversion_units_record <- data.table(si_name, ip_name, mult, offset, alt, multi_unit_name)
# }}}
