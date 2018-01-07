#' @import data.table
#' @importFrom progress progress_bar
NULL

#' Parse Energy+.idd file
#'
#' @param path Path to 'Energy+.idd' file
#'
#' @details The IDD files for EnergyPlus 8.5 to 8.8 have been pre-parsed and
#' stored internally and will automatically be used when parsing \code{IDF} and
#' \code{IMF} files. The parsing process was basically the same as that was
#' implemented in IDFEditor distributed with EnergyPlus, but using the powerful
#' \code{data.table} package to speed up the whole process and store the
#' results. The souce codes of IDFEditor can be found on
#' \href{GitHub}{https://github.com/NREL/EnergyPlus/tree/develop/src/IDF_Editor}:
#' . However, it will still take about 3-4 sec to parse an IDD file which is
#' much slower than IDFEditor written in Visual Basic.
#'
#' @return A list contains the IDD version, build, parsed class data and parsed
#' field data. Both class data and field data are stored in data.tables.
parse_idd <- function(path) {

    assertthat::assert_that(is_readable(path))

    # set progress bar
    pb <- progress::progress_bar$new(
           format = "  Parsing IDD (:what) [:bar] :percent in :elapsed",
           total = 100, clear = FALSE)

    # show progress bar
    pb$tick(0)

    pb$update(0.1, tokens = list(what = "Initialize"))
    # read idd string, get idd version and build
    idd_str <- read_idd(path)
    idd_version <- get_idd_ver(idd_str)
    idd_build <- get_idd_build(idd_str)

    idd_dt <- data.table(line = seq_along(idd_str), string = idd_str, key = "line")

    pb$update(0.2, tokens = list(what = "Parsing "))
    # Delete all comment and blank lines
    line_comment <- idd_dt[startsWith(string, "!"), line]
    line_blank <- idd_dt[nchar(string) == 0L, line]
    idd_dt[!(line %in% c(line_comment, line_blank))]
    idd_dt <- idd_dt[-c(line_comment, line_blank)]
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
    idd_dt[type == type_unknown & endsWith(string, ","), type := type_class]
    # categorize field lines
    idd_dt[grepl("\\", string, fixed = TRUE) & grepl("^[AaNn]", string), type := type_field]
    # ignore section if exists, e.g. "Simulation Data;"
    line_section <- idd_dt[type == type_unknown][endsWith(string, ";"), which = TRUE]
    if (not_empty(line_section)) {
        idd_dt <- idd_dt[-line_section]
    }
    # if there are still known lines, report an error
    line_error_invalid <- idd_dt[type == type_unknown, which = TRUE]
    if (not_empty(line_error_invalid)) {
        parse_issue(path, type = "Invalid line found", src = "IDD", stop = TRUE,
                    data_errors = idd_dt[line_error_invalid, list(line, string)])
    }
    # }}}

    pb$update(0.3, tokens = list(what = "Parsing "))
    # get class names
    idd_dt[type == type_class, class := substr(string, 1L, nchar(string) - 1L)]

    pb$update(0.4, tokens = list(what = "Parsing "))
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
    idd_dt[type %in% c(type_field, type_field_last),
           field_count := char_count(field_anid, "[,;]")]
    idd_dt <- idd_dt[type %in% c(type_field, type_field_last),
        strsplit(field_anid, "\\s*[,;]\\s*"), by = list(line)][
        idd_dt, on = "line"][field_count == 1L, V1 := field_anid][, field_anid := NULL]
    setnames(idd_dt, "V1", "field_anid")
    # get row numeber of last field per condensed field line in each class
    idd_dt[, row_id := .I]
    line_field_last <- idd_dt[field_count> 1L][type == type_field_last,
        row_id[.N], by = list(line, type)]$V1
    # set all type of condensed field lines to "field"
    idd_dt[field_count > 1L, type := type_field]
    idd_dt[line_field_last, type := type_field_last]
    idd_dt[, row_id := NULL]
    # }}}

    # seperate file AN and id {{{
    idd_dt[field_count == 1L,
        field_anid := trimws(substr(field_anid, 1L, nchar(field_anid) - 1L), "right")]
    idd_dt[field_count >= 1L,
           `:=`(field_an = substr(field_anid, 1L, 1L),
                field_id = substr(field_anid, 2L, nchar(field_anid)))]
    # }}}

    # get slash keys and values {{{
    idd_dt[type == type_slash, slash_key_value := string]
    # Remove slash
    idd_dt[!is.na(slash_key_value),
        slash_key_value := trimws(
            substr(slash_key_value, 2L, nchar(slash_key_value)), which = "left")]

    # handle informal field slash keys
    # {{{
    # have to handle some informal slash keys such as '\minimum >0' which should
    # be '\minimum> 0', and '\maximum <100' which should be `\maximum< 100`.
    line_bad_min_exclu <- idd_dt[!is.na(slash_key_value)][
        startsWith(slash_key_value, "minimum >"), line]
    if (not_empty(line_bad_min_exclu)) {
        idd_dt[line %in% line_bad_min_exclu,
        slash_key_value := paste0(
            "minimum> ",
            substr(slash_key_value, 10L, nchar(slash_key_value))
            )
        ]
    }

    line_bad_max_exclu <- idd_dt[!is.na(slash_key_value)][
        startsWith(slash_key_value, "maximum <"), line]
    if (not_empty(line_bad_max_exclu)) {
        idd_dt[line %in% line_bad_max_exclu,
        slash_key_value := paste0(
            "maximum< ",
            substr(slash_key_value, 10L, nchar(slash_key_value))
            )
        ]
    }
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
    idd_dt[, `:=`(space_loc = NULL)]
    # }}}
    # }}}

    pb$update(0.5, tokens = list(what = "Parsing "))
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
    ignored_slash_key <- c("BEGIN-EXTENSIBLE", "RETAINCASE", "EXTENSIBLE", "OBSOLETE")
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
    line_error_slash_key <- idd_dt[slash_supported == FALSE, which = TRUE]
    if (length(line_error_slash_key) > 0L) {
        parse_issue(path, type = "Invalid slash key found.", src = "IDD", stop = TRUE,
                    data_errors = idd_dt[line_error_slash_key, list(line, string)])
    }
    # check for unsupported slash values
    idd_dt[, slash_value_upper := toupper(slash_value)]
    # \type {{{
    line_error_type <- idd_dt[slash_key == "TYPE"][
       !(slash_value_upper %chin% c("REAL", "INTEGER", "ALPHA", "CHOICE",
            "OBJECT-LIST", "EXTERNAL-LIST", "NODE")), which = TRUE]
    if (length(line_error_type) > 0) {
        parse_issue(path, "Invalid \\type found", idd_dt[line_error_type, list(line, string)])
    }
    # }}}
    # \external-List {{{
    line_error_external_list <- idd_dt[slash_key %chin% "EXTERNAL-LIST"][
           !(slash_value_upper %chin% c("AUTORDDVARIABLE", "AUTORDDMETER",
               "AUTORDDVARIABLEMETER")), which = TRUE]
    if (length(line_error_type) > 0) {
        parse_issue(path, "Invalid \\type found", idd_dt[line_error_type, list(line, string)])
    }
    # }}}
    # \format {{{
    line_error_format <- idd_dt[slash_key %chin% "FORMAT"][
        !(slash_value_upper %chin% c("SINGLELINE", "VERTICES", "COMPACTSCHEDULE",
            "FLUIDPROPERTY", "VIEWFACTOR", "SPECTRAL")), which = TRUE]
    if (length(line_error_format) > 0) {
        parse_issue(path, "Invalid \\format found", idd_dt[line_error_format, list(line, string)])
    }
    # }}}
    # }}}

    # fix duplicated values
    # {{{
    # fix duplicated fields in "ZoneHVAC:HighTemperatureRadiant" and
    # "Foundation:Kiva"
    # NOTE: there are errors in "ZoneHVAC:HighTemperatureRadiant" that
    # duplicated fields ANid are used for 'N76', 'N77', 'N87' and
    # "Foundation:Kiva" for "N16", have to fix it in advanced.
    # {{{
    # fill class downwards to make search easiser
    dup_field_anid <- idd_dt[type %in% c(type_class, type_class_slash, type_field,
        type_field_last, type_field_slash)][
        , class := class[1L], by = list(cumsum(!is.na(class)))][
        type == type_field_slash][!is.na(field_anid), list(line, class, field_anid)]

    line_dup <- dup_field_anid[
        duplicated(dup_field_anid, by = c("class", "field_anid")), line]
    # add a suffix of 'd' to the duplicated field
    if (not_empty(line_dup)) {
        idd_dt[line %in% line_dup,
            `:=`(field_anid = paste0(field_anid, "_dup"),
                 field_id = paste0(field_id, "_dup"))]
    }
    # }}}

    # fix duplicated class slash lines such as "\min-fields 3" in such as
    # "SurfaceProperty:HeatTransferAlgorithm:SurfaceList"
    # {{{
    dup_class_slash <- idd_dt[type %in% c(type_class, type_class_slash)][
        , class := class[1L], by = list(cumsum(!is.na(class)))][
        type == type_class_slash, list(line, class, slash_key_value)]
    line_dup <- dup_class_slash[
        duplicated(dup_class_slash, by = c("class", "slash_key_value")), line]
    # remove duplicated class slash lines
    if (not_empty(line_dup)) {
        idd_dt <- idd_dt[!(line %in% line_dup)]
    }
    # }}}

    # fix duplicated field slash lines such as "\unit m" in such as
    # "HVACTemplate:Zone:WaterToAirHeatPump"
    # {{{
    dup_field_slash <- idd_dt[
        type %in% c(type_class, type_field, type_field_last, type_field_slash),
        list(type, line, class, field_anid, slash_key, slash_key_value)][
        , class := class[1L], by = list(cumsum(!is.na(class)))][
        slash_key != "NOTE"][, field_anid := field_anid[1L],
        by = list(class, cumsum(!is.na(field_anid)))][
        type == type_field_slash]

    line_dup <- dup_field_slash[
        duplicated(dup_field_slash, by = c("class", "field_anid", "slash_key_value")),
        line]
    # remove duplicated field slash lines
    if (not_empty(line_dup)) {
        idd_dt <- idd_dt[!(line %in% line_dup)]
    }
    # }}}
    # }}}
    # clean
    idd_dt[, `:=`(slash_key_value = NULL, field_anid = NULL)]

    pb$update(0.6, tokens = list(what = "Parsing "))
    idd_dt[, row_id := .I]
    # FIELD data
    # {{{
    # extract class data
    idd_field <- idd_dt[type %in% c(type_class, type_field, type_field_last, type_field_slash),
        .SD, .SDcol = c("row_id", "class", "field_an", "field_id", "slash_key", "slash_value")]
    # rolling fill downwards for class and field AN and id
    idd_field[, class := class[1L], by = list(cumsum(!is.na(class)))]
    idd_field[, field_an := field_an[1L], by = list(cumsum(!is.na(field_an)), class)]
    idd_field[, field_id := field_id[1L], by = list(cumsum(!is.na(field_id)), class)]
    # combine field AN and id again for easing distinguishing fields
    idd_field[, field_anid := paste0(field_an, field_id)]
    # As the first line of each class is a class name which has been filled,
    # delete it
    idd_field <- idd_field[!is.na(slash_key)]
    # if slash key exists and slash value not, it must be a logical attribute,
    # such as "\\unique-object". Set it to TRUE
    idd_field <- idd_field[is.na(slash_value), slash_value := "TRUE"]
    # order class as the sequence the appears in IDD
    idd_field[, class_order := .GRP, by = list(class)]
    # using dcast to cast all field attributes into seperated columns
    # get line of field AN and id
    idd_field_line <- idd_field[, list(class_order, field_anid, row_id)]
    idd_field_line <- idd_field_line[
        idd_field_line[, .I[1L], by = list(class_order, field_anid)]$V1]
    # dcast
    idd_field <- dcast.data.table(idd_field,
        class_order + class + field_anid + field_an + field_id ~ slash_key,
        value.var = "slash_value",
        fun.aggregate = list(function(x) paste0(x, collapse = " ")), fill = NA)
    # merge line into dcasted table
    idd_field <- merge(idd_field, idd_field_line,
        by = c("class_order", "field_anid"), all.x = TRUE, sort = FALSE)
    # set order according to line
    setorder(idd_field, row_id)
    # delete line column
    idd_field[, row_id := NULL]
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
                     required_field = as.logical(required_field),
                     unitsbasedonfield = as.logical(unitsbasedonfield))]
    # fill na
    idd_field[is.na(autocalculatable), autocalculatable := FALSE]
    idd_field[is.na(autosizable), autosizable := FALSE]
    idd_field[is.na(required_field), required_field := FALSE]
    idd_field[is.na(unitsbasedonfield), unitsbasedonfield := FALSE]

    # order fields per class
    idd_field[, field_order := seq_along(field_anid), by = list(class)]
    setorder(idd_field, class_order, field_order)
    # }}}

    pb$update(0.8, tokens = list(what = "Parsing "))
    # CLASS data
    # {{{
    # extract class data
    idd_class <- idd_dt[between(type, type_group, type_class_slash),
        .SD, .SDcol = c("group", "class", "slash_key", "slash_value")]
    # rolling fill downwards for group and class
    idd_class[, group := group[1L], by = list(cumsum(!is.na(group)))]
    idd_class[, class := class[1L], by = list(cumsum(!is.na(class)))]
    # As group has been add into a seperated column named "group"and also the
    # last class in one group has been mis-categorized into the next group by
    # the filing process, delete group slash_key
    idd_class <- idd_class[!(slash_key %chin% "GROUP")]
    # as the first na in first group can not be replaced using downward filling
    idd_class <- idd_class[!is.na(class)]
    # if slash key exists and slash value not, it must be a logical attribute,
    # such as "\\unique-object". Set it to TRUE
    idd_class <- idd_class[!is.na(slash_key)][
        is.na(slash_value), slash_value := "TRUE"]
    # order group and class as the sequence the appears in IDD
    idd_class[, group_order := .GRP, by = list(group)]
    idd_class[, class_order := .GRP, by = list(class)]
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
    idd_class <- idd_field[, list(max_fields = .N), by = class][idd_class, on = "class"]
    neworder <- c("group_order", "group", "class_order", "class", "format",
         "min_fields", "max_fields", "required_object", "unique_object")
    setcolorder(idd_class, c(neworder, setdiff(names(idd_class), neworder)))
    setorder(idd_class, group_order, class_order)
    # }}}

    pb$update(0.85, tokens = list(what = "Parsing "))
    # Object-List reference data {{{
    # collect \object-list data
    idd_object_list <- NULL
    # if '\object-list' exists
    if (has_name(idd_field, "object_list")) {
        idd_object_list <- idd_field[!is.na(object_list),
            list(class_order, field_order, object_list)][
            , strsplit(object_list, " ", fixed = TRUE),
            by = list(class_order, field_order)]
        setnames(idd_object_list, "V1", "ref_key")
    }

    # collect \reference-class-name data
    idd_class_reference <- NULL
    # if '\reference-class-name' exists
    if (has_name(idd_class, "reference_class_name")) {
        idd_class_reference <- idd_class[!is.na(reference_class_name),
            list(class_order, reference_class_name)][
            , strsplit(reference_class_name, " ", fixed = TRUE), by = list(class_order)]
        setnames(idd_class_reference, "V1", "ref_key")
        setcolorder(idd_class_reference, c("ref_key", "class_order"))
    }

    # collect \reference data
    idd_field_reference <- NULL
    # if '\reference' exists
    if (has_name(idd_field, "reference")) {
        idd_field_reference <- idd_field[!is.na(reference),
            list(class_order, field_order, reference)][
            , strsplit(reference, " ", fixed = TRUE), by = list(class_order, field_order)]
        setnames(idd_field_reference, c("class_order", "field_order", "ref_key"))
        setcolorder(idd_field_reference, c("ref_key", "class_order", "field_order"))
    }
    idd_ref_object <- list(key = idd_object_list,
                           class = idd_class_reference,
                           field = idd_field_reference)
    # }}}

    pb$update(0.90, tokens = list(what = "Parsing "))
    # External-List reference data {{{
    idd_ref_external <- NULL
    # if '\external-list' exists
    if (has_name(idd_field, "external_list")) {
        idd_ref_external <- idd_field[!is.na(external_list),
            list(class_order, field_order, external_list)]
        setnames(idd_ref_external, "external_list", "ref_key")
        setcolorder(idd_ref_external, c("ref_key", "class_order", "field_order"))
    }
    # }}}

    # set class
    setattr(idd_class, "class", c("IDD_Class", class(idd_class)))
    setattr(idd_field, "class", c("IDD_Field", class(idd_field)))
    if (!is.null(idd_ref_object)) {
        setattr(idd_ref_object, "class", c("IDD_Ref_Obj", class(idd_ref_object)))
    }
    if (!is.null(idd_ref_external)) {
        setattr(idd_ref_external, "class", c("IDD_Ref_Ext", class(idd_ref_external)))
    }

    pb$update(0.95, tokens = list(what = "Parsing "))
    idd <- list(version = idd_version,
                build = idd_build,
                class = idd_class,
                field = idd_field,
                ref_object = idd_ref_object,
                ref_external = idd_ref_external)
    # set class to IDD
    setattr(idd, "class", c("IDD", class(idd)))
    pb$tick(100L, tokens = list(what = "Complete"))
    return(idd)
}

################################################################################
#                                   helpers                                    #
################################################################################
# read_idd {{{1
read_idd <- function(filepath) {
    con = file(filepath)
    idd_str <- readLines(con, warn = FALSE)
    close(con)

    # Get rid of leading and trailing spaces
    idd_str <- trimws(idd_str, which = "both")

    return(idd_str)
}
# }}}1

# parse_issue {{{
parse_issue <- function (path, type = "", data_errors = NULL, info = NULL, src = c("IDD", "IDF"),
                        stop = TRUE, quote = TRUE) {
    if (!is.null(info)) {
        sep <- paste0(sep_line("-"), "\n")
        info <- paste0(info, "\n")
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
            error_truncated <- "**Only first 10 errors are shown below**\n"
        }
    }

    max_c <- max(sapply(data_errors[["line"]], nchar))
    if (quote) {
        data_str <- paste0(sprintf(paste0("Line %", max_c, "s: %s"), data_errors[["line"]],
                           sapply(data_errors[["string"]], sQuote)), collapse = "\n")
    } else {
        data_str <- paste0(sprintf(paste0("Line %", max_c, "s: %s"), data_errors[["line"]],
                           data_errors[["string"]]), collapse = "\n")
    }
    mes <- paste0("\n",
        sep_line("="), "\n",
        sprintf("%s PARSING ERROR for file %s", src, sQuote(path)), "\n",
        paste0(key_line, ": ", type), "\n",
        paste0("[Total Number]: ", error_num), "\n",
        error_truncated,
        sep_line("-"), "\n",
        data_str, "\n",
        sep,
        info,
        sep_line("="),
        collapse = "\n")
    mes <- gsub("\n\n", "\n", mes)

    if (stop) {
        stop(mes, call. = FALSE)
    } else {
        ori <- getOption("warning.length")
        options(warning.length = 8000)
        on.exit(options(warning.length = ori))
        warning(mes, call. = FALSE)
    }
}
# }}}
# get_idd_ver {{{
get_idd_ver <- function (idd_str) {
    ver_line <- idd_str[grepl("!IDD_Version", idd_str, fixed = TRUE)]

    if (length(ver_line) == 1L) {
        ver <- substr(ver_line, 14L, nchar(ver_line))
        return(ver)
    } else if (length(ver_line > 1L)) {
        "!DEBUG Multiple version found in IDD file."
        return(NULL)
    } else {
        "!DEBUG No version found in IDD file."
        return(NULL)
    }
}
# }}}
# get_idd_build {{{
get_idd_build <- function (idd_str) {
    build_line <- idd_str[grepl("!IDD_BUILD", idd_str, fixed = TRUE)]

    if (length(build_line) == 1L) {
        build <- substr(build_line, 12L, nchar(build_line))
        return(build)
    } else if (length(build_line > 1L)) {
        "!DEBUG Multiple build found in IDD file."
        return(NULL)
    } else {
        "!DEBUG No build found in IDD file."
        return(NULL)
    }
}
# }}}
