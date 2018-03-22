#' Parse EnergyPlus IDD files
#'
#' \code{eplusr} provides parsing of and programmatic access to EnergyPlus
#' Input Data Dictionary (IDD) files, and objects. It contains all data needed
#' to parse EnergyPlus models. IDD objects provide parsing and printing
#'
#' @details The IDD objects for EnergyPlus 8.5 to 8.8 have been pre-parsed and
#' stored internally and will automatically be used when parsing \code{IDF} and
#' \code{IMF} files. Internally, the powerful \code{data.table} package is used
#' to speed up the whole process and store the results. However, it will still
#' take about 5-6 sec to parse an IDD file.
#'
#' Normally, you may not need to parse any Energy+.idd file unless your model
#' is produced by EnergyPlus whose version is lower than 8.5. If so, it is
#' suggested to store the parsed IDD object and directly pass it to the
#' \code{idd} argument in \code{eplusr_model$new} in order to avoid the parsing
#' process whenever you read a model of that version.
#'
#' @section Usage:
#' ```
#'
#' idd <- IDD$new(path)
#'
#' idd$version()
#' idd$build()
#' idd$class_name(group = NULL)
#' idd$group_name(class = NULL)
#' idd$group_order(group = NULL)
#' idd$class_order(class = NULL)
#' idd$orders()
#'
#' idd$object(class)
#' idd$objects(class = NULL)
#' idd$objects_in_group(group)
#'
#' idd$is_valid_class(class)
#' idd$is_valid_group(group)
#'
#' idd$print()
#'
#' print(idd)
#'
#' ```
#'
#' @section Arguments:
#'
#' * `path`: Path to an EnergyPlus Input Data Dictionary (IDD) file, usually
#' named as `Energy+.idd`.
#'
#' * `group`: A valid group name or valid group names.
#'
#' * `class`: A valid class name or valid class names.
#'
#' @section Detail:
#'
#' `IDD$new()` parses an EnergyPlus Input Data Dictionary (IDD) file, and
#' returns an IDD object.
#'
#' `$version()` returns the version string of current idd file.
#'
#' `$build()` returns the build tag string of current idd file.
#'
#' `$group_name(class)` returns group name that that `class` belong to.
#'
#' `$class_name(group)` returns class names of that `group`. If `group` not
#' given, all class names in current IDD are returned.
#'
#' `$group_order(group)` returns integer orders (orders of name apperarance in
#' the IDD file) of that `group`.
#'
#' `$class_order(class)` returns integer orders (orders of name apperarance in
#' the IDD file) of that `class`.
#'
#' `$object(class)` returns an IDDObject of that `class`.
#'
#' `$objects(class)` returns a list of IDDObjects of `class`es. If `class` is
#' NULL, all IDDObjects in current IDD are returned.
#'
#' `$objects_in_group(group)` returns a list of IDDObjects in that `group`.
#'
#' `$is_valid_group(group)` return `TRUE` if the input is a valid `group` name.
#'
#' `$is_valid_class(class)` return `TRUE` if the input is a valid `class` name.
#'
#' @importFrom R6 R6Class
#' @importFrom purrr map
#' @importFrom assertthat assert_that
#' @importFrom cli cat_rule cat_line
#' @return An IDD object
#' @docType class
#' @name IDD
#' @author Hongyuan Jia
#' @references
#' \href{https://github.com/NREL/EnergyPlus/tree/develop/src/IDF_Editor}{IDFEditor
#' source code}
NULL

#' @export
# IDD {{{
IDD <- R6::R6Class(classname = "IDD",

    public = list(
        # INITIALIZE
        # {{{
        initialize = function (path) {
            idd_file <- parse_idd(path)

            private$m_version<- idd_file$version
            private$m_build <- idd_file$build
            private$m_objects <- idd_file$data[,
                object := list(list(IDDObject$new(data_class, data_field))),
                by = class_order]
            private$m_reference <- idd_file$reference
            private$m_external <- idd_file$external
        },
        # }}}

        # PROPERTIES GETTERS
        # {{{
        version = function () {
            return(private$m_version)
        },

        build = function () {
            return(private$m_build)
        },

        group_name = function (class = NULL) {
            if (is_empty(class)) return(private$m_objects[["group"]])
            assert_that(self$is_valid_class(class))
            cls <- class
            private$m_objects[class %in% cls, group]
        },

        class_name = function (group = NULL) {
            if (is_empty(group)) return(private$m_objects[["class"]])
            assert_that(self$is_valid_group(group))
            grp <- group
            private$m_objects[group %in% grp, class]
        },

        group_order = function (group = NULL) {
            if (is_empty(group)) {
                res <- private$m_objects[["group_order"]]
                names(res) <- private$m_objects[["group"]]
            } else {
                assert_that(self$is_valid_group(group))
                grp <- group
                res <- private$m_objects[group %in% grp, group_order]
                names(res) <- grp
            }
            return(res)
        },

        class_order = function (class = NULL) {
            if (is_empty(class)) {
                res <- private$m_objects[["class_order"]]
                names(res) <- private$m_objects[["class"]]
            } else {
                assert_that(self$is_valid_class(class))
                cls <- class
                res <- private$m_objects[class %in% cls, class_order]
                names(res) <- cls
            }
            return(res)
        },

        object = function (class) {
            assert_that(self$is_valid_class(class))
            cls <- class
            return(private$m_objects[class == cls, object][[1]])
        },

        objects = function (class = NULL) {
            if (is_empty(class)) return(private$m_objects[["object"]])
            private$assert_valid_class(class)
            cls <- class
            return(private$m_objects[class %in% cls, object])
        },

        objects_in_group = function (group) {
            assert_that(self$is_valid_group(group))
            grp <- group
            return(private$m_objects[group == grp, object])
        },

        required_objects = function () {
            # return a list of all required IDDObjects
            # {{{
            private$m_objects[required_object == TRUE, object]
            # }}}
        },

        unique_objects = function () {
            # return a list of all unique IDDObjcts
            # {{{
            private$m_objects[unique_object == TRUE, object]
            # }}}
        },

        reference = function () {
            # return reference data
            # {{{
            private$m_reference
            # }}}
        },

        external = function () {
            # return external reference data
            # {{{
            private$m_external
            # }}}
        },
        # }}}

        # ASSERTIONS
        # {{{
        is_valid_group = function (group) {
            assert_that(is_string(group))
            group %in% private$m_objects[["group"]]
        },

        is_valid_class = function (class) {
            assert_that(is_string(class))
            class %in% private$m_objects[["class"]]
        },
        # }}}

        print = function () {
            ver <- paste0("Version: ", private$m_version)
            bld <- paste0("Build: ", private$m_build)
            cls <- paste0("Total Class: ", nrow(private$m_objects))
            cli::cat_rule(left = "EnergyPlus Input Data Dictionary")
            cli::cat_bullet(ver)
            cli::cat_bullet(bld)
            cli::cat_bullet(cls)
        }
    ),

    private = list(
        # PRIVATE FIELDS
        # {{{
        m_version = character(),
        m_build = character(),
        m_objects = data.table(),
        m_reference = list(),
        m_external = list(),
        # }}}

        # PRIVATE FUNCTIONS
        # {{{
        assert_valid_group = function (group) {
            # assert that all members in group are valid group names.
            # {{{
            valid <- purrr::map_lgl(group, self$is_valid_group)
            assert_that(all(valid),
                msg = paste0("Invalid group names found for current IDD",
                             "(Version: ", self$version(),",",
                             "Build: ", self$build(), "): ",
                             backtick_collapse(group[!valid]), "."))
            # }}}
        },

        assert_valid_class = function (class) {
            # assert that all members in class are valid class names.
            # {{{
            valid <- purrr::map_lgl(class, self$is_valid_class)
            assert_that(all(valid),
                msg = paste0("Invalid class name found: ",
                             backtick_collapse(class[!valid]), "."))
            # }}}
        }
        # }}}
    )
)
# }}}

#' @importFrom progress progress_bar
#' @importFrom purrr transpose
#' @importFrom data.table data.table ":=" dcast.data.table between
#' @importFrom data.table setcolorder setnames setattr setorder
#' @importFrom stringr str_trim
# parse_idd {{{
parse_idd <- function(path) {

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

    idd_dt <- data.table::data.table(line = seq_along(idd_str),
                                     string = idd_str, key = "line")

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
    idd_dt[explpt_loc > 1, string := stringr::str_trim(substr(string, 1L, explpt_loc - 1L), "right")]
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
        parse_issue(type = "Invalid line found", src = "IDD", stop = TRUE,
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
           `:=`(field_anid = stringr::str_trim(substr(string, 1L, slash_loc - 1L), "right"),
                slash_key_value = substr(string, slash_loc, nchar(string)))]
    idd_dt[endsWith(field_anid, ";"), `:=`(type = type_field_last)]
    # clean
    idd_dt[, slash_loc := NULL]

    # handle condensed fields
    # {{{
    idd_dt[, field_count := 0L]
    idd_dt[type %in% c(type_field, type_field_last),
           field_count := char_count(field_anid, "[,;]")]
    # applause to Matt Dowle:
    # https://stackoverflow.com/questions/15673662/applying-a-function-to-each-row-of-a-data-table
    idd_dt <- idd_dt[data.table::between(type, type_field, type_field_last),
        {s <- strsplit(field_anid, "\\s*[,;]\\s*");
         list(line = rep(line, sapply(s, length)), V1 = unlist(s))}][
        idd_dt, on = "line"][field_count == 1L, V1 := field_anid][, field_anid := NULL]
    # idd_dt <- idd_dt[type %in% c(type_field, type_field_last),
    #     strsplit(field_anid, "\\s*[,;]\\s*"), by = list(line)][
    #     idd_dt, on = "line"][field_count == 1L, V1 := field_anid][, field_anid := NULL]
    # data.table::setnames(idd_dt, "V1", "field_anid")
    # get row numeber of last field per condensed field line in each class
    idd_dt[, row_id := .I]
    line_field_last <- idd_dt[field_count> 1L][type == type_field_last,
        row_id[.N], by = list(line, type)]$V1
    # set all type of condensed field lines to "field"
    idd_dt[field_count > 1L, type := type_field]
    idd_dt[line_field_last, type := type_field_last]
    idd_dt[, row_id := NULL]
    # }}}

    # seperate field AN and id {{{
    idd_dt[field_count == 1L,
        field_anid := stringr::str_trim(substr(field_anid, 1L, nchar(field_anid) - 1L), "right")]
    idd_dt[field_count >= 1L,
           `:=`(field_an = substr(field_anid, 1L, 1L),
                field_id = substr(field_anid, 2L, nchar(field_anid)))]
    # }}}

    # get slash keys and values {{{
    idd_dt[type == type_slash, slash_key_value := string]
    # Remove slash
    idd_dt[!is.na(slash_key_value),
        slash_key_value := stringr::str_trim(
            substr(slash_key_value, 2L, nchar(slash_key_value)), "left")]

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
                slash_value = stringr::str_trim(substr(slash_key_value, space_loc + 1L, nchar(slash_key_value)), "left"))]
    # for "\extensible:<#>" with comments
    idd_dt[space_loc > 0L & slash_key == "EXTENSIBLE" & grepl("\\D$", slash_value),
           slash_value := substr(slash_value, 1L, regexpr("\\D", slash_value) - 1L)]
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
                         "MIN-FIELDS", "FORMAT", "REFERENCE-CLASS-NAME",
                         "EXTENSIBLE")
    field_slash_key <- c("FIELD", "NOTE", "REQUIRED-FIELD", "UNITS", "IP-UNITS",
                         "UNITSBASEDONFIELD", "MINIMUM", "MINIMUM>", "MAXIMUM",
                         "MAXIMUM<", "DEFAULT", "DEPRECATED", "AUTOSIZABLE",
                         "AUTOCALCULATABLE", "TYPE", "KEY", "OBJECT-LIST",
                         "EXTERNAL-LIST", "REFERENCE", "BEGIN-EXTENSIBLE")
    ignored_slash_key <- c("RETAINCASE", "OBSOLETE")
    # mark group slash key and values
    idd_dt[slash_key %in% group_slash_key,
           `:=`(slash_supported = TRUE, type = type_group, group = slash_value)]
    # mark class slash key and values
    idd_dt[slash_key %in% class_slash_key,
           `:=`(slash_supported = TRUE, type = type_class_slash)]
    # mark field slash key and values
    idd_dt[slash_key %in% field_slash_key,
           `:=`(slash_supported = TRUE, type = type_field_slash)]
    # mark other ignored slash keys
    idd_dt[slash_key %in% ignored_slash_key, slash_supported := TRUE]
    # check for unsupported slash keys
    line_error_slash_key <- idd_dt[slash_supported == FALSE, which = TRUE]
    if (length(line_error_slash_key) > 0L) {
        parse_issue(type = "Invalid slash key found.", src = "IDD", stop = TRUE,
                    data_errors = idd_dt[line_error_slash_key, list(line, string)])
    }
    # remove comments for "\extensible:<#>"
    # check for unsupported slash values
    idd_dt[, slash_value_upper := toupper(slash_value)]
    # \type {{{
    line_error_type <- idd_dt[slash_key == "TYPE" &
       !(slash_value_upper %in% c("REAL", "INTEGER", "ALPHA", "CHOICE",
            "OBJECT-LIST", "EXTERNAL-LIST", "NODE")), which = TRUE]
    if (length(line_error_type) > 0) {
        parse_issue("Invalid \\type found", idd_dt[line_error_type, list(line, string)])
    }
    # }}}
    # \external-List {{{
    line_error_external_list <- idd_dt[slash_key == "EXTERNAL-LIST" &
           !(slash_value_upper %in% c("AUTORDDVARIABLE", "AUTORDDMETER",
               "AUTORDDVARIABLEMETER")), which = TRUE]
    if (length(line_error_external_list) > 0) {
        parse_issue("Invalid \\external-list found", idd_dt[line_error_external_list, list(line, string)])
    }
    # }}}
    # \format {{{
    line_error_format <- idd_dt[slash_key %in% "FORMAT" &
        !(slash_value_upper %in% c("SINGLELINE", "VERTICES", "COMPACTSCHEDULE",
            "FLUIDPROPERTY", "VIEWFACTOR", "SPECTRAL")), which = TRUE]
    if (length(line_error_format) > 0) {
        parse_issue("Invalid \\format found", idd_dt[line_error_format, list(line, string)])
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
    idd_field <- data.table::dcast.data.table(idd_field,
        class_order + class + field_anid + field_an + field_id ~ slash_key,
        value.var = "slash_value",
        fun.aggregate = list(function(x) paste0(x, collapse = " ")), fill = NA)
    # merge line into dcasted table
    idd_field <- merge(idd_field, idd_field_line,
        by = c("class_order", "field_anid"), all.x = TRUE, sort = FALSE)
    # set order according to line
    data.table::setorder(idd_field, row_id)
    # delete line column
    idd_field[, row_id := NULL]
    # set names
    new_nms <- gsub("-", "_", tolower(names(idd_field)), fixed = TRUE)
    data.table::setnames(idd_field, new_nms)

    # order fields per class
    idd_field[, field_order := seq_along(field_anid), by = list(class)]

    # add columns if there are no data in the IDD
    if (!has_name(idd_field, "autocalculatable")) idd_field[, autocalculatable := FALSE]
    if (!has_name(idd_field, "autosizable")) idd_field[, autosizable := FALSE]
    if (!has_name(idd_field, "required_field")) idd_field[, required_field := FALSE]
    if (!has_name(idd_field, "unitsbasedonfield")) idd_field[, unitsbasedonfield := FALSE]
    if (!has_name(idd_field, "begin_extensible")) idd_field[, begin_extensible := FALSE]
    if (!has_name(idd_field, "maximum")) idd_field[, maximum := NA_real_]
    if (!has_name(idd_field, "minimum")) idd_field[, minimum := NA_real_]
    if (!has_name(idd_field, "maximum<")) idd_field[, `maximum<` := NA_real_]
    if (!has_name(idd_field, "minimum>")) idd_field[, `minimum>` := NA_real_]
    if (!has_name(idd_field, "reference")) idd_field[, reference := NA_character_]
    if (!has_name(idd_field, "key")) idd_field[, key := NA_character_]
    if (!has_name(idd_field, "object_list")) idd_field[, object_list := NA_character_]
    if (!has_name(idd_field, "external_list")) idd_field[, external_list := NA_character_]
    if (!has_name(idd_field, "field")) idd_field[, field := NA_character_]
    if (!has_name(idd_field, "units")) idd_field[, units := NA_character_]
    if (!has_name(idd_field, "ip_units")) idd_field[, ip_units := NA_character_]

    # set column type and fill NA
    idd_field[, `:=`(autocalculatable = as.logical(autocalculatable),
                     autosizable = as.logical(autosizable),
                     required_field = as.logical(required_field),
                     unitsbasedonfield = as.logical(unitsbasedonfield),
                     begin_extensible = as.logical(begin_extensible),
                     maximum = as.double(maximum),
                     minimum = as.double(minimum),
                     `maximum<` = as.double(`maximum<`),
                     `minimum>` = as.double(`minimum>`))]

    idd_field[is.na(autocalculatable), autocalculatable := FALSE]
    idd_field[is.na(autosizable), autosizable := FALSE]
    idd_field[is.na(required_field), required_field := FALSE]
    idd_field[is.na(unitsbasedonfield), unitsbasedonfield := FALSE]
    idd_field[is.na(begin_extensible), begin_extensible := FALSE]

    # add range helper column
    idd_field[, `:=`(have_range = FALSE, lower_incbounds = FALSE, upper_incbounds = FALSE)]
    idd_field[!is.na(minimum), `:=`(have_range = TRUE, lower_incbounds = TRUE)]
    idd_field[!is.na(maximum), `:=`(have_range = TRUE, upper_incbounds = TRUE)]
    idd_field[!is.na(`minimum>`), `:=`(have_range = TRUE, minimum = `minimum>`)]
    idd_field[!is.na(`maximum<`), `:=`(have_range = TRUE, maximum = `maximum<`)]
    idd_field[, `:=`(`minimum>` = NULL, `maximum<` = NULL)]
    # get range
    idd_field[, range := list()]
    idd_field[!is.na(minimum) & is.na(maximum), range := list(list(
        list(lower = minimum, lower_incbounds = lower_incbounds,
             upper = Inf, upper_incbounds = upper_incbounds))),
        by = c("class_order", "field_order")]
    idd_field[is.na(minimum) & !is.na(maximum), range := list(list(
        list(lower = -Inf, lower_incbounds = lower_incbounds,
             upper = maximum, upper_incbounds = upper_incbounds))),
        by = c("class_order", "field_order")]
    idd_field[!is.na(minimum) & !is.na(maximum), range := list(list(
        list(lower = minimum, lower_incbounds = lower_incbounds,
             upper = maximum, upper_incbounds = upper_incbounds))),
        by = c("class_order", "field_order")]
    idd_field[, `:=`(minimum = NULL, maximum = NULL,
                     lower_incbounds = NULL, upper_incbounds = NULL)]

    # split reference
    idd_field[, new_reference := list(strsplit(reference, " ", fixed = TRUE)),
              by = c("class_order", "field_order")]
    # idd_field[, reference := NULL]
    data.table::setnames(idd_field,
                         c("new_reference", "reference"),
                         c("reference", "reference_string"))

    # split choice
    idd_field[, new_key := list(strsplit(key, " ", fixed = TRUE)),
              by = c("class_order", "field_order")]
    idd_field[, key := NULL]
    data.table::setnames(idd_field, "new_key", "key")

    # split objectList
    idd_field[, new_object_list := list(strsplit(object_list, " ", fixed = TRUE)),
              by = c("class_order", "field_order")]
    data.table::setnames(idd_field,
                         c("new_object_list", "object_list"),
                         c("object_list", "object_list_string"))

    # split externalList
    idd_field[, new_external_list := list(strsplit(external_list, " ", fixed = TRUE)),
              by = c("class_order", "field_order")]
    data.table::setnames(idd_field,
                         c("new_external_list", "external_list"),
                         c("external_list", "external_list_string"))


    # add field standard and lower case field names
    idd_field[is.na(field), `_field_name` := field_anid]
    idd_field[!is.na(field), `_field_name` := field]
    idd_field[is.na(units), `_unit` := ""]
    idd_field[!is.na(units), `_unit` := paste0("{", units, "}")]
    idd_field[!is.na(ip_units), `_ip_unit` := paste0("{", ip_units, "}")]
    idd_field[is.na(ip_units), `_ip_unit` := `_unit`]
    idd_field[!is.na(units), `_field` := paste0(`_field_name`, " ", `_unit`)]
    idd_field[!is.na(ip_units), `_field_ip` := paste0(`_field_name`, " ", `_ip_unit`)]
    idd_field[is.na(units), `_field` := `_field_name`]
    idd_field[is.na(ip_units), `_field_ip` := `_field_name`]

    data.table::setorder(idd_field, class_order, field_order)
    neworder <- c("class_order", "class", "field_order", "field", "field_anid",
                  "field_an", "field_id", "units", "ip_units", "required_field",
                  "type", "have_range", "range", "default", "key", "autosizable",
                  "autocalculatable", "note", "begin_extensible", "reference",
                  "object_list", "external_list", "unitsbasedonfield", "_field_name",
                  "_unit", "_ip_unit", "_field", "_field_ip",
                  "reference_string", "object_list_string", "external_list_string")
    idd_field <- idd_field[, .SD, .SDcol = neworder]
    data.table::setcolorder(idd_field, neworder)
    # }}}

    pb$update(0.8, tokens = list(what = "Parsing "))
    # CLASS data
    # {{{
    # extract class data
    idd_class <- idd_dt[data.table::between(type, type_group, type_class_slash),
        .SD, .SDcol = c("group", "class", "slash_key", "slash_value")]
    # rolling fill downwards for group and class
    idd_class[, group := group[1L], by = list(cumsum(!is.na(group)))]
    idd_class[, class := class[1L], by = list(cumsum(!is.na(class)))]
    # As group has been add into a seperated column named "group"and also the
    # last class in one group has been mis-categorized into the next group by
    # the filing process, delete group slash_key
    idd_class <- idd_class[!(slash_key %in% "GROUP")]
    # as the first na in first group can not be replaced using downward filling
    idd_class <- idd_class[!is.na(class)]
    # first handle classes without slashes such as "SwimmingPool:Indoor"
    class_no_slash <- idd_class[, .N, by = list(class)][N == 1L, class]
    idd_class <- idd_class[!class %in% class_no_slash & !is.na(slash_key) |
        class %in% class_no_slash]
    # if slash key exists and slash value not, it must be a logical attribute,
    # such as "\\unique-object". Set it to TRUE
    idd_class <- idd_class[is.na(slash_value), slash_value := "TRUE"]
    # order group and class as the sequence the appears in IDD
    idd_class[, group_order := .GRP, by = list(group)]
    idd_class[, class_order := .GRP, by = list(class)]
    # using dcast to cast all class attributes into seperated columns
    idd_class <- data.table::dcast.data.table(idd_class,
        group_order + group + class_order + class ~ slash_key,
        value.var = "slash_value",
        fun.aggregate = list(function(x) paste0(x, collapse = " ")), fill = NA)
    # delete column "NA" caused by classes without slash such as
    # "SwimmingPool:Indoor"
    if (has_name(idd_class, "NA")) idd_class[, `:=`(`NA` = NULL)]
    # set names
    new_nms <- gsub("-", "_", tolower(names(idd_class)), fixed = TRUE)
    data.table::setnames(idd_class, new_nms)
    # add columns if there are no data in the IDD
    if (!has_name(idd_class, "min_fields")) idd_class[, min_fields := NA_integer_]
    if (!has_name(idd_class, "required_object")) idd_class[, required_object := FALSE]
    if (!has_name(idd_class, "unique_object")) idd_class[, unique_object := FALSE]
    if (!has_name(idd_class, "extensible")) idd_class[, extensible := NA_integer_]
    if (!has_name(idd_class, "format")) idd_class[, format := NA_character_]
    if (!has_name(idd_class, "memo")) idd_class[, memo := NA_character_]
    if (!has_name(idd_class, "reference_class_name")) idd_class[, reference_class_name := NA_character_]
    # set column type
    idd_class[, `:=`(min_fields = as.integer(min_fields),
                     required_object = as.logical(required_object),
                     unique_object = as.logical(unique_object),
                     extensible = as.integer(extensible))]
    # fill na
    idd_class[is.na(format), format := "standard"]
    idd_class[is.na(min_fields), min_fields := 0L]
    idd_class[is.na(required_object), required_object := FALSE]
    idd_class[is.na(unique_object), unique_object := FALSE]
    idd_class[is.na(extensible), extensible := 0L]
    # get max field per class
    idd_class <- idd_field[, list(num_fields = .N), by = class][idd_class, on = "class"]
    # split reference_class_name
    idd_class[, new_reference_class_name := list(strsplit(reference_class_name, " ", fixed = TRUE)),
              by = c("class_order")]
    # idd_class[, reference_class_name := NULL]
    data.table::setnames(idd_class,
                         c("new_reference_class_name", "reference_class_name"),
                         c("reference_class_name", "reference_class_name_string"))
    neworder <- c("group_order", "group", "class_order", "class", "format", "memo",
         "min_fields", "num_fields", "required_object", "unique_object",
         "extensible", "reference_class_name", "reference_class_name_string")
    data.table::setcolorder(idd_class, neworder)
    data.table::setorder(idd_class, group_order, class_order)
    # }}}

    # REFERENCE data
    # {{{
    # Object List data
    idd_object_list <- idd_field[!is.na(object_list_string),
        list(class_order, class, field_order, object_list)]
    idd_field[, `:=`(object_list_string = NULL)]
    # Reference Field data
    idd_reference_field <- idd_field[!is.na(reference_string),
        list(class_order, class, field_order, reference)]
    idd_field[, `:=`(reference_string = NULL)]
    # Reference Class data
    idd_reference_class <- idd_class[!is.na(reference_class_name_string),
        list(class_order, class, reference_class_name)]
    idd_class[, `:=`(reference_class_name_string = NULL)]
    # Combine
    idd_reference <- list(object_list = idd_object_list,
                          reference_class = idd_reference_class,
                          reference_field = idd_reference_field)
    # }}}

    # EXTERNAL data
    # {{{
    # External List data
    idd_external_list <- idd_field[!is.na(external_list_string),
        list(class_order, class, field_order, external_list)]
    idd_field[, `:=`(external_list_string = NULL)]
    idd_external <- list(external_list = idd_external_list,
                         reference_rdd = list(),
                         reference_mdd = list())
    # }}}

    pb$update(0.85, tokens = list(what = "Parsing "))
    # split class data per class name
    idd_class_per_class <- split(idd_class, by = "class")

    pb$update(0.90, tokens = list(what = "Parsing "))
    # split field data per field name
    idd_field_per_class <- split(idd_field, by = "class")

    idd_data <- idd_class[, `:=`(data_class = idd_class_per_class,
                                 data_field = idd_field_per_class)]

    pb$update(0.95, tokens = list(what = "Parsing "))
    idd <- list(version = idd_version,
                build = idd_build,
                data = idd_data,
                reference = idd_reference,
                external = idd_external)
    # set class to IDD
    data.table::setattr(idd, "class", c("IDD_File", class(idd)))
    pb$tick(100L, tokens = list(what = "Complete"))
    return(idd)
}
# }}}

#' @importFrom stringr str_trim
#' @importFrom readr read_lines
# read_idd {{{
read_idd <- function(filepath) {
    idd_str <- readr::read_lines(filepath)
    # Have to fix encoding errors in version 8.3 and below
    idd_str <- iconv(idd_str, "UTF-8", "UTF-8", "byte")
    idd_str <- gsub("<92>", "'", idd_str, useBytes = TRUE, fixed = TRUE)
    idd_str <- gsub("<93>", "\"", idd_str, useBytes = TRUE, fixed = TRUE)
    idd_str <- gsub("<94>", "\"", idd_str, useBytes = TRUE, fixed = TRUE)
    idd_str <- gsub("<b0>", "deg", idd_str, useBytes = TRUE, fixed = TRUE)
    idd_str <- gsub("<d0>", "D", idd_str, useBytes = TRUE, fixed = TRUE)
    # Get rid of leading and trailing spaces
    idd_str <- stringr::str_trim(idd_str, "both")

    return(idd_str)
}
# }}}

#' @importFrom cli rule
# parse_issue {{{
parse_issue <- function (type, data_errors = NULL, info = NULL,
                         src = c("IDD", "IDF"), stop = TRUE, quote = TRUE) {
    if (!is.null(info)) {
        sep <- paste0(cli::rule(), "\n")
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
                           sapply(data_errors[["string"]], backtick)), collapse = "\n")
    } else {
        data_str <- paste0(sprintf(paste0("Line %", max_c, "s: %s"), data_errors[["line"]],
                           data_errors[["string"]]), collapse = "\n")
    }

    header <- sprintf("%s PARSING ERROR", src)

    mes <- paste0("\n",
        cli::rule(line = 2), "\n",
        header, "\n",
        paste0(key_line, ": ", type), "\n",
        paste0("[Total Number]: ", error_num), "\n",
        error_truncated,
        cli::rule(), "\n",
        data_str, "\n",
        sep,
        info,
        cli::rule(line = 2),
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

################################################################################
#                                  ASSERTIONS                                  #
################################################################################

#' @importFrom assertthat "on_failure<-"
on_failure(IDD$public_methods$is_valid_class) <- function (call, env) {
    paste0("Invalid class name found: ", backtick(eval(call$class, env)), ".")
}

on_failure(IDD$public_methods$is_valid_group) <- function (call, env) {
    paste0("Invalid group name found: ", backtick(eval(call$group, env)), ".")
}

'[.IDD' <- function(x, i, j, ..., drop = FALSE) {
    m_obj <- .subset2(x, "objects")()
    .subset2(m_obj, i)
}
