# TODO: fields created by extensible fields should have the same
# reference data

#' @export
# IDD {{{
IDD <- R6::R6Class(classname = "IDD",

    public = list(
        initialize = function (path) {
            idd_file <- parse_idd_(path)

            private$m_version<- idd_file$version
            private$m_build <- idd_file$build
            private$m_orders <- idd_file$order
            # private$m_data <- idd_file$field

            private$m_objects <- purrr::map(idd_file$data, IDDObject$new)
            names(private$m_objects) <- private$m_orders[, class]
        },

        #############
        #  Getters  #
        #############
        # {{{
        version = function () {
            return(private$m_version)
        },

        build = function () {
            return(private$m_build)
        },

        class_name = function (group = NULL) {
            if (is_empty(group)) return(names(private$m_objects))

            assert_that(self$is_valid_group(group))
            grp <- group
            private$m_orders[group == grp, class]
        },

        class_order = function (name = NULL) {
            private$get_order_element("class_order", name)
        },

        group_name = function (name = NULL) {
            private$get_order_element("group", name)
        },

        group_order = function (name = NULL) {
            private$get_order_element("group_order", name)
        },

        orders = function () {
            return(private$m_orders)
        },

        objects = function (name = NULL) {
            if (is_empty(name)) return(private$m_objects)
            purrr::walk(name, ~assert_that(self$is_valid_class(.x)))
            return(private$m_objects[name])
        },

        objects_group = function (group) {
            assert_that(self$is_valid_group(group))
            cls <- self$class_name(group)
            return(private$m_objects[cls])
        },

        is_valid_class = function (name) {
            assert_that(is_string(name))
            name %in% self$class_name()
        },

        is_valid_group = function (name) {
            assert_that(is_string(name))
            name %chin% private$m_orders[["group"]]
        },

        print = function () {
            ver <- paste0("Version: ", private$m_version)
            bld <- paste0("Build: ", private$m_build)
            cls <- paste0("Total Class: ", private$m_orders[, .N])
            cat(cli::rule(left = "EnergyPlus Input Data Dictionary"), "\n")
            cat(ver, bld, cls, sep = "\n")
        }
        # }}}
    ),

    private = list(
        m_version = character(),
        m_build = character(),
        m_orders = data.table(),
        # m_data = data.table(),
        m_objects = list(),

        get_order_element = function (type, name = NULL) {
        # return a single order element
            # {{{
            if (is_empty(name)) {
                ord <- private$m_orders[[type]]
                names(ord) <- self$class_name()
            } else {
                purrr::walk(name, ~assert_that(self$is_valid_class(.x)))
                ord <- private$m_orders[class %in% name][[type]]
                nms <- private$m_orders[class %in% name][["class"]]
                names(ord) <- nms
                ord <- ord[name]
            }

            return(ord)
            # }}}
        }

    )
)
# }}}

#' @export
# IDDObject {{{
IDDObject <- R6::R6Class(classname = "IDDObject",

    public = list(
        initialize = function (iddobj_list) {
            # {{{
            private$m_properties <- iddobj_list$class
            private$m_fields <- iddobj_list$field
            # }}}
        },

        ##########################
        #  PROPERTIES FUNCTIONS  #
        ##########################
        # properties getters
        # {{{
        group_name = function () {
        # return group name
            # {{{
            return(private$m_properties$group)
            # }}}
        },

        group_order = function () {
        # return group order
            # {{{
            return(private$m_properties$group_order)
            # }}}
        },

        class_name = function () {
        # return class name
            # {{{
            return(private$m_properties$class)
            # }}}
        },

        class_order = function () {
        # return class order
            # {{{
            return(private$m_properties$class_order)
            # }}}
        },

        class_format = function () {
        # return class format
            # {{{
            return(private$m_properties$format)
            # }}}
        },

        min_fields = function () {
        # return minimum field number requirement
            # {{{
            return(private$m_properties$min_fields)
            # }}}
        },

        num_fields = function () {
        # return total number of fields in definition
            # {{{
            return(private$m_properties$num_fields)
            # }}}
        },

        memo = function () {
        # return memo
            # {{{
            return(private$m_properties$memo)
            # }}}
        },

        num_extensible = function () {
        # return field number of extensible fields in a extensible field group
            # {{{
            return(private$m_properties$extensible)
            # }}}
        },

        reference_class_names = function () {
        # return reference class name
            # {{{
            .data <- private$m_properties$reference_class_name
            if (is.na(.data)) return(.data)
            return(strsplit(.data, " ", fixed = TRUE)[[1]])
            # }}}
        },

        first_extensible = function () {
        # return index of the first extensible field
            # {{{
            .data <- private$m_fields[begin_extensible == TRUE, field_order]
            if (is_empty(.data)) return(0L)
            return(.data)
            # }}}
        },

        extensible_group = function () {
        # return data of the extensible field group
            # {{{
            # if the current object is not extensible, return an empty data.table
            if (!self$is_extensible()) return(data.table())

            # get the line number of extensible fields
            first_ext <- self$first_extensible()
            num_ext <- self$num_extensible()
            lines_ext <- seq.int(first_ext, length.out = num_ext)
            .data <- private$m_fields[lines_ext][, `:=`(required_field = FALSE,
                                                        begin_extensible = FALSE)]

            return(.data[])
            # }}}
        },

        add_extensible_groups = function (num = 1L) {
        # add one or more extensible groups
            # {{{
            .data <- data.table::rbindlist(
                 purrr::map(seq_len(num),
                            ~private$append_extensible_numbers(.x)))
            private$m_fields <- data.table::rbindlist(list(private$m_fields, .data))
            private$m_properties[, num_fields := {private$m_fields[, .N]}]
            return(self)
            # }}}
        },
        # }}}

        # properties assertions
        # {{{
        is_version = function () {
        # return TRUE if this is the 'Version' object
            # {{{
            ifelse(self$class_name() == "Version", TRUE, FALSE)
            #}}}
        },

        is_required = function () {
        # return TRUE if this is a required object
            # {{{
            return(private$m_properties$required_object)
            # }}}
        },

        is_unique = function () {
        # return TRUE if this is a unique object
            # {{{
            return(private$m_properties$unique_object)
            # }}}
        },

        is_extensible = function () {
        # return TRUE if this object contains extensible fields
            # {{{
            ifelse(self$num_extensible(), TRUE, FALSE)
            # }}}
        },

        has_name = function () {
        # return TRUE if this object has a name field
            # {{{
            grepl("Name", private$m_fields[field_order == 1L, field], fixed = TRUE)
            # }}}
        },
        # }}}

        ######################
        #  FIELDS FUNCTIONS  #
        ######################
        # fields getters
        # {{{
        field_name = function (index = NULL, lower = FALSE, unit = FALSE, in_ip = FALSE) {
        # return field name by field index
            # {{{
            if (lower) {
                private$field_name_lcase(index, unit = unit, in_ip = in_ip)
            } else {
                private$field_name_std(index, unit = unit, in_ip = in_ip)
            }
            # }}}
        },

        field_index = function (name = NULL) {
        # return field index by field name (in either standard or lower case
        # format)
            # {{{
            if (is.null(name)) return(seq_len(self$num_fields()))
            name_std <- private$field_name_std()
            name_lc <- private$field_name_lcase()
            valid <- name %in% name_std | name %in% name_lc
            assert_that(all(valid),
                        msg = paste0("Invalid field name found: ",
                                     paste0(sQuote(name[!valid]), collapse = ", ")))
            res <- match(name, name_std)
            res_lc <- match(name, name_lc)
            res[is.na(res)] <- res_lc[is.na(res)]
            return(res)
            # }}}
        },

        field_type = function (index = NULL, name = NULL) {
        # return field type
            # {{{
            res <- private$fields(index, name)[, type]
            # if no type specified, using field_an
            input <- index %||% name
            mis <- input[is.na(res)]
            if (is_empty(mis)) return(res)
            an_mis <- private$fields(mis)[, field_an]
            type_mis <- ifelse(an_mis == "N", "real", "alpha")
            res[is.na(res)] <- type_mis
            return(res)
            # }}}
        },

        field_unit = function (index = NULL, name = NULL, in_ip = FALSE) {
        # return field unit in SI or IP format
            # {{{
            if (in_ip) {
                private$fields(index, name)[, ip_units]
            } else {
                private$fields(index, name)[, units]
            }
            # }}}
        },

        field_reference = function (index = NULL, name = NULL) {
        # return field references
            # {{{
            private$fields(index, name)[, reference]
            # }}}
        },

        field_object_list = function (index = NULL, name = NULL) {
        # return field reference key by field index
            # {{{
            private$fields(index, name)[, object_list]
            # }}}
        },

        field_default = function (index = NULL, name = NULL) {
        # return field default value by field index
            # {{{
            # if default exists, use it
            .field <- private$fields(index, name)
            .field[, .default := list(list(default)), by = "field_order"]

            # enforce value type
            .field[field_an == "N" & !tolower(default) %in% c("autosize", "autocalculate"),
                   .default := list(list(as.numeric(default))), by = "field_order"]
            # if no default, and choices exist, use the first one
            .field[is.na(default) & !is.na(key), .default := list(list(unlist(key)[1])),
                   by = "field_order"]
            # if no default and the type is not choice, use corresponding NA
            .field[purrr::map_lgl(.default, is.na) & field_an == "N",
                   .default := list(list(NA_real_)), by = "field_order"]
            .field[purrr::map_lgl(.default, is.na) & field_an == "A",
                   .default := list(list(NA_character_)), by = "field_order"]

            .field[, .default]
            # }}}
        },

        field_choice = function (index = NULL, name = NULL) {
        # return field choices by field index
            # {{{
            private$fields(index, name)[, key]
            # }}}
        },

        field_range = function (index = NULL, name = NULL) {
        # return field range value by field index
            # {{{
            res <- private$fields(index, name)[, range]
            out <- purrr::map_chr(res, private$str_range)
            names(res) <- out
            # }}}
        },

        # ASSERTIONS
        # {{{
        is_valid_field_num = function (num) {
        # check if the input number is acceptable as the total number of fields
        # for this object
            # {{{
            assert_that(is_integerish(num))
            # if has \min-fields
            min_num <- self$min_fields()
            if (min_num > 0L & min_num > num) return(FALSE)
            # if no \extensible:<#>
            if (!self$is_extensible()) {
                if (self$num_fields() < num) return(FALSE) else return(TRUE)
            # if has \extensible:<#>
            } else {
                if (private$get_extensible_field_index(num) == 0L) {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            }
            # }}}
        },

        is_valid_field_name = function (name) {
        # check if input name is a valid field name
            # {{{
            name %in% private$field_name_std() |
            name %in% private$field_name_lcase()
            # }}}
        },

        is_valid_field_index = function (index) {
        # check if input index is a valid field index
            # {{{
            purrr::walk(index, ~assert_that(is_integerish(.x)))
            index <= self$num_fields()
            # }}}
        },

        is_autosizable_field = function (index = NULL, name = NULL) {
        # check if a field is autosizable or not
            # {{{
            private$fields(index, name)[, autosizable]
            # }}}
        },

        is_autocalculatable_field = function (index = NULL, name = NULL) {
        # check if a field is autocalculatable or not
            # {{{
            private$fields(index, name)[, autocalculatable]
            # }}}
        },

        is_numeric_field = function (index = NULL, name = NULL) {
        # check if a field is a numeric field or not
            # {{{
            private$fields(index, name)[, field_an] == "N"
            # }}}
        },

        is_integer_field = function (index = NULL, name = NULL) {
        # check if a field is an integer field or not
            # {{{
            private$fields(index, name)[, type] == "integer"
            # }}}
        },

        is_required_field = function (index = NULL, name = NULL) {
        # return TURE if the index is an index of a required field
            # {{{
            private$fields(index, name)[, required_field]
            # }}}
        },
        # }}}
        # }}}

        print = function (ip_unit = FALSE) {
            # {{{
            cli::cat_line(clisymbols::symbol$star, clisymbols::symbol$star,
                          " Class: ", sQuote(self$class_name()), " ",
                          clisymbols::symbol$star, clisymbols::symbol$star)
            cli::cat_rule(center = "* MEMO* ", line = 1)
            cli::cat_line("  \"", msg(self$memo()), "\"")
            cli::cat_rule(center = "* PROPERTIES *", line = 1)
            cli::cat_line("  Group: ", self$group_name())
            cli::cat_line("  Unique object: ", self$is_unique())
            cli::cat_line("  Required: ", self$is_required())
            cli::cat_line("  Format: ", self$class_format())
            cli::cat_line("  Total fields: ", self$num_fields())
            cli::cat_line("  Minimum fields: ", self$min_fields())
            cli::cat_line("  Extensible field number: ", self$num_extensible())
            cli::cat_line("  First extensible field index: ", self$first_extensible())
            cli::cat_rule(center = "* FIELDS* ", line = 1)

            if (self$is_extensible()) {
                first_ext <- self$first_extensible()
                last_ext <- first_ext + self$num_extensible() - 1L
                last_req <- private$m_fields[required_field == TRUE, max(field_order)]
                num_print <- max(last_ext, last_req)
                mark_ext <- character(num_print)
                mark_ext[seq.int(first_ext, last_ext)] <- paste0(" <", clisymbols::symbol$arrow_down, ">")
            } else {
                num_print <- self$num_fields()
                mark_ext <- character(num_print)
            }
            lines <- seq_len(num_print)

            nms <- self$field_name(unit = TRUE, in_ip = ip_unit)[lines]
            index <- stringr::str_pad(lines, nchar(num_print), "left")
            required <- private$m_fields[field_order <= num_print][
                required_field == TRUE, field_order]

            if (not_empty(required)) {
                mark_req <- character(length(nms))
                mark_req[required] <- paste("* ")
                mark_req[-required] <- "  "
            } else {
                mark_req <- " "
            }

            cli::cat_line("  ", index, ":", mark_req, nms, mark_ext)
            if (self$is_extensible()) cli::cat_line("  ......")
            cli::cat_rule(line = 1)
            # }}}
        }
    ),

    private = list(
        # PRIVATE FIELDS
        # {{{
        # class property data
        m_properties = data.table(),
        # field data
        m_fields = data.table(),
        # }}}

        # PRIVATE FUNCTIONS
        # {{{
        fields = function (index = NULL, name = NULL) {
        # return single field data by field index or field name (either in
        # standard format or lower case format)
            # {{{
            # either 'index' or 'name' should be used, not both
            if (all(not_empty(index), not_empty(name))) {
                warning("Both 'index' and 'name' are given. 'name' will be ignored.",
                        call. = FALSE)
                name <- NULL
            }

            if (not_empty(index)) {
                assert_that(all(self$is_valid_field_index(index)),
                            msg = paste0("`index` shoud be no larger than the field number(",
                                   self$num_fields(), ") in current object."))
                out <- private$m_fields[index]
            } else if (not_empty(name)) {
                index <- field_index(name)
                out <- private$m_fields[index]
            } else {
                out <- private$m_fields
            }

            return(out)
            # }}}
        },

        field_name_std = function (index = NULL, unit = FALSE, in_ip = FALSE) {
        # return standard field name
            # {{{
            .data <- private$m_fields
            if (not_empty(index)) {
                assert_that(is.numeric(index))
                valid <- self$is_valid_field_index(index)
                assert_that(all(valid),
                            msg = paste0("Invalid field index found: ",
                                         paste0(sQuote(index[valid]), collapse = ", ")))
                .data <- .data[index]
            }
            if (unit) {
                if (in_ip) {
                    res <- .data[, `_field_ip`]
                } else {
                    res <- .data[, `_field`]
                }
            } else {
                res <- .data[, `_field_name`]
            }

            return(res)
            # }}}
        },

        field_name_lcase = function (index = NULL, unit = FALSE, in_ip = FALSE) {
        # return lower case field name
            # {{{
            std_res <- private$field_name_std(index, unit, in_ip)
            gsub("[ -]", "_", tolower(std_res))
            # }}}
        },

        get_extensible_field_index = function (index) {
        # change input index to the line number of extensible data
            # {{{
            (index - self$first_extensible() + 1L) %% self$num_extensible()
            # }}}
        },

        num_extensible_group = function () {
        # return the number of extensible groups in the original idd data
            # {{{
            (self$num_fields() - self$first_extensible() + 1) / self$num_extensible()
            # }}}
        },

        append_extensible_numbers = function (num = 1L) {
        # append numbers in field, field_id, field_anid, and field_order in
        # extensible groups
            # {{{
            assert_that(is_integerish(num) & num > 0L)
            num_ext <- self$num_extensible()
            # get current number of extensible groups
            num_grp <- private$num_extensible_group()
            # get max field_id
            max_field_id <- private$m_fields[.N, as.integer(field_id)]
            # get new added field id
            added_field_id <- (num - 1L) * self$num_extensible() + seq_len(self$num_extensible())
            # get total fields
            num_field <- self$num_fields()
            # append numbers in field, field_id, field_anid, field_order
            .data <- self$extensible_group()
            .data[, field := gsub("1", num_grp + num, field)]
            .data[, `_field_name` := gsub("1", num_grp + num, `_field_name`)]
            .data[, `_field` := gsub("1", num_grp + num, `_field`)]
            .data[, `_field_ip` := gsub("1", num_grp + num, `_field_ip`)]
            .data[, field_id := as.character(max_field_id + added_field_id)]
            .data[, field_anid := paste0(field_an, field_id)]
            .data[, field_order := num_field + added_field_id]
            .data[, field_order := num_field + added_field_id]

            return(.data)
            # }}}
        }
        # }}}
    )
)
# }}}

# parse_idd_ {{{
parse_idd_ <- function(path) {

    assert_that(is_readable(path))

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
    # remove comments for "\extensible:<#>"
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

    # order fields per class
    idd_field[, field_order := seq_along(field_anid), by = list(class)]

    # set column type
    idd_field[, `:=`(autocalculatable = as.logical(autocalculatable),
                     autosizable= as.logical(autosizable),
                     maximum = as.double(maximum),
                     minimum = as.double(minimum),
                     `maximum<` = as.double(`maximum<`),
                     `minimum>` = as.double(`minimum>`),
                     required_field = as.logical(required_field),
                     unitsbasedonfield = as.logical(unitsbasedonfield),
                     begin_extensible = as.logical(begin_extensible))]
    # fill na
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
    idd_field[, reference := NULL]
    setnames(idd_field, "new_reference", "reference")

    # split choice
    idd_field[, new_key := list(strsplit(key, " ", fixed = TRUE)),
              by = c("class_order", "field_order")]
    idd_field[, key := NULL]
    setnames(idd_field, "new_key", "key")

    # split objectList
    idd_field[, new_object_list := list(strsplit(object_list, " ", fixed = TRUE)),
              by = c("class_order", "field_order")]
    idd_field[, object_list := NULL]
    setnames(idd_field, "new_object_list", "object_list")

    # add field standard and lower case field names
    idd_field[is.na(field), `_field_name` := field_anid]
    idd_field[!is.na(field), `_field_name` := field]
    idd_field[is.na(units), `_unit` := ""]
    idd_field[!is.na(units), `_unit` := paste0("{", units, "}")]
    idd_field[!is.na(ip_units), `_ip_unit` := paste0("{", ip_units, "}")]
    idd_field[is.na(ip_units), `_ip_unit` := `_unit`]
    idd_field[, `_field` := paste0(`_field_name`, " ", `_unit`)]
    idd_field[, `_field_ip` := paste0(`_field_name`, " ", `_ip_unit`)]

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
    # first handle classes without slashes such as "SwimmingPool:Indoor"
    class_no_slash <- idd_class[, .N, by = list(class)][N == 1L, class]
    idd_class <- idd_class[!class %chin% class_no_slash & !is.na(slash_key) |
        class %chin% class_no_slash]
    # if slash key exists and slash value not, it must be a logical attribute,
    # such as "\\unique-object". Set it to TRUE
    idd_class <- idd_class[is.na(slash_value), slash_value := "TRUE"]
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
    neworder <- c("group_order", "group", "class_order", "class", "format", "memo",
         "min_fields", "num_fields", "required_object", "unique_object",
         "extensible", "reference_class_name")
    avail_col <- intersect(neworder, names(idd_class))
    setcolorder(idd_class, c(avail_col, setdiff(names(idd_class), neworder)))
    setorder(idd_class, group_order, class_order)
    # }}}

    # restore group and class order data
    idd_order <- idd_class[, .SD, .SDcol = c("group_order", "group", "class_order", "class")]

    pb$update(0.85, tokens = list(what = "Parsing "))
    # split class data per class name
    idd_class_per_class <- split(idd_class, by = "class")

    pb$update(0.90, tokens = list(what = "Parsing "))
    # split field data per field name
    idd_field_per_class <- split(idd_field, by = "class")

    idd_data <- purrr::transpose(list(class = idd_class_per_class,
                                      field = idd_field_per_class))

    pb$update(0.95, tokens = list(what = "Parsing "))
    idd <- list(version = idd_version,
                build = idd_build,
                order = idd_order,
                # field = idd_field,
                data = idd_data)
    # set class to IDD
    setattr(idd, "class", c("IDD_File", class(idd)))
    pb$tick(100L, tokens = list(what = "Complete"))
    return(idd)
}
# }}}

#######################################################################
#                             ASSERTIONS                              #
#######################################################################
on_failure(IDD$public_methods$is_valid_class) <- function (call, env) {
    paste0(sQuote(eval(call$name, env)), " is not a valid class name in current IDD.")
}

on_failure(IDD$public_methods$is_valid_group) <- function (call, env) {
    paste0(sQuote(eval(call$name, env)), " is not a valid group name in current IDD.")
}

on_failure(IDDObject$public_methods$is_valid_field_name) <- function (call, env) {
    paste0(sQuote(eval(call$index, env)), " is not a valid field name in current class.")
}

on_failure(IDDObject$public_methods$is_valid_field_name) <- function (call, env) {
    paste0(sQuote(eval(call$name, env)), " is not a valid field name in current class.")
}

on_failure(IDDObject$public_methods$is_valid_field_num) <- function (call, env) {
    paste0(sQuote(eval(call$num, env)), " is not a valid field number in current class.")
}

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
parse_issue <- function (path = NULL, type = "", data_errors = NULL, info = NULL,
                         src = c("IDD", "IDF"), stop = TRUE, quote = TRUE) {
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

    if (is.null(path)) {
        header <- sprintf("%s PARSING ERROR", src)
    } else {
        header <- sprintf("%s PARSING ERROR for file %s", src, sQuote(path))
    }

    mes <- paste0("\n",
        sep_line("="), "\n",
        header, "\n",
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
