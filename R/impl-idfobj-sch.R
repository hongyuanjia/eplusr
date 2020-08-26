# DAYTYPE {{{
DAYTYPE <- list(
    Weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
    Weekend = c("Saturday", "Sunday"),
    DesignDay = c("SummerDesignDay", "WinterDesignDay"),
    CustomDay = c("CustomDay1", "CustomDay2"),
    Holiday = "Holiday"
)
# }}}

# parse_sch_cmpt {{{
parse_sch_cmpt <- function (dt_value) {
    l <- mark_sch_cmpt_field(dt_value)
    obj <- l$object
    val <- l$value

    # remove all "through", "for" and "interpolate"
    val <- val[!J(NA_integer_), on = "index_schedule_value"]

    # mark each schedule value pair
    val[, value_index := rleid(rleid, object_id, year_day, index_daytype, index_schedule_value)]
    val[, daytype_index := rleid(rleid, object_id, year_day, index_daytype)]

    # store meta and actual schedule data separately
    meta <- val[, by = "daytype_index", .SD[1L], .SDcols = c("rleid", "object_id", "year_day", "daytype", "interpolate")]
    setcolorder(meta, c("rleid", "object_id", "year_day", "daytype_index"))
    sch <- dcast.data.table(val, value_index + daytype_index ~ pair_type, value.var = "value_chr")

    # parse until {{{
    s_until <- sch$until
    if (any(invld <- !stri_startswith_fixed(s_until, "until"))) {
        invld <- val[J(which(invld)[[1L]]), on = "value_index"]
        abort_bad_sch_compt_field("until", "Invalid 'Until:' field value found",
            get_object_info(obj, numbered = FALSE, prefix = ""),
            invld$field_index, invld$value_chr, invld$field_name)
    }
    s_until <- stri_sub(s_until, 6L)
    s_until[stri_sub(s_until, 1L, 1L) == ":"] <- stri_trim_left(stri_sub(s_until[stri_sub(s_until, 1L, 1L) == ":"], 2L))
    # use difftime to parse
    until <- as.integer(as.difftime(as.character(s_until), format = "%H:%M", units = "mins"))
    # NOTE: as.difftime("1:00:00", format = "%H:%M") did not give NA
    if (anyNA(until) || any(stri_count_fixed(s_until, ":") != 1L)) {
        invld <- val[J(which(is.na(until) | stri_count_fixed(s_until, ":") != 1L)[[1L]]), on = "value_index"]
        abort_bad_sch_compt_field("until", "Invalid time format in 'Until:' field found",
            get_object_info(obj, numbered = FALSE, prefix = ""),
            invld$field_index, invld$value_chr, invld$field_name)
    }
    set(sch, NULL, "time", until)
    set(sch, NULL, "until", NULL)

    sch[, by = "daytype_index", {
        # check overlapping
        if (!identical(order(time), seq_len(.N))) {
            invld <- .BY$daytype_index
            invld <- val[J(invld), on = "daytype_index"]
            abort_bad_sch_compt_field("until", "Overlapped time found in 'Until:' fields",
                get_object_info(obj, numbered = FALSE, prefix = ""),
                invld$field_index, invld$value_chr, invld$field_name)
        }

        # check completeness
        if (time[.N] != 1440L) {
            invld <- .BY$daytype_index
            invld <- val[J(invld), on = "daytype_index"]
            abort_bad_sch_compt_field("until", "Incomplete day time specifications found in 'Until:' fields",
                get_object_info(obj, numbered = FALSE, prefix = ""),
                invld$field_index, invld$value_chr, invld$field_name)
        }
    }]
    # }}}

    # parse value {{{
    set(sch, NULL, "value", suppressWarnings(as.double(sch$value)))
    if (anyNA(sch$value)) {
        invld <- val[J(which(is.na(sch$value))[1L]), on = "value_index"]
        abort_bad_sch_compt_field("until", "Invalid schedule value found in 'Value:' fields",
            get_object_info(obj, numbered = FALSE, prefix = ""),
            invld$field_index, invld$value_chr, invld$field_name)
    }
    # }}}

    setcolorder(sch, c("value_index", "daytype_index", "time", "value"))

    list(type_limits = obj, meta = meta, value = sch)
}
# }}}

# mark_sch_cmpt_field {{{
mark_sch_cmpt_field <- function (dt_value) {
    # store object meta data
    obj <- dt_value[J(2L), on = "field_index", by = c("rleid", "object_id"),
        list(class_name = class_name[[1L]], object_name = object_name[[1L]],
            type_limits = value_chr)]

    # extract core schedule fields
    val <- dt_value[field_index >= 3L, .SD, .SDcols = c("rleid", "object_id", "field_index", "field_name", "value_chr")]

    # change all to lower case
    set(val, NULL, "value_chr", stri_trans_tolower(val$value_chr))

    # mark datetime rows
    set(val, NULL, "enum", 0L)

    setindexv(val, c("rleid", "object_id"))

    # mark through {{{
    val[, by = c("rleid", "object_id"), c("enum", "year_day") := {
        year_day <- rep(NA_integer_, .N)
        i_thr <- which(stri_startswith_fixed(value_chr, "through"))

        # make sure through appears at latest in the 3rd field
        if (!length(i_thr) || i_thr[[1L]] != 1L) {
            abort_bad_sch_compt_field("through", "Failed to locate 'Through:' fields",
                get_object_info(obj, numbered = FALSE, prefix = ""),
                field_index[1L], value_chr[1L], field_name[1L])
        }

        # extract date string
        s_date <- stri_sub(value_chr[i_thr], 8)
        s_date[stri_sub(s_date, 1L, 1L) == ":"] <- stri_sub(s_date[stri_sub(s_date, 1L, 1L) == ":"], 2L)
        s_date <- stri_trim_left(s_date)

        # convert to an EpwDate
        date <- epw_date(s_date, leapyear = FALSE)
        if (anyNA(date) || any(!is_epwdate_type(date, "md"))) {
            invld <- .SD[i_thr[is.na(date) | !is_epwdate_type(date, "md")]]
            abort_bad_sch_compt_field("through", "Invalid date format found in 'Through:' fields",
                get_object_info(obj, numbered = FALSE, prefix = ""),
                invld$field_index, invld$value_chr, invld$field_name)
        }

        # get year day
        yr_day <- lubridate::yday(date)
        if (!identical(order(yr_day), seq_along(yr_day))) {
            invld <- .SD[i_thr]
            abort_bad_sch_compt_field("through", "Invalid date order found in 'Through:' fields",
                get_object_info(obj, numbered = FALSE, prefix = ""),
                invld$field_index, invld$value_chr, invld$field_name)
        }
        if (!365L %in% yr_day) {
            invld <- .SD[i_thr]
            abort_bad_sch_compt_field("through", "Incomplete year found in 'Through:' fields",
                get_object_info(obj, numbered = FALSE, prefix = ""),
                invld$field_index, invld$value_chr, invld$field_name)
        }

        # make sure next field for a through field is a "for"
        if (any(invld <- !stri_startswith_fixed(value_chr[i_thr + 1L], "for"))) {
            invld <- .SD[unique(sort(c(i_thr[invld] - 1L, i_thr[invld])))]
            abort_bad_sch_compt_field("through", "Expected a 'For:' field below 'Through:' fields",
                get_object_info(obj, numbered = FALSE, prefix = ""),
                invld$field_index, invld$value_chr, invld$field_name)
        }

        year_day[i_thr] <- as.integer(yr_day)
        enum[i_thr] <- 1L
        list(enum = enum, year_day = data.table::nafill(year_day, "locf"))
    }]
    # }}}

    # mark for {{{
    val[, by = c("rleid", "object_id"), c("enum", "daytype", "index_daytype") := {
        i_for <- which(stri_startswith_fixed(value_chr, "for"))

        # extract day types
        s_type <- stri_sub(value_chr[i_for], 4L)
        s_type[stri_sub(s_type, 1L, 1L) == ":"] <- stri_sub(s_type[stri_sub(s_type, 1L, 1L) == ":"], 2L)
        s_type <- stri_split_fixed(stri_trim_left(s_type), " ")

        # parse day type
        daytype <- tryCatch(lapply(s_type, match_daytype, sch = TRUE),
            eplusr_error_invalid_daytype = function (e) {
                invld <- which(vlapply(s_type, function (tp) e$data %chin% tp))[1]
                invld <- .SD[i_for[invld]]
                abort_bad_sch_compt_field("for", paste0("Invalid day type (", collapse(e$data), ") found in 'For:' fields"),
                    get_object_info(obj, numbered = FALSE, prefix = ""),
                    invld$field_index, invld$value_chr, invld$field_name)
            }
        )

        # split by through period
        ind_per <- cumsum(enum[i_for - 1L])
        daytype_per <- split(daytype, ind_per)

        for (i_per in seq_along(daytype_per)) {
            daytype_flat <- unlist(daytype_per[[i_per]], FALSE, FALSE)
            # make sure no duplications per through period
            if (anyDuplicated(daytype_flat)) {
                invld <- i_for[ind_per == i_per]
                i_thr <- invld[1L] - 1L
                invld <- .SD[c(i_thr, invld)]
                abort_bad_sch_compt_field("for", paste0("Duplicated day assignment for (", collapse(daytype_flat[duplicated(daytype_flat)]), ") found in 'For:' fields"),
                    get_object_info(obj, numbered = FALSE, prefix = ""),
                    invld$field_index, invld$value_chr, invld$field_name)
            }

            # make sure all days are covered
            if (length(miss <- setdiff(unlist(DAYTYPE, FALSE, FALSE), daytype_flat))) {
                other <- vlapply(daytype_per[[i_per]], function (x) "AllOtherDay" %chin% x)
                if (!any(other)) {
                    invld <- i_for[ind_per == i_per]
                    i_thr <- invld[1L] - 1L
                    invld <- .SD[c(i_thr, invld)]
                    abort_bad_sch_compt_field("for", paste0("Missing assignment for day type (", collapse(miss), ") in 'For:' fields"),
                        get_object_info(obj, numbered = FALSE, prefix = ""),
                        invld$field_index, invld$value_chr, invld$field_name)
                }

                # expand allotherday
                daytype_per[[i_per]][other] <- list(c(setdiff(daytype_per[[i_per]][other][[1L]], "AllOtherDay"), miss))
            }
        }

        all_daytypes <- c(unlist(DAYTYPE, FALSE, FALSE), "Weekday", "Weekend", "AllDay", "AllOtherDay", "Holiday")
        daytype <- lapply(s_type, function (s) match_in_vec(gsub("s$", "", s), all_daytypes, label = TRUE))

        enum[i_for] <- 2L
        index_daytype <- rep(NA_integer_, .N)
        index_daytype[i_for] <- seq_along(i_for)
        index_daytype <- data.table::nafill(index_daytype, "locf")
        # first for under through
        index_daytype[1L] <- 1L

        list(enum = enum, daytype = daytype[index_daytype], index_daytype = index_daytype)
    }]
    # }}}

    # mark interpolate {{{
    i_int <- which(stri_startswith_fixed(val$value_chr, "interpolate"))
    # make sure next field to through is a for
    if (!length(i_int)) {
        # 'No' is the default
        set(val, NULL, "interpolate", "no")
    } else {
        if (any(not_for <- val$enum[i_int - 1L] != 2L)) {
            invld <- val[c(i_int[not_for][1L] - 1L, i_int[not_for][1L])]
            abort_bad_sch_compt_field("interpolate", "Expected a 'For' field before 'Interpolate:' fields",
                get_object_info(obj, numbered = FALSE, prefix = ""),
                invld$field_index, invld$value_chr, invld$field_name)
        }

        s_int <- stri_sub(val$value_chr[i_int], 12)
        s_int[stri_sub(s_int, 1L, 1L) == ":"] <- stri_trim_left(stri_sub(s_int[stri_sub(s_int, 1L, 1L) == ":"], 2L))

        all_int <- c("no", "linear", "average")
        if (anyNA(int <- chmatch(s_int, all_int))) {
            invld <- val[i_int[is.na(int)][1L]]
            abort_bad_sch_compt_field("interpolate",
                "Invalid value found in 'Interpolate:' fields",
                get_object_info(obj, numbered = FALSE, prefix = ""),
                invld$field_index, invld$value_chr, invld$field_name)
        }

        set(val, i_int, "enum", 3L)
        set(val, i_int, "interpolate", s_int)
        set(val, i_int, "int_idx", int)
        val[, by = c("year_day", "index_daytype"), int_idx := data.table::nafill(int_idx, "locf")]
        val[J(NA_integer_), on = "int_idx", int_idx := 1L]
        set(val, 1:(i_int[[1L]] - 1L), "int_idx", NA_integer_)
        set(val, NULL, "interpolate", all_int[val$int_idx])
        set(val, NULL, "int_idx", NULL)
    }
    # }}}

    # pair until and value {{{
    set(val, NULL, "pair_type", NA_character_)
    val[J(0L), on = "enum", by = c("rleid", "object_id", "year_day", "index_daytype"),
        c("pair_type", "index_schedule_value") := {
            is_until <- seq_len(.N) %% 2 == 1L

            if (sum(is_until) * 2L != .N) {
                abort_bad_sch_compt_field("until", "Number of 'Until:' fields does not match number of value fields",
                    get_object_info(obj, numbered = FALSE, prefix = ""),
                    field_index, value_chr, field_name)
            }

            pair_type[is_until] <- "until"
            pair_type[!is_until] <- "value"
            list(pair_type, index_schedule_value = rep(seq_len(.N/2), each = 2L))
        }
    ]
    # }}}

    list(object = obj, value = val)
}
# }}}

# abort_bad_sch_compt_field {{{
abort_bad_sch_compt_field <- function (type, reason, object, index, value, field) {
    abort(paste0(reason, " for ", object, "\n",
        paste0(sprintf("  --> %i: \"%s\", !- %s", index, value, field), collapse = "\n")
        ),
        paste0("idfschcmpt_", type),
        data = list(field_index = index, field_name = field, value_chr = value)
    )
}
# }}}

# compose_sch_cmpt {{{
compose_sch_cmpt <- function (type_limits, meta, value) {
    # Through
    set(meta, NULL, "through", paste("Through:", transform_sch_date(meta$year_day)))

    # For
    set(meta, NULL, "for", paste("For:",
        vcapply(meta$daytype, function (x) {
            ms <- x %chin% c("Weekday", "Weekend", "AllDay", "AllOtherDay")
            if (any(ms)) x[ms] <- paste0(x[ms], "s")
            paste(x, collapse = " ")
        })
    ))

    # Interpolate
    if (nrow(meta) == 1L) {
        set(meta, NULL, "interp", list(list(paste("Interpolate:", stringi::stri_trans_totitle(meta$interpolate)))))
    } else {
        set(meta, NULL, "interp", as.list(paste("Interpolate:", stringi::stri_trans_totitle(meta$interpolate))))
    }
    set(meta, which(meta$interpolate == "no"), "interp", list(list(NULL)))

    # Until
    set(value, NULL, "until", paste("Until:", stri_sub(format(data.table::as.ITime(value$time * 60)), to = -4L)))
    set(value, which(value$time == 1440L), "until", paste0("Until: 24:00"))

    # Until and Value pair
    set(value, NULL, "pair", apply2(value$until, value$value, c))

    # compact
    val <- value[, by = "daytype_index", list(value_chr = list(unlist(pair, FALSE, FALSE)))]

    val[meta, on = "daytype_index", by = .EACHI,
        `:=`(
            through = i.through,
            value_chr = list(c(i.for, unlist(i.interp), unlist(value_chr, FALSE, FALSE))),
            rleid = i.rleid, object_id = i.object_id
        )
    ]
    val[, by = "through", value_chr := {
        value_chr[[1L]] <- c(.BY$through, value_chr[[1L]])
        list(value_chr)
    }]

    # clean
    set(meta, NULL, c("through", "for", "interp"), NULL)
    set(value, NULL, c("until", "pair"), NULL)

    # in case type limit is not specified
    if (!has_names(type_limits, "type_limits")) {
        set(type_limits, NULL, "type_limits", NA_character_)
        on.exit(set(type_limits, NULL, "type_limits", NULL), add = TRUE)
    }

    val <- val[, by = c("rleid", "object_id"), list(value_chr = list(unlist(value_chr, FALSE, FALSE)))][
        type_limits, on = c("rleid", "object_id"), by = .EACHI, {
            value_chr <- c(i.object_name, i.type_limits, unlist(value_chr, FALSE, FALSE))
            field_index <- seq_along(value_chr)
            list(field_index = field_index, value_chr = value_chr, value_num = NA_real_)
        }
    ]

    list(object = type_limits[, .SD, .SDcols = 1:4], value = val)
}
# }}}

# compact_sch_cmpt {{{
compact_sch_cmpt <- function (meta, value) {
    meta <- compact_sch_daytype(meta, other_day = NULL)
    value <- expand_sch_time(meta, value)
    value_d <- dcast.data.table(value, daytype_index ~ time, value.var = "value")

    # find duplicated
    dup <- duplicated(value_d, by = setdiff(seq_along(value_d), 1L))

    # match duplicated daytype index
    uni_value_d <- split(value_d[!dup], by = "daytype_index", keep.by = FALSE)
    dup_value_d <- split(value_d[dup], by = "daytype_index", keep.by = FALSE)
    map <- lapply(uni_value_d, function (d) {
        idx <- as.integer(names(which(vlapply(dup_value_d, identical, d))))
        unlist(meta$daytype[idx], FALSE, FALSE)
    })
    map <- data.table(daytype_index = as.integer(names(map)), daytype = map)

    # update meta
    meta <- meta[map, on = "daytype_index", by = .EACHI, daytype := {
        list(list(c(daytype[[1L]], i.daytype[[1L]])))
    }]
    meta <- compact_sch_daytype(meta[J(map$daytype_index), on = "daytype_index"])

    # remove duplicated values
    value <- compact_sch_time(meta, value[J(map$daytype_index), on = "daytype_index"])

    list(meta = meta, value = value)
}
# }}}

# compact_sch_daytype {{{
compact_sch_daytype <- function (meta, other_day = c("CustomDay", "Holiday"), invert = FALSE) {
    if (!is.null(other_day)) {
        # expand
        meta <- expand_sch_daytype(meta, other_day = TRUE)

        # validate
        if (!invert) {
            s_end <- stri_endswith_fixed(other_day, "s", case_insensitive = TRUE)
            if (any(s_end)) other_day[s_end] <- stri_sub(other_day[s_end], to = -2L)
            cmpt <- match_in_vec(other_day, label = TRUE, names(DAYTYPE))
            iscmpt <- !is.na(cmpt)
            other_day <- as.list(other_day)
            other_day[iscmpt] <- DAYTYPE[names(DAYTYPE) %chin% unlist(other_day[iscmpt], FALSE, FALSE)]
            other_day[!iscmpt] <- lapply(other_day[!iscmpt], match_in_vec, unlist(DAYTYPE, FALSE, FALSE), label = TRUE)
        } else {
            other_day <- match_daytype(other_day, TRUE, FALSE)
            setattr(other_day, "names", other_day)
            other_day <- lapply(other_day, match_daytype, TRUE, TRUE)
        }
    }

    if (!invert) {
        dict <- c(list(AllDay = unlist(DAYTYPE, FALSE, FALSE)), DAYTYPE[c("Weekday", "Weekend")])
        other_day <- unlist(other_day, FALSE, FALSE)
    } else {
        if (is.null(other_day)) abort("'other_day' should be given when 'invert' is TRUE.")
        dict <- other_day[!names(other_day) %chin% "AllOtherDay"]
        other_day <- setdiff(c(unlist(DAYTYPE, FALSE, FALSE)), unlist(other_day, FALSE, FALSE))
    }

    found_otherday <- FALSE
    meta[, by = c("rleid", "object_id", "daytype_index"), "daytype" := {
        # validate
        daytype <- lapply(daytype, match_daytype, sch = TRUE)

        if (length(other_day)) dict <- c(dict, list(AllOtherDay = other_day))

        for (i in seq_along(dict)) {
            if (all(dict[[i]] %chin% daytype[[1L]])) {
                # make sure AllOtherDay is at the end
                if ("AllOtherDay" %chin% names(dict[i])) {
                    found_otherday <<- TRUE
                    daytype <- list(c(setdiff(daytype[[1L]], dict[[i]]), names(dict[i])))
                } else {
                    daytype <- list(c(names(dict[i]), setdiff(daytype[[1L]], dict[[i]])))
                }
            }
        }

        # keep the input order
        dict <- dict[names(dict) != "AllOtherDay"]
        daytype <- list(c(intersect(names(dict), daytype[[1L]]), setdiff(daytype[[1L]], names(dict))))

        list(daytype)
    }]

    if (invert) {

        # still put those 3 day types into AllOtherDay
        if (!found_otherday && all(c("CustomDay1", "CustomDay2") %chin% other_day)) {
            other_day <- c("CustomDay1", "CustomDay2")
            meta[, by = c("rleid", "object_id", "daytype_index"), "daytype" := {
                if (all(other_day %chin% daytype[[1L]])) {
                    daytype <- list(c(setdiff(daytype[[1L]], other_day), "AllOtherDay"))
                    found_otherday <<- TRUE
                }

                list(daytype)
            }]
        }

        # compact all days in the same index of AllOtherDay
        if (found_otherday) {
            dict <- dict[names(dict) != "AllOtherDay"]
            meta[vlapply(daytype, function (x) "AllOtherDay" %chin% x), "daytype" := {
                list(list(c(intersect(names(dict), daytype[[1L]]), "AllOtherDay")))
            }]
        }
    }
    meta
}
# }}}

# expand_sch_daytype {{{
expand_sch_daytype <- function (meta, other_day = TRUE) {
    meta[, by = c("rleid", "object_id", "daytype_index"), "daytype" := {
        list(lapply(daytype, match_daytype, sch = TRUE))
    }]

    meta[, by = c("rleid", "object_id"), "daytype" := {
        daytype_flat <- unlist(daytype, FALSE, FALSE)

        # make sure all days are covered
        if (length(miss <- setdiff(unlist(DAYTYPE, FALSE, FALSE), daytype_flat))) {
            other <- vlapply(daytype, function (x) "AllOtherDay" %chin% x)
            if (!any(other)) {
                abort(paste0("Missing assignment for day type (", collapse(miss), ")"), "idfschcmpt_daytype")
            }

            # expand allotherday
            if (other_day) daytype[other] <- list(c(setdiff(daytype[other][[1L]], "AllOtherDay"), miss))
        }

        list(daytype)
    }]

    meta
}
# }}}

# compact_sch_time {{{
compact_sch_time <- function (meta, value, timestep = "auto") {
    if (timestep == "auto") {
        # add interpolate
        value[meta, on = "daytype_index", interpolate := i.interpolate]
        on.exit(set(value, NULL, "interpolate", NULL), add = TRUE)

        value[!J("linear"), on = "interpolate", by = "daytype_index", time_rleid := rleid(value)]
        value[J("linear"), on = "interpolate", by = "daytype_index",
            time_rleid := {
                d <- round(diff(value), 8L)
                if (d[[1L]] == 0.0) {
                    rleid(c(d[[1L]], d))
                } else {
                    # just to make sure the first one is different
                    rleid(c(d[[1L]] - 1L, d))
                }
            }
        ]
        val_cmpt <- value[, by = c("daytype_index", "time_rleid"), .SD[.N], .SDcols = c("value", "time")]
    } else {
        ts <- parse_sch_timestep(timestep)

        value[, by = "daytype_index", time_rleid := {
            dt <- c(time[[1L]], diff(time))
            if (!all(i <- ts >= dt & ts %% dt == 0L)) {
                abort(paste0("Invalid timestep specification: ", surround(timestep), ". ",
                    "It must be divisible by ", collapse(max(unique(dt[!i])), NULL), " mins."),
                    "idfschcmpt_timestep"
                )
            }
            mod <- cumsum(dt) %% ts
            div <- cumsum(dt) %/% ts
            div[mod == 0L] <- div[mod == 0L] - 1L
            div + 1L
        }]

        val_cmpt <- value[, by = c("daytype_index", "time_rleid"), list(value = round(mean(value), 8L), time = time[.N])]
    }

    set(value, NULL, "time_rleid", NULL)
    set(val_cmpt, NULL, "time_rleid", NULL)
    set(val_cmpt, NULL, "value_index", seq_len(nrow(val_cmpt)))
    setcolorder(val_cmpt, c("value_index", "daytype_index", "time", "value"))

    val_cmpt
}
# }}}

# expand_sch_time {{{
expand_sch_time <- function (meta, value, timestep = "auto") {
    if (timestep != "auto") ts <- parse_sch_timestep(timestep)

    # add interpolate
    value[meta, on = "daytype_index", interpolate := i.interpolate]
    on.exit(set(value, NULL, "interpolate", NULL), add = TRUE)

    # find the minimal timestep
    full_time <- value[, by = "daytype_index", {
        if (.N == 1L && time == 1440L) {
            # default to 1 min
            if (timestep == "auto") {
                list(value = value, time = 1L:1440L)
            } else {
                if (1440 %% ts != 0L) {
                    abort(paste0("Invalid timestep specification: ", surround(timestep), ". ",
                        "It should be a divisor of 1440 mins."),
                        "idfschcmpt_timestep"
                    )
                }
                ts <- as.integer(ts)
                list(value = value, time = seq(ts, 1440L, by = ts))
            }
        } else {
            d <- min(diff(time))
            gcd <- min(gcd(time, d))

            if (timestep == "auto") {
                ts <- gcd
            } else if (ts == 0 || ts > gcd || gcd %% ts != 0L) {
                abort(paste0("Invalid timestep specification: ", surround(timestep), ". ",
                    "It should be a divisor of ", gcd, " mins."),
                    "idfschcmpt_timestep"
                )
            }

            ts <- as.integer(ts)

            if (interpolate[[1L]] != "linear") {
                val <- NA_real_
            # assume first period is flat
            } else {
                steps <- as.integer(c(time[[1L]] / ts, diff(time) / ts + 1L))
                val <- mapply(seq, from = c(value[[1L]], value[-.N]), to = value, length.out = steps, SIMPLIFY = FALSE)
                val <- unlist(c(val[1L], lapply(val[-1L], "[", -1L)))
            }

            list(value = val, time = seq(ts, 1440L, by = ts))
        }
    }]

    val_exp <- value[full_time, on = c("daytype_index", "time"), roll = -Inf]
    val_exp[!J(NA_real_), on = "i.value", value := i.value]
    set(val_exp, NULL, c("i.value", "interpolate"), NULL)

    set(val_exp, NULL, "value_index", seq_len(nrow(val_exp)))
    setcolorder(val_exp, c("value_index", "daytype_index", "time", "value"))

    val_exp
}
# }}}

# get_sch_type_limits {{{
get_sch_type_limits <- function (idd_env, idf_env, name) {
    assert_string(name)

    range <- tryCatch(
        get_idf_value(idd_env, idf_env, class = "ScheduleTypeLimits", object = name, field = 4L,
            complete = TRUE, ignore_case = TRUE),
        eplusr_error_invalid_object_name = function (e) {
            msg <- sprintf("Invalid object name found in class 'ScheduleTypeLimits': '%s'", e$value)
            abort(msg, "invalid_object_name")
        }
    )
    range <- standardize_idf_value(idd_env, idf_env, range, type = "choice")

    type <- range$value_chr[4]
    # empty then use continuous default
    if (is.na(type)) type <- "Continuous"

    if (type == "Continuous") {
        lower_incbounds <- TRUE
        upper_incbounds <- TRUE
        if (is.na(range$value_num[2])) {
            range$value_num[2] <- -Inf
            lower_incbounds <- FALSE
        }
        if (is.na(range$value_num[3])) {
            range$value_num[3] <- Inf
            upper_incbounds <- FALSE
        }
        r <- ranger(range$value_num[2], lower_incbounds, range$value_num[3], upper_incbounds)
    } else if (type == "Discrete") {
        if (is.na(range$value_num[2]) || is.na(range$value_num[3])) {
            r <- list()
        } else if (range$value_num[2] > range$value_num[3]) {
            abort(sprintf("Invalid 'Lower Limit Value' (%s) for ScheduleTypeLimits object '%s' [ID:%i]. Should be no larger than 'Upper Limit Value' (%s)",
                range$value_num[2], range$object_name[1], range$object_id[1], range$value_num[3]),
                "idfschcmpt_typelimit"
            )
        } else {
            r <- seq(range$value_num[2], range$value_num[3])
        }
    } else {
        r <- list()
    }

    list(name = range$object_name[[1L]], range = r)
}
# }}}

# validate_sch_cmpt {{{
validate_sch_cmpt <- function (idd_env, idf_env, object, type_limits, level = eplusr_option("validate_level")) {
    if (is.null(type_limits)) {
        lim <- NULL
    } else {
        lim <- get_sch_type_limits(idd_env, idf_env, type_limits)
    }

    prop <- c("type_enum", "units", "ip_units")

    if (is.numeric(lim$range)) {
        prop <- c(prop, "choice")
    } else if (inherits(lim$range, "Range")) {
        prop <- c(prop, "has_range", "maximum", "minimum", "lower_incbounds", "upper_incbounds")
    }

    obj <- get_idf_object(idd_env, idf_env, object = object)
    val <- get_idf_value(idd_env, idf_env, object = object, property = prop)

    # make all visible fields required
    set(val, NULL, "required_field", TRUE)

    m <- tryCatch(mark_sch_cmpt_field(val),
        eplusr_error_idfschcmpt_through = function (e) e,
        eplusr_error_idfschcmpt_for = function (e) e,
        eplusr_error_idfschcmpt_interpolate = function (e) e,
        eplusr_error_idfschcmpt_until = function (e) e
    )

    if (!inherits(m, "eplusr_error")) {
        val[m$value, on = "field_index", pair_type := i.pair_type]

        if (is.numeric(lim$range)) {
            val[J("value"), on = "pair_type", `:=`(type_enum = IDDFIELD_TYPE$choice, choice = list(lim$range))]
        } else if (inherits(lim$range, "Range")) {
            val[J("value"), on = "pair_type", `:=`(type_enum = IDDFIELD_TYPE$real,
                has_range = TRUE,
                maximum = lim$range$maximum, minimum = lim$range$minimum,
                lower_incbounds = lim$range$lower_incbounds, upper_incbounds = lim$range$upper_incbounds,
                value_num = suppressWarnings(as.double(value_chr))
            )]
        }
    }

    validity <- validate_on_level(idd_env, idf_env, obj, val, level)

    # add schedule type limits checking
    if (is.na(val$value_chr[2])) {
        invld <- val[J(2L), on = "field_index"]
        setattr(invld, "bullet", "Failed to locate schedule type limits:")
        validity$invalid_schedule_type_limits <- invld
    }

    if (inherits(m, "eplusr_error")) {
        type <- stringi::stri_replace_first_fixed(class(m)[1L], "eplusr_error_idfschcmpt", "invalid")
        type <- paste0(type, "_field")

        invld <- val[J(m$data$field_index), on = "field_index"]

        # add additional message
        mes <- stri_extract_first_regex(m$message, ".+field")
        setattr(invld, "bullet", mes)

        validity[[type]] <- invld
    }

    validity
}
# }}}

# preprocess_sch_compt_data {{{
preprocess_sch_compt_data <- function (data, type_limits = NULL) {
    assert_data_frame(data, any.missing = FALSE, min.cols = 4L)
    assert_names(names(data), must.include = c("year_day", "daytype", "time", "value"))
    assert_multi_class(data$year_day, c("character", "Date", "EpwDate", "numeric", "integer"))
    assert_multi_class(data$time, c("character", "numeric", "difftime", "hms", "ITime"))
    assert_multi_class(data$value, c("integer", "numeric"))

    data <- as.data.table(data)

    # check year_day
    year_day <- data$year_day
    if (is.character(year_day) || is.numeric(year_day)) {
        year_day <- epw_date(year_day, leapyear = FALSE)
    }
    year_day <- lubridate::yday(year_day)
    year_day <- assert_integerish(year_day, lower = 1L, upper = 365, any.missing = FALSE,
        coerce = TRUE, .var.name = "data$year_day"
    )
    set(data, NULL, "year_day", year_day)

    # check time
    time <- data$time
    if (is.character(time)) {
        time <- as.difftime(time, format = "%H:%M", units = "mins")
    } else if (inherits(time, "hms")) {
        time <- as.integer(time) / 60L
    } else if (inherits(time, "difftime")) {
        units(time) <- "mins"
        time <- as.integer(time)
    } else if (inherits(time, "ITime")) {
        time <- as.integer(time) / 60L
    }
    if (!checkmate::test_integerish(time, lower = 1L, upper = 1440L, any.missing = FALSE)) {
        invld <- is.na(time) | time < 1L | time > 1440L
        abort(paste0("'time' should be or at least can be converted into 'HH:MM' format for one day. ",
            "Invalid time specification found: ", collapse(unique(data$time[invld]))), "idfschcmpt_until")
    }
    set(data, NULL, "time", as.integer(time))

    dt <- unique(data$daytype)
    if (!any(stri_detect_fixed(dt, ","))) {
        daytype <- match_daytype(data$daytype, TRUE, FALSE, FALSE)
        if (anyNA(daytype)) {
            abort(paste0("Invalid day type found: ", collapse(unique(data$daytype[is.na(daytype)]))), "idfschcmpt_for")
        }
        set(data, NULL, "daytype", daytype)
    } else {
        dt_s <- stringi::stri_split_regex(dt, "\\s*,\\s*", omit_empty = TRUE, simplify = FALSE)
        dt_s <- lapply(dt_s, function (s) {
            m <- match_daytype(s, sch = TRUE, expand = FALSE, stop = FALSE)
            if (anyNA(m)) {
                abort(paste0("Invalid day type found: ", collapse(s[is.na(m)])), "idfschcmpt_for")
            }
            m
        })
        dt <- data.table(daytype = dt, daytype_split = dt_s, group_id = seq_along(dt))

        # in case there is an 'id' column
        if (!has_names(data, "id")) set(data, NULL, "id", 1L)

        # keep original daytype order
        data <- data[dt, on = "daytype", `:=`(daytype_split = i.daytype_split, group_id = i.group_id)]
        data <- data[, by = "id", {
            len <- each_length(daytype_split)
            list(year_day = rep(year_day, len),
                 daytype = unlist(daytype_split, FALSE, FALSE),
                 time = rep(time, len),
                 value = rep(value, len),
                 group_id = rep(group_id, len)
            )
        }][J(unique(unlist(dt_s, FALSE, FALSE))), on = "daytype"]

        set(data, NULL, "id", rleid(data$id, data$group_id))
        set(data, NULL, "group_id", NULL)
    }

    # check range
    if (!is.null(type_limits)) {
        if (is.numeric(type_limits$range)) {
            invld <- data[!value %in% type_limits$range]

            if (nrow(invld)) {
                abort(paste0("Invalid schedule value found (", collapse(unique(invld$value)), "). ",
                    "Should be a subset of ", collapse(type_limits$range), " as defined in ScheduleTypeLimits ",
                    surround(type_limits$name), "."), "idfschcmpt_value")
            }
        } else if (inherits(type_limits$range, "Range")) {
            invld <- data[!in_range(value, type_limits$range)]
            if (nrow(invld)) {
                abort(paste0("Invalid schedule value found (", collapse(unique(invld$value)), "). ",
                    "Should be in range ", format(type_limits$range), " as defined in ScheduleTypeLimits ",
                    surround(type_limits$name), "."), "idfschcmpt_value")
            }
        }
    }

    # check id
    if (has_names(data, "id")) {
        set(data, NULL, "id", assert_integerish(data$id, any.missing = FALSE, sorted = TRUE, coerce = TRUE))

        meta <- data[, by = c("id", "year_day"), list(daytype = list(unique(daytype)))]

        # check if values are the same when grouped by id
        data <- data[, list(num = .N, daytype = list(unique(daytype))), by = c("id", "year_day", "time", "value")][, by = c("id", "year_day"),
        {
            if (length(unique(num)) > 1L) {
                abort(sprintf("Invalid 'id' (%i) found. Schedule data of day type %s mismatch.",
                    .BY$id, collapse(daytype[[1L]])), "idfschcmpt_id")
            }
            .SD
        }]
        set(data, NULL, c("num", "daytype"), NULL)

        set(meta, NULL, "daytype_index", rleid(meta$id, meta$year_day))
        set(data, NULL, "daytype_index", rleid(data$id, data$year_day))
    } else {
        set(data, NULL, "daytype_index", rleid(data$year_day, data$daytype))
        meta <- unique(data, by = "daytype_index")
        set(meta, NULL, setdiff(names(meta), c("year_day", "daytype_index", "daytype")), NULL)
    }
    set(data, NULL, "value_index", seq_len(nrow(data)))

    if (ncol(meta) > 3L) set(meta, NULL, setdiff(names(meta), c("year_day", "daytype_index", "daytype")), NULL)
    # currently only 'No' interpolate is supported here
    set(meta, NULL, "interpolate", "no")
    if (ncol(data) > 4L) set(data, NULL, setdiff(names(data), c("value_index", "daytype_index", "time", "value")), NULL)

    list(meta = meta, value = data)
}
# }}}

# update_sch_compt {{{
#' @importFrom checkmate assert_multi_class
update_sch_compt <- function (idd_env, idf_env, type_limits, meta, value, data, check_range = TRUE, compact = TRUE) {
    assert_flag(check_range)
    assert_flag(compact)

    limits <- NULL
    if (check_range) {
        if (is.null(type_limits$type_limits)) {
            verbose_info("'.check_range' is set to 'TRUE' but 'Schedule Type Limits' is not set. ",
                "No range checking on schedule values will be performed.")
        } else {
            limits <- get_sch_type_limits(idd_env, idf_env, type_limits$type_limits)
        }
    }
    pre <- preprocess_sch_compt_data(data, limits)

    set(pre$meta, NULL, "rleid", 1L)
    set(pre$meta, NULL, "object_id", type_limits$object_id)

    l <- compose_sch_cmpt(type_limits, pre$meta, pre$value)
    set(l$value, NULL, "class_name", "Schedule:Compact")
    set(l$value, NULL, "object_name", l$object$object_name)

    fld <- get_idd_field(idd_env, type_limits$class_name, l$value[, by = c("rleid", "object_id"), list(num = max(field_index))]$num, complete = TRUE)
    set(l$value, NULL, "field_name", fld$field_name)

    # syntax checking
    parsed <- parse_sch_cmpt(l$value)

    if (compact) {
        cmpt <- compact_sch_cmpt(parsed$meta, parsed$value)
        parsed$meta <- cmpt$meta
        parsed$value <- cmpt$value
    }

    parsed
}
# }}}

# match_daytype {{{
match_daytype <- function (daytype, sch = FALSE, expand = TRUE, stop = TRUE) {
    # for SQL
    if (!sch) {
        s_end <- stri_endswith_fixed(daytype, "s", case_insensitive = TRUE)
        if (any(s_end)) daytype[s_end] <- stri_sub(daytype[s_end], to = -2L)
        dt <- match_in_vec(daytype, label = TRUE, c(
            unique(c(unlist(DAYTYPE, FALSE, FALSE), names(DAYTYPE))),
            "SpecialDay", "NormalDay"
        ))
    } else {
        s_end <- stri_endswith_fixed(daytype, "s", case_insensitive = TRUE)
        if (any(s_end)) daytype[s_end] <- stri_sub(daytype[s_end], to = -2L)

        all_daytype_cmpt <- c(unlist(DAYTYPE, FALSE, FALSE),
            c("Weekday", "Weekend", "AllDay", "AllOtherDay", "Holiday")
        )
        dt <- match_in_vec(daytype, label = TRUE, c(
            unlist(DAYTYPE, FALSE, FALSE),
            "Weekday", "Weekend", "Holiday", "AllDay", "AllOtherDay"
        ))
    }

    if (stop && anyNA(dt)) {
        abort(paste0("Invalid day type found: ", collapse(unique(daytype[is.na(dt)])), "."),
            "invalid_daytype", data = daytype[is.na(dt)])
    }

    if (!expand) return(dt)

    # expand
    expd <- c()

    if ("Weekday" %chin% dt) expd <- c(expd, DAYTYPE$Weekday)
    if ("Weekend" %chin% dt) expd <- c(expd, DAYTYPE$Weekend)
    if (!sch) {
        if ("DesignDay" %chin% dt) expd <- c(expd, DAYTYPE$DesignDay)
        if ("CustomDay" %chin% dt) expd <- c(expd, DAYTYPE$CustomDay)
        if ("SpecialDay" %chin% dt) expd <- c(expd, DAYTYPE$DesignDay, DAYTYPE$CustomDay, DAYTYPE$Holiday)
        if ("NormalDay" %chin% dt) expd <- c(expd, DAYTYPE$Weekday, DAYTYPE$Weekend)
        # setdiff will remove duplicates
        dict <- c("Weekday", "Weekend", "DesignDay", "CustomDay", "SpecialDay", "NormalDay")
        dt <- dt[chmatch(dt, dict, 0L) == 0L]
    } else {
        if ("AllDay" %chin% dt) expd <- c(expd, unlist(DAYTYPE, FALSE, FALSE))
        dict <- c("Weekday", "Weekend", "AllDay")
        dt <- dt[chmatch(dt, dict, 0L) == 0L]
    }

    c(dt, expd)
}
# }}}

# parse_sch_timestep {{{
# adopted from scales:::fullseq.difftime()
parse_sch_timestep <- function (timestep) {
    assert_string(timestep)
    s <- stri_split_fixed(timestep, " ")[[1L]]
    if (length(s) == 1L) abort(paste0("Invalid timestep specification: ", surround(timestep)), "idfschcmpt_timestep")

    value <- suppressWarnings(as.numeric(s[[1L]]))
    if (is.na(value)) abort(paste0("Invalid timestep specification: ", surround(timestep)), "idfschcmpt_timestep")

    unit <- gsub("s$", "", s[[2L]])
    unit_in_sec <- c("sec" = 1L, "min" = 60L, "hour" = 3600L)
    unit <- unit_in_sec[unit]
    if (is.na(unit)) abort(paste0("Invalid timestep specification: ", surround(timestep)), "idfschcmpt_timestep")
    if (unit %% 60L != 0L) abort(paste0("Invalid timestep specification: ", surround(timestep)), "idfschcmpt_timestep")

    # converted into minutes
    value * (unit %/% 60L)
}
# }}}

# transform_sch_date {{{
transform_sch_date <- function (date, leapyear = FALSE) {
    if (is.integer(date)) {
        set_epwdate_year(epw_date(date, leapyear = leapyear),
            if (leapyear) EPWDATE_YEAR$leap$md else EPWDATE_YEAR$noleap$md
        )
    } else {
        lubridate::yday(epw_date(date, leapyear = leapyear))
    }
}
# }}}

# gcd {{{
# ref: https://stackoverflow.com/questions/21502181/finding-the-gcd-without-looping-r
gcd <- function(x, y) {
    r <- x%%y
    # return(data.table::fifelse(r, gcd(y, r), y))
    return(ifelse(r, gcd(y, r), y))
}
# }}}
