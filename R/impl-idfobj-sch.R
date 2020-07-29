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
    if (anyNA(until)) {
        invld <- val[J(which(is.na(until))[[1L]]), on = "value_index"]
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
        abort_bad_sch_compt_field("until", "Incomplete day time specifications found in 'Until:' fields",
            get_object_info(obj, numbered = FALSE, prefix = ""),
            invld$field_index, invld$value_chr, invld$field_name)
    }
    # }}}

    setcolorder(sch, c("value_index", "daytype_index", "time", "value"))

    list(type_limits = obj, meta = meta, value = sch)
}
# }}}

# abort_bad_sch_compt_field {{{
abort_bad_sch_compt_field <- function (type, reason, object, index, value, field) {
    abort(paste0(reason, " for ", object, "\n",
        paste0(sprintf("  #%i: \"%s\", !- %s", index, value, field), collapse = "\n")
        ),
        paste0("idfschcmpt_", type)
    )
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
        if (length(i_thr) == 0L || i_thr[[1L]] != 1L) {
            abort_bad_sch_compt_field("through", "Failed to locate 'Through:' fields",
                get_object_info(obj, numbered = FALSE, prefix = ""),
                field_index[1L], value_chr[1L], field_name[1L])
        }

        # extract date string
        s_date <- stri_sub(value_chr[i_thr], 8)
        s_date[stri_sub(s_date, 1L, 1L) == ":"] <- stri_sub(s_date[stri_sub(s_date, 1L, 1L) == ":"], 2L)
        s_date <- stri_trim_left(s_date)

        # convert to an EpwDate
        yr_day <- lubridate::yday(epw_date(s_date, leapyear = FALSE))

        if (anyNA(yr_day)) {
            invld <- .SD[i_thr[is.na(yr_day)]]
            abort_bad_sch_compt_field("through", "Invalid date format found in 'Through:' fields",
                get_object_info(obj, numbered = FALSE, prefix = ""),
                invld$field_index, invld$value_chr, invld$field_name)
        }
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
            invld <- .SD[sort(c(i_thr[invld] - 1L, i_thr[invld]))]
            abort_bad_sch_compt_field("through", "Expected a 'For:' field below 'Through:' field",
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

        if (!length(i_for)) abort_bad_sch_compt_field(obj, val, "for", "Failed to locate 'For:' fields", c(.BY, .SD))

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
            invld <- val[i_int[not_for][1L]]
            abort_bad_sch_compt_field("interpolate", "Expected a 'For' field before 'Interpolate:' field",
                get_object_info(obj, numbered = FALSE, prefix = ""),
                invld$field_index, invld$value_chr, invld$field_name)
        }

        s_int <- stri_sub(val$value_chr[i_int], 12)
        s_int[stri_sub(s_int, 1L, 1L) == ":"] <- stri_trim_left(stri_sub(s_int[stri_sub(s_int, 1L, 1L) == ":"], 2L))

        all_int <- c("no", "linear", "average")
        if (anyNA(int <- chmatch(s_int, all_int))) {
            invld <- val[i_int[is.na(int)][1L]]
            abort_bad_sch_compt_field("interpolate", "Expected a 'For' field before 'Interpolate:' field",
                get_object_info(obj, numbered = FALSE, prefix = ""),
                invld$field_index, invld$value_chr, invld$field_name)
        }

        set(val, i_int, "enum", 3L)
        set(val, i_int, "interpolate", s_int)
        set(val, i_int, "int_idx", int)
        data.table::setnafill(val, "locf", cols = "int_idx")
        set(val, NULL, "interpolate", all_int[val$int_idx])
        set(val, NULL, "int_idx", NULL)
    }
    # }}}

    # pair until and value {{{
    set(val, NULL, "pair_type", "value")
    val[J(0L), on = "enum", by = c("rleid", "object_id", "year_day", "index_daytype"),
        c("pair_type", "index_schedule_value") := {
            is_until <- seq_len(.N) %% 2 == 1L

            if (sum(is_until) * 2L != .N) {
                abort_bad_sch_compt_field("until", "Number of 'Until:' fields does not match number of value fields",
                    get_object_info(obj, numbered = FALSE, prefix = ""),
                    field_index, value_chr, field_name)
            }

            pair_type[is_until] <- "until"
            list(pair_type, index_schedule_value = rep(seq_len(.N/2), each = 2L))
        }
    ]
    # }}}

    list(object = obj, value = val)
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
    if (!nrow(val)) {
        set(val, NULL, c("rleid", "object_id"), list(integer(), integer()))
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
compact_sch_daytype <- function (meta, other_day = c("CustomDay", "Holiday")) {
    if (!is.null(other_day)) {
        # expand
        meta <- expand_sch_daytype(meta, other_day = TRUE)

        # validate
        s_end <- stri_endswith_fixed(other_day, "s", case_insensitive = TRUE)
        if (any(s_end)) other_day[s_end] <- stri_sub(other_day[s_end], to = -2L)
        cmpt <- match_in_vec(other_day, label = TRUE, names(DAYTYPE))
        iscmpt <- !is.na(cmpt)
        other_day <- as.list(other_day)
        other_day[iscmpt] <- DAYTYPE[names(DAYTYPE) %chin% unlist(other_day[iscmpt], FALSE, FALSE)]
        other_day[!iscmpt] <- lapply(other_day[!iscmpt], match_in_vec, unlist(DAYTYPE, FALSE, FALSE), label = TRUE)
        other_day <- unlist(other_day, FALSE, FALSE)
    }

    meta[, by = c("rleid", "object_id", "daytype_index"), "daytype" := {
        # validate
        daytype <- lapply(daytype, match_daytype, sch = TRUE)

        dict <- c(list(AllDay = unlist(DAYTYPE, FALSE, FALSE)), DAYTYPE[c("Weekday", "Weekend")])
        if (!is.null(other_day)) dict <- c(list(AllOtherDay = other_day), dict)

        for (i in seq_along(dict)) {
            if (all(dict[[i]] %chin% daytype[[1L]])) {
                if ("AllOtherDay" %chin% names(dict[i])) {
                    daytype <- list(c(setdiff(daytype[[1L]], dict[[i]]), names(dict[i])))
                } else {
                    daytype <- list(c(names(dict[i]), setdiff(daytype[[1L]], dict[[i]])))
                }
            }
        }

        list(daytype)
    }]

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
                stop(paste0("Missing assignment for day type (", collapse(miss), ")"))
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
    if (timestep != "auto") ts <- parse_sch_timestep(timestep)

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
        value[, by = "daytype_index", time_rleid := {
            dt <- c(time[[1L]], diff(time))
            if (!all(i <- ts >= dt & ts %% dt == 0L)) {
                stop("Invalid timestep specification: ", surround(timestep), ". ",
                    "It must be divisible by ", collapse(max(unique(dt[!i])), NULL), " mins.")
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
        # default to 1 hour
        if (.N == 1L && time == 1440L) {
            list(value = value, time = seq(60L, 1440L, by = 60L))
        } else {
            d <- min(diff(time))
            gcd <- min(gcd(time, d))

            if (timestep == "auto") {
                ts <- gcd
            } else if (ts == 0 || ts > gcd || gcd %% ts != 0L) {
                stop("Invalid timestep specification: ", surround(timestep), ". ",
                    "It should be a divisor of ", gcd, " mins.")
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
    range <- tryCatch(
        get_idf_value(idd_env, idf_env, class = "ScheduleTypeLimits", object = name, field = 4L,
            complete = TRUE, ignore_case = TRUE),
        eplusr_error_invalid_object_name = function (e) {
            msg <- sprintf("Invalid object name found in class 'ScheduleTypeLimits': '%s'", e$value)
            abort(msg, "invaid_object_name")
        }
    )
    range <- standardize_idf_value(idd_env, idf_env, range, type = "choice")

    type <- range$value_chr[4]
    if (type == "Continuous") {
        r <- ranger(range$value_num[2], TRUE, range$value_num[3], TRUE)
    } else if (type == "Discrete") {
        r <- seq(range$value_num[2], range$value_num[3])
    } else {
        r <- list()
    }

    list(name = range$object_name[[1L]], range = r)
}
# }}}

# validate_sch_cmpt {{{
validate_sch_cmpt <- function (idd_env, idf_env, object, type_limits) {
    lim <- get_sch_type_limits(idd_env, idf_env, type_limits)

    prop <- c("type_enum", "required_field")

    if (is.numeric(lim$range)) {
        choice <- TRUE
        range <- FALSE
        prop <- c(prop, "choice")
    } else {
        choice <- FALSE
        range <- TRUE
        prop <- c(prop, "has_range", "maximum", "minimum", "lower_incbounds", "upper_incbounds")
    }

    obj <- get_idf_object(idd_env, idf_env, object = object)
    val <- get_idf_value(idd_env, idf_env, object = object, property = prop)

    m <- mark_sch_cmpt_field(val)
    val[m$value, on = "field_index", pair_type := i.pair_type]

    if (is.numeric(lim$range)) {
        val[J("value"), on = "pair_type", `:=`(type_enum = IDDFIELD_TYPE$choice, choice = list(lim$range))]
    } else if (inherits(lim$range, "Range")) {
        val[J("value"), on = "pair_type", `:=`(type_enum = IDDFIELD_TYPE$choice,
            has_range = TRUE,
            maximum = lim$range$maximum, minimum = lim$range$minimum,
            lower_incbounds = lim$range$lower_incbounds, upper_incbounds = lim$range$upper_incbounds
        )]
    }

    validate_objects(idd_env, idf_env, obj, val,
        required_field = TRUE, unique_name = TRUE, type = TRUE,
        choice = choice, range = range
    )
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
        abort("'time' should be or at least can be converted into 'HH:MM' format.")
    }
    set(data, NULL, "time", as.integer(time))

    daytype <- match_daytype(data$daytype, TRUE, FALSE)
    if (anyNA(daytype)) {
        abort(paste0("Invalid day type (", collapse(data$daytype[is.na(daytype)]), ") found"))
    }
    set(data, NULL, "daytype", daytype)

    # check range
    if (!is.null(type_limits)) {
        if (is.numeric(type_limits$range)) {
            invld <- data[!value %in% type_limits$range]

            if (nrow(invld)) {
                abort(paste0("Invalid schedule value found (", collapse(unique(invld$value)), "). ",
                    "Should be a subset of ", collapse(type_limits$range), " as defined in ScheduleTypeLimits ",
                    surround(type_limits$name), "."))
            }
        } else if (inherits(type_limits$range, "Range")) {
            invld <- data[!in_range(value, type_limits$range)]
            if (nrow(invld)) {
                abort(paste0("Invalid schedule value found (", collapse(unique(invld$value)), "). ",
                    "Should be in range ", format(type_limits$range), " as defined in ScheduleTypeLimits ",
                    surround(type_limits$name), "."))
            }
        }
    }

    # check id
    if (has_names(data, "id")) {
        set(data, NULL, "id", assert_integerish(data$id, any.missing = FALSE, sorted = TRUE, coerce = TRUE))

        meta <- data[, by = c("id", "year_day"), list(daytype = list(unique(daytype)))]

        # check if values are the same when grouped by id
        data <- unique(data, by = c("id", "year_day", "time", "value"))[, by = c("id", "year_day"), {
            dayt <- unique(daytype)
            if (length(dayt) > 1L) {
                abort(sprintf("Invalid 'id' (%i) found. Schedule data of 'daytype' %s mismatch.", .BY$id, collapse(dayt)))
            }
            .SD
        }]

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
    if (check_range && !is.null(type_limits$type_limits)) {
        limits <- get_sch_type_limits(idd_env, idf_env, type_limits$type_limits)
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
match_daytype <- function (daytype, sch = FALSE, expand = TRUE) {
    # for SQL
    if (!sch) {
        dt <- match_in_vec(daytype, label = TRUE, c(
            unique(unlist(DAYTYPE, FALSE, FALSE), names(DAYTYPE)),
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

    if (anyNA(dt)) {
        abort(paste0("Invalid day type found: ", collapse(daytype[is.na(dt)]), "."), "invalid_daytype", data = daytype[is.na(dt)])
    }

    if (!expand) return(dt)

    # expand
    expd <- c()

    if ("Weekday" %chin% dt) expd <- c(expd, DAYTYPE$Weekday)
    if ("Weekend" %chin% dt) expd <- c(expd, DAYTYPE$Weekend)
    if (!sch) {
        if ("DesignDay" %chin% dt) expd <- c(expd, DAYTYPE$DesignDay)
        if ("CustomDay" %chin% dt) expd <- c(expd, DAYTYPE$CustomDay)
        if ("SpecialDay" %chin% dt) expd <- c(expd, DAYTYPE$SpecialDay)
        if ("NormalDay" %chin% dt) expd <- c(expd, DAYTYPE$NormalDay)
        dt <- setdiff(dt, c("Weekday", "Weekend", "DesignDay", "CustomDay", "SpecialDay", "NormalDay"))
    } else {
        if ("AllDay" %chin% dt) expd <- c(expd, unlist(DAYTYPE, FALSE, FALSE))
        dt <- setdiff(dt, c("Weekday", "Weekend", "AllDay"))
    }

    if (sch) c(dt, expd) else unique(c(dt, expd))
}
# }}}

# parse_sch_timestep {{{
# adopted from scales:::fullseq.difftime()
parse_sch_timestep <- function (timestep) {
    s <- stri_split_fixed(timestep, " ")[[1L]]
    if (length(s) == 1L) stop("Invalid timestep specification: ", surround(timestep))

    value <- suppressWarnings(as.numeric(s[[1L]]))
    if (is.na(value)) stop("Invalid timestep specification: ", surround(timestep))

    unit <- gsub("s$", "", s[[2L]])
    unit_in_sec <- c("sec" = 1L, "min" = 60L, "hour" = 3600L)
    unit <- unit_in_sec[unit]
    if (is.na(unit)) stop("Invalid timestep specification: ", surround(timestep))
    if (unit %% 60L != 0L) stop("Invalid timestep specification: ", surround(timestep))

    # converted into minutes
    value * (unit %/% 60L)
}
# }}}

# transform_sch_date {{{
transform_sch_date <- function (date, leapyear = FALSE) {
    if (is.integer(date)) {
        set_epwdate_year(epw_date(date, leapyear = leapyear),
            if (leapyear) EPWDATE_YEAR$leap$md else EPWDATE_YEAR$noleap$md
        )[]
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
