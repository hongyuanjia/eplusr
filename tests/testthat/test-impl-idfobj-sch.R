eplusr_option(verbose_info = FALSE)

# PARSE_SCH_CMPT {{{
test_that("parse_sch_cmpt()", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_idf <- file.path(eplus_config(8.8)$dir, "ExampleFiles/RefBldgMediumOfficeNew2004_Chicago.idf")
    idf <- read_idf(path_idf)
    idd_env <- get_priv_env(idf)$idd_env()
    idf_env <- get_priv_env(idf)$idf_env()

    val <- get_idf_value(idd_env, idf_env, object = "BLDG_SWH_SCH")
    expect_is(l <- parse_sch_cmpt(val), "list")

    val <- rbindlist(list(val[1:4], val[4][, `:=`(value_chr = "Interpolate: Linear")], val[5:.N]))
    expect_is(l <- parse_sch_cmpt(val), "list")

    expect_equal(names(l), c("type_limits", "meta", "value"))
    expect_equal(l$type_limits,
        data.table(rleid = 1L, object_id = 595L, class_name = "Schedule:Compact", 
            object_name = "BLDG_SWH_SCH", type_limits = "Fraction")
    )
    expect_equal(l$meta,
        data.table(
            rleid = c(1L, 1L, 1L),
            object_id = c(595L, 595L, 595L),
            year_day = c(365L, 365L, 365L),
            daytype_index = 1:3,
            daytype = list(c("Weekday", "SummerDesignDay"), c("Saturday", "WinterDesignDay"), c("Sunday", "Holiday", "AllOtherDay")),
            interpolate = c("linear", "no", "no")
        )
    )
    expect_equal(nrow(l$value), 44L)

    # can stop if failed to locate first through field
    expect_error(parse_sch_cmpt(copy(val)[3L, value_chr := "12/31"]), class = "eplusr_error_idfschcmpt_through")

    new_val <- init_idf_value(idd_env, idf_env, "Schedule:Compact", 188, complete = TRUE)[, field_in := NULL]
    new_val[1:95, value_chr := val$value_chr[1:95]]
    new_val[96:.N, value_chr := val$value_chr[3:95]]
    new_val[3L, value_chr := "Through: 12/1"]

    # can stop if invalid through date
    expect_error(parse_sch_cmpt(copy(new_val)[96L, value_chr := "Through: 31"]), class = "eplusr_error_idfschcmpt_through")

    # can stop if invalid order of through dates
    expect_error(parse_sch_cmpt(copy(new_val)[96L, value_chr := "Through: 1/1"]), class = "eplusr_error_idfschcmpt_through")

    # can stop if incompete year in through dates
    expect_error(parse_sch_cmpt(copy(new_val)[96L, value_chr := "Through: 12/30"]), class = "eplusr_error_idfschcmpt_through")

    # can stop if through field is not followed by a for field
    expect_error(parse_sch_cmpt(copy(new_val)[4L, value_chr := "Through: 12/30"]), class = "eplusr_error_idfschcmpt_through")

    # can stop if invalid day types
    expect_error(parse_sch_cmpt(copy(val)[4L, value_chr := "For: Someday"]), class = "eplusr_error_idfschcmpt_for")

    # can stop if duplicated day types
    expect_error(parse_sch_cmpt(copy(val)[4L, value_chr := "For: Monday Monday"]), class = "eplusr_error_idfschcmpt_for")

    # can stop if missing day types
    expect_error(parse_sch_cmpt(copy(val)[77L, value_chr := "For: Sunday Holiday"]), class = "eplusr_error_idfschcmpt_for")

    # can stop if invalid position of interpolate
    expect_error(parse_sch_cmpt(copy(val)[6L, value_chr := "Interpolate: No"]), class = "eplusr_error_idfschcmpt_interpolate")

    # can stop if invalid value of interpolate
    expect_error(parse_sch_cmpt(copy(val)[5L, value_chr := "Interpolate: a"]), class = "eplusr_error_idfschcmpt_interpolate")

    # can stop if mismatch number of until and value
    expect_error(parse_sch_cmpt(copy(val)[-6L]), class = "eplusr_error_idfschcmpt_until")

    # can stop if invalid until field
    expect_error(parse_sch_cmpt(copy(val)[8L, value_chr := "un"]), class = "eplusr_error_idfschcmpt_until")

    # can stop if invalid until time specs
    expect_error(parse_sch_cmpt(copy(val)[8L, value_chr := "until: 1:00:00"]), class = "eplusr_error_idfschcmpt_until")

    # can stop if until time overlaps
    expect_error(parse_sch_cmpt(copy(val)[8L, value_chr := "until: 1:00"]), class = "eplusr_error_idfschcmpt_until")

    # can stop if incomplete time in until
    expect_error(parse_sch_cmpt(copy(val)[42L, value_chr := "until: 23:00"]), class = "eplusr_error_idfschcmpt_until")

    # can stop if invalid value
    expect_error(parse_sch_cmpt(copy(val)[7L, value_chr := "a"]), class = "eplusr_error_idfschcmpt_until")
})
# }}}

# COMPOSE_SCH_CMPT {{{
test_that("compose_sch_cmpt()", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_idf <- file.path(eplus_config(8.8)$dir, "ExampleFiles/RefBldgMediumOfficeNew2004_Chicago.idf")
    idf <- read_idf(path_idf)
    idd_env <- get_priv_env(idf)$idd_env()
    idf_env <- get_priv_env(idf)$idf_env()
    val <- get_idf_value(idd_env, idf_env, object = "BLDG_SWH_SCH")
    val <- rbindlist(list(val[1:4], val[4][, `:=`(value_chr = "Interpolate: Linear")], val[5:.N]))

    expect_is(l <- parse_sch_cmpt(val), "list")

    expect_is(cmpt <- compose_sch_cmpt(l$type_limits, l$meta, l$value), "list")
    expect_equal(names(cmpt), c("object", "value"))
    expect_equal(cmpt$object,
        data.table(rleid = 1L, object_id = 595L, class_name = "Schedule:Compact",
            object_name = "BLDG_SWH_SCH")
    )
    # original is "Holidays", also 0.20 becomes 0.2
    expect_equal(cmpt$value$value_chr[-c(62, 77)], val$value_chr[-c(62, 77)])

    # can work with single row meta
    expect_is(cmpt <- compose_sch_cmpt(l$type_limits, l$meta[1L], l$value[daytype_index == 1L]), "list")

    # can work with empty type limits
    expect_is(cmpt <- compose_sch_cmpt(l$type_limits[, -"type_limits"], l$meta[1L], l$value), "list")
})
# }}}

# COMPACT_SCH_DAYTYPE {{{
test_that("compact_sch_daytype()", {
    meta <- data.table(
        rleid = 1L, object_id = 595L, year_day = 365L, daytype_index = 1:3,
        daytype = list(
            c("Weekday", "SummerDesignDay"),
            c("Saturday", "WinterDesignDay"),
            c("Sunday", "Holiday", "AllOtherDay")),
        interpolate = c("linear", "no", "no")
    )

    expect_equal(compact_sch_daytype(copy(meta), c("CustomDay", "Holidays"))$daytype,
        list(
            c("Weekday", "SummerDesignDay"),
            c("Saturday", "WinterDesignDay"),
            c("Sunday", "AllOtherDay")
        )
    )

    expect_error(compact_sch_daytype(copy(meta), NULL, TRUE))

    expect_equal(compact_sch_daytype(copy(meta), c("Weekday", "Weekends"), TRUE)$daytype,
        list(
            c("Weekday", "SummerDesignDay"),
            c("Saturday", "WinterDesignDay"),
            c("AllOtherDay")
        )
    )
})
# }}}

# EXPAND_SCH_DAYTYPE {{{
test_that("expand_sch_daytype()", {
    meta <- data.table(
        rleid = 1L, object_id = 595L, year_day = 365L, daytype_index = 1:3,
        daytype = list(
            c("Weekday", "SummerDesignDay"),
            c("Saturday", "WinterDesignDay"),
            c("Sunday", "Holiday", "AllOtherDay")),
        interpolate = c("linear", "no", "no")
    )

    expect_equal(expand_sch_daytype(copy(meta))$daytype,
        list(
            c("SummerDesignDay", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
            c("Saturday", "WinterDesignDay"),
            c("Sunday", "Holiday", "CustomDay1", "CustomDay2")
        )
    )

    expect_equal(expand_sch_daytype(copy(meta), FALSE)$daytype,
        list(
            c("SummerDesignDay", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
            c("Saturday", "WinterDesignDay"),
            c("Sunday", "Holiday", "AllOtherDay")
        )
    )

    expect_error(expand_sch_daytype(copy(meta)[3L, `:=`(daytype = list(daytype[[1L]][1L]))]),
        class = "eplusr_error_idfschcmpt_daytype"
    )
})
# }}}

# COMPACT_SCH_TIME {{{
test_that("compact_sch_time()", {
    meta <- data.table(
        rleid = 1L, object_id = 595L, year_day = 365L, daytype_index = 1:2,
        daytype = list(
            c("Weekday", "SummerDesignDay"),
            c("AllOtherDay")),
        interpolate = c("linear", "average")
    )

    value <- data.table(
        value_index = 1:22,
        daytype_index = c(rep(1L, 20L), rep(2L, 2)),
        time = c(seq(300L, 1440L, 60L), 900L, 1440L),
        value = rep(seq(0.1, 1.0, length.out = 11), 2L)
    )
    expect_equal(compact_sch_time(meta, value),
        data.table(
            value_index = 1:6,
            daytype_index = c(1L, 1L, 1L, 1L, 2L, 2L),
            time = c(300L, 900L, 960L, 1440L, 900L, 1440L),
            value = c(0.1, 1.0, 0.1, 0.82, 0.91, 1.0)
        )
    )

    expect_error(compact_sch_time(meta, value, "3 min"), class = "eplusr_error_idfschcmpt_timestep")

    value <- data.table(
        value_index = 1:22L,
        daytype_index = c(rep(1L, 11L), rep(2L, 11L)),
        time = rep(c(seq(300L, 1440L, 120L), 1440L), 2L),
        value = c(rep(1:11, each = 2))
    )
    expect_equal(nrow(compact_sch_time(meta, value, "600 min")), 6L)

    value <- data.table(
        value_index = 1:22,
        daytype_index = c(rep(1L, 20L), rep(2L, 2)),
        time = c(seq(300L, 1440L, 60L), 900L, 1440L),
        value = rep(c(0.1, seq(0.1, 1.0, length.out = 10)), 2L)
    )
    expect_equal(compact_sch_time(meta, value),
        data.table(
            value_index = 1:7,
            daytype_index = c(1L, 1L, 1L, 1L, 1L, 2L, 2L),
            time = c(360L, 900L, 960L, 1020L, 1440L, 900L, 1440L),
            value = c(0.1, 1.0, 0.1, 0.1, 0.8, 0.9, 1.0)
        )
    )

})
# }}}

# EXPAND_SCH_TIME {{{
test_that("expand_sch_time()", {
    meta <- data.table(
        rleid = 1L, object_id = 595L, year_day = 365L, daytype_index = 1:2,
        daytype = list(
            c("Weekday", "SummerDesignDay"),
            c("AllOtherDay")),
        interpolate = c("linear", "average")
    )

    value <- data.table(
        value_index = 1:22,
        daytype_index = c(rep(1L, 11L), rep(2L, 11)),
        time = rep(c(seq(300L, 1440L, 120L), 1440L), 2L),
        value = rep(c(seq(0.1, 1.0, length.out = 10), 1.0), 2L)
    )

    expect_error(expand_sch_time(meta, value, "7 hour"), class = "eplusr_error_idfschcmpt_timestep")

    # can expand schedule frequency
    expect_equal(nrow(expand_sch_time(meta, value)), 48)
    expect_equal(expand_sch_time(meta, value)$time, rep(seq(60L, 1440L, 60L), 2L))
    expect_equal(expand_sch_time(meta, value)[daytype_index == 1L, value], c(rep(0.10, 4), seq(0.1, 1.0, 0.05), 1.0))
    expect_equal(expand_sch_time(meta, value)[daytype_index == 2L, value], c(rep(0.10, 3), rep(seq(0.1, 1.0, 0.1), each = 2), 1.0))

    value <- data.table(
        value_index = 1:12,
        daytype_index = c(1L, rep(2L, 11L)),
        time = c(1440L, seq(300L, 1440L, 120L), 1440L),
        value = c(0.1, seq(0.1, 1.0, length.out = 10), 1.0)
    )

    expect_error(expand_sch_time(meta, value, "7 hour"), class = "eplusr_error_idfschcmpt_timestep")

    expect_equal(nrow(expand_sch_time(meta, value)), 1464)
    expect_equal(expand_sch_time(meta, value)$time, c(1:1440, seq(60L, 1440L, 60L)))
    expect_equal(expand_sch_time(meta, value)[daytype_index == 1L, value], rep(0.1, 1440L))
    expect_equal(expand_sch_time(meta, value)[daytype_index == 2L, value], c(rep(0.10, 3), rep(seq(0.1, 1.0, 0.1), each = 2), 1.0))

    expect_equal(nrow(expand_sch_time(meta, value, "1 hour")), 48L)
    expect_equal(expand_sch_time(meta, value, "1 hour")$time, rep(seq(60L, 1440L, 60L), 2))
    expect_equal(expand_sch_time(meta, value, "1 hour")[daytype_index == 1L, value], rep(0.1, 24))
    expect_equal(expand_sch_time(meta, value, "1 hour")[daytype_index == 2L, value], c(rep(0.10, 3), rep(seq(0.1, 1.0, 0.1), each = 2), 1.0))
})
# }}}

# GET_SCH_TYPE_LIMITS {{{
test_that("get_sch_type_limits()", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_idf <- file.path(eplus_config(8.8)$dir, "ExampleFiles/RefBldgMediumOfficeNew2004_Chicago.idf")
    idf <- read_idf(path_idf)
    idd_env <- get_priv_env(idf)$idd_env()
    idf_env <- get_priv_env(idf)$idf_env()

    expect_error(get_sch_type_limits(idd_env, idf_env, "a"), class = "eplusr_error_invalid_object_name")

    expect_equal(get_sch_type_limits(idd_env, idf_env, "fraction"),
        list(name = "Fraction", range = ranger(0, TRUE, 1, TRUE))
    )

    expect_is(obj <- idf$add(ScheduleTypeLimits = list("lim"))[[1L]], "IdfObject")
    expect_equal(get_sch_type_limits(idd_env, idf_env, "lim"),
        list(name = "lim", range = ranger())
    )

    idf$definition("ScheduleTypeLimits")
    expect_is(idf$add(ScheduleTypeLimits = list("lim1", ..4 = "discrete")), "list")
    expect_equal(get_sch_type_limits(idd_env, idf_env, "lim1"), list(name = "lim1", range = list()))

    expect_is(idf$add(ScheduleTypeLimits = list("lim2", 1, 5, "discrete")), "list")
    expect_equal(get_sch_type_limits(idd_env, idf_env, "lim2"), list(name = "lim2", range = 1:5))

    expect_is(idf$add(ScheduleTypeLimits = list("lim3", 2, 1, "discrete")), "list")
    expect_error(get_sch_type_limits(idd_env, idf_env, "lim3"), class = "eplusr_error_idfschcmpt_typelimit")

    expect_is(without_checking(idf$add(ScheduleTypeLimits = list("lim4", 1, 2, "integer"))), "list")
    expect_equal(get_sch_type_limits(idd_env, idf_env, "lim4"), list(name = "lim4", range = list()))
})
# }}}
