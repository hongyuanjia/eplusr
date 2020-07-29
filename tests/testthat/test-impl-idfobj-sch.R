context("IdfSchedule")

# IdfSchedule class{{{
test_that("IdfSchedule class", {
    eplusr_option(verbose_info = FALSE)
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_idf <- file.path(eplus_config(8.8)$dir, "ExampleFiles/RefBldgMediumOfficeNew2004_Chicago.idf")
    idf <- read_idf(path_idf)

    idd_env <- get_priv_env(idf)$idd_env()
    idf_env <- get_priv_env(idf)$idf_env()

    val <- get_idf_value(idd_env, idf_env, object = "BLDG_SWH_SCH")
    l <- parse_sch_cmpt(val)

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
            interpolate = c("no", "no", "no")
        )
    )
    expect_equal(nrow(l$value), 44L)

    # can expand schedule frequency
    expect(nrow(expand_sch_time(l$meta, l$value)), 72)
    expect_equal(names(expand_sch_time(l$meta, l$value)), c("value_index", "daytype_index", "time", "value"))
    expect(nrow(expand_sch_time(l$meta, l$value, "10 mins")), 144 * 3)
    expect(nrow(expand_sch_time(l$meta, l$value, "1 hour")), 24 * 3)
    expect_equal(names(expand_sch_time(l$meta, l$value, "1 hour")), c("value_index", "daytype_index", "time", "value"))
    expect_error(expand_sch_time(l$meta, l$value, "0 mins"))
    expect_error(expand_sch_time(l$meta, l$value, "1.5 hours"))

    # can compact schedule frequency
    expect_error(compact_sch_time(l$meta, l$value, "1 hour"))
    expect_is(val_exp <- expand_sch_time(l$meta, l$value, "10 mins"), "data.table")
    expect_equivalent(compact_sch_time(l$meta, copy(val_exp)), l$value)
    expect_equal(nrow(compact_sch_time(l$meta, copy(val_exp), "1 hour")), 24 * 3)
    expect_equal(names(compact_sch_time(l$meta, copy(val_exp), "1 hour")), c("value_index", "daytype_index", "time", "value"))

    # can expand schedule day types
    expect_equal(expand_sch_daytype(copy(l$meta))$daytype,
        list(
            c("SummerDesignDay", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
            c("Saturday", "WinterDesignDay"),
            c("Sunday", "Holiday", "CustomDay1", "CustomDay2")
        )
    )

    # can compact schedule day types
    expect_equal(compact_sch_daytype(expand_sch_daytype(copy(l$meta)), c("CustomDay", "Holiday"))$daytype,
        list(
            c("Weekday", "SummerDesignDay"),
            c("Saturday", "WinterDesignDay"),
            c("Sunday", "AllOtherDay")
        )
    )
    expect_equal(compact_sch_daytype(expand_sch_daytype(copy(l$meta)), c("CustomDay", "Holiday", "Sunday"))$daytype,
        list(
            c("Weekday", "SummerDesignDay"),
            c("Saturday", "WinterDesignDay"),
            c("AllOtherDay")
        )
    )
})
# }}}
