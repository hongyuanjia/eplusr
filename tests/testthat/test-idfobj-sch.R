context("IdfSchedule")

eplusr_option(verbose_info = FALSE)

# NEW {{{
test_that("NEW", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_idf <- file.path(eplus_config(8.8)$dir, "ExampleFiles/RefBldgLargeOfficeNew2004_Chicago.idf")
    expect_is(idf <- read_idf(path_idf), "Idf")

    expect_error(IdfScheduleCompact$new(""), class = "eplusr_error_idfobject_missing_parent")
    expect_error(IdfScheduleCompact$new("a", idf), class = "eplusr_error_invalid_object_name")

    expect_is(IdfScheduleCompact$new("sch", idf, new = TRUE), "IdfScheduleCompact")
    expect_is(schedule_compact(idf, "sch1", new = TRUE), "IdfScheduleCompact")

    expect_is(IdfScheduleCompact$new("bldg_occ_sch", idf), "IdfScheduleCompact")
    expect_is(schedule_compact(idf, "bldg_occ_sch"), "IdfScheduleCompact")
})
# }}}

# TYPELIMITS {{{
test_that("TYPELIMITS", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_idf <- file.path(eplus_config(8.8)$dir, "ExampleFiles/RefBldgLargeOfficeNew2004_Chicago.idf")
    expect_is(idf <- read_idf(path_idf), "Idf")

    expect_is(sch <- IdfScheduleCompact$new("sch", idf, new = TRUE), "IdfScheduleCompact")
    expect_null(sch$type_limits())
    expect_error(sch$type_limits(""), class = "eplusr_error_invalid_object_name")
    expect_equal(sch$type_limits("any number"), list(name = "Any Number", range = ranger()))

    expect_is(occu <- IdfScheduleCompact$new("bldg_occ_sch", idf), "IdfScheduleCompact")
    expect_equal(occu$type_limits(), list(name = "Fraction", range = ranger(0, TRUE, 1, TRUE)))
    expect_equal(occu$type_limits("any number"), list(name = "Any Number", range = ranger()))
})
# }}}

# SET {{{
test_that("SET", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_idf <- file.path(eplus_config(8.8)$dir, "ExampleFiles/RefBldgLargeOfficeNew2004_Chicago.idf")

    expect_is(idf <- read_idf(path_idf), "Idf")
    expect_is(sch <- IdfScheduleCompact$new("sch", idf, new = TRUE), "IdfScheduleCompact")

    # can stop if invalid day type
    expect_error(sch$set(c(1) := list(..24 = 0)), class = "eplusr_error_idfschcmpt_daytype")
    # can stop if no time specs is given
    expect_error(sch$set(allday := list(0)), class = "eplusr_error_idfschcmpt_time")
    # can stop if invalid value
    expect_error(sch$set(allday := list(..24 = "a")), class = "eplusr_error_idfschcmpt_value")

    expect_message(with_verbose(sch$set(c("weekday", "summerdesignday") := list(
        ..6 = 0.2, "8:00" = 0.5,
        ..12 = 0.95, "13:30" = 0.6, ..14 = 0.8,
        ..18 = 0.95, ..19 = 0.2, ..24 = 0),
        allotherday = list(..24 = 0)
    )), "No range checking")
    expect_equal(sch$value(simplify = TRUE),
        c("sch",
          NA,
          "Through: 12/31",
          "For: Weekdays SummerDesignDay",
          "Until: 06:00",
          "0.2",
          "Until: 08:00",
          "0.5",
          "Until: 12:00",
          "0.95",
          "Until: 13:30",
          "0.6",
          "Until: 14:00",
          "0.8",
          "Until: 18:00",
          "0.95",
          "Until: 19:00",
          "0.2",
          "Until: 24:00",
          "0",
          "For: AllOtherDays",
          "Until: 24:00",
          "0"
        )
    )

    # can stop if incomplete day type
    expect_error(sch$set(c("weekday", "summerdesignday") := list(
        ..6 = 0.2, "8:00" = 0.5,
        ..12 = 0.95, "13:30" = 0.6, ..14 = 0.8,
        ..18 = 0.95, ..19 = 0.2, ..24 = 0)
    ), class = "eplusr_error_idfschcmpt_for")

    # can stop if invalid time specification
    expect_error(sch$set(c("weekday", "summerdesignday") := list(
        ..0 = 0.2, "8:00" = 0.5,
        ..12 = 0.95, "13:30" = 0.6, ..14 = 0.8,
        ..18 = 0.95, ..19 = 0.2, ..24 = 0)
    ), class = "eplusr_error_idfschcmpt_until")
    expect_error(sch$set(c("weekday", "summerdesignday") := list(
        ..1 = 0.2, "8:00" = 0.5,
        ..12 = 0.95, "13:30" = 0.6, ..14 = 0.8,
        ..18 = 0.95, ..19 = 0.2, ..26 = 0)
    ), class = "eplusr_error_idfschcmpt_until")

    # can stop if invalid daytype
    expect_error(sch$set("someday" := list(
        ..6 = 0.2, "8:00" = 0.5,
        ..12 = 0.95, "13:30" = 0.6, ..14 = 0.8,
        ..18 = 0.95, ..19 = 0.2, ..24 = 0),
        allotherday = list(..24 = 0)
    ), class = "eplusr_error_idfschcmpt_for")

    # can stop if invalid value range
    expect_equal(sch$type_limits("fraction"), list(name = "Fraction", range = ranger(0, TRUE, 1, TRUE)))
    expect_error(sch$set("weekday" := list(
        ..6 = 2, "8:00" = 0.5,
        ..12 = 0.95, "13:30" = 0.6, ..14 = 0.8,
        ..18 = 0.95, ..19 = 0.2, ..24 = 0),
        allotherday = list(..24 = 0)
    ), class = "eplusr_error_idfschcmpt_value")
})
# }}}

# UPDATE {{{
test_that("UPDATE", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_idf <- file.path(eplus_config(8.8)$dir, "ExampleFiles/RefBldgLargeOfficeNew2004_Chicago.idf")

    expect_is(idf <- read_idf(path_idf), "Idf")
    expect_is(sch <- IdfScheduleCompact$new("sch", idf, new = TRUE), "IdfScheduleCompact")

    # simple case
    val <- data.table(year_day = 365, daytype = "AllDay", time = "24:00", value = 0)
    expect_is(sch$update(val), "IdfScheduleCompact")

    # can work with compacted daytypes
    val1 <- data.table(year_day = "12/31",
        daytype = "weekday, summerdesignday",
        time = c("6:00", "8:00", "12:00", "13:30", "14:00", "18:00", "19:00", "24:00"),
        value = c(0.2,    0.5,    0.95,    0.6,     0.8,     0.95,    0.2,     0.0)
    )
    val2 <- data.table(year_day = "12/31", daytype = "allotherday", time = "24:00", value = 0.0)
    val <- rbindlist(list(val1, val2))
    expect_is(sch$update(val), "IdfScheduleCompact")
    expect_equal(sch$value(simplify = TRUE),
        c("sch",
          NA,
          "Through: 12/31",
          "For: Weekdays SummerDesignDay",
          "Until: 06:00",
          "0.2",
          "Until: 08:00",
          "0.5",
          "Until: 12:00",
          "0.95",
          "Until: 13:30",
          "0.6",
          "Until: 14:00",
          "0.8",
          "Until: 18:00",
          "0.95",
          "Until: 19:00",
          "0.2",
          "Until: 24:00",
          "0",
          "For: AllOtherDays",
          "Until: 24:00",
          "0"
        )
    )

    # can work with id column
    val1 <- data.table(id = 1L, year_day = "12/31",
        daytype = "weekday",
        time = c("6:00", "8:00", "12:00", "13:30", "14:00", "18:00", "19:00", "24:00"),
        value = c(0.2,    0.5,    0.95,    0.6,     0.8,     0.95,    0.2,     0.0)
    )
    val2 <- copy(val1)[, daytype := "summerdesignday"]
    val3 <- data.table(id = 2L, year_day = "12/31", daytype = "allotherday", time = "24:00", value = 0.0)
    val <- rbindlist(list(val1, val2, val3))
    expect_is(sch$update(val), "IdfScheduleCompact")
    expect_equal(sch$value(simplify = TRUE),
        c("sch",
          NA,
          "Through: 12/31",
          "For: Weekdays SummerDesignDay",
          "Until: 06:00",
          "0.2",
          "Until: 08:00",
          "0.5",
          "Until: 12:00",
          "0.95",
          "Until: 13:30",
          "0.6",
          "Until: 14:00",
          "0.8",
          "Until: 18:00",
          "0.95",
          "Until: 19:00",
          "0.2",
          "Until: 24:00",
          "0",
          "For: AllOtherDays",
          "Until: 24:00",
          "0"
        )
    )


    # can stop if values in groups differ
    val1 <- data.table(id = 1L, year_day = "12/31",
        daytype = "weekday",
        time = c("6:00", "8:00", "12:00", "13:30", "14:00", "18:00", "19:00", "24:00"),
        value = c(0.2,    0.5,    0.95,    0.6,     0.8,     0.95,    0.2,     0.0)
    )
    val2 <- copy(val1)[, daytype := "summerdesignday"]
    val3 <- data.table(id = 1L, year_day = "12/31", daytype = "allotherday", time = "24:00", value = 0.0)
    val <- rbindlist(list(val1, val2, val3))
    expect_error(sch$update(val), class = "eplusr_error_idfschcmpt_id")
})
# }}}

# EXTRACT {{{
test_that("EXTRACT", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_idf <- file.path(eplus_config(8.8)$dir, "ExampleFiles/RefBldgLargeOfficeNew2004_Chicago.idf")

    expect_is(idf <- read_idf(path_idf), "Idf")
    expect_is(sch <- IdfScheduleCompact$new("sch", idf, new = TRUE), "IdfScheduleCompact")
    expect_null(sch$extract())
    expect_is(sch$set(
        c("weekday", "summerdesignday") := list(
            ..6 = 0.2, "8:00" = 0.5,
            ..12 = 0.95, "13:30" = 0.6, ..14 = 0.8,
            ..18 = 0.95, ..19 = 0.2, ..24 = 0),
        allotherday = list(..24 = 0)),
    "IdfScheduleCompact")

    expect_error(sch$extract(1), class = "eplusr_error_idfschcmpt_daytype")

    # can keep original day types
    expect_equal(sch$extract(),
        data.table(
            year_day = "12/31",
            id = c(rep(1L, 8), 2L),
            daytype = c(rep("Weekday,SummerDesignDay", 8), "AllOtherDay"),
            time = hms::hms(hours = c(6L, 8L, 12:14, 18:19, 24L, 24L), minutes = c(rep(0, 3), 30, rep(0, 5))),
            value = c(0.2, 0.5, 0.95, 0.6, 0.8, 0.95, 0.2, 0.0, 0.0)
        )
    )
    expect_equal(names(sch$extract()), c("year_day", "id", "daytype", "time", "value"))
    expect_equal(unique(sch$extract()$daytype), c("Weekday,SummerDesignDay", "AllOtherDay"))

    # can expand day types
    expect_equal(nrow(sch$extract(daytype = TRUE)), 54L)
    expect_equal(names(sch$extract(daytype = TRUE)), c("year_day", "id", "daytype", "time", "value"))
    expect_equal(unique(sch$extract(daytype = TRUE)$daytype), unlist(DAYTYPE, FALSE, FALSE))
    expect_equal(nrow(sch$extract(daytype = "expand")), 54L)
    expect_equal(names(sch$extract(daytype = TRUE)), c("year_day", "id", "daytype", "time", "value"))
    expect_equal(unique(sch$extract(daytype = "expand")$daytype), unlist(DAYTYPE, FALSE, FALSE))

    # can compact day types
    expect_equal(nrow(sch$extract(daytype = FALSE)), 19L)
    expect_equal(names(sch$extract(daytype = FALSE)), c("year_day", "id", "daytype", "time", "value"))
    expect_equal(unique(sch$extract(daytype = FALSE)$daytype),
        c("Weekday", "SummerDesignDay", "Weekend", "WinterDesignDay", "AllOtherDay"))
    expect_equal(nrow(sch$extract(daytype = "compact")), 19L)
    expect_equal(names(sch$extract(daytype = "compact")), c("year_day", "id", "daytype", "time", "value"))
    expect_equal(unique(sch$extract(daytype = "compact")$daytype),
        c("Weekday", "SummerDesignDay", "Weekend", "WinterDesignDay", "AllOtherDay"))

    # can compact using input
    expect_equal(nrow(d <- sch$extract(daytype = c("weekdays", "summerdesignday", "winterdesignday"))), 18L)
    expect_equal(unique(d$daytype), c("Weekday", "SummerDesignDay", "WinterDesignDay", "AllOtherDay"))

    # can keep all day types
    expect_equal(nrow(d <- sch$extract(daytype = "weekdays")), 17L)
    expect_equal(unique(d$daytype), c("Weekday", "SummerDesignDay", "AllOtherDay"))
    expect_equal(nrow(d <- sch$extract(daytype = c("weekdays", "weekend"))), 18L)
    expect_equal(unique(d$daytype), c("Weekday", "SummerDesignDay", "Weekend", "AllOtherDay"))

    # can expand time
    expect_equal(sch$extract(timestep = "auto"),
        data.table(
            year_day = "12/31",
            id = c(rep(1L, 8), 2L),
            daytype = c(rep("Weekday,SummerDesignDay", 8), "AllOtherDay"),
            time = hms::hms(hours = c(6L, 8L, 12:14, 18:19, 24L, 24L), minutes = c(rep(0, 3), 30, rep(0, 5))),
            value = c(0.2, 0.5, 0.95, 0.6, 0.8, 0.95, 0.2, 0.0, 0.0)
        )
    )

    expect_equal(nrow(sch$extract(timestep = "1 min")), 2880L)
    expect_equal(nrow(sch$extract(timestep = "1 hour")), 48L)
    expect_equal(nrow(sch$extract(timestep = "0.5 hour")), 96L)
})
# }}}

# VALIDATE {{{
test_that("VALIDATE", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_idf <- file.path(eplus_config(8.8)$dir, "ExampleFiles/RefBldgLargeOfficeNew2004_Chicago.idf")

    expect_is(idf <- read_idf(path_idf), "Idf")
    expect_is(sch <- IdfScheduleCompact$new("sch", idf, new = TRUE), "IdfScheduleCompact")
    expect_is(valid <- sch$validate(), "IdfValidity")
    expect_equal(valid$missing_value$field_index, 2:5)
    expect_equal(valid$invalid_schedule_type_limits$field_index, 2L)
    expect_equal(valid$invalid_through_field$field_index, 3L)
    expect_false(sch$is_valid())

    expect_equal(sch$type_limits("fraction"), list(name = "Fraction", range = ranger(0, TRUE, 1, TRUE)))
    expect_is(sch$set(
        c("weekday", "summerdesignday") := list(..6 = 2, ..24 = 0),
        allotherday = list(..24 = 0), .check_range = FALSE
    ), "IdfScheduleCompact")
    expect_is(valid <- sch$validate(), "IdfValidity")
    expect_equal(valid$invalid_range$field_index, 6L)
    expect_equal(valid$invalid_range$value_num, 2)
})
# }}}
