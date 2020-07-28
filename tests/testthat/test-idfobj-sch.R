context("IdfSchedule")

# IdfSchedule class{{{
test_that("IdfSchedule class", {
    eplusr_option(verbose_info = FALSE)
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_idf <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")

    expect_is(idf <- read_idf(path_idf), "Idf")

    # create new
    expect_is(sch <- IdfScheduleCompact$new("schedule", idf, TRUE), "IdfScheduleCompact")

    # get type limits
    expect_null(sch$type_limits())
    expect_error(sch$type_limits("A"))
    expect_is(lim <- sch$type_limits("fraction"), "list")
    expect_equal(lim, list(name = "Fraction", range = ranger(0, TRUE, 1, TRUE)))
    expect_equal(sch$type_limits(), lim)

    # set values
    expect_is(class = "IdfScheduleCompact"
        sch$set(c("weekday", "summerdesignday") := list(
            ..6 = 0.2, ..8 = 0.5,
            ..12 = 0.95, "13:30" = 0.6, ..14 = 0.8,
            ..18 = 0.95, ..19 = 0.2, ..24 = 0),
            allotherday = list(..24 = 0)
        )
    )

    expect_is(sch$extract(), "data.table")

    # expand day type
    expect_is(sch$extract(daytype = "expand"), "data.table")

    # change time step
    expect_is(sch$extract(timestep = "1 hour"), "data.table")

    # can update
    expect_is(sch$update(sch$extract(timestep = "1 hour")), "IdfScheduleCompact")

    # can validate
    expect_is(sch$validate(), "IdfValidity")
})
# }}}
