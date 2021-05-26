test_that("Rdd", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    idf <- read_idf(example())
    job <- idf$run(NULL, dir = tempdir(), echo = FALSE)

    expect_is(parse_rdd_file(tempfile()), "RddFile")

    expect_silent(rdd <- job$read_rdd())
    expect_silent(mdd <- job$read_mdd())

    expect_output(print(rdd))
    expect_output(print(mdd))

    expect_equal(names(rdd), c("index", "reported_time_step", "report_type", "variable", "units"))
    expect_equal(names(mdd), c("index", "reported_time_step", "report_type", "variable", "units"))
    expect_equal(attr(rdd, "eplus_version"), idf$version())
    expect_equal(attr(mdd, "eplus_version"), idf$version())

    expect_error(rdd_to_load(rdd, reporting_frequency = "hour"), class = "eplusr_error")
    expect_error(mdd_to_load(mdd, reporting_frequency = "hour"), class = "eplusr_error")
    expect_error(rdd_to_load(rdd[1:2][, reporting_frequency := c(1, 2)]), class = "eplusr_error")
    expect_error(mdd_to_load(mdd[1:2][, reporting_frequency := c(1, 2)]), class = "eplusr_error")
    expect_error(mdd_to_load(mdd, class = ""))

    expect_equivalent(rdd_to_load(rdd[1L]),
        data.table(id = 1L, class = "Output:Variable", index = 1:3,
            field = c("Key Value", "Variable Name", "Reporting Frequency"),
            value = c("*", "Site Outdoor Air Drybulb Temperature", "Timestep")
        )
    )

    expect_equivalent(rdd_to_load(rdd[1L][, key_value := "Environment"]),
        data.table(id = 1L, class = "Output:Variable", index = 1:3,
            field = c("Key Value", "Variable Name", "Reporting Frequency"),
            value = c("Environment", "Site Outdoor Air Drybulb Temperature", "Timestep")
        )
    )

    expect_equivalent(mdd_to_load(mdd[1L]),
        data.table(id = 1L, class = "Output:Meter", index = 1:2,
            field = c("Key Name", "Reporting Frequency"),
            value = c("Electricity:Facility", "Timestep")
        )
    )
    expect_equivalent(mdd_to_load(mdd[1L], class = "Output:Meter:MeterFileOnly"),
        data.table(id = 1L, class = "Output:Meter:MeterFileOnly", index = 1:2,
            field = c("Key Name", "Reporting Frequency"),
            value = c("Electricity:Facility", "Timestep")
        )
    )
})
