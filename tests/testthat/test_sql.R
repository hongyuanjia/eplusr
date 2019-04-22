context("Sql methods")

test_that("Sql methods", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    example <- copy_example()

    expect_output(job <- read_idf(example$idf)$run(example$epw, NULL))
    expect_silent(sql <- eplus_sql(job$locate_output(".sql")))

    # path
    expect_equal(sql$path(), normalizePath(file.path(tempdir(), "5Zone_Transformer.sql")))
    expect_equal(sql$path_idf(), normalizePath(file.path(tempdir(), "5Zone_Transformer.idf")))

    # can get all table names
    expect_equal(length(sql$list_table()), 44L)

    # can read table
    expect_error(sql$read_table("a"), "no such table")
    expect_is(sql$read_table("Zones"), "data.table")

    # can read report data dictionary
    expect_is(sql$report_data_dict(), "data.table")

    # can read report data
    expect_equal(nrow(sql$report_data(sql$report_data_dict())), 3840L)
    expect_equal(nrow(sql$report_data()), 3840L)
    expect_equal(nrow(sql$report_data("")), 1344L)
    expect_equal(nrow(sql$report_data(
        "TRANSFORMER 1", "Transformer Load Loss Rate")),
        192L
    )
    expect_equal(nrow(sql$report_data(
        "TRANSFORMER 1", "Transformer Load Loss Rate")),
        192L
    )
    expect_equal(year(sql$report_data(
        "TRANSFORMER 1", "Transformer Load Loss Rate", year = 2010)$datetime),
        rep(2010, 192)
    )
    expect_equal(lubridate::tz(sql$report_data(tz = "Asia/Shanghai")$datetime),
        "Asia/Shanghai"
    )
    expect_equal(sql$report_data(case = "test")$case, rep("test", 3840))
    expect_equal(names(sql$report_data(all = TRUE)),
        c("case", "datetime", "month", "day", "hour", "minute", "dst", "interval",
          "simulation_days", "day_type", "environment_name", "is_meter", "type",
          "index_group", "timestep_type", "key_value", "name", "reporting_frequency",
          "schedule_name", "units", "value"
        )
    )
    expect_equal(nrow(sql$report_data(period = seq(
        lubridate::ymd_hms("2019-01-14 0:0:0"), lubridate::ymd_hms("2019-01-15 0:0:0"), "15 min")
    )), 1900)
    expect_equal(nrow(sql$report_data(month = 1)), 1920)
    expect_equal(nrow(sql$report_data(month = 1, hour = 1)), 80)
    expect_equal(nrow(sql$report_data(minute = 0)), 960)
    expect_equal(nrow(sql$report_data(interval = 15)), 3840)
    expect_equal(nrow(sql$report_data(simulation_days = 1)), 3840)
    expect_equal(nrow(sql$report_data(day_type = "Tuesday")), 3840)
    expect_equal(nrow(sql$report_data(environment_name = "WINTERDAY")), 1920)

    expect_equal(nrow(sql$tabular_data()), 6662)
    expect_equal(nrow(sql$tabular_data(
        report_name = c(
            "AnnualBuildingUtilityPerformanceSummary",
            "Initialization Summary"
        ))),
        3774
    )
    expect_equal(nrow(sql$tabular_data(table_name = "Site and Source Energy")), 12)
    expect_equal(nrow(sql$tabular_data(column_name = "Total Energy")), 4)
    expect_equal(nrow(sql$tabular_data(row_name = "Total Site Energy")), 3)

    skip_on_os("mac")
    # can get path
    expect_equal(sql$path(), job$locate_output(".sql"))

    clean_wd(example$idf)
    unlink(c(example$idf, example$epw))
})
