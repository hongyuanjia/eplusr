context("Sql methods")

test_that("Sql methods", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    example <- copy_example()

    expect_output(job <- read_idf(example$idf)$run(example$epw, NULL))
    expect_silent(sql <- eplus_sql(job$locate_output(".sql")))

    # can get all table names
    expect_equal(length(sql$list_table()), 44L)

    # can read table
    expect_error(sql$read_table("a"), "no such table")
    expect_is(sql$read_table("Zones"), "data.table")

    # can read report data dictionary
    expect_is(sql$report_data_dict(), "data.table")

    # can read report data
    expect_is(sql$report_data(), "data.table")
    expect_is(sql$tabular_data(), "data.table")
    expect_false(has_name(sql$report_data(name = "EnergyTransfer:Building", case = NULL), "Case"))
    expect_true(has_name(sql$report_data(name = "EnergyTransfer:Building"), "Case"))
    expect_equal(unique(sql$report_data(name = "EnergyTransfer:Building", case = "test")$Case), "test")
    expect_equal(
        unique(format(sql$report_data(name = "EnergyTransfer:Building", year = 2016L)$DateTime, "%Y")),
        "2016"
    )
    expect_equal(
        attr(sql$report_data(name = "EnergyTransfer:Building", year = 2016L, tz = "America/Chicago")$DateTime, "tzone"),
        "America/Chicago"
    )

    skip_on_os("mac")
    # can get path
    expect_equal(sql$path(), job$locate_output(".sql"))

    clean_wd(example$idf)
    unlink(c(example$idf, example$epw))
})
