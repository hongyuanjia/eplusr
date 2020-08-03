context("HVAC Diagram")

test_that("HVAC Diagram", {
    eplusr_option(verbose_info = FALSE)
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    example <- copy_example()

    expect_silent(job <- eplus_job(example$idf, example$epw))

    expect_null(hvac_diagram(job$version(), tempfile()))

    expect_is(job$run(wait = TRUE, echo = FALSE), "EplusJob")

    expect_is(hvac_diagram(job$version(), job$locate_output(".bnd")), "character")
})
