test_that("HVAC Diagram", {
    skip_on_cran()

    eplusr_option(verbose_info = FALSE)
    example <- copy_example()

    expect_silent(job <- eplus_job(example$idf, example$epw))

    expect_warning(res <- hvac_diagram(job$version(), tempfile()))

    expect_is(job$run(wait = TRUE, echo = FALSE), "EplusJob")

    expect_warning(res <- hvac_diagram(job$version(), job$locate_output(".bnd")))
    expect_is(res, "character")
})
