context("Job methods")

test_that("Job methods", {
    skip_on_cran()
    install_ep88()
    example <- copy_example()

    job <- eplus_job(example$idf, example$epw)

    expect_output(job$run())

    expect_equal(job$status(),
        list(run_before = TRUE, changed_after = FALSE, terminated = FALSE,
            successful = TRUE, alive = FALSE, wait = TRUE)
    )

    expect_equal(job$output_dir(), dirname(example$idf))

    expect_is(job$errors(), "ErrFile")

    # clean
    clean_wd(example$idf)
    unlink(c(example$idf, example$epw))
})
