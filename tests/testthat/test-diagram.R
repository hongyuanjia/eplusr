test_that("HVAC Diagram", {
    skip_on_cran()
    skip_if_not_integration()

    bundle <- latest_output_bundle()

    expect_s3_class(
        pending_job <- eplus_job(bundle$idf_path, bundle$epw_path),
        "EplusJob"
    )

    expect_warning(res <- hvac_diagram(pending_job$version(), tempfile()))

    expect_s3_class(job <- bundle$job, "EplusJob")

    expect_warning(res <- hvac_diagram(job$version(), job$locate_output(".bnd")))
    expect_type(res, "character")
})
