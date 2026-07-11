# Keep one authoritative latest-version EnergyPlus run for read-only consumers
# during a testthat session. This is intentionally not persisted across runs.
.latest_output_bundle <- new.env(parent = emptyenv())

# Generate the shared output lazily so CRAN and skipped integration tests never
# invoke EnergyPlus merely by loading the test helpers.
latest_output_bundle <- function() {
    if (!exists("value", envir = .latest_output_bundle, inherits = FALSE)) {
        example <- copy_example()
        output_dir <- tempfile("eplusr-latest-output-")
        dir.create(output_dir)

        idf <- read_idf(example$idf)
        epw <- read_epw(example$epw)
        job <- idf$run(example$epw, output_dir, echo = FALSE)

        assign(
            "value",
            list(
                idf = idf,
                epw = epw,
                idf_path = example$idf,
                epw_path = example$epw,
                job = job
            ),
            envir = .latest_output_bundle
        )
    }

    get("value", envir = .latest_output_bundle, inherits = FALSE)
}

# Copy the bundle before a test mutates any generated output, then redirect the
# cloned job result to that private directory.
copy_latest_output_bundle <- function() {
    bundle <- latest_output_bundle()
    source_dir <- bundle$job$output_dir()
    output_dir <- tempfile("eplusr-latest-output-copy-")
    dir.create(output_dir)
    copied <- file.copy(
        list.files(source_dir, full.names = TRUE), output_dir,
        recursive = TRUE, copy.mode = TRUE, copy.date = TRUE
    )
    stopifnot(all(copied))

    job <- unserialize(serialize(bundle$job, NULL))
    private <- get_priv_env(job)
    private$m_job$output_dir <- normalizePath(output_dir)

    bundle$job <- job
    bundle
}
