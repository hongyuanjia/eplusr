library(testthat)
library(eplusr)
eplusr_option(verbose_info = FALSE)

# Coverage workers retrace the package in every nested R process; keep heavy
# worker trees from competing across test files while preserving group parallelism.
if (identical(Sys.getenv("_EPLUSR_SERIAL_API_TESTS_"), "true")) {
    worker_tests <- "^(api|run|job|group|param)$"
    withr::with_envvar(
        c(TESTTHAT_PARALLEL = "false"),
        test_check("eplusr", filter = worker_tests)
    )
    # The complementary phase retains the configured parallel test-file workers.
    test_check("eplusr", filter = worker_tests, invert = TRUE)
} else {
    test_check("eplusr")
}
