library(testthat)
library(eplusr)
eplusr_option(verbose_info = FALSE)

# Preserve ordinary check reporting while recording phase and individual test timings for CI.
testrun__phase <- function(name, ...) {
    directory <- Sys.getenv("_EPLUSR_TEST_TIMING_DIR_")
    started <- proc.time()[["elapsed"]]
    on.exit(
        {
            if (nzchar(directory)) {
                seconds <- proc.time()[["elapsed"]] - started
                write.csv(
                    data.frame(stage = name, seconds = seconds),
                    file.path(directory, paste0("tests-", name, "-phase.csv")),
                    row.names = FALSE
                )
            }
        },
        add = TRUE
    )
    results <- test_check("eplusr", ...)
    if (nzchar(directory)) {
        timings <- as.data.frame(results)
        timings$result <- NULL
        write.csv(
            timings,
            file.path(directory, paste0("tests-", name, "-cases.csv")),
            row.names = FALSE
        )
    }
    invisible(results)
}

# Separate runner shards avoid competing engine worker trees without omitting test files.
shard <- Sys.getenv("_EPLUSR_TEST_SHARD_")
if (nzchar(shard)) {
    filters <- c(
        api = "^api(-session)?$",
        replay = "^api-replay$",
        plugins = "^api-plugins$",
        engine = "^(run|job|group|param)$",
        ordinary = "^(api(-.*)?|run|job|group|param)$"
    )
    if (!shard %in% names(filters)) {
        stop("Unknown coverage test shard.")
    }
    testrun__phase(
        shard,
        filter = filters[[shard]],
        invert = shard == "ordinary"
    )
} else if (identical(Sys.getenv("_EPLUSR_SERIAL_API_TESTS_"), "true")) {
    worker_tests <- "^(api(-.*)?|run|job|group|param)$"
    withr::with_envvar(
        c(TESTTHAT_PARALLEL = "false"),
        testrun__phase("engine", filter = worker_tests)
    )
    # The complementary phase retains the configured parallel test-file workers.
    testrun__phase("ordinary", filter = worker_tests, invert = TRUE)
} else {
    testrun__phase("all")
}
