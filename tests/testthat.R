library(testthat)
library(eplusr)
eplusr_option(verbose_info = FALSE)

# Preserve ordinary check reporting while recording phase and individual test timings for CI.
testrun__phase <- function(name, ..., file = NULL, desc = NULL) {
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
    results <- if (is.null(file)) {
        test_check("eplusr", ...)
    } else {
        # Native description filtering keeps each source-owned group on its original worker.
        withr::local_envvar(TESTTHAT_IS_CHECKING = "true")
        test_file(
            file.path("testthat", file),
            package = "eplusr",
            load_package = "installed",
            reporter = check_reporter(),
            desc = desc,
            stop_on_failure = TRUE
        )
    }

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
    groups <- c(
        api = "api",
        `session-errors` = "session-errors",
        replay = "replay",
        run = "run",
        `run-api` = "run-api"
    )
    if (shard %in% names(groups)) {
        file <- if (shard %in% c("run", "run-api")) {
            "test-run.R"
        } else {
            "test-api.R"
        }
        testrun__phase(shard, file = file, desc = groups[[shard]])
    } else if (shard %in% c("plugins", "engine", "ordinary")) {
        filters <- c(
            plugins = "^plugin(-cli)?$",
            engine = "^(job|group|param)$",
            ordinary = "^(api|plugin(-cli)?|run|job|group|param)$"
        )
        testrun__phase(
            shard,
            filter = filters[[shard]],
            invert = shard == "ordinary"
        )
    } else {
        stop("Unknown coverage test shard.")
    }
} else if (identical(Sys.getenv("_EPLUSR_SERIAL_API_TESTS_"), "true")) {
    worker_tests <- "^(api|plugin(-cli)?|run|job|group|param)$"
    withr::with_envvar(
        c(TESTTHAT_PARALLEL = "false"),
        testrun__phase("engine", filter = worker_tests)
    )
    # The complementary phase retains the configured parallel test-file workers.
    testrun__phase("ordinary", filter = worker_tests, invert = TRUE)
} else {
    testrun__phase("all")
}
