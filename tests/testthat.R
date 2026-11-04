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

# Exercise only reviewed legacy boundaries in each existing worker and coverage denominator.
testrun__legacy <- function(shard) {
    version <- Sys.getenv("_EPLUSR_TEST_COVERAGE_LEGACY_")
    if (!nzchar(version)) {
        return(invisible(NULL))
    }
    definitions <- list(
        api = list(
            version = "23.2",
            cases = list(
                c(
                    "test-api.R",
                    "api",
                    "stock older API reports safe discovery and readiness capabilities"
                ),
                c(
                    "test-api.R",
                    "api",
                    "stock API exposes four groups and reviewed operation provenance"
                ),
                c(
                    "test-api.R",
                    "api",
                    "stock runtime rejects IDF and epJSON model version mismatches"
                )
            )
        ),
        `session-errors` = list(
            version = "9.4",
            cases = list(
                c(
                    "test-api.R",
                    "api",
                    "stock 9.4 state lifecycles require a package-owned fresh worker"
                ),
                c(
                    "test-run.R",
                    "run-api",
                    "stock 9.4 workers contain one state and preserve functional and feedback calls"
                ),
                c(
                    "test-run.R",
                    "run-api",
                    "stock 9.4 callback failures retire their worker without poisoning the caller"
                ),
                c(
                    "test-run.R",
                    "run-api",
                    "stock 9.4 CLI exits are contained by the public worker backend"
                ),
                c(
                    "test-api.R",
                    "api",
                    "stock sessions commit bounded plugin history before observation and control"
                )
            )
        ),
        replay = list(
            version = "9.5",
            cases = list(
                c(
                    "test-api.R",
                    "api",
                    "stock early API rejects missing or conflicting SDK identity"
                ),
                c(
                    "test-api.R",
                    "api",
                    "stock early API identifies its selected installation and console limitation"
                ),
                c(
                    "test-api.R",
                    "api",
                    "stock early API rounds fractional minute boundaries consistently"
                ),
                c(
                    "test-api.R",
                    "api",
                    "stock early design-only callback failures finish safely with original conditions"
                ),
                c(
                    "test-plugin.R",
                    "stock warmup trends retain elapsed hours across repeated days and environments"
                )
            )
        ),
        plugins = list(
            version = "9.6",
            cases = list(
                c(
                    "test-api.R",
                    "api",
                    "stock early API identifies its selected installation and console limitation"
                ),
                c(
                    "test-api.R",
                    "api",
                    "stock early API rounds fractional minute boundaries consistently"
                ),
                c(
                    "test-api.R",
                    "api",
                    "stock early design-only callback failures finish safely with original conditions"
                )
            )
        ),
        run = list(
            version = "24.2",
            cases = list(
                c(
                    "test-api.R",
                    "api",
                    "stock callbacks preserve exchange units and typed control ownership"
                ),
                c(
                    "test-api.R",
                    "api",
                    "stock API exposes four groups and reviewed operation provenance"
                )
            )
        ),
        `run-api` = list(
            version = "24.1",
            cases = list(
                c(
                    "test-api.R",
                    "api",
                    "stock older API reports safe discovery and readiness capabilities"
                ),
                c(
                    "test-api.R",
                    "api",
                    "stock API exposes four groups and reviewed operation provenance"
                ),
                c(
                    "test-api.R",
                    "api",
                    "stock runtime requires one valid prepared model version"
                )
            )
        ),
        engine = list(
            version = "22.1",
            cases = list(
                c(
                    "test-api.R",
                    "api",
                    "stock 22.x APIs preserve uniform groups and reject stale functional resources"
                ),
                c(
                    "test-api.R",
                    "api",
                    "stock early design-only callback failures finish safely with original conditions"
                ),
                c(
                    "test-api.R",
                    "api",
                    "stock older API reports safe discovery and readiness capabilities"
                )
            )
        ),
        ordinary = list(
            version = "9.5",
            cases = list(
                c(
                    "test-plugin.R",
                    "stock early trends keep a chronological clock across New Year"
                ),
                c(
                    "test-api.R",
                    "api",
                    "stock sessions preserve calendar provenance across December and January"
                )
            )
        )
    )
    definition <- definitions[[shard]]
    if (is.null(definition) || !identical(version, definition$version)) {
        stop(
            "The coverage legacy installation does not match its reviewed shard."
        )
    }
    if (version == "9.6") {
        definition$cases <- c(
            definition$cases,
            list(c(
                "test-api.R",
                "legacy-python",
                "stock 9.6 Linux restarts its selected CPython dependency across owned lifecycles"
            ))
        )
    }
    # A fresh process owns this whole legacy group: RTLD_GLOBAL engine/Python
    # dependencies from the baseline must not participate in its native resolution.
    callr::r(
        function(cases, version, shard, phase) {
            library(testthat)
            library(eplusr)
            eplusr_option(verbose_info = FALSE)
            for (index in seq_along(cases)) {
                case <- cases[[index]]
                results <- phase(
                    paste0(shard, "-legacy-", version, "-", index),
                    file = case[[1L]],
                    desc = case[-1L]
                )
                if (any(as.data.frame(results)$skipped)) {
                    stop("A selected legacy coverage case was skipped.")
                }
            }
            invisible(NULL)
        },
        args = list(definition$cases, version, shard, testrun__phase),
        libpath = .libPaths(),
        env = c(
            `_EPLUSR_TEST_API_VERSION_` = version,
            TESTTHAT_PARALLEL = "false",
            TESTTHAT_CPUS = "1"
        ),
        show = TRUE
    )
    invisible(NULL)
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
    testrun__legacy(shard)
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
