# Run a bounded, maintained public contract on an explicitly selected stock engine.
# Every worker inherits the successfully installed candidate package and task dependencies.
library(eplusr)
version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_")
stopifnot(
    nzchar(version),
    identical(Sys.getenv("_EPLUSR_RUN_API_TESTS_"), "true")
)
if (utils::packageVersion("testthat") < "3.3.0") {
    stop("The maintained contract runner requires testthat >= 3.3.0.")
}
configuration <- eplus_config(version)
message("Candidate package: ", find.package("eplusr"))
message("Package version: ", utils::packageVersion("eplusr"))
message("Native package library: ", getLoadedDLLs()[["eplusr"]][["path"]])
message("R platform: ", R.version$platform)
message("Selected stock installation: ", configuration$dir)
print(system2(
    file.path(configuration$dir, configuration$exe),
    "--version",
    stdout = TRUE
))

# Existing acceptance cases establish literal physical/control/history/replay results.
cases <- list(
    list(
        "api",
        c(
            "api",
            "stock older API reports safe discovery and readiness capabilities"
        )
    ),
    list(
        "api",
        c(
            "api",
            "stock sessions commit bounded plugin history before observation and control"
        )
    ),
    list(
        "api",
        c(
            "replay",
            "stock replay rebuilds independent managed trends before future policies"
        )
    ),
    list(
        "api",
        c(
            "api",
            "retired native callback addresses remain inert after garbage collection"
        )
    ),
    list(
        "plugin-cli",
        "native executable boundaries reject foreign threads and restore actual signal handlers"
    ),
    list(
        "plugin-cli",
        "executable export rejects managed trends before creating artifacts"
    )
)
if (version == "9.4") {
    # This release's public contract enters only through package-owned disposable workers.
    cases <- c(
        cases,
        list(
            list(
                "run",
                c(
                    "run-api",
                    "stock 9.4 workers contain one state and preserve functional and feedback calls"
                )
            ),
            list(
                "run",
                c(
                    "run-api",
                    "stock 9.4 CLI exits are contained by the public worker backend"
                )
            )
        )
    )
} else {
    cases <- c(
        cases,
        list(
            list(
                "api",
                c(
                    "api",
                    "stock API exposes four groups and reviewed operation provenance"
                )
            ),
            list(
                "plugin",
                "managed trend aggregates support one sample and newest wrapped windows"
            ),
            list(
                "plugin",
                "managed trend handles reject retired cross-state and ambiguous ownership"
            )
        )
    )
}
for (case in cases) {
    results <- testthat::test_file(
        file.path("tests/testthat", paste0("test-", case[[1L]], ".R")),
        package = "eplusr",
        load_package = "installed",
        desc = case[[2L]],
        reporter = testthat::CheckReporter$new(),
        stop_on_failure = TRUE,
        stop_on_warning = TRUE
    )
    # A selected unavailable capability must remain a visible incomplete contract cell.
    if (any(as.data.frame(results)$skipped)) {
        stop("A selected contract case was skipped; this cell has not passed.")
    }
}
