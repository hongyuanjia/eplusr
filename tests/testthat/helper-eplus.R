# Skip tests that exercise external programs or network services unless the
# single integration-test mode is explicitly enabled.
skip_if_not_integration <- function() {
    skip_if_not(
        identical(Sys.getenv("_EPLUSR_RUN_INTEGRATION_TESTS_"), "true"),
        "integration tests are disabled"
    )
}

if (identical(Sys.getenv("NOT_CRAN"), "true") &&
    identical(Sys.getenv("CI"), "true") &&
    !is_avail_eplus(LATEST_EPLUS_VER)) {
    install_eplus(LATEST_EPLUS_VER, local = TRUE)
    # NOTE: From EnergyPlus v23.1, the supported oldest version for transition
    # is v9.0. Have to install at least one version before v23.1 for testing
    # older version transitions
    if (identical(Sys.getenv("_EPLUSR_RUN_INTEGRATION_TESTS_"), "true") &&
        !is_avail_eplus("22.2")) {
        install_eplus("22.2", local = TRUE)
    }
}
