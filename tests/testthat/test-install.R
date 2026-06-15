fake_eplus_install <- function(parent, ver = LATEST_EPLUS_VER, name = NULL) {
    ver <- standardize_ver(ver)
    if (is.null(name)) name <- paste0("EnergyPlus-", gsub(".", "-", ver, fixed = TRUE))

    dir <- file.path(parent, name)
    dir.create(dir, recursive = TRUE)
    writeLines(paste0("!IDD_Version ", ver), file.path(dir, "Energy+.idd"))
    file.create(file.path(dir, paste0("energyplus", if (is_windows()) ".exe" else "")))
    dir
}

with_clean_eplus_cache <- function(code) {
    old_eplus <- .globals$eplus
    old_scanned <- .globals$eplus_scanned
    old_dirs <- getOption("eplusr.eplus_dirs")
    on.exit({
        .globals$eplus <- old_eplus
        .globals$eplus_scanned <- old_scanned
        options(eplusr.eplus_dirs = old_dirs)
    }, add = TRUE)

    .globals$eplus <- list()
    .globals$eplus_scanned <- FALSE
    options(eplusr.eplus_dirs = NULL)

    force(code)
}

test_that(".onLoad() does not locate EnergyPlus", {
    with_clean_eplus_cache({
        get(".onLoad", envir = asNamespace("eplusr"))("", "eplusr")

        expect_identical(.globals$eplus, list())
        expect_false(.globals$eplus_scanned)
    })
})

test_that("EnergyPlus can be lazily located from eplusr.eplus_dirs", {
    with_clean_eplus_cache({
        root <- tempfile()
        fake <- fake_eplus_install(root)
        options(eplusr.eplus_dirs = root)

        cfg <- eplus_config(LATEST_EPLUS_VER)

        expect_identical(cfg$dir, normalizePath(fake))
        expect_false(.globals$eplus_scanned)
    })
})

test_that("avail_eplus() performs full lazy discovery once", {
    with_clean_eplus_cache({
        root <- tempfile()
        fake_eplus_install(root)
        options(eplusr.eplus_dirs = root)

        expect_true(numeric_version(LATEST_EPLUS_VER) %in% avail_eplus())
        expect_true(.globals$eplus_scanned)
    })
})

test_that("locate_eplus() accepts extra parent and exact installation dirs", {
    with_clean_eplus_cache({
        root <- tempfile()
        fake <- fake_eplus_install(root)

        expect_true(numeric_version(LATEST_EPLUS_VER) %in% locate_eplus(dirs = root))

        .globals$eplus <- list()
        .globals$eplus_scanned <- FALSE

        expect_true(numeric_version(LATEST_EPLUS_VER) %in% locate_eplus(dirs = fake))
    })
})

test_that("locate_eplus() preserves existing custom EnergyPlus paths", {
    with_clean_eplus_cache({
        custom <- fake_eplus_install(tempfile(), name = "custom-eplus")
        root <- tempfile()
        fake_eplus_install(root)

        suppressMessages(use_eplus(custom))
        locate_eplus(dirs = root)

        expect_identical(eplus_config(LATEST_EPLUS_VER)$dir, normalizePath(custom))
    })
})

test_that("invalid extra EnergyPlus dirs are ignored quietly", {
    with_clean_eplus_cache({
        expect_silent(locate_eplus(dirs = file.path(tempdir(), "missing-eplus-dir")))
    })
})

test_that("Install EnergyPlus v9.0 and below", {
    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_INSTALL_OLD_") != "")
    skip_if_not(testthat:::on_ci())

    expect_equal(sort(as.character(avail_eplus())), sort(names(.globals$eplus)))

    if (is_avail_eplus("8.8")) {
        expect_error(install_eplus("8.8", local = TRUE))
    } else {
        install_eplus("8.8", local = TRUE)
    }
})

test_that("Install EnergyPlus v9.1 and above", {
    skip_on_cran()
    skip_if_not(testthat:::on_ci())

    expect_equal(sort(as.character(avail_eplus())), sort(names(.globals$eplus)))

    # test if patch on EnergyPlus v9.1 and above works
    if (!is_avail_eplus(LATEST_EPLUS_VER)) {
        expect_equal(ignore_attr = TRUE,
            res <- install_eplus(LATEST_EPLUS_VER, local = TRUE),
            0L
        )
        installer <- attr(res, "installer")

        # can update EnergyPlus config
        expect_true(is_avail_eplus(LATEST_EPLUS_VER))

        # can uninstall EnergyPlus
        expect_equal(ignore_attr = TRUE, uninstall_eplus(LATEST_EPLUS_VER), 0L)

        # can remove EnergyPlus config
        expect_false(LATEST_EPLUS_VER %in% as.character(avail_eplus()))

        # still need latest EnergyPlus for testing transitions
        install_eplus_from_file(LATEST_EPLUS_VER, installer, TRUE)
        # refresh config database
        expect_s3_class(locate_eplus(), "numeric_version")
    }
})

# vim: set fdm=marker:
