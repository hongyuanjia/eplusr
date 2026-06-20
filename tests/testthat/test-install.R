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

test_that("macOS Qt installer executable is found inside a mounted app bundle", {
    skip_on_os("windows")

    no_ext <- "EnergyPlus-9.4.0-998c4b761e-Darwin-macOS10.15-x86_64"
    mount_dir <- tempfile()
    exec_dir <- file.path(mount_dir, paste0(no_ext, ".app"), "Contents", "MacOS")
    dir.create(exec_dir, recursive = TRUE)

    exec <- file.path(exec_dir, no_ext)
    file.create(exec)
    Sys.chmod(exec, "755")

    expect_identical(macos_qt_installer_exec(mount_dir, no_ext), normalizePath(exec))
})

test_that("macOS Qt installer executable can be found at mounted volume root", {
    skip_on_os("windows")

    no_ext <- "EnergyPlus-9.4.0-998c4b761e-Darwin-macOS10.15-x86_64"
    mount_dir <- tempfile()
    dir.create(mount_dir)

    exec <- file.path(mount_dir, no_ext)
    file.create(exec)
    Sys.chmod(exec, "755")

    expect_identical(macos_qt_installer_exec(mount_dir, no_ext), normalizePath(exec))
})

test_that("macOS Qt installer uses executable from mounted DMG", {
    skip_on_os("windows")

    no_ext <- "EnergyPlus-9.4.0-998c4b761e-Darwin-macOS10.15-x86_64"
    inst <- file.path(tempdir(), paste0(no_ext, ".dmg"))
    mount_dir <- tempfile()
    qt_exec <- file.path(mount_dir, paste0(no_ext, ".app"), "Contents", "MacOS", no_ext)
    used_exec <- NULL
    unmounted <- FALSE

    local_mocked_bindings(
        macos_mount_dmg = function(exec, no_ext) {
            res <- 0L
            attr(res, "mount_dir") <- mount_dir
            res
        },
        macos_unmount_dmg = function(mount_dir) {
            unmounted <<- TRUE
            0L
        },
        macos_qt_installer_exec = function(mount_dir, no_ext) qt_exec,
        install_eplus_qt = function(ver, exec, dir, local = FALSE) {
            used_exec <<- exec
            0L
        }
    )

    res <- install_eplus_macos("9.4", inst, local = TRUE)

    expect_identical(used_exec, qt_exec)
    expect_true(unmounted)
    expect_identical(attr(res, "path"),
        normalizePath("~/Applications/EnergyPlus-9-4-0", mustWork = FALSE)
    )
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
