# download_idd() {{{
test_that("download_idd() can download IDD from EnergyPlus repo", {
    expect_error(download_idd(1, tempdir()), class = "eplusr_error_invalid_eplus_ver")

    skip_on_cran()
    skip_if(Sys.getenv("_EPLUSR_SKIP_TESTS_DOWNLOAD_IDD_") != "")

    # should download IDD v9.0.1 if input is 9, 9.0, 9.0.1
    expect_equal(read_idd(attr(download_idd("9.0", tempdir()), "file"))$version(), numeric_version("9.0.1"))
    expect_equal(read_idd(attr(download_idd("9.0.1", tempdir()), "file"))$version(), numeric_version("9.0.1"))
})
# }}}

# use_idd() {{{
test_that("can read IDD", {
    skip_on_cran()

    # remove all parsed IDD
    rm_global_cache("idd")

    expect_error(is_avail_idd("latest"))

    expect_equal(avail_idd(), NULL)

    # can stop if failed to read input file
    expect_error(use_idd(""), class = "eplusr_error_read_lines")

    # can directly download from EnergyPlus GitHub repo
    expect_s3_class(idd <- use_idd("8.4", download = TRUE), "Idd")

    # can directly return if input is an Idd
    expect_s3_class(use_idd(idd), "Idd")

    # can directly return if parsed previously
    expect_s3_class(use_idd("8.4"), "Idd")

    # can use the IDD in EnergyPlus VersionUpdater folder
    rm_global_cache("idd")
    # NOTE: from EnergyPlus v23.1, the oldest IDD version shipped with
    # EnergyPlus is EnergyPlus v9.0
    expect_s3_class(use_idd("9.0"), "Idd")

    # can stop if that EnergyPlus is not available and IDD if not found in any
    # existing VersionUpdater folder
    rm_global_cache("eplus", avail_eplus())
    expect_error(use_idd(LATEST_EPLUS_VER), class = "eplusr_error_locate_idd")

    # can direct read if corresponding EnergyPlus is found
    use_eplus(LATEST_EPLUS_VER)
    expect_s3_class(use_idd(LATEST_EPLUS_VER), "Idd")

    # can search in VersionUpdater folder if "Energy+.idd" is not found in
    # EnergyPlus folder
    use_eplus(LATEST_EPLUS_VER)
    f1 <- path_eplus(LATEST_EPLUS_VER, "Energy+.idd")
    f1_bak <- paste0(f1, ".bak")
    file.rename(f1, f1_bak)
    expect_s3_class(use_idd(LATEST_EPLUS_VER), "Idd")
    file.rename(f1_bak, f1)

    # can stop if no available IDD found in any existing VersionUpdater folder
    rm_global_cache("eplus")
    use_eplus(LATEST_EPLUS_VER)
    rm_global_cache("idd")
    f2 <- find_idd_from_updater("9.0")
    f2_bak <- paste0(f2, ".bak")
    file.rename(f2, f2_bak)
    expect_error(use_idd("9.0"), class = "eplusr_error_locate_idd")
    # but "auto" still work in this case
    expect_s3_class(use_idd("9.0", "auto"), "Idd")
    file.rename(f2_bak, f2)

    # can use "latest" notation
    expect_s3_class(use_idd("latest", download = TRUE), "Idd")

    # helper functions
    expect_true(numeric_version(LATEST_EPLUS_VER) %in% avail_idd())
    expect_true(is_avail_idd(as.double(stri_sub(LATEST_EPLUS_VER, to = -3L))))
    expect_true(is_avail_idd(stri_sub(LATEST_EPLUS_VER, to = -3L)))
    expect_true(is_avail_idd(LATEST_EPLUS_VER))

    # can use custom IDD
    expect_silent(use_idd(idftext("idd", "9.9.9")))
    expect_true(is_avail_idd("9.9.9"))

    # recover EnergyPlus config
    locate_eplus()

    # can parse old IDD
    expect_s3_class(use_idd("7.2"), "Idd")
    expect_s3_class(use_idd("8.0"), "Idd")
    expect_s3_class(use_idd("8.1"), "Idd")
    expect_s3_class(use_idd("8.2"), "Idd")

    # can auto find suitable IDD
    expect_s3_class(get_idd_from_ver("9.0", NULL), "Idd")
    # can give warning if hard-coded IDD is used
    expect_warning(get_idd_from_ver(standardize_ver("8.5"), use_idd("9.0")), "mismatch")
    expect_warning(get_idd_from_ver(NULL, use_idd("9.0")), "given IDD")
    # can stop if no available IDD parsed
    rm_global_cache("idd")
    rm_global_cache("eplus")
    expect_error(get_idd_from_ver("9.0", NULL), class = "eplusr_error_locate_idd")
    expect_error(get_idd_from_ver(NULL, NULL), class = "eplusr_error_no_avail_idd")

    locate_eplus()
    use_idd(LATEST_EPLUS_VER)
    expect_warning(get_idd_from_ver(NULL, NULL), "latest")
})
# }}}

# Idd class {{{
test_that("Idd class", {
    # can create an Idd object from string
    expect_silent(idd <- use_idd(idftext("idd", "9.9.9")))

    # can get Idd version
    expect_equal(idd$version(), as.numeric_version("9.9.9"))

    # can get Idd path
    expect_null(idd$path())

    # can get Idd build
    expect_equal(idd$build(), "7c3bbe4830")

    # can get all group names
    expect_equal(idd$group_name(), c("TestGroup1", "TestGroup2"))

    # can get group name of one class
    expect_equal(idd$from_group("TestSimple"), "TestGroup1")

    # can return when multiple class names are given
    expect_equal(idd$from_group(c("TestSlash", "TestSimple")),
        c("TestGroup2", "TestGroup1"))

    # can stop when invalid class name is given
    expect_error(idd$from_group("WrongClass"), class = "eplusr_error_invalid_class_name")

    # can return all class names
    expect_equal(idd$class_name(), c("TestSimple", "TestSlash"))

    # can group class names
    expect_equal(idd$class_name(by_group = TRUE), list(TestGroup1 = "TestSimple", TestGroup2 = "TestSlash"))

    # can return an index of a single group
    expect_equal(idd$group_index("TestGroup1"), 1)

    # can return multiple group indexes
    expect_equal(idd$group_index(c("TestGroup2", "TestGroup1", "TestGroup2")),
        c(2L, 1L, 2L))

    # can stop when invalid group names are given
    expect_error(idd$group_index("WrongGroup"), class = "eplusr_error_invalid_group_name")

    # can return an index of a single class
    expect_equal(idd$class_index("TestSlash"), 2L)

    # can return multiple class indexes
    expect_equal(idd$class_index(c("TestSlash", "TestSimple", "TestSimple")),
        c(2L, 1L, 1L))

    # can group class index
    expect_equal(idd$class_index(by_group = TRUE), list(TestGroup1 = 1L, TestGroup2 = 2L))

    # can stop when invalid class names are given
    expect_error(idd$class_index("WrongClass"), class = "eplusr_error_invalid_class_name")

    expect_s3_class(idd$object_relation("TestSimple"), "IddRelation")
    expect_s3_class(idd$object_relation("TestSimple", "ref_to"), "IddRelation")
    expect_s3_class(idd$object_relation("TestSimple", "ref_by"), "IddRelation")

    # can return names of all required classes
    expect_equal(idd$required_class_name(), "TestSlash")

    # can return names of all unique-object classes
    expect_equal(idd$unique_class_name(), "TestSlash")

    # can return names of all extensible classes
    expect_equal(idd$extensible_class_name(), "TestSlash")

    # can return a single IddObject using class name
    expect_s3_class(idd$object("TestSimple"), "IddObject")

    # can stop when invalid class names are given
    expect_error(idd$object("WrongClass"), class = "eplusr_error_invalid_class_name")
    expect_s3_class(idd$object("TestSlash"), "IddObject")

    # can return when multiple class names are given
    expect_equal(idd$objects(c("TestSimple", "TestSlash")),
        list(TestSimple = idd$object("TestSimple"),
            TestSlash = idd$object("TestSlash")))

    # can return all IddObjects in a group
    expect_type(idd$objects_in_group("TestGroup1"), "list")
    expect_equal(idd$objects_in_group("TestGroup1"), list(TestSimple = idd$object("TestSimple")))

    # can stop when invalid group names are given
    expect_error(idd$objects_in_group("WrongGroup"), class = "eplusr_error_invalid_group_name")

    # can stop when multiple group names are given
    expect_error(idd$objects_in_group(c("TestGroup1", "TestGroup2")), "Must have length 1")
    expect_s3_class(idd$objects_in_group("TestGroup1")[[1L]], "IddObject")

    expect_type(idd$objects_in_relation("TestSimple", "ref_to"), "list")
    expect_equal(names(idd$objects_in_relation("TestSimple", "ref_to")), "TestSimple")
    expect_type(idd$objects_in_relation("TestSimple", "ref_by"), "list")
    expect_equal(names(idd$objects_in_relation("TestSimple", "ref_by")), c("TestSimple", "TestSlash"))

    # can check if input is a valid group
    expect_false(idd$is_valid_group("WrongGroup"))
    expect_true(idd$is_valid_group("TestGroup1"))

    # can check if input is a valid class
    expect_false(idd$is_valid_class("WrongClass"))
    expect_true(idd$is_valid_class("TestSlash"))

    # can export IDD data in data.table format
    expect_equal(idd$to_table("TestSlash", all = TRUE),
        data.table(class = "TestSlash", index = 1:4,
            field = c("Test Character Field 1", "Test Numeric Field 1",
                      "Test Numeric Field 2", "Test Character Field 2"
            )
        )
    )
    expect_equal(idd$to_table("TestSlash"),
        data.table(class = "TestSlash", index = 1:3,
            field = c("Test Character Field 1", "Test Numeric Field 1",
                      "Test Numeric Field 2"
            )
        )
    )

    # can export IDD data in character format
    expect_equal(idd$to_string("TestSlash", all = TRUE),
        c(
        "TestSlash,",
        "    ,                        !- Test Character Field 1",
        "    ,                        !- Test Numeric Field 1 {m}",
        "    ,                        !- Test Numeric Field 2",
        "    ;                        !- Test Character Field 2"
        )

    )

    # can print without error
    expect_output(idd$print())

    # private functions
    expect_equal(get_priv_env(idd)$idd_env(), get_priv_env(idd)$m_idd_env)

    # private functions
    expect_equal(get_priv_env(idd)$log_env(), get_priv_env(idd)$m_log)
})
# }}}

# Idd S3 methods {{{
test_that("Idd S3 methods", {
    expect_silent(idd <- use_idd(idftext("idd", "9.9.9")))
    expect_equal(idd$TestSlash, idd$object("TestSlash"))
    expect_equal(idd[["TestSlash"]], idd$object("TestSlash"))
    expect_null(idd$Missing)
    expect_null(idd[["Missing"]])
    expect_error(idd$Missing <- "a", "cannot add bindings to a locked environment")
    expect_error(idd[["Missing"]] <- "a", "cannot add bindings to a locked environment")

    expect_silent(idd <- use_idd(idftext("idd", "9.9.9")))
    expect_equal(idd$TestSlash, idd$object("TestSlash"))
    expect_equal(idd[["TestSlash"]], idd$object("TestSlash"))
    expect_null(idd$Missing)
    expect_null(idd[["Missing"]])

    expect_equal(capture.output(str(idd)), capture.output(print(idd)))
    expect_equal(format(idd), "<EnergyPlus IDD v9.9.9 (7c3bbe4830) with 2 classes>")
    expect_equal(format(read_idd("!IDD_Version 9.9.9\n\\group TestGroup\nTest,\nA1; \\note something")),
        "<EnergyPlus IDD v9.9.9 with 1 class>")

    # can check equality
    expect_false(idd == "a")
    expect_true(idd == idd)
    expect_true(idd != "a")
    expect_false(idd != idd)
})
# }}}

# vim: set fdm=marker:
