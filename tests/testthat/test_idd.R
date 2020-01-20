context("Idd and IddObject")

eplusr_option(verbose_info = FALSE)

# download_idd() {{{
test_that("can download IDD from EnergyPlus repo", {
    skip_on_cran()
    expect_silent(download_idd(8.8, tempdir()))
    expect_true(file.exists(file.path(tempdir(), "V8-8-0-Energy+.idd")))

    expect_silent(download_idd("latest", tempdir()))
    expect_true(file.exists(file.path(tempdir(), "V9-2-0-Energy+.idd")))
})
# }}}

# use_idd() {{{
test_that("can read IDD", {
    skip_on_cran()
    glo <- eplusr:::`.globals`
    glo$idd <- list()
    expect_error(is_avail_idd("latest"))
    expect_equal(avail_idd(), NULL)
    if (any(avail_eplus() >= 8.4)) {
        expect_silent(use_idd(8.4))
    } else {
        expect_error(use_idd(8.4))
    }

    expect_silent(use_idd(8.4, download = TRUE))
    expect_silent(use_idd("latest", download = TRUE))
    expect_true(file.exists(file.path(tempdir(), "V8-4-0-Energy+.idd")))
    expect_is(use_idd("8.4.0"), "Idd")
    expect_true(numeric_version("8.4.0") %in% avail_idd())
    expect_true(is_avail_idd(8.4))
    expect_true(is_avail_idd("8.4"))
    expect_true(is_avail_idd("8.4.0"))
    expect_error(is_avail_idd("latest"))

    expect_silent(use_idd(8.7, download = "auto"))
    expect_equal(avail_idd(), numeric_version(c("8.4.0", "8.7.0", "9.2.0")))

    expect_silent(use_idd(text("idd", "9.9.9")))
    expect_true(is_avail_idd("9.9.9"))

    # can parse old IDD
    expect_warning(use_idd(7.2, download = "auto"))
    expect_warning(use_idd(8.0, download = "auto"))
    expect_warning(use_idd(8.1, download = "auto"))
})
# }}}

# Idd class {{{
test_that("Idd class", {
    .options$autocomplete <- TRUE
    # can create an Idd object from string
    expect_silent(idd <- use_idd(text("idd", "9.9.9")))

    # can get Idd version
    expect_equal(idd$version(), as.numeric_version("9.9.9"))

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
    expect_error(idd$from_group("WrongClass"), class = "error_class_name")

    # can return all class names
    expect_equal(idd$class_name(), c("TestSimple", "TestSlash"))

    # can return an index of a single group
    expect_equal(idd$group_index("TestGroup1"), 1)

    # can return multiple group indexes
    expect_equal(idd$group_index(c("TestGroup2", "TestGroup1", "TestGroup2")),
        c(2L, 1L, 2L))

    # can stop when invalid group names are given
    expect_error(idd$group_index("WrongGroup"), class = "error_group_name")

    # can return an index of a single class
    expect_equal(idd$class_index("TestSlash"), 2L)

    # can return multiple class indexes
    expect_equal(idd$class_index(c("TestSlash", "TestSimple", "TestSimple")),
        c(2L, 1L, 1L))

    # can stop when invalid class names are given
    expect_error(idd$class_index("WrongClass"), error = "error_class_name")

    expect_is(idd$object_relation("TestSimple"), "IddRelation")
    expect_is(idd$object_relation("TestSimple", "ref_to"), "IddRelation")
    expect_is(idd$object_relation("TestSimple", "ref_by"), "IddRelation")

    # can return names of all required classes
    expect_equal(idd$required_class_name(), "TestSlash")

    # can return names of all unique-object classes
    expect_equal(idd$unique_class_name(), "TestSlash")

    # can return names of all extensible classes
    expect_equal(idd$extensible_class_name(), "TestSlash")

    # can return a single IddObject using class name
    expect_is(idd$object("TestSimple"), "IddObject")

    # can stop when invalid class names are given
    expect_error(idd$object("WrongClass"), error = "error_class_name_us")

    # can return when multiple class names are given
    expect_equal(idd$objects(c("TestSimple", "TestSlash")),
        list(TestSimple = idd$object("TestSimple"),
            TestSlash = idd$object("TestSlash")))

    # can return all IddObjects in a group
    expect_is(idd$objects_in_group("TestGroup1"), "list")
    expect_equal(idd$objects_in_group("TestGroup1"), list(TestSimple = idd$object("TestSimple")))

    # can stop when invalid group names are given
    expect_error(idd$objects_in_group("WrongGroup"), class = "error_group_name")

    # can stop when multiple group names are given
    expect_error(idd$objects_in_group(c("TestGroup1", "TestGroup2")), class = "error_not_string")

    expect_is(idd$objects_in_relation("TestSimple", "ref_to"), "list")
    expect_equal(names(idd$objects_in_relation("TestSimple", "ref_to")), "TestSimple")
    expect_is(idd$objects_in_relation("TestSimple", "ref_by"), "list")
    expect_equal(names(idd$objects_in_relation("TestSimple", "ref_by")), c("TestSimple", "TestSlash"))

    # can check if input is a valid group
    expect_false(idd$is_valid_group("WrongGroup"))
    expect_true(idd$is_valid_group("TestGroup1"))

    # can check if input is a valid class
    expect_false(idd$is_valid_class("WrongClass"))
    expect_true(idd$is_valid_class("TestSlash"))

    # can print without error
    expect_output(idd$print())

    # can get single object using S3 method
    expect_equal(idd$TestSlash, idd$object("TestSlash"))
    expect_equal(idd[["TestSlash"]], idd$object("TestSlash"))

    expect_is(idd$object("TestSlash"), "IddObject")
    expect_is(idd$objects_in_group("TestGroup1")[[1L]], "IddObject")
})
# }}}
