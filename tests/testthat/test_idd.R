context("Parse method")

test_that("Parse IDD works", {

    text_idd <- c(
        "!IDD_Version 8.8.0\n
         !IDD_BUILD 7c3bbe4830\n
         \\group TestGroup\n
         TestClass,\n
           \\memo This is just a test\n
           \\unique-object\n
           \\format singleLine\n
         A1 ; \\field Test Field\n
         \\default 8.8\n")

    expect_silent(idd_str <- read_idd(text_idd))
    expect_equal(get_idd_ver(idd_str), "8.8.0")
    expect_equal(get_idd_build(idd_str), "7c3bbe4830")

    expect_silent(parse_idd(text_idd))
    expect_silent(idd <- IDD$new(text_idd))

    expect_equal(idd$version(), "8.8.0")
    expect_equal(idd$build(), "7c3bbe4830")

    expect_equal(idd$group_name(), "TestGroup")
    expect_equal(idd$group_name("TestClass"), "TestGroup")
    expect_error(idd$group_name("WrongClass"),
                 paste0("Invalid class name found: ", sQuote("WrongClass"), "."))

    expect_equal(idd$class_name(), "TestClass")
    expect_equal(idd$class_name("TestGroup"), "TestClass")
    expect_error(idd$class_name("WrongClass"),
                 paste0("Invalid class name found: ", sQuote("WrongClass"), "."))

    expect_equal(idd$group_order(), c(`TestGroup` = 1L))
    expect_equal(idd$group_order("TestGroup"), c(`TestGroup` = 1L))
    expect_error(idd$group_order("WrongGroup"),
                 paste0("Invalid group name found: ", sQuote("WrongGroup"), "."))

    expect_equal(idd$class_order(), c(`TestClass` = 1L))
    expect_equal(idd$class_order("TestClass"), c(`TestClass` = 1L))
    expect_error(idd$class_order("WrongClass"),
                 paste0("Invalid class name found: ", sQuote("WrongClass"), "."))

    expect_equal(idd$is_valid_group(c("TestGroup", "WrongGroup")), c(TRUE, FALSE))
    expect_equal(idd$is_valid_class(c("TestClass", "WrongClass")), c(TRUE, FALSE))

    skip_on_cran(
        expect_silent(IDD$new("../../idd/V8-8-0-Energy+.idd"))
        expect_silent(IDD$new("../../idd/V8-7-0-Energy+.idd"))
        expect_silent(IDD$new("../../idd/V8-6-0-Energy+.idd"))
        expect_silent(IDD$new("../../idd/V8-5-0-Energy+.idd"))
        expect_silent(IDD$new("../../idd/V8-4-0-Energy+.idd"))
        expect_silent(IDD$new("../../idd/V8-3-0-Energy+.idd"))
        expect_silent(IDD$new("../../idd/V8-2-0-Energy+.idd"))
        expect_silent(IDD$new("../../idd/V8-1-0-Energy+.idd"))
    )
})
assertthat::on_failure(IDDObject$public_methods$is_valid_field_num) <- function (call, env) {
    paste0(eval(self$class_name(),env))
}
idd <- IDD$new(text_idd)
idd["TestClass"]$is_valid_field_num(3)
assertthat::assert_that(idd["TestClass"]$is_valid_field_num(3))
