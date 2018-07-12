context("Idd methods")

# idd_text {{{
idd_text <- c(
    "!IDD_Version 8.8.0
     !IDD_BUILD 7c3bbe4830
     \\group TestGroup1

     TestSimple,
     A1 ; \\field Test Field
       \\reference RefTestSimpleA1

     \\group TestGroup2
     TestSlash,
       \\memo This is just a test
       \\required-object
       \\unique-object
       \\min-fields 3
       \\format singleLine
       \\reference-class-name RefTestSlash
       \\extensible 4 !all fields are extensible
     A1 , \\field Test Character Field 1
       \\note Test Note Parsing
       \\required-field
       \\begin-extensible
       \\external-list autoRDDvariable
     N1 , \\field Test Numeric Field 1
       \\units m
       \\ip-units inch
       \\unitsbasedonfield A2
       \\minimum 1
       \\maximum< 10
       \\default 2
       \\autosizable
       \\type integer
     N2 , \\field Test Numeric Field 2
       \\autocalculatable
       \\type real
     A2 ; \\field Test Character Field 2
       \\type choice
       \\key Key1
       \\key Key2
       \\object-list RefTestSimpleA1")
# }}}

idd_parsed <- parse_idd_file(idd_text)

describe("parse_idd_file()", {
    # {{{
    it("can read Idd from string", expect_silent(idd_str <- read_idd_str(idd_text)))
    it("can parse Idd from string", {
        # {{{
        expect_equal(names(idd_parsed),
            c("version", "build",
              "group",
              "class", "class_reference",
              "field", "field_reference",
              "field_default", "field_choice", "field_range",
              "field_object_list", "field_external_list"))
        # }}}
    })


    it("can get Idd version", {
        expect_equal(idd_parsed$version, as.numeric_version("8.8.0"))
    })
    it("can get Idd build", expect_equal(idd_parsed$build, "7c3bbe4830"))
    it("can parse group data", {
        # {{{
        expect_equal(idd_parsed$group$group_id, 1:2)
        expect_equal(idd_parsed$group$group_name, c("TestGroup1", "TestGroup2"))
        # }}}
    })
    it("can parse class property data", {
        # {{{
        expect_equal(idd_parsed$class$class_id, 1:2)
        expect_equal(idd_parsed$class$class_name, c("TestSimple", "TestSlash"))
        expect_equal(idd_parsed$class$group_id, 1:2)
        expect_equal(idd_parsed$class$class_format, c("standard", "singleLine"))
        expect_equal(idd_parsed$class$memo, c(NA_character_, "This is just a test"))
        expect_equal(idd_parsed$class$min_fields, c(0, 3))
        expect_equal(idd_parsed$class$num_fields, c(1, 4))
        expect_equal(idd_parsed$class$required_object, c(FALSE, TRUE))
        expect_equal(idd_parsed$class$unique_object, c(FALSE, TRUE))
        expect_equal(idd_parsed$class$has_name, c(TRUE, FALSE))
        expect_equal(idd_parsed$class$last_required, c(0, 1))
        expect_equal(idd_parsed$class$num_extensible, c(0, 4))
        expect_equal(idd_parsed$class$first_extensible, c(0, 1))
        expect_equal(idd_parsed$class$num_extensible_group, c(0, 1))
        # }}}
    })
    it("can parse class reference data", {
        # {{{
        expect_equal(idd_parsed$class_reference$reference_id, 1)
        expect_equal(idd_parsed$class_reference$reference, "RefTestSlash")
        expect_equal(idd_parsed$class_reference$class_id, 2)
        # }}}
    })
    it("can parse field property data", {
        # {{{
        expect_equal(idd_parsed$field$field_id, 1:5)
        expect_equal(idd_parsed$field$class_id, c(1, rep(2,4)))
        expect_equal(idd_parsed$field$field_index, c(1, 1:4))
        nms <- c("Test Field",
                 "Test Character Field 1",
                 "Test Numeric Field 1",
                 "Test Numeric Field 2",
                 "Test Character Field 2")
        expect_equal(idd_parsed$field$field_name, nms)
        expect_equal(idd_parsed$field$full_name,
                     paste0(nms, c("", "", " {m}", "", "")))
        expect_equal(idd_parsed$field$full_ipname,
                     paste0(nms, c("", "", " {ft}", "", "")))
        expect_equal(idd_parsed$field$units,
                     c(NA_character_, NA_character_, "m", NA_character_, NA_character_))
        expect_equal(idd_parsed$field$ip_units,
                     c(NA_character_, NA_character_, "ft", NA_character_, NA_character_))
        expect_equal(idd_parsed$field$required_field,
                     c(FALSE, TRUE, FALSE, FALSE, FALSE))
        expect_equal(idd_parsed$field$type,
                     c("alpha", "alpha", "integer", "real", "choice"))
        expect_equal(idd_parsed$field$autosizable,
                     c(FALSE, FALSE, TRUE, FALSE, FALSE))
        expect_equal(idd_parsed$field$autocalculatable,
                     c(FALSE, FALSE, FALSE, TRUE, FALSE))
        expect_equal(idd_parsed$field$note,
                     c(NA_character_, "Test Note Parsing", NA_character_, NA_character_, NA_character_))
        expect_equal(idd_parsed$field$is_name,
                     c(TRUE, FALSE, FALSE, FALSE, FALSE))
        expect_equal(idd_parsed$field$is_extensible,
                     c(FALSE, TRUE, TRUE, TRUE, TRUE))
        expect_equal(idd_parsed$field$has_default,
                     c(FALSE, FALSE, TRUE, FALSE, FALSE))
        expect_equal(idd_parsed$field$has_range,
                     c(FALSE, FALSE, TRUE, FALSE, FALSE))
        expect_equal(idd_parsed$field$has_reference,
                     c(TRUE, FALSE, FALSE, FALSE, FALSE))
        expect_equal(idd_parsed$field$has_object_list,
                     c(FALSE, FALSE, FALSE, FALSE, TRUE))
        expect_equal(idd_parsed$field$has_external_list,
                     c(FALSE, TRUE, FALSE, FALSE, FALSE))
        # }}}
    })
    it("can parse field reference data", {
        # {{{
        expect_equal(idd_parsed$field_reference$reference_id, 1)
        expect_equal(idd_parsed$field_reference$reference, "RefTestSimpleA1")
        expect_equal(idd_parsed$field_reference$field_id, 1)
        # }}}
    })
    it("can parse field default data", {
        # {{{
        expect_equal(idd_parsed$field_default$field_id, 3)
        expect_equal(idd_parsed$field_default$default, "2")
        expect_equal(idd_parsed$field_default$default_upper, "2")
        expect_equal(idd_parsed$field_default$default_num, 2)
        expect_equivalent(idd_parsed$field_default$default_ipnum, 6.56168,
            tolerance = 0.0001)
        # }}}
    })
    it("can parse field choice data", {
        # {{{
        expect_equal(idd_parsed$field_choice$choice_id, 1:2)
        expect_equal(idd_parsed$field_choice$choice, c("Key1", "Key2"))
        expect_equal(idd_parsed$field_choice$choice_upper, c("KEY1", "KEY2"))
        expect_equal(idd_parsed$field_choice$field_id, c(5, 5))
        # }}}
    })
    it("can parse field range data", {
        # {{{
        expect_equal(idd_parsed$field_range$range_id, 1)
        expect_equal(idd_parsed$field_range$minimum, 1)
        expect_equal(idd_parsed$field_range$lower_incbounds, TRUE)
        expect_equal(idd_parsed$field_range$maximum, 10)
        expect_equal(idd_parsed$field_range$upper_incbounds, FALSE)
        expect_equal(idd_parsed$field_range$field_id, 3)
        # }}}
    })
    it("can parse field object list data", {
        # {{{
        expect_equal(idd_parsed$field_object_list$object_list_id, 1)
        expect_equal(idd_parsed$field_object_list$object_list, "RefTestSimpleA1")
        expect_equal(idd_parsed$field_object_list$field_id, 5)
        # }}}
    })
    it("can parse field external list data", {
        # {{{
        expect_equal(idd_parsed$field_external_list$external_list_id, 1)
        expect_equal(idd_parsed$field_external_list$external_list, "autoRDDvariable")
        expect_equal(idd_parsed$field_external_list$field_id, 2)
        # }}}
    })
    it("can detect error of missing IDD version", {
        # {{{
        idd_wrong <- c(
            "\\group TestGroup

             TestInvalidSlash,
             A1 ; \\note something

             Some Mess Here")
        expect_error(parse_idd_file(idd_wrong), "No IDD version found")
        # }}}
    })
    it("can detect error of multiple IDD versions", {
        # {{{
        idd_wrong <- c(
            "!IDD_Version 8.8.0
             !IDD_Version 8.7.0
             \\group TestGroup

             TestInvalidSlash,
             A1 ; \\note something

             Some Mess Here")
        expect_error(parse_idd_file(idd_wrong), "Multiple IDD version found")
        # }}}
    })
    it("can warn about missing IDD build tag", {
        # {{{
        idd_wrong <- c(
            "!IDD_Version 8.8.0
             \\group TestGroup

             TestInvalidSlash,
             A1 ; \\note something
            ")
        expect_warning(parse_idd_file(idd_wrong))
        # }}}
    })
    it("can warn about multiple IDD build tags", {
        # {{{
        idd_wrong <- c(
            "!IDD_Version 8.8.0
             !IDD_BUILD abc
             !IDD_BUILD def
             \\group TestGroup

             TestInvalidSlash,
             A1 ; \\note something
            ")
        expect_warning(parse_idd_file(idd_wrong))
        # }}}
    })
    it("can detect error of invaid line", {
        # {{{
        idd_wrong <- c(
            "!IDD_Version 8.8.0
             !IDD_BUILD 7c3bbe4830
             \\group TestGroup

             TestInvalidSlash,
             A1 ; \\note something

             Some Mess Here")
        expect_error(parse_idd_file(idd_wrong))
        # }}}
    })
    it("can detect error of invalid slash key", {
        # {{{
        idd_wrong <- c(
            "!IDD_Version 8.8.0
             !IDD_BUILD 7c3bbe4830
             \\group TestGroup

             TestInvalidSlash,
             A1 ; \\invalid-slash-key")
        expect_error(parse_idd_file(idd_wrong))
        # }}}
    })
    it("can detect error of invaid type key", {
        # {{{
        idd_wrong <- c(
            "!IDD_Version 8.8.0
             !IDD_BUILD 7c3bbe4830
             \\group TestGroup

             TestInvalidSlash,
             A1 ; \\type invalid")
        expect_error(parse_idd_file(idd_wrong))
        # }}}
    })
    it("can detect error of invaid external list key", {
        # {{{
        idd_wrong <- c(
            "!IDD_Version 8.8.0
             !IDD_BUILD 7c3bbe4830
             \\group TestGroup

             TestInvalidSlash,
             A1 ; \\external-list invalid")
        expect_error(parse_idd_file(idd_wrong))
        # }}}
    })
    it("can detect error of invalid format key", {
        # {{{
        idd_wrong <- c(
            "!IDD_Version 8.8.0
             !IDD_BUILD 7c3bbe4830
             \\group TestGroup

             TestInvalidSlash,
             A1 ; \\format invalid")
        expect_error(parse_idd_file(idd_wrong))
        # }}}
    })
    # }}}
})

describe("Idd$new()", {
    it("can create an Idd object from string", {
           expect_silent(idd <- eplusr:::Idd$new(idd_text))
    })
})

idd <- eplusr:::Idd$new(idd_text)

describe("$version()", {
    it("can get Idd version", {
        expect_equal(idd$version(), as.numeric_version("8.8.0"))
    })
})

describe("$build()", {
    it("can get Idd build", {
        expect_equal(idd$build(), "7c3bbe4830")
    })
})

describe("$group_name()", {
    it("can get all group names", {
        expect_equal(idd$group_name(), c("TestGroup1", "TestGroup2"))
    })
    it("can get group name of one class", {
        expect_equal(idd$from_group("TestSimple"), "TestGroup1")
    })
    it("can return when multiple class names are given", {
        expect_equal(idd$from_group(c("TestSlash", "TestSimple")),
            c("TestGroup2", "TestGroup1"))
    })
    it("can stop when invalid class name is given", {
        expect_error(idd$from_group("WrongClass"), "Invalid class name found")
    })
})

describe("$class_name()", {
    it("can return all class names", {
        expect_equal(idd$class_name(), c("TestSimple", "TestSlash"))
    })
})

describe("$group_index()", {
    it("can return an index of a single group", {
        expect_equal(idd$group_index("TestGroup1"), 1)
    })
    it("can return multiple group indexes", {
        expect_equal(idd$group_index(c("TestGroup2", "TestGroup1", "TestGroup2")),
            c(2L, 1L, 2L))
    })
    it("can stop when invalid group names are given", {
        expect_error(idd$group_index("WrongGroup"), "Invalid group name found")
    })
})

describe("$class_index()", {
    it("can return an index of a single class", {
        expect_equal(idd$class_index("TestSlash"), 2L)
    })
    it("can return multiple class indexes", {
        expect_equal(idd$class_index(c("TestSlash", "TestSimple", "TestSimple")),
            c(2L, 1L, 1L))
    })
    it("can stop when invalid class names are given", {
        expect_error(idd$class_index("WrongClass"), "Invalid class name found")
    })

})

describe("$required_class_name()", {
    it("can return names of all required classes", {
        expect_equal(idd$required_class_name(), "TestSlash")
    })
})

describe("$unique_class_name()", {
    it("can return names of all unique classes", {
        expect_equal(idd$unique_class_name(), "TestSlash")
    })
})

describe("$extensible_class_name()", {
    it("can return names of all extensible classes", {
        expect_equal(idd$extensible_class_name(), "TestSlash")
    })
})

describe("$object()", {
    it("can return a single IddObject using class name", {
        expect_is(idd$object("TestSimple")$TestSimple, "IddObject")
    })
    it("can stop when invalid class names are given", {
        expect_error(idd$object("WrongClass"), "Invalid class name found: `WrongClass`.")
    })
    it("can return when multiple class names are given", {
        expect_equal(idd$object(c("TestSimple", "TestSlash")),
            list(TestSimple = idd$object("TestSimple")$TestSimple,
                TestSlash = idd$object("TestSlash")$TestSlash))
    })
})

describe("$object_in_group()", {
    it("can return all IddObjects in a group", {
        expect_is(idd$object_in_group("TestGroup1"), "list")
        expect_equal(idd$object_in_group("TestGroup1"), list(TestSimple = idd$object("TestSimple")$TestSimple))
    })
    it("can stop when invalid group names are given", {
        expect_error(idd$object_in_group("WrongGroup"), "Invalid group name found")
    })
    it("can stop when multiple group names are given", {
        expect_error(idd$object_in_group(c("TestGroup1", "TestGroup2")),
                     "group is not a string")
    })
})

describe("$is_valid_group()", {
    it("works", {
        expect_false(idd$is_valid_group("WrongGroup"))
        expect_true(idd$is_valid_group("TestGroup1"))
    })
})

describe("$is_valid_class()", {
    it("works", {
        expect_false(idd$is_valid_class("WrongClass"))
        expect_true(idd$is_valid_class("TestSlash"))
    })
})

describe("$print()", {
    it("works without any warning or error", {
        expect_output(idd$print())
    })
})

test_that("S3 subset", {
    expect_equal(idd$TestSlash, idd$object("TestSlash")[[1]])
    expect_equal(idd[["TestSlash"]], idd$object("TestSlash")[[1]])
})
