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

describe("parse_idd_file()", {
    it("can read Idd from string", {
        expect_silent(idd_str <- read_idd_str(idd_text))
    })
    it("can parse Idd from string", {
        expect_silent(idd_parsed <- parse_idd_file(idd_text))
        expect_equal(names(idd_parsed),
            c("version", "build",
              "group",
              "class", "class_property", "class_reference",
              "field", "field_property", "field_reference",
              "field_choice", "field_range", "field_extensible",
              "field_object_list", "field_external_list"))
    })

    idd_parsed <- parse_idd_file(idd_text)

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
    it("can parse class data", {
        # {{{
        expect_equal(idd_parsed$class$class_id, 1:2)
        expect_equal(idd_parsed$class$class_name, c("TestSimple", "TestSlash"))
        expect_equal(idd_parsed$class$group_id, 1:2)
        # }}}
    })
    it("can parse class property data", {
        # {{{
        expect_equal(idd_parsed$class_property$class_id, 1:2)
        expect_equal(idd_parsed$class_property$class_format, c("standard", "singleLine"))
        expect_equal(idd_parsed$class_property$memo, c(NA_character_, "This is just a test"))
        expect_equal(idd_parsed$class_property$min_fields, c(0, 3))
        expect_equal(idd_parsed$class_property$num_fields, c(1, 4))
        expect_equal(idd_parsed$class_property$required_object, c(FALSE, TRUE))
        expect_equal(idd_parsed$class_property$unique_object, c(FALSE, TRUE))
        expect_equal(idd_parsed$class_property$has_name, c(FALSE, FALSE))
        # }}}
    })
    it("can parse class reference data", {
        # {{{
        expect_equal(idd_parsed$class_reference$reference_id, 1)
        expect_equal(idd_parsed$class_reference$reference, "RefTestSlash")
        expect_equal(idd_parsed$class_reference$class_id, 2)
        # }}}
    })
    it("can parse field data", {
        # {{{
        expect_equal(idd_parsed$field$field_id, 1:5)
        expect_equal(idd_parsed$field$class_id, c(1, rep(2,4)))
        expect_equal(idd_parsed$field$field_order, c(1, 1:4))
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
        # }}}
    })
    it("can parse field property data", {
        # {{{
        expect_equal(idd_parsed$field_property$field_id, 1:5)
        expect_equal(idd_parsed$field_property$units,
                     c(NA_character_, NA_character_, "m", NA_character_, NA_character_))
        expect_equal(idd_parsed$field_property$ip_units,
                     c(NA_character_, NA_character_, "ft", NA_character_, NA_character_))
        expect_equal(idd_parsed$field_property$required_field,
                     c(FALSE, TRUE, FALSE, FALSE, FALSE))
        expect_equal(idd_parsed$field_property$type,
                     c("alpha", "alpha", "integer", "real", "choice"))
        expect_equal(idd_parsed$field_property$field_default,
                     c(NA_character_, NA_character_, "2", NA_character_, NA_character_))
        expect_equal(idd_parsed$field_property$autosizable,
                     c(FALSE, FALSE, TRUE, FALSE, FALSE))
        expect_equal(idd_parsed$field_property$autocalculatable,
                     c(FALSE, FALSE, FALSE, TRUE, FALSE))
        expect_equal(idd_parsed$field_property$note,
                     c(NA_character_, "Test Note Parsing", NA_character_, NA_character_, NA_character_))
        expect_equal(idd_parsed$field_property$begin_extensible,
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
    it("can parse field extensible data", {
        # {{{
        expect_equal(idd_parsed$field_extensible$extensible_id, 1:4)
        expect_equal(idd_parsed$field_extensible$field_id, 2:5)
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
})

describe("Idd$new()", {
    it("can create an Idd object from string", {
           expect_silent(idd <- Idd$new(idd_text))
    })

    skip_on_cran()
    it("can parse Idd files from 8.1 to 8.8 without any error", {
        expect_silent(Idd$new("../../idd/V8-8-0-Energy+.idd"))
        expect_silent(Idd$new("../../idd/V8-7-0-Energy+.idd"))
        expect_silent(Idd$new("../../idd/V8-6-0-Energy+.idd"))
        expect_silent(Idd$new("../../idd/V8-5-0-Energy+.idd"))
        expect_silent(Idd$new("../../idd/V8-4-0-Energy+.idd"))
        expect_silent(Idd$new("../../idd/V8-3-0-Energy+.idd"))
        expect_silent(Idd$new("../../idd/V8-2-0-Energy+.idd"))
        # there is no build tag in this file
        expect_warning(Idd$new("../../idd/V8-1-0-Energy+.idd"))
    })
})

idd <- read_idd(idd_text)

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

describe("$group_names()", {
    it("can get all group names", {
        expect_equal(idd$group_names(), c("TestGroup1", "TestGroup2"))
    })
    it("can get group name of one class", {
        expect_equal(idd$group_names("TestSimple"), c(TestSimple = "TestGroup1"))
    })
    it("can return when multiple class names are given", {
        expect_equal(idd$group_names(c("TestSlash", "TestSimple")),
            c(TestSlash = "TestGroup2", TestSimple = "TestGroup1"))
    })
    it("can stop when invalid class name is given", {
        expect_error(idd$group_names("WrongClass"), "Invalid class name found")
    })
})

describe("$class_names()", {
    it("can return all class names", {
        expect_equal(idd$class_names(), c("TestSimple", "TestSlash"))
    })
    it("can return class names of a single group", {
        expect_equal(idd$class_names("TestGroup2"), c(TestGroup2 = "TestSlash"))
    })
    it("can return when multiple group names are given", {
        expect_equal(idd$class_names(c("TestGroup2", "TestGroup1")),
            c(TestGroup2 = "TestSlash", TestGroup1 = "TestSimple"))
    })
    it("can stop when invalid group name is given", {
        expect_error(idd$class_names("WrongGroup"), "Invalid group name found")
    })
})

describe("$required_class_names()", {
    it("can return names of all required classes", {
        expect_equal(idd$required_class_names(), "TestSlash")
    })
})

describe("$unique_class_names()", {
    it("can return names of all unique classes", {
        expect_equal(idd$unique_class_names(), "TestSlash")
    })
})

describe("$extensible_class_names()", {
    it("can return names of all extensible classes", {
        expect_equal(idd$extensible_class_names(), "TestSlash")
    })
})

describe("$group_order()", {
    it("can return an order of a single group", {
        expect_equal(idd$group_orders("TestGroup1"), c("TestGroup1" = 1L))
    })
    it("can return multiple group orders", {
        expect_equal(idd$group_orders(c("TestGroup2", "TestGroup1")),
            c(TestGroup2 = 2L, TestGroup1 = 1L))
    })
    it("can stop when invalid group names are given", {
        expect_error(idd$group_orders("WrongGroup"), "Invalid group name found")
    })
})

describe("$class_order()", {
    it("can return an order of a single class", {
        expect_equal(idd$class_orders("TestSlash"), c("TestSlash" = 2L))
    })
    it("can return multiple group orders", {
        expect_equal(idd$group_orders(c("TestGroup2", "TestGroup1")),
            c(TestGroup2 = 2L, TestGroup1 = 1L))
    })
    it("can stop when invalid class names are given", {
        expect_error(idd$class_orders("WrongClass"), "Invalid class name found")
    })

})

describe("$object()", {
    it("can return a single IddObject using class name", {
        expect_is(idd$object("TestSimple"), "IddObject")
    })
    it("can stop when invalid class names are given", {
        expect_error(idd$object("WrongClass"), "Invalid class name found: `WrongClass`.")
    })
    it("can stop when multiple class names are given", {
        expect_error(idd$object(c("TestSimple", "TestSlash")), "class is not a string.")
    })
})

describe("$objects_in_group()", {
    it("can return all IddObjects in a group", {
        expect_is(idd$objects_in_group("TestGroup1"), "list")
        expect_equal(idd$objects_in_group("TestGroup1"),
                     list(idd$object("TestSimple")))
    })
    it("can stop when invalid group names are given", {
        expect_error(idd$objects_in_group("WrongGroup"), "Invalid group name found")
    })
    it("can stop when multiple group names are given", {
        expect_error(idd$objects_in_group(c("TestGroup1", "TestGroup2")),
                     "group is not a string")
    })
})

describe("$required_objects()", {
    it("can return all required IddObjects", {
        expect_equal(idd$required_objects(), list(idd$object("TestSlash")))
    })
})

describe("$unique_objects()", {
    it("can return all unique IddObjects", {
        expect_equal(idd$unique_objects(), list(idd$object("TestSlash")))
    })
})

describe("$reference_map()", {
    it("can return reference map of a single calss", {
        expect_equal(idd$reference_map("TestSlash"),
                     list(reference_class = character(0),
                          reference_field = character(0),
                          object_list = "TestSimple",
                          external_list = "eplusout.rdd"))
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
