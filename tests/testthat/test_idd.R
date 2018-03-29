context("IDD methods")

idd_normal <- c(
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
       \\type alpha
     A2 ; \\field Test Character Field 2
       \\key Key1
       \\key Key2
       \\object-list RefTestSimpleA1")

describe("parse_idd()", {
    it("can read IDD from string", {
        expect_silent(idd_str <- read_idd(idd_normal))
    })

    it("can parse IDD from string", {
        expect_silent(idd_parsed <- parse_idd(idd_normal))
        expect_equal(names(idd_parsed), c("version", "build", "order", "data",
                                          "reference", "external"))
    })

    it("can get IDD version", {
        expect_equal(idd_parsed$version, "8.8.0")
    })

    it("can get IDD build", {
        expect_equal(idd_parsed$build, "7c3bbe4830")
    })

    it("can parse class data", {
        # {{{
        expect_equal(idd_parsed$data$group_order, 1:2)
        expect_equal(idd_parsed$data$group, c("TestGroup1", "TestGroup2"))
        expect_equal(idd_parsed$data$class_order, 1:2)
        expect_equal(idd_parsed$data$class, c("TestSimple", "TestSlash"))
        expect_equal(idd_parsed$data$format, c("standard", "singleLine"))
        expect_equal(idd_parsed$data$memo, c(NA_character_, "This is just a test"))
        expect_equal(idd_parsed$data$extensible, c(0, 4))
        expect_equal(idd_parsed$data$reference, list(NA_character_, "RefTestSlash"))
        expect_equal(length(idd_parsed$data$data_class), 2)
        expect_equal(class(idd_parsed$data$data_class), "list")
        expect_is(idd_parsed$data$data_class[[1]], "data.table")
        expect_equivalent(idd_parsed$data$data_class[[1]],
            data.table::data.table(
                group_order = 1L,
                group = "TestGroup1",
                class_order = 1L,
                class = "TestSimple",
                format = "standard",
                memo = NA_character_,
                min_fields = 0L,
                num_fields = 1L,
                required_object = FALSE,
                unique_object = FALSE,
                extensible = 0L,
                has_reference = FALSE,
                reference = list(NA_character_)
            )
        )
        expect_equivalent(idd_parsed$data$data_class[[2]],
            data.table::data.table(
                group_order = 2L,
                group = "TestGroup2",
                class_order = 2L,
                class = "TestSlash",
                format = "singleLine",
                memo = "This is just a test",
                min_fields = 3L,
                num_fields = 4L,
                required_object = TRUE,
                unique_object = TRUE,
                extensible = 4L,
                has_reference = TRUE,
                reference = list("RefTestSlash")
            )
        )
        # }}}
    })

    it("can parse field data", {
        # {{{
        expect_equivalent(idd_parsed$data$data_field[[1]],
            data.table::setnames(data.table::data.table(
                class_order = 1L,
                class = "TestSimple",
                field_order = 1L,
                field = "Test Field",
                field_anid = "A1",
                field_an = "A",
                field_id = "1",
                units = NA_character_,
                ip_units = NA_character_,
                required_field = FALSE,
                type = NA_character_,
                have_range = FALSE,
                range = list(NULL),
                default = NA_character_,
                choice = list(NA_character_),
                autosizable = FALSE,
                autocalculatable = FALSE,
                note = NA_character_,
                begin_extensible = FALSE,
                has_reference = TRUE,
                has_object_list = FALSE,
                has_external_list = FALSE,
                unitsbasedonfield = FALSE,
                `_field_name` = "Test Field",
                `_unit` = "",
                `_ip_unit` = "",
                `_field` = "Test Field",
                `_field_ip` = "Test Field",
                reference  = list("RefTestSimpleA1"),
                object_list = list(NA_character_),
                external_list = list(NA_character_)), "choice", "key"
            )
        )

        expect_equivalent(idd_parsed$data$data_field[[2]],
            data.table::setnames(data.table::data.table(
                class_order = rep(2L, 4L),
                class = rep("TestSlash", 4L),
                field_order = 1L:4L,
                field = c("Test Character Field 1",
                          "Test Numeric Field 1",
                          "Test Numeric Field 2",
                          "Test Character Field 2"),
                field_anid = c("A1", "N1", "N2", "A2"),
                field_an = c("A", "N", "N", "A"),
                field_id = c("1", "1", "2", "2"),
                units = c(NA_character_, "m", NA_character_, NA_character_),
                ip_units = c(NA_character_, "inch", NA_character_, NA_character_),
                required_field = c(TRUE, rep(FALSE, 3L)),
                type = c(NA_character_, "integer", "alpha", NA_character_),
                have_range = c(FALSE, TRUE, FALSE, FALSE),
                range = list(NULL,
                             list(lower = 1L, lower_incbounds = TRUE,
                                  upper = 10, upper_incbounds = FALSE),
                             NULL,
                             NULL),
                default = c(NA_character_, "2", NA_character_, NA_character_),
                choice = list(NA_character_, NA_character_, NA_character_, c("Key1", "Key2")),
                autosizable = c(FALSE, TRUE, FALSE, FALSE),
                autocalculatable = c(FALSE, FALSE, TRUE, FALSE),
                note = c("Test Note Parsing", NA_character_, NA_character_, NA_character_),
                begin_extensible = c(TRUE, FALSE, FALSE, FALSE),
                has_reference = c(FALSE, FALSE, FALSE, FALSE),
                has_object_list = c(FALSE, FALSE, FALSE, TRUE),
                has_external_list = c(TRUE, FALSE, FALSE, FALSE),
                unitsbasedonfield = rep(FALSE, 4L),
                `_field_name` = c("Test Character Field 1",
                                  "Test Numeric Field 1",
                                  "Test Numeric Field 2",
                                  "Test Character Field 2"),
                `_unit` = c("", "{m}", "", ""),
                `_ip_unit` = c("", "{inch}", "", ""),
                `_field` = c("Test Character Field 1",
                             "Test Numeric Field 1 {m}",
                             "Test Numeric Field 2",
                             "Test Character Field 2"),
                `_field_ip` = c("Test Character Field 1",
                                "Test Numeric Field 1 {inch}",
                                "Test Numeric Field 2",
                                "Test Character Field 2"),
                reference  = list(NA_character_, NA_character_, NA_character_, NA_character_),
                object_list = list(NA_character_, NA_character_, NA_character_, "RefTestSimpleA1"),
                external_list = list("autoRDDvariable", NA_character_, NA_character_, NA_character_)
                ),
            "choice", "key")
        )
        # }}}
    })

    it("can handle exceptions", {
        # {{{
        idd_wrong1 <- c(
            "!IDD_Version 8.8.0
             !IDD_BUILD 7c3bbe4830
             \\group TestGroup

             TestInvalidSlash,
             A1 ; \\note something

             Some Mess Here")
        expect_error(parse_idd(idd_wrong1))

        idd_wrong2 <- c(
            "!IDD_Version 8.8.0
             !IDD_BUILD 7c3bbe4830
             \\group TestGroup

             TestInvalidSlash,
             A1 ; \\invalid-slash-key")
        expect_error(parse_idd(idd_wrong2))

        idd_wrong3 <- c(
            "\\group TestGroup

             TestInvalidSlash,
             A1 ; \\type invalid")
        expect_error(parse_idd(idd_wrong3))

        idd_wrong4 <- c(
            "\\group TestGroup

             TestInvalidSlash,
             A1 ; \\external-list invalid")
        expect_error(parse_idd(idd_wrong4))

        idd_wrong5 <- c(
            "\\group TestGroup

             TestInvalidSlash,
             A1 ; \\format invalid")
        expect_error(parse_idd(idd_wrong5))
        # }}}
    })
})

describe("IDD$new()", {
    it("can create an IDD object from string", {
           expect_silent(idd <- IDD$new(idd_normal))
    })

    skip_on_cran()
    it("can parse IDD files from 8.1 to 8.8 without any error", {
        expect_silent(IDD$new("../../idd/V8-8-0-Energy+.idd"))
        expect_silent(IDD$new("../../idd/V8-7-0-Energy+.idd"))
        expect_silent(IDD$new("../../idd/V8-6-0-Energy+.idd"))
        expect_silent(IDD$new("../../idd/V8-5-0-Energy+.idd"))
        expect_silent(IDD$new("../../idd/V8-4-0-Energy+.idd"))
        expect_silent(IDD$new("../../idd/V8-3-0-Energy+.idd"))
        expect_silent(IDD$new("../../idd/V8-2-0-Energy+.idd"))
        expect_silent(IDD$new("../../idd/V8-1-0-Energy+.idd"))
    })
})

describe("$version()", {
    it("can get IDD version", {
        expect_equal(idd$version(), "8.8.0")
    })
})

describe("$build()", {
    it("can get IDD build", {
        expect_equal(idd$build(), "7c3bbe4830")
    })
})

describe("$group_name()", {
    it("can get all group names", {
        expect_equal(idd$group_name(), c("TestGroup1", "TestGroup2"))
    })
    it("can get group name of one class", {
        expect_equal(idd$group_name("TestSimple"), "TestGroup1")
    })
    it("can stop when multiple class names are given", {
        expect_error(idd$group_name(c("TestSimple", "TestSlash")))
    })
    it("can stop when invalid class name is given", {
        expect_error(idd$group_name("WrongClass"),
                     paste0("Invalid class name found: ", backtick("WrongClass"), "."))
    })
})

describe("$class_name()", {
    it("can return all class names", {
        expect_equal(idd$class_name(), c("TestSimple", "TestSlash"))
    })
    it("can return class names of a single group", {
        expect_equal(idd$class_name("TestGroup2"), "TestSlash")
    })
    it("can stop when multiple group names are given", {
        expect_error(idd$class_name(c("TestGroup1", "TestGroup2")))
    })
    it("can stop when invalid group name is given", {
        expect_error(idd$class_name("WrongGroup"),
                     paste0("Invalid group name found: ", backtick("WrongGroup"), "."))
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

describe("$group_order()", {
    it("can return all group orders", {
           expect_equal(idd$group_order(), c(`TestGroup1` = 1L, `TestGroup2` = 2L))
    })
    it("can return an order of a single group", {
        expect_equal(idd$group_order("TestGroup1"), c(`TestGroup1` = 1L))
    })

    it("can stop when invalid group names are given", {
        expect_error(idd$group_order("WrongGroup"),
                     paste0("Invalid group name found: ", backtick("WrongGroup"), "."))
    })
})

describe("$class_order()", {
    it("can return all class orders", {
        expect_equal(idd$class_order(), c(`TestSimple` = 1L, `TestSlash` = 2L))
    })
    it("can return an order of a single class", {
        expect_equal(idd$class_order("TestSlash"), c(`TestSlash` = 2L))
    })
    it("can stop when invalid class names are given", {
        expect_error(idd$class_order("WrongClass"),
                     paste0("Invalid class name found: ", backtick("WrongClass"), "."))
    })

})

describe("$object()", {
    it("can return a single IDDObject using class name", {
        expect_is(idd$object("TestSimple"), "IDDObject")
    })
    it("can stop when invalid class names are given", {
        expect_error(idd$object("WrongClass"),
                     "Invalid class name found: `WrongClass`.")
    })
    it("can stop when multiple class names are given", {
        expect_error(idd$object(c("TestSimple", "TestSlash")),
                     "class is not a string.")
    })
})

describe("$objects()", {
    it("can return multiple IDDObjects using class name", {
        expect_is(idd$objects("TestSimple"), "list")
        expect_equal(idd$objects("TestSimple")[[1]], idd$object("TestSimple"))
    })
    it("can return all IDDObjects if no class name is given", {
        expect_is(idd$objects(), "list")
        expect_equal(idd$objects(), list(idd$object("TestSimple"), idd$object("TestSlash")))
    })
    it("can stop when invalid class names are given", {
        expect_error(idd$objects("WrongClass"),
                     "Invalid class name found: `WrongClass`.")
    })
})

describe("$objects_in_group()", {
    it("can return all IDDObjects in a group", {
        expect_equal(idd$objects_in_group("TestGroup1"), idd$objects("TestSimple"))
    })
    it("can stop when invalid group names are given", {
        expect_error(idd$objects_in_group("WrongGroup"),
                     "Invalid group name found: `WrongGroup`")
    })
    it("can stop when multiple group names are given", {
        expect_error(idd$objects_in_group(c("TestGroup1", "TestGroup2")))
    })
})

describe("$required_objects()", {
    it("can return all required IDDObjects", {
        expect_equal(idd$required_objects(), idd$objects("TestSlash"))
    })
})

describe("$unique_objects()", {
    it("can return all unique IDDObjects", {
        expect_equal(idd$unique_objects(), idd$objects("TestSlash"))
    })
})

describe("$reference()", {
    it("can return all reference data", {
        expect_equal(idd$reference(), idd_parsed$reference)
    })
})

describe("$external()", {
    it("can return all external data", {
        expect_equal(idd$external(), idd_parsed$external)
    })
})

describe("$is_valid_group", {
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
