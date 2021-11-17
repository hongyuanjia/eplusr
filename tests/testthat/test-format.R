test_that("Formatting", {
    # only test on UTF-8 supported platform
    skip_if_not(cli::is_utf8_output())

    # IDD {{{
    idd_parsed <- parse_idd_file(idftext("idd", "9.9.9"))

    expect_equal(format_name(idd_parsed$field),
        c("!- Test Field",
          "!- Test Character Field 1",
          "!- Test Numeric Field 1 {m}",
          "!- Test Numeric Field 2",
          "!- Test Character Field 2"
        )
    )

    expect_equal(format_index(idd_parsed$field), as.character(c(1, 1:4)))

    expect_equal(format_index(idd_parsed$field, required = TRUE),
        c("1 ", "1*", "2 ", "3 ", "4 ")
    )

    expect_equal(format_field(idd_parsed$field, leading = 2),
        c("  !- Test Field",
          "  !- Test Character Field 1",
          "  !- Test Numeric Field 1 {m}",
          "  !- Test Numeric Field 2",
          "  !- Test Character Field 2"
        )
    )

    expect_equal(format_field(idd_parsed$field, leading = 2, prefix = FALSE),
        c("  Test Field",
          "  Test Character Field 1",
          "  Test Numeric Field 1 {m}",
          "  Test Numeric Field 2",
          "  Test Character Field 2"
        )
    )
    expect_equal(format_objects(idd_parsed$group, component = "group")$out,
        c("Group: <TestGroup1>", "Group: <TestGroup2>")
    )
    expect_error(format_objects(idd_parsed$group, component = c("group", "class"))$out)
    expect_equal(format_objects(idd_parsed$class, component = "class")$out,
        c("Class: <TestSimple>", "Class: <TestSlash>")
    )
    expect_equal(
        format_objects(idd_parsed$group[idd_parsed$class, on = "group_id"],
            component = c("group", "class"), brief = TRUE)$out,
        c("[1<C>] Group: <TestGroup1>", "[1<C>] Group: <TestGroup2>")
    )
    expect_equal(
        format_objects(idd_parsed$group[idd_parsed$class, on = "group_id"],
            component = c("group", "class"), brief = FALSE)$out,
        list(
            c("Group: <TestGroup1>", "└─ Class: <TestSimple>", ""),
            c("Group: <TestGroup2>", "└─ Class: <TestSlash>", "")
        )
    )
    expect_equal(format_objects(idd_parsed$field, component = "field")$out,
        c("Field: <1: Test Field>",
          "Field: <1: Test Character Field 1>",
          "Field: <2: Test Numeric Field 1 {m}>",
          "Field: <3: Test Numeric Field 2>",
          "Field: <4: Test Character Field 2>"
        )
    )
    expect_equal(
        format_objects(
            idd_parsed$class[, .(group_id, class_id, class_name)][
            idd_parsed$field[, .(class_id, field_id, field_index, field_name, units, ip_units)], on = "class_id"],
            c("class", "field"), brief = FALSE)$out,
        list(
            c("Class: <TestSimple>",
              "└─ Field: <1: Test Field>", ""
            ),
            c("Class: <TestSlash>",
              "├─ Field: <1: Test Character Field 1>",
              "│─ Field: <2: Test Numeric Field 1 {m}>",
              "│─ Field: <3: Test Numeric Field 2>",
              "└─ Field: <4: Test Character Field 2>",
              ""
            )
        )
    )
    expect_equal(
        format_objects(
            idd_parsed$group[
            idd_parsed$class[, .(group_id, class_id, class_name)], on = "group_id"][
            idd_parsed$field[, .(class_id, field_id, field_index, field_name, units, ip_units)], on = "class_id"],
            c("group", "class", "field"))$out,
        list(
            c("Group: <TestGroup1>",
              "└─ [1<F>] Class: <TestSimple>",
              ""
            ),
            c("Group: <TestGroup2>",
              "└─ [4<F>] Class: <TestSlash>",
              ""
            )
        )
    )
    expect_equal(
        format_objects(
            idd_parsed$group[
            idd_parsed$class[, .(group_id, class_id, class_name)], on = "group_id"][
            idd_parsed$field[, .(class_id, field_id, field_index, field_name, units, ip_units)], on = "class_id"],
            c("group", "class", "field"), brief = FALSE)$out,
        list(
            list( "Group: <TestGroup1>",
                c("└─ Class: <TestSimple>",
                  "   └─ Field: <1: Test Field>",
                  "   ")
            ),
            list( "Group: <TestGroup2>",
                c("└─ Class: <TestSlash>",
                  "   ├─ Field: <1: Test Character Field 1>",
                  "   │─ Field: <2: Test Numeric Field 1 {m}>",
                  "   │─ Field: <3: Test Numeric Field 2>",
                  "   └─ Field: <4: Test Character Field 2>",
                  "   ")
            )
        )
    )

    expect_equal(
        format_objects(
            idd_parsed$group[
            idd_parsed$class[, .(group_id, class_id, class_name)], on = "group_id"][
            idd_parsed$field[, .(class_id, field_id, field_index, field_name, units, ip_units)], on = "class_id"],
            c("field", "class"), brief = FALSE)$out,
        list(
            c("Class: <TestSimple>",
              "└─ Field: <1: Test Field>",
              ""
            ),
            c("Class: <TestSlash>",
              "├─ Field: <1: Test Character Field 1>",
              "│─ Field: <2: Test Numeric Field 1 {m}>",
              "│─ Field: <3: Test Numeric Field 2>",
              "└─ Field: <4: Test Character Field 2>",
              ""
            )
        )
    )

    expect_equal(
        format_objects(
            idd_parsed$group[
            idd_parsed$class[, .(group_id, class_id, class_name)], on = "group_id"][
            idd_parsed$field[, .(class_id, field_id, field_index, field_name, units, ip_units)], on = "class_id"],
            c("field", "group"), brief = FALSE)$out,
        list(
            c("Group: <TestGroup1>",
              "└─ Field: <1: Test Field>",
              ""
            ),
            c("Group: <TestGroup2>",
              "├─ Field: <1: Test Character Field 1>",
              "│─ Field: <2: Test Numeric Field 1 {m}>",
              "│─ Field: <3: Test Numeric Field 2>",
              "└─ Field: <4: Test Character Field 2>",
              ""
            )
        )
    )

    # Relation
    expect_equal(format_idd_relation(data.table()), data.table(class_id = integer(), fmt = list()))
    expect_equal(
        format_idd_relation(get_idd_relation(idd_parsed, direction = "ref_by", name = TRUE), "ref_by")$fmt,
        c("Class: <TestSimple>",
          "└─ Field: <1: Test Field>",
          "   ^~~~~~~~~~~~~~~~~~~~~~",
          "   └─ Class: <TestSlash>",
          "      └─ Field: <1: Test Character Field 1>",
          ""
        )
    )
    expect_equal(
        format_idd_relation(get_idd_relation(idd_parsed, direction = "ref_to", name = TRUE), "ref_to")$fmt,
        c("Class: <TestSlash>",
          "└─ Field: <1: Test Character Field 1>",
          "   v~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
          "   └─ Class: <TestSimple>",
          "      └─ Field: <1: Test Field>",
          ""
        )
    )
    skip_on_cran()

    expect_is(idd88 <- parse_idd_file(file.path(eplus_config(8.8)$dir, "Energy+.idd")), "list")
    expect_equal(format_idd_relation(get_idd_relation(idd88, NULL, 21590, name = TRUE, class_ref = "all")[1:2])$fmt,
        c("Class: <AirLoopHVAC:OutdoorAirSystem:EquipmentList>",
          "└─ Field: <2: Component 1 Object Type>",
          "   p~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
          "   ├─ Class: <Fan:ConstantVolume>",
          "   │  └─ Field: <1: Name>",
          "   │  ",
          "   └─ Class: <Fan:VariableVolume>",
          "      └─ Field: <1: Name>",
          "      "
        )
    )

    expect_is(pos <- format_possible(get_iddobj_possible(idd88, 3)), "IddFieldPossible")
    expect_equal(pos$fmt_auto,
        c("* Auto value: <NA>", "* Auto value: <NA>", "* Auto value: <NA>",
          "* Auto value: <NA>", "* Auto value: <NA>", "* Auto value: <NA>",
          "* Auto value: <NA>", "* Auto value: <NA>")
    )
    expect_equal(pos$fmt_default,
        c("* Default: \"NONE\"", "* Default: 0", "* Default: \"Suburbs\"",
          "* Default: 0.04", "* Default: 0.4", "* Default: \"FullExterior\"",
          "* Default: 25", "* Default: 6")
    )
    expect_equal(pos$fmt_choice,
        c("* Choice: <NA>",
          "* Choice: <NA>",
          "* Choice:\n  - \"Country\"\n  - \"Suburbs\"\n  - \"City\"\n  - \"Ocean\"\n  - \"Urban\"",
          "* Choice: <NA>",
          "* Choice: <NA>",
          "* Choice:\n  - \"MinimalShadowing\"\n  - \"FullExterior\"\n  - \"FullInteriorAndExterior\"\n  - \"FullExteriorWithReflections\"\n  - \"FullInteriorAndExteriorWithReflections\"",
          "* Choice: <NA>",
          "* Choice: <NA>")
    )
    # }}}
    # IDF {{{
    expect_warning(format_header(special_format = TRUE))
    expect_equal(format_header(view_in_ip = TRUE)[2], "!-Option SortedOrder ViewInIPunits")

    idd_parsed <- get_priv_env(use_idd(8.8, "auto"))$m_idd_env
    idf_parsed <- parse_idf_file(idftext("idf", "8.8"))
    add_joined_cols(idd_parsed$field, idf_parsed$value, "field_id", c("field_index", "type_enum", "units", "ip_units"))
    # object
    expect_equal(format_objects(idf_parsed$object, component = "object")$out,
        c("Object [ID:1] <WD01>",
          "Object [ID:2] <WALL-1>",
          "Object [ID:3] <WALL-1PF>",
          "Object [ID:4] <WD02>",
          "Object [ID:5]"
        )
    )
    # value
    expect_equal(
        format_objects(get_idf_value(idd_parsed, idf_parsed, property = c("type_enum", "units", "ip_units")),
            component = "value", merge = FALSE)$out[c(1,2,4,5)],
        c('Value: <"WD01">',
          'Value: <"MediumSmooth">',
          'Value: <0.115>',
          'Value: <513>')
    )
    expect_equal(
        format_objects(get_idf_value(idd_parsed, idf_parsed),
            component = "value", merge = TRUE)$out[c(1,2,4,5)],
        c("1: \"WD01\",        !- Name",
          "2: \"MediumSmooth\",!- Roughness",
          "4: 0.115,         !- Conductivity {W/m-K}",
          "5: 513,           !- Density {kg/m3}"
        )
    )
    expect_equal(
        format_objects(get_idf_value(idd_parsed, idf_parsed, property = c("units", "ip_units", "type_enum")),
            component = c("object", "value"))$out[c(1,2,4,5)],
        c("[09<V>] Object [ID:1] <WD01>",
          "[05<V>] Object [ID:2] <WALL-1>",
          "[06<V>] Object [ID:4] <WD02>",
          "[01<V>] Object [ID:5]"
        )
    )
    expect_equal(
        format_objects(get_idf_value(idd_parsed, idf_parsed),
            component = c("object", "value"),
            brief = FALSE)$out[[1L]],
        c("Object [ID:1] <WD01>",
          "├─ 1: \"WD01\",        !- Name",
          "│─ 2: \"MediumSmooth\",!- Roughness",
          "│─ 3: 0.019099999,   !- Thickness {m}",
          "│─ 4: 0.115,         !- Conductivity {W/m-K}",
          "│─ 5: 513,           !- Density {kg/m3}",
          "│─ 6: 1381,          !- Specific Heat {J/kg-K}",
          "│─ 7: 0.9,           !- Thermal Absorptance",
          "│─ 8: 0.78,          !- Solar Absorptance",
          "└─ 9: 0.78;          !- Visible Absorptance",
          ""
        )
    )
    expect_equal(
        format_objects(get_idf_value(idd_parsed, idf_parsed),
            component = c("object", "value"),
            brief = FALSE, merge = FALSE)$out[[1L]],
        c("Object [ID:1] <WD01>",
          "├─ Value: <\"WD01\">",
          "│─ Value: <\"MediumSmooth\">",
          "│─ Value: <0.019099999>",
          "│─ Value: <0.115>",
          "│─ Value: <513>",
          "│─ Value: <1381>",
          "│─ Value: <0.9>",
          "│─ Value: <0.78>",
          "└─ Value: <0.78>",
          ""
        )
    )
    expect_equal(
        unlist(format_objects(get_idf_value(idd_parsed, idf_parsed),
            component = c("object", "field", "value"),
            brief = FALSE, merge = FALSE)$out[[1L]]),
        c("Object [ID:1] <WD01>",
          "├─ Field: <1: Name>",
          "│  └─ Value: <\"WD01\">",
          "│  ",
          "│─ Field: <2: Roughness>",
          "│  └─ Value: <\"MediumSmooth\">",
          "│  ",
          "│─ Field: <3: Thickness {m}>",
          "│  └─ Value: <0.019099999>",
          "│  ",
          "│─ Field: <4: Conductivity {W/m-K}>",
          "│  └─ Value: <0.115>",
          "│  ",
          "│─ Field: <5: Density {kg/m3}>",
          "│  └─ Value: <513>",
          "│  ",
          "│─ Field: <6: Specific Heat {J/kg-K}>",
          "│  └─ Value: <1381>",
          "│  ",
          "│─ Field: <7: Thermal Absorptance>",
          "│  └─ Value: <0.9>",
          "│  ",
          "│─ Field: <8: Solar Absorptance>",
          "│  └─ Value: <0.78>",
          "│  ",
          "└─ Field: <9: Visible Absorptance>",
          "   └─ Value: <0.78>",
          "   "
        )
    )
    expect_equal(
        unlist(with_nocolor(format_objects(get_idf_value(idd_parsed, idf_parsed),
                    component = c("class", "object", "value"),
                    brief = FALSE)$out[[1L]])),
        c("Class: <Material>",
          "├─ Object [ID:1] <WD01>",
          "│  ├─ 1: \"WD01\",        !- Name",
          "│  │─ 2: \"MediumSmooth\",!- Roughness",
          "│  │─ 3: 0.019099999,   !- Thickness {m}",
          "│  │─ 4: 0.115,         !- Conductivity {W/m-K}",
          "│  │─ 5: 513,           !- Density {kg/m3}",
          "│  │─ 6: 1381,          !- Specific Heat {J/kg-K}",
          "│  │─ 7: 0.9,           !- Thermal Absorptance",
          "│  │─ 8: 0.78,          !- Solar Absorptance",
          "│  └─ 9: 0.78;          !- Visible Absorptance",
          "│  ",
          "└─ Object [ID:4] <WD02>",
          "   ├─ 1: \"WD02\",        !- Name",
          "   │─ 2: \"MediumSmooth\",!- Roughness",
          "   │─ 3: 0.019099999,   !- Thickness {m}",
          "   │─ 4: 0.115,         !- Conductivity {W/m-K}",
          "   │─ 5: <Blank>,       !- Density {kg/m3}",
          "   └─ 6: <Blank>;       !- Specific Heat {J/kg-K}",
          "   "
          )
    )
    expect_equal(
        unlist(with_nocolor(format_objects(get_idf_value(idd_parsed, idf_parsed),
            component = c("class", "object", "field", "value"),
            brief = FALSE, merge = FALSE)$out[[1L]])),
        c("Class: <Material>",
          "├─ Object [ID:1] <WD01>",
          "│  ├─ Field: <1: Name>",
          "│  │  └─ Value: <\"WD01\">",
          "│  │  ",
          "│  │─ Field: <2: Roughness>",
          "│  │  └─ Value: <\"MediumSmooth\">",
          "│  │  ",
          "│  │─ Field: <3: Thickness {m}>",
          "│  │  └─ Value: <0.019099999>",
          "│  │  ",
          "│  │─ Field: <4: Conductivity {W/m-K}>",
          "│  │  └─ Value: <0.115>",
          "│  │  ",
          "│  │─ Field: <5: Density {kg/m3}>",
          "│  │  └─ Value: <513>",
          "│  │  ",
          "│  │─ Field: <6: Specific Heat {J/kg-K}>",
          "│  │  └─ Value: <1381>",
          "│  │  ",
          "│  │─ Field: <7: Thermal Absorptance>",
          "│  │  └─ Value: <0.9>",
          "│  │  ",
          "│  │─ Field: <8: Solar Absorptance>",
          "│  │  └─ Value: <0.78>",
          "│  │  ",
          "│  └─ Field: <9: Visible Absorptance>",
          "│     └─ Value: <0.78>",
          "│     ",
          "└─ Object [ID:4] <WD02>",
          "   ├─ Field: <1: Name>",
          "   │  └─ Value: <\"WD02\">",
          "   │  ",
          "   │─ Field: <2: Roughness>",
          "   │  └─ Value: <\"MediumSmooth\">",
          "   │  ",
          "   │─ Field: <3: Thickness {m}>",
          "   │  └─ Value: <0.019099999>",
          "   │  ",
          "   │─ Field: <4: Conductivity {W/m-K}>",
          "   │  └─ Value: <0.115>",
          "   │  ",
          "   │─ Field: <5: Density {kg/m3}>",
          "   │  └─ Value: <<Blank>>",
          "   │  ",
          "   └─ Field: <6: Specific Heat {J/kg-K}>",
          "      └─ Value: <<Blank>>",
          "      "
          )
    )

    # Format IDF
    # add field index, class id and class name
    add_joined_cols(idf_parsed$object, idf_parsed$value, "object_id", "class_id")
    add_joined_cols(idd_parsed$class, idf_parsed$value, "class_id", "class_name")
    add_joined_cols(idd_parsed$field, idf_parsed$value, "field_id", c("field_index", "units", "ip_units", "field_name"))
    expect_silent({fmt <- format_idf(idf_parsed$value, idf_parsed$object)})
    expect_equal(names(fmt), c("header", "format"))
    expect_equal(fmt$header,
        c("!-Generator eplusr",
          "!-Option SortedOrder",
          "",
          "!-NOTE: All comments with '!-' are ignored by the IDFEditor and are generated automatically.",
          "!-      Use '!' comments if they need to be retained when using the IDFEditor."
        )
    )
    expect_equal(fmt$format$class_id, c(1L, 55L, 90L, 103L))
    expect_equal(fmt$format$fmt[[2L]],
        list("!-   ===========  ALL OBJECTS IN CLASS: MATERIAL ===========",
             list(c("! this is a test comment for WD01"),
                  c("Material,",
                    "    WD01,                    !- Name",
                    "    MediumSmooth,            !- Roughness",
                    "    0.019099999,             !- Thickness {m}",
                    "    0.115,                   !- Conductivity {W/m-K}",
                    "    513,                     !- Density {kg/m3}",
                    "    1381,                    !- Specific Heat {J/kg-K}",
                    "    0.9,                     !- Thermal Absorptance",
                    "    0.78,                    !- Solar Absorptance",
                    "    0.78;                    !- Visible Absorptance")
            ),
            list(NULL,
                 c("Material,",
                   "    WD02,                    !- Name",
                   "    MediumSmooth,            !- Roughness",
                   "    0.019099999,             !- Thickness {m}",
                   "    0.115,                   !- Conductivity {W/m-K}",
                   "    ,                        !- Density {kg/m3}",
                   "    ;                        !- Specific Heat {J/kg-K}"
                 )
            )
        )
    )

    expect_null(format_idf(idf_parsed$value, idf_parsed$object, header = FALSE)$header)
    expect_null(format_idf(idf_parsed$value, idf_parsed$object, comment = FALSE)$format$fmt[[2L]][[2L]][[1L]])
    expect_silent({fmt <- format_idf(idf_parsed$value, idf_parsed$object,
        dt_order = data.table(object_id = 1:5, object_order = 0L),
        save_format = "new_top")})
    expect_equal(fmt$format$object_id, 1:5)
    expect_equal(fmt$format$fmt[[1L]],
         list(c("! this is a test comment for WD01"),
              c("Material,",
                "    WD01,                    !- Name",
                "    MediumSmooth,            !- Roughness",
                "    0.019099999,             !- Thickness {m}",
                "    0.115,                   !- Conductivity {W/m-K}",
                "    513,                     !- Density {kg/m3}",
                "    1381,                    !- Specific Heat {J/kg-K}",
                "    0.9,                     !- Thermal Absorptance",
                "    0.78,                    !- Solar Absorptance",
                "    0.78;                    !- Visible Absorptance")
        )
    )

    expect_is(pos <- format_possible(get_idfobj_possible(idd_parsed, idf_parsed, 2, 2)), "IdfValuePossible")
    expect_equal(pos$fmt_auto, "* Auto value: <NA>")
    expect_equal(pos$fmt_default, "* Default: <NA>")
    expect_equal(pos$fmt_choice, "* Choice: <NA>")
    expect_equal(pos$fmt_source, "* Source: \n  - \"WD01\"\n  - \"WD02\"")

    expect_equal(format_comment(data.table(comment = list("a", NULL))), c("!a", NA))
    # }}}
})
