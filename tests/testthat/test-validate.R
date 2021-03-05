# VALIDTATE {{{
test_that("Validate method", {
    idf <- read_idf(example(), use_idd(8.8, "auto"))
    idf_env <- get_priv_env(idf)$m_idf_env
    idd_env <- get_priv_env(idf)$idd_env()

    expect_is(empty_validity(), "IdfValidity")
    expect_equal(names(empty_validity()),
        c("missing_object",
          "duplicate_object",
          "conflict_name",
          "incomplete_extensible",
          "missing_value",
          "invalid_autosize",
          "invalid_autocalculate",
          "invalid_character",
          "invalid_numeric",
          "invalid_integer",
          "invalid_choice",
          "invalid_range",
          "invalid_reference"
        )
    )

    expect_error(level_checks(1))

    expect_is(format_validity(empty_validity()), "character")
    expect_output(print.IdfValidity(empty_validity()))
    expect_output(print.EpwValidity(empty_validity()))

    # MISSING OBJECT {{{
    env_in <- parse_idf_file(idftext("idf", 8.8))
    expect_equal(
        check_missing_object(idd_env, idf_env, env_in)$validity$missing_object,
        c("Building", "GlobalGeometryRules")
    )
    ids <- get_idd_class(idd_env, c("Building", "GlobalGeometryRules"))$class_id
    expect_equal(
        check_missing_object(idd_env, idf_env, list(object = list(class_id = ids)))$validity$missing_object,
        character(0)
    )
    # }}}

    # DUPLICATE OBJECT {{{
    env_in <- list2env(parse_idf_file(idftext("idf", 8.8)))
    env_in$validity <- empty_validity()
    expect_equal(nrow(check_duplicate_object(idd_env, idf_env, env_in)$validity$duplicate_object), 0L)
    env_in$object <- rbindlist(list(
        env_in$object,
        data.table(
            object_id = 6:7, object_name = c("Bld", "Bld"),
            object_name_lower = c("bld", "bld"),
            class_id = get_idd_class(idd_env, "Building")$class_id,
            comment = list(NULL, NULL)
        )
    ), use.names = TRUE)
    env_in$value <- rbindlist(list(
        env_in$value,
        data.table(
            object_id = 6:7,
            object_name = "Bld",
            class_id = get_idd_class(idd_env, "Building")$class_id,
            class_name = "Building",
            field_id = get_idd_field(idd_env, "Building", "Name")$field_id,
            field_index = 1L,
            field_name = "Name",
            units = NA_character_,
            ip_units = NA_character_,
            type_enum = IDDFIELD_TYPE$alpha,
            value_id = 45:46,
            value_chr = "Bld",
            value_num = NA_real_
        )
    ), fill = TRUE)
    expect_equal(check_duplicate_object(idd_env, idf_env, env_in)$validity$duplicate_object$object_id,
        c(6L, 7L)
    )
    # }}}

    # CONFLICT NAME {{{
    env_in <- list2env(parse_idf_file(idftext("idf", 8.8)))
    env_in$validity <- empty_validity()
    env_in$check_whole <- TRUE
    expect_equal(nrow(check_conflict_name(idd_env, idf_env, env_in)$validity$conflict_name), 0L)
    env_in$object <- rbindlist(list(
        env_in$object,
        data.table(
            object_id = 6:7, object_name = "Bld",
            object_name_lower = "bld",
            class_id = get_idd_class(idd_env, "Building")$class_id,
            comment = list(NULL, NULL)
        )
    ), use.names = TRUE)
    env_in$value <- rbindlist(list(
        env_in$value,
        data.table(
            object_id = 6:7,
            object_name = c("Bld", "Bld"),
            class_id = rep(get_idd_class(idd_env, "Building")$class_id, 2),
            class_name = rep("Building", 2),
            field_id = rep(get_idd_field(idd_env, "Building", "Name")$field_id, 2),
            field_index = rep(1L, 2),
            field_name = rep("Name", 2),
            units = rep(NA_character_, 2),
            ip_units = rep(NA_character_, 2),
            type_enum = rep(IDDFIELD_TYPE$alpha, 2),
            value_id = 45:46,
            value_chr = c("Bld", "Bld"),
            value_num = rep(NA_real_, 2)
        )
    ), fill = TRUE)
    expect_equal(check_conflict_name(idd_env, idf_env, env_in)$validity$conflict_name$object_id,
        c(6L, 7L)
    )
    # }}}

    # INCOMPLETE EXTENSIBLE {{{
    env_in <- list2env(parse_idf_file(idftext("idf", 8.8)))
    env_in$validity <- empty_validity()
    add_joined_cols(env_in$object, env_in$value, "object_id", c("class_id", "object_name"))
    add_class_property(idd_env, env_in$value, c("class_id", "class_name"))
    add_field_property(idd_env, env_in$value, c("extensible_group", "field_index", "field_name", "units", "ip_units", "type_enum"))
    expect_equal(nrow(check_incomplete_extensible(idd_env, idf_env, env_in)$validity$incomplete_extensible), 0L)
    invisible(env_in$value[extensible_group == 3L, value_chr := NA_character_])
    expect_silent({err <- check_incomplete_extensible(idd_env, idf_env, env_in)$validity$incomplete_extensible})
    expect_equal(err$object_id, rep(3L, 3))
    expect_equal(err$field_index, 17:19)
    expect_equal(err$value_id, 31:33)
    # }}}

    # MISSING VALUE {{{
    env_in <- list2env(parse_idf_file(idftext("idf", 8.8)))
    env_in$validity <- empty_validity()
    add_joined_cols(env_in$object, env_in$value, "object_id", c("class_id", "object_name"))
    add_class_property(idd_env, env_in$value, c("class_id", "class_name"))
    add_field_property(idd_env, env_in$value, c("required_field", "field_index", "field_name", "units", "ip_units", "type_enum"))
    invisible(env_in$value[J(c(1L, 10L, 15L)), on = "value_id", value_chr := NA_character_])

    expect_silent({mis <- check_missing_value(idd_env, idf_env, env_in)$validity$missing_value})
    expect_equal(mis$object_id, c(1:3, 4, 4))
    expect_equal(mis$value_id, c(1L, 10L, 15L, 45L, 46L))
    # }}}

    # INVALID AUTOSIZE {{{
    env_in <- list2env(parse_idf_file(idftext("idf", 8.8)))
    env_in$validity <- empty_validity()
    add_joined_cols(env_in$object, env_in$value, "object_id", c("class_id", "object_name"))
    add_class_property(idd_env, env_in$value, c("class_id", "class_name"))
    add_field_property(idd_env, env_in$value, c("autosizable", "field_index", "field_name", "units", "ip_units", "type_enum"))
    invisible(env_in$value[field_name == "Name", value_chr := "autosize"])
    set(env_in$value, NULL, "value_lower", stri_trans_tolower(env_in$value$value_chr))

    expect_silent({autosize <- check_invalid_autosize(idd_env, idf_env, env_in)$validity$invalid_autosize})
    expect_equal(autosize$object_id, 1:4)
    expect_equal(autosize$field_index, rep(1L, 4L))
    expect_equal(autosize$value_id, c(1L, 10L, 15L, 40L))
    # }}}

    # INVALID AUTOCALCULATE {{{
    env_in <- list2env(parse_idf_file(idftext("idf", 8.8)))
    env_in$validity <- empty_validity()
    add_joined_cols(env_in$object, env_in$value, "object_id", c("class_id", "object_name"))
    add_class_property(idd_env, env_in$value, c("class_id", "class_name"))
    add_field_property(idd_env, env_in$value, c("autocalculatable", "field_index", "field_name", "units", "ip_units", "type_enum"))
    invisible(env_in$value[field_name == "Name", value_chr := "autocalculate"])
    set(env_in$value, NULL, "value_lower", stri_trans_tolower(env_in$value$value_chr))

    expect_silent({autocal <- check_invalid_autocalculate(idd_env, idf_env, env_in)$validity$invalid_autocalculate})
    expect_equal(autocal$object_id, 1:4)
    expect_equal(autocal$field_index, rep(1L, 4L))
    expect_equal(autocal$value_id, c(1L, 10L, 15L, 40L))
    # }}}

    # INVALID CHARACTER {{{
    env_in <- list2env(parse_idf_file(idftext("idf", 8.8)))
    env_in$validity <- empty_validity()
    add_joined_cols(env_in$object, env_in$value, "object_id", c("class_id", "object_name"))
    add_class_property(idd_env, env_in$value, c("class_id", "class_name"))
    add_field_property(idd_env, env_in$value, c("field_index", "field_name", "units", "ip_units", "type_enum"))
    invisible(env_in$value[field_name == "Name", `:=`(value_chr = "1", value_num = 1L)])

    expect_silent({chr <- check_invalid_character(idd_env, idf_env, env_in)$validity$invalid_character})
    expect_equal(chr$object_id, 1:4)
    expect_equal(chr$field_index, rep(1L, 4L))
    expect_equal(chr$value_id, c(1L, 10L, 15L, 40L))
    # }}}

    # INVALID NUMERIC {{{
    env_in <- list2env(parse_idf_file(idftext("idf", 8.8)))
    env_in$validity <- empty_validity()
    add_joined_cols(env_in$object, env_in$value, "object_id", c("class_id", "object_name"))
    add_class_property(idd_env, env_in$value, c("class_id", "class_name"))
    add_field_property(idd_env, env_in$value, c("field_index", "field_name", "units", "ip_units", "type_enum"))
    invisible(env_in$value[object_id == 1L & type_enum <= IDDFIELD_TYPE$real, `:=`(value_num = NA_real_)])

    expect_silent({num <- check_invalid_numeric(idd_env, idf_env, env_in)$validity$invalid_numeric})
    expect_equal(num$object_id, c(rep(1L, 7), rep(3L, 3), rep(4L, 2)))
    expect_equal(num$value_id, c(3:9, 37:39, 45:46))
    # }}}

    # INVALID INTEGER {{{
    env_in <- list2env(parse_idf_file(idftext("idf", 8.8)))
    env_in$validity <- empty_validity()
    add_joined_cols(env_in$object, env_in$value, "object_id", c("class_id", "object_name"))
    add_class_property(idd_env, env_in$value, c("class_id", "class_name"))
    add_field_property(idd_env, env_in$value, c("field_index", "field_name", "units", "ip_units", "type_enum"))

    invisible(env_in$value[object_id == 1L & type_enum == IDDFIELD_TYPE$real, `:=`(type_enum = IDDFIELD_TYPE$integer)])
    expect_silent({int <- check_invalid_integer(idd_env, idf_env, env_in)$validity$invalid_integer})
    expect_equal(int$object_id, rep(1L, 5))
    expect_equal(int$value_id, c(3L, 4L, 7:9))
    # }}}

    # INVALID CHOICE {{{
    env_in <- list2env(parse_idf_file(idftext("idf", 8.8)))
    env_in$validity <- empty_validity()
    add_joined_cols(env_in$object, env_in$value, "object_id", c("class_id", "object_name"))
    add_class_property(idd_env, env_in$value, c("class_id", "class_name"))
    add_field_property(idd_env, env_in$value, c("choice", "field_index", "field_name", "units", "ip_units", "type_enum"))
    invisible(env_in$value[object_id == 1L & type_enum == IDDFIELD_TYPE$choice, value_chr := "wrong"])
    set(env_in$value, NULL, "value_lower", stri_trans_tolower(env_in$value$value_chr))

    expect_silent({cho <- check_invalid_choice(idd_env, idf_env, env_in)$validity$invalid_choice})
    expect_equal(cho$object_id, 1L)
    expect_equal(cho$value_id, 2L)
    # }}}

    # INVALID RANGE {{{
    env_in <- list2env(parse_idf_file(idftext("idf", 8.8)))
    env_in$validity <- empty_validity()
    add_joined_cols(env_in$object, env_in$value, "object_id", c("class_id", "object_name"))
    add_class_property(idd_env, env_in$value, c("class_id", "class_name"))
    add_field_property(idd_env, env_in$value,
        c("has_range", "maximum", "minimum", "lower_incbounds", "upper_incbounds",
          "field_index", "field_name", "units", "ip_units", "type_enum")
    )
    invisible(env_in$value[value_id == 3L, value_num := -1])

    expect_silent({ran <- check_invalid_range(idd_env, idf_env, env_in)$validity$invalid_range})
    expect_equal(ran$object_id, 1L)
    expect_equal(ran$value_id, 3L)
    # }}}

    # INVALID REFERENCE {{{
    env_in <- list2env(parse_idf_file(idftext("idf", 8.8)))
    env_in$validity <- empty_validity()
    env_in$check_whole <- TRUE
    add_joined_cols(env_in$object, env_in$value, "object_id", c("class_id", "object_name"))
    add_class_property(idd_env, env_in$value, c("class_id", "class_name"))
    add_field_property(idd_env, env_in$value,
        c("src_enum", "field_index", "field_name", "units", "ip_units", "type_enum")
    )

    expect_silent({ref <- check_invalid_reference(idd_env, env_in, env_in)$validity$invalid_reference})
    expect_equal(ref$object_id, c(rep(2L, 3), rep(3L, 2)))
    expect_equal(ref$value_id, c(12:14, 18L, 20L))
    # }}}
})
# }}}
