context("Basic Implementation")

# Basic Impl {{{
test_that("Basic Table Implementation", {
    expect_equal(assert_valid_type("a"), "a")
    expect_error(assert_valid_type("a", type = "id"), "integerish")
    expect_error(assert_valid_type("a", "Object ID", type = "id"), "Object")
    expect_error(assert_valid_type("a", len = 2, type = "name"), "length 2")
    expect_equal(assert_valid_type(1, type = "id"), 1L)
    expect_equal(assert_valid_type(1), 1L)
    expect_error(assert_valid_type(1, lower = 2, type = "both"), 1L)

    expect_error(
        recognize_input("ClassName", type = "class", underscore = TRUE, lower = TRUE),
        "underscore and lower cannot all be TRUE"
    )

    expect_equivalent(
        recognize_input("Class:Name", type = "class", underscore = TRUE),
        data.table(class_name_us = "Class_Name", rleid = 1L, original = "Class:Name")
    )

    expect_equivalent(
        recognize_input("Field Name", type = "field", underscore = TRUE),
        data.table(field_name_us = "field_name", rleid = 1L, original = "Field Name")
    )

    expect_equivalent(
        recognize_input("Field Name", type = "field", lower = TRUE),
        data.table(field_name_lower = "field name", rleid = 1L, original = "Field Name")
    )

    expect_equal(
        join_from_input(
            dt = data.table(object_id = 1:3, object_name = c("A", "B", "C")),
            input = recognize_input(2L, "object")
        ),
        data.table(rleid = 1L, object_id = 2L, object_name = "B")
    )

    expect_equal(
        join_from_input(
            dt = data.table(object_id = 1:3, object_name = c("A", "B", "C"),
                object_name_lower = c("a", "b", "c")),
            input = recognize_input("B", "object")
        ),
        data.table(rleid = 1L, object_id = 2L, object_name = "B", object_name_lower = "b")
    )

    expect_equal(
        join_from_input(
            dt = data.table(object_id = 1:3, object_name = c("A", "B", "C"),
                object_name_lower = c("a", "b", "c")),
            input = recognize_input("b", "object", lower = TRUE)
        ),
        data.table(rleid = 1L, object_id = 2L, object_name = "B", object_name_lower = "b")
    )

    expect_error(
        join_from_input(
            dt = data.table(object_id = 1:3, object_name = c("A", "B", "C"),
                object_name_lower = c("a", "b", "c")),
            input = recognize_input("D", "object", lower = TRUE),
            check = "object_id"
        ),
        class = "eplusr_error_invalid_object_name"
    )

    expect_equal(
        {
            base <- data.table(object_id = 1:3, object_name = c("A", "B", "C"),
                object_name_lower = c("a", "b", "c"))
            dt <- data.table(id = 2L)
            add_joined_cols(base, dt, on = c(id = "object_id"), cols = c("name" = "object_name"))
        },
        data.table(data.table(id = 2L, name = "B"))
    )

    # log {{{
    log <- new.env(parent = emptyenv())
    log$unsaved <- NA

    expect_silent(log_new_uuid(log))
    expect_equal(nchar(log$uuid), 2 + 1 + 10 + 1 + 10)

    log$order <- data.table(object_id = 1:5, object_order = 0L)
    expect_silent(log_new_order(log, 6L))
    expect_equal(log$order, data.table(object_id = 1:6, object_order = c(rep(0L, 5), 1L)))

    expect_silent(log_add_order(log, 6L))
    expect_equal(log$order[.N], data.table(object_id = 6L, object_order = 2L))

    expect_silent(log_del_order(log, 6L))
    expect_equal(log$order, data.table(object_id = 1:5, object_order = 0L))

    expect_silent(log_unsaved(log))
    expect_equal(log$unsaved, TRUE)

    expect_silent(log_saved(log))
    expect_equal(log$unsaved, FALSE)
    # }}}

    eplusr_option(validate_level = "final")
    eplusr_option(verbose_info = TRUE)
    expect_true(in_final_mode())
    expect_false(in_ip_mode())
    expect_true(in_verbose())
    expect_message(verbose_info("a"), "a")
    eplusr_option(verbose_info = FALSE)

    expect_error(abort_bad_key("object ID", 1L), class = "eplusr_error_invalid_object_id")
    expect_error(
        abort_bad_field("index", data.table(rleid = 1L, class_name = "Class", field_index = 1L, min_fields = 2L, num_fields = 3L)),
        class = "eplusr_error_invalid_field_index"
    )
    expect_error(
        abort_bad_field("name", data.table(rleid = 1L, class_name = "Class", field_name = "Name")),
        class = "eplusr_error_invalid_field_name"
    )

    expect_equal(new_id(data.table(object_id = 1:5), "object_id", 2L), 6:7)
    expect_equal(add_rleid(data.table(object_id = 1:5)), data.table(object_id = 1:5, rleid = 1:5))
    expect_equal(add_rleid(data.table(object_id = 1:5), "object"), data.table(object_id = 1:5, object_rleid = 1:5))

    expect_error(append_dt(data.table(a = 1), data.table()))
    expect_equal(append_dt(data.table(), data.table()), data.table())
    expect_equal(append_dt(data.table(a = 1), data.table(a = 2, b = 1)), data.table(a = c(1, 2)))
    expect_equal(append_dt(data.table(a = 1, b = 1), data.table(a = c(1, 2), b = c(3, 4)), "a"), data.table(a = c(1, 2), b = c(3, 4)))
})
# }}}
