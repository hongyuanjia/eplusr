# Integer diagnostics must refer to the original rows, not recycled subset indices.
test_that("integer truncation warnings identify only affected fields", {
    values <- data.table(
        field_name = c(
            "Label",
            "IntegerA",
            "RealB",
            "IntegerC",
            "Missing",
            "Automatic"
        ),
        value_chr = c("text", "2.5", "0.1", "4.5", "", "Autosize"),
        value_num = c(NA_real_, 2.5, 0.1, 4.5, NA_real_, NA_real_),
        type_enum = c(4L, 1L, 2L, 1L, 1L, 1L)
    )
    condition <- catch_cnd(get_value_list(values))
    expect_s3_class(condition, "eplusr_warning_warning_value_int_trunc")
    expect_match(
        conditionMessage(condition),
        "'IntegerA' and 'IntegerC'",
        fixed = TRUE
    )
    expect_false(grepl(
        "Label|RealB|Missing|Automatic",
        conditionMessage(condition)
    ))
    expect_warning(
        result <- get_value_list(values),
        "Truncated error",
        class = "eplusr_warning_warning_value_int_trunc"
    )
    expect_identical(result$IntegerA, 2L)
    expect_identical(result$IntegerC, 4L)
    expect_identical(result$RealB, 0.1)
    expect_identical(result$Missing, NA_integer_)
    expect_identical(result$Automatic, "Autosize")
    values[c(2L, 4L), `:=`(value_chr = c("2", "4"), value_num = c(2, 4))]
    expect_silent(get_value_list(values))
})
