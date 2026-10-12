# Mixed date formats must retain their original positions after numeric filtering.
test_that("mixed EPW dates retain decimal-tail and missing-value positions", {
    inputs <- list(
        c("4", "1/3", "1.1"),
        c("4", "1/3", "3.10"),
        c("1/3", "4", "3.10"),
        c("3.10", "4", "1/3"),
        c(NA_character_, "3.10", "4", "1/3"),
        character()
    )
    for (leapyear in c(TRUE, FALSE)) {
        for (input in inputs) {
            expected <- if (length(input)) {
                do.call(c, lapply(input, epw_date, leapyear = leapyear))
            } else {
                epw_date(input, leapyear = leapyear)
            }
            expect_silent(actual <- epw_date(input, leapyear = leapyear))
            expect_identical(actual, expected)
        }
    }
})
