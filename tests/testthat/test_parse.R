context("Parse method")

test_that("Parse IDF works", {

    expect_error(model <- eplus_model$new("5Zone_Transformer_8.8_bad.idf"))
    expect_silent(model <- eplus_model$new("5Zone_Transformer_8.8.idf"))
    expect_silent(model <- eplus_model$new("5Zone_Transformer_8.7.idf"))
    expect_silent(model <- eplus_model$new("AbsorptionChiller_Macro_8.6.imf"))

    model <- eplus_model$new("AbsorptionChiller_Macro_8.6.imf")
    expect_equal(class(.get(model, "model")), c("IMF", "list"))
    expect_equal(.get(model, "ver"), "8.6")
    expect_equal(.get(model, "type"), "IMF")
    expect_equal(.get(model, "idd")$version, "8.6.0")
})
