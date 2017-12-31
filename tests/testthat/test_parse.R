context("Parse method")

test_that("Parse IDF works", {

    expect_error(model <- eplus_model$new(system.file("testdata", "5Zone_Transformer_8.8_bad.idf", package = "eplusr")))
    expect_silent(model <- eplus_model$new(system.file("testdata", "5Zone_Transformer_8.8.idf", package = "eplusr")))
    expect_silent(model <- eplus_model$new(system.file("testdata", "5Zone_Transformer_8.7.idf", package = "eplusr")))
    expect_silent(model <- eplus_model$new(system.file("testdata", "AbsorptionChiller_Macro_8.6.imf", package = "eplusr")))

    model <- eplus_model$new(system.file("testdata", "AbsorptionChiller_Macro_8.6.imf", package = "eplusr"))
    expect_equal(class(.get(model, "model")), c("IMF", "list"))
    expect_equal(.get(model, "ver"), "8.6")
    expect_equal(.get(model, "type"), "IMF")
    expect_equal(.get(model, "idd")$version, "8.6.0")
})
