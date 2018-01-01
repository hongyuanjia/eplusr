context("Find method")

test_that("find method works", {
    model <- eplus_model$new("files/5Zone_Transformer_8.8.idf")

    # wrong input
    expect_error(model$all("version"))
    # missing 'class' arg
    expect_error(model$all("field"))

    expect_output(model$all("id"))
    expect_equal(model$all("class"), .get(model, "model")$class[, unique(class)])
    expect_output(model$all("field", "Material"))

    # wrong 'scale' arg
    expect_error(model$contains("Material", "id"))
    # no matched
    expect_error(model$contains("ABCDEFG"), scale = "field")

    expect_output(model$contains("Node", "field"))

    # no matched
    expect_error(model$matches("ABCDEFG", scale = "field"))
    # wrong '...' passed to grepl but still get output
    expect_output(model$matches("Material", "id"))
    expect_output(model$matches("Node", fixed = TRUE, scale = "field"))

    # missing id, model summary returned
    expect_silent(model$get())
    # wrong id
    expect_error(model$get(1e5))
    expect_error(model$get("1"))
    expect_error(model$get(list(1:3)))

    expect_silent(model$get(1, 2, 1))
    expect_silent(model$get(1:2, 1))
})
