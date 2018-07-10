context("Dup method")

test_that("Dup method works", {
    model <- eplus_model$new("files/5Zone_Transformer_8.8.idf")
    id_max <- max_id(.get(model, "model"))
    model$get(52L)
    # missing id
    expect_error(model$dup())
    # invalid id
    expect_error(model$dup(1e6))
    # cannot dup existing unique object
    expect_error(model$dup(1))

    expect_silent(model$dup(52))
    expect_equal(max_id(.get(model, "model")), id_max + 1L)
    expect_equal(.get(model, "model")$log[.N, action], "dup")
    expect_equal(.get(model, "model")$class[, max(object_id)], id_max + 1L)
    expect_equal(.get(model, "model")$value[, max(object_id)], id_max + 1L)
    expect_equal(.get(model, "model")$value[object_id == id_max + 1L, value][1], "CC03_1")

    expect_silent(model$dup(52, "test_dup"))
    expect_equal(.get(model, "model")$value[object_id == id_max + 2L, value][1], "test_dup")
})
