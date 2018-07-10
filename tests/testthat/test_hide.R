context("Hide method")

test_that("Hide method works", {
    model <- eplus_model$new("files/5Zone_Transformer_8.8.idf")
    id_max <- max_id(.get(model, "model"))
    # missing id
    expect_error(model$hide())
    # invalid id
    expect_error(model$hide(1e6))
    # cannot delete existing unique or required objects
    expect_error(model$hide(1))
    expect_error(model$hide(2))
    # cannot delete objects that have been referred by other objects
    expect_error(model$hide(55))
    expect_equal(.get(model, "model")$class[object_id == 55L, unique(edited)], 0L)

    # CORRECT
    expect_silent(model$hide(54))
    expect_equal(.get(model, "model")$class[object_id == 54L, unique(edited)], -1L)
})

