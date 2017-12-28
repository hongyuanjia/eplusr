context("Set method")

test_that("Set method works", {
    model <- eplus_model$new("files/5Zone_Transformer_8.8.idf")

    # missing id
    expect_error(model$set())
    # invalid id
    expect_error(model$set(1e6))
    # cannot set protected class
    expect_error(model$set(1, "any"))
    expect_error(model$set(1, 8.8))
    # empty field input
    expect_error(model$set(52))
    # mixed field name style
    expect_error(model$set(52L,
        name = "test_add", Roughness = "Rough")
    )
    # invalid input type
    expect_error(model$set(52L, name = 1L, roughness = "Rough",
        thickness = 0.8, conductivity = 55, density = 55, specific_heat = 100)
    )
    # invalid choice
    expect_error(model$set(52L, name = "test_add", roughness = "InvalidChoice",
        thickness = 0.8, conductivity = 55, density = 55, specific_heat = 100)
    )
    # exceed minimum range
    expect_error(model$set(52L, name = "test_add", roughness = "Rough",
        thickness = -1L, conductivity = 55, density = 55, specific_heat = 100)
    )
    expect_error(model$set(52L, name = "test_add", roughness = "Rough",
        thickness = 0.8, conductivity = 55, density = 55, specific_heat = 100,
        thermal_absorptance = 1.8)
    )
    # invalid field name
    expect_error(model$set(52L, name = "test_add", roughness = "Rough",
        thickness = 0.8, conductivity = 55, density = 55, specific_heat = 100,
        non_exist_name = 0)
    )

    # CORRECT
    expect_silent(model$set(52L, roughness = "Rough",
        thickness = 0.8, conductivity = 55, density = 55, specific_heat = 100)
    )
})
