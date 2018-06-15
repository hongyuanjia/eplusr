context("Add method")

test_that("add method works", {
    model <- eplus_model$new("files/5Zone_Transformer_8.8.idf")
    id_max <- max_id(.get(model, "model"))

    # invalid class name
    expect_error(model$add("version"))
    # cannot add existing unique object
    expect_error(model$add("Version"))
    # empty field input
    expect_error(model$add("Material"))
    # mixed field name style
    expect_error(model$add("Material",
        name = "test_add", Roughness = "Rough")
    )
    # missing required field
    expect_error(model$add("Material", roughness = "Rough",
        thickness = 0.8, conductivity = 55, density = 55, specific_heat = 100)
    )
    # invalid input type
    expect_error(model$add("Material", name = 1L, roughness = "Rough",
        thickness = 0.8, conductivity = 55, density = 55, specific_heat = 100)
    )
    # invalid choice
    expect_error(model$add("Material", name = "test_add", roughness = "InvalidChoice",
        thickness = 0.8, conductivity = 55, density = 55, specific_heat = 100)
    )
    # exceed minimum range
    expect_error(model$add("Material", name = "test_add", roughness = "Rough",
        thickness = -1L, conductivity = 55, density = 55, specific_heat = 100)
    )
    # invalid field name
    expect_error(model$add("Material", name = "test_add", roughness = "Rough",
        thickness = 0.8, conductivity = 55, density = 55, specific_heat = 100,
        non_exist_name = 0)
    )
    # exceed field number when `min` = TRUE
    expect_error(model$add("Material", name = "test_add", roughness = "Rough",
        thickness = 0.8, conductivity = 55, density = 55, specific_heat = 100,
        thermal_absorptance = 0.8)
    )
    # exceed minimum range when `min` = FALSE
    expect_error(model$add("Material", name = "test_add", roughness = "Rough",
        thickness = 0.8, conductivity = 55, density = 55, specific_heat = 100,
        thermal_absorptance = 1.8, min = FALSE)
    )

    # CORRECT
    expect_silent(model$add("Material", name = "test_add", roughness = "Rough",
        thickness = 0.8, conductivity = 55, density = 55, specific_heat = 100)
    )
    expect_equal(max_id(environment(model$add)$private$model), id_max + 1L)
    # CORRECT
    expect_message(model$add("Material", name = "test_add", roughness = "Rough",
        thickness = 0.8, conductivity = 55, density = 55, specific_heat = 100,
        thermal_absorptance = 0.8, min = FALSE)
    )
    expect_equal(max_id(environment(model$add)$private$model), id_max + 2L)
})
