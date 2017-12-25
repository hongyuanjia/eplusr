context("Add method")

test_that("add method works", {
    model <- eplus_model$new("files/5Zone_Transformer.idf")

    expect_error(model$add("version"))
    expect_error(model$add("Version"))
    expect_error(model$add("SimulationControl"))
    expect_error(model$add("Material"))
    expect_error(model$add("Material"))
    expect_error(model$add("Material",
        name = "test_add", Roughness = "Rough")
    )
    expect_error(model$add("Material",
        name = "test_add", Roughness = "Rough")
    )
    expect_silent(model$add("Material", name = "test_add", roughness = "Rough",
        thickness = 0.8, conductivity = 55, density = 55, specific_heat = 100)
    )
    expect_error(model$add(class = "Material", name = "test_add",
        roughness = "Rough", thickness = 0.8, conductivity = 55, density = 55,
        specific_heat = 100, non_exist_name = 0)
    )
})
