context("IdfGeometry class")

# IdfGeometry {{{
test_that("Simple Geometry Conversion", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    # simple shading
    idf <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/4ZoneWithShading_Simple_1.idf"))

    geom <- idf_geometry(idf)
})
# }}}
