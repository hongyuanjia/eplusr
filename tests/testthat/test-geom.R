context("IdfGeometry class")

# IdfGeometry {{{
test_that("IdfGeometry", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    # simple shading
    idf <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/4ZoneWithShading_Simple_1.idf"))
    geom <- idf_geometry(idf)

    idf$GlobalGeometryRules$Coordinate_System <- "Relative"
    idf$Output_Meter <- NULL

    expect_is(geom$parent(), "Idf")
    expect_is(geom$rules(), "list")
    expect_is(geom$coord_system("absolute", "absolute", "absolute"), "Idf")
    expect_equal(unlist(geom$rules()[3:5], use.names = FALSE), rep("absolute", 3L))
    expect_equal(unlist(idf$GlobalGeometryRules$value(3:5), use.names = FALSE), rep("Absolute", 3L))

    expect_is(conv <- geom$convert(), "Idf")
    expect_is(attr(conv, "mapping"), "data.table")

    expect_is(geom$round_digits(4L), "Idf")

    expect_is(geom$area(), "data.table")
    expect_is(geom$area(class = "Shading:Zone:Detailed"), "data.table")
    expect_is(geom$area(net = TRUE), "data.table")

    expect_is(geom$azimuth(), "data.table")
    expect_is(geom$azimuth(class = "Shading:Zone:Detailed"), "data.table")

    expect_is(geom$tilt(), "data.table")
    expect_is(geom$tilt(class = "Shading:Zone:Detailed"), "data.table")

    expect_is(v1 <- geom$view(render_by = "construction"), "IdfViewer")
    expect_is(v2 <- geom$view(axis = FALSE), "IdfViewer")
    v1$close()
    v2$close()

    expect_output(geom$print())
})
# }}}
