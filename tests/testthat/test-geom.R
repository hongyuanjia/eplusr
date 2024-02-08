# IdfGeometry {{{
test_that("IdfGeometry", {
    skip_on_cran()

    # simple shading
    idf <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "4ZoneWithShading_Simple_1.idf"))
    expect_warning(geom <- idf_geometry(idf$path()), "Relative")
    expect_warning(geom <- idf_geometry(idf), "Relative")
    suppressWarnings(expect_warning(idf_geometry(empty_idf(LATEST_EPLUS_VER))))

    idf$GlobalGeometryRules$Coordinate_System <- "Relative"

    expect_s3_class(geom$parent(), "Idf")
    expect_warning(expect_type(geom$rules(), "list"))
    expect_s3_class(geom$coord_system(), "IdfGeometry")
    expect_s3_class(geom$coord_system("world", "world", "world"), "IdfGeometry")
    expect_s3_class(geom$coord_system("absolute", "absolute", "absolute"), "IdfGeometry")
    expect_equal(unlist(geom$rules()[3:5], use.names = FALSE), rep("world", 3L))
    expect_equal(unlist(idf$GlobalGeometryRules$value(3:5), use.names = FALSE), rep("World", 3L))

    expect_s3_class(conv <- geom$convert(), "Idf")
    expect_s3_class(attr(conv, "mapping"), "data.table")

    expect_s3_class(geom$round_digits(4L), "Idf")

    expect_s3_class(geom$area(), "data.table")
    expect_s3_class(geom$area(class = "Shading:Zone:Detailed"), "data.table")
    expect_s3_class(geom$area(net = TRUE), "data.table")
    expect_s3_class({
        area <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "5ZoneAirCooledWithSpaces.idf"))$geometry()$area()
    }, "data.table")
    expect_equal(length(!is.na(area$space)), 63L)

    expect_s3_class(geom$azimuth(), "data.table")
    expect_s3_class(geom$azimuth(class = "Shading:Zone:Detailed"), "data.table")

    expect_s3_class(geom$tilt(), "data.table")
    expect_s3_class(geom$tilt(class = "Shading:Zone:Detailed"), "data.table")

    skip_on_os("mac")
    expect_s3_class(v1 <- geom$view(render_by = "construction"), "IdfViewer")
    expect_s3_class(v2 <- geom$view(axis = FALSE), "IdfViewer")
    v1$close()
    v2$close()

    expect_output(geom$print())
})
# }}}

# vim: set fdm=marker:
