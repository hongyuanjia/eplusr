# IdfViewer Implemention {{{
test_that("IdfViewer Implemention", {
    skip_on_cran()
    skip_on_os("mac")

    # simple model
    idf <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "5ZoneAirCooledWithSpaces.idf"))

    expect_type(geoms <- extract_geom(idf), "list")
    expect_type(geoms <- align_coord_system(geoms, "world", "world", "world"), "list")
    expect_s3_class(geoms$vertices2 <- triangulate_geoms(geoms), "data.table")

    rgl_init <- function(clear = TRUE) {
        new <- FALSE
        if (clear) {
            if (rgl::cur3d() == 0) new <- TRUE else rgl::clear3d()
        }
        if (!new) {
            dev <- rgl::cur3d()
        } else {
            rgl::open3d()
            dev <- rgl::cur3d()

            # set viewpoint
            rgl::view3d(0, -60, 60)

            # change mouse control method
            cur <- rgl::par3d("mouseMode")
            cur[["left"]] <- "trackball"
            cur[["wheel"]] <- "push"
            cur[["middle"]] <- "fov"
            rgl::par3d(dev = dev, mouseMode = cur)
            pan3d(2L)
        }

        rgl::bg3d(color = "white")

        rgl::set3d(dev)
        dev
    }

    dev <- rgl_init()
    expect_type(id_axis <- rgl_view_axis(dev, geoms), "integer")
    expect_type(id_ground <- rgl_view_ground(dev, geoms, alpha = 1.0), "integer")
    expect_type(id_wireframe <- rgl_view_wireframe(dev, geoms), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, wireframe = FALSE), "integer")
    expect_length(id_dayl_pnts <- rgl_view_point(dev, geoms), 0)

    expect_type(rgl_pop(id = id_ground), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "boundary"), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "construction"), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "zone"), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "space"), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "normal"), "integer")

    # complex model
    idf <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "HospitalLowEnergy.idf"))
    expect_type(geoms <- extract_geom(idf), "list")
    expect_type(geoms <- align_coord_system(geoms, "relative", "relative", "relative"), "list")
    expect_equal(unlist(geoms$rules[3:5], FALSE, FALSE), rep("relative", 3L))
    expect_type(geoms <- align_coord_system(geoms, "world", "world", "world"), "list")
    expect_equal(unlist(geoms$rules[3:5], FALSE, FALSE), rep("world", 3L))
    expect_s3_class(geoms$vertices2 <- triangulate_geoms(geoms), "data.table")

    expect_type(dev <- rgl_init(), "integer")
    expect_type(id_axis <- rgl_view_axis(dev, geoms), "integer")
    expect_type(id_ground <- rgl_view_ground(dev, geoms, alpha = 1.0), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "surface_type", wireframe = FALSE), "integer")
    expect_type(id_wireframe <- rgl_view_wireframe(dev, geoms), "integer")
    expect_type(id_dayl_pnts <- rgl_view_point(dev, geoms), "integer")

    expect_type(rgl_pop(id = id_ground), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "boundary"), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "construction"), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "zone"), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "normal"), "integer")

    rgl::close3d()
})
# }}}

# vim: set fdm=marker:
