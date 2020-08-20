context("IdfViwer Implemention")

# IdfViewer Implemention {{{
test_that("IdfViewer Implemention", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    # simple model
    idf <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/4ZoneWithShading_Simple_1.idf"))

    expect_is(geoms <- extract_geom(idf), "list")
    expect_is(geoms <- align_coord_system(geoms, "absolute", "absolute", "absolute"), "list")
    expect_is(geoms$vertices2 <- triangulate_geoms(geoms), "data.table")

    rgl_init <- function (clear = TRUE) {
        new <- FALSE
        if (clear) {
            if (rgl::rgl.cur() == 0) new <- TRUE else rgl::rgl.clear()
        }
        if (!new) {
            dev <- rgl::rgl.cur()
        } else {
            rgl::rgl.open()
            dev <- rgl::rgl.cur()

            # set viewpoint
            rgl::rgl.viewpoint(0, -60, 60)

            # change mouse control method
            cur <- rgl::par3d("mouseMode")
            cur[["left"]] <- "trackball"
            cur[["wheel"]] <- "push"
            cur[["middle"]] <- "fov"
            rgl::par3d(dev = dev, mouseMode = cur)
            pan3d(2L)
        }

        rgl::rgl.bg(color = "white")

        rgl::rgl.set(dev)
        dev
    }

    dev <- rgl_init()
    expect_is(id_axis <- rgl_view_axis(dev, geoms), "integer")
    expect_is(id_ground <- rgl_view_ground(dev, geoms, alpha = 1.0), "integer")
    expect_is(id_wireframe <- rgl_view_wireframe(dev, geoms), "integer")
    expect_is(id_surface <- rgl_view_surface(dev, geoms, wireframe = FALSE), "integer")
    expect_length(id_dayl_pnts <- rgl_view_point(dev, geoms), 0)

    expect_is(rgl_pop(id = id_ground), "integer")

    expect_is(rgl_pop(id = unlist(id_surface)), "integer")
    expect_is(id_surface <- rgl_view_surface(dev, geoms, "boundary"), "integer")

    expect_is(rgl_pop(id = unlist(id_surface)), "integer")
    expect_is(id_surface <- rgl_view_surface(dev, geoms, "construction"), "integer")

    expect_is(id_surface <- rgl_view_surface(dev, geoms, "zone"), "integer")

    expect_is(rgl_pop(id = unlist(id_surface)), "integer")
    expect_is(id_surface <- rgl_view_surface(dev, geoms, "normal"), "integer")

    # complex model
    idf <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/HospitalLowEnergy.idf"))
    expect_is(geoms <- extract_geom(idf), "list")
    expect_is(geoms <- align_coord_system(geoms, "relative", "relative", "relative"), "list")
    expect_equal(unlist(geoms$rules[3:5], FALSE, FALSE), rep("relative", 3L))
    expect_is(geoms <- align_coord_system(geoms, "absolute", "absolute", "absolute"), "list")
    expect_equal(unlist(geoms$rules[3:5], FALSE, FALSE), rep("absolute", 3L))
    expect_is(geoms$vertices2 <- triangulate_geoms(geoms), "data.table")

    expect_is(dev <- rgl_init(), "integer")
    expect_is(id_axis <- rgl_view_axis(dev, geoms), "integer")
    expect_is(id_ground <- rgl_view_ground(dev, geoms, alpha = 1.0), "integer")
    expect_is(id_surface <- rgl_view_surface(dev, geoms, "surface_type", wireframe = FALSE), "integer")
    expect_is(id_wireframe <- rgl_view_wireframe(dev, geoms), "integer")
    expect_is(id_dayl_pnts <- rgl_view_point(dev, geoms), "integer")

    expect_is(rgl_pop(id = id_ground), "integer")

    expect_is(rgl_pop(id = unlist(id_surface)), "integer")
    expect_is(id_surface <- rgl_view_surface(dev, geoms, "boundary"), "integer")

    expect_is(rgl_pop(id = unlist(id_surface)), "integer")
    expect_is(id_surface <- rgl_view_surface(dev, geoms, "construction"), "integer")

    expect_is(rgl_pop(id = unlist(id_surface)), "integer")
    expect_is(id_surface <- rgl_view_surface(dev, geoms, "zone"), "integer")

    expect_is(rgl_pop(id = unlist(id_surface)), "integer")
    expect_is(id_surface <- rgl_view_surface(dev, geoms, "normal"), "integer")

    rgl::rgl.close()
})
# }}}
