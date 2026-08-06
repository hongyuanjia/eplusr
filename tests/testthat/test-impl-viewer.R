# IdfViewer Implemention {{{
test_that("Geometry triangulation preserves polygon holes", {
    skip_if_not_installed("decido")

    geoms <- list(
        surface = data.table(id = 1L, name = "wall"),
        subsurface = data.table(
            id = 2L, name = "window", building_surface_name = "wall"
        ),
        vertices = rbindlist(list(
            data.table(
                id = 1L, index = 1:4,
                x = c(0, 4, 4, 0), y = c(0, 0, 4, 4), z = 0
            ),
            data.table(
                id = 2L, index = 1:3,
                x = c(1, 2, 1), y = c(1, 1, 2), z = 0
            )
        ))
    )

    triangulated <- triangulate_geoms(geoms)
    expect_true(all(is.finite(triangulated$x)))
    expect_true(all(is.finite(triangulated$y)))
    expect_true(all(is.finite(triangulated$z)))
    expect_equal(nrow(triangulated[id == 1L]), 21L)
    expect_equal(nrow(triangulated[id == 2L]), 3L)

    # Compare total triangle area instead of implementation-specific indices.
    surface <- triangulated[id == 1L]
    surface[, triangle := rep(seq_len(.N %/% 3L), each = 3L)]
    area <- surface[, by = "triangle", .(
        area = abs(
            (x[2L] - x[1L]) * (y[3L] - y[1L]) -
                (x[3L] - x[1L]) * (y[2L] - y[1L])
        ) / 2
    )][, sum(area)]
    expect_equal(area, 15.5, tolerance = 1e-10)
})

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
