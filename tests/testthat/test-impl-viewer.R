context("IdfViwer Implemention")

# VIEW {{{
test_that("VIEW", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    # simple model
    idf <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/4ZoneWithShading_Simple_1.idf"))

    expect_is(geoms <- extract_geom(idf), "list")
    expect_is(geoms <- align_coord_system(geoms, "absolute", "absolute", "absolute"), "list")

    expect_is(dev <- rgl_init(), "integer")
    expect_is(id_axis <- rgl_view_axis(dev, geoms), "numeric")
    expect_is(id_ground <- rgl_view_ground(dev, geoms, alpha = 1.0), "numeric")
    expect_is(id_type <- rgl_view_surface(dev, geoms, "surface_type"), "list")
    expect_is(id_surf <- rgl_view_wireframe(dev, geoms$surface), "numeric")
    expect_is(id_subsurf <- rgl_view_wireframe(dev, geoms$subsurface), "numeric")
    expect_is(id_shading <- rgl_view_wireframe(dev, geoms$shading, width = 2), "numeric")
    expect_length(id_dayl_pnts <- rgl_view_point(dev, geoms$daylighting_point), 0)

    rgl::rgl.pop(id = id_ground)

    rgl::rgl.pop(id = unlist(id_type))
    expect_is(id_bound <- rgl_view_surface(dev, geoms, "boundary"), "list")

    rgl::rgl.pop(id = unlist(id_bound))
    expect_is(id_const <- rgl_view_surface(dev, geoms, "construction"), "list")

    rgl::rgl.pop(id = unlist(id_const))
    expect_is(id_zone <- rgl_view_surface(dev, geoms, "zone"), "list")

    rgl::rgl.pop(id = unlist(id_zone))
    expect_is(id_norm <- rgl_view_surface(dev, geoms, "normal"), "list")

    # complex model
    idf <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/HospitalLowEnergy.idf"))
    expect_is(geoms <- extract_geom(idf), "list")
    expect_is(geoms <- align_coord_system(geoms, "relative", "relative", "relative"), "list")
    expect_equal(unlist(geoms$rules[3:5], FALSE, FALSE), rep("relative", 3L))
    expect_is(geoms <- align_coord_system(geoms, "absolute", "absolute", "absolute"), "list")
    expect_equal(unlist(geoms$rules[3:5], FALSE, FALSE), rep("absolute", 3L))

    expect_is(dev <- rgl_init(), "integer")
    expect_is(id_axis <- rgl_view_axis(dev, geoms), "numeric")
    expect_is(id_ground <- rgl_view_ground(dev, geoms, alpha = 1.0), "numeric")
    expect_is(id_type <- rgl_view_surface(dev, geoms, "surface_type"), "list")
    expect_is(id_surf <- rgl_view_wireframe(dev, geoms$surface), "numeric")
    expect_is(id_subsurf <- rgl_view_wireframe(dev, geoms$subsurface), "numeric")
    expect_is(id_shading <- rgl_view_wireframe(dev, geoms$shading, width = 2), "numeric")
    expect_is(id_dayl_pnts <- rgl_view_point(dev, geoms$daylighting_point), "numeric")

    rgl::rgl.pop(id = id_ground)

    rgl::rgl.pop(id = unlist(id_type))
    expect_is(id_bound <- rgl_view_surface(dev, geoms, "boundary"), "list")

    rgl::rgl.pop(id = unlist(id_bound))
    expect_is(id_const <- rgl_view_surface(dev, geoms, "construction"), "list")

    rgl::rgl.pop(id = unlist(id_const))
    expect_is(id_zone <- rgl_view_surface(dev, geoms, "zone"), "list")

    rgl::rgl.pop(id = unlist(id_zone))
    expect_is(id_norm <- rgl_view_surface(dev, geoms, "normal"), "list")

    idf <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/4ZoneWithShading_Simple_1.idf"))
    geoms <- extract_geom(idf)
    geoms <- align_coord_system(geoms, "absolute", "absolute", "absolute")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show_hidden = TRUE), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, wireframe = FALSE), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, x_ray = TRUE, ground = TRUE), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, axis = FALSE), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show = "floor"), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show = "floor", show_hidden = TRUE), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show = "roof"), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show = "roof", show_hidden = TRUE), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show = "wall"), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show = "wall", show_hidden = TRUE), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show = "window"), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show = "window", show_hidden = TRUE), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show = "door"), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show = "door", show_hidden = TRUE), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show = "shading"), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show = "shading", show_hidden = TRUE), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show = "daylighting"), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, show = "daylighting", show_hidden = TRUE), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, zone = "zone 1"), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, zone = "zone 1", show_hidden = TRUE), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, zone = "zone 1", show = "floor", show_hidden = TRUE), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, zone = "zone 1", show = "floor", show_hidden = TRUE), "list")

    dev <- rgl_init()
    expect_is(rgl_view(dev, geoms, zone = "zone 1", show = "floor", surface = 57, show_hidden = TRUE), "list")
})
# }}}
