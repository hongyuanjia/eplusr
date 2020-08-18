# rgl_viewpoint {{{
rgl_viewpoint <- function (dev, look_at = "iso", theta = NULL, phi = NULL, fov = NULL, zoom = NULL, scale = NULL) {
    assert_choice(look_at, c("iso", "top", "bottom", "front", "back", "left", "right"), null.ok = TRUE)
    assert_number(phi, lower = -90, upper = 90, null.ok = TRUE)
    assert_number(fov, lower = 0, upper = 179, null.ok = TRUE)

    if (is.null(fov)) fov <- rgl::par3d(dev = dev, "FOV")
    if (is.null(zoom)) zoom <- rgl::par3d(dev = dev, "zoom")
    if (is.null(scale)) scale <- rgl::par3d(dev = dev, "scale")

    if (!is.null(theta) || !is.null(phi)) {
        rot <- get_viewpoint_rotation(dev)

        if (!is.null(theta)) rot[2L] <- theta
        if (!is.null(phi)) rot[1L] <- phi
    } else if (!is.null(look_at)) {
        rot <- switch(look_at,
            top = c(0, 0, 0),
            bottom = c(180, 0, 0),
            front = c(-90, 0, 0),
            back = c(-90, 0, -180),
            left = c(-90, 0, 90),
            right = c(-90, 0, -90),
            iso = c(-75, 0, -30)
        )
    } else {
        # do nothing
        return(list(userMatrix = rgl::par3d(dev = dev, "userMatrix"), fov = fov, zoom = zoom, scale = scale))
    }

    m <- get_viewpoint_matrix(rot[1], rot[2], rot[3])

    rgl::rgl.set(dev)
    rgl::rgl.viewpoint(fov = fov, zoom = zoom, scale = scale, userMatrix = m)

    list(userMatrix = m, fov = fov, zoom = zoom, scale = scale)
}
# }}}

# rgl_view_surface {{{
rgl_view_surface <- function (dev, geoms, type = "surface_type", x_ray = FALSE, lit = FALSE) {
    assert_choice(type, c("surface_type", "boundary", "construction", "zone", "normal"))
    assert_flag(x_ray)

    rgl::rgl.set(dev)

    geoms <- map_view_color(geoms, type = type, x_ray = x_ray)

    id_surface_detailed <- integer()
    id_surface_simple <- integer()
    id_subsurface_detailed <- integer()
    id_subsurface_simple <- integer()
    id_shading_detailed <- integer()
    id_shading_simple <- integer()

    # handle special cases
    if (!any(geoms$surface$vertices$index < 0L)) {
        id_line_surface <- rgl_view_wireframe(dev, geoms$surface)
        id_line_subsurface <- rgl_view_wireframe(dev, geoms$subsurface)
    } else {
        id_line_surface <- rgl_view_wireframe(dev,
            list(meta = geoms$surface$meta, vertices = geoms$surface$vertices[index > 0L])
        )

        v <- geoms$surface$vertices[index < 0L][, index := -index]
        v[J(1L), on = "index", id := .I]
        v[, id := id[[1L]], by = list(cumsum(index == 1L))]

        id_line_subsurface <- rgl_view_wireframe(dev,
            list(meta = geoms$subsurface$meta,
                 vertices = rbindlist(list(geoms$subsurface$vertices, v))
            )
        )
    }
    id_line_shading <- rgl_view_wireframe(dev, geoms$shading, width = 2)

    # surface type {{{
    if (type == "surface_type") {
        if (nrow(geoms$surface$meta)) {
            detailed <- add_surface_hole_vertices(geoms$surface, geoms$subsurface)
            detailed <- triangulate_surfaces(detailed)

            detailed[geoms$surface$meta, on = "id", `:=`(color_ext = i.color_ext, color_int = i.color_int, alpha = i.alpha)]
            id_surface_detailed <- rgl_view_surface_twoside_tri(detailed, lit)
        }

        # simple subsurfaces are just rectangulars
        if (nrow(geoms$subsurface$meta)) {
            is_simple <- !stri_endswith_fixed(geoms$subsurface$meta$class, "Detailed")
            if (any(is_simple)) {
                simple <- geoms$subsurface$vertices[
                    geoms$subsurface$meta[is_simple, list(id, color_ext, color_int, alpha)],
                    on = "id", nomatch = NULL
                ]
                id_subsurface_simple <- rgl_view_surface_twoside_quad(simple, lit)
            }

            if (any(!is_simple)) {
                # triangulate
                detailed <- triangulate_surfaces(geoms$subsurface$vertices[J(geoms$subsurface$meta$id[!is_simple]), on = "id"])
                detailed[geoms$subsurface$meta[!is_simple], on = "id",
                    `:=`(color_ext = i.color_ext, color_int = i.color_int, alpha = i.alpha)]
                id_subsurface_detailed <- rgl_view_surface_twoside_tri(detailed, lit)
            }
        }

        # simple shading are just rectangulars
        if (nrow(geoms$shading$meta)) {
            is_simple <- !stri_endswith_fixed(geoms$shading$meta$class, "Detailed")
            if (any(is_simple)) {
                simple <- geoms$shading$vertices[
                    geoms$shading$meta[is_simple, list(id, color_ext, color_int, alpha)],
                    on = "id", nomatch = NULL
                ]
                id_shading_simple <- rgl_view_surface_twoside_quad(simple, lit)
            }

            if (any(!is_simple)) {
                # triangulate
                detailed <- triangulate_surfaces(geoms$shading$vertices[J(geoms$shading$meta$id[!is_simple]), on = "id"])
                detailed[geoms$shading$meta[!is_simple], on = "id",
                    `:=`(color_ext = i.color_ext, color_int = i.color_int, alpha = i.alpha)]
                id_shading_detailed <- rgl_view_surface_twoside_tri(detailed, lit)
            }
        }
    # }}}
    # boundary & construction {{{
    } else if (type == "boundary" || type == "construction") {
        if (nrow(geoms$surface$meta)) {
            detailed <- add_surface_hole_vertices(geoms$surface, geoms$subsurface)
            detailed <- triangulate_surfaces(detailed)

            detailed[geoms$surface$meta, on = "id", `:=`(color = i.color, alpha = i.alpha)]
            id_surface_detailed <- rgl_view_surface_oneside_tri(detailed, lit)
        }

        # simple subsurfaces are just rectangulars
        if (nrow(geoms$subsurface$meta)) {
            is_simple <- !stri_endswith_fixed(geoms$subsurface$meta$class, "Detailed")
            if (any(is_simple)) {
                simple <- geoms$subsurface$vertices[
                    geoms$subsurface$meta[is_simple, list(id, color, alpha)],
                    on = "id", nomatch = NULL
                ]
                id_subsurface_simple <- rgl_view_surface_oneside_quad(simple, lit)
            }

            if (any(!is_simple)) {
                # triangulate
                detailed <- triangulate_surfaces(geoms$subsurface$vertices[J(geoms$subsurface$meta$id[!is_simple]), on = "id"])
                detailed[geoms$subsurface$meta[!is_simple], on = "id", `:=`(color = i.color, alpha = i.alpha)]
                id_subsurface_detailed <- rgl_view_surface_oneside_tri(detailed, lit)
            }
        }

        # simple shading are just rectangulars
        if (nrow(geoms$shading$meta)) {
            is_simple <- !stri_endswith_fixed(geoms$shading$meta$class, "Detailed")
            if (any(is_simple)) {
                simple <- geoms$shading$vertices[
                    geoms$shading$meta[is_simple, list(id, color, alpha)],
                    on = "id", nomatch = NULL
                ]
                id_shading_simple <- rgl_view_surface_oneside_quad(simple, lit)
            }

            if (any(!is_simple)) {
                # triangulate
                detailed <- triangulate_surfaces(geoms$shading$vertices[J(geoms$shading$meta$id[!is_simple]), on = "id"])
                detailed[geoms$shading$meta[!is_simple], on = "id",
                    `:=`(color = i.color, alpha = i.alpha)]
                id_shading_detailed <- rgl_view_surface_oneside_tri(detailed, lit)
            }
        }
    # }}}
    # zone {{{
    # no need to plot subsurface
    } else if (type == "zone") {
        # simple surfaces are just rectangulars
        if (nrow(geoms$surface$meta)) {
            is_simple <- !stri_endswith_fixed(geoms$surface$meta$class, "Detailed")
            if (any(is_simple)) {
                simple <- geoms$surface$vertices[
                    geoms$surface$meta[is_simple, list(id, color, alpha)],
                    on = "id", nomatch = NULL
                ]
                id_surface_simple <- rgl_view_surface_oneside_quad(simple, lit)
            }

            if (any(!is_simple)) {
                # triangulate
                detailed <- triangulate_surfaces(geoms$surface$vertices[J(geoms$surface$meta$id[!is_simple]), on = "id"])
                detailed[geoms$surface$meta[!is_simple], on = "id", `:=`(color = i.color, alpha = i.alpha)]
                id_surface_detailed <- rgl_view_surface_oneside_tri(detailed, lit)
            }
        }

        # simple shading are just rectangulars
        if (nrow(geoms$shading$meta)) {
            is_simple <- !stri_endswith_fixed(geoms$shading$meta$class, "Detailed")
            if (any(is_simple)) {
                simple <- geoms$shading$vertices[
                    geoms$shading$meta[is_simple, list(id, color, alpha)],
                    on = "id", nomatch = NULL
                ]
                id_shading_simple <- rgl_view_surface_oneside_quad(simple, lit)
            }

            if (any(!is_simple)) {
                # triangulate
                detailed <- triangulate_surfaces(geoms$shading$vertices[J(geoms$shading$meta$id[!is_simple]), on = "id"])
                detailed[geoms$shading$meta[!is_simple], on = "id", `:=`(color = i.color, alpha = i.alpha)]
                id_shading_detailed <- rgl_view_surface_oneside_tri(detailed, lit)
            }
        }
    # }}}
    # normal {{{
    } else if (type == "normal") {
        # simple surfaces are just rectangulars
        if (nrow(geoms$surface$meta)) {
            is_simple <- !stri_endswith_fixed(geoms$surface$meta$class, "Detailed")
            if (any(is_simple)) {
                simple <- geoms$surface$vertices[
                    geoms$surface$meta[is_simple, list(id, color_ext, color_int, alpha)],
                    on = "id", nomatch = NULL
                ]
                id_surface_simple <- rgl_view_surface_twoside_quad(simple, lit)
            }

            if (any(!is_simple)) {
                # triangulate
                detailed <- triangulate_surfaces(geoms$surface$vertices[J(geoms$surface$meta$id[!is_simple]), on = "id"])
                detailed[geoms$surface$meta[!is_simple], on = "id",
                    `:=`(color_ext = i.color_ext, color_int = i.color_int, alpha = i.alpha)]
                id_surface_detailed <- rgl_view_surface_twoside_tri(detailed, lit)
            }
        }

        # simple shading are just rectangulars
        if (nrow(geoms$shading$meta)) {
            is_simple <- !stri_endswith_fixed(geoms$shading$meta$class, "Detailed")
            if (any(is_simple)) {
                simple <- geoms$shading$vertices[
                    geoms$shading$meta[is_simple, list(id, color_ext, color_int, alpha)],
                    on = "id", nomatch = NULL
                ]
                id_shading_simple <- rgl_view_surface_twoside_quad(simple, lit)
            }

            if (any(!is_simple)) {
                # triangulate
                detailed <- triangulate_surfaces(geoms$shading$vertices[J(geoms$shading$meta$id[!is_simple]), on = "id"])
                detailed[geoms$shading$meta[!is_simple], on = "id",
                    `:=`(color_ext = i.color_ext, color_int = i.color_int, alpha = i.alpha)]
                id_shading_detailed <- rgl_view_surface_twoside_tri(detailed, lit)
            }
        }
    }
    # }}}

    list(surface = list(line = id_line_surface, detailed = id_surface_detailed, simple = id_surface_simple),
         subsurface = list(line = id_line_subsurface, detailed = id_subsurface_detailed, simple = id_subsurface_simple),
         shading = list(line = id_line_shading, detailed = id_shading_detailed, simple = id_shading_simple)
    )
}
# }}}

# rgl_view_surface_twoside_tri {{{
rgl_view_surface_twoside_tri <- function (vertices, lit = FALSE) {
    ext <- rgl::rgl.triangles(
        x = as.matrix(fast_subset(vertices, c("x", "y", "z"))),
        color = as.matrix(fast_subset(vertices, rep("color_ext", 3L))),
        alpha = as.matrix(fast_subset(vertices, rep("alpha", 3L))),
        front = "filled", back = "culled", lit = lit
    )
    int <- rgl::rgl.triangles(
        x = as.matrix(fast_subset(vertices, c("x", "y", "z"))),
        color = as.matrix(fast_subset(vertices, rep("color_int", 3L))),
        alpha = as.matrix(fast_subset(vertices, rep("alpha", 3L))),
        front = "culled", back = "filled", lit = lit
    )
    c(ext, int)
}
# }}}

# rgl_view_surface_twoside_quad {{{
rgl_view_surface_twoside_quad <- function (vertices, lit = FALSE) {
    ext <- rgl::rgl.quads(
        x = as.matrix(fast_subset(vertices, c("x", "y", "z"))),
        color = as.matrix(fast_subset(vertices, rep("color_ext", 3L))),
        alpha = as.matrix(fast_subset(vertices, rep("alpha", 3L))),
        front = "filled", back = "culled", lit = lit
    )
    int <- rgl::rgl.quads(
        x = as.matrix(fast_subset(vertices, c("x", "y", "z"))),
        color = as.matrix(fast_subset(vertices, rep("color_int", 3L))),
        alpha = as.matrix(fast_subset(vertices, rep("alpha", 3L))),
        front = "culled", back = "filled", lit = lit
    )
    c(ext, int)
}
# }}}

# rgl_view_surface_oneside_tri {{{
rgl_view_surface_oneside_tri <- function (vertices, lit = FALSE) {
    rgl::rgl.triangles(
        x = as.matrix(fast_subset(vertices, c("x", "y", "z"))),
        color = as.matrix(fast_subset(vertices, rep("color", 3L))),
        alpha = as.matrix(fast_subset(vertices, rep("alpha", 3L))),
        lit = lit
    )
}
# }}}

# rgl_view_surface_oneside_quad {{{
rgl_view_surface_oneside_quad <- function (vertices, lit = FALSE) {
    rgl::rgl.quads(
        x = as.matrix(fast_subset(vertices, c("x", "y", "z"))),
        color = as.matrix(fast_subset(vertices, rep("color", 3L))),
        alpha = as.matrix(fast_subset(vertices, rep("alpha", 3L))),
        lit = lit
    )
}
# }}}

# rgl_view_wireframe {{{
rgl_view_wireframe <- function (dev, geom, color = "black", width = 1.5, alpha = 1.0, ...) {
    if (!nrow(geom$vertices)) return(integer())

    rgl::rgl.set(dev)
    l <- pair_line_vertex(geom$vertices)
    rgl::rgl.lines(l$x, l$y, l$z, color = color, lwd = width, lit = FALSE, alpha = alpha, ...)
}
# }}}

# rgl_view_point {{{
rgl_view_point <- function (dev, geom, color = "red", size = 8.0, lit = TRUE, ...) {
    if (!nrow(geom$vertices)) return(integer())

    rgl::rgl.set(dev)
    rgl::rgl.points(geom$vertices$x, geom$vertices$y, geom$vertices$z,
        color = color, size = size, lit = lit, ...)
}
# }}}

# rgl_view_axis {{{
rgl_view_axis <- function (dev, geoms, expand = 2.0, width = 1.5, color = c("red", "green", "blue", "orange"), alpha = 1.0) {
    assert_number(expand, finite = TRUE, lower = 1.0)
    assert_number(width, lower = 1E-5, finite = TRUE)
    assert_number(alpha, lower = 0, upper = 1)
    assert_character(color, len = 4L, any.missing = FALSE)

    types <- c("surface", "subsurface", "shading", "daylighting_point")

    x <- y <- z <- 0.0
    for (nm in types) {
        if (nrow(geoms[[nm]]$vertices)) {
            x <- max(c(geoms[[nm]]$vertices$x, x), na.rm = TRUE)
            y <- max(c(geoms[[nm]]$vertices$y, y), na.rm = TRUE)
            z <- max(c(geoms[[nm]]$vertices$z, z), na.rm = TRUE)
        }
    }

    if (x == y && y == z && x == 0.0) return(list(north = integer(), axis = integer()))

    val <- max(c(x, y, z))

    # add north axis
    id_north <- integer()
    if (!is.na(geoms$building$north_axis) && geoms$building$north_axis != 0.0) {
        north <- geoms$building$north_axis
        v <- matrix(c(0, 0, 0, 1, 0, val * expand, 0, 1), ncol = 4L, byrow = TRUE)
        v <- rgl::rotate3d(v, deg_to_rad(-north), 0, 0, 1)
        v <- set(setnames(as.data.table(v[, 1:3]), c("x", "y", "z")), NULL, c("id", "index"), list(1L, 1:2))

        id_north <- rgl_view_wireframe(dev, list(vertices = v), color = color[4L], width = width * 2)
    }

    vert <- data.table(
        id = 1L, index = 1:6,
        x = c(0.0, val * expand, rep(0.0, 4L)),
        y = c(rep(0.0, 2L), 0.0, val * expand, rep(0.0, 2L)),
        z = c(rep(0.0, 4L), 0.0, val * max(c(1.25, expand / 2)))
    )

    list(north = id_north,
        axis = rgl_view_wireframe(dev, list(vertices = vert), width = width,
            alpha = alpha, color = rep(c(color[[1L]], color[[3L]], color[[2L]]), each = 2L))
    )
}
# }}}

# rgl_view_ground {{{
rgl_view_ground <- function (dev, geoms, expand = 1.02, color = "#EDEDEB", alpha = 1.0) {
    assert_number(expand, finite = TRUE, lower = 1.0)
    assert_number(alpha, lower = 0.0, upper = 1.0)
    assert_string(color)

    types <- c("surface", "subsurface", "shading", "daylighting_point")

    x <- y <- c(0.0, 0.0)
    for (nm in types) {
        if (nrow(geoms[[nm]]$vertices)) {
            x <- range(c(geoms[[nm]]$vertices$x, x), na.rm = TRUE)
            y <- range(c(geoms[[nm]]$vertices$y, y), na.rm = TRUE)
        }
    }

    if (x == y && x == 0.0) return(integer())

    dis_x <- x[2L] - x[1L]
    dis_y <- y[2L] - y[1L]
    expand <- expand - 1.0
    rgl::rgl.quads(
        c(x[[1L]] - dis_x * expand,
          x[[2L]] + dis_x * expand,
          x[[2L]] + dis_x * expand,
          x[[1L]] - dis_x * expand),
        c(y[[1L]] - dis_y * expand,
          y[[1L]] - dis_y * expand,
          y[[2L]] + dis_y * expand,
          y[[2L]] + dis_y * expand),
        0,
        color = color, lit = FALSE, alpha = alpha
    )
}
# }}}

# rgl_snapshot {{{
rgl_snapshot <- function (dev, filename) {
    assert_string(filename)

    # set the last plot device as active
    rgl::rgl.set(dev)

    if (!dir.exists(dirname(filename))) dir.create(dirname(filename), recursive = TRUE)

    if (has_ext(filename, "png")) {
        rgl::rgl.snapshot(filename, "png", top = FALSE)
    } else if (has_ext(filename, c("ps", "eps", "tex", "pdf", "svg", "pgf"))) {
        rgl::rgl.postscript(filename, tools::file_ext(filename))
    } else {
        abort(paste0("Not supported export format ", surround(tools::file_ext(filename)), ". ",
            "Current supported: ", collapse(c("png", "ps", "eps", "tex", "pdf", "svg", "pgf"), max_num = 10)
        ))
    }

    normalizePath(filename)
}
# }}}

# get_viewpoint_rotation {{{
get_viewpoint_rotation <- function (dev) {
    m <- rgl::par3d(dev = dev, "userMatrix")

    x <- atan2(-m[2L, 3L], m[3L, 3L])
    y <- -asin(m[1L, 3L])

    sin_z <- cos(x) * m[2L, 1L] + sin(x) * m[3L, 1L]
    cos_z <- cos(x) * m[2L, 2L] + sin(x) * m[3L, 2L]
    z <- atan2(sin_z, cos_z)

    if (x * 180 / pi < -90) x <- x + pi

    if (x * 180 / pi > 90) x <- x - pi

    c(x, y, z) * 180 / pi
}
# }}}

# get_viewpoint_matrix {{{
get_viewpoint_matrix <- function (deg_x, deg_y, deg_z) {
    viewpoint_rotate_matrix("x", deg_x) %*%
    viewpoint_rotate_matrix("y", deg_y) %*%
    viewpoint_rotate_matrix("z", deg_z)
}
# }}}

# viewpoint_rotate_matrix {{{
viewpoint_rotate_matrix <- function (axis, degree) {
    rad <- deg_to_rad(degree)
    s <- sin(rad)
    c <- cos(rad)

    m <- diag(4L)

    if (axis == "x") {
        m[2L, 2L] <- c
        m[2L, 3L] <- -s
        m[3L, 2L] <- s
        m[3L, 3L] <- c
    } else if (axis == "y") {
        m[1L, 1L] <- c
        m[1L, 3L] <- s
        m[3L, 1L] <- -s
        m[3L, 3L] <- c
    } else if (axis == "z") {
        m[1L, 1L] <- c
        m[1L, 2L] <- -s
        m[2L, 1L] <- s
        m[2L, 2L] <- c
    }

    m
}
# }}}

# pair_line_vertex {{{
pair_line_vertex <- function (vertices) {
    # for lines, vertices should be provided in pairs
    # only need to plot surfaces here, since windows and holes are the same
    # things here
    vertices[, by = "id", {
        idx <- c(sort(c(index, index[-1L])), 1L)
        list(x = x[idx], y = y[idx], z = z[idx])
    }]
}
# }}}

# add_surface_hole_vertices {{{
add_surface_hole_vertices <- function (geom_surface, geom_subsurface) {
    if (!nrow(geom_subsurface$meta)) return(geom_surface$vertices)

    geom_subsurface$meta[geom_surface$meta, on = c("building_surface_name" = "name"), `:=`(id_surface = i.id)]
    on.exit(set(geom_subsurface$meta, NULL, "id_surface", NULL), add = TRUE)

    holes <- geom_subsurface$vertices[fast_subset(geom_subsurface$meta, c("id", "id_surface")), on = "id", nomatch = NULL]

    if (!nrow(holes)) return(geom_surface$vertices)

    set(holes, NULL, c("id", "id_surface", "index"), list(holes$id_surface, NULL, -holes$index))
    # add windows as holes
    setorderv(rbindlist(list(geom_surface$vertices, holes)), "id")
}
# }}}

# triangulate_surfaces {{{
triangulate_surfaces <- function (vertices) {
    # tweaked for speed
    trans <- align_face(vertices)
    set(trans, NULL, "rev_trans", lapply(trans$trans, solve.default))
    add_joined_cols(trans, vertices, "id", c("trans", "rev_trans"))
    set(vertices, NULL, "vertice_index", seq_len(nrow(vertices)))

    vertices[, by = "vertice_index", c("rev_x", "rev_y", "rev_z") := {
        v <- rev_trans[[1L]] %*% c(x, y, z, 1L)
        list(v[1L,], v[2L,], v[3L,])
    }]

    vertices[, by = "id", `:=`(hole = max(which(index == -1L), 0L))]

    vert <- vertices[, by = "id", {
        tri <- decido::earcut(list(rev_x, rev_y), hole[1L])
        list(index = seq_along(tri),
             x = rev_x[tri], y = rev_y[tri], z = rev_z[tri],
             trans = trans[1L]
        )
    }]

    # clean
    set(vertices, NULL, setdiff(names(vertices), c("id", "index", "x", "y", "z")), NULL)

    set(vert, NULL, "vertice_index", seq_len(nrow(vert)))
    vert[, by = "vertice_index", c("x", "y", "z") := {
        v <- trans[[1L]] %*% c(x, y, z, 1L)
        list(v[1L,], v[2L,], v[3L,])
    }]

    # clean
    set(vert, NULL, c("trans", "vertice_index"), NULL)
}
# }}}

# pan3d {{{
# adapted from rgl examples
pan3d <- function(button, dev = rgl::rgl.cur(), subscene = rgl::currentSubscene3d(dev)) {
    start <- list()

    begin <- function(x, y) {
        activeSubscene <- rgl::par3d("activeSubscene", dev = dev)
        start$listeners <<- rgl::par3d("listeners", dev = dev, subscene = activeSubscene)
        for (sub in start$listeners) {
            init <- rgl::par3d(c("userProjection", "viewport"), dev = dev, subscene = sub)
            init$pos <- c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5)
            start[[as.character(sub)]] <<- init
        }
    }

    update <- function(x, y) {
        for (sub in start$listeners) {
            init <- start[[as.character(sub)]]
            xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
            mouseMatrix <- rgl::translationMatrix(xlat[1], xlat[2], xlat[3])
            rgl::par3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = sub)
        }
    }
    rgl::rgl.setMouseCallbacks(button, begin, update, dev = dev, subscene = subscene)
}
# }}}

# pan_view {{{
pan_view <- function (dev, x, y, z) {
    init <- rgl::par3d(c("userProjection", "viewport"), dev = dev)
    init$pos <- c(0.5, 0.5, 0.5)

    xlat <- 2 * (c(x, y, z) - init$pos)
    mouseMatrix <- rgl::translationMatrix(xlat[1], xlat[2], xlat[3])
    rgl::par3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev)
}
# }}}

# rgl_pop {{{
rgl_pop <- function (id, type = "shapes") {
    try(rgl::rgl.pop(id = id, type = "shapes"), silent = TRUE)
}
# }}}

# map_view_color {{{
map_view_color <- function (geoms, type = "surface_type", x_ray = FALSE) {
    assert_choice(type, c("surface_type", "boundary", "construction", "zone", "normal"))
    alpha <- if (x_ray) 0.4 else 1.0

    set.seed(1L)

    if (type == "normal") {
        # add surface
        if (nrow(geoms$surface$meta)) {
            set(geoms$surface$meta, NULL, c("color_ext", "color_int", "alpha"),
                list(COLOR_MAP$surface_type["NormalMaterial_Ext"],
                     COLOR_MAP$surface_type["NormalMaterial_Int"],
                     alpha
                )
            )
        }
        if (nrow(geoms$subsurface$meta)) {
            set(geoms$subsurface$meta, NULL, c("color_ext", "color_int", "alpha"),
                list(COLOR_MAP$surface_type["NormalMaterial_Ext"],
                     COLOR_MAP$surface_type["NormalMaterial_Int"],
                     alpha
                )
            )
        }
        if (nrow(geoms$shading$meta)) {
            set(geoms$shading$meta, NULL, c("color_ext", "color_int", "alpha"),
                list(COLOR_MAP$surface_type["NormalMaterial_Ext"],
                     COLOR_MAP$surface_type["NormalMaterial_Int"],
                     alpha
                )
            )
        }
    } else if (type == "surface_type") {
        map <- data.table(
            surface_type = names(COLOR_MAP$surface_type),
            color = COLOR_MAP$surface_type
        )[!J("Undefined"), on = "surface_type"]
        map <- map[
          !(seq_len(nrow(map)) %% 3L == 1L)][
          , `:=`(surface_type = gsub("_.+", "", surface_type),
                 type = rep(c("ext", "int"), .N/2)
          )
        ]
        map <- dcast.data.table(map, surface_type ~ type, value.var = "color")

        # add surface
        if (nrow(geoms$surface$meta)) {
            set(geoms$surface$meta, NULL, c("color_ext", "color_int", "alpha"),
                list(COLOR_MAP$surface_type["Undefined"],
                     COLOR_MAP$surface_type["Undefined"],
                     alpha
                )
            )
            geoms$surface$meta[map, on = "surface_type", `:=`(color_ext = i.ext, color_int = i.int)]
        }
        if (nrow(geoms$subsurface$meta)) {
            set(geoms$subsurface$meta, NULL, c("color_ext", "color_int", "alpha"),
                list(COLOR_MAP$surface_type["Undefined"],
                     COLOR_MAP$surface_type["Undefined"],
                     alpha
                )
            )
            geoms$subsurface$meta[map, on = "surface_type", `:=`(color_ext = i.ext, color_int = i.int)]
            geoms$subsurface$meta[J(c("Window", "GlassDoor")), on = "surface_type", `:=`(alpha = 0.6)]
        }
        if (nrow(geoms$shading$meta)) {
            set(geoms$shading$meta, NULL, c("color_ext", "color_int", "alpha"),
                list(COLOR_MAP$surface_type["Undefined"],
                     COLOR_MAP$surface_type["Undefined"],
                     alpha
                )
            )
            geoms$shading$meta[map, on = "surface_type", `:=`(color_ext = i.ext, color_int = i.int)]
        }
    } else if (type == "boundary") {
        map <- data.table(
            boundary = names(COLOR_MAP$boundary),
            color = COLOR_MAP$boundary
        )

        # add surface
        if (nrow(geoms$surface$meta)) {
            set(geoms$surface$meta, NULL, c("color", "alpha"),
                list(COLOR_MAP$surface_type["Undefined"], alpha)
            )
            geoms$surface$meta[map, on = c("outside_boundary_condition" = "boundary"), `:=`(color = i.color)]
            geoms$surface$meta[J("Outdoors", "SunExposed", "NoWind"),
                on = c("outside_boundary_condition", "sun_exposure", "wind_exposure"),
                `:=`(color = COLOR_MAP$boundary["Outdoors_Sun"])]
            geoms$surface$meta[J("Outdoors", "NoSun", "WindExposed"),
                on = c("outside_boundary_condition", "sun_exposure", "wind_exposure"),
                `:=`(color = COLOR_MAP$boundary["Outdoors_Wind"])]
            geoms$surface$meta[J("Outdoors", "SunExposed", "WindExposed"),
                on = c("outside_boundary_condition", "sun_exposure", "wind_exposure"),
                `:=`(color = COLOR_MAP$boundary["Outdoors_Sunwind"])]
        }
        if (nrow(geoms$subsurface$meta)) {
            set(geoms$subsurface$meta, NULL, c("color", "alpha"),
                list(COLOR_MAP$surface_type["Undefined"], alpha)
            )
        }
        if (nrow(geoms$shading$meta)) {
            set(geoms$shading$meta, NULL, c("color", "alpha"),
                list(COLOR_MAP$surface_type["Undefined"], alpha)
            )
        }
    } else if (type == "construction") {
        const <- unique(c(
            geoms$surface$meta$construction_name,
            geoms$subsurface$meta$construction_name
        ))
        map <- data.table(
            construction_name = const,
            color = sample(COLOR_MAP$color, length(const), replace = TRUE)
        )

        # add surface
        if (nrow(geoms$surface$meta)) {
            set(geoms$surface$meta, NULL, "alpha", list(alpha))
            geoms$surface$meta[map, on = "construction_name", `:=`(color = i.color)]
        }
        if (nrow(geoms$subsurface$meta)) {
            set(geoms$subsurface$meta, NULL, "alpha", list(alpha))
            geoms$subsurface$meta[map, on = "construction_name", `:=`(color = i.color)]
        }
        if (nrow(geoms$shading$meta)) {
            set(geoms$shading$meta, NULL, c("color", "alpha"),
                list(COLOR_MAP$surface_type["Undefined"], alpha)
            )
        }
    } else if (type == "zone") {
        set(geoms$zone, NULL, c("color", "alpha"), list(sample(COLOR_MAP$color, nrow(geoms$zone), replace = TRUE), alpha))
        # add surface
        if (nrow(geoms$surface$meta)) {
            set(geoms$surface$meta, NULL, c("color", "alpha"), list(COLOR_MAP$surface_type["Undefined"], alpha))
            geoms$surface$meta[geoms$zone, on = c("zone_name" = "name"), `:=`(color = i.color, alpha = i.alpha)]

            if (nrow(geoms$subsurface$meta)) {
                set(geoms$subsurface$meta, NULL, c("color", "alpha"), list(COLOR_MAP$surface_type["Undefined"], alpha))
                geoms$subsurface$meta[geoms$surface$meta, on = c("building_surface_name" = "name"),
                    `:=`(color = i.color, alpha = i.alpha)]
            }
            if (nrow(geoms$shading$meta)) {
                set(geoms$shading$meta, NULL, c("color", "alpha"), list(COLOR_MAP$surface_type["Undefined"], alpha))
                geoms$shading$meta[geoms$surface$meta, on = c("base_surface_name" = "name"),
                    `:=`(color = i.color, alpha = i.alpha)]
            }
        }
    }

    geoms
}
# }}}

# COLOR_MAP {{{
# Reference: 'openstudio\openstudiocore\ruby\openstudio\sketchup_plugin\lib\interfaces\MaterialsInterface.rb'
COLOR_MAP <- list(
    surface_type = c(
        Undefined = "#FFFFFFFF",
        NormalMaterial = "#FFFFFFFF",
        NormalMaterial_Ext = "#FFFFFFFF",
        NormalMaterial_Int = "#FF0000FF",
        Floor = "#808080FF",
        Floor_Ext = "#808080FF",
        Floor_Int = "#BFBFBFFF",
        Wall = "#CCB266FF",
        Wall_Ext = "#CCB266FF",
        Wall_Int = "#EBE2C5FF",
        Roof = "#994C4CFF",
        Roof_Ext = "#994C4CFF",
        Roof_Int = "#CA9595FF",
        Ceiling = "#994C4CFF",
        Ceiling_Ext = "#994C4CFF",
        Ceiling_Int = "#CA9595FF",
        Window = "#66B2CC99",
        Window_Ext = "#66B2CC99",
        Window_Int = "#C0E2EB99",
        Door = "#99854CFF",
        Door_Ext = "#99854CFF",
        Door_Int = "#CABC95FF",
        GlassDoor = "#66B2CC99",
        GlassDoor_Ext = "#66B2CC99",
        GlassDoor_Int = "#C0E2EB99",
        GlazedDoor = "#66B2CC99",
        GlazedDoor_Ext = "#66B2CC99",
        GlazedDoor_Int = "#C0E2EB99",
        SiteShading = "#4B7C95FF",
        SiteShading_Ext = "#4B7C95FF",
        SiteShading_Int = "#BBD1DCFF",
        BuildingShading = "#714C99FF",
        BuildingShading_Ext = "#714C99FF",
        BuildingShading_Int = "#D8CBE5FF",
        ZoneShading = "#4C6EB2FF",
        ZoneShading_Ext = "#4C6EB2FF",
        ZoneShading_Int = "#B7C5E0FF",
        InteriorPartitionSurface = "#9EBC8FFF",
        InteriorPartitionSurface_Ext = "#9EBC8FFF",
        InteriorPartitionSurface_Int = "#D5E2CFFF"
    ),
    boundary = c(
        Adiabatic = "#FF65B2",
        Surface = "#009900",
        Zone = "#FF0000",
        Outdoors = "#A3CCCC",
        Outdoors_Sun = "#28CCCC",
        Outdoors_Wind = "#099FA2",
        Outdoors_Sunwind = "#4477A1",
        Foundation = "#A600AF",
        Ground = "#CCB77A",
        GroundFCfactorMethod = "#997A1E",
        OtherSideCoefficients = "#3F3F3F",
        OtherSideConditionsModel = "#99004C",
        GroundSlabPreprocessorAverage = "#FFBF00",
        GroundSlabPreprocessorCore = "#FFB632",
        GroundSlabPreprocessorPerimeter = "#FFB265",
        GroundBasementPreprocessorAverageWall = "#CC3300",
        GroundBasementPreprocessorAverageFloor = "#CC5128",
        GroundBasementPreprocessorUpperWall = "#CC7051",
        GroundBasementPreprocessorLowerWall = "#CCADA3"
    ),
    color = c(
        AliceBlue = "#F0F8FF",
        AntiqueWhite = "#FAEBD7",
        Aqua = "#00FFFF",
        Aquamarine = "#7FFFD4",
        Azure = "#F0FFFF",
        Beige = "#F5F5DC",
        Bisque = "#FFE4C4",
        Black = "#000000",
        BlanchedAlmond = "#FFEBCD",
        Blue = "#0000FF",
        BlueViolet = "#8A2BE2",
        Brown = "#A52A2A",
        BurlyWood = "#DEB887",
        CadetBlue = "#5F9EA0",
        Chartreuse = "#7FFF00",
        Chocolate = "#D2691E",
        Coral = "#FF7F50",
        CornflowerBlue = "#6495ED",
        Cornsilk = "#FFF8DC",
        Crimson = "#DC143C",
        Cyan = "#00FFFF",
        DarkBlue = "#00008B",
        DarkCyan = "#008B8B",
        DarkGoldenrod = "#B8860B",
        DarkGray = "#A9A9A9",
        DarkGreen = "#006400",
        DarkKhaki = "#BDB76B",
        DarkMagenta = "#8B008B",
        DarkOliveGreen = "#556B2F",
        DarkOrange = "#FF8C00",
        DarkOrchid = "#9932CC",
        DarkRed = "#8B0000",
        DarkSalmon = "#E9967A",
        DarkSeaGreen = "#8FBC8F",
        DarkSlateBlue = "#483D8B",
        DarkSlateGray = "#2F4F4F",
        DarkTurquoise = "#00CED1",
        DarkViolet = "#9400D3",
        DeepPink = "#FF1493",
        DeepSkyBlue = "#00BFFF",
        DimGray = "#696969",
        DodgerBlue = "#1E90FF",
        FireBrick = "#B22222",
        FloralWhite = "#FFFAF0",
        ForestGreen = "#228B22",
        Fuchsia = "#FF00FF",
        Gainsboro = "#DCDCDC",
        GhostWhite = "#F8F8FF",
        Gold = "#FFD700",
        Goldenrod = "#DAA520",
        Gray = "#808080",
        Green = "#008000",
        GreenYellow = "#ADFF2F",
        Honeydew = "#F0FFF0",
        HotPink = "#FF69B4",
        IndianRed = "#CD5C5C",
        Indigo = "#4B0082",
        Ivory = "#FFFFF0",
        Khaki = "#F0E68C",
        Lavender = "#E6E6FA",
        LavenderBlush = "#FFF0F5",
        LawnGreen = "#7CFC00",
        LemonChiffon = "#FFFACD",
        LightBlue = "#ADD8E6",
        LightCoral = "#F08080",
        LightCyan = "#E0FFFF",
        LightGoldenrodYellow = "#FAFAD2",
        LightGreen = "#90EE90",
        LightGrey = "#D3D3D3",
        LightPink = "#FFB6C1",
        LightSalmon = "#FFA07A",
        LightSeaGreen = "#20B2AA",
        LightSkyBlue = "#87CEFA",
        LightSlateGray = "#778899",
        LightSteelBlue = "#B0C4DE",
        LightYellow = "#FFFFE0",
        Lime = "#00FF00",
        LimeGreen = "#32CD32",
        Linen = "#FAF0E6",
        Magenta = "#FF00FF",
        Maroon = "#800000",
        MediumAquamarine = "#66CDAA",
        MediumBlue = "#0000CD",
        MediumOrchid = "#BA55D3",
        MediumPurple = "#9370DB",
        MediumSeaGreen = "#3CB371",
        MediumSlateBlue = "#7B68EE",
        MediumSpringGreen = "#00FA9A",
        MediumTurquoise = "#48D1CC",
        MediumVioletRed = "#C71585",
        MidnightBlue = "#191970",
        MintCream = "#F5FFFA",
        MistyRose = "#FFE4E1",
        Moccasin = "#FFE4B5",
        NavajoWhite = "#FFDEAD",
        Navy = "#000080",
        OldLace = "#FDF5E6",
        Olive = "#808000",
        OliveDrab = "#808000",
        Orange = "#FFA500",
        OrangeRed = "#FF4500",
        Orchid = "#DA70D6",
        PaleGoldenrod = "#EEE8AA",
        PaleGreen = "#98FB98",
        PaleTurquoise = "#AFEEEE",
        PaleVioletRed = "#DB7093",
        PapayaWhip = "#FFEFD5",
        PeachPuff = "#FFDAB9",
        Peru = "#CD853F",
        Pink = "#FFC0CB",
        Plum = "#DDA0DD",
        PowderBlue = "#B0E0E6",
        Purple = "#800080",
        Red = "#FF0000",
        RosyBrown = "#BC8F8F",
        RoyalBlue = "#4169E1",
        SaddleBrown = "#8B4513",
        Salmon = "#FA8072",
        SandyBrown = "#F4A460",
        SeaGreen = "#2E8B57",
        Seashell = "#FFF5EE",
        Sienna = "#A0522D",
        Silver = "#C0C0C0",
        SkyBlue = "#87CEEB",
        SlateBlue = "#6A5ACD",
        SlateGray = "#708090",
        Snow = "#FFFAFA",
        SpringGreen = "#00FF7F",
        SteelBlue = "#4682B4",
        Tan = "#D2B48C",
        Teal = "#D2B48C",
        Thistle = "#D8BFD8",
        Tomato = "#FF6347",
        Turquoise = "#40E0D0",
        Violet = "#EE82EE",
        Wheat = "#F5DEB3",
        WhiteSmoke = "#F5F5F5",
        Yellow = "#FFFF00",
        YellowGreen = "#9ACD32"
    )
)
# }}}
