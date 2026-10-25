# viewer__add_surface_hole_vertices {{{
# Insert subsurface rings as negative-index holes before ear-clipping parent surfaces.
viewer__add_surface_hole_vertices <- function(surface, subsurface, vertices) {
    if (!nrow(surface) || !nrow(subsurface)) {
        return(vertices)
    }

    subsurface[
        surface,
        on = c("building_surface_name" = "name"),
        `:=`(id_surface = i.id)
    ]
    on.exit(set(subsurface, NULL, "id_surface", NULL), add = TRUE)

    holes <- vertices[
        fast_subset(subsurface, c("id", "id_surface")),
        on = "id",
        nomatch = NULL
    ]
    if (!nrow(holes)) {
        return(vertices)
    }

    # Negative indices delimit holes for decido::earcut without changing object IDs.
    set(
        holes,
        NULL,
        c("id", "id_surface", "index"),
        list(holes$id_surface, NULL, -holes$index)
    )
    setorderv(rbindlist(list(vertices, holes)), "id")
}
# }}}

# viewer__triangulate_geoms {{{
# Triangulate arbitrary EnergyPlus polygons while retaining triangles and quads for cheap handling.
viewer__triangulate_geoms <- function(geoms) {
    if (!nrow(geoms$vertices)) {
        return(copy(geoms$vertices))
    }
    if (nrow(geoms$subsurface)) {
        geoms$vertices <- viewer__add_surface_hole_vertices(
            geoms$surface,
            geoms$subsurface,
            geoms$vertices
        )
    }
    num_vert <- geoms$vertices[, by = "id", list(num = .N)]
    if (!any(num_vert$num > 4L)) {
        return(copy(geoms$vertices))
    }

    rbindlist(list(
        copy(geoms$vertices[J(num_vert$id[num_vert$num <= 4L]), on = "id"]),
        viewer__triangulate_surfaces(geoms$vertices[
            J(num_vert$id[num_vert$num > 4L]),
            on = "id"
        ])
    ))
}
# }}}

# viewer__triangulate_surfaces {{{
# Project each face to 2D for ear clipping, then transform generated triangles back to 3D.
viewer__triangulate_surfaces <- function(vertices) {
    trans <- align_face(vertices)
    dt_trans <- rbindlist(lapply(trans$trans, as.list))
    dt_inv_trans <- rbindlist(lapply(trans$trans, function(x) {
        as.list(solve.default(x))
    }))

    set(dt_trans, NULL, "id", trans$id)
    set(dt_inv_trans, NULL, "id", trans$id)
    add_joined_cols(dt_inv_trans, vertices, "id", sprintf("V%i", 1:16))
    vertices[, `:=`(
        inv_x = x * V1 + y * V5 + z * V9 + V13,
        inv_y = x * V2 + y * V6 + z * V10 + V14,
        inv_z = x * V3 + y * V7 + z * V11 + V15
    )]

    vert <- vertices[, by = "id", {
        hole <- which(index == -1L)
        if (!length(hole)) {
            hole <- 0L
        }
        # Retain the mainline Earcut 3.x refinement for narrow or concave polygons.
        tri <- decido::earcut(list(inv_x, inv_y), hole, refine = TRUE)
        list(
            index = seq_along(tri),
            x = inv_x[tri],
            y = inv_y[tri],
            z = inv_z[tri]
        )
    }]
    set(
        vertices,
        NULL,
        setdiff(names(vertices), c("id", "index", "x", "y", "z")),
        NULL
    )

    add_joined_cols(dt_trans, vert, "id", sprintf("V%i", 1:16))
    vert[, `:=`(
        x = x * V1 + y * V5 + z * V9 + V13,
        y = x * V2 + y * V6 + z * V10 + V14,
        z = x * V3 + y * V7 + z * V11 + V15
    )]
    set(vert, NULL, setdiff(names(vert), c("id", "index", "x", "y", "z")), NULL)
    vert
}
# }}}

# vim: set fdm=marker:
