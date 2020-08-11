# IdfGeometry {{{
IdfGeometry <- R6Class("IdfGeometry", cloneable = FALSE,
    public = list(
        initialize = function (idf) {
            if (!is_idf(idf)) idf <- read_idf(idf)
            private$m_parent <- idf

            # extract geometry surfaces
            private$m_geometry <- extract_geometry(private$m_parent)
        },

        vertices = function ()
            geom_vertices(self, private),

        view = function (new = TRUE, clear = TRUE, render_by = "surface_type",
                         axis = TRUE, wireframe = TRUE, surface = TRUE, x_ray = FALSE,
                         line_width = 1.5, line_color = "black",
                         theta = 0, phi = -60, fov = 60, zoom = 1,
                         background = "white", size = c(0, 30, 800))
            geom_view(self, private, new = new, clear = clear, render_by = render_by,
                      axis = axis, wireframe = wireframe, surface = surface, x_ray = x_ray,
                      line_width = line_width, line_color = line_color,
                      theta = theta, phi = phi, fov = fov, zoom = zoom,
                      background = background, size = size),

        save_snapshot = function (filename, bring_to_front = TRUE, axis = FALSE)
            geom_save_snapshot(self, private, filename, bring_to_front, axis = axis)
    ),

    private = list(
        m_parent = NULL,
        m_geometry = NULL,
        m_log = NULL
    )
)
# }}}

# extract_geometry {{{
extract_geometry <- function (idf) {
    cls <- data.table(class = idf$class_name(by_group = TRUE)["Thermal Zones and Surfaces"][[1L]])
    set(cls, NULL, c("type", "subtype", "misc"),
        as.data.table(stri_split_fixed(cls$class, ":", n = 3L, simplify = TRUE))
    )

    cls_surface <- cls[type %in% c("BuildingSurface", "Wall", "RoofCeiling", "Floor", "Wall", "Roof", "Ceiling")]
    cls_subsurface <- cls[type %in% c("FenestrationSurface", "Window", "Door", "GlazedDoor")]
    cls_shading <- cls[type == "Shading"]

    if (nrow(cls_surface) + nrow(cls_subsurface) + nrow(cls_shading) == 0L) return(data.table())

    # get global geometry rules
    rules <- get_global_geom_rules(idf)

    # zone origins
    zone <- get_zone_origin(idf)

    # surfaces
    surf <- get_surface_vertices(idf, cls_surface, rules, zone)

    # subsurfaces
    win <- get_subsurface_vertices(idf, cls_subsurface, rules, surf)

    # shadings
    shading <- get_shading_vertices(idf, cls_shading, rules, zone, surf)

    # treat windows as holes
    if (!nrow(win)) {
        hole <- data.table()
    } else {
        hole <- copy(win)[!J(NA_character_), on = "parent_surface_name"]
        set(hole, NULL, "surface_type", "Hole")
        # use surface id for grouping
        hole[surf, on = c(parent_surface_name = "name"), id := i.id]
    }

    dt <- rbindlist(list(surf, hole, win, shading), fill = TRUE)

    # offset coordinates
    # this make sure every point has the same origin of (0, 0, 0)
    dt[J(NA_real_), on = "origin_x", origin_x := 0.0]
    dt[J(NA_real_), on = "origin_y", origin_y := 0.0]
    dt[J(NA_real_), on = "origin_z", origin_z := 0.0]
    dt[, `:=`(x = origin_x + x, y = origin_y + y, z = origin_z + z)]
    set(dt, NULL, c("origin_x", "origin_y", "origin_z"), NULL)

    # rotate if necessary
    if (idf$is_valid_class("Building")) {
        north <- idf$Building$North_Axis
        if (is.na(north)) {
            warn("North Axis unknown, using 0", "warn_unknown_north_axis")
            north <- 0
        }
    } else {
        north <- 0
        warn("Could not find 'Building' object, assuming 0 rotation",
            "warn_no_building"
        )
    }
    dt <- rotate_vertices(dt, north, c("x", "y", "z"))

    # add number of vert and vertex index
    dt[, `:=`(n_vert = .N, index_vertex = seq_len(.N)), by = "id"]

    dt
}
# }}}

# get_global_geom_rules {{{
get_global_geom_rules <- function (idf) {
    if (idf$is_valid_class("GlobalGeometryRules")) {
        rules <- idf$to_table(class = "GlobalGeometryRules", all = TRUE)
        rules <- setattr(as.list(rules$value), "names", rules$field)

        # get all possible values
        choices <- idf$definition("GlobalGeometryRules")$field_choice()

        for (i in seq_along(rules)) {
            if (is.na(rules[[i]])) {
                warn(sprintf("Empty '%s' found in 'GlobalGeometryRules'. Assuming '%s'.",
                    names(rules[i]), choices[[i]][[1L]]
                ), "warn_invalid_ggr")
                rules[[i]] <- stri_trans_tolower(choices[[i]][[1L]])
            } else if (!stri_trans_tolower(rules[[i]]) %chin% stri_trans_tolower(choices[[i]])) {
                warn(sprintf("Invalid '%s' found ('%s') in 'GlobalGeometryRules'. Assuming '%s'.",
                    names(rules[i]), rules[[i]], choices[[i]][[1L]]
                ), "warn_invalid_ggr")
                rules[[i]] <- stri_trans_tolower(choices[[i]][[1L]])
            } else {
                rules[[i]] <- stri_trans_tolower(rules[[i]])
            }
        }

        setattr(rules, "names", lower_name(names(rules)))
    } else {
        warn("No 'GlobalGeometryRules' object found in current IDF. Assuming all defaults.",
            "warn_no_global_geom_rules"
        )
        rules <- list(
            starting_vertex_position = "upperleftcorner",
            vertex_entry_direction = "counterclockwise",
            coordinate_system = "relative",
            daylighting_reference_point_coordinate_system = "relative",
            rectangular_surface_coordinate_system = "relative"
        )
    }

    rules
}
# }}}

# get_zone_origin {{{
get_zone_origin <- function (idf) {
    if (!idf$is_valid_class("Zone")) {
        zone <- data.table(name = character(), x = double(), y = double(), z = double(), name_lower = character())
    } else {
        zone <- idf$to_table(class = "Zone", wide = TRUE, all = TRUE, string_value = FALSE)[
            , .SD, .SDcols = c("name", paste(c("X", "Y", "Z"), "Origin"), "Direction of Relative North")]
        setnames(zone, c("name", "x", "y", "z", "dir_relative_north"))
        set(zone, NULL, "name_lower", stri_trans_tolower(zone$name))
        if (nrow(mis_origin <- na.omit(zone, by = c("x", "y", "z"), invert = TRUE))) {
            warn(paste0("Zone below has unknown origin. (0, 0, 0) will be used:\n",
                collapse(mis_origin$name)
            ), "warn_no_zone_origin")
            zone[J(NA_real_), on = "x", x := 0.0]
            zone[J(NA_real_), on = "y", y := 0.0]
            zone[J(NA_real_), on = "z", z := 0.0]
        }

        if (anyNA(zone$dir_relative_north)) {
            warn(paste0("Zone below has unknown direction of relative North. 0 will be used:\n",
                collapse(zone[is.na(dir_relative_north), name])
            ), "warn_no_zone_north")

            zone[J(NA_real_), on = "dir_relative_north", dir_relative_north := 0.0]
        }
    }
    zone
}
# }}}

# get_surface_vertices {{{
get_surface_vertices <- function (idf, cls, rules, zone) {
    surf <- data.table()

    # detailed surfaces
    if (nrow(detailed <- cls[J("Detailed"), on = "subtype", nomatch = NULL])) {
        surf_build <- list()
        surf_sep <- list()

        # first handle BuildingSurface:Detailed
        # it has different fields with other detailed surface classes
        if ("BuildingSurface" %in% detailed$type) {
            surf_build <- extract_surface_detailed(idf, detailed[J("BuildingSurface"), on = "type"])
            detailed <- detailed[!J("BuildingSurface"), on = "type"]
        }

        # then other detailed surface classes
        if (nrow(detailed)) {
            surf_sep <- extract_surface_detailed(idf, detailed$class)
        }

        # combine
        surf <- rbindlist(list(surf_build, surf_sep))

        # a surface will be an adiabatic one if the outside boundary condition
        # object is itself
        surf[stri_trans_tolower(name) == stri_trans_tolower(outside_boundary_condition_object),
            `:=`(outside_boundary_condition = "Adiabatic", outside_boundary_condition_object = NA_character_)]

        # convert to counter clockwise vertex entry direction
        if (rules$vertex_entry_direction != "counterclockwise") {
            surf[, by = "id", `:=`(x = rev(x), y = rev(y), z = rev(z))]
        }

        # add zone origin
        surf <- add_zone_origin(surf, zone, rules$coordinate_system)
    }

    # other simplified surface specs
    simple <- cls[!J("Detailed"), on = "subtype"]

    if (!nrow(simple)) return(surf)

    surf_simple <- extract_surface_simple(idf, simple)
    # add zone origin
    surf_simple <- add_zone_origin(surf_simple, zone, rules$rectangular_surface_coordinate_system)

    # combine all
    rbindlist(list(surf, surf_simple))
}
# }}}

# get_subsurface_vertices {{{
get_subsurface_vertices <- function (idf, cls, rules, surf) {
    subsurf <- data.table()

    # detailed surfaces
    if (nrow(detailed <- cls[J("Detailed"), on = "subtype", nomatch = NULL])) {
        subsurf <- extract_subsurface_detailed(idf, detailed)

        cls <- cls[!J("FenestrationSurface"), on = "type"]

        # convert to counter clockwise vertex entry direction
        if (rules$vertex_entry_direction != "counterclockwise") {
            subsurf[, by = "id", `:=`(x = rev(x), y = rev(y), z = rev(z))]
        }

        # add parent surface
        subsurf <- add_parent_surface(subsurf, surf, rules$coordinate_system)
    }

    # other simplified surface specs
    simple <- cls[!J("Detailed"), on = "subtype"]

    if (!nrow(simple)) return(subsurf)

    subsurf_simple <- extract_subsurface_simple(idf, simple)
    # add parent surface
    subsurf_simple <- add_parent_surface(subsurf_simple, surf, rules$rectangular_surface_coordinate_system)

    # combine all
    rbindlist(list(subsurf, subsurf_simple))
}
# }}}

# get_shading_vertices {{{
get_shading_vertices <- function (idf, cls, rules, zone, surf) {
    shade <- data.table()

    if (nrow(detailed <- cls[J("Detailed"), on = "misc", nomatch = NULL])) {
        shade <- extract_shading_detailed(idf, detailed)

        # convert to counter clockwise vertex entry direction
        if (rules$vertex_entry_direction != "counterclockwise") {
            shade[, by = "id", `:=`(x = rev(x), y = rev(y), z = rev(z))]
        }

        # add zone origin
        shade <- add_zone_origin(shade, zone, rules$coordinate_system)
        # add parent surface
        shade <- add_parent_surface(shade, surf, rules$coordinate_system)
    }

    # other simplified surface specs
    simple <- cls[!J("Detailed"), on = "subtype"]

    if (!nrow(simple)) return(shade)

    shade_simple <- extract_shading_simple(idf, simple)
    # add parent surface
    shade_simple <- add_parent_surface(shade_simple, surf, rules$rectangular_surface_coordinate_system)

    # combine all
    rbindlist(list(shade, shade_simple))
}
# }}}

# extract_surface_detailed {{{
extract_surface_detailed <- function (idf, class) {
    dt <- idf$to_table(class = class$class, wide = TRUE, string_value = FALSE,
        align = TRUE, group_ext = "index")

    set(dt, NULL, "Name", NULL)

    # get standard surface type
    cls <- class[!J("Detailed"), on = "subtype"]
    `Surface Type` <- NULL
    dt[cls, on = "class", `Surface Type` := i.type]

    # rename coordinate columns
    setnames(dt, paste0("Vertex ", c("X", "Y", "Z"), "-coordinate"), c("x", "y", "z"))
    setnames(dt, stri_trans_tolower(stri_replace_all_fixed(names(dt), " ", "_")))

    # unlist
    na.omit(dt[, {
        l <- viapply(x, length)
        list(id = rep(id, l),
             name = rep(name, l),
             class = rep(class, l),
             surface_type = rep(surface_type, l),
             construction_name = rep(construction_name, l),
             zone_name = rep(zone_name, l),
             outside_boundary_condition = rep(outside_boundary_condition, l),
             outside_boundary_condition_object = rep(outside_boundary_condition_object, l),
             sun_exposure = rep(sun_exposure, l),
             wind_exposure = rep(wind_exposure, l),
             x = unlist(x, TRUE, FALSE),
             y = unlist(y, TRUE, FALSE),
             z = unlist(z, TRUE, FALSE)
        )
    }], cols = c("x", "y", "z"))
}
# }}}

# extract_surface_simple {{{
extract_surface_simple <- function (idf, class) {
    dt <- idf$to_table(class = class$class, wide = TRUE, string_value = FALSE, all = TRUE, force = TRUE)

    set(dt, NULL, "Name", NULL)

    # get standard surface type
    cls <- class
    `Surface Type` <- NULL
    dt[cls, on = "class", `Surface Type` := i.type]

    # handle both wall and roof
    if ("Height" %in% names(dt)) {
        setnames(dt, "Height", "height_width")
        if ("Width" %in% names(dt)) {
            Width <- NULL
            dt[J(NA_real_), on = "height_width", height_width := Width]
            set(dt, NULL, "Width", NULL)
        }
    } else if ("Width" %in% names(dt)) {
        setnames(dt, "Width", "height_width")
    }

    # rename columns
    setnames(dt, stri_trans_tolower(stri_replace_all_fixed(names(dt), " ", "_")))

    # calculate detailed vertices
    vert <- dt[, {
        vert <- get_vertices_from_specs(azimuth_angle, tilt_angle,
            starting_x_coordinate, starting_y_coordinate, starting_z_coordinate,
            length, height_width)
        list(id = rep(id, 4L), x = unlist(vert$x), y = unlist(vert$y), z = unlist(vert$z))
    }]

    set(dt, NULL, c("azimuth_angle", "tilt_angle", "starting_x_coordinate",
            "starting_y_coordinate", "starting_z_coordinate", "length", "height_width"), NULL)
    dt <- vert[dt, on = "id"]

    # make sure it has the same columns as detailed surfaces
    if (!"outside_boundary_condition_object" %in% names(dt)) {
        set(dt, NULL, "outside_boundary_condition_object", NA_character_)
    }
    set(dt, NULL, c("outside_boundary_condition", "sun_exposure", "wind_exposure"), NA_character_)

    # change column orders
    setcolorder(dt, c("id", "name", "class", "surface_type", "construction_name",
            "zone_name", "outside_boundary_condition", "outside_boundary_condition_object",
            "sun_exposure", "wind_exposure", "x", "y", "z")
    )

    dt
}
# }}}

# extract_subsurface_detailed {{{
extract_subsurface_detailed <- function (idf, class) {
    dt <- idf$to_table(class = class$class, wide = TRUE, string_value = FALSE, all = TRUE)

    set(dt, NULL, "Name", NULL)

    # get standard surface type
    cls <- class[!J("Detailed"), on = "subtype"]
    `Surface Type` <- NULL
    dt[cls, on = "class", `Surface Type` := i.type]

    # rename columns
    setnames(dt, stri_trans_tolower(stri_replace_all_fixed(names(dt), " ", "_")))
    setnames(dt, "building_surface_name", "parent_surface_name")

    # melt table to put coordinate together
    dt <- melt.data.table(dt, names(dt)[1L:7L], patterns("x-coordinate", "y-coordinate", "z-coordinate"),
        value.name = c("x", "y", "z")
    )
    set(dt, NULL, "variable", NULL)

    # retain the original order
    setorderv(dt, "id")

    # make sure it has all necessary columns
    set(dt, NULL, c("zone_name", "outside_boundary_condition", "sun_exposure", "wind_exposure"), NA_character_)
    setcolorder(dt, c(
        "id", "name", "class", "surface_type", "construction_name",
        "parent_surface_name", "zone_name", "outside_boundary_condition",
        "outside_boundary_condition_object", "sun_exposure", "wind_exposure",
        "x", "y", "z"
    ))

    na.omit(dt, cols = c("x", "y", "z"))
}
# }}}

# extract_subsurface_simple {{{
extract_subsurface_simple <- function (idf, class) {
    return(data.table())
    dt <- idf$to_table(class = class$class, wide = TRUE, string_value = FALSE, all = TRUE, force = TRUE)

    set(dt, NULL, "Name", NULL)

    # get standard surface type
    cls <- class
    `Surface Type` <- NULL
    dt[cls, on = "class", `Surface Type` := i.type]

    # rename columns
    setnames(dt, stri_trans_tolower(stri_replace_all_fixed(names(dt), " ", "_")))

    # clean
    if ("frame_and_divider_name" %in% names(dt)) set(dt, NULL, "frame_and_divider_name", NULL)
    if ("multiplier" %in% names(dt)) set(dt, NULL, "multiplier", NULL)

    browser()
    # calculate detailed vertices
    vert <- dt[, {
        x <- list(
            starting_x_coordinate,
            starting_x_coordinate,
            starting_x_coordinate + length,
            starting_x_coordinate + length
        )

        y <- list(
            starting_z_coordinate + height,
            starting_z_coordinate,
            starting_z_coordinate,
            starting_z_coordinate + height
        )

        vert <- get_vertices_from_specs(azimuth_angle, tilt_angle,
            starting_x_coordinate, starting_y_coordinate, starting_z_coordinate,
            length, height_width)
        list(id = rep(id, 4L), x = unlist(vert$x), y = unlist(vert$y), z = unlist(vert$z))
    }]
}
# }}}

# extract_shading_detailed {{{
extract_shading_detailed <- function (idf, class) {
    dt <- idf$to_table(class = class$class, wide = TRUE, string_value = FALSE,
        align = TRUE, force = TRUE, group_ext = "index")

    set(dt, NULL, c("Name", "Transmittance Schedule Name", "Number of Vertices"), NULL)

    # get standard surface type
    cls <- class
    `Surface Type` <- NULL
    dt[cls, on = "class", `Surface Type` := paste0(i.subtype, "Shading")]

    # rename columns
    setnames(dt, paste0("Vertex ", c("X", "Y", "Z"), "-coordinate"), c("x", "y", "z"))
    setnames(dt, stri_trans_tolower(stri_replace_all_fixed(names(dt), " ", "_")))
    if ("base_surface_name" %in% names(dt)) {
        setnames(dt, "base_surface_name", "parent_surface_name")
    } else {
        set(dt, NULL, "parent_surface_name", NA_character_)
    }

    na.omit(dt[, {
        l <- viapply(x, length)
        list(id = rep(id, l),
             name = rep(name, l),
             class = rep(class, l),
             surface_type = rep(surface_type, l),
             construction_name = NA_character_,
             parent_surface_name = rep(parent_surface_name, l),
             zone_name = NA_character_,
             outside_boundary_condition = NA_character_,
             outside_boundary_condition_object = NA_character_,
             sun_exposure = NA_character_,
             wind_exposure = NA_character_,
             x = unlist(x, TRUE, FALSE),
             y = unlist(y, TRUE, FALSE),
             z = unlist(z, TRUE, FALSE)
        )
    }], cols = c("x", "y", "z"))
}
# }}}

# extract_shading_simple {{{
extract_shading_simple <- function (idf, class) {
    return(data.table())
    # for overhang
    if ("Overhang" %in% class$subtype) {
        dt_overhang <- idf$to_table(class = class[J("Overhang"), on = "subtype", class],
            wide = TRUE, string_value = FALSE, all = TRUE, force = TRUE)

        if ("Depth as Fraction of Window/Door Height" %in% names(dt_overhang)) {
            if (!"Depth" %in% names(dt_overhang)) set(dt_overhang, NULL, "Depth", NA_real_)
            Depth <- `Depth as Fraction of Window/Door Height` <- NULL
            dt_overhang[J(NA_real_), on = "Depth", Depth := `Depth as Fraction of Window/Door Height`]
            set(dt_overhang, NULL, "Depth as Fraction of Window/Door Height", NULL)
        }
    }

    # for fin
    if ("Fin" %in% class$subtype) {
        dt_fin <- idf$to_table(class = class[J("Fin"), on = "subtype", class],
            wide = TRUE, string_value = FALSE, all = TRUE, force = TRUE)

        if ("Left Depth as Fraction of Window/Door Width" %in% names(dt_fin)) {
            if (!"Left Depth" %in% names(dt_fin)) set(dt_fin, NULL, "Left Depth", NA_real_)
            `Left Depth` <- `Left Depth as Fraction of Window/Door Width` <- NULL
            dt_fin[J(NA_real_), on = "Left Depth", `Left Depth` := `Left Depth as Fraction of Window/Door Width`]
            set(dt_fin, NULL, "Left Depth as Fraction of Window/Door Width", NULL)
        }

        if ("Right Depth as Fraction of Window/Door Width" %in% names(dt_fin)) {
            if (!"Right Depth" %in% names(dt_fin)) set(dt_fin, NULL, "Right Depth", NA_real_)
            `Right Depth` <- `Right Depth as Fraction of Window/Door Width` <- NULL
            dt_fin[J(NA_real_), on = "Right Depth", `Right Depth` := `Right Depth as Fraction of Window/Door Width`]
            set(dt_fin, NULL, "Right Depth as Fraction of Window/Door Width", NULL)
        }
    }

    dt <- idf$to_table(class = class$class, wide = TRUE, string_value = FALSE, all = TRUE, force = TRUE)

    set(dt, NULL, "Name", NULL)

    # get standard surface type
    cls <- class
    `Surface Type` <- NULL
    dt[cls, on = "class", `Surface Type` := i.type]

    # rename columns
    setnames(dt, stri_trans_tolower(stri_replace_all_fixed(names(dt), " ", "_")))

    # clean
    if ("frame_and_divider_name" %in% names(dt)) set(dt, NULL, "frame_and_divider_name", NULL)
    if ("multiplier" %in% names(dt)) set(dt, NULL, "multiplier", NULL)

    browser()
    # calculate detailed vertices
    vert <- dt[, {
        x <- list(
            starting_x_coordinate,
            starting_x_coordinate,
            starting_x_coordinate + length,
            starting_x_coordinate + length
        )

        y <- list(
            starting_z_coordinate + height,
            starting_z_coordinate,
            starting_z_coordinate,
            starting_z_coordinate + height
        )

        vert <- get_vertices_from_specs(azimuth_angle, tilt_angle,
            starting_x_coordinate, starting_y_coordinate, starting_z_coordinate,
            length, height_width)
        list(id = rep(id, 4L), x = unlist(vert$x), y = unlist(vert$y), z = unlist(vert$z))
    }]
}
# }}}

# add_zone_origin {{{
add_zone_origin <- function (dt, zone, coord_sys) {
    if (!nrow(dt)) return(dt)

    # update zone name
    set(dt, NULL, "zone_lower", stri_trans_tolower(dt$zone_name))

    if (coord_sys != "relative") {
        # add origins
        dt[zone, on = c("zone_lower" = "name_lower"),
            `:=`(zone_name = i.name, origin_x = 0.0, origin_y = 0.0, origin_z = 0.0)]
    } else {
        # add origins
        dt[zone, on = c("zone_lower" = "name_lower"),
            `:=`(zone_name = i.name, origin_x = i.x, origin_y = i.y, origin_z = i.z)]
    }

    set(dt, NULL, "zone_lower", NULL)

    dt
}
# }}}

# add_parent_surface {{{
add_parent_surface <- function (dt, surf, coord_sys) {
    if (!nrow(dt)) return(dt)

    if (!nrow(surf)) {
        # this means that all subsurface are not valid
        set(dt, NULL, "parent_surface_name", NA_character_)
        set(dt, NULL, c("origin_x", "origin_y", "origin_z"), 0.0)
    } else {
        set(surf, NULL, "surface_lower", stri_trans_tolower(surf$name))
        set(dt, NULL, "surface_lower", stri_trans_tolower(dt$parent_surface_name))

        dt[surf, on = "surface_lower",
            `:=`(parent_surface_name = i.name, zone_name = i.zone_name,
                 outside_boundary_condition = i.outside_boundary_condition,
                 origin_x = i.origin_x, origin_y = i.origin_y, origin_z = i.origin_z)]

        if (coord_sys != "relative") {
            set(dt, NULL, c("origin_x", "origin_y", "origin_z"), 0.0)
        }

        set(surf, NULL, "surface_lower", NULL)
        set(dt, NULL, "surface_lower", NULL)
    }

    dt
}
# }}}

# get_vertices_from_specs {{{
get_vertices_from_specs <- function (azimuth, tilt, x0, y0, z0, length, height_width) {
    cos_azimuth <- cos(deg_to_rad(azimuth))
    sin_azimuth <- sin(deg_to_rad(azimuth))
    cos_tilt <- cos(deg_to_rad(tilt))
    sin_tilt <- sin(deg_to_rad(tilt))

    x_init <- list(0.0, 0.0, length, length)
    y_init <- list(height_width, 0.0, 0.0, height_width)

    x <- mapply(
        function (x, y) x0 - cos_azimuth * x - cos_tilt * sin_azimuth * y,
        x = x_init, y = y_init, SIMPLIFY = FALSE
    )

    y <- mapply(
        function (x, y) y0 - sin_azimuth * x - cos_tilt * cos_azimuth * y,
        x = x_init, y = y_init, SIMPLIFY = FALSE
    )

    z <- lapply(y_init, function (y) z0 - sin_tilt * y)

    list(x = x, y = y, z = z)
}
# }}}

# rotate_vertices {{{
rotate_vertices <- function (dt, degree, vertices) {
    browser()
    if (is.na(degree)) return(dt)

    # rotate if necessary
    coord <- rgl::rotate3d(as.matrix(dt[, .SD, .SDcols = vertices]),
        deg_to_rad(degree), 0, 0, 1)

    set(dt, NULL, c("x", "y", "z"), as.data.table(coord))

    dt
}
# }}}

# get_outward_normal {{{
get_outward_normal <- function (dt) {
    # calculate normal vector of surfaces using Newell Method
    # Reference: https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal#Newell.27s_Method
    dt[, by = "id", {
        nx <- seq_len(.N) %% .N + 1L
        # calculate the distance from the origin to the first point on each polygon
        norm_x <- sum((z + z[nx]) * (y - y[nx]))
        norm_y <- sum((x + x[nx]) * (z - z[nx]))
        norm_z <- sum((y + y[nx]) * (x - x[nx]))
        dis <- x[1L] * norm_x + y[1L] * norm_y + z[1L] * norm_z
        list(x = norm_x, y = norm_y, z = norm_z, distance = dis)
    }]
}
# }}}

# triangulate_surfaces {{{
triangulate_surfaces <- function (dt) {
    # calculate normal vector of surfaces using Newell Method
    # Reference: https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal#Newell.27s_Method
    norm <- dt[!J("hole"), on = "surface_type", by = "id", {
        nx <- seq_len(.N) %% .N + 1L
        # calculate the distance from the origin to the first point on each polygon
        norm_x <- sum((z + z[nx]) * (y - y[nx]))
        norm_y <- sum((x + x[nx]) * (z - z[nx]))
        norm_z <- sum((y + y[nx]) * (x - x[nx]))
        dis <- x[1L] * norm_x + y[1L] * norm_y + z[1L] * norm_z
        list(x = norm_x, y = norm_y, z = norm_z, distance = dis)
    }]

    # get projection axis
    norm[, by = "id", projection_axis := which.max(abs(unlist(.SD))), .SDcols = c("x", "y", "z")]
    dt[norm, on = "id", projection_axis := i.projection_axis]

    # get projected vertices for triangulation
    dt[, c("projected_x", "projected_y") := {
        v <- list(x, y, z)[-projection_axis]
        list(projected_x = v[[1L]], projected_y = v[[2L]])
    }, by = "id"]

    # triangulation using earcut algorithm
    if ("color_int" %in% names(dt)) {
        dt[, by = "id", {
            # get vertex index of the start of a hole
            hole <- index_vertex[c(0L, diff(rleid(name))) > 0]
            if (!length(hole)) hole <- 0L
            tri <- decido::earcut(list(projected_x, projected_y), hole)
            list(x = x[tri], y = y[tri], z = z[tri],
                 color = rep(color[1L], length(tri)),
                 color_int = rep(color_int[1L], length(tri)),
                 alpha = rep(alpha[1L], length(tri))
            )
        }]
    } else {
        dt[, by = "id", {
            # get vertex index of the start of a hole
            hole <- index_vertex[c(0L, diff(rleid(name))) > 0]
            if (!length(hole)) hole <- 0L
            tri <- decido::earcut(list(projected_x, projected_y), hole)
            list(x = x[tri], y = y[tri], z = z[tri],
                 color = rep(color[1L], length(tri)),
                 alpha = rep(alpha[1L], length(tri))
            )
        }]
    }
}
# }}}

# pair_line_vertex {{{
pair_line_vertex <- function (dt) {
    # for lines, vertices should be provided in pairs
    # only need to plot surfaces here, since windows and holes are the same
    # things here
    dt[!J("Hole"), on = "surface_type", by = "id", {
        idx <- c(sort(c(index_vertex, index_vertex[-1L])), 1L)
        list(x = x[idx], y = y[idx], z = z[idx])
    }]
}
# }}}

# geom_vertices {{{
geom_vertices <- function (self, private) {
    data.table::copy(private$m_geometry)
}
# }}}

# geom_view {{{
geom_view <- function (self, private, new = TRUE, clear = TRUE, axis = TRUE,
                       render_by = "surface_type", wireframe = TRUE, surface = TRUE,
                       x_ray = FALSE, line_width = 1.5, line_color = "black",
                       theta = 0, phi = -60, fov = 60, zoom = 1, background = "white",
                       size = c(0, 30, 800)) {
    if (!requireNamespace("rgl", quietly = TRUE)) {
        abort(paste0(
            "'eplusr' relies on the 'rgl' package to view 3D IDF geometry; ",
            "please add this to your library with install.packages('rgl') and try agian."
        ))
    }
    if (!requireNamespace("decido", quietly = TRUE)) {
        abort(paste0(
            "'eplusr' relies on the 'decido' package to view 3D IDF geometry; ",
            "please add this to your library with install.packages('decido') and try agian."
        ))
    }

    # remove logged rgl ids
    private$m_log$id <- NULL
    private$m_log$view <- NULL

    # copy the original data
    dt <- data.table::copy(private$m_geometry)

    if (!nrow(dt)) {
        message("Current IDF does not contain any supported surfaces.")
        return(invisible())
    }

    # map color
    dt <- map_color(dt, type = render_by, x_ray = x_ray)

    # initial rgl window
    private$m_log$id$device <- rgl_init(new = new, clear = clear,
        theta = theta, phi = phi, fov = fov,
        zoom = zoom, background = background, size = size
    )

    # Add x, y, and z Axes
    if (axis) geom_view_add_axis(self, private)

    if (surface) {
        tri <- triangulate_surfaces(dt)

        if (render_by != "surface_type") {
            private$m_log$id$surface <- rgl::triangles3d(
                x = as.matrix(tri[, .SD, .SDcols = c("x", "y", "z")]),
                color = as.matrix(tri[, .SD, .SDcols = rep("color", 3L)]),
                alpha = as.matrix(tri[, .SD, .SDcols = rep("alpha", 3L)])
            )
        } else {
            private$m_log$id$surface$front <- rgl::triangles3d(
                x = as.matrix(tri[, .SD, .SDcols = c("x", "y", "z")]),
                color = as.matrix(tri[, .SD, .SDcols = rep("color", 3L)]),
                alpha = as.matrix(tri[, .SD, .SDcols = rep("alpha", 3L)]),
                front = "fill", back = "culled"
            )
            private$m_log$id$surface$back <- rgl::triangles3d(
                x = as.matrix(tri[, .SD, .SDcols = c("x", "y", "z")]),
                color = as.matrix(tri[, .SD, .SDcols = rep("color_int", 3L)]),
                alpha = as.matrix(tri[, .SD, .SDcols = rep("alpha", 3L)]),
                front = "culled", back = "fill"
            )
        }
    }

    if (wireframe) {
        l <- pair_line_vertex(dt)
        private$m_log$id$wireframe <- rgl::rgl.lines(l$x, l$y, l$z,
            color = line_color, lwd = line_width, lit = FALSE
        )

        private$m_log$view$line_color <- line_color
        private$m_log$view$line_width <- line_width
    }

    invisible(self)
}
# }}}

# geom_view_add_axis {{{
geom_view_add_axis <- function (self, private) {
    dt <- private$m_geometry
    private$m_log$id$x <- rgl::rgl.lines(c(0, max(dt$x)* 1.05), c(0, 0), c(0, 0), color = "red", lit = FALSE)
    private$m_log$id$y <- rgl::rgl.lines(c(0, 0), c(0, max(dt$y) * 1.05), c(0, 0), color = "green", lit = FALSE)
    private$m_log$id$z <- rgl::rgl.lines(c(0, 0), c(0, 0), c(0, max(dt$z) * 1.05), color = "blue", lit = FALSE)
}
# }}}

# geom_view_add_ground {{{
geom_view_add_ground <- function (self, private, ground = "#CCCCC9") {
    dt <- private$m_geometry
    rx <- range(dt$x)
    ry <- range(dt$y)
    private$m_log$id$ground <- rgl::rgl.quads(
        c(rx[1], rx[2], rx[2], rx[1]), c(ry[1], ry[1], ry[2], ry[2]), 0,
        color = ground, lit = FALSE
    )
}
# }}}

# geom_save_snapshot {{{
geom_save_snapshot <- function (self, private, filename, bring_to_front = TRUE, axis = FALSE) {
    if (!requireNamespace("rgl", quietly = TRUE)) {
        abort(paste0("'eplusr' relies on the 'rgl' package to view 3D IDF geometry; ",
            "please add this to your library with install.packages('rgl') and try agian."
        ))
    }

    if (is.null(private$m_log$id$device)) {
        abort("No rgl window currently open. Please run '$view()' first.")
    }

    # set the last plot device as active
    rgl::rgl.set(private$m_log$id$device)

    redraw_axis <- FALSE
    # remove axis first
    if (!axis && !is.null(private$m_log$id$x)) {
        rgl::rgl.pop(id = c(private$m_log$id$x, private$m_log$id$y, private$m_log$id$z))
        redraw_axis <- TRUE
    }

    if (has_ext(filename, "png")) {
        rgl::rgl.snapshot(filename, "png", top = bring_to_front)
    } else if (has_ext(filename, c("ps", "eps", "tex", "pdf", "svg", "pgf"))) {
        if (bring_to_front) rgl::rgl.bringtotop()
        rgl::rgl.postscript(filename, tools::file_ext(filename))
    } else {
        abort(paste0("Not supported export format ", surround(tools::file_ext(filename)), ". ",
            "Current supported: ", collapse(c("png", "ps", "eps", "tex", "pdf", "svg", "pgf"))
        ))
    }

    # redraw axis
    if (redraw_axis) geom_view_add_axis(self, private)

    invisible()
}
# }}}

# deg_to_rad {{{
deg_to_rad <- function (x) x / 180 * pi
# }}}

# rad_to_deg {{{
rad_to_deg <- function (x) x / pi * 180
# }}}

# rgl_init {{{
#' @importFrom checkmate assert_flag assert_numeric
rgl_init <- function (new = FALSE, clear = TRUE, theta = 0, phi = -60, fov = 60,
                      zoom = 1, background = "white", size = c(0, 30, 800)) {
    checkmate::assert_flag(new)
    checkmate::assert_flag(clear)
    checkmate::assert_numeric(size, max.len = 4L)

    if (clear) {
        if (rgl::rgl.cur() == 0) new <- TRUE else rgl::rgl.clear()
    }

    if (new) {
        rgl::rgl.open()

        # set window size and position
        if (length(size) == 1L) {
            size = c(0, 0, size, size)
        } else if (length(size) == 2L) {
            size = c(0, 0, size)
        } else if (length(size) == 3L) {
            size = c(size[1:2], size[1:2] + size[3L])
        } else if (length(size) == 4L) {
            size = c(size[1:2], size[1:2] + size[3:4])
        }

        rgl::par3d(windowRect = size)

        # set viewpoint
        rgl::rgl.viewpoint(theta, phi, fov, zoom)

        # change mouse control method
        cur <- rgl::par3d("mouseMode")
        cur[["left"]] <- "trackball"
        cur[["wheel"]] <- "push"
        cur[["middle"]] <- "fov"
        rgl::par3d("mouseMode" = cur)
        pan3d(2L)
    }

    rgl::rgl.bg(color = background)

    rgl::rgl.cur()
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
            init <- rgl::par3d(c("userProjection","viewport"), dev = dev, subscene = sub)
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

# COLOR_MAP {{{
# Reference: 'openstudio\openstudiocore\ruby\openstudio\sketchup_plugin\lib\interfaces\MaterialsInterface.rb'
#' @importFrom grDevices rgb
COLOR_MAP <- list(
    surface_type = c(
        Undefined = rgb(255/255, 255/255, 255/255, 1),
        NormalMaterial = rgb(255/255, 255/255, 255/255, 1),
        NormalMaterial_Ext = rgb(255/255, 255/255, 255/255, 1),
        NormalMaterial_Int = rgb(255/255, 0/255, 0/255, 1),
        Floor = rgb(128/255, 128/255, 128/255, 1),
        Floor_Ext = rgb(128/255, 128/255, 128/255, 1),
        Floor_Int = rgb(191/255, 191/255, 191/255, 1),
        Wall = rgb(204/255, 178/255, 102/255, 1),
        Wall_Ext = rgb(204/255, 178/255, 102/255, 1),
        Wall_Int = rgb(235/255, 226/255, 197/255, 1),
        Roof = rgb(153/255, 76/255, 76/255, 1),
        Roof_Ext = rgb(153/255, 76/255, 76/255, 1),
        Roof_Int = rgb(202/255, 149/255, 149/255, 1),
        Ceiling = rgb(153/255, 76/255, 76/255, 1),
        Ceiling_Ext = rgb(153/255, 76/255, 76/255, 1),
        Ceiling_Int = rgb(202/255, 149/255, 149/255, 1),
        Window = rgb(102/255, 178/255, 204/255, 0.6),
        Window_Ext = rgb(102/255, 178/255, 204/255, 0.6),
        Window_Int = rgb(192/255, 226/255, 235/255, 0.6),
        Door = rgb(153/255, 133/255, 76/255, 1),
        Door_Ext = rgb(153/255, 133/255, 76/255, 1),
        Door_Int = rgb(202/255, 188/255, 149/255, 1),
        Glassdoor = rgb(153/255, 133/255, 76/255, 1),
        Glassdoor_Ext = rgb(153/255, 133/255, 76/255, 1),
        Glassdoor_Int = rgb(202/255, 188/255, 149/255, 1),
        SiteShading = rgb(75/255, 124/255, 149/255, 1),
        SiteShading_Ext = rgb(75/255, 124/255, 149/255, 1),
        SiteShading_Int = rgb(187/255, 209/255, 220/255, 1),
        BuildingShading = rgb(113/255, 76/255, 153/255, 1),
        BuildingShading_Ext = rgb(113/255, 76/255, 153/255, 1),
        BuildingShading_Int = rgb(216/255, 203/255, 229/255, 1),
        ZoneShading = rgb(76/255, 110/255, 178/255, 1),
        ZoneShading_Ext = rgb(76/255, 110/255, 178/255, 1),
        ZoneShading_Int = rgb(183/255, 197/255, 224/255, 1),
        InteriorPartitionSurface = rgb(158/255, 188/255, 143/255, 1),
        InteriorPartitionSurface_Ext = rgb(158/255, 188/255, 143/255, 1),
        InteriorPartitionSurface_Int = rgb(213/255, 226/255, 207/255, 1)
    ),
    boundary = c(
        Surface = rgb(0/255, 153/255, 0/255),
        Adiabatic = rgb(255/255, 101/255, 178/255),
        Zone = rgb(255/255, 0/255, 0/255),
        Outdoors = rgb(163/255, 204/255, 204/255),
        Outdoors_Sun = rgb(40/255, 204/255, 204/255),
        Outdoors_Wind = rgb(9/255, 159/255, 162/255),
        Outdoors_Sunwind = rgb(68/255, 119/255, 161/255),
        Ground = rgb(204/255, 183/255, 122/255),
        GroundFCFactorMethod = rgb(153/255, 122/255, 30/255),
        GroundSlabPreprocessorAverage = rgb(255/255, 191/255, 0/255),
        GroundSlabPreprocessorCore = rgb(255/255, 182/255, 50/255),
        GroundSlabPreprocessorPerimeter = rgb(255/255, 178/255, 101/255),
        GroundBasementPreprocessorAverageWall = rgb(204/255, 51/255, 0/255),
        GroundBasementPreprocessorAverageFloor = rgb(204/255, 81/255, 40/255),
        GroundBasementPreprocessorUpperWall = rgb(204/255, 112/255, 81/255),
        GroundBasementPreprocessorLowerWall = rgb(204/255, 173/255, 163/255),
        OtherSideCoefficients = rgb(63/255, 63/255, 63/255),
        OtherSideConditionsModel = rgb(153/255, 0/255, 76/255)
    ),
    color = c(
        AliceBlue = rgb(240/255, 248/255, 255/255),
        AntiqueWhite = rgb(250/255, 235/255, 215/255),
        Aqua = rgb(0/255, 255/255, 255/255),
        Aquamarine = rgb(127/255, 255/255, 212/255),
        Azure = rgb(240/255, 255/255, 255/255),
        Beige = rgb(245/255, 245/255, 220/255),
        Bisque = rgb(255/255, 228/255, 196/255),
        Black = rgb(0/255, 0/255, 0/255),
        BlanchedAlmond = rgb(255/255, 235/255, 205/255),
        Blue = rgb(0/255, 0/255, 255/255),
        BlueViolet = rgb(138/255, 43/255, 226/255),
        Brown = rgb(165/255, 42/255, 42/255),
        BurlyWood = rgb(222/255, 184/255, 135/255),
        CadetBlue = rgb(95/255, 158/255, 160/255),
        Chartreuse = rgb(127/255, 255/255, 0/255),
        Chocolate = rgb(210/255, 105/255, 30/255),
        Coral = rgb(255/255, 127/255, 80/255),
        CornflowerBlue = rgb(100/255, 149/255, 237/255),
        Cornsilk = rgb(255/255, 248/255, 220/255),
        Crimson = rgb(220/255, 20/255, 60/255),
        Cyan = rgb(0/255, 255/255, 255/255),
        DarkBlue = rgb(0/255, 0/255, 139/255),
        DarkCyan = rgb(0/255, 139/255, 139/255),
        DarkGoldenrod = rgb(184/255, 134/255, 11/255),
        DarkGray = rgb(169/255, 169/255, 169/255),
        DarkGreen = rgb(0/255, 100/255, 0/255),
        DarkKhaki = rgb(189/255, 183/255, 107/255),
        DarkMagenta = rgb(139/255, 0/255, 139/255),
        DarkOliveGreen = rgb(85/255, 107/255, 47/255),
        DarkOrange = rgb(255/255, 140/255, 0/255),
        DarkOrchid = rgb(153/255, 50/255, 204/255),
        DarkRed = rgb(139/255, 0/255, 0/255),
        DarkSalmon = rgb(233/255, 150/255, 122/255),
        DarkSeaGreen = rgb(143/255, 188/255, 143/255),
        DarkSlateBlue = rgb(72/255, 61/255, 139/255),
        DarkSlateGray = rgb(47/255, 79/255, 79/255),
        DarkTurquoise = rgb(0/255, 206/255, 209/255),
        DarkViolet = rgb(148/255, 0/255, 211/255),
        DeepPink = rgb(255/255, 20/255, 147/255),
        DeepSkyBlue = rgb(0/255, 191/255, 255/255),
        DimGray = rgb(105/255, 105/255, 105/255),
        DodgerBlue = rgb(30/255, 144/255, 255/255),
        FireBrick = rgb(178/255, 34/255, 34/255),
        FloralWhite = rgb(255/255, 250/255, 240/255),
        ForestGreen = rgb(34/255, 139/255, 34/255),
        Fuchsia = rgb(255/255, 0/255, 255/255),
        Gainsboro = rgb(220/255, 220/255, 220/255),
        GhostWhite = rgb(248/255, 248/255, 255/255),
        Gold = rgb(255/255, 215/255, 0/255),
        Goldenrod = rgb(218/255, 165/255, 32/255),
        Gray = rgb(128/255, 128/255, 128/255),
        Green = rgb(0/255, 128/255, 0/255),
        GreenYellow = rgb(173/255, 255/255, 47/255),
        Honeydew = rgb(240/255, 255/255, 240/255),
        HotPink = rgb(255/255, 105/255, 180/255),
        IndianRed = rgb(205/255, 92/255, 92/255),
        Indigo = rgb(75/255, 0/255, 130/255),
        Ivory = rgb(255/255, 255/255, 240/255),
        Khaki = rgb(240/255, 230/255, 140/255),
        Lavender = rgb(230/255, 230/255, 250/255),
        LavenderBlush = rgb(255/255, 240/255, 245/255),
        LawnGreen = rgb(124/255, 252/255, 0/255),
        LemonChiffon = rgb(255/255, 250/255, 205/255),
        LightBlue = rgb(173/255, 216/255, 230/255),
        LightCoral = rgb(240/255, 128/255, 128/255),
        LightCyan = rgb(224/255, 255/255, 255/255),
        LightGoldenrodYellow = rgb(250/255, 250/255, 210/255),
        LightGreen = rgb(144/255, 238/255, 144/255),
        LightGrey = rgb(211/255, 211/255, 211/255),
        LightPink = rgb(255/255, 182/255, 193/255),
        LightSalmon = rgb(255/255, 160/255, 122/255),
        LightSeaGreen = rgb(32/255, 178/255, 170/255),
        LightSkyBlue = rgb(135/255, 206/255, 250/255),
        LightSlateGray = rgb(119/255, 136/255, 153/255),
        LightSteelBlue = rgb(176/255, 196/255, 222/255),
        LightYellow = rgb(255/255, 255/255, 224/255),
        Lime = rgb(0/255, 255/255, 0/255),
        LimeGreen = rgb(50/255, 205/255, 50/255),
        Linen = rgb(250/255, 240/255, 230/255),
        Magenta = rgb(255/255, 0/255, 255/255),
        Maroon = rgb(128/255, 0/255, 0/255),
        MediumAquamarine = rgb(102/255, 205/255, 170/255),
        MediumBlue = rgb(0/255, 0/255, 205/255),
        MediumOrchid = rgb(186/255, 85/255, 211/255),
        MediumPurple = rgb(147/255, 112/255, 219/255),
        MediumSeaGreen = rgb(60/255, 179/255, 113/255),
        MediumSlateBlue = rgb(123/255, 104/255, 238/255),
        MediumSpringGreen = rgb(0/255, 250/255, 154/255),
        MediumTurquoise = rgb(72/255, 209/255, 204/255),
        MediumVioletRed = rgb(199/255, 21/255, 133/255),
        MidnightBlue = rgb(25/255, 25/255, 112/255),
        MintCream = rgb(245/255, 255/255, 250/255),
        MistyRose = rgb(255/255, 228/255, 225/255),
        Moccasin = rgb(255/255, 228/255, 181/255),
        NavajoWhite = rgb(255/255, 222/255, 173/255),
        Navy = rgb(0/255, 0/255, 128/255),
        OldLace = rgb(253/255, 245/255, 230/255),
        Olive = rgb(128/255, 128/255, 0/255),
        OliveDrab = rgb(128/255, 128/255, 0/255),
        Orange = rgb(255/255, 165/255, 0/255),
        OrangeRed = rgb(255/255, 69/255, 0/255),
        Orchid = rgb(218/255, 112/255, 214/255),
        PaleGoldenrod = rgb(238/255, 232/255, 170/255),
        PaleGreen = rgb(152/255, 251/255, 152/255),
        PaleTurquoise = rgb(175/255, 238/255, 238/255),
        PaleVioletRed = rgb(219/255, 112/255, 147/255),
        PapayaWhip = rgb(255/255, 239/255, 213/255),
        PeachPuff = rgb(255/255, 218/255, 185/255),
        Peru = rgb(205/255, 133/255, 63/255),
        Pink = rgb(255/255, 192/255, 203/255),
        Plum = rgb(221/255, 160/255, 221/255),
        PowderBlue = rgb(176/255, 224/255, 230/255),
        Purple = rgb(128/255, 0/255, 128/255),
        Red = rgb(255/255, 0/255, 0/255),
        RosyBrown = rgb(188/255, 143/255, 143/255),
        RoyalBlue = rgb(65/255, 105/255, 225/255),
        SaddleBrown = rgb(139/255, 69/255, 19/255),
        Salmon = rgb(250/255, 128/255, 114/255),
        SandyBrown = rgb(244/255, 164/255, 96/255),
        SeaGreen = rgb(46/255, 139/255, 87/255),
        Seashell = rgb(255/255, 245/255, 238/255),
        Sienna = rgb(160/255, 82/255, 45/255),
        Silver = rgb(192/255, 192/255, 192/255),
        SkyBlue = rgb(135/255, 206/255, 235/255),
        SlateBlue = rgb(106/255, 90/255, 205/255),
        SlateGray = rgb(112/255, 128/255, 144/255),
        Snow = rgb(255/255, 250/255, 250/255),
        SpringGreen = rgb(0/255, 255/255, 127/255),
        SteelBlue = rgb(70/255, 130/255, 180/255),
        Tan = rgb(210/255, 180/255, 140/255),
        Teal = rgb(210/255, 180/255, 140/255),
        Thistle = rgb(216/255, 191/255, 216/255),
        Tomato = rgb(255/255, 99/255, 71/255),
        Turquoise = rgb(64/255, 224/255, 208/255),
        Violet = rgb(238/255, 130/255, 238/255),
        Wheat = rgb(245/255, 222/255, 179/255),
        WhiteSmoke = rgb(245/255, 245/255, 245/255),
        Yellow = rgb(255/255, 255/255, 0/255),
        YellowGreen = rgb(154/255, 205/255, 50/255)
    )
)
# }}}

# map_color {{{
map_color <- function (dt, type = "surface_type", x_ray = FALSE) {
    type <- match.arg(type, c("surface_type", "boundary", "construction", "zone"))

    alpha <- if (x_ray) 0.4 else 1.0
    # init color to white
    set(dt, NULL, c("color", "alpha"), list("white", alpha))

    if (type == "surface_type") {
        cl <- data.table(surface_type = names(COLOR_MAP$surface_type), color = COLOR_MAP$surface_type)
        dt[cl, on = "surface_type", `:=`(color = i.color)]

        # TODO: figure out the outside face and the inside face
        dt[, surface_type_int := surface_type]
        dt[cl, on = c("surface_type_int" = "surface_type"), `:=`(color_int = i.color)]
        set(dt, NULL, "surface_type_int", NULL)

        trans <- paste0(c("Window", "GlassDoor"), rep(c("", "_Int", "_Ext"), 2L))
        if (!x_ray) dt[J(trans), on = "surface_type", alpha := 0.6]
    } else if (type == "boundary") {
        set(dt, NULL, "boundary_lower", stri_trans_tolower(dt$outside_boundary_condition))
        set(dt, NULL, "sun_exposure_lower", stri_trans_tolower(dt$sun_exposure))
        set(dt, NULL, "wind_exposure_lower", stri_trans_tolower(dt$wind_exposure))

        dt[J("outdoors", "sunexposed", "windexposed"),
           on = c("boundary_lower", "sun_exposure_lower", "wind_exposure_lower"),
           boundary_lower := "outdoors_sunwind"]

        dt[boundary_lower == "outdoors" & sun_exposure_lower == "sunexposed" & wind_exposure_lower != "windexposed",
           boundary_lower := "outdoors_sun"]

        dt[boundary_lower == "outdoors" & sun_exposure_lower != "sunexposed" & wind_exposure_lower == "windexposed",
           boundary_lower := "outdoors_wind"]

        cl <- data.table(boundary_lower = stri_trans_tolower(names(COLOR_MAP$boundary)), color = COLOR_MAP$boundary)
        dt[cl, on = "boundary_lower", color := i.color]

        set(dt, NULL, c("boundary_lower", "sun_exposure_lower", "wind_exposure_lower"), NULL)
    } else {
        col <- switch(type, construction = "construction_name", zone = "zone_name")
        col_lower <- paste0(col, "_lower")

        set(dt, NULL, col_lower, stri_trans_tolower(dt[[col]]))

        cl <- dt[!is.na(get(col)), list(lower = unique(get(col_lower)))][
            , color := sample(COLOR_MAP$color, .N, replace = TRUE)]
        setnames(cl, "lower", col_lower)
        dt[cl, on = c(col_lower), color := i.color]
        set(dt, NULL, col_lower, NULL)
    }

    dt
}
# }}}
