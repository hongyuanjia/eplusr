# get_geom_class {{{
get_geom_class <- function (idf, object = NULL) {
    # geometry and daylighting points
    cls <- idf$class_name(by_group = TRUE)[c("Thermal Zones and Surfaces", "Daylighting")]
    cls <- data.table(class = unlist(cls))
    # category by class names
    set(cls, NULL, c("type", "subtype", "misc"),
        as.data.table(stri_split_fixed(cls$class, ":", n = 3L, simplify = TRUE))
    )

    cls[type %chin% c("BuildingSurface", "Wall", "RoofCeiling", "Floor", "Wall", "Roof", "Ceiling"), category := "Surface"]
    cls[type %chin% c("FenestrationSurface", "Window", "Door", "GlazedDoor"), category := "SubSurface"]
    cls[type == "RoofCeiling", subtype := "Roof"]
    cls[type == "Shading", category := "Shading"]
    cls[type == "Daylighting", category := "Daylighting"]
    cls <- setcolorder(cls[!J(NA_character_), on = "category"], "category")

    if (is.null(object)) return(cls)

    obj <- get_idf_object(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
        object = object, ignore_case = TRUE)[, list(class = class_name,
            id = object_id, name = object_name)]

    cls[obj, on = "class", nomatch = NULL]
}
# }}}

# get_global_geom_rules {{{
get_global_geom_rules <- function (idf) {
    if (!idf$is_valid_class("GlobalGeometryRules")) {
        warn("No 'GlobalGeometryRules' object found in current IDF. Assuming all defaults.",
            "geom_no_global_geom_rules"
        )
        rules <- list(
            starting_vertex_position = "upperleftcorner",
            vertex_entry_direction = "counterclockwise",
            coordinate_system = "relative",
            daylighting_reference_point_coordinate_system = "relative",
            rectangular_surface_coordinate_system = "relative"
        )
    } else {
        rules <- get_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
            "GlobalGeometryRules", all = TRUE, property = "choice"
        )
        # assign default value for missing fields
        if (any(i <- rules$value_id < 0L)) {
            rules <- assign_idf_value_default(
                get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
                rules[value_id < 0L, defaulted := TRUE]
            )
            set(rules, NULL, "defaulted", NULL)
        }

        choices <- rules$choice
        rules <- setattr(as.list(rules$value_chr), "names", rules$field_name)

        for (i in seq_along(rules)) {
            if (is.na(rules[[i]])) {
                warn(sprintf("Empty '%s' found in 'GlobalGeometryRules'. Assuming '%s'.",
                    names(rules[i]), choices[[i]][[1L]]
                ), "geom_invalid_ggr")
                rules[[i]] <- stri_trans_tolower(choices[[i]][[1L]])
            } else if (!stri_trans_tolower(rules[[i]]) %chin% stri_trans_tolower(choices[[i]])) {
                warn(sprintf("Invalid '%s' found ('%s') in 'GlobalGeometryRules'. Assuming '%s'.",
                    names(rules[i]), rules[[i]], choices[[i]][[1L]]
                ), "geom_invalid_ggr")
                rules[[i]] <- stri_trans_tolower(choices[[i]][[1L]])
            } else {
                rules[[i]] <- stri_trans_tolower(rules[[i]])
            }

            if (rules[[i]] == "world") rules[[i]] <- "absolute"
        }

        setattr(rules, "names", lower_name(names(rules)))
    }

    rules
}
# }}}

# get_building_transformation {{{
get_building_transformation <- function (idf) {
    if (!idf$is_valid_class("Building")) {
        warn("Could not find 'Building' object, assuming 0 rotation", "geom_no_building")

        list(id = NA_integer_, name = NA_character_, north_axis = 0.0)
    } else {
        bldg <- get_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
            "Building", field = 2L, complete = TRUE)
        id <- bldg$object_id[[1L]]
        name <- bldg$object_name[[1L]]
        north_axis <- bldg$value_num[[2L]]

        if (is.na(north_axis)) {
            warn("North Axis unknown. Using 0.", "geom_unknown_north_axis")
            north_axis <- 0.0
        }

        list(id = id, name = name, north_axis = north_axis)
    }
}
# }}}

# get_zone_transformation {{{
get_zone_transformation <- function (idf) {
    if (!idf$is_valid_class("Zone")) {
        zone <- data.table(id = integer(), name = character(), x = double(), y = double(), z = double())
    } else {
        zone <- idf$to_table(class = "Zone", wide = TRUE, all = TRUE, string_value = FALSE)[
            , .SD, .SDcols = c("id", "name", paste(c("X", "Y", "Z"), "Origin"), "Direction of Relative North")]
        setnames(zone, c("id", "name", "x", "y", "z", "dir_relative_north"))
        if (nrow(mis_origin <- na.omit(zone, by = c("x", "y", "z"), invert = TRUE))) {
            warn(paste0("Zone below has unknown origin. (0, 0, 0) will be used:\n",
                collapse(mis_origin$name)
            ), "geom_no_zone_origin")
            zone[J(NA_real_), on = "x", x := 0.0]
            zone[J(NA_real_), on = "y", y := 0.0]
            zone[J(NA_real_), on = "z", z := 0.0]
        }

        if (anyNA(zone$dir_relative_north)) {
            warn(paste0("Zone below has unknown direction of relative North. 0 will be used:\n",
                collapse(zone[is.na(dir_relative_north), name])
            ), "geom_no_zone_north")

            zone[J(NA_real_), on = "dir_relative_north", dir_relative_north := 0.0]
        }
    }
    zone
}
# }}}

# extract_geom {{{
extract_geom <- function (idf, object = NULL) {
    geom_class <- get_geom_class(idf, object)

    # get current global geometry rules
    rules <- get_global_geom_rules(idf)

    # convert to counter clockwise vertex entry direction
    if (rules$vertex_entry_direction == "clockwise") {
        warn(paste("'Clockwise' vertex entry direction found in 'GlobalGeometryRules'.",
            "All vertices will be changed to 'Counterclockwise' direction."
        ))
        idf <- reverse_idf_detailed_vertices(idf, geom_class)
        rules$vertex_entry_direction <- "counterclockwise"
    }

    # extract surface data
    surface <- extract_geom_surface(idf, geom_class)

    # extract subsurface data
    subsurface <- extract_geom_subsurface(idf, geom_class, surface = surface)

    # extract shading data
    shading <- extract_geom_shading(idf, geom_class, subsurface = subsurface)

    # extract daylighting reference point data
    dayl_pnts <- extract_geom_daylighting_point(idf, geom_class)

    # merge all vertices
    vertices <- rbindlist(list(surface$vertices, subsurface$vertices, shading$vertices, dayl_pnts$vertices))
    if (nrow(vertices)) setindexv(vertices, "id")

    # building transformation
    building <- get_building_transformation(idf)

    # zone transformation
    zone <- get_zone_transformation(idf)

    list(rules = rules, building = building, zone = zone,
         surface = surface$meta, subsurface = subsurface$meta,
         shading = shading$meta, daylighting_point = dayl_pnts$meta,
         vertices = vertices
    )
}
# }}}

# extract_geom_surface {{{
extract_geom_surface <- function (idf, geom_class = NULL, object = NULL) {
    if (is.null(geom_class)) geom_class <- get_geom_class(idf, object)

    if (!nrow(geom_class)) return(list(meta = data.table(), vertices = data.table()))

    detailed <- extract_geom_surface_detailed(idf, geom_class)
    simple <- extract_geom_surface_simple(idf, geom_class)

    meta <- rbindlist(list(detailed$meta, simple$meta))
    vertices <- rbindlist(list(detailed$vertices, simple$vertices))
    if (nrow(meta)) {
        list(meta = setorderv(meta, "id"), vertices = setorderv(vertices, "id"))
    } else {
        list(meta = meta, vertices = vertices)
    }
}
# }}}

# extract_geom_surface_detailed {{{
extract_geom_surface_detailed <- function (idf, geom_class = NULL, object = NULL) {
    if (is.null(geom_class)) geom_class <- get_geom_class(idf, object)
    geom_class <- geom_class[J("Surface", "Detailed"), on = c("category", "subtype"), nomatch = NULL]

    if (!nrow(geom_class)) return(list(meta = data.table(), vertices = data.table()))

    # fields needed
    fld <- get_idd_field(get_priv_env(idf)$idd_env(), "BuildingSurface:Detailed", 1:8)$field_name

    # extract data
    dt <- get_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
        class = geom_class$class, object = geom_class$id, complete = TRUE,
        property = "extensible_group"
    )[field_name %chin% fld | extensible_group > 0L]

    # meta
    meta <- dt[J(0L), on = "extensible_group"]
    meta <- standardize_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(), meta, keep = FALSE)
    meta <- dcast.data.table(meta, object_id + class_name ~ field_name, value.var = "value_chr")
    setnames(meta, lower_name(names(meta)))
    meta[geom_class[!J("Detailed"), on = "subtype"], on = c(class_name = "class"), surface_type := i.subtype]
    setcolorder(meta, c("object_id", "name", "class_name", "surface_type",
        "construction_name", "zone_name"))
    # a surface will be an adiabatic one if the outside boundary condition
    # object is itself
    meta[name == outside_boundary_condition_object, `:=`(outside_boundary_condition = "Adiabatic", outside_boundary_condition_object = NA_character_)]
    setnames(meta, c("object_id", "class_name"), c("id", "class"))

    # vertices
    vertices <- dt[!J(0L), on = "extensible_group"]
    vertices[, by = c("object_id", "extensible_group"),
        extensible_group_index := seq_len(.N)]
    vertices <- dcast.data.table(vertices, object_id + extensible_group ~ extensible_group_index,
        value.var = "value_num")
    setnames(vertices, c("id", "index", "x", "y", "z"))

    list(meta = meta, vertices = vertices)
}
# }}}

# extract_geom_surface_simple {{{
extract_geom_surface_simple <- function (idf, geom_class = NULL, object = NULL) {
    if (is.null(geom_class)) geom_class <- get_geom_class(idf, object)
    geom_class <- geom_class[category == "Surface" & subtype != "Detailed"]

    if (!nrow(geom_class)) return(list(meta = data.table(), vertices = data.table()))

    # extract data
    dt <- get_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
        class = geom_class$class, object = geom_class$id, all = TRUE)

    dt <- standardize_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(), dt, keep = FALSE)

    # add surface type
    dt[geom_class, on = c("class_name" = "class"), surface_type := i.type]

    # use "height" for all
    dt[surface_type != "Wall" & field_name == "Width", field_name := "Height"]

    # change to wide
    dt <- dcast.data.table(dt, object_id + class_name + surface_type ~ field_name, value.var = "value_chr")

    # rename
    setnames(dt, lower_name(names(dt)))

    # in case there is no '*.Interzone'
    if (!"outside_boundary_condition_object" %chin% names(dt)) {
        set(dt, NULL, "outside_boundary_condition_object", NA_character_)
    }

    # rename
    setnames(dt, paste0("starting_", c("x", "y", "z"), "_coordinate"), c("x0", "y0", "z0"))
    setnames(dt, c("azimuth_angle", "tilt_angle"), c("azimuth", "tilt"))

    # format numeric fields
    cols <- c("azimuth", "tilt", "length", "height", "x0", "y0", "z0")
    dt[, c(cols) := lapply(.SD, as.numeric), .SDcols = cols]

    # calculate vertices
    vertices <- get_vertices_from_specs(dt$azimuth, dt$tilt, dt$length, dt$height, dt$x0, dt$y0, dt$z0)
    vertices <- data.table(id = rep(dt$object_id, each = 4L), index = rep(1:4, nrow(dt)),
        x = round(as.numeric(t(matrix(unlist(vertices$x), ncol = 4L))), 4L),
        y = round(as.numeric(t(matrix(unlist(vertices$y), ncol = 4L))), 4L),
        z = round(as.numeric(t(matrix(unlist(vertices$z), ncol = 4L))), 4L)
    )

    # extract meta
    set(dt, NULL, c("azimuth", "tilt", "length", "height", "x0", "y0", "z0"), NULL)

    # complete column
    geom_class[J(c("Exterior", "")), on = "subtype", `:=`(
        outside_boundary_condition = "Outdoors",
        sun_exposure = "SunExposed", wind_exposure = "WindExposed"
    )]
    geom_class[J(c("Adiabatic", "Ground", "GroundContact")), on = "subtype", `:=`(
        outside_boundary_condition = gsub("Contact", "", subtype, fixed = TRUE),
        sun_exposure = "NoSun", wind_exposure = "NoWind"
    )]
    geom_class[J(c("Interzone")), on = "subtype", `:=`(
        outside_boundary_condition = "Surface",
        sun_exposure = "NoSun", wind_exposure = "NoWind"
    )]

    dt[geom_class, on = c("class_name" = "class"),
        `:=`(outside_boundary_condition = i.outside_boundary_condition,
             sun_exposure = i.sun_exposure, wind_exposure = i.wind_exposure
        )
    ]

    setnames(dt, c("object_id", "class_name"), c("id", "class"))
    setcolorder(dt, c("id", "name", "class", "surface_type",
        "construction_name", "zone_name", "outside_boundary_condition"))
    list(meta = dt, vertices = vertices)
}
# }}}

# extract_geom_subsurface {{{
extract_geom_subsurface <- function (idf, geom_class = NULL, object = NULL, surface = NULL) {
    if (is.null(geom_class)) geom_class <- get_geom_class(idf, object)

    if (!nrow(geom_class)) return(list(meta = data.table(), vertices = data.table()))

    detailed <- extract_geom_subsurface_detailed(idf, geom_class)
    simple <- extract_geom_subsurface_simple(idf, geom_class, surface = surface)

    meta <- rbindlist(list(detailed$meta, simple$meta))
    vertices <- rbindlist(list(detailed$vertices, simple$vertices))
    if (nrow(meta)) {
        list(meta = setorderv(meta, "id"), vertices = setorderv(vertices, "id"))
    } else {
        list(meta = meta, vertices = vertices)
    }
}
# }}}

# extract_geom_subsurface_detailed {{{
extract_geom_subsurface_detailed <- function (idf, geom_class = NULL, object = NULL) {
    if (is.null(geom_class)) geom_class <- get_geom_class(idf, object)

    # currently only 'FenestrationSurface:Detailed' is the defailed geometry
    geom_class <- geom_class[J("SubSurface", "Detailed"), on = c("category", "subtype"), nomatch = NULL]

    if (!nrow(geom_class)) return(list(meta = data.table(), vertices = data.table()))

    # extract data
    dt <- get_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
        class = geom_class$class, object = geom_class$id, all = TRUE
    )

    # meta
    meta <- dt[J(1:5), on = "field_index"]
    meta <- standardize_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(), meta, keep = FALSE)
    meta <- dcast.data.table(meta, object_id + class_name ~ field_name, value.var = "value_chr")
    setnames(meta, lower_name(names(meta)))
    setcolorder(meta, c("object_id", "name", "class_name", "surface_type", "construction_name"))
    setnames(meta, c("object_id", "class_name"), c("id", "class"))

    # vertices
    if (idf$version() < 9.0) {
        vertices <- dt[J(11:22), on = "field_index"][, by = "object_id",
            list(index = rep(1:4, each = 3L), field = rep(c("x", "y", "z"), 4L), value_num)]
    } else {
        vertices <- dt[J(10:21), on = "field_index"][, by = "object_id",
            list(index = rep(1:4, each = 3L), field = rep(c("x", "y", "z"), 4L), value_num)]
    }
    vertices <- dcast.data.table(vertices, object_id + index ~ field, value.var = "value_num")
    setnames(vertices, "object_id", "id")

    list(meta = meta, vertices = vertices)
}
# }}}

# extract_geom_subsurface_simple {{{
extract_geom_subsurface_simple <- function (idf, geom_class = NULL, object = NULL, surface = NULL) {
    if (is.null(geom_class)) geom_class <- get_geom_class(idf, object)
    geom_class <- geom_class[category == "SubSurface" & subtype != "Detailed"]

    if (!nrow(geom_class)) return(list(meta = data.table(), vertices = data.table()))

    # extract data
    dt <- get_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
        class = geom_class$class, object = geom_class$id, all = TRUE)

    dt <- standardize_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(), dt, keep = FALSE)

    # remove uncessary fields
    dt <- dt[!J(c("Frame and Divider Name", "Multiplier", "Shading Control Name")), on = "field_name"]

    # change to wide
    dt <- dcast.data.table(dt, object_id + class_name ~ field_name, value.var = "value_chr")

    # rename
    setnames(dt, lower_name(names(dt)))

    # in case there is no '*.Interzone'
    if (!"outside_boundary_condition_object" %chin% names(dt)) {
        set(dt, NULL, "outside_boundary_condition_object", NA_character_)
    }

    # rename
    setnames(dt, paste0("starting_", c("x", "z"), "_coordinate"), c("x0", "z0"))

    # format numeric fields
    cols <- c("length", "height", "x0", "z0")
    dt[, c(cols) := lapply(.SD, as.numeric), .SDcols = cols]

    # vertices
    # remove ones that have incomplete data
    vertices <- dt[
        , by = "object_id", list(
        index = 1:4,
        x = c(x0, x0, x0 + length, x0 + length),
        y = c(z0 + height, z0, z0, z0 + height),
        z = 0.0
    )]
    setnames(vertices, "object_id", "id")

    # meta
    meta <- set(dt, NULL, cols, NULL)
    # add surface type
    meta[geom_class, on = c("class_name" = "class"), surface_type := i.type]
    # rename
    setnames(meta, c("object_id", "class_name"), c("id", "class"))
    setcolorder(meta, c("id", "name", "class", "surface_type",
        "construction_name", "building_surface_name", "outside_boundary_condition_object"))

    # get parent surface vertices
    nm_surface <- meta[J(unique(vertices$id)), on = "id", unique(building_surface_name)]
    if (is.null(surface)) {
        surface <- extract_geom_surface(idf, object = nm_surface)
    } else {
        surface$meta <- surface$meta[J(nm_surface), on = "name"]
        surface$vertices <- surface$vertices[surface$meta, on = "id"]
    }

    # get transformation from face coordinates to zone
    surface$trans <- align_face(surface$vertices)

    # add surface name for matching
    surface$trans[surface$meta, on = "id", building_surface_name := i.name]
    vertices[meta, on = "id", building_surface_name := i.building_surface_name]

    # transform
    vertices[surface$trans, on = "building_surface_name", "trans" := i.trans][
        , by = "id", c("x", "y", "z") := {
            if (is.null(trans[[1L]])) {
                list(NA_real_, NA_real_, NA_real_)
            } else {
                vert <- apply(matrix(c(x, y, z, rep(1.0, .N)), ncol = 4L), 1, function (x) trans[[1L]] %*% x)[1:3,]
                list(vert[1L,], vert[2L,], vert[3L,])
            }
        }
    ]
    set(vertices, NULL, c("building_surface_name", "trans"), NULL)

    list(meta = meta, vertices = vertices)
}
# }}}

# extract_geom_shading {{{
extract_geom_shading <- function (idf, geom_class = NULL, object = NULL, subsurface = NULL) {
    if (is.null(geom_class)) geom_class <- get_geom_class(idf, object)

    if (!nrow(geom_class)) return(list(meta = data.table(), vertices = data.table()))

    detailed <- extract_geom_shading_detailed(idf, geom_class)
    simple <- extract_geom_shading_simple(idf, geom_class, subsurface = subsurface)

    meta <- rbindlist(list(detailed$meta, simple$meta))
    vertices <- rbindlist(list(detailed$vertices, simple$vertices))
    if (nrow(meta)) {
        list(meta = setorderv(meta, "id"), vertices = setorderv(vertices, "id"))
    } else {
        list(meta = meta, vertices = vertices)
    }
}
# }}}

# extract_geom_shading_detailed {{{
extract_geom_shading_detailed <- function (idf, geom_class = NULL, object = NULL) {
    if (is.null(geom_class)) geom_class <- get_geom_class(idf, object)
    geom_class <- geom_class[J("Shading", "Detailed"), on = c("category", "misc"), nomatch = NULL]

    if (!nrow(geom_class)) return(list(meta = data.table(), vertices = data.table()))

    # extract data
    dt <- get_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
        class = geom_class$class, object = geom_class$id,
        complete = TRUE, property = "extensible_group"
    )[field_name %chin% c("Name", "Base Surface Name") | extensible_group > 0L]

    # meta
    meta <- dt[J(0L), on = "extensible_group"]
    meta <- standardize_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(), meta, keep = FALSE)
    meta <- dcast.data.table(meta, object_id + class_name ~ field_name, value.var = "value_chr")
    setnames(meta, lower_name(names(meta)))
    # in case there is no Shading:Zone:Detailed
    if (!"base_surface_name" %chin% names(meta)) {
        set(meta, NULL, "base_surface_name", NA_character_)
    }
    meta[geom_class, on = c(class_name = "class"), surface_type := paste0(i.subtype, "Shading")]
    setcolorder(meta, c("object_id", "name", "class_name", "surface_type", "base_surface_name"))
    setnames(meta, c("id", "name", "class", "surface_type", "base_surface_name"))

    # vertices
    vertices <- dt[!J(0L), on = "extensible_group"]
    vertices[, by = c("object_id", "extensible_group"),
        extensible_group_index := seq_len(.N)]
    vertices <- dcast.data.table(vertices, object_id + extensible_group ~ extensible_group_index,
        value.var = "value_num")
    setnames(vertices, c("id", "index", "x", "y", "z"))

    list(meta = meta, vertices = vertices)
}
# }}}

# extract_geom_shading_simple {{{
extract_geom_shading_simple <- function (idf, geom_class = NULL, object = NULL, subsurface = NULL) {
    if (is.null(geom_class)) geom_class <- get_geom_class(idf, object)
    geom_class <- geom_class[category == "Shading" & misc != "Detailed"]

    if (!nrow(geom_class)) return(list(meta = data.table(), vertices = data.table()))

    # extract data
    dt <- get_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
        class = geom_class$class, object = geom_class$id, all = TRUE)

    dt <- standardize_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(), dt, keep = FALSE)

    # add surface type
    dt[geom_class, on = c("class_name" = "class"), surface_type := paste0(i.subtype, "Shading")]

    # Site and Building shadings {{{
    meta_shade <- data.table()
    vertices_shade <- data.table()
    if (any(c("Site", "Building") %chin% geom_class$subtype)) {
        shade <- dt[surface_type %chin% c("SiteShading", "BuildingShading") & field_index != 1L]
        shade <- dcast.data.table(shade, object_id + object_name + class_name + surface_type ~ field_name, value.var = "value_num")
        setnames(shade, lower_name(names(shade)))
        setnames(shade, paste0("starting_", c("x", "y", "z"), "_coordinate"), c("x0", "y0", "z0"))
        setnames(shade, c("azimuth_angle", "tilt_angle"), c("azimuth", "tilt"))

        # calculate vertices
        vertices <- get_vertices_from_specs(shade$azimuth, shade$tilt, shade$length, shade$height, shade$x0, shade$y0, shade$z0)
        vertices_shade <- data.table(id = rep(shade$object_id, each = 4L), index = rep(1:4, nrow(shade)),
            x = as.numeric(t(matrix(unlist(vertices$x), ncol = 4L))),
            y = as.numeric(t(matrix(unlist(vertices$y), ncol = 4L))),
            z = as.numeric(t(matrix(unlist(vertices$z), ncol = 4L)))
        )

        # extract meta
        meta_shade <- set(shade, NULL, c("azimuth", "tilt", "length", "height", "x0", "y0", "z0"), NULL)
        setnames(meta_shade, c("id", "name", "class", "surface_type"))

        # make sure all meta tables have the same column
        set(meta_shade, NULL, "base_surface_name", NA_character_)
    }
    # }}}

    # Overhang {{{
    meta_overhang <- data.table()
    vertices_overhang <- data.table()
    if ("Overhang" %chin% geom_class$subtype) {
        overhang <- dt[J("OverhangShading"), on = "surface_type"]
        meta_overhang <- overhang[J(2L), on = "field_index"]
        set(meta_overhang, NULL, value = NULL,
            setdiff(names(meta_overhang),
                c("object_id", "object_name", "class_name", "surface_type", "value_chr"))
        )
        setnames(meta_overhang, c("class", "id", "name", "window_or_door_name", "surface_type"))
        setcolorder(meta_overhang, c("id", "name", "class", "surface_type"))
        set(meta_overhang, NULL, "surface_type", "ZoneShading")

        # calculate vertices
        vertices_overhang <- overhang[!J(1:2), on = "field_index"]
        vertices_overhang[J(7L), on = "field_index", field_name := "Depth"]
        vertices_overhang <- dcast.data.table(vertices_overhang, object_id + object_name + class_name + surface_type ~ field_name, value.var = "value_num")
        setnames(vertices_overhang, lower_name(names(vertices_overhang)))
        setnames(vertices_overhang, gsub("_(from|above){0,1}_window(_or){0,1}_door(_width){0,1}", "", names(vertices_overhang)))
        setnames(vertices_overhang, "object_id", "id")

        # get parent subsurface vertices
        if (any(!is.na(meta_overhang$window_or_door_name))) {
            nm_subsurf<- meta_overhang[!J(NA_character_), on = "window_or_door_name", unique(window_or_door_name)]
            if (is.null(subsurface)) {
                subsurf <- extract_geom_subsurface(idf, object = nm_subsurf)
            } else {
                subsurf <- subsurface
                subsurf$meta <- subsurf$meta[J(nm_subsurf), on = "name"]
                subsurf$vertices <- subsurf$vertices[subsurf$meta, on = "id"]
            }
            # get transformation from face coordinates to zone
            subsurf$trans <- align_face(subsurf$vertices)

            # figure out bounds of the subsurface in face coordinates
            bound_verts <- subsurf$vertices[subsurf$trans, on = "id", by = .EACHI,
                {
                    align_inv <- solve(i.trans[[1L]])
                    align_vert <- apply(matrix(c(x, y, z, rep(1.0, .N)), ncol = 4L), 1, function (x) align_inv %*% x)[1:3,]
                    r_x <- range(align_vert[1,])
                    r_y <- range(align_vert[2,])
                    list(min_x = r_x[[1L]], max_x = r_x[[2L]], min_y = r_y[[1L]], max_y = r_y[[2L]])
                }
            ]
            bound_verts[subsurf$meta, on = "id", window_or_door_name := i.name]
            set(bound_verts, NULL, "id", NULL)

            # add subsurface name
            vertices_overhang[meta_overhang, on = "id", window_or_door_name := i.window_or_door_name]
            # add subsurface bounds
            vertices_overhang[bound_verts, on = "window_or_door_name",
                `:=`(min_x = i.min_x, max_x = i.max_x, min_y = i.min_y, max_y = i.max_y)]

            # get depth for overhang projection
            vertices_overhang[J("Shading:Overhang:Projection"), on = "class_name",
                depth := depth * (max_y - min_y)]

            # transform vertices
            subsurf$trans[subsurf$meta, on = "id", window_or_door_name := i.name]
            vertices_overhang[subsurf$trans, on = "window_or_door_name", "trans" := i.trans]

            vertices_overhang <- vertices_overhang[, by = "id", {
                if (is.null(trans[[1L]])) {
                    list(index = 1:4, x = NA_real_, y = NA_real_, z = NA_real_)
                } else {
                    x <- c(max_x + right_extension, max_x + right_extension, min_x - left_extension, min_x - left_extension)
                    y <- c(max_y + height + depth * cos(deg_to_rad(tilt_angle)), max_y + height, max_y + height, max_y + height + depth * cos(deg_to_rad(tilt_angle)))
                    z <- c(depth * sin(deg_to_rad(tilt_angle)), 0.0, 0.0, depth * sin(deg_to_rad(tilt_angle)))
                    vert <- apply(matrix(c(x, y, z, rep(1.0, 4L)), ncol = 4L), 1, function (x) trans[[1L]] %*% x)[1:3,]
                    list(index = 1:4, x = vert[1L,], y = vert[2L,], z = vert[3L,])
                }
            }]

            # add building surface name
            meta_overhang[subsurf$meta, on = c("window_or_door_name" = "name"), base_surface_name := i.building_surface_name]
            set(meta_overhang, NULL, "window_or_door_name", NULL)
        }
    }
    # }}}

    # Fin {{{
    meta_fin <- data.table()
    vertices_fin <- data.table()
    if ("Fin" %chin% geom_class$subtype) {
        fin <- dt[J("FinShading"), on = "surface_type"]
        meta_fin <- fin[J(2L), on = "field_index"]
        set(meta_fin, NULL, value = NULL,
            setdiff(names(meta_fin),
                c("object_id", "object_name", "class_name", "surface_type", "value_chr"))
        )
        setnames(meta_fin, c("class", "id", "name", "window_or_door_name", "surface_type"))
        setcolorder(meta_fin, c("id", "name", "class", "surface_type"))
        set(meta_fin, NULL, "surface_type", "ZoneShading")

        # calculate vertices
        vertices_fin <- fin[!J(1:2), on = "field_index"]
        vertices_fin[J("Shading:Fin:Projection"), on = "class_name", field_name := gsub("((Left|Right) Depth).*", "\\1", field_name)]
        vertices_fin <- dcast.data.table(vertices_fin, object_id + object_name + class_name + surface_type ~ field_name, value.var = "value_num")
        setnames(vertices_fin, lower_name(names(vertices_fin)))
        setnames(vertices_fin, gsub("_from.+", "", names(vertices_fin)))
        setnames(vertices_fin, gsub("(above|below).+", "\\1", names(vertices_fin)))
        setnames(vertices_fin, "object_id", "id")

        # get parent subsurface vertices
        if (any(!is.na(meta_fin$window_or_door_name))) {
            nm_subsurf <- meta_fin[!J(NA_character_), on = "window_or_door_name", unique(window_or_door_name)]
            if (is.null(subsurface)) {
                subsurf <- extract_geom_subsurface(idf, object = nm_subsurf)
            } else {
                subsurf <- subsurface
                subsurf$meta <- subsurf$meta[J(nm_subsurf), on = "name"]
                subsurf$vertices <- subsurf$vertices[subsurf$meta, on = "id"]
            }

            # get transformation from face coordinates to zone
            subsurf$trans <- align_face(subsurf$vertices)

            # figure out bounds of the subsurf in face coordinates
            bound_verts <- subsurf$vertices[subsurf$trans, on = "id", by = .EACHI,
                {
                    align_inv <- solve(i.trans[[1L]])
                    align_vert <- apply(matrix(c(x, y, z, rep(1.0, .N)), ncol = 4L), 1, function (x) align_inv %*% x)[1:3,]

                    r_x <- range(align_vert[1,])
                    r_y <- range(align_vert[2,])
                    list(min_x = r_x[[1L]], max_x = r_x[[2L]], min_y = r_y[[1L]], max_y = r_y[[2L]])
                }
            ]
            bound_verts[subsurf$meta, on = "id", window_or_door_name := i.name]
            set(bound_verts, NULL, "id", NULL)

            # add subsurface name
            vertices_fin[meta_fin, on = "id", window_or_door_name := i.window_or_door_name]
            # add subsurface bounds
            vertices_fin[bound_verts, on = "window_or_door_name",
                `:=`(min_x = i.min_x, max_x = i.max_x, min_y = i.min_y, max_y = i.max_y)]

            # get depth for fin projection
            vertices_fin[J("Shading:Fin:Projection"), on = "class_name",
                `:=`(left_depth = left_depth * (max_y - min_y),
                     right_depth = right_depth * (max_y - min_y))
            ]

            # transform vertices
            subsurf$trans[subsurf$meta, on = "id", window_or_door_name := i.name]
            vertices_fin[subsurf$trans, on = "window_or_door_name", "trans" := i.trans]

            vertices_fin <- vertices_fin[, by = "id", {
                if (is.null(trans[[1L]])) {
                    # set id of right fins to negative to distinguish
                    list(new_id = c(rep(.BY$id, 4L), rep(-.BY$id, 4L)),
                         index = rep(1:4, 2),
                         x = NA_real_, y = NA_real_, z = NA_real_
                    )
                } else {
                    lx <- c(min_x - left_extension + left_depth * cos(deg_to_rad(left_tilt_angle)),
                            min_x - left_extension + left_depth * cos(deg_to_rad(left_tilt_angle)),
                            min_x - left_extension, min_x - left_extension)
                    ly <- c(max_y + left_distance_above, min_y - left_distance_below,
                            min_y - left_distance_below, max_y + left_distance_above)
                    lz <- c(left_depth * sin(deg_to_rad(left_tilt_angle)),
                            left_depth * sin(deg_to_rad(left_tilt_angle)),
                            0.0, 0.0)
                    lvert <- apply(matrix(c(lx, ly, lz, rep(1.0, 4L)), ncol = 4L), 1, function (x) trans[[1L]] %*% x)[1:3,]

                    rx <- c(max_x + right_extension + right_depth * cos(deg_to_rad(right_tilt_angle)),
                            max_x + right_extension + right_depth * cos(deg_to_rad(right_tilt_angle)),
                            max_x + right_extension, max_x + right_extension)
                    ry <- c(max_y + right_distance_above, min_y - right_distance_below,
                            min_y - right_distance_below, max_y + right_distance_above)
                    rz <- c(right_depth * sin(deg_to_rad(right_tilt_angle)),
                            right_depth * sin(deg_to_rad(right_tilt_angle)),
                            0.0, 0.0)
                    rvert <- apply(matrix(c(rx, ry, rz, rep(1.0, 4L)), ncol = 4L), 1, function (x) trans[[1L]] %*% x)[1:3,]

                    # set id of right fins to negative to distinguish
                    list(new_id = c(rep(.BY$id, 4L), rep(-.BY$id, 4L)), index = rep(1:4, 2),
                         x = c(lvert[1L,], rvert[1L,]),
                         y = c(lvert[2L,], rvert[2L,]),
                         z = c(lvert[3L,], rvert[3L,])
                    )
                }
            }]
            set(vertices_fin, NULL, "id", NULL)
            setnames(vertices_fin, "new_id", "id")

            # add building surface name
            meta_fin[subsurf$meta, on = c("window_or_door_name" = "name"), base_surface_name := i.building_surface_name]
            set(meta_fin, NULL, "window_or_door_name", NULL)

            meta_fin[, by = "id", index := .I]
            meta_fin <- rbindlist(list(
                copy(meta_fin)[, name := paste(name, "Left")],
                meta_fin[, `:=`(id = -id, name = paste(name, "Right"))]
            ))
            setorderv(meta_fin, "index")
            set(meta_fin, NULL, "index", NULL)
        }
    }
    # }}}

    meta <- rbindlist(list(meta_shade, meta_overhang, meta_fin))
    vertices <- rbindlist(list(vertices_shade, vertices_overhang, vertices_fin))

    list(meta = meta, vertices = vertices)
}
# }}}

# extract_geom_daylighting_point {{{
extract_geom_daylighting_point <- function (idf, geom_class = NULL, object = NULL) {
    if (is.null(geom_class)) geom_class <- get_geom_class(idf, object)

    # Previous v8.6, normal daylighting control ref pnts are saved in
    # 'Daylighting:Controls' and DE daylighting in
    # 'Daylighting:DELight:ReferencePoint'
    if (idf$version() > 8.5) {
        # currently only 'FenestrationSurface:Detailed' is the defailed geometry
        geom_class <- geom_class[J("Daylighting", "ReferencePoint"), on = c("category", "subtype"), nomatch = NULL]

        if (!nrow(geom_class)) return(list(meta = data.table(), vertices = data.table()))

        # extract data
        dt <- get_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
            class = geom_class$class, object = geom_class$id, all = TRUE)

        # meta
        meta <- dt[J(2L), on = "field_index"]
        meta <- standardize_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(), meta, keep = FALSE)
        set(meta, NULL, setdiff(names(meta), c("class_name", "object_id", "object_name", "value_chr")), NULL)
        setnames(meta, c("class", "id", "name", "zone_name"))
        setcolorder(meta, c("id", "name"))

        # vertices
        vertices <- dt[!J(1:2), on = "field_index", by = "object_id",
            list(index = field_index - 2L, value_num)]
        vertices <- dcast.data.table(vertices, object_id ~ index, value.var = "value_num")
        setnames(vertices, c("id", "x", "y", "z"))
        set(vertices, NULL, "index", 1L)
        setcolorder(vertices, c("id", "index"))
    } else {
        meta <- data.table()
        vertices <- data.table()
        # points in 'Daylighting:Controls'
        if (nrow(ctrl <- geom_class[J("Daylighting", "Controls"), on = c("category", "subtype"), nomatch = NULL])) {
            # extract data
            dt <- get_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
                class = ctrl$class, object = ctrl$object, field = c(1:8))

            # meta
            meta <- dt[J(1L), on = "field_index"]
            meta <- standardize_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(), meta, keep = FALSE)
            set(meta, NULL, setdiff(names(meta), c("class_name", "object_id", "object_name", "value_chr")), NULL)
            setnames(meta, c("class", "id", "name", "zone_name"))
            setcolorder(meta, c("id", "name"))
            # generate ref point names in the same way as transition program
            meta <- meta[, by = "id", list(
                name = paste0(data.table::fifelse(is.na(zone_name), "", zone_name), c("_DaylRefPt1", "_DaylRefPt2")),
                class = rep(class, 2L),
                zone_name = rep(zone_name, 2L)
            )]

            # vertices
            vertices <- dt[!J(1:2), on = "field_index", by = "object_id",
                list(index = rep(1:2, each = 3L), field = rep(c("x", "y", "z"), 2L), value_num)]
            vertices <- dcast.data.table(vertices, object_id + index ~ field, value.var = "value_num")
            setnames(vertices, "object_id", "id")
        }

        # points in 'Daylighting:DELight:ReferencePoint'
        if (nrow(ctrl <- geom_class[J("DELight", "ReferencePoint"), on = c("subtype", "misc"), nomatch = NULL])) {
            # extract data
            dt <- get_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
                class = ctrl$class, object = ctrl$object, field = 1:5)

            # meta
            meta2 <- dt[J(2L), on = "field_index"]
            meta2 <- standardize_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(), meta2, keep = FALSE)
            set(meta2, NULL, setdiff(names(meta2), c("class_name", "object_id", "object_name", "value_chr")), NULL)
            setnames(meta2, c("class", "id", "name", "delight_name"))
            setcolorder(meta2, c("id", "name"))
            # get zone name
            if (!"Daylighting:DELight:Controls" %chin% geom_class$class) {
                setnames(meta2, "delight_name", "zone_name")
                set(meta2, NULL, "zone_name", NA_character_)
            } else {
                de <- get_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
                    class = "Daylighting:DELight:Controls", field = 2L)
                meta2[de, on = c("delight_name" = "object_name"), zone_name := i.value_chr]
                set(meta2, NULL, "delight_name", NULL)
            }

            # vertices
            vertices2 <- dt[!J(1:2), on = "field_index", by = "object_id",
                list(index = field_index - 2L, value_num)]
            vertices2 <- dcast.data.table(vertices2, object_id ~ index, value.var = "value_num")
            setnames(vertices2, c("id", "x", "y", "z"))
            set(vertices2, NULL, "index", 1L)
            setcolorder(vertices2, c("id", "index"))

            meta <- rbindlist(list(meta, meta2))
            vertices <- rbindlist(list(vertices, vertices2))
        }
    }

    list(meta = meta, vertices = vertices)
}
# }}}

# convert_geom {{{
convert_geom <- function (idf, geoms = NULL, type = c("surface", "subsurface", "shading")) {
    assert_subset(type, c("surface", "subsurface", "shading"), empty.ok = FALSE)

    if (is.null(geoms)) geoms <- extract_geom(idf)

    surf <- list()
    if ("surface" %chin% type) {
        surf <- convert_geom_surface_simple(idf, list(meta = geoms$surface, vertices = geoms$vertices))
    }
    subsurf <- list()
    if ("subsurface" %chin% type) {
        subsurf <- convert_geom_subsurface_simple(idf, list(meta = geoms$subsurface, vertices = geoms$vertices))
    }
    shading <- list()
    if ("shading" %chin% type) {
        shading <- convert_geom_shading_simple(idf, list(meta = geoms$shading, vertices = geoms$vertices))
    }

    object <- rbindlist(list(surf$object, subsurf$object, shading$object))
    if (!nrow(object)) return(list(idf = idf, map = data.table()))

    value <- rbindlist(list(surf$value, subsurf$value, shading$value))
    map <- rbindlist(list(surf$map, subsurf$map, shading$map))

    object_set <- object[object_id > 0L]
    value_set <- value[object_id > 0L]

    object_add <- object[object_id < 0L]
    value_add <- value[object_id < 0L]

    set <- set_idf_object(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(), object_set, value_set, replace = TRUE)

    # log
    get_priv_env(idf)$log_add_order(c(set$changed, set$updated))
    get_priv_env(idf)$log_unsaved()
    get_priv_env(idf)$log_new_uuid()
    get_priv_env(idf)$update_idf_env(set)

    if (nrow(object_add)) {
        add <- add_idf_object(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(), object_add, value_add)

        # log
        get_priv_env(idf)$log_add_order(c(add$changed, add$updated))
        get_priv_env(idf)$log_unsaved()
        get_priv_env(idf)$log_new_uuid()
        get_priv_env(idf)$update_idf_env(add)
    }

    # update actual object id for newly add objects in the mapping
    id <- get_idf_object(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
        class = map[new_id < 0L, new_class], object = map[new_id < 0L, new_name]
    )$object_id
    map[new_id < 0L, new_id := id]

    list(idf = idf, map = map)
}
# }}}

# convert_geom_simple {{{
convert_geom_simple <- function (idf, geom, target_class, field_keep = NULL, first_vertex) {
    if (!nrow(geom$meta)) return(list(object = data.table(), value = data.table(), map = data.table()))

    # init object table {{{
    obj <- init_idf_object(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
        rep(target_class, nrow(geom$meta)), id = FALSE, name = FALSE
    )
    # restore original comments
    ori_obj <- get_idf_object(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(), object = geom$meta$id)
    set(obj, NULL, c("object_id", "object_name", "object_name_lower", "comment"),
        list(ori_obj$object_id, ori_obj$object_name, ori_obj$object_name_lower, ori_obj$comment))

    # store mapping
    ori_map <- fast_subset(ori_obj, c("object_id", "object_name", "class_name"))
    new_map <- fast_subset(obj, c("object_id", "object_name", "class_name"))
    map <- setnames(cbind(ori_map, new_map), c("ori_id", "ori_name", "ori_class", "new_id", "new_name", "new_class"))
    # }}}

    # init value table {{{
    # make sure enough fields
    num <- geom$vertices[, by = "id", .N]
    val <- init_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
        rep(target_class, nrow(num)), first_vertex - 1L + num$N * 3L,
        id = FALSE, complete = TRUE
    )
    set(val, NULL, "field_in", NULL)

    # update object id and name
    val[obj, on = "rleid", `:=`(object_id = i.object_id, object_name = i.object_name)]

    # use the original value id for handling references
    if (!is.null(field_keep)) {
        ori_val <- get_idf_value(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(), object = num$id, all = TRUE)[
            J(field_keep), on = "field_name", nomatch = NULL]
        val[ori_val, on = c("object_id", "field_name"), `:=`(value_id = i.value_id, value_chr = i.value_chr)]
    }

    # assign new character values
    new_val <- melt.data.table(geom$meta, id.vars = c("id", "name", "class"),
        variable.name = "field_in", variable.factor = FALSE, value.name = "value_chr")
    set(val, NULL, "field_in", lower_name(val$field_name))
    val[new_val, on = c("object_id" = "id", "field_in"), value_chr := i.value_chr]
    set(val, NULL, "field_in", NULL)

    # assign new vertices
    vert <- melt.data.table(geom$vertices, id.vars = c("id", "index"))
    setorderv(vert, c("id", "index"))
    vert[, field_index := first_vertex - 1L + seq_len(.N), by = "id"]
    val[vert, on = c("object_id" = "id", "field_index"),
        `:=`(value_chr = as.character(i.value), value_num = i.value)]

    # set value id for all new fields to negative
    val[J(NA_integer_), on = "value_id", value_id := -seq_len(.N)]
    # }}}

    list(object = obj, value = val, map = map)
}
# }}}

# convert_geom_surface_simple {{{
convert_geom_surface_simple <- function (idf, geom = NULL) {
    if (is.null(geom)) {
        geom <- extract_geom_surface_simple(idf)
    } else {
        if (!any(is_simple <- !stri_endswith_fixed(geom$meta$class, "Detailed"))) {
            geom <- list(meta = data.table(), vertices = data.table())
        } else {
            geom$meta <- geom$meta[is_simple]
            geom$vertices <- geom$vertices[J(geom$meta$id[is_simple]), on = "id", nomatch = NULL]
        }
    }
    convert_geom_simple(idf, geom, "BuildingSurface:Detailed",
        c("Name", "Construction Name", "Zone Name", "Outside Boundary Condition Object"),
        first_vertex = 11L
    )
}
# }}}

# convert_geom_subsurface_simple {{{
convert_geom_subsurface_simple <- function (idf, geom = NULL) {
    if (is.null(geom)) {
        geom <- extract_geom_subsurface_simple(idf)
    } else {
        if (!any(is_simple <- geom$meta$class != "FenestrationSurface:Detailed")) {
            geom <- list(meta = data.table(), vertices = data.table())
        } else {
            geom$meta <- geom$meta[is_simple]
            geom$vertices <- geom$vertices[J(geom$meta$id[is_simple]), on = "id", nomatch = NULL]
        }
    }
    convert_geom_simple(idf, geom, "FenestrationSurface:Detailed",
        c("Name", "Construction Name", "Building Surface Name", "Frame and Divider Name", "Outside Boundary Condition Object"),
        first_vertex = if (idf$version() < 9.0) 11L else 10L
    )
}
# }}}

# convert_geom_shading_simple {{{
convert_geom_shading_simple <- function (idf, geom = NULL) {
    if (is.null(geom)) {
        geom <- extract_geom_shading_simple(idf)
    } else {
        if (!any(is_simple <- !stri_endswith_fixed(geom$meta$class, "Detailed"))) {
            geom <- list(meta = data.table(), vertices = data.table())
        } else {
            geom$meta <- geom$meta[is_simple]
            geom$vertices <- geom$vertices[J(geom$meta$id[is_simple]), on = "id", nomatch = NULL]
        }
    }

    if (!nrow(geom$meta)) return(list(object = data.table(), value = data.table(), map = data.table()))

    meta <- split(geom$meta, by = "surface_type")

    site <- list()
    bldg <- list()
    zone <- list()

    if ("SiteShading" %chin% names(meta)) {
        site <- convert_geom_simple(idf,
            list(meta = meta$SiteShading, vertices = geom$vertices[J(meta$SiteShading$id), on = "id"]),
            "Shading:Site:Detailed", c("Name", "Transmittance Schedule Name"),
            first_vertex = 4L
        )
    }
    if ("BuildingShading" %chin% names(meta)) {
        bldg <- convert_geom_simple(idf,
            list(meta = meta$BuildingShading, vertices = geom$vertices[J(meta$BuildingShading$id), on = "id"]),
            "Shading:Building:Detailed", c("Name", "Transmittance Schedule Name"),
            first_vertex = 4L
        )
    }
    if ("ZoneShading" %chin% names(meta)) {
        overhang <- list()
        fin <- list()

        if (any(c("Shading:Overhang", "Shading:Overhang:Projection") %chin% meta$ZoneShading$class)) {
            overhang$meta <- meta$ZoneShading[J(c("Shading:Overhang", "Shading:Overhang:Projection")), on = "class", nomatch = NULL]
            overhang$vertices <- geom$vertices[J(overhang$meta$id), on = "id"]

            overhang <- convert_geom_simple(idf, overhang, "Shading:Zone:Detailed", "Name", first_vertex = 5L)
        }

        # should handle fin shading separately
        if (any(c("Shading:Fin", "Shading:Fin:Projection") %chin% meta$ZoneShading$class)) {
            fin$meta <- meta$ZoneShading[J(c("Shading:Fin", "Shading:Fin:Projection")), on = "class", nomatch = NULL]
            fin$vertices <- geom$vertices[J(fin$meta$id), on = "id"]

            fin_left <- convert_geom_simple(idf,
                list(meta = fin$meta[id > 0L], vertices = fin$vertices[id > 0L]),
                "Shading:Zone:Detailed", "Name", first_vertex = 5L
            )
            set(fin_left$object, NULL, c("object_name", "object_name_lower"),
                list(paste(fin_left$object$object_name, "Left"),
                    paste(fin_left$object$object_name_lower, "left")
                )
            )
            # in case adding a suffix 'Left' can introduce name conflict
            fin_left$object <- make_idf_object_name(
                get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
                fin_left$object, use_old = TRUE
            )
            # update name
            set(fin_left$object, NULL, c("object_name", "object_name_lower"), NULL)
            setnames(fin_left$object, c("new_object_name", "new_object_name_lower"), c("object_name", "object_name_lower"))
            fin_left$value[fin_left$object, on = "object_id", object_name := i.object_name]
            fin_left$value[J(1L), on = "field_index", value_chr := object_name]
            set(fin_left$map, NULL, "new_name", fin_left$object$object_name)

            fin_right <- convert_geom_simple(idf,
                list(meta = fin$meta[id < 0L][, id := -id], vertices = fin$vertices[id < 0L][, id := -id]),
                "Shading:Zone:Detailed", "Name", first_vertex = 5L
            )
            set(fin_right$object, NULL, c("object_name", "object_name_lower"),
                list(paste(fin_right$object$object_name, "Right"),
                    paste(fin_right$object$object_name_lower, "right")
                )
            )
            # in case adding a suffix 'Right' can introduce name conflict
            fin_right$object <- make_idf_object_name(
                get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(),
                fin_right$object, use_old = TRUE
            )
            # update name
            set(fin_right$object, NULL, c("object_name", "object_name_lower"), NULL)
            setnames(fin_right$object, c("new_object_name", "new_object_name_lower"), c("object_name", "object_name_lower"))
            fin_right$value[fin_right$object, on = "object_id", object_name := i.object_name]
            fin_right$value[J(1L), on = "field_index", value_chr := object_name]
            set(fin_right$map, NULL, "new_name", fin_right$object$object_name)

            set(fin_right$object, NULL, "object_id", -fin_right$object$object_id)
            set(fin_right$value, NULL, "object_id", -fin_right$value$object_id)
            set(fin_right$map, NULL, "new_id", -fin_right$map$new_id)

            fin$object <- rbindlist(list(fin_left$object, fin_right$object))
            fin$value <- rbindlist(list(fin_left$value, fin_right$value))
            fin$map <- rbindlist(list(fin_left$map, fin_right$map))
        }

        zone$object <- rbindlist(list(overhang$object, fin$object), use.names = TRUE)
        zone$value <- rbindlist(list(overhang$value, fin$value))
        zone$map <- rbindlist(list(overhang$map, fin$map))
    }

    list(object = rbindlist(list(site$object, bldg$object, zone$object), use.names = TRUE),
         value = rbindlist(list(site$value, bldg$value, zone$value)),
         map = rbindlist(list(site$map, bldg$map, zone$map))
    )
}
# }}}

# subset_geom {{{
subset_geom <- function (geoms, type = c("all", "floor", "wall", "roof", "window", "door", "shading", "daylighting"),
                         zone = NULL, surface = NULL) {
    assert_subset(type, c("all", "floor", "wall", "roof", "window", "door", "shading", "daylighting"))
    zone <- assert_valid_type(zone, "Zone ID|Name", null.ok = TRUE)
    surface <- assert_valid_type(surface, "Surface ID|Name", null.ok = TRUE)

    # subset geoms by components {{{
    if (!length(type)) {
        geoms$surface <- geoms$surface[0L]
        geoms$subsurface <- geoms$surface[0L]
        geoms$shading <- geoms$surface[0L]
        geoms$daylighting_point <- geoms$surface[0L]
        geoms$vertices <- geoms$vertices[0L]
        if (has_names(geoms, "vertices2")) geoms$vertices2 <- geoms$vertices2[0L]
        return(geoms)
    }

    ALL_COMP <- c("floor", "wall", "roof", "window", "door", "shading", "daylighting")
    if ("all" %chin% type) type <- ALL_COMP
    dshow <- setdiff(ALL_COMP, type)
    if (length(dshow)) {
        dshow_surf <- c("Floor", "Wall", "Roof")[c("floor", "wall", "roof") %chin% dshow]
        dshow_subsurf <- c("Window", "Door")[c("window", "door") %chin% dshow]
        if (nrow(geoms$surface)) {
            if ("Roof" %chin% dshow_surf) dshow_surf <- c(dshow_surf, "Ceiling")
            if (length(dshow_surf)) {
                geoms$surface <- geoms$surface[!J(dshow_surf), on = "surface_type"]
            }
        }

        if (nrow(geoms$subsurface)) {
            if ("Door" %chin% dshow_subsurf) dshow_subsurf <- c(dshow_subsurf, "GlassDoor")
            if (length(dshow_subsurf)) {
                geoms$subsurface <- geoms$subsurface[!J(dshow_subsurf), on = "surface_type"]
            }
        }

        if (nrow(geoms$shading) && "shading" %chin% dshow) {
            geoms$shading <- geoms$shading[0L]
        }
    }
    # }}}

    # subset geoms by zones {{{
    if (!is.null(zone)) {
        if (is.integer(zone)) {
            geoms$zone <- geoms$zone[J(zone), on = "id", nomatch = NULL]
        } else {
            set(geoms$zone, NULL, "name_lower", stri_trans_tolower(geoms$zone$name))
            geoms$zone <- geoms$zone[J(stri_trans_tolower(zone)), on = "name_lower", nomatch = NULL]
            set(geoms$zone, NULL, "name_lower", NULL)
        }

        if (!nrow(geoms$zone) || !nrow(geoms$surface)) {
            geoms$surface <- geoms$surface[0L]
            geoms$subsurface <- geoms$subsurface[0L]
            geoms$shading <- geoms$shading[0L]
            geoms$daylighting_point <- geoms$daylighting_point[0L]
        } else {
            geoms$surface <- geoms$surface[J(geoms$zone$name), on = "zone_name", nomatch = NULL]

            if (nrow(geoms$subsurface)) {
                geoms$subsurface <- geoms$subsurface[J(geoms$surface$name), on = "building_surface_name", nomatch = NULL]
            }

            if (nrow(geoms$shading)) {
                geoms$shading <- geoms$shading[J(geoms$surface$name), on = "base_surface_name", nomatch = NULL]
            }
        }
    }
    # }}}

    # subset geoms by surfaces {{{
    if (!is.null(surface)) {
        if (is.integer(surface)) {
            on <- "id"
        } else {
            surface <- stri_trans_tolower(surface)
            on <- "name_lower"
        }
        if (nrow(geoms$surface)) {
            if (is.character(surface)) {
                set(geoms$surface, NULL, "name_lower", stri_trans_tolower(geoms$surface$name))
            }
            geoms$surface <- geoms$surface[J(surface), on = on, nomatch = NULL]
            if (is.character(surface)) {
                set(geoms$surface, NULL, "name_lower", NULL)
            }
        }
        if (nrow(geoms$subsurface)) {
            if (is.character(surface)) {
                set(geoms$subsurface, NULL, "name_lower", stri_trans_tolower(geoms$subsurface$name))
            }
            # get name of sufaces whose subsurfaces have been removed
            geoms$subsurface <- geoms$subsurface[J(surface), on = on, nomatch = NULL]
            if (is.character(surface)) {
                set(geoms$subsurface, NULL, "name_lower", NULL)
            }
        }
        if (nrow(geoms$shading)) {
            if (is.character(surface)) {
                set(geoms$shading, NULL, "name_lower", stri_trans_tolower(geoms$shading$name))
                geoms$shading <- geoms$shading[J(surface), on = on, nomatch = NULL]
                set(geoms$shading, NULL, "name_lower", NULL)
            }
        }
    }
    # }}}

    # subset daylighting points by zones {{{
    if ((!is.null(zone) || !is.null(surface)) && NROW(geoms$daylighting_point)) {
        if (!nrow(geoms$surface)) {
            geoms$daylighting_point <- geoms$daylighting_point[0L]
        } else if (nrow(geoms$daylighting_point)) {
            geoms$daylighting_point <- geoms$daylighting_point[
                J(geoms$surface$zone_name), on = "zone_name", nomatch = NULL]
        }
    }
    # }}}

    vid <- unique(c(geoms$surface$id, geoms$subsurface$id, geoms$shading$id, geoms$daylighting_point$id, geoms$hole$id))
    if (!length(vid)) {
        geoms$vertices <- geoms$vertices[0L]
        if (NROW(geoms$vertices2)) geoms$vertices2 <- geoms$vertices2[0L]
    } else if (nrow(geoms$vertices)) {
        geoms$vertices <- geoms$vertices[J(vid), on = "id"]
        if (NROW(geoms$vertices2)) geoms$vertices2 <- geoms$vertices2[J(vid), on = "id"]
    }
    geoms
}
# }}}

# align_coord_system {{{
align_coord_system <- function (geoms, detailed = NULL, simple = NULL, daylighting = NULL) {
    assert_choice(detailed, c("absolute", "relative"), null.ok = TRUE)
    assert_choice(simple, c("absolute", "relative"), null.ok = TRUE)
    assert_choice(daylighting, c("absolute", "relative"), null.ok = TRUE)

    if (is.null(detailed) && is.null(simple) && is.null(daylighting)) return(geoms)
    if (!nrow(geoms$zone)) return(geoms)

    add_zone_name(geoms)

    # init
    empty <- data.table(id = integer(), zone_name = character(), mult = integer())
    if (nrow(geoms$surface)) {
        set(geoms$surface, NULL, "mult", 0L)
        on.exit(set(geoms$surface, NULL, "mult", NULL), add = TRUE)
    } else {
        geoms$surface <- empty
        on.exit(geoms$surface <- data.table(), add = TRUE)
    }
    if (nrow(geoms$subsurface)) {
        set(geoms$subsurface, NULL, "mult", 0L)
        on.exit(set(geoms$subsurface, NULL, c("mult", "zone_name"), NULL), add = TRUE)
    } else {
        geoms$subsurface <- empty
        on.exit(geoms$subsurface <- data.table(), add = TRUE)
    }
    if (nrow(geoms$shading)) {
        set(geoms$shading, NULL, "mult", 0L)
        on.exit(set(geoms$shading, NULL, c("mult", "zone_name"), NULL), add = TRUE)
    } else {
        geoms$shading <- empty
        on.exit(geoms$shading <- data.table(), add = TRUE)
    }
    if (nrow(geoms$daylighting_point)) {
        set(geoms$daylighting_point, NULL, "mult", 0L)
        on.exit(set(geoms$daylighting_point, NULL, "mult", NULL), add = TRUE)
    } else {
        geoms$daylighting_point <- empty
        on.exit(geoms$daylighting_point <- data.table(), add = TRUE)
    }

    # indicates whether detailed/simple class names have been checked
    has_checked <- FALSE
    has_changed <- FALSE

    if (!is.null(detailed) && detailed != geoms$rules$coordinate_system) {
        has_checked <- TRUE
        has_changed <- TRUE

        is_det_surf <- stri_endswith_fixed(geoms$surface$class, "Detailed")
        is_det_subsurf <- stri_endswith_fixed(geoms$subsurface$class, "Detailed")
        is_det_shading <- stri_endswith_fixed(geoms$shading$class, "Detailed")

        # update rules
        geoms$rules$coordinate_system <- detailed

        # -1 for absolute to relative and 1 for relative to absolute
        mult <- if (detailed == "relative") -1L else 1L

        if (any(is_det_surf)) {
            set(geoms$surface, which(is_det_surf), "mult", mult)
        }
        if (any(is_det_subsurf) && nrow(geoms$surface)) {
            set(geoms$subsurface, which(is_det_subsurf), "mult", mult)
        }
        if (any(is_det_shading) && nrow(geoms$surface)) {
            set(geoms$shading, which(is_det_shading), "mult", mult)
        }
    }

    if (!is.null(simple) && simple != geoms$rules$rectangular_surface_coordinate_system) {
        has_changed <- TRUE
        if (has_checked) {
            is_sim_surf <- !is_det_surf
            is_sim_subsurf <- !is_det_subsurf
            is_sim_shading <- !is_det_shading
        } else {
            is_sim_surf <- !stri_endswith_fixed(geoms$surface$class, "Detailed")
            is_sim_subsurf <- !stri_endswith_fixed(geoms$subsurface$class, "Detailed")
            is_sim_shading <- !stri_endswith_fixed(geoms$shading$class, "Detailed")
        }

        # update rules
        geoms$rules$rectangular_surface_coordinate_system <- simple

        # -1 for absolute to relative and 1 for relative to absolute
        mult <- if (simple == "relative") -1L else 1L

        if (any(is_sim_surf)) {
            set(geoms$surface, which(is_sim_surf), "mult", mult)
        }
        if (any(is_sim_subsurf) && nrow(geoms$surface)) {
            set(geoms$subsurface, which(is_sim_subsurf), "mult", mult)
        }
        if (any(is_sim_shading) && nrow(geoms$surface)) {
            set(geoms$shading, which(is_sim_shading), "mult", mult)
        }
    }

    if (!is.null(daylighting) && daylighting != geoms$rules$daylighting_reference_point_coordinate_system) {
        has_changed <- TRUE
        # update rules
        geoms$rules$daylighting_reference_point_coordinate_system <- daylighting

        if (nrow(geoms$daylighting_point)) {
            # -1 for absolute to relative and 1 for relative to absolute
            mult <- if (daylighting == "relative") -1L else 1L
            set(geoms$daylighting_point, NULL, "mult", mult)
        }
    }

    if (!has_changed) return(geoms)

    # combine
    meta <- rbindlist(list(
        fast_subset(geoms$surface, names(empty)),
        fast_subset(geoms$subsurface, names(empty)),
        fast_subset(geoms$shading, names(empty)),
        fast_subset(geoms$daylighting_point, names(empty))
    ))

    # add data to the vertices table
    add_joined_cols(meta, geoms$vertices, "id", c("zone_name", "mult"))

    # transform
    geoms$vertices[geoms$zone, on = c("zone_name" = "name"),
        c("x", "y", "z") := {
            x <- x + mult * i.x
            y <- y + mult * i.y
            z <- z + mult * i.z

            deg <- mult * (i.dir_relative_north)

            # rotate by z-axis
            # NOTE: use formula specific for z-rotation, avoid grouping to speed up
            rot <- deg != 0.0
            sina <- sin(deg_to_rad(deg[rot]))
            cosa <- cos(deg_to_rad(deg[rot]))

            x[rot] <- x[rot] *  cosa + y[rot] * sina
            y[rot] <- x[rot] * -sina + y[rot] * cosa
            list(x, y, z)
        }
    ]

    set(geoms$vertices, NULL, c("zone_name", "mult"), NULL)

    geoms
}
# }}}

# set_geom_vertices {{{
set_geom_vertices <- function (idf, geom, digits = NULL) {
    if (!NROW(geom$meta)) return(idf)

    # only works for detailed geometry classes
    map <- data.table(
        class = c(
            "Zone",
            "BuildingSurface:Detailed", "FenestrationSurface:Detailed",
            "Shading:Site:Detailed", "Shading:Building:Detailed",
            "Shading:Zone:Detailed"),
        first_vertex = c(
            3L,
            11L, if (idf$version() < 9.0) 11L else 10L,
            4L, 4L, 5L
        )
    )
    meta <- geom$meta[map, on = "class", nomatch = NULL, list(id, first_vertex)]
    if (!nrow(meta)) return(idf)

    vert <- geom$vertices[meta, on = "id", nomatch = NULL]
    if (!is.null(digits)) {
        assert_count(digits)
        vert[, c("x", "y", "z") := lapply(.SD, round, digits = digits), .SDcols = c("x", "y", "z")]
    }

    vert <- melt.data.table(vert, id.vars = c("id", "index", "first_vertex"))
    setorderv(vert, c("id", "index"))
    vert[, field_index := first_vertex - 1L + seq_len(.N), by = "id"]

    add_field_property(get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env()$value, "field_index")
    get_priv_env(idf)$idf_env()$value[vert, on = c("object_id" = "id", "field_index"),
        `:=`(value_chr = as.character(i.value), value_num = i.value)]
    set(get_priv_env(idf)$idf_env()$value, NULL, "field_index", NULL)

    # log
    get_priv_env(idf)$log_new_order(unique(vert$id))
    get_priv_env(idf)$log_unsaved()
    get_priv_env(idf)$log_new_uuid()

    idf
}
# }}}

# add_zone_name {{{
add_zone_name <- function (geoms) {
    if (!nrow(geoms$surface)) return(geoms)

    if (nrow(geoms$subsurface)) {
        geoms$subsurface[geoms$surface, on = c("building_surface_name" = "name"), zone_name := i.zone_name]
    }
    if (nrow(geoms$shading)) {
        geoms$shading[geoms$surface, on = c("base_surface_name" = "name"), zone_name := i.zone_name]
    }
    geoms
}
# }}}

# del_zone_name {{{
del_zone_name <- function (geoms) {
    if (nrow(geoms$subsurface) && has_names(geoms$subsurface, "zone_name")) {
        set(geoms$subsurface, NULL, "zone_name", NULL)
    }
    if (nrow(geoms$shading) && has_names(geoms$shading, "zone_name")) {
        set(geoms$shading, NULL, "zone_name", NULL)
    }
    geoms
}
# }}}

# remove_incomplete_vertices {{{
remove_incomplete_vertices <- function (vertices) {
    # only keep rows that have valid x, y, z values from the beginning
    vertices_valid <- na.omit(vertices, cols = c("x", "y", "z"))
    if (nrow(vertices_valid) != nrow(vertices)) {
        vertices <- vertices[, by = "id", {
            index <- index[index == seq_len(.N)]
            list(index = index, x = x[index], y = y[index], z = z[index])
        }]
    }
    vertices
}
# }}}

# reverse_idf_detailed_vertices {{{
reverse_idf_detailed_vertices <- function (idf, geom_class = NULL) {
    if (is.null(geom_class)) geom_class <- get_geom_class(idf)

    detailed <- geom_class[J("Detailed"), on = "subtype", nomatch = NULL]
    if (!nrow(detailed)) return(idf)

    # only works for detailed geometry classes
    map <- data.table(
        class = c("BuildingSurface:Detailed", "FenestrationSurface:Detailed",
            "Shading:Site:Detailed", "Shading:Building:Detailed",
            "Shading:Zone:Detailed"),
        first_vertex = c(11L, if (idf$version() < 9.0) 11L else 10L,
            4L, 4L, 5L
        )
    )
    detailed <- detailed[map, on = "class", nomatch = NULL, list(class, first_vertex)]

    dt <- idf$to_table(class = detailed$class)
    set(dt, NULL, "field_index", dt$index)

    dt <- dt[detailed, on = list(class, field_index >= first_vertex)][,
        by = "id", value := {
            i <- (index - field_index + 1L) %% 3L
            value[i == 1L] <- rev(value[i == 1L])
            value[i == 2L] <- rev(value[i == 2L])
            value[i == 0L] <- rev(value[i == 0L])
            value
        }
    ]

    idf$update(dt, .default = FALSE, .empty = TRUE)

    idf
}
# }}}

# apply_upper_left_corner_rule {{{
apply_upper_left_corner_rule <- function (vertices) {
    trans <- align_face(vertices)

    vertices[trans, on = "id", by = .EACHI,
        c("x", "y", "z") := {
            if (.N < 3L) {
                list(x, y, z)
            } else {
                trans_inv <- solve(i.trans[[1L]])
                align_vert <- apply(matrix(c(x, y, z, rep(1.0, .N)), ncol = 4L), 1, function (x) trans_inv %*% x)[1:3,]

                if (any(align_vert[3L, ] >= 0.001)) abort("invalid align transformation")

                max_y <- -Inf
                min_x <- Inf
                ulc_index <- 1L
                for (i in seq_len(.N)) {
                    if (max_y < align_vert[2, i] || (max_y < align_vert[2, i] + 1E-5 && min_x > align_vert[1, i])) {
                        ulc_index <- i
                        max_y <- align_vert[2, i]
                        min_x <- align_vert[1, i]
                    }
                }

                if (ulc_index == 1L) {
                    list(x, y, z)
                } else {
                    i <- c(ulc_index:.N, 1:(ulc_index - 1L))
                    list(x[i], y[i], z[i])
                }
            }
        }
    ]

    vertices
}
# }}}

# get_vertices_from_specs {{{
get_vertices_from_specs <- function (azimuth, tilt, length, height, x0, y0, z0) {
    cos_azimuth <- cos(deg_to_rad(azimuth))
    sin_azimuth <- sin(deg_to_rad(azimuth))
    cos_tilt <- cos(deg_to_rad(tilt))
    sin_tilt <- sin(deg_to_rad(tilt))

    x_init <- list(0.0, 0.0, length, length)
    y_init <- list(height, 0.0, 0.0, height)

    x <- mapply(
        function (x, y) x0 - cos_azimuth * x - cos_tilt * sin_azimuth * y,
        x = x_init, y = y_init, SIMPLIFY = FALSE
    )

    y <- mapply(
        function (x, y) y0 + sin_azimuth * x - cos_tilt * cos_azimuth * y,
        x = x_init, y = y_init, SIMPLIFY = FALSE
    )

    z <- lapply(y_init, function (y) z0 + sin_tilt * y)

    list(x = x, y = y, z = z)
}
# }}}

# align_face {{{
align_face <- function (vertices) {
    norm <- get_outward_normal(vertices)
    # get z' with outward normal
    norm[, by = "id", trans := list(list(align_z_prime(x, y, z)))]

    dt_trans <- setDT(data.table::transpose(norm$trans))
    dt_inv_trans <- setDT(data.table::transpose(lapply(norm$trans, solve.default)))
    set(dt_trans, NULL, "id", norm$id)
    set(dt_inv_trans, NULL, "id", norm$id)

    add_joined_cols(dt_inv_trans, vertices, "id", sprintf("V%i", 1:16))
    vertices[, `:=`(
        inv_x = x * V1 + y * V5 + z * V9  + V13,
        inv_y = x * V2 + y * V6 + z * V10 + V14,
        inv_z = x * V3 + y * V7 + z * V11 + V15
    )]
    vert <- vertices[, by = "id", list(inv_x = min(inv_x), inv_y = min(inv_y), inv_z = min(inv_z))]
    # clean
    set(vertices, NULL, setdiff(names(vertices), c("id", "index", "x", "y", "z")), NULL)

    add_joined_cols(dt_trans, vert, "id", sprintf("V%i", 1:16))

    # 4 X 4 matrix multiplication
    vert[, `:=`(
        V13 = inv_x * V1 + inv_y * V5 + inv_z * V9  + V13,
        V14 = inv_x * V2 + inv_y * V6 + inv_z * V10 + V14,
        V15 = inv_x * V3 + inv_y * V7 + inv_z * V11 + V15,
        V16 = inv_x * V4 + inv_y * V8 + inv_z * V12 + V16
    )]

    vert[, by = "id", list(
        trans = list(matrix(
            c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16),
            ncol = 4L
        ))
    )]
}
# }}}

# get_newall_vector {{{
get_newall_vector <- function (vertices) {
    # Reference: https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal#Newell.27s_Method
    vertices[, by = "id", {
        nx <- seq_len(.N) %% .N + 1L
        # calculate the distance from the origin to the first point on each polygon
        list(
            x = sum((z + z[nx]) * (y - y[nx])),
            y = sum((x + x[nx]) * (z - z[nx])),
            z = sum((y + y[nx]) * (x - x[nx]))
        )
    }]
}
# }}}

# get_outward_normal {{{
get_outward_normal <- function (vertices) {
    # calculate normal vector of surfaces using Newell Method
    # Reference: https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal#Newell.27s_Method
    vertices <- get_newall_vector(vertices)
    vertices[, by = "id", c("x", "y", "z") := as.list(normalize(c(x, y, z)))]
    vertices
}
# }}}

# align_z_prime {{{
align_z_prime <- function (x, y, z) {
    axis_x <- c(1, 0, 0)
    axis_y <- c(0, 1, 0)
    axis_z <- c(0, 0, 1)
    axis_x_neg <- c(-1, 0, 0)

    zp <- normalize(c(x, y, z))

    dot_zp <- as.numeric(zp %*% axis_z)

    if (is.na(dot_zp) || is.nan(dot_zp)) {
        return(matrix(rep(NA_real_, 16L), ncol = 4L))
    }

    # check if face normal is up or down
    # not facing up or down, set yPrime along zAxis
    if (abs(dot_zp) < 0.99) {
        yp <- normalize(axis_z - as.numeric(zp %*% axis_z) * zp)
        xp <- crossproduct(yp, zp)
    } else {
        xp <- normalize(axis_x_neg - as.numeric(zp %*% axis_x_neg) * zp)
        yp <- crossproduct(zp, xp)
    }

    trans <- diag(nrow = 4L)
    trans[1:3, 1] <- xp
    trans[1:3, 2] <- yp
    trans[1:3, 3] <- zp
    trans
}
# }}}

# crossproduct {{{
crossproduct <- function(v1, v2) {
    v1[c(2L, 3L, 1L)] * v2[c(3L, 1L, 2L)] - v1[c(3L, 1L, 2L)] * v2[c(2L, 3L, 1L)]
}
# }}}

# normalize {{{
normalize <- function(v) v / sqrt(sum(v^2))
# }}}

# deg_to_rad {{{
deg_to_rad <- function (x) x / 180 * pi
# }}}

# rad_to_deg {{{
rad_to_deg <- function (x) x / pi * 180
# }}}

# get_angle {{{
get_angle <- function (v1, v2) {
    normalize(v1) %*% normalize(v2)
    d <- rad_to_deg(acos(normalize(v1) %*% normalize(v2)))[1]
    if (v1[[1]] < 0) d <- d + 180
    d
}
# }}}

# get_tilt {{{
get_tilt <- function (out_norm) {
    get_angle(out_norm, c(0, 0, 1))
}
# }}}

# get_azimuth {{{
get_azimuth <- function (out_norm) {
    get_angle(out_norm, c(0, 1, 0))
}
# }}}

# get_area {{{
get_area <- function (newall) {
    sqrt(sum(newall ^ 2)) / 2.0
}
# }}}
