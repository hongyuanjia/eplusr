# COLOR_MAP {{{
# Reference: 'openstudio\openstudiocore\ruby\openstudio\sketchup_plugin\lib\interfaces\MaterialsInterface.rb'
COLOR_MAP <- list(
    surface_type = c(
        undefined = grDevices::rgb(255/255, 255/255, 255/255, 1),
        normalmaterial = grDevices::rgb(255/255, 255/255, 255/255, 1),
        normalmaterial_ext = grDevices::rgb(255/255, 255/255, 255/255, 1),
        normalmaterial_int = grDevices::rgb(255/255, 0/255, 0/255, 1),
        floor = grDevices::rgb(128/255, 128/255, 128/255, 1),
        floor_ext = grDevices::rgb(128/255, 128/255, 128/255, 1),
        floor_int = grDevices::rgb(191/255, 191/255, 191/255, 1),
        wall = grDevices::rgb(204/255, 178/255, 102/255, 1),
        wall_ext = grDevices::rgb(204/255, 178/255, 102/255, 1),
        wall_int = grDevices::rgb(235/255, 226/255, 197/255, 1),
        roof = grDevices::rgb(153/255, 76/255, 76/255, 1),
        roof_ext = grDevices::rgb(153/255, 76/255, 76/255, 1),
        roof_int = grDevices::rgb(202/255, 149/255, 149/255, 1),
        ceiling = grDevices::rgb(153/255, 76/255, 76/255, 1),
        ceiling_ext = grDevices::rgb(153/255, 76/255, 76/255, 1),
        ceiling_int = grDevices::rgb(202/255, 149/255, 149/255, 1),
        window = grDevices::rgb(102/255, 178/255, 204/255, 0.6),
        window_ext = grDevices::rgb(102/255, 178/255, 204/255, 0.6),
        window_int = grDevices::rgb(192/255, 226/255, 235/255, 0.6),
        door = grDevices::rgb(153/255, 133/255, 76/255, 1),
        door_ext = grDevices::rgb(153/255, 133/255, 76/255, 1),
        door_int = grDevices::rgb(202/255, 188/255, 149/255, 1),
        glassdoor = grDevices::rgb(153/255, 133/255, 76/255, 1),
        glassdoor_ext = grDevices::rgb(153/255, 133/255, 76/255, 1),
        glassdoor_int = grDevices::rgb(202/255, 188/255, 149/255, 1),
        siteshading = grDevices::rgb(75/255, 124/255, 149/255, 1),
        siteshading_ext = grDevices::rgb(75/255, 124/255, 149/255, 1),
        siteshading_int = grDevices::rgb(187/255, 209/255, 220/255, 1),
        buildingshading = grDevices::rgb(113/255, 76/255, 153/255, 1),
        buildingshading_ext = grDevices::rgb(113/255, 76/255, 153/255, 1),
        buildingshading_int = grDevices::rgb(216/255, 203/255, 229/255, 1),
        spaceshading = grDevices::rgb(76/255, 110/255, 178/255, 1),
        spaceshading_ext = grDevices::rgb(76/255, 110/255, 178/255, 1),
        spaceshading_int = grDevices::rgb(183/255, 197/255, 224/255, 1),
        interiorpartitionsurface = grDevices::rgb(158/255, 188/255, 143/255, 1),
        interiorpartitionsurface_ext = grDevices::rgb(158/255, 188/255, 143/255, 1),
        interiorpartitionsurface_int = grDevices::rgb(213/255, 226/255, 207/255, 1)
    ),
    boundary = c(
        surface = grDevices::rgb(0/255, 153/255, 0/255),
        adiabatic = grDevices::rgb(255/255, 0/255, 0/255),
        zone = grDevices::rgb(255/255, 0/255, 0/255),
        outdoors = grDevices::rgb(163/255, 204/255, 204/255),
        outdoors_sun = grDevices::rgb(40/255, 204/255, 204/255),
        outdoors_wind = grDevices::rgb(9/255, 159/255, 162/255),
        outdoors_sunwind = grDevices::rgb(68/255, 119/255, 161/255),
        ground = grDevices::rgb(204/255, 183/255, 122/255),
        groundfcfactormethod = grDevices::rgb(153/255, 122/255, 30/255),
        groundslabpreprocessoraverage = grDevices::rgb(255/255, 191/255, 0/255),
        groundslabpreprocessorcore = grDevices::rgb(255/255, 182/255, 50/255),
        groundslabpreprocessorperimeter = grDevices::rgb(255/255, 178/255, 101/255),
        groundbasementpreprocessoraveragewall = grDevices::rgb(204/255, 51/255, 0/255),
        groundbasementpreprocessoraveragefloor = grDevices::rgb(204/255, 81/255, 40/255),
        groundbasementpreprocessorupperwall = grDevices::rgb(204/255, 112/255, 81/255),
        groundbasementpreprocessorlowerwall = grDevices::rgb(204/255, 173/255, 163/255),
        othersidecoefficients = grDevices::rgb(63/255, 63/255, 63/255),
        othersideconditionsmodel = grDevices::rgb(153/255, 0/255, 76/255)
    )
)
# }}}

# plot_idf {{{
plot_idf <- function (idf, render_by = c("surface_type", "boundary", "construction", "zone"),
                      new = TRUE, clear = TRUE, wireframe = FALSE) {
    render_by <- match.arg(render_by)
    dt <- extract_idf_surfaces(idf)

    if (is.null(dt)) return(invisible())
    plot_surface(dt, new = new, clear = clear, wireframe = wireframe, render_by = render_by)
}
# }}}

# extract_idf_surfaces {{{
extract_idf_surfaces <- function (idf) {
    cls <- idf$class_name(by_group = TRUE)["Thermal Zones and Surfaces"][[1L]]
    if (is.null(cls)) {
        message("Current IDF does not contain any supported surfaces.")
        return(invisible())
    }

    surf <- data.table()
    win <- data.table()
    shade <- data.table()

    # zone origins
    if (!idf$is_valid_class("Zone")) {
        zone <- data.table(name = character(), x = double(), y = double(), z = double(), name_lower = character())
    } else {
        zone <- idf$to_table(class = "Zone", wide = TRUE, all = TRUE, string_value = FALSE)[
            , .SD, .SDcols = c("name", paste(c("X", "Y", "Z"), "Origin"))]
        setnames(zone, c("name", "x", "y", "z"))
        set(zone, NULL, "name_lower", stri_trans_tolower(zone$name))
        zone[J(NA_real_), on = "x", x := 0.0]
        zone[J(NA_real_), on = "y", y := 0.0]
        zone[J(NA_real_), on = "z", z := 0.0]
    }

    # surfaces
    if ("BuildingSurface:Detailed" %in% cls) {
        surf <- extract_idf_surface_table(idf, "BuildingSurface:Detailed")
    }

    # other detailed surface specs
    cls_surf <- stri_subset_regex(cls, "(Wall|Roof|Floor|Ceiling).+Detailed")
    if (length(cls_surf)) {
        type <- stri_extract_first_regex(cls_surf, "(Wall|Roof|Floor)")
        surf_sep <- mapply(extract_idf_surface_table, cls_surf, type,
            MoreArgs = list(idf = idf), SIMPLIFY = FALSE, USE.NAMES = FALSE

        )

        # combine
        surf <- rbindlist(list(surf, surf_sep))
    }

    # update zone name
    set(surf, NULL, "zone_lower", stri_trans_tolower(surf$zone))

    # add origins
    surf[zone, on = c("zone_lower" = "name_lower"),
        `:=`(zone = i.name, origin_x = i.x, origin_y = i.y, origin_z = i.z)]

    set(surf, NULL, "zone_lower", NULL)

    # TODO: handle other surfaces

    if ("FenestrationSurface:Detailed" %in% cls) {
        win <- extract_idf_surface_table(idf, "FenestrationSurface:Detailed")
        # add zone
        set(win, NULL, "surface_lower", stri_trans_tolower(win$surface))
        set(surf, NULL, "surface_lower", stri_trans_tolower(surf$name))
        win[surf, on = "surface_lower",
            `:=`(surface = i.name, zone = i.zone, boundary = i.boundary,
                 boundary_obj = i.boundary_obj,
                 origin_x = i.origin_x, origin_y = i.origin_y, origin_z = i.origin_z)]

        set(win, NULL, "surface_lower", NULL)
    }

    if (length(cls_shade <- stri_subset_regex(cls, "Shading.+Detailed"))) {
        shade <- rbindlist(lapply(cls_shade, extract_idf_surface_table, idf = idf, type = "shading"))

        # add zone for zone shading
        if (!has_name(surf, "surface_lower")) {
            set(surf, NULL, "surface_lower", stri_trans_tolower(surf$name))
        }

        set(shade, NULL, "surface_lower", stri_trans_tolower(shade$surface))
        shade[surf, on = "surface_lower",
            `:=`(surface = i.name, zone = i.zone, boundary = i.boundary,
                 boundary_obj = i.boundary_obj,
                 origin_x = i.origin_x, origin_y = i.origin_y, origin_z = i.origin_z)]

        set(shade, NULL, "surface_lower", NULL)
    }

    if (has_name(surf, "surface_lower")) set(surf, NULL, "surface_lower", NULL)

    dt <- rbindlist(list(surf, win, shade), fill = TRUE)

    if (!nrow(dt)) {
        message("Current IDF does not contain any supported surfaces.")
        return(invisible())
    }

    # offset coordinates
    dt[!J(NA_real_, NA_real_, NA_real_), on = c("origin_x", "origin_y", "origin_z"),
        `:=`(x = origin_x + x, y = origin_y + y, z = origin_z + z)]
    set(dt, NULL, c("origin_x", "origin_y", "origin_z"), NULL)

    # rotate if necessary
    coord <- rgl::rotate3d(as.matrix(dt[, list(x, y, z)]),
        deg_to_arc(idf$Building$North_Axis), 0, 0, 1)

    set(dt, NULL, c("x", "y", "z"), as.data.table(coord))

    dt
}
# }}}

# extract_idf_surface_table {{{
extract_idf_surface_table <- function (idf, class, type) {
    dt <- idf$to_table(class = class, wide = TRUE, string_value = FALSE, align = TRUE)
    if (!has_name(dt, "Surface Type")) set(dt, NULL, "Surface Type", as.character(type))

    # extra columns
    detail <- c("Construction Name",
        if (stri_startswith_fixed(class, "Shading")) "Base Surface Name" else "Building Surface Name",
        "Zone Name", "Outside Boundary Condition", "Outside Boundary Condition Object")
    has_detail <- detail %in% names(dt)

    dt_m <- melt.data.table(dt,
        id.vars = c("id", "name", "Surface Type", detail[has_detail]),
        measure.vars = patterns("X-coordinate", "Y-coordinate", "Z-coordinate")
    )
    setnames(dt_m, "Surface Type", "type")
    setnames(dt_m, paste0("value", 1:3), c("x", "y", "z"))
    set(dt_m, NULL, "variable", NULL)
    set(dt_m, NULL, "type", stri_trans_tolower(dt_m$type))
    setorderv(dt_m, "id")

    if (any(!has_detail)) {
        set(dt_m, NULL, detail[!has_detail], NA_character_)
    }

    setcolorder(dt_m, c("id", "name", "type", detail))
    setnames(dt_m, detail, c("const", "surface", "zone", "boundary", "boundary_obj"))

    na.omit(dt_m, cols = c("x", "y", "z"))
}
# }}}

# map_color {{{
map_color <- function (dt, type = "surface_type") {
    cl <- data.table(type = names(COLOR_MAP[[type]]), color = COLOR_MAP[[type]])

    dt[cl, on = "type", color := i.color]
    dt[, alpha := 1.0]
    trans <- paste0(c("window", "glassdoor"), rep(c("", "_int", "_ext"), 2L))
    dt[J(trans), on = "type", alpha := 0.6]
}
# }}}

#' @importFrom rgl rgl.open rgl.quads
# plot_surface {{{
plot_surface <- function (dt, new = FALSE, clear = TRUE, wireframe = FALSE, render_by, ...) {
    map_color(dt, type = render_by)

    pt_num <- dt[, .N, by = c("id", "name")]

    # for triangles
    dt_tri <- dt[pt_num[J(3L), on = "N", -"N", nomatch = NULL], on = c("id", "name")]
    # for quadrangles
    dt_quad <- dt[pt_num[J(4L), on = "N", -"N", nomatch = NULL], on = c("id", "name")]
    # for polygons
    dt_poly <- dt[pt_num[!J(c(3L, 4L)), on = "N", -"N"], on = c("id", "name")]

    get_matrix <- function (dt) {
        M <- as.matrix(dt[, .SD, .SDcols = c("x", "y", "z")])
        dimnames(M)[[1L]] <- dt$name
        M
    }

    get_color <- function(dt) as.matrix(dt[, .SD, .SDcols = rep("color", 3L)])
    get_alpha <- function(dt) as.matrix(dt[, .SD, .SDcols = rep("alpha", 3L)])

    plot_type <- function (dt, type = c("tri", "quad", "poly")) {
        type <- match.arg(type)
        if (!nrow(dt)) return(NULL)

        rgl_fun <- switch(type,
            tri = rgl::triangles3d,
            quad = rgl::quads3d,
            poly = rgl::polygon3d
        )

        if (type != "poly") {
            mat <- get_matrix(dt)
            clr <- get_color(dt)
            alp <- get_alpha(dt)

            if (!wireframe) {
                rgl_fun(mat, color = clr, lit = FALSE, alpha = alp)
            }
            rgl_fun(mat, color = "black", lit = FALSE, front = "lines", back = "lines", lwd = 2.0)
        # for polygons, have to decide which are the two coordinates describe
        # the polygons
        } else {
            dt_base <- dt[,
                list(x = length(unique(x)), y = length(unique(y)), z = length(unique(z))),
                by = c("id", "name")
            ]

            plot_poly <- function (dt, dt_base, axis) {
                dt_axis <-dt_base[J(1L), on = axis, nomatch = NULL]
                if (!nrow(dt_axis)) return(NULL)
                dt <- dt[dt_axis, on = c("id", "name")]

                dt_s <- split(dt, by = c("id", "name"))

                base <- switch(axis, x = 1, y = 2, z = 3)

                for (surf in dt_s) {
                    mat <- get_matrix(surf)
                    clr <- get_color(surf)

                    # for polygons, no need to draw the frame since it should already
                    # shown by other simple surfaces
                    # try(rgl_fun(mat, color = clr, lit = FALSE, coords = setdiff(1:3, base), fill = FALSE), silent = TRUE)

                    tryCatch(
                        rgl_fun(mat, color = clr, lit = FALSE, coords = setdiff(1:3, base), fill = !wireframe),
                        error = function (e) {
                            cat(sprintf("Failed to render surface '%s'\n", surf$name[1]))
                        }
                    )
                }
            }

            plot_poly(dt, dt_base, "x")
            plot_poly(dt, dt_base, "y")
            plot_poly(dt, dt_base, "z")
        }
    }

    rgl_init(new = new, clear = clear)

    # Add x, y, and z Axes
    rgl::rgl.lines(c(0, max(dt$x) * 2), c(0, 0), c(0, 0), color = "red", lit = FALSE)
    rgl::rgl.lines(c(0, 0), c(0, max(dt$y) * 2), c(0, 0), color = "green", lit = FALSE)
    rgl::rgl.lines(c(0, 0), c(0, 0), c(0, max(dt$z) * 2), color = "blue", lit = FALSE)

    plot_type(dt_tri, "tri")
    plot_type(dt_quad, "quad")
    plot_type(dt_poly, "poly")
}
# }}}

#' @importFrom rgl rgl.open par3d rgl.bg rgl.viewpoint rgl.clear
# rgl_init {{{
rgl_init <- function (new = FALSE, clear = TRUE) {
    if (new) {
        rgl.open()
    } else if (clear) {
        rgl.clear(type = c("shapes"))
    }

    rgl.viewpoint(0, -60)

    cur <- par3d("mouseMode")
    cur[["wheel"]] <- "push"
    cur[["middle"]] <- "fov"
    par3d("mouseMode" = cur)
    trackball_origin(1L)
    pan3d(2L)

    rgl.bg(color = "white")
}
# }}}

#' @importFrom rgl rgl.cur currentSubscene3d par3d rgl.setMouseCallbacks translationMatrix
# pan3d {{{
# adapted from rgl examples
pan3d <- function(button, dev = rgl.cur(), subscene = currentSubscene3d(dev)) {
    start <- list()

    begin <- function(x, y) {
        activeSubscene <- par3d("activeSubscene", dev = dev)
        start$listeners <<- par3d("listeners", dev = dev, subscene = activeSubscene)
        for (sub in start$listeners) {
            init <- par3d(c("userProjection","viewport"), dev = dev, subscene = sub)
            init$pos <- c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5)
            start[[as.character(sub)]] <<- init
        }
    }

    update <- function(x, y) {
        for (sub in start$listeners) {
            init <- start[[as.character(sub)]]
            xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
            mouseMatrix <- translationMatrix(xlat[1], xlat[2], xlat[3])
            par3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = sub )
        }
    }
    rgl.setMouseCallbacks(button, begin, update, dev = dev, subscene = subscene)
}
# }}}

#' @importFrom rgl rgl.cur par3d rgl.set rgl.setMouseCallbacks rotationMatrix translationMatrix
# trackball_origin {{{
# adapted from https://r.789695.n4.nabble.com/Defining-origin-for-rotation-in-RGL-device-td3472859.html
trackball_origin <- function(button = 1, dev = rgl.cur(), origin = c(0, 0, 0)) {
    width <- height <- rotBase <- NULL
    userMatrix <- list()
    cur <- rgl.cur()
    offset <- NULL
    scale <- NULL

    xprod <- function(a, b) {
        c(a[2]*b[3] - a[3]*b[2],
          a[3]*b[1] - a[1]*b[3],
          a[1]*b[2] - a[2]*b[1])
    }

    vlen <- function(a) sqrt(sum(a^2))

    angle <- function(a,b) {
        dot <- sum(a*b)
        acos(dot/vlen(a)/vlen(b))
    }

    screenToVector <- function(x, y) {
        radius <- max(width, height)/2
        centre <- c(width, height)/2
        pt <- (c(x, y) - centre)/radius
        len <- vlen(pt)

        if (len > 1.e-6) pt <- pt/len

        maxlen <- sqrt(2)
        angle <- (maxlen - len)/maxlen*pi/2
        z <- sin(angle)
        len <- sqrt(1 - z^2)
        pt <- pt * len
        return (c(pt, z))
    }

    trackballBegin <- function(x, y) {
        vp <- par3d("viewport")
        width <<- vp[3]
        height <<- vp[4]
        cur <<- rgl.cur()
        bbox <- par3d("bbox")
        center <- c(sum(bbox[1:2])/2, sum(bbox[3:4])/2, sum(bbox[5:6])/2)
        scale <<- par3d("scale")
        offset <<- (center - origin)*scale
        for (i in dev) {
            if (inherits(try(rgl.set(i, TRUE)), "try-error")) dev <<- dev[dev != i]
            else userMatrix[[i]] <<- par3d("userMatrix")
        }
        rgl.set(cur, TRUE)
        rotBase <<- screenToVector(x, height - y)
     }

     trackballUpdate <- function(x,y) {
         rotCurrent <- screenToVector(x, height - y)
         angle <- angle(rotBase, rotCurrent)
         axis <- xprod(rotBase, rotCurrent)
         mouseMatrix <- rotationMatrix(angle, axis[1], axis[2], axis[3])
         for (i in dev) {
             if (inherits(try(rgl.set(i, TRUE)), "try-error")) dev <<- dev[dev != i]
             else par3d(userMatrix = t(translationMatrix(-offset[1], -offset[2], -offset[3])) %*% mouseMatrix  %*% t(translationMatrix(offset[1], offset[2], offset[3])) %*% userMatrix[[i]])
         }
         rgl.set(cur, TRUE)
     }

     for (i in dev) {
         rgl.set(i, TRUE)
         rgl.setMouseCallbacks(button, begin = trackballBegin, update = trackballUpdate, end = NULL)
     }

     rgl.set(cur, TRUE)
}
# }}}
