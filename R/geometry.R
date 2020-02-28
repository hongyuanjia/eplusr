# normalize_idf_surfaces {{{
normalize_idf_surfaces <- function (idf) {
    idf
}
# }}}

# IdfGeometry {{{
IdfGeometry <- R6Class("IdfGeometry", cloneable = FALSE,
    public = list(
        initialize = function (idf) {
            if (!is_idf(idf)) idf <- read_idf(idf)
            private$m_parent <- idf

            # change all non-detailed surfaces to their detailed equivalent
            private$m_parent <- normalize_idf_surfaces(private$m_parent)

            # extract geometry surfaces
            private$m_geometry <- extracct_idfgeom(private$m_parent)

            # save uuid
            private$m_log$parent_uuid <- ._get_private(private$m_parent)$m_log$uuid
        },

        vertices = function ()
            geom_vertices(self, private),

        view = function (new = TRUE, clear = TRUE, render_by = "surface_type",
                         axis = TRUE, wireframe = TRUE, surface = TRUE,
                         line_width = 1.5, line_color = "black",
                         theta = 0, phi = -60, fov = 60, zoom = 1,
                         background = "white", size = c(0, 30, 800))
            geom_view(self, private, new = new, clear = clear, render_by = render_by,
                      axis = axis, wireframe = wireframe, surface = surface,
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

# extracct_idfgeom {{{
extracct_idfgeom <- function (idf) {
    cls <- idf$class_name(by_group = TRUE)["Thermal Zones and Surfaces"][[1L]]
    if (is.null(cls)) {
        message("Current IDF does not contain any surfaces.")
        return(invisible())
    }

    surf <- data.table()
    hole <- data.table()
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
        surf <- extracct_idfgeom_surface(idf, "BuildingSurface:Detailed")
    }

    # other detailed surface specs
    cls_surf <- stri_subset_regex(cls, "(Wall|Roof|Floor|Ceiling).+Detailed")
    if (length(cls_surf)) {
        type <- stri_extract_first_regex(cls_surf, "(Wall|Roof|Floor)")
        surf_sep <- mapply(extracct_idfgeom_surface, cls_surf, type,
            MoreArgs = list(idf = idf), SIMPLIFY = FALSE, USE.NAMES = FALSE

        )

        # combine
        surf <- rbindlist(c(list(surf), surf_sep))
    }

    set(surf, NULL, "category", "surface")
    # update zone name
    set(surf, NULL, "zone_lower", stri_trans_tolower(surf$zone))

    # add origins
    surf[zone, on = c("zone_lower" = "name_lower"),
        `:=`(zone = i.name, origin_x = i.x, origin_y = i.y, origin_z = i.z)]

    set(surf, NULL, "zone_lower", NULL)

    # TODO: handle other surfaces

    if ("FenestrationSurface:Detailed" %in% cls) {
        win <- extracct_idfgeom_surface(idf, "FenestrationSurface:Detailed")
        # add zone
        set(win, NULL, "surface_lower", stri_trans_tolower(win$surface))
        set(surf, NULL, "surface_lower", stri_trans_tolower(surf$name))
        win[surf, on = "surface_lower",
            `:=`(surface = i.name, zone = i.zone, boundary = i.boundary,
                 boundary_obj = i.boundary_obj, surface_id = i.id,
                 origin_x = i.origin_x, origin_y = i.origin_y, origin_z = i.origin_z)]

        set(win, NULL, "surface_lower", NULL)
        set(win, NULL, "category", "window")

        # treat windows as holes
        # use surface id for grouping
        # TODO: What if there are windows that are not attached to any surfaces?
        hole <- copy(win)[!J(NA_character_), on = "surface"][, `:=`(id = surface_id)]
        set(hole, NULL, "category", "hole")

        # clean
        set(win, NULL, "surface_id", NULL)
        set(hole, NULL, "surface_id", NULL)
    }

    if (length(cls_shade <- stri_subset_regex(cls, "Shading.+Detailed"))) {
        shade <- rbindlist(lapply(cls_shade, extracct_idfgeom_surface, idf = idf))

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
        set(shade, NULL, "category", "shading")
    }

    if (has_name(surf, "surface_lower")) set(surf, NULL, "surface_lower", NULL)

    dt <- rbindlist(list(surf, hole, win, shade), fill = TRUE)

    if (!nrow(dt)) {
        message("Current IDF does not contain any supported surfaces.")
        return(invisible())
    }

    # offset coordinates
    # this make sure every point has the same origin of (0, 0, 0)
    dt[!J(NA_real_, NA_real_, NA_real_), on = c("origin_x", "origin_y", "origin_z"),
        `:=`(x = origin_x + x, y = origin_y + y, z = origin_z + z)]
    set(dt, NULL, c("origin_x", "origin_y", "origin_z"), NULL)

    # rotate if necessary
    coord <- rgl::rotate3d(as.matrix(dt[, list(x, y, z)]),
        deg_to_rad(idf$Building$North_Axis), 0, 0, 1)

    set(dt, NULL, c("x", "y", "z"), as.data.table(coord))

    # add number of vert and vertex index
    dt[, `:=`(n_vert = .N, index_vertex = seq_len(.N)), by = "id"]

    dt
}
# }}}

# extracct_idfgeom_surface {{{
extracct_idfgeom_surface <- function (idf, class, type = NA_character_) {
    dt <- idf$to_table(class = class, wide = TRUE, string_value = FALSE, align = TRUE)

    if (is.na(type)) {
        # determine shading type based on class names
        if (stri_startswith_fixed(class, "Shading")) {
            if (stri_detect_fixed(class, "Site")) {
                type <- "siteshading"
            } else if (stri_detect_fixed(class, "Building")) {
                type <- "buildingshading"
            } else {
                type <- "zoneshading"
            }
        }
    }

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

# triangulate_surfaces {{{
triangulate_surfaces <- function (dt) {
    # calculate normal vector of surfaces using Newell Method
    # Reference: https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal#Newell.27s_Method
    norm <- dt[!J("hole"), on = "category", by = "id", {
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
# }}}

# pair_line_vertex {{{
pair_line_vertex <- function (dt) {
    # for lines, vertices should be provided in pairs
    # only need to plot surfaces here, since windows and holes are the same
    # things here
    dt[!J("hole"), on = "category", by = "id", {
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
                       line_width = 1.5, line_color = "black",
                       theta = 0, phi = -60, fov = 60, zoom = 1, background = "white",
                       size = c(0, 30, 800)) {
    if (!requireNamespace("rgl", quietly = TRUE)) {
        abort("error_no_rgl", paste0(
            "'eplusr' relies on the 'rgl' package to view 3D IDF geometry; ",
            "please add this to your library with install.packages('rgl') and try agian."
        ))
    }
    if (!requireNamespace("decido", quietly = TRUE)) {
        abort("error_no_decido", paste0(
            "'eplusr' relies on the 'decido' package to view 3D IDF geometry; ",
            "please add this to your library with install.packages('decido') and try agian."
        ))
    }

    # remove logged rgl ids
    private$m_log$id <- NULL
    private$m_log$view <- NULL

    # copy the original data
    dt <- data.table::copy(private$m_geometry)

    # map color
    map_color(dt, type = render_by)

    # initial rgl window
    private$m_log$id$device <- rgl_init(new = new, clear = clear,
        theta = theta, phi = phi, fov = fov,
        zoom = zoom, background = background, size = size
    )

    # Add x, y, and z Axes
    if (axis) geom_view_add_axis(self, private)

    if (surface) {
        tri <- triangulate_surfaces(dt)

        private$m_log$id$surface <- rgl::triangles3d(
            x = as.matrix(tri[, .SD, .SDcols = c("x", "y", "z")]),
            color = as.matrix(tri[, .SD, .SDcols = rep("color", 3L)]),
            alpha = as.matrix(tri[, .SD, .SDcols = rep("alpha", 3L)])
        )
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
        abort("error_no_rgl", paste0(
            "'eplusr' relies on the 'rgl' package to view 3D IDF geometry; ",
            "please add this to your library with install.packages('rgl') and try agian."
        ))
    }

    if (rgl::rgl.cur() == 0) {
        abort("error_no_rgl_window", "No rgl window currently open. Please run '$view()' first.")
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
        abort("error_not_rgl_supported_fmt", paste0(
            "Not supported export format ", surround(tools::file_ext(filename)), ". ",
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
rgl_init <- function (new = FALSE, clear = TRUE, theta = 0, phi = -60, fov = 60,
                      zoom = 1, background = "white", size = c(0, 30, 800)) {
    assert(is_flag(new), is_flag(clear), is_number(theta), is_number(phi), is_number(fov), is_number(zoom))
    assert(are_number(size), length(size) <= 4L,
        msg = sprintf("'size' should be a numeric vector with length no more than %i.", length(size))
    )

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
        undefined = rgb(255/255, 255/255, 255/255, 1),
        normalmaterial = rgb(255/255, 255/255, 255/255, 1),
        normalmaterial_ext = rgb(255/255, 255/255, 255/255, 1),
        normalmaterial_int = rgb(255/255, 0/255, 0/255, 1),
        floor = rgb(128/255, 128/255, 128/255, 1),
        floor_ext = rgb(128/255, 128/255, 128/255, 1),
        floor_int = rgb(191/255, 191/255, 191/255, 1),
        wall = rgb(204/255, 178/255, 102/255, 1),
        wall_ext = rgb(204/255, 178/255, 102/255, 1),
        wall_int = rgb(235/255, 226/255, 197/255, 1),
        roof = rgb(153/255, 76/255, 76/255, 1),
        roof_ext = rgb(153/255, 76/255, 76/255, 1),
        roof_int = rgb(202/255, 149/255, 149/255, 1),
        ceiling = rgb(153/255, 76/255, 76/255, 1),
        ceiling_ext = rgb(153/255, 76/255, 76/255, 1),
        ceiling_int = rgb(202/255, 149/255, 149/255, 1),
        window = rgb(102/255, 178/255, 204/255, 0.6),
        window_ext = rgb(102/255, 178/255, 204/255, 0.6),
        window_int = rgb(192/255, 226/255, 235/255, 0.6),
        door = rgb(153/255, 133/255, 76/255, 1),
        door_ext = rgb(153/255, 133/255, 76/255, 1),
        door_int = rgb(202/255, 188/255, 149/255, 1),
        glassdoor = rgb(153/255, 133/255, 76/255, 1),
        glassdoor_ext = rgb(153/255, 133/255, 76/255, 1),
        glassdoor_int = rgb(202/255, 188/255, 149/255, 1),
        siteshading = rgb(75/255, 124/255, 149/255, 1),
        siteshading_ext = rgb(75/255, 124/255, 149/255, 1),
        siteshading_int = rgb(187/255, 209/255, 220/255, 1),
        buildingshading = rgb(113/255, 76/255, 153/255, 1),
        buildingshading_ext = rgb(113/255, 76/255, 153/255, 1),
        buildingshading_int = rgb(216/255, 203/255, 229/255, 1),
        zoneshading = rgb(76/255, 110/255, 178/255, 1),
        zoneshading_ext = rgb(76/255, 110/255, 178/255, 1),
        zoneshading_int = rgb(183/255, 197/255, 224/255, 1),
        interiorpartitionsurface = rgb(158/255, 188/255, 143/255, 1),
        interiorpartitionsurface_ext = rgb(158/255, 188/255, 143/255, 1),
        interiorpartitionsurface_int = rgb(213/255, 226/255, 207/255, 1)
    ),
    boundary = c(
        surface = rgb(0/255, 153/255, 0/255),
        adiabatic = rgb(255/255, 0/255, 0/255),
        zone = rgb(255/255, 0/255, 0/255),
        outdoors = rgb(163/255, 204/255, 204/255),
        outdoors_sun = rgb(40/255, 204/255, 204/255),
        outdoors_wind = rgb(9/255, 159/255, 162/255),
        outdoors_sunwind = rgb(68/255, 119/255, 161/255),
        ground = rgb(204/255, 183/255, 122/255),
        groundfcfactormethod = rgb(153/255, 122/255, 30/255),
        groundslabpreprocessoraverage = rgb(255/255, 191/255, 0/255),
        groundslabpreprocessorcore = rgb(255/255, 182/255, 50/255),
        groundslabpreprocessorperimeter = rgb(255/255, 178/255, 101/255),
        groundbasementpreprocessoraveragewall = rgb(204/255, 51/255, 0/255),
        groundbasementpreprocessoraveragefloor = rgb(204/255, 81/255, 40/255),
        groundbasementpreprocessorupperwall = rgb(204/255, 112/255, 81/255),
        groundbasementpreprocessorlowerwall = rgb(204/255, 173/255, 163/255),
        othersidecoefficients = rgb(63/255, 63/255, 63/255),
        othersideconditionsmodel = rgb(153/255, 0/255, 76/255)
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
map_color <- function (dt, type = "surface_type") {
    type <- match.arg(type, c("surface_type", "boundary", "construction", "zone"))

    # init alpha to 1.0 and color to white
    set(dt, NULL, c("color", "alpha"), list("white", 1.0))

    if (type == "surface_type") {
        cl <- data.table(type = names(COLOR_MAP$surface_type), color = COLOR_MAP$surface_type)
        dt[cl, on = "type", color := i.color]
        trans <- paste0(c("window", "glassdoor"), rep(c("", "_int", "_ext"), 2L))
        dt[J(trans), on = "type", alpha := 0.6]
    } else if (type == "boundary") {
        set(dt, NULL, "boundary_lower", stri_trans_tolower(dt$boundary))
        cl <- data.table(boundary_lower = names(COLOR_MAP$boundary), color = COLOR_MAP$boundary)
        dt[cl, on = "boundary_lower", color := i.color]
        set(dt, NULL, "boundary_lower", NULL)
    } else {
        col <- switch(type, construction = "const", zone = "zone")
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
