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
        },

        gross_area = function () {},

        net_area = function () {},

        outward_normal = function () {},

        tilt = function () {},

        azimuth = function () {},

        centroid = function () {},

        vertices = function () {
            private$m_geometry
        },

        view = function (new = TRUE, clear = TRUE, render_by = "surface_type",
                         axis = TRUE, wireframe = TRUE, surface = TRUE,
                         line_width = 1.5, line_color = "black",
                         theta = 0, phi = -60, fov = 60, zoom = 1,
                         background = "white", size = c(0, 30, 800))
            geom_view(self, private, new = new, clear = clear, render_by = render_by,
                      axis = axis, wireframe = wireframe, surface = surface,
                      line_width = line_width, line_color = line_color,
                      theta = theta, phi = phi, fov = fov, zoom = zoom,
                      background = background, size = size)
    ),

    private = list(
        m_parent = NULL,
        m_rule = NULL,
        m_geometry = NULL
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
#' @importFrom decido earcut
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

# geom_view {{{
geom_view <- function (self, private, new = TRUE, clear = TRUE, axis = TRUE,
                       render_by = "surface_type", wireframe = TRUE, surface = TRUE,
                       line_width = 1.5, line_color = "black",
                       theta = 0, phi = -60, fov = 60, zoom = 1, background = "white",
                       size = c(0, 30, 800)) {

    # copy the original data
    dt <- data.table::copy(private$m_geometry)

    # map color
    map_color(dt, type = render_by)

    # initial rgl window
    rgl_init(new = new, clear = clear)

    # Add x, y, and z Axes
    if (axis) {
        rgl::rgl.lines(c(0, max(dt$x)* 1.05), c(0, 0), c(0, 0), color = "red", lit = FALSE)
        rgl::rgl.lines(c(0, 0), c(0, max(dt$y) * 1.05), c(0, 0), color = "green", lit = FALSE)
        rgl::rgl.lines(c(0, 0), c(0, 0), c(0, max(dt$z) * 1.05), color = "blue", lit = FALSE)
    }

    if (surface) {
        tri <- triangulate_surfaces(dt)

        rgl::triangles3d(
            x = as.matrix(tri[, .SD, .SDcols = c("x", "y", "z")]),
            color = as.matrix(tri[, .SD, .SDcols = rep("color", 3L)]),
            alpha = as.matrix(tri[, .SD, .SDcols = rep("alpha", 3L)])
        )
    }

    if (wireframe) {
        # for lines, vertices should be provided in pairs
        # only need to plot surfaces here, since windows and holes are the same
        # things here
        l <- dt[!J("hole"), on = "category", by = "id", {
            idx <- c(sort(c(index_vertex, index_vertex[-1L])), 1L)
            list(x = x[idx], y = y[idx], z = z[idx])
        }]
        rgl::rgl.lines(l$x, l$y, l$z, color = line_color, lwd = line_width, lit = FALSE)
    }

    invisible(self)
}
# }}}

# deg_to_rad {{{
deg_to_rad <- function (x) x / 180 * pi
# }}}

# rad_to_deg {{{
rad_to_deg <- function (x) x / pi * 180
# }}}

#' @importFrom rgl rgl.open par3d rgl.bg rgl.viewpoint rgl.clear
# rgl_init {{{
rgl_init <- function (new = FALSE, clear = TRUE, theta = 0, phi = -60, fov = 60,
                      zoom = 1, background = "white", size = c(0, 30, 800)) {
    assert(is_flag(new), is_flag(clear), is_number(theta), is_number(phi), is_number(fov), is_number(zoom))
    assert(are_number(size), length(size) <= 4L,
        msg = sprintf("'size' should be a numeric vector with length no more than %i.", length(size))
    )

    if (new) {
        rgl::rgl.open()
    } else if (clear) {
        rgl::rgl.clear()
    }

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
    par3d(windowRect = size)

    # set viewpoint
    rgl.viewpoint(theta, phi, fov, zoom)

    # change mouse control method
    cur <- par3d("mouseMode")
    cur[["left"]] <- "trackball"
    cur[["wheel"]] <- "push"
    cur[["middle"]] <- "fov"
    par3d("mouseMode" = cur)
    pan3d(2L)

    rgl.bg(color = background)
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

# COLOR_MAP {{{
# Reference: 'openstudio\openstudiocore\ruby\openstudio\sketchup_plugin\lib\interfaces\MaterialsInterface.rb'
#' @importFrom grDevices rgb
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
        zoneshading = grDevices::rgb(76/255, 110/255, 178/255, 1),
        zoneshading_ext = grDevices::rgb(76/255, 110/255, 178/255, 1),
        zoneshading_int = grDevices::rgb(183/255, 197/255, 224/255, 1),
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
    } else if (type == "construction") {
        set(dt, NULL, "const_lower", stri_trans_tolower(dt$const))
        cl <- dt[!is.na(const), list(const_lower = unique(const_lower))][
            , color := grDevices::palette(grDevices::hcl.colors(.N, "dark 3"))]
        dt[cl, on = "const_lower", color := i.color]
        set(dt, NULL, "const_lower", NULL)
    } else if (type == "zone") {
        set(dt, NULL, "zone_lower", stri_trans_tolower(dt$zone))
        cl <- dt[!is.na(zone), list(zone_lower = unique(zone_lower))][
            , color := grDevices::palette(grDevices::hcl.colors(.N, "dark 3"))]
        dt[cl, on = "zone_lower", color := i.color]
        set(dt, NULL, "zone_lower", NULL)
    }

    dt
}
# }}}
