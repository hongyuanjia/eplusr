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

        plot = function (new = TRUE, clear = TRUE, render_by = "surface_type")
            plot_surface(private$m_geometry, new = new, clear = clear, render_by = render_by)
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
        hole <- win[!J(NA_character_), on = "surface"][, `:=`(id = surface_id)]
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
    dt[!J(NA_real_, NA_real_, NA_real_), on = c("origin_x", "origin_y", "origin_z"),
        `:=`(x = origin_x + x, y = origin_y + y, z = origin_z + z)]
    set(dt, NULL, c("origin_x", "origin_y", "origin_z"), NULL)

    # rotate if necessary
    coord <- rgl::rotate3d(as.matrix(dt[, list(x, y, z)]),
        deg_to_rad(idf$Building$North_Axis), 0, 0, 1)

    set(dt, NULL, c("x", "y", "z"), as.data.table(coord))

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

# deg_to_rad {{{
deg_to_rad <- function (x) x / 180 * pi
# }}}
# rad_to_deg {{{
rad_to_deg <- function (x) x / pi * 180
# }}}
