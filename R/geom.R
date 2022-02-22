#' @importFrom R6 R6Class
#' @include impl-idf.R
NULL

#' Modify and Visualize an EnergyPlus Model Geometry
#'
#' `IdfGeometry` is an abstraction of a collection of geometry in an [Idf]. It
#' provides more detail methods to query geometry properties, update geometry
#' vertices and visualize geometry in 3D using the
#' [rgl](https://cran.r-project.org/package=rgl) package.
#'
#' @importFrom R6 R6Class
#' @docType class
#' @name IdfGeometry
#' @seealso [Idf] class
#' @author Hongyuan Jia
NULL

# idf_geometry {{{
#' Create an `IdfGeometry` object
#'
#' @param parent A path to an IDF file or an [Idf] object.
#'
#' @param object A character vector of valid names or an integer
#'        vector of valid IDs of objects to extract. If `NULL`, all
#'        objects in geometry classes will be extracted.
#'
#' @return An `IdfGeometry` object.
#'
#' @name IdfGeometry
#' @export
idf_geometry <- function (parent, object = NULL) {
    IdfGeometry$new(parent, object)
}
# }}}

# IdfGeometry {{{
#' @export
IdfGeometry <- R6Class("IdfGeometry", cloneable = FALSE,
    public = list(
        # initialize {{{
        #' @description
        #' Create an `IdfGeometry` object
        #'
        #' @param parent A path to an IDF file or an [Idf] object.
        #'
        #' @param object A character vector of valid names or an integer
        #'        vector of valid IDs of objects to extract. If `NULL`, all
        #'        objects in geometry classes will be extracted.
        #'
        #' @return An `IdfGeometry` object.
        #'
        #' @examples
        #' \dontrun{
        #' # example model shipped with eplusr from EnergyPlus v8.8
        #' path_idf <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr") # v8.8
        #'
        #' # create from an Idf object
        #' idf <- read_idf(path_idf, use_idd(8.8, "auto"))
        #' geom <- idf$geometry()
        #' geom <- IdfGeometry$new(idf)
        #'
        #' # create from an IDF file
        #' geom <- idf_geometry(path_idf)
        #' geom <- IdfGeometry$new(path_idf)
        #' }
        initialize = function (parent, object = NULL) {
            if (!is_idf(parent)) parent <- read_idf(parent)
            private$m_parent <- parent

            # extract geometry surfaces
            private$m_geoms <- extract_geom(private$m_parent, object)

            # init log env
            private$m_log <- new.env(hash = FALSE, parent = emptyenv())

            # log input object
            if (!is.null(object)) {
                private$m_log$object <- get_geom_class(private$m_parent, object)$id
            }

            # add a uuid
            private$m_log$uuid <- unique_id()

            # log parent data
            private$log_parent_uuid()
            private$log_parent_order()
            private$log_geom_class()
        },
        # }}}

        # parent {{{
        #' @description
        #' Get parent [Idf] object
        #'
        #' @details
        #' `$parent()` returns the parent [Idf] object of current `IdfGeometry`
        #' object.
        #'
        #' @return An [Idf] object.
        #'
        #' @examples
        #' \dontrun{
        #' geom$parent()
        #' }
        parent = function ()
            idfgeom_parent(self, private),
        # }}}

        # rules {{{
        #' @description
        #' Get global geometry rules
        #'
        #' @details
        #' `$rules()` returns global geometry rules.
        #'
        #' @return An [Idf] object.
        #'
        #' @examples
        #' \dontrun{
        #' geom$rules()
        #' }
        rules = function ()
            idfgeom_rules(self, private),
        # }}}

        # convert {{{
        #' @description
        #' Convert simple geometry objects
        #'
        #' @details
        #' EnergyPlus provides several classes that allow for simplified entry
        #' of geometries, such as `Wall:Exterior`, `Window` and etc.
        #' `$convert()` will generate detailed vertices from simplified geometry
        #' specifications and replace the original object with its corresponding
        #' detailed class, including:
        #'
        #' * `BuildingSurface:Detailed`
        #' * `FenestrationSurface:Detailed`
        #' * `Shading:Site:Detailed`
        #' * `Shading:Building:Detailed`
        #' * `Shading:Zone:Detailed`
        #'
        #' @param type A character vector giving what types of simplified
        #'        geometries should be converted. Should be a subset of
        #'        `"surface"`, `"subsurface"` and `"shading"`. Default is set to
        #'        all of them.
        #'
        #' @return The modified [Idf] object.
        #'
        #' @examples
        #' \dontrun{
        #' geom$convert()
        #' }
        convert = function (type = c("surface", "subsurface", "shading"))
            idfgeom_convert(self, private, type),
        # }}}

        # coord_system {{{
        #' @description
        #' Convert vertices to specified coordinate systems
        #'
        #' @details
        #' `$coord_system()` converts all vertices of geometries into specified
        #' coordinate systems, e.g. from world to relative, and vice versa.
        #' Besides, it also updates the `GlobalGeometryRules` in parent [Idf]
        #' accordingly.
        #'
        #' @param detailed,simple,daylighting A string specifying the coordinate
        #'        system for detailed geometries, simple (rectangular surface)
        #'        geometries, and daylighting reference points. Should be one of
        #'        `"relative"`, `"world"` and `"absolute"`. `"absolute"` is the
        #'        same as `"world"` and converted to it.
        #'
        #' @return The modified [Idf] object.
        #'
        #' @examples
        #' \dontrun{
        #' geom$coord_system("world", "world", "world")
        #' }
        coord_system = function (detailed = NULL, simple = NULL, daylighting = NULL)
            idfgeom_coord_system(self, private, detailed, simple, daylighting),
        # }}}

        # round_digits {{{
        #' @description
        #' Round digits on geometry vertices
        #'
        #' @details
        #' `$round_digits()` performs number rounding on vertices of detailed
        #' geometry object vertices, e.g. `BuildingSurface:Detailed`,
        #' `FenestrationSurface:Detailed` and etc.
        #'
        #' `$round_digits()` may be useful for clean up IDF files generated
        #' using OpenStudio which often gives vertices with long trailing
        #' digits.
        #'
        #' @param digits An integer giving the number of decimal places to be
        #'        used. Default: `4`.
        #'
        #' @return The modified [Idf] object.
        #'
        #' @examples
        #' \dontrun{
        #' geom$round_digits()
        #' }
        round_digits = function (digits = 4L)
            idfgeom_round_digits(self, private, digits),
        # }}}

        # area {{{
        #' @description
        #' Get area
        #'
        #' @details
        #' `$area()` returns the area of surfaces in square meters.
        #'
        #' @param class A character vector of valid geometry class names.
        #'        Default: `NULL`.
        #'
        #' @param object A character vector of valid names or an integer
        #'        vector of valid IDs of targeting objects.
        #'        Default: `NULL`.
        #'
        #' @param net If `TRUE`, the gross area is returned. If `FALSE`, the net
        #'        area is returned. Default: `FALSE`.
        #'
        #' @return A [data.table::data.table()] of 6 columns:
        #'
        #' * `id`: Integer type. Object IDs.
        #' * `name`: Character type. Object names.
        #' * `class`: Character type. Class names.
        #' * `zone`: Character type. Zone names that specified objects belong to.
        #' * `type`: Character type. Surface types.
        #' * `area`: Numeric type. Surface Area in m2.
        #'
        #' @examples
        #' \dontrun{
        #' geom$area()
        #' }
        area = function (class = NULL, object = NULL, net = FALSE)
            idfgeom_area(self, private, class, object, net),
        # }}}

        # azimuth {{{
        #' @description
        #' Get azimuth
        #'
        #' @details
        #' `$azimuth()` returns the azimuth of surfaces in degree.
        #'
        #' @param class A character vector of valid geometry class names.
        #'        Default: `NULL`.
        #'
        #' @param object A character vector of valid names or an integer
        #'        vector of valid IDs of targeting objects.
        #'        Default: `NULL`.
        #'
        #' @return A [data.table::data.table()] of 6 columns:
        #'
        #' * `id`: Integer type. Object IDs.
        #' * `name`: Character type. Object names.
        #' * `class`: Character type. Class names.
        #' * `zone`: Character type. Zone names that specified objects belong to.
        #' * `type`: Character type. Surface types.
        #' * `azimuth`: Numeric type. Azimuth in degree.
        #'
        #' @examples
        #' \dontrun{
        #' geom$azimuth()
        #' }
        azimuth = function (class = NULL, object = NULL)
            idfgeom_azimuth(self, private, class, object),
        # }}}

        # tilt {{{
        #' @description
        #' Get tilt
        #'
        #' @details
        #' `$tilt()` returns the tilt of surfaces in degree.
        #'
        #' @param class A character vector of valid geometry class names.
        #'        Default: `NULL`.
        #'
        #' @param object A character vector of valid names or an integer
        #'        vector of valid IDs of targeting objects.
        #'        Default: `NULL`.
        #'
        #' @return A [data.table::data.table()] of 6 columns:
        #'
        #' * `id`: Integer type. Object IDs.
        #' * `name`: Character type. Object names.
        #' * `class`: Character type. Class names.
        #' * `zone`: Character type. Zone names that specified objects belong to.
        #' * `type`: Character type. Surface types.
        #' * `tilt`: Numeric type. Azimuth in degree.
        #'
        #' @examples
        #' \dontrun{
        #' geom$tilt()
        #' }
        tilt = function (class = NULL, object = NULL)
            idfgeom_tilt(self, private, class, object),
        # }}}

        # view {{{
        #' @description
        #' View 3D geometry
        #'
        #' @details
        #' `$view()` uses the [rgl](https://cran.r-project.org/package=rgl)
        #' package to visualize the IDF geometry in 3D in a similar way as
        #' [OpenStudio](https://openstudio.net/).
        #'
        #' `$view()` returns an [IdfViewer] object which can be used to further
        #' tweak the viewer scene.
        #'
        #' In the rgl window, you can control the view using your mouse:
        #'
        #' * Left button: Trackball
        #' * Right button: Pan
        #` * Middle button: Field-of-view (FOV). `0' means orthographic
        #'   projection.
        #' * Wheel: Zoom
        #'
        #' For more detailed control on the scene, see [IdfViewer].
        #'
        #' @param new If `TRUE`, a new rgl window will be open using
        #'        [rgl::rgl.open()]. If `FALSE`, existing rgl window will be
        #'        reused if possible. Default: `FALSE`.
        #'
        #' @param render_by A single string specifying the way of rendering the
        #'        geometry. Possible values are:
        #'
        #' * `"surface_type"`: Default. Render the model by surface type model.
        #'   Walls, roofs, windows, doors, floors, and shading surfaces will
        #'   have unique colors.
        #' * `"boundary"`: Render the model by outside boundary condition. Only
        #'   surfaces that have boundary conditions will be rendered with a
        #'   color. All other surfaces will be white.
        #' * `"construction"`: Render the model by surface constructions.
        #' * `"zone"`: Render the model by zones assigned.
        #' * `"normal"`: Render the model by surface normal. The outside face of
        #'   a heat transfer face will be rendered as white and the inside face
        #'   will be rendered as red.
        #'
        #' @param axis If `TRUE`, the X, Y and Z axes will be drawn at the
        #'        global origin. Default: `TRUE`.
        #'
        #' @param wireframe If `TRUE`, the wireframe of each surface will be
        #'        shown. Default: `TRUE`.
        #'
        #' @param x_ray If `TRUE`, all surfaces wll be rendered translucently.
        #'        Default: `FALSE`.
        #'
        #' @return An [IdfViewer] object
        #'
        #' @examples
        #' \dontrun{
        #' idf$view()
        #' idf$view(render_by = "zone")
        #' idf$view(new = TRUE, render_by = "construction")
        #' }
        view = function (new = FALSE, render_by = "surface_type",
                         wireframe = TRUE, x_ray = FALSE, axis = TRUE)
            idfgeom_view(self, private, new = new, render_by = render_by,
                axis = axis, wireframe = wireframe, x_ray = x_ray),
        # }}}

        # print {{{
        #' @description
        #' Print an `IdfGeometry` object
        #'
        #' @return The `IdfGeometry` itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' geom$print()
        #' }
        print = function ()
            idfgeom_print(self, private)
        # }}}

        # TODO: set_wwr
        # TODO: rotate
        # TODO: scale
        # TODO: match
        # TODO: intersect
        # TODO: translate
        # TODO: building
        # TODO: zone
        # TODO: surface
        # TODO: subsurface
        # TODO: shading
        # TODO: daylighting_point
    ),

    private = list(
        m_parent = NULL,
        m_geoms = NULL,
        m_viewer = NULL,
        m_log = NULL,

        # PRIVATE FUNCTIONS {{{
        uuid = function () private$m_log$uuid,
        log_new_uuid = function () log_new_uuid(private$m_log),

        parent_uuid = function () get_priv_env(private$m_parent)$uuid(),
        log_parent_uuid = function () private$m_log$parent_uuid <- get_priv_env(private$m_parent)$uuid(),
        cached_parent_uuid = function () private$m_log$parent_uuid,

        parent_order = function () get_priv_env(private$m_parent)$m_log$order,
        log_parent_order = function () private$m_log$parent_order <- copy(get_priv_env(private$m_parent)$m_log$order),
        cached_parent_order = function () private$m_log$parent_order,

        geom_class = function () {
            # find current geometry objects
            grp <- c("Thermal Zones and Surfaces", "Daylighting")
            grp <- grp[private$m_parent$is_valid_group(grp)]

            cls <- c("Building", "GlobalGeometryRules")
            cls <- cls[private$m_parent$is_valid_class(cls)]
            if (!length(cls)) cls <- NULL

            if (!length(grp)) return(data.table())

            obj <- get_idf_object_multi_scope(
                get_priv_env(private$m_parent)$idd_env(),
                get_priv_env(private$m_parent)$idf_env(),
                class = cls, group = grp
            )
            set(obj, NULL, setdiff(names(obj), c("class_name", "object_id", "object_name")), NULL)

            # category by class names
            set(obj, NULL, c("type", "subtype", "misc"),
                as.data.table(stri_split_fixed(obj$class_name, ":", n = 3L, simplify = TRUE))
            )
            obj[type %chin% c("Building", "GlobalGeometryRules",
                "Zone", "BuildingSurface", "Wall", "RoofCeiling", "Floor",
                "Wall", "Roof", "Ceiling", "FenestrationSurface", "Window",
                "Door", "GlazedDoor", "Shading", "Daylighting")]
        },
        log_geom_class = function () private$m_log$geom_class <- private$geom_class(),
        cached_geom_class = function () private$m_log$geom_class,

        # dynamic update data when parent IDF has been changed
        geoms = function () {
            if (private$parent_uuid() == private$cached_parent_uuid()) {
                return(private$m_geoms)
            }

            # find out objects that has been changed
            set <- data.table::fsetdiff(private$parent_order(), private$cached_parent_order())
            del <- data.table::fsetdiff(private$cached_parent_order(), private$parent_order())

            # should re-extract geometry
            if (any(c(set$object_id, del$object_id) %in% c(private$geom_class()$object_id, private$cached_geom_class()$object_id))) {
                private$m_geoms <- extract_geom(private$m_parent, private$m_log$object)

                # log parent data
                private$log_parent_uuid()
                private$log_parent_order()
                private$log_geom_class()
            }

            private$m_geoms
        }
        # }}}

    )
)
# }}}

# idfgeom_parent {{{
idfgeom_parent <- function (self, private) {
    private$m_parent
}
# }}}
# idfgeom_rules {{{
idfgeom_rules <- function (self, private) {
    private$geoms()$rules
}
# }}}
# idfgeom_coord_system {{{
idfgeom_coord_system <- function (self, private, detailed = NULL, simple = NULL, daylighting = NULL) {
    if (is.null(detailed) && is.null(simple) && is.null(daylighting)) return(self)

    assert_choice(detailed, c("world", "absolute", "relative"), null.ok = TRUE)
    assert_choice(simple, c("world", "absolute", "relative"), null.ok = TRUE)
    assert_choice(daylighting, c("world", "absolute", "relative"), null.ok = TRUE)

    if (!is.null(detailed) && detailed == "absolute") detailed <- "world"
    if (!is.null(simple) && simple == "absolute") simple <- "world"
    if (!is.null(daylighting) && daylighting == "absolute") daylighting <- "world"

    rules <- private$geoms()$rules

    private$m_geoms <- align_coord_system(private$geoms(), detailed, simple, daylighting)

    # update the global geometry rules if exists, otherwise add one
    if (!private$m_parent$is_valid_class("GlobalGeometryRules")) {
        ggr <- private$m_parent$add(GlobalGeometryRules = private$m_geoms$rules)[[1L]]
    } else {
        ggr <- private$m_parent$set(GlobalGeometryRules := private$m_geoms$rules)[[1L]]
    }
    val <- standardize_idf_value(
        get_priv_env(private$m_parent)$idd_env(),
        get_priv_env(private$m_parent)$idf_env(),
        get_idf_value(
            get_priv_env(private$m_parent)$idd_env(),
            get_priv_env(private$m_parent)$idf_env(),
            object = ggr$id()
        ),
        "choice"
    )
    get_priv_env(private$m_parent)$idf_env()$value[val, on = "value_id", value_chr := i.value_chr]

    # update vertices of detailed geometries
    if (rules$coordinate_system != private$m_geoms$rules$coordinate_system) {
        meta <- rbindlist(list(
            if (nrow(private$m_geoms$surface)) fast_subset(private$m_geoms$surface, c("id", "class")),
            if (nrow(private$m_geoms$subsurface)) fast_subset(private$m_geoms$subsurface, c("id", "class")),
            if (nrow(private$m_geoms$shading)) fast_subset(private$m_geoms$shading, c("id", "class"))
        ))
        set_geom_vertices(private$m_parent, list(meta = meta, vertices = private$m_geoms$vertices))
    }

    # add a uuid
    private$log_new_uuid()

    # log parent data
    private$log_parent_uuid()
    private$log_parent_order()
    private$log_geom_class()

    self
}
# }}}
# idfgeom_convert {{{
idfgeom_convert <- function (self, private, type = c("surface", "subsurface", "shading")) {
    conv <- convert_geom(private$m_parent, private$geoms(), type = type)
    private$m_parent <- conv$idf

    # update geometry data
    # originally store whole IDF geometry data
    if (!length(private$m_log$object)) {
        private$m_geoms <- extract_geom(private$m_parent)
    # originally store a subset of IDF geometry data
    } else {
        id <- conv$map[J(private$m_log$object), on = "ori_id", nomatch = NULL, new_id]
        if (!length(id)) id <- NULL
        private$m_geoms <- extract_geom(private$m_parent, id)
        private$m_log$object <- id
    }

    # add a uuid
    private$log_new_uuid()

    # log parent data
    private$log_parent_uuid()
    private$log_parent_order()
    private$log_geom_class()

    # store the mapping as an attribute
    setattr(private$m_parent, "mapping", conv$map)

    invisible(private$m_parent)
}
# }}}
# idfgeom_round_digits {{{
idfgeom_round_digits <- function (self, private, digits = 4L) {
    zone <- data.table()
    origin <- data.table()
    if (nrow(private$m_geoms$zone)) {
        zone <- data.table(id = private$m_geoms$zone$id, class = "Zone")
        origin <- private$m_geoms$zone[, .SD, .SDcols = c("id", "x", "y", "z")]
        set(origin, NULL, "index", 1L)
    }

    check_empty <- function (dt) if (!nrow(dt)) data.table() else dt
    meta <- rbindlist(list(zone,
        check_empty(fast_subset(private$m_geoms$surface, c("id", "class"))),
        check_empty(fast_subset(private$m_geoms$subsurface, c("id", "class"))),
        check_empty(fast_subset(private$m_geoms$shading, c("id", "class")))
    ))

    # add a uuid
    private$log_new_uuid()

    # log parent data
    private$log_parent_uuid()
    private$log_parent_order()

    vert <- rbindlist(list(private$m_geoms$vertices, origin), use.names = TRUE)
    set_geom_vertices(private$m_parent, list(meta = meta, vertices = vert), digits = digits)

    invisible(private$m_parent)
}
# }}}
# idfgeom_subset_vertices {{{
idfgeom_subset_vertices <- function (self, private, class = NULL, object = NULL) {
    geoms <- private$geoms()
    obj <- data.table()

    if (is.null(class) && is.null(object)) return(list(geoms = geoms, object = obj))

    # match object
    obj <- get_idf_object_multi_scope(
        get_priv_env(private$m_parent)$idd_env(),
        get_priv_env(private$m_parent)$idf_env(),
        class = class, object = object
    )
    geom_class <- get_geom_class(private$m_parent, obj$object_id)
    geoms$vertices <- geoms$vertices[J(geom_class$id), on = "id", nomatch = NULL]

    list(geoms = geoms, object = obj)
}
# }}}
# idfgeom_cal_property {{{
idfgeom_cal_property <- function (self, private, class = NULL, object = NULL, fun) {
    l <- idfgeom_subset_vertices(self, private, class, object)
    geoms <- l$geoms
    obj <- l$object

    if (!nrow(geoms$vertices)) {
        return(data.table(id = integer(), name = character(), class = character(),
            zone = character(), type = character(), property = double()
        ))
    }

    # remove daylighting points
    if (NROW(geoms$daylighting_point)) {
        geoms$vertices <- geoms$vertices[!J(geoms$daylighting_point$id), on = "id"]
    }
    prop <- get_newall_vector(geoms$vertices)[, by = "id", list(property = fun(c(x, y, z)))]

    add_zone_name(geoms)
    cols <- c("id", "name", "class", "zone_name", "surface_type")
    meta <- rbindlist(list(
        NULL,
        if (nrow(geoms$surface)) fast_subset(geoms$surface, cols),
        if (nrow(geoms$subsurface)) fast_subset(geoms$subsurface, cols),
        if (nrow(geoms$shading)) fast_subset(geoms$shading, cols)
    ))
    setnames(meta, c("id", "name", "class", "zone", "type"))
    del_zone_name(geoms)

    add_joined_cols(meta, prop, "id", names(meta)[-1L])
    setcolorder(prop, names(meta))

    prop
}
# }}}
# idfgeom_area {{{
idfgeom_area <- function (self, private, class = NULL, object = NULL, net = FALSE) {
    assert_flag(net)
    prop <- idfgeom_cal_property(self, private, class, object, get_area)
    setnames(prop, "property", "area")

    if (!net) return(prop[])

    hole <- prop[J(c("Window", "Door", "GlazedDoor")), on = "type", nomatch = NULL]
    if (nrow(hole)) {
        add_joined_cols(private$geoms()$subsurface, hole, "id", "building_surface_name")
        prop[hole, on = c("name" = "building_surface_name"), area := area - i.area]
    }

    prop[]
}
# }}}
# idfgeom_azimuth {{{
idfgeom_azimuth <- function (self, private, class = NULL, object = NULL) {
    prop <- idfgeom_cal_property(self, private, class, object, get_azimuth)
    setnames(prop, "property", "azimuth")[]
}
# }}}
# idfgeom_tilt {{{
idfgeom_tilt <- function (self, private, class = NULL, object = NULL) {
    prop <- idfgeom_cal_property(self, private, class, object, get_tilt)
    setnames(prop, "property", "tilt")[]
}
# }}}
# idfgeom_view {{{
idfgeom_view <- function (self, private, new = FALSE, render_by = "surface_type",
                          wireframe = TRUE, x_ray = FALSE, axis = TRUE) {
    assert_flag(new)
    assert_flag(wireframe)
    assert_flag(x_ray)
    assert_flag(axis)
    assert_string(render_by)

    if (new || is.null(private$m_viewer)) {
        private$m_viewer <- idf_viewer(self)
    }

    private$m_viewer$render_by(render_by)
    private$m_viewer$wireframe(wireframe)
    private$m_viewer$x_ray(x_ray)
    private$m_viewer$axis(axis)

    if (is.null(private$m_viewer$device())) private$m_viewer$show()

    invisible(private$m_viewer)
}
# }}}
# idfgeom_print {{{
idfgeom_print <- function (self, private) {
    cli::cat_rule("EnergPlus IDF Geometry", line = 1)

    if (is.null(private$m_parent$path())) path <- crayon::bold$bgRed("NOT LOCAL") else path <- surround(private$m_parent$path())

    cli::cat_line(" * ", c(
        str_trunc(paste0("Path: ", path), width = cli::console_width() - 3L),
        paste0("Version: ", surround(private$m_parent$version()))
    ))

    geoms <- private$geoms()

    cli::cat_line(sprintf(" * Building: '%s'", geoms$building$name))
    cli::cat_line(sprintf(" * North Axis: %s\u00B0", geoms$building$north_axis))
    cli::cat_line(sprintf(" * Zone Num: %s", NROW(geoms$zone)))
    cli::cat_line(sprintf(" * Surface Num: %s", NROW(geoms$surface)))
    cli::cat_line(sprintf(" * SubSurface Num: %s", NROW(geoms$subsurface)))
    cli::cat_line(sprintf(" * Shading Num: %s", NROW(geoms$shading)))
    cli::cat_line(sprintf(" * Dayl Ref Pnt Num: %s", NROW(geoms$daylighting_point)))
    cli::cat_line(" * Coordinate System:")
    cli::cat_line(c(
        sprintf("   - Detailed: '%s'", ifelse(geoms$rules$coordinate_system == "world", "World", "Relative")),
        sprintf("   - Simple:   '%s'", ifelse(geoms$rules$rectangular_surface_coordinate_system == "world", "World", "Relative")),
        sprintf("   - Daylighting: '%s'", ifelse(geoms$rules$daylighting_reference_point_coordinate_system == "world", "World", "Relative"))
    ))
}
# }}}
