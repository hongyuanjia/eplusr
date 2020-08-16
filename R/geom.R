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
#' @name IdfGeometry
#' @export
idf_geometry <- function (parent, object = NULL) {
    IdfGeometry$new(parent, object)
}
# }}}

# IdfGeometry {{{
#' @export
IdfGeometry <- R6Class("IdfGeometry", cloneable = TRUE,
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

            # add a uuid
            private$m_log$uuid <- unique_id()
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
        #' geom$parent()
        parent = function ()
            idfgeom_parent(self, private),
        # }}}

        # rules {{{
        #' @description
        #' Get global geometry rules
        #'
        #' @details
        #' `$rules()` set global geometry rules.
        #'
        #' @return An [Idf] object.
        #'
        #' @examples
        #' geom$rules()
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
        #' @param object A character vector of valid names or an integer
        #'        vector of valid IDs of targeting objects to be converted.
        #'        Default: `NULL`.
        #'
        #' @return The modified [Idf] object.
        #'
        #' @examples
        #' \dontrun{
        #' geom$convert()
        #' }
        convert = function (type = c("surface", "subsurface", "shading"), object = NULL)
            idfgeom_convert(self, private, type, object),
        # }}}

        # round_digits {{{
        #' @description
        #' Round digits on geometry vertices
        #'
        #' @details
        #' `$round_digits()` performs number rounding on geometry object
        #' vertices. It may be useful for clean up IDF files generated using
        #' OpenStudio which often gives vertices with long trailing digits.
        #'
        #' @param digits An integer giving the number of decimal places to be
        #'        used. Default: `5`.
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
        #' @return A [data.table::data.table()] of 4 columns:
        #'
        #' * `id`: Integer type. Object IDs.
        #' * `name`: Character type. Object names.
        #' * `class`: Character type. Class names.
        #' * `area`: Numeric type. Surface Area in m2.
        #'
        #' @examples
        #' \dontrun{
        #' geom$area()
        #' }
        area = function (class = NULL, object = NULL, net = FALSE)
            idfgeom_area(self, private, class, object, net),
        # }}}

        # outward_normal {{{
        #' @description
        #' Get outward normal vector
        #'
        #' @details
        #' `$outward_normal()` returns the outward normal vector of surfaces.
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
        #' * `x`: Numeric type. X axis value.
        #' * `y`: Numeric type. Y axis value.
        #' * `z`: Numeric type. Z axis value.
        #'
        #' @examples
        #' \dontrun{
        #' geom$outward_normal()
        #' }
        outward_normal = function (class = NULL, object = NULL)
            idfgeom_outward_normal(self, private, class, object),
        # }}}

        # centroid {{{
        #' @description
        #' Get centroid
        #'
        #' @details
        #' `$centroid()` returns the centroid of surfaces.
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
        #' * `x`: Numeric type. X axis value.
        #' * `y`: Numeric type. Y axis value.
        #' * `z`: Numeric type. Z axis value.
        #'
        #' @examples
        #' \dontrun{
        #' geom$centroid()
        #' }
        centroid = function (class = NULL, object = NULL)
            idfgeom_centroid(self, private, class, object),
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
        #' @return A [data.table::data.table()] of 4 columns:
        #'
        #' * `id`: Integer type. Object IDs.
        #' * `name`: Character type. Object names.
        #' * `class`: Character type. Class names.
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
        #' @return A [data.table::data.table()] of 4 columns:
        #'
        #' * `id`: Integer type. Object IDs.
        #' * `name`: Character type. Object names.
        #' * `class`: Character type. Class names.
        #' * `tilt`: Numeric type. Azimuth in degree.
        #'
        #' @examples
        #' \dontrun{
        #' geom$tilt()
        #' }
        tilt = function (class = NULL, object = NULL)
            idfgeom_tilt(self, private, class, object),
        # }}}

        # vertices {{{
        #' @description
        #' Extract geometry data
        #'
        #' @details
        #' `$extract()` returns a list that contains data of geometry objects
        #' specified.
        #'
        #' @param type A character vector of valid geometry type names,
        #'        including:
        #'
        #' * `"surface"`
        #' * `"subsurface"`
        #' * `"shading"`
        #' * `"daylighting_point"` (daylighting control reference points)
        #'
        #' If `NULL`, all types will be returned.
        #'
        #' @param class A character vector of valid geometry class names.
        #'        Default: `NULL`.
        #'
        #' @param object A character vector of valid names or an integer
        #'        vector of valid IDs of targeting objects.
        #'        Default: `NULL`.
        #'
        #' @return A list of 4 columns:
        #'
        #' * `id`: Integer type. Object IDs.
        #' * `name`: Character type. Object names.
        #' * `class`: Character type. Class names.
        #' * `x`: Numeric type. X axis value.
        #' * `y`: Numeric type. Y axis value.
        #' * `z`: Numeric type. Z axis value.
        #'
        #' @examples
        #' \dontrun{
        #' geom$extract()
        #' }
        extract = function (type = NULL, class = NULL, object = NULL)
            idfgeom_extract(self, private, type, class, object),
        # }}}

        # view {{{
        #' @description
        #' View 3D geometry
        #'
        #' @details
        #' `$view()` uses the [rgl](https://cran.r-project.org/package=rgl)
        #' package to visualize the IDF geometry in 3D in a similar way as
        #' [OpenStudio](https://www.openstudio.net).
        #'
        #' In the rgl window, you can control the view using your mouse:
        #'
        #' * Left button: Trackball
        #' * Right button: Pan
        #' * Middle button: Field-of-view (FOV). '0' means orthographic
        #'   projection.
        #' * Wheel: Zoom
        #'
        #' @param new If `TRUE`, a new rgl window will be open using
        #'        [rgl::rgl.open()]. If `FALSE`, existing rgl window will be
        #'        reused if possible. Default: `FALSE`.
        #'
        #' @param render_by A single string specifying the way of rendering the
        #'        geometry. Possible values are:
        #'
        #'     * `"surface_type"`: Default. Render the model by surface type
        #'       model. Walls, roofs, windows, doors, floors, and shading
        #'       surfaces will have unqiue colors.
        #'     * `"boundary"`: Render the model by outside boundary condition.
        #'       Only surfaces that have boundary conditions will be rendered
        #'       with a color. All other surfaces will be white.
        #'     * `"construction"`: Render the model by surface constructions.
        #'     * `"zone"`: Render the model by zones assigned.
        #'     * `"normal"`: Render the model by surface normal. The outside
        #'       face of a heat transfer face will be rendered as white and the
        #'       inside face will be rendered as red.
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
        #' @param line_width The line width of wireframes. Default: `1.5`.
        #'
        #' @param line_color The color of wireframes. Default: `black`.
        #'
        #' @param theta The rotation around z-axis in degrees. Default: `0`.
        #'
        #' @param phi The azimuth angle in degrees. Default: `-60`.
        #'
        #' @param fov The field-of-view angle in degrees. `0` means isometric.
        #'        Default: `60`.
        #'
        #' @param zoom The zoom factor. Default: `1`.
        #'
        #' @param background The color of the backports. Default: `"white"`.
        #'
        #' @param size A numeric vector specifying the x-y coordiates, width and
        #'        height of the rgl device. The x-y coordinates are based on the
        #'        original located in the upper left corner of current screen.
        #'        By default, x-y coordinates are set to zeros.
        #'
        #' * If 1 number, the width and height of rgl windows will be
        #'   the same as input number.
        #' * If 2 numbers, the width and height of rgl windows will be
        #'   set based on input number
        #' * If 3 numbers, the first 2 numbers will be used as x-y
        #'   coordinates and the 3rd number will be used for both window
        #'   width and height.
        #' * If 4 numbers, the first 2 numbers will be used as x-y
        #'   coordinates. The next 2 specify the window size.
        #'
        #' @return An [IdfViewer] object
        #'
        #' @examples
        #' \dontrun{
        #' idf$view()
        #' idf$view(render_by = "zone")
        #' idf$view(new, render_by = "construction")
        #' }
        view = function (new = TRUE, clear = TRUE,
                         show = "all", render_by = "surface_type",
                         zone = NULL, surface = NULL,
                         wireframe = TRUE, x_ray = FALSE, axis = TRUE,
                         line_width = 1.5, line_color = "black",
                         theta = 0, phi = -60, fov = 60, zoom = 1,
                         background = "white", size = c(0, 30, 800))
            idfgeom_view(self, private, new = new, clear = clear, render_by = render_by,
                      axis = axis, wireframe = wireframe, surface = surface, x_ray = x_ray,
                      line_width = line_width, line_color = line_color,
                      theta = theta, phi = phi, fov = fov, zoom = zoom,
                      background = background, size = size),
        # }}}

        # save_snapshot {{{
        #' @description
        #' Capture and save current rgl view as an image
        #'
        #' @details
        #' `$save_view()` captures the current rgl view and saves it as an
        #' image file to disk.
        #'
        #' @param filename A single string specifying the file name. Current
        #'        supported formats are `png`, `pdf`, `svg`, `ps`, `eps`, `tex`
        #'        and `pgf`.
        #'
        #' @param autoview If `TRUE`, a new view will be created if there is no
        #'        existing one using
        #'        \href{../../eplusr/html/Idf.html#method-view}{\code{$view()}}
        #'        Default: `FALSE`.
        #'
        #' @param autoclose If `TRUE`, current rgl window will be closed after
        #'        saving. Default: `FALSE`.
        #'
        #' @param bring_to_front If `TRUE`, The rgl window will be brought to
        #'        the front when saving. Default: `FALSE`.
        #'
        #' @param axis If `TRUE`, the X, Y and Z axes will be saved in the
        #'        image. Default: `FALSE`.
        #'
        #' @return The `Idf` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' geom$view()
        #' geom$save_snapshot(tempfile(fileext = ".png"))
        #' }
        #'
        save_snapshot = function (filename, bring_to_front = TRUE, axis = FALSE)
            idfgeom_save_snapshot(self, private, filename, bring_to_front, axis = axis),
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

        # set_wwr
        #
        # rotate
        #
        # scale
        #
        # match
        #
        # intersect
        #
        # translate(vector)
        #
        # global_geom_rules
        # align_coord_system
        #
        # building
        # zone
        # surface
        # subsurface
        # shading
        # daylighting_point
        #
        # mesh3d
        # as.data.table
    ),

    private = list(
        m_parent = NULL,
        m_geoms = NULL,
        m_viewer = NULL,
        m_log = NULL,

        # PRIVATE FUNCTIONS {{{
        uuid = function () private$m_log$uuid,
        log_new_uuid = function () log_new_uuid(private$m_log),

        log_saved = function () log_saved(private$m_log),
        log_unsaved = function () log_unsaved(private$m_log),

        log_new_order = function (id) log_new_order(private$m_log, id),
        log_add_order = function (id) log_add_order(private$m_log, id),
        log_del_order = function (id) log_del_order(private$m_log, id),

        idd_env = function () get_priv_env(private$m_idd)$m_idd_env,
        idf_env = function () private$m_idf_env,

        update_idf_env = function (lst) {
            private$m_idf_env$object <- lst$object
            private$m_idf_env$value <- lst$value
            private$m_idf_env$reference <- lst$reference
        },

        deep_clone = function (name, value) idf_deep_clone(self, private, name, value)
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
    private$m_geoms$rules
}
# }}}
# idfgeom_align_coord {{{
idfgeom_align_coord <- function (self, private, detailed = NULL, simple = NULL, daylighting = NULL) {
    private$m_geoms <- align_coord_system(private$m_geoms, detailed, simple, daylighting)
    invisible()
}
# }}}
# idfgeom_convert {{{
idfgeom_convert <- function (self, private, type = c("surface", "subsurface", "shading")) {
}
# }}}
# idfgeom_round_digits {{{
idfgeom_round_digits <- function (self, private, digits = 4L) {
}
# }}}
# idfgeom_area {{{
idfgeom_area <- function (self, private, class = NULL, object = NULL, net = FALSE) {
}
# }}}
# idfgeom_outward_normal {{{
idfgeom_outward_normal <- function (self, private, class = NULL, object = NULL) {
}
# }}}
# idfgeom_centroid {{{
idfgeom_centroid <- function (self, private, class = NULL, object = NULL) {
}
# }}}
# idfgeom_azimuth {{{
idfgeom_azimuth <- function (self, private, class = NULL, object = NULL) {
}
# }}}
# idfgeom_tilt {{{
idfgeom_tilt <- function (self, private, class = NULL, object = NULL) {
}
# }}}
# idfgeom_extract {{{
idfgeom_extract <- function (self, private, class = NULL, object = NULL) {
}
# }}}
# idfgeom_view {{{
idfgeom_view <- function (self, private, new = TRUE, clear = TRUE, axis = TRUE,
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

    private$m_viewer <- idf_viewer(self)
    private$m_viewer$show()

    invisible(private$m_viewer)
}
# }}}
