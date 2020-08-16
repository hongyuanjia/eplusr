#' @importFrom R6 R6Class
#' @include impl-idf.R
NULL

#' Visualize an EnergyPlus Model Geometry and Simulation Results
#'
#' `IdfViewer` is a class designed to view geometry of an [Idf] and map
#' simulation results to the geometries.
#'
#' @importFrom R6 R6Class
#' @docType class
#' @name IdfViewer
#' @seealso [IdfGeometry] class
#' @author Hongyuan Jia
NULL

# idf_viewer {{{
#' @name IdfViewer
#' @export
idf_viewer <- function (parent) {
    IdfViewer$new(parent)
}
# }}}

# IdfViewer {{{
#' @export
IdfViewer <- R6Class("IdfViewer", cloneable = FALSE,
    public = list(
        # initialize {{{
        #' @description
        #' Create an `IdfGeometry` object
        #'
        #' @param geometry An [IdfGeometry] object. `geometry` can also be a
        #'        path to an IDF file or an [Idf] object. In this case, an
        #'        `IdfGeometry` is created based on input [Idf].
        #'
        #' @return An `IdfViewer` object.
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
        initialize = function (geometry) {
            if (!requireNamespace("rgl", quietly = TRUE)) {
                abort(paste0(
                    "'eplusr' relies on the 'rgl' package to view 3D IDF geometry; ",
                    "please add this to your library with 'install.packages(\"rgl\")' and try agian."
                ))
            }
            if (!requireNamespace("decido", quietly = TRUE)) {
                abort(paste0(
                    "'eplusr' relies on the 'decido' package to view 3D IDF geometry; ",
                    "please add this to your library with 'install.packages(\"decido\")' and try agian."
                ))
            }

            if (inherits(geometry, "IdfGeometry")) {
                private$m_geom <- geometry
            } else if (is_idf(geometry)) {
                private$m_geom <- idf$geometry()
            } else if (is.character(geometry)) {
                private$m_geom <- read_idf(geometry)$geometry()
            }

            # init log env
            private$m_log <- new.env(parent = emptyenv())

            # add a uuid
            private$m_log$uuid <- unique_id()

            # log geomtry id
            private$log_geom_uuid()

            # log data
            private$m_log$geoms <- copy_list(get_priv_env(private$m_geom)$m_geoms)
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
            idfviewer_parent(self, private),
        # }}}

        # geometry {{{
        #' @description
        #' Get parent [IdfGeometry] object
        #'
        #' @details
        #' `$geometry()` returns the parent [IdfGeometry] object.
        #'
        #' @return An [IdfGeometry] object.
        #'
        #' @examples
        #' viewer$geometry()
        geometry = function ()
            idfviewer_geometry(self, private),
        # }}}

        # device {{{
        #' @description
        #' Get Rgl device ID
        #'
        #' @details
        #' If Rgl is used, the Rgl device ID is returned. If WebGL is
        #' used, the `elementID` is returned. If no viewer has been open, `NULL`
        #' is returned.
        #'
        #' @return A number or `NULL`
        #'
        #' @examples
        #' \dontrun{
        #' geom$device()
        #' }
        device = function ()
            idfviewer_device(self, private),
        # }}}

        # background {{{
        #' @description
        #' Set the background color of the scene
        #'
        #' @param color A single string giving the background color. Default:
        #'        `white`.
        #'
        #' @examples
        #' \dontrun{
        #' viewer$background("blue")
        #' }
        background = function (color = "white")
            idfviewer_background(self, private, color),
        # }}}

        # viewpoint {{{
        #' @description
        #' Set the viewpoint orientation of the scene
        #'
        #' @param theta Theta in polar coordinates. Default: `0`.
        #'
        #' @param phi Phi in polar coordinates. Default: `-60`.
        #'
        #' @param fov Field-of-view angle in degrees. If `0`, a parallel or
        #'        orthogonal projection is used. Default: `60`.
        #'
        #' @param zoom Zoom factor. Default: `1`.
        #'
        #' @param scale A numeric vector of length 3 giving the rescaling to
        #'        apply to each axis. Default: `c(1, 1, 1)`.
        #'
        #' @examples
        #' \dontrun{
        #' geom$viewpoint()
        #' }
        viewpoint = function (theta = 0, phi = -60, fov = 60, zoom = 1, scale = c(1, 1, 1))
            idfviewer_viewpoint(self, private, theta, phi, fov, zoom, scale),
        # }}}

        # win_size {{{
        #' @description
        #' Set the window size
        #'
        #' @param left,top,right,bottom A single number indicating the pixels of
        #'        the displayed window. Defaults: `0` (`left`), `0` (`top`),
        #'        `600` (`right`) and `600` (`bottom`).
        #'
        #' @examples
        #' \dontrun{
        #' viewer$win_size(0, 0, 400, 500)
        #' }
        win_size = function (left = 0, top = 0, right = 600, bottom = 600)
            idfviewer_win_size(self, private, left, top, right, bottom),
        # }}}

        # mouse_mode {{{
        #' @description
        #' Set the handlers of mouse control
        #'
        #' @details
        #' Possible values are:
        #'
        #' | Mode                      | Description                                                                                                                      |
        #' | ---                       | ---                                                                                                                              |
        #' | "none"                    | No action                                                                                                                        |
        #' | "trackball"               | The mouse acts as a virtual trackball. Clicking and dragging rotates the scene                                                   |
        #' | "xAxis", "yAxis", "zAxis" | Like "trackball", but restricted to rotation about one axis                                                                      |
        #' | "polar"                   | The mouse affects rotations by controlling polar coordinates directly                                                            |
        #' | "zoom"                    | The mouse zooms the display                                                                                                      |
        #' | "fov"                     | The mouse affects perspective by changing the field of view                                                                      |
        #' | "pull"                    | Rotating the mouse wheel towards the user “ pulls the scene closer”                                                              |
        #' | "push"                    | The same rotation “pushes the scene away”                                                                                        |
        #' | "pan"                     | Pan the camera view vertically or horizontally                                                                                   |
        #'
        #' @param left,right,middle Refer to the buttons on a three button
        #'        mouse, or simulations of them on other mice. Defaults:
        #'        `"trackball"` (`left`), `"pan"` (`right`) and `"fov"`
        #'        (`middle`).
        #'
        #' @param wheel Refer to the mouse wheel. Default: `"push"`.
        #'
        #' @examples
        #' \dontrun{
        #' viewer$mouse_mode()
        #' }
        mouse_mode = function (left = "trackball", right = "pan", middle = "fov", wheel = "push")
            idfviewer_mouse_mode(self, private, left, right, middle, wheel),
        # }}}

        # axis {{{
        #' @description
        #' Toggle axis in the scene
        #'
        #' @details
        #' `$axis()` adds or removes X, Y and Z axis in the scene.
        #'
        #' @param add If `TRUE`, axis is added to the scene. If `FALSE`, axis is
        #'        removed in the scene.
        #'
        #' @param expand A single number giving the factor to expand based on
        #'        the largest X, Y and Z coordinate values. Default: `1.02`.
        #'
        #' @param width A number giving the line width of axis. Default: `1.5`.
        #'
        #' @param color A character of length 3 giving the color of X, Y and Z
        #'        axis. Default: `c("red", "green", "blue")`.
        #'
        #' @param alpha A number giving the alpha value of axis. Default: `1.0`.
        #'
        #' @return A single logical value as `add`.
        #'
        #' @examples
        #' \dontrun{
        #' viewer$axis()
        #' }
        axis = function (add = TRUE, expand = 1.02, width = 1.5, color = c("red", "green", "blue"), alpha = 1.0)
            idfviewer_axis(self, private, add, expand, width, color, alpha),
        # }}}

        # ground {{{
        #' @description
        #' Toggle ground in the scene
        #'
        #' @details
        #' `$ground()` adds or removes ground in the scene.
        #'
        #' @param add If `TRUE`, ground is added to the scene. If `FALSE`,
        #'        ground is removed in the scene.
        #'
        #' @param expand A single number giving the factor to expand based on
        #'        the largest X, Y and Z coordinate values. Default: `1.02`.
        #'
        #' @param color A string giving the color of ground. Default: `#EDEDEB`.
        #'
        #' @param alpha A number giving the alpha value of ground. Default: `1.0`.
        #'
        #' @return A single logical value as `add`.
        #'
        #' @examples
        #' \dontrun{
        #' viewer$ground()
        #' }
        ground = function (add = TRUE, expand = 1.02, color = "#EDEDEB", alpha = 1.0)
            idfviewer_ground(self, private, add, expand, color, alpha),
        # }}}

        # wireframe {{{
        #' @description
        #' Toggle wireframe
        #'
        #' @details
        #' `$wireframe()` turns on/off wireframes.
        #'
        #' @param add If `TRUE`, wireframe is turned on. If `FALSE`, wireframe
        #'        is turned off. Default: `TRUE`.
        #'
        #' @param width A number giving the line width of axis. Default: `1.5`.
        #'
        #' @param color A character of length 3 giving the color of X, Y and Z
        #'        axis. Default: `c("red", "green", "blue")`.
        #'
        #' @param alpha A number giving the alpha value of axis. Default: `1.0`.
        #'
        #' @return A single logical value as `add`.
        #'
        #' @examples
        #' \dontrun{
        #' viewer$wireframe()
        #' }
        wireframe = function (add = TRUE, width = 1.5, color = "black", alpha = 1.0)
            idfviewer_wireframe(self, private, add, width, color, alpha),
        # }}}

        # x_ray {{{
        #' @description
        #' Toggle X-ray face style
        #'
        #' @details
        #' `$x_ray()` turns on/off X-ray face style.
        #'
        #' @param on If `TRUE`, X-ray is turned on. If `FALSE`, X-ray is turned
        #'        off. Default: `TRUE`.
        #'
        #' @return A single logical value as `on`.
        #'
        #' @examples
        #' \dontrun{
        #' viewer$x_ray()
        #' }
        x_ray = function (on = TRUE)
            idfviewer_x_ray(self, private, on),
        # }}}

        # render_by {{{
        #' @description
        #' Set render style
        #'
        #' @details
        #' `$render_by()` sets the render style of geometries.
        #'
        #' @param type A single string giving the render style. Should be one
        #' of:
        #'
        #' * `"surface_type"`: Default. Render the model by surface type
        #'   model. Walls, roofs, windows, doors, floors, and shading
        #'   surfaces will have unqiue colors.
        #' * `"boundary"`: Render the model by outside boundary condition.
        #'   Only surfaces that have boundary conditions will be rendered
        #'   with a color. All other surfaces will be white.
        #' * `"construction"`: Render the model by surface constructions.
        #' * `"zone"`: Render the model by zones assigned.
        #' * `"normal"`: Render the model by surface normal. The outside
        #'   face of a heat transfer face will be rendered as white and the
        #'   inside face will be rendered as red.
        #'
        #' @return A same value as `style`.
        #'
        #' @examples
        #' \dontrun{
        #' viewer$render_by()
        #' }
        render_by = function (type = "surface_type")
            idfviewer_render_by(self, private, type),
        # }}}

        # show {{{
        #' @description
        #' Show [Idf] geometry
        #'
        #' @param type A character vector of geometry components to show. If
        #'        `"all"`, all geometry components will be shown. Otherwise,
        #'        should be a subset of following:
        #'
        #' * `"floor"`
        #' * `"wall"`
        #' * `"roof"`
        #' * `"window"`
        #' * `"door"`
        #' * `"shading"`
        #' * `"daylighting"`
        #'
        #' @param zone A character vector of names or an integer vector of IDs
        #'        of zones in current [Idf] to show. If `NULL`, no subsetting is
        #'        performed.
        #'
        #' @param surface A character vector of names or an integer vector of IDs
        #'        of surfaces in current [Idf] to show. If `NULL`, no subsetting
        #'        is performed.
        #'
        #' @param add If `TRUE`, compoents will be added to the existing scene.
        #'        Otherwise, existing components will be removed before showing
        #'        the new ones.
        #'
        #' @return The `IdfViewer` itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' viewer$show()
        #' }
        show = function (type = c("all", "floor", "wall", "roof", "window", "door", "shading", "daylighting"),
                         zone = NULL, surface = NULL, add = TRUE)
            idfviewer_show(self, private, type, zone, surface, add),
        # }}}

        # focus {{{
        #' @description
        #' Bring the scene window to the top
        #'
        #' @examples
        #' \dontrun{
        #' viewer$top()
        #' }
        focus = function ()
            idfviewer_focus(self, private),
        # }}}

        # close {{{
        #' @description
        #' Close the scene window
        #'
        #' @examples
        #' \dontrun{
        #' viewer$close()
        #' }
        close = function ()
            idfviewer_close(self, private),
        # }}}

        # snapshot {{{
        #' @description
        #' Capture and save current rgl view as an image
        #'
        #' @details
        #' `$snapshot()` captures the current rgl view and saves it as an image
        #' file to disk.
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
        #' @return A single string of the file path.
        #'
        #' @examples
        #' \dontrun{
        #' viewer$show()
        #' viewer$snapshot(tempfile(fileext = ".png"))
        #' }
        #'
        snapshot = function (filename, bring_to_front = TRUE)
            idfviewer_snapshot(self, private, filename, bring_to_front)
        # }}}

        # mesh3d
        # as.data.table
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_geom = NULL,
        m_device = NULL,
        m_background = "white",
        m_viewpoint = list(theta = 0, phi = -60, fov = 60, zoom = 1, scale = c(1, 1, 1)),
        m_mouse_mode = list(left = "trackball", right = "pan", middle = "fov", wheel = "push"),
        m_x_ray = FALSE,
        m_show_hidden = FALSE,
        m_render_by = "surface_type",
        m_win_size = c(0, 0, 600, 600),
        m_axis = list(expand = 1.02, width = 1.5, color = c("red", "green", "blue"), alpha = 1.0),
        m_ground = list(expand = 1.02, color = "#EDEDEB", alpha = 1.0),
        m_wireframe = list(width = 1.5, color = "black", alpha = 1.0),
        m_id_axis = NULL,
        m_id_ground = NULL,
        m_id_wireframe = NULL,
        m_id_surface = NULL,
        m_id_dayl = NULL,
        m_log = NULL,
        # }}}

        # PRIVATE FUNCTIONS {{{
        uuid = function () private$m_log$uuid,
        log_new_uuid = function () log_new_uuid(private$m_log),

        geom_uuid = function () get_priv_env(private$m_geom)$m_log$uuid,
        log_geom_uuid = function () private$m_log$geom_uuid <- get_priv_env(private$m_geom)$m_log$uuid,
        cached_geom_uuid = function () private$m_log$geom_uuid,

        geoms = function () {
            if (private$geom_uuid() != private$cached_geom_uuid()) {
                private$m_log$geoms <- copy_list(get_priv_env(private$m_geom)$m_geoms)
            }

            private$m_log$geoms <- align_coord_system(
                private$m_log$geoms, "absolute", "absolute", "absolute"
            )

            private$m_log$geoms
        }
        # }}}
    )
)
# }}}

# idfviewer_parent {{{
idfviewer_parent <- function (self, private) {
    private$m_geom$parent()
}
# }}}
# idfviewer_geometry {{{
idfviewer_geometry <- function (self, private) {
    private$m_geom
}
# }}}
# idfviewer_device {{{
idfviewer_device <- function (self, private) {
    # in case the window has been killed accidentally
    if (length(private$m_device) && !private$m_device %in% rgl::rgl.dev.list()) {
        private$m_device <- NULL
    }
    private$m_device
}
# }}}
# idfviewer_background {{{
idfviewer_background <- function (self, private, color = "white") {
    assert_string(color)
    private$m_background <- color
    if (!is.null(self$device())) rgl::rgl.bg(color = color)
    invisible()
}
# }}}
# idfviewer_viewpoint {{{
idfviewer_viewpoint <- function (self, private, theta = 0, phi = -60, fov = 60, zoom = 1, scale = c(1, 1, 1)) {
    private$m_viewpoint <- list(theta = theta, phi = phi, fov = fov, zoom = zoom, scale = scale)
    if (!is.null(self$device())) do.call(rgl::rgl.viewpoint, private$m_viewpoint)
    invisible()
}
# }}}
# idfviewer_win_size {{{
idfviewer_win_size <- function (self, private, left = 0, top = 0, right = 600, bottom = 600) {
    assert_number(left, lower = 0, finite = TRUE)
    assert_number(top, lower = 0, finite = TRUE)
    assert_number(right, lower = 0, finite = TRUE)
    assert_number(bottom, lower = 0, finite = TRUE)

    private$m_win_size <- c(left = left, top = top, right = right, bottom = bottom)
    if (!is.null(self$device())) {
        rgl::par3d(dev = private$m_device, windowRect = private$m_win_size)
    }
    invisible()
}
# }}}
# idfviewer_mouse_mode {{{
idfviewer_mouse_mode <- function (self, private, left = "trackball", right = "pan", middle = "fov", wheel = "push") {
    choices <- c("none", "trackball", "xAxis", "yAxis", "zAxis", "polar", "zoom", "fov", "pull", "push", "pan")
    assert_choice(left, choices)
    assert_choice(right, choices)
    assert_choice(middle, choices)
    assert_choice(wheel, c("none", "pull", "push"))

    if (length(self$device())) {
        cur <- rgl::par3d(dev = private$m_device, "mouseMode")
        pan_but <- 0L
        if (left == "pan") pan_but <- 1L else cur[["left"]] <- left
        if (right == "pan") pan_but <- 2L else cur[["right"]] <- right
        if (middle == "pan") pan_but <- 3L else cur[["middle"]] <- middle
        cur[["wheel"]] <- wheel

        rgl::par3d(dev = private$m_device, mouseMode = cur)
        if (pan_but > 0L) pan3d(pan_but)
    }

    private$m_mouse_mode <- list(left = left, right = right, middle = middle, wheel = wheel)
    invisible()
}
# }}}
# idfviewer_axis {{{
idfviewer_axis <- function (self, private, add = TRUE, expand = 1.02, width = 1.5, color = c("red", "green", "blue"), alpha = 1.0) {
    assert_flag(add)
    assert_number(expand, lower = 1.0)
    assert_number(width, lower = 1E-5, finite = TRUE)
    assert_character(color, len = 3, any.missing = TRUE)
    assert_number(alpha, lower = 0, upper = 1, finite = TRUE)

    if (add) {
        private$m_axis <- list(expand = expand, width = width, color = color, alpha = alpha)
        if (length(self$device())) {
            # only extent when nothing else
            if (length(private$m_id_wireframe) || length(private$m_id_surface) || length(private$m_id_dayl)) {
                par <- rgl::par3d(dev = private$m_device, "ignoreExtent")
                on.exit(rgl::par3d(dev = private$m_device, ignoreExtent = par), add = TRUE)
                rgl::par3d(dev = private$m_device, ignoreExtent = TRUE)
            }

            if (length(private$m_id_axis)) {
                rgl_pop(id = private$m_id_axis)
            }

            private$m_id_axis <- do.call(rgl_view_axis,
                c(list(dev = private$m_device, geoms = private$geoms()), private$m_log$axis))
        }
    } else {
        if (length(self$device()) && length(private$m_id_axis)) {
            par <- rgl::par3d(dev = private$m_device, "ignoreExtent")
            on.exit(rgl::par3d(dev = private$m_device, ignoreExtent = par), add = TRUE)
            rgl::par3d(dev = private$m_device, ignoreExtent = TRUE)

            rgl_pop(id = private$m_id_axis)
        }
        private$m_id_axis <- NULL
        private$m_axis <- NULL
    }
    invisible(add)
}
# }}}
# idfviewer_ground {{{
idfviewer_ground <- function (self, private, add = TRUE, expand = 1.02, color = "#EDEDEB", alpha = 1.0) {
    assert_flag(add)
    assert_number(expand, lower = 1.0)
    assert_string(color)
    assert_number(alpha, lower = 0, upper = 1, finite = TRUE)

    if (add) {
        private$m_log$ground <- list(expand = expand, color = color, alpha = alpha)
        if (length(self$device())) {
            # only extent when nothing else
            if (length(private$m_id_wireframe) || length(private$m_id_surface) || length(private$m_id_dayl)) {
                par <- rgl::par3d(dev = private$m_device, "ignoreExtent")
                on.exit(rgl::par3d(dev = private$m_device, ignoreExtent = par), add = TRUE)
                rgl::par3d(dev = private$m_device, ignoreExtent = TRUE)
            }

            if (length(private$m_id_ground)) {
                rgl_pop(id = private$m_id_ground)
            }

            private$m_id_ground <- do.call(rgl_view_ground,
                c(list(dev = private$m_device, geoms = private$geoms()), private$m_log$ground))
        }
    } else {
        if (length(self$device()) && length(private$m_id_ground)) {
            # only extent when nothing else
            if (length(private$m_id_wireframe) || length(private$m_id_surface) || length(private$m_id_dayl)) {
                par <- rgl::par3d(dev = private$m_device, "ignoreExtent")
                on.exit(rgl::par3d(dev = private$m_device, ignoreExtent = par), add = TRUE)
                rgl::par3d(dev = private$m_device, ignoreExtent = TRUE)
            }
            rgl_pop(id = private$m_id_ground)
        }
        private$m_id_ground <- NULL
        private$m_log$ground <- NULL
    }
    invisible(add)
}
# }}}
# idfviewer_wireframe {{{
idfviewer_wireframe <- function (self, private, add = TRUE, width = 1.5, color = "black", alpha = 1.0) {
    assert_flag(add)
    assert_number(width, lower = 1E-5, finite = TRUE)
    assert_string(color)
    assert_number(alpha, lower = 0, upper = 1, finite = TRUE)

    if (add) {
        private$m_wireframe <- list(width = width, color = color, alpha = alpha)
        if (length(self$device())) {
            if (length(id <- unlist(private$m_id_wireframe, use.names = FALSE))) {
                rgl_pop("shapes", id = id)
            }

            geoms <- private$geoms()
            private$m_id_wireframe <- list(
                surface = do.call(rgl_view_wireframe, c(list(dev = private$m_device, geom = geoms$surface), private$m_wireframe)),
                subsurface = do.call(rgl_view_wireframe, c(list(dev = private$m_device, geom = geoms$subsurface), private$m_wireframe)),
                shading = do.call(rgl_view_wireframe, c(list(dev = private$m_device, geom = geoms$shading), private$m_wireframe))
            )
        }
    } else {
        if (length(self$device())) {
            if (length(id <- unlist(private$m_id_wireframe, use.names = FALSE))) {
                rgl_pop("shapes", id = id)
            }
        }
        private$m_id_wireframe <- NULL
        private$m_wireframe <- NULL
    }
    invisible(add)
}
# }}}
# idfviewer_x_ray {{{
idfviewer_x_ray <- function (self, private, on = TRUE) {
    assert_flag(on)

    private$m_x_ray <- on

    if (length(self$device()) && length(private$m_id_surface)) {
        if (length(id <- unlist(private$m_id_surface, use.names = FALSE))) {
            rgl_pop(id = id)
        }
        private$m_id_surface <- private$m_id_surface <- rgl_view_surface(
            private$m_device, private$m_log$geoms_show,
            type = private$m_render_by, x_ray = private$m_x_ray
        )
    }

    invisible(on)
}
# }}}
# idfviewer_render_by {{{
idfviewer_render_by <- function (self, private, style = c("surface_type", "boundary", "construction", "zone", "normal")) {
    assert_choice(style, c("surface_type", "boundary", "construction", "zone", "normal"))

    private$m_render_by <- style

    if (length(self$device()) && length(private$m_id_surface)) {
        if (length(id <- unlist(private$m_id_surface, use.names = FALSE))) {
            rgl_pop(id = id)
        }
        private$m_id_surface <- private$m_id_surface <- rgl_view_surface(
            private$m_device, private$m_log$geoms_show,
            type = private$m_render_by, x_ray = private$m_x_ray
        )
    }

    invisible(style)
}
# }}}
# idfviewer_show {{{
idfviewer_show <- function (self, private, type = "all", zone = NULL, surface = NULL, add = TRUE) {
    geoms <- subset_geom(private$geoms(), type = type, zone = zone, surface = surface)

    # open a new device
    if (!length(self$device())) {
        rgl::rgl.open()
        private$m_device <- rgl::rgl.cur()

        # apply window size
        rgl::par3d(dev = private$m_device, windowRect = private$m_win_size)

        # apply viewpoint
        do.call(rgl::rgl.viewpoint, private$m_viewpoint)

        # apply background
        rgl::rgl.bg(color = private$m_background)

        # apply mouse mode
        do.call(self$mouse_mode, private$m_mouse_mode)

        # add axis
        if (length(private$m_axis)) do.call(self$axis, private$m_axis)

        # add ground
        if (length(private$m_ground)) do.call(self$ground, private$m_ground)

        # add wireframe
        if (length(private$m_wireframe)) do.call(self$wireframe, private$m_wireframe)
    }

    if (length(private$m_id_dayl)) {
        rgl_pop(id = private$m_id_dayl)
        private$m_id_dayl <- NULL
    }

    if (any(c("all", "daylighting") %chin% type)) {
        private$m_id_dayl <- rgl_view_point(private$m_device, geoms$daylighting_point)
    }

    if (length(private$m_id_surface)) {
        if (length(id <- unlist(private$m_id_surface, use.names = FALSE))) {
            rgl_pop(id = id)
        }
        private$m_id_surface <- NULL
    }

    private$m_id_surface <- rgl_view_surface(private$m_device,
        geoms, type = private$m_render_by, x_ray = private$m_x_ray
    )

    private$m_log$geoms_show <- geoms

    invisible()
}
# }}}
# idfviewer_focus {{{
idfviewer_focus <- function (self, private) {
    if (is.null(self$device())) {
        verbose_info("No viewer window found. Skip.")
    } else {
        rgl::rgl.set(private$m_device)
        rgl::rgl.bringtotop()
    }

    invisible()
}
# }}}
# idfviewer_close {{{
idfviewer_close <- function (self, private) {
    if (is.null(self$device())) {
        verbose_info("No viewer window found. Skip.")
    } else {
        rgl::rgl.set(private$m_device)
        rgl::rgl.close()
    }

    invisible()
}
# }}}
# idfviewer_snapshot {{{
idfviewer_snapshot <- function (self, private, filename, bring_to_front = TRUE) {
    if (!requireNamespace("rgl", quietly = TRUE)) {
        abort(paste0("'eplusr' relies on the 'rgl' package to view 3D IDF geometry; ",
            "please add this to your library with install.packages('rgl') and try agian."
        ))
    }

    if (!length(self$device())) {
        abort("No viewer window currently open. Please run '$show()' first.")
    }

    # set the last plot device as active
    rgl::rgl.set(private$m_device)

    if (has_ext(filename, "png")) {
        rgl::rgl.snapshot(filename, "png", top = bring_to_front)
    } else if (has_ext(filename, c("ps", "eps", "tex", "pdf", "svg", "pgf"))) {
        if (bring_to_front) rgl::rgl.bringtotop()
        rgl::rgl.postscript(filename, tools::file_ext(filename))
    } else {
        abort(paste0("Not supported export format ", surround(tools::file_ext(filename)), ". ",
            "Current supported: ", collapse(c("png", "ps", "eps", "tex", "pdf", "svg", "pgf"), max_num = 10)
        ))
    }

    filename
}
# }}}

