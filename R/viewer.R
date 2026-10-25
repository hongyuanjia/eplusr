#' @importFrom R6 R6Class
#' @include impl-idf.R
#' @include impl-viewer-webgl.R
#' @include viewer-provider.R
NULL

#' Visualize an EnergyPlus Model Geometry and Simulation Results
#'
#' `IdfViewer` renders an [IdfGeometry] in a browser using Three.js. Geometry is
#' transferred as compact binary buffers; metadata and viewer options remain
#' small JSON structures. Browser interactions include surface picking,
#' multi-selection, Zone/Space filtering, isolation, clipping, result coloring,
#' standard viewpoints, wireframes, X-ray rendering, and snapshots.
#'
#' @docType class
#' @name IdfViewer
#' @seealso [IdfGeometry]
#' @author Hongyuan Jia
NULL

# idf_viewer {{{
#' Create an `IdfViewer` object
#'
#' @param geometry An [IdfGeometry], [Idf], or path to an IDF file.
#'
#' @return An `IdfViewer` object.
#' @name IdfViewer
#' @export
idf_viewer <- function(geometry) {
    IdfViewer$new(geometry)
}
# }}}

# IdfViewer {{{
#' @export
IdfViewer <- R6Class(
    "IdfViewer",
    cloneable = FALSE,
    public = list(
        # initialize {{{
        #' @description Create an `IdfViewer` object.
        #' @param geometry An [IdfGeometry], [Idf], or path to an IDF file.
        initialize = function(geometry) {
            if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
                abort(paste0(
                    "'eplusr' uses the 'htmlwidgets' package to view 3D IDF geometry; ",
                    "install it with install.packages('htmlwidgets') and try again."
                ))
            }
            if (!requireNamespace("decido", quietly = TRUE)) {
                abort(paste0(
                    "'eplusr' uses the 'decido' package to triangulate IDF geometry; ",
                    "install it with install.packages('decido') and try again."
                ))
            }

            if (inherits(geometry, "IdfGeometry")) {
                private$m_geom <- geometry
            } else if (is_idf(geometry)) {
                private$m_geom <- geometry$geometry()
            } else if (is.character(geometry) && length(geometry) == 1L) {
                private$m_geom <- read_idf(geometry)$geometry()
            } else {
                abort("'geometry' must be an IdfGeometry, Idf, or IDF path.")
            }

            # Keep the element ID stable across repeated standalone or local HTTP renders.
            private$m_element_id <- paste0(
                "eplusr-viewer-",
                gsub("[^A-Za-z0-9_-]", "", unique_id())
            )
        },
        # }}}

        # parent {{{
        #' @description Return the parent [Idf].
        parent = function() private$m_geom$parent(),
        # }}}

        # geometry {{{
        #' @description Return the parent [IdfGeometry].
        geometry = function() private$m_geom,
        # }}}

        # device {{{
        #' @description Return the current htmlwidget element ID, or `NULL` before rendering.
        device = function() {
            if (is.null(private$m_widget)) NULL else private$m_element_id
        },
        # }}}

        # widget {{{
        #' @description Build a portable or live Three.js htmlwidget.
        #' @param width,height Optional CSS or pixel dimensions.
        #' @param frame_endpoint Optional external result-frame URL.
        #' @param live Start a lightweight loopback connection to this R session.
        #'   Use TRUE for R commands and selection readback; FALSE for reports.
        widget = function(
            width = NULL,
            height = NULL,
            frame_endpoint = NULL,
            live = FALSE
        ) {
            assert_flag(live)
            if (is.null(width)) {
                width <- private$m_size$width
            }
            if (is.null(height)) {
                height <- private$m_size$height
            }
            provider_mode <- length(private$m_results) &&
                all(vapply(
                    private$m_results,
                    function(spec) identical(spec$mode, "provider"),
                    logical(1L)
                ))
            provider <- if (provider_mode) self$frame_provider() else NULL
            if (provider_mode && !live) {
                assert_string(frame_endpoint, min.chars = 1L)
            }
            scene <- viewer__build_idf_view_scene(
                private$m_geom,
                options = private$options(),
                results = if (provider_mode) list() else private$m_results,
                frame_provider = provider,
                frame_endpoint = frame_endpoint %||% "/frames"
            )
            if (live) {
                # A new payload gets its own scene identity and closes the prior channel.
                private$stop_live()
                private$m_live <- viewer__session(
                    scene,
                    provider,
                    private$m_on_select
                )
                private$m_live$query_results <- private$query_results
                scene$live <- private$m_live$config
                if (provider_mode) {
                    scene$results <- provider$metadata(
                        scene$live$frame_endpoint
                    )
                }
            }
            if (!is.null(private$m_sql_source)) {
                objects <- viewer__selected_objects(
                    scene$objects,
                    vapply(
                        scene$objects,
                        function(object) as.character(object$id),
                        ""
                    )
                )
                scene$results <- viewer__result_query_payload(
                    scene$results,
                    private$m_sql_source,
                    private$m_sql_filters,
                    objects,
                    live
                )
            }
            private$m_widget <- viewer__create_idf_view_widget(
                scene,
                width = width,
                height = height,
                element_id = private$m_element_id
            )
            private$m_widget
        },
        # }}}        # background {{{
        #' @description Set the scene background color.
        #' @param color Any CSS color understood by Three.js.
        background = function(color = "#f3f5f7") {
            assert_string(color)
            private$m_background <- color
            if (!is.null(private$m_live)) {
                private$m_live$send("background", list(value = color))
            }
            invisible()
        },
        # }}}

        # viewpoint {{{
        #' @description Set the initial camera viewpoint.
        #' @param look_at One of `iso`, `top`, `bottom`, `front`, `back`, `left`, or `right`.
        #' @param theta,phi,fov,zoom,scale Compatibility arguments retained from the former viewer.
        viewpoint = function(
            look_at = "iso",
            theta = NULL,
            phi = NULL,
            fov = NULL,
            zoom = NULL,
            scale = NULL
        ) {
            assert_choice(
                look_at,
                c("iso", "top", "bottom", "front", "back", "left", "right")
            )
            assert_number(fov, lower = 1, upper = 179, null.ok = TRUE)
            private$m_viewpoint <- list(
                look_at = look_at,
                theta = theta,
                phi = phi,
                fov = fov,
                zoom = zoom,
                scale = scale
            )
            if (!is.null(private$m_live)) {
                private$m_live$send("view", list(value = look_at))
            }
            invisible()
        },
        # }}}

        # win_size {{{
        #' @description Set widget dimensions using the former window rectangle convention.
        #' @param left,top,right,bottom Window coordinates in pixels.
        win_size = function(left = 0, top = 0, right = 900, bottom = 640) {
            assert_number(left, lower = 0, finite = TRUE)
            assert_number(top, lower = 0, finite = TRUE)
            assert_number(right, lower = left, finite = TRUE)
            assert_number(bottom, lower = top, finite = TRUE)
            private$m_size <- list(width = right - left, height = bottom - top)
            invisible()
        },
        # }}}

        # mouse_mode {{{
        #' @description Configure browser mouse controls.
        #' @param left,right,middle,wheel Mouse action names retained for API compatibility.
        mouse_mode = function(
            left = "trackball",
            right = "pan",
            middle = "fov",
            wheel = "pull"
        ) {
            choices <- c(
                "none",
                "trackball",
                "polar",
                "zoom",
                "fov",
                "pull",
                "push",
                "pan"
            )
            assert_choice(left, choices)
            assert_choice(right, choices)
            assert_choice(middle, choices)
            assert_choice(wheel, c("none", "pull", "push", "zoom"))
            private$m_mouse_mode <- list(
                left = left,
                right = right,
                middle = middle,
                wheel = wheel
            )
            invisible()
        },
        # }}}

        # axis {{{
        #' @description Toggle coordinate axes.
        #' @param add Whether axes are shown.
        #' @param expand,width,color,alpha Compatibility styling arguments.
        axis = function(
            add = TRUE,
            expand = 2,
            width = 1.5,
            color = c("red", "green", "blue", "orange"),
            alpha = 1
        ) {
            assert_flag(add)
            private$m_axis <- add
            if (!is.null(private$m_live)) {
                private$m_live$send("axis", list(on = add))
            }
            invisible(add)
        },
        # }}}

        # ground {{{
        #' @description Toggle the ground plane.
        #' @param add Whether the ground is shown.
        #' @param expand,color,alpha Ground styling options.
        ground = function(
            add = TRUE,
            expand = 1.02,
            color = "#e3e5e7",
            alpha = 1
        ) {
            assert_flag(add)
            private$m_ground <- add
            if (!is.null(private$m_live)) {
                private$m_live$send("ground", list(on = add))
            }
            invisible(add)
        },
        # }}}

        # wireframe {{{
        #' @description Toggle surface boundary wireframes.
        #' @param add Whether wireframes are shown.
        #' @param width,color,alpha Wireframe styling options.
        wireframe = function(
            add = TRUE,
            width = 1.5,
            color = "#24282f",
            alpha = 1
        ) {
            assert_flag(add)
            private$m_wireframe <- add
            if (!is.null(private$m_live)) {
                private$m_live$send("wireframe", list(on = add))
            }
            invisible(add)
        },
        # }}}

        # x_ray {{{
        #' @description Toggle translucent X-ray rendering.
        #' @param on Whether X-ray rendering is enabled.
        x_ray = function(on = TRUE) {
            assert_flag(on)
            private$m_x_ray <- on
            if (!is.null(private$m_live)) {
                private$m_live$send("x_ray", list(on = on))
            }
            invisible(on)
        },
        # }}}

        # render_by {{{
        #' @description Set the categorical or numerical render style.
        #' @param type One of `surface_type`, `boundary`, `construction`, `zone`,
        #' `space`, `normal`, or `result` after calling `$color_by()`.
        render_by = function(type = "surface_type") {
            choices <- c(
                "surface_type",
                "boundary",
                "construction",
                "zone",
                "space",
                "normal",
                "result"
            )
            assert_choice(type, choices)
            if (type == "result" && !length(private$m_results)) {
                abort(
                    "Call '$color_by()' or '$results()' before rendering simulation results."
                )
            }
            private$m_render_by <- type
            if (!is.null(private$m_live)) {
                private$m_live$send("render_by", list(value = type))
            }
            invisible(type)
        },
        # }}}

        # color_by {{{
        #' @description Map numerical data to surfaces and enable result coloring.
        #' @param data A data frame containing object keys, values, and optional times.
        #' @param value Names of one or more numeric result columns.
        #' @param object Result scope: `surface`, `zone`, `space`, or `building`.
        #' @param key Optional object-key column. It is inferred from common names by default.
        #' @param time Optional timestamp or period column.
        #' @param label,units Optional legend labels and units. Scalar values are
        #'   recycled across result columns.
        #' @param palette One or more palettes: `viridis`, `diverging`,
        #'   `grayscale`, `sequential`, or `rainbow`.
        #' @param limits Optional numeric limits of length two, or a list with
        #'   one pair per result column.
        #' @param mode `embed` packs all frames into the widget; `provider`
        #'   exposes on-demand frames through [IdfViewerFrameProvider].
        color_by = function(
            data,
            value,
            object = "surface",
            key = NULL,
            time = NULL,
            label = NULL,
            units = NULL,
            palette = "viridis",
            limits = NULL,
            mode = "embed"
        ) {
            assert_data_frame(data, min.rows = 1L)
            assert_character(value, min.len = 1L, any.missing = FALSE)
            assert_choice(object, c("surface", "zone", "space", "building"))
            assert_string(key, null.ok = TRUE)
            assert_string(time, null.ok = TRUE)
            assert_character(label, any.missing = FALSE, null.ok = TRUE)
            assert_character(units, any.missing = FALSE, null.ok = TRUE)
            assert_subset(
                palette,
                c("viridis", "diverging", "grayscale", "sequential", "rainbow")
            )
            assert_choice(mode, c("embed", "provider"))
            limit_list <- viewer__normalize_viewer_limits(limits, length(value))
            private$reset_frame_provider()
            labels <- rep_len(label %||% value, length(value))
            unit_values <- rep_len(units %||% "", length(value))
            palettes <- rep_len(palette, length(value))
            private$m_sql_source <- NULL
            private$m_results <- lapply(seq_along(value), function(index) {
                list(
                    source_type = "data",
                    data = data,
                    value = value[[index]],
                    object = object,
                    key = key,
                    time = time,
                    label = labels[[index]],
                    units = unit_values[[index]],
                    palette = palettes[[index]],
                    limits = limit_list[[index]],
                    mode = mode
                )
            })
            private$m_render_by <- "result"
            private$refresh_results()
            invisible(self)
        },
        # }}}

        # results {{{
        #' @description Attach EnergyPlus SQLite output variables to the model.
        #' @param source An [EplusSql] or completed [EplusJob]. NULL uses the parent Idf's last_job().
        #' @param name Alias of variable, matching EplusSql$report_data().
        #' @param key_value Result keys or a key_value/name dictionary data.frame.
        #' @param year,tz,period,month,day,hour,minute,interval,simulation_days,day_type Selectors with the same semantics as EplusSql$report_data().
        #' @details Live widgets expose source variable, environment, frequency, key and calendar selectors under Results > Select result data. case, all and wide describe table output and do not apply to a 3D viewer.
        #' @param variable Optional variable names. `NULL` selects spatial
        #'   Surface, Zone, and Space variables discoverable from the result dictionary.
        #' @param reporting_frequency Optional reporting frequencies.
        #' @param environment_name Optional environment period names.
        #' @param palette Default result palette.
        #' @param limits Optional named list of display limits by variable name.
        #' @param mode `embed` stores all selected frames; `provider` queries
        #'   the attached EnergyPlus SQLite file lazily from an local connection.
        results = function(
            source = NULL,
            variable = NULL,
            reporting_frequency = NULL,
            environment_name = NULL,
            palette = "viridis",
            limits = NULL,
            mode = "embed",
            key_value = NULL,
            name = NULL,
            year = NULL,
            tz = NULL,
            period = NULL,
            month = NULL,
            day = NULL,
            hour = NULL,
            minute = NULL,
            interval = NULL,
            simulation_days = NULL,
            day_type = NULL
        ) {
            if (is.null(source)) {
                source <- self$parent()$last_job()
            }
            if (is.null(source)) {
                abort(
                    "The parent Idf has no last job. Supply an existing EplusJob or EplusSql; no simulation is started automatically."
                )
            }
            assert_character(variable, any.missing = FALSE, null.ok = TRUE)
            assert_character(name, any.missing = FALSE, null.ok = TRUE)
            if (
                !is.null(variable) &&
                    !is.null(name) &&
                    !setequal(tolower(variable), tolower(name))
            ) {
                abort("'variable' and 'name' are aliases; supply only one.")
            }
            assert_character(
                reporting_frequency,
                any.missing = FALSE,
                null.ok = TRUE
            )
            assert_character(
                environment_name,
                any.missing = FALSE,
                null.ok = TRUE
            )
            assert_string(tz, null.ok = TRUE)
            if (!is.null(tz) && !tz %chin% OlsonNames()) {
                abort("Unknown time zone.")
            }
            assert_choice(
                palette,
                c("viridis", "diverging", "grayscale", "sequential", "rainbow")
            )
            assert_choice(mode, c("embed", "provider"))
            filters <- list(
                name = name %||% variable,
                key_value = key_value,
                year = year,
                tz = tz,
                period = period,
                month = month,
                day = day,
                hour = hour,
                minute = minute,
                interval = interval,
                simulation_days = simulation_days,
                day_type = day_type,
                environment_name = environment_name,
                reporting_frequency = reporting_frequency
            )
            specs <- viewer__sql_result_specs(
                source,
                variable = filters$name,
                reporting_frequency = reporting_frequency,
                environment_name = environment_name,
                palette = palette,
                limits = limits,
                mode = mode,
                report_filters = filters
            )
            provider <- NULL
            payload <- NULL
            if (!is.null(private$m_live)) {
                objects <- viewer__selected_objects(
                    private$m_live$objects,
                    vapply(
                        private$m_live$objects,
                        function(object) as.character(object$id),
                        ""
                    )
                )
                # Prepare and validate the replacement before releasing the last working query.
                if (mode == "provider") {
                    provider <- IdfViewerFrameProvider$new(specs, objects)
                    payload <- provider$metadata(
                        private$m_live$config$frame_endpoint
                    )
                } else {
                    payload <- viewer__results_payload(specs, objects)
                }
                payload <- tryCatch(
                    viewer__result_query_payload(
                        payload,
                        source,
                        filters,
                        objects,
                        TRUE
                    ),
                    error = function(error) {
                        if (!is.null(provider)) {
                            provider$close()
                        }
                        stop(error)
                    }
                )
            }
            private$reset_frame_provider()
            private$m_results <- specs
            private$m_sql_source <- source
            private$m_sql_filters <- filters
            private$m_sql_options <- list(
                palette = palette,
                limits = limits,
                mode = mode
            )
            private$m_frame_provider <- provider
            private$m_render_by <- "result"
            if (!is.null(private$m_live)) {
                private$m_live$provider <- provider
                private$m_live$send("results", list(results = payload))
            }
            invisible(self)
        },
        # }}}

        # frame_provider {{{
        #' @description Return the framework-independent result frame provider.
        #' @details Configure `$color_by()` or `$results()` with
        #'   `mode = "provider"` first. In an local HTTP route, pass
        #'   `req$query$variable` and `req$query$frames` directly to
        #'   `provider$fetch()`, set `Content-Type` to
        #'   `application/octet-stream`, and send the returned raw vector.
        #' @return An [IdfViewerFrameProvider] object.
        frame_provider = function() {
            modes <- vapply(
                private$m_results,
                function(spec) spec$mode %||% "embed",
                character(1L)
            )
            if (!length(modes) || any(modes != "provider")) {
                abort(
                    "Call '$color_by(..., mode = \"provider\")' or '$results(..., mode = \"provider\")' first."
                )
            }
            if (is.null(private$m_frame_provider)) {
                context <- viewer__scene_context(
                    private$m_geom,
                    private$options()
                )
                private$m_frame_provider <- IdfViewerFrameProvider$new(
                    private$m_results,
                    context$objects
                )
            }
            private$m_frame_provider
        },
        # }}}

        # update {{{
        #' @description Send an incremental command to the current live view.
        #' @param command One of time, variable, render_by, view, projection,
        #'   fit, fit_model, fit_selection, range_mode, result_filter, show_all,
        #'   play, pause, palette, range, aggregation, explode, clip, measure,
        #'   hide_selected, isolate, exit_isolation, clear_selection, panel,
        #'   diagnostics, background, wireframe, x_ray, window_style, back_faces, axis, or ground.
        #'   For window_style, use value = "glass" (default) or "opaque".
        #'   Back faces are culled by default; back_faces with on = TRUE enables inspection.
        #'   For result_filter, minimum and maximum specify inclusive visible values;
        #'   NULL removes a bound. Setting a bound activates Results coloring.
        #'   Color-scale limits remain unchanged.
        #'   The panel command controls the transient property inspector.
        #' @param ... Named arguments for the browser command.
        update = function(command, ...) {
            assert_choice(
                command,
                c(
                    "time",
                    "variable",
                    "render_by",
                    "view",
                    "fit",
                    "fit_model",
                    "fit_selection",
                    "projection",
                    "range_mode",
                    "result_filter",
                    "show_all",
                    "play",
                    "pause",
                    "palette",
                    "range",
                    "aggregation",
                    "explode",
                    "clip",
                    "measure",
                    "hide_selected",
                    "isolate",
                    "exit_isolation",
                    "clear_selection",
                    "panel",
                    "diagnostics",
                    "background",
                    "wireframe",
                    "x_ray",
                    "window_style",
                    "back_faces",
                    "axis",
                    "ground"
                )
            )
            private$require_live()$send(command, list(...))
            invisible(self)
        },
        # }}}

        # select {{{
        #' @description Select existing objects by their semantic metadata.
        #' @details Zone and Space criteria select their member surfaces;
        #'   several supplied criteria are combined by intersection.
        #' @param zone,space Zone and Space names.
        #' @param surface Surface names or IDs.
        select = function(zone = NULL, space = NULL, surface = NULL) {
            live <- private$require_live()
            objects <- viewer__selected_objects(
                live$objects,
                vapply(
                    live$objects,
                    function(object) as.character(object$id),
                    ""
                )
            )
            keep <- if (is.null(zone) && is.null(space) && is.null(surface)) {
                rep(FALSE, nrow(objects))
            } else {
                rep(TRUE, nrow(objects))
            }
            for (criterion in c("zone", "space", "surface")) {
                values <- get(criterion)
                if (is.null(values)) {
                    next
                }
                checkmate::assert_atomic_vector(values, any.missing = FALSE)
                column <- switch(
                    criterion,
                    zone = "zone_name",
                    space = "space_name",
                    surface = "name"
                )
                matches <- tolower(as.character(objects[[column]])) %chin%
                    tolower(as.character(values))
                if (criterion == "surface") {
                    matches <- matches |
                        as.character(objects$id) %chin% as.character(values)
                }
                keep <- keep & matches
            }
            live$send(
                "select_ids",
                list(ids = as.list(as.character(objects$id[keep])))
            )
            invisible(self)
        },
        # }}}

        # filter {{{
        #' @description Filter the current live scene without sending geometry again.
        #' @param zone,space,surface Names to include; NULL removes that filter.
        #' @param type Component names such as wall, roof, floor or window.
        #' @param name Optional literal substring of the object name, matched without
        #'   regard to case. Combined with the other visibility filters.
        filter = function(
            zone = NULL,
            space = NULL,
            surface = NULL,
            type = NULL,
            name = NULL
        ) {
            assert_string(name, null.ok = TRUE)
            values <- list(
                zone = zone,
                space = space,
                surface = surface,
                type = type,
                name = name
            )
            for (value in values) {
                assert_character(value, any.missing = FALSE, null.ok = TRUE)
            }
            private$require_live()$send(
                "filter",
                lapply(values, function(value) {
                    if (is.null(value)) NULL else as.list(value)
                })
            )
            invisible(self)
        },
        # }}}

        # selection {{{
        #' @description Return selected model metadata last reported by the browser.
        #' @return A data.table containing the original object IDs and metadata.
        selection = function() private$require_live()$read()$selection,
        # }}}

        # state {{{
        #' @description Return connection status and the last acknowledged browser state.
        #' @details Commands are asynchronous. The acknowledged counter allows
        #'   scripts to distinguish a sent update from a completed update.
        state = function() private$require_live()$read(),
        # }}}

        # on_select {{{
        #' @description Register a callback called with selected model metadata.
        #' @param callback An R function taking one data.table, or NULL to remove it.
        on_select = function(callback = NULL) {
            if (!is.null(callback) && !is.function(callback)) {
                abort("'callback' must be a function or NULL.")
            }
            private$m_on_select <- callback
            if (!is.null(private$m_live)) {
                private$m_live$callback <- callback
            }
            invisible(self)
        },
        # }}}

        # diagnostics {{{
        #' @description Configure geometry-quality diagnostics.
        #' @param on Whether potential adjacency issues are calculated and shown.
        #' @param tolerance Maximum vertex separation in metres treated as a
        #'   potential near-miss. Exact coincident vertices are not flagged.
        diagnostics = function(on = TRUE, tolerance = 0.01) {
            assert_flag(on)
            assert_number(tolerance, lower = 1e-6, finite = TRUE)
            private$m_diagnostics <- list(enabled = on, tolerance = tolerance)
            invisible(self)
        },
        # }}}

        # clear_color {{{
        #' @description Remove mapped results and return to surface-type coloring.
        clear_color = function() {
            private$reset_frame_provider()
            private$m_results <- list()
            private$m_sql_source <- NULL
            private$m_render_by <- "surface_type"
            private$refresh_results()
            invisible(self)
        },
        # }}}

        # show {{{
        #' @description Show existing geometry without modifying the source model.
        #' @param type Geometry components to include.
        #' @param zone,space,surface Zone, Space, and Surface IDs or names.
        #' @param width,dayl_color,dayl_size Former viewer styling arguments.
        #' @param live Enable R updates and selection readback. Defaults to
        #'   interactive R; reports remain self-contained by default.
        show = function(
            type = "all",
            zone = NULL,
            space = NULL,
            surface = NULL,
            width = 1.5,
            dayl_color = "red",
            dayl_size = 5,
            live = interactive()
        ) {
            private$stop_live()
            private$reset_frame_provider()
            private$m_show <- list(
                type = type,
                zone = zone,
                space = space,
                surface = surface
            )
            widget <- self$widget(live = live)
            if (interactive()) {
                print(widget)
            }
            invisible(widget)
        },
        # }}}        # focus {{{
        #' @description Re-display the current widget.
        focus = function() {
            if (is.null(private$m_widget)) {
                verbose_info("No viewer widget found. Skip.")
            } else if (interactive()) {
                print(private$m_widget)
            }
            invisible()
        },
        # }}}

        # close {{{
        #' @description Forget the current widget instance.
        close = function() {
            private$stop_live()
            private$reset_frame_provider()
            private$m_widget <- NULL
            invisible()
        },
        # }}}

        # snapshot {{{
        #' @description Export the configured scene as portable HTML or PNG.
        #' @details Exports rebuild the configured scene. Use the browser's
        #'   screenshot button to capture its current camera and selection.
        #' @param filename Output .html or .png path.
        #' @param selfcontained Embed dependencies in an HTML export.
        #' @param ... Additional arguments to webshot2::webshot().
        snapshot = function(filename, selfcontained = TRUE, ...) {
            if (
                length(private$m_results) &&
                    any(vapply(
                        private$m_results,
                        function(spec) identical(spec$mode, "provider"),
                        logical(1L)
                    ))
            ) {
                abort("Snapshots require embedded results; use mode = 'embed'.")
            }
            viewer__snapshot(self, filename, selfcontained, ...)
        },
        # }}}        # print {{{
        #' @description Print a concise viewer summary.
        print = function() {
            cli::cat_rule("EnergyPlus IDF Three.js Viewer", line = 1)
            path <- private$m_geom$parent()$path()
            if (is.null(path)) {
                path <- cli::style_bold(cli::bg_red("NOT LOCAL"))
            } else {
                path <- surround(path)
            }
            cli::cat_line(cli::ansi_strtrim(c(
                paste0(" ", cli::symbol$bullet, " Path: ", path),
                paste0(
                    " ",
                    cli::symbol$bullet,
                    " Version: ",
                    surround(private$m_geom$parent()$version())
                ),
                paste0(" ", cli::symbol$bullet, " Renderer: Three.js/WebGL")
            )))
            invisible(self)
        }
        # }}}
    ),

    private = list(
        m_geom = NULL,
        m_widget = NULL,
        m_live = NULL,
        m_on_select = NULL,
        m_element_id = NULL,
        m_background = "#f3f5f7",
        m_viewpoint = list(look_at = "iso", fov = 38),
        m_mouse_mode = list(
            left = "trackball",
            right = "pan",
            middle = "fov",
            wheel = "pull"
        ),
        m_size = list(width = NULL, height = 640),
        m_axis = TRUE,
        m_ground = FALSE,
        m_wireframe = TRUE,
        m_x_ray = FALSE,
        m_render_by = "surface_type",
        m_show = list(type = "all", zone = NULL, space = NULL, surface = NULL),
        m_results = list(),
        m_sql_source = NULL,
        m_sql_filters = list(),
        m_sql_options = list(),
        m_frame_provider = NULL,
        m_diagnostics = list(enabled = FALSE, tolerance = 0.01),

        # options {{{
        # Keep a single options contract between R, standalone widgets, and local HTTP.
        options = function() {
            list(
                background = private$m_background,
                viewpoint = private$m_viewpoint,
                mouse_mode = private$m_mouse_mode,
                axis = private$m_axis,
                ground = private$m_ground,
                wireframe = private$m_wireframe,
                x_ray = private$m_x_ray,
                render_by = private$m_render_by,
                show = private$m_show,
                diagnostics = private$m_diagnostics
            )
        },
        # }}}

        # require_live {{{
        # Fail clearly instead of pretending a portable widget has an R connection.
        require_live = function() {
            if (is.null(private$m_live) || private$m_live$closed) {
                abort(
                    "Render '$widget(live = TRUE)' or '$show(live = TRUE)' first."
                )
            }
            private$m_live
        },
        # }}}

        # stop_live {{{
        # Close this connection before replacing its scene or releasing the viewer.
        stop_live = function() {
            if (!is.null(private$m_live)) {
                private$m_live$close()
            }
            private$m_live <- NULL
            invisible()
        },
        # }}}

        # query_results {{{
        # Reuse the public selectors for browser queries and keep failures transactional.
        query_results = function(filters) {
            if (is.null(private$m_sql_source)) {
                abort("No EnergyPlus result source is attached.")
            }
            filters <- viewer__browser_result_filters(filters)
            filters <- utils::modifyList(
                private$m_sql_filters,
                filters,
                keep.null = TRUE
            )
            do.call(
                self$results,
                c(
                    list(source = private$m_sql_source),
                    private$m_sql_options,
                    filters
                )
            )
        },
        # }}}

        # refresh_results {{{
        # Replace only result buffers and metadata; camera and geometry stay in the browser.
        refresh_results = function() {
            if (is.null(private$m_live)) {
                return(invisible())
            }
            objects <- viewer__selected_objects(
                private$m_live$objects,
                vapply(
                    private$m_live$objects,
                    function(object) as.character(object$id),
                    ""
                )
            )
            provider <- length(private$m_results) &&
                all(vapply(
                    private$m_results,
                    function(spec) identical(spec$mode, "provider"),
                    logical(1L)
                ))
            if (provider) {
                private$m_frame_provider <- IdfViewerFrameProvider$new(
                    private$m_results,
                    objects
                )
                private$m_live$provider <- private$m_frame_provider
                results <- private$m_frame_provider$metadata(
                    private$m_live$config$frame_endpoint
                )
            } else {
                private$m_live$provider <- NULL
                results <- viewer__results_payload(private$m_results, objects)
            }
            private$m_live$send("results", list(results = results))
            invisible()
        },
        # }}}

        # reset_frame_provider {{{
        # Close stale query state whenever geometry filters or result specifications change.
        reset_frame_provider = function() {
            if (!is.null(private$m_frame_provider)) {
                private$m_frame_provider$close()
            }
            private$m_frame_provider <- NULL
            invisible()
        },
        # }}}

        # finalize {{{
        # Ensure provider-owned DuckDB connections do not outlive the viewer object.
        finalize = function() {
            private$stop_live()
            private$reset_frame_provider()
        }
        # }}}
    )
)
# }}}

# vim: set fdm=marker:
