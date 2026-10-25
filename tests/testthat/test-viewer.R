# IdfViewer {{{
test_that("IdfViewer builds a Three.js htmlwidget and preserves viewer controls", {
    skip_on_cran()
    skip_if_not_installed("htmlwidgets")
    skip_if_not_installed("decido")

    path <- path_eplus_example(
        Sys.getenv("EPLUSR_VIEWER_TEST_VERSION", LATEST_EPLUS_VER),
        "5ZoneAirCooledWithSpaces.idf"
    )
    idf <- read_idf(path)
    viewer <- idf_viewer(idf)

    expect_s3_class(viewer$parent(), "Idf")
    expect_s3_class(viewer$geometry(), "IdfGeometry")
    expect_null(viewer$device())
    expect_null(viewer$background("#eeeeee"))
    expect_null(viewer$viewpoint("top"))
    expect_null(viewer$win_size(0, 0, 720, 480))
    expect_null(viewer$mouse_mode())
    expect_true(viewer$axis())
    expect_true(viewer$ground())
    expect_true(viewer$wireframe())
    expect_true(viewer$x_ray())
    expect_equal(viewer$render_by("space"), "space")

    widget <- viewer$widget()
    expect_s3_class(widget, "htmlwidget")
    expect_identical(widget$x$options$render_by, "space")
    expect_type(viewer$device(), "character")
    expect_true(length(widget$x$objects) > 0L)
    expect_true(nzchar(widget$x$geometry$positions))

    # Public filtering includes Space, which the former viewer failed to pass through.
    space <- names(idf$Space)[[1L]]
    subset_widget <- viewer$show(space = space)
    expect_s3_class(subset_widget, "htmlwidget")
    expect_true(all(vapply(
        subset_widget$x$objects,
        function(x) identical(x$space_name, space),
        logical(1L)
    )))

    zone <- names(idf$Zone)[[1L]]
    viewer$color_by(
        data.frame(
            zone = rep(zone, 2L),
            time = c("01:00", "02:00"),
            temperature = c(20, 25),
            load = c(100, 150)
        ),
        value = c("temperature", "load"),
        object = "zone",
        time = "time",
        units = c("degC", "W"),
        palette = c("viridis", "diverging")
    )
    result_widget <- viewer$show(zone = zone)
    expect_identical(result_widget$x$options$render_by, "result")
    expect_length(result_widget$x$results$variables, 2L)
    expect_identical(
        result_widget$x$results$variables[[1L]]$times,
        c("01:00", "02:00")
    )
    expect_identical(result_widget$x$results$variables[[1L]]$shape[[1L]], 2L)
    expect_identical(
        result_widget$x$results$variables[[2L]]$palette,
        "diverging"
    )

    html <- tempfile(fileext = ".html")
    expect_type(viewer$snapshot(html, selfcontained = FALSE), "character")
    expect_true(file.exists(html))
    expect_null(viewer$focus())
    expect_null(viewer$close())
    expect_null(viewer$device())
    expect_output(viewer$print(), "Three.js")
})
# }}}

# Frame provider integration {{{
test_that("IdfViewer exposes Ambiorix-ready provider metadata", {
    skip_on_cran()
    skip_if_not_installed("htmlwidgets")
    skip_if_not_installed("decido")

    idf <- read_idf(path_eplus_example(
        Sys.getenv("EPLUSR_VIEWER_TEST_VERSION", LATEST_EPLUS_VER),
        "5ZoneAirCooledWithSpaces.idf"
    ))
    zone <- names(idf$Zone)[[1L]]
    viewer <- idf_viewer(idf)
    viewer$color_by(
        data.frame(
            zone = rep(zone, 2L),
            time = c("01:00", "02:00"),
            value = c(20, 25)
        ),
        value = "value",
        object = "zone",
        time = "time",
        mode = "provider"
    )

    expect_error(viewer$widget(), "frame_endpoint")
    provider <- viewer$frame_provider()
    expect_s3_class(provider, "IdfViewerFrameProvider")
    widget <- viewer$widget(frame_endpoint = "/api/viewer/frames")
    result <- widget$x$results$variables[[1L]]
    expect_identical(result$mode, "provider")
    expect_identical(result$endpoint, "/api/viewer/frames")
    expect_identical(result$variable, 0L)
    expect_length(
        provider$fetch(result$variable, "1"),
        length(widget$x$objects) * 4L
    )
    expect_error(
        viewer$snapshot(tempfile(fileext = ".html")),
        "embedded results"
    )

    expect_null(viewer$close())
    closed <- catch_cnd(provider$fetch("0", "0"))
    expect_identical(closed$status, 410L)
})
# }}}

# vim: set fdm=marker:

test_that("live selections use canonical model identities and reject stale scenes", {
    skip_if_not_installed("httpuv")
    scene <- list(
        scene_id = "scene-a",
        objects = list(
            list(
                id = 10L,
                name = "Wall A",
                zone_name = "Zone A",
                space_name = NULL
            ),
            list(
                id = 20L,
                name = "Wall B",
                zone_name = "Zone B",
                space_name = "Space B"
            )
        )
    )
    observed <- NULL
    session <- viewer__session(scene, callback = function(selection) {
        observed <<- selection
    })
    on.exit(session$close(), add = TRUE)
    expect_identical(session$read()$connected, 0L)
    expect_false(session$receive(list(
        type = "selection",
        scene_id = "old-scene",
        selected_ids = list("10")
    )))
    expect_equal(nrow(session$read()$selection), 0L)

    expect_true(session$receive(list(
        type = "selection",
        scene_id = "scene-a",
        selected_ids = list("10", "999"),
        name = "untrusted browser metadata",
        state = list(render_by = "zone")
    )))
    expect_identical(observed$id, 10L)
    expect_identical(observed$name, "Wall A")
    expect_true(is.na(observed$space_name))
    expect_identical(session$read()$view$render_by, "zone")
    observed$name <- "mutated callback copy"
    expect_identical(session$read()$selection$name, "Wall A")
    session$close()
    expect_true(session$read()$closed)
    expect_error(session$send("view", list(value = "top")), "closed")
})

test_that("live viewer connections have independent queues and lifecycles", {
    skip_if_not_installed("httpuv")
    scene <- list(
        scene_id = "independent",
        objects = list(list(id = 1L, name = "Wall"))
    )
    first <- viewer__session(scene)
    second <- viewer__session(scene)
    on.exit(
        {
            first$close()
            second$close()
        },
        add = TRUE
    )
    expect_false(identical(first$config$websocket, second$config$websocket))
    first$send("view", list(value = "top"))
    expect_identical(first$read()$sent, 1L)
    expect_identical(second$read()$sent, 0L)
    first$close()
    expect_false(second$read()$closed)
    expect_identical(second$send("view", list(value = "front")), 1L)
})

test_that("live viewer cleanup tolerates an already stopped listener", {
    skip_if_not_installed("httpuv")
    scene <- list(
        scene_id = "stopped-listener",
        objects = list(list(id = 1L, name = "Wall"))
    )
    session <- viewer__session(scene)
    on.exit(session$close(), add = TRUE)
    httpuv::stopServer(session$server)
    expect_no_error(session$close())
    expect_no_error(session$close())
    expect_true(session$read()$closed)
    expect_identical(session$read()$connected, 0L)
})

# Public frame-provider interface.
# IdfViewerFrameProvider {{{
test_that("frame provider exposes validated binary frames without an HTTP framework", {
    zone <- "Test Zone"
    objects <- data.table(
        id = 1L,
        name = "Test Surface",
        zone_name = zone,
        space_name = NA_character_,
        object_index = 0L
    )
    spec <- list(
        data = data.frame(
            zone = rep(zone, 2L),
            period = c("A", "B"),
            value = c(1.5, 2.5),
            load = c(100, 150)
        ),
        value = "value",
        object = "zone",
        key = "zone",
        time = "period",
        label = "Zone value",
        units = "W",
        palette = "viridis",
        limits = NULL,
        mode = "provider"
    )
    provider <- IdfViewerFrameProvider$new(
        list(
            spec,
            utils::modifyList(spec, list(value = "load", label = "Zone load"))
        ),
        objects
    )
    on.exit(provider$close(), add = TRUE)
    metadata <- provider$metadata("/api/viewer/frames")
    payload <- metadata$variables[[1L]]

    expect_s3_class(provider, "IdfViewerFrameProvider")
    expect_length(metadata$variables, 2L)
    expect_identical(payload$mode, "provider")
    expect_identical(payload$endpoint, "/api/viewer/frames")
    expect_identical(payload$variable, 0L)
    expect_identical(payload$times, c("A", "B"))
    values <- readBin(
        provider$fetch("0", "1"),
        "numeric",
        size = 4L,
        n = nrow(objects),
        endian = "little"
    )
    expect_length(values, nrow(objects))
    expect_true(any(values == 2.5))
    aggregate_values <- readBin(
        provider$fetch(0L, c(0L, 1L)),
        "numeric",
        size = 4L,
        n = nrow(objects),
        endian = "little"
    )
    expect_true(any(aggregate_values == 2))
    load_values <- readBin(
        provider$fetch("1", "1"),
        "numeric",
        size = 4L,
        n = nrow(objects),
        endian = "little"
    )
    expect_identical(load_values, 150)

    invalid <- catch_cnd(provider$fetch("DROP TABLE", "0"))
    expect_s3_class(invalid, "eplusr_error_viewer_frame")
    expect_identical(invalid$status, 400L)
    unknown_variable <- catch_cnd(provider$fetch("3", "0"))
    expect_identical(unknown_variable$status, 404L)
    unknown <- catch_cnd(provider$fetch("0", "3"))
    expect_s3_class(unknown, "eplusr_error_viewer_frame")
    expect_identical(unknown$status, 404L)

    expect_identical(provider$close(), provider)
    closed <- catch_cnd(provider$fetch("0", "0"))
    expect_identical(closed$status, 410L)
})

# Name filtering must survive the public R-to-browser transport unchanged.
test_that("live name filters combine with other criteria and can be cleared", {
    skip_on_cran()
    skip_if_not_installed("httpuv")
    skip_if_not_installed("htmlwidgets")
    skip_if_not_installed("decido")
    idf <- read_idf(path_eplus_example(
        Sys.getenv("EPLUSR_VIEWER_TEST_VERSION", LATEST_EPLUS_VER),
        "5ZoneAirCooledWithSpaces.idf"
    ))
    viewer <- idf_viewer(idf)
    on.exit(viewer$close(), add = TRUE)
    viewer$widget(live = TRUE)
    zone <- names(idf$Zone)[[1L]]
    viewer$filter(zone = zone, type = "wall", name = "TOP")
    session <- get_priv_env(viewer)$m_live
    message <- tail(session$pending, 1L)[[1L]]
    expect_identical(message$command, "filter")
    expect_identical(message$args$name, list("TOP"))
    expect_identical(message$args$zone, list(zone))
    expect_identical(message$args$type, list("wall"))
    viewer$filter(name = NULL)
    expect_null(tail(session$pending, 1L)[[1L]]$args$name)
    expect_error(viewer$filter(name = c("A", "B")), "name")
})
