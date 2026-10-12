# CRAN exercises capability and failure paths without loading native OpenGL.
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    withr::local_envvar(c(RGL_USE_NULL = "true"))
}


# IdfViewer Implemention {{{
test_that("Geometry triangulation preserves polygon holes", {
    skip_if_not_installed("decido")

    geoms <- list(
        surface = data.table(id = 1L, name = "wall"),
        subsurface = data.table(
            id = 2L, name = "window", building_surface_name = "wall"
        ),
        vertices = rbindlist(list(
            data.table(
                id = 1L, index = 1:4,
                x = c(0, 4, 4, 0), y = c(0, 0, 4, 4), z = 0
            ),
            data.table(
                id = 2L, index = 1:3,
                x = c(1, 2, 1), y = c(1, 1, 2), z = 0
            )
        ))
    )

    triangulated <- triangulate_geoms(geoms)
    expect_true(all(is.finite(triangulated$x)))
    expect_true(all(is.finite(triangulated$y)))
    expect_true(all(is.finite(triangulated$z)))
    expect_equal(nrow(triangulated[id == 1L]), 21L)
    expect_equal(nrow(triangulated[id == 2L]), 3L)

    # Compare total triangle area instead of implementation-specific indices.
    surface <- triangulated[id == 1L]
    surface[, triangle := rep(seq_len(.N %/% 3L), each = 3L)]
    area <- surface[, by = "triangle", .(
        area = abs(
            (x[2L] - x[1L]) * (y[3L] - y[1L]) -
                (x[3L] - x[1L]) * (y[2L] - y[1L])
        ) / 2
    )][, sum(area)]
    expect_equal(area, 15.5, tolerance = 1e-10)
})

test_that("IdfViewer Implemention", {
    skip_on_cran()
    skip_on_os("mac")

    # simple model
    idf <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "5ZoneAirCooledWithSpaces.idf"))

    expect_type(geoms <- extract_geom(idf), "list")
    expect_type(geoms <- align_coord_system(geoms, "world", "world", "world"), "list")
    expect_s3_class(geoms$vertices2 <- triangulate_geoms(geoms), "data.table")

    rgl_init <- function(clear = TRUE) {
        new <- FALSE
        if (clear) {
            if (rgl::cur3d() == 0) new <- TRUE else rgl::clear3d()
        }
        if (!new) {
            dev <- rgl::cur3d()
        } else {
            rgl::open3d()
            dev <- rgl::cur3d()

            # set viewpoint
            rgl::view3d(0, -60, 60)

            # change mouse control method
            cur <- rgl::par3d("mouseMode")
            cur[["left"]] <- "trackball"
            cur[["wheel"]] <- "push"
            cur[["middle"]] <- "fov"
            rgl::par3d(dev = dev, mouseMode = cur)
            pan3d(2L)
        }

        rgl::bg3d(color = "white")

        rgl::set3d(dev)
        dev
    }

    dev <- rgl_init()
    expect_type(id_axis <- rgl_view_axis(dev, geoms), "integer")
    expect_type(id_ground <- rgl_view_ground(dev, geoms, alpha = 1.0), "integer")
    expect_type(id_wireframe <- rgl_view_wireframe(dev, geoms), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, wireframe = FALSE), "integer")
    expect_length(id_dayl_pnts <- rgl_view_point(dev, geoms), 0)

    expect_type(rgl_pop(id = id_ground), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "boundary"), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "construction"), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "zone"), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "space"), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "normal"), "integer")

    # complex model
    idf <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "HospitalLowEnergy.idf"))
    expect_type(geoms <- extract_geom(idf), "list")
    expect_type(geoms <- align_coord_system(geoms, "relative", "relative", "relative"), "list")
    expect_equal(unlist(geoms$rules[3:5], FALSE, FALSE), rep("relative", 3L))
    expect_type(geoms <- align_coord_system(geoms, "world", "world", "world"), "list")
    expect_equal(unlist(geoms$rules[3:5], FALSE, FALSE), rep("world", 3L))
    expect_s3_class(geoms$vertices2 <- triangulate_geoms(geoms), "data.table")

    expect_type(dev <- rgl_init(), "integer")
    expect_type(id_axis <- rgl_view_axis(dev, geoms), "integer")
    expect_type(id_ground <- rgl_view_ground(dev, geoms, alpha = 1.0), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "surface_type", wireframe = FALSE), "integer")
    expect_type(id_wireframe <- rgl_view_wireframe(dev, geoms), "integer")
    expect_type(id_dayl_pnts <- rgl_view_point(dev, geoms), "integer")

    expect_type(rgl_pop(id = id_ground), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "boundary"), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "construction"), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "zone"), "integer")

    expect_type(rgl_pop(id = unlist(id_surface)), "integer")
    expect_type(id_surface <- rgl_view_surface(dev, geoms, "normal"), "integer")

    rgl::close3d()
})
# }}}

# Reject unsupported backends before they can create empty or placeholder files.
test_that("NULL devices reject native snapshot exports", {
    skip_if_not_installed("rgl")
    local_mocked_bindings(rgl.useNULL = function() TRUE, .package = "rgl")
    for (ext in c("png", "pdf", "svg")) {
        path <- tempfile(fileext = paste0(".", ext))
        expect_error(
            rgl_snapshot(1L, path),
            class = "eplusr_error_snapshot_unavailable"
        )
        expect_false(file.exists(path))
    }
})

# A requested browser backend must not silently fall back to unsupported rendering.
test_that("browser exports require a working browser backend", {
    skip_if_not_installed("rgl")
    local_mocked_bindings(viewer__can_webshot = function() FALSE)
    path <- tempfile(fileext = ".png")
    expect_error(
        rgl_snapshot(1L, path, webshot = TRUE),
        class = "eplusr_error_snapshot_unavailable"
    )
    expect_false(file.exists(path))
})

# Failed exports must leave an existing destination untouched and remove partial files.
test_that("snapshot backend failures do not replace existing output", {
    skip_if_not_installed("rgl")
    local_mocked_bindings(
        rgl.useNULL = function() FALSE,
        set3d = function(...) invisible(NULL),
        rgl.postscript = function(filename, ...) {
            file.create(filename)
            warning("Postscript conversion failed")
        },
        .package = "rgl"
    )
    path <- tempfile(fileext = ".pdf")
    writeLines("existing output", path)
    expect_error(rgl_snapshot(1L, path), class = "eplusr_error_snapshot_failed")
    expect_identical(readLines(path), "existing output")
})

# An empty backend output is failure even if no warning was signalled.
test_that("empty snapshot files are rejected", {
    skip_if_not_installed("rgl")
    local_mocked_bindings(
        rgl.useNULL = function() FALSE,
        set3d = function(...) invisible(NULL),
        rgl.postscript = function(filename, ...) file.create(filename),
        .package = "rgl"
    )
    path <- tempfile(fileext = ".pdf")
    expect_error(rgl_snapshot(1L, path), class = "eplusr_error_snapshot_failed")
    expect_false(file.exists(path))
})

# A controlled coloured scene must produce image content, not the NULL-device placeholder.
test_that("browser snapshots contain rendered scene pixels", {
    skip_on_cran()
    skip_if_not_integration()
    skip_if_not_installed("rgl")
    skip_if_not_installed("png")
    skip_if_not(
        viewer__can_webshot(),
        "browser snapshot backend is unavailable"
    )
    dev <- rgl::open3d(useNULL = TRUE)
    withr::defer(rgl::close3d(dev))
    rgl::bg3d("white")
    rgl::spheres3d(0, 0, 0, radius = 1, color = "red")
    path <- tempfile(fileext = ".png")
    expect_warning(result <- rgl_snapshot(dev, path, webshot = TRUE), NA)
    expect_identical(result, normalizePath(path))
    image <- png::readPNG(path)
    expect_gte(dim(image)[1L], 32L)
    expect_gte(dim(image)[2L], 32L)
    pixels <- matrix(image[,, 1:3], ncol = 3L)
    expect_gt(nrow(unique(pixels)), 1L)
})

# Validate the vector backend separately because browser and native exports use different devices.
test_that("native PDF snapshots contain a PDF document", {
    skip_on_cran()
    skip_if_not_integration()
    skip_if_not_installed("rgl")
    skip_if(rgl::rgl.useNULL(), "native OpenGL device is unavailable")
    dev <- rgl::open3d()
    withr::defer(rgl::close3d(dev))
    rgl::spheres3d(0, 0, 0, radius = 1, color = "red")
    path <- tempfile(fileext = ".pdf")
    expect_silent(result <- rgl_snapshot(dev, path))
    expect_identical(result, normalizePath(path))
    expect_identical(rawToChar(readBin(path, "raw", n = 5L)), "%PDF-")
    expect_gt(file.info(path)$size, 100L)
})

# Device type takes precedence over native defaults when both kinds coexist.
test_that("NULL devices are rejected even with native defaults", {
    skip_if_not_installed("rgl")
    local_mocked_bindings(
        rgl.useNULL = function() FALSE,
        rgl.dev.list = function() c(null = 1L, glx = 2L),
        .package = "rgl"
    )
    path <- tempfile(fileext = ".png")
    expect_error(
        rgl_snapshot(1L, path),
        class = "eplusr_error_snapshot_unavailable"
    )
    expect_false(file.exists(path))
})

# Exercise the successful file-commit path with a real PDF from the standard R device.
test_that("successful snapshots replace the destination with a complete PDF", {
    skip_if_not_installed("rgl")
    local_mocked_bindings(
        rgl.useNULL = function() FALSE,
        rgl.dev.list = function() c(glx = 1L),
        set3d = function(...) invisible(NULL),
        rgl.postscript = function(filename, ...) {
            grDevices::pdf(filename)
            graphics::plot.new()
            graphics::points(0.5, 0.5)
            grDevices::dev.off()
            invisible(NULL)
        },
        .package = "rgl"
    )
    path <- tempfile(fileext = ".pdf")
    writeLines("existing output", path)
    expect_silent(result <- rgl_snapshot(1L, path))
    expect_identical(result, normalizePath(path))
    expect_identical(readBin(path, "raw", n = 5L), charToRaw("%PDF-"))
    expect_gt(file.info(path)$size, 100L)
})

# Malformed files must never be committed merely because they have nonzero size.
test_that("malformed snapshot output is rejected", {
    skip_if_not_installed("rgl")
    local_mocked_bindings(
        rgl.useNULL = function() FALSE,
        rgl.dev.list = function() c(glx = 1L),
        set3d = function(...) invisible(NULL),
        rgl.postscript = function(filename, ...) {
            writeLines("invalid document", filename)
        },
        .package = "rgl"
    )
    path <- tempfile(fileext = ".pdf")
    expect_error(rgl_snapshot(1L, path), class = "eplusr_error_snapshot_failed")
    expect_false(file.exists(path))
})

# vim: set fdm=marker:
