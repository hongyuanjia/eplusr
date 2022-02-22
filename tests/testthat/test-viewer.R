# IdfViewer {{{
test_that("IdfViewer class", {
    skip_on_cran()

    # simple model
    idf <- read_idf(path_eplus_example(8.8, "4ZoneWithShading_Simple_1.idf"))

    expect_is(viewer <- idf_viewer(idf), "IdfViewer")
    expect_is(viewer$parent(), "Idf")
    expect_is(viewer$geometry(), "IdfGeometry")
    expect_null(viewer$device())
    expect_null(viewer$background())
    expect_null(viewer$viewpoint())
    expect_null(viewer$win_size())
    expect_null(viewer$mouse_mode())
    expect_true(viewer$axis())
    expect_true(viewer$ground())
    expect_true(viewer$wireframe())
    expect_true(viewer$x_ray())
    expect_equal(viewer$render_by(), "surface_type")

    expect_null(viewer$show())
    expect_null(viewer$show("floor"))
    expect_null(viewer$show("roof"))
    expect_null(viewer$show("window"))
    expect_null(viewer$show("shading"))
    expect_null(viewer$show("wall"))
    expect_null(viewer$show(NULL))

    expect_false(viewer$x_ray(FALSE))

    expect_false(viewer$axis(FALSE))
    expect_true(viewer$axis(TRUE))

    expect_true(viewer$ground(expand = 2, color = "grey"))
    expect_false(viewer$ground(FALSE))

    expect_null(viewer$show())
    expect_equal(viewer$render_by("normal"), "normal")
    expect_equal(viewer$render_by("construction"), "construction")
    expect_equal(viewer$render_by("zone"), "zone")
    expect_equal(viewer$render_by("boundary"), "boundary")
    expect_equal(viewer$render_by("surface_type"), "surface_type")

    expect_false(viewer$wireframe(FALSE))
    expect_true(viewer$wireframe(TRUE))

    expect_null(viewer$show(zone = "Zone 1"))
    expect_null(viewer$show(zone = "Zone 2"))
    expect_null(viewer$show(zone = "Zone"))
    expect_null(viewer$show(zone = "Zone 1", surface = names(idf$Window)))
    expect_null(viewer$show(surface = names(idf$Window)))

    expect_is(f <- viewer$snapshot(tempfile(fileext = ".pdf")), "character")
    expect_true(file.exists(f))
    if (testthat:::on_ci()) {
        expect_is(f <- viewer$snapshot(tempfile(fileext = ".png"), webshot = TRUE), "character")
        expect_true(file.exists(f))
    } else {
        expect_is(f <- viewer$snapshot(tempfile(fileext = ".png")), "character")
        expect_true(file.exists(f))
    }
    expect_null(viewer$focus())
    expect_null(viewer$close())

    expect_output(viewer$print())
})
# }}}
