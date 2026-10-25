# IdfViewer implementation {{{
test_that("viewer scene uses compact binary buffers with stable object mappings", {
    skip_on_cran()
    skip_if_not_installed("decido")

    idf <- read_idf(path_eplus_example(
        Sys.getenv("EPLUSR_VIEWER_TEST_VERSION", LATEST_EPLUS_VER),
        "5ZoneAirCooledWithSpaces.idf"
    ))
    geometry <- idf$geometry()
    options <- list(
        background = "#ffffff",
        viewpoint = list(look_at = "iso"),
        mouse_mode = list(),
        axis = TRUE,
        ground = FALSE,
        wireframe = TRUE,
        x_ray = FALSE,
        render_by = "surface_type",
        show = list(type = "all", zone = NULL, space = NULL, surface = NULL)
    )
    scene <- viewer__build_idf_view_scene(geometry, options)

    expect_named(
        scene,
        c(
            "scene_id",
            "building",
            "objects",
            "geometry",
            "options",
            "results",
            "diagnostics"
        )
    )
    expect_null(scene$results)
    expect_false(scene$diagnostics$enabled)
    expect_true(length(scene$objects) > 0L)
    positions <- readBin(
        jsonlite::base64_dec(scene$geometry$positions),
        "numeric",
        size = 4L,
        n = 1e7,
        endian = "little"
    )
    face_objects <- readBin(
        jsonlite::base64_dec(scene$geometry$face_objects),
        "integer",
        size = 4L,
        n = 1e7,
        endian = "little"
    )
    edge_positions <- readBin(
        jsonlite::base64_dec(scene$geometry$edge_positions),
        "numeric",
        size = 4L,
        n = 1e7,
        endian = "little"
    )
    edge_objects <- readBin(
        jsonlite::base64_dec(scene$geometry$edge_objects),
        "integer",
        size = 4L,
        n = 1e7,
        endian = "little"
    )

    expect_equal(length(positions) %% 9L, 0L)
    expect_length(face_objects, length(positions) / 9L)
    expect_equal(length(edge_positions) %% 6L, 0L)
    expect_length(edge_objects, length(edge_positions) / 6L)
    expect_true(all(face_objects >= 0L & face_objects < length(scene$objects)))
})

test_that("multiple result variables preserve independent packed frames", {
    skip_on_cran()
    skip_if_not_installed("decido")

    idf <- read_idf(path_eplus_example(
        Sys.getenv("EPLUSR_VIEWER_TEST_VERSION", LATEST_EPLUS_VER),
        "5ZoneAirCooledWithSpaces.idf"
    ))
    geoms <- copy_list(get_priv_env(idf$geometry())$geoms())
    geoms <- align_coord_system(geoms, "world", "world", "world")
    objects <- viewer__object_metadata(geoms)
    zone <- na.omit(objects$zone_name)[[1L]]
    data <- data.frame(
        zone = rep(zone, 2L),
        period = c("A", "B"),
        temperature = c(20, 21),
        load = c(100, 150)
    )
    payload <- viewer__results_payload(
        list(
            list(
                data = data,
                value = "temperature",
                object = "zone",
                key = "zone",
                time = "period",
                label = "Temperature",
                units = "C",
                palette = "viridis",
                limits = NULL
            ),
            list(
                data = data,
                value = "load",
                object = "zone",
                key = "zone",
                time = "period",
                label = "Load",
                units = "W",
                palette = "diverging",
                limits = NULL
            )
        ),
        objects
    )

    expect_identical(payload$active, 0L)
    expect_length(payload$variables, 2L)
    expect_identical(
        vapply(payload$variables, `[[`, character(1L), "label"),
        c("Temperature", "Load")
    )
    expect_identical(payload$variables[[2L]]$range, c(100, 150))
})

test_that("adjacency diagnostics flag close vertices but ignore exact matches", {
    vertices <- data.table(
        id = c(1L, 2L, 3L),
        x = c(0, 0.005, 0),
        y = 0,
        z = 0
    )
    data.table::setkey(vertices, id)
    geoms <- list(vertices = vertices)
    objects <- data.table(id = c(1L, 2L, 3L), object_index = 0:2)
    diagnostics <- viewer__adjacency_diagnostics(
        geoms,
        objects,
        list(enabled = TRUE, tolerance = 0.01)
    )

    expect_true(diagnostics$enabled)
    expect_identical(diagnostics$count, 2L)
    expect_length(
        readBin(
            jsonlite::base64_dec(diagnostics$point_positions),
            "numeric",
            size = 4L,
            n = 100L,
            endian = "little"
        ),
        6L
    )
})

test_that("geometry diagnostics classify actionable polygon problems", {
    coordinates <- list(
        matrix(c(0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0), ncol = 3L, byrow = TRUE),
        matrix(c(0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0), ncol = 3L, byrow = TRUE),
        matrix(
            c(0, 0, 1, 1, 0, 1, 1, 1, 1.1, 0, 1, 1),
            ncol = 3L,
            byrow = TRUE
        ),
        matrix(
            c(0.2, 0.2, 0, 1.2, 0.2, 0, 1.2, 0.8, 0, 0.2, 0.8, 0),
            ncol = 3L,
            byrow = TRUE
        ),
        matrix(c(0, 0, 2, 1, 1, 2, 0, 1, 2, 1, 0, 2), ncol = 3L, byrow = TRUE)
    )
    vertices <- rbindlist(lapply(seq_along(coordinates), function(id) {
        data.table(
            id = id,
            index = seq_len(nrow(coordinates[[id]])),
            x = coordinates[[id]][, 1L],
            y = coordinates[[id]][, 2L],
            z = coordinates[[id]][, 3L]
        )
    }))
    data.table::setkey(vertices, id)
    objects <- data.table(
        id = 1:5,
        object_index = 0:4,
        name = c("Parent", "Duplicate", "Warped", "Window", "Bow tie"),
        class = "BuildingSurface:Detailed",
        category = c("surface", "surface", "surface", "subsurface", "surface"),
        outside_boundary_condition = NA_character_,
        building_surface_name = c(NA, NA, NA, "Parent", NA)
    )

    issues <- viewer__geometry_issues(list(vertices = vertices), objects, 0.01)
    types <- vapply(issues, `[[`, character(1L), "type")

    expect_true(all(
        c(
            "duplicate",
            "nonplanar",
            "subsurface_outside",
            "self_intersection"
        ) %chin%
            types
    ))
    expect_true(all(vapply(
        issues,
        function(issue) all(issue$object_indices >= 0L),
        logical(1L)
    )))
    duplicate <- issues[[which(types == "duplicate")[[1L]]]]
    expect_match(duplicate$label, "Parent and Duplicate")
})

test_that("DuckDB maps time-varying Zone results before binary transfer", {
    skip_on_cran()
    skip_if_not_installed("decido")

    idf <- read_idf(path_eplus_example(
        Sys.getenv("EPLUSR_VIEWER_TEST_VERSION", LATEST_EPLUS_VER),
        "5ZoneAirCooledWithSpaces.idf"
    ))
    geometry <- idf$geometry()
    geoms <- copy_list(get_priv_env(geometry)$geoms())
    geoms <- align_coord_system(geoms, "world", "world", "world")
    objects <- viewer__object_metadata(geoms)
    zone <- na.omit(objects$zone_name)[[1L]]
    payload <- viewer__result_payload(
        list(
            data = data.frame(
                zone = rep(zone, 2L),
                period = c("A", "B"),
                value = c(1.5, 2.5)
            ),
            value = "value",
            object = "zone",
            key = "zone",
            time = "period",
            label = "Zone value",
            units = "W",
            palette = "viridis",
            limits = NULL
        ),
        objects
    )

    expect_identical(payload$times, c("A", "B"))
    expect_identical(payload$shape, c(2L, nrow(objects)))
    values <- readBin(
        jsonlite::base64_dec(payload$values),
        "numeric",
        size = 4L,
        n = 1e7,
        endian = "little"
    )
    expect_length(values, prod(payload$shape))
    expect_true(any(is.nan(values)))
    expect_true(any(values == 1.5))
    expect_true(any(values == 2.5))
})

test_that("triangulation preserves polygons and subsurface holes", {
    skip_on_cran()
    skip_if_not_installed("decido")

    idf <- read_idf(path_eplus_example(
        Sys.getenv("EPLUSR_VIEWER_TEST_VERSION", LATEST_EPLUS_VER),
        "HospitalLowEnergy.idf"
    ))
    geoms <- extract_geom(idf)
    geoms <- align_coord_system(geoms, "world", "world", "world")
    vertices <- viewer__triangulate_geoms(geoms)
    expect_s3_class(vertices, "data.table")
    expect_true(nrow(vertices) > 0L)
    expect_false(anyNA(vertices[, c("x", "y", "z")]))
})
# }}}

# vim: set fdm=marker:

test_that("Geometry triangulation preserves polygon holes", {
    skip_if_not_installed("decido")

    geoms <- list(
        surface = data.table(id = 1L, name = "wall"),
        subsurface = data.table(
            id = 2L,
            name = "window",
            building_surface_name = "wall"
        ),
        vertices = rbindlist(list(
            data.table(
                id = 1L,
                index = 1:4,
                x = c(0, 4, 4, 0),
                y = c(0, 0, 4, 4),
                z = 0
            ),
            data.table(
                id = 2L,
                index = 1:3,
                x = c(1, 2, 1),
                y = c(1, 1, 2),
                z = 0
            )
        ))
    )

    triangulated <- viewer__triangulate_geoms(geoms)
    expect_true(all(is.finite(triangulated$x)))
    expect_true(all(is.finite(triangulated$y)))
    expect_true(all(is.finite(triangulated$z)))
    expect_equal(nrow(triangulated[id == 1L]), 21L)
    expect_equal(nrow(triangulated[id == 2L]), 3L)

    # Compare total triangle area instead of implementation-specific indices.
    surface <- triangulated[id == 1L]
    surface[, triangle := rep(seq_len(.N %/% 3L), each = 3L)]
    area <- surface[,
        by = "triangle",
        .(
            area = abs(
                (x[2L] - x[1L]) *
                    (y[3L] - y[1L]) -
                    (x[3L] - x[1L]) * (y[2L] - y[1L])
            ) /
                2
        )
    ][, sum(area)]
    expect_equal(area, 15.5, tolerance = 1e-10)
})

test_that("failed HTML snapshots preserve the existing destination", {
    skip_if_not_installed("htmlwidgets")
    target <- tempfile(fileext = ".html")
    writeLines("existing report", target)
    viewer <- list(widget = function(live) list())
    testthat::local_mocked_bindings(
        saveWidget = function(...) warning("capture unavailable"),
        .package = "htmlwidgets"
    )
    expect_error(
        viewer__snapshot(viewer, target),
        class = "eplusr_error_snapshot_failed"
    )
    expect_identical(readLines(target), "existing report")
})

test_that("empty HTML snapshots are rejected before replacing output", {
    skip_if_not_installed("htmlwidgets")
    target <- tempfile(fileext = ".html")
    writeLines("existing report", target)
    viewer <- list(widget = function(live) list())
    testthat::local_mocked_bindings(
        saveWidget = function(widget, file, ...) file.create(file),
        .package = "htmlwidgets"
    )
    expect_error(
        viewer__snapshot(viewer, target),
        "nonempty",
        class = "eplusr_error_snapshot_failed"
    )
    expect_identical(readLines(target), "existing report")
})

test_that("valid portable exports replace only the validated destination", {
    skip_if_not_installed("htmlwidgets")
    target <- tempfile(fileext = ".html")
    writeLines("existing report", target)
    viewer <- list(widget = function(live) {
        expect_false(live)
        list()
    })
    testthat::local_mocked_bindings(
        saveWidget = function(widget, file, ...) {
            writeLines("<html>viewer</html>", file)
        },
        .package = "htmlwidgets"
    )
    expect_identical(viewer__snapshot(viewer, target), normalizePath(target))
    expect_identical(readLines(target), "<html>viewer</html>")
})

# SQL source queries and frame preparation.
# viewer__sql_fixture {{{
# Build the smallest EnergyPlus-compatible SQLite result needed to test lazy viewer queries.
viewer__sql_fixture <- function(zone) {
    path <- tempfile(fileext = ".sql")
    con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(duckdb::dbDisconnect(con), add = TRUE)
    load_duckdb_sqlite(con)
    path_sql <- duckdb::dbQuoteLiteral(con, path)
    duckdb::sql_exec(
        paste0("ATTACH ", path_sql, " AS viewer_fixture (TYPE sqlite)"),
        conn = con
    )
    attached <- TRUE
    # Always detach the SQLite file before another connection opens the fixture.
    on.exit(
        {
            if (attached) {
                try(duckdb::sql_exec("USE memory", conn = con), silent = TRUE)
                try(
                    duckdb::sql_exec("DETACH viewer_fixture", conn = con),
                    silent = TRUE
                )
            }
        },
        add = TRUE
    )
    duckdb::sql_exec("USE viewer_fixture", conn = con)

    # Materialize one data frame as a physical table in the attached SQLite database.
    write_fixture_table <- function(name, data) {
        duckdb::duckdb_register(con, "viewer_fixture_data", data)
        on.exit(
            duckdb::duckdb_unregister(con, "viewer_fixture_data"),
            add = TRUE
        )
        name_sql <- duckdb::dbQuoteIdentifier(con, name)
        duckdb::sql_exec(
            paste0(
                "CREATE TABLE ",
                name_sql,
                " AS SELECT * FROM viewer_fixture_data"
            ),
            conn = con
        )
    }

    write_fixture_table(
        "ReportDataDictionary",
        data.frame(
            ReportDataDictionaryIndex = c(1L, 2L),
            IsMeter = 0L,
            Type = "Average",
            IndexGroup = "Zone",
            TimestepType = "Zone",
            KeyValue = zone,
            Name = c(
                "Zone Mean Air Temperature",
                "Zone Air System Sensible Heating Rate"
            ),
            ReportingFrequency = c("Hourly", "TimeStep"),
            ScheduleName = "",
            Units = c("C", "W")
        )
    )
    write_fixture_table(
        "ReportData",
        data.frame(
            ReportDataIndex = 1:5,
            TimeIndex = c(1L, 2L, 3L, 1L, 2L),
            ReportDataDictionaryIndex = c(1L, 1L, 1L, 2L, 2L),
            Value = c(21.5, 22.5, 99, 100, 150)
        )
    )
    write_fixture_table(
        "Time",
        data.frame(
            TimeIndex = 1:3,
            Year = 2017L,
            Month = 1L,
            Day = 1L,
            Hour = c(1L, 2L, 3L),
            Minute = 0L,
            DST = 0L,
            Interval = 60L,
            IntervalType = 1L,
            SimulationDays = 1L,
            DayType = "Sunday",
            EnvironmentPeriodIndex = 1L,
            WarmupFlag = c(0L, 0L, 1L)
        )
    )
    write_fixture_table(
        "EnvironmentPeriods",
        data.frame(
            EnvironmentPeriodIndex = 1L,
            SimulationIndex = 1L,
            EnvironmentName = "RUN PERIOD 1",
            EnvironmentType = 3L
        )
    )
    duckdb::sql_exec("USE memory", conn = con)
    duckdb::sql_exec("DETACH viewer_fixture", conn = con)
    attached <- FALSE
    path
}
# }}}

test_that("EplusSql frame provider queries only the selected variable and frame", {
    skip_on_cran()
    zone <- "Test Zone"
    objects <- data.table(
        id = 1L,
        name = "Test Surface",
        zone_name = zone,
        space_name = NA_character_,
        object_index = 0L
    )
    sql <- eplus_sql(viewer__sql_fixture(zone))
    specs <- viewer__sql_result_specs(
        sql,
        variable = "Zone Mean Air Temperature",
        reporting_frequency = "Hourly",
        environment_name = "run period 1",
        mode = "provider"
    )
    provider <- IdfViewerFrameProvider$new(specs, objects)
    on.exit(provider$close(), add = TRUE)
    payload <- provider$metadata("/api/viewer/frames")$variables[[1L]]

    expect_length(specs, 1L)
    expect_identical(payload$times, c("01-01 01:00", "01-01 02:00"))
    expect_identical(payload$range, c(21.5, 22.5))
    values <- readBin(
        provider$fetch("0", "1"),
        "numeric",
        size = 4L,
        n = nrow(objects),
        endian = "little"
    )
    expect_true(any(values == 22.5, na.rm = TRUE))
    expect_false(any(values == 99, na.rm = TRUE))
})
# }}}

# vim: set fdm=marker:

test_that("viewer time zones and date-times match report_data and catalogs omit blank keys", {
    skip_on_cran()
    sql <- eplus_sql(viewer__sql_fixture("Test Zone"))
    objects <- data.table(
        id = 1L,
        name = "Test Surface",
        zone_name = "Test Zone",
        space_name = "",
        object_index = 0L
    )
    filters <- list(
        year = 2020L,
        tz = "Europe/Paris",
        period = as.POSIXct("2020-01-01 02:00:00", tz = "Europe/Paris")
    )
    expected <- do.call(
        sql$report_data,
        c(list(name = "Zone Mean Air Temperature"), filters)
    )
    specs <- viewer__sql_result_specs(
        sql,
        variable = "Zone Mean Air Temperature",
        report_filters = filters
    )
    payload <- viewer__results_payload(specs, objects)$variables[[1L]]
    expect_identical(
        payload$times,
        format(expected$datetime, tz = "Europe/Paris", usetz = TRUE)
    )
    expect_equal(
        readBin(
            jsonlite::base64_dec(payload$values),
            "numeric",
            size = 4L,
            n = 1L
        ),
        expected$value
    )
    browser <- viewer__browser_result_filters(list(
        period = list("2020-01-01T02:00"),
        tz = "Europe/Paris"
    ))
    expect_equal(browser$period, filters$period)
    expect_error(
        viewer__browser_result_filters(list(period = "not a date")),
        "standard|Invalid"
    )
    catalog <- viewer__result_catalog(sql, objects)
    expect_identical(sort(catalog$variables), sort(sql$report_data_dict()$name))
    expect_false("" %in% catalog$keys)
})


test_that("viewer queries share report_data calendar and key semantics in both modes", {
    skip_on_cran()
    sql <- eplus_sql(viewer__sql_fixture("Test Zone"))
    objects <- data.table(
        id = 1L,
        name = "Test Surface",
        zone_name = "Test Zone",
        space_name = NA_character_,
        object_index = 0L
    )
    filters <- list(
        key_value = "TEST ZONE",
        month = 1L,
        day = 1L,
        hour = 2L,
        minute = 0L,
        interval = 60L,
        simulation_days = 1L,
        day_type = "weekend",
        period = as.Date("2017-01-01"),
        environment_name = "run period 1"
    )
    expected <- do.call(
        sql$report_data,
        c(list(name = "Zone Mean Air Temperature"), filters)
    )
    expect_equal(expected$value, 22.5)
    specs <- viewer__sql_result_specs(
        sql,
        variable = "Zone Mean Air Temperature",
        reporting_frequency = "Hourly",
        environment_name = filters$environment_name,
        report_filters = filters,
        mode = "provider"
    )
    provider <- IdfViewerFrameProvider$new(specs, objects)
    on.exit(provider$close(), add = TRUE)
    expect_identical(
        provider$metadata("/frames")$variables[[1L]]$shape,
        c(1L, 1L)
    )
    expect_identical(
        readBin(provider$fetch(), "numeric", n = 1, size = 4),
        expected$value
    )
    embedded <- viewer__results_payload(specs, objects)$variables[[1L]]
    expect_identical(
        readBin(
            jsonlite::base64_dec(embedded$values),
            "numeric",
            n = 1,
            size = 4
        ),
        expected$value
    )
    expect_equal(embedded$range, c(22.5, 22.5))
    expect_error(
        viewer__sql_result_specs(sql, report_filters = list(hour = 25)),
        "hour"
    )
    expect_error(
        viewer__sql_result_specs(sql, report_filters = list(month = 12)),
        "No result times"
    )
})

test_that("result dictionary pairs and reporting frequencies are respected", {
    skip_on_cran()
    sql <- eplus_sql(viewer__sql_fixture("Test Zone"))
    pairs <- data.table(
        key_value = "Test Zone",
        name = "Zone Air System Sensible Heating Rate",
        keep = 1L
    )
    specs <- viewer__sql_result_specs(
        sql,
        reporting_frequency = "TimeStep",
        report_filters = list(key_value = pairs)
    )
    expect_equal(specs[[1L]]$dictionary_indices, 2L)
    expect_named(pairs, c("key_value", "name", "keep"))
})

test_that("results accepts EplusJob and defaults to the parent last_job without running it", {
    skip_on_cran()
    path <- path_eplus_example(
        Sys.getenv("EPLUSR_VIEWER_TEST_VERSION", LATEST_EPLUS_VER),
        "5ZoneAirCooledWithSpaces.idf"
    )
    idf <- read_idf(path)
    zone <- names(idf$Zone)[[1L]]
    sql_path <- viewer__sql_fixture(zone)
    idf_path <- sub("\\.sql$", ".idf", sql_path)
    expect_true(file.copy(path, idf_path))
    idf <- read_idf(idf_path)
    viewer <- idf_viewer(idf)
    on.exit(viewer$close(), add = TRUE)
    expect_error(viewer$results(), "no last job")
    job <- eplus_job(idf, NULL)
    # Attach a completed-job record to existing fixture output; never invoke an engine.
    job_private <- get_priv_env(job)
    idf_private <- get_priv_env(idf)
    job_private$m_job <- list(exit_status = 0L)
    idf_private$m_log$job <- job
    expect_identical(idf$last_job(), job)
    viewer$results(
        job,
        name = "Zone Mean Air Temperature",
        hour = 2,
        mode = "embed"
    )
    explicit <- viewer$widget()$x$results$variables[[1L]]
    viewer$results(name = "Zone Mean Air Temperature", hour = 2, mode = "embed")
    implicit <- viewer$widget()$x$results$variables[[1L]]
    expect_identical(explicit$values, implicit$values)
    expect_length(implicit$times, 1L)
    expect_true(viewer$widget()$x$results$query$enabled == FALSE)
    expect_error(viewer$results(job, name = "A", variable = "B"), "aliases")
})

test_that("failed live source queries preserve working provider and source", {
    skip_on_cran()
    path <- path_eplus_example(
        Sys.getenv("EPLUSR_VIEWER_TEST_VERSION", LATEST_EPLUS_VER),
        "5ZoneAirCooledWithSpaces.idf"
    )
    idf <- read_idf(path)
    sql <- eplus_sql(viewer__sql_fixture(names(idf$Zone)[[1L]]))
    viewer <- idf_viewer(idf)
    on.exit(viewer$close(), add = TRUE)
    viewer$results(sql, name = "Zone Mean Air Temperature", mode = "provider")
    viewer$widget(live = TRUE)
    provider <- viewer$frame_provider()
    before <- provider$fetch()
    expect_error(
        viewer$results(sql, name = "Missing", mode = "provider"),
        "No requested"
    )
    expect_identical(viewer$frame_provider()$fetch(), before)
    private <- get_priv_env(viewer)
    private$query_results(list(
        name = "Zone Air System Sensible Heating Rate",
        hour = list(2L)
    ))
    values <- readBin(
        viewer$frame_provider()$fetch(),
        "numeric",
        size = 4,
        n = 1e5
    )
    expect_true(any(values == 150, na.rm = TRUE))
    expect_error(
        private$query_results(list(sql = "DROP TABLE Time")),
        "Unknown"
    )
    expect_error(private$query_results(list(hour = "2 OR 1=1")), "numeric")
})

# Verify real dictionary/time relationships rather than independent option lists.
test_that("viewer catalogs retain only recorded spatial filter combinations", {
    skip_on_cran()
    sql <- eplus_sql(viewer__sql_fixture(c("Zone A", "Zone B")))
    objects <- data.table(
        name = c("Surface A", "Surface B"),
        zone_name = c("Zone A", "Zone B")
    )
    con <- conn_sql(sql$path())
    # Add a warmup-only type and an unrecorded dictionary promise to exercise both exclusions.
    with_sql(con, {
        duckdb::sql_exec(
            "UPDATE Time SET DayType = 'WinterDesignDay' WHERE WarmupFlag = 1",
            conn = con
        )
        duckdb::sql_exec(
            paste(
                "INSERT INTO ReportDataDictionary",
                "SELECT 3, IsMeter, Type, IndexGroup, TimestepType, KeyValue,",
                "'Unused variable', ReportingFrequency, ScheduleName, Units",
                "FROM ReportDataDictionary WHERE ReportDataDictionaryIndex = 1"
            ),
            conn = con
        )
    })
    catalog <- viewer__result_catalog(sql, objects)
    expect_length(catalog$combinations, 2L)
    expect_equal(
        catalog$source_variable_count,
        length(unique(sql$report_data_dict()$name))
    )
    expect_true("Unused variable" %in% catalog$unmapped_variables)
    expect_false("Unused variable" %in% catalog$variables)
    expect_false("WinterDesignDay" %in% catalog$day_types)
    rows <- rbindlist(catalog$combinations)
    expect_equal(rows[name == "Zone Mean Air Temperature", key_value], "Zone A")
    expect_equal(
        rows[name == "Zone Mean Air Temperature", reporting_frequency],
        "Hourly"
    )
    expect_equal(
        rows[name == "Zone Air System Sensible Heating Rate", key_value],
        "Zone B"
    )
    expect_equal(
        rows[
            name == "Zone Air System Sensible Heating Rate",
            reporting_frequency
        ],
        "TimeStep"
    )
    expect_true(all(rows$environment_name == "RUN PERIOD 1"))
    expect_true(all(rows$day_type == "Sunday"))
})

# Reconnection must restore result metadata without replaying unrelated camera actions.
test_that("reconnecting live viewers receive the latest applied result metadata", {
    skip_if_not_installed("httpuv")
    session <- viewer__session(list(
        scene_id = "reconnect-scene",
        objects = list()
    ))
    on.exit(session$close(), add = TRUE)
    connect <- get("on_open", envir = environment(session$receive))
    route <- sub("^ws://[^/]+", "", session$config$websocket)

    # Model only the WebSocket transport; exercise the production connection callback.
    socket <- function() {
        ws <- new.env(parent = emptyenv())
        ws$request <- list(PATH_INFO = route)
        ws$messages <- list()
        # Retain serialized messages as a client would receive them.
        ws$send <- function(message) {
            ws$messages[[length(ws$messages) + 1L]] <- jsonlite::fromJSON(
                message,
                simplifyVector = FALSE
            )
        }
        # Keep handlers so closing a mock client uses the actual removal path.
        ws$onMessage <- function(callback) ws$message_callback <- callback
        ws$onClose <- function(callback) ws$close_callback <- callback
        ws
    }
    first <- socket()
    connect(first)
    results <- list(
        query = list(filters = list(environment_name = "Winter")),
        variables = list(list(times = "01-21 06:00", range = c(10, 21)))
    )
    session$send("results", list(results = results))
    session$send("view", list(value = "top"))
    first$close_callback()
    second <- socket()
    connect(second)
    expect_length(second$messages, 1L)
    expect_identical(second$messages[[1L]]$command, "results")
    expect_identical(
        second$messages[[1L]]$args$results$query$filters$environment_name,
        "Winter"
    )
    expect_identical(
        second$messages[[1L]]$args$results$variables[[1L]]$times,
        "01-21 06:00"
    )
    expect_equal(
        unlist(second$messages[[1L]]$args$results$variables[[1L]]$range),
        c(10, 21)
    )

    session$send("results", list(results = NULL))
    second$close_callback()
    third <- socket()
    connect(third)
    expect_length(third$messages, 1L)
    expect_null(third$messages[[1L]]$args$results)
})
