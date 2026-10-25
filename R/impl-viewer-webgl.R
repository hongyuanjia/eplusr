# viewer__pack_view_float32 {{{
# Encode dense numerical buffers once so htmlwidgets does not format every value as JSON text.
viewer__pack_view_float32 <- function(x) {
    if (!length(x)) {
        return("")
    }
    jsonlite::base64_enc(writeBin(
        as.numeric(x),
        raw(),
        size = 4L,
        endian = "little"
    ))
}
# }}}

# viewer__pack_view_int32 {{{
# Object indices stay zero-based across R and JavaScript to avoid conversion during picking.
viewer__pack_view_int32 <- function(x) {
    if (!length(x)) {
        return("")
    }
    jsonlite::base64_enc(writeBin(
        as.integer(x),
        raw(),
        size = 4L,
        endian = "little"
    ))
}
# }}}

# viewer__object_metadata {{{
# Build a renderer-neutral object table shared by geometry, results, and selection events.
viewer__object_metadata <- function(geoms) {
    add_zone_space_name(geoms)

    normalize <- function(x, category) {
        if (!nrow(x)) {
            return(data.table())
        }
        x <- copy(x)
        required <- c(
            "id",
            "name",
            "class",
            "surface_type",
            "zone_name",
            "space_name",
            "construction_name",
            "outside_boundary_condition",
            "outside_boundary_condition_object",
            "building_surface_name"
        )
        for (column in setdiff(required, names(x))) {
            set(x, NULL, column, NA_character_)
        }
        x <- x[, required, with = FALSE]
        set(x, NULL, "category", category)
        x
    }

    objects <- rbindlist(
        list(
            normalize(geoms$surface, "surface"),
            normalize(geoms$subsurface, "subsurface"),
            normalize(geoms$shading, "shading")
        ),
        use.names = TRUE,
        fill = TRUE
    )
    if (nrow(objects)) {
        set(objects, NULL, "object_index", seq_len(nrow(objects)) - 1L)
    }
    objects
}
# }}}

# viewer__triangulate_view_faces {{{
# Convert every polygon to consecutive triangles while preserving the originating IDF object.
viewer__triangulate_view_faces <- function(geoms, object_ids) {
    vertices <- viewer__triangulate_geoms(geoms)
    vertices <- vertices[J(object_ids), on = "id", nomatch = NULL]
    if (!nrow(vertices)) {
        return(vertices)
    }

    vertices[, by = "id", {
        if (.N == 4L) {
            index <- c(1L, 2L, 3L, 1L, 3L, 4L)
        } else {
            if (.N %% 3L) {
                abort(
                    "Triangulated geometry does not contain complete triangles."
                )
            }
            index <- seq_len(.N)
        }
        .SD[index]
    }]
}
# }}}

# viewer__edge_buffers {{{
# Keep original polygon boundaries so wireframes do not expose triangulation diagonals.
viewer__edge_buffers <- function(geoms, objects) {
    vertices <- geoms$vertices[J(objects$id), on = "id", nomatch = NULL]
    if (!nrow(vertices)) {
        return(list(positions = numeric(), objects = integer()))
    }
    add_joined_cols(objects, vertices, "id", "object_index")

    edge <- vertices[, by = c("id", "object_index"), {
        next_index <- c(seq.int(2L, .N), 1L)
        list(
            x = as.vector(rbind(x, x[next_index])),
            y = as.vector(rbind(y, y[next_index])),
            z = as.vector(rbind(z, z[next_index])),
            segment = rep(seq_len(.N), each = 2L)
        )
    }]
    unique_edges <- edge[!duplicated(edge, by = c("id", "segment"))]
    list(
        positions = as.vector(t(as.matrix(edge[, c("x", "y", "z")]))),
        objects = unique_edges[["object_index"]]
    )
}
# }}}

# viewer__point_buffer {{{
# Daylighting points are sent separately because they are not ray-pickable surface triangles.
viewer__point_buffer <- function(geoms) {
    if (!nrow(geoms$daylighting_point)) {
        return(numeric())
    }
    points <- geoms$vertices[
        J(geoms$daylighting_point$id),
        on = "id",
        nomatch = NULL
    ]
    as.vector(t(as.matrix(points[, c("x", "y", "z")])))
}
# }}}

# viewer__normalize_viewer_limits {{{
# Normalize one shared range or per-variable ranges into a stable list contract.
viewer__normalize_viewer_limits <- function(limits, count) {
    if (is.null(limits)) {
        return(rep(list(NULL), count))
    }
    if (is.numeric(limits)) {
        checkmate::assert_numeric(
            limits,
            len = 2L,
            any.missing = FALSE,
            finite = TRUE
        )
        return(rep(list(as.numeric(limits)), count))
    }
    if (!is.list(limits) || length(limits) != count) {
        abort(sprintf(
            "'limits' must be a numeric pair or a list of length %s.",
            count
        ))
    }
    lapply(limits, function(value) {
        if (is.null(value)) {
            return(NULL)
        }
        checkmate::assert_numeric(
            value,
            len = 2L,
            any.missing = FALSE,
            finite = TRUE
        )
        as.numeric(value)
    })
}
# }}}

# viewer__sql_source {{{
# Resolve supported simulation-result objects to their authoritative SQLite path and dictionary.
viewer__sql_source <- function(source) {
    if (inherits(source, "EplusSql")) {
        return(list(
            path = source$path(),
            dictionary = source$report_data_dict()
        ))
    }
    if (inherits(source, "EplusJob")) {
        path <- job_sql_path(source, get_priv_env(source))
        return(list(path = path, dictionary = source$report_data_dict()))
    }
    abort("'source' must be an EplusSql or completed EplusJob object.")
}
# }}}

# viewer__sql_result_specs {{{
# Convert the EnergyPlus result dictionary into independently selectable spatial variables.
viewer__sql_result_specs <- function(
    source,
    variable = NULL,
    reporting_frequency = NULL,
    environment_name = NULL,
    palette = "viridis",
    limits = NULL,
    mode = "embed",
    report_filters = list()
) {
    resolved <- viewer__sql_source(source)
    dict <- copy(resolved$dictionary)
    if (!is.null(report_filters$key_value)) {
        dict <- subset_sql_report_data_dict(
            dict,
            key_value = copy(report_filters$key_value),
            name = variable
        )
        variable <- NULL
    }
    selected_times <- viewer__sql_filtered_times(
        resolved$path,
        utils::modifyList(
            report_filters,
            list(environment_name = environment_name),
            keep.null = TRUE
        )
    )
    if (!nrow(dict)) {
        abort("No report variables were found in the EnergyPlus SQLite output.")
    }

    if (!is.null(variable)) {
        requested_variables <- tolower(variable)
        dict <- dict[tolower(dict$name) %chin% requested_variables]
    } else {
        # Surface and zone dictionary groups are the result families that can be mapped spatially.
        dict <- dict[grepl(
            "surface|zone|space",
            dict$index_group,
            ignore.case = TRUE
        )]
    }
    if (!is.null(reporting_frequency)) {
        requested_frequencies <- tolower(reporting_frequency)
        dict <- dict[
            tolower(dict$reporting_frequency) %chin% requested_frequencies
        ]
    }
    if (!nrow(dict)) {
        abort(
            "No requested spatial report variables were found in the EnergyPlus SQLite output."
        )
    }

    groups <- unique(dict[,
        c("name", "reporting_frequency", "units", "index_group"),
        with = FALSE
    ])
    duplicated_names <- duplicated(tolower(groups$name)) |
        duplicated(tolower(groups$name), fromLast = TRUE)
    limit_values <- if (is.null(limits)) list() else limits
    if (!is.list(limit_values)) {
        abort(
            "'limits' must be a named list when attaching EnergyPlus SQLite results."
        )
    }

    lapply(seq_len(nrow(groups)), function(index) {
        group <- groups[index]
        rows <- dict[
            dict$name == group$name[[1L]] &
                dict$reporting_frequency == group$reporting_frequency[[1L]] &
                dict$units == group$units[[1L]] &
                dict$index_group == group$index_group[[1L]]
        ]
        label <- group$name
        if (duplicated_names[[index]]) {
            label <- sprintf("%s \u00b7 %s", label, group$reporting_frequency)
        }
        limit <- limit_values[[group$name]] %||% NULL
        if (!is.null(limit)) {
            checkmate::assert_numeric(
                limit,
                len = 2L,
                any.missing = FALSE,
                finite = TRUE
            )
        }
        list(
            source_type = "sql",
            selected_times = selected_times,
            report_filters = report_filters,
            path = resolved$path,
            dictionary = rows,
            dictionary_indices = rows$report_data_dictionary_index,
            value = "value",
            object = "auto",
            key = "key_value",
            time = "datetime",
            label = label,
            units = group$units[[1L]],
            palette = palette,
            limits = limit,
            environment_name = environment_name,
            mode = mode
        )
    })
}
# }}}

# viewer__sql_scope {{{
# Resolve the spatial scope once so embedded and lazy result paths use identical matching.
viewer__sql_scope <- function(spec, objects) {
    suggested <- if (
        grepl("surface", spec$dictionary$index_group[[1L]], ignore.case = TRUE)
    ) {
        "surface"
    } else if (
        grepl("zone", spec$dictionary$index_group[[1L]], ignore.case = TRUE)
    ) {
        "zone"
    }
    viewer__infer_viewer_result_scope(
        spec$dictionary$key_value,
        objects,
        suggested
    )
}
# }}}

# viewer__infer_viewer_result_scope {{{
# Infer Surface, Zone, or Space scope by comparing EnergyPlus keys with scene metadata.
viewer__infer_viewer_result_scope <- function(keys, objects, suggested = NULL) {
    keys <- unique(tolower(trimws(as.character(keys))))
    keys <- keys[nzchar(keys)]
    candidates <- c(surface = "name", zone = "zone_name", space = "space_name")
    scores <- vapply(
        candidates,
        function(column) {
            targets <- unique(tolower(trimws(as.character(objects[[column]]))))
            sum(keys %chin% targets[nzchar(targets)])
        },
        integer(1L)
    )
    if (!any(scores > 0L)) {
        return(NULL)
    }
    winners <- names(scores)[scores == max(scores)]
    if (!is.null(suggested) && suggested %chin% winners) {
        return(suggested)
    }
    winners[[1L]]
}
# }}}

# viewer__sql_embed_spec {{{
# Materialize selected SQLite variables only for standalone HTML embedding.
viewer__sql_embed_spec <- function(spec, objects) {
    scope <- viewer__sql_scope(spec, objects)
    if (is.null(scope)) {
        return(NULL)
    }
    keys <- spec$dictionary[, c("key_value", "name"), with = FALSE]
    data <- get_sql_report_data(
        spec$path,
        key_value = keys,
        case = NULL,
        all = TRUE,
        environment_name = spec$environment_name
    )
    if (!nrow(data)) {
        return(NULL)
    }
    if (data.table::uniqueN(data$environment_name) > 1L) {
        set(
            data,
            NULL,
            ".viewer_datetime",
            paste(data$environment_name, data$datetime, sep = " \u00b7 ")
        )
        time <- ".viewer_datetime"
    } else {
        time <- "datetime"
    }
    utils::modifyList(
        spec,
        list(
            source_type = "data",
            data = data,
            object = scope,
            key = "key_value",
            time = time,
            mode = "embed"
        )
    )
}
# }}}

# viewer__sql_time_query {{{
# Build the fixed-schema EnergyPlus time query with only validated dictionary IDs and quoted filters.
viewer__sql_time_query <- function(spec, con) {
    dictionary_indices <- unique(as.integer(spec$dictionary_indices))
    dictionary_indices <- dictionary_indices[!is.na(dictionary_indices)]
    if (!length(dictionary_indices)) {
        abort("No valid report dictionary indices were selected.")
    }
    selected_filter <- if (isTRUE(spec$filter_times)) {
        "AND t.TimeIndex IN (SELECT time_index FROM viewer_filter_times)"
    } else {
        ""
    }
    environment_filter <- selected_filter
    if (length(spec$environment_name)) {
        values <- vapply(
            tolower(spec$environment_name),
            function(value) {
                as.character(duckdb::dbQuoteLiteral(con, value))
            },
            character(1L)
        )
        environment_filter <- paste(
            selected_filter,
            sprintf(
                "AND lower(ep.EnvironmentName) IN (%s)",
                paste(values, collapse = ", ")
            )
        )
    }
    sprintf(
        paste(
            "SELECT DISTINCT t.TimeIndex AS time_index, ep.EnvironmentName AS environment_name,",
            "t.EnvironmentPeriodIndex AS environment_period_index,",
            "t.Month AS month, t.Day AS day, t.Hour AS hour, t.Minute AS minute",
            "FROM Time t",
            "LEFT JOIN EnvironmentPeriods ep ON ep.EnvironmentPeriodIndex = t.EnvironmentPeriodIndex",
            "JOIN ReportData rd ON rd.TimeIndex = t.TimeIndex",
            "WHERE coalesce(t.WarmupFlag, 0) = 0",
            "AND rd.ReportDataDictionaryIndex IN (%s) %s",
            "ORDER BY t.TimeIndex"
        ),
        paste(dictionary_indices, collapse = ", "),
        environment_filter
    )
}
# }}}

# viewer__sql_time_labels {{{
# Format Hourly through Annual EnergyPlus rows without assuming every calendar field is populated.
viewer__sql_time_labels <- function(times) {
    labels <- vapply(
        seq_len(nrow(times)),
        function(index) {
            row <- times[index, , drop = FALSE]
            fields <- unlist(
                row[c("month", "day", "hour", "minute")],
                use.names = FALSE
            )
            if (all(is.finite(fields))) {
                return(sprintf(
                    "%02d-%02d %02d:%02d",
                    fields[[1L]],
                    fields[[2L]],
                    fields[[3L]],
                    fields[[4L]]
                ))
            }
            if (all(is.finite(fields[1:2]))) {
                return(sprintf("%02d-%02d", fields[[1L]], fields[[2L]]))
            }
            if (is.finite(fields[[1L]])) {
                return(sprintf("Month %02d", fields[[1L]]))
            }
            sprintf("Frame %s", index)
        },
        character(1L)
    )
    environment <- as.character(times$environment_name)
    missing_environment <- is.na(environment) | !nzchar(environment)
    environment[missing_environment] <- paste(
        "Environment",
        times$environment_period_index[missing_environment]
    )
    if (length(unique(environment)) > 1L) {
        labels <- paste(environment, labels, sep = " \u00b7 ")
    }
    labels
}
# }}}

# viewer__sql_mapped_query {{{
# Map one selected EnergyPlus time index directly onto compact viewer object indices.
viewer__sql_mapped_query <- function(spec, scope, con, time_index = NULL) {
    dictionary_indices <- unique(as.integer(spec$dictionary_indices))
    dictionary_indices <- dictionary_indices[!is.na(dictionary_indices)]
    target <- switch(
        scope,
        surface = "name",
        zone = "zone_name",
        space = "space_name"
    )
    time_filter <- if (is.null(time_index)) {
        ""
    } else {
        sprintf(
            "AND rd.TimeIndex IN (%s)",
            paste(unique(as.integer(time_index)), collapse = ", ")
        )
    }
    time_group <- if (is.null(time_index)) ", rd.TimeIndex" else ""
    selected_filter <- if (isTRUE(spec$filter_times)) {
        "AND t.TimeIndex IN (SELECT time_index FROM viewer_filter_times)"
    } else {
        ""
    }
    environment_filter <- selected_filter
    if (length(spec$environment_name)) {
        values <- vapply(
            tolower(spec$environment_name),
            function(value) {
                as.character(duckdb::dbQuoteLiteral(con, value))
            },
            character(1L)
        )
        environment_filter <- paste(
            selected_filter,
            sprintf(
                "AND lower(ep.EnvironmentName) IN (%s)",
                paste(values, collapse = ", ")
            )
        )
    }
    sprintf(
        paste(
            if (is.null(time_index)) {
                "SELECT o.object_index, rd.TimeIndex AS time_index, avg(rd.Value) AS value"
            } else {
                "SELECT o.object_index, avg(rd.Value) AS value"
            },
            "FROM ReportData rd",
            "JOIN ReportDataDictionary d ON d.ReportDataDictionaryIndex = rd.ReportDataDictionaryIndex",
            "JOIN Time t ON t.TimeIndex = rd.TimeIndex",
            "LEFT JOIN EnvironmentPeriods ep ON ep.EnvironmentPeriodIndex = t.EnvironmentPeriodIndex",
            "JOIN viewer_objects o ON lower(cast(d.KeyValue AS VARCHAR)) = lower(cast(o.%s AS VARCHAR))",
            "WHERE coalesce(t.WarmupFlag, 0) = 0",
            "AND rd.ReportDataDictionaryIndex IN (%s) %s %s",
            "GROUP BY o.object_index%s ORDER BY o.object_index"
        ),
        as.character(duckdb::dbQuoteIdentifier(con, target)),
        paste(dictionary_indices, collapse = ", "),
        time_filter,
        environment_filter,
        time_group
    )
}
# }}}

# viewer__prepare_viewer_result {{{
# Validate result columns once and add stable ordering fields used by both transfer modes.
viewer__prepare_viewer_result <- function(spec) {
    data <- as.data.frame(spec$data)
    value <- spec$value
    assert_string(value)
    if (!value %chin% names(data)) {
        abort(sprintf(
            "Column %s does not exist in result data.",
            surround(value)
        ))
    }
    if (!is.numeric(data[[value]])) {
        abort(sprintf("Result column %s must be numeric.", surround(value)))
    }

    scope <- spec$object
    key <- spec$key
    if (is.null(key) && scope != "building") {
        candidates <- switch(
            scope,
            surface = c("id", "surface", "surface_name", "name", "key_value"),
            zone = c("zone", "zone_name", "name", "key_value"),
            space = c("space", "space_name", "name", "key_value")
        )
        key <- candidates[candidates %chin% names(data)][1L]
        if (!length(key) || is.na(key)) {
            abort(sprintf(
                "Could not infer the result key column for %s values; supply %s.",
                scope,
                surround("key")
            ))
        }
    }
    if (!is.null(key) && !key %chin% names(data)) {
        abort(sprintf(
            "Column %s does not exist in result data.",
            surround(key)
        ))
    }

    time <- spec$time
    if (!is.null(time) && !time %chin% names(data)) {
        abort(sprintf(
            "Column %s does not exist in result data.",
            surround(time)
        ))
    }
    data$.viewer_row <- seq_len(nrow(data))
    data$.viewer_time <- if (is.null(time)) {
        "Value"
    } else {
        as.character(data[[time]])
    }

    list(data = data, value = value, scope = scope, key = key)
}
# }}}

# viewer__result_query {{{
# Build the package-owned SQL mapping; callers can only select a validated time label.
viewer__result_query <- function(prepared, con, time_label = NULL) {
    data <- prepared$data
    scope <- prepared$scope
    key <- prepared$key
    quote_id <- function(x) as.character(duckdb::dbQuoteIdentifier(con, x))
    value_sql <- paste0("d.", quote_id(prepared$value))
    if (scope == "building") {
        join <- "TRUE"
    } else {
        target <- switch(
            scope,
            surface = if (is.numeric(data[[key]])) "id" else "name",
            zone = "zone_name",
            space = "space_name"
        )
        join <- sprintf(
            "lower(cast(d.%s AS VARCHAR)) = lower(cast(o.%s AS VARCHAR))",
            quote_id(key),
            quote_id(target)
        )
    }
    where <- if (is.null(time_label)) {
        ""
    } else {
        labels <- vapply(
            time_label,
            function(value) {
                as.character(duckdb::dbQuoteLiteral(con, value))
            },
            character(1L)
        )
        sprintf(
            "WHERE d.\".viewer_time\" IN (%s)",
            paste(labels, collapse = ", ")
        )
    }
    aggregate_frames <- length(time_label) > 1L
    time_select <- if (aggregate_frames) {
        "min(d.\".viewer_time\")"
    } else {
        "d.\".viewer_time\""
    }
    time_group <- if (aggregate_frames) "" else ", d.\".viewer_time\""
    sprintf(
        paste(
            "SELECT o.object_index, %s AS time_label,",
            "min(d.\".viewer_row\") AS time_order, avg(%s) AS value",
            "FROM viewer_objects o JOIN viewer_data d ON %s %s",
            "GROUP BY o.object_index%s",
            "ORDER BY time_order, o.object_index"
        ),
        time_select,
        value_sql,
        join,
        where,
        time_group
    )
}
# }}}

# viewer__abort_viewer_frame {{{
# Attach an HTTP-compatible status without coupling the provider to an Ambiorix response object.
viewer__abort_viewer_frame <- function(message, status) {
    abort(message, "viewer_frame", status = as.integer(status))
}
# }}}

# viewer__parse_viewer_variable_index {{{
# Parse the zero-based variable value supplied directly from an Ambiorix query string.
viewer__parse_viewer_variable_index <- function(value, count) {
    if (is.null(value)) {
        value <- "0"
    }
    if (
        length(value) != 1L ||
            is.na(value) ||
            !grepl("^[0-9]+$", as.character(value))
    ) {
        viewer__abort_viewer_frame("Invalid result variable index.", 400L)
    }
    index <- suppressWarnings(as.integer(value)) + 1L
    if (is.na(index) || index < 1L || index > count) {
        viewer__abort_viewer_frame("Unknown result variable.", 404L)
    }
    index
}
# }}}

# viewer__parse_viewer_frame_indices {{{
# Parse integer vectors or comma-separated zero-based frame indices from an Ambiorix request.
viewer__parse_viewer_frame_indices <- function(value, count) {
    if (is.null(value)) {
        value <- "0"
    }
    if (is.numeric(value)) {
        if (
            !length(value) ||
                anyNA(value) ||
                any(!is.finite(value)) ||
                any(value != floor(value)) ||
                any(value < 0L) ||
                any(value >= .Machine$integer.max)
        ) {
            viewer__abort_viewer_frame("Invalid frame indices.", 400L)
        }
        indices <- unique(as.integer(value)) + 1L
    } else {
        if (
            length(value) != 1L ||
                is.na(value) ||
                !grepl("^[0-9]+(?:,[0-9]+)*$", value)
        ) {
            viewer__abort_viewer_frame("Invalid frame indices.", 400L)
        }
        indices <- suppressWarnings(as.integer(strsplit(
            value,
            ",",
            fixed = TRUE
        )[[1L]])) +
            1L
    }
    if (anyNA(indices) || any(indices < 1L | indices > count)) {
        viewer__abort_viewer_frame("Unknown result frame.", 404L)
    }
    indices
}
# }}}

# viewer__frame_source_close {{{
# Create one idempotent cleanup closure for a provider-owned DuckDB connection.
viewer__frame_source_close <- function(con, registrations) {
    state <- new.env(parent = emptyenv())
    state$closed <- FALSE
    function() {
        if (state$closed) {
            return(invisible())
        }
        state$closed <- TRUE
        for (name in registrations) {
            try(duckdb::duckdb_unregister(con, name), silent = TRUE)
        }
        try(duckdb::dbDisconnect(con), silent = TRUE)
        invisible()
    }
}
# }}}

# viewer__sql_frame_source {{{
# Keep one EnergyPlus SQLite variable attached until its framework-independent provider closes.
viewer__sql_frame_source <- function(spec, objects) {
    scope <- viewer__sql_scope(spec, objects)
    if (is.null(scope)) {
        return(NULL)
    }
    con <- conn_sql(spec$path)
    complete <- FALSE
    close <- viewer__frame_source_close(
        con,
        c("viewer_objects", "viewer_filter_times")
    )
    on.exit(if (!complete) close(), add = TRUE)

    duckdb::duckdb_register(con, "viewer_objects", as.data.frame(objects))
    if (!is.null(spec$selected_times)) {
        duckdb::duckdb_register(
            con,
            "viewer_filter_times",
            as.data.frame(spec$selected_times[, "time_index", with = FALSE])
        )
        spec$filter_times <- TRUE
    }
    times <- duckdb::sql_query(viewer__sql_time_query(spec, con), conn = con)
    if (!nrow(times)) {
        return(NULL)
    }
    summary <- duckdb::sql_query(
        sprintf(
            "SELECT min(value) AS minimum, max(value) AS maximum FROM (%s) mapped",
            viewer__sql_mapped_query(spec, scope, con)
        ),
        conn = con
    )
    range <- spec$limits
    if (is.null(range)) {
        range <- unlist(summary[1L, c("minimum", "maximum")], use.names = FALSE)
    }
    if (length(range) != 2L || any(!is.finite(range))) {
        range <- c(0, 1)
    }

    # Query only the requested time indices and aggregate multiple frames in DuckDB.
    fetch <- function(indices) {
        mapped <- duckdb::sql_query(
            viewer__sql_mapped_query(
                spec,
                scope,
                con,
                times$time_index[indices]
            ),
            conn = con
        )
        values <- rep(NaN, nrow(objects))
        values[mapped$object_index + 1L] <- mapped$value
        values
    }
    labels <- viewer__sql_time_labels(times)
    if (
        !is.null(spec$report_filters$year) || !is.null(spec$report_filters$tz)
    ) {
        rows <- spec$selected_times[match(times$time_index, time_index)]
        labels <- format(
            rows$datetime,
            tz = spec$report_filters$tz %||% "UTC",
            usetz = TRUE
        )
        if (data.table::uniqueN(times$environment_name) > 1L) {
            labels <- paste(times$environment_name, labels, sep = " \u00b7 ")
        }
    }
    metadata <- list(
        label = spec$label,
        units = spec$units,
        palette = spec$palette,
        times = labels,
        range = as.numeric(range),
        shape = c(nrow(times), nrow(objects))
    )
    # Materialize all selected frames in one grouped SQL query for portable reports.
    embed <- function() {
        mapped <- duckdb::sql_query(
            viewer__sql_mapped_query(spec, scope, con),
            conn = con
        )
        values <- matrix(NaN, nrow = nrow(times), ncol = nrow(objects))
        values[cbind(
            match(mapped$time_index, times$time_index),
            mapped$object_index + 1L
        )] <- mapped$value
        c(
            list(mode = "embed"),
            metadata,
            list(values = viewer__pack_view_float32(as.vector(t(values))))
        )
    }
    complete <- TRUE
    list(metadata = metadata, fetch = fetch, embed = embed, close = close)
}
# }}}

# viewer__data_frame_source {{{
# Register user-supplied tabular results once and query frames through the same provider contract.
viewer__data_frame_source <- function(spec, objects) {
    prepared <- viewer__prepare_viewer_result(spec)
    con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    complete <- FALSE
    close <- viewer__frame_source_close(con, c("viewer_data", "viewer_objects"))
    on.exit(if (!complete) close(), add = TRUE)

    duckdb::duckdb_register(con, "viewer_data", prepared$data)
    duckdb::duckdb_register(con, "viewer_objects", as.data.frame(objects))
    times <- duckdb::sql_query(
        paste(
            "SELECT \".viewer_time\" AS time_label, min(\".viewer_row\") AS time_order",
            "FROM viewer_data GROUP BY \".viewer_time\" ORDER BY time_order"
        ),
        conn = con
    )$time_label
    summary <- duckdb::sql_query(
        sprintf(
            "SELECT min(value) AS minimum, max(value) AS maximum FROM (%s) mapped",
            viewer__result_query(prepared, con)
        ),
        conn = con
    )
    range <- spec$limits
    if (is.null(range)) {
        range <- unlist(summary[1L, c("minimum", "maximum")], use.names = FALSE)
    }
    if (length(range) != 2L || any(!is.finite(range))) {
        range <- c(0, 1)
    }

    # Preserve the provider's dense object order for browser-side typed arrays.
    fetch <- function(indices) {
        mapped <- duckdb::sql_query(
            viewer__result_query(prepared, con, times[indices]),
            conn = con
        )
        values <- rep(NaN, nrow(objects))
        values[mapped$object_index + 1L] <- mapped$value
        values
    }
    complete <- TRUE
    list(
        metadata = list(
            label = spec$label %||% prepared$value,
            units = spec$units,
            palette = spec$palette,
            times = as.character(times),
            range = as.numeric(range),
            shape = c(length(times), nrow(objects))
        ),
        fetch = fetch,
        close = close
    )
}
# }}}

# viewer__frame_source {{{
# Dispatch result sources without exposing source-specific DuckDB details to the public R6 class.
viewer__frame_source <- function(spec, objects) {
    if (identical(spec$source_type, "sql")) {
        return(viewer__sql_frame_source(spec, objects))
    }
    viewer__data_frame_source(spec, objects)
}
# }}}

# viewer__result_payload {{{
# Use DuckDB for the object/time join and pack all frames for a standalone widget.
viewer__result_payload <- function(spec, objects) {
    if (is.null(spec)) {
        return(NULL)
    }
    if (identical(spec$source_type, "sql")) {
        source <- viewer__sql_frame_source(spec, objects)
        if (is.null(source)) {
            return(NULL)
        }
        on.exit(source$close(), add = TRUE)
        return(source$embed())
    }
    if (!is.null(spec$mode) && !identical(spec$mode, "embed")) {
        abort(
            "Provider result specifications must be handled by IdfViewerFrameProvider."
        )
    }

    prepared <- viewer__prepare_viewer_result(spec)
    con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(duckdb::dbDisconnect(con), add = TRUE)
    duckdb::duckdb_register(con, "viewer_data", prepared$data)
    duckdb::duckdb_register(con, "viewer_objects", as.data.frame(objects))
    on.exit(duckdb::duckdb_unregister(con, "viewer_data"), add = TRUE)
    on.exit(duckdb::duckdb_unregister(con, "viewer_objects"), add = TRUE)

    mapped <- duckdb::sql_query(viewer__result_query(prepared, con), conn = con)
    if (!nrow(mapped)) {
        abort("No result rows could be matched to viewer objects.")
    }

    times <- unique(mapped$time_label[order(mapped$time_order)])
    matrix <- matrix(NaN, nrow = length(times), ncol = nrow(objects))
    matrix[cbind(
        match(mapped$time_label, times),
        mapped$object_index + 1L
    )] <- mapped$value
    finite <- matrix[is.finite(matrix)]
    range <- spec$limits
    if (is.null(range)) {
        range <- if (length(finite)) base::range(finite) else c(0, 1)
    }

    list(
        mode = "embed",
        label = spec$label %||% prepared$value,
        units = spec$units,
        palette = spec$palette,
        times = times,
        range = as.numeric(range),
        shape = c(length(times), nrow(objects)),
        values = viewer__pack_view_float32(as.vector(t(matrix)))
    )
}
# }}}

# viewer__results_payload {{{
# Preserve multiple selectable variables while omitting SQL variables that cannot map to this model.
viewer__results_payload <- function(specs, objects) {
    if (!length(specs)) {
        return(NULL)
    }
    variables <- Filter(
        Negate(is.null),
        lapply(specs, viewer__result_payload, objects = objects)
    )
    if (!length(variables)) {
        abort("No result variables could be matched to viewer objects.")
    }
    list(active = 0L, variables = variables)
}
# }}}

# viewer__adjacency_diagnostics {{{
# Find close-but-not-coincident vertices on different objects using a bounded spatial hash.
viewer__adjacency_diagnostics <- function(geoms, objects, options) {
    enabled <- isTRUE(options$enabled)
    tolerance <- options$tolerance %||% 0.01
    if (!enabled || !nrow(objects)) {
        return(list(
            enabled = enabled,
            tolerance = tolerance,
            count = 0L,
            point_positions = "",
            object_indices = "",
            issues = list()
        ))
    }

    vertices <- geoms$vertices[J(objects$id), on = "id", nomatch = NULL]
    if (!nrow(vertices)) {
        return(list(
            enabled = TRUE,
            tolerance = tolerance,
            count = 0L,
            point_positions = "",
            object_indices = "",
            issues = list()
        ))
    }
    add_joined_cols(objects, vertices, "id", "object_index")
    vertices <- unique(vertices[,
        c("object_index", "x", "y", "z"),
        with = FALSE
    ])
    cells <- floor(as.matrix(vertices[, c("x", "y", "z")]) / tolerance)
    cell_keys <- apply(cells, 1L, paste, collapse = ":")
    buckets <- split(seq_len(nrow(vertices)), cell_keys)
    offsets <- as.matrix(expand.grid(dx = -1:1, dy = -1:1, dz = -1:1))
    pairs <- vector("list", nrow(vertices))

    # Only compare the 27 neighboring cells; exact shared vertices are valid adjacency.
    for (index in seq_len(nrow(vertices))) {
        neighbor_cells <- sweep(offsets, 2L, cells[index, ], `+`)
        neighbor_keys <- apply(neighbor_cells, 1L, paste, collapse = ":")
        candidates <- unlist(buckets[neighbor_keys], use.names = FALSE)
        candidates <- candidates[
            candidates > index &
                vertices$object_index[candidates] !=
                    vertices$object_index[[index]]
        ]
        if (!length(candidates)) {
            next
        }
        delta <- as.matrix(vertices[candidates, c("x", "y", "z")]) -
            rep(
                as.numeric(vertices[index, c("x", "y", "z")]),
                each = length(candidates)
            )
        distance_squared <- rowSums(delta^2)
        candidates <- candidates[
            distance_squared > .Machine$double.eps &
                distance_squared <= tolerance^2
        ]
        if (length(candidates)) pairs[[index]] <- cbind(index, candidates)
    }
    pairs <- do.call(rbind, pairs)
    if (is.null(pairs) || !nrow(pairs)) {
        return(list(
            enabled = TRUE,
            tolerance = tolerance,
            count = 0L,
            point_positions = "",
            object_indices = "",
            issues = list()
        ))
    }
    midpoints <- (as.matrix(vertices[pairs[, 1L], c("x", "y", "z")]) +
        as.matrix(vertices[pairs[, 2L], c("x", "y", "z")])) /
        2
    object_pairs <- cbind(
        vertices$object_index[pairs[, 1L]],
        vertices$object_index[pairs[, 2L]]
    )
    signature <- paste(
        apply(round(midpoints / tolerance, 4L), 1L, paste, collapse = ":"),
        apply(t(apply(object_pairs, 1L, sort)), 1L, paste, collapse = ":"),
        sep = "|"
    )
    keep <- !duplicated(signature)
    midpoints <- midpoints[keep, , drop = FALSE]
    object_pairs <- object_pairs[keep, , drop = FALSE]
    issues <- lapply(seq_len(nrow(midpoints)), function(index) {
        pair <- as.integer(object_pairs[index, ])
        names <- vapply(
            pair + 1L,
            function(object_index) {
                # Direct helper tests may provide only IDs; diagnostics still need a stable readable label.
                name <- if ("name" %chin% names(objects)) {
                    objects$name[[object_index]]
                } else {
                    NULL
                }
                class <- if ("class" %chin% names(objects)) {
                    objects$class[[object_index]]
                } else {
                    NULL
                }
                value <- name %||% class
                if (!length(value) || is.na(value) || !nzchar(value)) {
                    value <- sprintf(
                        "object %s",
                        objects$object_index[[object_index]]
                    )
                }
                value
            },
            character(1L)
        )
        list(
            type = "near_vertex",
            severity = "warning",
            label = sprintf(
                "Nearby vertices between %s and %s",
                names[[1L]],
                names[[2L]]
            ),
            object_indices = pair,
            point = as.numeric(midpoints[index, ])
        )
    })
    list(
        enabled = TRUE,
        tolerance = tolerance,
        count = nrow(midpoints),
        point_positions = viewer__pack_view_float32(as.vector(t(midpoints))),
        object_indices = viewer__pack_view_int32(as.vector(t(object_pairs))),
        issues = issues
    )
}
# }}}

# viewer__polygon_normal {{{
# Compute a stable Newell normal for arbitrary EnergyPlus polygon vertex counts.
viewer__polygon_normal <- function(coords) {
    if (nrow(coords) < 3L) {
        return(c(0, 0, 0))
    }
    next_index <- c(seq.int(2L, nrow(coords)), 1L)
    normal <- c(
        sum(
            (coords[, 2L] - coords[next_index, 2L]) *
                (coords[, 3L] + coords[next_index, 3L])
        ),
        sum(
            (coords[, 3L] - coords[next_index, 3L]) *
                (coords[, 1L] + coords[next_index, 1L])
        ),
        sum(
            (coords[, 1L] - coords[next_index, 1L]) *
                (coords[, 2L] + coords[next_index, 2L])
        )
    )
    length <- sqrt(sum(normal^2))
    if (!is.finite(length) || length <= .Machine$double.eps) {
        return(c(0, 0, 0))
    }
    normal / length
}
# }}}

# viewer__project_polygon {{{
# Project a 3D polygon onto its most stable two-dimensional coordinate plane.
viewer__project_polygon <- function(coords, normal) {
    # Self-intersecting polygons can have a zero Newell normal; use the thinnest model axis then.
    drop <- if (sum(abs(normal)) > .Machine$double.eps) {
        which.max(abs(normal))
    } else {
        which.min(apply(coords, 2L, function(values) diff(range(values))))
    }
    list(coords = coords[, setdiff(1:3, drop), drop = FALSE], drop = drop)
}
# }}}

# viewer__segments_intersect {{{
# Detect proper or collinear 2D segment intersections within a numerical tolerance.
viewer__segments_intersect <- function(a, b, c, d, tolerance) {
    cross <- function(p, q, r) {
        (q[[1L]] - p[[1L]]) *
            (r[[2L]] - p[[2L]]) -
            (q[[2L]] - p[[2L]]) * (r[[1L]] - p[[1L]])
    }
    on_segment <- function(p, q, r) {
        q[[1L]] >= min(p[[1L]], r[[1L]]) - tolerance &&
            q[[1L]] <= max(p[[1L]], r[[1L]]) + tolerance &&
            q[[2L]] >= min(p[[2L]], r[[2L]]) - tolerance &&
            q[[2L]] <= max(p[[2L]], r[[2L]]) + tolerance
    }
    values <- c(cross(a, b, c), cross(a, b, d), cross(c, d, a), cross(c, d, b))
    if (
        values[[1L]] * values[[2L]] < -tolerance^2 &&
            values[[3L]] * values[[4L]] < -tolerance^2
    ) {
        return(TRUE)
    }
    (abs(values[[1L]]) <= tolerance && on_segment(a, c, b)) ||
        (abs(values[[2L]]) <= tolerance && on_segment(a, d, b)) ||
        (abs(values[[3L]]) <= tolerance && on_segment(c, a, d)) ||
        (abs(values[[4L]]) <= tolerance && on_segment(c, b, d))
}
# }}}

# viewer__polygon_self_intersects {{{
# Check non-adjacent projected polygon edges for self-intersections.
viewer__polygon_self_intersects <- function(coords, tolerance) {
    if (nrow(coords) < 4L) {
        return(FALSE)
    }
    count <- nrow(coords)
    for (first in seq_len(count)) {
        if (first >= count - 1L) {
            next
        }
        first_next <- first %% count + 1L
        for (second in seq.int(first + 1L, count)) {
            second_next <- second %% count + 1L
            if (
                second == first || second == first_next || second_next == first
            ) {
                next
            }
            if (
                viewer__segments_intersect(
                    coords[first, ],
                    coords[first_next, ],
                    coords[second, ],
                    coords[second_next, ],
                    tolerance
                )
            ) {
                return(TRUE)
            }
        }
    }
    FALSE
}
# }}}

# viewer__geometry_issues {{{
# Classify planarity, orientation, duplicate, overlap, containment, and intersection issues.
viewer__geometry_issues <- function(geoms, objects, tolerance) {
    if (!nrow(objects)) {
        return(list())
    }
    vertices <- geoms$vertices[J(objects$id), on = "id", nomatch = NULL]
    if (!nrow(vertices)) {
        return(list())
    }
    add_joined_cols(objects, vertices, "id", "object_index")
    setorderv(vertices, c("object_index", "index"))
    groups <- split(vertices, by = "object_index", keep.by = FALSE)
    metrics <- vector("list", nrow(objects))
    issues <- list()
    threshold <- max(as.numeric(tolerance), 1e-6)
    all_coords <- as.matrix(vertices[, c("x", "y", "z")])
    model_center <- colMeans(all_coords)

    # Keep issue creation centralized so every diagnostic uses zero-based browser indices.
    add_issue <- function(type, severity, label, indices, point) {
        issues[[length(issues) + 1L]] <<- list(
            type = type,
            severity = severity,
            label = label,
            object_indices = as.integer(indices),
            point = as.numeric(point)
        )
    }

    for (name in names(groups)) {
        object_index <- as.integer(name)
        object_row <- object_index + 1L
        coords <- as.matrix(groups[[name]][, c("x", "y", "z")])
        coords <- coords[
            !duplicated(round(coords / threshold, 6L)),
            ,
            drop = FALSE
        ]
        if (nrow(coords) < 3L) {
            next
        }
        normal <- viewer__polygon_normal(coords)
        center <- colMeans(coords)
        projection <- viewer__project_polygon(coords, normal)
        projected <- projection$coords
        distance <- abs(drop(
            (coords - matrix(coords[1L, ], nrow(coords), 3L, byrow = TRUE)) %*%
                normal
        ))
        signature <- paste(
            sort(apply(round(coords / threshold), 1L, paste, collapse = ":")),
            collapse = ";"
        )
        canonical <- normal
        first_nonzero <- which(abs(canonical) > 1e-8)[1L]
        if (!is.na(first_nonzero) && canonical[[first_nonzero]] < 0) {
            canonical <- -canonical
        }
        metrics[[object_row]] <- list(
            coords = coords,
            center = center,
            normal = normal,
            canonical = canonical,
            plane = sum(canonical * center),
            projection = projected,
            drop = projection$drop,
            minimum = apply(projected, 2L, min),
            maximum = apply(projected, 2L, max),
            signature = signature
        )
        # Use a distinct row variable so data.table does not resolve object_index as its column.
        object <- objects[object_row]
        label <- object$name[[1L]] %||% object$class[[1L]]
        if (max(distance) > threshold) {
            add_issue(
                "nonplanar",
                "error",
                sprintf(
                    "%s is non-planar by up to %.5g m",
                    label,
                    max(distance)
                ),
                object_index,
                center
            )
        }
        if (viewer__polygon_self_intersects(projected, threshold)) {
            add_issue(
                "self_intersection",
                "error",
                sprintf("%s has intersecting polygon edges", label),
                object_index,
                center
            )
        }
        if (
            identical(object$category[[1L]], "surface") &&
                identical(
                    tolower(object$outside_boundary_condition[[1L]] %||% ""),
                    "outdoors"
                ) &&
                sum(normal * (center - model_center)) < -threshold
        ) {
            add_issue(
                "reversed_normal",
                "warning",
                sprintf("%s may have a reversed outward normal", label),
                object_index,
                center
            )
        }
    }

    signatures <- split(
        which(vapply(metrics, Negate(is.null), logical(1L))) - 1L,
        vapply(
            Filter(Negate(is.null), metrics),
            `[[`,
            character(1L),
            "signature"
        )
    )
    for (indices in signatures[lengths(signatures) > 1L]) {
        pairs <- utils::combn(indices, 2L)
        for (column in seq_len(ncol(pairs))) {
            pair <- pairs[, column]
            boundaries <- tolower(objects$outside_boundary_condition[pair + 1L])
            if (all(!is.na(boundaries)) && all(boundaries == "surface")) {
                next
            }
            add_issue(
                "duplicate",
                "error",
                sprintf(
                    "%s and %s have duplicate vertices",
                    objects$name[[pair[[1L]] + 1L]],
                    objects$name[[pair[[2L]] + 1L]]
                ),
                pair,
                (metrics[[pair[[1L]] + 1L]]$center +
                    metrics[[pair[[2L]] + 1L]]$center) /
                    2
            )
        }
    }

    surface_indices <- which(
        objects$category == "surface" &
            vapply(metrics, Negate(is.null), logical(1L))
    ) -
        1L
    plane_keys <- vapply(
        surface_indices,
        function(index) {
            metric <- metrics[[index + 1L]]
            paste(
                c(round(metric$canonical, 3L), round(metric$plane / threshold)),
                collapse = ":"
            )
        },
        character(1L)
    )
    for (indices in split(surface_indices, plane_keys)[
        lengths(split(surface_indices, plane_keys)) > 1L
    ]) {
        pairs <- utils::combn(indices, 2L)
        for (column in seq_len(ncol(pairs))) {
            pair <- pairs[, column]
            first <- metrics[[pair[[1L]] + 1L]]
            second <- metrics[[pair[[2L]] + 1L]]
            if (
                first$signature == second$signature || first$drop != second$drop
            ) {
                next
            }
            overlap <- pmin(first$maximum, second$maximum) -
                pmax(first$minimum, second$minimum)
            if (all(overlap > threshold)) {
                add_issue(
                    "overlap",
                    "warning",
                    sprintf(
                        "%s and %s may overlap on the same plane",
                        objects$name[[pair[[1L]] + 1L]],
                        objects$name[[pair[[2L]] + 1L]]
                    ),
                    pair,
                    (first$center + second$center) / 2
                )
            }
        }
    }

    parent_lookup <- stats::setNames(
        objects$object_index,
        tolower(objects$name)
    )
    parent_names <- objects$building_surface_name
    subsurfaces <- which(
        objects$category == "subsurface" &
            !is.na(parent_names) &
            nzchar(parent_names)
    )
    for (row in subsurfaces) {
        child_index <- objects$object_index[[row]]
        parent_index <- unname(parent_lookup[[tolower(objects$building_surface_name[[
            row
        ]])]])
        if (
            is.null(parent_index) ||
                is.na(parent_index) ||
                is.null(metrics[[child_index + 1L]]) ||
                is.null(metrics[[parent_index + 1L]])
        ) {
            next
        }
        child <- metrics[[child_index + 1L]]$coords
        parent <- metrics[[parent_index + 1L]]$coords
        outside <- apply(child, 1L, function(point) {
            any(
                point < apply(parent, 2L, min) - threshold |
                    point > apply(parent, 2L, max) + threshold
            )
        })
        if (any(outside)) {
            add_issue(
                "subsurface_outside",
                "error",
                sprintf(
                    "%s extends outside parent surface %s",
                    objects$name[[row]],
                    objects$building_surface_name[[row]]
                ),
                c(parent_index, child_index),
                colMeans(child)
            )
        }
    }
    issues
}
# }}}

# viewer__geometry_diagnostics {{{
# Merge adjacency candidates with broader geometry-quality diagnostics for one browser payload.
viewer__geometry_diagnostics <- function(geoms, objects, options) {
    diagnostics <- viewer__adjacency_diagnostics(geoms, objects, options)
    if (!isTRUE(diagnostics$enabled)) {
        return(diagnostics)
    }
    extra <- viewer__geometry_issues(geoms, objects, diagnostics$tolerance)
    diagnostics$issues <- c(diagnostics$issues, extra)
    diagnostics$count <- length(diagnostics$issues)
    diagnostics
}
# }}}

# viewer__scene_context {{{
# Apply geometry filters once so providers and scene serialization share identical object indices.
viewer__scene_context <- function(geometry, options) {
    geoms <- copy_list(get_priv_env(geometry)$geoms())
    geoms <- align_coord_system(geoms, "world", "world", "world")
    if (length(options$show)) {
        geoms <- do.call(subset_geom, c(list(geoms = geoms), options$show))
    }
    objects <- viewer__object_metadata(geoms)
    list(geoms = geoms, objects = objects)
}
# }}}

# viewer__build_idf_view_scene {{{
# Assemble the compact payload consumed by the Three.js widget.
viewer__build_idf_view_scene <- function(
    geometry,
    options,
    results = list(),
    frame_provider = NULL,
    frame_endpoint = NULL
) {
    context <- viewer__scene_context(geometry, options)
    geoms <- context$geoms
    objects <- context$objects
    faces <- viewer__triangulate_view_faces(geoms, objects$id)

    if (nrow(faces)) {
        add_joined_cols(objects, faces, "id", "object_index")
        positions <- as.vector(t(as.matrix(faces[, c("x", "y", "z")])))
        vertex_objects <- faces$object_index
        face_objects <- vertex_objects[seq.int(
            1L,
            length(vertex_objects),
            by = 3L
        )]
    } else {
        positions <- numeric()
        face_objects <- integer()
    }
    edges <- viewer__edge_buffers(geoms, objects)

    object_list <- lapply(seq_len(nrow(objects)), function(index) {
        value <- as.list(objects[index])
        value[vapply(
            value,
            function(x) length(x) == 1L && is.na(x),
            logical(1L)
        )] <- list(NULL)
        value
    })

    list(
        scene_id = unique_id(),
        building = geoms$building,
        objects = object_list,
        geometry = list(
            positions = viewer__pack_view_float32(positions),
            face_objects = viewer__pack_view_int32(face_objects),
            edge_positions = viewer__pack_view_float32(edges$positions),
            edge_objects = viewer__pack_view_int32(edges$objects),
            point_positions = viewer__pack_view_float32(viewer__point_buffer(
                geoms
            ))
        ),
        options = options,
        results = if (is.null(frame_provider)) {
            viewer__results_payload(results, objects)
        } else {
            frame_provider$metadata(frame_endpoint)
        },
        diagnostics = viewer__geometry_diagnostics(
            geoms,
            objects,
            options$diagnostics %||% list()
        )
    )
}
# }}}

# viewer__create_idf_view_widget {{{
# Create the widget from precomputed scene buffers without exposing Three.js details to callers.
viewer__create_idf_view_widget <- function(
    scene,
    width = NULL,
    height = NULL,
    element_id = NULL
) {
    if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
        abort(
            "Package 'htmlwidgets' is required for 3D geometry viewing. Install it with install.packages('htmlwidgets')."
        )
    }
    htmlwidgets::createWidget(
        "idfviewer",
        scene,
        width = width,
        height = height,
        package = "eplusr",
        elementId = element_id,
        sizingPolicy = htmlwidgets::sizingPolicy(
            viewer.fill = TRUE,
            browser.fill = TRUE,
            viewer.padding = 0,
            browser.padding = 0,
            defaultWidth = "100%",
            defaultHeight = 640
        )
    )
}
# }}}
