# Parse the small numeric frame query without depending on an application framework.
viewer__query <- function(query) {
    if (is.null(query) || !nzchar(query)) {
        return(list())
    }
    parts <- strsplit(query, "&", fixed = TRUE)[[1L]]
    values <- lapply(parts, function(part) {
        pair <- strsplit(part, "=", fixed = TRUE)[[1L]]
        c(
            utils::URLdecode(pair[[1L]]),
            utils::URLdecode(paste(pair[-1L], collapse = "="))
        )
    })
    stats::setNames(lapply(values, `[[`, 2L), vapply(values, `[[`, "", 1L))
}

# Return the canonical object table; browser messages never supply model metadata.
viewer__selected_objects <- function(objects, ids = character()) {
    rows <- lapply(objects, function(object) {
        lapply(object, function(value) {
            if (is.null(value)) NA_character_ else value
        })
    })
    table <- data.table::rbindlist(rows, fill = TRUE)
    if (is.null(table)) {
        table <- data.table::data.table(id = character())
    }
    ids <- as.character(unlist(ids, use.names = FALSE))
    if (!length(ids) || !nrow(table)) {
        return(table[0L])
    }
    table[as.character(table$id) %chin% ids]
}

# Run one loopback connection for a viewer, sharing state with the current R session.
viewer__session <- function(scene, provider = NULL, callback = NULL) {
    if (!requireNamespace("httpuv", quietly = TRUE)) {
        abort(
            "Package 'httpuv' is required for live viewer connections; install.packages('httpuv')."
        )
    }
    state <- new.env(parent = emptyenv())
    state$provider <- provider
    state$scene_id <- scene$scene_id
    state$objects <- scene$objects
    state$callback <- callback
    state$selection <- viewer__selected_objects(scene$objects)
    state$browser <- list()
    state$clients <- new.env(parent = emptyenv())
    state$pending <- list()
    state$latest_results <- NULL
    state$serial <- 0L
    state$acknowledged <- 0L
    state$error <- NULL
    state$closed <- FALSE
    state$server <- NULL
    token <- paste0(unique_id(), unique_id())
    route <- paste0("/", token)

    # JSON arrays retain their shape for zero, one, or many selected objects.
    encode <- function(message) {
        jsonlite::toJSON(
            message,
            auto_unbox = TRUE,
            null = "null",
            na = "null",
            digits = NA
        )
    }

    # Serve only this viewer's result endpoint; there is no application or model API.
    call <- function(req) {
        headers <- list(
            "Access-Control-Allow-Origin" = "*",
            "Cache-Control" = "no-store"
        )
        if (!identical(req$PATH_INFO, paste0(route, "/frames"))) {
            return(list(
                status = 404L,
                headers = headers,
                body = "Unknown viewer endpoint."
            ))
        }
        if (!identical(req$REQUEST_METHOD, "GET") || is.null(state$provider)) {
            return(list(
                status = 404L,
                headers = headers,
                body = "No result frame provider."
            ))
        }
        tryCatch(
            {
                query <- viewer__query(req$QUERY_STRING)
                body <- state$provider$fetch(
                    query$variable %||% 0L,
                    query$frames %||% 0L
                )
                headers[["Content-Type"]] <- "application/octet-stream"
                list(status = 200L, headers = headers, body = body)
            },
            error = function(error) {
                headers[["Content-Type"]] <- "text/plain; charset=utf-8"
                list(
                    status = error$status %||% 500L,
                    headers = headers,
                    body = conditionMessage(error)
                )
            }
        )
    }

    # Reject a stale scene and rebuild selected rows from the server-owned object table.
    receive <- function(message) {
        if (
            state$closed ||
                !is.list(message) ||
                !identical(message$scene_id, state$scene_id)
        ) {
            return(invisible(FALSE))
        }
        if (identical(message$type, "query_results")) {
            tryCatch(
                {
                    if (!is.function(state$query_results)) {
                        abort("No result source is attached.")
                    }
                    state$query_results(message$filters)
                },
                error = function(error) {
                    state$send(
                        "result_query_error",
                        list(message = conditionMessage(error))
                    )
                }
            )
            return(invisible(TRUE))
        }
        if (identical(message$type, "error")) {
            state$error <- as.character(
                message$error %||% "Viewer command failed."
            )
            return(invisible(FALSE))
        }
        if (!is.character(message$type) || length(message$type) != 1L) {
            return(invisible(FALSE))
        }
        if (!message$type %in% c("state", "selection", "ack")) {
            return(invisible(FALSE))
        }
        old <- state$selection$id
        state$selection <- viewer__selected_objects(
            state$objects,
            message$selected_ids
        )
        state$browser <- message$state %||% list()
        if (identical(message$type, "ack") && is.numeric(message$sequence)) {
            state$acknowledged <- max(
                state$acknowledged,
                as.integer(message$sequence)
            )
        }
        if (
            identical(message$type, "selection") &&
                !identical(old, state$selection$id) &&
                is.function(state$callback)
        ) {
            tryCatch(
                state$callback(data.table::copy(state$selection)),
                error = function(error) {
                    warning(
                        "Viewer selection callback failed: ",
                        conditionMessage(error),
                        call. = FALSE
                    )
                }
            )
        }
        invisible(TRUE)
    }

    # Register sockets independently so two different widgets never share a global channel.
    on_open <- function(ws) {
        if (!identical(ws$request$PATH_INFO, paste0(route, "/socket"))) {
            ws$close(1008L, "Unknown viewer connection.")
            return(invisible())
        }
        key <- unique_id()
        state$clients[[key]] <- ws
        ws$onMessage(function(binary, message) {
            if (binary || nchar(message, type = "bytes") > 1024L * 1024L) {
                return(invisible())
            }
            tryCatch(
                receive(jsonlite::fromJSON(message, simplifyVector = FALSE)),
                error = function(error) invisible(FALSE)
            )
        })
        ws$onClose(function() {
            if (exists(key, envir = state$clients, inherits = FALSE)) {
                rm(list = key, envir = state$clients)
            }
        })
        # A refreshed widget contains its original metadata. Restore the latest
        # applied query before it can keep a stale calendar or color range.
        pending_results <- any(vapply(
            state$pending,
            function(message) identical(message$command, "results"),
            logical(1L)
        ))
        if (!pending_results && !is.null(state$latest_results)) {
            ws$send(encode(state$latest_results))
        }
        # Commands issued before the browser opens must not disappear silently.
        for (message in state$pending) {
            ws$send(encode(message))
        }
        state$pending <- list()
        invisible()
    }

    port <- httpuv::randomPort(host = "127.0.0.1")
    state$server <- httpuv::startServer(
        "127.0.0.1",
        port,
        list(call = call, onWSOpen = on_open)
    )
    state$config <- list(
        scene_id = scene$scene_id,
        websocket = sprintf("ws://127.0.0.1:%d%s/socket", port, route),
        frame_endpoint = sprintf("http://127.0.0.1:%d%s/frames", port, route)
    )

    # Send a versioned command without blocking R while a browser connects or renders.
    state$send <- function(command, args = list()) {
        if (state$closed) {
            abort(
                "The live viewer is closed; render '$widget(live = TRUE)' again."
            )
        }
        state$serial <- state$serial + 1L
        state$error <- NULL
        message <- list(
            scene_id = state$scene_id,
            sequence = state$serial,
            command = command,
            args = args
        )
        # Retain only the latest result replacement, rather than replaying all
        # interaction history when a browser reloads or a second output opens.
        if (identical(command, "results")) {
            state$latest_results <- message
        }
        clients <- as.list.environment(state$clients)
        if (!length(clients)) {
            if (length(state$pending) >= 100L) {
                abort(
                    "The browser is not connected; render the widget before sending more commands."
                )
            }
            state$pending[[length(state$pending) + 1L]] <- message
        } else {
            lapply(clients, function(ws) ws$send(encode(message)))
        }
        invisible(state$serial)
    }

    # Drain already-scheduled events, then return a copy rather than mutable session state.
    state$read <- function() {
        if (!state$closed) {
            later::run_now(0, all = FALSE)
        }
        list(
            connected = length(ls(state$clients)),
            closed = state$closed,
            sent = state$serial,
            acknowledged = state$acknowledged,
            error = state$error,
            view = state$browser,
            selection = data.table::copy(state$selection)
        )
    }

    # Release only this viewer's sockets and listener, leaving other widgets running.
    state$close <- function() {
        if (state$closed) {
            return(invisible())
        }
        state$closed <- TRUE
        # The listener owns sockets that may already be released during shutdown.
        if (state$server$isRunning()) {
            httpuv::stopServer(state$server)
        }
        rm(list = ls(state$clients), envir = state$clients)
        state$pending <- list()
        invisible()
    }
    state$receive <- receive
    state
}

# Export through a private sibling file so failed captures never overwrite user output.
viewer__snapshot <- function(viewer, filename, selfcontained = TRUE, ...) {
    assert_string(filename)
    if (!has_ext(filename, c("html", "png"))) {
        abort("Three.js snapshots support '.html' and '.png' files.")
    }
    if (
        has_ext(filename, "png") &&
            !requireNamespace("webshot2", quietly = TRUE)
    ) {
        abort(
            "Package 'webshot2' is required for PNG snapshots.",
            "snapshot_unavailable"
        )
    }
    directory <- dirname(filename)
    if (!dir.exists(directory)) {
        dir.create(directory, recursive = TRUE)
    }
    suffix <- if (has_ext(filename, "png")) ".png" else ".html"
    temporary <- tempfile(".eplusr-view-", tmpdir = directory, fileext = suffix)
    html <- tempfile(fileext = ".html")
    on.exit(unlink(c(temporary, html)), add = TRUE)
    # Portable exports contain no listener URLs or live callbacks.
    widget <- viewer$widget(live = FALSE)
    tryCatch(
        withCallingHandlers(
            {
                if (has_ext(filename, "html")) {
                    htmlwidgets::saveWidget(
                        widget,
                        temporary,
                        selfcontained = selfcontained
                    )
                } else {
                    htmlwidgets::saveWidget(widget, html, selfcontained = TRUE)
                    webshot2::webshot(html, temporary, ...)
                }
            },
            warning = function(warning) {
                abort(conditionMessage(warning), "snapshot_failed")
            }
        ),
        error = function(error) {
            abort(
                paste("Viewer snapshot failed:", conditionMessage(error)),
                "snapshot_failed"
            )
        }
    )
    if (
        !file.exists(temporary) ||
            is.na(file.info(temporary)$size) ||
            file.info(temporary)$size <= 0
    ) {
        abort(
            "Viewer snapshot did not produce a nonempty file.",
            "snapshot_failed"
        )
    }
    if (has_ext(filename, "png")) {
        connection <- file(temporary, "rb")
        signature <- readBin(connection, "raw", 8L)
        close(connection)
        if (!identical(signature, as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))) {
            abort(
                "Viewer snapshot did not produce a valid PNG.",
                "snapshot_failed"
            )
        }
    }
    if (!file.rename(temporary, filename)) {
        abort("Could not replace the snapshot destination.", "snapshot_failed")
    }
    normalizePath(filename)
}
