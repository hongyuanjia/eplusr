# Run the actual R example in a persistent session so its live widget remains controllable.
viewer__serve_real_example <- function(root) {
    source(file.path(root, "tools/viewer/real-results.R"), local = TRUE)
    preview <- file.path(root, "output/viewer-real-results")
    dir.create(preview, recursive = TRUE, showWarnings = FALSE)
    widget$height <- "calc(100vh - 16px)"
    htmltools::save_html(
        htmltools::tagList(
            htmltools::tags$title(
                "Large office — EnergyPlus 23.1 design-day results"
            ),
            widget
        ),
        file.path(preview, "index.html"),
        libdir = "lib"
    )
    server <- httpuv::startServer(
        "127.0.0.1",
        40680L,
        list(
            call = function(request) list(status = 404L, body = "Not found"),
            staticPaths = list("/" = httpuv::staticPath(preview))
        )
    )
    on.exit(
        {
            viewer$close()
            httpuv::stopServer(server)
        },
        add = TRUE
    )
    jsonlite::write_json(
        list(
            url = "http://127.0.0.1:40680/",
            zones = zones,
            variable = "Zone Mean Air Temperature",
            frames = 24L,
            scene = widget$x$scene_id
        ),
        file.path(preview, "info.json"),
        auto_unbox = TRUE
    )
    cat("Real result preview ready: http://127.0.0.1:40680/\n")
    flush.console()
    error <- NULL
    while (!file.exists(file.path(preview, "stop"))) {
        later::run_now(0.1, all = FALSE)
        command_path <- file.path(preview, "command.json")
        if (file.exists(command_path)) {
            command <- jsonlite::read_json(command_path, simplifyVector = TRUE)
            unlink(command_path)
            error <- NULL
            # Exercise public R methods rather than injecting changes into the browser.
            tryCatch(
                {
                    if (command$action == "filter") {
                        do.call(
                            viewer$filter,
                            command[intersect(
                                names(command),
                                c("zone", "space", "surface", "type", "name")
                            )]
                        )
                    }
                    if (command$action == "select") {
                        viewer$select(zone = command$zone)
                    }
                    if (command$action == "update") {
                        do.call(
                            viewer$update,
                            c(list(command$command), command$args)
                        )
                    }
                },
                error = function(condition) {
                    error <<- conditionMessage(condition)
                }
            )
        }
        state <- viewer$state()
        jsonlite::write_json(
            list(state = state, error = error),
            file.path(preview, "status.next.json"),
            auto_unbox = TRUE,
            null = "null",
            na = "null"
        )
        file.rename(
            file.path(preview, "status.next.json"),
            file.path(preview, "status.json")
        )
    }
}

# Rscript exposes its file path, so the preview works from any checkout location.
preview_script <- grep("^--file=", commandArgs(), value = TRUE)
preview_root <- if (length(preview_script)) {
    normalizePath(file.path(
        dirname(sub("^--file=", "", preview_script[[1L]])),
        "../.."
    ))
} else {
    normalizePath(".")
}
viewer__serve_real_example(preview_root)
