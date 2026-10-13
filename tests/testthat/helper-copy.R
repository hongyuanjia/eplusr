copy_example <- function(ver = LATEST_EPLUS_VER) {
    if (!is_avail_eplus(ver)) return()

    cfg <- eplus_config(ver)

    example_name <- "5Zone_Transformer.idf"
    weather_name <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
    ddy_name <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3.ddy"

    path_example <- file.path(cfg$dir, "ExampleFiles", example_name)
    path_idf <- normalizePath(file.path(tempdir(), example_name), mustWork = FALSE)
    file.copy(path_example, path_idf, overwrite = TRUE)

    path_weather <- file.path(cfg$dir, "WeatherData", weather_name)
    path_epw <- normalizePath(file.path(tempdir(), weather_name), mustWork = FALSE)
    file.copy(path_weather, path_epw, overwrite = TRUE)

    list(idf = path_idf, epw = path_epw)
}

copy_eplus_example <- function(ver, file, dir = tempdir()) {
    path <- path_eplus_example(ver, file, strict = TRUE)
    dest <- normalizePath(file.path(dir, basename(path)), mustWork = FALSE)

    flag <- file.copy(path, dest, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)

    if (any(!flag)) {
        stop(sprintf("Failed to copy file from '%s' to '%s'.", path[!flag], dest[!flag]))
    }

    dest
}

clean_tempdir <- function() {
    unlink(list.files(tempdir(), full.names = TRUE), force = TRUE)
}

# Derive a one-day stock-SDK model instead of distributing local experiment inputs.
testrun__api_fixture <- function() {
    skip_if_not(
        identical(Sys.getenv("_EPLUSR_RUN_API_TESTS_"), "true"),
        "API integration tests are disabled"
    )
    skip_if_not_installed("rdyncall")
    root <- eplus_config("26.1")$dir
    model <- read_idf(file.path(root, "ExampleFiles", "1ZoneUncontrolled.idf"))
    model$set(
        RunPeriod := list(
            begin_month = 1,
            begin_day_of_month = 1,
            end_month = 1,
            end_day_of_month = 1
        )
    )
    directory <- tempfile("eplusr-api-job-")
    dir.create(directory)
    path <- file.path(directory, "model.idf")
    model$save(path)
    list(
        model = path,
        weather = file.path(
            root,
            "WeatherData",
            "USA_CO_Golden-NREL.724666_TMY3.epw"
        ),
        directory = directory
    )
}


# Wait for the worker bootstrap handshake, failing immediately if its owner exits.
# covr traces the namespace again in each fresh worker before the backend initializer can run.
testrun__api_started <- function(ready, alive, diagnostic = NULL) {
    deadline <- Sys.time() + 120
    while (!ready() && alive() && Sys.time() < deadline) {
        Sys.sleep(0.1)
    }
    info <- "API worker bootstrap did not complete before timeout or owner exit"
    if (!ready() && !is.null(diagnostic)) {
        info <- paste(
            info,
            jsonlite::toJSON(diagnostic(), auto_unbox = TRUE, null = "null")
        )
    }
    expect_true(ready(), info = info)
}


# Model an OS cancellation failure while retaining a live process dependency.
testrun__denied_process <- function() {
    structure(
        list(
            # A denied signal must leave the process alive.
            is_alive = function() TRUE,
            # Expose the process API's failure instead of marking a job killed.
            kill = function() stop("OS cancellation denied")
        ),
        class = "process"
    )
}
