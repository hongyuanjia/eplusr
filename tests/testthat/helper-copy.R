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
