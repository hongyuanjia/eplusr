copy_example <- function () {
    stopifnot(is_avail_eplus(8.8))
    cfg <- eplus_config(8.8)

    example_name <- "5Zone_Transformer.idf"
    weather_name <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
    ddy_name <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3.ddy"

    path_example <- file.path(cfg$dir, "ExampleFiles", example_name)
    path_idf <- file.path(getwd(), example_name)
    file.copy(path_example, path_idf, overwrite = TRUE)

    path_weather <- file.path(cfg$dir, "WeatherData", weather_name)
    path_epw <- file.path(getwd(), weather_name)
    file.copy(path_weather, path_epw, overwrite = TRUE)

    list(idf = path_idf, epw = path_epw)
}
