# download_geojson {{{
download_geojson <- function (dir = tempdir()) {
    url_json <- "https://raw.githubusercontent.com/NREL/EnergyPlus/develop/weather/master.geojson"

    path_json <- file.path(tempdir(), "weather.geojson")
    download.file(url_json, path_json, method = "libcurl", mode = "wb")
    normalizePath(path_json)
}
# }}}

# read_countrycode {{{
read_countrycode <- function () {
    u <- "https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3"
    div <- rvest::html_nodes(xml2::read_html(u), "div.div-col ul li")
    data.table::data.table(
        iso_codes = rvest::html_text(rvest::html_nodes(div, "span")),
        country = rvest::html_text(rvest::html_nodes(div, "a"))
    )
}
# }}}

# parse_weather_geojson {{{
parse_weather_geojson <- function () {
    # download the geojson file from EnergyPlus GitHub repo
    path_json <- download_geojson()

    json <- jsonlite::fromJSON(path_json)
    p <- json$features$properties
    data.table::setDT(p)
    g <- json$features$geometry
    data.table::setDT(g)
    m <- cbind(p, g)

    m[, `:=`(longitude = coordinates[[1L]][1L], latitude = coordinates[[1L]][2L]), by = seq.int(nrow(m))]
    m[, coordinates := NULL]

    regex_url <- "(?<=\\<a href=).*(epw|ddy|stat)"
    m[, `:=`(
        epw_url = stringr::str_extract(epw, regex_url),
        ddy_url = stringr::str_extract(ddy, regex_url)
    )]

    matched <- stringr::str_match(m$dir, "weather-location/(.+_wmo_region_\\d)/([A-Z]{3})/(.*?)/[A-Z]{3}_(.*?)(?:\\.(\\d+?))*_([A-Za-z0-9]+?)>")[, -1]
    data.table::set(m, NULL, c("wmo_region", "country_code", "state_province", "location", "wmo_number", "source_type"),
        data.table::as.data.table(matched)
    )

    # for California climate zone
    m[stringr::str_detect(title, "CZ\\d{2}RV2"),
        `:=`(country_code = "USA",
             state_province = "California",
             location = paste0("California Climate Zone ", stringr::str_extract(title, "(?<=CZ)\\d{2}"))
        )
    ]

    # for other non recognized
    m[is.na(location), location := title]

    # get country codes
    codes <- read_countrycode()

    m <- codes[m, on = list(iso_codes = country_code)]

    m[country == "China",
        `:=`(
          state_province = stringr::str_extract(location, "[A-Za-z]+?(?=\\.)"),
          location = stringr::str_extract(location, "(?<=\\.).*$")
        )
    ]

    m[, list(title, location, state_province, country, wmo_region, wmo_number,
        source_type, longitude, latitude, epw_url, ddy_url)]
}
# }}}

weather_db <- parse_weather_geojson()

usethis::use_data(weather_db, internal = TRUE, overwrite = TRUE, compress = "xz")
