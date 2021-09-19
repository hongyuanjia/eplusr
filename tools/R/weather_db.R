# download weather.geojson from EnergyPlus GitHub repo
download_geojson <- function(eplus_src = NULL, dir = tempdir()) {
    if (!is.null(eplus_src)) {
        path_json <- normalizePath(file.path(eplus_src, "weather", "master.geojson"))
    } else {
        url_json <- "https://raw.githubusercontent.com/NREL/EnergyPlus/develop/weather/master.geojson"
        path_json <- file.path(tempdir(), "weather.geojson")
        download.file(url_json, path_json, mode = "wb")
        path_json <- normalizePath(path_json)
    }

    path_json
}

# read ISO 3166-1 alpha-3 country codes from Wikipedia
download_countrycode <- function(dir = tempdir()) {
    u <- "https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3"

    div <- rvest::html_nodes(xml2::read_html(u), "div.plainlist ul li")

    codes <- data.table::data.table(
        iso_codes = rvest::html_text(rvest::html_nodes(div, "span")),
        country = vapply(div,
            function(node) {
                text <- rvest::html_text(rvest::html_nodes(node, "a"))
                paste0(text, collapse = ", ")
            },
            character(1L)
        )
    )

    path <- normalizePath(file.path(dir, "country_codes.csv"), mustWork = FALSE)
    data.table::fwrite(codes, path)
    path
}

# parse weather.geojson file
parse_weather_geojson <- function(path_json, path_codes) {
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
    codes <- data.table::fread(path_codes)

    m <- codes[m, on = list(iso_codes = country_code)]

    m[country == "China",
        `:=`(
          state_province = stringr::str_extract(location, "[A-Za-z]+?(?=\\.)"),
          location = stringr::str_extract(location, "(?<=\\.).*$")
        )
    ]

    m <- m[, list(title, location, state_province, country, wmo_region, wmo_number,
        source_type, longitude, latitude, epw_url, ddy_url)]

    data.table::fwrite(m, file.path(tempdir(), "weather_db.csv"))
    m
}
