# download weather.geojson from EnergyPlus GitHub repo
download_geojson <- function(eplus_src = NULL, dir = tempdir()) {
    if (!is.null(eplus_src)) {
        url_json <- normalizePath(file.path(eplus_src, "weather", "master.geojson"), mustWork = TRUE)
        path_json <- file.path(dir, "weather.geojson")
        file.copy(url_json, path_json, overwrite = TRUE, copy.mode = TRUE)
    } else {
        url_json <- "https://raw.githubusercontent.com/NREL/EnergyPlus/develop/weather/master.geojson"
        path_json <- file.path(dir, "weather.geojson")
        download.file(url_json, path_json, mode = "wb")
    }

    normalizePath(path_json)
}

# download kml files from Climate.OneBuilding.Org
download_kml <- function(dir = tempdir()) {
    page <- xml2::read_html("http://climate.onebuilding.org/sources/default.html")
    kml <- xml2::xml_attr(xml2::xml_find_all(page, "//li//a"), "href")
    kml <- kml[grepl("\\.kml$", kml)]

    root <- "http://climate.onebuilding.org/sources"

    links <- file.path(root, kml)
    dest <- file.path(dir, kml)

    for (i in seq_along(links)) {
        try(download.file(links[i], dest[i], mode = "wb"), silent = TRUE)
    }

    normalizePath(dest, mustWork = FALSE)[file.exists(dest)]
}

# parse kml files
parse_kml <- function(path_kml, path_codes) {
    doc <- xml2::xml_ns_strip(xml2::read_xml(path_kml, encoding = "latin1"))

    places <- xml2::xml_find_all(doc, "//Placemark")

    # country, state_province, city
    name <- xml2::xml_text(xml2::xml_find_all(places, "name"))
    loc <- stringi::stri_match_first_regex(name, "(.+) ([A-Z a-z]+) ([A-Z]+)")[, -1L]
    colnames(loc) <- c("location", "state_province", "country_code")
    loc <- data.table::as.data.table(loc)

    # data source, wmo number, etc.
    desc <- xml2::xml_text(xml2::xml_find_all(places, "description/text()"))
    read_table <- function(cdata) {
        html <- paste0("<!doctype html>\n<meta charset=utf-8>\n", cdata)
        cells <- as.list(xml2::xml_text(xml2::xml_find_all(xml2::read_html(html), ".//tr/td")))
        if (length(cells) == 12) {
            cells <- cells[c(1:2, 4, length(cells))]
        } else {
            cells <- cells[c(1:3, length(cells))]
        }
        names(cells) <- c("title", "source_type", "wmo_number", "zip_url")
        cells$source_type <- gsub("Data Source ", "", cells$source_type)
        cells$wmo_number <- gsub("WMO ", "", cells$wmo_number)
        cells$zip_url <- gsub("URL ", "", cells$zip_url)

        cells$wmo_region <- basename(dirname(dirname(cells$zip_url)))
        cells$wmo_region <- as.integer(stringi::stri_extract_first_regex(cells$wmo_region, "\\d+"))

        cells
    }
    desc <- data.table::rbindlist(lapply(desc, read_table))

    # coordinates
    coord <- xml2::xml_text(xml2::xml_find_all(places, "Point/coordinates"))
    coord <- stringi::stri_split_fixed(coord, ",", n = 3, simplify = TRUE)
    colnames(coord) <- c("longitude", "latitude", "altitude")
    coord <- data.table::as.data.table(coord)
    for (i in seq_len(ncol(coord))) data.table::set(coord, NULL, i, as.double(coord[[i]]))

    tbl <- cbind(desc, coord, loc)
    data.table::set(tbl, NULL, "title", stri_replace_all_fixed(tbl$title, " ", "_"))
    data.table::setcolorder(tbl,
        c("country_code", "state_province", "location", "wmo_number", "source_type",
            "longitude", "latitude", "zip_url"
        )
    )

    # get country codes
    codes <- data.table::fread(path_codes)
    db <- codes[tbl, on = list(iso_codes = country_code)]
    data.table::set(db, NULL, c("iso_codes", "altitude"), NULL)
    data.table::set(db, NULL, "provider", "Climate.OneBuilding.Org")
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

    re <- "(?<=\\<a href=).*(epw|ddy|stat|zip)"
    m[, `:=`(
        epw_url = stringi::stri_extract_first_regex(epw, re),
        ddy_url = stringi::stri_extract_first_regex(ddy, re),
        stat_url = stringi::stri_extract_first_regex(stat, re),
        zip_url = stringi::stri_extract_first_regex(all, re)
    )]

    re <- "weather-location/.+_wmo_region_(\\d)/([A-Z]{3})/(.*/?)[A-Z]{3}_(.*?)(?:\\.(\\d+?))*_([A-Za-z0-9]+?)>"
    matched <- stringi::stri_match_first_regex(m$dir, re)[, -1]
    data.table::set(m, NULL, c("wmo_region", "country_code", "state_province", "location", "wmo_number", "source_type"),
        data.table::as.data.table(matched)
    )
    data.table::set(m, NULL, "wmo_region", as.integer(m$wmo_region))

    m[stringi::stri_isempty(state_province), state_province := NA_character_]
    m[stringi::stri_endswith_fixed(state_province, "/"), state_province := stringi::stri_sub(state_province, to = -2L)]

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

    m[, list(title, location, state_province, country, wmo_region, wmo_number,
        source_type, longitude, latitude, epw_url, ddy_url, stat_url, zip_url,
        provider = "EnergyPlus.net"
    )]
}
