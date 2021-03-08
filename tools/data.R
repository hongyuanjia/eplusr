# download_geojson {{{
# download weather.geojson from EnergyPlus GitHub repo
download_geojson <- function (dir = tempdir()) {
    url_json <- "https://raw.githubusercontent.com/NREL/EnergyPlus/develop/weather/master.geojson"

    path_json <- file.path(tempdir(), "weather.geojson")
    download.file(url_json, path_json, method = "libcurl", mode = "wb")
    normalizePath(path_json)
}
# }}}

# download_countrycode {{{
# read ISO 3166-1 alpha-3 country codes from Wikipedia
download_countrycode <- function () {
    u <- "https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3"
    div <- rvest::html_nodes(xml2::read_html(u), "div.plainlist ul li")

    vapply(rvest::html_nodes(div, "span"), function (node) paste0(rvest::html_text(node)), character(1L))

    codes <- data.table::data.table(
        iso_codes = rvest::html_text(rvest::html_nodes(div, "span")),
        country = vapply(div,
            function (node) {
                text <- rvest::html_text(rvest::html_nodes(node, "a"))
                paste0(text, collapse = ", ")
            },
            character(1L)
        )
    )

    path <- normalizePath(file.path(tempdir(), "country_codes.csv"), mustWork = FALSE)
    fwrite(codes, path)
    path
}
# }}}

# parse_weather_geojson {{{
# parse weather.geojson file
parse_weather_geojson <- function (path_json, path_codes) {
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

    fwrite(m, file.path(tempdir(), "weather_db.csv"))
    m
}
# }}}

# extract_reportvar_rules {{{
# extract report variable transition rules from latested IDFVersionUpdater
# folder
extract_reportvar_rules <- function () {
    dir <- file.path(eplus_config(max(avail_eplus()))$dir, "PreProcess/IDFVersionUpdater")

    re <- "Report Variables (\\d-\\d)-\\d(?:-\\d+){0,1} to (\\d-\\d)-\\d(?:-\\d+){0,1}"
    paths <- list.files(dir, re, full.names = TRUE)

    # extract versions
    m <- stringi::stri_match_first_regex(basename(paths), re)

    # store meta data
    report_vars <- data.table::data.table(path = paths,
        from = as.double(as.character(standardize_ver(m[, 2L])[, 1L:2L])),
        to = as.double(as.character(standardize_ver(m[, 3L])[, 1L:2L]))
    )

    # read tables
    report_vars[, dt := lapply(paths, data.table::fread, skip = 2, fill = TRUE, sep = ",",
        col.names = c("old", "new", "special"), select = 1:3
    )]

    # add version info and comine into one
    report_vars <- rbindlist(mapply(
        function (from, to, dt) {
            # add from and to
            dt <- data.table::set(data.table::copy(dt), NULL, c("from", "to"), list(from, to))
            data.table::setcolorder(dt, c("from", "to"))
        },
        report_vars$from, report_vars$to, report_vars$dt, SIMPLIFY = FALSE
    ))

    # remove redundant lines
    report_vars <- report_vars[!J("old variable name"), on = "old"]

    # tag delete
    report_vars[stringi::stri_detect_fixed(new, "delete", case_insensitive = TRUE), new := NA_character_]

    # change empty special comments to NA
    report_vars[stringi::stri_isempty(stringi::stri_trim_both(special)), special := NA_character_]

    fwrite(report_vars, file.path(tempdir(), "report_vars.csv"))

    # set index
    data.table::setindexv(report_vars, c("from", "to"))

    report_vars
}
# }}}

WEATHER_DB <- parse_weather_geojson(download_geojson(), download_countrycode())

REPORTVAR_RULES <- extract_reportvar_rules()

source("extract_outputs.R")
# EnergyPlus sorce directory
dir_src <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/github_repo/EnergyPlus")
paths <- get_md_paths(dir_src, "doc_md")
OUTPUT_VARS <- data.table::rbindlist(lapply(paths, extract_outputs))
unlink("doc_md", recursive = TRUE)

usethis::use_data(WEATHER_DB, REPORTVAR_RULES, OUTPUT_VARS, internal = TRUE, overwrite = TRUE, compress = "xz")
