update_weather_db <- function(eplus_src = NULL, force = FALSE, dir_assets = here::here("tools/data/weather_db")) {
    meta <- read_meta()
    if (is.null(meta$weather_db)) force <- TRUE

    if (is.null(dir_assets)) {
        dir_assets <- here::here("tools/data/weather_db")
        if (!dir.exists(dir_assets)) dir.create(dir_assets, recursive = TRUE)

        dir_assets_json <- file.path(dir_assets, "EnergyPlus")
        if (!dir.exists(dir_assets_json)) dir.create(dir_assets_json)
        geojson <- download_geojson(eplus_src, file.path(dir_assets, "EnergyPlus"))

        dir_assets_kml <- file.path(dir_assets, "OneBuilding")
        if (!dir.exists(dir_assets_kml)) dir.create(dir_assets_kml)
        kml <- download_kml(dir_assets_kml)

        country_code <- download_countrycode(dir_assets)
    } else {
        geojson <- normalizePath(file.path(dir_assets, "EnergyPlus/weather.geojson"), mustWork = TRUE)
        kml <- normalizePath(list.files(file.path(dir_assets, "OneBuilding"), "\\.kml$", full.names = TRUE), mustWork = TRUE)
        country_code <- normalizePath(file.path(dir_assets, "country_codes.csv"), mustWork = TRUE)
    }
    kml <- sort(kml)

    # current hash
    new_hash_geojson <- tools::md5sum(geojson)
    new_hash_kml <- tools::md5sum(kml)
    new_hash_country <- tools::md5sum(country_code)
    names(new_hash_geojson) <- basename(names(new_hash_geojson))
    names(new_hash_kml) <- basename(names(new_hash_kml))
    names(new_hash_country) <- basename(names(new_hash_country))

    hash <- list(geojson = as.list(new_hash_geojson), kml = as.list(new_hash_kml), country_code = as.list(new_hash_country))

    if (identical(meta$weather_db, hash) && !force) {
        message("'weather.geojson' did not change since last update. Skip")
        return(invisible())
    }

    db_geojson <- parse_weather_geojson(geojson, country_code)
    db_kml <- data.table::rbindlist(lapply(kml, parse_kml, path_codes = country_code))

    db <- data.table::rbindlist(list(db_geojson, db_kml), use.names = TRUE, fill = TRUE)

    write_meta(meta, weather_db = hash)

    db
}

update_output_vars <- function(eplus_src, force = FALSE) {
    meta <- read_meta()

    if (is.null(meta$output_vars)) force <- TRUE

    if (force) {
        ver <- eplusr:::ALL_EPLUS_VER
        output_vars <- extract_output_vars(eplus_src, ver)
    } else {
        i <- which(meta$output_vars == eplusr:::ALL_EPLUS_VER)
        if (i == length(eplusr:::ALL_EPLUS_VER)) {
            message("Output variables did not change since last update. Skip")
            return(invisible())
        }
        ver <- eplusr:::ALL_EPLUS_VER[(i + 1):length(eplusr:::ALL_EPLUS_VER)]
        output_vars <- append(eplusr:::OUTPUT_VARS, extract_output_vars(eplus_src, ver))
    }

    write_meta(meta, output_vars = eplusr:::LATEST_EPLUS_VER)

    output_vars
}

update_reportvar_rules <- function(eplus_src, force = FALSE) {
    meta <- read_meta()

    if (is.null(meta$reportvar_rules)) force <- TRUE

    if (force) {
        ver <- eplusr:::ALL_EPLUS_VER
        reportvar_rules <- extract_reportvar_rules(eplus_src, ver)
    } else {
        i <- which(meta$reportvar_rules == eplusr:::ALL_EPLUS_VER)
        if (i == length(eplusr:::ALL_EPLUS_VER)) {
            message("Report variable rules did not change since last update. Skip")
            return(invisible())
        }
        ver <- eplusr:::ALL_EPLUS_VER[(i + 1):length(eplusr:::ALL_EPLUS_VER)]

        reportvar_rules <- data.table::rbindlist(list(
            eplusr:::REPORTVAR_RULES,
            extract_reportvar_rules(eplus_src, ver)
        ))
    }

    write_meta(meta, reportvar_rules = eplusr:::LATEST_EPLUS_VER)

    reportvar_rules
}

update_internal_data <- function(eplus_src, force = FALSE) {
    "%||%" <- function(x, y) if (is.null(x)) y else x
    if (is.logical(force) && length(force) == 1L && !is.na(force)) {
        force_weather_db <- force_reportvar_rules <- force_output_vars <- force
    } else if (is.list(force) && !is.null(names(force))) {
        force_weather_db <- force$weather_db %||% FALSE
        force_reportvar_rules <- force$reportvar_rules %||% FALSE
        force_output_vars <- force$output_vars %||% FALSE
    } else {
        stop("'force' should be a length-1 logical vector or a named list")
    }

    WEATHER_DB <- update_weather_db(eplus_src, force = force_weather_db)
    REPORTVAR_RULES <- update_reportvar_rules(eplus_src, force = force_reportvar_rules)
    OUTPUT_VARS <- update_output_vars(eplus_src, force = force_output_vars)

    save <- any(!is.null(WEATHER_DB), !is.null(REPORTVAR_RULES), !is.null(OUTPUT_VARS))

    if (is.null(WEATHER_DB)) {
        WEATHER_DB <- eplusr:::WEATHER_DB
    }
    if (is.null(REPORTVAR_RULES)) {
        REPORTVAR_RULES <- eplusr:::REPORTVAR_RULES
    }
    if (is.null(OUTPUT_VARS)) {
        OUTPUT_VARS <- eplusr:::OUTPUT_VARS
    }

    if (save) {
        usethis::use_data(WEATHER_DB, REPORTVAR_RULES, OUTPUT_VARS,
            internal = TRUE, overwrite = TRUE, compress = "xz")
    } else {
        message("Nothing has been updated. Skip saving internal data.")
    }
}
