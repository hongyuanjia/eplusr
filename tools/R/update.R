update_weather_db <- function(eplus_src = NULL, force = FALSE) {
    meta <- read_meta()
    if (is.null(meta$weather_db)) force <- TRUE

    geojson <- download_geojson(eplus_src)

    # current hash
    new_hash <- unname(tools::md5sum(geojson))

    if (identical(meta$weather_db, new_hash) && !force) {
        message("'weather.geojson' did not change since last update. Skip")
        return(invisible())
    }

    db <- parse_weather_geojson(geojson, download_countrycode())

    write_meta(meta, weather_db = new_hash)

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
    WEATHER_DB <- update_weather_db(eplus_src, force = force)
    REPORTVAR_RULES <- update_reportvar_rules(eplus_src, force = force)
    OUTPUT_VARS <- update_output_vars(eplus_src, force = force)

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
