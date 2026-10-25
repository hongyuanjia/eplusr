# Return the report_data selectors that affect numerical or temporal results.
viewer__report_filter_names <- function() {
    c(
        "key_value",
        "name",
        "year",
        "tz",
        "period",
        "month",
        "day",
        "hour",
        "minute",
        "interval",
        "simulation_days",
        "day_type",
        "environment_name",
        "reporting_frequency"
    )
}

# Validate a browser request as data; never evaluate browser expressions or SQL fragments.
viewer__browser_result_filters <- function(filters) {
    if (
        !is.list(filters) ||
            is.null(names(filters)) ||
            any(!names(filters) %chin% viewer__report_filter_names())
    ) {
        abort("Unknown result query parameter.")
    }
    numeric_fields <- c(
        "year",
        "month",
        "day",
        "hour",
        "minute",
        "interval",
        "simulation_days"
    )
    for (field in names(filters)) {
        value <- unlist(filters[[field]], use.names = FALSE)
        if (!length(value)) {
            filters[field] <- list(NULL)
        } else if (field %chin% numeric_fields) {
            if (!is.numeric(value)) {
                abort(paste("Invalid numeric filter:", field))
            }
            filters[[field]] <- value
        } else if (field == "period") {
            assert_character(value, any.missing = FALSE)
            timezone <- as.character(filters$tz %||% "UTC")
            if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", value))) {
                filters[[field]] <- as.Date(value)
            } else {
                filters[[field]] <- as.POSIXct(
                    gsub("T", " ", value, fixed = TRUE),
                    tz = timezone,
                    tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M")
                )
            }
            if (anyNA(filters[[field]])) abort("Invalid dates in 'period'.")
        } else {
            assert_character(value, any.missing = FALSE)
            filters[[field]] <- value
        }
    }
    filters
}

# Apply the same environment/calendar/day-type helpers as EplusSql$report_data().
viewer__sql_filtered_times <- function(path, filters) {
    env <- subset_sql_environment_periods(
        read_sql_table(path, "EnvironmentPeriods"),
        filters$environment_name
    )
    times <- read_sql_table(path, "Time")[
        env,
        on = "environment_period_index",
        nomatch = NULL
    ]
    if (!nrow(times)) {
        abort("No result times match the selected environment.")
    }
    times <- complete_sql_time(times)
    fields <- intersect(
        names(filters),
        c(
            "year",
            "tz",
            "period",
            "month",
            "day",
            "hour",
            "minute",
            "interval",
            "simulation_days",
            "day_type"
        )
    )
    # NULL means the report_data default (UTC), not the machine's local time zone.
    calendar_filters <- filters[fields]
    calendar_filters$tz <- calendar_filters$tz %||% "UTC"
    times <- do.call(subset_sql_time, c(list(time = times), calendar_filters))
    if ("warmup_flag" %chin% names(times)) {
        times <- times[is.na(times$warmup_flag) | times$warmup_flag == 0L]
    }
    if (!nrow(times)) {
        abort("No result times match the selected filters.")
    }
    times
}

# Build a catalog from the original source, independent of the currently applied query.
viewer__result_catalog <- function(source, objects) {
    resolved <- viewer__sql_source(source)
    dict <- copy(resolved$dictionary)
    source_variables <- sort(unique(dict$name))
    spatial_keys <- unique(tolower(unlist(
        objects[,
            intersect(names(objects), c("name", "zone_name", "space_name")),
            with = FALSE
        ],
        use.names = FALSE
    )))
    # Blank Space metadata must not admit whole-building meters with empty result keys.
    spatial_keys <- spatial_keys[
        !is.na(spatial_keys) & nzchar(trimws(spatial_keys))
    ]
    dict <- dict[tolower(key_value) %chin% spatial_keys]
    if (!nrow(dict)) {
        abort(
            "No source variables match this model's Surface, Zone or Space names."
        )
    }
    # Catalog relationships come from recorded non-warmup rows, not dictionary promises.
    # Only integer IDs from the trusted source enter this fixed-schema SQL query.
    indices <- unique(as.integer(dict$report_data_dictionary_index))
    indices <- indices[!is.na(indices)]
    available <- get_sql_query(
        resolved$path,
        sprintf(
            paste(
                "SELECT DISTINCT rd.ReportDataDictionaryIndex AS report_data_dictionary_index,",
                "ep.EnvironmentName AS environment_name, t.DayType AS day_type",
                "FROM ReportData rd JOIN Time t ON t.TimeIndex = rd.TimeIndex",
                "JOIN EnvironmentPeriods ep ON ep.EnvironmentPeriodIndex = t.EnvironmentPeriodIndex",
                "WHERE coalesce(t.WarmupFlag, 0) = 0",
                "AND rd.ReportDataDictionaryIndex IN (%s)"
            ),
            paste(indices, collapse = ", ")
        )
    )
    available <- unique(dict[
        available,
        on = "report_data_dictionary_index",
        nomatch = NULL
    ][,
        c(
            "name",
            "environment_name",
            "reporting_frequency",
            "key_value",
            "day_type"
        ),
        with = FALSE
    ])
    if (!nrow(available)) {
        abort("No recorded source results match this model.")
    }
    setorderv(available, names(available))
    list(
        variables = sort(unique(available$name)),
        source_variable_count = length(source_variables),
        unmapped_variables = setdiff(source_variables, available$name),
        frequencies = sort(unique(available$reporting_frequency)),
        environments = sort(unique(available$environment_name)),
        keys = sort(unique(available$key_value)),
        day_types = sort(unique(stats::na.omit(available$day_type))),
        combinations = lapply(seq_len(nrow(available)), function(index) {
            as.list(available[index])
        })
    )
}

# Attach query controls to numerical payloads without exposing paths or SQL to the browser.
viewer__result_query_payload <- function(
    results,
    source,
    filters,
    objects,
    live
) {
    if (is.null(source) || is.null(results)) {
        return(results)
    }
    serializable <- filters
    # Reflect aliases such as weekend as the canonical source day types in native lists.
    if (!is.null(serializable$day_type)) {
        serializable$day_type <- match_daytype(serializable$day_type)
    }
    if (!is.null(serializable$period)) {
        serializable$period <- as.character(serializable$period)
    }
    # A dictionary data frame has pair-wise semantics; retain it until the user changes keys/name.
    if (is.data.frame(serializable$key_value)) {
        serializable$key_value <- NULL
    }
    results$query <- list(
        enabled = live,
        paired_keys = is.data.frame(filters$key_value),
        catalog = viewer__result_catalog(source, objects),
        filters = serializable
    )
    results
}
