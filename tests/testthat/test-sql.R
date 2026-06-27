test_that("SQL literals are quoted for DuckDB", {
    expect_equal(.sql_sep(c("Annual", "O'Brien")), "'annual','o''brien'")
    expect_equal(.sql_sep(c("Annual", "O'Brien"), ignore_case = FALSE), "'Annual','O''Brien'")
    expect_equal(.sql_make(c("Annual", "O'Brien"), sql_col = "report_name"),
        "lower(report_name) IN ('annual','o''brien')"
    )
})

test_that("DuckDB SQLite extension is explicit", {
    calls <- character()

    expect_true(with_mocked_bindings(
        install_duckdb_sqlite(),
        sql_exec = function(sql, conn) {
            calls <<- c(calls, sql)
            0
        },
        .package = "duckdb"
    ))
    expect_equal(calls, "INSTALL sqlite")
})

test_that("DuckDB SQLite extension install failure includes guidance", {
    expect_error(
        with_mocked_bindings(
            install_duckdb_sqlite(),
            sql_exec = function(sql, conn) stop("offline install", call. = FALSE),
            .package = "duckdb"
        ),
        "network, proxy, or offline",
        class = "eplusr_error_duckdb_sqlite_install"
    )
})

test_that("DuckDB SQLite load failure does not auto-install", {
    path <- tempfile(fileext = ".sql")
    file.create(path)
    calls <- character()

    expect_error(
        with_mocked_bindings(
            conn_sql(path),
            sql_exec = function(sql, conn) {
                calls <<- c(calls, sql)
                stop("load failed", call. = FALSE)
            },
            .package = "duckdb"
        ),
        "install_duckdb_sqlite",
        class = "eplusr_error_duckdb_sqlite_extension"
    )
    expect_equal(calls, "LOAD sqlite")
})

test_that("DuckDB SQLite attach failure reports attach context", {
    path <- tempfile(fileext = ".sql")
    file.create(path)
    calls <- character()

    expect_error(
        with_mocked_bindings(
            conn_sql(path),
            sql_exec = function(sql, conn) {
                sql <- as.character(sql)
                calls <<- c(calls, sql)
                if (grepl("^ATTACH ", sql)) stop("attach failed", call. = FALSE)
                0
            },
            .package = "duckdb"
        ),
        "Failed to attach EnergyPlus SQLite output",
        class = "eplusr_error_duckdb_sqlite_attach"
    )
    expect_equal(length(calls), 2L)
    expect_equal(calls[[1L]], "LOAD sqlite")
    expect_match(calls[[2L]], "^ATTACH '.+' AS eplus_sql \\(TYPE sqlite\\)$")
})

test_that("DuckDB SQLite connection attaches output database", {
    path <- tempfile(fileext = ".sql")
    file.create(path)
    calls <- character()

    con <- with_mocked_bindings(
        conn_sql(path),
        sql_exec = function(sql, conn) {
            calls <<- c(calls, as.character(sql))
            0
        },
        .package = "duckdb"
    )
    on.exit(duckdb::dbDisconnect(con), add = TRUE)

    expect_true(duckdb::dbIsValid(con))
    expect_equal(calls[[1L]], "LOAD sqlite")
    expect_match(calls[[2L]], "^ATTACH '.+' AS eplus_sql \\(TYPE sqlite\\)$")
    expect_equal(calls[[3L]], "USE eplus_sql")
})

test_that("DuckDB SQL query failures are preserved", {
    path <- tempfile(fileext = ".sql")
    file.create(path)

    expect_error(
        with_mocked_bindings(
            read_sql_table(path, "Zones"),
            sql_exec = function(sql, conn) 0,
            sql_query = function(sql, conn) stop("query failed", call. = FALSE),
            .package = "duckdb"
        ),
        "query failed"
    )
})

test_that("Sql methods", {
    skip_on_cran()

    example <- copy_example()
    idf <- read_idf(example$idf)

    expect_s3_class(job <- idf$run(example$epw, NULL, echo = FALSE), "EplusJob")
    expect_silent(sql <- eplus_sql(job$locate_output(".sql")))

    expect_output(sql$print())
    expect_output(str(sql))
    expect_type(format(sql), "character")

    # path
    expect_equal(sql$path(), normalizePath(file.path(tempdir(), "5Zone_Transformer.sql")))
    expect_equal(sql$path_idf(), normalizePath(file.path(tempdir(), "5Zone_Transformer.idf")))

    # can get all table names
    expect_equal(length(sql$list_table()), 44L)

    # can read table
    expect_error(sql$read_table("a"), "no such table")
    expect_s3_class(sql$read_table("Zones"), "data.table")

    # can read report data dictionary
    expect_s3_class(sql$report_data_dict(), "data.table")

    # can read report data
    expect_equal(nrow(sql$report_data(sql$report_data_dict())), 3840L)
    expect_equal(nrow(sql$report_data()), 3840L)
    expect_equal(nrow(sql$report_data(name = sql$report_data_dict()[is.na(key_value), name])), 1344L)
    expect_equal(nrow(sql$report_data(
        "TRANSFORMER 1", "Transformer Load Loss Rate")),
        192L
    )
    expect_equal(nrow(sql$report_data(
        "TRANSFORMER 1", "Transformer Load Loss Rate")),
        192L
    )
    expect_equal(year(sql$report_data(
        "TRANSFORMER 1", "Transformer Load Loss Rate", year = 2010)$datetime),
        rep(2010, 192)
    )
    expect_equal(lubridate::tz(sql$report_data(tz = "Asia/Shanghai")$datetime),
        "Asia/Shanghai"
    )
    expect_equal(sql$report_data(case = "test")$case, rep("test", 3840))
    expect_equal(names(sql$report_data(all = TRUE)),
        c("case", "datetime", "month", "day", "hour", "minute", "dst", "interval",
          "simulation_days", "day_type", "environment_name",
          "environment_period_index", "is_meter", "type", "index_group",
          "timestep_type", "key_value", "name", "reporting_frequency",
          "schedule_name", "units", "value"
        )
    )
    expect_error(sql$report_data(period = seq(1, 5, 1)), class = "eplusr_error")
    expect_equal(nrow(sql$report_data(period = seq(
        lubridate::ymd("2019-01-14"), lubridate::ymd("2019-01-15"), "1 day")
    )), 1920)
    expect_equal(nrow(sql$report_data(period = seq(
        lubridate::ymd_hms("2019-01-14 0:0:0"), lubridate::ymd_hms("2019-01-15 0:0:0"), "15 min")
    )), 1900)
    expect_equal(nrow(sql$report_data(month = 1)), 1920)
    expect_equal(nrow(sql$report_data(month = 1, day = 14:15)), 1920)
    expect_equal(nrow(sql$report_data(month = 1, hour = 1)), 80)
    expect_equal(nrow(sql$report_data(minute = 0)), 960)
    expect_equal(nrow(sql$report_data(interval = 15)), 3840)
    expect_equal(nrow(sql$report_data(simulation_days = 1)), 3840)
    expect_error(sql$report_data(day_type = "what"))
    expect_equal(nrow(sql$report_data(day_type = "weekday")), 3840)
    expect_equal(nrow(sql$report_data(day_type = "weekend")), 0)
    expect_equal(nrow(sql$report_data(day_type = "customday")), 0)
    expect_equal(nrow(sql$report_data(day_type = "specialday")), 0)
    expect_equal(nrow(sql$report_data(day_type = "tuesday")), 3840)
    expect_equal(nrow(sql$report_data(day_type = "normalday")), 3840)
    expect_equal(nrow(sql$report_data(day_type = "designday")), 0)
    expect_equal(nrow(sql$report_data(environment_name = "WINTERDAY")), 1920)

    expect_equal(nrow(sql$tabular_data()), 12077L)
    expect_equal(nrow(sql$tabular_data(
        report_name = c(
            "AnnualBuildingUtilityPerformanceSummary",
            "Initialization Summary"
        ))),
        857L
    )
    expect_equal(nrow(sql$tabular_data(table_name = "Site and Source Energy")), 12)
    expect_equal(nrow(sql$tabular_data(column_name = "Total Energy")), 4)
    expect_equal(nrow(sql$tabular_data(row_name = "Total Site Energy")), 3)
    # can convert to wide table
    expect_silent(tab <- sql$tabular_data(row_name = "Total Site Energy", wide = TRUE, case = NULL))
    expect_equal(names(tab), "AnnualBuildingUtilityPerformanceSummary.Entire Facility.Site and Source Energy")

    expect_equal(
        ignore_attr = TRUE,
        read_idf(path_eplus_example(LATEST_EPLUS_VER, "1ZoneUncontrolled.idf"))$
            run(NULL, tempdir(), echo = FALSE)$
            tabular_data(table_name = "Site and Source Energy", wide = TRUE)[[1]][
            , lapply(.SD, class)],
        data.table(
            case = "character",
            report_name = "character",
            report_for = "character",
            table_name = "character",
            row_name = "character",
            `Total Energy [GJ]` = "numeric",
            `Energy Per Total Building Area [MJ/m2]` = "numeric",
            `Energy Per Conditioned Building Area [MJ/m2]` = "numeric"
        )
    )
    expect_equal(
        ignore_attr = TRUE,
        tab[[1L]][, lapply(.SD, class)],
        data.table(
            report_name = "character",
            report_for = "character",
            table_name = "character",
            row_name = "character",
            `Total Energy [GJ]` = "numeric",
            `Energy Per Total Building Area [MJ/m2]` = "numeric",
            `Energy Per Conditioned Building Area [MJ/m2]` = "numeric"
        )
    )

    # can get path
    if (!is_macos()) expect_equal(sql$path(), job$locate_output(".sql"))
    clean_wd(example$idf)
    unlink(c(example$idf, example$epw))
})

test_that("Data extraction", {
    skip_on_cran()

    # can handle multiple time resolution
    example <- copy_example()
    all_freq <- c("Detailed", "Timestep", "Hourly", "Daily", "Monthly",
          "RunPeriod", "Environment", "Annual"
    )
    idf <- read_idf(example$idf)
    job <- idf$run(NULL, echo = FALSE)
    # remove original run periods
    idf$RunPeriod <- NULL
    # define new run periods
    idf$add(RunPeriod = list("Long", 1, 1, NULL, 12, 31), RunPeriod = list("Short", 7, 1, NULL, 8, 15))

    # add new output variables to cover all possible report frequency
    idf$`Output:Variable` <- NULL
    idf$`Output:Meter:MeterFileOnly` <- NULL
    rdd <- job$read_rdd()[seq_along(all_freq)][, reporting_frequency := all_freq]
    mdd <- job$read_mdd()[seq_along(all_freq)][, reporting_frequency := all_freq]
    idf$load(rdd_to_load(rdd))
    idf$load(mdd_to_load(mdd))

    # save as temp file
    idf$save(tempfile(fileext = ".idf"))
    # run with weather file
    job <- idf$run(example$epw, echo = FALSE)

    res1 <- job$report_data(wide = TRUE)
    res2 <- job$report_data(all = TRUE, wide = TRUE)
    expect_equal(nrow(res1), nrow(res2))

    jobs <- lapply(all_freq, function (freq) {
        idf$`Output:Variable`<- NULL

        dt <- idf$to_table(class = "Output:Meter")
        dt[index == 2L, value := freq]
        idf$update(dt)

        idf$save(tempfile(fileext = ".idf"))

        idf$run(NULL, echo = FALSE)
    })

    expect_silent(data_all <- lapply(jobs, function (job) get_sql_report_data(job$locate_output(".sql"), all = TRUE)))
    expect_silent(data_wide <- lapply(jobs, function (job) get_sql_report_data(job$locate_output(".sql"), all = TRUE, wide = TRUE)))

    clean_wd(example$idf)
    unlink(c(example$idf, example$epw))
})

# vim: set fdm=marker:
