context("Sql methods")

test_that("Sql methods", {
    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    example <- copy_example()
    idf <- read_idf(example$idf)

    expect_is(job <- read_idf(example$idf)$run(example$epw, NULL, echo = FALSE), "EplusJob")
    expect_silent(sql <- eplus_sql(job$locate_output(".sql")))

    expect_output(sql$print())
    expect_output(str(sql))
    expect_is(format(sql), "character")

    # path
    expect_equal(sql$path(), normalizePath(file.path(tempdir(), "5Zone_Transformer.sql")))
    expect_equal(sql$path_idf(), normalizePath(file.path(tempdir(), "5Zone_Transformer.idf")))

    # can get all table names
    expect_equal(length(sql$list_table()), 44L)

    # can read table
    expect_error(sql$read_table("a"), "no such table")
    expect_is(sql$read_table("Zones"), "data.table")

    # can read report data dictionary
    expect_is(sql$report_data_dict(), "data.table")

    # can read report data
    expect_equal(nrow(sql$report_data(sql$report_data_dict())), 3840L)
    expect_equal(nrow(sql$report_data()), 3840L)
    expect_equal(nrow(sql$report_data("")), 1344L)
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

    expect_equal(nrow(sql$tabular_data()), 6662)
    expect_equal(nrow(sql$tabular_data(
        report_name = c(
            "AnnualBuildingUtilityPerformanceSummary",
            "Initialization Summary"
        ))),
        3774
    )
    expect_equal(nrow(sql$tabular_data(table_name = "Site and Source Energy")), 12)
    expect_equal(nrow(sql$tabular_data(column_name = "Total Energy")), 4)
    expect_equal(nrow(sql$tabular_data(row_name = "Total Site Energy")), 3)
    # can convert to wide table
    expect_silent(tab <- sql$tabular_data(row_name = "Total Site Energy", wide = TRUE, case = NULL))
    expect_equal(names(tab), "AnnualBuildingUtilityPerformanceSummary.Entire Facility.Site and Source Energy")
    expect_equivalent(
        read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles/1ZoneUncontrolled.idf"))$
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
    expect_equivalent(tab[[1L]][, lapply(.SD, class)],
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

    skip_on_os("mac")
    # can get path
    expect_equal(sql$path(), job$locate_output(".sql"))
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
    idf$add(RunPeriod = list("Long", 1, 1, 12, 31), RunPeriod = list("Short", 7, 1, 8, 15))

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

test_that("CSV extraction", {
    skip_on_cran()
    # create a group job with multiple time resolution results {{{
    example <- copy_example(8.8)
    idf <- read_idf(example$idf)
    job <- idf$run(NULL, echo = FALSE)
    # remove original run periods
    idf$RunPeriod <- NULL
    # define new run periods
    idf$add(RunPeriod = list("Long",
        begin_month = 1, begin_day_of_month = 1,
        end_month = 12, end_day_of_month = 31
    ))
    idf$add(RunPeriod = list("Short",
        begin_month = 6, begin_day_of_month = 1,
        end_month = 12, end_day_of_month = 31
    ))

    # add new output variables to cover all possible report frequency
    idf$`Output:Variable` <- NULL
    idf$`Output:Meter:MeterFileOnly` <- NULL

    # according to ReadVarsESO.f90:
    # 1. if detailed/timestep/hourly exists, all Date/Time will be formatted as
    #    " mm/dd  HH:MM:OO"
    # 2. if no detailed/timestep/hourly exists but daily exists, all Date/Time
    #    will be formatted as " mm/dd"
    # 3. if no detailed/timestep/hourly/daily exists but monthly exists, all
    #    Date/Time will be formatted as "January..." (no leading space)
    # 4. if no detailed/timestep/hourly/daily/monthly exists but
    #    annual/environment/runperiod exists, all Date/Time will be formatted as
    #    "simdays=X" (no leading space)
    all_freq <- list(
        "Detailed"         = "Detailed",             # 01
        "TimeStep"         = "TimeStep",             # 02
        "Hourly"           = "Hourly",               # 03
        "Daily"            = "Daily",                # 04
        "Monthly"          = "Monthly",              # 05
        "Environment"      = "Environment",          # 06
        "Annual"           = "Annual",               # 07
        "HourlyAndDaily"   = c("Hourly", "Daily"),   # 08
        "DailyAndMonthly"  = c("Daily", "Monthly"),  # 09
        "MonthlyAndAnnual" = c("Monthly", "Annual"), # 10
        "DailyOrMonthly"   = c("Daily", "Monthly"),  # 11
        "MonthlyOrAnnual"  = c("Monthly", "Annual")  # 12
    )

    create_tmp_idf <- function (idf, name, freq, or = FALSE) {
        tmp <- idf$clone()
        rdd <- job$read_rdd()[reported_time_step == "HVAC"]
        mdd <- job$read_mdd()
        if (!or) {
            rdd <- rdd[seq_along(freq)][, reporting_frequency := freq]
            mdd <- mdd[seq_along(freq)][, reporting_frequency := freq]
        } else {
            rdd <- rdd[1][, reporting_frequency := freq[1]]
            mdd <- mdd[2][, reporting_frequency := freq[2]]
        }
        tmp$load(rdd_to_load(rdd))
        tmp$load(mdd_to_load(mdd, class = "Output:Meter:MeterFileOnly"))
        tmp$load(mdd_to_load(mdd, class = "Output:Meter:Cumulative:MeterFileOnly"))
        tmp$save(tempfile(fileext = ".idf"), overwrite = TRUE)
    }

    path <- vcapply(names(all_freq), function (x) create_tmp_idf(idf, x, all_freq[[x]], grepl("Or", x)))
    names(path) <- names(all_freq)

    grp <- group_job(path, example$epw)
    grp$run(echo = FALSE)
    # }}}

    # can give exact column names as CSV file
    for (i in seq_along(path)) {
        expect_equal(
            sort(names(fread(grp$locate_output(i, ".csv"), nrows = 0))[-1]),
            sort(add_csv_variable(grp$report_data_dict(i))$Variable)
        )
    }

    expect_is(path_sqls <- epgroup_sql_path(grp, get_priv_env(grp), NULL), "character")
    expect_is(path_csvs <- epgroup_csv_path(grp, get_priv_env(grp), NULL), "list")

    # wide {{{
    expect_is(d1 <- get_sql_report_data(path_sqls[1], path_csvs[[1]], wide = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[1], wide = TRUE), "data.table")
    expect_equivalent(d1[, -"Date/Time"], d2[, -"Date/Time"])

    expect_is(d1 <- get_sql_report_data(path_sqls[2], path_csvs[[2]], wide = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[2], wide = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[3], path_csvs[[3]], wide = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[3], wide = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[4], path_csvs[[4]], wide = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[4], wide = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[5], path_csvs[[5]], wide = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[5], wide = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[6], path_csvs[[6]], wide = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[6], wide = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[7], path_csvs[[7]], wide = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[7], wide = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[8], path_csvs[[8]], wide = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[8], wide = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[9], path_csvs[[9]], wide = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[9], wide = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[10], path_csvs[[10]], wide = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[10], wide = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[11], path_csvs[[11]], wide = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[11], wide = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[12], path_csvs[[12]], wide = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[12], wide = TRUE), "data.table")
    expect_equivalent(d1, d2)
    # }}}

    # wide & all {{{
    expect_is(d1 <- get_sql_report_data(path_sqls[1], path_csvs[[1]], wide = TRUE, all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[1], wide = TRUE, all = TRUE), "data.table")
    expect_equivalent(d1[, -"Date/Time"], d2[, -"Date/Time"])

    expect_is(d1 <- get_sql_report_data(path_sqls[2], path_csvs[[2]], wide = TRUE, all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[2], wide = TRUE, all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[3], path_csvs[[3]], wide = TRUE, all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[3], wide = TRUE, all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[4], path_csvs[[4]], wide = TRUE, all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[4], wide = TRUE, all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[5], path_csvs[[5]], wide = TRUE, all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[5], wide = TRUE, all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[6], path_csvs[[6]], wide = TRUE, all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[6], wide = TRUE, all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[7], path_csvs[[7]], wide = TRUE, all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[7], wide = TRUE, all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[8], path_csvs[[8]], wide = TRUE, all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[8], wide = TRUE, all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[9], path_csvs[[9]], wide = TRUE, all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[9], wide = TRUE, all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[10], path_csvs[[10]], wide = TRUE, all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[10], wide = TRUE, all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[11], path_csvs[[11]], wide = TRUE, all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[11], wide = TRUE, all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[12], path_csvs[[12]], wide = TRUE, all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[12], wide = TRUE, all = TRUE), "data.table")
    expect_equivalent(d1, d2)
    # }}}

    # non-wide {{{
    expect_is(d1 <- get_sql_report_data(path_sqls[1], path_csvs[[1]]), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[1]), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[2], path_csvs[[2]]), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[2]), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[3], path_csvs[[3]]), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[3]), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[4], path_csvs[[4]]), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[4]), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[5], path_csvs[[5]]), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[5]), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[6], path_csvs[[6]]), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[6]), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[7], path_csvs[[7]]), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[7]), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[8], path_csvs[[8]]), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[8]), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[9], path_csvs[[9]]), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[9]), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[10], path_csvs[[10]]), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[10]), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[11], path_csvs[[11]]), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[11]), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[12], path_csvs[[12]]), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[12]), "data.table")
    expect_equivalent(d1, d2)
    # }}}

    # non-wide & all {{{
    expect_is(d1 <- get_sql_report_data(path_sqls[1], path_csvs[[1]], all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[1], all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[2], path_csvs[[2]], all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[2], all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[3], path_csvs[[3]], all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[3], all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[4], path_csvs[[4]], all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[4], all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[5], path_csvs[[5]], all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[5], all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[6], path_csvs[[6]], all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[6], all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[7], path_csvs[[7]], all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[7], all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[8], path_csvs[[8]], all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[8], all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[9], path_csvs[[9]], all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[9], all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[10], path_csvs[[10]], all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[10], all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[11], path_csvs[[11]], all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[11], all = TRUE), "data.table")
    expect_equivalent(d1, d2)

    expect_is(d1 <- get_sql_report_data(path_sqls[12], path_csvs[[12]], all = TRUE), "data.table")
    expect_is(d2 <- get_sql_report_data(path_sqls[12], all = TRUE), "data.table")
    expect_equivalent(d1, d2)
    # }}}
})
