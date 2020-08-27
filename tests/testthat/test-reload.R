context("Reload")

# Reload {{{
test_that("Reload", {
    skip_on_cran()
    eplusr_option(verbose_info = FALSE)
    if (!is_avail_eplus(8.8)) install_eplus(8.8)

    # default
    expect_equal(reload(1L), 1L)

    example <- copy_example()

    idd <- use_idd(8.8)
    idf <- read_idf(example$idf)
    epw <- read_epw(example$epw)
    job <- idf$run(NULL, tempdir(), echo = FALSE)
    grp <- group_job(idf, NULL)$run(tempdir())
    par <- param_job(idf, NULL)
    par$apply_measure(function (x, y) x, 1:2)
    par$run(tempdir())

    f_idd <- tempfile(fileext = ".rds")
    f_idf <- tempfile(fileext = ".rds")
    f_epw <- tempfile(fileext = ".rds")
    f_job <- tempfile(fileext = ".rds")
    f_grp <- tempfile(fileext = ".rds")
    f_par <- tempfile(fileext = ".rds")
    saveRDS(idd, f_idd)
    saveRDS(idf, f_idf)
    saveRDS(epw, f_epw)
    saveRDS(job, f_job)
    saveRDS(grp, f_grp)
    saveRDS(par, f_par)

    idd <- readRDS(f_idd)
    idf <- readRDS(f_idf)
    epw <- readRDS(f_epw)
    job <- readRDS(f_job)
    grp <- readRDS(f_grp)
    par <- readRDS(f_par)

    expect_equal(data.table::truelength(get_priv_env(idd)$idd_env()$group), 0L)
    expect_equal(data.table::truelength(get_priv_env(idd)$idd_env()$class), 0L)
    expect_equal(data.table::truelength(get_priv_env(idd)$idd_env()$field), 0L)
    expect_equal(data.table::truelength(get_priv_env(idd)$idd_env()$reference), 0L)
    expect_equal(data.table::truelength(get_priv_env(idf)$idd_env()$group), 0L)
    expect_equal(data.table::truelength(get_priv_env(idf)$idd_env()$class), 0L)
    expect_equal(data.table::truelength(get_priv_env(idf)$idd_env()$field), 0L)
    expect_equal(data.table::truelength(get_priv_env(idf)$idd_env()$reference), 0L)
    expect_equal(data.table::truelength(get_priv_env(idf)$idf_env()$object), 0L)
    expect_equal(data.table::truelength(get_priv_env(idf)$idf_env()$value), 0L)
    expect_equal(data.table::truelength(get_priv_env(idf)$idf_env()$reference), 0L)
    expect_equal(data.table::truelength(get_priv_env(epw)$m_header$typical), 0L)
    expect_equal(data.table::truelength(get_priv_env(epw)$m_header$ground), 0L)
    expect_equal(data.table::truelength(get_priv_env(epw)$m_header$holiday$holiday), 0L)
    expect_equal(data.table::truelength(get_priv_env(epw)$m_header$period$period), 0L)
    expect_equal(data.table::truelength(get_priv_env(epw)$m_data), 0L)

    expect_silent(reload(idd))
    expect_silent(reload(idf))
    expect_silent(reload(epw))
    expect_silent(reload(job))
    expect_silent(reload(grp))
    expect_silent(reload(par))

    expect_true(data.table::truelength(get_priv_env(idd)$idd_env()$group) > 0L)
    expect_true(data.table::truelength(get_priv_env(idd)$idd_env()$class) > 0L)
    expect_true(data.table::truelength(get_priv_env(idd)$idd_env()$field) > 0L)
    expect_true(data.table::truelength(get_priv_env(idd)$idd_env()$reference) > 0L)

    expect_idf_reloaded <- function (idf) {
        expect_true(data.table::truelength(get_priv_env(idf)$idd_env()$group) > 0L)
        expect_true(data.table::truelength(get_priv_env(idf)$idd_env()$class) > 0L)
        expect_true(data.table::truelength(get_priv_env(idf)$idd_env()$field) > 0L)
        expect_true(data.table::truelength(get_priv_env(idf)$idd_env()$reference) > 0L)
        expect_true(data.table::truelength(get_priv_env(idf)$idf_env()$object) > 0L)
        expect_true(data.table::truelength(get_priv_env(idf)$idf_env()$value) > 0L)
        expect_true(data.table::truelength(get_priv_env(idf)$idf_env()$reference) > 0L)
    }

    expect_idf_reloaded(idf)
    expect_idf_reloaded(epw)
    expect_idf_reloaded(get_priv_env(job)$m_idf)
    expect_idf_reloaded(get_priv_env(par)$m_seed)
    lapply(get_priv_env(grp)$m_idfs, expect_idf_reloaded)
    lapply(get_priv_env(par)$m_idfs, expect_idf_reloaded)
    expect_true(data.table::truelength(get_priv_env(grp)$m_job) > 0L)
    expect_true(data.table::truelength(get_priv_env(par)$m_job) > 0L)

    expect_true(job$status()$successful)
    expect_true(grp$status()$successful)
    expect_true(par$status()$successful)

})
# }}}
