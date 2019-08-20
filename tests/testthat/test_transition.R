context("Transition")

test_that("Transition Helper", {
    eplusr_option(verbose_info = FALSE)
    idf <- read_idf(example(), use_idd(8.8, "auto"))

    # transition action {{{
    expect_equivalent(
        trans_action(idf, "Construction",
            offset = list(2L, 4L),
            add = list(2L:3L, "No")
        ),
        data.table(
            id = rep(15:17, each = 4L),
            name = rep(c("R13WALL", "FLOOR", "ROOF31"), each = 4L),
            class = rep("Construction", 12L),
            index = rep(c(1:4), 3L),
            field = rep(c("Name", NA_character_, NA_character_, "Outside Layer"), 3L),
            value = c(
                "R13WALL", "No", "No", "R13LAYER",
                "FLOOR", "No", "No", "C5 - 4 IN HW CONCRETE",
                "ROOF31", "No", "No", "R31LAYER"
            )
        )
    )

    # can insert new extensible fields
    expect_equivalent(
        trans_action(idf, "Construction",
            insert = list(2:3, NA, step = 1)
        ),
        data.table(
            id = rep(15:17, each = 6L),
            name = rep(c("R13WALL", "FLOOR", "ROOF31"), each = 6L),
            class = rep("Construction", 18L),
            index = rep(c(1:6), 3L),
            field = rep(c("Name", NA_character_, NA_character_, "Outside Layer", NA_character_, NA_character_), 3L),
            value = c(
                "R13WALL", NA_character_, NA_character_, "R13LAYER", NA_character_, NA_character_,
                "FLOOR", NA_character_, NA_character_, "C5 - 4 IN HW CONCRETE", NA_character_, NA_character_,
                "ROOF31", NA_character_, NA_character_, "R31LAYER", NA_character_, NA_character_
            )
        )
    )

    # can insert multiple times
    expect_equivalent(
        trans_action(idf, "RunPeriod",
            insert = list(c(`Start Year` = 4)),
            insert = list(c(`End Year` = 7))
        ),
        data.table(
            id = rep(8L, 13),
            name = rep(NA_character_, 13),
            class = rep("RunPeriod", 13),
            index = 1:13,
            field = c("Name", "Begin Month", "Begin Day of Month", "Start Year",
                      "End Month", "End Day of Month", "End Year",
                      "Day of Week for Start Day", "Use Weather File Holidays and Special Days",
                      "Use Weather File Daylight Saving Period", "Apply Weekend Holiday Rule",
                      "Use Weather File Rain Indicators", "Use Weather File Snow Indicators"
            ),
            value = c(NA_character_, "1", "1", NA_character_, "12", "31", NA_character_,
                "Tuesday", "Yes", "Yes", "No", "Yes", "Yes"
            )
        )
    )
    # }}}

    # preprocess {{{
    expect_silent(new_idf <- trans_preprocess(idf, 8.9, "Construction"))
    expect_equivalent(new_idf$version(), numeric_version("8.9.0"))
    expect_false(new_idf$is_valid_class("Construction"))
    expect_false(._get_private(new_idf)$m_log$uuid == ._get_private(idf)$m_log$uuid)
    # }}}

    # versions {{{
    expect_equivalent(trans_upper_versions(idf, 9.1),
        numeric_version(c("8.8.0", "8.9.0", "9.0.0", "9.0.1", "9.1.0"))
    )
    # }}}

    # transition functions {{{
    expect_equivalent(
        trans_fun_names(c("8.8.0", "8.9.0", "9.0.1", "9.1.0")),
        c("f880t890", "f890t900", "f900t910")
    )
    # }}}
})

test_that("Transition", {
    skip_on_cran()
    skip_if_not(is_avail_eplus(9.1))

    # suppress build tag missing warnings
    suppressWarnings(use_idd(7.2))
    suppressWarnings(use_idd(8.0))

    expect_error(transition(idf, 7.3), class = "error_not_idd_ver")

    # basic workflow {{{
    expect_silent(res <- transition(idf, 8.0))
    expect_silent(res <- transition(idf, 8.0, keep_all = TRUE))
    expect_equal(names(res), c("7.2", "8.0"))
    expect_silent(res <- transition(idf, 8.0, save = TRUE, dir = file.path(tempdir(), "eplusr"), keep_all = TRUE))
    expect_equal(sapply(res, function (idf) basename(idf$path())),
        c("7.2" = paste0(prefix(idf), "V720.idf"),
          "8.0" = paste0(prefix(idf), "V800.idf"))
    )
    expect_true(all(file.exists(file.path(tempfile(), "eplusr", paste0(tools::file_path_sans_ext(basename(idf$path())), "V720.idf")))))
    expect_silent(res_eplus <- version_updater(idf, 8.0, dir = file.path(tempdir(), "eplus"), keep_all = TRUE))
    expect_identical(lapply(res, content), lapply(res_eplus, content))
    # }}}
    expect_identical_trans(7.2, 8.0)
    expect_identical_trans(8.0, 8.1)
    expect_identical_trans(8.1, 8.2)
    expect_identical_trans(8.2, 8.3)
    expect_identical_trans(8.3, 8.4)
    expect_identical_trans(8.4, 8.5)
    expect_identical_trans(8.5, 8.6)
    expect_identical_trans(8.6, 8.7)
    expect_identical_trans(8.7, 8.8)
    expect_identical_trans(8.8, 8.9)
    expect_identical_trans(8.9, 9.0)
    expect_identical_trans(9.0, 9.1)
})
