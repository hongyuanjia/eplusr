has_hashed_result_file <- function(info) {
    if (is.null(info) || !length(info)) return(FALSE)

    any(vapply(info, function(x) {
        isTRUE(x$exists) &&
            !is.null(x$hash_algorithm) &&
            x$hash_algorithm %in% c("sha256", "md5") &&
            !is.null(x$hash) &&
            nzchar(x$hash) &&
            !is.null(x$size) &&
            x$size >= 0
    }, logical(1L)))
}

manifest_file_path <- function(path, manifest_path) {
    if (grepl("^(/|[A-Za-z]:[/\\\\]|[/\\\\]{2})", path)) {
        normalizePath(path, mustWork = FALSE)
    } else {
        normalizePath(file.path(dirname(manifest_path), path), mustWork = FALSE)
    }
}

write_job_manifest <- function(x, path) {
    jsonlite::write_json(x, path, pretty = TRUE, auto_unbox = TRUE,
        null = "null", na = "null")
}

test_that("save_job() and read_job() round-trip job manifests", {
    skip_on_cran()
    skip_if_not(is_avail_eplus(LATEST_EPLUS_VER))

    path_idf <- copy_eplus_example(LATEST_EPLUS_VER, "1ZoneUncontrolled.idf")
    path_epw <- path_eplus_weather(LATEST_EPLUS_VER, "USA_CO_Golden-NREL.724666_TMY3.epw")

    job <- eplus_job(path_idf, path_epw)
    path <- tempfile(fileext = ".json")

    expect_equal(save_job(job, path), path)
    expect_true(jsonlite::validate(paste(readLines(path), collapse = "\n")))
    manifest <- jsonlite::fromJSON(path,
        simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
    expect_named(manifest$inputs, c("model_store", "epw"))

    restored <- read_job(path)
    expect_s3_class(restored, "EplusJob")
    expect_equal(restored$path(), job$path())
    expect_equal(restored$status(), job$status())

    invalid <- tempfile(fileext = ".json")
    writeLines('{"format":"eplusr-job"}', invalid)
    expect_error(read_job(invalid), "job manifest")

    invalid_kind <- jsonlite::fromJSON(path,
        simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
    invalid_kind$inputs$idfs <- "unexpected.idf"
    invalid <- tempfile(fileext = ".json")
    write_job_manifest(invalid_kind, invalid)
    expect_error(read_job(invalid), "job manifest\\$inputs")

    invalid_store <- manifest
    invalid_store$inputs$model_store$models[[1L]]$prepared_path <- NULL
    invalid <- tempfile(fileext = ".json")
    write_job_manifest(invalid_store, invalid)
    expect_error(read_job(invalid), "job manifest\\$inputs\\$model_store")

    invalid_ref <- manifest
    invalid_ref$inputs$model_store$cases[[1L]]$model_id <- 999L
    invalid <- tempfile(fileext = ".json")
    write_job_manifest(invalid_ref, invalid)
    expect_error(read_job(invalid), class = "eplusr_error_job_json_model_store")
})

test_that("completed EplusJob results can be restored from JSON", {
    skip_on_cran()
    skip_if_not_integration()
    skip_if_not(is_avail_eplus(LATEST_EPLUS_VER))

    # The hash-verification branch changes the ERR file, so work on a private
    # copy of the session-level real output bundle.
    bundle <- copy_latest_output_bundle()
    job <- bundle$job

    path <- tempfile(fileext = ".json")
    save_job(job, path)

    manifest <- jsonlite::fromJSON(path,
        simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
    expect_true(has_hashed_result_file(manifest$run$file_info))

    restored <- read_job(path)
    expect_equal(restored$status(), job$status())
    expect_equal(restored$list_files(simplify = TRUE), job$list_files(simplify = TRUE))
    expect_s3_class(restored$errors(), "ErrFile")

    err_index <- which(vapply(manifest$run$file_info, function(x) {
        identical(x$type, "err") && isTRUE(x$exists)
    }, logical(1L)))[[1L]]
    err_file <- manifest$run$file_info[[err_index]]$path
    cat("\nmodified after save_job()\n", file = manifest_file_path(err_file, path),
        append = TRUE)

    expect_warning(read_job(path), class = "eplusr_warning_job_json_file_changed")
    expect_error(read_job(path, verify = "error"), class = "eplusr_error_job_json_file_changed")
    expect_silent(read_job(path, verify = "ignore"))
})

test_that("EplusGroupJob run state can be restored from JSON", {
    skip_on_cran()
    skip_if_not_integration()
    skip_if_not(is_avail_eplus(LATEST_EPLUS_VER))

    bundle <- copy_latest_output_bundle()
    path_idf <- bundle$idf_path
    path_epw <- bundle$epw_path
    job <- bundle$job
    result <- get_priv_env(job)$m_job

    group <- group_job(path_idf, path_epw)
    priv <- get_priv_env(group)
    jobs <- pre_job_inputs(path_idf, path_epw, result$output_dir,
        design_day = FALSE, eplus = LATEST_EPLUS_VER)
    set(jobs, NULL,
        c("status", "process", "exit_status", "result", "start_time",
          "end_time", "stdout", "stderr"),
        list("completed", list(NULL), 0L, list(result), result$start_time,
            result$end_time, list(character()), list(character()))
    )
    priv$m_job <- list(
        jobs = jobs,
        options = list(num_parallel = 1L, echo = FALSE, expand_obj = TRUE, readvars = TRUE)
    )
    priv$m_log$start_time <- result$start_time
    priv$m_log$end_time <- result$end_time

    path <- tempfile(fileext = ".json")
    save_job(group, path)

    manifest <- jsonlite::fromJSON(path,
        simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
    expect_true(has_hashed_result_file(manifest$run$jobs[[1L]]$result$file_info))

    invalid_kind <- manifest
    invalid_kind$run$version <- as.character(LATEST_EPLUS_VER)
    invalid <- tempfile(fileext = ".json")
    write_job_manifest(invalid_kind, invalid)
    expect_error(read_job(invalid), class = "eplusr_error_job_json_kind")

    restored <- read_job(path)
    expect_s3_class(restored, "EplusGroupJob")
    expect_equal(restored$status()[1:5], group$status()[1:5])
    expect_equal(restored$status()$job_status$exit_status, 0L)
    expect_equal(restored$list_files(1, simplify = TRUE), group$list_files(1, simplify = TRUE))
})

test_that("ParametricJob generated models are snapshotted and restored", {
    skip_on_cran()
    skip_if_not(is_avail_eplus(LATEST_EPLUS_VER))

    path_idf <- copy_eplus_example(LATEST_EPLUS_VER, "1ZoneUncontrolled.idf")
    path_epws <- path_eplus_weather(LATEST_EPLUS_VER,
        c("USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw",
          "USA_CO_Golden-NREL.724666_TMY3.epw")
    )

    param <- param_job(path_idf, NULL)
    param$weathers(path_epws, names = c("sf", "golden"))
    param$apply_measure(function(idf, num) idf, num = 1:2, .names = "case")

    path <- tempfile(fileext = ".json")
    save_job(param, path)

    manifest <- jsonlite::fromJSON(path,
        simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
    expect_length(manifest$inputs$weather_cases, 2L)
    expect_length(manifest$parametric$model_cases, 2L)
    expect_length(manifest$parametric$run_cases, 4L)

    snap_dir <- file.path(dirname(path),
        paste0(tools::file_path_sans_ext(basename(path)), "_files"), "model_store")
    expect_true(dir.exists(snap_dir))
    expect_length(list.files(snap_dir, "\\.idf$", full.names = TRUE, recursive = TRUE), 3L)

    invalid_kind <- manifest
    invalid_kind$parametric <- NULL
    invalid <- tempfile(fileext = ".json")
    write_job_manifest(invalid_kind, invalid)
    expect_error(read_job(invalid), class = "eplusr_error_job_json_kind")

    invalid_weather <- manifest
    invalid_weather$inputs$weather_cases[[1L]]$weather_index <- NULL
    invalid <- tempfile(fileext = ".json")
    write_job_manifest(invalid_weather, invalid)
    expect_error(read_job(invalid), class = "eplusr_error_job_json_kind")

    invalid_run <- manifest
    invalid_run$parametric$run_cases[[1L]]$weather_index <- "bad"
    invalid <- tempfile(fileext = ".json")
    write_job_manifest(invalid_run, invalid)
    expect_error(read_job(invalid), class = "eplusr_error_job_json_kind")

    restored <- read_job(path)
    expect_s3_class(restored, "ParametricJob")
    expect_equal(names(restored$models()), names(param$models()))
    expect_equal(restored$weathers(), param$weathers())
    expect_equal(restored$cases(), param$cases())
    expect_equal(restored$cases("run"), param$cases("run"))
})
