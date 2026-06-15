#' @include standalone-schema.R
NULL

#' Save and Read EnergyPlus Job Manifests
#'
#' `save_job()` writes a JSON manifest for an [EplusJob], [EplusGroupJob], or
#' [ParametricJob]. `read_job()` reads that manifest, validates its structure,
#' verifies saved output file metadata, and restores the job object so existing
#' simulation outputs can be collected.
#'
#' The JSON manifest stores references to input and output files instead of
#' embedding EnergyPlus files. By default, paths under the manifest directory
#' are stored relative to the manifest so the directory can be moved together.
#' EnergyPlus result files are also recorded with file size, modification time,
#' and a checksum when available. These file metadata are informational and do
#' not prevent `read_job()` from restoring the job object if files have been
#' removed.
#'
#' @section Model snapshots:
#' The manifest stores a prepared model snapshot store for [EplusJob],
#' [EplusGroupJob], and [ParametricJob]. For [ParametricJob], arbitrary R
#' measure functions passed to `$apply_measure()` are not serialized; the
#' restored job uses the generated model snapshots and stored case data.
#'
#' @param x An [EplusJob], [EplusGroupJob], or [ParametricJob].
#' @param path A JSON file path. If `NULL`, a default file name is used in the
#'   input model directory.
#' @param overwrite Whether an existing manifest, and any generated
#'   [ParametricJob] model snapshots, may be overwritten.
#' @param relative Whether paths under the manifest directory should be written
#'   as relative paths.
#' @param hash Whether to hash existing EnergyPlus result files. File size and
#'   modification time are still recorded when `FALSE`.
#' @param validate Whether to validate the JSON manifest against eplusr's job
#'   manifest schema before restoring the object.
#' @param verify How to handle saved EnergyPlus result files whose current
#'   checksum or metadata no longer match the manifest. `"warn"` restores the
#'   job and emits a warning, `"error"` stops, and `"ignore"` skips the check.
#'
#' @return `save_job()` invisibly returns the manifest path. `read_job()` returns
#'   an [EplusJob], [EplusGroupJob], or [ParametricJob].
#'
#' @examples
#' \dontrun{
#' job <- eplus_job(path_idf, path_epw)
#' job$run(echo = FALSE)
#'
#' path <- save_job(job, tempfile(fileext = ".json"))
#' restored <- read_job(path)
#' restored$status()
#' }
#'
#' @export
save_job <- function(x, path = NULL, overwrite = FALSE, relative = TRUE,
                     hash = TRUE) {
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(relative)
    checkmate::assert_flag(hash)

    if (!job_json_is_supported_job(x)) {
        abort("'x' must be an EplusJob, EplusGroupJob, or ParametricJob object.",
            "job_json_invalid_object")
    }

    status <- x$status()
    if (isTRUE(status$alive)) {
        abort("Cannot save a job while its simulation is still running.",
            "job_json_running")
    }

    priv <- get_priv_env(x)
    if (inherits(priv$m_job, "process") || inherits(priv$m_job, "r_process")) {
        abort("Cannot save unretrieved background process state. Please call `$status()` after it finishes and try again.",
            "job_json_process")
    }

    if (is.null(path)) {
        path <- job_json_default_path(x)
    } else {
        checkmate::assert_string(path, min.chars = 1L)
        path <- normalizePath(path, mustWork = FALSE)
    }

    if (file.exists(path) && !overwrite) {
        abort(sprintf("File already exists: %s", surround(path)), "job_json_file_exists")
    }

    dir <- dirname(path)
    if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE)) {
        abort(sprintf("Failed to create output directory: %s", surround(dir)),
            "job_json_dir")
    }

    manifest <- job_json_manifest(x, path, overwrite = overwrite,
        relative = relative, hash = hash)
    job_json_validate_manifest(manifest)

    jsonlite::write_json(
        manifest, path, pretty = TRUE, auto_unbox = TRUE,
        null = "null", na = "null"
    )

    invisible(path)
}

#' @rdname save_job
#' @export
read_job <- function(path, validate = TRUE, verify = c("warn", "error", "ignore")) {
    checkmate::assert_file_exists(path, "r", "json")
    checkmate::assert_flag(validate)
    verify <- match.arg(verify)

    path <- normalizePath(path, mustWork = TRUE)
    manifest <- jsonlite::fromJSON(path,
        simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)

    if (validate) {
        job_json_validate_manifest(manifest)
    }

    base <- dirname(path)
    rel <- job_json_get(manifest, "paths_relative_to")
    if (!is.null(rel)) {
        base <- normalizePath(file.path(base, rel), mustWork = FALSE)
    }

    job_json_verify_files(manifest, base, verify)

    kind <- job_json_scalar_character(manifest$kind)
    switch(kind,
        EplusJob = job_json_restore_eplus_job(manifest, base),
        EplusGroupJob = job_json_restore_group_job(manifest, base),
        ParametricJob = job_json_restore_parametric_job(manifest, base),
        abort(sprintf("Unsupported job manifest kind: %s", surround(kind)),
            "job_json_kind")
    )
}

JOB_JSON_FORMAT <- "eplusr-job"
JOB_JSON_MANIFEST_VERSION <- 2L

SCHEMA_EPLUS_JOB_MANIFEST <- schema_flatten(schema_read('{
  "version": "1.0.0",
  "$defs": {
    "nullable_string": {
      "check": { "kind": "string", "min.chars": 1, "null.ok": true, "na.ok": true }
    },
    "string_vector": {
      "any": [
        { "check": { "kind": "character", "any.missing": true, "null.ok": true } },
        {
          "check": { "kind": "list", "null.ok": true },
          "keys": { "type": "unnamed" },
          "rest": { "$ref": "#/$defs/nullable_string" }
        }
      ]
    },
    "flag_or_null": {
      "check": { "kind": "flag", "null.ok": true }
    },
    "logical_vector": {
      "any": [
        { "check": { "kind": "logical", "null.ok": true } },
        {
          "check": { "kind": "list", "null.ok": true },
          "keys": { "type": "unnamed" },
          "rest": { "$ref": "#/$defs/flag_or_null" }
        }
      ]
    },
    "text_or_array": {
      "any": [
        { "check": { "kind": "character", "any.missing": true, "null.ok": true } },
        { "check": { "kind": "list", "null.ok": true } }
      ]
    },
    "generic_rows": {
      "check": { "kind": "list", "null.ok": true }
    },
    "result_file": {
      "check": { "kind": "list", "null.ok": true },
      "rest": { "$ref": "#/$defs/text_or_array" }
    },
    "model_store_model": {
      "check": { "kind": "list" },
      "keys": {
        "type": "unique",
        "must.include": [
          "model_id", "role", "name", "source_path", "prepared_path",
          "version", "sql", "dict", "signature", "size", "mtime"
        ],
        "subset.of": [
          "model_id", "role", "name", "source_path", "prepared_path",
          "version", "sql", "dict", "signature", "size", "mtime"
        ]
      },
      "fields": {
        "model_id": { "check": { "kind": "int", "lower": 1 } },
        "role": {
          "check": {
            "kind": "choice",
            "choices": ["input", "seed", "case"]
          }
        },
        "name": { "$ref": "#/$defs/nullable_string" },
        "source_path": { "$ref": "#/$defs/nullable_string" },
        "prepared_path": { "check": { "kind": "string", "min.chars": 1 } },
        "version": { "check": { "kind": "string", "min.chars": 1 } },
        "sql": { "check": { "kind": "flag" } },
        "dict": { "check": { "kind": "flag" } },
        "signature": { "check": { "kind": "string", "min.chars": 1 } },
        "size": { "check": { "kind": "number", "lower": 0 } },
        "mtime": { "$ref": "#/$defs/nullable_string" }
      }
    },
    "model_store_case": {
      "check": { "kind": "list" },
      "keys": {
        "type": "unique",
        "must.include": ["case_index", "model_id", "name", "run_path"],
        "subset.of": ["case_index", "model_id", "name", "run_path"]
      },
      "fields": {
        "case_index": { "check": { "kind": "int", "lower": 1 } },
        "model_id": { "check": { "kind": "int", "lower": 1 } },
        "name": { "$ref": "#/$defs/nullable_string" },
        "run_path": { "$ref": "#/$defs/nullable_string" }
      }
    },
    "model_store": {
      "check": { "kind": "list" },
      "keys": {
        "type": "unique",
        "must.include": [
          "version", "seed_model_id", "cases_valid", "invalid_reason",
          "models", "cases"
        ],
        "subset.of": [
          "version", "seed_model_id", "cases_valid", "invalid_reason",
          "models", "cases"
        ]
      },
      "fields": {
        "version": { "check": { "kind": "int", "lower": 1, "upper": 1 } },
        "seed_model_id": { "check": { "kind": "int", "lower": 1, "null.ok": true } },
        "cases_valid": { "check": { "kind": "flag" } },
        "invalid_reason": { "$ref": "#/$defs/nullable_string" },
        "models": {
          "check": { "kind": "list", "min.len": 1 },
          "keys": { "type": "unnamed" },
          "rest": { "$ref": "#/$defs/model_store_model" }
        },
        "cases": {
          "check": { "kind": "list" },
          "keys": { "type": "unnamed" },
          "rest": { "$ref": "#/$defs/model_store_case" }
        }
      }
    },
    "result_file_record": {
      "check": { "kind": "list" },
      "fields": {
        "type": { "$ref": "#/$defs/nullable_string" },
        "index": { "check": { "kind": "int", "lower": 1, "null.ok": true } },
        "path": { "$ref": "#/$defs/nullable_string" },
        "exists": { "check": { "kind": "flag" } },
        "size": { "check": { "kind": "number", "lower": 0, "null.ok": true } },
        "mtime": { "$ref": "#/$defs/nullable_string" },
        "hash_algorithm": { "$ref": "#/$defs/nullable_string" },
        "hash": { "$ref": "#/$defs/nullable_string" }
      }
    },
    "result_file_info": {
      "check": { "kind": "list", "null.ok": true },
      "keys": { "type": "unnamed" },
      "rest": { "$ref": "#/$defs/result_file_record" }
    },
    "energyplus_result": {
      "check": { "kind": "list", "null.ok": true },
      "fields": {
        "version": { "$ref": "#/$defs/nullable_string" },
        "energyplus": { "$ref": "#/$defs/nullable_string" },
        "start_time": { "$ref": "#/$defs/nullable_string" },
        "end_time": { "$ref": "#/$defs/nullable_string" },
        "exit_status": { "check": { "kind": "int", "null.ok": true } },
        "output_dir": { "$ref": "#/$defs/nullable_string" },
        "file": { "$ref": "#/$defs/result_file" },
        "file_info": { "$ref": "#/$defs/result_file_info" },
        "run": { "$ref": "#/$defs/generic_rows" }
      }
    },
    "job_row": {
      "check": { "kind": "list" },
      "fields": {
        "index": { "check": { "kind": "int", "lower": 1 } },
        "status": { "$ref": "#/$defs/nullable_string" },
        "model": { "$ref": "#/$defs/nullable_string" },
        "weather": { "$ref": "#/$defs/nullable_string" },
        "output_dir": { "$ref": "#/$defs/nullable_string" },
        "energyplus_exe": { "$ref": "#/$defs/nullable_string" },
        "annual": { "check": { "kind": "flag" } },
        "design_day": { "check": { "kind": "flag" } },
        "resources": { "$ref": "#/$defs/string_vector" },
        "exit_status": { "check": { "kind": "int", "null.ok": true } },
        "start_time": { "$ref": "#/$defs/nullable_string" },
        "end_time": { "$ref": "#/$defs/nullable_string" },
        "stdout": { "$ref": "#/$defs/text_or_array" },
        "stderr": { "$ref": "#/$defs/text_or_array" },
        "result": { "$ref": "#/$defs/energyplus_result" }
      }
    }
  },
  "check": { "kind": "list" },
  "keys": {
    "type": "unique",
    "must.include": [
      "format", "manifest_version", "kind", "package_version",
      "created_at", "paths_relative_to", "inputs", "log", "run"
    ],
    "subset.of": [
      "format", "manifest_version", "kind", "package_version",
      "created_at", "paths_relative_to", "inputs", "log", "run",
      "parametric"
    ]
  },
  "fields": {
    "format": {
      "check": { "kind": "choice", "choices": ["eplusr-job"] }
    },
    "manifest_version": {
      "check": { "kind": "int", "lower": 1 }
    },
    "kind": {
      "check": {
        "kind": "choice",
        "choices": ["EplusJob", "EplusGroupJob", "ParametricJob"]
      }
    },
    "package_version": {
      "check": { "kind": "string", "min.chars": 1 }
    },
    "created_at": {
      "check": { "kind": "string", "min.chars": 1 }
    },
    "paths_relative_to": {
      "check": { "kind": "string", "min.chars": 1, "null.ok": true }
    },
    "inputs": {
      "check": { "kind": "list" },
      "keys": {
        "type": "unique",
        "subset.of": [
          "model_store", "epw", "epws", "weather"
        ]
      },
      "fields": {
        "model_store": { "$ref": "#/$defs/model_store" },
        "epw": { "$ref": "#/$defs/nullable_string" },
        "epws": { "$ref": "#/$defs/string_vector" },
        "weather": { "$ref": "#/$defs/nullable_string" }
      }
    },
    "log": {
      "check": { "kind": "list" },
      "keys": {
        "type": "unique",
        "subset.of": [
          "uuid", "seed_uuid", "idf_uuid", "unsaved", "start_time",
          "end_time", "killed"
        ]
      },
      "fields": {
        "uuid": { "$ref": "#/$defs/nullable_string" },
        "seed_uuid": { "$ref": "#/$defs/nullable_string" },
        "idf_uuid": { "$ref": "#/$defs/string_vector" },
        "unsaved": { "$ref": "#/$defs/logical_vector" },
        "start_time": { "$ref": "#/$defs/nullable_string" },
        "end_time": { "$ref": "#/$defs/nullable_string" },
        "killed": { "$ref": "#/$defs/flag_or_null" }
      }
    },
    "run": {
      "check": { "kind": "list", "null.ok": true },
      "fields": {
        "version": { "$ref": "#/$defs/nullable_string" },
        "energyplus": { "$ref": "#/$defs/nullable_string" },
        "start_time": { "$ref": "#/$defs/nullable_string" },
        "end_time": { "$ref": "#/$defs/nullable_string" },
        "exit_status": { "check": { "kind": "int", "null.ok": true } },
        "output_dir": { "$ref": "#/$defs/nullable_string" },
        "file": { "$ref": "#/$defs/result_file" },
        "file_info": { "$ref": "#/$defs/result_file_info" },
        "run": { "$ref": "#/$defs/generic_rows" },
        "options": { "check": { "kind": "list", "null.ok": true } },
        "jobs": {
          "check": { "kind": "list", "null.ok": true },
          "keys": { "type": "unnamed" },
          "rest": { "$ref": "#/$defs/job_row" }
        }
      }
    },
    "parametric": {
      "check": { "kind": "list", "null.ok": true },
      "fields": {
        "applied": { "check": { "kind": "flag" } },
        "simple": { "$ref": "#/$defs/flag_or_null" },
        "replayable": { "check": { "kind": "flag" } },
        "measure_name": { "$ref": "#/$defs/nullable_string" },
        "bare": { "$ref": "#/$defs/flag_or_null" },
        "params": { "$ref": "#/$defs/generic_rows" },
        "cases": { "$ref": "#/$defs/generic_rows" }
      }
    }
  }
}'))

job_json_validate_manifest <- function(x) {
    schema_validate(SCHEMA_EPLUS_JOB_MANIFEST, x, mode = "assert", name = "job manifest")

    if (!identical(job_json_scalar_character(x$format), JOB_JSON_FORMAT)) {
        abort("Invalid eplusr job manifest format.", "job_json_format")
    }

    ver <- job_json_scalar_integer(x$manifest_version)
    if (is.na(ver) || ver > JOB_JSON_MANIFEST_VERSION) {
        abort(sprintf(
            "Unsupported eplusr job manifest version: %s.",
            surround(job_json_scalar_character(x$manifest_version))
        ), "job_json_version")
    }

    job_json_validate_manifest_kind(x)
    job_json_validate_manifest_model_store(x)

    invisible(x)
}

job_json_validate_manifest_kind <- function(x) {
    kind <- job_json_scalar_character(x$kind)
    specs <- switch(kind,
        EplusJob = list(
            inputs = c("model_store", "epw"),
            log = c("uuid", "seed_uuid", "unsaved", "start_time", "end_time", "killed"),
            run = c("version", "energyplus", "start_time", "end_time",
                "exit_status", "output_dir", "file", "file_info", "run"),
            parametric = NULL
        ),
        EplusGroupJob = list(
            inputs = c("model_store", "epws"),
            log = c("uuid", "idf_uuid", "unsaved", "start_time", "end_time", "killed"),
            run = c("options", "jobs"),
            parametric = NULL
        ),
        ParametricJob = list(
            inputs = c("model_store", "weather"),
            log = c("uuid", "idf_uuid", "seed_uuid", "unsaved", "start_time",
                "end_time", "killed"),
            run = c("options", "jobs"),
            parametric = c("applied", "simple", "replayable", "measure_name",
                "bare", "params", "cases")
        ),
        abort(sprintf("Unsupported job manifest kind: %s", surround(kind)),
            "job_json_kind")
    )

    job_json_assert_manifest_keys(x$inputs, specs$inputs, specs$inputs,
        sprintf("%s inputs", kind))
    job_json_assert_manifest_keys(x$log, specs$log, specs$log,
        sprintf("%s log", kind))

    if (is.null(x$run)) {
        # Unrun jobs have no run state to validate.
    } else {
        job_json_assert_manifest_keys(x$run, specs$run, specs$run,
            sprintf("%s run", kind))
    }

    if (is.null(specs$parametric)) {
        if ("parametric" %in% names(x)) {
            abort(sprintf("Field 'parametric' is only valid for ParametricJob manifests, not %s.",
                surround(kind)), "job_json_kind")
        }
    } else {
        if (!("parametric" %in% names(x)) || is.null(x$parametric)) {
            abort("ParametricJob manifests must include field 'parametric'.",
                "job_json_kind")
        }
        job_json_assert_manifest_keys(x$parametric, specs$parametric, specs$parametric,
            sprintf("%s parametric", kind))
    }

    invisible(x)
}

job_json_validate_manifest_model_store <- function(x) {
    kind <- job_json_scalar_character(x$kind)
    store <- x$inputs$model_store
    models <- store$models

    if (is.null(models) || !length(models)) {
        abort("Job manifest model store must contain at least one model.",
            "job_json_model_store")
    }

    model_ids <- viapply(models, function(row) job_json_scalar_integer(row$model_id))
    if (anyDuplicated(model_ids)) {
        abort("Job manifest model store contains duplicate model ids.",
            "job_json_model_store")
    }

    seed_id <- job_json_scalar_integer(store$seed_model_id, default = NULL)
    if (identical(kind, "ParametricJob") && is.null(seed_id)) {
        abort("ParametricJob manifest model store must include a seed model id.",
            "job_json_model_store")
    }
    if (!is.null(seed_id) && !(seed_id %in% model_ids)) {
        abort("Job manifest model store seed model id does not match any stored model.",
            "job_json_model_store")
    }

    cases <- store$cases
    if (kind %in% c("EplusJob", "EplusGroupJob") && (is.null(cases) || !length(cases))) {
        abort(sprintf("%s manifest model store must contain at least one case.", kind),
            "job_json_model_store")
    }

    if (is.null(cases) || !length(cases)) return(invisible(x))

    case_indices <- viapply(cases, function(row) job_json_scalar_integer(row$case_index))
    if (anyDuplicated(case_indices)) {
        abort("Job manifest model store contains duplicate case indices.",
            "job_json_model_store")
    }

    case_model_ids <- viapply(cases, function(row) job_json_scalar_integer(row$model_id))
    missing <- setdiff(case_model_ids, model_ids)
    if (length(missing)) {
        abort("Job manifest model store cases reference unknown model ids.",
            "job_json_model_store")
    }

    invisible(x)
}

job_json_assert_manifest_keys <- function(x, required, allowed, context) {
    keys <- names(x)
    missing <- setdiff(required, keys)
    extra <- setdiff(keys, allowed)
    if (!length(missing) && !length(extra)) return(invisible(x))

    msg <- sprintf("Job manifest fields are not valid for %s.", context)
    if (length(missing)) {
        msg <- paste0(msg, "\nMissing fields: ", collapse(surround(missing)), ".")
    }
    if (length(extra)) {
        msg <- paste0(msg, "\nUnexpected fields: ", collapse(surround(extra)), ".")
    }

    abort(msg, "job_json_kind")
}

job_json_verify_files <- function(manifest, base, action) {
    if (identical(action, "ignore")) return(invisible(NULL))

    changes <- job_json_file_changes(manifest, base)
    if (!nrow(changes)) return(invisible(NULL))

    msg <- paste0(
        "Saved EnergyPlus result files may have changed since the job manifest was written.\n",
        job_json_file_change_summary(changes), "\n",
        "Re-run the simulation or restore the files from a trusted copy before using these results for analysis.\n",
        "Use `read_job(..., verify = \"ignore\")` to skip this check, or `verify = \"error\"` to fail instead.\n\n",
        "Affected files:\n",
        job_json_file_change_bullets(changes)
    )

    if (identical(action, "error")) {
        abort(msg, "job_json_file_changed", changes = changes)
    }

    warn(msg, "job_json_file_changed", changes = changes)
    invisible(changes)
}

job_json_file_changes <- function(manifest, base) {
    records <- job_json_file_records(manifest)
    if (!length(records)) return(data.table())

    out <- lapply(records, function(record) job_json_file_change(record, base))
    out <- out[!vapply(out, is.null, logical(1L))]
    if (!length(out)) return(data.table())

    rbindlist(out, fill = TRUE)
}

job_json_file_records <- function(manifest) {
    records <- list()
    run <- job_json_get(manifest, "run")

    add_records <- function(info) {
        if (is.null(info)) return()
        records <<- c(records, info)
    }

    add_records(job_json_get(run, "file_info"))
    jobs <- job_json_get(run, "jobs")
    if (!is.null(jobs)) {
        for (job in jobs) {
            add_records(job_json_get(job_json_get(job, "result"), "file_info"))
        }
    }

    records
}

job_json_file_change <- function(record, base) {
    if (!isTRUE(job_json_scalar_logical(record$exists, default = FALSE))) {
        return(NULL)
    }

    path <- job_json_decode_path_nullable(record$path, base)
    type <- job_json_scalar_character(record$type)
    index <- job_json_scalar_integer(record$index, default = NA_integer_)
    expected_hash <- job_json_scalar_character(record$hash)
    algorithm <- job_json_scalar_character(record$hash_algorithm)
    expected_size <- job_json_scalar_numeric(record$size)
    expected_mtime <- job_json_parse_time(record$mtime)

    if (is.null(path) || !file.exists(path)) {
        return(data.table(
            problem = "missing",
            type = type %||% NA_character_,
            index = index,
            path = path %||% NA_character_,
            expected_hash = expected_hash %||% NA_character_,
            actual_hash = NA_character_,
            expected_size = expected_size %||% NA_real_,
            actual_size = NA_real_
        ))
    }

    info <- file.info(path)
    actual_size <- as.numeric(info$size)

    if (!is.null(algorithm) && !is.null(expected_hash) && nzchar(expected_hash)) {
        actual_hash <- job_json_file_hash(path, algorithm)$value
        if (!identical(actual_hash, expected_hash)) {
            return(data.table(
                problem = "modified",
                type = type %||% NA_character_,
                index = index,
                path = path,
                expected_hash = expected_hash,
                actual_hash = actual_hash,
                expected_size = expected_size %||% NA_real_,
                actual_size = actual_size
            ))
        }
    } else if (job_json_file_metadata_changed(expected_size, actual_size, expected_mtime, info$mtime)) {
        return(data.table(
            problem = "metadata_changed",
            type = type %||% NA_character_,
            index = index,
            path = path,
            expected_hash = NA_character_,
            actual_hash = NA_character_,
            expected_size = expected_size %||% NA_real_,
            actual_size = actual_size
        ))
    }

    NULL
}

job_json_file_metadata_changed <- function(expected_size, actual_size, expected_mtime, actual_mtime) {
    size_changed <- !is.null(expected_size) && !is.na(expected_size) &&
        !identical(expected_size, actual_size)
    time_changed <- !is.null(expected_mtime) && !is.na(expected_mtime) &&
        abs(as.numeric(difftime(actual_mtime, expected_mtime, units = "secs"))) > 1

    size_changed || time_changed
}

job_json_file_change_summary <- function(changes) {
    tab <- table(changes[["problem"]])
    parts <- sprintf("%i %s", as.integer(tab), names(tab))
    paste0("Detected ", collapse(parts), ".")
}

job_json_file_change_bullets <- function(changes, n = 5L) {
    shown <- utils::head(changes, n)
    bullets <- vcapply(seq_len(nrow(shown)), function(i) {
        type <- shown$type[[i]]
        label <- if (is.na(type)) "<unknown>" else type
        sprintf("- [%s] %s: %s", shown$problem[[i]], label, shown$path[[i]])
    })

    extra <- nrow(changes) - length(bullets)
    if (extra > 0L) {
        bullets <- c(bullets, sprintf("- ... and %i more", extra))
    }

    paste(bullets, collapse = "\n")
}

job_json_is_supported_job <- function(x) {
    inherits(x, "EplusJob") || inherits(x, "EplusGroupJob") || inherits(x, "ParametricJob")
}

job_json_kind <- function(x) {
    if (inherits(x, "ParametricJob")) {
        "ParametricJob"
    } else if (inherits(x, "EplusGroupJob")) {
        "EplusGroupJob"
    } else {
        "EplusJob"
    }
}

job_json_default_path <- function(x) {
    priv <- get_priv_env(x)
    kind <- job_json_kind(x)

    dir <- switch(kind,
        EplusJob = priv$m_model_store$default_dir(),
        EplusGroupJob = priv$m_model_store$default_dir(),
        ParametricJob = priv$m_model_store$default_dir(seed = TRUE)
    )

    file <- switch(kind,
        EplusJob = "eplusr-job.json",
        EplusGroupJob = "eplusr-group-job.json",
        ParametricJob = "eplusr-parametric-job.json"
    )

    normalizePath(file.path(dir, file), mustWork = FALSE)
}

job_json_manifest <- function(x, path, overwrite, relative, hash) {
    priv <- get_priv_env(x)
    base <- dirname(path)
    kind <- job_json_kind(x)

    out <- list(
        format = JOB_JSON_FORMAT,
        manifest_version = JOB_JSON_MANIFEST_VERSION,
        kind = kind,
        package_version = as.character(utils::packageVersion("eplusr")),
        created_at = job_json_format_time(current()),
        paths_relative_to = if (relative) "." else NULL,
        inputs = switch(kind,
            EplusJob = job_json_eplus_inputs(priv, path, overwrite, relative),
            EplusGroupJob = job_json_group_inputs(priv, path, overwrite, relative),
            ParametricJob = job_json_parametric_inputs(priv, path, overwrite, relative)
        ),
        log = switch(kind,
            EplusJob = job_json_eplus_log(priv),
            EplusGroupJob = job_json_group_log(priv),
            ParametricJob = job_json_parametric_log(priv)
        ),
        run = switch(kind,
            EplusJob = job_json_encode_energyplus_result(priv$m_job, base, relative, hash),
            EplusGroupJob = job_json_encode_group_run(priv$m_job, base, relative, hash),
            ParametricJob = job_json_encode_group_run(priv$m_job, base, relative, hash)
        )
    )

    if (identical(kind, "ParametricJob")) {
        out$parametric <- job_json_parametric_state(priv)
    }

    out
}

job_json_eplus_inputs <- function(private, path, overwrite, relative) {
    base <- dirname(path)
    list(
        model_store = job_json_encode_model_store(private$m_model_store, base, relative,
            path = path, overwrite = overwrite),
        epw = job_json_encode_path(private$m_epw_path, base, relative)
    )
}

job_json_group_inputs <- function(private, path, overwrite, relative) {
    base <- dirname(path)
    list(
        model_store = job_json_encode_model_store(private$m_model_store, base, relative,
            path = path, overwrite = overwrite),
        epws = job_json_encode_path(private$m_epws_path, base, relative)
    )
}

job_json_parametric_inputs <- function(private, path, overwrite, relative) {
    list(
        model_store = job_json_encode_model_store(private$m_model_store, dirname(path), relative,
            path = path, overwrite = overwrite),
        weather = job_json_encode_path(private$m_epws_path, dirname(path), relative)
    )
}

job_json_encode_model_store <- function(store, base, relative, path, overwrite) {
    stem <- tools::file_path_sans_ext(basename(path))
    dir <- normalizePath(file.path(base, paste0(stem, "_files"), "model_store"),
        mustWork = FALSE)
    snapshot <- store$snapshot(dir, overwrite = overwrite)

    models <- snapshot$models
    if (nrow(models)) {
        set(models, NULL, "source_path",
            job_json_encode_path(models$source_path, base, relative))
        set(models, NULL, "prepared_path",
            job_json_encode_path(models$prepared_path, base, relative))
        set(models, NULL, "mtime", job_json_format_time(models$mtime))
    }

    cases <- snapshot$cases
    if (nrow(cases)) {
        set(cases, NULL, "run_path",
            job_json_encode_path(cases$run_path, base, relative))
    }

    list(
        version = 1L,
        seed_model_id = job_json_scalar_or_null(snapshot$seed_model_id),
        cases_valid = isTRUE(snapshot$cases_valid),
        invalid_reason = snapshot$invalid_reason,
        models = job_json_encode_table(models),
        cases = job_json_encode_table(cases)
    )
}

job_json_eplus_log <- function(private) {
    log <- private$m_log
    list(
        uuid = job_json_env_get(log, "uuid"),
        seed_uuid = job_json_env_get(log, "seed_uuid"),
        unsaved = job_json_env_get(log, "unsaved", FALSE),
        start_time = job_json_format_time(job_json_env_get(log, "start_time")),
        end_time = job_json_format_time(job_json_env_get(log, "end_time")),
        killed = job_json_env_get(log, "killed")
    )
}

job_json_group_log <- function(private) {
    log <- private$m_log
    list(
        uuid = job_json_env_get(log, "uuid"),
        idf_uuid = job_json_env_get(log, "idf_uuid"),
        unsaved = job_json_env_get(log, "unsaved", FALSE),
        start_time = job_json_format_time(job_json_env_get(log, "start_time")),
        end_time = job_json_format_time(job_json_env_get(log, "end_time")),
        killed = job_json_env_get(log, "killed")
    )
}

job_json_parametric_log <- function(private) {
    log <- private$m_log
    utils::modifyList(job_json_group_log(private), list(
        seed_uuid = job_json_env_get(log, "seed_uuid")
    ))
}

job_json_parametric_state <- function(private) {
    log <- private$m_log
    params <- job_json_env_get(log, "params")

    list(
        applied = private$m_model_store$has_cases(),
        simple = job_json_env_get(log, "simple"),
        replayable = isTRUE(job_json_env_get(log, "simple")),
        measure_name = job_json_env_get(log, "measure_name"),
        bare = job_json_env_get(log, "bare"),
        params = job_json_encode_table(params),
        cases = if (is.null(params) || !private$m_model_store$cases_valid()) {
            NULL
        } else {
            job_json_encode_table(param_cases(NULL, private))
        }
    )
}

job_json_encode_group_run <- function(x, base, relative, hash) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "r_process")) {
        abort("Cannot save unretrieved background group job state.", "job_json_process")
    }

    list(
        options = x$options %||% list(),
        jobs = job_json_encode_group_jobs(x$jobs, base, relative, hash)
    )
}

job_json_encode_group_jobs <- function(jobs, base, relative, hash) {
    if (is.null(jobs)) return(NULL)

    rows <- vector("list", nrow(jobs))
    for (i in seq_len(nrow(jobs))) {
        row <- list(
            index = as.integer(jobs$index[[i]]),
            status = jobs$status[[i]],
            model = job_json_encode_path(jobs$model[[i]], base, relative),
            weather = job_json_encode_path(jobs$weather[[i]], base, relative),
            output_dir = job_json_encode_path(jobs$output_dir[[i]], base, relative),
            energyplus_exe = job_json_encode_path(jobs$energyplus_exe[[i]], base, relative),
            annual = as.logical(jobs$annual[[i]]),
            design_day = as.logical(jobs$design_day[[i]]),
            resources = if ("resources" %in% names(jobs)) {
                job_json_encode_path(jobs$resources[[i]], base, relative)
            } else {
                NULL
            },
            exit_status = job_json_scalar_or_null(jobs$exit_status[[i]]),
            start_time = job_json_format_time(jobs$start_time[[i]]),
            end_time = job_json_format_time(jobs$end_time[[i]]),
            stdout = job_json_encode_value(jobs$stdout[[i]]),
            stderr = job_json_encode_value(jobs$stderr[[i]]),
            result = job_json_encode_energyplus_result(jobs$result[[i]], base, relative, hash)
        )
        rows[[i]] <- row
    }

    rows
}

job_json_encode_energyplus_result <- function(x, base, relative, hash) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "process")) {
        abort("Cannot save unretrieved background EnergyPlus process state.",
            "job_json_process")
    }

    list(
        version = as.character(x$version),
        energyplus = job_json_encode_path(x$energyplus, base, relative),
        start_time = job_json_format_time(x$start_time),
        end_time = job_json_format_time(x$end_time),
        exit_status = job_json_scalar_or_null(x$exit_status),
        output_dir = job_json_encode_path(x$output_dir, base, relative),
        file = job_json_encode_value(x$file),
        file_info = job_json_encode_file_info(x$file, x$output_dir, base, relative, hash),
        run = job_json_encode_table(x$run)
    )
}

job_json_encode_file_info <- function(files, output_dir, base, relative, hash) {
    if (is.null(files)) return(NULL)

    types <- names(files)
    if (is.null(types)) types <- rep(NA_character_, length(files))

    out <- list()
    for (i in seq_along(files)) {
        paths <- job_json_file_values(files[[i]])
        if (!length(paths)) next

        for (j in seq_along(paths)) {
            out[[length(out) + 1L]] <- job_json_file_record(
                type = types[[i]],
                index = if (length(paths) > 1L) j else NULL,
                path = paths[[j]],
                output_dir = output_dir,
                base = base,
                relative = relative,
                hash = hash
            )
        }
    }

    out
}

job_json_file_values <- function(x) {
    if (is.null(x)) return(character())
    if (is.list(x) && !is.data.frame(x)) return(unlist(x, use.names = FALSE))
    as.character(x)
}

job_json_file_record <- function(type, index, path, output_dir, base, relative, hash) {
    type <- job_json_scalar_or_null(type)
    path <- job_json_scalar_character(path)

    if (is.null(path) || is.na(path) || !nzchar(path)) {
        return(list(
            type = type,
            index = index,
            path = NULL,
            exists = FALSE,
            size = NULL,
            mtime = NULL,
            hash_algorithm = NULL,
            hash = NULL
        ))
    }

    path <- job_json_result_file_path(path, output_dir)
    exists <- file.exists(path)
    info <- if (exists) file.info(path) else NULL
    digest <- if (exists && hash) job_json_file_hash(path) else NULL

    list(
        type = type,
        index = index,
        path = job_json_encode_path(path, base, relative),
        exists = exists,
        size = if (exists) as.numeric(info$size) else NULL,
        mtime = if (exists) job_json_format_time(info$mtime) else NULL,
        hash_algorithm = if (is.null(digest)) NULL else digest$algorithm,
        hash = if (is.null(digest)) NULL else digest$value
    )
}

job_json_result_file_path <- function(path, output_dir) {
    if (!job_json_is_abs_path(path)) {
        output_dir <- job_json_scalar_character(output_dir)
        if (!is.null(output_dir) && !is.na(output_dir) && nzchar(output_dir)) {
            path <- file.path(output_dir, path)
        }
    }

    normalizePath(path, mustWork = FALSE)
}

job_json_file_hash <- function(path, algorithm = NULL) {
    if (is.null(algorithm)) {
        algorithm <- if (exists("sha256sum", envir = asNamespace("tools"), inherits = FALSE)) {
            "sha256"
        } else {
            "md5"
        }
    }

    if (identical(algorithm, "sha256")) {
        return(list(algorithm = "sha256", value = unname(tools::sha256sum(path))))
    }

    list(algorithm = "md5", value = unname(tools::md5sum(path)))
}

job_json_encode_table <- function(x) {
    if (is.null(x)) return(NULL)
    if (!inherits(x, "data.table")) x <- as.data.table(x)
    if (!nrow(x)) return(list())

    rows <- vector("list", nrow(x))
    for (i in seq_len(nrow(x))) {
        row <- vector("list", length(x))
        names(row) <- names(x)
        for (nm in names(x)) {
            col <- x[[nm]]
            if (is.list(col) && !inherits(col, "POSIXt")) {
                row[[nm]] <- job_json_encode_value(col[[i]])
            } else {
                row[[nm]] <- job_json_encode_value(col[i])
            }
        }
        rows[[i]] <- row
    }

    rows
}

job_json_encode_value <- function(x) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "POSIXt")) return(job_json_format_time(x))
    if (inherits(x, "numeric_version")) return(as.character(x))
    if (inherits(x, "data.table") || inherits(x, "data.frame")) return(job_json_encode_table(x))
    if (is.list(x)) return(lapply(x, job_json_encode_value))
    if (is.character(x)) return(enc2utf8(x))
    if (is.integer(x)) return(as.integer(x))
    if (is.numeric(x)) return(as.numeric(x))
    if (is.logical(x)) return(as.logical(x))
    x
}

job_json_decode_model_store <- function(x, base) {
    models <- job_json_decode_table(x$models)
    if (nrow(models)) {
        set(models, NULL, "model_id", as.integer(models$model_id))
        set(models, NULL, "source_path",
            job_json_decode_path_vector(models$source_path, base))
        set(models, NULL, "prepared_path",
            job_json_decode_path_vector(models$prepared_path, base))
        set(models, NULL, "sql", as.logical(models$sql))
        set(models, NULL, "dict", as.logical(models$dict))
        set(models, NULL, "size", as.numeric(models$size))
        set(models, NULL, "mtime", job_json_parse_time(models$mtime))
    }

    cases <- job_json_decode_table(x$cases)
    if (nrow(cases)) {
        set(cases, NULL, "case_index", as.integer(cases$case_index))
        set(cases, NULL, "model_id", as.integer(cases$model_id))
        set(cases, NULL, "run_path",
            job_json_decode_path_vector(cases$run_path, base))
    }

    JobModelStore$new(
        models = models,
        cases = cases,
        seed_model_id = job_json_scalar_integer(x$seed_model_id, default = NULL),
        cases_valid = job_json_scalar_logical(x$cases_valid, default = TRUE),
        invalid_reason = job_json_scalar_character(x$invalid_reason)
    )
}

job_json_restore_eplus_job <- function(manifest, base) {
    inputs <- manifest$inputs
    store <- job_json_decode_model_store(inputs$model_store, base)
    epw <- job_json_decode_path_nullable(inputs$epw, base)
    job <- eplus_job(
        store$first_prepared_path(),
        epw
    )

    private <- get_priv_env(job)
    private$m_model_store <- store
    private$m_epw_path <- epw
    job_json_restore_eplus_log(private, manifest$log)
    private$m_job <- job_json_decode_energyplus_result(manifest$run, base)

    job
}

job_json_restore_group_job <- function(manifest, base) {
    inputs <- manifest$inputs
    epws <- job_json_decode_epws(inputs$epws, base)
    store <- job_json_decode_model_store(inputs$model_store, base)

    first_epw <- if (length(epws)) epws[[1L]] else NULL
    job <- group_job(store$first_prepared_path(), first_epw)
    private <- get_priv_env(job)
    private$m_model_store <- store
    private$m_epws_path <- if (length(epws)) {
        vcapply(epws, function(epw) epw %||% NA_character_)
    } else {
        NULL
    }

    job_json_restore_group_log(private, manifest$log)
    private$m_job <- job_json_decode_group_run(manifest$run, base)

    job
}

job_json_restore_parametric_job <- function(manifest, base) {
    inputs <- manifest$inputs
    store <- job_json_decode_model_store(inputs$model_store, base)
    weather <- job_json_decode_path_nullable(inputs$weather, base)
    job <- param_job(
        store$model_prepared_path(store$seed_model_id()),
        weather
    )

    private <- get_priv_env(job)
    private$m_model_store <- store
    private$m_epws_path <- weather

    job_json_restore_parametric_log(private, manifest$log, manifest$parametric)
    private$m_job <- job_json_decode_group_run(manifest$run, base)

    job
}

job_json_restore_eplus_log <- function(private, log) {
    private$log_seed_uuid()
    job_json_restore_common_log(private$m_log, log)
}

job_json_restore_group_log <- function(private, log) {
    private$log_idf_uuid()
    job_json_restore_common_log(private$m_log, log)
}

job_json_restore_parametric_log <- function(private, log, parametric) {
    private$log_seed_uuid()
    if (private$m_model_store$has_cases()) private$log_idf_uuid()
    job_json_restore_common_log(private$m_log, log)

    if (!is.null(parametric)) {
        private$m_log$simple <- job_json_scalar_logical(parametric$simple, default = NULL)
        private$m_log$measure_name <- job_json_scalar_character(parametric$measure_name)
        private$m_log$bare <- job_json_scalar_logical(parametric$bare, default = NULL)
        private$m_log$params <- job_json_decode_param_table(parametric$params)
    }
}

job_json_restore_common_log <- function(log_env, log) {
    if (!is.null(log$uuid)) log_env$uuid <- job_json_scalar_character(log$uuid)
    if (!is.null(log$seed_uuid)) log_env$seed_uuid <- job_json_scalar_character(log$seed_uuid)
    if (!is.null(log$idf_uuid)) log_env$idf_uuid <- job_json_decode_character_vector(log$idf_uuid)
    if (!is.null(log$unsaved)) log_env$unsaved <- job_json_decode_value(log$unsaved)

    start <- job_json_parse_time(log$start_time)
    end <- job_json_parse_time(log$end_time)
    if (!is.null(start)) log_env$start_time <- start
    if (!is.null(end)) log_env$end_time <- end
    if (!is.null(log$killed)) log_env$killed <- job_json_scalar_logical(log$killed)
}

job_json_decode_group_run <- function(x, base) {
    if (is.null(x)) return(NULL)
    list(
        jobs = job_json_decode_group_jobs(x$jobs, base),
        options = x$options %||% list()
    )
}

job_json_decode_group_jobs <- function(rows, base) {
    if (is.null(rows) || !length(rows)) return(data.table())

    n <- length(rows)
    dt <- data.table(
        index = viapply(rows, function(row) job_json_scalar_integer(row$index)),
        status = vcapply(rows, function(row) job_json_scalar_character(row$status) %||% NA_character_),
        model = vcapply(rows, function(row) job_json_decode_path(row$model, base)),
        output_dir = vcapply(rows, function(row) job_json_decode_path(row$output_dir, base)),
        energyplus_exe = vcapply(rows, function(row) job_json_decode_path(row$energyplus_exe, base)),
        annual = vlapply(rows, function(row) job_json_scalar_logical(row$annual, default = FALSE)),
        design_day = vlapply(rows, function(row) job_json_scalar_logical(row$design_day, default = FALSE)),
        exit_status = viapply(rows, function(row) job_json_scalar_integer(row$exit_status, default = NA_integer_)),
        start_time = job_json_parse_time(lapply(rows, "[[", "start_time")),
        end_time = job_json_parse_time(lapply(rows, "[[", "end_time"))
    )

    weather <- lapply(rows, function(row) job_json_decode_path_nullable(row$weather, base))
    resources <- lapply(rows, function(row) job_json_decode_path_vector(row$resources, base, null.ok = TRUE))
    process <- replicate(n, NULL, simplify = FALSE)
    stdout <- lapply(rows, function(row) job_json_decode_character_vector(row$stdout))
    stderr <- lapply(rows, function(row) job_json_decode_character_vector(row$stderr))
    result <- lapply(rows, function(row) job_json_decode_energyplus_result(row$result, base))

    dt[, weather := list(weather)]
    dt[, resources := list(resources)]
    dt[, process := list(process)]
    dt[, stdout := list(stdout)]
    dt[, stderr := list(stderr)]
    dt[, result := list(result)]

    setcolorder(dt, c("index", "status", "model", "weather", "output_dir",
        "energyplus_exe", "annual", "design_day", "resources", "process",
        "exit_status", "result", "start_time", "end_time", "stdout", "stderr"))

    dt
}

job_json_decode_energyplus_result <- function(x, base) {
    if (is.null(x)) return(NULL)
    list(
        version = numeric_version(job_json_scalar_character(x$version)),
        energyplus = job_json_decode_path(x$energyplus, base),
        start_time = job_json_parse_time(x$start_time),
        end_time = job_json_parse_time(x$end_time),
        exit_status = job_json_scalar_integer(x$exit_status, default = NA_integer_),
        output_dir = job_json_decode_path(x$output_dir, base),
        file = job_json_decode_file_list(x$file),
        file_info = job_json_decode_file_info(x$file_info, base),
        run = job_json_decode_run_table(x$run)
    )
}

job_json_decode_file_list <- function(x) {
    if (is.null(x)) return(NULL)
    lapply(x, function(value) {
        if (is.null(value)) return(NA_character_)
        if (is.list(value) && !is.data.frame(value)) {
            if (!length(value)) return(character())
            return(vcapply(value, function(i) job_json_scalar_character(i) %||% NA_character_))
        }
        as.character(value)
    })
}

job_json_decode_file_info <- function(rows, base) {
    if (is.null(rows)) return(NULL)

    lapply(rows, function(row) {
        list(
            type = job_json_scalar_character(row$type),
            index = job_json_scalar_integer(row$index, default = NA_integer_),
            path = job_json_decode_path_nullable(row$path, base),
            exists = job_json_scalar_logical(row$exists, default = FALSE),
            size = job_json_scalar_numeric(row$size),
            mtime = job_json_parse_time(row$mtime),
            hash_algorithm = job_json_scalar_character(row$hash_algorithm),
            hash = job_json_scalar_character(row$hash)
        )
    })
}

job_json_decode_run_table <- function(rows) {
    dt <- job_json_decode_table(rows)
    for (col in c("start_time", "end_time")) {
        if (col %in% names(dt)) set(dt, NULL, col, job_json_parse_time(dt[[col]]))
    }
    dt
}

job_json_decode_param_table <- function(rows) {
    dt <- job_json_decode_table(rows)
    if (!nrow(dt)) return(dt)

    int_cols <- c("param_index", "case_index", "object_id", "field_id",
        "field_index", "value_id", "index")
    for (col in intersect(int_cols, names(dt))) {
        set(dt, NULL, col, as.integer(dt[[col]]))
    }

    if ("value_num" %in% names(dt)) {
        set(dt, NULL, "value_num", as.numeric(dt$value_num))
    }

    dt
}

job_json_decode_table <- function(rows) {
    if (is.null(rows) || !length(rows)) return(data.table())

    rows <- lapply(rows, as.list)
    cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
    out <- data.table(.job_json_row = seq_along(rows))

    for (col in cols) {
        vals <- lapply(rows, function(row) {
            if (col %in% names(row)) row[[col]] else NA
        })

        scalar <- vlapply(vals, job_json_is_scalar)
        if (all(scalar)) {
            vec <- unlist(lapply(vals, function(x) if (is.null(x)) NA else x), use.names = FALSE)
            if (length(vec) == length(rows)) {
                set(out, NULL, col, vec)
            } else {
                set(out, NULL, col, vals)
            }
        } else {
            set(out, NULL, col, vals)
        }
    }

    set(out, NULL, ".job_json_row", NULL)
    out
}

job_json_is_scalar <- function(x) {
    is.null(x) || (is.atomic(x) && length(x) <= 1L)
}

job_json_encode_path <- function(path, base, relative) {
    if (is.null(path)) return(NULL)
    path <- as.character(path)
    out <- rep(NA_character_, length(path))
    ok <- !is.na(path)
    out[ok] <- normalizePath(path[ok], mustWork = FALSE)

    if (relative) {
        out[ok] <- vcapply(out[ok], job_json_rel_path, base = base)
    }

    if (length(out) == 1L) out[[1L]] else out
}

job_json_decode_path <- function(path, base) {
    path <- job_json_scalar_character(path)
    if (is.null(path) || is.na(path)) return(NA_character_)
    if (!job_json_is_abs_path(path)) path <- file.path(base, path)
    normalizePath(path, mustWork = FALSE)
}

job_json_decode_path_nullable <- function(path, base) {
    if (is.null(path)) return(NULL)
    out <- job_json_decode_path(path, base)
    if (is.na(out)) NULL else out
}

job_json_decode_path_vector <- function(path, base, null.ok = FALSE) {
    if (is.null(path)) {
        if (null.ok) return(NULL)
        return(character())
    }

    if (is.list(path) && !is.data.frame(path)) {
        return(vcapply(path, function(p) {
            out <- job_json_decode_path(p, base)
            if (is.na(out)) NA_character_ else out
        }))
    }

    vcapply(as.character(path), job_json_decode_path, base = base)
}

job_json_decode_epws <- function(path, base) {
    if (is.null(path)) return(NULL)
    if (!is.list(path) || is.data.frame(path)) {
        path <- as.list(path)
    }

    lapply(path, job_json_decode_path_nullable, base = base)
}

job_json_rel_path <- function(path, base) {
    base <- normalizePath(base, mustWork = FALSE)
    path <- normalizePath(path, mustWork = FALSE)
    prefix <- paste0(base, .Platform$file.sep)

    if (identical(path, base)) {
        "."
    } else if (startsWith(path, prefix)) {
        substring(path, nchar(prefix) + 1L)
    } else {
        path
    }
}

job_json_is_abs_path <- function(path) {
    grepl("^(/|[A-Za-z]:[/\\\\]|[/\\\\]{2})", path)
}

job_json_format_time <- function(x) {
    if (is.null(x)) return(NULL)
    x <- as.POSIXct(x, origin = "1970-01-01")
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x)
    out[ok] <- format(x[ok], "%Y-%m-%dT%H:%M:%OS3%z")
    if (length(out) == 1L) out[[1L]] else out
}

job_json_parse_time <- function(x) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "POSIXt")) return(as.POSIXct(x))
    if (is.list(x)) x <- vcapply(x, function(i) job_json_scalar_character(i) %||% NA_character_)
    x <- as.character(x)
    out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01")
    ok <- !is.na(x) & nzchar(x)
    if (any(ok)) {
        out[ok] <- as.POSIXct(strptime(x[ok], "%Y-%m-%dT%H:%M:%OS%z", tz = ""))
    }
    if (length(out) == 1L) out[[1L]] else out
}

job_json_decode_value <- function(x) {
    if (is.null(x)) return(NULL)
    if (!is.list(x) || is.data.frame(x)) return(x)
    lapply(x, job_json_decode_value)
}

job_json_decode_character_vector <- function(x) {
    if (is.null(x)) return(character())
    if (is.list(x)) return(vcapply(x, function(i) job_json_scalar_character(i) %||% NA_character_))
    as.character(x)
}

job_json_decode_optional_character <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.list(x)) return(vcapply(x, function(i) job_json_scalar_character(i) %||% NA_character_))
    as.character(x)
}

job_json_scalar_character <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.list(x)) {
        if (!length(x)) return(NULL)
        x <- x[[1L]]
    }
    as.character(x)[[1L]]
}

job_json_scalar_integer <- function(x, default = NA_integer_) {
    if (is.null(x)) return(default)
    suppressWarnings(as.integer(job_json_scalar_character(x)))
}

job_json_scalar_numeric <- function(x, default = NULL) {
    if (is.null(x)) return(default)
    suppressWarnings(as.numeric(job_json_scalar_character(x)))
}

job_json_scalar_logical <- function(x, default = NA) {
    if (is.null(x)) return(default)
    as.logical(x)[[1L]]
}

job_json_scalar_or_null <- function(x) {
    if (length(x) == 0L || is.null(x) || (length(x) == 1L && is.na(x))) NULL else x
}

job_json_env_get <- function(env, name, default = NULL) {
    if (exists(name, envir = env, inherits = FALSE)) {
        get(name, envir = env, inherits = FALSE)
    } else {
        default
    }
}

job_json_get <- function(x, name, default = NULL) {
    if (!is.null(x) && name %in% names(x)) x[[name]] else default
}

# vim: set fdm=marker:
