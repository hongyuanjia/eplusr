#' @importFrom R6 R6Class
#' @importFrom data.table data.table rbindlist set setDT
NULL

JobModelStore <- R6::R6Class(classname = "JobModelStore", cloneable = FALSE,
    public = list(
        initialize = function(root = NULL, models = NULL, cases = NULL,
                              seed_model_id = NULL, cases_valid = TRUE,
                              invalid_reason = NULL) {
            private$m_root <- normalizePath(root %||% tempfile("eplusr-job-model-store-"),
                mustWork = FALSE)
            private$m_model_dir <- file.path(private$m_root, "models")
            if (!dir.exists(private$m_model_dir)) {
                dir.create(private$m_model_dir, recursive = TRUE, showWarnings = FALSE)
            }

            private$m_models <- job_model_store_models(models)
            private$m_cases <- job_model_store_cases(cases)
            private$m_seed_model_id <- seed_model_id
            private$m_cases_valid <- isTRUE(cases_valid)
            private$m_invalid_reason <- invalid_reason

            if (nrow(private$m_models)) {
                private$m_next_model_id <- max(private$m_models$model_id) + 1L
            }
        },

        add_input_model = function(idf, role = "input", name = NULL, sql = TRUE, dict = TRUE) {
            idf <- get_init_idf(idf, sql = sql, dict = dict)
            self$add_model(idf, role = role, name = name, source_path = idf$path(),
                sql = isTRUE(attr(idf, "sql")), dict = isTRUE(attr(idf, "dict")))
        },

        add_model = function(idf, role = "input", name = NULL, source_path = NULL,
                             sql = FALSE, dict = FALSE) {
            if (!is_idf(idf)) abort("'idf' must be an Idf object.", "job_model_store")

            model_id <- private$m_next_model_id
            private$m_next_model_id <- private$m_next_model_id + 1L

            source_path <- job_model_store_path(source_path)
            file_name <- job_model_store_filename(name, source_path, idf$path(), model_id)
            model_dir <- file.path(private$m_model_dir, sprintf("%04d", model_id))
            if (!dir.exists(model_dir)) {
                dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
            }
            prepared_path <- normalizePath(file.path(model_dir, file_name), mustWork = FALSE)
            prepared_path <- idf$save(prepared_path, overwrite = TRUE, copy_external = FALSE)

            sig <- job_model_store_file_signature(prepared_path)
            private$m_models <- rbindlist(list(private$m_models, data.table(
                model_id = model_id,
                role = role,
                name = job_model_store_scalar(name),
                source_path = source_path,
                prepared_path = prepared_path,
                version = as.character(idf$version()),
                sql = isTRUE(sql),
                dict = isTRUE(dict),
                signature = sig$hash,
                size = sig$size,
                mtime = sig$mtime
            )), fill = TRUE)

            model_id
        },

        set_seed = function(idf) {
            had_cases <- nrow(private$m_cases) > 0L
            private$m_seed_model_id <- self$add_input_model(idf, role = "seed",
                sql = TRUE, dict = TRUE)
            if (had_cases) {
                self$invalidate_cases("The seed model has been replaced. Regenerate parametric models before running or saving them.")
            }
            invisible(private$m_seed_model_id)
        },

        seed_model_id = function() {
            private$m_seed_model_id
        },

        set_cases = function(model_ids, names = NULL) {
            model_ids <- as.integer(model_ids)
            if (!length(model_ids)) {
                private$m_cases <- job_model_store_cases()
                private$m_cases_valid <- TRUE
                private$m_invalid_reason <- NULL
                return(invisible(self))
            }

            if (any(!model_ids %in% private$m_models$model_id)) {
                abort("Unknown model id found when setting job cases.", "job_model_store")
            }

            if (!is.null(names)) {
                assert_character(names, any.missing = FALSE)
                assert_same_len(model_ids, names)
                names <- as.character(names)
            } else {
                names <- rep(NA_character_, length(model_ids))
            }

            private$m_cases <- data.table(
                case_index = seq_along(model_ids),
                model_id = model_ids,
                name = names,
                run_path = rep(NA_character_, length(model_ids))
            )
            private$m_cases_valid <- TRUE
            private$m_invalid_reason <- NULL
            invisible(self)
        },

        invalidate_cases = function(reason) {
            private$m_cases_valid <- FALSE
            private$m_invalid_reason <- reason
            invisible(self)
        },

        assert_cases_valid = function() {
            if (!private$m_cases_valid) {
                abort(private$m_invalid_reason %||%
                    "Parametric models are stale. Regenerate them before running or saving.",
                    "param_models_stale")
            }
            invisible(TRUE)
        },

        cases_valid = function() {
            private$m_cases_valid
        },

        invalid_reason = function() {
            private$m_invalid_reason
        },

        has_cases = function() {
            nrow(private$m_cases) > 0L
        },

        case_count = function() {
            nrow(private$m_cases)
        },

        case_has_names = function() {
            nrow(private$m_cases) && all(!is.na(private$m_cases$name) & nzchar(private$m_cases$name))
        },

        case_names = function() {
            if (!nrow(private$m_cases)) return(character())
            nms <- private$m_cases$name
            miss <- is.na(nms) | !nzchar(nms)
            if (any(miss)) {
                paths <- self$case_source_paths()
                paths[is.na(paths)] <- self$case_model_paths()[is.na(paths)]
                nms[miss] <- tools::file_path_sans_ext(basename(paths[miss]))
            }
            nms
        },

        rename_cases = function(names) {
            self$assert_cases_valid()
            assert_character(names, any.missing = FALSE, min.len = 1L)

            n <- nrow(private$m_cases)
            if (length(names) == 1L && n > 1L) {
                names <- paste0(names, "_", lpad(seq_len(n), "0"))
            } else if (length(names) != n) {
                abort(paste(
                    "Invalid parametric model names found.",
                    n, "models created but", length(names), "new names given"
                ), "param_names")
            }

            set(private$m_cases, NULL, "name", make.unique(names, sep = "_"))
            invisible(self)
        },

        load_model = function(model_id) {
            row <- private$model_row(model_id)
            read_idf(row$prepared_path)
        },

        load_seed = function() {
            if (is.null(private$m_seed_model_id)) {
                abort("No seed model has been stored.", "job_model_store")
            }
            self$load_model(private$m_seed_model_id)
        },

        load_cases = function() {
            self$assert_cases_valid()
            if (!nrow(private$m_cases)) return(NULL)

            out <- lapply(private$m_cases$model_id, self$load_model)
            names(out) <- self$case_names()
            out
        },

        model_source_path = function(model_id) {
            private$model_row(model_id)$source_path
        },

        model_prepared_path = function(model_id) {
            private$model_row(model_id)$prepared_path
        },

        model_version = function(model_id) {
            numeric_version(private$model_row(model_id)$version)
        },

        model_signature = function(model_id) {
            private$model_row(model_id)$signature
        },

        model_has_design_day = function(model_id) {
            self$load_model(model_id)$is_valid_class("SizingPeriod:DesignDay")
        },

        case_model_ids = function() {
            private$m_cases$model_id
        },

        case_source_paths = function() {
            private$case_model_field("source_path")
        },

        case_model_paths = function() {
            private$case_model_field("prepared_path")
        },

        case_versions = function() {
            private$case_model_field("version")
        },

        case_signatures = function() {
            sig <- private$case_model_field("signature")
            names(sig) <- self$case_names()
            sig
        },

        case_run_paths = function() {
            private$m_cases$run_path
        },

        default_dir = function(seed = FALSE) {
            if (seed && !is.null(private$m_seed_model_id)) {
                path <- self$model_source_path(private$m_seed_model_id)
                if (is.na(path)) path <- self$model_prepared_path(private$m_seed_model_id)
                return(dirname(path))
            }

            if (nrow(private$m_cases)) {
                paths <- self$case_source_paths()
                paths[is.na(paths)] <- self$case_model_paths()[is.na(paths)]
                return(dirname(paths[[1L]]))
            }

            if (!is.null(private$m_seed_model_id)) {
                path <- self$model_source_path(private$m_seed_model_id)
                if (is.na(path)) path <- self$model_prepared_path(private$m_seed_model_id)
                return(dirname(path))
            }

            private$m_root
        },

        materialize_cases = function(paths, copy_external = FALSE) {
            self$assert_cases_valid()
            assert_character(paths, any.missing = FALSE)
            assert_same_len(paths, private$m_cases$model_id)

            paths <- normalizePath(paths, mustWork = FALSE)
            resources <- vector("list", length(paths))
            done <- character()

            for (i in seq_along(paths)) {
                dir <- dirname(paths[[i]])
                if (!dir.exists(dir)) {
                    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
                }

                if (paths[[i]] %in% done) next
                model_id <- private$m_cases$model_id[[i]]

                if (copy_external) {
                    idf <- self$load_model(model_id)
                    idf$save(paths[[i]], overwrite = TRUE, copy_external = TRUE)
                    deps <- idf$external_deps()
                    resources[[i]] <- if (length(deps)) deps else NULL
                } else {
                    file_copy(self$model_prepared_path(model_id), paths[[i]])
                }
                done <- c(done, paths[[i]])
            }

            set(private$m_cases, NULL, "run_path", paths)
            list(paths = paths, resources = resources)
        },

        snapshot = function(dir, overwrite = FALSE) {
            dir <- normalizePath(dir, mustWork = FALSE)
            if (dir.exists(dir) && length(list.files(dir, all.files = TRUE, no.. = TRUE)) && !overwrite) {
                abort(sprintf("Job model snapshot directory already exists: %s", surround(dir)),
                    "job_json_file_exists")
            }
            if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE)) {
                abort(sprintf("Failed to create job model snapshot directory: %s", surround(dir)),
                    "job_json_dir")
            }

            models <- data.table::copy(private$m_models)
            for (i in seq_len(nrow(models))) {
                src <- models$prepared_path[[i]]
                dest_dir <- file.path(dir, "models", sprintf("%04d", models$model_id[[i]]))
                if (!dir.exists(dest_dir)) {
                    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
                }
                dest <- normalizePath(file.path(dest_dir, basename(src)), mustWork = FALSE)
                file_copy(src, dest)
                sig <- job_model_store_file_signature(dest)
                models$prepared_path[[i]] <- dest
                models$signature[[i]] <- sig$hash
                models$size[[i]] <- sig$size
                models$mtime[[i]] <- sig$mtime
            }

            list(
                seed_model_id = private$m_seed_model_id,
                cases_valid = private$m_cases_valid,
                invalid_reason = private$m_invalid_reason,
                models = models,
                cases = data.table::copy(private$m_cases)
            )
        },

        first_prepared_path = function() {
            if (nrow(private$m_cases)) return(self$case_model_paths()[[1L]])
            if (!is.null(private$m_seed_model_id)) return(self$model_prepared_path(private$m_seed_model_id))
            private$m_models$prepared_path[[1L]]
        },

        models_table = function() {
            data.table::copy(private$m_models)
        },

        cases_table = function() {
            data.table::copy(private$m_cases)
        }
    ),

    private = list(
        m_root = NULL,
        m_model_dir = NULL,
        m_next_model_id = 1L,
        m_models = NULL,
        m_cases = NULL,
        m_seed_model_id = NULL,
        m_cases_valid = TRUE,
        m_invalid_reason = NULL,

        model_row = function(model_id) {
            id <- as.integer(model_id)
            row <- private$m_models[private$m_models$model_id == id]
            if (!nrow(row)) abort("Unknown model id.", "job_model_store")
            row[1L]
        },

        case_model_field = function(field) {
            if (!nrow(private$m_cases)) return(character())
            private$m_models[match(private$m_cases$model_id, private$m_models$model_id)][[field]]
        }
    )
)

job_model_store_models <- function(x = NULL) {
    if (is.null(x)) {
        return(data.table(
            model_id = integer(),
            role = character(),
            name = character(),
            source_path = character(),
            prepared_path = character(),
            version = character(),
            sql = logical(),
            dict = logical(),
            signature = character(),
            size = numeric(),
            mtime = as.POSIXct(character(), tz = "UTC")
        ))
    }

    x <- setDT(x)
    if (!"mtime" %in% names(x)) set(x, NULL, "mtime", as.POSIXct(NA))
    x
}

job_model_store_cases <- function(x = NULL) {
    if (is.null(x)) {
        return(data.table(
            case_index = integer(),
            model_id = integer(),
            name = character(),
            run_path = character()
        ))
    }

    x <- setDT(x)
    if (!"run_path" %in% names(x)) set(x, NULL, "run_path", NA_character_)
    x
}

job_model_store_path <- function(path) {
    if (is.null(path) || is.na(path)) return(NA_character_)
    normalizePath(path, mustWork = FALSE)
}

job_model_store_scalar <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[[1L]])) return(NA_character_)
    as.character(x[[1L]])
}

job_model_store_filename <- function(name, source_path, idf_path, model_id) {
    if (!is.null(name) && length(name) && !is.na(name[[1L]]) && nzchar(name[[1L]])) {
        stem <- make_filename(as.character(name[[1L]]), unique = FALSE)
        return(paste0(stem, ".idf"))
    }

    path <- source_path
    if (is.na(path)) path <- job_model_store_path(idf_path)
    if (!is.na(path)) return(basename(path))

    sprintf("model_%s.idf", lpad(model_id, "0"))
}

job_model_store_file_signature <- function(path) {
    info <- file.info(path)
    list(
        hash = unname(tools::md5sum(path)),
        size = as.numeric(info$size),
        mtime = as.POSIXct(info$mtime, tz = "UTC")
    )
}
