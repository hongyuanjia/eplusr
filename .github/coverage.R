# Merge unchanged covr counters by integer positions instead of repeated name scans.
coverage__merge <- function(objects) {
    if (!length(objects)) {
        return(NULL)
    }
    result <- objects[[1L]]
    if (any(vapply(objects, function(x) !is.null(x$tests), logical(1L)))) {
        stop("Test-level tallies require covr's original merger.")
    }
    for (object in objects[-1L]) {
        positions <- match(names(object), names(result))
        fresh <- which(is.na(positions))
        if (length(fresh)) {
            result <- c(result, object[fresh])
            positions[fresh] <- length(result) -
                length(fresh) +
                seq_along(fresh)
        }
        common <- which(!seq_along(object) %in% fresh)
        for (i in common) {
            j <- positions[[i]]
            result[[j]]$value <- result[[j]]$value + object[[i]]$value
        }
    }
    result
}

# Cache only integer source coordinates; each process still creates its own counters.
coverage__trace <- function(env, cache_file, timing_dir) {
    ns <- asNamespace("covr")
    original <- get("impute_srcref", ns)
    cache <- new.env(parent = emptyenv())
    if (file.exists(cache_file)) {
        list2env(readRDS(cache_file), cache)
    }
    hits <- 0L
    misses <- 0L
    # Rebuild source references against this process's own original source environment.
    cached <- function(x, parent_ref) {
        if (is.null(parent_ref) || !get("is_conditional_or_loop", ns)(x)) {
            return(NULL)
        }
        source <- attr(parent_ref, "srcfile")
        key <- paste(
            source$filename,
            paste(as.integer(parent_ref), collapse = ","),
            as.character(x[[1L]]),
            length(x),
            sep = "|"
        )
        if (exists(key, cache, inherits = FALSE)) {
            hits <<- hits + 1L
            refs <- get(key, cache, inherits = FALSE)
            if (is.null(refs)) {
                return(NULL)
            }
            return(lapply(refs, function(ref) {
                if (is.null(ref)) NULL else srcref(source, ref)
            }))
        }
        misses <<- misses + 1L
        result <- original(x, parent_ref)
        assign(
            key,
            lapply(result, function(ref) {
                if (is.null(ref)) NULL else as.integer(ref)
            }),
            cache
        )
        if (is.null(result)) {
            assign(key, NULL, cache)
        }
        result
    }
    unlockBinding("impute_srcref", ns)
    assign("impute_srcref", cached, ns)
    lockBinding("impute_srcref", ns)
    on.exit(
        {
            unlockBinding("impute_srcref", ns)
            assign("impute_srcref", original, ns)
            lockBinding("impute_srcref", ns)
        },
        add = TRUE
    )
    started <- proc.time()[["elapsed"]]
    get("trace_environment", ns)(env)
    if (!file.exists(cache_file)) {
        pending <- tempfile("coordinates-", tmpdir = dirname(cache_file))
        saveRDS(as.list(cache), pending, compress = FALSE)
        file.rename(pending, cache_file)
    }
    # Capture the full zero-count denominator once; subsequent traces contain only values.
    metadata <- file.path(dirname(cache_file), "source-metadata.rds")
    if (!file.exists(metadata)) {
        pending <- tempfile("pending-metadata-", tmpdir = dirname(cache_file))
        saveRDS(as.list(get(".counters", ns)), pending, compress = FALSE)
        if (!file.rename(pending, metadata)) {
            stop("Cannot publish coverage source metadata.")
        }
    }
    elapsed <- proc.time()[["elapsed"]] - started
    if (nzchar(timing_dir)) {
        utils::write.csv(
            data.frame(
                pid = Sys.getpid(),
                trace_seconds = elapsed,
                hits = hits,
                misses = misses
            ),
            tempfile("worker-", tmpdir = timing_dir, fileext = ".csv"),
            row.names = FALSE
        )
    }
    # Measure launch through the first paused observation without changing the native lifecycle.
    if (exists("api_session__initialize", env, inherits = FALSE)) {
        original_initialize <- get("api_session__initialize", env)
        initialize <- function(ctx, timeout = ctx$config$timeout) {
            started <- proc.time()[["elapsed"]]
            on.exit(
                utils::write.csv(
                    data.frame(
                        pid = Sys.getpid(),
                        startup_seconds = proc.time()[["elapsed"]] - started
                    ),
                    tempfile("session-", tmpdir = timing_dir, fileext = ".csv"),
                    row.names = FALSE
                ),
                add = TRUE
            )
            original_initialize(ctx, timeout)
        }
        environment(initialize) <- list2env(
            list(
                original_initialize = original_initialize,
                timing_dir = timing_dir
            ),
            parent = baseenv()
        )
        unlockBinding("api_session__initialize", env)
        assign("api_session__initialize", initialize, env)
        lockBinding("api_session__initialize", env)
    }
    invisible(NULL)
}

# Apply the adapter only to this run's temporary package loader and covr bindings.
coverage__run <- function(path = ".", timing_dir, ...) {
    ns <- asNamespace("covr")
    if (utils::packageVersion("covr") != "3.6.5") {
        stop(
            "Review the coverage adapter before using a different covr version."
        )
    }
    original_hooks <- get("add_hooks", ns)
    original_tests <- get("testInstalledPackage", asNamespace("tools"))
    original_merge <- get("merge_coverage.character", ns)
    started <- proc.time()[["elapsed"]]
    dir.create(timing_dir, recursive = TRUE, showWarnings = FALSE)
    timing_dir <- normalizePath(timing_dir, winslash = "/")
    on.exit(coverage__timings(timing_dir), add = TRUE)
    # Record parent stages independently of the child process timing receipts.
    record <- function(stage, seconds, count = NA_integer_) {
        utils::write.csv(
            data.frame(stage = stage, seconds = seconds, count = count),
            tempfile("stage-", tmpdir = timing_dir, fileext = ".csv"),
            row.names = FALSE
        )
        message("Coverage ", stage, " seconds: ", round(seconds, 3L))
    }
    # covr inserts hooks into a fresh install, so coordinates never cross runs or revisions.
    hooks <- function(pkg_name, lib, ...) {
        record("install", proc.time()[["elapsed"]] - started)
        original_hooks(pkg_name, lib, ...)
        loader <- file.path(lib, pkg_name, "R", pkg_name)
        lines <- readLines(loader, warn = FALSE)
        hit <- grep("covr:::trace_environment(ns)", lines, fixed = TRUE)
        if (length(hit) != 1L) {
            stop("The covr loader changed; review the coverage adapter.")
        }
        tracer <- paste(
            deparse(coverage__trace, width.cutoff = 500L),
            collapse = "\n"
        )
        replacement <- paste0(
            "(",
            tracer,
            ")(ns, ",
            encodeString(file.path(lib, "source-coordinates.rds"), quote = '"'),
            ", ",
            encodeString(timing_dir, quote = '"'),
            ")"
        )
        lines[hit] <- sub(
            "covr:::trace_environment(ns)",
            replacement,
            lines[hit],
            fixed = TRUE
        )
        if (!isTRUE(getOption("covr.record_tests", FALSE))) {
            exit_hook <- grep("covr:::save_trace", lines, fixed = TRUE)
            if (length(exit_hook) != 1L) {
                stop("The covr exit hook changed.")
            }
            writer <- paste(
                deparse(coverage__save_trace, width.cutoff = 500L),
                collapse = "
"
            )
            lines[exit_hook] <- sub(
                "covr:::save_trace",
                paste0("(", writer, ")"),
                lines[exit_hook],
                fixed = TRUE
            )
        }
        writeLines(lines, loader)
    }
    # This includes test processes and their engine workers, but excludes trace merging.
    tests <- function(...) {
        before <- proc.time()[["elapsed"]]
        on.exit(record("tests", proc.time()[["elapsed"]] - before), add = TRUE)
        original_tests(...)
    }
    # Stream standard covr files and retain every zero counter and source metadata entry.
    merge <- function(files) {
        if (isTRUE(getOption("covr.record_tests", FALSE))) {
            return(original_merge(files))
        }
        read_seconds <- 0
        merge_seconds <- 0
        result <- NULL
        metadata <- NULL
        for (file in files) {
            before <- proc.time()[["elapsed"]]
            object <- suppressWarnings(readRDS(file))
            if (is.numeric(object)) {
                if (is.null(metadata)) {
                    metadata <- readRDS(file.path(
                        dirname(file),
                        "source-metadata.rds"
                    ))
                    result <- metadata
                    for (i in seq_along(result)) {
                        result[[i]]$value <- 0
                    }
                }
                positions <- match(names(object), names(result))
                if (
                    is.null(names(object)) ||
                        anyNA(object) ||
                        anyNA(positions) ||
                        anyDuplicated(names(object)) ||
                        length(object) != length(result)
                ) {
                    stop(
                        "Worker coverage keys do not match the complete source metadata."
                    )
                }
                read_seconds <- read_seconds + proc.time()[["elapsed"]] - before
                before <- proc.time()[["elapsed"]]
                for (i in seq_along(object)) {
                    j <- positions[[i]]
                    result[[j]]$value <- result[[j]]$value + object[[i]]
                }
                merge_seconds <- merge_seconds +
                    proc.time()[["elapsed"]] -
                    before
                next
            }
            object <- as.list(object)
            read_seconds <- read_seconds + proc.time()[["elapsed"]] - before
            if (!is.null(object$tests)) {
                return(original_merge(files))
            }
            before <- proc.time()[["elapsed"]]
            result <- if (is.null(result)) {
                object
            } else {
                coverage__merge(list(result, object))
            }
            merge_seconds <- merge_seconds + proc.time()[["elapsed"]] - before
        }
        record("trace_read", read_seconds, length(files))
        record("trace_merge", merge_seconds, length(files))
        result
    }
    testthat::local_mocked_bindings(
        add_hooks = hooks,
        merge_coverage.character = merge,
        .package = "covr"
    )
    testthat::local_mocked_bindings(
        testInstalledPackage = tests,
        .package = "tools"
    )
    withr::local_envvar(c(`_EPLUSR_TEST_TIMING_DIR_` = timing_dir))
    covr::package_coverage(path = path, ...)
}

# Emit timing summaries to the CI log; raw timing receipts are not published as artifacts.
coverage__timings <- function(directory) {
    files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
    # Independent files avoid concurrent writes from worker processes.
    read <- function(pattern) {
        paths <- files[grepl(pattern, basename(files))]
        if (!length(paths)) {
            return(NULL)
        }
        rows <- lapply(paths, utils::read.csv)
        columns <- unique(unlist(lapply(rows, names)))
        rows <- lapply(rows, function(row) {
            for (column in setdiff(columns, names(row))) {
                row[[column]] <- NA
            }
            row[columns]
        })
        do.call(rbind, rows)
    }
    stages <- read("^(stage-|tests-.*-phase)")
    if (!is.null(stages)) {
        message("Coverage phase timings (wall seconds; nested phases overlap):")
        print(stages, row.names = FALSE)
    }
    workers <- read("^worker-")
    if (!is.null(workers)) {
        message(
            "Coverage worker tracing: ",
            nrow(workers),
            " namespace loads; summed seconds ",
            round(sum(workers$trace_seconds), 3L),
            "; median/p95/max seconds ",
            paste(
                round(
                    stats::quantile(workers$trace_seconds, c(0.5, 0.95, 1)),
                    3L
                ),
                collapse = "/"
            )
        )
    }
    sessions <- read("^session-")
    if (!is.null(sessions)) {
        message(
            "Coverage session initialization: ",
            nrow(sessions),
            " attempts; median/p95/max seconds ",
            paste(
                round(
                    stats::quantile(sessions$startup_seconds, c(0.5, 0.95, 1)),
                    3L
                ),
                collapse = "/"
            )
        )
    }
    cases <- read("^tests-.*-cases")
    if (!is.null(cases)) {
        message(
            "Coverage test-file summed case seconds (parallel cases can overlap):"
        )
        print(stats::aggregate(real ~ file, cases, sum), row.names = FALSE)
        message("Coverage ten slowest test cases:")
        print(
            utils::head(
                cases[
                    order(cases$real, decreasing = TRUE),
                    c("file", "test", "real")
                ],
                10L
            ),
            row.names = FALSE
        )
    }
    invisible(NULL)
}

# Atomically save every original expression key and count without repeating source environments.
coverage__save_trace <- function(directory) {
    counters <- as.list(get(".counters", asNamespace("covr")))
    values <- vapply(counters, function(counter) counter$value, numeric(1L))
    pending <- tempfile("pending-counts-", tmpdir = directory)
    saveRDS(values, pending, compress = FALSE)
    complete <- tempfile("covr_trace_", tmpdir = directory)
    if (!file.rename(pending, complete)) {
        stop("Cannot publish complete worker coverage counts.")
    }
    invisible(complete)
}

# Require every shard's full denominator, then combine ordinary covr objects for one report.
coverage__combine <- function(files) {
    objects <- lapply(files, readRDS)
    if (length(objects) != 5L) {
        stop("All five coverage shards are required.")
    }
    reference <- objects[[1L]]
    for (object in objects[-1L]) {
        if (!setequal(names(reference), names(object))) {
            stop("Coverage shard expression keys differ.")
        }
        positions <- match(names(reference), names(object))
        for (i in seq_along(reference)) {
            left <- reference[[i]]
            right <- object[[positions[[i]]]]
            if (
                !identical(as.integer(left$srcref), as.integer(right$srcref)) ||
                    !identical(
                        attr(left$srcref, "srcfile")$filename,
                        attr(right$srcref, "srcfile")$filename
                    ) ||
                    !identical(left$functions, right$functions)
            ) {
                stop("Coverage shard source metadata differs.")
            }
        }
    }
    merged <- coverage__merge(objects)
    for (name in setdiff(names(attributes(reference)), c("names", "library"))) {
        attr(merged, name) <- attr(reference, name)
    }
    merged
}
