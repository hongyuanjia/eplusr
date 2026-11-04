test_that("replay prediction validates ownership, phases and bounded history", {
    ctx <- testapi__session_context()
    session <- api_session__construct(ctx)
    on.exit(session$close(), add = TRUE)
    expect_error(session$predict(list(a = list(NULL)), 1L), "recorded paused")
    ctx$config$record <- TRUE
    ctx$phase <- "paused"
    expect_error(session$predict(list(a = list(NULL)), 0L))
    expect_error(session$predict(list(a = list(NULL)), 1L, tolerance = -1))
    expect_error(
        session$predict(list(a = list(NULL)), 1L, timeout = 0),
        "positive"
    )
    expect_error(session$predict(list(), 1L))
    expect_error(session$predict(list(list(NULL)), 1L))
    expect_error(session$predict(setNames(list(list(NULL)), ""), 1L))
    expect_error(session$predict(list(a = list(NULL, NULL)), 1L))
    expect_error(session$predict(list(a = list(c(unknown = 1))), 1L), "Actions")
    expect_error(session$predict(list(a = function(x) x), 1L))
    expected <- list(
        step = 1L,
        values = c(temperature = 20),
        controller_state = NULL
    )
    expect_silent(api_replay__match(expected, expected, 0))
    changed <- expected
    changed$values <- c(temperature = 21)
    expect_error(api_replay__match(changed, expected, 1e-8), "diverged")
    changed <- expected
    changed$step <- 2L
    expect_error(api_replay__match(changed, expected, 1e-8), "diverged")
})

# Forecast actual heating alternatives while the original native stack remains paused.
test_that("stock replay branches match history and preserve the original live session", {
    fixture <- testapi__controlled_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    session <- eplus_session(
        fixture$model,
        eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
        fixture$weather,
        variables = list(
            temperature = c("Zone Mean Air Temperature", "ZONE ONE"),
            heating = c(
                "Zone Thermostat Heating Setpoint Temperature",
                "ZONE ONE"
            )
        ),
        actuators = list(
            heating = c(
                "Schedule:Constant",
                "Schedule Value",
                "Session Heating"
            )
        ),
        record = TRUE
    )
    on.exit(session$close(), add = TRUE)
    session$initialize()
    origin <- session$step(c(heating = 22), n = 3L)
    identity <- session$status()$identity
    context <- environment(session$observe)$ctx
    prediction <- session$predict(
        list(
            hold = rep(list(NULL), 3L),
            warm = rep(list(c(heating = 24)), 3L),
            release = rep(list(c(heating = NA_real_)), 3L)
        ),
        3L
    )
    expect_s3_class(prediction, "EnergyPlusPrediction")
    expect_identical(prediction$method, "replay")
    expect_identical(session$observe(), origin)
    expect_identical(session$status()$identity, identity)
    expect_identical(session$status()$phase, "paused")
    for (branch in prediction$branches) {
        expect_identical(branch$boundary, origin)
        expect_identical(branch$replay_steps, 4L)
        expect_identical(branch$future_steps, 3L)
        expect_true(branch$complete)
        expect_identical(branch$status, "success")
        expect_null(branch$reason)
        expect_false(identical(branch$identity$pid, identity$pid))
        expect_false(tryCatch(
            ps::ps_is_running(ps::ps_handle(branch$identity$pid)),
            error = function(condition) FALSE
        ))
        expect_false(tryCatch(
            ps::ps_is_running(ps::ps_handle(branch$identity$pipeline_pid)),
            error = function(condition) FALSE
        ))
    }
    expect_equal(
        prediction$branches$hold$observations[[3L]]$values[["heating"]],
        22
    )
    expect_equal(
        prediction$branches$warm$observations[[3L]]$values[["heating"]],
        24
    )
    expect_equal(
        prediction$branches$release$observations[[3L]]$values[["heating"]],
        20
    )
    for (index in seq_len(3L)) {
        expect_equal(
            session$step()$values,
            prediction$branches$hold$observations[[index]]$values,
            tolerance = 1e-10
        )
    }
    expect_error(
        session$predict(list(a = list(NULL)), 1L, history_limit = 1),
        "history"
    )
    occupied <- tempfile()
    dir.create(occupied)
    writeLines("occupied", file.path(occupied, "keep"))
    expect_error(
        session$predict(list(a = list(NULL)), 1L, dir = occupied),
        "empty or new"
    )
    before_failures <- session$observe()
    mixed <- session$predict(
        list(
            first = list(NULL),
            fail = function(observation, state) stop("future failure"),
            last = list(NULL)
        ),
        1L
    )
    expect_identical(names(mixed$branches), c("first", "fail", "last"))
    expect_identical(
        vapply(mixed$branches, `[[`, character(1L), "status"),
        c(first = "success", fail = "failed", last = "success")
    )
    expect_match(mixed$branches$fail$reason, "future failure")
    expect_true("error" %in% mixed$branches$fail$error$class)
    expect_false(mixed$branches$fail$complete)
    expect_null(mixed$branches$fail$observations)
    expect_identical(
        mixed$branches$first$observations,
        mixed$branches$last$observations
    )
    expect_identical(session$observe(), before_failures)
    expect_identical(session$status()$identity, identity)
    expect_identical(session$status()$phase, "paused")
    expect_identical(session$step()$step, before_failures$step + 1L)
    slow <- session$predict(
        list(slow = function(observation, state) {
            Sys.sleep(5)
            list(action = NULL, state = state)
        }),
        1L,
        timeout = 0.1
    )
    expect_identical(slow$branches$slow$status, "failed")
    expect_match(slow$branches$slow$reason, "timed out")
    expect_identical(session$status()$phase, "paused")
    expired <- session$predict(list(a = list(NULL)), 1L, timeout = 1e-9)
    expect_identical(expired$branches$a$status, "failed")
    expect_match(expired$branches$a$reason, "branch timed out")
    expect_identical(session$step()$step, before_failures$step + 2L)
    # A native callback interrupt must survive IPC wrapping and stop the candidate loop.
    interrupted <- tryCatch(
        {
            session$predict(
                list(
                    cancel = function(observation, state) {
                        stop(structure(
                            list(message = "cancel branch", call = NULL),
                            class = c("interrupt", "condition")
                        ))
                    },
                    later = list(NULL)
                ),
                1L
            )
            FALSE
        },
        interrupt = function(condition) TRUE
    )
    expect_true(interrupted)
    expect_identical(session$status()$identity, identity)
    expect_identical(session$status()$phase, "paused")
    expect_identical(session$step()$step, before_failures$step + 3L)
    # A mismatch during worker-side replay is caught through the native callback boundary.
    history <- api_replay__history(context, 128 * 1024^2)
    history[[2L]]$observation$values[["temperature"]] <- history[[
        2L
    ]]$observation$values[["temperature"]] +
        5
    expect_error(
        api_replay__branch(
            context,
            history,
            list(NULL),
            1L,
            1e-8,
            context$config$timeout,
            tempfile()
        ),
        "diverged"
    )
    expect_identical(session$status()$identity, identity)
    expect_identical(session$status()$phase, "paused")
    # A future policy must not alter inputs silently for subsequent branches.
    original_bytes <- readBin(
        context$replay$model,
        raw(),
        n = file.info(context$replay$model)$size
    )
    mutator <- local({
        file <- context$replay$model
        # Change only the source snapshot, whose native source state is already parsed.
        function(observation, state) {
            writeLines("changed by a future policy", file)
            list(action = NULL, state = state)
        }
    })
    expect_error(session$predict(list(mutate = mutator), 1L), "inputs.*changed")
    writeBin(original_bytes, context$replay$model)
    expect_identical(session$status()$phase, "paused")
    # A changed snapshot is rejected before touching the original or a branch engine.
    writeLines("changed", context$replay$model)
    expect_error(session$predict(list(a = list(NULL)), 1L), "inputs.*changed")
    expect_identical(session$status()$phase, "paused")
})

# Replay recorded controller memory without re-running the original historical policy.
test_that("stock replay restores explicit controller memory and reports short horizons", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    # Advance only explicit memory so restored history can be counted exactly.
    controller <- function(observation, state) {
        list(action = NULL, state = state + 1L)
    }
    session <- eplus_session(
        fixture$model,
        eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
        fixture$weather,
        controller = controller,
        controller_state = 0L,
        record = TRUE
    )
    on.exit(session$close(), add = TRUE)
    session$initialize()
    origin <- session$step(n = 2L)
    prediction <- session$predict(list(same = controller), 2L)
    expect_identical(prediction$branches$same$boundary, origin)
    expect_identical(
        prediction$branches$same$observations[[2L]]$controller_state,
        4L
    )
    expect_identical(session$observe(), origin)
    session$step(n = 93L)
    short <- session$predict(list(hold = list(NULL, NULL)), 2L)
    expect_false(short$branches$hold$complete)
    expect_identical(short$branches$hold$status, "incomplete")
    expect_match(short$branches$hold$reason, "ended before")
    expect_identical(short$branches$hold$future_steps, 0L)
    expect_identical(session$status()$phase, "paused")
})

# Verify bounded loading and integrity against the last acknowledged native boundary.
test_that("replay journals reject corruption and incomplete historical prefixes", {
    ctx <- testapi__session_context()
    root <- tempfile()
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    input <- file.path(root, "input.txt")
    writeLines("frozen input", input)
    ctx$replay <- list(files = input, fingerprint = tools::md5sum(input))
    ctx$config$journal <- root
    ctx$observation <- list(
        step = 1L,
        values = c(temperature = 20),
        controller_state = NULL
    )
    ctx$journal_hash <- api_replay__write(root, ctx$observation, NULL, NULL)
    expect_length(api_replay__history(ctx, 1024^2), 1L)
    expect_error(api_replay__history(ctx, 0))
    expect_error(api_replay__history(ctx, 1), "history_limit")
    path <- file.path(root, "0000000001.rds")
    original <- readRDS(path)
    changed <- original
    changed$action <- c(heating = 23)
    saveRDS(changed, path, compress = FALSE)
    expect_error(api_replay__history(ctx, 1024^2), "integrity")
    changed$previous_hash <- "altered chain"
    saveRDS(changed, path, compress = FALSE)
    expect_error(api_replay__history(ctx, 1024^2), "integrity")
    changed <- original
    changed$observation$step <- 2L
    saveRDS(changed, path, compress = FALSE)
    expect_error(api_replay__history(ctx, 1024^2), "sequence")
    saveRDS(original, path, compress = FALSE)
    ctx$observation$values[[1L]] <- 21
    expect_error(api_replay__history(ctx, 1024^2), "paused boundary")
    ctx$observation$values[[1L]] <- 20
    ctx$observation$step <- 2L
    expect_error(api_replay__history(ctx, 1024^2), "missing")
    # A nonempty directory at the destination forces a real atomic rename failure.
    blocked <- file.path(root, "0000000002.rds")
    dir.create(blocked)
    writeLines("keep", file.path(blocked, "keep"))
    expect_error(
        suppressWarnings(api_replay__write(root, ctx$observation, NULL, NULL)),
        "commit"
    )
})

# Keep weather/model reserved names separate from hidden or same-named declared resources.
test_that("recorded input snapshots preserve hidden resources and reject ambiguous names", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    resources <- file.path(
        fixture$directory,
        c("weather.epw", ".schedule.csv", "replay-1.idf")
    )
    for (resource in resources) {
        writeLines(rep("1", 8760), resource)
    }
    additions <- sprintf(
        "Schedule:File,Frozen %d,,%s,1,0,8760,Comma,No,60;",
        seq_along(resources),
        resources
    )
    cat(
        "\n",
        paste(additions, collapse = "\n"),
        "\n",
        file = fixture$model,
        append = TRUE
    )
    session <- eplus_session(
        fixture$model,
        eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
        fixture$weather,
        record = TRUE
    )
    on.exit(session$close(), add = TRUE)
    ctx <- environment(session$observe)$ctx
    expect_identical(basename(ctx$replay$model), "replay-2.idf")
    copies <- file.path(dirname(ctx$replay$model), basename(resources))
    expect_equal(
        unname(tools::md5sum(copies)),
        unname(tools::md5sum(resources))
    )
    expect_equal(
        unname(tools::md5sum(ctx$replay$weather)),
        unname(tools::md5sum(fixture$weather))
    )
    # Windows may spell the same materialized path with different separators.
    expect_true(
        normalizePath(copies[[2L]], winslash = "/") %in%
            normalizePath(ctx$replay$files, winslash = "/")
    )
    session$initialize()
    writeLines("changed hidden schedule", copies[[2L]])
    expect_error(session$predict(list(a = list(NULL)), 1L), "inputs.*changed")
    expect_identical(session$status()$phase, "paused")
    # Distinct original directories may not silently flatten to one file.
    directories <- file.path(fixture$directory, c("one", "two"))
    for (directory in directories) {
        dir.create(directory)
    }
    duplicates <- file.path(directories, "same.csv")
    for (resource in duplicates) {
        writeLines(rep("1", 8760), resource)
    }
    cat(
        "\n",
        paste(
            sprintf(
                "Schedule:File,Duplicate %d,,%s,1,0,8760,Comma,No,60;",
                1:2,
                duplicates
            ),
            collapse = "\n"
        ),
        "\n",
        file = fixture$model,
        append = TRUE
    )
    expect_error(
        eplus_session(
            fixture$model,
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
            fixture$weather,
            record = TRUE
        ),
        "distinct basenames"
    )
    missing <- file.path(fixture$directory, "missing.csv")
    cat(
        "\n",
        sprintf("Schedule:File,Missing,,%s,1,0,8760,Comma,No,60;", missing),
        "\n",
        file = fixture$model,
        append = TRUE
    )
    expect_error(
        suppressWarnings(eplus_session(
            fixture$model,
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
            fixture$weather,
            record = TRUE
        )),
        "all declared"
    )
})


# Absolute tolerance is checked per output, even when many other outputs match exactly.
test_that("replay comparison does not average away one divergent output", {
    expected <- list(
        step = 1L,
        values = setNames(rep(0, 200), paste0("v", 1:200))
    )
    actual <- expected
    actual$values[[1L]] <- 1e-6
    expect_error(api_replay__match(actual, expected, 1e-8), "diverged")
    actual$values[[1L]] <- 1e-9
    expect_silent(api_replay__match(actual, expected, 1e-8))
})

# Do not publish results for a boundary which changed during orchestration.
test_that("replay prediction refuses a changed source boundary", {
    ctx <- testapi__session_context()
    session <- api_session__construct(ctx)
    on.exit(session$close(), add = TRUE)
    ctx$config$record <- TRUE
    ctx$phase <- "paused"
    ctx$identity <- list(pid = 1L)
    ctx$observation <- list(step = 1L)
    # Isolate orchestration from the engine to exercise a concurrent source change.
    local_mocked_bindings(
        api_replay__history = function(...) {
            list(list(observation = ctx$observation, action = NULL))
        },
        api_replay__branch = function(...) {
            ctx$identity <- list(pid = 2L)
            list(method = "replay")
        }
    )
    expect_error(
        session$predict(list(a = list(NULL)), 1L),
        "Original session changed"
    )
})

# A terminal future boundary keeps its observation and stops requesting subsequent steps.
test_that("replay branches stop at a completed future boundary and close", {
    ctx <- list(
        replay = list(model = "model.idf", weather = "weather.epw"),
        config = list(
            installation = "26.1",
            variables = list(),
            actuators = list()
        )
    )
    first <- list(step = 1L, values = numeric(), controller_state = NULL)
    terminal <- first
    terminal$step <- 2L
    phase <- "paused"
    steps <- 0L
    closed <- FALSE
    checked <- 0L
    # Represent a worker that advances once and reports native completion at that boundary.
    branch <- list(
        initialize = function(...) first,
        status = function() {
            list(
                phase = phase,
                identity = list(pid = 1L),
                outcome = "completed"
            )
        },
        step = function(...) {
            steps <<- steps + 1L
            phase <<- "completed"
            terminal
        },
        close = function() closed <<- TRUE
    )
    local_mocked_bindings(
        eplus_session = function(...) branch,
        api_replay__check_inputs = function(...) checked <<- checked + 1L
    )
    result <- api_replay__branch(
        ctx,
        list(list(observation = first, action = NULL)),
        list(NULL, NULL),
        2L,
        1e-8,
        120,
        tempfile()
    )
    expect_identical(result$observations, list(terminal))
    expect_identical(result$boundary, first)
    expect_identical(result$future_steps, 1L)
    expect_false(result$complete)
    expect_identical(result$outcome, "completed")
    expect_identical(steps, 1L)
    expect_true(closed)
    expect_identical(checked, 2L)
})

# Plugin definitions must remain ordinary reusable backend functions, with frozen dependencies.

# Isolate orchestration to cover repeated failures, ordinary metadata and interrupt propagation.
test_that("replay prediction retains failed entries but never swallows caller interrupts", {
    ctx <- testapi__session_context()
    session <- api_session__construct(ctx)
    on.exit(session$close(), add = TRUE)
    ctx$config$record <- TRUE
    ctx$phase <- "paused"
    ctx$observation <- list(step = 1L)
    calls <- 0L
    local_mocked_bindings(
        api_replay__history = function(...) {
            list(list(observation = ctx$observation))
        },
        api_replay__check_inputs = function(...) invisible(NULL),
        api_replay__branch = function(...) {
            calls <<- calls + 1L
            stop(simpleError(if (calls == 1L) "" else "candidate failure"))
        }
    )
    result <- session$predict(list(first = list(NULL), second = list(NULL)), 1L)
    expect_identical(calls, 2L)
    expect_identical(result$branches$first$status, "failed")
    expect_identical(result$branches$first$reason, "Prediction branch failed.")
    expect_identical(result$branches$second$error$message, "candidate failure")
    expect_identical(
        result$branches$second$error$class,
        c("simpleError", "error", "condition")
    )
    expect_gte(result$branches$second$elapsed, 0)
    expect_identical(ctx$phase, "paused")
    calls <- 0L
    # Even a condition that inherits both error and interrupt must terminate prediction.
    local_mocked_bindings(api_replay__branch = function(...) {
        calls <<- calls + 1L
        stop(structure(
            list(message = "cancel prediction", call = NULL),
            class = c("interrupt", "error", "condition")
        ))
    })
    interrupted <- tryCatch(
        {
            session$predict(list(first = list(NULL), later = list(NULL)), 1L)
            FALSE
        },
        interrupt = function(condition) TRUE
    )
    expect_true(interrupted)
    expect_identical(calls, 1L)
    expect_identical(ctx$phase, "paused")
})

# Rebuild deterministic plugin state from recorded controls, then let each policy use its own history.
test_that("stock replay rebuilds independent managed trends before future policies", {
    fixture <- testapi__controlled_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    marker <- file.path(fixture$directory, "primary-policy.txt")
    plugin <- eplus_plugin(
        list(end_zone_timestep_after_zone_reporting = function(
            api,
            state,
            plugin
        ) {
            if (api$exchange$kind_of_sim(state) != 3L) {
                return(NULL)
            }
            handle <- api$exchange$get_variable_handle(
                state,
                "Zone Thermostat Heating Setpoint Temperature",
                "ZONE ONE"
            )
            plugin$data$total <- plugin$data$total +
                api$exchange$get_variable_value(state, handle)
        }),
        data = list(total = 0),
        trends = list(
            total = list(
                source = function(api, state, plugin) plugin$data$total,
                capacity = 3L
            )
        )
    )
    # This side effect belongs only to the primary policy, never its recorded prefix.
    primary <- function(observation, state) {
        cat("decision\n", file = marker, append = TRUE)
        list(action = c(heating = 22), state = list(n = state$n + 1L))
    }
    session <- eplus_session(
        fixture$model,
        fixture$weather,
        eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
        variables = list(
            heating = c(
                "Zone Thermostat Heating Setpoint Temperature",
                "ZONE ONE"
            )
        ),
        actuators = list(
            heating = c(
                "Schedule:Constant",
                "Schedule Value",
                "Session Heating"
            )
        ),
        backend = plugin,
        controller = primary,
        controller_state = list(n = 0L),
        record = TRUE
    )
    on.exit(try(session$close(), silent = TRUE), add = TRUE)
    session$initialize()
    origin <- session$step(n = 2L)
    identity <- session$status()$identity
    expect_identical(origin$plugins[[1L]]$trends$total$values, c(64, 42, 20))
    # Mutating a branch's ordinary observation must never change its plugin's buffer.
    warmer <- function(observation, state) {
        history <- observation$plugins[[1L]]$trends$total$values
        heating <- history[[1L]] - history[[2L]] + 2
        observation$plugins[[1L]]$trends$total$values[] <- -999
        list(action = c(heating = heating), state = list(n = state$n + 1L))
    }
    cooler <- function(observation, state) {
        history <- observation$plugins[[1L]]$trends$total$values
        heating <- history[[1L]] - history[[2L]] - 2
        list(action = c(heating = heating), state = list(n = state$n + 1L))
    }
    prediction <- session$predict(list(warmer = warmer, cooler = cooler), 2L)
    expect_identical(prediction$method, "replay")
    expect_identical(
        vapply(prediction$branches, `[[`, character(1L), "status"),
        c(warmer = "success", cooler = "success")
    )
    for (branch in prediction$branches) {
        expect_identical(branch$boundary, origin)
        expect_identical(branch$replay_steps, 3L)
        expect_identical(
            branch$observations[[2L]]$controller_state,
            list(n = 4L)
        )
        expect_identical(
            branch$observations[[2L]]$plugins[[1L]]$trends$total$valid_count,
            3L
        )
        expect_identical(
            branch$observations[[2L]]$plugins[[1L]]$trends$total$times,
            c(1.25, 1, .75)
        )
        expect_false(identical(branch$identity$pid, identity$pid))
        expect_false(tryCatch(
            ps::ps_is_running(ps::ps_handle(branch$identity$pid)),
            error = function(condition) FALSE
        ))
    }
    expect_false(identical(
        prediction$branches$warmer$identity$pid,
        prediction$branches$cooler$identity$pid
    ))
    expect_identical(
        prediction$branches$warmer$observations[[2L]]$plugins[[
            1L
        ]]$trends$total$values,
        c(114, 88, 64)
    )
    expect_identical(
        prediction$branches$cooler$observations[[2L]]$plugins[[
            1L
        ]]$trends$total$values,
        c(102, 84, 64)
    )
    expect_length(readLines(marker), 2L)
    expect_identical(session$observe(), origin)
    expect_identical(session$status()$identity, identity)
    expect_identical(session$status()$phase, "paused")
    failed_worker <- file.path(fixture$directory, "failed-worker.rds")
    failure <- function(observation, state) {
        saveRDS(Sys.getpid(), failed_worker)
        observation$plugins[[1L]]$trends$total$values[] <- -999
        stop("managed future failure")
    }
    mixed <- session$predict(
        list(first = cooler, fail = failure, last = cooler),
        2L
    )
    expect_identical(
        vapply(mixed$branches, `[[`, character(1L), "status"),
        c(first = "success", fail = "failed", last = "success")
    )
    expect_match(mixed$branches$fail$reason, "managed future failure")
    expect_null(mixed$branches$fail$observations)
    expect_identical(
        mixed$branches$first$observations,
        mixed$branches$last$observations
    )
    expect_identical(
        mixed$branches$first$observations,
        prediction$branches$cooler$observations
    )
    expect_false(tryCatch(
        ps::ps_is_running(ps::ps_handle(readRDS(failed_worker))),
        error = function(condition) FALSE
    ))
    interrupted_worker <- file.path(fixture$directory, "interrupted-worker.rds")
    cancel <- function(observation, state) {
        saveRDS(Sys.getpid(), interrupted_worker)
        stop(structure(
            list(message = "managed branch interrupt", call = NULL),
            class = c("interrupt", "condition")
        ))
    }
    interrupted_directory <- file.path(
        fixture$directory,
        "interrupted-prediction"
    )
    interrupted <- tryCatch(
        {
            session$predict(
                list(cancel = cancel, later = cooler),
                1L,
                dir = interrupted_directory
            )
            FALSE
        },
        interrupt = function(condition) TRUE
    )
    expect_true(interrupted)
    expect_false(dir.exists(file.path(interrupted_directory, "branch-002")))
    expect_false(tryCatch(
        ps::ps_is_running(ps::ps_handle(readRDS(interrupted_worker))),
        error = function(condition) FALSE
    ))
    expect_length(readLines(marker), 2L)
    expect_identical(session$observe(), origin)
    expect_identical(session$status()$identity, identity)
    expect_identical(session$status()$phase, "paused")
    next_primary <- session$step()
    expect_identical(
        next_primary$plugins[[1L]]$trends$total$values,
        c(86, 64, 42)
    )
    expect_identical(next_primary$controller_state, list(n = 3L))
})

# Sources tied to a worker identity can opt out without disabling deterministic trends.
test_that("stock replay refuses explicitly unreconstructible trends before launching branches", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    plugin <- eplus_plugin(
        list(initialize = function(api, state, plugin) NULL),
        trends = list(
            worker = list(
                source = function(api, state, plugin) Sys.getpid(),
                capacity = 2L,
                replay = FALSE
            )
        )
    )
    session <- eplus_session(
        fixture$model,
        fixture$weather,
        eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
        backend = plugin,
        record = TRUE
    )
    on.exit(try(session$close(), silent = TRUE), add = TRUE)
    origin <- session$initialize()
    identity <- session$status()$identity
    expect_identical(origin$plugins[[1L]]$trends$worker$replay, FALSE)
    directory <- file.path(fixture$directory, "forbidden-branches")
    expect_error(
        session$predict(list(a = list(NULL)), 1L, dir = directory),
        "worker.*binding.*cannot be reconstructed.*replay = FALSE"
    )
    expect_false(dir.exists(directory))
    expect_identical(session$observe(), origin)
    expect_identical(session$status()$identity, identity)
    expect_identical(session$status()$phase, "paused")
    expect_identical(session$step()$step, 2L)
})

# Tiny numerical differences may be tolerated, while an undeclared nonreplayable source fails the prefix.
test_that("stock replay compares managed sample values with the requested absolute tolerance", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    marker <- file.path(fixture$directory, "future-entered")
    plugin <- eplus_plugin(
        list(initialize = function(api, state, plugin) NULL),
        trends = list(
            identity = list(
                source = function(api, state, plugin) Sys.getpid() * 1e-12,
                capacity = 2L
            )
        )
    )
    session <- eplus_session(
        fixture$model,
        fixture$weather,
        eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
        backend = plugin,
        record = TRUE
    )
    on.exit(try(session$close(), silent = TRUE), add = TRUE)
    session$initialize()
    origin <- session$step()
    future <- function(observation, state) {
        writeLines("entered", marker)
        list(action = NULL, state = state)
    }
    rejected <- session$predict(list(strict = future), 1L, tolerance = 0)
    expect_identical(rejected$branches$strict$status, "failed")
    expect_match(rejected$branches$strict$reason, "Replay diverged")
    expect_false(file.exists(marker))
    accepted <- session$predict(list(tolerant = future), 1L, tolerance = 1e-6)
    expect_identical(accepted$branches$tolerant$status, "success")
    expect_true(file.exists(marker))
    actual <- accepted$branches$tolerant$boundary$plugins[[1L]]$trends$identity
    expected <- origin$plugins[[1L]]$trends$identity
    expect_false(identical(actual$values, expected$values))
    expect_lt(max(abs(actual$values - expected$values)), 1e-6)
    expect_identical(
        actual[setdiff(names(actual), "values")],
        expected[setdiff(names(expected), "values")]
    )
    expect_identical(session$observe(), origin)
    expect_identical(session$status()$phase, "paused")
    expect_identical(session$step()$step, 3L)
})

# A real caller SIGINT must reap the active replay worker while the primary is still alive and resumable.
test_that("stock managed replay caller interrupts leave the original session resumable", {
    skip_if_not_installed("ps")
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    ready <- file.path(fixture$directory, "branch-ready.rds")
    owner <- callr::r_bg(
        function(model, weather, version, ready) {
            library(eplusr)
            plugin <- eplus_plugin(
                list(initialize = function(api, state, plugin) NULL),
                data = list(n = 0L),
                trends = list(
                    n = list(
                        source = function(api, state, plugin) {
                            plugin$data$n <- plugin$data$n + 1L
                            plugin$data$n
                        },
                        capacity = 2L
                    )
                )
            )
            session <- eplus_session(
                model,
                weather,
                eplus = version,
                backend = plugin,
                record = TRUE
            )
            on.exit(try(session$close(), silent = TRUE), add = TRUE)
            session$initialize()
            origin <- session$step(n = 2L)
            identity <- session$status()$identity
            # Signal readiness from the actual future policy, after all historical checks passed.
            slow <- function(observation, state) {
                saveRDS(c(Sys.getpid(), ps::ps_ppid(ps::ps_handle())), ready)
                Sys.sleep(30)
                list(action = NULL, state = state)
            }
            interrupted <- tryCatch(
                {
                    session$predict(list(slow = slow), 1L)
                    FALSE
                },
                interrupt = function(condition) TRUE
            )
            branch_alive <- vapply(
                readRDS(ready),
                function(pid) {
                    tryCatch(
                        ps::ps_is_running(ps::ps_handle(pid)),
                        error = function(condition) FALSE
                    )
                },
                logical(1L)
            )
            preserved <- identical(session$observe(), origin) &&
                identical(session$status()$identity, identity) &&
                identical(session$status()$phase, "paused")
            resumed <- session$step()
            # Return before owner exit; process-tree teardown cannot satisfy this evidence.
            list(
                interrupted = interrupted,
                preserved = preserved,
                branch_alive = branch_alive,
                primary_alive = session$status()$alive,
                step = resumed$step,
                values = resumed$plugins[[1L]]$trends$n$values
            )
        },
        args = list(
            fixture$model,
            fixture$weather,
            Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
            ready
        ),
        libpath = .libPaths(),
        stdout = "|",
        stderr = "|",
        supervise = FALSE
    )
    on.exit(if (owner$is_alive()) owner$kill(), add = TRUE)
    deadline <- Sys.time() + 120
    while (!file.exists(ready) && owner$is_alive() && Sys.time() < deadline) {
        Sys.sleep(.05)
    }
    expect_true(
        file.exists(ready),
        info = paste(owner$read_error_lines(), collapse = "\n")
    )
    handles <- lapply(readRDS(ready), ps::ps_handle)
    on.exit(
        for (handle in handles) {
            if (ps::ps_is_running(handle)) {
                try(ps::ps_kill(handle), silent = TRUE)
            }
        },
        add = TRUE
    )
    owner$interrupt()
    owner$wait(timeout = 45000)
    expect_false(owner$is_alive())
    outcome <- owner$get_result()
    expect_true(outcome$interrupted)
    expect_true(outcome$preserved)
    expect_false(any(outcome$branch_alive))
    expect_true(outcome$primary_alive)
    expect_identical(outcome$step, 4L)
    expect_identical(outcome$values, c(4, 3))
    expect_false(any(vapply(handles, ps::ps_is_running, logical(1L))))
})

# Reconstructing a different registration is a configuration mismatch, even when all source values match.
test_that("stock replay rejects changed trend registration before future policies", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    marker <- file.path(fixture$directory, "unexpected-policy")
    backend <- function(api, state) {
        definitions <- list(list(
            source = function(api, state, plugin) 1,
            capacity = 2L
        ))
        names(definitions) <- paste0("worker-", Sys.getpid())
        eplus_plugin(
            list(initialize = function(api, state, plugin) NULL),
            trends = definitions
        )(api, state)
    }
    session <- eplus_session(
        fixture$model,
        fixture$weather,
        eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
        backend = backend,
        record = TRUE
    )
    on.exit(try(session$close(), silent = TRUE), add = TRUE)
    origin <- session$initialize()
    future <- function(observation, state) {
        writeLines("entered", marker)
        list(action = NULL, state = state)
    }
    prediction <- session$predict(list(changed = future), 1L)
    expect_identical(prediction$branches$changed$status, "failed")
    expect_match(prediction$branches$changed$reason, "Replay diverged")
    expect_false(file.exists(marker))
    expect_identical(session$observe(), origin)
    expect_identical(session$status()$phase, "paused")
    expect_identical(session$step()$step, 2L)
})
