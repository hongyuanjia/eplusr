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
    expect_error(
        session$predict(
            list(fail = function(observation, state) stop("future failure")),
            1L
        ),
        "future failure"
    )
    expect_identical(session$status()$phase, "paused")
    expect_error(
        session$predict(
            list(slow = function(observation, state) {
                Sys.sleep(5)
                list(action = NULL, state = state)
            }),
            1L,
            timeout = 0.1
        ),
        "timed out"
    )
    expect_identical(session$status()$phase, "paused")
    expect_error(
        session$predict(list(a = list(NULL)), 1L, timeout = 1e-9),
        "branch timed out"
    )
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
    session <- eplus_session(fixture$model, fixture$weather, record = TRUE)
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
        eplus_session(fixture$model, fixture$weather, record = TRUE),
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
