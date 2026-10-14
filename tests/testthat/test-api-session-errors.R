test_that("script setup errors, unknown bindings and command timeout stop stock sessions", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    for (mode in c("script", "variable", "actuator", "timeout")) {
        controller <- NULL
        variables <- list()
        actuators <- list()
        if (mode == "script") {
            controller <- file.path(fixture$directory, "missing-controller.R")
            writeLines("ordinary_value <- 1", controller)
        } else if (mode == "variable") {
            variables <- list(missing = c("Unknown Output", "ZONE ONE"))
        } else if (mode == "actuator") {
            actuators <- list(
                missing = c("Schedule:Constant", "Schedule Value", "Unknown")
            )
        } else {
            # Keep an actual native callback active until the client deadline expires.
            controller <- function(observation, state) {
                Sys.sleep(5)
                list(action = NULL, state = state)
            }
        }
        session <- eplus_session(
            fixture$model,
            fixture$weather,
            controller = controller,
            variables = variables,
            actuators = actuators,
            dir = file.path(fixture$directory, mode)
        )
        on.exit(try(session$close(), silent = TRUE), add = TRUE)
        if (mode == "timeout") {
            session$initialize()
            expect_error(session$step(timeout = 0.1), "timed out")
            expect_identical(session$status()$phase, "terminated")
            expect_true(session$result()$status()$terminated)
        } else {
            expect_error(session$initialize(), "define|Unknown")
            expect_identical(session$status()$phase, "failed")
        }
        expect_false(session$status()$alive)
        session$close()
    }
})

test_that("worker scripts can observe a running session, pause, and detect client loss", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    # Direct installation selection and invalid weather fail before launching an engine.
    root <- eplus_config("26.1")$dir
    expect_error(
        eplus_session(fixture$model, NULL, eplus = root),
        "requires weather"
    )
    expect_error(eplus_session(
        fixture$model,
        fixture$weather,
        controller = function(x) x
    ))
    script <- file.path(fixture$directory, "controller.R")
    writeLines(
        c(
            "# Source once in the worker, using its ordinary package search path.",
            "library(stats)",
            "controller <- function(observation, state) {",
            "    Sys.sleep(0.05)",
            "    list(action = NULL, state = state + 1L)",
            "}"
        ),
        script
    )
    session <- eplus_session(
        fixture$model,
        fixture$weather,
        controller = script,
        controller_state = 0L,
        dir = file.path(fixture$directory, "script")
    )
    on.exit(try(session$close(), silent = TRUE), add = TRUE)
    session$initialize()
    session$resume()
    running <- session$observe()
    expect_gte(running$step, 2L)
    paused <- session$pause()
    expect_identical(session$status()$phase, "paused")
    expect_gte(paused$step, running$step)
    expect_identical(paused$controller_state, paused$step - 1L)
    expect_identical(session$observe(), paused)
    # Losing the owning client must stop the worker without advancing the paused observation.
    ctx <- environment(session$observe)$ctx
    close(ctx$connection)
    ctx$connection <- NULL
    deadline <- Sys.time() + 30
    while (session$status()$alive && Sys.time() < deadline) {
        Sys.sleep(0.05)
    }
    expect_false(session$status()$alive)
    expect_identical(session$status()$phase, "stopped")
    expect_identical(session$status()$outcome$session_reason, "connection_lost")
    expect_identical(session$observe(), paused)
    session$close()
})

test_that("session transport failures retain native diagnostics and bounded termination", {
    for (mode in c("unexpected", "eof", "terminal", "bad_final", "timeout")) {
        pair <- testapi__protocol_pair()
        ctx <- pair$ctx
        on.exit(
            {
                api_session__disconnect(ctx)
                try(close(pair$client), silent = TRUE)
            },
            add = TRUE
        )
        if (mode == "unexpected") {
            api_session__send(pair$client, list(type = "boundary", id = 99L))
            expect_error(
                api_session__guard(ctx, api_session__wait(ctx, 1L, 1)),
                "Unexpected"
            )
            expect_identical(ctx$phase, "terminated")
        } else if (mode == "eof") {
            close(pair$client)
            ctx$live <- FALSE
            expect_null(api_session__wait(ctx, 1L, 1))
            expect_identical(ctx$phase, "completed")
        } else if (mode == "terminal") {
            api_session__send(
                pair$client,
                list(type = "terminal", observation = list(step = 96L))
            )
            close(pair$client)
            ctx$live <- FALSE
            # Passive status must consume the final frame and EOF without advancing a simulation.
            session <- api_session__construct(ctx)
            expect_identical(session$status()$phase, "completed")
            api_session__finalize(ctx, session)
            expect_identical(ctx$observation$step, 96L)
            expect_true(ctx$terminal)
        } else if (mode == "bad_final") {
            api_session__send(pair$client, list(type = "boundary", id = 1L))
            ctx$live <- FALSE
            expect_error(api_session__collect(ctx), "Unexpected final")
        } else {
            expect_error(
                api_session__guard(ctx, api_session__wait(ctx, 1L, 0.01)),
                "timed out"
            )
            expect_false(ctx$live)
            expect_identical(ctx$phase, "terminated")
        }
        api_session__disconnect(ctx)
        try(close(pair$client), silent = TRUE)
    }
    ctx <- testapi__session_context()
    ctx$phase <- "failed"
    ctx$error <- list(class = "interrupt", message = NULL)
    ctx$outcome <- list(stop_reason = "interrupt")
    expect_identical(
        api_session__diagnostic(ctx, "fallback"),
        "EnergyPlus worker interrupted."
    )
    api_session__fail(ctx, simpleError("protocol failure"))
    expect_identical(ctx$phase, "failed")
    expect_identical(ctx$error$class, "interrupt")
    expect_identical(ctx$error$message, "protocol failure")
    ctx$error <- NULL
    ctx$outcome <- NULL
    expect_identical(api_session__diagnostic(ctx, "fallback"), "fallback")
})

test_that("session command parsing rejects invalid state and handles every boundary operation", {
    path <- tempfile()
    connection <- file(path, open = "w+b")
    on.exit(close(connection), add = TRUE)
    control <- new.env(parent = emptyenv())
    control$connection <- connection
    control$mode <- "running"
    control$observation <- list(step = 1L)
    control$action <- c(cooling = 26)
    settings <- list(actuators = list(heating = 1, cooling = 2))
    stopped <- FALSE
    api <- list(
        runtime = list(stop_simulation = function(state) stopped <<- TRUE)
    )
    expect_error(
        api_session__dispatch(
            api,
            NULL,
            settings,
            control,
            list(id = "bad", operation = "pause")
        ),
        "Invalid"
    )
    expect_error(
        api_session__dispatch(
            api,
            NULL,
            settings,
            control,
            list(id = 1L, operation = "step", n = 1L)
        ),
        "not paused"
    )
    api_session__dispatch(
        api,
        NULL,
        settings,
        control,
        list(id = 2L, operation = "pause")
    )
    expect_identical(control$mode, "paused")
    api_session__dispatch(
        api,
        NULL,
        settings,
        control,
        list(id = 3L, operation = "observe")
    )
    api_session__dispatch(
        api,
        NULL,
        settings,
        control,
        list(id = 4L, operation = "step", n = 2L, action = c(heating = 22))
    )
    expect_identical(control$remaining, 2L)
    expect_identical(control$action, c(cooling = 26, heating = 22))
    control$mode <- "paused"
    api_session__dispatch(
        api,
        NULL,
        settings,
        control,
        list(id = 5L, operation = "resume")
    )
    expect_identical(control$mode, "running")
    api_session__dispatch(
        api,
        NULL,
        settings,
        control,
        list(id = 6L, operation = "close")
    )
    expect_true(stopped)
    expect_identical(control$mode, "closing")
})

test_that("worker death and startup expiry close the listener and preserve diagnostics", {
    local_mocked_bindings(use_eplus = function(directory) NULL)
    for (mode in c("dead", "timeout")) {
        ctx <- testapi__session_context()
        ctx$config$installation <- "selected-installation"
        alive <- mode == "timeout"
        # Model failure before the native worker can authenticate, without loading an engine.
        ctx$job <- R6::R6Class(
            "SessionStartupFailure",
            cloneable = FALSE,
            public = list(
                # Publish the pipeline handle while leaving the IPC listener without a peer.
                run = function(...) {
                    private$m_job <- list(is_alive = function() alive)
                    invisible(self)
                },
                # Record the bounded cancellation request made by the session guard.
                kill = function() alive <<- FALSE,
                # A dead startup retains its actual failure message instead of a fabricated timeout.
                status = function() {
                    list(
                        successful = FALSE,
                        api = list(
                            phase = "failed",
                            error = list(message = "startup failed")
                        )
                    )
                }
            ),
            private = list(m_job = NULL)
        )$new()
        session <- api_session__construct(ctx)
        expect_error(
            session$initialize(timeout = 0.01),
            if (mode == "dead") "startup failed" else "startup timed out"
        )
        expect_null(ctx$server)
        expect_false(alive)
        expect_identical(
            session$status()$phase,
            if (mode == "dead") "failed" else "terminated"
        )
        session$close()
        api_session__finalize(ctx, session)
    }
})

test_that("a failed command write and unsuccessful Job cancellation still reap the pipeline", {
    pair <- testapi__protocol_pair()
    ctx <- pair$ctx
    on.exit(
        {
            api_session__disconnect(ctx)
            try(close(pair$client), silent = TRUE)
        },
        add = TRUE
    )
    # Exercise the final fallback when the pipeline cancellation method itself cannot finish.
    ctx$job$kill <- function() stop("Job cancellation failed")
    local_mocked_bindings(
        api_session__send = function(...) stop("command write failed"),
        run__api_kill = function(process) ctx$live <- FALSE
    )
    expect_error(
        api_session__command(ctx, "observe", 1),
        "command write failed"
    )
    expect_false(ctx$live)
    expect_identical(ctx$phase, "terminated")
    expect_identical(ctx$error$message, "command write failed")
})

test_that("socket reservation stops after twenty occupied candidates", {
    attempts <- 0L
    # Recreate a fully occupied candidate range and ensure no unbounded bind loop occurs.
    local_mocked_bindings(
        serverSocket = function(port) {
            attempts <<- attempts + 1L
            stop("port occupied")
        },
        .package = "base"
    )
    expect_error(api_session__listen(), "Unable to reserve")
    expect_identical(attempts, 20L)
})

# Reject invalid branch specifications before creating a prediction worker.
