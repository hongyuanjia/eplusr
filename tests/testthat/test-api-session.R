test_that("session definitions, actions and budgets reject unsafe configuration", {
    expect_equal(api_session__definitions(list(), 2L, "variables"), list())
    expect_equal(
        api_session__definitions(
            list(t = c("Temperature", "Zone")),
            2L,
            "variables"
        )$t,
        c("Temperature", "Zone")
    )
    expect_error(
        api_session__definitions(
            list(c("Temperature", "Zone")),
            2L,
            "variables"
        ),
        "unique"
    )
    expect_error(
        api_session__definitions(
            setNames(list(c("a", "b"), c("c", "d")), c("x", "x")),
            2L,
            "variables"
        ),
        "unique"
    )
    expect_error(api_session__definitions(
        list(t = "Temperature"),
        2L,
        "variables"
    ))
    expect_equal(
        api_session__action(c(heating = NA_real_), "heating"),
        c(heating = NA_real_)
    )
    expect_null(api_session__action(NULL, character()))
    for (bad in list(c(missing = 20), c(heating = Inf), c(heating = NaN), 20)) {
        expect_error(api_session__action(bad, "heating"))
    }
    expect_error(
        api_session__action(
            structure(c(heating = 1L), class = "EnergyPlusHandle"),
            "heating"
        ),
        "live objects"
    )
    native_connection <- file()
    on.exit(close(native_connection), add = TRUE)
    expect_error(api_session__plain(native_connection), "live objects")
    expect_error(api_session__timeout(0), "positive")
    expect_error(api_session__timeout(Inf))
    expect_silent(api_session__timeout(1))
})

test_that("session proxies enforce phase, process ownership and cleanup", {
    ctx <- testapi__session_context()
    session <- api_session__construct(ctx)
    on.exit(session$close(), add = TRUE)
    expect_s3_class(session, "EnergyPlusSession")
    expect_identical(session$status()$phase, "created")
    expect_error(session$observe(), "no available")
    expect_error(session$step(), "paused")
    expect_error(session$pause(), "running")
    expect_error(session$resume(), "paused")
    expect_error(session$result(), "Resume or close")
    expect_error(session$initialize(timeout = 0), "positive")
    expect_identical(session$status()$phase, "created")
    copy <- unserialize(serialize(session, NULL))
    expect_error(copy$status(), "cannot be restored")
    expect_error(copy$close(), "cannot be restored")
    # A copied proxy finalizer must also leave the original context alone.
    expect_null(api_session__finalize(environment(copy$observe)$ctx, copy))
    expect_error(
        run__check_setup(local({
            value <- session
            function(api, state) value
        })),
        "live objects"
    )
    ctx$phase <- "paused"
    ctx$observation <- list(step = 1L)
    expect_identical(session$observe(), ctx$observation)
    expect_identical(session$pause(), ctx$observation)
    expect_error(session$initialize(), "once")
    expect_error(session$step(n = 0L))
    ctx$config$controller <- function(observation, state) NULL
    expect_error(session$step(c(heating = 20)), "cannot be combined")
    expect_error(session$resume(c(heating = 20)), "cannot be combined")
    session$close()
    expect_true(session$close())
    expect_identical(session$status()$phase, "closed")
    expect_identical(session$result(), ctx$job)
    expect_identical(session$observe(), ctx$observation)
})

test_that("session wire frames are bounded and diagnose truncated or malformed messages", {
    path <- tempfile()
    connection <- file(path, open = "w+b")
    on.exit(close(connection), add = TRUE)
    message <- list(id = 1L, action = c(heating = 21), state = list(count = 1L))
    api_session__send(connection, message)
    seek(connection, where = 0, origin = "start", rw = "read")
    expect_identical(api_session__receive(connection), message)
    expect_null(api_session__receive(connection))
    expect_error(
        api_session__send(connection, list(raw = raw(8L * 1024L * 1024L))),
        "exceeds"
    )
    for (bytes in list(
        c(writeBin(0L, raw(), endian = "big")),
        c(writeBin(30L, raw(), endian = "big"), as.raw(1:3)),
        c(
            writeBin(length(serialize(1, NULL)), raw(), endian = "big"),
            serialize(1, NULL)
        )
    )) {
        input <- rawConnection(bytes)
        expect_error(api_session__receive(input))
        close(input)
    }
})

test_that("session controller memory advances explicitly and retains unrelated actions", {
    control <- new.env(parent = emptyenv())
    control$observation <- list(values = c(temperature = 20))
    control$action <- c(cooling = 26)
    control$controller_state <- 0L
    # Return a partial action and a new ordinary controller state.
    control$controller <- function(observation, controller_state) {
        list(
            action = c(heating = observation$values[[1L]] + 1),
            state = controller_state + 1L
        )
    }
    settings <- list(actuators = list(heating = 1, cooling = 2))
    api_session__control(settings, control)
    expect_identical(control$action, c(cooling = 26, heating = 21))
    expect_identical(control$controller_state, 1L)
    control$controller <- function(observation, state) {
        list(action = NULL, state = state)
    }
    api_session__control(settings, control)
    expect_identical(control$action, c(cooling = 26, heating = 21))
    control$controller <- function(observation, state) 1
    expect_error(api_session__control(settings, control), "must return")
    control$controller <- NULL
    expect_silent(api_session__control(settings, control))
})

test_that("stock sessions initialize, step, pause, resume and finalize the existing Job", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    variables <- list(temperature = c("Zone Mean Air Temperature", "ZONE ONE"))
    session <- eplus_session(
        fixture$model,
        fixture$weather,
        variables = variables,
        dir = file.path(fixture$directory, "session")
    )
    on.exit(try(session$close(), silent = TRUE), add = TRUE)
    first <- session$initialize()
    identity <- session$status()$identity
    expect_identical(first$step, 1L)
    expect_identical(first$kind_of_sim, 3L)
    expect_identical(first$callback, "end_zone_timestep_after_zone_reporting")
    expect_equal(first$time_end - first$time_start, 0.25, tolerance = 1e-10)
    expect_true(is.finite(first$values[["temperature"]]))
    Sys.sleep(0.2)
    gc()
    expect_identical(session$observe(), first)
    next_step <- session$step(n = 3L)
    expect_identical(next_step$step, 4L)
    expect_equal(next_step$time_end - first$time_end, 0.75, tolerance = 1e-10)
    expect_identical(session$status()$identity, identity)
    expect_error(session$result(), "Resume or close")
    session$resume()
    # A running pause can win the race or encounter a naturally completed one-day model.
    if (session$status()$phase == "running") {
        paused <- session$pause()
        if (session$status()$phase == "paused") {
            expect_gte(paused$step, 4L)
            session$resume()
        }
    }
    job <- session$result()
    expect_s3_class(job, "EplusJob")
    expect_true(job$status()$successful)
    expect_identical(job$status()$api$pid, identity$pid)
    expect_identical(session$observe()$step, 96L)
    expect_gt(nrow(job$read_table("ReportData")), 0L)
    session$close()
    expect_identical(session$status()$phase, "closed")
    expect_false(session$status()$alive)
})

test_that("stock sessions preserve controller state and stop safely on controller errors", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    # Keep controller state separate from the live API and advance once per completed transition.
    controller <- function(observation, state) {
        if (state == 2L) {
            stop("session controller failed")
        }
        list(action = NULL, state = state + 1L)
    }
    session <- eplus_session(
        fixture$model,
        fixture$weather,
        variables = list(
            temperature = c("Zone Mean Air Temperature", "ZONE ONE")
        ),
        controller = controller,
        controller_state = 0L,
        dir = file.path(fixture$directory, "controller")
    )
    on.exit(try(session$close(), silent = TRUE), add = TRUE)
    first <- session$initialize()
    expect_identical(first$controller_state, 0L)
    second <- session$step()
    expect_identical(second$controller_state, 1L)
    third <- session$step()
    expect_identical(third$controller_state, 2L)
    expect_error(session$step(), "session controller failed")
    status <- session$status()
    expect_identical(status$phase, "failed")
    expect_identical(status$outcome$state_phase, "closed")
    expect_false(status$alive)
    expect_error(session$resume(), "paused")
    session$close()
})
test_that("session handshakes authenticate before decoding and retain their selected installation", {
    ctx <- testapi__session_context()
    ctx$config$installation <- "selected-installation"
    clients <- list()
    alive <- TRUE
    selected <- NULL
    # Reconstruct rejected raw and decoded greetings before the legitimate worker handshake.
    peer <- R6::R6Class(
        "SessionProtocolPeer",
        cloneable = FALSE,
        public = list(
            # Queue complete frames before the caller begins its event loop.
            run = function(..., backend) {
                settings <- environment(backend)$settings
                for (index in seq_len(3L)) {
                    valid <- index > 1L
                    connection <- socketConnection(
                        "127.0.0.1",
                        settings$port,
                        blocking = TRUE,
                        open = "a+b",
                        timeout = 2
                    )
                    clients[[length(clients) + 1L]] <<- connection
                    writeBin(
                        if (valid) {
                            charToRaw(settings$token)
                        } else {
                            charToRaw(paste(
                                rep("x", nchar(settings$token)),
                                collapse = ""
                            ))
                        },
                        connection
                    )
                    flush(connection)
                    if (valid) {
                        api_session__send(
                            connection,
                            list(
                                type = if (index == 2L) "invalid" else "hello",
                                identity = list(pid = 1L)
                            )
                        )
                        if (index == 3L) {
                            api_session__send(
                                connection,
                                list(
                                    type = "boundary",
                                    id = 0L,
                                    phase = "paused",
                                    observation = list(step = 1L)
                                )
                            )
                        }
                    }
                }
                private$m_job <- list(
                    # Model a live pipeline while the deterministic handshake is consumed.
                    is_alive = function() alive,
                    # Expose a plain pipeline identity without any native process ownership.
                    get_pid = function() 2L
                )
                invisible(self)
            },
            # The test peer becomes complete before explicit cleanup.
            status = function() {
                list(
                    successful = TRUE,
                    api = list(phase = "completed", error = NULL)
                )
            }
        ),
        private = list(m_job = NULL)
    )$new()
    ctx$job <- peer
    session <- api_session__construct(ctx)
    on.exit(
        {
            alive <- FALSE
            try(session$close(), silent = TRUE)
            api_session__finalize(ctx, session)
            for (connection in clients) {
                try(close(connection), silent = TRUE)
            }
        },
        add = TRUE
    )
    # Installation selection is observed without loading an optional engine.
    local_mocked_bindings(use_eplus = function(directory) {
        selected <<- directory
        NULL
    })
    # Establish the caller RNG explicitly, even when optional native tests are disabled.
    withr::local_seed(654L)
    seed <- .Random.seed
    first <- session$initialize(timeout = 2)
    expect_identical(first$step, 1L)
    expect_identical(selected, "selected-installation")
    expect_identical(.Random.seed, seed)
    expect_identical(session$status()$identity$pipeline_pid, 2L)
    alive <- FALSE
    session$close()
    api_session__finalize(ctx, session)
    expect_false(exists(ctx$id, api_session__registry, inherits = FALSE))
})

test_that("collecting a paused stock session reaps the native worker and pipeline", {
    skip_if_not_installed("ps")
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    # Do not retain the public proxy after leaving this local frame.
    handles <- local({
        session <- eplus_session(
            fixture$model,
            fixture$weather,
            dir = file.path(fixture$directory, "gc-session")
        )
        session$initialize()
        identity <- session$status()$identity
        lapply(c(identity$pid, identity$pipeline_pid), ps::ps_handle)
    })
    on.exit(
        for (handle in handles) {
            if (ps::ps_is_running(handle)) {
                try(ps::ps_kill(handle), silent = TRUE)
            }
        },
        add = TRUE
    )
    gc()
    deadline <- Sys.time() + 10
    while (
        any(vapply(handles, ps::ps_is_running, logical(1L))) &&
            Sys.time() < deadline
    ) {
        Sys.sleep(0.05)
    }
    expect_false(any(vapply(handles, ps::ps_is_running, logical(1L))))
})

test_that("closing a paused stock session records early stop and forbids continuation", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    session <- eplus_session(
        fixture$model,
        fixture$weather,
        dir = file.path(fixture$directory, "early-close")
    )
    on.exit(try(session$close(), silent = TRUE), add = TRUE)
    first <- session$initialize()
    session$close()
    expect_identical(session$observe(), first)
    expect_false(session$result()$status()$successful)
    expect_identical(session$status()$outcome$phase, "stopped")
    expect_false(session$status()$alive)
    expect_error(session$step(), "paused")
})

test_that("caller interrupts with a NULL message preserve their condition and clean protocol state", {
    ctx <- testapi__session_context()
    condition <- structure(
        list(message = NULL, call = NULL),
        class = c("interrupt", "condition")
    )
    caught <- tryCatch(
        api_session__guard(ctx, stop(condition)),
        interrupt = identity
    )
    expect_s3_class(caught, "interrupt")
    expect_identical(ctx$phase, "terminated")
    expect_identical(ctx$error$message, "Session interrupted.")
    expect_identical(ctx$error$class, c("interrupt", "condition"))
    # Non-interrupt conditions can also lack a message and still need a stable diagnosis.
    ctx <- testapi__session_context()
    api_session__fail(
        ctx,
        structure(
            list(message = NULL, call = NULL),
            class = c("error", "condition")
        )
    )
    expect_identical(ctx$error$message, "Session protocol failed.")
})

test_that("HVAC substeps do not shift completed zone intervals and actuator releases", {
    fixture <- testapi__controlled_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    session <- eplus_session(
        fixture$model,
        fixture$weather,
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
        initial_action = c(heating = 23),
        dir = file.path(fixture$directory, "substeps")
    )
    on.exit(try(session$close(), silent = TRUE), add = TRUE)
    first <- session$initialize()
    expect_equal(
        c(first$time_start, first$time_end),
        c(0, 0.25),
        tolerance = 1e-10
    )
    expect_equal(first$values[["heating"]], 23)
    fourth <- session$step(c(heating = NA_real_), n = 3L)
    expect_identical(fourth$step, 4L)
    expect_equal(
        c(fourth$time_start, fourth$time_end),
        c(0.75, 1),
        tolerance = 1e-10
    )
    expect_equal(fourth$values[["heating"]], 20)
    session$close()
})

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
