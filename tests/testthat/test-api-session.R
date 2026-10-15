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

test_that("published snapshots are bounded, complete and independent of session commands", {
    ctx <- testapi__session_context()
    session <- api_session__construct(ctx)
    on.exit(session$close(), add = TRUE)
    expect_null(session$observe(wait = FALSE))
    expect_error(session$observe(wait = NA))
    dir.create(ctx$config$snapshot_dir)
    control <- new.env(parent = emptyenv())
    control$snapshot_files <- character()
    ctx$phase <- "running"
    for (step in 1:4) {
        control$observation <- list(step = step, values = c(t = step))
        api_session__publish(ctx$config, control)
        expect_identical(session$observe(wait = FALSE), control$observation)
        expect_lte(length(list.files(ctx$config$snapshot_dir)), 2L)
    }
    expect_identical(ctx$request, 0L)
    # A reader-held file may prevent deletion on Windows; skip publication instead of growing the cache.
    control$observation <- list(step = 5L, values = c(t = 5))
    with_mocked_bindings(
        {
            expect_null(api_session__publish(ctx$config, control))
            expect_length(list.files(ctx$config$snapshot_dir), 2L)
            expect_identical(session$observe(wait = FALSE)$step, 4L)
        },
        unlink = function(...) 1L,
        .package = "base"
    )
    control$observation <- list(step = 4L, values = c(t = 4))
    expect_silent(api_session__publish(ctx$config, control))
    # A failed read during rotation retains the last complete caller-side value.
    writeLines("incomplete", control$snapshot_files[[1L]])
    expect_identical(session$observe(wait = FALSE)$step, 4L)
    unlink(control$snapshot_files)
    expect_identical(session$observe(wait = FALSE)$step, 4L)
    ctx$observation <- NULL
    expect_null(session$observe(wait = FALSE))
    # An interrupted writer never exposes the temporary file as a completed sample.
    saveRDS(list(step = 99L), file.path(ctx$config$snapshot_dir, "pending"))
    expect_null(session$observe(wait = FALSE))
    local_mocked_bindings(file.rename = function(...) FALSE, .package = "base")
    expect_error(api_session__publish(ctx$config, control), "Cannot publish")
    session$close()
    expect_false(dir.exists(ctx$config$snapshot_dir))
})

test_that("stock sparse snapshots skip unrequested steps without losing exact commands or replay", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    expect_error(eplus_session(
        fixture$model,
        fixture$weather,
        sample_every = 0L
    ))
    expect_error(eplus_session(
        fixture$model,
        fixture$weather,
        sample_every = 1.5
    ))
    for (record in c(FALSE, TRUE)) {
        session <- eplus_session(
            fixture$model,
            fixture$weather,
            variables = list(t = c("Zone Mean Air Temperature", "ZONE ONE")),
            sample_every = 31L,
            record = record,
            dir = file.path(fixture$directory, paste0("sparse-", record))
        )
        on.exit(try(session$close(), silent = TRUE), add = TRUE)
        first <- session$initialize()
        expect_identical(session$observe(wait = FALSE), first)
        expect_identical(session$step(n = 3L)$step, 4L)
        session$resume()
        expect_true(session$result()$status()$successful)
        expect_identical(session$status()$outcome$completed_steps, 96L)
        expect_identical(
            session$observe(wait = FALSE)$step,
            if (record) 96L else 93L
        )
        ctx <- environment(session$observe)$ctx
        expect_lte(length(list.files(ctx$config$snapshot_dir)), 2L)
        if (record) {
            expect_length(list.files(ctx$config$journal), 96L)
        }
        session$close()
    }
})

test_that("nonblocking stock observation does not wait for a busy controller", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    marker <- file.path(fixture$directory, "controller-entered")
    # Block a genuine engine callback after announcing entry; cached reads need no reply.
    controller <- function(observation, state) {
        if (state == 0L) {
            writeLines("entered", marker)
            Sys.sleep(2)
        }
        list(action = NULL, state = state + 1L)
    }
    session <- eplus_session(
        fixture$model,
        fixture$weather,
        controller = controller,
        controller_state = 0L,
        sample_every = 31L,
        dir = file.path(fixture$directory, "busy-controller")
    )
    on.exit(try(session$close(), silent = TRUE), add = TRUE)
    first <- session$initialize()
    session$resume()
    deadline <- Sys.time() + 30
    while (!file.exists(marker) && Sys.time() < deadline) {
        Sys.sleep(0.01)
    }
    expect_true(file.exists(marker))
    ctx <- environment(session$observe)$ctx
    request <- ctx$request
    started <- proc.time()[["elapsed"]]
    expect_identical(session$observe(wait = FALSE), first)
    expect_lt(proc.time()[["elapsed"]] - started, 1)
    expect_identical(ctx$request, request)
    expect_identical(ctx$phase, "running")
    expect_true(session$result()$status()$successful)
    expect_identical(session$observe(wait = FALSE)$controller_state, 95L)
})

test_that("sparse running commands obtain their actual boundary rather than an older sample", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    model <- read_idf(fixture$model)
    model$set(RunPeriod := list(end_day_of_month = 7))
    model$save(overwrite = TRUE)
    session <- eplus_session(
        model,
        fixture$weather,
        sample_every = 100000L,
        variables = list(t = c("Zone Mean Air Temperature", "ZONE ONE")),
        dir = file.path(fixture$directory, "sparse-commands")
    )
    on.exit(try(session$close(), silent = TRUE), add = TRUE)
    first <- session$initialize()
    session$resume()
    observed <- session$observe()
    expect_gt(observed$step, first$step)
    expect_equal(observed$time_end, observed$step / 4, tolerance = 1e-10)
    paused <- session$pause()
    expect_gte(paused$step, observed$step)
    if (session$status()$phase == "paused") {
        expect_identical(session$observe(wait = FALSE), paused)
        session$resume()
    }
    expect_true(session$result()$status()$successful)
    expect_identical(session$status()$outcome$completed_steps, 672L)
})
