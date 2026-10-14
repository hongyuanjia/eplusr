test_that("API backend setup is portable and rejects native ownership across workers", {
    expect_identical(run__backend("cli"), list(engine = "cli", setup = NULL))
    expect_identical(run__backend("api"), list(engine = "api", setup = NULL))
    expect_error(run__backend("other"), "backend")
    for (invalid in list(
        NULL,
        NA_character_,
        character(),
        c("cli", "api"),
        "a",
        "CLI",
        1,
        list(),
        new.env()
    )) {
        expect_error(run__backend(invalid), "backend")
    }
    ordinary <- local({
        config <- data.table(target = 21)
        function(api, state) config$target
    })
    expect_identical(run__backend(ordinary)$engine, "api")
    native <- local({
        state <- structure(new.env(), class = "EnergyPlusState")
        function(api, current) state
    })
    expect_error(run__backend(native), "live objects")
    skip_if_not_installed("rdyncall")
    pointer <- local({
        value <- rdyncall::as.externalptr(integer(1L))
        function(api, state) value
    })
    expect_error(run__backend(pointer), "live objects")
    expect_null(run__api_version(numeric_version("26.1")))
    expect_error(run__api_version(numeric_version("26.2")), "26.1")
})

test_that("API workers clean up on completion, stops, setup and callback conditions", {
    phase <- "completed"
    stopped <- FALSE
    closed <- 0L
    fake <- list(
        info = function() {
            list(
                version = "26.1.0-test",
                api_version = "0.2",
                library = "test-library",
                pid = Sys.getpid()
            )
        },
        state_manager = list(
            new_state = function() new.env(),
            status = function(state) {
                list(
                    phase = if (stopped) "closed" else phase,
                    stop_reason = if (stopped) "callback_error" else NULL,
                    exit_code = 0L
                )
            }
        ),
        runtime = list(
            set_console_output_status = function(...) NULL,
            callback_begin_zone_timestep_before_init_heat_balance = function(
                ...
            ) {
                NULL
            },
            run_energyplus = function(...) {
                if (stopped) {
                    stop("callback failure")
                }
                0L
            }
        ),
        close = function() {
            closed <<- closed + 1L
            invisible(NULL)
        }
    )
    local_mocked_bindings(eplus_api = function(directory) fake)
    result <- run__api_engine("test", "model.idf")
    expect_identical(result$exit_status, 0L)
    expect_identical(result$api$phase, "completed")
    expect_identical(result$api$library, "test-library")
    phase <- "stopped"
    expect_identical(run__api_engine("test", "model.idf")$exit_status, 1L)
    expect_identical(
        run__api_engine("test", "model.idf")$api$native_exit_status,
        0L
    )
    setup <- function(api, state) stop("setup failure")
    result <- run__api_engine("test", "model.idf", setup)
    expect_identical(result$api$stop_reason, "setup_error")
    expect_match(result$api$error$message, "setup failure")
    stopped <- TRUE
    result <- run__api_engine("test", "model.idf")
    expect_identical(result$api$stop_reason, "callback_error")
    expect_identical(result$api$phase, "failed")
    expect_identical(result$api$state_phase, "closed")
    interrupt <- function(api, state) {
        stop(structure(
            list(message = "interrupt", call = NULL),
            class = c("interrupt", "condition")
        ))
    }
    result <- run__api_engine("test", "model.idf", interrupt)
    expect_identical(result$exit_status, 130L)
    expect_identical(result$api$stop_reason, "interrupt")
    expect_equal(closed, 6L)
    # Unexpected native unwinds retain quarantine rather than claiming normal state disposal.
    fake$state_manager$status <- function(state) {
        list(
            phase = "quarantined",
            stop_reason = "unexpected_unwind",
            exit_code = NULL
        )
    }
    result <- run__api_engine("test", "model.idf")
    expect_identical(result$api$phase, "quarantined")
    expect_identical(result$api$state_phase, "quarantined")
    expect_true(is.na(result$api$native_exit_status))
    local_mocked_bindings(eplus_api = function(directory) {
        stop("missing bridge")
    })
    result <- run__api_engine("test", "model.idf")
    expect_identical(result$api$stop_reason, "load_error")
    expect_match(result$api$error$message, "missing bridge")
})

test_that("API command runners retain process output and diagnose dead workers", {
    failure <- FALSE
    local_mocked_bindings(run_command = function(command, args, ...) {
        expect_equal(args$directory, "test")
        list(
            exit_status = 0L,
            stdout = "native output",
            stderr = character(),
            process = list(
                get_result = function() {
                    if (failure) {
                        stop("worker crashed")
                    }
                    list(
                        exit_status = 0L,
                        api = list(phase = "completed", error = NULL)
                    )
                }
            )
        )
    })
    result <- run__api_command("test", "model.idf", tempdir(), FALSE, NULL)
    expect_equal(result$stdout, "native output")
    expect_equal(result$api$phase, "completed")
    failure <- TRUE
    result <- run__api_command("test", "model.idf", tempdir(), FALSE, NULL)
    expect_equal(result$exit_status, 1L)
    expect_equal(result$api$stop_reason, "worker_error")
    expect_match(result$stderr, "worker crashed")
})


test_that("stock API execution adapters support background and run_idf workflows", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    process <- energyplus(
        fixture$model,
        fixture$weather,
        output_dir = file.path(fixture$directory, "background"),
        backend = "api",
        wait = FALSE,
        readvars = FALSE,
        echo = FALSE
    )
    on.exit(if (process$is_alive()) process$kill(), add = TRUE)
    # Instrumented worker startup can exceed the ordinary simulation deadline.
    process$wait(180000)
    expect_false(process$is_alive())
    expect_identical(process$get_result()$api$phase, "completed")
    single <- run_idf(
        fixture$model,
        fixture$weather,
        output_dir = file.path(fixture$directory, "run-idf"),
        backend = "api",
        expand_obj = FALSE,
        echo = FALSE
    )
    expect_identical(single$backend, "api")
    expect_identical(single$exit_status, 0L)
    low <- EnergyPlus(
        fixture$model,
        fixture$weather,
        output_dir = file.path(fixture$directory, "low-background"),
        backend = "api",
        wait = FALSE,
        echo = FALSE
    )
    on.exit(if (low$run$process$is_alive()) low$run$process$kill(), add = TRUE)
    low$run$process$wait(180000)
    expect_identical(low$run$process$get_result()$run$api$phase, "completed")
})


test_that("stock API controls stop safely and independent parallel jobs retain identity", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    setup <- function(api, state) {
        api$runtime$callback_begin_zone_timestep_after_init_heat_balance(
            state,
            function(s) {
                api$runtime$stop_simulation(s)
            }
        )
    }
    stopped <- energyplus(
        fixture$model,
        fixture$weather,
        output_dir = file.path(fixture$directory, "stop"),
        backend = setup,
        readvars = FALSE,
        echo = FALSE
    )
    expect_identical(
        stopped$api$phase,
        "stopped",
        info = jsonlite::toJSON(
            list(api = stopped$api, stderr = stopped$stderr),
            auto_unbox = TRUE,
            null = "null"
        )
    )
    expect_identical(stopped$api$stop_reason, "user_stop")
    expect_identical(stopped$api$native_exit_status, 0L)
    expect_false(stopped$exit_status == 0L)
    failed <- energyplus(
        fixture$model,
        fixture$weather,
        output_dir = file.path(fixture$directory, "error"),
        readvars = FALSE,
        echo = FALSE,
        backend = function(api, state) {
            api$runtime$callback_begin_zone_timestep_after_init_heat_balance(
                state,
                function(s) stop("controller failed")
            )
        }
    )
    expect_identical(
        failed$api$stop_reason,
        "callback_error",
        info = jsonlite::toJSON(
            list(api = failed$api, stderr = failed$stderr),
            auto_unbox = TRUE,
            null = "null"
        )
    )
    expect_match(failed$api$error$message, "controller failed")
    expect_identical(failed$api$state_phase, "closed")
})

test_that("stock API run_multi reports finalized controller stops", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    setup <- function(api, state) {
        api$runtime$callback_begin_zone_timestep_after_init_heat_balance(
            state,
            function(s) {
                api$runtime$stop_simulation(s)
            }
        )
    }
    multiple <- run_multi(
        rep(fixture$model, 2L),
        fixture$weather,
        output_dir = file.path(fixture$directory, c("multi-a", "multi-b")),
        backend = setup,
        echo = FALSE
    )
    expect_true(all(multiple$status == "stopped"))
    expect_true(all(multiple$backend == "api"))
})

test_that("API setup snapshots lexical data and recursive helpers without unrelated native captures", {
    expect_null(run__prepare_setup(NULL))
    expect_identical(run__prepare_setup(sum), sum)
    expect_identical(run__prepare_setup(stats::median), stats::median)
    setup <- local({
        config <- data.table(target = 21)
        unrelated <- structure(new.env(), class = "EnergyPlusState")
        countdown <- function(n) {
            if (n == 0L) config$target else countdown(n - 1L)
        }
        function(api, state) countdown(3L)
    })
    expect_identical(run__backend(setup)$engine, "api")
    frozen <- run__prepare_setup(setup)
    portable <- unserialize(serialize(frozen, NULL))
    expect_equal(portable(NULL, NULL), 21)
    expect_false(exists("unrelated", environment(frozen), inherits = FALSE))
    environment(setup)$config <- data.table(target = 30)
    expect_equal(frozen(NULL, NULL), 21)

    cyclic <- local({
        config <- new.env(parent = emptyenv())
        config$self <- config
        config$target <- 22
        function(api, state) config$target
    })
    expect_identical(run__backend(cyclic)$engine, "api")
    expect_equal(
        unserialize(serialize(run__prepare_setup(cyclic), NULL))(NULL, NULL),
        22
    )

    attributed <- local({
        config <- structure(
            1,
            owner = structure(new.env(), class = "EnergyPlusState")
        )
        function(api, state) config
    })
    expect_error(run__backend(attributed), "live objects")
})

test_that("unavailable API pipeline results keep explicit failure and termination semantics", {
    process <- list(get_result = function() stop("pipeline lost"))
    failed <- run__api_collect(process, tempdir(), "weather.epw")
    expect_identical(failed$api$stop_reason, "worker_error")
    expect_identical(failed$file$epw, "weather.epw")
    terminated <- run__api_collect(process, tempdir(), NULL, TRUE)
    expect_identical(terminated$api$phase, "terminated")
    expect_identical(terminated$api$stop_reason, "process_terminated")
    expect_true(is.na(terminated$api$native_exit_status))
    state <- list(
        jobs = data.table(
            model = "model.idf",
            weather = list(NULL),
            output_dir = tempdir(),
            energyplus_exe = file.path(tempdir(), "energyplus"),
            index = 1L
        ),
        options = list(backend = "api")
    )
    interrupted <- run__api_failed_state(state, simpleError("lost"), TRUE)
    expect_identical(interrupted$jobs$status, "terminated")
    expect_identical(
        interrupted$jobs$result[[1L]]$api$stop_reason,
        "process_terminated"
    )
    failed <- run__api_failed_state(state, simpleError("crashed"))
    expect_identical(failed$jobs$status, "failed")
    expect_identical(failed$jobs$result[[1L]]$api$phase, "failed")
})

test_that("API setup rejects native objects in captured environment parent chains", {
    setup <- local({
        parent <- new.env(parent = emptyenv())
        parent$owner <- structure(
            new.env(parent = emptyenv()),
            class = "EnergyPlusState"
        )
        config <- new.env(parent = parent)
        config$target <- 22
        function(api, state) config$target
    })
    expect_error(run__backend(setup), "live objects")
    pointer <- local({
        parent <- new.env(parent = emptyenv())
        parent$pointer <- new("externalptr")
        config <- new.env(parent = parent)
        function(api, state) config
    })
    expect_error(run__backend(pointer), "live objects")
})

test_that("API tree shutdown remains bounded and reports failures explicitly", {
    calls <- character()
    process <- list(
        kill_tree = function() calls <<- c(calls, "tree"),
        wait = function(timeout) {
            expect_identical(timeout, 1000)
            calls <<- c(calls, "wait")
        },
        is_alive = function() FALSE
    )
    expect_true(run__api_kill(process))
    expect_identical(calls, c("tree", "wait"))
    process$kill_tree <- function() stop("kill denied")
    expect_false(run__api_kill(process))
})

test_that("foreground API loop interruption reaps its actual native engine descendant", {
    skip_if_not_installed("ps")
    fixture <- testrun__api_fixture()
    marker <- tempfile()
    setup <- local({
        path <- marker
        function(api, state) {
            api$runtime$callback_begin_new_environment(state, function(state) {
                writeLines(as.character(Sys.getpid()), path)
                Sys.sleep(60)
            })
            writeLines("started", paste0(path, ".started"))
        }
    })
    handle <- NULL
    interruption <- structure(
        list(message = "test foreground interrupt", call = NULL),
        class = c("interrupt", "condition")
    )
    local_mocked_bindings(handle_sim_events = function(state) {
        testrun__api_started(
            function() file.exists(paste0(marker, ".started")),
            function() {
                any(vapply(
                    state$jobs$process,
                    function(p) p$is_alive(),
                    logical(1L)
                ))
            }
        )
        deadline <- Sys.time() + 30
        while (!file.exists(marker) && Sys.time() < deadline) {
            Sys.sleep(.1)
        }
        expect_true(file.exists(marker))
        handle <<- ps::ps_handle(as.integer(readLines(marker)))
        expect_true(ps::ps_is_running(handle))
        stop(interruption)
    })
    condition <- tryCatch(
        run_multi(
            fixture$model,
            fixture$weather,
            output_dir = file.path(fixture$directory, "foreground-interrupt"),
            backend = setup,
            echo = FALSE
        ),
        interrupt = identity
    )
    expect_identical(condition, interruption)
    deadline <- Sys.time() + 10
    while (ps::ps_is_running(handle) && Sys.time() < deadline) {
        Sys.sleep(.1)
    }
    expect_false(ps::ps_is_running(handle))
})

test_that("synchronous API command conditions reap the worker tree before rethrow", {
    alive <- TRUE
    reaped <- FALSE
    condition <- simpleError("foreground pipe failure")
    local_mocked_bindings(
        r_bg = function(..., supervise) {
            expect_true(supervise)
            list(
                is_alive = function() alive,
                poll_io = function(...) stop(condition),
                kill_tree = function() {
                    alive <<- FALSE
                    reaped <<- TRUE
                },
                wait = function(...) invisible(NULL)
            )
        },
        .package = "callr"
    )
    caught <- tryCatch(
        run_command(
            function() NULL,
            wd = tempdir(),
            wait = TRUE,
            echo = FALSE,
            call_r = TRUE,
            kill_tree = TRUE
        ),
        error = identity
    )
    expect_identical(caught, condition)
    expect_true(reaped)
    expect_false(alive)
})

test_that("empty failure messages retain API diagnostics and single-case columns", {
    failure <- run__api_failure(simpleError(""), tempdir(), NULL)
    expect_identical(failure$run$stderr, "Simulation pipeline failed.")
    expect_identical(failure$api$phase, "failed")
    condition <- structure(
        list(call = NULL),
        class = c("interrupt", "condition")
    )
    result <- run__api_failure(condition, tempdir(), NULL, TRUE)
    expect_match(result$run$stderr, "interrupted")
    state <- list(
        jobs = data.table(
            model = "m.idf",
            weather = list(NULL),
            output_dir = tempdir(),
            energyplus_exe = "energyplus",
            index = 1L
        ),
        options = list(backend = "api")
    )
    result <- run__api_failed_state(state, condition, TRUE)
    expect_true(all(c("stdout", "stderr") %in% names(result$jobs)))
    expect_match(result$jobs$stderr[[1L]], "interrupted")
})

test_that("raw background API process death automatically reaps its engine worker", {
    skip_if_not_installed("ps")
    fixture <- testrun__api_fixture()
    marker <- tempfile()
    setup <- local({
        path <- marker
        function(api, state) {
            api$runtime$callback_begin_new_environment(state, function(state) {
                writeLines(as.character(Sys.getpid()), path)
                Sys.sleep(60)
            })
            writeLines("started", paste0(path, ".started"))
        }
    })
    process <- energyplus(
        fixture$model,
        fixture$weather,
        output_dir = file.path(fixture$directory, "parent-death"),
        backend = setup,
        wait = FALSE,
        echo = FALSE,
        readvars = FALSE
    )
    on.exit(if (process$is_alive()) run__api_kill(process), add = TRUE)
    testrun__api_started(
        function() file.exists(paste0(marker, ".started")),
        function() process$is_alive()
    )
    deadline <- Sys.time() + 30
    while (!file.exists(marker) && Sys.time() < deadline) {
        Sys.sleep(.1)
    }
    expect_true(file.exists(marker))
    handle <- ps::ps_handle(as.integer(readLines(marker)))
    expect_true(ps::ps_is_running(handle))
    expect_true(process$kill())
    process$wait(1000)
    deadline <- Sys.time() + 10
    while (ps::ps_is_running(handle) && Sys.time() < deadline) {
        Sys.sleep(.1)
    }
    expect_false(ps::ps_is_running(handle))
})


# Every public execution entry exposes only the unified backend selector.
test_that("public runners expose one backend selector without setup compatibility", {
    methods <- lapply(
        list(Idf, EplusJob, EplusGroupJob, ParametricJob),
        function(class) {
            class$public_methods$run
        }
    )
    for (runner in c(
        list(EnergyPlus, energyplus, run_idf, run_multi),
        methods
    )) {
        expect_false("api_setup" %in% names(formals(runner)))
        expect_identical(formals(runner)$backend, "cli")
    }
    expect_error(
        energyplus("unused.idf", NULL, api_setup = function(api, state) NULL),
        "unused argument"
    )
    expect_error(
        run_idf("unused.idf", NULL, "unused", api_setup = NULL),
        "unused argument"
    )
})

# Check both orchestration modes using actual workers, rather than mocked forwarding.
test_that("function backends initialize once through low-level and standalone runners", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    for (runner in c("EnergyPlus", "energyplus", "run_idf")) {
        for (wait in c(TRUE, FALSE)) {
            directory <- file.path(
                fixture$directory,
                paste(
                    match(runner, c("EnergyPlus", "energyplus", "run_idf")),
                    wait,
                    sep = "-"
                )
            )
            marker <- paste0(directory, ".initialized")
            setup <- local({
                file <- marker
                # A non-NULL return is ignored; initialization still runs once before simulation.
                function(api, state) {
                    write(as.character(Sys.getpid()), file, append = TRUE)
                    "ignored initialization result"
                }
            })
            args <- list(
                model = fixture$model,
                weather = fixture$weather,
                output_dir = directory,
                backend = setup,
                wait = wait,
                echo = FALSE
            )
            if (runner == "energyplus") {
                args$readvars <- FALSE
            }
            result <- do.call(get(runner), args)
            if (!wait) {
                process <- if (runner == "EnergyPlus") {
                    result$run$process
                } else {
                    result
                }
                on.exit(
                    if (process$is_alive()) run__api_kill(process),
                    add = TRUE
                )
                process$wait(180000)
                expect_false(process$is_alive())
                result <- process$get_result()
            }
            metadata <- if (runner == "EnergyPlus") {
                result$run$api
            } else {
                result$api
            }
            expect_identical(
                metadata$phase,
                "completed",
                info = jsonlite::toJSON(
                    metadata,
                    auto_unbox = TRUE,
                    null = "null"
                )
            )
            expect_length(readLines(marker), 1L)
            expect_identical(as.integer(readLines(marker)), metadata$pid)
            expect_false(identical(metadata$pid, Sys.getpid()))
            expect_identical(result$backend, "api")
        }
    }
})


# Stock CLI results expose the same engine identity without native API metadata.
test_that("standalone CLI results retain a string backend identity", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    for (runner in c("EnergyPlus", "energyplus", "run_idf")) {
        directory <- file.path(
            fixture$directory,
            paste0(
                "cli-",
                match(runner, c("EnergyPlus", "energyplus", "run_idf"))
            )
        )
        args <- list(
            model = fixture$model,
            weather = fixture$weather,
            output_dir = directory,
            backend = "cli",
            echo = FALSE
        )
        if (runner == "energyplus") {
            args$readvars <- FALSE
        }
        result <- do.call(get(runner), args)
        expect_identical(result$backend, "cli")
        expect_null(result$api)
    }
})
