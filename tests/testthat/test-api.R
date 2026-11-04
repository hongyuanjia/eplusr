describe("api", {
    test_that("API metadata and state ownership are explicit", {
        ctx <- testapi__fake()
        api <- ctx$api
        expect_s3_class(api, "EnergyPlusAPI")
        expect_equal(api$info()$version, "26.1.0-fake")
        expect_equal(nrow(api$methods()), 155L)
        state <- api$state_manager$new_state()
        expect_equal(api$state_manager$status(state)$phase, "created")
        other <- testapi__fake()
        expect_error(
            other$api$state_manager$delete_state(state),
            "different API"
        )
        expect_error(api$state_manager$reset_state(1), "managed")
        api$state_manager$delete_state(state)
        api$state_manager$delete_state(state)
        expect_equal(ctx$deleted, 1L)
        expect_error(api$exchange$year(state), "closed")
        api$close()
        api$close()
        expect_equal(ctx$unloaded, 1L)
        expect_error(api$state_manager$new_state(), "closed")
        other$api$close()
    })

    test_that("functional resources invalidate across reset and state changes", {
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        other <- api$state_manager$new_state()
        glycol <- api$functional$glycol(state, "water")
        refrigerant <- api$functional$refrigerant(state, "R22")
        psych <- api$functional$psychrometrics(state)
        expect_equal(glycol$specific_heat(state, 35), 0)
        expect_equal(refrigerant$saturation_pressure(state, 25), 0)
        expect_equal(psych$relative_humidity_b(state, 24, .009, 101325), 0)
        expect_error(glycol$density(other, 35), "Functional object")
        expect_error(api$functional$glycol(state, NA_character_), "string")
        api$functional$initialize(state)
        api$state_manager$reset_state(state)
        expect_error(
            psych$density(state, 101325, 24, .009),
            "Functional object"
        )
        expect_equal(api$state_manager$status(state)$generation, 1L)
        next_glycol <- api$functional$glycol(state, "water")
        next_glycol$delete(state)
        expect_error(next_glycol$density(state, 35), "closed")
        ctx$null_resource <- TRUE
        expect_error(
            api$functional$refrigerant(state, "missing"),
            "Unknown EnergyPlus"
        )
        api$close()
        expect_equal(ctx$deleted, 2L)
    })

    test_that("exchange handles are checked by state, kind, generation and API flag", {
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        entry <- attr(state, "entry")
        expect_error(
            api$exchange$get_variable_handle(state, "T", "Z"),
            "initialized"
        )
        entry$running <- TRUE
        entry$pointer$ready <- TRUE
        handle <- api$exchange$get_variable_handle(state, "T", "Z")
        expect_s3_class(handle, "EnergyPlusHandle")
        expect_equal(api$exchange$get_variable_value(state, handle), 0)
        expect_error(api$exchange$get_meter_value(state, handle), "data kind")
        expect_error(api$exchange$get_variable_value(state, 1L), "data kind")
        ctx$bad_value <- TRUE
        expect_error(
            api$exchange$get_variable_value(state, handle),
            "invalid exchange"
        )
        expect_true(api$exchange$api_error_flag(state))
        api$exchange$reset_api_error_flag(state)
        expect_false(api$exchange$api_error_flag(state))
        ctx$bad_value <- FALSE
        expect_error(
            api$exchange$today_weather_outdoor_dry_bulb_at_time(state, -1L, 1L),
            "hour"
        )
        expect_error(
            api$exchange$today_weather_outdoor_dry_bulb_at_time(state, 0L, 0L),
            "interval"
        )
        expect_error(
            api$exchange$today_weather_outdoor_dry_bulb_at_time(state, 0L, 5L),
            "interval"
        )
        expect_equal(
            api$exchange$today_weather_outdoor_dry_bulb_at_time(state, 0L, 1L),
            0
        )
        entry$running <- FALSE
        api$state_manager$reset_state(state)
        entry$running <- TRUE
        entry$pointer$ready <- TRUE
        expect_error(
            api$exchange$get_variable_value(state, handle),
            "generation"
        )
        entry$running <- FALSE
        api$close()
    })

    test_that("API data arrays are copied and freed on success and R conversion failure", {
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        entry <- attr(state, "entry")
        entry$running <- TRUE
        entry$pointer$ready <- TRUE
        data <- api$exchange$get_api_data(state)
        expect_equal(names(data), c("what", "name", "key", "type", "unit"))
        expect_equal(data$name, "Temperature")
        # Honor a generated padded layout for every row rather than assuming five packed pointers.
        ctx$rows <- 2L
        ctx$api_data$size <- 64L
        ctx$api_data$fields$offset <- c(0L, 16L, 24L, 32L, 48L)
        offsets <- integer()
        ctx$ffi$unpack <- function(pointer, offset, type) {
            offsets <<- c(offsets, offset)
            c("OutputVariable", "Temperature", "ZONE ONE", "", "C")[
                match(offset %% 64L, ctx$api_data$fields$offset)
            ]
        }
        expect_equal(
            api$exchange$get_api_data(state)$name,
            rep("Temperature", 2L)
        )
        expect_equal(
            offsets,
            c(ctx$api_data$fields$offset, 64L + ctx$api_data$fields$offset)
        )
        ctx$rows <- 0L
        expect_equal(nrow(api$exchange$get_api_data(state)), 0L)
        ctx$rows <- 1L
        ctx$ffi$string <- function(pointer) stop("decode failed")
        expect_error(api$exchange$get_api_data(state), "decode failed")
        freed <- Filter(function(x) x$symbol == "freeAPIData", ctx$log)
        expect_equal(length(freed), 4L)
        entry$running <- FALSE
        api$close()
    })

    test_that("callback errors and interrupts rethrow only after native state deletion", {
        for (condition in list(
            simpleError("controller error"),
            structure(
                list(message = "controller interrupt", call = NULL),
                class = c("interrupt", "condition")
            )
        )) {
            ctx <- testapi__fake()
            api <- ctx$api
            state <- api$state_manager$new_state()
            count <- 0L
            api$runtime$callback_begin_system_timestep_before_predictor(
                state,
                function(s) {
                    count <<- count + 1L
                    stop(condition)
                }
            )
            result <- tryCatch(
                api$runtime$run_energyplus(state, testapi__model()),
                error = identity,
                interrupt = identity
            )
            expect_identical(result, condition)
            expect_identical(
                api$state_manager$status(state)$stop_reason,
                if (inherits(condition, "interrupt")) {
                    "interrupt"
                } else {
                    "callback_error"
                }
            )
            expect_equal(count, 1L)
            expect_equal(ctx$deleted, 1L)
            expect_equal(api$state_manager$status(state)$phase, "closed")
            expect_null(api__running$state)
            api$close()
        }
    })

    test_that("close, reset, deletion and recursive runs are forbidden inside callbacks", {
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        api$runtime$callback_begin_new_environment(state, function(s) {
            expect_error(api$close(), "running")
            expect_error(api$state_manager$delete_state(s), "running")
            expect_error(api$state_manager$reset_state(s), "running")
            expect_error(
                api$runtime$run_energyplus(s, testapi__model()),
                "recursively"
            )
        })
        expect_equal(api$runtime$run_energyplus(state, testapi__model()), 0L)
        expect_equal(api$state_manager$status(state)$phase, "completed")
        expect_error(
            api$runtime$run_energyplus(state, testapi__model()),
            "reset"
        )
        api$state_manager$reset_state(state)
        expect_length(attr(state, "entry")$callbacks, 0L)
        expect_equal(api$runtime$run_energyplus(state, testapi__model()), 0L)
        api$close()
    })

    test_that("functional callbacks also protect their active native frame", {
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        condition <- simpleError("functional handler")
        api$functional$callback_error(state, function(level, message) {
            expect_error(api$state_manager$reset_state(state), "running")
            expect_error(api$close(), "running")
            stop(condition)
        })
        psych <- api$functional$psychrometrics(state)
        ctx$hook <- function(symbol, args) {
            if (symbol == "psyCpAirFnW") {
                args[[1L]]$callbacks$registerErrorCallback(2L, "warning")
            }
        }
        result <- tryCatch(psych$specific_heat(state, .01), error = identity)
        expect_identical(result, condition)
        expect_equal(ctx$deleted, 1L)
        api$close()
    })

    test_that("user stops, input rejection and unknown native unwinds remain distinct", {
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        expect_error(
            api$runtime$run_energyplus(state, NA_character_),
            "character vector"
        )
        expect_error(
            api$runtime$run_energyplus(state, "missing.idf"),
            "existing"
        )
        expect_error(
            api$runtime$run_energyplus(
                state,
                testapi__model("PythonPlugin:Instance,x;")
            ),
            "PythonPlugin"
        )
        expect_error(
            api$runtime$run_energyplus(
                state,
                testapi__model('{"PythonPlugin:Instance":{}}')
            ),
            "PythonPlugin"
        )
        expect_error(
            api$runtime$callback_begin_new_environment(state, 1),
            "R function"
        )
        api$runtime$callback_begin_new_environment(state, function(s) {
            api$runtime$stop_simulation(s)
        })
        api$runtime$run_energyplus(state, testapi__model())
        expect_equal(api$state_manager$status(state)$phase, "stopped")
        expect_equal(api$state_manager$status(state)$stop_reason, "user_stop")
        api$state_manager$reset_state(state)
        ctx$fail_native <- TRUE
        expect_error(
            api$runtime$run_energyplus(state, testapi__model()),
            "native unwind"
        )
        expect_equal(api$state_manager$status(state)$phase, "quarantined")
        expect_error(api$close(), "quarantined")
        expect_error(api$exchange$year(state), "quarantined")
        expect_null(api__running$state)
        ctx$quarantined <- FALSE
        api$close()
    })

    test_that("argument validation never forwards arbitrary R objects", {
        expect_error(
            api__arguments("Z)v", list(NA_character_), "name"),
            "string"
        )
        expect_error(api__arguments("d)v", list(new.env()), "value"), "finite")
        expect_error(api__arguments("i)v", list(1.5), "value"), "integer")
        expect_error(api__arguments("I)v", list(-1), "value"), "integer")
        expect_equal(
            api__arguments("id)v", list(TRUE, 2L), c("flag", "value")),
            list(1L, 2)
        )
    })

    test_that("native callback proxies reject foreign threads without entering R", {
        skip_if_not_installed("rdyncall")
        directory <- tempfile("eplusr-guard-fixture-")
        dir.create(directory)
        source <- test_path("fixtures", "callback-thread.c")
        file.copy(source, file.path(directory, "callback-thread.c"))
        old <- setwd(directory)
        on.exit(setwd(old), add = TRUE)
        compile <- system2(
            file.path(R.home("bin"), "R"),
            c("CMD", "SHLIB", "callback-thread.c"),
            stdout = TRUE,
            stderr = TRUE
        )
        expect_null(
            attr(compile, "status"),
            info = paste(compile, collapse = "\n")
        )
        library <- rdyncall::dynload(
            file.path(
                directory,
                paste0("callback-thread", .Platform$dynlib.ext)
            ),
            auto.unload = FALSE
        )
        on.exit(rdyncall::dynunload(library), add = TRUE)
        owner <- rdyncall::dynsym(library, "callback_on_owner")
        worker <- rdyncall::dynsym(library, "callback_on_worker")
        signatures <- c("p)v", "i)v", "Z)v", "iZ)v")
        for (kind in seq_along(signatures)) {
            count <- 0L
            original <- rdyncall::ccallback(signatures[kind], function(...) {
                count <<- count + 1L
                invisible(NULL)
            })
            guard <- .Call(
                "eplusr_guard_create",
                original,
                signatures[kind],
                PACKAGE = "eplusr"
            )
            rdyncall::dyncall(owner, "pi)v", guard, kind - 1L)
            expect_equal(count, 1L)
            expect_equal(
                .Call("eplusr_guard_foreign", guard, PACKAGE = "eplusr"),
                0
            )
            expect_equal(
                rdyncall::dyncall(worker, "pi)i", guard, kind - 1L),
                0L
            )
            expect_equal(count, 1L)
            expect_equal(
                .Call("eplusr_guard_foreign", guard, PACKAGE = "eplusr"),
                1
            )
            expect_identical(
                .Call("eplusr_guard_original", guard, PACKAGE = "eplusr"),
                original
            )
            .Call("eplusr_guard_release", guard, PACKAGE = "eplusr")
            expect_error(
                .Call("eplusr_guard_foreign", guard, PACKAGE = "eplusr"),
                "closed"
            )
        }
        original <- rdyncall::ccallback("p)v", function(s) NULL)
        expect_error(
            .Call("eplusr_guard_create", 1L, "p)v", PACKAGE = "eplusr"),
            "live"
        )
        expect_error(
            .Call("eplusr_guard_create", original, ")i", PACKAGE = "eplusr"),
            "Unsupported"
        )
        guards <- lapply(seq_len(256L), function(i) {
            .Call("eplusr_guard_create", original, "p)v", PACKAGE = "eplusr")
        })
        expect_error(
            .Call("eplusr_guard_create", original, "p)v", PACKAGE = "eplusr"),
            "256"
        )
        invisible(lapply(guards, function(g) {
            .Call("eplusr_guard_release", g, PACKAGE = "eplusr")
        }))
    })

    test_that("thread and bridge diagnostics stop control without a false completion result", {
        for (kind in c("foreign", "bridge", "interrupt")) {
            ctx <- testapi__fake()
            api <- ctx$api
            state <- api$state_manager$new_state()
            invoked <- FALSE
            api$runtime$callback_begin_new_environment(state, function(s) {
                invoked <<- TRUE
            })
            ctx$ffi$diagnostic <- function(callback) {
                list(
                    foreign = as.integer(kind == "foreign"),
                    bridge = list(
                        error_invocations = as.integer(kind != "foreign"),
                        disable_reason = if (kind == "interrupt") {
                            "r_interrupt"
                        } else {
                            "boundary_error"
                        }
                    )
                )
            }
            failure <- tryCatch(
                api$runtime$run_energyplus(state, testapi__model()),
                error = identity,
                interrupt = identity
            )
            expect_s3_class(
                failure,
                if (kind == "interrupt") "interrupt" else "error"
            )
            expect_match(
                conditionMessage(failure),
                "foreign thread|bridge disabled"
            )
            expect_identical(
                api$state_manager$status(state)$stop_reason,
                if (kind == "interrupt") "interrupt" else "callback_error"
            )
            expect_false(invoked)
            expect_equal(api$state_manager$status(state)$phase, "closed")
            expect_equal(ctx$deleted, 1L)
            api$close()
        }
    })

    test_that("library loading audits identity and releases failed probe states", {
        skip_if_not_installed("rdyncall")
        ctx <- testapi__loader()
        local_mocked_bindings(
            dynload = function(...) if (ctx$load) ctx$pointer else NULL,
            dynport_load_into = function(portfile, envir, libhandle = NULL) {
                ctx$handle <- libhandle
                if (ctx$symbols) {
                    for (symbol in setdiff(
                        names(testapi__signatures()),
                        ctx$omit
                    )) {
                        # Capture each binding separately to preserve its own symbol and ABI.
                        binding <- local({
                            name <- symbol
                            signature <- testapi__signatures()[[symbol]]
                            f <- function(...) ctx$call(name, signature, ...)
                            attr(f, "signature") <- signature
                            f
                        })
                        envir[[symbol]] <- binding
                    }
                }
                if (ctx$layout) {
                    envir$APIDataEntry <- testapi__layout()
                    if (isTRUE(ctx$bad_fields)) {
                        envir$APIDataEntry$fields$name[5L] <- "wrong"
                    }
                }
                invisible(envir)
            },
            dynunload = function(...) {
                ctx$unloaded <- ctx$unloaded + 1L
            },
            is.nullptr = is.null,
            .package = "rdyncall"
        )
        expect_error(api__load(tempfile()), "not found")
        ctx$load <- FALSE
        expect_error(api__load(ctx$directory), "Cannot load")
        ctx$load <- TRUE
        ctx$symbols <- FALSE
        expect_error(api__load(ctx$directory), "Missing required")
        ctx$symbols <- TRUE
        ctx$layout <- FALSE
        expect_error(api__load(ctx$directory), "APIDataEntry layout")
        ctx$layout <- TRUE
        ctx$bad_fields <- TRUE
        expect_error(api__load(ctx$directory), "APIDataEntry layout")
        ctx$bad_fields <- FALSE
        ctx$version <- "EnergyPlus, Version 26.2.0-test"
        expect_error(api__load(ctx$directory), "not EnergyPlus 26.1")
        ctx$version <- "EnergyPlus, Version 26.1.0-test"
        ctx$null_state <- TRUE
        expect_error(api__load(ctx$directory), "identity-check state")
        ctx$null_state <- FALSE
        ctx$fail_identity <- TRUE
        expect_error(api__load(ctx$directory), "identity conversion")
        expect_equal(ctx$deleted, 1L)
        ctx$fail_identity <- FALSE
        ctx$api_version <- "wrong"
        expect_error(api__load(ctx$directory), "API version")
        expect_equal(ctx$deleted, 2L)
        ctx$api_version <- "0.2"
        # Managed metadata avoids its native getter; a restricted trend getter remains optional.
        ctx$omit <- c("inputFilePath", "getPluginTrendVariableValue")
        api <- api__load(ctx$directory)
        expect_match(api$info()$version, "26.1.0")
        methods <- api$methods()
        expect_false("inputFilePath" %in% methods$symbol)
        expect_identical(
            methods$signature[methods$symbol == "getPluginTrendVariableValue"],
            ""
        )
        state <- api$state_manager$new_state()
        api$runtime$issue_text(state, "positional arguments")
        expect_null(names(ctx$arguments))
        # Create and diagnose the real guarded trampoline, while registration is mocked.
        api$runtime$callback_begin_new_environment(state, function(s) NULL)
        api$state_manager$delete_state(state)
        api$close()
        expect_equal(ctx$deleted, 4L)
        expect_identical(ctx$handle, ctx$pointer)
        expect_equal(ctx$unloaded, 8L)
        ctx$version <- "EnergyPlus, Version 26.2.0-test"
        api <- api__load(ctx$directory, "26.2.0")
        expect_match(api$info()$version, "26.2.0")
        expect_identical(api$info()$abi, "26.1-26.2")
        api$close()
        # Native loading failures must restore both an inherited PATH and an unset PATH.
        for (path in list(Sys.getenv("PATH"), NA_character_)) {
            withr::with_envvar(c(PATH = path), {
                ctx$load <- FALSE
                expect_error(api__load(ctx$directory), "Cannot load")
                expect_identical(
                    Sys.getenv("PATH", unset = NA_character_),
                    path
                )
            })
        }
    })

    test_that("constructor uses installation discovery and rejects unreviewed versions", {
        local_mocked_bindings(
            is_eplus_path = function(path) {
                identical(path, "stock-installation")
            },
            use_eplus = function(path) list(version = "26.1.0", dir = path),
            eplus_config = function(version) {
                list(version = paste0(version, ".0"), dir = "discovered")
            },
            api__check_bridge = function() invisible(NULL),
            api__load = function(directory, version) directory
        )
        expect_equal(eplus_api("stock-installation"), "stock-installation")
        expect_equal(eplus_api("26.1"), "discovered")
        expect_equal(eplus_api("26.2"), "discovered")
        expect_equal(eplus_api("25.2"), "discovered")
        expect_error(eplus_api("9.3"), "installation")
    })

    test_that("bridge safety gate handles missing, already loaded, failing and cached builds", {
        available <- FALSE
        loaded <- FALSE
        # Limit base replacements to this test; avoid ever entering an unsafe real callback.
        local_mocked_bindings(
            requireNamespace = function(...) available,
            isNamespaceLoaded = function(...) loaded,
            .package = "base"
        )
        expect_error(api__check_bridge(), "Install a rdyncall build")
        available <- TRUE
        local_mocked_bindings(
            dynport_load_into = function(portfile, envir) NULL,
            .package = "rdyncall"
        )
        expect_error(api__check_bridge(), "existing-handle")
        local_mocked_bindings(
            dynport_load_into = function(portfile, envir, libhandle = NULL) {
                NULL
            },
            .package = "rdyncall"
        )
        loaded <- TRUE
        saved <- as.list(api__bridge_cache)
        rm(list = ls(api__bridge_cache), envir = api__bridge_cache)
        on.exit(
            {
                rm(list = ls(api__bridge_cache), envir = api__bridge_cache)
                list2env(saved, api__bridge_cache)
            },
            add = TRUE
        )
        expect_error(api__check_bridge(), "Restart R")
        loaded <- FALSE
        safe <- FALSE
        local_mocked_bindings(
            r = function(..., timeout) {
                expect_identical(timeout, 120)
                if (identical(safe, "error")) {
                    stop("probe child failed")
                }
                safe
            },
            .package = "callr"
        )
        expect_error(api__check_bridge(), "complete callback")
        safe <- "error"
        expect_error(
            api__check_bridge(),
            "safety probe failed: probe child failed"
        )
        safe <- rep(TRUE, 3L)
        expect_null(api__check_bridge())
        loaded <- TRUE
        expect_null(api__check_bridge())
        key <- ls(api__bridge_cache)[1L]
        api__bridge_cache[[key]]$namespace <- new.env(parent = emptyenv())
        expect_error(api__check_bridge(), "Restart R")
    })

    test_that("bridge probe requires all three diagnostic contracts", {
        skip_if_not_installed("rdyncall")
        reason <- NULL
        local_mocked_bindings(
            ccallback = function(signature, fun) fun,
            dyncall = function(fun, signature) {
                result <- tryCatch(
                    fun(),
                    error = identity,
                    interrupt = identity
                )
                reason <<- if (inherits(result, "interrupt")) {
                    "r_interrupt"
                } else if (inherits(result, "error")) {
                    "r_error"
                } else {
                    "boundary_error"
                }
                0L
            },
            callback_status = function(...) {
                list(
                    disabled = TRUE,
                    disable_reason = reason,
                    error_invocations = 1
                )
            },
            .package = "rdyncall"
        )
        previous <- getOption("warn")
        expect_true(all(api__probe_bridge()))
        expect_identical(getOption("warn"), previous)
    })

    test_that("process and pointer guards prevent restored or foreign ownership", {
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        entry <- attr(state, "entry")
        ctx$pid <- -1L
        expect_error(api$exchange$year(state), "another R process")
        expect_error(api$state_manager$status(state), "another R process")
        expect_error(api$close(), "another R process")
        expect_error(api__new_state(ctx), "owning R process")
        ctx$pid <- Sys.getpid()
        saved <- entry$pointer
        entry$pointer <- NULL
        expect_error(api$exchange$year(state), "serialized")
        entry$pointer <- saved
        ctx$ffi$live <- function() FALSE
        expect_error(api$exchange$year(state), "serialize API")
        expect_error(api$close(), "serialize API")
        ctx$ffi$live <- function() TRUE
        old_call <- ctx$call
        ctx$call <- function(...) NULL
        expect_error(api__new_state(ctx), "allocate a state")
        ctx$call <- old_call
        ctx$native_depth <- 1L
        expect_error(api__new_state(ctx), "active native")
        ctx$native_depth <- 0L
        expect_match(api$functional$ep_version(), "26.1")
        expect_error(
            api$exchange$get_object_names(state, "Zone"),
            "initialized"
        )
        api$close()
        expect_error(api__new_state(ctx), "closed")
    })

    test_that("unreferenced APIs and unregistered states reach deterministic fallback cleanup", {
        ctx <- testapi__fake()
        state <- ctx$api$state_manager$new_state()
        rm(state)
        gc()
        expect_equal(ctx$deleted, 1L)
        ctx$api$state_manager$new_state()
        # Release the full self-referencing API graph, retaining only an external observer.
        observer <- new.env()
        ctx$ffi$unload <- local({
            target <- observer
            function() target$unloaded <- TRUE
        })
        rm(ctx)
        gc()
        expect_true(observer$unloaded)
    })

    test_that("standalone diagnostics and non-callback interrupts cannot report success", {
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        api$functional$callback_error(state, function(...) NULL)
        ctx$ffi$diagnostic <- function(callback) {
            list(foreign = 1L, bridge = list(error_invocations = 0L))
        }
        expect_error(api$functional$psychrometrics(state), "foreign thread")
        expect_equal(ctx$deleted, 1L)
        api$close()
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        native <- ctx$call
        ctx$call <- function(symbol, arguments = list()) {
            if (symbol == "energyplus") {
                stop(structure(
                    list(message = "outer interrupt", call = NULL),
                    class = c("interrupt", "condition")
                ))
            }
            native(symbol, arguments)
        }
        result <- tryCatch(
            api$runtime$run_energyplus(state, testapi__model()),
            interrupt = identity
        )
        expect_s3_class(result, "interrupt")
        expect_true(ctx$quarantined)
        expect_equal(ctx$deleted, 0L)
        ctx$quarantined <- FALSE
        api$close()
    })

    test_that("resource retirement, late registration and engine failure have explicit results", {
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        glycol <- api$functional$glycol(state, "water")
        api$runtime$run_energyplus(state, testapi__model())
        expect_error(glycol$density(state, 25), "Functional object")
        expect_error(
            api$exchange$request_variable(state, "T", "Z"),
            "before running"
        )
        expect_error(
            api$runtime$callback_begin_new_environment(state, function(s) NULL),
            "Register callbacks"
        )
        api$state_manager$reset_state(state)
        entry <- attr(state, "entry")
        cause <- simpleError("first condition")
        api$runtime$callback_begin_new_environment(state, function(s) NULL)
        entry$condition <- cause
        # The callback returns without calling user code after the first recorded condition.
        result <- tryCatch(
            api$runtime$run_energyplus(state, testapi__model()),
            error = identity
        )
        expect_identical(result, cause)
        api$close()
        ctx <- testapi__fake()
        state <- ctx$api$state_manager$new_state()
        native <- ctx$call
        ctx$call <- function(symbol, arguments = list()) {
            if (symbol == "energyplus") 1L else native(symbol, arguments)
        }
        expect_equal(
            ctx$api$runtime$run_energyplus(state, testapi__model()),
            1L
        )
        expect_equal(
            ctx$api$state_manager$status(state)$stop_reason,
            "engine_error"
        )
        ctx$api$close()
    })

    test_that("null API data and custom callback names cannot bypass validation", {
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        spec <- Filter(
            function(x) x$method == "callback_user_defined_component_model",
            ctx$manifest$methods
        )[[1L]]
        expect_null(api__callback(
            ctx,
            attr(state, "entry"),
            state,
            spec,
            function(s) NULL,
            list(program_name = "program")
        ))
        entry <- attr(state, "entry")
        entry$running <- TRUE
        entry$pointer$ready <- TRUE
        ctx$ffi$unpack <- function(...) NULL
        expect_equal(api$exchange$get_api_data(state)$name, "")
        native <- ctx$call
        ctx$call <- function(symbol, arguments = list()) {
            if (symbol == "getAPIData") NULL else native(symbol, arguments)
        }
        expect_error(api$exchange$get_api_data(state), "null API data")
        entry$running <- FALSE
        api$close()
    })

    test_that("platform library names match the installation ABI layouts", {
        expect_equal(
            api__library_name("windows", "Windows"),
            "EnergyPlusAPI.dll"
        )
        expect_equal(
            api__library_name("unix", "Darwin"),
            "libenergyplusapi.dylib"
        )
        expect_equal(api__library_name("unix", "Linux"), "libenergyplusapi.so")
        binding <- api_bind__select("26.1-26.2")
        binding$pointer_bytes <- 4L
        local_mocked_bindings(api_bind__select = function(abi) binding)
        expect_error(api__load(tempfile()), "64-bit")
    })

    test_that("plugin rejection respects IDF comments and case-insensitive object names", {
        ctx <- testapi__fake()
        state <- ctx$api$state_manager$new_state()
        for (text in c(
            "Version,26.1;\n! configuration\nPythonPlugin:Instances,x;",
            "Version,26.1;\npythonplugin:instances,x;"
        )) {
            expect_error(
                ctx$api$runtime$run_energyplus(state, testapi__model(text)),
                "PythonPlugin"
            )
        }
        ctx$api$close()
    })

    test_that("nested functional callbacks defer cleanup to the outer native return", {
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        psych <- api$functional$psychrometrics(state)
        cause <- simpleError("nested property callback")
        depth <- 0L
        api$functional$callback_error(state, function(level, message) {
            depth <<- depth + 1L
            if (depth == 1L) {
                psych$specific_heat(state, .01)
                expect_equal(ctx$deleted, 0L)
                expect_gt(ctx$native_depth, 0L)
            } else {
                stop(cause)
            }
        })
        ctx$hook <- function(symbol, arguments) {
            if (symbol == "psyCpAirFnW") {
                arguments[[1L]]$callbacks$registerErrorCallback(2L, "nested")
            }
        }
        result <- tryCatch(psych$specific_heat(state, .01), error = identity)
        expect_identical(result, cause)
        expect_equal(ctx$deleted, 1L)
        expect_equal(depth, 2L)
        api$close()
    })

    test_that("stop and nested simulation commands require an active outer runtime", {
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        expect_error(api$runtime$stop_simulation(state), "active runtime")
        ctx$native_depth <- 1L
        expect_error(
            api$runtime$run_energyplus(state, testapi__model()),
            "recursively"
        )
        ctx$native_depth <- 0L
        api$close()
    })

    # A bridge interrupt may be observable only after the last native callback or a standalone property call.
    test_that("late bridge interrupts retain their category after native return", {
        for (mode in c("run", "functional")) {
            ctx <- testapi__fake()
            api <- ctx$api
            state <- api$state_manager$new_state()
            if (mode == "run") {
                api$runtime$callback_begin_new_environment(state, function(s) {
                    NULL
                })
            } else {
                api$functional$callback_error(state, function(...) NULL)
            }
            interrupted <- FALSE
            native <- ctx$call
            ctx$call <- function(symbol, arguments = list()) {
                result <- native(symbol, arguments)
                if (
                    symbol ==
                        if (mode == "run") {
                            "energyplus"
                        } else {
                            "initializeFunctionalAPI"
                        }
                ) {
                    interrupted <<- TRUE
                }
                result
            }
            ctx$ffi$diagnostic <- function(callback) {
                list(
                    foreign = 0L,
                    bridge = list(
                        error_invocations = as.integer(interrupted),
                        disable_reason = "r_interrupt"
                    )
                )
            }
            result <- tryCatch(
                if (mode == "run") {
                    api$runtime$run_energyplus(state, testapi__model())
                } else {
                    api$functional$psychrometrics(state)
                },
                error = identity,
                interrupt = identity
            )
            expect_s3_class(result, "interrupt")
            expect_identical(
                api$state_manager$status(state)$stop_reason,
                "interrupt"
            )
            expect_identical(api$state_manager$status(state)$phase, "closed")
            expect_equal(ctx$deleted, 1L)
            expect_false(ctx$quarantined)
            api$close()
        }
    })

    # A stock property error can enter rdyncall's boundary before the managed R handler runs.
    test_that("stock functional bridge interrupts return safely with their original category", {
        skip_if(
            identical(Sys.getenv("_EPLUSR_TEST_API_VERSION_"), "9.4"),
            "9.4 requires a package-owned backend/session worker; dedicated worker tests cover its contract"
        )
        fixture <- testrun__api_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        result <- callr::r(
            function(directory) {
                api <- eplusr::eplus_api(directory)
                on.exit(api$close(), add = TRUE)
                ctx <- environment(api$close)$ctx
                original <- ctx$ffi$callback
                enabled <- FALSE
                # Inject before the managed wrapper, while retaining the real native guard and rdyncall trampoline.
                ctx$ffi$callback <- function(signature, fun) {
                    original(signature, function(...) {
                        if (enabled) {
                            stop(structure(
                                list(
                                    message = "bridge interrupt probe",
                                    call = NULL
                                ),
                                class = c("interrupt", "condition")
                            ))
                        }
                        fun(...)
                    })
                }
                state <- api$state_manager$new_state()
                api$functional$callback_error(state, function(...) NULL)
                fluid <- api$functional$glycol(state, "WATER")
                enabled <- TRUE
                condition <- tryCatch(
                    fluid$specific_heat(state, -1000),
                    error = identity,
                    interrupt = identity
                )
                list(
                    class = class(condition),
                    status = api$state_manager$status(state),
                    quarantined = ctx$quarantined
                )
            },
            args = list(
                eplus_config(Sys.getenv(
                    "_EPLUSR_TEST_API_VERSION_",
                    "26.1"
                ))$dir
            ),
            libpath = .libPaths()
        )
        expect_true("interrupt" %in% result$class)
        expect_identical(result$status$stop_reason, "interrupt")
        expect_identical(result$status$phase, "closed")
        expect_false(result$quarantined)
    })

    # The manifest distinguishes native ownership defects from mode-dependent capabilities.
    test_that("audited API methods expose the remaining native safety restrictions", {
        ctx <- testapi__fake()
        on.exit(ctx$api$close(), add = TRUE)
        methods <- ctx$api$methods()
        blocked <- methods$method[methods$status == "unsupported"]
        expect_identical(blocked, "get_trend_value")
        expect_equal(sum(methods$plugin_only), 10L)
        expect_equal(sum(methods$status == "supported"), 154L)
        state <- ctx$api$state_manager$new_state()
        for (method in methods$method[
            methods$plugin_only & methods$status == "supported"
        ]) {
            expect_error(ctx$api$exchange[[method]](state), "executable-hosted")
        }
        expect_error(ctx$api$exchange$get_trend_value(state), "out of bounds")
    })

    # Borrowed plugin handles must remain state- and kind-bound before entering native arrays.
    test_that("plugin exchange checks handles, missing globals and aggregate ranges", {
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        entry <- attr(state, "entry")
        ctx$plugin_mode <- TRUE
        entry$running <- TRUE
        entry$pointer$ready <- TRUE
        on.exit(
            {
                entry$running <- FALSE
                api$close()
            },
            add = TRUE
        )
        available <- "R Shared"
        local_mocked_bindings(api__data = function(...) {
            data.frame(what = "PluginGlobalVariable", key = available)
        })
        global <- api$exchange$get_global_handle(state, "r shared")
        trend <- api$exchange$get_trend_handle(state, "R History")
        expect_identical(attr(global, "kind"), "plugin_global")
        expect_identical(attr(trend, "kind"), "plugin_trend")
        missing <- api$exchange$get_global_handle(state, "missing")
        expect_identical(as.integer(missing), -1L)
        expect_error(
            api$exchange$get_global_value(state, missing),
            "negative handle"
        )
        expect_error(
            api$exchange$set_global_value(state, trend, 2),
            "data kind"
        )
        missing_trend <- api$exchange$get_trend_handle(state, "missing")
        expect_error(
            api$exchange$get_trend_sum(state, missing_trend, 2),
            "negative handle"
        )
        expect_error(api$exchange$get_trend_sum(state, global, 2), "data kind")
        expect_error(api$exchange$get_trend_sum(state, trend, 1), "count >= 2")
        expect_error(api$exchange$get_trend_sum(state, trend, 2.5), "C integer")
        expect_error(api$exchange$get_trend_sum(state, trend, Inf), "finite")
        api$exchange$set_global_value(state, global, 3)
        expect_equal(api$exchange$get_global_value(state, global), 0)
        for (method in c(
            "get_trend_average",
            "get_trend_min",
            "get_trend_max",
            "get_trend_sum",
            "get_trend_direction"
        )) {
            expect_equal(api$exchange[[method]](state, trend, 2), 0)
        }
        # Unicode case folding must never admit a name that the byte-oriented native lookup cannot find.
        available <- "\u00e9 SHARED"
        accent <- api$exchange$get_global_handle(state, "\u00e9 shared")
        expect_identical(as.integer(accent), 1L)
        calls <- Filter(
            function(x) x$symbol == "getPluginGlobalVariableHandle",
            ctx$log
        )
        expect_identical(
            utils::tail(calls, 1L)[[1L]]$arguments[[2L]],
            available
        )
        expect_identical(
            as.integer(api$exchange$get_global_handle(state, "\u00c9 shared")),
            -1L
        )
        entry$generation <- entry$generation + 1L
        expect_error(api$exchange$get_global_value(state, global), "generation")
        expect_error(api$exchange$get_trend_sum(state, trend, 2), "generation")
    })

    # External HVAC callbacks share the same deferred-error, GC and orderly-stop boundary.
    test_that("stock external HVAC manager callback stops and reports callback failures", {
        fixture <- testapi__controlled_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        if (numeric_version(version) <= "22.1") {
            api <- eplus_api(version)
            on.exit(api$close(), add = TRUE)
            state <- api$state_manager$new_state()
            expect_error(
                api$runtime$callback_register_external_hvac_manager(
                    state,
                    function(s) NULL
                ),
                "unavailable.*mandatory HVAC"
            )
            return(invisible(NULL))
        }
        for (mode in c("stop", "error", "interrupt")) {
            marker <- file.path(fixture$directory, paste0(mode, ".txt"))
            setup <- function(api, state) {
                api$runtime$callback_register_external_hvac_manager(
                    state,
                    function(s) {
                        cat("called\n", file = marker, append = TRUE)
                        gc()
                        if (mode == "error") {
                            stop("external HVAC callback failed")
                        }
                        if (mode == "interrupt") {
                            stop(structure(
                                list(message = "HVAC interrupt", call = NULL),
                                class = c("interrupt", "condition")
                            ))
                        }
                        api$runtime$stop_simulation(s)
                    }
                )
            }
            result <- energyplus(
                fixture$model,
                fixture$weather,
                eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
                output_dir = file.path(fixture$directory, mode),
                backend = setup,
                readvars = FALSE,
                echo = FALSE
            )
            expect_true(file.exists(marker))
            expect_identical(
                result$api$stop_reason,
                switch(
                    mode,
                    stop = "user_stop",
                    error = "callback_error",
                    interrupt = "interrupt"
                )
            )
        }
    })

    # All six native CSV sections preserve their original field order and trailing newline.
    test_that("CSV discovery uses the matching releasable API data array", {
        data <- data.frame(
            what = c(
                "Actuator",
                "InternalVariable",
                "PluginGlobalVariable",
                "PluginTrendVariable,",
                "OutputMeter",
                "OutputVariable"
            ),
            name = c(
                "Schedule:Constant",
                "Zone Volume",
                "",
                "",
                "",
                "Temperature"
            ),
            key = c(
                "Heating",
                "ZONE ONE",
                "R GLOBAL",
                "R TREND",
                "Electricity:Facility",
                "ZONE ONE"
            ),
            type = c("Schedule Value", "", "", "", "", ""),
            unit = c("C", "m3", "", "", "J", "C")
        )
        expect_identical(
            api__data_csv(data),
            paste0(
                paste(
                    c(
                        "**ACTUATORS**",
                        "Actuator,Schedule:Constant,Schedule Value,Heating,C",
                        "**INTERNAL_VARIABLES**",
                        "InternalVariable,Zone Volume,ZONE ONE,m3",
                        "**PLUGIN_GLOBAL_VARIABLES**",
                        "PluginGlobalVariable,R GLOBAL",
                        "**TRENDS**",
                        "PluginTrendVariable,R TREND",
                        "**METERS**",
                        "OutputMeter,Electricity:Facility,J",
                        "**VARIABLES**",
                        "OutputVariable,Temperature,ZONE ONE,C"
                    ),
                    collapse = "\n"
                ),
                "\n"
            )
        )
        expect_identical(
            api__data_csv(data[FALSE, ]),
            paste0(
                paste(
                    paste0(
                        "**",
                        c(
                            "ACTUATORS",
                            "INTERNAL_VARIABLES",
                            "PLUGIN_GLOBAL_VARIABLES",
                            "TRENDS",
                            "METERS",
                            "VARIABLES"
                        ),
                        "**"
                    ),
                    collapse = "\n"
                ),
                "\n"
            )
        )
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        entry <- attr(state, "entry")
        on.exit(
            {
                entry$running <- FALSE
                api$close()
            },
            add = TRUE
        )
        expect_error(
            api$exchange$list_available_api_data_csv(state),
            "initialized"
        )
        entry$running <- TRUE
        entry$pointer$ready <- TRUE
        expect_match(
            api$exchange$list_available_api_data_csv(state),
            "OutputVariable,Temperature,ZONE ONE,C",
            fixed = TRUE
        )
        symbols <- vapply(ctx$log, function(x) x$symbol, character(1L))
        expect_true("freeAPIData" %in% symbols)
        expect_false("listAllAPIDataCSV" %in% symbols)
    })

    # Logical clearing drops user closures without retiring native proxies or host calculations.
    test_that("callback clearing is group- and API-scoped and retains native proxies", {
        ctx <- testapi__fake()
        other <- testapi__fake()
        on.exit(other$api$close(), add = TRUE)
        on.exit(ctx$api$close(), add = TRUE)
        states <- lapply(1:2, function(i) ctx$api$state_manager$new_state())
        calls <- new.env(parent = emptyenv())
        calls$user <- calls$functional <- calls$host <- calls$hvac <- calls$component <- 0L
        for (state in states) {
            ctx$api$runtime$callback_progress(state, function(x) {
                calls$user <- calls$user + 1L
            })
            ctx$api$functional$callback_error(state, function(...) {
                calls$functional <- calls$functional + 1L
            })
        }
        state <- states[[1L]]
        entry <- attr(state, "entry")
        api__host_callback(
            ctx$api,
            state,
            "callback_begin_new_environment",
            function(s) calls$host <- calls$host + 1L
        )
        ctx$api$runtime$callback_register_external_hvac_manager(
            state,
            function(s) {
                calls$hvac <- calls$hvac + 1L
            }
        )
        ctx$api$runtime$callback_user_defined_component_model(
            state,
            function(s) calls$component <- calls$component + 1L,
            "component"
        )
        other_state <- other$api$state_manager$new_state()
        other$api$runtime$callback_progress(other_state, function(x) {
            calls$user <- calls$user + 1L
        })
        retained <- entry$callbacks
        ctx$api$runtime$clear_callbacks()
        ctx$api$runtime$clear_callbacks()
        for (callback in entry$callbacks) {
            callback(entry$pointer)
        }
        second <- attr(states[[2L]], "entry")
        second$pointer$callbacks$registerProgressCallback(1L)
        other_entry <- attr(other_state, "entry")
        other_entry$pointer$callbacks$registerProgressCallback(1L)
        expect_identical(entry$callbacks, retained)
        expect_equal(calls$user, 1L)
        expect_equal(calls$functional, 1L)
        expect_equal(calls$host, 1L)
        expect_equal(calls$hvac, 1L)
        expect_equal(calls$component, 1L)
        ctx$api$functional$clear_callbacks()
        entry$pointer$callbacks$registerErrorCallback(1L, "ignored")
        second$pointer$callbacks$registerErrorCallback(1L, "ignored")
        expect_equal(calls$functional, 1L)
        expect_true(all(vapply(
            second$callback_records,
            function(x) is.null(x$fun),
            logical(1L)
        )))
        ctx$api$state_manager$reset_state(state)
        expect_length(entry$callbacks, 0L)
        expect_length(entry$callback_records, 0L)
        ctx$api$close()
        expect_error(ctx$api$runtime$clear_callbacks(), "closed")
    })

    # Clearing from inside delivery must not unwind C or retain the user's captured data.
    test_that("a callback can clear itself and release its captured environment", {
        ctx <- testapi__fake()
        on.exit(ctx$api$close(), add = TRUE)
        state <- ctx$api$state_manager$new_state()
        counter <- new.env(parent = emptyenv())
        counter$calls <- counter$finalized <- 0L
        ctx$api$runtime$callback_begin_new_environment(
            state,
            local({
                token <- new.env(parent = emptyenv())
                reg.finalizer(token, function(x) {
                    counter$finalized <- counter$finalized + 1L
                })
                function(s) {
                    invisible(token)
                    counter$calls <- counter$calls + 1L
                    ctx$api$runtime$clear_callbacks()
                }
            })
        )
        expect_equal(
            ctx$api$runtime$run_energyplus(state, testapi__model()),
            0L
        )
        gc()
        expect_equal(counter$calls, 1L)
        expect_equal(counter$finalized, 1L)
        ctx$api$state_manager$reset_state(state)
        ctx$api$runtime$callback_progress(state, function(x) {
            stop("fresh registration")
        })
        expect_error(
            ctx$api$runtime$run_energyplus(state, testapi__model()),
            "fresh registration"
        )
    })

    # Metadata parsing never mistakes option values for weather flags or resolves paths after cwd changes.
    test_that("captured run paths handle defaults, options and unsupported preprocessing", {
        model <- testapi__model()
        capture <- function(args) {
            api__capture_input(args, model, "Version,26.1;")
        }
        default <- capture(model)
        expect_identical(default$input, api__absolute_path(model))
        expect_identical(default$weather, api__absolute_path("in.epw"))
        for (args in list(
            c("-w", "weather one.epw", model),
            c("--weather", "weather one.epw", model),
            c("--weather=weather one.epw", model),
            c("-wweather one.epw", model)
        )) {
            metadata <- capture(args)
            expect_null(metadata$reason)
            expect_identical(
                metadata$weather,
                api__absolute_path("weather one.epw")
            )
        }
        options <- c(
            "-d",
            "--output-directory",
            "-p",
            "--output-prefix",
            "-s",
            "--output-suffix",
            "-j",
            "--jobs"
        )
        for (option in options) {
            expect_null(capture(c(option, "ignored", model))$reason)
        }
        expect_null(
            capture(c(
                "--annual",
                "--design-day",
                "--readvars",
                "--convert",
                "--convert-only",
                "-a",
                "-D",
                "-r",
                "-c",
                "--jobs=1",
                "--output-directory=out",
                "-dout",
                "-pname",
                "-sL",
                "-j1",
                model
            ))$reason
        )
        expect_identical(capture(c("--", model))$weather, default$weather)
        for (option in c(
            "-m",
            "--epmacro",
            "-x",
            "--expandobjects",
            "-i",
            "--idd",
            "--unknown"
        )) {
            expect_match(capture(c(option, model))$reason, "prepared input")
        }
        expect_match(capture("-w")$reason, "Missing weather")
        expect_match(capture("--weather=")$reason, "Missing weather")
        expect_match(capture(c("-w", ""))$reason, "Missing weather")
        expect_equal(
            capture(c("-wfirst.epw", "-wlast.epw", model))$weather,
            api__absolute_path("last.epw")
        )
    })

    # epJSON keys are frozen at entry, preserve canonical case, and never call allocating getters.
    test_that("metadata queries use immutable prepared epJSON and state lifecycle gates", {
        ctx <- testapi__fake()
        api <- ctx$api
        on.exit(api$close(), add = TRUE)
        state <- api$state_manager$new_state()
        entry <- attr(state, "entry")
        input <- tempfile(fileext = ".epJSON")
        text <- '{"Zone":{"Zulu":{},"alpha":{},"é":{}},"Version":{"Version 1":{"version_identifier":"26.1"}}}'
        writeLines(text, input, useBytes = TRUE)
        expect_error(api$exchange$get_input_file_path(state), "initialized")
        api$runtime$callback_begin_new_environment(state, function(s) {
            writeLines("changed", input)
            expect_identical(
                api$exchange$get_input_file_path(s),
                api__absolute_path(input)
            )
            expect_identical(
                api$exchange$get_weather_file_path(s),
                api__absolute_path("weather.epw")
            )
            expect_identical(
                api$exchange$get_object_names(s, "Zone"),
                c("Zulu", "alpha", "é")
            )
            expect_identical(
                api$exchange$get_object_names(s, "Version"),
                "Version 1"
            )
            expect_identical(
                api$exchange$get_object_names(s, "zone"),
                character()
            )
            for (value in list(
                NULL,
                NA_character_,
                character(),
                "",
                1,
                c("Zone", "Building")
            )) {
                expect_error(
                    api$exchange$get_object_names(s, value),
                    "object_type_name"
                )
            }
        })
        api$runtime$run_energyplus(state, c("-w", "weather.epw", input))
        expect_null(entry$input_metadata$source)
        expect_error(
            api$exchange$get_object_names(state, "Zone"),
            "initialized"
        )
        expect_false(any(vapply(
            ctx$log,
            function(x) {
                x$symbol %in%
                    c(
                        "inputFilePath",
                        "epwFilePath",
                        "getObjectNames",
                        "freeObjectNames"
                    )
            },
            logical(1L)
        )))
        api$state_manager$reset_state(state)
        expect_null(entry$input_metadata)
        entry$running <- TRUE
        entry$pointer$ready <- TRUE
        expect_error(
            api$exchange$get_input_file_path(state),
            "no R-managed run context"
        )
        entry$input_metadata <- api__capture_input(c("-x", input), input, text)
        expect_error(api$exchange$get_input_file_path(state), "prepared input")
        entry$input_metadata <- api__capture_input(input, input, "[]")
        expect_error(
            api$exchange$get_object_names(state, "Zone"),
            "invalid epJSON"
        )
        entry$running <- FALSE
    })

    # Conversion is lazy, cached after success, and retried rather than caching a failed snapshot.
    test_that("IDF metadata conversion is isolated, cached and fails clearly", {
        ctx <- testapi__fake()
        on.exit(ctx$api$close(), add = TRUE)
        state <- ctx$api$state_manager$new_state()
        entry <- attr(state, "entry")
        entry$running <- TRUE
        entry$pointer$ready <- TRUE
        entry$input_metadata <- api__capture_input(
            "input.idf",
            "input.idf",
            "Version,26.1;"
        )
        calls <- 0L
        local_mocked_bindings(api__convert_input = function(directory, source) {
            calls <<- calls + 1L
            expect_identical(source, "Version,26.1;")
            list(Version = list("Version 1" = list()))
        })
        for (i in 1:2) {
            expect_identical(
                ctx$api$exchange$get_object_names(state, "Version"),
                "Version 1"
            )
        }
        expect_identical(calls, 1L)
        entry$running <- FALSE
    })

    # Stand-in converter processes exercise output verification and unconditional temporary cleanup.
    test_that("the stock converter checks exit status and removes its private files", {
        mode <- "ok"
        temporary <- NULL
        local_mocked_bindings(
            run = function(command, args, ...) {
                expect_match(command, "energyplus")
                expect_identical(args[[1L]], "--convert-only")
                temporary <<- dirname(tail(args, 1L))
                expect_identical(readLines(tail(args, 1L)), "Version,26.1;")
                if (mode == "ok") {
                    writeLines(
                        '{"Version":{"Version 1":{}}}',
                        file.path(temporary, "input.epJSON")
                    )
                }
                list(status = if (mode == "failure") 1L else 0L)
            },
            .package = "processx"
        )
        expect_named(api__convert_input(tempdir(), "Version,26.1;"), "Version")
        expect_false(dir.exists(temporary))
        for (value in c("missing", "failure")) {
            mode <- value
            expect_error(
                api__convert_input(tempdir(), "Version,26.1;"),
                "conversion failed"
            )
            expect_false(dir.exists(temporary))
        }
    })

    # Exercise both clearing groups, lazy conversion and GC with the unmodified native engine.
    test_that("stock managed clearing and metadata queries preserve the native lifecycle", {
        fixture <- testrun__api_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        marker <- file.path(fixture$directory, "managed-metadata.rds")
        setup <- function(api, state) {
            counts <- new.env(parent = emptyenv())
            counts$functional <- counts$user <- counts$host <- counts$finalized <- 0L
            counts$metadata <- NULL
            legacy94 <- grepl("Version 9.4.0", api$info()$version, fixed = TRUE)
            functional_state <- if (legacy94) {
                state
            } else {
                api$state_manager$new_state()
            }
            api$functional$callback_error(functional_state, function(...) {
                counts$functional <- counts$functional + 1L
            })
            fluid <- api$functional$glycol(functional_state, "water")
            fluid$specific_heat(functional_state, -1000)
            counts$before <- counts$functional
            api$functional$clear_callbacks()
            fluid$specific_heat(functional_state, -1000)
            counts$after <- counts$functional
            if (!legacy94) {
                api$state_manager$delete_state(functional_state)
            }
            api$runtime$callback_begin_zone_timestep_before_init_heat_balance(
                state,
                local({
                    token <- new.env(parent = emptyenv())
                    reg.finalizer(token, function(x) {
                        counts$finalized <- counts$finalized + 1L
                    })
                    function(s) {
                        invisible(token)
                        counts$user <- counts$user + 1L
                        if (counts$user == 2L) api$runtime$clear_callbacks()
                    }
                })
            )
            eplusr:::api__host_callback(
                api,
                state,
                "callback_end_zone_timestep_after_zone_reporting",
                function(s) {
                    counts$host <- counts$host + 1L
                    if (
                        api$exchange$api_data_fully_ready(s) &&
                            is.null(counts$metadata)
                    ) {
                        counts$metadata <- list(
                            input = api$exchange$get_input_file_path(s),
                            weather = api$exchange$get_weather_file_path(s),
                            zones = api$exchange$get_object_names(s, "Zone"),
                            reports = api$exchange$get_object_names(
                                s,
                                "Output:Variable"
                            ),
                            missing = api$exchange$get_object_names(
                                s,
                                "NoSuchType"
                            ),
                            input_exists = file.exists(api$exchange$get_input_file_path(
                                s
                            )),
                            weather_exists = file.exists(api$exchange$get_weather_file_path(
                                s
                            ))
                        )
                    }
                    if (counts$host == 20L) {
                        gc()
                        saveRDS(as.list(counts), marker)
                        api$runtime$stop_simulation(s)
                    }
                }
            )
        }
        result <- energyplus(
            fixture$model,
            fixture$weather,
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
            output_dir = file.path(fixture$directory, "managed"),
            backend = setup,
            readvars = FALSE,
            echo = FALSE
        )
        expect_identical(result$api$stop_reason, "user_stop")
        expect_true(file.exists(marker))
        if (!file.exists(marker)) {
            return(invisible(NULL))
        }
        data <- readRDS(marker)
        expect_gt(data$before, 0L)
        expect_identical(data$after, data$before)
        expect_equal(data$user, 2L)
        expect_equal(data$host, 20L)
        expect_equal(data$finalized, 1L)
        expect_identical(data$metadata$zones, "ZONE ONE")
        expect_gt(length(data$metadata$reports), 10L)
        expect_identical(
            data$metadata$reports,
            paste0("Output:Variable ", seq_along(data$metadata$reports))
        )
        expect_identical(data$metadata$missing, character())
        expect_true(data$metadata$input_exists)
        expect_true(data$metadata$weather_exists)
    })

    # Logical clearing never erases an error or interrupt raised by the current delivery.
    test_that("clearing a callback preserves its subsequent failure and native cleanup", {
        for (condition in list(
            simpleError("after clearing"),
            structure(
                list(message = "after clearing interrupt", call = NULL),
                class = c("interrupt", "condition")
            )
        )) {
            ctx <- testapi__fake()
            api <- ctx$api
            state <- api$state_manager$new_state()
            api$runtime$callback_begin_new_environment(state, function(s) {
                api$runtime$clear_callbacks()
                stop(condition)
            })
            actual <- tryCatch(
                api$runtime$run_energyplus(state, testapi__model()),
                error = identity,
                interrupt = identity
            )
            expect_identical(actual, condition)
            expect_identical(api$state_manager$status(state)$phase, "closed")
            expect_equal(ctx$deleted, 1L)
            api$close()
        }
    })

    # Both reviewed releases share declarations but retain independent installation identity.
    test_that("version profiles select shared ABI and preserve the trend safety boundary", {
        for (version in c("26.1.0", "26.2.0")) {
            manifest <- api__manifest(version)
            expect_identical(manifest$energyplus, version)
            expect_identical(manifest$abi, "26.1-26.2")
            trend <- Filter(
                function(x) x$method == "get_trend_value",
                manifest$methods
            )
            expect_identical(trend[[1L]]$status, "unsupported")
        }
        expect_error(api_profile__select(character()), "installation")
        expect_error(api_profile__select("9.3.0"), "installation")
        expect_error(api_bind__select("unknown"))
    })

    # Verify 26.2 fixes through node humidity and meter behavior, not actuator readback alone.
    test_that("stock 26.2 exposes the first meter and recalculates overridden outdoor node humidity", {
        skip_if_not(identical(
            Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
            "26.2"
        ))
        fixture <- testrun__api_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        cat(
            "\nOutdoorAir:Node,API Outdoor Node;\n",
            file = fixture$model,
            append = TRUE
        )
        marker <- file.path(fixture$directory, "weather-and-meter.rds")
        setup <- function(api, state) {
            exchange <- api$exchange
            variables <- c(
                temperature = "System Node Temperature",
                humidity = "System Node Humidity Ratio",
                pressure = "System Node Pressure"
            )
            for (name in variables) {
                exchange$request_variable(state, name, "API OUTDOOR NODE")
            }
            handles <- NULL
            api$runtime$callback_begin_zone_timestep_before_init_heat_balance(
                state,
                function(s) {
                    if (
                        !exchange$api_data_fully_ready(s) ||
                            exchange$warmup_flag(s) ||
                            exchange$kind_of_sim(s) != 3L
                    ) {
                        return(NULL)
                    }
                    if (is.null(handles)) {
                        data <- exchange$get_api_data(s)
                        meters <- data$key[data$what == "OutputMeter"]
                        actuators <- subset(
                            data,
                            what == "Actuator" & key == "API OUTDOOR NODE"
                        )
                        # Discover the stock node actuator names from the engine's copied metadata.
                        actuator <- function(pattern) {
                            row <- actuators[grepl(pattern, actuators$type), ]
                            stopifnot(nrow(row) == 1L)
                            exchange$get_actuator_handle(
                                s,
                                row$name,
                                row$type,
                                row$key
                            )
                        }
                        handles <<- list(
                            meter = exchange$get_meter_handle(s, meters[[1L]]),
                            dry = actuator("Dry"),
                            wet = actuator("Wet"),
                            variables = lapply(variables, function(name) {
                                exchange$get_variable_handle(
                                    s,
                                    name,
                                    "API OUTDOOR NODE"
                                )
                            })
                        )
                    }
                    exchange$set_actuator_value(s, handles$dry, 26.7)
                    exchange$set_actuator_value(s, handles$wet, 19.4)
                }
            )
            api$runtime$callback_end_zone_timestep_after_zone_reporting(
                state,
                function(s) {
                    if (
                        is.null(handles) ||
                            exchange$warmup_flag(s) ||
                            exchange$kind_of_sim(s) != 3L
                    ) {
                        return(NULL)
                    }
                    values <- vapply(
                        handles$variables,
                        function(handle) exchange$get_variable_value(s, handle),
                        numeric(1L)
                    )
                    psych <- api$functional$psychrometrics(s)
                    saveRDS(
                        list(
                            meter = as.integer(handles$meter),
                            values = values,
                            expected_humidity = psych$humidity_ratio_d(
                                s,
                                26.7,
                                19.4,
                                values[["pressure"]]
                            )
                        ),
                        marker
                    )
                    api$runtime$stop_simulation(s)
                }
            )
        }
        result <- energyplus(
            fixture$model,
            fixture$weather,
            eplus = "26.2",
            backend = setup,
            output_dir = file.path(fixture$directory, "weather"),
            readvars = FALSE,
            echo = FALSE
        )
        expect_identical(
            result$api$stop_reason,
            "user_stop",
            info = result$api$error$message
        )
        expect_true(file.exists(marker))
        measured <- readRDS(marker)
        expect_identical(measured$meter, 0L)
        expect_equal(measured$values[["temperature"]], 26.7)
        expect_equal(
            measured$values[["humidity"]],
            measured$expected_humidity,
            tolerance = 1e-8
        )
    })

    # Exercise the common public contract on each explicitly selected stock installation.
    test_that("stock API exposes four groups and reviewed operation provenance", {
        skip_if(
            identical(Sys.getenv("_EPLUSR_TEST_API_VERSION_"), "9.4"),
            "9.4 requires a package-owned backend/session worker; dedicated worker tests cover its contract"
        )
        skip_if_not(identical(Sys.getenv("_EPLUSR_RUN_API_TESTS_"), "true"))
        version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        # A fresh process owns its bridge safety proof and standalone native resources.
        result <- callr::r(
            function(version) {
                original_path <- Sys.getenv("PATH")
                api <- eplusr::eplus_api(version)
                restored_path <- identical(Sys.getenv("PATH"), original_path)
                on.exit(api$close(), add = TRUE)
                state <- api$state_manager$new_state()
                psych <- api$functional$psychrometrics(state)
                heat_capacity <- psych$specific_heat(state, 0)
                # Exercise both native resource owners and their public deletion boundary.
                water <- api$functional$glycol(state, "WATER")
                steam <- api$functional$refrigerant(state, "STEAM")
                water_density <- water$density(state, 20)
                steam_pressure <- steam$saturation_pressure(state, 100)
                water$delete(state)
                steam$delete(state)
                retired <- tryCatch(
                    water$density(state, 20),
                    error = conditionMessage
                )
                console <- tryCatch(
                    api$runtime$set_console_output_status(state, FALSE),
                    error = conditionMessage
                )
                api$exchange$request_variable(
                    state,
                    "Zone Mean Air Temperature",
                    "ZONE ONE"
                )
                limitation <- tryCatch(
                    api$exchange$get_trend_value(state, 1L, 1L),
                    error = conditionMessage
                )
                api$state_manager$reset_state(state)
                reset <- api$state_manager$status(state)
                api$state_manager$delete_state(state)
                list(
                    restored_path = restored_path,
                    console = console,
                    info = api$info(),
                    heat_capacity = heat_capacity,
                    water_density = water_density,
                    steam_pressure = steam_pressure,
                    retired = retired,
                    methods = api$methods(),
                    limitation = limitation,
                    reset = reset,
                    deleted = api$state_manager$status(state)
                )
            },
            args = list(version),
            libpath = .libPaths()
        )
        expect_true(result$restored_path)
        if (version == "9.5") {
            expect_match(result$console, "cannot suppress")
        } else {
            expect_null(result$console)
        }
        expect_match(result$info$version, paste0(version, ".0"), fixed = TRUE)
        # The stock psychrometric equation clamps humidity ratio to 1e-5 kg/kg.
        expect_equal(result$heat_capacity, 1004.8585895, tolerance = 1e-9)
        expect_equal(result$water_density, 998.2, tolerance = 0.001)
        expect_equal(result$steam_pressure, 101325, tolerance = 0.01)
        expect_match(result$retired, "closed")
        methods <- result$methods
        if (version == "24.2") {
            expect_true(all(grepl(
                "native wrapper deletion",
                methods$ownership[methods$method == "delete"]
            )))
        }
        expect_true(all(c("implementation", "provenance") %in% names(methods)))
        expect_identical(
            methods$implementation[methods$method == "get_api_data"],
            if (numeric_version(version) < "23.2") "unsupported" else "native"
        )
        expect_identical(
            methods$implementation[
                methods$method == "list_available_api_data_csv"
            ],
            if (numeric_version(version) < "23.2") "unsupported" else "managed"
        )
        expect_identical(
            methods$implementation[methods$method == "get_trend_value"],
            "unsupported"
        )
        expect_match(result$limitation, paste0(version, ".0"), fixed = TRUE)
        expect_identical(result$reset$generation, 1L)
        expect_identical(result$deleted$phase, "closed")
    })

    # Reject prepared input from another release before a native run can change the state.
    test_that("stock runtime rejects IDF and epJSON model version mismatches", {
        skip_if(
            identical(Sys.getenv("_EPLUSR_TEST_API_VERSION_"), "9.4"),
            "9.4 requires a package-owned backend/session worker; dedicated worker tests cover its contract"
        )
        skip_if_not(identical(Sys.getenv("_EPLUSR_RUN_API_TESTS_"), "true"))
        version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        result <- callr::r(
            function(version) {
                api <- eplusr::eplus_api(version)
                on.exit(api$close(), add = TRUE)
                lapply(c("idf", "epJSON"), function(extension) {
                    directory <- tempfile("eplusr-model-version-")
                    dir.create(directory)
                    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
                    model <- file.path(directory, paste0("input.", extension))
                    writeLines(
                        if (extension == "idf") {
                            "! Prepared for another engine\nVersion,9.4;"
                        } else {
                            '{"Version":{"Version 1":{"version_identifier":"9.4"}}}'
                        },
                        model
                    )
                    state <- api$state_manager$new_state()
                    message <- tryCatch(
                        {
                            api$runtime$run_energyplus(
                                state,
                                c("-d", directory, model)
                            )
                            NA_character_
                        },
                        error = conditionMessage
                    )
                    status <- api$state_manager$status(state)
                    api$state_manager$delete_state(state)
                    list(message = message, phase = status$phase)
                })
            },
            args = list(version),
            libpath = .libPaths()
        )
        for (input in result) {
            expect_match(input$message, "model version must match")
            expect_match(input$message, paste0(version, ".0"), fixed = TRUE)
            expect_identical(input$phase, "created")
        }
    })

    # A native lifecycle cannot safely be selected when the prepared input identity is absent or ambiguous.
    test_that("stock runtime requires one valid prepared model version", {
        skip_if(
            identical(Sys.getenv("_EPLUSR_TEST_API_VERSION_"), "9.4"),
            "9.4 requires a package-owned backend/session worker; dedicated worker tests cover its contract"
        )
        skip_if_not(identical(Sys.getenv("_EPLUSR_RUN_API_TESTS_"), "true"))
        version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        result <- callr::r(
            function(version) {
                api <- eplusr::eplus_api(version)
                on.exit(api$close(), add = TRUE)
                inputs <- list(
                    c("idf", "Building,Unnamed;"),
                    c("idf", "Version,9.4; Version,9.5;"),
                    c("idf", "Version,unknown;"),
                    c("epJSON", "{}"),
                    c("epJSON", "{")
                )
                lapply(inputs, function(input) {
                    model <- tempfile(fileext = paste0(".", input[[1L]]))
                    on.exit(unlink(model), add = TRUE)
                    writeLines(input[[2L]], model)
                    state <- api$state_manager$new_state()
                    message <- tryCatch(
                        {
                            api$runtime$run_energyplus(state, model)
                            NA_character_
                        },
                        error = conditionMessage
                    )
                    status <- api$state_manager$status(state)
                    api$state_manager$delete_state(state)
                    list(message = message, phase = status$phase)
                })
            },
            args = list(version),
            libpath = .libPaths()
        )
        for (input in result) {
            expect_match(input$message, "model version")
            expect_identical(input$phase, "created")
        }
    })

    # Reviewed 22.x libraries share D221 declarations while preserving managed resource ownership.
    test_that("stock 22.x APIs preserve uniform groups and reject stale functional resources", {
        skip_if_not(identical(Sys.getenv("_EPLUSR_RUN_API_TESTS_"), "true"))
        version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        skip_if_not(version %in% c("22.1", "22.2"))
        result <- callr::r(
            function(version) {
                api <- eplusr::eplus_api(version)
                on.exit(api$close(), add = TRUE)
                state <- api$state_manager$new_state()
                glycol <- api$functional$glycol(state, "water")
                heat_capacity <- glycol$specific_heat(state, 20)
                api$runtime$set_console_output_status(state, FALSE)
                api$exchange$request_variable(
                    state,
                    "Zone Mean Air Temperature",
                    "ZONE ONE"
                )
                before <- api$exchange$api_data_fully_ready(state)
                api$state_manager$reset_state(state)
                stale <- tryCatch(
                    glycol$specific_heat(state, 20),
                    error = conditionMessage
                )
                api$state_manager$delete_state(state)
                deleted <- tryCatch(
                    api$exchange$request_variable(state, "Temperature", "Zone"),
                    error = conditionMessage
                )
                list(
                    info = api$info(),
                    methods = api$methods(),
                    heat_capacity = heat_capacity,
                    before = before,
                    stale = stale,
                    deleted = deleted
                )
            },
            args = list(version),
            libpath = .libPaths()
        )
        expect_match(result$info$version, paste0(version, ".0"), fixed = TRUE)
        expect_true(all(
            c("state_manager", "functional", "runtime", "exchange") %in%
                result$methods$group
        ))
        expect_equal(result$heat_capacity, 4180, tolerance = 0.02)
        expect_false(result$before)
        expect_match(result$stale, "reset|stale|generation|deleted")
        expect_match(result$deleted, "closed|deleted")
    })

    # Stock release metadata must remain safe when discovery layouts or public getters differ.
    test_that("stock older API reports safe discovery and readiness capabilities", {
        fixture <- testrun__api_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        marker <- file.path(fixture$directory, "discovery.rds")
        setup <- function(api, state) {
            exchange <- api$exchange
            methods <- api$methods()
            before <- exchange$api_data_fully_ready(state)
            exchange$request_variable(
                state,
                "Zone Mean Air Temperature",
                "ZONE ONE"
            )
            api$runtime$callback_end_zone_timestep_after_zone_reporting(
                state,
                function(s) {
                    if (
                        !exchange$api_data_fully_ready(s) ||
                            exchange$warmup_flag(s) ||
                            exchange$kind_of_sim(s) != 3L
                    ) {
                        return(NULL)
                    }
                    capture <- function(f) {
                        tryCatch(f(), error = conditionMessage)
                    }
                    handle <- exchange$get_variable_handle(
                        s,
                        "Zone Mean Air Temperature",
                        "ZONE ONE"
                    )
                    saveRDS(
                        list(
                            before = before,
                            data = capture(function() exchange$get_api_data(s)),
                            csv = capture(function() {
                                exchange$list_available_api_data_csv(s)
                            }),
                            year = capture(function() {
                                exchange$calendar_year(s)
                            }),
                            value = exchange$get_variable_value(s, handle),
                            methods = methods
                        ),
                        marker
                    )
                    api$runtime$stop_simulation(s)
                }
            )
        }
        result <- energyplus(
            fixture$model,
            fixture$weather,
            eplus = version,
            backend = setup,
            output_dir = file.path(fixture$directory, "discovery"),
            readvars = FALSE,
            echo = FALSE
        )
        expect_identical(
            result$api$stop_reason,
            "user_stop",
            info = result$api$error$message
        )
        expect_true(file.exists(marker))
        measured <- readRDS(marker)
        expect_false(measured$before)
        expect_true(is.finite(measured$value))
        if (numeric_version(version) < "23.2") {
            expect_match(measured$data, "unavailable.*discovery")
            expect_match(measured$csv, "unavailable.*discovery")
            expect_match(measured$year, "unavailable.*calendar")
        } else {
            data <- measured$data
            expect_identical(
                names(data),
                c("what", "name", "key", "type", "unit")
            )
            variables <- subset(
                data,
                what == "OutputVariable" & name == "Zone Mean Air Temperature"
            )
            expect_true("ZONE ONE" %in% toupper(variables$key))
            actuators <- subset(
                data,
                what == "Actuator" & name == "Weather Data"
            )
            expect_true("Outdoor Dry Bulb" %in% actuators$type)
            expect_true("ENVIRONMENT" %in% toupper(actuators$key))
            if (version %in% c("23.2", "24.1")) {
                expect_true(all(is.na(data$unit)))
            }
            expect_match(
                measured$csv,
                "Weather Data,Outdoor Dry Bulb,Environment"
            )
            expect_true(is.finite(measured$year))
        }
    })

    # Check the five-pointer API data contract while the stock callback reads and writes controls.
    test_that("stock callbacks preserve exchange units and typed control ownership", {
        skip_if(
            numeric_version(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")) <
                "24.2",
            "Five-pointer native units are available from 24.2"
        )
        fixture <- testapi__controlled_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        marker <- file.path(fixture$directory, "exchange.rds")
        # Bind requested outputs and control callbacks to this worker-owned state.
        setup <- function(api, state) {
            api$exchange$request_variable(
                state,
                "Zone Mean Air Temperature",
                "ZONE ONE"
            )
            # Inspect live data only after readiness, before predictor reads the thermostat schedule.
            api$runtime$callback_begin_system_timestep_before_predictor(
                state,
                function(s) {
                    if (
                        !api$exchange$api_data_fully_ready(s) ||
                            api$exchange$warmup_flag(s) ||
                            api$exchange$kind_of_sim(s) != 3L
                    ) {
                        return()
                    }
                    temperature <- api$exchange$get_variable_handle(
                        s,
                        "Zone Mean Air Temperature",
                        "ZONE ONE"
                    )
                    schedule <- api$exchange$get_actuator_handle(
                        s,
                        "Schedule:Constant",
                        "Schedule Value",
                        "Session Heating"
                    )
                    api$exchange$set_actuator_value(s, schedule, 22)
                    wrong_kind <- tryCatch(
                        api$exchange$get_variable_value(s, schedule),
                        error = conditionMessage
                    )
                    saveRDS(
                        list(
                            data = tryCatch(
                                api$exchange$get_api_data(s),
                                error = conditionMessage
                            ),
                            temperature = api$exchange$get_variable_value(
                                s,
                                temperature
                            ),
                            actuator = api$exchange$get_actuator_value(
                                s,
                                schedule
                            ),
                            wrong_kind = wrong_kind
                        ),
                        marker
                    )
                    api$exchange$reset_actuator(s, schedule)
                    api$runtime$stop_simulation(s)
                }
            )
        }
        result <- energyplus(
            fixture$model,
            fixture$weather,
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
            output_dir = file.path(fixture$directory, "exchange"),
            backend = setup,
            readvars = FALSE,
            echo = FALSE
        )
        expect_identical(result$api$stop_reason, "user_stop")
        expect_true(file.exists(marker))
        if (!file.exists(marker)) {
            return(invisible(NULL))
        }
        actual <- readRDS(marker)
        data <- actual$data
        version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        if (numeric_version(version) < "23.2") {
            expect_match(data, "unavailable.*discovery")
        } else {
            expect_named(data, c("what", "name", "key", "type", "unit"))
            row <- data[
                data$what == "OutputVariable" &
                    data$name == "Zone Mean Air Temperature" &
                    data$key == "ZONE ONE",
            ]
            expect_equal(nrow(row), 1L)
            if (numeric_version(version) < "24.2") {
                expect_true(is.na(row$unit))
            } else {
                expect_identical(row$unit, "C")
            }
        }
        expect_true(is.finite(actual$temperature))
        expect_equal(actual$actuator, 22)
        expect_match(actual$wrong_kind, "data kind")
    })

    # The 9.4 C API retains process-global simulation and error callbacks after state deletion.
    test_that("stock 9.4 state lifecycles require a package-owned fresh worker", {
        skip_if_not(
            dir.exists(file.path(
                path.expand("~/Applications"),
                "EnergyPlus-9-4-0"
            )) ||
                identical(Sys.getenv("_EPLUSR_TEST_API_VERSION_"), "9.4")
        )
        expect_error(
            eplus_api("9.4"),
            "9[.]4.*fresh worker|fresh worker.*9[.]4"
        )
    })

    # The early state-based releases must identify the selected library without newer exports.
    test_that("stock early API identifies its selected installation and console limitation", {
        skip_if_not(identical(Sys.getenv("_EPLUSR_RUN_API_TESTS_"), "true"))
        version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        skip_if_not(version %in% c("9.5", "9.6"))
        api <- eplus_api(version)
        on.exit(api$close(), add = TRUE)
        expect_match(api$info()$version, paste0(version, ".0"), fixed = TRUE)
        expect_identical(api$functional$ep_version(), api$info()$version)
        state <- api$state_manager$new_state()
        psych <- api$functional$psychrometrics(state)
        expect_equal(
            psych$specific_heat(state, 0),
            1004.8585895,
            tolerance = 1e-9
        )
        expect_false(api$exchange$api_data_fully_ready(state))
        expect_error(api$exchange$get_api_data(state), "unavailable")
        expect_error(api$exchange$calendar_year(state), "unavailable")
        expect_silent(api$runtime$set_console_output_status(state, TRUE))
        if (version == "9.5") {
            expect_match(api$info()$identity_source, "SDK.*symbols.*CLI")
            expect_error(
                api$runtime$set_console_output_status(state, FALSE),
                "cannot suppress"
            )
            expect_error(api$exchange$current_sim_time(state), "lifecycle")
        } else {
            expect_silent(api$runtime$set_console_output_status(state, FALSE))
        }
        api$state_manager$reset_state(state)
        expect_identical(api$state_manager$status(state)$generation, 1L)
        api$state_manager$delete_state(state)
    })

    # Match modern rounded minute semantics, including the completed hour's minute 60.
    test_that("stock early API rounds fractional minute boundaries consistently", {
        version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        skip_if_not(version %in% c("9.5", "9.6"))
        fixture <- testrun__api_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        model <- read_idf(fixture$model)
        model$set(Timestep := list(number_of_timesteps_per_hour = 6))
        model$save(fixture$model, overwrite = TRUE)
        api <- eplus_api(version)
        on.exit(try(api$close(), silent = TRUE), add = TRUE)
        state <- api$state_manager$new_state()
        minutes <- integer()
        api$runtime$callback_end_zone_timestep_after_zone_reporting(
            state,
            function(s) {
                if (
                    api$exchange$kind_of_sim(s) != 3L ||
                        api$exchange$warmup_flag(s)
                ) {
                    return(NULL)
                }
                minutes <<- c(minutes, api$exchange$minutes(s))
                if (length(minutes) == 6L) api$runtime$stop_simulation(s)
            }
        )
        api$runtime$run_energyplus(
            state,
            c("-w", fixture$weather, "-d", fixture$directory, fixture$model)
        )
        expect_identical(minutes, c(10L, 20L, 30L, 40L, 50L, 60L))
    })

    # A failed early hook must return its original condition even when no weather boundary exists.
    test_that("stock early design-only callback failures finish safely with original conditions", {
        version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        skip_if_not(version %in% c("9.5", "9.6", "22.1"))
        fixture <- testrun__api_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        for (mode in c("error", "interrupt")) {
            result <- callr::r(
                function(version, model, directory, mode) {
                    api <- eplusr::eplus_api(version)
                    on.exit(api$close(), add = TRUE)
                    state <- api$state_manager$new_state()
                    api$runtime$callback_begin_system_timestep_before_predictor(
                        state,
                        function(s) {
                            if (api$exchange$warmup_flag(s)) {
                                return(NULL)
                            }
                            stop(structure(
                                list(
                                    message = "original early failure",
                                    call = NULL,
                                    marker = 42L
                                ),
                                class = c(mode, "condition")
                            ))
                        }
                    )
                    condition <- tryCatch(
                        api$runtime$run_energyplus(
                            state,
                            c("--design-day", "-d", directory, model)
                        ),
                        error = identity,
                        interrupt = identity
                    )
                    list(
                        class = class(condition),
                        marker = condition$marker,
                        message = conditionMessage(condition),
                        status = api$state_manager$status(state)
                    )
                },
                args = list(
                    version,
                    fixture$model,
                    file.path(fixture$directory, mode),
                    mode
                ),
                libpath = .libPaths(),
                timeout = 120
            )
            expect_true(mode %in% result$class)
            expect_identical(result$marker, 42L)
            expect_identical(result$message, "original early failure")
            expect_identical(result$status$phase, "closed")
            expect_identical(
                result$status$stop_reason,
                if (mode == "interrupt") "interrupt" else "callback_error"
            )
        }
    })

    # Legacy identity checks inspect the selected installation without accepting unverified SDK metadata.
    test_that("stock early API rejects missing or conflicting SDK identity", {
        skip_if_not(identical(Sys.getenv("_EPLUSR_RUN_API_TESTS_"), "true"))
        version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        skip_if_not(identical(version, "9.5"))
        root <- eplus_config(version)$dir
        on.exit(use_eplus(root), add = TRUE)
        directory <- tempfile("eplusr-early-identity-")
        dir.create(directory)
        on.exit(unlink(directory, recursive = TRUE), add = TRUE)
        api <- eplus_api(version)
        source_library <- api$info()$library
        library <- if (.Platform$OS.type == "windows") {
            "EnergyPlusAPI.dll"
        } else if (Sys.info()[["sysname"]] == "Darwin") {
            "libenergyplusapi.dylib"
        } else {
            "libenergyplusapi.so"
        }

        api$close()
        executable <- if (.Platform$OS.type == "windows") {
            "energyplus.exe"
        } else {
            "energyplus"
        }
        for (name in c(library, executable, "Energy+.idd")) {
            source_path <- if (name == library) {
                source_library
            } else {
                file.path(root, name)
            }
            if (
                !suppressWarnings(file.symlink(
                    source_path,
                    file.path(directory, name)
                ))
            ) {
                expect_true(file.copy(
                    file.path(root, name),
                    file.path(directory, name)
                ))
            }
        }
        expect_error(eplus_api(directory), "matching installed SDK")
        dir.create(file.path(directory, "pyenergyplus"))
        sdk <- file.path(directory, "pyenergyplus", "func.py")
        writeLines("invalid SDK", sdk)
        expect_error(eplus_api(directory), "Cannot identify.*SDK")
        source <- readLines(
            file.path(root, "pyenergyplus", "func.py"),
            warn = FALSE
        )
        writeLines(
            sub(
                'ep_version_minor = int("5")',
                'ep_version_minor = int("6")',
                source,
                fixed = TRUE
            ),
            sdk
        )
        expect_error(eplus_api(directory), "symbols do not match.*SDK")
        writeLines(
            sub(
                'ep_version_build = str("',
                'ep_version_build = str("wrong',
                source,
                fixed = TRUE
            ),
            sdk
        )
        expect_error(eplus_api(directory), "CLI does not match.*SDK")
        writeLines(source, sdk)
        selected <- eplus_api(directory)
        on.exit(selected$close(), add = TRUE)
        expect_match(selected$info()$identity_source, "SDK.*symbols.*CLI")
    })

    # Exercise the engine-visible address after retirement, in a child that contains a possible crash.
    test_that("retired native callback addresses remain inert after garbage collection", {
        skip_if_not_installed("rdyncall")
        directory <- tempfile("eplusr-retired-fixture-")
        dir.create(directory)
        on.exit(unlink(directory, recursive = TRUE), add = TRUE)
        file.copy(test_path("fixtures", "callback-thread.c"), directory)
        compile <- callr::rcmd(
            "SHLIB",
            "callback-thread.c",
            wd = directory,
            fail_on_status = FALSE,
            echo = FALSE
        )
        expect_identical(compile$status, 0L, info = compile$stderr)
        result <- callr::r(
            function(directory) {
                library(eplusr)
                fixture <- rdyncall::dynload(
                    file.path(
                        directory,
                        paste0("callback-thread", .Platform$dynlib.ext)
                    ),
                    auto.unload = FALSE
                )
                owner <- rdyncall::dynsym(fixture, "callback_on_owner")
                registered <- dyn.load(file.path(
                    directory,
                    paste0("callback-thread", .Platform$dynlib.ext)
                ))
                retain <- getNativeSymbolInfo(
                    "callback_retain",
                    registered
                )$address
                signatures <- c("p)v", "i)v", "Z)v", "iZ)v")
                lapply(seq_along(signatures), function(kind) {
                    count <- 0L
                    original <- rdyncall::ccallback(
                        signatures[[kind]],
                        function(...) {
                            count <<- count + 1L
                            invisible(NULL)
                        }
                    )
                    guard <- .Call(
                        "eplusr_guard_create",
                        original,
                        signatures[[kind]],
                        PACKAGE = "eplusr"
                    )
                    retained <- .Call(retain, guard)
                    rdyncall::dyncall(owner, "pi)v", guard, kind - 1L)
                    before <- count
                    .Call("eplusr_guard_release", guard, PACKAGE = "eplusr")
                    rm(guard)
                    gc()
                    # No replacement guard or foreign-thread poisoning may mask retirement.
                    rdyncall::dyncall(owner, "pi)v", retained, kind - 1L)
                    c(before = before, after = count)
                })
            },
            args = list(directory),
            libpath = .libPaths(),
            timeout = 30
        )
        expect_identical(result, rep(list(c(before = 1L, after = 1L)), 4L))
    })
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
        for (bad in list(
            c(missing = 20),
            c(heating = Inf),
            c(heating = NaN),
            20
        )) {
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
        message <- list(
            id = 1L,
            action = c(heating = 21),
            state = list(count = 1L)
        )
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
        variables <- list(
            temperature = c("Zone Mean Air Temperature", "ZONE ONE")
        )
        session <- eplus_session(
            fixture$model,
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
            fixture$weather,
            variables = variables,
            dir = file.path(fixture$directory, "session")
        )
        on.exit(try(session$close(), silent = TRUE), add = TRUE)
        first <- session$initialize()
        identity <- session$status()$identity
        expect_identical(first$step, 1L)
        expect_identical(first$kind_of_sim, 3L)
        expect_identical(
            first$callback,
            "end_zone_timestep_after_zone_reporting"
        )
        expect_equal(first$time_end - first$time_start, 0.25, tolerance = 1e-10)
        expect_true(is.finite(first$values[["temperature"]]))
        Sys.sleep(0.2)
        gc()
        expect_identical(session$observe(), first)
        next_step <- session$step(n = 3L)
        expect_identical(next_step$step, 4L)
        expect_equal(
            next_step$time_end - first$time_end,
            0.75,
            tolerance = 1e-10
        )
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
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
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
                                    type = if (index == 2L) {
                                        "invalid"
                                    } else {
                                        "hello"
                                    },
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
                eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
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
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
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
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
            fixture$weather,
            variables = list(
                heating = c(
                    "Zone Thermostat Heating Setpoint Temperature",
                    "ZONE ONE"
                ),
                temperature = c("Zone Mean Air Temperature", "ZONE ONE")
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
        # The ideal-loads zone must physically respond to the higher thermostat action.
        expect_gt(
            first$values[["temperature"]],
            fourth$values[["temperature"]] + 1
        )
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
        local_mocked_bindings(
            file.rename = function(...) FALSE,
            .package = "base"
        )
        expect_error(
            api_session__publish(ctx$config, control),
            "Cannot publish"
        )
        session$close()
        expect_false(dir.exists(ctx$config$snapshot_dir))
    })

    test_that("stock sparse snapshots skip unrequested steps without losing exact commands or replay", {
        fixture <- testrun__api_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        expect_error(eplus_session(
            fixture$model,
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
            fixture$weather,
            sample_every = 0L
        ))
        expect_error(eplus_session(
            fixture$model,
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
            fixture$weather,
            sample_every = 1.5
        ))
        for (record in c(FALSE, TRUE)) {
            session <- eplus_session(
                fixture$model,
                eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
                fixture$weather,
                variables = list(
                    t = c("Zone Mean Air Temperature", "ZONE ONE")
                ),
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
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
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
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
            sample_every = 100000L,
            variables = list(t = c("Zone Mean Air Temperature", "ZONE ONE")),
            dir = file.path(fixture$directory, "sparse-commands")
        )
        on.exit(try(session$close(), silent = TRUE), add = TRUE)
        first <- session$initialize()
        # Force a second calendar day so the assertion cannot depend on the worker winning an IPC race.
        boundary <- session$step(n = 96L)
        expect_gt(boundary$step, first$step)
        session$resume()
        observed <- session$observe()
        expect_gte(observed$step, boundary$step)
        expect_equal(
            observed$time_end,
            ((observed$step - 1L) %% 96L + 1L) / 4,
            tolerance = 1e-10
        )
        expect_equal(observed$day, 1L + (observed$step - 1L) %/% 96L)
        paused <- session$pause()
        expect_gte(paused$step, observed$step)
        if (session$status()$phase == "paused") {
            expect_identical(session$observe(wait = FALSE), paused)
            session$resume()
        }
        expect_true(session$result()$status()$successful)
        expect_identical(session$status()$outcome$completed_steps, 672L)
    })

    # A model from another supported release must not bypass the frozen installation identity.
    test_that("sessions reject model and installation version mismatches before starting workers", {
        local_mocked_bindings(
            eplus_config = function(...) {
                list(version = "26.1.0", dir = tempdir())
            },
            eplus_job = function(...) list(version = function() "26.2.0")
        )
        expect_error(
            eplus_session("model.idf", "weather.epw"),
            "model version must match"
        )
    })

    # Completed observations distinguish calendar availability from EPW year across a real year boundary.
    test_that("stock sessions preserve calendar provenance across December and January", {
        fixture <- testrun__api_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        model <- read_idf(fixture$model)
        model$set(
            RunPeriod := list(
                begin_month = 12,
                begin_day_of_month = 31,
                begin_year = 2019,
                end_month = 1,
                end_day_of_month = 1,
                end_year = 2020
            )
        )
        model$save(fixture$model, overwrite = TRUE)
        cat(
            "\nOutput:Variable,ZONE ONE,Zone Mean Air Temperature,Timestep;\n",
            file = fixture$model,
            append = TRUE
        )
        session <- eplus_session(
            fixture$model,
            fixture$weather,
            eplus = version,
            variables = list(
                temperature = c("Zone Mean Air Temperature", "ZONE ONE")
            ),
            dir = file.path(fixture$directory, "calendar"),
            sample_every = 128L
        )
        on.exit(try(session$close(), silent = TRUE), add = TRUE)
        first <- session$initialize()
        december <- session$step(n = 95L)
        expect_identical(session$observe(wait = FALSE), december)
        january <- session$step(n = 1L)
        expect_identical(session$observe(wait = FALSE), january)
        expect_identical(first$month, 12L)
        expect_identical(first$day, 31L)
        expect_identical(january$month, 1L)
        expect_identical(january$day, 1L)
        expect_identical(january$step, 97L)
        expect_true(is.finite(january$values[["temperature"]]))
        for (observation in list(first, december, january)) {
            expect_identical(
                observation$calendar_year_available,
                numeric_version(version) >= "23.2"
            )
            expect_true(is.finite(observation$weather_year))
        }
        if (numeric_version(version) < "23.2") {
            expect_identical(first$year, NA_integer_)
            expect_identical(december$year, NA_integer_)
            expect_identical(january$year, NA_integer_)
        } else {
            expect_identical(first$year, 2019L)
            expect_identical(december$year, 2019L)
            expect_identical(january$year, 2020L)
        }
        expect_equal(
            c(
                december$month,
                december$day,
                december$time_start,
                december$time_end
            ),
            c(12, 31, 23.75, 24)
        )
        session$resume()
        job <- session$result()
        expect_true(job$status()$successful)
        # SQLite records the completed interval before native calendarYear advances for yearly reporting.
        times <- job$read_table("Time")
        row <- subset(
            times,
            month == 12 & day == 31 & hour == 24 & minute == 0 & interval == 15
        )
        expect_equal(nrow(row), 1L)
        expect_equal(
            c(december$month, december$day, december$time_end),
            c(row$month, row$day, row$hour)
        )
        expect_equal(december$time_start, row$hour - row$interval / 60)
        if (numeric_version(version) >= "23.2") {
            expect_equal(december$year, row$year)
        }
        session$close()
    })

    # A deterministic R history proves hook, commit, observation and policy ordering in real workers.
    test_that("stock sessions commit bounded plugin history before observation and control", {
        fixture <- testapi__controlled_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        plugin <- eplus_plugin(
            list(end_zone_timestep_after_zone_reporting = function(
                api,
                state,
                plugin
            ) {
                if (api$exchange$kind_of_sim(state) != 3L) {
                    return(NULL)
                }
                plugin$data$count <- plugin$data$count + 1L
                if (plugin$data$count > 1L) {
                    handle <- api$exchange$get_trend_handle(state, "sequence")
                    previous <- api$exchange$get_trend_value(state, handle, 1L)
                    if (previous != plugin$data$count - 1L) {
                        stop("hook history order")
                    }
                }
            }),
            data = list(count = 0L),
            trends = list(
                sequence = list(
                    source = function(api, state, plugin) plugin$data$count,
                    capacity = 3L
                )
            )
        )
        controller <- function(observation, state) {
            trend <- observation$plugins[[1L]]$trends$sequence
            if (trend$values[[1L]] != observation$step) {
                stop("policy history order")
            }
            list(action = NULL, state = state + 1L)
        }
        histories <- lapply(c(1L, 31L), function(frequency) {
            session <- eplus_session(
                fixture$model,
                fixture$weather,
                eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
                backend = plugin,
                controller = controller,
                controller_state = 0L,
                sample_every = frequency,
                dir = file.path(
                    fixture$directory,
                    paste0("history-", frequency)
                )
            )
            on.exit(try(session$close(), silent = TRUE), add = TRUE)
            first <- session$initialize()
            expect_identical(first$plugins[[1L]]$trends$sequence$values, 1)
            expect_identical(
                first$plugins[[1L]]$trends$sequence$valid_count,
                1L
            )
            expect_identical(session$observe(), first)
            expect_identical(session$observe(wait = FALSE), first)
            fourth <- session$step(n = 3L)
            trend <- fourth$plugins[[1L]]$trends$sequence
            expect_identical(trend$values, c(4, 3, 2))
            expect_identical(trend$times, c(1, 0.75, 0.5))
            expect_identical(trend$total_samples, 4L)
            expect_identical(trend$valid_count, 3L)
            expect_identical(fourth$controller_state, 3L)
            expect_identical(session$observe(), fourth)
            session$resume()
            session$result()
            final <- session$observe()
            expect_identical(
                final$plugins[[1L]]$trends$sequence$values,
                c(96, 95, 94)
            )
            expect_identical(
                final$plugins[[1L]]$trends$sequence$total_samples,
                96L
            )
            expect_identical(
                session$status()$outcome$plugins[[
                    1L
                ]]$trends$sequence$total_samples,
                96L
            )
            session$close()
            expect_false(session$status()$alive)
            final$plugins
        })
        expect_identical(histories[[1L]], histories[[2L]])
    })

    # Sparse publication must still advance sources on every completed weather zone timestep.
    test_that("stock sparse session publication retains exact managed history", {
        fixture <- testrun__api_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        plugin <- eplus_plugin(
            list(initialize = function(api, state, plugin) NULL),
            data = list(n = 0L),
            trends = list(
                n = list(
                    source = function(api, state, plugin) {
                        plugin$data$n <- plugin$data$n + 1L
                        plugin$data$n
                    },
                    capacity = 1L
                )
            )
        )
        session <- eplus_session(
            fixture$model,
            fixture$weather,
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
            backend = plugin,
            sample_every = 31L,
            dir = file.path(fixture$directory, "sparse-history")
        )
        on.exit(try(session$close(), silent = TRUE), add = TRUE)
        first <- session$initialize()
        expect_identical(first$plugins[[1L]]$trends$n$values, 1)
        expect_identical(session$pause(), first)
        expect_identical(session$observe(wait = FALSE), first)
        session$resume()
        session$result()
        sampled <- session$observe(wait = FALSE)
        expect_identical(sampled$step, 93L)
        expect_identical(sampled$plugins[[1L]]$trends$n$values, 93)
        expect_identical(sampled$plugins[[1L]]$trends$n$total_samples, 93L)
        expect_identical(
            session$status()$outcome$plugins[[1L]]$trends$n$total_samples,
            96L
        )
        expect_identical(
            session$status()$outcome$plugins[[1L]]$trends$n$valid_count,
            1L
        )
        session$close()
        expect_false(session$status()$alive)
    })

    # Source conditions use the existing deferred boundary, and cleanup cannot replace the first failure.
    test_that("stock session trend source failures and timeouts reap their workers", {
        fixture <- testrun__api_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        expect_error(
            eplus_session(
                fixture$model,
                fixture$weather,
                eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
                backend = "cli"
            ),
            "requires the API backend"
        )
        for (failure in c("error", "interrupt", "timeout")) {
            plugin <- eplus_plugin(
                list(finalize = function(api, state, plugin) {
                    plugin$data$finalized <- TRUE
                    if (failure != "timeout") stop("later cleanup failure")
                }),
                data = list(n = 0L),
                trends = list(
                    n = list(
                        source = function(api, state, plugin) {
                            plugin$data$n <- plugin$data$n + 1L
                            if (plugin$data$n == 2L) {
                                if (failure == "timeout") {
                                    Sys.sleep(5)
                                } else {
                                    stop(structure(
                                        list(
                                            message = "first session source failure",
                                            call = NULL
                                        ),
                                        class = c(
                                            paste0("source_", failure),
                                            failure,
                                            "condition"
                                        )
                                    ))
                                }
                            }
                            plugin$data$n
                        },
                        capacity = 2L
                    )
                )
            )
            session <- eplus_session(
                fixture$model,
                fixture$weather,
                eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
                backend = plugin,
                dir = file.path(fixture$directory, failure)
            )
            on.exit(try(session$close(), silent = TRUE), add = TRUE)
            first <- session$initialize()
            expect_identical(first$plugins[[1L]]$trends$n$values, 1)
            if (failure == "timeout") {
                expect_error(session$step(timeout = 0.1), "timed out")
            } else {
                expect_error(session$step(), "first session source failure")
                outcome <- session$status()$outcome
                expect_identical(outcome$state_phase, "closed")
                expect_true(
                    paste0("source_", failure) %in% session$status()$error$class
                )
                expect_identical(outcome$plugins[[1L]]$calls[["finalize"]], 1L)
                expect_identical(
                    outcome$plugins[[1L]]$trends$n$total_samples,
                    1L
                )
            }
            expect_false(session$status()$alive)
            expect_identical(
                session$observe(wait = FALSE)$plugins,
                first$plugins
            )
            session$close()
        }
    })

    # Losing the IPC peer closes the authenticated socket while its owner remains alive through cleanup.
    test_that("stock plugin sessions stop after IPC disconnection and early close", {
        skip_if_not_installed("ps")
        fixture <- testrun__api_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        ready <- file.path(fixture$directory, "owner-ready.rds")
        directory <- file.path(fixture$directory, "disconnected-history")
        owner <- callr::r_bg(
            function(model, weather, version, directory, ready) {
                library(eplusr)
                plugin <- eplus_plugin(
                    list(finalize = function(api, state, plugin) {
                        saveRDS(
                            api$state_manager$status(state)$phase,
                            file.path(directory, "finalized.rds")
                        )
                    }),
                    trends = list(
                        n = list(
                            source = function(api, state, plugin) 1,
                            capacity = 1L
                        )
                    )
                )
                session <- eplus_session(
                    model,
                    weather,
                    eplus = version,
                    backend = plugin,
                    dir = directory
                )
                session$initialize()
                identity <- session$status()$identity
                saveRDS(identity, ready)
                handles <- lapply(
                    c(identity$pid, identity$pipeline_pid),
                    ps::ps_handle
                )
                # Drop the owner's real R socket without exiting its process or invoking processx cleanup.
                closeAllConnections()
                deadline <- Sys.time() + 30
                while (
                    any(vapply(handles, ps::ps_is_running, logical(1L))) &&
                        Sys.time() < deadline
                ) {
                    Sys.sleep(0.05)
                }
                list(
                    alive = vapply(handles, ps::ps_is_running, logical(1L)),
                    finalized = file.exists(file.path(
                        directory,
                        "finalized.rds"
                    ))
                )
            },
            args = list(
                fixture$model,
                fixture$weather,
                Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
                directory,
                ready
            ),
            libpath = .libPaths(),
            stdout = "|",
            stderr = "|",
            supervise = FALSE
        )
        on.exit(if (owner$is_alive()) owner$kill(), add = TRUE)
        deadline <- Sys.time() + 120
        while (
            !file.exists(ready) && owner$is_alive() && Sys.time() < deadline
        ) {
            Sys.sleep(0.05)
        }
        expect_true(
            file.exists(ready),
            info = paste(owner$read_error_lines(), collapse = "\n")
        )
        identity <- readRDS(ready)
        handles <- lapply(c(identity$pid, identity$pipeline_pid), ps::ps_handle)
        on.exit(
            for (handle in handles) {
                if (ps::ps_is_running(handle)) {
                    try(ps::ps_kill(handle), silent = TRUE)
                }
            },
            add = TRUE
        )
        # The owner reports worker cleanup before returning, so process exit cannot satisfy this assertion.
        owner$wait(timeout = 45000)
        expect_false(owner$is_alive())
        outcome <- owner$get_result()
        expect_false(any(outcome$alive))
        expect_true(outcome$finalized)
        expect_false(any(vapply(handles, ps::ps_is_running, logical(1L))))
        plugin <- eplus_plugin(
            list(finalize = function(api, state, plugin) {
                plugin$data$finalized <- TRUE
            }),
            trends = list(
                n = list(source = function(api, state, plugin) 1, capacity = 1L)
            )
        )
        session <- eplus_session(
            fixture$model,
            fixture$weather,
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
            backend = plugin,
            dir = file.path(fixture$directory, "early-history-close")
        )
        on.exit(try(session$close(), silent = TRUE), add = TRUE)
        first <- session$initialize()
        session$close()
        expect_identical(session$observe(), first)
        expect_false(session$status()$alive)
        expect_identical(
            session$status()$outcome$plugins[[1L]]$trends$n$total_samples,
            1L
        )
        expect_true(session$status()$outcome$plugins[[1L]]$data$finalized)
    })
})

describe("session-errors", {
    test_that("script setup errors, unknown bindings and command timeout stop stock sessions", {
        fixture <- testrun__api_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        for (mode in c("script", "variable", "actuator", "timeout")) {
            controller <- NULL
            variables <- list()
            actuators <- list()
            if (mode == "script") {
                controller <- file.path(
                    fixture$directory,
                    "missing-controller.R"
                )
                writeLines("ordinary_value <- 1", controller)
            } else if (mode == "variable") {
                variables <- list(missing = c("Unknown Output", "ZONE ONE"))
            } else if (mode == "actuator") {
                actuators <- list(
                    missing = c(
                        "Schedule:Constant",
                        "Schedule Value",
                        "Unknown"
                    )
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
                eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
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
        root <- eplus_config(Sys.getenv(
            "_EPLUSR_TEST_API_VERSION_",
            "26.1"
        ))$dir
        expect_error(
            eplus_session(fixture$model, NULL, eplus = root),
            "requires weather"
        )
        expect_error(eplus_session(
            fixture$model,
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
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
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
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
        expect_identical(
            session$status()$outcome$session_reason,
            "connection_lost"
        )
        expect_identical(session$observe(), paused)
        session$close()
    })

    test_that("session transport failures retain native diagnostics and bounded termination", {
        for (mode in c(
            "unexpected",
            "eof",
            "terminal",
            "bad_final",
            "timeout"
        )) {
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
                api_session__send(
                    pair$client,
                    list(type = "boundary", id = 99L)
                )
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

    test_that("idle readiness polling avoids the Windows empty-select floor without reporting writable sockets as input", {
        pair <- testapi__protocol_pair()
        on.exit(
            {
                api_session__disconnect(pair$ctx)
                try(close(pair$client), silent = TRUE)
            },
            add = TRUE
        )
        select <- base::socketSelect
        # Keep actual TCP readiness but emulate R's Windows minimum wait when no descriptor is ready.
        local_mocked_bindings(
            socketSelect = function(socklist, write = FALSE, timeout = 0) {
                ready <- select(socklist, write = write, timeout = timeout)
                if (timeout == 0 && !any(ready)) {
                    Sys.sleep(0.2)
                }
                ready
            },
            .package = "base"
        )
        started <- proc.time()[["elapsed"]]
        for (index in 1:5) {
            expect_false(api_session__readable(pair$ctx$connection))
        }
        expect_lt(proc.time()[["elapsed"]] - started, 0.5)
        message <- list(type = "boundary", id = 1L)
        api_session__send(pair$client, message)
        # TCP delivery itself is asynchronous; poll until the message arrives without blocking an idle probe.
        deadline <- Sys.time() + 2
        while (
            !api_session__readable(pair$ctx$connection) && Sys.time() < deadline
        ) {
            Sys.sleep(0.001)
        }
        expect_true(api_session__readable(pair$ctx$connection))
        expect_identical(api_session__receive(pair$ctx$connection), message)
        close(pair$client)
        deadline <- Sys.time() + 2
        while (
            !api_session__readable(pair$ctx$connection) && Sys.time() < deadline
        ) {
            Sys.sleep(0.001)
        }
        expect_true(api_session__readable(pair$ctx$connection))
        expect_null(api_session__receive(pair$ctx$connection))
    })
})

describe("replay", {
    test_that("replay prediction validates ownership, phases and bounded history", {
        ctx <- testapi__session_context()
        session <- api_session__construct(ctx)
        on.exit(session$close(), add = TRUE)
        expect_error(
            session$predict(list(a = list(NULL)), 1L),
            "recorded paused"
        )
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
        expect_error(
            session$predict(list(a = list(c(unknown = 1))), 1L),
            "Actions"
        )
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
        expect_error(
            session$predict(list(mutate = mutator), 1L),
            "inputs.*changed"
        )
        writeBin(original_bytes, context$replay$model)
        expect_identical(session$status()$phase, "paused")
        # A changed snapshot is rejected before touching the original or a branch engine.
        writeLines("changed", context$replay$model)
        expect_error(
            session$predict(list(a = list(NULL)), 1L),
            "inputs.*changed"
        )
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
            suppressWarnings(api_replay__write(
                root,
                ctx$observation,
                NULL,
                NULL
            )),
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
        expect_error(
            session$predict(list(a = list(NULL)), 1L),
            "inputs.*changed"
        )
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
        result <- session$predict(
            list(first = list(NULL), second = list(NULL)),
            1L
        )
        expect_identical(calls, 2L)
        expect_identical(result$branches$first$status, "failed")
        expect_identical(
            result$branches$first$reason,
            "Prediction branch failed."
        )
        expect_identical(
            result$branches$second$error$message,
            "candidate failure"
        )
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
                session$predict(
                    list(first = list(NULL), later = list(NULL)),
                    1L
                )
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
        expect_identical(
            origin$plugins[[1L]]$trends$total$values,
            c(64, 42, 20)
        )
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
        prediction <- session$predict(
            list(warmer = warmer, cooler = cooler),
            2L
        )
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
                branch$observations[[2L]]$plugins[[
                    1L
                ]]$trends$total$valid_count,
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
        interrupted_worker <- file.path(
            fixture$directory,
            "interrupted-worker.rds"
        )
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
        attempt <- NULL
        interrupted <- tryCatch(
            {
                attempt <- session$predict(
                    list(cancel = cancel, later = cooler),
                    1L,
                    dir = interrupted_directory
                )
                FALSE
            },
            interrupt = function(condition) TRUE
        )
        # Retain plain terminal classification if a platform loses interrupt propagation.
        diagnostics <- lapply(attempt$branches, function(branch) {
            list(
                status = branch$status,
                reason = branch$reason,
                error = branch$error,
                outcome = branch$outcome[c(
                    "phase",
                    "stop_reason",
                    "error",
                    "native_exit_status"
                )]
            )
        })
        expect_true(
            interrupted,
            info = jsonlite::toJSON(diagnostics, auto_unbox = TRUE)
        )
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
        accepted <- session$predict(
            list(tolerant = future),
            1L,
            tolerance = 1e-6
        )
        expect_identical(accepted$branches$tolerant$status, "success")
        expect_true(file.exists(marker))
        actual <- accepted$branches$tolerant$boundary$plugins[[
            1L
        ]]$trends$identity
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
                    saveRDS(
                        c(Sys.getpid(), ps::ps_ppid(ps::ps_handle())),
                        ready
                    )
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
        while (
            !file.exists(ready) && owner$is_alive() && Sys.time() < deadline
        ) {
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
})

# The reviewed bundled runtime must survive reset and close/reopen without sharing an active owner.
describe("legacy-python", {
    test_that("stock 9.6 Linux restarts its selected CPython dependency across owned lifecycles", {
        version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        skip_if_not(version == "9.6" && is_linux())
        fixture <- testrun__api_fixture()
        on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
        api <- eplus_api(version)
        on.exit(try(api$close(), silent = TRUE), add = TRUE)
        # Every cycle executes the actual stock model and reports a completed native state.
        execute <- function(api, state, name) {
            expect_identical(
                api$runtime$run_energyplus(
                    state,
                    c(
                        "-w",
                        fixture$weather,
                        "-d",
                        file.path(fixture$directory, name),
                        fixture$model
                    )
                ),
                0L
            )
            expect_identical(api$state_manager$status(state)$phase, "completed")
        }
        state <- api$state_manager$new_state()
        execute(api, state, "first")
        api$state_manager$reset_state(state)
        execute(api, state, "after-reset")
        api$close()
        api <- eplus_api(version)
        state <- api$state_manager$new_state()
        execute(api, state, "after-reopen")
        other <- api$state_manager$new_state()
        condition <- tryCatch(
            api$runtime$run_energyplus(
                other,
                c(
                    "-w",
                    fixture$weather,
                    "-d",
                    file.path(fixture$directory, "active-owner"),
                    fixture$model
                )
            ),
            error = identity
        )
        expect_s3_class(condition, "eplusr_api_error")
        expect_match(conditionMessage(condition), "already initialized")
        expect_identical(api$state_manager$status(other)$phase, "created")
        api$state_manager$delete_state(state)
        execute(api, other, "after-owner-retired")
        information <- api$info()$python_runtime
        message(
            "Selected CPython ",
            information$version,
            " from ",
            information$library
        )
        expect_match(information$version, "^3[.]7[.]")
        expect_identical(information$wchar_bytes, 4L)
        expect_true(startsWith(
            normalizePath(information$library),
            paste0(normalizePath(eplus_config(version)$dir), "/")
        ))
        api$close()
    })
})
