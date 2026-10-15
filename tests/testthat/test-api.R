test_that("API metadata and state ownership are explicit", {
    ctx <- testapi__fake()
    api <- ctx$api
    expect_s3_class(api, "EnergyPlusAPI")
    expect_equal(api$info()$version, "26.1.0-fake")
    expect_equal(nrow(api$methods()), 155L)
    state <- api$state_manager$new_state()
    expect_equal(api$state_manager$status(state)$phase, "created")
    other <- testapi__fake()
    expect_error(other$api$state_manager$delete_state(state), "different API")
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
    expect_error(psych$density(state, 101325, 24, .009), "Functional object")
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
    expect_error(api$exchange$get_variable_value(state, handle), "generation")
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
    expect_equal(api$exchange$get_api_data(state)$name, rep("Temperature", 2L))
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
    expect_error(api$runtime$run_energyplus(state, testapi__model()), "reset")
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
    expect_error(api$runtime$run_energyplus(state, "missing.idf"), "existing")
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
    expect_error(api__arguments("Z)v", list(NA_character_), "name"), "string")
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
    expect_null(attr(compile, "status"), info = paste(compile, collapse = "\n"))
    library <- rdyncall::dynload(
        file.path(directory, paste0("callback-thread", .Platform$dynlib.ext)),
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
        expect_equal(rdyncall::dyncall(worker, "pi)i", guard, kind - 1L), 0L)
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
    # Unsupported methods remain inspectable when their optional native symbol is absent.
    ctx$omit <- "inputFilePath"
    api <- api__load(ctx$directory)
    expect_match(api$info()$version, "26.1.0")
    methods <- api$methods()
    expect_equal(methods$signature[methods$symbol == ctx$omit], "")
    state <- api$state_manager$new_state()
    api$runtime$issue_text(state, "positional arguments")
    expect_null(names(ctx$arguments))
    # Create and diagnose the real guarded trampoline, while registration is mocked.
    api$runtime$callback_begin_new_environment(state, function(s) NULL)
    api$state_manager$delete_state(state)
    api$close()
    expect_equal(ctx$deleted, 4L)
    expect_identical(ctx$handle, ctx$pointer)
    expect_equal(ctx$unloaded, 7L)
})

test_that("constructor uses installation discovery and rejects non-26.1 versions", {
    local_mocked_bindings(
        is_eplus_path = function(path) identical(path, "stock-installation"),
        use_eplus = function(path) list(version = "26.1.0", dir = path),
        eplus_config = function(version) {
            list(version = paste0(version, ".0"), dir = "discovered")
        },
        api__check_bridge = function() invisible(NULL),
        api__load = identity
    )
    expect_equal(eplus_api("stock-installation"), "stock-installation")
    expect_equal(eplus_api("26.1"), "discovered")
    expect_error(eplus_api("26.2"), "26.1 installation")
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
        dynport_load_into = function(portfile, envir, libhandle = NULL) NULL,
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
    expect_error(api__check_bridge(), "safety probe failed: probe child failed")
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
            result <- tryCatch(fun(), error = identity, interrupt = identity)
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
        "ownership|strings|allocat|reclaim"
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
    expect_equal(ctx$api$runtime$run_energyplus(state, testapi__model()), 1L)
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
    expect_equal(api__library_name("windows", "Windows"), "EnergyPlusAPI.dll")
    expect_equal(api__library_name("unix", "Darwin"), "libenergyplusapi.dylib")
    expect_equal(api__library_name("unix", "Linux"), "libenergyplusapi.so")
    manifest <- api__manifest()
    manifest$pointer_bytes <- 4L
    local_mocked_bindings(api__manifest = function() manifest)
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
            api$runtime$callback_begin_new_environment(state, function(s) NULL)
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
        args = list(eplus_config("26.1")$dir),
        libpath = .libPaths()
    )
    expect_true("interrupt" %in% result$class)
    expect_identical(result$status$stop_reason, "interrupt")
    expect_identical(result$status$phase, "closed")
    expect_false(result$quarantined)
})
