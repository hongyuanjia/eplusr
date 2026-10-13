# Read generated signatures for native stand-ins without loading an optional bridge.
testapi__signatures <- function() {
    functions <- read.dcf(system.file(
        "dynports",
        "EnergyPlus-26.1.dynport",
        package = "eplusr"
    ))[1L, "Function"]
    lines <- trimws(strsplit(functions, "\n", fixed = TRUE)[[1L]])
    names <- sub("\\(.*", "", lines)
    signatures <- sub("^[^(]*\\(([^; ]+).*", "\\1", lines)
    setNames(as.list(signatures), names)
}

# A stand-in typeinfo lets copied data exercise layout offsets rather than hard-coded strides.
testapi__layout <- function() {
    structure(
        list(
            size = 40L,
            fields = data.frame(
                name = c("what", "name", "key", "type", "unit"),
                offset = seq.int(0L, 32L, 8L)
            )
        ),
        class = "typeinfo"
    )
}

# Build a native-call stand-in with real lifecycle records and callback execution.
testapi__fake <- function() {
    ctx <- new.env(parent = emptyenv())
    ctx$directory <- tempfile()
    ctx$library <- "fake-library"
    ctx$version <- "26.1.0-fake"
    ctx$api_version <- "0.2"
    ctx$manifest <- api__manifest()
    ctx$signatures <- testapi__signatures()
    ctx$api_data <- testapi__layout()
    ctx$log <- list()
    ctx$deleted <- 0L
    ctx$unloaded <- 0L
    ctx$rows <- 1L
    ctx$bad_value <- FALSE
    ctx$fail_native <- FALSE
    ctx$null_resource <- FALSE
    ctx$hook <- NULL
    ctx$call <- function(symbol, arguments = list()) {
        ctx$log[[length(ctx$log) + 1L]] <- list(
            symbol = symbol,
            arguments = arguments
        )
        if (symbol == "stateNew") {
            state <- new.env(parent = emptyenv())
            state$ready <- FALSE
            state$flag <- FALSE
            state$stop <- FALSE
            state$callbacks <- list()
            return(state)
        }
        if (symbol == "stateDelete") {
            ctx$deleted <- ctx$deleted + 1L
            return(NULL)
        }
        if (symbol == "stateReset") {
            arguments[[1L]]$ready <- FALSE
            arguments[[1L]]$callbacks <- list()
            arguments[[1L]]$stop <- FALSE
            return(NULL)
        }
        if (symbol == "energyPlusVersion") {
            return(ctx$version)
        }
        if (symbol == "apiErrorFlag") {
            return(as.integer(arguments[[1L]]$flag))
        }
        if (symbol == "resetErrorFlag") {
            arguments[[1L]]$flag <- FALSE
            return(NULL)
        }
        if (symbol == "apiDataFullyReady") {
            return(as.integer(arguments[[1L]]$ready))
        }
        if (symbol == "numTimeStepsInHour") {
            return(4L)
        }
        if (grepl("^(callback|register)", symbol)) {
            arguments[[1L]]$callbacks[[symbol]] <- arguments[[2L]]
            return(NULL)
        }
        if (symbol == "stopSimulation") {
            arguments[[1L]]$stop <- TRUE
            return(NULL)
        }
        if (symbol == "energyplus") {
            if (ctx$fail_native) {
                stop("unexpected native unwind")
            }
            pointer <- arguments[[1L]]
            pointer$ready <- TRUE
            for (i in seq_len(3L)) {
                for (name in names(pointer$callbacks)) {
                    pointer$callbacks[[name]](pointer)
                }
                if (pointer$stop) break
            }
            return(0L)
        }
        if (symbol %in% c("glycolNew", "refrigerantNew")) {
            if (ctx$null_resource) {
                return(NULL)
            }
            return("resource-pointer")
        }
        if (symbol == "getAPIData") {
            assign("count", ctx$rows, envir = arguments[[2L]])
            return("data-pointer")
        }
        if (grepl("Handle$", symbol)) {
            return(if ("missing" %in% arguments) -1L else 1L)
        }
        if (grepl("Value$", symbol) && ctx$bad_value) {
            arguments[[1L]]$flag <- TRUE
        }
        if (!is.null(ctx$hook)) {
            ctx$hook(symbol, arguments)
        }
        signature <- ctx$signatures[[symbol]]
        type <- substr(signature, nchar(signature), nchar(signature))
        switch(type, v = NULL, d = 0, i = 0L, I = 0L, Z = "", p = "pointer")
    }
    ctx$ffi <- list(
        callback = function(signature, fun) fun,
        diagnostic = function(callback) {
            list(foreign = 0L, bridge = list(error_invocations = 0L))
        },
        release = function(callback) NULL,
        argv = identity,
        live = function() TRUE,
        null = is.null,
        pointer = function(value) parent.frame(),
        unpack = function(pointer, offset, type) {
            c("OutputVariable", "Temperature", "ZONE ONE", "", "C")[
                (offset %% 40L) / 8L + 1L
            ]
        },
        string = identity,
        unload = function() {
            ctx$unloaded <- ctx$unloaded + 1L
        }
    )
    ctx$api <- api__construct(ctx)
    ctx
}

# A prepared input lets the runtime stand-in exercise its R lifecycle without EnergyPlus.
testapi__model <- function(text = "Version,26.1;") {
    path <- tempfile(fileext = ".idf")
    writeLines(text, path)
    path
}

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
    for (kind in c("foreign", "bridge")) {
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
                    error_invocations = as.integer(kind == "bridge"),
                    disable_reason = "boundary_error"
                )
            )
        }
        expect_error(
            api$runtime$run_energyplus(state, testapi__model()),
            "foreign thread|bridge disabled"
        )
        expect_false(invoked)
        expect_equal(api$state_manager$status(state)$phase, "closed")
        expect_equal(ctx$deleted, 1L)
        api$close()
    }
})

# A loader seam proves identity checks and cleanup without an installed engine.
testapi__loader <- function() {
    ctx <- new.env(parent = emptyenv())
    ctx$version <- "EnergyPlus, Version 26.1.0-test"
    ctx$api_version <- "0.2"
    ctx$symbols <- TRUE
    ctx$load <- TRUE
    ctx$layout <- TRUE
    ctx$omit <- NULL
    ctx$handle <- NULL
    ctx$null_state <- FALSE
    ctx$fail_identity <- FALSE
    ctx$deleted <- 0L
    ctx$unloaded <- 0L
    ctx$pointer <- rdyncall::as.externalptr(integer(1L))
    ctx$directory <- tempfile("eplusr-loader-")
    dir.create(ctx$directory)
    for (name in c(
        "EnergyPlusAPI.dll",
        "libenergyplusapi.dylib",
        "libenergyplusapi.so"
    )) {
        file.create(file.path(ctx$directory, name))
    }
    ctx$call <- function(address, signature, ...) {
        ctx$arguments <- list(...)
        switch(
            address,
            energyPlusVersion = ctx$version,
            stateNew = if (ctx$null_state) NULL else ctx$pointer,
            apiVersionFromEPlus = {
                if (ctx$fail_identity) {
                    stop("identity conversion")
                }
                ctx$api_version
            },
            stateDelete = {
                ctx$deleted <- ctx$deleted + 1L
                NULL
            },
            NULL
        )
    }
    ctx
}

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
        r = function(...) {
            if (identical(safe, "error")) {
                stop("probe child failed")
            }
            safe
        },
        .package = "callr"
    )
    expect_error(api__check_bridge(), "complete callback")
    safe <- "error"
    expect_error(api__check_bridge(), "complete callback")
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

# A dependency-free proxy exposes protocol and ownership guards without loading an engine.
testapi__session_context <- function() {
    ctx <- new.env(parent = emptyenv())
    ctx$config <- list(
        timeout = 1,
        actuators = list(
            heating = c("Schedule:Constant", "Schedule Value", "Heating")
        ),
        controller = NULL
    )
    ctx$id <- basename(tempfile("test-session-"))
    ctx$owner_pid <- Sys.getpid()
    ctx$phase <- "created"
    ctx$error <- NULL
    ctx$outcome <- NULL
    ctx$observation <- NULL
    ctx$identity <- NULL
    ctx$process <- NULL
    ctx$server <- NULL
    ctx$connection <- NULL
    ctx$request <- 0L
    ctx$terminal <- FALSE
    ctx$job <- list(
        # Supply finalized pipeline metadata for caller-side protocol tests.
        status = function() {
            list(
                successful = TRUE,
                api = list(phase = "completed", error = NULL)
            )
        }
    )
    ctx
}

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

# Derive HVAC controls from the stock example so system substeps are present without a bundled model.
testapi__controlled_fixture <- function() {
    fixture <- testrun__api_fixture()
    additions <- c(
        "ScheduleTypeLimits,Session Temperature,-100,100,Continuous,Temperature;",
        "Schedule:Constant,Session Heating,Session Temperature,20;",
        "Schedule:Constant,Session Cooling,Session Temperature,26;",
        "Schedule:Constant,Session Control,,4;",
        "ThermostatSetpoint:DualSetpoint,Session Thermostat,Session Heating,Session Cooling;",
        "ZoneControl:Thermostat,Session Zone Control,ZONE ONE,Session Control,ThermostatSetpoint:DualSetpoint,Session Thermostat;",
        "ZoneHVAC:EquipmentConnections,ZONE ONE,Session Equipment,Session Supply,,Session Air,Session Return;",
        "ZoneHVAC:EquipmentList,Session Equipment,SequentialLoad,ZoneHVAC:IdealLoadsAirSystem,Session Ideal,1,1;",
        "ZoneHVAC:IdealLoadsAirSystem,Session Ideal,,Session Supply,,,50,13,0.0156,0.0077,NoLimit,,,NoLimit,,,,,ConstantSensibleHeatRatio,0.7,None,,,None,NoEconomizer,None,0.7,0.65;",
        "Output:Variable,ZONE ONE,Zone Thermostat Heating Setpoint Temperature,Timestep;"
    )
    cat(paste(additions, collapse = "\n"), file = fixture$model, append = TRUE)
    fixture
}

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

# Pair real local sockets with deterministic lifecycle metadata for malformed peer responses.
testapi__protocol_pair <- function() {
    ctx <- testapi__session_context()
    listener <- api_session__listen()
    client <- socketConnection(
        "127.0.0.1",
        listener$port,
        blocking = TRUE,
        open = "a+b",
        timeout = 2
    )
    ctx$connection <- socketAccept(
        listener$server,
        blocking = TRUE,
        open = "a+b",
        timeout = 2
    )
    close(listener$server)
    ctx$live <- TRUE
    ctx$process <- list(is_alive = function() ctx$live)
    ctx$job$kill <- function() ctx$live <- FALSE
    list(ctx = ctx, client = client)
}

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
test_that("host plugins validate hooks and freeze independent bindings", {
    expect_error(eplus_plugin(list()))
    expect_error(
        eplus_plugin(list(unknown = function(a, b, c) NULL)),
        "Unknown"
    )
    expect_error(eplus_plugin(list(initialize = function(a) NULL)))
    expect_error(eplus_plugin(
        list(initialize = function(a, b, c) NULL),
        warmup = NA
    ))
    ctx <- testapi__fake()
    api <- ctx$api
    on.exit(try(api$close(), silent = TRUE), add = TRUE)
    state <- api$state_manager$new_state()
    expect_error(
        eplus_plugin(list(initialize = function(a, b, c) NULL), data = state),
        "live"
    )
    expect_error(
        eplus_plugin(list(initialize = function(a, b, c) state)),
        "live"
    )
    amount <- 2L
    factory <- eplus_plugin(list(
        initialize = function(api, state, plugin) {
            plugin$data$total <- amount
        },
        end_zone_timestep_after_zone_reporting = function(api, state, plugin) {
            plugin$data$total <- plugin$data$total + 1L
        }
    ))
    amount <- 100L
    expect_true(is.function(factory))
    expect_s3_class(factory, "EnergyPlusPlugin")
    expect_identical(run__backend(factory)$engine, "api")
    factory(api, state)
    api$runtime$run_energyplus(state, testapi__model())
    first <- api$state_manager$status(state)$plugins[[1L]]
    expect_identical(first$data$total, 5L)
    expect_identical(
        first$calls[["end_zone_timestep_after_zone_reporting"]],
        3L
    )
    api$state_manager$reset_state(state)
    factory(api, state)
    api$runtime$run_energyplus(state, testapi__model())
    expect_identical(
        api$state_manager$status(state)$plugins[[1L]]$data$total,
        5L
    )
    expect_error(factory(api, state), "fresh or reset")
    other <- testapi__fake()
    expect_error(factory(other$api, state), "owned state")
    other$api$close()
})

# Warmup suppression affects timestep hooks, while environment lifecycle hooks remain observable.
test_that("host plugin warmup delivery is explicit", {
    for (warmup in c(FALSE, TRUE)) {
        ctx <- testapi__fake()
        api <- ctx$api
        exchange <- api$exchange
        exchange$warmup_flag <- function(state) TRUE
        state <- api$state_manager$new_state()
        factory <- eplus_plugin(
            list(
                begin_new_environment = function(api, state, plugin) {
                    plugin$data$environment <- plugin$data$environment + 1L
                },
                begin_system_timestep_before_predictor = function(
                    api,
                    state,
                    plugin
                ) {
                    plugin$data$steps <- plugin$data$steps + 1L
                }
            ),
            data = list(environment = 0L, steps = 0L),
            warmup = warmup
        )
        factory(api, state)
        api$runtime$run_energyplus(state, testapi__model())
        data <- api$state_manager$status(state)$plugins[[1L]]$data
        expect_identical(data$environment, 3L)
        expect_identical(data$steps, if (warmup) 3L else 0L)
        api$close()
    }
})

# Native return precedes reverse cleanup, and the first controller failure keeps its identity.
test_that("host plugin cleanup preserves the first error and releases native caches", {
    ctx <- testapi__fake()
    api <- ctx$api
    on.exit(try(api$close(), silent = TRUE), add = TRUE)
    state <- api$state_manager$new_state()
    failure <- simpleError("plugin controller failure")
    eplus_plugin(list(
        initialize = function(api, state, plugin) {
            plugin$handles$temperature <- "worker handle cache"
        },
        begin_system_timestep_before_predictor = function(api, state, plugin) {
            stop(failure)
        },
        finalize = function(api, state, plugin) {
            plugin$data$outcome <- plugin$outcome$phase
            stop("later cleanup failure")
        }
    ))(api, state)
    record <- attr(state, "entry")$plugins[[1L]]
    condition <- tryCatch(
        api$runtime$run_energyplus(state, testapi__model()),
        error = identity
    )
    expect_identical(conditionMessage(condition), conditionMessage(failure))
    expect_identical(api$state_manager$status(state)$phase, "closed")
    expect_identical(record$calls[["finalize"]], 1L)
    expect_identical(record$data$outcome, "failed")
    expect_null(record$context)
    expect_null(record$hooks)
    expect_null(record$api)
    expect_identical(ctx$deleted, 1L)
    api$close()
    expect_identical(record$calls[["finalize"]], 1L)
})

# Initialization and normal finalization failures retire the state at a safe R boundary.
test_that("host plugin initialization and finalizer errors are visible", {
    for (event in c("initialize", "finalize")) {
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        hooks <- list(finalize = function(api, state, plugin) {
            plugin$data$cleaned <- TRUE
        })
        hooks[[event]] <- function(api, state, plugin) {
            stop("plugin lifecycle failure")
        }
        plugin <- eplus_plugin(hooks)
        if (event == "initialize") {
            expect_error(plugin(api, state), "plugin lifecycle failure")
        } else {
            plugin(api, state)
            expect_error(
                api$runtime$run_energyplus(state, testapi__model()),
                "plugin lifecycle failure"
            )
        }
        expect_identical(api$state_manager$status(state)$phase, "closed")
        expect_identical(ctx$deleted, 1L)
        expect_identical(
            api$state_manager$status(state)$plugins[[1L]]$phase,
            "failed"
        )
        api$close()
    }
})

# Reset, deletion and API close also finalize bindings that never entered simulation.
test_that("unused host plugins finalize once and drain every state on close", {
    ctx <- testapi__fake()
    api <- ctx$api
    state <- api$state_manager$new_state()
    eplus_plugin(list(finalize = function(api, state, plugin) {
        plugin$data$cleaned <- TRUE
    }))(api, state)
    record <- attr(state, "entry")$plugins[[1L]]
    api$state_manager$reset_state(state)
    expect_identical(record$calls[["finalize"]], 1L)
    expect_true(record$data$cleaned)
    expect_length(attr(state, "entry")$plugins, 0L)
    bad <- eplus_plugin(list(finalize = function(api, state, plugin) {
        stop("cleanup failed")
    }))
    bad(api, state)
    expect_error(api$state_manager$reset_state(state), "cleanup failed")
    expect_identical(api$state_manager$status(state)$phase, "closed")
    second <- api$state_manager$new_state()
    bad(api, second)
    expect_error(api$close(), "cleanup failed")
    expect_identical(ctx$deleted, 2L)
    expect_identical(ctx$unloaded, 1L)
    expect_true(api$info()$closed)
    expect_silent(api$close())
})

# Lifecycle methods cannot tear down or start an engine while plugin R code is active.
test_that("host plugin hooks cannot recursively change native ownership", {
    ctx <- testapi__fake()
    api <- ctx$api
    state <- api$state_manager$new_state()
    model <- testapi__model()
    eplus_plugin(list(initialize = function(api, state, plugin) {
        plugin$data$errors <- c(
            tryCatch(
                api$state_manager$delete_state(state),
                error = conditionMessage
            ),
            tryCatch(
                api$state_manager$reset_state(state),
                error = conditionMessage
            ),
            tryCatch(api$close(), error = conditionMessage),
            tryCatch(
                api$runtime$run_energyplus(state, model),
                error = conditionMessage
            )
        )
    }))(api, state)
    api$runtime$run_energyplus(state, model)
    errors <- api$state_manager$status(state)$plugins[[1L]]$data$errors
    expect_length(errors, 4L)
    expect_true(all(grepl("running|recursively", errors)))
    expect_identical(ctx$plugin_depth, 0L)
    api$close()
})

# Returning native resources in ordinary plugin data fails before it can reach Job IPC.
test_that("host plugin summaries reject live native data", {
    ctx <- testapi__fake()
    api <- ctx$api
    state <- api$state_manager$new_state()
    eplus_plugin(list(initialize = function(api, state, plugin) {
        plugin$data <- state
    }))(api, state)
    expect_error(api$runtime$run_energyplus(state, testapi__model()), "live")
    expect_null(api$state_manager$status(state)$plugins[[1L]]$data)
    api$close()
})

# Exercise two real plugins on the same 26.1 native state through the existing Job pipeline.
test_that("stock host plugins control the same state in registration order", {
    fixture <- testapi__controlled_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    log <- file.path(fixture$directory, "plugin-order.txt")
    # Each factory snapshots its own label, control and lexical dependencies.
    make_plugin <- function(label, setting) {
        eplus_plugin(
            list(
                initialize = function(api, state, plugin) {
                    api$exchange$request_variable(
                        state,
                        "Zone Thermostat Heating Setpoint Temperature",
                        "ZONE ONE"
                    )
                    plugin$data$steps <- 0L
                    plugin$data$predictions <- 0L
                    plugin$data$heating <- numeric()
                    plugin$data$seen <- numeric()
                    cat(
                        paste0("initialize-", label, "\n"),
                        file = log,
                        append = TRUE
                    )
                },
                begin_system_timestep_before_predictor = function(
                    api,
                    state,
                    plugin
                ) {
                    if (
                        !api$exchange$api_data_fully_ready(state) ||
                            api$exchange$kind_of_sim(state) != 3L
                    ) {
                        return(NULL)
                    }
                    if (is.null(plugin$handles$actuator)) {
                        plugin$handles$actuator <- api$exchange$get_actuator_handle(
                            state,
                            "Schedule:Constant",
                            "Schedule Value",
                            "Session Heating"
                        )
                    }
                    plugin$data$seen <- c(
                        plugin$data$seen,
                        api$exchange$get_actuator_value(
                            state,
                            plugin$handles$actuator
                        )
                    )
                    api$exchange$set_actuator_value(
                        state,
                        plugin$handles$actuator,
                        setting
                    )
                    if (plugin$data$predictions == 0L) {
                        cat(
                            paste0("predictor-", label, "\n"),
                            file = log,
                            append = TRUE
                        )
                    }
                    plugin$data$predictions <- plugin$data$predictions + 1L
                },
                end_zone_timestep_after_zone_reporting = function(
                    api,
                    state,
                    plugin
                ) {
                    if (
                        !api$exchange$api_data_fully_ready(state) ||
                            api$exchange$kind_of_sim(state) != 3L
                    ) {
                        return(NULL)
                    }
                    if (is.null(plugin$handles$variable)) {
                        plugin$handles$variable <- api$exchange$get_variable_handle(
                            state,
                            "Zone Thermostat Heating Setpoint Temperature",
                            "ZONE ONE"
                        )
                    }
                    plugin$data$heating <- c(
                        plugin$data$heating,
                        api$exchange$get_variable_value(
                            state,
                            plugin$handles$variable
                        )
                    )
                    if (plugin$data$steps == 0L) {
                        cat(
                            paste0("report-", label, "\n"),
                            file = log,
                            append = TRUE
                        )
                    }
                    plugin$data$steps <- plugin$data$steps + 1L
                    # Force collection while C++ owns the simulation and callback/context references.
                    if (plugin$data$steps %% 24L == 0L) gc()
                },
                finalize = function(api, state, plugin) {
                    plugin$data$outcome <- plugin$outcome$phase
                    plugin$data$identity <- plugin$identity
                    cat(
                        paste0("finalize-", label, "\n"),
                        file = log,
                        append = TRUE
                    )
                }
            ),
            data = list(label = label)
        )
    }
    first <- make_plugin("A", 22)
    second <- make_plugin("B", 24)
    backend <- function(api, state) {
        first(api, state)
        second(api, state)
    }
    job <- eplus_job(fixture$model, fixture$weather)
    job$run(backend = backend, echo = FALSE)
    status <- job$status()
    expect_true(status$successful)
    plugins <- status$api$plugins
    expect_length(plugins, 2L)
    expect_identical(
        vapply(plugins, function(x) x$data$label, character(1L)),
        c("A", "B")
    )
    expect_true(all(vapply(
        plugins,
        function(x) x$data$steps == 96L,
        logical(1L)
    )))
    expect_true(all(plugins[[2L]]$data$seen == 22))
    expect_true(all(plugins[[2L]]$data$heating == 24))
    expect_identical(
        readLines(log),
        c(
            "initialize-A",
            "initialize-B",
            "predictor-A",
            "predictor-B",
            "report-A",
            "report-B",
            "finalize-B",
            "finalize-A"
        )
    )
    expect_true(all(vapply(
        plugins,
        function(x) x$calls[["finalize"]] == 1L,
        logical(1L)
    )))
    expect_identical(plugins[[1L]]$data$identity$pid, status$api$pid)
    expect_identical(plugins[[2L]]$data$identity$pid, status$api$pid)
    expect_false(identical(status$api$pid, Sys.getpid()))
})

# A Job reports controller, initialize and finalize failures without serializing live state.
test_that("stock host plugin failures remain distinct and retain cleanup summaries", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    for (event in c(
        "initialize",
        "begin_system_timestep_before_predictor",
        "finalize"
    )) {
        hooks <- list(finalize = function(api, state, plugin) {
            plugin$data$cleaned <- TRUE
        })
        hooks[[event]] <- function(api, state, plugin) {
            stop("stock plugin failure")
        }
        plugin <- eplus_plugin(hooks)
        job <- eplus_job(fixture$model, fixture$weather)
        job$run(
            backend = plugin,
            dir = file.path(fixture$directory, event),
            echo = FALSE
        )
        status <- job$status()
        expect_false(status$successful)
        expect_match(status$api$error$message, "stock plugin failure")
        expect_identical(
            status$api$stop_reason,
            switch(
                event,
                initialize = "plugin_initialize_error",
                finalize = "plugin_finalize_error",
                "callback_error"
            )
        )
        expect_identical(status$api$state_phase, "closed")
        expect_identical(status$api$plugins[[1L]]$phase, "failed")
        expect_identical(status$api$plugins[[1L]]$calls[["finalize"]], 1L)
    }
})

# A partially registered plugin must retire its state even if allocating a trampoline fails.
test_that("host plugins clean up partial callback registration", {
    ctx <- testapi__fake()
    api <- ctx$api
    state <- api$state_manager$new_state()
    ctx$ffi$callback <- function(...) {
        stop("plugin trampoline allocation failed")
    }
    plugin <- eplus_plugin(list(
        begin_system_timestep_before_predictor = function(api, state, plugin) {
            NULL
        },
        finalize = function(api, state, plugin) plugin$data$cleaned <- TRUE
    ))
    expect_error(plugin(api, state), "trampoline allocation")
    status <- api$state_manager$status(state)
    expect_identical(status$phase, "closed")
    expect_true(status$plugins[[1L]]$data$cleaned)
    expect_identical(status$plugins[[1L]]$phase, "failed")
    expect_identical(ctx$deleted, 1L)
    api$close()
})

# Standalone native property errors inside initialize are deferred to the outer plugin boundary.
test_that("host initialize preserves nested functional callback errors", {
    ctx <- testapi__fake()
    api <- ctx$api
    state <- api$state_manager$new_state()
    ctx$hook <- function(symbol, arguments) {
        if (symbol == "psyCpAirFnW") {
            arguments[[1L]]$callbacks$registerErrorCallback(
                2L,
                "property warning"
            )
        }
    }
    plugin <- eplus_plugin(list(
        initialize = function(api, state, plugin) {
            api$functional$callback_error(state, function(...) {
                stop("nested plugin property failure")
            })
            api$functional$psychrometrics(state)$specific_heat(state, .01)
        },
        finalize = function(api, state, plugin) plugin$data$cleaned <- TRUE
    ))
    expect_error(plugin(api, state), "nested plugin property failure")
    status <- api$state_manager$status(state)
    expect_true(status$plugins[[1L]]$data$cleaned)
    expect_identical(status$phase, "closed")
    expect_identical(ctx$deleted, 1L)
    expect_identical(ctx$plugin_depth, 0L)
    expect_identical(api__running$plugin_depth, 0L)
    api$close()
})

# Retired callbacks cannot touch discarded native caches, and live contexts cannot enter another worker.
test_that("host plugin contexts are local and finished callbacks are inert", {
    ctx <- testapi__fake()
    api <- ctx$api
    state <- api$state_manager$new_state()
    plugin <- eplus_plugin(
        list(
            begin_system_timestep_before_predictor = function(
                api,
                state,
                plugin
            ) {
                plugin$data$n <- plugin$data$n + 1L
            }
        ),
        data = list(n = 0L)
    )
    expect_error(plugin(api, NULL), "owned state")
    plugin(api, state)
    record <- attr(state, "entry")$plugins[[1L]]
    expect_error(
        eplus_plugin(
            list(initialize = function(a, b, c) NULL),
            data = record$context
        ),
        "live"
    )
    callback <- attr(
        state,
        "entry"
    )$pointer$callbacks$callbackBeginTimeStepBeforePredictor
    api$runtime$run_energyplus(state, testapi__model())
    expect_silent(callback(attr(state, "entry")$pointer))
    expect_identical(record$data$n, 3L)
    expect_identical(
        record$calls[["begin_system_timestep_before_predictor"]],
        3L
    )
    api$close()
    ctx <- testapi__fake()
    api <- ctx$api
    state <- api$state_manager$new_state()
    eplus_plugin(list(finalize = function(a, b, c) {
        stop("delete cleanup failed")
    }))(api, state)
    expect_error(api$state_manager$delete_state(state), "delete cleanup failed")
    expect_identical(ctx$deleted, 1L)
    api$close()
})

# Containers of helper functions must not retain a moving globalenv dependency.
test_that("host plugin recipes freeze list helpers and shared environment aliases", {
    for (container in c("list", "environment", "active")) {
        amount <- 2L
        if (container == "list") {
            helpers <- list(calc = function() amount)
        } else {
            helpers <- new.env(parent = emptyenv())
            helpers$n <- 0L
            helpers$self <- helpers
            helpers$calc <- function() {
                helpers$n <- helpers$n + 1L
                amount
            }
            if (container == "active") {
                makeActiveBinding("value", function(value) amount, helpers)
                lockBinding("value", helpers)
                lockEnvironment(helpers, bindings = FALSE)
            }
        }
        factory <- eplus_plugin(list(
            initialize = function(api, state, plugin) {
                plugin$data$total <- 0L
            },
            end_zone_timestep_after_zone_reporting = function(
                api,
                state,
                plugin
            ) {
                plugin$data$total <- plugin$data$total + helpers$calc()
            },
            finalize = function(api, state, plugin) {
                plugin$data$alias <- if (is.environment(helpers)) {
                    identical(helpers$self, helpers)
                } else {
                    TRUE
                }
                plugin$data$n <- if (is.environment(helpers)) helpers$n else 3L
                plugin$data$active <- if (container == "active") {
                    helpers$value
                } else {
                    2L
                }
            }
        ))
        amount <- 100L
        ctx <- testapi__fake()
        api <- ctx$api
        state <- api$state_manager$new_state()
        factory(api, state)
        api$runtime$run_energyplus(state, testapi__model())
        data <- api$state_manager$status(state)$plugins[[1L]]$data
        expect_identical(data$total, 6L)
        expect_identical(data$n, 3L)
        expect_true(data$alias)
        expect_identical(data$active, 2L)
        api$close()
    }
})

# A hook on one API protects all native ownership in the same R process.
test_that("host plugin lifecycle guards cover another API context", {
    first <- testapi__fake()
    second <- testapi__fake()
    api <- first$api
    state <- api$state_manager$new_state()
    other <- second$api$state_manager$new_state()
    model <- testapi__model()
    record <- new.env(parent = emptyenv())
    record$calls <- c(initialize = 0L)
    record$error <- NULL
    record$context <- new.env(parent = emptyenv())
    # Use the same dispatcher as actual plugin initialization to hold the process guard.
    record$hooks <- list(initialize = function(api, state, plugin) {
        plugin$errors <- c(
            tryCatch(
                second$api$state_manager$delete_state(other),
                error = conditionMessage
            ),
            tryCatch(
                second$api$state_manager$reset_state(other),
                error = conditionMessage
            ),
            tryCatch(second$api$close(), error = conditionMessage),
            tryCatch(
                second$api$runtime$run_energyplus(other, model),
                error = conditionMessage
            )
        )
    })
    plugin__call(first, record, "initialize", api, state)
    expect_length(record$context$errors, 4L)
    expect_true(all(grepl("running|recursively", record$context$errors)))
    expect_identical(second$deleted, 0L)
    expect_identical(second$unloaded, 0L)
    expect_identical(api__running$plugin_depth, 0L)
    api$close()
    second$api$close()
})

# Dependency analysis must distinguish caller references from worker-created locals and quoted code.
test_that("backend dependency freezing handles replacement roots and omitted indices", {
    helper <- new.env(parent = emptyenv())
    helper$n <- 0L
    code <- function(argument) {
        local <- list()
        local$field <- 1
        for (index in 1:2) {
            helper$n <- helper$n + index
        }
        matrix <- matrix(1:4, 2)
        matrix[, 1] <- 0
        quote(unused$field <- 1)
        nested <- function(parameter) helper$n <- helper$n + parameter
        nested(argument)
        helper$n
    }
    globals <- run__setup_globals(code)
    expect_true("helper" %in% globals)
    expect_false(any(
        c("local", "index", "parameter", "matrix", "unused") %in%
            setdiff(globals, codetools::findGlobals(code, merge = TRUE))
    ))
    copy <- run__prepare_setup(code)
    helper$n <- 100L
    expect_identical(copy(2L), 5L)
    expect_identical(helper$n, 100L)
    ctx <- testapi__fake()
    cached <- ctx$api$state_manager$new_state()
    expect_error(
        eplus_plugin(list(initialize = function(api, state, plugin) {
            cached$field <- 1
        })),
        "live"
    )
    ctx$api$close()
})
