# Borrowed states must never be allocated, reset or deleted by the R plugin owner.
test_that("executable bindings protect ownership and keep independent hook state", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    pointer <- new.env(parent = emptyenv())
    pointer$ready <- TRUE
    pointer$flag <- FALSE
    pointer$callbacks <- list()
    contexts <- list()
    local_mocked_bindings(eplus_api = function(...) {
        ctx <- testapi__fake()
        contexts[[length(contexts) + 1L]] <<- ctx
        ctx$api
    })
    plugin <- eplus_plugin(list(
        initialize = function(api, state, plugin) {
            plugin$data$n <- 0L
            plugin$data$blocked <- vapply(
                list(
                    function() api$state_manager$new_state(),
                    function() api$state_manager$delete_state(state),
                    function() api$state_manager$reset_state(state),
                    function() api$close(),
                    function() api$runtime$run_energyplus(state, "missing.idf"),
                    function() {
                        api$runtime$callback_begin_new_environment(
                            state,
                            function(state) NULL
                        )
                    }
                ),
                function(call) {
                    tryCatch(
                        {
                            call()
                            FALSE
                        },
                        error = function(e) TRUE
                    )
                },
                logical(1L)
            )
        },
        begin_new_environment = function(api, state, plugin) {
            plugin$data$n <- plugin$data$n + 1L
            gc()
        },
        finalize = function(api, state, plugin) {
            plugin$data$cleaned <- TRUE
            plugin$data$unavailable <- tryCatch(
                {
                    api$exchange$current_time(state)
                    FALSE
                },
                error = function(e) TRUE
            )
        }
    ))
    saveRDS(
        unserialize(environment(plugin)$payload),
        file.path(directory, "definition.rds")
    )
    config <- list(
        definition = file.path(directory, "definition.rds"),
        installation = "fake",
        directory = directory,
        name = "plugin"
    )
    path <- file.path(directory, "config.json")
    jsonlite::write_json(config, path, auto_unbox = TRUE)
    for (id in c("first", "second")) {
        expect_identical(
            plugin_cli__dispatch(path, "begin_new_environment", id, pointer),
            0L
        )
    }
    expect_identical(
        plugin_cli__dispatch(path, "begin_new_environment", "first", pointer),
        0L
    )
    expect_null(api__running$state)
    for (id in c("first", "second")) {
        expect_identical(
            plugin_cli__dispatch(path, "finalize", id, pointer),
            0L
        )
        expect_identical(
            plugin_cli__dispatch(path, "finalize", id, pointer),
            0L
        )
        binding <- plugin_cli__instances[[id]]
        status <- plugin__status(binding$entry)[[1L]]
        expect_true(all(status$data$blocked))
        expect_true(status$data$cleaned)
        expect_true(status$data$unavailable)
        expect_identical(status$calls[["finalize"]], 1L)
        expect_identical(status$data$n, if (id == "first") 2L else 1L)
        expect_true(file.exists(file.path(
            directory,
            paste0("plugin-results-", id, ".json")
        )))
        expect_identical(
            plugin_cli__dispatch(path, "begin_new_environment", id, pointer),
            1L
        )
        rm(list = id, envir = plugin_cli__instances)
    }
    expect_true(all(vapply(
        contexts,
        function(x) x$deleted == 0L && x$unloaded == 0L,
        logical(1L)
    )))
    expect_identical(
        plugin_cli__dispatch(path, "finalize", "unbound", pointer),
        0L
    )
})

# Failure summaries preserve the first hook condition while finalization still completes.
test_that("executable dispatch catches hook errors and interrupts before native return", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    local_mocked_bindings(eplus_api = function(...) testapi__fake()$api)
    for (event in c(
        "initialize",
        "begin_new_environment",
        "finalize",
        "interrupt"
    )) {
        hooks <- list(finalize = function(api, state, plugin) {
            plugin$data$cleaned <- TRUE
            stop("second cleanup error")
        })
        name <- if (event == "interrupt") "begin_new_environment" else event
        hooks[[name]] <- if (event == "interrupt") {
            function(api, state, plugin) {
                stop(structure(
                    list(message = "plugin interrupted"),
                    class = c("interrupt", "condition")
                ))
            }
        } else {
            function(api, state, plugin) stop("first hook error")
        }
        plugin <- eplus_plugin(hooks)
        saveRDS(
            unserialize(environment(plugin)$payload),
            file.path(directory, "definition.rds")
        )
        path <- file.path(directory, "config.json")
        jsonlite::write_json(
            list(
                definition = file.path(directory, "definition.rds"),
                installation = "fake",
                directory = directory,
                name = "error"
            ),
            path,
            auto_unbox = TRUE
        )
        pointer <- new.env(parent = emptyenv())
        pointer$ready <- TRUE
        pointer$flag <- FALSE
        first <- plugin_cli__dispatch(
            path,
            "begin_new_environment",
            event,
            pointer
        )
        if (event == "finalize") {
            expect_identical(first, 0L)
            expect_identical(
                plugin_cli__dispatch(path, "finalize", event, pointer),
                1L
            )
        } else {
            expect_identical(first, 1L)
        }
        binding <- plugin_cli__instances[[event]]
        status <- plugin__status(binding$entry)[[1L]]
        expect_identical(status$phase, "failed")
        expect_match(
            status$error$message,
            if (event == "interrupt") "interrupted" else "first hook error"
        )
        expect_identical(status$calls[["finalize"]], 1L)
        expect_true(binding$entry$closed)
        expect_true(file.exists(file.path(
            directory,
            paste0("error-results-", event, ".json")
        )))
        expect_identical(
            plugin_cli__dispatch(path, "finalize", event, pointer),
            1L
        )
        rm(list = event, envir = plugin_cli__instances)
    }
    expect_null(api__running$state)
    expect_identical(api__running$plugin_depth, 0L)
    expect_message(expect_identical(
        plugin_cli__dispatch(
            file.path(directory, "missing.json"),
            "begin_new_environment",
            "missing",
            NULL
        ),
        1L
    ))
})

# Exported objects use the executable's actual Python names and reject unsupported versions/paths.
test_that("executable plugin export validates artifacts and hook mappings", {
    skip_if_not_installed("rdyncall")
    directory <- tempfile()
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    plugin <- eplus_plugin(list(
        end_zone_timestep_after_zone_reporting = function(api, state, plugin) {
            NULL
        }
    ))
    expect_error(
        export_eplus_plugin(function() NULL, directory),
        "EnergyPlusPlugin"
    )
    expect_error(
        export_eplus_plugin(plugin, directory, name = "bad-name"),
        "pattern"
    )
    local_mocked_bindings(eplus_config = function(...) list(version = "25.2.0"))
    expect_error(export_eplus_plugin(plugin, directory), "26.1")
    mapping <- plugin_cli__hooks()
    expect_identical(
        unname(mapping[["end_zone_timestep_after_zone_reporting"]]),
        "on_end_of_zone_timestep_after_zone_reporting"
    )
    expect_identical(
        unname(mapping[["end_system_timestep_before_hvac_reporting"]]),
        "on_end_of_system_timestep_before_hvac_reporting"
    )
})

# Run real PythonPlugin-to-R calls in the unmodified executable, sharing one engine process.
test_that("stock executable hosts two R plugin instances and reports cleanup", {
    fixture <- testapi__controlled_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    directory <- file.path(fixture$directory, "plugins")
    # Factories carry independent labels/control values through export and borrowed state binding.
    make <- function(label, setting) {
        eplus_plugin(list(
            initialize = function(api, state, plugin) {
                plugin$data$label <- label
                plugin$data$pid <- Sys.getpid()
                plugin$data$n <- 0L
                plugin$data$seen <- numeric()
                plugin$data$reported <- numeric()
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
                plugin$data$reported <- c(
                    plugin$data$reported,
                    api$exchange$get_variable_value(
                        state,
                        plugin$handles$variable
                    )
                )
                plugin$data$n <- plugin$data$n + 1L
                # Declared native globals and trend aggregates are available only in executable plugins.
                if (is.null(plugin$handles$global)) {
                    plugin$handles$global <- api$exchange$get_global_handle(
                        state,
                        "R Shared"
                    )
                    plugin$handles$trend <- api$exchange$get_trend_handle(
                        state,
                        "R History"
                    )
                    missing <- api$exchange$get_global_handle(
                        state,
                        "Missing R Global"
                    )
                    plugin$data$missing <- as.integer(missing)
                    plugin$data$csv <- api$exchange$list_available_api_data_csv(
                        state
                    )
                    # Rejected calls must return R conditions without poisoning the native plugin.
                    plugin$data$rejected <- vapply(
                        list(
                            function() {
                                api$exchange$get_global_value(state, missing)
                            },
                            function() {
                                api$exchange$get_trend_sum(
                                    state,
                                    plugin$handles$global,
                                    2L
                                )
                            },
                            function() {
                                api$exchange$get_trend_sum(
                                    state,
                                    plugin$handles$trend,
                                    1L
                                )
                            },
                            function() {
                                api$exchange$get_trend_value(
                                    state,
                                    plugin$handles$trend,
                                    4L
                                )
                            }
                        ),
                        function(call) {
                            tryCatch(
                                {
                                    call()
                                    FALSE
                                },
                                error = function(e) TRUE
                            )
                        },
                        logical(1L)
                    )
                }
                api$exchange$set_global_value(
                    state,
                    plugin$handles$global,
                    setting
                )
                plugin$data$global <- api$exchange$get_global_value(
                    state,
                    plugin$handles$global
                )
                plugin$data$trend <- list(
                    average = api$exchange$get_trend_average(
                        state,
                        plugin$handles$trend,
                        2L
                    ),
                    minimum = api$exchange$get_trend_min(
                        state,
                        plugin$handles$trend,
                        2L
                    ),
                    maximum = api$exchange$get_trend_max(
                        state,
                        plugin$handles$trend,
                        2L
                    ),
                    sum = api$exchange$get_trend_sum(
                        state,
                        plugin$handles$trend,
                        2L
                    ),
                    direction = api$exchange$get_trend_direction(
                        state,
                        plugin$handles$trend,
                        2L
                    )
                )
                if (plugin$data$n %% 24L == 0L) gc()
            },
            finalize = function(api, state, plugin) {
                plugin$data$cleaned <- TRUE
            }
        ))
    }
    exports <- list(
        export_eplus_plugin(
            make("A", 22),
            directory,
            "PluginA",
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        ),
        export_eplus_plugin(
            make("B", 24),
            directory,
            "PluginB",
            eplus = eplus_config(Sys.getenv(
                "_EPLUSR_TEST_API_VERSION_",
                "26.1"
            ))$dir
        )
    )
    expect_error(
        export_eplus_plugin(
            make("A", 22),
            directory,
            "PluginA",
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        ),
        "already exist"
    )
    expect_error(
        export_eplus_plugin(
            make("A", 22),
            paste0(directory, ",bad"),
            eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
        ),
        "delimiter"
    )
    cat(
        paste(
            c(
                exports[[1L]]$idf[[1L]],
                exports[[1L]]$idf[[2L]],
                exports[[2L]]$idf[[2L]],
                "PythonPlugin:Variables,R Globals,R Shared;",
                "PythonPlugin:TrendVariable,R History,R Shared,4;"
            ),
            collapse = "\n"
        ),
        file = fixture$model,
        append = TRUE
    )
    process <- processx::process$new(
        file.path(
            eplus_config(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"))$dir,
            eplus_config(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"))$exe
        ),
        c(
            "-w",
            fixture$weather,
            "-d",
            file.path(fixture$directory, "output"),
            fixture$model
        ),
        stdout = "|",
        stderr = "|"
    )
    on.exit(if (process$is_alive()) process$kill_tree(), add = TRUE)
    pid <- process$get_pid()
    # Instrumented startup and full GC can exceed the ordinary simulation duration.
    process$wait(180000)
    expect_false(process$is_alive())
    expect_identical(
        process$get_exit_status(),
        0L,
        info = paste(process$read_all_output(), process$read_all_error())
    )
    results <- lapply(exports, function(x) {
        paths <- Sys.glob(x$results)
        expect_length(paths, 1L)
        jsonlite::read_json(paths[[1L]])$status[[1L]]
    })
    for (result in results) {
        expect_identical(result$data$pid, pid)
        expect_identical(result$data$n, 96L)
        expect_true(result$data$cleaned)
        expect_true(all(unlist(result$data$reported) == 24))
        expect_identical(result$calls$finalize, 1L)
        expect_identical(result$phase, "completed")
        expect_identical(result$data$missing, -1L)
        expect_match(
            result$data$csv,
            "PluginGlobalVariable,R SHARED",
            fixed = TRUE
        )
        expect_match(
            result$data$csv,
            "PluginTrendVariable,R HISTORY",
            fixed = TRUE
        )
        expect_identical(
            result$data$global,
            if (result$data$label == "A") 22L else 24L
        )
        expect_true(all(unlist(result$data$rejected)))
        trend <- unlist(result$data$trend)
        expect_equal(
            trend,
            c(
                average = 24,
                minimum = 24,
                maximum = 24,
                sum = 48,
                direction = 0
            ),
            tolerance = 1e-8
        )
        expect_equal(trend[["sum"]], 2 * trend[["average"]])
        expect_lte(trend[["minimum"]], trend[["average"]])
        expect_gte(trend[["maximum"]], trend[["average"]])
    }
    expect_identical(results[[1L]]$data$label, "A")
    expect_identical(results[[2L]]$data$label, "B")
    expect_true(all(unlist(results[[2L]]$data$seen) == 22))
    expect_false(identical(results[[1L]]$id, results[[2L]]$id))
})

# R's runtime paths differ from the engine's platform-specific library basename.
test_that("executable exports select the matching R runtime library", {
    expect_identical(
        plugin_cli__r_library("H", "B", "windows", "Windows"),
        file.path("B", "R.dll")
    )
    expect_identical(
        plugin_cli__r_library("H", "B", "unix", "Darwin"),
        file.path("H", "lib", "libR.dylib")
    )
    expect_identical(
        plugin_cli__r_library("H", "B", "unix", "Linux"),
        file.path("H", "lib", "libR.so")
    )
})

# A borrowed plugin cannot publish a misleading summary when the destination is a directory.
test_that("executable summary failures keep borrowed state closed", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    ctx <- testapi__fake()
    local_mocked_bindings(eplus_api = function(...) ctx$api)
    plugin <- eplus_plugin(list(finalize = function(api, state, plugin) {
        plugin$data$cleaned <- TRUE
    }))
    definition <- file.path(directory, "definition.rds")
    saveRDS(unserialize(environment(plugin)$payload), definition)
    pointer <- new.env(parent = emptyenv())
    binding <- plugin_cli__bind(
        list(
            definition = definition,
            installation = "fake",
            directory = directory,
            name = "disk"
        ),
        pointer,
        "full"
    )
    dir.create(file.path(directory, "disk-results-full.json"))
    expect_warning(
        expect_error(plugin_cli__finish(binding), "Could not publish"),
        "cannot rename"
    )
    expect_true(binding$entry$closed)
    expect_true(binding$ctx$closed)
    expect_identical(binding$record$calls[["finalize"]], 1L)
    expect_true(binding$record$data$cleaned)
    expect_identical(ctx$deleted, 0L)
})

# Reject native-only hooks before producing an executable artifact, and diagnose missing definitions.
test_that("executable exports reject unavailable hook entrypoints", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    plugin <- eplus_plugin(list(
        end_zone_timestep_after_zone_reporting = function(api, state, plugin) {
            NULL
        }
    ))
    local_mocked_bindings(
        eplus_config = function(...) list(version = "26.1.0", dir = directory),
        plugin_cli__check_bridge = function() NULL,
        plugin_cli__hooks = function() {
            c(begin_new_environment = "on_begin_new_environment")
        }
    )
    expect_error(export_eplus_plugin(plugin, directory), "does not expose")
    path <- file.path(directory, "config.json")
    jsonlite::write_json(
        list(definition = file.path(directory, "missing.rds")),
        path,
        auto_unbox = TRUE
    )
    expect_message(
        expect_identical(
            plugin_cli__dispatch(
                path,
                "begin_new_environment",
                "missing-definition",
                NULL
            ),
            1L
        ),
        "definition is missing"
    )
})

# The isolated export preflight must delegate to the safety gate and propagate child errors.
test_that("executable preflight delegates safety checks and preserves failures", {
    local_mocked_bindings(plugin_cli__bridge_identity = function(...) NULL)
    calls <- 0L
    local_mocked_bindings(api__check_bridge = function() {
        calls <<- calls + 1L
        invisible(NULL)
    })
    local_mocked_bindings(
        r = function(func, libpath, timeout) {
            expect_identical(environment(func), baseenv())
            expect_identical(libpath, .libPaths())
            expect_identical(timeout, 60)
            func()
        },
        .package = "callr"
    )
    expect_null(plugin_cli__check_bridge())
    expect_identical(calls, 1L)
    local_mocked_bindings(api__check_bridge = function() {
        stop("preflight child failed")
    })
    expect_error(plugin_cli__check_bridge(), "preflight child failed")
})

# A dispatcher preparation error must become the first diagnostic and still finalize once.
test_that("executable dispatch preparation failures retain diagnostics and cleanup", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    local_mocked_bindings(
        eplus_api = function(...) testapi__fake()$api,
        plugin__callback = function(...) stop("callback preparation failed")
    )
    plugin <- eplus_plugin(list(
        begin_new_environment = function(api, state, plugin) NULL,
        finalize = function(api, state, plugin) {
            plugin$data$cleaned <- TRUE
        }
    ))
    definition <- file.path(directory, "definition.rds")
    saveRDS(unserialize(environment(plugin)$payload), definition)
    path <- file.path(directory, "config.json")
    jsonlite::write_json(
        list(
            definition = definition,
            installation = "fake",
            directory = directory,
            name = "preparation"
        ),
        path,
        auto_unbox = TRUE
    )
    pointer <- new.env(parent = emptyenv())
    pointer$ready <- TRUE
    pointer$flag <- FALSE
    pointer$stop <- FALSE
    instance <- "preparation-failure"
    on.exit(rm(list = instance, envir = plugin_cli__instances), add = TRUE)
    expect_message(
        expect_identical(
            plugin_cli__dispatch(
                path,
                "begin_new_environment",
                instance,
                pointer
            ),
            1L
        ),
        "callback preparation failed"
    )
    binding <- plugin_cli__instances[[instance]]
    expect_identical(binding$record$error, binding$entry$condition)
    expect_match(
        conditionMessage(binding$record$error),
        "callback preparation failed"
    )
    expect_identical(binding$entry$stop_reason, "callback_error")
    expect_true(pointer$stop)
    expect_true(binding$entry$closed)
    expect_true(binding$record$data$cleaned)
    expect_identical(binding$record$calls[["finalize"]], 1L)
    expect_identical(
        plugin_cli__dispatch(path, "finalize", instance, pointer),
        1L
    )
    expect_identical(binding$record$calls[["finalize"]], 1L)
})

# Exporter proof reuse is process-local and never applies to failed or changed checks.
test_that("executable preflight reuses only successful unchanged build proofs", {
    identity <- list(pid = Sys.getpid(), build = "first")
    cache <- new.env(parent = emptyenv())
    local_mocked_bindings(
        plugin_cli__bridge_cache = cache,
        plugin_cli__bridge_identity = function(...) identity
    )
    calls <- 0L
    result <- TRUE
    local_mocked_bindings(
        r = function(...) {
            calls <<- calls + 1L
            if (inherits(result, "condition")) {
                stop(result)
            }
            result
        },
        .package = "callr"
    )
    expect_null(plugin_cli__check_bridge())
    expect_null(plugin_cli__check_bridge())
    expect_identical(calls, 1L)
    identity$build <- "second"
    expect_null(plugin_cli__check_bridge())
    expect_identical(calls, 2L)
    identity$pid <- identity$pid + 1L
    result <- FALSE
    expect_error(plugin_cli__check_bridge(), "did not complete successfully")
    expect_null(cache$identity)
    result <- simpleError("preflight failed")
    expect_error(plugin_cli__check_bridge(), "preflight failed")
    expect_null(cache$identity)
    result <- TRUE
    expect_null(plugin_cli__check_bridge())
    expect_identical(calls, 5L)
    identity <- NULL
    expect_null(plugin_cli__check_bridge())
    expect_null(plugin_cli__check_bridge())
    expect_identical(calls, 7L)
})

# Identity includes wrapper/native files and resolves the child library without loading namespaces.
test_that("executable bridge identity detects library and installed code changes", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    expect_null(plugin_cli__bridge_identity(directory))
    packages <- file.path(directory, c("eplusr", "rdyncall"))
    for (i in seq_along(packages)) {
        dir.create(packages[i])
        writeLines(
            c(paste0("Package: ", basename(packages[i])), "Version: 1.0"),
            file.path(packages[i], "DESCRIPTION")
        )
        if (i == 1L) {
            expect_silent(expect_null(plugin_cli__bridge_identity(directory)))
        }
        dir.create(file.path(packages[i], "R"))
        writeLines("wrappers", file.path(packages[i], "R", "code.rdb"))
        dir.create(file.path(packages[i], "libs"))
        writeLines("native", file.path(packages[i], "libs", "bridge.so"))
    }
    identity <- plugin_cli__bridge_identity(directory)
    expect_identical(identity$pid, Sys.getpid())
    expect_identical(identity$libpath, directory)
    expect_identical(length(identity$files), 6L)
    expect_identical(plugin_cli__bridge_identity(directory), identity)
    writeLines(
        "modified native bridge",
        file.path(packages[2L], "libs", "bridge.so")
    )
    expect_false(identical(plugin_cli__bridge_identity(directory), identity))
    identity <- plugin_cli__bridge_identity(directory)
    writeLines("modified wrappers", file.path(packages[2L], "R", "code.rdb"))
    expect_false(identical(plugin_cli__bridge_identity(directory), identity))
    expect_false(identical(
        plugin_cli__bridge_identity(c(directory, .Library)),
        identity
    ))
    # A disappearing installation cannot produce a reusable proof.
    local_mocked_bindings(
        find.package = function(...) packages,
        .package = "base"
    )
    unlink(file.path(packages[2L], "DESCRIPTION"))
    expect_null(plugin_cli__bridge_identity(directory))
    local_mocked_bindings(
        find.package = function(...) stop("lookup failed"),
        .package = "base"
    )
    expect_null(plugin_cli__bridge_identity(directory))
})

# Library compatibility does not validate standalone PythonPlugin hosting for older executables.
test_that("stock executable plugin exports reject unreviewed engines before writing files", {
    skip_if_not(identical(Sys.getenv("_EPLUSR_RUN_API_TESTS_"), "true"))
    version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
    skip_if_not(version %in% c("24.2", "25.1", "25.2"))
    plugin <- eplus_plugin(list(initialize = function(api, state, plugin) NULL))
    directory <- tempfile("eplusr-unreviewed-plugin-")
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    expect_error(
        export_eplus_plugin(plugin, directory, eplus = version),
        "executable.*26.1.*26.2"
    )
    expect_false(dir.exists(directory))
})

# Managed histories require the host commit boundary, which the executable adapter lacks.
test_that("executable export rejects managed trends before creating artifacts", {
    plugin <- eplus_plugin(
        list(finalize = function(api, state, plugin) NULL),
        trends = list(
            history = list(
                source = function(api, state, plugin) 1,
                capacity = 3L
            )
        )
    )
    directory <- tempfile("managed-trend-export-")
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    expect_error(
        export_eplus_plugin(plugin, directory),
        "Managed trends require an eplusr host plugin"
    )
    expect_false(dir.exists(directory))
})

# The fixture calls symbols from the installed candidate, never a copied production source.
test_that("native executable boundaries reject foreign threads and restore actual signal handlers", {
    skip_if_not_installed("rdyncall")
    directory <- tempfile("eplusr-plugin-native-")
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
            package <- rdyncall::dynload(
                getLoadedDLLs()[["eplusr"]][["path"]],
                auto.unload = FALSE
            )
            fixture <- rdyncall::dynload(
                file.path(
                    directory,
                    paste0("callback-thread", .Platform$dynlib.ext)
                ),
                auto.unload = FALSE
            )
            bootstrap <- rdyncall::dynsym(package, "eplusr_plugin_bootstrap")
            dispatch <- rdyncall::dynsym(package, "eplusr_plugin_dispatch")
            worker <- rdyncall::dynsym(fixture, "plugin_on_worker")
            handler <- rdyncall::dynsym(fixture, "plugin_signal_handler")
            interrupt <- rdyncall::dynsym(fixture, "plugin_raise_interrupt")
            # Reject missing native addresses before any foreign call can dereference them.
            symbols <- list(
                package = package,
                fixture = fixture,
                bootstrap = bootstrap,
                dispatch = dispatch,
                worker = worker,
                handler = handler,
                interrupt = interrupt
            )
            missing <- vapply(
                symbols,
                function(pointer) {
                    is.null(pointer) || rdyncall::is.nullptr(pointer)
                },
                logical(1L)
            )
            if (any(missing)) {
                stop(
                    "Missing native fixture symbols: ",
                    paste(names(symbols)[missing], collapse = ", ")
                )
            }
            # Bootstrap on the owning R thread before any foreign call claims ownership.
            message("native stage: owner bootstrap")
            initial <- rdyncall::dyncall(bootstrap, ")i")
            rdyncall::dyncall(handler, "i)i", 1L)
            message("native stage: foreign entrypoints")
            foreign <- rdyncall::dyncall(worker, "pp)i", bootstrap, dispatch)
            foreign_handler <- rdyncall::dyncall(handler, "i)i", 0L)
            after_foreign <- rdyncall::dyncall(bootstrap, ")i")
            failures <- lapply(c("signal", "error"), function(mode) {
                library <- if (mode == "signal") {
                    function(...) {
                        rdyncall::dyncall(interrupt, ")v")
                        # Only R's own evaluation stack may raise the pending interrupt.
                        Sys.sleep(0)
                    }
                } else {
                    function(...) stop("native bootstrap sentinel")
                }
                message("native stage: ", mode, " bootstrap")
                assign("library", library, envir = .GlobalEnv)
                on.exit(rm("library", envir = .GlobalEnv), add = TRUE)
                rdyncall::dyncall(handler, "i)i", 1L)
                status <- rdyncall::dyncall(bootstrap, ")i")
                message("native stage: ", mode, " returned ", status)
                restored <- rdyncall::dyncall(handler, "i)i", 0L)
                rm("library", envir = .GlobalEnv)
                on.exit(NULL, add = FALSE)
                c(
                    status = status,
                    restored = restored,
                    recovered = rdyncall::dyncall(bootstrap, ")i")
                )
            })
            list(
                initial = initial,
                foreign = foreign,
                handler = foreign_handler,
                after_foreign = after_foreign,
                failures = failures,
                package = getLoadedDLLs()[["eplusr"]][["path"]]
            )
        },
        args = list(directory),
        libpath = .libPaths(),
        timeout = 30,
        show = TRUE
    )
    expect_identical(result$initial, 0L)
    expect_identical(result$foreign, 11L)
    expect_identical(result$handler, 1L)
    expect_identical(result$after_foreign, 0L)
    expect_identical(
        result$failures,
        rep(list(c(status = 1L, restored = 1L, recovered = 0L)), 2L)
    )
    expect_identical(
        normalizePath(result$package),
        normalizePath(getLoadedDLLs()[["eplusr"]][["path"]])
    )
})
