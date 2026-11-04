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

# Hooks share their lexical bindings within one plugin, while every binding gets a fresh copy.
test_that("plugin freezing preserves shared and inherited lexical state", {
    hooks <- local({
        count <- 0L
        increment <- function() count <<- count + 1L
        list(
            initialize = function(api, state, plugin) increment(),
            begin_new_environment = local({
                update <- increment
                function(api, state, plugin) update()
            }),
            finalize = function(api, state, plugin) plugin$data$count <- count
        )
    })
    factory <- eplus_plugin(hooks)
    # Mutating the source after factory creation must not change its frozen initial state.
    hooks$initialize(NULL, NULL, NULL)
    ctx <- testapi__fake()
    api <- ctx$api
    on.exit(try(api$close(), silent = TRUE), add = TRUE)
    for (index in seq_len(2L)) {
        state <- api$state_manager$new_state()
        factory(api, state)
        api$runtime$run_energyplus(state, testapi__model())
        expect_identical(
            api$state_manager$status(state)$plugins[[1L]]$data$count,
            4L
        )
        api$state_manager$delete_state(state)
    }
    expect_identical(environment(hooks$finalize)$count, 1L)
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

# A public stock run supplies deterministic hook values without PythonPlugin objects.
test_that("host plugin trends commit after hooks and retain bounded valid history", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    api <- eplus_api(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"))
    on.exit(try(api$close(), silent = TRUE), add = TRUE)
    state <- api$state_manager$new_state()
    plugin <- eplus_plugin(
        list(
            initialize = function(api, state, plugin) {
                plugin$data$n <- 0L
                plugin$handles$trend <- api$exchange$get_trend_handle(
                    state,
                    "sequence"
                )
                plugin$data$empty <- tryCatch(
                    api$exchange$get_trend_value(
                        state,
                        plugin$handles$trend,
                        1L
                    ),
                    error = conditionMessage
                )
            },
            end_zone_timestep_after_zone_reporting = function(
                api,
                state,
                plugin
            ) {
                if (api$exchange$kind_of_sim(state) != 3L) {
                    return(NULL)
                }
                plugin$data$n <- plugin$data$n + 1L
                if (plugin$data$n > 1L) {
                    plugin$data$previous <- api$exchange$get_trend_value(
                        state,
                        plugin$handles$trend,
                        1L
                    )
                }
                gc()
                if (plugin$data$n == 5L) api$runtime$stop_simulation(state)
            },
            finalize = function(api, state, plugin) {
                plugin$data$values <- vapply(
                    1:3,
                    function(index) {
                        api$exchange$get_trend_value(
                            state,
                            plugin$handles$trend,
                            index
                        )
                    },
                    numeric(1L)
                )
                plugin$data$range <- tryCatch(
                    api$exchange$get_trend_value(
                        state,
                        plugin$handles$trend,
                        4L
                    ),
                    error = conditionMessage
                )
            }
        ),
        trends = list(
            sequence = list(
                source = function(api, state, plugin) plugin$data$n,
                capacity = 3L
            )
        )
    )
    plugin(api, state)
    # A second binding still receives every formal hook without acquiring or duplicating trend samples.
    eplus_plugin(
        list(end_zone_timestep_after_zone_reporting = function(
            api,
            state,
            plugin
        ) {
            if (api$exchange$kind_of_sim(state) == 3L) {
                plugin$data$n <- plugin$data$n + 1L
            }
        }),
        data = list(n = 0L)
    )(api, state)
    expect_identical(
        api$state_manager$status(state)$plugins[[
            1L
        ]]$trends$sequence$valid_count,
        0L
    )
    api$runtime$run_energyplus(
        state,
        c("-w", fixture$weather, "-d", fixture$directory, fixture$model)
    )
    result <- api$state_manager$status(state)$plugins[[1L]]
    expect_match(result$data$empty, "valid history")
    expect_identical(result$data$previous, 4)
    expect_equal(result$data$values, c(5, 4, 3))
    expect_match(result$data$range, "valid history")
    expect_identical(result$trends$sequence$capacity, 3L)
    expect_identical(result$trends$sequence$valid_count, 3L)
    expect_identical(result$trends$sequence$total_samples, 5L)
    expect_identical(result$trends$sequence$latest$warmup, FALSE)
    bindings <- api$state_manager$status(state)$plugins
    expect_length(bindings, 2L)
    expect_identical(bindings[[2L]]$data$n, 5L)
    expect_length(bindings[[2L]]$trends, 0L)
})

# State inspection and managed ownership remain available without entering native trend arrays.
test_that("managed trend handles reject retired cross-state and ambiguous ownership", {
    withr::local_options(list(
        eplusr.test.trend.handle = NULL,
        eplusr.test.trend.other_state = NULL
    ))
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    api <- eplus_api(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"))
    on.exit(try(api$close(), silent = TRUE), add = TRUE)
    state <- api$state_manager$new_state()
    factory <- eplus_plugin(
        list(
            initialize = function(api, state, plugin) {
                if (plugin$id == "1") {
                    # Names cannot escape the active binding even when another state declares that name.
                    plugin$data$other_state <- tryCatch(
                        api$exchange$get_trend_handle(
                            getOption("eplusr.test.trend.other_state"),
                            "n"
                        ),
                        error = conditionMessage
                    )
                    options(
                        eplusr.test.trend.handle = api$exchange$get_trend_handle(
                            state,
                            "n"
                        )
                    )
                } else {
                    for (operation in c(
                        "value",
                        "average",
                        "min",
                        "max",
                        "sum",
                        "direction"
                    )) {
                        message <- tryCatch(
                            api$exchange[[paste0("get_trend_", operation)]](
                                state,
                                getOption("eplusr.test.trend.handle"),
                                1L
                            ),
                            error = conditionMessage
                        )
                        if (
                            !is.character(message) ||
                                !grepl("plugin binding", message)
                        ) {
                            stop(
                                "A managed trend crossed plugin binding ownership."
                            )
                        }
                    }
                }
            },
            begin_new_environment = function(api, state, plugin) {
                plugin$data$initial <- tryCatch(
                    api$exchange$get_trend_value(
                        state,
                        api$exchange$get_trend_handle(state, "n"),
                        1L
                    ),
                    error = conditionMessage
                )
            }
        ),
        trends = list(
            n = list(source = function(api, state, plugin) 1, capacity = 1L)
        )
    )
    other <- api$state_manager$new_state()
    # Register the same name on an independent owned state without importing live objects into hooks.
    eplus_plugin(
        list(initialize = function(api, state, plugin) invisible(NULL)),
        trends = list(
            n = list(
                # This independent source is never run; registration alone establishes its ownership.
                source = function(api, state, plugin) 1,
                capacity = 1L
            )
        )
    )(api, other)
    options(eplusr.test.trend.other_state = other)
    factory(api, state)
    handle <- api$exchange$get_trend_handle(state, "n")
    expect_error(api$exchange$get_trend_handle(state, "missing"), "not found")
    # Transporting the whole ownership graph still cannot revive serialized native library pointers.
    transported <- unserialize(serialize(
        list(api = api, state = state, handle = handle),
        NULL
    ))
    expect_error(
        transported$api$exchange$get_trend_value(
            transported$state,
            transported$handle,
            1L
        ),
        "API is closed"
    )
    for (operation in c("value", "average", "min", "max", "sum", "direction")) {
        expect_error(
            api$exchange[[paste0("get_trend_", operation)]](other, handle, 1L),
            "another state"
        )
    }
    factory(api, state)
    expect_error(api$exchange$get_trend_handle(state, "n"), "ambiguous")
    api$state_manager$reset_state(state)
    for (operation in c("value", "average", "min", "max", "sum", "direction")) {
        expect_error(
            api$exchange[[paste0("get_trend_", operation)]](state, handle, 1L),
            "retired"
        )
    }
    factory(api, state)
    api$runtime$run_energyplus(
        state,
        c("-w", fixture$weather, "-d", fixture$directory, fixture$model)
    )
    result <- api$state_manager$status(state)$plugins[[1L]]
    expect_match(result$data$initial, "valid history")
    expect_match(result$data$other_state, "another state or plugin binding")
    expect_identical(result$trends$n$capacity, 1L)
    expect_identical(result$trends$n$valid_count, 1L)
    expect_identical(result$trends$n$total_samples, 96L)
    for (operation in c("value", "average", "min", "max", "sum", "direction")) {
        expect_error(
            api$exchange[[paste0("get_trend_", operation)]](state, handle, 1L),
            "retired"
        )
    }
})

# Warmup samples are optional and must disappear before formal-weather hooks observe history.
test_that("managed trend warmup history clears before formal environment hooks", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    api <- eplus_api(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"))
    on.exit(try(api$close(), silent = TRUE), add = TRUE)
    state <- api$state_manager$new_state()
    factory <- eplus_plugin(
        list(
            after_new_environment_warmup_complete = function(
                api,
                state,
                plugin
            ) {
                if (api$exchange$kind_of_sim(state) != 3L) {
                    return(NULL)
                }
                plugin$data$empty <- tryCatch(
                    api$exchange$get_trend_value(
                        state,
                        api$exchange$get_trend_handle(state, "warm"),
                        1L
                    ),
                    error = conditionMessage
                )
            },
            end_zone_timestep_after_zone_reporting = function(
                api,
                state,
                plugin
            ) {
                if (
                    api$exchange$kind_of_sim(state) == 3L &&
                        !api$exchange$warmup_flag(state)
                ) {
                    api$runtime$stop_simulation(state)
                }
            }
        ),
        trends = list(
            warm = list(
                source = function(api, state, plugin) 7,
                capacity = 2L,
                warmup = TRUE
            )
        )
    )
    factory(api, state)
    api$runtime$run_energyplus(
        state,
        c("-w", fixture$weather, "-d", fixture$directory, fixture$model)
    )
    result <- api$state_manager$status(state)$plugins[[1L]]
    expect_match(result$data$empty, "valid history")
    expect_identical(result$trends$warm$total_samples, 1L)
    expect_identical(result$trends$warm$valid_count, 1L)
    expect_false(result$trends$warm$latest$warmup)
})

# Real repeated warmup dates must retain chronological trends and reset at both phase boundaries.
test_that("stock warmup trends retain elapsed hours across repeated days and environments", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    model <- read_idf(fixture$model)
    model$add(
        RunPeriod = list(
            name = "Second weather environment",
            begin_month = 1,
            begin_day_of_month = 2,
            end_month = 1,
            end_day_of_month = 2
        )
    )
    model$save(fixture$model, overwrite = TRUE)
    plugin <- eplus_plugin(
        list(
            # Retain ordinary evidence separately for each independently reset weather environment.
            begin_new_environment = function(api, state, plugin) {
                if (api$exchange$kind_of_sim(state) != 3L) {
                    return(NULL)
                }
                plugin$data$n <- 0L
                plugin$data$weather[[length(plugin$data$weather) + 1L]] <- list(
                    initial_count = api$state_manager$status(state)$plugins[[
                        1L
                    ]]$trends$warm$valid_count,
                    midnight = list()
                )
            },
            # Formal weather starts a new history and an independent elapsed coordinate.
            after_new_environment_warmup_complete = function(
                api,
                state,
                plugin
            ) {
                if (api$exchange$kind_of_sim(state) != 3L) {
                    return(NULL)
                }
                index <- length(plugin$data$weather)
                plugin$data$weather[[
                    index
                ]]$formal_count <- api$state_manager$status(state)$plugins[[
                    1L
                ]]$trends$warm$valid_count
                plugin$data$n <- 0L
            },
            # Reads precede the current hook's commit, so hook 98 sees values 97 and 96.
            end_zone_timestep_after_zone_reporting = function(
                api,
                state,
                plugin
            ) {
                if (api$exchange$kind_of_sim(state) != 3L) {
                    return(NULL)
                }
                plugin$data$n <- plugin$data$n + 1L
                warmup <- api$exchange$warmup_flag(state)
                if (
                    (warmup && plugin$data$n %in% c(98L, 194L)) ||
                        (!warmup && plugin$data$n == 3L)
                ) {
                    handle <- api$exchange$get_trend_handle(state, "warm")
                    evidence <- list(
                        values = c(
                            api$exchange$get_trend_value(state, handle, 1L),
                            api$exchange$get_trend_value(state, handle, 2L)
                        ),
                        slope = api$exchange$get_trend_direction(
                            state,
                            handle,
                            2L
                        ),
                        latest = api$state_manager$status(state)$plugins[[
                            1L
                        ]]$trends$warm$latest
                    )
                    index <- length(plugin$data$weather)
                    if (warmup) {
                        checks <- plugin$data$weather[[index]]$midnight
                        plugin$data$weather[[index]]$midnight[[
                            length(checks) + 1L
                        ]] <- evidence
                    } else {
                        plugin$data$weather[[index]]$formal <- evidence
                    }
                }
            }
        ),
        data = list(n = 0L, weather = list()),
        warmup = TRUE,
        trends = list(
            warm = list(
                # Increasing once per quarter-hour gives a literal four value units per hour.
                source = function(api, state, plugin) plugin$data$n,
                capacity = 2L,
                warmup = TRUE
            )
        )
    )
    result <- energyplus(
        fixture$model,
        fixture$weather,
        eplus = Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"),
        backend = plugin,
        output_dir = file.path(fixture$directory, "warmup"),
        readvars = FALSE,
        echo = FALSE
    )
    expect_null(result$api$error)
    weather <- result$api$plugins[[1L]]$data$weather
    expect_length(weather, 2L)
    for (environment in weather) {
        expect_identical(environment$initial_count, 0L)
        expect_identical(environment$formal_count, 0L)
        expect_length(environment$midnight, 2L)
        for (index in seq_along(environment$midnight)) {
            checkpoint <- environment$midnight[[index]]
            expect_equal(checkpoint$values, c(97, 96) + 96 * (index - 1L))
            expect_equal(checkpoint$slope, 4)
            expect_equal(checkpoint$latest$time, c(24.25, 48.25)[[index]])
            expect_true(checkpoint$latest$warmup)
        }
        expect_equal(environment$formal$values, c(2, 1))
        expect_equal(environment$formal$slope, 4)
        expect_equal(environment$formal$latest$time, 0.5)
        expect_false(environment$formal$latest$warmup)
    }
    expect_false(identical(
        weather[[1L]]$formal$latest$environment,
        weather[[2L]]$formal$latest$environment
    ))
})

# Source conditions cross the native boundary only after finalization and state cleanup.
test_that("managed trend source failures preserve conditions and finalize once", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    for (failure in c("nonfinite", "complex", "error", "interrupt")) {
        api <- eplus_api(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"))
        state <- api$state_manager$new_state()
        factory <- eplus_plugin(
            list(finalize = function(api, state, plugin) {
                plugin$data$finalized <- 1L
                stop("later cleanup failure")
            }),
            trends = list(
                n = list(
                    source = function(api, state, plugin) {
                        if (failure == "complex") {
                            return(1 + 1i)
                        }
                        if (failure == "nonfinite") {
                            return(Inf)
                        }
                        stop(structure(
                            list(
                                message = "original trend failure",
                                call = NULL,
                                marker = 42L
                            ),
                            class = c(
                                if (failure == "interrupt") {
                                    "interrupt"
                                } else {
                                    "error"
                                },
                                "condition"
                            )
                        ))
                    },
                    capacity = 1L
                )
            )
        )
        factory(api, state)
        condition <- tryCatch(
            api$runtime$run_energyplus(
                state,
                c("-w", fixture$weather, "-d", fixture$directory, fixture$model)
            ),
            error = identity,
            interrupt = identity
        )
        if (failure %in% c("nonfinite", "complex")) {
            expect_match(conditionMessage(condition), "finite numeric")
        } else {
            expect_identical(condition$message, "original trend failure")
            expect_identical(condition$marker, 42L)
            expect_s3_class(
                condition,
                if (failure == "interrupt") "interrupt" else "error"
            )
        }
        result <- api$state_manager$status(state)$plugins[[1L]]
        expect_identical(result$data$finalized, 1L)
        expect_identical(result$calls[["finalize"]], 1L)
        expect_identical(result$trends$n$valid_count, 0L)
        expect_identical(result$phase, "failed")
        api$close()
    }
})

# Multiple factories retain their own histories and can source real declared engine outputs.
test_that("managed trend bindings isolate sources and retain engine output semantics", {
    fixture <- testapi__controlled_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    api <- eplus_api(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"))
    on.exit(try(api$close(), silent = TRUE), add = TRUE)
    state <- api$state_manager$new_state()
    make <- function(offset) {
        eplus_plugin(
            list(
                initialize = function(api, state, plugin) {
                    plugin$data$n <- offset
                    api$exchange$request_variable(
                        state,
                        "Zone Mean Air Temperature",
                        "ZONE ONE"
                    )
                },
                end_zone_timestep_after_zone_reporting = function(
                    api,
                    state,
                    plugin
                ) {
                    if (api$exchange$kind_of_sim(state) != 3L) {
                        return(NULL)
                    }
                    plugin$data$n <- plugin$data$n + 1L
                    if (plugin$data$n == offset + 5L) {
                        api$runtime$stop_simulation(state)
                    }
                },
                finalize = function(api, state, plugin) {
                    plugin$data$n <- api$exchange$get_trend_value(
                        state,
                        api$exchange$get_trend_handle(state, "n"),
                        1L
                    )
                    plugin$data$temperature <- api$exchange$get_trend_value(
                        state,
                        api$exchange$get_trend_handle(state, "temperature"),
                        1L
                    )
                }
            ),
            trends = list(
                n = list(
                    source = function(api, state, plugin) plugin$data$n,
                    capacity = 1L
                ),
                temperature = list(
                    source = function(api, state, plugin) {
                        if (is.null(plugin$handles$temperature)) {
                            plugin$handles$temperature <- api$exchange$get_variable_handle(
                                state,
                                "Zone Mean Air Temperature",
                                "ZONE ONE"
                            )
                        }
                        api$exchange$get_variable_value(
                            state,
                            plugin$handles$temperature
                        )
                    },
                    capacity = 2L
                )
            )
        )
    }
    make(0L)(api, state)
    make(100L)(api, state)
    api$runtime$clear_callbacks()
    api$runtime$run_energyplus(
        state,
        c("-w", fixture$weather, "-d", fixture$directory, fixture$model)
    )
    results <- api$state_manager$status(state)$plugins
    expect_equal(vapply(results, function(x) x$data$n, numeric(1L)), c(5, 105))
    expect_identical(results[[1L]]$trends$n$total_samples, 5L)
    expect_identical(results[[2L]]$trends$n$total_samples, 5L)
    expect_true(is.finite(results[[1L]]$data$temperature))
    expect_equal(results[[1L]]$data$temperature, results[[2L]]$data$temperature)
    expect_identical(
        results[[1L]]$trends$temperature$latest$callback,
        "end_zone_timestep_after_zone_reporting"
    )
})

# New environments clear history before plugin lifecycle hooks; invalid indices never expose slots.
test_that("managed trends reset before new environments and validate every history index", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    model <- read_idf(fixture$model)
    model$add(
        RunPeriod = list(
            name = "SECOND",
            begin_month = 1,
            begin_day_of_month = 2,
            end_month = 1,
            end_day_of_month = 2
        )
    )
    model$save(fixture$model, overwrite = TRUE)
    api <- eplus_api(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"))
    on.exit(try(api$close(), silent = TRUE), add = TRUE)
    state <- api$state_manager$new_state()
    eplus_plugin(
        list(
            initialize = function(api, state, plugin) {
                plugin$data$environments <- 0L
                plugin$data$empty <- character()
            },
            begin_new_environment = function(api, state, plugin) {
                plugin$data$environments <- plugin$data$environments + 1L
                plugin$data$empty <- c(
                    plugin$data$empty,
                    tryCatch(
                        api$exchange$get_trend_value(
                            state,
                            api$exchange$get_trend_handle(state, "one"),
                            1L
                        ),
                        error = conditionMessage
                    )
                )
            },
            finalize = function(api, state, plugin) {
                handle <- api$exchange$get_trend_handle(state, "one")
                plugin$data$invalid <- vapply(
                    list(
                        0L,
                        -1L,
                        1.5,
                        NA_real_,
                        Inf,
                        1 + 1i,
                        "1",
                        numeric(),
                        2L
                    ),
                    function(index) {
                        tryCatch(
                            api$exchange$get_trend_value(state, handle, index),
                            error = conditionMessage
                        )
                    },
                    character(1L)
                )
                plugin$data$newest <- api$exchange$get_trend_value(
                    state,
                    handle,
                    1L
                )
            }
        ),
        trends = list(
            one = list(source = function(api, state, plugin) 7, capacity = 1L)
        )
    )(api, state)
    api$runtime$run_energyplus(
        state,
        c("-w", fixture$weather, "-d", fixture$directory, fixture$model)
    )
    result <- api$state_manager$status(state)$plugins[[1L]]
    expect_gte(result$data$environments, 2L)
    expect_true(all(grepl("valid history", result$data$empty)))
    expect_true(all(grepl("valid history", result$data$invalid)))
    expect_identical(result$data$newest, 7)
    expect_identical(result$trends$one$total_samples, 96L)
})

# Declaration validation happens before workers/native calls and capability rows distinguish managed use.
test_that("host plugin managed trend declarations and capability metadata are explicit", {
    hook <- list(finalize = function(api, state, plugin) NULL)
    source <- function(api, state, plugin) 1
    expect_error(eplus_plugin(hook, trends = list(n = list(source = source))))
    expect_error(eplus_plugin(
        hook,
        trends = list(n = list(source = source, capacity = 0L))
    ))
    expect_error(eplus_plugin(
        hook,
        trends = list(n = list(source = source, capacity = Inf))
    ))
    expect_error(eplus_plugin(
        hook,
        trends = list(n = list(source = 1, capacity = 1L))
    ))
    expect_error(eplus_plugin(
        hook,
        trends = list(n = list(source = source, capacity = 1L, warmup = NA))
    ))
    expect_error(eplus_plugin(
        list(initialize = function(api, state, plugin) NULL),
        trends = list(n = list(source = source, capacity = 1L, replay = NA))
    ))
    expect_error(eplus_plugin(
        hook,
        trends = list(n = list(source = source, capacity = 1L, extra = TRUE))
    ))
    expect_error(eplus_plugin(
        hook,
        trends = list(
            n = list(source = source, capacity = 1L),
            n = list(source = source, capacity = 1L)
        )
    ))
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    api <- eplus_api(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"))
    on.exit(api$close(), add = TRUE)
    methods <- api$methods()
    expect_identical(
        methods$managed_plugin_trend[
            methods$method %in%
                c(
                    "get_trend_handle",
                    "get_trend_value",
                    "get_trend_average",
                    "get_trend_min",
                    "get_trend_max",
                    "get_trend_sum",
                    "get_trend_direction"
                )
        ],
        rep(TRUE, 7L)
    )
    expect_identical(
        methods$status[methods$method == "get_trend_value"],
        "unsupported"
    )
})

# Owned early lifecycles provide elapsed hours independently of the EPW year and calendar getter.
test_that("stock early trends keep a chronological clock across New Year", {
    version <- Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1")
    skip_if_not(version %in% c("9.5", "9.6"))
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
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
    api <- eplus_api(version)
    on.exit(try(api$close(), silent = TRUE), add = TRUE)
    state <- api$state_manager$new_state()
    plugin <- eplus_plugin(
        list(
            end_zone_timestep_after_zone_reporting = function(
                api,
                state,
                plugin
            ) {
                if (api$exchange$kind_of_sim(state) != 3L) {
                    return(NULL)
                }
                time <- api$exchange$current_sim_time(state)
                plugin$data$times <- c(plugin$data$times, time)
            },
            finalize = function(api, state, plugin) {
                handle <- api$exchange$get_trend_handle(state, "clock")
                plugin$data$last <- api$exchange$get_trend_value(
                    state,
                    handle,
                    1L
                )
            }
        ),
        data = list(times = numeric()),
        trends = list(
            clock = list(
                source = function(api, state, plugin) {
                    api$exchange$current_sim_time(state)
                },
                capacity = 2L
            )
        )
    )
    plugin(api, state)
    # Clearing user callbacks must preserve the host's clock and history lifecycle hooks.
    api$runtime$clear_callbacks()
    # Rebind user hooks after the deliberate clear, preserving one independent active history.
    plugin(api, state)
    api$runtime$run_energyplus(
        state,
        c("-w", fixture$weather, "-d", fixture$directory, fixture$model)
    )
    result <- tail(api$state_manager$status(state)$plugins, 1L)[[1L]]
    expect_length(result$data$times, 192L)
    expect_equal(
        result$data$times[c(1L, 96L, 97L, 192L)],
        c(0.25, 24, 24.25, 48)
    )
    expect_true(all(diff(result$data$times) > 0))
    expect_equal(result$data$last, 48)
    expect_identical(result$trends$clock$total_samples, 192L)
    expect_equal(result$trends$clock$latest$time, 48)
})

# Known hook values give independently worked aggregates through the public host plugin API.
test_that("managed trend aggregates support one sample and newest wrapped windows", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    api <- eplus_api(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"))
    on.exit(try(api$close(), silent = TRUE), add = TRUE)
    state <- api$state_manager$new_state()
    plugin <- eplus_plugin(
        list(
            initialize = function(api, state, plugin) {
                plugin$data$n <- 0L
                plugin$handles$trend <- api$exchange$get_trend_handle(
                    state,
                    "sequence"
                )
            },
            end_zone_timestep_after_zone_reporting = function(
                api,
                state,
                plugin
            ) {
                if (api$exchange$kind_of_sim(state) != 3L) {
                    return(NULL)
                }
                plugin$data$n <- plugin$data$n + 1L
                if (plugin$data$n == 2L) {
                    plugin$data$single <- vapply(
                        c("average", "min", "max", "sum"),
                        function(operation) {
                            api$exchange[[paste0("get_trend_", operation)]](
                                state,
                                plugin$handles$trend,
                                1L
                            )
                        },
                        numeric(1L)
                    )
                }
                if (plugin$data$n == 5L) api$runtime$stop_simulation(state)
            },
            finalize = function(api, state, plugin) {
                # The capacity-three history has wrapped from 1:5 to newest-first 5,4,3.
                plugin$data$windows <- lapply(c(2L, 3L), function(count) {
                    vapply(
                        c("average", "min", "max", "sum"),
                        function(operation) {
                            api$exchange[[paste0("get_trend_", operation)]](
                                state,
                                plugin$handles$trend,
                                count
                            )
                        },
                        numeric(1L)
                    )
                })
            }
        ),
        trends = list(
            sequence = list(
                source = function(api, state, plugin) plugin$data$n,
                capacity = 3L
            )
        )
    )
    plugin(api, state)
    api$runtime$run_energyplus(
        state,
        c("-w", fixture$weather, "-d", fixture$directory, fixture$model)
    )
    result <- api$state_manager$status(state)$plugins[[1L]]
    expect_equal(unname(result$data$single), c(1, 1, 1, 1))
    expect_equal(unname(result$data$windows[[1L]]), c(4.5, 4, 5, 9))
    expect_equal(unname(result$data$windows[[2L]]), c(4, 3, 5, 12))
})

# A quarter-hour zone clock makes chronological slope units observable instead of index-based.
test_that("managed trend direction uses elapsed hours and independent wrapped histories", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    api <- eplus_api(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"))
    on.exit(try(api$close(), silent = TRUE), add = TRUE)
    state <- api$state_manager$new_state()
    for (sequence in list(
        2 * (1:5) + 1,
        20 - 3 * (1:5),
        rep(7, 5),
        c(2, 3, 10, 9, 20)
    )) {
        eplus_plugin(
            list(
                initialize = function(api, state, plugin) {
                    plugin$data$n <- 0L
                },
                end_zone_timestep_after_zone_reporting = function(
                    api,
                    state,
                    plugin
                ) {
                    if (api$exchange$kind_of_sim(state) != 3L) {
                        return(NULL)
                    }
                    plugin$data$n <- plugin$data$n + 1L
                    if (plugin$data$n == 5L && plugin$id == "4") {
                        api$runtime$stop_simulation(state)
                    }
                },
                finalize = function(api, state, plugin) {
                    handle <- api$exchange$get_trend_handle(state, "sequence")
                    plugin$data$slopes <- vapply(
                        2:4,
                        function(count) {
                            api$exchange$get_trend_direction(
                                state,
                                handle,
                                count
                            )
                        },
                        numeric(1L)
                    )
                }
            ),
            data = list(sequence = sequence),
            trends = list(
                sequence = list(
                    source = function(api, state, plugin) {
                        plugin$data$sequence[[plugin$data$n]]
                    },
                    capacity = 4L
                )
            )
        )(api, state)
    }
    api$runtime$run_energyplus(
        state,
        c("-w", fixture$weather, "-d", fixture$directory, fixture$model)
    )
    results <- api$state_manager$status(state)$plugins
    expect_equal(results[[1L]]$data$slopes, c(8, 8, 8))
    expect_equal(results[[2L]]$data$slopes, c(-12, -12, -12))
    expect_equal(results[[3L]]$data$slopes, c(0, 0, 0))
    # Four-point least squares is 20 value units/hour; the endpoint slope is 22 2/3.
    expect_equal(results[[4L]]$data$slopes, c(44, 20, 20))
    expect_true(all(vapply(
        results,
        function(result) result$trends$sequence$valid_count == 4L,
        logical(1L)
    )))
})

# Aggregate validation uses valid samples, while a capacity-one buffer remains useful.
test_that("managed trend aggregate counts reject invalid and insufficient history", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    api <- eplus_api(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"))
    on.exit(try(api$close(), silent = TRUE), add = TRUE)
    state <- api$state_manager$new_state()
    eplus_plugin(
        list(
            initialize = function(api, state, plugin) {
                plugin$data$n <- 0L
                handle <- api$exchange$get_trend_handle(state, "sequence")
                plugin$data$empty <- vapply(
                    c("average", "min", "max", "sum", "direction"),
                    function(operation) {
                        tryCatch(
                            api$exchange[[paste0("get_trend_", operation)]](
                                state,
                                handle,
                                1L
                            ),
                            error = conditionMessage
                        )
                    },
                    character(1L)
                )
            },
            end_zone_timestep_after_zone_reporting = function(
                api,
                state,
                plugin
            ) {
                if (api$exchange$kind_of_sim(state) != 3L) {
                    return(NULL)
                }
                plugin$data$n <- plugin$data$n + 1L
                if (plugin$data$n == 5L) api$runtime$stop_simulation(state)
            },
            finalize = function(api, state, plugin) {
                handle <- api$exchange$get_trend_handle(state, "sequence")
                plugin$data$invalid <- lapply(
                    c("average", "min", "max", "sum", "direction"),
                    function(operation) {
                        vapply(
                            list(
                                0L,
                                -1L,
                                1.5,
                                NA_real_,
                                Inf,
                                1 + 1i,
                                "1",
                                numeric(),
                                c(1L, 2L),
                                4L
                            ),
                            function(count) {
                                tryCatch(
                                    api$exchange[[paste0(
                                        "get_trend_",
                                        operation
                                    )]](state, handle, count),
                                    error = conditionMessage
                                )
                            },
                            character(1L)
                        )
                    }
                )
                single <- api$exchange$get_trend_handle(state, "single")
                plugin$data$single <- vapply(
                    c("average", "min", "max", "sum"),
                    function(operation) {
                        api$exchange[[paste0("get_trend_", operation)]](
                            state,
                            single,
                            1L
                        )
                    },
                    numeric(1L)
                )
                plugin$data$direction_one <- tryCatch(
                    api$exchange$get_trend_direction(state, single, 1L),
                    error = conditionMessage
                )
            }
        ),
        trends = list(
            sequence = list(
                source = function(api, state, plugin) plugin$data$n,
                capacity = 3L
            ),
            single = list(
                source = function(api, state, plugin) plugin$data$n,
                capacity = 1L
            )
        )
    )(api, state)
    api$runtime$run_energyplus(
        state,
        c("-w", fixture$weather, "-d", fixture$directory, fixture$model)
    )
    result <- api$state_manager$status(state)$plugins[[1L]]
    expect_true(all(grepl("valid history", result$data$empty)))
    expect_true(all(grepl(
        "Trend count.*valid history",
        unlist(result$data$invalid)
    )))
    expect_equal(unname(result$data$single), c(5, 5, 5, 5))
    expect_match(result$data$direction_one, "at least two")
})

# A new close-time interrupt remains observable after all owned states are retired.
test_that("stock explicit close preserves fresh plugin cleanup interrupts", {
    fixture <- testrun__api_fixture()
    on.exit(unlink(fixture$directory, recursive = TRUE), add = TRUE)
    api <- eplus_api(Sys.getenv("_EPLUSR_TEST_API_VERSION_", "26.1"))
    on.exit(try(api$close(), silent = TRUE), add = TRUE)
    state <- api$state_manager$new_state()
    plugin <- eplus_plugin(list(finalize = function(api, state, plugin) {
        stop(structure(
            list(message = "fresh close interrupt", call = NULL, marker = 42L),
            class = c("interrupt", "condition")
        ))
    }))
    plugin(api, state)
    condition <- tryCatch(api$close(), interrupt = identity)
    expect_s3_class(condition, "interrupt")
    expect_identical(condition$message, "fresh close interrupt")
    expect_identical(condition$marker, 42L)
    expect_identical(api$state_manager$status(state)$phase, "closed")
    expect_null(api$close())
})
