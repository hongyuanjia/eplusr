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
    local_mocked_bindings(eplus_config = function(...) list(version = "26.2.0"))
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
                if (plugin$data$n %% 24L == 0L) gc()
            },
            finalize = function(api, state, plugin) {
                plugin$data$cleaned <- TRUE
            }
        ))
    }
    exports <- list(
        export_eplus_plugin(make("A", 22), directory, "PluginA"),
        export_eplus_plugin(
            make("B", 24),
            directory,
            "PluginB",
            eplus = eplus_config("26.1")$dir
        )
    )
    expect_error(
        export_eplus_plugin(make("A", 22), directory, "PluginA"),
        "already exist"
    )
    expect_error(
        export_eplus_plugin(make("A", 22), paste0(directory, ",bad")),
        "delimiter"
    )
    cat(
        paste(
            c(
                exports[[1L]]$idf[[1L]],
                exports[[1L]]$idf[[2L]],
                exports[[2L]]$idf[[2L]]
            ),
            collapse = "\n"
        ),
        file = fixture$model,
        append = TRUE
    )
    process <- processx::process$new(
        file.path(eplus_config("26.1")$dir, eplus_config("26.1")$exe),
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
