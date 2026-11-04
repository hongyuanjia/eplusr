#' Define an R plugin hosted by eplusr
#'
#' Create a reusable initialization function for `backend`. The plugin runs in
#' the same R worker and thread as the EnergyPlus 26.1 library. This is an eplusr
#' host plugin. Use [export_eplus_plugin()] to run the same hooks in the
#' EnergyPlus executable through its plugin loader.
#'
#' @param hooks Named list of functions `function(api, state, plugin)`. Names
#'   are supported state callback names from `eplus_api()$runtime`, without the
#'   `callback_` prefix. The host also accepts `initialize` and `finalize`.
#' @param data Initial ordinary R data, copied independently for every binding.
#' @param warmup Whether timestep hooks also run during simulation warmup.
#'   Environment, input and sizing lifecycle hooks are always delivered.
#'
#' @param trends Named list of managed histories. Each definition contains a
#'   numeric `source = function(api, state, plugin)`, positive integer `capacity`,
#'   and optional `warmup = FALSE` and `replay = TRUE`. Set `replay = FALSE`
#'   for sources that cannot be safely reconstructed in a fresh worker.
#'   Sources may read R-owned `plugin$data` or
#'   outputs declared with `request_variable()` and read through managed handles.
#'
#' @details
#' Pass the returned function directly as `backend`, or call several returned
#' functions from one backend initializer to register multiple plugins.
#' Initialization happens before simulation. Native hooks execute in registration
#' order; finalizers execute once, in reverse registration order, after the
#' native simulation returns or before an unused state is deleted or reset.
#'
#' Plugins share one worker interpreter; global assignments and external side
#' effects are not isolated. Keep per-plugin mutable state in its context.
#' Each binding receives its own mutable `plugin` environment with `data`,
#' `handles`, `id`, `identity` and `outcome`. Store ordinary results in `data`
#' and worker-local native handles in `handles`. `outcome` is populated before
#' finalization. Finalizers should clean up R resources and inspect this outcome;
#' a callback failure may already have made native operations unavailable.
#' Native state deletion, reset, recursive simulation and API close are blocked
#' while a plugin hook executes.
#'
#' Callback errors and interrupts request a native stop and are rethrown after
#' native return and plugin cleanup. The first failure takes precedence over
#' later finalizer failures. Hard process termination or an unexpected native
#' unwind cannot guarantee user finalization. No native pointer is returned in
#' plugin summaries. Completed summaries appear in state status and API Job
#' metadata under `plugins`, including ordinary final `data` and hook counts.
#' Progress/message callbacks and component hooks requiring additional arguments
#' remain available through the low-level API, outside this state-hook interface.
#' Managed trends require no PythonPlugin declarations. Resolve their local names
#' with `api$exchange$get_trend_handle(state, name)` and read index 1 (newest)
#' with `get_trend_value(state, handle, index)`. Histories begin empty; invalid
#' indices and insufficient valid history raise errors without zero padding.
#' Outside a hook, lookup requires a unique name across active bindings. Handles
#' cannot cross plugin bindings or states and retire after finalization or reset.
#' `get_trend_average()`, `get_trend_min()`, `get_trend_max()` and
#' `get_trend_sum()` aggregate the requested most recent valid samples, accepting
#' a positive integer count including one. `get_trend_direction()` requires at
#' least two samples and fits a least-squares slope against their actual completed-
#' zone times in elapsed simulation hours. The slope has source-value units per
#' hour and is positive when values increase chronologically, despite newest-first
#' indexing. These counts cannot exceed the valid history; no storage is padded.
#'
#' One sample is committed after all end-zone hooks, including updates made by
#' those hooks. Reads in the hooks see previously committed history. Collection
#' defaults to formal weather simulation, excluding sizing and warmup. An
#' explicitly warmup-enabled trend is cleared before formal-weather hooks; every
#' new environment clears history. Reads, HVAC iterations and snapshot frequency
#' do not add samples. Each finite numeric source is evaluated at the completed
#' zone boundary; an engine output retains that output's averaging semantics.
#' Plugin status and completed summaries expose each trend's capacity,
#' valid_count, total_samples, replay eligibility and latest environment/completed-zone
#' time metadata.
#' Storage retains at most capacity samples. Source errors and interrupts use the
#' same deferred failure and cleanup boundary as hooks.
#'
#' Session prediction rebuilds each binding from its frozen backend initializer,
#' replays recorded controls and compares bounded history before future policies.
#' `replay = TRUE` requires deterministic hooks and sources under those inputs;
#' the host cannot prove arbitrary R functions deterministic. Declare external,
#' random or worker-specific sources with `replay = FALSE` to reject prediction
#' before any branch starts. Undeclared differences are caught by prefix comparison
#' before a future policy runs. Matching history does not establish native state equality.
#'
#' Native PythonPlugin globals and trends remain distinct; the unsafe native
#' trend value reader stays disabled. Host plugins do not enable state cloning.
#'
#' @return An initialization function accepted by `backend`.
#' @examples
#' \dontrun{
#' plugin <- eplus_plugin(list(
#'     initialize = function(api, state, plugin) {
#'         plugin$data$steps <- 0L
#'     },
#'     end_zone_timestep_after_zone_reporting = function(api, state, plugin) {
#'         plugin$data$steps <- plugin$data$steps + 1L
#'     }
#' ))
#' job <- eplus_job("model.idf", "weather.epw")
#' job$run(backend = plugin)
#' }
#' @include api.R
#' @export
eplus_plugin <- function(
    hooks,
    data = list(),
    warmup = FALSE,
    trends = list()
) {
    checkmate::assert_list(hooks, min.len = 1L, names = "unique")
    checkmate::assert_flag(warmup)
    allowed <- c("initialize", "finalize", names(plugin__hooks()))
    if (any(!names(hooks) %in% allowed)) {
        api__error(
            "Unknown R plugin hook; use a supported state callback name."
        )
    }
    trends <- plugin__trend_definitions(trends)
    api_session__plain(data)
    for (hook in hooks) {
        checkmate::assert_function(hook, nargs = 3L)
        run__check_setup(hook)
    }
    # Freeze lexical dependencies before serialization; a worker's global frame is different.
    # Freeze hooks together so ordinary helper environments retain their aliases.
    recipe <- function() list(hooks = hooks, data = data, trends = trends)
    definition <- run__prepare_setup(recipe)()
    payload <- serialize(c(definition, list(warmup = warmup)), NULL)
    initializer <- function(api, state) plugin__register(api, state, payload)
    environment(initializer) <- list2env(
        list(payload = payload),
        parent = environment(plugin__register)
    )
    class(initializer) <- c("EnergyPlusPlugin", "function")
    initializer
}

# Derive state-only hook names from the same ownership manifest as the managed API.
plugin__hooks <- function() {
    methods <- Filter(
        function(x) {
            x$group == "runtime" &&
                x$kind == "callback" &&
                x$callback == "p)v" &&
                length(x$parameters) == 2L &&
                x$status == "supported"
        },
        api__manifest()$methods
    )
    stats::setNames(
        vapply(methods, function(x) x$method, character(1L)),
        sub(
            "^callback_",
            "",
            vapply(methods, function(x) x$method, character(1L))
        )
    )
}

# Enter one plugin hook on the owner thread and prohibit destructive lifecycle operations.
plugin__call <- function(ctx, record, hook, api, state) {
    active <- ctx$active_plugin
    ctx$active_plugin <- record
    on.exit(
        {
            ctx$active_plugin <- active
        },
        add = TRUE
    )
    record$calls[[hook]] <- record$calls[[hook]] + 1L
    ctx$plugin_depth <- ctx$plugin_depth + 1L
    api__running$plugin_depth <- api__running$plugin_depth + 1L
    on.exit(
        {
            ctx$plugin_depth <- ctx$plugin_depth - 1L
            api__running$plugin_depth <- api__running$plugin_depth - 1L
        },
        add = TRUE
    )
    # Record the first local error without changing the condition crossing the safe R boundary.
    capture <- function(condition) {
        if (is.null(record$error)) {
            record$error <- condition
        }
        record$phase <- "failed"
        stop(condition)
    }
    tryCatch(
        record$hooks[[hook]](api, state, record$context),
        error = capture,
        interrupt = capture
    )
    invisible(NULL)
}

# Create independent data and callback closures for each state binding, including repeated factories.
plugin__register <- function(api, state, payload) {
    ctx <- attr(state, "owner")
    if (!is.environment(ctx) || !identical(ctx$api, api)) {
        api__error("An R plugin requires an API and its owned state.")
    }
    entry <- api__entry(ctx, state)
    if (
        entry$phase != "created" ||
            ctx$native_depth > 0L ||
            ctx$plugin_depth > 0L ||
            !is.null(api__running$state)
    ) {
        api__error("Bind R plugins before running a fresh or reset state.")
    }
    definition <- unserialize(payload)
    record <- new.env(parent = emptyenv())
    record$id <- as.character(length(entry$plugins) + 1L)
    record$hooks <- definition$hooks
    record$calls <- stats::setNames(
        integer(length(record$hooks)),
        names(record$hooks)
    )
    record$context <- list2env(
        list(
            data = definition$data,
            handles = list(),
            id = record$id,
            identity = api$info(),
            outcome = NULL
        ),
        parent = emptyenv()
    )
    class(record$context) <- "EnergyPlusPluginContext"
    record$trends <- plugin__trend_records(definition$trends %||% list())
    record$trends_active <- TRUE
    record$finished <- FALSE
    record$error <- NULL
    record$data <- NULL
    record$phase <- "initializing"
    record$api <- api
    record$state <- state
    entry$plugins[[length(entry$plugins) + 1L]] <- record
    # Retire the failed setup state after all already-bound plugins have had a cleanup opportunity.
    capture <- function(condition) {
        if (is.null(record$error)) {
            record$error <- condition
        }
        if (is.null(entry$condition)) {
            entry$condition <- condition
        }
        entry$phase <- "failed"
        entry$stop_reason <- "plugin_initialize_error"
        api__dispose(ctx, entry)
        stop(condition)
    }
    tryCatch(
        {
            plugin__trend_lifecycle(ctx, entry, state)
            if ("initialize" %in% names(record$hooks)) {
                plugin__call(ctx, record, "initialize", api, state)
            }
            for (hook in setdiff(
                names(record$hooks),
                c("initialize", "finalize")
            )) {
                api__host_callback(
                    api,
                    state,
                    plugin__hooks()[[hook]],
                    plugin__callback(ctx, record, hook, definition$warmup)
                )
            }
            record$phase <- "registered"
        },
        error = capture,
        interrupt = capture
    )
    invisible(NULL)
}

# Keep each callback's hook name fixed and suppress only warmup timestep delivery by default.
plugin__callback <- function(ctx, record, hook, warmup) {
    force(hook)
    force(warmup)
    function(state) {
        if (record$finished) {
            return(invisible(NULL))
        }
        lifecycle <- hook %in%
            c(
                "begin_new_environment",
                "after_new_environment_warmup_complete",
                "after_component_get_input",
                "end_zone_sizing",
                "end_system_sizing",
                "unitary_system_sizing"
            )
        if (!warmup && !lifecycle && record$api$exchange$warmup_flag(state)) {
            return(invisible(NULL))
        }
        record$phase <- "running"
        plugin__call(ctx, record, hook, record$api, state)
    }
}

# Finish each binding once outside C++, retaining the first failure and ordinary result data.
plugin__finish <- function(ctx, entry) {
    for (record in rev(entry$plugins)) {
        if (record$finished) {
            next
        }
        record$finished <- TRUE
        record$context$outcome <- list(
            phase = entry$phase,
            stop_reason = entry$stop_reason,
            exit_code = entry$exit_code,
            error = if (is.null(entry$condition)) {
                NULL
            } else {
                list(
                    class = class(entry$condition),
                    message = conditionMessage(entry$condition)
                )
            }
        )
        # Later cleanup failures must not replace the original controller error or interrupt.
        capture <- function(condition) {
            if (is.null(record$error)) {
                record$error <- condition
            }
            if (is.null(entry$condition)) {
                entry$condition <- condition
                entry$stop_reason <- "plugin_finalize_error"
            }
            invisible(NULL)
        }
        tryCatch(
            {
                if ("finalize" %in% names(record$hooks)) {
                    plugin__call(
                        ctx,
                        record,
                        "finalize",
                        record$api,
                        record$state
                    )
                }
            },
            error = capture,
            interrupt = capture
        )
        tryCatch(
            {
                api_session__plain(record$context$data)
                record$data <- unserialize(serialize(record$context$data, NULL))
            },
            error = capture,
            interrupt = capture
        )
        record$phase <- if (is.null(record$error)) "completed" else "failed"
        record$trend_status <- plugin__trend_status(record)
        record$trends_active <- FALSE
        record$trends <- NULL
        # Drop closures and native handle caches before native state teardown.
        record$hooks <- NULL
        record$context <- NULL
        record$api <- NULL
        record$state <- NULL
    }
    invisible(NULL)
}

# Copy diagnostics without exporting plugin contexts, callbacks or native pointers.
plugin__status <- function(entry) {
    lapply(entry$plugins, function(record) {
        list(
            id = record$id,
            phase = record$phase,
            calls = record$calls,
            trends = record$trend_status %||% plugin__trend_status(record),
            data = record$data,
            error = if (is.null(record$error)) {
                NULL
            } else {
                list(
                    class = class(record$error),
                    message = conditionMessage(record$error)
                )
            }
        )
    })
}

# Copy binding-local bounded histories for observations without transferring handles or contexts.
plugin__history <- function(entry) {
    lapply(entry$plugins, function(record) {
        trends <- plugin__trend_status(record)
        for (name in names(trends)) {
            # Arrays contain valid newest-first samples only; unused capacity never crosses IPC.
            trends[[name]]$values <- record$trends[[name]]$values
            trends[[name]]$times <- record$trends[[name]]$times
        }
        list(id = record$id, trends = trends)
    })
}

# Validate named finite histories before freezing their sources with the plugin hooks.
plugin__trend_definitions <- function(trends) {
    checkmate::assert_list(trends, names = "unique")
    lapply(trends, function(trend) {
        if (
            !is.list(trend) ||
                !all(c("source", "capacity") %in% names(trend)) ||
                any(
                    !names(trend) %in%
                        c("source", "capacity", "warmup", "replay")
                )
        ) {
            api__error(
                "Each trend needs source and capacity, with optional warmup and replay."
            )
        }
        checkmate::assert_function(trend$source, nargs = 3L)
        run__check_setup(trend$source)
        checkmate::assert_int(trend$capacity, lower = 1L)
        trend$capacity <- as.integer(trend$capacity)
        trend$warmup <- trend$warmup %||% FALSE
        checkmate::assert_flag(trend$warmup)
        # Replay eligibility is explicit; it cannot be inferred from an arbitrary R source.
        trend$replay <- trend$replay %||% TRUE
        checkmate::assert_flag(trend$replay)
        trend
    })
}

# Allocate binding-local bounded storage without treating unused capacity as measurements.
plugin__trend_records <- function(definitions) {
    lapply(definitions, function(definition) {
        trend <- list2env(definition, parent = emptyenv())
        trend$values <- numeric()
        trend$times <- numeric()
        trend$total_samples <- 0L
        trend$latest <- NULL
        trend$boundary <- NULL
        trend
    })
}

# Publish ordinary metadata while the binding is active and after retirement.
plugin__trend_status <- function(record) {
    lapply(record$trends, function(trend) {
        list(
            capacity = trend$capacity,
            replay = trend$replay,
            valid_count = length(trend$values),
            total_samples = trend$total_samples,
            latest = trend$latest
        )
    })
}

# Route only explicitly managed histories around the disabled native reader.
plugin__trend_operation <- function(ctx, spec, values) {
    if (ctx$closed || spec$group != "exchange") {
        return(FALSE)
    }
    if (inherits(values$trend_handle, "EnergyPlusManagedTrendHandle")) {
        return(TRUE)
    }
    if (spec$method != "get_trend_handle") {
        return(FALSE)
    }
    entry <- api__entry(ctx, values$state)
    any(vapply(
        entry$plugins,
        function(record) length(record$trends) > 0L,
        logical(1L)
    ))
}

# Resolve names locally during hooks, and reject ambiguous state-wide queries outside hooks.
plugin__trend_invoke <- function(ctx, method, values) {
    if (ctx$closed || !ctx$ffi$live()) {
        api__error("EnergyPlus API is closed.")
    }
    entry <- api__entry(ctx, values$state)
    if (method == "get_trend_handle") {
        name <- values$trend_var_name
        checkmate::assert_string(name, min.chars = 1L)
        if (
            !is.null(ctx$active_plugin) &&
                !identical(ctx$active_plugin$state, values$state)
        ) {
            api__error(
                "Managed trend lookup belongs to another state or plugin binding."
            )
        }
        records <- if (is.null(ctx$active_plugin)) {
            entry$plugins
        } else {
            list(ctx$active_plugin)
        }
        records <- Filter(
            function(record) {
                record$trends_active && name %in% names(record$trends)
            },
            records
        )
        if (!length(records)) {
            api__error("Managed trend was not found in this plugin binding.")
        }
        if (length(records) != 1L) {
            api__error(
                "Managed trend name is ambiguous across plugin bindings."
            )
        }
        return(structure(
            1L,
            class = c(
                "EnergyPlusManagedTrendHandle",
                "EnergyPlusHandle",
                "integer"
            ),
            owner = ctx,
            state_id = entry$id,
            binding = records[[1L]],
            name = name,
            kind = "managed_trend"
        ))
    }
    handle <- values$trend_handle
    record <- attr(handle, "binding")
    if (
        !identical(attr(handle, "owner"), ctx) ||
            !identical(attr(handle, "state_id"), entry$id) ||
            !is.environment(record) ||
            !isTRUE(record$trends_active) ||
            !identical(record$state, values$state) ||
            (!is.null(ctx$active_plugin) &&
                !identical(ctx$active_plugin, record))
    ) {
        api__error(
            "Managed trend handle is retired or belongs to another state or plugin binding."
        )
    }
    trend <- record$trends[[attr(handle, "name")]]
    # Indexed reads and aggregate windows share the same bounded valid-history limit.
    index <- if (method == "get_trend_value") {
        values$time_index
    } else {
        values$count
    }
    if (
        !is.numeric(index) ||
            is.complex(index) ||
            length(index) != 1L ||
            !is.finite(index) ||
            index != trunc(index) ||
            index < 1 ||
            index > length(trend$values)
    ) {
        api__error(
            if (method == "get_trend_value") {
                "Trend index must be a positive integer within valid history."
            } else {
                "Trend count must be a positive integer within valid history."
            }
        )
    }
    if (method == "get_trend_value") {
        return(trend$values[[index]])
    }
    # Count one is meaningful for these aggregates, unlike the native plugin restriction.
    samples <- trend$values[seq_len(index)]
    switch(
        method,
        get_trend_average = mean(samples),
        get_trend_min = min(samples),
        get_trend_max = max(samples),
        get_trend_sum = sum(samples),
        get_trend_direction = {
            if (index < 2L) {
                api__error(
                    "Trend direction requires at least two valid samples."
                )
            }
            # Both vectors are newest-first; actual elapsed hours preserve chronological sign.
            times <- trend$times[seq_len(index)]
            centered <- times - mean(times)
            sum(centered * (samples - mean(samples))) / sum(centered^2)
        }
    )
}

# Install one protected final callback so all selected end-zone hooks precede the commit.
plugin__prepare <- function(ctx, entry) {
    if (
        !any(vapply(
            entry$plugins,
            function(record) length(record$trends) > 0L,
            logical(1L)
        ))
    ) {
        return(invisible(NULL))
    }
    state <- entry$plugins[[1L]]$state
    api__host_callback(
        ctx$api,
        state,
        "callback_end_zone_timestep_after_zone_reporting",
        function(state) {
            plugin__commit(ctx, entry, state)
        }
    )
    invisible(NULL)
}

# Collect one finite sample per eligible completed-zone boundary under the callback condition guard.
plugin__commit <- function(ctx, entry, state) {
    api <- ctx$api
    exchange <- api$exchange
    warmup <- exchange$warmup_flag(state)
    if (exchange$kind_of_sim(state) != 3L) {
        return(invisible(NULL))
    }
    environment <- exchange$current_environment_num(state)
    # Remove the HVAC offset from environment elapsed time, then add the completed zone boundary.
    # Unlike calendar day-of-year, this coordinate stays chronological across New Year.
    time <- exchange$current_sim_time(state) -
        exchange$current_time(state) +
        exchange$hour(state) +
        exchange$zone_time_step_number(state) /
            exchange$num_time_steps_in_hour(state)
    boundary <- list(
        environment = environment,
        time = time,
        warmup = warmup,
        time_start = time - exchange$zone_time_step(state),
        callback = "end_zone_timestep_after_zone_reporting"
    )
    for (record in entry$plugins) {
        if (!length(record$trends)) {
            next
        }
        active <- ctx$active_plugin
        ctx$active_plugin <- record
        ctx$plugin_depth <- ctx$plugin_depth + 1L
        api__running$plugin_depth <- api__running$plugin_depth + 1L
        # Source closures receive the same ownership and destructive-operation guard as hooks.
        restore <- function() {
            ctx$active_plugin <- active
            ctx$plugin_depth <- ctx$plugin_depth - 1L
            api__running$plugin_depth <- api__running$plugin_depth - 1L
        }
        tryCatch(
            {
                for (trend in record$trends) {
                    if (
                        (!trend$warmup && warmup) ||
                            identical(trend$boundary, boundary)
                    ) {
                        next
                    }
                    value <- trend$source(api, state, record$context)
                    if (
                        !is.numeric(value) ||
                            is.complex(value) ||
                            length(value) != 1L ||
                            !is.finite(value)
                    ) {
                        api__error(
                            "Managed trend source must return one finite numeric value."
                        )
                    }
                    trend$values <- head(
                        c(as.double(value), trend$values),
                        trend$capacity
                    )
                    trend$times <- head(c(time, trend$times), trend$capacity)
                    trend$total_samples <- trend$total_samples + 1L
                    trend$latest <- boundary
                    trend$boundary <- boundary
                }
            },
            error = function(condition) {
                record$error <- record$error %||% condition
                stop(condition)
            },
            interrupt = function(condition) {
                record$error <- record$error %||% condition
                stop(condition)
            },
            finally = restore()
        )
    }
    invisible(NULL)
}

# Clear phase-local histories before environment and formal-weather hooks are delivered.
plugin__trend_lifecycle <- function(ctx, entry, state) {
    if (
        isTRUE(entry$trend_lifecycle) ||
            !any(vapply(
                entry$plugins,
                function(record) length(record$trends) > 0L,
                logical(1L)
            ))
    ) {
        return(invisible(NULL))
    }
    entry$trend_lifecycle <- TRUE
    for (method in c(
        "callback_begin_new_environment",
        "callback_after_new_environment_warmup_complete"
    )) {
        api__host_callback(ctx$api, state, method, function(state) {
            for (record in entry$plugins) {
                for (trend in record$trends) {
                    trend$values <- numeric()
                    trend$times <- numeric()
                    trend$total_samples <- 0L
                    trend$latest <- NULL
                    trend$boundary <- NULL
                }
            }
        })
    }
    invisible(NULL)
}
