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
#' Host plugins do not enable PythonPlugin global/trend APIs or state cloning.
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
eplus_plugin <- function(hooks, data = list(), warmup = FALSE) {
    checkmate::assert_list(hooks, min.len = 1L, names = "unique")
    checkmate::assert_flag(warmup)
    allowed <- c("initialize", "finalize", names(plugin__hooks()))
    if (any(!names(hooks) %in% allowed)) {
        api__error(
            "Unknown R plugin hook; use a supported state callback name."
        )
    }
    api_session__plain(data)
    for (hook in hooks) {
        checkmate::assert_function(hook, nargs = 3L)
        run__check_setup(hook)
    }
    # Freeze lexical dependencies before serialization; a worker's global frame is different.
    # Freeze hooks together so ordinary helper environments retain their aliases.
    recipe <- function() list(hooks = hooks, data = data)
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
            if ("initialize" %in% names(record$hooks)) {
                plugin__call(ctx, record, "initialize", api, state)
            }
            for (hook in setdiff(
                names(record$hooks),
                c("initialize", "finalize")
            )) {
                api$runtime[[plugin__hooks()[[hook]]]](
                    state,
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
