#' Export an R plugin for the EnergyPlus executable
#'
#' Generate a PythonPlugin adapter that embeds R in the stock EnergyPlus 26.1
#' executable process. Add the returned `idf` objects to the simulation model.
#' The export records this R installation and package library paths; regenerate
#' it after moving or updating R or the package libraries. Initialization occurs
#' on first native delivery, after model input processing; declare requested
#' output variables in the model. Merge the unique SearchPaths object when
#' exporting several modules, then add their separate Instance objects.
#'
#' @param plugin A factory returned by [eplus_plugin()].
#' @param dir Directory for the adapter and serialized plugin definition.
#' @param name Python module/class identifier, unique among exported plugins.
#' @param eplus EnergyPlus 26.1 version or installation directory.
#' @return A list containing `idf`, `directory`, `module`, and `results` pattern.
#' @details
#' The executable owns the native state. R hooks may read data, write actuators
#' and use functional methods, but cannot create/delete/reset states, register
#' native callbacks, recursively run simulations, or close the borrowed API.
#' Each PythonPlugin instance has an independent R context in a shared embedded
#' R interpreter. Global variables and trend aggregates declared in the model
#' are available through the borrowed exchange API. Individual trend reads remain
#' disabled because stock 26.1 has an unsafe upper index boundary.
#' A synchronous hook runs on the R initialization thread.
#'
#' Finalization runs once on handled hook failure or adapter shutdown. At shutdown
#' the native state may already be destroyed: finalizers can use ordinary R data
#' but cannot call the engine API. Hard termination cannot guarantee finalization.
#' Final summaries are written as JSON files matching the returned `results` pattern.
#' A shutdown finalizer failure is recorded there; it cannot change an already
#' completed executable's exit code. Check the summaries as well as the exit code.
#' The adapter cannot observe the final engine exit result: `engine$completion`
#' is `"unknown"`, even when the plugin itself finalized without an R error.
#' This entrypoint requires the installed R and eplusr native libraries, including
#' rdyncall, on the executable's architecture. It does not modify EnergyPlus.
#' @include plugin.R
#' @export
export_eplus_plugin <- function(
    plugin,
    dir,
    name = "eplusr_rplugin",
    eplus = "26.1"
) {
    checkmate::assert_class(plugin, "EnergyPlusPlugin")
    checkmate::assert_string(name, pattern = "^[A-Za-z][A-Za-z0-9_]*$")
    checkmate::assert_string(dir)
    config <- if (is_eplus_path(eplus)) {
        suppressMessages(use_eplus(eplus))
    } else {
        eplus_config(eplus)
    }
    if (!identical(as.character(config$version), "26.1.0")) {
        api__error(
            "Executable R plugins currently support only EnergyPlus 26.1."
        )
    }
    plugin_cli__check_bridge()
    definition <- unserialize(environment(plugin)$payload)
    mapping <- plugin_cli__hooks()
    if (
        any(
            !setdiff(names(definition$hooks), c("initialize", "finalize")) %in%
                names(mapping)
        )
    ) {
        api__error(
            "The executable does not expose one of the selected plugin hooks."
        )
    }
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
    }
    directory <- normalizePath(dir, winslash = "/", mustWork = TRUE)
    if (grepl("[,;!\r\n]", directory)) {
        api__error("Plugin directory contains an IDF delimiter.")
    }
    paths <- file.path(directory, paste0(name, c(".py", ".json", ".rds")))
    if (any(file.exists(paths))) {
        api__error(
            "Plugin export files already exist; choose a new directory or name."
        )
    }
    lib_r <- plugin_cli__r_library()
    metadata <- list(
        schema = 1L,
        configuration = paths[[2L]],
        r_home = R.home(),
        lib_r = normalizePath(lib_r, winslash = "/", mustWork = TRUE),
        libraries = as.list(normalizePath(
            .libPaths(),
            winslash = "/",
            mustWork = TRUE
        )),
        package = normalizePath(
            getLoadedDLLs()[["eplusr"]][["path"]],
            winslash = "/",
            mustWork = TRUE
        ),
        installation = normalizePath(
            config$dir,
            winslash = "/",
            mustWork = TRUE
        ),
        definition = paths[[3L]],
        directory = directory,
        name = name,
        hooks = as.list(mapping[intersect(
            names(definition$hooks),
            names(mapping)
        )])
    )
    saveRDS(definition, paths[[3L]])
    jsonlite::write_json(
        metadata,
        paths[[2L]],
        auto_unbox = TRUE,
        pretty = TRUE
    )
    template <- readLines(
        system.file("plugins", "adapter.py", package = "eplusr"),
        warn = FALSE
    )
    # JSON encoding supplies Python-compatible string literals for paths and identifiers.
    prefix <- c(
        paste0("CONFIG = ", jsonlite::toJSON(paths[[2L]], auto_unbox = TRUE)),
        paste0("CLASS = ", jsonlite::toJSON(name, auto_unbox = TRUE))
    )
    writeLines(c(prefix, template), paths[[1L]])
    list(
        directory = directory,
        module = name,
        results = file.path(directory, paste0(name, "-results-*.json")),
        idf = c(
            paste0(
                "PythonPlugin:SearchPaths,",
                name,
                ",Yes,Yes,No,",
                directory,
                ";"
            ),
            paste0(
                "PythonPlugin:Instance,",
                name,
                ",Yes,",
                name,
                ",",
                name,
                ";"
            )
        )
    )
}

# Retain only successful exporter proofs; every engine worker still checks its own bridge.
plugin_cli__bridge_cache <- new.env(parent = emptyenv())

# Identify the exact installed code selected by a fresh child without loading rdyncall here.
plugin_cli__bridge_identity <- function(libpath = .libPaths()) {
    packages <- tryCatch(
        find.package(c("eplusr", "rdyncall"), lib.loc = libpath, quiet = TRUE),
        error = function(e) character()
    )
    if (length(packages) != 2L) {
        return(NULL)
    }
    # Track both R wrappers and native code so reinstalling either invalidates proof.
    files <- unlist(
        lapply(packages, function(directory) {
            c(
                file.path(directory, "DESCRIPTION"),
                list.files(file.path(directory, "R"), full.names = TRUE),
                list.files(
                    file.path(directory, "libs"),
                    recursive = TRUE,
                    full.names = TRUE
                )
            )
        }),
        use.names = FALSE
    )
    metadata <- file.info(files)[, c("size", "mtime", "ctime"), drop = FALSE]
    if (anyNA(metadata)) {
        return(NULL)
    }
    list(
        pid = Sys.getpid(),
        libpath = libpath,
        files = files,
        metadata = metadata
    )
}

# Verify native bridge capabilities in a fresh process without loading it in the exporting R session.
plugin_cli__check_bridge <- function() {
    identity <- plugin_cli__bridge_identity()
    if (
        !is.null(identity) &&
            identical(plugin_cli__bridge_cache$identity, identity)
    ) {
        return(invisible(NULL))
    }
    # Clear stale proof before probing; a failed or incomplete check cannot populate the cache.
    plugin_cli__bridge_cache$identity <- NULL
    # The isolated preflight can load the optional bridge even if the caller already used rdyncall.
    probe <- function() {
        get("api__check_bridge", asNamespace("eplusr"))()
        TRUE
    }
    environment(probe) <- baseenv()
    result <- callr::r(probe, libpath = .libPaths(), timeout = 60)
    if (!isTRUE(result)) {
        api__error(
            "The isolated plugin bridge safety check did not complete successfully."
        )
    }
    plugin_cli__bridge_cache$identity <- identity
    invisible(NULL)
}

# Locate the matching R shared library without depending on executable search paths.
plugin_cli__r_library <- function(
    home = R.home(),
    bin = R.home("bin"),
    os = .Platform$OS.type,
    sysname = Sys.info()[["sysname"]]
) {
    if (os == "windows") {
        return(file.path(bin, "R.dll"))
    }
    extension <- if (sysname == "Darwin") ".dylib" else ".so"
    file.path(home, "lib", paste0("libR", extension))
}

# Translate the C runtime hook names to the executable's PythonPlugin names.
plugin_cli__hooks <- function() {
    names <- names(plugin__hooks())
    result <- stats::setNames(paste0("on_", names), names)
    result <- sub("^on_end_zone_timestep", "on_end_of_zone_timestep", result)
    result <- sub(
        "^on_end_system_timestep",
        "on_end_of_system_timestep",
        result
    )
    result[[
        "begin_system_timestep_before_predictor"
    ]] <- "on_begin_timestep_before_predictor"
    result[[
        "after_new_environment_warmup_complete"
    ]] <- "on_after_new_environment_warmup_is_complete"
    result[["after_component_get_input"]] <- "on_end_of_component_input_read_in"
    result <- result[names(result) != "register_external_hvac_manager"]
    result
}

# Keep borrowed proxies alive until the executable adapter finalizes each instance.
plugin_cli__instances <- new.env(parent = emptyenv())

# Bind an executable-owned state without registering a finalizer that could delete it.
plugin_cli__bind <- function(config, pointer, instance) {
    definition <- readRDS(config$definition)
    api <- eplus_api(config$installation)
    ctx <- environment(api$close)$ctx
    ctx$native_depth <- 1L
    # Only the executable loader initializes native PythonPlugin globals and trends.
    ctx$plugin_mode <- TRUE
    entry <- list2env(
        list(
            id = "1",
            pointer = pointer,
            generation = 0L,
            phase = "running",
            closed = FALSE,
            running = TRUE,
            initialized = TRUE,
            stop_reason = NULL,
            exit_code = NULL,
            condition = NULL,
            callbacks = list(),
            resources = list(),
            plugins = list(),
            functional_directory = NULL,
            callback_directory = NULL
        ),
        parent = emptyenv()
    )
    ctx$states[[entry$id]] <- entry
    state <- new.env(parent = emptyenv())
    class(state) <- "EnergyPlusState"
    attr(state, "owner") <- ctx
    attr(state, "entry") <- entry
    context <- list2env(
        list(
            data = definition$data,
            handles = list(),
            id = instance,
            identity = api$info(),
            outcome = NULL
        ),
        parent = emptyenv()
    )
    class(context) <- "EnergyPlusPluginContext"
    record <- list2env(
        list(
            id = instance,
            hooks = definition$hooks,
            calls = stats::setNames(
                integer(length(definition$hooks)),
                names(definition$hooks)
            ),
            context = context,
            finished = FALSE,
            error = NULL,
            data = NULL,
            phase = "registered",
            api = api,
            state = state
        ),
        parent = emptyenv()
    )
    entry$plugins <- list(record)
    list(
        ctx = ctx,
        entry = entry,
        record = record,
        config = config,
        warmup = definition$warmup
    )
}

# Finish ordinary R state once, without touching an engine state that shutdown may have destroyed.
plugin_cli__finish <- function(binding) {
    binding$entry$running <- FALSE
    binding$entry$closed <- TRUE
    binding$entry$phase <- if (is.null(binding$entry$condition)) {
        "unknown"
    } else {
        "failed"
    }
    binding$ctx$closed <- TRUE
    plugin__finish(binding$ctx, binding$entry)
    binding$entry$pointer <- NULL
    if (!is.null(binding$entry$condition)) {
        binding$entry$phase <- "failed"
    }
    path <- file.path(
        binding$config$directory,
        paste0(binding$config$name, "-results-", binding$record$id, ".json")
    )
    temporary <- paste0(path, ".writing")
    # Encode named hook counters as JSON objects, retaining lifecycle names in durable summaries.
    statuses <- lapply(plugin__status(binding$entry), function(status) {
        status$calls <- as.list(status$calls)
        status
    })
    jsonlite::write_json(
        list(
            identity = list(
                pid = binding$ctx$pid,
                version = binding$ctx$version
            ),
            engine = list(
                completion = "unknown",
                exit_code = NULL,
                state_owner = "executable"
            ),
            status = statuses
        ),
        temporary,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null"
    )
    if (!file.rename(temporary, path)) {
        stop("Could not publish R plugin summary.")
    }
    invisible(NULL)
}

# Capture every R error/interrupt before returning a nonzero plugin status to EnergyPlus.
plugin_cli__dispatch <- function(path, event, instance, pointer) {
    # Preserve the first diagnostic, finalize ordinary state, and request a native stop.
    capture <- function(condition) {
        if (exists(instance, plugin_cli__instances, inherits = FALSE)) {
            binding <- plugin_cli__instances[[instance]]
            if (is.null(binding$entry$condition)) {
                binding$entry$condition <- condition
            }
            if (is.null(binding$record$error)) {
                binding$record$error <- condition
            }
            binding$record$phase <- "failed"
            binding$entry$stop_reason <- if (inherits(condition, "interrupt")) {
                "interrupt"
            } else {
                "callback_error"
            }
            if (!binding$record$finished) {
                try(
                    binding$ctx$call(
                        "stopSimulation",
                        list(binding$entry$pointer)
                    ),
                    silent = TRUE
                )
                try(plugin_cli__finish(binding), silent = TRUE)
            }
        }
        message(conditionMessage(condition))
        1L
    }
    tryCatch(
        {
            if (!exists(instance, plugin_cli__instances, inherits = FALSE)) {
                if (event == "finalize") {
                    return(0L)
                }
                if (!file.exists(path)) {
                    stop("R plugin configuration is missing.")
                }
                config <- jsonlite::read_json(path, simplifyVector = TRUE)
                if (!file.exists(config$definition)) {
                    stop("R plugin definition is missing.")
                }
                binding <- plugin_cli__bind(config, pointer, instance)
                plugin_cli__instances[[instance]] <- binding
                previous <- api__running$state
                api__running$state <- binding$entry
                on.exit(
                    {
                        api__running$state <- previous
                    },
                    add = TRUE
                )
                if ("initialize" %in% names(binding$record$hooks)) {
                    plugin__call(
                        binding$ctx,
                        binding$record,
                        "initialize",
                        binding$record$api,
                        binding$record$state
                    )
                }
            }
            binding <- plugin_cli__instances[[instance]]
            if (event == "finalize") {
                if (!binding$record$finished) {
                    plugin_cli__finish(binding)
                }
                return(if (is.null(binding$entry$condition)) 0L else 1L)
            }
            if (binding$record$finished) {
                return(1L)
            }
            previous_hook <- api__running$state
            api__running$state <- binding$entry
            on.exit(
                {
                    api__running$state <- previous_hook
                },
                add = TRUE,
                after = FALSE
            )
            if (event %in% names(binding$record$hooks)) {
                plugin__callback(
                    binding$ctx,
                    binding$record,
                    event,
                    binding$warmup
                )(binding$record$state)
            }
            0L
        },
        error = capture,
        interrupt = capture
    )
}
