#' Access the EnergyPlus library API
#'
#' Create a managed interface to a stock EnergyPlus 26.1 installation. The four
#' groups use the same method names as the bundled Python API, with an explicit
#' managed `state` argument. This experimental interface does not change CLI jobs.
#'
#' @param eplus EnergyPlus version or installation directory. Only 26.1 is
#'   supported by this experimental library interface.
#' @return An environment with `state_manager`, `functional`, `runtime`, and
#'   `exchange` groups, and `info()`, `methods()`, and `close()` methods.
#' @details
#' `state_manager$new_state()` returns an opaque state. Use `reset_state(state)`
#' before re-running a completed state, and `delete_state(state)` when finished.
#' Reset invalidates callbacks, requests, functional objects, and data handles.
#' `state_manager$status(state)` reports the phase, generation, and stop reason.
#'
#' `functional$psychrometrics(state)`, `glycol(state, name)`, and
#' `refrigerant(state, name)` create state-bound objects with Python-compatible
#' methods. Each property method still takes its owning state. Public resource
#' deletion invalidates the R object; stock 26.1 may retain native allocations.
#' Standalone factories support water and steam, matching the stock Python SDK.
#' `functional$ep_version()` returns the full version string as an R character.
#' Standalone functional files are isolated in a temporary state-owned directory.
#' Property objects are invalidated when a simulation begins.
#'
#' Register callbacks through `runtime$callback_*(state, f)`, then call
#' `runtime$run_energyplus(state, command_line_args)`. The program name is added
#' automatically. State callbacks receive the managed state; progress/message
#' callbacks receive their native payload, and the functional error callback
#' receives severity and message. Callbacks run synchronously on the R thread.
#' A native proxy blocks foreign-thread delivery and reports failure after return.
#' Up to 256 callbacks may be retained in one R process across all states.
#' Errors and interrupts are saved inside the callback, request an orderly stop,
#' and are rethrown only after native return and state deletion. `stop_simulation`
#' is a user stop, distinct from complete simulation. The return value is the
#' native integer exit code; inspect state status to distinguish completion.
#'
#' Exchange handles are numeric objects bound to state, generation, and kind.
#' Queries and actions requiring initialized data must run in a ready callback.
#' Passing a handle from another state, kind, or reset generation is rejected
#' before calling C. Invalid native exchange values are checked with the API
#' error flag. `get_api_data(state)` copies a data frame and uses `freeAPIData()`.
#' `methods()` lists exact bindings, arguments, ownership, and unsupported methods.
#'
#' `rdyncall` is optional and is loaded only here. A build containing its native
#' callback-boundary fix (PR 75) is required. A quarantined R process checks error,
#' interrupt and return-conversion behavior; package version alone is insufficient
#' until the fix has an official release. Start a fresh R process if rdyncall
#' was already loaded by other code. EnergyPlus is loaded lazily and its
#' version/API identity is checked. Do not serialize live API objects to workers.
#'
#' `close()` deletes states and resources before unloading the library. A
#' finalizer is a fallback. Close/reset/delete and nested runs are rejected during
#' simulation. Original 26.1 library leaks are not repaired: use a fresh R process
#' for each long-lived simulation lifecycle. PythonPlugin models, Job integration,
#' worker sessions, step/pause and clone are outside this stage.
#' @examples
#' \dontrun{
#' api <- eplus_api("26.1")
#' state <- api$state_manager$new_state()
#' psych <- api$functional$psychrometrics(state)
#' psych$relative_humidity_b(state, 24, 0.009, 101325)
#' api$state_manager$delete_state(state)
#' api$close()
#'
#' # Feedback control requires a prepared model with ZONE ONE and a constant
#' # HEATING SETPOINT schedule connected to that zone's heating thermostat.
#' api <- eplus_api("26.1")
#' state <- api$state_manager$new_state()
#' exchange <- api$exchange
#' control <- new.env(parent = emptyenv())
#' control$handles <- NULL
#' control$previous_temperature <- NULL
#' exchange$request_variable(state, "Zone Mean Air Temperature", "ZONE ONE")
#'
#' # Write before the predictor reads the thermostat schedule. The first step
#' # keeps native control because no completed zone measurement exists yet.
#' api$runtime$callback_begin_system_timestep_before_predictor(state, function(s) {
#'     if (!exchange$api_data_fully_ready(s) || exchange$warmup_flag(s) ||
#'         exchange$kind_of_sim(s) != 3L) return(invisible(NULL))
#'     if (is.null(control$handles)) {
#'         control$handles <- list(
#'             temperature = exchange$get_variable_handle(
#'                 s, "Zone Mean Air Temperature", "ZONE ONE"
#'             ),
#'             heating = exchange$get_actuator_handle(
#'                 s, "Schedule:Constant", "Schedule Value", "HEATING SETPOINT"
#'             )
#'         )
#'     }
#'     if (!is.null(control$previous_temperature)) {
#'         # Bound the proportional control result to 18–23 degrees Celsius.
#'         heating <- max(18, min(23, 21 + 0.8 * (21 - control$previous_temperature)))
#'         exchange$set_actuator_value(s, control$handles$heating, heating)
#'     }
#' })
#'
#' # Save the completed zone measurement for the following control step.
#' api$runtime$callback_end_zone_timestep_after_zone_reporting(state, function(s) {
#'     if (!exchange$api_data_fully_ready(s) || exchange$warmup_flag(s) ||
#'         exchange$kind_of_sim(s) != 3L) return(invisible(NULL))
#'     control$previous_temperature <- exchange$get_variable_value(
#'         s, control$handles$temperature
#'     )
#' })
#' # Explicit cleanup also runs if the controller or simulation raises an error.
#' tryCatch(
#'     api$runtime$run_energyplus(state, c(
#'         "-w", "weather.epw", "-d", "api-output", "prepared-model.idf"
#'     )),
#'     finally = api$close()
#' )
#' }
#' @useDynLib eplusr, .registration = TRUE
#' @export
eplus_api <- function(eplus = "26.1") {
    config <- if (
        is.character(eplus) && length(eplus) == 1L && is_eplus_path(eplus)
    ) {
        suppressMessages(use_eplus(eplus))
    } else {
        suppressMessages(eplus_config(eplus))
    }
    if (!length(config) || as.character(config$version)[1L] != "26.1.0") {
        api__error(
            "The experimental library API requires an EnergyPlus 26.1 installation."
        )
    }
    api__check_bridge()
    api__load(config$dir)
}

# Track the one active native simulation in this R process, across API objects.
api__running <- new.env(parent = emptyenv())
api__running$state <- NULL
api__bridge_cache <- new.env(parent = emptyenv())

# Throw a conventional R condition before an invalid request can reach native code.
api__error <- function(message) {
    stop(structure(
        list(message = message, call = NULL),
        class = c("eplusr_api_error", "error", "condition")
    ))
}

# Read the audited static manifest; no library or optional bridge is loaded here.
api__manifest <- function() {
    jsonlite::fromJSON(
        system.file("api", "energyplus-26.1.json", package = "eplusr"),
        simplifyVector = FALSE
    )
}

# Probe only rdyncall in a child: an unfixed bridge must never enter EnergyPlus.
api__probe_bridge <- function() {
    previous <- options(warn = 2)
    on.exit(options(previous), add = TRUE)
    checks <- list(
        list(fun = function() stop("callback probe"), reason = "r_error"),
        list(fun = function() new.env(), reason = "boundary_error"),
        list(
            fun = function() {
                stop(structure(
                    list(message = "callback interrupt", call = NULL),
                    class = c("interrupt", "condition")
                ))
            },
            reason = "r_interrupt"
        )
    )
    vapply(
        checks,
        function(check) {
            callback <- rdyncall::ccallback(")i", check$fun)
            result <- tryCatch(
                rdyncall::dyncall(callback, ")i"),
                error = identity,
                interrupt = identity
            )
            status <- rdyncall::callback_status(callback)
            identical(result, 0L) &&
                isTRUE(status$disabled) &&
                identical(status$disable_reason, check$reason) &&
                status$error_invocations == 1
        },
        logical(1)
    )
}

# Cache capability proof by package location and PID, rather than an unreleased version label.
api__check_bridge <- function() {
    was_loaded <- isNamespaceLoaded("rdyncall")
    if (!requireNamespace("rdyncall", quietly = TRUE)) {
        api__error(
            "Install a rdyncall build containing the PR #75 callback-boundary fix to use eplus_api()."
        )
    }
    location <- normalizePath(system.file(package = "rdyncall"))
    key <- paste(
        Sys.getpid(),
        location,
        file.info(file.path(location, "DESCRIPTION"))$mtime
    )
    namespace <- asNamespace("rdyncall")
    proof <- api__bridge_cache[[key]]
    # Reloading a namespace ends the prior native capability proof, even at the same version.
    if (is.list(proof) && identical(proof$namespace, namespace)) {
        return(invisible(NULL))
    }
    if (was_loaded) {
        api__error(
            "Restart R before enabling the library API: rdyncall was already loaded without an API safety check."
        )
    }
    probe <- api__probe_bridge
    environment(probe) <- baseenv()
    safe <- tryCatch(
        callr::r(probe, libpath = .libPaths(), timeout = 30),
        error = function(e) FALSE
    )
    if (!all(safe)) {
        api__error(
            "This rdyncall build does not contain the complete callback-boundary fix (PR #75)."
        )
    }
    api__bridge_cache[[key]] <- list(namespace = namespace)
    invisible(NULL)
}

# Resolve the stock library basename for each supported installation layout.
api__library_name <- function(
    os = .Platform$OS.type,
    sysname = Sys.info()[["sysname"]]
) {
    if (os == "windows") {
        "EnergyPlusAPI.dll"
    } else if (sysname == "Darwin") {
        "libenergyplusapi.dylib"
    } else {
        "libenergyplusapi.so"
    }
}

# Bind only audited supported symbols and identify the actual library before exposing it.
api__load <- function(directory) {
    manifest <- api__manifest()
    if (.Machine$sizeof.pointer != manifest$pointer_bytes) {
        api__error("The EnergyPlus 26.1 manifest requires a 64-bit R process.")
    }
    filename <- api__library_name()
    path <- file.path(directory, filename)
    if (!file.exists(path)) {
        api__error(paste("EnergyPlus API library not found:", path))
    }
    library <- rdyncall::dynload(path, auto.unload = FALSE)
    if (is.null(library)) {
        api__error(
            "Cannot load the selected EnergyPlus API library into this R process."
        )
    }
    success <- FALSE
    on.exit(if (!success) rdyncall::dynunload(library), add = TRUE)
    supported <- unique(vapply(
        Filter(function(x) x$status == "supported", manifest$methods),
        function(x) x$symbol,
        character(1)
    ))
    needed <- unique(c(
        setdiff(supported, ""),
        "stateNew",
        "stateDelete",
        "apiVersionFromEPlus",
        "freeAPIData",
        "stopSimulation",
        "apiErrorFlag",
        "resetErrorFlag",
        "setEnergyPlusRootDirectory"
    ))
    symbols <- lapply(needed, function(name) {
        descriptor <- Filter(function(x) x$symbol == name, manifest$native)[[
            1L
        ]]
        address <- rdyncall::dynsym(library, name)
        if (is.null(address)) {
            api__error(paste("Missing required EnergyPlus symbol:", name))
        }
        list(address = address, signature = descriptor$signature)
    })
    names(symbols) <- needed
    # Each closure retains its symbol and the shared library for nested calls.
    call <- function(symbol, arguments = list()) {
        binding <- symbols[[symbol]]
        do.call(
            rdyncall::dyncall,
            c(list(binding$address, binding$signature), arguments)
        )
    }
    version <- call("energyPlusVersion")
    if (!grepl("26[.]1[.]", version)) {
        api__error("Loaded library is not EnergyPlus 26.1.")
    }
    pointer <- call("stateNew")
    if (rdyncall::is.nullptr(pointer)) {
        api__error("EnergyPlus could not allocate an identity-check state.")
    }
    # Retire the probe state even when API-version conversion fails.
    probe_live <- TRUE
    on.exit(
        if (probe_live) call("stateDelete", list(pointer)),
        add = TRUE,
        after = FALSE
    )
    api_version <- call("apiVersionFromEPlus", list(pointer))
    call("stateDelete", list(pointer))
    probe_live <- FALSE
    if (api_version != manifest$api_version) {
        api__error(
            "Loaded EnergyPlus API version does not match the 26.1 manifest."
        )
    }
    ffi <- list(
        callback = function(signature, fun) {
            original <- rdyncall::ccallback(signature, fun)
            .Call(
                "eplusr_guard_create",
                original,
                signature,
                PACKAGE = "eplusr"
            )
        },
        diagnostic = function(callback) {
            original <- .Call(
                "eplusr_guard_original",
                callback,
                PACKAGE = "eplusr"
            )
            list(
                foreign = .Call(
                    "eplusr_guard_foreign",
                    callback,
                    PACKAGE = "eplusr"
                ),
                bridge = rdyncall::callback_status(original)
            )
        },
        release = function(callback) {
            .Call("eplusr_guard_release", callback, PACKAGE = "eplusr")
        },
        argv = rdyncall::strarrayptr,
        pointer = rdyncall::as.externalptr,
        unpack = rdyncall::unpack,
        string = rdyncall::ptr2str,
        null = rdyncall::is.nullptr,
        live = function() !rdyncall::is.nullptr(library),
        unload = function() rdyncall::dynunload(library)
    )
    ctx <- new.env(parent = emptyenv())
    ctx$directory <- normalizePath(directory)
    ctx$library <- normalizePath(path)
    ctx$version <- version
    ctx$api_version <- api_version
    ctx$manifest <- manifest
    ctx$call <- call
    ctx$ffi <- ffi
    api <- api__construct(ctx)
    success <- TRUE
    api
}

# Construct managed groups above the real/fake native-call seam used by lifecycle tests.
api__construct <- function(ctx) {
    ctx$pid <- Sys.getpid()
    ctx$closed <- FALSE
    ctx$next_id <- 0L
    ctx$states <- new.env(parent = emptyenv())
    ctx$quarantined <- FALSE
    ctx$native_depth <- 0L
    native <- ctx$call
    # Native callbacks also occur in standalone functional calls; guard their teardown.
    ctx$call <- function(symbol, arguments = list()) {
        ctx$native_depth <- ctx$native_depth + 1L
        on.exit(
            {
                ctx$native_depth <- ctx$native_depth - 1L
            },
            add = TRUE
        )
        native(symbol, arguments)
    }
    api <- new.env(parent = emptyenv())
    ctx$api <- api
    for (group in c("state_manager", "functional", "runtime", "exchange")) {
        api[[group]] <- api__group(ctx, group)
    }
    # Inspect lifecycle metadata without exposing the native state pointer.
    api$state_manager$status <- function(state) {
        entry <- api__entry(ctx, state, allow_closed = TRUE)
        list(
            phase = entry$phase,
            generation = entry$generation,
            stop_reason = entry$stop_reason,
            exit_code = entry$exit_code
        )
    }
    # Identify the library and process associated with this managed API.
    api$info <- function() {
        list(
            version = ctx$version,
            api_version = ctx$api_version,
            library = ctx$library,
            pid = ctx$pid,
            closed = ctx$closed
        )
    }
    # Expose the versioned signature, support, and ownership contract for every method.
    api$methods <- function() {
        rows <- lapply(ctx$manifest$methods, function(x) {
            x$parameters <- paste(unlist(x$parameters), collapse = ", ")
            as.data.frame(x, stringsAsFactors = FALSE)
        })
        do.call(rbind, rows)
    }
    # Drain owned states before releasing the optional native library.
    api$close <- function() api__close(ctx)
    reg.finalizer(
        api,
        function(object) {
            if (
                !ctx$closed &&
                    !ctx$quarantined &&
                    ctx$pid == Sys.getpid() &&
                    ctx$native_depth == 0L &&
                    is.null(api__running$state)
            ) {
                api__close(ctx)
            }
        },
        onexit = TRUE
    )
    class(api) <- "EnergyPlusAPI"
    lockEnvironment(api, bindings = TRUE)
    api
}

# Create callable group methods from a fixed manifest, including explicit unsupported entries.
api__group <- function(ctx, group, resource = NULL) {
    output <- new.env(parent = emptyenv())
    entries <- Filter(function(x) x$group == group, ctx$manifest$methods)
    for (spec in entries) {
        output[[spec$method]] <- api__method(ctx, spec, resource)
    }
    output
}

# Preserve Python argument names without a separate hand-written wrapper for each scalar call.
api__method <- function(ctx, spec, resource) {
    method <- function() {
        api__invoke(ctx, spec, as.list(environment()), resource)
    }
    parameters <- unlist(spec$parameters)
    formals(method) <- as.pairlist(stats::setNames(
        rep(list(quote(expr = )), length(parameters)),
        parameters
    ))
    method
}

# Check state identity, process, and lifecycle before a native pointer is retrieved.
api__entry <- function(ctx, state, allow_closed = FALSE) {
    if (
        !inherits(state, "EnergyPlusState") ||
            !identical(attr(state, "owner"), ctx)
    ) {
        api__error(
            "State belongs to a different API or is not a managed EnergyPlus state."
        )
    }
    if (ctx$pid != Sys.getpid()) {
        api__error(
            "Live EnergyPlus objects cannot be used in another R process."
        )
    }
    entry <- attr(state, "entry")
    if (!allow_closed && (ctx$closed || entry$closed)) {
        api__error("EnergyPlus state or API is closed.")
    }
    if (!allow_closed && ctx$ffi$null(entry$pointer)) {
        api__error(
            "A live state pointer cannot be serialized or restored from an R object."
        )
    }
    entry
}

# Keep only records in the registry; an unused state can still reach its finalizer.
api__new_state <- function(ctx) {
    if (ctx$closed) {
        api__error("EnergyPlus API is closed.")
    }
    if (ctx$native_depth > 0L || !is.null(api__running$state)) {
        api__error("Cannot create a state inside an active native callback.")
    }
    if (ctx$pid != Sys.getpid()) {
        api__error("Create EnergyPlus states in their owning R process.")
    }
    pointer <- ctx$call("stateNew")
    if (ctx$ffi$null(pointer)) {
        api__error("EnergyPlus could not allocate a state.")
    }
    ctx$next_id <- ctx$next_id + 1L
    entry <- new.env(parent = emptyenv())
    entry$id <- as.character(ctx$next_id)
    entry$pointer <- pointer
    entry$generation <- 0L
    entry$phase <- "created"
    entry$closed <- FALSE
    entry$running <- FALSE
    entry$initialized <- FALSE
    entry$stop_reason <- NULL
    entry$exit_code <- NULL
    entry$condition <- NULL
    entry$callbacks <- list()
    entry$resources <- list()
    entry$functional_directory <- NULL
    entry$callback_directory <- NULL
    ctx$states[[entry$id]] <- entry
    state <- new.env(parent = emptyenv())
    class(state) <- "EnergyPlusState"
    attr(state, "owner") <- ctx
    attr(state, "entry") <- entry
    reg.finalizer(
        state,
        function(object) {
            if (
                !entry$closed &&
                    !entry$running &&
                    !ctx$quarantined &&
                    ctx$pid == Sys.getpid() &&
                    ctx$native_depth == 0L
            ) {
                api__dispose(ctx, entry)
            }
        },
        onexit = TRUE
    )
    lockEnvironment(state, bindings = TRUE)
    state
}

# Release public resources before the native state; keep callback references until deletion returns.
api__dispose <- function(ctx, entry) {
    if (entry$closed) {
        return(invisible(NULL))
    }
    if (
        entry$running || ctx$native_depth > 0L || !is.null(api__running$state)
    ) {
        api__error("Cannot delete a state while EnergyPlus is running.")
    }
    for (resource in entry$resources) {
        api__delete_resource(ctx, entry, resource)
    }
    ctx$call("stateDelete", list(entry$pointer))
    if (!is.null(entry$functional_directory)) {
        unlink(entry$functional_directory, recursive = TRUE)
    }
    entry$closed <- TRUE
    entry$phase <- "closed"
    entry$pointer <- NULL
    for (callback in entry$callbacks) {
        ctx$ffi$release(callback)
    }
    entry$callbacks <- list()
    entry$resources <- list()
    rm(list = entry$id, envir = ctx$states)
    invisible(NULL)
}

# Reset ends all prior native registrations and invalidates every typed child handle.
api__reset <- function(ctx, entry) {
    if (
        entry$running || ctx$native_depth > 0L || !is.null(api__running$state)
    ) {
        api__error("Cannot reset a state while EnergyPlus is running.")
    }
    for (resource in entry$resources) {
        api__delete_resource(ctx, entry, resource)
    }
    ctx$call("stateReset", list(entry$pointer))
    entry$generation <- entry$generation + 1L
    entry$phase <- "created"
    entry$initialized <- FALSE
    entry$condition <- NULL
    entry$stop_reason <- NULL
    entry$exit_code <- NULL
    for (callback in entry$callbacks) {
        ctx$ffi$release(callback)
    }
    entry$callbacks <- list()
    entry$resources <- list()
    invisible(NULL)
}

# An explicit API close drains all owned states before its library reference is released.
api__close <- function(ctx) {
    if (ctx$pid != Sys.getpid()) {
        api__error(
            "Live EnergyPlus objects cannot be closed in another R process."
        )
    }
    if (ctx$quarantined) {
        api__error(
            "An unexpected native unwind quarantined this API; exit this R process."
        )
    }
    if (ctx$closed) {
        return(invisible(NULL))
    }
    if (!ctx$ffi$live()) {
        api__error(
            "The native library reference is no longer live; do not serialize API objects."
        )
    }
    entries <- as.list(ctx$states)
    if (
        ctx$native_depth > 0L ||
            !is.null(api__running$state) ||
            any(vapply(entries, function(x) x$running, logical(1)))
    ) {
        api__error("Cannot close an API while EnergyPlus is running.")
    }
    for (entry in entries) {
        api__dispose(ctx, entry)
    }
    ctx$ffi$unload()
    ctx$closed <- TRUE
    invisible(NULL)
}

# Give standalone functional initialization its own files, restoring the user's working directory.
api__functional_directory <- function(entry) {
    if (is.null(entry$functional_directory)) {
        entry$functional_directory <- tempfile("eplusr-functional-")
        dir.create(entry$functional_directory)
    }
    entry$functional_directory
}

# Initialize tables once for standalone functional calls; a running engine owns its initialization.
api__initialize <- function(ctx, entry) {
    if (!entry$initialized) {
        if (!entry$running) {
            ctx$call("initializeFunctionalAPI", list(entry$pointer))
        }
        entry$initialized <- TRUE
    }
    invisible(NULL)
}

# Retain state/generation ownership for resource pointers and pure psychrometric views.
api__resource <- function(ctx, entry, state, kind, arguments) {
    api__initialize(ctx, entry)
    resource <- new.env(parent = emptyenv())
    resource$kind <- kind
    resource$state_id <- entry$id
    resource$generation <- entry$generation
    resource$closed <- FALSE
    resource$pointer <- NULL
    if (kind != "psychrometrics") {
        symbol <- if (kind == "glycol") "glycolNew" else "refrigerantNew"
        resource$pointer <- ctx$call(symbol, c(list(entry$pointer), arguments))
        if (ctx$ffi$null(resource$pointer)) {
            api__error(paste("Unknown EnergyPlus", kind, "name."))
        }
    }
    entry$resources[[length(entry$resources) + 1L]] <- resource
    output <- api__group(ctx, kind, resource)
    # The view pins its state and API until all property-method closures are released.
    attr(output, "state") <- state
    class(output) <- "EnergyPlusFunctional"
    lockEnvironment(output, bindings = TRUE)
    output
}

# Stock public deleters can be no-ops; R validity ends regardless of native reclamation.
api__delete_resource <- function(ctx, entry, resource) {
    if (resource$closed) {
        return(invisible(NULL))
    }
    if (resource$kind != "psychrometrics") {
        symbol <- if (resource$kind == "glycol") {
            "glycolDelete"
        } else {
            "refrigerantDelete"
        }
        ctx$call(symbol, list(entry$pointer, resource$pointer))
    }
    resource$closed <- TRUE
    resource$pointer <- NULL
    invisible(NULL)
}

# Validate ordinary scalar arguments against the native signature, never coercing arbitrary objects.
api__arguments <- function(signature, values, parameters) {
    types <- strsplit(
        strsplit(signature, ")", fixed = TRUE)[[1L]][1L],
        "",
        fixed = TRUE
    )[[1L]]
    for (i in seq_along(values)) {
        type <- types[i]
        value <- values[[i]]
        if (type == "Z") {
            if (!is.character(value) || length(value) != 1L || is.na(value)) {
                api__error("Expected one non-missing string.")
            }
            values[[i]] <- enc2utf8(value)
        } else if (type %in% c("i", "I", "d")) {
            if (
                (!is.numeric(value) && !is.logical(value)) ||
                    length(value) != 1L ||
                    !is.finite(value)
            ) {
                api__error("Expected one finite numeric or logical scalar.")
            }
            if (
                type != "d" &&
                    (value != trunc(value) ||
                        abs(value) > .Machine$integer.max ||
                        (type == "I" && value < 0))
            ) {
                api__error("Expected a representable C integer.")
            }
            values[[i]] <- if (type == "d") {
                as.double(value)
            } else {
                as.integer(value)
            }
        }
    }
    # Native forecast tables index a 24-hour day and the configured zone-step interval.
    if (
        "hour" %in%
            parameters &&
            (values[[which(parameters == "hour")]] < 0 ||
                values[[which(parameters == "hour")]] > 23)
    ) {
        api__error("Forecast hour must be between 0 and 23.")
    }
    values
}

# Typed numeric handles prevent stale or cross-kind C array indices after a reset.
api__handle <- function(value, ctx, entry, kind) {
    structure(
        as.integer(value),
        class = c("EnergyPlusHandle", "integer"),
        owner = ctx,
        state_id = entry$id,
        generation = entry$generation,
        kind = kind
    )
}

# Accept only the handle kind expected by its getter/setter before discarding R metadata.
api__check_handle <- function(value, ctx, entry, kind) {
    if (
        !inherits(value, "EnergyPlusHandle") ||
            !identical(attr(value, "owner"), ctx) ||
            !identical(attr(value, "state_id"), entry$id) ||
            !identical(attr(value, "generation"), entry$generation) ||
            !identical(attr(value, "kind"), kind)
    ) {
        api__error(
            "Handle belongs to another state, reset generation, or data kind."
        )
    }
    as.integer(value)
}

# Diagnose foreign-thread delivery and bridge-level marshalling failures on the R thread.
api__callback_failure <- function(ctx, entry) {
    for (callback in entry$callbacks) {
        diagnostic <- ctx$ffi$diagnostic(callback)
        if (diagnostic$foreign > 0 || diagnostic$bridge$error_invocations > 0) {
            message <- if (diagnostic$foreign > 0) {
                "EnergyPlus attempted an R callback on a foreign thread."
            } else {
                paste(
                    "The native callback bridge disabled a callback:",
                    diagnostic$bridge$disable_reason
                )
            }
            return(simpleError(message))
        }
    }
    NULL
}

# Catch the original condition in R before it can unwind any EnergyPlus C++ frame.
api__callback <- function(ctx, entry, state, spec, fun, extra) {
    if (!is.function(fun)) {
        api__error("A callback must be an R function.")
    }
    if (entry$running || entry$phase != "created" || ctx$native_depth > 0L) {
        api__error("Register callbacks before starting a fresh or reset state.")
    }
    guarded <- function(...) {
        if (!entry$running && !is.null(entry$callback_directory)) {
            native_directory <- getwd()
            setwd(entry$callback_directory)
            on.exit(setwd(native_directory), add = TRUE)
        }
        if (!is.null(entry$condition)) {
            return(invisible(NULL))
        }
        payload <- if (spec$callback == "p)v") list(state) else list(...)
        capture <- function(condition) {
            if (is.null(entry$condition)) {
                entry$condition <- condition
            }
            entry$stop_reason <- "callback_error"
            if (entry$running) {
                ctx$call("stopSimulation", list(entry$pointer))
            }
            invisible(NULL)
        }
        failure <- api__callback_failure(ctx, entry)
        if (!is.null(failure)) {
            capture(failure)
            return(invisible(NULL))
        }
        tryCatch(do.call(fun, payload), error = capture, interrupt = capture)
        invisible(NULL)
    }
    if (length(extra)) {
        extra <- api__arguments("Z)v", extra, names(extra))
    }
    callback <- ctx$ffi$callback(spec$callback, guarded)
    entry$callbacks[[length(entry$callbacks) + 1L]] <- callback
    ctx$call(spec$symbol, c(list(entry$pointer, callback), extra))
    invisible(NULL)
}

# Copy the five-pointer APIDataEntry layout, then always invoke its matching native deleter.
api__data <- function(ctx, entry) {
    count <- integer(1L)
    pointer <- ctx$call(
        "getAPIData",
        list(entry$pointer, ctx$ffi$pointer(count))
    )
    if (ctx$ffi$null(pointer)) {
        api__error("EnergyPlus returned a null API data array.")
    }
    on.exit(ctx$call("freeAPIData", list(pointer, count[1L])), add = TRUE)
    fields <- unlist(ctx$manifest$api_data_fields)
    rows <- lapply(seq_len(count[1L]), function(i) {
        vapply(
            seq_along(fields),
            function(j) {
                offset <- as.integer(
                    (i - 1L) *
                        ctx$manifest$api_data_stride +
                        (j - 1L) * ctx$manifest$pointer_bytes
                )
                value <- ctx$ffi$unpack(pointer, offset, "p")
                if (ctx$ffi$null(value)) "" else ctx$ffi$string(value)
            },
            character(1)
        )
    })
    matrix <- if (length(rows)) {
        do.call(rbind, rows)
    } else {
        matrix(character(), 0L, length(fields))
    }
    stats::setNames(as.data.frame(matrix, stringsAsFactors = FALSE), fields)
}

# Execute one native lifecycle, retaining argv/callbacks and reporting failures only after cleanup.
api__run <- function(ctx, entry, args) {
    if (!is.character(args) || anyNA(args) || !length(args)) {
        api__error("command_line_args must be a non-missing character vector.")
    }
    if (
        !is.null(api__running$state) ||
            entry$phase != "created" ||
            ctx$native_depth > 0L
    ) {
        api__error(
            "EnergyPlus cannot run recursively or reuse a state without reset."
        )
    }
    models <- args[grepl("[.](idf|epjson)$", args, ignore.case = TRUE)]
    if (length(models) != 1L || !file.exists(models)) {
        api__error("Provide one existing prepared IDF or epJSON model.")
    }
    source <- paste(readLines(models, warn = FALSE), collapse = "\n")
    # IDF names are case-insensitive and comments may precede a plugin object.
    idf_source <- gsub("![^\n]*", "", source)
    if (
        grepl(
            "(^|;)\\s*PythonPlugin:",
            idf_source,
            perl = TRUE,
            ignore.case = TRUE
        ) ||
            grepl('"PythonPlugin:', source, fixed = TRUE)
    ) {
        api__error(
            "PythonPlugin models are outside the EnergyPlus 26.1 R library workflow."
        )
    }
    argv <- ctx$ffi$argv(enc2utf8(c("energyplus", args)))
    ctx$call("setEnergyPlusRootDirectory", list(entry$pointer, ctx$directory))
    for (resource in entry$resources) {
        api__delete_resource(ctx, entry, resource)
    }
    entry$resources <- list()
    entry$generation <- entry$generation + 1L
    entry$running <- TRUE
    entry$initialized <- TRUE
    entry$phase <- "running"
    api__running$state <- entry
    # Clear the process lock even if the bridge unexpectedly raises at the outer call.
    on.exit(
        {
            entry$running <- FALSE
            api__running$state <- NULL
        },
        add = TRUE
    )
    code <- tryCatch(
        ctx$call(
            "energyplus",
            list(entry$pointer, as.integer(length(args) + 1L), argv)
        ),
        error = function(condition) {
            # Unknown unwinds must not be followed by native deletion or library unload.
            ctx$quarantined <- TRUE
            entry$phase <- "quarantined"
            stop(condition)
        },
        interrupt = function(condition) {
            ctx$quarantined <- TRUE
            entry$phase <- "quarantined"
            stop(condition)
        }
    )
    entry$running <- FALSE
    api__running$state <- NULL
    entry$exit_code <- code
    if (is.null(entry$condition)) {
        entry$condition <- api__callback_failure(ctx, entry)
    }
    if (!is.null(entry$condition)) {
        condition <- entry$condition
        api__dispose(ctx, entry)
        stop(condition)
    }
    entry$phase <- if (!is.null(entry$stop_reason)) {
        "stopped"
    } else if (code == 0L) {
        "completed"
    } else {
        "failed"
    }
    if (code != 0L && is.null(entry$stop_reason)) {
        entry$stop_reason <- "engine_error"
    }
    code
}

# Dispatch group operations through one lifecycle/ownership gate before the typed native call.
api__invoke <- function(ctx, spec, values, resource) {
    if (ctx$pid != Sys.getpid()) {
        api__error(
            "Live EnergyPlus objects cannot be used in another R process."
        )
    }
    if (ctx$quarantined) {
        api__error(
            "This API is quarantined after an unexpected native unwind; exit this R process."
        )
    }
    if (spec$status != "supported") {
        api__error(spec$reason)
    }
    if (ctx$closed) {
        api__error("EnergyPlus API is closed.")
    }
    if (!ctx$ffi$live()) {
        api__error(
            "The native library reference is no longer live; do not serialize API objects."
        )
    }
    if (spec$kind == "new_state") {
        return(api__new_state(ctx))
    }
    if (spec$method == "ep_version") {
        return(ctx$call("energyPlusVersion"))
    }
    state <- values$state
    entry <- api__entry(ctx, state, allow_closed = spec$kind == "delete_state")
    # Functional error callbacks need the same deferred-condition contract as simulation callbacks.
    on.exit(
        {
            if (
                !entry$running &&
                    !entry$closed &&
                    !ctx$quarantined &&
                    ctx$native_depth == 0L
            ) {
                if (is.null(entry$condition)) {
                    entry$condition <- api__callback_failure(ctx, entry)
                }
            }
            if (
                !entry$running &&
                    !entry$closed &&
                    !ctx$quarantined &&
                    ctx$native_depth == 0L &&
                    !is.null(entry$condition)
            ) {
                condition <- entry$condition
                api__dispose(ctx, entry)
                stop(condition)
            }
        },
        add = TRUE
    )
    if (
        spec$group %in%
            c("functional", "glycol", "refrigerant", "psychrometrics") &&
            !entry$running
    ) {
        original_directory <- getwd()
        entry$callback_directory <- original_directory
        setwd(api__functional_directory(entry))
        # Restore cwd before deferred-condition cleanup removes the owned directory.
        on.exit(setwd(original_directory), add = TRUE, after = FALSE)
    }
    if (spec$kind == "delete_state") {
        return(api__dispose(ctx, entry))
    }
    if (spec$kind == "reset_state") {
        return(api__reset(ctx, entry))
    }
    if (spec$kind == "initialize") {
        return(api__initialize(ctx, entry))
    }
    if (spec$kind == "run") {
        return(api__run(ctx, entry, values$command_line_args))
    }
    if (spec$kind == "callback") {
        extra <- values[setdiff(names(values), c("state", "f"))]
        return(api__callback(ctx, entry, state, spec, values$f, extra))
    }
    values <- values[setdiff(names(values), "state")]
    if (spec$kind == "factory") {
        if (nzchar(spec$signature)) {
            values <- api__arguments(
                spec$signature,
                c(list(entry$pointer), values),
                c("state", names(values))
            )[-1L]
        }
        return(api__resource(ctx, entry, state, spec$method, values))
    }
    if (!is.null(resource)) {
        if (
            resource$closed ||
                resource$generation != entry$generation ||
                resource$state_id != entry$id
        ) {
            api__error(
                "Functional object is closed or belongs to a previous reset generation."
            )
        }
        if (spec$kind == "delete_resource") {
            return(api__delete_resource(ctx, entry, resource))
        }
    }
    if (
        spec$requires_ready &&
            (!entry$running ||
                !ctx$call("apiDataFullyReady", list(entry$pointer)))
    ) {
        api__error(
            "Exchange data is only available in an initialized simulation callback."
        )
    }
    if (spec$kind == "api_data") {
        return(api__data(ctx, entry))
    }
    if (spec$method == "request_variable" && entry$phase != "created") {
        api__error("Request variables before running a fresh or reset state.")
    }
    if (spec$method == "stop_simulation" && !entry$running) {
        api__error("Stop a simulation only from its active runtime callback.")
    }
    handle_name <- character()
    if (nzchar(spec$input_handle_kind)) {
        handle_name <- names(values)[1L]
        values[[handle_name]] <- api__check_handle(
            values[[handle_name]],
            ctx,
            entry,
            spec$input_handle_kind
        )
    }
    if ("time_step_number" %in% names(values)) {
        steps <- values$time_step_number
        max_steps <- ctx$call("numTimeStepsInHour", list(entry$pointer))
        if (
            length(steps) != 1L ||
                !is.numeric(steps) ||
                is.na(steps) ||
                steps < 1 ||
                steps > max_steps
        ) {
            api__error(
                "Forecast time_step_number is outside the configured interval."
            )
        }
    }
    arguments <- c(
        list(entry$pointer),
        if (!is.null(resource) && resource$kind != "psychrometrics") {
            list(resource$pointer)
        },
        values
    )
    parameters <- c(
        "state",
        if (!is.null(resource) && resource$kind != "psychrometrics") "resource",
        names(values)
    )
    arguments <- api__arguments(spec$signature, arguments, parameters)
    check_flag <- spec$group == "exchange" &&
        (length(handle_name) || nzchar(spec$handle_kind))
    if (check_flag) {
        ctx$call("resetErrorFlag", list(entry$pointer))
    }
    result <- ctx$call(spec$symbol, arguments)
    if (check_flag && ctx$call("apiErrorFlag", list(entry$pointer))) {
        api__error("EnergyPlus reported an invalid exchange handle or value.")
    }
    if (spec$method == "stop_simulation") {
        entry$stop_reason <- "user_stop"
    }
    if (nzchar(spec$handle_kind)) {
        return(api__handle(result, ctx, entry, spec$handle_kind))
    }
    if (spec$boolean) {
        return(isTRUE(result != 0L))
    }
    if (endsWith(spec$signature, ")v")) {
        return(invisible(NULL))
    }
    result
}
