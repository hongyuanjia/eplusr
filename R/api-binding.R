# Map a reviewed ABI family to porter-generated declarations, never to handwritten signatures.
api_bind__select <- function(abi) {
    stopifnot(
        abi %in%
            c(
                "9.4.0",
                "9.5.0",
                "9.6.0",
                "23.1.0",
                "23.2.0",
                "24.1.0",
                "24.2-26.2",
                "25.2-26.2",
                "26.1-26.2"
            )
    )
    # Porter output for 24.2, 25.1 and 25.2 matches all 26.1 functions and APIDataEntry fields.
    list(
        dynport = if (
            abi %in% c("9.4.0", "9.5.0", "9.6.0", "23.1.0", "23.2.0", "24.1.0")
        ) {
            paste0("EnergyPlus-", sub("[.]0$", "", abi), ".dynport")
        } else {
            "EnergyPlus-26.1.dynport"
        },
        api_version = "0.2",
        pointer_bytes = 8L,
        api_data_fields = if (abi %in% c("9.4.0", "9.5.0", "9.6.0", "23.1.0")) {
            character()
        } else if (abi %in% c("23.2.0", "24.1.0")) {
            c("what", "name", "key", "type")
        } else {
            c("what", "name", "key", "type", "unit")
        }
    )
}

# Load generated bindings and identify the selected library before exposing managed methods.
api__load <- function(directory, version = "26.1.0") {
    manifest <- api__manifest(version)
    binding <- api_bind__select(manifest$abi)
    if (.Machine$sizeof.pointer != binding$pointer_bytes) {
        api__error("The EnergyPlus ABI requires a 64-bit R process.")
    }
    filename <- api__library_name()
    path <- file.path(directory, filename)
    if (!file.exists(path)) {
        api__error(paste("EnergyPlus API library not found:", path))
    }
    library <- rdyncall::dynload(path, auto.unload = FALSE)
    if (is.null(library)) {
        api__error(
            "Cannot load the selected EnergyPlus API library into this R process. Check that R and the library have matching architectures."
        )
    }
    success <- FALSE
    on.exit(if (!success) rdyncall::dynunload(library), add = TRUE)
    # Reuse the selected handle: the dynport must never search for another installation.
    native <- new.env(parent = baseenv())
    rdyncall::dynport_load_into(
        system.file("dynports", binding$dynport, package = "eplusr"),
        native,
        libhandle = library
    )
    signatures <- lapply(as.list(native), function(binding) {
        attr(binding, "signature")
    })
    layout <- api_cap__check(native, signatures, manifest, binding)
    # Dispatch managed calls through the generated C wrappers.
    call <- function(symbol, arguments = list()) {
        # R method names follow Python; generated wrappers use the C header names.
        do.call(native[[symbol]], unname(arguments))
    }
    loaded_version <- if (numeric_version(version) < "9.6") {
        api_legacy__identity(directory, version, library)
    } else {
        call("energyPlusVersion")
    }

    if (
        !grepl(
            paste0(
                "Version ",
                gsub(".", "[.]", version, fixed = TRUE),
                "([^0-9.]|$)"
            ),
            loaded_version
        )
    ) {
        api__error(paste("Loaded library is not EnergyPlus", version))
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
    if (api_version != binding$api_version) {
        api__error(
            "Loaded EnergyPlus API version does not match the reviewed ABI."
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
    ctx$identity_source <- if (numeric_version(version) < "9.6") {
        "selected SDK version/build, library generation symbols, selected CLI build; no native version getter"
    } else {
        "selected library native version getter"
    }
    ctx$version <- loaded_version

    ctx$api_version <- api_version
    ctx$manifest <- manifest
    ctx$signatures <- signatures
    ctx$api_data <- layout
    ctx$call <- call
    ctx$ffi <- ffi
    api <- api__construct(ctx)
    success <- TRUE
    api
}
# Identify pre-version-getter libraries using the selected SDK, symbol generation, and CLI.
api_legacy__identity <- function(directory, version, library) {
    sdk <- file.path(directory, "pyenergyplus", "func.py")
    if (!file.exists(sdk)) {
        api__error(
            "Early EnergyPlus identity requires its matching installed SDK."
        )
    }
    source <- readLines(sdk, warn = FALSE)
    fields <- c("major", "minor", "patch", "build")
    values <- vapply(
        fields,
        function(field) {
            pattern <- paste0(
                '^\\s*self[.]ep_version_',
                field,
                ' = (int|str)\\("([^"]+)"\\)\\s*$'
            )
            line <- grep(pattern, source, value = TRUE)
            if (length(line) != 1L) {
                api__error("Cannot identify the selected early EnergyPlus SDK.")
            }
            sub(pattern, "\\2", line)
        },
        character(1L)
    )
    sdk_version <- paste(values[1:3], collapse = ".")
    # These exact presence/absence boundaries distinguish the 9.4, 9.5 and 9.6 generations.
    has_nodes <- !is.null(rdyncall::dynsym(
        library,
        "getNumNodesInCondFDSurfaceLayer"
    ))
    has_version <- !is.null(rdyncall::dynsym(
        library,
        "energyPlusVersion"
    ))
    if (sdk_version != version || !has_nodes || has_version) {
        api__error(
            "Selected EnergyPlus library symbols do not match its early SDK identity."
        )
    }
    executable <- file.path(
        directory,
        if (.Platform$OS.type == "windows") "energyplus.exe" else "energyplus"
    )
    cli <- processx::run(executable, "--version", error_on_status = FALSE)
    identity <- paste0(sdk_version, "-", values["build"])
    if (cli$status != 0L || !grepl(identity, cli$stdout, fixed = TRUE)) {
        api__error(
            "Selected EnergyPlus CLI does not match its early SDK identity."
        )
    }
    trimws(cli$stdout)
}

# Observe every owned lifecycle boundary so old releases have an environment-local elapsed clock.
api_legacy__prepare <- function(ctx, entry, state) {
    if (numeric_version(ctx$manifest$energyplus) > "22.1" || ctx$plugin_mode) {
        return(invisible(NULL))
    }
    entry$stop_safe <- FALSE
    entry$deferred_stop <- FALSE
    # Early engines can dereference uninitialized resilience tables after an initialization stop.
    api__host_callback(
        ctx$api,
        state,
        "callback_end_zone_timestep_after_zone_reporting",
        function(state) invisible(NULL)
    )
    if (numeric_version(ctx$manifest$energyplus) >= "9.6") {
        return(invisible(NULL))
    }
    entry$relative_clock <- new.env(parent = emptyenv())
    clock <- entry$relative_clock
    # Warmup repeats the initial date; formal simulation starts a fresh relative coordinate.
    clear <- function(state) {
        clock$offset <- 0
        clock$date <- NULL
        clock$active <- TRUE
        invisible(NULL)
    }
    clock$active <- FALSE
    api__host_callback(ctx$api, state, "callback_begin_new_environment", clear)
    api__host_callback(
        ctx$api,
        state,
        "callback_after_new_environment_warmup_complete",
        clear
    )
    api__host_callback(
        ctx$api,
        state,
        "callback_begin_zone_timestep_before_init_heat_balance",
        function(state) {
            date <- ctx$call("dayOfYear", list(entry$pointer))
            if (!is.null(clock$date) && !identical(clock$date, date)) {
                clock$offset <- clock$offset + 24
            }
            clock$date <- date
            invisible(NULL)
        }
    )
    invisible(NULL)
}

# Supply native-equivalent elapsed hours only when this host observed the entire lifecycle.
api_legacy__current_sim_time <- function(ctx, entry) {
    clock <- entry$relative_clock
    if (is.null(clock) || !isTRUE(clock$active) || !entry$running) {
        api__error(
            "current_sim_time requires an observed owned simulation lifecycle on this EnergyPlus release."
        )
    }
    clock$offset + ctx$call("currentTime", list(entry$pointer))
}

# Supply the selected IDD explicitly where the old library cannot set its installation root.
api_legacy__run_args <- function(ctx, entry, args) {
    if (numeric_version(ctx$manifest$energyplus) >= "9.6") {
        ctx$call(
            "setEnergyPlusRootDirectory",
            list(entry$pointer, ctx$directory)
        )
        return(args)
    }
    if (
        any(
            args %in%
                c(
                    "-m",
                    "--epmacro",
                    "-x",
                    "--expandobjects",
                    "-r",
                    "--readvars"
                )
        )
    ) {
        api__error(
            "This EnergyPlus release cannot locate selected-installation helpers through its API; prepare preprocessing and reporting outside the native run."
        )
    }
    if (!any(args %in% c("-i", "--idd")) && !any(grepl("^--idd=", args))) {
        args <- c("--idd", file.path(ctx$directory, "Energy+.idd"), args)
    }
    args
}

# Delay an early stop until the reporting arrays exist, without discarding the first condition.
api_legacy__stop <- function(ctx, entry) {
    if (
        numeric_version(ctx$manifest$energyplus) <= "22.1" &&
            !isTRUE(entry$stop_safe)
    ) {
        entry$deferred_stop <- TRUE
    } else {
        ctx$call("stopSimulation", list(entry$pointer))
    }
    invisible(NULL)
}

# The protected boundary must still run after user delivery was disabled by a callback condition.
api_legacy__stop_tick <- function(ctx, entry, method) {
    if (
        numeric_version(ctx$manifest$energyplus) > "22.1" ||
            method != "callback_end_zone_timestep_after_zone_reporting"
    ) {
        return(invisible(NULL))
    }
    if (
        ctx$call("kindOfSim", list(entry$pointer)) == 3L &&
            ctx$call("warmupFlag", list(entry$pointer)) == 0L
    ) {
        entry$stop_safe <- TRUE
        if (isTRUE(entry$deferred_stop)) {
            entry$deferred_stop <- FALSE
            ctx$call("stopSimulation", list(entry$pointer))
        }
    }
    invisible(NULL)
}
