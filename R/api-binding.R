# Map a reviewed ABI family to porter-generated declarations, never to handwritten signatures.
api_bind__select <- function(abi) {
    stopifnot(identical(abi, "26.1-26.2"))
    list(
        dynport = "EnergyPlus-26.1.dynport",
        api_version = "0.2",
        pointer_bytes = 8L,
        api_data_fields = c("what", "name", "key", "type", "unit")
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
            "Cannot load the selected EnergyPlus API library into this R process."
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
    loaded_version <- call("energyPlusVersion")
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
