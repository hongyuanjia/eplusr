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
