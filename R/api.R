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
#' `list_available_api_data_csv()` returns a character string reconstructed from
#' that safely released data, without calling the unfreeable native CSV allocator.
#' Its `plugin_only` column identifies global/trend methods available only to
#' executable-hosted R plugins. Their handles remain state- and kind-bound.
#' Missing globals return a negative handle without entering the native fatal path;
#' negative plugin handles cannot be used for reading or writing. Trend aggregates
#' require `count >= 2`, matching stock 26.1. `get_trend_value()` remains disabled:
#' the native upper index check permits an out-of-bounds read.
#'
#' `callback_register_external_hvac_manager()` uses the same callback safety
#' boundary as other runtime hooks, but replaces the native HVAC manager entirely.
#' This is an experimental engine interface, not a general HVAC control hook;
#' the caller must supply its HVAC calculations. Native initialization is limited.
#' `clear_callbacks()` remains disabled because stock 26.1 has no unregister API;
#' resetting or deleting the state retires callbacks safely.
#'
#' `rdyncall` is optional and is loaded only here. A build containing its native
#' callback-boundary fix (PR 75), existing-handle dynport loading and signature
#' metadata are required. A quarantined R process checks error,
#' interrupt and return-conversion behavior; package version alone is insufficient
#' until the fix has an official release. Start a fresh R process if rdyncall
#' was already loaded by other code. EnergyPlus is loaded lazily and its
#' version/API identity is checked. Do not serialize live API objects to workers.
#'
#' `close()` deletes states and resources before unloading the library. A
#' finalizer is a fallback. Close/reset/delete and nested runs are rejected during
#' simulation. Original 26.1 library leaks are not repaired: use a fresh R process
#' for each long-lived simulation lifecycle. PythonPlugin models and clone are outside this scope. Job integration uses
#' `backend = "api"`; [eplus_session()] provides live worker stepping and pause.
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
api__running$plugin_depth <- 0L
api__bridge_cache <- new.env(parent = emptyenv())

# Throw a conventional R condition before an invalid request can reach native code.
api__error <- function(message) {
    stop(structure(
        list(message = message, call = NULL),
        class = c("eplusr_api_error", "error", "condition")
    ))
}

# Define reviewed management policy; function signatures and layouts come from the dynport.
api__manifest <- function() {
    list(
        energyplus = "26.1",
        api_version = "0.2",
        pointer_bytes = 8L,
        methods = list(
            api__rule(
                "state_manager",
                "new_state",
                "stateNew",
                kind = "new_state",
                ownership = "state"
            ),
            api__rule(
                "state_manager",
                "reset_state",
                "stateReset",
                parameters = "state",
                kind = "reset_state",
                ownership = "reset"
            ),
            api__rule(
                "state_manager",
                "delete_state",
                "stateDelete",
                parameters = "state",
                kind = "delete_state",
                ownership = "delete"
            ),
            api__rule(
                "glycol",
                "delete",
                "glycolDelete",
                parameters = "state",
                kind = "delete_resource",
                ownership = "resource_delete"
            ),
            api__rule(
                "glycol",
                "specific_heat",
                "glycolSpecificHeat",
                parameters = "state, temperature"
            ),
            api__rule(
                "glycol",
                "density",
                "glycolDensity",
                parameters = "state, temperature"
            ),
            api__rule(
                "glycol",
                "conductivity",
                "glycolConductivity",
                parameters = "state, temperature"
            ),
            api__rule(
                "glycol",
                "viscosity",
                "glycolViscosity",
                parameters = "state, temperature"
            ),
            api__rule(
                "refrigerant",
                "delete",
                "refrigerantDelete",
                parameters = "state",
                kind = "delete_resource",
                ownership = "resource_delete"
            ),
            api__rule(
                "refrigerant",
                "saturation_pressure",
                "refrigerantSaturationPressure",
                parameters = "state, temperature"
            ),
            api__rule(
                "refrigerant",
                "saturation_temperature",
                "refrigerantSaturationTemperature",
                parameters = "state, pressure"
            ),
            api__rule(
                "refrigerant",
                "saturated_enthalpy",
                "refrigerantSaturatedEnthalpy",
                parameters = "state, temperature, quality"
            ),
            api__rule(
                "refrigerant",
                "saturated_density",
                "refrigerantSaturatedDensity",
                parameters = "state, temperature, quality"
            ),
            api__rule(
                "refrigerant",
                "saturated_specific_heat",
                "refrigerantSaturatedSpecificHeat",
                parameters = "state, temperature, quality"
            ),
            api__rule(
                "psychrometrics",
                "density",
                "psyRhoFnPbTdbW",
                parameters = "state, barometric_pressure, dry_bulb_temp, humidity_ratio"
            ),
            api__rule(
                "psychrometrics",
                "latent_energy_of_air",
                "psyHfgAirFnWTdb",
                parameters = "state, dry_bulb_temp"
            ),
            api__rule(
                "psychrometrics",
                "latent_energy_of_moisture_in_air",
                "psyHgAirFnWTdb",
                parameters = "state, dry_bulb_temp"
            ),
            api__rule(
                "psychrometrics",
                "enthalpy",
                "psyHFnTdbW",
                parameters = "state, dry_bulb_temp, humidity_ratio"
            ),
            api__rule(
                "psychrometrics",
                "enthalpy_b",
                "psyHFnTdbRhPb",
                parameters = "state, dry_bulb_temp, relative_humidity_fraction, barometric_pressure"
            ),
            api__rule(
                "psychrometrics",
                "specific_heat",
                "psyCpAirFnW",
                parameters = "state, humidity_ratio"
            ),
            api__rule(
                "psychrometrics",
                "dry_bulb",
                "psyTdbFnHW",
                parameters = "state, enthalpy, humidity_ratio"
            ),
            api__rule(
                "psychrometrics",
                "vapor_density",
                "psyRhovFnTdbWPb",
                parameters = "state, dry_bulb_temp, humidity_ratio, barometric_pressure"
            ),
            api__rule(
                "psychrometrics",
                "relative_humidity",
                "psyRhFnTdbRhov",
                parameters = "state, dry_bulb_temp, vapor_density"
            ),
            api__rule(
                "psychrometrics",
                "relative_humidity_b",
                "psyRhFnTdbWPb",
                parameters = "state, dry_bulb_temp, humidity_ratio, barometric_pressure"
            ),
            api__rule(
                "psychrometrics",
                "wet_bulb",
                "psyTwbFnTdbWPb",
                parameters = "state, dry_bulb_temp, humidity_ratio, barometric_pressure"
            ),
            api__rule(
                "psychrometrics",
                "specific_volume",
                "psyVFnTdbWPb",
                parameters = "state, dry_bulb_temp, humidity_ratio, barometric_pressure"
            ),
            api__rule(
                "psychrometrics",
                "saturation_pressure",
                "psyPsatFnTemp",
                parameters = "state, dry_bulb_temp"
            ),
            api__rule(
                "psychrometrics",
                "saturation_temperature",
                "psyTsatFnHPb",
                parameters = "state, enthalpy, barometric_pressure"
            ),
            api__rule(
                "psychrometrics",
                "vapor_density_b",
                "psyRhovFnTdbRh",
                parameters = "state, dry_bulb_temp, relative_humidity_fraction"
            ),
            api__rule(
                "psychrometrics",
                "humidity_ratio",
                "psyWFnTdbH",
                parameters = "state, dry_bulb_temp, enthalpy"
            ),
            api__rule(
                "psychrometrics",
                "humidity_ratio_b",
                "psyWFnTdpPb",
                parameters = "state, dew_point_temp, barometric_pressure"
            ),
            api__rule(
                "psychrometrics",
                "humidity_ratio_c",
                "psyWFnTdbRhPb",
                parameters = "state, dry_bulb_temp, relative_humidity_fraction, barometric_pressure"
            ),
            api__rule(
                "psychrometrics",
                "humidity_ratio_d",
                "psyWFnTdbTwbPb",
                parameters = "state, dry_bulb_temp, wet_bulb_temp, barometric_pressure"
            ),
            api__rule(
                "psychrometrics",
                "dew_point",
                "psyTdpFnWPb",
                parameters = "state, humidity_ratio, barometric_pressure"
            ),
            api__rule(
                "psychrometrics",
                "dew_point_b",
                "psyTdpFnTdbTwbPb",
                parameters = "state, dry_bulb_temp, wet_bulb_temp, barometric_pressure"
            ),
            api__rule(
                "functional",
                "initialize",
                "initializeFunctionalAPI",
                parameters = "state",
                kind = "initialize"
            ),
            api__rule(
                "functional",
                "glycol",
                "glycolNew",
                parameters = "state, glycol_name",
                kind = "factory",
                ownership = "resource"
            ),
            api__rule(
                "functional",
                "refrigerant",
                "refrigerantNew",
                parameters = "state, refrigerant_name",
                kind = "factory",
                ownership = "resource"
            ),
            api__rule(
                "functional",
                "psychrometrics",
                "",
                parameters = "state",
                kind = "factory",
                ownership = "resource"
            ),
            api__rule(
                "functional",
                "ep_version",
                "energyPlusVersion",
                ownership = "static_string"
            ),
            api__rule(
                "functional",
                "callback_error",
                "registerErrorCallback",
                parameters = "state, f",
                kind = "callback",
                callback = "iZ)v",
                ownership = "callback"
            ),
            api__rule(
                "functional",
                "clear_callbacks",
                "",
                reason = "No native unregister function; reset or delete the owning state instead."
            ),
            api__rule(
                "runtime",
                "run_energyplus",
                "energyplus",
                parameters = "state, command_line_args",
                kind = "run",
                ownership = "run"
            ),
            api__rule(
                "runtime",
                "stop_simulation",
                "stopSimulation",
                parameters = "state"
            ),
            api__rule(
                "runtime",
                "set_console_output_status",
                "setConsoleOutputState",
                parameters = "state, print_output"
            ),
            api__rule(
                "runtime",
                "issue_warning",
                "issueWarning",
                parameters = "state, message"
            ),
            api__rule(
                "runtime",
                "issue_severe",
                "issueSevere",
                parameters = "state, message"
            ),
            api__rule(
                "runtime",
                "issue_text",
                "issueText",
                parameters = "state, message"
            ),
            api__rule(
                "runtime",
                "callback_progress",
                "registerProgressCallback",
                parameters = "state, f",
                kind = "callback",
                callback = "i)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_message",
                "registerStdOutCallback",
                parameters = "state, f",
                kind = "callback",
                callback = "Z)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_begin_new_environment",
                "callbackBeginNewEnvironment",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_after_new_environment_warmup_complete",
                "callbackAfterNewEnvironmentWarmupComplete",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_begin_zone_timestep_before_init_heat_balance",
                "callbackBeginZoneTimeStepBeforeInitHeatBalance",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_begin_zone_timestep_after_init_heat_balance",
                "callbackBeginZoneTimeStepAfterInitHeatBalance",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_begin_system_timestep_before_predictor",
                "callbackBeginTimeStepBeforePredictor",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_begin_zone_timestep_before_set_current_weather",
                "callbackBeginZoneTimestepBeforeSetCurrentWeather",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_after_predictor_before_hvac_managers",
                "callbackAfterPredictorBeforeHVACManagers",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_after_predictor_after_hvac_managers",
                "callbackAfterPredictorAfterHVACManagers",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_inside_system_iteration_loop",
                "callbackInsideSystemIterationLoop",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_end_zone_timestep_before_zone_reporting",
                "callbackEndOfZoneTimeStepBeforeZoneReporting",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_end_zone_timestep_after_zone_reporting",
                "callbackEndOfZoneTimeStepAfterZoneReporting",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_end_system_timestep_before_hvac_reporting",
                "callbackEndOfSystemTimeStepBeforeHVACReporting",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_end_system_timestep_after_hvac_reporting",
                "callbackEndOfSystemTimeStepAfterHVACReporting",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_end_zone_sizing",
                "callbackEndOfZoneSizing",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_end_system_sizing",
                "callbackEndOfSystemSizing",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_after_component_get_input",
                "callbackEndOfAfterComponentGetInput",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_user_defined_component_model",
                "callbackUserDefinedComponentModel",
                parameters = "state, f, program_name",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_unitary_system_sizing",
                "callbackUnitarySystemSizing",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "callback_register_external_hvac_manager",
                "registerExternalHVACManager",
                parameters = "state, f",
                kind = "callback",
                callback = "p)v",
                ownership = "callback"
            ),
            api__rule(
                "runtime",
                "clear_callbacks",
                "",
                reason = "No native unregister function; reset or delete the owning state instead."
            ),
            api__rule(
                "exchange",
                "get_api_data",
                "getAPIData",
                parameters = "state",
                kind = "api_data",
                requires_ready = TRUE,
                ownership = "api_data"
            ),
            api__rule(
                "exchange",
                "list_available_api_data_csv",
                "getAPIData",
                parameters = "state",
                kind = "api_data_csv",
                requires_ready = TRUE,
                ownership = "api_data"
            ),
            api__rule(
                "exchange",
                "api_data_fully_ready",
                "apiDataFullyReady",
                parameters = "state",
                boolean = TRUE
            ),
            api__rule(
                "exchange",
                "api_error_flag",
                "apiErrorFlag",
                parameters = "state",
                boolean = TRUE
            ),
            api__rule(
                "exchange",
                "reset_api_error_flag",
                "resetErrorFlag",
                parameters = "state"
            ),
            api__rule(
                "exchange",
                "get_input_file_path",
                "inputFilePath",
                parameters = "state",
                reason = "Allocated char[] has no public deleter in stock 26.1.",
                requires_ready = TRUE,
                ownership = "allocated_string"
            ),
            api__rule(
                "exchange",
                "get_weather_file_path",
                "epwFilePath",
                parameters = "state",
                reason = "Allocated char[] has no public deleter in stock 26.1.",
                requires_ready = TRUE,
                ownership = "allocated_string"
            ),
            api__rule(
                "exchange",
                "get_object_names",
                "getObjectNames",
                parameters = "state, object_type_name",
                reason = "Stock freeObjectNames does not release the allocated name strings.",
                requires_ready = TRUE,
                ownership = "object_names"
            ),
            api__rule(
                "exchange",
                "get_num_nodes_in_cond_fd_surf_layer",
                "getNumNodesInCondFDSurfaceLayer",
                parameters = "state, surface_name, material_name",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "request_variable",
                "requestVariable",
                parameters = "state, variable_name, variable_key",
                ownership = "request"
            ),
            api__rule(
                "exchange",
                "get_variable_handle",
                "getVariableHandle",
                parameters = "state, variable_name, variable_key",
                handle_kind = "variable",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_meter_handle",
                "getMeterHandle",
                parameters = "state, meter_name",
                handle_kind = "meter",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_actuator_handle",
                "getActuatorHandle",
                parameters = "state, component_type, control_type, actuator_key",
                handle_kind = "actuator",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_variable_value",
                "getVariableValue",
                parameters = "state, variable_handle",
                input_handle_kind = "variable",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_meter_value",
                "getMeterValue",
                parameters = "state, meter_handle",
                input_handle_kind = "meter",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "set_actuator_value",
                "setActuatorValue",
                parameters = "state, actuator_handle, actuator_value",
                input_handle_kind = "actuator",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "reset_actuator",
                "resetActuator",
                parameters = "state, actuator_handle",
                input_handle_kind = "actuator",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_actuator_value",
                "getActuatorValue",
                parameters = "state, actuator_handle",
                input_handle_kind = "actuator",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_internal_variable_handle",
                "getInternalVariableHandle",
                parameters = "state, variable_type, variable_key",
                handle_kind = "internal_variable",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_internal_variable_value",
                "getInternalVariableValue",
                parameters = "state, variable_handle",
                input_handle_kind = "internal_variable",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_construction_handle",
                "getConstructionHandle",
                parameters = "state, var_name",
                handle_kind = "construction",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_ems_global_handle",
                "getEMSGlobalVariableHandle",
                parameters = "state, var_name",
                handle_kind = "ems_global",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_ems_global_value",
                "getEMSGlobalVariableValue",
                parameters = "state, handle",
                input_handle_kind = "ems_global",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "set_ems_global_value",
                "setEMSGlobalVariableValue",
                parameters = "state, handle, value",
                input_handle_kind = "ems_global",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_global_handle",
                "getPluginGlobalVariableHandle",
                parameters = "state, var_name",
                plugin_only = TRUE,
                handle_kind = "plugin_global",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_global_value",
                "getPluginGlobalVariableValue",
                parameters = "state, handle",
                plugin_only = TRUE,
                input_handle_kind = "plugin_global",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "set_global_value",
                "setPluginGlobalVariableValue",
                parameters = "state, handle, value",
                plugin_only = TRUE,
                input_handle_kind = "plugin_global",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_trend_handle",
                "getPluginTrendVariableHandle",
                parameters = "state, trend_var_name",
                plugin_only = TRUE,
                handle_kind = "plugin_trend",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_trend_value",
                "getPluginTrendVariableValue",
                parameters = "state, trend_handle, time_index",
                reason = "Stock 26.1 indexes trend history out of bounds at time_index = history size; no public history-size query.",
                plugin_only = TRUE,
                input_handle_kind = "plugin_trend",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_trend_average",
                "getPluginTrendVariableAverage",
                parameters = "state, trend_handle, count",
                plugin_only = TRUE,
                input_handle_kind = "plugin_trend",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_trend_min",
                "getPluginTrendVariableMin",
                parameters = "state, trend_handle, count",
                plugin_only = TRUE,
                input_handle_kind = "plugin_trend",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_trend_max",
                "getPluginTrendVariableMax",
                parameters = "state, trend_handle, count",
                plugin_only = TRUE,
                input_handle_kind = "plugin_trend",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_trend_sum",
                "getPluginTrendVariableSum",
                parameters = "state, trend_handle, count",
                plugin_only = TRUE,
                input_handle_kind = "plugin_trend",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "get_trend_direction",
                "getPluginTrendVariableDirection",
                parameters = "state, trend_handle, count",
                plugin_only = TRUE,
                input_handle_kind = "plugin_trend",
                requires_ready = TRUE
            ),
            api__rule("exchange", "year", "year", parameters = "state"),
            api__rule(
                "exchange",
                "calendar_year",
                "calendarYear",
                parameters = "state"
            ),
            api__rule("exchange", "month", "month", parameters = "state"),
            api__rule(
                "exchange",
                "day_of_month",
                "dayOfMonth",
                parameters = "state"
            ),
            api__rule("exchange", "hour", "hour", parameters = "state"),
            api__rule(
                "exchange",
                "current_time",
                "currentTime",
                parameters = "state"
            ),
            api__rule("exchange", "minutes", "minutes", parameters = "state"),
            api__rule(
                "exchange",
                "num_time_steps_in_hour",
                "numTimeStepsInHour",
                parameters = "state"
            ),
            api__rule(
                "exchange",
                "zone_time_step_number",
                "zoneTimeStepNum",
                parameters = "state"
            ),
            api__rule(
                "exchange",
                "day_of_week",
                "dayOfWeek",
                parameters = "state"
            ),
            api__rule(
                "exchange",
                "day_of_year",
                "dayOfYear",
                parameters = "state"
            ),
            api__rule(
                "exchange",
                "daylight_savings_time_indicator",
                "daylightSavingsTimeIndicator",
                parameters = "state",
                boolean = TRUE
            ),
            api__rule(
                "exchange",
                "holiday_index",
                "holidayIndex",
                parameters = "state"
            ),
            api__rule(
                "exchange",
                "sun_is_up",
                "sunIsUp",
                parameters = "state",
                boolean = TRUE
            ),
            api__rule(
                "exchange",
                "is_raining",
                "isRaining",
                parameters = "state",
                boolean = TRUE
            ),
            api__rule(
                "exchange",
                "warmup_flag",
                "warmupFlag",
                parameters = "state",
                boolean = TRUE
            ),
            api__rule(
                "exchange",
                "zone_time_step",
                "zoneTimeStep",
                parameters = "state"
            ),
            api__rule(
                "exchange",
                "system_time_step",
                "systemTimeStep",
                parameters = "state"
            ),
            api__rule(
                "exchange",
                "current_environment_num",
                "currentEnvironmentNum",
                parameters = "state"
            ),
            api__rule(
                "exchange",
                "actual_time",
                "actualTime",
                parameters = "state"
            ),
            api__rule(
                "exchange",
                "actual_date_time",
                "actualDateTime",
                parameters = "state"
            ),
            api__rule(
                "exchange",
                "kind_of_sim",
                "kindOfSim",
                parameters = "state"
            ),
            api__rule(
                "exchange",
                "today_weather_is_raining_at_time",
                "todayWeatherIsRainAtTime",
                parameters = "state, hour, time_step_number",
                boolean = TRUE,
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "today_weather_is_snowing_at_time",
                "todayWeatherIsSnowAtTime",
                parameters = "state, hour, time_step_number",
                boolean = TRUE,
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "today_weather_outdoor_dry_bulb_at_time",
                "todayWeatherOutDryBulbAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "today_weather_outdoor_dew_point_at_time",
                "todayWeatherOutDewPointAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "today_weather_outdoor_barometric_pressure_at_time",
                "todayWeatherOutBarometricPressureAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "today_weather_outdoor_relative_humidity_at_time",
                "todayWeatherOutRelativeHumidityAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "today_weather_wind_speed_at_time",
                "todayWeatherWindSpeedAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "today_weather_wind_direction_at_time",
                "todayWeatherWindDirectionAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "today_weather_sky_temperature_at_time",
                "todayWeatherSkyTemperatureAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "today_weather_horizontal_ir_at_time",
                "todayWeatherHorizontalIRSkyAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "today_weather_beam_solar_at_time",
                "todayWeatherBeamSolarRadiationAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "today_weather_diffuse_solar_at_time",
                "todayWeatherDiffuseSolarRadiationAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "today_weather_albedo_at_time",
                "todayWeatherAlbedoAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "today_weather_liquid_precipitation_at_time",
                "todayWeatherLiquidPrecipitationAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "tomorrow_weather_is_raining_at_time",
                "tomorrowWeatherIsRainAtTime",
                parameters = "state, hour, time_step_number",
                boolean = TRUE,
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "tomorrow_weather_is_snowing_at_time",
                "tomorrowWeatherIsSnowAtTime",
                parameters = "state, hour, time_step_number",
                boolean = TRUE,
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "tomorrow_weather_outdoor_dry_bulb_at_time",
                "tomorrowWeatherOutDryBulbAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "tomorrow_weather_outdoor_dew_point_at_time",
                "tomorrowWeatherOutDewPointAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "tomorrow_weather_outdoor_barometric_pressure_at_time",
                "tomorrowWeatherOutBarometricPressureAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "tomorrow_weather_outdoor_relative_humidity_at_time",
                "tomorrowWeatherOutRelativeHumidityAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "tomorrow_weather_wind_speed_at_time",
                "tomorrowWeatherWindSpeedAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "tomorrow_weather_wind_direction_at_time",
                "tomorrowWeatherWindDirectionAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "tomorrow_weather_sky_temperature_at_time",
                "tomorrowWeatherSkyTemperatureAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "tomorrow_weather_horizontal_ir_at_time",
                "tomorrowWeatherHorizontalIRSkyAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "tomorrow_weather_beam_solar_at_time",
                "tomorrowWeatherBeamSolarRadiationAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "tomorrow_weather_diffuse_solar_at_time",
                "tomorrowWeatherDiffuseSolarRadiationAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "tomorrow_weather_albedo_at_time",
                "tomorrowWeatherAlbedoAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "tomorrow_weather_liquid_precipitation_at_time",
                "tomorrowWeatherLiquidPrecipitationAtTime",
                parameters = "state, hour, time_step_number",
                requires_ready = TRUE
            ),
            api__rule(
                "exchange",
                "current_sim_time",
                "currentSimTime",
                parameters = "state"
            )
        )
    )
}

# Normalize one Python-compatible method contract without duplicating C ABI declarations.
api__rule <- function(
    group,
    method,
    symbol,
    parameters = "",
    kind = "ordinary",
    reason = "",
    callback = "",
    boolean = FALSE,
    plugin_only = FALSE,
    handle_kind = "",
    requires_ready = FALSE,
    input_handle_kind = "",
    ownership = "scalar"
) {
    owners <- list(
        state = "owned state; managed stateDelete before library unload",
        reset = "stateReset; invalidates children, requests and callbacks",
        delete = "stateDelete; retires resources and retained callbacks",
        resource_delete = "R view invalidation; stock 26.1 public native deleter is a no-op",
        scalar = "scalar return or copied argument; no caller-owned native allocation",
        resource = "state/generation-owned view; native allocations may survive public deletion",
        static_string = "borrowed static string copied to R character",
        callback = "retained trampoline and R function until stateReset/stateDelete",
        run = "argv and callbacks retained for synchronous run; state survives normal return",
        api_data = "owned array copied to R then freeAPIData",
        allocated_string = "allocated char[]; no public deleter; unsupported",
        object_names = "allocated array/name strings; incomplete public deleter; unsupported",
        request = "argument strings copied by engine; requests retained until reset/delete"
    )
    list(
        group = group,
        method = method,
        symbol = symbol,
        parameters = if (nzchar(parameters)) {
            strsplit(parameters, ", ", fixed = TRUE)[[1L]]
        } else {
            character()
        },
        status = if (nzchar(reason)) "unsupported" else "supported",
        reason = reason,
        kind = kind,
        callback = callback,
        boolean = boolean,
        plugin_only = plugin_only,
        handle_kind = handle_kind,
        requires_ready = requires_ready,
        input_handle_kind = input_handle_kind,
        ownership = owners[[ownership]]
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
    if (!"libhandle" %in% names(formals(rdyncall::dynport_load_into))) {
        api__error(
            "Install a rdyncall build supporting existing-handle dynport loading."
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
        # Coverage-instrumented cold startup needs more time than the callback checks.
        callr::r(probe, libpath = .libPaths(), timeout = 120),
        error = function(e) {
            api__error(paste0(
                "rdyncall safety probe failed: ",
                conditionMessage(e)
            ))
        }
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

# Load generated bindings and identify the selected library before exposing managed methods.
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
    # Reuse the selected handle: the dynport must never search for another installation.
    native <- new.env(parent = baseenv())
    rdyncall::dynport_load_into(
        system.file("dynports", "EnergyPlus-26.1.dynport", package = "eplusr"),
        native,
        libhandle = library
    )
    signatures <- lapply(as.list(native), function(binding) {
        attr(binding, "signature")
    })
    for (name in needed) {
        if (!is.function(native[[name]]) || is.null(signatures[[name]])) {
            api__error(paste("Missing required EnergyPlus symbol:", name))
        }
    }
    # The generated typeinfo owns the ABI layout; R only decides how its data is copied.
    layout <- native$APIDataEntry
    if (!inherits(layout, "typeinfo") || !is.data.frame(layout$fields)) {
        api__error("The EnergyPlus dynport lacks the APIDataEntry layout.")
    }
    # Dispatch managed calls through the generated C wrappers.
    call <- function(symbol, arguments = list()) {
        # R method names follow Python; generated wrappers use the C header names.
        do.call(native[[symbol]], unname(arguments))
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
    ctx$signatures <- signatures
    ctx$api_data <- layout
    ctx$call <- call
    ctx$ffi <- ffi
    api <- api__construct(ctx)
    success <- TRUE
    api
}

# Construct managed groups above the real/fake native-call seam used by lifecycle tests.
api__construct <- function(ctx) {
    # Attach ABI metadata from generated wrappers without storing another signature table.
    ctx$manifest$methods <- lapply(ctx$manifest$methods, function(spec) {
        spec$signature <- if (nzchar(spec$symbol)) {
            ctx$signatures[[spec$symbol]]
        } else {
            ""
        }
        if (is.null(spec$signature)) {
            spec$signature <- ""
        }
        spec
    })
    ctx$pid <- Sys.getpid()
    ctx$closed <- FALSE
    ctx$next_id <- 0L
    ctx$states <- new.env(parent = emptyenv())
    ctx$quarantined <- FALSE
    ctx$native_depth <- 0L
    ctx$plugin_depth <- 0L
    ctx$plugin_mode <- FALSE
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
        status <- list(
            phase = entry$phase,
            generation = entry$generation,
            stop_reason = entry$stop_reason,
            exit_code = entry$exit_code
        )
        if (length(entry$plugins)) {
            status$plugins <- plugin__status(entry)
        }
        status
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
                    is.null(api__running$state) &&
                    api__running$plugin_depth == 0L
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
    entry$plugins <- list()
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
                    ctx$native_depth == 0L &&
                    api__running$plugin_depth == 0L
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
        entry$running ||
            ctx$native_depth > 0L ||
            ctx$plugin_depth > 0L ||
            api__running$plugin_depth > 0L ||
            !is.null(api__running$state)
    ) {
        api__error(
            "Cannot delete a state while EnergyPlus or an R plugin is running."
        )
    }
    # User finalizers run only at an R boundary, before native resources disappear.
    plugin__finish(ctx, entry)
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
        entry$running ||
            ctx$native_depth > 0L ||
            ctx$plugin_depth > 0L ||
            api__running$plugin_depth > 0L ||
            !is.null(api__running$state)
    ) {
        api__error(
            "Cannot reset a state while EnergyPlus or an R plugin is running."
        )
    }
    plugin__finish(ctx, entry)
    # The public invocation boundary reports this once after retiring the failed state.
    if (!is.null(entry$condition)) {
        return(invisible(NULL))
    }
    entry$plugins <- list()
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
            ctx$plugin_depth > 0L ||
            api__running$plugin_depth > 0L ||
            !is.null(api__running$state) ||
            any(vapply(entries, function(x) x$running, logical(1)))
    ) {
        api__error("Cannot close an API while EnergyPlus is running.")
    }
    condition <- NULL
    for (entry in entries) {
        api__dispose(ctx, entry)
        if (is.null(condition)) condition <- entry$condition
    }
    ctx$ffi$unload()
    ctx$closed <- TRUE
    # Report cleanup failure only after every state and the library have been drained.
    if (!is.null(condition)) {
        stop(condition)
    }
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
    # Typed pointers occupy one native argument, including pointers to named structs.
    arguments <- strsplit(signature, ")", fixed = TRUE)[[1L]][1L]
    arguments <- gsub("\\*<[^>]+>|\\*[A-Za-z]", "p", arguments)
    types <- strsplit(
        arguments,
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
    if (
        kind %in% c("plugin_global", "plugin_trend") && as.integer(value) < 0L
    ) {
        api__error(
            "Plugin variable was not found; cannot use a negative handle."
        )
    }
    as.integer(value)
}

# Retain the first callback condition and its stop category at every safe return boundary.
api__retain_callback_failure <- function(entry, condition) {
    if (is.null(condition)) {
        return(invisible(NULL))
    }
    entry$condition <- entry$condition %||% condition
    entry$stop_reason <- if (inherits(entry$condition, "interrupt")) {
        "interrupt"
    } else {
        "callback_error"
    }
    invisible(NULL)
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
            if (
                diagnostic$foreign == 0 &&
                    identical(diagnostic$bridge$disable_reason, "r_interrupt")
            ) {
                return(structure(
                    list(message = message, call = NULL),
                    class = c("interrupt", "condition")
                ))
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
    # Return normally from the R callback so no condition unwinds an active C++ frame.
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
        # Retain the first condition and request a native stop without deleting the running state.
        capture <- function(condition) {
            api__retain_callback_failure(entry, condition)
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
    fields <- ctx$api_data$fields$name
    rows <- lapply(seq_len(count[1L]), function(i) {
        vapply(
            seq_along(fields),
            function(j) {
                offset <- as.integer(
                    (i - 1L) * ctx$api_data$size + ctx$api_data$fields$offset[j]
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

# Reconstruct the stock CSV from safely copied APIDataEntry rows, avoiding its unfreeable char[].
api__data_csv <- function(data) {
    sections <- list(
        ACTUATORS = c("Actuator", "name", "type", "key", "unit"),
        INTERNAL_VARIABLES = c("InternalVariable", "name", "key", "unit"),
        PLUGIN_GLOBAL_VARIABLES = c("PluginGlobalVariable", "key"),
        TRENDS = c("PluginTrendVariable", "key"),
        METERS = c("OutputMeter", "key", "unit"),
        VARIABLES = c("OutputVariable", "name", "key", "unit")
    )
    # Stock 26.1 appends a comma to trend entry kinds; its CSV spells the kind normally.
    kinds <- sub(",$", "", data$what)
    lines <- unlist(
        lapply(names(sections), function(name) {
            spec <- sections[[name]]
            rows <- data[kinds == spec[[1L]], spec[-1L], drop = FALSE]
            values <- if (nrow(rows)) {
                vapply(
                    seq_len(nrow(rows)),
                    function(i) {
                        paste(
                            c(
                                spec[[1L]],
                                unlist(
                                    rows[i, , drop = FALSE],
                                    use.names = FALSE
                                )
                            ),
                            collapse = ","
                        )
                    },
                    character(1L)
                )
            } else {
                character()
            }
            c(paste0("**", name, "**"), values)
        }),
        use.names = FALSE
    )
    paste0(paste(lines, collapse = "\n"), "\n")
}

# Execute one native lifecycle, retaining argv/callbacks and reporting failures only after cleanup.
api__run <- function(ctx, entry, args) {
    if (!is.character(args) || anyNA(args) || !length(args)) {
        api__error("command_line_args must be a non-missing character vector.")
    }
    if (
        !is.null(api__running$state) ||
            api__running$plugin_depth > 0L ||
            entry$phase != "created" ||
            ctx$native_depth > 0L ||
            ctx$plugin_depth > 0L
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
        api__retain_callback_failure(entry, api__callback_failure(ctx, entry))
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
    if (!is.null(entry$condition)) {
        entry$phase <- "failed"
    }
    plugin__finish(ctx, entry)
    if (!is.null(entry$condition)) {
        condition <- entry$condition
        api__dispose(ctx, entry)
        stop(condition)
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
    if (spec$plugin_only && !ctx$plugin_mode) {
        api__error("This method requires an executable-hosted R plugin state.")
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
                    api__retain_callback_failure(
                        entry,
                        api__callback_failure(ctx, entry)
                    )
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
                if (ctx$plugin_depth == 0L) {
                    api__dispose(ctx, entry)
                }
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
        api__dispose(ctx, entry)
        if (!is.null(entry$condition)) {
            stop(entry$condition)
        }
        return(invisible(NULL))
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
    if (spec$kind == "api_data_csv") {
        return(api__data_csv(api__data(ctx, entry)))
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
    # Reject missing globals before the native lookup, which fatally terminates stock 26.1.
    if (spec$method == "get_global_handle") {
        data <- api__data(ctx, entry)
        names <- data$key[data$what == "PluginGlobalVariable"]
        # The engine folds bytes, not Unicode case; pass its exact stored spelling to the fatal lookup.
        lower <- "abcdefghijklmnopqrstuvwxyz"
        upper <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        index <- match(
            chartr(lower, upper, arguments[[2L]]),
            chartr(lower, upper, names)
        )
        if (is.na(index)) {
            return(api__handle(-1L, ctx, entry, spec$handle_kind))
        }
        arguments[[2L]] <- names[[index]]
    }
    # Stock aggregate functions require at least two historical values, including min/max/sum.
    if (spec$input_handle_kind == "plugin_trend" && arguments[[3L]] < 2L) {
        api__error("Stock 26.1 trend aggregates require count >= 2.")
    }
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

#' Control a live EnergyPlus simulation
#'
#' Create an experimental session proxy for stock EnergyPlus 26.1. Native state,
#' handles and callbacks stay in one fresh engine worker. Pausing preserves that
#' worker and the current native call; it is not a checkpoint or state copy.
#'
#' @param model An IDF path or [Idf] object, as accepted by [eplus_job()].
#' @param weather An EPW path or [Epw] object. Sessions control weather run periods.
#' @param eplus EnergyPlus version or installation directory. Only 26.1 is supported.
#' @param variables Named list of character pairs `c(variable_name, key)`.
#' @param actuators Named list of character triples `c(component_type, control_type, key)`.
#' @param initial_action Optional named numeric actuator values for the first
#'   weather step. Without it, the first step retains the model's native control.
#' @param controller Optional `function(observation, controller_state)` or path to
#'   an R script defining `controller`. It must return `list(action = ..., state = ...)`.
#'   Scripts are sourced once in the engine worker; this is a library-hosted
#'   controller, not an EnergyPlus executable plugin loader.
#' @param controller_state Initial serializable controller state.
#' @param dir Output directory. A new temporary directory is used by default.
#' @param timeout Maximum seconds to wait for a command or worker startup.
#'   Expiry terminates the session; it never advances a waiting simulation.
#' @param record Retain immutable input copies and a disk journal of completed
#'   observations and effective actuator actions for replay prediction. Default FALSE.
#' @param sample_every Positive number of completed zone timesteps between
#'   published snapshots during continuous running. Default 1. Controllers and
#'   replay recording still collect every step; publication follows this interval.
#' @return An `EnergyPlusSession` environment with the following methods:
#'   * `$initialize(timeout = ...)`: run through the first completed weather zone
#'     step and return its observation while paused. Initialization happens once.
#'   * `$observe(timeout = ..., wait = TRUE)`: with `wait = TRUE`, return the paused
#'     observation or request the next completed running boundary. With `wait = FALSE`,
#'     read the latest published snapshot immediately without sending a worker command.
#'     Return NULL before initialization; repeated reads can return the same step.
#'   * `$step(action = NULL, n = 1L, timeout = ...)`: advance a paused session by
#'     `n` zone steps, then pause. Named numeric actions take effect before the
#'     next predictor; `NA_real_` releases the named actuator to native control.
#'   * `$pause(timeout = ...)`: wait until the next completed zone boundary and pause.
#'   * `$resume(action = NULL, timeout = ...)`: continue asynchronously until
#'     completion or a pause request. An installed controller runs at each boundary.
#'   * `$predict(branches, horizon, tolerance = 1e-8, timeout = ...,
#'     history_limit = 128 * 1024^2, dir = NULL)`: from a recorded paused session,
#'     replay history in independent workers and predict future zone steps.
#'     `branches` is a uniquely named list of action lists (length `horizon`)
#'     or `function(observation, controller_state)` controllers.
#'   * `$status()`: inspect phase, worker identity, last observation and final outcome.
#'   * `$result(timeout = ...)`: wait for a running session's final [EplusJob].
#'     A paused session must first be resumed or closed.
#'   * `$close(timeout = 10)`: request an orderly native stop and reap the pipeline.
#'     A finalizer terminates the process tree as a fallback.
#' @details
#' Observations contain named `values`, explicit `controller_state`, completed
#' weather `step`, `environment`, `kind_of_sim`, calendar fields, zone-step index
#' and the interval `[time_start, time_end]` in hours. `year` is the simulation
#' calendar year; `weather_year` retains the EPW year. `callback` identifies the
#' sampling point. Output-variable averaging follows EnergyPlus's variable
#' definition: for example, Zone Mean Air Temperature is a zone-step average,
#' not an instantaneous endpoint measurement. Warmup and sizing are excluded.
#'
#' The first returned observation already represents one completed zone step.
#' A multi-step action is held until the next action or release. With a controller,
#' each subsequent step computes its action from the last completed observation
#' and explicit state; manual actions and controller operation cannot be mixed.
#' Publication retains at most two complete snapshots, independently of replay
#' recording. A slow or absent reader never holds up the worker; if an open file
#' prevents rotation, that publication is skipped. Snapshot `step`
#' and simulation interval identify freshness; intermediate unpublished steps are
#' not retained. Sampling and file publication have a cost, even though reading
#' requires no engine acknowledgement. Explicit control commands
#' retain their exact boundary, regardless of `sample_every`. At natural completion,
#' the latest sample can precede the last step; normal sampling lags by fewer
#' than `sample_every` steps, while skipped publications can lag further;
#' final outcome `completed_steps` records the actual number of completed steps.
#' The finalized Job's configured output reports are separate from this cache.
#' By default no unbounded per-step history is retained. On natural completion the last collected
#' observation and finalized Job remain available. Errors, caller interrupts,
#' timeout and disconnection stop or terminate the worker; a failed native state
#' cannot be resumed. Closing early is distinct from completing the run period.
#'
#' With `record = TRUE`, historical observations, explicit controller state and
#' effective actions are journaled to disk; no native pointers are saved. Prediction
#' freezes a prefix ending at the paused boundary, replays its actions without
#' re-running the original controller, and checks every registered observation
#' and calendar field before applying future branch controls. Future function
#' controllers start with the original boundary's explicit controller state.
#' A NULL future action holds prior actuator claims; NA releases them.
#' Each branch uses a fresh worker, executes sequentially and is closed before
#' the next branch. Results contain ordinary observations, worker identity,
#' elapsed time, replay/future step counts and `method = "replay"`.
#' `tolerance` is an absolute bound for each registered numeric value.
#' An early natural end returns a shorter horizon with `complete = FALSE`.
#' Errors or caller interrupts abort prediction and close its worker while the
#' original session remains paused. A single timeout budget covers each branch.
#' Input and library fingerprints are rechecked; journal loading is bounded by
#' `history_limit`. Journaling consumes disk and replay cost grows with history.
#' Matching exposed observations does not establish equality of hidden native
#' state. Randomness, external side effects and unrecorded inputs can diverge.
#' This is deterministic-input replay, not clone, checkpoint or saved native state.
#'
#' Models, external resources and SQL outputs use the existing Job pipeline.
#' PythonPlugin models and persistent restart/state cloning are outside this scope.
#' Live sessions cannot be captured by another worker controller or serialized
#' as a way of resuming a simulation in a different process.
#' @examples
#' \dontrun{
#' session <- eplus_session(
#'     "model.idf", "weather.epw",
#'     variables = list(temperature = c("Zone Mean Air Temperature", "ZONE ONE"))
#' )
#' observation <- session$initialize()
#' observation <- session$step(n = 4L)
#' session$resume()
#' job <- session$result()
#' session$close()
#'
#' # With a prepared model whose heating schedule is named Heating:
#' session <- eplus_session(
#'     "model.idf", "weather.epw", record = TRUE,
#'     variables = list(temperature = c("Zone Mean Air Temperature", "ZONE ONE")),
#'     actuators = list(heating = c("Schedule:Constant", "Schedule Value", "Heating"))
#' )
#' session$initialize()
#' session$step(c(heating = 21), n = 4L)
#' prediction <- session$predict(
#'     list(hold = rep(list(NULL), 4L), warm = rep(list(c(heating = 23)), 4L)),
#'     horizon = 4L
#' )
#' # The original is still paused; apply a chosen action independently.
#' session$step(c(heating = 23))
#' session$close()
#' }
#' @export
eplus_session <- function(
    model,
    weather,
    eplus = "26.1",
    variables = list(),
    actuators = list(),
    initial_action = NULL,
    controller = NULL,
    controller_state = NULL,
    dir = NULL,
    timeout = 120,
    record = FALSE,
    sample_every = 1L
) {
    checkmate::assert_flag(record)
    checkmate::assert_int(sample_every, lower = 1L)
    config <- if (
        is.character(eplus) && length(eplus) == 1L && is_eplus_path(eplus)
    ) {
        suppressMessages(use_eplus(eplus))
    } else {
        suppressMessages(eplus_config(eplus))
    }
    run__api_version(config$version)
    api_session__timeout(timeout)
    variables <- api_session__definitions(variables, 2L, "variables")
    actuators <- api_session__definitions(actuators, 3L, "actuators")
    initial_action <- api_session__action(initial_action, names(actuators))
    api_session__plain(controller_state)
    # Snapshot ordinary state so its caller environment cannot retain the public proxy.
    controller_state <- unserialize(serialize(controller_state, NULL))
    if (is.character(controller)) {
        controller <- normalizePath(checkmate::assert_file_exists(
            controller,
            "r"
        ))
    } else if (!is.null(controller)) {
        checkmate::assert_function(controller, nargs = 2L)
        run__check_setup(controller)
        controller <- run__prepare_setup(controller)
    }
    if (is.null(weather)) {
        api_session__error("A session requires weather run periods.")
    }
    job <- eplus_job(model, weather)
    run__api_version(job$version())
    ctx <- new.env(parent = emptyenv())
    ctx$config <- list(
        installation = normalizePath(config$dir),
        variables = variables,
        actuators = actuators,
        initial_action = initial_action,
        controller = controller,
        controller_state = controller_state,
        timeout = timeout,
        record = record,
        sample_every = as.integer(sample_every),
        snapshot_dir = tempfile("eplusr-snapshots-"),
        directory = normalizePath(
            dir %||% tempfile("eplusr-session-"),
            mustWork = FALSE
        )
    )
    ctx$job <- job
    ctx$id <- basename(tempfile("session-proxy-"))
    ctx$owner_pid <- Sys.getpid()
    ctx$phase <- "created"
    ctx$outcome <- NULL
    ctx$error <- NULL
    ctx$observation <- NULL
    ctx$identity <- NULL
    ctx$process <- NULL
    ctx$server <- NULL
    ctx$connection <- NULL
    ctx$request <- 0L
    ctx$terminal <- FALSE
    ctx$replay <- NULL
    ctx$journal_hash <- NULL
    if (record) {
        api_replay__freeze(ctx)
    }
    api_session__construct(ctx)
}

# Keep process-local proxy identities so deserialization cannot acquire an existing live session.
api_session__registry <- new.env(parent = emptyenv())

# Reject copied or cross-process proxies before touching process handles or connection numbers.
api_session__owner <- function(ctx) {
    if (!api_session__is_owner(ctx)) {
        api_session__error(
            "A live session proxy belongs to its original R process and cannot be restored from serialization."
        )
    }
    invisible(NULL)
}

# Share one ownership predicate between rejecting operations and the silent GC fallback.
api_session__is_owner <- function(ctx) {
    identical(ctx$owner_pid, Sys.getpid()) &&
        exists(ctx$id, api_session__registry, inherits = FALSE) &&
        identical(get(ctx$id, api_session__registry, inherits = FALSE), ctx)
}

# Use ordinary R conditions so caller-side failures never enter a native callback stack.
api_session__error <- function(message) {
    stop(structure(
        list(message = message, call = NULL),
        class = c(
            "eplusr_session_error",
            "eplusr_api_error",
            "error",
            "condition"
        )
    ))
}

# Validate budgets before launching or sending a request, leaving invalid calls recoverable.
api_session__timeout <- function(timeout) {
    checkmate::assert_number(timeout, lower = 0, finite = TRUE)
    if (timeout == 0) {
        api_session__error("'timeout' must be positive.")
    }
    invisible(NULL)
}

# Validate names and positional SDK identifiers before starting any worker.
api_session__definitions <- function(definitions, size, argument) {
    checkmate::assert_list(definitions)
    if (!length(definitions)) {
        return(definitions)
    }
    if (
        is.null(names(definitions)) ||
            anyNA(names(definitions)) ||
            any(!nzchar(names(definitions))) ||
            anyDuplicated(names(definitions))
    ) {
        api_session__error(paste0(
            "'",
            argument,
            "' requires unique nonempty names."
        ))
    }
    for (value in definitions) {
        checkmate::assert_character(
            value,
            len = size,
            any.missing = FALSE,
            min.chars = 1L
        )
    }
    definitions
}

# Require finite named actions; NA explicitly releases a previously claimed actuator.
api_session__action <- function(action, names) {
    api_session__plain(action)
    if (is.null(action)) {
        return(NULL)
    }
    checkmate::assert_numeric(action, any.missing = TRUE)
    if (
        is.null(names(action)) ||
            anyNA(names(action)) ||
            any(!nzchar(names(action))) ||
            anyDuplicated(names(action)) ||
            any(!names(action) %in% names) ||
            any(is.infinite(action)) ||
            any(is.nan(action))
    ) {
        api_session__error(
            "Actions require known, unique actuator names and finite values or NA."
        )
    }
    action
}

# Reuse the native-resource exclusion when serializing explicit controller state.
api_session__plain <- function(value) {
    inspect <- function() value
    run__check_setup(inspect)
    invisible(NULL)
}

# Bind one private context without retaining the public proxy through a finalizer closure.
api_session__bind <- function(fun, ctx) {
    environment(fun) <- list2env(list(ctx = ctx), parent = environment(fun))
    formals(fun) <- formals(fun)[-1L]
    fun
}

# Build the session proxy; native objects remain inaccessible to the caller.
api_session__construct <- function(ctx) {
    assign(ctx$id, ctx, envir = api_session__registry)
    session <- new.env(parent = emptyenv())
    class(session) <- "EnergyPlusSession"
    methods <- list(
        initialize = api_session__initialize,
        observe = api_session__observe,
        step = api_session__step,
        pause = api_session__pause,
        resume = api_session__resume,
        status = api_session__status,
        result = api_session__result,
        close = api_session__close,
        predict = api_replay__predict
    )
    for (name in names(methods)) {
        session[[name]] <- api_session__bind(methods[[name]], ctx)
    }
    reg.finalizer(
        session,
        api_session__bind(api_session__finalize, ctx),
        onexit = TRUE
    )
    lockEnvironment(session, bindings = TRUE)
    session
}

# Frame small ordinary R messages so a socket never transfers native handles or pointers.
api_session__send <- function(connection, message) {
    payload <- serialize(message, NULL, xdr = TRUE)
    if (length(payload) > 8L * 1024L * 1024L) {
        api_session__error("Session message exceeds 8 MiB.")
    }
    writeBin(as.integer(length(payload)), connection, endian = "big")
    writeBin(payload, connection)
    flush(connection)
    invisible(NULL)
}

# Read a complete bounded frame; an empty header means the peer has disconnected.
api_session__receive <- function(connection) {
    size <- readBin(connection, integer(), n = 1L, endian = "big")
    if (!length(size)) {
        return(NULL)
    }
    if (size < 1L || size > 8L * 1024L * 1024L) {
        api_session__error("Invalid session message size.")
    }
    payload <- readBin(connection, raw(), n = size)
    if (length(payload) != size) {
        api_session__error("Incomplete session message.")
    }
    message <- unserialize(payload)
    if (!is.list(message)) {
        api_session__error("Invalid session message.")
    }
    message
}

# A writable sentinel bypasses Windows R's 200 ms empty-select floor without treating writability as input.
api_session__readable <- function(connection) {
    socketSelect(
        list(connection, connection),
        write = c(FALSE, TRUE),
        timeout = 0
    )[[1L]]
}

# Reserve a listening socket atomically rather than choosing an already-used free port.
api_session__listen <- function() {
    # Transport bookkeeping must not perturb a caller's controller RNG stream.
    start <- (Sys.getpid() +
        as.integer((as.numeric(Sys.time()) %% 1) * 100000)) %%
        50000L
    for (offset in seq_len(20L)) {
        port <- 10000L + (start + offset) %% 50000L
        server <- tryCatch(serverSocket(port), error = function(e) NULL)
        if (!is.null(server)) return(list(server = server, port = port))
    }
    api_session__error("Unable to reserve a local session socket.")
}

# Materialize and launch the existing Job pipeline, then wait for its first completed weather step.
api_session__initialize <- function(ctx, timeout = ctx$config$timeout) {
    api_session__owner(ctx)
    api_session__timeout(timeout)
    if (ctx$phase != "created") {
        api_session__error("A session can only be initialized once.")
    }
    dir.create(ctx$config$snapshot_dir, mode = "0700")
    listener <- api_session__listen()
    ctx$server <- listener$server
    settings <- ctx$config
    settings$port <- listener$port
    settings$token <- paste0(
        basename(tempfile("session-")),
        basename(tempfile("session-"))
    )
    setup <- function(api, state) api_session__setup(api, state, settings)
    environment(setup) <- list2env(
        list(settings = settings),
        parent = asNamespace("eplusr")
    )
    ctx$phase <- "initializing"
    api_session__guard(ctx, {
        # Select the constructor's frozen installation immediately before dispatch resolves its path.
        suppressMessages(use_eplus(ctx$config$installation))
        ctx$job$run(
            dir = ctx$config$directory,
            wait = FALSE,
            echo = FALSE,
            copy_external = TRUE,
            readvars = FALSE,
            backend = setup
        )
        ctx$process <- get_priv_env(ctx$job)$m_job
        deadline <- Sys.time() + timeout
        while (
            is.null(ctx$connection) &&
                ctx$process$is_alive() &&
                Sys.time() < deadline
        ) {
            if (socketSelect(list(ctx$server), timeout = 0.05)) {
                connection <- socketAccept(
                    ctx$server,
                    blocking = TRUE,
                    open = "a+b",
                    timeout = 2
                )
                # Authenticate bounded raw bytes before decoding an R object from the accepted socket.
                expected <- charToRaw(settings$token)
                preface <- readBin(connection, raw(), n = length(expected))
                if (!identical(preface, expected)) {
                    close(connection)
                    next
                }
                hello <- api_session__receive(connection)
                if (!is.null(hello) && identical(hello$type, "hello")) {
                    ctx$connection <- connection
                    ctx$identity <- hello$identity
                    ctx$identity$pipeline_pid <- ctx$process$get_pid()
                } else {
                    close(connection)
                }
            }
        }
        if (is.null(ctx$connection)) {
            if (!ctx$process$is_alive()) {
                api_session__collect(ctx)
            }
            api_session__error(
                api_session__diagnostic(
                    ctx,
                    "Session worker startup timed out."
                )
            )
        }
        close(ctx$server)
        ctx$server <- NULL
        api_session__wait(
            ctx,
            0L,
            max(
                0.01,
                as.numeric(difftime(deadline, Sys.time(), units = "secs"))
            )
        )
    })
}

# Validate and issue a serialized command, then await its boundary acknowledgement.
api_session__command <- function(
    ctx,
    operation,
    timeout,
    action = NULL,
    n = NULL
) {
    api_session__timeout(timeout)
    ctx$request <- ctx$request + 1L
    api_session__guard(ctx, {
        # Natural completion can race with a running observe/pause/close request.
        # A readable terminal/EOF lets the regular wait path collect the finalized Job.
        send <- function() {
            api_session__send(
                ctx$connection,
                list(
                    id = ctx$request,
                    operation = operation,
                    action = action,
                    n = n
                )
            )
        }
        if (
            !ctx$terminal && !is.null(ctx$connection) && ctx$process$is_alive()
        ) {
            tryCatch(suppressWarnings(send()), error = function(condition) {
                if (
                    ctx$process$is_alive() &&
                        !api_session__readable(ctx$connection)
                ) {
                    stop(condition)
                }
            })
        }
        api_session__wait(ctx, ctx$request, timeout)
    })
}

# Any caller interrupt or protocol failure reaps the whole pipeline before rethrowing its condition.
api_session__guard <- function(ctx, expression) {
    tryCatch(
        expression,
        error = function(condition) {
            api_session__fail(ctx, condition)
            stop(condition)
        },
        interrupt = function(condition) {
            api_session__fail(ctx, condition)
            stop(condition)
        }
    )
}

# Wait only for the requested acknowledgement, retaining the same worker during a pause.
api_session__wait <- function(ctx, id, timeout) {
    deadline <- Sys.time() + timeout
    while (Sys.time() < deadline) {
        if (
            !is.null(ctx$connection) &&
                socketSelect(list(ctx$connection), timeout = 0.05)
        ) {
            message <- api_session__receive(ctx$connection)
            if (is.null(message)) {
                close(ctx$connection)
                ctx$connection <- NULL
            } else if (identical(message$type, "terminal")) {
                ctx$observation <- message$observation
                ctx$journal_hash <- message$journal_hash
                ctx$terminal <- TRUE
            } else if (
                identical(message$id, id) && identical(message$type, "boundary")
            ) {
                ctx$phase <- message$phase
                ctx$observation <- message$observation
                ctx$journal_hash <- message$journal_hash
                return(ctx$observation)
            } else {
                api_session__error("Unexpected session response.")
            }
        }
        if (!ctx$process$is_alive()) {
            api_session__collect(ctx)
            if (ctx$phase == "failed") {
                api_session__error(
                    api_session__diagnostic(ctx, "Session simulation failed.")
                )
            }
            return(ctx$observation)
        }
        if (is.null(ctx$connection)) Sys.sleep(0.05)
    }
    api_session__error(
        "Session command timed out; the worker will be terminated."
    )
}

# Collect finalized pipeline diagnostics without inventing native cleanup on an abrupt process exit.
api_session__collect <- function(ctx) {
    # Natural completion may precede a caller query; recover its final buffered snapshot.
    while (
        !is.null(ctx$connection) &&
            api_session__readable(ctx$connection)
    ) {
        message <- api_session__receive(ctx$connection)
        if (is.null(message)) {
            close(ctx$connection)
            ctx$connection <- NULL
            break
        }
        if (!identical(message$type, "terminal")) {
            api_session__error("Unexpected final session response.")
        }
        ctx$observation <- message$observation
        ctx$journal_hash <- message$journal_hash
        ctx$terminal <- TRUE
    }
    status <- ctx$job$status()
    ctx$outcome <- status$api
    ctx$error <- status$api$error
    ctx$phase <- if (isTRUE(status$successful)) {
        "completed"
    } else if (identical(status$api$phase, "stopped")) {
        "stopped"
    } else {
        "failed"
    }
    invisible(NULL)
}

# Retain last completed measurements while reporting abrupt caller-side termination honestly.
api_session__fail <- function(ctx, condition) {
    if (!is.null(ctx$process) && ctx$process$is_alive()) {
        # Use Job cancellation so its final public result records termination too.
        try(ctx$job$kill(), silent = TRUE)
        if (ctx$process$is_alive()) run__api_kill(ctx$process)
    }
    message <- conditionMessage(condition)
    if (
        !is.character(message) ||
            length(message) != 1L ||
            is.na(message) ||
            !nzchar(message)
    ) {
        message <- if (inherits(condition, "interrupt")) {
            "Session interrupted."
        } else {
            "Session protocol failed."
        }
    }
    if (ctx$phase != "failed" || is.null(ctx$error)) {
        ctx$error <- list(class = class(condition), message = message)
    } else if (!nzchar(ctx$error$message %||% "")) {
        # Keep the native condition class while adding a usable transport-side diagnostic.
        ctx$error$message <- message
    }
    if (!ctx$phase %in% c("failed", "completed", "stopped")) {
        ctx$phase <- "terminated"
    }
    api_session__disconnect(ctx)
    invisible(NULL)
}

# Close only R IPC resources; native cleanup belongs to the engine worker.
api_session__disconnect <- function(ctx) {
    for (field in c("connection", "server")) {
        if (!is.null(ctx[[field]])) {
            try(close(ctx[[field]]), silent = TRUE)
        }
        ctx[[field]] <- NULL
    }
    invisible(NULL)
}

# Inspect completion locally; this method never advances a paused native simulation.
api_session__status <- function(ctx) {
    api_session__owner(ctx)
    if (
        !is.null(ctx$process) &&
            !ctx$process$is_alive() &&
            !ctx$phase %in%
                c("closed", "terminated", "completed", "failed", "stopped")
    ) {
        api_session__collect(ctx)
    }
    list(
        phase = ctx$phase,
        alive = !is.null(ctx$process) && ctx$process$is_alive(),
        identity = ctx$identity,
        observation = ctx$observation,
        outcome = ctx$outcome,
        error = ctx$error
    )
}

# Publish immutable files with unique step names; rotation never waits for a reader.
api_session__publish <- function(settings, control) {
    directory <- settings$snapshot_dir
    path <- file.path(directory, sprintf("%010d.rds", control$observation$step))
    if (file.exists(path)) {
        return(invisible(NULL))
    }
    # Rotate before writing, retaining two slots even when Windows cannot remove an open file.
    previous <- control$snapshot_files
    retired <- utils::tail(previous, -1L)
    unlink(retired)
    if (any(file.exists(retired))) {
        return(invisible(NULL))
    }
    temporary <- file.path(directory, "pending")
    saveRDS(control$observation, temporary, compress = FALSE)
    if (!file.rename(temporary, path)) {
        api_session__error("Cannot publish session snapshot.")
    }
    # Retain a previous complete file while a reader races with publication/rotation.
    control$snapshot_files <- c(path, utils::head(previous, 1L))
    invisible(NULL)
}

# Read only complete published files; a rotation race falls back to the caller's last snapshot.
api_session__latest <- function(ctx) {
    if (!ctx$phase %in% c("running", "terminated", "failed")) {
        return(ctx$observation)
    }
    paths <- sort(
        list.files(
            ctx$config$snapshot_dir,
            pattern = "^[0-9]{10}\\.rds$",
            full.names = TRUE
        ),
        decreasing = TRUE
    )
    for (path in paths) {
        observation <- tryCatch(
            suppressWarnings(readRDS(path)),
            error = function(e) NULL
        )
        if (
            !is.null(observation) &&
                (is.null(ctx$observation) ||
                    observation$step >= ctx$observation$step)
        ) {
            ctx$observation <- observation
            break
        }
    }
    ctx$observation
}

# Return a stable paused observation, or sample the next completed running boundary.
api_session__observe <- function(
    ctx,
    timeout = ctx$config$timeout,
    wait = TRUE
) {
    api_session__owner(ctx)
    checkmate::assert_flag(wait)
    if (!wait) {
        return(api_session__latest(ctx))
    }
    phase <- api_session__status(ctx)$phase
    if (phase == "running") {
        return(api_session__command(ctx, "observe", timeout))
    }
    if (phase %in% c("paused", "completed", "stopped", "closed")) {
        return(ctx$observation)
    }
    api_session__error("The session has no available observation.")
}

# Advance an exact positive number of weather zone steps from a paused boundary.
api_session__step <- function(
    ctx,
    action = NULL,
    n = 1L,
    timeout = ctx$config$timeout
) {
    if (api_session__status(ctx)$phase != "paused") {
        api_session__error("Stepping requires a paused session.")
    }
    checkmate::assert_int(n, lower = 1L)
    action <- api_session__action(action, names(ctx$config$actuators))
    if (!is.null(action) && !is.null(ctx$config$controller)) {
        api_session__error(
            "Manual actions cannot be combined with a controller."
        )
    }
    api_session__command(ctx, "step", timeout, action, as.integer(n))
}

# Pause a running simulation at its next completed zone boundary; repeated pause is harmless.
api_session__pause <- function(ctx, timeout = ctx$config$timeout) {
    phase <- api_session__status(ctx)$phase
    if (phase %in% c("paused", "completed", "stopped")) {
        return(ctx$observation)
    }
    if (phase != "running") {
        api_session__error("Pausing requires a running session.")
    }
    api_session__command(ctx, "pause", timeout)
}

# Release a paused boundary without resetting native state, then return immediately after acknowledgement.
api_session__resume <- function(
    ctx,
    action = NULL,
    timeout = ctx$config$timeout
) {
    if (api_session__status(ctx)$phase != "paused") {
        api_session__error("Resuming requires a paused session.")
    }
    action <- api_session__action(action, names(ctx$config$actuators))
    if (!is.null(action) && !is.null(ctx$config$controller)) {
        api_session__error(
            "Manual actions cannot be combined with a controller."
        )
    }
    api_session__command(ctx, "resume", timeout, action)
}

# Expose the existing finalized Job only after the live session has left its native call.
api_session__result <- function(ctx, timeout = ctx$config$timeout) {
    api_session__timeout(timeout)
    phase <- api_session__status(ctx)$phase
    if (phase %in% c("paused", "created", "initializing")) {
        api_session__error(
            "Resume or close the session before collecting its result."
        )
    }
    if (phase == "running") {
        api_session__guard(ctx, api_session__wait(ctx, -1L, timeout))
    }
    ctx$job
}

# Stop at a control boundary and wait for cleanup, falling back to bounded process-tree termination.
api_session__close <- function(ctx, timeout = 10) {
    api_session__owner(ctx)
    if (ctx$phase == "closed") {
        return(invisible(TRUE))
    }
    if (!is.null(ctx$process) && ctx$process$is_alive()) {
        api_session__command(ctx, "close", timeout)
        api_session__guard(ctx, api_session__wait(ctx, -1L, timeout))
    }
    api_session__disconnect(ctx)
    unlink(ctx$config$snapshot_dir, recursive = TRUE)
    ctx$phase <- "closed"
    invisible(TRUE)
}

# A collected or exiting proxy cannot leave its paused engine or descendants behind.
api_session__finalize <- function(ctx, session) {
    # A copied proxy has no authority over the original process or its R connection identifiers.
    if (!api_session__is_owner(ctx)) {
        return(invisible(NULL))
    }
    rm(list = ctx$id, envir = api_session__registry)
    if (!is.null(ctx$process) && ctx$process$is_alive()) {
        try(run__api_kill(ctx$process), silent = TRUE)
    }
    api_session__disconnect(ctx)
    unlink(ctx$config$snapshot_dir, recursive = TRUE)
    invisible(NULL)
}

# Reconstruct the controller once in the engine process and retain the authenticated IPC connection.
api_session__setup <- function(api, state, settings) {
    control <- new.env(parent = emptyenv())
    control$connection <- socketConnection(
        "127.0.0.1",
        settings$port,
        blocking = TRUE,
        open = "a+b",
        timeout = 2
    )
    # Prove this is the private worker before the owner deserializes any IPC payload.
    writeBin(charToRaw(settings$token), control$connection)
    flush(control$connection)
    api_session__send(
        control$connection,
        list(
            type = "hello",
            identity = c(
                api$info()[c("version", "api_version", "library", "pid")],
                list(generation = api$state_manager$status(state)$generation)
            )
        )
    )
    control$mode <- "initializing"
    control$step <- 0L
    control$remaining <- 0L
    control$need_control <- FALSE
    control$id <- 0L
    control$observation <- NULL
    control$snapshot_files <- character()
    control$action <- settings$initial_action
    control$journal_hash <- NULL
    control$controller_state <- settings$controller_state
    control$controller <- settings$controller
    control$variable_handles <- NULL
    control$actuator_handles <- NULL
    if (is.character(control$controller)) {
        scope <- new.env(parent = globalenv())
        sys.source(control$controller, envir = scope)
        if (!exists("controller", scope, inherits = FALSE)) {
            api_session__error("Controller script must define 'controller'.")
        }
        control$controller <- get("controller", scope, inherits = FALSE)
        checkmate::assert_function(control$controller, nargs = 2L)
    }
    for (definition in settings$variables) {
        api$exchange$request_variable(state, definition[1L], definition[2L])
    }
    # Apply controls before predictor reads schedules, repeating through system substeps of a zone step.
    if (length(settings$actuators) || !is.null(control$controller)) {
        api$runtime$callback_begin_system_timestep_before_predictor(
            state,
            function(s) {
                if (!api_session__weather(api, s)) {
                    return(invisible(NULL))
                }
                api_session__handles(api, s, settings, control)
                # Compute a controller action only when a new zone step actually reaches its predictor.
                # This avoids advancing controller memory after the final simulated step.
                if (control$need_control && !is.null(control$observation)) {
                    api_session__control(settings, control)
                    control$need_control <- FALSE
                }
                for (name in names(control$action)) {
                    handle <- control$actuator_handles[[name]]
                    if (is.na(control$action[[name]])) {
                        api$exchange$reset_actuator(s, handle)
                    } else {
                        api$exchange$set_actuator_value(
                            s,
                            handle,
                            control$action[[name]]
                        )
                    }
                }
                invisible(NULL)
            }
        )
    }
    # Each completed weather zone step is a safe waiting point within the same native run.
    api$runtime$callback_end_zone_timestep_after_zone_reporting(
        state,
        function(s) {
            if (!api_session__weather(api, s)) {
                return(invisible(NULL))
            }
            api_session__handles(api, s, settings, control)
            control$step <- control$step + 1L
            # Sparse running skips variable reads and calendar construction, but never control or replay.
            due <- control$step %% settings$sample_every == 0L
            if (
                control$mode != "running" ||
                    due ||
                    !is.null(control$controller) ||
                    isTRUE(settings$record)
            ) {
                control$observation <- api_session__snapshot(
                    api,
                    s,
                    settings,
                    control
                )
            }
            if (due) {
                api_session__publish(settings, control)
            }
            if (isTRUE(settings$record)) {
                # Commit the actual held action before exposing its completed boundary.
                control$journal_hash <- api_replay__write(
                    settings$journal,
                    control$observation,
                    control$action,
                    control$journal_hash
                )
            }
            if (control$mode == "stepping") {
                control$remaining <- control$remaining - 1L
            }
            if (
                control$mode == "initializing" ||
                    (control$mode == "stepping" && control$remaining == 0L)
            ) {
                control$mode <- "paused"
                api_session__publish(settings, control)
                api_session__reply(control, control$id)
            }
            repeat {
                ready <- if (control$mode == "paused") {
                    socketSelect(list(control$connection), timeout = 0.05)
                } else {
                    api_session__readable(control$connection)
                }
                if (ready) {
                    command <- api_session__receive(control$connection)
                    if (is.null(command)) {
                        api$runtime$stop_simulation(s)
                        control$mode <- "disconnected"
                        break
                    }
                    # A command arriving after the initial poll still gets this exact boundary.
                    if (control$observation$step != control$step) {
                        control$observation <- api_session__snapshot(
                            api,
                            s,
                            settings,
                            control
                        )
                    }
                    api_session__dispatch(api, s, settings, control, command)
                }
                if (control$mode != "paused") break
            }
            if (control$mode %in% c("running", "stepping")) {
                control$need_control <- TRUE
            }
            invisible(NULL)
        }
    )
    # The package engine calls this only after native return and its public cleanup.
    finish <- function(metadata) {
        try(
            api_session__send(
                control$connection,
                list(
                    type = "terminal",
                    observation = control$observation,
                    journal_hash = control$journal_hash,
                    metadata = metadata
                )
            ),
            silent = TRUE
        )
        try(close(control$connection), silent = TRUE)
        invisible(NULL)
    }
    # Distinguish a deliberate close and a lost client without rewriting the native stop classification.
    metadata <- function(value) {
        value$completed_steps <- control$step
        value$session_reason <- switch(
            control$mode,
            closing = "user_close",
            disconnected = "connection_lost",
            value$stop_reason %||% value$phase
        )
        value
    }
    structure(
        list(finish = finish, metadata = metadata),
        class = "EnergyPlusSessionBridge"
    )
}

# Sessions participate only after API readiness in non-warmup weather simulations.
api_session__weather <- function(api, state) {
    api$exchange$api_data_fully_ready(state) &&
        !api$exchange$warmup_flag(state) &&
        api$exchange$kind_of_sim(state) == 3L
}

# Resolve typed handles once per engine lifecycle, entirely inside its worker.
api_session__handles <- function(api, state, settings, control) {
    if (!is.null(control$variable_handles)) {
        return(invisible(NULL))
    }
    control$variable_handles <- lapply(settings$variables, function(d) {
        api$exchange$get_variable_handle(state, d[1L], d[2L])
    })
    control$actuator_handles <- lapply(settings$actuators, function(d) {
        api$exchange$get_actuator_handle(state, d[1L], d[2L], d[3L])
    })
    # SDK handle lookup returns -1 for an unknown binding, even before the first write.
    if (any(vapply(control$variable_handles, as.integer, integer(1L)) < 0L)) {
        api_session__error("Unknown session output variable or key.")
    }
    if (any(vapply(control$actuator_handles, as.integer, integer(1L)) < 0L)) {
        api_session__error("Unknown session actuator or key.")
    }
    invisible(NULL)
}

# Report explicit completed-zone metadata without representing an average as an instantaneous sample.
api_session__snapshot <- function(api, state, settings, control) {
    exchange <- api$exchange
    # current_time() can retain an HVAC substep offset after zone reporting.
    # The hour and zone index identify the completed zone boundary itself.
    end <- exchange$hour(state) +
        exchange$zone_time_step_number(state) /
            exchange$num_time_steps_in_hour(state)
    list(
        step = control$step,
        environment = exchange$current_environment_num(state),
        kind_of_sim = exchange$kind_of_sim(state),
        year = exchange$calendar_year(state),
        weather_year = exchange$year(state),
        month = exchange$month(state),
        day = exchange$day_of_month(state),
        hour = exchange$hour(state),
        zone_time_step_number = exchange$zone_time_step_number(state),
        time_start = end - exchange$zone_time_step(state),
        time_end = end,
        callback = "end_zone_timestep_after_zone_reporting",
        values = vapply(
            control$variable_handles,
            function(handle) exchange$get_variable_value(state, handle),
            numeric(1L)
        ),
        variables = settings$variables,
        controller_state = control$controller_state
    )
}

# Acknowledge a control boundary with ordinary data; no native state crosses IPC.
api_session__reply <- function(control, id) {
    api_session__send(
        control$connection,
        list(
            type = "boundary",
            id = id,
            phase = if (control$mode == "paused") "paused" else "running",
            observation = control$observation,
            journal_hash = control$journal_hash
        )
    )
}

# Interpret one validated request at a zone boundary, holding the native stack while paused.
api_session__dispatch <- function(api, state, settings, control, command) {
    if (
        !is.numeric(command$id) ||
            length(command$id) != 1L ||
            !command$operation %in%
                c("step", "resume", "pause", "observe", "close")
    ) {
        api_session__error("Invalid session command.")
    }
    if (command$operation %in% c("step", "resume")) {
        if (control$mode != "paused") {
            api_session__error("The engine is not paused.")
        }
        action <- api_session__action(command$action, names(settings$actuators))
        if (!is.null(action)) {
            control$action <- api_session__merge_action(control$action, action)
        }
        control$id <- command$id
        control$mode <- if (command$operation == "step") {
            "stepping"
        } else {
            "running"
        }
        if (command$operation == "step") {
            checkmate::assert_int(command$n, lower = 1L)
            control$remaining <- command$n
        } else {
            api_session__reply(control, command$id)
        }
    } else if (command$operation == "pause") {
        control$mode <- "paused"
        api_session__reply(control, command$id)
    } else if (command$operation == "observe") {
        api_session__reply(control, command$id)
    } else {
        api$runtime$stop_simulation(state)
        control$mode <- "closing"
        api_session__reply(control, command$id)
    }
    invisible(NULL)
}

# Advance explicit controller memory only when the next simulation step is authorized.
api_session__control <- function(settings, control) {
    if (is.null(control$controller)) {
        return(invisible(NULL))
    }
    result <- control$controller(control$observation, control$controller_state)
    if (!is.list(result) || !all(c("action", "state") %in% names(result))) {
        api_session__error(
            "A controller must return list(action = ..., state = ...)."
        )
    }
    api_session__plain(result$state)
    action <- api_session__action(result$action, names(settings$actuators))
    if (!is.null(action)) {
        control$action <- api_session__merge_action(control$action, action)
    }
    control$controller_state <- result$state
    invisible(NULL)
}

# Keep unspecified actuator claims while replacing only the explicitly supplied values or releases.
api_session__merge_action <- function(current, action) {
    c(current[setdiff(names(current), names(action))], action)
}

# Use meaningful diagnostics for native interrupts whose R condition message is empty.
api_session__diagnostic <- function(ctx, fallback) {
    message <- ctx$error$message
    if (
        is.character(message) &&
            length(message) == 1L &&
            !is.na(message) &&
            nzchar(message)
    ) {
        return(message)
    }
    if (identical(ctx$outcome$stop_reason, "interrupt")) {
        return("EnergyPlus worker interrupted.")
    }
    fallback
}

# Preserve the exact model, weather and external resources before the first native run.
api_replay__freeze <- function(ctx) {
    root <- tempfile("eplusr-replay-")
    inputs <- file.path(root, "inputs")
    dir.create(inputs, recursive = TRUE)
    private <- get_priv_env(ctx$job)
    source <- private$m_model_store$load_model(private$m_model_store$case_model_ids()[[
        1L
    ]])
    dependencies <- as.character(source$external_deps())
    if (any(!file.exists(dependencies))) {
        api_session__error(
            "Replay requires all declared external resources to exist."
        )
    }
    dependencies <- unique(normalizePath(dependencies, mustWork = TRUE))
    reserved <- tolower(basename(dependencies))
    if (anyDuplicated(reserved)) {
        api_session__error(
            "Replay resources must have distinct basenames before copying."
        )
    }
    # Reserve a model name which cannot overwrite a declared copied resource.
    candidates <- sprintf("replay-%d.idf", seq_len(length(reserved) + 1L))
    model <- file.path(
        inputs,
        candidates[which(!candidates %in% reserved)[[1L]]]
    )
    private$m_model_store$materialize_cases(model, copy_external = TRUE)
    # Keep weather separate even if a schedule happens to be named weather.epw.
    weather_directory <- file.path(root, "weather")
    dir.create(weather_directory)
    # The Job pipeline later flattens weather and resources, so reserve its basename too.
    weather_candidates <- sprintf(
        "replay-weather-%d.epw",
        seq_len(length(reserved) + 1L)
    )
    weather <- file.path(
        weather_directory,
        weather_candidates[which(!weather_candidates %in% reserved)[[1L]]]
    )
    file_copy(private$m_epw_path, weather)
    ctx$job <- eplus_job(model, weather)
    library <- file.path(ctx$config$installation, api__library_name())
    files <- c(
        list.files(
            root,
            recursive = TRUE,
            full.names = TRUE,
            all.files = TRUE,
            no.. = TRUE
        ),
        library,
        file.path(
            ctx$config$installation,
            c(
                "Energy+.idd",
                "Energy+.schema.epJSON",
                "ExpandObjects",
                "ExpandObjects.exe",
                "epmacro",
                "EPMacro.exe"
            )
        )
    )
    files <- files[file.exists(files)]
    ctx$replay <- list(
        model = model,
        weather = weather,
        files = files,
        fingerprint = tools::md5sum(files)
    )
    ctx$config$journal <- file.path(root, "history")
    dir.create(ctx$config$journal)
    invisible(NULL)
}

# Atomically journal ordinary observations and effective actions, including held claims and releases.
api_replay__write <- function(directory, observation, action, previous_hash) {
    entry <- list(
        observation = observation,
        action = action,
        previous_hash = previous_hash
    )
    path <- file.path(directory, sprintf("%010d.rds", observation$step))
    temporary <- paste0(path, ".writing")
    saveRDS(entry, temporary, compress = FALSE)
    if (!file.rename(temporary, path)) {
        api_session__error("Cannot commit replay journal.")
    }
    unname(tools::md5sum(path))
}

# Recheck immutable inputs before and after each worker so one policy cannot alter another branch.
api_replay__check_inputs <- function(ctx) {
    if (
        any(!file.exists(ctx$replay$files)) ||
            !identical(tools::md5sum(ctx$replay$files), ctx$replay$fingerprint)
    ) {
        api_session__error("Replay inputs or selected library have changed.")
    }
    invisible(NULL)
}

# Validate and bound one immutable prefix before launching a prediction worker.
api_replay__history <- function(ctx, limit) {
    checkmate::assert_number(limit, lower = 1, finite = TRUE)
    api_replay__check_inputs(ctx)
    paths <- file.path(
        ctx$config$journal,
        sprintf("%010d.rds", seq_len(ctx$observation$step))
    )
    sizes <- file.info(paths)$size
    if (anyNA(sizes) || sum(sizes) > limit) {
        api_session__error(
            "Replay history is missing or exceeds history_limit."
        )
    }
    history <- lapply(paths, readRDS)
    previous_hash <- NULL
    for (index in seq_along(history)) {
        entry <- history[[index]]
        if (!identical(entry$previous_hash, previous_hash)) {
            api_session__error("Replay journal integrity check failed.")
        }
        previous_hash <- unname(tools::md5sum(paths[[index]]))
        api_session__plain(entry)
        if (
            !is.list(entry) ||
                !identical(entry$observation$step, as.integer(index))
        ) {
            api_session__error("Invalid replay history sequence.")
        }
        api_session__action(entry$action, names(ctx$config$actuators))
    }
    if (!identical(previous_hash, ctx$journal_hash)) {
        api_session__error("Replay journal integrity check failed.")
    }
    if (!identical(history[[length(history)]]$observation, ctx$observation)) {
        api_session__error(
            "Replay journal does not match the original paused boundary."
        )
    }
    history
}

# Compare registered outputs and exact boundary semantics without claiming hidden-state equivalence.
api_replay__match <- function(actual, expected, tolerance) {
    exact <- setdiff(names(expected), "values")
    values <- actual$values
    reference <- expected$values
    # Absolute tolerance applies to every registered value, not their mean error.
    equal <- identical(names(values), names(reference)) &&
        length(values) == length(reference) &&
        identical(is.na(values), is.na(reference)) &&
        isTRUE(all(
            is.na(reference) |
                values == reference |
                (is.finite(values) &
                    is.finite(reference) &
                    abs(values - reference) <= tolerance)
        ))
    if (!identical(actual[exact], expected[exact]) || !equal) {
        api_session__error(paste(
            "Replay diverged at completed step",
            expected$step
        ))
    }
    invisible(NULL)
}

# Produce recorded actions during the prefix and execute only future branch policy thereafter.
api_replay__controller <- function(history, future, tolerance) {
    count <- length(history)
    # Select recorded prefix controls before advancing the chosen future policy.
    controller <- function(observation, state) {
        # Verify each prefix boundary inside the worker, avoiding an IPC round trip per past step.
        if (observation$step <= count) {
            api_replay__match(
                observation,
                history[[observation$step]]$observation,
                tolerance
            )
        }
        next_step <- observation$step + 1L
        if (next_step <= count) {
            entry <- history[[next_step]]
            return(list(
                action = entry$action,
                state = entry$observation$controller_state
            ))
        }
        if (is.function(future)) {
            return(future(observation, state))
        }
        list(action = future[[next_step - count]], state = state)
    }
    # Freeze only ordinary recorded data and the selected future policy.
    run__prepare_setup(controller)
}

# Replay and predict sequentially with one fresh worker per branch, leaving the original untouched.
api_replay__predict <- function(
    ctx,
    branches,
    horizon,
    tolerance = 1e-8,
    timeout = ctx$config$timeout,
    history_limit = 128 * 1024^2,
    dir = NULL
) {
    api_session__owner(ctx)
    if (
        !isTRUE(ctx$config$record) || api_session__status(ctx)$phase != "paused"
    ) {
        api_session__error(
            "Prediction requires a recorded paused session (record = TRUE)."
        )
    }
    checkmate::assert_int(horizon, lower = 1L)
    checkmate::assert_number(tolerance, lower = 0, finite = TRUE)
    api_session__timeout(timeout)
    checkmate::assert_list(branches, min.len = 1L, names = "unique")
    for (future in branches) {
        if (is.function(future)) {
            checkmate::assert_function(future, nargs = 2L)
            run__check_setup(future)
        } else {
            checkmate::assert_list(future, len = horizon)
            for (action in future) {
                api_session__action(action, names(ctx$config$actuators))
            }
        }
    }
    history <- api_replay__history(ctx, history_limit)
    origin <- list(
        identity = ctx$identity,
        observation = ctx$observation,
        step = length(history),
        history_hash = ctx$journal_hash,
        input_fingerprint = ctx$replay$fingerprint
    )
    directory <- dir %||% tempfile("eplusr-prediction-")
    checkmate::assert_string(directory)
    if (
        dir.exists(directory) &&
            length(list.files(directory, all.files = TRUE, no.. = TRUE))
    ) {
        api_session__error("Prediction output directory must be empty or new.")
    }
    results <- vector("list", length(branches))
    names(results) <- names(branches)
    for (index in seq_along(branches)) {
        results[[index]] <- api_replay__branch(
            ctx,
            history,
            branches[[index]],
            horizon,
            tolerance,
            timeout,
            file.path(directory, sprintf("branch-%03d", index))
        )
    }
    # Recheck identity and observation without sending a command to the original engine.
    if (
        !identical(ctx$identity, origin$identity) ||
            !identical(ctx$observation, origin$observation) ||
            api_session__status(ctx)$phase != "paused"
    ) {
        api_session__error("Original session changed during prediction.")
    }
    structure(
        list(
            method = "replay",
            origin = origin,
            branches = results,
            tolerance = tolerance
        ),
        class = "EnergyPlusPrediction"
    )
}

# Close the branch on every exit, including history mismatch, policy error and caller interrupt.
api_replay__branch <- function(
    ctx,
    history,
    future,
    horizon,
    tolerance,
    timeout,
    directory
) {
    started <- Sys.time()
    api_replay__check_inputs(ctx)
    # Share one deadline across startup, replay and every future step.
    budget <- function() {
        remaining <- timeout -
            as.numeric(difftime(Sys.time(), started, units = "secs"))
        if (remaining <= 0) {
            api_session__error("Prediction branch timed out.")
        }
        remaining
    }
    branch <- eplus_session(
        ctx$replay$model,
        ctx$replay$weather,
        eplus = ctx$config$installation,
        variables = ctx$config$variables,
        actuators = ctx$config$actuators,
        initial_action = history[[1L]]$action,
        controller = api_replay__controller(history, future, tolerance),
        controller_state = history[[1L]]$observation$controller_state,
        dir = directory
    )
    on.exit(try(branch$close(), silent = TRUE), add = TRUE)
    observation <- branch$initialize(timeout = budget())
    identity <- branch$status()$identity
    api_replay__match(observation, history[[1L]]$observation, tolerance)
    if (length(history) > 1L) {
        # The worker checks intermediate observations; the owner verifies the final fork itself.
        observation <- branch$step(
            n = as.integer(length(history) - 1L),
            timeout = budget()
        )
        api_replay__match(
            observation,
            history[[length(history)]]$observation,
            tolerance
        )
    }
    boundary <- observation
    observations <- list()
    for (step in seq_len(horizon)) {
        if (branch$status()$phase != "paused") {
            break
        }
        next_observation <- branch$step(timeout = budget())
        # Completion can return the same final snapshot without advancing another zone.
        if (next_observation$step == observation$step) {
            break
        }
        observations[[length(observations) + 1L]] <- next_observation
        observation <- next_observation
    }
    complete <- length(observations) == horizon
    branch$close()
    api_replay__check_inputs(ctx)
    list(
        method = "replay",
        identity = identity,
        boundary = boundary,
        observations = observations,
        replay_steps = length(history),
        future_steps = length(observations),
        complete = complete,
        outcome = branch$status()$outcome,
        elapsed = as.numeric(difftime(Sys.time(), started, units = "secs"))
    )
}
