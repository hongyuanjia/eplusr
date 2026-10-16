# Select reviewed R behavior independently of the generated C binding format.
api_profile__select <- function(version) {
    version <- as.character(version)
    if (length(version) != 1L || !version %in% c("26.1.0", "26.2.0")) {
        api__error("The API requires an EnergyPlus 26.1 or 26.2 installation.")
    }
    list(version = version, abi = "26.1-26.2")
}

# Define reviewed management policy; function signatures and layouts come from the dynport.
api__manifest <- function(version = "26.1.0") {
    profile <- api_profile__select(version)
    list(
        energyplus = profile$version,
        abi = profile$abi,
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
                kind = "clear_callbacks",
                ownership = "logical_callbacks"
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
                kind = "clear_callbacks",
                ownership = "logical_callbacks"
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
                "",
                parameters = "state",
                kind = "input_metadata",
                requires_ready = TRUE,
                ownership = "input_metadata"
            ),
            api__rule(
                "exchange",
                "get_weather_file_path",
                "",
                parameters = "state",
                kind = "input_metadata",
                requires_ready = TRUE,
                ownership = "input_metadata"
            ),
            api__rule(
                "exchange",
                "get_object_names",
                "",
                parameters = "state, object_type_name",
                kind = "input_metadata",
                requires_ready = TRUE,
                ownership = "input_metadata"
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
                reason = "Stock 26.1/26.2 index trend history out of bounds at time_index = history size; no public history-size query.",
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
        resource_delete = "R view invalidation; stock 26.1/26.2 public native deleter is a no-op",
        scalar = "scalar return or copied argument; no caller-owned native allocation",
        resource = "state/generation-owned view; native allocations may survive public deletion",
        static_string = "borrowed static string copied to R character",
        callback = "retained trampoline until stateReset/stateDelete; user function may be detached",
        run = "argv and callbacks retained for synchronous run; state survives normal return",
        api_data = "owned array copied to R then freeAPIData",
        input_metadata = "R-owned frozen prepared input; no allocating native getter",
        logical_callbacks = "detach user functions; retain native proxies until reset/delete",
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
