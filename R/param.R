#' @importFrom R6 R6Class
#' @importFrom cli cat_boxx cat_line cat_rule
#' @importFrom data.table rbindlist set setattr setcolorder
#' @importFrom tools file_path_sans_ext
NULL

#' Create and Run Parametric Analysis, and Collect Results
#'
#' `ParametricJob` class provides a prototype of conducting parametric analysis
#' of EnergyPlus simulations.
#'
#' Basically, it is a collection of multiple `EplusJob` objects. The seed model
#' and generated models are stored internally as prepared model snapshots and
#' parsed as [Idf] objects only when needed. Also, an object in `Output:SQLite`
#' with `Option Type` value of `SimpleAndTabular` will be automatically created
#' if it does not exists, like [Idf] class does.
#'
#' @docType class
#' @name ParametricJob
#' @author Hongyuan Jia
NULL

#' @export
# ParametricJob {{{
ParametricJob <- R6::R6Class(classname = "ParametricJob", cloneable = FALSE,
    inherit = EplusGroupJob,
    public = list(

        # INITIALIZE {{{
        #' @description
        #' Create a `ParametricJob` object
        #'
        #' @param idf Path to EnergyPlus IDF file or an `Idf` object.
        #' @param epw Path to EnergyPlus EPW file or an `Epw` object. `epw` can
        #'        also be `NULL` which will force design-day-only simulation
        #'        when `$run()` method is called. Note this needs at least one
        #'        `Sizing:DesignDay` object exists in the [Idf].
        #'
        #' @return A `ParametricJob` object.
        #'
        #' @examples
        #' \dontrun{
        #' if (is_avail_eplus("8.8")) {
        #'      path_idf <- path_eplus_example("8.8", "5Zone_Transformer.idf")
        #'      path_epw <- path_eplus_weather("8.8", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")
        #'
        #'     # create from an IDF and an EPW
        #'     param <- param_job(path_idf, path_epw)
        #'     param <- ParametricJob$new(path_idf, path_epw)
        #'
        #'     # create from an Idf and an Epw object
        #'     param_job(read_idf(path_idf), read_epw(path_epw))
        #' }
        #' }
        #'
        initialize = function(idf, epw) {
            private$m_model_store <- JobModelStore$new()
            private$m_model_store$set_seed(idf)

            # log if the input idf has been changed
            private$m_log <- new.env(hash = FALSE, parent = emptyenv())
            private$m_log$unsaved <- logical()

            private$m_weather_cases <- param_weather_cases(epw)
            param_refresh_run_cases(private)

            # save uuid
            private$log_seed_uuid()
            private$log_new_uuid()
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        # version {{{
        #' @description
        #' Get the version of seed IDF
        #'
        #' @details
        #' `$version()` returns the version of input seed [Idf] object.
        #'
        #' @return A [base::numeric_version()] object.
        #'
        #' @examples
        #' \dontrun{
        #' param$version()
        #' }
        #'
        version = function()
            param_version(self, private),
        # }}}

        # seed {{{
        #' @description
        #' Get or replace the seed [Idf] object
        #'
        #' @details
        #' `$seed()` returns the parsed input seed [Idf] object. `$seed(new)`
        #' replaces the seed model. If parametric models have already been
        #' generated, they become stale and cannot be run or saved until
        #' `$param()` or `$apply_measure()` is called again.
        #'
        #' @param new A path to an EnergyPlus IDF file or an [Idf] object used
        #'   to replace the current seed. If missing, the current seed is
        #'   returned.
        #'
        #' @examples
        #' \dontrun{
        #' param$seed()
        #' }
        #'
        seed = function(new)
            param_seed(self, private, new),
        # }}}

        # weather {{{
        #' @description
        #' Get the [Epw] object
        #'
        #' @details
        #' `$weather()` returns the input [Epw] object. If no [Epw] is provided
        #' when creating the `ParametricJob` object, `NULL` is returned.
        #'
        #' @examples
        #' \dontrun{
        #' param$weather()
        #' }
        #'
        weather = function()
            param_weather(self, private),
        # }}}

        # weathers {{{
        #' @description
        #' Get or set weather cases
        #'
        #' @details
        #' `$weathers()` returns the current weather case table. `$weathers(epws)`
        #' replaces the weather cases and rebuilds the actual run cases used by
        #' `$run()`, `$save()` and `$cases(type = "run")`. Existing generated
        #' parametric models are kept.
        #'
        #' @param epws A path, a character vector of paths, an [Epw] object, or a
        #'   list of paths and [Epw] objects. `NULL` or `NA` entries indicate
        #'   design-day-only weather cases. If missing, the current weather case
        #'   table is returned.
        #' @param names Optional weather case names. If `NULL`, names are taken
        #'   from `epws`, file names, or `"design_day"`.
        #'
        #' @return If `epws` is missing, a [data.table::data.table()]. Otherwise,
        #'   the modified `ParametricJob` object invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' param$weathers()
        #' param$weathers(c(sf = "USA_CA_San.Francisco.epw", golden = "USA_CO_Golden.epw"))
        #' }
        #'
        weathers = function(epws, names = NULL)
            param_weathers(self, private, epws, names),
        # }}}

        # param {{{
        #' @description
        #' Set parameters for parametric simulations
        #'
        #' @details
        #' `$param()` takes parameter definitions in list format, which is
        #' similar to [Idf$set()][Idf] except that each field is not assigned
        #' with a single value, but a vector of any length, indicating the
        #' levels of each parameter.
        #'
        #' Similar like the way of modifying object field values in
        #' [Idf$set()][Idf], there are 3 different ways of defining a parameter
        #' in epluspar:
        #'
        #' * `object = list(field = c(value1, value2, ...))`: Where `object` is
        #'   a valid object ID or name. Note object ID should be denoted with
        #'   two periods `..`, e.g. `..10` indicates the object with ID `10`, It
        #'   will set that specific field in that object as one parameter.
        #' * `.(object, object) := list(field = c(value1, value2, ...))`:
        #'   Similar like above, but note the use of `.()` in the left hand
        #'   side.  You can put multiple object ID or names in `.()`. It will
        #'   set the field of all specified objects as one parameter.
        #' * `class := list(field = c(value1, value2, ...))`: Note the use of
        #'   `:=` instead of `=`. The main difference is that, unlike `=`, the
        #'   left hand side of `:=` should be a valid class name in current
        #'   [Idf]. It will set that field of all objects in specified
        #'   class as one parameter.
        #'
        #' For example, the code block below defines 3 parameters:
        #'
        #' * Field `Fan Total Efficiency` in object named `Supply Fan 1` in class
        #'   `Fan:VariableVolume` class, with 10 levels being 0.1 to 1.0 with a
        #'   0.1 step.
        #' * Field `Thickness` in all objects in class `Material`, with 10
        #'   levels being 0.01 to 0.1 m with a 0.1 m step.
        #' * Field `Conductivity` in all objects in class `Material`, with 10
        #'  levels being 0.1 to 1.0 W/m-K with a 0.1 W/m-K step.
        #'
        #' ```
        #' param$param(
        #'     `Supply Fan 1` = list(Fan_Total_Efficiency = seq(0.1, 1.0, 0.1)),
        #'     Material := list(
        #'         Thickness = seq(0.01, 0.1, 0.1),
        #'         Conductivity = seq(0.1, 1.0, 0.1)
        #'     )
        #' )
        #' ```
        #'
        #' @param ... Lists of parameter definitions. Please see above on the
        #'        syntax.
        #'
        #' @param .names A character vector of the parameter names. If `NULL`,
        #'        the parameter will be named in format `param_X`, where
        #'        `X` is the index of parameter. Default: `NULL`.
        #'
        #' @param .cross If `TRUE`, all combinations of parameter values will be
        #'        used to create models. If `FALSE`, each parameter should have
        #'        the same length of values. Default: `FALSE`.
        #'
        #' @return The modified `ParametricJob` object invisibly.
        #'
        #' @examples
        #' \dontrun{
        #'
        #' param$param(
        #'     Material := .(
        #'         Thickness = seq(0.1, 1, length.out = 3),
        #'         Conductivity = seq(0.1, 0.6, length.out = 3)
        #'     ),
        #'    "Supply Fan 1" = .(fan_total_efficiency = c(0.1, 0.5, 0.8))
        #' )
        #'
        #' # specify parameter values
        #' param$param(
        #'     Material := .(
        #'         Thickness = seq(0.1, 1, length.out = 3),
        #'         Conductivity = seq(0.1, 0.6, length.out = 3)
        #'      ),
        #'     "Supply Fan 1" = list(fan_total_efficiency = c(0.1, 0.5, 0.8)),
        #'     .names = c("thickness", "conduct", "fan_eff")
        #' )
        #'
        #' # each parameter should have the same length of values
        #' try(
        #' param$param(
        #'     Material := list(Thickness = c(0.1, 0.2)),
        #'     "Supply Fan 1" = list(fan_total_efficiency = c(0.1, 0.5, 0.8))
        #' )
        #' )
        #'
        #' # use all combinations of parameters
        #' param$param(
        #'     Material := list(
        #'         Thickness = seq(0.1, 1, length.out = 3),
        #'         Conductivity = seq(0.1, 0.6, length.out = 3)
        #'     ),
        #'     "Supply Fan 1" = list(fan_total_efficiency = c(0.1, 0.5, 0.8)),
        #'     .cross = TRUE
        #' )
        #' }
        #'
        param = function(..., .names = NULL, .cross = FALSE)
            param_param(self, private, ..., .names = .names, .cross = .cross),
        # }}}

        # apply_measure {{{
        #' @description
        #' Create parametric models
        #'
        #' @details
        #' `$apply_measure()` allows to apply a measure to an [Idf] and creates
        #' parametric models for analysis. Basically, a measure is just a
        #' function that takes an [Idf] object and other argument input, argument
        #' returns a modified [Idf] object as output. Use `...` to supply
        #' different arguments, **except for the first `Idf` argument**, to that
        #' measure. Under the hook, [base::mapply()] is used to create multiple
        #' [Idf]s according to the input values.
        #'
        #' @param measure A function that takes an `Idf` and other arguments as
        #'        input and returns an [Idf] object as output.
        #' @param ... Arguments **except first `Idf` argument** that are passed
        #'        to that `measure`.
        #' @param .names A character vector of the names of parametric `Idf`s.
        #'        If `NULL`, the new `Idf`s will be named in format
        #'        `measure_name + number`.
        #'
        #' @return The modified `ParametricJob` object itself, invisibly.
        #' @examples
        #' \dontrun{
        #' # create a measure to change the orientation of the building
        #' rotate_building <- function(idf, degree = 0L) {
        #'     if (!idf$is_valid_class("Building")) {
        #'        stop("Input model does not have a Building object")
        #'     }
        #'
        #'     if (degree > 360 || degree < -360 ) {
        #'         stop("Input degree should in range [-360, 360]")
        #'     }
        #'
        #'     cur <- idf$Building$North_Axis
        #'
        #'     new <- cur + degree
        #'
        #'     if (new > 360) {
        #'         new <- new %% 360
        #'         warning("Calculated new north axis is greater than 360. ",
        #'             "Final north axis will be ", new
        #'         )
        #'     } else if (new < -360) {
        #'         new <- new %% -360
        #'         warning("Calculated new north axis is smaller than -360. ",
        #'             "Final north axis will be ", new
        #'         )
        #'     }
        #'
        #'     idf$Building$North_Axis <- new
        #'
        #'     idf
        #' }
        #'
        #' # apply measure
        #' # this will create 12 models
        #' param$apply_measure(rotate_building, degree = seq(30, 360, 30))
        #'
        #' # apply measure with new names specified
        #' param$apply_measure(rotate_building, degree = seq(30, 360, 30),
        #'     .names = paste0("rotate_", seq(30, 360, 30))
        #' )
        #' }
        #'
        apply_measure = function(measure, ..., .names = NULL)
            param_apply_measure(self, private, measure, ..., .names = .names),
        # }}}

        # models {{{
        #' @description
        #' Get created parametric [Idf] objects
        #'
        #' @details
        #' `$models()` returns a list of parametric models generated using input
        #' [Idf] object and
        #' \href{../../eplusr/html/ParametricJob.html#method-apply_measure}{\code{$apply_measure()}}
        #' method. Model names are assigned in the same way as the `.names`
        #' argument in
        #' \href{../../eplusr/html/ParametricJob.html#method-apply_measure}{\code{$apply_measure()}}.
        #' If no measure has been applied, `NULL` is returned. Returned [Idf]
        #' objects are detached copies of the internally stored prepared
        #' snapshots. Modifying them will not change the models that are used
        #' for `$run()` or `$save()`.
        #'
        #' @param names A character vector of new names for parametric models.
        #'        If a single string, it will be used as a prefix and all models
        #'        will be named in pattern `names_X`, where `X` is the model
        #'        index. If `NULL`, existing parametric models are directly
        #'        returned. Default: `NULL`.
        #'
        #' @examples
        #' \dontrun{
        #' param$models()
        #' }
        #'
        models = function(names = NULL)
            param_models(self, private, names),
        # }}}

        # cases {{{
        #' @description
        #' Get a summary of parametric or run cases
        #'
        #' @details
        #' `$cases()` returns a [data.table][data.table::data.table()] giving a
        #' summary of parametric models and parameter values. With
        #' `type = "run"`, it returns the actual simulation case table created
        #' from parametric model cases and weather cases.
        #'
        #' The model case `data.table` has the following columns:
        #'
        #' * `index`: Integer type. The indices of parameter models
        #' * `case`: Character type. The names of parameter models
        #' * Parameters: Type depends on the parameter values. Each parameter
        #'   stands in a separate column. For parametric models created using
        #'   `ParametricJob$param()`, the column names will be the same as what
        #'   you specified in `.names`. For the case of
        #'   `ParametricJob$apply_measure()`, this will be the argument names of
        #'   the measure functions.
        #'
        #' The run case `data.table` has the following columns:
        #'
        #' * `index`: Integer type. The indices of actual simulations
        #' * `case`: Character type. The names of actual simulations
        #' * `model_index`: Integer type. The indices of parameter models
        #' * `model_case`: Character type. The names of parameter models
        #' * `weather_index`: Integer type. The indices of weather cases
        #' * `weather_case`: Character type. The names of weather cases
        #' * `epw`: Character type. The paths of weather files. `NA` means
        #'   design-day-only simulation.
        #'
        #' @param type `"model"` returns parameter model cases and preserves the
        #'   historical `$cases()` behavior. `"run"` returns the actual simulation
        #'   cases. Default: `"model"`.
        #'
        #' @return If no corresponding cases exist, `NULL` is returned.
        #'   Otherwise, a [data.table][data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' param$cases()
        #' }
        #'
        cases = function(type = c("model", "run"))
            param_cases(self, private, type),
        # }}}

        # save {{{
        #' @description
        #' Save parametric models
        #'
        #' @details
        #' `$save()` saves all parametric models in specified folder. An error
        #' will be issued if no measure has been applied.
        #'
        #' @param dir The parent output directory for models to be saved. If
        #'        `NULL`, the directory of the seed model will be used. Default:
        #'        `NULL`.
        #' @param separate If `TRUE`, all models are saved in a separate folder
        #'        with each model's name under specified directory. If `FALSE`,
        #'        all models are saved in the specified directory. Default:
        #'        `TRUE`.
        #' @param copy_external Only applicable when `separate` is `TRUE`. If
        #'        `TRUE`, the external files that every `Idf` object depends on
        #'        will also be copied into the saving directory. The values of
        #'        file paths in the Idf will be changed automatically.
        #         This ensures that the output directory will have all files
        #         needed for the model to run. Default: `FALSE`.
        #'
        #' @return A [data.table::data.table()] with two columns:
        #'
        #' * model: The path of saved parametric model files.
        #' * weather: The path of saved weather files.
        #'
        #' @examples
        #' \dontrun{
        #' # save all parametric models with each model in a separate folder
        #' param$save(tempdir())
        #'
        #' # save all parametric models with all models in the same folder
        #' param$save(tempdir(), separate = FALSE)
        #' }
        #'
        save = function(dir = NULL, separate = TRUE, copy_external = FALSE)
            param_save(self, private, dir, separate, copy_external),
        # }}}

        # run {{{
        #' @description
        #' Run parametric simulations
        #'
        #' @details
        #' `$run()` runs all parametric simulations in parallel. The number of
        #' parallel EnergyPlus process can be controlled by
        #' `eplusr_option("num_parallel")`. If `wait` is FALSE, then the job
        #' will be run in the background. You can get updated job status by just
        #' printing the `ParametricJob` object.
        #'
        #' @param dir The parent output directory for specified simulations.
        #'        Outputs of each simulation are placed in a separate folder
        #'        under the parent directory. If `NULL`, the directory of the
        #'        seed model will be used. Default: `NULL`.
        #' @param wait If `TRUE`, R will hang on and wait all EnergyPlus
        #'        simulations finish. If `FALSE`, all EnergyPlus simulations are
        #'        run in the background.  Default: `TRUE`.
        #' @param force Only applicable when the last simulation runs with
        #'        `wait` equals to `FALSE` and is still running. If `TRUE`,
        #'        current running job is forced to stop and a new one will
        #'        start. Default: `FALSE`.
        #' @param copy_external If `TRUE`, the external files that current `Idf`
        #'        object depends on will also be copied into the simulation
        #'        output directory. The values of file paths in the Idf will be
        #'        changed automatically. Currently, only `Schedule:File` class
        #'        is supported.  This ensures that the output directory will
        #'        have all files needed for the model to run. Default is
        #'        `FALSE`.
        #' @param echo Only applicable when `wait` is `TRUE`. Whether to
        #'        simulation status. Default: same as `wait`.
        #' @param separate If `TRUE`, all models are saved in a separate folder
        #'        with each model's name under `dir` when simulation. If `FALSE`,
        #'        all models are saved in `dir` when simulation. Default:
        #'        `TRUE`.
        #' @param readvars If `TRUE`, the `ReadVarESO` post-processor will run
        #'        to generate CSV files from the ESO output. Since those CSV
        #'        files are never used when extracting simulation data in eplusr,
        #'        setting it to `FALSE` can speed up the simulation if there are
        #'        hundreds of output variables or meters. Default: `TRUE`.
        #'
        #' @return The `ParametricJob` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # run parametric simulations
        #' param$run(wait = TRUE, echo = FALSE)
        #'
        #' # run in background
        #' param$run(wait = FALSE)
        #' # get detailed job status by printing
        #' print(param)
        #' }
        #'
        run = function(dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait, separate = TRUE, readvars = TRUE)
            param_run(self, private, dir, wait, force, copy_external, echo, separate, readvars),
        # }}}

        # print {{{
        #' @description
        #' Print `ParametricJob` object
        #'
        #' @details
        #' `$print()` shows the core information of this `ParametricJob`,
        #' including the path of IDFs and EPWs and also the simulation job
        #' status.
        #'
        #' `$print()` is quite useful to get the simulation status, especially
        #' when `wait` is `FALSE` in `$run()`. The job status will be updated
        #' and printed whenever `$print()` is called.
        #'
        #' @return The `ParametricJob` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' param$print()
        #'
        #' Sys.sleep(10)
        #' param$print()
        #' }
        #'
        print = function()
            param_print(self, private)
        # }}}
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_model_case_ids = integer(),
        m_model_case_names = character(),
        m_weather_cases = NULL,
        m_run_cases = NULL,
        # }}}

        # PRIVATE FUNCTIONS {{{
        seed_uuid = function() private$m_model_store$model_signature(private$m_model_store$seed_model_id()),
        log_seed_uuid = function() private$m_log$seed_uuid <- private$seed_uuid(),
        cached_seed_uuid = function() private$m_log$seed_uuid
        # }}}
    )
)
# }}}

#' Create An EnergyPlus Parametric Simulation Job
#'
#' `param_job()` takes an IDF and EPW as input and returns a `ParametricJob`.
#' For details on `ParametricJob`, please see [ParametricJob] class.
#'
#' @param idf A path to EnergyPlus IDF or IMF file or an `Idf` object.
#' @param epw A path to EnergyPlus EPW file or an `Epw` object. `epw` can also
#'        be `NULL` which will force design-day-only simulation when
#'        [`$run()`][ParametricJob] method is called. Note this needs at least
#'        one `Sizing:DesignDay` object exists in the [Idf].
#' @return A `ParametricJob` object.
#' @seealso [eplus_job()] for creating an EnergyPlus single simulation job.
#' @name ParametricJob
#' @export
# param_job {{{
param_job <- function(idf, epw) {
    ParametricJob$new(idf, epw)
}
# }}}

# param_version {{{
param_version <- function(self, private) {
    private$m_model_store$model_version(private$m_model_store$seed_model_id())
}
# }}}
# param_seed {{{
param_seed <- function(self, private, new) {
    if (missing(new)) {
        return(private$m_model_store$load_seed())
    }

    param_assert_not_running(private)
    private$m_model_store$set_seed(new)
    if (!length(private$m_model_case_ids)) {
        param_refresh_run_cases(private)
    } else {
        private$m_run_cases <- NULL
    }
    param_clear_run_state(private, new_uuid = FALSE)
    private$log_seed_uuid()
    private$log_new_uuid()
    invisible(self)
}
# }}}
# param_models {{{
param_models <- function(self, private, names = NULL) {
    assert_character(names, any.missing = FALSE, null.ok = TRUE, min.len = 1L)
    if (!length(private$m_model_case_ids)) {
        verbose_info("No parametric models have been created.")
        if (!is.null(names)) {
            verbose_info("Nothing to rename.")
        }
        return(NULL)
    }

    private$m_model_store$assert_cases_valid()

    if (length(names)) {
        private$m_model_case_names <- param_normalize_case_names(
            names, length(private$m_model_case_ids), "parametric model"
        )
        param_refresh_run_cases(private)
        param_clear_run_state(private)
    }

    out <- lapply(private$m_model_case_ids, private$m_model_store$load_model)
    names(out) <- private$m_model_case_names
    out
}
# }}}
# param_weather {{{
param_weather <- function(self, private) {
    weather <- private$m_weather_cases
    if (is.null(weather) || !nrow(weather)) return(NULL)
    has_epw <- !is.na(weather$epw)
    if (!any(has_epw)) return(NULL)

    paths <- weather$epw[has_epw]
    if (length(paths) == 1L && nrow(weather) == 1L) return(read_epw(paths[[1L]]))

    out <- lapply(weather$epw, function(path) {
        if (is.na(path)) NULL else read_epw(path)
    })
    names(out) <- weather$weather_case
    out
}
# }}}
# param_weathers {{{
param_weathers <- function(self, private, epws, names = NULL) {
    if (missing(epws)) {
        return(data.table::copy(private$m_weather_cases))
    }

    param_assert_not_running(private)
    private$m_weather_cases <- param_weather_cases(epws, names)
    param_refresh_run_cases(private)
    param_clear_run_state(private)

    invisible(self)
}
# }}}
# param_cases {{{
param_cases <- function(self, private, type = c("model", "run")) {
    type <- match.arg(type)

    if (identical(type, "run")) {
        return(param_run_cases(self, private))
    }

    param_model_cases(self, private)
}
# }}}
# param_model_cases {{{
param_model_cases <- function(self, private) {
    if (!length(private$m_model_case_ids)) {
        verbose_info("No parametric models have been created.")
        return(NULL)
    }
    private$m_model_store$assert_cases_valid()

    cases <- copy(private$m_log$params)

    if (!private$m_log$simple) {
        if ("case" %in% names(cases)) {
            set(cases, NULL, "case", private$m_model_case_names)
        }
        return(cases)
    }

    # remove duplicates
    cases <- unique(cases, by = c("case_index", "param_index"))
    # add field type
    seed <- private$m_model_store$load_seed()
    add_field_property(get_priv_env(seed)$idd_env(), cases, "type_enum")
    # get value list
    set(cases, NULL, "value", get_value_list(cases))
    # change to wide
    cases <- data.table::dcast.data.table(cases, case_index ~ param_name, value.var = "value")
    for (col in setdiff(names(cases), "case_index")) {
        set(cases, NULL, col, unlist(cases[[col]], FALSE, FALSE))
    }
    setnames(cases, "case_index", "index")
    set(cases, NULL, "case", private$m_model_case_names)
    setcolorder(cases, c("index", "case"))

    cases[]
}
# }}}
# param_run_cases {{{
param_run_cases <- function(self, private) {
    run <- private$m_run_cases
    if (is.null(run) || !nrow(run)) {
        verbose_info("No run cases have been created.")
        return(NULL)
    }
    private$m_model_store$assert_cases_valid()

    out <- data.table::copy(run)
    model <- suppressMessages(param_model_cases(self, private))
    if (!is.null(model)) {
        setnames(model, c("index", "case"), c("model_index", "model_case"))
        out <- model[out, on = c("model_index", "model_case")]
        setcolorder(out, c("index", "case", "model_index", "model_case",
            "weather_index", "weather_case", "epw"))
    }

    out[]
}
# }}}
# param_model_case_table {{{
param_model_case_table <- function(private) {
    if (!length(private$m_model_case_ids)) {
        return(data.table(model_index = integer(), model_id = integer(), model_case = character()))
    }

    data.table(
        model_index = seq_along(private$m_model_case_ids),
        model_id = private$m_model_case_ids,
        model_case = private$m_model_case_names
    )
}
# }}}
# param_param {{{
param_param <- function(self, private, ..., .names = NULL, .cross = FALSE, .env = parent.frame()) {
    assert_flag(.cross)
    assert_character(.names, null.ok = TRUE, any.missing = FALSE)

    seed <- private$m_model_store$load_seed()
    l <- expand_idf_dots_value(
        get_priv_env(seed)$idd_env(), get_priv_env(seed)$idf_env(),
        ..., .type = "object", .complete = FALSE, .unique = TRUE, .empty = FALSE,
        .default = FALSE, .scalar = FALSE, .pair = FALSE, .env = .env)

    # clean previous parameter data
    private$m_log$params <- NULL

    # extract parameters
    params <- unique(l$value, by = c("rleid", "class_id", "field_id"))
    # if no duplicates, param and l$value is the same
    if (nrow(params) == nrow(l$value)) params <- copy(params)
    # remove object scope data
    set(params, NULL, c("object_name", "object_id", "value_id"), NULL)

    # set parameter index
    set(params, NULL, "param_index", seq_len(nrow(params)))

    if (!.cross) {
        # all parameters should have the same length
        len <- viapply(params$value_chr, length)

        if (length(unique(len)) > 1L) {
            abort(paste0(
                "When '.cross' is 'FALSE', all input parameter values should have the same length. ",
                "But different value lengths were detected:\n",
                params[, paste(sprintf(
                    " #%s| Parameter '%s' in class '%s' --> Length: %i",
                    lpad(param_index, "0"), field_name, class_name, len), collapse = "\n"
                )]
            ))
        }
    } else {
        set(params, NULL, "value_chr", as.list(do.call(data.table::CJ, params$value_chr)))
        set(params, NULL, "value_num", as.list(do.call(data.table::CJ, params$value_num)))
    }

    # add object and value mapping
    params[
        l$value[, by = c("rleid", "class_id", "field_id"),
            list(param_index = .GRP, object_id = list(object_id), object_name = list(object_name), value_id = list(value_id))],
        on = "param_index", `:=`(object_id = i.object_id, object_name = i.object_name, value_id = i.value_id)
    ]

    # get full parameter table
    params <- params[,
        by = c("param_index", "rleid", "class_id", "class_name", "field_id", "field_index", "field_name"),
        {
            len_obj <- length(object_id[[1L]])
            len_val <- length(value_chr[[1L]])

            object_id <- rep(object_id[[1L]], each = len_val)
            object_name <- rep(object_name[[1L]], each = len_val)
            case_index <- rep(seq.int(len_val), len_obj)
            value_id <- rep(value_id[[1L]], each = len_val)
            value_chr <- rep(value_chr[[1L]], len_obj)
            value_num <- rep(value_num[[1L]], len_obj)
            list(case_index = case_index,
                object_id = object_id, object_name = object_name,
                value_id = value_id, value_chr = value_chr, value_num = value_num
            )
        }
    ]
    cols <- c("param_index", "case_index", "object_id", "class_name",
        "field_id", "field_index", "field_name", "value_chr", "value_num")
    set(params, NULL, setdiff(names(params), cols), NULL)

    # validate parameter names
    if (is.null(.names)) {
        set(params, NULL, "param_name", sprintf("param_%s", lpad(params$param_index, "0")))
    } else {
        if (length(.names) != max(params$param_index)) {
            abort(sprintf(
                paste("Invalid parameter names found.",
                    "%s parameters specified but %i parameter names have been specified."
                ),
                max(params$param_index), length(.names)
            ), "param_name")
        }
        nms <- make.unique(.names, sep = "_")
        set(params, NULL, "param_name", nms[params$param_index])
    }

    # get inputs for Idf$update()
    dt <- copy(params)
    setnames(dt,
        c("object_id", "class_name", "field_index", "value_chr"),
        c("id", "class", "index", "value")
    )
    set(dt, NULL, c("param_index", "field_name", "value_num"), NULL)

    # create fake measure
    measure <- function(idf, dt) {idf$update(dt); idf}

    # create parametric models
    param_apply_measure(self, private, measure, split(dt, by = "case_index", keep.by = FALSE))

    private$m_log$params <- params
    private$m_log$simple <- TRUE

    invisible(self)
}
# }}}
# param_apply_measure {{{
#' @importFrom checkmate assert_function
param_apply_measure <- function(self, private, measure, ..., .names = NULL, .env = parent.frame()) {
    checkmate::assert_function(measure)
    assert_character(.names, any.missing = FALSE, null.ok = TRUE, min.len = 1L)

    if (length(formals(measure)) < 2L) {
        abort("'measure' function must have at least two argument", "param_measure")
    }

    measure_wrapper <- function(idf, ..., .__PROGRESS_BAR__) {
        cli::cli_progress_update(id = .__PROGRESS_BAR__$id, .envir = .__PROGRESS_BAR__$env)
        idf <- idf$clone(deep = TRUE)
        idf <- measure(idf, ...)
        if (!is_idf(idf)) {
            stop("Measure should return an 'Idf' object, not '", class(idf)[[1]], "'")
        }
        idf
    }

    # in case 'function(idf, ...)' is specified as a measure
    if (is.name(substitute(measure, .env))) {
        bare <- FALSE
        mea_nm <- deparse(substitute(measure, .env))
    } else {
        bare <- TRUE
        mea_nm <- "case"
    }
    private$m_log$measure_name <- mea_nm
    private$m_log$bare <- bare

    seed <- private$m_model_store$load_seed()

    # progress bar
    progress_bar <- cli::cli_progress_bar(
        total = max(viapply(list(...), length)), clear = TRUE,
        format = "[{cli::pb_current}/{cli::pb_total}] | {cli::pb_percent} {cli::pb_bar} [Elapsed: {cli::pb_elapsed}]"
    )

    # create models
    out <- mapply(measure_wrapper, ...,
        MoreArgs = list(
            idf = seed,
            .__PROGRESS_BAR__ = list(id = progress_bar, env = environment())
        ),
        SIMPLIFY = FALSE, USE.NAMES = FALSE
    )

    # in case there are no argument specified to measure
    if (!length(out)) abort("No arguments have been given to the input measure.")

    # validate parametric model names
    if (is.null(.names)) {
        nms <- paste0(mea_nm, "_", seq_along(out))
    } else {
        if (length(.names) == 1L && length(out) > 1L) {
            .names <- paste(.names, lpad(seq_along(out), "0"), sep = "_")
        }
        if (length(out) != length(.names)) {
            abort(paste(
                "Invalid parametric model names found.",
                length(out), "models created but", length(.names), "names given"
            ), "param_names")
        }
        nms <- make.unique(.names, sep = "_")
    }
    setattr(out, "names", nms)

    # construct parameter table
    cl <- as.call(c(measure, list(quote(idf)), list(...)))
    cases <- as.data.table(as.list(match.call(measure, cl)[-c(1:2)]))
    set(cases, NULL, "index", seq_along(out))
    set(cases, NULL, "case", nms)
    setcolorder(cases, c("index", "case"))
    private$m_log$params <- cases
    private$m_log$simple <- FALSE

    model_ids <- viapply(seq_along(out), function(i) {
        private$m_model_store$add_model(out[[i]], role = "case",
            name = nms[[i]], source_path = NULL, sql = TRUE, dict = TRUE)
    })
    private$m_model_store$set_cases(integer())
    private$m_model_case_ids <- model_ids
    private$m_model_case_names <- nms
    param_refresh_run_cases(private)

    # log unique ids
    private$log_idf_uuid()
    private$log_new_uuid()
    private$m_log$unsaved <- rep(FALSE, private$m_model_store$case_count())

    if (bare) {
        mea_nm <- "function"
    } else {
        mea_nm <- surround(mea_nm)
    }
    verbose_info("Measure ", mea_nm, " has been applied with ", length(out),
        " new models created:\n",
        paste0(
            format_indexed_lines(nms,
                more = paste0("... and ", max(length(nms) - 20L, 0L),
                    " more. Use $cases() to inspect all cases.")
            ),
            collapse = "\n"
        ),
        class = "param_measure"
    )

    invisible(self)
}
# }}}
# param_run {{{
param_run <- function(self, private, output_dir = NULL, wait = TRUE,
                       force = FALSE, copy_external = FALSE, echo = wait,
                       separate = TRUE, readvars = TRUE) {
    if (!private$m_model_store$has_cases()) {
        abort("No measure has been applied.")
    }
    private$m_model_store$assert_cases_valid()

    private$log_new_uuid()
    if (is.null(output_dir)) output_dir <- private$m_model_store$default_dir(seed = TRUE)
    epgroup_run_models(self, private, output_dir, wait, force, copy_external, echo, separate, readvars)
}
# }}}
# param_save {{{
#' @importFrom checkmate assert_string
param_save <- function(self, private, dir = NULL, separate = TRUE, copy_external = FALSE) {
    assert_string(dir, null.ok = TRUE)
    if (!private$m_model_store$has_cases()) {
        abort("No parametric models found since no measure has been applied.")
    }
    private$m_model_store$assert_cases_valid()

    if (is.null(dir)) dir <- private$m_model_store$default_dir(seed = TRUE)

    if (!dir.exists(dir)) {
        # nocov start
        tryCatch(dir.create(dir, recursive = TRUE),
            warning = function(w) {
                stop("Failed to create output directory: ",
                     surround(dir), call. = FALSE)
            }
        )
        # nocov end
    }

    filename <- make_filename(private$m_model_store$case_names())

    if (separate) {
        path_param <- file.path(dir, filename, paste0(filename, ".idf"))
    } else {
        copy_external <- FALSE
        path_param <- file.path(dir, paste0(filename, ".idf"))
    }

    # save model
    path_param <- private$m_model_store$materialize_cases(path_param,
        copy_external = copy_external)$paths
    path_epw <- param_copy_weather_files(private$m_epws_path, dirname(path_param))

    data.table(model = path_param, weather = path_epw)
}
# }}}
# param_print {{{
param_print <- function(self, private) {
    path_idf <- private$m_model_store$model_source_path(private$m_model_store$seed_model_id())
    if (is.na(path_idf)) {
        path_idf <- private$m_model_store$model_prepared_path(private$m_model_store$seed_model_id())
    }
    param_print_header(private, path_idf)

    if (!private$m_model_store$has_cases()) {
        cli::cat_line("<< No measure has been applied >>",
            col = "white", background_col = "blue")
        return(invisible(self))
    }

    if (!private$m_model_store$cases_valid()) {
        cli::cat_line("<< Parametric models are stale. Regenerate them before running or saving. >>",
            col = "white", background_col = "red")
        return(invisible(self))
    }

    if (!private$m_log$simple && !private$m_log$bare) {
        cli::cat_line(paste0("Applied Measure: ", surround(private$m_log$measure_name)))
    }

    cli::cat_line(paste0("Parametric Models [", length(private$m_model_case_ids), "]: "))
    if (nrow(private$m_weather_cases) > 1L) {
        cli::cat_line(paste0("Weather Cases [", nrow(private$m_weather_cases), "]: "))
    }

    epgroup_print_status(self, private, epw = nrow(private$m_weather_cases) > 1L)
}
# }}}

# param_weather_cases {{{
param_weather_cases <- function(epws = NULL, names = NULL) {
    assert_character(names, null.ok = TRUE, any.missing = FALSE)

    if (is.null(epws)) {
        epws <- list(NULL)
    } else if (is_epw(epws) || checkmate::test_string(epws)) {
        epws <- list(epws)
    } else {
        epws <- as.list(epws)
    }

    if (!length(epws)) epws <- list(NULL)

    paths <- vcapply(epws, function(epw) {
        if (is.null(epw)) return(NA_character_)
        if (length(epw) == 1L && is.na(epw)) return(NA_character_)
        get_init_epw(epw)
    })

    if (!is.null(names)) {
        assert_same_len(paths, names)
        weather_names <- names
    } else if (checkmate::test_names(names(epws))) {
        weather_names <- names(epws)
    } else {
        weather_names <- tools::file_path_sans_ext(basename(paths))
        weather_names[is.na(paths)] <- "design_day"
    }

    weather_names <- make.unique(as.character(weather_names), sep = "_")

    data.table(
        weather_index = seq_along(paths),
        weather_case = weather_names,
        epw = paths
    )
}
# }}}
# param_refresh_run_cases {{{
param_refresh_run_cases <- function(private) {
    weather <- private$m_weather_cases
    if (is.null(weather) || !nrow(weather)) {
        weather <- param_weather_cases(NULL)
        private$m_weather_cases <- weather
    }

    has_model_cases <- length(private$m_model_case_ids) > 0L
    if (has_model_cases && !private$m_model_store$cases_valid()) {
        private$m_run_cases <- NULL
        return(invisible(FALSE))
    }

    if (has_model_cases) {
        model_ids <- private$m_model_case_ids
        model_names <- private$m_model_case_names
    } else if (nrow(weather) > 1L) {
        model_ids <- private$m_model_store$seed_model_id()
        model_names <- "seed"
    } else {
        private$m_model_store$set_cases(integer())
        private$m_run_cases <- NULL
        private$m_epws_path <- if (is.na(weather$epw[[1L]])) NULL else weather$epw[[1L]]
        private$m_log$unsaved <- logical()
        return(invisible(FALSE))
    }

    run <- data.table(
        model_index = rep(seq_along(model_ids), each = nrow(weather)),
        model_case = rep(model_names, each = nrow(weather)),
        model_id = rep(model_ids, each = nrow(weather)),
        weather_index = rep(weather$weather_index, times = length(model_ids)),
        weather_case = rep(weather$weather_case, times = length(model_ids)),
        epw = rep(weather$epw, times = length(model_ids))
    )

    if (nrow(weather) == 1L && has_model_cases) {
        case <- model_names
    } else if (!has_model_cases) {
        case <- weather$weather_case
    } else {
        case <- paste(run$model_case, run$weather_case, sep = "__")
    }
    set(run, NULL, "index", seq_len(nrow(run)))
    set(run, NULL, "case", make.unique(case, sep = "_"))
    setcolorder(run, c("index", "case", "model_index", "model_case",
        "model_id", "weather_index", "weather_case", "epw"))

    private$m_run_cases <- data.table::copy(run[, setdiff(names(run), "model_id"), with = FALSE])
    private$m_model_store$set_cases(run$model_id, run$case)
    private$m_epws_path <- if (all(is.na(run$epw))) NULL else run$epw
    private$m_log$unsaved <- rep(FALSE, nrow(run))

    invisible(TRUE)
}
# }}}
# param_normalize_case_names {{{
param_normalize_case_names <- function(names, n, type) {
    if (length(names) == 1L && n > 1L) {
        names <- paste(names, lpad(seq_len(n), "0"), sep = "_")
    } else if (length(names) != n) {
        abort(paste(
            "Invalid", type, "names found.",
            n, "cases exist but", length(names), "names given"
        ), "param_names")
    }

    make.unique(names, sep = "_")
}
# }}}
# param_assert_not_running {{{
param_assert_not_running <- function(private) {
    proc <- private$m_job
    if ((inherits(proc, "process") || inherits(proc, "r_process")) && proc$is_alive()) {
        abort(paste0(
            "Cannot change ParametricJob inputs while simulations are still running. ",
            "Please call `$kill()` first."
        ), "param_running")
    }
    invisible(TRUE)
}
# }}}
# param_clear_run_state {{{
param_clear_run_state <- function(private, new_uuid = TRUE) {
    private$m_job <- NULL
    private$m_log$start_time <- NULL
    private$m_log$end_time <- NULL
    private$m_log$killed <- NULL
    private$m_log$stdout <- NULL
    private$m_log$stderr <- NULL
    if (isTRUE(new_uuid)) private$log_new_uuid()
    invisible(TRUE)
}
# }}}
# param_copy_weather_files {{{
param_copy_weather_files <- function(epws, dirs) {
    out <- rep(NA_character_, length(dirs))
    if (is.null(epws)) return(out)

    if (length(epws) == 1L) epws <- rep(epws, length(dirs))
    assert_same_len(epws, dirs)

    has_epw <- !is.na(epws)
    if (any(has_epw)) {
        out[has_epw] <- file_copy(
            epws[has_epw],
            file.path(dirs[has_epw], basename(epws[has_epw]))
        )
    }
    out
}
# }}}
# param_print_header {{{
param_print_header <- function(private, path_idf) {
    weather <- private$m_weather_cases
    if (is.null(weather) || !nrow(weather) || all(is.na(weather$epw))) {
        path_epw <- NULL
    } else if (nrow(weather) == 1L) {
        path_epw <- weather$epw[[1L]]
    } else {
        path_epw <- NULL
    }

    print_job_header(title = "EnergPlus Parametric Simulation Job",
        path_idf = path_idf,
        path_epw = path_epw,
        eplus_ver = private$m_model_store$model_version(private$m_model_store$seed_model_id()),
        name_idf = "Seed", name_epw = "Weather"
    )
}
# }}}

#' @export
`==.ParametricJob` <- function(e1, e2) {
    if (!inherits(e2, "ParametricJob")) return(FALSE)
    identical(get_priv_env(e1)$uuid(), get_priv_env(e2)$uuid())
}

#' @export
`!=.ParametricJob` <- function(e1, e2) {
    Negate(`==.ParametricJob`)(e1, e2)
}

# vim: set fdm=marker:
