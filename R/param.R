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
#' Basically, it is a collection of multiple `EplusJob` objects. However, the
#' model is first parsed and the [Idf] object is stored internally, instead of
#' storing only the path of Idf like in [EplusJob] class. Also, an object in
#' `Output:SQLite` with `Option Type` value of `SimpleAndTabular` will be
#' automatically created if it does not exists, like [Idf] class does.
#'
#' @section Usage:
#' ```
#' param <- param_job(idf, epw)
#' param$version()
#' param$seed()
#' param$weather()
#' param$apply_measure(measure, ..., .names = NULL)
#' param$models()
#' param$save(dir = NULL, separate = TRUE, copy_external = FALSE)
#' param$run(dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait)
#' param$kill()
#' param$status()
#' param$output_dir(which = NULL)
#' param$locate_output(which = NULL, suffix = ".err", strict = TRUE)
#' param$errors(which = NULL, info = FALSE)
#' param$list_table(which = NULL)
#' param$read_table(which = NULL, table)
#' param$read_rdd(which = NULL)
#' param$read_mdd(which = NULL)
#' param$report_data(which = NULL, key_value = NULL, name = NULL, year = NULL, tz = "UTC",
#'                   case = "auto", all = FALSE, wide = FALSE, period = NULL, month = NULL,
#'                   day = NULL, hour = NULL, minute = NULL, interval = NULL,
#'                   simulation_days = NULL, day_type = NULL, environment_name = NULL)
#' param$tabular_data(which, report_name = NULL, report_for = NULL, table_name = NULL,
#'                    column_name = NULL, row_name = NULL)
#' param$print()
#' ```
#' @section Create:
#' ```
#' param <- param_job(idf, epw)
#' ```
#'
#' **Arguments**
#'
#' * `idf`: Path to EnergyPlus IDF file or an `Idf` object.
#' * `epw`: Path to EnergyPlus EPW file or an `Epw` object. `epw` can also be
#'   `NULL` which will force design-day-only simulation when `$run()` method is
#'   called. Note this needs at least one `Sizing:DesignDay` object exists in
#'   the [Idf].
#'
#' @section Get Seed Model and Weather:
#' ```
#' param$version()
#' param$seed()
#' param$weather()
#' ```
#'
#' `$version()` returns the version of input [Idf] object.
#'
#' `$seed()` returns the input [Idf] object.
#'
#' `$weather()` returns the input [Epw] object. If no [Epw] is provided when
#' creating the `ParametricJob` object, `NULL` is returned.
#'
#' @section Apply Design Alternatives:
#' ```
#' param$apply_measure(measure, ..., .names = NULL)
#' ```
#'
#' `$apply_measure()` allows to apply a measure to an [Idf] and creates
#' parametric models for analysis. Basically, a measure is just a function that
#' takes an [Idf] object and other arguments as input, and returns a modified
#' [Idf] object as output. Use `...` to supply different arguments, **except for
#' the first `Idf` argument**, to that measure. Under the hook, [base::mapply()]
#' is used to create multiple [Idf]s according to the input values.
#'
#' **Arguments**
#'
#' * `measure`: A function that takes an `Idf` and other arguments as input and
#'     returns an `Idf` object as output.
#' * `...`: Arguments **except first `Idf` argument** that are passed to that
#'   `measure`.
#' * `.names`: A character vector of the names of parametric `Idf`s. If `NULL`,
#'     the new `Idf`s will be named in format `measure_name + number`.
#'
#' @section Extract models:
#' ```
#' param$models()
#' param$save(dir = NULL, separate = TRUE, copy_external = FALSE)
#' ```
#'
#' `$models()` returns a list of parametric models generated using input [Idf]
#' object and `$apply_measure()` method. Model names are assigned in the same
#' way as the `.names` arugment in `$apply_measure()`. If no measure has been
#' applied, `NULL` is returned. Note that it is not recommended to conduct any
#' extra modification on those models directly, after they were created using
#' `$apply_measure()`, as this may lead to an un-reproducible process. A warning
#' message will be issued if any of those models has been modified when running
#' simulations.
#'
#' `$save()` saves all parametric models in specified folder. An error will be
#' issued if no measure has been applied. `$save()` returns a
#' [data.table][data.table::data.table()] with two columns:
#'
#' * model: The path of saved parametric model files.
#' * weather: The path of saved weather files.
#'
#' **Arguments**
#'
#' * `dir`: The parent output directory for models to be saved. If `NULL`, the
#'   directory of the seed model will be used. Default: `NULL`.
#' * `separate`: If `TRUE`, all models are saved in a separate folder with each
#'   model's name under specified directory. If `FALSE`, all models are saved in
#'   the specified directory. Default: `TRUE`.
#' * `copy_external`: Only applicable when `separate` is `TRUE`. If `TRUE`, the
#'   external files that every `Idf` object depends on will also be copied into
#'   the saving directory. The values of file paths in the Idf will be changed
#'   automatically. Currently, only `Schedule:File` class is supported.  This
#'   ensures that the output directory will have all files needed for the model
#'   to run. Default: `FALSE`.
#'
#' @section Run and Collect Results:
#' ```
#' param$run(dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait)
#' param$kill()
#' param$status()
#' param$output_dir(which = NULL)
#' param$locate_output(which = NULL, suffix = ".err", strict = TRUE)
#' param$errors(which = NULL, info = FALSE)
#' param$list_table(which = NULL)
#' param$read_table(which = NULL, table)
#' param$read_rdd(which = NULL)
#' param$read_mdd(which = NULL)
#' param$report_data_dict(which = NULL)
#' param$report_data(which = NULL, key_value = NULL, name = NULL, year = NULL, tz = "UTC",
#'                   case = "auto", all = FALSE, wide = FALSE, period = NULL, month = NULL,
#'                   day = NULL, hour = NULL, minute = NULL, interval = NULL,
#'                   simulation_days = NULL, day_type = NULL, environment_name = NULL)
#' param$tabular_data(which, report_name = NULL, report_for = NULL, table_name = NULL,
#'                    column_name = NULL, row_name = NULL)
#' ```
#'
#' All those functions have the same meaning as in [EplusJob] class, except
#' that they only return the results of specified simulations. Most arguments
#' have the same meanings as in [EplusJob] class.
#'
#' `$run()` runs all parametric simulations in parallel. The number of parallel
#' EnergyPlus process can be controlled by `eplusr_option("num_parallel")`. If
#' `wait` is FALSE, then the job will be run in the background. You can get
#' updated job status by just printing the `ParametricJob` object.
#'
#' `$kill()` kills all background EnergyPlus processes that are current running
#' if possible. It only works when simulations run in non-waiting mode.
#'
#' `$status()` returns a named list of values indicates the status of the job:
#'
#'   * `run_before`: `TRUE` if the job has been run before. `FALSE` otherwise.
#'   * `alive`: `TRUE` if the job is still running in the background. `FALSE`
#'     otherwise.
#'   * `terminated`: `TRUE` if the job was terminated during last
#'      simulation. `FALSE` otherwise. `NA` if the job has not been run yet.
#'   * `successful`: `TRUE` if last simulation ended successfully. `FALSE`
#'     otherwise. `NA` if the job has not been run yet.
#'   * `changed_after`: `TRUE` if the *seed model* has been modified since last
#'      simulation. `FALSE` otherwise.
#'   * `job_status`: A [data.table][data.table::data.table()] contains meta data
#'     for each simulation job. For details, please see [run_multi()]. If the
#'     job has not been run before, a null
#'     [data.table][data.table::data.table()] is returned.
#'
#' $errors() returns an [ErrFile][read_err()] object which contains all contents
#' of the simulation error file (`.err`). If `info` is `FALSE`, only warnings
#' and errors are printed.
#'
#' `$output_dir()` returns the output directory of specified simulations.
#'
#' `$locate_output()` returns the path of a single output file of specified
#' simulations.
#'
#' `$list_table()` returns a list of character vectors containing all available
#' table and view names in the SQLite file.
#'
#' `$read_table()` takes a valid table name of those from `$list_table()` and
#' returns that table data in a [data.table][data.table::data.table()] format.
#'
#' `$read_rdd()` and `$read_mdd()` return the core data of Report Data
#' Dictionary (RDD) file and Meter Data Dictionary (MDD) file respectively. For
#' details, please see [read_rdd()].
#'
#' `$report_data_dict()` returns a [data.table][data.table::data.table()] which
#' contains all information about report data for specified simulations. For
#' details on the meaning of each columns, please see "2.20.2.1
#' ReportDataDictionary Table" in EnergyPlus "Output Details and Examples"
#' documentation.
#'
#' `$report_data()` extracts the report data in a
#' [data.table][data.table::data.table()] using key values, variable names and
#' other arguments.
#'
#' `$tabular_data()` extracts tabular data in a
#' [data.table][data.table::data.table()].
#'
#' For convenience, input character arguments matching in `$report_data()` and
#' `$tabular_data()` are **case-insensitive**.
#'
#' For `$read_table()`, `$read_rdd()`, `$read_mdd()`, `$report_data_dict()`,
#' `$report_data()` and `$tabular_data()`, the returned data.table has a `case`
#' column in the returned [data.table][data.table::data.table()] that indicates
#' the names of models. For detailed documentation on the results of those
#' methods, please see [EplusJob].
#'
#' **Arguments**
#'
#' * `which`: An integer vector of the indexes or a character vector or names of
#'   parametric simulations. If `NULL`, results of all parametric simulations
#'   are returned. Default: `NULL`.
#' * `dir`: The parent output directory for specified simulations. Outputs of
#'   each simulation are placed in a separate folder under the parent directory.
#' * `wait`: If `TRUE`, R will hang on and wait all EnergyPlus simulations
#'   finish. If `FALSE`, all EnergyPlus simulations are run in the background.
#'   Default: `TRUE`.
#' * `force`: Only applicable when the last simulation runs with `wait` equals
#'   to `FALSE` and is still running. If `TRUE`, current running job is
#'   forced to stop and a new one will start. Default: `FALSE`.
#' * `copy_external`: If `TRUE`, the external files that every `Idf` object
#'   depends on will also be copied into the simulation output directory. The
#'   values of file paths in the Idf will be changed automatically. Currently,
#'   only `Schedule:File` class is supported.  This ensures that the output
#'   directory will have all files needed for the model to run. Default is
#'   `FALSE`.
#' * `echo`: Only applicable when `wait` is `TRUE`. Whether to simulation
#'   status. Default: The same value of `wait`.
#' * `suffix`: A string that indicates the file extension of simulation output.
#'   Default: `".err"`.
#' * `strict`: If `TRUE`, it checks if the simulation was terminated, is
#'   still running or the file does not exist. Default: `TRUE`.
#' * `info`: If `FALSE`,only warnings and errors are printed. Default: `FALSE`.
#' * `key_value`: A character vector to identify key values of the data. If
#'   `NULL`, all keys of that variable will be returned. `key_value` can also be
#'   data.frame that contains `key_value` and `name` columns. In this case,
#'   `name` argument in `$report_data()` is ignored. All available `key_value`
#'   for current simulation output can be obtained using `$report_data_dict()`.
#'   Default: `NULL`.
#' * `name`: A character vector to identify names of the data. If
#'   `NULL`, all names of that variable will be returned. If `key_value` is a
#'   data.frame, `name` is ignored. All available `name` for current simulation
#'   output can be obtained using `$report_data_dict()`.  Default: `NULL`.
#' * `year`: Year of the date time in column `datetime`. If `NULL`, it
#'    will calculate a year value that meets the start day of week restriction
#'    for each environment. Default: `NULL`.
#' * `tz`: Time zone of date time in column `datetime`. Default: `"UTC"`.
#' * `case`: If not `NULL`, a character column will be added indicates the case
#'   of this simulation. If `"auto"`, the name of the IDF file without extension
#'   is used.
#' * `all`: If `TRUE`, extra columns are also included in the returned
#'   [data.table][data.table::data.table()].
#' * `wide`: If `TRUE`, the output is formated in the same way as standard
#'   EnergyPlus csv output file.
#' * `period`: A Date or POSIXt vector used to specify which time period to
#'    return. The year value does not matter and only month, day, hour and
#'    minute value will be used when subsetting. If `NULL`, all time period of
#'    data is returned. Default: `NULL`.
#' * `month`, `day`, `hour`, `minute`: Each is an integer vector for month, day,
#'    hour, minute subsetting of `datetime` column when querying on the SQL
#'    database. If `NULL`, no subsetting is performed on those components. All
#'    possible `month`, `day`, `hour` and `minute` can be obtained using
#'    `$read_table("Time")`.  Default: `NULL`.
#' * `interval`: An integer vector used to specify which interval length of
#'    report to extract. If `NULL`, all interval will be used. Default: `NULL`.
#' * `simulation_days`: An integer vector to specify which simulation day data
#'    to extract. Note that this number resets after warmup and at the beginning
#'    of an environment period. All possible `simulation_days` can be obtained
#'    using `$read_table("Time")`. If `NULL`, all simulation days will be used.
#'    Default: `NULL`.
#' * `day_type`: A character vector to specify which day type of data to
#'    extract. All possible day types are: `Sunday`, `Monday`, `Tuesday`,
#'   `Wednesday`, `Thursday`, `Friday`, `Saturday`, `Holiday`,
#'   `SummerDesignDay`, `WinterDesignDay`, `CustomDay1`, and `CustomDay2`. All
#'   possible values for current simulation output can be obtained using
#'   `$read_table("Time")`.
#' * `environment_name`: A character vector to specify which environment data to
#'    extract. If `NULL`, all environment data are returned. Default: `NULL`.
#'    All possible `environment_name` for current simulation output can
#'    be obtained using
#'
#'    ```
#'    $read_table("EnvironmentPeriods")
#'    ```
#'
#' * `report_name`, `report_for`, `table_name`, `column_name`, `row_name`:
#'   Each is a character vector for subsetting when querying the SQL database.
#'   For the meaning of each argument, please see the description above.
#'
#' @section Printing:
#' ```
#' param$print()
#' print(param)
#' ```
#'
#' `$print()` shows the core information of this `ParametricJob`, including the
#' path of seed model and weather, the version and path of EnergyPlus used to
#' run simulations, the measure that has been applied and parametric models
#' generated, and also the simulation job status.
#'
#' `$print()` is quite useful to get the simulation status, especially when
#' `wait` is `FALSE` in `$run()`. The job status will be updated and printed
#' whenever `$print()` is called.
#'
#' @examples
#' \dontrun{
#' if (is_avail_eplus(8.8)) {
#'     idf_name <- "1ZoneUncontrolled.idf"
#'     epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'
#'     idf_path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", idf_name)
#'     epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData", epw_name)
#'
#'     # create from local files
#'     param_job(idf_path, epw_path)
#'
#'     # create from an Idf and an Epw object
#'     param <- param_job(read_idf(idf_path), read_epw(epw_path))
#'
#'     # get the seed model
#'     param$seed()
#'
#'     # get the weather
#'     param$weather()
#'
#'     # get status of current job
#'     param$status()
#'
#'     # create a measure to change the orientation of the building
#'     rotate_building <- function (idf, degree = 0L) {
#'         if (!idf$is_valid_class("Building")) {
#'            stop("Input model does not have a Building object")
#'         }
#'
#'         if (degree > 360 || degree < -360 ) {
#'             stop("Input degree should in range [-360, 360]")
#'         }
#'
#'         cur <- idf$Building$North_Axis
#'
#'         new <- cur + degree
#'
#'         if (new > 360) {
#'             new <- new %% 360
#'             warning("Calculated new north axis is greater than 360. ",
#'                 "Final north axis will be ", new
#'             )
#'         } else if (new < -360) {
#'             new <- new %% -360
#'             warning("Calculated new north axis is smaller than -360. ",
#'                 "Final north axis will be ", new
#'             )
#'         }
#'
#'         idf$Building$North_Axis <- new
#'
#'         idf
#'     }
#'
#'     # apply measure
#'     # this will create 12 models
#'     param$apply_measure(rotate_building, degree = seq(30, 360, 30))
#'     # apply measure with new names specified
#'     param$apply_measure(rotate_building, degree = seq(30, 360, 30),
#'         .names = paste0("rotate_", seq(30, 360, 30))
#'     )
#'
#'     # extract all parametric models
#'     param$models()
#'
#'     # save all parametric models with each model in a separate folder
#'     param$save(tempdir())
#'
#'     # save all parametric models with all models in the same folder
#'     param$save(tempdir(), separate = FALSE)
#'
#'     # run parametric simulations
#'     param$run(wait = TRUE)
#'
#'     # run in background
#'     param$run(wait = FALSE)
#'     # get detailed job status by printing
#'     print(param)
#'
#'     # status now includes a data.table with detailed information on each simulation
#'     param$status()
#'
#'     # print simulation errors
#'     param$errors()
#'
#'     # extract output of all simulations
#'     param$report_data()
#'
#'     # extract only some simulations
#'     param$report_data(c(1, 3))
#'     param$tabular_data(c(1, 3))
#'     param$report_data(c("rotate_30", "rotate_120"))
#'     param$tabular_data(c("rotate_30", "rotate_120"))
#'
#'     # get output directory
#'     param$output_dir()
#'     param$output_dir(c(1, 3))
#'
#'     # get path of specific output file
#'     param$locate_output(c(1, 3), ".csv")
#' }
#' }
#' @docType class
#' @name ParametricJob
#' @author Hongyuan Jia
NULL

#' Create An EnergyPlus Parametric Simulation Job
#'
#' `param_job()` takes an IDF and EPW as input and returns a `ParametricJob`.
#' For details on `ParametricJob`, please see [ParametricJob] class.
#'
#' @param idf A path to EnergyPlus IDF or IMF file or an `Idf` object.
#' @param epw A path to EnergyPlus EPW file or an `Epw` object. `epw` can also
#' be `NULL` which will force design-day-only simulation when
#' [`$run()`][ParametricJob] method is called. Note this needs at least one
#' `Sizing:DesignDay` object exists in the [Idf].
#' @return A `ParametricJob` object.
#' @examples
#' if (is_avail_eplus(8.8)) {
#'     idf_name <- "1ZoneUncontrolled.idf"
#'     epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'
#'     idf_path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", idf_name)
#'     epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData", epw_name)
#'
#'     # create from local files
#'     param_job(idf_path, epw_path)
#'
#'     # create from an Idf and an Epw object
#'     param_job(read_idf(idf_path), read_epw(epw_path))
#' }
#' @seealso [eplus_job()] for creating an EnergyPlus single simulation job.
#' @export
#' @author Hongyuan Jia
# param_job {{{
param_job <- function (idf, epw) {
    Parametric$new(idf, epw)
}
# }}}

# Parametric {{{
Parametric <- R6::R6Class(classname = "ParametricJob", cloneable = FALSE,
    inherit = EplusGroup,
    public = list(

        # INITIALIZE {{{
        initialize = function (idf, epw) {
            private$m_seed <- get_init_idf(idf)
            if (!is.null(epw)) private$m_epws <- list(get_init_epw(epw))

            # add Output:SQLite if necessary
            add_sql <- idf_add_output_sqlite(private$m_seed)
            # add Output:VariableDictionary if necessary
            add_dict <- idf_add_output_vardict(private$m_seed)
            # log if the input idf has been changed
            private$m_log$unsaved <- add_sql || add_dict

            # save uuid
            private$m_log$seed_uuid <- ._get_private(private$m_seed)$m_log$uuid
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        version = function ()
            param_version(self, private),

        seed = function ()
            param_seed(self, private),

        models = function ()
            param_models(self, private),

        weather = function ()
            param_weather(self, private),

        apply_measure = function (measure, ..., .names = NULL)
            param_apply_measure(self, private, measure, ..., .names = .names),

        save = function (dir = NULL, separate = TRUE, copy_external = FALSE)
            param_save(self, private, dir, separate, copy_external),

        run = function (dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait)
            param_run(self, private, dir, wait, force, copy_external, echo),

        print = function ()
            param_print(self, private)
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_seed = NULL,
        m_idfs = NULL,
        m_epws = NULL,
        m_job = NULL,
        m_log = NULL
        # }}}
    )
)
# }}}

# param_version {{{
param_version <- function (self, private) {
    private$m_seed$version()
}
# }}}
# param_seed {{{
param_seed <- function (self, private) {
    private$m_seed
}
# }}}
# param_models {{{
param_models <- function (self, private) {
    private$m_idfs
}
# }}}
# param_weather {{{
param_weather <- function (self, private) {
    private$m_epws[[1L]]
}
# }}}
# param_apply_measure {{{
param_apply_measure <- function (self, private, measure, ..., .names = NULL) {
    assert(is.function(measure))

    if (length(formals(measure)) < 2L) {
        abort("error_measure_no_arg", "`measure` function must have at least two argument.")
    }

    measure_wrapper <- function (idf, ...) {
        assert(is_idf(idf), msg = paste0("Measure should take an `Idf` object as input, not `", class(idf)[[1]], "`."))
        idf <- idf$clone(deep = TRUE)
        idf <- measure(idf, ...)
        assert(is_idf(idf), msg = paste0("Measure should return an `Idf` object, not `", class(idf)[[1]], "`."))
        idf
    }

    mea_nm <- deparse(substitute(measure, parent.frame()))
    private$m_log$measure_name <- mea_nm

    out <- mapply(measure_wrapper, ...,
        MoreArgs = list(idf = private$m_seed), SIMPLIFY = FALSE, USE.NAMES = FALSE)

    if (is.null(.names)) {
        if (length(mea_nm) > 1L) mea_nm <- "case"
        nms <- paste0(mea_nm, "_", seq_along(out))
    } else {
        assert(have_same_len(out, .names),
            msg = paste0(length(out), " models created with only ", length(.names), " names given.")
        )
        nms <- make.unique(as.character(.names), sep = "_")
    }

    setattr(out, "names", nms)

    private$m_idfs <- out

    # log unique ids
    private$m_log$uuid <- vcapply(private$m_idfs, function (idf) ._get_private(idf)$m_log$uuid)

    if (eplusr_option("verbose_info")) {
        if (length(private$m_log$measure_name) > 1L) {
            mea_nm <- "function"
        } else {
            mea_nm <- surround(mea_nm)
        }
        verbose_info("Measure ", mea_nm, " has been applied with ", length(out),
            " new models created:\n", paste0("[", lpad(seq_along(nms), "0"), "]", ": ",
            nms, collapse = "\n")
        )
    }

    invisible(self)
}
# }}}
# param_run {{{
param_run <- function (self, private, output_dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait) {
    if (is.null(private$m_idfs)) {
        abort("error_no_measured_applied", "No measure has been applied.")
    }

    # check if generated models have been modified outside
    uuid <- vapply(private$m_idfs, function (idf) ._get_private(idf)$m_log$uuid, character(1))
    if (any(uuid != private$m_log$uuid)) {
        warn("warning_param_modified",
            paste0(
                "Some of the parametric models have been modified after created using `$apply_measure()`. ",
                "Running these models will result in simulation outputs that may be not reproducible. ",
                "It is recommended to re-apply your original measure using `$apply_measure()` and call `$run()` again. ",
                "Models that have been modified are listed below:\n",
                paste0(" # ", seq_along(uuid)[uuid != private$m_log$uuid]," | ",
                    names(uuid)[uuid != private$m_log$uuid], collapse = "\n"
                )
            )
        )
    }

    epgroup_run_models(self, private, output_dir, wait, force, copy_external, echo)
}
# }}}
# param_save {{{
param_save <- function (self, private, dir = NULL, separate = TRUE, copy_external = FALSE) {
    if (is.null(private$m_idfs)) {
        abort("error_no_measured_applied",
            "No parametric models found since no measure has been applied."
        )
    }

    # restore uuid
    uuid <- vcapply(private$m_idfs, function (idf) ._get_private(idf)$m_log$uuid)

    path_idf <- normalizePath(private$m_seed$path(), mustWork = TRUE)

    if (is.null(private$m_epws)) {
        path_epw <- NULL
    } else {
        path_epw <- vcapply(private$m_epws, function (epw) epw$path())
    }

    if (is.null(dir))
        dir <- dirname(path_idf)
    else {
        assert(is_string(dir))
    }

    if (!dir.exists(dir)) {
        tryCatch(dir.create(dir, recursive = TRUE),
            warning = function (w) {
                stop("Failed to create output directory: ",
                     surround(dir), call. = FALSE)
            }
        )
    }

    nms <- names(private$m_idfs)
    filename <- make_filename(nms)

    if (separate) {
        path_param <- file.path(dir, filename, paste0(filename, ".idf"))
    } else {
        copy_external <- FALSE
        path_param <- file.path(dir, paste0(filename, ".idf"))
    }

    # save model
    path_param <- apply2_chr(private$m_idfs, path_param,
        function (x, y) x$save(y, overwrite = TRUE, copy_external = copy_external)
    )
    # copy weather
    if (!is.null(path_epw)) {
        path_epw <- copy_run_files(path_epw, unique(dirname(path_param)))
    } else {
        path_epw <- NA_character_
    }

    # assign original uuid in case it is updated when saving
    # if not assign original here, the model modification checkings in `$run()`
    # may be incorrect.
    for (i in seq_along(uuid)) {
        log <- ._get_private(private$m_idfs[[i]])$m_log
        log$uuid <- uuid[[i]]
    }

    data.table::data.table(model = path_param, weather = path_epw)
}
# }}}
# param_print {{{
param_print <- function (self, private) {
    path_epw <- if (is.null(private$m_epws)) NULL else vcapply(private$m_epws, function (epw) epw$path())
    print_job_header(title = "EnergPlus Parametric Simulation Job",
        path_idf = private$m_seed$path(),
        path_epw = path_epw,
        eplus_ver = private$m_seed$version(),
        name_idf = "Seed", name_epw = "Weather"
    )

    if (is.null(private$m_idfs)) {
        cli::cat_line("<< No measure has been applied >>",
            col = "white", background_col = "blue")
        return(invisible())
    }

    cli::cat_line(c(
        paste0("Applied Measure: ", surround(private$m_log$measure_name)),
        paste0("Parametric Models [", length(private$m_idfs), "]: ")
    ))

    epgroup_print_status(self, private, epw = FALSE)
}
# }}}
