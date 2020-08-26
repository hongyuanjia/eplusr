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
        #' if (is_avail_eplus(8.8)) {
        #'     idf_name <- "1ZoneUncontrolled.idf"
        #'     epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
        #'
        #'     idf_path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", idf_name)
        #'     epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData", epw_name)
        #'
        #'     # create from an IDF and an EPW
        #'     param <- param_job(idf_path, epw_path)
        #'     param <- ParametricJob$new(idf_path, epw_path)
        #'
        #'     # create from an Idf and an Epw object
        #'     param_job(read_idf(idf_path), read_epw(epw_path))
        #' }
        #' }
        #'
        initialize = function (idf, epw) {
            # add Output:SQLite and Output:VariableDictionary if necessary
            idf <- get_init_idf(idf, sql = TRUE, dict = TRUE, csv = TRUE)

            private$m_seed <- idf

            # log if the input idf has been changed
            private$m_log <- new.env(hash = FALSE, parent = emptyenv())
            private$m_log$unsaved <- attr(idf, "sql") || attr(idf, "dict") || attr(idf, "csv")

            if (!is.null(epw)) private$m_epws_path <- get_init_epw(epw)

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
        version = function ()
            param_version(self, private),
        # }}}

        # seed {{{
        #' @description
        #' Get the seed [Idf] object
        #'
        #' @details
        #' `$seed()` returns the parsed input seed [Idf] object.
        #'
        #' @examples
        #' \dontrun{
        #' param$seed()
        #' }
        #'
        seed = function ()
            param_seed(self, private),
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
        weather = function ()
            param_weather(self, private),
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
        #' arugment in
        #' \href{../../eplusr/html/ParametricJob.html#method-apply_measure}{\code{$apply_measure()}}.
        #' If no measure has been applied, `NULL` is returned. Note that it is
        #' not recommended to conduct any extra modification on those models
        #' directly, after they were created using
        #' \href{../../eplusr/html/ParametricJob.html#method-apply_measure}{\code{$apply_measure()}},
        #' as this may lead to an un-reproducible process. A warning message
        #' will be issued if any of those models has been modified when running
        #' simulations.
        #'
        #' @examples
        #' \dontrun{
        #' param$models()
        #' }
        #'
        models = function ()
            param_models(self, private),
        # }}}

        # apply_measure {{{
        #' @description
        #' Create parametric models
        #'
        #' @details
        #' `$apply_measure()` allows to apply a measure to an [Idf] and creates
        #' parametric models for analysis. Basically, a measure is just a
        #' function that takes an [Idf] object and other arguments as input, and
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
        #' rotate_building <- function (idf, degree = 0L) {
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
        apply_measure = function (measure, ..., .names = NULL)
            param_apply_measure(self, private, measure, ..., .names = .names),
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
        #'        Currently, only `Schedule:File` class is supported.  This
        #'        ensures that the output directory will have all files needed
        #'        for the model to run. Default: `FALSE`.
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
        save = function (dir = NULL, separate = TRUE, copy_external = FALSE)
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
        #'        under the parent directory.
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
        run = function (dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait)
            param_run(self, private, dir, wait, force, copy_external, echo),
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
        print = function ()
            param_print(self, private)
        # }}}
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_seed = NULL,
        # }}}
        # PRIVATE FUNCTIONS {{{
        seed_uuid = function () get_priv_env(private$m_seed)$m_log$uuid,
        log_seed_uuid = function () private$m_log$seed_uuid <- private$seed_uuid(),
        cached_seed_uuid = function () private$m_log$seed_uuid
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
param_job <- function (idf, epw) {
    ParametricJob$new(idf, epw)
}
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
    if (is.null(private$m_epws_path)) NULL else read_epw(private$m_epws_path)
}
# }}}
# param_apply_measure {{{
#' @importFrom checkmate assert_function
param_apply_measure <- function (self, private, measure, ..., .names = NULL, .env = parent.frame()) {
    checkmate::assert_function(measure)

    if (length(formals(measure)) < 2L) {
        stop("'measure' function must have at least two argument")
    }

    measure_wrapper <- function (idf, ...) {
        if (!is_idf(idf)) {
            stop("Measure should take an 'Idf' object as input, not '", class(idf)[[1]], "'")
        }
        idf <- idf$clone(deep = TRUE)
        idf <- measure(idf, ...)
        if (!is_idf(idf)) {
            stop("Measure should return an 'Idf' object, not '", class(idf)[[1]], "'")
        }
        idf
    }

    if (is.name(substitute(measure, .env))) {
        bare <- FALSE
        mea_nm <- deparse(substitute(measure, .env))
    } else {
        bare <- TRUE
        mea_nm <- "case"
    }
    private$m_log$measure_name <- mea_nm

    out <- mapply(measure_wrapper, ...,
        MoreArgs = list(idf = private$m_seed), SIMPLIFY = FALSE, USE.NAMES = FALSE)

    if (is.null(.names)) {
        nms <- paste0(mea_nm, "_", seq_along(out))
    } else {
        if (length(out) != length(.names)) {
            stop(paste0(length(out), " models created with only ", length(.names), " names given"))
        }
        nms <- make.unique(as.character(.names), sep = "_")
    }

    setattr(out, "names", nms)

    private$m_idfs <- out

    # log unique ids
    private$log_idf_uuid()
    private$log_new_uuid()
    private$m_log$unsaved <- rep(TRUE, length(out))

    if (eplusr_option("verbose_info")) {
        if (bare) {
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
        abort("No measure has been applied.")
    }

    # check if generated models have been modified outside
    uuid <- private$idf_uuid()
    if (any(i <- uuid != private$cached_idf_uuid())) {
        warn(paste0(
                "Some of the parametric models have been modified after created using `$apply_measure()`. ",
                "Running these models will result in simulation outputs that may be not reproducible. ",
                "It is recommended to re-apply your original measure using `$apply_measure()` and call `$run()` again. ",
                "Models that have been modified are listed below:\n",
                paste0(" # ", seq_along(uuid)[i], " | ", names(uuid)[i], collapse = "\n")
            ),
            "param_model_modified"
        )
        private$log_unsaved(which(i))
    }

    private$log_new_uuid()
    epgroup_run_models(self, private, output_dir, wait, force, copy_external, echo)
}
# }}}
# param_save {{{
#' @importFrom checkmate assert_string
param_save <- function (self, private, dir = NULL, separate = TRUE, copy_external = FALSE) {
    if (is.null(private$m_idfs)) {
        abort("No parametric models found since no measure has been applied.")
    }

    # restore uuid
    uuid <- private$idf_uuid()

    path_idf <- normalizePath(private$m_seed$path(), mustWork = TRUE)

    if (is.null(dir))
        dir <- dirname(path_idf)
    else {
        assert_string(dir)
    }

    if (!dir.exists(dir)) {
        tryCatch(dir.create(dir, recursive = TRUE),
            warning = function (w) {
                stop("Failed to create output directory: ",
                     surround(dir), call. = FALSE)
            }
        )
    }

    filename <- make_filename(names(private$m_idfs))

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
    path_epw <- private$m_epws_path
    if (!is.null(path_epw)) {
        path_epw <- copy_run_files(path_epw, unique(dirname(path_param)))
    } else {
        path_epw <- NA_character_
    }

    # assign original uuid in case it is updated when saving
    # if not assign original here, the model modification checkings in `$run()`
    # may be incorrect.
    for (i in seq_along(uuid)) {
        log <- get_priv_env(private$m_idfs[[i]])$m_log
        log$uuid <- uuid[[i]]
    }

    data.table::data.table(model = path_param, weather = path_epw)
}
# }}}
# param_print {{{
param_print <- function (self, private) {
    print_job_header(title = "EnergPlus Parametric Simulation Job",
        path_idf = private$m_seed$path(),
        path_epw = private$m_epws_path,
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

#' @export
`==.ParametricJob` <- function (e1, e2) {
    if (!inherits(e2, "ParametricJob")) return(FALSE)
    identical(get_priv_env(e1)$uuid(), get_priv_env(e2)$uuid())
}

#' @export
`!=.ParametricJob` <- function (e1, e2) {
    Negate(`==.ParametricJob`)(e1, e2)
}
