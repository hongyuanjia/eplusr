#' Create and Run Parametric Analysis, and Collect Results
#'
#' `ParametricJob` class provides a prototype of conducting parametric analysis
#' of EnergyPlus simulations.
#'
#' Basically, it is a collection of multiple `EplusJob` objects.
#'
#' @section Usage:
#' ```
#' param <- param_job(idf, epw)
#' param$apply_measure(measure, ..., .names = NULL)
#' param$run(dir = NULL, parallel_backend = future::multiprocess)
#' param$kill(which = NULL)
#' param$status(which = NULL)
#' param$errors(info = FALSE)
#' param$output_dir(which = NULL)
#' param$locate_output(which = NULL, suffix = ".err", strict = TRUE)
#' param$report_data_dict(which = NULL)
#' param$report_data(which = NULL, key_value = NULL, name = NULL, year = NULL, tz = "GMT", case = "auto")
#' param$tabular_data(which = NULL)
#' param$print()
#' ```
#' @section Create:
#' ```
#' param <- param_job(idf, epw)
#' ```
#'
#' **Arguments**
#'
#' * `idf`: Path to EnergyPlus IDF or IMF file or an `Idf` object.
#' * `epw`: Path to EnergyPlus EPW file or an `Epw` object.
#'
#' @section Get Seed Model and Weather:
#' ```
#' param$seed()
#' param$weather()
#' ```
#'
#' `$seed()` will return the input `Idf` object.
#'
#' `$weather()` will return the input `Epw` object.
#'
#' @section Apply Design Alternatives:
#' ```
#' param$apply_measure(measure, ..., .names = NULL)
#' ```
#'
#' `$apply_measure()` allows to apply a measure to an `Idf` and creates
#'     parametric models for analysis. Basically, a measure is just a function
#'     that takes an `Idf` object and other arguments as input, and returns a
#'     modified `Idf` object as output. Use `...` to supply different arguments
#'     to that measure. Under the hook, [mapply()] is used to create multiple
#'     `Idf`s according to the input values.
#'
#' **Arguments**
#'
#' * `measure`: A function that takes an `Idf` and other arguments as input and
#'     returns an `Idf` object as output.
#' * `...`: Other arguments passed to that `measure`.
#' * `.names`: A character vector of the names of parametric `Idf`s. If `NULL`,
#'     the new `Idf`s will be named in format `measure_name + number`.
#'
#' @section Run and Collect Results:
#' ```
#' param$run(dir = NULL, parallel_backend = future::multiprocess)
#' param$kill(which = NULL)
#' param$status(which = NULL)
#' param$errors(info = FALSE)
#' param$output_dir(which = NULL)
#' param$locate_output(which = NULL, suffix = ".err", strict = TRUE)
#' param$report_data_dict(which = NULL)
#' param$report_data(which = NULL, key_value = NULL, name = NULL, year = NULL, tz = "GMT", case = "auto")
#' param$tabular_data(which = NULL)
#' ```
#'
#' All those functions have the same meaning in [EplusJob class][job], except that they
#' only return the results of specified simulation.
#'
#' **Arguments**
#'
#' * `which`: An integer vector of the indexes or a character vector or names of
#'     parametric simulations.
#' * `parallel_backend`: Any acceptable input for [future::plan()].
#'
#' All other arguments have the same meanings as in [EplusJob class][job].
#'
#' @docType class
#' @name param
#' @author Hongyuan Jia
NULL

#' Create An EnergyPlus Parametric Simulation Job
#'
#' `param_job()` takes an IDF and EPW as input and returns a `ParametricJob`.
#' For details on `ParametricJob`, please see [ParametricJob class][param].
#'
#' @param idf A path to EnergyPlus IDF or IMF file or an `Idf` object.
#' @param epw A path to EnergyPlus EPW file or an `Epw` object.
#' @return A `ParametricJob` object.
#' @seealso [eplus_job()] for creating an EnergyPlus single simulation job.
#' @export
# param_job {{{
param_job <- function (idf, epw) {
    Parametric$new(idf, epw)
}
# }}}

#' @importFrom R6 R6Class
#' @importFrom future multiprocess
#' @importFrom data.table setattr rbindlist
#' @importFrom purrr walk2 map map2 map_chr
#' @importFrom cli cat_rule cat_bullet cat_line
# Parametric {{{
Parametric <- R6::R6Class(classname = "ParametricJob", cloneable = FALSE,
    public = list(

        # INITIALIZE {{{
        initialize = function (idf, epw) {

            if (is_idf(idf)) {
                private$m_idf <- idf$clone()
            } else {
                private$m_idf <- read_idf(idf)
            }

            # add sql output
            idf_self <- ._get_self(private$m_idf)
            idf_priv <- ._get_private(private$m_idf)
            i_idf_add_output_sqlite(idf_self, idf_priv)

            if (is_epw(epw)) {
                private$m_epw <- epw
            } else {
                private$m_epw <- read_epw(epw)
            }
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        seed = function ()
            private$m_idf,

        weather = function ()
            private$m_epw,

        apply_measure = function (measure, ..., .names = NULL)
            i_param_apply_measure(self, private, measure, ..., .names = NULL),

        run = function (dir = NULL, parallel_backend = future::multiprocess)
            i_param_run(self, private, dir, wait = TRUE, parallel_backend),

        kill = function (which = NULL)
            i_param_kill(self, private, which),

        status = function (which = NULL)
            i_param_status(self, private, which),

        output_dir = function (which = NULL)
            i_param_output_dir(self, private, which),

        locate_output = function (which = NULL, suffix = ".err", strict = TRUE)
            i_param_locate_output(self, private, which, suffix, strict),

        errors = function (which = NULL, info = FALSE)
            i_param_output_errors(self, private, which, info),

        report_data_dict = function (which = NULL)
            i_param_report_data_dict(self, private, which),

        report_data = function (which = NULL, key_value = NULL, name = NULL,
                                all = FALSE, year = NULL, tz = "GMT")
            i_param_report_data(self, private, which, key_value, name, all, year, tz),

        tabular_data = function(which = NULL)
            i_param_tabular_data(self, private, which),

        print = function ()
            i_param_print(self, private)
        # }}}
    ),

    # PRIVATE FIELDS {{{
    private = list(
        m_idf = NULL,
        m_epw = NULL,
        m_job = NULL,
        m_sql = NULL,
        m_log = NULL,
        m_param = NULL
    )
    # }}}
)
# }}}

# i_param_apply_measure {{{
i_param_apply_measure <- function (self, private, measure, ..., .names = NULL) {
    assert_that(is.function(measure))

    measure_wrapper <- function (idf, ...) {
        assert_that(is_idf(idf))
        idf <- idf$clone()
        measure(idf, ...)
    }

    mea_nm <- deparse(substitute(measure, parent.frame()))
    private$m_log$measure_name <- mea_nm

    out <- mapply(measure_wrapper, ...,
        MoreArgs = list(idf = private$m_idf), SIMPLIFY = FALSE, USE.NAMES = FALSE)

    if (is.null(.names)) {
        out_nms <- paste0(mea_nm, "_", seq_along(out))
    } else {
        if (!is_same_len(out, .names))
            stop(length(out), " models created with only ", length(.names),
                " names given.", call. = FALSE)
        nms <- as.character(.names)
        out_nms <- make.names(gsub(" ", "_", fixed = TRUE, make.unique(nms, sep = "_")))
    }

    data.table::setattr(out, "names", out_nms)

    private$m_param <- out

    message("Measure ", backtick(mea_nm), " has been applied with ", length(out),
        " new models created:\n", paste0(seq_along(out_nms), ": ", out_nms, collapse = "\n"))
}
# }}}

# i_param_job_from_which {{{
i_param_job_from_which <- function (self, private, which) {
    jobs <- private$m_job

    if (is.null(jobs))
        stop("The parametric has not been run before.", call. = FALSE)
    if (is.null(which)) return(jobs)

    if (is.character(which)) {
        nms <- names(jobs)
        valid <- which %in% nms
        if (any(!valid))
            stop("Invalid job name found for current Parametric: ",
                backtick_collapse(which), ".", call. = FALSE)

    } else if (are_count(which)) {
        valid <- which <= length(jobs)
        if (any(!valid))
            stop("Invalid job index found for current Parametric: ",
                backtick_collapse(which), ".", call. = FALSE)
    } else {
        stop("`which` should either be a character or an integer vector.",
            call. = FALSE)
    }

    jobs[which]
}
# }}}

# i_param_run {{{
i_param_run <- function (self, private, output_dir = NULL, wait = TRUE, parallel_backend = future::multiprocess) {
    if (is.null(private$m_param))
        stop("No measure has been applied.", call. = FALSE)

    ver <- private$m_idf$version()

    nms <- names(private$m_param)

    path_idf <- normalizePath(private$m_idf$path(), mustWork = TRUE)
    path_epw <- normalizePath(private$m_epw$path(), mustWork = TRUE)

    if (is.null(output_dir))
        output_dir <- dirname(path_idf)
    else {
        assert_that(is_string(output_dir))
    }

    if (!dir.exists(output_dir)) {
        tryCatch(dir.create(output_dir, recursive = TRUE),
            warning = function (w) {
                stop("Failed to create output directory: ",
                     backtick(output_dir), call. = FALSE)
            }
        )
    }

    path_param <- file.path(output_dir, nms, paste0(nms, ".idf"))

    purrr::walk2(private$m_param, path_param,
        ~.x$save(.y, overwrite = TRUE, copy_external = TRUE))

    private$m_job <- purrr::map(private$m_param, EplusJob$new, epw = path_epw)

    start_time <- Sys.time()
    proc <- run_multi(ver, path_param, path_epw, parallel_backend = parallel_backend)
    end_time <- Sys.time()

    assign_proc <- function (job, proc, start_time, end_time) {
        priv <- ._get_private(job)
        priv$m_process <- proc
        priv$m_log$start_time <- start_time
        priv$m_log$end_time <- end_time
    }

    purrr::map2(private$m_job, proc, assign_proc,
        start_time = start_time, end_time = end_time)

    private$m_job
}
# }}}

# i_param_kill {{{
i_param_kill <- function (self, private, which) {
    message("Currently, parametric simulations can only be run in waiting mode, ",
        "and cannot be kill.")
    return(invisible(NULL))

    job <- i_param_job_from_which(self, private, which)
    for (j in job) {
        j$kill()
    }
}
# }}}

# i_param_status {{{
i_param_status <- function (self, private, which) {
    job <- i_param_job_from_which(self, private, which)
    purrr::map(job, ~.x$status())
}
# }}}

# i_param_output_dir {{{
i_param_output_dir <- function (self, private, which) {
    job <- i_param_job_from_which(self, private, which)
    purrr::map_chr(job, ~.x$output_dir())
}
# }}}

# i_param_locate_output {{{
i_param_locate_output <- function (self, private, which, suffix = ".err", strict = TRUE) {
    job <- i_param_job_from_which(self, private, which)
    purrr::map_chr(job, ~.x$locate_output(suffix = suffix, strict = strict))
}
# }}}

# i_param_output_errors {{{
i_param_output_errors <- function (self, private, which, info = FALSE) {
    job <- i_param_job_from_which(self, private, which)
    purrr::map(job, ~.x$errors(info = info))
}
# }}}

# i_param_report_data_dict {{{
i_param_report_data_dict <- function (self, private, which) {
    job <- i_param_job_from_which(self, private, which)
    purrr::map(job, ~.x$report_data_dict())
}
# }}}

# i_param_report_data {{{
i_param_report_data <- function (self, private, which, key_value = NULL,
                                 name = NULL, all = FALSE, year = NULL, tz = "GMT") {
    job <- i_param_job_from_which(self, private, which)
    data.table::rbindlist(purrr::map(job,
        ~.x$report_data(key_value = key_value, name = name, year = year, tz = tz)))
}
# }}}

# i_param_tabular_data {{{
i_param_tabular_data <- function (self, private, which = NULL) {
    job <- i_param_job_from_which(self, private, which)
    res <- data.table::rbindlist(purrr::map(job,
        ~{
            d <- .x$tabular_data()
            case <- tools::file_path_sans_ext(basename(
                .x$locate_output(".idf", strict = FALSE)))
            d[, Case := case]
            d
        })
    )

    data.table::setcolorder(res, c("Case", setdiff(names(res), "Case")))

    res[]
}
# }}}

# i_param_print {{{
i_param_print <- function (self, private) {
    cli::cat_rule("EnergPlus Parametric")
    cli::cat_bullet(c(
        paste0("Seed Model: ", backtick(normalizePath(private$m_idf$path(), mustWork = FALSE))),
        paste0("Default Weather: ", backtick(private$m_epw$path()))
    ))

    if (is.null(private$m_param)) {
        cli::cat_line("<< No measure has been applied >>")
        return(invisible())
    } else {
        cli::cat_bullet(c(
            paste0("Applied Measure: ", backtick(private$m_log$measure_name)),
            paste0("Parametric Models [", length(private$m_param), "]: ")
        ))

        cli::cat_line(paste0("  - ", names(private$m_param), collapse = "\n"))
    }
}
# }}}

