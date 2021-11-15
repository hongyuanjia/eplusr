#' Reload Idf data
#'
#' @details
#' eplusr relies heavily on the [data.table][data.table::data.table()] package.
#' The core data of all main classes in eplusr are saved as
#' [data.table::data.table()]s. This introduces a problem when loading saved
#' [Idf] objects or other class objects via an `*.RDS` and `*.RData` file on
#' disk: the stored [data.table::data.table()]s lose their column
#' over-allocation. `reload()` is a helper function that calls
#' [data.table::setDT()] on all internal [data.table::data.table()]s to make
#' sure they are initialized properly.
#'
#' It is recommended to call `reload()` on each [Idd], [Idf] and other
#' class object in eplusr loaded with [readRDS()] or [load()], to make sure all
#' eplusr's functionaries works properly.
#'
#' @param x An object of class [Idd], [IddObject], [Idf], [IdfObject], [Epw],
#'        [EplusJob], [EplusGroupJob] or [ParametricJob] object. Any object of
#'        other class will be directly returned without any modifications.
#'
#' @param ... further arguments passed to or from other methods. Currently not
#'        used.
#'
#' @return The input object with its internal [data.table::data.table()]s
#' properly initialized.
#'
#' @export
reload <- function (x, ...) {
    UseMethod("reload")
}

reload_idd_env <- function (idd_env) {
    idd_env$group <- setDT(idd_env$group)
    idd_env$class <- setDT(idd_env$class)
    idd_env$field <- setDT(idd_env$field)
    idd_env$reference <- setDT(idd_env$reference)
    idd_env
}
reload_idf_env <- function (idf_env) {
    idf_env$object <- setDT(idf_env$object)
    idf_env$value <- setDT(idf_env$value)
    idf_env$reference <- setDT(idf_env$reference)
    idf_env
}
reload_log_env <- function (log_env) {
    log_env$order <- setDT(log_env$order)
    log_env
}

#' @export
reload.default <- function (x, ...) {
    x
}

#' @export
reload.Idf <- function (x, ...) {
    reload_idd_env(get_priv_env(x)$idd_env())
    reload_idf_env(get_priv_env(x)$idf_env())
    reload_log_env(get_priv_env(x)$m_log)
    x
}

#' @export
reload.Idd <- function (x, ...) {
    reload_idd_env(get_priv_env(x)$m_idd_env)
    x
}

#' @export
reload.IddObject <- function (x, ...) {
    reload_idd_env(get_priv_env(x)$idd_env())
    x
}

#' @export
reload.Epw <- function (x, ...) {
    priv <- get_priv_env(x)
    reload_idd_env(priv$idd_env())
    reload_idf_env(priv$idf_env())
    reload_log_env(priv$m_log)
    priv$m_data <- setDT(priv$m_data)
    x
}

#' @export
reload.EplusJob <- function (x, ...) {
    priv <- get_priv_env(x)
    reload.Idf(priv$m_idf)

    if (!is.null(priv$m_job$file)) {
        priv$m_job$file <- setDT(priv$m_job$file)
    }
    if (!is.null(priv$m_job$run)) {
        priv$m_job$run <- setDT(priv$m_job$run)
    }

    x
}

#' @export
reload.EplusGroupJob <- function (x, ...) {
    priv <- get_priv_env(x)
    for (idf in priv$m_idfs) reload.Idf(idf, ...)

    if (inherits(priv$m_job$jobs, "data.table")) {
        priv$m_job$jobs <- setDT(priv$m_job$jobs)
    }

    x
}

#' @export
reload.ParametricJob <- function (x, ...) {
    priv <- get_priv_env(x)
    reload.Idf(priv$m_seed)
    if (!is.null(priv$m_idfs)) for (idf in priv$m_idfs) reload.Idf(idf, ...)

    if (inherits(priv$m_job$jobs, "data.table")) {
        priv$m_job$jobs <- setDT(priv$m_job$jobs)
    }
    x
}
