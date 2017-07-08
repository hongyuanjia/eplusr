################################################################################
#                              On-load Functions                               #
################################################################################

.onAttach <- function (libname, pkgname) {
    packageStartupMessage("Welcome to `eplusr`.")
}

.onLoad <- function(libname, pkgname) {
    op <- options()
    op.eplusr <- list(
       eplusr.temp_dir = normalizePath(file.path(tempdir(), "eplusr"), mustWork = FALSE),
       eplusr.eplus_dir = find_eplus(),
       eplusr.parallel_num = parallel::detectCores(),
  )
  toset <- !(names(op.eplusr) %in% names(op))
  if(any(toset)) options(op.eplusr[toset])

  if (!dir.exists(op.eplusr$eplusr.temp_dir)) {
      dir.create(op.eplusr$eplusr.temp_dir, showWarnings = FALSE, recursive = TRUE)
  }

  invisible()
}
