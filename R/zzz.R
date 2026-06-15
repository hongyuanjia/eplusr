.onLoad <- function(libname, pkgname) {
    # To get rid of the CRAN check NOTE like:
    # > Running R code in 'testthat.R' had CPU time 6.6 times elapsed time
    # See: https://github.com/Rdatatable/data.table/issues/5658
    if (any(grepl("_R_CHECK", names(Sys.getenv()), fixed = TRUE))) {
        data.table::setDTthreads(2)
    }
    reg_custom_units()
    S7::methods_register()
}
