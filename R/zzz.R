.onLoad <- function(libname, pkgname) {
    # detect all available EnergyPlus installed in normal locations
    find_eplus <- function (ver) {
        suppressMessages(tryCatch(use_eplus(ver),
            error = function (e) NULL))
    }

    lapply(rev(all_eplus_release_commit()$version), find_eplus)
}
