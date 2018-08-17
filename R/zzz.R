.onLoad <- function(libname, pkgname) {
    locate_eplus()
}

.onAttach <- function(libname, pkgname) {
    locate_eplus()
}
