.onLoad <- function(libname, pkgname) {
    locate_eplus()
    reg_custom_units()
}

.onAttach <- function(libname, pkgname) {
    locate_eplus()
    reg_custom_units()
}
