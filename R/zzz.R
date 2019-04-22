.onLoad <- function(libname, pkgname) {
    locate_eplus()
    reg_custom_units()
    check_color()
}

.onAttach <- function(libname, pkgname) {
    locate_eplus()
    reg_custom_units()
    check_color()
}
