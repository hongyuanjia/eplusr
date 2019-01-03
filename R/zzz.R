.onLoad <- function(libname, pkgname) {
    locate_eplus()
    reg_custom_units()
    reset_clone_indicator()
    check_color()
}

.onAttach <- function(libname, pkgname) {
    locate_eplus()
    reg_custom_units()
    reset_clone_indicator()
    check_color()
}
