# eplus_path {{{
eplus_path <- function (ver = NULL, path = NULL) {
    os <- Sys.info()['sysname']
    if (!is.null(ver)) {
        assert_that(is_eplus_ver(ver))
        ver_dash <- dash_ver(ver)
        eplus_home <- switch(os,
            "Windows" = paste0("C:/EnergyPlusV", ver_dash),
            "Linux" = paste0("/usr/local/EnergyPlus-", ver_dash),
            "Darwin" = paste0("/Applications/EnergyPlus-", ver_dash))
    } else if (!is.null(path)) {
        eplus_home <- path
    } else {
        stop("Both 'ver' and 'path' are NULL.", call. = FALSE)
    }

    ext <- ""
    if (os == "Windows") ext <- ".exe"

    eplus_exe <- normalizePath(
        file.path(eplus_home, paste0("energyplus", ext)),
        winslash = "/", mustWork = FALSE
    )

    return(c(eplus_home = eplus_home, eplus_exe = eplus_exe))
}

# }}}
# dash_ver {{{
dash_ver <- function (ver) {
    assert_that(is_eplus_ver(ver))
    paste0(sub(".", "-", ver, fixed = TRUE), "-0")
}
# }}}
