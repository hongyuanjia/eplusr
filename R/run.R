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
        if (!dir.exists(eplus_home)) {
            stop(msg("Cannot locate EnergyPlus V", ver, " at ",
                     sQuote(eplus_home), ". Please give 'path'."))
        }
    } else if (!is.null(path)) {
        if (!dir.exists(eplus_home)) stop(msg(sQuote(path), " does not exists."))
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

    energyplus_idd <- normalizePath(
        file.path(eplus_home, "Energy+.idd"), winslash = "/", mustWork = FALSE
    )

    if (!file.exists(eplus_exe)) {
        stop(msg("EnergyPlus executable does not exist in the folder."))
    }

    if (!file.exists(energyplus_idd)) {
        warning(msg(sQuote("Energy+.idd"), " does not exist in EnergyPlus
                    installation folder."))
        energyplus_idd <- NULL
    }

    return(c(eplus_home = eplus_home, eplus_exe = eplus_exe, idd = energyplus_idd))
}
# }}}
# dash_ver {{{
dash_ver <- function (ver) {
    assert_that(is_eplus_ver(ver))
    paste0(sub(".", "-", ver, fixed = TRUE), "-0")
}
# }}}
