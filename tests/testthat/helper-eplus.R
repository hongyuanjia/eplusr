if (identical(Sys.getenv("NOT_CRAN"), "true") && !is_avail_eplus(LATEST_EPLUS_VER)) {
    install_eplus(LATEST_EPLUS_VER, local = TRUE)
}
