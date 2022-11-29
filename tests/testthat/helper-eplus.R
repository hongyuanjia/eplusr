if (identical(Sys.getenv("NOT_CRAN"), "true") && !is_avail_eplus(22.1)) {
    install_eplus(22.1, local = TRUE)
}
