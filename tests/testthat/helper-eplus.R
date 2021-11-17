if (identical(Sys.getenv("NOT_CRAN"), "true") && !is_avail_eplus(8.8)) {
    install_eplus(8.8)
}
