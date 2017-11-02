page_main <- "https://www.energyplus.net"

wthr_download <- function(search, savedir = getwd(), type = c("all", "ddy", "epw", "stat")) {
    src <- wthr_search(search = search)
    cat("Search completed.", nrow(src), "results returned:\n",
        "Please type the number of results you want to download:\n")
    sel <- utils::menu(c(src[["station"]], "All"))

    dl_type <- rlang::arg_match(type)
    tryCatch({
        if (sel == 0L) {
            dest_src <- NULL
        } else if (sel <= nrow(src)) {
            dest_src <- src[sel,]
        } else {
            dest_src <- src
        }},
        interrupt = function(c) "Nothing selected. Download canceled."
    )

    if (!is.null(dest_src)) {
        dl_tbl <- dest_src %>%
            dplyr::mutate(dl_info = purrr::map(path, wthr_get_dl_info)) %>%
            tidyr::unnest() %>%
            dplyr::select(-path) %>%
            dplyr::filter(type == dl_type)
    } else {
        message("Nothing selected. Download canceled.")
        return(invisible())
    }

    purrr::pwalk(dl_tbl, wthr_download_sgl, dir = savedir)

}

wthr_search <- function(search = NULL) {
    page_wthrsearch <- paste0(page_main, "/weather-search")

    results_src_raw <- xml2::read_html(paste0(page_wthrsearch, "/", search)) %>%
        rvest::html_node(".region") %>%
        rvest::html_node(".btn-group-vertical") %>%
        rvest::html_children()

    src_name <- rvest::html_text(results_src_raw)
    src_path <- rvest::html_attr(results_src_raw, name = "href")
    results_src <- dplyr::tibble(station = src_name, path = src_path)

    return(results_src)
}

wthr_get_dl_info <- function(path) {
    # Get the base file name in order to rename the downloaded zip file.
    dl_basename <- basename(path)
    results_dl_raw <- read_html(paste0(page_main, path)) %>%
        html_node(".region") %>%
        html_node(".btn-group-vertical") %>%
        html_children()

    dl_type <- html_text(results_dl_raw)
    dl_type[length(dl_type)] <- "all"
    dl_url <- paste0(page_main, html_attr(results_dl_raw, "href"))

    dl_filename <- c(basename(dl_url[-length(dl_url)]), paste0(dl_basename, ".zip"))

    results_dl <- tibble(type = dl_type, url = dl_url, filename = dl_filename)

    return(results_dl)
}

wthr_download_sgl <- function(station, type, url, filename, dir = getwd()) {
    cat("Start downloading weather source", sQuote(station), "for type",
        sQuote(type), "...\n\n")

    filepath <- file.path(dir, filename)

    tryCatch(
        # Have to set mode to 'wb', othewise the downloaded file will be not readable.
        download.file(url, filepath, mode = "wb", quite = TRUE),
        error = function(c) paste0("Failed to downloaded file: \n", url)
    )
    cat("File downloaded sucessfully, located at:\n", filepath, "\n")

    return(invisible())
}