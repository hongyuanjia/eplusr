page_main <- "https://www.energyplus.net"

#' @title Download weather file from EnergyPlus website
#' @description Weather data for more than 2100 locations are now available in
#' EnergyPlus weather format — 1042 locations in the USA, 71 locations in
#' Canada, and more than 1000 locations in 100 other countries throughout the
#' world. The weather data are arranged by World Meteorological Organization
#' region and Country. \code{wthr_download} will search the weather database in
#' EnergyPlus website, return the results and automatically download the weather
#' file.
#' @param search A string used to search in the weather database.
#' @param savedir Directory where the weather file will be downloaded to. Default: getwd()
#' @param type The desired file format. Should be one of "all", "ddy", "epw", or
#' "stat".
#' @param quiet Whether to show the progress bar of downloading. Default: TRUE
#' @examples
#' \dontrun{
#' if(interactive()){
#'     wthr_download("chongqing")
#'  }
#' }
#' @export
#' @importFrom utils menu
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate select filter
#' @importFrom purrr map pwalk
#' @importFrom tidyr unnest
wthr_download <- function(search, savedir = getwd(),
                          type = c("all", "ddy", "epw", "stat"), quiet = FALSE) {
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

    purrr::pwalk(dl_tbl, wthr_download_sgl, dir = savedir, quiet = quiet)

}

#' @title Search the weather file database in EnergyPlus.net
#' @description \code{wthr_search} serach the weather file database in
#' EnergyPlus website and return the returned inquired results.
#' @param search A string used to search in the weather database.
#' @return A data frame contains the returned weather source names and locations.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  wthr_search("Chongqing")
#'  }
#' }
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_node html_children html_text html_attr
#' @importFrom dplyr tibble
wthr_search <- function(search) {
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
    results_dl_raw <- xml2::read_html(paste0(page_main, path)) %>%
        rvest::html_node(".region") %>%
        rvest::html_node(".btn-group-vertical") %>%
        rvest::html_children()

    dl_type <- rvest::html_text(results_dl_raw)
    dl_type[length(dl_type)] <- "all"
    dl_url <- paste0(page_main, rvest::html_attr(results_dl_raw, "href"))

    dl_filename <- c(basename(dl_url[-length(dl_url)]), paste0(dl_basename, ".zip"))

    results_dl <- dplyr::tibble(type = dl_type, url = dl_url, filename = dl_filename)

    return(results_dl)
}

wthr_download_sgl <- function(station, type, url, filename, dir = getwd(), quiet = TRUE) {
    cat("\nStart downloading weather source", sQuote(station), "for type",
        sQuote(type), "...\n\n")

    filepath <- file.path(dir, filename)

    tryCatch(
        # Have to set mode to 'wb', othewise the downloaded file will be not readable.
        download.file(url, filepath, mode = "wb", quiet = quiet),
        error = function(c) paste0("Failed to downloaded file: \n", url)
    )
    cat("File downloaded sucessfully, located at:\n", filepath, "\n")

    return(invisible())
}
