read_meta <- function(path = here::here("tools/meta.json")) {
    if (tools::file_ext(path) != "json") {
        stop(sprintf("'path' should has an extension of JSON but '%s' was found.", tools::file_ext(path)))
    }

    if (!file.exists(path)) {
        message(sprintf("Meta file '%s' does not exit. An empty JSON file has been created.", path))
        jsonlite::write_json(NULL, path)
    }

    c(list(path = path), jsonlite::read_json(path))
}

write_meta <- function(meta, ...) {
    d <- list(...)

    # reorder
    d <- c(meta[!names(meta) %in% c("path", names(d))], d)
    jsonlite::write_json(d[order(names(d))], meta$path, pretty = TRUE, auto_unbox = TRUE)
}

set_proxy <- function() {
    Sys.setenv(https_proxy = "http://127.0.0.1:7890")
    Sys.setenv(http_proxy = "http://127.0.0.1:7890")
    httr::set_config(httr::use_proxy("socks5://127.0.0.1:7890"))
}

unset_proxy <- function() {
    Sys.setenv(https_proxy = "")
    Sys.setenv(http_proxy = "")
    httr::reset_config()
}
