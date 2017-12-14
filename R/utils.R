################################################################################
#                           Miscellaneous Functions                            #
################################################################################
# has_*_ext {{{1
has_ext <- function (path, ext) {
    grepl(ext, tools::file_ext(path), ignore.case = TRUE, perl = TRUE)
}

assertthat::on_failure(has_ext) <- function (call, env = parent.env) {
    path <- eval(call$path, env)
    ext <- eval(call$ext, env)
    msg("File ", sQuote(basename(path)), " does not have extension ", sQuote(ext), ".")
}

has_model_ext <- function (x) {
    ext <- tools::file_ext(x)
    grepl("(i[dm]f|expidf)$", ext, ignore.case = TRUE)
}

has_epw_ext <- function (x) {
    ext <- tools::file_ext(x)
    grepl("epw", ext, ignore.case = TRUE)
}

has_idf_ext <- function (x) {
    ext <- tools::file_ext(x)
    grepl("i[dm]f", ext, ignore.case = TRUE)
}

has_imf_ext <- function (x) {
    ext <- tools::file_ext(x)
    grepl("i[dm]f", ext, ignore.case = TRUE)
}

has_epg_ext <- function (x) {
    ext <- tools::file_ext(x)
    grepl("epg", ext, ignore.case = TRUE)
}

has_epat_ext <- function (x) {
    ext <- tools::file_ext(x)
    grepl("epat", ext, ignore.case = TRUE)
}

has_json_ext <- function (x) {
    ext <- tools::file_ext(x)
    grepl("json", ext, ignore.case = TRUE)
}
# }}}1

`%||%` <- function (x, y) {
    if (is.null(x)) {
        y
    } else {
        x
    }
}

# file_* {{{1
file_path <- function (..., normalize = TRUE) {
    os <- Sys.info()['sysname']
    if (os == "Windows") {
        fsep = "\\"
    } else {
        fsep = "/"
    }

    if (normalize) {
        path <- normalizePath(file.path(..., fsep = fsep), mustWork = FALSE)
    } else {
        path <- file.path(..., fsep = fsep)
    }

    return(path)
}

file_prefix <- function (x, basename = TRUE) {
    if (basename) tools::file_path_sans_ext(basename(x)) else tools::file_path_sans_ext(x)
}
# }}}1

is_empty <- function (x) {
    length(x) == 0L
}

msg <- function (..., prefix = " ", initial = "") {
    paste(strwrap(paste0(...)), collapse = "\n")
}

# file_exists: Case-sensitive file existence checking {{{1
file_exists <- function (...) {
    files <- normalizePath(c(...), winslash = "/", mustWork = FALSE)
    dirs <- dirname(files)
    all_files <- purrr::set_names(
        purrr::map(dirs, list.files, full.names = TRUE),
        files
    )
    purrr::map2_lgl(files, all_files,
        ~any(grepl(pattern = .x, x = .y, fixed = TRUE))
    )
}
# }}}1

# get_suffix_type {{{1
get_suffix_type <- function (prefix) {
    ori_wd <- getwd()
    on.exit(setwd(ori_wd), add = TRUE)
    setwd(dirname(prefix))
    type <- c("table", "meter", "sizing")

    get_sgl_type_suffix <- function (prefix, type) {
        all_suffixes <- c("C", "L", "D")
        purrr::set_names(
            purrr::map_lgl(all_suffixes,
                ~any(file_exists(
                    output_files(prefix = prefix, suffix_type = .x,
                        type = type, simplify = TRUE))
                )
            ),
            all_suffixes
        )
    }

    idx <- purrr::map_lgl(
        purrr::simplify_all(
            purrr::transpose(
                purrr::map(type, ~get_sgl_type_suffix(prefix, .x))
            )
        ),
        any
    )

    suffix <- names(which(idx))

    return(suffix)
}
# }}}1

# csQuote{{{
csQuote <- function (x, and = TRUE) {
    x_sq <- sQuote(x)
    if (length(x_sq) > 1L & and) {
        x_sq[length(x_sq)] <- paste0("and ", x_sq[length(x_sq)])
    }

    x_csq <- paste0(x_sq, collapse = ", ")

    return(x_csq)
}
# }}}
