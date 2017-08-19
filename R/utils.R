################################################################################
#                           Miscellaneous Functions                            #
################################################################################

# lsos: A function of improved list of objects
# {{{1
.ls.objects <- function (pos = 1, pattern, order.by, decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           capture.output(format(utils::object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

# shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
# }}}1

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
    grepl("i[dm]f", ext, ignore.case = TRUE)
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
