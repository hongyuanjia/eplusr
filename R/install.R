#' @importFrom data.table data.table fread
#' @importFrom tools file_path_sans_ext
#' @importFrom cli rule
NULL

#' Download and Install EnergyPlus
#'
#' Download specified version of EnergyPlus for your platform from GitHub and
#' install it.
#'
#' @param ver The EnergyPlus version number, e.g., `8.7`. The special value
#'     `"latest"`, which is the default, means the latest version.
#'
#' @param force Whether to install EnergyPlus even if it has already been
#'     installed.
#'
#' @param dir Where to save EnergyPlus installer file. For `install_eplus()`,
#' the installer will be saved into [tempdir()]
#'
#' @details
#'
#' `download_eplus()` downloads specified version of EnergyPlus from
#' [EnergyPlus GitHub Repository](https://github.com/NREL/EnergyPlus).
#'
#' `install_eplus()` will try to install EnergyPlus into the default location,
#' e.g.  `C:\\EnergyPlusVX-Y-0` on Windows, `/usr/local/EnergyPlus-X-Y-0` on
#' Linux, and `/Applications/EnergyPlus-X-Y-0` on macOS.
#'
#' Note that the installation process requires administrative privileges
#' during the installation and you have to run R with administrator (or with
#' sudo if you are on Linux) to make it work if you are not in interactive mode.
#'
#' @name install_eplus
#' @return An invisible integer `0` if succeed. Moreover, some attributes will
#' also be returned:
#' * For `install_eplus()`:
#'     - `path`: the EnergyPlus installation path
#'     - `installer`: the path of downloaded EnergyPlus installer file
#' * For `download_eplus()`:
#'     - `file`: the path of downloaded EnergyPlus installer file
#'
#' @examples
#' \dontrun{
#'
#' # for the latest version of EnergyPlus
#' download_eplus("latest", dir = tempdir())
#' install_eplus("latest")
#'
#' # for a specific version of EnergyPlus
#' download_eplus(8.8, dir = tempdir())
#' install_eplus(8.8)
#' }
#' @author Hongyuan Jia
#' @export
# install_eplus {{{
install_eplus <- function (ver = "latest", force = FALSE) {
    ver <- standardize_ver(ver)

    # check if the same version has been installed already
    if (is_avail_eplus(ver) && !isTRUE(force))
        stop(paste0("It seems EnergyPlus v", ver, "has been already",
                "installed at ", backtick(eplus_config(ver)$dir),
                ". Set `force` to TRUE to reinstall."), call. = FALSE)

    message(sprintf("Starting to download EnergyPlus v%s...", ver), "\n", cli::rule(line = 2))

    dl <- download_eplus(ver, tempdir())

    message(sprintf("Starting to install EnergyPlus v%s...", ver), "\n", cli::rule(line = 2))

    message("NOTE: Administrative privileges required during installation. ",
            "Please make sure R is running with an administrator acount or equivalent.")

    inst <- attr(dl, "file")
    res <- switch(os_type(),
           windows = install_eplus_win(inst),
           linux = install_eplus_linux(inst),
           macos = install_eplus_macos(inst))

    if (res != 0L) stop("Failed to install EnergyPlus v", ver, ".", call. = FALSE)

    path <- eplus_default_path(ver)
    message(sprintf("EnergyPlus v%s successfully installed into %s.", ver, path))

    # add newly installed EnergyPlus to dictionary
    use_eplus(ver)

    res <- 0L
    attr(res, "path") <- path
    attr(res, "installer") <- inst

    invisible(res)
}
# }}}

#' @name install_eplus
#' @export
# download_eplus {{{
download_eplus <- function (ver = "latest", dir) {
    ver <- standardize_ver(ver)
    url <- eplus_download_url(ver)
    file <- basename(url)

    dest <- normalizePath(file.path(dir, file), mustWork = FALSE)
    dl <- download_file(url, dest)

    if (dl != 0L) stop("Failed to download EnergyPlus v", ver, ".", call. = FALSE)

    message("The installer file of EnergyPlus ", paste0("v", ver), " ",
        backtick(file), " has been successfully downloaded into ", dir, ".")

    attr(dl, "file") <- dest
    invisible(dl)
}
# }}}

# eplus_download_url: get EnergyPlus installer download URL {{{
eplus_download_url <- function (ver) {
    cmt <- eplus_release_commit(ver)

    if (is_empty(cmt))
        stop("Failed to get installer data for EnergyPlus v", ver, ". ",
             "All available version are: ",
             backtick_collapse(all_cmt$version), ".", call. = FALSE)

    os <- switch(os_type(), windows = "Windows", macos = "Darwin", linux = "Linux")
    if (!is_windows() ||
        identical(Sys.info()[['machine']], "x86-64") ||
        identical(Sys.info()[['machine']], "x86_64")) {
        arch <- "x86_64"
    } else {
        arch <- "i386"
    }
    ext <- switch(os_type(), windows = "exe", macos = "dmg", linux = "sh")

    base_url <- "https://github.com/NREL/EnergyPlus/releases/download/"
    file <- sprintf("EnergyPlus-%s-%s-%s-%s.%s", cmt$version, cmt$commit, os, arch, ext)
    paste0(base_url,"v", cmt$version, "/", file)
}
# }}}
# eplus_release_commit: return EnergyPlus release commit data {{{
eplus_release_commit <- function(ver) {
    ver <- standardize_ver(ver)

    assert_that(is_eplus_ver(ver))

    all_eplus_release_commit()[version == as.character(ver)]
}
# }}}
# download_file: same as download.file except that it creates the target directory if necessary {{{
download_file <- function (url, dest) {
    if (file.exists(dest))
        tryCatch(unlink(dest),
            warning = function (w) {
                stop("Failed to delete the existing file ",
                    backtick(dest), "before downloading.", call. = FALSE)
            }
        )

    dest_dir <- dirname(dest)

    if (!dir.exists(dest_dir))
        tryCatch(dir.create(dest_dir, recursive = TRUE), warning = function(w) stop(w, call. = FALSE))

    utils::download.file(url, dest, mode = "wb")
}
# }}}
# install_eplus_win {{{
install_eplus_win <- function (exec) {
    system(sprintf("%s /S", exec))
}
# }}}
# install_eplus_linux {{{
install_eplus_linux <- function (exec) {
    # change working directory
    ori_wd <- getwd()
    on.exit(setwd(ori_wd), add = TRUE)

    exe_dir <- dirname(exec)
    setwd(exe_dir)

    f <- basename(exec)
    v <- gsub("\\.", "-", stringr::str_match(f, "EnergyPlus-(\\d\\.\\d\\.\\d)-")[,2])
    system(sprintf('chmod +x %s', f))
    system(sprintf('echo "y\r" | sudo ./%s', f))
    system(sprintf('sudo chmod -R a+w /usr/local/EnergyPlus-%s', v))
}
# }}}

#' Configure which version of EnergyPlus to use
#'
#' @param eplus An acceptable EnergyPlus version or an EnergyPlus installation
#'        path.
#' @param ver An acceptable EnergyPlus version.
#'
#' @details
#'
#' `use_eplus()` adds an EnergyPlus version into the EnergyPlus version
#'     cache in eplusr. That cache will be used to get corresponding
#'     [Idd] object when parsing IDF files and call corresponding
#'     EnergyPlus to run models.
#'
#' `eplus_config()` returns the a list of configure data of specified version of
#' EnergyPlus. If no data found, an empty list will be returned.
#'
#' `avail_eplus()` returns all versions of available EnergyPlus.
#'
#' `is_avail_eplus()` checks if the specified version of EnergyPlus is
#' available or not.
#'
#' @return
#' * For `use_eplus()` and `eplus_config()`, an (invisible for
#'   `use_eplus()`) list of three contains EnergyPlus version, directory and
#'   EnergyPlus executable.  version of EnergyPlus;
#' * For `avail_eplus()`, a character vector;
#' * For `is_avis_avail_eplus()`, a scalar logical vector.
#'
#' @rdname use_eplus
#' @examples
#' \dontrun{
#' # add specific version of EnergyPlus
#' use_eplus(8.9)
#' use_eplus("8.8.0")
#'
#' # get configure data of specific EnergyPlus version if avaiable
#' eplus_config(8.6)
#' }
#'
#' # get all versions of avaiable EnergyPlus
#' avail_eplus()
#'
#' # check if specific version of EnergyPlus is available
#' is_avail_eplus(8.5)
#' is_avail_eplus(8.8)
#'
#' @seealso [download_eplus()] and [install_eplus()] for downloading and
#' installing EnergyPlus
#'
#' @export
# use_eplus {{{
use_eplus <- function (eplus) {
    # if eplus is a version, try to locate it in the default path
    if (is_eplus_ver(eplus, strict = TRUE)) {
        ver <- standardize_ver(eplus)
        eplus_dir <- eplus_default_path(eplus)
        if (!is_eplus_path(eplus_dir)) {
            stop("Cannot locate EnergyPlus v", trimws(eplus), " at default ",
                "installation path ", backtick(eplus_dir), ". Please specify ",
                "explicitly the path of EnergyPlus installation.", call. = FALSE)
        }
    } else if (is_eplus_path(eplus)){
        ver <- get_ver_from_path(eplus)
        eplus_dir <- eplus
    } else {
        stop("`eplus` should be either a valid EnergyPlus version or an ",
            "EnergyPlus installation path.", call. = FALSE)
    }

    exe <- paste0("energyplus", ifelse(is_windows(), ".exe", ""))
    res <- list(version = ver, dir = eplus_dir, exe = exe)

    ori <- .globals$eplus_config[[as.character(ver)]]
    .globals$eplus_config[[as.character(ver)]] <- res

    if (is.null(ori)) {
        message("EnergyPlus v", ver, " located at ", backtick(eplus_dir),
            " has been added.")
    } else if (identical(ori$dir, eplus_dir)) {
        message("Configure data of EnergyPlus v", ver, " located at ",
            backtick(eplus_dir), " already exists. No Updating performed.")
    } else {
        message("Update configure data of EnergyPlus v", ver, ":\n",
            "    Former location: ", backtick(ori$dir), " ---> ",
                   "New location: ", backtick(eplus_dir))
    }

    invisible(res)
}
# }}}

#' @rdname use_eplus
#' @export
# eplus_config {{{
eplus_config <- function (ver) {
    assert_that(is_eplus_ver(ver, strict = TRUE))
    ver <- standardize_ver(ver)
    res <- .globals$eplus_config[[as.character(ver)]]
    if (is.null(res)) {
        warning("Failed to find configuration data of EnergyPlus v", ver, ".",
            call. = FALSE)
        res <- list()
    }

    res
}
# }}}

#' @rdname use_eplus
#' @export
# avail_eplus {{{
avail_eplus <- function () names(.globals$eplus_config)
# }}}

#' @rdname use_eplus
#' @export
# is_avail_eplus {{{
is_avail_eplus <- function (ver) !is_empty(suppressWarnings(eplus_config(ver)))
# }}}

# locate_eplus {{{
locate_eplus <- function () {
    find_eplus <- function (ver) {
        suppressMessages(tryCatch(use_eplus(ver),
            error = function (e) NULL))
    }

    lapply(rev(all_eplus_release_commit()$version), find_eplus)

    invisible()
}
# }}}
# eplus_default_path {{{
eplus_default_path <- function (ver) {
    ver <- standardize_ver(ver)
    assert_that(is_eplus_ver(ver))
    ver_dash <- paste0(ver[1,1], "-", ver[1,2], "-", ver[1,3])
    if (is_windows()) {
        d <- paste0("C:/EnergyPlusV", ver_dash)
    } else if (is_linux()) {
        d <- paste0("/usr/local/EnergyPlus-", ver_dash)
    } else {
        d <- paste0("/Applications/EnergyPlus-", ver_dash)
    }
    d
}
# }}}
# get_ver_from_path {{{
get_ver_from_path <- function (path) {
    idd_file <- normalizePath(file.path(path, "Energy+.idd"), mustWork = TRUE)

    h <- readr::read_lines(idd_file, n_max = 1L)

    tryCatch(get_idd_ver(h),
        error = function (e) stop("Failed to parse EnergyPlus version using IDD ",
            backtick(idd_file), ".", call. = FALSE))
}
# }}}
