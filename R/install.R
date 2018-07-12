#' @importFrom gh gh
#' @importFrom data.table data.table
#' @importFrom purrr modify_depth
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom processx run
#' @importFrom cli rule
NULL

#' Download and Install EnergyPlus
#'
#' Download specified version of EnergyPlus for your platform from GitHub and
#' install it.
#'
#' This function will try to install EnergyPlus into the default location, e.g.
#' \file{C:/EnergyPlusVX-Y-0} on Windows, \file{/usr/local/EnergyPlus-X-Y-0} on
#' Linux, and \file{/Applications/EnergyPlus-X-Y-0} on MacOS.
#'
#' Note that the installation process requires administrative privileges
#' required during the installation and you may need to pass password to enable
#' sudo commands used for installation.
#'
#' @param ver The EnergyPlus version number, e.g., \code{8.7}; the special value
#' \code{"latest"} means the latest version (fetched from GitHub releases).
#'
#' @param dir Where to save EnergyPlus installer file. Default is current
#' working directory. Only applicable to `download_eplus()`. For
#' `install_eplus()`, it will be saved into `tempdir()`.
#'
#' @param force Whether to install EnergyPlus even if it has already been
#' installed.
#'
#' @name install_eplus
#' @export
# install_eplus {{{
install_eplus <- function (ver = "latest", force = FALSE) {
    # check if the same version has been installed already
    if (ver == "latest") ver <- eplus_latest_release()
    ver_exists <- is_avail_eplus(ver)
    if (ver_exists) {
        if (!force) {
            message(msg(sprintf("It seems EnergyPlus v%s has been already
                installed at %s. Set 'force' to TRUE to reinstall.",
                ver, backtick(eplus_config(ver)$dir))))
            return(invisible())
        }
    }

    message(sprintf("Starting to download EnergyPlus v%s...", ver), "\n", cli::rule(line = 2))
    dl <- download_eplus(ver = ver, dir = tempdir())
    exec <- attr(dl, "file")

    message(sprintf("Starting to install EnergyPlus v%s...", ver), "\n", cli::rule(line = 2))
    res <- switch(os_type(),
           windows = install_eplus_win(exec),
           linux = install_eplus_linux(exec),
           macos = install_eplus_macos(exec))

    if (res != 0L) {
        stop(sprintf("Failed to install EnergyPlus v%s.", ver), call. = FALSE)
    }

    path <- eplus_default_path(ver)
    message(sprintf("EnergyPlus v%s successfully installed into %s.", ver, path))

    return(invisible())
}
# }}}

#' @name install_eplus
#' @export
# download_eplus {{{
download_eplus <- function (ver = "latest", dir = getwd()) {
    release <- repo_releases(owner = "NREL", repo = "EnergyPlus", ver = ver)
    ver <- attr(release, "version")
    url <- release[core_file == TRUE & ext == "exe" & arch == os_arch(), url]
    dest <- file.path(dir, basename(url))

    # download and install
    dl <- download_file(url, dest)
    if (dl != 0L) {
        stop(sprintf("Failed to download EnergyPlus v%s.", ver), call. = FALSE)
    } else {
        message("EnergyPlus", paste0("v", ver), " has been downloaded successfully into ",
        dir, ".")
    }
    dl
}
# }}}

# repo_releases: Get release versions and download URL using GitHub API {{{
repo_releases <- function (owner, repo, ver = "latest", pre_release = FALSE,
                           type = c("binary", "source")) {
    # get query response
    # {{{
    rels <- gh::gh("GET /repos/:owner/:repo/releases", repo = repo, owner = owner)
    if (all(rels == "")) {
        stop("GitHub repository ", backtick(paste0(owner, "/", repo)),
                " does not have any release.", call. = FALSE)
    }
    # get all tags
    tags <- vapply(rels, "[[", "", "tag_name")
    # get all versions
    vers <- gsub("^v", "", tags)
    # check if the version is a major.minor format
    not_patch <- grepl("^\\d+\\.\\d+(\\.0){0,1}$", vers)
    # get pre-release indicator
    prerels <- vapply(rels, "[[", logical(1), "prerelease")
    # get tarball download URL
    tars <- paste0(vapply(rels, "[[", "", "tarball_url"), ".tar.gz")
    # get zipball download URL
    zips <- paste0(vapply(rels, "[[", "", "zipball_url"), ".zip")
    # combine into a data.table
    res <- data.table::data.table(tag = tags, version = vers, patch = !not_patch,
        prerelease = prerels, tarball = tars, zipball = zips)
    # }}}

    # get download binary file data
    # {{{
    assets <- lapply(rels, "[[", "assets")
    name <- purrr::modify_depth(assets, 2, "name")
    url <- purrr::modify_depth(assets, 2, "browser_download_url")
    # combine into the data.table
    res[, `:=`(file = name, url = url)]
    # }}}

    # check the version
    # {{{
    ver <- as.character(ver)
    if (ver != "latest") {
        ver <- standardize_ver(ver)
        targ <- res[version == as.character(ver)]
        # check if version is correct
        if (nrow(targ) == 0L) {
            msg_ver <- res[, paste0("  Version: ", backtick(version), ifelse(prerelease, " (Pre-release)", ""),
                                     collapse = "\n")]
            stop("Could not find ", backtick(paste0(repo, " v", ver)), ". ",
                 "Possible values are:\n", msg_ver, call. = FALSE)
        }
    } else {
        if (pre_release) targ <- res[1L] else targ <- res[prerelease == FALSE][1L]
    }
    # }}}

    type <- match.arg(type)
    if (type == "binary") {
        # {{{
        links <- targ[, lapply(.SD, unlist), .SDcol = c("file", "url")]

        # check if there is no binary releases
        if (nrow(links) == 0L) {
            stop(backtick(repo), " ", targ[["tag"]], " does not release any binary file. ",
                 "Only source code is available.", call. = FALSE)
        }

        # check if there are other files such as release notes, dependencies on
        # the release file list
        repo_lcase <- tolower(repo)
        links[, `:=`(core_file = FALSE)]
        links[grepl(repo, file, ignore.case = TRUE), `:=`(core_file = TRUE)]

        # get file extension
        links[, `:=`(ext = tools::file_ext(file))]

        # guess platform using file extension and file name
        links[, os := NA_character_]
        links[ext %in% c("zip", "exe"), os := "windows"]
        links[ext %in% c("dmg"), os := "macos"]
        links[ext %in% c("deb", "sh"), os := "linux"]
        links[is.na(os) & grepl("[-._](win(dows){0,1})[-._]", file, ignore.case = TRUE), os := "windows"]
        links[is.na(os) & grepl("[-._](darwin|mac(os){0,1}|apple)[-._]", file, ignore.case = TRUE), os := "macos"]
        links[is.na(os) & grepl("[-._](linux)[-._]", file, ignore.case = TRUE), os := "linux"]
        links[is.na(os) & grepl("[-._](ubuntu)[-._]", file, ignore.case = TRUE), os := "ubuntu"]

        # guess architecture using file name
        links[, `:=`(arch = NA_character_)]
        links[grepl("i386|32bit|x86", file, ignore.case = TRUE), `:=`(arch = "32bit")]
        links[grepl("x86_64|64bit|x64", file, ignore.case = TRUE), `:=`(arch = "64bit")]

        # if platform specific released core files found, then only return those
        if (links[core_file == TRUE & !is.na(os), .N] > 0L) {
            links <- links[os == os_type() | is.na(os)]
        }
        # }}}
    } else {
        # {{{
        url <- ifelse(is_windows(), targ[["zipball"]], targ[["tarball"]])
        links <- data.table::data.table(
           file = paste0(repo, tools::file_path_sans_ext(basename(url)), "_src",
                         ".", tools::file_ext(url)),
           url = url, core_file = TRUE, ext = tools::file_ext(url),
           os = os_type(), arch = NA_character_)
        # }}}
    }

    attr(links, "tag") <- targ[["tag"]]
    attr(links, "version") <- targ[["version"]]
    attr(links, "prerelease") <- targ[["prerelease"]]

    return(links)
}
# }}}
# download_file {{{
download_file <- function (url, dest) {
    if (file.exists(dest)) unlink(dest)
    dest_dir <- dirname(dest)
    if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
    res <- utils::download.file(url, dest, mode = "wb")
    attr(res, "url") <- url
    attr(res, "file") <- dest
    return(res)
}
# }}}
# os_type: Return operation system type {{{
os_type <- function () {
    if (.Platform$OS.type == 'windows') {
        "windows"
    } else if (Sys.info()[['sysname']] == 'Darwin') {
        "macos"
    } else if (Sys.info()[['sysname']] == 'Linux') {
        "linux"
    } else {
        "unknown"
    }
}
# }}}
# os_arch: Return the architecture {{{
os_arch <- function () {
    if (identical(Sys.info()[['machine']], "x86-64")) {
        c("64bit")
    } else {
        c("32bit")
    }
}
# }}}
# eplus_latest_release: get the latest release version of EnergyPlus {{{
eplus_latest_release <- function () {
    latest <- gh::gh("GET /repos/:owner/:repo/releases/latest", repo = "EnergyPlus", owner = "NREL")
    ver <- as.numeric_version(gsub("^v", "", latest[["tag_name"]]))
    message('The latest EnergyPlus version is ', ver)
    return(ver)
}
# }}}
# install_eplus_win {{{
install_eplus_win <- function (exec) {
    has_ps <- unname(Sys.which("powershell") != "")
    win_exec <- normalizePath(exec)

    if (has_ps) {
        cmd <- sprintf("& %s /S | Out-Null", exec)
        res <- run_cmd(command = "powershell", args = cmd,
                       echo = TRUE, windows_verbatim_args = TRUE)
    } else {
        message("Cannot install EnergyPlus silently. Will try to install from GUI.")
        res <- run_cmd(commandline = exec)
    }

    return(res)
}
# }}}
# install_eplus_macos {{{
install_eplus_macos <- function (exec) {
    no_ext <- tools::file_path_sans_ext(base)
    message("NOTE: Administrative privileges required during the installation")
    cmd <- sprintf("sudo hdiutil attach %s | sudo installer -pkg /Volumes/%s/%s.pkg -target /", exec, no_ext, no_ext)
    run_cmd(commandline = cmd)
}
# }}}
# install_eplus_linux {{{
install_eplus_linux <- function (exec) {
    cmd <- sprintf("sudo chmod +x %s | echo 'y\r' | sudo ./%s", exec, exec)
    run_cmd(commandline = cmd)
}
# }}}
# run_cmd {{{
# only return the status and suppress annoying warnings from `processx::run`
run_cmd <- function (...) {
    res <- tryCatch(
        {r <- suppressWarnings(processx::run(...))
         r$status
        },
        error = function (e) 1L
    )

    return(res)
}
# }}}
