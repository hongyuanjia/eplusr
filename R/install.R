#' Install EnergyPlus
#'
#' Download specified version of EnergyPlus for your platform from Github and
#' install it.
#'
#' This function will try to install EnergyPlus into the default location, e.g.
#' \file{C:/EnergyPlusVX-X-0} on Windows, \file{/usr/local/EnergyPlus-X-Y-0} on
#' Linux, and \file{/Applications/EnergyPlus-X-Y-0} on MacOS.
#'
#' Note that the installation process requires administrative privileges
#' required during the installation and you may need to pass password to enable
#' sudo commands used for installation.
#'
#' @param ver The EnergyPlus version number, e.g., \code{8.7}; the special value
#'            \code{latest} means the latest version (fetched from Github
#'            releases).
#' @param force Whether to install EnergyPlus even if it has already been
#'              installed.
#' @export
# install_eplus {{{
install_eplus <- function (ver = "latest", force = FALSE) {
    # translate 'latest'
    if (ver == "latest") ver <- eplus_latest_ver()
    assert_that(is_supported_ver(ver))

    # change working directory to temp dir
    # ori_wd <- setwd(tempdir())
    # on.exit(setwd(ori_wd), add = TRUE)

    # check if the same version has been installed already
    ver_exist <- tryCatch(eplus_path(ver), error = function (e) NULL)
    if (!is.null(ver_exist)) {
        if (!force) {
            message(msg(sprintf("It seems EnergyPlus v%s has been already
                installed at %s. Set 'force' to TRUE to reinstall.",
                ver, sQuote(ver_exist["home"]))))
            return(invisible())
        }
    }

    message(sprintf("Starting to download EnergyPlus v%s...", ver), "\n", sep_line("="))
    dl <- download_eplus(ver, tempdir())
    if (dl != 0L) {
        stop(sprintf("Failed to download EnergyPlus v%s.", ver), call. = FALSE)
    }

    exec <- attr(dl, "destfile")
    message(sprintf("Starting to install EnergyPlus v%s...", ver), "\n", sep_line("="))
    res <- switch(osname(),
           Windows = install_eplus_win(exec),
           Linux = install_eplus_linux(exec),
           Darwin = install_eplus_macos(exec))

    if (res != 0L) {
        stop(sprintf("Cannot install EnergyPlus v%s.", ver), call. = FALSE)
    }

    path <- eplus_path(ver)["home"]
    message(sprintf("EnergyPlus v%s successfully installed into %s.", ver, path))

    return(invisible())
}
# }}}

# full_ver {{{
full_ver <- function (ver) {
    assert_that(is_eplus_ver(ver))
    paste0(ver, ".0")
}
# }}}
# dash_ver {{{
dash_ver <- function (ver) {
    assert_that(is_eplus_ver(ver))
    paste0(sub(".", "-", ver, fixed = TRUE), "-0")
}
# }}}
# sha_ver {{{
sha_ver <- function (ver) {
    assert_that(is_supported_ver(ver))

    dict <- c(`8.3` = "6d97d074ea",
              `8.4` = "832e4bb9cb",
              `8.5` = "c87e61b44b",
              `8.6` = "198c6a3cff",
              `8.7` = "78a111df4a",
              `8.8` = "7c3bbe4830")

    dict[as.character(ver)]
}
# }}}
# os_arch {{{
os_arch <- function () if (identical(Sys.info()[['machine']], "x86-64")) "x86_64" else "i386"
# }}}
# targ_ext {{{
targ_ext <- function () switch(osname(), Windows = "exe", Linux = "sh", Darwin = "dmg")
# }}}
# eplus_latest_ver {{{
# get the latest release version of EnergyPlus
# imported from `blogdown::install_hugo`
eplus_latest_ver <- function () {
    h <- readLines('https://github.com/NREL/EnergyPlus/releases/latest', warn = FALSE)
    r <- '^.*?releases/tag/v([0-9.]+)".*'
    ver <- gsub(r, '\\1', grep(r, h, value = TRUE)[1])
    short_ver <- substr(ver, 1L, 3L)
    message('The latest EnergyPlus version is ', short_ver)

    return(short_ver)
}
# }}}
# get_eplus_download_url {{{
get_eplus_download_url <- function (ver) {
    eplus_ver <- full_ver(ver)
    inst_ver <- dash_ver(ver)
    sha <- sha_ver(ver)
    os <- osname()
    arch <- os_arch()
    ext <- targ_ext()

    base <- sprintf("https://github.com/NREL/EnergyPlus/releases/download/v%s", eplus_ver)

    file <- sprintf("EnergyPlus-%s-%s-%s-%s.%s", eplus_ver, sha, os, arch, ext)

    url <- paste0(base, "/", file)
    setattr(url, "file", file)

    return(url)
}
# }}}
# download_eplus {{{
download_eplus <- function (ver, destdir) {
    url <- get_eplus_download_url(ver)
    file <- attr(url, "file")

    dir.create(destdir, showWarnings = FALSE, recursive = TRUE)
    destfile <- normalizePath(file.path(destdir, file), mustWork = FALSE)
    # delete existing downloaded file
    unlink(destfile, recursive = TRUE)

    res <- download.file(url, destfile, mode = 'wb')
    setattr(res, "url", url)
    setattr(res, "destfile", destfile)

    return(res)
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
