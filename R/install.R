#' @importFrom data.table data.table fread
#' @importFrom tools file_path_sans_ext
#' @importFrom utils download.file
#' @importFrom cli rule
NULL

#' Download and Install EnergyPlus
#'
#' Download specified version of EnergyPlus for your platform from GitHub and
#' install it.
#'
#' @param ver The EnergyPlus version number, e.g., `"8.7"`. For `download_eplus()`
#'        and `install_eplus()`, the special value `"latest"`, which is the
#'        default, means the latest version supported by eplusr.
#'
#' @param local Whether to install EnergyPlus only for current user. For Windows
#'        and Linux, if `FALSE`, administrative privileges are required to
#'        install EnergyPlus to the default system-level location. See details.
#'        `local` should be also set to `FALSE` if you do not have the write
#'        access to the directory specified via `dir`. Default: `FALSE`. For
#'        macOS, administrative privileges are always required no matter you
#'        want EnergyPlus to be install at `/Applications` or `~/Applications`.
#'
#' @param dir A single string of directory.
#'
#'   * For `download_eplus()`, where to save EnergyPlus installer file.
#'     Default: `"."`.
#'   * For `install_eplus()`, the installer will always be saved into
#'     [tempdir()]. But you can use `dir` to specify the **parent** directory
#'     of EnergyPlus installation, i.e. the **parent** directory of
#'     `EnergyPlusVX-Y-0` on Windows and `EnergyPlus-X-Y-0` on Linux and macOS.
#'     If `NULL`, the default installation path will be used.
#'     See details for more information. Please note that `dir` only works
#'     when on macOS and EnergyPlus will always be installed into the default
#'     location. Default: `NULL`.
#'
#' @param force Whether to install EnergyPlus even if it has already been
#'        installed. Setting to `TRUE` if you want to install the downloaded
#'        EnergyPlus anyway. Please note that this may results in multiple
#'        EnergyPlus installations of the same version at different locations.
#'        eplusr will only use the first EnergyPlus installation. Default: `FALSE`.
#'
#' @param portable Whether to install EnergyPlus using the portable `zip` (on
#'        Windows) and `tar.gz` (on macOS and Linux) installer for EnergyPlus
#'        v8.8 and above. Default: `FALSE`.
#'
#' @param ... Other arguments to be passed to the installer. Current only one
#'        additional argument exists and is only for Linux:
#'
#'   * `dir_bin`: A path where symbolic links will be created to the software
#'     executables. The default is `/usr/local/bin` if `local` is `FALSE`
#'     and `~/.local/bin` if `local` is `TRUE`.
#'
#' @details
#'
#' `download_eplus()` downloads specified version of EnergyPlus from
#' [EnergyPlus GitHub Repository](https://github.com/NREL/EnergyPlus).
#'
#' `install_eplus()` tries to install EnergyPlus into the default location,
#' e.g. \verb{C:\EnergyPlusVX-Y-0} on Windows, `/usr/local/EnergyPlus-X-Y-0` on
#' Linux, and `/Applications/EnergyPlus-X-Y-0` on macOS.
#'
#' Note that installing to the default location requires administrative
#' privileges and you have to run R with administrator (or with sudo if you are
#' on Linux) to make it work if you are not in interactive mode.
#'
#' If you can't run R with administrator, it is possible to install EnergyPlus
#' to your home corresponding directory by setting `local` to `TRUE`.
#'
#' The user level EnergyPlus installation path is:
#'
#' * Windows:
#'   - `dir(Sys.getenv("LOCALAPPDATA"), "EnergyPlusVX-Y-0")` OR
#'   - \verb{C:\Users\<User>\AppData\Local\EnergyPlusVX-Y-0} if environment
#'     variable `"LOCALAPPDATA"` is not set
#' * macOS: `/Users/User/Applications/EnergyPlus-X-Y-0`
#' * Linux: `"~/.local/EnergyPlus-X-Y-0"`
#'
#' On Windows and Linux, you can also specify your custom directory using the
#' `dir` argument. Remember to change `local` to `FALSE` in order to ask for
#' administrator privileges if you do not have the write access to that
#' directory. On macOS, `dir` only works when `portable` is set to `TRUE`.
#'
#' Please note that when `local` is set to `FALSE`, no symbolic links
#' will be created, since this process requires administrative privileges.
#'
#' `uninstall_eplus()` tries to uninstall specified version of EnergyPlus
#' located by eplusr. Similar as `install_eplus()`, administrative privileges
#' may be required.
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
#' # download the latest version of EnergyPlus
#' download_eplus("latest", dir = tempdir())
#' # install the latest version of EnergyPlus system-wide which is the default
#' # and requires administrative privileges
#' install_eplus("latest")
#'
#' # for a specific version of EnergyPlus
#' download_eplus("8.8", dir = tempdir())
#' install_eplus("8.8")
#'
#' # force to reinstall
#' install_eplus("8.8", force = TRUE)
#'
#' # install EnergyPlus in your home directory
#' install_eplus("8.8", local = TRUE, force = TRUE)
#'
#' # custom EnergyPlus install home directory
#' install_eplus("8.8", dir = "~/MyPrograms", local = TRUE, force = TRUE)
#' }
#' @author Hongyuan Jia
#' @export
#' @importFrom checkmate assert_string
# install_eplus {{{
install_eplus <- function(ver = "latest", local = FALSE, dir = NULL, force = FALSE, portable = FALSE, ...) {
    checkmate::assert_atomic_vector(ver, any.missing = FALSE, len = 1L)
    ver <- standardize_ver(ver)

    # check if the same version has been installed already
    if (is_avail_eplus(ver)) {
        if (!isTRUE(force)) {
            abort(paste0("It seems EnergyPlus v", ver, " has been already installed at ",
                surround(eplus_config(ver)$dir), ". Set `force` to `TRUE` to reinstall."
            ))
        }
    }

    verbose_info(sprintf("Starting to download EnergyPlus v%s...", ver))

    dl <- download_eplus(ver, tempdir(), portable = portable)
    inst <- attr(dl, "file")

    path <- install_eplus_from_file(ver, inst, local = local, dir = dir, portable = portable, ...)

    res <- 0L
    attr(res, "path") <- path
    attr(res, "installer") <- inst

    invisible(res)
}
# }}}
# install_eplus_from_file {{{
install_eplus_from_file <- function(ver, inst, local = FALSE, dir = NULL, portable = FALSE, ...) {
    ver <- standardize_ver(ver)
    verbose_info(sprintf("Starting to install EnergyPlus v%s...", ver))

    if (!local && !portable)
        verbose_info("NOTE: Administrative privileges required during installation. ",
            "Please make sure R is running with an administrator account or equivalent.")

    res <- switch(
        os_type(),
        windows = install_eplus_win(ver,   inst, local = local, dir = dir, portable = portable),
        linux   = install_eplus_linux(ver, inst, local = local, dir = dir, portable = portable, ...),
        macos   = install_eplus_macos(ver, inst, local = local, dir = dir, portable = portable)
    )

    if (res != 0L) abort(paste0("Failed to install EnergyPlus v", ver, "."))

    path <- attr(res, "path")
    verbose_info(sprintf("EnergyPlus v%s successfully installed into '%s'.", ver, path))

    # add newly installed EnergyPlus to dictionary
    use_eplus(path)

    path
}
# }}}

#' @name install_eplus
#' @export
# uninstall_eplus {{{
uninstall_eplus <- function(ver) {
    ver <- standardize_ver(ver)

    # stop if cannot locate EnergyPlus
    dir <- tryCatch(eplus_config(ver)$dir, eplusr_warning_miss_eplus_config = function(w) abort(conditionMessage(w)))

    verbose_info(sprintf("Start uninstalling EnergyPlus v%s...", ver))

    # detect if it is a portable EnergyPlus installation
    is_portable <- switch(
        os_type(),
        windows = if (ver >= "9.2") {
            file.exists(file.path(dir, "maintenancetool.exe"))
        } else {
            file.exists(file.path(dir, "Uninstall.exe"))
        },
        linux = file.exists(file.path(dir, "uninstall.sh")),
        macos = if (ver >= "9.2") {
            file.exists(file.path(dir, "maintenancetool.app/Contents/MacOS/maintenancetool"))
        } else {
            TRUE
        }
    )
    verbose_info("NOTE: Administrative privileges may be required during uninstallation. ",
        "Please make sure R is running with an administrator acount or equivalent.")

    if (is_portable) {
        res <- uninstall_eplus_portable(ver, dir)
    } else {
        res <- switch(os_type(),
            windows = uninstall_eplus_win(ver, dir),
            linux   = uninstall_eplus_linux(ver, dir),
            macos   = uninstall_eplus_macos(ver, dir)
        )
    }

    if (res != 0L) abort(paste0("Failed to uninstall EnergyPlus v", ver, "."))

    verbose_info(sprintf("EnergyPlus v%s ('%s') has been uninstalled successfully.", ver, dir))

    # remove config
    .globals$eplus[[as.character(ver)]] <- NULL
    verbose_info(sprintf("Configuration of EnergyPlus v%s has been removed.", ver))

    attr(res, "path") <- dir

    invisible(res)
}
# }}}

#' @name install_eplus
#' @param portable Whether to download the portable version of EnergyPlus. Only
#' works for EnergyPlus v8.8 and above. Default: `FALSE`.
#' @export
# download_eplus {{{
download_eplus <- function(ver = "latest", dir, portable = FALSE) {
    ver <- match_minor_ver(standardize_ver(ver, complete = FALSE), ALL_EPLUS_VER, "eplus")
    url <- eplus_download_url(ver, portable = portable)

    file <- basename(url)

    dest <- normalizePath(file.path(dir, file), mustWork = FALSE)
    # set timeout option to a large number: 30 mins
    old <- options("timeout" = 60 * 30)
    dl <- download_file(url, dest)
    on.exit(options("timeout" = old), add = TRUE)

    if (dl != 0L) stop("Failed to download EnergyPlus v", ver, ".", call. = FALSE)

    verbose_info("The installer file of EnergyPlus ", paste0("v", ver), " ",
        surround(file), " has been successfully downloaded into ", dir, ".")

    attr(dl, "file") <- dest
    invisible(dl)
}
# }}}

# eplus_download_url: get EnergyPlus installer download URL {{{
eplus_download_url <- function(ver, portable = FALSE) {
    base_url <- "https://github.com/NREL/EnergyPlus/releases/download/"

    cmt <- eplus_release_commit(ver)

    if (!nrow(cmt)) {
        abort(paste0(
            "Failed to get installer data for EnergyPlus v", ver, ". ",
            "All available version are: ",
            collapse(ALL_EPLUS_RELEASE_COMMIT[order(version), version]), "."
        ))
    }

    # get operating system
    os <- os_type()
    if (os == "unknown") abort("Unsupported operating system.")
    ostype <- switch(os,
        windows = "Windows", macos = "Darwin", linux = "Linux"
    )

    # get installer file extension
    if (!portable) {
        ext <- switch(os_type(), windows = "exe", macos = "dmg", linux = "sh")
    } else {
        if (numeric_version(cmt$version) < "8.8") {
            abort("Portable version of EnergyPlus is only available for v8.8 and above.")
        }
        ext <- switch(os_type(), windows = "zip", macos = "tar.gz", linux = "tar.gz")
    }

    # get architecture
    info_arch <- Sys.info()[["machine"]]
    if (info_arch %in% c("x86-64", "x86_64")) {
        arch <- "x86_64"
    } else {
        arch <- info_arch
        # stop if it is a 32-bit system for Linux
        if (arch == "i386" && os != "windows") {
            abort("EnergyPlus does not provide 32-bit version installer for macOS and Linux.")
        }
        # NOTE: check Windows i386 support for EnergyPlus 24.1.0 and above
    }

    # EnergyPlus v9.4 and above provide different installers for various
    # Ubuntu distribution and macOS versions and should be handled differently
    if (os == "windows" || !cmt$version %in% names(ALL_EPLUS_OSVER)) {
        # no macOS arm64 installer provided for ENergyPlus v9.4 and below
        if (arch == "arm64") {
            verbose_info(
                "EnergyPlus does not provide v", cmt$version, " installer for ",
                if (ostype == "Darwin") "macOS" else ostype, " arm64 platform. ",
                "The x86_64 installer will be used instead."
            )
            arch <- "x86_64"
        }

        file <- sprintf("EnergyPlus-%s-%s-%s-%s.%s", cmt$version, cmt$commit, ostype, arch, ext)
        return(paste0(base_url, "v", cmt$version, "/", file))
    }

    # if arm64 installer is not provided, use the x86_64 one
    if (arch == "arm64" && is.null(ALL_EPLUS_OSVER[[cmt$version]][[os]][[arch]])) {
        verbose_info(
            "EnergyPlus does not provide v", cmt$version, " installer for ", ostype, " arm64 platform. ",
            "The x86_64 installer will be used instead."
        )
        arch <- "x86_64"
    }

    idx_osver <- NA_integer_
    osver_cur <- os_version()
    osver_cur_num <- numeric_version(osver_cur, FALSE)
    osvers_nm <- ALL_EPLUS_OSVER[[cmt$version]][[os]][[arch]]
    osvers <- gsub(os, "", osvers_nm, ignore.case = TRUE)
    dist <- linux_dist()

    # use the latest installer if not Ubuntu
    if (os == "linux" && dist != "ubuntu") {
        idx_osver <- length(osvers)
        verbose_info(
            "Current distribution is ", dist, ". ",
            "Currently, EnergyPlus only provides installers for Ubuntu. ",
            "The latest installer for Ubuntu ", osvers[idx_osver], " will be used."
        )
    }

    if (in_verbose()) osname <- if (os == "macos") "macOS" else "Ubuntu"

    # use the latest installer if fail to get the os version
    if (is.na(idx_osver) && (is.na(osver_cur) || is.na(osver_cur_num))) {
        idx_osver <- length(osvers)

        verbose_info(
            "Failed to determine the current ", osname, " version. ",
            "The latest installer for ", osname, " ", osvers[idx_osver], " will be used."
        )
    }

    if (is.na(idx_osver)) {
        # check if the exact version is supported
        if (osver_cur %in% osvers) {
            idx_osver <- which(osvers == osver_cur)
        } else {
            osver_cur_num <- numeric_version(osver_cur, FALSE)
            osvers_num <- numeric_version(osvers)

            # if the major version is not supported, use the nearest installer
            if (!length(idx_major <- which(osver_cur_num[, 1L] == osvers_num[, 1L]))) {
                idx_osver <- which.min(abs(
                    as.numeric(osver_cur_num[, 1L]) - as.numeric(osvers_num[, 1L])
                ))

                verbose_info(
                    "EnergyPlus does not provide v", cmt$version, " installer for ", osname,  " ", osver_cur, ". ",
                    "The nearest installer for ", osname, " ", osvers[idx_osver], " will be used."
                )
            } else {
                # only minor version mismatches
                if (length(idx_major) == 1L) {
                   idx_osver <- idx_major
                # in case multiple major version matches
                } else {
                    # if no minor version, use the latest installer
                    if (is.na(osver_cur_num[, 2L])) {
                        idx_osver <- idx_major[osvers_num[idx_major] == max(osvers_num[idx_major])]
                    } else {
                        idx_osver <- idx_major[which.min(abs(
                            as.numeric(osver_cur_num[, 2L]) - as.numeric(osvers_num[idx_major][, 2L])
                        ))]
                    }
                }
                verbose_info(
                    "EnergyPlus does not provide v", cmt$version, " installer for ", osname,  " ", osver_cur, ". ",
                    "Installer for ", osname, " ", osvers[idx_major], " is available and will be used.",
                )
            }
        }
    }

    if (is.na(idx_osver)) {
        abort(paste0("Failed to determine the installer for EnergyPlus v", cmt$version, "."))
    }
    osver <- osvers_nm[idx_osver]

    file <- sprintf("EnergyPlus-%s-%s-%s-%s-%s.%s",
        cmt$version, cmt$commit, ostype, osver, arch, ext
    )
    paste0(base_url, "v", cmt$version, "/", file)
}
# }}}
# eplus_release_commit: return EnergyPlus release commit data {{{
eplus_release_commit <- function(ver) {
    ver <- standardize_ver(ver)

    ALL_EPLUS_RELEASE_COMMIT[version == as.character(ver)]
}
# }}}
# download_file: same as download.file except that it creates the target directory if necessary {{{
download_file <- function(url, dest) {
    if (file.exists(dest))
        tryCatch(unlink(dest),
            warning = function(w) {
                stop("Failed to delete the existing file ",
                    surround(dest), "before downloading.", call. = FALSE)
            }
        )

    dest_dir <- dirname(dest)

    if (!dir.exists(dest_dir))
        tryCatch(dir.create(dest_dir, recursive = TRUE), warning = function(w) stop(w, call. = FALSE))

    utils::download.file(url, dest, mode = "wb")
}
# }}}
# install_eplus_win {{{
install_eplus_win <- function(ver, exec, local = FALSE, dir = NULL, portable = FALSE) {
    ver <- standardize_ver(ver)
    if (is.null(dir)) {
        if (local) {
            dir <- get_win_user_path(error = TRUE)
        } else {
            dir <- "C:\\"
        }
    }

    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    dir <- normalizePath(file.path(dir, paste0("EnergyPlusV", gsub("\\.", "-", ver))), mustWork = FALSE)

    if (portable) {
        # install using the portable version
        res <- install_eplus_portable(ver, exec, dir)
    } else {
        if (ver >= "9.2") {
            res <- install_eplus_qt(ver, exec, dir, local = local)
        } else {
            res <- system(sprintf("%s /S /D=%s", exec, dir))
        }
        attr(res, "path") <- dir
    }
    res
}
# }}}
# get_win_user_path {{{
get_win_user_path <- function(error = FALSE) {
    appdata <- Sys.getenv("LOCALAPPDATA", "")
    if (appdata != "") return(normalizePath(appdata))

    # get the current user name
    user <- Sys.getenv("USERNAME", "")
    if (user == "") {
        userp <- Sys.getenv("USERPROFILE", "")
        if (userp != "") {
            user <- basename(userp)
        } else if (Sys.which("whoami") == "") {
            whoami <- processx::run("whoami", error_on_status = FALSE)
            if (whoami$status != 0L) {
                if (!error) return("")

                abort("Cannot get the user-level install path because it failed to get current logged user name.")
            }

            user <- gsub("\r\n", "", basename(whoami$stdout), fixed = TRUE)
        }
    }

    normalizePath(file.path("C:/Users", user, "AppData/Local"))
}
# }}}
# sudo_on_mac {{{
sudo_on_mac <- function(cmd) {
    if (interactive()) {
        # see: https://stackoverflow.com/questions/1517183/is-there-any-graphical-sudo-for-mac-os-x
        system(sprintf("osascript -e 'do shell script \"%s\" with administrator privileges'", cmd))
    } else {
        system(sprintf("sudo %s", cmd))
    }
}
# }}}
# install_eplus_macos {{{
install_eplus_macos <- function(ver, exec, local = FALSE, dir = NULL, portable = FALSE) {
    ver <- standardize_ver(ver)
    no_ext <- tools::file_path_sans_ext(basename(exec))

    if ((portable && is.null(dir)) || ver >= "9.1") {
        ver_dash <- gsub(".", "-", ver, fixed = TRUE)
        if (local) {
            dir <- normalizePath(file.path("~/Applications", paste0("EnergyPlus-", ver_dash)), mustWork = FALSE)
        } else {
            dir <- file.path("/Applications", paste0("EnergyPlus-", ver_dash))
        }
    }

    if (portable) {
        if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
        dir <- normalizePath(dir, mustWork = TRUE)

        # install using the portable version
        res <- install_eplus_portable(ver, exec, dir)
    } else {
        # mount
        system(sprintf("hdiutil mount %s", exec))
        if (ver < "9.1") {
            if (local) {
                res <- system(sprintf("installer -pkg /Volumes/%s/%s.pkg -target CurrentUserHomeDirectory", no_ext, no_ext))
            } else {
                res <- sudo_on_mac(sprintf("installer -pkg /Volumes/%s/%s.pkg -target LocalSystem", no_ext, no_ext))
            }
            attr(res, "path") <- eplus_default_path(ver, local = local)
        } else {
            res <- install_eplus_qt(ver, exec, dir, local = local)
            attr(res, "path") <- dir
        }
        system(sprintf("hdiutil unmount /Volumes/%s/", no_ext))
    }
    res
}
# }}}
# install_eplus_linux {{{
#' @importFrom checkmate assert_string
install_eplus_linux <- function(ver, exec, local = FALSE, dir = NULL, dir_bin = NULL, portable = FALSE) {
    ver <- standardize_ver(ver)

    if (local) {
        if (is.null(dir)) dir <- "~/.local"
        if (is.null(dir_bin)) dir_bin <- "~/.local/bin"
    } else {
        if (is.null(dir)) dir <- "/usr/local"
        if (is.null(dir_bin)) dir_bin <- "/usr/local/bin"
    }

    assert_string(dir_bin)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    if (!dir.exists(dir_bin)) dir.create(dir_bin, recursive = TRUE)

    dir <- normalizePath(dir, mustWork = TRUE)
    dir_bin <- normalizePath(dir_bin, mustWork = TRUE)

    ver_dash <- gsub("\\.", "-", ver)
    dir_eplus <- file.path(dir, paste0("EnergyPlus-", ver_dash))

    if (portable) {
        # install using the portable version
        res <- install_eplus_portable(ver, exec, dir_eplus)
    } else {
        # EnergyPlus installation are broken since 9.1.0, which extract all files
        # directly into `/usr/local.
        # see https://github.com/NREL/EnergyPlus/issues/7256
        if (ver == "9.1") {
            if (Sys.which("sed") != "") {
                # copy the original installer
                temp_exec <- file.path(tempdir(), paste0("patched-", basename(exec)))
                flag <- file.copy(exec, temp_exec, overwrite = TRUE, copy.date = TRUE)
                if (!flag) {
                    abort(paste0(
                        "Failed to create a temporary copy of EnergyPlus installer '",
                        dir, "' at temporary directory '", tempdir(), "'."
                    ))
                }
                patch_eplus_linux_sh(ver, temp_exec)
                exec <- temp_exec
            } else {
                message("There is a known issue in EnergyPlus installation for v9.1.0 which ",
                    "fails to extract files into correct directory ('", dir_eplus, "'). ",
                    "eplusr uses 'sed' to fix the issue before running the installation, ",
                    "but 'sed' is not found on current system. ",
                    "Please remember to manually move corresponding files ",
                    "from '", dir, "' to '", dir_eplus, "' in order to make sure ",
                    "eplusr can locate EnergyPlus v", ver, "correctly.",
                    "For more information, please see https://github.com/NREL/EnergyPlus/issues/7256"
                )
            }
        }

        # EnergyPlus v9.3 and above provide a QtIFW based installer for Linux
        system(sprintf('chmod +x %s', exec))
        # installers for EnergyPlus v9.2 and above should provide the full path
        # of EnergyPlus install directory
        if (ver >= "9.2") dir <- dir_eplus
        if (local) {
            res <- system(sprintf('echo "y\n%s\n%s" | %s', dir, dir_bin, exec))
            res <- res + system(sprintf('chmod -R a+w %s', dir_eplus))
        } else {
            res <- system(sprintf('echo "y\n%s\n%s" | sudo %s', dir, dir_bin, exec))
            res <- res + system(sprintf('sudo chmod -R a+w %s', dir_eplus))
        }
        attr(res, "path") <- dir_eplus
    }

    res
}
# }}}
# patch_eplus_linux_sh {{{
patch_eplus_linux_sh <- function(ver, exec) {
    if (ver == "9.1") {
        system(sprintf("sed -i '%is/%s/%s/' %s",
            77,
            "\\${install_directory}",
            "\\${install_directory}\\/\\${package_name}",
            exec
        ))
        system(sprintf("sed -i '%is/%s/%s/' %s",
            79,
            "\\${install_directory}",
            "\\${install_directory}\\/\\${package_name}",
            exec
        ))
        system(sprintf("sed -i '%is/%s/%s/' %s",
            116,
            "ParametricPreProcessor",
            "ParametricPreprocessor",
            exec
        ))
    }
}
# }}}
# install_eplus_qt {{{
install_eplus_qt <- function(ver, exec, dir, local = FALSE, verbose = FALSE) {
    ver <- standardize_ver(ver)
    # create a tempfile of QTIFW control script
    ctrl <- tempfile(fileext = ".qs")
    # NOTE: shoud escape slash twice here
    if (is_windows()) dir <- gsub("\\", "\\\\", dir, fixed = TRUE)
    write_lines(file = ctrl, x = paste0("
        function Controller() {
            installer.installationFinished.connect(function() {
                gui.clickButton(buttons.NextButton);
            })
            installer.uninstallationFinished.connect(function() {
                gui.clickButton(buttons.NextButton);
            })

            gui.setSilent(true);
        };

        Controller.prototype.IntroductionPageCallback = function() {
            gui.clickButton(buttons.NextButton);
        };

        Controller.prototype.TargetDirectoryPageCallback = function() {
            var page = gui.pageWidgetByObjectName('TargetDirectoryPage');
            page.TargetDirectoryLineEdit.setText(\"", dir, "\");
            gui.clickButton(buttons.NextButton, 1000);
        };

        Controller.prototype.ComponentSelectionPageCallback = function() {
        ",
        if (local) {
            "gui.currentPageWidget().deselectComponent(\"Symlinks\");"
        },
        "
            gui.clickButton(buttons.NextButton, 1000);
        };

        Controller.prototype.LicenseAgreementPageCallback = function() {
        ",
        if (ver < "9.6") {
        "
            gui.currentPageWidget().AcceptLicenseRadioButton.setChecked(true);
        "
        # Fix action of license acceptance
        # see: https://github.com/NREL/EnergyPlus/issues/9174
        } else {
        '
            gui.currentPageWidget().AcceptLicenseCheckBox.setChecked(true);
        '
        },
        "
            gui.clickButton(buttons.NextButton, 1000);
        };

        Controller.prototype.StartMenuDirectoryPageCallback = function() {
            gui.clickButton(buttons.NextButton);
        };

        Controller.prototype.ReadyForInstallationPageCallback = function() {
            gui.clickButton(buttons.CommitButton);
        };

        Controller.prototype.PerformInstallationPageCallback = function()
        {
            gui.clickButton(buttons.CommitButton);
        };

        Controller.prototype.FinishedPageCallback = function() {
            gui.clickButton(buttons.FinishButton);
        };
    ", collapse = "\n"))
    res <- system(sprintf("%s %s --script %s", exec, if (verbose) "--verbose " else "", ctrl))
    # sometimes QtIFW returns 1 if success for EnergyPlus v9.5 installer
    if (res == 1L) res <- res - 1L
    res
}
# }}}
# install_eplus_portable {{{
install_eplus_portable <- function(ver, file, dir) {
    ver <- standardize_ver(ver)
    file <- normalizePath(file, mustWork = TRUE)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    ext <- tools::file_ext(file)

    # It is possible that unzip fails due to very long path on Windows So first
    # unzip it in R's temporary directory and then move the extracted files to
    # the target directory
    if (is_windows()) {
        tmpdir <- tempfile("epinst-")
    } else {
        tmpdir <- tempfile(tmpdir = dirname(dir))
    }
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

    if (ext == "zip") {
        utils::unzip(file, overwrite = TRUE, exdir = tmpdir)
        res <- 0L
    } else if (ext == "gz" && tools::file_ext(tools::file_path_sans_ext(file)) == "tar") {
        res <- utils::untar(file, exdir = tmpdir)
        if (res != 0L) return(res)
    } else {
        abort("Unsupported portable EnergyPlus installer file format.")
    }

    epdir <- list.files(list.files(tmpdir, full.names = TRUE), full.names = TRUE)

    # file.rename only works if dest does not exist or is empty
    files <- list.files(epdir, full.names = TRUE)
    if (length(list.files(dir)) == 0L) {
        res <- file.rename(files, file.path(dir, basename(files)))
        # file.rename is error-prone, so check if the file is moved successfully
        # if not, copy it
        if (!all(res)) {
            res <- file.copy(files[!res], file.path(dir, basename(files))[!res],
                overwrite = TRUE, recursive = TRUE, copy.date = TRUE
            )
        }
    } else {
        res <- file.copy(files, dir, overwrite = TRUE, recursive = TRUE, copy.date = TRUE)
    }

    res <- as.integer(!all(res))
    attr(res, "path") <- dir
    res
}
# }}}
# uninstall_eplus_win {{{
uninstall_eplus_win <- function(ver, dir) {
    ver <- standardize_ver(ver)

    if (ver >= "9.2") {
        uninstall_eplus_qt(ver, dir)
    } else {
        uninstaller <- normalizePath(file.path(dir, "Uninstall.exe"), mustWork = FALSE)
        if (!file.exists(uninstaller)) {
            abort(sprintf(paste0(
                "Failed to locate 'Uninstall.exe' under EnergyPlus installation directory '%s'.",
            ), dir))
        }

        message(paste("The uninstaller will pop up a message box asking",
            "whether to remove files copied to the system directory during installation.",
            "Please make your choice."
        ))
        system(sprintf("%s /S", uninstaller))
    }
}
# }}}
# uninstall_eplus_macos {{{
uninstall_eplus_macos <- function(ver, dir) {
    ver <- standardize_ver(ver)

    if (ver >= "9.2") {
        uninstall_eplus_qt(ver, dir)
    } else {
        # test if EnergyPlus directory is accessible for current user
        if (identical(unname(try(file.access(dir, 2L), silent = TRUE)), 0L)) {
            system(sprintf("rm -rf %s", dir))
        } else {
            sudo_on_mac(sprintf("rm -rf %s", dir))
        }
    }
}
# }}}
# uninstall_eplus_qt {{{
uninstall_eplus_qt <- function(ver, dir) {
    ver <- standardize_ver(ver)

    ext <- if (is_windows()) ".exe" else ""
    if (!is_macos()) {
        exe <- sprintf("maintenancetool%s", ext)
    } else {
        exe <- "maintenancetool.app/Contents/MacOS/maintenancetool"
    }

    uninstaller <- normalizePath(file.path(dir, exe), mustWork = FALSE)
    if (!file.exists(uninstaller)) {
        abort(sprintf(
            "Failed to locate '%s' under EnergyPlus installation directory '%s'.",
            basename(uninstaller), dir)
        )
    }
    install_eplus_qt(ver, uninstaller, dir)
}
# }}}
# uninstall_eplus_linux {{{
uninstall_eplus_linux <- function(ver, dir, force = FALSE) {
    ver <- standardize_ver(ver)
    uninstaller <- normalizePath(file.path(dir, "uninstall.sh"), mustWork = FALSE)
    if (!file.exists(uninstaller)) {
        if (!force) {
            abort(sprintf(paste0(
                "Failed to locate 'uninstall.sh' under EnergyPlus installation directory '%s'. ",
                "Unable to remove symbolic links."
            ), dir))
        } else {
            warn(sprintf(paste0(
                "Failed to locate 'uninstall.sh' under EnergyPlus installation directory '%s'. ",
                "Symbolic links will NOT be removed"
            ), dir))
        }
    }

    if (!utils::file_test("-x", uninstaller)) {
        system(sprintf("sudo chmod +x %s", uninstaller))
    }

    verbose_info(sprintf("Removing symbolic links for EnergyPlus v%s.", ver))
    system(sprintf("sudo bash %s", uninstaller))
    verbose_info(sprintf("Removing installation directory of EnergyPlus v%s.", ver))
    system(sprintf("sudo rm -rf %s", dir))
}
# }}}
# uninstall_eplus_portable {{{
uninstall_eplus_portable <- function(ver, dir) {
    if (is_windows()) {
        unlink(dir, recursive = TRUE, force = TRUE)
    } else {
        # test if EnergyPlus directory is accessible for current user
        if (identical(unname(try(file.access(dir, 2L), silent = TRUE)), 0L)) {
            unlink(dir, recursive = TRUE, force = TRUE)
        } else {
            if (is_macos()) {
                sudo_on_mac(sprintf("rm -rf %s", dir))
            } else {
                system(sprintf("sudo rm -rf %s", dir))
            }
        }
    }
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
#' `use_eplus()` adds an EnergyPlus version into the EnergyPlus version cache in
#' eplusr. That cache will be used to get corresponding [Idd] object when
#' parsing IDF files and call corresponding EnergyPlus to run models.
#'
#' `eplus_config()` returns the a list of configure data of specified version of
#' EnergyPlus. If no data found, an empty list will be returned.
#'
#' `avail_eplus()` returns all versions of available EnergyPlus.
#'
#' `locate_eplus()` re-searches all EnergyPlus installations at the **default**
#' locations and returns versions of EnergyPlus it finds. Please note that all
#' configure data of EnergyPlus installed at custom locations will be
#' **removed**.
#'
#' `is_avail_eplus()` checks if the specified version of EnergyPlus is
#' available or not.
#'
#' @return
#' * For `use_eplus()` and `eplus_config()`, an (invisible for
#'   `use_eplus()`) list of three contains EnergyPlus version, directory and
#'   EnergyPlus executable.  version of EnergyPlus;
#' * For `avail_eplus()`, a [numeric_version][base::numeric_version()] vector or `NULL` if no
#'   available EnergyPlus is found;
#' * For `is_avis_avail_eplus()`, a scalar logical vector.
#'
#' @rdname use_eplus
#' @examples
#' \dontrun{
#' # add specific version of EnergyPlus
#' use_eplus("8.9")
#' use_eplus("8.8.0")
#'
#' # get configure data of specific EnergyPlus version if avaiable
#' eplus_config("8.6")
#' }
#'
#' # get all versions of avaiable EnergyPlus
#' avail_eplus()
#'
#' # check if specific version of EnergyPlus is available
#' is_avail_eplus("8.5")
#' is_avail_eplus("8.8")
#'
#' @seealso [download_eplus()] and [install_eplus()] for downloading and
#' installing EnergyPlus
#'
#' @export
# use_eplus {{{
#' @importFrom checkmate assert_vector
use_eplus <- function(eplus) {
    assert_vector(eplus, len = 1L)

    ver <- convert_to_eplus_ver(eplus, strict = TRUE, max = FALSE)[[1L]]
    # if eplus is a version, try to locate it in the default path
    if (!anyNA(ver)) {
        ori_ver <- ver[, 1:2]
        # try user-level first
        eplus_dir <- eplus_default_path(ver, local = TRUE)
        dir_cache <- eplus_dir
        if (any(chk <- is_eplus_path(eplus_dir))) {
            if (sum(chk) > 1L) {
                verbose_info("Multiple versions found for EnergyPlus v", ori_ver, " in user directory: ",
                    collapse(paste0("v", ver)), ". ",
                    "The last patched version v", max(ver), " will be used. ",
                    "Please explicitly give the full version if you want to use the other versions."
                )
                # which.max does not work with numeric_version objects
                eplus_dir <- eplus_dir[max(order(ver))]
                ver <- max(ver)
            } else {
                eplus_dir <- eplus_dir[chk]
                ver <- ver[chk]
                verbose_info("Found EnergyPlus v", ori_ver, " in user directory: ", eplus_dir)
            }
        # try system-level default location
        } else if (any({eplus_dir <- eplus_default_path(ver); chk <- is_eplus_path(eplus_dir)})) {
            if (sum(chk) > 1L) {
                verbose_info("Multiple versions found for EnergyPlus v", ori_ver, " in system directory: ",
                    collapse(paste0("v", ver)), ". ",
                    "The last patched version v", max(ver), " will be used. ",
                    "Please explicitly give the full version if you want to use the other versions."
                )
                # which.max does not work with numeric_version objects
                eplus_dir <- eplus_dir[max(order(ver))]
                ver <- max(ver)
            } else {
                eplus_dir <- eplus_dir[chk]
                ver <- ver[chk]
                verbose_info("Found EnergyPlus v", ori_ver, " in system directory: ", eplus_dir)
            }
        } else {
            msg <- NULL
            if (length(ver) > 1L) {
                msg <- paste0("Multiple possible versions found for EnergyPlus v", ori_ver, ": ",
                    collapse(paste0("v", ver)), ".\n")
            }

            fail <- paste0("Cannot locate EnergyPlus v", ori_ver, " at default ",
                "installation path ", surround(c(dir_cache, eplus_dir)), collapse = "\n")
            abort(paste0(msg, fail, "\nPlease specify explicitly the path of EnergyPlus installation."), "locate_eplus")
        }
    } else if (is_eplus_path(eplus)) {
        ver <- get_ver_from_eplus_path(eplus)
        eplus_dir <- eplus
    } else {
        abort("`eplus` should be either a valid EnergyPlus version or an EnergyPlus installation path.")
    }

    eplus_dir <- normalizePath(eplus_dir)
    exe <- paste0("energyplus", if (is_windows()) ".exe" else "")
    res <- list(version = ver, dir = eplus_dir, exe = exe)

    ori <- .globals$eplus[[as.character(ver)]]
    .globals$eplus[[as.character(ver)]] <- res

    if (is.null(ori)) {
        verbose_info("EnergyPlus v", ver, " located at ", surround(eplus_dir),
            " has been added.")
    } else if (identical(ori$dir, eplus_dir)) {
        verbose_info("Configure data of EnergyPlus v", ver, " located at ",
            surround(eplus_dir), " already exists. No Updating performed.")
    } else {
        verbose_info("Update configure data of EnergyPlus v", ver, ":\n",
            "  Former location: ", surround(ori$dir), " ---> ",
                 "New location: ", surround(eplus_dir))
    }

    if (ver < "8.3") {
        verbose_info("NOTE: Currently, eplusr only supports running IDFs of EnergyPlus v8.3 and above. ",
            "This is because eplusr uses EnergyPlus command line interface ",
            "which is available only in EnergyPlus v8.3 and above. ",
            "However, IDF modifications are supported regardless of versions."
        )
    }

    invisible(res)
}
# }}}

#' @rdname use_eplus
#' @export
# eplus_config {{{
eplus_config <- function(ver) {
    assert_vector(ver, len = 1L)
    ver_m <- convert_to_eplus_ver(ver, all_ver = names(.globals$eplus))

    if (is.na(ver_m)) {
        warn(paste0("Failed to find configuration data of EnergyPlus v", standardize_ver(ver)), "miss_eplus_config")
        return(list())
    }

    .globals$eplus[[as.character(ver_m)]]
}
# }}}

#' @rdname use_eplus
#' @export
# avail_eplus {{{
avail_eplus <- function() {
    res <- names(.globals$eplus)
    if (!length(res)) return(NULL)
    sort(numeric_version(res))
}
# }}}

#' @rdname use_eplus
#' @export
# is_avail_eplus {{{
is_avail_eplus <- function(ver) {
    length(suppressWarnings(eplus_config(ver))) > 0L
}
# }}}

#' @rdname use_eplus
#' @export
# locate_eplus {{{
locate_eplus <- function() {
    find_eplus <- function(ver) {
        suppressMessages(tryCatch(use_eplus(ver),
            error = function(e) NULL))
    }

    lapply(rev(ALL_EPLUS_RELEASE_COMMIT$version), find_eplus)

    avail_eplus()
}
# }}}
# eplus_default_path {{{
eplus_default_path <- function(ver, local = FALSE) {
    if (anyNA(ver <- convert_to_eplus_ver(ver))) {
        stop("'ver' must be a vector of valid EnergyPlus versions")
    }

    ver_dash <- paste0(ver[, 1L], "-", ver[, 2L], "-", ver[, 3L])
    if (is_windows()) {
        if (local) {
            d <- get_win_user_path()
            if (d == "") return(NA_character_)
            d <- normalizePath(file.path(d, paste0("EnergyPlusV", ver_dash)), mustWork = FALSE)
        } else {
            d <- paste0("C:\\EnergyPlusV", ver_dash)
        }
    } else if (is_linux()) {
        if (local) {
            d <- paste0("~/.local/EnergyPlus-", ver_dash)
        } else {
            d <- paste0("/usr/local/EnergyPlus-", ver_dash)
        }
    } else {
        if (local) {
            d <- paste0("~/Applications/EnergyPlus-", ver_dash)
        } else {
            d <- paste0("/Applications/EnergyPlus-", ver_dash)
        }
    }
    d
}
# }}}
# get_ver_from_eplus_path {{{
get_ver_from_eplus_path <- function(path) {
    idd_file <- normalizePath(file.path(path, "Energy+.idd"), mustWork = TRUE)

    tryCatch(get_idd_ver(read_lines(idd_file, nrows = 1L)),
        error = function(e) {
            stop("Failed to parse EnergyPlus version using IDD ",
                surround(idd_file), ".\n", conditionMessage(e)
            )
        }
    )
}
# }}}

# vim: set fdm=marker:
