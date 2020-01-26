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
#' @param ver The EnergyPlus version number, e.g., `8.7`. The special value
#'        `"latest"`, which is the default, means the latest version.
#'
#' @param local Whether to install EnergyPlus only for current user. For Windows
#'        and Linux, if `FALSE`, administrative privileges are required to
#'        install EnergyPlus to the default system-level location. See details.
#'        `local` should be also set to `FALSE` if you do not have the write
#'        access to the directory specified via `dir`. Default: `FALSE`. For
#'        macOS, administrative privileges are always required no matter you
#'        want EnergyPlus to be install at `/Applications` or `~/Applications`.
#'
#' @param dir
#'     * For `download_eplus()`, where to save EnergyPlus installer file.
#'       Default: `"."`.
#'     * For `install_eplus()`, the installer will always be saved into
#'       [tempdir()]. But you can use `dir` to specify the **parent** directory
#'       of EnergyPlus installation, i.e. the **parent** directory of
#'       `EnergyPlusVX-Y-0` on Windows and `EnergyPlus-X-Y-0` on Linux. macOS is
#'       not supported. If `NULL`, the default installation path will be used.
#'       See details for more information. Please note that `dir` does not work
#'       on macOS and EnergyPlus will always be installed into the default
#'       location. Default: `NULL`.
#'
#' @param force Whether to install EnergyPlus even if it has already been
#'        installed.
#'
#' @param ... Other arguments to be passed to the installer. Current only one
#'        additional argument exists and is only for Linux:
#'     * dir_bin: A path where symbolic links will be created to the software
#'       executables. The default is `/usr/local/bin` if `local` is `FALSE`
#'       and `~/.local/bin` if `local` is `TRUE`.
#'
#' @details
#'
#' `download_eplus()` downloads specified version of EnergyPlus from
#' [EnergyPlus GitHub Repository](https://github.com/NREL/EnergyPlus).
#'
#' `install_eplus()` will try to install EnergyPlus into the default location,
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
#' * macOS: `/Users/<User>/Applications/EnergyPlus-X-Y-0`
#' * Linux: `"~/.local/EnergyPlus-X-Y-0"`
#'
#' On Windows and Linux, you can also specify your custom directory using the
#' `dir` argument. Remember to change `local` to `FALSE` in order to ask for
#' administrator privileges if you do not have the write access to that
#' directory.
#'
#' Please note that on macOS, when `local` is set to `FALSE`, no symbolic links
#' will be created, since this process requires administrative privileges.
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
#' download_eplus(8.8, dir = tempdir())
#' install_eplus(8.8)
#'
#' # force to reinstall
#' install_eplus(8.8, force = TRUE)
#'
#' # install EnergyPlus in your home directory
#' install_eplus(8.8, local = TRUE, force = TRUE)
#'
#' # custom EnergyPlus install home directory
#' install_eplus(8.8, dir = "~/MyPrograms", local = TRUE, force = TRUE)
#' }
#' @author Hongyuan Jia
#' @export
# install_eplus {{{
install_eplus <- function (ver = "latest", local = FALSE, dir = NULL, force = FALSE, ...) {
    ver <- standardize_ver(ver)
    if (!is.null(dir)) assert(is_string(dir))

    # check if the same version has been installed already
    if (is_avail_eplus(ver)) {
        if (!isTRUE(force)) {
            abort("error_eplus_to_install_exists", paste0(
                "It seems EnergyPlus v", ver, " has been already installed at ",
                surround(eplus_config(ver)$dir), ". Set `force` to `TRUE` to reinstall."
            ))
        }

        if (is_macos() & ver >= 9.1) {
            abort("error_eplus_to_force_install_macos", paste0(
                "Cannot perform force reinstallation when EnergyPlus version is v9.1 and above. ",
                "Please first uninstall EnergyPlus v", ver, " at ",
                surround(eplus_config(ver)$dir), " and then run 'install_eplus(\"", ver, "\")'."
            ))
        }
    }

    verbose_info(sprintf("Starting to download EnergyPlus v%s...", ver), "\n", cli::rule(line = 2))

    dl <- download_eplus(ver, tempdir())

    verbose_info(sprintf("Starting to install EnergyPlus v%s...", ver), "\n", cli::rule(line = 2))

    if (!local)
        verbose_info("NOTE: Administrative privileges required during installation. ",
            "Please make sure R is running with an administrator acount or equivalent.")

    inst <- attr(dl, "file")
    res <- switch(os_type(),
           windows = install_eplus_win(ver, inst, local = local, dir = dir),
           linux = install_eplus_linux(ver, inst, local = local, dir = dir, ...),
           macos = install_eplus_macos(ver, inst, local = local))

    if (res != 0L) stop("Failed to install EnergyPlus v", ver, ".", call. = FALSE)

    path <- eplus_default_path(ver)
    verbose_info(sprintf("EnergyPlus v%s successfully installed into %s.", ver, path))

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
    ver <- match_minor_ver(standardize_ver(ver, complete = FALSE), ALL_EPLUS_VER, "eplus")
    url <- eplus_download_url(ver)
    file <- basename(url)

    dest <- normalizePath(file.path(dir, file), mustWork = FALSE)
    dl <- download_file(url, dest)

    if (dl != 0L) stop("Failed to download EnergyPlus v", ver, ".", call. = FALSE)

    verbose_info("The installer file of EnergyPlus ", paste0("v", ver), " ",
        surround(file), " has been successfully downloaded into ", dir, ".")

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
             collapse(all_cmt$version), ".", call. = FALSE)

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

    assert(is_eplus_ver(ver))

    ALL_EPLUS_RELEASE_COMMIT[version == as.character(ver)]
}
# }}}
# download_file: same as download.file except that it creates the target directory if necessary {{{
download_file <- function (url, dest) {
    if (file.exists(dest))
        tryCatch(unlink(dest),
            warning = function (w) {
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
install_eplus_win <- function (ver, exec, local = FALSE, dir = NULL) {
    if (is.null(dir)) {
        if (local) {
            dir <- get_win_user_path(error = TRUE)
        } else {
            dir <- "C:\\"
        }
    }

    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    dir <- normalizePath(file.path(dir, paste0("EnergyPlusV", gsub("\\.", "-", ver))), mustWork = FALSE)

    if (ver > 9.1) {
        system(sprintf("%s /S /D=%s", exec, dir))
    } else {
        install_eplus_qt(ver, exec, dir)
    }
}
# }}}
# get_win_user_path {{{
get_win_user_path <- function (error = FALSE) {
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

                abort("error_cannot_get_win_user", paste0(
                    "Cannot get the user-level install path because ",
                    "it failed to get current logged user name."
                ))
            }

            user <- gsub("\r\n", "", basename(whoami$stdout), fixed = TRUE)
        }
    }

    normalizePath(file.path("C:/Users", user, "AppData/Local"))
}
# }}}
# install_eplus_macos {{{
install_eplus_macos <- function (ver, exec, local = FALSE) {
    no_ext <- tools::file_path_sans_ext(basename(exec))

    # mount
    system(sprintf("hdiutil mount %s", exec))
    if (ver < 9.1) {
        if (local) {
            system(sprintf("installer -pkg /Volumes/%s/%s.pkg -target CurrentUserHomeDirectory", no_ext, no_ext))
        } else {
            system(sprintf("sudo installer -pkg /Volumes/%s/%s.pkg -target LocalSystem", no_ext, no_ext))
        }
    } else {
        ver_dash <- gsub("\\.", "-", ver)
        if (local) {
            dir <- normalizePath(file.path("~/Applications", paste0("EnergyPlus-", ver_dash)), mustWork = FALSE)
        } else {
            dir <- file.path("/Applications", paste0("EnergyPlus-", ver_dash))
        }
        exec <- sprintf("/Volumes/%s/%s.app/Contents/MacOS/%s", no_ext, no_ext, no_ext)
        system(sprintf('chmod +x %s', exec))
        install_eplus_qt(exec, dir, local = local)
    }
    system(sprintf("hdiutil unmount /Volumes/%s/", no_ext))
}
# }}}
# install_eplus_linux {{{
install_eplus_linux <- function (ver, exec, local = FALSE, dir = NULL, dir_bin = NULL) {
    if (local) {
        if (is.null(dir)) dir <- "~/.local"
        if (is.null(dir_bin)) dir_bin <- "~/.local/bin"
    } else {
        if (is.null(dir)) dir <- "/usr/local"
        if (is.null(dir_bin)) dir_bin <- "/usr/local/bin"
    }

    if (!is.null(dir_bin)) assert(is_string(dir_bin))
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    if (!dir.exists(dir_bin)) dir.create(dir_bin, recursive = TRUE)

    dir <- normalizePath(dir, mustWork = TRUE)
    dir_bin <- normalizePath(dir_bin, mustWork = TRUE)

    system(sprintf('chmod +x %s', exec))
    # EnergyPlus installation are broken since 9.1.0, which extract all files
    # directly into `/usr/local.
    # see https://github.com/NREL/EnergyPlus/issues/7256
    ver_dash <- gsub("\\.", "-", ver)
    if (ver >= 9.1) {
        if (Sys.which("sed") != "") {
            patch_eplus_linux_sh(ver, exec)
        } else {
            dir_eplus <- file.path(dir, paste0("EnergyPlus-", ver_dash))
            message("There is a known issue in EnergyPlus installation since v9.1.0 which ",
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

    if (local) {
        system(sprintf('echo "y\n%s\n%s" | %s', dir, dir_bin, exec))
        system(sprintf('chmod -R a+w %s/EnergyPlus-%s', dir, ver_dash))
    } else {
        system(sprintf('echo "y\n%s\n%s" | sudo %s', dir, dir_bin, exec))
        system(sprintf('sudo chmod -R a+w %s/EnergyPlus-%s', dir, ver_dash))
    }
}
# }}}
# patch_eplus_linux_sh {{{
patch_eplus_linux_sh <- function (ver, exec) {
    if (ver == "9.1.0") {
        system(sprintf("sed -i '%is/^.*$/%s/' %s", 47,
            "ori_install_directory=${install_directory}\\ninstall_directory=${install_directory}\\/${package_name}",
            exec
        ))
        # change the start line of tar.gz as a new line has been added above
        system(sprintf("sed -i '%is/+163/+164/' %s", 80, exec))
        system(sprintf("sed -i '%is/^.*$/%s/' %s", 89,
            "install_directory=${ori_install_directory}",
            exec
        ))
    } else if (ver > 9.1) {
        system(sprintf("sed -i '%is/^.*$/%s/' %s", 70,
            "install_directory=${install_directory}\\/${package_name}",
            exec
        ))
    }
}
# }}}
# install_eplus_qt {{{
install_eplus_qt <- function (exec, dir, local = FALSE) {
    # create a tempfile of QTIFW control script
    ctrl <- tempfile(fileext = ".qs")
    # NOTE: shoud escape slash twice here
    if (is_windows()) dir <- gsub("\\", "\\\\", dir, fixed = TRUE)
    write_lines(file = ctrl, x = paste0("
        function Controller() {
            installer.setMessageBoxAutomaticAnswer('OverwriteTargetDirectory', QMessageBox.Yes);
            installer.setMessageBoxAutomaticAnswer('TargetDirectoryInUse', QMessageBox.Ok);
            installer.setMessageBoxAutomaticAnswer('cancelInstallation', QMessageBox.Yes);
            installer.installationFinished.connect(function() {
                gui.clickButton(buttons.NextButton);
            })
        };

        Controller.prototype.IntroductionPageCallback = function() {
            gui.clickButton(buttons.NextButton);
            var page = gui.currentPageWidget()
            page.completeChanged.connect(function() {
                gui.clickButton(buttons.NextButton);
            });
        };

        Controller.prototype.TargetDirectoryPageCallback = function() {
            var page = gui.pageWidgetByObjectName('TargetDirectoryPage');
            page.TargetDirectoryLineEdit.setText(\"", dir, "\");
            gui.clickButton(buttons.NextButton);
        };

        Controller.prototype.ComponentSelectionPageCallback = function() {
        ",
        if (local) {
            "gui.currentPageWidget().deselectComponent(\"Symlinks\");"
        },
        "
            gui.clickButton(buttons.NextButton);
        };

        Controller.prototype.LicenseAgreementPageCallback = function() {
            gui.currentPageWidget().AcceptLicenseRadioButton.setChecked(true);
            gui.clickButton(buttons.NextButton);
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
    system(sprintf("%s --script %s", exec, ctrl))
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
#' * For `avail_eplus()`, a [numeric_version][base::numeric_version()] vector or `NULL` if no
#'   available EnergyPlus is found;
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
    assert(is_scalar(eplus))

    # if eplus is a version, try to locate it in the default path
    if (is_eplus_ver(eplus, strict = TRUE)) {
        ver <- standardize_ver(eplus, complete = FALSE)
        ori_ver <- ver

        # have to check all possible patched versions
        all_ver <- unique(c(ALL_IDD_VER, names(.globals$eplus_config)))
        if (is.na(ver[, 3L])) {
            ver <- numeric_version(all_ver[ver == numeric_version(all_ver)[, 1L:2L]])
        } else {
            ver <- numeric_version(all_ver[ver == numeric_version(all_ver)])
        }

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

            fail <- paste0("Cannot locate EnergyPlus v", stringi::stri_trim_both(eplus), " at default ",
                "installation path ", surround(c(dir_cache, eplus_dir)), collapse = "\n")
            abort("error_cannot_locate_eplus", paste0(msg, fail, "\n",
                "Please specify explicitly the path of EnergyPlus installation."
            ))
        }
    } else if (is_eplus_path(eplus)){
        ver <- get_ver_from_path(eplus)
        eplus_dir <- eplus
    } else {
        abort("error_invalid_eplus_input", paste0("`eplus` should be either a ",
            "valid EnergyPlus version or an EnergyPlus installation path."
        ))
    }

    exe <- paste0("energyplus", if (is_windows()) ".exe" else "")
    res <- list(version = ver, dir = normalizePath(eplus_dir), exe = exe)

    ori <- .globals$eplus_config[[as.character(ver)]]
    .globals$eplus_config[[as.character(ver)]] <- res

    if (is.null(ori)) {
        verbose_info("EnergyPlus v", ver, " located at ", surround(eplus_dir),
            " has been added.")
    } else if (identical(ori$dir, eplus_dir)) {
        verbose_info("Configure data of EnergyPlus v", ver, " located at ",
            surround(eplus_dir), " already exists. No Updating performed.")
    } else {
        verbose_info("Update configure data of EnergyPlus v", ver, ":\n",
            "    Former location: ", surround(ori$dir), " ---> ",
                   "New location: ", surround(eplus_dir))
    }

    if (ver < 8.3) {
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
eplus_config <- function (ver) {
    assert(is_idd_ver(ver, strict = TRUE))
    ver <- standardize_ver(ver, complete = FALSE)
    ver_m <- match_minor_ver(ver, names(.globals$eplus_config), "eplus")
    if (is.na(ver)) {
        warn("warning_miss_eplus_config",
            "Failed to find configuration data of EnergyPlus v", ver, ".",
            call. = FALSE)
        return(list())
    }

    .globals$eplus_config[[as.character(ver_m)]]
}
# }}}

#' @rdname use_eplus
#' @export
# avail_eplus {{{
avail_eplus <- function () {
    res <- names(.globals$eplus_config)
    if (!length(res)) return(NULL)
    sort(numeric_version(res))
}
# }}}

#' @rdname use_eplus
#' @export
# is_avail_eplus {{{
is_avail_eplus <- function (ver) {
    length(suppressWarnings(eplus_config(ver))) > 0L
}
# }}}

# locate_eplus {{{
locate_eplus <- function () {
    find_eplus <- function (ver) {
        suppressMessages(tryCatch(use_eplus(ver),
            error = function (e) NULL))
    }

    lapply(rev(ALL_EPLUS_RELEASE_COMMIT$version), find_eplus)

    invisible()
}
# }}}
# eplus_default_path {{{
eplus_default_path <- function (ver, local = FALSE) {
    ver <- standardize_ver(ver)
    assert(is_idd_ver(ver))
    ver_dash <- paste0(ver[, 1L], "-", ver[, 2L], "-", ver[, 3L])
    if (is_windows()) {
        if (local) {
            d <- get_win_user_path()
            if (d == "") return(NA_character_)
            d <- normalizePath(file.path(d, paste0("EnergyPlusV", ver_dash)), "/", FALSE)
        } else {
            d <- paste0("C:/EnergyPlusV", ver_dash)
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
# get_ver_from_path {{{
get_ver_from_path <- function (path) {
    idd_file <- normalizePath(file.path(path, "Energy+.idd"), mustWork = TRUE)

    tryCatch(get_idd_ver(read_lines(idd_file, nrows = 1L)),
        error_miss_idd_ver = function (e) {
            stop("Failed to parse EnergyPlus version using IDD ",
                surround(idd_file), ".\n", conditionMessage(e)
            )
        },
        error_invalid_idd_ver = function (e) {
            stop("Failed to parse EnergyPlus version using IDD ",
                surround(idd_file), ".\n", conditionMessage(e)
            )
        },
        error_multi_idd_ver = function (e) {
            stop("Failed to parse EnergyPlus version using IDD ",
                surround(idd_file), ".\n", conditionMessage(e)
            )
        }
    )
}
# }}}
