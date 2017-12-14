################################################################################
#                              On-load Functions                               #
################################################################################
.onLoad <- function(libname, pkgname) {
    eplus_info <- tryCatch(find_eplus(verbose = FALSE),
                           error = function(e) {dplyr::tibble(path = "", version = "")})
    eplus_info_latest <- eplus_info[length(eplus_info),]

    op <- options()

    # For use `sQuote` and `sQuote`
    options(useFancyQuotes = FALSE)

    op.eplusr <- list(
       eplusr.temp_dir = normalizePath(file_path(tempdir(), "eplusr"), mustWork = FALSE),
       eplusr.eplus_dir = eplus_info_latest[["path"]],
       eplusr.parallel_num = parallel::detectCores()
    )

    toset <- !(names(op.eplusr) %in% names(op))
    if(any(toset)) options(op.eplusr[toset])

    if (!dir.exists(op.eplusr$eplusr.temp_dir)) {
      dir.create(op.eplusr$eplusr.temp_dir, showWarnings = FALSE, recursive = TRUE)
    }

    invisible()
}

.onAttach <- function (libname, pkgname) {
    v <- utils::packageVersion("eplusr")
    packageStartupMessage("eplusr ", v)

    eplus_info <- tryCatch(find_eplus(verbose = FALSE),
                           error = function(e) {dplyr::tibble(path = "", version = "")})
    eplus_info_latest <- eplus_info[nrow(eplus_info),]

    eplus_dir <- eplus_info_latest[["path"]]
    eplus_ver <- eplus_info_latest[["version"]]

    if (eplus_dir == "") {
        packageStartupMessage("**********\nCould not find EnergyPlus installed path.")
        packageStartupMessage("You may set it mannually by change the option 'eplusr.eplus_dir'.\n**********")
    } else {
        if (length(eplus_info) > 1L) {
            packageStartupMessage("**********\nMultiple EnergyPlus Versions (",
                                  paste0(paste0("'", eplus_info[["version"]], "'"), collapse = ", "),
                                  ") have been found.")
            packageStartupMessage("The lasted verion '", eplus_ver, "' will be used as default which located:\n\n", eplus_dir)
            packageStartupMessage("\nYou can change it mannually in the option 'eplusr.eplus_dir'.\n**********")
        } else {
            packageStartupMessage("**********\nEnergyPlus Version: ", attr(eplus_dir, eplus_ver),
                                  " has been successfully located:\n", eplus_dir, "\n**********")
        }
    }
}
