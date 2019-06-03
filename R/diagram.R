# Run Post Processor HVAC-Diagram
#
# `hvac_diagram()` takes an EnergyPlus version and a file paht of `Bind Node
# Detail` (`.bnd`) file as input, runs post processor program `HVAC-Diagram`
# and returns the path of generated `.svg` file.
#
# @param eplus A valid EnergyPlus version.
# @param path A path of EnergyPlus `Bind Node Detail` (`.bnd`) file.
# @return File path of the generated `.svg` file if successful. Otherwise
# `NULL`.
# @export
# hvac_diagram {{{
hvac_diagram <- function (eplus, path) {
    assert(file.exists(path), has_ext(path, "bnd"))

    nm <- tools::file_path_sans_ext(basename(path))
    wd <- dirname(path)

    # change file names to eplusout.bnd
    if (tolower(nm) != "eplusout.bnd") {
        dir <- file.path(tempdir(), stringi::stri_rand_strings(1, 10))
        flag <- dir.create(dir, showWarnings = FALSE)
        if (!flag) stop("Internal error. Failed to copy temperory directory to store bnd file.")

        flag <- file.copy(path, file.path(dir, "eplusout.bnd"), copy.date = TRUE)

        if (!flag) stop("Internal error. Failed to copy bnd file.")

        wd <- dir
    }

    on.exit(unlink(wd, recursive = TRUE, force = TRUE), add = TRUE)

    loc <- paste0("PostProcess/HVAC-Diagram", if (is_windows()) ".exe" else "")
    if (is_eplus_ver(eplus, TRUE)) {
        exe <- normalizePath(file.path(eplus_config(eplus)$dir, loc), mustWork = TRUE)
    } else {
        exe <- normalizePath(file.path(dirname(eplus), loc), mustWork = TRUE)
    }

    p <- processx::run(exe, "eplusout.bnd", wd = wd, windows_verbatim_args = TRUE)

    if (p$status == 0) {
        svg <- normalizePath(file.path(dirname(path), paste0(nm, ".svg")), mustWork = FALSE)
        file.copy(file.path(wd, "eplusout.svg"), svg, copy.date = TRUE, overwrite = TRUE)
        invisible(svg)
    } else {
        invisible(NULL)
    }
}
# }}}
