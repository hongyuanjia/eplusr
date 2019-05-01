# R CMD Check
## OS specific build and check arguments
## get as cran from environment
args <- if (Sys.getenv("NOT_CRAN", FALSE)) c("--as-cran") else c()
build_args <- c("--force")

## do not build manual and vignette on appveyor
if (.Platform$OS.type == "windows") {
    args <- c("--no-manual", "--no-build-vignettes", args)
    build_args <- c("--no-build-vignettes", build_args)
}

do_package_checks(args = args, build_args = build_args)

# pkgdown
if (ci_get_branch() == "master" && Sys.getenv("TRAVIS_OS_NAME") == "linux") {
    do_pkgdown(commit_paths = "docs/*", document = FALSE, path = "docs", branch = "gh-pages")
}
