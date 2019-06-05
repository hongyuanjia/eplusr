# R CMD Check
## OS specific build and check arguments
## get as cran from environment
args <- if (Sys.getenv("NOT_CRAN", FALSE)) c("--as-cran") else c()
build_args <- c("--force")

# do not build manual for appveyor
# also fix LaTeX Error: File `inconsolata.sty' not found on osx
# https://github.com/travis-ci/travis-ci/issues/7875
if (.Platform$OS.type == "windows" || Sys.getenv("TRAVIS_OS_NAME") == "osx") args <- c("--no-manual", args)

do_package_checks(args = args, build_args = build_args)

# pkgdown
# make sure to clean site to rebuild everything
if (ci_get_branch() == "master" && Sys.getenv("TRAVIS_OS_NAME") == "linux" && Sys.getenv("TRAVIS_R_VERSION_STRING") == "release") {
    do_pkgdown(document = TRUE, orphan = TRUE)
}
