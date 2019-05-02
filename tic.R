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
if (ci_get_branch() == "master" && Sys.getenv("TRAVIS_OS_NAME") == "linux") {
    do_pkgdown(commit_paths = "docs/*", document = TRUE, path = "docs", branch = "gh-pages")
}
