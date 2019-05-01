# R CMD Check
## OS specific build and check arguments
## get as cran from environment
args <- if (Sys.getenv("NOT_CRAN", FALSE)) c("--as-cran") else c()
build_args <- c("--force")

## do not build manual and vignette on appveyor
if (.Platform$OS.type == "windows") {
    args <- c("--no-manual", "--no-build-vignettes", args)
    build_args <- c("--no-manual", "--no-build-vignettes", build_args)
}

do_package_checks(args = args, build_args = build_args)

# fix LaTeX Error: File `inconsolata.sty' not found
# https://github.com/travis-ci/travis-ci/issues/7875
if (Sys.getenv("TRAVIS_OS_NAME") == "osx") {
    get_stage("before_install") %>%
    add_code_step(system("sudo tlmgr install inconsolata"))
}

# pkgdown
if (ci_get_branch() == "master" && Sys.getenv("TRAVIS_OS_NAME") == "linux") {
    do_pkgdown(commit_paths = "docs/*", document = FALSE, path = "docs", branch = "gh-pages")
}
