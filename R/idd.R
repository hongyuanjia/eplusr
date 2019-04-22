#' @importFrom R6 R6Class
#' @include impl-idd.R
NULL

#' Parse, Query and Modify EnergyPlus Input Data Dictionary (IDD)
#'
#' eplusr provides parsing of and programmatic access to EnergyPlus
#' Input Data Dictionary (IDD) files, and objects. It contains all data needed
#' to parse EnergyPlus models. `Idd` class provides parsing and printing while
#' `IddObject` provides detailed information of curtain class.
#'
#' @section Overview:
#'
#' EnergyPlus operates off of text input files written in its own Input
#' Data File (IDF) format. IDF files are similar to XML files in that they are
#' intended to conform to a data schema written using similar syntax. For XML,
#' the schema format is XSD; for IDF, the schema format is IDD. For each release
#' of EnergyPlus, valid IDF files are defined by the "Energy+.idd" file shipped
#' with the release.
#'
#' eplusr tries to detect all installed EnergyPlus in default installation
#' locations when loading, i.e. `C:\\EnergyPlusVX-X-0` on Windows,
#' `/usr/local/EnergyPlus-X-Y-0` on Linux, and
#' `/Applications/EnergyPlus-X-Y-0` on macOS and stores all found locations
#' internally. This data is used to locate the distributed "Energy+.idd" file of
#' each EnergyPlus version. And also, every time an IDD file is parsed, an `Idd`
#' object is created and cached in an environment.
#'
#' Parsing an IDD file starts from [use_idd()]. When using [use_idd()], eplusr
#' will first try to find the cached `Idd` object of that version, if possible.
#' If failed, and EnergyPlus of that version is available (see [avail_eplus()]),
#' the `"Energy+.idd"` distributed with EnergyPlus will be parsed and cached. So
#' each IDD file only needs to be parsed once and can be used when parsing every
#' IDF file of that version.
#'
#' Internally, the powerful [data.table](https://cran.r-project.org/package=data.table)
#' package is used to speed up the whole IDD parsing process and store the
#' results. However, it will still take about 2-3 sec per IDD. Under the hook,
#' eplusr uses a SQL-like structure to store both IDF and IDD data in
#' [data.table::data.table] format. Every IDD will be parsed and stored in
#' four tables:
#'
#' * `group`: contains group index and group names.
#' * `class`: contains class names and properties.
#' * `field`: contains field names and field properties.
#' * `reference`: contains reference names of fields.
#'
#' @section Usage:
#' \preformatted{
#' idd$version()
#' idd$build()
#' idd$group_index(group = NULL)
#' idd$group_name()
#' idd$from_group(class)
#' idd$class_index(class = NULL)
#' idd$class_name()
#' idd$required_class_name()
#' idd$unique_class_name()
#' idd$extenesible_class_name()
#' idd$is_valid_group(group)
#' idd$is_valid_class(class)
#' idd$object(class)
#' idd$objects(class)
#' idd$objects_in_relation(class, direction = c("ref_by", "ref_to"))
#' idd$object_in_group(group)
#' idd$ClassName
#' idd[[ClassName]]
#' idd$print()
#' print(idd)
#' }
#'
#' @section Arguments:
#'
#' * `idd`: An `Idd` object.
#' * `group`: A valid group name or valid group names.
#' * `class`: A valid class name or valid class names.
#' * `ClassName`: A single length character vector of one valid class name.
#' * `Direciton`: The relation direction to extract. Should be either "ref_by"
#'   or "ref_to".
#'
#' @section Detail:
#'
#' `$version()` returns the IDD version in [base::numeric_version()].
#'
#' `$build()` returns the build tag string.
#'
#' `$group_index()` returns integer indexes (indexes of name appearance in
#'     the IDD file) of specified groups.
#'
#' `$group_name()` returns all group names.
#'
#' `$from_group()` returns the names of group that specified classes belongs to.
#'
#' `$is_valid_group()` return `TRUE` if the input is a valid group name.
#'
#' `$class_index()` returns integer indexes (indexes of name appearance in
#' the IDD file) of specified classes.
#'
#' `$class_name()` returns all class names.
#'
#' `$required_class_name()` returns the names of all required classes.
#'
#' `$unique_class_name()` returns the names of all unique classes.
#'
#' `$extensible_class_name()` returns the names of all extensible classes.
#'
#' `$is_valid_class()` return `TRUE` if the input is a valid class name.
#'
#' `$object` returns a list of `IddObject`s of specified classes.
#'
#' `$object_in_group()` returns a list of `IddObject`s in that group.
#'
#' eplusr also provides custom S3 method of \code{$} and \code{[[} to make it
#' more convenient to get a single `IddObject`. Basically, `idd$ClassName` and
#' \code{idd[[ClassName]]}, is equivalent to \code{idd$object(ClassName)[[1]]}.
#' Here, `ClassName` is a single valid class name where all characters other
#' than letters and numbers are replaced by a underscore `_`.
#'
#' For details about `IddObject`, please see [IddObject] class.
#'
#' @examples
#' # get the Idd object of EnergyPlus v8.8
#' idd <- use_idd(8.8, download = "auto")
#'
#' # version
#' idd$version()
#'
#' # build
#' idd$build()
#'
#' # all group names
#' str(idd$group_name())
#'
#' # all class names
#' str(idd$class_name())
#'
#' # all required class names
#' str(idd$required_class_name())
#'
#' # all unique class names
#' str(idd$unique_class_name())
#'
#' # IddObject of SimulationControl class
#' idd$SimulationControl
#' # OR
#' idd[["SimulationControl"]]
#'
#' @docType class
#' @name Idd
#' @seealso [IddObject] class which provides detailed information of
#' curtain class
#'
#' @author Hongyuan Jia
#' @references
#' [IDFEditor](https://github.com/NREL/EnergyPlus/tree/develop/src/IDF_Editor)
#' [OpenStudio utilities library](https://openstudio-sdk-documentation.s3.amazonaws.com/cpp/OpenStudio-2.5.1-doc/utilities/html/idf_page.html)
NULL

# Idd {{{
Idd <- R6::R6Class(classname = "Idd", cloneable = FALSE,

    public = list(
        # INITIALIZE {{{
        initialize = function (path) {
            # add a uuid
            private$m_uuid <- unique_id()

            idd_file <- parse_idd_file(path)
            private$m_version <- idd_file$version
            private$m_build <- idd_file$build

            private$m_idd_env <- list2env(
                idd_file[!names(idd_file) %in% c("version", "build")], parent = emptyenv()
            )

            # add current idd to .globals
            .globals$idd[[as.character(private$m_version)]] <- self
        },
        # }}}

        # PROPERTY GETTERS {{{
        version = function ()
            idd_version(self, private),

        build = function ()
            idd_build(self, private),

        group_name = function ()
            idd_group_name(self, private),

        from_group = function (class)
            idd_from_group(self, private, class),

        class_name = function (index = NULL)
            idd_class_name(self, private, index = index),

        required_class_name = function ()
            idd_required_class_name(self, private),

        unique_class_name = function ()
            idd_unique_class_name(self, private),

        extensible_class_name = function ()
            idd_extensible_class_name(self, private),

        group_index = function (group = NULL)
            idd_group_index(self, private, group),

        class_index = function (class = NULL)
            idd_class_index(self, private, class),
        # }}}

        # ASSERTIONS {{{
        is_valid_group = function (group)
            idd_is_valid_group_name(self, private, group),

        is_valid_class = function (class)
            idd_is_valid_class_name(self, private, class),
        # }}}

        # OBJECT GETTERS {{{
        object = function (class)
            idd_obj(self, private, class),

        objects = function (class)
            idd_objects(self, private, class),

        objects_in_relation = function (class, direction = c("ref_by", "ref_to"))
            idd_objects_in_relation(self, private, class),

        objects_in_group = function (group)
            idd_objects_in_group(self, private, group = group),
        # }}}

        print = function ()
            idd_print(self, private)
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_uuid = NULL,
        m_version = NULL,
        m_build = NULL,
        m_idd_env = NULL
        # }}}
    )
)
# }}}

# idd_version {{{
idd_version <- function (self, private) {
    private$m_version
}
# }}}
# idd_build {{{
idd_build <- function (self, private) {
    private$m_build
}
# }}}
# idd_group_name {{{
idd_group_name <- function (self, private) {
    get_idd_group_name(private$m_idd_env)
}
# }}}
# idd_from_group {{{
idd_from_group <- function (self, private, class) {
    get_idd_class(private$m_idd_env, class, "group_name")$group_name
}
# }}}
# idd_group_index {{{
idd_group_index <- function (self, private, group = NULL) {
    get_idd_group_index(private$m_idd_env, group)
}
# }}}
# idd_class_name {{{
idd_class_name <- function (self, private, index = NULL) {
    get_idd_class(private$m_idd_env, index)$class_name
}
# }}}
# idd_class_index {{{
idd_class_index <- function (self, private, class) {
    get_idd_class(private$m_idd_env, class)$class_id
}
# }}}
# idd_required_class_name {{{
idd_required_class_name <- function (self, private) {
    private$m_idd_env$class[required_object == TRUE, class_name]
}
# }}}
# idd_unique_class_name {{{
idd_unique_class_name <- function (self, private) {
    private$m_idd_env$class[unique_object == TRUE, class_name]
}
# }}}
# idd_extensible_class_name {{{
idd_extensible_class_name <- function (self, private) {
    private$m_idd_env$class[num_extensible > 0L, class_name]
}
# }}}
# idd_is_valid_group_name {{{
idd_is_valid_group_name <- function (self, private, group) {
    group %chin% private$m_idd_env$group$group_name
}
# }}}
# idd_is_valid_class_name {{{
idd_is_valid_class_name <- function (self, private, class) {
    class %chin% private$m_idd_env$class$class_name
}
# }}}
# idd_obj {{{
idd_obj <- function (self, private, class) {
    IddObject$new(class, self)
}
# }}}
# idd_objects {{{
idd_objects <- function (self, private, class) {
    res <- lapply(class, IddObject$new, self)
    setattr(res, "names", class)
    res
}
# }}}
# idd_objects_in_relation {{{
idd_objects_in_relation <- function (self, private, class, direction = c("ref_to", "ref_by")) {
    assert(!is.null(class), msg = "Please give class name.")
    rel <- get_idd_relation(private$m_idd_env, class, max_depth = 0L, direction = direction)
    res <- lapply(rel$class_id, IddObject$new, self)
    setattr(res, "names", rel$class_name)
    res
}
# }}}
# idd_objects_in_group {{{
idd_objects_in_group <- function (self, private, group) {
    assert(is_string(group))

    grp_id <- idd_group_index(self, private, group)

    cls <- private$m_idd_env$class[J(grp_id), on = "group_id", class_name]

    res <- lapply(cls, IddObject$new, self)
    setattr(res, "names", cls)
    res
}
# }}}
# idd_print {{{
idd_print <- function (self, private) {
    cli::cat_rule("EnergyPlus Input Data Dictionary")
    cli::cat_line("* ", c(
        paste0("Version", ": ", private$m_version),
        paste0("Build", ": ", private$m_build),
        paste0("Total Class", ": ", nrow(private$m_idd_env$class))
    ))
}
# }}}

#' @export
# [.Idd {{{
'[.Idd' <- function(x, i) {
    if (is_string(i)) {
        funs <- setdiff(ls(x), "initialize")
        if (i %in% funs) {
            NextMethod()
        } else {
            in_nm <- underscore_name(i)

            self <- ._get_self(x)
            priv <- ._get_private(x)

            all_nm <- idd_class_name(self, priv)
            all_nm_us <- underscore_name(all_nm)

            m <- chmatch(in_nm, all_nm_us)

            if (is.na(m)) {
                NextMethod()
            } else {
                idd_objects(self, priv, all_nm[m])
            }
        }
    } else {
        NextMethod()
    }
}
# }}}

#' @export
# [[.Idd {{{
`[[.Idd` <- function(x, i) {
    assert(is_scalar(i))
    if (i %in% setdiff(ls(x), "initialize")) {
        NextMethod()
    } else {
        .subset2(x, "object")(i)
    }
}
# }}}

#' @export
# $.Idd {{{
`$.Idd` <- function (x, i) {
    if (is_string(i)) {
        funs <- setdiff(ls(x), "initialize")
        if (i %in% funs) {
            NextMethod()
        } else {
            in_nm <- underscore_name(i)

            self <- ._get_self(x)
            priv <- ._get_private(x)

            all_nm <- idd_class_name(self, priv)
            all_nm_us <- underscore_name(all_nm)

            m <- chmatch(in_nm, all_nm_us)

            if (is.na(m)) {
                NextMethod()
            } else {
                idd_obj(self, priv, all_nm[m])
            }
        }
    } else {
        NextMethod()
    }
}
# }}}

# read_idd {{{
read_idd <- function (path) {
    Idd$new(path)
}
# }}}

#' Use a specific EnergyPlus Input Data Dictionary (IDD) file
#'
#' @param idd Either a path, a connection, or literal data (either a single
#'     string or a raw vector) to an EnergyPlus Input Data Dictionary (IDD)
#'     file, usually named as `Energy+.idd`, or a valid version of IDD, e.g.
#'     `8.9`, `"8.9.0"`.
#' @param download If `TRUE` and argument `idd`, the IDD file will be
#'     downloaded from [EnergyPlus GitHub Repository](https://github.com/NREL/EnergyPlus),
#'     and saved to [tempdir()]. It will be parsed after it is downloaded
#'     successfully. A special value of `"auto"` can be given, which will
#'     automatically download corresponding IDD file if the Idd object is
#'     currently not available. It is useful in case when you only want to edit
#'     an EnergyPlus Input Data File (IDF) directly but do not want to install
#'     whole EnergyPlus software. Default is `FALSE`.
#' @param ver A valid EnergyPlus version, e.g. `8`, `8.7`, `"8.7"` or `"8.7.0"`.
#'     For `download_idd()`, the special value `"latest"`, which is default,
#'     means the latest version.
#' @param dir A directory to indicate where to save the IDD file.
#'
#' @details
#' `use_idd()` takes a valid version or a path of an EnergyPlus Input Data
#'     Dictionary (IDD) file, usually named "Energy+.idd" and return an `Idd`
#'     object. For details on `Idd` class, please see [Idd].
#'
#' `download_idd()` downloads specified version of EnergyPlus IDD file from
#' [EnergyPlus GitHub Repository](https://github.com/NREL/EnergyPlus). It is
#' useful in case where you only want to edit an EnergyPlus Input Data File
#' (IDF) directly but do not want to install whole EnergyPlus software.
#'
#' `avail_idd()` returns versions of all cached `Idd` object.
#'
#' `is_avail_idd()` returns `TRUE` if input version of IDD file has been parsed
#' and cached.
#'
#' eplusr tries to detect all installed EnergyPlus in default installation
#' locations when loading. If argument `idd` is a version, eplusr will first try
#' to find the cached `Idd` object of that version, if possible. If failed, and
#' EnergyPlus of that version is available (see [avail_eplus()]), the
#' `"Energy+.idd"` distributed with EnergyPlus will be parsed and stored in
#' eplusr Idd cache.
#'
#' @return
#' * `use_idd()` returns an `Idd` object
#' * `download_idd()` returns an invisible integer `0` if succeed. Also an
#'   attribute named `file` which is the full path of the downloaded IDD file;
#' * `avail_idd()` returns a character vector or `NULL` if no available Idd object
#'   found
#' * `is_avail_idd()` returns a single logical vector.
#'
#' @examples
#' \dontrun{
#' # get all available Idd version
#' avail_idd()
#'
#' # check if specific version of Idd is available
#' is_avail_idd(8.5)
#'
#' # download latest IDD file from EnergyPlus GitHub repo
#' download_idd("latest", tempdir())
#'
#' # use specific version of Idd
#' # only works if EnergyPlus v8.8 has been found or Idd v8.8 exists
#' use_idd(8.8)
#'
#' # If Idd object is currently not avail_idd, automatically download IDD file
#' # from EnergyPlus GitHub repo and parse it
#' use_idd(8.8, download = "auto")
#'
#' # now Idd v8.8 should be available
#' is_avail_idd(8.8)
#'
#' # get specific version of parsed Idd object
#' use_idd(8.8)
#'
#' avail_idd() # should contain "8.8.0"
#' }
#' @seealso [Idd] Class for parsing, querying and making modifications to
#' EnergyPlus IDD file
#'
#' @export
#' @author Hongyuan Jia
# use_idd {{{
use_idd <- function (idd, download = FALSE) {
    if (is_idd(idd)) return(idd)

    if (!is_version(idd) || (is_version(idd) && !is_idd_ver(idd, only_exist = FALSE))) {
        return(tryCatch(read_idd(idd), error_read_file = function (e) {
            abort("error_invalid_idd_input",
                paste0("Parameter `idd` should be a valid version, a path, or ",
                    "a single character string of an EnergyPlus Input Data ",
                    "Dictionary (IDD) file (usually named `Energy+.idd`). ",
                    "Invalid input found: ",
                    if (length(idd) > 1L) {
                        surround(paste0(idd[1L], "..."))
                    } else {
                        surround(idd)
                    },
                    "."
                )
            )
        }))
    }

    if (is_idd_ver(idd, strict = FALSE, only_exist = TRUE)) {
        ver <- standardize_ver(idd)

        if (isTRUE(download)) {
            dl <- download_idd(ver, dir = tempdir())
            idd <- attr(dl, "file")

        } else {

            # if found in cache, return it directly
            if (is_avail_idd(ver)) return(.globals$idd[[as.character(ver)]])

            message("IDD v", ver, " has not been parsed before.\nTry to locate ",
                "`Energy+.idd` in EnergyPlus v", ver, " installation folder ",
                surround(eplus_default_path(ver)), ".")

            if (!is_avail_eplus(ver)) {

                msg_f <- paste0("Failed to locate `Energy+.idd` because EnergyPlus v",
                    ver, " is not available. ")

                if (!identical(download, "auto")) {
                    abort("error_no_matched_idd",
                        paste0(
                            msg_f, " You may want to set `download` to TRUE or ",
                            "\"auto\" to download the IDD file from EnregyPlus ",
                            "GitHub repo."
                        )
                    )
                }

                message(msg_f, "\nStarting to download the IDD file from EnergyPlus GitHub repo...")

                dl <- download_idd(ver, dir = tempdir())
                idd <- attr(dl, "file")

            } else {
                config <- eplus_config(ver)
                idd <- normalizePath(file.path(config$dir, "Energy+.idd"), mustWork = FALSE)

                if (!file.exists(idd)) {
                    msg_f <- paste0("`Energy+.idd` file does not exist in EnergyPlus v",
                        config$version, " installation folder ", surround(config$dir), ". ")

                    if (!identical(download, "auto")) {
                        stop(msg_f, "You may want to set `download` to TRUE or ",
                            "\"auto\" to download the IDD file from EnregyPlus ",
                            "GitHub repo.", call. = FALSE)
                    }

                    message(msg_f, "\nStarting to download the IDD file from EnergyPlus GitHub repo...")

                    dl <- download_idd(ver, dir = tempdir())
                    idd <- attr(dl, "file")
                }
            }
        }

        message("IDD file found: ", surround(idd), ".")
    }

    message("Start parsing...")
    idd <- read_idd(idd)
    message("Parsing completed.")
    idd
}
# }}}

#' @rdname use_idd
#' @export
# download_idd {{{
download_idd <- function (ver = "latest", dir) {
    ver <- standardize_ver(ver, no_patch = TRUE)
    assert(is_idd_ver(ver))

    base_url <- paste0("https://raw.githubusercontent.com/NREL/EnergyPlus/v", LATEST_EPLUS_VER, "/idd/")

    ver_dash <- paste0(ver[1,1], "-", ver[1,2], "-", ver[1,3])

    # only check the main and minor version, not patch
    if (identical(ver[1L, c(1L, 2L)], LATEST_EPLUS_VER[1L, c(1L, 2L)])) {
        file_url <- "Energy%2B.idd.in"
    } else {
        file_url <- paste0("V", ver_dash, "-Energy%2B.idd")
    }

    url <- paste0(base_url, file_url)

    file <- paste0("V", ver_dash, "-Energy+.idd")
    dest <- normalizePath(file.path(dir, file), mustWork = FALSE)
    res <- download_file(url, dest)

    if (res != 0L)
        stop(sprintf("Failed to download EnergyPlus IDD v%s.", ver), call. = FALSE)

    if (identical(ver, LATEST_EPLUS_VER)) {
        cmt <- ALL_EPLUS_RELEASE_COMMIT$commit[1]

        l <- read_lines(dest, trim = FALSE)

        l[1L:2L, string := c(paste0("!IDD_Version ", ver), paste0("!IDD_BUILD ", cmt))]

        write_lines(l, dest)
    }

    message("EnergyPlus v", ver, " IDD file ", surround(file), " has been successfully ",
        "downloaded into ", normalizePath(dir), ".")

    attr(res, "file") <- dest
    invisible(res)
}
# }}}

#' @rdname use_idd
#' @export
# avail_idd {{{
avail_idd <- function () names(.globals$idd)
# }}}

#' @rdname use_idd
#' @export
# is_avail_idd {{{
is_avail_idd <- function (ver) {
    assert(is_idd_ver(ver, strict = TRUE, only_exist = FALSE))
    as.character(standardize_ver(ver)) %in% names(.globals$idd)
}
# }}}

# get_idd_from_ver {{{
# Get Idd object from input IDF version
#
# @param idf_ver NULL or a valid IDF version
# @param idd NULL or valid input for [use_idd()]
# @param warn If `TRUE`, extra warning message will be shown
get_idd_from_ver <- function (idf_ver = NULL, idd = NULL, warn = TRUE) {
    if (!is.null(idf_ver)) {
        if (is.null(idd)) {
            # if input IDF has a version but neither that version of EnergyPlus
            # nor IDD is available, rewrite the message
            idd <- tryCatch(use_idd(idf_ver),
                error_no_matched_idd = function (e) {
                    mes <- stri_replace_all_fixed(conditionMessage(e),
                        "You may want to set `download`",
                        "You may want to use `use_idd()` and set `download`"
                    )
                    stop(mes)
                }
            )
        } else {
            idd <- use_idd(idd)
            if (warn && idf_ver != idd$version()) {
                warn("waring_idf_idd_mismatch_ver",
                    paste0(
                        "Version Mismatch. The IDF file parsing has a differnet ",
                        "version (", idf_ver, ") than the IDD file used (",
                        idd$version(), "). Parsing errors may occur."
                    ),
                    idf_ver = idf_ver,
                    idd_ver = idd$version()
                )
            }
        }
    } else {
        mes <- "Missing version field in input IDF."

        if (!is.null(idd)) {
            idd <- use_idd(idd)
            if (warn) {
                warn("warn_given_idd_used",
                    paste0(
                        mes, " The given IDD version ", idd$version(),
                        " will be used. Parsing errors may occur."
                    )
                )
            }
        } else {
            if (is.null(avail_idd())) {
                abort("error_no_avail_idd",
                    paste(mes, "No parsed IDD was available to use.")
                )
            }

            idd <- use_idd(avail_idd()[length(avail_idd())])
            if (warn) {
                warn("warn_latest_idd",
                    paste0(mes,
                        " The latest parsed IDD version ", idd$version(),
                        " will be used. Parsing errors may occur."
                    )
                )
            }
        }
    }
    idd
}
# }}}
