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
#' `/Applications/EnergyPlus-X-Y-0` on MacOS and stores all found locations
#' internally. This data is used to locate the distributed "Energy+.idd" file of
#' each EnergyPlus version. And also, every time an IDD file is parsed, an `Idd`
#' object is created and cached in an environment.
#'
#' Parsing an IDD file starts from [use_idd()].  When using [use_idd()], eplusr
#' will first try to find the cached `Idd` object of that version, if possible.
#' If failed, and EnergyPlus of that version is available (see [avail_eplus()]),
#' the `"Energy+.idd"` distributed with EnergyPlus will be parsed and cached. So
#' each IDD file only needs to be parsed once and can be used when parsing every
#' IDF file of that version.
#'
#' Internally, the powerful [data.table](https://cran.r-project.org/package=data.table)
#' package is used to speed up the whole IDD parsing process and store the
#' results. However, it will still take about 3-4 sec per IDD.  Under the hook,
#' eplusr uses a SQL-like structure to store both IDF and IDD data in
#' `data.frame` format. Every IDD will be parsed and stored in four tables:
#'
#' * `group`: contains group index and group names.
#' * `class`: contains class names and properties.
#' * `class_reference`: contains reference names of classes.
#' * `field`: contains field names and field properties.
#' * `field_reference`: contains reference names of fields.
#' * `field_default`: contains default values of fields.
#' * `field_choice`: contains choices of choice-type fields.
#' * `field_range`: contains range data of fields.
#' * `field_object_list`: contains object-list data of fields.
#' * `field_external_list`: contains external-list data of fields.
#'
#' @section Usage:
#' \preformatted{
#' idd$version()
#' idd$build()
#' idd$group_index(group = NULL)
#' idd$group_name()
#' idd$is_valid_group(group)
#' idd$from_group(class)
#' idd$class_index(class = NULL)
#' idd$class_name()
#' idd$is_valid_class(class)
#' idd$required_class_name()
#' idd$unique_class_name()
#' idd$extenesible_class_name()
#' idd$object(class)
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
#'
#' @section Detail:
#'
#' `$version()` returns the version string.
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
#' `$objectN` returns a list of `IddObject`s of specified classes.
#'
#' `$object_in_group()` returns a list of `IddObject`s in that group.
#'
#' eplusr also provides custom S3 method of \code{$} and \code{[[} to make it
#' more convenient to get a single `IddObject`. Basically, `idd$ClassName` and
#' \code{idd[[ClassName]]}, is equivalent to \code{idd$object(ClassName)[[1]]}.
#' Here, `ClassName` is a single valid class name where all characters other
#' than letters and numbers are replaced by a underscore `_`.
#'
#' For details about `IddObject`, please see [IddObject class][idd_object].
#'
#' @importFrom R6 R6Class
#' @importFrom data.table setattr
#' @importFrom cli cat_rule cat_bullet
#' @importFrom assertthat assert_that
#' @docType class
#' @name idd
#' @author Hongyuan Jia
#' @seealso [IddObject class][idd_object]
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
            private$m_uuid <- uuid::UUIDgenerate(use.time = TRUE)

            idd_file <- parse_idd_file(path)
            private$m_version<- idd_file$version
            private$m_build <- idd_file$build

            private$m_idd_tbl <- list2env(
                idd_file[!names(idd_file) %in% c("version", "build")], parent = emptyenv()
            )
            # assign tbls to IddObject R6Class Generator
            private$m_iddobj_generator <- create_iddobj_generator(self, private, IddObject)
        },
        # }}}

        # PROPERTY GETTERS {{{
        version = function ()
            i_version(self, private),

        build = function ()
            i_build(self, private),

        group_name = function ()
            i_group_name(self, private, type = "idd"),

        from_group = function (class)
            i_from_group(self, private, class = class),

        class_name = function ()
            i_class_name(self, private, type = "idd"),

        required_class_name = function ()
            i_required_class_name(self, private),

        unique_class_name = function ()
            i_unique_class_name(self, private),

        extensible_class_name = function ()
            i_extensible_class_name(self, private),

        group_index = function (group = NULL)
            i_group_index(self, private, group = group),

        class_index = function (class = NULL)
            i_class_index(self, private, name = class, type = "idd"),
        # }}}

        # OBJECT GETTERS {{{
        object = function (class)
            i_iddobject(self, private, class = class),

        object_in_group = function (group)
            i_iddobject_in_group(self, private, group = group),
        # }}}

        # ASSERTIONS {{{
        is_valid_group = function (group)
            i_is_valid_group_name(self, private, group = group),

        is_valid_class = function (class)
            i_is_valid_class_name(self, private, name = class, type = "idd"),
        # }}}

        print = function ()
            i_print_idd(self, private)
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_uuid = NULL,
        m_version = NULL,
        m_build = NULL,
        m_idd_tbl = NULL,
        m_iddobj_generator = NULL
        # }}}
    )
)
# }}}

#' @export
# [[.Idd {{{
'[[.Idd' <- function(x, i) {
    if (is_string(i)) {
        funs <- setdiff(ls(x), "initialize")
        if (i %in% funs) {
            NextMethod()
        } else {
            in_nm <- i_underscore_name(i)

            self <- .subset2(.subset2(x, ".__enclos_env__"), "self")
            priv <- .subset2(.subset2(x, ".__enclos_env__"), "private")

            all_nm <- i_class_name(self, priv, type = "idd")

            all_nm_u <- i_underscore_name(all_nm)

            m <- match(in_nm, all_nm_u)

            if (is.na(m)) {
                NextMethod()
            } else {
                .subset2(x, "object")(all_nm[m])[[1]]
            }
        }
    } else {
        NextMethod()
    }
}
# }}}

#' @export
# $.Idd {{{
'$.Idd' <- function (x, name) {
    if (is_string(name)) {
        funs <- setdiff(ls(x), "initialize")
        if (name %in% funs) {
            NextMethod()
        } else {
            in_nm <- i_underscore_name(name)

            self <- .subset2(.subset2(x, ".__enclos_env__"), "self")
            priv <- .subset2(.subset2(x, ".__enclos_env__"), "private")

            all_nm <- i_class_name(self, priv, type = "idd")

            all_nm_u <- i_underscore_name(all_nm)

            m <- match(in_nm, all_nm_u)

            if (is.na(m)) {
                NextMethod()
            } else {
                .subset2(x, "object")(all_nm[m])[[1]]
            }
        }
    } else {
        NextMethod()
    }
}
# }}}

# read_idd {{{
read_idd <- function (path) {
    idd <- Idd$new(path)

    .globals$idd[[as.character(idd$version())]] <- idd

    idd
}
# }}}

#' Use a specific EnergyPlus Input Data Dictionary (IDD) file
#'
#' `use_idd()` takes a path of an EnergyPlus Input Data Dictionary (IDD) file,
#' usually named "Energy+.idd" and return an `Idd` object. For details on `Idd`
#' class, please see [idd].
#'
#' `download_idd()` downloads specified version of EnergyPlus IDD file from
#' [EnergyPlus GitHub Repository](https://github.com/NREL/EnergyPlus).
#'
#' `avail_idd()` returns versions of all cached `Idd` object.
#'
#' `is_avail_idd()` returns `TRUE` if input version of IDD file has been parsed
#' and cached.
#'
#' @param idd A path to an EnergyPlus Input Data Dictionary (IDD) file, usually
#'     named as `Energy+.idd` or a valid version of IDD, e.g. 8.9, "8.9.0".
#' @param download If `"TRUE"` and argument `idd`, the IDD file will be
#'     downloaded from [EnergyPlus GitHub Repository](https://github.com/NREL/EnergyPlus),
#'     and saved to [tempdir()]. It will be parsed after it is downloaded
#'     successfully. It is useful in case where you only want to edit an
#'     EnergyPlus Input Data File (IDF) directly but do not want to install
#'     whole EnergyPlus software.
#' @param ver A valid EnergyPlus version, e.g., 8.7, "8.7" or "8.7.0". For
#'     `download_idd`, The special value `"latest"` means the latest version
#'     (fetched from GitHub releases). Default: `"latest"`.
#' @param dir A directory to indicate where to save the IDD file. Default:
#'     current working directory.
#'
#' @details
#' eplusr tries to detect all installed EnergyPlus in default installation
#' locations when loading. If argument `idd` is a version, eplusr will first try
#' to find the cached `Idd` object of that version, if possible. If failed, and
#' EnergyPlus of that version is available (see [avail_eplus()]), the
#' `"Energy+.idd"` distributed with EnergyPlus will be parsed and stored in
#' eplusr Idd cache.
#'
#' `download_idd()` downloads EnergyPlus Input Data Dictionary (IDD) file from
#' [EnergyPlus GitHub Repository](https://github.com/NREL/EnergyPlus). It is
#' useful in case where you only want to edit an EnergyPlus Input Data File
#' (IDF) directly but do not want to install whole EnergyPlus software.
#'
#' @importFrom assertthat assert_that
#' @return `use_idd` returns an `Idd` object; `download_idd` returns the full
#'     path of the downloaded IDD file; `avail_idd` returns a character vector;
#'     `is_avail_idd` returns a single logical vector.
#' @export
#' @seealso [Idd Class][idd]
#' @examples
#' \dontrun{
#'
#' use_idd(8.8)
#'
#' download_idd()
#' download_idd(8.8)
#'
#' is_avail_idd(8.8)
#' }
# use_idd {{{
use_idd <- function (idd, download = FALSE) {
    assert_that(is_scalar(idd))

    if (is_eplus_ver(idd)) {
        ver <- standardize_ver(idd)

        # download Energy+.idd from EnergyPlus repo
        if (download) {

            message("Try to download Idd v", ver, " from EnergyPlus GitHub Repository...")

            path_idd <- download_idd(ver, dir = tempdir())

        } else {

            # if found in cache, return it directly
            if (is_parsed_idd_ver(ver)) return(.globals$idd[[as.character(ver)]])

            message("Idd v", ver, " has not been parsed before. Try to locate ",
                "`Energy+.idd` in EnergyPlus installation folder.")

            # stop if corresponding version of EnergyPlus is not found
            if (!is_avail_eplus(ver))
                stop("Failed to locate `Energy+.idd` because EnergyPlus v",
                    ver, " is not available.", call. = FALSE)

            config <- eplus_config(ver)
            path_idd <- normalizePath(file.path(config$dir, "Energy+.idd"), mustWork = FALSE)

            # stop if "Energy+.idd" file is not found in EnergyPlus location
            if (!file.exists(path_idd))
                stop("`Energy+.idd` file does not exist in EnergyPlus v",
                    config$version, " installation folder ",
                    backtick(config$dir), ".", call. = FALSE)
        }

        message("IDD file found: ", backtick(path_idd), ".\nStart parsing...")

        # read IDD file and store in cache
        res <- read_idd(path_idd)

    } else {

        message("Start parsing...")

        res <- read_idd(idd)
    }

    res
}
# }}}

#' @rdname use_idd
#' @export
# download_idd {{{
download_idd <- function (ver = "latest", dir = getwd()) {
    latest <- repo_releases("NREL", "EnergyPlus", "latest")
    latest_ver <- as.numeric_version(attr(latest, "version"))

    rels <- gh::gh("GET /repos/:owner/:repo/contents/:path", repo = "EnergyPlus",
        owner = "NREL", path = "idd", ref = paste0("v", latest_ver))

    if (as.character(ver) == "latest") ver <- latest_ver
    assert_that(is_eplus_ver(ver))
    ver <- standardize_ver(as.character(ver))

    file_nm <- purrr::map_chr(rels, "name")
    file_lnk <- purrr::map_chr(rels, "download_url")
    names(file_lnk) <- file_nm

    if (ver == latest_ver) {
        lnk <- file_lnk["Energy+.idd.in"]
    } else {
        all_ver_dash <- stringr::str_extract(file_nm, "(?<=V)(\\d-\\d-\\d)(?=-Energy\\+.idd)")
        all_ver <- as.numeric_version(all_ver_dash[!is.na(all_ver_dash)])
        all_valid_ver <- all_ver[all_ver >= 7.0]

        if (!ver %in% all_valid_ver)
            stop("Invalid IDD version found: ", ver, ". All possible versions are ",
                paste0(paste0("  * ", all_valid_ver), collapse = "\n"), call. = FALSE)

        ver_dash <- paste0("V", ver[,1], "-", ver[,2], "-", ver[,3], "-Energy+.idd")
        lnk <- file_lnk[ver_dash]
    }

    name <- sub("%2B", "+", basename(lnk))
    dest <- normalizePath(file.path(dir, name), mustWork = FALSE)
    res <- download_file(lnk, dest)

    if (res != 0L)
        stop(sprintf("Failed to download EnergyPlus IDD v%s.", ver), call. = FALSE)

    if (ver == latest_ver) {
        build <- latest[1L, stringr::str_extract(file, "(?<=EnergyPlus-\\d\\.\\d\\.\\d-).{10}(?=-)")]

        l <- readr::read_lines(dest)

        l[1] <- paste0("!IDD_Version ", ver)
        l[2] <- paste0("!IDD_BUILD ", build)

        readr::write_lines(l, dest)
    }

    message("EnergyPlus IDD file ", paste0("v", ver), " has been downloaded ",
        "successfully into ", dir, ".")

    dest
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
    assert_that(is_eplus_ver(ver))
    ver <- as.character(standardize_ver(ver))
    ver %in% names(.globals$idd)
}
# }}}
