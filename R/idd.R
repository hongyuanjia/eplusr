#' Parse EnergyPlus IDD files
#'
#' eplusr provides parsing of and programmatic access to EnergyPlus
#' Input Data Dictionary (IDD) files, and objects. It contains all data needed
#' to parse EnergyPlus models. `Idd` class provides parsing and printing while
#' `IddObject` provides detailed information of curtain class.
#'
#' @details The `Idd` objects for EnergyPlus 8.5 to 8.8 have been pre-parsed and
#' stored internally and will automatically be used when parsing IDF files and
#' strings.  Internally, the powerful \code{data.table} package is used to speed
#' up the whole process and store the results. However, it will still take about
#' 5-6 sec to parse an IDD file.
#'
#' Normally, you may not need to parse any Energy+.idd file unless your model
#' is produced by EnergyPlus whose version is lower than 8.5. If so, it is
#' suggested to store the parsed IDD object and directly pass it to the `idd`
#' argument in [read_idf()] in order to avoid the parsing process whenever you
#' read a model of that version.
#'
#' Under the hook, eplusr uses a SQL-like structure to store both IDF and IDD
#' data in `data.frame` format. Every IDD will be parsed and stored in four
#' tables:
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
#' ```
#' # read
#' idd <- use_idd(idd)
#'
#' # basic info
#' idd$version()
#' idd$build()
#'
#' idd$group_index(group = NULL)
#' idd$group_name()
#' idd$is_valid_group(group)
#' idd$from_group(class)
#'
#' idd$class_index(class = NULL)
#' idd$class_name()
#' idd$is_valid_class(class)
#'
#' idd$required_class_name()
#' idd$unique_class_name()
#' idd$extenesible_class_name()
#'
#' # idd object
#' idd$object(class)
#' idd$object_in_group(group)
#'
#' idd$print()
#' print(idd)
#' ```
#'
#' @section Arguments:
#'
#' * `idd`: Path to an EnergyPlus Input Data Dictionary (IDD) file, usually
#'     named as `Energy+.idd` or a valid version of pre-parsed IDD (8.3 - 8.9).
#' * `group`: A valid group name or valid group names.
#' * `class`: A valid class name or valid class names.
#'
#' @section Detail:
#'
#' `use_idd` will parses an EnergyPlus Input Data Dictionary (IDD) file, and
#' returns an `Idd` object. If `idd` is a valid version of pre-parsed IDD file,
#' then the pre-parsed `Idd` object will be returned.
#'
#' `$version` returns the version string.
#'
#' `$build` returns the build tag string.
#'
#' `$group_index` returns integer indexes (indexes of name appearance in
#'     the IDD file) of specified groups.
#'
#' `$group_name` returns all group names.
#'
#' `$from_group` returns the names of group that specified classes belongs to.
#'
#' `$is_valid_group` return `TRUE` if the input is a valid group name.
#'
#' `$class_index` returns integer indexes (indexes of name appearance in
#' the IDD file) of specified classes.
#'
#' `$class_name` returns all class names.
#'
#' `$required_class_name` returns the names of all required classes.
#'
#' `$unique_class_name` returns the names of all unique classes.
#'
#' `$extensible_class_name` returns the names of all extensible classes.
#'
#' `$is_valid_class` return `TRUE` if the input is a valid class name.
#'
#' `$object` returns a list of `IddObject`s of specified classes.
#'
#' `$object_in_group` returns a list of `IddObject`s in that group.
#'
#' For details about `IddObject`, please see [idd_object].
#'
#' @importFrom R6 R6Class
#' @importFrom data.table setattr
#' @importFrom cli cat_rule cat_bullet
#' @importFrom assertthat assert_that
#' @return For `use_idd`, an Idd object.
#' @docType class
#' @name idd
#' @author Hongyuan Jia
#' @references
#' \href{https://github.com/NREL/EnergyPlus/tree/develop/src/IDF_Editor}{IDFEditor
#' source code}
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

# read_idd {{{
read_idd <- function (path) {
    idd <- Idd$new(path)

    .globals$idd[[as.character(idd$version())]] <- idd

    idd
}
# }}}

#' Use a specific EnergyPlud Input Data Dictionary (IDD) file
#'
#' `use_idd` takes a path of an EnergyPlus Input Data Dictionary (IDD) file,
#' usually named "Energy+.idd" and return an `Idd` object. For details on `Idd`
#' class, please see [idd].
#'
#' @param idd A path to an EnergyPlus Input Data Dictionary (IDD) file, usually
#'     named as `Energy+.idd` or a valid version of IDD, e.g. 8.9, "8.9.0".
#' @param download If `"TRUE"` and argument `idd`, the IDD file will be
#'     downloaded from [EnergyPlus GitHub Repository](https://github.com/NREL/EnergyPlus),
#'     and will be parsed after it is downloaded successfully.It is useful in
#'     case where you only want to edit an EnergyPlus Input Data File (IDF)
#'     directly but donnot want to install whole EnergyPlus software.
#'
#' @details eplusr tries to detect all installed EnergyPlus in default
#'     installation locations when loading. If argument `idd` is a version,
#'     eplusr will first try to find the cached `Idd` object of that version, if
#'     possible. If failed, and EnergyPlus of that version is available (see
#'     [avail_eplus()]), the `"Energy+.idd"` distributed with EnergyPlus will be
#'     parsed and stored in eplusr's Idd cache.
#'
#' @importFrom assertthat assert_that
#' @return An `Idd` object
#' @export
# use_idd {{{
use_idd <- function (idd, download = FALSE) {
    assert_that(is_scalar(idd))

    if (is_eplus_ver(idd)) {
        ver <- standerize_ver(idd)

        # if found in cache, return it directly
        if (is_parsed_idd_ver(ver)) return(.globals$idd[[as.character(ver)]])

        message("Idd v", ver, " has not been parsed before. Try to locate ",
            "`Energy+.idd` in EnergyPlus installation folder.")

        # stop if corresponding version of EnergyPlus is not found
        if (!is_avail_eplus(ver))
            stop("Failed to locate `Energy+.idd` because EnergyPlus v",
                ver, "is not available.", call. = FALSE)

        config <- eplus_config(ver)
        path_idd <- normalizePath(file.path(config$dir, "Energy+.idd"), mustWork = FALSE)

        # stop if "Energy+.idd" file is not found in EnergyPlus location
        if (!file.exists(path_idd))
            stop("`Energy+.idd` file does not exist in EnergyPlus v",
                config$version, " installation folder ",
                backtick(config$dir), ".", call. = FALSE)

        message("`Energy+.idd` file found. Start parsing...")
        # read IDD file and store in cache
        res <- read_idd(path_idd)
    } else {
        message("Start parsing...")

        res <- read_idd(idd)
    }

    res
}
# }}}

# [.Idd {{{
#' @export
'[.Idd' <- function(x, i, j, ..., drop = FALSE) {
    m_obj <- .subset2(x, "object")()
    .subset2(m_obj, i)
}
# }}}

#' Download EnergyPlus Input Data Dictionary (IDD) File
#'
#' `download_idd` downloads EnergyPlus Input Data Dictionary (IDD) file from
#' [EnergyPlus GitHub Repository](https://github.com/NREL/EnergyPlus). It is
#' useful in case where you only want to edit an EnergyPlus Input Data File
#' (IDF) directly but donnot want to install whole EnergyPlus software.
#'
#' @param ver The EnergyPlus version number, e.g., 8.7, "8.7" or "8.7.0". The
#'     special value `"latest"` means the latest version (fetched from GitHub
#'     releases).
#' @param dir A directory to indicate where to save the IDD file. Default:
#'     current working directory.
#' @return A full path of the downloaded IDD file.
#' @seealso [use_idd()]
#' @importFrom gh gh
#' @importFrom purrr map_chr
#' @importFrom stringr str_detect
#' @importFrom readr read_lines write_lines
#' @export
#' @examples
#' \donotrun{
#' download_idd()
#' download_idd(8.8)
#' }
# download_idd {{{
download_idd <- function (ver = "latest", dir = getwd()) {
    latest <- repo_releases("NREL", "EnergyPlus", "latest")
    latest_ver <- as.numeric_version(attr(latest, "version"))

    rels <- gh::gh("GET /repos/:owner/:repo/contents/:path", repo = "EnergyPlus",
        owner = "NREL", path = "idd", ref = paste0("v", latest_ver))

    if (ver == "latest") ver <- latest_ver
    assert_that(is_eplus_ver(ver))
    ver <- standerize_ver(as.character(ver))

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

    message("EnergyPlus IDD file ", paste0("v", ver), " has been downloaded
        successfully into ", dir, ".")

    dest
}
# }}}
