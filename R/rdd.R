#' @importFrom data.table data.table set setattr
#' @importFrom stringi stri_match_first_regex stri_split_fixed
#' @importFrom stringi stri_startswith_fixed stri_sub
#' @importFrom lubridate ymd_hm
#' @importFrom cli cat_line cat_rule
#' @include utils.R
NULL

#' Read an EnergyPlus Report Data Dictionary File
#'
#' `read_rdd()` takes a file path of EnergyPlus Report Data Dictionary (RDD)
#' file, parses it and returns a `RddFile` object.
#'
#' `read_mdd()` takes a file path of EnergyPlus Meter Data Dictionary (MDD)
#' file, parses it and returns a `MddFile` object.
#'
#' Basically, a `RddFile` and `MddFile` object is a
#' [data.table][data.table::data.table()] with 3 additional attributes:
#'
#' * `eplus_version`: A [numeric_version][base::numeric_version()] object. The
#'   version of EnergyPlus used during the simulation.
#' * `eplus_build`: A single string. The build tag of EnergyPlus used during the
#'   simulation.
#' * `datetime`: A DateTime (POSIXct). The time when the simulation started.
#'
#' @param path a file path of EnergyPlus EnergyPlus Report Data Dictionary file,
#' usually with an extension `.rdd`.
#' @return An `RddFile` object.
#' @export
#' @examples
#' \dontrun{
#' # run simulation and get the err file
#' idf_name <- "1ZoneUncontrolled.idf"
#' epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#' idf_path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", idf_name)
#' epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData", epw_name)
#' job <- eplus_job(idf_path, epw_path)
#'
#' # read the err file
#' read_rdd(job$locate_output(".rdd"))
#' read_mdd(job$locate_output(".mdd"))
#' }
#' @rdname rdd
#' @author Hongyuan Jia
# read_rdd {{{
read_rdd <- function (path) {
    assert(has_ext(path, "rdd"))
    parse_rdd_file(path)
}
# }}}

#' @rdname rdd
#' @param path a file path of EnergyPlus EnergyPlus Meter Data Dictionary file,
#' usually with an extension `.mdd`.
#' @return An `MddFile` object.
#' @export
# read_mdd {{{
read_mdd <- function (path) {
    assert(has_ext(path, "mdd"))
    parse_mdd_file(path)
}
# }}}

# parse_rdd_file {{{
parse_rdd_file <- function (path) {
    rdd <- data.table(reported_time_step = character(), report_type = character(),
        variable = character(), units = character()
    )

    eplus_version <- numeric_version(NA, strict = FALSE)
    eplus_build <- NA_character_
    datetime <- as.POSIXct(NA)

    if (!file.exists(path)) {
        setattr(rdd, "eplus_version", eplus_version)
        setattr(rdd, "eplus_build", eplus_build)
        setattr(rdd, "datetime", datetime)
        setattr(rdd, "class", c("RddFile", class(rdd)))
        return(rdd)
    }

    # read first line
    header <- read_lines(path, nrows = 1)

    if (stri_startswith_fixed(header$string, "Program Version")) {
        rdd_head <- stri_split_fixed(header$string, ",")[[1L]]

        # EnergyPlus eplus_version and eplus_build
        ver_bld <- stri_match_first_regex(rdd_head[3L], "Version (\\d\\.\\d\\.\\d)-([0-9a-z]{10})")
        if (!is.na(ver_bld[, 2L])) eplus_version <- standardize_ver(ver_bld[, 2L])
        if (!is.na(ver_bld[, 3L])) eplus_build <- ver_bld[, 3L]

        # simulation date time
        d <- stri_match_first_regex(rdd_head[4L], "YMD=(\\d{4}\\.\\d{2}\\.\\d{2} \\d{2}:\\d{2})")[, 2L]
        if(!is.na(d)) datetime <- lubridate::ymd_hm(d, tz = Sys.timezone())
    }

    rdd <- tryCatch(
        fread(path, skip = 1, sep = ",", header = TRUE, col.names = c("reported_time_step", "report_type", "variable_unit")),
        error = function (e) {e$message <- paste0("Failed to read RDD/MDD data.\n", e$message); stop(e)}
    )

    set(rdd, NULL, c("variable", "units"), as.data.table(stri_split_fixed(rdd$variable_unit, "[", n = 2, simplify = TRUE)))
    set(rdd, NULL, "variable_unit", NULL)
    set(rdd, NULL, "units", stri_sub(rdd$units, to = -2L))
    rdd[stri_isempty(units), `:=`(units = NA_character_)]

    setattr(rdd, "eplus_version", eplus_version)
    setattr(rdd, "eplus_build", eplus_build)
    setattr(rdd, "datetime", datetime)
    setattr(rdd, "class", c("RddFile", class(rdd)))

    rdd[]
}
# }}}

# parse_mdd_file {{{
parse_mdd_file <- function (path) {
    mdd <- parse_rdd_file(path)
    setattr(mdd, "class", c("MddFile", class(mdd)))
    mdd[]
}
# }}}

#' @export
# print.RddFile {{{
print.RddFile <- function (x, ...) {
    cli::cat_rule("EnergyPlus Report Data Dictionary File", line = 2)

    if (!is.na(attr(x, "eplus_build"))) {
        cli::cat_line(paste0("  * EnergyPlus version: ", attr(x, "eplus_version"), " (", attr(x, "eplus_build"), ")"))
    } else {
        cli::cat_line(paste0("  * EnergyPlus version: ", attr(x, "eplus_version")))
    }
    cli::cat_line(paste0("  * Simulation started: ", attr(x, "datetime")))

    cli::cat_line()
    cli::cat_rule("Details")

    NextMethod()
    invisible(x)
}
# }}}

#' @export
# print.MddFile {{{
print.MddFile <- function (x, ...) {
    cli::cat_rule("EnergyPlus Meter Data Dictionary File", line = 2)

    if (!is.na(attr(x, "eplus_build"))) {
        cli::cat_line(paste0("  * EnergyPlus version: ", attr(x, "eplus_version"), " (", attr(x, "eplus_build"), ")"))
    } else {
        cli::cat_line(paste0("  * EnergyPlus version: ", attr(x, "eplus_version")))
    }
    cli::cat_line(paste0("  * Simulation started: ", attr(x, "datetime")))

    cli::cat_line()
    cli::cat_rule("Details")

    NextMethod()
    invisible(x)
}
# }}}
