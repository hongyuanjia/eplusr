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
#' file, parses it and returns a `RddFile` object. `read_mdd()` takes a file
#' path of EnergyPlus Meter Data Dictionary (MDD) file, parses it and returns a
#' `MddFile` object.
#'
#' Basically, a `RddFile` and `MddFile` object is a
#' [data.table][data.table::data.table()] with 5 columns and 3 additional
#' attributes:
#'
#' 5 Columns:
#'
#' *`index`: Integer. Index of each variable.
#' * `reported_time_step`: Character. Reported time step for the variables.
#'   Possible value: `Zone` and `HVAC`.
#' * `report_type`: Character. Report types. Possible value: `Average`, `Sum`
#'   and `Meter`. Note that `Meter` is only for MDD file. All variables will
#'   have `report_type` being `Meter`.
#' * `variable`: Character. Report variable names.
#' * `units`: Character. Units of reported values. `NA` if report values do not
#'   have units.
#'
#' 3 Attributes:
#'
#' * `eplus_version`: A [numeric_version][base::numeric_version()] object. The
#'   version of EnergyPlus used during the simulation.
#' * `eplus_build`: A single string. The build tag of EnergyPlus used during the
#'   simulation.
#' * `datetime`: A DateTime (POSIXct). The time when the simulation started.
#'
#' @param path For `read_rdd()`, a file path of EnergyPlus EnergyPlus Report Data
#' Dictionary file with an extension `.rdd`. For `read_mdd()`, a file path of
#' EnergyPlus EnergyPlus Meter Data Dictionary file with an extension `.mdd`
#' @return For `read_rdd()`, an `RddFile` object. For `read_mdd()`, a `MddFile`
#' object.
#' @export
#' @examples
#' \dontrun{
#' # run simulation and get the err file
#' idf_name <- "1ZoneUncontrolled.idf"
#' epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#' idf_path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", idf_name)
#' epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData", epw_name)
#' job <- eplus_job(idf_path, epw_path)
#' job$run(dir = tempdir())
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
    parse_rdd_file(path)[]
}
# }}}

#' @rdname rdd
#' @export
# read_mdd {{{
read_mdd <- function (path) {
    assert(has_ext(path, "mdd"))
    parse_rdd_file(path, mdd = TRUE)[]
}
# }}}

# parse_rdd_file {{{
parse_rdd_file <- function (path, mdd = FALSE) {
    rdd <- data.table(index = integer(), reported_time_step = character(),
        report_type = character(), variable = character(), units = character()
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

    if (!stri_startswith_fixed(header$string, "Program Version") &&
        !stri_startswith_fixed(header$string, "! Program Version")) {
        type <- if (mdd) "mdd" else "rdd"
        abort(paste0("error_invalid_", type),
            paste0("Input file is not a valid EnergyPlus ", stri_trans_toupper(type), " file.")
        )
    }

    rdd_head <- stri_split_fixed(header$string, ",")[[1L]]

    idf_fmt <- stri_startswith_fixed(rdd_head[[1L]], "!")

    # EnergyPlus eplus_version and eplus_build
    ver_bld <- stri_match_first_regex(rdd_head[3L], "Version (\\d\\.\\d\\.\\d)-([0-9a-z]{10})")
    if (!is.na(ver_bld[, 2L])) eplus_version <- standardize_ver(ver_bld[, 2L])
    if (!is.na(ver_bld[, 3L])) eplus_build <- ver_bld[, 3L]

    # simulation date time
    d <- stri_match_first_regex(rdd_head[4L], "YMD=(\\d{4}\\.\\d{2}\\.\\d{2} \\d{2}:\\d{2})")[, 2L]
    if(!is.na(d)) datetime <- lubridate::ymd_hm(d, tz = Sys.timezone())

    if (idf_fmt) {
        drop_num <- if (mdd) 1L else 1L:2L
        rdd <- tryCatch(
            fread(path, skip = 2, sep = ",", header = FALSE, drop = drop_num,
                col.names = c("variable", "step_type_units")),
            error = function (e) {
                e$message <- paste0("Failed to read ", if (mdd) "MDD" else "RDD", " data.\n", e$message)
                stop(e, call. = FALSE)
            }
        )

        if (mdd) {
            rdd <- unique(rdd)
            set(rdd, NULL, "units", stri_split_fixed(rdd$step_type_units, " [", n = 2, simplify = TRUE)[, 2L])
            set(rdd, NULL, c("reported_time_step", "report_type"), list("Zone", "Meter"))
            set(rdd, NULL, "step_type_units", NULL)
        } else {
            set(rdd, NULL, c("reported_time_step", "type_units"),
                as.data.table(
                    stri_split_fixed(
                        stri_split_fixed(rdd$step_type_units, "!- ", n = 2, simplify = TRUE)[, 2L],
                        " ", n = 2, simplify = TRUE
                    )
                )
            )
            set(rdd, NULL, c("report_type", "units"), as.data.table(stri_split_fixed(rdd$type_units, " [", n = 2, simplify = TRUE)))
            set(rdd, NULL, c("step_type_units", "type_units"), NULL)
        }
    } else {
        rdd <- tryCatch(
            fread(path, skip = 1, sep = ",", header = TRUE, col.names = c("reported_time_step", "report_type", "variable_unit")),
            error = function (e) {e$message <- paste0("Failed to read RDD/MDD data.\n", e$message); stop(e)}
        )
        set(rdd, NULL, c("variable", "units"), as.data.table(stri_split_fixed(rdd$variable_unit, " [", n = 2, simplify = TRUE)))
        set(rdd, NULL, "variable_unit", NULL)
    }

    # add index
    rdd[, index := .I]

    set(rdd, NULL, "units", stri_sub(rdd$units, to = -2L))
    rdd[stri_isempty(units), `:=`(units = NA_character_)]
    setcolorder(rdd, c("index", "reported_time_step", "report_type", "variable", "units"))

    setattr(rdd, "eplus_version", eplus_version)
    setattr(rdd, "eplus_build", eplus_build)
    setattr(rdd, "datetime", datetime)
    cls <- if (mdd) "MddFile" else "RddFile"
    setattr(rdd, "class", c(cls, class(rdd)))

    rdd
}
# }}}

#' Format RddFile Object to Standard Input for `Idf$load()` Method
#'
#' `rdd_to_load()` and `mdd_to_load()` takes a `RddFile` and `MddFile` object
#' respectively and format it into a [data.table][data.table::data.table()] in
#' acceptable format for `$load()` method in [Idf] class.
#'
#' @param rdd,mdd A `RddFile` object created using [read_rdd()] and a `MddFile`
#' object created using [read_mdd()], respectively.
#' @param key_value Key value name for **all** variables. If not specified and
#' the `key_value` column in the input `RddFile` object will be used. If
#' `key_value` column does not exist, `"*"` are used for all variables.
#' @param reporting_frequency Variable value reporting frequency for **all**
#' variables. If not specified and the `reporting_freqency` column in the input
#' `RddFile` object will be used. If `reporting_freqency` column does not exist,
#' `"Timestep"` are used for all variables. All possible values: `"Detailed"`,
#' `"Timestep"`, `"Hourly"`, `"Daily"`, `"Monthly"`, `"RunPeriod"`,
#' `"Environment"`, and `"Annual"`.
#' @param class Class name for meter output. All possible values:
#' `"Output:Meter"`, `"Output:Meter:MeterFileOnly"`,
#' `"Output:Meter:Cumulative"`, and `"Output:Meter:Cumulative:MeterFileOnly"`.
#' Default: `"Output:Meter"`.
#' @return
#' A [data.table][data.table::data.table()] with 5 columns with an additional
#' attribute named `eplus_version` extracted from the original `RddFile` and
#' `MddFile`:
#'
#' * `id`: Integer type. Used to distinguish each object definition.
#' * `class`: Character type. Class names, e.g. `Output:Variable` and
#'   `Output:Meter`.
#' * `index`: Integer type. Field indices.
#' * `field`: Character type. Field names.
#' * `value`: Character type. The value of each field to be added.
#'
#' @rdname rdd_to_load
#' @export
#' @examples
#' \dontrun{
#' # read an example distributed with eplusr
#' path_idf <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
#' idf <- read_idf(path_idf)
#'
#' # run Design-Day-Only simulation silently
#' job <- idf$run(NULL, tempdir(), echo = FALSE)
#'
#' # read RDD and MDD
#' rdd <- job$read_rdd()
#' mdd <- job$read_mdd()
#'
#' # perform subsetting on the variables
#' # e.g.:
#' rdd_sub <- rdd[grepl("Site", variable)]
#' mdd_sub <- mdd[grepl("Electricity", variable)]
#'
#' # use newly added helper `rdd_to_load()` and `mdd_to_load()` and `$load()` to
#' # add `Output:Variable` and `Output:Meter*`
#' idf$load(rdd_to_load(rdd_sub))
#' idf$load(mdd_to_load(mdd_sub))
#'
#' # default `Key Value` is `"*"` and `Reporting Frequency` is `Timestep`
#' # can overwrite using `key_value` and `reporting_freqency` arg
#' rdd_to_load(rdd_sub, key_value = "Environment", reporting_frequency = "hourly")
#'
#' # if input has column `key_value`, default is to use it, unless `key_value` is
#' # explicitly specified
#' rdd_to_load(rdd_sub[, key_value := "Environment"])
#' rdd_to_load(rdd_sub[, key_value := "Environment"], key_value = "*")
#'
#' # `reporting_frequency` arg works in the same way as `key_value` arg, i.e.:
#' # if input has column `reporting_frequency`, use it, unless
#' # `reporting_frequency` is explicitly specified
#' rdd_to_load(rdd_sub[,  reporting_frequency := "monthly"])
#' rdd_to_load(rdd_sub[,  reporting_frequency := "monthly"], reporting_frequency = "detailed")
#'
#' # if input has column `key_value`, default is to use it, unless `key_value` is
#' # explicitly specified
#' rdd_to_load(rdd_sub[, key_value := "Environment"])
#' rdd_to_load(rdd_sub[, key_value := "Environment"], key_value = "*")
#'
#' # meter class can be further specified using `class` arg
#' mdd_to_load(mdd_sub, class = "Output:Meter:MeterFileOnly")
#' }
#' @export
# rdd_to_load {{{
rdd_to_load <- function (rdd, key_value, reporting_frequency) {
    assert(is_rdd(rdd))

    # copy the original
    rdd <- copy(rdd)
    ver <- attr(rdd, "eplus_version")

    # update index
    rdd[, index := .I]

    set(rdd, NULL, "class", "Output:Variable")

    if (!missing(key_value)) {
        assert(is_string(key_value))
        set(rdd, NULL, "key_value", key_value)
    } else if (!has_name(rdd, "key_value")) {
        set(rdd, NULL, "key_value", "*")
    } else {
        set(rdd, NULL, "key_value", as.character(rdd$key_value))
    }

    if (!missing(reporting_frequency)) {
        rep_freq <- validate_report_freq(reporting_frequency)
        set(rdd, NULL, "reporting_frequency", rep_freq)
    } else if (!has_name(rdd, "reporting_frequency")) {
        set(rdd, NULL, "reporting_frequency", "Timestep")
    } else {
        set(rdd, NULL, "reporting_frequency",
            validate_report_freq(as.character(rdd$reporting_frequency), scalar = FALSE)
        )
    }

    setnames(rdd, c("index", "key_value", "variable", "reporting_frequency"),
        c("id", "Key Value", "Variable Name", "Reporting Frequency")
    )

    rdd <- data.table::melt.data.table(rdd, id.vars = c("id", "class"),
        measure.vars = c("Key Value", "Variable Name", "Reporting Frequency"),
        variable.name = "field", variable.factor = FALSE
    )
    set(rdd, NULL, "index", rowidv(rdd, "id"))
    setorderv(rdd, c("id", "index"))
    setcolorder(rdd, c("id", "class", "index", "field", "value"))
    setattr(rdd, "eplus_version", ver)
    rdd[]
}
# }}}

#' @rdname rdd_to_load
#' @export
# mdd_to_load {{{
mdd_to_load <- function (mdd, reporting_frequency, class = c("Output:Meter",
                                                             "Output:Meter:MeterFileOnly",
                                                             "Output:Meter:Cumulative",
                                                             "Output:Meter:Cumulative:MeterFileOnly")) {
    assert(is_mdd(mdd))
    ver <- attr(mdd, "eplus_version")
    class <- match.arg(class)

    # copy the original
    mdd <- copy(mdd)

    # update index
    mdd[, index := .I]

    set(mdd, NULL, "class", class)

    if (!missing(reporting_frequency)) {
        rep_freq <- validate_report_freq(reporting_frequency)
        set(mdd, NULL, "reporting_frequency", rep_freq)
    } else if (!has_name(mdd, "reporting_frequency")) {
        set(mdd, NULL, "reporting_frequency", "Timestep")
    } else {
        set(mdd, NULL, "reporting_frequency",
            validate_report_freq(as.character(mdd$reporting_frequency), scalar = FALSE)
        )
    }

    setnames(mdd, c("index", "variable", "reporting_frequency"),
        c("id", "Key Name", "Reporting Frequency")
    )

    mdd <- data.table::melt.data.table(mdd, id.vars = c("id", "class"),
        measure.vars = c("Key Name", "Reporting Frequency"),
        variable.name = "field", variable.factor = FALSE
    )
    set(mdd, NULL, "index", rowidv(mdd, "id"))
    setorderv(mdd, c("id", "index"))
    setcolorder(mdd, c("id", "class", "index", "field", "value"))
    setattr(mdd, "eplus_version", ver)
    mdd[]
}
# }}}

# validate_report_freq {{{
validate_report_freq <- function (reporting_frequency, scalar = TRUE) {
    if (scalar) assert(is_string(reporting_frequency))

    all_freq <- c("Detailed", "Timestep", "Hourly", "Daily", "Monthly",
          "RunPeriod", "Environment", "Annual")

    freq <- match_in_vec(reporting_frequency, all_freq, label = TRUE)

    assert(no_na(freq),
        msg = paste0("Invalid reporting frequency found: ",
            collapse(unique(reporting_frequency[is.na(freq)])), ". All possible values: ",
            collapse(all_freq), "."
        ),
        err_type = "error_invalid_reporting_frequency"
    )

    freq
}
# }}}

#' @export
# print.RddFile {{{
print.RddFile <- function (x, ...) {
    cli::cat_rule("EnergyPlus Report Data Dictionary File", line = 2)

    if (!is.null(attr(x, "eplus_build")) && !is.na(attr(x, "eplus_build"))) {
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

    if (!is.null(attr(x, "eplus_build")) && !is.na(attr(x, "eplus_build"))) {
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
