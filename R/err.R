#' @importFrom data.table data.table set setcolorder setattr
#' @importFrom stringi stri_match_first_regex stri_detect_fixed stri_detect_regex
#' @importFrom stringi stri_endswith_fixed stri_split_fixed stri_startswith_fixed
#' @importFrom stringi stri_sub stri_wrap
#' @importFrom cli cat_line cat_rule rule
NULL

#' Read an EnergyPlus Simulation Error File
#'
#' `read_err()` takes a file path of EnergyPlus simulation error file, usually
#' with an extension `.err`, parses it and returns an `ErrFile` object.
#'
#' Basically, an `ErrFile` object is a list with 7 elements:
#'
#' * `eplus_version`: A [numeric_version][base::numeric_version()] object. The
#'   version of EnergyPlus used during the simulation.
#' * `eplus_build`: A single string. The build tag of EnergyPlus used during the
#'   simulation.
#' * `datetime`: A DateTime (POSIXct). The time when the simulation started.
#' * `idd_version`: A [numeric_version][base::numeric_version()]. The version of
#'   IDD used during the simulation.
#' * `successful`: `TRUE` when the simulation ended successfully, and `FALSE`
#'   otherwise.
#' * `terminated`: `TRUE` when the simulation was terminated, and `FALSE`
#'   otherwise.
#' * `data`: A [data.table][data.table::data.table()] that contains parsed error
#'   messages.
#'
#' @param path a file path of EnergyPlus simulation error file, usually
#' with an extension `.err`.
#' @return An `ErrFile` object.
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
#' read_err(job$locate_output(".err"))
#' }
# read_err {{{
read_err <- function (path) {
    parse_err_file(path)
}
# }}}

# parse_err_file {{{
parse_err_file <- function (path) {
    res <- list(
        eplus_version = numeric_version(NA, strict = FALSE), eplus_build = NA_character_,
        datetime = as.POSIXct(NA), idd_version = NA,
        successful = FALSE, terminated = FALSE, data = data.table()
    )
    setattr(res, "class", "ErrFile")

    if (!file.exists(path)) return(res)

    # read err file
    err_dt <- read_lines(path, trim = TRUE)

    if (!nrow(err_dt)) return(res)

    # parse header line
    if (err_dt[1L, stri_detect_fixed(string, "Program Version")]) {
        err_head <- stri_split_fixed(err_dt$string[[1L]], ",")[[1L]]

        # EnergyPlus version and build
        ver_bld <- stri_match_first_regex(err_head[3L], "Version (\\d\\.\\d\\.\\d)-([0-9a-z]{10})")
        if (!is.na(ver_bld[, 2L])) res$eplus_version <- standardize_ver(ver_bld[, 2L])
        if (!is.na(ver_bld[, 3L])) res$eplus_build <- ver_bld[, 3L]

        # simulation date time
        d <- stri_match_first_regex(err_head[4L], "YMD=(\\d{4}\\.\\d{2}\\.\\d{2} \\d{2}:\\d{2})")[, 2L]
        if(!is.na(d)) res$datetime <- lubridate::ymd_hm(d, tz = Sys.timezone())

        # IDD version
        v <- stri_match_first_regex(err_head[5L], "IDD_Version (\\d\\.\\d\\.\\d)")[, 2L]
        if (!is.na(v)) res$idd_version <- standardize_ver(v)

        # exclude header line
        err_dt <- err_dt[-1L]
    }

    # extract message
    # first need to check if string starts with "**", in case of style below
    # occur:
    # **  Fatal  ** IP:
    # ...Summary ...
    # ..... Reference ...
    # ..... Last severe ...
    err_dt[stri_startswith_fixed(string, "**"),
        `:=`(prefix = stri_sub(string, to = 13L), message = stri_sub(string, 15L))
    ]
    err_dt[is.na(prefix), `:=`(prefix = "**   ~~~   **", message = string)]

    # exclude empty lines
    err_dt <- err_dt[message != ""]

    # check if simulation complete successfully
    if (nrow(err_dt[stri_detect_regex(message, "^(EnergyPlus|(GroupdTempCalc\\S*)) Completed Successfully")])) {
        res$successful <- TRUE
    } else if (nrow(err_dt[stri_startswith_fixed(message, "EnergyPlus Terminated")])) {
        res$terminated <- TRUE
    }

    # exclude unuseful lines
    l_end <- err_dt[stri_detect_regex(message, "(Final|(EnergyPlus Warmup|Sizing)) Error Summary"), which = TRUE]
    if (length(l_end)) err_dt <- err_dt[-(min(l_end):.N)]

    # extract environment
    err_dt[stri_startswith_fixed(message, "Beginning"), `:=`(environment = stri_sub(message, 11L))]

    # set environment index
    err_dt[!is.na(environment), environment_index := .I]

    # fill downwards
    err_dt[, `:=`(environment_index = environment_index[1L], environment = environment[1L]),
        by = list(cumsum(!is.na(environment_index)))]

    # for those simulation without sizing and etc.
    err_dt[is.na(environment_index), `:=`(environment = "Simulation Initiation", environment_index = 0L)]

    # exclude environment lines
    err_dt <- err_dt[!line %in% err_dt[environment_index > 0L, line[1L], by = c("environment_index")]$V1]

    # make sure environment index starts from 1L
    if (!min(err_dt$environment_index)) set(err_dt, NULL, "environment_index", err_dt$environment_index + 1L)

    # extract log level
    err_dt[, level := stri_match_first_regex(prefix, "[^~\\s\\*]+")[, 1L]]

    # different message types {{{
    # separate message
    # (a) separate by level
    # ** Warning ** Weather file ...
    # **   ~~~   ** ..Location object ...
    # **   ~~~   ** ..Weather File ...
    # **   ~~~   ** ..due to ...
    # **   ~~~   ** ..Time Zone ...
    # ** Warning ** GetSurfaceData ...
    #
    # (b) separate by 13 stars
    # ************* SizeLowTempRadiantSystem ...
    # **   ~~~   ** User-Specified ...
    # **   ~~~   ** differs from Design ...
    # **   ~~~   ** This may, or may not ...
    # **   ~~~   ** Verify that the value ...
    # ************* SizeLowTempRadiantSystem ...
    # **   ~~~   ** User-Specified Maximum ...
    #
    # (c) continuous messages
    #     message starts with a class name should be treated as part of previous
    #     message
    #     This type is hard to parse correctly, as it is difficult to determine
    #     the ending of the message. Here only the first message is used
    # ** Warning ** The following schedule ...
    # **   ~~~   **  file but are never ...
    # ************* Schedule:Year or ...
    # ************* Schedule:Year or ...
    # ************* Schedule:Year or ...
    #
    # (d) continuous messages
    #     message starts with ".." should be treated as part of previous message
    # ************* The following error ...
    # ************* Nominally Unused Constructions
    # ************* ..The nominally unused ...
    # ************* ..extra time during ...
    # ************* ..object. You may ...
    # }}}

    l <- err_dt[stri_startswith_fixed(message, "The following") |
                stri_endswith_fixed(message, "is shown.") |
                stri_startswith_fixed(message, ".."), which = TRUE
    ]
    if (length(l)) set(err_dt, l, "prefix", "**   ~~~   **")

    # mark normal informative message and number all messages
    err_dt[!is.na(level) | prefix == stringi::stri_dup("*", 13L), index := .I]
    err_dt[!is.na(index) & prefix == stringi::stri_dup("*", 13L), level := "Info"]
    err_dt[!is.na(level), index := .I]

    # number messages of different level
    err_dt[!is.na(index), level_index := seq_len(.N), by = c("level")]
    err_dt[, index := index[1L], by = list(cumsum(!is.na(index)))]
    err_dt[, `:=`(level = level[1L], level_index = level_index[1L]),
        by = list(cumsum(!is.na(level_index)))]

    set(err_dt, NULL, c("string", "line", "prefix"), NULL)
    setcolorder(err_dt, c("index", "environment_index", "environment", "level_index", "level", "message"))

    res$data <- err_dt

    res
}
# }}}

#' Print EnergyPlus Error File
#'
#' `ErrFile` is mainly used to extract and print data in an EnergyPlus Error
#' File (`.err`).
#'
#' @param x An `ErrFile` created using [read_err()].
#' @param brief If `TRUE`, only summary data is printed. Default: `FALSE`.
#' @param info If `FALSE`, informative messages are excluded. Only warnings and
#' errors are printed. Default: `TRUE`.
#' @param ... Further arguments passed to or from other methods.
#' @return An `ErrFile` object, invisibly.
#' @export
# print.ErrFile {{{
print.ErrFile <- function (x, brief = FALSE, info = TRUE, ...) {
    cli::cat_rule("EnergyPlus Error File", line = 2)

    if (!is.na(x$eplus_build)) {
        cli::cat_line(paste0("  * EnergyPlus version: ", x$eplus_version, " (", x$eplus_build, ")"))
    } else {
        cli::cat_line(paste0("  * EnergyPlus version: ", x$eplus_version))
    }

    cli::cat_line(paste0("  * Simulation started: ", x$datetime))
    cli::cat_line(paste0("  * Terminated: ", x$terminated))
    cli::cat_line(paste0("  * Successful: ", x$successful))

    if (info) {
        dt <- x$data
    } else {
        dt <- x$data[level != "Info"]
    }

    if (!nrow(dt)) {
        cat("\n  EnergyPlus terminated without any error message generated.\n", sep = "")
        return(invisible(x))
    }

    # error summary
    num_sum <- dt[level != "Info", list(num = max(level_index)), by = c("level")]
    set(num_sum, NULL, "level", factor(num_sum$level, c("Warning", "Severe", "Fatal"), ordered = TRUE))
    err_sm <- num_sum[order(level), paste0("  * ",rpad(paste0(as.character(level), "[", stri_sub(as.character(level), to = 1L, ), "]: "), " "), num)]
    cli::cat_line(err_sm)

    if (brief) return(invisible(x))

    cli::cat_line()
    # add row id
    set(dt, NULL, "id", seq_len(nrow(dt)))
    # line wrap long message and extend 8 spaces
    set(dt, NULL, "out",
        vapply(stri_wrap(dt$message, exdent = 8L, simplify = FALSE),
            paste0, character(1L), collapse = "\n"
        )
    )
    # exdent 8 spaces for multiple-line messages
    dt[dt[, .I[-1L], by = c("index")]$V1, out := paste0(stringi::stri_dup(" ", 8L), out)]

    # get all total message number in a level
    dt[, level_num := max(level_index), by = "level"]
    # add "[W 1/n]" prefix at the beginning
    dt[dt[, .I[1L], by = c("level", "level_index")]$V1,
       out := paste0("[", stri_sub(level, to = 1L), " ", level_index, "/", level_num, "] ", out)
    ]

    # add environment name heading
    dt[dt[, .I[1L], by = c("environment_index")]$V1, out := paste0(rule(paste0("During ", environment)), "\n", out), by = "id"]
    # separate different environment
    dt[dt[, .I[1L], by = c("environment_index")]$V1[-1L], out := paste0("\n", out)]

    cli::cat_line(dt$out)

    # clean
    if (info) set(dt, NULL, c("id", "out", "level_num"), NULL)
    return(invisible(x))
}
# }}}
