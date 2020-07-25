#' @importFrom data.table data.table set setcolorder setattr
#' @importFrom stringi stri_match_first_regex stri_detect_fixed stri_detect_regex
#' @importFrom stringi stri_endswith_fixed stri_split_fixed stri_startswith_fixed
#' @importFrom stringi stri_sub stri_wrap
#' @importFrom lubridate ymd_hm
#' @importFrom cli cat_line cat_rule rule
NULL

#' Read an EnergyPlus Simulation Error File
#'
#' `read_err()` takes a file path of EnergyPlus simulation error file, usually
#' with an extension `.err`, parses it and returns an `ErrFile` object.
#'
#' Basically, an `ErrFile` object is a [data.table][data.table::data.table()]
#' with 6 columns and 6 additional attributes:
#'
#' 6 Columns:
#'
#' * `index`: Integer. Index of messages.
#' * `envir_index`: Integer. Index of simulation environments.
#' * `envir`: Character. Names of simulation environments.
#' * `level_index`: Integer. Index for each severe level.
#' * `level`: Character. Name of severe levels. Possible values: `Info`,
#'   `Warning`, `Severe`, and etc.
#' * `message`: Character. Error messages.
#'
#' 6 Attributes:
#'
#' * `path`: A single string. The path of input file.
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
#' job$run(dir = tempdir())
#'
#' # read the err file
#' read_err(job$locate_output(".err"))
#' }
#' @importFrom checkmate assert_file
# read_err {{{
read_err <- function (path) {
    checkmate::assert_file(path, extension = c("err", "vcperr"))
    parse_err_file(path)
}
# }}}

# parse_err_file {{{
parse_err_file <- function (path) {
    # data
    err <- data.table(index = integer(), envir_index = integer(),
        envir = character(), level_index = integer(), level = character(),
        message = character()
    )

    is_trans <- tolower(tools::file_ext(path)) == "vcperr"

    # attributes
    if (is_trans) {
        cls <- c("TransitionErrFile", "ErrFile", class(data.table()))

        att <- list(
            path = normalizePath(path, mustWork = FALSE),
            from = numeric_version(NA, strict = FALSE),
            to = numeric_version(NA, strict = FALSE),
            successful = FALSE
        )
    } else {
        cls <- c("ErrFile", class(data.table()))

        att <- list(
            path = normalizePath(path, mustWork = FALSE),
            eplus_version = numeric_version(NA, strict = FALSE), eplus_build = NA_character_,
            datetime = as.POSIXct(NA), idd_version = NA,
            successful = FALSE, terminated = FALSE
        )
    }

    # return empty err
    if (!file.exists(path)) {
        for (i in names(att)) setattr(err, i, att[[i]])
        setattr(err, "class", cls)
        return(err)
    }

    # read err file
    err_dt <- read_lines(path, trim = TRUE)

    # return empty err
    if (!nrow(err_dt)) {
        for (i in names(att)) setattr(err, i, att[[i]])
        setattr(err, "class", cls)
        return(err)
    }

    # parse header line
    if (err_dt[1L, stri_detect_fixed(string, "Program Version")]) {
        err_head <- stri_split_fixed(err_dt$string[[1L]], ",")[[1L]]

        # EnergyPlus {{{
        if (!is_trans) {
            # EnergyPlus version and build
            ver_bld <- stri_match_first_regex(err_head[3L], "Version (\\d\\.\\d\\.\\d)-([0-9a-z]{10})")
            if (!is.na(ver_bld[, 2L])) att$eplus_version <- standardize_ver(ver_bld[, 2L])
            if (!is.na(ver_bld[, 3L])) att$eplus_build <- ver_bld[, 3L]

            # simulation date time
            d <- stri_match_first_regex(err_head[4L], "YMD=(\\d{4}\\.\\d{2}\\.\\d{2} \\d{2}:\\d{2})")[, 2L]
            if(!is.na(d)) att$datetime <- lubridate::ymd_hm(d, tz = Sys.timezone())

            # IDD version
            v <- stri_match_first_regex(err_head[5L], "IDD_Version (\\d\\.\\d\\.\\d)")[, 2L]
            if (!is.na(v)) att$idd_version <- standardize_ver(v)
        # }}}
        # IDFVersionUpdater {{{
        } else {
            trans_ver <- stri_match_first_regex(err_head[2L], "Conversion (\\d\\.\\d) => (\\d\\.\\d)")
            if (!is.na(trans_ver[, 2L])) att$from <- standardize_ver(trans_ver[, 2L])
            if (!is.na(trans_ver[, 3L])) att$to <- standardize_ver(trans_ver[, 3L])
            att$successful <- stri_detect_fixed(err_dt[.N, string], "Conversion Completed Successfully")
            err_dt <- err_dt[-.N]
        }
        # }}}

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
        `:=`(prefix = stri_sub(string, to = 13L), message = stri_sub(string, 15L)
        )
    ]
    # recurring error messages
    err_dt[stri_startswith_fixed(message, " **"),
        `:=`(prefix = stri_sub(message, to = 14L), message = stri_sub(message, 16)
        )
    ]

    err_dt[J(NA_character_), on = "prefix", `:=`(prefix = "**   ~~~   **", message = string)]

    # exclude empty lines
    err_dt <- err_dt[!J(""), on = "message"]

    # check if simulation complete successfully
    if (nrow(err_dt[stri_detect_regex(message, "^(EnergyPlus|(GroupdTempCalc\\S*)) Completed Successfully")])) {
        att$successful <- TRUE
    } else if (nrow(err_dt[stri_startswith_fixed(message, "EnergyPlus Terminated")])) {
        att$terminated <- TRUE
    }

    # exclude unuseful lines
    l_end <- err_dt[stri_detect_regex(message, "(Final|(EnergyPlus Warmup|Sizing)) Error Summary"), which = TRUE]
    if (length(l_end)) err_dt <- err_dt[-(min(l_end):.N)]

    # extract envir
    err_dt[stri_startswith_fixed(message, "Beginning"), `:=`(envir = stri_sub(message, 11L))]
    err_dt[stri_startswith_fixed(message, "===== Recurring Error Summary ====="), `:=`(envir = "Recurring Errors")]

    # set envir index
    err_dt[!is.na(envir), envir_index := .I]

    # fill downwards
    err_dt[, `:=`(envir_index = envir_index[1L], envir = envir[1L]),
        by = list(cumsum(!is.na(envir_index)))]

    # for those simulation without sizing and etc.
    err_dt[J(NA_integer_), on = "envir_index", `:=`(envir = "Simulation Initiation", envir_index = 0L)]

    # exclude envir lines
    err_dt <- err_dt[!line %in% err_dt[envir_index > 0L, line[1L], by = c("envir_index")]$V1]

    # make sure envir index starts from 1L
    if (!min(err_dt$envir_index)) set(err_dt, NULL, "envir_index", err_dt$envir_index + 1L)

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
    #
    # (e) recurring error message
    # ************* ===== Recurring Error Summary =====
    # ************* Nominally Unused Constructions
    # *************  ** Warning ** GetSpecificHeatGlycol: Temperature out of ...
    # *************  **   ~~~   **   This error occurred 6260 total times;
    # *************  **   ~~~   **   during Warmup 0 times;
    # *************  **   ~~~   **   during Sizing 0 times.
    # *************  **   ~~~   **   Max=34813.033168 {C}  Min=125.270579 {C}
    # }}}

    l <- err_dt[(stri_startswith_fixed(message, "The following") & envir != "Recurring Errors") |
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

    # extract total recurring times
    err_dt[J("Recurring Errors"), on = "envir", by = "level_index",
        num := {
            num <- stri_match_first_regex(message, "This error occurred (\\d+) total times;")[, 2L]
            num <- as.integer(num)
            if (any(!is.na(num))) num <- rep(num[!is.na(num)], length.out = .N)
            num
        }
    ]

    set(err_dt, NULL, c("string", "line", "prefix"), NULL)
    setcolorder(err_dt, c("index", "envir_index", "envir", "level_index", "level", "message"))

    # assign attributes
    for (i in names(att)) setattr(err_dt, i, att[[i]])

    setattr(err_dt, "class", cls)

    err_dt
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

    if (!is.null(attr(x, "eplus_build")) && !is.na(attr(x, "eplus_build"))) {
        cli::cat_line(paste0("  * EnergyPlus version: ", attr(x, "eplus_version"), " (", attr(x, "eplus_build"), ")"))
    } else {
        cli::cat_line(paste0("  * EnergyPlus version: ", attr(x, "eplus_version")))
    }
    cli::cat_line(paste0("  * Simulation started: ", attr(x, "datetime")))
    cli::cat_line(paste0("  * Terminated: ", attr(x, "terminated")))
    cli::cat_line(paste0("  * Successful: ", attr(x, "successful")))

    if (info) {
        dt <- x
    } else {
        dt <- x[!J("Info"), on = "level"]
    }

    if (!nrow(dt)) {
        cat("\n  [EnergyPlus did not generate any message...]\n", sep = "")
        return(invisible(x))
    }

    # error summary
    if (any(dt$level != "Info")) {
        num_sum <- dt[!J("Info"), on = "level", list(num = max(level_index)), by = c("level")]
        set(num_sum, NULL, "level", factor(num_sum$level, c("Warning", "Severe", "Fatal"), ordered = TRUE))
        err_sm <- num_sum[order(level), paste0("  * ", rpad(paste0(as.character(level), "[", stri_sub(as.character(level), to = 1L, ), "]: "), " "), num)]
        cli::cat_line(err_sm)
    }

    if (brief) return(invisible(x))

    cli::cat_line()
    cli::cat_line(format_errdt(dt, info))

    return(invisible(x))
}
# }}}

# print.TransitionErrFile {{{
print.TransitionErrFile <- function (x, brief = FALSE, info = TRUE, ...) {
    cli::cat_rule("IDFVersionUpdater Error File", line = 2)

    path <- attr(x, "path")
    path_idf <- normalizePath(paste0(tools::file_path_sans_ext(path), ".idf"), mustWork = FALSE)
    cli::cat_line(paste0("  * Input file: ", path_idf))
    cli::cat_line(paste0("  *  From  Ver: ", attr(x, "from")))
    cli::cat_line(paste0("  * Toward Ver: ", attr(x, "to")))

    if (info) {
        dt <- x
    } else {
        dt <- x[!J("Info"), on = "level"]
    }

    if (!nrow(dt)) {
        cat("\n  [IDFVersionUpdater did not generate any message...]\n", sep = "")
        return(invisible(x))
    }

    # error summary
    if (any(dt$level != "Info")) {
        num_sum <- dt[!J("Info"), on = "level", list(num = max(level_index)), by = c("level")]
        set(num_sum, NULL, "level", factor(num_sum$level, c("Warning", "Severe", "Fatal"), ordered = TRUE))
        err_sm <- num_sum[order(level), paste0("  * ", rpad(paste0(as.character(level), "[", stri_sub(as.character(level), to = 1L, ), "]: "), " "), num)]
        cli::cat_line(err_sm)
    }

    if (brief) return(invisible(x))

    cli::cat_line()
    cli::cat_line(format_errdt(dt, info))

    return(invisible(x))
}
# }}}

# format_errdt {{{
format_errdt <- function (dt, info = TRUE) {
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
    dt[dt[!J("Recurring Errors"), on = "envir", .I[1L], by = c("level", "level_index")]$V1,
       out := paste0("[", stri_sub(level, to = 1L), " ", level_index, "/", level_num, "] ", out)
    ]
    dt[dt[J("Recurring Errors"), on = "envir", .I[1L], nomatch = 0L, by = c("level", "level_index")]$V1,
       out := {
           rec_times <- rep("", .N)
           rec_times[!is.na(num)] <- paste0(" (", num[!is.na(num)], ")")
           paste0("[", stri_sub(level, to = 1L), " ", level_index, "/", level_num, rec_times, "] ", out)
       }
    ]

    # add envir name heading
    dt[dt[!J("Recurring Errors"), on = "envir", .I[1L], by = c("envir_index")]$V1,
        out := paste0(rule(paste0("During ", envir)), "\n", out), by = "id"]
    dt[dt[J("Recurring Errors"), on = "envir", .I[1L], nomatch = 0L, by = c("envir_index")]$V1,
        out := paste0(rule(envir), "\n", out), by = "id"]
    # separate different envir
    dt[dt[, .I[1L], by = c("envir_index")]$V1[-1L], out := paste0("\n", out)]

    # clean
    if (info) set(dt, NULL, c("id", "level_num"), NULL)

    on.exit(set(dt, NULL, "out", NULL), add = TRUE)
    dt$out
}
# }}}
