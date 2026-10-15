#' @importFrom callr r_bg
#' @importFrom checkmate assert_flag assert_file_exists assert_directory_exists
#' @importFrom checkmate assert_logical
#' @importFrom cli cat_line
#' @importFrom data.table data.table setattr setnames
#' @importFrom lubridate with_tz
#' @importFrom tools file_path_sans_ext
#' @importFrom processx process
#' @importFrom tools file_path_sans_ext
NULL

# get current time with system time zone
current <- function() {
    tm <- Sys.time()
    attr(tm, "tzone") <- Sys.timezone()
    tm
}

#' Get file path from EnergyPlus installation directory
#'
#' @details
#'
#' * `path_eplus()` returns the file path specified in EnergyPlus installation
#'   directory.
#' * `path_eplus_processor()` is the same as `path_eplus()` expect it
#'   automatically prepend the executable extension, i.e. `.exe` on Windows and
#'   empty on macOS and Linux.
#' * `path_eplus_example()` returns the file path specified under the `ExampleFiles`
#'   folder in EnergyPlus installation directory.
#' * `path_eplus_weather()` returns the file path specified under the
#'   `WeatherData` folder in EnergyPlus installation directory.
#' * `path_eplus_dataset()` returns the file path specified under the
#'   `DataSets` folder in EnergyPlus installation directory.
#'
#' @param ver An acceptable EnergyPlus version or an EnergyPlus installation
#'        directory
#' @param ... File paths passed to [base::file.path()].
#' @param file A single string of file name.
#' @param strict If `TRUE`, an error will be issued if the specified file does
#'        not exist
#'
#' @examples
#' \dontrun{
#' path_eplus("8.8", "Energy+.idd")
#'
#' path_eplus_processor("8.8", "EPMacro", strict = TRUE)
#' path_eplus_processor("8.8", "PreProcess", "GrndTempCalc", "Slab", strict = TRUE)
#'
#' path_eplus_example("8.8", "1ZoneUncontrolled.idf")
#' path_eplus_example("8.8", "BasicFiles/Exercise1A.idf")
#'
#' path_eplus_weather("8.8", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.ddy")
#'
#' path_eplus_dataset("8.8", "Boilers.idf")
#' path_eplus_dataset("8.8", "FMUs/MoistAir.fmu")
#' }
#' @export
#' @author Hongyuan Jia
path_eplus <- function(ver, ..., strict = FALSE) {
    inputs <- list(...)
    dir <- suppressMessages(use_eplus(ver))$dir

    path <- normalizePath(do.call(file.path, c(dir, inputs)), mustWork = FALSE)
    if (strict && any(miss <- !file.exists(path))) {
        abort(sprintf(
            "Failed to find file(s) under EnergyPlus installation directory ('%s'):\n%s",
            dir,
            paste0(collapse = "\n", sprintf(" #%s| '%s'",
                lpad(which(miss), "0"),
                substring(gsub(dir, "", path, fixed = TRUE), 2L)
            ))
        ))
    }
    path
}

#' @export
#' @rdname path_eplus
path_eplus_processor <- function(ver, ..., strict = FALSE) {
    inputs <- list(...)
    exe <- if (is_windows()) ".exe" else ""
    inputs[[length(inputs)]] <- sprintf("%s%s", inputs[[length(inputs)]], exe)
    do.call(path_eplus, c(ver = ver, inputs, strict = strict))
}

#' @export
#' @rdname path_eplus
path_eplus_example <- function(ver, file, strict = FALSE) {
    path_eplus(ver, "ExampleFiles", file, strict = strict)
}

#' @export
#' @rdname path_eplus
path_eplus_weather <- function(ver, file, strict = FALSE) {
    path_eplus(ver, "WeatherData", file, strict = strict)
}

#' @export
#' @rdname path_eplus
path_eplus_dataset <- function(ver, file, strict = FALSE) {
    path_eplus(ver, "DataSets", file, strict = strict)
}

#' Clean working directory of a previous EnergyPlus simulation
#'
#' Clean working directory of an EnergyPlus simulation by deleting all input and
#' output files of previous simulation.
#'
#' @param path An `.idf` or `.imf` file path.
#'
#' @details
#'
#' `clean_wd()` imitates the same process that EnergyPlus does whenever a new
#'  simulation is getting to start. It deletes all related output files that
#'  have the same name prefix as the input path. The input model itself and
#'  any weather file are not deleted. `clean_wd()` is called internally when
#'  running EnergyPlus models using [run_idf()] and [run_multi()].
#'
#' @examples
#' \dontrun{
#' # run a test simulation
#' idf_path <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
#' epw_path <- path_eplus_weather("8.8",
#'      "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#' )
#' dir <- file.path(tempdir(), "test")
#' run_idf(idf_path, epw_path, output_dir = dir, echo = FALSE)
#'
#' list.files(dir)
#'
#' # remove all output files
#' clean_wd(file.path(dir, basename(idf_path)))
#'
#' list.files(dir)
#' }
#' @export
#' @author Hongyuan Jia
clean_wd <- function(path) .clean_wd(path, "C")
.clean_wd <- function(path, suffix_type = c("C", "L", "D"), exclude = NULL) {
    assert_string(path)
    assert_character(exclude, null.ok = TRUE)
    wd <- dirname(path)

    suffix_type <- match.arg(suffix_type)

    out_files <- unlist(get_eplus_output_name(path, suffix_type), use.names = FALSE)

    individuals <- c(
        "Energy+.ini", "Energy+.idd",
        # EPMacro input
        "in.imf",
        # EnergyPlus input
        "in.idf",
        # EnergyPlus epJSON input
        "in.epJSON",
        # EnergyPlus weather input
        "in.epw",
        # EnergyPlus weather input
        "in.stat",
        # EPMacro output
        "out.idf",
        # temporary mvi file used for ReadVarsESO for eplusout.mtr
        "test.mvi",
        # ReadVarsESO output
        "readvars.audit",
        # ExpandObjects output
        "expanded.idf", "expandedidf.err", "BasementGHTIn.idf", "GHTIn.idf",
        # Basement preprocessor output
        "EPObjects.TXT", "GrTemp.TXT", "TempInit.TXT", "RunSolar.TXT",
        "RunINPUT.TXT", "RunTGMAVG.TXT", "RunDEBUGOUT.TXT", "basementout.audit",
        "MonthlyResults.csv",
        # Slab preprocessor output
        "slab.int", "SLABSurfaceTemps.TXT", "SLABINP.txt", "eplusout.err",
        "SLABDBOUT.TXT", "SLABSplit Surface Temps.TXT", "audit.out",

        "fort.6"
    )

    if (tools::file_path_sans_ext(basename(path)) == "in") {
        # keep the original input IDF and EPW
        individuals <- setdiff(individuals, c("in.epw", sprintf("in.%s", tools::file_ext(path))))
    }

    targets <- c(out_files, individuals)
    if (!is.null(exclude)) targets <- setdiff(targets, exclude)

    targets <- file.path(wd, targets)

    unlink(targets[file.exists(targets)])
}

get_eplus_output_name <- function(path, suffix_type = c("C", "L", "D")) {
    suffix_type <- match.arg(suffix_type)

    if (is.null(path)) {
        without_ext <- "eplus"
    } else {
        without_ext <- basename(path)
        if (without_ext == "in") without_ext <- "eplus"
    }

    pre <- switch(suffix_type, "C" = without_ext, "L" = "eplusout", "D" = without_ext)

    files <- list(
        # TODO: did not know what this output is used for
        log = list(pre = pre, ext = ".log"),

        # output from EPMacro program
        epmidf = list(pre = pre, ext = ".epmidf"),
        epmdet = list(pre = pre, ext = ".epmdet"),
        # output from ExpandObjects program
        epxidf = list(pre = pre, ext = ".expidf"),
        experr = list(pre = pre, ext = ".experr"),
        # output from Basement program
        bsmt_idf = list(pre = pre, ext = "_bsmt.idf"),
        bsmt_csv = list(pre = pre, ext = "_bsmt.csv"),
        bsmt_out = list(pre = pre, ext = "_bsmt.out"),
        bsmt_audit = list(pre = pre, ext = "_bsmt.audit"),
        # output from Slab program
        slab_gtp = list(pre = pre, ext = "_slab.gtp"),
        slab_out = list(pre = pre, ext = "_slab.out"),
        slab_ger = list(pre = pre, ext = "_slab.ger"),
        # echo of input
        audit = list(pre = pre, ext = ".audit"),
        # branches and nodes
        bnd = list(pre = pre, ext = ".bnd"),
        # debug output
        dbg = list(pre = pre, ext = ".dbg"),
        # surface drawing
        dxf = list(pre = pre, ext = ".dxf"),
        # EMS report
        edd = list(pre = pre, ext = ".edd"),
        # standard and optional reports
        eio = list(pre = pre, ext = ".eio"),
        # standard output file
        eso = list(pre = pre, ext = ".eso"),
        variable = list(pre = pre, ext = ".csv"),
        # one line summary of success or failure
        end = list(pre = pre, ext = ".end"),
        # error file
        err = list(pre = pre, ext = ".err"),
        # meter names
        mdd = list(pre = pre, ext = ".mdd"),
        # meter details report
        mtd = list(pre = pre, ext = ".mtd"),
        # log file for PerformancePrecisionTradeoffs
        perflog = list(pre = paste0(pre, "_perflog"), ext = ".csv"),
        # variable names
        rdd = list(pre = pre, ext = ".rdd"),
        # results from Output:Surfaces:List, Lines
        sln = list(pre = pre, ext = ".sln"),
        # daylighting factors for exterior windows
        dfs = list(pre = pre, ext = ".dfs"),
        # HVAC-Diagram output
        svg = list(pre = pre, ext = ".svg"),
        # cost information
        sci = list(pre = pre, ext = ".sci"),
        # results from Output:Surfaces:List, VRML
        wrl = list(pre = pre, ext = ".wrl"),
        # SQLite file
        sqlite = list(pre = pre, ext = ".sql"),
        # GLHE
        glhe = list(pre = pre, ext = ".glhe"),
        # output from convertESOMTR program
        ipeso = list(pre = pre, ext = ".ipeso"),
        ipmtr = list(pre = pre, ext = ".ipmtr"),
        iperr = list(pre = pre, ext = ".iperr"),
        # ReadVarsESO audit
        rvaudit = list(pre = pre, ext = ".rvaudit"),
        # JSON outputs
        json = list(
            pre = paste0(pre, c(
                "",
                "_detailed_zone",
                "_detailed_HVAC",
                "_timestep",
                "_yearly",
                "_monthly",
                "_daily",
                "_hourly",
                "_runperiod"
            )),
            ext = ".json"
        ),
        epjson = list(pre = pre, ext = ".epJSONout"),

        # tabular outputs
        table = list(pre = pre, ext = c(".csv", ".tab", ".txt", ".htm", ".xml")),
        # daylighting intensity map output
        map = list(pre = pre, ext = c(".csv", ".tab", ".txt")),
        # results from Sizing:System
        ssz = list(pre = pre, ext = c(".csv", ".tab", ".txt")),
        # results from Sizing:Zone
        zsz = list(pre = pre, ext = c(".csv", ".tab", ".txt")),
        # meter output file
        mtr = list(pre = pre, ext = ".mtr"),
        meter = list(pre = pre, ext = ".csv"),
        # SQLite error file
        sqlite_err = list(pre = pre, ext = ".err"),
        # TODO: did not know what this output is used for
        ads = list(pre = pre, ext = ".out"),
        # window screen transmittance map output
        screen = list(pre = pre, ext = ".csv"),
        # surface shading combination report
        shd = list(pre = pre, ext = ".shd"),
        shading = list(pre = pre, ext = ".csv"),
        # descriptive of inputs into DElight inputs and simulation results
        delight = list(pre = pre, ext = c(".delightin", ".delightout", ".delighteldmp", ".delightdfdmp"))
    )
    files$cbor <- files$msgpack <- files$json
    files$cbor$ext <- ".cbor"
    files$msgpack$ext <- ".msgpack"

    if (suffix_type == "C") {
        files$table$pre       <-  paste0(pre, "Table")
        files$map$pre         <-  paste0(pre, "Map")
        files$zsz$pre         <-  paste0(pre, "Zsz")
        files$ssz$pre         <-  paste0(pre, "Ssz")
        files$meter$pre       <-  paste0(pre, "Meter")
        files$ads$pre         <-  paste0(pre, "Ads")
        files$sqlite_err$pre  <-  paste0(pre, "Sqlite")
        files$screen$pre      <-  paste0(pre, "Screen")
        files$shading$pre     <-  paste0(pre, "Shading")
        files$delight$pre     <-  paste0(pre, "DElight")
        files$delight$ext     <-  c(".in", ".out", ".dfdmp", ".eldmp")
    } else if (suffix_type == "L") {
        files$table$pre       <-  "eplustbl"
        files$map$pre         <-  "eplusmap"
        files$zsz$pre         <-  "epluszsz"
        files$ssz$pre         <-  "eplusssz"
        files$meter$pre       <-  "eplusmtr"
        files$ads$pre         <-  "eplusADS"
        files$sqlite_err$pre  <-  "sqlite"
        files$screen$pre      <-  "eplusscreen"
        files$shading$pre     <-  "eplusshading"
        files$delight$pre     <-  "eplusout"
        files$delight$ext     <-  c(".delightin", ".delightout", ".delightdfdmp", ".delighteldmp")
    } else {
        files$table$pre       <-  paste0(pre, "-table")
        files$map$pre         <-  paste0(pre, "-map")
        files$zsz$pre         <-  paste0(pre, "-zsz")
        files$ssz$pre         <-  paste0(pre, "-ssz")
        files$meter$pre       <-  paste0(pre, "-meter")
        files$sqlite_err$pre  <-  paste0(pre, "-sqlite")
        files$ads$pre         <-  paste0(pre, "-ads")
        files$screen$pre      <-  paste0(pre, "-screen")
        files$shading$pre     <-  paste0(pre, "-shading")
        files$delight$pre     <-  paste0(pre, "-delight")
        files$delight$ext     <-  c(".in", ".out", ".dfdmp", ".eldmp")
    }

    lapply(files, function(f) paste0(f$pre, f$ext))
}

eplus_exe <- function(eplus) {
    if (checkmate::test_file_exists(eplus, "x") || (is_windows() && has_ext(eplus, "exe"))) {
        suppressMessages(use_eplus(dirname(eplus)))
        return(normalizePath(eplus, mustWork = TRUE))
    }

    if (is_eplus_path(eplus)) {
        config <- suppressMessages(use_eplus(eplus))
        return(normalizePath(file.path(config$dir, config$exe), mustWork = TRUE))
    }

    if (!is_avail_eplus(eplus)) use_eplus(eplus)
    config <- tryCatch(eplus_config(eplus),
        eplusr_warning_miss_eplus_config = function(w) abort(conditionMessage(w), "miss_eplus_config")
    )

    normalizePath(file.path(config$dir, config$exe), mustWork = TRUE)
}

create_energyplus_ini <- function(eplus, output_dir) {
    dir <- normalizePath(dirname(eplus))
    sep <- if (is_windows()) "\\" else "/"
    if (is_windows()) {
        dir <- paste0(utils::shortPathName(dir), sep)
    } else {
        dir <- paste0(dir, sep)
    }

    ini <- c(
        "[program]",
        sprintf("dir=%s", dir),
        "[BasementGHT]",
        sprintf("dir=PreProcess%sGrndTempCalc%s", sep, sep),
        "[SlabGHT]",
        sprintf("dir=PreProcess%sGrndTempCalc%s", sep, sep)
    )

    out <- normalizePath(file.path(output_dir, "Energy+.ini"), mustWork = FALSE)
    write_lines(ini, out)
    out
}

copy_energyplus_idd <- function(eplus, output_dir, idd = NULL, name = "Energy+.idd") {
    output_dir <- normalizePath(output_dir, "/")
    if (is.null(idd)) {
        # directly use the IDD file from EnergyPlus folder
        idd <- normalizePath(file.path(dirname(eplus), name), mustWork = FALSE)
        # nocov start
        if (!file.exists(idd)) {
            abort(sprintf("'%s' file did not found in EnergyPlus installation folder: '%s'", name, dirname(idd)))
        }
        # nocov end
    } else {
        assert_file_exists(idd, "r", "idd")
        idd <- normalizePath(idd)
    }
    if (idd_copied <- output_dir != dirname(idd) || basename(idd) != name) {
        idd <- file_copy(idd, file.path(output_dir, name))
    }

    attr(idd, "copied") <- idd_copied
    idd
}

run_command <- function(
    command,
    args = NULL,
    wd,
    wait = TRUE,
    echo = TRUE,
    post_callback = NULL,
    post_name = NULL,
    exit_msg = NULL,
    exit_callback = NULL,
    call_r = FALSE,
    kill_tree = FALSE
) {
    assert_flag(wait)
    assert_flag(echo)
    assert_string(post_name, null.ok = TRUE)
    assert_string(exit_msg, null.ok = TRUE)
    assert_function(post_callback, null.ok = TRUE)
    assert_function(exit_callback, null.ok = TRUE)
    assert_flag(call_r)
    assert_flag(kill_tree)
    if (!call_r) {
        assert_character(args, null.ok = TRUE)
    }

    start_time <- current()

    args <- args %||% character()

    if (wait) {
        std_out <- "|"
        std_err <- "|"
        post_fun <- NULL
    } else {
        std_out <- tempfile()
        std_err <- tempfile()

        post_fun <- function() {
            out <- suppressWarnings(read_lines(std_out)$string)
            err <- suppressWarnings(read_lines(std_err)$string)

            # remove "\r"
            out <- gsub("\r", "", out, fixed = TRUE)
            err <- gsub("\r", "", err, fixed = TRUE)

            if (!length(out)) {
                out <- NULL
            }
            if (!length(err)) {
                err <- NULL
            }
            unlink(c(std_out, std_err))

            res <- list(stdout = out, stderr = err, end_time = current())

            if (!is.null(post_callback)) {
                res <- list(post_callback(), run = res)
                if (!is.null(post_name)) {
                    names(res) <- c(post_name, "run")
                }
            }

            res
        }
    }

    if (call_r) {
        if (!length(args)) {
            args <- list()
        }
        proc <- callr::r_bg(func = command, args = args,
            wd = wd, cleanup = TRUE, cleanup_tree = TRUE,
            windows_verbatim_args = FALSE,
            stdout = std_out, stderr = std_err, post_process = post_fun,
            package = TRUE,
            supervise = kill_tree
        )
    } else {
        proc <- processx::process$new(command = command, args = args,
            wd = wd, cleanup = TRUE, cleanup_tree = TRUE,
            windows_verbatim_args = FALSE,
            stdout = std_out, stderr = std_err, post_process = post_fun
        )
    }

    if (!wait) {
        # just return the process
        res <- list(
            process = proc,
            exit_status = NULL,
            stdout = NULL,
            stderr = NULL,
            start_time = start_time,
            end_time = NULL
        )
    } else {
        # Reap API descendants before a foreground condition leaves the caller's stack.
        callback <- function() {
            if (!proc$is_alive()) {
                return(NULL)
            }
            k <- if (kill_tree) {
                run__api_kill(proc)
            } else {
                # nocov start
                tryCatch(proc$kill(), error = function(e) FALSE)
                # nocov end
            }
            # nocov start
            if (!is.null(exit_callback)) {
                exit_callback(command, args)
            }
            if (k && !is.null(exit_msg)) {
                cli::cat_line(exit_msg, col = "white", background_col = "red")
            }
            # nocov end
        }
        on.exit(callback(), add = TRUE)

        stdout <- c()
        stderr <- c()
        get_output <- function(echo = TRUE) {
            newout <- proc$read_output_lines(2000)
            if (length(newout) && all(nzchar(newout))) {
                newout <- gsub("\r", "", newout, fixed = TRUE)
                if (echo) {
                    cli::cat_line(newout)
                }
                stdout <<- c(stdout, newout)
            }

            newerr <- proc$read_error(2000)
            if (length(newerr) && all(nzchar(newerr))) {
                newerr <- gsub("\r", "", newerr, fixed = TRUE)
                if (echo) {
                    cli::cat_line(newerr, col = "red")
                }
                stderr <<- c(stderr, newerr)
            }
        }

        while (proc$is_alive()) {
            polled <- proc$poll_io(200)

            # if output/error, collect it
            if (any(polled == "ready")) get_output(echo)
        }

        # needed to get the exit status
        proc$wait()

        # might still have output
        while (proc$is_incomplete_output() || proc$is_incomplete_error()) {
            proc$poll_io(-1)
            get_output(echo)
        }

        list(process = proc,
             exit_status = proc$get_exit_status(),
             stdout = stdout,
             stderr = stderr,
             start_time = start_time,
             end_time = current()
        )
    }
}

get_ver_from_idf_path <- function(path) get_idf_ver(read_lines(path))

get_eplus_loc_from_input <- function(idf, eplus = NULL) {
    eplus <- eplus %||% as.character(get_ver_from_idf_path(idf))
    if (!length(eplus)) {
        abort(paste0("Missing version field in input IDF file '", normalizePath(idf), "'. ",
            "Failed to determine the version of EnergyPlus to use."),
            "miss_idf_ver"
        )
    }

    eplus_exe(eplus)
}

get_ver_from_output_string <- function(line) {
    re <- "Program Version,EnergyPlus, Version (\\d{1,2}\\.\\d\\.\\d)-[0-9a-f]+, YMD=\\d+\\.\\d{2}\\.\\d{2} \\d{2}:\\d{2}"
    standardize_ver(regmatches(line, regexec(re, line))[[1L]][2L])
}

pre_eplus_command <- function(exectuable,
                              model, weather = NULL,
                              output_dir = NULL, output_prefix = NULL,
                              wait = TRUE, echo = TRUE, eplus = NULL,
                              strict = TRUE, legacy = TRUE) {
    assert_flag(wait)
    assert_flag(echo)
    assert_flag(strict)
    assert_flag(legacy)

    if (!strict) {
        assert_file_exists(model, "r")
    } else {
        assert_file_exists(model, "r", c("idf", "epmidf", "epxidf", "imf"))
    }

    model <- normalizePath(model)
    dir_model <- normalizePath(dirname(model))
    file_model <- basename(model)
    name_model <- tools::file_path_sans_ext(file_model)
    ext_model <- tools::file_ext(model)

    if (!is.null(weather)) {
        assert_file_exists(weather, "r", "epw")
        weather <- normalizePath(weather)
        dir_weather <- normalizePath(dirname(weather))
        file_weather <- basename(weather)
    }

    if (!strict) {
        energyplus_exe <- NULL
        if (!is.null(eplus)) {
            energyplus_exe <- eplus_exe(eplus)
        } else {
            l <- read_lines(model, nrows = 1L)$string
            energyplus_exe <- eplus_exe(get_ver_from_output_string(l))
        }
    } else {
        energyplus_exe <- get_eplus_loc_from_input(model, eplus)
    }

    assert_string(output_dir, null.ok = TRUE)
    output_dir <- normalizePath(output_dir %||% dir_model, mustWork = FALSE)
    # nocov start
    if (!dir.exists(output_dir) && !dir.create(output_dir, FALSE, TRUE)) {
        abort(sprintf("Failed to create output directory '%s'.", output_dir))
    }
    # nocov end

    # always copy input files to the output directory
    if (output_dir != dir_model) {
        file_copy(model, file.path(output_dir, file_model))
    }
    if (!is.null(weather) && output_dir != dir_weather) {
        file_copy(weather, file.path(output_dir, file_weather))
    }

    assert_string(output_prefix, null.ok = TRUE)
    if (is.null(output_prefix)) {
        output_prefix <- tools::file_path_sans_ext(file_model)
    }

    if (is.null(exectuable)) {
        exectuable <- energyplus_exe
    } else {
        exectuable <- do.call(path_eplus_processor,
            c(ver = dirname(energyplus_exe), as.list(exectuable), strict = TRUE)
        )
    }

    idf <- NULL
    epw <- NULL

    if (legacy) {
        # create in.idf/in.imf in the same directory
        if (tolower(name_model) == "in" && ext_model == "idf") {
            idf <- model
        } else {
            if (ext_model %in% c("epmidf", "expidf")) ext_model <- "idf"

            if (ext_model %in% c("idf", "imf")) {
                idf <- file_copy(model, file.path(dir_model, sprintf("in.%s", ext_model)))
            }
        }

        # create in.epw in the same directory of input model
        if (!is.null(weather)) {
            weather_in <- tools::file_path_sans_ext(basename(weather))
            weather_ext <- tools::file_ext(weather)
            if (tolower(weather_in) != "in") {
                epw <- file_copy(weather, file.path(dir_model, sprintf("in.%s", weather_ext)))
            }
        }
    }

    list(
        energyplus = energyplus_exe, exectuable = exectuable,
        model = model, weather = weather,
        idf = idf, epw = epw,
        output_dir = output_dir, output_prefix = output_prefix
    )
}

remove_eplus_in_files <- function(model, weather = NULL) {
    wd <- dirname(model)
    if (tools::file_path_sans_ext(basename(model)) != "in") {
        unlink(file.path(wd, sprintf("in.%s", tools::file_ext(model))))
    }

    if (!is.null(weather)) {
        if (tools::file_path_sans_ext(basename(weather)) != "in") {
            unlink(file.path(wd, sprintf("in.%s", tools::file_ext(weather))))
        }
    }
}

interrupted_msg <- function(exe, idf, epw) {
    sprintf("TERMINATED [%s] --> [IDF] '%s' + [EPW] '%s'", exe, basename(idf), basename(epw %||% "<Empty>"))
}

#' @rdname energyplus
#' @keywords internal
#' @export
EPMacro <- function(model,
                    output_dir = NULL, output_prefix = NULL,
                    wait = TRUE, echo = TRUE, eplus = NULL) {
    cmd <- pre_eplus_command(
        exectuable = "EPMacro",
        model = model, weather = NULL,
        output_dir = output_dir, output_prefix = output_prefix,
        wait = wait, echo = echo, eplus = eplus
    )

    if (!has_ext(model, "imf")) {
        verbose_info(sprintf(
            "Input model has an extension of '.%s' but not '.imf'. Skip...",
            tools::file_ext(model)
        ))
        remove_eplus_in_files(cmd$model)

        return(list(file = NULL, run = NULL))
    }

    path_sim <- function(file) normalizePath(file.path(dirname(cmd$model), basename(file)), mustWork = FALSE)
    path_out <- function(file) normalizePath(file.path(cmd$output_dir, basename(file)), mustWork = FALSE)
    path_out2 <- function(suffix) normalizePath(file.path(cmd$output_dir, sprintf("%s%s", cmd$output_prefix, suffix)), mustWork = FALSE)

    # handle ouput file renaming
    file_callback <- function() {
        remove_eplus_in_files(cmd$model)

        file <- list()
        file$imf <- path_out(cmd$model)
        file$epmidf <- file_rename_if_exist(path_sim("out.idf"), path_out2(".epmidf"))
        file$epmdet <- file_rename_if_exist(path_sim("audit.out"), path_out2(".epmdet"))
        file
    }

    # only run post processing when running in background
    post_callback <- NULL
    if (!wait) post_callback <- file_callback

    run <- run_command(cmd$exectuable, wd = dirname(cmd$model), wait = wait, echo = echo,
        post_callback = post_callback, post_name = "file",
        exit_msg = interrupted_msg("EPMacro", cmd$model, cmd$weather)
    )

    file <- NULL
    if (wait) file <- file_callback()

    list(file = file, run = run)
}

#' @rdname energyplus
#' @keywords internal
#' @export
ExpandObjects <- function(model,
                          output_dir = NULL, output_prefix = NULL,
                          wait = TRUE, echo = TRUE, eplus = NULL, idd = NULL) {
    cmd <- pre_eplus_command(
        exectuable = "ExpandObjects",
        model = model, weather = NULL,
        output_dir = output_dir, output_prefix = output_prefix,
        wait = wait, echo = echo, eplus = eplus
    )

    if (!is.null(idd)) assert_file_exists(idd, "r", "idd")

    wd <- dirname(cmd$idf)
    path_sim <- function(file) normalizePath(file.path(wd, basename(file)), mustWork = FALSE)
    path_out <- function(file) normalizePath(file.path(cmd$output_dir, basename(file)), mustWork = FALSE)
    path_out2 <- function(suffix) normalizePath(file.path(cmd$output_dir, sprintf("%s%s", cmd$output_prefix, suffix)), mustWork = FALSE)

    create_energyplus_ini(cmd$energyplus, wd)
    copy_energyplus_idd(cmd$energyplus, wd, idd)

    # handle ouput file renaming
    file_callback <- function() {
        if (tools::file_ext(cmd$model) != "idf") unlink(cmd$idf)
        remove_eplus_in_files(cmd$model)

        unlink(path_sim("Energy+.ini"))
        if (wd != dirname(cmd$energyplus)) unlink(path_sim("Energy+.idd"))

        file <- list()
        file$idf <- path_out(cmd$model)
        file$expidf <- file_rename_if_exist(path_sim("expanded.idf"), path_out2(".expidf"))
        file$basement <- file_rename_if_exist(path_sim("BasementGHTIn.idf"), path_out("BasementGHTIn.idf"))
        file$slab <- file_rename_if_exist(path_sim("GHTIn.idf"), path_out("GHTIn.idf"))
        file$experr <- file_rename_if_exist(path_sim("expandedidf.err"), path_out2(".experr"))
        file
    }

    # only run post processing when running in background
    post_callback <- NULL
    if (!wait) post_callback <- file_callback

    run <- run_command(cmd$exectuable, wd = dirname(cmd$idf), wait = wait, echo = echo,
        post_callback = post_callback, post_name = "file",
        exit_msg = interrupted_msg("ExpandObjects", cmd$model, cmd$weather)
    )

    file <- NULL
    if (wait) file <- file_callback()

    list(file = file, run = run)
}

#' @rdname energyplus
#' @keywords internal
#' @export
Basement <- function(model, weather,
                     output_dir = NULL, output_prefix = NULL,
                     wait = TRUE, echo = TRUE, eplus = NULL, idd = NULL) {
    cmd <- pre_eplus_command(
        exectuable = c("PreProcess", "GrndTempCalc", "Basement"),
        model = model, weather = weather,
        output_dir = output_dir, output_prefix = output_prefix,
        wait = wait, echo = echo, eplus = eplus
    )

    if (!is.null(idd)) assert_file_exists(idd, "r", "idd")

    wd <- dirname(cmd$idf)
    path_sim <- function(file) normalizePath(file.path(wd, basename(file)), mustWork = FALSE)
    path_out <- function(file) normalizePath(file.path(cmd$output_dir, basename(file)), mustWork = FALSE)
    path_sim2 <- function(suffix) normalizePath(file.path(wd, sprintf("%s%s", cmd$output_prefix, suffix)), mustWork = FALSE)
    path_out2 <- function(suffix) normalizePath(file.path(cmd$output_dir, sprintf("%s%s", cmd$output_prefix, suffix)), mustWork = FALSE)

    cmd$idf <- file_rename_if_exist(cmd$idf, path_sim("BasementGHTIn.idf"))

    unlink(path_sim(c(
        "EPObjects.TXT", "GrTemp.TXT", "TempInit.TXT", "RunSolar.TXT",
        "RunINPUT.TXT", "RunTGMAVG.TXT", "eplusout.end", "audit.out",
        "rundebugout.txt", "basementout.audit"
    )))

    unlink(path_sim2(c(
        ".audit", ".out", "_out.idf", "_bsmt.csv", "_bsmt.audit",
        "_bsmt.out", "_out_bsmt.idf"
    )))

    create_energyplus_ini(cmd$energyplus, wd)

    # copy BasementGHT.idd
    idd <- copy_energyplus_idd(cmd$exectuable, wd, idd, "BasementGHT.idd")

    # handle ouput file renaming
    file_callback <- function() {
        remove_eplus_in_files(cmd$model, cmd$weather)
        if (attr(idd, "copied")) unlink(path_sim("BasementGHT.idd"))
        unlink(path_sim("Energy+.ini"))

        file <- list()
        file$idf <- path_out(cmd$model)
        file$epw <- path_out(cmd$weather)

        file$bsmt_idf <- file_rename_if_exist(path_sim("EPObjects.TXT"), path_out2("_bsmt.idf"))
        file$bsmt_csv <- file_rename_if_exist(path_sim("MonthlyResults.csv"), path_out2("_bsmt.csv"))
        file$bsmt_out <- file_rename_if_exist(path_sim("RunINPUT.TXT"), path_out2("_bsmt.out"))

        if (!file.exists(path_sim("RunDEBUGOUT.TXT"))) {
            file$bsmt_audit <- path_out2("_bsmt.audit")
            write_lines("Basement Audit File", file$bsmt_audit)
        } else {
            file$bsmt_audit <- file_rename(path_sim("RunDEBUGOUT.TXT"), path_out2("_bsmt.audit"))
        }

        if (file.exists(path_sim("audit.out"))) {
            write_lines(read_lines(path_sim("audit.out")), file$bsmt_audit, append = TRUE)
        }
        if (file.exists(path_sim("eplusout.err"))) {
            write_lines(read_lines(path_sim("eplusout.err")), file$bsmt_audit, append = TRUE)
        }

        unlink(path_sim(c(
            "EPObjects.TXT", "GrTemp.TXT", "TempInit.TXT", "RunSolar.TXT",
            "RunINPUT.TXT", "RunTGMAVG.TXT", "eplusout.end", "audit.out",
            "RunDEBUGOUT.TXT", "basementout.audit", "eplusout.err")
        ))

        file
    }

    # only run post processing when running in background
    post_callback <- NULL
    if (!wait) post_callback <- file_callback

    run <- run_command(cmd$exectuable, wd = dirname(cmd$idf), wait = wait, echo = echo,
        post_callback = post_callback, post_name = "file",
        exit_msg = interrupted_msg("Basement", cmd$model, cmd$weather)
    )

    file <- NULL
    if (wait) file <- file_callback()

    list(file = file, run = run)
}

#' @rdname energyplus
#' @keywords internal
#' @export
Slab <- function(model, weather,
                 output_dir = NULL, output_prefix = NULL,
                 wait = TRUE, echo = TRUE, eplus = NULL, idd = NULL) {
    cmd <- pre_eplus_command(
        exectuable = c("PreProcess", "GrndTempCalc", "Slab"),
        model = model, weather = weather,
        output_dir = output_dir, output_prefix = output_prefix,
        wait = wait, echo = echo, eplus = eplus
    )

    wd <- dirname(cmd$idf)

    path_sim <- function(file) normalizePath(file.path(wd, basename(file)), mustWork = FALSE)
    path_out <- function(file) normalizePath(file.path(cmd$output_dir, basename(file)), mustWork = FALSE)
    path_sim2 <- function(suffix) normalizePath(file.path(wd, sprintf("%s%s", cmd$output_prefix, suffix)), mustWork = FALSE)
    path_out2 <- function(suffix) normalizePath(file.path(cmd$output_dir, sprintf("%s%s", cmd$output_prefix, suffix)), mustWork = FALSE)

    cmd$idf <- file_rename_if_exist(cmd$idf, path_sim("GHTIn.idf"))

    unlink(path_sim(c(
        "EPObjects.TXT", "SLABDBOUT.TXT", "SLABINP.TXT",
        "SLABSplit Surface Temps.TXT", "eplusout.end", "audit.out"
    )))
    unlink(path_sim2(c(".gtp", ".ger", "_slab.gtp", "_slab.ger")))

    create_energyplus_ini(cmd$energyplus, wd)

    # copy SlabGHT.idd
    idd <- copy_energyplus_idd(cmd$exectuable, wd, idd, "SlabGHT.idd")

    # handle ouput file renaming
    file_callback <- function() {
        remove_eplus_in_files(cmd$model, cmd$weather)
        if (attr(idd, "copied")) unlink(path_sim("SlabGHT.idd"))
        unlink(path_sim("Energy+.ini"))

        file <- list()
        file$idf <- path_out(cmd$model)
        file$epw <- path_out(cmd$weather)

        file$slab_gtp <- file_rename_if_exist(path_sim("SLABSurfaceTemps.TXT"), path_out2("_slab.gtp"))
        file$slab_out <- file_rename_if_exist(path_sim("SLABINP.TXT"), path_out2("_slab.out"))
        file$slab_ger <- file_rename_if_exist(path_sim("eplusout.err"), path_out2("_slab.ger"))

        unlink(path_sim(c(
            "EPObjects.TXT", "eplusout.err", "SLABDBOUT.TXT",
            "SLABSplit Surface Temps.TXT", "eplusout.end", "audit.out"
        )))

        file
    }

    # only run post processing when running in background
    post_callback <- NULL
    if (!wait) post_callback <- file_callback

    run <- run_command(cmd$exectuable, wd = dirname(cmd$idf), wait = wait, echo = echo,
        post_callback = post_callback, post_name = "file",
        exit_msg = interrupted_msg("Slab", cmd$model, cmd$weather)
    )

    file <- NULL
    if (wait) file <- file_callback()

    list(file = file, run = run)
}

#' @rdname energyplus
#' @keywords internal
#' @export
EnergyPlus <- function(
    model,
    weather,
    output_dir = NULL,
    output_prefix = NULL,
    output_suffix = c("C", "L", "D"),
    wait = TRUE,
    echo = TRUE,
    annual = FALSE,
    design_day = FALSE,
    idd = NULL,
    eplus = NULL,
    backend = "cli"
) {
    configuration <- run__backend(backend)
    backend <- configuration$engine
    api_setup <- configuration$setup
    if (backend == "api" && !wait) {
        # Orchestration returns a normal R process; live API objects stay in its engine child.
        runner <- run_command(
            function(...) {
                result <- EnergyPlus(..., wait = TRUE)
                result$run$process <- NULL
                result
            },
            list(
                model = model,
                weather = weather,
                output_dir = output_dir,
                output_prefix = output_prefix,
                output_suffix = output_suffix,
                echo = echo,
                annual = annual,
                design_day = design_day,
                idd = idd,
                eplus = eplus,
                backend = api_setup %||% backend
            ),
            wd = tempdir(),
            wait = FALSE,
            echo = echo,
            call_r = TRUE,
            kill_tree = TRUE
        )
        return(list(file = NULL, run = runner, backend = backend))
    }
    assert_flag(annual)
    assert_flag(design_day)
    output_suffix <- match.arg(output_suffix)

    cmd <- pre_eplus_command(
        exectuable = NULL,
        model = model, weather = weather,
        output_dir = output_dir, output_prefix = output_prefix,
        wait = wait, echo = echo, eplus = eplus
    )

    eplus_ver <- get_ver_from_eplus_path(dirname(cmd$energyplus))
    if (backend == "api") {
        run__api_version(eplus_ver)
    }
    # just for test purpose
    legacy <- getOption("eplusr.eplus_legacy", eplus_ver < "8.3")

    # cannot set annual and design_day if energy
    if (any(annual, design_day) && legacy) {
        abort(
            paste0(
                "Currently, 'annual' and 'design_day' options are only supported when running IDFs of EnergyPlus v8.3 and above. ",
                "This is because those options are only implemented in EnergyPlus command line interface ",
                "which is available only in EnergyPlus v8.3 and above. ",
                "You can update the version of your model using 'transition()' or 'version_updater()' and try again."
            ),
            "eplus_ver_not_supported"
        )
    }

    # cannot set both annual and design_day
    if (annual && design_day) {
        abort("Cannot force both design-day and annual simulations", "both_ddy_annual")
    }

    # clean up working directory
    .clean_wd(cmd$idf, "L")

    # get all legacy EnergyPlus output file names
    legacy_files <- get_eplus_output_name(NULL, "L")

    # get all targeted EnergyPlus output file names
    out_files <- get_eplus_output_name(cmd$output_prefix, output_suffix)

    # remove all old output files in the output directory
    unlink(file.path(cmd$output_dir, unlist(out_files, use.names = FALSE)))

    # exclude EPMacro and ExpandObjects outputs
    out_files <- out_files[!names(out_files) %in% c("epmidf", "epmdet", "epxidf", "experr")]
    legacy_files <- legacy_files[names(legacy_files) %in% names(out_files)]

    # handle EnergyPlus < v8.3 where there is no EnergyPlus command line
    # interface
    if (legacy) {
        args <- NULL

        wd <- dirname(cmd$model)

        # create ini file in and copy idd
        create_energyplus_ini(cmd$energyplus, wd)
        idd <- copy_energyplus_idd(cmd$energyplus, wd, idd)

        # handle ouput file renaming
        file_callback <- function() {
            remove_eplus_in_files(cmd$model, cmd$weather)
            if (attr(idd, "copied")) {
                unlink(file.path(wd, "Energy+.idd"))
            }
            unlink(file.path(wd, "Energy+.ini"))

            # manually handle file renaming
            file <- lapply(seq_along(legacy_files), function(i) {
                from <- file.path(wd, legacy_files[[i]])
                to <- file.path(cmd$output_dir, out_files[[i]])
                files <- file_rename_if_exist(from, to)
                files <- files[!is.na(files)]
                if (!length(files)) {
                    files <- NA_character_
                }
                files
            })
            names(file) <- names(out_files)

            file
        }
    } else {
        # no more need "in.idf" and "in.epw"
        remove_eplus_in_files(cmd$model, cmd$weather)

        if (output_suffix == "L") {
            cmd$output_prefix <- "eplus"
        }

        args <- c(
            "--output-directory", cmd$output_dir,
            "--output-prefix", cmd$output_prefix,
            "--output-suffix", output_suffix
        )

        if (annual) {
            args <- c(args, "--annual")
        }
        if (design_day) {
            args <- c(args, "--design-day")
        }
        if (!is.null(idd)) {
            args <- c(args, "--idd", normalizePath(idd, mustWork = FALSE))
        }
        if (!is.null(weather)) {
            args <- c(
                args,
                "--weather",
                normalizePath(weather, mustWork = FALSE)
            )
        }

        # NOTE: if input file has an extension of 'epmidf' or 'epmidf', create
        # an IDF file with the same name
        model <- cmd$model
        if (clean_idf <- tools::file_ext(model) %in% c("epmidf", "expidf")) {
            model <- file_copy(model, file.path(dirname(model), sprintf("%s.idf", tools::file_path_sans_ext(basename(model)))))
        }

        args <- c(args, model)

        file_callback <- function() {
            if (clean_idf) {
                unlink(model)
            }
            file <- list()
            file$idf <- normalizePath(file.path(cmd$output_dir, basename(cmd$model)))
            if (!is.null(cmd$weather)) {
                file$epw <- normalizePath(file.path(cmd$output_dir, basename(cmd$weather)))
            }
            out <- lapply(out_files, function(files) {
                files <- file.path(cmd$output_dir, files)
                files <- files[file.exists(files)]
                if (!length(files)) {
                    NA_character_
                } else {
                    normalizePath(files)
                }
            })

            c(file, out)
        }
    }

    # only run post processing when running in background
    post_callback <- NULL
    if (!wait) post_callback <- file_callback

    run <- if (backend == "api") {
        run__api_command(
            dirname(cmd$energyplus),
            args,
            dirname(cmd$idf),
            echo,
            api_setup
        )
    } else {
        run_command(
            cmd$exectuable,
            args,
            wd = dirname(cmd$idf),
            wait = wait,
            echo = echo,
        post_callback = post_callback, post_name = "file",
        exit_msg = interrupted_msg("EnergyPlus", cmd$model, cmd$weather)
    )
    }

    if (wait) {
        file <- file_callback()
    }

    list(file = file, run = run, backend = backend)
}

#' @rdname energyplus
#' @keywords internal
#' @export
convertESOMTR <- function(eso, output_dir = NULL, output_prefix = NULL, rules = NULL,
                          wait = TRUE, echo = TRUE, eplus = NULL) {
    assert_file_exists(eso, "r", c("eso", "mtr"))

    cmd <- pre_eplus_command(
        exectuable = c("PostProcess", "convertESOMTRpgm", "convertESOMTR"),
        model = eso, weather = NULL,
        output_dir = output_dir, output_prefix = output_prefix,
        wait = wait, echo = echo, eplus = eplus, strict = FALSE
    )

    wd <- normalizePath(dirname(cmd$model))
    path_sim <- function(file) normalizePath(file.path(wd, basename(file)), mustWork = FALSE)
    path_out <- function(file) normalizePath(file.path(cmd$output_dir, basename(file)), mustWork = FALSE)
    path_out2 <- function(suffix) normalizePath(file.path(cmd$output_dir, sprintf("%s%s", cmd$output_prefix, suffix)), mustWork = FALSE)

    if (is.null(rules)) {
        rules <- normalizePath(file.path(dirname(cmd$exectuable), "convert.txt"), mustWork = TRUE)
        rules_copied <- TRUE
    } else {
        rules <- normalizePath(assert_file_exists(rules, "r"))
        rules_copied <- tolower(basename(rules)) != "convert.txt" ||
            normalizePath(dirname(rules)) != wd
    }
    # copy conversion rules into the working directory
    rules <- file_copy(rules, path_sim("convert.txt"))

    # copy eso file
    ext <- tools::file_ext(cmd$model)
    eso <- sprintf("eplusout.%s", ext)
    if (eso_copied <- basename(cmd$model) != eso) {
        eso <- file_copy(cmd$model, path_sim(sprintf("eplusout.%s", ext)))
    }

    # handle ouput file renaming
    file_callback <- function() {
        file <- list()
        file$eso <- NA_character_
        file$mtr <- NA_character_
        file$ipeso <- NA_character_
        file$ipmtr <- NA_character_
        file$iperr <- NA_character_

        if (ext == "eso") {
            file$eso <- path_out(cmd$model)
        } else {
            file$mtr <- path_out(cmd$model)
        }

        if (rules_copied) unlink(rules)

        if (file.exists(path_sim("ip.eso"))) {
            if (eso_copied) unlink(path_sim("eplusout.eso"))
            file$ipeso <- file_rename(path_sim("ip.eso"), path_out2(".ipeso"))
        }

        if (file.exists(path_sim("ip.mtr"))) {
            if (eso_copied) unlink(path_sim("eplusout.mtr"))
            file$ipmtr <- file_rename(path_sim("ip.mtr"), path_out2(".ipmtr"))
        }

        if (file.exists(path_sim("ip.err"))) {
            file$iperr <- file_rename(path_sim("ip.err"), path_out2(".iperr"))
        }

        file
    }

    # only run post processing when running in background
    post_callback <- NULL
    if (!wait) post_callback <- file_callback

    run <- run_command(cmd$exectuable, wd = wd, wait = wait, echo = echo,
        post_callback = post_callback, post_name = "file",
        exit_msg = interrupted_msg("convertESOMTR", cmd$model, cmd$weather)
    )

    file <- NULL
    if (wait) file <- file_callback()

    list(file = file, run = run)
}

#' @rdname energyplus
#' @keywords internal
#' @export
ReadVarsESO <- function(eso, output_dir = NULL, output_prefix = NULL,
                        output_suffix = c("C", "L", "D"),
                        max_col = NULL, wait = TRUE, echo = TRUE, eplus = NULL) {
    assert_file_exists(eso, "r", c("eso", "mtr", "ipeso", "ipmtr"))
    output_suffix <- match.arg(output_suffix)

    max_col <- assert_int(max_col, lower = 1L, upper = 250L, null.ok = TRUE, coerce = TRUE)
    if (is.null(max_col)) max_col <- "unlimited"

    cmd <- pre_eplus_command(
        exectuable = c("PostProcess", "ReadVarsESO"),
        model = eso, weather = NULL,
        output_dir = output_dir, output_prefix = output_prefix,
        wait = wait, echo = echo, eplus = eplus, strict = FALSE
    )

    wd <- dirname(cmd$model)
    path_sim <- function(file) normalizePath(file.path(wd, basename(file)), mustWork = FALSE)
    path_out <- function(file) normalizePath(file.path(cmd$output_dir, basename(file)), mustWork = FALSE)
    path_out2 <- function(suffix) normalizePath(file.path(cmd$output_dir, sprintf("%s%s", cmd$output_prefix, suffix)), mustWork = FALSE)

    # file name without extension
    nm <- tools::file_path_sans_ext(basename(cmd$model))
    ext_in <- tools::file_ext(cmd$model)
    # handle eso and mtr after convertESOMTR
    ext_in <- switch(ext_in, ipeso = "eso", ipmtr = "mtr", ext_in)

    # copy files
    if (copied <- basename(cmd$model) != sprintf("eplusout.%s", ext_in)) {
        eso <- file_copy(cmd$model, path_sim(sprintf("eplusout.%s", ext_in)))
    }

    out <- get_eplus_output_name(cmd$output_prefix, output_suffix)

    if (ext_in == "eso") {
        ext <- "rvi"
        inp <- "eplusout.inp"
        csv <- out$variable
    } else {
        ext <- "mvi"
        inp <- "eplusmtr.inp"
        csv <- out$meter
    }

    # copy inp file if exists
    if (file.exists(inp_in <- path_sim(sprintf("%s.%s", nm, ext)))) {
        inp <- file_copy(inp_in, path_sim(inp))
    # create a temporary rvi if eplusout.inp does not exist
    } else {
        inp <- path_sim(inp)
        write_lines(c(sprintf("eplusout.%s", ext_in), csv, ""), inp)
    }

    # handle ouput file renaming
    file_callback <- function() {
        file <- list()
        file$eso <- NA_character_
        file$mtr <- NA_character_
        file$variable <- NA_character_
        file$meter <- NA_character_
        file$rvaudit <- NA_character_

        if (ext_in == "eso") {
            file$eso <- path_out(cmd$model)
        } else {
            file$mtr <- path_out(cmd$model)
        }

        if (copied) unlink(eso)

        unlink(inp)

        if (file.exists(path_sim(csv))) {
            csv <- file_rename(path_sim(csv), path_out(csv))
            if (ext_in == "eso") {
                file$variable <- csv
            } else {
                file$meter <- csv
            }
        }

        if (file.exists(path_sim("readvars.audit"))) {
            file$rvaudit <- file_rename(path_sim("readvars.audit"), path_out2(".rvaudit"))
        }

        file
    }

    # only run post processing when running in background
    post_callback <- NULL
    if (!wait) post_callback <- file_callback

    run <- run_command(cmd$exectuable, c(basename(inp), max_col), wd = wd, wait = wait,
        echo = echo, post_callback = post_callback, post_name = "file",
        exit_msg = interrupted_msg("ReadVarsESO", cmd$model, cmd$weather)
    )

    file <- NULL
    if (wait) file <- file_callback()

    list(file = file, run = run)
}

#' @rdname energyplus
#' @keywords internal
#' @export
HVAC_Diagram <- function(bnd, output_dir = NULL, output_prefix = NULL,
                         wait = TRUE, echo = TRUE, eplus = NULL) {
    assert_file_exists(bnd, "r", "bnd")

    cmd <- pre_eplus_command(
        exectuable = c("PostProcess", "HVAC-Diagram"),
        model = bnd, weather = NULL,
        output_dir = output_dir, output_prefix = output_prefix,
        wait = wait, echo = echo, eplus = eplus, strict = FALSE
    )

    wd <- dirname(cmd$model)
    path_sim <- function(file) normalizePath(file.path(wd, basename(file)), mustWork = FALSE)
    path_out <- function(file) normalizePath(file.path(cmd$output_dir, basename(file)), mustWork = FALSE)
    path_out2 <- function(suffix) normalizePath(file.path(cmd$output_dir, sprintf("%s%s", cmd$output_prefix, suffix)), mustWork = FALSE)

    if (copied <- basename(cmd$model) != "eplusout.bnd") {
        bnd <- file_copy(cmd$model, path_sim("eplusout.bnd"))
    }

    # handle ouput file renaming
    file_callback <- function() {
        file <- list()
        file$bnd <- path_out(cmd$model)
        file$svg <- NA_character_

        if (copied) unlink(bnd)

        if (file.exists(path_sim("eplusout.svg"))) {
            file$svg <- file_rename(path_sim("eplusout.svg"), path_out2(".svg"))
        }

        file
    }

    # only run post processing when running in background
    post_callback <- NULL
    if (!wait) post_callback <- file_callback

    run <- run_command(cmd$exectuable, wd = wd, wait = wait,
        echo = echo, post_callback = post_callback, post_name = "file",
        exit_msg = interrupted_msg("HVAC-Diagram", cmd$model, cmd$weather)
    )

    file <- NULL
    if (wait) file <- file_callback()

    list(file = file, run = run)
}

#' Run EnergyPlus and its various processors
#'
#' @details
#'
#' `EPMacro()` calls the EnergyPlus EPMacro processor.
#'
#' `ExpandObjects()` calls the EnergyPlus ExpandObjects processor.
#'
#' `Basement()` calls the EnergyPlus Basement preprocessor.
#'
#' `Slab()` calls the EnergyPlus Slab preprocessor.
#'
#' `EnergyPlus()` calls EnergyPlus itself.
#'
#' `convertESOMTR()` calls EnergyPlus convertESOMTR post-processor.
#'
#' `ReadVarsESO()` calls EnergyPlus ReadVarsESO post-processor.
#'
#' `HVAC_Diagram()` calls EnergyPlus HVAC-Diagram post-processor.
#'
#' `energyplus()` is the one which correctly chains all the steps that call
#' those pre- and post- processors to form a complete EnergyPlus simulation.
#'
#' @note
#'
#' `energyplus()` can only run in waiting mode.
#'
#' @param model \[`character(1)`\]\cr
#' A path of an EnergyPlus IDF or IMF file.
#'
#' @param eso \[`character(1)`\]\cr
#' A path of an EnergyPlus standard output (`.eso`) or EnergyPlus meter output
#' (`.mtr`) file.
#'
#' @param bnd \[`character(1)`\]\cr
#' A path of an EnergyPlus branch node details (`.bnd`) file.
#'
#' @param weather \[`character(1)` or `NULL`\]\cr
#' A path of an EnergyPlus weather (EPW) file. If `NULL`, design-day-only
#' simulation is triggered, regardless of the `design_day` value.
#'
#' @param output_dir \[`character(1)` or `NULL`\]\cr
#' Output directory of EnergyPlus simulation outputs. If `NULL`, the directory
#' where the input `model` locates is used. Default: `NULL`.
#'
#' @param output_prefix \[`character(1)` or `NULL`\]\cr
#' Prefix for EnergyPlus output file names. If `NULL`, the input `model` file
#' name is used.  Default: `NULL`.
#'
#' @param output_suffix \[`character(1)`\]\cr
#' Suffix style for EnergyPlus output file names. Should be one of the
#' followings:
#'
#' - `C`: **Capital**, e.g. `eplusTable.csv`. This is the default.
#' - `L`: **Legacy**, e.g. `eplustbl.csv`.
#' - `D`: **Dash**, e.g. `eplus-table.csv`.
#'
#' @param epmacro \[`logical(1)`\]\cr
#' If `TRUE`, EPMacro processor is called prior to simulation. Only applicable
#' if input file is an `IMF` file. Default: `TRUE`.
#'
#' @param expand_obj \[`logical(1)`\]\cr
#' If `TRUE`, ExpandObjects processor is called prior to simulation. Should be
#' `TRUE` if calling Basement or Slab preprocessors is desired. Default: `TRUE`.
#'
#' @param annual \[`logical(1)`\]\cr
#' If `TRUE`, annual simulation is forced. Currently, only support EnergyPlus >=
#' v8.3. Note that `annual` and `design_day` cannot both be `TRUE`. Default:
#' `FALSE`.
#'
#' @param design_day \[`logical(1)`\]\cr
#' If `TRUE`, design-day-only simulation is forced. Currently, only support
#' EnergyPlus >= v8.3. Note that `annual` and `design_day` cannot both be
#' `TRUE`. Default: `FALSE`.
#'
#' @param eso_to_ip \[`logical(1)`\]\cr
#' If `TRUE`, convertESOMTR post-processor is called after simulation to convert
#' the units of data in `eso` file from SI units to IP units. Default: `FALSE`.
#'
#' @param readvars \[`logical(1)`\]\cr
#' If `TRUE`, ReadVarsESO post-processor is called after to simulation. Default:
#' `TRUE`.
#'
#' @param echo \[`logical(1)`\]\cr
#' Whether to show standard output and error from EnergyPlus and its pre- and
#' post- processors. Default: `TRUE`.
#'
#' @param wait \[`logical(1)`\]\cr
#' If `FALSE`, simulation is run in the background and a [processx::process]
#' object is returned. Extra steps are needed to collect the results after the
#' process completes.
#'
#' @param idd \[`character(1)` or `NULL`\]\cr
#' The full path of EnergyPlus IDD (Input Data Dictionary). If `NULL`,
#' `Energy+.idd` file in EnergyPlus installation directory is used. Default:
#' `NULL`.
#'
#' @param eplus \[`character(1)` or `NULL`\]\cr
#' An EnergyPlus version or a path of EnergyPlus installation directory. If
#' `NULL`, the version of EnergyPlus to use is determined by the version of
#' input `model`. Default: `NULL`.
#'
#' @param resources \[`character()` or `NULL`\]\cr
#' Any external file dependencies that EnergyPlus will use for simulation. If
#' not `NULL`, files will be copied to the output directory. Default: `NULL`.
#'
#' @param backend Simulation engine: `"cli"` (default), experimental `"api"`,
#'   or an initialization `function(api, state)` which selects the API engine.
#'   The API backend supports stock EnergyPlus 26.1 and creates a fresh R process
#'   for each engine run. Preprocessing and result collection use the same pipeline.
#'   A function is executed once in the engine worker
#'   before simulation. Register requests and callbacks using the supplied managed
#'   objects. Capture serializable lexical configuration in a self-contained closure;
#'   Live API objects and native pointers cannot be transferred. The return value is ignored.
#'   Controller failures and worker interrupts yield failed
#'   results with condition metadata; an explicit stop yields a stopped result.
#'   Results record the resolved `"cli"` or `"api"` backend; API results also carry
#'   `$api` metadata. Existing CLI result fields
#'   remain available. Job orchestration may use additional R processes.
#'
#' @keywords internal
#' @return
#'
#' Functions except for `energyplus()` return a list containing:
#'
#' - `file`: a named list of full paths of output files
#' - `run`: a named list of outputs from the process.
#'   `EnergyPlus()` also returns `backend`, the resolved engine name.
#'
#' `energyplus()` returns a list containing:
#'
#' - `version`: EnergyPlus [version][numeric_version()] used
#' - `energyplus`: EnergyPlus installation directory
#' - `start_time`: a [POSIXct()] giving the local time when the simulation
#'   starts
#' - `end_time`: a [POSIXct()] giving the local time when the simulation ends
#' - `output_dir`: full path of output directory of simulation outputs
#' - `file`: a named list of relative paths of output files under `output_dir`
#' - `run`: a [data.table][data.table::data.table()] of each outputs from the
#'   all called processes
#' - `backend`: the resolved engine name, `"cli"` or `"api"`
#' - `api`: native identity and outcome metadata, present only for the API engine
#'
#' @export
energyplus <- function(
    model,
    weather,
    output_dir = NULL,
    output_prefix = NULL,
    output_suffix = c("C", "L", "D"),
    epmacro = TRUE,
    expand_obj = TRUE,
    annual = FALSE,
    design_day = FALSE,
    eso_to_ip = FALSE,
    readvars = TRUE,
    echo = TRUE,
    wait = TRUE,
    idd = NULL,
    eplus = NULL,
    resources = NULL,
    backend = "cli"
) {
    configuration <- run__backend(backend)
    backend <- configuration$engine
    api_setup <- configuration$setup
    assert_flag(wait)

    if (wait) {
        run_energyplus(model = model, weather = weather, output_dir = output_dir,
            output_prefix = output_prefix, output_suffix = output_suffix,
            epmacro = epmacro, expand_obj = expand_obj,
            annual = annual, design_day = design_day,
            eso_to_ip = eso_to_ip, readvars = readvars,
            echo = echo,
            idd = idd,
            eplus = eplus,
            resources = resources,
            backend = api_setup %||% backend
        )
    } else {
        run_command(
            function(...) energyplus(...),
            args = list(
                model = model, weather = weather, output_dir = output_dir,
                output_prefix = output_prefix, output_suffix = output_suffix,
                epmacro = epmacro, expand_obj = expand_obj,
                annual = annual, design_day = design_day,
                eso_to_ip = eso_to_ip, readvars = readvars,
                echo = echo,
                idd = idd,
                eplus = eplus,
                resources = resources,
                backend = api_setup %||% backend
            ),
            wd = tempdir(),
            call_r = TRUE,
            wait = FALSE,
            kill_tree = backend == "api"
        )$process
    }
}
run_energyplus <- function(
    model,
    weather,
    output_dir = NULL,
    output_prefix = NULL,
    output_suffix = c("C", "L", "D"),
    epmacro = TRUE,
    expand_obj = TRUE,
    annual = FALSE,
    design_day = FALSE,
    eso_to_ip = FALSE,
    readvars = TRUE,
    echo = TRUE,
    idd = NULL,
    eplus = NULL,
    resources = NULL,
    backend = "cli"
) {
    configuration <- run__backend(backend)
    backend <- configuration$engine
    api_setup <- configuration$setup
    assert_flag(epmacro)
    assert_flag(expand_obj)
    assert_flag(eso_to_ip)
    assert_flag(readvars)
    if (!is.null(resources)) {
        assert_file_exists(resources)
    }
    output_suffix <- match.arg(output_suffix)

    start_time <- current()

    # validate inputs and copy input files into the output directory
    cmd <- pre_eplus_command(NULL, model, weather, output_dir, output_prefix,
        echo = echo, eplus = eplus, legacy = FALSE)

    # get EnergyPlus exectuable version
    eplus_ver <- get_ver_from_eplus_path(dirname(cmd$energyplus))
    if (backend == "api") {
        run__api_version(eplus_ver)
    }
    # just for test purpose
    legacy <- getOption("eplusr.eplus_legacy", eplus_ver < "8.3")

    file <- list()
    run <- list()

    # run simulation in a temporary simulation directory
    # NOTE: Copy input files to a temporary folder inside the specified output
    #       directory in case that multiple EnergyPlus instances are running
    #       with that folder being the output directory which all expect an
    #       "in.idf" input file. This behavior mimicks EP-Launch.
    #
    #       For EnergyPlus v8.3 and above, there is no need to do so thanks to
    #       the command line interface. But all the preprocessors and
    #       postprocessors behave the same despite the version which all expect
    #       the legacy input/output file names. Also, obFMU is used and
    #       "ShouldExportCSVResults" is set to "Yes", obFMU always assumes that
    #       it was run inside a temporary directory inside the output directory.
    #       So just in case, all simulations should be run in the temporary
    #       directory despite of what EnergyPlus version is used.
    #
    #       However, even though, this will not always work because it is
    #       possible that the input model has some external file dependencies,
    #       e.g. FMU, schedule files. If this is the case, eplus_job() should be
    #       used.
    cmd$sim_dir <- normalizePath(tempfile("EPTEMP-", cmd$output_dir), mustWork = FALSE)
    # nocov start
    while (dir.exists(cmd$sim_dir)) {
        cmd$sim_dir <- normalizePath(tempfile("EPTEMP-", cmd$output_dir), mustWork = FALSE)
    }
    if (!dir.create(cmd$sim_dir, FALSE)) {
        abort(sprintf("Failed to create simulation directory '%s'.", cmd$sim_dir))
    }
    # nocov end

    path_sim <- function(file) normalizePath(file.path(cmd$sim_dir, basename(file)), mustWork = FALSE)
    path_out <- function(file) normalizePath(file.path(cmd$output_dir, basename(file)), mustWork = FALSE)
    path_sim2 <- function(suffix) normalizePath(file.path(cmd$sim_dir, sprintf("%s%s", cmd$output_prefix, suffix)), mustWork = FALSE)
    path_out2 <- function(suffix) normalizePath(file.path(cmd$output_dir, sprintf("%s%s", cmd$output_prefix, suffix)), mustWork = FALSE)

    temp_name <- function() basename(tempfile(tmpdir = cmd$sim_dir))
    file_temp <- function(file, ext = NULL) {
        res <- rep(NA_character_, length(file))
        if (all(isna <- is.na(file))) return(res)
        if (is.null(ext)) {
            ext <- paste0(".", tools::file_ext(file))
        }
        res[!isna] <- file_rename(file[!isna], path_sim(sprintf("%s%s", temp_name(), ext)))
        res
    }
    file_out <- function(file, ext = NULL) {
        res <- rep(NA_character_, length(file))
        if (all(isna <- is.na(file))) return(res)
        if (is.null(ext)) {
            ext <- paste0(".", tools::file_ext(file))
        }
        res[!isna] <- file_rename(file[!isna], path_out(sprintf("%s%s", cmd$output_prefix, ext[!isna])))
        res
    }

    # clean output directory
    .clean_wd(path_out(cmd$model), output_suffix)

    # create "in.i[dm]f" file to the simulation directory
    cmd$idf <- file_copy(cmd$model, path_sim(sprintf("in.%s", file_ext(cmd$model))))
    file$epw <- NA_character_
    if (!is.null(cmd$weather)) {
        file$epw <- path_out(cmd$weather)
        cmd$epw <- file_copy(cmd$weather, path_sim(sprintf("in.%s", file_ext(cmd$weather))))
    }

    # make sure the resources are copied to both the temporary and output
    # directory, which is essential for obFMU to work
    if (!is.null(resources)) {
        file_copy(resources, path_sim(resources))
        resources <- file_copy(resources, path_out(resources))
    }

    # 1. run EPMacro
    run$EPMacro <- list()
    file$imf <- NA_character_
    file$epmidf <- NA_character_
    file$epmdet <- NA_character_
    if (!epmacro && has_ext(cmd$idf, "imf")) epmacro <- TRUE
    if (!(epmacro && has_ext(cmd$idf, "imf"))) {
        file$idf <- path_out(cmd$model)
    } else {
        file$imf <- path_out(cmd$model)

        res_epmacro <- EPMacro(cmd$idf, cmd$sim_dir, cmd$output_prefix,
            wait = TRUE, echo = echo, eplus = eplus_ver)
        run$EPMacro <- res_epmacro$run

        # Since cmd$idf has the legency name "in.imf", EPMacro() will not delete
        # it after calling EPMacro
        unlink(cmd$idf)

        # copy the output epmidf file to the output directory
        file$epmidf <- NA_character_
        if (!is.na(res_epmacro$file$epmidf)) {
            # Here can not directly copy the ".epmidf" file to the output
            # directory, since EnergyPlus() will call clean_wd().
            # In order to keep this file after calling EnergyPlus(), here rename
            # it to an tempfile name in the simulation directory and move it to
            # the output directory with correct name after calling EnergyPlus()
            file$epmidf <- file_temp(res_epmacro$file$epmidf)

            # copy the output epmidf file to "in.epmidf"
            cmd$idf <- file_copy(file$epmidf, path_sim("in.epmidf"))
        }

        # move the output epmdet file to the output directory
        file$epmdet <- NA_character_
        if (!is.na(res_epmacro$file$epmdet)) {
            # same as above, rename it with a random name in the simulation
            # directory
            file$epmdet <- file_temp(res_epmacro$file$epmdet)
        }
    }

    # 2. run ExpandObjects
    run$ExpandObjects <- list()
    file$expidf <- NA_character_
    file$experr <- NA_character_
    if (expand_obj) {
        res_expandobj <- ExpandObjects(cmd$idf, cmd$sim_dir, cmd$output_prefix,
            wait = TRUE, echo = echo, eplus = eplus_ver, idd = idd)
        run$ExpandObjects <- res_expandobj$run

        if (!is.na(res_expandobj$file$basement)) {
            file$basement <- res_expandobj$file$basement
        }
        if (!is.na(res_expandobj$file$slab)) {
            file$slab <- res_expandobj$file$slab
        }

        # copy the output expidf file to the output directory
        if (!is.na(res_expandobj$file$expidf)) {
            # rename it with a random name in the simulation directory
            file$expidf <- file_temp(res_expandobj$file$expidf)

            # delete the "in.idf"
            unlink(cmd$idf)

            # use the expanded model as the input for later simulation
            cmd$idf <- file_copy(file$expidf, path_sim2("_expanded.idf"))
        }

        if (!is.na(res_expandobj$file$experr)) {
            # rename it with a random name in the simulation directory
            file$experr <- file_temp(res_expandobj$file$experr)
        }
    }

    # 3. run Basement preprocessor
    run$Basement <- list()
    file$bsmt_idf <- NA_character_
    file$bsmt_csv <- NA_character_
    file$bsmt_out <- NA_character_
    file$bsmt_audit <- NA_character_
    if (!is.null(file$basement) && !is.na(file$basement)) {
        if (is.null(cmd$epw)) {
            abort(paste(
                "'BasementGHTIn.idf' found after running ExpandObjects",
                "which means that the Basement preprocessor is needed to run",
                "before running EnergyPlus. However, no weather input is found.",
                "Please specify a non-NULL 'weather' argument and run again."
            ))
        }

        res_basement <- Basement(file$basement, cmd$epw, cmd$sim_dir, cmd$output_prefix,
            wait = TRUE, echo = echo, eplus = eplus_ver)
        run$Basement <- res_basement$run

        # remove Basement input file
        unlink(file$basement)
        file$basement <- NULL

        # append bsmt_idf to the input model
        write_lines(read_lines(res_basement$file$bsmt_idf), file$expidf, append = TRUE)
        write_lines(read_lines(res_basement$file$bsmt_idf), cmd$idf, append = TRUE)

        # rename output files with random names to avoid them being deleted by
        # EnergyPlus()
        file$bsmt_idf <- file_temp(res_basement$file$bsmt_idf, "_bsmt.idf")
        file$bsmt_csv <- file_temp(res_basement$file$bsmt_csv, "_bsmt.csv")
        file$bsmt_out <- file_temp(res_basement$file$bsmt_out, "_bsmt.out")
        file$bsmt_audit <- file_temp(res_basement$file$bsmt_audit, "_bsmt.audit")
    }

    # 4. run Slab preprocessor
    run$Slab <- list()
    file$slab_gtp <- NA_character_
    file$slab_out <- NA_character_
    file$slab_ger <- NA_character_
    if (!is.null(file$slab) && !is.na(file$slab)) {
        if (is.null(cmd$epw)) {
            abort(paste(
                "'GHTIn.idf' found after running ExpandObjects ",
                "which means that the Slab preprocessor is needed to run ",
                "before running EnergyPlus. However, no weather input is found.",
                "Please specify a non-NULL 'weather' argument and run again."
            ))
        }

        res_slab <- Slab(file$slab, cmd$epw, cmd$sim_dir, cmd$output_prefix,
            wait = TRUE, echo = echo, eplus = eplus_ver)
        run$Slab <- res_slab$run

        # remove Slab input file
        unlink(file$slab)
        file$slab <- NULL

        # append slab_gtp to the input model
        write_lines(read_lines(res_slab$file$slab_gtp), file$expidf, append = TRUE)
        write_lines(read_lines(res_slab$file$slab_gtp), cmd$idf, append = TRUE)

        # rename output files with random names to avoid them being deleted by
        # EnergyPlus()
        file$slab_gtp <- file_temp(res_slab$file$slab_gtp, "_slab.gtp")
        file$slab_out <- file_temp(res_slab$file$slab_out, "_slab.out")
        file$slab_ger <- file_temp(res_slab$file$slab_ger, "_slab.ger")
    }

    # 5. run EnergyPlus
    # NOTE: It is possible that the input file still uses resources with
    # relative paths, e.g. `Schedule:File`. `resources` should be specified in
    # this case.
    #
    # TODO: Revisit this after refactor IDF parsing. If the time spent on
    # parsing the IDF file is neglectable, copy all external files into the
    # temporary simulation directory maybe an option.
    res_eplus <- EnergyPlus(cmd$idf, cmd$epw, cmd$sim_dir, cmd$output_prefix,
        output_suffix, wait = TRUE, echo = echo, annual = annual,
        design_day = design_day,
        idd = idd,
        eplus = eplus_ver,
        backend = api_setup %||% backend
    )
    # now it's safe to move all output files to the output directory
    res_eplus$file$idf <- NULL
    res_eplus$file$epw <- NULL
    res_eplus$file <- lapply(res_eplus$file, function(f) {
        if (length(f) == 1L && is.na(f)) {
            return(f)
        }
        file_rename_if_exist(path_sim(f), path_out(f))
    })

    # In order to avoid unnecessary file copy of the output eso/mtr file,
    # rename it to the legacy name "eplusout.eso/mtr".
    if (eso_to_ip || readvars) {
        res_eplus$file$eso <- file_rename_if_exist(res_eplus$file$eso, path_sim("eplusout.eso"))
        res_eplus$file$mtr <- file_rename_if_exist(res_eplus$file$mtr, path_sim("eplusout.mtr"))
    }
    run$EnergyPlus <- res_eplus$run

    # move output files from EPMacro, ExpandObjects, Basement and Slab to the
    # output directory after calling EnergyPlus()
    file$epmidf <- file_out(file$epmidf)
    file$epmdet <- file_out(file$epmdet)
    file$expidf <- file_out(file$expidf)
    file$experr <- file_out(file$experr)
    file$bsmt_idf <- file_out(file$bsmt_idf, "_bsmt.idf")
    file$bsmt_csv <- file_out(file$bsmt_csv, "_bsmt.csv")
    file$bsmt_out <- file_out(file$bsmt_out, "_bsmt.out")
    file$bsmt_audit <- file_out(file$bsmt_audit, "_bsmt.audit")
    file$slab_gtp <- file_out(file$slab_gtp, "_slab.gtp")
    file$slab_out <- file_out(file$slab_out, "_slab.out")
    file$slab_ger <- file_out(file$slab_ger, "_slab.ger")

    # 6. run convertESOMTR
    run$convertESOMTR <- list()
    file$eso <- NA_character_
    file$mtr <- NA_character_
    file$ipeso <- NA_character_
    file$ipmtr <- NA_character_
    file$iperr <- NA_character_
    if (eso_to_ip) {
        if (!is.na(res_eplus$file$eso) || !is.na(res_eplus$file$mtr)) {
            # convertESOMTR() calls convertESOMTR without any arguments.
            # If both "eplusout.eso" and "eplusout.mtr" exist, both ipeso and
            # ipmtr will be generated. There is no need to call convertESOMTR
            # with "eplusout.mtr" as the input.
            res_ip <- convertESOMTR(res_eplus$file$eso, cmd$sim_dir, cmd$outptu_prefix,
                wait = TRUE, echo = echo, eplus = eplus_ver)
            run$convertESOMTR <- res_ip$run

            # Since convertESOMTR() only handles one type of input, one of
            # `file$eso` and `file$mtr` will always be NA
            if (!is.na(res_eplus$file$eso)) {
                res_ip$file$eso <- res_eplus$file$eso
            }
            if (!is.na(res_eplus$file$mtr)) {
                res_ip$file$mtr <- res_eplus$file$mtr
            }

            # directly move the error file to the output directory
            res_ip$file$iperr <- file_out(res_ip$file$iperr)
            file$iperr <- res_ip$file$iperr

            # If no ReadVarsESO() is skippped, directly move to output directory
            if (!readvars) {
                file$ipeso <- file_out(res_ip$file$ipeso)
                file$ipmtr <- file_out(res_ip$file$ipmtr)
                file$eso <- file_out(res_ip$file$eso)
                file$mtr <- file_out(res_ip$file$mtr)
            # In order to avoid unnecessary file copy of the output eso/mtr file,
            # backup the original "eplusout.eso/mtr" to "eplusout.sieso/simtr"
            # and rename its IP version to the legacy name "eplusout.eso/mtr".
            } else {
                res_ip$file$eso <- file_rename_if_exist(res_ip$file$eso, path_sim("eplusout.sieso"))
                res_ip$file$mtr <- file_rename_if_exist(res_ip$file$mtr, path_sim("eplusout.simtr"))
                res_ip$file$ipeso <- file_rename_if_exist(res_ip$file$ipeso, path_sim("eplusout.eso"))
                res_ip$file$ipmtr <- file_rename_if_exist(res_ip$file$ipmtr, path_sim("eplusout.mtr"))

                # the file data will be appended during calling ReadVarsESO()
            }
        }
    }

    # 7. run ReadVarsESO
    run$ReadVarsESO_MTR <- list()
    run$ReadVarsESO_ESO <- list()
    file$variable <- NA_character_
    file$meter <- NA_character_
    file$rvaudit <- NA_character_
    if (readvars) {
        if (!eso_to_ip) {
            eso <- res_eplus$file$eso
            mtr <- res_eplus$file$mtr
        } else {
            eso <- res_ip$file$ipeso
            mtr <- res_ip$file$ipmtr
        }

        if (!is.na(mtr)) {
            res_readmtr <- ReadVarsESO(mtr, cmd$output_dir, cmd$output_prefix, output_suffix,
                wait = TRUE, echo = echo, eplus = eplus_ver)
            run$ReadVarsESO_MTR <- res_readmtr$run

            # Since mtr is the legacy name "eplusout.mtr", ReadVarsESO() will
            # not delete it after calling ReadVarsESO.
            unlink(path_out("eplusout.mtr"))

            file$meter <- res_readmtr$file$meter
            file$rvaudit <- res_readmtr$file$rvaudit

            if (!eso_to_ip) {
                res_eplus$file$mtr <- file_out(res_eplus$file$mtr)
                file$mtr <- res_eplus$file$mtr
            } else {
                # For eplusout.ipmtr, since it is not needed anymore, directly
                # move it to the output directory
                res_ip$file$ipmtr <- file_out(res_ip$file$ipmtr, ".ipmtr")
                file$ipmtr <- res_ip$file$ipmtr

                # move "eplusout.mtr" to the output directory
                res_ip$file$mtr <- file_out(res_ip$file$mtr, ".mtr")
                file$mtr <- res_ip$file$mtr
            }

            # read meter rvaudit in order to append it to variable rvaudit
            mtr_rvaudit <- NULL
            if (!is.na(file$rvaudit)) {
                mtr_rvaudit <- read_lines(file$rvaudit, trim = FALSE)
            }
        }

        if (!is.na(eso)) {
            res_readeso <- ReadVarsESO(eso, cmd$output_dir, cmd$output_prefix, output_suffix,
                wait = TRUE, echo = echo, eplus = eplus_ver)
            run$ReadVarsESO_ESO <- res_readeso$run

            # Since eso is the legacy name "eplusout.eso", ReadVarsESO() will
            # not delete it after calling ReadVarsESO.
            unlink(path_out("eplusout.eso"))

            file$variable <- res_readeso$file$variable
            file$rvaudit <- res_readeso$file$rvaudit

            if (!eso_to_ip) {
                res_eplus$file$eso <- file_out(res_eplus$file$eso)
                file$eso <- res_eplus$file$eso
            } else {
                # For eplusout.ipeso, since it is not needed anymore, directly
                # move it to the output directory
                res_ip$file$ipeso <- file_out(res_ip$file$ipeso, ".ipeso")
                file$ipeso <- res_ip$file$ipeso

                # move "eplusout.eso" to the output directory
                res_ip$file$eso <- file_out(res_ip$file$eso, ".eso")
                file$eso <- res_ip$file$eso
            }

            if (!is.na(res_eplus$file$mtr) && !is.null(mtr_rvaudit) && nrow(mtr_rvaudit)) {
                write_lines(mtr_rvaudit, file$rvaudit, append = TRUE)
            }
        }
    }

    # 8. run HVACDiagram
    run$HVAC_Diagram <- list()
    if (!is.na(res_eplus$file$bnd)) {
        res_eplus$file$bnd <- file_rename(res_eplus$file$bnd, path_sim("eplusout.bnd"))

        # do not echo and run in the simulation dir for safety
        res_diagram <- HVAC_Diagram(res_eplus$file$bnd, cmd$sim_dir, cmd$output_prefix,
            wait = TRUE, echo = FALSE, eplus = eplus_ver)
        run$HVAC_Diagram <- res_diagram$run

        # Since bnd is the legacy name "eplusout.bnd", HVAC_Diagram() will
        # not delete it after calling HVAC-Diagram
        res_eplus$file$bnd <- file_out(res_diagram$file$bnd)
        res_eplus$file$svg <- file_out(res_diagram$file$svg)
    }

    file <- utils::modifyList(res_eplus$file, file)
    file <- file[order(names(file))]
    file$idf <- NA_character_
    if (is.na(file$imf)) {
        file$idf <- path_out(cmd$model)
    }
    file <- lapply(file, basename)
    if (is.null(resources)) {
        resources <- NA_character_
    }
    file['resource'] <- list(basename(resources))

    run <- data.table(
        program = names(run),
        exit_status = lapply(run, "[[", "exit_status"),
        start_time = lapply(run, "[[", "start_time"),
        end_time = lapply(run, "[[", "end_time"),
        stdout = lapply(run, "[[", "stdout"),
        stderr = lapply(run, "[[", "stderr")
    )

    # 9. FMUImport/FMUExport
    unlink(path_sim("tmp-fmus"), recursive = TRUE, force = TRUE)

    unlink(cmd$sim_dir, recursive = TRUE, force = TRUE)

    result <- list(
        version = eplus_ver, energyplus = normalizePath(dirname(cmd$energyplus)),
        start_time = start_time, end_time = current(),
        exit_status = res_eplus$run$exit_status,
        output_dir = cmd$output_dir, file = file, run = run
    )
    # Keep the resolved engine observable for both CLI and function-based API runs.
    result$backend <- backend
    if (backend == "api") {
        result$api <- res_eplus$run$api
    }
    result
}

# reference: https://github.com/r-lib/revdepcheck/blob/master/R/event-loop.R
run_sim_event_loop <- function(state) {
    if (!nrow(state$jobs)) return()

    # in case run in background
    setDT(state$jobs)

    # initialize job status and worker
    set(state$jobs, NULL,
           c("status",  "process", "exit_status", "result", "start_time",   "end_time",     "stdout", "stderr"),
        list("waiting", list(),    NA_integer_ ,  list(),    as.POSIXct(NA), as.POSIXct(NA), list(),   list())
    )

    # global progress bar
    state$progress <- cli::cli_progress_bar(
        total = nrow(state$jobs), clear = FALSE,
        format = "[{cli::pb_current}/{cli::pb_total}] | {cli::pb_percent} {cli::pb_bar} [Elapsed: {cli::pb_elapsed}]"
    )
    # catch current enviroment where the progress bar is created
    # this is needed to correctly update the progress
    state$env <- environment()

    # kill all simulation jobs once exit
    on.exit(kill_all_sims(state), add = TRUE)

    # init one simulation for each worker
    num <- min(state$options$num_parallel, nrow(state$jobs))
    for (i in seq_len(num)) {
        state <- schedule_next_sim(state)
        state <- do_sim(state)
    }

    # run simulations until all simulation completes
    while (TRUE) {
        if (are_all_completed(state)) break;
        state <- handle_sim_events(state)
        state <- schedule_next_sim(state)
        state <- do_sim(state)
    }

    state
}

# Reap every API pipeline and engine worker when the foreground event loop exits abnormally.
kill_all_sims <- function(state) {
    is_running <- which(vlapply(state$jobs$process, function(p) {
        inherits(p, "r_process") && p$is_alive()
    }))
    if (identical(state$options$backend, "api")) {
        for (p in state$jobs$process[is_running]) {
            run__api_kill(p)
        }
    }
    # nocov start
    if (length(is_running)) {
        if (!identical(state$options$backend, "api")) {
            for (p in state$jobs$process) {
                p$kill()
            }
        }

        # change status
        set(state$jobs, is_running, "status", "terminated")

        # update the output from terminated jobs
        set(
            state$jobs,
            is_running,
            "exit_status",
            viapply(state$jobs$process[is_running], function(p) {
                p$get_exit_status()
            })
        )
        set(
            state$jobs,
            is_running,
            "stdout",
            lapply(state$jobs$process[is_running], function(p) {
                tryCatch(p$read_all_output_lines(), error = function(e) {
                    NA_character_
                })
            })
        )
        set(
            state$jobs,
            is_running,
            "stderr",
            lapply(state$jobs$process[is_running], function(p) {
                tryCatch(p$read_all_error_lines(), error = function(e) {
                    NA_character_
                })
            })
        )
    }

    # check if any waiting simulations
    is_waiting <- which(state$jobs$status %chin% c("waiting", "ready"))

    # change status
    set(state$jobs, is_waiting, "status", "cancelled")

    # print info of terminated jobs
    if (state$options$echo) {
        if (any(is_running)) {
            terminated <- state$jobs[is_running, get_sim_status_string("terminated", index, model, weather)]
            cat_line(terminated, col = "white", background_col = "red")
        }

        if (any(is_waiting)) {
            cancelled <- state$jobs[is_waiting, get_sim_status_string("cancelled", index, model, weather)]
            cat_line(cancelled, col = "white", background_col = "red")
        }
    }

    state
    # nocov end
}

schedule_next_sim <- function(state) {
    # cannot run more workers?
    if (sum(state$jobs$status == "running") >= state$options$num_parallel) return(state)

    # waiting -> running
    # always schedule only one new job
    if (any(ready <- state$jobs$status == "waiting")) {
        set(state$jobs, state$jobs$index[ready][1L], "status", "ready")
    }

    state
}

handle_sim_events <- function(state) {
    run <- state$jobs$status == "running"
    if (!any(run)) return(state)

    state$jobs[
        run & vlapply(process, function(x) !is.null(x) && !x$is_alive()),
        c(
            "stdout",
            "stderr",
            "exit_status",
            "result",
            "status",
            "end_time"
        ) := {
            res <- if (identical(state$options$backend, "api")) {
                Map(run__api_collect, process, output_dir, weather)
            } else {
                lapply(process, function(p) p$get_result())
            }

            # somehow get_exit_status() function may return NA after execution
            # of a (successful) command
            # ref: https://github.com/r-lib/processx/issues/220
            exit_status <- viapply(res, "[[", "exit_status")
            exit_status[is.na(exit_status)] <- 0L

            if (state$options$echo) {
                comp <- get_sim_status_string("completed", index, model, weather, exit_status)
                cli::cli_progress_output(paste0(comp, collapse = "\n"), id = state$progress, .envir = state$env)
            }
            cli::cli_progress_update(inc = .N, id = state$progress, .envir = state$env)

            status[exit_status == 0L] <- "completed"
            status[exit_status != 0L] <- "failed"
            # A controller stop is finalized output, but never a completed full simulation.
            stopped <- vlapply(res, function(x) {
                identical(x$api$phase, "stopped")
            })
            status[stopped] <- "stopped"

            list(stdout = lapply(res, function(r) unlist(r$run$stdout)),
                 stderr = lapply(res, function(r) unlist(r$run$stderr)),
                 exit_status = exit_status, result = res,
                 status = status, end_time = current()
            )
        }
    ]

    state
}

do_sim <- function(state) {
    # clean wd
    ready <- which(state$jobs$status == "ready")

    if (!length(ready)) {
        return(state)
    }

    state$jobs[ready, by = "index", c("status", "process", "start_time") := {
        process <- energyplus(eplus = energyplus_exe, model = model,
            weather = unlist(weather), output_dir = output_dir, annual = annual,
            design_day = design_day, wait = FALSE, echo = FALSE, resources = resources[[1L]],
                expand_obj = state$options$expand_obj,
                readvars = state$options$readvars,
                backend = state$options$api_setup %||% state$options$backend %||% "cli"
        )

        if (state$options$echo) {
            run <- get_sim_status_string("running", index, model, weather)
            cli::cli_progress_output(paste0(run, collapse = "\n"), id = state$progress, .envir = state$env)
        }

        list(status = "running", process = list(process), start_time = current())
    }]

    state
}

get_sim_status_string <- function(type, index, model, weather, exit_code = NULL) {
    status <- c("running", "completed", "cancelled", "terminated")
    if (length(type) == 1L && type %in% status) {
        type <- switch(type,
            running    = "RUNNING   ",
            completed  = "COMPLETED ",
            cancelled  = "CANCELLED ",
            terminated = "TERMINATED"
        )
        if (!is.null(exit_code)) type[exit_code != 0L] <- "FAILED    "
    }

    mes <- paste0(lpad(index, "0"), "|", type, " --> ",
        "[IDF]", surround(basename(model))
    )

    has_epw <- !vlapply(weather, function(x) is.null(x) || is.na(x))

    if (any(has_epw)) {
        mes[has_epw] <- paste0(mes, " + ", "[EPW]", surround(basename(unlist(weather[has_epw]))))
    }

    mes
}

are_all_completed <- function(state) {
    all(state$jobs$status %chin% c("completed", "failed", "stopped"))
}

pre_job_inputs <- function(model, weather, output_dir, design_day = FALSE, annual = FALSE, eplus = NULL) {
    assert_file_exists(model)
    model <- normalizePath(model, mustWork = TRUE)

    jobs <- data.table(model = model)

    # validate output_dir
    assert_character(output_dir, any.missing = FALSE, null.ok = TRUE)
    if (is.null(output_dir)) {
        if (anyDuplicated(model)) {
            abort("'model' cannot have any duplications when 'output_dir' is NULL.", "duplicated_sim")
        }
        output_dir <- normalizePath(dirname(model))
    } else {
        assert_same_len(model, output_dir)
        output_dir <- normalizePath(output_dir, mustWork = FALSE)
    }
    set(jobs, NULL, "output_dir", output_dir)
    if (anyDuplicated(jobs)) {
        abort(
            paste0("Duplication found in the combination of 'model' and 'output_dir'. ",
                "One model could not be run in the same output directory multiple times simultaneously."),
            "duplicated_sim"
        )
    }

    # validate weather
    assert_character(weather, null.ok = TRUE)
    if (is.null(weather)) {
        weather <- rep(NA_character_, length(model))
    } else if (length(weather) == 1L) {
        weather <- rep(weather, length(model))
    } else {
        assert_same_len(model, weather)
    }
    ddy <- is.na(weather)
    weather <- as.list(weather)
    weather[ddy] <- list(NULL)
    if (any(ddy) && length(weather) == 1L) weather <- list()
    set(jobs, NULL, "weather", weather)

    # validate eplus
    if (is.null(eplus)) {
        # in case there are same models, only parse version once
        jobs[, by = "model", `:=`(ver = list(get_ver_from_idf_path(model)))]

        is_miss <- viapply(jobs$ver, length) == 0L
        if (any(is_miss)) {
            msg <- paste0("  #", lpad(seq_along(unique(model[is_miss]))), "| ", surround(unique(model[is_miss])), collapse = "\n")
            abort(paste0("Missing version field in input IDF file. Failed to determine the ",
                "version of EnergyPlus to use:\n", msg), "miss_idf_ver")
        }
        jobs[, by = "model", `:=`(energyplus_exe = eplus_exe(ver[[1L]]))]
        set(jobs, NULL, "ver", NULL)
    } else {
        if (length(eplus) != 1L) assert_same_len(model, eplus)
        set(jobs, NULL, "energyplus_exe", vcapply(eplus, eplus_exe))
    }

    # validate design_day and annual
    assert_logical(design_day, any.missing = FALSE)
    assert_logical(annual, any.missing = FALSE)
    if (length(design_day) != 1L) assert_same_len(model, design_day)
    if (length(annual) != 1L) assert_same_len(model, annual)
    if (any(annual & design_day)) {
        abort("Cannot force both design-day-only simulation and annual simulation at the same time",
            "both_ddy_annual"
        )
    }
    set(jobs, NULL, c("design_day", "annual"), list(design_day, annual))

    set(jobs, NULL, "index", seq_len(nrow(jobs)))
    setcolorder(jobs, "index")

    jobs
}

#' Run simulations of EnergyPlus models.
#'
#' @param model A path (for `run_idf()`) or a vector of paths (for
#'        `run_multi()`) of EnergyPlus IDF or IMF files.
#'
#' @param weather A path (for `run_idf()`) or a vector of paths (for
#'        `run_multi()`) of EnergyPlus EPW weather files.
#'        If set to `NULL`, design-day-only simulation will be triggered,
#'        regardless of the `design-day` value.
#'        For `run_multi()`, `weather` can also be a single EPW file path. In
#'        this case, that weather will be used for all simulations; otherwise,
#'        `model` and `weather` should have the same length. You can set to
#'        design-day-only simulation to some specific simulations by setting the
#'        corresponding weather to `NA`.
#'
#' @param output_dir Output directory path (for `rum_idf()`) or paths (for
#'        `run_mult()`). If NULL, the directory of input model is used. For
#'        `run_multi()`, `output_dir`, if not `NULL`, should have the same
#'        length as `model`. Any duplicated combination of `model` and
#'        `output_dir` is prohibited.
#'
#' @param design_day Force design-day-only simulation. For `rum_multi()`,
#'        `design_day` can also be a logical vector which has the same length as
#'        `model`. Note that `design_day` and `annual` cannot be all `TRUE` at
#'        the same time. Default: `FALSE`.
#'
#' @param annual Force annual simulation. For `rum_multi()`,
#'        `annual` can also be a logical vector which has the same length as
#'        `model`. Note that `design_day` and `annual` cannot be all `TRUE` at
#'        the same time. Default: `FALSE`.
#'
#' @param backend Simulation engine: `"cli"` (default), experimental `"api"`,
#'   or an initialization `function(api, state)` which selects the API engine.
#'   The API backend supports stock EnergyPlus 26.1 and creates a fresh R process
#'   for each engine run. Preprocessing and result collection use the same pipeline.
#'   A function is executed once in the engine worker
#'   before simulation. Register requests and callbacks using the supplied managed
#'   objects. Capture serializable lexical configuration in a self-contained closure;
#'   Live API objects and native pointers cannot be transferred. The return value is ignored.
#'   Controller failures and worker interrupts yield failed
#'   results with condition metadata; an explicit stop yields a stopped result.
#'   Results record the resolved `"cli"` or `"api"` backend; API results also carry
#'   `$api` metadata. Existing CLI result fields
#'   remain available. Job orchestration may use additional R processes.
#'
#' @param expand_obj Whether to run ExpandObjects preprocessor before simulation.
#'        Default: TRUE.
#'
#' @param echo Only applicable when `wait` is `TRUE`. Whether to show standard
#'        output and error from EnergyPlus for `run_idf()` and simulation status
#'        for `run_multi()`. Default: `TRUE`.
#'
#' @param wait If `TRUE`, R will hang on and wait all EnergyPlus simulations
#'        finish. If `FALSE`, all EnergyPlus simulations are run in the
#'        background, and a [process][processx::process] object is returned.
#'
#' @param eplus An acceptable input (for `run_idf()`) or inputs (for
#'        `run_multi()`) of [use_eplus()] and [eplus_config()]. If `NULL`, which
#'        is the default, the version of EnergyPlus to use is determined by the
#'        version of input model. For `run_multi()`, `eplus`, if not `NULL`,
#'        should have the same length as `model`.
#'
#' @details
#'
#' `run_idf()` is a wrapper of EnergyPlus itself, plus various pre-processors
#' and post-processors which enables to run EnergyPlus model with different
#' options.
#'
#' `run_multi()` provides the functionality of running multiple models in
#' parallel.
#'
#' It is suggested to run simulations using [EplusJob] class and [EplusGroupJob]
#' class, which provide much more detailed controls on the simulation and also
#' methods to extract simulation outputs.
#'
#' @return
#'
#' * For `run_idf()`, if `wait` is `TRUE`, a named list containing:
#'
#' | No.  | Column        | Type                      | Description                                                          |
#' | ---: | -----         | -----                     | -----                                                                |
#' | 1    | `idf`         | `character(1)`            | Full path of input IDF file                                          |
#' | 2    | `epw`         | `character(1)` or `NULL`  | Full path of input EPW file                                          |
#' | 3    | `version`     | `character(1)`            | Version of called EnergyPlus                                         |
#' | 4    | `exit_status` | `integer(1)` or `NULL`    | Exit status of EnergyPlus. `NULL` if terminated or `wait` is `FALSE` |
#' | 5    | `start_time`  | `POSIXct(1)`              | Start of time of simulation                                          |
#' | 6    | `end_time`    | `POSIXct(1)` or `NULL`    | End of time of simulation. `NULL` if `wait` is `FALSE`               |
#' | 7    | `output_dir`  | `character(1)`            | Full path of simulation output directory                             |
#' | 8    | `energyplus`  | `character(1)`            | Full path of called EnergyPlus executable                            |
#' | 9    | `stdout`      | `character(1)` or `NULL`  | Standard output of EnergyPlus during simulation                      |
#' | 10   | `stderr`      | `character(1)` or `NULL`  | Standard error of EnergyPlus during simulation                       |
#' | 11   | `process`     | [r_process][callr::r_bg()] | A process object which called EnergyPlus and ran the simulation      |
#' | 12   | `backend`     | `character(1)`            | Resolved engine name, `"cli"` or `"api"`                            |
#'
#'   If `wait` is `FALSE`, the [R process][callr::r_bg()] is directly returned.
#'   You can get the results by calling `result <- proc$get_result()` (`proc` is
#'   the returned process). Please note that in this case, `result$process` will
#'   always be `NULL`. But you can easily assign it by running `result$process
#'   <- proc`
#'
#' * For `run_multi()`, if `wait` is TRUE, a
#'   [data.table][data.table::data.table()] containing:
#'
#'   | No.  | Column        | Type        | Description                                                      |
#'   | ---: | -----         | -----       | -----                                                            |
#'   | 1    | `index`       | `integer`   | Index of simulation                                              |
#'   | 2    | `status`      | `character` | Simulation status                                                |
#'   | 3    | `idf`         | `character` | Full path of input IDF file                                      |
#'   | 4    | `epw`         | `character` | Full path of input EPW file. `NA` for design-day-only simulation |
#'   | 5    | `version`     | `character` | Version of EnergyPlus                                            |
#'   | 6    | `exit_status` | `integer`   | Exit status of EnergyPlus. `NA` if terminated                    |
#'   | 7    | `start_time`  | `POSIXct`   | Start of time of simulation                                      |
#'   | 8    | `end_time`    | `POSIXct`   | End of time of simulation.                                       |
#'   | 9    | `output_dir`  | `character` | Full path of simulation output directory                         |
#'   | 10   | `energyplus`  | `character` | Full path of called EnergyPlus executable                        |
#'   | 11   | `stdout`      | `list`      | Standard output of EnergyPlus during simulation                  |
#'   | 12   | `stderr`      | `list`      | Standard error of EnergyPlus during simulation                   |
#'   | 13   | `backend`     | `character` | Resolved engine name, `"cli"` or `"api"`                         |
#'
#'   Both result forms also include API outcome metadata as `api` when the
#'   API engine is selected.
#'
#'   For column `status`, there are 4 possible values:
#'   - `"completed"`: the simulation job is completed successfully
#'   - `"failed"`: the simulation job ended with error
#'   - `"terminated"`: the simulation job started but was terminated
#'   - `"cancelled"`: the simulation job was cancelled, i.e. did not start at all
#'
#' * For `run_multi()`, if `wait` is `FALSE`, a [r_process][callr::r_bg()]
#'   object of background R process which handles all simulation jobs is
#'   returned. You can check if the jobs are completed using `$is_alive()` and
#'   get the final data.table using `$get_result()` method.
#'
#' @note
#' If your input model has external file dependencies, e.g. FMU, schedule files,
#' etc. `run_idf()` and `run_multi()` will not work if the output directory is
#' different that where the input mode lives. If this is the case, parse the
#' model using [read_idf()] and use [Idf$run()][Idf] or [eplus_job()] instead.
#' They are able to automatically change the paths of external files to absolute
#' paths or copy them into the output directory, based on your choice.
#'
#' @references
#' [Running EnergyPlus from Command Line (EnergyPlus GitHub Repository)](https://github.com/NatLabRockies/EnergyPlus/blob/develop/doc/running-energyplus-from-command-line.md)
#'
#' @examples
#' \dontrun{
#' idf_path <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
#'
#' if (is_avail_eplus("8.8")) {
#'     # run a single model
#'     epw_path <- file.path(
#'         eplus_config("8.8")$dir,
#'         "WeatherData",
#'         "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'     )
#'
#'     run_idf(idf_path, epw_path, output_dir = tempdir())
#'
#'     # run multiple model in parallel
#'     idf_paths <- file.path(eplus_config("8.8")$dir, "ExampleFiles",
#'         c("1ZoneUncontrolled.idf", "1ZoneUncontrolledFourAlgorithms.idf")
#'     )
#'     epw_paths <- rep(epw_path, times = 2L)
#'     output_dirs <- file.path(tempdir(), tools::file_path_sans_ext(basename(idf_paths)))
#'     run_multi(idf_paths, epw_paths, output_dir = output_dirs)
#' }
#' }
#' @rdname run_model
#' @seealso
#' [EplusJob] class and [ParametricJob] class which provide a more friendly
#' interface to run EnergyPlus simulations and collect outputs.
#' @author Hongyuan Jia
#' @export
run_idf <- function(
    model,
    weather,
    output_dir,
    design_day = FALSE,
    annual = FALSE,
    expand_obj = TRUE,
    wait = TRUE,
    echo = TRUE,
    eplus = NULL,
    backend = "cli"
) {
    configuration <- run__backend(backend)
    backend <- configuration$engine
    api_setup <- configuration$setup
    assert_flag(wait)

    if (is.null(weather)) {
        design_day <- TRUE
    }

    eplus <- get_eplus_loc_from_input(model, eplus)

    start_time <- current()

    post_fun <- function(res) {
        out <- list()
        out$idf <- normalizePath(model, mustWork = FALSE)
        if (is.na(res$file$epw)) {
            out["epw"] <- list(NULL)
        } else {
            out$epw <- normalizePath(weather, mustWork = FALSE)
        }
        out$version <- res$version
        out$exit_status <- res$exit_status
        out$start_time <- res$start_time
        out$end_time <- res$end_time
        out$output_dir <- res$output_dir
        out$energyplus <- normalizePath(
            file.path(
                res$energyplus,
                sprintf(
                    "energyplus%s",
                    c(".exe", "")[c(is_windows(), !is_windows())]
                )
            )
        )
        out$stdout <- unlist(res$run$stdout)
        # in case there is no stderr message
        out["stderr"] <- list(unlist(res$run$stderr))
        out$backend <- res$backend
        if (identical(res$backend, "api")) {
            out$api <- res$api
        }
        out
    }

    proc <- run_command(
        function(
            model,
            weather,
            output_dir,
            annual,
            design_day,
            echo,
            eplus,
            post_fun,
            expand_obj,
            backend
        ) {
            post_fun(energyplus(
                model = model,
                weather = weather,
                output_dir = output_dir,
                annual = annual,
                design_day = design_day,
                echo = echo,
                eplus = eplus,
                expand_obj = expand_obj,
                backend = backend
            ))
        },
        list(
            model = model,
            weather = weather,
            output_dir = output_dir,
            annual = annual,
            design_day = design_day,
            echo = echo,
            eplus = eplus,
            post_fun = post_fun,
            expand_obj = expand_obj,
            backend = api_setup %||% backend
        ),
        wd = tempdir(),
        call_r = TRUE,
        wait = wait,
        kill_tree = backend == "api"
    )

    if (!wait) {
        return(proc$process)
    }

    out <- proc$process$get_result()
    out$process <- proc$process
    out
}

#' @export
#' @rdname run_model
run_multi <- function(
    model,
    weather,
    output_dir,
    design_day = FALSE,
    annual = FALSE,
    expand_obj = TRUE,
    wait = TRUE,
    echo = TRUE,
    eplus = NULL,
    backend = "cli"
) {
    configuration <- run__backend(backend)
    backend <- configuration$engine
    api_setup <- configuration$setup
    # for weather, NA here means design-day-simulation
    assert_flag(expand_obj)
    assert_flag(wait)
    assert_flag(echo)

    jobs <- pre_job_inputs(model, weather, output_dir, design_day, annual, eplus)
    set(jobs, NULL, "resources", list())
    options <- list(num_parallel = eplusr_option("num_parallel"), echo = echo,
        expand_obj = expand_obj,
        readvars = TRUE,
        backend = backend,
        api_setup = api_setup
    )
    state <- list(jobs = jobs, options = options)

    if (wait) {
        post_process_sim_state(run_sim_event_loop(state))
    } else {
        # always echo in order to catch standard output and error
        state$options$echo <- TRUE
        callr::r_bg(
            function(state) post_process_sim_state(run_sim_event_loop(state)),
            args = list(state = state),
            package = TRUE,
            supervise = backend == "api"
        )
    }
}

post_process_sim_state <- function(state) {
    jobs <- copy(state$jobs)

    set(jobs, NULL, "idf", normalizePath(jobs$model, mustWork = FALSE))
    set(jobs, NULL, "model", NULL)

    set(jobs, NULL, "epw",
        apply2_chr(jobs$result, jobs$weather, function(res, weather) {
            if (is.na(res$file$epw)) {
                NA_character_
            } else {
                normalizePath(weather, mustWork = FALSE)
            }
        })
    )
    set(jobs, NULL, "weather", NULL)

    set(
        jobs,
        NULL,
        "version",
        vcapply(jobs$energyplus_exe, function(ep) {
            as.character(get_ver_from_eplus_path(dirname(ep)))
        })
    )

    set(jobs, NULL, "output_dir", vcapply(jobs$result, function(res) res$output_dir))

    setnames(jobs, "energyplus_exe", "energyplus")

    cols <- c("index", "status", "idf", "epw", "version", "exit_status",
        "start_time", "end_time", "output_dir", "energyplus", "stdout", "stderr")

    # Batch results retain a scalar engine label, never the initializer closure.
    set(jobs, NULL, "backend", state$options$backend %||% "cli")
    cols <- c(cols, "backend")
    if (identical(state$options$backend, "api")) {
        set(jobs, NULL, "api", lapply(jobs$result, "[[", "api"))
        cols <- c(cols, "api")
    }
    set(jobs, NULL, setdiff(names(jobs), cols), NULL)
    setcolorder(jobs, cols)
    jobs
}

# vim: set fdm=marker:
# Resolve the public selector once, keeping engine identity separate from frozen worker setup.
run__backend <- function(backend) {
    if (is.function(backend)) {
        # Validate before freezing so live native resources never enter the worker payload.
        run__check_setup(backend)
        return(list(engine = "api", setup = run__prepare_setup(backend)))
    }
    if (
        !is.character(backend) ||
            length(backend) != 1L ||
            is.na(backend) ||
            !backend %in% c("cli", "api")
    ) {
        abort(
            "'backend' must be 'cli', 'api', or an initialization function(api, state).",
            "backend"
        )
    }
    list(engine = backend, setup = NULL)
}

# Inspect captured configuration while leaving namespace references and data.table selfrefs portable.
run__check_setup <- function(setup) {
    seen <- list()
    functions <- list()
    # Follow the serialized object graph, recording cycles before visiting dependencies.
    inspect <- function(object) {
        if (
            inherits(
                object,
                c(
                    "EnergyPlusAPI",
                    "EnergyPlusState",
                    "EnergyPlusHandle",
                    "EnergyPlusFunctional",
                    "EnergyPlusSession",
                    "EnergyPlusPluginContext",
                    "connection"
                )
            ) ||
                typeof(object) %in% c("externalptr", "weakref")
        ) {
            abort(
                "Create API objects and native handles inside the 'backend' initialization function; live objects cannot be transferred to a worker.",
                "backend"
            )
        }
        if (is.function(object)) {
            if (any(vlapply(functions, function(x) identical(x, object)))) {
                return(invisible(NULL))
            }
            functions[[length(functions) + 1L]] <<- object
            env <- environment(object)
            if (is.null(env) || isNamespace(env) || identical(env, baseenv())) {
                return(invisible(NULL))
            }
            # Inspect free variables, not unrelated jobs or pointers in the surrounding caller frame.
            for (name in run__setup_globals(object)) {
                if (exists(name, env, inherits = TRUE)) {
                    inspect(get(name, env, inherits = TRUE))
                }
            }
        } else if (is.environment(object)) {
            if (
                isNamespace(object) ||
                    identical(object, globalenv()) ||
                    identical(object, baseenv()) ||
                    identical(object, emptyenv()) ||
                    any(vlapply(seen, function(x) identical(x, object)))
            ) {
                return(invisible(NULL))
            }
            seen[[length(seen) + 1L]] <<- object
            for (name in ls(object, all.names = TRUE)) {
                inspect(get(name, object, inherits = FALSE))
            }
            # Ordinary environment parents are serialized too; special named environments are not.
            inspect(parent.env(object))
        } else if (is.list(object)) {
            for (value in object) {
                inspect(value)
            }
        }
        # data.table repairs its internal self-reference when deserialized; it is not a user native handle.
        for (attribute in attributes(object)[setdiff(
            names(attributes(object)),
            ".internal.selfref"
        )]) {
            inspect(attribute)
        }
        invisible(NULL)
    }
    inspect(setup)
}

# Keep the experimental ABI gate independent of normal CLI installation discovery.
run__api_version <- function(version) {
    if (as.character(standardize_ver(version)) != "26.1.0") {
        abort("The API backend requires stock EnergyPlus 26.1.", "api_version")
    }
    invisible(NULL)
}

# Own exactly one engine lifecycle inside a fresh process; only plain result metadata leaves it.
run__api_engine <- function(directory, args, api_setup = NULL) {
    api <- NULL
    state <- NULL
    stage <- "load"
    metadata <- list(
        version = NA_character_,
        api_version = NA_character_,
        library = NA_character_,
        pid = Sys.getpid(),
        phase = "failed",
        stop_reason = NULL,
        native_exit_status = NA_integer_,
        error = NULL
    )
    code <- 1L
    on.exit(if (!is.null(api)) try(api$close(), silent = TRUE), add = TRUE)
    # Preserve failure diagnostics while leaving quarantined native ownership to process exit.
    capture <- function(condition) {
        metadata$error <<- list(
            class = class(condition),
            message = conditionMessage(condition)
        )
        metadata$stop_reason <<- if (inherits(condition, "interrupt")) {
            "interrupt"
        } else {
            paste0(stage, "_error")
        }
        if (!is.null(state)) {
            status <- api$state_manager$status(state)
            metadata$state_phase <<- status$phase
            if (length(status$plugins)) {
                metadata$plugins <<- status$plugins
            }
            metadata$phase <<- if (identical(status$phase, "quarantined")) {
                "quarantined"
            } else {
                "failed"
            }
            metadata$native_exit_status <<- status$exit_code %||% NA_integer_
            if (
                isTRUE(
                    status$stop_reason %in%
                        c(
                            "callback_error",
                            "plugin_initialize_error",
                            "plugin_finalize_error"
                        )
                ) &&
                    !inherits(condition, "interrupt")
            ) {
                metadata$stop_reason <<- status$stop_reason
            }
        }
        code <<- if (inherits(condition, "interrupt")) 130L else 1L
        invisible(NULL)
    }
    tryCatch(
        {
            api <- eplus_api(directory)
            info <- api$info()
            metadata[names(info)[names(info) %in% names(metadata)]] <- info[
                names(info) %in% names(metadata)
            ]
            state <- api$state_manager$new_state()
            api$runtime$set_console_output_status(state, TRUE)
            # Every zone boundary enters R so an OS interrupt can request an orderly native stop.
            api__host_callback(
                api,
                state,
                "callback_begin_zone_timestep_before_init_heat_balance",
                function(s) invisible(NULL)
            )
            stage <- "setup"
            if (!is.null(api_setup)) {
                setup_result <- api_setup(api, state)
                # A package-owned session bridge sends its final snapshot after the cleanup handler.
                # Ordinary user setup return values remain ignored.
                if (inherits(setup_result, "EnergyPlusSessionBridge")) {
                    on.exit(setup_result$finish(metadata), add = TRUE)
                }
            }
            stage <- "engine"
            code <- api$runtime$run_energyplus(state, args)
            status <- api$state_manager$status(state)
            metadata$phase <- status$phase
            metadata$stop_reason <- status$stop_reason
            metadata$native_exit_status <- code
            if (length(status$plugins)) {
                metadata$plugins <- status$plugins
            }
            # A stopped simulation has partial finalized results and must not report full completion.
            if (identical(status$phase, "stopped")) code <- 1L
        },
        error = capture,
        interrupt = capture
    )
    # Preserve the session command reason alongside the underlying native stop reason.
    if (
        exists("setup_result", inherits = FALSE) &&
            inherits(setup_result, "EnergyPlusSessionBridge")
    ) {
        metadata <- setup_result$metadata(metadata)
    }
    list(exit_status = code, api = metadata)
}

# Capture native stdout/stderr through process pipes rather than accumulating callback messages in RAM.
run__api_command <- function(directory, args, wd, echo, api_setup) {
    run <- run_command(
        function(directory, args, api_setup) {
            run__api_engine(directory, args, api_setup)
        },
        list(directory = directory, args = args, api_setup = api_setup),
        wd = wd,
        wait = TRUE,
        echo = echo,
        call_r = TRUE,
        kill_tree = TRUE
    )
    result <- tryCatch(run$process$get_result(), error = function(condition) {
        run__api_failure(condition, wd, NULL)
    })
    run$exit_status <- result$exit_status
    run$api <- result$api
    if (!is.null(result$api$error)) {
        run$stderr <- c(run$stderr, result$api$error$message)
    }
    run
}

# Include replacement-assignment reads that codetools treats as local bindings.
run__setup_globals <- function(fun) {
    # Analyze one lexical scope at a time, so worker-created locals do not capture caller siblings.
    scope <- function(parameters, expression, outer = character()) {
        locals <- parameters
        roots <- character()
        children <- list()
        # First collect whole-variable bindings; nested functions define separate scopes.
        declarations <- function(node) {
            if (!is.call(node)) {
                return(invisible(NULL))
            }
            operator <- if (is.symbol(node[[1L]])) {
                as.character(node[[1L]])
            } else {
                ""
            }
            if (
                operator %in% c("function", "quote", "substitute", "expression")
            ) {
                return(invisible(NULL))
            }
            if (operator %in% c("<-", "=") && is.symbol(node[[2L]])) {
                locals <<- c(locals, as.character(node[[2L]]))
            }
            if (operator == "for") {
                locals <<- c(locals, as.character(node[[2L]]))
            }
            for (index in seq_along(node)[-1L]) {
                if (!identical(node[[index]], quote(expr = ))) {
                    declarations(node[[index]])
                }
            }
            invisible(NULL)
        }
        declarations(expression)
        # Replacement assignment reads its root object before updating the local binding.
        references <- function(node) {
            if (!is.call(node)) {
                return(invisible(NULL))
            }
            operator <- if (is.symbol(node[[1L]])) {
                as.character(node[[1L]])
            } else {
                ""
            }
            if (operator %in% c("quote", "substitute", "expression")) {
                return(invisible(NULL))
            }
            if (operator == "function") {
                children[[length(children) + 1L]] <<- node
                return(invisible(NULL))
            }
            if (operator %in% c("<-", "=", "<<-") && is.call(node[[2L]])) {
                root <- node[[2L]]
                while (is.call(root) && length(root) >= 2L) {
                    root <- root[[2L]]
                }
                if (is.symbol(root)) roots <<- c(roots, as.character(root))
            }
            for (index in seq_along(node)[-1L]) {
                if (!identical(node[[index]], quote(expr = ))) {
                    references(node[[index]])
                }
            }
            invisible(NULL)
        }
        references(expression)
        result <- setdiff(roots, c(locals, outer))
        for (child in children) {
            result <- c(
                result,
                scope(names(child[[2L]]), child[[3L]], c(outer, locals))
            )
        }
        unique(result)
    }
    unique(c(
        codetools::findGlobals(fun, merge = TRUE),
        scope(names(formals(fun)), body(fun))
    ))
}

# Freeze each controller's lexical dependencies so callr does not serialize its entire caller frame.
run__prepare_setup <- function(setup) {
    if (is.null(setup)) {
        return(NULL)
    }
    originals <- list()
    copies <- list()
    populated <- list()
    pending <- list()
    bindings <- list()
    scheduled <- new.env(parent = emptyenv())
    # Reuse each lexical frame, copying only referenced bindings rather than the caller's whole frame.
    scope <- function(env) {
        if (
            identical(env, baseenv()) ||
                identical(env, emptyenv()) ||
                isNamespace(env)
        ) {
            return(env)
        }
        index <- which(vlapply(originals, function(x) identical(x, env)))
        if (length(index)) {
            return(copies[[index[1L]]])
        }
        copy <- new.env(parent = baseenv())
        originals[[length(originals) + 1L]] <<- env
        copies[[length(copies) + 1L]] <<- copy
        parent.env(copy) <- scope(parent.env(env))
        copy
    }
    # Locate the actual binding owner so closures in sibling frames share inherited mutable state too.
    binding_owner <- function(name, env) {
        while (!exists(name, env, inherits = FALSE)) {
            env <- parent.env(env)
        }
        env
    }
    # Freeze metadata dependencies too; data.table alone owns and repairs its native self-reference.
    freeze_attributes <- function(value) {
        result <- attributes(value)
        for (name in setdiff(names(result), ".internal.selfref")) {
            result[[name]] <- freeze(result[[name]])
        }
        result
    }
    # Freeze lexical dependencies throughout ordinary containers while preserving cycles and aliases.
    freeze <- function(value) {
        if (is.function(value)) {
            env <- environment(value)
            if (is.null(env) || isNamespace(env) || identical(env, baseenv())) {
                return(value)
            }
            index <- which(vlapply(originals, function(x) identical(x, value)))
            if (length(index)) {
                return(copies[[index[1L]]])
            }
            copy <- value
            environment(copy) <- scope(env)
            # Memoize before recursing into helper containers and mutually recursive functions.
            originals[[length(originals) + 1L]] <<- value
            copies[[length(copies) + 1L]] <<- copy
            copy_index <- length(copies)
            # Register the closure before visiting metadata, which may refer back to its lexical frame.
            attributes(copy) <- freeze_attributes(value)
            copies[[copy_index]] <<- copy
            for (name in run__setup_globals(value)) {
                if (exists(name, env, inherits = TRUE)) {
                    owner <- binding_owner(name, env)
                    if (isNamespace(owner) || identical(owner, baseenv())) {
                        next
                    }
                    target <- scope(owner)
                    key <- paste(
                        which(vlapply(originals, function(x) {
                            identical(x, owner)
                        }))[[1L]],
                        name
                    )
                    # Defer binding installation until every closure has its final attribute graph.
                    if (
                        !exists(name, target, inherits = FALSE) &&
                            !exists(key, scheduled, inherits = FALSE)
                    ) {
                        assign(key, TRUE, scheduled)
                        bindings[[length(bindings) + 1L]] <<- list(
                            owner = owner,
                            name = name,
                            target = target
                        )
                    }
                }
            }
            return(copy)
        }
        if (is.environment(value)) {
            if (
                isNamespace(value) ||
                    identical(value, globalenv()) ||
                    identical(value, baseenv()) ||
                    identical(value, emptyenv())
            ) {
                return(value)
            }
            copy <- scope(value)
            if (any(vlapply(populated, function(x) identical(x, value)))) {
                return(copy)
            }
            # Queue full fields until closures have their final attributes, retaining metadata cycles safely.
            populated[[length(populated) + 1L]] <<- value
            pending[[length(pending) + 1L]] <<- value
            return(copy)
        }
        if (is.list(value)) {
            copy <- lapply(value, freeze)
            attributes(copy) <- freeze_attributes(value)
            return(copy)
        }
        attributes(value) <- freeze_attributes(value)
        value
    }
    # Install lexical dependencies only after recursive closure metadata has finished freezing.
    populate_binding <- function(binding) {
        if (exists(binding$name, binding$target, inherits = FALSE)) {
            return(invisible(NULL))
        }
        if (bindingIsActive(binding$name, binding$owner)) {
            fun <- activeBindingFunction(binding$name, binding$owner)
            run__check_setup(fun)
            makeActiveBinding(binding$name, freeze(fun), binding$target)
        } else {
            assign(
                binding$name,
                freeze(get(binding$name, binding$owner, inherits = FALSE)),
                binding$target
            )
        }
        invisible(NULL)
    }
    # Fill mutable environment shells after immutable closure copies are fully memoized.
    populate <- function(value) {
        copy <- scope(value)
        for (name in ls(value, all.names = TRUE)) {
            if (bindingIsActive(name, value)) {
                binding <- activeBindingFunction(name, value)
                run__check_setup(binding)
                if (!exists(name, copy, inherits = FALSE)) {
                    makeActiveBinding(name, freeze(binding), copy)
                }
            } else {
                assign(
                    name,
                    freeze(get(name, value, inherits = FALSE)),
                    envir = copy
                )
            }
            if (bindingIsLocked(name, value)) lockBinding(name, copy)
        }
        # Keep the lazy global-frame snapshot: the live global environment is not portable.
        if (!identical(parent.env(value), globalenv())) {
            parent.env(copy) <- freeze(parent.env(value))
        }
        attributes(copy) <- freeze_attributes(value)
        if (environmentIsLocked(value)) {
            lockEnvironment(copy, bindings = FALSE)
        }
        invisible(NULL)
    }
    result <- freeze(setup)
    while (length(bindings) || length(pending)) {
        if (length(bindings)) {
            binding <- bindings[[1L]]
            bindings[[1L]] <- NULL
            populate_binding(binding)
        } else {
            value <- pending[[1L]]
            pending[[1L]] <- NULL
            populate(value)
        }
    }
    result
}

# Report an unavailable pipeline result without inventing native identity or cleanup outcomes.
run__api_failure <- function(
    condition,
    output_dir,
    weather,
    terminated = FALSE
) {
    # OS interrupt conditions can have no message; retain a usable diagnostic and list-column value.
    message <- conditionMessage(condition)
    if (!length(message) || !nzchar(message[[1L]])) {
        message <- if (inherits(condition, "interrupt")) {
            "Simulation interrupted."
        } else {
            "Simulation pipeline failed."
        }
    }
    list(
        exit_status = 1L,
        output_dir = output_dir,
        backend = "api",
        file = list(
            epw = if (is.null(weather) || is.na(weather)) {
                NA_character_
            } else {
                basename(weather)
            }
        ),
        run = list(stdout = NULL, stderr = message),
        api = list(
            version = NA_character_,
            api_version = NA_character_,
            library = NA_character_,
            pid = NA_integer_,
            native_exit_status = NA_integer_,
            phase = if (terminated) "terminated" else "failed",
            stop_reason = if (terminated) {
                "process_terminated"
            } else {
                "worker_error"
            },
            error = list(
                class = class(condition),
                message = message
            )
        )
    )
}

# Convert a dead API pipeline into a failed result while retaining successful metadata verbatim.
run__api_collect <- function(process, output_dir, weather, terminated = FALSE) {
    tryCatch(process$get_result(), error = function(condition) {
        run__api_failure(condition, output_dir, weather, terminated)
    })
}

# A killed group cannot recover completed case results; retain inputs and mark completion unknown.
run__api_failed_state <- function(state, condition, terminated = FALSE) {
    jobs <- copy(state$jobs)
    results <- Map(
        function(directory, weather) {
            run__api_failure(condition, directory, weather, terminated)
        },
        jobs$output_dir,
        jobs$weather
    )
    set(jobs, NULL, "result", results)
    set(jobs, NULL, "status", if (terminated) "terminated" else "failed")
    set(jobs, NULL, "exit_status", 1L)
    set(jobs, NULL, "stdout", list(lapply(results, function(x) x$run$stdout)))
    set(jobs, NULL, "stderr", list(lapply(results, function(x) x$run$stderr)))
    set(jobs, NULL, "start_time", as.POSIXct(NA))
    set(jobs, NULL, "end_time", current())
    state$jobs <- jobs
    state
}

# Share the bounded tree shutdown across jobs, groups and foreground command cleanup.
run__api_kill <- function(process) {
    tryCatch(
        {
            process$kill_tree()
            process$wait(1000)
            !process$is_alive()
        },
        error = function(condition) FALSE
    )
}
