#' @importFrom callr r_bg
#' @importFrom checkmate assert_flag assert_file_exists assert_directory_exists
#' @importFrom checkmate assert_logical
#' @importFrom cli cat_line
#' @importFrom crayon red
#' @importFrom data.table data.table setattr setnames
#' @importFrom lubridate with_tz
#' @importFrom tools file_path_sans_ext
#' @importFrom processx process
#' @importFrom progress progress_bar
#' @importFrom tools file_path_sans_ext
NULL

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
#' @note
#'
#' These functions are not vectorized, which means that only a single file path
#' can be specified at a time.
#'
#' @param ver An acceptable EnergyPlus version or an EnergyPlus installation
#'        directory
#' @param ... File paths passed to [base::file.path()].
#' @param file A single string of file name.
#' @param .strict If `TRUE`, an error will be issued if the specified file does
#'        not exist
#'
#' @examples
#' \dontrun{
#' path_eplus(8.8, "Energy+.idd")
#'
#' path_eplus_processor(8.8, "EPMacro", .strict = TRUE)
#' path_eplus_processor(8.8, "PreProcess", "GrndTempCalc", "Slab", .strict = TRUE)
#'
#' path_eplus_example(8.8, "1ZoneUncontrolled.idf")
#' path_eplus_example(8.8, "BasicFiles/Exercise1A.idf")
#'
#' path_eplus_weather(8.8, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.ddy")
#'
#' path_eplus_dataset(8.8, "Boilers.idf")
#' path_eplus_dataset(8.8, "FMUs/MoistAir.fmu")
#' }
#' @export
#' @author Hongyuan Jia
path_eplus <- function(ver, ..., .strict = FALSE) {
    inputs <- c(...)
    dir <- suppressMessages(use_eplus(ver))$dir

    path <- normalizePath(do.call(file.path, as.list(c(dir, inputs))), mustWork = FALSE)
    if (.strict && !file.exists(path)) {
        abort(sprintf(
            "Failed to find file '%s' under EnergyPlus installation directory ('%s').",
            substring(gsub(dir, "", path, fixed = TRUE), 2L), dir
        ))
    }
    path
}

#' @export
#' @rdname path_eplus
path_eplus_processor <- function(ver, ..., .strict = FALSE) {
    inputs <- c(...)
    exe <- if (is_windows()) ".exe" else ""
    inputs[length(inputs)] <- sprintf("%s%s", inputs[length(inputs)], exe)
    path_eplus(ver, inputs, .strict = .strict)
}

#' @export
#' @rdname path_eplus
path_eplus_example <- function(ver, file, .strict = FALSE) {
    path_eplus(ver, "ExampleFiles", file, .strict = .strict)
}

#' @export
#' @rdname path_eplus
path_eplus_weather <- function(ver, file, .strict = FALSE) {
    path_eplus(ver, "WeatherData", file, .strict = .strict)
}

#' @export
#' @rdname path_eplus
path_eplus_dataset <- function(ver, file, .strict = FALSE) {
    path_eplus(ver, "DataSets", file, .strict = .strict)
}

#' Clean working directory of a previous EnergyPlus simulation
#'
#' Clean working directory of an EnergyPlus simulation by deleting all input and
#' output files of previous simulation.
#'
#' @param path An `.idf` or `.imf` file path.
#'
#' @details
#' `clean_wd()` imitates the same process that EnergyPlus does whenever a new
#'     simulation is getting to start. It deletes all related output files that
#'     have the same name prefix as the input path. The input model itself and
#'     any weather file are not deleted. `clean_wd()` is called internally when
#'     running EnergyPlus models using [run_idf()] and [run_multi()].
#'
#' @examples
#' \dontrun{
#' # run a test simulation
#' idf_path <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
#' epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData",
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
# clean_wd {{{
clean_wd <- function(path, suffix_type = c("C", "L", "D")) {
    assert_string(path)
    wd <- dirname(path)

    suffix_type <- match.arg(suffix_type)

    out_files <- file.path(wd, unlist(get_eplus_output_name(path, suffix_type), use.names = FALSE))

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
        individuals <- file.path(wd, setdiff(individuals, c("in.epw", sprintf("in.%s", tools::file_ext(path)))))
    } else {
        individuals <- file.path(wd, individuals)
    }

    targets <- c(out_files, individuals)

    unlink(targets[file.exists(targets)])
}

get_eplus_output_name <- function(path, suffix_type = c("C", "L", "D")) {
    suffix_type <- match.arg(suffix_type)

    if (is.null(path)) {
        without_ext <- "eplus"
    } else {
        without_ext <- basename(tools::file_path_sans_ext(path))
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
        # log file for PerformancemidcisionTradeoffs
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
        sqlite = list(pre = pre, ext = ".sqlite"),
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
        files$shading$pre         <-  paste0(pre, "-shading")
        files$delight$pre     <-  paste0(pre, "-delight")
        files$delight$ext     <-  c(".in", ".out", ".dfdmp", ".eldmp")
    }

    lapply(files, function(f) paste0(f$pre, f$ext))
}
# }}}

#' Run simulations of EnergyPlus models.
#'
#' @param model A path (for `run_idf()`) or a vector of paths (for
#'        `run_multi()`) of EnergyPlus IDF or IMF files.
#'
#' @param weather A path (for `run_idf()`) or a vector of paths (for
#'        `run_multi()`) of EnergyPlus EPW weather files. For `run_multi()`,
#'        `weather` can also be a single EPW file path. In this case, that
#'        weather will be used for all simulations; otherwise, `model` and
#'        `weather` should have the same length.
#'
#' @param output_dir Output directory path (for `rum_idf()`) or paths (for
#'        `run_mult()`). If NULL, the directory of input model is used. For
#'        `run_multi()`, `output_dir`, if not `NULL`, should have the same
#'        length as `model`. Any duplicated combination of `model` and
#'        `output_dir` is prohibited.
#'
#' @param design_day Force design-day-only simulation. For `rum_multi()`,
#'        `design_day` can also be a logical vector which has the same length as
#'        `model`. Default: `FALSE`.
#'
#' @param annual Force design-day-only simulation. For `rum_multi()`,
#'        `annual` can also be a logical vector which has the same length as
#'        `model`. Note that `design_day` and `annual` cannot be all `TRUE` at
#'        the same time. Default: `FALSE`.
#'
#' @param expand_obj Whether to run ExpandObject preprocessor before simulation.
#'        Default: `TRUE`.
#'
#' @param echo Only applicable when `wait` is `TRUE`. Whether to show standard
#'        output and error from EnergyPlus command line interface for
#'        `run_idf()` and simulation status for `run_multi()`. Default: `TRUE`.
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
#' `run_idf()` is a wrapper of EnergyPlus command line interface which enables to
#' run EnergyPlus model with different options.
#'
#' `run_multi()` provides the functionality of running multiple models in
#' parallel.
#'
#' `run_idf()` and `run_multi()` currently only support EnergyPlus v8.3 and
#' above. This is because eplusr uses EnergyPlus command line interface which is
#' a new feature as of EnergyPlus v8.3.
#'
#' It is suggested to run simulations using [EplusJob] class and [EplusGroupJob]
#' class, which provide much more detailed controls on the simulation and also
#' methods to extract simulation outputs.
#'
#' @return
#'
#' * For `run_idf()`, a named list of 11 elements:
#'
#'   | No.  | Column        | Type                         | Description                                                                          |
#'   | ---: | -----         | -----                        | -----                                                                                |
#'   | 1    | `idf`         | `character(1)`               | Full path of input IDF file                                                          |
#'   | 2    | `epw`         | `character(1)` or `NULL`     | Full path of input EPW file                                                          |
#'   | 3    | `version`     | `character(1)`               | Version of called EnergyPlus                                                         |
#'   | 4    | `exit_status` | `integer(1)` or `NULL`       | Exit status of EnergyPlus. `NULL` if terminated or `wait` is `FALSE`                 |
#'   | 5    | `start_time`  | `POSIXct(1)`                 | Start of time of simulation                                                          |
#'   | 6    | `end_time`    | `POSIXct(1)` or `NULL`       | End of time of simulation. `NULL` if `wait` is `FALSE`                               |
#'   | 7    | `output_dir`  | `character(1)`               | Full path of simulation output directory                                             |
#'   | 8    | `energyplus`  | `character(1)`               | Full path of called EnergyPlus executable                                            |
#'   | 9    | `stdout`      | `character(1)` or `NULL`     | Standard output of EnergyPlus during simulation                                      |
#'   | 10   | `stderr`      | `character(1)` or `NULL`     | Standard error of EnergyPlus during simulation                                       |
#'   | 11   | `process`     | [process][processx::process] | A [process][processx::process] object which called EnergyPlus and ran the simulation |
#'
#' * For `rum_multi()`, if `wait` is TRUE, a
#'   [data.table][data.table::data.table()] of 12 columns:
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
#' @references
#' [Running EnergyPlus from Command Line (EnergyPlus GitHub Repository)](https://github.com/NREL/EnergyPlus/blob/develop/doc/running-energyplus-from-command-line.md)
#'
#' @examples
#' \dontrun{
#' idf_path <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
#'
#' if (is_avail_eplus(8.8)) {
#'     # run a single model
#'     epw_path <- file.path(
#'         eplus_config(8.8)$dir,
#'         "WeatherData",
#'         "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'     )
#'
#'     run_idf(idf_path, epw_path, output_dir = tempdir())
#'
#'     # run multiple model in parallel
#'     idf_paths <- file.path(eplus_config(8.8)$dir, "ExampleFiles",
#'         c("1ZoneUncontrolled.idf", "1ZoneUncontrolledFourAlgorithms.idf")
#'     )
#'     epw_paths <- rep(epw_path, times = 2L)
#'     output_dirs <- file.path(tempdir(), tools::file_path_sans_ext(basename(idf_paths)))
#'     run_multi(idf_paths, epw_paths, output_dir = output_dirs)
#' }
#' }
#' @rdname run_model
#' @seealso [EplusJob] class and [ParametricJob] class which provide
#' a more friendly interface to run EnergyPlus simulations and collect outputs.
#' @author Hongyuan Jia
#' @export
# run_idf {{{
run_idf <- function (model, weather, output_dir, design_day = FALSE,
                     annual = FALSE, expand_obj = TRUE, wait = TRUE, echo = TRUE, eplus = NULL) {

    model <- normalizePath(model, mustWork = TRUE)
    if (!is.null(weather)) weather <- normalizePath(weather, mustWork = TRUE)

    eplus <- eplus %||% as.character(get_idf_ver(read_lines(model)))
    if (!length(eplus)) {
        abort(paste0("Missing version field in input IDF file. ",
            "Failed to determine the version of EnergyPlus to use."),
            "miss_idf_ver"
        )
    }

    energyplus_exe <- eplus_exe(eplus)

    # get output directory
    if (is.null(output_dir)) output_dir <- dirname(model)
    output_dir <- normalizePath(output_dir, mustWork = FALSE)
    if (!dir.exists(output_dir)) {
        tryCatch(dir.create(output_dir, recursive = TRUE),
            warning = function (w) abort(paste0("Failed to create output directory: ", surround(output_dir)), "create_output_dir")
        )
    }

    # copy input files
    loc_m <- copy_run_files(model, output_dir)
    if (is.null(weather)) {
        loc_w <- NULL
    } else {
        loc_w <- copy_run_files(weather, output_dir)
    }

    # clean wd
    clean_wd(loc_m)

    # Let energyplus commandline interface itself to handle output file
    # directory. This is the easiest way to external file dependencies, instead
    # of doing it on the R side.
    # See #344
    res <- energyplus(energyplus_exe, model, weather, output_dir = output_dir,
        annual = annual, design_day = design_day, expand_obj = expand_obj,
        wait = wait, echo = echo)

    res$idf <- model
    # in case no weather is given
    res["epw"] <- list(weather)
    res$version <- as.character(eplus_config(eplus)$version)

    res[c("idf", "epw", "version", "exit_status", "start_time", "end_time", "output_dir",
        "energyplus", "stdout", "stderr", "process")]
}
# }}}

#' @export
#' @rdname run_model
# run_multi {{{
run_multi <- function (model, weather, output_dir, design_day = FALSE,
                       annual = FALSE, expand_obj = TRUE, wait = TRUE, echo = TRUE, eplus = NULL) {
    assert_flag(wait)
    assert_flag(echo)
    assert_logical(design_day, any.missing = FALSE)
    assert_logical(annual, any.missing = FALSE)
    assert_logical(expand_obj, any.missing = FALSE)

    if (length(model) != 1L) {
        if (!is.null(weather) && length(weather) != 1L) {
            assert_same_len(model, weather)
        }
        if (!is.null(eplus) && length(eplus) != 1L) {
            assert_same_len(model, eplus)
        }
        if (length(design_day) != 1L) {
            assert_same_len(model, design_day)
        }
        if (length(annual) != 1L) {
            assert_same_len(model, annual)
        }
        if (length(expand_obj) != 1L) {
            assert_same_len(model, expand_obj)
        }
    }

    if (any(annual & design_day)) {
        abort("Cannot force both design-day-only simulation and annual simulation at the same time",
            "both_ddy_annual"
        )
    }

    model <- normalizePath(model, mustWork = TRUE)

    if (is.null(weather)) {
        input_weather <- rep(NA_character_, length(model))
        weather <- list(NULL)
    } else {
        if (length(weather) == 1L) weather <- rep(weather, length(model))
        ddy <- is.na(weather)
        input_weather <- weather
        input_weather[!ddy] <- normalizePath(input_weather[!ddy], mustWork = TRUE)
        weather <- as.list(weather)
        weather[ddy] <- list(NULL)
    }

    if (is.null(eplus)) {
        ver_list <- lapply(model, function (x) as.character(get_idf_ver(read_lines(x))))
        ver_miss <- viapply(ver_list, length) == 0L
        if (any(ver_miss)) {
            msg <- paste0("  #", lpad(seq_along(model)[ver_miss]), "| ", surround(model[ver_miss]),
                collapse = "\n")
            abort(paste0("Missing version field in input IDF file. Failed to determine the ",
                "version of EnergyPlus to use:\n", msg), "miss_idf_ver")
        }

        ver <- unlist(ver_list)
        energyplus_exe <- vcapply(ver, eplus_exe)
        ver <- vcapply(ver, function (v) as.character(eplus_config(v)$version))
    } else {
        energyplus_exe <- vcapply(eplus, eplus_exe)
        ver <- vcapply(eplus, function (v) as.character(eplus_config(v)$version))
    }

    if (anyDuplicated(model) & is.null(output_dir)) {
        abort("'model' cannot have any duplications when 'output_dir' is NULL.", "duplicated_sim")
    }

    if (is.null(output_dir)) {
        output_dir <- dirname(model)
    } else {
        assert_same_len(model, output_dir)
    }

    output_dir <- normalizePath(output_dir, mustWork = FALSE)

    jobs <- data.table::data.table(input_model = model, output_dir = output_dir)

    if (anyDuplicated(jobs))
        stop(paste0("Duplication found in the combination of 'model' and 'output_dir'. ",
            "One model could not be run in the same output directory multiple times simultaneously."),
            "duplicated_sim"
        )

    d <- unique(output_dir[!dir.exists(output_dir)])
    created <- vapply(d, dir.create, logical(1L), showWarnings = FALSE, recursive = TRUE)
    if (any(!created)) {
        abort(paste0("Failed to create output directory:\n",
            paste0(surround(d[!created]), collapse = "\n")
        ))
    }

    jobs[, `:=`(
        energyplus = energyplus_exe,
        model = copy_run_files(model, output_dir), version = ver,
        index = .I, annual = annual, design_day = design_day, expand_obj = expand_obj
    )]

    if (is.null(weather)) {
        set(jobs, NULL, c("input_weather", "weather"), list(NA_character_, list(NULL)))
    } else {
        if (any(!is.na(input_weather))) {
            weather[!is.na(input_weather)] <- as.list(
                copy_run_files(
                    unlist(weather[!is.na(input_weather)]),
                    output_dir[!is.na(input_weather)]
                )
            )
        }
        set(jobs, NULL, c("input_weather", "weather"), list(input_weather, weather))
    }

    options <- list(num_parallel = eplusr_option("num_parallel"), echo = echo)

    if (wait) {
        run_parallel_jobs(jobs, options)
    } else {
        # always echo in order to catch standard output and error
        options$echo <- TRUE
        callr::r_bg(function (jobs, options) {
            utils::getFromNamespace("run_parallel_jobs", "eplusr")(jobs, options)
        }, args = list(jobs = jobs, options = options))
    }
}
# }}}

# run_parallel_jobs {{{
# reference: https://github.com/r-lib/revdepcheck/blob/master/R/event-loop.R
run_parallel_jobs <- function(jobs, options) {
    if (nrow(jobs) == 0) return()
    assert_count(options$num_parallel, positive = TRUE)

    # in case run in background
    jobs <- setDT(jobs)

    ## Kill all child processes if we quit from this function
    on.exit(kill_jobs(jobs, options), add = TRUE)

    # initialize job status and worker
    set(jobs, NULL, c("status", "index_str", "process", "stdout", "stderr", "exit_status", "start_time", "end_time"),
        list("waiting", lpad(jobs$index, "0"), list(), list(), list(), NA_integer_, as.POSIXct(NA), as.POSIXct(NA))
    )
    setindexv(jobs, "status")

    # Our global progress bar
    progress_bar <- progress::progress_bar$new(
        total = nrow(jobs), clear = FALSE,
        format = "[:current/:total | :percent] :bar [Elapsed: :elapsedfull]"
    )

    # Initialise one task for each worker
    num <- min(options$num_parallel, nrow(jobs))
    for (i in seq_len(num)) {
        progress_bar$tick(0)
        jobs <- schedule_next_sim(jobs, options, progress_bar)
        jobs <- run_job(jobs, options, progress_bar)
    }

    # Run until all simulation complete
    while (TRUE) {
        if (are_all_completed(jobs)) break;
        progress_bar$tick(0)
        jobs <- handle_events(jobs, options, progress_bar)
        jobs <- schedule_next_sim(jobs, options, progress_bar)
        jobs <- run_job(jobs, options, progress_bar)
    }

    set(jobs, NULL, c("model", "weather"), NULL)
    setnames(jobs, c("input_model", "input_weather"), c("idf", "epw"))

    jobs[, .SD, .SDcols = c("index", "status", "idf", "epw", "version", "exit_status",
        "start_time", "end_time", "output_dir", "energyplus", "stdout", "stderr"
    )]
}
# }}}
# kill_jobs {{{
kill_jobs <- function(jobs, options) {
    jobs[vlapply(process, function (x) inherits(x, "process") && x$is_alive()), `:=`(
        status = {for (p in process) p$kill(); "terminated"}
    )]

    jobs[J(c("waiting", "ready")), on = "status", status := "cancelled"]

    if (any(is_term <- jobs$status == "terminated")) {
        jobs[is_term, `:=`(
            stdout = lapply(process, function (x) tryCatch(x$read_all_output_lines(), error = function (e) NA_character_)),
            stderr = lapply(process, function (x) tryCatch(x$read_all_error_lines(), error = function (e) NA_character_)),
            exit_status = viapply(process, function (x) x$get_exit_status())
        )]
    }

    if (options$echo) {
        if (any(is_term)) {
            terminated <- jobs[is_term, sim_status("terminate", index_str, model, weather)]
            cat_line(terminated, col = "white", background_col = "red")
        }

        if (any(is_canc <- jobs$status == "cancelled")) {
            cancelled <- jobs[is_canc, sim_status("cancel", index_str, model, weather)]
            cat_line(cancelled, col = "white", background_col = "red")
        }
    }
}
# }}}
# schedule_next_sim {{{
schedule_next_sim <- function(jobs, options, progress_bar) {
    # Cannot run more workers?
    if (sum(jobs$status == "running") >= options$num_parallel) {
        return(jobs)
    }

    # waiting -> running
    # always schedule only one new job
    if (any(ready <- jobs$status == "waiting")) {
        set(jobs, jobs$index[ready][1L], "status", "ready")
    }

    jobs
}
# }}}
# run_job {{{
run_job <- function(jobs, options, progress_bar) {
    # clean wd
    ready <- which(jobs$status == "ready")

    if (!length(ready)) return(jobs)

    jobs[ready, c("status", "process", "start_time") := {
        clean_wd(model)

        process <- energyplus(eplus = energyplus, model = model,
            weather = unlist(weather), output_dir = output_dir, annual = annual,
            design_day = design_day, wait = FALSE, echo = FALSE,
            expand_obj = expand_obj)$process

        if (options$echo) {
            run <- sim_status("run", index_str, model, weather)
            progress_bar$message(paste0(run, collapse = "\n"))
        }
        progress_bar$tick(0)

        start_time <- lubridate::with_tz(process$get_start_time(), Sys.timezone())

        list(status = "running", process = list(process), start_time = start_time)
    }]
}
# }}}
# are_all_completed {{{
are_all_completed <- function(jobs) {
    all(jobs$status %chin% c("completed", "failed"))
}
# }}}
# handle_events {{{
handle_events <- function(jobs, options, progress_bar) {
    run <- jobs$status == "running"
    if (!any(run)) return(jobs)

    jobs[run & vlapply(process, function (x) !is.null(x) && !x$is_alive()),
        c("stdout", "stderr", "exit_status", "status", "end_time") := {
            res <- lapply(process, function (p) p$get_result())

            # somehow get_exit_status() function may return NA after execution
            # of a (successful) command
            # ref: https://github.com/r-lib/processx/issues/220
            exit_code <- viapply(process, function (x) x$get_exit_status())
            exit_code[is.na(exit_code)] <- 0L

            if (options$echo) {
                comp <- sim_status("complete", index_str, model, weather, exit_code)
                progress_bar$message(paste0(comp, collapse = "\n"))
            }
            progress_bar$tick(.N)

            status[exit_code == 0L] <- "completed"
            status[exit_code != 0L] <- "failed"

            list(stdout = lapply(res, "[[", "stdout"), stderr = lapply(res, "[[", "stderr"),
                 exit_status = exit_code, status = status, end_time = Sys.time()
            )
        }
    ]
}
# }}}
# sim_status {{{
sim_status <- function (type, index, model, weather, exit_code = NULL) {
    status <- c("run", "complete", "cancel", "terminate")
    if (length(type) == 1L && type %in% status) {
        type <- switch(type,
            run       = "RUNNING   ",
            complete  = "COMPLETED ",
            cancel    = "CANCELLED ",
            terminate = "TERMINATED"
        )
        if (!is.null(exit_code)) type[exit_code != 0L] <- "FAILED    "
    }

    mes <- paste0(lpad(index, "0"), "|", type, " --> ",
        "[IDF]", surround(basename(model))
    )

    has_epw <- !vlapply(weather, function (x) is.null(x) || is.na(x))

    if (any(has_epw)) {
        mes[has_epw] <- paste0(mes, " + ", "[EPW]", surround(basename(unlist(weather[has_epw]))))
    }

    mes
}
# }}}
# energyplus {{{
energyplus <- function (eplus, model, weather, output_dir, output_prefix = NULL,
                        output_suffix = c("C", "L", "D"), expand_obj = TRUE,
                        readvars = TRUE, annual = FALSE, design_day = FALSE,
                        idd = NULL, echo = TRUE, wait = TRUE) {

    output_suffix <- match.arg(output_suffix)

    assert_file_exists(eplus)
    assert_file_exists(model)
    assert_flag(expand_obj)
    assert_flag(readvars)
    assert_flag(annual)
    assert_flag(design_day)
    assert_flag(echo)
    assert_flag(wait)
    if (!is.null(weather)) assert_file_exists(weather)
    if (!is.null(output_dir)) assert_directory_exists(output_dir, "w")
    if (!is.null(output_prefix)) assert_string(output_prefix)
    if (!is.null(idd)) assert_file_exists(idd)

    if (annual && design_day) {
        abort("Cannot force both design-day and annual simulations", "both_ddy_annual")
    }

    # argument docs {{{
    ############################################################################
    #           Notes on arguments (From EnergyPlus documentation)             #
    ############################################################################
    # 1. model
    # Full path of a model file with extension of idf or imf to use.

    # 2. weather
    # Full path of a weather file to use.

    # 3. output_dir
    # Output directory of the simulation results.

    # 4. output_prefix
    # Prefix for output file names (default: eplus)

    # 5. output_suffix
    # Suffix style for output file names (default: L)
    #     L: Legacy (e.g., eplustbl.csv)
    #     C: Capital (e.g., eplusTable.csv)
    #     D: Dash (e.g., eplus-table.csv)

    # 6. epmacro
    # If TRUE, EPMacro will be run perior to simulation.

    # 7. expand_obj
    # If TRUE, ExpandObjects will be run perior to simulation.

    # 8. readvars
    # If TRUE, ReadVarsESO will be run after simulation.

    # 9. annual
    # If TRUE, force annual simulation.

    # 10. design_day
    # If TRUE, force design-day-only simulation.

    # 11. idd
    # The full path of Input Data Dictionary. (default: Energy+.idd in
    # executable directory)

    # 12. legacy (Currently not implemented yet.)
    # If TRUE, use legacy mode to run EnergyPlus. By using legacy mode, the
    # input idf file and weather file will be renamed into "in.idf" and "in.epw"
    # respectively. The file "Energy+.idd" in EnergyPlus folder will be copied
    # into the working direcotry.
    # BACCKGROUND: The command line interface is a new feature as of EnergyPlus
    # 8.3. Prior to version 8.3, the EnergyPlus executable took no command line
    # arguments, and instead expected the IDD (Input Data Dictionary) file and
    # the IDF files to be located in the current working directory and named
    # Energy+.idd and in.idf respectively.  If a weather file was required by
    # the simulation, then an in.epw file was also required in the same
    # directory. This behavior is still respected if no arguments are passed on
    # the command line.
    ############################################################################

    # }}}
    # Get the right format of the input command to EnergyPlus. {{{
    # NOTE: `ifelse` function cannot return NULL.
    if (is.null(output_dir)) output_dir <- dirname(model)
    if (is.null(output_prefix)) output_prefix <- tools::file_path_sans_ext(basename(model))

    if (has_ext(model, "imf")) {
        cmd_epmacro <- "--epmacro"
    } else {
        cmd_epmacro <- NULL
    }

    if (readvars) cmd_readvars <- "--readvars" else cmd_readvars <- NULL
    if (annual) cmd_annual <- "--annual" else cmd_annual <- NULL
    if (design_day) cmd_design_day <- "--design-day" else cmd_design_day <- NULL
    if (!is.null(idd)) cmd_idd <- paste0("--idd", shQuote(idd)) else cmd_idd <- NULL
    # }}}

    # manually run ExpandObjects first
    # see #213
    # backup original model path
    model_ori <- model
    expanded <- FALSE
    if (expand_obj) {
        model <- expand_objects(eplus, model, keep_ext = FALSE)
        expanded <- attr(model, "expanded")
    }

    arg_weather <- if (is.null(weather)) NULL else c("--weather", weather)
    args <- c(
        arg_weather,
        "--output-directory", output_dir,
        "--output-prefix", output_prefix,
        "--output-suffix", output_suffix,
        cmd_epmacro,
        cmd_readvars,
        cmd_annual,
        cmd_design_day,
        cmd_idd,
        model
    )

    if (expanded) {
        model_exp <- model
        model <- model_ori
        # _exp.idf --> .expidf
        rename_exp <- function () {
            path <- basename(paste0(stri_sub(tools::file_path_sans_ext(model_exp), to = -5L), ".expidf"))
            try(file.rename(model_exp, file.path(output_dir, path)), silent = TRUE)
        }
    }

    res <- list()

    if (wait) {
        p_stdout <- p_stderr <- "|"
        post_fun <- NULL
    # should always poll stdout and stderr
    # see https://github.com/r-lib/processx/issues/235
    } else {
        p_stdout <- tempfile()
        p_stderr <- tempfile()
        post_fun <- function () {
            stdout <- suppressWarnings(read_lines(p_stdout)$string)
            stderr <- suppressWarnings(read_lines(p_stderr)$string)
            if (!length(stdout)) stdout <- character(0)
            if (!length(stderr)) stderr <- character(0)
            if (expanded) rename_exp()
            unlink(c(p_stdout, p_stderr))
            list(stdout = stdout, stderr = stderr, end_time = Sys.time())
        }
    }

    # run energyplus
    proc <- processx::process$new(command = eplus, args = args, wd = dirname(model),
        cleanup = TRUE, windows_verbatim_args = FALSE,
        stdout = p_stdout, stderr = p_stderr, post_process = post_fun
    )

    # kill energyplus on exit
    exit_callback <- function () {
        if (expanded) rename_exp()
        if (!proc$is_alive()) return(NULL)

        k <- tryCatch(proc$kill(), error = function (e) FALSE)

        if (k) {
            msg <- paste0("TERMINATED--> [Idf]`", basename(model), "` + [Epw]`", basename(weather), "`")
            cli::cat_line(msg, col = "white", background_col = "red")
        }
    }

    if (wait) {
        # kill the process when function exits
        on.exit(exit_callback(), add = TRUE)

        res <- eplus_run_wait(proc, echo = echo)
    } else {
        # TODO: exit time
        # just return the process
        res <- list(
            process = proc,
            exit_status = NULL,
            stdout = NULL,
            stderr = NULL,
            start_time = lubridate::with_tz(proc$get_start_time(), Sys.timezone()),
            end_time = NULL
        )
    }

    # add more data
    res$output_dir <- output_dir
    res$energyplus <- eplus

    res
}
# }}}
# eplus_run_wait {{{
eplus_run_wait <- function (proc, echo = TRUE) {
    stdout <- c()
    stderr <- c()

    # get_output {{{
    get_output <- function (echo = TRUE) {
        newout <- proc$read_output_lines(2000)
        if (echo) cli::cat_line(newout)
        if (length(newout) && all(nzchar(newout))) {
            stdout <<- c(stdout, newout)
        }

        newerr <- proc$read_error(2000)
        if (echo) cat(crayon::red(newerr), sep = "")
        if (length(newerr) && all(nzchar(newerr))) {
            stderr <<- c(stderr, newerr)
        }
    }
    # }}}

    while (proc$is_alive()) {
        polled <- proc$poll_io(200)

        ## If output/error, then collect it
        if (any(polled == "ready")) get_output(echo)
    }

    # Needed to get the exit status
    proc$wait()

    # We might still have output
    while (proc$is_incomplete_output() || proc$is_incomplete_error()) {
        proc$poll_io(-1)
        get_output(echo)
    }

    list(process = proc,
         exit_status = proc$get_exit_status(),
         stdout = stdout,
         stderr = stderr,
         start_time = lubridate::with_tz(proc$get_start_time(), Sys.timezone()),
         end_time = Sys.time()
    )
}
# }}}
# eplus_exe {{{
eplus_exe <- function (eplus) {
    if (checkmate::test_file_exists(eplus, "x") || (is_windows() && has_ext(eplus, "exe"))) {
        suppressMessages(use_eplus(dirname(eplus)))
        return(normalizePath(eplus, mustWork = TRUE))
    }

    if (!is_avail_eplus(eplus)) use_eplus(eplus)
    config <- tryCatch(eplus_config(eplus),
        eplusr_warning_miss_eplus_config = function (w) abort(conditionMessage(w), "miss_eplus_config")
    )

    normalizePath(file.path(config$dir, config$exe), mustWork = TRUE)
}
# }}}
# copy_run_files {{{
copy_run_files <- function (file, dir) {
    loc <- file.path(dir, basename(file))
    file_copy(file, loc, err_title = "Failed to copy files into simulation output directory")
}
# }}}
# get_run_time {{{
get_run_time <- function (stdout) {
    if (!length(stdout)) return(NULL)
    last <- stdout[length(stdout)]

    period <- lubridate::hms(last, quiet = TRUE)
    if (is.na(period)) NULL else period
}
# }}}
# expand_objects {{{
expand_objects <- function (eplus, idf, keep_ext = FALSE) {
    exe <- file.path(dirname(eplus), paste0("ExpandObjects", if (is_windows()) ".exe" else ""))

    dir <- dirname(idf)

    # create "in.idf"
    file.copy(idf, file.path(dir, "in.idf"), overwrite = TRUE, copy.date = TRUE)

    # create ini file
    ini <- file.path(dir, "Energy+.ini")
    write_lines(c("[program]", sprintf("dir=%s", normalizePath(dirname(eplus)))), ini)

    # run ExpandObjects
    processx::run(exe, wd = dir)

    # remove ini file
    unlink(ini, force = TRUE)

    # get output file path
    if (keep_ext) {
        out <- file.path(dir, paste0(tools::file_path_sans_ext(basename(idf)), ".expidf"))
    } else {
        out <- file.path(dir, paste0(tools::file_path_sans_ext(basename(idf)), "_exp.idf"))
    }

    # rename genereated expidf file
    if (file.exists(file.path(dir, "in.expidf"))) {
        file.rename(file.path(dir, "in.expidf"), out)
        expanded <- TRUE
    } else if (file.exists(file.path(dir, "expanded.idf"))) {
        file.rename(file.path(dir, "expanded.idf"), out)
        expanded <- TRUE
    } else {
        expanded <- FALSE
        unlink(file.path(dir, "in.idf"), force  = TRUE)
        unlink(file.path(dir, "expandedidf.err"), force  = TRUE)
        unlink(file.path(dir, "fort.6"), force  = TRUE)
    }

    unlink(file.path(dirname(idf), "in.idf"), force  = TRUE)
    unlink(file.path(dirname(idf), "expandedidf.err"), force  = TRUE)
    unlink(file.path(dirname(idf), "fort.6"), force  = TRUE)
    if (file.exists(out)) {
        attr(out, "expanded") <- expanded
        out
    } else {
        attr(idf, "expanded") <- expanded
        idf
    }
}
# }}}

create_energyplus_ini <- function(eplus, output_dir) {
    dir <- normalizePath(dirname(eplus))
    if (is_windows()) {
        dir <- paste0(utils::shortPathName(dir), "\\")
    } else {
        dir <- paste0(dir, "/")
    }

    ini <- c(
        "[program]",
        sprintf("dir=%s", dir),
        "[BasementGHT]",
        "dir=PreProcess\\GrndTempCalc",
        "[SlabGHT]",
        "dir=PreProcess\\GrndTempCalc"
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
        if (!file.exists(idd)) {
            abort(sprintf("'%s' file did not found in EnergyPlus installation folder: '%s'", name, dirname(idd)))
        }
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

run_command <- function(command, args = NULL, wd, wait = TRUE, echo = TRUE,
                        post_callback = NULL, post_name = NULL,
                        exit_msg = NULL, exit_callback = NULL) {
    assert_character(args, null.ok = TRUE)
    assert_flag(wait)
    assert_flag(echo)
    assert_string(post_name, null.ok = TRUE)
    assert_string(exit_msg, null.ok = TRUE)
    assert_function(post_callback, null.ok = TRUE)
    assert_function(exit_callback, null.ok = TRUE)

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

            if (!length(out)) out <- NULL
            if (!length(err)) err <- NULL

            unlink(c(std_out, std_err))

            res <- list(stdout = out, stderr = err, end_time = Sys.time())

            if (!is.null(post_callback)) {
                res <- list(post_callback(), run = res)
                if (!is.null(post_name)) {
                    names(res) <- c(post_name, "run")
                }
            }

            res
        }
    }

    proc <- processx::process$new(command = command, args = args,
        wd = wd, cleanup = TRUE, cleanup_tree = TRUE,
        windows_verbatim_args = FALSE,
        stdout = std_out, stderr = std_err, post_process = post_fun
    )

    if (!wait) {
        # just return the process
        res <- list(
            process = proc,
            exit_status = NULL,
            stdout = NULL,
            stderr = NULL,
            start_time = lubridate::with_tz(proc$get_start_time(), Sys.timezone()),
            end_time = NULL
        )
    } else {
        callback <- function () {
            if (!proc$is_alive()) return(NULL)

            k <- tryCatch(proc$kill(), error = function(e) FALSE)

            if (!is.null(exit_callback)) exit_callback(command, args)

            if (k && !is.null(exit_msg)) {
                cli::cat_line(exit_msg, col = "white", background_col = "red")
            }
        }

        # kill the process when  exits
        on.exit(callback(), add = TRUE)

        stdout <- c()
        stderr <- c()
        get_output <- function(echo = TRUE) {
            newout <- proc$read_output_lines(2000)
            if (echo) cli::cat_line(newout)
            if (length(newout) && all(nzchar(newout))) {
                stdout <<- c(stdout, newout)
            }

            newerr <- proc$read_error(2000)
            if (echo) cli::cat_line(newerr, col = "red")
            if (length(newerr) && all(nzchar(newerr))) {
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
             start_time = lubridate::with_tz(proc$get_start_time(), Sys.timezone()),
             end_time = Sys.time()
        )
    }
}
get_eplus_loc_from_input <- function(idf, eplus = NULL) {
    eplus <- eplus %||% as.character(get_idf_ver(read_lines(idf)))
    if (!length(eplus)) {
        abort(paste0("Missing version field in input IDF file. ",
            "Failed to determine the version of EnergyPlus to use."),
            "miss_idf_ver"
        )
    }

    eplus_exe(eplus)
}

parse_eplus_ver_from_first_line <- function(line) {
    re <- "Program Version,EnergyPlus, Version (\\d\\.\\d\\.\\d)-[0-9a-f]+, YMD=\\d+\\.\\d{2}\\.\\d{2} \\d{2}:\\d{2}"
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
            energyplus_exe <- eplus_exe(parse_eplus_ver_from_first_line(l))
        }
    } else {
        energyplus_exe <- get_eplus_loc_from_input(model, eplus)
    }

    assert_string(output_dir, null.ok = TRUE)
    output_dir <- normalizePath(output_dir %||% dir_model, mustWork = FALSE)
    if (!dir.exists(output_dir) && !dir.create(output_dir, FALSE, TRUE)) {
        abort(sprintf("Failed to create output directory '%s'.", output_dir))
    }

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
    } else {
        if (!is_valid_file_name(output_prefix)) {
            abort(sprintf("Invalid 'output_prefix': '%s'", output_prefix))
        }
    }

    if (is.null(exectuable)) {
        exectuable <- energyplus_exe
    } else {
        exectuable <- path_eplus_processor(dirname(energyplus_exe), exectuable, .strict = TRUE)
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

#' @name energyplus
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

    # handle ouput file renaming
    file_callback <- function() {
        if (tools::file_ext(cmd$model) != "imf") unlink(cmd$idf)
        remove_eplus_in_files(cmd$model)

        file <- list()
        file$imf <- normalizePath(file.path(cmd$output_dir, basename(cmd$model)))
        with_wd(cmd$output_dir, {
            wd <- dirname(cmd$model)
            file$epmidf <- file_rename_if_exist(file.path(wd, "out.idf"), sprintf("%s.epmidf", cmd$output_prefix))
            file$epmdet <- file_rename_if_exist(file.path(wd, "audit.out"), sprintf("%s.epmdet", cmd$output_prefix))
        })
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

#' @name energyplus
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
    create_energyplus_ini(cmd$energyplus, wd)
    copy_energyplus_idd(cmd$energyplus, wd, idd)

    # handle ouput file renaming
    file_callback <- function() {
        if (tools::file_ext(cmd$model) != "idf") unlink(cmd$idf)
        remove_eplus_in_files(cmd$model)
        unlink(file.path(wd, "Energy+.ini"))
        if (wd != dirname(cmd$energyplus)) unlink(file.path(wd, "Energy+.idd"))

        file <- list()
        file$idf <- normalizePath(file.path(cmd$output_dir, basename(cmd$model)))
        with_wd(cmd$output_dir, {
            file$expidf <- file_rename_if_exist(file.path(wd, "expanded.idf"), sprintf("%s.expidf", cmd$output_prefix))
            file$basement <- file_rename_if_exist(file.path(wd, "BasementGHTIn.idf"), "BasementGHTIn.idf")
            file$slab <- file_rename_if_exist(file.path(wd, "GHTIn.idf"), "GHTIn.idf")
            file$experr <- file_rename_if_exist(file.path(wd, "expandedidf.err"), sprintf("%s.experr", cmd$output_prefix))
        })
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

#' @name energyplus
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

    cmd$idf <- file_rename_if_exist(cmd$idf, file.path(dirname(cmd$idf), "BasementGHTIn.idf"))

    wd <- dirname(cmd$idf)
    with_wd(wd, {
        unlink(c("EPObjects.TXT", "GrTemp.TXT", "TempInit.TXT", "RunSolar.TXT",
            "RunINPUT.TXT", "RunTGMAVG.TXT", "eplusout.end", "audit.out",
            "rundebugout.txt", "basementout.audit"))

        unlink(sprintf(
            "%s%s", cmd$output_prefix,
            c(
                ".audit", ".out", "_out.idf", "_bsmt.csv", "_bsmt.audit",
                "_bsmt.out", "_out_bsmt.idf"
            )
        ))
    })

    create_energyplus_ini(cmd$energyplus, wd)

    # copy BasementGHT.idd
    idd <- copy_energyplus_idd(cmd$exectuable, wd, idd, "BasementGHT.idd")

    # handle ouput file renaming
    file_callback <- function() {
        remove_eplus_in_files(cmd$model, cmd$weather)
        if (attr(idd, "copied")) unlink(file.path(wd, "BasementGHT.idd"))
        unlink(file.path(wd, "Energy+.ini"))

        file <- list()
        file$idf <- normalizePath(file.path(cmd$output_dir, basename(cmd$model)))
        file$epw <- normalizePath(file.path(cmd$output_dir, basename(cmd$weather)))
        with_wd(wd, {
            file$bsmt_idf <- file_rename_if_exist("EPObjects.txt", sprintf("%s_bsmt.idf", cmd$output_prefix))
            file$bsmt_csv <- file_rename_if_exist("MonthlyResults.csv", sprintf("%s_bsmt.csv", cmd$output_prefix))
            file$bsmt_out <- file_rename_if_exist("RunINPUT.TXT", sprintf("%s_bsmt.out", cmd$output_prefix))

            if (!file.exists("RunDEBUGOUT.TXT")) {
                file$bsmt_audit <- normalizePath(sprintf("%s_bsmt.audit", cmd$output_prefix), mustWork = FALSE)
                write_lines("Basement Audit File", file$bsmt_audit)
            } else {
                file$bsmt_audit <- file_rename("RunDEBUGOUT.TXT", sprintf("%s_bsmt.audit", cmd$output_prefix))
            }

            if (file.exists("audit.out")) {
                write_lines(read_lines("audit.out"), file$bsmt_audit, append = TRUE)
            }
            if (file.exists("eplusout.err")) {
                write_lines(read_lines("eplusout.err"), file$bsmt_audit, append = TRUE)
            }

            unlink(c("EPObjects.TXT", "GrTemp.TXT", "TempInit.TXT", "RunSolar.TXT",
                "RunINPUT.TXT", "RunTGMAVG.TXT", "eplusout.end", "audit.out",
                "RunDEBUGOUT.TXT", "basementout.audit", "eplusout.err"))
        })

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

#' @name energyplus
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

    cmd$idf <- file_rename_if_exist(cmd$idf, file.path(dirname(cmd$idf), "GHTIn.idf"))

    wd <- dirname(cmd$idf)
    with_wd(wd, {
        unlink(c("EPObjects.TXT", "SLABDBOUT.TXT", "SLABINP.TXT",
            "SLABSplit Surface Temps.TXT", "eplusout.end", "audit.out"))

        unlink(sprintf(
            "%s%s", cmd$output_prefix, c(".gtp", ".ger", "_slab.gtp", "_slab.ger")
        ))
    })

    create_energyplus_ini(cmd$energyplus, wd)

    # copy SlabGHT.idd
    idd <- copy_energyplus_idd(cmd$exectuable, wd, idd, "SlabGHT.idd")

    # handle ouput file renaming
    file_callback <- function() {
        remove_eplus_in_files(cmd$model, cmd$weather)
        if (attr(idd, "copied")) unlink(file.path(wd, "SlabGHT.idd"))
        unlink(file.path(wd, "Energy+.ini"))

        file <- list()
        file$idf <- normalizePath(file.path(cmd$output_dir, basename(cmd$model)))
        file$epw <- normalizePath(file.path(cmd$output_dir, basename(cmd$weather)))
        with_wd(wd, {
            file$slab_gtp <- file_rename_if_exist("SLABSurfaceTemps.txt", sprintf("%s_slab.gtp", cmd$output_prefix))
            file$slab_out <- file_rename_if_exist("SLABINP.txt", sprintf("%s_slab.out", cmd$output_prefix))
            file$slab_ger <- file_rename_if_exist("eplusout.err", sprintf("%s_slab.ger", cmd$output_prefix))

            unlink(c("EPObjects.TXT", "eplusout.err", "SLABDBOUT.TXT",
                "SLABSplit Surface Temps.TXT", "eplusout.end", "audit.out"))
        })

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

#' @name energyplus
#' @keywords internal
#' @export
EnergyPlus <- function(model, weather, output_dir = NULL,
                       output_prefix = NULL, output_suffix = c("C", "L", "D"),
                       wait = TRUE, echo = TRUE,
                       annual = FALSE, design_day = FALSE,
                       idd = NULL, eplus = NULL) {
    assert_flag(annual)
    assert_flag(design_day)
    output_suffix <- match.arg(output_suffix)

    cmd <- pre_eplus_command(
        exectuable = NULL,
        model = model, weather = weather,
        output_dir = output_dir, output_prefix = output_prefix,
        wait = wait, echo = echo, eplus = eplus
    )

    eplus_ver <- get_ver_from_path(dirname(cmd$energyplus))
    # just for test purpose
    legacy <- getOption("eplusr.eplus_83", eplus_ver < 8.3)

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
        wd <- dirname(cmd$model)

        # create ini file in and copy idd
        create_energyplus_ini(cmd$energyplus, wd)
        idd <- copy_energyplus_idd(cmd$energyplus, wd, idd)

        # handle ouput file renaming
        file_callback <- function() {
            remove_eplus_in_files(cmd$model, cmd$weather)
            if (attr(idd, "copied")) unlink(file.path(wd, "Energy+.idd"))
            unlink(file.path(wd, "Energy+.ini"))

            # manually handle file renaming
            file <- lapply(seq_along(legacy_files), function(i) {
                from <- file.path(wd, legacy_files[[i]])
                to <- file.path(cmd$output_dir, out_files[[i]])
                files <- file_rename_if_exist(from, to)
                files <- files[!is.na(files)]
                if (!length(files)) files <- NA_character_
                files
            })
            names(file) <- names(out_files)

            file
        }
    } else {
        # no more need "in.idf" and "in.epw"
        remove_eplus_in_files(cmd$model, cmd$weather)

        args <- c(
            "--output-directory", cmd$output_dir,
            "--output-prefix", cmd$output_prefix,
            "--output-suffix", output_suffix
        )

        if (annual) args <- c(args, "--annual")
        if (design_day) args <- c(args, "--design-day")
        if (!is.null(idd)) args <- c(args, "--idd", normalizePath(idd, mustWork = FALSE))
        if (!is.null(weather)) args <- c(args, "--weather", normalizePath(weather, mustWork = FALSE))

        args <- c(args, cmd$model)

        file_callback <- function() {
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

    run <- run_command(cmd$exectuable, args, wd = dirname(cmd$idf), wait = wait, echo = echo,
        post_callback = post_callback, post_name = "file",
        exit_msg = interrupted_msg("EnergyPlus", cmd$model, cmd$weather)
    )

    if (wait) file <- file_callback()

    list(file = file, run = run)
}

#' @name energyplus
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

    if (is.null(rules)) {
        rules <- normalizePath(file.path(dirname(cmd$exectuable), "convert.txt"), mustWork = TRUE)
        rules_copied <- TRUE
    } else {
        rules <- normalizePath(assert_file_exists(rules, "r"))
        rules_copied <- tolower(basename(rules)) != "convert.txt" ||
            normalizePath(dirname(rules)) != wd
    }
    # copy conversion rules into the working directory
    rules <- file_copy(rules, file.path(wd, "convert.txt"))

    # copy eso file
    ext <- tools::file_ext(cmd$model)
    eso <- sprintf("eplusout.%s", ext)
    if (eso_copied <- basename(cmd$model) != eso) {
        eso <- file_copy(cmd$model, file.path(wd, sprintf("eplusout.%s", ext)))
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
            file$eso <- normalizePath(file.path(cmd$output_dir, basename(cmd$model)))
        } else {
            file$mtr <- normalizePath(file.path(cmd$output_dir, basename(cmd$model)))
        }

        with_wd(wd, {
            if (rules_copied) unlink(rules)

            if (file.exists("ip.eso")) {
                if (eso_copied) unlink("eplusout.eso")
                file$ipeso <- file_rename("ip.eso", sprintf("%s.ipeso", cmd$output_prefix))
            }

            if (file.exists("ip.mtr")) {
                if (eso_copied) unlink("eplusout.mtr")
                file$ipmtr <- file_rename("ip.mtr", sprintf("%s.ipmtr", cmd$output_prefix))
            }

            if (file.exists("ip.err")) {
                file$iperr <- file_rename("ip.err", sprintf("%s.iperr", cmd$output_prefix))
            }
        })

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

#' @name energyplus
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

    # file name without extension
    nm <- tools::file_path_sans_ext(basename(cmd$model))
    ext_in <- tools::file_ext(cmd$model)
    # handle eso and mtr after convertESOMTR
    ext_in <- switch(ext_in, ipeso = "eso", ipmtr = "mtr", ext_in)

    # copy files
    if (copied <- basename(cmd$model) != sprintf("eplusout.%s", ext_in)) {
        eso <- file_copy(cmd$model, file.path(wd, sprintf("eplusout.%s", ext_in)))
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
    if (file.exists(inp_in <- file.path(wd, sprintf("%s.%s", nm, ext)))) {
        inp <- file_copy(inp_in, file.path(wd, inp))
    # create a temporary rvi if eplusout.inp does not exist
    } else {
        inp <- normalizePath(file.path(wd, inp), mustWork = FALSE)
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
            file$eso <- normalizePath(file.path(cmd$output_dir, basename(cmd$model)))
        } else {
            file$mtr <- normalizePath(file.path(cmd$output_dir, basename(cmd$model)))
        }

        with_wd(wd, {
            if (copied) unlink(eso)

            unlink(inp)

            if (file.exists(csv)) {
                csv <- file_rename(csv, file.path(cmd$output_dir, csv))
                if (ext_in == "eso") {
                    file$variable <- csv
                } else {
                    file$meter <- csv
                }

            }

            if (file.exists("readvars.audit")) {
                file$rvaudit <- file_rename("readvars.audit", file.path(cmd$output_dir, sprintf("%s.rvaudit", cmd$output_prefix)))
            }
        })

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

#' @name energyplus
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

    if (copied <- basename(cmd$model) != "eplusout.bnd") {
        bnd <- file_copy(cmd$model, file.path(wd, "eplusout.bnd"))
    }

    # handle ouput file renaming
    file_callback <- function() {
        file <- list()
        file$bnd <- normalizePath(file.path(cmd$output_dir, basename(cmd$model)))
        file$svg <- NA_character_

        with_wd(wd, {
            if (copied) unlink(bnd)

            if (file.exists("eplusout.svg")) {
                file$svg <- file_rename("eplusout.svg", file.path(cmd$output_dir, sprintf("%s.svg", cmd$output_prefix)))
            }
        })

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
#' @param model [`character(1)`]\cr
#' A path of an EnergyPlus IDF or IMF file.
#'
#' @param eso [`character(1)`]\cr
#' A path of an EnergyPlus standard output (`.eso`) or EnergyPlus meter output
#' (`.mtr`) file.
#'
#' @param bnd [`character(1)`]\cr
#' A path of an EnergyPlus branch node details (`.bnd`) file.
#'
#' @param weather [`character(1)` or `NULL`]\cr
#' A path of an EnergyPlus weather (EPW) file. If `NULL`, design-day-only
#' simulation is triggered, regardless of the `design_day` value.
#'
#' @param output_dir [`character(1)` or `NULL`]\cr
#' Output directory of EnergyPlus simulation outputs. If `NULL`, the directory
#' where the input `model` locates is used. Default: `NULL`.
#'
#' @param output_prefix [`character(1)` or `NULL`]\cr
#' Prefix for EnergyPlus output file names. If `NULL`, the input `model` file
#' name is used.  Default: `NULL`.
#'
#' @param output_suffix [`character(1)`]\cr
#' Suffix style for EnergyPlus output file names. Should be one of the
#' followings:
#'
#' - `C`: **Capital**, e.g. `eplusTable.csv`. This is the default.
#' - `L`: **Legacy**, e.g. `eplustbl.csv`.
#' - `D`: **Dash**, e.g. `eplus-table.csv`.
#'
#' @param epmacro [`logical(1)`]\cr
#' If `TRUE`, EPMacro processor is called perior to simulation. Only applicable
#' if input file is an `IMF` file. Default: `TRUE`.
#'
#' @param expand_obj [`logical(1)`]\cr
#' If `TRUE`, ExpandObjects processor is called perior to simulation. Should be
#' `TRUE` if calling Basement or Slab preprocessors is desired. Default: `TRUE`.
#'
#' @param annual [`logical(1)`]\cr
#' If `TRUE`, annual simulation is forced. Currently, only support EnergyPlus >=
#' v8.3. Note that `annual` and `design_day` cannot both be `TRUE`. Default:
#' `FALSE`.
#'
#' @param design_day [`logical(1)`]\cr
#' If `TRUE`, design-day-only simulation is forced. Currently, only support
#' EnergyPlus >= v8.3. Note that `annual` and `design_day` cannot both be
#' `TRUE`. Default: `FALSE`.
#'
#' @param eso_to_ip [`logical(1)`]\cr
#' If `TRUE`, convertESOMTR post-processor is called after simulation to convert
#' the units of data in `eso` file from SI units to IP units. Default: `FALSE`.
#'
#' @param readvars [`logical(1)`]\cr
#' If `TRUE`, ReadVarsESO post-processor is called after to simulation. Default:
#' `TRUE`.
#'
#' @param echo [`logical(1)`]\cr
#' Wheter to show standard output and error from EnergyPlus and its pre- and
#' post- processors. Default: `TRUE`.
#'
#' @param wait [`logical(1)`]\cr
#' If `FALSE`, simualtion is run in the background and a [processx::process]
#' object is returned. Extra steps are needed to collect the results after the
#' process completes.
#'
#' @param idd [`character(1)` or `NULL`]\cr
#' The full path of EnergyPlus IDD (Input Data Dictionary). If `NULL`,
#' `Energy+.idd` file in EnergyPlus installation directory is used. Default:
#' `NULL`.
#'
#' @param eplus [`character(1)` or `NULL`]\cr
#' An EnergyPlus version or a path of EnergyPlus installation directory. If
#' `NULL`, the version of EnergyPlus to use is determined by the version of
#' input `model`.  Default: `NULL`.
#'
#' @name energyplus
#' @keywords internal
#' @return
#'
#' Functions except for `energyplus()` return a list of two elements:
#'
#' - `file`: a named list of full paths of output files
#' - `run`: a named list of outputs from the process.
#'
#' `energyplus()` returns a list of 7 elements:
#'
#' - `ver`: EnergyPlus [version][numeric_version()] used
#' - `energyplus`: EnergyPlus installation directory
#' - `start_time`: a [POSIXct()] giving the local time when the simulation
#'   starts
#' - `end_time`: a [POSIXct()] giving the local time when the simulation ends
#' - `output_dir`: full path of output directory of simulation outputs
#' - `file`: a named list of relative paths of output files under `output_dir`
#' - `run`: a [data.table][data.table::data.table()] of each outputs from the
#'   all called processes
#'
#' @export
energyplus <- function(model, weather, output_dir = NULL,
                       output_prefix = NULL, output_suffix = c("C", "L", "D"),
                       epmacro = TRUE, expand_obj = TRUE,
                       annual = FALSE, design_day = FALSE,
                       eso_to_ip = FALSE, readvars = TRUE,
                       echo = TRUE, idd = NULL, eplus = NULL) {
    assert_flag(epmacro)
    assert_flag(expand_obj)
    assert_flag(eso_to_ip)
    assert_flag(readvars)
    output_suffix <- match.arg(output_suffix)

    # validate inputs and do some preparations and create "in.idf" and
    # "in.epw" in the same directory as input model
    cmd <- pre_eplus_command(NULL, model, weather, output_dir, output_prefix,
        echo = echo, eplus = eplus)

    # get EnergyPlus exectuable version
    eplus_ver <- get_ver_from_path(dirname(cmd$energyplus))
    # just for test purpose
    legacy <- getOption("eplusr.eplus_legacy", eplus_ver < 8.3)

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
    #       the legacy input/output file names. They should be run inside the
    #       temperory simulation directory under the specified output directory.
    #       Once finished, move them back to the output directory.
    cmd$sim_dir <- cmd$output_dir
    temp_dir <- FALSE
    if (legacy || epmacro || expand_obj || eso_to_ip || readvars) {
        temp_dir <- TRUE
        cmd$sim_dir <- normalizePath(tempfile("EPTEMP-", cmd$output_dir), mustWork = FALSE)
        while (dir.exists(cmd$sim_dir)) {
            cmd$sim_dir <- normalizePath(tempfile("EPTEMP-", cmd$output_dir), mustWork = FALSE)
        }
        if (!dir.create(cmd$sim_dir, FALSE)) {
            abort(sprintf("Failed to create simulation directory '%s'.", cmd$sim_dir))
        }
    # If only run EnergyPlus with command line interface, there is no need to
    # create "in.idf" and "in.epw"
    } else if (!legacy) {
        unlink(c(cmd$idf, cmd$epw))
        cmd$idf <- cmd$model
        cmd$epw <- cmd$weather
    }

    path_sim <- function(file) normalizePath(file.path(cmd$sim_dir, basename(file)), mustWork = FALSE)
    path_out <- function(file) normalizePath(file.path(cmd$output_dir, basename(file)), mustWork = FALSE)
    temp_name <- function() basename(tempfile(tmpdir = cmd$sim_dir))
    file_temp <- function(file, ext = NULL) {
        res <- rep(NA_character_, length(file))
        if (all(isna <- is.na(file))) return(res)
        if (is.null(ext)) ext <- paste0(".", tools::file_ext(file))
        res[!isna] <- file_rename(file[!isna], path_sim(sprintf("%s%s", temp_name(), ext)))
        res
    }
    file_out <- function(file, ext = NULL) {
        res <- rep(NA_character_, length(file))
        if (all(isna <- is.na(file))) return(res)
        if (is.null(ext)) ext <- paste0(".", tools::file_ext(file))
        res[!isna] <- file_rename(file[!isna], path_out(sprintf("%s%s", cmd$output_prefix, ext[!isna])))
        res
    }

    # clean output directory
    .clean_wd(path_out("in.imf"), "L")
    .clean_wd(path_out(basename(cmd$model)), output_suffix)

    # move "in.epw" to the simulation directory
    file$epw <- NA_character_
    if (!is.null(cmd$epw)) {
        file$epw <- path_out(basename(cmd$weather))
        if (temp_dir) cmd$epw <- file_rename(cmd$epw, path_sim(cmd$epw))
    }

    # 1. run EPMacro
    run$EPMacro <- list()
    file$imf <- NA_character_
    file$epmidf <- NA_character_
    file$epmdet <- NA_character_
    if (!epmacro && has_ext(cmd$idf, "imf")) epmacro <- TRUE
    if (epmacro && has_ext(cmd$idf, "imf")) {
        file$imf <- path_out(basename(cmd$model))

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
    } else {
        file$idf <- path_out(basename(cmd$model))
        cmd$idf <- file_copy(cmd$model, path_sim("in.idf"))
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
            cmd$idf <- file_copy(file$expidf, path_out(sprintf("%s_expanded.idf", cmd$output_prefix)))

        # If no expanded object generated, the input idf should be used as the
        # input for EnergyPlus simulation and should be deleted after simulation
        } else {
            # prepend an "_expanded" suffix
            cmd$idf <- file_out(res_expandobj$file$idf, "_expanded.idf")
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
    # For EnergyPlus earlier than v8.3, run the simulation in the temporary
    # simulation directory.
    # NOTE: It is possible that the input file still uses resources with
    # relative paths, e.g. `Schedule:File`. But currently this is the approach I
    # came out with.
    #
    # TODO: Revisit this after refactor IDF parsing. If the time spent on
    # parsing the IDF file is neglectable, copy all external files into the
    # temporary simulation directory maybe an option.
    if (legacy) {
        res_eplus <- EnergyPlus(cmd$idf, cmd$epw, cmd$sim_dir, cmd$output_prefix,
            output_suffix, wait = TRUE, echo = echo, annual = annual,
            design_day = design_day, idd = idd, eplus = eplus_ver)

        # now it's safe to move all output files to the output directory
        res_eplus$file$idf <- NULL
        res_eplus$file$epw <- NULL
        res_eplus$file <- lapply(res_eplus$file, function(f) {
            if (length(f) == 1L && is.na(f)) return(f)
            ext <- gsub(cmd$output_prefix, "", basename(f), fixed = TRUE)
            file_out(f, ext)
        })
    # For EnergyPlus v8.3 and later, running directly using the output directory
    # is safe
    } else {
        res_eplus <- EnergyPlus(cmd$idf, cmd$weather, cmd$output_dir, cmd$output_prefix,
            output_suffix, wait = TRUE, echo = echo, annual = annual,
            design_day = design_day, idd = idd, eplus = eplus_ver)
    }

    # delete the temporary input model since it has been put in the output
    # directory but not the simulation directory
    unlink(cmd$idf)

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
            if (!is.na(res_eplus$file$eso)) res_ip$file$eso <- res_eplus$file$eso
            if (!is.na(res_eplus$file$mtr)) res_ip$file$mtr <- res_eplus$file$mtr

            # directly move the error file to the output directory
            res_ip$file$iperr <- file_out(res_ip$file$iperr)
            file$iperr <- res_ip$file$iperr

            if (!readvars) {
                file$ipeso <- res_ip$file$ipeso
                file$ipmtr <- res_ip$file$ipmtr
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
    file$eso <- NA_character_
    file$mtr <- NA_character_
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

        res_diagram <- HVAC_Diagram(res_eplus$file$bnd, cmd$output_dir, cmd$output_prefix,
            wait = TRUE, echo = echo, eplus = eplus_ver)
        run$HVAC_Diagram <- res_diagram$run

        # Since bnd is the legacy name "eplusout.bnd", HVAC_Diagram() will
        # not delete it after calling HVAC-Diagram
        res_eplus$file$bnd <- file_out(res_diagram$file$bnd)
        res_eplus$file$svg <- file_out(res_diagram$file$svg)
    }

    file <- modifyList(res_eplus$file, file)
    file <- file[order(names(file))]
    file$idf <- NA_character_
    if (is.na(file$imf)) file$idf <- path_out(basename(cmd$model))

    run <- data.table(
        program = names(run),
        exit_status = lapply(run, "[[", "exit_status"),
        start_time = lapply(run, "[[", "start_time"),
        end_time = lapply(run, "[[", "end_time"),
        stdout = lapply(run, "[[", "stdout"),
        stderr = lapply(run, "[[", "stderr"),
        process = lapply(run, "[[", "process")
    )

    # 9. FMUImport/FMUExport
    unlink(path_sim("tmp-fmus"), recursive = TRUE, force = TRUE)

    if (temp_dir) unlink(cmd$sim_dir, recursive = TRUE, force = TRUE)

    return(list(file = file, run = run))
}
