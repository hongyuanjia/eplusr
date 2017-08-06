#' Import EnergyPlus .epg group file
#'
#' \code{read_epg} returns a tibble which contains the necessary infomation in
#' order to run simulations using \code{\link{run_epg}}.
#'
#' @param epg A file path of EnergyPlus .epg file.
#' @return A tibble.
#' @importFrom readr read_csv cols col_integer
#' @importFrom dplyr as_tibble mutate select one_of
#' @importFrom purrr map_at
#' @export
# read_epg{{{1
read_epg <- function(epg){

    assertthat::assert_that(is_epg_file(epg), msg = "Invalid epg file.")

    sim_info <- readr::read_csv(file = epg, comment = "!",
                                col_names = c("model", "weather", "result", "run_times"),
                                col_types = cols(.default = "c", run_times = col_integer()))

    sim_info <- dplyr::as_tibble(
        purrr::map_at(sim_info, c("model", "weather", "result"), file_path)
    )
    sim_info <- dplyr::mutate(sim_info, output_dir = dirname(result))
    sim_info <- dplyr::mutate(sim_info, output_prefix = file_path(result))
    sim_info <- dplyr::select(sim_info, dplyr::one_of("model", "weather", "output_dir", "output_prefix", "run_times"))

    class(sim_info) <- c("epg", "eplus_job", class(sim_info))

    return(sim_info)
}
# }}}1

# is_epg{{{1
is_epg_file <- function (epg) {
    if (assertthat::is.string(epg)) {
        if (has_epg_ext(epg)) {
            assertthat::is.readable(epg)
            try_epg <- readr::read_csv(epg, comment = "!", n_max = 0,
                col_names = FALSE,
                col_types = readr::cols(.default = readr::col_character())
            )
            if (identical(ncol(try_epg), 4L)) {
                TRUE
            } else {
                FALSE
            }
        } else {
            FALSE
        }
    } else {
        FALSE
    }
}
# }}}1

# run_epg{{{1
run_epg <- function (epg, n = NULL, eplus_ver = NULL, eplus_dir = NULL) {

    # Check 'epg'{{{2
    if (is.character(epg)) {
        if (!file.exists(epg)) {
            stop("Input 'epg' file does not exists.", call. = FALSE)
        } else {
            job <- read_epg(epg)
        }
    } else if (is.data.frame(epg)) {
        type <- attr(epg, "job_type")
        col_names <- colnames(epg)
        required <- c("model", "weather", "output_dir", "output_prefix", "run_times")
        ex_col <- setdiff(col_names, required)
        mis_col <- setdiff(required, col_names)
        if (all(type == "epg", length(ex_col) == 0L, length(mis_col) == 0L)) {
            job <- epg
        } else {
            stop("Invalid 'epg'. Please use 'read_epg' to get the correct type", call. = FALSE)
        }
    } else {
        stop("'epg' should be either a data.frame or an .epg file path.", call. = FALSE)
    }
    # }}}2

    # Write epg file{{{2
    if (unique(job[["output_dir"]]) == 1L) {
        path <- unique(job[["output_dir"]])
        if (!dir.exists(path)) {
            dir.create(path, showWarnings = FALSE, recursive = TRUE)
        }
        file_name <- normalizePath(file_path(path, "run.epg"), mustWork = FALSE)
        write_epg(models = normalizePath(job[["model"]], mustWork = TRUE),
                  weathers = normalizePath(job[["weather"]], mustWork = TRUE),
                  output_dirs = normalizePath(file_path(job[["output_dir"]], job[["output_prefix"]])), mustWork = FALSE,
                  run_times = job[["run_times"]], path = file_name)
    }
    # }}}2

    # Run
    rum_multi(models = epg[["model"]], weathers = epg[["weather"]],
              output_dirs = epg[["output_dir"]], output_prefix = epg[["outout_prefixes"]],
              n = n, eplus_ver = eplus_ver, eplus_dir = eplus_dir)
}
# }}}1

# write_epg{{{1
write_epg <- function(models, weathers, output_dirs, run_times, path){
    header <- paste0(
        "! EnergyPlus Group File",
        "! ------------------------------------------------------------------------------------------------",
        "! Each line represents a specific simulation. If you don't want a simulation to run, add a comment",
        "! character (an exclamation point) to the beginning of that line. Commas are used to separate the ",
        "! fields. Each line consists of the following fields: ",
        "!",
        "!    input file name, weather file name, output file name (no extension), counter",
        "!",
        "! ------------------------------------------------------------------------------------------------",
        collapse = "\n")

    contents <- paste(models, weathers, output_dirs, run_times, sep = ",")
    epg_info <- paste(header, contents)
    readr::write_lines(epg_info, path)
}
# }}}1
