################################################################################
#                          EnergyPlus Results Reading                          #
################################################################################

#' Import jEPlus .json type project.
#'
#' \code{read_jeplus} takes a file path of an .json type project of jEPlus,
#' and return a list containing model paths, weather paths, parametric fields,
#' and parametric values. The returned list will have an attribute 'job_type'
#' with value 'jeplus' which will be used when running jobs using
#' \link{\code{run_job}}.
#'
#' @param json A file path of a .json file.
#' @return A list containing project info.
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_split str_replace_all str_replace str_detect str_replace
#' @importFrom purrr set_names map map_chr map2 set_names flatten_chr cross_n
#' @importFrom data.table rbindlist
#' @importFrom dplyr as_tibble
#' @export
# read_jeplus{{{1
read_jeplus <- function (json) {
    # Read jeplus JSON project file.
    info <- jsonlite::fromJSON(json)

    # Get parameter info.
    params <- info[["parameters"]]

    param_id <- params[["id"]]
    param_name <- params[["name"]]

    param_field <- stringr::str_split(params[["searchString"]], "\\|")
    param_field <- purrr::set_names(param_field, param_name)

    param_value <- stringr::str_replace_all(params[["valuesString"]], "[\\{\\}]", "")
    # Check if  the parametric value is a numeric seq.
    regex_seq <- "\\[(\\d+(?:\\.\\d+)*):(\\d+(?:\\.\\d+)*):(\\d+(?:\\.\\d)*)\\]"
    idx_value_seq <- stringr::str_detect(param_value, regex_seq)

    param_value <- stringr::str_split(param_value, "(\\s)*,(\\s)*")
    param_value <- map2(idx_value_seq, seq_along(param_value),
                        ~{if (.x) {
                             from <- as.numeric(stringr::str_replace(param_value[[.y]], regex_seq, "\\1"))
                             by <- as.numeric(stringr::str_replace(param_value[[.y]], regex_seq, "\\2"))
                             to <- as.numeric(stringr::str_replace(param_value[[.y]], regex_seq, "\\3"))
                             as.character(seq(from = from , to = to, by = by))
                         } else {
                             param_value[[.y]]
                         }})

    # Get selected parameter values.
    param_value_selected <- params[["selectedAltValue"]]
    param_value <- purrr::map2(param_value_selected, param_value, ~{if (.x > 0) .y <- .y[.x] else .y})

    param_value <- purrr::map(param_value, ~stringr::str_split(.x, "(\\s)*\\|(\\s)*"))
    param_value <- purrr::set_names(param_value, param_name)

    # Create case names according to parameter names.
    case_names <- purrr::pmap(list(param_id, param_value, param_value_selected),
                              function(name, value, selected) {
                                  if (as.integer(selected) > 0) {
                                      paste0(name, selected)
                                  } else {
                                      paste0(name, seq_along(value))
                                  }
                              })
    case_names <- dplyr::as_tibble(data.table::rbindlist(purrr::cross_n(case_names)))
    case_names <- map_chr(seq(1:nrow(case_names)), ~paste(case_names[.x,], collapse = "_"))

    # Get all combination of case values.
    param_value <- purrr::cross_n(param_value)
    param_value <- purrr::set_names(param_value, case_names)

    # Get input file info.
    idfs <- purrr::flatten_chr(stringr::str_split(info[["idftemplate"]], "\\s*;\\s*"))
    wthrs <- purrr::flatten_chr(stringr::str_split(info[["weatherFile"]], "\\s*;\\s*"))
    idf_path <- paste0(info[["idfdir"]], idfs)
    wthr_path <- paste0(info[["weatherDir"]], wthrs)

    sim_info <- list(idf_path = idf_path, weather_path = wthr_path,
                     param_field = param_field, param_value = param_value)

    class(sim_info) <- c("jeplus", "eplus_job", class(sim_info))

    return(sim_info)
}
# }}}1

#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom data.table data.table as.data.table
#' @importFrom dplyr tibble
#' @importFrom purrr map
#' @export
# collect_eplus: A function to read EnergyPlus simulation results.
# collect_eplus
# collect_eplus {{{1
collect_eplus <- function (path, output = c("variable", "meter", "table", "surface report"),
                           suffix_type = c("auto", "C", "L", "D"), which = NULL,
                           year = current_year(), new_date = "datetime",
                           tz = Sys.timezone(), drop_na = FALSE, unnest = FALSE,
                           long_table = FALSE) {
    # Read only one path at a time
    assertthat::assert_that(assertthat::is.string(path))
    # Default is to read variable output
    output <- rlang::arg_match(output)
    # Default is to auto-detect suffix type
    suffix_type <- rlang::arg_match(suffix_type)

    # Check if given 'path' is a dir or a file {{{2
    is_file <- utils::file_test("-f", path)
    is_dir <- utils::file_test("-d", path)
    is_model <- has_model_ext(path)
    # Stop if 'path' is neither a model nor a dir.
    assertthat::assert_that(any(all(is_file, is_model), is_dir),
        msg = msg("'path' should be a directory containing EnergyPlus simulation
                  results or a path of an EnergyPlus model where simulation
                  results locates.")
    )
    # }}}2
    # Get the suffix type of output {{{2
    # If 'path' is a directory
    if (is_dir) {
        ori_wd <- getwd()
        on.exit(setwd(ori_wd), add = TRUE)
        setwd(path)
        # Get all models in the directory
        model <- list.files(path, pattern = "\\.i[dm]f", ignore.case = TRUE, full.names = TRUE)
        # Get the prefix of model and handle same model with different ext
        prefix <- unique(file_prefix(model, basename = FALSE))
        assertthat::assert_that(assertthat::is.string(prefix),
            msg = msg("Input 'path' is a directory that contains more than one
                      EnergyPlus models. Please set 'path' to one path of the
                      models that you want to collect simulation results of.")
        )
    # If 'path' is a model path
    } else {
        model <- path
        # Get the prefix of model
        prefix <- file_prefix(model, basename = FALSE)
    }
    # Get the suffix of model
    if (suffix_type == "auto") suffix_type <- get_suffix_type(prefix)
    assertthat::assert_that(assertthat::is.string(suffix_type),
        msg = msg("The directory of 'path' contains results from one model but
                  with multiple output suffixes: ", csQuote(suffix_type), ". Please
                  specify 'suffix_type' explicitly, e.g. one of c('C', 'L', 'D').")
    )
    # }}}2
    # Get the output files {{{2
    file_to_read <- get_file_to_read(prefix = prefix, suffix = suffix_type, type = output)
    # }}}2

    data <- switch(output,
        variable = dplyr::tibble(model_prefix = basename(prefix),
            variable = purrr::set_names(purrr::map(file_to_read, read_variable,
                year = year, new_date = new_date, tz = tz, drop_na = drop_na,
                long_table = long_table),
                basename(prefix)
            )
        ),
        meter = dplyr::tibble(model_prefix = basename(prefix),
            meter = purrr::set_names(purrr::map(file_to_read, read_meter,
                year = year, new_date = new_date, tz = tz, drop_na = drop_na,
                long_table = long_table),
                basename(prefix)
            )
        ),
        table = dplyr::tibble(model_prefix = basename(prefix),
            table = purrr::set_names(
                purrr::map(file_to_read, read_table, which = which),
                basename(prefix)
            )
        ),
        surface_report = dplyr::tibble(model_prefix = basename(prefix),
            surface_report = purrr::set_names(
                purrr::map(file_to_read, read_surf_rpt),
                basename(prefix)
            )
        )
    )

    if (unnest) {
        data <- tidyr::unnest(data)
    }

    return(data)
}
# }}}1

#' @importFrom data.table data.table
# get_eplus_main_output_names
# {{{1
get_eplus_main_output_names <- function (output_prefix, output_pattern) {
    if (all(output_prefix == "in", output_pattern == "legacy")) {
        variable <- "eplusout.csv"
        meter <- "eplusmtr.csv"
        surf_rpt <- "eplusout.eio"
        table <- "eplustbl.csv"
    } else if (all(output_prefix != "in", output_pattern == "capital")) {
        variable <- paste0(output_prefix,  ".csv")
        meter <- paste0(output_prefix, "Meter.csv")
        surf_rpt <- paste0(output_prefix, ".eio")
        table <- paste0(output_prefix, "Table.csv")
    } else {
        stop("Could not detect the result names.")
    }

    main_names <- data.table(prefix = output_prefix,
                             pattern = output_pattern,
                             variable = variable, meter = meter,
                             surface_report = surf_rpt,
                             table = table)

    return(main_names)
}
# }}}1

#' @importFrom purrr map2
#' @importFrom data.table rbindlist
# get_eplus_main_output_files
# {{{1
get_eplus_main_output_files <- function (path) {
    output_prefix <- get_eplus_output_prefix_str(path = path)
    output_pattern <- get_eplus_output_prefix_ptn(output_prefix = output_prefix)
    file_names <- purrr::map2(output_prefix, output_pattern,
                              get_eplus_main_output_names)
    file_names <- data.table::rbindlist(file_names)

    return(file_names)
}
# }}}1

#' @importFrom purrr map
# check_eplus_output_file_exist
# {{{1
check_eplus_output_file_exist <- function (path, file_names, type) {
    input <- file_names[["prefix"]]
    files <- file_path(path, file_names[[type]])
    purrr::map(seq_along(files),
                   function(i) {
                       if (!file.exists(files[i])) {
                           message("EnergyPlus '", type ,"' file '",
                                   basename(files[i]),
                                   "' does not exist for input file '", input[i],
                                   "', and will be ignored during reading process.")
                           return(files[i])
                       } else {
                           return(NULL)
                       }
                   }) %>% unlist
}
# }}}1

#' Read EnergyPlus Surface Details Report from an .eio file.
#'
#' \code{read_surf_rpt} takes a file path of EnergyPlus .eio output file as
#' input, and returns a data.table object which contains the contents of the
#' report. It is worth noting that you have to add an "Output:Surfaces:List"
#' object in your model in order to generate an Surface Details Report in the
#' .eio output file.
#'
#' @param eio A path of an EnergyPlus .eio output file.
#' @return A data.table containing the Surface Details Report.
#' @importFrom stringr str_which str_split str_trim str_replace_all str_replace str_extract
#' @importFrom readr read_lines cols col_character col_double col_integer read_csv
#' @importFrom data.table rbindlist as.data.table fread
#' @importFrom purrr flatten_chr map2
#' @export
# read_surf_rpt
# {{{1
read_surf_rpt <- function(eio){
    # Read raw .eio file
    eio_contents <- readr::read_lines(eio)
    # Row num of all headers
    row_all <- stringr::str_which(eio_contents, "! <.*>")
    # Starting row num of surface details report
    row_surf <- stringr::str_which(eio_contents, "! <Zone/Shading Surfaces>,<Zone Name>/#Shading Surfaces,# Surfaces")
    row_header <- row_surf+1
    row_unit <- row_surf+2

    # Stop if there is no Surface Details Report
    if(length(row_surf) == 0){
        stop("'Surface Details Report' was not found in the eio file. ",
             "Please check if the 'Output:Surfaces:List' output exists in the IDF file.",
             call. = FALSE)
    }

    # Format output table headers
    header_name <- as.character(stringr::str_split(eio_contents[row_header], ",", simplify = TRUE))
    # Clean header characters
    header_name <- stringr::str_trim(stringr::str_replace_all(header_name, "(?:!\\s)*<(.*)>", "\\1"))
    header_name <- stringr::str_trim(stringr::str_replace_all(header_name, "^~", ""))
    header_unit <- as.character(stringr::str_split(eio_contents[row_unit], ",", simplify = TRUE))
    header_unit <- stringr::str_replace(header_unit, "! <Units>", "")
    header <- stringr::str_trim(paste(header_name, header_unit))
    col_types <- cols("HeatTransfer/Shading/Frame/Divider_Surface" = col_character(),
                      "Surface Name" = col_character(),
                      "Surface Class" = col_character(),
                      "Base Surface" = col_character(),
                      "Heat Transfer Algorithm" = col_character(),
                      "Construction/Transmittance Schedule" = col_character(),
                      "ExtBoundCondition" = col_character(),
                      "ExtConvCoeffCalc" = col_character(),
                      "IntConvCoeffCalc" = col_character(),
                      "SunExposure" = col_character(),
                      "WindExposure" = col_character(),
                      "#Sides" = col_integer(),
                      .default = col_double())

    # Raw table of surf info
    row_next_rpt <- purrr::detect(row_all, ~.x > row_unit)
    # If surface report is the last report
    if (is.null(row_next_rpt)) {
        surf_rpt <- eio_contents[row_surf:length(eio_contents)]
    } else {
        surf_rpt <- eio_contents[row_surf:(row_next_rpt -1)]
    }

    # Extract zone name per surface
    len <- length(surf_rpt)
    # Get the row number of zone info
    row_zone_start <- stringr::str_which(surf_rpt, "^(Shading_Surfaces|Zone_Surfaces),.*?,\\s*\\d")
    row_zone_end <- c(row_zone_start[-1]-1, len)
    row_zone_len <- row_zone_end - row_zone_start
    # Have to change the '# Surfaces' value as the original number excludes
    # 'Frame/Divider_Surface'.
    zone_surfaces_rev <- stringr::str_replace(surf_rpt[row_zone_start], "\\d+$", as.character((row_zone_len)))
    raw_zone_info <- stringr::str_c(rep(zone_surfaces_rev, row_zone_len), collapse = "\n")
    zone_info <- readr::read_csv(raw_zone_info,
                                 col_names = c("Zone/Shading Surfaces", "Zone Name/#Shading Surfaces", "# Surfaces"))

    # Table except sub header
    raw_per_zone <- purrr::flatten_chr(purrr::map2(row_zone_start, row_zone_len, ~{raw <- surf_rpt[(.x+1):(.x+.y)]}))
    # Supress warning messages from read_csv
    surf_info <- suppressWarnings(readr::read_csv(stringr::str_c(raw_per_zone, collapse = "\n"),
                                                  col_names = header, col_types = col_types,
                                                  na = c("", "NA", "N/A")))

    # Combine zone info and surface info per zone
    surf_info <- dplyr::bind_cols(zone_info, surf_info)

    return(surf_info)
}
# }}}1

#' @importFrom readr read_csv cols
#' @importFrom data.table as.data.table setnames
# read_meter: A function to take the path of EnergyPlus meter results and return
# a data.table of the contents with the first being a "POSIXt" column
# transformed from EnergyPlus standard "Date/Time".

# - 'meter': A path of EnergyPlus meter results. Normally a .csv file named
# (idf)Meter.csv or eplusmtr.csv.

# - 'year': An integer indicates the year value added to "Date/Time" column. If
# not specified, current calender year will be used.

# - 'eplus_date_col': The name of EnergyPlus standard datetime column. Normally
# "Date/Time".

# - 'new_date_col': A character indicates the name of the new transformed
# 'POSIXt' column.

# - 'tz': A character indicates the time zone of the transformed time column.
# The default value is the current system time zone.

# - 'rp_na': What will replace NA.

# - 'to_GJ': Whether converted the energy consumption from Joule to GigaJoule
# (1X10^9).

# - 'long': If TRUE, a long table will be returned with first column being the
# POSIXt column, and next 'component' indicating energy consumption components,
# 'type' indicating energy types (e.g. Electricity, and Gas), 'value' indicating
# the value of energy used, 'unit' indicating the unit of energy used, and
# 'timestep' indicating the tiem step of data collected. A meter output from a
# 10-min-timestep simulation will takes about 5 seconds to load.  So, use with
# caution.
# read_meter
# read_meter {{{1
read_meter <- function (path, year = current_year(), new_date = "datetime",
                        tz = Sys.timezone(), drop_na = FALSE, to_GJ = FALSE,
                        unnest = FALSE, long_table = FALSE) {
    meter <- read_variable(path = path, year = year, new_date = new_date,
        tz = tz, drop_na = drop_na, long_table = long_table
    )

    return(meter)
}
# }}}1

#' @importFrom readr read_csv cols
#' @importFrom data.table as.data.table
# read_variable: A function to take the path of EnergyPlus results and return a
# data.table of the contents with the first being a "POSIXt" column transformed
# from EnergyPlus standard "Date/Time".

# - 'result': A path of EnergyPlus meter results. Normally a .csv file named
# (idf).csv or eplusout.csv.

# - 'year': An integer indicates the year value added to "Date/Time" column. If
# not specified, current calender year will be used.

# - 'eplus_date_col': The name of EnergyPlus standard datetime column. Normally
# "Date/Time".

# - 'new_date_col': A character indicates the name of the new transformed
# 'POSIXt' column.

# - 'tz': A character indicates the time zone of the transformed time column.
# The default value is the current system time zone.

# - 'rp_na': What will replace NA.

# - 'long': If TRUE, a long table will be returned with first column being the
# POSIXt column, and next 'component' indicating energy consumption components,
# 'type' indicating energy types (e.g. Electricity, and Gas), 'value' indicating
# the value of energy used, 'unit' indicating the unit of energy used, and
# 'timestep' indicating the tiem step of data collected. A meter output from a
# 10-min-timestep simulation will takes about 5 seconds to load.  So, use with
# caution.
# read_variable
# read_variable {{{1
read_variable <- function (path, year = current_year(), new_date = "datetime",
                           tz = Sys.timezone(), drop_na = FALSE, long_table = FALSE) {
    assertthat::assert_that(assertthat::is.string(path))
    assertthat::assert_that(has_ext(path, "csv"))
    if (!file.exists(path)) return(NULL)
    csv <- suppressMessages(readr::read_csv(path))
    if (drop_na) {
        csv <- tidyr::drop_na(csv)
    }
    data <- dplyr::mutate(csv,
        rlang::UQ(new_date) := time_from_eplus(`Date/Time`, year = year, tz = tz))
    if (new_date != "Date/Time") {
        data <- dplyr::select(data,
            dplyr::one_of(new_date), dplyr::everything(), -`Date/Time`
        )
    }

    if (long_table) {
        data <- long_table_full(data)
   }

    return(data)
}
# }}}1

#' @importFrom tools file_ext
#' @importFrom stringr str_subset str_replace_all str_match str_split
#' @importFrom readr read_lines
#' @importFrom data.table as.data.table setnames setcolorder
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom purrr map set_names
# read_table: A function to read EnergyPlus table results.
# read_table
# read_table {{{1
read_table <- function (file, which = NULL) {
    assertthat::assert_that(assertthat::is.string(file))
    assertthat::assert_that(has_ext(file, ext = "html{0,1}"),
        msg = msg("'file' should have an extension of either '.htm' or '.html'.")
    )
    assertthat::assert_that(all(is.data.frame(which), ncol(which) == 3L),
        msg = msg("'which' should be a data.frame that contains 'Report', 'For',
                  'Table' info of tables you want to read.")
    )

    tbl_names <- dplyr::mutate(read_table_info(file, full_name = TRUE),
        id = seq_along(.data$full_name)
    )

    # Get table contents.
    tbls_raw <- rvest::html_nodes(xml2::read_html(file), "table")
    # Stop if the logic above results in a number mismatch of table names and
    # tables.
    assertthat::assert_that(identical(length(tbls_raw), nrow(tbl_names)),
        msg = msg("Error[Debug]: Mismatch length of extracted table names and
                  table number.")
    )

    # Get the table id to be extracted.
    if (!is.null(which)) {
        # Make sure 'which' has the column names to be used in joining.
        tbl_sel <- purrr::set_names(which, c("report", "for", "table"))
        # Check if there are invalid rows in 'which'.
        valid_sel <- dplyr::semi_join(tbl_sel, tbl_names)
        invalid_sel <- dplyr::anti_join(tbl_sel, valid_sel)
        if (nrow(invalid_sel) > 0L) {
            warning(msg("Invalid row found in 'which': ",
                "There is no table called ", sQuote(invalid_sel[["table"]]),
                " in report ", sQuote(invalid_sel[["report"]]),
                " for ", sQuote(invalid_sel[["for"]]), "."),
                "\nAll invalid nrows will be ignored. ", call. = FALSE)
        }
        id_tbl <- as.integer(dplyr::pull(dplyr::inner_join(tbl_names, valid_sel), id))
    } else {
        id_tbl <- 1:nrow(tbl_names)
    }

    tbls <- rvest::html_table(tbls_raw[id_tbl], header = TRUE)

    # Get the combined table names.
    rows <- tbl_names[id_tbl,]
    names <- paste0(
        "[Report]:", sQuote(rows[["report"]]), " | ",
        "[For]:", sQuote(rows[["for"]]), " | [Table]:", sQuote(rows[["table"]]))

    # Combine table names and contents.
    tbls <- purrr::set_names(tbls, names)
    # Always rename the first column to "Components".
    tbls <- purrr::map(tbls, ~{names(.x)[1] <- "Components"; .x})

    return(tbls)
}
# }}}1

# read_table_info {{{1
read_table_info <- function(file, full_name = FALSE) {
    assertthat::assert_that(assertthat::is.string(file))
    assertthat::assert_that(has_ext(file, ext = "html{0,1}"), msg = "'file'
                            should have an extension of either '.htm' or
                            '.html'")

    regex_tbl_name <- "<!-- FullName:(.*)-->"
    # Get table names.
    # NOTE: Did not find a way to extract comments in htm/htmls in 'rvest'
    # package. Have to use a ugly regex method.
    name_comments <- stringr::str_subset(readr::read_lines(file), regex_tbl_name)
    full_names <- stringr::str_replace(name_comments, regex_tbl_name, "\\1")
    tbl_names <- purrr::set_names(
        dplyr::as_tibble(str_split(full_names, "_", simplify = TRUE)),
        c("report", "for", "table")
    )
    if (full_name) {
        tbl_names <- dplyr::mutate(tbl_names, full_name = full_names)
        tbl_names <- dplyr::select(tbl_names, full_name, dplyr::everything())
    }

    return(tbl_names)
}
# }}}1
# long_table_full {{{1
long_table_full <- function (wide_table) {
    long_tbl <- long_table(data)
    info <- get_output_info(data)
    full_tbl <- dplyr::select(dplyr::full_join(long_tbl, info), -value, value)

    return(full_tbl)
}
# }}}1
# get_file_to_read {{{1
get_file_to_read <- function (prefix, suffix, type) {
    ext <- switch(type,
        # Currently `read_table` only support .htm(l) files.
        table = "html{0,1}",
        variable = "csv",
        meter = "csv",
        `surface report` = "eio"
    )

    # Get file candidates
    if (type != "surface report") {
        file_cand <- output_files(prefix, suffix_type = suffix, type = type)
    } else {
        file_cand <- output_files(prefix, suffix_type = suffix, ext = ".eio")
    }

    file <- file_cand[has_ext(file_cand, ext = ext)]

    return(file)
}
# }}}1
