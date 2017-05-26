################################################################################
#                          EnergyPlus Results Reading                          #
################################################################################

# import_epg: A function to create a data.table of simulation info from an
# EnergyPlus .epg file.
# {{{1
import_epg <- function(epg){

    sim_info <-
        data.table::as.data.table(readr::read_csv(file = epg, comment = "!",
                                                  col_names = c("model", "weather", "result", "run_times"),
                                                  col_types = cols(.default = "c", run_times = col_integer())))

    sim_info <- sim_info[, lapply(.SD, function(x) gsub(x=x, pattern = "\\\\", replacement = "/"))]
    sim_info <- sim_info[, `:=`(output_dir = dirname(result),
                                output_prefix = basename(result))]
    sim_info <- sim_info[, `:=`(result = NULL)]
    sim_info <- data.table::setcolorder(sim_info, c("model", "weather", "output_dir", "output_prefix", "run_times"))

    attr(sim_info, "job_type") <- "epg"

    return(sim_info[])
}
# }}}1

# import_jeplus: A function to create a data.table of simulation info fram a
# jEplus .json project file.
# {{{1
import_jeplus <- function (json) {
    # Read jeplus JSON project file.
    info <- jsonlite::fromJSON(json)

    # Get parameter info.
    params <- info[["parameters"]]

    param_name <- params[["name"]]

    param_field <- stringr::str_split(params[["searchString"]], "\\|")
    param_field <- purrr::set_names(param_field, param_name)

    param_value <- stringr::str_replace_all(params[["valuesString"]], "[\\{\\}]", "")
    param_value <- stringr::str_split(param_value, "(\\s)*,(\\s)*")

    # Get selected parameter values.
    param_value_selected <- params[["selectedAltValue"]]
    param_value <- map2(param_value_selected, param_value, ~{if (.x > 0) .y <- .y[.x] else .y})

    param_value <- purrr::map(param_value, ~stringr::str_split(.x, "(\\s)*\\|(\\s)*"))
    param_value <- purrr::set_names(param_value, param_name)

    # Create case names according to parameter names.
    case_names <- purrr::map2(param_name, param_value, ~paste0(.x, seq_along(.y)))
    case_names <- data.table::rbindlist(purrr::cross_n(case_names))
    case_names <- purrr::flatten_chr(purrr::by_row(case_names, ~paste(.x, collapse = "_"))[[".out"]])

    # Get all combination of case values.
    param_value <- purrr::cross_n(param_value)
    param_value <- set_names(param_value, case_names)


    # Get input file info.
    idfs <- purrr::flatten_chr(stringr::str_split(info[["idftemplate"]], "\\s*;\\s*"))
    wthrs <- purrr::flatten_chr(stringr::str_split(info[["weatherFile"]], "\\s*;\\s*"))
    idf_path <- paste0(info[["idfdir"]], idfs)
    wthr_path <- paste0(info[["weatherDir"]], wthrs)

    sim_info <- list(idf_path = idf_path, weather_path = wthr_path,
                     param_field = param_field, param_value = param_value)

    attr(sim_info, "job_type") <- "jeplus"

    return(sim_info)
}
# }}}1

# read_eplus: A function to read EnergyPlus simulation results.
# {{{1
read_eplus <- function (path, output = "variable",
                        year = current_year(), eplus_date_col = "Date/Time",
                        new_date_col = "datetime", tz = Sys.timezone(),
                        rp_na = NA, to_GJ = NULL, unnest = FALSE, long = FALSE) {
    # Check if the input model path is given.
    ext <- tools::file_ext(path)
    if (ext != "") {
        if (length(grep("i[dm]f", ext, ignore.case = TRUE)) == 0) {
            stop("'path' should be a path of folder or a path of the input .idf or .imf file.",
                 call. = FALSE)
        } else {
            prefix = tools::file_path_sans_ext(basename(path))
            file_names <- data.table(prefix = prefix,
                                     variable = paste0(prefix, ".csv"),
                                     meter = paste0(prefix, "Meter.csv"),
                                     surface_report = paste0(prefix, ".eio"),
                                     table = paste0(prefix, "Table.csv"))
            path <- dirname(path)
        }
    } else {
        # Get the output name pattern.
        file_names <- get_eplus_main_output_files(path)
    }

    if (is.null(to_GJ)) {
        to_GJ <- FALSE
    }

    if (is.na(match(output, c("variable", "meter", "table", "surface report")))) {
        stop("Invalid value of argument 'output'. It should be one of ",
             "c('variable', 'meter', 'table', 'surface report').", call. = FALSE)
    } else if (output == "variable"){
        file_name <- file_names[["variable"]]
    } else if (output == "meter"){
        file_name <- file_names[["meter"]]
    } else if (output == "surface report"){
        file_name <- file_names[["surface_report"]]
    } else {
        file_name <- file_names[["table"]]
    }

    check_eplus_output_file_exist(path, file_names, output)

    if (output == "variable") {
      data <-
          dplyr::tibble(case = file_names[["prefix"]],
                        variable_output = purrr::map(file.path(path, file_name),
                                                     function (file) {
                                                         if (file.exists(file)) {
                                                             read_variable(result = file,
                                                                               year = year,
                                                                               eplus_date_col = eplus_date_col,
                                                                               new_date_col = new_date_col,
                                                                               tz = tz,
                                                                               rp_na = rp_na,
                                                                               long = long)
                                                         } else {
                                                             return(NULL)
                                                         }
                                                     })) %>% data.table::as.data.table() #%>% tidyr::unnest()

    } else if (output == "meter"){
      data <-
          dplyr::tibble(case = file_names[["prefix"]],
                        meter_output = purrr::map(file.path(path, file_name),
                                                  function (file) {
                                                      if (file.exists(file)) {
                                                          read_variable(result = file,
                                                                            year = year,
                                                                            eplus_date_col = eplus_date_col,
                                                                            new_date_col = new_date_col,
                                                                            tz = tz,
                                                                            to_GJ = to_GJ,
                                                                            rp_na = rp_na,
                                                                            long = long)
                                                      } else {
                                                          return(NULL)
                                                      }
                                                  }
                                                  )) %>% data.table::as.data.table() #%>% tidyr::unnest()

    } else if (output == "surface report") {
      data <-
          dplyr::tibble(case = file_names[["prefix"]],
                        surface_report = purrr::map(file.path(path, file_name),
                                                    function (file) {
                                                        if (file.exists(file)) {
                                                            read_surf_rpt(eio = file)
                                                        } else {
                                                            return(NULL)
                                                        }
                                                    }
                                                    )) %>% data.table::as.data.table() #%>% tidyr::unnest()
    } else  {
      data <-
          dplyr::tibble(case = file_names[["prefix"]],
                        table_output = purrr::map(file.path(path, file_name),
                                                  function (file) {
                                                      if (file.exists(file)) {
                                                          read_table(file = file)
                                                      } else {
                                                          return(NULL)
                                                      }
                                                  }
                                                  )) %>% data.table::as.data.table() #%>% tidyr::unnest()
    }

    if (unnest) {
        data <- tidyr::unnest(data = data)
    }

    return(data)
}
# }}}1

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

# check_eplus_output_file_exist
# {{{1
check_eplus_output_file_exist <- function (path, file_names, type) {
    input <- file_names[["prefix"]]
    files <- file.path(path, file_names[[type]])
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

# read_surf_rpt: A function to read EnergyPlus Surface Details Report in eio file.
# {{{1
read_surf_rpt <- function(eio){
    # Read raw .eio file
    eio <- readr::read_lines(eio)
    # Row num of all headers
    rownum_all <- eio %>% grep(x=., "! <.*>", value = F)
    # Starting row num of surface details report
    rownum_surf <- eio %>%
        grep(x=., "! <Zone/Shading Surfaces>,<Zone Name>/#Shading Surfaces,# Surfaces")

    # Stop if there is no Surface Details Report
    if(length(rownum_surf) == 0){
        stop("'Surface Details Report' was not found in the eio file. ",
             "Please check if the 'Output:Surfaces:List' output exists in the IDF file.")
    }

    # Format output table headers
    header <-
        map2_chr(eio[rownum_surf+1] %>% stringr::str_split(pattern = ",") %>% unlist(),
                 eio[rownum_surf+2] %>% stringr::str_split(pattern = ",") %>% unlist(),
                 ~paste(.x,.y)) %>%
        gsub(x=., "!\\s", "") %>% gsub(x=., "<|>", "") %>% stringr::str_trim()

    # Raw table of surf info
    surf_info_raw <-
        eio[rownum_surf:rownum_all[grep(x=rownum_all, pattern = rownum_surf)+3]] %>% .[-length(.)]

    # Extract zone name per surface
    zone_name <-
        purrr::map2(surf_info_raw %>% grep(x=., "Shading_Surfaces|Zone_Surfaces", value = F) %>%
                    c(., length(surf_info_raw)+1) %>% diff(.)-1,
                surf_info_raw %>% grep(x=., "Shading_Surfaces|Zone_Surfaces", value = T),
                ~replicate(.x,.y)) %>% unlist() %>% paste0(collapse = "\n") %>%
    data.table::fread(col.names = c("Zone/Shading Surfaces", "Zone Name/#Shading Surfaces"),drop=3)

    # Table except sub header
    surf_info  <-
        surf_info_raw[-c(1:3)] %>%
        grep(x=., "^(?!Zone_Surfaces|Shading_Surfaces)", perl=T, value=T) %>%
        paste0(., collapse = "\n") %>%
        readr::read_csv(col_names = header) %>% data.table::as.data.table() %>%
        .[, cbind(zone_name, .SD)]

    return(surf_info)
}
# }}}1

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
# {{{1
read_meter <- function (meter, year = current_year(), eplus_date_col = "Date/Time",
                              new_date_col = "datetime", tz = Sys.timezone(),
                              rp_na = 0L, to_GJ = FALSE, long = FALSE) {
    meter <-
        readr::read_csv(meter, col_types = cols(.default = "d", `Date/Time` = "c")) %>%
        data.table::as.data.table() %>%
        na_replace(type = "na", replacement = rp_na) %>%
        eplus_time_trans(year = year, eplus_date_col = eplus_date_col,
                         new_date_col = new_date_col, tz = tz, keep_ori = FALSE)

    meter <- meter %>%
        data.table::setnames(col_names(., c("Electricity:Facility", "Gas:Facility")),
                             col_names(., c("Electricity:Facility", "Gas:Facility")) %>%
                                 gsub(x =.,  "(.*):(.*)\\s", "\\2:\\1 ")) %>% .[]

    if (to_GJ) {
        meter <- meter[, lapply(.SD, function(x) round(x/1E9, digits = 4)),
                       by = c(new_date_col)] %>%
                       data.table::setnames(col_names(., new_date_col, invert = TRUE),
                                            col_names(., new_date_col, invert = TRUE) %>%
                                                gsub(x = ., pattern = "\\[J\\]", replacement = "\\[GJ\\]"))
    }

    if (long) {
        meter <- long_table(meter)
    }

    return(meter)
}
# }}}1

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
# {{{1
read_variable <- function (result, year = current_year(), eplus_date_col = "Date/Time",
                               new_date_col = "datetime", tz = Sys.timezone(),
                               rp_na = NA, long = FALSE) {
    result <-
        readr::read_csv(result, col_types = cols(.default = "d", `Date/Time` = "c")) %>%
        data.table::as.data.table() %>%
        na_replace(type = "na", replacement = rp_na) %>%
        eplus_time_trans(year = year, eplus_date_col = eplus_date_col,
                         new_date_col = new_date_col, tz = tz, keep_ori = FALSE)

    if (long) {
        result <- long_table(result)
   }

    return(result)
}
# }}}1

# read_epg: A function to read EnergyPlus simulation results grouped by an *.epg
# file.
# {{{1
# TODO: merge readTable into it.
read_epg <- function(epg, results = "meter", case_ref = "idf"){
    # Read simluation info from *.epg file.
    sim_info <-  import_epg(epg)

    if (is.na(match(table, c("variable", "meter", "table", "surface report")))) {
        stop("Invalid value of argument 'results'. It should be one of ",
             "c('variable', 'meter', 'table', 'surface report').", call. = FALSE)
    } else if (table == "variable"){
        file_suffix <- ".csv"
    } else if (table == "meter"){
        file_suffix <- "Meter.csv"
    } else if (table == "surface report"){
        file_suffix <- ".eio"
    } else {
        file_suffix <- "Table.csv"
    }

    if (is.na(match(case_ref, c("idf", "weather", "result")))) {
        stop("Invalid 'case_ref'. 'case_ref' should be one of c('idf', ",
             "'weather', 'result').", call. = FALSE)
    } else if (case_ref == "idf") {
        case_col <- "idf"
    } else if (case_ref == "weather") {
        case_col <- "weather"
    } else {
        case_col <- "result_suffix"
    }

    if(table == "variable" | table == "meter"){
      data <-
          dplyr::tibble(Case = sim_info[, get(case_col)],
                        results = purrr::map(paste(sim_info[, ResultPath], file_pattern, sep = ""),
                                             ~readr::read_csv(.))) %>%
      tidyr::unnest() %>% data.table::as.data.table() %>%
      naReplace() %>%
      eplus_time_trans()
  }else if(table == "surface report"){
      data <-
          dplyr::tibble(Case = sim_info[, get(case_col)],
                        results = purrr::map(paste(sim_info[, ResultPath], file_pattern, sep = ""),
                                             ~read_surf_rpt(.))) %>%
      tidyr::unnest() %>% data.table::as.data.table()
  }else{
      data <-
          dplyr::tibble(Case = sim_info[, get(case_col)],
                        results = map(paste(sim_info[, ResultPath], file_pattern, sep = ""),
                                      ~readr::read_csv(.))) %>%
      tidyr::unnest() %>% data.table::as.data.table()
  }

  if((to.src)*(table == "meter")){
      data %<>% siteToSrc()
  }else if(all(to.src, (table != "meter"))){
      stop("'to.src' only applys when 'table' is 'meter'.")
  }

  return(data)
}
# }}}1

# read_table: A function to read EnergyPlus table results.
# {{{1
read_table <- function (file, name = c("report", "for", "table"), regex = FALSE) {
    # Check input.
    if (all(!identical(tools::file_ext(basename(file)), "htm"),
            !identical(tools::file_ext(basename(file)), "html"))) {
        stop("Input file should be a .htm/.html file.", call. = FALSE)
    }
    if (missingArg(name)) {
        stop("Please give 'name' value.", call. = FALSE)
    }
    if (any(!is.character(name), length(name) != 3)) {
        stop("'name' should be a character vector of length 3 indicating the ",
             "name of report, the name of 'for' and the name of table.", call. = FALSE)
    }

    regex_tbl_name <- "<!-- FullName:(.*)-->"
    # Get table names.
    # NOTE: Did not find a way to extract comments in htm/htmls in 'rvest'
    # package. Have to use a ugly regex method.
    tbl_name_comments <- stringr::str_subset(readr::read_lines(file), regex_tbl_name)
    tbl_full_names <- stringr::str_replace_all(tbl_name_comments, regex_tbl_name, "\\1")
    tbl_name_split <- data.table::as.data.table(stringr::str_match(tbl_full_names, "(.*)_(.*)"))
    tbl_name_split <- data.table::setnames(tbl_name_split, c("full_name", "report_for", "table"))
    tbl_name_split <- tbl_name_split[, c("report", "for") := as.data.frame(str_split(report_for, "_", 2, simplify = TRUE))][,
                                     c("full_name", "report_for") := NULL]
    tbl_names <- setcolorder(tbl_name_split, c("report", "for", "table"))

    if (!regex) {
        # Set 'which' to TRUE without '.j' will return the row number. Or can
        # use method: DT[  , .I[X=="B"] ]
        # Borrowed from: http://stackoverflow.com/questions/22408306/using-i-to-return-row-numbers-with-data-table-package
        table_id <- tbl_names[report == name[1] & `for` == name[2] & table == name[3], which = TRUE]
    } else {
        report_names <- unique(as.character(tbl_names[, report]))
        report_sel <- stringr::str_subset(report_names, name[1])

        for_names <- unique(as.character(tbl_names[, `for`]))
        for_sel <- stringr::str_subset(for_names, name[2])

        table_names <- unique(as.character(tbl_names[, table]))
        table_sel <- stringr::str_subset(table_names, name[3])

        table_id <- tbl_names[report %in% report_sel &
                              `for` %in% for_sel &
                              table %in% table_sel, which = TRUE]
    }

    if (length(table_id) == 0) {
        stop("No matched table found. Please check the value of 'name' or set ",
             "'regex' to TRUE if you want to extract multiple tables using ",
             "regular expressions.", call. = FALSE)
    }

    # Get table contents.
    tbls_raw <- rvest::html_nodes(xml2::read_html(file), "table")
    tbls <- rvest::html_table(tbls_raw[table_id], header = TRUE)

    # Get the combined table names.
    names <- tbl_names[table_id, paste0("[Report]:(", report, ") [For]:(", `for`, ") [Table]:(", table, ")")]

    # Combine table names and contents.
    tbls <- set_names(tbls, names)
    # Always rename the first column to "Components".
    tbls <- map(tbls, ~{names(.x)[1] <- "Components"; .x})

    return(tbls)

}
# }}}1
