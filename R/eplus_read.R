################################################################################
#                          EnergyPlus Results Reading                          #
################################################################################

# eplus_epg_read: A function to create a data.table of simulation info from a
#                 EnergyPlus *.epg file.
# {{{1
eplus_epg_read <- function(epg){

    sim_info <-
        readr::read_csv(file = epg, comment = "!",
                        col_names = c("idf", "weather", "result", "run_times"),
                        col_types = cols(.default = "c", run_times = col_integer())) %>%
        data.table::as.data.table() %>%
        .[,lapply(.SD, function(x) gsub(x=x, pattern = "\\\\", replacement = "/"))] %>%
        .[, `:=`(idf_name      = basename(idf),
                 idf_path      = dirname(idf),
                 weather_name  = basename(weather),
                 weather_path  = dirname(weather),
                 result_prefix = basename(result),
                 result_path   = dirname(result))] %>%
        .[, `:=`(idf = NULL, weather = NULL, result = NULL)] %>%
        data.table::setcolorder(c("idf_name", "weather_name", "result_prefix",
                                  "idf_path", "weather_path", "result_path", "run_times")) %>%
        .[]
    # if(full.path){
    #     epg_file <- dir(path, full.names = TRUE)
    # }else{
    #     epg_file <- dir(path, pattern = ".epg$", recursive = F, full.names = TRUE)
    # }
    # sim_info <-
    #     fread(epg_file, skip = 9, col.names = c("IDFFile", "WeatherFile", "ResultPath"), drop = 4) %>%
    #     .[,lapply(.SD, function(x) gsub(x=x, pattern = "\\\\", replacement = "/"))]

    # if(relative.path){
    #     sim_info %<>%
    #         .[, ResultFolder := ResultPath] %>%
    #         .[, lapply(.SD, function(x) gsub(x=x, pattern = ".*/(.*)$",
    #                                          replacement = "\\1", ignore.case = T, perl = T)),
# by = .(ResultPath)] %>%
    # setcolorder(c("IDFFile", "WeatherFile", "ResultFolder", "ResultPath")) %>% .[]
    # }

    return(sim_info)
}
# }}}1

# eplus_read: A function to read EnergyPlus simulation results.
# {{{1
eplus_read <- function (path, output = "variable",
                        year = current_year(), eplus_date_col = "Date/Time",
                        new_date_col = "datetime", tz = Sys.timezone(),
                        rp_na = NA, to_GJ = FALSE, long = FALSE) {
    # Get the output name pattern.
    file_names <- get_eplus_main_output_files(path)

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
                                                             eplus_result_read(result = file,
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
                                                          eplus_result_read(result = file,
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
                                                            eplus_surf_rpt_read(eio = file)
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
                                                          eplus_table_read(table = file)
                                                      } else {
                                                          return(NULL)
                                                      }
                                                  }
                                                  )) %>% data.table::as.data.table() #%>% tidyr::unnest()
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

# eplus_surf_rpt_read: A function to read EnergyPlus Surface Details Report in eio file.
# {{{1
eplus_surf_rpt_read <- function(eio){
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

# eplus_meter_read: A function to take the path of EnergyPlus meter results and
#                   return a data.table of the contents with the first being a
#                   "POSIXt" column transformed from EnergyPlus standard
#                   "Date/Time".

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
eplus_meter_read <- function (meter, year = current_year(), eplus_date_col = "Date/Time",
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

# eplus_result_read: A function to take the path of EnergyPlus results and
#                    return a data.table of the contents with the first being a
#                    "POSIXt" column transformed from EnergyPlus standard
#                    "Date/Time".

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
eplus_result_read <- function (result, year = current_year(), eplus_date_col = "Date/Time",
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

# eplus_epg_sim_read: A function to read EnergyPlus simulation results grouped
#                     by an *.epg file.
# {{{1
# TODO: merge readTable into it.
eplus_epg_sim_read <- function(epg, results = "meter", case_ref = "idf"){
    # Read simluation info from *.epg file.
    sim_info <-  eplus_epg_read(epg)

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
                                             ~eplus_surf_rpt_read(.))) %>%
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

# eplus_tbl_info_read: A helper function to extract range info EnergyPlus csv
#                      format table results.
# {{{1
eplus_tbl_info_read <- function(table){
    # Read raw table results {{{2
    raw_table <-
        readr::read_lines(table)[-(1:6)] # exclude first 6 lines that are of no use.
    # }}}2
    # Get report names {{{2
    report_category <-
        grep(x = raw_table, pattern = "report:", ignore.case = TRUE, value = TRUE)
    report_category <-
        gsub(x = report_category, pattern = ".*,", replacement = "")

    report_for <-
        grep(x = raw_table, pattern = "report:", ignore.case = TRUE)
    report_for <-
        gsub(x = raw_table[`+`(report_for, +1)], pattern = ".*,", replacement = "")

    report_name <- paste(report_category, report_for, sep = ":")
    ## }}}2
    # Get report range {{{2
    report_rowstart <-
         grep(x = raw_table, pattern = "report:", ignore.case = TRUE)

    report_length <-
        diff(c(report_rowstart, length(raw_table)))
    # }}}2
    # table info extraction {{{2
    # Get raw table contents
    table_rows <- grep(x = raw_table, pattern = "^,")
    # Get table range
    table_startrow <- table_rows[c(min(table_rows), diff(table_rows)) > 1]
    table_endrow <- table_rows[c(diff(table_rows), max(table_rows)) > 1]
    # all table names
    table_name <-  raw_table[`-`(table_startrow, 2)]
    # table range
    table_range <- data.table::data.table(table_name, table_startrow, table_endrow)

    table_info <-
        dplyr::tibble(report_name = report_name,
                      table_name = purrr::map2(report_rowstart, report_length,
                                               function(x, y){
                                                   report_range <- seq(from = x, length.out = y, by = 1)
                                                   report <- raw_table[report_range]
                                                   table_rows <- grep(x = report, pattern = "^,", value = FALSE)
                                                   table_startrow <- table_rows[c(min(table_rows), diff(table_rows)) > 1]
                                                   table_endrow <- table_rows[c(diff(table_rows), max(table_rows)) > 1]
                                                   table_name_per_report <- report[`-`(table_startrow, 2)]
                                                   data.table::data.table(table_name_per_report, table_startrow, table_endrow)
                                               }))
    table_info <- data.table::as.data.table(tidyr::unnest(table_info))

    # Delete table that has no contents to avoid errors when extract table contents
    table_info[table_startrow != table_endrow]
    # }}}2
  return(table_info)
}
# }}}1

# eplus_table_read: A function to read EnergyPlus table results
# {{{1
eplus_table_read <- function(table){
    # Read raw table results {{{2
    raw_table <- readr::read_lines(table)[-(1:6)] # exclude first 6 lines that are of no use.
    # }}}2
    # Extract tables names and ranges per report {{{2
    table_info <- eplus_tbl_info_read(table)
    # }}}2
    # Extract tables names and ranges without report info {{{2
    table_rows <- grep(x = raw_table, pattern = "^,", value = FALSE)

    table_startrow <- table_rows[c(min(table_rows), diff(table_rows)) > 1]
    table_endrow <- table_rows[c(diff(table_rows), max(table_rows)) > 1]
    table_name <- raw_table[`-`(table_startrow, 2)]
    table_range <- data.table::data.table(table_name, table_startrow, table_endrow)

    table_start <- table_range[table_startrow != table_endrow, table_startrow]
    table_end <- table_range[table_startrow != table_endrow, table_endrow]
    table_name <- table_range[table_startrow != table_endrow, table_name]
    # }}}2
    # Extract table contents with table names {{{2
    tables <- purrr::map2(table_start, table_end,
                          function(x, y){
                              str <- paste0(raw_table[x:y], collapse = "\n")
                              table <- data.table::as.data.table(readr::read_csv(str))
                              table <- table[, X1 := NULL]
                              table <- data.table::setnames(table, "X2", "Components")})
    tables <- purrr::set_names(tables, table_name)
  # }}}2
    # Group tables by report {{{2
    unique_rptname <- unique(table_info$report_name)
    group_list <-
        purrr::map(unique_rptname,
                   function(rptname){
                       grep(x=table_info$report_name, rptname, fixed = TRUE)
                   })
    group_start <- purrr::map_int(group_list, min)
    group_end <- purrr::map_int(group_list, max)

    table <- purrr::map2(group_start,group_end,
                         function(x,y){
                             list(tables[x:y])
                         })

    names(table) <- unique_rptname
    # }}}2
    return(table)
}
# }}}1
