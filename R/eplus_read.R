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
        meter <- meter %>% long_table()
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
        result <- result %>% long_table()
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
        case_col <- "idf"
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
        raw_table %>% grep(x=., pattern = "report:", ignore.case = T, value = T) %>%
        gsub(x=., pattern = ".*,", "")

    report_for <-
        raw_table %>% grep(x=., pattern = "report:", ignore.case = T, perl = T) %>%
        `+`(., +1) %>% raw_table[.] %>% gsub(x=., pattern = ".*,", "")

    report_name <-
        data.table::data.table(report_category, report_for) %>%
        .[, (report_name = paste(report_category, report_for, sep = ":"))]
    ## }}}2
    # Get report range {{{2
    report_rowstart <-
        raw_table %>% grep(x=., pattern = "report:", ignore.case = T, perl = T)

    report_length <-
        raw_table %>% grep(x=., pattern = "report:", ignore.case = T, perl = T) %>%
        c(., length(raw_table)) %>% diff()
    # }}}2
    # table info extraction {{{2
    # Get raw table contents
    table_rows <-
        raw_table %>% grep(x=., pattern = "^,", value = F)
    # Get table range
    table_startrow <-
        table_rows[c(min(table_rows), diff(table_rows)) > 1]
    table_endrow <-
        table_rows[c(diff(table_rows), max(table_rows)) > 1]
    ## all table names
    table_name <- `-`(table_startrow,2) %>% raw_table[.]
    ## table range
    table_range <- data.table::data.table(table_name, table_startrow, table_endrow)

    table_info <-
        dplyr::tibble(report_name = report_name,
                      table_name = purrr::map2(report_rowstart, report_length,
                                               function(x,y){
                                                   report_range <- seq(from = x, length.out = y, by = 1)
                                                   report <- raw_table[report_range]
                                                   table_rows <-
                                                       report %>% grep(x=., pattern = "^,", value = F)
                                                   table_startrow <-
                                                       table_rows[c(min(table_rows), diff(table_rows)) > 1]
                                                   table_endrow <-
                                                       table_rows[c(diff(table_rows), max(table_rows)) > 1]
                                                   table_name_per_report <- `-`(table_startrow,2) %>% report[.]
                                                   data.table::data.table(table_name_per_report, table_startrow, table_endrow)
                                               })) %>%
    tidyr::unnest() %>% data.table::as.data.table()

    # Delete table that has no contents to avoid errors when extract table contents
    table_info[table_startrow!=table_endrow]
    # }}}2
  return(table_info)
}
# }}}1

# eplus_table_read: A function to read EnergyPlus table results
# {{{1
eplus_table_read <- function(table){
    # Read raw table results {{{2
    raw_table <-
        readr::read_lines(table)[-(1:6)] # exclude first 6 lines that are of no use.
    # }}}2
    # Extract tables names and ranges per report {{{2
    table_info <- eplus_tbl_info_read(table)
    # }}}2
    # Extract tables names and ranges without report info {{{2
    table_rows <-
      raw_table %>% grep(x=., pattern = "^,", value = F)

    table_startrow <-
      table_rows[c(min(table_rows), diff(table_rows)) > 1]

    table_endrow <-
      table_rows[c(diff(table_rows), max(table_rows)) > 1]

    table_name <- `-`(table_startrow,2) %>% raw_table[.]

    table_start <-
      data.table::data.table(table_name, table_startrow, table_endrow) %>%
      .[table_startrow != table_endrow] %>% .[, table_startrow]

    table_end <-
      data.table::data.table(table_name, table_startrow, table_endrow) %>%
      .[table_startrow != table_endrow] %>% .[, table_endrow]

    table_name <-
      data.table::data.table(table_name, table_startrow, table_endrow) %>%
      .[table_startrow != table_endrow] %>% .[, table_name]
    # }}}2
  # Extract table contents with table names {{{2
  tables <- purrr::map2(table_start, table_end,
                        function(x,y){
                            raw_table[x:y] %>%
                                paste0(., collapse = "\n") %>% readr::read_csv() %>%
                                data.table::as.data.table() %>% .[, X1:=NULL] %>%
                                data.table::setnames("X2", "Components") %>% .[] }) %>%
            purrr::set_names(table_name)

  # }}}2
    # Group tables by report {{{2
    unique_rptname <-
        table_info %>% .[, report_name] %>% unique()
    group_list <-
        purrr::map(unique_rptname, function(rptname){
                   grep(x=table_info$report_name, rptname, fixed = T)})
    group_start <- group_list %>% purrr::map_int(.,min)
    group_end <- group_list %>% purrr::map_int(.,max)

    table <-
        purrr::map2(group_start,group_end,
                    function(x,y){
                        list(tables[x:y])
                   })

    names(table) <- unique_rptname
    # }}}2
    return(table)
}
# }}}1
