################################################################################
#                         EnergyPlus Data Manipulation                         #
################################################################################

#' Column name extraction using RegEx.
#'
#' \code{col_names} returns all column names that are matched by the specifed
#' RegEx. Basically, it is a wrapper function of \code{\link[base]{grep}} but
#' can take multiple RegExs at a time.
#'
#' @param data A data frame that you want to extract column names from.
#' @param pattern A RegEx used for column name extraction. If multiple patterns
#' are given, then all items in the vector will be seperately used for
#' searching. Default is ".*", which will return all column names of the input
#' data frame.
#' @param regex If FALSE, \code{pattern} is treated as a character vector of
#' column names.
#' @param ignore_case A logical value. If FALSE, the pattern matching is case
#' sensitive and if TRUE, case is ignored during matching. Default is TRUE.
#' @param invert A logical value. If TRUE return column names that do not match.
#' Default is FALSE.
#' @return A character vector of matched column names.
#' @importFrom purrr map flatten_chr
#' @export
#' @examples
#' col_names(mtcars)
#' col_names(mtcars, "t$")
#' col_names(mtcars, c("^c", "t$"))
#' col_names(mtcars, "cyl", invert = TRUE)
#' col_names(mtcars, c("cly", "disp"), regex = FALSE)
# col_names
# {{{1
col_names <- function (data, pattern = ".*", regex = TRUE,
                       ignore_case = TRUE, invert = FALSE) {
    check_df(data)

    name <- colnames(data)

    if (!regex) {
        colnames <- pattern[!is.na(match(pattern, name))]
        missings <- pattern[is.na(match(pattern, name))]
        if (length(missings) > 0) {
            warning("Column ", paste(paste0("'", missings, "'"), collapse = ", "),
                    " not found in the input data.", call. = FALSE)
        }
    } else {
        colnames <- purrr::map(pattern,
                               ~grep(x = name, .x, ignore.case = ignore_case,
                                     perl = TRUE, value = TRUE, invert = FALSE))
        colnames <- unique(purrr::flatten_chr(colnames))
    }

    if (invert) {
        colnames <- setdiff(name, colnames)
    }

    return(colnames)
}
# }}}1

#' Column selection using RegEx in a data.table.
#'
#' \code{col_select} will first check whether the input is a data.table object
#' or not. If not, it will convert it into a data.table and all row names will
#' be lost.
#'
#' @param data A data.frame or data.table to subset culumns from.
#' @param col A RegEx used for column subsetting. If blank, all columns be
#' returned.
#' @param by A character vector used for grouped column subsetting using the
#' feature of \code{by} argument in data.table. Especially useful when you want
#' grouping or cases when the \code{by} columns are not easy to be combined with
#' \code{col}. NOTE: \code{by} is not a regex, but a character vector.
#' @param regex If FALSE, \code{col} is treated as a character vector of
#' column names, not a regex.
#' @param ignore_case A logical value. If FALSE, the pattern matching is case
#' sensitive and if TRUE, case is ignored during matching. Default is TRUE.
#' \code{ignore_case} only takes effect on \code{col} not \code{by}.
#' @param invert A logical value. If TRUE return column names that do not match.
#' Default is FALSE. \code{invert} only takes effect on \code{col} not
#' \code{by}.
#' @return A data.table that only contains the subsetted columns.
#' @export
#' @examples
#' col_select(mtcars, "t$")
# col_select
# {{{1
col_select <- function (data, col = NULL, by = NULL, regex = TRUE,
                        ignore_case = TRUE, invert = FALSE) {
    check_df(data)
    data <- conv_dt(data)

    # If no 'col' given, return the input data as it is.
    if (is.null(col)) {
        sel_data <- data
    } else {
        sel_col <- col_names(data, pattern = col, regex = regex,
                             ignore_case = ignore_case, invert = invert)
        if (length(sel_col) == 0) {
            stop("No matched column found.", call. = FALSE)
        }
        if (is.null(by)) {
            sel_data <- data[, ..sel_col]
        } else {
            by_col <- col_names(data, pattern = by, regex = FALSE)
            if (length(by_col) == 0) {
                stop("No matched by column found.\n",
                     "NOTE: `by` is not parsed as a regex but a character vector of column names.",
                     call. = FALSE)
            } else {
                sel_data <- data[, .SD, .SDcol = sel_col, by = by_col]
            }
        }
    }

    return(sel_data)
}
# }}}1

#' Row subsetting using RegEx in a data.table.
#'
#' \code{row_select} is a function for easy row subsetting using RegEx in one
#' column in a data.table, especially when the target column is a character
#' type. It will first check whether the input is a data.table object or not. If
#' not, it will convert it into a data.table and all row names will be lost.
#'
#' @param data A data.frame or data.table to subset culumns from.
#' @param based_col A character column name used for row subsetting.
#' @param row_pattern A RegEx used for row subsetting.
#' @return It returns a data.table with subsetted rows.
#' @export
#' @examples
#' row_select(iris, "Species", "^(?!setosa)")
# row_select
# {{{1
row_select <- function(data, based_col = "Variable", row_pattern = ""){
    check_df(data)
    data <- conv_dt(data)

    if (row_pattern == "") {
        stop("Argument 'row_pattern' has to be specified.")
    }

    # Get all unique values in based column.
    var_list <- unique(data[[based_col]])

    row_selected <-
        data[get(based_col) %in% grep(x = var_list, pattern = row_pattern,
                                      ignore.case = TRUE, perl = TRUE, value= TRUE)]
    return(row_selected)
}
# }}}1

#' A function to return an integer of current calender year.
#'
#' @return An integer of current calender year.
#' @importFrom lubridate year
#' @export
#' @examples
#' current_year()
# current_year
# {{{1
current_year <- function () {
    year <- lubridate::year(Sys.Date())
    return(year)
}
# }}}1

#' Create an annual time sequence for data easily merging.
#'
#' \{annual_seq} is a function used for creating an annual time sequence and can
#' is used internally in \code{\link{annual_sch}}.
#'
#' @param interval An integer indicates the interval mins of the time sequence.
#' @param year An integer indicates the year of the time sequence. The default
#' value is the current year.
#' @param tz A character indicates the time zone of the time sequence. The
#' default value is the current system time zone.
#' @param down If TRUE, the time sequence will start from '01-01 00:00:00'
#' offsetted by inverval.
#' @param leap_day If FALSE, and the input year is a leap year, '02-29' related
#' data will be deleted.
#' @return It returns a time sequence of giving year and inverval.
#' @importFrom lubridate minutes
#' @export
# annual_seq
# {{{1
annual_seq <- function(interval = 1L, year = current_year(), tz = Sys.timezone(),
                       down = TRUE, leap_day = TRUE){
    if (!is.numeric(interval)) {
        stop("Argument 'interval' should be a numeric object.")
    } else {
        interval <- as.integer(interval)
        if (!any(60L %% interval == 0, interval %% 60 == 0)) {
            warning("Argument 'inverval' is neither a number that 60 can exactly",
                    " divided by or a number that is divisible by 60. Results",
                    " may be not acceptable.")
        }
    }

    # If 'year' argument is not given, set the year as current year.
    if (missingArg(year)) {
        year <- current_year()
    } else if (!is.numeric(year)) {
        stop("Argument 'year' should be a numeric object.")
    }

    year <- as.integer(year)

    if (missingArg(tz)) {
        tz <- Sys.timezone()
    }

    # Start the time sequence from "01-01 00:00:00" + interval.
    if(down){
        annual_seq <-
            seq(as.POSIXct(paste(year, "-01-01 00:00:00", sep = ""), tz = tz)
                + lubridate::minutes(interval),
                as.POSIXct(paste(year, "-12-31 23:59:59", sep = ""), tz = tz)
                + lubridate::minutes(interval),
                by = paste(interval, "min"))
    # Start the time sequence from "01-01 00:00:00".
    } else {
        annual_seq <-
            seq(as.POSIXct(paste(year, "-01-01 00:00:00", sep = ""), tz = tz),
                as.POSIXct(paste(year, "-12-31 23:59:59", sep = ""), tz = tz),
                by = paste(interval, "min"))
    }

    # Delete data of leap day "02-29" if it is not desired in the result.
    if(all(year %% 4 == 0L, !leap_day)){
        # Convert the tiem sequence into a data.table with column name
        # "datetime".
        annual_dt <- data.table::data.table(datetime = annual_seq)
        annual_dt <- annual_dt[, date := as.IDate(datetime, tz = tz)][
                                 date != paste0(year, "-02-29")][, date := NULL][]
    annual_seq <- annual_dt[, datetime]
    }

    return(annual_seq)
}
# }}}1

#' Create an annual schedule data for later used in EnergyPlus.
#'
#' \code{annual_sch} is a function to merge the input time series data.table
#' object with an annual time sequence. It returns a merged data.table of the
#' input and an annual time sequence with the same year of the input.
#'
#' @param data A data.frame or data.table.
#' @param interval An integer indicates the interval mins of the time sequence.
#' @param date_col A character indicates the name of 'POSIXt' or 'Date' column.
#' If not given, it will be auto-detected.
#' @param down If TRUE, the time sequence will start from '01-01 00:00:00'
#' offsetted by inverval.
#' @param leap_day If FALSE, and the input year is a leap year, '02-29' related
#' data will be deleted.
#' @param rp_na A value to replace the missing values in the merged data.table.
#' Default is NA.
#' @param tz A character indicates the time zone of the time sequence. The
#' default value is the current system time zone.
#' @return It returns a time sequence of giving year and inverval.
#' @importFrom lubridate year
#' @importFrom data.table data.table set merge
#' @export
# annual_sch
# {{{1
annual_sch <- function(data, date_col = NULL,
                       down = TRUE, leap_day = TRUE, rp_na = NA,
                       tz = Sys.timezone()){
    check_df(data)
    data <- conv_dt(data)

    date_col_name <- check_date_col(data, date_col)

    date_col <- data[[date_col_name]]

    # Set the year as the mode value of year of date column.
    # NOTE: may need further modification to provide a more robust solution.
    freq <- table(lubridate::year(date_col))
    year <- as.numeric(names(freq))[which.max(freq)]

    # Get the interval in mins.
    interval <- `/`(get_interval(date_col), 60L)

    annual <- data.table::data.table(annual = annual_seq(interval = interval, year = year,
                                                         down = down, leap_day = leap_day,
                                                         ...))

    data_merged <- merge(annual, y = data, all.x = TRUE,
                         by.x = "annual", by.y = date_col)

    # Borrowed from the answer of Matt Dowle in stackoverflow:
    # http://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
    if(!is.na(rp_na)){
        for (j in names(data_merged)) {
            data.table::set(data_merged, which(is.na(data_merged[[j]])), j, rp_na)
        }
    }

    return(data_merged)
}
# }}}1

#' Format POSIXct time into EnergyPlus Date/Time format.
#'
#' \code{eplus_time_fmt}: A function to transform "00:00" to "24:00" and format
#' date into EnergyPlus format, i.e. "mm/dd HH:MM:SS". Use with caution because
#' the 'POSIXct' and 'Date' data will be converted into characters.
#'
#' @param data A data.table object to manipulate.
#' @param date_col A character indicates the name of 'POSIXt' or 'Date' column.
#' If not given, it will be auto-detected.
#' @param std_fmt If FALSE, '00:00' will be replaced with EnergyPlus-flavored
#' '24:00' format.
#' @param copy Whether hard copy the input to protect it unchanged.
#' @importFrom lubridate year
#' @importFrom data.table data.table as.IDate ":="
#' @export
# eplus_time_trans
# {{{1
eplus_time_fmt <- function(data, date_col = NULL, std_fmt = TRUE, copy = TRUE){

    # Hard copy the input data to protect the original data untouched.
    if (copy) {
        data <- copy(data)
    }

    check_df(data)
    data <- conv_dt(data)

    date_col_name <- check_date_col(data, date_col)

    date_col <- data[[date_col_name]]

    # Set the year as the mode value of year of date column.
    # NOTE: May need further modification to provide a more robust solution.
    freq <- table(lubridate::year(date_col))
    year <- as.numeric(names(freq))[which.max(freq)]

    data[, data.table::`:=`(date = data.table::as.IDate(date_col, tz = tz),
                            Time = format(date_col, "%T"))]

    # Replace '00:00' with EnergyPlus-flavored '24:00' format.
    if(!std_fmt){
        data[Time == "00:00:00", data.table::`:=`(date = date-1, Time = "24:00:00")]
    }

    data <- data[, data.table::`:=`(date = format(date, "%m/%d"))][,
                   c(date_col) := paste(date, Time)][,
                   data.table::`:=`(date = NULL, Time = NULL)]

    return(data)
}
# }}}1

#' Transform EnergyPlus Date/Time format into POSIXct format.
#'
#' \code{eplus_time_trans} takes a data.table as input, and transform the column
#' with EnergyPlus Date/Time format into a column of POSIXct.
#'
#' @param data A data.table object to manipulate.
#' @param year An integer indicates the year of the time sequence. The default
#' value is the current year.
#' @param eplus_date_col A character indicates the column name of EnergyPlus
#' time in the input.
#' @param new_date_col A character indicates the name of the new transformed
#' POSIXt column.
#' @param tz A character indicates the time zone of the transformed time column.
#' The default value is the current system time zone.
#' @param keep_ori Whether to keep the original EnergyPlus time column. Default
#' is FALSE which means the EnergyPlus time column will be deleted.
#' @param copy Whether hard copy the input to protect it unchanged. Default is
#' FALSE.
#' @return A data.table with first column being the transformed POSIXct column
#' from EnergyPlus Date/Time format column.
#' @importFrom lubridate ymd_hms
#' @importFrom data.table ":=" setcolorder
#' @export
# eplus_time_trans
# {{{1
eplus_time_trans <- function(data, year = current_year(),
                             eplus_date_col = "Date/Time",
                             new_date_col = "datetime", tz = Sys.timezone(),
                             keep_ori = FALSE, copy = FALSE){
    check_df(data)
    data <- conv_dt(data)

    # Hard copy the data to protect the original data untouched.
    if (copy) {
        data <- copy(data)
    }

    # Build a date column with standard "%Y-%m-%d %H:%M:%S" ISO format.
    data <- data[, c(new_date_col) := gsub(x = data[[eplus_date_col]], "^", paste0(year, "-"))]
    data <- data[, c(new_date_col) := gsub(x = data[[new_date_col]], "/", "-")]
    data <- data[, c(new_date_col) := gsub(x = data[[new_date_col]], "  ", " ")]
    data <- data[, c(new_date_col) := lubridate::ymd_hms(data[[new_date_col]], tz = tz)]
    data <- data.table::setcolorder(data, c(new_date_col, col_names(data, new_date_col, invert = T)))

    # Delete the original
    if (!keep_ori) {
        data <- data[, c(eplus_date_col) := NULL]
    }

    return(data)
}
# }}}1

#' Add a padding time column for further aggregation.
#'
#' \code{add_time} adds a padding time column in a data.frame for further easy
#' data aggregation. It can also replace the datetime that is (year+1)-01-01
#' with (year)-12-31.
#'
#' @param data A data frame containing at least one datetime column.
#' @param base A character indicates the name of datetime column.  If not given,
#' it will be auto-detected.
#' @param step The timestep of the added datetime variable, which should be
#' higher than the interval of the input datetime variable. If 'NULL', it will
#' be one level higher than the interval of the input datetime variable. It can
#' also be an integer indicates the interval of minutes. In conclusion,
#' 'interval' should be a number or one of c("year", "quarter", "month", "week",
#' "yday", "mday", "wday", "day", "date", "hour", "min", "second").
#' @param new The column name of the added variable. If 'NULL' it will be
#' the name of the original datetime variable with the interval name added to
#' it, separeted by an underscore.
#' @param one_year If TRUE, the (year+1)-01-01 will be replaced with
#' (year)-12-31.
#' @importFrom lubridate ceiling_date floor_date round_date
#' @importFrom stringr str_replace_all
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate
#' @export
# add_time
# add_time{{{1
add_time <- function (data, base = NULL, new = NULL, step,
                      toward = c("up", "down", "round"), one_year = FALSE) {

    # TODO: Add checking for invalid step such as '600 secs'.
    check_df(data)
    if (is.null(base)) {
        base <- check_date_col(data)
    }
    datetimes <- data[[base]]

    if (one_year) {
        datetimes <- one_year(datetimes)
    }

    if(is.null(new)) {
        new <- paste0(base, "_", stringr::str_replace_all(step, "\\s", ""))
    } else {
        new <- stringr::str_replace_all(new, "\\s", "")
    }

    toward <- rlang::arg_match(toward)

    data_thicken <-
        switch(toward,
               up = dplyr::mutate(data, !!new := lubridate::ceiling_date(datetimes, unit = step)),
               down = dplyr::mutate(data, !!new := lubridate::floor_date(datetimes, unit = step)),
               round = dplyr::mutate(data, !!new := lubridate::round_date(datetimes, unit = step))
               )

    if (identical(new, base)) {
        message("Column '", new, "' has been replaced with new timestep of '",
                step, "'.")
    } else {
        message("A new column named '", new, "' with step '", step,
                "' has been added based on column '", base, "'.")
    }

    return(data_thicken)
}
# }}}1

#' Resample data according to given time step.
#'
#' \code{resample} adds a padding time column and aggregate time series data
#' according to given time step.
#'
#' @inheritParams add_time
#' @param drop If TRUE, all non-numeric columns will be dropped during
#' resampling. Otherwise, they will be used as group columns.
#' @importFrom purrr map_lgl
#' @export
# resample{{{1
resample <- function (data, base = NULL, new = NULL, step = "month", drop = FALSE) {

    if (is.null(base)) {
        base <- check_date_col(data)
    }

    data_thicken <- add_time(data = data, base = base, new = new, step = step)

    # Cause `add_time` always add the new column as the last column.
    if (is.null(new)) {
        date_cols <- get_date_col(data_thicken)
        new <- date_cols[length(date_cols)]
    }

    # Delete the original datetime column if the new name is not the same as the
    # base name.
    if (!identical(base, new)) {
        data_thicken <- dplyr::select(data_thicken, -dplyr::one_of(base))
    }

    # Check if there are non-numeric columns
    non_num_cols <- names(purrr::discard(data_thicken, is.numeric))
    non_num_cols <- non_num_cols[non_num_cols != new]
    new_name <- rlang::syms(new)
    if (assertthat::not_empty(non_num_cols)) {
        if (drop) {
            warning("Non-numeric column found: ",
                    paste0(sQuote(non_num_cols), collapse = ", "),
                    ". It/They will be dropped during resampling. ",
                    "Please set 'drop' to FALSE argument if you want to keep it/them.",
                    call. = FALSE)
            data_thicken <- dplyr::select(data_thicken, -dplyr::one_of(non_num_cols))
            data_thicken <- dplyr::group_by(data_thicken, rlang::UQS(new_name))
        } else {
            non_num <- rlang::syms(non_num_cols)
            data_thicken <- dplyr::group_by(data_thicken, rlang::UQS(non_num), rlang::UQS(new_name))
        }
    } else {
        data_thicken <- dplyr::group_by(data_thicken, rlang::UQS(new_name))
    }

    data_agg <- dplyr::summarise_all(data_thicken, mean)
    data_agg <- dplyr::ungroup(data_agg)

    return(data_agg)
}
# }}}1

#' Replace NA, NAN or Inf in a data.table.
#'
#' @param data A data.frame or data.table object to manipulate. If a data.frame,
#' it will be converted into a data.table.
#' @param col_pattern A RegEx used for column subsetting.
#' @param type A character indicates the type of nonsense data. Currently,
#' \code{NA}, \code{NAN}, and \code{Inf} are support.
#' @param replacement A single character or number used to replace the nonsense
#' data.
#' @return A data.table with nonsense data replaced.
#' @importFrom data.table set fun
#' @export
# na_replace
# {{{1
na_replace <- function(data, col_pattern, type = "na", replacement = 0L){
    check_df(data)
    dataa <- conv_dt(data)

    if(type == "na"){
        if(hasArg(col_pattern)){
            for (j in col_names(data, col_pattern))
                data <-
                    data.table::set(data,which(is.na(data[[j]])),j,replacement)
        }else{
            for (j in seq_len(ncol(data)))
                data <-
                    data.table::set(data,which(is.na(data[[j]])),j,replacement)
        }
    }else if(type == "nan"){
        if(hasArg(col_pattern)){
            for (j in col_names(data, col_pattern))
                data <-
                    data.table::set(data,which(is.nan(data[[j]])),j,replacement)
        }else{
            for (j in seq_len(ncol(data)))
                data <-
                    data.table::set(data,which(is.nan(data[[j]])),j,replacement)
        }
    }else if(type == "inf"){
        if(hasArg(col_pattern)){
            for (j in col_names(data, col_pattern))
                data <-
                    data.table::set(data,which(is.infinite(data[[j]])),j,replacement)
        }else{
            for (j in seq_len(ncol(data)))
                data <-
                    data.table::set(data,which(is.infinite(data[[j]])),j,replacement)
        }
    }else{
        stop("Invalid `type`.")
    }
    return(data)
}
# }}}1

#' Tramsform EnergyPlus meter output from site energy to source energy.
#'
#' @param data A data.table object containing EnergyPlus standard meter result.
#' @param ele_pattern A RegEx to extract columns of electricity consumption.
#' @param gas_pattern A RegEx to extract columns of gas consumption.
#' @param ele_fct The site to source convert factor of electricity. Default is 3.095
#' which is the standard value in California, USA.
#' @param gas_fct The site to source convert factor of gas. Default is 1.092
#' which is the standard value in California, USA.
#' @param digits A integer indicates the digits of calculated results.
#' @param to_GJ If TRUE, the unit of energy consumption will be converted from
#' Joule to Gigajoule.
#' @return A data.table containing EnergyPlus meter results with site energy
#' converted into source energy.
#' @importFrom data.table setnames
#' @export
# site_to_src
# {{{1
site_to_src <- function (data, ele_pattern = "electricity", gas_pattern = "gas",
                         ele_fct = 3.095, gas_fct = 1.092, digits = 4, to_GJ = FALSE) {
    check_df(data)
    data <- conv_dt(data)

    data <- data[, lapply(.SD, function(x) round(x*ele_fct, digits)),
                   .SDcol = col_names(data, ele_pattern),
                   by = c(col_names(data, ele_pattern, invert = T))]
    data <- data[, lapply(.SD, function(x) round(x*gas_fct, digits)),
                   .SDcol = col_names(data, gas_pattern),
                   by = c(col_names(data, gas_pattern, invert = T))]

    if(to_GJ){
        data <- data[, lapply(.SD, function(x) round(x/1E9, digits)),
                       .SDcol = col_names(data, ele_pattern),
                       by = c(col_names(data, ele_pattern, invert = T))]
        data <- data[, lapply(.SD, function(x) round(x/1E9, digits)),
                       .SDcol = col_names(data, gas_pattern),
                       by = c(col_names(data, gas_pattern, invert = T))]
        data <- data.table::setnames(data, gsub(x=col_names(data),
                                                pattern = "\\[J\\].*$",
                                                replacement = "[GJ]"))
    }

    return(data)
}
# }}}1

#' Melt a data.frame with column selection using RegEx.
#'
#' Basically, \code{data_melt} is a wrapper function of
#' \code{\link{data.table::melt.data.table}} but enables to use RegEx for
#' convenient selection of both id columns and measure columns.
#'
#' @param data A data.frame or data.table object to manipulate.
#' @param id_pattern A RegEx used for id column selection.
#' @param measure_pattern A RegEx used for measure column selection.
#' @param variable_name Name for the measured variable names column. The default
#' name is 'variable'.
#' @param value_name Name for the molten data values column. The default name is
#' 'value'.
#' @param na_rm If TRUE, NA values will be removed from the molten data.
#' @param variable_factor If TRUE, the variable column will be converted to
#' factor, else it will be a character column.
#' @param value_factor If TRUE, the value column will be converted to factor,
#' else the molten value type is left unchanged.
#' @return A molten data.table.
#' @importFrom data.table melt
#' @export
#' @examples
#' data_melt(iris, id_pattern = "^S")
# data_melt
# {{{1
data_melt <- function (data, id_pattern, measure_pattern, ignore_case = TRUE,
                       variable_name = "variable", value_name = "value",
                       na_rm = FALSE, variable_factor = TRUE, value_factor = FALSE) {
    check_df(data)
    data <- conv_dt(data)

    if(hasArg(id_pattern)){
        if(hasArg(measure_pattern)){
            data_melt <-
                melt(data, id.vars = col_names(data, id_pattern, ignore_case = ignore_case),
                     measure.vars = col_names(data, measure_pattern, ignore_case = ignore_case),
                     variable.name = variable_name, value.name = value_name, na.rm = na_rm,
                     variable.factor = variable_factor, value.factor = value_factor)
        }else{
            data_melt <-
                melt(data, id.vars = col_names(data, id_pattern, ignore_case = ignore_case),
                     variable.name = variable_name, value.name = value_name, na.rm = na_rm,
                     variable.factor = variable_factor, value.factor = value_factor)
        }
    }else{
        if(hasArg(measure_pattern)){
            data_melt <-
                melt(data, measure.vars = col_names(data, measure_pattern, ignore_case = ignore_case),
                     variable.name = variable_name, value.name = value_name, na.rm = na_rm,
                     variable.factor = variable_factor, value.factor = value_factor)
        }else{
            stop("Both arguments `id_pattern` and `measure_pattern` are missing!", call. = FALSE)}
    }
    return(data_melt)
}
# }}}1

# #' Format EnergyPlus output and meter result into a long table.
# #'
# #' \code{long_table} takes an EnergyPlus "output" or "meter" result as an input
# #' and returns a long table with first column being the POSIXt column, and next
# #' 'component' indicating energy consumption components, 'type' indicating
# #' energy types (e.g.  Electricity, and Gas), 'value' indicating the value of
# #' the component, 'unit' indicating the unit of energy used, and 'timestep'
# #' indicating the tiem step of data collected. A meter output from a
# #' 10-min-timestep annual simulation will takes about 5 seconds to load.  So,
# #' use with caution.
# #'
# #' @param data A data.table object containing EnergyPlus standard output or
# #' meter result.
# #' @return A data.table with long table format.
# #' @importFrom data.table melt setcolorder
# #' @export
# # long_table
# # {{{1
# long_table <- function(data) {
#     check_df(data)
#     data <- conv_dt(data)

#     date_col <- get_date_col(data)

#     data <- melt(data, id.vars = date_col,
#                  variable.name = "component", value.name = "value",
#                  variable.factor = FALSE)
#     data <- data[, c("component", "type", "unit", "timestep") := data.table::tstrsplit(component, "[:\\[\\(]")][,
#                  lapply(.SD, function(x) gsub(x=x, "\\]*\\)*", "")), .SDcol = c("type", "unit", "timestep"), by = c(date_col, "component", "value")][,
#                  lapply(.SD, function(x) gsub(x=x, "^\\s+", "")), .SDcol = c("type", "unit", "timestep"), by = c(date_col, "component", "value")][,
#                  lapply(.SD, function(x) gsub(x=x, "\\s+$", "")), .SDcol = c("type", "unit", "timestep"), by = c(date_col, "component", "value")]
#                  # lapply(.SD, str_trim), .SDcol = c("type", "unit", "timestep"), by = c(date_col, "component", "value")]

#     data <- data.table::setcolorder(data, c(date_col, "component", "type", "value", "unit", "timestep"))

#     return(data)
# }
# # }}}1

#' Calculate CV(RMSE) between two numeric vector.
#'
#' @param x,y A numeric vector to calculate with.
#' @param na_rm If TRUE, NA values will be removed.
#' @return The CV(RMSE) of x to y.
#' @export
# cvrmse
# {{{1
cvrmse <- function (x, y, na_rm = TRUE, ...) {
    rmse <- sqrt(mean((x - y)^2, na.rm = na_rm))
    cvrmse <- rmse/mean(y)
    return(cvrmse)
}
# }}}1

#' Calculate NMBE between two numeric vector.
#'
#' @param x,y A numeric vector to calculate with.
#' @param na_rm If TRUE, NA values will be removed.
#' @return The NMBE of x to y.
#' @export
# nmbe
# {{{1
nmbe <- function (x, y, na.rm=TRUE, ...) {
    me <- mean( x - y, na.rm = na.rm)
    nmbe <- me/mean(y)
    return(nmbe)
}
# }}}1

#' Calculate different percentage between two numeric vector.
#'
#' @param x,y A numeric vector to calculate with.
#' @return The different percentage from x to y.
#' @export
# bias
# {{{1
bias <- function (x, y) {
  bias <- (x - y)/y
  return(bias)
}
# }}}1

#' Apply a function between groups
#'
#' \code{case_cal} .
#'
#' @param data A data.table object.
#' @param case_col Unquoted column name that is used as the group column.
#' @param col_pattern A RegEx indicates the column that the function applys to.
#' @param fun A character indicates the name of the function. It must be a
#' function that takes two arguments, i.e. cvrmse(x, y).
#' @param case_order If TRUE, the function applied will take the appearance
#' order in group column. For example, the group column contains three unique
#' c("a", "b", "c"), and only fun(a, b), fun(a, c) and fun(b, c) will be
#' calculated. If FALSE, all permutation without duplicated elements will be
#' calculated.
#' @param melt If TRUE, a molten data.table will be returned with the first
#' column named "case" showing what function and arguments has been applied.
#' @return A data.table
#' @importFrom purrr cross_d map_lgl map2_df
#' @importFrom data.table as.data.table
#' @export
# case_cal
# {{{1
case_cal <- function (data, case_col, col_pattern = NULL, fun, case_order = TRUE,
                      melt = FALSE) {
    check_df(data)
    data <- conv_dt(data)

    if(!(hasArg(fun))){
        stop("Argument 'fun' is missing!")
    }

    # NSE: Non-standard evaluation implementation.
    case_col <- substitute(case_col)
    cases <- unique(data[, eval(case_col, envir = .SD)])

    if (case_order) {
        cases <- as.data.table(t(combn(cases, 2)))
        cases <- setnames(cases, c("x", "y"))
    } else {
        # Get all combinations of cases
        cases <- purrr::cross_d(list(x = cases, y = cases), function (x, y) y == x)
    }

    # Get selected column names
    if (is.null(col_pattern)) {
        # Only apply functions to numeric columns
        numeric_cols <- colnames(data)[purrr::map_lgl(data, is.numeric)]
        # Select numeric columns and exclude case column
        sel_cols <- grep(x = numeric_cols, paste0("^", case_col, "$"), value = TRUE, invert = TRUE)

        non_numeric_cols <- colnames(data)[!purrr::map_lgl(data, is.numeric)]
        non_numeric_cols <- grep(x = non_numeric_cols, paste0("^", case_col, "$"), value = TRUE, invert = TRUE)
        if (length(non_numeric_cols) > 0) {
            warning("Non-numeric columns found: ", paste0(paste0("'", non_numeric_cols, "'"), collapse = ", "), ".\n",
                    "Function '", fun, "' was only applied to numeric columns.", call. = FALSE)
        }
    } else {
        sel_cols <- col_names(data, as.character(case_col), invert = TRUE)
        sel_cols <- grep(x = sel_cols, col_pattern, value = TRUE, perl = TRUE)
    }

    fun <- substitute(fun)

    cal <-
        purrr::map2_df(cases$x, cases$y,
                       function (x, y) {
                           case_1 <- data[eval(case_col) == x, ..sel_cols]
                           case_2 <- data[eval(case_col) == y, ..sel_cols]
                           cal <- as.data.table(purrr::map2_df(case_1, case_2, eval(parse(text = fun))))
                           cal <- cal[, case := paste0(fun, "(", x, ", ", y, ")")]
                           # Set column order
                           cal <- data.table::setcolorder(cal, c("case", col_names(cal, "case", invert = TRUE)))
                       })

    if (melt) {
        cal <- melt(cal, id.vars = "case")
    }

  return(cal)
}
# }}}1

######################
#  helper functions  #
######################

#' @importFrom purrr map flatten_chr
# addCalExp: A function to generate column calculation expression in data.table.
# addCalExp
# {{{1
addCalExp <- function(data, newcol.name = "NewColumn", sep = "", ref = TRUE, p.list){
  if(missing(p.list)){
    stop("No column pattern specified!")
  }

  colnames <-
    purrr::map(p.list,
               function(par){
                   if(grepl(x=par,"^@")){
                       par %>% sub(x=.,"^@(.*)$", " \\1 ")
                   }else if(length(col_names(data, par))){
                       col_names(data, par) %>%
                           gsub(x=., "(.*)", "`\\1`")
                   }else{
                       stop("Column not found in the data.")
               }}) %>% purrr::flatten_chr()

  if(sep == ""){
      if(ref){
          expr <- paste0('c("', newcol.name, '") := ', paste0(colnames, collapse = ""))
      }else{
          expr <- paste0('.(`', newcol.name, '` = ', paste0(colnames), ')', collapse = "")
      }
  }else{
      if(ref){
          expr <- paste0('c("', newcol.name, '") := ', paste(colnames, collapse = sep))
      }else{
          expr <- paste0('.(`', newcol.name, '` = ', paste(colnames, collapse = sep), ')')
      }
  }
  return(expr)
}
# }}}1

#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom data.table copy
# addCalCol: A function to add new column using regex in data.table
# addCalCol
# {{{1
addCalCol <- function(data, newcol.name, p.list, bycol_pattern, sep = "",
                      ref = TRUE, copy = FALSE){
    if(copy){
        data %<>% copy(.)
    }
    if(hasArg(bycol_pattern)){
        data %<>% .[, eval(parse(text = addCalExp(data, newcol.name = newcol.name, sep = sep,
                                                  ref = ref, p.list = p.list))),
                    by = c(col_names(., bycol_pattern))] %>% .[]
    }else{
        data %<>% .[, eval(parse(text = addCalExp(data, newcol.name = newcol.name, sep = sep,
                                                  ref = ref, p.list = p.list)))] %>% .[]
    }
    return(data)
}
# }}}1

# check_df: A helper function to check if the input object is a data.frame or
#           not.
# check_df
# {{{1
check_df <- function (data) {
    if (!is.data.frame(data)) {
        stop("'data' should be a data.frame.", call. = FALSE)
    }
}
# }}}1

#' @importFrom data.table is.data.table as.data.table
# conv_dt: A helper function to check if the input object is a data.table or
#          not. If not, convert it to a data.table.
# conv_dt
# {{{1
conv_dt <- function (data) {
    # Check if the input is a data.table object. If not, convert it to a
    # data.table.
    if (!data.table::is.data.table(data)) {
        warning("Input is a data.frame object. ",
                "It has been converted to a data.table.", call. = FALSE)
        data <- data.table::as.data.table(data)
    }
    return(data)
}
# }}}1

#' @importFrom purrr map map_lgl
# get_date_col: A helper function to get the column contains date and time in a
# data.frame. It will return a name vector of column that has a calss of
# 'POSIXt' or 'Date'. If none is found, it will return a vector with a length of
# zero.
# get_date_col
# {{{1
get_date_col <- function(data){
    check_df(data)

    # Get classes for all columns.
    classes <- purrr::map(data, class)
    # Find the column with a class of "POSIXt" or "Date".
    date_classes <- (purrr::map_lgl(classes, function(classes) 'POSIXt' %in% classes)|
                     purrr::map_lgl(classes, function(classes) 'Date' %in% classes))

    # Find the name of date column.
    date_col <- names(which(date_classes))

    return(date_col)
}
# }}}1

# check_date_col: A helper function to check if there is only one date column in
# the input.
# check_date_col
# {{{1
check_date_col <- function (data) {
    date_col <- get_date_col(data = data)
    if (length(date_col) == 0) {
        stop("None datetime column found.", call. = FALSE)
    }
    # If there are multiple date columns found, stop.
    if (length(date_col) > 1) {
        stop("Multiple datetime columns found.", call. = FALSE)
    }

    return(date_col)
}
# }}}1

# get_interval: A helper function to get the minimum time interval of a POSIXct
# or Date sequence. It return a minimum interval of given time sequence in secs.
# - 'date_seq': A time sequence.
# get_interval
# {{{1
get_interval <- function (date_seq) {

    time_diff <- as.numeric(names(table(diff(as.numeric(date_seq)))))

    if (min(time_diff) == 0) {
        stop("Duplicate times found in date column.")
    } else if (length(time_diff) != 1) {
        warning("Multiple time differences found in date column. ",
                "The minimum one will be used.")
        interval <- min(time_diff)
    } else {
        interval <- time_diff
    }

    return(interval)
}
# }}}1
# S3 methods for get_timestep{{{
get_timestep <- function (x, unit, by, simplify) {
   UseMethod("get_timestep", x)
}

get_timestep.default <- function (x, unit, simplify = FALSE) {
   stop("Unsupport for class ", class(x),".", call. = FALSE)
}

get_timestep.POSIXt <- function (x, ..., unit = "second", simplify = FALSE) {
    datetimes_to_nums <- as.integer(x)
    datetimes_diff_table <- table(diff(datetimes_to_nums))
    steps <- format_table(datetimes_diff_table, unit = "second")
    steps <- dplyr::mutate(steps, step = units::set_units(step, unit))
    if (simplify) {
        if (length(steps[["step"]]) > 1L) {
            stop("Input have multiple timesteps. Unable to simplify.",
                 call. = FALSE)
        }
        steps <- steps[["step"]]
    }
    return(steps)
}

get_timestep.Date <- function (x, ..., unit = "day", simplify = FALSE) {
    datetimes_to_nums <- as.integer(x)
    datetimes_diff_table <- table(diff(datetimes_to_nums))
    steps <- format_table(datetimes_diff_table, unit = "day")
    steps <- dplyr::mutate(steps, step = units::set_units(step, unit))
    if (simplify) {
        if (length(steps[["step"]]) > 1L) {
            stop("Input have multiple timesteps. Unable to simplify.",
                 call. = FALSE)
        }
        steps <- steps[["step"]]
    }
    return(steps)
}

get_timestep.data.frame <- function(x, by = NULL, unit = NULL, simplify = FALSE) {
    if (!is.null(by)) {
        dfs <- split(x, x[[by]])
        steps <- map(dfs, .get_timestep_df)
    } else {
        steps <- .get_timestep_df(x)
    }

    if (!is.null(unit)) {
        steps <- purrr::map(steps, ~dplyr::mutate(.x, step = units::set_units(step, unit)))
    }

    return(steps)
}

.get_timestep_df <- function (df) {
    dates <- names(purrr::keep(df, assertthat::is.date))
    times <- names(purrr::keep(df, assertthat::is.time))

    datetimes <- df[, c(dates, times)]

    assertthat::assert_that(assertthat::not_empty(datetimes),
                            msg = "Input does not contain any column with date or time.")

    datetimes_to_nums <- purrr::map_df(datetimes, as.integer)
    datetimes_diff_table <- purrr::map(datetimes_to_nums, ~table(diff(.x)))

    steps <- purrr::map(names(datetimes_diff_table),
                        ~{
                            name <- names(datetimes_diff_table[.x])
                            table <- datetimes_diff_table[[.x]]
                            format_table(table, unit = ifelse(name %in% dates, "day", "second"))
                        })
    steps <- purrr::set_names(steps, names(datetimes))

    return(steps)
}

get_unit <- function (x) {
    assertthat::assert_that(inherits(x, "units"),
                            msg = "Input should be a object of units class.")
    unit_info <- units(x)
    unit <- unit_info[["numerator"]]
    return(unit)
}

format_table <- function (table, unit) {
    assertthat::assert_that(is.table(table), msg = "Input is not a table object.")
    per <- round(as.numeric(prop.table(table)*100))

    # Make sure the sum of percentages equals 100%.
    per[which.max(per)] <- 100 - sum(per[-which.max(per)])

    tbl <- as_tibble(table)
    tbl <- purrr::set_names(tbl, c("step", "n"))
    tbl <- dplyr::mutate(tbl,
                         step = as.integer(step),
                         `%` = round(per))
    tbl <- dplyr::mutate(tbl,
                         step = units::set_units(step, unit),
                         `%` = units::set_units(`%`, "%"))
    return(tbl)
}
# }}}

#' @importFrom lubridate year days
# one_year: A helper function to replace (year + 1)-01-01 occurance with
# year-12-31.
# one_year
# {{{1
one_year <- function (date_seq) {

    # Get the frequency of all years.
    years <- sort(table(lubridate::year(date_seq)))
    # If all date sequence is in one year, return as it is.
    if (length(years) == 1) {
        return(date_seq)

    # If there are more than two number of years occur, give an error.
    } else if (length(years) > 2){
        stop("The span of the time exceeds more than 2 years.")

    # If there are only two number of years occur,
    } else {
        year_diff <- diff(as.integer(names(years)))
        dt <- data.table::data.table(ori = date_seq)
        dt <- dt[, year := lubridate::year(ori)]
        if (year_diff == -1L) {
            dt[year == as.integer(names(years)[1]),
               ori := ori - lubridate::days(1)][, year := NULL]
        } else {
            dt[year == as.integer(names(years)[1]),
               ori := ori + lubridate::days(1)][, year := NULL]
        }
        one_year <- dt[["ori"]]

        return(one_year)
    }
}
# }}}1

# uniform_interval_name: A helper function to get the right interval string.
# uniform_interval_name
# {{{1
uniform_interval_name <- function(interval) {
    if (interval %in% c("y", "year", "years")) {
        interval <- "year"
    } else if (interval %in% c("q", "quarter", "quarters")){
        interval <- "quarter"
    } else if (interval %in% c("m", "mo", "mon", "mont", "month", "months")) {
        interval <- "month"
    } else if (interval %in% c("w", "we", "wee", "week", "weeks")){
        interval <- "week"
    } else if (interval %in% c("yd", "yda", "yday", "ydays")) {
        interval <- "yday"
    } else if (interval %in% c("md", "mda", "mday", "mdays")) {
        interval <- "mday"
    } else if (interval %in% c("wd", "wda", "wday", "wdays")) {
        interval <- "wday"
    } else if (interval %in% c("d", "da", "day", "days")) {
        interval <- "day"
    } else if (interval %in% c("dat", "date")) {
        interval <- "date"
    } else if (interval %in% c("h", "ho", "hou", "hour", "hours")) {
        interval <- "hour"
    } else if (interval %in% c("mi", "min", "mins", "minute", "minutes")) {
        interval <- "minute"
    } else if (interval %in% c("s", "se", "sec", "secs", "second", "seconds")) {
        interval <- "second"
    } else {
        stop("Invalid `interval` value.")
    }
    return(interval)
}
# }}}1
