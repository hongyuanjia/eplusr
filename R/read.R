#' @importFrom tools file_path_sans_ext
#' @importFrom lubridate force_tz
#' @importFrom fasttime fastPOSIXct
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
NULL

# collect_eplus: A function to read EnergyPlus simulation results.
# collect_eplus {{{1
collect_eplus <- function (path, output = c("variable", "meter", "table"),
                           long = FALSE, report = NULL, key = NULL, table = NULL) {
    # Read only one path at a time
    assertthat::assert_that(is_string(path))
    # Default is to read variable output
    output <- match.arg(output)

    # Get the output files {{{
    file_to_read <- get_file_to_read(path, type = output)
    assertthat::assert_that(file.exists(file_to_read),
                            msg = msg("The simulation output file does not exists."))
    # }}}

    data <- switch(output,
        variable = read_variable(file_to_read, long = long),
        meter = read_variable(file_to_read, long = long),
        table = read_table(file_to_read, report = report, key = key, table = table)
    )

    data[, model := basename(path)]
    setcolorder(data, c("model", setdiff(names(data), "model")))

    return(data[])
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

# read_variable {{{1
read_variable <- function (path, long = FALSE, year = 2017) {
    assertthat::assert_that(is_string(path))
    assertthat::assert_that(has_output_ext(path))

    if (!file.exists(path)) return(NULL)
    data <- fread(path)[, `Date/Time` := paste0(year, "/", `Date/Time`)][
        , `Date/Time` := (fasttime::fastPOSIXct(`Date/Time`, tz = "GMT"))][
        , `Date/Time` := lubridate::force_tz(`Date/Time`, tz = Sys.timezone())]
    setnames(data, "Date/Time", "datetime")

    if (long) {
        data <- long_table(data)
    }

    return(data)
}
# }}}1

# read_table: A function to read EnergyPlus table results.
# read_table {{{
read_table <- function (file, report = NULL, key = NULL, table = NULL) {
    assertthat::assert_that(is_string(file))
    assertthat::assert_that(file.exists(file))
    assertthat::assert_that(has_ext(file, ext = "html") || has_ext(file, ext = "htm"),
        msg = msg("'file' should have an extension of either '.htm' or '.html'.")
    )

    tbls <- read_table_info(file)

    # Get table contents.
    tbls_raw <- rvest::html_nodes(xml2::read_html(file), "table")
    # Stop if the logic above results in a number mismatch of table names and
    # tables.
    assertthat::assert_that(identical(length(tbls_raw), nrow(tbls)),
        msg = msg("Error[Debug]: Mismatch length of extracted table names and
                  table number.")
    )

    tbls[, content := tbls_raw]

    if (not_empty(report)) {
        .report <- report
        tbls <- tbls[report %in% .report]
    }

    if (not_empty(key)) {
        .key <- key
        tbls <- tbls[key %in% .key]
    }

    if (not_empty(table)) {
        .table <- table
        tbls <- tbls[table %in% .table]
    }

    if (is_empty(tbls)) {
        stop("No matched table found.")
    }

    tbls[, content := rvest::html_table(content, header = TRUE)][
        , `:=`(id = NULL, name = NULL)]

    return(tbls)
}
# }}}

# read_table_info {{{1
read_table_info <- function(file) {
    # Get table names.
    # NOTE: Did not find a way to extract comments in htm/htmls in 'rvest'
    # package. Have to use a ugly regex method.
    regex_tbl_name <- "<!-- FullName:(.*)-->"

    l <- readLines(file)
    l_comm <- l[startsWith(l, "<!-- FullName:")]
    tbl_names <- gsub(regex_tbl_name, "\\1", l_comm)
    tbl_dt <- data.table(id = seq_along(tbl_names), name = tbl_names)[
        , c("report", "key", "table") := tstrsplit(name, "_", fixed = TRUE)]

    return(tbl_dt)
}
# }}}1
# long_table {{{1
long_table <- function(data) {
    assertthat::assert_that(is.data.frame(data))
    setDT(data)

    cols <- get_output_col(data)
    key_cols <- attr(cols, "unknown")

    if (is_empty(cols)) {
        stop("Input does not contains any EnergyPlus output variables.",
             call. = FALSE)
    }

    data <- melt.data.table(data, id.vars = key_cols,
        variable.name = "component", value.name = "value", variable.factor = FALSE)

    data[, c("key", "variable", "unit", "frequency") := data.table::tstrsplit(component, "[:\\[\\(]")][
         , `:=`(variable = substr(variable, 1L, nchar(variable) - 1L),
                unit = substr(unit, 1L, nchar(unit) - 1L),
                frequency = substr(frequency, 1L, nchar(frequency) - 1L))
    ]

    setcolorder(data, c(key_cols, "component", "key", "variable", "unit", "frequency", "value"))

    return(data)
}
# }}}1
# get_output_col {{{
get_output_col <- function (data) {
    nms <- names(data)
    reg <- ".*:.*\\[.*\\](.*)"
    cols <- nms[grepl(reg, nms)]

    setattr(cols, "unknown", setdiff(nms, cols))

    return(cols)
}
# }}}
# get_date_col {{{1
get_date_col <- function(df){
    assertthat::assert_that(is.data.frame(df))

    # Get classes for all columns.
    classes <- sapply(df, class)
    # Find the column with a class of "POSIXt" or "Date".
    date_classes <- (purrr::map_lgl(classes, function(classes) 'POSIXt' %in% classes)|
                     purrr::map_lgl(classes, function(classes) 'Date' %in% classes))

    # Find the name of date column.
    date_col <- names(which(date_classes))

    return(date_col)
}
# }}}1
# get_file_to_read {{{1
get_file_to_read <- function (path, type = c("variable", "meter", "table")) {
    prefix <- tools::file_path_sans_ext(path)

    suffix <- switch(type,
        # Currently `read_table` only support .htm(l) files.
        table = "Table.htm",
        variable = ".csv",
        meter = "Meter.csv"
    )

    file <- paste0(prefix, suffix)
    return(file)
}
# }}}1
