################################################################################
#                     EnergyPlus Working Directory Cleaning                    #
################################################################################

#' Clean an working directory of EnergyPlus
#'
#' \code{clean_wd} is a function to clean the working directory of EnergyPlus
#' after backing up specifed files. All other files that are not parts of
#' EnergyPlus input or output files will be left as it is.  This is also true if
#' you rename those files of EnergyPlus before you run this function. Currently,
#' files work as input schedule files are not supported, and will be left
#' untouched.
#'
#' @param path A EnergyPlus working directory.
#' @param backup Should be NULL or one of c('input', 'basic', 'all').  If NULL,
#' none file will be backed up.  If 'input', *.imf, *.idf, *.epw, *.epmidf will
#' be backed up.  If 'basic', *.imf, *.idf, *.ewp, *.epmidf, and results of
#' summary report, meter output and variable output will be backed up.  If
#' 'all', all EnergyPlus input and output files will be backed up.
#' @param backup_folder A character indicates the name prefix of the folder that
#' will be auto-generated in current working directory to store backed-up files.
#' The full folder name will be ('backup_folder'_(suffix)). The suffix is a
#' character that shows the current date and time in format like
#' 'Apr06_01h_02m'.  If this this folder already, which means that you run this
#' function multiple times within 1 minute, the folder name will be the decribed
#' above with 6 random characters.
#' @param rename If TRUE, the backed-up files will be renamed with the same
#' suffix as backup_folder.
#' @export
#' @importFrom purrr walk2
# clean_wd
# {{{1
clean_wd <- function(path, suffix_type = c("C", "L", "D"), extra = NULL,
                     keep_input = FALSE, backup = FALSE, backup_folder = NULL,
                     mark = "datetime", newname_mark = NULL) {

    # Args validation {{{2
    # assertthat::assert_that(file.exists(path))
    ext <- tools::file_ext(path)
    assertthat::assert_that(grepl("i[dm]f", ext, ignore.case = TRUE),
        msg = "'path' should be a EnergyPlus model path."
    )
    suffix_type <- rlang::arg_match(suffix_type)
    # Check for invalid input of argument 'backup'.
    if (is.logical(backup)) {
        if (backup) {
            backup_type <- "basic"
        } else {
            backup_type <- NULL
        }
    } else {
        backup_type <- rlang::arg_match(backup, c("input", "table", "variable",
        "meter", "basic", "all"))
    }
    # }}}2
    # Set working dir {{{2
    wd <- dirname(path)
    # Store the original working directory of current R session.
    ori_wd <- getwd()
    # Set the working directory back to the original path.
    on.exit(setwd(ori_wd))
    # Set the working dirctory to the input of EnergyPlus result folder.
    setwd(wd)
    # }}}2
    prefix <- file_prefix(path)
    if (!is.null(backup_type)) {
        backup_files <- get_backup_files(type = backup_type, prefix = prefix, suffix_type = suffix_type)

        backup_files(backup_files, backup_folder, mark, newname_mark)
    }
    clean_files <- get_clean_files(prefix = prefix, suffix_type = suffix_type, extra = extra, except_input = keep_input)
    purrr::walk(clean_files,
                function(file_name) {
                    if (file.exists(file_name)) {
                        unlink(file_name, force = TRUE)
                        message("File ", file_name, " has been successfully deleted ",
                                "from folder '", wd, "'.")
                    }
                })
    return(invisible())
}
# }}}1

#' @importFrom purrr walk
# backup_files
# {{{1
backup_files <- function(files, folder_prefix = NULL, folder_suffix = "datetime",
                         newname_suffix = NULL) {

    if (is.null(folder_prefix)) folder_prefix <- "backup"
    folder <- name_create(prefix = folder_prefix, suffix = folder_suffix)
    backup_folder <- folder_create(folder_name = folder)

    purrr::walk(files,
                ~backup_file(file = .x, backup_folder = backup_folder,
                             newname_suffix = newname_suffix)
    )

    return(invisible())
}
# }}}1

# get_clean_files
# {{{1
get_clean_files <- function (prefix, suffix_type, extra = NULL, except_input = FALSE) {
    prefix <- file_prefix(prefix)
    clean_files <- output_files(prefix = prefix, suffix_type = suffix_type)
    if (except_input) {
        input <- output_files(prefix = prefix, suffix_type = suffix_type, type = "input", simplify = TRUE)
        clean_files <- setdiff(clean_files, input)
    }
    extra = c("Energy+.ini", "fort.6", "audit.out", "post_proc.bat", extra)
    extra_files <- file_path(prefix, extra)
    clean_files <- c(clean_files, extra_files)
    return(clean_files)
}
# }}}1

# get_backup_files
# {{{1
get_backup_files <- function (type = NULL, prefix, suffix_type) {
    if (is.null(type)) {
        return(NULL)
    } else {
        type <- rlang::arg_match(type, c("input", "table", "variable", "meter", "basic", "all"))

        input <- output_files(prefix, suffix_type, type = "input", simplify = TRUE)
        table <- output_files(prefix, suffix_type, type = "table", simplify = TRUE)
        variable <- output_files(prefix, suffix_type, type = "variable", simplify = TRUE)
        meter <- output_files(prefix, suffix_type, type = "meter", simplify = TRUE)
        basic <- c(input, table, variable, meter)
        all <- output_files(prefix, suffix_type)

        backup_files <- switch(type, input = input, table = table,
                               variable = variable, meter = meter,
                               basic = basic, all = all)
    }

    return(backup_files)
}
# }}}1

#' @importFrom stringr str_trim
# name_create: A helper function to create a folder with name being the
#              concatenation of specified prefix and suffix string.
# name_create
# {{{1
name_create <- function (prefix = "backups", suffix = "datetime") {
    assertthat::assert_that(assertthat::is.string(prefix))
    prefix <- gsub("\\s", "_", stringr::str_trim(prefix))

    if (!is.null(suffix)) {
        assertthat::assert_that(assertthat::is.string(suffix))
        if (!is.na(match(suffix, c("datetime", "date", "time")))) {
            # Store the original time locale.
            ori_locale <- Sys.getlocale(category = "LC_TIME")
            # Change the time locale to the original value. In order to not print
            # the locale while run this function, store the output of
            # Sys.setlocale().
            on.exit(not_print <- Sys.setlocale(category = "LC_TIME", locale = ori_locale))
            # Change the time locale to "English_United States.1252".
            not_print <- Sys.setlocale(category = "LC_TIME", locale = "English_United States.1252")
            # Get the name suffix and then restore time locale setting.
            suffix <- switch(suffix,
                             datetime = format(Sys.time(), "(%b%d_%Hh_%Mm)"),
                             date = format(Sys.time(), "(%b%d)"),
                             time = format(Sys.time(), "(%Hh_%Mm_%Ss)"))
        } else {
            suffix <- gsub("\\s", "_", stringr::str_trim(suffix))
        }
    }

    name <- paste(c(prefix, suffix), sep = "_", collapse = "_")

    return(name)
}
# }}}1

#' @importFrom stringi stri_rand_strings
# folder_create: A helper function to create a folder. If the folder already
#                exist, a folder with a suffix of 5 random characters will be
#                created.
# folder_create
# {{{1
folder_create <- function (folder_name) {
    # If folder does not exist
    if (!dir.exists(folder_name)) {
        dir.create(folder_name, recursive = TRUE)

    # If folder already exists, add a trailing string of random 5 selections
    # from "a" to "z" and "0" to "9"
    } else {
        rand_str <- stringi::stri_rand_strings(1, 5, pattern = "[a-z0-9]")
        folder_name <- paste0(folder_name, "_", rand_str)
        dir.create(folder_name)
    }

    return(folder_name)
}
# }}}1

# backup_file: A helper function to copy input file into specified folder with
#              an option to rename the file with a formatted suffix string.
# backup_file
# {{{1
backup_file <- function(file, backup_folder, newname_suffix = NULL) {
    # If the file does not exist, return nothing.
    if (!file.exists(file)) {
        return(invisible())
    }

    file <- normalizePath(file, mustWork = FALSE)
    backup_folder <- normalizePath(backup_folder, mustWork = FALSE)
    if (identical(dirname(file), backup_folder)) {
        return(invisible())
    }

    file_name <- basename(file)
    prefix <- file_prefix(file_name)
    ext <- tools::file_ext(file_name)

    if (is.null(newname_suffix)) {
        copy_flag <- file.copy(from = file,
                               to = backup_folder, copy.date = TRUE,
                               overwrite = TRUE)
        if (copy_flag) {
            message("File '", file,
                    "' has been successfully backed up into folder '",
                    backup_folder, "'.")
        } else {
            warning("Fail to back up file '", file, "'.", call. = FALSE)
        }
    } else {
        new_name_prefix <- name_create(prefix = prefix, suffix = newname_suffix)
        new_name <- file_path(dirname(file), paste0(new_name_prefix, ".", ext))
        rename_flag <- file.rename(from = file, to = new_name)
        if (rename_flag) {
            message("File '", file,
                    "' has been successfully backed up into folder '",
                    backup_folder, "' with a new name '", basename(new_name), "'.")
        } else {
            warning("Fail to back up file '", file, "'.", call. = FALSE)
        }
    }

    return(invisible())
}
# }}}1

# output_files {{{
output_files <- function (prefix, suffix_type = c("L", "C", "D"), ext = NULL,
                          type = c("input", "table", "meter", "variable", "sizing"),
                          simplify = FALSE, exist_only = FALSE) {
    # prefix <- file_prefix(prefix)
    suffix_type <- rlang::arg_match(suffix_type)

    suffix <- switch(suffix_type,
        L = c("*.mat", ".epw", ".idf", ".imf", "delight.dfdmp", "delight.eldmp",
              "delight.in", "delight.out", "dfs.csv", ".epmdet", ".epmidf",
              "map.csv", "map.tab", "map.txt", "meter.csv", "meter.tab",
              "meter.txt", "out.audit", "out.bnd", "out.csv", "out.dbg",
              "out.det", "out.dxf", "out.edd", "out.eio", "out.end",
              "out.epmdet", "out.epmidf", "out.err", "out.eso", "out.expidf",
              "out.mdd", "out.mtd", "out.mtr", "out.rdd", "out.rvaudit",
              "out.sci", "out.shd", "out.sln", "out.sql", "out.ssz", "out.svg",
              "out.tab", "out.txt", "out.wrl", "out.zsz", "screen.csv",
              "spark.log", "sqlite.err", "ssz.csv", "ssz.tab", "ssz.txt",
              "tbl.csv", "tbl.htm", "tbl.html", "tbl.tab", "tbl.txt", "tbl.xml",
              "zsz.csv", "zsz.tab", "zsz.txt"),

        C = c("*.mat", ".audit", ".bnd", ".csv", ".dbg", ".det", ".dxf", ".edd",
              ".eio", ".end", ".epmdet", ".epmidf", ".epw", ".err", ".eso",
              ".expidf", ".idf", ".imf", ".mdd", ".mtd", ".mtr", ".rdd",
              ".rvaudit", ".sci", ".shd", ".sln", ".sql", ".ssz", ".svg",
              ".tab", ".txt", ".wrl", "DFS.csv", "Map.csv", "Map.tab",
              "Map.txt", "Meter.csv", "Meter.tab", "Meter.txt", "Screen.csv",
              "Spark.log", "Sqlite.err", "Ssz.csv", "Ssz.tab", "Ssz.txt",
              "Table.csv", "Table.htm", "Table.html", "Table.tab", "Table.txt",
              "Table.xml", "Zsz.csv", "Zsz.tab", "Zsz.txt", "delight.dfdmp",
              "delight.eldmp", "delight.in", "delight.out"),

        D = c("*.mat", ".zsz", ".audit", ".bnd", ".csv", ".dbg", ".det", ".dxf",
              ".edd", ".eio", ".end", ".epmdet", ".epmidf", ".epw", ".err",
              ".eso", ".expidf", ".idf", ".imf", ".mdd", ".mtd", ".mtr", ".rdd",
              ".rvaudit", ".sci", ".shd", ".sln", ".sql", ".ssz", ".svg",
              ".tab", ".txt", ".wrl", "-delight.dfdmp", "-delight.eldmp",
              "-delight.in", "-delight.out", "-dfs.csv", "-map.csv", "-map.tab",
              "-map.txt", "-meter.csv", "-meter.tab", "-meter.txt",
              "-screen.csv", "-spark.log", "-sqlite.err", "-ssz.csv",
              "-ssz.tab", "-ssz.txt", "-table.csv", "-table.htm", "-table.html",
              "-table.tab", "-table.txt", "-table.xml", "-zsz.csv", "-zsz.tab",
              "-zsz.txt")
    )

    fixed <- TRUE
    if (missing(type)) type <- NULL
    if (!is.null(type)) {
        if (!is.null(ext)) {
            ext <- NULL
            warning("'ext' will be ignored when 'type' specified.", call. = FALSE)
        }
        type <- rlang::arg_match(type)
        if (identical(type, "input")) {
            ext <- c(".idf", ".imf", ".epw")
        } else if (identical(type, "table")) {
            ext <- switch(suffix_type, L = "tbl.", C = "Table.", D = "-table.")
        } else if (identical(type, "meter")) {
            ext <- switch(suffix_type, L = "meter.", C = "Meter.", D = "-meter.")
        } else if (identical(type, "variable")){
            fixed <- FALSE
            ext <- switch(suffix_type,
                          L = c("^out.csv$", "^out.txt$", "^out.tab$", "^out.sql$"),
                          C = c("^.csv$", "^.txt$", "^.tab$", "^.sql$"),
                          D = c("^.csv$", "^.txt$", "^.tab$", "^.sql$")
            )
        } else {
            ext <- switch(suffix_type,
                L = c("ssz.", "zsz."),
                C = c("Ssz.", "Zsz."),
                D = c("-ssz.", "-zsz.")
            )
        }
        simplify <- TRUE
    }

    if (!is.null(ext)) {
        assertthat::assert_that(is.character(ext))
        suffix <- purrr::map(paste0(ext), grep, x = suffix, fixed = fixed, value = TRUE)
        files <- purrr::map(suffix, ~{ if (!purrr::is_empty(.x)) paste0(prefix, .x) })
        if (exist_only) {
            if (!is.null(files)) {
                files <- purrr::map(files, ~{if (!is.null(.x)) .x[file.exists(.x)] })
            }
        }

        files <- purrr::set_names(files, ext)
        if (length(ext) == 1L) {
            simplify <- TRUE
        }
        if (simplify) {
            files <- unique(purrr::simplify(files))
        }
    } else {
        files <- paste0(prefix, suffix)
    }

    return(files)
}
# }}}
