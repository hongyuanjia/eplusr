################################################################################
#                     EnergyPlus Working Directory Cleaning                    #
################################################################################

# clean_wd: A function to clean the working directory of EnergyPlus after
# backing up specifed files. All other files that are not parts of EnergyPlus
# input or output files will be left as it is.  This is also true if you rename
# those files of EnergyPlus before you run this function. Currently, files work
# as input schedule files are not supported, and will be left untouched.

# - 'path': A EnergyPlus working directory.

# - 'backup': Should be NULL or one of c('input', 'basic', 'all').  If NULL,
# none file will be backed up.  If 'input', *.imf, *.idf, *.epw, *.epmidf will
# be backed up.  If 'basic', *.imf, *.idf, *.ewp, *.epmidf, and results of
# summary report, meter output and variable output will be backed up.  If 'all',
# all EnergyPlus input and output files will be backed up.

# - 'backup_folder': A character indicates the name prefix of the folder that
# will be auto-generated in current working directory to store backed-up files.
# The full folder name will be ('backup_folder'_(suffix)). The suffix is a
# character that shows the current date and time in format like 'Apr06_01h_02m'.
# If this this folder already, which means that you run this function multiple
# times within 1 minute, the folder name will be the decribed above with 6
# random characters.

# - 'rename': If TRUE, the backed-up files will be renamed with the same suffix
# as backup_folder.

# {{{1
clean_wd <- function(path = getwd(), backup = NULL, backup_folder = NULL,
                           rename = FALSE) {

    # Store the original working directory of current R session.
    ori_wd <- getwd()

    # Check if input path exists.
    if (!dir.exists(path)) {
        stop("Input path does exist.", call. = FALSE)
    } else {
        # Set the working dirctory to the input of EnergyPlus result folder.
        setwd(path)

    }

    # Check for invalid input of argument 'backup'.
    if (!is.null(backup)) {
        if (is.na(match(backup, c("input", "basic", "all")))) {
            stop("Argument 'backup' should be NULL or one of c('input', 'basic', 'all').",
                 call. = FALSE)
        } else {
            # Get the output prefix.
            output_prefix <- get_eplus_output_prefix_str(path = path)

            if (is.null(output_prefix)) {
                message("No cleaning will be done.")
                return(invisible())
            }

            # Get the output pattern.
            output_pattern <- get_eplus_output_prefix_ptn(output_prefix = output_prefix)

            purrr::walk2(output_prefix, output_pattern,
                         ~backup_eplus_wd(path = path, type = backup,
                                          output_prefix = .x, output_pattern = .y,
                                          backup_folder = backup_folder, rename = rename))
            purrr::walk2(output_prefix, output_pattern,
                         ~clean_eplus_wd(path = path,
                                         output_prefix = .x, output_pattern = .y))
        }
    } else {
        # Get the output prefix.
        output_prefix <- get_eplus_output_prefix_str(path = path)

        if (is.null(output_prefix)) {
            message("No cleaning will be done.")
            return(invisible())
        }

        # Get the output pattern.
        output_pattern <- get_eplus_output_prefix_ptn(output_prefix = output_prefix)

        purrr::walk2(output_prefix, output_pattern,
                     ~clean_eplus_wd(path = path,
                                     output_prefix = .x, output_pattern = .y))
    }

    # Set the working directory back to the original path.
    setwd(ori_wd)
}
# }}}1

# backup_eplus_wd
# {{{1
backup_eplus_wd <- function(path = getwd(), type = NULL,
                            output_prefix, output_pattern,
                            backup_folder = NULL, rename = FALSE) {

    # Store the original working directory of current R session.
    ori_wd <- getwd()

    if (!dir.exists(path)) {
        stop("Input path does exist.", call. = FALSE)
    } else {
        # Set the working dirctory to the input of EnergyPlus result folder.
        setwd(path)
    }

    suffix <- suffix_create(string = "datetime")
    if (is.null(backup_folder)) {
        backup_folder <- name_create(prefix = "backups", suffix = suffix)
    } else {
        backup_folder <- name_create(prefix = backup_folder, suffix = suffix)
    }

    backup_folder <- folder_create(folder_name = backup_folder)

    backup_files <- get_eplus_backup_files(type = type,
                                           output_prefix = output_prefix,
                                           output_pattern = output_pattern)

    purrr::walk(backup_files,
                ~backup_file(file_name = .x, backup_folder = backup_folder,
                             rename = rename, newname_suffix = suffix)
    )

    # Set the working directory back to the original path.
    setwd(ori_wd)
}
# }}}1

# clean_eplus_wd
# {{{1
clean_eplus_wd <- function(path = getwd(),
                           output_prefix = NULL, output_pattern) {

    # Store the original working directory of current R session.
    ori_wd <- getwd()

    if (!dir.exists(path)) {
        stop("Input path does exist.", call. = FALSE)
    } else {
        # Set the working dirctory to the input of EnergyPlus result folder.
        setwd(path)
    }

    clean_files <- get_eplus_clean_files(output_prefix = output_prefix,
                                         output_pattern = output_pattern)

    purrr::walk(clean_files,
                function(file_name) {
                    if (file.exists(file_name)) {
                        unlink(file_name)
                        message("File ", file_name, " has been successfully deleted ",
                                "from folder '", path, "'.\n")
                    }
                })

    # Set the working directory back to the original path.
    setwd(ori_wd)
}
# }}}1

# get_eplus_clean_files
# {{{1
get_eplus_clean_files <- function (output_prefix, output_pattern,
                                   extra = c("Energy+.ini")) {
    if (all(output_prefix == "in", output_pattern == "legacy")) {
        clean_files <- c("BasementGHTIn.idf", "audit.out", "audit.out",
                         "eplusmap.csv", "eplusmap.tab", "eplusmap.txt",
                         "eplusmtr.csv", "eplusmtr.tab", "eplusmtr.txt",
                         "eplusout.audit", "eplusout.bnd", "eplusout.csv",
                         "eplusout.dbg", "eplusout.dbg",
                         "eplusout.delightdfdmp", "eplusout.delighteldmp",
                         "eplusout.delightin", "eplusout.delightout",
                         "eplusout.dfs", "eplusout.dxf", "eplusout.edd",
                         "eplusout.eio", "eplusout.end", "eplusout.err",
                         "eplusout.eso", "eplusout.inp", "eplusout.inp",
                         "eplusout.log", "eplusout.mdd", "eplusout.mtd",
                         "eplusout.mtr", "eplusout.rdd", "eplusout.sci",
                         "eplusout.shd", "eplusout.sln", "eplusout.sparklog",
                         "eplusout.sql", "eplusout.svg", "eplusout.tab",
                         "eplusout.txt", "eplusout.wrl", "eplusscreen.csv",
                         "eplusssz.csv", "eplusssz.tab", "eplusssz.txt",
                         "eplustbl.csv", "eplustbl.htm", "eplustbl.html",
                         "eplustbl.tab", "eplustbl.txt", "eplustbl.xml",
                         "epluszsz.csv", "epluszsz.tab", "epluszsz.txt",
                         "expanded.idf", "expandedidf.err", "in.epw", "in.idf",
                         "in.idf.temp", "in.imf", "in.stat", "out.idf",
                         "readvars.audit", "slab.int", "sqlite.err", "test.mvi")

        extra_files <- extra

        clean_files <- c(clean_files, extra_files)
    } else if (all(output_prefix != "in", output_pattern == "capital")) {
        output_suffix <- c("*.mat", ".Zsz", ".audit", ".bnd", ".csv", ".dbg",
                           ".det", ".dxf", ".edd", ".eio", ".end", ".epmdet",
                           ".epmidf", ".err", ".eso", ".expidf", ".idf", ".mdd",
                           ".mtd", ".mtr", ".rdd", ".rvaudit", ".sci", ".shd",
                           ".sln", ".sql", ".ssz", ".svg", ".tab", ".txt",
                           ".wrl", "DElight.dfdmp", "DElight.eldmp",
                           "DElight.in", "DElight.out", "DFS.csv", "Map.csv",
                           "Map.tab", "Map.txt", "Meter.csv", "Meter.tab",
                           "Meter.txt", "Screen.csv", "Spark.log", "Sqlite.err",
                           "Ssz.csv", "Ssz.tab", "Ssz.txt", "Table.csv",
                           "Table.htm", "Table.html", "Table.tab", "Table.txt",
                           "Table.xml", "Zsz.csv", "Zsz.tab", "Zsz.txt")

        clean_files <- paste0(output_prefix, output_suffix)

        extra_files <- c("Energy+.ini", "fort.6", "audit.out", "post_proc.bat")

        clean_files <- c(clean_files, extra_files)
    } else {
        stop("Invalid value of argument 'output_prefix' or output_pattern'.")
    }

    return(clean_files)
}
# }}}1

# get_eplus_backup_files
# {{{1
get_eplus_backup_files <- function (type = NULL,
                                    output_prefix, output_pattern) {
    if (is.null(type)) {
        return(NULL)
    } else if (type == "input") {
        if (all(output_prefix == "in", output_pattern == "legacy")) {
            backup_files <- c("in.imf", "in.idf", "out.idf", "in.epw")
        } else if (all(output_prefix != "in", output_pattern == "capital")) {
            backup_files <- paste0(output_prefix, c(".imf",".idf", ".epmidf"))
        } else {
            stop("Invalid value of argument 'output_prefix' or output_pattern'.")
        }
    } else if (type == "basic") {
        if (all(output_prefix == "in", output_pattern == "legacy")) {
            backup_files <- c(# Input files.
                              "in.imf", "in.idf", "out.idf", "in.epw",
                              # Raw output
                              "eplusout.eio", "eplusout.eso", "eplustbl.csv",
                              # Summary report
                              "eplustbl.htm", "eplustbl.html", "eplustbl.tab",
                              "eplustbl.txt", "eplustbl.xml",
                              # Meter output
                              "eplusmtr.csv", "eplusmtr.tab", "eplusmtr.txt",
                              # Variable output
                              "eplusout.tab","eplusout.txt","eplusout.csv",
                              "eplusout.sql")
        } else if (all(output_prefix != "in", output_pattern == "capital")) {
            backup_files <- c(# Input files
                              "in.imf", "in.idf", "out.idf", "in.epw",
                              # Raw output
                              "eplusout.eio", "eplusout.eso",
                              # Summary report
                              "eplustbl.csv", "eplustbl.txt", "eplustbl.tab",
                              "eplustbl.htm", "eplustbl.html", "eplustbl.xml",
                              # Meter output
                              "eplusmtr.csv", "eplusmtr.tab", "eplusmtr.txt",
                              # Variable output
                              "eplusout.tab","eplusout.txt","eplusout.csv",
                              "eplusout.sql")
            backup_files <- paste0(output_prefix,
                                     # Input files
                                   c(".imf",".idf", ".epmidf", ".epw",
                                     # Raw output
                                     ".eio", ".eso",
                                     # Summary report
                                     "Table.csv", "Table.txt", "Table.tab",
                                     "Table.htm", "Table.html", "Table.xml",
                                     # Meter output
                                     "Meter.csv", "Meter.tab", "Meter.txt",
                                     # Variable output
                                     ".csv", ".tab", ".txt"
                                     ))
        } else {
            stop("Invalid value of argument 'output_prefix' or output_pattern'.")
        }
    } else if (type == "all") {
        backup_files <- get_eplus_clean_files(output_prefix = output_prefix,
                                              output_suffix = output_suffix)
    } else {
        stop("Invalid value of argument 'type'.")
    }

    return(backup_files)
}
# }}}1

# suffix_create: A helper function to create a formatted string used as a folder
#                name.
# {{{1
suffix_create <- function (string = c("datetime", "date", "time")) {
    if (missingArg(string)) string <- "datetime"

    if (!is.na(match(string, c("datetime", "date", "time")))) {
        # Store the original time locale.
        ori_locale <- Sys.getlocale(category = "LC_TIME")
        # Change the time locale to "English_United States.1252". In order to
        # not print the locale while run this function, store the output of
        # Sys.setlocale().
        not_print <- Sys.setlocale(category = "LC_TIME", locale="English_United States.1252")
        # Get the name suffix and then restore time locale setting.

        if (match(string, "datetime")) {
            # suffix <- Sys.time() %>% format("(%b%d_%Hh_%Mm_%Ss)")
            suffix <- Sys.time() %>% format("(%b%d_%Hh_%Mm)")
        } else if (match(string, "date")) {
            suffix <- Sys.time() %>% format("(%b%d)")
        } else {
            suffix <- Sys.time() %>% format("(%Hh_%Mm_%Ss)")
        }

        # Change the time locale to the original value. In order to not print
        # the locale while run this function, store the output of
        # Sys.setlocale().
        not_print <- Sys.setlocale(category = "LC_TIME", locale= ori_locale)

    } else {
        suffix <- as.character(string) %>% stringr::str_trim() %>% gsub(x=., "\\s", "_")
    }

    return(suffix)
}
# }}}1

# name_create: A helper function to create a folder with name being the
#              concatenation of specified prefix and suffix string.
# {{{1
name_create <- function (prefix = "backups", suffix = suffix_create()) {
    prefix <- as.character(prefix) %>% stringr::str_trim() %>% gsub(x=., "\\s", "_")
    suffix <- as.character(suffix) %>% stringr::str_trim() %>% gsub(x=., "\\s", "_")

    name <- paste0(prefix, "_", suffix)
}
# }}}1

# folder_create: A helper function to create a folder. If the folder already
#                exist, a folder with a suffix of 5 random characters will be
#                created.
# {{{1
folder_create <- function (folder_name) {
    # If folder does not exist
    if (!dir.exists(folder_name)) {
        dir.create(folder_name)

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
# {{{1
backup_file <- function(file_name, backup_folder = name_create(),
                        rename = FALSE, newname_suffix = suffix_create()) {
    if (!dir.exists(backup_folder)) {
        stop("Backup folder'", backup_folder, "' does not exists.", call. = FALSE)
    }

    # If the file does not exist, return nothing.
    original_exist_flag <- file.exists(file_name)
    if (!original_exist_flag) {
        return(invisible())
    }

    if (!rename) {
        copy_flag <- file.copy(from = file_name,
                               to = backup_folder,
                               copy.mode = TRUE, copy.date = TRUE,
                               overwrite = TRUE)
        if (copy_flag) {
            message("File '", file.path(getwd(), file_name),
                    "' has been successfully backed up into folder '",
                    file.path(getwd(), backup_folder), "'.")
        } else {
            message("Fail to back up file '", file.path(getwd(), file_name), "'.")
        }
    } else {
        new_name <- paste0(file_path_sans_ext(file_name), "_",
                           newname_suffix, ".", file_ext(file_name))

        rename_flag <- file.copy(from = file_name,
                                 to = file.path(backup_folder, new_name),
                                 copy.mode = TRUE, copy.date = TRUE,
                                 overwrite = TRUE)
        if (rename_flag) {
            message("File '", file.path(getwd(), file_name),
                    "' has been successfully backed up into folder '",
                    file.path(getwd(), backup_folder),
                    "' with a new name '", new_name, "'.")
        } else {
            message("Fail to back up file '", file.path(getwd(), file_name), "'.")
        }
    }

    return(invisible())

}
    # # If the file already exists in the backup folder.
    # backup_exist_flag <- file.exists(file.path(backup_folder, file_name))
    # if (backup_exist_flag) {
    #     # Do not want to rename it.
    #     if (!rename) {
    #         message("File '", file.path(getwd(), file_name),
    #                 "' has already been backed up into folder '",
    #                 file.path(getwd(), backup_folder),
    #                 "' before, and will not be backed up this time.")
    #         return(invisible())
    #     # Want to rename it.
    #     } else {
    #         # Copy the original file into a temp dir in order to make it
    #         # possible to rename the file.
    #         file.copy(from = file_name, to = backup_folder,
    #                   copy.mode = TRUE, copy.date = TRUE, overwrite = TRUE)

    #         file.copy(from = file_name, to = file.path(backup_folder, "fuck.idf"),
    #                   copy.mode = TRUE, copy.date = TRUE, overwrite = TRUE)

    #         tempdir() %>% gsub(x=., "\\\\", "/")
    #         new_name <- paste0(file_path_sans_ext(file_name), "_",
    #                            newname_suffix, ".", file_ext(file_name))

    #         rename_flag <-
    #             file.rename(from = file.path(backup_folder, basename(file_name)),
    #                         to = file.path(backup_folder, new_name))

    #     }
    # }

    # # 'copy_flag' expected to be TRUE.
    # copy_flag <- file.copy(from = file_name, to = backup_folder,
    #                        copy.mode = TRUE, copy.date = TRUE, overwrite = FALSE)

    # #
    # if () {

    # }
    # # Otherwise, copy the file without renaming it.
    # if (all(copy_flag, !rename)) {
    #     message("File '", file.path(getwd(), file_name),
    #             "' has already been backed up into folder '",
    #             file.path(getwd(), backup_folder),
    #             "' before, and will not be backed up this time.")
    #     return(invisible())
    # # Copy the
    # } else if (all(copy_flag, rename)) {

    # }

    #     message("File '", file.path(getwd(), file_name),
    #             "' has already been backed up into folder '",
    #             file.path(getwd(), backup_folder),
    #             "' before, and will not be backed up this time.")
    #     return(invisible())
    # }

    # if (all(copy_flag, rename)) {
        # new_name <- paste0(file_path_sans_ext(file_name), "_",
        #                    newname_suffix, ".", file_ext(file_name))

    #     rename_flag <-
    #         file.rename(from = file.path(backup_folder, basename(file_name)),
    #                     to = file.path(backup_folder, new_name))

    #     if (rename_flag) {
    #         message("File '", file.path(getwd(), file_name),
    #                 "' has been successfully backed up into folder '",
    #                 file.path(getwd(), backup_folder),
    #                 "' with a new name '", new_name, "'.")
    #         return(invisible())
    #     } else {
    #         message("File '", file.path(getwd(), file_name),
    #                 "' has already been backed up into folder '",
    #                 file.path(getwd(), backup_folder),
    #                 "' before, and will not be backed up this time.")
    #     }
    # } else {
    #     if (copy_flag) {
    #         message("File '", file.path(getwd(), file_name),
    #                 "' has been successfully backed up into folder '",
    #                 file.path(getwd(), backup_folder), "'.")
    #     } else {
    #         message("Fail to back up file '", file.path(getwd(), file_name), "'.")
    #     }
    # }
    # # } else {
    # #     warning('File "',file_name, '" does not exist, and will not be backed up.',
    # #             call. = FALSE)
    # }
# }
# }}}1

# get_eplus_output_prefix_str
# {{{1
get_eplus_output_prefix_str <- function(path = getwd()) {
    # Store the original working directory of current R session.
    ori_wd <- getwd()

    if (!dir.exists(path)) {
        stop("Input path does exist.", call. = FALSE)
    } else {
        # Set the working dirctory to the input of EnergyPlus result folder.
        setwd(path)
    }

    output_prefix <- tools::list_files_with_exts(dir = path,
                                                 exts = c("imf", "idf", "epmidf"))
    output_prefix <- tools::file_path_sans_ext(basename(output_prefix))

    if (length(output_prefix) == 0) {
        message("None 'imf', 'idf' or 'epmidf' file found in the input path.")
        output_prefix <- NULL
    } else {
        output_prefix <- unique(gsub(x = output_prefix, "^out$", "in"))
    }

    # Set the working directory back to the original path.
    setwd(ori_wd)

    return(output_prefix)
}
# }}}1

# get_eplus_output_prefix_ptn
# {{{1
get_eplus_output_prefix_ptn <- function(output_prefix = get_eplus_output_prefix_str()) {

    if (all(is.null(output_prefix))) {
        output_pattern <- NULL
    } else {
        output_pattern <-
            purrr::map_chr(output_prefix, ~ifelse(.x == "in", "legacy", "capital"))
    }

    return(output_pattern)
}
# }}}1
