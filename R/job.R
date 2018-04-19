# job {{{
Job <- R6::R6Class(classname = "EPJob",
    # # ACTIVE {{{
    # active = list(
    #     city = function (value) {
    #         # {{{
    #         if (missing(value)) {
    #             private$m_location$city
    #         } else {
    #             private$m_location$city <- value
    #         }
    #         # }}}
    #     },

    #     state_province = function (value) {
    #         # {{{
    #         if (missing(value)) {
    #             private$m_location$state_province
    #         } else {
    #             private$m_location$state_province <- value
    #         }
    #         # }}}
    #     },

    #     country = function (value) {
    #         # {{{
    #         if (missing(value)) {
    #             private$m_location$country
    #         } else {
    #             private$m_location$country <- value
    #         }
    #         # }}}
    #     },

    #     data_source = function (value) {
    #         # {{{
    #         if (missing(value)) {
    #             private$m_location$data_source
    #         } else {
    #             private$m_location$data_source <- value
    #         }
    #         # }}}
    #     },

    #     wmo_number = function (value) {
    #         # {{{
    #         if (missing(value)) {
    #             private$m_location$wmo_number
    #         } else {
    #             private$m_location$wmo_number <- value
    #         }
    #         # }}}
    #     },

    #     latitude = function (value) {
    #         # {{{
    #         if (missing(value)) {
    #             private$m_location$latitude
    #         } else {
    #             private$m_location$latitude <- value
    #         }
    #         # }}}
    #     },

    #     longtitude = function (value) {
    #         # {{{
    #         if (missing(value)) {
    #             private$m_location$longtitude
    #         } else {
    #             private$m_location$longtitude <- value
    #         }
    #         # }}}
    #     },

    #     time_zone = function (value) {
    #         # {{{
    #         if (missing(value)) {
    #             private$m_location$time_zone
    #         } else {
    #             private$m_location$time_zone <- value
    #         }
    #         # }}}
    #     },

    #     elevation = function (value) {
    #         # {{{
    #         if (missing(value)) {
    #             private$m_location$elevation
    #         } else {
    #             private$m_location$elevation <- value
    #         }
    #         # }}}
    #     }

    # ),
    # # }}}

    public = list(

        # INITIALIZE {{{
        initialize = function (idf, epw) {
            assert_that(is_idf(idf))
            assert_that(is_epw(epw))
            private$m_idf <- idf
            private$m_epw <- epw

            # try to locate EnergyPlus of this version
            eplus_path <- eplus_default_path(idf$version())
            if (is_valid_eplus_path(eplus_path)) {
                self$set_eplus(eplus_path)
                if (is.null(private$m_eplus$version)) {
                    stop("Failed to find corresponding version of EnergyPlus.",
                         call. = FALSE)
                }
            } else {
                warning("Cannot locate EnergyPlus V", private$m_idf$version,
                         " at default installation path. Please set the ",
                         "EnergyPlus installation path using `$set_eplus()`.",
                         call. = FALSE)
            }

            # init log {{{
            private$m_log <- data.table::data.table(
                no = integer(), idf = character(), epw = character(),
                dir = character(), time = character(), proc = list(),
                wait = logical(), sql = character()
            )
            # }}}
        },
        # }}}

        set_eplus = function (path) {
            # set the EnergyPlus path to use
            # {{{
            if (!is_valid_eplus_path(path)) {
                stop("Input path is not a valid EnergyPlus path")
            }
            # give a message if the eplus path has been initialized before
            old_path <- private$m_eplus$path
            old_ver <- private$m_eplus$version
            new_path <- normalizePath(path)
            new_ver <- get_ver_from_path(path)
            if (is.null(new_ver)) {
                stop("Failed to detect the version of EnergyPlus located in ",
                     backtick(new_path), ".", call. = FALSE)
            }
            if (!is.null(old_ver)) {
                if (old_ver != new_ver) {
                    warning("Change the attached EnergyPlus version from ",
                            backtick(old_ver), " to ", backtick(new_ver), ".",
                            "Simulation errors may occur.", call. = FALSE)
                }
            }
            if (!is.null(private$m_eplus$path)) {
                message("Change the attached EnergyPlus path from ",
                        backtick(private$m_eplus$path), "to ", backtick(new_path), ".")
            }
            private$m_eplus$path <- new_path
            private$m_eplus$exe <- paste0("energyplus", exe())
            private$m_eplus$version <- new_ver
            # }}}
        },

        run = function (dir = NULL, wait = FALSE, auto_save = FALSE, ...) {
            # run simulation job
            # {{{
            if (is.null(private$m_eplus$path)) {
                stop("There is no attached EnergyPlus to this Idf. Please set ",
                     "the EnergyPlus path to use using $set_eplus().", call. = FALSE)
            }
            assert_that(private$m_eplus$version >= 8.3,
                msg = "Currently, `$run()` only supports EnergyPlus V8.3 or higher.")

            private$run_info(dir = dir)
            eplus <- file.path(private$m_eplus$path, private$m_eplus$exe)
            proc <- run_idf(eplus,
                            private$m_info$path_idf,
                            private$m_info$path_epw,
                            private$m_info$out_dir,
                            echo = wait, ...)
            # private$log_job(proc, wait)
            # }}}
        }
    ),

    private = list(
        m_idf = NULL,
        m_epw = NULL,
        m_eplus = NULL,
        m_info = NULL,
        m_log = NULL,

        run_info = function (dir = NULL, auto_save = FALSE) {
            # get basic simulation run info
            # {{{
            # if the model is not created from a local file
            path_idf <- private$m_idf$path()
            if (is.null(path_idf)) {
                # and the output dir is not given
                if (is.null(dir)) {
                    # save it as a temp file and run it in temp dir
                    run_dir <- normalizePath(file.path(tempdir(), "eplusr", "idf"),
                        mustWork = FALSE)
                    path_idf <- normalizePath(tempfile(pattern = "idf_", tmpdir = run_dir, fileext = ".idf"),
                        mustWork = FALSE)
                    name_idf <- tools::file_path_sans_ext(basename(path_idf))
                    message(msg("Could not find model file path, nor `dir` is
                                given. The model will be saved as a temporary
                                file at ", backtick(run_dir), " and run in that
                                directory."))
                    flg_sav <- TRUE
                # but output dir is given
                } else {
                    # save it to the given output dir with random name and run
                    # it in that dir
                    run_dir <- normalizePath(dir, mustWork = FALSE)
                    path_idf <- normalizePath(tempfile("model_", run_dir, ".idf"),
                        mustWork = FALSE)
                    name_idf <- tools::file_path_sans_ext(basename(path_idf))
                    message(msg("The model will be saved in the output dir ",
                                backtick(run_dir), " with a random name ",
                                backtick(paste0(name_idf, ".idf")), " and run
                                there."))
                    flg_sav <- TRUE
                }
            # if the model is created from a local file
            } else {
                # but the output dir is not given
                if (is.null(dir)) {
                    # use the model path
                    run_dir <- dirname(path_idf)
                    # # save it as a temp file with same name and run it in temp dir
                    # run_dir <- normalizePath(file.path(tempdir(), "eplusr", "idf"),
                    #     mustWork = FALSE)
                    name_idf <- tools::file_path_sans_ext(basename(path_idf))
                    path_idf <- normalizePath(file.path(run_dir, paste0(name_idf, ".idf")),
                        mustWork = FALSE)
                    message("`dir` is not given. The model will be run in ",
                            "the same folder as the Idf file.")
                    # if there are unsaved changes, give warning
                    if (private$m_idf$is_unsaved()) {
                        if (!auto_save) {
                            warning("The Idf has been modified without saving. Simulation ",
                                    "results may not be correct.", call. = FALSE)
                            flg_sav <- FALSE
                        } else {
                            message("The Idf has been modified. It will be saved before run.",
                                    call. = FALSE)
                            flg_sav <- TRUE
                        }
                    }
                # and the output dir is given
                } else {
                    # save it with same name in the output dir and run it there
                    run_dir <- normalizePath(dir, mustWork = FALSE)
                    name_idf <- tools::file_path_sans_ext(basename(path_idf))
                    path_idf <- normalizePath(file.path(run_dir, paste0(name_idf, ".idf")),
                        mustWork = FALSE)
                    if (path_idf == private$m_idf$path()) {
                        flg_sav <- FALSE
                    } else {
                        flg_sav <- TRUE
                    }
                }
            }

            # create output dir
            if (!dir.exists(run_dir)) {
                if (!flg_sav) {
                    warning("The Idf file has been deleted. It will be created ",
                            "using `Idf$save()` before run.", call. = FALSE)
                    flg_sav <- TRUE
                }
                tryCatch(dir.create(run_dir, recursive = TRUE),
                    warning = function (w) {
                        stop("Failed to create output directory: ",
                             backtick(run_dir), call. = FALSE)
                    }
                )
            }

            # resolve external file links
            flg_res <- private$resolve_external(run_dir)
            if (flg_res) flg_sav <- TRUE

            # save the model if necessary
            if (flg_sav) {
                readr::write_lines(private$m_idf$string(), path_idf)
            }

            path_epw <- private$m_epw$path()
            if (is.null(path_epw)) {
                flg_epw_sav <- TRUE
                path_epw <- normalizePath(tempfile(pattern = "epw_", tmpdir = run_dir,
                    fileext = ".epw"))
                warning("Epw was not created from local file. It will be created ",
                        "with random name ", basename(path_epw), "in the output directory ",
                        backtick(run_dir), " before run.", call. = FALSE)
            } else if (!file.exists(path_epw)) {
                flg_epw_sav <- TRUE
                warning("Epw file has been deleted. It will be created using",
                        "`Epw$save()` in the output directory ", backtick(run_dir),
                        " before run.", call. = FALSE)
            } else {
                flg_epw_sav <- FALSE
            }

            name_epw <- tools::file_path_sans_ext(basename(path_epw))
            if (flg_epw_sav) {
                private$m_epw$save(path_epw, overwrite = TRUE)
            }

            # save info
            private$m_info$out_dir <- run_dir
            private$m_info$path_idf <- path_idf
            private$m_info$path_epw <- path_epw
            private$m_info$name_idf <- name_idf
            private$m_info$name_epw <- name_epw
            # }}}
        },

        resolve_external = function (dir) {
            # change external file location to local output dir
            # {{{
            idf <- private$m_idf
            ori <- getwd()
            setwd(dir)
            on.exit(setwd(ori), add = TRUE)
            if (!idf$is_valid_class("Schedule:File")) return(FALSE)
            sch <- idf$objects_in_class("Schedule:File")
            flg_res <- FALSE
            for (i in sch) {
                link <- normalizePath(i$get_value(name = "File Name")[[1]],
                    mustWork = FALSE)
                name <- basename(link)
                dest <- normalizePath(file.path(dir, name), mustWork = FALSE)
                if (file.exists(link) & dest != link) {
                    flg_c <- file.copy(link, dest, overwrite = TRUE,
                                             copy.date = TRUE)
                    if (flg_c) {
                        i$set_value(`File Name` = name)
                        flg_res <<- TRUE
                    }
                } else if (!file.exists(link)){
                    warning("Broken external file link found in Idf: ",
                        backtick(link), ".", call. = FALSE)
                }
            }
            flg_res
            # }}}
        },

        log_job = function (proc) {
            # log job info
            # {{{
            sql <- normalizePath(file.path(private$m_info$out_dir,
                paste0(private$m_inof$name_idf, ".sql")), mustWork = FALSE)
            new_log <- data.table::data.table(
                no = nrow(private$m_log) + 1L,
                idf = private$m_info$path_idf,
                epw = private$m_info$path_epw,
                dir = private$m_info$out_dir,
                time = Sys.time(),
                proc = proc,
                wait = wait,
                sql = sql
            )

            private$m_log <- data.table::rbindlist(list(
                private$m_log, new_log
            ))
            # }}}
        }
    )
)
# }}}

