# extract report variable transition rules from latested IDFVersionUpdater folder
extract_reportvar_rules <- function(eplus_src, ver = eplusr:::ALL_EPLUS_VER) {
    dir <- file.path(eplus_src, "src/Transition/SupportFiles")

    re <- "Report Variables (\\d+-\\d-\\d(?:-\\d+){0,1}) to (\\d+-\\d-\\d(?:-\\d+){0,1})"
    paths <- list.files(dir, re, full.names = TRUE)

    # extract versions
    m <- stringi::stri_match_first_regex(basename(paths), re)

    from <- eplusr:::standardize_ver(m[, 2L])
    to <- eplusr:::standardize_ver(m[, 3L])

    # only necessary for versions >= 7.2
    i_72 <- from >= "7.2"
    from <- from[i_72]
    to <- to[i_72]
    paths <- paths[i_72]

    i_ver <- ver %in% as.character(to)
    if (length(mis_ver <- ver[!i_ver])) {
        message("Versions did not have reporting variable transition rules and will be skipped: ",
            paste0("'", mis_ver, "'", collapse = ", ")
        )
    }
    i_ver <- as.character(to) %in% ver

    from <- from[i_ver]
    to <- to[i_ver]
    paths <- paths[i_ver]

    if (!length(to)) return(data.table::data.table())

    # store meta data
    report_vars <- data.table::data.table(path = paths,
        from = as.double(as.character(from[, 1L:2L])),
        to = as.double(as.character(to[, 1L:2L]))
    )

    # read tables
    # skip empty table if needed
    lnum <- vapply(paths, function(path) nrow(data.table::fread(path, sep = NULL, header = FALSE)), 1L)
    report_vars[lnum > 3L, dt := lapply(path, data.table::fread, skip = 2, fill = TRUE, sep = ",",
        col.names = c("old", "new", "special"), select = 1:3
    )]
    report_vars[lnum <= 3L, by = "from",
        dt := list(list(data.table(old = character(), new = character(), special = character())))
    ]

    # add version info and comine into one
    report_vars <- data.table::rbindlist(mapply(
        function(from, to, dt) {
            # add from and to
            dt <- data.table::set(data.table::copy(dt), NULL, c("from", "to"), list(from, to))
            data.table::setcolorder(dt, c("from", "to"))
        },
        report_vars$from, report_vars$to, report_vars$dt, SIMPLIFY = FALSE
    ))

    # remove redundant lines
    report_vars <- report_vars[!J("old variable name"), on = "old"]

    # tag delete
    report_vars[stringi::stri_detect_fixed(new, "delete", case_insensitive = TRUE), new := NA_character_]

    # change empty special comments to NA
    report_vars[stringi::stri_isempty(stringi::stri_trim_both(special)), special := NA_character_]

    data.table::fwrite(report_vars, file.path(tempdir(), "report_vars.csv"))

    # set index
    data.table::setindexv(report_vars, c("from", "to"))

    report_vars
}
