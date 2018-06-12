.onLoad <- function(libname, pkgname) {
    suppressMessages(init_avail_eplus())

    for (i in names(.options)) {
        opt <- .options[[i]]
        if (is.character(opt)) {
            eval(parse(text = paste0("options(eplusr.", i ," = '", opt, "')")))
        } else {
            eval(parse(text = paste0("options(eplusr.", i ," = ", opt, ")")))
        }
    }
}
