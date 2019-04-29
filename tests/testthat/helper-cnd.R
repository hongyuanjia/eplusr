catch_cnd <- function (expr) {
    tryCatch(condition = identity, {
        force(expr)
        return(NULL)
    })
}
