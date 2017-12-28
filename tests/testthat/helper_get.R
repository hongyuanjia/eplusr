.get <- function (model, x) {
    environment(model$initialize)$private[[x]]
}
