# Check the required exports and generated data layout before allocating any engine state.
api_cap__check <- function(native, signatures, manifest, binding) {
    supported <- unique(vapply(
        Filter(function(x) x$status == "supported", manifest$methods),
        function(x) x$symbol,
        character(1)
    ))
    needed <- unique(c(
        setdiff(supported, ""),
        "stateNew",
        "stateDelete",
        "apiVersionFromEPlus",
        "freeAPIData",
        "stopSimulation",
        "apiErrorFlag",
        "resetErrorFlag",
        "setEnergyPlusRootDirectory"
    ))
    for (name in needed) {
        if (!is.function(native[[name]]) || is.null(signatures[[name]])) {
            api__error(paste("Missing required EnergyPlus symbol:", name))
        }
    }
    # The generated typeinfo owns the ABI layout; R only decides how its data is copied.
    layout <- native$APIDataEntry
    if (
        !inherits(layout, "typeinfo") ||
            !is.data.frame(layout$fields) ||
            !identical(
                as.character(layout$fields$name),
                binding$api_data_fields
            )
    ) {
        api__error("The EnergyPlus dynport lacks the APIDataEntry layout.")
    }
    layout
}
