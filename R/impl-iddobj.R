#' @include impl-idd.R
NULL

# get_iddfield_relation {{{
get_iddfield_relation <- function (idd_env, class_id, field_id = NULL, name = TRUE,
                                    direction = c("ref_to", "ref_by", "all")) {
    direction <- match.arg(direction)
    if (direction == "ref_to") {
        res <- list(
            ref_to = get_idd_relation(idd_env, class_id, field_id,
                max_depth = 0L, name = name, direction = "ref_to"),
            ref_by = NULL
        )
    } else if (direction == "ref_by") {
        res <- list(
            ref_to = NULL,
            ref_by = get_idd_relation(idd_env, class_id, field_id,
                max_depth = 0L, name = name, direction = "ref_by")
        )
    } else {
        res <- list(
            ref_to = get_idd_relation(idd_env, class_id, field_id,
                max_depth = 0L, name = name, direction = "ref_to"),
            ref_by = get_idd_relation(idd_env, class_id, field_id,
                max_depth = 0L, name = name, direction = "ref_by")
        )
    }

    setattr(res, "class", c("IddRelation", class(res)))

    res
}
# }}}
# get_iddfield_possible {{{
get_iddfield_possible <- function (idd_env, class_id, field_id = NULL) {
    all <- if (is.null(field_id)) TRUE else FALSE
    if (all) {
        cls_id <- class_id
        fld <- idd_env$field[J(cls_id), on = "class_id"]
    } else {
        fld_id <- field_id
        fld <- idd_env$field[J(fld_id), on = "field_id"]
    }
    fld <- add_class_name(idd_env, fld)

    # auto fields
    set(fld, NULL, "auto", NA_character_)
    fld[J(TRUE), on = "autosizable", `:=`(auto = "Autosize")]
    fld[J(TRUE), on = "autocalculatable", `:=`(auto = "Autocalculate")]

    # default
    fld <- field_default_to_unit(fld, "si", if (in_ip_mode()) "ip" else "si")

    # range
    fld[, `:=`(range = list(ranger(minimum, lower_incbounds, maximum, upper_incbounds))), by = field_id]

    # reference
    ref <- get_iddfield_relation(idd_env, fld$class_id, fld$field_index, direction = "all")

    structure(
        list(
            possible = fld[, list(
                class_id, class_name, field_id, field_index, field_name,
                auto, default, default_num, choice, range
            )],
            relation = ref
        ),
        class = c("IddFieldPossible", "list")
    )
}
# }}}
# has_iddfield_relation {{{
has_iddfield_relation <- function (idd_env, class, field = NULL) {
    rel <- get_iddfield_relation(idd_env, class, field, name = FALSE, direction = "all")
    any(nrow(rel$ref_to) > 0L, nrow(rel$ref_by) > 0L)
}
# }}}
# has_iddfield_ref_by {{{
has_iddfield_ref_by <- function (idd_env, class, field = NULL) {
    nrow(get_iddfield_relation(idd_env, class, field, name = FALSE, direction = "ref_by")$ref_by) > 0L
}
# }}}
# has_iddfield_ref_to {{{
has_iddfield_ref_to <- function (idd_env, class, field = NULL) {
    nrow(get_iddfield_relation(idd_env, class, field, name = FALSE, direction = "ref_to")$ref_to) > 0L
}
# }}}

# get_iddobj_table {{{
get_iddobj_table <- function (idd_env, class_id = NULL, all = FALSE) {
    cols <- c("class_name", "field_index", "field_name", "units", "ip_units")

    fld <- get_idd_field(idd_env, class_id, all = all)[, .SD, .SDcols = cols]

    setnames(fld, c("class_name", "field_index", "field_name"), c("class", "index", "field"))

    if (in_ip_mode()) {
        set(fld, NULL, "units", NULL)
        setnames(fld, "ip_units", "unit")
    } else {
        set(fld, NULL, "ip_units", NULL)
        setnames(fld, "units", "unit")
    }

    fld
}
# }}}
# get_iddobj_string {{{
get_iddobj_string <- function (idd_env, class_id = NULL, comment = NULL, leading = 4L, sep_at = 29L) {
    fld <- get_idd_field(idd_env, class_id)
    # add fake value in order to correctly format
    set(fld, NULL, "value_chr", NA_character_)

    str_fld <- format_field(fld, leading = leading, sep_at = sep_at)
    str_cls <- paste0(fld$class_name[[1L]], ",")
    str_cmt <- NULL

    if (!is.null(comment)) {
        assert(is.character(comment))
        str_cmt <- c(paste0("!", comment), "")
    }

    c(str_cmt, str_cls, str_fld)
}
# }}}
