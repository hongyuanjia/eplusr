#' @include impl-idd.R
NULL

# get_iddobj_relation {{{
get_iddobj_relation <- function (idd_env, class_id, field_id = NULL,
                                 direction = c("ref_to", "ref_by", "all"), depth = 0L, name = FALSE,
                                 class = NULL, group = NULL, keep_all = FALSE,
                                 class_ref = c("both", "none", "all"), match_all = FALSE) {
    direction <- match.arg(direction)
    if (direction == "ref_to") {
        res <- list(
            ref_to = get_idd_relation(idd_env, class_id, field_id,
                direction = "ref_to", depth = depth, name = name,
                class = class, group = group, keep_all = keep_all,
                class_ref = class_ref, match_all = match_all),
            ref_by = NULL
        )
    } else if (direction == "ref_by") {
        res <- list(
            ref_to = NULL,
            ref_by = get_idd_relation(idd_env, class_id, field_id,
                direction = "ref_by", depth = depth, name = name,
                class = class, group = group, keep_all = keep_all,
                class_ref = class_ref, match_all = match_all)
        )
    } else {
        res <- list(
            ref_to = get_idd_relation(idd_env, class_id, field_id,
                direction = "ref_to", depth = depth, name = name,
                class = class, group = group, keep_all = keep_all,
                class_ref = class_ref, match_all = match_all),
            ref_by = get_idd_relation(idd_env, class_id, field_id,
                direction = "ref_by", depth = depth, name = name,
                class = class, group = group, keep_all = keep_all,
                class_ref = class_ref, match_all = match_all)
        )
    }

    if (name) setattr(res, "class", c("IddRelation", class(res)))

    res
}
# }}}
# get_iddobj_possible {{{
get_iddobj_possible <- function (idd_env, class_id = NULL, field_id = NULL) {
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
    fld <- field_default_to_unit(idd_env, fld, "si", if (in_ip_mode()) "ip" else "si")
    setnames(fld, c("default_chr", "default_num"), c("value_chr", "value_num"))
    # make sure default is a list
    if (nrow(fld) == 1L) {
        set(fld, NULL, "default", list(get_value_list(fld)))
    } else {
        set(fld, NULL, "default", get_value_list(fld))
    }

    # range
    fld[, `:=`(range = list(ranger(minimum, lower_incbounds, maximum, upper_incbounds))), by = field_id]

    res <- fld[, .SD, .SDcols = c(
        "class_id", "class_name",
        "field_id", "field_index", "field_name",
        "auto", "default", "choice", "range"
    )]

    setattr(res, "class", c("IddFieldPossible", class(res)))
    res
}
# }}}

# get_iddobj_table {{{
get_iddobj_table <- function (idd_env, class_id = NULL, all = FALSE) {
    fld <- get_idd_field(idd_env, class_id, all = all)[
        , .SD, .SDcols = c("class_name", "field_index", "field_name")
    ]

    setnames(fld, c("class_name", "field_index", "field_name"), c("class", "index", "field"))

    fld
}
# }}}
# get_iddobj_string {{{
get_iddobj_string <- function (idd_env, class_id = NULL, comment = NULL, leading = 4L, sep_at = 29L, all = FALSE) {
    fld <- get_idd_field(idd_env, class_id, property = c("units", "ip_units"), all = all)
    # add fake value in order to correctly format
    set(fld, NULL, "value_chr", NA_character_)

    str_fld <- format_field(fld, leading = leading, sep_at = sep_at)
    str_cls <- paste0(fld$class_name[[1L]], ",")
    str_cmt <- NULL

    if (!is.null(comment)) {
        str_cmt <- c(paste0("!", comment), "")
    }

    c(str_cmt, str_cls, str_fld)
}
# }}}
