# ---
# repo: hongyuanjia/schemate
# file: standalone-schema.R
# last-updated: 2026-06-15
# copyright: Copyright (c) 2026 Hongyuan Jia
# SPDX-License-Identifier: MIT
# license: MIT
# imports: [checkmate (>= 2.0.0), S7]
# optional: [jsonlite]
# source-commit: 54c76104ed0cfb95104b2c42e552b3da2017f937
# ---
#
# # Standalone Changelog
#
# ## 2026-06-14
# - Add file-level copyright and SPDX license metadata to the generated bundle.
# - Improve validation diagnostics for `all`, `any`, `one`, and `not`
#   combinators.
#
# ## 2026-06-13
# - Match S7 base classes in checkmate rules.
#
# ## 2026-06-06
# - Rename missing-target controls from `error_if_missing` to
#   `missing = "error"` / `missing = "ignore"` across schema deletion and
#   predicate-based batch edit helpers.
# - Add public `schema_flatten()` for reusing flattened schemas across repeated
#   validation calls.
# - Speed up exact field validation for wide schemas and reduce key-copying
#   overhead while compacting grouped fields.
# - Speed up schema compaction's structural equality checks by comparing S7 slots
#   directly and normalizing `keys` rules consistently with `check_names()`.
# - Reduce intermediate S7 object construction while compiling grouped fields and
#   compacting container field groups.
# - Preserve grouped field bindings when predicate-based batch edits rewrite every
#   grouped field to structurally equivalent targets.
# - Cache fixed `schema_replace_where()` replacements, including cached flat
#   replacements for `SchemaFlat` inputs.
# - Run standalone local verification against a temporary copy of the full package
#   test suite, then remove the copied tests.
# - Use S7 double dispatch for compact structural comparisons.
#
# ## 2026-06-05
# - Treat `fields`, `patterns`, and `positions` as optional child validators
#   during schema validation; use `keys` and `check` rules for required keys and
#   length constraints.
#
# ## 2026-06-04
# - Add schema path query and batch edit helpers: `schema_paths()`,
#   `schema_find()`, `schema_modify_where()`, `schema_replace_where()`,
#   `schema_where_path()`, and `schema_where_check()`.
# - Cleanup roxygen2 documentation comments when bundling.
# - Share standalone DESCRIPTION generation logic between generation and local
#   tests, while preserving the source package DESCRIPTION.
#
# ## 2026-06-03
#
# - Initial standalone schema bundle for `schemate`.
# - Includes schema inference, schema editing, JSON reading/writing, and schema
#   validation in a single `standalone-schema.R` file.
# - Rename non-exported functions with consistent prefixes.
# - Add `patterns` and `rest` schema support, including `schema_set_rest()` and
#   `schema_del_rest()`.
# - Add `positions` schema support for unnamed list prefix validation, including
#   `schema_add_position()` and `schema_del_position()`.
# - Add `schema_infer(arrays = "rest")` and `schema_compact()` for compacting
#   inferred JSON array schemas.
# - Allow logical field edit paths to traverse grouped schema fields.
# - Allow `groups` entries to contain complete schema nodes for JSON round-trips.
# - Treat `jsonlite` as optional; JSON IO requires it at runtime, while printing
#   has a base R fallback.
# - Refactor schema compaction internals to use S7 dispatch.
# - Remove the unimplemented `defs` argument from `schema_compact()`.
# - Expand package documentation and examples for nested objects, data frames,
#   rest schemas, positions, and validation diagnostics.
# - Narrow the standalone changelog pre-commit reminder to source and standalone
#   tooling changes.
#
# nocov start

# -------------------- schema-utils.R
# shared utilities {{{
schema_utils__coalesce <- function(x, y) {
    if (is.null(x)) y else x
}

schema_utils__checkmate_result <- function(result, label = NULL) {
    if (isTRUE(result)) {
        return(NULL)
    }

    substr(result, 1L, 1L) <- tolower(substr(result, 1L, 1L))

    if (!is.null(label)) {
        return(sprintf("%s: %s", label, result))
    }

    result
}

schema_utils__formal_args <- function(def) {
    names(formals(def, envir = parent.frame()))
}

schema_utils__require_namespace <- function(pkg, reason) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        return(invisible(TRUE))
    }

    stop(
        sprintf(
            "Package `%s` is required to %s. Install it with `install.packages(\"%s\")`.",
            pkg,
            reason,
            pkg
        ),
        call. = FALSE
    )
}

schema_utils__checkmate_fun_cache <- new.env(parent = emptyenv())

schema_utils__checkmate_fun <- function(kind) {
    checkmate::assert_string(kind)
    fun <- schema_utils__checkmate_fun_cache[[kind]]
    if (is.null(fun)) {
        fun <- utils::getFromNamespace(paste0("check_", kind), asNamespace("checkmate"))
        schema_utils__checkmate_fun_cache[[kind]] <- fun
    }

    fun
}

schema_utils__checkmate_args <- function(kind) {
    schema_utils__formal_args(schema_utils__checkmate_fun(kind))[-1L]
}

schema_utils__checkmate_validator <- function(check, ..., label = NULL) {
    checkmate::assert_function(check)

    args <- list(...)
    force(label)

    function(value) {
        schema_utils__checkmate_result(do.call(check, c(list(value), args)), label = label)
    }
}

schema_utils__checkmate_rule <- function(class, check, ..., label = NULL, branch = NULL) {
    checkmate::assert_function(check)
    checkmate::assert_string(label, null.ok = TRUE)
    checkmate::assert_string(branch, null.ok = TRUE)

    if (!isS4(class)) {
        checkmate::assert_multi_class(
            class,
            c("S7_class", "S7_base_class", "S7_S3_class", "S7_missing", "S7_any"),
            null.ok = TRUE
        )
    }

    structure(
        list(
            class = class,
            check = check,
            args = list(...),
            label = label,
            branch = branch
        ),
        class = "CheckmateRule"
    )
}

schema_utils__checkmate_any <- function(...) {
    rules <- list(...)
    checkmate::assert_list(rules, "CheckmateRule", any.missing = FALSE, min.len = 1L, null.ok = FALSE)

    structure(
        list(
            mode = "any",
            rules = rules,
            class = Reduce(`|`, lapply(rules, `[[`, "class"))
        ),
        class = c("CheckmateSpecAny", "CheckmateSpec")
    )
}

schema_utils__base_type <- function(value) {
    switch(
        typeof(value),
        closure = ,
        builtin = ,
        special = "function",
        language = "call",
        symbol = "name",
        typeof(value)
    )
}

schema_utils__match_class <- function(value, class) {
    if (is.null(class)) {
        return(is.null(value))
    }
    if (inherits(class, "S7_any")) {
        return(TRUE)
    }
    if (inherits(class, "S7_missing")) {
        return(missing(value))
    }
    if (inherits(class, "S7_base_class")) {
        return(identical(schema_utils__base_type(value), class$class))
    }
    if (inherits(class, "S7_union")) {
        return(any(vapply(class$classes, schema_utils__match_class, logical(1L), value = value)))
    }
    if (inherits(class, "S7_S3_class")) {
        return(!isS4(value) && all(class$class %in% class(value)))
    }
    if (inherits(class, "S7_class")) {
        return(S7::S7_inherits(value, class))
    }
    if (isS4(class)) {
        return(isS4(value) && inherits(value, class@className))
    }

    inherits(value, class)
}

schema_utils__checkmate_match_rule <- function(value, rules) {
    for (i in seq_along(rules)) {
        rule_class <- rules[[i]]$class

        if (schema_utils__match_class(value, rule_class)) {
            return(i)
        }
    }

    NA_integer_
}

schema_utils__checkmate_validate_rule <- function(value, rule) {
    msg <- schema_utils__checkmate_result(
        do.call(rule$check, c(list(value), rule$args)),
        label = rule$label
    )

    if (!is.null(msg) && !is.null(rule$branch)) {
        msg <- sprintf("[%s] %s", rule$branch, msg)
    }

    msg
}

schema_utils__checkmate_property <- function(
    class = S7::class_any,
    check,
    ...,
    getter = NULL,
    setter = NULL,
    default = NULL,
    name = NULL,
    label = NULL
) {
    if (inherits(class, "CheckmateSpec")) {
        spec <- class
        extra_args <- list(...)

        if (!missing(check)) {
            stop("When `class` is a `CheckmateSpec`, `check` must be omitted.", call. = FALSE)
        }
        if (length(extra_args)) {
            stop(
                "When `class` is a `CheckmateSpec`, checker arguments must be supplied in `schema_utils__checkmate_rule()`.",
                call. = FALSE
            )
        }
        if (!is.null(label)) {
            stop(
                "When `class` is a `CheckmateSpec`, `label` must be supplied in `schema_utils__checkmate_rule()`.",
                call. = FALSE
            )
        }

        return(S7::new_property(
            class = spec$class,
            getter = getter,
            setter = setter,
            validator = function(value) {
                idx <- schema_utils__checkmate_match_rule(value, spec$rules)

                if (is.na(idx)) {
                    return("No matching validation branch found.")
                }

                schema_utils__checkmate_validate_rule(value, spec$rules[[idx]])
            },
            default = default,
            name = name
        ))
    }

    if (missing(check)) {
        stop("`check` must be supplied unless `class` is a `CheckmateSpec`.")
    }

    S7::new_property(
        class = class,
        getter = getter,
        setter = setter,
        validator = schema_utils__checkmate_validator(check, ..., label = label),
        default = default,
        name = name
    )
}

schema_utils__convert <- S7::new_generic(
    "schema_utils__convert",
    "from",
    function(from, to, ...) S7::S7_dispatch()
)
# }}}

# schema_utils__prop {{{
schema_utils__prop <- function(class, check, null.ok = FALSE, ...) {
    checkmate::assert_flag(null.ok)

    if (null.ok) {
        if (!"null.ok" %in% schema_utils__formal_args(check)) {
            stop("input checkmate function does not support `null.ok` argument, but `null.ok = TRUE` was specified.")
        }
        class <- NULL | class
    }

    schema_utils__checkmate_property(
        class,
        check,
        null.ok = null.ok,
        ...
    )
}

schema_utils__prop_string <- function(min.chars = 1L, null.ok = TRUE, default = NULL, ...) {
    schema_utils__prop(
        S7::class_character,
        checkmate::check_string,
        null.ok = null.ok,
        min.chars = min.chars,
        default = default,
        ...
    )
}

schema_utils__prop_character <- function(any.missing = FALSE, min.chars = 1L, null.ok = FALSE, default = NULL, ...) {
    schema_utils__prop(
        S7::class_character,
        checkmate::check_character,
        null.ok = null.ok,
        any.missing = any.missing,
        min.chars = min.chars,
        default = default,
        ...
    )
}

schema_utils__prop_choice <- function(choices, null.ok = TRUE, default = NULL, ...) {
    schema_utils__prop(
        S7::class_character,
        checkmate::check_choice,
        choices = choices,
        null.ok = null.ok,
        default = default,
        ...
    )
}

schema_utils__prop_list <- function(types = character(), names = "unique", null.ok = TRUE, default = list(), ...) {
    if (length(types) && !is.null(utils::packageName())) {
        types <- unique(c(paste0(utils::packageName(), "::", types), types))
    }

    schema_utils__prop(
        S7::class_list,
        checkmate::check_list,
        types = types,
        names = names,
        null.ok = null.ok,
        default = default,
        ...
    )
}

schema_utils__prop_ref <- function(null.ok = TRUE) {
    schema_utils__prop_string(
        null.ok = null.ok,
        min.chars = 1L,
        pattern = "^#/\\$defs/[^/]+$"
    )
}
# }}}

schema_utils__as_json <- S7::new_generic("schema_utils__as_json", "x", function(x, ...) S7::S7_dispatch())

schema_utils__as_list_add_desc <- function(out, x) {
    if (is.null(x@desc)) {
        return(out)
    }

    c(list(description = x@desc), out)
}

schema_utils__as_list_nary <- function(x, operator) {
    out <- list()
    out[[operator]] <- lapply(x@branches, as.list)
    schema_utils__as_list_add_desc(out, x)
}

schema_utils__as_list_rule <- function(x) {
    c(list(kind = x@kind), unclass(x@args))
}

schema_utils__as_list_rule_names <- function(x, drop_kind = FALSE, empty_as_null = FALSE) {
    if (is.null(x)) {
        return(NULL)
    }

    args <- unclass(x@args)
    if (drop_kind) {
        args <- args[names(args) != "kind"]
    }
    if (empty_as_null && !length(args)) {
        return(NULL)
    }

    args
}

schema_utils__keys_as_list <- function(x, empty_as_null = TRUE) {
    schema_utils__as_list_rule_names(x, drop_kind = TRUE, empty_as_null = empty_as_null)
}

schema_utils__json_indent <- function(depth, pretty) {
    if (!pretty) {
        return("")
    }

    paste(rep("  ", depth), collapse = "")
}

schema_utils__json_quote <- function(x) {
    out <- encodeString(x, quote = "\"", justify = "none")
    out[is.na(x)] <- "null"
    out
}

schema_utils__json_convert <- function(x, depth = 0L, pretty = TRUE, auto_unbox = TRUE) {
    if (is.list(x)) {
        return(schema_utils__json_convert_list(x, depth = depth, pretty = pretty, auto_unbox = auto_unbox))
    }

    schema_utils__json_convert_atom(x, depth = depth, pretty = pretty, auto_unbox = auto_unbox)
}

schema_utils__json_convert_list <- function(x, depth, pretty, auto_unbox) {
    nms <- names(x)
    named <- !is.null(nms)
    indent <- schema_utils__json_indent(depth, pretty = pretty)
    child_indent <- schema_utils__json_indent(depth + 1L, pretty = pretty)
    newline <- if (pretty) "\n" else ""
    space <- if (pretty) " " else ""

    if (!length(x)) {
        return(if (named) "{}" else "[]")
    }

    values <- vapply(
        x,
        schema_utils__json_convert,
        character(1L),
        depth = depth + 1L,
        pretty = pretty,
        auto_unbox = auto_unbox,
        USE.NAMES = FALSE
    )

    if (!named) {
        if (!pretty) {
            return(paste0("[", paste(values, collapse = ","), "]"))
        }

        return(paste0(
            "[",
            newline,
            child_indent,
            paste(values, collapse = paste0(",", newline, child_indent)),
            newline,
            indent,
            "]"
        ))
    }

    keys <- schema_utils__json_quote(nms)
    entries <- paste0(keys, ":", space, values)
    if (!pretty) {
        return(paste0("{", paste(entries, collapse = ","), "}"))
    }

    paste0(
        "{",
        newline,
        child_indent,
        paste(entries, collapse = paste0(",", newline, child_indent)),
        newline,
        indent,
        "}"
    )
}

schema_utils__json_convert_atom <- function(x, depth, pretty, auto_unbox) {
    if (is.null(x)) {
        return("null")
    }

    if (!length(x)) {
        return("[]")
    }

    if (is.character(x)) {
        values <- schema_utils__json_quote(x)
    } else if (is.logical(x)) {
        values <- ifelse(x, "true", "false")
        values[is.na(x)] <- "null"
    } else if (is.numeric(x)) {
        values <- as.character(x)
        values[is.na(x) | !is.finite(x)] <- "null"
    } else {
        stop(sprintf("Cannot serialize object of type `%s` with base R JSON fallback.", typeof(x)), call. = FALSE)
    }

    if (auto_unbox && length(values) == 1L) {
        return(values[[1L]])
    }

    indent <- schema_utils__json_indent(depth, pretty = pretty)
    child_indent <- schema_utils__json_indent(depth + 1L, pretty = pretty)
    newline <- if (pretty) "\n" else ""

    if (!pretty) {
        return(paste0("[", paste(values, collapse = ","), "]"))
    }

    paste0(
        "[",
        newline,
        child_indent,
        paste(values, collapse = paste0(",", newline, child_indent)),
        newline,
        indent,
        "]"
    )
}

schema_utils__to_json_fallback <- function(x, pretty = TRUE, auto_unbox = TRUE) {
    if (is.object(x) && !is.list(x)) {
        x <- as.list(x)
    }

    schema_utils__json_convert(x, depth = 0L, pretty = pretty, auto_unbox = auto_unbox)
}

schema_utils__as_json_impl <- function(x, pretty = TRUE, auto_unbox = TRUE) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
        return(schema_utils__to_json_fallback(x, pretty = pretty, auto_unbox = auto_unbox))
    }

    jsonlite::toJSON(
        as.list(x),
        pretty = pretty,
        auto_unbox = auto_unbox,
        null = "null",
        na = "null"
    )
}

schema_utils__cat_json <- function(x, ..., pretty = TRUE, auto_unbox = TRUE) {
    cat(schema_utils__as_json(x, pretty = pretty, auto_unbox = auto_unbox), "\n", sep = "")
    invisible(x)
}


# -------------------- schema-spec.R

SCHEMA_SPEC_KINDS <- sort(sub("^check_", "", grep("^check_", getNamespaceExports("checkmate"), value = TRUE)))
SCHEMA_SPEC_KINDS_CONTAINER <- c("list", "data_frame", "data_table", "tibble")
SCHEMA_SPEC_OPERATORS <- c("check", "$ref", "all", "any", "one", "not")
SCHEMA_SPEC_KEYWORDS <- c(SCHEMA_SPEC_OPERATORS, "fields", "groups", "patterns", "positions", "rest", "keys", "description", "$defs", "version")

# SchemaSpec {{{
# - node variants become distinct classes instead of a single tagged union
# - rule payload is represented by shared `SchemaRule*` classes
# - exact and pattern bindings are represented by separate classes
# - positions child list represents unnamed prefixItems semantics
# - rest child is made explicit
# - n-ary combinators share common abstract parents
schema_spec__kind_is_container <- function(kind) {
    kind %in% SCHEMA_SPEC_KINDS_CONTAINER
}

SchemaSpec <- S7::new_class("SchemaSpec", abstract = TRUE)

SchemaRule <- S7::new_class(
    "SchemaRule",
    parent = SchemaSpec,
    abstract = TRUE
)

SchemaRuleCheck <- S7::new_class(
    "SchemaRuleCheck",
    parent = SchemaRule,
    properties = list(
        kind = schema_utils__prop_choice(SCHEMA_SPEC_KINDS, null.ok = FALSE),
        args = schema_utils__prop_list(null.ok = FALSE, names = "unique", default = list())
    ),
    validator = function(self) {
        if (length(self@args)) {
            schema_utils__checkmate_result(
                checkmate::check_subset(
                    names(self@args),
                    schema_utils__checkmate_args(self@kind),
                    empty.ok = FALSE
                ),
                sprintf("invalid arguments for check kind `%s`", self@kind)
            )
        }
    }
)

SchemaRuleNames <- S7::new_class(
    "SchemaRuleNames",
    parent = SchemaRule,
    properties = list(
        args = schema_utils__prop_list(null.ok = FALSE, names = "unique", default = list())
    ),
    validator = function(self) {
        if (!length(self@args)) {
            return(NULL)
        }

        schema_utils__checkmate_result(
            checkmate::check_subset(names(self@args), schema_utils__formal_args(checkmate::check_names)[-1L]),
            "invalid arguments for `checkmate::check_names()`"
        )
    }
)

schema_spec__name_type <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }

    x@args$type
}

SchemaNode <- S7::new_class(
    "SchemaNode",
    parent = SchemaSpec,
    properties = list(desc = schema_utils__prop_string()),
    abstract = TRUE
)

SchemaBindingExact <- S7::new_class(
    "SchemaBindingExact",
    parent = SchemaSpec,
    abstract = TRUE,
    properties = list(
        keys = schema_utils__prop_character(any.missing = FALSE, min.len = 1L, unique = TRUE),
        target = S7::new_property(SchemaNode)
    )
)

SchemaBindingExactCmpt <- S7::new_class(
    "SchemaBindingExactCmpt",
    parent = SchemaBindingExact
)

SchemaBindingPattern <- S7::new_class(
    "SchemaBindingPattern",
    parent = SchemaSpec,
    abstract = TRUE,
    properties = list(
        pattern = schema_utils__prop_string(null.ok = FALSE, min.chars = 1L),
        target = S7::new_property(SchemaNode)
    ),
    validator = function(self) {
        ok <- tryCatch({
            grepl(self@pattern, "")
            TRUE
        }, error = function(e) FALSE)
        if (!ok) {
            return(sprintf("`pattern` must be a valid regular expression: %s", self@pattern))
        }
    }
)

SchemaBindingPatternCmpt <- S7::new_class(
    "SchemaBindingPatternCmpt",
    parent = SchemaBindingPattern
)

SchemaNodeCheck <- S7::new_class(
    "SchemaNodeCheck",
    parent = SchemaNode,
    properties = list(
        value = S7::new_property(SchemaRuleCheck),
        name = S7::new_property(
            NULL | SchemaRuleNames,
            default = NULL
        )
    ),
    abstract = TRUE
)

SchemaNodeLeaf <- S7::new_class(
    "SchemaNodeLeaf",
    parent = SchemaNodeCheck,
    validator = function(self) {
        if (schema_spec__kind_is_container(self@value@kind)) {
            return(sprintf(
                "`SchemaNodeLeaf` does not allow container kind `%s`; container kinds are: %s.",
                self@value@kind,
                paste0("'", SCHEMA_SPEC_KINDS_CONTAINER, "'", collapse = ", ")
            ))
        }
    }
)

SchemaNodeContainer <- S7::new_class(
    "SchemaNodeContainer",
    parent = SchemaNodeCheck,
    abstract = TRUE
)

SchemaNodeContainerCmpt <- S7::new_class(
    "SchemaNodeContainerCmpt",
    parent = SchemaNodeContainer,
    properties = list(
        exact = schema_utils__prop_list("SchemaBindingExactCmpt", names = "unnamed", default = list()),
        patterns = schema_utils__prop_list("SchemaBindingPatternCmpt", names = "unnamed", default = list()),
        positions = schema_utils__prop_list("SchemaNode", names = "unnamed", default = list()),
        rest = S7::new_property(NULL | SchemaNode, default = NULL)
    ),
    validator = function(self) {
        if (!schema_spec__kind_is_container(self@value@kind)) {
            return(sprintf(
                "@value requires a container kind; got `%s`. Allowed container kinds are: %s.",
                self@value@kind,
                paste0("'", SCHEMA_SPEC_KINDS_CONTAINER, "'", collapse = ", ")
            ))
        }

        if (length(self@exact)) {
            keys <- unlist(lapply(self@exact, function(x) x@keys), use.names = FALSE)
            msg <- schema_utils__checkmate_result(checkmate::check_character(keys, unique = TRUE), label = "@exact")
            if (!is.null(msg)) {
                return(sprintf("%s ('%s')", msg, keys[duplicated(keys)][[1L]]))
            }
        }

        if (length(self@positions) && !identical(schema_spec__name_type(self@name), "unnamed")) {
            return("`positions` requires `keys$type = 'unnamed'`.")
        }

        if (identical(schema_spec__name_type(self@name), "unnamed") && (length(self@exact) || length(self@patterns))) {
            return("`keys$type = 'unnamed'` only allows `positions` and `rest` constraints.")
        }
    }
)

SchemaNodeRef <- S7::new_class(
    "SchemaNodeRef",
    parent = SchemaNode,
    properties = list(
        ref = schema_utils__prop_ref(null.ok = FALSE)
    )
)

SchemaNodeNary <- S7::new_class(
    "SchemaNodeNary",
    parent = SchemaNode,
    abstract = TRUE
)

SchemaNodeNaryCmpt <- S7::new_class(
    "SchemaNodeNaryCmpt",
    parent = SchemaNodeNary,
    properties = list(
        branches = schema_utils__prop_list("SchemaNode", names = "unnamed", min.len = 1L, default = list())
    ),
    abstract = TRUE
)

SchemaNodeAllCmpt <- S7::new_class(
    "SchemaNodeAllCmpt",
    parent = SchemaNodeNaryCmpt
)

SchemaNodeAnyCmpt <- S7::new_class(
    "SchemaNodeAnyCmpt",
    parent = SchemaNodeNaryCmpt
)

SchemaNodeOneCmpt <- S7::new_class(
    "SchemaNodeOneCmpt",
    parent = SchemaNodeNaryCmpt
)

SchemaNodeNot <- S7::new_class(
    "SchemaNodeNot",
    parent = SchemaNode,
    abstract = TRUE
)

SchemaNodeNotCmpt <- S7::new_class(
    "SchemaNodeNotCmpt",
    parent = SchemaNodeNot,
    properties = list(
        branch = S7::new_property(SchemaNode)
    )
)
# }}}

# schame_spec_node {{{
schema_spec__error <- function(path, message) {
    stop(sprintf("Path at '%s' is invalid:\n- %s", path, message), call. = FALSE)
}

schema_spec__assert <- function(path, what, check) {
    if (!isTRUE(check)) {
        schema_spec__error(path, sprintf("%s: %s", what, check))
    }
}

schema_spec__assert_list <- function(path, what, x, names = "unique", ...) {
    schema_spec__assert(path, what, checkmate::check_list(x, names = names, ...))
}

schema_spec__assert_names <- function(path, what, x, ...) {
    schema_spec__assert(path, what, checkmate::check_names(names(x), ...))
}

schema_spec__rule <- function(x, path) {
    schema_spec__assert_list(path, "'check' rule", x)
    schema_spec__assert_names(path, "'check' rule", x, must.include = "kind")

    args <- if (any(names(x) != "kind")) {
        unclass(x[names(x) != "kind"])
    } else {
        list()
    }

    SchemaRuleCheck(
        kind = x$kind,
        args = args
    )
}

schema_spec__name_rule <- function(x, path) {
    schema_spec__assert_list(path, "'keys' rule", x)
    SchemaRuleNames(args = unclass(x))
}

schema_spec__node_check <- function(x, path, defs, root = FALSE) {
    schema_spec__assert_list(path, "'check'", x)
    schema_spec__assert_names(
        path,
        "'check'",
        x,
        must.include = "check",
        subset.of = c("check", "keys", "fields", "groups", "patterns", "positions", "rest", "description")
    )

    parts <- list(desc = x$description)
    parts$value <- schema_spec__rule(x$check, paste0(path, "$check"))
    if (!is.null(x$keys)) {
        parts$name <- schema_spec__name_rule(x$keys, paste0(path, "$keys"))
    }

    if (!schema_spec__kind_is_container(parts$value@kind)) {
        if (!is.null(x$fields)) {
            schema_spec__error(path, "'fields' is only allowed on container check nodes.")
        }
        if (!is.null(x$groups)) {
            schema_spec__error(path, "'groups' is only allowed on container 'check' nodes.")
        }
        if (!is.null(x$patterns)) {
            schema_spec__error(path, "'patterns' is only allowed on container 'check' nodes.")
        }
        if (!is.null(x$positions)) {
            schema_spec__error(path, "'positions' is only allowed on container 'check' nodes.")
        }
        if (!is.null(x$rest)) {
            schema_spec__error(path, "'rest' is only allowed on container 'check' nodes.")
        }

        return(do.call(SchemaNodeLeaf, parts))
    }

    parts$exact <- c(
        schema_spec__binding_fields(x$fields, paste0(path, "$fields"), defs),
        schema_spec__binding_groups(x$groups, paste0(path, "$groups"), defs)
    )
    parts$patterns <- schema_spec__binding_patterns(x$patterns, paste0(path, "$patterns"), defs)
    parts$positions <- schema_spec__positions(x$positions, paste0(path, "$positions"), defs)

    if (!is.null(x$rest)) {
        parts$rest <- schema_spec__node(x$rest, paste0(path, "$rest"), defs)
    }

    do.call(SchemaNodeContainerCmpt, parts)
}

schema_spec__has_operator <- function(x) {
    any(names(x) %in% SCHEMA_SPEC_OPERATORS)
}

schema_spec__operator <- function(x, path) {
    op <- names(x)
    op <- op[op %in% SCHEMA_SPEC_OPERATORS]

    if (length(op) == 0L) {
        schema_spec__error(
            path,
            sprintf(
                "primary operator: Must be element of set {%s}, but is missing.",
                paste0("'", SCHEMA_SPEC_OPERATORS, "'", collapse = ", ")
            )
        )
    } else if (length(op) > 1L) {
        schema_spec__error(
            path,
            sprintf(
                "primary operator: Must be element of set {%s}, but multiple found: {%s}.",
                paste0("'", SCHEMA_SPEC_OPERATORS, "'", collapse = ", "),
                paste0("'", op, "'", collapse = ", ")
            )
        )
    }

    op
}

schema_spec__binding_fields <- function(fields, path, defs) {
    if (is.null(fields)) {
        return(NULL)
    }

    schema_spec__assert_list(path, "'fields'", fields, types = "list")

    lapply(names(fields), function(name) {
        SchemaBindingExactCmpt(
            keys = name,
            target = schema_spec__node(
                x = fields[[name]],
                path = paste0(path, "$", name),
                defs = defs,
                root = FALSE
            )
        )
    })
}

schema_spec__binding_groups <- function(groups, path, defs) {
    if (is.null(groups)) {
        return(NULL)
    }

    schema_spec__assert_list(path, "'groups'", groups, types = "list", names = "unnamed")

    lapply(seq_along(groups), function(i) {
        group <- groups[[i]]
        loc <- paste0(path, "[", i, "]")

        schema_spec__assert_list(loc, "group item", group)

        schema_spec__assert_names(
            loc,
            "group item",
            group,
            type = "unique",
            must.include = "names",
            subset.of = c("names", setdiff(SCHEMA_SPEC_KEYWORDS, c("version", "$defs")))
        )

        target <- group[names(group) != "names"]
        SchemaBindingExactCmpt(
            keys = group$names,
            target = schema_spec__node(
                x = target,
                path = loc,
                defs = defs,
                root = FALSE
            )
        )
    })
}

schema_spec__binding_patterns <- function(patterns, path, defs) {
    if (is.null(patterns)) {
        return(NULL)
    }

    schema_spec__assert_list(path, "'patterns'", patterns, types = "list", names = "unique")

    lapply(names(patterns), function(pattern) {
        SchemaBindingPatternCmpt(
            pattern = pattern,
            target = schema_spec__node(
                x = patterns[[pattern]],
                path = paste0(path, "$", pattern),
                defs = defs,
                root = FALSE
            )
        )
    })
}

schema_spec__positions <- function(positions, path, defs) {
    if (is.null(positions)) {
        return(NULL)
    }

    schema_spec__assert_list(path, "'positions'", positions, types = "list", names = "unnamed")

    lapply(seq_along(positions), function(i) {
        schema_spec__node(
            x = positions[[i]],
            path = paste0(path, "[", i, "]"),
            defs = defs,
            root = FALSE
        )
    })
}

schema_spec__node_ref <- function(x, path, defs, root = FALSE) {
    schema_spec__assert_list(path, "$`$ref`", x, types = "character")
    schema_spec__assert_names(path, "$`$ref`", x, must.include = "$ref", subset.of = c("$ref", "description"))

    checkmate::assert_character(defs, any.missing = FALSE, min.chars = 1L, names = "unnamed", null.ok = TRUE)
    if (!is.null(defs)) {
        def_name <- sub("^#/\\$defs/", "", x$`$ref`)
        schema_spec__assert(
            path,
            "'$ref' target",
            checkmate::check_choice(def_name, defs)
        )
    }

    SchemaNodeRef(ref = x$`$ref`, desc = x$description)
}

schema_spec__node_branch <- function(x, path, defs, operator) {
    schema_spec__assert_list(path, "branch node", x)

    if (schema_spec__has_operator(x)) {
        schema_spec__node(x, path = path, defs = defs, root = FALSE)
    } else if ("kind" %in% names(x)) {
        if (any(SCHEMA_SPEC_KEYWORDS %in% names(x))) {
            schema_spec__error(
                path,
                sprintf(
                    "- @branches in shorthand check node format must not contain reserved node-level keys or any primary operator, but {%s} %s found.",
                    paste0("'", SCHEMA_SPEC_KEYWORDS[SCHEMA_SPEC_KEYWORDS %in% names(x)], "'", collapse = ","),
                    if (sum(SCHEMA_SPEC_KEYWORDS %in% names(x)) > 1L) "were" else "was"
                )
            )
        }

        schema_spec__node_check(list(check = x), path = path, defs = defs, root = FALSE)
    } else {
        schema_spec__error(
            path,
            "branch node must be a valid schema node with a primary operator or a shorthand 'check' rule"
        )
    }
}

schema_spec__node_nary <- function(x, path, defs, operator, constructor, root = FALSE) {
    schema_spec__assert_list(path, sprintf("'%s' node", operator), x, types = c("list", "character"))
    schema_spec__assert_names(
        path,
        sprintf("'%s' node", operator),
        x,
        type = "unique",
        must.include = operator,
        subset.of = c("description", operator)
    )
    schema_spec__assert_names(
        sprintf("%s$%s", path, operator),
        sprintf("'%s' node", operator),
        x[[operator]],
        type = "unnamed"
    )

    do.call(
        constructor,
        list(
            branches = lapply(seq_along(x[[operator]]), function(i) {
                schema_spec__node_branch(
                    x[[operator]][[i]],
                    path = sprintf("%s$%s[%d]", path, operator, i),
                    defs = defs,
                    operator = operator
                )
            }),
            desc = x$description
        )
    )
}

schema_spec__node_not <- function(x, path, defs, root = FALSE) {
    schema_spec__assert_list(path, "'not' node", x, types = c("list", "character"))
    schema_spec__assert_names(
        path,
        sprintf("'%s' node", "not"),
        x,
        type = "unique",
        must.include = "not",
        subset.of = c("description", "not")
    )

    do.call(
        SchemaNodeNotCmpt,
        list(
            branch = schema_spec__node_branch(
                x$not,
                paste0(path, "$not"),
                defs = defs,
                operator = "not"
            ),
            desc = x$description
        )
    )
}

schema_spec__node <- function(x, path = "$", defs = character(), root = FALSE) {
    if (S7::S7_inherits(x, SchemaNode)) {
        return(x)
    }

    schema_spec__assert_list(path, "schema node", x)
    schema_spec__assert_names(path, "schema node", x)
    if (!root && !is.null(x$`$defs`)) {
        schema_spec__error(path, "`$defs` is only allowed at the root schema document.")
    }

    switch(
        schema_spec__operator(x, path),
        check = schema_spec__node_check(x, path, defs = defs, root = root),
        `$ref` = schema_spec__node_ref(x, path, defs = defs, root = root),
        all = schema_spec__node_nary(
            x,
            path,
            defs = defs,
            operator = "all",
            constructor = SchemaNodeAllCmpt,
            root = root
        ),
        any = schema_spec__node_nary(
            x,
            path,
            defs = defs,
            operator = "any",
            constructor = SchemaNodeAnyCmpt,
            root = root
        ),
        one = schema_spec__node_nary(
            x,
            path,
            defs = defs,
            operator = "one",
            constructor = SchemaNodeOneCmpt,
            root = root
        ),
        not = schema_spec__node_not(x, path, defs = defs, root = root),
        schema_spec__error(path, "unsupported primary operator.")
    )
}
# }}}

# as.list.SchemaSpec {{{
S7::method(as.list, SchemaBindingExact) <- function(x, ...) {
    out <- list()
    if (length(x@keys) == 1L) {
        out$fields <- list()
        out$fields[[x@keys]] <- as.list(x@target)
    } else {
        out$groups <- c(list(names = x@keys), as.list(x@target))
    }
    out
}

S7::method(as.list, SchemaBindingPattern) <- function(x, ...) {
    out <- list(patterns = list())
    out$patterns[[x@pattern]] <- as.list(x@target)
    out
}

S7::method(as.list, SchemaRuleCheck) <- function(x, ...) {
    schema_utils__as_list_rule(x)
}

S7::method(as.list, SchemaRuleNames) <- function(x, ...) {
    schema_utils__as_list_rule_names(x)
}

S7::method(as.list, SchemaNodeLeaf) <- function(x, ...) {
    out <- list(check = as.list(x@value))
    keys <- schema_utils__keys_as_list(x@name)
    if (!is.null(keys)) {
        out$keys <- keys
    }
    schema_utils__as_list_add_desc(out, x)
}

S7::method(as.list, SchemaNodeContainerCmpt) <- function(x, ...) {
    out <- list(check = as.list(x@value))
    keys <- schema_utils__keys_as_list(x@name)
    if (!is.null(keys)) {
        out$keys <- keys
    }

    bindings <- lapply(x@exact, as.list)
    fields <- unlist(lapply(bindings, "[[", "fields"), recursive = FALSE)
    groups <- lapply(bindings, "[[", "groups")
    groups <- groups[!vapply(groups, is.null, logical(1L))]
    if (length(fields)) {
        out$fields <- fields
    }
    if (length(groups)) {
        out$groups <- groups
    }
    patterns <- unlist(lapply(lapply(x@patterns, as.list), "[[", "patterns"), recursive = FALSE)
    if (length(patterns)) {
        out$patterns <- patterns
    }
    if (length(x@positions)) {
        out$positions <- lapply(x@positions, as.list)
    }
    if (!is.null(x@rest)) {
        out$rest <- as.list(x@rest)
    }

    schema_utils__as_list_add_desc(out, x)
}

S7::method(as.list, SchemaNodeRef) <- function(x, ...) {
    out <- list()
    out[["$ref"]] <- x@ref
    schema_utils__as_list_add_desc(out, x)
}

S7::method(as.list, SchemaNodeAllCmpt) <- function(x, ...) {
    schema_utils__as_list_nary(x, "all")
}

S7::method(as.list, SchemaNodeAnyCmpt) <- function(x, ...) {
    schema_utils__as_list_nary(x, "any")
}

S7::method(as.list, SchemaNodeOneCmpt) <- function(x, ...) {
    schema_utils__as_list_nary(x, "one")
}

S7::method(as.list, SchemaNodeNotCmpt) <- function(x, ...) {
    out <- list(not = as.list(x@branch))
    schema_utils__as_list_add_desc(out, x)
}
# }}}

# schema_utils__as_json.SchemaSpec {{{
S7::method(schema_utils__as_json, SchemaSpec) <- function(x, pretty = TRUE, auto_unbox = TRUE) {
    schema_utils__as_json_impl(x, pretty = pretty, auto_unbox = auto_unbox)
}
# }}}

S7::method(print, SchemaSpec) <- function(x, ..., pretty = TRUE, auto_unbox = TRUE) {
    schema_utils__cat_json(x, ..., pretty = pretty, auto_unbox = auto_unbox)
}


# -------------------- schema-doc.R

# SchemaDoc {{{
SchemaDoc <- S7::new_class(
    "SchemaDoc",
    parent = SchemaSpec,
    properties = list(
        version = schema_utils__prop_string(null.ok = TRUE),
        path = schema_utils__prop_string(),
        root = S7::new_property(SchemaNode),
        defs = schema_utils__prop_list("SchemaNode")
    )
)
# }}}

# schema_doc {{{
schema_doc__defs <- function(x, path = "$`$defs`") {
    if (!length(x)) {
        return(stats::setNames(list(), character()))
    }

    schema_spec__assert_list(path, "'$defs'", x, types = "list", names = "unique")
    nms <- names(x)
    if (any(grepl("/", nms, fixed = TRUE))) {
        schema_spec__error(path, "'$defs': Names must not contain '/'.")
    }

    stats::setNames(
        lapply(nms, function(name) {
            schema_spec__node(
                x[[name]],
                path = sprintf("%s[['%s']]", path, name),
                defs = nms,
                root = FALSE
            )
        }),
        nms
    )
}

#' Parse schema documents
#'
#' `schema_doc()` parses a schema DSL list into a schemate schema document
#' object.
#'
#' Normal users usually create schema documents with `schema_infer()`,
#' `schema_read()`, or the edit helpers. Use `schema_doc()` when you are
#' hand-authoring a schema as an R list.
#'
#' @param x A schema DSL list or an existing schemate schema document.
#' @param path Optional source path stored as runtime metadata.
#'
#' @return A schemate schema document object.
#'
#' @examples
#' doc <- schema_doc(list(check = list(kind = "string", min.chars = 1)))
#' doc
#'
#' schema_validate(doc, "ok")
#' schema_validate(doc, 1L, mode = "check")
#'
#' @noRd
schema_doc <- function(x, path = NULL) {
    if (S7::S7_inherits(x, SchemaDoc)) {
        return(x)
    }

    schema_spec__assert_list("$", "schema document", x, names = "unique")
    schema_spec__assert_names("$", "schema document", x, subset.of = SCHEMA_SPEC_KEYWORDS)

    defs <- schema_doc__defs(x$`$defs`)

    root <- x[!names(x) %in% c("$defs", "version")]
    if (!length(root)) {
        schema_spec__error("$", "'root': Schema document must contain a root schema node.")
    }

    SchemaDoc(
        path = path,
        version = x$version,
        root = schema_spec__node(root, path = "$", defs = names(defs), root = TRUE),
        defs = defs
    )
}
# }}}

# as.list.SchemaDoc {{{
S7::method(as.list, SchemaDoc) <- function(x, ...) {
    # Top-level serialization contract for SchemaDoc:
    # 1. `version` first when present
    # 2. root `description` next when present
    # 3. `$defs` next when present
    # 4. serialized root operator-specific entries last
    # `path` is runtime metadata and is intentionally excluded.
    out <- list()
    root <- as.list(x@root)

    if (!is.null(x@version)) {
        out$version <- x@version
    }

    if ("description" %in% names(root)) {
        out$description <- root$description
        root$description <- NULL
    }

    if (length(x@defs)) {
        defs <- stats::setNames(lapply(x@defs, as.list), names(x@defs))
        out$`$defs` <- defs
    }

    out <- c(out, root)

    out
}
# }}}


# -------------------- schema-compact.R

schema_compact__normalize_checkmate_args <- function(fun, args) {
    if (!length(args)) {
        return(list())
    }

    defaults <- as.list(formals(fun))
    matched <- match.call(
        definition = fun,
        call = as.call(c(list(quote(check), quote(.x)), args)),
        expand.dots = TRUE
    )
    matched <- as.list(matched)[-1L]
    matched$x <- NULL

    out <- list()
    for (name in names(matched)) {
        if (name %in% names(defaults) && identical(matched[[name]], defaults[[name]])) {
            next
        }
        out[[name]] <- matched[[name]]
    }

    out
}

schema_compact__normalize_check_args <- function(kind, args) {
    fun <- tryCatch(
        schema_utils__checkmate_fun(kind),
        error = function(e) NULL
    )
    if (is.null(fun)) {
        return(args)
    }

    schema_compact__normalize_checkmate_args(fun, args)
}

schema_compact__normalize_check_rule <- function(x) {
    if (!is.list(x) || is.null(x$kind)) {
        return(x)
    }

    c(
        list(kind = x$kind),
        schema_compact__normalize_check_args(
            kind = x$kind,
            args = x[names(x) != "kind"]
        )
    )
}

schema_compact__normalize_names_args <- function(args) {
    schema_compact__normalize_checkmate_args(checkmate::check_names, args)
}

schema_compact__normalize_names_rule <- function(x) {
    if (!is.list(x)) {
        return(x)
    }

    schema_compact__normalize_names_args(x)
}

schema_compact__normalize_schema_list <- function(x) {
    if (!is.list(x)) {
        return(x)
    }

    out <- lapply(x, schema_compact__normalize_schema_list)
    if (!is.null(out$check)) {
        out$check <- schema_compact__normalize_check_rule(out$check)
    }
    if (!is.null(out$keys)) {
        out$keys <- schema_compact__normalize_names_rule(out$keys)
    }
    out
}

schema_compact__dedupe_equivalent_nodes <- function(x) {
    if (!length(x)) {
        return(x)
    }

    x[!duplicated(lapply(x, function(node) schema_compact__normalize_schema_list(as.list(node))))]
}

schema_compact__same <- S7::new_generic(
    "schema_compact__same",
    c("x", "y"),
    function(x, y) S7::S7_dispatch()
)

schema_compact__same_list <- function(x, y) {
    if (length(x) != length(y)) {
        return(FALSE)
    }

    if (!length(x)) {
        return(TRUE)
    }

    all(vapply(
        seq_along(x),
        function(i) schema_compact__same(x[[i]], y[[i]]),
        logical(1L)
    ))
}

schema_compact__same_optional <- function(x, y) {
    if (is.null(x) || is.null(y)) {
        return(is.null(x) && is.null(y))
    }

    schema_compact__same(x, y)
}

schema_compact__same_leaf <- function(x, y) {
    identical(x@desc, y@desc) &&
        schema_compact__same(x@value, y@value) &&
        schema_compact__same_optional(x@name, y@name)
}

schema_compact__same_ref <- function(x, y) {
    identical(x@desc, y@desc) &&
        identical(x@ref, y@ref)
}

schema_compact__same_container <- function(x, y) {
    identical(x@desc, y@desc) &&
        schema_compact__same(x@value, y@value) &&
        schema_compact__same_optional(x@name, y@name) &&
        schema_compact__same_list(x@exact, y@exact) &&
        schema_compact__same_list(x@patterns, y@patterns) &&
        schema_compact__same_list(x@positions, y@positions) &&
        schema_compact__same_optional(x@rest, y@rest)
}

schema_compact__same_nary <- function(x, y) {
    identical(x@desc, y@desc) &&
        schema_compact__same_list(x@branches, y@branches)
}

schema_compact__same_not <- function(x, y) {
    identical(x@desc, y@desc) &&
        schema_compact__same(x@branch, y@branch)
}

S7::method(schema_compact__same, list(SchemaSpec, SchemaSpec)) <- function(x, y) FALSE
S7::method(schema_compact__same, list(SchemaRuleCheck, SchemaRuleCheck)) <- function(x, y) {
    if (!identical(x@kind, y@kind)) {
        return(FALSE)
    }
    if (identical(x@args, y@args)) {
        return(TRUE)
    }

    identical(
        schema_compact__normalize_check_args(x@kind, x@args),
        schema_compact__normalize_check_args(y@kind, y@args)
    )
}
S7::method(schema_compact__same, list(SchemaRuleNames, SchemaRuleNames)) <- function(x, y) {
    if (identical(x@args, y@args)) {
        return(TRUE)
    }

    identical(
        schema_compact__normalize_names_args(x@args),
        schema_compact__normalize_names_args(y@args)
    )
}
S7::method(schema_compact__same, list(SchemaBindingExactCmpt, SchemaBindingExactCmpt)) <- function(x, y) {
    identical(x@keys, y@keys) &&
        schema_compact__same(x@target, y@target)
}
S7::method(schema_compact__same, list(SchemaBindingPatternCmpt, SchemaBindingPatternCmpt)) <- function(x, y) {
    identical(x@pattern, y@pattern) &&
        schema_compact__same(x@target, y@target)
}
S7::method(schema_compact__same, list(SchemaNodeLeaf, SchemaNodeLeaf)) <- schema_compact__same_leaf
S7::method(schema_compact__same, list(SchemaNodeRef, SchemaNodeRef)) <- schema_compact__same_ref
S7::method(schema_compact__same, list(SchemaNodeContainerCmpt, SchemaNodeContainerCmpt)) <-
    schema_compact__same_container
S7::method(schema_compact__same, list(SchemaNodeAllCmpt, SchemaNodeAllCmpt)) <- schema_compact__same_nary
S7::method(schema_compact__same, list(SchemaNodeAnyCmpt, SchemaNodeAnyCmpt)) <- schema_compact__same_nary
S7::method(schema_compact__same, list(SchemaNodeOneCmpt, SchemaNodeOneCmpt)) <- schema_compact__same_nary
S7::method(schema_compact__same, list(SchemaNodeNotCmpt, SchemaNodeNotCmpt)) <- schema_compact__same_not

schema_compact__find_exact_group <- function(grouped, target) {
    for (i in seq_along(grouped)) {
        if (schema_compact__same(grouped[[i]]$target, target)) {
            return(i)
        }
    }

    NA_integer_
}

schema_compact__materialize_exact_groups <- function(grouped) {
    unname(lapply(grouped, function(group) {
        SchemaBindingExactCmpt(
            keys = unique(unlist(group$keys, use.names = FALSE)),
            target = group$target
        )
    }))
}

schema_compact__group_exact_bindings <- function(exact) {
    grouped <- list()
    for (binding in exact) {
        index <- schema_compact__find_exact_group(grouped, binding@target)
        if (is.na(index)) {
            grouped[[length(grouped) + 1L]] <- list(keys = list(), target = binding@target)
            index <- length(grouped)
        }
        grouped[[index]]$keys[[length(grouped[[index]]$keys) + 1L]] <- binding@keys
    }

    schema_compact__materialize_exact_groups(grouped)
}

schema_compact__compact_exact_bindings <- function(exact, arrays, groups, name) {
    if (!length(exact)) {
        return(list())
    }

    should_group <- groups && !identical(schema_compact__name_type(name), "unnamed")
    if (!should_group) {
        out <- vector("list", length(exact))
        for (i in seq_along(exact)) {
            binding <- exact[[i]]
            out[[i]] <- SchemaBindingExactCmpt(
                keys = binding@keys,
                target = schema_compact__node(binding@target, arrays = arrays, groups = groups)
            )
        }
        return(out)
    }

    grouped <- list()
    for (binding in exact) {
        target <- schema_compact__node(binding@target, arrays = arrays, groups = groups)
        index <- schema_compact__find_exact_group(grouped, target)
        if (is.na(index)) {
            grouped[[length(grouped) + 1L]] <- list(keys = list(), target = target)
            index <- length(grouped)
        }
        grouped[[index]]$keys[[length(grouped[[index]]$keys) + 1L]] <- binding@keys
    }

    schema_compact__materialize_exact_groups(grouped)
}

schema_compact__name_type <- function(x) {
    if (is.null(x)) {
        return(NA_character_)
    }

    schema_utils__coalesce(x@args$type, NA_character_)
}

schema_compact__name_required <- function(x) {
    if (is.null(x)) {
        return(character())
    }

    if (!is.null(x@args$identical.to)) {
        return(x@args$identical.to)
    }

    schema_utils__coalesce(x@args$must.include, character())
}

schema_compact__names_mergeable <- function(x) {
    types <- vapply(x, schema_compact__name_type, character(1L), USE.NAMES = FALSE)
    types <- types[!is.na(types)]

    if (!length(types)) {
        return(TRUE)
    }

    all(types == types[[1L]])
}

schema_compact__merge_name <- function(x) {
    x <- Filter(Negate(is.null), x)
    if (!length(x)) {
        return(NULL)
    }

    types <- vapply(x, schema_compact__name_type, character(1L), USE.NAMES = FALSE)
    types <- types[!is.na(types)]
    if (length(types) && all(types == types[[1L]]) && identical(types[[1L]], "unnamed")) {
        return(SchemaRuleNames(args = list(type = "unnamed")))
    }

    exact <- lapply(x, function(rule) rule@args$identical.to)
    if (all(!vapply(exact, is.null, logical(1L))) && all(vapply(exact[-1L], identical, logical(1L), exact[[1L]]))) {
        return(SchemaRuleNames(args = list(identical.to = exact[[1L]])))
    }

    required <- lapply(x, schema_compact__name_required)
    must_include <- Reduce(intersect, required)
    args <- list(type = "named")
    if (length(must_include)) {
        args$must.include <- must_include
    }
    SchemaRuleNames(args = args)
}

schema_compact__binding_field_map <- function(exact) {
    out <- list()
    for (binding in exact) {
        for (key in binding@keys) {
            out[[key]] <- binding@target
        }
    }

    out
}

schema_compact__merge_node_options <- function(nodes, arrays, groups) {
    nodes <- schema_compact__dedupe_equivalent_nodes(nodes)
    if (!length(nodes)) {
        return(NULL)
    }
    if (length(nodes) == 1L) {
        return(nodes[[1L]])
    }

    schema_compact__compact_any(nodes, desc = NULL, arrays = arrays, groups = groups)
}

schema_compact__merge_field_maps <- function(maps, arrays, groups) {
    keys <- unique(unlist(lapply(maps, names), use.names = FALSE))
    if (!length(keys)) {
        return(list())
    }

    unname(lapply(keys, function(key) {
        targets <- Filter(
            Negate(is.null),
            lapply(maps, function(map) map[[key]])
        )
        SchemaBindingExactCmpt(
            keys = key,
            target = schema_compact__merge_node_options(targets, arrays = arrays, groups = groups)
        )
    }))
}

schema_compact__merge_rest <- function(x, arrays, groups) {
    rest <- Filter(Negate(is.null), lapply(x, function(node) node@rest))
    if (!length(rest)) {
        return(NULL)
    }

    schema_compact__merge_node_options(rest, arrays = arrays, groups = groups)
}

schema_compact__containers_can_merge <- function(x, y) {
    schema_compact__same(x@value, y@value) &&
        schema_compact__names_mergeable(list(x@name, y@name)) &&
        schema_compact__same_list(x@patterns, y@patterns) &&
        schema_compact__same_list(x@positions, y@positions)
}

schema_compact__merge_containers <- function(x, arrays, groups) {
    first <- x[[1L]]
    exact <- schema_compact__merge_field_maps(
        lapply(x, function(node) schema_compact__binding_field_map(node@exact)),
        arrays = arrays,
        groups = groups
    )

    node <- SchemaNodeContainerCmpt(
        value = first@value,
        name = schema_compact__merge_name(lapply(x, function(node) node@name)),
        exact = exact,
        patterns = first@patterns,
        positions = first@positions,
        rest = schema_compact__merge_rest(x, arrays = arrays, groups = groups),
        # Descriptions are not part of merge compatibility; keep the first one.
        desc = first@desc
    )

    schema_compact__compact_container_groups(node, groups = groups)
}

schema_compact__merge_compatible_containers <- function(branches, arrays, groups) {
    used <- rep(FALSE, length(branches))
    out <- list()

    for (i in seq_along(branches)) {
        if (used[[i]]) {
            next
        }

        branch <- branches[[i]]
        if (!S7::S7_inherits(branch, SchemaNodeContainerCmpt)) {
            out[[length(out) + 1L]] <- branch
            used[[i]] <- TRUE
            next
        }

        idx <- i
        if (i < length(branches)) {
            for (j in seq.int(i + 1L, length(branches))) {
                if (
                    !used[[j]] &&
                        S7::S7_inherits(branches[[j]], SchemaNodeContainerCmpt) &&
                        schema_compact__containers_can_merge(branch, branches[[j]])
                ) {
                    idx <- c(idx, j)
                }
            }
        }

        used[idx] <- TRUE
        out[[length(out) + 1L]] <- if (length(idx) == 1L) {
            branch
        } else {
            schema_compact__merge_containers(branches[idx], arrays = arrays, groups = groups)
        }
    }

    out
}

schema_compact__compact_any <- function(branches, desc, arrays, groups) {
    branches <- schema_compact__dedupe_equivalent_nodes(branches)
    if (arrays) {
        branches <- schema_compact__merge_compatible_containers(branches, arrays = arrays, groups = groups)
        branches <- schema_compact__dedupe_equivalent_nodes(branches)
    }

    if (length(branches) == 1L && is.null(desc)) {
        return(branches[[1L]])
    }

    SchemaNodeAnyCmpt(branches = branches, desc = desc)
}

schema_compact__compact_container_groups <- function(node, groups) {
    if (!groups || !length(node@exact) || identical(schema_compact__name_type(node@name), "unnamed")) {
        return(node)
    }

    S7::set_props(node, exact = schema_compact__group_exact_bindings(node@exact))
}

schema_compact__node <- S7::new_generic(
    "schema_compact__node",
    "node",
    function(node, arrays, groups) S7::S7_dispatch()
)

S7::method(schema_compact__node, SchemaNodeLeaf) <- function(node, arrays, groups) {
    node
}

S7::method(schema_compact__node, SchemaNodeRef) <- function(node, arrays, groups) {
    node
}

S7::method(schema_compact__node, SchemaNodeContainerCmpt) <- function(node, arrays, groups) {
    exact <- schema_compact__compact_exact_bindings(
        node@exact,
        arrays = arrays,
        groups = groups,
        name = node@name
    )
    patterns <- lapply(node@patterns, function(binding) {
        SchemaBindingPatternCmpt(
            pattern = binding@pattern,
            target = schema_compact__node(binding@target, arrays = arrays, groups = groups)
        )
    })
    positions <- lapply(node@positions, schema_compact__node, arrays = arrays, groups = groups)
    rest <- if (is.null(node@rest)) {
        NULL
    } else {
        schema_compact__node(node@rest, arrays = arrays, groups = groups)
    }

    SchemaNodeContainerCmpt(
        value = node@value,
        name = node@name,
        exact = exact,
        patterns = patterns,
        positions = positions,
        rest = rest,
        desc = node@desc
    )
}

S7::method(schema_compact__node, SchemaNodeAllCmpt) <- function(node, arrays, groups) {
    SchemaNodeAllCmpt(
        branches = lapply(node@branches, schema_compact__node, arrays = arrays, groups = groups),
        desc = node@desc
    )
}

S7::method(schema_compact__node, SchemaNodeAnyCmpt) <- function(node, arrays, groups) {
    branches <- lapply(node@branches, schema_compact__node, arrays = arrays, groups = groups)
    schema_compact__compact_any(branches, desc = node@desc, arrays = arrays, groups = groups)
}

S7::method(schema_compact__node, SchemaNodeOneCmpt) <- function(node, arrays, groups) {
    SchemaNodeOneCmpt(
        branches = lapply(node@branches, schema_compact__node, arrays = arrays, groups = groups),
        desc = node@desc
    )
}

S7::method(schema_compact__node, SchemaNodeNotCmpt) <- function(node, arrays, groups) {
    SchemaNodeNotCmpt(
        branch = schema_compact__node(node@branch, arrays = arrays, groups = groups),
        desc = node@desc
    )
}

S7::method(schema_compact__node, SchemaNode) <- function(node, arrays, groups) {
    stop("Unsupported schema node.", call. = FALSE)
}

S7::method(schema_compact__node, S7::class_any) <- function(node, arrays, groups) {
    stop("Unsupported schema node.", call. = FALSE)
}

#' Compact a schema document
#'
#' `schema_compact()` simplifies schema documents produced by inference or
#' hand-authoring. It can merge observed array element alternatives and group
#' sibling fields that share identical schemas.
#'
#' @param x A `SchemaDoc` or raw schema DSL list.
#' @param arrays Whether to merge compatible `any` branches, especially the
#'   observed element alternatives produced by `schema_infer(arrays = "rest")`.
#' @param groups Whether to combine sibling fields with identical schemas into
#'   `groups`.
#'
#' @return A compacted `SchemaDoc`.
#'
#' @examples
#' schema <- schema_infer(
#'     list(items = list(list(id = 1L, name = "a"), list(id = 2L, label = "b"))),
#'     keys = "named",
#'     arrays = "rest"
#' )
#' schema
#'
#' schema_compact(schema)
#'
#' @noRd
schema_compact <- function(x, arrays = TRUE, groups = TRUE) {
    checkmate::assert_flag(arrays)
    checkmate::assert_flag(groups)

    doc <- schema_doc(x)
    defs <- lapply(doc@defs, schema_compact__node, arrays = arrays, groups = groups)

    SchemaDoc(
        version = doc@version,
        path = doc@path,
        root = schema_compact__node(doc@root, arrays = arrays, groups = groups),
        defs = defs
    )
}


# -------------------- schema-edit.R

SCHEMA_EDIT_RESERVED_TOKENS <- c(
    "fields",
    "defs",
    "all",
    "any",
    "one",
    "not",
    "groups",
    "patterns",
    "positions",
    "rest",
    "check",
    "keys",
    "description",
    "version"
)

schema_edit__is_reserved_token <- function(token) {
    checkmate::test_choice(token, SCHEMA_EDIT_RESERVED_TOKENS)
}

schema_edit__quote_segment <- function(x) {
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    x <- gsub("`", "\\`", x, fixed = TRUE)
    paste0("`", x, "`")
}

schema_edit__path_segment <- function(x) {
    checkmate::assert_string(x, min.chars = 1L)
    if (grepl("^[A-Za-z.][A-Za-z0-9_.]*$", x, useBytes = TRUE)) {
        return(x)
    }

    schema_edit__quote_segment(x)
}

schema_edit__path_append <- function(path, ...) {
    tokens <- list(...)
    if (!length(tokens)) {
        return(path)
    }

    for (token in tokens) {
        segment <- schema_edit__path_segment(token)
        if (identical(path, "$")) {
            path <- paste0("$", segment)
        } else {
            path <- paste0(path, "$", segment)
        }
    }

    path
}

schema_edit__path_append_field <- function(path, name) {
    if (schema_edit__is_reserved_token(name)) {
        return(schema_edit__path_append(path, "fields", name))
    }

    schema_edit__path_append(path, name)
}

schema_edit__path_append_index <- function(path, token, index) {
    paste0(schema_edit__path_append(path, token), "[", index, "]")
}

schema_edit__parse_quoted_segment <- function(path, pos) {
    n <- nchar(path)
    chars <- character()
    pos <- pos + 1L

    while (pos <= n) {
        chr <- substring(path, pos, pos)

        if (identical(chr, "`")) {
            segment <- paste0(chars, collapse = "")
            if (!nzchar(segment)) {
                stop(sprintf("`path` contains an empty quoted segment near `%s`.", substring(path, pos)), call. = FALSE)
            }
            return(list(segment = segment, pos = pos + 1L))
        }

        if (identical(chr, "\\")) {
            if (pos == n) {
                stop(sprintf("Unsupported path escape near `%s`.", substring(path, pos)), call. = FALSE)
            }
            next_chr <- substring(path, pos + 1L, pos + 1L)
            if (!next_chr %in% c("`", "\\")) {
                stop(sprintf("Unsupported path escape near `%s`.", substring(path, pos)), call. = FALSE)
            }
            chars[[length(chars) + 1L]] <- next_chr
            pos <- pos + 2L
            next
        }

        chars[[length(chars) + 1L]] <- chr
        pos <- pos + 1L
    }

    stop(sprintf("Unterminated quoted path segment near `%s`.", substring(path, pos)), call. = FALSE)
}

schema_edit__parse_path <- function(path) {
    checkmate::assert_string(path, min.chars = 1L)
    if (!startsWith(path, "$")) {
        stop("`path` must start with `$`.", call. = FALSE)
    }
    if (identical(path, "$")) {
        return(list())
    }

    tokens <- list()
    pos <- 2L
    n <- nchar(path)

    while (pos <= n) {
        chr <- substring(path, pos, pos)

        if (identical(chr, "$")) {
            if (pos == 2L || pos == n || substring(path, pos + 1L, pos + 1L) %in% c("$", "[")) {
                stop(sprintf("`path` contains an empty segment near `%s`.", substring(path, pos)), call. = FALSE)
            }
            pos <- pos + 1L
            next
        }

        if (identical(chr, "`")) {
            quoted <- schema_edit__parse_quoted_segment(path, pos)
            tokens[[length(tokens) + 1L]] <- quoted$segment
            pos <- quoted$pos
            if (pos <= n && !(substring(path, pos, pos) %in% c("$", "["))) {
                stop(sprintf("Unsupported path segment near `%s`.", substring(path, pos)), call. = FALSE)
            }
            next
        }

        if (identical(chr, "[")) {
            if (!length(tokens)) {
                stop(sprintf("Unsupported path index near `%s`.", substring(path, pos)), call. = FALSE)
            }
            index_text <- substring(path, pos)
            match <- regexec("^\\[([0-9]+)\\]", index_text)
            hit <- regmatches(index_text, match)[[1L]]
            if (!length(hit)) {
                stop(sprintf("Unsupported path index near `%s`.", substring(path, pos)), call. = FALSE)
            }
            tokens[[length(tokens) + 1L]] <- as.integer(hit[[2L]])
            pos <- pos + nchar(hit[[1L]])
            next
        }

        start <- pos
        while (pos <= n && !(substring(path, pos, pos) %in% c("$", "["))) {
            pos <- pos + 1L
        }
        segment <- substring(path, start, pos - 1L)
        if (!nzchar(segment)) {
            stop(sprintf("Unsupported path segment near `%s`.", substring(path, start)), call. = FALSE)
        }
        tokens[[length(tokens) + 1L]] <- segment
    }

    tokens
}

schema_edit__ref_value <- function(x) {
    checkmate::assert_string(x, min.chars = 1L)

    if (grepl("^#/\\$defs/[^/]+$", x)) {
        return(x)
    }

    if (grepl("/", x, fixed = TRUE)) {
        stop("`name` must be a definition name or a local ref of the form `#/$defs/name`.", call. = FALSE)
    }

    paste0("#/$defs/", x)
}

schema_edit__abort_with_context <- function(context, error) {
    message <- conditionMessage(error)
    if (startsWith(message, context)) {
        stop(message, call. = FALSE)
    }
    stop(sprintf("%s\n%s", context, message), call. = FALSE)
}

schema_edit__as_node <- function(x, defs, path, context = sprintf("Invalid schema fragment at path `%s`.", path)) {
    tryCatch(
        {
            if (S7::S7_inherits(x, SchemaNode)) {
                return(x)
            }
            schema_spec__node(x, path = path, defs = defs, root = FALSE)
        },
        error = function(e) {
            schema_edit__abort_with_context(context, e)
        }
    )
}

schema_edit__as_group_binding <- function(
    x,
    defs,
    path,
    context = sprintf("Invalid schema group at path `%s`.", path)
) {
    tryCatch(
        schema_spec__binding_groups(list(x), path = path, defs = defs)[[1L]],
        error = function(e) {
            schema_edit__abort_with_context(context, e)
        }
    )
}

schema_edit__field_binding_index <- function(bindings, name) {
    idx <- which(vapply(
        bindings,
        function(binding) name %in% binding@keys,
        logical(1L)
    ))

    if (!length(idx)) {
        return(NA_integer_)
    }

    idx[[1L]]
}

schema_edit__exact_slice <- function(bindings, from, to) {
    if (from > to) {
        return(list())
    }

    bindings[from:to]
}

schema_edit__exact_binding <- function(keys, target) {
    if (!length(keys)) {
        return(NULL)
    }

    list(SchemaBindingExactCmpt(keys = keys, target = target))
}

schema_edit__replace_field_binding <- function(bindings, index, name, target) {
    binding <- bindings[[index]]
    pos <- match(name, binding@keys)
    if (is.na(pos)) {
        stop(sprintf("Field `%s` does not exist.", name), call. = FALSE)
    }

    remaining <- binding@keys[-pos]
    shared <- schema_edit__exact_binding(remaining, binding@target)
    field <- schema_edit__exact_binding(name, target)

    c(
        schema_edit__exact_slice(bindings, 1L, index - 1L),
        shared,
        field,
        schema_edit__exact_slice(bindings, index + 1L, length(bindings))
    )
}

schema_edit__delete_field_binding <- function(bindings, index, name) {
    binding <- bindings[[index]]
    pos <- match(name, binding@keys)
    if (is.na(pos)) {
        stop(sprintf("Field `%s` does not exist.", name), call. = FALSE)
    }

    remaining <- binding@keys[-pos]
    replacement <- schema_edit__exact_binding(remaining, binding@target)
    c(
        schema_edit__exact_slice(bindings, 1L, index - 1L),
        replacement,
        schema_edit__exact_slice(bindings, index + 1L, length(bindings))
    )
}

schema_edit__node_refs <- function(node) {
    if (S7::S7_inherits(node, SchemaNodeRef)) {
        return(node@ref)
    }
    if (S7::S7_inherits(node, SchemaNodeContainerCmpt)) {
        refs <- unlist(
            lapply(node@exact, function(binding) schema_edit__node_refs(binding@target)),
            use.names = FALSE
        )
        refs <- c(
            refs,
            unlist(lapply(node@patterns, function(binding) schema_edit__node_refs(binding@target)), use.names = FALSE)
        )
        refs <- c(refs, unlist(lapply(node@positions, schema_edit__node_refs), use.names = FALSE))
        if (!is.null(node@rest)) {
            refs <- c(refs, schema_edit__node_refs(node@rest))
        }
        return(refs)
    }
    if (S7::S7_inherits(node, SchemaNodeNaryCmpt)) {
        return(unlist(lapply(node@branches, schema_edit__node_refs), use.names = FALSE))
    }
    if (S7::S7_inherits(node, SchemaNodeNotCmpt)) {
        return(schema_edit__node_refs(node@branch))
    }

    character()
}

schema_edit__update_node <- function(node, ...) {
    S7::set_props(node, ...)
}

schema_edit__path_not_found <- function(path) {
    stop(sprintf("`path` does not exist: %s", path), call. = FALSE)
}

schema_edit__modify_container_child <- function(node, kind, key, tokens, fn, path) {
    checkmate::assert_choice(kind, c("field", "group"))

    if (identical(kind, "field")) {
        if (!is.character(key)) {
            schema_edit__path_not_found(path)
        }

        index <- schema_edit__field_binding_index(node@exact, key)
        if (is.na(index)) {
            schema_edit__path_not_found(path)
        }
    } else {
        if (!is.numeric(key)) {
            schema_edit__path_not_found(path)
        }

        group_index <- which(vapply(node@exact, function(binding) length(binding@keys) > 1L, logical(1L)))
        if (key < 1L || key > length(group_index)) {
            schema_edit__path_not_found(path)
        }
        index <- group_index[[key]]
    }

    exact <- node@exact
    target <- schema_edit__modify_tree(exact[[index]]@target, tokens, fn, path)
    exact <- if (identical(kind, "field")) {
        schema_edit__replace_field_binding(exact, index, key, target)
    } else {
        exact[[index]] <- SchemaBindingExactCmpt(
            keys = exact[[index]]@keys,
            target = target
        )
        exact
    }
    schema_edit__update_node(node, exact = exact)
}

schema_edit__modify_tree <- S7::new_generic(
    "schema_edit__modify_tree",
    "node",
    function(node, tokens, fn, path) {
        if (!length(tokens)) {
            return(fn(node))
        }
        S7::S7_dispatch()
    }
)

S7::method(schema_edit__modify_tree, SchemaNode) <- function(node, tokens, fn, path) {
    schema_edit__path_not_found(path)
}

S7::method(schema_edit__modify_tree, SchemaNodeContainerCmpt) <- function(node, tokens, fn, path) {
    token <- tokens[[1L]]
    rest <- tokens[-1L]

    if (is.character(token) && token %in% c("fields", "groups")) {
        kind <- if (identical(token, "fields")) "field" else "group"
        if (!length(rest) || (identical(kind, "field") && !is.character(rest[[1L]]))) {
            schema_edit__path_not_found(path)
        }
        return(schema_edit__modify_container_child(node, kind, rest[[1L]], rest[-1L], fn, path))
    }

    if (is.character(token) && identical(token, "rest")) {
        if (is.null(node@rest)) {
            schema_edit__path_not_found(path)
        }
        return(schema_edit__update_node(
            node,
            rest = schema_edit__modify_tree(node@rest, rest, fn, path)
        ))
    }

    if (is.character(token) && identical(token, "patterns")) {
        if (!length(rest) || !is.character(rest[[1L]])) {
            schema_edit__path_not_found(path)
        }
        pattern <- rest[[1L]]
        index <- which(vapply(node@patterns, function(binding) identical(binding@pattern, pattern), logical(1L)))
        if (!length(index)) {
            schema_edit__path_not_found(path)
        }

        patterns <- node@patterns
        patterns[[index[[1L]]]] <- SchemaBindingPatternCmpt(
            pattern = patterns[[index[[1L]]]]@pattern,
            target = schema_edit__modify_tree(patterns[[index[[1L]]]]@target, rest[-1L], fn, path)
        )
        return(schema_edit__update_node(node, patterns = patterns))
    }

    if (is.character(token) && identical(token, "positions")) {
        if (!length(rest) || !is.numeric(rest[[1L]])) {
            schema_edit__path_not_found(path)
        }
        index <- rest[[1L]]
        if (index < 1L || index > length(node@positions)) {
            schema_edit__path_not_found(path)
        }

        positions <- node@positions
        positions[[index]] <- schema_edit__modify_tree(positions[[index]], rest[-1L], fn, path)
        return(schema_edit__update_node(node, positions = positions))
    }

    if (is.character(token) && !schema_edit__is_reserved_token(token)) {
        return(schema_edit__modify_container_child(node, "field", token, rest, fn, path))
    }

    schema_edit__path_not_found(path)
}

S7::method(schema_edit__modify_tree, SchemaNodeNaryCmpt) <- function(node, tokens, fn, path) {
    token <- tokens[[1L]]
    rest <- tokens[-1L]
    operator <- if (S7::S7_inherits(node, SchemaNodeAllCmpt)) {
        "all"
    } else if (S7::S7_inherits(node, SchemaNodeAnyCmpt)) {
        "any"
    } else if (S7::S7_inherits(node, SchemaNodeOneCmpt)) {
        "one"
    } else {
        stop("Unsupported n-ary schema node type.", call. = FALSE)
    }

    if (!is.character(token) || !identical(token, operator) || !length(rest) || !is.numeric(rest[[1L]])) {
        schema_edit__path_not_found(path)
    }

    index <- rest[[1L]]
    if (index < 1L || index > length(node@branches)) {
        schema_edit__path_not_found(path)
    }

    branches <- node@branches
    branches[[index]] <- schema_edit__modify_tree(branches[[index]], rest[-1L], fn, path)
    schema_edit__update_node(node, branches = branches)
}

S7::method(schema_edit__modify_tree, SchemaNodeNotCmpt) <- function(node, tokens, fn, path) {
    token <- tokens[[1L]]
    if (!is.character(token) || !identical(token, "not")) {
        schema_edit__path_not_found(path)
    }

    schema_edit__update_node(
        node,
        branch = schema_edit__modify_tree(node@branch, tokens[-1L], fn, path)
    )
}

schema_edit__modify_doc <- function(x, path, fn) {
    doc <- x
    tokens <- schema_edit__parse_path(path)

    if (!length(tokens)) {
        return(schema_edit__update_node(doc, root = fn(doc@root)))
    }

    if (is.character(tokens[[1L]]) && identical(tokens[[1L]], "defs")) {
        if (length(tokens) < 2L || !is.character(tokens[[2L]])) {
            stop(sprintf("`path` does not exist: %s", path), call. = FALSE)
        }
        name <- tokens[[2L]]
        if (is.null(doc@defs[[name]])) {
            stop(sprintf("`path` does not exist: %s", path), call. = FALSE)
        }

        defs_list <- doc@defs
        defs_list[[name]] <- schema_edit__modify_tree(defs_list[[name]], tokens[-c(1L, 2L)], fn, path)
        return(schema_edit__update_node(doc, defs = defs_list))
    }

    schema_edit__update_node(doc, root = schema_edit__modify_tree(doc@root, tokens, fn, path))
}

schema_edit__normalize_fragment <- function(x, what) {
    if (S7::S7_inherits(x, SchemaNode)) {
        return(as.list(x))
    }

    if (is.list(x)) {
        return(x)
    }

    stop(sprintf("Expected %s or `SchemaNode` object.", what), call. = FALSE)
}

schema_edit__combinator <- function(operator, branches, description = NULL) {
    checkmate::assert_choice(operator, c("all", "any", "one"))
    checkmate::assert_string(description, null.ok = TRUE)
    if (!length(branches)) {
        stop(sprintf("`schema_%s()` requires at least one branch.", operator), call. = FALSE)
    }
    if (!is.null(names(branches)) && any(nzchar(names(branches)))) {
        stop(sprintf("`schema_%s()` branches must be unnamed.", operator), call. = FALSE)
    }

    out <- list()
    out[[operator]] <- lapply(
        branches,
        schema_edit__normalize_fragment,
        what = "a schema branch fragment"
    )
    if (!is.null(description)) {
        out <- c(list(description = description), out)
    }
    out
}

schema_edit__group_value <- function(value, description = NULL) {
    value <- schema_edit__normalize_fragment(value, "a schema node fragment")
    operators <- names(value)[names(value) %in% SCHEMA_SPEC_OPERATORS]
    if (length(operators) != 1L) {
        stop("`value` must contain exactly one primary schema operator.", call. = FALSE)
    }

    desc <- schema_utils__coalesce(description, value$description)
    value <- value[names(value) != "description"]
    if (!is.null(desc)) {
        value <- c(list(description = desc), value)
    }
    value
}

#' Create a schema check fragment
#'
#' `schema_check()` creates a raw schema fragment with a `check` operator. The
#' helper performs only lightweight structural validation; semantic validation of
#' `kind` and check arguments is handled by `schema_doc()` and schema edit verbs.
#'
#' @param kind Check kind string.
#' @param ... Additional named checkmate arguments stored inside `check`.
#' @param description Optional node description.
#'
#' @return A raw schema fragment accepted by `schema_doc()` and schema edit verbs.
#'
#' @examples
#' schema_check("string", min.chars = 1)
#' schema <- schema_doc(schema_check("string", min.chars = 1))
#' schema
#'
#' @noRd
schema_check <- function(kind, ..., description = NULL) {
    checkmate::assert_string(kind, min.chars = 1L)
    checkmate::assert_string(description, null.ok = TRUE)
    dots <- list(...)
    if (length(dots)) {
        dot_names <- names(dots)
        if (is.null(dot_names) || !all(nzchar(dot_names))) {
            stop("`kind` must be supplied once; `...` must be named and must not include `kind`.", call. = FALSE)
        }
        if ("kind" %in% dot_names) {
            stop("`...` must not include `kind`.", call. = FALSE)
        }
        checkmate::assert_names(dot_names, type = "named")
        if (anyDuplicated(dot_names)) {
            stop("`...` must use unique names.", call. = FALSE)
        }
    }

    out <- list(check = c(list(kind = kind), dots))
    if (!is.null(description)) {
        out <- c(list(description = description), out)
    }
    out
}

#' Create a schema reference fragment
#'
#' `schema_ref()` creates a local `$defs` reference fragment. `name` may be
#' either a bare definition name such as `"text"` or a local ref string of the
#' form `"#/$defs/text"`.
#'
#' @param name Definition name or local `$defs` ref string.
#' @param description Optional node description.
#'
#' @return A raw schema fragment accepted by `schema_doc()` and schema edit verbs.
#'
#' @examples
#' schema <- schema_doc(list(
#'     `$defs` = list(text = schema_check("string")),
#'     `$ref` = "#/$defs/text"
#' ))
#' schema
#'
#' schema_validate(schema, "ok", mode = "test")
#' schema_ref("text")
#'
#' @noRd
schema_ref <- function(name, description = NULL) {
    checkmate::assert_string(description, null.ok = TRUE)

    out <- list(`$ref` = schema_edit__ref_value(name))
    if (!is.null(description)) {
        out <- c(list(description = description), out)
    }
    out
}

#' Create an `all` schema combinator fragment
#'
#' @param ... Branch schema fragments.
#' @param description Optional node description.
#'
#' @return A raw schema fragment accepted by `schema_doc()` and schema edit verbs.
#'
#' @examples
#' schema <- schema_doc(schema_all(
#'     schema_check("string"),
#'     schema_check("string", min.chars = 1)
#' ))
#' schema
#'
#' schema_validate(schema, "ok", mode = "test")
#'
#' @noRd
schema_all <- function(..., description = NULL) {
    schema_edit__combinator("all", list(...), description = description)
}

#' Create an `any` schema combinator fragment
#'
#' @param ... Branch schema fragments.
#' @param description Optional node description.
#'
#' @return A raw schema fragment accepted by `schema_doc()` and schema edit verbs.
#'
#' @examples
#' schema <- schema_doc(schema_any(schema_check("int"), schema_check("string")))
#' schema
#'
#' schema_validate(schema, "ok", mode = "test")
#'
#' @noRd
schema_any <- function(..., description = NULL) {
    schema_edit__combinator("any", list(...), description = description)
}

#' Create a `one` schema combinator fragment
#'
#' @param ... Branch schema fragments.
#' @param description Optional node description.
#'
#' @return A raw schema fragment accepted by `schema_doc()` and schema edit verbs.
#'
#' @examples
#' schema <- schema_doc(schema_one(schema_check("int"), schema_check("string")))
#' schema
#'
#' schema_validate(schema, "ok", mode = "test")
#'
#' @noRd
schema_one <- function(..., description = NULL) {
    schema_edit__combinator("one", list(...), description = description)
}

#' Create a `not` schema combinator fragment
#'
#' @param branch Branch schema fragment.
#' @param description Optional node description.
#'
#' @return A raw schema fragment accepted by `schema_doc()` and schema edit verbs.
#'
#' @examples
#' schema <- schema_doc(schema_not(schema_check("null")))
#' schema
#'
#' schema_validate(schema, "ok", mode = "test")
#'
#' @noRd
schema_not <- function(branch, description = NULL) {
    checkmate::assert_string(description, null.ok = TRUE)

    out <- list(not = schema_edit__normalize_fragment(branch, "a schema branch fragment"))
    if (!is.null(description)) {
        out <- c(list(description = description), out)
    }
    out
}

#' Create a schema group fragment
#'
#' @param names Field names covered by the group.
#' @param value Schema node fragment containing exactly one primary operator.
#' @param description Optional group description.
#'
#' @return A raw schema group fragment accepted in a schema document `groups`
#'   list or by `schema_add_group()`.
#'
#' @examples
#' schema <- schema_doc(list(
#'     check = list(kind = "list"),
#'     groups = list(schema_group(c("x", "y"), schema_check("number")))
#' ))
#' schema
#'
#' schema_validate(schema, list(x = 1, y = 2), mode = "test")
#'
#' @noRd
schema_group <- function(names, value, description = NULL) {
    checkmate::assert_character(names, any.missing = FALSE, min.len = 1L, unique = TRUE)

    c(list(names = names), schema_edit__group_value(value, description = description))
}

#' Replace a schema node
#'
#' @param x A `SchemaDoc`.
#' @param path Path to the target schema node. Use `$` for the root node. Bare
#'   field segments such as `$id` implicitly traverse container `fields`. Use
#'   `$fields$id` to write the explicit field path. Backtick-quote field names
#'   that contain path operators, for example ``$`a$b` ``.
#' @param value Replacement schema fragment using the same list syntax accepted
#'   by `schema_doc()`, or a fragment produced by helpers such as
#'   `schema_check()` or `schema_ref()`.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_doc(list(
#'     check = list(kind = "list"),
#'     fields = list(id = schema_check("int"))
#' ))
#' schema <- schema_replace(schema, "$id", schema_check("int", lower = 1))
#' schema
#'
#' schema_validate(schema, list(id = 1L), mode = "test")
#'
#' @noRd
schema_replace <- S7::new_generic(
    "schema_replace",
    "x",
    function(x, path = "$", value) {
        S7::S7_dispatch()
    }
)

S7::method(schema_replace, SchemaDoc) <- function(x, path = "$", value) {
    schema_edit__modify_doc(x, path, function(node) {
        schema_edit__as_node(
            value,
            defs = names(x@defs),
            path = path,
            context = sprintf("Invalid replacement at path `%s`.", path)
        )
    })
}

#' Set or remove a schema node description
#'
#' @param x A `SchemaDoc`.
#' @param path Path to the target schema node. Use `$` for the root node. Bare
#'   field segments such as `$id` implicitly traverse container `fields`. Use
#'   `$fields$id` to write the explicit field path. Backtick-quote field names
#'   that contain path operators, for example ``$`a$b` ``.
#' @param description Optional description string. Use `NULL` to remove the
#'   description.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_doc(schema_check("string"))
#' schema_set_desc(schema, "$", "A non-empty label.")
#'
#' @noRd
schema_set_desc <- S7::new_generic(
    "schema_set_desc",
    "x",
    function(x, path = "$", description = NULL) {
        S7::S7_dispatch()
    }
)

S7::method(schema_set_desc, SchemaDoc) <- function(x, path = "$", description = NULL) {
    checkmate::assert_string(description, null.ok = TRUE)

    schema_edit__modify_doc(x, path, function(node) {
        schema_edit__update_node(node, desc = description)
    })
}

#' Set a schema node keys rule
#'
#' @param x A `SchemaDoc`.
#' @param path Path to the target schema node. Use `$` for the root node. Bare
#'   field segments such as `$id` implicitly traverse container `fields`. Use
#'   `$fields$id` to write the explicit field path. Backtick-quote field names
#'   that contain path operators, for example ``$`a$b` ``.
#' @param ... Named `keys` rule arguments passed through to the schema DSL.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_doc(list(check = list(kind = "list")))
#' schema
#' schema_validate(schema, list(id = 1L), mode = "test")
#'
#' schema <- schema_set_keys(schema, type = "named", must.include = "id")
#' schema
#' schema_validate(schema, list(id = 1L), mode = "assert")
#'
#' @noRd
schema_set_keys <- S7::new_generic(
    "schema_set_keys",
    "x",
    function(x, path = "$", ...) {
        S7::S7_dispatch()
    }
)

S7::method(schema_set_keys, SchemaDoc) <- function(x, path = "$", ...) {
    dots <- list(...)

    if (!length(dots)) {
        stop("Supply at least one keys-rule argument.", call. = FALSE)
    }

    schema_edit__modify_doc(x, path, function(node) {
        if (!S7::S7_inherits(node, SchemaNodeCheck)) {
            stop("`keys` is only allowed on check nodes.", call. = FALSE)
        }
        rule <- schema_spec__name_rule(dots, paste0(path, "$keys"))
        schema_edit__update_node(node, name = rule)
    })
}

#' Delete a schema node keys rule
#'
#' @param x A `SchemaDoc`.
#' @param path Path to the target schema node. Use `$` for the root node. Bare
#'   field segments such as `$id` implicitly traverse container `fields`. Use
#'   `$fields$id` to write the explicit field path. Backtick-quote field names
#'   that contain path operators, for example ``$`a$b` ``.
#' @param missing Missing-target behavior. Use `"error"` to raise an error or
#'   `"ignore"` to leave the schema unchanged.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_doc(list(check = list(kind = "list"), keys = list(type = "named")))
#' schema <- schema_del_keys(schema)
#' schema
#'
#' as.list(schema)$keys
#'
#' @noRd
schema_del_keys <- S7::new_generic(
    "schema_del_keys",
    "x",
    function(x, path = "$", missing = "error") {
        S7::S7_dispatch()
    }
)

S7::method(schema_del_keys, SchemaDoc) <- function(x, path = "$", missing = "error") {
    missing <- match.arg(missing, c("error", "ignore"))

    schema_edit__modify_doc(x, path, function(node) {
        if (!S7::S7_inherits(node, SchemaNodeCheck)) {
            stop("`keys` is only allowed on check nodes.", call. = FALSE)
        }
        if (is.null(node@name)) {
            if (identical(missing, "error")) {
                stop(sprintf("`keys` does not exist at `%s`.", path), call. = FALSE)
            }
            return(node)
        }
        schema_edit__update_node(node, name = NULL)
    })
}

#' Add a field schema to a container node
#'
#' @param x A `SchemaDoc`.
#' @param name Field name to add.
#' @param field Schema fragment using the same list syntax accepted by
#'   `schema_doc()`, or a fragment produced by helpers such as `schema_check()`.
#' @param path Path to the target container node. Use `$` for the root node.
#'   Bare field segments such as `$id` implicitly traverse container `fields`. Use
#'   `$fields$id` to write the explicit field path. Backtick-quote field names
#'   that contain path operators, for example ``$`a$b` ``.
#' @param overwrite Logical flag indicating whether an existing field of the same
#'   name should be replaced.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_doc(list(check = list(kind = "list")))
#' schema
#' schema <- schema_add_field(schema, "id", schema_check("int", lower = 1))
#' schema
#'
#' schema_validate(schema, list(id = 1L), mode = "test")
#'
#' @noRd
schema_add_field <- S7::new_generic(
    "schema_add_field",
    "x",
    function(x, name, field, path = "$", overwrite = FALSE) {
        S7::S7_dispatch()
    }
)

S7::method(schema_add_field, SchemaDoc) <- function(x, name, field, path = "$", overwrite = FALSE) {
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_flag(overwrite)
    doc <- x

    schema_edit__modify_doc(doc, path, function(node) {
        if (!S7::S7_inherits(node, SchemaNodeContainerCmpt)) {
            stop(sprintf("`path` does not identify a container node: %s", path), call. = FALSE)
        }

        idx <- schema_edit__field_binding_index(node@exact, name)
        if (!overwrite && !is.na(idx)) {
            stop(sprintf("Field `%s` already exists at `%s`.", name, path), call. = FALSE)
        }

        field <- schema_edit__as_node(
            field,
            defs = names(doc@defs),
            path = paste0(path, "$fields$", name),
            context = sprintf("Invalid field schema `%s` at path `%s`.", name, path)
        )

        binding <- SchemaBindingExactCmpt(keys = name, target = field)
        exact <- node@exact
        if (is.na(idx)) {
            exact[[length(exact) + 1L]] <- binding
        } else {
            exact <- schema_edit__replace_field_binding(exact, idx, name, field)
        }
        schema_edit__update_node(node, exact = exact)
    })
}

#' Add a schema group to a container node
#'
#' @param x A `SchemaDoc`.
#' @param group Schema group fragment using the same list syntax accepted by
#'   `schema_doc()`, or a fragment produced by `schema_group()`.
#' @param path Path to the target container node. Use `$` for the root node.
#'   Bare field segments such as `$id` implicitly traverse container `fields`. Use
#'   `$fields$id` to write the explicit field path. Backtick-quote field names
#'   that contain path operators, for example ``$`a$b` ``.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_doc(list(check = list(kind = "list")))
#' schema
#' schema <- schema_add_group(schema, schema_group(c("x", "y"), schema_check("number")))
#' schema
#'
#' schema_validate(schema, list(x = 1, y = 2), mode = "test")
#'
#' @noRd
schema_add_group <- S7::new_generic(
    "schema_add_group",
    "x",
    function(x, group, path = "$") {
        S7::S7_dispatch()
    }
)

S7::method(schema_add_group, SchemaDoc) <- function(x, group, path = "$") {
    doc <- x

    schema_edit__modify_doc(doc, path, function(node) {
        if (!S7::S7_inherits(node, SchemaNodeContainerCmpt)) {
            stop(sprintf("`path` does not identify a container node: %s", path), call. = FALSE)
        }
        group <- schema_edit__as_group_binding(
            group,
            defs = names(doc@defs),
            path = paste0(path, "$groups"),
            context = sprintf("Invalid group schema at path `%s`.", path)
        )
        schema_edit__update_node(node, exact = c(node@exact, list(group)))
    })
}

#' Set or replace a container rest schema
#'
#' @param x A `SchemaDoc`.
#' @param field Schema fragment using the same list syntax accepted by
#'   `schema_doc()`, or a fragment produced by helpers such as `schema_check()`,
#'   to store as the `rest` schema.
#' @param path Path to the target container node. Use `$` for the root node.
#'   Bare field segments such as `$id` implicitly traverse container `fields`. Use
#'   `$fields$id` to write the explicit field path. Backtick-quote field names
#'   that contain path operators, for example ``$`a$b` ``.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_doc(list(
#'     check = list(kind = "list"),
#'     keys = list(type = "unnamed")
#' ))
#' schema <- schema_set_rest(schema, schema_check("string"))
#' schema
#'
#' schema_validate(schema, list("a", "b"), mode = "test")
#'
#' @noRd
schema_set_rest <- S7::new_generic(
    "schema_set_rest",
    "x",
    function(x, field, path = "$") {
        S7::S7_dispatch()
    }
)

S7::method(schema_set_rest, SchemaDoc) <- function(x, field, path = "$") {
    doc <- x

    schema_edit__modify_doc(doc, path, function(node) {
        if (!S7::S7_inherits(node, SchemaNodeContainerCmpt)) {
            stop(sprintf("`path` does not identify a container node: %s", path), call. = FALSE)
        }
        field <- schema_edit__as_node(
            field,
            defs = names(doc@defs),
            path = paste0(path, "$rest"),
            context = sprintf("Invalid rest schema at path `%s`.", path)
        )
        schema_edit__update_node(node, rest = field)
    })
}

#' Add a position schema to an unnamed container node
#'
#' @param x A `SchemaDoc`.
#' @param index 1-based insertion index. `1` inserts at the front and
#'   `length(positions) + 1` appends.
#' @param value Schema fragment using the same list syntax accepted by
#'   `schema_doc()`, or a fragment produced by helpers such as `schema_check()`.
#' @param path Path to the target unnamed container node. Use `$` for the root
#'   node.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_doc(list(
#'     check = list(kind = "list"),
#'     keys = list(type = "unnamed")
#' ))
#' schema <- schema_add_position(schema, 1, schema_check("string"))
#' schema <- schema_add_position(schema, 2, schema_check("int"))
#' schema
#'
#' schema_validate(schema, list("a", 1L), mode = "test")
#'
#' @noRd
schema_add_position <- S7::new_generic(
    "schema_add_position",
    "x",
    function(x, index, value, path = "$") {
        S7::S7_dispatch()
    }
)

S7::method(schema_add_position, SchemaDoc) <- function(x, index, value, path = "$") {
    checkmate::assert_count(index, positive = TRUE)
    doc <- x

    schema_edit__modify_doc(doc, path, function(node) {
        if (!S7::S7_inherits(node, SchemaNodeContainerCmpt)) {
            stop(sprintf("`path` does not identify a container node: %s", path), call. = FALSE)
        }
        if (!identical(schema_spec__name_type(node@name), "unnamed")) {
            stop("`positions` requires `keys$type = 'unnamed'`.", call. = FALSE)
        }
        if (index > length(node@positions) + 1L) {
            stop(sprintf("`index` must be at most %d at `%s`.", length(node@positions) + 1L, path), call. = FALSE)
        }

        value <- schema_edit__as_node(
            value,
            defs = names(doc@defs),
            path = sprintf("%s$positions[%d]", path, index),
            context = sprintf("Invalid position schema %d at path `%s`.", index, path)
        )

        positions <- append(node@positions, list(value), after = index - 1L)
        schema_edit__update_node(node, positions = positions)
    })
}

#' Delete a field schema from a container node
#'
#' @param x A `SchemaDoc`.
#' @param name Field name to remove.
#' @param path Path to the target container node. Use `$` for the root node.
#'   Bare field segments such as `$id` implicitly traverse container `fields`. Use
#'   `$fields$id` to write the explicit field path. Backtick-quote field names
#'   that contain path operators, for example ``$`a$b` ``.
#' @param missing Missing-target behavior. Use `"error"` to raise an error or
#'   `"ignore"` to leave the schema unchanged.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_doc(list(check = list(kind = "list")))
#' schema
#' schema <- schema_add_field(schema, "id", schema_check("int"))
#' schema
#' schema <- schema_del_field(schema, "id")
#' schema
#'
#' @noRd
schema_del_field <- S7::new_generic(
    "schema_del_field",
    "x",
    function(x, name, path = "$", missing = "error") {
        S7::S7_dispatch()
    }
)

S7::method(schema_del_field, SchemaDoc) <- function(x, name, path = "$", missing = "error") {
    checkmate::assert_string(name, min.chars = 1L)
    missing <- match.arg(missing, c("error", "ignore"))

    schema_edit__modify_doc(x, path, function(node) {
        if (!S7::S7_inherits(node, SchemaNodeContainerCmpt)) {
            if (identical(missing, "error")) {
                stop(sprintf("Field `%s` does not exist at `%s`.", name, path), call. = FALSE)
            }
            return(node)
        }

        idx <- schema_edit__field_binding_index(node@exact, name)
        if (is.na(idx)) {
            if (identical(missing, "error")) {
                stop(sprintf("Field `%s` does not exist at `%s`.", name, path), call. = FALSE)
            }
            return(node)
        }

        exact <- node@exact
        exact <- schema_edit__delete_field_binding(exact, idx, name)
        schema_edit__update_node(node, exact = exact)
    })
}

#' Delete a schema group from a container node
#'
#' @param x A `SchemaDoc`.
#' @param index 1-based group index to remove.
#' @param path Path to the target container node. Use `$` for the root node.
#'   Bare field segments such as `$id` implicitly traverse container `fields`. Use
#'   `$fields$id` to write the explicit field path. Backtick-quote field names
#'   that contain path operators, for example ``$`a$b` ``.
#' @param missing Missing-target behavior. Use `"error"` to raise an error or
#'   `"ignore"` to leave the schema unchanged.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_doc(list(
#'     check = list(kind = "list"),
#'     groups = list(schema_group(c("x", "y"), schema_check("number")))
#' ))
#' schema
#'
#' schema_del_group(schema, 1)
#'
#' @noRd
schema_del_group <- S7::new_generic(
    "schema_del_group",
    "x",
    function(x, index, path = "$", missing = "error") {
        S7::S7_dispatch()
    }
)

S7::method(schema_del_group, SchemaDoc) <- function(x, index, path = "$", missing = "error") {
    checkmate::assert_count(index, positive = TRUE)
    missing <- match.arg(missing, c("error", "ignore"))

    schema_edit__modify_doc(x, path, function(node) {
        if (!S7::S7_inherits(node, SchemaNodeContainerCmpt)) {
            if (identical(missing, "error")) {
                stop(sprintf("Group %d does not exist at `%s`.", index, path), call. = FALSE)
            }
            return(node)
        }

        group_idx <- which(vapply(node@exact, function(binding) length(binding@keys) > 1L, logical(1L)))
        if (index > length(group_idx)) {
            if (identical(missing, "error")) {
                stop(sprintf("Group %d does not exist at `%s`.", index, path), call. = FALSE)
            }
            return(node)
        }

        exact <- node@exact
        exact[[group_idx[[index]]]] <- NULL
        schema_edit__update_node(node, exact = exact)
    })
}

#' Delete a container rest schema
#'
#' @param x A `SchemaDoc`.
#' @param path Path to the target container node. Use `$` for the root node.
#'   Bare field segments such as `$id` implicitly traverse container `fields`. Use
#'   `$fields$id` to write the explicit field path. Backtick-quote field names
#'   that contain path operators, for example ``$`a$b` ``.
#' @param missing Missing-target behavior. Use `"error"` to raise an error or
#'   `"ignore"` to leave the schema unchanged.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_doc(list(check = list(kind = "list")))
#' schema <- schema_set_rest(schema, schema_check("string"))
#' schema <- schema_del_rest(schema)
#' schema
#'
#' as.list(schema)$rest
#'
#' @noRd
schema_del_rest <- S7::new_generic(
    "schema_del_rest",
    "x",
    function(x, path = "$", missing = "error") {
        S7::S7_dispatch()
    }
)

S7::method(schema_del_rest, SchemaDoc) <- function(x, path = "$", missing = "error") {
    missing <- match.arg(missing, c("error", "ignore"))

    schema_edit__modify_doc(x, path, function(node) {
        if (!S7::S7_inherits(node, SchemaNodeContainerCmpt) || is.null(node@rest)) {
            if (identical(missing, "error")) {
                stop(sprintf("Rest schema does not exist at `%s`.", path), call. = FALSE)
            }
            return(node)
        }

        schema_edit__update_node(node, rest = NULL)
    })
}

#' Delete a position schema from an unnamed container node
#'
#' @param x A `SchemaDoc`.
#' @param index 1-based position index to remove.
#' @param path Path to the target unnamed container node. Use `$` for the root
#'   node.
#' @param missing Missing-target behavior. Use `"error"` to raise an error or
#'   `"ignore"` to leave the schema unchanged.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_doc(list(check = list(kind = "list"), keys = list(type = "unnamed")))
#' schema <- schema_add_position(schema, 1, schema_check("string"))
#' schema <- schema_del_position(schema, 1)
#' schema
#'
#' as.list(schema)$positions
#'
#' @noRd
schema_del_position <- S7::new_generic(
    "schema_del_position",
    "x",
    function(x, index, path = "$", missing = "error") {
        S7::S7_dispatch()
    }
)

S7::method(schema_del_position, SchemaDoc) <- function(x, index, path = "$", missing = "error") {
    checkmate::assert_count(index, positive = TRUE)
    missing <- match.arg(missing, c("error", "ignore"))

    schema_edit__modify_doc(x, path, function(node) {
        if (!S7::S7_inherits(node, SchemaNodeContainerCmpt) || index > length(node@positions)) {
            if (identical(missing, "error")) {
                stop(sprintf("Position %d does not exist at `%s`.", index, path), call. = FALSE)
            }
            return(node)
        }

        positions <- node@positions
        positions[[index]] <- NULL
        schema_edit__update_node(node, positions = positions)
    })
}

#' Add a schema definition
#'
#' @param x A `SchemaDoc`.
#' @param name Definition name to add.
#' @param value Schema fragment using the same list syntax accepted by
#'   `schema_doc()`, or a fragment produced by helpers such as `schema_check()`,
#'   to store in `$defs`.
#' @param overwrite Logical flag indicating whether an existing definition of the
#'   same name should be replaced.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_doc(schema_check("string"))
#' schema <- schema_add_def(schema, "text", schema_check("string"))
#' schema
#'
#' names(as.list(schema)$`$defs`)
#'
#' @noRd
schema_add_def <- S7::new_generic(
    "schema_add_def",
    "x",
    function(x, name, value, overwrite = FALSE) {
        S7::S7_dispatch()
    }
)

S7::method(schema_add_def, SchemaDoc) <- function(x, name, value, overwrite = FALSE) {
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_flag(overwrite)
    doc <- x
    if (grepl("/", name, fixed = TRUE)) {
        stop("`name` must not contain `/`.", call. = FALSE)
    }
    if (!overwrite && !is.null(doc@defs[[name]])) {
        stop(sprintf("Definition `%s` already exists.", name), call. = FALSE)
    }

    defs_names <- names(doc@defs)
    if (!name %in% defs_names) {
        defs_names <- c(defs_names, name)
    }
    value <- schema_edit__as_node(
        value,
        defs = defs_names,
        path = sprintf("$`$defs`[['%s']]", name),
        context = sprintf("Invalid schema definition `%s`.", name)
    )

    defs <- doc@defs
    defs[[name]] <- value
    schema_edit__update_node(doc, defs = defs)
}

#' Delete a schema definition
#'
#' @param x A `SchemaDoc`.
#' @param name Definition name to remove.
#' @param missing Missing-target behavior. Use `"error"` to raise an error or
#'   `"ignore"` to leave the schema unchanged.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_doc(schema_check("string"))
#' schema <- schema_add_def(schema, "text", schema_check("string"))
#' schema <- schema_del_def(schema, "text")
#' schema
#'
#' as.list(schema)$`$defs`
#'
#' @noRd
schema_del_def <- S7::new_generic(
    "schema_del_def",
    "x",
    function(x, name, missing = "error") {
        S7::S7_dispatch()
    }
)

S7::method(schema_del_def, SchemaDoc) <- function(x, name, missing = "error") {
    checkmate::assert_string(name, min.chars = 1L)
    missing <- match.arg(missing, c("error", "ignore"))

    doc <- x
    if (is.null(doc@defs[[name]])) {
        if (identical(missing, "error")) {
            stop(sprintf("Definition `%s` does not exist.", name), call. = FALSE)
        }
        return(doc)
    }

    defs <- doc@defs
    defs[[name]] <- NULL

    refs <- c(schema_edit__node_refs(doc@root), unlist(lapply(defs, schema_edit__node_refs), use.names = FALSE))
    missing_refs <- setdiff(sub("^#/\\$defs/", "", refs), names(defs))
    if (length(missing_refs)) {
        stop(sprintf("Definition `%s` is still referenced.", missing_refs[[1L]]), call. = FALSE)
    }

    schema_edit__update_node(doc, defs = defs)
}


# -------------------- schema-flat.R

# SchemaFlat {{{
schema_flat__node_is_flat <- function(x) {
    S7::S7_inherits(x, SchemaNodeLeaf) ||
        S7::S7_inherits(x, SchemaNodeContainerFlat) ||
        S7::S7_inherits(x, SchemaNodeNaryFlat) ||
        S7::S7_inherits(x, SchemaNodeNotFlat)
}

schema_flat__binding_name <- function(x) {
    x@keys[[1L]]
}

schema_flat__binding_names <- function(x) {
    vapply(x, schema_flat__binding_name, character(1L))
}

SchemaBindingExactFlat <- S7::new_class(
    "SchemaBindingExactFlat",
    parent = SchemaBindingExact,
    validator = function(self) {
        if (length(self@keys) != 1L) {
            return("@keys requires exactly one key.")
        }

        if (!schema_flat__node_is_flat(self@target)) {
            return("@target must be a flat schema node.")
        }
    }
)

SchemaBindingPatternFlat <- S7::new_class(
    "SchemaBindingPatternFlat",
    parent = SchemaBindingPattern,
    validator = function(self) {
        if (!schema_flat__node_is_flat(self@target)) {
            return("@target must be a flat schema node.")
        }
    }
)

SchemaNodeContainerFlat <- S7::new_class(
    "SchemaNodeContainerFlat",
    parent = SchemaNodeContainer,
    properties = list(
        exact = schema_utils__prop_list(
            "SchemaBindingExactFlat",
            names = "unnamed",
            default = list()
        ),
        patterns = schema_utils__prop_list(
            "SchemaBindingPatternFlat",
            names = "unnamed",
            default = list()
        ),
        positions = schema_utils__prop_list(
            "SchemaNode",
            names = "unnamed",
            default = list()
        ),
        rest = S7::new_property(
            NULL | SchemaNode,
            default = NULL
        )
    ),
    validator = function(self) {
        if (!schema_spec__kind_is_container(self@value@kind)) {
            return(sprintf(
                "@value requires a container kind; got `%s`. Allowed container kinds are: %s.",
                self@value@kind,
                paste0("'", SCHEMA_SPEC_KINDS_CONTAINER, "'", collapse = ", ")
            ))
        }

        if (!is.null(self@rest) && !schema_flat__node_is_flat(self@rest)) {
            return("@rest must be a flat schema node.")
        }

        if (length(self@positions)) {
            bad <- !vapply(self@positions, schema_flat__node_is_flat, logical(1L))
            if (any(bad)) {
                return("@positions must all be flat schema nodes.")
            }
        }

        if (length(self@exact) > 0L) {
            keys <- vapply(self@exact, function(x) x@keys, character(1L))
            msg <- schema_utils__checkmate_result(checkmate::check_character(keys, unique = TRUE), label = "@exact")
            if (!is.null(msg)) {
                return(sprintf("%s ('%s')", msg, keys[duplicated(keys)][[1L]]))
            }
        }

        if (length(self@positions) && !identical(schema_spec__name_type(self@name), "unnamed")) {
            return("`positions` requires `keys$type = 'unnamed'`.")
        }

        if (identical(schema_spec__name_type(self@name), "unnamed") && (length(self@exact) || length(self@patterns))) {
            return("`keys$type = 'unnamed'` only allows `positions` and `rest` constraints.")
        }
    }
)

SchemaNodeNaryFlat <- S7::new_class(
    "SchemaNodeNaryFlat",
    parent = SchemaNodeNary,
    properties = list(
        branches = schema_utils__prop_list(
            "SchemaNode",
            names = "unnamed",
            min.len = 1L,
            default = list()
        )
    ),
    validator = function(self) {
        bad <- !vapply(self@branches, schema_flat__node_is_flat, logical(1L))
        if (any(bad)) {
            return("@branches must all be flat schema nodes.")
        }
    },
    abstract = TRUE
)

SchemaNodeAllFlat <- S7::new_class("SchemaNodeAllFlat", parent = SchemaNodeNaryFlat)
SchemaNodeAnyFlat <- S7::new_class("SchemaNodeAnyFlat", parent = SchemaNodeNaryFlat)
SchemaNodeOneFlat <- S7::new_class("SchemaNodeOneFlat", parent = SchemaNodeNaryFlat)

SchemaNodeNotFlat <- S7::new_class(
    "SchemaNodeNotFlat",
    parent = SchemaNodeNot,
    properties = list(
        branch = S7::new_property(
            SchemaNode,
            validator = function(value) {
                if (!schema_flat__node_is_flat(value)) {
                    return("@branch must be a flat schema node.")
                }
            }
        )
    )
)

SchemaFlat <- S7::new_class(
    "SchemaFlat",
    parent = SchemaSpec,
    properties = list(
        path = schema_utils__prop_string(null.ok = TRUE, default = NULL),
        version = schema_utils__prop_string(null.ok = TRUE),
        root = S7::new_property(
            SchemaNode,
            validator = function(value) {
                if (!schema_flat__node_is_flat(value)) {
                    return("@root must be a flat schema node.")
                }
            }
        )
    )
)
# }}}

S7::method(schema_compact__same, list(SchemaBindingExactFlat, SchemaBindingExactFlat)) <- function(x, y) {
    identical(x@keys, y@keys) &&
        schema_compact__same(x@target, y@target)
}
S7::method(schema_compact__same, list(SchemaBindingPatternFlat, SchemaBindingPatternFlat)) <- function(x, y) {
    identical(x@pattern, y@pattern) &&
        schema_compact__same(x@target, y@target)
}
S7::method(schema_compact__same, list(SchemaNodeContainerFlat, SchemaNodeContainerFlat)) <-
    schema_compact__same_container
S7::method(schema_compact__same, list(SchemaNodeAllFlat, SchemaNodeAllFlat)) <- schema_compact__same_nary
S7::method(schema_compact__same, list(SchemaNodeAnyFlat, SchemaNodeAnyFlat)) <- schema_compact__same_nary
S7::method(schema_compact__same, list(SchemaNodeOneFlat, SchemaNodeOneFlat)) <- schema_compact__same_nary
S7::method(schema_compact__same, list(SchemaNodeNotFlat, SchemaNodeNotFlat)) <- schema_compact__same_not

# as.list.Schema {{{
S7::method(as.list, SchemaNodeContainerFlat) <- function(x, ...) {
    out <- list(check = as.list(x@value))
    keys <- schema_utils__keys_as_list(x@name)
    if (!is.null(keys)) {
        out$keys <- keys
    }
    if (length(x@exact)) {
        fields <- stats::setNames(
            lapply(x@exact, function(binding) as.list(binding@target)),
            vapply(x@exact, function(binding) binding@keys, character(1L))
        )
        out$fields <- fields
    }
    if (length(x@patterns)) {
        out$patterns <- stats::setNames(
            lapply(x@patterns, function(binding) as.list(binding@target)),
            vapply(x@patterns, function(binding) binding@pattern, character(1L))
        )
    }
    if (length(x@positions)) {
        out$positions <- lapply(x@positions, as.list)
    }
    if (!is.null(x@rest)) {
        out$rest <- as.list(x@rest)
    }
    schema_utils__as_list_add_desc(out, x)
}

S7::method(as.list, SchemaNodeAllFlat) <- function(x, ...) {
    schema_utils__as_list_nary(x, "all")
}

S7::method(as.list, SchemaNodeAnyFlat) <- function(x, ...) {
    schema_utils__as_list_nary(x, "any")
}

S7::method(as.list, SchemaNodeOneFlat) <- function(x, ...) {
    schema_utils__as_list_nary(x, "one")
}

S7::method(as.list, SchemaNodeNotFlat) <- function(x, ...) {
    schema_utils__as_list_add_desc(list(not = as.list(x@branch)), x)
}

S7::method(as.list, SchemaFlat) <- function(x, ...) {
    # Top-level serialization contract for SchemaFlat:
    # 1. `version` first when present
    # 2. root `description` next when present
    # 3. serialized root operator-specific entries last
    # 4. `path` is compile metadata and is intentionally excluded
    # Root node serialization itself follows the shared contract that
    # `description` appears before all operator-specific entries.
    out <- list()
    if (!is.null(x@version)) {
        out$version <- x@version
    }
    c(out, as.list(x@root))
}
# }}}

# schema_flatten {{{
#' Flatten a schema for repeated validation
#'
#' `schema_flatten()` converts a schema document, raw schema DSL list, or flat
#' runtime schema node into a `SchemaFlat`. Reuse the flattened schema when
#' validating many inputs against the same schema.
#'
#' @param x A schema document, raw schema DSL list, `SchemaFlat`, or flattened
#'   flat schema node.
#'
#' @return A flattened `SchemaFlat`.
#'
#' @examples
#' schema <- schema_doc(list(
#'     check = list(kind = "list"),
#'     fields = list(id = list(check = list(kind = "int", lower = 1)))
#' ))
#'
#' flat <- schema_flatten(schema)
#' schema_validate(flat, list(id = 1L), mode = "test")
#'
#' @noRd
schema_flatten <- function(x) {
    if (
        S7::S7_inherits(x, SchemaDoc) ||
            S7::S7_inherits(x, SchemaFlat) ||
            S7::S7_inherits(x, SchemaNode)
    ) {
        return(schema_flat__compile(x))
    }

    schema_flat__compile(schema_doc(x))
}
# }}}

# schema_flat__compile {{{
schema_flat__rule_check <- function(x) {
    if (!length(x@args)) {
        return(SchemaRuleCheck(kind = x@kind))
    }

    SchemaRuleCheck(kind = x@kind, args = x@args)
}

schema_flat__rule_names <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }
    SchemaRuleNames(args = x@args)
}

schema_flat__ref_name <- function(ref) {
    sub("^#/\\$defs/", "", ref)
}

schema_flat__context <- function(defs = list()) {
    ctx <- new.env(parent = emptyenv())
    ctx$defs <- defs
    ctx$cache <- list()
    ctx$stack <- character()
    ctx
}

schema_flat__overlay_desc <- S7::new_generic(
    "schema_flat__overlay_desc",
    "x",
    function(x, desc) S7::S7_dispatch()
)

S7::method(schema_flat__overlay_desc, SchemaNodeLeaf) <- function(x, desc) {
    if (is.null(desc)) {
        return(x)
    }

    SchemaNodeLeaf(value = x@value, name = x@name, desc = desc)
}

S7::method(schema_flat__overlay_desc, SchemaNodeContainerFlat) <- function(x, desc) {
    if (is.null(desc)) {
        return(x)
    }

    SchemaNodeContainerFlat(
        value = x@value,
        name = x@name,
        exact = x@exact,
        patterns = x@patterns,
        positions = x@positions,
        rest = x@rest,
        desc = desc
    )
}

S7::method(schema_flat__overlay_desc, SchemaNodeAllFlat) <- function(x, desc) {
    if (is.null(desc)) {
        return(x)
    }

    SchemaNodeAllFlat(branches = x@branches, desc = desc)
}

S7::method(schema_flat__overlay_desc, SchemaNodeAnyFlat) <- function(x, desc) {
    if (is.null(desc)) {
        return(x)
    }

    SchemaNodeAnyFlat(branches = x@branches, desc = desc)
}

S7::method(schema_flat__overlay_desc, SchemaNodeOneFlat) <- function(x, desc) {
    if (is.null(desc)) {
        return(x)
    }

    SchemaNodeOneFlat(branches = x@branches, desc = desc)
}

S7::method(schema_flat__overlay_desc, SchemaNodeNotFlat) <- function(x, desc) {
    if (is.null(desc)) {
        return(x)
    }

    SchemaNodeNotFlat(branch = x@branch, desc = desc)
}

S7::method(schema_flat__overlay_desc, SchemaNode) <- function(x, desc) {
    if (is.null(desc)) {
        return(x)
    }

    stop("unsupported flattened schema node.", call. = FALSE)
}

schema_flat__def <- function(name, ctx) {
    if (is.null(ctx$defs[[name]])) {
        stop(sprintf("`$ref` target `#/$defs/%s` is not available during flattening.", name), call. = FALSE)
    }

    if (name %in% ctx$stack) {
        stop(
            sprintf(
                "circular `$ref` detected while compiling: %s.",
                paste(c(ctx$stack, name), collapse = " -> ")
            ),
            call. = FALSE
        )
    }

    if (!is.null(ctx$cache[[name]])) {
        return(ctx$cache[[name]])
    }

    ctx$stack <- c(ctx$stack, name)
    on.exit(
        {
            ctx$stack <- utils::head(ctx$stack, -1L)
        },
        add = TRUE
    )

    compiled <- schema_flat__node(ctx$defs[[name]], ctx)
    ctx$cache[[name]] <- compiled
    compiled
}

schema_flat__ref <- function(x, ctx) {
    schema_flat__overlay_desc(schema_flat__def(schema_flat__ref_name(x@ref), ctx), x@desc)
}

S7::method(schema_utils__convert, SchemaBindingExactCmpt) <- function(from, to, ...) {
    if (!identical(to, SchemaBindingExactFlat)) {
        stop("`SchemaBindingExactCmpt` can only be converted to `SchemaBindingExactFlat`.", call. = FALSE)
    }
    if (length(from@keys) != 1L) {
        stop("`SchemaBindingExactCmpt` must contain exactly one key to convert to `SchemaBindingExactFlat`.", call. = FALSE)
    }

    SchemaBindingExactFlat(keys = from@keys, target = from@target)
}

schema_flat__bindings <- function(bindings, ctx) {
    if (!length(bindings)) {
        return(list())
    }

    compiled <- vector("list", sum(vapply(bindings, function(binding) length(binding@keys), integer(1L))))
    pos <- 1L
    for (binding in bindings) {
        target <- schema_flat__node(binding@target, ctx)
        for (key in binding@keys) {
            compiled[[pos]] <- SchemaBindingExactFlat(keys = key, target = target)
            pos <- pos + 1L
        }
    }

    if (length(compiled)) {
        keys <- schema_flat__binding_names(compiled)
        dup_keys <- unique(keys[duplicated(keys)])
        if (length(dup_keys)) {
            stop(sprintf("duplicate compiled field key(s): %s.", paste(dup_keys, collapse = ", ")), call. = FALSE)
        }
    }

    compiled
}

schema_flat__pattern <- function(binding, ctx) {
    SchemaBindingPatternFlat(
        pattern = binding@pattern,
        target = schema_flat__node(binding@target, ctx)
    )
}

schema_flat__patterns <- function(patterns, ctx) {
    lapply(patterns, schema_flat__pattern, ctx = ctx)
}

schema_flat__positions <- function(positions, ctx) {
    lapply(positions, schema_flat__node, ctx = ctx)
}

schema_flat__branches <- function(branches, ctx) {
    lapply(branches, schema_flat__node, ctx = ctx)
}

schema_flat__doc <- function(x) {
    ctx <- schema_flat__context(x@defs)

    SchemaFlat(
        path = x@path,
        version = x@version,
        root = schema_flat__node(x@root, ctx)
    )
}

schema_flat__node <- S7::new_generic(
    "schema_flat__node",
    "x",
    function(x, ctx) S7::S7_dispatch()
)

S7::method(schema_flat__node, SchemaNodeLeaf) <- function(x, ctx) {
    SchemaNodeLeaf(
        value = schema_flat__rule_check(x@value),
        name = schema_flat__rule_names(x@name),
        desc = x@desc
    )
}

S7::method(schema_flat__node, SchemaNodeContainerCmpt) <- function(x, ctx) {
    exact <- schema_flat__bindings(x@exact, ctx)
    patterns <- schema_flat__patterns(x@patterns, ctx)
    positions <- schema_flat__positions(x@positions, ctx)
    rest <- if (is.null(x@rest)) NULL else schema_flat__node(x@rest, ctx)

    SchemaNodeContainerFlat(
        value = schema_flat__rule_check(x@value),
        name = schema_flat__rule_names(x@name),
        exact = exact,
        patterns = patterns,
        positions = positions,
        rest = rest,
        desc = x@desc
    )
}

S7::method(schema_flat__node, SchemaNodeRef) <- function(x, ctx) {
    schema_flat__ref(x, ctx)
}

S7::method(schema_flat__node, SchemaNodeAllCmpt) <- function(x, ctx) {
    SchemaNodeAllFlat(branches = schema_flat__branches(x@branches, ctx), desc = x@desc)
}

S7::method(schema_flat__node, SchemaNodeAnyCmpt) <- function(x, ctx) {
    SchemaNodeAnyFlat(branches = schema_flat__branches(x@branches, ctx), desc = x@desc)
}

S7::method(schema_flat__node, SchemaNodeOneCmpt) <- function(x, ctx) {
    SchemaNodeOneFlat(branches = schema_flat__branches(x@branches, ctx), desc = x@desc)
}

S7::method(schema_flat__node, SchemaNodeNotCmpt) <- function(x, ctx) {
    SchemaNodeNotFlat(branch = schema_flat__node(x@branch, ctx), desc = x@desc)
}

S7::method(schema_flat__node, SchemaNode) <- function(x, ctx) {
    stop("unsupported authoring schema node.", call. = FALSE)
}

S7::method(schema_flat__node, S7::class_any) <- function(x, ctx) {
    stop("unsupported authoring schema node.", call. = FALSE)
}

schema_flat__compile <- S7::new_generic("schema_flat__compile", "x", function(x) S7::S7_dispatch())

S7::method(schema_flat__compile, SchemaFlat) <- function(x) {
    x
}

S7::method(schema_flat__compile, SchemaNodeLeaf) <- function(x) {
    SchemaFlat(root = x)
}

S7::method(schema_flat__compile, SchemaNodeContainerFlat) <- function(x) {
    SchemaFlat(root = x)
}

S7::method(schema_flat__compile, SchemaNodeAllFlat) <- function(x) {
    SchemaFlat(root = x)
}

S7::method(schema_flat__compile, SchemaNodeAnyFlat) <- function(x) {
    SchemaFlat(root = x)
}

S7::method(schema_flat__compile, SchemaNodeOneFlat) <- function(x) {
    SchemaFlat(root = x)
}

S7::method(schema_flat__compile, SchemaNodeNotFlat) <- function(x) {
    SchemaFlat(root = x)
}

S7::method(schema_flat__compile, SchemaDoc) <- function(x) {
    schema_flat__doc(x)
}

S7::method(schema_flat__compile, S7::class_any) <- function(x) {
    stop(
        paste(
            "`schema_flatten()` only accepts a schema document, raw schema DSL list, `SchemaFlat`, or flat runtime `SchemaNode`.",
            "Use `schema_flat__node()` internally for authoring `SchemaNode` values."
        ),
        call. = FALSE
    )
}
# }}}


# -------------------- schema-infer.R
#' @noRd
NULL

#' Infer a conservative schema from example data
#'
#' `schema_infer()` builds a `SchemaDoc` from example data using conservative,
#' structural inference only. It infers base/container check kinds and nested
#' field structure, but does not guess higher-level authoring constructs such as
#' `$defs`, `$ref`, `keys`, `groups`, or combinators.
#'
#' To parse an existing schema DSL document, use `schema_doc()` or
#' `schema_read()` instead.
#'
#' @param x Example data to infer from.
#' @param version Optional schema document version string.
#' @param keys Strategy for inferring optional `keys` rules from observed names.
#'   Use `"none"` to skip names-rule inference, `"named"` to require named
#'   inputs, `"required"` to require the observed names to be present, or
#'   `"exact"` to require the observed names in the observed order.
#' @param arrays Strategy for inferring unnamed lists. Use `"none"` to keep
#'   unnamed lists generic, or `"rest"` to infer them as unnamed containers whose
#'   observed element schemas are stored in `rest`.
#'
#' @return A `SchemaDoc` inferred from `x`.
#'
#' @examples
#' payload <- list(items = list(list(id = 1L), list(id = 2L)))
#' schema_infer(payload, keys = "named", arrays = "rest")
#'
#' @noRd
schema_infer <- function(
    x,
    version = NULL,
    keys = c("none", "named", "required", "exact"),
    arrays = c("none", "rest")
) {
    checkmate::assert_string(version, null.ok = TRUE)
    keys <- checkmate::matchArg(keys, c("none", "named", "required", "exact"))
    arrays <- checkmate::matchArg(arrays, c("none", "rest"))

    if (S7::S7_inherits(x, SchemaDoc)) {
        if (!is.null(version) || !identical(keys, "none") || !identical(arrays, "none")) {
            stop("`version`, `keys`, and `arrays` cannot be supplied when `x` is already a `SchemaDoc`.", call. = FALSE)
        }
        return(x)
    }

    if (identical(arrays, "none") && keys %in% c("required", "exact") && schema_infer__has_unnamed_list(x)) {
        stop(sprintf("`keys = '%s'` requires named elements.", keys), call. = FALSE)
    }

    root <- schema_infer__node(x, keys = keys, arrays = arrays)
    doc <- list()
    if (!is.null(version)) {
        doc$version <- version
    }
    doc <- c(doc, root)

    schema_doc(doc)
}

schema_infer__kind <- function(kind, label = kind) {
    if (!kind %in% SCHEMA_SPEC_KINDS) {
        stop(
            sprintf(
                "`schema_infer()` cannot infer `%s` because check kind `%s` is not supported.",
                label,
                kind
            ),
            call. = FALSE
        )
    }

    kind
}

schema_infer__keys_rule <- function(x, keys) {
    if (identical(keys, "none")) {
        return(NULL)
    }

    if (!schema_infer__has_named_elements(x)) {
        return(NULL)
    }

    nms <- names(x)
    switch(
        keys,
        named = list(type = "named"),
        required = list(type = "named", must.include = nms),
        exact = list(identical.to = nms),
        NULL
    )
}

schema_infer__check_node <- function(kind, fields = NULL, keys_rule = NULL, rest = NULL) {
    out <- list(check = list(kind = schema_infer__kind(kind)))
    if (!is.null(keys_rule) && length(keys_rule)) {
        out$keys <- keys_rule
    }
    if (!is.null(fields) && length(fields)) {
        out$fields <- fields
    }
    if (!is.null(rest) && length(rest)) {
        out$rest <- rest
    }
    out
}

schema_infer__has_named_elements <- function(x) {
    length(x) > 0L && isTRUE(checkmate::check_names(names(x), type = "named"))
}

schema_infer__has_unnamed_elements <- function(x) {
    length(x) > 0L && is.null(names(x))
}

schema_infer__has_unnamed_list <- function(x) {
    is.list(x) && schema_infer__has_unnamed_elements(x)
}

schema_infer__is_unnamed_list <- function(x) {
    is.list(x) && is.null(names(x))
}

schema_infer__dedupe_nodes <- function(x) {
    if (!length(x)) {
        return(x)
    }

    x[!duplicated(x)]
}

schema_infer__array_rest <- function(x, keys, arrays) {
    if (!length(x)) {
        return(NULL)
    }

    nodes <- schema_infer__dedupe_nodes(lapply(x, schema_infer__node, keys = keys, arrays = arrays))
    if (length(nodes) == 1L) {
        return(nodes[[1L]])
    }

    list(any = nodes)
}

schema_infer__fields <- function(x, keys, arrays) {
    if (schema_infer__has_named_elements(x)) {
        nms <- names(x)
        return(stats::setNames(
            lapply(nms, function(name) schema_infer__node(x[[name]], keys = keys, arrays = arrays)),
            nms
        ))
    }

    NULL
}

schema_infer__atomic_kind <- function(x) {
    if (inherits(x, "Date")) {
        return(schema_infer__kind("date", label = class(x)[[1L]]))
    }

    if (inherits(x, "POSIXct")) {
        return(schema_infer__kind("POSIXct", label = class(x)[[1L]]))
    }

    if (is.factor(x)) {
        return(schema_infer__kind("factor"))
    }

    if (is.logical(x)) {
        return(schema_infer__kind(if (length(x) == 1L && !is.na(x)) "flag" else "logical"))
    }

    if (is.integer(x)) {
        return(schema_infer__kind(if (length(x) == 1L && !is.na(x)) "int" else "integer"))
    }

    if (is.double(x)) {
        return(schema_infer__kind(if (length(x) == 1L && !is.na(x)) "number" else "numeric"))
    }

    if (is.character(x)) {
        return(schema_infer__kind(if (length(x) == 1L && !is.na(x)) "string" else "character"))
    }

    if (is.complex(x)) {
        return(schema_infer__kind("complex"))
    }

    if (is.raw(x)) {
        return(schema_infer__kind("raw"))
    }

    NULL
}

schema_infer__node <- function(x, keys = "none", arrays = "none") {
    if (inherits(x, "data.table")) {
        return(schema_infer__check_node(
            "data_table",
            fields = schema_infer__fields(x, keys = keys, arrays = arrays),
            keys_rule = schema_infer__keys_rule(x, keys)
        ))
    }

    if (inherits(x, c("tbl_df", "tbl"))) {
        return(schema_infer__check_node(
            "tibble",
            fields = schema_infer__fields(x, keys = keys, arrays = arrays),
            keys_rule = schema_infer__keys_rule(x, keys)
        ))
    }

    if (is.data.frame(x)) {
        return(schema_infer__check_node(
            "data_frame",
            fields = schema_infer__fields(x, keys = keys, arrays = arrays),
            keys_rule = schema_infer__keys_rule(x, keys)
        ))
    }

    if (is.null(x)) {
        return(schema_infer__check_node("null"))
    }

    if (is.list(x)) {
        if (identical(arrays, "rest") && schema_infer__is_unnamed_list(x)) {
            return(schema_infer__check_node(
                "list",
                keys_rule = list(type = "unnamed"),
                rest = schema_infer__array_rest(x, keys = keys, arrays = arrays)
            ))
        }

        return(schema_infer__check_node(
            "list",
            fields = schema_infer__fields(x, keys = keys, arrays = arrays),
            keys_rule = schema_infer__keys_rule(x, keys)
        ))
    }

    atomic_kind <- schema_infer__atomic_kind(x)
    if (!is.null(atomic_kind)) {
        return(schema_infer__check_node(atomic_kind, keys_rule = schema_infer__keys_rule(x, keys)))
    }

    stop(
        sprintf(
            paste(
                "`schema_infer()` does not support objects of class {%s}.",
                "Please construct the schema manually with authoring helpers."
            ),
            paste(class(x), collapse = ", ")
        ),
        call. = FALSE
    )
}


# -------------------- schema-json.R
schema_json__read_json <- function(txt) {
    schema_utils__require_namespace("jsonlite", "read schema JSON")
    jsonlite::fromJSON(txt, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}

#' Read and write schema JSON
#'
#' `schema_read()` reads schema JSON into a `SchemaDoc`. `schema_write()`
#' serializes schema objects to schema JSON. Both functions require the
#' suggested package `jsonlite`.
#'
#' @param txt JSON text, local file path, or URL accepted by
#'   `jsonlite::fromJSON()`.
#' @param x A schema object accepted by `as.list()`, usually a `SchemaDoc`.
#' @param path Output file path.
#' @param overwrite Whether an existing output file may be overwritten.
#' @param pretty Whether JSON output should be pretty-printed.
#' @param auto_unbox Passed to `jsonlite::write_json()`.
#'
#' @return `schema_read()` returns a `SchemaDoc`. `schema_write()` invisibly
#'   returns `path`.
#'
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' schema <- schema_infer(list(id = 1L))
#' schema
#'
#' path <- tempfile(fileext = ".json")
#' schema_write(schema, path)
#'
#' schema_read(path)
#'
#' @noRd
schema_write <- function(x, path, overwrite = FALSE, pretty = TRUE, auto_unbox = TRUE) {
    schema_utils__require_namespace("jsonlite", "write schema JSON")
    checkmate::assert_string(path, min.chars = 1L)
    checkmate::assert_flag(overwrite)
    if (!overwrite && file.exists(path)) {
        stop(sprintf("File already exists: %s", path), call. = FALSE)
    }

    jsonlite::write_json(
        as.list(x),
        path,
        pretty = pretty,
        auto_unbox = auto_unbox,
        null = "null",
        na = "null"
    )

    invisible(path)
}

#' @noRd
schema_read <- function(txt) {
    json <- schema_json__read_json(txt)
    path <- NULL
    if (checkmate::test_string(txt) && !jsonlite::validate(txt)) {
        if (grepl("^https?://", txt, useBytes = TRUE) || file.exists(txt)) {
            path <- txt
        }
    }
    schema_doc(json, path)
}

# -------------------- schema-query.R

schema_query__is_node <- function(x) {
    S7::S7_inherits(x, SchemaNode)
}

schema_query__collect_node <- S7::new_generic(
    "schema_query__collect_node",
    "node",
    function(node, path, emit) S7::S7_dispatch()
)

schema_query__collect_terminal <- function(node, path, emit) {
    emit(path, node)
}

S7::method(schema_query__collect_node, SchemaNodeLeaf) <- schema_query__collect_terminal
S7::method(schema_query__collect_node, SchemaNodeRef) <- schema_query__collect_terminal

schema_query__collect_container <- function(node, path, emit) {
    emit(path, node)

    for (binding in node@exact) {
        for (key in binding@keys) {
            schema_query__collect_node(
                binding@target,
                schema_edit__path_append_field(path, key),
                emit
            )
        }
    }

    for (binding in node@patterns) {
        pattern_path <- schema_edit__path_append(path, "patterns", binding@pattern)
        schema_query__collect_node(binding@target, pattern_path, emit)
    }

    for (i in seq_along(node@positions)) {
        schema_query__collect_node(
            node@positions[[i]],
            schema_edit__path_append_index(path, "positions", i),
            emit
        )
    }

    if (!is.null(node@rest)) {
        schema_query__collect_node(node@rest, schema_edit__path_append(path, "rest"), emit)
    }

    invisible(NULL)
}

S7::method(schema_query__collect_node, SchemaNodeContainerCmpt) <- schema_query__collect_container
S7::method(schema_query__collect_node, SchemaNodeContainerFlat) <- schema_query__collect_container

schema_query__collect_nary <- function(node, path, emit) {
    emit(path, node)

    operator <- schema_query__nary_operator(node)
    for (i in seq_along(node@branches)) {
        schema_query__collect_node(
            node@branches[[i]],
            schema_edit__path_append_index(path, operator, i),
            emit
        )
    }

    invisible(NULL)
}

S7::method(schema_query__collect_node, SchemaNodeNaryCmpt) <- schema_query__collect_nary
S7::method(schema_query__collect_node, SchemaNodeNaryFlat) <- schema_query__collect_nary

schema_query__collect_not <- function(node, path, emit) {
    emit(path, node)
    schema_query__collect_node(node@branch, schema_edit__path_append(path, "not"), emit)
    invisible(NULL)
}

S7::method(schema_query__collect_node, SchemaNodeNotCmpt) <- schema_query__collect_not
S7::method(schema_query__collect_node, SchemaNodeNotFlat) <- schema_query__collect_not

schema_query__nary_operator <- function(node) {
    if (S7::S7_inherits(node, SchemaNodeAllCmpt) || S7::S7_inherits(node, SchemaNodeAllFlat)) {
        "all"
    } else if (S7::S7_inherits(node, SchemaNodeAnyCmpt) || S7::S7_inherits(node, SchemaNodeAnyFlat)) {
        "any"
    } else if (S7::S7_inherits(node, SchemaNodeOneCmpt) || S7::S7_inherits(node, SchemaNodeOneFlat)) {
        "one"
    } else {
        stop("Unsupported n-ary schema node type.", call. = FALSE)
    }
}

schema_query__walk_doc <- function(doc, defs, emit) {
    schema_query__collect_node(doc@root, "$", emit)
    if (defs && length(doc@defs)) {
        for (name in names(doc@defs)) {
            schema_query__collect_node(
                doc@defs[[name]],
                schema_edit__path_append("$", "defs", name),
                emit
            )
        }
    }

    invisible(NULL)
}

schema_query__walk <- function(x, defs = TRUE, emit) {
    checkmate::assert_flag(defs)

    if (S7::S7_inherits(x, SchemaDoc)) {
        return(schema_query__walk_doc(x, defs = defs, emit = emit))
    }

    if (S7::S7_inherits(x, SchemaFlat)) {
        schema_query__collect_node(x@root, "$", emit)
        return(invisible(NULL))
    }

    if (schema_query__is_node(x)) {
        schema_query__collect_node(x, "$", emit)
        return(invisible(NULL))
    }

    schema_query__walk_doc(schema_doc(x), defs = defs, emit = emit)
}

schema_query__assert_predicate_result <- function(result, path) {
    if (!is.logical(result) || length(result) != 1L || is.na(result)) {
        stop(sprintf("`where` must return a single TRUE or FALSE at path `%s`.", path), call. = FALSE)
    }
    result
}

schema_query__call_where <- function(where, path, node) {
    result <- tryCatch(
        where(path, node),
        error = function(e) {
            stop(
                sprintf("Failed to evaluate `where` at path `%s`.\n%s", path, conditionMessage(e)),
                call. = FALSE
            )
        }
    )
    schema_query__assert_predicate_result(result, path)
}

schema_query__call_fn <- function(fn, path, node) {
    tryCatch(
        fn(path, node),
        error = function(e) {
            stop(
                sprintf("Failed to modify schema node at path `%s`.\n%s", path, conditionMessage(e)),
                call. = FALSE
            )
        }
    )
}

schema_query__rewrite_state <- function() {
    new.env(parent = emptyenv())
}

schema_query__rewrite_result <- function(node, changed = FALSE) {
    list(node = node, changed = changed)
}

schema_query__rewrite_results_same_target <- function(results) {
    if (!length(results)) {
        return(TRUE)
    }

    first <- results[[1L]]$node
    all(vapply(
        results,
        function(result) schema_compact__same(first, result$node),
        logical(1L)
    ))
}

schema_query__rewrite_exact_binding <- function(binding, key_results, exact_constructor) {
    all_changed <- all(vapply(key_results, function(result) result$changed, logical(1L)))
    if (
        length(binding@keys) > 1L &&
            all_changed &&
            schema_query__rewrite_results_same_target(key_results)
    ) {
        return(list(exact_constructor(keys = binding@keys, target = key_results[[1L]]$node)))
    }

    out <- vector("list", length(binding@keys))
    for (i in seq_along(binding@keys)) {
        out[[i]] <- exact_constructor(
            keys = binding@keys[[i]],
            target = key_results[[i]]$node
        )
    }
    out
}

schema_query__compile_flat_replacement <- function(node, path, context) {
    if (schema_flat__node_is_flat(node)) {
        return(node)
    }

    tryCatch(
        schema_flat__compile(SchemaDoc(root = node, defs = list()))@root,
        error = function(e) {
            schema_edit__abort_with_context(context, e)
        }
    )
}

schema_query__dynamic_replacer <- function(fn) {
    function(path, node, defs, flat) {
        value <- schema_query__call_fn(fn, path, node)
        context <- sprintf("Invalid replacement at path `%s`.", path)
        replacement <- schema_edit__as_node(
            value,
            defs = defs,
            path = path,
            context = context
        )

        if (flat) {
            return(schema_query__compile_flat_replacement(replacement, path = path, context = context))
        }

        replacement
    }
}

schema_query__constant_replacer <- function(value) {
    state <- new.env(parent = emptyenv())
    state$value <- value
    state$has_node <- FALSE
    state$node <- NULL
    state$has_flat_node <- FALSE
    state$flat_node <- NULL

    function(path, node, defs, flat) {
        context <- sprintf("Invalid replacement at path `%s`.", path)
        if (!state$has_node) {
            state$node <- schema_edit__as_node(
                state$value,
                defs = defs,
                path = path,
                context = context
            )
            state$has_node <- TRUE
        }

        if (!flat) {
            return(state$node)
        }

        if (!state$has_flat_node) {
            state$flat_node <- schema_query__compile_flat_replacement(
                state$node,
                path = path,
                context = context
            )
            state$has_flat_node <- TRUE
        }

        state$flat_node
    }
}

schema_query__replace_current <- function(node, path, replacer, defs, flat) {
    replacer(path = path, node = node, defs = defs, flat = flat)
}

schema_query__rewrite_apply <- function(x, where, replacer, defs, missing) {
    result <- schema_query__rewrite_input(x, where, replacer, defs = defs)
    if (!result$count && identical(missing, "error")) {
        stop("`where` did not match any schema paths.", call. = FALSE)
    }

    result$value
}

schema_query__check_match <- function(where, path, node, state, ancestor) {
    matched <- schema_query__call_where(where, path, node)
    if (matched && !is.null(ancestor)) {
        stop(
            sprintf(
                "`where` matched both ancestor path `%s` and descendant path `%s`; please narrow the selector.",
                ancestor,
                path
            ),
            call. = FALSE
        )
    }
    if (matched) {
        state$count <- state$count + 1L
    }
    matched
}

schema_query__rewrite_node <- S7::new_generic(
    "schema_query__rewrite_node",
    "node",
    function(node, path, where, replacer, defs, state, ancestor = NULL, flat = FALSE) S7::S7_dispatch()
)

schema_query__rewrite_terminal <- function(
    node,
    path,
    where,
    replacer,
    defs,
    state,
    ancestor = NULL,
    flat = FALSE
) {
    matched <- schema_query__check_match(where, path, node, state, ancestor)
    if (matched) {
        return(schema_query__rewrite_result(
            schema_query__replace_current(node, path, replacer, defs, flat = flat),
            changed = TRUE
        ))
    }

    schema_query__rewrite_result(node)
}

S7::method(schema_query__rewrite_node, SchemaNodeLeaf) <- schema_query__rewrite_terminal
S7::method(schema_query__rewrite_node, SchemaNodeRef) <- schema_query__rewrite_terminal

schema_query__rewrite_container <- function(
    node,
    path,
    where,
    replacer,
    defs,
    state,
    ancestor,
    flat,
    exact_constructor,
    pattern_constructor
) {
    matched <- schema_query__check_match(where, path, node, state, ancestor)
    next_ancestor <- if (matched) path else ancestor

    exact <- list()
    exact_changed <- FALSE
    for (binding in node@exact) {
        key_results <- lapply(binding@keys, function(key) {
            schema_query__rewrite_node(
                binding@target,
                schema_edit__path_append_field(path, key),
                where = where,
                replacer = replacer,
                defs = defs,
                state = state,
                ancestor = next_ancestor,
                flat = flat
            )
        })
        binding_changed <- any(vapply(key_results, function(result) result$changed, logical(1L)))
        exact_changed <- exact_changed || binding_changed

        if (binding_changed) {
            rewritten <- schema_query__rewrite_exact_binding(binding, key_results, exact_constructor)
            for (new_binding in rewritten) {
                exact[[length(exact) + 1L]] <- new_binding
            }
        } else {
            exact[[length(exact) + 1L]] <- binding
        }
    }

    patterns <- node@patterns
    patterns_changed <- FALSE
    if (length(patterns)) {
        for (i in seq_along(patterns)) {
            binding <- patterns[[i]]
            result <- schema_query__rewrite_node(
                binding@target,
                schema_edit__path_append(path, "patterns", binding@pattern),
                where = where,
                replacer = replacer,
                defs = defs,
                state = state,
                ancestor = next_ancestor,
                flat = flat
            )
            if (result$changed) {
                patterns[[i]] <- pattern_constructor(pattern = binding@pattern, target = result$node)
                patterns_changed <- TRUE
            }
        }
    }

    positions <- node@positions
    positions_changed <- FALSE
    if (length(positions)) {
        for (i in seq_along(positions)) {
            result <- schema_query__rewrite_node(
                positions[[i]],
                schema_edit__path_append_index(path, "positions", i),
                where = where,
                replacer = replacer,
                defs = defs,
                state = state,
                ancestor = next_ancestor,
                flat = flat
            )
            if (result$changed) {
                positions[[i]] <- result$node
                positions_changed <- TRUE
            }
        }
    }

    rest <- node@rest
    rest_changed <- FALSE
    if (!is.null(rest)) {
        result <- schema_query__rewrite_node(
            rest,
            schema_edit__path_append(path, "rest"),
            where = where,
            replacer = replacer,
            defs = defs,
            state = state,
            ancestor = next_ancestor,
            flat = flat
        )
        if (result$changed) {
            rest <- result$node
            rest_changed <- TRUE
        }
    }

    if (matched) {
        return(schema_query__rewrite_result(
            schema_query__replace_current(node, path, replacer, defs, flat = flat),
            changed = TRUE
        ))
    }

    if (exact_changed || patterns_changed || positions_changed || rest_changed) {
        node <- schema_edit__update_node(
            node,
            exact = exact,
            patterns = patterns,
            positions = positions,
            rest = rest
        )
        return(schema_query__rewrite_result(node, changed = TRUE))
    }

    schema_query__rewrite_result(node)
}

schema_query__rewrite_container_method <- function(
    node,
    path,
    where,
    replacer,
    defs,
    state,
    ancestor = NULL,
    flat = FALSE
) {
    flat <- schema_flat__node_is_flat(node)
    exact_constructor <- if (flat) SchemaBindingExactFlat else SchemaBindingExactCmpt
    pattern_constructor <- if (flat) SchemaBindingPatternFlat else SchemaBindingPatternCmpt

    schema_query__rewrite_container(
        node,
        path = path,
        where = where,
        replacer = replacer,
        defs = defs,
        state = state,
        ancestor = ancestor,
        flat = flat,
        exact_constructor = exact_constructor,
        pattern_constructor = pattern_constructor
    )
}

S7::method(schema_query__rewrite_node, SchemaNodeContainerCmpt) <- schema_query__rewrite_container_method
S7::method(schema_query__rewrite_node, SchemaNodeContainerFlat) <- schema_query__rewrite_container_method

schema_query__rewrite_nary <- function(
    node,
    path,
    where,
    replacer,
    defs,
    state,
    ancestor = NULL,
    flat = FALSE
) {
    flat <- schema_flat__node_is_flat(node)
    matched <- schema_query__check_match(where, path, node, state, ancestor)
    next_ancestor <- if (matched) path else ancestor
    operator <- schema_query__nary_operator(node)

    branches <- node@branches
    changed <- FALSE
    for (i in seq_along(branches)) {
        result <- schema_query__rewrite_node(
            branches[[i]],
            schema_edit__path_append_index(path, operator, i),
            where = where,
            replacer = replacer,
            defs = defs,
            state = state,
            ancestor = next_ancestor,
            flat = flat
        )
        if (result$changed) {
            branches[[i]] <- result$node
            changed <- TRUE
        }
    }

    if (matched) {
        return(schema_query__rewrite_result(
            schema_query__replace_current(node, path, replacer, defs, flat = flat),
            changed = TRUE
        ))
    }

    if (changed) {
        return(schema_query__rewrite_result(schema_edit__update_node(node, branches = branches), changed = TRUE))
    }

    schema_query__rewrite_result(node)
}

S7::method(schema_query__rewrite_node, SchemaNodeNaryCmpt) <- schema_query__rewrite_nary
S7::method(schema_query__rewrite_node, SchemaNodeNaryFlat) <- schema_query__rewrite_nary

schema_query__rewrite_not <- function(
    node,
    path,
    where,
    replacer,
    defs,
    state,
    ancestor = NULL,
    flat = FALSE
) {
    flat <- schema_flat__node_is_flat(node)
    matched <- schema_query__check_match(where, path, node, state, ancestor)
    next_ancestor <- if (matched) path else ancestor

    result <- schema_query__rewrite_node(
        node@branch,
        schema_edit__path_append(path, "not"),
        where = where,
        replacer = replacer,
        defs = defs,
        state = state,
        ancestor = next_ancestor,
        flat = flat
    )

    if (matched) {
        return(schema_query__rewrite_result(
            schema_query__replace_current(node, path, replacer, defs, flat = flat),
            changed = TRUE
        ))
    }

    if (result$changed) {
        return(schema_query__rewrite_result(schema_edit__update_node(node, branch = result$node), changed = TRUE))
    }

    schema_query__rewrite_result(node)
}

S7::method(schema_query__rewrite_node, SchemaNodeNotCmpt) <- schema_query__rewrite_not
S7::method(schema_query__rewrite_node, SchemaNodeNotFlat) <- schema_query__rewrite_not

schema_query__rewrite_doc <- function(doc, where, replacer, include_defs) {
    state <- schema_query__rewrite_state()
    state$count <- 0L
    defs <- names(doc@defs)

    root <- schema_query__rewrite_node(
        doc@root,
        path = "$",
        where = where,
        replacer = replacer,
        defs = defs,
        state = state,
        flat = FALSE
    )

    defs_list <- doc@defs
    defs_changed <- FALSE
    if (include_defs && length(defs_list)) {
        for (name in names(defs_list)) {
            result <- schema_query__rewrite_node(
                defs_list[[name]],
                path = schema_edit__path_append("$", "defs", name),
                where = where,
                replacer = replacer,
                defs = defs,
                state = state,
                flat = FALSE
            )
            if (result$changed) {
                defs_list[[name]] <- result$node
                defs_changed <- TRUE
            }
        }
    }

    list(
        value = if (root$changed || defs_changed) {
            schema_edit__update_node(doc, root = root$node, defs = defs_list)
        } else {
            doc
        },
        count = state$count
    )
}

schema_query__rewrite_flat <- function(flat, where, replacer) {
    state <- schema_query__rewrite_state()
    state$count <- 0L

    root <- schema_query__rewrite_node(
        flat@root,
        path = "$",
        where = where,
        replacer = replacer,
        defs = character(),
        state = state,
        flat = TRUE
    )

    list(
        value = if (root$changed) {
            schema_edit__update_node(flat, root = root$node)
        } else {
            flat
        },
        count = state$count
    )
}

schema_query__rewrite_bare_node <- function(node, where, replacer) {
    state <- schema_query__rewrite_state()
    state$count <- 0L
    flat <- schema_flat__node_is_flat(node)

    result <- schema_query__rewrite_node(
        node,
        path = "$",
        where = where,
        replacer = replacer,
        defs = character(),
        state = state,
        flat = flat
    )

    list(
        value = result$node,
        count = state$count
    )
}

schema_query__rewrite_input <- function(x, where, replacer, defs) {
    if (S7::S7_inherits(x, SchemaDoc)) {
        return(schema_query__rewrite_doc(x, where = where, replacer = replacer, include_defs = defs))
    }

    if (S7::S7_inherits(x, SchemaFlat)) {
        return(schema_query__rewrite_flat(x, where = where, replacer = replacer))
    }

    if (schema_query__is_node(x)) {
        return(schema_query__rewrite_bare_node(x, where = where, replacer = replacer))
    }

    schema_query__rewrite_doc(schema_doc(x), where = where, replacer = replacer, include_defs = defs)
}

#' Query schema paths and matching nodes
#'
#' `schema_paths()` lists editable logical schema paths. `schema_find()` returns
#' paths whose schema node satisfies a predicate.
#'
#' Logical paths describe fields as users see them in the validated data. Grouped
#' fields are expanded to one path per field.
#'
#' @param x A schema document or raw schema DSL list.
#' @param defs Whether to include root `$defs` entries.
#' @param where Predicate function with signature `function(path, node)`.
#'
#' @return A character vector of schema paths.
#'
#' @examples
#' schema <- schema_compact(schema_infer(list(
#'     issued = list(`date-parts` = list(list(2024L))),
#'     created = list(`date-parts` = list(list(2024L)))
#' ), arrays = "rest"))
#' schema
#'
#' schema_find(schema, schema_where_path("(^|\\$)`date-parts`\\$rest$"))
#' schema_find(schema, schema_where_check("int"))
#'
#' @noRd
schema_paths <- function(x, defs = TRUE) {
    paths <- character()
    schema_query__walk(x, defs = defs, emit = function(path, node) {
        paths[[length(paths) + 1L]] <<- path
        invisible(NULL)
    })
    paths
}

#' @noRd
schema_find <- function(x, where, defs = TRUE) {
    checkmate::assert_function(where)
    paths <- character()
    schema_query__walk(x, defs = defs, emit = function(path, node) {
        if (schema_query__call_where(where, path, node)) {
            paths[[length(paths) + 1L]] <<- path
        }
        invisible(NULL)
    })
    paths
}

#' Modify schema nodes selected by a predicate
#'
#' `schema_modify_where()` modifies every schema node matched by `where`.
#' `schema_replace_where()` is a convenience wrapper that replaces all matched
#' nodes with the same schema fragment.
#'
#' Batch edits operate on logical paths. Editing every path inside a grouped
#' schema field preserves the group when the replacement targets are structurally
#' equivalent; partial edits or differing replacement targets split the group
#' into per-field bindings. If `where` matches both a node and one of its
#' descendants in the same call, the edit errors and asks you to narrow the
#' selector.
#'
#' @param x A schema document or raw schema DSL list.
#' @param where Predicate function with signature `function(path, node)`.
#' @param fn Function with signature `function(path, node)` returning a schema
#'   fragment or `SchemaNode`.
#' @param value Replacement schema fragment or `SchemaNode`.
#' @param defs Whether to include root `$defs` entries.
#' @param missing Missing-match behavior. Use `"error"` to raise an error when
#'   `where` matches no paths or `"ignore"` to leave the schema unchanged.
#'
#' @return A modified `SchemaDoc`.
#'
#' @examples
#' schema <- schema_compact(schema_infer(list(
#'     issued = list(`date-parts` = list(list(2024L))),
#'     created = list(`date-parts` = list(list(2024L)))
#' ), arrays = "rest"))
#' schema <- schema_add_def(schema, "year", schema_check("int", lower = 0))
#' schema
#'
#' schema_find(schema, schema_where_path("(^|\\$)`date-parts`\\$rest$"))
#'
#' schema <- schema_replace_where(
#'     schema,
#'     schema_where_path("(^|\\$)`date-parts`\\$rest$"),
#'     schema_ref("year")
#' )
#' schema
#'
#' @noRd
schema_modify_where <- function(x, where, fn, defs = TRUE, missing = "ignore") {
    checkmate::assert_function(where)
    checkmate::assert_function(fn)
    checkmate::assert_flag(defs)
    missing <- match.arg(missing, c("error", "ignore"))

    schema_query__rewrite_apply(
        x,
        where = where,
        replacer = schema_query__dynamic_replacer(fn),
        defs = defs,
        missing = missing
    )
}

#' @noRd
schema_replace_where <- function(x, where, value, defs = TRUE, missing = "ignore") {
    force(value)
    checkmate::assert_function(where)
    checkmate::assert_flag(defs)
    missing <- match.arg(missing, c("error", "ignore"))

    schema_query__rewrite_apply(
        x,
        where = where,
        replacer = schema_query__constant_replacer(value),
        defs = defs,
        missing = missing
    )
}

#' Create schema query predicates
#'
#' `schema_where_path()` matches logical schema paths. `schema_where_check()`
#' matches check nodes by kind.
#'
#' @details
#' `schema_where_check()` matches check nodes present in the schema tree being
#' walked. It does not resolve `$ref` targets while querying an authoring
#' `SchemaDoc`; use `schema_flatten()` first if a query should see referenced
#' definitions through the flattened schema.
#'
#' @param pattern Pattern passed to `grepl()` for matching schema paths.
#' @param fixed Whether `pattern` should be matched literally.
#' @param kind Optional check kind to match, such as `"list"` or `"int"`.
#'
#' @return A predicate function with signature `function(path, node)`.
#'
#' @examples
#' by_path <- schema_where_path("(^|\\$)`date-parts`\\$rest$")
#' by_int <- schema_where_check("int")
#'
#' schema <- schema_infer(list(id = 1L))
#' schema
#'
#' schema_find(schema, by_int)
#'
#' @noRd
schema_where_path <- function(pattern, fixed = FALSE) {
    checkmate::assert_string(pattern, min.chars = 1L)
    checkmate::assert_flag(fixed)

    function(path, node) {
        grepl(pattern, path, fixed = fixed, useBytes = TRUE)
    }
}

#' @noRd
schema_where_check <- function(kind = NULL) {
    checkmate::assert_string(kind, min.chars = 1L, null.ok = TRUE)
    if (!is.null(kind) && !kind %in% SCHEMA_SPEC_KINDS) {
        stop(sprintf("Unsupported check kind `%s`.", kind), call. = FALSE)
    }

    function(path, node) {
        S7::S7_inherits(node, SchemaNodeCheck) && (is.null(kind) || identical(node@value@kind, kind))
    }
}


# -------------------- schema-validate.R
# schema_validate {{{
#' Validate input against a schema
#'
#' `schema_validate()` validates an R object against a `SchemaDoc`,
#' `SchemaFlat`, or flattened schema node. When validating many inputs against
#' the same schema, flatten it once with `schema_flatten()` and reuse the
#' flattened schema.
#'
#' @param schema A `SchemaDoc`, `SchemaFlat`, or flattened schema node.
#' @param x Input object to validate.
#' @param mode One of `"assert"`, `"check"`, `"test"`, or `"expect"`.
#' @param name Optional display name used in validation messages.
#' @param ... Reserved for future extension.
#'
#' @return In `"assert"` mode, invisibly returns `x` or throws an error. In
#'   `"check"` mode, returns `TRUE` or a diagnostic string. In `"test"` mode,
#'   returns `TRUE` or `FALSE`. In `"expect"` mode, returns a testthat-style
#'   expectation object.
#'
#' @examples
#' schema <- schema_doc(list(
#'     check = list(kind = "list"),
#'     fields = list(id = list(check = list(kind = "int", lower = 1)))
#' ))
#' schema
#'
#' schema_validate(schema, list(id = 1L), mode = "test")
#' schema_validate(schema, list(id = 0L), mode = "check", name = "payload")
#'
#' flat <- schema_flatten(schema)
#' schema_validate(flat, list(id = 2L), mode = "test")
#'
#' @noRd
schema_validate <- S7::new_generic(
    "schema_validate",
    "schema",
    function(schema, x, mode = "assert", name = NULL, ...) S7::S7_dispatch()
)

schema_validate__or <- function(x, y) {
    if (is.null(x)) y else x
}

schema_validate__make_expectation <- function(ok, message = NULL) {
    structure(
        list(
            message = schema_validate__or(message, if (ok) "Validation passed." else "Validation failed."),
            srcref = NULL,
            trace = NULL,
            passed = ok
        ),
        class = if (ok) {
            c("expectation_success", "expectation", "condition")
        } else {
            c("expectation_failure", "expectation", "error", "condition")
        }
    )
}

schema_validate__prefix_message <- function(result, path) {
    if (isTRUE(result)) {
        return(TRUE)
    }

    sprintf("%s: %s", path, result)
}

schema_validate__call_check <- function(fun, x, args = list()) {
    do.call(fun, c(list(x), args))
}

schema_validate__field_path <- function(path, key) {
    paste0(path, "$", key)
}

schema_validate__item_path <- function(path, i) {
    sprintf("%s[[%d]]", path, i)
}

schema_validate__keys_type <- function(schema) {
    if (is.null(schema@name)) {
        return(NULL)
    }

    schema@name@args$type
}

schema_validate__container_has_keyed_children <- function(schema) {
    length(schema@exact) ||
        length(schema@patterns) ||
        !is.null(schema@rest)
}

schema_validate__branch_kind <- function(branch) {
    if (
        S7::S7_inherits(branch, SchemaNodeLeaf) ||
            S7::S7_inherits(branch, SchemaNodeContainerFlat)
    ) {
        return(branch@value@kind)
    }
    if (S7::S7_inherits(branch, SchemaNodeAllFlat)) {
        return("all")
    }
    if (S7::S7_inherits(branch, SchemaNodeAnyFlat)) {
        return("any")
    }
    if (S7::S7_inherits(branch, SchemaNodeOneFlat)) {
        return("one")
    }
    if (S7::S7_inherits(branch, SchemaNodeNotFlat)) {
        return("not")
    }

    "schema"
}

schema_validate__branch_label <- function(branch, i) {
    sprintf("[%d:%s]", i, schema_validate__branch_kind(branch))
}

schema_validate__branch_result <- function(branch, i, result) {
    sprintf("%s %s", schema_validate__branch_label(branch, i), result)
}

schema_validate__rule <- function(value, x, path) {
    schema_validate__prefix_message(
        schema_validate__call_check(
            schema_utils__checkmate_fun(value@kind),
            x,
            args = value@args
        ),
        path
    )
}

schema_validate__names_rule <- function(value, x, path) {
    schema_validate__prefix_message(
        schema_validate__call_check(checkmate::check_names, names(x), args = value@args),
        path
    )
}

schema_validate__impl <- S7::new_generic(
    "schema_validate__impl",
    "schema",
    function(schema, x, path) S7::S7_dispatch()
)

S7::method(schema_validate__impl, SchemaNodeLeaf) <- function(schema, x, path) {
    res <- schema_validate__rule(schema@value, x, path)
    if (!isTRUE(res)) {
        return(res)
    }

    if (!is.null(schema@name)) {
        return(schema_validate__names_rule(schema@name, x, path))
    }

    TRUE
}

S7::method(schema_validate__impl, SchemaNodeContainerFlat) <- function(schema, x, path) {
    res <- schema_validate__rule(schema@value, x, path)
    if (!isTRUE(res)) {
        return(res)
    }

    if (!is.null(schema@name)) {
        res <- schema_validate__names_rule(schema@name, x, path)
        if (!isTRUE(res)) {
            return(res)
        }
    }

    if (identical(schema_validate__keys_type(schema), "unnamed")) {
        if (length(schema@exact) || length(schema@patterns)) {
            return(sprintf("%s cannot use named field constraints with `keys$type = 'unnamed'`.", path))
        }

        n_positions <- length(schema@positions)
        n_present <- min(length(x), n_positions)

        if (n_present) {
            for (i in seq_len(n_present)) {
                res <- schema_validate__impl(schema@positions[[i]], x[[i]], schema_validate__item_path(path, i))
                if (!isTRUE(res)) {
                    return(res)
                }
            }
        }

        if (length(x) <= n_positions) {
            return(TRUE)
        }

        extra <- seq.int(n_positions + 1L, length(x))
        if (is.null(schema@rest)) {
            return(TRUE)
        }

        for (i in extra) {
            res <- schema_validate__impl(schema@rest, x[[i]], schema_validate__item_path(path, i))
            if (!isTRUE(res)) {
                return(res)
            }
        }

        return(TRUE)
    }

    raw_names <- names(x)
    if (length(x) && is.null(schema@name) && schema_validate__container_has_keyed_children(schema)) {
        if (!checkmate::test_character(raw_names, null.ok = FALSE, any.missing = FALSE, min.chars = 1L)) {
            return(sprintf(
                "%s must be a named object because this schema declares keyed child constraints.",
                path
            ))
        }
    }

    present <- schema_validate__or(raw_names, character())
    declared <- schema_flat__binding_names(schema@exact)
    present_pos <- match(declared, present, nomatch = 0L)

    for (i in seq_along(schema@exact)) {
        pos <- present_pos[[i]]
        if (!pos) {
            next
        }

        binding <- schema@exact[[i]]
        nm <- declared[[i]]
        res <- schema_validate__impl(binding@target, x[[pos]], schema_validate__field_path(path, nm))
        if (!isTRUE(res)) {
            return(res)
        }
    }

    extra <- setdiff(present, declared)
    pattern_matched <- character()
    for (nm in extra) {
        matched <- Filter(function(binding) grepl(binding@pattern, nm), schema@patterns)
        if (!length(matched)) {
            next
        }

        pattern_matched <- c(pattern_matched, nm)
        for (binding in matched) {
            res <- schema_validate__impl(binding@target, x[[nm]], schema_validate__field_path(path, nm))
            if (!isTRUE(res)) {
                return(res)
            }
        }
    }

    extra <- setdiff(extra, pattern_matched)
    if (!length(extra)) {
        return(TRUE)
    }

    if (is.null(schema@rest)) {
        return(TRUE)
    }

    for (nm in extra) {
        res <- schema_validate__impl(schema@rest, x[[nm]], schema_validate__field_path(path, nm))
        if (!isTRUE(res)) {
            return(res)
        }
    }

    TRUE
}

S7::method(schema_validate__impl, SchemaNodeAllFlat) <- function(schema, x, path) {
    for (i in seq_along(schema@branches)) {
        branch <- schema@branches[[i]]
        res <- schema_validate__impl(branch, x, path)
        if (!isTRUE(res)) {
            return(sprintf(
                "%s failed branch %s of `all`: %s",
                path,
                schema_validate__branch_label(branch, i),
                res
            ))
        }
    }

    TRUE
}

S7::method(schema_validate__impl, SchemaNodeAnyFlat) <- function(schema, x, path) {
    msgs <- character()
    for (i in seq_along(schema@branches)) {
        branch <- schema@branches[[i]]
        res <- schema_validate__impl(branch, x, path)
        if (isTRUE(res)) {
            return(TRUE)
        }
        msgs <- c(msgs, schema_validate__branch_result(branch, i, res))
    }

    sprintf(
        "%s failed `any` (0/%d branches matched): %s",
        path,
        length(schema@branches),
        paste(msgs, collapse = " | ")
    )
}

S7::method(schema_validate__impl, SchemaNodeOneFlat) <- function(schema, x, path) {
    ok <- 0L
    matched <- character()
    msgs <- character()
    for (i in seq_along(schema@branches)) {
        branch <- schema@branches[[i]]
        res <- schema_validate__impl(branch, x, path)
        if (isTRUE(res)) {
            ok <- ok + 1L
            matched <- c(matched, schema_validate__branch_label(branch, i))
        } else {
            msgs <- c(msgs, schema_validate__branch_result(branch, i, res))
        }
    }

    if (ok == 1L) {
        return(TRUE)
    }
    if (ok == 0L) {
        return(sprintf(
            "%s failed `one` (0/%d branches matched; expected exactly 1): %s",
            path,
            length(schema@branches),
            paste(msgs, collapse = " | ")
        ))
    }

    sprintf(
        "%s failed `one` (%d/%d branches matched; expected exactly 1): matched %s",
        path,
        ok,
        length(schema@branches),
        paste(matched, collapse = ", ")
    )
}

S7::method(schema_validate__impl, SchemaNodeNotFlat) <- function(schema, x, path) {
    res <- schema_validate__impl(schema@branch, x, path)
    if (isTRUE(res)) {
        return(sprintf(
            "%s matched forbidden branch %s of `not`.",
            path,
            schema_validate__branch_label(schema@branch, 1L)
        ))
    }

    TRUE
}

S7::method(schema_validate__impl, SchemaNode) <- function(schema, x, path) {
    stop("unsupported flattened schema node.", call. = FALSE)
}

S7::method(schema_validate__impl, S7::class_any) <- function(schema, x, path) {
    stop("unsupported flattened schema node.", call. = FALSE)
}

schema_validate__dispatch <- function(result, x, mode) {
    checkmate::assert_choice(mode, c("assert", "check", "test", "expect"))

    if (identical(mode, "check")) {
        return(result)
    }
    if (identical(mode, "test")) {
        return(isTRUE(result))
    }
    if (identical(mode, "expect")) {
        return(schema_validate__make_expectation(isTRUE(result), if (isTRUE(result)) NULL else result))
    }

    if (isTRUE(result)) {
        return(invisible(x))
    }
    stop(result, call. = FALSE)
}

S7::method(schema_validate, SchemaFlat) <- function(schema, x, mode = "assert", name = NULL, ...) {
    if (is.null(name)) {
        name <- deparse(substitute(x))
    }
    schema_validate__dispatch(schema_validate__impl(schema@root, x, name), x, mode)
}

S7::method(schema_validate, SchemaNodeLeaf) <- function(schema, x, mode = "assert", name = NULL, ...) {
    if (is.null(name)) {
        name <- deparse(substitute(x))
    }
    schema_validate__dispatch(schema_validate__impl(schema, x, name), x, mode)
}

S7::method(schema_validate, SchemaNodeContainerFlat) <- function(schema, x, mode = "assert", name = NULL, ...) {
    if (is.null(name)) {
        name <- deparse(substitute(x))
    }
    schema_validate__dispatch(schema_validate__impl(schema, x, name), x, mode)
}

S7::method(schema_validate, SchemaNodeAllFlat) <- function(schema, x, mode = "assert", name = NULL, ...) {
    if (is.null(name)) {
        name <- deparse(substitute(x))
    }
    schema_validate__dispatch(schema_validate__impl(schema, x, name), x, mode)
}

S7::method(schema_validate, SchemaNodeAnyFlat) <- function(schema, x, mode = "assert", name = NULL, ...) {
    if (is.null(name)) {
        name <- deparse(substitute(x))
    }
    schema_validate__dispatch(schema_validate__impl(schema, x, name), x, mode)
}

S7::method(schema_validate, SchemaNodeOneFlat) <- function(schema, x, mode = "assert", name = NULL, ...) {
    if (is.null(name)) {
        name <- deparse(substitute(x))
    }
    schema_validate__dispatch(schema_validate__impl(schema, x, name), x, mode)
}

S7::method(schema_validate, SchemaNodeNotFlat) <- function(schema, x, mode = "assert", name = NULL, ...) {
    if (is.null(name)) {
        name <- deparse(substitute(x))
    }
    schema_validate__dispatch(schema_validate__impl(schema, x, name), x, mode)
}

S7::method(schema_validate, SchemaDoc) <- function(schema, x, mode = "assert", name = NULL, ...) {
    schema_validate(schema_flat__compile(schema), x, mode = mode, name = name, ...)
}
# }}}

# nocov end
