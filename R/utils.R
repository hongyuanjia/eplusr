# console_width {{{
# Reference: `cli` (https://github.com/r-lib/cli)
console_width <- function() {
    width <- getOption(
        "cli.width",
        Sys.getenv("RSTUDIO_CONSOLE_WIDTH",
                   getOption("width", 80)
        )
    )

    return(as.integer(width))
}
# }}}
# `%||%` {{{
`%||%` <- function (x, y) {
    if (is.null(x)) {
        y
    } else {
        x
    }
}
# }}}
# char_count {{{
char_count <- function (x, pattern, ...) {
    nchar(as.character(x)) - nchar(gsub(pattern, "", x, ...))
}
# }}}

#' @importFrom stats na.omit
# avail_cols {{{
avail_cols <- function (x, table) {
    stats::na.omit(names(x)[match(table, names(x))])
}
# }}}
# sep_line {{{
sep_line <- function (char = "-", length = console_width()) {
    strrep(char, length)
}
# }}}
# msg {{{
msg <- function (..., prefix = " ", initial = "") {
    paste(strwrap(paste0(...)), collapse = "\n")
}
# }}}
# globalVariables {{{
id = output = extra_required = field_order = target_order = active = NULL
ori_value = output = output_class = output_count = output_diff = output_field = NULL
output_group = output_id = output_name = output_name_lower = output_order = NULL
output_ref = output_space = output_unit = output_value = ref_key = ref_value = NULL
reference = reference_class_name = required_field = required_object = right = NULL
row_id = set_value = slash_key = slash_key_value = slash_loc = slash_supported = NULL
slash_value = slash_value_upper = space_loc = special_key = special_loc = NULL
special_value = step = string = target_order = type = unique_object = NULL
unitsbasedonfield = value = value_count = value_fill = wrong = NULL
max_fields = object_id = num = group_order = class_order = edited = NULL
N = V1 = na.omit = NULL
autocalculatable = autosizable = call. = char = check_type = class_group = NULL
class_upper_case = colon_loc = default = explpt_loc = external_list = field = NULL
field_an = field_anid = field_count = field_id = group = leading_spaces = line = NULL
macro_key = max_fields = maximum = `maximum<` = min_fields = minimum = `minimum>` = NULL
new_value = num_fields = num_obj = object_list = NULL
# }}}
