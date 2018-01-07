#' @importFrom assertthat assert_that "on_failure<-"

# is_eplus_ver {{{
is_eplus_ver <- function (ver) {
    ver_fmt <- "[78]\\.[0-9]"
    grepl(ver_fmt, as.character(ver))
}

assertthat::on_failure(is_eplus_ver) <- function (call, env) {
    paste0(sQuote(eval(call$ver, env)), " is not a valid EnergyPlus version.")
}
# }}}
# is_supported_ver {{{
is_supported_ver <- function (ver) {
    supp_ver <- paste0("8.", 1:8)
    ver %in% supp_ver
}

assertthat::on_failure(is_supported_ver) <- function (call, env) {
    paste0("EnergyPlus version ", sQuote(eval(call$ver, env)), " is not supported yet.")
}
# }}}
# is_eplus_exists {{{
is_eplus_exists <- function (eplus_exe) {
    file.exists(eplus_exe)
}

assertthat::on_failure(is_eplus_exists) <- function (call, env) {
    paste0("EnergyPlus does not exist")
}
# }}}

# has_macro {{{1
has_macro <- function (str) {
    any(sapply(macro_dict, function(x) any(startsWith(str, x))))
}
# }}}1
# is_valid_id {{{
is_valid_id <- function (id, idf) {
    is_scalar(id) && is_integerish(id) && id %in% attr(idf, "id")
}

assertthat::on_failure(is_valid_id) <- function(call, env) {
    paste0(sQuote(eval(call$id, env)), " is not a valid object id. You can find all valid id using \"$all('id')\"")
}
# }}}
# is_comment_id {{{
is_comment_id <- function (id, idf) {
    is_scalar(id) && is_integerish(id) && id %in% idf$comment[, unique(object_id)]
}
# }}}
# is_valid_class {{{
is_valid_class <- function(class, idf) {
    is_string(class) && class %chin% valid_class(idf)
}

assertthat::on_failure(is_valid_class) <- function(call, env) {
    paste0(sQuote(eval(call$class, env)), " is not a valid class name. You can find all valid classes using \"$all('class')\"")
}
# }}}
# is_class_exist {{{
is_class_exist <- function (idf, class) {
    class_name <- class
    class_name %chin% valid_class(idf)
}

assertthat::on_failure(is_class_exist) <- function (call, env) {
    paste0(sQuote(eval(call$class, env)), " does not exist in current model")
}
# }}}
# can_be_duplicated {{{
can_be_duplicated <- function (class, idf) {
    class_name <- class
    assertthat::assert_that(is_string(class_name))
    !(is_valid_class(class_name, idf) &&
      idf$class[class == class_name, unique_object])
}

assertthat::on_failure(can_be_duplicated) <- function(call, env) {
    paste0(sQuote(eval(call$class, env)), " is an unique object and already exists")
}
# }}}
# can_be_deleted {{{
can_be_deleted <- function (class, idf) {
    class_name <- class
    assertthat::assert_that(is_string(class_name))
    !(is_valid_class(class_name, idf) &&
      (idf$class[class == class_name, unique_object] ||
       idf$class[class == class_name, required_object]))
}

assertthat::on_failure(can_be_deleted) <- function(call, env) {
    paste0(sQuote(eval(call$class, env)), " is an unique or required object that cannot be deleted")
}
# }}}
# can_be_modified {{{
can_be_modified <- function (class, idf) {
    class_name <- class
    assertthat::assert_that(is_valid_class(class_name, idf))
    !identical(class, "Version")
}
assertthat::on_failure(can_be_modified) <- function(call, env) {
    paste0(sQuote(eval(call$class, env)), " is protected and cannot be modified.")
}
# }}}
# not_deleted {{{
not_deleted <- function (id, idf) {
    !(id %in% attr(idf, "id_del"))
}

assertthat::on_failure(not_deleted) <- function(call, env) {
    paste0("Object with ID ", sQuote(eval(call$id, env)), " has already been deleted before")
}
# }}}
# is_idf {{{
is_idf <- function (x) inherits(x, "IDF")

assertthat::on_failure(is_idf) <- function (call, env) {
    paste0(deparse(call$x), " is not an IDF object")
}
# }}}
# is_imf {{{
is_imf <- function (x) inherits(x, "IMF")

assertthat::on_failure(is_imf) <- function (call, env) {
    paste0(deparse(call$x), " is not an IMF object")
}
# }}}
# is_model {{{
is_model <- function (x) inherits(x, "IMF") || inherits(x, "IDF")

assertthat::on_failure(is_model) <- function (call, env) {
    paste0(deparse(call$x), " is neither an IDF nor IMF object")
}
# }}}
# is_idf_class {{{
is_idf_class <- function (x) inherits(x, "IDF_Class")

assertthat::on_failure(is_idf_class) <- function (call, env) {
    paste0(deparse(call$x), " is not an IDF_Class object")
}
# }}}
# is_idf_value {{{
is_idf_value <- function (x) inherits(x, "IDF_Value")

assertthat::on_failure(is_idf_value) <- function (call, env) {
    paste0(deparse(call$x), " is not an IDF_Value object")
}
# }}}
# is_idf_obj {{{
is_idf_obj <- function (x) inherits(x, "IDF_Value") || inherits(x, "IDF_Class")

assertthat::on_failure(is_idf_obj) <- function (call, env) {
    paste0(deparse(call$x), " is neither an IDF_Class nor IDF_Value object")
}
# }}}
# is_idf_comment {{{
is_idf_comment <- function (x) inherits(x, "IDF_Comment")

assertthat::on_failure(is_idf_comment) <- function (call, env) {
    paste0(deparse(call$x), " is not an IDF_Comment object")
}
# }}}
# is_idf_ref {{{
is_idf_ref <- function (x) inherits(x, "IDF_Ref")

assertthat::on_failure(is_idf_ref) <- function (call, env) {
    paste0(deparse(call$x), " is not an IDF_Ref object")
}
# }}}
# is_idd {{{
is_idd <- function (x) inherits(x, "IDD")

assertthat::on_failure(is_idd) <- function (call, env) {
    paste0(deparse(call$x), " is not an IDD object")
}
# }}}
# is_idd_class {{{
is_idd_class <- function (x) inherits(x, "IDD_Class")

assertthat::on_failure(is_idd_class) <- function (call, env) {
    paste0(deparse(call$x), " is not an IDD_Class object")
}
# }}}
# is_idd_value {{{
is_idd_value <- function (x) inherits(x, "IDD_Field")

assertthat::on_failure(is_idd_value) <- function (call, env) {
    paste0(deparse(call$x), " is not an IDD_Field object")
}
# }}}
# is_idd_ref_obj {{{
is_idd_ref_obj <- function (x) inherits(x, "IDD_Ref_Obj")

assertthat::on_failure(is_idd_ref_obj) <- function (call, env) {
    paste0(deparse(call$x), " is not an IDD_Ref_Obj object")
}
# }}}
# is_idd_ref_ext {{{
is_idd_ref_ext <- function (x) inherits(x, "IDD_Ref_Ext")

assertthat::on_failure(is_idd_ref_ext) <- function (call, env) {
    paste0(deparse(call$x), " is not an IDD_Ref_Ext object")
}
# }}}
# is_pre_parsed {{{
is_pre_parsed <- function (ver) {
    ver %in% paste0("8.", 5:8)
}
# }}}
# has_hvac_template {{{
has_hvac_template <- function (idf) {
    idf$class[startsWith(class, "HVACTemplate"), .(unique(class))][, .N] > 0L
}
# }}}
# has_class {{{
has_class <- function (idf, class) {
    class %in% valid_class(idf)
}
assertthat::on_failure(has_class) <- function (call, env) {
    paste0(deparse(call$idf), " does not contain any ", eval(call$class, env), " object")
}
# }}}

# not_empty {{{
not_empty <- function (x) {
    all((dim(x) %||% length(x)) != 0)
}
assertthat::on_failure(not_empty) <- function (call, env) {
    paste0(deparse(call$x), " is empty")
}
# }}}
# is_empty {{{
is_empty <- function (x) {
    !not_empty(x)
}
assertthat::on_failure(is_empty) <- function (call, env) {
    paste0(deparse(call$x), " is not empty")
}
# }}}
# is_string {{{
is_string <- function(x) is.character(x) && length(x) == 1
assertthat::on_failure(is_string) <- function(call, env) {
    paste0(deparse(call$x), " is not a string (a length one character vector).")
}
# }}}
# is_scalar {{{
is_scalar <- function(x) {
    length(x) == 1L
}
assertthat::on_failure(is_scalar) <- function(call, env) {
  paste0(deparse(call$x), " is not a scalar.")
}
# }}}
# is_writeable {{{
is_writeable <- function(path) {
    assertthat::assert_that(is_string(path), file.exists(path))
    file.access(path, mode = 2)[[1]] == 0
}
assertthat::on_failure(is_writeable) <- function (call, env) {
    paste0(eval(call$path, env), " is not writeable")
}
# }}}
# is_readable {{{
is_readable <- function(path) {
    assertthat::assert_that(is_string(path), file.exists(path))
    file.access(path, mode = 4)[[1]] == 0
}
assertthat::on_failure(is_readable) <- function (call, env) {
    paste0(eval(call$path, env), " is not readable")
}
# }}}
# has_name {{{
has_name <- function(x, which) which %in% names(x)
assertthat::on_failure(has_name) <- function(call, env) {
    paste0(deparse(call$x), " does not have name ", eval(call$which, env))
}
# }}}
# is_integerish {{{
is_integerish <- function(x) {
    is.integer(x) || (is.numeric(x) && all(x == as.integer(x)))
}
# }}}
# has_ext {{{
has_ext <- function (path, ext) {
    ext == tolower(tools::file_ext(path))
}

assertthat::on_failure(has_ext) <- function (call, env = parent.env) {
    path <- eval(call$path, env)
    ext <- eval(call$ext, env)
    msg("File ", sQuote(basename(path)), " does not have extension ", sQuote(ext), ".")
}
# }}}
# has_output_ext {{{
has_output_ext <- function (x) {
    has_ext(x, "csv") || has_ext(x, "txt") || has_ext(x, "tab")
}

assertthat::on_failure(has_output_ext) <- function (call, env = parent.env) {
    path <- eval(call$path, env)
    ext <- eval(call$ext, env)
    msg("File ", sQuote(basename(path)), " is not an EnergyPlus output data file.")
}
# }}}
