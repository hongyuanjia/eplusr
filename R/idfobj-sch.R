#' Create an `IdfScheduleCompact` object.
#'
#' `schedule_compact()` takes a parent `Idf` object, a name for
#' `Schedule:Compact` object, and returns a corresponding [IdfScheduleCompact]
#' object.
#'
#' If `new` is `TRUE`, an empty [IdfScheduleCompact] is created,
#' with all field values being empty. The empty [IdfScheduleCompact] is directly
#' added into the parent [Idf] object. It is recommended to use `$validate()`
#' method in [IdfScheduleCompact] to see what kinds of further modifications are
#' needed for those empty fields and use `$set()` and `$update()` method to set
#' field values.
#'
#' @param parent An [Idf] object.
#' @param name A valid name (a string) for a `Schedule:Compact` object.
#' @param new If `TRUE`, a new empty [IdfScheduleCompact] is created. Default:
#'        `FALSE`.
#' @return An [IdfScheduleCompact] object.
#' @export
#' @name IdfSchedule
# schedule_compact {{{
schedule_compact <- function (parent, name, new = FALSE) {
    IdfScheduleCompact$new(name, parent, new)
}
# }}}

#' Create and Modify an EnergyPlus Schedule
#'
#' `IdfScheduleCompact` is an abstraction of a single `Schedule:Compact` object
#' in an [Idf]. It provides more detailed methods to add, modify and extract
#' schedule values.
#'
#' @importFrom R6 R6Class
#' @docType class
#' @name IdfSchedule
#' @seealso [Idf] class
#' @author Hongyuan Jia
NULL

# IdfSchedule {{{
#' @export
IdfSchedule <- R6::R6Class(classname = "IdfSchedule", lock_objects = FALSE, inherit = IdfObject)
# }}}

#' @export
#' @name IdfSchedule
# IdfScheduleCompact {{{
IdfScheduleCompact <- R6::R6Class(classname = "IdfScheduleCompact", lock_objects = FALSE,
    inherit = IdfSchedule,
    public = list(
        # INITIALIZE {{{
        #' @description
        #' Create an `IdfScheduleCompact` object
        #'
        #' @param object An integer specifying an object ID.
        #' @param parent An [Idf] object specifying the parent object.
        #' @param new If `TRUE`, an empty `IdfScheduleCompact` will be created.
        #'        Default: `FALSE`
        #'
        #' @return An `IdfScheduleCompact` object.
        #' @examples
        #' \dontrun{
        #' model <- read_idf(system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr"))
        #'
        #' # create an empty 'Schedule:Compact'
        #' schedule_compact(model, "sch", TRUE)
        #'
        #' # get an existing 'Schedule:Compact'
        #' sch <- schedule_compact(model, "sch")
        #' }
        initialize = function (object, parent, new = FALSE)
            idfsch_cmpt_init(super, self, private, object, parent, new),
        # }}}

        # PUBLIC FUNCTIONS {{{
        # type_limits {{{
        #' @description
        #' Get or set the schedule type limits
        #'
        #' @param name A string specifying the name of an `ScheduleTypeLimits`
        #'        object in current [Idf]. If missing, value of last time will
        #'        be used.
        #'
        #' @return A list of 2 elements:
        #'
        #' * `name`: The name of the `ScheduleTypeLimits` object
        #' * `range`: The range of current schedule values
        type_limits = function (name)
            idfsch_cmpt_type_limits(super, self, private, name),
        # }}}

        # set {{{
        #' @description
        #' Set schedule values
        #'
        #' @details
        #' Please note that all schedules will be applied for all days in a
        #' year. For detailed modifications, please check `$update()` method
        #' which accepts data.frame input.
        #'
        #' @param ... Schedule day types and value specifications in lists.
        #'
        #' * Group day types inside `c(...)` at the LHS of `:=`
        #' * Put actual schedule values inside `list(...)` at the RHS of `:=`
        #' * Each schedule value should be named a time. Time can be given in
        #'   either `..H` or `"HH:MM"`.
        #'
        #' @param .check_range If `TRUE`, schedule values will be checked based
        #'        on `$type_limits()`. Default: `TRUE`.
        #'
        #' @return The modified `IdfScheduleCompact` object.
        #'
        #' @examples
        #' \dontrun{
        #' sch$set(c("weekday", "summerdesignday") := list(
        #'     ..6 = 0.2, "8:00" = 0.5,
        #'     ..12 = 0.95, "13:30" = 0.6, ..14 = 0.8,
        #'     ..18 = 0.95, ..19 = 0.2, ..24 = 0),
        #'     allotherday = list(..24 = 0)
        #' )
        #' }
        set = function (..., .check_range = TRUE)
            idfsch_cmpt_set(super, self, private, ..., .check_range = .check_range, .env = parent.frame()),
        # }}}

        # update {{{
        #' @description
        #' Update schedule values using data.frame
        #'
        #' @param data A data.frame of at least 4 columns:
        #'
        #' * `year_day`: Used for the `Through:` fields. Can be in one of the
        #'   following formats:
        #'   - `character`: Day specifications in either `mm/dd` or `mm-dd`
        #'     where `mm` is the month in `1:12` or in character and `dd` is the
        #'     day in month in `1:31`
        #'   - `Date`: The year component will be ignored and only the month and
        #'     day component will be used
        #'   - `integer`: Julian day, e.g. `360`, `365` and etc
        #' * `id` (Optional): Integer type. Used to group together different day
        #'   types with same schedule values.  Grouped day types will be
        #'   compacted in a single `For:` field, e.g. `For: Weekdays
        #'   SummaryDesignDay`. Grouped day types should have the same schedule
        #'   values. Otherwise an error will be issued.
        #' * `daytype`: Character type. Used for the `For:` fields. All possible
        #'   values are listed below. Case-insensitive matching is used.
        #'   Different day types can be grouped using the `id` column mentioned
        #'   above or put together directly in a single string separate by comma
        #'   (`,`), e.g. `"weekend, holiday"`
        #'   - `"AllDay(s)"`
        #'   - `"Weekday(s)"`, and also `"Monday"`, `"Tuesday"`, `"Wednesday"`,
        #'     `"Thursday"` and `"Friday"`
        #'   - `"Weekend(s)"`, and also `"Saturday"` and `"Sunday"`
        #'   - `"SummaryDesignDay"` and `"WinterDesignDay"`
        #'   - `"Holiday"`
        #'   - `"CustomDay1"` and `"CustomDay2"`
        #'   - `"AllOtherDay(s)"`
        #' * `time`: Used for the `Until:` fields. Can be in one of the
        #'   following formats:
        #'   - `character`: Time specifications in `HH:MM`
        #'     where `HH` is the hour in `0:24` and `MM` is the
        #'     minute in `0:60`
        #'   - `integer`: Time differences from `00:00:00` in **minutes**, e.g.
        #'     `seq(60, 60 * 24, by = 60)`
        #'   - `hms`: `hms` objects constructed using `hms::hms()` or
        #'     equivalents, e.g.  `hms::hms(minutes = 30, hours = 1)`, and
        #'     `hms::as_hms("1:30:00")`
        #'   - `difftime`: `difftime` objects constructed using `as.difftime()`
        #'     or equivalents, e.g. `as.difftime(1:24, units = "hours")`
        #'   - `ITime`: `ITime` objects constructed using
        #'     `data.table::as.ITime()`, e.g. `as.ITime("01:30:00")`
        #' * `value`: Numeric type. Used for the actual schedule values
        #'
        #' @param check_range If `TRUE`, schedule values will be checked based
        #'        on `$type_limits()`. Default: `TRUE`.
        #'
        #' @param compact If `TRUE`, same schedule values from different day
        #'        types will be compacted together.  Also, time periods that
        #'        have the same schedule values will also be compacted.
        #'        Note that only `"Holiday"`, `"CustomDay1"` and `"CustomDay2"`
        #'        will be compacted into `"AllOtherDays"`. For example, if the
        #'        `daytype` column contains only `"Weekdays"`,
        #'        `"SummerDesignDay"` and `"AllOtherDays"`, `"AllOtherDays"`
        #'        will be expanded to `"Weekends"`, `"WinterDesignDay"` and
        #'        `"AllOtherDays"`.  Default: `FALSE`.
        #'
        #' @return The modified `IdfScheduleCompact` object.
        #'
        #' @examples
        #' \dontrun{
        #' sch$update(sch$extract())
        #'
        #' val1 <- data.table::data.table(
        #'     year_day = "12/31",
        #'     daytype = "weekday,summerdesignday",
        #'     time = c("6:00", "8:00", "12:00", "13:30", "14:00", "18:00", "19:00", "24:00"),
        #'     value = c(0.2,    0.5,    0.95,    0.6,     0.8,     0.95,    0.2,     0.0)
        #' )
        #' val2 <- data.table::data.table(
        #'     year_day = "12/31", daytype = "allotherday",
        #'     time = "24:00", value = 0.0
        #' )
        #' val <- data.table::rbindlist(list(val1, val2))
        #' sch$update(val)
        #' }
        update = function (data, check_range = TRUE, compact = FALSE)
            idfsch_cmpt_update(super, self, private, data, check_range, compact),
        # }}}

        # validate {{{
        #' @description
        #' Check possible object field value errors
        #'
        #' @details
        #' `$validate()` checks if there are errors in current
        #' `IdfScheduleCompact` object under specified validation level and
        #' returns an `IdfValidity` object.
        #' Schedule value ranges will be checked if current validate level
        #' contains range checking (if corresponding `ScheduleTypeLimits`
        #' `Numeric Type` is `Continuous`) or choice checking (if corresponding
        #' `ScheduleTypeLimits` `Numeric Type` is `Discrete`).
        #'
        #' For detailed description on validate checking, see
        #' \href{../../eplusr/html/IdfObject.html#method-validate}{\code{IdfObject$validate()}}
        #' documentation above.
        #'
        #' @param level One of `"none"`, `"draft"`, `"final"` or a list of 10
        #'        elements with same format as [custom_validate()] output.
        #'
        #' @return An `IdfValidity` object.
        #'
        #' @examples
        #' \dontrun{
        #' sch$validate()
        #'
        #' # check at predefined validate level
        #' sch$validate("none")
        #' sch$validate("draft")
        #' sch$validate("final")
        #' }
        validate = function (level = eplusr_option("validate_level"))
            idfsch_cmpt_validate(super, self, private, level),
        # }}}

        # is_valid {{{
        #' @description
        #' Check if there is any error in current object
        #'
        #' @details
        #' `$is_valid()` returns `TRUE` if there is no error in current
        #' `IdfScheduleCompact` object under specified validation level.
        #' Schedule value ranges will be checked if current validate level
        #' contains range checking (if corresponding `ScheduleTypeLimits`
        #' `Numeric Type` is `Continuous`) or choice checking (if corresponding
        #' `ScheduleTypeLimits` `Numeric Type` is `Discrete`).
        #'
        #' `$is_valid()` checks if there are errors in current `IdfObject` object
        #' under specified validation level and returns `TRUE` or `FALSE`
        #' accordingly. For detailed description on validate checking, see
        #' \href{../../eplusr/html/IdfObject.html#method-validate}{\code{IdfObject$validate()}}
        #' documentation above.
        #'
        #' @param level One of `"none"`, `"draft"`, `"final"` or a list of 10
        #'        elements with same format as [custom_validate()] output.
        #'
        #' @return A single logical value of `TRUE` or `FALSE`.
        #'
        #' @examples
        #' \dontrun{
        #' sch$is_valid()
        #' }
        #'
        is_valid = function (level = eplusr_option("validate_level"))
            idfsch_cmpt_is_valid(super, self, private, level),
        # }}}

        # extract {{{
        #' @description
        #' Extract schedule values
        #'
        #' @param daytype Should be one of:
        #'
        #' * `NULL`: Do nothing Grouped day types will be concatenated with a
        #'   comma, e.g. `Weekdays,SummerDesignDay`. This is the default
        #'   behavior.
        #' * `TRUE` or `"expand"`: All compacted day types will be expanded.
        #' * `FALSE` or `"compact"`: Same schedule values from different day
        #'   types will be compacted together.
        #' * A character vector specifying the grouped day types, e.g.
        #'   `c("weekday", "summerdesignday")`. All other days except specified
        #'   ones will be classified into day type `AllOtherDays`, if possible.
        #'   If not possible, those day types will still be extracted
        #'   separately.
        #'
        #' @param timestep The time step of schedule values, e.g. "10 mins" and
        #'        "1 hour". Valid units include `sec(s)`, `min(s)`, and
        #'        `hour(s)`. If `NULL`, the original time specifications will be
        #'        kept. If `"auto"`, the time periods with the same schedule
        #'        values will be compacted. Default: `NULL`.
        #'
        #' @return `NULL` if current schedule is empty. Otherwise, a
        #' [data.table::data.table()] of 5 columns:
        #'
        #' * `year_day`: Character type. Used for the `Through:` fields.
        #'   Day specifications in `mm/dd` format
        #' * `id`: Integer type. The group index of day types
        #' * `daytype`: Character type. Used for the `For:` fields. All possible
        #'   values are:
        #'   - `"AllDay"`
        #'   - `"Weekday"`, and also `"Monday"`, `"Tuesday"`, `"Wednesday"`,
        #'     `"Thursday"` and `"Friday"`
        #'   - `"Weekend"`, and also `"Saturday"` and `"Sunday"`
        #'   - `"SummaryDesignDay"` and `"WinterDesignDay"`
        #'   - `"Holiday"`
        #'   - `"CustomDay1"` and `"CustomDay2"`
        #'   - `"AllOtherDay"`
        #' * `time`: `hms` vector. Used for the `Until:` fields. It is handy for
        #'   plotting since `hms` object is directly supported by the scale
        #'   system of [ggplot2](https://cran.r-project.org/package=ggplot2)
        #'   package
        #' * `value`: Numeric type. Actual schedule values
        #'
        #' @examples
        #' \dontrun{
        #' sch$extract()
        #' sch$extract("expand")
        #' sch$extract(timestep = "30 mins")
        #' }
        extract = function (daytype = NULL, timestep = NULL)
            idfsch_cmpt_extract(super, self, private, daytype, timestep)
        # }}}
        # }}}
    ),

    # PRIVATE FUNCTIONS {{{
    private = list(
        m_data = NULL
    )
    # }}}
)
# }}}

# idfsch_cmpt_init {{{
idfsch_cmpt_init <- function (super, self, private, object, parent, new = FALSE) {
    if (missing(parent) || !is_idf(parent)) {
        abort(paste("IdfScheduleCompact can only be created based a parent Idf object.",
                "Please give `parent`, which should be an Idf object."),
            "idfobject_missing_parent"
        )
    } else {
        private$m_parent <- parent
    }

    if (!new) {
        obj <- get_idf_object(private$idd_env(), private$idf_env(),
            "Schedule:Compact", object, ignore_case = TRUE)

        private$m_object_id <- obj$object_id
        private$m_class_id <- obj$class_id
        private$m_data <- parse_sch_cmpt(get_idf_value(private$idd_env(), private$idf_env(), object = private$m_object_id))
    } else {
        assert_string(object, .var.name = "Name of Schedule:Compact")
        obj <- private$m_parent$add(Schedule_Compact = list(object))[[1L]]

        private$m_object_id <- obj$id()
        private$m_class_id <- obj$definition()$class_index()

        # create type limit table
        private$m_data$type_limits <- data.table(
            rleid = 1L, object_id = obj$id(), class_name = "Schedule:Compact",
            object_name = object
        )
    }
}
# }}}
# idfsch_cmpt_type_limits {{{
idfsch_cmpt_type_limits <- function (super, self, private, name) {
    if (missing(name)) {
        if (is.null(private$m_data$type_limits$type_limits)) {
            verbose_info("Schedule Type Limits have not been set yet.")
            return(invisible())
        }
        lim <- get_sch_type_limits(private$idd_env(), private$idf_env(), private$m_data$type_limits$type_limits)
    } else {
        lim <- get_sch_type_limits(private$idd_env(), private$idf_env(), name)

        # update data
        type_limits <- get_idf_object(private$idd_env(), private$idf_env(), object = private$m_object_id)
        cols <- c("rleid", "object_id", "class_name", "object_name")
        set(type_limits, NULL, setdiff(names(type_limits), cols), NULL)
        setcolorder(type_limits, cols)
        set(type_limits, NULL, "type_limits", lim$name)
        private$m_data$type_limits <- type_limits

        idfobj_set(self, private, list(Schedule_Type_Limits_Name = type_limits$type_limits),
            .default = FALSE, .empty = TRUE)
    }

    lim
}
# }}}
# idfsch_cmpt_set {{{
idfsch_cmpt_set <- function (super, self, private, ..., .default = "min", .check_range = TRUE, .env = parent.frame()) {
    dots <- parse_dots_value(..., .scalar = TRUE, .unique = TRUE, .env = .env)

    # group
    val <- dots$value

    # check daytype
    if (anyNA(val$name)) {
        abort(sprintf("Invalid daytype found: %s", val[is.na(name), collapse(id)]),
            "idfschcmpt_daytype")
    }

    # format hour
    if (anyNA(val$field_name)) {
        if (length(invld <- val[J(NA_integer_, NA_character_), on = c("field_index", "field_name"), nomatch = 0L, which = TRUE])) {
            abort(sprintf("Invalid time specification found for day type %s. Each schedule value should be named with a time value.",
                collapse(unique(val$name[invld]))), "idfschcmpt_time")
        }
        val[J(NA_integer_), on = "field_name", field_name :=
            stri_sub(format(hms::hms(hours = field_index)), to = -4L)
        ]
    }

    # check invalid number
    if (anyNA(val$value_num)) {
        abort(sprintf("Invalid schedule value found: %s", val[is.na(value_num), collapse(value_chr)]),
            "idfschcmpt_value")
    }
    set(val, NULL, c("each_rleid", "id", "field_index", "value_chr"), NULL)

    # rename
    setnames(val, c("name", "field_name", "value_num"), c("daytype", "time", "value"))

    # add year day
    set(val, NULL, "year_day", 365L)

    # column order
    setcolorder(val, c("rleid", "year_day", "daytype", "time", "value"))
    setnames(val, "rleid", "id")

    self$update(val, check_range = .check_range, compact = FALSE)
}
# }}}
# idfsch_cmpt_update {{{
idfsch_cmpt_update <- function (super, self, private, data, check_range = TRUE, compact = TRUE) {
    load <- update_sch_compt(private$idd_env(), private$idf_env(),
        private$m_data$type_limits,
        private$m_data$meta,
        private$m_data$value,
        data, check_range, compact
    )
    composed <- compose_sch_cmpt(load$type_limits, load$meta, load$value)

    # in case type limit is still not set
    if (is.na(load$type_limits$type_limits)) set(load$type_limits, NULL, "type_limits", NULL)

    # update data
    private$m_data <- load

    # reuse value id if possible
    val <- get_idf_value(private$idd_env(), private$idf_env(),
        object = load$type_limits$object_id,
        field = composed$value[.N, field_index], complete = TRUE)
    set(val, NULL, "value_chr", composed$value$value_chr)
    set(val, NULL, "value_num", composed$value$value_num)
    val[value_id < 0L, value_id := new_id(private$idf_env()$value, "value_id", .N)]

    # overwrite value table
    idf_env <- private$idf_env()
    idf_env$value <- append_dt(idf_env$value, val, "object_id")

    self
}
# }}}
# idfsch_cmpt_extract {{{
#' @importFrom hms hms
idfsch_cmpt_extract <- function (super, self, private, daytype = NULL, timestep = NULL) {
    l <- private$m_data
    meta <- copy(l$meta)
    value <- copy(l$value)

    if (is.null(value)) {
        verbose_info("No schedule values have been set. ",
            "Please use '$set()' or '$update()' to set schedule values before calling '$extract()'.")
        return(invisible())
    }

    keep_order <- TRUE
    if (is.null(daytype)) {
        set(meta, NULL, "daytype", vcapply(meta$daytype, paste0, collapse = ","))
    } else {
        if (isTRUE(daytype) || identical(daytype, "expand")) {
            meta <- expand_sch_daytype(meta)
            keep_order <- FALSE
        } else if ((checkmate::test_flag(daytype) && !daytype) || identical(daytype, "compact")){
            meta <- compact_sch_daytype(meta)
        } else if (checkmate::test_character(daytype, any.missing = FALSE, unique = TRUE)) {
            meta <- compact_sch_daytype(meta, daytype, invert = TRUE)
        } else {
            abort(paste0("Invalid 'daytype'. 'daytype' should be NULL, ",
                "a single logicial value, ", "\"compact\", \"expand\" or ",
                "a character vector of valid day type specifications."),
                "idfschcmpt_daytype"
            )
        }
    }

    if (!is.null(timestep)) {
        assert_string(timestep)
        value <- expand_sch_time(meta, value, timestep = "1 min")
        value <- compact_sch_time(meta, value, timestep = timestep)
    }

    val <- meta[value, on = "daytype_index"][
        , by = "value_index", {
            len <- each_length(daytype)
            list(year_day = rep(year_day, len),
                 id = rep(daytype_index, len),
                 daytype = unlist(daytype, FALSE, FALSE),
                 time = rep(time, len),
                 value = rep(value, len)
            )
        }
    ]

    # keep day type order
    if (!is.null(daytype)) {
        if (keep_order) {
            val <- val[J(unlist(meta$daytype, FALSE, FALSE)), on = "daytype"]
        } else {
            val <- val[J(unique(c(unlist(DAYTYPE, FALSE, FALSE), names(DAYTYPE), "AllOtherDay"))),
                on = "daytype", nomatch = NULL]
        }
    }

    # clean
    set(val, NULL, "value_index", NULL)

    # use hms
    set(val, NULL, "time", hms::hms(minutes = val$time))
    # set date
    set(val, NULL, "year_day", format(transform_sch_date(val$year_day)))
    val
}
# }}}
# idfsch_cmpt_validate {{{
idfsch_cmpt_validate <- function (super, self, private, level = eplusr_option("validate_level")) {
    validate_sch_cmpt(private$idd_env(), private$idf_env(),
        private$m_object_id, private$m_data$type_limits$type_limits, level)
}
# }}}
# idfsch_cmpt_is_valid {{{
idfsch_cmpt_is_valid <- function (super, self, private, level = eplusr_option("validate_level")) {
    count_check_error(idfsch_cmpt_validate(super, self, private, level)) == 0L
}
# }}}

#' @export
# $<-.IdfSchedule {{{
`$<-.IdfSchedule` <- function (x, name, value) {
    # all field names start with a capital letter
    if (!substr(name, 1, 1) %chin% LETTERS && name %chin% ls(x)) return(NextMethod())

    utils::getFromNamespace("unlockBinding", ns = "base")("set", x)
    ori <- x$set
    on.exit(add = TRUE, {
        x$set <- ori
        lockBinding("set", x)
    })
    x$set <- get_super_env(x)$set

    NextMethod()
}
# }}}
