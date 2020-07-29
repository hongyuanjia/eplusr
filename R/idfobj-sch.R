#' Create and Modify an EnergyPlus Schedule
#'
#' `IdfScheduleCompact` is an abstraction of a single schedule in an [Idf]. It
#' provides more detailed methods to modify schedule values.
#'
#' @importFrom R6 R6Class
#' @docType class
#' @name IdfScheduleCompact
#' @seealso [Idf] class
#' @author Hongyuan Jia
NULL

# IdfSchedule {{{
#' @export
IdfSchedule <- R6::R6Class(classname = "IdfSchedule", lock_objects = FALSE, inherit = IdfObject)
# }}}

#' @export
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
            idfsch_cmpt_set(super, self, private, ..., .check_range = .check_range),
        # }}}

        # update {{{
        #' @description
        #' Update schedule values
        #'
        #' @param data A [data.table::data.table()] of at least 4 columns:
        #'
        #' * `year_day`
        #' * `daytype`
        #' * `time`
        #' * `value`
        #'
        #' @param check_range If `TRUE`, schedule values will be checked based
        #'        on `$type_limits()`. Default: `TRUE`.
        #'
        #' @param compact If `TRUE`, same schedule values from different day
        #'        types will be compacted together. Also, time periods that have
        #'        the same schedule values will also be compacted. Default:
        #'        `TRUE`.
        #'
        #' @return The modified `IdfScheduleCompact` object.
        #'
        #' @examples
        #' \dontrun{
        #' sch$update(sch$extract())
        #' }
        update = function (data, check_range = TRUE, compact = TRUE)
            idfsch_cmpt_update(super, self, private, data, check_range, compact),
        # }}}

        # validate {{{
        #' @description
        #' Validate schedule values
        #'
        #' Check schedule values based on `$type_limits()`.
        #'
        #' @return An `IdfValidity` object.
        #'
        #' @examples
        #' \dontrun{
        #' sch$validate()
        #' }
        validate = function ()
            idfsch_cmpt_validate(super, self, private),
        # }}}

        # extract {{{
        #' @description
        #' Extract schedule values
        #'
        #' @param daytype If `"compact"`, same schedule values from different day
        #'        types will be compacted together. If `"expand"`, all compacted
        #'        day types will be expanded. Default: `NULL`.
        #' @param timestep If specified, the time step of schedule values will
        #'        be updated if possible. Default: `NULL`.
        #'
        #' @return A [data.table::data.table()]
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
            "error_idfobject_missing_parent"
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
        obj <- without_checking(private$m_parent$add(Schedule_Compact = list(object))[[1L]])

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
    }

    lim
}
# }}}
# idfsch_cmpt_set {{{
idfsch_cmpt_set <- function (super, self, private, ..., .default = "min", .check_range = TRUE) {
    dots <- parse_dots_value(..., .scalar = TRUE, .unique = TRUE)

    # group
    val <- dots$value

    # check daytype
    if (anyNA(val$name)) {
        abort(sprintf("Invalid daytype found: %s", val[is.na(name), collapse(id)]))
    }

    # format hour
    if (anyNA(val$field_name)) {
        if (length(invld <- val[J(NA_integer_, NA_character_), on = c("field_index", "field_name"), nomatch = 0L, which = TRUE])) {
            abort(sprintf("Invalid time specification found for day type %s. Each schedule value should be named with a time value.", collapse(unique(val$name[invld]))))
        }
        val[J(NA_integer_), on = "field_name", field_name :=
            stri_sub(format(hms::hms(hours = field_index)), to = -4L)
        ]
    }

    # check invalid number
    if (anyNA(val$value_num)) {
        abort(sprintf("Invalid schedule value found: %s", val[is.na(value_num), collapse(value_chr)]))
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

    if (!is.null(daytype)) {
        if (isTRUE(daytype) || identical(daytype, "expand")) {
            meta <- expand_sch_daytype(meta)
        } else if ((checkmate::test_flag(daytype) && !daytype) || identical(daytype, "compact")){
            meta <- compact_sch_daytype(meta)
        }
    }

    if (!is.null(timestep)) {
        value <- expand_sch_time(meta, value, timestep = "1 min")
        value <- compact_sch_time(meta, value, timestep = timestep)
    }

    val <- meta[value, on = "daytype_index"][
        , by = "value_index", {
            len <- each_length(daytype)
            list(year_day = rep(year_day, len),
                 daytype = unlist(daytype, FALSE, FALSE),
                 time = rep(time, len),
                 value = rep(value, len)
            )
        }
    ][J(unique(c(unlist(DAYTYPE, FALSE, FALSE), names(DAYTYPE), "AllOtherDay"))),
        on = "daytype", nomatch = NULL]

    set(val, NULL, "value_index", NULL)

    # use hms
    set(val, NULL, "time", hms::hms(minutes = val$time))
    # set date
    set(val, NULL, "year_day", format(transform_sch_date(val$year_day)))
    val
}
# }}}
# idfsch_cmpt_validate {{{
idfsch_cmpt_validate <- function (super, self, private) {
    type_limits <- private$m_data$type_limits$type_limits

    if (is.null(type_limits)) {
        verbose_info("Schedule Type Limits have not been set yet. No validation has been performed.")
        return(invisible())
    }

    validate_sch_cmpt(private$idd_env(), private$idf_env(), private$m_object_id, type_limits)
}
# }}}
