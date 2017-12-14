epat_valid_names <- c("epat_ver", "project_name", "model", "weather", "pair",
                      "param_table", "eplus_dir", "wd_path", "cores")
supported_special_opt <- NULL
unsupported_special_opt <- c("sample", "file", "calc", "jython", "python2", "python3")

#' Import EPAT .json type project
#'
#' \code{import_epat} takes a file path of an .json type project of EPAT, and
#' return a list containing model paths, weather paths, parametric fields, and
#' parametric values. The returned list will have an attribute 'job_type' with
#' value 'epat' which will be used when running jobs using
#' \link{\code{run_job}}.
#'
#' @param json A file path of a .json file.
#' @return A list containing project info.
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_split str_replace_all str_replace str_detect str_replace
#' @importFrom purrr set_names map map_chr map2 set_names flatten_chr cross_n
#' @importFrom data.table rbindlist
#' @importFrom dplyr as_tibble
#' @export
# read_epat{{{1
read_epat <- function(epat, drop = TRUE, parse = TRUE) {

    assertthat::assert_that(is_epat_file(epat), msg = "Invalid EPAT project file.")

    sim_info <- jsonlite::read_json(epat, simplifyVector = TRUE)

    # Check extra names.
    nms <- names(sim_info)
    nms_extra <- setdiff(nms, epat_valid_names)
    if (!is_empty(nms_extra)) {
        warning("Unknown components found and will be dropped: ",
            paste(sQuote(nms_extra), collapse = ", "), ".",
            call. = FALSE
        )
        sim_info <- sim_info[epat_valid_names]
    }

    class(sim_info) <- c("epat", class(sim_info))

    if (parse) {
        sim_info <- parse_epat(sim_info)
    }

    return(sim_info)
}
# }}}1

# validate_epat {{{1
validate_epat <- function (epat) {
    epat <- get_epat_unparsed(epat)
    epat_p <- parse_epat(epat)
    return(epat_p)
}
# }}}1

# parse_epat{{{1
parse_epat <- function (epat) {
    # Validation checking
    epat <- get_epat_unparsed(epat)
    if (inherits(epat, "eplusr_job")) {
        return(epat)
    }

    # Get parameter info.
    project_name <- epat[["project_name"]]
    model <- epat[["model"]]
    weather <- epat[["weather"]]
    param_table <- epat[["param_table"]]
    wd_path <- epat[["wd_path"]]

    param_table <- dplyr::rename(dplyr::as_tibble(param_table),
        id = `ID`, name = `Name`, search_tag = `Search Tag`,
        value_expr = `Value Expressions`, fixed_value = `Fixed Value`
    )
    param_table <- dplyr::mutate(param_table,
        value = purrr::map(value_expr, parse_param_values)
    )
    # Only use selected values
    param_table <- dplyr::mutate(param_table,
        value = purrr::map2(param_table$fixed_value, param_table$value,
            ~{if (as.integer(.x) > 0) .y[as.integer(.x)] else .y}
        )
    )

    # Create case names according to id and get all combination of case values.
    case_params <- get_case_param(
        param_table[["id"]], param_table[["search_tag"]], param_table[["value"]]
    )

    proj_dir <- file_path(wd_path, project_name)

    job <- list(epat_ver = epat[["epat_ver"]],
        model = model, weather = weather, params = case_params,
        project_dir = proj_dir, pair = epat[["pair"]],
        eplus_dir = epat[["eplus_dir"]], cores = epat[["cores"]]
    )

    attr(job, "unparsed") <- epat

    # Add 'eplusr_job' class.
    class(job) <- unique(c("epat", "eplusr_job", class(epat)))

    return(job)
}
# }}}1

# run_epat{{{1
run_epat <- function (epat, group = c("model", "weather", "all"), pair = NULL,
                      output_prefix = NULL, output_suffix = c("C", "L", "D"),
                      cores = NULL, eplus_ver = NULL, eplus_dir = NULL,
                      special_run = NULL) {
    job <- validate_epat(epat)

    # Get job info {{{2
    models <- job[["model"]]
    weathers <- job[["weather"]]
    pair <- pair %||% job[["pair"]]
    eplus_dir <- eplus_dir %||% job[["eplus_dir"]]
    cores <- as.integer(cores %||% job[["cores"]])
    proj_dir <- job[["project_dir"]]
    params <- job[["params"]]
    # }}}2
    # Get values of 'group' {{{2
    # Default value of 'group' is NULL
    if (missing(group)) group <- NULL
    # If 'group' is given, should be one of c('model', 'weather', 'all')
    if (!is.null(group)) group <- rlang::arg_match(group)
    # }}}2
    # Check param fields in model templates {{{2
    fields <- names(params[[1]])
    missing_fields <- get_missing_param_fields(models, fields)
    missing_fields <- purrr::discard(missing_fields, is_empty)
    if (!is_empty(missing_fields)) {
        msg <- purrr::simplify(
            purrr::map(names(missing_fields),
                ~{
                    model <- .x
                    missing_fields <- missing_fields[[.x]]
                    paste0("For model template ", sQuote(model), ", parameter fields ",
                           csQuote(missing_fields), " is/are missing.\n")
                 }
            )
        )
        stop("Incomplete model template(s) found:\n", msg, call. = FALSE)
    }
    # }}}2
    # Get input pairs and selected models and weathers {{{2
    input_pairs <- get_input_pairs(models, weathers, pair = pair)
    models_sel <- input_pairs[["model"]]
    weathers_sel <- input_pairs[["weather"]]
    # }}}2
    # Get all model content of all cases {{{2
    model_contents <- get_model_content(models_sel, params)
    # }}}2
    # Save param models per case and get the paths {{{2
    model_dirs <- write_param_models(model_contents)
    # }}}2
    # Get output dirs according to 'group' {{{2
    output_dirs <- get_group_output_dir(group = group, proj_dir = proj_dir,
        input_pairs = input_pairs, job = job, model_dirs = model_dirs)
    # }}}2
    # Get default value of 'output_prefix' {{{2
    if (is.null(output_prefix)) output_prefixes <- file_prefix(models_sel)
    # }}}2
    # Write the 'run.epat' file to output dir {{{2
    epat_unparsed <- attr(job, "unparsed")
    if (!dir.exists(proj_dir)) dir.create(proj_dir, recursive = TRUE)
    jsonlite::write_json(epat_unparsed, file_path(proj_dir, "run.epat"), pretty = TRUE)
    #) }}}2
    # Run {{{2
    run_multi(models = purrr::simplify(model_dirs), weathers = weathers_sel,
              cores = cores,
              output_dirs = output_dirs, output_prefixes = output_prefixes,
              output_suffix = output_suffix, special_run = special_run,
              eplus_ver = eplus_ver, eplus_dir = eplus_dir, show_msg = TRUE)
    # }}}2
    # Save `job_index.csv` in the project dir {{{2
    job_index <- create_job_index(job = job, group = group, proj_dir = proj_dir,
        input_pairs = input_pairs, params = params, model_dirs = model_dirs
    )
    readr::write_csv(job_index, file_path(proj_dir, "job_index.csv"))
    # }}}2

    return(invisible())
}
# }}}1

# collect_epat {{{1
collect_epat <- function (x, case_names = NULL, no_info = FALSE, no_params = TRUE,
                          output = c("variable", "meter", "table", "surface report"),
                          suffix_type = c("auto", "C", "L", "D"), which = NULL,
                          year = current_year(), new_date = "datetime",
                          tz = Sys.timezone(), drop_na = FALSE, unnest = FALSE,
                          long_table = FALSE) {
    # Initialize
    is_job_index <- FALSE
    epat <- tryCatch(validate_epat(x), error = function (e) NULL)
    # If 'x' is not a parsable epat
    if (is.null(epat)) {
        assertthat::assert_that(assertthat::is.string(x))
        is_file <- utils::file_test("-f", x)
        is_dir <- utils::file_test("-d", x)
        if (is_file) {
            is_job_index <- identical(basename(x), "job_index.csv")
            path <- x
        } else if (is_dir) {
            path <- file_path(x, "job_index.csv")
            is_job_index <- file.exists(path)
        } else {
            is_job_index <- FALSE
        }
    # If 'x' is a parsable epat
    } else {
        x <- epat[["project_dir"]]
        is_dir <- file_test("-d", x)
        if (is_dir) {
            path <- file_path(x, "job_index.csv")
            is_job_index <- file_test("-f", path)
        } else {
            is_job_index <- FALSE
        }
    }
    # Stop if no 'job_index.csv' has been found
    assertthat::assert_that(is_job_index,
        msg = msg("Could not find 'job_index.csv'. 'x' should be an epat object,
                  a path to an .epat file, a path to a 'job_index.csv' or a
                  directory that contains one.")
    )

    # Read "job_index.csv"
    job_index <- read_job_index(path)

    # Get output dirs
    dirs <- job_index[["dir"]]
    output_dirs <- file_path(dirname(path), dirs)

    # Get default value of 'case_names'
    case_names <- case_names %||% dirs
    is_equal_length <- identical(length(case_names), length(dirs))
    assertthat::assert_that(all(is.character(case_names), is_equal_length),
        msg = msg("'case_names' should be a character vector with a same length
                  as case number.")
    )

    # Read data
    data <- tidyr::unnest(
        dplyr::mutate(job_index, case = case_names,
            data = purrr::map(output_dirs,
                ~collect_eplus(path = .x, output = output, suffix_type = suffix_type,
                    which = which, year = year, new_date = new_date, tz = tz,
                    drop_na = drop_na, unnest = FALSE, long_table = long_table
                )
            )
        ),
        data
    )
    data <- dplyr::select(data, no, case, dplyr::everything())

    # Delete info columns except 'case'
    if (no_info) {
        data <- dplyr::select(data, -dplyr::one_of("no", "dir", "template", "weather", "model_prefix"))
    }

    # Delete param columns
    if (no_params) {
        data <- dplyr::select(data, -dplyr::starts_with("@@"))
    }

    # Unnest data
    if (unnest) {
        data <- tidyr::unnest(data)
    }

    return(data)
}
# }}}1

#' A Shiny Gadget for editing EPAT job
#'
#' \code{edit_pat} takes an EPAT json project file, starts a Shiny Gadget to
#' edit it interactively, and returns an EPAT job that can be directly called by
#' \link[eplusr]{run_job}.
#'
#' @param json A EPAT json file or json string.
#' @return A eplusr job with "job_type" attribute of "epat".
#' @export
#'
# edit_epat{{{1
edit_epat <- function (epat, parse = FALSE) {

    library(shiny)
    library(miniUI)

    epat <- validate_epat(epat)
    job <- attr(epat, "unparsed")
    # Get job info {{{2
    proj_name <- job[["project_name"]] %||% "unnamed"
    models <- job[["model"]]
    weathers <- job[["weather"]]
    pair <- job[["pair"]]
    param_table <- job[["param_table"]]
    cores <- job[["cores"]] %||% 4L
    eplus_dir <- file_path(job[["eplus_dir"]] %||% getOption("eplusr.eplus_dir"))
    wd_path <- file_path(job[["wd_path"]] %||% getwd())
    # }}}2
    # helper fun: missing_required {{{2
    missing_required <- function (fields) {
        nms <- names(fields)
        idx <- purrr::map_lgl(fields, is.null)
        mis_fields <- nms[idx]

        return(mis_fields)
    }
    # }}}2
    # UI{{{2
    ui <- miniPage(
        shinytoastr::useToastr(),
        theme = shinythemes::shinytheme("flatly"),
        gadgetTitleBar("Parametric Job Editor"),
        miniTabstripPanel(
            # File input{{{3
            miniTabPanel("File Input", icon = icon("file"),
                miniContentPanel(
                    wellPanel(
                        fluidRow(column(12, h4("Project:"))),
                        fluidRow(
                            column(9,
                                textInput("proj_name", label = NULL,
                                    value = proj_name, width = "100%",
                                    placeholder = "Please insert project name"
                                )
                            ),
                            column(3,
                                actionButton("save_proj_name", "Save", icon = icon("save"))
                            )
                        )
                    ),
                    wellPanel(
                        fileChooseInput(id = "model_path", title = strong("IDF/IMF Model Template"),
                            placeholder = "Please select a model template",
                            btnLabel = "Add", btnIcon = icon("plus")
                        ),
                        h5(strong("Model template list:")),
                        shinycssloaders::withSpinner(
                            DT::dataTableOutput("tbl_models"), proxy.height = "50px"
                        )
                    ),
                    wellPanel(
                        fileChooseInput(id = "weather_path", title = strong("Weather"),
                            placeholder = "Please select a weather file",
                            btnLabel = "Add", btnIcon = icon("plus")
                        ),
                        h5(strong("Weather list:")),
                        shinycssloaders::withSpinner(
                            DT::dataTableOutput("tbl_weather"), proxy.height = "50px"
                        )
                    ),
                    wellPanel(h5(strong("Input combination")),
                        shinycssloaders::withSpinner(
                            rhandsontable::rHandsontableOutput("input_pairs"),
                            proxy.height = "50px"
                        )
                    )
                )
            ),
            # }}}3
            # Parameters{{{3
            miniTabPanel("Parameters", icon = icon("sliders"),
                miniContentPanel(
                    wellPanel(
                        shinycssloaders::withSpinner(
                            DT::dataTableOutput("dt_param_table"), type = 6)),
                    wellPanel(
                        fluidRow(
                            column(6, textInput("id", "Field ID*:", value = NULL, width = "100%")),
                            column(6, textInput("name", "Field Name*:", value = NULL, width = "100%"))
                        ),
                        textInput("tag", "Search tag*:", value = NULL, width = "100%"),
                        textInput("desc", "Description:", value = NULL, width = "100%"),
                        textInput("values", "Value Expressions*:", value = NULL, width = "100%"),
                        div(id = "preview_values", class = "frame", style = "border-style: solid;",
                            tagAppendAttributes(textOutput("preview_values"), style = "white-space:pre-wrap;")),
                        tags$br(),
                        selectInput("fixed_value", "Fix on i-th value:", choices = 0L),
                        tags$br(),
                        helpText("Note: Fields marked with '*' are requred."),
                        miniButtonBlock(
                            actionButton("add_param", label = "Add", icon = icon("plus", lib = "glyphicon")),
                            actionButton("save_param", label = "Save", icon = icon("ok", lib = "glyphicon")),
                            actionButton("copy_param", label = "Copy", icon = icon("copy", lib = "glyphicon")),
                            actionButton("delete_param", label = "Delete", icon = icon("remove", lib = "glyphicon"))
                        )
                    )
                )
            ),
            # }}}3
            # Settings{{{3
            miniTabPanel("Settings", icon = icon("wrench"),
                miniContentPanel(
                    wellPanel(
                        dirChooseInput("eplus_dir", title = "EnergyPlus Location:",
                            value = eplus_dir, placeholder = "Please select an EnergyPlus location"
                        ),
                        fluidRow(
                            column(2,
                                selectInput("cores", label = "Parallel job number:",
                                    choices = seq(1:parallel::detectCores()),
                                    selected = cores
                                )
                            )
                        ),
                        dirChooseInput("wd_path", title = "Working Directory:",
                            value = wd_path, placeholder = "Please select a working folder"
                        )
                    )
                )
            )
            # }}}3
        )
    )
    # }}}2
    # Server{{{2
    server <- function(input, output, session) {

        # Make 'job' a reactive value
        makeReactiveBinding("models", env = parent.frame(n = 2))
        makeReactiveBinding("weathers", env = parent.frame(n = 2))
        makeReactiveBinding("param_table", env = parent.frame(n = 2))

        # Get reactive values of project name {{{3
        proj_name <- eventReactive(input$save_proj_name,
            {
                proj_name <- input$proj_name
                return(proj_name)
            }, ignoreNULL = FALSE
        )
        # }}}3
        # Get reactive values of model and weather{{{3
        model_path <- callModule(fileChoose, "model_path", filetypes = c("idf", "IDF", "imf", "IMF"))
        weather_path <- callModule(fileChoose, "weather_path", filetypes = c("epw", "EPW"))
        model_list <- eventReactive(model_path[["btn"]](),
            {
                models <- unique(c(models, model_path[["path"]]()))
            }, ignoreNULL = FALSE
        )
        weather_list <- eventReactive(weather_path[["btn"]](),
            {
                weathers <- unique(c(weathers, weather_path[["path"]]()))
            }, ignoreNULL = FALSE
        )
        model_table <- reactive({
            dplyr::tibble(`Model Template` = model_list())
        })
        weather_table <- reactive({
            dplyr::tibble(Weather = weather_list())
        })
        input_pair <- reactive({
            pairs <- get_input_pairs(model_list(), weather_list())
            pairs <- purrr::set_names(pairs, c("Model Template", "Weather"))
            pairs <- tibble::as_tibble(pairs)
            pairs <- tibble::add_column(pairs, Selected = rep(TRUE, nrow(pairs)), .before = 1L)
            return(pairs)
        })
        rh_input_pair <- reactive({
            rh <- rhandsontable::rhandsontable(input_pair(), readOnly = TRUE)
            rh <- rhandsontable::hot_col(rh, "Selected", readOnly = FALSE)
            rh <- rhandsontable::hot_table(rh, contextMenu = FALSE)
            return(rh)
        })
        input_filter <- reactive({
            input_from_rh <- rhandsontable::hot_to_r(input$input_pairs)
            pairs <- dplyr::rename(input_from_rh, model = `Model Template`, weather = Weather)
            pairs <- dplyr::filter(pairs, Selected)
            pairs <- dplyr::select(pairs, -Selected)
            return(pairs)
        })
        # }}}3
        # Get reacitve values of parameters{{{3
        param_id <- reactive({
            param_id <- input$id
            if (is.null(param_id)) return(NULL)
            if (identical(param_id, "")) return(NULL)
            return(param_id)
        })
        param_name <- reactive({
            param_name <- input$name
            if (is.null(param_name)) return(NULL)
            if (identical(param_name, "")) return(NULL)
            return(param_name)
        })
        param_desc <- reactive({
            param_desc <- input$desc
            if (is.null(param_desc)) return(NULL)
            if (identical(param_desc, "")) return(NULL)
            return(param_desc)
        })
        param_tag <- reactive({
            param_tag <- input$tag
            if (is.null(param_tag)) return(NULL)
            if (identical(param_tag, "")) return(NULL)
            return(param_tag)
        })
        param_values <- reactive({
            param_values <- input$values
            if (is.null(param_values)) return(NULL)
            if (identical(param_values, "")) return(NULL)
            return(param_values)
        })
        param_values_parsed <- reactive({
            eval_error <- "Invalid R expression or jEPlus paramater definition."
            eval_results <- tryCatch(parse_param_values(param_values()),
                error = function (e) eval_error
            )
            return(eval_results)
        })
        param_fixed <- reactive({
            param_fixed <- input$fixed_value
            if (is.null(param_fixed)) return(NULL)
            if (identical(param_fixed, "")) return(NULL)
            return(param_fixed)
        })
        param_fixed_choices <- reactive({
            eval_error <- "Invalid R expression or jEPlus paramater definition."
            # Update fix value selection
            if (identical(param_values_parsed(), eval_error)) {
                0L
            } else {
                c(0L, seq_along(param_values_parsed()))
            }
        })
        params <- reactive(
            tibble(
                # TODO: try shiny::isTruthy
                ID = ifelse(is.null(input$id), NA_character_, input$id),
                Name = ifelse(is.null(input$name), NA_character_, input$name),
                `Search Tag` = ifelse(is.null(input$tag), NA_character_, input$tag),
                `Value Expressions` = ifelse(is.null(input$values), NA_character_, input$values),
                `Fixed Value` = ifelse(is.null(input$fixed_value), 0L, input$fixed_value),
                Description = ifelse(is.null(input$desc), NA_character_, input$desc)
            )
        )
        required_fields <- reactive(
            list(ID = param_id(), Name = param_name(),
            `Search Tag` = param_tag(), `Value Expressions` = param_values())
        )
        mis_req_fields <- reactive({
            missing_required(required_fields())
        })
        # }}}3
        # Get reactive values of EnergyPlus location and working directory{{{3
        eplus_dir <- callModule(dirChoose, "eplus_dir")
        wd_path <- callModule(dirChoose, "wd_path")
        # }}}3
        # Update inputs according to selected line of the parameter table{{{3
        observeEvent(input$dt_param_table_rows_selected,
            {
                s <- input$dt_param_table_rows_selected
                if (!is.null(s)) {
                    row <- param_table[s,]
                    updateTextInput(session, "id", value = row[["ID"]])
                    updateTextInput(session, "name", value = row[["Name"]])
                    updateTextInput(session, "tag", value = row[["Search Tag"]])
                    updateTextInput(session, "desc", value = row[["Description"]])
                    updateTextInput(session, "values", value = row[["Value Expressions"]])
                    updateSelectInput(session, "fixed_value",
                                      choices = param_fixed_choices(),
                                      selected = as.integer(row[["Fixed Value"]]))
                }

            }
        )
        # }}}3
        # Show a model and weather list table {{{3
        output$tbl_models <- DT::renderDataTable(
            {
                model_table()
            }, filter = "none", options = list(dom = 't')
        )
        output$tbl_weather <- DT::renderDataTable(
            {
                weather_table()
            }, filter = "none", options = list(dom = 't')
        )
        output$input_pairs <- rhandsontable::renderRHandsontable(
            {
                rh_input_pair()
            }
        )
        # }}}3
        # Preview param values {{{3
        output$preview_values <- renderText({
            if (is.null(param_values())) {
                "Please input an R expression or any jEPlus parameter definition in '@@(...)'."
            } else {
                csQuote(param_values_parsed(), and = FALSE)
            }
        })
        # }}}3
        # Update fixed values{{{3
        observeEvent(
            param_values_parsed(),
            # input$preview,
            updateSelectInput(session, "fixed_value", choices = param_fixed_choices())
        )
        # }}}3
        # Show a parameter table {{{3
        observe({
            input$add_param
            input$save_param
            input$delete_param
            input$copy_param
            output$dt_param_table <- DT::renderDataTable(
                DT::datatable(param_table,
                    selection = "single", option = list(dom = "t")
                )
            )
        })
        # }}}3
        # Add parameters{{{3
        observeEvent(input$add_param,
            {
                # shinyjs::show("div_param_table")
                shinyjs::reset("div_param_input")
                # Check required fields before save parameters{{{4
                if (length(mis_req_fields()) > 0L) {
                    mis_fields <- paste0("Parameter attribute ", dQuote(mis_req_fields()), " is missing.")
                    error_msg <- paste(mis_fields, collapse = "<br>")
                    shinytoastr::toastr_error(error_msg, title = "Error",
                        closeButton = TRUE, progressBar = TRUE,
                        position = "bottom-right"
                    )
                # }}}4
                # Show the parameter table{{{4
                } else {
                    param_table <<- dplyr::bind_rows(param_table, isolate(params()))
                    param_table <<- tidyr::drop_na(param_table, ID, Name, `Search Tag`, `Value Expressions`)
                }
                # }}}4
            }
        )
        # }}}3
        # Save parameters{{{3
        observeEvent(input$save_param,
            {
                # Check required fields before save parameters{{{4
                if (length(mis_req_fields()) > 0L) {
                    mis_fields <- paste0("Parameter attribute ", dQuote(mis_req_fields()), " is missing.")
                    error_msg <- paste(mis_fields, collapse = "<br>")
                    shinytoastr::toastr_error(error_msg, title = "Error",
                        closeButton = TRUE, progressBar = TRUE,
                        position = "bottom-right"
                    )
                # }}}4
                # Show the parameter table{{{4
                } else {
                    s <- input$dt_param_table_rows_selected
                    if (!is.null(s)) {
                        row <- isolate(params())
                        param_table[s,] <<- row
                    } else {
                        shinytoastr::toastr_error("Please select a parameter from the table before save.", title = "Error",
                            closeButton = TRUE, progressBar = TRUE,
                            position = "bottom-right"
                        )
                    }
                }
                # }}}4
            }
        )
        # }}}3
        # Delete the selected parameter{{{3
        observeEvent(input$delete_param,
            {
                 s <- input$dt_param_table_rows_selected
                 if (!is.null(s)) {
                     param_table <<- param_table[-s,]
                 } else {
                    shinytoastr::toastr_error("Please select a parameter from the table before delete.", title = "Error",
                        closeButton = TRUE, progressBar = TRUE,
                        position = "bottom-right"
                    )
                 }

            }
        )
        # }}}3
        # Copy the selected parameter{{{3
        observeEvent(input$copy_param,
            {
                 s <- input$dt_param_table_rows_selected
                 if (!is.null(s)) {
                     row <- param_table$table[s,]
                     param_table <<- dplyr::bind_rows(param_table, row)
                 } else {
                    shinytoastr::toastr_error("Please select a parameter from the table before copy.", title = "Error",
                        closeButton = TRUE, progressBar = TRUE,
                        position = "bottom-right"
                    )
                 }

            }
        )
        # }}}3
        # Return project contents when press Done{{{3
        observeEvent(input$done,
            {
                project <- list(
                    epat_ver = "0.0.0.9",
                    project_name = isolate(proj_name()),
                    model = isolate(model_list()),
                    weather = isolate(weather_list()),
                    pair = isolate(input_filter()),
                    param_table = isolate(param_table),
                    eplus_dir = isolate(eplus_dir[["path"]]()),
                    wd_path = isolate(wd_path[["path"]]()),
                    cores = isolate(input$cores)
                )

                class(project) <- c("epat", class(project))

                if (parse) {
                    project <- parse_epat(project)
                }

                stopApp(project)
            }
        )
        # }}}3

        # Stop shiny app when closing the web brower.
        session$onSessionEnded(stopApp)
    }
    # }}}2

    runGadget(shinyApp(ui, server))
}
# }}}1

# Shiny Modules used in `edit_epat`
# fileChooseInput {{{1
fileChooseInput <- function(id, title = NULL, value = "", placeholder = NULL,
                         label = "Select", chooseTitle = "Please Select file(s)",
                         btnLabel = NULL, btnIcon = NULL) {
    # Create a namespace function using the provided id
    ns <- NS(id)

    # obj <- tagList(
    if (is.null(btnLabel)) {
        obj <- tagList(
            fluidRow(column(12, h5(title))),
            fluidRow(
                column(9,
                    textInput(ns("text"), label = NULL, value = value,
                        placeholder = placeholder, width = "100%"
                    )
                ),
                column(3,
                    shinyFiles::shinyFilesButton(ns("choose"),
                        label = label, title = chooseTitle, multiple = TRUE)
                )
            )
        )

    } else {
        obj <- tagList(
            fluidRow(column(12, h5(title))),
            fluidRow(
                column(9,
                    textInput(ns("text"), label = NULL, value = value,
                        placeholder = placeholder, width = "100%"
                    )
                ),
                column(3,
                    shinyFiles::shinyFilesButton(ns("choose"),
                        label = label, title = chooseTitle, multiple = TRUE),
                    actionButton(ns("btn"), label = btnLabel)
                )
            )
        )
    }

    return(obj)
}
# }}}1
# fileChoose {{{1
fileChoose <- function (input, output, session, filetypes = NULL) {
    if (is.null(filetypes)) {
        shinyFiles::shinyFileChoose(input, "choose", roots = shinyFiles::getVolumes())
    } else {
        shinyFiles::shinyFileChoose(input, "choose", roots = shinyFiles::getVolumes(),
            filetypes = filetypes
        )
    }

    # Get the return value of shinyFileChoose
    file_sel <- reactive({
        file_path <- shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$choose)
        if (is.null(file_path)) return(NULL)
        if (identical(file_path, "")) return(NULL)
        return(file_path$datapath)
    })

    observeEvent(input$choose,
        {
            if (!is.null(file_sel())) updateTextInput(session, "text", value = file_sel())
        }
    )

    file_paths <- reactive({
        file_paths <- input$text
        if (is.null(file_paths)) return(NULL)
        if (identical(file_paths, "")) return(NULL)
        file_paths <- purrr::simplify(stringr::str_split(file_paths, ","))
        return(file_path(file_paths))
    })

    return(list(path = file_paths, btn = reactive(input$btn)))
}
# }}}1
# dirChooseInput {{{1
dirChooseInput <- function(id, title = NULL, value = "", placeholder = NULL,
                         label = "Select", chooseTitle = "Please Select a folder",
                         btnLabel = NULL, btnIcon = NULL) {
    # Create a namespace function using the provided id
    ns <- NS(id)

    # obj <- tagList(
    if (is.null(btnLabel)) {
        obj <- tagList(
            fluidRow(column(12, h5(title))),
            fluidRow(
                column(9,
                    textInput(ns("text"), label = NULL, value = value,
                        placeholder = placeholder, width = "100%"
                    )
                ),
                column(3,
                    shinyFiles::shinyDirButton(ns("choose"),
                        label = label, title = chooseTitle)
                )
            )
        )

    } else {
        obj <- tagList(
            fluidRow(column(12, h5(title))),
            fluidRow(
                column(9,
                    textInput(ns("text"), label = NULL, value = value,
                        placeholder = placeholder, width = "100%"
                    )
                ),
                column(3,
                    shinyFiles::shinyDirButton(ns("choose"),
                        label = label, title = chooseTitle),
                    actionButton(ns("btn"), label = btnLabel, icon = btnIcon)
                )
            )
        )
    }

    return(obj)
}
# }}}1
# dirChoose {{{1
dirChoose <- function (input, output, session) {
    shinyFiles::shinyDirChoose(input, "choose", roots = shinyFiles::getVolumes())

    # Get the return value of shinyFileChoose
    dir_sel <- reactive({
        dir_sel <- shinyFiles::parseDirPath(shinyFiles::getVolumes(), input$choose)
        if (is.null(dir_sel)) return(NULL)
        if (identical(dir_sel, "")) return(NULL)
        return(dir_sel)
    })

    observeEvent(input$choose,
        {
            if (!is.null(dir_sel())) updateTextInput(session, "text", value = dir_sel())
        }
    )

    dir_path <- reactive({
        dir_path <- input$text
        if (is.null(dir_path)) return(NULL)
        if (identical(dir_path, "")) return(NULL)
        return(file_path(dir_path))
    })

    return(list(path = dir_path, btn = reactive(input$btn)))
}
# }}}1

# read_job_index {{{1
read_job_index <- function (path, no_at = FALSE) {
    assertthat::assert_that(
        all(assertthat::is.string(path), basename(path) == "job_index.csv"),
        msg = msg("'path' should be a path to a 'job_index.csv'.")
    )

    job_index <- suppressMessages(readr::read_csv(path))

    # Validate column names
    nms <- names(job_index)
    req <- c("no", "dir", "template", "weather")
    mis <- req[is.na(match(req, nms))]
    if (length(mis) > 0L) {
        stop("Invalid job index file. Missing column:\n", csQuote(mis), call. = FALSE)
    }

    if (no_at) {
        job_index <- purrr::set_names(job_index,
            stringr::str_replace_all(names(job_index), "@", "")
        )
    }

    return(job_index)
}
# }}}1
# is_epat_file {{{1
is_epat_file <- function (epat) {
    if (assertthat::is.string(epat)) {
        if (has_epat_ext(epat)) {
            assertthat::assert_that(assertthat::is.readable(epat))
            try_epat <- jsonlite::read_json(epat)
            nms <- names(try_epat)
            nms_missing <- setdiff(epat_valid_names, nms)
            if (is_empty(nms_missing)) {
                TRUE
            } else {
                FALSE
            }
        } else {
            FALSE
        }
    } else {
        FALSE
    }
}
# }}}1
# get_epat_unparsed {{{1
get_epat_unparsed <- function (x) {
    if (is_epat_file(x)) {
        epat <- read_epat(x, parse = FALSE)
    } else {
        if (inherits(x, "epat")) {
            epat <- x
        } else {
            stop("'epat' should be an 'epat' object or a file path of an .epat file.",
                 call. = FALSE)
        }
    }

    return(epat)
}
# }}}1
# get_case_param {{{1
get_case_param <- function (id, field, value) {
    case_name <- make_case_name(id, value)
    values <- purrr::simplify_all(purrr::cross(value), .type = "character")
    values <- purrr::map(values, purrr::set_names, field)
    names(values) <- case_name
    values <- purrr::map(values, get_jeplus_param_comb_value)

    return(values)
}
# }}}1
# make_case_name {{{1
make_case_name <- function (id, value) {
    assertthat::assert_that(is.list(value) || assertthat::not_empty(value))
    assertthat::assert_that(is.character(id) || assertthat::not_empty(id))
    assertthat::assert_that(assertthat::are_equal(length(id), length(value)))

    case <- purrr::map2(id, value, ~paste0(.x, "_", seq_along(.y)))
    case_dt <- data.table::rbindlist(purrr::cross(case))
    case_name <- purrr::invoke(paste, case_dt, sep = "-")

    return(case_name)
}
# }}}1
# parse_param_values{{{1
parse_param_values <- function (string, try = TRUE) {
    # Check if a jEPlus parameter expr
    if (is_jeplus_param_def(string)) {
        def <- stringr::str_replace(string, "^@@(.*)$", "\\1")
        values <- parse_jeplus_param_def(def)
    } else {
        # Eval the text in a new environment parent from an empty
        # environment.
        if (try) {
            values <- tryCatch(eval(parse(text = string), envir = environment()),
                error = function (e) "Invalid R expression.")
        } else {
            values <- eval(parse(text = string), envir = environment())
        }
    }

    return(values)
}
# }}}1
# parse_jeplus_param_def {{{1
parse_jeplus_param_def <- function (x) {
    x <- as.character(x)
    assertthat::assert_that(assertthat::is.string(x))
    # Trim leading and trailing spaces
    expr <- stringr::str_trim(x, side = "both")
    expr_type <- get_jeplus_param_expr_type(expr)
    value <- switch(expr_type,
        normal = get_jeplus_param_expr_normal_value(expr),
        special = stop("Currently ",
            csQuote(paste0("@", unsupported_special_opt)), "are not supported.",
            call. = FALSE),
        unknown = stop("Incorrect jEPlus parameter definition.", call. = FALSE))

    return(value)
}
# }}}1
# jEPlus parameter definition regex {{{1
sp <- "\\s*"
num <- paste0(sp, "\\d+(?:\\.\\d+)*", sp)
grp <- function (..., grp = TRUE) {
    ifelse(grp, paste0("(", ..., ")"), paste0("(?:", ..., ")"))
}

dis <- function (grp = FALSE) {
    grp("\\{\\s*.+?\\s*(?:,\\s*.+?\\s*)*\\}", grp = grp)
}
con <- function (grp = FALSE, num_grp = FALSE) {
    grp_num <- grp(num, grp = num_grp)
    grp("\\[", grp_num, ":", grp_num, ":", grp_num, "\\]", grp = grp)
}
or <- function (x, y, grp = FALSE) {
    grp(x, "|", y, grp = grp)
}
alls <- function (grp = FALSE, sgl_grp = FALSE, num_grp = FALSE) {
    grp(or(dis(grp = sgl_grp), con(grp = sgl_grp, num_grp = num_grp)), grp = grp)
}
inc <- function (x, grp = FALSE) {
    grp("&", sp, x, sp, grp = grp)
}
exc <- function (x, grp = FALSE) {
    grp("\\^", sp, x, sp, grp = grp)
}
star <- function (..., grp = FALSE) {
    paste0(grp(..., grp = grp), "*")
}
reg <- function (...) {
    paste0("^", ..., "$")
}
# Final regex
def <- reg(alls(), sp, star(or(grp(inc(alls()), exc(alls())), grp(exc(alls()), inc(alls())))))
# }}}1
# is_jeplus_param_def {{{1
is_jeplus_param_def <- function (x) {
    grepl(reg("@@", sp, alls(), sp), x)
}
# }}}1
# get_jeplus_param_expr_type {{{1
# jEPlus parameter definition {{{2
# jEPlus parameter definition
# http://www.jeplus.org/wiki/doku.php?id=docs:manual_1_7_params
# NORMAL EXPRESSION
# operators:
## 1. union operator: "&"
## 2. exclusion operator: "^"
## operator chaining syntax: {a, b, c} & [x:y:z] ^ {i, j, k}
# syntax:
## 1. discrete: {x, y, z}
## 2. integer and double: [x:y:z]
# SPECIAL EXPRESSION
# special operators:
## 1. @sample
### supported distribution
### 1. gaussian, n
### 2. uniform, u
### 3. triangular, tr
### 4. lognormal, ln
### 5. exponential, e
### 6. discrete, d
## 2. @file
## 3. @calc
## 4. "|" (combinatorial)
## 5. python, including: jython, python2, python3.
# }}}2
# Get the type of expression, e.g. normal, special(sample, file, calc) and
# unknown.
# Check if special operator prefix "@" exists
get_jeplus_param_expr_type <- function (expr) {
    if (stringr::str_detect(expr, "^@")) {
        if(!stringr::str_detect(expr, "^@\\s*\\w+\\s*\\(.*\\)$")) {
            type <- "unknown"
            # stop("Invalid jEplus parameter definition.", call. = TRUE)
        } else {
            type <- "special"
            # special_opt <- string::str_replace(expr,
            # "^@\\s*(\\w+)\\s*\\((.*)\\)$", "\\1"
            # )
            # if (!is.na(match(special_opt, unsupported_special_opt))) {
            # stop("Currently only", csQuote(supported_special_opt),
            # " is supported.", call. = FALSE
            # )
            # }
            # if (!is.na(match(special_opt, unsupported_special_opt))) {
            # stop("Currently ", csQuote(unsupported_special_opt),
            # " is not supported."
            # )
            # }
        }
    } else if (stringr::str_detect(expr, def)) {
        type <- "normal"
    } else {
        type <- "unknown"
    }

    return(type)
}
# }}}1
# get_jeplus_param_expr_normal_value {{{1
get_jeplus_param_expr_normal_value <- function (expr) {
    if (stringr::str_detect(expr, def)) {
        comp_fir <- stringr::str_extract(expr, alls())
        comp_inc <- stringr::str_extract(expr, inc(alls()))
        comp_exc <- stringr::str_extract(expr, exc(alls()))

        expr_matched <- as.character(stringr::str_match_all(expr, def)[[1]])
        values_fir <- get_jeplus_param_expr_sgl_value(comp_fir)
        # Single normal expression
        if (all(is.na(expr_matched[2]), is.na(expr_matched[3]))) {
            values <- values_fir
            return(values)
        }
        values_inc <- get_jeplus_param_expr_sgl_value(
            stringr::str_extract_all(comp_inc, alls())
        )
        values_exc <- get_jeplus_param_expr_sgl_value(
            stringr::str_extract_all(comp_exc, alls())
        )

        # "&" first, "^" second
        if (is.na(expr_matched[2])) {
            values <- unique(c(values_fir, values_inc))
            values <- setdiff(values, values_exc)
        } else {
            values <- setdiff(values_fir, values_exc)
            values <- unique(c(values, values_inc))
        }
    } else {
        stop("Invalid value expression.", call. = FALSE)
    }

    return(values)
}

get_jeplus_param_expr_sgl_value <- function (expr) {
    if (stringr::str_detect(expr, con())) {
        values <- get_jeplus_param_expr_sgl_con_value(expr)
        values <- seq(from = values[1], to = values[3], by = values[2])
        values <- as.character(values)
    } else {
        values <- get_jeplus_param_expr_sgl_dis_value(expr)
    }

    return(values)
}

get_jeplus_param_expr_sgl_dis_value <- function (expr) {
    expr <- stringr::str_trim(expr, "both")
    expr <- stringr::str_replace_all(expr, "[\\{\\}]", "")
    values <- stringr::str_split(expr, "(\\s)*,(\\s)*")
    values <- purrr::flatten_chr(values)
    return(values)
}

get_jeplus_param_expr_sgl_con_value <- function (expr) {
    expr <- stringr::str_replace_all(expr, "\\s", "")
    values <- stringr::str_extract_all(expr, num)[[1]]
    values <- as.numeric(values)
    return(values)
}
# }}}1
# get_jeplus_param_comb_value {{{1
get_jeplus_param_comb_value <- function (value, num_comb = NULL) {

    assertthat::assert_that(assertthat::not_empty(names(value)))

    field <- purrr::simplify(stringr::str_split(names(value), "\\|"))
    value <- purrr::simplify(stringr::str_split(value, "\\|"))

    names(value) <- field

    return(value)
}
# }}}1
# get_model_content {{{1
get_model_content <- function (models, params) {
    # Get all param info of the target models
    param_info <- get_model_param_info(models)
    # Replace all param fields with values
    param_info$param_line <- replace_param_lines(lines = param_info$param_line, params = params)
    param_info <- purrr::map(param_info$param_line,
        ~{list(content = param_info$content,
               param_line_num = param_info$param_line_num,
               param_line = .x)
         }
    )
    model_content <- purrr::map(param_info, replace_model_lines)
    model_basenames <- basename(models)
    model_content <- purrr::map(model_content, purrr::set_names, nm = model_basenames)

    return(model_content)
}
# }}}1
# get_model_param_info {{{1
get_model_param_info <- function (models) {
    # Read all model lines
    model_lines <- purrr::map(models, read_idf_lines)
    # Get line number of lines that contains param fields
    idx_line_params <- purrr::map(model_lines, stringr::str_which, pattern = "@@.*@@")
    # Get lines per model that contains param fields
    line_params <-
        map2(model_lines, idx_line_params,
             ~{
                 model_line <- .x
                 idx <- .y
                 line_params <- model_line[idx]
             })
    param_info <- list(content = model_lines,
        param_line_num = idx_line_params, param_line = line_params)

    return(param_info)
}
# }}}1
# write_param_models {{{
write_param_models <- function (model_contents) {
    # Get case names
    case_dirs <- names(model_contents)
    # Save models
    temp_dir <- getOption("eplusr.temp_dir")
    model_dirs <- file_path(temp_dir, case_dirs)
    model_save_dirs <- purrr::map2(model_contents, model_dirs,
        ~{
            models <- .x; path <- .y
            if (!dir.exists(path)) {
                dir.create(path, recursive = TRUE)
            }
            paths <- file_path(path, names(models))
            purrr::walk2(models, paths, readr::write_lines)
            return(paths)
         }
    )

    return(model_save_dirs)
}
# }}}
# replace_model_lines {{{1
replace_model_lines <- function (param_info_per_case) {
    assertthat::assert_that(is.list(param_info_per_case))
    assertthat::assert_that(assertthat::has_name(param_info_per_case, "content"))
    assertthat::assert_that(assertthat::has_name(param_info_per_case, "param_line_num"))
    assertthat::assert_that(assertthat::has_name(param_info_per_case, "param_line"))

    purrr::pmap(param_info_per_case, replace_model_lines_per_case)
}
# }}}1
# replace_model_lines_per_case {{{1
replace_model_lines_per_case <- function (content, param_line_num, param_line) {
    content[param_line_num] <- param_line

    return(content)
}
# }}}1
# replace_param_lines {{{1
replace_param_lines <- function (lines, params) {
    assertthat::assert_that(is.list(lines))
    assertthat::assert_that(is.list(params))
    purrr::map(params, ~replace_param_lines_per_case(list_line_params = lines, value = .x))
}
# }}}1
# replace_param_lines_per_case {{{1
replace_param_lines_per_case <- function (list_line_params, value) {
    map(list_line_params, ~replace_param_lines_per_value(.x, value))
}
# }}}1
# replace_param_lines_per_value {{{1
replace_param_lines_per_value <- function (line_params, value) {

    fields <- names(value)
    assertthat::assert_that(assertthat::not_empty(fields))
    values <- unname(value)

    env <- environment()
    purrr::walk2(fields, values,
        ~{
            field <- .x; value <- .y
            line_params <- stringr::str_replace_all(line_params, stringr::fixed(field), value)
            assign("line_params", line_params, envir = env)
         }
    )

    return(line_params)
}
# }}}1
# get_input_pairs {{{1
get_input_pairs <- function (models, weathers, pair = NULL) {

    if (is_empty(pair)) pair <- NULL

    input_pairs <- purrr::cross_df(list(model = models, weather = weathers))

    if (!is.null(pair)) {
        input_pairs <- dplyr::inner_join(pair, input_pairs)
    }
    return(input_pairs)
}
# }}}1
# get_missing_param_fields {{{1
get_missing_param_fields <- function (models, fields) {
    # Get param field for every models
    missing_fields <- purrr::map(models,
        ~setdiff(fields, list_params(read_idf_lines(.x)))
    )
    names(missing_fields) <- models

    return(missing_fields)
}
# }}}1
# get_group_output_dir {{{1
get_group_output_dir <- function (group, proj_dir, input_pairs, job, model_dirs,
                                  case_only = FALSE) {
    # Get case names
    case_name <- names(model_dirs)
    # Get all models and weathers in the job object
    models <- job[["model"]]
    weathers <- job[["weather"]]
    # Get the selected models and weathers from 'input_pairs'
    models_sel <- input_pairs[["model"]]
    weathers_sel <- input_pairs[["weather"]]
    # Get the position of selected models/weathers in all model
    # templates/weathers
    models_idx <- purrr::map_int(models_sel, ~which(models == .x))
    weathers_idx <- purrr::map_int(weathers_sel, ~which(weathers == .x))
    # Get the jEPlus-like mark format
    model_mark <- paste0("T_", models_idx)
    weather_mark <- paste0("W_", weathers_idx)
    # Get the file name of model and weathers
    model_prefix <- file_prefix(models_sel)
    weather_prefix <- file_prefix(weathers_sel)

    if (is.null(group)) {
        input_mark <- paste(model_mark, weather_mark, sep = "-")
        case_dir <- purrr::simplify(purrr::map(case_name, ~paste0(input_mark, "-", .x)))
    } else if (identical(group, "model")) {
        input_mark <- purrr::map(weather_mark, ~paste0(., "-", case_name))
        case_dir <- purrr::simplify(
            purrr::map2(model_prefix, input_mark,
                ~file_path(.x, .y, normalize = FALSE)
            )
        )
    } else if (identical(group, "weather")) {
        input_mark <- purrr::map(model_mark, ~paste0(., "-", case_name))
        case_dir <- purrr::simplify(
            purrr::map2(weather_prefix, input_mark,
                ~file_path(.x, .y, normalize = FALSE)
            )
        )
    } else {
        case_dir <- purrr::simplify(
            purrr::map2(model_prefix, weather_prefix,
                ~file_path(.x, .y, case_name, normalize = FALSE)
            )
        )
    }

    if (case_only) {
        return(case_dir)
    }

    output_dirs <- file_path(proj_dir, case_dir)

    return(output_dirs)
}
# }}}1
# create_job_index {{{1
create_job_index <- function (job, group, proj_dir, input_pairs, params, model_dirs) {

    output_dirs <- get_group_output_dir(group = group, proj_dir = proj_dir,
        input_pairs = input_pairs, job = job, model_dirs = model_dirs, case_only = TRUE)

    models <- input_pairs[["model"]]
    weathers <- input_pairs[["weather"]]

    basic_info <- dplyr::tibble(
        no = seq_along(purrr::simplify(model_dirs)),
        dir = output_dirs,
        template = basename(models),
        weather = basename(weathers)
    )

    tbl_params <- purrr::map_df(names(params),
        ~{
            case <- .x
            len <- length(model_dirs[[case]])
            param_rep <- map(as.list(params[[.x]]), ~rep(.x, len))
            tbl_param <- dplyr::as_tibble(param_rep)
         }
    )

    job_index <- dplyr::bind_cols(basic_info, tbl_params)

    return(job_index)
}
# }}}1

# # write_epg{{{1
# write_epg <- function(epg, path, head_info = NULL){
    # assertthat::assert_that(inherits(epg, "epg"))
#
    # header <- paste(
        # "! EnergyPlus Group File",
        # "! ------------------------------------------------------------------------------------------------",
        # "! Each line represents a specific simulation. If you don't want a simulation to run, add a comment",
        # "! character (an exclamation point) to the beginning of that line. Commas are used to separate the ",
        # "! fields. Each line consists of the following fields: ",
        # "!",
        # "!    input file name, weather file name, output file name (no extension), counter",
        # "!",
        # "! ------------------------------------------------------------------------------------------------",
        # sep = "\n", collapse = "\n")
#
    # epg_content <- dplyr::mutate(epg,
        # output_name = file_path(output_dir, output_prefix),
        # output_dir = NULL, output_prefix = NULL
    # )
    # epg_content <- dplyr::select(epg_content,
        # model, weather, output_name, run_times
    # )
    # # Add 6 leading padding spaces to 'run_times' to create the same format as
    # # original EnergyPlus.
    # epg_content <- dplyr::mutate(epg_content,
        # run_times = stringr::str_pad(as.character(run_times), 7L, "left")
    # )
#
    # content <- readr::format_csv(epg_content, col_names = FALSE)
    # epg_info <- paste(header, content, sep = "\n", collapse = "\n")
#
    # if (!is.null(head_info)) {
        # assertthat::assert_that(is.character(head_info))
        # head_info <- paste0("! ", head_info, collapse = "\n")
        # extra_head <- paste(
            # paste0("! Generated by eplusr ", packageVersion("eplusr")),
            # "! ================================================================================================",
            # "!",
            # head_info,
            # "!",
            # "! ================================================================================================",
            # "!",
            # sep = "\n", collapse = "\n"
        # )
#
        # epg_info <- paste(extra_head, epg_info, "\n", sep = "\n", collapse = "\n")
    # }
#
    # readr::write_lines(epg_info, path)
#
    # return(invisible())
# }
# # }}}1

#' A Shiny app for EnergyPlus Parametric Analysis in R.
#'
#' \code{epat} will launch a shiny app called EPAT (EnergyPlus Parametric
#' Analysis Toolkit).
#'
#' @export
#' @examples
#' epat()
# epat{{{1
epat <- function () {
    shiny::runApp(appDir = system.file("inst/app", package="eplusr"))
}
# }}}1
