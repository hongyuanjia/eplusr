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
    models_sel <- purrr::simplify_all(purrr::transpose(input_pairs))[[1]]
    weathers_sel <- purrr::simplify_all(purrr::transpose(input_pairs))[[2]]
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
            pairs <- data.table::rbindlist(pairs)
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
        output$dt_param_table <- DT::renderDataTable(param_table,
            selection = "single", option = list(dom = "t")
        )
        proxy <- DT::dataTableProxy("dt_param_table")
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
                    param_table <- dplyr::bind_rows(param_table, isolate(params()))
                    param_table <- tidyr::drop_na(param_table, ID, Name, `Search Tag`, `Value Expressions`)
                    DT::replaceData(proxy, param_table, resetPaging = FALSE)
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
                        param_table[s,] <- row
                        DT::replaceData(proxy, param_table, resetPaging = FALSE)
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
                     param_table <- param_table[-s,]
                     DT::replaceData(proxy, param_table, resetPaging = FALSE)
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
                     param_table <- dplyr::bind_rows(param_table, row)
                     DT::replaceData(proxy, param_table, resetPaging = FALSE)
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
                        label = label, title = chooseTitle, multiple = TRUE,
                        icon = icon("folder-open")
                    )
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
                        label = label, title = chooseTitle, multiple = TRUE,
                        icon = icon("folder-open")
                    ),
                    actionButton(ns("btn"), label = btnLabel, icon = btnIcon)
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
    values <- purrr::simplify_all(purrr::cross_n(value), .type = "character")
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
    case_dt <- data.table::rbindlist(purrr::cross_n(case))
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
            values <- tryCatch(eval(parse(text = string), env = environment()),
                error = function (e) "Invalid R expression.")
        } else {
            values <- eval(parse(text = string), env = environment())
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

    filter <- if (is.null(pair)) NULL else pair_filter(pair)
    input_pairs <- purrr::cross2(models, weathers, .filter = filter)

    return(input_pairs)
}
# }}}1
# pair_filter {{{1
pair_filter <- function (pair) {
    # Change paths in pair into UNIX-like ones
    pair <- purrr::map_df(pair, ~normalizePath(.x, winslash = "/", mustWork = FALSE))

    filter_string <- paste0(
        purrr::map_chr(pair,
            ~{
                glue::glue(
                    "(
                        model %in% c({csQuote(.x[1], and = FALSE)}) &
                        weather %in% c({csQuote(.x[2], and = FALSE)})
                     )"
                )
             }
        ), collapse = " | "
    )

    fun_stringr <- glue::glue(
        "
        function (model, weather) {
            {filter_string}
        }
        "
    )

    fun <- purrr::as_function(eval(parse(text = fun_stringr)))

    return(fun)
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
    models_sel <- purrr::simplify_all(purrr::transpose(input_pairs))[[1]]
    weathers_sel <- purrr::simplify_all(purrr::transpose(input_pairs))[[2]]
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

    models <- purrr::simplify_all(purrr::transpose(input_pairs))[[1]]
    weathers <- purrr::simplify_all(purrr::transpose(input_pairs))[[2]]

    model_dirs %>% purrr::simplify() %>% as.list %>% as_tibble()

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
            param_rep <- map(as.list(params[[.x]]), ~rep(.x, 2))
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

#' A Shiny Gadget for interactively showing EnergyPlus output.
#'
#' \code{show_output} takes an EnergyPlus output, e.g. variables, meters, and
#' launch a shiny gadget for interactively subsetting or plotting the data.
#'
#' @param data A data.frame containing EnergyPlus output.
#' @param state A list containing all or some input values of the Shiny Gadget.
#' This simulates some functionality of Shiny Bookmark.
#' @param group A single character string indicating the group column name if
#' the input data contains multiple EnergyPlus simulation results.
#' @param out The return of the Shiny Gadget.
#' @return Depands on the \code{out}.
#' @export
# show_output{{{
show_output <- function (data, state = NULL, group = NULL,
                         out = c("ggplot2", "plotly", "dygraphs",
                                 "state",
                                 "long_table", "wide_table", "output_info")) {

    library(shiny)
    library(ggplot2)

    # Get the input data name for source code creation
    data_name <- deparse(substitute(data))
    assertthat::assert_that(assertthat::are_equal(length(group), 1L),
                            msg = "'group' should be a single character string.")
    # Standardize input data
    data <- standardize_wide_table(data, exclude = group)

    col_datetime <- get_date_col(data)
    output_info <- get_output_info(data)
    selects <- get_select(output_info)
    col_opt <- stringr::str_to_title(setdiff(c(group, names(selects)), c("freq")))

    out <- rlang::arg_match(out)

    # Get all possible values and default values for all inputs{{{
    # Date and Time
    date_start_min <- lubridate::date(min(data[[col_datetime]]))
    date_start_max <- lubridate::date(max(data[[col_datetime]]))
    date_end_min <- date_start_min
    date_end_max <- date_start_max
    date_start_default <- lubridate::date(min(data[[col_datetime]]))
    date_end_default <- date_start_default + lubridate::days(1)
    time_start_default <- min(data[[col_datetime]])
    time_end_default <- time_start_default

    # Case
    if (is.null(group)) {
        case_choices <- "No Case"
    } else {
        case_choices <- unique(data[[group]])
    }
    case_default <- case_choices

    # Variable
    variable_choices <- var_by(output_info, var = "variable", by = "unit", pretty = TRUE)
    variable_default <- variable_choices[[1]][[1]]

    # Key
    key_choices <- var_by(output_info, var = "key", by = "variable", pretty = TRUE)
    # Default select all keys under the the first variable group.
    key_default <- key_choices[[paste0("Variable: ", variable_default)]][[1]]

    # Plot group
    group_choices <- c("No group", col_opt)
    group_default <- group_choices[1]

    # Plot color
    color_choices <- c("No color", col_opt)
    color_default <- "Variable"

    # Plot facet row
    facet_row_choices <- c("No facet", col_opt)
    facet_row_default <- "Unit"

    # Plot facet col
    facet_col_choices <- facet_row_choices
    facet_col_default <- facet_col_choices[1]
    # }}}

    # Use stated values{{{
    if (!is.null(state)) {
        assertthat::assert_that(rlang::is_bare_list(state),
                                msg = paste0("'state' should be a list, not a ",
                                             class(state)," object."))

        state_name_list <- c("date_start", "date_end", "time_start", "time_end",
                             "case", "variable", "key",
                             "plot_group", "plot_color", "facet_row", "facet_col")

        extras <- setdiff(names(state), state_name_list)
        assertthat::assert_that(rlang::is_empty(extras),
                                msg = paste0("Unknown state variable: ", c_name(extras), "."))
    }

    if (is.null(state)) {
        state <- list()
    }

    init_date_start <- if_then_else(is.null(state[["date_start"]]), date_start_default, state[["date_start"]])

    init_date_start <- if_then_else(is.null(state[["date_start"]]), date_start_default, state[["date_start"]])
    init_date_end   <- if_then_else(is.null(state[["date_end"]]), date_end_default, state[["date_end"]])
    init_time_start <- if_then_else(is.null(state[["time_start"]]), time_start_default, state[["time_start"]])
    init_time_end   <- if_then_else(is.null(state[["time_end"]]), time_end_default, state[["time_end"]])
    init_case       <- if_then_else(is.null(state[["case"]]), case_default, state[["case"]])
    init_variable   <- if_then_else(is.null(state[["variable"]]), variable_default, state[["variable"]])
    init_key        <- if_then_else(is.null(state[["key"]]), key_default, state[["key"]])
    init_plot_group <- if_then_else(is.null(state[["plot_group"]]), group_default, state[["plot_group"]])
    init_plot_color <- if_then_else(is.null(state[["plot_color"]]), color_default, state[["plot_color"]])
    init_facet_row  <- if_then_else(is.null(state[["facet_row"]]), facet_row_default, state[["facet_row"]])
    init_facet_col  <- if_then_else(is.null(state[["facet_col"]]), facet_col_default, state[["facet_col"]])
    # }}}

    # ui{{{
    ui <- bootstrapPage(
        theme = shinythemes::shinytheme("lumen"),
        sidebarLayout(
            sidebarPanel(id = "coll_plot_var", multiple = TRUE, open = c("Time", "Variables"),
                actionButton("cancel", label = "Cancel"),
                actionButton("done", label = "Done"),

                shinyBS::bsCollapse(id = "coll_plot_var", multiple = TRUE, open = c("Variables"),
                    shinyBS::bsCollapsePanel(title = "Time", style = "info",
                        # Time{{{
                        div(
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                dateInput("date_s", label = "Start Date:",
                                          value = init_date_start, min = date_start_min, max = date_start_max)
                            ),

                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                shinyTime::timeInput("time_s", label = "Start Time:",
                                          value = init_time_start, seconds = FALSE)
                            )
                        ),
                        div(
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                dateInput("date_e", label = "End Date:",
                                          value = init_date_end, min = date_end_min, max = date_end_max)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                shinyTime::timeInput("time_e", label = "End Time:",
                                          value = init_time_end, seconds = FALSE)
                            )
                        ),
                        actionButton("update_time", label = "Update")
                        # }}}
                    ),
                    shinyBS::bsCollapsePanel(title = "Variables", style = "primary",
                        # Variable{{{
                        shinyWidgets::pickerInput(inputId = "case", label = "Case:",
                                                  choices = case_choices, selected = init_case, multiple = TRUE,
                                                  choicesOpt = list(icon = rep("glyphicon-triangle-right",
                                                                               length(case_choices))),
                                                  options = list(style = "btn-primary",
                                                                 `live-search` = TRUE,
                                                                 size = 5,
                                                                 `actions-box` = TRUE)
                                                  ),

                        tags$hr(),

                        shinyWidgets::pickerInput(inputId = "variable", label = "Variable:",
                                                  choices = variable_choices, selected = init_variable, multiple = TRUE,
                                                  choicesOpt = list(icon = rep("glyphicon-triangle-right",
                                                                               get_choice_num(variable_choices))),
                                                  options = list(style = "btn-primary",
                                                                 `live-search` = TRUE,
                                                                 size = 10,
                                                                 `actions-box` = TRUE)
                                                  ),

                        tags$hr(),

                        shinyWidgets::pickerInput(inputId = "key", label = "Key:",
                                                  choices = key_choices, selected = init_key, multiple = TRUE,
                                                  choicesOpt = list(icon = rep("glyphicon-triangle-right",
                                                                               get_choice_num(key_choices))),
                                                  options = list(style = "btn-primary",
                                                                 `live-search` = TRUE,
                                                                 size = 10,
                                                                 `actions-box` = TRUE)
                                                  ),

                        # actionButton("update_key", label = "Update")
                        actionButton("update_var", label = "Update")
                        # }}}
                    ),
                    shinyBS::bsCollapsePanel(title = "Plot Options", style = "info",
                        # Group and facet{{{
                        shinyWidgets::pickerInput("plot_group", label = "Group:",
                                                  choices = group_choices, selected = init_plot_group, multiple = FALSE,
                                                  options = list(style = "btn-primary",
                                                                 `live-search` = TRUE,
                                                                 size = 5)
                                                  ),
                        actionButton("update_group", label = "Update"),

                        tags$hr(),

                        shinyWidgets::pickerInput("plot_color", label = "Color:",
                                                  choices = color_choices, selected = init_plot_color, multiple = FALSE,
                                                  options = list(style = "btn-primary",
                                                                 `live-search` = TRUE,
                                                                 size = 5)
                                                  ),
                        actionButton("update_color", label = "Update"),

                        tags$hr(),

                        h5("Facet:"),
                        div(
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                shinyWidgets::pickerInput("facet_row", label = "Row:",
                                                          choices = facet_row_choices, selected = init_facet_row, multiple = FALSE,
                                                          options = list(style = "btn-primary",
                                                                         `live-search` = TRUE,
                                                                         size = 5)
                                                          )
                            ),

                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                shinyWidgets::pickerInput("facet_col", label = "Column:",
                                                          choices = facet_col_choices, selected = init_facet_col, multiple = FALSE,
                                                          options = list(style = "btn-primary",
                                                                         `live-search` = TRUE,
                                                                         size = 5)
                                                          )
                            )
                        ),
                        actionButton("update_facet", label = "Update")
                        # }}}
                    )
                )
            ),

            mainPanel(
                div(class = "container-fluid",
                # Main panel{{{
                    tabsetPanel(type = "tabs",
                        tabPanel("R Code",
                                 textInput("data_name_prefix", label = "Data Name Prefix:", value = "data"),
                                 actionButton("update_data_name_prefix", "Update"),
                                 actionButton("copy_code", "Copy Code to Clipboard", icon = icon("clipboard")),
                                 shinyWidgets::receiveSweetAlert("success_copy"),
                                 tags$br(),
                                 shinyAce::aceEditor(outputId = "source_code", mode = "r",
                                           theme = "monokai", vimKeyBinding = TRUE,
                                           height = "600px", readOnly = TRUE,
                                           autoComplete = "live")
                                 # verbatimTextOutput("source_code")
                                 # tags$style(type="text/css", "#debug {white-space: pre-wrap;}"),
                                 # textOutput("debug"),
                                 # tags$style(type="text/css", "#debug2 {white-space: pre-wrap;}"),
                                 # textOutput("debug2"),
                                 # tags$style(type="text/css", "#debug3 {white-space: pre-wrap;}"),
                                 # textOutput("debug3")
                        ),
                        tabPanel("ggplot2",
                                 shinyjqui::jqui_resizabled(plotOutput("ggplot"))
                        ),
                        tabPanel("plotly",
                                 shinyjqui::jqui_resizabled(plotly::plotlyOutput("plotly"))
                        ),
                        tabPanel("dygraph",
                                 tags$br(),
                                 tags$br(),
                                 uiOutput("dygraph"),
                                 div(id = "dygraphs_legend")
                                 # shinydashboard::box(textOutput("dygraphs_legend"), title = "Legend", collapsible = TRUE, width = )
                        )
                    )
                )
                # }}}
            )
        )
    )
    # }}}

# server{{{
    server <- function (input, output, session) {
        # Set reactive date time input{{{
        time_range <- reactive({
                plot_date_s <- input$date_s
                plot_date_e <- input$date_e
                plot_time_s <- input$time_s
                plot_time_e <- input$time_e

                plot_time_s <- update(plot_time_s,
                                      year = lubridate::year(plot_date_s),
                                      month = lubridate::month(plot_date_s),
                                      mday = lubridate::mday(plot_date_s))

                plot_time_e <- update(plot_time_e,
                                      year = lubridate::year(plot_date_e),
                                      month = lubridate::month(plot_date_e),
                                      mday = lubridate::mday(plot_date_e))

                plot_time_range <- c(plot_time_s, plot_time_e)

                return(plot_time_range)
        })
        # }}}

        # Set reactive variable input{{{
        case <- reactive({
            cases <- input$case
            # If no 'group', always return NULL.
            if (is.null(group)) {
                return(NULL)
            }
            if (is.null(cases)) {
                return(NULL)
            }
            return(cases)
        })
        variable <- reactive({
            variables <- input$variable
            if (is.null(variables)) {
                return(selects[["variable"]])
            }
            return(variables)
        })
        key <- reactive({
            keys <- input$key
            if (is.null(keys)) {
                return(selects[["key"]])
            }
            return(keys)
        })
        # }}}

        # Set reactive option input{{{
        plot_group <- reactive({
            plot_group <- input$plot_group
            if (is.null(plot_group)) {
                return(NULL)
            }
            if (plot_group == "") {
                return(NULL)
            }
            if (plot_group == "No group") {
                return(NULL)
            }
            return(stringr::str_to_lower(plot_group))
        })
        plot_color <- reactive({
            plot_color <- input$plot_color
            if (is.null(plot_color)) {
                return(NULL)
            }
            if (plot_color == "") {
                return(NULL)
            }
            if (plot_color == "No color") {
                return(NULL)
            }
            return(stringr::str_to_lower(plot_color))
        })
        facet_row <- reactive({
            facet_row <- input$facet_row
            if (is.null(facet_row)) {
                return(".")
            }
            if (facet_row == "") {
                return(".")
            }
            if (facet_row == "No facet") {
                return(".")
            }
            return(stringr::str_to_lower(facet_row))
        })
        facet_col <- reactive({
            facet_col <- input$facet_col
            if (is.null(facet_col)) {
                return(".")
            }
            if (facet_col == "") {
                return(".")
            }
            if (facet_col == "No facet") {
                return(".")
            }
            return(stringr::str_to_lower(facet_col))
        })
        plot_facet <- reactive({
            if (all(facet_row() == ".", facet_col() == ".")) {
                return(NULL)
            } else {
                plot_facet <- paste0(facet_row(), "~", facet_col())
            }
            return(stringr::str_to_lower(plot_facet))
        })
        # }}}

        # Set reactive value of code action input{{{
        data_name_prefix <- reactive({
            data_name_prefix <- input$data_name_prefix
            if (is.null(data_name_prefix)) {
                return("data")
            }
            if (data_name_prefix == "") {
                return("data")
            }
            return(data_name_prefix)
        })
        # }}}

        # Set reactive data{{{
        data_all <- reactiveValues()

        data_all$output_info <- reactive({
            info_filtered <- filter_output_info(info = output_info,
                                                key = key(),
                                                variable = variable())
            return(info_filtered)
        })
        data_all$wide_table <- reactive({
            if (!is.null(time_range())) {
                time_range = format(time_range(), "%F %X")
                data <- dplyr::filter_at(data, dplyr::vars(col_datetime),
                                         dplyr::all_vars(. >= time_range[1] & .< time_range[2]))
            }
            if (!is.null(case())) {
                cases = case()
                data <- dplyr::filter_at(data, dplyr::vars(group),
                                         dplyr::all_vars(. %in% cases))
            }
            data_filtered <- dplyr::select(data, dplyr::one_of(group, col_datetime,
                                                               data_all$output_info()[["output"]]))
            return(data_filtered)
        })
        data_all$long_table <- reactive({
            long_table <- long_table(data_all$wide_table(), group = group)
            return(long_table)
        })
        data_all$data_plot <- reactive({
            data_plot <- dplyr::full_join(data_all$long_table(), data_all$output_info())
            data_plot <- tidyr::drop_na(data_plot)
            return(data_plot)
        })
        data_all$data_units <- reactive({
            data_units <- purrr::flatten_chr(get_select(data_all$output_info(), "unit"))
            return(data_units)
        })
        data_all$source_code <- reactive({
            time_range = format(time_range(), "%F %X")
            create_source_code(data_name = data_name, data_name_prefix = data_name_prefix(),
                               group = group, col_datetime = col_datetime,
                               time_range = time_range, cases = case(),
                               outputs = data_all$output_info()[["output"]],
                               plot_group = plot_group(),
                               plot_color = plot_color(),
                               plot_facet = plot_facet())
        })
        # }}}

        # Update variable inputs{{{
        # TODO: What if the variables do not contain any data (all NAs)?
        observe({
            info_filtered <- dplyr::filter(output_info, variable %in% variable())
            keys <- isolate(key())
            keys_lost <- setdiff(keys, info_filtered[["key"]])
            if (length(keys_lost) > 0L) {
                info_filtered <- dplyr::filter(info_filtered, !key %in% keys_lost)
                selected_key <- keys[!keys %in% keys_lost]
            } else {
                selected_key <- keys
            }

            choices_key <- var_by(info_filtered, var = "key", by = "variable", pretty = TRUE)

            shinyWidgets::updatePickerInput(session, inputId = "key", choices = choices_key, selected = selected_key,
                                            choicesOpt = list(icon = rep("glyphicon-triangle-right",
                                                                         get_choice_num(choices_key)))
                                            )
        })
        # }}}

        # Plot using `ggplot2`{{{
        observeEvent(c(input$update_time,
                       input$update_var,
                       input$update_group,
                       input$update_color,
                       input$update_facet),
            {
                data_units <- isolate(data_all$data_units())
                data_plot <- isolate(data_all$data_plot())
                data_plot_splitted <- base::split(data_plot, data_plot[["unit"]])

                data_all$plot_ggplot <-
                    purrr::map(names(data_plot_splitted),
                               ~{
                                   data_plot <- data_plot_splitted[[.x]]
                                   p <- ggplot_line(data = data_plot,
                                                    x = col_datetime, y = "value",
                                                    group = isolate(plot_group()),
                                                    color = isolate(plot_color()),
                                                    facet = isolate(plot_facet()))
                                   p <- p +
                                        scale_x_datetime(name = "", breaks = scales::date_breaks("4 hour"),
                                                         labels = scales::date_format("%b %d %Hh", tz = Sys.timezone()))
                                        theme(legend.position = "bottom",
                                              legend.background = element_rect(color = "black"),
                                              legend.key = element_rect(color = "gray"),
                                              strip.background = element_rect(fill = "white", color = "black"))

                                   if (all(length(data_units) > 1L, .x != data_units[length(data_units)])) {
                                       p <- p +
                                           theme(legend.position = "none",
                                                 axis.title.x = element_blank(),
                                                 axis.text.x = element_blank(),
                                                 axis.ticks.x = element_blank())
                                   }
                                   return(p)
                               })

                data_all$plot_ggplot <- do.call(cowplot::plot_grid, c(data_all$plot_ggplot, ncol = 1, align = "hv"))
                output$ggplot <- renderPlot({data_all$plot_ggplot})

                data_all$plot_plotly <- plotly::ggplotly(data_all$plot_ggplot) #%>%
                    # layout(legend = list(x = 0.5, xanchor = "center", y = -0.25))
                output$plotly <- plotly::renderPlotly({data_all$plot_plotly})
            }
        )
        # }}}

        # Plot using `dygraphs`{{{
        observeEvent(c(input$update_time,
                       input$update_var,
                       input$update_group,
                       input$update_color,
                       input$update_facet),
            {

                data_units <- purrr::flatten_chr(get_select(isolate(data_all$output_info()), "unit"))
                ids_dygraph <- paste0("dy_", seq_along(data_units))

                # Generate dygraphOutput
                output$dygraph <- renderUI({
                    multi_dygraphOutput(outputIds = ids_dygraph)
                })

                # Create data for dygraphs

                data_plot_splitted <- base::split(isolate(data_all$data_plot()),
                                                  isolate(data_all$data_plot())[["unit"]])
                data_all$plot_dygraphs <- purrr::map(names(data_plot_splitted),
                                ~{
                                    data_plot <- data_plot_splitted[[.x]]
                                    wide_table_splitted <- wide_table(data_plot)
                                    xts_splitted <- wide_to_xts(wide_table_splitted, short_name = TRUE)
                                    plot_dygraph <-
                                        dygraphs::dygraph(xts_splitted, group = "dygraphs",
                                                ylab = paste0("[",as.character(.x), "]")) %>%
                                        dygraphs::dyRangeSelector() %>%
                                        dygraphs::dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                                        dygraphs::dyOptions(useDataTimezone = TRUE, axisLineWidth = 1.5) %>%
                                        dygraphs::dyLegend(width = 400, show = "follow", labelsSeparateLines = TRUE) %>%
                                        # dyLegend(width = 400, show = "follow", labelsSeparateLines = TRUE, labelsDiv = "dygraphs_legend") %>%
                                        dygraphs::dyCrosshair(direction = "vertical") %>%
                                        dygraphs::dyCSS(system.file("css", "dygraphs.css", package = "eplusr"))
                                    return(plot_dygraph)
                                })

                for (i in seq_along(ids_dygraph)) {
                    # Need local so that each item gets its own number. Without it, the value
                    # of i in the renderPlot() will be the same across all instances, because
                    # of when the expression is evaluated.
                    local({
                        my_i <- i
                        # plotname <- paste0("Unit: ", data$units[my_i])

                        output[[ids_dygraph[my_i]]] <- dygraphs::renderDygraph({
                            isolate(data_all$plot_dygraphs)[[my_i]]
                        })
                    })
                }
            }
        )
        # }}}

        # Source code actions{{{
        observeEvent(input$update_data_name_prefix,
            {
                shinyAce::updateAceEditor(session, "source_code", value = data_all$source_code())
                updateTextInput(session, "data_name_prefix", value = isolate(data_name_prefix()))
            }
        )
        observeEvent(input$copy_code,
            {
                clipr::write_clip(content = isolate(data_all$source_code()), object_type = "character")
                shinyWidgets::sendSweetAlert("success_copy", title = "Success!", text = "Source Code has been successfully copied to clipboard", type = "success")
            }
        )
        # }}}

        # # Debug{{{
        # output$source_code <- renderText({
        #     data_all$source_code()
        # })
        # output$debug <- renderPrint({
        #     data_all$output_info()
        # })
        # output$debug2 <- renderPrint({
        #     data_all$wide_table()
        # })
        # output$debug3 <- renderPrint({
        #     data_all$long_table()
        # })
        # # }}}

        # Return project contents when press Done{{{
        observeEvent(input$done,
            {
                stopApp(
                    switch(out,
                        long_table  = isolate(data_all$long_table()),
                        wide_table  = isolate(data_all$wide_table()),
                        output_info = isolate(data_all$output_info()),
                        ggplot2     = isolate(data_all$plot_ggplot),
                        plotly      = isolate(data_all$plot_plotly),
                        dygraphs    = isolate(data_all$plot_dygraphs),
                        state       = list(date_start = isolate(input$date_s),
                                           date_end   = isolate(input$date_e),
                                           time_start = isolate(input$time_s),
                                           time_end   = isolate(input$time_e),
                                           case       = isolate(input$case),
                                           variable   = isolate(input$variable),
                                           key        = isolate(input$key),
                                           plot_group = isolate(input$plot_group),
                                           plot_color = isolate(input$plot_color),
                                           facet_row  = isolate(input$facet_row),
                                           facet_col  = isolate(input$facet_col)
                                           )
                    )
                )
            }
        )
        # }}}

        observeEvent(input$cancel, stopApp())

        # Stop shiny app when closing the web brower.
        session$onSessionEnded(stopApp)
    }
    # }}}

    runGadget(shinyApp(ui, server))
}
# }}}

#######################################
#  helper function for show_output()  #
#######################################
# csQuote{{{
csQuote <- function (x, and = TRUE) {
    x_sq <- sQuote(x)
    if (length(x_sq) > 1L & and) {
        x_sq[length(x_sq)] <- paste0("and ", x_sq[length(x_sq)])
    }

    x_csq <- paste0(x_sq, collapse = ", ")

    return(x_csq)
}
# }}}
# get_output_info{{{
get_output_info <- function (wide_table) {
    wide_table <- suppressWarnings(standardize_wide_table(wide_table))
    outputs <- colnames(wide_table)
    # Exclude non-EnergyPlus output columns
    idx <- stringr::str_detect(outputs, "^.*?:.*?\\s\\[.*\\]\\(.*\\)$")
    output_names <- outputs[idx]

    dplyr::tibble(output = output_names,
                  key = stringr::str_to_lower(stringr::str_extract(output_names, ".*(?=:)")),
                  variable = stringr::str_extract(output_names, "(?<=:).*(?=\\s\\[)"),
                  unit = stringr::str_extract(output_names, "(?<=\\[).*(?=\\])"),
                  freq = stringr::str_extract(output_names, "(?<=\\]\\().*?(?=\\)$)"))
}
# }}}
# get_select{{{
get_select <- function (data, cols) {
    assertthat::assert_that(is.data.frame(data))

    # Find the column with classes of "character" or "factor"
    classes <- purrr::map(data, class)
    chr_classes <- (purrr::map_lgl(classes, function(classes) 'character' %in% classes)|
                    purrr::map_lgl(classes, function(classes) 'factor' %in% classes))

    # Find the name of chracter columns
    chr_cols <- names(which(chr_classes))

    # Validate 'cols'
    if (missing(cols)) {
        cols <- chr_cols
    }
    if (!is.character(cols)) {
        stop("'cols' should be a character vector", call. = FALSE)
    }

    # Stop if not a character column or column not found
    idx_col <- !is.na(match(cols, chr_cols))
    if (!all(idx_col)) {
        err_col <- cols[!idx_col]
        err_col_msg <- paste0(paste0("'", err_col, "'"), collapse = ", ")
        stop("Invalid 'cols'. Could not find column ", mis_col_msg,
             " or they are not character columns.", call. = FALSE)
    }

    # Get selects
    selects <- purrr::map(cols, ~tryCatch(as.character(unique(data[[.x]])),
                                          error = function (e) {NULL}))
    selects <- purrr::set_names(selects, cols)

    return(selects)
}
# }}}
# var_by{{{
var_by <- function(tbl_varinfo, var, by, pretty = FALSE) {
    all_cols <- colnames(tbl_varinfo)
    if (is.na(match(var, all_cols))) {
        stop("'var' must match a column name to extract information from. ",
             "Unknown column:", c_name(var), ".", call. = FALSE)
    }
    if (is.na(match(by, all_cols))) {
        stop("'by' must match a column name to group by. ",
             "Unknown column:", c_name(by), ".", call. = FALSE)
    }

    var_quo <- rlang::parse_quosure(var)
    by_quo <- rlang::parse_quosure(by)

    nested_info <-
        tbl_varinfo %>%
        dplyr::select(rlang::UQ(var_quo), rlang::UQ(by_quo)) %>%
        dplyr::distinct() %>%
        dplyr::group_by(rlang::UQ(by_quo)) %>%
        tidyr::nest()

    var_by_unnamed <-
        nested_info %>%
        dplyr::pull(var = "data") %>%
        purrr::map(dplyr::pull, var = var)

    var_by <-
        var_by_unnamed %>%
        purrr::map(as.list) %>%
        purrr::set_names(nested_info[[by]])

    if (pretty) {
        head_by <- paste0(stringr::str_to_title(by), ": ")
        pretty_names <- paste0(head_by, names(var_by))
        var_by <- purrr::set_names(var_by, pretty_names)
    }
    return(var_by)
}
# }}}
# filter_output_info{{{
filter_output_info <- function(info, key = NULL, variable = NULL, unit = NULL) {
    # Filter info table
    if (any(is.null(key), key == "")) {
        filter_key <- NULL
    } else {
        filter_key <- paste0("key %in% c(", c_name(key), ")")
    }

    if (any(is.null(variable), variable == "")) {
        filter_variable <- NULL
    } else {
        filter_variable <- paste0("variable %in% c(", c_name(variable), ")")
    }

    if (any(is.null(unit), unit == "")) {
        filter_unit <- NULL
    } else {
        filter_unit <- paste0("unit %in% c(", c_name(unit), ")")
    }

    filter_info <- stringr::str_c(filter_key, filter_variable, filter_unit, sep = " & ")
    if (assertthat::not_empty(filter_info)) {
        filter_info <- stringr::str_c("dplyr::filter(info, ", filter_info, ")")
        info <- rlang::eval_tidy(rlang::parse_expr(filter_info))
    }

    return(info)
}
# }}}
# standardize_wide_table{{{
standardize_wide_table <- function (data, exclude = NULL) {
    # Check if input is a data.frame.
    assertthat::assert_that(is.data.frame(data),
                            msg = "Input should be a data.frame object.")

    # Check if input has a datetime column.
    col_datetime <- get_date_col(data)
    assertthat::assert_that(assertthat::not_empty(col_datetime),
                            msg = "'data' does not have any datetime column.")

    # If multiple datetime columns found, the first one will be used.
    if (length(col_datetime) > 1L) {
        warning("Multiple datetime columns found. The first one will be used ",
                "and others will be dropped.", call. = FALSE)
        data <- dplyr::select(data, -dplyr::one_of(col_datetime[-1]))
    }

    # Check if all column names meet the EnergyPlus standard output name format.
    if (!is.null(exclude)) {
        # assertthat::assert_that(length(exclude) == 1L,
        #                         msg = "'exlude' should be a single character string.")
        col_missing <- exclude[is.na(match(exclude, colnames(data)))]
        assertthat::assert_that(rlang::is_empty(col_missing),
                                msg = paste0("Invalid 'exclude'. Could not find column ",
                                             c_name(col_missing), "."))
    }
    col_exclude <- c(col_datetime, exclude)
    col_names <- colnames(data)[!colnames(data) %in% col_exclude]
    col_regex <- "^.*?:.*?\\s\\[.*\\]\\(.*\\)$"
    col_idx <- stringr::str_detect(col_names, col_regex)
    col_not <- col_names[!col_idx]
    # Columns not met will be dropped.
    if (assertthat::not_empty(col_not)) {
        warning("Columns that do not have EnergyPlus-flavoured output names be dropped:\n",
                c_name(col_not), ".", call. = FALSE)
        data <- dplyr::select(data, -dplyr::one_of(col_not))
    }

    return(data)
}
# }}}
# standardize_long_table{{{
standardize_long_table <- function (data, exclude = NULL) {
    # Check if input is a data.frame.
    assertthat::assert_that(is.data.frame(data),
                            msg = "Input should be a data.frame object.")

    # Check if input has a datetime column.
    col_datetime <- get_date_col(data)
    assertthat::assert_that(assertthat::not_empty(col_datetime),
                            msg = "'data' does not have any datetime column.")

    # If multiple datetime columns found, the first one will be used.
    if (length(col_datetime) > 1L) {
        warning("Multiple datetime columns found. The first one will be used ",
                "and others will be dropped.", call. = FALSE)
        data <- dplyr::select(data, -dplyr::one_of(col_datetime[-1]))
    }

    # Check if all column names meet the format.
    if (!is.null(exclude)) {
        # assertthat::assert_that(length(exclude) == 1L,
        #                         msg = "'exlude' should be a single character string.")
        col_missing <- exclude[is.na(match(exclude, colnames(data)))]
        assertthat::assert_that(rlang::is_empty(col_missing),
                                msg = paste0("Invalid 'exclude'. Could not find column ",
                                             c_name(col_missing), "."))
    }
    col_exclude <- c(col_datetime, exclude)
    col_names <- colnames(data)[!colnames(data) %in% col_exclude]
    col_required <- c("output", "value")
    col_not <- col_names[is.na(match(col_names, col_required))]
    col_missing <- col_required[is.na(match(col_required, col_names))]
    # Check requred columns.
    assertthat::assert_that(rlang::is_empty(col_missing),
                            msg = paste0("Missing column ", c_name(col_missing), "."))
    # Columns not met will be dropped.
    if (assertthat::not_empty(col_not)) {
        warning("Columns below be dropped:\n",
                c_name(col_not), ".", call. = FALSE)
        data <- dplyr::select(data, -dplyr::one_of(col_not))
    }

    output_all <- data[["output"]]
    row_all <- 1:nrow(data)
    output_regex <- "^.*?:.*?\\s\\[.*\\]\\(.*\\)$"
    output_idx <- stringr::str_detect(output_all, output_regex)
    output_not <- output_all[!output_idx]
    row_not <- row_all[!output_idx]
    msg <- paste0("Row ", row_not,": '", output_not, "'.", collapse = "\n")
    if (!all(output_idx)) {
        warning("Rows with 'output' values that do not have EnergyPlus-flavoured output name format be dropped:\n",
                msg, ".", call. = FALSE)
        data <- dplyr::slice(data, -row_not)
    }

    return(data)
}
# }}}
# long_table{{{1
long_table <- function(wide_table, group = NULL) {
    wide_table <- standardize_wide_table(wide_table, exclude = group)

    col_datetime <- get_date_col(wide_table)
    if (is.null(group)) {
        long_table <- tidyr::gather(wide_table, key = output, value = value,
                                    -dplyr::one_of(col_datetime))
    } else {
        long_table <- tidyr::gather(wide_table, key = output, value = value,
                                    -dplyr::one_of(group, col_datetime))
    }
    return(long_table)
}
# }}}1
# wide_table{{{1
wide_table <- function(long_table, group = NULL) {
    long_table <- standardize_long_table(long_table, exclude = group)
    wide_table <- tidyr::spread(long_table, key = output, value = value)
    return(wide_table)
}
# }}}1
# wide_to_xts{{{
wide_to_xts <- function (wide_table, tz = Sys.timezone(), short_name = FALSE) {
    wide_table <- standardize_wide_table(wide_table)

    col_datetime <- get_date_col(wide_table)
    # Find the name of non numeric columns.
    col_non_num <- names(purrr::discard(dplyr::select(wide_table, -one_of(col_datetime)),
                                         is.numeric))
    if (assertthat::not_empty(col_non_num)) {
        warning("Non numeric columns below will be dropped during conversion:\n",
                c_name(col_non_num), call. = FALSE)
        wide_table <- dplyr::select(wide_table, -one_of(col_non_num))
    }

    if (short_name) {
        short_names <- stringr::str_replace(colnames(wide_table), "\\s\\[.*\\]\\(.*\\)$", "")
        wide_table <- purrr::set_names(wide_table, short_names)
    }

    # Do the conversion by using intermediate the dataframe format to avoid date
    # column duplicated.
    df <- data.frame(wide_table, row.names = col_datetime)
    xts <- xts::as.xts(df, tzone = tz)
    return(xts)
}
# }}}
# ggplot_line{{{
ggplot_line <- function (data, x, y, color = NULL, group = NULL, facet = NULL) {
    p <- ggplot(data, aes_string(x = x, y = y)) + theme_bw()
    p <- p + geom_line(size = 1)

    if (!is.null(color)) {
        p <- p + aes_string(color = color)
    }

    if (!is.null(group)) {
        p <- p + aes_string(group = group)
    }

    if (!is.null(facet)) {
        # facet <- rlang::parse_quosure(facet)
        facet <- as.formula(facet)
        p <- p+ facet_grid(facets = facet, scales = "free")
    }
    p
}
# }}}
# multi_dygraphOutput{{{
multi_dygraphOutput <- function (outputIds, width = "100%", height = "400px") {
    outputIds <- as.character(outputIds)
    multi_dygraphOutput_list <- purrr::map(outputIds, dygraphs::dygraphOutput, width = width, height = height)
    multi_dygraphOutput_tagLists <- purrr::invoke(shiny::tagList, multi_dygraphOutput_list)
    return(multi_dygraphOutput_tagLists)
}
# }}}
# get_choice_num{{{
get_choice_num <- function (choice_list) {
    flat_1 <- purrr::map(choice_list, ~purrr::flatten_chr(.x))
    flat_2 <- purrr::flatten_chr(flat_1)
    n <- length(flat_2)
    return(n)
}
# }}}
# if_then_else{{{
if_then_else <- function (.if, .then, .else) {
    if (.if) {
        .then
    } else {
        .else
    }
}
# }}}
# create_source_code{{{
create_source_code <- function (data_name, data_name_prefix, group, col_datetime, time_range,
                                cases, outputs, plot_group, plot_color, plot_facet) {
    code_data_check <-
        glue("
             # OUTPUT SELECTION
             ## Check if data meets the requirements.
             {data_name_prefix}_wide_table <- standardize_wide_table({data_name}, exclude = {if_then_else(is.null(group), 'NULL', c_name(group))})\n
             ")

    code_data_time_filtered <-
        glue("
             ## Filter data according to selected time range.
             {data_name_prefix}_time_filtered <- dplyr::filter({data_name_prefix}_wide_table,
                                                 {col_datetime} >= '{time_range[1]}' &
                                                 {col_datetime} <  '{time_range[2]}')\n
             ")

    code_data_group_filtered <-
        glue("
             {data_name_prefix}_group_filtered <- dplyr::filter({data_name_prefix}_time_filtered,
                                                  {group} %in% {glue('c({c_name(cases)})')})\n
             ")

    code_name_before <- if_then_else(rlang::is_empty(code_data_group_filtered),
                                     glue('{data_name_prefix}_time_filtered'),
                                     glue('{data_name_prefix}_group_filtered'))
    code_data_selected <-
        glue("
             ## Filter data according to selected outputs.
             {data_name_prefix}_selected <- dplyr::select({code_name_before}, {if_then_else(is.null(group), '', glue({group}, ', '))}{col_datetime},
             {paste0('    `', outputs, '`', collapse = ',\n')})\n\n
             ")

    code_data_ggplot <-
        glue("
             # PLOT SELECTED OUTPUTS USING GGPLOT2
             ## Gather data for use in `ggplot2`.
             ### Get output info of selected data.
             {data_name_prefix}_output_info <- eplusr::get_output_info({data_name_prefix}_selected)
             ### Change selected data into long table format.
             {data_name_prefix}_long_table <- tidyr::gather({data_name_prefix}_selected, key = output, value = value,
                                              {if_then_else(is.null(group), '', glue('-', {group}, ', '))}-{col_datetime})
             ### Join output info data and long table data.
             {data_name_prefix}_full_join <- dplyr::full_join({data_name_prefix}_long_table, {data_name_prefix}_output_info)
             ### Delete rows containing NAs.
             {data_name_prefix} <- tidyr::drop_na({data_name_prefix}_full_join)\n
             ")

    code_ggplot_init <-
        glue("
             ## Initialize ggplot object
             p_{data_name_prefix} <- ggplot({data_name_prefix}, aes(x = {col_datetime}, y = value))\n
             ")

    code_ggplot_line <-
        glue("
             ## Add line plot
             p_{data_name_prefix} <- p_{data_name_prefix} + geom_line(size = 1)\n
             ")

    code_ggplot_color <-
        glue("
             ## Add color aes.
             p_{data_name_prefix} <- p_{data_name_prefix} + aes(color = {plot_color})\n
             ")
    code_ggplot_group <-
        glue("
             ## Add group aes.
             p_{data_name_prefix} <- p_{data_name_prefix} + aes(group = {plot_group})\n
             ")
    code_ggplot_facet <-
        glue("
             ## Add facets.
             p_{data_name_prefix} <- p_{data_name_prefix} + facet_grid(facets = {plot_facet}, scales = 'free')\n
             ")

    code_ggplot_theme <-
        glue("
             ## Set theme
             p_{data_name_prefix} <- p_{data_name_prefix} + theme_bw() +
                 theme(legend.position = 'bottom',
                       legend.background = element_rect(color = 'black'),
                       legend.key = element_rect(color = 'gray'),
                       strip.background = element_rect(fill = 'white', color = 'black'))
             ")

    source_code <- paste0(code_data_check,
                          code_data_time_filtered,
                          code_data_group_filtered,
                          code_data_selected,
                          code_data_ggplot,
                          code_ggplot_init,
                          code_ggplot_line,
                          code_ggplot_group,
                          code_ggplot_color,
                          code_ggplot_facet,
                          code_ggplot_theme,
                          sep = "\n",
                          collapse = "\n")
    return(source_code)
}
# }}}
