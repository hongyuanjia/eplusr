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
edit_epat <- function (json, parse = FALSE) {

    library(shiny)
    library(shinyFiles)
    library(shinyjs)
    library(shinycssloaders)
    library(DT)
    library(miniUI)

    job <- read_epat(json)

    # UI{{{2
    ui <- miniPage(
        useShinyjs(),
        theme = shinythemes::shinytheme("flatly"),
        gadgetTitleBar("Parametric Job Editor"),
        miniTabstripPanel(
            # File input{{{3
            miniTabPanel("File Input", icon = icon("file"),
                miniContentPanel(
                    div(class = "container-fluid",
                        div(class = "row", style = "no-gutters",
                            div(class = "col-sm-9", style="display:inline-block", textInput("model_path", label = "IDF/IMF model template:", value = job$idf_path, width = "100%")),
                            div(style="display:inline-block", shinyFilesButton("model_sel", label = "...", title = "Please select an IDF/IMF model template", multiple = FALSE)),
                            div(style="display:inline-block", actionButton("edit_model", label = "Edit", icon = icon("pencil-square"))),
                            tags$style(type='text/css', "#model_sel {vertical-align: top; margin-top: 25px;}"),
                            tags$style(type='text/css', "#edit_model {vertical-align: top; margin-top: 25px;}")
                        )
                    ),
                    tags$br(),
                    div(class = "container-fluid",
                        div(class = "row", style = "no-gutters",
                            div(class = "col-sm-9", style="display:inline-block", textInput("weather_path", label = "Weather file:", value = job$weather_path, width = "100%")),
                            div(style="display:inline-block", shinyFilesButton("weather_sel", label = "..", title = "Please select a weather file:", multiple = TRUE)),
                            div(style="display:inline-block", actionButton("edit_weather", label = "Edit", icon = icon("pencil-square"))),
                            tags$style(type='text/css', "#weather_sel {vertical-align: top; margin-top: 25px;}"),
                            tags$style(type='text/css', "#edit_weather {vertical-align: top; margin-top: 25px;}")
                        )
                    )
                )
            ),
            # }}}3
            # Parameters{{{3
            miniTabPanel("Parameters", icon = icon("sliders"),
                miniContentPanel(
                    shinycssloaders::withSpinner(DT::dataTableOutput("param_table"), type = 6),
                    fluidRow(
                        column(6, textInput("id", "Field ID*:", value = NULL, width = "100%")),
                        column(6, textInput("name", "Field Name*:", value = NULL, width = "100%"))
                    ),
                    textInput("tag", "Search tag*:", value = NULL, width = "100%"),
                    textInput("desc", "Description:", value = NULL, width = "100%"),
                    fluidRow(
                        column(8, textInput("values", "Value Expressions*:", value = NULL, width = "100%")),
                        column(4, actionButton("preview", label = "Preview values", icon = icon("eye")))
                    ),
                    tags$style(type='text/css', "#preview {vertical-align: middel; margin-top: 25px;}"),
                    shinyjs::hidden(div(id = "preview_values", class = "frame", style = "border-style: solid;",
                        tagAppendAttributes(textOutput("preview_values"), style = "white-space:pre-wrap;"))),
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
            ),
            # }}}3
            # Settings{{{3
            miniTabPanel("Settings", icon = icon("wrench"),
                miniContentPanel(
                    div(class = "container-fluid",
                        div(class = "row", style = "no-gutters",
                            div(class = "col-sm-10", style = "display:inline-block", textInput("eplus_path", label = "EnergyPlus location:", value = job$eplus_path, width = "100%")),
                            div(style = "display:inline-block", shinyDirButton("eplus_sel", label = "...", title = "Please select an EnergyPlus location:")),
                            tags$style(type='text/css', "#eplus_sel {vertical-align: top; margin-top: 25px;}")
                        )
                    ),
                    div(class = "container-fluid",
                        div(class = "row", style = "no-gutters",
                            div(class = "col-sm-1", style = "display:inline-block",
                                selectInput("parallel_num", label = "Parallel job:", choices = c(1:8), selected = 4)
                            )
                        )
                    ),
                    div(class = "container-fluid",
                        div(class = "row", style = "no-gutters",
                            div(class = "col-sm-10", style = "display:inline-block", textInput("wd_path", label = "Working dir:", value = job$wd_path, width = "100%")),
                            div(style = "display:inline-block", shinyDirButton("wd_sel", label = "...", title = "Working dir:")),
                            tags$style(type='text/css', "#wd_sel {vertical-align: top; margin-top: 25px;}")
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

        project <- reactiveValues()

        # shinyFileChoose for model and weather{{{3
        shinyFileChoose(input, 'model_sel', roots = getVolumes(), filetypes = c("idf", "IDF", "imf", "IMF"))
        shinyFileChoose(input, 'weather_sel', roots = getVolumes(), filetypes = c("epw", "EPW"))
        # }}}3

        # Get reactive values of model and weather{{{3
        model_path <- reactive({
            model_path <- parseFilePaths(getVolumes(), input$model_sel)
            if (is.null(model_path)) return(NULL)
            if (identical(model_path, "")) return(NULL)
            return(model_path)
        })
        weather_path <- reactive({
            weather_path <- parseFilePaths(getVolumes(), input$weather_sel)
            if (is.null(weather_path)) return(NULL)
            if (identical(weather_path, "")) return(NULL)
            return(weather_path)
        })
        # }}}3

        # Update model and weather path according to shinyFileChoose{{{3
        observeEvent(input$model_sel,
            {
                if (!is.null(model_path())) {
                    updateTextInput(session, "model_path", value = model_path()$datapath)
                }
            }
        )
        observeEvent(input$weather_sel,
            {
                if (!is.null(weather_path())) {
                    updateTextInput(session, "weather_path", value = weather_path()$datapath)
                }
            }
        )
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
        param_fixed <- reactive({
            param_fixed <- input$fixed_value
            if (is.null(param_fixed)) return(NULL)
            if (identical(param_fixed, "")) return(NULL)
            return(param_fixed)
        })
        project$params <- reactive(
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
        # }}}3

        # Preview parameter values{{{3
        # Only show the preview panel after the preview button is clicken for the
        # first time.
        observeEvent(
            input$preview,
            shinyjs::show("preview_values")
        )
        observeEvent(
            input$preview,
            {
                param_values <- isolate(param_values())

                eval_results <- preview_values(param_values)

                # Update preview values
                output$preview_values <- renderText({
                    if (is.null(param_values())) {
                        "Please input R expressions with format 'R(expr)' or input any jEplus parameter definition."
                    } else {
                        eval_results
                    }
                })

                # Update fix value selection
                eval_error <- "Invalid expression!"
                if (identical(eval_results, eval_error)) {
                    updateSelectInput(session, "fixed_value", choices = 0L)
                } else {
                    updateSelectInput(session, "fixed_value", choices = c(0L, seq_along(eval_results)))
                }
            }
        )
        # }}}3

        # Show a parameter table{{{3
        param_table <- reactiveValues(
            table = job$param_table
        )
        output$param_table <- DT::renderDataTable(
            {
                param_table$table
            }, selection = "single", option = list(dom = "t")
        )
        proxy = DT::dataTableProxy("param_table")
        # Add parameters{{{4
        observeEvent(input$add_param,
            {
                # shinyjs::show("div_param_table")
                shinyjs::reset("div_param_input")

                # Check required fields before save parameters{{{5
                required_fields <- list(
                    ID = isolate(param_id()),
                    Name = isolate(param_name()),
                    `Search Tag` = isolate(param_tag()),
                    `Value Expressions` = isolate(param_values())
                )

                check_null <- purrr::map(seq_along(required_fields), ~{
                    field <- required_fields[[.x]]
                    name <- names(required_fields)[.x]
                    if (is.null(field)) {
                        div(icon("exclamation-sign", lib = "glyphicon"), paste0('Parameter attribute "', name, '" is missing.'))
                    }
                })

                check_null <- purrr::keep(check_null, purrr::negate(is.null))
                if (length(check_null) > 0L) {
                    shinyjs::show("field_required")

                    error_msg <- paste(check_null, collapse = "<br>")
                    showNotification(ui = div(h5(strong("Error")), check_null), duration = 15, type = "error")
                # }}}5
                # Show the parameter table{{{5
                } else {
                    param_table$table <- dplyr::bind_rows(param_table$table, isolate(project$params()))
                    param_table$table <- tidyr::drop_na(param_table$table, ID, Name, `Search Tag`, `Value Expressions`)
                    replaceData(proxy, param_table$table, resetPaging = FALSE)
                }
                # }}}5
            }
        )
        # }}}4
        # Save parameters{{{4
        observeEvent(input$save_param,
            {
                # Check required fields before save parameters{{{5
                required_fields <- list(
                    ID = isolate(param_id()),
                    Name = isolate(param_name()),
                    `Search Tag` = isolate(param_tag()),
                    `Value Expressions` = isolate(param_values())
                )

                check_null <- purrr::map(seq_along(required_fields), ~{
                    field <- required_fields[[.x]]
                    name <- names(required_fields)[.x]
                    if (is.null(field)) {
                        paste0('Parameter attribute "', name, '" is missing.')
                    }
                })

                check_null <- purrr::keep(check_null, purrr::negate(is.null))

                if (length(check_null) > 0L) {
                    shinyjs::show("field_required")

                    error_msg <- paste(check_null, collapse = "<br>")
                    shinyBS::createAlert(session, anchorId = "field_required", alertId = "required_alert",
                         title = "Missing Values of Required Fields", content = error_msg,
                         style = "danger", append = FALSE)
                # }}}5
                # Show the parameter table{{{5
                } else {
                    s <- input$param_table_rows_selected
                    if (!is.null(s)) {
                        row <- isolate(project$params())
                        param_table$table[s,] <- row
                        replaceData(proxy, param_table$table, resetPaging = FALSE)
                    } else {
                        showNotification(ui = div(h5(strong("Error")),
                                                  div(icon("exclamation-sign", lib = "glyphicon"),
                                                      paste0("Please select a parameter from the table before save."))),
                                         duration = 15, type = "error")
                    }
                }
                # }}}5
            }
        )
        # }}}4
        # }}}3

        # Update inputs according to selected line of the parameter table{{{3
        observeEvent(input$param_table_rows_selected,
            {
                 s <- input$param_table_rows_selected
                 if (!is.null(s)) {
                     row <- param_table$table[s,]
                     updateTextInput(session, "id", value = row[["ID"]])
                     updateTextInput(session, "name", value = row[["Name"]])
                     updateTextInput(session, "tag", value = row[["Search Tag"]])
                     updateTextInput(session, "desc", value = row[["Description"]])
                     updateTextInput(session, "values", value = row[["Value Expressions"]])
                     updateSelectInput(session, "fixed_value",
                                       choices = c(0L, seq_along(preview_values(row[["Value Expressions"]]))),
                                       selected = as.integer(row[["Fixed Value"]]))
                 }

            }
        )
        # }}}3

        # Delete the selected parameter{{{3
        observeEvent(input$delete_param,
            {
                 s <- input$param_table_rows_selected
                 if (!is.null(s)) {
                     param_table$table <- param_table$table[-s,]
                 } else {
                    showNotification(ui = div(h5(strong("Error")),
                                              div(icon("exclamation-sign", lib = "glyphicon"),
                                                  paste0("Please select a parameter from the table before delete."))),
                                     duration = 15, type = "error")
                 }

            }
        )
        # }}}3

        # Copy the selected parameter{{{3
        observeEvent(input$copy_param,
            {
                 s <- input$param_table_rows_selected
                 if (!is.null(s)) {
                     row <- param_table$table[s,]
                     param_table$table <- dplyr::bind_rows(param_table$table, row)
                 } else {
                    showNotification(ui = div(h5(strong("Error")),
                                              div(icon("exclamation-sign", lib = "glyphicon"),
                                                  paste0("Please select a parameter from the table before copy."))),
                                     duration = 15, type = "error")
                 }

            }
        )
        # }}}3

        # Delete the selected parameter{{{3
        observeEvent(
            input$delete_param,
            {
                 s <- input$param_table_rows_selected
                 if (!is.null(s)) {
                     param_table$table <- param_table$table[-s,]
                 }
            }
        )
        # }}}3

        # shinyDirChoose for EnergyPlus and Working dir{{{3
        shinyDirChoose(input, "eplus_sel", roots = getVolumes())
        shinyDirChoose(input, "wd_sel", roots = getVolumes())
        # }}}3

        # Get reactive values of EnergyPlus location and working directory{{{3
        eplus_sel <- reactive({
            eplus_sel <- parseDirPath(getVolumes(), input$eplus_sel)
            if (is.null(eplus_sel)) return(NULL)
            if (identical(eplus_sel, "")) return(NULL)
            return(eplus_sel)
        })
        wd_sel <- reactive({
            wd_sel <- parseDirPath(getVolumes(), input$wd_sel)
            if (is.null(wd_sel)) return(NULL)
            if (identical(wd_sel, "")) return(NULL)
            return(wd_sel)
        })
        # }}}3

        # Update model and weather path according to shinyFileChoose{{{3
        observeEvent(input$eplus_sel,
            {
                if (!is.null(eplus_sel())) {
                    updateTextInput(session, "eplus_path", value = eplus_sel())
                }
            }
        )
        observeEvent(input$wd_sel,
            {
                if (!is.null(wd_sel())) {
                    updateTextInput(session, "wd_path", value = wd_sel())
                }
            }
        )
        # }}}3

        # Return project contents when press Done{{{3
        observeEvent(input$done,
            {
                project <- list(
                    epat_ver = "0.0.0.9",
                    idf_path = isolate(input$model_path),
                    weather_path = isolate(input$weather_path),
                    param_table = isolate(param_table$table),
                    eplus_path = isolate(input$eplus_path),
                    wd_path = isolate(input$wd_path),
                    parallel_num = isolate(input$parallel_num)
                )

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

#####################################
#  helper function for edit_epat()  #
#####################################
# read_epat{{{1
read_epat <- function(json) {
    job <- jsonlite::fromJSON(json)
    # if (is.null(job$epat_ver)) {
    #     stop("Input is not a valid EPAT project file.", call. = FALSE)
    # }
    attr(job, "job_type") <- "epat"
    return(job)
}
# }}}1
    # preview_values{{{2
    preview_values <- function (string) {
        # Check if a R expression string
        if (stringr::str_detect(string, "^R\\(.*\\)$")) {
            string_replaced <- stringr::str_replace_all(string, "^R\\((.*)\\)$", "\\1")
            eval_error <- "Invalid R expression!"
            eval_results <- tryCatch(
                # Eval the text in a new environment parent from an empty
                # environment.
                eval(parse(text = string_replaced), env = environment()),
                # eval(parse(text = param_values), env = new.env(parent = emptyenv())),
                error = function (e) eval_error
            )
        } else {
            # TODO: Add support for other types of jEPlus value strings
            string_replaced <- stringr::str_replace_all(string, "[\\{\\}]", "")
            # Check if the parametric value is a numeric seq.
            regex_seq <- "\\[(\\d+(?:\\.\\d+)*):(\\d+(?:\\.\\d+)*):(\\d+(?:\\.\\d)*)\\]"
            idx_value_seq <- stringr::str_detect(string_replaced, regex_seq)
            param_value <- stringr::str_split(string_replaced, "(\\s)*,(\\s)*")
            param_value <- purrr::map2(idx_value_seq, seq_along(param_value),
                                        ~{if (.x) {
                                             from <- as.numeric(stringr::str_replace(param_value[[.y]], regex_seq, "\\1"))
                                             by <- as.numeric(stringr::str_replace(param_value[[.y]], regex_seq, "\\2"))
                                             to <- as.numeric(stringr::str_replace(param_value[[.y]], regex_seq, "\\3"))
                                             as.character(seq(from = from , to = to, by = by))
                                         } else {
                                             param_value[[.y]]
                                         }})
           eval_results <- purrr::flatten_chr(param_value)

        }
        return(eval_results)
    }
    # }}}2

# parse_epat{{{1
parse_epat <- function (info) {
    # Validation checking

    # Get parameter info.
    params <- info[["param_table"]]

    param_id <- params[["ID"]]
    param_name <- params[["Name"]]

    param_field <- stringr::str_split(params[["Search Tag"]], "\\|")
    param_field <- purrr::set_names(param_field, param_name)

    param_value <- purrr::map(params[["Value Expressions"]], preview_values)

    # Get selected parameter values.
    param_value_selected <- as.integer(params[["Fixed Value"]])
    param_value <- purrr::map2(param_value_selected, param_value, ~{if (.x > 0) .y <- .y[.x] else .y})

    param_value <- purrr::map(param_value, ~stringr::str_split(.x, "(\\s)*\\|(\\s)*"))
    param_value <- purrr::set_names(param_value, param_name)

    # Create case names according to parameter names.
    case_names <- purrr::map2(param_id, param_value, ~paste0(.x, seq_along(.y)))
    case_names <- data.table::rbindlist(purrr::cross_n(case_names))
    case_names <- purrr::map_chr(seq(1:nrow(case_names)), ~paste(case_names[.x], collapse = "_"))

    # Get all combination of case values.
    param_value <- purrr::cross_n(param_value)
    param_value <- purrr::set_names(param_value, case_names)

    # Get input file info.
    idf_path <- info[["idf_path"]]
    wthr_path <- info[["weather_path"]]

    # Get other misc info
    eplus_path <- info[["eplus_path"]]
    wd_path <- info[["wd_path"]]
    parallel_num <- info[["parallel_num"]]

    sim_info <- list(idf_path = idf_path, weather_path = wthr_path,
                     param_field = param_field, param_value = param_value,
                     eplus_path = eplus_path, wd_path = wd_path, parallel_num = parallel_num)

    attr(sim_info, "job_type") <- "epat"

    return(sim_info)
}
# }}}1

#######################################
#  helper function for show_output()  #
#######################################
# c_name{{{
c_name <- function (x) {
    c_name <- paste0("'", x ,"'", collapse = ", ")
    return(c_name)
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
    check_df(data)

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
