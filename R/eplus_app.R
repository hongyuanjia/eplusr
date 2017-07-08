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
edit_epat <- function (json, parse = TRUE) {

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

#####################################
#  helper function for edit_epat()  #
#####################################
# read_epat{{{1
read_epat <- function(json) {
    job <- jsonlite::fromJSON(json)
    if (is.null(job$epat_ver)) {
        stop("Input is not a valid EPAT project file.", call. = FALSE)
    }
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
    case_names <- map_chr(seq(1:nrow(case_names)), ~paste(case_names[.x], collapse = "_"))

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
