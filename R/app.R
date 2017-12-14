#' A Shiny app for EnergyPlus Parametric Analysis in R.
#'
#' \code{epat} will launch a shiny app called EPAT (EnergyPlus Parametric
#' Analysis Toolkit).
#'
#' @export
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
                         out = c("wide_table", "ggplot2", "state")) {

    library(shiny)
    library(ggplot2)

    # Get the input data name for source code creation
    data_name <- deparse(substitute(data))

    if (!is.null(group)) {
        assertthat::assert_that(assertthat::are_equal(length(group), 1L),
            msg = "'group' should be a single character string."
        )
    }

    # Standardize input data
    data <- standardize_wide_table(data, exclude = group)
    # names(data) <- stringr::str_to_lower(names(data))

    col_datetime <- get_date_col(data)
    output_info <- get_output_info(data)
    selects <- get_select(output_info)
    col_opt <- setdiff(c(group, names(selects)), c("freq"))
    names(col_opt) <- stringr::str_to_title(col_opt)

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
    com_choices <- col_opt
    if (!is.null(group)) {
        com_choices <- c(com_choices, paste0(group, " & ", c("output", "variable", "key")))
        names(com_choices) <- stringr::str_to_title(com_choices)
    }
    group_choices <- c("No Group" = "No group", com_choices)
    group_default <- stringr::str_to_title(group) %||% "variable"

    # Plot color
    color_choices <- c("No color" = "No color", com_choices)
    color_default <- group_default

    # Plot facet row
    facet_row_choices <- c("No facet" = "No facet", col_opt)
    facet_row_default <- "unit"

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
                                msg = paste0("Unknown state variable: ", csQuote(extras), "."))
    }

    if (is.null(state)) {
        state <- list()
    }

    init_date_start <- state[["date_start"]] %||% date_start_default
    init_date_start <- state[["date_start"]] %||% date_start_default
    init_date_end   <- state[["date_end"]] %||% date_end_default
    init_time_start <- state[["time_start"]] %||% time_start_default
    init_time_end   <- state[["time_end"]] %||% time_end_default
    init_case       <- state[["case"]] %||% case_default
    init_variable   <- state[["variable"]] %||% variable_default
    init_key        <- state[["key"]] %||% key_default
    init_plot_group <- state[["plot_group"]] %||% group_default
    init_plot_color <- state[["plot_color"]] %||% color_default
    init_facet_row  <- state[["facet_row"]] %||% facet_row_default
    init_facet_col  <- state[["facet_col"]] %||% facet_col_default
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
                        tags$hr(),

                        shinyWidgets::pickerInput("plot_color", label = "Color:",
                                                  choices = color_choices, selected = init_plot_color, multiple = FALSE,
                                                  options = list(style = "btn-primary",
                                                                 `live-search` = TRUE,
                                                                 size = 5)
                                                  ),
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
                        actionButton("update_option", label = "Update")
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
                                 shinyWidgets::useSweetAlert(),
                                 tags$br(),
                                 shinyAce::aceEditor(outputId = "source_code", mode = "r",
                                           theme = "monokai", vimKeyBinding = TRUE,
                                           height = "600px", readOnly = TRUE,
                                           autoComplete = "live")
                                 # verbatimTextOutput("source_code"),
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
            return(plot_group)
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
            return(plot_color)
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
            return(facet_row)
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
            return(facet_col)
        })
        plot_facet <- reactive({
            if (all(facet_row() == ".", facet_col() == ".")) {
                return(NULL)
            } else {
                plot_facet <- paste0(facet_row(), "~", facet_col())
            }
            return(plot_facet)
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

        # Output info for selected dataset
        info_filtered <- reactive({
            info_filtered <- filter_output_info(info = output_info,
                key = key(), variable = variable()
            )
            return(info_filtered)
        })

        wide_tbl <- reactive({
            # Subset by time range
            if (!is.null(time_range())) {
                time_range = format(time_range(), "%F %X")
                data <- dplyr::filter_at(data, dplyr::vars(col_datetime),
                    dplyr::all_vars(. >= time_range[1] & .< time_range[2])
                )
            }
            # Subset by 'case'
            if (!is.null(case())) {
                cases = case()
                data <- dplyr::filter_at(data, dplyr::vars(group),
                    dplyr::all_vars(. %in% cases)
                )
            }

            data_filtered <- dplyr::select(data, dplyr::one_of(group, col_datetime,
                info_filtered()[["output"]])
            )

            return(data_filtered)
        })

        long_tbl <- reactive({
            long_tbl <- wide_to_long(wide_tbl(), group = group)
            return(long_tbl)
        })

        data_plot <- reactive({
            data_plot <- dplyr::full_join(long_tbl(), info_filtered())
            if (!is.null(group)) {
                data_plot <- mutate(data_plot,
                    rlang::UQ(paste0(group, " & output")) := paste0("[", rlang::UQ(rlang::sym(group)), "]", output),
                    rlang::UQ(paste0(group, " & variable")) := paste0("[", rlang::UQ(rlang::sym(group)), "]", variable),
                    rlang::UQ(paste0(group, " & key")) := paste0("[", rlang::UQ(rlang::sym(group)), "]", key)
                )
            }
            # data_plot <- tidyr::drop_na(data_plot)
            return(data_plot)
        })

        data_units <- reactive({
            data_units <- purrr::flatten_chr(get_select(info_filtered(), "unit"))
            return(data_units)
        })

        source_code <- reactive({
            time_range <- format(time_range(), "%F %X")
            create_source_code(data_name = data_name, data_name_prefix = data_name_prefix(),
                               group = group, col_datetime = col_datetime,
                               time_range = time_range, cases = case(),
                               outputs = info_filtered()[["output"]],
                               plot_group = plot_group(),
                               plot_color = plot_color(),
                               plot_facet = plot_facet())
        })

        data_plot_splitted <- reactive({
            base::split(data_plot(), data_plot()[["unit"]])
        })

        # data_xts <- reactive({
            # purrr::map(names(data_plot_splitted()),
               # ~{
                    # data_plot <- data_plot_splitted()[[.x]]
                    # wide_table_splitted <- long_to_wide(data_plot, group = group)
                    # xts_splitted <- wide_to_xts(wide_table_splitted, short_name = TRUE)
                # }
            # )
        # })
        # }}}
        # Create plot using `ggplot2` {{{
        plot_ggplot <- eventReactive(
            c(input$update_time, input$update_var, input$update_option),
            {
                p <- ggplot_line(data = data_plot(),
                                 x = col_datetime, y = "value",
                                 group = plot_group(),
                                 color = plot_color(),
                                 facet = plot_facet()
                )
                p <- p + labs(x = NULL, y = NULL)
                p <- p + theme(legend.position = "bottom",
                    legend.background = element_rect(color = "black"),
                    legend.key = element_rect(color = "gray"),
                    strip.background = element_blank(),
                    strip.placement = "outside"
                ) +
                guides(color = guide_legend(title.position = "top", ncol = 1))

                return(p)
            }, ignoreNULL = FALSE
        )
        # }}}
        # Create plot using `plotly` {{{
        plot_plotly <- reactive({
            p <- plotly::ggplotly(plot_ggplot(), dynamicTicks = TRUE)

            p <- plotly::style(p,
                line = list(width= 0.5)
            )
            p
        })
        # }}}
        # Plot {{{
        output$ggplot <- renderPlot({plot_ggplot()})
        output$plotly <- plotly::renderPlotly({plot_plotly()})

                # # Generate dygraphOutput
                # output$dygraph <- renderUI({
                    # multi_dygraphOutput(outputIds = ids_dygraph())
                # })
#
                # # Create data for dygraphs
                # for (i in seq_along(ids_dygraph())) {
                    # # Need local so that each item gets its own number. Without it, the value
                    # # of i in the renderPlot() will be the same across all instances, because
                    # # of when the expression is evaluated.
                    # local({
                        # my_i <- i
                        # output[[ids_dygraph()[my_i]]] <- dygraphs::renderDygraph({
                            # isolate(plot_dygraphs())[[my_i]]
                        # })
                    # })
                # }
        # }}}
        # # Create plot using `dygraphs` {{{
        # ids_dygraph <- reactive({paste0("dy_", seq_along(data_units))})
        # plot_dygraphs <- reactive({
            # ids_dygraph <- paste0("dy_", seq_along(data_units()))
#
            # plot_dygraph <- purrr::map(names(data_plot_splitted),
               # ~{
                    # plot_dygraph <-
                        # dygraphs::dygraph(dat_xts(), group = "dygraphs",
                                # ylab = paste0("[",as.character(.x), "]")) %>%
                        # dygraphs::dyRangeSelector() %>%
                        # dygraphs::dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                        # dygraphs::dyOptions(useDataTimezone = TRUE, axisLineWidth = 1.5) %>%
                        # dygraphs::dyLegend(width = 400, show = "follow", labelsSeparateLines = TRUE) %>%
                        # # dyLegend(width = 400, show = "follow", labelsSeparateLines = TRUE, labelsDiv = "dygraphs_legend") %>%
                        # dygraphs::dyCrosshair(direction = "vertical") %>%
                        # dygraphs::dyCSS(system.file("css", "dygraphs.css", package = "eplusr"))
                    # return(plot_dygraph)
                # }
            # )
            # return(plot_dygraph)
        # })
        # # }}}
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

            shinyWidgets::updatePickerInput(session, inputId = "key",
                choices = choices_key, selected = selected_key,
                choicesOpt = list(icon = rep("glyphicon-triangle-right",
                    get_choice_num(choices_key))
                )
            )
        })
        # }}}
        # Source code actions{{{
        observeEvent(input$update_data_name_prefix,
            {
                shinyAce::updateAceEditor(session, "source_code", value = source_code())
                updateTextInput(session, "data_name_prefix", value = isolate(data_name_prefix()))
            }
        )
        observeEvent(input$copy_code,
            {
                clipr::write_clip(content = isolate(source_code()), object_type = "character")
                shinyWidgets::sendSweetAlert(session = session, title = "Success!",
                    text = "Source Code has been successfully copied to clipboard", type = "success"
                )
            }
        )
        # }}}
        # Debug{{{
        # output$debug <- renderPrint({
            # input$plot_group
        # })
        # output$debug2 <- renderPrint({
            # wide_tbl()
        # })
        # output$debug3 <- renderPrint({
            # long_tbl()
        # })
        # }}}
        # Return project contents when press Done{{{
        observeEvent(input$done,
            {
                stopApp(
                    switch(out,
                        wide_table  = isolate(wide_tbl()),
                        ggplot2     = isolate(plot_ggplot()),
                        # dygraphs    = isolate(plot_dygraphs()),
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

# show_dygraphs {{{1
show_dygraphs <- function (data, group = "output", key = "model_prefix", ylab = "unit") {
    library(shiny)

    plot_table <- create_dygraphs(data, group = group, key = key, ylab = ylab)
    dy_info <- multi_dygraphOutput(plot_table, by = ylab, id_prefix = "dy", col_width = 6L)
    plots <- get_plots(plot_table, by = ylab, plot_col = "dys")
    id_output <- dy_info[["id"]]
    tag_output <- dy_info[["tag"]]

    ui <- shinyUI(
        fluidPage(
            uiOutput("dygraph")
        )
    )

    server <- shinyServer(
        function (input, output, session) {
            # Generate dygraphOutput
            output$dygraph <- renderUI({
                tag_output
            })
            # Create dygraphs
            for (i in seq_along(id_output)) {
                # Need local so that each item gets its own number. Without it, the value
                # of i in the renderPlot() will be the same across all instances, because
                # of when the expression is evaluated.
                local({
                    my_i <- i
                    # plotname <- paste0("Unit: ", data$units[my_i])
                    output[[id_output[my_i]]] <- dygraphs::renderDygraph({
                        plots[[my_i]]
                    })
                })
            }
            # Stop shiny app when closing the web brower.
            session$onSessionEnded(stopApp)
        }
    )

    runGadget(shinyApp(ui, server))
}
# }}}1

#########################################
#  helper function for show_dygraphs()  #
#########################################
# get_plots {{{1
get_plots <- function (plot_table, by = NULL, plot_col = "dys") {
    if (is.null(by)) {
        plots <- plot_table[[plot_col]]
    } else {
        plots_sp <- split(plot_table, plot_table[[by]])
        plots <- purrr::flatten(purrr::map(plots_sp, ~.x[[plot_col]]))
    }

    return(plots)
}
# }}}1
# arrange_plots {{{1
arrange_plots <- function (plots, by = NULL) {
    if (!is.null(by)) {
        n_col_per_row <- purrr::map(split(plots, plots[[by]]), nrow)
    } else {
        n_col_per_row <- nrow(plots)
    }

    return(n_col_per_row)
}
# }}}1
# multi_dygraphOutput {{{1
multi_dygraphOutput <- function (plot_table, by = NULL, id_prefix = "dy", col_width = 6L) {
    arrange_info <- arrange_plots(plot_table, by = by)

    if (!is.null(by)) {
        list_output <- purrr::transpose(purrr::map(seq_along(arrange_info),
            ~{
                id_row <- paste0(id_prefix, "_row_", .x)
                n_cols <- arrange_info[[.x]]
                seq_cols <- 1:n_cols
                id_plot <- paste0(id_row, "_col_", seq_cols)
                col_plot <- map(id_plot,
                    ~column(col_width,
                        shinycssloaders::withSpinner(dygraphs::dygraphOutput(.x))
                        # shinyjqui::jqui_resizabled(
                            # shinycssloaders::withSpinner(dygraphs::dygraphOutput(.x))
                        # )
                    )
                )
                tag_col_plot <- purrr::invoke(tagList, col_plot)
                tag_row_plot <- div(
                    h3(strong(paste0("For ", stringr::str_to_title(by), " [", names(arrange_info[.x]), "]"))),
                    tags$br(),
                    fluidRow(tag_col_plot),
                    # shinyjqui::jqui_sortabled(fluidRow(tag_col_plot)),
                    tags$hr(), tags$br()
                )
                info <- list(id = id_plot, tag = tag_row_plot)
                return(info)
             }
        ))

        id_output <- purrr::flatten_chr(list_output$id)
        tag_output <- div(id = "dygraphs", purrr::invoke(tagList, list_output$tag))
        # tag_output <- shinyjqui::jqui_sortabled(
            # div(id = "dygraphs", purrr::invoke(tagList, list_output$tag))
        # )
    } else {
        id_plot <- paste0(id_prefix, "_", 1:arrange_info)
        col_plot <- map(id_plot,
            ~column(12,
                shinycssloaders::withSpinner(dygraphs::dygraphOutput(.x))
                # shinyjqui::jqui_resizabled(
                    # shinycssloaders::withSpinner(dygraphs::dygraphOutput(.x))
                # )
            )
        )
        tag_col_plot <- purrr::invoke(tagList, col_plot)
        tag_row_plot <- div(id = "dygraphs", tags$br(), fluidRow(tag_col_plot), tags$hr(), tags$br())
        # tag_row_plot <- shinyjqui::jqui_sortabled(
            # div(id = "dygraphs", tags$br(), fluidRow(tag_col_plot), tags$hr(), tags$br())
        # )

        id_output <- id_plot
        tag_output <- tag_row_plot
    }

    results <- list(id = id_output, tag = tag_output)

    return(results)
}
# }}}1
# create_dygraphs {{{1
create_dygraphs <- function (wide_table, group = "model_prefix", key = "output",
                             ylab = "variable") {

    col_datetime <- get_date_col(wide_table)

    full <- wide_to_full(wide_table, group = group)

    full_nest <- tidyr::nest(
        dplyr::select(
            dplyr::group_by(full, rlang::UQ(rlang::sym(key))),
            dplyr::one_of(c(group, col_datetime, key, "value"))
        )
    )

    full_xts <- dplyr::mutate(full_nest,
        xts = purrr::map(data, ~wide_to_xts(long_to_wide(.x, key = group)))
    )

    full_xts <- dplyr::full_join(get_output_info(wide_table), full_xts)

    full_dys <- dplyr::mutate(full_xts,
        dys = purrr::pmap(
            list(xts, rlang::UQ(rlang::sym(key)), rlang::UQ(rlang::sym(ylab))),
            function (data, main, ylab) {
                dygraph_output(data = data, main = main, ylab = ylab)
            }
        )
    )

    return(full_dys)
}
# }}}1

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
                eval(parse(text = string_replaced), envir = environment()),
                # eval(parse(text = param_values), envir = new.env(parent = emptyenv())),
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
                  key = stringr::str_extract(output_names, ".*(?=:)"),
                  # key = stringr::str_to_lower(stringr::str_extract(output_names, ".*(?=:)")),
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
# ggplot_line{{{
ggplot_line <- function (data, x, y, color = NULL, group = NULL, facet = NULL) {
    p <- ggplot(data, aes_string(x = paste0("`", x, "`"), y = paste0("`", y, "`"))) + theme_bw()
    p <- p + geom_line(size = 1)

    if (!is.null(color)) {
        p <- p + aes_string(color = paste0("`", color, "`"))
    }

    if (!is.null(group)) {
        p <- p + aes_string(group = paste0("`", group, "`"))
    }

    if (!is.null(facet)) {
        # facet <- rlang::parse_quosure(facet)
        facet <- as.formula(facet)
        p <- p+ facet_grid(facets = facet, scales = "free", switch = "y", labeller = label_wrap_gen())
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
# standardize_wide_table{{{
standardize_wide_table <- function (data, eplus = FALSE, exclude = NULL) {
    # Check if input is a data.frame.
    assertthat::assert_that(is.data.frame(data),
        msg = "Input should be a data.frame object."
    )

    # Check if input has a datetime column.
    col_datetime <- get_date_col(data)
    assertthat::assert_that(assertthat::not_empty(col_datetime),
        msg = "'data' does not have any datetime column."
    )

    # If multiple datetime columns found, the first one will be used.
    if (length(col_datetime) > 1L) {
        warning(msg("Multiple datetime columns found. The first one will be used
            and others will be dropped."), call. = FALSE)
        data <- dplyr::select(data, -dplyr::one_of(col_datetime[-1]))
    }

    # Check if all column names meet the EnergyPlus standard output name format.
    if (eplus) {
        col_names <- colnames(data)
        if (!is.null(exclude)) {
            col_missing <- setdiff(exclude, col_names)
            assertthat::assert_that(rlang::is_empty(col_missing),
                msg = msg("Invalid 'exclude'. Could not find column ",
                          csQuote(col_missing), ".")
            )
            col_exclude <- c(exclude, col_datetime)
        } else {
            col_exclude <- col_datetime
        }
        col_names_check <- setdiff(col_names, col_exclude)
        col_regex <- "^.*?:.*?\\s\\[.*\\]\\(.*\\)$"
        col_idx <- stringr::str_detect(col_names_check, col_regex)
        col_not <- col_names_check[!col_idx]
        # Columns not met will be dropped.
        if (assertthat::not_empty(col_not)) {
            warning(
                msg("Columns that do not have EnergyPlus-flavoured output names
                    be dropped:\n", csQuote(col_not), ". You can set 'eplus' to
                    FALSE or specify them in 'exclude' if you want to keep
                    them."), call. = FALSE)
        }
        data <- dplyr::select(data, -dplyr::one_of(col_not))
    }

    return(data)
}
# }}}
# standardize_long_table{{{
standardize_long_table <- function (data, key = "output", value = "value",
                                    eplus = FALSE, exclude = NULL) {
    # Check if input is a data.frame.
    assertthat::assert_that(is.data.frame(data),
        msg = "Input should be a data.frame object.")

    # Check if input has a datetime column.
    col_datetime <- get_date_col(data)
    assertthat::assert_that(assertthat::not_empty(col_datetime),
        msg = "'data' does not have any datetime column.")

    # If multiple datetime columns found, the first one will be used.
    if (length(col_datetime) > 1L) {
        warning(msg("Multiple datetime columns found. The first one will be used
            and others will be dropped."), call. = FALSE)
        data <- dplyr::select(data, -dplyr::one_of(col_datetime[-1]))
    }

    # Check if all column names meet the format.
    if (!is.null(exclude)) {
        col_missing <- exclude[is.na(match(exclude, colnames(data)))]
        assertthat::assert_that(rlang::is_empty(col_missing),
            msg = msg(paste0("Invalid 'exclude'. Could not find column ",
                          csQuote(col_missing), ".")))
    }
    col_exclude <- c(col_datetime, exclude)
    col_names <- colnames(data)[!colnames(data) %in% col_exclude]

    col_required <- c(key, value)
    col_not <- col_names[is.na(match(col_names, col_required))]
    col_missing <- col_required[is.na(match(col_required, col_names))]
    # Check required columns.
    assertthat::assert_that(rlang::is_empty(col_missing),
        msg = paste0("Missing column ", csQuote(col_missing), "."))
    # Columns not met will be dropped.
    if (assertthat::not_empty(col_not)) {
        warning("Columns below be dropped:\n", csQuote(col_not), ".", call. = FALSE)
        data <- dplyr::select(data, -dplyr::one_of(col_not))
    }

    if (eplus) {
        output_all <- data[[key]]
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
    }

    return(data)
}
# }}}
# long_to_wide{{{1
long_to_wide <- function(long_table, key = "output", value = "value",
                         eplus = FALSE, group = NULL) {
    long_table <- standardize_long_table(long_table, key = key, value = value,
        eplus = eplus, exclude = group
    )
    wide_table <- tidyr::spread_(long_table, key = key, value = value)
    return(wide_table)
}
# }}}1
# wide_to_xts{{{
wide_to_xts <- function (wide_table, tz = Sys.timezone(),
                         eplus = FALSE, short_name = FALSE) {
    wide_table <- standardize_wide_table(wide_table, eplus = eplus, exclude = NULL)

    col_datetime <- get_date_col(wide_table)
    # Find the name of non numeric columns.
    col_non_num <- names(purrr::discard(
        dplyr::select(wide_table, -dplyr::one_of(col_datetime)),
        is.numeric
    ))
    if (assertthat::not_empty(col_non_num)) {
        warning("Non numeric columns below will be dropped during conversion:\n",
                csQuote(col_non_num), call. = FALSE)
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
# wide_to_long{{{1
wide_to_long <- function(wide_table, key = "output", value = "value",
                         eplus = TRUE, group = NULL) {
    wide_table <- standardize_wide_table(wide_table, eplus = eplus, exclude = group)

    col_datetime <- get_date_col(wide_table)
    if (is.null(group)) {
        long_table <- tidyr::gather(wide_table,
            key = rlang::UQ(key), value = rlang::UQ(value),
            -dplyr::one_of(col_datetime)
        )
    } else {
        long_table <- tidyr::gather(wide_table,
            key = rlang::UQ(key), value = rlang::UQ(value),
            -dplyr::one_of(group, col_datetime)
        )
    }

    return(long_table)
}
# }}}1
# wide_to_full {{{1
wide_to_full <- function (wide_table, group = NULL) {
    wide <- wide_to_long(wide_table, key = "output", value = "value", group = group)
    info <- get_output_info(wide_table)

    full <- dplyr::full_join(wide, info, by = "output")

    return(full)
}
# }}}1
# dygraph_facet {{{
dygraph_facet <- function (data, group = NULL, row = NULL, col = NULL) {
    data_row <- base::split(data, data[[row]])
    data_col_per_row <- purrr::map(data_row, ~base::split(.x, .x[[col]]))
    nms <- purrr::modify_depth(data_col_per_row, 2, names)
    # Handle when missing group
    data_wide_tbl <- purrr::modify_depth(data_col_per_row, 2L,
        ~long_to_wide(.x, key = group)
    )
    data_xts <- purrr::modify_depth(data_wide_tbl, 2L, wide_to_xts)

    # Row-wise
    plots <- purrr::map(seq_along(nms),
        function (row_id) {
            row_name <- names(nms[row_id])
            col_name <- names(nms[[row_id]])
            purrr::map(col_name,
                ~{
                    dygraph_output(data_xts[[row_name]][[.x]],
                        main = row_name, ylab = .x)
                 }
            )
        }
    )

    return(plots)
}
# }}}
# dygraph_output {{{
dygraph_output <- function (data, main = NULL, xlab = NULL, ylab = NULL, legend_width = 400) {
    dygraphs::dygraph(data, group = "dygraphs", main = main, xlab = xlab, ylab = ylab) %>%
    dygraphs::dyRangeSelector() %>%
    dygraphs::dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dygraphs::dyOptions(useDataTimezone = TRUE, axisLineWidth = 1.5) %>%
    dygraphs::dyLegend(width = legend_width, labelsSeparateLines = TRUE) %>%
    # dygraphs::dyLegend(width = 400, show = "follow", labelsSeparateLines = TRUE) %>%
    # dyLegend(width = 400, show = "follow", labelsSeparateLines = TRUE, labelsDiv = "dygraphs_legend") %>%
    dygraphs::dyCrosshair(direction = "vertical") %>%
    dygraphs::dyCSS(system.file("css", "dygraphs.css", package = "eplusr"))
}
# }}}
# create_source_code{{{
create_source_code <- function (data_name, data_name_prefix, group, col_datetime, time_range,
                                cases, outputs, plot_group, plot_color, plot_facet) {
    code_data_check <-
        glue::glue("
            # OUTPUT SELECTION
            ## Check if data meets the requirements.
            {data_name_prefix}_wide_table <- standardize_wide_table({data_name}, exclude = {if_then_else(is.null(group), 'NULL', csQuote(group, and = FALSE))})\n
        ")

    code_data_time_filtered <-
        glue::glue("
            ## Filter data according to selected time range.
            {data_name_prefix}_time_filtered <- dplyr::filter({data_name_prefix}_wide_table,
                                                {col_datetime} >= '{time_range[1]}' &
                                                {col_datetime} <  '{time_range[2]}')\n
        ")

    code_data_group_filtered <-
        glue::glue("
            {data_name_prefix}_group_filtered <- dplyr::filter({data_name_prefix}_time_filtered,
                {group} %in% {glue::glue('c({csQuote(cases, and = FALSE)})')}
            )\n
        ")

    code_name_before <- if_then_else(rlang::is_empty(code_data_group_filtered),
                                    glue::glue('{data_name_prefix}_time_filtered'),
                                    glue::glue('{data_name_prefix}_group_filtered'))
    code_data_selected <-
        glue::glue("
            ## Filter data according to selected outputs.
            {data_name_prefix}_selected <- dplyr::select({code_name_before}, {if_then_else(is.null(group), '', glue::glue({group}, ', '))}{col_datetime},
            {paste0('    `', outputs, '`', collapse = ',\n')}
            )\n\n
        ")

    code_data_ggplot <-
        glue::glue("
            # PLOT SELECTED OUTPUTS USING GGPLOT2
            ## Gather data for use in `ggplot2`.
            ### Get output info of selected data.
            {data_name_prefix}_output_info <- eplusr::get_output_info({data_name_prefix}_selected)
            ### Change selected data into long table format.
            {data_name_prefix}_long_table <- tidyr::gather({data_name_prefix}_selected, key = output, value = value,
                                             {if_then_else(is.null(group), '', glue::glue('-', {group}, ', '))}-{col_datetime}
            )
            ### Join output info data and long table data.
            {data_name_prefix}_full_join <- dplyr::full_join({data_name_prefix}_long_table, {data_name_prefix}_output_info)
            ### Delete rows containing NAs.
            {data_name_prefix} <- tidyr::drop_na({data_name_prefix}_full_join)\n
        ")

    code_ggplot_init <-
        glue::glue("
            ## Initialize ggplot object
            p_{data_name_prefix} <- ggplot({data_name_prefix}, aes(x = {col_datetime}, y = value))\n
        ")

    code_ggplot_line <-
        glue::glue("
            ## Add line plot
            p_{data_name_prefix} <- p_{data_name_prefix} + geom_line(size = 0.5)\n
        ")

    code_ggplot_color <-
        glue::glue("
            ## Add color aes.
            p_{data_name_prefix} <- p_{data_name_prefix} + aes(color = `{plot_color}`)\n
        ")
    code_ggplot_group <-
        glue::glue("
            ## Add group aes.
            p_{data_name_prefix} <- p_{data_name_prefix} + aes(group = `{plot_group}`)\n
        ")
    code_ggplot_facet <-
        glue::glue("
            ## Add facets.
            p_{data_name_prefix} <- p_{data_name_prefix} + facet_grid(facets = {plot_facet}, switch = 'y', scales = 'free')\n
        ")

    code_ggplot_theme <-
        glue::glue("
            ## Set theme
            p_{data_name_prefix} <- p_{data_name_prefix} + theme_bw() +
                theme(legend.position = 'bottom',
                      legend.background = element_rect(color = 'black'),
                      legend.key = element_rect(color = 'gray'),
                      strip.background = element_blank(),
                      strip.placement = 'outside'
                )
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
