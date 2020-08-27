# sql_viewer {{{
dyCSScool <- function (dygraph) {

    dygraph$x$css <- '
    .dygraph-legend {
    width: auto !important;
    min-width: 150px;
    color: white;
    background-color: #BABABA !important;
    padding-left:5px;
    border-color:#BABABA;
    border-style:solid;
    border-width:thin;
    transition:0s 4s;
    z-index: 80 !important;
    box-shadow: 2px 2px 5px rgba(0, 0, 0, .3);
    border-radius: 3px;
    }

    .dygraph-legend:hover{
    transform: translate(-110%);
    transition: 0s;
    }

    .dygraph-legend > span {
    color: black;
    padding-left:5px;
    padding-right:2px;
    margin-left:-5px;
    background-color: white !important;
    display: block;
    }

    .dygraph-legend > span:first-child {
    margin-top:2px;
    }

    .dygraph-legend > span > span{
    display: inline;
    }

    .highlight {
    border-left: 2px solid #BABABA;
    padding-left:3px !important;
    }
    '
    dygraph
}

sql_viewer <- function () {
    # ui {{{
    ui <- shiny::fluidPage(
        shiny::titlePanel("EnergyPlus Result Viewer"),

        shiny::fluidRow(
            shiny::column(2, shiny::actionButton("choose_sql", label = "Pick a SQL file")),
            shiny::column(10, shiny::uiOutput("ui_path_sql"))
        ),

        shiny::tags$br(),

        shiny::h5("Filter data with selectize group", align = "center"),
        shinyWidgets::panel(
            shinyWidgets::selectizeGroupUI(
                id = "sel_dict",
                params = list(
                    name = list(inputId = "name", title = "Variable:"),
                    key_value = list(inputId = "key_value", title = "Key:"),
                    units = list(inputId = "units", title = "Units:"),
                    reporting_frequency = list(inputId = "reporting_frequency", title = "RunPeriod:")
                )
            ),
            status = "primary"
        ),

        shiny::tags$br(),

        shiny::h5("Simulation Output Dictionary", align = "center"),
        shinyWidgets::panel(DT::dataTableOutput("dict"), status = "primary"),
        shiny::actionButton("act_read", label = "Read Data"),

        shiny::tags$br(),

        shiny::h5("Simulation Results", align = "center"),
        shinyWidgets::panel(DT::dataTableOutput("data"), status = "primary"),

        shiny::tags$br(),

        shiny::h5("Plots", align = "center"),
        shinyWidgets::panel(shiny::uiOutput("graphs"), status = "primary")
    )
    # }}}

    # server {{{
    server <- function(input, output, session) {

        path_sql <- shiny::reactiveVal(value = NULL)
        shiny::observeEvent(input$choose_sql, {
            path <- tryCatch(file.choose(), error = function(e) e)

            if (inherits(path, "error")) {
                path_sql(NULL)
            } else {
                path_sql(path)
            }
        })

        path_csv <- shiny::reactive({
            if (is.null(path_sql())) return(NULL)
            csv <- paste0(tools::file_path_sans_ext(path_sql()), ".csv")
            if (file.exists(csv)) csv else NULL
        })

        dict <- shiny::reactive({
            if (is.null(path_sql())) {
                data.table::data.table(
                    name = character(),
                    key_value = character(),
                    units = character(),
                    reporting_frequency = character()
                )
            } else {
                d <- eplusr:::get_sql_report_data_dict(path_sql())
                eplusr:::fast_subset(d,
                    c("name", "key_value", "units", "reporting_frequency", "is_meter")
                )
            }
        })

        dict_sub <- shiny::callModule(
            module = shinyWidgets::selectizeGroupServer,
            id = "sel_dict",
            data = dict,
            vars = c("name", "key_value", "units", "reporting_frequency")
        )

        output$ui_path_sql <- shiny::renderUI({
            if (is.null(path_sql())) shiny::h5("No SQL selected...")
            else shiny::h5(path_sql())
        })

        output$dict <- DT::renderDT(
            eplusr:::fast_subset(dict_sub(), c("name", "key_value", "units", "reporting_frequency", "is_meter")),
            rownames = FALSE,
            colnames = c("Name", "Key", "Units", "Frequency")
        )

        data <- shiny::reactiveVal()
        shiny::observeEvent(input$act_read, {
            if (!is.null(path_sql())) {
                d <- eplusr:::get_sql_report_data(path_sql(), path_csv(),
                    data.table::setDT(dict_sub()),
                    wide = TRUE, all = TRUE, day_type = c("normalday", "holiday")
                )
                data.table::set(d, NULL,
                    c("case", "environment_period_index", "month", "day", "hour", "minute"),
                    NULL
                )
                data.table::setcolorder(d, "datetime")
                data(d)
            }
        })

        output$data <- DT::renderDT(
            {
                if (!NROW(data())) return(NULL)
                data()[, -"datetime"]
            },
            rownames = FALSE,
            caption = "Simulation Data"
        )

        output$graphs <- renderUI({
            if (!NROW(data())) return(NULL)
            vars <- setdiff(names(data()), c("Date/Time", "environment_name", "simulation_days", "day_type"))
            data_sub <- data()[, .SD, .SDcols = c("datetime", vars)]

            # get variable name
            dict <- eplusr:::add_csv_variable(data.table::copy(shiny::isolate(dict_sub())))

            # split by name
            dict_sp <- split(dict, by = "name")

            # create a list of dygraphs
            l <- lapply(dict_sp, function (dict_per) {
                # select variables
                data_per <- data_sub[, .SD, .SDcols = c("datetime", dict_per$Variable)]

                # rename
                data.table::setnames(data_per, c("datetime", dict_per$key_value))

                # create dygraph
                xts <- data.table::as.xts.data.table(data_per)
                dygraphs::dygraph(xts,
                        main = sprintf("%s [%s]", dict_per$name[1L], dict_per$units[1L]),
                        group = "graphs", width = "100%") %>%
                    dygraphs::dyAxis("y", labelWidth = 100) %>%
                    dygraphs::dyLegend(show = "always", hideOnMouseOut = FALSE, width = 600) %>%
                    dygraphs::dyRangeSelector() %>%
                    dygraphs::dyHighlight(
                        highlightSeriesBackgroundAlpha = 0.8,
                        highlightSeriesOpts = list(strokeWidth = 2)) %>%
                    dyCSScool()
            })

            shiny::tagList(l)
        })

        # Stop shiny app when closing the web brower.
        session$onSessionEnded(shiny::stopApp)
    }
    # }}}

    shiny::runGadget(shiny::shinyApp(ui = ui, server = server), viewer = shiny::browserViewer())
}
# }}}
