# convertMenuItem {{{
convertMenuItem <- function (mi, tabName) {
    mi$children[[1]]$attribs['data-toggle'] = "tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    mi
}
# }}}

# eplusrModal {{{
eplusrModal <- function(id, title, trigger, ..., size, footer = NULL, close.button = TRUE, width = NULL) {
    if (!missing(size)) {
        if (size == "large") {
            size = "modal-lg"
        } else if(size == "small") {
            size = "modal-sm"
        }
        size <- paste("modal-dialog", size)
        width = NULL
    } else {
        size <- "modal-dialog"
    }

    if (is.null(footer)) {
        footer <- shiny::tagList()
    }

    if (close.button) {
        footer <- shiny::tagAppendChild(footer,
            shiny::tagList(shiny::tags$button(type = "button",
                class = "btn btn-default", "data-dismiss" = "modal", "Close")
            )
        )
    }

    bsTag <- shiny::tags$div(class = size,
        shiny::tags$div(class = "modal-content",
            shiny::tags$div(class = "modal-header",
                shiny::tags$button(type = "button", class = "close", "data-dismiss" = "modal",
                    shiny::tags$span(shiny::HTML("&times;"))
                 ),
                shiny::tags$h4(class = "modal-title", title)
            ),
            shiny::tags$div(class = "modal-body", list(...)),
            shiny::tags$div(class = "modal-footer", footer)
        )
    )

    if(!is.null(width)) {
        bsTag <- addAttribs(bsTag, style = paste0("width: ", width, " !important;"))
    }

    bsTag <- shiny::tags$div(class = "modal sbs-modal fade", id = id,
                             tabindex = "-1", "data-sbs-trigger" = trigger, bsTag)
}
# }}}

# TODO:
# 1. Use `formattable` to show eplus `*.end` files. (Use reactivePoll)
# 2. Use `collapsibleTree` to show the structure of the project.
# 3. Use `diagrammaR` to show the structure of eplus models
# 4. Use `rpivotTable` to show the simulation results of all cases.
# 5. Use `shinyFeedback` to check inputs.
# 6. Find some way to use `rhandsontable`.

# dashboard {{{
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(collapsibleTree)

# help(package = "shinyWidgets")
ui <- dashboardPage(
    # skin = "purple",

    # Header {{{
    dashboardHeader(title = "EPAT",
        dropdownMenu(type = "messages",
            messageItem(
                from = "Sales Dept",
                message = "Sales are steady this month."
            )
        ),
        dropdownMenu(type = "notifications",
            notificationItem(
                text = "5 new users today", icon("users")
            )
        )
    ),
    # }}}

    # Sidebar {{{
    dashboardSidebar(
        # sidebarUserPanel("Hongyuan", subtitle = NULL, image = icon("headphones")),
        sidebarMenu(
            # project {{{
            convertMenuItem(
                menuItem("Project", tabName = "tab_project", icon = icon("file"),
                    tags$li(
                        actionLink("lnk_proj_new", " New Project", icon = icon("file")),
                        actionLink("lnk_proj_open", " Open Project...", icon = icon("folder-open")),
                        actionLink("lnk_proj_save", " Save", icon = icon("floppy-o")),
                        actionLink("lnk_proj_saveas", " Save As...", icon = icon("floppy-o")),
                        actionLink("lnk_proj_jeplus", " Import jEPlus JSON Project...", icon = icon("tasks"))
                    )
                ),
                tabName = "tab_project"
            ),
            # }}}
            # edit {{{
            convertMenuItem(
                menuItem("Edit", tabName = "tab_edit", icon = icon("edit"),
                    menuItem("Import Parameters from CSV...", tabName = "tab_edit_imp_csv", icon = icon("sign-in")),
                    menuItem("Export Parameters to CSV...", tabName = "tab_edit_exp_csv", icon = icon("sign-out")),
                    menuItem("Settings...", tabName = "tab_edit_settings", icon = icon("gears"))
                ),
                tabName = "tab_edit"
            ),
            # }}}
            # action {{{
            menuItem("Action", tabName = "tab_action", icon = icon("play"),
                menuItem(tabName = "tab_action_validate", "Validate Jobs", icon = icon("check-square")),
                menuItem(tabName = "tab_action_simulate", "Run Jobs", icon = icon("play")),
                menuItem(tabName = "tab_action_summary", "Show Result Summary", icon = icon("list")),
                menuItem(tabName = "tab_action_plot", "Plot...", icon = icon("bar-chart"))
            ),
            # }}}
            # tools {{{
            menuItem("Tools", tabName = "tab_tools", icon = icon("wrench"),
                menuItem("IDF Version Converter", tabName = "tab_tools_converter", icon = icon("retweet")),
                menuItem("IMF to IDF Converter", tabName = "tab_tools_imftoidf", icon = icon("mail-forward"))
            ),
            # }}}
            # help {{{
            menuItem("Help", tabName = "tab_help", icon = icon("question-circle"),
                menuItem("User Guide", tabName = "guide", icon = icon("question-circle")),
                menuItem("About", tabName = "about", icon = icon("exclamation-circle"))
            )
            # }}}
        )
    ),
    # }}}

    # Body {{{
    dashboardBody(
        # # mod_proj_new {{{
        # eplusrModal(id = "mod_proj_new", title = "New Project", close.button = FALSE,
            # trigger = "lnk_proj_new", size = "large",
            # textInput("lnk_proj_new_name", label = "Name of the Project:",
                # value = "", placeholder = "Please insert a name"
            # ),
            # footer = tagList(
                # shinyWidgets::actionBttn("mod_proj_new_btn_start", "Start",
                    # size = "sm", style = "fill", color = "primary"),
                # tagAppendAttributes(
                    # shinyWidgets::actionBttn("mod_proj_new_btn_cancel", "Cancel",
                        # size = "sm", style = "fill"),
                    # "data-dismiss" = "modal")
            # )
        # ),
        # # }}}
        # # mod_proj_open {{{
        # eplusrModal(id = "mod_proj_open", title = "Open Project", close.button = FALSE,
            # trigger = "lnk_proj_open", size = "large",
            # textInput("lnk_proj_open_name", label = "Name of the Project:",
                # value = "", placeholder = "Please insert a name"
            # ),
            # footer = tagList(
                # shinyWidgets::actionBttn("mod_proj_open_btn_start", "Start",
                    # size = "sm", style = "fill", color = "primary"),
                # tagAppendAttributes(
                    # shinyWidgets::actionBttn("mod_proj_open_btn_cancel", "Cancel",
                        # size = "sm", style = "fill"),
                    # "data-dismiss" = "modal")
            # )
        # ),
        # # }}}
        # # mod_proj_save {{{
        # eplusrModal(id = "mod_proj_save", title = "Save Project", close.button = FALSE,
            # trigger = "lnk_proj_save", size = "large",
            # textInput("lnk_proj_save_name", label = "Name of the Project:",
                # value = "", placeholder = "Please insert a name"
            # ),
            # footer = tagList(
                # shinyWidgets::actionBttn("mod_proj_save_btn_start", "Start",
                    # size = "sm", style = "fill", color = "primary"),
                # tagAppendAttributes(
                    # shinyWidgets::actionBttn("mod_proj_save_btn_cancel", "Cancel",
                        # size = "sm", style = "fill"),
                    # "data-dismiss" = "modal")
            # )
        # ),
        # # }}}
        # # mod_proj_saveas {{{
        # eplusrModal(id = "mod_proj_saveas", title = "Save Project", close.button = FALSE,
            # trigger = "lnk_proj_saveas", size = "large",
            # textInput("lnk_proj_saveas_name", label = "Name of the Project:",
                # value = "", placeholder = "Please insert a name"
            # ),
            # footer = tagList(
                # shinyWidgets::actionBttn("mod_proj_saveas_btn_start", "Start",
                    # size = "sm", style = "fill", color = "primary"),
                # tagAppendAttributes(
                    # shinyWidgets::actionBttn("mod_proj_saveas_btn_cancel", "Cancel",
                        # size = "sm", style = "fill"),
                    # "data-dismiss" = "modal")
            # )
        # ),
        # # }}}
        # # mod_proj_jeplus {{{
        # eplusrModal(id = "mod_proj_jeplus", title = "Import jEPlus JSON Project", close.button = FALSE,
            # trigger = "lnk_proj_jeplus", size = "large",
            # textInput("lnk_proj_jeplus_name", label = "Name of the Project:",
                # value = "", placeholder = "Please insert a name"
            # ),
            # footer = tagList(
                # shinyWidgets::actionBttn("mod_proj_jeplus_btn_start", "Start",
                    # size = "sm", style = "fill", color = "primary"),
                # tagAppendAttributes(
                    # shinyWidgets::actionBttn("mod_proj_jeplus_btn_cancel", "Cancel",
                        # size = "sm", style = "fill"),
                    # "data-dismiss" = "modal")
            # )
        # ),
        # # }}}
        tabItems(
            # tab_project {{{
            tabItem(tabName = "tab_project",
                # mod_proj_new {{{
                eplusrModal(id = "mod_proj_new", title = "New Project", close.button = FALSE,
                    trigger = "lnk_proj_new", size = "large",
                    textInput("lnk_proj_new_name", label = "Name of the Project:",
                        value = "", placeholder = "Please insert a name"
                    ),
                    footer = tagList(
                        shinyWidgets::actionBttn("mod_proj_new_btn_start", "Start",
                            size = "sm", style = "fill", color = "primary"),
                        tagAppendAttributes(
                            shinyWidgets::actionBttn("mod_proj_new_btn_cancel", "Cancel",
                                size = "sm", style = "fill"),
                            "data-dismiss" = "modal")
                    )
                ),
                # }}}
        # mod_proj_open {{{
        eplusrModal(id = "mod_proj_open", title = "Open Project", close.button = FALSE,
            trigger = "lnk_proj_open", size = "large",
            textInput("lnk_proj_open_name", label = "Name of the Project:",
                value = "", placeholder = "Please insert a name"
            ),
            footer = tagList(
                shinyWidgets::actionBttn("mod_proj_open_btn_start", "Start",
                    size = "sm", style = "fill", color = "primary"),
                tagAppendAttributes(
                    shinyWidgets::actionBttn("mod_proj_open_btn_cancel", "Cancel",
                        size = "sm", style = "fill"),
                    "data-dismiss" = "modal")
            )
        ),
        # }}}
        # mod_proj_save {{{
        eplusrModal(id = "mod_proj_save", title = "Save Project", close.button = FALSE,
            trigger = "lnk_proj_save", size = "large",
            textInput("lnk_proj_save_name", label = "Name of the Project:",
                value = "", placeholder = "Please insert a name"
            ),
            footer = tagList(
                shinyWidgets::actionBttn("mod_proj_save_btn_start", "Start",
                    size = "sm", style = "fill", color = "primary"),
                tagAppendAttributes(
                    shinyWidgets::actionBttn("mod_proj_save_btn_cancel", "Cancel",
                        size = "sm", style = "fill"),
                    "data-dismiss" = "modal")
            )
        ),
        # }}}
        # mod_proj_saveas {{{
        eplusrModal(id = "mod_proj_saveas", title = "Save Project", close.button = FALSE,
            trigger = "lnk_proj_saveas", size = "large",
            textInput("lnk_proj_saveas_name", label = "Name of the Project:",
                value = "", placeholder = "Please insert a name"
            ),
            footer = tagList(
                shinyWidgets::actionBttn("mod_proj_saveas_btn_start", "Start",
                    size = "sm", style = "fill", color = "primary"),
                tagAppendAttributes(
                    shinyWidgets::actionBttn("mod_proj_saveas_btn_cancel", "Cancel",
                        size = "sm", style = "fill"),
                    "data-dismiss" = "modal")
            )
        ),
        # }}}
        # mod_proj_jeplus {{{
        eplusrModal(id = "mod_proj_jeplus", title = "Import jEPlus JSON Project", close.button = FALSE,
            trigger = "lnk_proj_jeplus", size = "large",
            textInput("lnk_proj_jeplus_name", label = "Name of the Project:",
                value = "", placeholder = "Please insert a name"
            ),
            footer = tagList(
                shinyWidgets::actionBttn("mod_proj_jeplus_btn_start", "Start",
                    size = "sm", style = "fill", color = "primary"),
                tagAppendAttributes(
                    shinyWidgets::actionBttn("mod_proj_jeplus_btn_cancel", "Cancel",
                        size = "sm", style = "fill"),
                    "data-dismiss" = "modal")
            )
        ),
        # }}}
                fluidRow(
                    valueBox(value = "Project:", subtitle = "project", icon = icon("briefcase")),
                    valueBox(value = "Model Template:", subtitle = "Template", icon = icon("laptop")),
                    valueBox(value = "Weather File:", subtitle = "Weather", icon = icon("sun-o"))
                ),
                fluidRow(
                    shinycssloaders::withSpinner(tableOutput("param_table"), type = 6)
                ),
                fluidRow(
                    # Show project summary
                )
            ),
            # }}}
            # tab_edit {{{
            tabItem(tabName = "tab_edit",
                fluidRow(
                    shinycssloaders::withSpinner(DT::dataTableOutput("tab_edit_param_table"), type = 6)
                ),
                shinyBS::bsCollapsePanel("File Input", style = "primary",
                    fluidRow(
                        column(6, textInput("id", "Field ID*:", value = NULL, width = "100%")),
                        column(6, textInput("name", "Field Name*:", value = NULL, width = "100%"))
                    ),
                    fluidRow(
                        column(12,
                            textInput("tag", "Search tag*:", value = NULL, width = "100%")
                        )
                    ),
                    fluidRow(
                        column(12,
                            textInput("desc", "Description:", value = NULL, width = "100%")
                        )
                    ),
                    fluidRow(
                        column(8,
                            textInput("values", "Value Expressions*:", value = NULL, width = "100%")
                        ),
                        column(4,
                            actionButton("preview", label = "Preview values", icon = icon("eye"))
                        )
                    ),
                    fluidRow(
                        column(12,
                            shinyjs::hidden(
                                div(id = "preview_values", class = "frame", style = "border-style: solid;",
                                    tagAppendAttributes(
                                        textOutput("preview_values"), style = "white-space:pre-wrap;"
                                    )
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(12,
                            selectInput("fixed_value", "Fix on i-th value:", choices = 0L)
                        )
                    ),
                    helpText("Note: Fields marked with '*' are requred.")
                )
            )
            # }}}
        )
    )
    # }}}
)

server <- function (input, output, session) {

    # Stop shiny app when closing the web brower.
    session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)
# }}}

# UI {{{1
shinyUI(tagList(
        # Change width of shiny notifications{{{2
        tags$head(
             tags$style(
                 HTML(".shiny-notification {
                      position: absolute;
                      bottom: 8px;
                      right: 16px;
                      width: 400px;
                      }
                      "
                 )
             )
        ),
        # }}}2
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        bootstrapPage(
            title = "EPAT - EnergyPlus Parametric Analysis Toolkit",
            theme = shinythemes::shinytheme("flatly"),
            # navbar {{{2
            bs3_navbar("EPAT",
                       navs = list(
                            # File {{{3
                            bs3_dropdown(name = "File", #icon = icon("file"),
                                         navs = list(
                                             actionLink("new", "New Project", icon = icon("file")),
                                             actionLink("open", "Open Project...", icon = icon("folder-open")),
                                             tags$hr(class = "sep"),
                                             actionLink("save", "Save", icon = icon("floppy-o")),
                                             actionLink("saveas", "Save As...", icon = icon("floppy-o")),
                                             tags$hr(class = "sep"),
                                             actionLink("import_jeplus", "Import jEplus JSON Project...", icon = icon("tasks")),
                                             tags$hr(class = "sep"),
                                             actionLink("exit", "Exit", icon = icon("window-close-o"))

                                         )
                            ),
                            # }}}3

                            # Edit{{{3
                            bs3_dropdown(name = "Edit", #icon = icon("edit"),
                                         navs = list(
                                             actionLink("import_csv", "Import Parameters from CSV...", icon = icon("sign-in")),
                                             actionLink("export_csv", "Export Parameters to CSV...", icon = icon("sign-out")),
                                             tags$hr(class = "sep"),
                                             actionLink("settings", "Settings...", icon = icon("gears"))

                                         )
                            ),
                            # }}}3

                            # Action{{{3
                            bs3_dropdown(name = "Action", #icon = icon("play"),
                                         navs = list(
                                             actionLink("validate", "Validate Jobs", icon = icon("check-square")),
                                             actionLink("simulate", "Run Jobs", icon = icon("play")),
                                             tags$hr(class = "sep"),
                                             actionLink("summary", "Show Result Summary", icon = icon("list")),
                                             actionLink("plot", "Plot...", icon = icon("bar-chart"))
                                         )
                            ),
                            # }}}3

                            # Tools{{{3
                            bs3_dropdown(name = "Tools", #icon = icon("wrench"),
                                         navs = list(
                                             actionLink("converter", "IDF Version Converter", icon = icon("retweet")),
                                             actionLink("imftoidf", "IMF to IDF Converter", icon = icon("mail-forward"))
                                         )
                            ),
                            # }}}3

                            # Help{{{3
                            bs3_dropdown(name = "Help", #icon = icon("question-circle"),
                                         navs = list(
                                             actionLink("guide", "User Guide", icon = icon("question-circle")),
                                             actionLink("about", "About", icon = icon("exclamation-circle"))
                                         )
                            )
                            # }}}3
                      )
            ),
            # }}}2

            # page_project{{{2
            div(id = "div_page_project",
                sidebarLayout(
                    sidebarPanel(width = 5,
                        shinyBS::bsCollapse(id = "coll_page_project", multiple = TRUE,
                                            open = c("File Input","Parameter Input"),
                            shinyBS::bsCollapsePanel("File Input", style = "primary",
                                # File Input
                                # {{{3
                                div(class = "container-fluid",
                                    div(class = "row", style = "no-gutters",
                                        div(class = "col-sm-9", style="display:inline-block", textInput("model_path", label = "IDF/IMF model template:", placeholder = "No template selected.", width = "100%")),
                                        div(style="display:inline-block", shinyFilesButton("model_sel", label = "...", title = "Please select an IDF/IMF model template", multiple = FALSE)),
                                        div(style="display:inline-block", actionButton("edit_model", label = "Edit", icon = icon("pencil-square"))),
                                        tags$style(type='text/css', "#model_sel {vertical-align: top; margin-top: 25px;}"),
                                        tags$style(type='text/css', "#edit_model {vertical-align: top; margin-top: 25px;}")
                                    )
                                ),
                                tags$br(),
                                div(class = "container-fluid",
                                    div(class = "row", style = "no-gutters",
                                        div(class = "col-sm-9", style="display:inline-block", textInput("weather_path", label = "Weather file:", placeholder = "No weather selected.", width = "100%")),
                                        div(style="display:inline-block", shinyFilesButton("weather_sel", label = "..", title = "Please select a weather file:", multiple = TRUE)),
                                        div(style="display:inline-block", actionButton("edit_weather", label = "Edit", icon = icon("pencil-square"))),
                                        tags$style(type='text/css', "#weather_sel {vertical-align: top; margin-top: 25px;}"),
                                        tags$style(type='text/css', "#edit_weather {vertical-align: top; margin-top: 25px;}")
                                    )
                                )
                                # }}}3
                            ),
                            shinyBS::bsCollapsePanel("Parameter Input", style = "primary",
                                # Parameter Input
                                # {{{3
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
                                column(12,
                                    div(class = "btn-group pull-right", role = "group",
                                        actionButton("add_param", label = "Add", icon = icon("plus", lib = "glyphicon")),
                                        actionButton("save_param", label = "Save", icon = icon("ok", lib = "glyphicon")),
                                        actionButton("copy_param", label = "Copy", icon = icon("copy", lib = "glyphicon")),
                                        actionButton("delete_param", label = "Delete", icon = icon("remove", lib = "glyphicon"))
                                    )
                                ),
                                tags$br(),
                                helpText("Note: Fields marked with '*' are requred.")
                                # }}}3
                            )
                        )
                    ),
                    mainPanel(width = 7,
                        shinyBS::bsCollapse(id = "coll_main_output", multiple = TRUE,
                                            open = c("Parameter Table"),
                            shinyBS::bsCollapsePanel("Parameter Table", style = "info",
                                shinycssloaders::withSpinner(DT::dataTableOutput("param_table"), type = 6)
                            )
                        )
                    )
                )
            ),
            # }}}2

        # modal_save_project{{{2
        shinyBS::bsModal("modal_save_project", title = "Save Project",
            trigger = "save", size = "large",
            div(class = "container-fluid",
                div(class = "row",
                    div(class = "col-sm-10",
                        textInput("save_project_path", label = "Save project to:", value = file.path(getwd(), "project.json"), width = "100%")
                    ),
                    div(style = "display:inline-block",
                        shinySaveButton("save_project_sel", label = "...", title = "Please input a location to save an EPAT project file:", filetype = list(JSON = "json"))
                    ),
                    div(style = "display:inline-block",
                        actionButton("save_project", "Save")
                    ),
                    tags$style(type='text/css', "#save_project_sel {vertical-align: top; margin-top: 25px;}"),
                    tags$style(type='text/css', "#save_project {vertical-align: top; margin-top: 25px;}")
                )
            )
        ),
        # }}}2

        # modal_save_as_project{{{2
        shinyBS::bsModal("modal_save_as_project", title = "Save Project As...",
            trigger = "saveas", size = "large",
            div(class = "container-fluid",
                div(class = "row",
                    div(class = "col-sm-10",
                        textInput("save_as_project_path", label = "Save project to:", value = file.path(getwd(), "project.json"), width = "100%")
                    ),
                    div(style = "display:inline-block",
                        shinySaveButton("save_as_project_sel", label = "...", title = "Please input a location to save an EPAT project file:", filetype = list(JSON = "json"))
                    ),
                    div(style = "display:inline-block",
                        actionButton("save_as_project", "Save")
                    ),
                    tags$style(type='text/css', "#save_as_project_sel {vertical-align: top; margin-top: 25px;}"),
                    tags$style(type='text/css', "#save_as_project {vertical-align: top; margin-top: 25px;}")
                )
            )
        ),
        # }}}2

        # modal_jeplus_import{{{2
        shinyBS::bsModal("modal_jeplus_import", title = "Import jEplus JSON Project",
            trigger = "import_jeplus", size = "large",
            div(class = "container-fluid",
                div(class = "row",
                    div(class = "col-sm-10",
                        textInput("jeplus_path", label = "jEplus JSON project file:", placeholder = "No json file selected.", width = "100%")
                    ),
                    div(style = "display:inline-block",
                        shinyFilesButton("jeplus_sel", label = "...", title = "Please select a jEPlus JSON project file:", multiple = FALSE)
                    ),
                    div(style = "display:inline-block",
                        actionButton("import_jeplus_project", "Import")
                    ),
                    tags$style(type='text/css', "#jeplus_sel {vertical-align: top; margin-top: 25px;}"),
                    tags$style(type='text/css', "#import_jeplus_project {vertical-align: top; margin-top: 25px;}")
                )
            )
        ),
        # }}}2

        # page_plotting{{{2
        shinyjs::hidden(
            div(id = "div_page_plotting"
            )
        ),
        # }}}2

        # page_settings{{{2
        shinyjs::hidden(
            div(id = "div_page_settings", class = "well",
                shinyBS::bsCollapse(id = "coll_page_settings", multiple = TRUE, open = c("Executables", "Options"),
                    shinyBS::bsCollapsePanel("Executables", style = "primary",
                        div(class = "container-fluid",
                            div(class = "row", style = "no-gutters",
                                div(class = "col-sm-10", style = "display:inline-block", textInput("eplus_path", label = "EnergyPlus location:", value = "", placeholder = "No EnergyPlus location selected.", width = "100%")),
                                div(style = "display:inline-block", shinyDirButton("eplus_sel", label = "...", title = "Please select an EnergyPlus location:")),
                                tags$style(type='text/css', "#eplus_sel {vertical-align: top; margin-top: 25px;}")
                            )
                        )
                    ),
                    shinyBS::bsCollapsePanel("Options", style = "primary",
                        div(class = "container-fluid",
                            div(class = "row", style = "no-gutters",
                                div(class = "col-sm-1", style = "display:inline-block",
                                    selectInput("parallel_num", label = "Parallel job:", choices = c(1:8), selected = 4)
                                )
                            )
                        ),

                        div(class = "container-fluid",
                            div(class = "row", style = "no-gutters",
                                div(class = "col-sm-10", style = "display:inline-block", textInput("wd_path", label = "Working dir:", value = getwd(), width = "100%")),
                                div(style = "display:inline-block", shinyDirButton("wd_sel", label = "...", title = "Working dir:")),
                                tags$style(type='text/css', "#wd_sel {vertical-align: top; margin-top: 25px;}")
                            )
                        ),
                        div(class = "container-fluid",
                            div(class = "row", style = "no-gutters",
                                div(class = "col-sm-10", style = "display:inline-block",
                                    checkboxGroupInput("clean", label = "Clean Up:", choices = c("Keep all EnergyPlus files, e.g. eplusout.eso" = "keep_all", "Delete selected files:" = "delete"))
                                )
                            )
                        )
                    )
                ),
                div(class = "container-fluid",
                    div(class = "row",
                        div(class = "col-sm-1 pull-right",
                            actionButton("cannel_settings", "Cannel")
                        ),
                        div(class = "col-sm-1 pull-right",
                            actionButton("save_settings", "Save")
                        )
                    )
                )
            )
        ),
        # }}}2

        # page_help{{{2
        shinyjs::hidden(
            div(id = "div_page_help", class = "jumbotron",
                h1("EPAT"),
                p("EnergyPlus Parametric Analysis Toolkit powered by R and Shiny")
            )
        )
        # }}}2

    )
)
)
# }}}1
