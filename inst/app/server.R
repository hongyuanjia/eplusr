# Server{{{1
shinyServer(function(input, output, session) {

    # Nav menu actions{{{2
    observeEvent(input$new,
        {
            shinyjs::show("div_page_project")
            shinyjs::hide("div_page_settings")
            shinyjs::hide("div_page_help")
        }
    )

    observeEvent(input$open,
        {
            shinyjs::show("div_page_project")
            shinyjs::hide("div_page_settings")
            shinyjs::hide("div_page_help")
        }
    )

    observeEvent(input$import,
        {
            shinyjs::show("div_page_project")
            shinyjs::hide("div_page_settings")
            shinyjs::hide("div_page_help")
        }
    )

    observeEvent(input$settings,
        {
            shinyjs::hide("div_page_project")
            shinyjs::show("div_page_settings")
            shinyjs::hide("div_page_help")
        }
    )

    observeEvent(input$exit,
        {
            js$closeWindow()
            stopApp()
        }
    )

    observeEvent(input$about,
        {
            shinyjs::hide("div_page_project")
            shinyjs::hide("div_page_settings")
            shinyjs::show("div_page_help")
        }
    )
    # }}}2

    # File{{{2
    ################
    #  file input  #
    ################
    project <- reactiveValues()
    # Save project{{{3
    shinyFileSave(input, "save_project_sel", roots = getVolumes())
    save_project_path <- reactive({
        save_project_path <- parseSavePath(getVolumes(), input$save_project_sel)
        if (is.null(save_project_path)) return(NULL)
        return(save_project_path)
    })
    observe({
        if (!is.null(save_project_path())) {
            updateTextInput(session, "save_project_path", value = save_project_path()$datapath)
        }
    })
    observeEvent(
        input$save_project,
        {
            project_to_save <- list(
                idf_path = isolate(input$model_path),
                weather_path = isolate(input$weather_path),
                param_table = isolate(param_table$table),
                eplus_path = isolate(input$eplus_path),
                wd_path = isolate(input$wd_path),
                parallel_num = isolate(input$parallel_num)
            )
            project_to_save <- jsonlite::toJSON(project_to_save)
            readr::write_lines(project_to_save, isolate(input$save_project_path))
        }
    )
    # project$param_table
    # param_table
    # }}}3

    # Save as project{{{3
    shinyFileSave(input, "save_as_project_sel", roots = getVolumes())
    save_as_project_path <- reactive({
        save_as_project_path <- parseSavePath(getVolumes(), input$save_as_project_sel)
        if (is.null(save_as_project_path)) return(NULL)
        return(save_as_project_path)
    })
    observe({
        if (!is.null(save_as_project_path())) {
            updateTextInput(session, "save_as_project_path", value = save_as_project_path()$datapath)
        }
    })
    observeEvent(
        input$save_as_project,
        {
            project_to_save <- list(
                epat_ver = "0.0.0.9",
                idf_path = isolate(input$model_path),
                weather_path = isolate(input$weather_path),
                param_table = isolate(param_table$table),
                eplus_path = isolate(input$eplus_path),
                wd_path = isolate(input$wd_path),
                parallel_num = isolate(input$parallel_num)
            )
            project_to_save <- jsonlite::toJSON(project_to_save)
            readr::write_lines(project_to_save, isolate(input$save_as_project_path))
        }
    )
    # project$param_table
    # param_table
    # }}}3

    # shinyFileChoose for model and weather{{{3
    shinyFileChoose(input, 'model_sel', roots = getVolumes(), filetypes = c("idf", "IDF", "imf", "IMF"))
    shinyFileChoose(input, 'weather_sel', roots = getVolumes(), filetypes = c("epw", "EPW"))
    # }}}3

    # Get reactive values of model and weather{{{3
    model_path <- reactive({
        model_path <- parseFilePaths(getVolumes(), input$model_sel)
        if (is.null(model_path)) return(NULL)
        return(model_path)
    })
    weather_path <- reactive({
        weather_path <- parseFilePaths(getVolumes(), input$weather_sel)
        if (is.null(weather_path)) return(NULL)
        return(weather_path)
    })
    # }}}3

    # Update model and weather path according to shinyFileChoose{{{3
    observe({
        if (!is.null(model_path())) {
            updateTextInput(session, "model_path", value = model_path()$datapath)
        }
    })
    observe({
        if (!is.null(weather_path())) {
            updateTextInput(session, "weather_path", value = weather_path()$datapath)
        }
    })
    # }}}3

    #####################
    #  parameter input  #
    #####################
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
            `Fixed Value` = ifelse(is.null(input$fixed_value), NA_character_, input$fixed_value),
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
                    "Please input R expressions."
                } else {
                    eval_results
                }
            })

            # Update fix value selection
            eval_error <- "Invalid R expression!"
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
        table = dplyr::tibble(
            ID = NA_character_,
            Name = NA_character_,
            `Search Tag` = NA_character_,
            `Value Expressions` = NA_character_,
            `Fixed Value` = 0L,
            Description = NA_character_
        )
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
                    param_table$table[s,] <- isolate(project$params())
                    param_table$table <- tidyr::drop_na(param_table$table, ID, Name, `Search Tag`, `Value Expressions`)
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

    # Import jEPlus json{{{3
    shinyFileChoose(input, 'jeplus_sel', roots = getVolumes(), filetypes = c("json", "JSON"))
    jeplus_sel <- reactive({
        jeplus_sel <- parseFilePaths(getVolumes(), input$jeplus_sel)
        if (is.null(jeplus_sel)) return(NULL)
        return(jeplus_sel)
    })
    observe({
        if (!is.null(jeplus_sel())) {
            updateTextInput(session, "jeplus_path", value = jeplus_sel()$datapath)
        }
    })
    observeEvent(
        input$import_jeplus_project,
        {
            shinyBS::toggleModal(session, "modal_jeplus_import", toggle = "close")
            project_info <- import_jeplus_project(req(input$jeplus_path))
            param_table$table <- dplyr::as_tibble(project_info$params)
            updateTextInput(session, "model_path", value = project_info$idf_path)
            updateTextInput(session, "weather_path", value = project_info$weather_path)
            updateSelectInput(session, "parallel_num", selected = project_info$num_threads)

            shinyjs::show("div_param_table")
            output$param_table <- DT::renderDataTable(
                {
                    param_table$table
                }, selection = "single", option = list(dom = "t")
            )

        }
    )
    # }}}3
    # }}}2

    # Edit{{{2
    shinyDirChoose(input, "eplus_sel", roots = getVolumes())
    shinyDirChoose(input, "wd_sel", roots = getVolumes())
    # Get reactive values of EnergyPlus location and working directory{{{3
    eplus_sel <- reactive({
        eplus_sel <- parseDirPath(getVolumes(), input$eplus_sel)
        if (is.null(eplus_sel)) return(NULL)
        return(eplus_sel)
    })
    wd_sel <- reactive({
        wd_sel <- parseDirPath(getVolumes(), input$wd_sel)
        if (is.null(wd_sel)) return(NULL)
        return(wd_sel)
    })
    # }}}3

    # Update model and weather path according to shinyFileChoose{{{3
    observe({
        if (!is.null(eplus_sel())) {
            updateTextInput(session, "eplus_path", value = eplus_sel())
        }
    })
    observe({
        if (!is.null(wd_sel())) {
            updateTextInput(session, "wd_path", value = wd_sel())
        }
    })
    # }}}3
    # }}}2

    # Stop shiny app when closing the web brower.
    session$onSessionEnded(stopApp)
})
# }}}1
