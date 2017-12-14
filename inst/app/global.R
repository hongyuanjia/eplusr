library(shiny)
library(DT)
library(shinyjs)
library(shinyFiles)
library(shinyBS)
library(shinycssloaders)
library(shinythemes)
library(shinyLP)
library(shinydashboard)

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# import_jeplus_project{{{1
import_jeplus_project <- function (json) {
    # Read jeplus JSON project file.
    info <- jsonlite::fromJSON(json)

    project_name <- as.character(info[["projectID"]])
    num_threads <- as.integer(info[["execSettings"]][["numThreads"]])
    params <- info[["parameters"]]
    params <- dplyr::select(params, ID = id, Name = name, `Search Tag` = searchString, Description = description,
                            `Fixed Value` = selectedAltValue, `Value Expressions` = valuesString)
    params <- params[, c("ID", "Name", "Search Tag", "Value Expressions", "Fixed Value", "Description")]

    # Get input file info.
    idfs <- purrr::flatten_chr(stringr::str_split(info[["idftemplate"]], "\\s*;\\s*"))
    wthrs <- purrr::flatten_chr(stringr::str_split(info[["weatherFile"]], "\\s*;\\s*"))
    idf_path <- paste0(info[["idfdir"]], idfs)
    wthr_path <- paste0(info[["weatherDir"]], wthrs)

    project_info <- list(project_name = project_name, num_threads = num_threads,
                         idf_path = idf_path, weather_path = wthr_path, params = params)
    return(project_info)
}
# }}}1
# preview_values{{{1
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
# }}}1
# bs3_dropdown{{{1
#' Custom dropdown menu function for \code{\link[editR:editR-package]{editR}}
#'
#' This function creates custom Twitter Bootstrap 3 dropdown menus for \code{\link[editR:editR-package]{editR}}.
#'
#' @param name The name of the dropdown menu.
#'
#' @param navs A list of UI elements to include in the dropdown menu (e.g. \code{\link[shiny:actionLink]{actionLinks}}).
#'
#' @return This function return the html code for a Twitter Bootstrap 3 dropdown menu.
#'
#' @author Simon Garnier: \email{garnier@@njit.edu}, \link[https://twitter.com/sjmgarnier]{@@sjmgarnier}
#'
bs3_dropdown <- function(name = "My menu", navs) {
  tags$li(class = "dropdown",
          tags$a(name, class = "dropdown-toggle", "data-toggle" = "dropdown",
                 tags$span(class = "caret")
          ),
          tags$ul(class = "dropdown-menu",
                  lapply(navs, function(x) {tags$li(x)})
          )
  )
}
# }}}1
# bs3_navbar{{{1
#' Custom navbar function for \code{\link[editR:editR-package]{editR}}
#'
#' This function creates custom Twitter Bootstrap 3 navbars for \code{\link[editR:editR-package]{editR}}.
#'
#' @param name The name of the navbar.
#'
#' @param navs A list of UI elements to include in the navbar (e.g. \code{\link[shiny:actionLink]{actionLinks}}).
#'
#' @return This function return the html code for a Twitter Bootstrap 3 navbar.
#'
#' @author Simon Garnier: \email{garnier@@njit.edu}, \link[https://twitter.com/sjmgarnier]{@@sjmgarnier}
#'
bs3_navbar <- function(brand = "My website", navs) {
  tags$nav(class = "navbar navbar-default",
           tags$div(class = "container-fluid",
                    tags$div(class = "navbar-header",
                             tags$a(brand, class = "navbar-brand",  href = "#")
                    ),
                    tags$div(
                      tags$ul(class = "nav navbar-nav",
                              lapply(navs, function(x) {tags$li(x)})
                      )
                    )
           )
  )
}
# }}}1
