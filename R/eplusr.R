################################################################################
#                             Deprecated functions                             #
################################################################################

# # caseAlign: A function to make time interval of two datasets be the same
# # {{{1
# caseAlign <- function(x, y, date_col = NULL, col_based = "Case", bind = FALSE){
#     x_case_names <- x[[col_based]]
#     y_case_names <- y[[col_based]]

#     x_date_col_name <- check_date_col(x)
#     y_date_col_name <- check_date_col(y)
#     x_time_interval <- get_interval(x[[x_date_col_name]])
#     y_time_interval <- get_interval(y[[y_date_col_name]])

#     if(x_time_interval == y_time_interval) {
#         message("x and y has the same time interval. No change has been made.")
#         return(x)
#     }

#     walk2(x_case_names, x_time_interval,
#           function(x_case, x_interval){
#               if(x_interval > y_time_interval){
#                   stop(paste0("Data input `y` has smaller time interval than case '", x_case,
#                               "' in data input `x`. Data in `x` will not be aggregated."))
#               }else if(x_interval < y_time_interval){
#                   warning(paste0("Data input `x` for case '", x_case,
#                                  "' has smaller time interval than data input `x`.",
#                                  " Data in `y` will be aggregated according to the interval of `x`."))
#               }
#                                  })

#   x_agg_by_case <-
#     map2(x_case_names, x_time_interval, function(x_case, x_interval, y_interval){
#       x_per_case <- x %>% .[Case == x_case]
#       if(x_interval < y_time_interval){
#         x_per_case %<>%
#           agg_by(interval = as.integer(y_interval), by_col = "Case") %>%
#           .[-.N]
#       }
#       return(x_per_case)
#     }, y_interval = y_time_interval) %>% rbindlist()

#   results <- x_agg_by_case

#   if(bind){
#     results <-
#       rbind(, x_agg_by_case)
#   }

#   return(results)
# }
# # }}}1

# # statCal: A function to make one statistial calculation between two data.table
# # {{{1
# statCal <- function(sim, obs, col_pattern, fun, melt = FALSE, by_pattern = NULL, case_col = "Case", case.align = FALSE){

#     # if(case.align) {sim <- caseAlign(sim = sim, obs = obs, bind = FALSE)}

#     if(!(hasArg(col_pattern))){
#         col_pattern = "^(?!DateTime|Case)"
#     }

#     if(!(hasArg(fun))){
#         stop("Argument 'fun' is missing!")
#     }

#     sim_case_names <- sim[, unique(get(case_col))]
#     obs_case_names <- obs[, unique(get(case_col))]

#     if(length(obs_case_names) > 1){
#         warning("Only the first unique values in 'case_col' was used!")
#         obs <- obs[eval(parse(text = case_col)) == obs_case_names[1]]
#     }

#   results <-
#     map(sim_case_names, ~map_df(col_pattern, function(x, case_name){
#       sim <- sim[eval(parse(text = case_col)) == case_name]
#       results <- map2_df(sim[, col_names(sim, x), with = F],
#                          obs[, col_names(obs, x), with = F], eval(parse(text = fun))) %>%
#         as.data.table() %>% .[, Sim_Case := case_name] %>%
#         setcolorder(c("Sim_Case", col_names(., "^(?!Sim_Case)"))) %>% .[]
#     }, case_name = .x)) %>% rbindlist()

#   nrow_results_per_case <-
#     map_int(results[, unique(Sim_Case)], ~nrow(results %>% .[Sim_Case == .x]))
#   nrow_obs_by_pattern <- nrow(obs %>% .[, lapply(.SD, unique), .SDcol = col_names(., by_pattern)]

#   if(melt){
#     if(!is.null(by_pattern){
#       if(all(nrow_results_per_case == nrow_obs_by_pattern){
#         results <-
#           sim %>% .[, .SD, .SDcol = col_names(., by_pattern)] %>%
#           bind_cols(., results) %>%
#           melt(., id.vars = c(col_names(.,by_pattern, "Sim_Case")),
#                variable.name = "Variable", value.name = fun) %>%
#           .[, lapply(.SD, function(x) signif(x=x, digits = 4)),
#             by = c(col_names(.,by_pattern, "Sim_Case", "Variable"))]

#       }else{
#         results %<>%
#           melt(., id.vars = c("Sim_Case"),
#                variable.name = "Variable", value.name = fun) %>%
#           .[, lapply(.SD, function(x) signif(x=x, digits = 4)),
#             by = c(col_names(.,by_pattern, "Sim_Case", "Variable"))]
#         warning("'by_pattern' feature will not apply because of non-equal length of 'by_pattern' in 'obs' and results.")
#       }

#     }else{
#       results %<>%
#         melt(., id.vars = c("Sim_Case"), variable.name = "Variable", value.name = fun) %>%
#         .[, lapply(.SD, function(x) signif(x=x, digits = 4)), by = c("Sim_Case", "Variable")]
#     }

#   }else{
#     results %<>%
#       .[, Stat_Fun := fun]
#     if(!is.null(by_pattern){
#       if(all(nrow_results_per_case == nrow_obs_by_pattern){
#         results <-
#           sim %>% .[, .SD, .SDcol = col_names(., by_pattern)] %>%
#           bind_cols(., results) %>%
#           .[, lapply(.SD, function(x) signif(x = x, digits = 4)),
#             by = c(col_names(.,by_pattern, "Sim_Case", "Stat_Fun"))]
#       }else{
#       results %<>%
#         .[, lapply(.SD, function(x) signif(x = x, digits = 4)),
#           by = c(case_col, "Stat_Fun")]
#         warning("'by_pattern' feature will not apply because of non-equal length of 'by_pattern' in 'obs' and results.")
#       }

#     }else{
#       results %<>%
#       .[, lapply(.SD, function(x) signif(x = x,digits = 4)), by = c("Sim_Case", "Stat_Fun")]
#     }
#   }

#   return(results)
# }
# # }}}1

# # statmCal: A function to make multiple statistical calculation between two data.tables
# {{{1
# statmCal <- function(sim, obs, col_pattern, fun, melt = FALSE, by_pattern = NULL, case_col = "Case", case.align = FALSE){

#   if(case.align){
#     sim <- caseAlign(sim, obs, case_col = case_col, bind = FALSE)
#   }

#   if(!(hasArg(col_pattern))){
#     col_pattern = "^(?!DateTime|Case)"
#   }

#   if(!(hasArg(fun))){
#     stop("Argument 'fun' is missing!")
#   }

#   if(melt == TRUE){
#     Reduce(function(...) merge(..., all = T),
#            lapply(fun, function(fun) statCal(sim = sim, obs = obs, fun,
#                                              col_pattern = col_pattern, melt = TRUE,
#                                              by_pattern = by_pattern, case_col = case_col,
#                                              case.align = case.align))) %>%
#     return(.)
#   }else{
#     lapply(fun, function(fun) statCal(sim = sim, obs = obs, fun,
#                                       col_pattern = col_pattern, melt = FALSE,
#                                       by_pattern = by_pattern, case_col = case_col,
#                                       case.align = case.align)) %>%
#     rbindlist(.) %>%
#     return(.)
#   }
# }
# # }}}1

# # dataComb: A function to combine two data.tables by cases.
# # {{{1
# dataComb <- function(sim, obs, date_col = "DateTime", col_pattern,
#                         case.name = c("sim", "obs"), melt = TRUE){
#   data <-
#     rbind(
#       sim %>% copy(.) %>% .[, Case := case.name[1]],
#       obs %>% copy(.) %>% .[, Case := case.name[2]],
#       use.names = T)

#   if(hasArg(col_pattern)){
#     data %<>%
#       .[, .SD, .SDcol = col_names(., col_pattern),
#         by = c(date_col, "Case")]
#   }

#   if(melt){
#     data %<>%
#       melt(id.vars = c(date_col, "Case"),
#            variable.name = "Variable", value.name = "Value")
#   }

#   data %<>%
#     setcolorder(c(date_col, "Case",
#                 col_names(., paste("^(?!", date_col, "|Case)", sep = ""))))

#   return(data)
# }
# # }}}1

# # linePlot: A function to plot simulated and measured data using geom_line
# # {{{1
# linePlot <- function(data, date_col = "DateTime", legend.label = "case",
#                      line.type = TRUE, scales = "free_x", ...){
#   data %<>%
#     copy(.) %>%
#     .[, Variable_Case:= paste(Variable, Case, sep = "_")] %>% .[]
# if(line.type){
#   if(legend.label == "case"){
#     data %>%
#     ggplot(aes_(x = as.name(date_col), y = quote(Value),
#                 color = quote(Case), fill = quote(Case), linetype = quote(Case))) +
#     geom_line(...) +
#     scale_x_datetime(name = "", breaks = date_breaks("1 day"),
#                      labels = date_format("%m/%d", tz = Sys.timezone()), expand =
#                                           c(0.01,0)) +
#     facet_wrap(~Variable, ncol = 1, scales = scales) +
#     theme_bw() +
#     theme(legend.position = "bottom",
#           legend.background = element_rect(color = "black"),
#           legend.key = element_rect(color = "gray"),
#           strip.background = element_rect(fill = "white", color = "black")) %>%
#     return(.)

#   }else if(legend.label == "variable"){
#     data %>%
#     ggplot(aes_(x = as.name(date_col), y = quote(Value),
#                 color = quote(Variable_Case), fill = quote(Variable_Case), linetype = quote(Variable_Case))) +
#     geom_line(...) +
#     scale_x_datetime(name = "", breaks = date_breaks("1 day"),
#                      labels = date_format("%m/%d", tz = Sys.timezone()), expand =
#                                           c(0.01,0)) +
#     facet_wrap(~Variable, ncol = 1, scales = scales) +
#     theme_bw() +
#     guides(col = guide_legend(nrow = 2)) +
#     theme(legend.position = "bottom",
#           legend.background = element_rect(color = "black"),
#           legend.key = element_rect(color = "gray"),
#           strip.background = element_rect(fill = "white", color = "black")) %>%
#     return(.)

#   }else {

#     stop("Invilid 'legend.label': ", legend.label, " !")
#   }
# }else{
#   if(legend.label == "case"){
#     data %>%
#     ggplot(aes_(x = as.name(date_col), y = quote(Value),
#                 color = quote(Case), fill = quote(Case))) +
#     geom_line(...) +
#     scale_x_datetime(name = "", breaks = date_breaks("1 day"),
#                      labels = date_format("%m/%d", tz = Sys.timezone()), expand =
#                                           c(0.01,0)) +
#     facet_wrap(~Variable, ncol = 1, scales = scales) +
#     theme_bw() +
#     theme(legend.position = "bottom",
#           legend.background = element_rect(color = "black"),
#           legend.key = element_rect(color = "gray"),
#           strip.background = element_rect(fill = "white", color = "black")) %>%
#     return(.)

#   }else if(legend.label == "variable"){
#     data %>%
#     ggplot(aes_(x = as.name(date_col), y = quote(Value),
#                 color = quote(Variable_Case), fill = quote(Variable_Case))) +
#     geom_line(...) +
#     scale_x_datetime(name = "", breaks = date_breaks("1 day"),
#                      labels = date_format("%m/%d", tz = Sys.timezone()), expand =
#                                           c(0.01,0)) +
#     facet_wrap(~Variable, ncol = 1, scales = scales) +
#     theme_bw() +
#     guides(col = guide_legend(nrow = 2)) +
#     theme(legend.position = "bottom",
#           legend.background = element_rect(color = "black"),
#           legend.key = element_rect(color = "gray"),
#           strip.background = element_rect(fill = "white", color = "black")) %>%
#     return(.)

#   }else {

#     stop("Invilid 'legend.label': ", legend.label, " !")
#   }

# }
# }
# # }}}1

# # barPlot: A function for easy EnergyPlus meter data plotting using geom_bar
# # {{{1
# barPlot <- function(data, x.col = "Month", y.col = "value",
#                     case_col = "Case", fill.col, facet.col, scales.free = "free_x"){
#   if(!(x.col %in% names(data))){
#     stop("x column not found in data input!")
#   }
#   if(!(y.col %in% names(data))){
#     stop("y column not found in data input!")
#   }
#   if(!(case_col %in% names(data))){
#     stop("Case column not found in data input!")
#   }
#   if(!hasArg(fill.col)){
#     plot <-
#       data %>%
#       ggplot(aes_(x= as.name(x.col), y = as.name(y.col),
#                   group = as.name(case_col), fill = as.name(case_col))) +
#       geom_bar(stat = "identity", position = "dodge", color = "black") +
#       scale_y_continuous(expand = c(0,0.02)) +
#       ylab("") +
#       scale_color_hue(l = 1)
#   }else{
#     plot <-
#       data %>%
#       ggplot(aes_(x= as.name(x.col), y = as.name(y.col),
#                   group = as.name(case_col), fill = as.name(fill.col))) +
#       geom_bar(stat = "identity", position = "dodge", color = "black") +
#       scale_y_continuous(expand = c(0,0.02)) +
#       ylab("") +
#       scale_color_hue(l = 1)
#   }
#   if(!hasArg(facet.col)){
#     plot <-
#       plot + facet_wrap(~eval(parse(text=case_col)), ncol = 1, scales = scales.free)
#   }else{
#     plot <-
#       plot + facet_wrap(~eval(parse(text=facet.col)), ncol = 1, scales = scales.free)
#   }
#   if(x.col == "Month"){
#     plot <-
#       plot +
#       scale_x_continuous(breaks = seq(1:12), expand = c(0.01,0))
#   }

#   if(x.col == "Year"){
#     plot <-
#       plot +
#       scale_x_continuous(expand = c(0.01,0)) +
#       theme(axis.text.x = element_blank(),
#             axis.ticks.x = element_blank(),
#             panel.grid.major.x = element_blank(),
#             panel.grid.minor.x = element_blank(),
#             strip.background = element_blank())
#   }
#   return(plot)
# }
# # }}}1

# theme_clean: A theme with minimal formatting of ggplot2
# {{{1
theme_clean <- function(base_size = 14, base_family = "Palatino Linotype") {
    theme_bw(base_size = base_size, base_family = base_family) +
        theme(axis.title = element_text(size = 14),
              axis.text = element_text(face = "bold"),
              legend.key=element_rect(colour=NA, fill =NA),
              panel.grid = element_blank(),
              panel.border = element_rect(fill = NA, colour = "black", size=1),
              panel.background = element_rect(fill = "white", colour = "black"),
              strip.background = element_rect(fill = NA))
}
# }}}1
