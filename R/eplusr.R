#' eplusr: A Toolkit for Using EnergyPlus in R
#'
#' @details eplusr provides a rich toolkit of using EnergyPlus directly in
#' R, which enables programmatic navigation, modification of EnergyPlus models
#' and makes it less painful to do parametric simulations and analysis.
#'
#' @section Features:
#'
#' - Download and install EnergyPlus in R
#' - Read, parse and modify EnergyPlus:
#'     - Input Data File (IDF)
#'     - Weather File (EPW)
#'     - Report Data Dictionary (RDD) & Meter Data Dictionary (MDD)
#'     - Error File (ERR)
#' - Modify multiple versions of IDFs and run corresponding EnergyPlus
#'   both in the background and in the front
#' - Rich-featured interfaces to query and modify IDFs
#' - Automatically handle referenced fields and validate input during
#'   modification
#' - Take fully advantage of most common used data structure for data
#'   science in R – data.frame
#'     - Extract model, weather data into data.frames
#'     - Modify multiple objects via data.frames input
#'     - Query output via SQL in Tidy format which is much better for
#'       data analysis and visualization
#' - Provide a simple yet extensible prototype of conducting parametric
#'   simulations and collect all results in one go
#' - A pure R-based version updater [transition()] which is much faster than
#'   VersionUpdater distributed with EnergyPlus
#'
#' @name eplusr-package
#' @author Hongyuan Jia
"_PACKAGE"
