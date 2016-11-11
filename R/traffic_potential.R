#' Traffic Potential Tool
#'
#'A wrapper for shiny::runApp(). Does not take any arguments.
#'
#' @author Rahul Dodhia
#' @export
#'
#' @import shiny
#' @import shinydashboard
#' @import HelperFunctions
traffic_potential=function(...) {
  appDir <- system.file("shiny-examples","traffic_potential_app", package = "AvvoTrafficPotential")
  print(appDir)
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing 'AvvoTrafficPotential'.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal",...)
}
