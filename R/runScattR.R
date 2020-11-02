#' @export
runScattR <- function() {

  # Hierarchy
  if (exists("layer1") == F) define_hierarchy()

  # Forcings
  if (exists("forced_factor") == F) input_color_group()
  if (exists("forced_character") == F) input_mouseclick()
  if (exists("forced_numeric") == F) input_xy()

  # App location in package
  appDir <- system.file("shiny-examples", "scattr", package = "scattr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `scattr`.", call. = FALSE)
  }

  # Run App
  shiny::runApp(appDir, display.mode = "normal", launch.browser = T)
}
