#' @export
runUI <- function() {
  appDir <- system.file("shiny-examples", "vsapp", package = "variableStars")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `variableStars`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}