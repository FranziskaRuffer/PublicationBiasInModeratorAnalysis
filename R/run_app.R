#' Launch the Shiny app
#'
#' This function launches the Shiny application included in the package.
#' @export
pb_mods_App <- function() {
  app_dir <- system.file("shiny", package = "PublicationBiasInModeratorAnalysis")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try re-installing the package.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
