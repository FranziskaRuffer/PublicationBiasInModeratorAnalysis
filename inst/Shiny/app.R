#' # Shiny App: Publication Bias Sensitivity Analysis for Meta-Analyses with Moderators
#' 
#' ## How to run this app
#' 
#' ### Install the R package for this project from Github
#' devtools::install_github("FranziskaRuffer/PublicationBiasInModeratorAnalysis")
#' 
#' ### Load the package and run the app
#' library(PublicationBiasInModeratorAnalysis)
#' shiny::runApp(system.file("shiny", package = "PublicationBiasInModeratorAnalysis"))
#' 
shiny::shinyApp(ui, server)
