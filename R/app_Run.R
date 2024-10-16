#' Launch the Shiny Application
#'
#' This function launches a Shiny application for CO2 analysis, configured with the provided modules.
#'
#' @param ... Additional arguments passed to `shiny::shinyApp`.
#'
#' @importFrom checkmate assert_list
#' @importFrom shiny shinyApp
#'
#' @return A Shiny application object.
#' @export
run_app <- function(...) {

  # fix for linux systems with weird rJava behaviour
  if (is.null(getOption("java.parameters")))
    options(java.parameters = "-Xss100m")

  app  <- shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    ...
  )

  options(HadesAnalysisModules.analysisType = "")
  options(HadesAnalysisModules.pathToResultsDatabase = "")

  return(app)
}

