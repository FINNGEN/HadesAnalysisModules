#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny tagList tags div p
#' 
#' @noRd
app_ui <- function(request) {

  # get options from url
  analysisTypeFromOptions <- getOption("HadesAnalysisModules.analysisType")
  pathToResultsDatabaseFromOptions <- getOption("HadesAnalysisModules.pathToResultsDatabase")

  # if not options it means that the server did not receive the parameters
  if (analysisTypeFromOptions == "" || pathToResultsDatabaseFromOptions== "") {
    return(
      shiny::tagList(
        shiny::tags$div(
          shiny::tags$p("Loading ...")
        )
      )
    )
  }else{

    if (file.exists(pathToResultsDatabaseFromOptions) == FALSE) {
      return(
        shiny::tagList(
          shiny::tags$div(
            shiny::tags$p("The path to the results database does not exist: ", pathToResultsDatabaseFromOptions)
          )
        )
      )
    }

    if (analysisTypeFromOptions == "CohortDiagnostics") {
      return(
        mod_cohortDiagnosticsVisualization_ui(pathToResultsDatabase = pathToResultsDatabaseFromOptions)
      )
    }

    return(
      shiny::tagList(
        shiny::tags$div(
          shiny::tags$p("The analysis type is not supported: ", analysisTypeFromOptions)
        )
      )
    )
  }

}
