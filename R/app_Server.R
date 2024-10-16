#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny reactive parseQueryString observe req insertUI onFlushed observeEvent
#' @importFrom duckdb dbConnect duckdb
#' @importFrom dplyr tbl collect pull
#' @importFrom ParallelLogger logInfo
#' @noRd
app_server <- function(input, output, session) {

  #
  # Retrieve path from the URL and copy them to
  #
  rf_urlParams <- shiny::reactive({
    query <- shiny::parseQueryString(session$clientData$url_search)
    analysisType <- query[['analysisType']]
    pathToResultsDatabase <- query[['pathToResultsDatabase']]
    if (!is.null(analysisType) && !is.null(pathToResultsDatabase)) {
      return(list(analysisType = analysisType, pathToResultsDatabase = pathToResultsDatabase))
    }else{
      return(list(analysisType = "", pathToResultsDatabase = ""))
    }
  })

  #
  # based on rf_pathToResultsDatabase, loads module ui. Then clicks hidenButton to trigger the server load
  #
  shiny::observe({
    shiny::req(rf_urlParams())

    # get parameters from url
    analysisTypeFromUrl <- rf_urlParams()$analysisType
    pathToResultsDatabaseFromUrl <- rf_urlParams()$pathToResultsDatabase

    # get parameters from options
    analysisTypeFromOptions <- getOption("HadesAnalysisModules.analysisType")
    pathToResultsDatabaseFromOptions <- getOption("HadesAnalysisModules.pathToResultsDatabase")

    # log start
    ParallelLogger::logInfo("[Start] Start logging")
    ParallelLogger::logInfo("[Start] analysisTypeFromUrl: ", analysisTypeFromUrl, ", pathToResultsDatabaseFromUrl: ", pathToResultsDatabaseFromUrl)
    ParallelLogger::logInfo("[Start] analysisTypeFromOptions: ", analysisTypeFromOptions, ", pathToResultsDatabaseFromOptions: ", pathToResultsDatabaseFromOptions)

    # if parameters empty or have change, the update and reload
    if (
      analysisTypeFromUrl != analysisTypeFromOptions || pathToResultsDatabaseFromUrl != pathToResultsDatabaseFromOptions
      ) {
      options("HadesAnalysisModules.analysisType" = analysisTypeFromUrl)
      options("HadesAnalysisModules.pathToResultsDatabase" = pathToResultsDatabaseFromUrl)
      ParallelLogger::logInfo("[Start] Reload UI ")
      session$reload()
    }else{
      # if up to date call module server
      if(analysisTypeFromOptions == "CohortDiagnostics" & file.exists(pathToResultsDatabaseFromOptions) == TRUE){
        mod_cohortDiagnosticsVisualization_server(pathToResultsDatabaseFromOptions)
      }
    }

    ParallelLogger::logInfo("[Start] Loaded module server for ", analysisTypeFromOptions)

  })



}
























