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
  # Retrieve path from the URL and copy them to r$pathToResultsDatabase
  #
  rf_urlParams <- shiny::reactive({
    query <- shiny::parseQueryString(session$clientData$url_search)
    analysisType <- query[['analysisType']]
    pathToResultsDatabase <- query[['pathToResultsDatabase']]
    if (!is.null(analysisType) && !is.null(pathToResultsDatabase)) {
      return(list(analysisType = analysisType, pathToResultsDatabase = pathToResultsDatabase))
    }else{
      return(list(analysisType = "CohortDiagnostics", pathToResultsDatabase = "/var/folders/sv/b7srhyqn3fnd5hz384ppxlbs7dcz72/T//RtmpAaaPRL/testtimeCohortDiagnostics/results/CohortDiagnosticsResults.sqlite"))
    }
  })

  #
  # based on rf_pathToResultsDatabase, loads module ui. Then clicks hidenButton to trigger the server load
  #
  shiny::observe({
    shiny::req(rf_urlParams())

    analysisType <- rf_urlParams()$analysisType
    pathToResultsDatabase <- rf_urlParams()$pathToResultsDatabase

    # log start
    ParallelLogger::logInfo("[Start] Start logging")
    ParallelLogger::logInfo("[Start] analysisType: ", analysisType)
    ParallelLogger::logInfo("[Start] pathToResultsDatabase: ", pathToResultsDatabase)

    # select module ui based on analysisType
    if (analysisType == "CohortDiagnostics") {
      ui <- mod_cohortDiagnosticsVisualization_ui(pathToResultsDatabase)
    }

    # load module ui
    shiny::insertUI(
      selector = "#hidenButton",
      where = "afterEnd",
      ui = ui
    )

    ParallelLogger::logInfo("[Start] Loaded module UI for ", analysisType)

    # trigger button on flushed
    shiny::onFlushed(function (){
      # shinyjs::runjs('$(".wrapper").css("height", "auto");')
      # shinyjs::runjs('$(".shiny-spinner-placeholder").hide();')
      # shinyjs::runjs('$(".load-container.shiny-spinner-hidden.load1").hide();')
      shinyjs::runjs('$("#hidenButton").click();')
    })

  })

  #
  # when the button is clicked after flushed, load the module server,  rf_pathToResultsDatabase
  #
  shiny::observeEvent(input$hidenButton,{
    shiny::req(rf_urlParams())

    analysisType <- rf_urlParams()$analysisType
    pathToResultsDatabase <- rf_urlParams()$pathToResultsDatabase

    # load module server based on analysisType
    if(analysisType == "CohortDiagnostics"){
      mod_cohortDiagnosticsVisualization_server(pathToResultsDatabase)
    }

    ParallelLogger::logInfo("[Start] Loaded module server for ", analysisType)

  })

}
























