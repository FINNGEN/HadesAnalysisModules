#' CohortDiagnostics Results Visualization Module
#'
#' This module provides UI and server components for visualizing CohortDiagnostics results.
#'
#' @param pathToResultsDatabase The path to the results database
#'
#' @importFrom shiny NS
#' @importFrom OhdsiShinyModules cohortDiagnosticsServer
#' @importFrom DatabaseConnector createConnectionDetails
#' @importFrom checkmate assertCharacter assertFileExists
#'
#' @export
mod_cohortDiagnosticsVisualization_ui <- function(pathToResultsDatabase) {

  analisysResults <- .dataFromCohortDiagnosticsSqlitePath(pathToResultsDatabase)

  connectionHandler <- analisysResults$connectionHandler
  dataSource <- analisysResults$dataSource
  resultDatabaseSettings <- analisysResults$resultDatabaseSettings

  newui <- source(system.file("shiny", "DiagnosticsExplorer", "ui.R", package = "CohortDiagnostics"), local = TRUE)

  return(newui)
}


#' CohortDiagnostics Results Visualization Server Module
#'
#' This function creates a server module for visualizing CohortDiagnostics results.
#'
#' @param pathToResultsDatabase The path to the results database
#'
#' @return A server module for visualizing CohortDiagnostics results
#' @importFrom OhdsiShinyModules cohortDiagnosticsServer
#' @importFrom DatabaseConnector createConnectionDetails
#' @importFrom checkmate assertCharacter assertFileExists
#'
#' @export
mod_cohortDiagnosticsVisualization_server <- function(pathToResultsDatabase) {

  analisysResults <- .dataFromCohortDiagnosticsSqlitePath(pathToResultsDatabase)

  connectionHandler <- analisysResults$connectionHandler
  dataSource <- analisysResults$dataSource
  resultDatabaseSettings <- analisysResults$resultDatabaseSettings

  OhdsiShinyModules::cohortDiagnosticsServer(
    id = "DiagnosticsExplorer",
    connectionHandler = connectionHandler,
    dataSource = dataSource,
    resultDatabaseSettings = shinySettings
  )
}


.dataFromCohortDiagnosticsSqlitePath  <- function(pathToSqlite) {
  checkmate::assertCharacter(pathToSqlite)
  checkmate::assertFileExists(pathToSqlite, extension = "sqlite")

  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = pathToSqlite)

  shinySettings <- list(
    connectionDetails = connectionDetails,
    resultsDatabaseSchema = c("main"),
    vocabularyDatabaseSchema = c("main"),
    aboutText = NULL,
    tablePrefix = "",
    cohortTableName = "cohort",
    databaseTableName = "database",
    enableAnnotation = TRUE,
    enableAuthorization = FALSE
  )

  connectionHandler <- ResultModelManager::PooledConnectionHandler$new(shinySettings$connectionDetails)

  resultDatabaseSettings <- list(
    schema = shinySettings$resultsDatabaseSchema,
    vocabularyDatabaseSchema = shinySettings$vocabularyDatabaseSchema,
    cdTablePrefix = shinySettings$tablePrefix,
    cgTable = shinySettings$cohortTableName,
    databaseTable = shinySettings$databaseTableName
  )

  dataSource <-
    OhdsiShinyModules::createCdDatabaseDataSource(connectionHandler = connectionHandler,
                                                  resultDatabaseSettings = resultDatabaseSettings)

  return(list(
    connectionHandler = connectionHandler,
    dataSource = dataSource,
    resultDatabaseSettings = shinySettings
  ))

}
