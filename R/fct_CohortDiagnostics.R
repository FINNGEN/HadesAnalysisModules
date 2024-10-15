
#' @title execute_CohortDiagnostics
#' @description This function calculates cohort overlaps based on the provided cohort table and analysis settings, and exports the results to a DuckDB database.
#'
#' @param exportFolder A string representing the path to the folder where the results will be exported.
#' @param cohortTableHandler An R6 object of class `CohortTableHandler` containing information about the cohort tables.
#' @param analysisSettings A list containing analysis settings, including `cohortIds` and `minCellCount`.
#'
#' @return A string representing the path to the exported results database.
#'
#' @importFrom checkmate assertDirectoryExists assertR6 assertList assertSubset assertNumeric checkFileExists
#' @importFrom ParallelLogger logInfo
#' @importFrom dplyr filter mutate select as_tibble
#' @importFrom duckdb dbConnect dbDisconnect dbWriteTable dbListTables
#' @importFrom DBI dbGetQuery
#' @importFrom tibble tibble
#' @importFrom yaml as.yaml
#'
#' @export
#'
execute_CohortDiagnostics <- function(
    exportFolder,
    cohortTableHandler,
    analysisSettings
) {
  #
  # Check parameters
  #
  exportFolder |> checkmate::assertDirectoryExists()
  cohortTableHandler |> checkmate::assertR6(class = "CohortTableHandler")
  analysisSettings <- analysisSettings |> assertAnalysisSettings_CohortDiagnostics()


  # get parameters from cohortTableHandler
  connection <- cohortTableHandler$connectionHandler$getConnection()
  cohortTable <- cohortTableHandler$cohortTableNames$cohortTable
  cdmDatabaseSchema <- cohortTableHandler$cdmDatabaseSchema
  cohortDatabaseSchema <- cohortTableHandler$cohortDatabaseSchema
  vocabularyDatabaseSchema <- cohortTableHandler$vocabularyDatabaseSchema
  cohortDefinitionSet <- cohortTableHandler$cohortDefinitionSet
  databaseId <- cohortTableHandler$databaseName
  databaseName <- cohortTableHandler$CDMInfo$cdm_source_abbreviation
  databaseDescription <- cohortTableHandler$CDMInfo$cdm_source_name
  vocabularyVersionCdm <- cohortTableHandler$CDMInfo$cdm_version
  vocabularyVersion <- cohortTableHandler$vocabularyInfo$vocabulary_version

  # get parameters from analysisSettings
  cohortIds <- analysisSettings$cohortIds
  runInclusionStatistics <- analysisSettings$runInclusionStatistics
  runIncludedSourceConcepts <- analysisSettings$runIncludedSourceConcepts
  runOrphanConcepts <- analysisSettings$runOrphanConcepts
  runVisitContext <- analysisSettings$runVisitContext
  runBreakdownIndexEvents <- analysisSettings$runBreakdownIndexEvents
  runIncidenceRate <- analysisSettings$runIncidenceRate
  runCohortRelationship <- analysisSettings$runCohortRelationship
  runTemporalCohortCharacterization <- analysisSettings$runTemporalCohortCharacterization
  temporalCovariateSettings <- analysisSettings$temporalCovariateSettings
  minCellCount <- analysisSettings$minCellCount

  exportFolderResults <- file.path(exportFolder, "results")
  if (!dir.exists(exportFolderResults)) {
    dir.create(exportFolderResults, recursive = TRUE)
  }

  CohortDiagnostics::executeDiagnostics(
    cohortDefinitionSet = cohortDefinitionSet,
    exportFolder = exportFolderResults,
    databaseId = databaseId,
    cohortDatabaseSchema = cohortDatabaseSchema,
    databaseName = databaseName,
    databaseDescription = databaseDescription,
    connection = connection,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = cohortTable,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    cohortIds = cohortIds,
    runInclusionStatistics = runInclusionStatistics,
    runIncludedSourceConcepts = TRUE,
    runOrphanConcepts = TRUE,
    runTimeSeries = FALSE,
    runVisitContext = TRUE,
    runBreakdownIndexEvents = TRUE,
    runIncidenceRate = TRUE,
    runCohortRelationship = TRUE,
    runTemporalCohortCharacterization = TRUE,
    temporalCovariateSettings = temporalCovariateSettings,
    minCellCount = minCellCount,
    # TODO
    # minCharacterizationMean = 0.01,
    # irWashoutPeriod = 0
    incremental = TRUE,
    incrementalFolder = cohortTableHandler$incrementalFolder
  )

  pathToResultsDatabase <- file.path(exportFolderResults, "CohortDiagnosticsResults.sqlite")

  CohortDiagnostics::createMergedResultsFile(
    dataFolder = exportFolderResults,
    sqliteDbPath = pathToResultsDatabase)

  return(pathToResultsDatabase)

}

#' @title Assert Analysis Settings for CohortDiagnostics
#' @description Validates the `analysisSettings` list to ensure it contains the required elements (`cohortIdCases`, `cohortIdControls`, `analysisIds`, `covariatesIds`, `minCellCount`, `chunksSizeNOutcomes`, `cores`) with correct types and values. This function is specifically designed for checking settings related to CohortDiagnostics analysis.
#'
#' @param analysisSettings A list containing analysis settings. It must include the following elements:
#' \describe{
#'   \item{cohortIdCases}{A numeric value representing the cohort ID for cases.}
#'   \item{cohortIdControls}{A numeric value representing the cohort ID for controls.}
#'   \item{analysisIds}{A numeric vector of analysis IDs.}
#'   \item{covariatesIds}{A numeric vector of covariate IDs (optional).}
#'   \item{minCellCount}{A numeric value representing the minimum cell count, must be 0 or higher.}
#'   \item{chunksSizeNOutcomes}{A numeric value representing the chunk size for outcomes (optional).}
#'   \item{cores}{A numeric value representing the number of cores to use for parallel processing.}
#' }
#'
#' @return Returns the validated `analysisSettings` list.
#'
#' @importFrom checkmate assertList assertSubset assertNumeric
#'
#' @export
assertAnalysisSettings_CohortDiagnostics <- function(analysisSettings) {

  analysisSettings |> checkmate::assertList()
  c('cohortIds',
    'runInclusionStatistics',
    'runIncludedSourceConcepts',
    'runOrphanConcepts',
    'runVisitContext',
    'runBreakdownIndexEvents',
    'runIncidenceRate',
    'runCohortRelationship',
    'runTemporalCohortCharacterization',
    'temporalCovariateSettings',
    'minCellCount'
  ) |> checkmate::assertSubset(names(analysisSettings))

  analysisSettings$cohortIds |> checkmate::assertNumeric()
  analysisSettings$runInclusionStatistics |> checkmate::assertLogical()
  analysisSettings$runIncludedSourceConcepts |> checkmate::assertLogical()
  analysisSettings$runOrphanConcepts |> checkmate::assertLogical()
  analysisSettings$runVisitContext |> checkmate::assertLogical()
  analysisSettings$runBreakdownIndexEvents |> checkmate::assertLogical()
  analysisSettings$runIncidenceRate |> checkmate::assertLogical()
  analysisSettings$runCohortRelationship |> checkmate::assertLogical()
  analysisSettings$runTemporalCohortCharacterization |> checkmate::assertLogical()
  analysisSettings$temporalCovariateSettings |> checkmate::assertList()
  analysisSettings$minCellCount |> checkmate::assertNumeric()

  return(analysisSettings)

}
