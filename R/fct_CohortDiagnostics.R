
#' @title execute_CohortDiagnostics
#' @description This function executes CohortDiagnostics based on the provided cohort table and analysis settings, and exports the results to a specified folder.
#'
#' @param exportFolder A string representing the path to the folder where the results will be exported.
#' @param cohortTableHandler An R6 object of class `CohortTableHandler` containing information about the cohort tables.
#' @param analysisSettings A list containing analysis settings for CohortDiagnostics.
#'
#' @return A string representing the path to the exported results folder.
#'
#' @importFrom checkmate assertDirectoryExists assertR6
#' @importFrom CohortDiagnostics executeDiagnostics
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
#' @description Validates the `analysisSettings` list to ensure it contains the required elements for CohortDiagnostics analysis with correct types and values.
#'
#' @param analysisSettings A list containing analysis settings. It must include the following elements:
#' \describe{
#'   \item{cohortIds}{A numeric vector of cohort IDs.}
#'   \item{runInclusionStatistics}{A logical value indicating whether to run inclusion statistics.}
#'   \item{runIncludedSourceConcepts}{A logical value indicating whether to run included source concepts.}
#'   \item{runOrphanConcepts}{A logical value indicating whether to run orphan concepts.}
#'   \item{runVisitContext}{A logical value indicating whether to run visit context.}
#'   \item{runBreakdownIndexEvents}{A logical value indicating whether to run breakdown of index events.}
#'   \item{runIncidenceRate}{A logical value indicating whether to run incidence rate.}
#'   \item{runCohortRelationship}{A logical value indicating whether to run cohort relationship.}
#'   \item{runTemporalCohortCharacterization}{A logical value indicating whether to run temporal cohort characterization.}
#'   \item{temporalCovariateSettings}{A list of temporal covariate settings.}
#'   \item{minCellCount}{A numeric value representing the minimum cell count.}
#' }
#'
#' @return Returns the validated `analysisSettings` list.
#'
#' @importFrom checkmate assertList assertSubset assertNumeric assertLogical
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
