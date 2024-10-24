
test_that("execute_CohortDiagnostics works with defaultSettings", {
  # set up
  cohortTableHandler <-
    helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({
    rm(cohortTableHandler)
    gc()
  })

  exportFolder <- withr::local_tempdir('testCodeWAS')

  temporalCovariateSettings <- HadesExtras::FeatureExtraction_createTemporalCovariateSettingsFromList(
    analysisIds = c(101, 102, 141, 204, 601, 641, 301, 341, 404, 701, 702, 703, 741, 801, 841, 501, 541, 910, 911),
    temporalStartDays = c(-365*2, -365*1, 0, 1, 365+1),
    temporalEndDays = c(-365*1-1, -1, 0, 365*1, 365*2)
  )

  analysisSettings <- list(
    cohortIds = c(1,2),
    runInclusionStatistics = TRUE,
    runIncludedSourceConcepts = TRUE,
    runOrphanConcepts = TRUE,
    runVisitContext = TRUE,
    runBreakdownIndexEvents = FALSE,
    runIncidenceRate = TRUE,
    runCohortRelationship = FALSE,
    runTemporalCohortCharacterization = TRUE,
    temporalCovariateSettings = temporalCovariateSettings,
    minCellCount = 5
  )

  # function
  suppressWarnings({
  pathToResultsDatabase <- execute_CohortDiagnostics(
    exportFolder = exportFolder,
    cohortTableHandler = cohortTableHandler,
    analysisSettings = analysisSettings
  )
  })

  expect_true(file.exists(pathToResultsDatabase))

})


#
# test_that("execute_CohortDiagnostics works with imported from file", {
#   # set up
#   cohortTableHandler <-
#     helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
#   withr::defer({
#     rm(cohortTableHandler)
#     gc()
#   })
#
#   # create cohort from cohortData
#   sourcePersonToPersonId <- helper_getParedSourcePersonAndPersonIds(
#     connection = cohortTableHandler$connectionHandler$getConnection(),
#     cohortDatabaseSchema = cohortTableHandler$cdmDatabaseSchema,
#     numberPersons = 2*100
#   )
#
#   cohort_data <- tibble::tibble(
#     cohort_name = rep(c("Cohort A", "Cohort B"), 100),
#     person_source_value = sourcePersonToPersonId$person_source_value,
#     cohort_start_date = rep(as.Date(c("2020-01-01", "2020-01-01")), 100),
#     cohort_end_date = rep(as.Date(c("2020-01-03", "2020-01-04")), 100)
#   )
#
#   cohortDefinitionSet <- cohortDataToCohortDefinitionSet(
#     cohortData = cohort_data
#   )
#
#   cohortTableHandler$insertOrUpdateCohorts(cohortDefinitionSet)
#
#   exportFolder <- withr::local_tempdir('testCodeWAS')
#
#   temporalCovariateSettings <- HadesExtras::FeatureExtraction_createTemporalCovariateSettingsFromList(
#     analysisIds = c(101, 102, 141, 204, 601, 641, 301, 341, 404, 701, 702, 703, 741, 801, 841, 501, 541, 910, 911),
#     temporalStartDays = c(-365*2, -365*1, 0, 1, 365+1),
#     temporalEndDays = c(-365*1-1, -1, 0, 365*1, 365*2)
#   )
#
#   analysisSettings <- list(
#     cohortIds = c(1,2),
#     runInclusionStatistics = TRUE,
#     runIncludedSourceConcepts = TRUE,
#     runOrphanConcepts = TRUE,
#     runVisitContext = TRUE,
#     runBreakdownIndexEvents = FALSE,
#     runIncidenceRate = TRUE,
#     runCohortRelationship = FALSE,
#     runTemporalCohortCharacterization = TRUE,
#     temporalCovariateSettings = temporalCovariateSettings,
#     minCellCount = 5
#   )
#
#   # function
#   suppressWarnings({
#     pathToResultsDatabase <- execute_CohortDiagnostics(
#       exportFolder = exportFolder,
#       cohortTableHandler = cohortTableHandler,
#       analysisSettings = analysisSettings
#     )
#   })
#
#   expect_true(file.exists(pathToResultsDatabase))
#
# })

