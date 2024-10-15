#
# test_that("mod_analysisSettings_CohortDiagnostics works", {
#
#   # set up
#   cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
#   withr::defer({rm(cohortTableHandler);gc()})
#
#   r_connectionHandler <- shiny::reactiveValues(
#     cohortTableHandler = cohortTableHandler,
#     hasChangeCounter = 0
#   )
#
#   # run module
#   shiny::testServer(
#     mod_analysisSettings_CohortDiagnostics_server,
#     args = list(
#       id = "test",
#       r_connectionHandler = r_connectionHandler
#     ),
#     {
#       # "aggregated"
#       session$setInputs(
#         selectCohorts_pickerInput = NULL,
#         runInclusionStatistics_switch = TRUE,
#         runIncludedSourceConcepts_switch = TRUE,
#         runOrphanConcepts_switch = TRUE,
#         runVisitContext_switch = TRUE,
#         runIncidenceRate_switch = TRUE,
#         runTemporalCohortCharacterization_switch = TRUE,
#         features_pickerInput = c(101, 141, 1, 2, 402, 702, 41),
#         minCellCount_numericInput = 5)
#
#       analysisSettings <- rf_analysisSettings()
#       analysisSettings  |> expect_null()
#
#        # cant update rf_ranges()
#
# })



