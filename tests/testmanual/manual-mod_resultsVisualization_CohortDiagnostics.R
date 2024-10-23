
# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

# set up
cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasAsthmaCohortsMatched")
on.exit({rm(cohortTableHandler);gc()})

exportFolder <- file.path(tempdir(), "testtimeCohortDiagnostics")
dir.create(exportFolder, showWarnings = FALSE)
on.exit({unlink(exportFolder, recursive = TRUE)})

temporalCovariateSettings <- HadesExtras::FeatureExtraction_createTemporalCovariateSettingsFromList(
  analysisIds = c(101, 102, 141, 204, 601, 641, 301, 341, 404, 701, 702, 703, 741, 801, 841, 501, 541, 910, 911),
  temporalStartDays = c(-365*2, -365*1, 0, 1, 365+1),
  temporalEndDays = c(-365*1-1, -1, 0, 365*1, 365*2)
)

analysisSettings <- list(
  cohortIds = c(2, 2001),
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
pathToResultsDatabase <- execute_CohortDiagnostics(
  exportFolder = exportFolder,
  cohortTableHandler = cohortTableHandler,
  analysisSettings = analysisSettings
)



# run app --------------------------------------------------------------
CohortDiagnostics::launchDiagnosticsExplorer(pathToResultsDatabase, launch.browser = TRUE)

# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  mod_cohortDiagnosticsVisualization_ui( pathToResultsDatabase),
  function(input,output,session){
    mod_cohortDiagnosticsVisualization_server(pathToResultsDatabase)
  },
  options = list(launch.browser=TRUE)
)

app

# run full app --------------------------------------------------------------
devtools::load_all(".")

options = list(launch.browser=FALSE, port = 5907)

browseURL(paste0("http://localhost:5907/?analysisType=CohortDiagnostics&pathToResultsDatabase=", pathToResultsDatabase))

run_app(options = options)


# write to textfile
a$value |> as.character() |> brio::writeLines(con  = "manualtest-mod_resultsVisualization_CohortDiagnostics.html")
