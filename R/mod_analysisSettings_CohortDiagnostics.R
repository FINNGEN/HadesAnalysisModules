#' @title CohortDiagnostics Analysis Settings UI
#' @description UI module for configuring the settings for CohortDiagnostics analysis. This module allows users to select cohorts and configure various analysis options.
#'
#' @param id A string representing the module's namespace.
#'
#' @return A Shiny UI element that can be included in a Shiny app.
#'
#' @importFrom shiny NS tags h4 numericInput checkboxInput
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tagList hr
#'
#' @export
#'
mod_analysisSettings_CohortDiagnostics_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    #
    shiny::tags$h4("Cohorts"),
    shinyWidgets::pickerInput(
      inputId = ns("selectCohorts_pickerInput"),
      label = "Select one or more cohorts:",
      choices = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    #
    htmltools::hr(),
    shiny::tags$h4("Settings"),
    shiny::checkboxInput(
      inputId = ns("runInclusionStatistics_switch"),
      label = "Run Inclusion Statistics (for Atlas cohorts, calculates attrition plot)",
      value = TRUE
    ),
    shiny::checkboxInput(
      inputId = ns("runIncludedSourceConcepts_switch"),
      label = "Run Included Source Concepts",
      value = TRUE
    ),
    shiny::checkboxInput(
      inputId = ns("runOrphanConcepts_switch"),
      label = "Run Orphan Concepts (for Atlas cohorts, suggest concepts to add to cohort definition)",
      value = TRUE
    ),
    shiny::checkboxInput(
      inputId = ns("runVisitContext_switch"),
      label = "Run Visit Context (Visit types around the cohort start)",
      value = TRUE
    ),
    shiny::checkboxInput(
      inputId = ns("runIncidenceRate_switch"),
      label = "Run Incidence Rate",
      value = TRUE
    ),
    # shiny::checkboxInput(
    #   inputId = ns("runCohortRelationship_switch"),
    #   label = "Run Cohort Relationship",
    #   value = FALSE
    # ),
    shiny::checkboxInput(
      inputId = ns("runTemporalCohortCharacterization_switch"),
      label = "Run Temporal Cohort Characterization (Find covarates in time windows)",
      value = FALSE
    ),
    shiny::numericInput(
      inputId = ns("minCellCount_numericInput"),
      label = "Min Cell Count",
      width = "100px",
      value = 1,
      min = 1,
      max = 1000
    ),
    htmltools::hr(),
    #
    mod_fct_covariateSelector_ui(
      inputId = ns("features_pickerInput"),
      label = "Select features to compare between cases and controls:",
      analysisIdsToShow = c(
        101, 102, 141, 204,
        1, 2, 3, 6, 8, 9, 10, 41,
        601, 641,
        301, 341, 404, 906,
        701, 702, 703, 741, 908,
        801, 841, 909,
        501, 541, 907,
        910, 911 ),
      analysisIdsSelected = c(141, 1, 2, 8, 10, 641, 341, 404, 701, 702, 841, 541 )
    ),
    #
    htmltools::hr(),
    shiny::tags$h4("Time windows"),
    mod_temporalRanges_ui(ns("time_windows")),
    htmltools::hr()
  )
}

#' @title CohortDiagnosticss Analysis Settings Server
#' @description Server module for handling the logic of the cohort overlaps analysis settings UI. This module updates the UI elements based on the selected cohorts and returns the analysis settings as a reactive expression.
#'
#' @param id A string representing the module's namespace.
#' @param r_connectionHandler A reactive object that provides access to the cohort data and connection information.
#'
#' @return A reactive expression that returns the analysis settings as a list.
#'
#' @importFrom shiny moduleServer reactive req observe
#' @importFrom shinyjs toggleState
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom HadesExtras FeatureExtraction_createTemporalCovariateSettingsFromList
#'
#' @export
#'
mod_analysisSettings_CohortDiagnostics_server <- function(id, r_connectionHandler) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns


    #
    # reactive variables
    #
    startRanges = list(c(-50000,50000), c(0,0), c(-50000,-1), c(1,50000))
    rf_ranges <- mod_temporalRanges_server("time_windows", startRanges = startRanges)


    #
    # render selectDatabases_pickerInput with database names
    #
    shiny::observe({
      shiny::req(r_connectionHandler$cohortTableHandler)
      shiny::req(r_connectionHandler$hasChangeCounter)

      cohortIdAndNames <- r_connectionHandler$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- list()
      if(nrow(cohortIdAndNames) != 0){
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, paste(cohortIdAndNames$shortName, "("  , cohortIdAndNames$cohortName, ")")))
      }

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "selectCohorts_pickerInput",
        choices = cohortIdAndNamesList,
        selected = NULL
      )
    })


    #
    # return reactive options
    #
    rf_analysisSettings <- shiny::reactive({
      if(
        is.null(input$selectCohorts_pickerInput) |
        is.null(input$minCellCount_numericInput) |
        is.null(input$runInclusionStatistics_switch) |
        is.null(input$runIncludedSourceConcepts_switch) |
        is.null(input$runOrphanConcepts_switch) |
        is.null(input$runVisitContext_switch) |
        is.null(input$runIncidenceRate_switch) |
        #is.null(input$runCohortRelationship_switch) |
        is.null(input$runTemporalCohortCharacterization_switch) |
        is.null(input$features_pickerInput)
      ){
        return(NULL)
      }


      temporalCovariateSettings  <- HadesExtras::FeatureExtraction_createTemporalCovariateSettingsFromList(
        analysisIds = input$features_pickerInput |> as.integer(),
        temporalStartDays = rf_ranges()$temporalStartDays,
        temporalEndDays = rf_ranges()$temporalEndDays
      )

      analysisSettings <- list(
        cohortIds = input$selectCohorts_pickerInput |> as.integer(),
        runInclusionStatistics = input$runInclusionStatistics_switch,
        runIncludedSourceConcepts = input$runIncludedSourceConcepts_switch,
        runOrphanConcepts = input$runOrphanConcepts_switch,
        runVisitContext = input$runVisitContext_switch,
        runBreakdownIndexEvents = FALSE, # always FALSE, at the moment
        runIncidenceRate = input$runIncidenceRate_switch,
        runCohortRelationship =  FALSE,
        runTemporalCohortCharacterization = input$runTemporalCohortCharacterization_switch,
        temporalCovariateSettings = temporalCovariateSettings,
        minCellCount = input$minCellCount_numericInput
      )

      return(analysisSettings)

    })

    return(rf_analysisSettings)

  })
}




















