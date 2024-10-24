% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_CohortDiagnostics.R
\name{execute_CohortDiagnostics}
\alias{execute_CohortDiagnostics}
\title{execute_CohortDiagnostics}
\usage{
execute_CohortDiagnostics(exportFolder, cohortTableHandler, analysisSettings)
}
\arguments{
\item{exportFolder}{A string representing the path to the folder where the results will be exported.}

\item{cohortTableHandler}{An R6 object of class \code{CohortTableHandler} containing information about the cohort tables.}

\item{analysisSettings}{A list containing analysis settings for CohortDiagnostics.}
}
\value{
A string representing the path to the exported results folder.
}
\description{
This function executes CohortDiagnostics based on the provided cohort table and analysis settings, and exports the results to a specified folder.
}
