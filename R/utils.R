# Copyright 2023 DARWIN EU®
#
# This file is part of IncidencePrevalence
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Cohort attrition
#'
#' @param result Result for which to get attrition
#' @param ... Additional arguments. For denominator cohort, cohortDefinitionId
#' can be used to specify a particular cohort of interest. For incidence and
#' prevalence analyses results, analysisId can be used to specify a
#' particular analysis of interest.
#'
#' @return tibble with counts and reasons for attrition.
#' @export
#'
#' @examples
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 200)
#' cdm$denominator <- generateDenominatorCohortSet(cdm,
#'   ageGroup = list(c(18, 40)),
#'   daysPriorHistory = 120
#' )
#' attrition(cdm$denominator)
attrition <- function(result, ...) {
  UseMethod("attrition")
}

#' @export
attrition.IncidencePrevalenceDenominator <- function(result,
                                                     cohortDefinitionId = NULL, ...) {
  attrition <- attr(result, "attrition")
  if (!is.null(cohortDefinitionId)) {
    attrition <- attrition %>%
      dplyr::filter(.data$cohort_definition_id == .env$cohortDefinitionId)
  }
  return(attrition)
}

#' @export
attrition.IncidencePrevalenceResult <- function(result,
                                                analysisId = NULL, ...) {
  attrition <- attr(result, "attrition")
  if (!is.null(analysisId)) {
    attrition <- attrition %>%
      dplyr::filter(.data$analysis_id == .env$analysisId)
  }
  return(attrition)
}


#' Settings associated with a cohort set
#'
#' @param result Result for which to get settings
#' @param ... Additional arguments. For denominator cohort, cohortDefinitionId
#' can be used to specify a particular cohort of interest. For incidence and
#' prevalence analyses results, analysisId can be used to specify a
#' particular analysis of interest.
#'
#' @return tibble with settings used when generating the cohort set
#' @export
#'
#' @examples
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 200)
#' cdm$denominator <- generateDenominatorCohortSet(cdm,
#'   startDate = NULL,
#'   endDate = NULL,
#'   ageGroup = list(c(18, 40)),
#'   sex = c("Female", "Male"),
#'   daysPriorHistory = 120
#' )
#' settings(result = cdm$denominator)
#' settings(result = cdm$denominator, cohortDefinitionId = 1)
#' incidence <- estimateIncidence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome",
#'   interval = "overall"
#' )
#' settings(result = incidence)
#' settings(result = incidence, analysisId = 1)
settings <- function(result, ...) {
  UseMethod("settings")
}

#' @export
settings.IncidencePrevalenceDenominator <- function(result,
                                                    cohortDefinitionId = NULL, ...) {
  settings <- attr(result, "settings")
  if (!is.null(cohortDefinitionId)) {
    settings <- settings %>%
      dplyr::filter(.data$cohort_definition_id == .env$cohortDefinitionId)
  }
  return(settings)
}

#' @export
settings.IncidencePrevalenceResult <- function(result,
                                               analysisId = NULL, ...) {
  settings <- attr(result, "settings")
  if (!is.null(analysisId)) {
    settings <- settings %>%
      dplyr::filter(.data$analysis_id == .env$analysisId)
  }
  return(settings)
}

#' Counts of cohorts in a cohort set
#'
#' @param cohortTable Set of cohorts
#' @param cohortDefinitionId cohortDefinitionId can be used to
#' specify a particular cohort of interest
#'
#' @return tibble with settings used when generating the cohort set
#' @export
#'
#' @examples
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 200)
#' cdm$denominator <- generateDenominatorCohortSet(cdm,
#'   startDate = NULL,
#'   endDate = NULL,
#'   ageGroup = list(c(18, 40)),
#'   sex = c("Female", "Male"),
#'   daysPriorHistory = 120
#' )
#' cohortCount(cohortTable = cdm$denominator)
#' cohortCount(cohortTable = cdm$denominator, cohortDefinitionId = 1)
cohortCount <- function(cohortTable, cohortDefinitionId = NULL) {
  UseMethod("cohortCount")
}

#' @export
cohortCount.IncidencePrevalenceDenominator <- function(cohortTable,
                                                       cohortDefinitionId = NULL) {
  cohortCount <- attr(cohortTable, "cohortCount")
  if (!is.null(cohortDefinitionId)) {
    cohortCount <- cohortCount %>%
      dplyr::filter(.data$cohort_definition_id == .env$cohortDefinitionId)
  }
  return(cohortCount)
}

#'  Participants contributing to an analysis
#'
#' @param result Result object
#' @param analysisId ID of a specific analysis to return participants for
#'
#' @return References to tables with the study participants contributing to
#' a given analysis
#' @export
#'
#' @examples
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 200)
#' cdm$denominator <- generateDenominatorCohortSet(cdm,
#'   startDate = NULL,
#'   endDate = NULL,
#'   ageGroup = list(c(18, 40)),
#'   sex = c("Female", "Male"),
#'   daysPriorHistory = 120
#' )
#' incidence <- estimateIncidence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome",
#'   interval = "overall"
#' )
#' participants(result = incidence)
#' participants(result = incidence, analysisId = 1)
participants <- function(result, analysisId = NULL) {
  UseMethod("participants")
}

#' @export
participants.IncidencePrevalenceResult <- function(result,
                                                   analysisId = NULL) {
  included <- attr(result, "participants")
  if (!is.null(analysisId)) {
    included <- included[[paste0("study_population_analyis_", analysisId)]]
  }
  return(included)
}
