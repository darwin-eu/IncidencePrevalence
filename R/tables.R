# Copyright 2024 DARWIN EU®
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
#
#' Format a point_prevalence object into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result object with results from
#' estimatePointPrevalence().
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param splitStrata If TRUE strata columns will be splitted.
#' @param cdmName If TRUE database names will be displayed.
#' @param outcomeName If TRUE outcome cohort names will be displayed.
#' @param outcomeSettings If TRUE settings related to the outcome cohorts will
#' be displayed.
#' @param denominatorName If TRUE denominator cohort names will be displayed.
#' @param denominatorSettings If TRUE settings related to the denominator cohorts
#' will be displayed.
#' @param analysisSettings If TRUE database names will be displayed.
#' @param groupColumn Column to use as group labels.
#' #' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param .options Named list with additional formatting options.
#' IncidencePrevalence::optionsTableIncidencePrevalence() shows allowed
#' arguments and their default values.
#'
#' @examples
#' \donttest{
#' library(IncidencePrevalence)
#'
#' cdm <- mockIncidencePrevalenceRef()
#'
#' cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
#'
#' prev <- estimatePointPrevalence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome",
#'   summarisedResult = TRUE,
#' )
#'
#' tablePointPrevalence(prev)
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
#' @return A table with a formatted version of the point prevalence result.
#'
#' @export
tablePointPrevalence <- function(result,
                                 formatEstimateName = c(
                                   "Denominator (N)" = "<denominator_count>",
                                   "Outcome (N)" = "<outcome_count>",
                                   "Prevalence [95% CI]" = "<prevalence> (<prevalence_95CI_lower> - <prevalence_95CI_upper>)"
                                 ),
                                 header = c("group", "strata"),
                                 splitStrata = TRUE,
                                 cdmName = TRUE,
                                 outcomeName = TRUE,
                                 outcomeSettings = FALSE,
                                 denominatorName = TRUE,
                                 denominatorSettings = FALSE,
                                 analysisSettings = FALSE,
                                 groupColumn = NULL,
                                 type = "gt",
                                 .options = list()) {
  tablePrevalence(
    result = result,
    formatEstimateName = formatEstimateName,
    header = header,
    splitStrata = splitStrata,
    cdmName = cdmName,
    outcomeName = outcomeName,
    outcomeSettings = outcomeSettings,
    denominatorName = denominatorName,
    denominatorSettings = denominatorSettings,
    analysisSettings = analysisSettings,
    groupColumn = groupColumn,
    type = type,
    prevalenceType = "point",
    .options = .options
  )
}

# Copyright 2024 DARWIN EU®
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
#
#' Format a period_prevalence object into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result object with results from
#' estimatePeriodPrevalence().
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param splitStrata If TRUE strata columns will be splitted.
#' @param cdmName If TRUE database names will be displayed.
#' @param outcomeName If TRUE outcome cohort names will be displayed.
#' @param outcomeSettings If TRUE settings related to the outcome cohorts will
#' be displayed.
#' @param denominatorName If TRUE denominator cohort names will be displayed.
#' @param denominatorSettings If TRUE settings related to the denominator cohorts
#' will be displayed.
#' @param analysisSettings If TRUE database names will be displayed.
#' @param groupColumn Column to use as group labels.
#' #' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param .options Named list with additional formatting options.
#' IncidencePrevalence::optionsTableIncidencePrevalence() shows allowed
#' arguments and their default values.
#'
#' @examples
#' \donttest{
#' library(IncidencePrevalence)
#'
#' cdm <- mockIncidencePrevalenceRef()
#'
#' cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
#'
#' prev <- estimatePeriodPrevalence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome",
#'   summarisedResult = TRUE,
#' )
#'
#' tablePeriodPrevalence(prev)
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
#' @return A table with a formatted version of the period prevalence result.
#'
#' @export
tablePeriodPrevalence <- function(result,
                                 formatEstimateName = c(
                                   "Denominator (N)" = "<denominator_count>",
                                   "Outcome (N)" = "<outcome_count>",
                                   "Prevalence [95% CI]" = "<prevalence> (<prevalence_95CI_lower> - <prevalence_95CI_upper>)"
                                 ),
                                 header = c("group", "strata"),
                                 splitStrata = TRUE,
                                 cdmName = TRUE,
                                 outcomeName = TRUE,
                                 outcomeSettings = FALSE,
                                 denominatorName = TRUE,
                                 denominatorSettings = FALSE,
                                 analysisSettings = FALSE,
                                 groupColumn = NULL,
                                 type = "gt",
                                 .options = list()) {
  tablePrevalence(
    result = result,
    formatEstimateName = formatEstimateName,
    header = header,
    splitStrata = splitStrata,
    cdmName = cdmName,
    outcomeName = outcomeName,
    outcomeSettings = outcomeSettings,
    denominatorName = denominatorName,
    denominatorSettings = denominatorSettings,
    analysisSettings = analysisSettings,
    groupColumn = groupColumn,
    type = type,
    prevalenceType = "period",
    .options = .options
  )
}


tablePrevalence <- function(
    result,
    formatEstimateName = c(
      "Denominator (N)" = "<denominator_count>",
      "Outcome (N)" = "<outcome_count>",
      "Prevalence [95% CI]" = "<prevalence> (<prevalence_95CI_lower> - <prevalence_95CI_upper>)"
    ),
    header = c("group", "strata"),
    splitStrata = TRUE,
    cdmName = TRUE,
    outcomeName = TRUE,
    outcomeSettings = FALSE,
    denominatorName = TRUE,
    denominatorSettings = FALSE,
    analysisSettings = FALSE,
    groupColumn = NULL,
    type = "gt",
    prevalenceType = "point",
    .options = list()
) {
  # check input
  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(.data$result_type %in% c(
      paste0(prevalenceType, "_prevalence")
    ))
  checkmate::assertList(.options)
  checkmate::assertLogical(
    c(splitStrata, cdmName, outcomeName, outcomeSettings, denominatorName,
      denominatorSettings, analysisSettings),
    any.missing = FALSE
  )

  # .options
  .options <- defaultTableIncidencePrevalence(.options)

  # prepare visOmopTable input
  ## settings
  settingsToSelect <- colnames(omopgenerics::settings(result))
  settingsToSelect <- settingsToSelect[!settingsToSelect %in% c(
    "result_type", "package_name", "package_version", "denominator_cohort_name", "outcome_cohort_name", "min_cell_count"
  )]
  if (!outcomeSettings) {
    settingsToSelect <- settingsToSelect[!grepl("outcome", settingsToSelect)]
  }
  if (!denominatorSettings) {
    settingsToSelect <- settingsToSelect[!grepl("denominator", settingsToSelect)]
  }
  if (!analysisSettings) {
    settingsToSelect <- settingsToSelect[!grepl("analysis", settingsToSelect)]
  }

  if (outcomeSettings | denominatorSettings | analysisSettings) {
    result <- result |>
      visOmopResults::splitAdditional() |>
      dplyr::left_join(
        omopgenerics::settings(result) |>
          dplyr::select(dplyr::all_of(settingsToSelect)),
        by = "result_id"
      ) |>
      visOmopResults::uniteAdditional(
        cols = c("start_date", "end_date", settingsToSelect[!settingsToSelect %in% "result_id"])
      )
  }

  ## cdm name
  if (cdmName) {
    renameColumns <- c("Database name" = "cdm_name")
    excludeColumns <- c("result_id", "estimate_type", "variable_name")
  } else {
    excludeColumns <- c("result_id", "estimate_type", "variable_name", "cdm_name")
  }
  ## outcome name
  if (outcomeName) {
    renameColumns <- c(renameColumns, "Outcome cohort name" = "variable_level")
  } else {
    excludeColumns <- c(excludeColumns, "variable_level")
  }
  ## denominator name
  if (denominatorName) {
    split <- c("group", "additional")
  } else {
    excludeColumns <- c(excludeColumns, "group_name", "group_level")
    split <- c("additional")
    if ("group" %in% header) {
      cli::cli_inform("Omiting group from header as `denominatorName = FALSE`")
      header <- header[!header %in% "group"]
    }
  }

  ## split
  if (splitStrata) {
    split <- c(split, "strata")
  }

  visOmopResults::visOmopTable(
    result = result,
    formatEstimateName = formatEstimateName,
    header = header,
    split = split,
    groupColumn = groupColumn,
    type = type,
    renameColumns = renameColumns,
    showMinCellCount = TRUE,
    excludeColumns = excludeColumns,
    .options = .options
  )

}


defaultTableIncidencePrevalence <- function(.options) {

  defaults <- visOmopResults::optionsVisOmopTable()

  for (opt in names(.options)) {
    defaults[[opt]] <- .options[[opt]]
  }

  return(defaults)
}

#' Additional arguments for the functions tablePointPrevalence,
#' tablePeriodPrevalence, and tableIncidence.
#'
#' @description
#' It provides a list of allowed inputs for .option argument in the table
#' functions of the package IncidencePrevalence, and their given default values.
#'
#'
#' @return The default .options named list.
#'
#' @export
#'
#' @examples
#' {
#' optionsTableIncidencePrevalence()
#' }
#'
optionsTableIncidencePrevalence <- function() {
  defaults <- visOmopResults::optionsVisOmopTable()
}
