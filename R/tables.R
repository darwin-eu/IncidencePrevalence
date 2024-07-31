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


#' Table of prevalence results
#'
#' @param result Prevalence results
#' @param type Type of table. Can be "gt", "flextable", or "tibble".
#' @param .options Table options to apply
#'
#' @return Table of prevalence results
#' @export
#'
#' @examples
tablePrevalence <- function(
    result,
    type = "gt",
    .options = list()
) {

  # check input
  if (!inherits(result, "summarised_result")) {
    cli::cli_abort(c("x" = "Table functionality only works with results in a summarised_result format.", "i" = "These can be obtained with the argument `summarisedResult` in estimatePeriodPrevalence() and estimatePointPrevalence()."))
  }

  formatEstimateName <- c(
    "Denominator (N)" = "<denominator_count>",
    "Outcome (N)" = "<outcome_count>",
    "Prevalence [95% CI]" = "<prevalence> (<prevalence_95CI_lower> - <prevalence_95CI_upper>)"
  )
  header <- c("variable", "estimate")
  splitStrata <- TRUE
  cdmName <- TRUE
  outcomeName <- TRUE
  outcomeSettings <- FALSE
  denominatorName <- TRUE
  denominatorSettings <- TRUE
  analysisSettings <- FALSE
  groupColumn <- NULL

  tableInternal(
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
    resultType = c("point_prevalence", "period_prevalence"),
    .options = .options
  )

}


#' Table of incidence results
#'
#' @param result Incidence results
#' @param type Type of table. Can be "gt", "flextable", or "tibble".
#' @param .options Table options to apply
#'
#' @return Table of results
#' @export
#'
tableIncidence <- function(
    result,
    type = "gt",
    .options = list()
) {

  # check input
  if (!inherits(result, "summarised_result")) {
    cli::cli_abort(c("x" = "Table functionality only works with results in a summarised_result format.", "i" = "These can be obtained with the argument `summarisedResult` in estimateIncidence()."))
  }

  header <- c("variable", "estimate")
  splitStrata <- TRUE
  cdmName <- TRUE
  outcomeName <- TRUE
  outcomeSettings <- FALSE
  denominatorName <- TRUE
  denominatorSettings <- TRUE
  analysisSettings <- FALSE
  groupColumn <- NULL

  tableInternal(
    result = result,
    formatEstimateName = c(
      "Denominator (N)" = "<denominator_count>",
      "Person-years" = "<person_years>",
      "Outcome (N)" = "<outcome_count>",
      "Incidence 100 person-years [95% CI]" =
        "<incidence_100000_pys> (<incidence_100000_pys_95CI_lower> -
      <incidence_100000_pys_95CI_upper>)"
    ),
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
    resultType = "incidence",
    .options = .options
  )

}

tableInternal <- function(
    result,
    formatEstimateName = c(
      "Denominator (N)" = "<denominator_count>",
      "Person-years" = "<person_years>",
      "Outcome (N)" = "<outcome_count>",
      "Incidence per 100,000 person-years [95% CI]" = "<incidence_100000_pys> (<incidence_100000_pys_95CI_lower> - <incidence_100000_pys_95CI_upper>)"
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
    resultType = "incidence",
    .options = list()
) {
  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(.data$result_type %in% resultType)
  if (nrow(result) == 0) {
    cli::cli_abort("No results of the type {resultType} were found in the summarised result provided.")
  }
  checkmate::assertList(.options)
  checkmate::assertLogical(
    c(splitStrata, cdmName, outcomeName, outcomeSettings, denominatorName,
      denominatorSettings, analysisSettings),
    any.missing = FALSE
  )

  # .options
  .options <- defaultTableIncidencePrevalence(.options, resultType)

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
      )
    if("incidence_start_date" %in% colnames(result)){
      result <- result |>
        visOmopResults::uniteAdditional(
          cols = c("incidence_start_date", "incidence_end_date", settingsToSelect[!settingsToSelect %in% "result_id"])
        )
    } else {
      result <- result |>
        visOmopResults::uniteAdditional(
          cols = c("prevalence_start_date", "prevalence_end_date", settingsToSelect[!settingsToSelect %in% "result_id"])
        )
    }
  }

  ## cdm name
  if (cdmName) {
    renameColumns <- c("Database name" = "cdm_name")
    excludeColumns <- c("result_id", "estimate_type")
  } else {
    excludeColumns <- c("result_id", "estimate_type", "cdm_name")
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
  if (!"variable" %in% header) {
    excludeColumns <- c(excludeColumns, "variable_name")
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

defaultTableIncidencePrevalence <- function(.options, type) {

  defaults <- visOmopResults::optionsVisOmopTable()

  if ("incidence" %in% type) {
    defaults$keepNotFormatted = FALSE
  }

  for (opt in names(.options)) {
    defaults[[opt]] <- .options[[opt]]
  }

  return(defaults)
}

#' Additional arguments for the functions tablePrevalence.
#'
#' @description
#' It provides a list of allowed inputs for .option argument in tablePrevalence,
#' and their given default values.
#'
#'
#' @return The default .options named list.
#'
#' @export
#'
#' @examples
#' {
#' optionsTablePrevalence()
#' }
#'
optionsTablePrevalence <- function() {
  defaultTableIncidencePrevalence(NULL, type = "prevalence")
}

#' Additional arguments for the functions tableIncidence.
#'
#' @description
#' It provides a list of allowed inputs for .option argument in tableIncidence,
#' and their given default values.
#'
#'
#' @return The default .options named list.
#'
#' @export
#'
#' @examples
#' {
#' optionsTableIncidence()
#' }
#'
optionsTableIncidence <- function() {
  defaultTableIncidencePrevalence(NULL, "incidence")
}
