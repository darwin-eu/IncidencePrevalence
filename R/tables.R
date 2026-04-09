# Copyright 2025 DARWIN EU®
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

#' Table of prevalence results
#'
#' @param result Prevalence results
#' @param type Type of table. Can be "gt", "flextable", or "tibble"
#' @param header A vector specifying the elements to include in the header. The
#' order of elements matters, with the first being the topmost header.
#' The header vector can contain one of the following variables: "cdm_name",
#' "denominator_cohort_name", "outcome_cohort_name", "prevalence_start_date",
#' "prevalence_end_date", "estimate_name", variables in the `strata_name` column,
#' and any of the settings columns specified in `settingsColumn` argument.
#' The header can also include other names to use as overall header labels
#' @param groupColumn Variables to use as group labels. Allowed columns are the
#' same as in `header`
#' @param settingsColumn Variables from the settings attribute to display in
#' the table
#' @param hide  Table columns to exclude, options are the ones described in
#' `header`
#' @param style A style supported by visOmopResults::visOmopTable()
#' @param .options Table options to apply
#'
#' @return Table of prevalence results
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm,
#'   name = "denominator",
#'   cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
#' )
#' prev <- estimatePointPrevalence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome",
#'   interval = "months"
#' )
#' tablePrevalence(prev)
#' }
tablePrevalence <- function(result,
                            type = "gt",
                            header = c("estimate_name"),
                            groupColumn = c("cdm_name", "outcome_cohort_name"),
                            settingsColumn = c("denominator_age_group", "denominator_sex"),
                            hide = c("denominator_cohort_name", "analysis_interval"),
                            style = "default",
                            .options = list()) {
  rlang::check_installed("visOmopResults", version = "1.0.2")

  formatEstimateName <- c(
    "Denominator (N)" = "<denominator_count>",
    "Outcome (N)" = "<outcome_count>",
    "Prevalence [95% CI]" = "<prevalence> (<prevalence_95CI_lower> - <prevalence_95CI_upper>)"
  )

  tableInternal(
    result = result,
    formatEstimateName = formatEstimateName,
    header = header,
    groupColumn = groupColumn,
    settingsColumn = settingsColumn,
    type = type,
    hide = hide,
    resultType = "prevalence",
    style = style,
    .options = .options
  )
}


#' Table of incidence results
#'
#' @param result Incidence results
#' @param type Type of table. Can be "gt", "flextable", or "tibble"
#' @param header A vector specifying the elements to include in the header. The
#' order of elements matters, with the first being the topmost header.
#' The header vector can contain one of the following variables: "cdm_name",
#' "denominator_cohort_name", "outcome_cohort_name", "incidence_start_date",
#' "incidence_end_date", "estimate_name", variables in the `strata_name` column,
#' and any of the settings columns specified in `settingsColumn` argument.
#' The header can also include other names to use as overall header labels
#' @param groupColumn Variables to use as group labels. Allowed columns are the
#' same as in `header`
#' @param settingsColumn Variables from the settings attribute to display in
#' the table
#' @param hide  Table columns to exclude, options are the ones described in
#' `header`
#' @param style A style supported by visOmopResults::visOmopTable()
#' @param .options Table options to apply
#'
#' @return Table of results
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
#' )
#' inc <- estimateIncidence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' tableIncidence(inc)
#' }
tableIncidence <- function(result,
                           type = "gt",
                           header = c("estimate_name"),
                           groupColumn = c("cdm_name", "outcome_cohort_name"),
                           settingsColumn = c("denominator_age_group", "denominator_sex"),
                           hide = c("denominator_cohort_name", "analysis_interval"),
                           style = "default",
                           .options = list()) {
  rlang::check_installed("visOmopResults", version = "1.0.2")

  # dynamically create formatEstimateName
  estimates <- unique(result$estimate_name)
  inc_est <- estimates[grepl("incidence_\\d+_pys$", estimates)]
  if (length(inc_est) > 0) {
    denom <- sub("incidence_(\\d+)_pys", "\\1", inc_est[1])
    formatEstimateName <- c(
      "Denominator (N)" = "<denominator_count>",
      "Person-years" = "<person_years>",
      "Outcome (N)" = "<outcome_count>",
      setNames(
        paste0("<incidence_", denom, "_pys> (<incidence_", denom, "_pys_95CI_lower> - <incidence_", denom, "_pys_95CI_upper>)"),
        paste0("Incidence ", format(as.numeric(denom), big.mark=",", scientific = FALSE), " person-years [95% CI]")
      )
    )
  } else {
    formatEstimateName <- c(
      "Denominator (N)" = "<denominator_count>",
      "Person-years" = "<person_years>",
      "Outcome (N)" = "<outcome_count>"
    )
  }

  tableInternal(
    result = result,
    formatEstimateName = formatEstimateName,
    header = header,
    groupColumn = groupColumn,
    type = type,
    hide = hide,
    settingsColumn = settingsColumn,
    resultType = "incidence",
    style = style,
    .options = .options
  )
}

tableInternal <- function(
    result,
    formatEstimateName = c(
      "(N)" = "<denominator_count>",
      "Person-years" = "<person_years>",
      "Outcome (N)" = "<outcome_count>",
      "Incidence per 100,000 person-years [95% CI]" = "<incidence_100000_pys> (<incidence_100000_pys_95CI_lower> - <incidence_100000_pys_95CI_upper>)",
      "Incidence proportion [95% CI]" = "<incidence_proportion> (<incidence_proportion_95CI_lower> - <incidence_proportion_95CI_upper>)"
    ),
    header = c("group", "strata"),
    type = "gt",
    resultType = "incidence",
    groupColumn = character(),
    settingsColumn = character(),
    hide = character(),
    style = style,
    .options = list()) {
  result <- omopgenerics::newSummarisedResult(result) |>
    omopgenerics::filterSettings(.data$result_type == resultType)
  if (nrow(result) == 0) {
    cli::cli_warn("No results of the type {resultType} were found in the summarised result provided.")
    return(visOmopResults::visOmopTable(result = result, type = type, style = style))
  }
  omopgenerics::assertList(.options)
  omopgenerics::assertCharacter(header, null = TRUE)
  omopgenerics::assertCharacter(resultType)
  omopgenerics::assertCharacter(hide, null = TRUE)
  omopgenerics::assertChoice(type, visOmopResults::tableType())
  if (!is.list(groupColumn)) groupColumn <- list(groupColumn)
  omopgenerics::assertList(groupColumn, null = TRUE)

  # .options
  .options <- defaultTableIncidencePrevalence(.options, resultType)

  # fix for visOmopTable
  hide <- c(hide, "variable_name", "variable_level")
  settingsColumn <- settingsColumn[settingsColumn != "outcome_cohort_name"]

  # visOmopTable
  visOmopResults::visOmopTable(
    result = result,
    estimateName = formatEstimateName,
    header = header,
    groupColumn = groupColumn,
    settingsColumn = settingsColumn,
    type = type,
    rename = c("Database name" = "cdm_name"),
    hide = hide,
    style = style,
    .options = .options
  )
}

defaultTableIncidencePrevalence <- function(.options, type) {
  defaults <- visOmopResults::tableOptions()

  if ("incidence" %in% type) {
    defaults$keepNotFormatted <- FALSE
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
#'   optionsTablePrevalence()
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
#'   optionsTableIncidence()
#' }
#'
optionsTableIncidence <- function() {
  defaultTableIncidencePrevalence(NULL, "incidence")
}

#' Table of incidence attrition results
#'
#' @param result A summarised_result object. Output of
#' summariseCohortAttrition().
#' @param type Type of table. Check supported types with
#' `visOmopResults::tableType()`.
#' @param header Columns to use as header. See options with
#' `colnames(omopgenerics::splitAll(result))`. Variables in `settingsColumn`
#' are also allowed
#' @param groupColumn Variables to use as group labels. Allowed columns are the
#' same as in `header`
#' @param settingsColumn Variables from the settings attribute to display in
#' the table
#' @param hide  Table columns to exclude, options are the ones described in
#' `header`
#' @param style A style supported by visOmopResults::visOmopTable()
#'
#' @return A visual table.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
#' )
#' inc <- estimateIncidence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' tableIncidenceAttrition(inc)
#' }
tableIncidenceAttrition <- function(result,
                                    type = "gt",
                                    header = "variable_name",
                                    groupColumn = c("cdm_name", "outcome_cohort_name"),
                                    settingsColumn = NULL,
                                    hide = c("denominator_cohort_name", "estimate_name", "reason_id", "variable_level"),
                                    style = "default") {
  # initial checks
  result <- omopgenerics::validateResultArgument(result)
  omopgenerics::assertChoice(type, c("gt", "flextable", "tibble"))

  # check settings
  result <- result |>
    omopgenerics::filterSettings(
      .data$result_type == "incidence_attrition"
    )

  if (nrow(result) == 0) {
    cli::cli_warn("`result` object does not contain any `result_type == 'incidence_attrition'` information.")
    return(emptyResultTable(type))
  }

  result <- result |>
    dplyr::mutate(variable_name = stringr::str_to_sentence(gsub("_", " ", .data$variable_name)))

  # format table
  tab <- visOmopResults::visOmopTable(
    result = result,
    estimateName = c("N" = "<count>"),
    header = header,
    groupColumn = groupColumn,
    settingsColumn = settingsColumn,
    type = type,
    hide = hide,
    style = style
  )

  return(tab)
}


#' Table of prevalence attrition results
#'
#' @param result A summarised_result object. Output of
#' summariseCohortAttrition().
#' @param type Type of table. Check supported types with
#' `visOmopResults::tableType()`.
#' @param header Columns to use as header. See options with
#' `colnames(omopgenerics::splitAll(result))`. Variables in `settingsColumn`
#' are also allowed
#' @param groupColumn Variables to use as group labels. Allowed columns are the
#' same as in `header`
#' @param settingsColumn Variables from the settings attribute to display in
#' the table
#' @param hide  Table columns to exclude, options are the ones described in
#' `header`
#' @param style A style supported by visOmopResults::visOmopTable()
#'
#' @return A visual table.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
#' )
#' prev <- estimatePointPrevalence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome",
#'   interval = "months"
#' )
#' tablePrevalenceAttrition(prev)
#' }
tablePrevalenceAttrition <- function(result,
                                     type = "gt",
                                     header = "variable_name",
                                     groupColumn = c("cdm_name", "outcome_cohort_name"),
                                     settingsColumn = NULL,
                                     hide = c("denominator_cohort_name", "estimate_name", "reason_id", "variable_level"),
                                     style = "default") {
  # initial checks
  result <- omopgenerics::validateResultArgument(result)
  omopgenerics::assertChoice(type, visOmopResults::tableType())

  # check settings
  result <- result |>
    omopgenerics::filterSettings(
      .data$result_type == "prevalence_attrition"
    )

  if (nrow(result) == 0) {
    cli::cli_warn("`result` object does not contain any `result_type == 'incidence_attrition'` information.")
    return(emptyResultTable(type))
  }

  result <- result |>
    dplyr::mutate(variable_name = stringr::str_to_sentence(gsub("_", " ", .data$variable_name)))

  # format table
  tab <- visOmopResults::visOmopTable(
    result = result,
    estimateName = c("N" = "<count>"),
    header = header,
    groupColumn = groupColumn,
    settingsColumn = settingsColumn,
    type = type,
    hide = hide,
    style = style
  )

  return(tab)
}


emptyResultTable <- function(type) {
  x <- dplyr::tibble(`Table has no data` = character())
  if (type == "gt") {
    result <- gt::gt(x)
  } else if (type == "flextable") {
    result <- flextable::flextable(x)
  } else {
    result <- x
  }
  result
}
