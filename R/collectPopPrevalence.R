# Copyright 2022 DARWIN EU®
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


#' Collect population prevalence estimates
#'
#' @param cdm CDMConnector CDM reference object
#' @param denominatorTable denominatorTable
#' @param outcomesTable Name of the table with the outcome cohorts
#' @param denominatorCohortIds Cohort ids of denominator populations
#' @param outcomeCohortIds Outcome cohort ids
#' @param type type of prevalence, point or period
#' @param points where to compute the point prevalence
#' @param interval Time intervals for prevalence estimates
#' @param fullPeriodsRequired If full period is required
#' @param minContribution Minimum proportions that
#' individuals must have to contribute
#' @param confidenceInterval Method for confidence intervals
#' @param minCellCount Minimum number of events to report- results
#' lower than this will be obscured. If NULL all results will be reported.
#' @param verbose Whether to report progress
#'
#' @return
#' @export
#'
#' @examples
collectPopPrevalence <- function(cdm,
                                 denominatorTable,
                                 outcomesTable,
                                 denominatorCohortIds = NULL,
                                 outcomeCohortIds = NULL,
                                 type = "point",
                                 interval = "months",
                                 fullPeriodsRequired = TRUE,
                                 points = "start",
                                 minContribution = 0.5,
                                 confidenceInterval = "binomial",
                                 minCellCount = 5,
                                 verbose = FALSE) {
  # help to avoid formatting errors
  if (!is.null(denominatorCohortIds) &&
    is.numeric(denominatorCohortIds)) {
    denominatorCohortIds <- as.character(denominatorCohortIds)
  }
  if (!is.null(outcomeCohortIds) &&
    is.numeric(outcomeCohortIds)) {
    outcomeCohortIds <- as.character(outcomeCohortIds)
  }
  if (is.character(type)) {
    type <- tolower(type)
  }
  if (is.character(interval)) {
    interval <- tolower(interval)
  }
  if (is.character(confidenceInterval)) {
    confidenceInterval <- tolower(confidenceInterval)
  }
  if (is.character(points)) {
    points <- tolower(points)
  }

  ## check for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()
  cdmCheck <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdmCheck,
    add = errorMessage
  )
  if (!isTRUE(cdmCheck)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::assert_character(outcomeCohortIds,
    add = errorMessage,
    null.ok = TRUE
  )
  denomCheck <- denominatorTable %in% names(cdm)
  checkmate::assertTRUE(denomCheck,
    add = errorMessage
  )
  if (!isTRUE(denomCheck)) {
    errorMessage$push(
      "- `denominatorTable` is not found in cdm"
    )
  }
  checkmate::assert_character(denominatorCohortIds,
    add = errorMessage,
    null.ok = TRUE
  )
  outcomeCheck <- outcomesTable %in% names(cdm)
  checkmate::assertTRUE(outcomeCheck,
    add = errorMessage
  )
  if (!isTRUE(outcomeCheck)) {
    errorMessage$push(
      "- `outcomesTable` is not found in cdm"
    )
  }
  checkmate::assert_character(outcomeCohortIds,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assert_choice(type,
    choices = c("point", "period"),
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(interval %in%
      c(
        "days", "weeks", "months",
        "quarters", "years"
      )),
    add = errorMessage
  )
  checkmate::assertTRUE(all(points %in% c("start", "middle", "end")),
    add = errorMessage
  )
  checkmate::assert_number(minCellCount)
  checkmate::assert_numeric(minContribution,
    add = errorMessage,
    lower = 0,
    upper = 1
  )
  checkmate::assert_logical(verbose,
    add = errorMessage
  )
  checkmate::assert_logical(fullPeriodsRequired,
    add = errorMessage
  )
  checkmate::assert_choice(confidenceInterval,
    choices = c("none", "binomial"),
    add = errorMessage,
    null.ok = TRUE
  )
  # report initial assertions
  checkmate::reportAssertions(collection = errorMessage)


  # if not given, use all denominator and outcome cohorts
  if (is.null(denominatorCohortIds)) {
    denominatorCohortIds <- cdm[[denominatorTable]] %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::pull()
  }
  if (is.null(outcomeCohortIds)) {
    outcomeCohortIds <- cdm[[outcomesTable]] %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::pull()
  }


  studySpecs <- tidyr::expand_grid(
    outcomeCohortIds = outcomeCohortIds,
    denominatorCohortIds = denominatorCohortIds,
    interval = interval,
    point = points,
    minContribution = minContribution,
    verbose = verbose
  )

  studySpecs <- studySpecs %>%
    dplyr::mutate(prevalence_analysis_id = as.character(dplyr::row_number()))

  studySpecs <- split(
    studySpecs,
    studySpecs[, c("prevalence_analysis_id")]
  )

  # get prs
  prsList <- lapply(studySpecs, function(x) {
    workingPrev <- getPopPrevalence(
      cdm = cdm,
      denominatorTable = denominatorTable,
      denominatorCohortIds = x$denominatorCohortIds,
      outcomesTable = outcomesTable,
      outcomeCohortIds = x$outcomeCohortIds,
      type = type,
      interval = x$interval,
      fullPeriodsRequired = fullPeriodsRequired,
      point = x$point,
      minContribution = x$minContribution,
      verbose = x$verbose
    )

    workingPrevPr <- workingPrev[["pr"]] %>%
      dplyr::mutate(prevalence_analysis_id = x$prevalence_analysis_id) %>%
      dplyr::relocate("prevalence_analysis_id")

    workingPrevAnalysisSettings <- workingPrev[["analysis_settings"]] %>%
      dplyr::mutate(
        outcome_cohort_ids = x$outcomeCohortIds,
        denominator_cohort_id = x$denominatorCohortIds,
        type = type,
        interval = x$interval,
        full_periods_required = fullPeriodsRequired,
        point = x$point,
        min_contribution = x$minContribution,
        confidence_interval = confidenceInterval,
        min_cell_count = minCellCount,
        prevalence_analysis_id = x$prevalence_analysis_id
      ) %>%
      dplyr::relocate("prevalence_analysis_id")

    workingPrevAttrition <- workingPrev[["attrition"]] %>%
      dplyr::mutate(prevalence_analysis_id = x$prevalence_analysis_id) %>%
      dplyr::relocate("prevalence_analysis_id")

    workingPrevPersonTable <- workingPrev[["person_table"]] %>%
      dplyr::mutate(prevalence_analysis_id = x$prevalence_analysis_id) %>%
      dplyr::relocate("prevalence_analysis_id")

    result <- list()
    result[["pr"]] <- workingPrevPr
    result[["analysis_settings"]] <- workingPrevAnalysisSettings
    result[["person_table"]] <- workingPrevPersonTable
    result[["attrition"]] <- workingPrevAttrition

    return(result)
  })

  prsList <- purrr::flatten(prsList)

  # analysis settings
  analysisSettings <- prsList[names(prsList) == "analysis_settings"]
  # to tibble
  analysisSettings <- dplyr::bind_rows(analysisSettings,
    .id = NULL
  )

  # analysis settings
  personTable <- prsList[names(prsList) == "person_table"]
  # to tibble
  personTable <- dplyr::bind_rows(personTable,
    .id = NULL
  )

  # prevalence estimates
  prs <- prsList[names(prsList) == "pr"]
  # to tibble
  prs <- dplyr::bind_rows(prs,
    .id = NULL
  )

  # get confidence intervals
  prs <- getCiPrevalence(prs, confidenceInterval) %>%
    dplyr::relocate("prev_low", .after = "prev") %>%
    dplyr::relocate("prev_high", .after = "prev_low")

  # obscure counts
  if (!is.null(minCellCount)) {
    prs <- obscureCounts(prs,
      minCellCount = minCellCount,
      substitute = NA
    )
  } else {
    # no results obscured due to a low count
    prs <- prs %>%
      dplyr::mutate(cohort_obscured = "FALSE") %>%
      dplyr::mutate(result_obscured = "FALSE")
  }

  # attrition summary
  attrition <- prsList[names(prsList) == "attrition"]
  # to tibble
  attrition <- dplyr::bind_rows(attrition,
    .id = NULL
  )

  # results to return as a list
  results <- list()
  results[["prevalence_estimates"]] <- prs
  results[["analysis_settings"]] <- analysisSettings
  results[["person_table"]] <- personTable
  results[["attrition"]] <- attrition

  return(results)
}
