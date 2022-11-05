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
  if (!is.null(denominatorCohortIds) &
      is.numeric(denominatorCohortIds)) {
    denominatorCohortIds <- as.character(denominatorCohortIds)
  }
  if (!is.null(outcomeCohortIds) &
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
  error_message <- checkmate::makeAssertCollection()
  db_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(db_inherits_check,
    add = error_message
  )
  if (!isTRUE(db_inherits_check)) {
    error_message$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::assert_character(outcomeCohortIds,
    add = error_message,
    null.ok = TRUE
  )
  denominator_check<-denominatorTable %in% names(cdm)
  checkmate::assertTRUE(denominator_check,
                        add = error_message)
  if (!isTRUE(denominator_check)) {
    error_message$push(
      "- `denominatorTable` is not found in cdm"
    )
  }
  checkmate::assert_character(denominatorCohortIds,
                              add = error_message,
                              null.ok = TRUE
  )
  outcome_check<-outcomesTable %in% names(cdm)
  checkmate::assertTRUE(outcome_check,
                        add = error_message)
  if (!isTRUE(outcome_check)) {
    error_message$push(
      "- `outcomesTable` is not found in cdm"
    )
  }
  checkmate::assert_character(outcomeCohortIds,
                              add = error_message,
                              null.ok = TRUE
  )
  checkmate::assert_choice(type,
                           choices = c("point","period"),
                           add = error_message
  )
  checkmate::assertTRUE(all(interval %in% c("days","weeks","months","quarters","years")),
                        add = error_message
  )
  checkmate::assertTRUE(all(points %in% c("start","middle","end")),
                        add = error_message
  )
  checkmate::assert_number(minCellCount)
  checkmate::assert_numeric(minContribution,
    add = error_message,
    lower = 0,
    upper = 1
  )
  checkmate::assert_logical(verbose,
    add = error_message
  )
  checkmate::assert_logical(fullPeriodsRequired,
    add = error_message
  )
  checkmate::assert_choice(confidenceInterval,
    choices = c("none", "binomial"),
    add = error_message,
    null.ok = TRUE
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)


  # if not given, use all denominator and outcome cohorts
  if(is.null(denominatorCohortIds)){
    denominatorCohortIds <-   cdm[[denominatorTable]] %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::pull()
  }
  if(is.null(outcomeCohortIds)){
    outcomeCohortIds <- cdm[[outcomesTable]] %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::pull()
  }


  study_specs <- tidyr::expand_grid(
    outcomeCohortIds = outcomeCohortIds,
    denominatorCohortIds = denominatorCohortIds,
    interval = interval,
    point = points,
    minContribution = minContribution,
    verbose = verbose
  )

  study_specs <- study_specs %>%
    dplyr::mutate(prevalence_analysis_id = as.character(dplyr::row_number()))

  study_specs <- split(
    study_specs,
    study_specs[, c("prevalence_analysis_id")]
  )

  # get prs
  prs_list <- lapply(study_specs, function(x) {

    working_prev <- getPopPrevalence(
      cdm = cdm,
      denominatorTable=denominatorTable,
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

    working_prev_pr <- working_prev[["pr"]] %>%
      dplyr::mutate(prevalence_analysis_id = x$prevalence_analysis_id) %>%
      dplyr::relocate(.data$prevalence_analysis_id)

    working_prev_analysis_settings <- working_prev[["analysis_settings"]]  %>%
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
        prevalence_analysis_id = x$prevalence_analysis_id) %>%
      dplyr::relocate(.data$prevalence_analysis_id)

    working_prev_attrition <- working_prev[["attrition"]] %>%
      dplyr::mutate(prevalence_analysis_id = x$prevalence_analysis_id ) %>%
      dplyr::relocate(.data$prevalence_analysis_id)

    working_prev_person_table <- working_prev[["person_table"]] %>%
      dplyr::mutate(prevalence_analysis_id = x$prevalence_analysis_id ) %>%
      dplyr::relocate(.data$prevalence_analysis_id)

    result <- list()
    result[["pr"]] <- working_prev_pr
    result[["analysis_settings"]] <- working_prev_analysis_settings
    result[["person_table"]] <- working_prev_person_table
    result[["attrition"]] <- working_prev_attrition

    return(result)

  })

  prs_list <- purrr::flatten(prs_list)

  # analysis settings
  analysis_settings <- prs_list[names(prs_list) == "analysis_settings"]
  # to tibble
  analysis_settings <- dplyr::bind_rows(analysis_settings,
                                        .id = NULL
  )

  # analysis settings
  person_table <- prs_list[names(prs_list) == "person_table"]
  # to tibble
  person_table <- dplyr::bind_rows(person_table,
                                   .id = NULL
  )

  # prevalence estimates
  prs <- prs_list[names(prs_list) == "pr"]
  # to tibble
  prs <- dplyr::bind_rows(prs,
                          .id = NULL
  )

  # get confidence intervals
  prs <- get_ci_prevalence(prs, confidenceInterval) %>%
    dplyr::relocate(.data$prev_low, .after = .data$prev) %>%
    dplyr::relocate(.data$prev_high, .after = .data$prev_low)

  # obscure counts
  if (!is.null(minCellCount)) {
  prs <- obscure_counts(prs,
                        minCellCount = minCellCount,
                        substitute = NA)
  } else {
    # no results obscured due to a low count
    prs <- prs %>%
      dplyr::mutate(cohort_obscured = "FALSE") %>%
      dplyr::mutate(result_obscured = "FALSE")
  }

  # attrition summary
  attrition <- prs_list[names(prs_list) == "attrition"]
  # to tibble
  attrition <- dplyr::bind_rows(attrition,
                                .id = NULL
  )

  # results to return as a list
  results <- list()
  results[["prevalence_estimates"]] <- prs
  results[["analysis_settings"]] <- analysis_settings
  results[["person_table"]] <- person_table
  results[["attrition"]] <- attrition

  return(results)

}
