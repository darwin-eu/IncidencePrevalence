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


#' Collect population incidence estimates
#'
#' @param cdm CDMConnector CDM reference
#' @param denominatorTable Name of the table with the denominator cohorts
#' @param outcomesTable Name of the table with the outcome cohorts.
#' Can be "days", "weeks", "months", "quarters", or "years".
#' @param denominatorCohortIds Cohort ids of denominator populations
#' @param outcomeCohortIds Outcome cohort ids
#' @param interval Time intervals for incidence estimates
#' @param fullPeriodsRequired If full period is required
#' @param outcomeWashout Clean windows
#' @param repeatedEvents Repeated events
#' @param confidenceInterval Method for confidence intervals
#' @param minCellCount Minimum number of events to report- results
#' lower than this will be obscured. If NULL all results will be reported.
#' @param verbose Whether to report progress
#'
#' @return
#' @export
#'
#' @examples
collectPopIncidence <- function(cdm,
                                denominatorTable,
                                outcomesTable,
                                denominatorCohortIds = NULL,
                                outcomeCohortIds = NULL,
                                interval = "months",
                                fullPeriodsRequired = TRUE,
                                outcomeWashout = 0,
                                repeatedEvents = FALSE,
                                confidenceInterval = "poisson",
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
  if (is.character(interval)) {
    interval <- tolower(interval)
  }
  if (is.character(confidenceInterval)) {
    confidenceInterval <- tolower(confidenceInterval)
  }


  ## check for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()
  cdmCheck <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdmCheck,
    add = errorMessage
  )
  if (!isTRUE(cdmCheck)) {
    "- cdm must be a CDMConnector CDM reference object"
  }
  denominatorCheck <- denominatorTable %in% names(cdm)
  checkmate::assertTRUE(denominatorCheck,
    add = errorMessage
  )
  if (!isTRUE(denominatorCheck)) {
    errorMessage$push(
      "- `denominatorTable` is not found in cdm"
    )
  }
  checkmate::assertTRUE(all(c(
    "cohort_definition_id",
    "subject_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %in%
    names(cdm[[denominatorTable]] %>%
      utils::head(1) %>%
      dplyr::collect())))
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
  checkmate::assert_choice(interval,
    choices = c("days", "weeks", "months", "quarters", "years"),
    add = errorMessage
  )
  checkmate::assert_logical(fullPeriodsRequired,
    add = errorMessage
  )
  checkmate::assert_numeric(outcomeWashout,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assert_logical(repeatedEvents,
    add = errorMessage
  )
  checkmate::assert_choice(confidenceInterval,
    choices = c("none", "poisson"),
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assert_number(minCellCount)
  checkmate::assert_logical(verbose,
    add = errorMessage
  )
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

  # further checks that there are the required data elements
  errorMessage <- checkmate::makeAssertCollection()
  dateCheck <- nrow(cdm[[denominatorTable]] %>%
    dplyr::select("cohort_start_date", "cohort_end_date") %>%
    dplyr::filter(.data$cohort_start_date > .data$cohort_end_date) %>%
    dplyr::collect()) == 0
  checkmate::assertTRUE(dateCheck)
  if (!isTRUE(dateCheck)) {
    errorMessage$push(
      "- some end dates before start dates in denominator"
    )
  }
  denomCountCheck <- cdm[[denominatorTable]] %>%
    dplyr::filter(.data$cohort_definition_id %in%
      .env$denominatorCohortIds) %>%
    dplyr::count() %>%
    dplyr::pull() > 0
  checkmate::assertTRUE(denomCountCheck,
    add = errorMessage
  )
  if (!isTRUE(denomCountCheck)) {
    errorMessage$push(
  "- nobody found in `denominatorTable` with one of the `denominatorCohortIds`"
    )
  }
  outcomeCountCheck <- cdm[[outcomesTable]] %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$outcomeCohortIds) %>%
    dplyr::count() %>%
    dplyr::pull() > 0
  checkmate::assertTRUE(outcomeCountCheck,
    add = errorMessage
  )
  if (!isTRUE(outcomeCountCheck)) {
    errorMessage$push(
      "- nobody found in `outcomesTable` with one of the `outcomeCohortIds`"
    )
  }

  missingCheck <- nrow(cdm[[denominatorTable]] %>%
    dplyr::filter(is.na(.data$cohort_definition_id) | is.na(.data$subject_id) |
      is.na(.data$cohort_start_date) | is.na(.data$cohort_end_date)) %>%
    dplyr::collect()) == 0
  if (!isTRUE(missingCheck)) {
    errorMessage$push(
      "- there is missing data in `denominatorTable`"
    )
  }
  checkmate::reportAssertions(collection = errorMessage)


  # get outcomes + cohort_start_date & cohort_end_date
  outcome <- cdm[[outcomesTable]] %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$outcomeCohortIds) %>%
    dplyr::rename("outcome_id" = "cohort_definition_id") %>%
    dplyr::rename("outcome_start_date" = "cohort_start_date") %>%
    dplyr::rename("outcome_end_date" = "cohort_end_date") %>%
    dplyr::inner_join(
      cdm[[denominatorTable]] %>%
        dplyr::filter(.data$cohort_definition_id %in%
                        .env$denominatorCohortIds) %>%
        dplyr::select(-"cohort_definition_id") %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::compute()

  # get only the outcomes that will affect the incidence calculation
  outcome <- outcome %>%
    # most recent outcome starting before cohort start per person
    dplyr::filter(.data$outcome_start_date < .data$cohort_start_date) %>%
    dplyr::group_by(.data$subject_id,
                    .data$cohort_start_date,
                    .data$outcome_id) %>%
    dplyr::filter(.data$outcome_start_date ==
                    max(.data$outcome_start_date, na.rm = TRUE)) %>%
    dplyr::union_all(
      # all starting during cohort period
      outcome %>%
        dplyr::filter(.data$outcome_start_date >= .data$cohort_start_date) %>%
        dplyr::filter(.data$outcome_start_date <= .data$cohort_end_date)
    ) %>%
    dplyr::compute()

  outcome <- outcome %>%
    dplyr::group_by(
      .data$subject_id,
      .data$cohort_start_date,
      .data$outcome_id
    ) %>%
    dbplyr::window_order(.data$outcome_start_date) %>%
    dplyr::mutate(index = rank()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  cdm[[outcomesTable]] <- outcome %>%
    dplyr::select(-"outcome_end_date") %>%
    dplyr::full_join(
      outcome %>%
        dplyr::mutate(index = .data$index + 1) %>%
        dplyr::rename("outcome_prev_end_date" = "outcome_end_date") %>%
        dplyr::select(-"outcome_start_date"),
      by = c("subject_id", "cohort_start_date",
             "cohort_end_date", "outcome_id", "index")) %>%
    dplyr::select(-"index") %>%
    dplyr::compute()

  studySpecs <- tidyr::expand_grid(
    outcome_cohort_ids = outcomeCohortIds,
    denominator_cohort_ids = denominatorCohortIds,
    interval = interval,
    full_periods_required = fullPeriodsRequired,
    outcome_washout = outcomeWashout,
    repeated_events = repeatedEvents,
    confidence_interval = confidenceInterval,
    verbose = verbose
  )

  if (is.null(outcomeWashout)) {
    studySpecs$outcome_washout <- NA
  }

  studySpecs <- studySpecs %>%
    dplyr::mutate(incidence_analysis_id = as.character(dplyr::row_number()))

  studySpecs <- split(
    studySpecs,
    studySpecs[, c("incidence_analysis_id")]
  )

  # get irs
  irsList <- lapply(studySpecs, function(x) {
    workingInc <- getPopIncidence(
      cdm = cdm,
      denominatorTable = denominatorTable,
      denominatorCohortIds = x$denominator_cohort_ids,
      outcomesTable = outcomesTable,
      outcomeCohortIds = x$outcome_cohort_ids,
      interval = x$interval,
      fullPeriodsRequired = x$full_periods_required,
      outcomeWashout = x$outcome_washout,
      repeatedEvents = x$repeated_events,
      verbose = x$verbose
    )

    workingIncIr <- workingInc[["ir"]] %>%
      dplyr::mutate(incidence_analysis_id = x$incidence_analysis_id) %>%
      dplyr::relocate("incidence_analysis_id")

    workingIncPersonTable <- workingInc[["person_table"]] %>%
      dplyr::mutate(incidence_analysis_id = x$incidence_analysis_id) %>%
      dplyr::relocate("incidence_analysis_id")

    workingIncAnalysisSettings <- workingInc[["analysis_settings"]] %>%
      dplyr::mutate(
        outcome_cohort_ids = x$outcome_cohort_ids,
        denominator_cohort_ids = x$denominator_cohort_ids,
        interval = x$interval,
        outcome_washout = x$outcome_washout,
        repeated_events = x$repeated_events,
        confidence_interval = .env$confidenceInterval,
        min_cell_count = .env$minCellCount,
        incidence_analysis_id = x$incidence_analysis_id
      ) %>%
      dplyr::relocate("incidence_analysis_id")


    workingIncAttrition <- workingInc[["attrition"]] %>%
      dplyr::mutate(incidence_analysis_id = x$incidence_analysis_id) %>%
      dplyr::relocate("incidence_analysis_id")

    result <- list()
    result[["ir"]] <- workingIncIr
    result[["analysis_settings"]] <- workingIncAnalysisSettings
    result[["person_table"]] <- workingIncPersonTable
    result[["attrition"]] <- workingIncAttrition

    return(result)
  })


  irsList <- purrr::flatten(irsList)

  # analysis settings
  analysisSettings <- irsList[names(irsList) == "analysis_settings"]
  # to tibble
  analysisSettings <- dplyr::bind_rows(analysisSettings,
    .id = NULL
  )

  # incidence estimates
  irs <- irsList[names(irsList) == "ir"]
  # to tibble
  irs <- dplyr::bind_rows(irs,
    .id = NULL
  )

  # get confidence intervals
  irs <- getCiIncidence(irs, confidenceInterval) %>%
    dplyr::relocate("ir_100000_pys_low", .after = "ir_100000_pys") %>%
    dplyr::relocate("ir_100000_pys_high", .after = "ir_100000_pys_low")

  # obscure counts
  if (!is.null(minCellCount)) {
    irs <- obscureCounts(irs, minCellCount = minCellCount, substitute = NA)
  } else {
    # no results obscured due to a low count
    irs <- irs %>%
      dplyr::mutate(cohort_obscured = "FALSE") %>%
      dplyr::mutate(result_obscured = "FALSE")
  }

  # person_table summary
  personTable <- irsList[names(irsList) == "person_table"]
  # to tibble
  personTable <- dplyr::bind_rows(personTable,
    .id = NULL
  )

  # attrition summary
  attrition <- irsList[names(irsList) == "attrition"]
  # to tibble
  attrition <- dplyr::bind_rows(attrition,
    .id = NULL
  )

  # results to return as a list
  results <- list()
  results[["incidence_estimates"]] <- irs
  results[["analysis_settings"]] <- analysisSettings
  results[["person_table"]] <- personTable
  results[["attrition"]] <- attrition

  return(results)
}
