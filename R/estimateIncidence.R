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
#' @param outcomeTable Name of the table with the outcome cohorts.
#' Can be "weeks", "months", "quarters", or "years".
#' @param denominatorCohortId Cohort ids of denominator populations
#' @param outcomeCohortId Outcome cohort ids
#' @param interval Time intervals for incidence estimates
#' @param fullPeriods If full period is required
#' @param outcomeWashout Clean windows
#' @param repeatedEvents Repeated events
#' @param confidenceInterval Method for confidence intervals
#' @param returnAnalysisCohort Whether to return population
#' @param minCellCount Minimum number of events to report- results
#' lower than this will be obscured. If NULL all results will be reported.
#' @param verbose Whether to report progress
#'
#' @return
#' @export
#'
#' @examples
estimateIncidence <- function(cdm,
                              denominatorTable,
                              outcomeTable,
                              denominatorCohortId = NULL,
                              outcomeCohortId = NULL,
                              interval = "months",
                              fullPeriods = TRUE,
                              outcomeWashout = 0,
                              repeatedEvents = FALSE,
                              confidenceInterval = "poisson",
                              returnAnalysisCohort = TRUE,
                              minCellCount = 5,
                              verbose = FALSE) {
  if (verbose == TRUE) {
    startCollect <- Sys.time()
    message("Progress: Checking inputs")
  }
  # help to avoid formatting errors
  if (!is.null(denominatorCohortId) &&
    is.numeric(denominatorCohortId)) {
    denominatorCohortId <- as.character(denominatorCohortId)
  }
  if (!is.null(outcomeCohortId) &&
    is.numeric(outcomeCohortId)) {
    outcomeCohortId <- as.character(outcomeCohortId)
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
  checkmate::assert_character(denominatorCohortId,
    add = errorMessage,
    null.ok = TRUE
  )
  outcomeCheck <- outcomeTable %in% names(cdm)
  checkmate::assertTRUE(outcomeCheck,
    add = errorMessage
  )
  if (!isTRUE(outcomeCheck)) {
    errorMessage$push(
      "- `outcomeTable` is not found in cdm"
    )
  }
  checkmate::assert_character(outcomeCohortId,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assert_choice(interval,
    choices = c(
      "weeks", "months", "quarters", "years",
      "overall"
    ),
    add = errorMessage
  )
  checkmate::assert_logical(fullPeriods,
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
  if (is.null(denominatorCohortId)) {
    denominatorCohortId <- cdm[[denominatorTable]] %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::pull()
  }
  if (is.null(outcomeCohortId)) {
    outcomeCohortId <- cdm[[outcomeTable]] %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::pull()
  }

  # further checks that there are the required data elements
  errorMessage <- checkmate::makeAssertCollection()
  denomCountCheck <- cdm[[denominatorTable]] %>%
    dplyr::filter(.data$cohort_definition_id %in%
      .env$denominatorCohortId) %>%
    dplyr::count() %>%
    dplyr::pull() > 0
  checkmate::assertTRUE(denomCountCheck,
    add = errorMessage
  )
  if (!isTRUE(denomCountCheck)) {
    errorMessage$push(
      "- nobody found in `denominatorTable` with one of the `denominatorCohortId`"
    )
  }
  outcomeCountCheck <- cdm[[outcomeTable]] %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$outcomeCohortId) %>%
    dplyr::count() %>%
    dplyr::pull() > 0
  checkmate::assertTRUE(outcomeCountCheck,
    add = errorMessage
  )
  if (!isTRUE(outcomeCountCheck)) {
    errorMessage$push(
      "- nobody found in `outcomeTable` with one of the `outcomeCohortId`"
    )
  }
  checkmate::reportAssertions(collection = errorMessage)
  if (verbose == TRUE) {
    message("Progress: All input checks passed")
    duration <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
    message(glue::glue(
      "Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }

  # get outcomes + cohort_start_date & cohort_end_date
  if (verbose == TRUE) {
    message("Progress: limit to relevant outcomes")
    start <- Sys.time()
  }
  outcome <- cdm[[outcomeTable]] %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$outcomeCohortId) %>%
    dplyr::rename("outcome_cohort_id" = "cohort_definition_id") %>%
    dplyr::rename("outcome_start_date" = "cohort_start_date") %>%
    dplyr::rename("outcome_end_date" = "cohort_end_date") %>%
    dplyr::inner_join(
      cdm[[denominatorTable]] %>%
        dplyr::filter(.data$cohort_definition_id %in%
          .env$denominatorCohortId) %>%
        dplyr::select(-"cohort_definition_id") %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::compute()

  outcome <- outcome %>%
    # most recent outcome starting before cohort start per person
    dplyr::filter(.data$outcome_start_date < .data$cohort_start_date) %>%
    dplyr::group_by(
      .data$subject_id,
      .data$cohort_start_date,
      .data$outcome_cohort_id
    ) %>%
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
      .data$outcome_cohort_id
    ) %>%
    dbplyr::window_order(.data$outcome_start_date) %>%
    dplyr::mutate(index = rank()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  # add to cdm_reference
  cdm[[outcomeTable]] <- outcome %>%
    dplyr::select(-"outcome_end_date") %>%
    dplyr::full_join(
      outcome %>%
        dplyr::mutate(index = .data$index + 1) %>%
        dplyr::rename("outcome_prev_end_date" = "outcome_end_date") %>%
        dplyr::select(-"outcome_start_date"),
      by = c(
        "subject_id", "cohort_start_date",
        "cohort_end_date", "outcome_cohort_id", "index"
      )
    ) %>%
    dplyr::select(-"index") %>%
    dplyr::compute()
  if (verbose == TRUE) {
    message("Progress: Limited to relevant outcomes")
    duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
    message(glue::glue(
      "Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }

  if (verbose == TRUE) {
    message("Progress: Get incidence for subgroups")
    start <- Sys.time()
  }
  studySpecs <- tidyr::expand_grid(
    outcome_cohort_id = outcomeCohortId,
    denominator_cohort_id = denominatorCohortId,
    interval = interval,
    full_periods = fullPeriods,
    outcome_washout = outcomeWashout,
    repeated_events = repeatedEvents,
    confidence_interval = confidenceInterval
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
    workingInc <- getIncidence(
      cdm = cdm,
      denominatorTable = denominatorTable,
      denominatorCohortId = x$denominator_cohort_id,
      outcomeTable = outcomeTable,
      outcomeCohortId = x$outcome_cohort_id,
      interval = x$interval,
      fullPeriods = x$full_periods,
      outcomeWashout = x$outcome_washout,
      repeatedEvents = x$repeated_events,
      returnAnalysisCohort = returnAnalysisCohort,
      verbose = verbose
    )

    workingIncIr <- workingInc[["ir"]] %>%
      dplyr::mutate(incidence_analysis_id = x$incidence_analysis_id) %>%
      dplyr::relocate("incidence_analysis_id")
    if (returnAnalysisCohort == TRUE) {
      workingIncPersonTable <- workingInc[["person_table"]] %>%
        dplyr::mutate(incidence_analysis_id = x$incidence_analysis_id) %>%
        dplyr::relocate("incidence_analysis_id")
    }

    workingIncAnalysisSettings <- workingInc[["analysis_settings"]] %>%
      dplyr::mutate(
        outcome_cohort_id = x$outcome_cohort_id,
        denominator_cohort_id = x$denominator_cohort_id,
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
    if (returnAnalysisCohort == TRUE) {
      result[["person_table"]] <- workingIncPersonTable
    }
    result[["attrition"]] <- workingIncAttrition


    return(result)
  })
  if (verbose == TRUE) {
    message("Progress: Incidence fetched for subgroups")
    duration <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
    message(glue::glue(
      "Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }

  if (verbose == TRUE) {
    message("Progress: Combining results")
    start <- Sys.time()
  }
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
  if (nrow(irs) > 0) {
    irs <- getCiIncidence(irs, confidenceInterval) %>%
      dplyr::relocate("ir_100000_pys_low", .after = "ir_100000_pys") %>%
      dplyr::relocate("ir_100000_pys_high", .after = "ir_100000_pys_low")

    # obscure counts
    irs <- obscureCounts(irs, minCellCount = minCellCount, substitute = NA)
  }

  # person_table summary
  if (returnAnalysisCohort == TRUE) {
    personTable <- irsList[names(irsList) == "person_table"]
    # to tibble
    personTable <- dplyr::bind_rows(personTable,
      .id = NULL
    )
  }

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
  if (returnAnalysisCohort == TRUE) {
    results[["person_table"]] <- personTable
  }
  results[["attrition"]] <- attrition

  if (verbose == TRUE) {
    message("Progress: All incidence results computed")
    duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
    message(glue::glue(
      "Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }


  if (verbose == TRUE) {
    duration <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
    message(glue::glue(
      "Overall time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }
  return(results)
}
