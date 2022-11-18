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
#' @param outcomeTable Name of the table with the outcome cohorts
#' @param denominatorCohortId Cohort ids of denominator populations
#' @param outcomeCohortId Outcome cohort ids
#' @param type type of prevalence, point or period
#' @param points where to compute the point prevalence
#' @param interval Time intervals for prevalence estimates
#' @param fullPeriods If full period is required
#' @param fullContribution If complete periods are required for
#' individuals to contribute
#' @param confidenceInterval Method for confidence intervals
#' @param minCellCount Minimum number of events to report- results
#' lower than this will be obscured. If NULL all results will be reported.
#' @param verbose Whether to report progress
#'
#' @return
#' @export
#'
#' @examples
estimatePrevalence <- function(cdm,
                               denominatorTable,
                               outcomeTable,
                               denominatorCohortId = NULL,
                               outcomeCohortId = NULL,
                               type = "point",
                               interval = "months",
                               fullPeriods = TRUE,
                               fullContribution = FALSE,
                               points = "start",
                               confidenceInterval = "binomial",
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
  checkmate::assert_character(outcomeCohortId,
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
  checkmate::assert_logical(fullContribution,
    add = errorMessage
  )
  checkmate::assert_logical(verbose,
    add = errorMessage
  )
  checkmate::assert_logical(fullPeriods,
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


  studySpecs <- tidyr::expand_grid(
    outcomeCohortId = outcomeCohortId,
    denominatorCohortId = denominatorCohortId,
    interval = interval,
    point = points,
    fullContribution = fullContribution
  )

  studySpecs <- studySpecs %>%
    dplyr::mutate(prevalence_analysis_id = as.character(dplyr::row_number()))

  studySpecs <- split(
    studySpecs,
    studySpecs[, c("prevalence_analysis_id")]
  )

  # get prs
  prsList <- lapply(studySpecs, function(x) {
    workingPrev <- getPrevalence(
      cdm = cdm,
      denominatorTable = denominatorTable,
      denominatorCohortId = x$denominatorCohortId,
      outcomeTable = outcomeTable,
      outcomeCohortId = x$outcomeCohortId,
      type = type,
      interval = x$interval,
      fullPeriods = fullPeriods,
      point = x$point,
      fullContribution = x$fullContribution,
      verbose = verbose
    )

    if (nrow(workingPrev[["pr"]]) == 0) {
      workingPrevPr <- tibble::tibble()
    } else {
      workingPrevPr <- workingPrev[["pr"]] %>%
        dplyr::mutate(prevalence_analysis_id = x$prevalence_analysis_id) %>%
        dplyr::relocate("prevalence_analysis_id")
    }

    workingPrevAnalysisSettings <- workingPrev[["analysis_settings"]] %>%
      dplyr::mutate(
        outcome_cohort_id = x$outcomeCohortId,
        denominator_cohort_id = x$denominatorCohortId,
        type = type,
        interval = x$interval,
        full_periods = fullPeriods,
        point = x$point,
        fullContribution = x$fullContribution,
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
  if (nrow(prs) > 0) {
    prs <- getCiPrevalence(prs, confidenceInterval) %>%
      dplyr::relocate("prev_low", .after = "prev") %>%
      dplyr::relocate("prev_high", .after = "prev_low")

    # obscure counts
    prs <- obscureCounts(prs,
      minCellCount = minCellCount,
      substitute = NA
    )
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


  if (verbose == TRUE) {
    duration <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
    message(glue::glue(
      "Overall time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }
  return(results)
}
