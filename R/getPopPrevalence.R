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

#' Get population prevalence estimates
#'
#' @param cdm CDMConnector CDM reference object
#' @param denominatorTable denominatorTable
#' @param denominatorCohortIds denominatorCohortIds
#' @param outcomesTable outcomesTable
#' @param outcomeCohortIds outcomeCohortIds
#' @param type type
#' @param interval interval
#' @param fullPeriodsRequired full period requirement
#' @param point point where to compute prevalence inside interval
#' @param minContribution minContribution
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
getPopPrevalence <- function(cdm,
                             denominatorTable,
                             denominatorCohortIds = NULL,
                             outcomesTable,
                             outcomeCohortIds = NULL,
                             type = "point",
                             interval = "months",
                             fullPeriodsRequired = TRUE,
                             point = "start",
                             minContribution = 0.5,
                             verbose = FALSE) {
  ## Analysis code
  # bring in study population
  studyPopDb <- cdm[[denominatorTable]]
  if (!is.null(denominatorCohortIds)) {
    studyPopDb <- studyPopDb %>%
      dplyr::filter(.data$cohort_definition_id ==
        .env$denominatorCohortIds)
  }

  outcomeDb <- cdm[[outcomesTable]]
  if (!is.null(outcomeCohortIds)) {
    outcomeDb <- outcomeDb %>%
      dplyr::filter(.data$cohort_definition_id == .env$outcomeCohortIds) %>%
      dplyr::compute()
  }

  # keep outcomes of people in the denominator
  outcomeDb <- outcomeDb %>%
    dplyr::inner_join(
      studyPopDb %>%
        dplyr::select("subject_id"),
      by = "subject_id"
    )

  # bring outcomes into memory
  if (verbose == TRUE) {
    message("Bringing outcomes into memory")
  }

  studyPop <- studyPopDb %>% dplyr::collect()

  outcome <- outcomeDb %>%
    dplyr::rename("subject_id" = "subject_id") %>%
    dplyr::rename("outcome_start_date" = "cohort_start_date") %>%
    dplyr::rename("outcome_end_date" = "cohort_end_date") %>%
    dplyr::select("subject_id", "outcome_start_date", "outcome_end_date") %>%
    dplyr::collect()

  if (interval == "days") {
    type <- "point"
  }

  # start date
  start <- min(studyPop$cohort_start_date)
  # end date
  end <- max(studyPop$cohort_end_date)
  # compute studyDays as a function of inputs
  studyDays <- computeStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    fullPeriodsRequired = fullPeriodsRequired,
    type = type,
    point = point
  )

  if (nrow(studyDays) == 0) {
    stop("Not enough following to compute the desired prevalence.")
  }

  # fetch prevalence
  # looping through each time interval
  pr <- list()
  for (i in seq_along(studyDays$time)) {
    workingStart <- studyDays$start_time[i]
    if (type == "period") {
      workingEnd <- studyDays$end_time[i]
      workingPeriod <- as.numeric(workingEnd - workingStart) + 1
    } else {
      workingEnd <- workingStart
      workingPeriod <- 1
    }

    # drop people with end_date prior to workingStart
    # drop people with start_date after workingEnd
    workingPop <- studyPop %>%
      dplyr::filter(.data$cohort_end_date >= .env$workingStart) %>%
      dplyr::filter(.data$cohort_start_date <= .env$workingEnd)

    # individuals start date for this period
    # which could be start of the period or later
    workingPop <- workingPop %>%
      dplyr::mutate(
        t_start_date =
          dplyr::if_else(.data$cohort_start_date <= .env$workingStart,
            .env$workingStart,
            .data$cohort_start_date
          )
      )


    # individuals end date for this period
    # end of the period or earlier
    workingPop <- workingPop %>%
      dplyr::mutate(
        t_end_date =
          dplyr::if_else(.data$cohort_end_date >= .env$workingEnd,
            .env$workingEnd,
            .data$cohort_end_date
          )
      )

    workingPop <- workingPop %>%
      dplyr::left_join(outcome,
        by = "subject_id"
      )

    workingPop <- workingPop %>%
      dplyr::select(
        "subject_id", "t_start_date",
        "t_end_date", "outcome_start_date", "outcome_end_date"
      )

    workingPop <- workingPop %>%
      dplyr::mutate(contribution = (as.numeric(difftime(.data$t_end_date,
        .data$t_start_date,
        units = "days"
      )) + 1) /
        workingPeriod) %>%
      dplyr::group_by(.data$subject_id) %>%
      dplyr::mutate(contribution = sum(.data$contribution)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$contribution >= .env$minContribution)

    denominator <- workingPop %>%
      dplyr::select("subject_id") %>%
      dplyr::distinct() %>%
      nrow()

    numerator <- workingPop %>%
      dplyr::filter(.data$outcome_start_date <= .data$t_end_date) %>%
      dplyr::filter(.data$outcome_end_date >= .data$t_start_date) %>%
      dplyr::select("subject_id") %>%
      dplyr::distinct() %>%
      nrow()

    pr[[paste0(i)]] <- studyDays[i, ] %>%
      dplyr::mutate(numerator = numerator) %>%
      dplyr::mutate(denominator = denominator) %>%
      dplyr::mutate(prev = numerator / denominator) %>%
      dplyr::select("time", "numerator", "denominator",
                    "prev", "start_time", "end_time")
  }

  pr <- dplyr::bind_rows(pr)

  # study design related variables
  # add study design related variables
  analysisSettings <- tibble::tibble(
    type = .env$type,
    point = .env$point,
    interval = .env$interval,
    min_contribution = .env$minContribution,
    full_periods_required = .env$fullPeriodsRequired
  )

  studyPop <- studyPop %>%
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date")

  results <- list()
  results[["pr"]] <- pr
  results[["analysis_settings"]] <- analysisSettings
  results[["person_table"]] <- studyPop
  results[["attrition"]] <- tibble::tibble(attrition = "attrition")

  return(results)
}
