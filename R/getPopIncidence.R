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

#' Get population incidence estimates
#'
#' @param cdm CDMConnector CDM reference object
#' @param denominatorTable denominatorTable
#' @param outcomeTable outcomeTable
#' @param denominatorId denominatorId
#' @param outcomeId outcomeId
#' @param interval interval
#' @param fullPeriods fullPeriods
#' @param outcomeWashout outcomeWashout
#' @param repeatedEvents repeatedEvents
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
getPopIncidence <- function(cdm,
                            denominatorTable,
                            outcomeTable,
                            denominatorId,
                            outcomeId,
                            interval,
                            fullPeriods,
                            outcomeWashout,
                            repeatedEvents,
                            verbose) {
  if (verbose == TRUE) {
    message("-- Getting incidence")
  }
  if (!is.null(outcomeWashout)) {
    if (is.na(outcomeWashout)) {
      outcomeWashout <- NULL
    }
  }

  ## Analysis code
  # people in the relevant denominator
  studyPop <- cdm[[denominatorTable]] %>%
    dplyr::filter(.data$cohort_definition_id ==
      .env$denominatorId) %>%
    dplyr::select(-"cohort_definition_id")

  # outcomes of people in the denominator
  outcome <- cdm[[outcomeTable]] %>%
    dplyr::filter(.data$outcome_id == .env$outcomeId) %>%
    dplyr::select(-"outcome_id") %>%
    dplyr::inner_join(studyPop,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    )

  # bring into memory
  outcome <- outcome %>%
    dplyr::collect()
  studyPop <- studyPop %>%
    dplyr::collect()

  # participants without an outcome
  studyPopNoOutcome <- studyPop %>%
    dplyr::anti_join(
      outcome,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::mutate(outcome_start_date = as.Date(NA))

  # if outcome starts before cohort end date
  # update cohort_end_date
  studyPopOutcome <- outcome %>%
    dplyr::mutate(cohort_end_date = dplyr::if_else(
      !is.na(.data$outcome_start_date) &
        .data$outcome_start_date < .data$cohort_end_date,
      .data$outcome_start_date,
      .data$cohort_end_date
    ))

  if (is.null(outcomeWashout)) {
    # exclude anyone with a previous outcome
    studyPopOutcome <- studyPopOutcome %>%
      dplyr::filter(is.na(.data$outcome_prev_end_date)) %>%
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)
  } else {
    # otherwise add the washout to the previous outcome
    studyPopOutcome <- studyPopOutcome %>%
      dplyr::mutate(outcome_prev_end_date = dplyr::if_else(
        is.na(.data$outcome_prev_end_date),
        .data$outcome_prev_end_date,
        .data$outcome_prev_end_date + lubridate::days(.env$outcomeWashout + 1)
      )) %>%
      dplyr::mutate(cohort_start_date = dplyr::if_else(
        is.na(.data$outcome_prev_end_date) |
          (.data$cohort_start_date > .data$outcome_prev_end_date),
        .data$cohort_start_date,
        .data$outcome_prev_end_date
      )) %>%
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)
    if (repeatedEvents == FALSE) {
      # limit to first outcome in follow up if
      # not allowing at repeated outcomes
      studyPopOutcome <- studyPopOutcome %>%
        dplyr::group_by(.data$subject_id) %>%
        dplyr::filter(is.na(.data$outcome_prev_end_date) |
          .data$outcome_start_date ==
            min(.data$outcome_start_date, na.rm = TRUE)) %>%
        dplyr::ungroup()
    }
  }

  # match format of studyPop
  studyPopOutcome <- studyPopOutcome %>%
    dplyr::select(-"outcome_prev_end_date") %>%
    dplyr::mutate(cohort_end_date = dplyr::if_else(
      !is.na(.data$outcome_start_date),
      .data$outcome_start_date,
      .data$cohort_end_date
    ))

  # combine those without an outcome back with those with an outcome
  # this is now our study population to get the incidence rates for
  studyPop <- studyPopNoOutcome %>%
    dplyr::union_all(studyPopOutcome)


  # study dates
  # based on the earliest start and latest end of those
  # in the relevant denominator
  start_end <- studyPop %>%
    dplyr::summarise(
      min = min(.data$cohort_start_date, na.rm = TRUE),
      max = max(.data$cohort_end_date, na.rm = TRUE)
    )

  studyDays <- getStudyDays(
    startDate = start_end$min,
    endDate = start_end$max,
    timeInterval = interval,
    fullPeriods = fullPeriods
  )

  if (nrow(studyDays) == 0) {
    # if no study days we´ll return an empty tibble
    ir <- tibble::tibble()
  }

  if (nrow(studyDays) > 0) {
  # otherwise fetch incidence rates
  # looping through each time interval
  ir <- list()
  for (i in seq_len(nrow(studyDays))) {
    workingTime <- studyDays$time[i]
    workingStartTime <- studyDays$start_time[i]
    workingEndTime <- studyDays$end_time[i]

    # people who can contribute to the period
    workingPop <- studyPop %>%
      dplyr::filter(.data$cohort_end_date >= .env$workingStartTime) %>%
      dplyr::filter(.data$cohort_start_date <= .env$workingEndTime)

    if (nrow(workingPop > 0)) {
      # individuals start date for this period
      # which could be start of the period or later
      workingPop <- workingPop %>%
        dplyr::mutate(tStart = dplyr::if_else(.data$cohort_start_date <=
          .env$workingStartTime, .env$workingStartTime,
        .data$cohort_start_date
        ))

      # individuals end date for this period
      # end of the period or earlier
      workingPop <- workingPop %>%
        dplyr::mutate(
          tEnd =
            dplyr::if_else(.data$cohort_end_date >= .env$workingEndTime,
              .env$workingEndTime,
              .data$cohort_end_date
            )
        )

      workingPop <- workingPop %>%
        dplyr::select("subject_id", "tStart", "tEnd", "outcome_start_date")

      # compute working days
      workingPop <- workingPop %>%
        dplyr::mutate(workingDays = as.numeric(difftime(
          .data$tEnd +
            lubridate::days(1),
          .data$tStart,
          units = "days"
        )))

      # erase outcome_start_date if not during period
      workingPop <- workingPop %>%
        dplyr::mutate(outcome_start_date = dplyr::if_else(
          .data$outcome_start_date <= .data$tEnd &
          .data$outcome_start_date >= .data$tStart,
          .data$outcome_start_date,
          as.Date(NA),
          .data$outcome_start_date
        ))

      ir[[paste0(i)]] <- workingPop %>%
        dplyr::summarise(
          n_persons = dplyr::n_distinct(.data$subject_id),
          person_days = sum(.data$workingDays),
          n_events = sum(!is.na(.data$outcome_start_date))
        ) %>%
        dplyr::mutate(person_years = .data$person_days / 365.25) %>%
        dplyr::mutate(
          ir_100000_pys =
            (.data$n_events / .data$person_years) * 100000
        ) %>%
        dplyr::mutate(time = .env$workingTime) %>%
        dplyr::mutate(start_time = .env$workingStartTime) %>%
        dplyr::mutate(end_time = .env$workingEndTime)
    }
  }

  ir <- dplyr::bind_rows(ir)

  }

  # study design related variables
  analysisSettings <- tibble::tibble(
    outcome_washout = .env$outcomeWashout,
    repeated_events = .env$repeatedEvents,
    interval = .env$interval,
    full_periods = .env$fullPeriods
  )

  studyPop <- studyPop %>%
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date")

  # return list
  results <- list()
  results[["ir"]] <- ir
  results[["analysis_settings"]] <- analysisSettings
  results[["person_table"]] <- studyPop
  results[["attrition"]] <- tibble::tibble(attrition = "attrition")

  return(results)
}
