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
  # bring in study population
  studyPop <- cdm[[denominatorTable]] %>%
    dplyr::filter(.data$cohort_definition_id ==
      .env$denominatorId) %>%
    dplyr::select(-"cohort_definition_id") %>%
    dplyr::compute()

  # keep outcomes of people in the denominator
  outcome <- cdm[[outcomeTable]] %>%
    dplyr::filter(.data$outcome_id == .env$outcomeId) %>%
    dplyr::select(-"outcome_id") %>%
    dplyr::inner_join(studyPop,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::compute()

  # study dates
  start <- studyPop %>%
    dplyr::summarise(min=min(.data$cohort_start_date, na.rm=TRUE)) %>%
    dplyr::pull()
  end <- studyPop %>%
    dplyr::summarise(max=max(.data$cohort_end_date, na.rm=TRUE)) %>%
    dplyr::pull()
  studyDays <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    fullPeriods = fullPeriods
  )

  if (nrow(studyDays) == 0) {
    stop("Not enough time observed to compute the desired incidence")
  }

  studyPop <- studyPop %>%
    dplyr::anti_join(
      outcome,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::mutate(outcome_start_date = as.Date(NA))

  outcome <- outcome %>%
    dplyr::mutate(cohort_end_date = dplyr::if_else(
      !is.na(.data$outcome_start_date) &
        .data$outcome_start_date < .data$cohort_end_date,
      .data$outcome_start_date,
      .data$cohort_end_date
    ))

  if (is.null(outcomeWashout)) {
    outcome <- outcome %>%
      dplyr::filter(is.na(.data$outcome_prev_end_date)) %>%
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)
  } else {
    sqlAddDaysWashout <- sqlAddDays(
      dialect = CDMConnector::dbms(attr(cdm, "dbcon")),
      daysToAdd = 1 + outcomeWashout,
      variable = "outcome_prev_end_date"
    )

    outcome <- outcome %>%
      dplyr::mutate(outcome_prev_end_date = dplyr::if_else(
        is.na(.data$outcome_prev_end_date),
        .data$outcome_prev_end_date,
        as.Date(dbplyr::sql(sqlAddDaysWashout))
      )) %>%
      dplyr::mutate(cohort_start_date = dplyr::if_else(
        is.na(.data$outcome_prev_end_date) |
          (.data$cohort_start_date > .data$outcome_prev_end_date),
        .data$cohort_start_date,
        .data$outcome_prev_end_date
      )) %>%
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)

    if (repeatedEvents == FALSE) {
      outcome <- outcome %>%
        dplyr::group_by(.data$subject_id) %>%
        dplyr::filter(is.na(.data$outcome_prev_end_date) |
          .data$outcome_start_date ==
            min(.data$outcome_start_date, na.rm = TRUE)) %>%
        dplyr::ungroup()
    }
  }
  studyPop <- studyPop %>% dplyr::compute()

  studyPop <- studyPop %>%
    dplyr::union_all(outcome %>%
      dplyr::select(-"outcome_prev_end_date") %>%
      dplyr::mutate(cohort_end_date = dplyr::if_else(
        !is.na(.data$outcome_start_date),
        .data$outcome_start_date,
        .data$cohort_end_date
      ))) %>%
    dplyr::collect()

  # fetch incidence rates
  # looping through each time interval
  ir <- list()
  for (i in seq_len(nrow(studyDays))) {
    workingTime <- studyDays$time[i]
    workingStartTime <- studyDays$start_time[i]
    workingEndTime <- studyDays$end_time[i]

    # drop people with end_date prior to workingStartTime
    # drop people with start_date after workingEndTime
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

      # compute working days and
      # erase outcome_start_date if not
      # inside interval
      workingPop <- workingPop %>%
        dplyr::mutate(workingDays = as.numeric(difftime(
          .data$tEnd +
            lubridate::days(1),
          .data$tStart,
          units = "days"
        ))) %>%
        dplyr::mutate(outcome_start_date = dplyr::if_else(
          .data$outcome_start_date <=
            .data$tEnd,
          .data$outcome_start_date,
          as.Date(NA),
          .data$outcome_start_date
        )) %>%
        dplyr::mutate(outcome_start_date = dplyr::if_else(
          .data$outcome_start_date >=
            .data$tStart,
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
