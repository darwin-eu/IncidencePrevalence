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
#' @param outcomesTable outcomesTable
#' @param denominatorCohortIds denominatorCohortIds
#' @param outcomeCohortIds outcomeCohortIds
#' @param interval interval
#' @param fullPeriodsRequired fullPeriodsRequired
#' @param outcomeWashout outcomeWashout
#' @param repeatedEvents repeatedEvents
#' @param verbose verbose
#'
#' @importFrom lubridate `%within%`
#' @return
#' @export
#'
#' @examples
get_pop_incidence <- function(cdm,
                              denominatorTable,
                              outcomesTable,
                              denominatorCohortIds,
                              outcomeCohortIds,
                              interval,
                              fullPeriodsRequired,
                              outcomeWashout,
                              repeatedEvents,
                              verbose) {

  if (!is.null(outcomeWashout)) {
    if (is.na(outcomeWashout)) {
      outcomeWashout <- NULL
    }
  }

  ## Analysis code
  # bring in study population
  study_pop <- cdm[[denominatorTable]] %>%
    dplyr::filter(.data$cohort_definition_id ==
      .env$denominatorCohortIds) %>%
    dplyr::select(-"cohort_definition_id") %>%
    dplyr::compute()

  # keep outcomes of people in the denominator
  outcome <- cdm[[outcomesTable]] %>%
    dplyr::filter(.data$outcome_id == .env$outcomeCohortIds) %>%
    dplyr::select(-"outcome_id") %>%
    dplyr::inner_join(study_pop,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::compute()

  # bring outcomes into memory
  if (verbose == TRUE) {
    message("Bringing outcomes into memory")
  }

  # start date
  start_date <- min(dplyr::pull(study_pop, "cohort_start_date"), na.rm = TRUE)
  # end date to the last day of last available full period
  end_date <- max(dplyr::pull(study_pop, "cohort_end_date"), na.rm = TRUE)

  # study dates
  study_days <- computeStudyDays(
    startDate = start_date,
    endDate = end_date,
    timeInterval = interval,
    fullPeriodsRequired = fullPeriodsRequired
  )

  if (nrow(study_days) == 0) {
    stop("Not enough time observed to compute the desired incidence")
  }

  study_pop <- study_pop %>%
    dplyr::anti_join(
      outcome,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::mutate(outcome_start_date = as.Date(NA))

  outcome <- outcome %>%
    dplyr::mutate(cohort_end_date = dplyr::if_else(
      !is.na(.data$outcome_start_date) & .data$outcome_start_date < .data$cohort_end_date,
      .data$outcome_start_date,
      .data$cohort_end_date
    ))

  if (is.null(outcomeWashout)) {
    outcome <- outcome %>%
      dplyr::filter(is.na(.data$outcome_prev_end_date)) %>%
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)
  } else {

    sqlAddDays_washout <- sqlAddDays(
      dialect = CDMConnector::dbms(attr(cdm, "dbcon")),
      days_to_add = 1+outcomeWashout,
      variable = "outcome_prev_end_date"
    )

    outcome <- outcome %>%
      dplyr::mutate(outcome_prev_end_date = dplyr::if_else(
        is.na(.data$outcome_prev_end_date),
        .data$outcome_prev_end_date,
        as.Date(dbplyr::sql(sqlAddDays_washout)))) %>%
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
          .data$outcome_start_date == min(.data$outcome_start_date, na.rm = TRUE)) %>%
        dplyr::ungroup()
    }
  }

  study_pop <- study_pop %>%
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
  for (i in seq_along(1:nrow(study_days))) {
    working_t_name <- study_days$time[i]
    working_t_start <- study_days$start_time[i]
    working_t_end <- study_days$end_time[i]
    working_t_days <- as.numeric(working_t_end - working_t_start) + 1

    # drop people with end_date prior to working_t_start
    # drop people with start_date after working_t_end
    working_pop <- study_pop %>%
      dplyr::filter(.data$cohort_end_date >= .env$working_t_start) %>%
      dplyr::filter(.data$cohort_start_date <= .env$working_t_end)

    if(nrow(working_pop>0)){
    # individuals start date for this period
    # which could be start of the period or later
    working_pop <- working_pop %>%
      dplyr::mutate(t_start_date = dplyr::if_else(.data$cohort_start_date <=
        .env$working_t_start, .env$working_t_start,
      .data$cohort_start_date
      ))

    # individuals end date for this period
    # end of the period or earlier
    working_pop <- working_pop %>%
      dplyr::mutate(
        t_end_date =
          dplyr::if_else(.data$cohort_end_date >= .env$working_t_end,
            .env$working_t_end,
            .data$cohort_end_date
          )
      )

    working_pop <- working_pop %>%
      dplyr::select("subject_id", "t_start_date", "t_end_date", "outcome_start_date")

    # compute working days and
    # erase outcome_start_date if not
    # inside interval
    sql_add_day_to_t_end <- sqlAddDays(
      dialect = CDMConnector::dbms(attr(cdm, "dbcon")),
      days_to_add = 1,
      variable = "t_end_date"
    )

    working_pop <- working_pop %>%
      dplyr::mutate(working_days = as.numeric(difftime(.data$t_end_date +
                                                         lubridate::days(1),
        .data$t_start_date,
        units = "days"
      ))) %>%
      dplyr::mutate(outcome_start_date = dplyr::if_else(.data$outcome_start_date <= .data$t_end_date,
        .data$outcome_start_date,
        as.Date(NA),
        .data$outcome_start_date
      )) %>%
      dplyr::mutate(outcome_start_date = dplyr::if_else(.data$outcome_start_date >= .data$t_start_date,
        .data$outcome_start_date,
        as.Date(NA),
        .data$outcome_start_date
      ))

    ir[[paste0(i)]] <- working_pop %>%
      dplyr::summarise(
        n_persons = dplyr::n_distinct(.data$subject_id),
        person_days = sum(.data$working_days),
        n_events = sum(!is.na(.data$outcome_start_date))
      ) %>%
      dplyr::mutate(person_years = .data$person_days / 365.25) %>%
      dplyr::mutate(ir_100000_pys = (.data$n_events / .data$person_years) * 100000) %>%
      dplyr::mutate(time = .env$working_t_name) %>%
      dplyr::mutate(start_time = .env$working_t_start) %>%
      dplyr::mutate(end_time = .env$working_t_end)
    }
  }

  ir <- dplyr::bind_rows(ir)

  # study design related variables
  analysis_settings <- tibble::tibble(
    outcome_washout = .env$outcomeWashout,
    repeated_events = .env$repeatedEvents,
    interval = .env$interval,
    full_periods_required = .env$fullPeriodsRequired
  )

  study_pop <- study_pop %>%
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date")

  # return list
  results <- list()
  results[["ir"]] <- ir
  results[["analysis_settings"]] <- analysis_settings
  results[["person_table"]] <- study_pop
  results[["attrition"]] <- tibble::tibble(attrition = "attrition") # placeholder

  return(results)
}
