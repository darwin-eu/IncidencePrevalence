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

getIncidence <- function(cdm,
                            denominatorTable,
                            outcomeTable,
                            denominatorCohortId,
                            outcomeCohortId,
                            interval,
                            completeDatabaseIntervals,
                            outcomeWashout,
                            repeatedEvents,
                            returnAnalysisCohort,
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
  # collect the people in the relevant denominator
  # along with their outcomes
  studyPop <- cdm[[denominatorTable]] %>%
    dplyr::filter(.data$cohort_definition_id ==
                    .env$denominatorCohortId) %>%
    dplyr::select(-"cohort_definition_id") %>%
    dplyr::left_join(cdm[[outcomeTable]] %>%
                       dplyr::filter(.data$outcome_cohort_id == .env$outcomeCohortId) %>%
                       dplyr::select(-"outcome_cohort_id"),
                     by = c("subject_id",
                            "cohort_start_date",
                            "cohort_end_date")) %>%
    dplyr::collect()

  # participants without an outcome
  studyPopNoOutcome <- studyPop %>%
    dplyr::filter(is.na(.data$outcome_start_date) &
                  is.na(.data$outcome_prev_end_date))

  # participants with an outcome
  # if outcome starts before cohort end date
  # update cohort_end_date
  studyPopOutcome <- studyPop %>%
    dplyr::filter(!is.na(.data$outcome_start_date) |
                  !is.na(.data$outcome_prev_end_date)) %>%
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
    if (repeatedEvents == FALSE &
        sum(!is.na(studyPopOutcome$outcome_start_date)) > 0) {
      studyPopOutcome <- studyPopOutcome %>%
        dplyr::group_by(.data$subject_id) %>%
        dplyr::mutate(events_post=sum(!is.na(.data$outcome_start_date)))
      studyPopOutcome <- dplyr::bind_rows(
        # with history of outcome, without outcome in follow up
        studyPopOutcome %>%
          dplyr::group_by(.data$subject_id) %>%
          dplyr::filter(.data$events_post==0),
        # with outcome in follow up - limit to first
        studyPopOutcome %>%
          dplyr::filter(.data$events_post>=1) %>%
          dplyr::group_by(.data$subject_id) %>%
          dplyr::filter(.data$cohort_start_date ==
                          min(.data$cohort_start_date)) %>%
          dplyr::ungroup()) %>%
          dplyr::select(!"events_post")
    }
  }

  # set cohort end date
  studyPopOutcome <- studyPopOutcome %>%
    dplyr::mutate(cohort_end_date = dplyr::if_else(
      !is.na(.data$outcome_start_date),
      .data$outcome_start_date,
      .data$cohort_end_date
    ))

  # combine those without an outcome back with those with an outcome
  # this is now our study population to get the incidence rates for
  studyPop <- studyPopNoOutcome %>%
    dplyr::bind_rows(studyPopOutcome)

  # study dates
  # based on the earliest start and latest end of those
  # in the relevant denominator
  start_end <- studyPop %>%
    dplyr::summarise(
      min = min(.data$cohort_start_date, na.rm = TRUE),
      max = max(.data$cohort_end_date, na.rm = TRUE)
    )

  if(interval=="overall"){
    # note, full periods argument does not apply
    # for overall we just go from start to end
    studyDays <- tibble::tibble(
      time="overall",
      start_time=start_end$min,
      end_time=start_end$max
    )
  } else {
  studyDays <- getStudyDays(
    startDate = start_end$min,
    endDate = start_end$max,
    timeInterval = interval,
    completeDatabaseIntervals = completeDatabaseIntervals
  )
  }

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
      dplyr::filter(.data$cohort_end_date >= .env$workingStartTime &
                    .data$cohort_start_date <= .env$workingEndTime)

    if (nrow(workingPop > 0)) {
      # individuals start date for this period
      # which could be start of the period or later
      workingPop <- workingPop %>%
        dplyr::mutate(tStart = dplyr::if_else(.data$cohort_start_date <=
          .env$workingStartTime, .env$workingStartTime,
        .data$cohort_start_date
        ))  %>%
      # individuals end date for this period
      # end of the period or earlier
        dplyr::mutate(
          tEnd =
            dplyr::if_else(.data$cohort_end_date >= .env$workingEndTime,
              .env$workingEndTime,
              .data$cohort_end_date
            )
        )

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
          as.Date(NA)
        ))

      ir[[paste0(i)]] <- workingPop %>%
        dplyr::summarise(
          n_persons = dplyr::n_distinct(.data$subject_id),
          person_days = sum(.data$workingDays),
          n_events = sum(!is.na(.data$outcome_start_date))
        ) %>%
        dplyr::mutate(time = .env$workingTime) %>%
        dplyr::mutate(start_time = .env$workingStartTime) %>%
        dplyr::mutate(end_time = .env$workingEndTime)
    }
  }

  ir <- dplyr::bind_rows(ir) %>%
    dplyr::mutate(person_years = .data$person_days / 365.25) %>%
    dplyr::mutate(ir_100000_pys =
                    (.data$n_events / .data$person_years) * 100000)

  }

  # study design related variables
  analysisSettings <- tibble::tibble(
    analysis_outcome_washout = .env$outcomeWashout,
    analysis_repeated_events = .env$repeatedEvents,
    analysis_interval = .env$interval,
    analysis_complete_database_intervals = .env$completeDatabaseIntervals
  )
  if(returnAnalysisCohort==TRUE){
  studyPop <- studyPop %>%
    dplyr::select("subject_id", "cohort_start_date",
                  "cohort_end_date", "outcome_start_date")}

  # return list
  results <- list()
  results[["ir"]] <- ir
  results[["analysis_settings"]] <- analysisSettings
  if(returnAnalysisCohort==TRUE){
  results[["person_table"]] <- studyPop
  }
  results[["attrition"]] <- tibble::tibble(attrition = "attrition")

  return(results)
}
