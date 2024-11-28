# Copyright 2024 DARWIN EU®
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
                         weeks,
                         months,
                         quarters,
                         years,
                         overall,
                         completeDatabaseIntervals,
                         outcomeWashout,
                         repeatedEvents,
                         tablePrefix,
                         analysisId,
                         strata,
                         includeOverallStrata) {

  if (!is.null(outcomeWashout) && is.na(outcomeWashout)) {
      outcomeWashout <- NULL
    }

  ## Analysis code
  # people in the relevant denominator
  # along with their outcomes
  studyPop <- cdm[[denominatorTable]] %>%
    dplyr::filter(.data$cohort_definition_id ==
                    .env$denominatorCohortId) %>%
    dplyr::select(-"cohort_definition_id") %>%
    dplyr::left_join(
      cdm[[outcomeTable]] %>%
        dplyr::filter(.data$outcome_cohort_id ==
                        .env$outcomeCohortId) %>%
        dplyr::select(-"outcome_cohort_id"),
      by = c(
        "subject_id",
        "cohort_start_date",
        "cohort_end_date"
      )
    ) %>%
    dplyr::compute(
      name = paste0(tablePrefix, "_inc_5"),
      temporary = FALSE,
      overwrite = TRUE
    )

  attrition <- recordAttrition(
    table = studyPop,
    id = "subject_id",
    reasonId = 11,
    reason = "Starting analysis population"
  )

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
    dplyr::mutate(cohort_end_date = dplyr::coalesce(
      .data$outcome_start_date,
      .data$cohort_end_date
    ))

  nStudyPopOutcome <- studyPopOutcome %>%
    utils::head(10) %>%
    dplyr::tally() %>%
    dplyr::pull("n")


  if(nStudyPopOutcome > 0){
    if (is.null(outcomeWashout)) {
      # exclude anyone with a previous outcome
      studyPopOutcome <- studyPopOutcome %>%
        dplyr::filter(is.na(.data$outcome_prev_end_date) &
                        .data$cohort_start_date <= .data$cohort_end_date)
    } else {
      # otherwise add the washout to the previous outcome
      outcomeWashoutPlusOne <- as.integer(outcomeWashout + 1)
      studyPopOutcome <- studyPopOutcome %>%
        dplyr::mutate(outcome_prev_end_date =
                        as.Date(.data$outcome_prev_end_date)) %>%
        dplyr::mutate(outcome_prev_end_date = dplyr::if_else(
          is.na(.data$outcome_prev_end_date),
          as.Date(.data$outcome_prev_end_date),
          as.Date(!!CDMConnector::dateadd("outcome_prev_end_date",
                                          {{ outcomeWashoutPlusOne }},
                                          interval = "day"
          ))
        )) %>%
        dplyr::mutate(cohort_start_date = dplyr::if_else(
          is.na(.data$outcome_prev_end_date) |
            (.data$cohort_start_date > .data$outcome_prev_end_date),
          .data$cohort_start_date,
          .data$outcome_prev_end_date
        )) %>%
        dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)
      if (repeatedEvents == FALSE &&
          sum(!is.na(studyPopOutcome %>%
                     dplyr::pull(.data$outcome_start_date))) > 0) {
        studyPopOutcome <- studyPopOutcome %>%
          dplyr::group_by(.data$subject_id) %>%
          dplyr::mutate(events_post = sum(dplyr::if_else(
            !is.na(.data$outcome_start_date), 1, 0
          ), na.rm = TRUE))

        studyPopOutcomeWH <- studyPopOutcome %>%
          dplyr::group_by(.data$subject_id) %>%
          dplyr::filter(.data$events_post == 0) %>%
          dplyr::compute(
            name = paste0(tablePrefix, "_inc_5a"),
            temporary = FALSE,
            overwrite = TRUE
          )

        studyPopOutcome <- dplyr::union_all(
          # with history of outcome, without outcome in follow up
          studyPopOutcomeWH,
          # with outcome in follow up - limit to first
          studyPopOutcome %>%
            dplyr::filter(.data$events_post >= 1) %>%
            dplyr::group_by(.data$subject_id) %>%
            dplyr::filter(.data$cohort_start_date ==
                            min(.data$cohort_start_date, na.rm = TRUE)) %>%
            dplyr::ungroup()
        ) %>%
          dplyr::select(-"events_post")
      }
    }

    # set cohort end date
    studyPopOutcome <- studyPopOutcome %>%
      dplyr::mutate(cohort_end_date = dplyr::if_else(
        !is.na(.data$outcome_start_date),
        .data$outcome_start_date,
        .data$cohort_end_date
      ))

  }

  # combine those without an outcome back with those with an outcome
  # this is now our study population to get the incidence rates for
  studyPop <- studyPopNoOutcome %>%
    dplyr::union_all(studyPopOutcome) %>%
    dplyr::collect()
  attrition <- recordAttrition(
    table = studyPop,
    id = "subject_id",
    reasonId = 12,
    reason = "Excluded due to prior event (do not pass outcome washout during study period)",
    existingAttrition = attrition
  )
# intervals to estimate
interval <- c(
  "weeks" = weeks,
  "months" = months,
  "quarters" = quarters,
  "years" = years,
  "overall" = overall
)
interval <- names(interval[interval])

  # study dates
  # based on the earliest start and latest end of those
  # in the relevant denominator
  studyDays <- list()
  if (nrow(studyPop) > 0) {
    startEnd <- studyPop %>%
      dplyr::summarise(
        min = as.Date(min(.data$cohort_start_date, na.rm = TRUE)),
        max = as.Date(max(.data$cohort_end_date, na.rm = TRUE))
      )
    if ("weeks" %in% interval) {
      studyDays[["weeks"]] <- getStudyDays(
        startDate = startEnd$min,
        endDate = startEnd$max,
        timeInterval = "weeks",
        completeDatabaseIntervals = completeDatabaseIntervals
      )
    }
    if ("months" %in% interval) {
      studyDays[["months"]] <- getStudyDays(
        startDate = startEnd$min,
        endDate = startEnd$max,
        timeInterval = "months",
        completeDatabaseIntervals = completeDatabaseIntervals
      )
    }
    if ("quarters" %in% interval) {
      studyDays[["quarters"]] <- getStudyDays(
        startDate = startEnd$min,
        endDate = startEnd$max,
        timeInterval = "quarters",
        completeDatabaseIntervals = completeDatabaseIntervals
      )
    }
    if ("years" %in% interval) {
      studyDays[["years"]] <- getStudyDays(
        startDate = startEnd$min,
        endDate = startEnd$max,
        timeInterval = "years",
        completeDatabaseIntervals = completeDatabaseIntervals
      )
    }
    if ("overall" %in% interval) {
      # note, full periods argument does not apply
      # for overall we just go from start to end
      studyDays[["overall"]] <- dplyr::tibble(
        time = "overall",
        start_time = startEnd$min,
        end_time = startEnd$max
      )
    }
  } else{
    # empty population
    studyDays[["none"]] <- dplyr::tibble()
  }
 if (nrow(dplyr::bind_rows(studyDays)) == 0) {
   # no study days
    studyDays[["none"]] <- dplyr::tibble()
  }

  if ("none" %in% names(studyDays)) {
    # if no study days we´ll return an empty tibble
    ir <- dplyr::tibble()

    attrition <- recordAttrition(
      table = dplyr::tibble(subject_id = integer()),
      id = "subject_id",
      reasonId = 13,
      reason = "Not observed during the complete database interval",
      existingAttrition = attrition
    )
  }

  if (!"none" %in% names(studyDays)) {
    # drop for complete database intervals requirement
    minStartDate <- min(dplyr::bind_rows(studyDays)$start_time)
    maxStartDate <- max(dplyr::bind_rows(studyDays)$end_time )
    studyPop <- studyPop %>%
      dplyr::filter(.data$cohort_end_date >= .env$minStartDate,
                    .data$cohort_start_date <= .env$maxStartDate)

    attrition <- recordAttrition(
      table = studyPop,
      id = "subject_id",
      reasonId = 14,
      reason = "Not observed during the complete database interval",
      existingAttrition = attrition
    )

    # fetch incidence rates
    # looping through each time interval
    ir <- list()
    for(j in seq_along(interval)){
      workingInterval <- interval[j]
      workingStudyDays <- studyDays[[workingInterval]]
    for (i in seq_len(nrow(workingStudyDays))) {
      workingStartTime <- workingStudyDays$start_time[i]
      workingEndTime <- workingStudyDays$end_time[i]

      # people who can contribute to the period
      workingPop <- studyPop %>%
        dplyr::filter(.data$cohort_end_date >= .env$workingStartTime,
                      .data$cohort_start_date <= .env$workingEndTime)

      if (nrow(workingPop) > 0) {
        # individuals start date for this period
        # which could be start of the period or later
        workingPop <- workingPop %>%
          dplyr::mutate(tStart = dplyr::if_else(.data$cohort_start_date <= .env$workingStartTime,
                                                as.Date(.env$workingStartTime),
                                                as.Date(.data$cohort_start_date)
          ),
          # individuals end date for this period
          # end of the period or earlier
            tEnd =
              dplyr::if_else(.data$cohort_end_date >= .env$workingEndTime,
                             as.Date(.env$workingEndTime),
                             as.Date(.data$cohort_end_date)
              )
          ) %>%
          dplyr::mutate(workingDays = as.numeric(difftime(
            .data$tEnd,
            .data$tStart,
            units = "days"
          )) + 1)  %>%
        # erase outcome_start_date if not during period
          dplyr::mutate(outcome_start_date = dplyr::if_else(
            .data$outcome_start_date <= .data$tEnd &
              .data$outcome_start_date >= .data$tStart,
            as.Date(.data$outcome_start_date),
            as.Date(NA)
          ))

        if(length(strata)==0 || includeOverallStrata == TRUE){
          ir[[paste0(i, "_", j)]] <- workingPop %>%
            dplyr::summarise(
              denominator_count = dplyr::n_distinct(.data$subject_id),
              person_days = sum(.data$workingDays),
              outcome_count = sum(!is.na(.data$outcome_start_date))
            ) %>%
            dplyr::mutate(incidence_start_date = .env$workingStartTime,
                          incidence_end_date = .env$workingEndTime,
                          analysis_interval = .env$workingInterval)
        } else {
          ir[[paste0(i, "_", j)]] <- dplyr::tibble()
        }

        if(length(strata)>=1){
          ir[[paste0(i, "_", j)]] <- ir[[paste0(i, "_", j)]] %>%
            omopgenerics::uniteStrata()
          for(k in seq_along(strata)){
            ir[[paste0(i, "_", j, "_", k)]] <-  dplyr::bind_rows(ir[[paste0(i, "_", j)]],
                                  getStratifiedIncidenceResult(workingPop = workingPop,
                                                                              workingStrata = strata[[k]],
                                                                              workingStartTime = workingStartTime,
                                                                              workingEndTime= workingEndTime,
                                                                              workingInterval = workingInterval))
          }
        }

      }
    }}
    ir <- dplyr::bind_rows(ir)
    if(nrow(ir) > 0){
    ir <- ir %>%
      dplyr::mutate(
        person_years = round(.data$person_days / 365.25, 3),
        incidence_100000_pys =
          round(((.data$outcome_count / .data$person_years) * 100000), 3)
      )
    }

  }

  # study design related variables
  if(is.null(outcomeWashout)){
    outcomeWashout <- "inf"
  }
  analysisSettings <- dplyr::tibble(
    analysis_outcome_washout = .env$outcomeWashout,
    analysis_repeated_events = .env$repeatedEvents,
    analysis_complete_database_intervals = .env$completeDatabaseIntervals
  )
  studyPop <- studyPop %>%
    dplyr::select(
      "subject_id", "cohort_start_date",
      "cohort_end_date", "outcome_start_date"
    )


  # return list
  results <- list()
  results[["ir"]] <- ir
  results[["analysis_settings"]] <- analysisSettings
  results[["attrition"]] <- attrition

  return(results)
}


getStratifiedIncidenceResult <- function(workingPop, workingStrata,
                                         workingStartTime, workingEndTime,
                                         workingInterval){
  workingPop %>%
    dplyr::group_by(dplyr::pick(.env$workingStrata)) %>%
    dplyr::summarise(
      denominator_count = dplyr::n_distinct(.data$subject_id),
      person_days = sum(.data$workingDays),
      outcome_count = sum(!is.na(.data$outcome_start_date))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(incidence_start_date = .env$workingStartTime,
                  incidence_end_date = .env$workingEndTime,
                  analysis_interval = .env$workingInterval) %>%
    omopgenerics::uniteStrata(cols = workingStrata)
}
