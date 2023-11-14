# Copyright 2023 DARWIN EU®
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
                         tablePrefix,
                         returnParticipants,
                         analysisId,
                         strata,
                         includeOverallStrata) {
  if (!is.null(outcomeWashout)) {
    if (is.na(outcomeWashout)) {
      outcomeWashout <- NULL
    }
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
    )

  studyPop <- studyPop %>%
    CDMConnector::computeQuery(
      name = paste0(tablePrefix, "_inc_5"),
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
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
    dplyr::mutate(cohort_end_date = dplyr::if_else(
      !is.na(.data$outcome_start_date) &
        .data$outcome_start_date < .data$cohort_end_date,
      .data$outcome_start_date,
      .data$cohort_end_date
    ))

  if (is.null(outcomeWashout)) {
    # exclude anyone with a previous outcome
    studyPopOutcome <- studyPopOutcome %>%
      dplyr::filter(is.na(.data$outcome_prev_end_date) &
        .data$cohort_start_date <= .data$cohort_end_date)
  } else {
    # otherwise add the washout to the previous outcome
    outcomeWashoutPlusOne <- outcomeWashout + 1
    studyPopOutcome <- studyPopOutcome %>%
      dplyr::mutate(outcome_prev_end_date = dplyr::if_else(
        is.na(.data$outcome_prev_end_date),
        .data$outcome_prev_end_date,
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
        dplyr::filter(.data$events_post == 0)

      studyPopOutcomeWH <- studyPopOutcomeWH %>%
        CDMConnector::computeQuery(
          name = paste0(tablePrefix, "_inc_5a"),
          temporary = FALSE,
          schema = attr(cdm, "write_schema"),
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

  # combine those without an outcome back with those with an outcome
  # this is now our study population to get the incidence rates for
  studyPopDb <- studyPopNoOutcome %>%
    dplyr::union_all(studyPopOutcome)

  studyPop <- studyPopDb %>%
    dplyr::collect()
  attrition <- recordAttrition(
    table = studyPop,
    id = "subject_id",
    reasonId = 12,
    reason = "Excluded due to prior event (do not pass outcome washout during study period)",
    existingAttrition = attrition
  )

  # study dates
  # based on the earliest start and latest end of those
  # in the relevant denominator
  if (nrow(studyPop) > 0) {
    startEnd <- studyPop %>%
      dplyr::summarise(
        min = min(.data$cohort_start_date, na.rm = TRUE),
        max = max(.data$cohort_end_date, na.rm = TRUE)
      )

    if (interval == "overall") {
      # note, full periods argument does not apply
      # for overall we just go from start to end
      studyDays <- dplyr::tibble(
        time = "overall",
        start_time = startEnd$min,
        end_time = startEnd$max
      )
    } else {
      studyDays <- getStudyDays(
        startDate = startEnd$min,
        endDate = startEnd$max,
        timeInterval = interval,
        completeDatabaseIntervals = completeDatabaseIntervals
      )
    }
  } else {
    studyDays <- dplyr::tibble()
  }

  if (nrow(studyDays) == 0) {
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

  if (nrow(studyDays) > 0) {
    # drop for complete database intervals requirement
    minStartDate <- min(studyDays$start_time)
    maxStartDate <- max(studyDays$end_time)
    studyPop <- studyPop %>%
      dplyr::filter(.data$cohort_end_date >= .env$minStartDate) %>%
      dplyr::filter(.data$cohort_start_date <= .env$maxStartDate)

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
    for (i in seq_len(nrow(studyDays))) {
      workingStartTime <- studyDays$start_time[i]
      workingEndTime <- studyDays$end_time[i]

      # people who can contribute to the period
      workingPop <- studyPop %>%
        dplyr::filter(.data$cohort_end_date >= .env$workingStartTime) %>%
        dplyr::filter(.data$cohort_start_date <= .env$workingEndTime)

      if (nrow(workingPop) > 0) {
        # individuals start date for this period
        # which could be start of the period or later
        workingPop <- workingPop %>%
          dplyr::mutate(tStart = dplyr::if_else(.data$cohort_start_date <= .env$workingStartTime,
            as.Date(.env$workingStartTime),
            as.Date(.data$cohort_start_date)
          )) %>%
          # individuals end date for this period
          # end of the period or earlier
          dplyr::mutate(
            tEnd =
              dplyr::if_else(.data$cohort_end_date >= .env$workingEndTime,
                as.Date(.env$workingEndTime),
                as.Date(.data$cohort_end_date)
              )
          )

        # compute working days
        workingPop <- workingPop %>%
          dplyr::mutate(workingDays = as.numeric(difftime(
            .data$tEnd,
            .data$tStart,
            units = "days"
          )) + 1)

        # erase outcome_start_date if not during period
        workingPop <- workingPop %>%
          dplyr::mutate(outcome_start_date = dplyr::if_else(
            .data$outcome_start_date <= .data$tEnd &
              .data$outcome_start_date >= .data$tStart,
            as.Date(.data$outcome_start_date),
            as.Date(NA)
          ))

        if(length(strata)==0 || includeOverallStrata == TRUE){
        ir[[paste0(i)]] <- workingPop %>%
          dplyr::summarise(
            n_persons = dplyr::n_distinct(.data$subject_id),
            person_days = sum(.data$workingDays),
            n_events = sum(!is.na(.data$outcome_start_date))
          ) %>%
          dplyr::mutate(incidence_start_date = .env$workingStartTime) %>%
          dplyr::mutate(incidence_end_date = .env$workingEndTime)
        } else {
          ir[[paste0(i)]] <- dplyr::tibble()
        }


        if(length(strata)>=1){
          ir[[paste0(i)]] <- ir[[paste0(i)]] %>%
            dplyr::mutate(strata_name = "Overall",
                          strata_level = "Overall")
          for(j in seq_along(strata)){
          ir[[paste0(i)]] <-  dplyr::bind_rows(ir[[paste0(i)]],
                             getStratifiedIncidenceResult(workingPop = workingPop,
                                                 workingStrata = strata[[j]],
                                                 workingStartTime = workingStartTime,
                                                 workingEndTime= workingEndTime))
          }
        }

      }
    }

    ir <- dplyr::bind_rows(ir) %>%
      dplyr::mutate(person_years = .data$person_days / 365.25) %>%
      dplyr::mutate(
        incidence_100000_pys =
          (.data$n_events / .data$person_years) * 100000
      )
  }

  # study design related variables
  analysisSettings <- dplyr::tibble(
    analysis_outcome_washout = .env$outcomeWashout,
    analysis_repeated_events = .env$repeatedEvents,
    analysis_interval = .env$interval,
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

  if (returnParticipants == TRUE) {
    # keep a table permanent one for the given analysis
    # so that we can refer back to it (e.g when using participants() function)


    studyPopDb <- studyPopDb %>%
      dplyr::select(!"outcome_prev_end_date") %>%
      dplyr::rename(
        !!paste0(
          "cohort_start_date",
          "_analysis_",
          analysisId
        ) := "cohort_start_date",
        !!paste0(
          "cohort_end_date",
          "_analysis_",
          analysisId
        ) := "cohort_end_date",
        !!paste0(
          "outcome_start_date",
          "_analysis_",
          analysisId
        ) := "outcome_start_date"
      ) %>%
      CDMConnector::computeQuery(
        name = paste0(
          tablePrefix,
          "_analysis_",
          analysisId
        ),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )
    # keep a record of the table name
    results[["person_table"]] <- paste0(
      tablePrefix,
      "_analysis_",
      analysisId
    )
  }

  return(results)
}


getStratifiedIncidenceResult <- function(workingPop, workingStrata,
                                workingStartTime, workingEndTime){
  workingPop %>%
    dplyr::group_by(dplyr::pick(.env$workingStrata)) %>%
    dplyr::summarise(
      n_persons = dplyr::n_distinct(.data$subject_id),
      person_days = sum(.data$workingDays),
      n_events = sum(!is.na(.data$outcome_start_date))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(incidence_start_date = .env$workingStartTime) %>%
    dplyr::mutate(incidence_end_date = .env$workingEndTime) %>%
    tidyr::unite("strata_level",
                 c(dplyr::all_of(.env$workingStrata)),
                 remove = FALSE, sep = " and ") %>%
    dplyr::mutate(strata_name = !!paste0(workingStrata, collapse = " and ")) %>%
    dplyr::relocate("strata_level", .after = "strata_name") %>%
    dplyr::select(!dplyr::any_of(workingStrata))
}
