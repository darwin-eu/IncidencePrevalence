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

getPrevalence <- function(cdm,
                          denominatorTable,
                          denominatorCohortId,
                          outcomeTable,
                          outcomeCohortId,
                          outcomeLookbackDays,
                          type,
                          interval,
                          completeDatabaseIntervals,
                          timePoint,
                          fullContribution) {
  if (is.na(outcomeLookbackDays)) {
    outcomeLookbackDays <- NULL
  }

  # keeping outcome of interest
  # of people in the denominator of interest
  studyPop <- cdm[[denominatorTable]] %>%
    dplyr::filter(.data$cohort_definition_id ==
                    .env$denominatorCohortId) %>%
    dplyr::left_join(
      cdm[[outcomeTable]] %>%
        dplyr::filter(.data$cohort_definition_id == .env$outcomeCohortId) %>%
        dplyr::rename("outcome_start_date" = "cohort_start_date") %>%
        dplyr::rename("outcome_end_date" = "cohort_end_date") %>%
        dplyr::select(
          "subject_id", "outcome_start_date",
          "outcome_end_date"
        ),
      by = "subject_id"
    ) %>%
    dplyr::compute()

  attrition <- recordAttrition(
    table = studyPop,
    id = "subject_id",
    reason = "Starting analysis population"
  )

  # start date
  start <- studyPop %>%
    dplyr::summarise(min(.data$cohort_start_date, na.rm = TRUE)) %>%
    dplyr::pull()
  # end date
  end <- studyPop %>%
    dplyr::summarise(max(.data$cohort_end_date, na.rm = TRUE)) %>%
    dplyr::pull()
  # get studyDays as a function of inputs
  studyDays <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    completeDatabaseIntervals = completeDatabaseIntervals,
    type = type,
    timePoint = timePoint
  )

  if (nrow(studyDays) == 0) {
    # if no study days we´ll return an empty tibble
    pr <- tibble::tibble()

    attrition <- recordAttrition(
      table = tibble::tibble(subject_id = integer()),
      id = "subject_id",
      reason = "Not observed during the complete database interval",
      existingAttrition = attrition
    )
    attrition <- recordAttrition(
      table = tibble::tibble(subject_id = integer()),
      id = "subject_id",
      reason = "Do not satisfy full contribution requirement for an interval",
      existingAttrition = attrition
    )
  } else {
    # drop for complete database intervals requirement
    minStartDate <- min(studyDays$start_time)
    maxStartDate <- max(studyDays$end_time)
    studyPop <- studyPop %>%
      dplyr::filter(.data$cohort_end_date >= minStartDate) %>%
      dplyr::filter(.data$cohort_start_date <= maxStartDate)

    attrition <- recordAttrition(
      table = studyPop,
      id = "subject_id",
      reason = "Not observed during the complete database interval",
      existingAttrition = attrition
    )

    # drop people who never fulfill fullContribution
    # go period by period
    # note we´re doing this out of the main loop below so that we can fill in
    # the attrition table
    if (fullContribution == TRUE) {
      studyPop <- studyPop %>% dplyr::mutate(has_full_contribution = 0)
      # update if they do have a full contribution
      # we´re going to update the 5 (or less) intervals at a time
      startTimeBatches <- split(
        studyDays$start_time,
        ceiling(seq_along(studyDays$start_time) / 10)
      )
      endTimeBatches <- split(
        studyDays$end_time,
        ceiling(seq_along(studyDays$start_time) / 10)
      )
      for (i in seq_along(startTimeBatches)) {
        # all except the last batch should have length 5
        # the last batch will be whatever is left over
        if(length(startTimeBatches[[i]]) == 10){
          studyPop <- studyPop %>%
            dplyr::mutate(
              has_full_contribution =
                dplyr::if_else(
                  .data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][1]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][2]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][3]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][4]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][5])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][6])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][7]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][7])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][8]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][8])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][9]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][9])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][10]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][10]),
                  .data$has_full_contribution+1, .data$has_full_contribution)
            ) %>% dplyr::compute()
        } else if (length(startTimeBatches[[i]]) == 9){
          studyPop <- studyPop %>%
            dplyr::mutate(
              has_full_contribution =
                dplyr::if_else(
                  .data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][1]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][2]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][3]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][4]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][5])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][6])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][7]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][7])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][8]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][8])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][9]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][9]),
                  .data$has_full_contribution+1, .data$has_full_contribution)
            ) %>% dplyr::compute()
        } else if (length(startTimeBatches[[i]]) == 8){
          studyPop <- studyPop %>%
            dplyr::mutate(
              has_full_contribution =
                dplyr::if_else(
                  .data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][1]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][2]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][3]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][4]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][5])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][6])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][7]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][7])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][8]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][8]),
                  .data$has_full_contribution+1, .data$has_full_contribution)
            ) %>% dplyr::compute()
        } else if (length(startTimeBatches[[i]]) == 7){
          studyPop <- studyPop %>%
            dplyr::mutate(
              has_full_contribution =
                dplyr::if_else(
                  .data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][1]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][2]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][3]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][4]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][5])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][6])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][7]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][7]),
                  .data$has_full_contribution+1, .data$has_full_contribution)
            ) %>% dplyr::compute()
        } else if (length(startTimeBatches[[i]]) == 6){
          studyPop <- studyPop %>%
            dplyr::mutate(
              has_full_contribution =
                dplyr::if_else(
                  .data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][1]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][2]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][3]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][4]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][5])||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][6]),
                  .data$has_full_contribution+1, .data$has_full_contribution)
            ) %>% dplyr::compute()
        } else if (length(startTimeBatches[[i]]) == 5){
          studyPop <- studyPop %>%
            dplyr::mutate(
              has_full_contribution =
                dplyr::if_else(
                  .data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][1]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][2]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][3]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][4]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][5]),
                  .data$has_full_contribution+1, .data$has_full_contribution)
            ) %>% dplyr::compute()
        } else if(length(startTimeBatches[[i]]) == 4) {
          studyPop <- studyPop %>%
            dplyr::mutate(
              has_full_contribution =
                dplyr::if_else(
                  .data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][1]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][2]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][3]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][4]) ,
                  .data$has_full_contribution+1, .data$has_full_contribution)
            ) %>% dplyr::compute()
        } else if(length(startTimeBatches[[i]]) == 3) {
          studyPop <- studyPop %>%
            dplyr::mutate(
              has_full_contribution =
                dplyr::if_else(
                  .data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][1]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][2]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][3]) ,
              .data$has_full_contribution+1, .data$has_full_contribution)
          ) %>% dplyr::compute()
        } else if(length(startTimeBatches[[i]]) == 2) {
          studyPop <- studyPop %>%
            dplyr::mutate(
              has_full_contribution =
                dplyr::if_else(
                  .data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][1]) ||
                    .data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][2]) ,
                  .data$has_full_contribution+1, .data$has_full_contribution)
            ) %>% dplyr::compute()
        } else {
          studyPop <- studyPop %>%
            dplyr::mutate(
              has_full_contribution =
                dplyr::if_else(
                  .data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                    .data$cohort_start_date <= local(startTimeBatches[[i]][1]) ,
                  .data$has_full_contribution+1, .data$has_full_contribution)
            ) %>% dplyr::compute()
        }
      }

      studyPop <- studyPop %>%
        dplyr::filter(.data$has_full_contribution >= 1) %>%
        dplyr::select(!"has_full_contribution") %>%
        dplyr::compute()

      attrition <- recordAttrition(
        table = studyPop,
        id = "subject_id",
        reason = "Do not satisfy full contribution requirement for an interval",
        existingAttrition = attrition
      )

    } else if (fullContribution == FALSE) { # otherwise nobody excluded
      attrition <- recordAttrition(
        table = tibble::tibble(subject_id = integer()),
        id = "subject_id",
        reason = "Do not satisfy full contribution requirement for an interval",
        existingAttrition = attrition
      )
    }

    # fetch prevalence
    # looping through each time interval

    # bring in to arrow
    # studyPop <- studyPop %>% dplyr::collect()
    studyPop <- arrow::arrow_table(studyPop %>% dplyr::collect())

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

      if (fullContribution == TRUE) {
        # require presence for all of period
        # drop people with end_date not after workingEnd
        # and start_date not before workingStart
        workingPop <- studyPop %>%
          dplyr::filter(.data$cohort_end_date >= .env$workingEnd &
            .data$cohort_start_date <= .env$workingStart)
      } else {
        # otherwise include people if they can contribute a day
        # drop people with end_date prior to workingStart
        # and start_date after workingEnd
        workingPop <- studyPop %>%
          dplyr::filter(.data$cohort_end_date >= .env$workingStart &
            .data$cohort_start_date <= .env$workingEnd)
      }

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

      nPopulation <- workingPop %>%
        dplyr::select("subject_id") %>%
        dplyr::distinct() %>%
        dplyr::collect() %>%
        dplyr::count() %>%
        dplyr::pull()

      if (is.null(outcomeLookbackDays)) {
        # include any time prior
        nCases <- workingPop %>%
          dplyr::filter(.data$outcome_start_date <= .data$t_end_date) %>%
          dplyr::select("subject_id") %>%
          dplyr::distinct() %>%
          dplyr::collect() %>%
          dplyr::count() %>%
          dplyr::pull()
      } else if (outcomeLookbackDays != 0) {
          # include in window using outcomeLookbackDays
          nCases <- workingPop %>%
            dplyr::filter(.data$outcome_start_date <= .data$t_end_date) %>%
            #collect here as lubridate days is not yet supported in arrow
            dplyr::collect() %>%
            dplyr::filter(.data$outcome_end_date >=
                            (.data$t_start_date - lubridate::days(.env$outcomeLookbackDays))) %>%
            dplyr::select("subject_id") %>%
            dplyr::distinct() %>%
            dplyr::count() %>%
            dplyr::pull()
      } else {
        # include ongoing in current time of interest
        nCases <- workingPop %>%
          dplyr::filter(.data$outcome_start_date <= .data$t_end_date  &
                        .data$outcome_end_date >= .data$t_start_date ) %>%
          dplyr::select("subject_id") %>%
          dplyr::distinct() %>%
          dplyr::collect() %>%
          dplyr::count() %>%
          dplyr::pull()
      }

      pr[[paste0(i)]] <- studyDays[i, ] %>%
        dplyr::mutate(n_cases = .env$nCases) %>%
        dplyr::mutate(n_population = .env$nPopulation) %>%
        dplyr::mutate(prevalence = .env$nCases / .env$nPopulation) %>%
        dplyr::select(
          "n_cases", "n_population",
          "prevalence", "start_time", "end_time"
        ) %>%
        dplyr::rename("prevalence_start_date" = "start_time") %>%
        dplyr::rename("prevalence_end_date" = "end_time")
    }
    pr <- dplyr::bind_rows(pr)
  }

  studyPop <- studyPop %>%
    dplyr::select("subject_id", "cohort_start_date") %>%
    dplyr::distinct()

  results <- list()
  results[["pr"]] <- pr
  results[["person_table"]] <- studyPop
  results[["attrition"]] <- attrition

  return(results)
}
