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
    dplyr::select(-"cohort_definition_id") %>%
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
    CDMConnector::computeQuery()

  attrition <- recordAttrition(
    table = studyPop,
    id = "subject_id",
    reason = "Starting analysis population"
  )

  # start date
  start <- studyPop %>%
    dplyr::summarise(min(.data$cohort_start_date, na.rm = TRUE)) %>%
    dplyr::pull() %>%
    as.Date()
  # end date
  end <- studyPop %>%
    dplyr::summarise(max(.data$cohort_end_date, na.rm = TRUE)) %>%
    dplyr::pull() %>%
    as.Date()
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
    minStartDate_ <- as.character(minStartDate)
    maxStartDate_ <- as.character(maxStartDate)
    studyPop <- studyPop %>%
      dplyr::mutate(minStartDate = !!CDMConnector::asDate(.env$minStartDate_),
                    maxStartDate = !!CDMConnector::asDate(.env$maxStartDate_)) %>%
      dplyr::filter(.data$cohort_end_date >= .data$minStartDate,
                    .data$cohort_start_date <= .data$maxStartDate) %>%
      dplyr::select(-minStartDate, -maxStartDate)

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
      studyPop<- addFullContributionFlag(studyPop, studyDays)

      studyPop <- studyPop %>%
        dplyr::filter(.data$has_full_contribution >= 1) %>%
        dplyr::select(-"has_full_contribution") %>%
        CDMConnector::computeQuery()

      attrition <- recordAttrition(
        table = studyPop,
        id = "subject_id",
        reason = "Do not satisfy full contribution requirement for an interval",
        existingAttrition = attrition
      )
    } else if (fullContribution == FALSE) { # otherwise nobody excluded
      attrition <- recordAttrition(
        table = studyPop,
        id = "subject_id",
        reason = "Do not satisfy full contribution requirement for an interval",
        existingAttrition = attrition
      )
    }

    # fetch prevalence
    # looping through each time interval

    # bring in to R
    studyPopLocal <- studyPop %>% dplyr::collect()

    # pr <- list()
    pr <- vector(mode = "list", length = length(studyDays$time))

    for (i in seq_along(studyDays$time)) {
      workingStart <- studyDays$start_time[i]
      workingEnd <- studyDays$end_time[i]

      if (fullContribution == TRUE) {
        # require presence for all of period
        # drop people with end_date not after workingEnd
        # and start_date not before workingStart
        workingPop <- studyPopLocal %>%
          dplyr::filter(.data$cohort_end_date >= .env$workingEnd) %>%
          dplyr::filter(.data$cohort_start_date <= .env$workingStart)
      } else {
        # otherwise include people if they can contribute a day
        # drop people with end_date prior to workingStart
        # and start_date after workingEnd
        workingPop <- studyPopLocal %>%
          dplyr::filter(.data$cohort_end_date >= .env$workingStart) %>%
          dplyr::filter(.data$cohort_start_date <= .env$workingEnd)
      }

      workingPop <- workingPop %>%
        dplyr::mutate(
          # individuals start date for this period
          # which could be start of the period or later
          cohort_start_date =
            dplyr::if_else(.data$cohort_start_date <= .env$workingStart,
              .env$workingStart,
              as.Date(.data$cohort_start_date)
            ),
          # individuals end date for this period
          # end of the period or earlier
          cohort_end_date =
            dplyr::if_else(.data$cohort_end_date >= .env$workingEnd,
              .env$workingEnd,
              as.Date(.data$cohort_end_date)
            )
        )

      if (is.null(outcomeLookbackDays)) {
        # include any time prior
        result <- workingPop %>%
          dplyr::summarise(
            n_persons = dplyr::n_distinct(.data$subject_id),
            n_cases = dplyr::n_distinct(.data$subject_id[
              !is.na(.data$outcome_start_date) &
                .data$outcome_start_date <= .data$cohort_end_date
            ])
          )
      } else if (outcomeLookbackDays != 0) {
        # include in window using outcomeLookbackDays
        result <- workingPop %>%
          dplyr::summarise(
            n_persons = dplyr::n_distinct(.data$subject_id),
            n_cases = dplyr::n_distinct(.data$subject_id[
              !is.na(.data$outcome_start_date) &
                .data$outcome_start_date <= .data$cohort_end_date &
                .data$outcome_end_date >=
                  (.data$cohort_start_date - lubridate::days(.env$outcomeLookbackDays))
            ])
          )
      } else {
        # include ongoing in current time of interest
        result <- workingPop %>%
          dplyr::summarise(
            n_persons = dplyr::n_distinct(.data$subject_id),
            n_cases = dplyr::n_distinct(.data$subject_id[
              !is.na(.data$outcome_start_date) &
                .data$outcome_start_date <= .data$cohort_end_date &
                .data$outcome_end_date >= .data$cohort_start_date
            ])
          )
      }

      pr[[paste0(i)]] <- studyDays[i, ] %>%
        dplyr::mutate(
          n_population = result$n_persons,
          n_cases = result$n_cases
        )
    }

    pr <- dplyr::bind_rows(pr) %>%
      dplyr::mutate(prevalence = .data$n_cases / .data$n_population) %>%
      dplyr::select(
        "n_cases", "n_population",
        "prevalence", "start_time", "end_time"
      ) %>%
      dplyr::rename("prevalence_start_date" = "start_time") %>%
      dplyr::rename("prevalence_end_date" = "end_time")
  }

  results <- list()
  results[["pr"]] <- pr
  results[["person_table"]] <- studyPop
  results[["attrition"]] <- attrition

  return(results)
}



# we want to check if someone satisfies the full contribution criteria
# for at least one of the periods (of which there can be a varying number)
# because going period by period leads to clumsy sql, we want to get a
# case when statement which includes each of these

# at the momenet we´ll do this by batching and some convoluted if else logic to
# account for different batch lengths (as the last batch length is unknown)
# note, we´ll compute every 5th batch to keep the generated sql manageable

# this could be improved by generating one case when with all the periods
addFullContributionFlag <- function(workingData, workingStudyDays) {
  workingData <- workingData %>% dplyr::mutate(has_full_contribution = 0)
  # update if they do have a full contribution

  # we´re going to update the 10 (or less) intervals at a time
  startTimeBatches <- split(
    workingStudyDays$start_time,
    ceiling(seq_along(workingStudyDays$start_time) / 15)
  )
  endTimeBatches <- split(
    workingStudyDays$end_time,
    ceiling(seq_along(workingStudyDays$start_time) / 15)
  )
  for (i in seq_along(startTimeBatches)) {
    # all except the last batch should have maximum length
    # the last batch will be whatever is left over

    if (length(startTimeBatches[[i]]) == 15) {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              (.data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                 .data$cohort_start_date <= local(startTimeBatches[[i]][1])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][2])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][3])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][4])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][5])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][6])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][7]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][7])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][8]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][8])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][9]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][9])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][10]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][10])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][11]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][11])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][12]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][12])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][13]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][13])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][14]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][14])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][15]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][15])),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    } else if (length(startTimeBatches[[i]]) == 14) {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              (.data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                 .data$cohort_start_date <= local(startTimeBatches[[i]][1])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][2])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][3])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][4])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][5])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][6])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][7]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][7])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][8]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][8])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][9]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][9])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][10]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][10])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][11]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][11])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][12]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][12])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][13]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][13])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][14]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][14])),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    } else if (length(startTimeBatches[[i]]) == 13) {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              (.data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                 .data$cohort_start_date <= local(startTimeBatches[[i]][1])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][2])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][3])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][4])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][5])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][6])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][7]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][7])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][8]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][8])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][9]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][9])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][10]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][10])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][11]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][11])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][12]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][12])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][13]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][13])),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    } else if (length(startTimeBatches[[i]]) == 12) {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              (.data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                 .data$cohort_start_date <= local(startTimeBatches[[i]][1])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][2])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][3])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][4])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][5])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][6])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][7]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][7])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][8]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][8])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][9]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][9])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][10]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][10])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][11]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][11])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][12]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][12])),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    } else if (length(startTimeBatches[[i]]) == 11) {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              (.data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                 .data$cohort_start_date <= local(startTimeBatches[[i]][1])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][2])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][3])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][4])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][5])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][6])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][7]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][7])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][8]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][8])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][9]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][9])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][10]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][10])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][11]) &
                   .data$cohort_start_date <= local(startTimeBatches[[i]][11])),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    } else if (length(startTimeBatches[[i]]) == 10) {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              (.data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                .data$cohort_start_date <= local(startTimeBatches[[i]][1])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][2])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][3])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][4])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][5])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][6])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][7]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][7])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][8]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][8])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][9]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][9])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][10]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][10])),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    } else if (length(startTimeBatches[[i]]) == 9) {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              (.data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                .data$cohort_start_date <= local(startTimeBatches[[i]][1])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][2])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][3])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][4])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][5])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][6])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][7]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][7])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][8]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][8])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][9]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][9])),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    } else if (length(startTimeBatches[[i]]) == 8) {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              (.data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                .data$cohort_start_date <= local(startTimeBatches[[i]][1])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][2])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][3])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][4])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][5])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][6])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][7]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][7])) ||
                (.data$cohort_end_date >= local(endTimeBatches[[i]][8]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][8])),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    } else if (length(startTimeBatches[[i]]) == 7) {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              (.data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                .data$cohort_start_date <= local(startTimeBatches[[i]][1])) ||
                 (.data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][2])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][3])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][4])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][5])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][6])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][7]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][7])),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    } else if (length(startTimeBatches[[i]]) == 6) {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              (.data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                .data$cohort_start_date <= local(startTimeBatches[[i]][1])) ||
                 (.data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][2])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][3])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][4])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][5])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][6]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][6])),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    } else if (length(startTimeBatches[[i]]) == 5) {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              (.data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                .data$cohort_start_date <= local(startTimeBatches[[i]][1])) ||
                 (.data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][2])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][3])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][4])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][5]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][5])),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    } else if (length(startTimeBatches[[i]]) == 4) {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              (.data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                .data$cohort_start_date <= local(startTimeBatches[[i]][1])) ||
                 (.data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][2])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][3])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][4]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][4])),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    } else if (length(startTimeBatches[[i]]) == 3) {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              (.data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                .data$cohort_start_date <= local(startTimeBatches[[i]][1])) ||
                 (.data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][2])) ||
                    (.data$cohort_end_date >= local(endTimeBatches[[i]][3]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][3])),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    } else if (length(startTimeBatches[[i]]) == 2) {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              (.data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                .data$cohort_start_date <= local(startTimeBatches[[i]][1])) ||
                 (.data$cohort_end_date >= local(endTimeBatches[[i]][2]) &
                  .data$cohort_start_date <= local(startTimeBatches[[i]][2])),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    } else {
      workingData <- workingData %>%
        dplyr::mutate(
          has_full_contribution =
            dplyr::if_else(
              .data$cohort_end_date >= local(endTimeBatches[[i]][1]) &
                .data$cohort_start_date <= local(startTimeBatches[[i]][1]),
              .data$has_full_contribution + 1, .data$has_full_contribution
            )
        )
    }
    if (i %% 2 == 0) {
      workingData <- workingData %>% dplyr::compute()
    }
  }
  return(workingData)
}
