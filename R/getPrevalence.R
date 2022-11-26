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
                             fullContribution,
                             verbose) {

  if (is.na(outcomeLookbackDays)) {
    outcomeLookbackDays <- NULL
  }

  ## Analysis code
  # bring in study population
  if (verbose == TRUE) {
    message("Bringing population into memory")
  }
  # keeping outcomes of people in the denominator
  studyPop <- cdm[[denominatorTable]] %>%
    dplyr::left_join(
    cdm[[outcomeTable]] %>%
    dplyr::filter(.data$cohort_definition_id == .env$outcomeCohortId) %>%
    dplyr::rename("outcome_start_date" = "cohort_start_date") %>%
    dplyr::rename("outcome_end_date" = "cohort_end_date") %>%
    dplyr::select("subject_id", "outcome_start_date",
                  "outcome_end_date"),
    by="subject_id")

  if (interval == "days") {
    type <- "point"
  }

  # start date
  start <- studyPop %>%
    dplyr::summarise(min(.data$cohort_start_date, na.rm=TRUE)) %>%
    dplyr::pull()
  # end date
  end <- studyPop %>%
    dplyr::summarise(max(.data$cohort_end_date, na.rm=TRUE)) %>%
    dplyr::pull()
  # compute studyDays as a function of inputs
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
  } else {
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

    if(fullContribution==TRUE){
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

    denominator <- workingPop %>%
      dplyr::select("subject_id") %>%
      dplyr::distinct() %>%
      dplyr::count() %>%
      dplyr::pull()

    if(!is.null(outcomeLookbackDays)){
      numerator <- workingPop %>%
        dplyr::filter(.data$outcome_start_date <= .data$t_end_date) %>%
        dplyr::filter(.data$outcome_end_date >= (
          !!CDMConnector::dateadd("t_start_date", -{{outcomeLookbackDays}},
                                  interval = "day"))) %>%
        dplyr::select("subject_id") %>%
        dplyr::distinct() %>%
        dplyr::count() %>%
        dplyr::pull()
    } else {
      numerator <- workingPop %>%
        dplyr::filter(.data$outcome_start_date <= .data$t_end_date) %>%
        dplyr::select("subject_id") %>%
        dplyr::distinct() %>%
        dplyr::count() %>%
        dplyr::pull()
    }

    pr[[paste0(i)]] <- studyDays[i, ] %>%
      dplyr::mutate(numerator = .env$numerator) %>%
      dplyr::mutate(denominator = .env$denominator) %>%
      dplyr::mutate(prev = .env$numerator / .env$denominator) %>%
      dplyr::select("time", "numerator", "denominator",
                    "prev", "start_time", "end_time")
  }

  pr <- dplyr::bind_rows(pr)
  }

  # study design related variables
  analysisSettings <- tibble::tibble(
    type = .env$type,
    time_point = .env$timePoint,
    interval = .env$interval,
    full_contribution = .env$fullContribution,
    full_periods_required = .env$completeDatabaseIntervals
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
