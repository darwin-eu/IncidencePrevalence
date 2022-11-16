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
#' @param denominatorId denominatorId
#' @param outcomeTable outcomeTable
#' @param outcomeId outcomeId
#' @param type type
#' @param interval interval
#' @param fullPeriods full period requirement
#' @param point point where to compute prevalence inside interval
#' @param fullContribution fullContribution
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
getPopPrevalence <- function(cdm,
                             denominatorTable,
                             denominatorId,
                             outcomeTable,
                             outcomeId,
                             type,
                             interval,
                             fullPeriods,
                             point,
                             fullContribution,
                             verbose) {
  ## Analysis code
  # bring in study population
  if (verbose == TRUE) {
    message("Bringing population into memory")
  }
  # keeping outcomes of people in the denominator
  studyPop <- cdm[[denominatorTable]] %>%
    dplyr::left_join(
    cdm[[outcomeTable]] %>%
    dplyr::filter(.data$cohort_definition_id == .env$outcomeId) %>%
    dplyr::rename("outcome_start_date" = "cohort_start_date") %>%
    dplyr::rename("outcome_end_date" = "cohort_end_date") %>%
    dplyr::select("subject_id", "outcome_start_date",
                  "outcome_end_date"),
    by="subject_id") %>%
    dplyr::collect()

  if (interval == "days") {
    type <- "point"
  }

  # start date
  start <- min(studyPop$cohort_start_date)
  # end date
  end <- max(studyPop$cohort_end_date)
  # compute studyDays as a function of inputs
  studyDays <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    fullPeriods = fullPeriods,
    type = type,
    point = point
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
      nrow()

    numerator <- workingPop %>%
      dplyr::filter(.data$outcome_start_date <= .data$t_end_date) %>%
      dplyr::filter(.data$outcome_end_date >= .data$t_start_date) %>%
      dplyr::select("subject_id") %>%
      dplyr::distinct() %>%
      nrow()

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
    point = .env$point,
    interval = .env$interval,
    full_contribution = .env$fullContribution,
    full_periods_required = .env$fullPeriods
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
