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

#' Run benchmark of incidence and prevalence analyses
#'
#' @param cdm A CDM reference object
#' @param analysisType A string of the following: "all", "only incidence",
#' "only prevalence"
#' @param returnParticipants Whether to return participants
#'
#' @return a tibble with time taken for different analyses
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(
#'   sampleSize = 100,
#'   earliestObservationStartDate = as.Date("2010-01-01"),
#'   latestObservationStartDate = as.Date("2010-01-01"),
#'   minDaysToObservationEnd = 364,
#'   maxDaysToObservationEnd = 364,
#'   outPre = 0.1
#' )
#'
#' timings <- benchmarkIncidencePrevalence(cdm)
#' }
benchmarkIncidencePrevalence <- function(cdm,
                                         returnParticipants = FALSE,
                                         analysisType = "all") {
  errorMessage <- checkmate::makeAssertCollection()
  cdmCheck <- inherits(cdm, "cdm_reference")
  if (!isTRUE(cdmCheck)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  analysistypeCheck <- analysisType %in% c(
    "all", "only incidence",
    "only prevalence"
  )
  if (!isTRUE(analysistypeCheck)) {
    errorMessage$push(
      "- `analysisType` is not one of the possibilities
      ('all', 'only incidence'or 'only prevalence')"
    )
  }
  checkmate::reportAssertions(collection = errorMessage)

  # will add timings to list
  timings <- list()

  tictoc::tic()
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator_typical",
    daysPriorObservation = c(0, 180),
    sex = c("Both", "Female"),
    ageGroup = list(
      c(0, 150),
      c(10, 70)
    )
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[["typical_denominator"]] <- dplyr::tibble(
    task = "generating denominator (8 cohorts)",
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  # create two outcome cohorts
  # assume an outcome prevalence of 10%
  nSample <- as.integer(ceiling(cdm$denominator_typical %>%
    dplyr::count() %>%
    dplyr::pull() * 0.1))

  # we will create two outcome cohorts
  cdm$bench_outcome <- dplyr::union_all(
    cdm$person %>%
    dplyr::select("person_id") %>%
    dplyr::distinct() %>%
    dplyr::slice_sample(n = nSample) %>%
    dplyr::left_join(cdm$observation_period,
                     by = c("person_id")) %>%
    dplyr::mutate(cohort_definition_id = 1L),
    cdm$person %>%
      dplyr::select("person_id") %>%
      dplyr::distinct() %>%
      dplyr::slice_sample(n = nSample) %>%
      dplyr::left_join(cdm$observation_period,
                       by = c("person_id")) %>%
      dplyr::mutate(cohort_definition_id = 2L)) %>%
    dplyr::select("subject_id" = "person_id",
                  "cohort_definition_id",
                  "cohort_start_date" = "observation_period_start_date",
                  "cohort_end_date" = "observation_period_end_date") %>%
    dplyr::filter(!is.na(.data$cohort_start_date) &
                  !is.na(.data$cohort_end_date)) %>%
    dplyr::compute(temporary = FALSE,
                   name = "bench_outcome") %>%
    omopgenerics::newCohortTable()

  # calculate prevalence if analysisType is not "only incidence"
  if (analysisType != "only incidence") {
    # point prevalence
    tictoc::tic()
    pointPrev <- estimatePointPrevalence(
      cdm = cdm,
      returnParticipants = returnParticipants,
      denominatorTable = "denominator_typical",
      outcomeTable = "bench_outcome",
      interval = "years"
    )
    t <- tictoc::toc(quiet = TRUE)
    timings[["pointPrev"]] <- dplyr::tibble(
      task = paste0(
        "yearly point prevalence for two outcomes with eight denominator cohorts"
        ),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )

    # period prevalence
    tictoc::tic()
    period_prev <- estimatePeriodPrevalence(
      cdm = cdm,
      returnParticipants = returnParticipants,
      denominatorTable = "denominator_typical",
      outcomeTable = "bench_outcome",
      interval = "years",
      fullContribution = TRUE
    )
    t <- tictoc::toc(quiet = TRUE)
    timings[["period_prev"]] <- dplyr::tibble(
      task = paste0(
        "yearly period prevalence for two outcomes with eight denominator cohorts"
      ),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )
  }

  # calculate incidence if analysisType is not "only prevalence"
  if (analysisType != "only prevalence") {
    tictoc::tic()
    incTypicalYears <- estimateIncidence(
      cdm = cdm,
      returnParticipants = returnParticipants,
      denominatorTable = "denominator_typical",
      outcomeTable = "bench_outcome",
      interval = "years"
    )
    t <- tictoc::toc(quiet = TRUE)
    timings[["incTypicalYears"]] <- dplyr::tibble(
      task = paste0(
        "yearly incidence for two outcomes with eight denominator cohorts"
      ),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )
  }


  # combine results
  timings <- dplyr::bind_rows(timings) %>%
    dplyr::mutate(time_taken_mins = round(.data$time_taken_secs / 60, 2)) %>%
    dplyr::mutate(dbms = attr(attr(cdm, "cdm_source"), "source_type")) %>%
    dplyr::mutate(person_n = cdm$person %>%
      dplyr::count() %>%
        dplyr::pull()) %>%
    dplyr::mutate(min_observation_start = cdm$observation_period %>%
      dplyr::summarise(
        db_min_obs_start =
          min(.data$observation_period_start_date,
            na.rm = TRUE
          )
      ) %>%
      dplyr::pull()) %>%
    dplyr::mutate(max_observation_end = cdm$observation_period %>%
      dplyr::summarise(
        max_observation_end =
          max(.data$observation_period_end_date,
            na.rm = TRUE
          )
      ) %>%
      dplyr::pull())

  if (isFALSE(returnParticipants)) {
    timings <- timings %>%
      dplyr::mutate(with_participants = "No")
  } else {
    timings <- timings %>%
      dplyr::mutate(with_participants = "Yes")
  }

  CDMConnector::dropTable(cdm = cdm,
                          name = dplyr::contains("denominator_typical"))
  CDMConnector::dropTable(cdm = cdm,
                          name = dplyr::contains("bench_outcome"))
  CDMConnector::dropTable(cdm = cdm,
                          name = dplyr::contains("point_prev_participants"))
  CDMConnector::dropTable(cdm = cdm,
                          name = dplyr::contains("period_prev_participants"))
  CDMConnector::dropTable(cdm = cdm,
                          name = dplyr::contains("inc_participants"))

  # as a summarised result
  timings <- timings %>%
    dplyr::mutate(result_id = 1L,
                  cdm_name = omopgenerics::cdmName(cdm),
                  result_type = "IncidecnePrevalence benchmark",
                  package_name = "IncidencePrevalence",
                  package_version =
                    as.character(utils::packageVersion("IncidencePrevalence")),
                  group_name = "task",
                  group_level = .data$task,
                  strata_name = "overall",
                  strata_level = "overall",
                  variable_name = "overall",
                  variable_level = "overall",
                  estimate_name = "Time taken (minutes)",
                  estimate_type = "numeric",
                  estimate_value = as.character(.data$time_taken_mins),
                  additional_name = paste0("dbms &&& person_n &&& ",
                                          "min_observation_start &&& ",
                                          "max_observation_end &&& ",
                                          "with_participants"),
                  additional_level = paste0(.data$dbms, " &&& ",
                                            .data$person_n, " &&& ",
                                            .data$min_observation_start, " &&& ",
                                            .data$max_observation_end, " &&& ",
                                            .data$with_participants)
                  ) %>%
    dplyr::select(dplyr::all_of(
      colnames(omopgenerics::emptySummarisedResult()))) %>%
    omopgenerics::newSummarisedResult()


  return(timings)
}
