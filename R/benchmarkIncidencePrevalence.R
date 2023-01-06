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

#' Run benchmark of incidence and prevalence analyses
#'
#' @param cdm A CDM reference object
#' @param outputFolder Folder to save results as CSV
#' @param verbose Either TRUE or FALSE. If TRUE, progress will be reported.
#'
#' @return a tibble with time taken for different analyses
#' @export
#'
#' @examples
benchmarkIncidencePrevalence <- function(cdm,
                                         outputFolder = NULL,
                                         verbose = FALSE) {
  errorMessage <- checkmate::makeAssertCollection()
  cdmCheck <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdmCheck,
    add = errorMessage
  )
  if (!isTRUE(cdmCheck)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  if (!is.null(outputFolder)) {
    checkmate::assertDirectoryExists(outputFolder)
  }
  checkmate::assert_logical(verbose,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  # will add timings to list
  timings <- list()

  tictoc::tic()
  cdm$denominator_typical <- generateDenominatorCohortSet(
    cdm = cdm,
    daysPriorHistory = c(0, 180),
    sex = c("Male", "Female"),
    ageGroup = list(
      c(0, 15), c(16, 25),
      c(26, 32), c(33, 39),
      c(40, 60), c(61, 64),
      c(65, 79), c(80, 150)
    ),
    verbose = verbose
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[["typical_denominator"]] <- tibble::tibble(
    task = "generating typical denominator (32 cohorts)",
    time_taken_secs = as.numeric(t$toc - t$tic)
  )


  # add a hypothetical outcome cohort
  n_sample <- (cdm$denominator_typical %>%
    dplyr::filter(.data$cohort_definition_id == 1) %>%
    dplyr::count() %>%
    dplyr::pull()) * 0.25

  cdm$bench_outcome <- cdm$denominator_typical %>%
    dplyr::filter(.data$cohort_definition_id == 1) %>%
    utils::head(n_sample) %>%
    CDMConnector::computeQuery()

  tictoc::tic()
  point_prev_typical_years <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator_typical",
    outcomeTable = "bench_outcome",
    interval = "years",
    verbose = verbose
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[["point_prev_typical_years"]] <- tibble::tibble(
    task = "yearly point prevalence - typical denominator, one outcome",
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  tictoc::tic()
  point_prev_typical_months <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator_typical",
    outcomeTable = "bench_outcome",
    interval = "months",
    verbose = verbose
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[["point_prev_typical_months"]] <- tibble::tibble(
    task = "monthly point prevalence - typical denominator, one outcome",
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  tictoc::tic()
  period_prev_typical_years <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator_typical",
    outcomeTable = "bench_outcome",
    interval = "years",
    fullContribution = TRUE,
    verbose = verbose
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[["period_prev_typical_years"]] <- tibble::tibble(
    task = "yearly period prevalence - typical denominator, one outcome",
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  tictoc::tic()
  period_prev_typical_months <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator_typical",
    outcomeTable = "bench_outcome",
    interval = "months",
    fullContribution = TRUE,
    verbose = verbose
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[["period_prev_typical_months"]] <- tibble::tibble(
    task = "monthly period prevalence - typical denominator, one outcome",
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  tictoc::tic()
  inc_typical_years <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator_typical",
    outcomeTable = "bench_outcome",
    interval = "years",
    verbose = verbose
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[["inc_typical_years"]] <- tibble::tibble(
    task = "yearly incidence - typical denominator, one outcome",
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  tictoc::tic()
  inc_typical_months <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator_typical",
    outcomeTable = "bench_outcome",
    interval = "months",
    verbose = verbose
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[["inc_typical_months"]] <- tibble::tibble(
    task = "monthly incidence - typical denominator, one outcome",
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  # combine results
  timings <- dplyr::bind_rows(timings) %>%
    dplyr::mutate(time_taken_mins = .data$time_taken_secs / 60) %>%
    dplyr::mutate(time_taken_hours = .data$time_taken_mins / 60) %>%
    dplyr::mutate(dbms = CDMConnector::dbms(cdm)) %>%
    dplyr::mutate(person_n = cdm$person %>% dplyr::count() %>% dplyr::pull()) %>%
    dplyr::mutate(min_observation_start = cdm$observation_period %>%
      dplyr::summarise(min_obs_start = min(.data$observation_period_start_date, na.rm = TRUE)) %>%
      dplyr::pull()) %>%
    dplyr::mutate(max_observation_end = cdm$observation_period %>%
      dplyr::summarise(max_observation_end = min(.data$observation_period_end_date, na.rm = TRUE)) %>%
      dplyr::pull())

  if (!is.null(outputFolder) && dir.exists(outputFolder)) {
    utils::write.csv(timings,
      file.path(outputFolder, "IncidencePrevalenceBenchmark.csv"),
      row.names = FALSE
    )
  }

  return(timings)
}
