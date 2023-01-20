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
#' @param sample An integer for which to take a random sample when generating
#' the denominator cohort
#' @param nOutcomes An integer specifying the number of outcomes to create in
#' the denominator cohort
#' @param prevOutcomes An array of integers for the prevalence of the outcomes
#' in the population (in %). If the user wants all the outcomes with the same
#' prevalence, they can also provide a single integer
#' @param analysisType A string of the following: "all", "only incidence",
#' "only prevalence"
#' @param outputFolder Folder to save results as CSV
#' @param verbose Either TRUE or FALSE. If TRUE, progress will be reported.
#'
#' @return a tibble with time taken for different analyses
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomia_dir())
#' cdm <- CDMConnector::cdm_from_con(
#'   con = con,
#'   cdm_schema = "main"
#' )
#'
#' timings <- IncidencePrevalence::benchmarkIncidencePrevalence(cdm)
#' }
benchmarkIncidencePrevalence <- function(cdm,
                                         sample = NULL,
                                         nOutcomes = 1,
                                         prevOutcomes = 25,
                                         analysisType = "all",
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
  checkmate::assertNumeric(sample,
                           lower = 0,
                           add = errorMessage,
                           null.ok = TRUE
  )
  checkmate::assertIntegerish(nOutcomes,
                           lower = 1,
                           add = errorMessage
  )
  checkmate::assertNumeric(prevOutcomes,
                           add = errorMessage,
                           any.missing = FALSE,
                           lower = 0,
                           upper = 100
  )
  lengthpOcheck <- length(prevOutcomes) %in% c(1,nOutcomes)
  checkmate::assertTRUE(lengthpOcheck,
                        add = errorMessage
  )
  if (!isTRUE(lengthpOcheck)) {
    errorMessage$push(
      "- `prevOutcomes` is not of the expected length (either 1 or nOutcomes)"
    )
  }
  analysistypeCheck <- analysisType %in% c("all", "only incidence",
                                           "only prevalence")
  checkmate::assertTRUE(analysistypeCheck,
                        add = errorMessage
  )
  if (!isTRUE(analysistypeCheck)) {
    errorMessage$push(
      "- `analysisType` is not one of the possibilities
      ('all', 'only incidence'or 'only prevalence')"
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
    sample = sample,
    verbose = verbose
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[["typical_denominator"]] <- tibble::tibble(
    task = "generating typical denominator (32 cohorts)",
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  # change prevOutcomes to a vector, if an integer, to simplify the code
  if(length(prevOutcomes) == 1 && nOutcomes != 1) {
    prevOutcomes <- c(rep(prevOutcomes,nOutcomes))
  }

  # create table for the first outcome
  n_sample <- as.integer(cdm$denominator_typical %>%
    dplyr::count() %>%
    dplyr::pull()) * prevOutcomes[1]/100

  cdm$bench_outcome <- cdm$denominator_typical %>%
    dplyr::distinct(.data$subject_id,.keep_all=TRUE) %>%
    dplyr::slice_sample(n=n_sample) %>% dplyr::mutate(cohort_definition_id = 1)

  # add as many hypothetical outcome cohorts as required
  if(nOutcomes > 1) {
    for(i in 1:(length(prevOutcomes)-1)) {
      n_sample <- as.integer(cdm$denominator_typical %>%
                               dplyr::count() %>%
                               dplyr::pull()) * prevOutcomes[i+1]/100

      outcome_temp <- cdm$denominator_typical %>%
        dplyr::distinct(.data$subject_id,.keep_all=TRUE) %>%
        dplyr::slice_sample(n=n_sample) %>%
        dplyr::mutate(cohort_definition_id = i+1)
      cdm$bench_outcome <- cdm$bench_outcome %>%
        dplyr::full_join(outcome_temp, by = c("cohort_definition_id",
                                              "subject_id","cohort_start_date",
                                              "cohort_end_date"))
    }
  }

  # calculate prevalence if analysisType is not "only incidence"
  if(analysisType != "only incidence") {
    # point prevalence
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
      task = paste0("yearly point prevalence - typical denominator, ",nOutcomes," outcome(s)"),
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
      task = paste0("monthly point prevalence - typical denominator, ",nOutcomes," outcome(s)"),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )

    # period prevalence
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
      task = paste0("yearly period prevalence - typical denominator, ",nOutcomes," outcome(s)"),
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
      task = paste0("monthly period prevalence - typical denominator, ",nOutcomes," outcome(s)"),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )
  }

  # calculate incidence if analysisType is not "only prevalence"
  if(analysisType  != "only prevalence") {
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
      task = paste0("yearly incidence - typical denominator, ",nOutcomes," outcome(s)"),
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
      task = paste0("monthly incidence - typical denominator, ",nOutcomes," outcome(s)"),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )
  }


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
