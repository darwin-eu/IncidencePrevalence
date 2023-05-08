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
#' @param cohortDateRange Two dates. The first indicating the earliest cohort
#' start date and the second indicating the latest possible cohort end date. If
#' NULL or the first date is set as missing, the earliest observation_start_date
#' in the observation_period table will be used for the former. If  NULL or the
#' second date is set as missing, the latest observation_end_date in the
#' observation_period table will be used for the latter.
#' @param temporary If TRUE, temporary tables will be used throughout. If
#' FALSE, permanent tables will be created in the write_schema of the cdm
#' using the write_prefix (if specified). Note existing permanent tables in
#' the write schema starting with the write_prefix will be at risk of being
#' dropped or overwritten.
#' @param returnParticipants Whether to return participants (requires temporary
#' to be FALSE)
#' @param nOutcomes An integer specifying the number of outcomes to create in
#' the denominator cohort
#' @param prevOutcomes An array of integers for the prevalence of the outcomes
#' in the population (in %). If the user wants all the outcomes with the same
#' prevalence, they can also provide a single integer
#' @param analysisType A string of the following: "all", "only incidence",
#' "only prevalence"
#' @param outputFolder Folder to save results as CSV
#' @param fileName Name given to CSV with results
#'
#' @return a tibble with time taken for different analyses
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 10000,
#'                                   earliestObservationStartDate = as.Date("2010-01-01") ,
#'                                   latestObservationStartDate = as.Date("2018-01-01"))
#' timings <- IncidencePrevalence::benchmarkIncidencePrevalence(cdm)
#' }
benchmarkIncidencePrevalence <- function(cdm,
                                         cohortDateRange = NULL,
                                         temporary = TRUE,
                                         returnParticipants = FALSE,
                                         nOutcomes = 1,
                                         prevOutcomes = 0.25,
                                         analysisType = "all",
                                         outputFolder = NULL,
                                         fileName = NULL) {
  errorMessage <- checkmate::makeAssertCollection()
  cdmCheck <- inherits(cdm, "cdm_reference")
  if (!isTRUE(cdmCheck)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::assertLogical(temporary,
                             len = 1,
                             add = errorMessage
  )
  checkmate::assertIntegerish(nOutcomes,
                           lower = 0,
                           add = errorMessage
  )
  checkmate::assertNumeric(prevOutcomes,
                           add = errorMessage,
                           any.missing = FALSE,
                           lower = 0,
                           upper = 1
  )
  lengthpOcheck <- length(prevOutcomes) %in% c(1,nOutcomes)
  if (!isTRUE(lengthpOcheck)) {
    errorMessage$push(
      "- `prevOutcomes` is not of the expected length (either 1 or nOutcomes)"
    )
  }
  analysistypeCheck <- analysisType %in% c("all", "only incidence",
                                           "only prevalence")
  if (!isTRUE(analysistypeCheck)) {
    errorMessage$push(
      "- `analysisType` is not one of the possibilities
      ('all', 'only incidence'or 'only prevalence')"
    )
  }
  if (!is.null(outputFolder)) {
    checkmate::assertDirectoryExists(outputFolder)
  }
  if (!is.null(outputFolder)) {
  checkmate::assertCharacter(fileName,
                             len = 1,
                             add = errorMessage,
                             null.ok = FALSE
  )
  }
  checkmate::reportAssertions(collection = errorMessage)

  # will add timings to list
  timings <- list()

  tictoc::tic()
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator_typical",
    cohortDateRange = cohortDateRange,
    daysPriorHistory = 180,
    sex = c("Male", "Female"),
    ageGroup = list(
      c(0, 25), c(26, 64),
      c(65, 79), c(80, 150)
    ),
    temporary = temporary
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[["typical_denominator"]] <- tibble::tibble(
    task = "generating denominator (8 cohorts)",
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  # change prevOutcomes to a vector, if an integer, to simplify the code
  if(length(prevOutcomes) == 1 && nOutcomes != 1) {
    prevOutcomes <- c(rep(prevOutcomes,nOutcomes))
  }

  # create table for the first outcome
  n_sample <- as.integer(cdm$denominator_typical %>%
    dplyr::count() %>%
    dplyr::pull()) * prevOutcomes[1]

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
  cdm$bench_outcome <- addCohortCountAttr(cdm$bench_outcome)

  # calculate prevalence if analysisType is not "only incidence"
  if(analysisType != "only incidence") {
    # point prevalence
    tictoc::tic()
    point_prev_typical_years <- estimatePointPrevalence(
      cdm = cdm,
      temporary = temporary,
      returnParticipants = returnParticipants,
      denominatorTable = "denominator_typical",
      outcomeTable = "bench_outcome",
      interval = "years"
    )
    t <- tictoc::toc(quiet = TRUE)
    timings[["point_prev_typical_years"]] <- tibble::tibble(
      task = paste0("yearly point prevalence, ",nOutcomes," outcome(s)"),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )

    tictoc::tic()
    point_prev_typical_months <- estimatePointPrevalence(
      cdm = cdm,
      temporary = temporary,
      returnParticipants = returnParticipants,
      denominatorTable = "denominator_typical",
      outcomeTable = "bench_outcome",
      interval = "months"
    )
    t <- tictoc::toc(quiet = TRUE)
    timings[["point_prev_typical_months"]] <- tibble::tibble(
      task = paste0("monthly point prevalence, ",nOutcomes," outcome(s)"),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )

    # period prevalence
    tictoc::tic()
    period_prev_typical_years <- estimatePeriodPrevalence(
      cdm = cdm,
      temporary = temporary,
      returnParticipants = returnParticipants,
      denominatorTable = "denominator_typical",
      outcomeTable = "bench_outcome",
      interval = "years",
      fullContribution = TRUE
    )
    t <- tictoc::toc(quiet = TRUE)
    timings[["period_prev_typical_years"]] <- tibble::tibble(
      task = paste0("yearly period prevalence, ",nOutcomes," outcome(s)"),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )

    tictoc::tic()
    period_prev_typical_months <- estimatePeriodPrevalence(
      cdm = cdm,
      temporary = temporary,
      returnParticipants = returnParticipants,
      denominatorTable = "denominator_typical",
      outcomeTable = "bench_outcome",
      interval = "months",
      fullContribution = TRUE
    )
    t <- tictoc::toc(quiet = TRUE)
    timings[["period_prev_typical_months"]] <- tibble::tibble(
      task = paste0("monthly period prevalence, ",nOutcomes," outcome(s)"),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )
  }

  # calculate incidence if analysisType is not "only prevalence"
  if(analysisType  != "only prevalence") {
    tictoc::tic()
    inc_typical_years <- estimateIncidence(
      cdm = cdm,
      temporary = temporary,
      returnParticipants = returnParticipants,
      denominatorTable = "denominator_typical",
      outcomeTable = "bench_outcome",
      interval = "years"
    )
    t <- tictoc::toc(quiet = TRUE)
    timings[["inc_typical_years"]] <- tibble::tibble(
      task = paste0("yearly incidence, ",nOutcomes," outcome(s)"),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )

    tictoc::tic()
    inc_typical_months <- estimateIncidence(
      cdm = cdm,
      temporary = temporary,
      returnParticipants = returnParticipants,
      denominatorTable = "denominator_typical",
      outcomeTable = "bench_outcome",
      interval = "months"
    )
    t <- tictoc::toc(quiet = TRUE)
    timings[["inc_typical_months"]] <- tibble::tibble(
      task = paste0("monthly incidence, ",nOutcomes," outcome(s)"),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )
  }


  # combine results
  timings <- dplyr::bind_rows(timings) %>%
    dplyr::mutate(time_taken_secs = round(.data$time_taken_secs, 2)) %>%
    dplyr::mutate(time_taken_mins = round(.data$time_taken_secs / 60, 2)) %>%
    dplyr::mutate(time_taken_hours = round(.data$time_taken_mins / 60, 2)) %>%
    dplyr::mutate(dbms = CDMConnector::dbms(cdm)) %>%
    dplyr::mutate(person_n = cdm$person %>% dplyr::count() %>% dplyr::pull()) %>%
    dplyr::mutate(db_min_observation_start = cdm$observation_period %>%
      dplyr::summarise(db_min_obs_start = min(.data$observation_period_start_date,
                                              na.rm = TRUE)) %>%
      dplyr::pull()) %>%
    dplyr::mutate(max_observation_end = cdm$observation_period %>%
      dplyr::summarise(max_observation_end = max(.data$observation_period_end_date,
                                                 na.rm = TRUE)) %>%
      dplyr::pull())

  if(isTRUE(temporary)){
    timings <- timings %>%
      dplyr::mutate(tables = "temporary")
  } else {
    timings <- timings %>%
      dplyr::mutate(tables = "permanent")
  }

  if(isFALSE(returnParticipants)){
    timings <- timings %>%
      dplyr::mutate(with_participants = "No")
  } else {
    timings <- timings %>%
      dplyr::mutate(with_participants = "Yes")
  }

  if (!is.null(outputFolder) && dir.exists(outputFolder)) {
    utils::write.csv(timings,
      file.path(outputFolder, paste0(fileName,".csv")),
      row.names = FALSE
    )
  }

  return(timings)
}
