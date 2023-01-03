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

#' Run benchmark of incidence and prevalence analyses
#'
#' @param cdm A CDM reference object
#' @param type Whether to run a "simple", "typical", or "complex" analysis
#' @param denominatorSampleSize Number for a random sample when generating
#' denominator cohorts
#' @param  outcomeTableBench A cohort table in the cdm reference containing
#' a set of outcome cohorts. All cohorts in the table will be used.
#' @param estimationInterval Interval used for estimating incidence and
#' prevalence. Can be "weeks", "months", "quarters", or "years"
#' @param verbose Either TRUE or FALSE. If TRUE, progress will be reported.
#'
#' @return a tibble with time taken for different analyses
#' @export
#'
#' @examples
benchmarkIncidencePrevalence <- function(cdm,
                                         type = "simple",
                                         denominatorSampleSize = NULL,
                                         outcomeTableBench = NULL,
                                         estimationInterval = "years",
                                         verbose = FALSE) {

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(
    all(type %in%
          c("simple", "typical", "complex")),
    add = errorMessage
  )
  checkmate::assert_number(denominatorSampleSize, null.ok = TRUE)
  outcomeCheck <- outcomeTableBench %in% names(cdm)
  checkmate::assertTRUE(outcomeCheck,
                        add = errorMessage
  )
  if (!isTRUE(outcomeCheck)) {
    errorMessage$push(
      "- `outcomeTableBench` is not found in cdm"
    )
  }
  checkmate::assertTRUE(
    all(estimationInterval %in%
          c(
            "weeks", "months",
            "quarters", "years"
          )),
    add = errorMessage
  )
  checkmate::assert_logical(verbose,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  # will add timings to list
  timings <- list()

  for(i in seq_along(type)){
    workingType<-type[[i]]
    if (verbose == TRUE) {
      message(glue::glue(" -- {workingType} study settings"))
    }
  for(j in seq_along(type)){

      workingEstimationInterval<-estimationInterval[[j]]
      if (verbose == TRUE) {
        message(glue::glue(" -- {workingEstimationInterval} study settings"))
      }

  if (verbose == TRUE) {
    message(glue::glue("Getting denominator"))
  }
  tictoc::tic()
  if (workingType == "simple") {
    cdm$denominator <- generateDenominatorCohortSet(
      cdm = cdm,
      sample = denominatorSampleSize,
      verbose = verbose
    )
  } else if (workingType == "typical") {
    cdm$denominator <- generateDenominatorCohortSet(
      cdm = cdm,
      daysPriorHistory = c(0, 180),
      sex = c("Male", "Female"),
      ageGroup = list(
        c(0, 15), c(16, 25),
        c(26, 32), c(33, 39),
        c(40, 60), c(61, 64),
        c(65, 79), c(80, 150)
      ),
      sample = denominatorSampleSize
    )
  } else {
    cdm$denominator <- generateDenominatorCohortSet(
      cdm = cdm,
      daysPriorHistory = c(0, 365),
      sex = c("Male", "Female", "Both"),
      ageGroup = list(
        c(0, 5), c(6, 10),
        c(11, 15), c(16, 20),
        c(21, 25), c(26, 30),
        c(31, 35), c(36, 40),
        c(41, 45), c(46, 50),
        c(51, 55), c(56, 60),
        c(61, 65), c(66, 70),
        c(71, 75), c(76, 150)
      ),
      sample = denominatorSampleSize
    )
  }
  t <- tictoc::toc(quiet = TRUE)
  timings[[paste0("dpop_", i,"_",j)]] <- tibble::tibble(
    type = .env$workingType,
    task =  "generating denominator",
    notes = paste0(
      "nrow ", cdm$denominator %>% dplyr::count() %>% dplyr::pull(),
      "; npeople ", cdm$denominator %>%
        dplyr::select("subject_id") %>% dplyr::distinct() %>%
        dplyr::count() %>% dplyr::pull(),
      "; min cohort start ",
      cdm$denominator %>%
        dplyr::summarise(
          min_cohort_start_date =
            min(.data$cohort_start_date, na.rm = TRUE)
        ) %>%
        dplyr::pull(), "; max cohort start ",
      cdm$denominator %>%
        dplyr::summarise(
          max_cohort_start_date =
            max(.data$cohort_start_date, na.rm = TRUE)
        ) %>%
        dplyr::pull()
    ),
    time_taken_secs = as.numeric(t$toc - t$tic),
    time_taken_mins = as.numeric(t$toc - t$tic) / 60
  )

  if (verbose == TRUE) {
    message(glue::glue("Getting point prevalence"))
  }
  tictoc::tic()
  point_prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = outcomeTableBench,
    interval = estimationInterval,
    verbose = verbose
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[[paste0("point_prev_", i,"_",j)]] <- tibble::tibble(
    type = .env$workingType,
    task = "estimating point prevalence",
    interval = .env$workingEstimationInterval,
    notes = paste0("n estimates ", nrow(point_prev)),
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  if (verbose == TRUE) {
    message(glue::glue("Getting period prevalence (full contribution not required)"))
  }
  tictoc::tic()
  period_prev1 <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = outcomeTableBench,
    interval = estimationInterval,
    fullContribution = FALSE,
    verbose = verbose
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[[paste0("period_prev1_", i,"_",j)]] <- tibble::tibble(
    type = .env$workingType,
    task = "estimating period prevalence (full contribution not required)",
    interval = .env$workingEstimationInterval,
    notes = paste0("n estimates ", nrow(period_prev1)),
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  if (verbose == TRUE) {
    message(glue::glue("Getting period prevalence (full contribution required)"))
  }
  tictoc::tic()
  period_prev2 <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = outcomeTableBench,
    interval = estimationInterval,
    fullContribution = TRUE,
    verbose = verbose
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[[paste0("period_prev2_", i,"_",j)]] <- tibble::tibble(
    type = .env$workingType,
    task = "estimating period prevalence (full contribution required)",
    interval = .env$workingEstimationInterval,
    notes = paste0("n estimates ", nrow(period_prev2)),
    time_taken_secs = as.numeric(t$toc - t$tic)
  )



  if (verbose == TRUE) {
    message(glue::glue("Getting incidence - washout all history, no repeated events"))
  }
  tictoc::tic()
  inc1 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = outcomeTableBench,
    interval = estimationInterval,
    outcomeWashout = NULL,
    repeatedEvents = FALSE,
    verbose = verbose
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[[paste0("inc1_", i,"_",j)]] <- tibble::tibble(
    type = .env$workingType,
    task = "estimating incidence - washout all history, no repeated events",
    interval = .env$workingEstimationInterval,
    notes = paste0("n estimates ", nrow(inc1)),
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  if (verbose == TRUE) {
    message(glue::glue("Getting incidence - 180 day washout, with repeated events"))
  }
  tictoc::tic()
  inc2 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = outcomeTableBench,
    interval = estimationInterval,
    outcomeWashout = 180,
    repeatedEvents = TRUE,
    verbose = verbose
  )
  t <- tictoc::toc(quiet = TRUE)
  timings[[paste0("inc2_", i,"_",j)]] <- tibble::tibble(
    type = .env$workingType,
    task = "estimating incidence - 180 day washout, with repeated events",
    interval = .env$workingEstimationInterval,
    notes = paste0("n estimates ", nrow(inc2)),
    time_taken_secs = as.numeric(t$toc - t$tic)
  )
  }}

  # combine results
  timings <- dplyr::bind_rows(timings) %>%
    dplyr::mutate(time_taken_mins = .data$time_taken_secs / 60) %>%
    dplyr::mutate(time_taken_hours = .data$time_taken_mins / 60) %>%
    dplyr::mutate(dbms = CDMConnector::dbms(cdm)) %>%
    dplyr::relocate("notes", .after = "dbms")

  return(timings)
}
