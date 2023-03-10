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


#' Estimate point prevalence
#'
#' @param cdm A CDM reference object
#' @param denominatorTable A cohort table with a set of denominator cohorts
#' (for example, created using the `generateDenominatorCohortSet()`
#' function).
#' @param outcomeTable A cohort table in the cdm reference containing
#' a set of outcome cohorts.
#' @param denominatorCohortId The cohort definition ids of the denominator
#' cohorts of interest. If NULL all cohorts will be considered in the
#' analysis.
#' @param outcomeCohortId The cohort definition ids of the outcome
#' cohorts of interest. If NULL all cohorts will be considered in the
#' analysis.
#' @param outcomeCohortName Corresponding names for each outcomeCohortId.
#' @param outcomeLookbackDays Days lookback when considering an outcome
#' as prevalent. If NULL any prior outcome will be considered as prevalent. If
#' 0, only ongoing outcomes will be considered as prevalent.
#' @param interval Time intervals over which period prevalence is estimated. Can
#' be "weeks", "months", "quarters", or "years". ISO weeks will
#' be used for weeks. Calendar months, quarters, or years can be used as
#' the period. If more than one option is chosen then results will
#' be estimated for each chosen interval.
#' @param timePoint where to compute the point prevalence
#' @param minCellCount Minimum number of events to report- results
#' lower than this will be obscured. If NULL all results will be reported.
#' @param tablePrefix The stem for the permanent tables that will
#' be created when running the analysis. Permanent tables will be created using
#' this prefix, and any existing tables that start with this will be at risk of
#' being dropped or overwritten. If NULL, temporary tables will be
#' used throughout.
#' @param returnParticipants Either TRUE or FALSE. If TRUE references to
#' participants from the analysis will be returned allowing for further
#' analysis. Note, if using permanent tables and returnParticipants is TRUE,
#' one table per analysis will be kept in the cdm write schema (these
#' can be dropped at subsequently using dropStemTables() function)
#' @param verbose Whether to report progress
#'
#' @return Point prevalence estimates
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here ")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' cdm$denominator <- generateDenominatorCohortSet(
#'   cdm = cdm,
#'   startDate = as.Date("2008-01-01"),
#'   endDate = as.Date("2018-01-01")
#' )
#' estimatePointPrevalence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome",
#'   interval = "months"
#' )
#' }
estimatePointPrevalence <- function(cdm,
                                    denominatorTable,
                                    outcomeTable,
                                    denominatorCohortId = NULL,
                                    outcomeCohortId = NULL,
                                    outcomeCohortName = NULL,
                                    outcomeLookbackDays = 0,
                                    interval = "years",
                                    timePoint = "start",
                                    minCellCount = 5,
                                    tablePrefix = NULL,
                                    returnParticipants = FALSE,
                                    verbose = FALSE) {

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(
    all(tolower(interval) %in%
          c(
            "weeks", "months",
            "quarters", "years"
          )),
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  estimatePrevalence(
    cdm = cdm,
    denominatorTable = denominatorTable,
    outcomeTable = outcomeTable,
    denominatorCohortId = denominatorCohortId,
    outcomeCohortId = outcomeCohortId,
    outcomeCohortName = outcomeCohortName,
    outcomeLookbackDays = outcomeLookbackDays,
    type = "point",
    interval = interval,
    completeDatabaseIntervals = FALSE,
    fullContribution = FALSE,
    timePoint = timePoint,
    minCellCount = minCellCount,
    tablePrefix = tablePrefix,
    returnParticipants =returnParticipants,
    verbose = verbose
  )
}


#' Estimate period prevalence
#'
#' @param cdm A CDM reference object
#' @param denominatorTable A cohort table with a set of denominator cohorts
#' (for example, created using the `generateDenominatorCohortSet()`
#' function).
#' @param outcomeTable A cohort table in the cdm reference containing
#' a set of outcome cohorts.
#' @param denominatorCohortId The cohort definition ids of the denominator
#' cohorts of interest. If NULL all cohorts will be considered in the
#' analysis.
#' @param outcomeCohortId The cohort definition ids of the outcome
#' cohorts of interest. If NULL all cohorts will be considered in the
#' analysis.
#' @param outcomeCohortName Corresponding names for each outcomeCohortId.
#' @param outcomeLookbackDays Days lookback when considering an outcome
#' as prevalent. If NULL any prior outcome will be considered as prevalent. If
#' 0, only ongoing outcomes will be considered as prevalent.
#' @param interval Time intervals over which period prevalence is estimated.
#' This can be "weeks", "months", "quarters", "years", or "overall".
#' ISO weeks will be used for weeks. Calendar months, quarters, or
#' years can be used as the period. If more than one option
#' is chosen then results will be estimated for each chosen interval.
#' @param completeDatabaseIntervals TRUE/ FALSE. Where TRUE, prevalence will
#' only be estimated for those intervals where the database
#' captures all the interval (based on the earliest and latest observation
#' period start dates, respectively).
#' @param fullContribution TRUE/ FALSE. Where TRUE, individuals will only be
#' included if they in the database for the entire interval of interest. If
#' FALSE they are only required to present for one day of the interval in
#' order to contribute.
#' @param minCellCount Minimum number of events to report- results
#' lower than this will be obscured. If NULL all results will be reported.
#' @param tablePrefix The stem for the permanent tables that will
#' be created when running the analysis. Permanent tables will be created using
#' this prefix, and any existing tables that start with this will be at risk of
#' being dropped or overwritten. If NULL, temporary tables will be
#' used throughout.
#' @param returnParticipants Either TRUE or FALSE. If TRUE references to
#' participants from the analysis will be returned allowing for further
#' analysis. Note, if using permanent tables and returnParticipants is TRUE,
#' one table per analysis will be kept in the cdm write schema (these
#' can be dropped at subsequently using dropStemTables() function)
#' @param verbose Whether to report progress
#'
#' @return  Period prevalence estimates
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here ")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' dpop <- generateDenominatorCohortSet(
#'   cdm = cdm,
#'   startDate = as.Date("2008-01-01"),
#'   endDate = as.Date("2018-01-01")
#' )
#' cdm$denominator <- dpop$denominator_population
#' estimatePeriodPrevalence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome",
#'   interval = "months"
#' )
#' }
estimatePeriodPrevalence <- function(cdm,
                                     denominatorTable,
                                     outcomeTable,
                                     denominatorCohortId = NULL,
                                     outcomeCohortId = NULL,
                                     outcomeCohortName = NULL,
                                     outcomeLookbackDays = 0,
                                     interval = "years",
                                     completeDatabaseIntervals = TRUE,
                                     fullContribution = FALSE,
                                     minCellCount = 5,
                                     tablePrefix = NULL,
                                     returnParticipants = FALSE,
                                     verbose = FALSE) {
  estimatePrevalence(
    cdm = cdm,
    denominatorTable = denominatorTable,
    outcomeTable = outcomeTable,
    denominatorCohortId = denominatorCohortId,
    outcomeCohortId = outcomeCohortId,
    outcomeCohortName = outcomeCohortName,
    outcomeLookbackDays = outcomeLookbackDays,
    type = "period",
    interval = interval,
    completeDatabaseIntervals = completeDatabaseIntervals,
    fullContribution = fullContribution,
    timePoint = "start",
    minCellCount = minCellCount,
    tablePrefix = tablePrefix,
    returnParticipants = returnParticipants,
    verbose = verbose
  )
}

estimatePrevalence <- function(cdm,
                               denominatorTable,
                               outcomeTable,
                               denominatorCohortId = NULL,
                               outcomeCohortId = NULL,
                               outcomeCohortName = NULL,
                               outcomeLookbackDays = 0,
                               type = "point",
                               interval = "months",
                               completeDatabaseIntervals = TRUE,
                               fullContribution = FALSE,
                               timePoint = "start",
                               minCellCount = 5,
                               tablePrefix = NULL,
                               returnParticipants = FALSE,
                               verbose = FALSE) {
  if (verbose == TRUE) {
    startCollect <- Sys.time()
  }
  # help to avoid formatting errors
  if (is.character(type)) {
    type <- tolower(type)
  }
  if (is.character(interval)) {
    interval <- tolower(interval)
  }
  if (is.character(timePoint)) {
    timePoint <- tolower(timePoint)
  }

  ## check for standard types of user error
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
  denomCheck <- denominatorTable %in% names(cdm)
  checkmate::assertTRUE(denomCheck,
    add = errorMessage
  )
  if (!isTRUE(denomCheck)) {
    errorMessage$push(
      "- `denominatorTable` is not found in cdm"
    )
  }
  checkmate::assertIntegerish(denominatorCohortId,
    add = errorMessage,
    null.ok = TRUE
  )
  outcomeCheck <- outcomeTable %in% names(cdm)
  checkmate::assertTRUE(outcomeCheck,
    add = errorMessage
  )
  if (!isTRUE(outcomeCheck)) {
    errorMessage$push(
      "- `outcomeTable` is not found in cdm"
    )
  }
  checkmate::assertIntegerish(outcomeCohortId,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assertCharacter(outcomeCohortName,
                             add = errorMessage,
                             null.ok = TRUE
  )
  checkmate::assert_numeric(outcomeLookbackDays,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assert_choice(type,
    choices = c("point", "period"),
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(interval %in%
      c(
        "weeks", "months",
        "quarters", "years",
        "overall"
      )),
    add = errorMessage
  )
  checkmate::assertTRUE(all(timePoint %in% c("start", "middle", "end")),
    add = errorMessage
  )
  checkmate::assert_number(minCellCount)
  checkmate::assert_logical(fullContribution,
    add = errorMessage
  )
  checkmate::assert_logical(verbose,
    add = errorMessage
  )
  checkmate::assert_logical(completeDatabaseIntervals,
    add = errorMessage
  )
    checkmate::assertCharacter(tablePrefix,
                               len = 1,
                               add = errorMessage,
                               null.ok = TRUE
    )
  checkmate::assert_logical(returnParticipants,
                            add = errorMessage
  )
  # report initial assertions
  checkmate::reportAssertions(collection = errorMessage)


  # if not given, use all denominator and outcome cohorts
  if (is.null(denominatorCohortId)) {
    denominatorCohortId <- cdm[[denominatorTable]] %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::pull()
  }
  if (is.null(outcomeCohortId)) {
    outcomeCohortId <- cdm[[outcomeTable]] %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::pull()
  }
  if (is.null(outcomeCohortName)) {
    outcomeCohortName <- NA
  }
  outcomeRef <- tibble::tibble(
    outcome_cohort_id = .env$outcomeCohortId,
    outcome_cohort_name = .env$outcomeCohortName
  )

  studySpecs <- tidyr::expand_grid(
    outcomeCohortId = outcomeCohortId,
    outcomeLookbackDays = outcomeLookbackDays,
    denominatorCohortId = denominatorCohortId,
    interval = interval,
    timePoint = timePoint,
    fullContribution = fullContribution,
    completeDatabaseIntervals = completeDatabaseIntervals
  )
  if (is.null(outcomeLookbackDays)) {
    studySpecs$outcomeLookbackDays <- NA
  }

  studySpecs <- studySpecs %>%
    dplyr::mutate(analysis_id = as.character(dplyr::row_number()))

  studySpecs <- split(
    studySpecs,
    studySpecs[, c("analysis_id")]
  )

  # get prs
  counter <- 0
  prsList <- lapply(studySpecs, function(x) {
    if (verbose == TRUE) {
      counter <<- counter + 1
      message(glue::glue(
        "Getting prevalence for analysis {counter} of {length(studySpecs)}"
      ))
    }
    workingPrev <- getPrevalence(
      cdm = cdm,
      denominatorTable = denominatorTable,
      denominatorCohortId = x$denominatorCohortId,
      outcomeTable = outcomeTable,
      outcomeCohortId = x$outcomeCohortId,
      outcomeLookbackDays = x$outcomeLookbackDays,
      type = type,
      interval = x$interval,
      completeDatabaseIntervals = x$completeDatabaseIntervals,
      timePoint = x$timePoint,
      fullContribution = x$fullContribution,
      tablePrefix = tablePrefix,
      returnParticipants = returnParticipants,
      analysisId = x$analysis_id
    )

    if (nrow(workingPrev[["pr"]]) == 0) {
      workingPrevPr <- tibble::tibble()
    } else {
      workingPrevPr <- workingPrev[["pr"]] %>%
        dplyr::mutate(analysis_id = x$analysis_id) %>%
        dplyr::relocate("analysis_id")
    }

    workingPrevAnalysisSettings <- tibble::tibble(
      analysis_id = x$analysis_id,
      outcome_cohort_id = x$outcomeCohortId,
      denominator_cohort_id = x$denominatorCohortId,
      analysis_outcome_lookback_days = x$outcomeLookbackDays,
      analysis_type = type,
      analysis_interval = x$interval,
      analysis_complete_database_intervals = x$completeDatabaseIntervals,
      analysis_time_point = x$timePoint,
      analysis_full_contribution = x$fullContribution,
      analysis_min_cell_count = minCellCount
    )

    workingPrevAttrition <- workingPrev[["attrition"]] %>%
      dplyr::mutate(analysis_id = x$analysis_id) %>%
      dplyr::relocate("analysis_id")

    if(returnParticipants==TRUE){
    workingPrevPersonTable <- workingPrev[["person_table"]] %>%
      dplyr::mutate(analysis_id = !!x$analysis_id) %>%
      dplyr::relocate("analysis_id")
    }

    result <- list()
    result[["pr"]] <- workingPrevPr
    result[["analysis_settings"]] <- workingPrevAnalysisSettings
    result[["attrition"]] <- workingPrevAttrition
    if(returnParticipants==TRUE){
    result[[paste0(
      "study_population_analyis_",
      x$analysis_id
    )]] <- workingPrevPersonTable }

    return(result)
  })

  prsList <- purrr::flatten(prsList)

  # analysis settings
  analysisSettings <- prsList[names(prsList) == "analysis_settings"]
  analysisSettings <- dplyr::bind_rows(analysisSettings,
    .id = NULL
  )

  analysisSettings <- analysisSettings %>%
    dplyr::left_join(
      settings(cdm[[denominatorTable]]) %>%
        dplyr::rename("cohort_id" = "cohort_definition_id") %>%
        dplyr::rename_with(
          .cols = tidyselect::everything(),
          function(x) {
            paste0("denominator_", x)
          }
        ),
      by = "denominator_cohort_id"
    )

  # attrition
  # combine analysis attrition with the previous attrition for
  # the denominator cohort used
  for (i in seq_along(studySpecs)) {
    prsList[names(prsList) == "attrition"][[i]] <- dplyr::bind_rows(
      attrition(cdm[[denominatorTable]]) %>%
        dplyr::rename("denominator_cohort_id" = "cohort_definition_id") %>%
        dplyr::filter(.data$denominator_cohort_id ==
                        studySpecs[[i]]$denominatorCohortId) %>%
        dplyr::mutate(analysis_id = studySpecs[[i]]$analysis_id),
      prsList[names(prsList) == "attrition"][[i]] %>%
        dplyr::mutate(step = "Estimating prevalence")
    )
  }

  # study population
  if(returnParticipants==TRUE){
  personTable <- prsList[stringr::str_detect(
    names(prsList),
    "study_population"
  )]
  }

  # prevalence estimates
  prs <- prsList[names(prsList) == "pr"]
  prs <- dplyr::bind_rows(prs,
    .id = NULL
  )

  # get confidence intervals
  if (nrow(prs) > 0) {
    prs <- prs %>%
      dplyr::relocate("prevalence_start_date", .after = "analysis_id") %>%
      dplyr::relocate("prevalence_end_date", .after = "prevalence_start_date")

    prs <- prs %>%
      dplyr::bind_cols(binomialCiWilson(
        prs$n_cases,
        prs$n_population
      )) %>%
      dplyr::relocate("prevalence_95CI_lower", .after = "prevalence") %>%
      dplyr::relocate("prevalence_95CI_upper", .after = "prevalence_95CI_lower")

    # obscure counts
    prs <- obscureCounts(prs,
      minCellCount = minCellCount,
      substitute = NA
    )
  }

  # attrition summary
  attrition <- prsList[names(prsList) == "attrition"]
  # to tibble
  attrition <- dplyr::bind_rows(attrition,
    .id = NULL
  ) %>%
    dplyr::select(-"denominator_cohort_id")

  # return results as an IncidencePrevalenceResult class
  attr(prs, "settings") <- analysisSettings%>%
    dplyr::left_join(outcomeRef, by = "outcome_cohort_id") %>%
    dplyr::relocate("outcome_cohort_name", .after = "outcome_cohort_id") %>%
    dplyr::relocate("denominator_cohort_id", .after = "analysis_min_cell_count")
  attr(prs, "attrition") <- attrition
  if(returnParticipants==TRUE){
  attr(prs, "participants") <- personTable
  }

  class(prs) <- c("IncidencePrevalenceResult", class(prs))

  if (verbose == TRUE) {
    dur <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
    message(glue::glue(
      "Time taken to estimate prevalence: {floor(dur/60)} mins and {dur %% 60 %/% 1} secs"
    ))
  }

  return(prs)
}



binomialCiWilson <- function(x, n) {
  alpha <- 0.05
  p <- x / n
  q <- 1 - p
  z <- stats::qnorm(1 - alpha / 2)
  t1 <- (x + z^2 / 2) / (n + z^2)
  t2 <- z * sqrt(n) / (n + z^2) * sqrt(p * q + z^2 / (4 * n))
  return(tibble::tibble(
    prevalence_95CI_lower = t1 - t2,
    prevalence_95CI_upper = t1 + t2
  ))
}
