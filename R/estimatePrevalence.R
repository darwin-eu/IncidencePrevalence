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
#' @param interval Time intervals over which period prevalence is estimated. Can
#' be "weeks", "months", "quarters", or "years". ISO weeks will
#' be used for weeks. Calendar months, quarters, or years can be used as
#' the period. If more than one option is chosen then results will
#' be estimated for each chosen interval.
#' @param timePoint where to compute the point prevalence
#' @param strata Variables added to the denominator cohort table for which to
#' stratify estimates.
#' @param includeOverallStrata Whether to include an overall result as well as
#' strata specific results (when strata has been specified).
#' @param minCellCount Minimum number of events to report- results
#' lower than this will be obscured. If NULL all results will be reported.
#' @param returnParticipants Either TRUE or FALSE. If TRUE references to
#' participants from the analysis will be returned allowing for further
#' analysis. Note, if using permanent tables and returnParticipants is TRUE,
#' one table per analysis will be kept in the cdm write schema.
#'
#' @return Point prevalence estimates
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
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
                                    interval = "years",
                                    timePoint = "start",
                                    strata = list(),
                                    includeOverallStrata = TRUE,
                                    minCellCount = 5,
                                    returnParticipants = FALSE) {


  if (isTRUE(returnParticipants)) {
    lifecycle::deprecate_warn(
      when = "0.8.0",
      what = "IncidencePrevalence::estimatePointPrevalence(returnParticipants)",
      details = "The returnParticipants argument will be removed in the next release"
    )
  }


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
    type = "point",
    interval = interval,
    completeDatabaseIntervals = FALSE,
    fullContribution = FALSE,
    timePoint = timePoint,
    strata = strata,
    includeOverallStrata = includeOverallStrata,
    minCellCount = minCellCount,
    returnParticipants = returnParticipants
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
#' @param strata Variables added to the denominator cohort table for which to
#' stratify estimates.
#' @param includeOverallStrata Whether to include an overall result as well as
#' strata specific results (when strata has been specified).
#' @param minCellCount Minimum number of events to report- results
#' lower than this will be obscured. If NULL all results will be reported.
#' @param returnParticipants Either TRUE or FALSE. If TRUE references to
#' participants from the analysis will be returned allowing for further
#' analysis. Note, if using permanent tables and returnParticipants is TRUE,
#' one table per analysis will be kept in the cdm write schema.
#'
#' @return  Period prevalence estimates
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
#' )
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
                                     interval = "years",
                                     completeDatabaseIntervals = TRUE,
                                     fullContribution = FALSE,
                                     strata = list(),
                                     includeOverallStrata = TRUE,
                                     minCellCount = 5,
                                     returnParticipants = FALSE) {

  if (isTRUE(returnParticipants)) {
    lifecycle::deprecate_warn(
      when = "0.8.0",
      what = "IncidencePrevalence::estimatePeriodPrevalence(returnParticipants)",
      details = "The returnParticipants argument will be removed in the next release"
    )
  }

  estimatePrevalence(
    cdm = cdm,
    denominatorTable = denominatorTable,
    outcomeTable = outcomeTable,
    denominatorCohortId = denominatorCohortId,
    outcomeCohortId = outcomeCohortId,
    type = "period",
    interval = interval,
    completeDatabaseIntervals = completeDatabaseIntervals,
    fullContribution = fullContribution,
    timePoint = "start",
    strata = strata,
    includeOverallStrata = includeOverallStrata,
    minCellCount = minCellCount,
    returnParticipants = returnParticipants
  )
}

estimatePrevalence <- function(cdm,
                               denominatorTable,
                               outcomeTable,
                               denominatorCohortId = NULL,
                               outcomeCohortId = NULL,
                               type = "point",
                               interval = "months",
                               completeDatabaseIntervals = TRUE,
                               fullContribution = FALSE,
                               timePoint = "start",
                               strata = list(),
                               includeOverallStrata = TRUE,
                               minCellCount = 5,
                               returnParticipants = FALSE) {

  summarisedResult <- FALSE

  startCollect <- Sys.time()

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

  checkInputEstimatePrevalence(
    cdm, denominatorTable, outcomeTable,
    denominatorCohortId, outcomeCohortId,
    type,
    interval, completeDatabaseIntervals,
    fullContribution, timePoint,
    minCellCount,
    returnParticipants
  )

  # if not given, use all denominator and outcome cohorts
  if (is.null(denominatorCohortId)) {
    denominatorCohortId <- omopgenerics::cohortCount(
      cdm[[denominatorTable]]) %>%
      dplyr::filter(.data$number_records > 0) %>%
      dplyr::pull("cohort_definition_id")
  }
  if (is.null(outcomeCohortId)) {
    outcomeCohortId <- omopgenerics::cohortCount(cdm[[outcomeTable]]) %>%
      dplyr::pull("cohort_definition_id")
  }

  ## add outcome from attribute
  if (is.null(outcomeCohortId)) {
    outcomeCohortId <- omopgenerics::cohortCount(cdm[[outcomeTable]]) %>% dplyr::pull("cohort_definition_id")
  }

  outcomeRef <- omopgenerics::settings(cdm[[outcomeTable]]) %>%
    dplyr::filter(.env$outcomeCohortId %in% .data$cohort_definition_id) %>%
    dplyr::collect("cohort_definition_id", "cohort_name") %>%
    dplyr::rename("outcome_cohort_id" = "cohort_definition_id",
                  "outcome_cohort_name" = "cohort_name")

  if(nrow(outcomeRef) == 0){
    cli::cli_abort(message = c("Specified outcome IDs not found in the cohort set of
                    {paste0('cdm$', outcomeTable)}",
                     "i" = "Run CDMConnector::cohort_set({paste0('cdm$', outcomeTable)})
                   to check which IDs exist"))
  }

  studySpecs <- tidyr::expand_grid(
    outcomeCohortId = outcomeCohortId,
    denominatorCohortId = denominatorCohortId,
    interval = interval,
    timePoint = timePoint,
    fullContribution = fullContribution,
    completeDatabaseIntervals = completeDatabaseIntervals
  )

  studySpecs <- studySpecs %>%
    dplyr::mutate(analysis_id = as.character(dplyr::row_number()))

  studySpecs <- split(
    studySpecs,
    studySpecs[, c("analysis_id")]
  )

  tablePrefix <- paste0(
    paste0(sample(x = letters, size = 5, replace = T), collapse = ""),
    type,
    "_prev"
  )

  # get prs
  counter <- 0
  prsList <- lapply(studySpecs, function(x) {
    counter <<- counter + 1
    message(glue::glue(
      "Getting prevalence for analysis {counter} of {length(studySpecs)}"
    ))

    workingPrev <- getPrevalence(
      cdm = cdm,
      denominatorTable = denominatorTable,
      denominatorCohortId = x$denominatorCohortId,
      outcomeTable = outcomeTable,
      outcomeCohortId = x$outcomeCohortId,
      type = type,
      interval = x$interval,
      completeDatabaseIntervals = x$completeDatabaseIntervals,
      timePoint = x$timePoint,
      fullContribution = x$fullContribution,
      tablePrefix = tablePrefix,
      returnParticipants = returnParticipants,
      analysisId = x$analysis_id,
      strata = strata,
      includeOverallStrata = includeOverallStrata
    )

    if (nrow(workingPrev[["pr"]]) == 0) {
      workingPrevPr <- dplyr::tibble()
    } else {
      workingPrevPr <- workingPrev[["pr"]] %>%
        dplyr::mutate(analysis_id = x$analysis_id) %>%
        dplyr::relocate("analysis_id")
    }

    workingPrevAnalysisSettings <- dplyr::tibble(
      analysis_id = x$analysis_id,
      outcome_cohort_id = x$outcomeCohortId,
      denominator_cohort_id = x$denominatorCohortId,
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

    result <- list()
    result[["pr"]] <- workingPrevPr
    result[["analysis_settings"]] <- workingPrevAnalysisSettings
    result[["attrition"]] <- workingPrevAttrition
    if (returnParticipants == TRUE) {
      result[[paste0(
        "study_population_analyis_",
        x$analysis_id
      )]] <- workingPrev[["person_table"]]
    }

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
      omopgenerics::settings(cdm[[denominatorTable]]) %>%
        dplyr::rename("cohort_id" = "cohort_definition_id") %>%
        dplyr::rename_with(
          .cols = dplyr::everything(),
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
      omopgenerics::attrition(cdm[[denominatorTable]]) %>%
        dplyr::rename("denominator_cohort_id" = "cohort_definition_id") %>%
        dplyr::filter(.data$denominator_cohort_id ==
                        studySpecs[[i]]$denominatorCohortId) %>%
        dplyr::mutate(analysis_id = studySpecs[[i]]$analysis_id) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.integer)),
      prsList[names(prsList) == "attrition"][[i]] %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.integer))
    )
  }

  # participants
  if (returnParticipants == TRUE) {
    participantTables <- prsList[grepl("study_population_analyis_", names(prsList))]

    # make sure to not overwrite any existing participant table (from
    # previous function calls)
    p <- 1 + length(stringr::str_subset(
      CDMConnector::listTables(attr(attr(cdm, "cdm_source"), "dbcon"),
                               schema = attr(attr(cdm, "cdm_source"), "write_Schema")
      ),
      paste0(type, "_prev_participants_")
    ))

    nm <- paste0(type, "_prev_participants_", p)
    cdm[[nm]] <- purrr::reduce(
      participantTables, dplyr::full_join, by = "subject_id"
    ) %>%
      dplyr::compute(name = nm, temporary = FALSE, overwrite = TRUE)
  }

  CDMConnector::dropTable(
    cdm = cdm,
    name = dplyr::starts_with(paste0(tablePrefix, "_analysis_"))
  )

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

    if (!summarisedResult) {
      # obscure counts
      prs <- obscureCounts(prs,
                           minCellCount = minCellCount,
                           substitute = NA
      )
    }
  }

  # attrition summary
  attrition <- prsList[names(prsList) == "attrition"]
  # to tibble
  attrition <- dplyr::bind_rows(attrition,.id = NULL) %>%
    dplyr::select(-"denominator_cohort_id") %>%
    dplyr::relocate("analysis_id")
  # obscure counts
  attrition <- obscureAttrition(attrition, minCellCount = minCellCount)

  analysisSettings <- analysisSettings %>%
    dplyr::left_join(outcomeRef, by = "outcome_cohort_id") %>%
    dplyr::relocate("outcome_cohort_name",
                    .after = "outcome_cohort_id") %>%
    dplyr::relocate("denominator_cohort_id",
                    .after = "analysis_min_cell_count") %>%
    dplyr::mutate(cdm_name = attr(cdm, "cdm_name"))


  if (!summarisedResult) {
    # add settings to estimates and attrition
    if (nrow(prs) >= 1) {
      prs <- prs %>%
        dplyr::left_join(analysisSettings, by = "analysis_id")
    }
    attrition <- attrition %>%
      dplyr::left_join(analysisSettings, by = "analysis_id")
    attr(prs, "settings") <- analysisSettings

  } else {
    # summarised result
    ## settings
    analysisSettings <- analysisSettings |>
      dplyr::mutate(
        result_id = as.integer(.data$analysis_id),
        result_type = paste0(.env$type, "_prevalence"),
        package_name = "IncidencePrevalence",
        package_version = as.character(utils::packageVersion("IncidencePrevalence"))
      ) |>
      dplyr::select(!dplyr::ends_with("_cohort_id")) |>
      dplyr::select(!dplyr::ends_with("_cohort_definition_id")) |>
      dplyr::select(c(
        "result_id", "result_type", "package_name", "package_version",
        "analysis_type", "analysis_interval",
        "analysis_complete_database_intervals", "analysis_full_contribution"),
        dplyr::starts_with("denominator_"), dplyr::starts_with("outcome_")
      )
    ## result
    if (!"strata_name" %in% colnames(prs)) {
      prs <- prs |>
        visOmopResults::uniteStrata()
    }
    prs <- prs |>
      dplyr::distinct() |>
      dplyr::mutate("analysis_id" = as.integer(.data$analysis_id)) |>
      dplyr::rename(
        "result_id" = "analysis_id",
        "outcome_count" = "n_cases",
        "denominator_count" = "n_population"
      ) |>
      dplyr::left_join(
        analysisSettings |>
          dplyr::select(c(
            "result_id", "denominator_cohort_name", "outcome_cohort_name"
          )),
        by = "result_id"
      ) |>
      visOmopResults::uniteGroup("denominator_cohort_name") |>
      visOmopResults::uniteAdditional(cols = c("prevalence_start_date", "prevalence_end_date")) |>
      visOmopResults::uniteNameLevel(
        cols = "outcome_cohort_name",
        name = "variable_name",
        level = "variable_level"
      ) |>
      tidyr::pivot_longer(
        cols = c("denominator_count", "outcome_count", "prevalence",
                 "prevalence_95CI_lower", "prevalence_95CI_upper"),
        names_to = "estimate_name",
        values_to = "estimate_value"
      ) |>
      dplyr::mutate(
        "estimate_value" = as.character(.data$estimate_value),
        "estimate_type" = dplyr::if_else(
          grepl("count", .data$estimate_name), "integer", "numeric"
        ),
        "cdm_name" = attr(cdm, "cdm_name"),
        "strata_name" = dplyr::if_else(.data$strata_name == "Overall", "overall", gsub(" and ", " &&& ", .data$strata_name)),
        "strata_level" = dplyr::if_else(.data$strata_level == "Overall", "overall", gsub(" and ", " &&& ", .data$strata_level))
      )

    prs <- omopgenerics::newSummarisedResult(prs, settings = analysisSettings) |>
      omopgenerics::suppress(minCellCount = minCellCount)

    ## attrition
    attrition <- attrition |>
      dplyr::mutate("result_id" = as.integer(.data$analysis_id)) |>
      dplyr::select(!"analysis_id") |>
      dplyr::relocate("result_id")

  }

  # return results as an IncidencePrevalenceResult class
  attr(prs, "attrition") <- attrition
  if (returnParticipants == TRUE) {
    attr(prs, "participants") <- cdm[[nm]]
  }
  class(prs) <- c("IncidencePrevalenceResult", "PrevalenceResult", class(prs))

  dur <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
  message(glue::glue(
    "Time taken: {floor(dur/60)} mins and {dur %% 60 %/% 1} secs"
  ))

  return(prs)
}

binomialCiWilson <- function(x, n) {
  alpha <- 0.05
  p <- x / n
  q <- 1 - p
  z <- stats::qnorm(1 - alpha / 2)
  t1 <- (x + z^2 / 2) / (n + z^2)
  t2 <- z * sqrt(n) / (n + z^2) * sqrt(p * q + z^2 / (4 * n))
  return(dplyr::tibble(
    prevalence_95CI_lower = t1 - t2,
    prevalence_95CI_upper = t1 + t2
  ))
}
