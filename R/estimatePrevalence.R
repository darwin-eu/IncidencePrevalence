# Copyright 2025 DARWIN EU®
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
#' @param denominatorCohortId The cohort definition ids or the cohort names of
#' the denominator cohorts of interest. If NULL all cohorts will be considered
#' in the analysis.
#' @param outcomeCohortId The cohort definition ids or the cohort names of the
#' outcome cohorts of interest. If NULL all cohorts will be considered in the
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
#'
#' @return Point prevalence estimates
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
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
                                    includeOverallStrata = TRUE) {
  omopgenerics::assertTrue(
    all(tolower(interval) %in%
      c(
        "weeks", "months",
        "quarters", "years"
      ))
  )

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
    includeOverallStrata = includeOverallStrata
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
#' @param denominatorCohortId The cohort definition ids or the cohort names of
#' the denominator cohorts of interest. If NULL all cohorts will be considered
#' in the analysis.
#' @param outcomeCohortId The cohort definition ids or the cohort names of the
#' outcome cohorts of interest. If NULL all cohorts will be considered in the
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
#'
#' @return  Period prevalence estimates
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
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
                                     includeOverallStrata = TRUE) {
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
    includeOverallStrata = includeOverallStrata
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
                               includeOverallStrata = TRUE) {
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

  cohortIds <- checkInputEstimatePrevalence(
    cdm, denominatorTable, outcomeTable,
    denominatorCohortId, outcomeCohortId,
    type,
    interval, completeDatabaseIntervals,
    fullContribution, timePoint
  )

  denominatorCohortId <- cohortIds[[1]]
  outcomeCohortId <- cohortIds[[2]]

  # if not given, use all denominator and outcome cohorts
  if (is.null(denominatorCohortId)) {
    denominatorCohortId <- omopgenerics::cohortCount(
      cdm[[denominatorTable]]
    ) %>%
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
    dplyr::filter(.data$cohort_definition_id %in% .env$outcomeCohortId) %>%
    dplyr::select("cohort_definition_id", "cohort_name") |>
    dplyr::collect() %>%
    dplyr::rename(
      "outcome_cohort_id" = "cohort_definition_id",
      "outcome_cohort_name" = "cohort_name"
    )

  if (nrow(outcomeRef) == 0) {
    cli::cli_abort(message = c("Specified outcome IDs not found in the cohort set of
                    {paste0('cdm$', outcomeTable)}",
      "i" = "Run omopgenerics::settings({paste0('cdm$', outcomeTable)})
                   to check which IDs exist"
    ))
  }

  # further checks that there are the required data elements
  checkInputEstimateAdditional(
    cdm, denominatorTable, outcomeTable, denominatorCohortId,
    outcomeCohortId
  )

  studySpecs <- tidyr::expand_grid(
    outcomeCohortId = outcomeCohortId,
    denominatorCohortId = denominatorCohortId,
    timePoint = timePoint,
    fullContribution = fullContribution,
    completeDatabaseIntervals = completeDatabaseIntervals
  )

  studySpecs <- studySpecs |>
    dplyr::mutate(
      weeks = dplyr::if_else("weeks" %in% .env$interval,
        TRUE, FALSE
      ),
      months = dplyr::if_else("months" %in% .env$interval,
        TRUE, FALSE
      ),
      quarters = dplyr::if_else("quarters" %in% .env$interval,
        TRUE, FALSE
      ),
      years = dplyr::if_else("years" %in% .env$interval,
        TRUE, FALSE
      ),
      overall = dplyr::if_else("overall" %in% .env$interval,
        TRUE, FALSE
      )
    )

  studySpecs <- studySpecs %>%
    dplyr::mutate(analysis_id = as.character(dplyr::row_number()))

  studySpecs <- split(
    studySpecs,
    studySpecs[, c("analysis_id")]
  )

  tablePrefix <- paste0(
    paste0(sample(x = letters, size = 5, replace = TRUE), collapse = ""),
    type,
    "_prev"
  )

  # get prs
  counter <- 0
  prsList <- lapply(studySpecs, function(x) {
    counter <<- counter + 1
    cli::cli_alert_info(
      "Getting prevalence for analysis {counter} of {length(studySpecs)}"
    )

    workingPrev <- getPrevalence(
      cdm = cdm,
      denominatorTable = denominatorTable,
      denominatorCohortId = x$denominatorCohortId,
      outcomeTable = outcomeTable,
      outcomeCohortId = x$outcomeCohortId,
      type = type,
      weeks = x$weeks,
      months = x$months,
      quarters = x$quarters,
      years = x$years,
      overall = x$overall,
      completeDatabaseIntervals = x$completeDatabaseIntervals,
      timePoint = x$timePoint,
      fullContribution = x$fullContribution,
      tablePrefix = tablePrefix,
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
      analysis_type = paste0(.env$type, " prevalence"),
      analysis_complete_database_intervals = x$completeDatabaseIntervals,
      analysis_time_point = x$timePoint,
      analysis_full_contribution = x$fullContribution
    )

    workingPrevAttrition <- workingPrev[["attrition"]] %>%
      dplyr::mutate(analysis_id = x$analysis_id) %>%
      dplyr::relocate("analysis_id")

    result <- list()
    result[["pr"]] <- workingPrevPr
    result[["analysis_settings"]] <- workingPrevAnalysisSettings
    result[["attrition"]] <- workingPrevAttrition

    return(result)
  })

  prsList <- purrr::list_flatten(prsList, name_spec = "{inner}")

  # analysis settings
  analysisSettings <- prsList[names(prsList) == "analysis_settings"]
  analysisSettings <- dplyr::bind_rows(analysisSettings, .id = NULL)

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
    ) %>%
    dplyr::left_join(outcomeRef, by = "outcome_cohort_id") |>
    dplyr::group_by(dplyr::across(!c(
      "analysis_id", "outcome_cohort_id", "denominator_cohort_id", "outcome_cohort_name", "denominator_cohort_name"
    ))) |>
    dplyr::mutate(result_id = as.integer(dplyr::cur_group_id())) |>
    dplyr::ungroup()

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

  omopgenerics::dropTable(
    cdm = cdm,
    name = dplyr::starts_with(paste0(tablePrefix, "_analysis_"))
  )

  # prevalence estimates
  prs <- prsList[names(prsList) == "pr"]
  prs <- dplyr::bind_rows(prs, .id = NULL)

  # get confidence intervals
  if (nrow(prs) > 0) {
    prs <- prs %>%
      dplyr::bind_cols(binomialCiWilson(
        prs$outcome_count,
        prs$denominator_count
      ))
  }

  # attrition summary
  attrition <- prsList[names(prsList) == "attrition"]
  # to tibble
  attrition <- dplyr::bind_rows(attrition, .id = NULL) %>%
    dplyr::select(-"denominator_cohort_id")

  # summarised result
  ## attrition
  attritionSR <- attrition |>
    dplyr::distinct() |>
    dplyr::left_join(
      analysisSettings |>
        dplyr::select(c(
          "analysis_id", "denominator_cohort_name", "outcome_cohort_name", "result_id"
        )),
      by = "analysis_id"
    ) |>
    dplyr::select(!"analysis_id") |>
    omopgenerics::uniteGroup(cols = c("denominator_cohort_name", "outcome_cohort_name")) |>
    tidyr::pivot_longer(
      cols = c(
        "number_records", "number_subjects", "excluded_records",
        "excluded_subjects"
      ),
      names_to = "variable_name",
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      "variable_level" = NA_character_,
      "estimate_name" = "count",
      "estimate_value" = as.character(.data$estimate_value),
      "estimate_type" = "integer",
      "cdm_name" = omopgenerics::cdmName(cdm)
    ) |>
    omopgenerics::uniteStrata("reason") |>
    omopgenerics::uniteAdditional("reason_id") |>
    dplyr::relocate(omopgenerics::resultColumns())

  ## result
  if (nrow(prs) == 0) {
    prs <- omopgenerics::emptySummarisedResult()
  } else {
    if (!"strata_name" %in% colnames(prs)) {
      prs <- prs |>
        omopgenerics::uniteStrata()
    }
    prs <- prs |>
      dplyr::distinct() |>
      dplyr::left_join(
        analysisSettings |>
          dplyr::select(c(
            "analysis_id", "denominator_cohort_name", "outcome_cohort_name", "result_id"
          )),
        by = "analysis_id"
      ) |>
      dplyr::select(!"analysis_id") |>
      dplyr::rename(
        "outcome_count" = "outcome_count",
        "denominator_count" = "denominator_count"
      ) |>
      omopgenerics::uniteGroup(cols = c("denominator_cohort_name", "outcome_cohort_name")) |>
      omopgenerics::uniteAdditional(cols = c("prevalence_start_date", "prevalence_end_date", "analysis_interval")) |>
      tidyr::pivot_longer(
        cols = c(
          "denominator_count", "outcome_count", "prevalence",
          "prevalence_95CI_lower", "prevalence_95CI_upper"
        ),
        names_to = "estimate_name",
        values_to = "estimate_value"
      ) |>
      dplyr::mutate(
        "variable_name" = dplyr::if_else(
          .data$estimate_name == "denominator_count", "Denominator", "Outcome"
        ),
        "variable_level" = NA_character_,
        "estimate_value" = as.character(.data$estimate_value),
        "estimate_type" = dplyr::if_else(
          grepl("count", .data$estimate_name), "integer", "numeric"
        ),
        "cdm_name" = omopgenerics::cdmName(cdm)
      )
  }

  ## settings
  analysisSettings <- analysisSettings |>
    dplyr::mutate(
      result_type = "prevalence",
      package_name = "IncidencePrevalence",
      package_version = as.character(utils::packageVersion("IncidencePrevalence"))
    ) |>
    dplyr::select(!dplyr::ends_with("_cohort_id")) |>
    dplyr::select(!dplyr::ends_with("_cohort_definition_id")) |>
    dplyr::select(!c("denominator_cohort_name", "outcome_cohort_name")) |>
    dplyr::select(
      c(
        "result_id", "result_type", "package_name", "package_version",
        "analysis_type",
        "analysis_complete_database_intervals", "analysis_full_contribution"
      ),
      dplyr::starts_with("denominator_"), dplyr::starts_with("outcome_")
    ) |>
    dplyr::distinct()

  prs <- omopgenerics::newSummarisedResult(
    x = prs,
    settings = analysisSettings |>
      dplyr::mutate(dplyr::across(-"result_id", as.character))
  )

  attritionSR <- attritionSR |>
    omopgenerics::newSummarisedResult(
      settings = analysisSettings |>
        dplyr::mutate(result_type = "prevalence_attrition") |>
        dplyr::mutate(dplyr::across(-"result_id", as.character))
    )

  prs <- omopgenerics::bind(prs, attritionSR)

  dur <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
  cli::cli_alert_success(
    "Time taken: {floor(dur/60)} mins and {dur %% 60 %/% 1} secs"
  )

  return(prs)
}

binomialCiWilson <- function(x, n, prefix = "prevalence") {
  alpha <- 0.05
  p <- x / n
  q <- 1 - p
  z <- stats::qnorm(1 - alpha / 2)
  t1 <- (x + z^2 / 2) / (n + z^2)
  t2 <- z * sqrt(n) / (n + z^2) * sqrt(p * q + z^2 / (4 * n))
  return(dplyr::tibble(
    !!paste0(prefix, "_95CI_lower") := t1 - t2,
    !!paste0(prefix, "_95CI_upper") := t1 + t2
  ))
}
