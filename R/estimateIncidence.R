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

#' Collect population incidence estimates
#'
#' @param cdm A CDM reference object
#' @param denominatorTable A cohort table with a set of denominator cohorts
#' (for example, created using the `generateDenominatorCohortSet()`
#' function).
#' @param outcomeTable A cohort table in the cdm reference containing
#' a set of outcome cohorts.
#' @param censorTable A cohort table in the cdm reference containing a cohort
#' to be used for censoring. Individuals will stop contributing time at risk
#' from the date of their first record in the censor cohort. If they appear in
#' the censor cohort before entering the denominator cohort they will be
#' excluded. The censor cohort can only contain one record per individual.
#' @param denominatorCohortId The cohort definition ids or the cohort names of
#' the denominator cohorts of interest. If NULL all cohorts will be considered
#' in the analysis.
#' @param outcomeCohortId The cohort definition ids or the cohort names of the
#' outcome cohorts of interest. If NULL all cohorts will be considered in the
#' analysis.
#' @param censorCohortId The cohort definition id or the cohort name of the
#' cohort to be used for censoring. Must be specified if there are multiple
#' cohorts in the censor table.
#' @param interval Time intervals over which incidence is estimated. Can
#' be "weeks", "months", "quarters", "years", or "overall". ISO weeks will
#' be used for weeks. Calendar months, quarters, or years can be used, or an
#' overall estimate for the entire time period observed (from earliest cohort
#' start to last cohort end) can also be estimated. If more than one option is
#' chosen then results will be estimated for each chosen interval.
#' @param completeDatabaseIntervals TRUE/ FALSE. Where TRUE, incidence will
#' only be estimated for those intervals where the denominator cohort
#' captures all the interval.
#' @param outcomeWashout The number of days used for a 'washout' period
#' between the end of one outcome and an individual starting to contribute
#' time at risk. If Inf, no time can be contributed after an event has
#' occurred.
#' @param repeatedEvents TRUE/ FALSE. If TRUE, an individual will be able to
#' contribute multiple events during the study period (time while they are
#' present in an outcome cohort and any subsequent washout will be
#' excluded). If FALSE, an individual will only contribute time up to their
#' first event.
#' @param strata Variables added to the denominator cohort table for which to
#' stratify estimates.
#' @param includeOverallStrata Whether to include an overall result as well as
#' strata specific results (when strata has been specified).
#' @param rateDenominator The denominator to use for the incidence rate
#' calculation. Default is 100000.
#'
#' @return Incidence estimates
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
#' )
#' inc <- estimateIncidence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' }
estimateIncidence <- function(cdm,
                              denominatorTable,
                              outcomeTable,
                              censorTable = NULL,
                              denominatorCohortId = NULL,
                              outcomeCohortId = NULL,
                              censorCohortId = NULL,
                              interval = "years",
                              completeDatabaseIntervals = TRUE,
                              outcomeWashout = Inf,
                              repeatedEvents = FALSE,
                              strata = list(),
                              includeOverallStrata = TRUE,
                              rateDenominator = 100000) {
  startCollect <- Sys.time()

  tablePrefix <- paste0(
    sample(letters, 5, TRUE) |> paste0(collapse = ""), "_inc"
  )

  # help to avoid formatting errors
  if (is.character(interval)) {
    interval <- tolower(interval)
  }
  rateDenominator <- as.integer(rateDenominator)

  cohortIds <- checkInputEstimateIncidence(
    cdm, denominatorTable, outcomeTable, censorTable,
    denominatorCohortId, outcomeCohortId, censorCohortId,
    interval, completeDatabaseIntervals,
    outcomeWashout, repeatedEvents
  )
  denominatorCohortId <- cohortIds[[1]]
  outcomeCohortId <- cohortIds[[2]]
  censorCohortId <- cohortIds[[3]]

  checkStrata(strata, cdm[[denominatorTable]])

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

  if (denominatorTable == outcomeTable &&
    any(denominatorCohortId %in% outcomeCohortId)) {
    cli::cli_abort("Denominator cohort can not be the same as the outcome cohort")
  }

  ## add outcome from attribute
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

  # get outcomes + cohort_start_date & cohort_end_date
  cdm[[paste0(tablePrefix, "_inc_1")]] <- cdm[[outcomeTable]] %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$outcomeCohortId) %>%
    dplyr::rename(
      "outcome_cohort_id" = "cohort_definition_id",
      "outcome_start_date" = "cohort_start_date",
      "outcome_end_date" = "cohort_end_date"
    ) %>%
    dplyr::inner_join(
      cdm[[denominatorTable]] %>%
        dplyr::filter(.data$cohort_definition_id %in%
          .env$denominatorCohortId) %>%
        dplyr::select(c(
          "subject_id", "cohort_start_date",
          "cohort_end_date"
        )) %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::compute(
      name = paste0(tablePrefix, "_inc_1"),
      temporary = FALSE,
      overwrite = TRUE,
      logPrefix = "IncidencePrevalence_estimateIncidence_cohort_outcomes_"
    )

  cdm[[paste0(tablePrefix, "_inc_2")]] <- cdm[[paste0(tablePrefix, "_inc_1")]] %>%
    # most recent outcome starting before cohort start per person
    dplyr::filter(.data$outcome_start_date < .data$cohort_start_date) %>%
    dplyr::compute(
      name = paste0(tablePrefix, "_inc_2"),
      temporary = FALSE,
      overwrite = TRUE,
      logPrefix = "IncidencePrevalence_estimateIncidence_most_recent_"
    )

  if (nrow(cdm[[paste0(tablePrefix, "_inc_2")]] |>
    utils::head(1) |>
    dplyr::collect()) > 0) {
    cdm[[paste0(tablePrefix, "_inc_2")]] <- cdm[[paste0(tablePrefix, "_inc_2")]] %>%
      dplyr::group_by(
        .data$subject_id,
        .data$cohort_start_date,
        .data$outcome_cohort_id
      ) %>%
      dplyr::filter(.data$outcome_start_date ==
        max(.data$outcome_start_date, na.rm = TRUE)) %>%
      dplyr::compute(
        name = paste0(tablePrefix, "_inc_2"),
        temporary = FALSE,
        overwrite = TRUE,
        logPrefix = "IncidencePrevalence_estimateIncidence_most_recent_2_"
      )
  }

  cdm[[paste0(tablePrefix, "_inc_2")]] <- cdm[[paste0(tablePrefix, "_inc_2")]] %>%
    dplyr::union_all(
      # all starting during cohort period
      cdm[[paste0(tablePrefix, "_inc_1")]] %>%
        dplyr::filter(.data$outcome_start_date >= .data$cohort_start_date) %>%
        dplyr::filter(.data$outcome_start_date <= .data$cohort_end_date)
    ) %>%
    dplyr::compute(
      name = paste0(tablePrefix, "_inc_2"),
      temporary = FALSE,
      overwrite = TRUE,
      logPrefix = "IncidencePrevalence_estimateIncidence_during_"
    )

  cdm[[paste0(tablePrefix, "_inc_3")]] <- cdm[[paste0(tablePrefix, "_inc_2")]] %>%
    dplyr::group_by(
      .data$subject_id,
      .data$cohort_start_date,
      .data$cohort_end_date,
      .data$outcome_cohort_id
    ) %>%
    dplyr::arrange(.data$outcome_start_date) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute(
      name = paste0(tablePrefix, "_inc_3"),
      temporary = FALSE,
      overwrite = TRUE,
      logPrefix = "IncidencePrevalence_estimateIncidence_index_"
    )

  cdm[[paste0(tablePrefix, "_inc_4")]] <- cdm[[paste0(tablePrefix, "_inc_3")]] %>%
    dplyr::select(-"outcome_end_date") %>%
    dplyr::full_join(
      cdm[[paste0(tablePrefix, "_inc_3")]] %>%
        dplyr::mutate(index = .data$index + 1) %>%
        dplyr::rename("outcome_prev_end_date" = "outcome_end_date") %>%
        dplyr::select(-"outcome_start_date"),
      by = c(
        "subject_id", "cohort_start_date",
        "cohort_end_date", "outcome_cohort_id", "index"
      )
    ) %>%
    dplyr::select(-"index") %>%
    dplyr::compute(
      name = paste0(tablePrefix, "_inc_4"),
      temporary = FALSE,
      overwrite = TRUE,
      logPrefix = "IncidencePrevalence_estimateIncidence_full_join_"
    )

  studySpecs <- getIncAnalysisSpecs(outcomeCohortId = outcomeCohortId,
                      denominatorCohortId = denominatorCohortId,
                      completeDatabaseIntervals = completeDatabaseIntervals,
                      outcomeWashout = outcomeWashout,
                      repeatedEvents = repeatedEvents,
                      interval = interval)

  # get irs
  counter <- 0
  irsList <- purrr::map(studySpecs, function(x) {
    counter <<- counter + 1
    cli::cli_alert_info(
      "Getting incidence for analysis {counter} of {length(studySpecs)}"
    )
    workingInc <- getIncidence(
      cdm = cdm,
      denominatorTable = denominatorTable,
      denominatorCohortId = x$denominator_cohort_id,
      outcomeTable = paste0(tablePrefix, "_inc_4"),
      outcomeCohortId = x$outcome_cohort_id,
      censorTable = censorTable,
      censorCohortId = censorCohortId,
      weeks = x$weeks,
      months = x$months,
      quarters = x$quarters,
      years = x$years,
      overall = x$overall,
      completeDatabaseIntervals = x$complete_database_intervals,
      outcomeWashout = x$outcome_washout,
      repeatedEvents = x$repeated_events,
      tablePrefix = tablePrefix,
      analysisId = x$analysis_id,
      strata = strata,
      includeOverallStrata = includeOverallStrata,
      rateDenominator = rateDenominator
    )

    workingIncIr <- workingInc[["ir"]] %>%
      dplyr::mutate(analysis_id = x$analysis_id) %>%
      dplyr::relocate("analysis_id")

    workingIncAnalysisSettings <- workingInc[["analysis_settings"]] %>%
      dplyr::mutate(
        outcome_cohort_id = x$outcome_cohort_id,
        denominator_cohort_id = x$denominator_cohort_id,
        analysis_id = x$analysis_id
      ) %>%
      dplyr::relocate("analysis_id") %>%
      dplyr::mutate(analysis_outcome_washout = as.character(.data$analysis_outcome_washout))

    # attrition is the same for each interval
    workingIncAttrition <- workingInc[["attrition"]] %>%
      dplyr::mutate(analysis_id = x$analysis_id) %>%
      dplyr::relocate("analysis_id")

    result <- list()
    result[["ir"]] <- workingIncIr
    result[["analysis_settings"]] <- workingIncAnalysisSettings
    result[["attrition"]] <- workingIncAttrition


    return(result)
  })

  irsList <- purrr::list_flatten(irsList, name_spec = "{inner}")

  # analysis settings
  analysisSettings <- irsList[names(irsList) == "analysis_settings"]
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
    ) |>
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
    irsList[names(irsList) == "attrition"][[i]] <- dplyr::bind_rows(
      omopgenerics::attrition(cdm[[denominatorTable]]) %>%
        dplyr::rename("denominator_cohort_id" = "cohort_definition_id") %>%
        dplyr::filter(.data$denominator_cohort_id ==
          studySpecs[[i]]$denominator_cohort_id) %>%
        dplyr::mutate(analysis_id = studySpecs[[i]]$analysis_id) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.integer)),
      irsList[names(irsList) == "attrition"][[i]] %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.integer))
    )
  }
  attrition <- irsList[names(irsList) == "attrition"]
  attrition <- dplyr::bind_rows(attrition, .id = NULL) %>%
    dplyr::select(-"denominator_cohort_id") %>%
    dplyr::relocate("analysis_id")

  # incidence estimates
  irs <- irsList[names(irsList) == "ir"]
  # to tibble
  irs <- dplyr::bind_rows(irs, .id = NULL)

  # get confidence intervals
  if (nrow(irs) > 0) {
    irs <- irs %>%
      dplyr::bind_cols(
        incRateCiExact(irs$outcome_count, irs$person_years, rateDenominator)
      )
  }

  omopgenerics::dropSourceTable(
    cdm = cdm,
    name = dplyr::starts_with(paste0(tablePrefix, "_inc_"))
  )
  omopgenerics::dropSourceTable(
    cdm = cdm,
    name = dplyr::starts_with(paste0(tablePrefix, "_analysis_"))
  )

  ## attrition
  attritionSR <- attrition |>
    dplyr::distinct() |>
    dplyr::inner_join(
      analysisSettings |>
        dplyr::select(c("analysis_id", "denominator_cohort_name", "outcome_cohort_name", "result_id")),
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
      "estimate_name" = "count",
      "estimate_value" = as.character(.data$estimate_value),
      "estimate_type" = "integer",
      "variable_level" = NA_character_,
      "cdm_name" = omopgenerics::cdmName(cdm)
    ) |>
    omopgenerics::uniteStrata("reason") |>
    omopgenerics::uniteAdditional("reason_id") |>
    dplyr::relocate(omopgenerics::resultColumns())

  ## result
  if (nrow(irs) == 0) {
    irs <- omopgenerics::emptySummarisedResult()
  } else {
    if (!"strata_name" %in% colnames(irs)) {
      irs <- irs |>
        omopgenerics::uniteStrata()
    }
    irs <- irs |>
      dplyr::distinct() |>
      dplyr::inner_join(
        analysisSettings |>
          dplyr::select(c("analysis_id", "denominator_cohort_name", "outcome_cohort_name", "result_id")),
        by = "analysis_id"
      ) |>
      dplyr::select(!"analysis_id") |>
      omopgenerics::uniteGroup(cols = c("denominator_cohort_name", "outcome_cohort_name")) |>
      omopgenerics::uniteAdditional(cols = c("incidence_start_date", "incidence_end_date", "analysis_interval")) |>
      tidyr::pivot_longer(
        cols = c(
          "denominator_count", "outcome_count", "person_days", "person_years",
          paste0("incidence_", rateDenominator, "_pys"),
          paste0("incidence_", rateDenominator, "_pys_95CI_lower"),
          paste0("incidence_", rateDenominator, "_pys_95CI_upper")
        ),
        names_to = "estimate_name",
        values_to = "estimate_value"
      ) |>
      dplyr::mutate(
        "variable_name" = dplyr::if_else(
          .data$estimate_name %in% c("denominator_count", "person_days", "person_years"),
          "Denominator", "Outcome"
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
      result_type = "incidence",
      package_name = "IncidencePrevalence",
      package_version = as.character(utils::packageVersion("IncidencePrevalence"))
    ) |>
    dplyr::select(!dplyr::ends_with("_cohort_id")) |>
    dplyr::select(!dplyr::ends_with("_cohort_definition_id")) |>
    dplyr::select(!c("denominator_cohort_name", "outcome_cohort_name")) |>
    dplyr::select(
      c(
        "result_id", "result_type", "package_name", "package_version",
        "analysis_outcome_washout", "analysis_repeated_events",
        "analysis_complete_database_intervals", "analysis_censor_cohort_name"
      ),
      dplyr::starts_with("denominator_"), dplyr::starts_with("outcome_")
    ) |>
    dplyr::distinct()

  ## summarised result
  irs <- omopgenerics::newSummarisedResult(
    x = irs,
    settings = analysisSettings |>
      dplyr::mutate(dplyr::across(-"result_id", as.character))
  )

  attritionSR <- attritionSR |>
    omopgenerics::newSummarisedResult(
      settings = analysisSettings |>
        dplyr::mutate(result_type = "incidence_attrition") |>
        dplyr::mutate(dplyr::across(-"result_id", as.character))
    )

  irs <- omopgenerics::bind(irs, attritionSR)

  dur <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
  cli::cli_alert_success(
    "Overall time taken: {floor(dur/60)} mins and {dur %% 60 %/% 1} secs"
  )

  return(irs)
}



incRateCiExact <- function(ev, pt, rateDenominator) {
  rateDenominator <- as.integer(rateDenominator)
  return(dplyr::tibble(
    lower =
      ((stats::qchisq(p = 0.025, df = 2 * ev) / 2) / pt) * rateDenominator,
    upper =
      ((stats::qchisq(p = 0.975, df = 2 * (ev + 1)) / 2) / pt) * rateDenominator
  ) |>
    dplyr::mutate(lower = round(.data$lower, 3),
                  upper = round(.data$upper, 3)) |>
    dplyr::rename(
      !!paste0("incidence_", rateDenominator, "_pys_95CI_lower") := "lower",
      !!paste0("incidence_", rateDenominator, "_pys_95CI_upper") := "upper"
    )
  )
}

# returns a tibble with one row per analysis specification
getIncAnalysisSpecs <- function(outcomeCohortId,
                                denominatorCohortId,
                                completeDatabaseIntervals,
                                outcomeWashout,
                                repeatedEvents,
                                interval) {
  studySpecs <- tidyr::expand_grid(
    outcome_cohort_id = outcomeCohortId,
    denominator_cohort_id = denominatorCohortId,
    complete_database_intervals = completeDatabaseIntervals,
    outcome_washout = outcomeWashout,
    repeated_events = repeatedEvents
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
  if (any(is.infinite(outcomeWashout))) {
    studySpecs$outcome_washout[
      which(is.infinite(studySpecs$outcome_washout))
    ] <- NA
  }
  studySpecs <- studySpecs %>%
    dplyr::mutate(analysis_id = as.character(dplyr::row_number()))
  studySpecs <- split(
    studySpecs,
    studySpecs[, c("analysis_id")]
  )

  studySpecs
}
