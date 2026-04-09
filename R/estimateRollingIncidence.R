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

#' Collect rolling population incidence estimates
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
#' @param interval Time intervals over which rolling incidence is estimated. Can
#' be "weeks", "months", "quarters", or "years". ISO weeks will
#' be used for weeks. Calendar months, quarters, or years can be used. If more 
#' than one option is chosen then results will be estimated for each chosen interval.
#' @param nIntervals The total number of relative-time windows to estimate (e.g., 
#' 12 means 12 windows of the specified interval).
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
#' @return Rolling incidence estimates
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
#' )
#' rolling_inc <- estimateRollingIncidence(
#'   cdm               = cdm,
#'   denominatorTable  = "denominator",
#'   outcomeTable      = "outcome",
#'   interval          = "months",
#'   nIntervals        = 12
#' )
#' }
estimateRollingIncidence <- function(cdm,
                                     denominatorTable,
                                     outcomeTable,
                                     denominatorCohortId = NULL,
                                     outcomeCohortId = NULL,
                                     interval = "months",
                                     nIntervals = 12,
                                     outcomeWashout = Inf,
                                     repeatedEvents = FALSE,
                                     strata = list(),
                                     includeOverallStrata = TRUE,
                                     rateDenominator = 100000) {
  startCollect <- Sys.time()

  tablePrefix <- paste0(
    sample(letters, 5, TRUE) |> paste0(collapse = ""), "_inc"
  )

  if (is.character(interval)) {
    interval <- tolower(interval)
  }
  rateDenominator <- as.integer(rateDenominator)

  cohortIds <- checkInputEstimateRollingIncidence(
    cdm, denominatorTable, outcomeTable,
    denominatorCohortId, outcomeCohortId,
    interval, nIntervals,
    outcomeWashout, repeatedEvents
  )
  denominatorCohortId <- cohortIds[[1]]
  outcomeCohortId <- cohortIds[[2]]

  checkStrata(strata, cdm[[denominatorTable]])

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
      logPrefix = "IncidencePrevalence_estimateRollingIncidence_cohort_outcomes_"
    )

  cdm[[paste0(tablePrefix, "_inc_2")]] <- cdm[[paste0(tablePrefix, "_inc_1")]] %>%
    # most recent outcome starting before cohort start per person
    dplyr::filter(.data$outcome_start_date < .data$cohort_start_date) %>%
    dplyr::compute(
      name = paste0(tablePrefix, "_inc_2"),
      temporary = FALSE,
      overwrite = TRUE,
      logPrefix = "IncidencePrevalence_estimateRollingIncidence_most_recent_"
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
        logPrefix = "IncidencePrevalence_estimateRollingIncidence_most_recent_2_"
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
      logPrefix = "IncidencePrevalence_estimateRollingIncidence_during_"
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
      logPrefix = "IncidencePrevalence_estimateRollingIncidence_index_"
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
      logPrefix = "IncidencePrevalence_estimateRollingIncidence_full_join_"
    )

  studySpecs <- tidyr::expand_grid(
    outcome_cohort_id = outcomeCohortId,
    denominator_cohort_id = denominatorCohortId,
    outcome_washout = outcomeWashout,
    repeated_events = repeatedEvents,
    interval = interval
  )
  if (any(is.infinite(outcomeWashout))) {
    studySpecs$outcome_washout[
      which(is.infinite(studySpecs$outcome_washout))
    ] <- NA
  }
  studySpecs <- studySpecs %>%
    dplyr::mutate(analysis_id = as.character(dplyr::row_number()))
  studySpecsList <- split(
    studySpecs,
    studySpecs[, c("analysis_id")]
  )

  counter <- 0
  irsList <- purrr::map(studySpecsList, function(x) {
    counter <<- counter + 1
    cli::cli_alert_info(
      "Getting rolling incidence for analysis {counter} of {length(studySpecsList)}"
    )

    denId <- x$denominator_cohort_id
    outId <- x$outcome_cohort_id
    analysisId <- x$analysis_id
    washout <- x$outcome_washout
    if (!is.null(washout) && is.na(washout)) {
      washout <- NULL
    }
    events <- x$repeated_events
    intvl <- x$interval
    
    # Days per interval logic
    if (intvl == "weeks") {
      days_in_interval <- 7
    } else if (intvl == "months") {
      days_in_interval <- 30
    } else if (intvl == "quarters") {
      days_in_interval <- 90
    } else if (intvl == "years") {
      days_in_interval <- 365
    } else {
      cli::cli_abort("Interval {intvl} not supported for rolling incidence. Use weeks, months, quarters, or years.")
    }

    studyPopStart <- cdm[[denominatorTable]] %>%
      dplyr::filter(.data$cohort_definition_id == .env$denId) %>%
      dplyr::select(-"cohort_definition_id") %>%
      dplyr::compute(
        name = paste0(tablePrefix, "_inc_5a"),
        temporary = FALSE,
        overwrite = TRUE,
        logPrefix = "IncidencePrevalence_estimateRollingIncidence_denominator_"
      )

    attrition <- recordAttrition(
      table = studyPopStart,
      id = "subject_id",
      reasonId = 11,
      reason = "Starting analysis population"
    )

    studyPop <- studyPopStart |>
      dplyr::left_join(
        cdm[[paste0(tablePrefix, "_inc_4")]] %>%
          dplyr::filter(.data$outcome_cohort_id == .env$outId) %>%
          dplyr::select(-"outcome_cohort_id"),
        by = c("subject_id", "cohort_start_date", "cohort_end_date")
      ) %>%
      dplyr::compute(
        name = paste0(tablePrefix, "_inc_5b"),
        temporary = FALSE,
        overwrite = TRUE,
        logPrefix = "IncidencePrevalence_estimateRollingIncidence_outcomes_"
      )

    studyPopNoOutcome <- studyPop %>%
      dplyr::filter(is.na(.data$outcome_start_date) & is.na(.data$outcome_prev_end_date)) %>%
      dplyr::mutate(risk_start_date = .data$cohort_start_date)

    studyPopOutcome <- studyPop %>%
      dplyr::filter(!is.na(.data$outcome_start_date) | !is.na(.data$outcome_prev_end_date)) %>%
      dplyr::mutate(cohort_end_date = dplyr::coalesce(.data$outcome_start_date, .data$cohort_end_date)) %>%
      dplyr::mutate(risk_start_date = .data$cohort_start_date)

    nStudyPopOutcome <- studyPopOutcome %>% utils::head(10) %>% dplyr::tally() %>% dplyr::pull("n")

    if (nStudyPopOutcome > 0) {
      if (is.null(washout)) {
        studyPopOutcome <- studyPopOutcome %>%
          dplyr::filter(is.na(.data$outcome_prev_end_date) & .data$risk_start_date <= .data$cohort_end_date)
      } else {
        washoutPlusOne <- as.integer(washout + 1)
        
        # We need to iteratively filter the events to only keep valid ones,
        # and base the next risk_start_date ONLY on the end date of the previous VALID event.
        
        studyPopOutcome_local <- studyPopOutcome %>%
          dplyr::collect() %>%
          dplyr::arrange(.data$subject_id, .data$outcome_start_date)
        
        if (nrow(studyPopOutcome_local) > 0) {
          # Split by subject_id for faster sequential processing
          split_pop <- split(studyPopOutcome_local, studyPopOutcome_local$subject_id)
          
          valid_pop_list <- lapply(split_pop, function(df) {
            valid_rows <- list()
            current_valid_end <- as.Date(NA)
            
            for (i in seq_len(nrow(df))) {
              row <- df[i, ]
              
              if (i == 1) {
                # First event is always valid if it's the first in the cohort period
                # (Pre-cohort events are handled by outcome_prev_end_date from inc_1/inc_2)
                if (!is.na(row$outcome_prev_end_date)) {
                   row$risk_start_date <- as.Date(row$outcome_prev_end_date) + washoutPlusOne
                }
                
                # Check if this first event is pushed outside the cohort
                if (row$risk_start_date <= row$cohort_end_date) {
                  valid_rows[[length(valid_rows) + 1]] <- row
                  if (!is.na(row$outcome_start_date)) {
                    current_valid_end <- row$outcome_start_date
                  }
                }
              } else {
                # Subsequent events: check against the LAST VALID event's end date
                if (!is.na(current_valid_end)) {
                   required_start <- current_valid_end + washoutPlusOne
                   if (!is.na(row$outcome_start_date) && row$outcome_start_date >= required_start) {
                     row$risk_start_date <- required_start
                     if (row$risk_start_date <= row$cohort_end_date) {
                       valid_rows[[length(valid_rows) + 1]] <- row
                       current_valid_end <- row$outcome_start_date
                     }
                   }
                } else {
                   # If we somehow kept a row without an outcome (e.g. no more events), just append
                   if (row$risk_start_date <= row$cohort_end_date) {
                      valid_rows[[length(valid_rows) + 1]] <- row
                   }
                }
              }
            }
            
            if (length(valid_rows) > 0) {
              return(dplyr::bind_rows(valid_rows))
            } else {
              return(NULL)
            }
          })
          
          # Combine back
          valid_pop <- dplyr::bind_rows(valid_pop_list)
          
          if (nrow(valid_pop) > 0) {
            # Filter repeated events if FALSE
            if (events == FALSE) {
              valid_pop <- valid_pop %>%
                dplyr::group_by(.data$subject_id) %>%
                dplyr::filter(.data$risk_start_date == min(.data$risk_start_date, na.rm = TRUE)) %>%
                dplyr::ungroup()
            }
            
            # Push back to database
            # We use an intermediate to avoid db table locking issues if copying directly over
            temp_name <- paste0(tablePrefix, "_inc_5d_temp")
            
                  db_con <- attr(cdm, "dbcon")
                  if (is.null(db_con)) {
                     # Fallback 2: using con
                    db_con <- attr(cdm, "con")
                  }
                  if (is.null(db_con)) {
                    db_con <- attr(cdm[[denominatorTable]], "dbcon")
                  }
                  if (is.null(db_con)) {
                    db_con <- attr(cdm[[denominatorTable]], "con")
                  }
                  if (is.null(db_con)) {
                    # Final fallback: connection source
                    db_con <- cdm[[denominatorTable]]$src$con
                  }
                  if (is.null(db_con)) {
                     # Last resort for standard mock cdms
                     db_con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
                  }
                  
                  valid_pop_db <- dplyr::copy_to(
                    dest = db_con,
                    df = valid_pop,
                    name = temp_name,
                    overwrite = TRUE
                  )
                  
                  # Must convert to a proper tbl so union_all works later
                  if (!inherits(valid_pop_db, "tbl_sql")) {
                     # fallback to just joining them as dataframes if it is local
                     studyPopOutcome <- valid_pop
                  } else {
                     studyPopOutcome <- valid_pop_db
                  }
          } else {
            # No valid outcomes after washout
            studyPopOutcome <- studyPopOutcome %>% utils::head(0)
          }
        }
      }

      studyPopOutcome <- studyPopOutcome %>%
        dplyr::mutate(cohort_end_date = dplyr::if_else(
          !is.na(.data$outcome_start_date),
          .data$outcome_start_date,
          .data$cohort_end_date
        ))
    }

    studyPop <- studyPopNoOutcome %>%
      dplyr::collect() %>%
      dplyr::union_all(studyPopOutcome %>% dplyr::collect())

    if (is.null(washout)) {
      working_reason <- "Apply washout - anyone with outcome prior to start excluded"
    } else {
      working_reason <- paste0("Apply washout criteria of ", washout, " days (note, additional records may be created for those with an outcome)")
    }

    attrition <- recordAttrition(
      table = studyPop,
      id = "subject_id",
      reasonId = 12,
      reason = working_reason,
      existingAttrition = attrition
    ) |> dplyr::mutate(analysis_id = analysisId) |> dplyr::relocate("analysis_id")

    ir <- list()
    if (nrow(studyPop) > 0) {
      for (m in seq_len(nIntervals)) {
        window_start_days <- (m - 1) * days_in_interval
        window_end_days <- m * days_in_interval - 1

        workingPop <- studyPop %>%
          dplyr::mutate(
            person_window_start_m = .data$cohort_start_date + .env$window_start_days,
            person_window_end_m = .data$cohort_start_date + .env$window_end_days
          ) %>%
          dplyr::filter(
            .data$cohort_end_date >= .data$person_window_start_m,
            .data$risk_start_date <= .data$person_window_end_m
          )

        if (nrow(workingPop) > 0) {
          workingPop <- workingPop %>%
            dplyr::mutate(
              tStart = dplyr::if_else(.data$risk_start_date > .data$person_window_start_m,
                                      .data$risk_start_date,
                                      .data$person_window_start_m),
              tEnd = dplyr::if_else(.data$cohort_end_date >= .data$person_window_end_m,
                                    .data$person_window_end_m,
                                    .data$cohort_end_date)
            ) %>%
            dplyr::mutate(workingDays = as.numeric(difftime(.data$tEnd, .data$tStart, units = "days")) + 1) %>%
            dplyr::mutate(outcome_start_date = dplyr::if_else(
              .data$outcome_start_date <= .data$tEnd & .data$outcome_start_date >= .data$tStart,
              as.Date(.data$outcome_start_date),
              as.Date(NA)
            ))

          if (length(strata) == 0 || includeOverallStrata == TRUE) {
            ir[[paste0(m, "_overall")]] <- workingPop %>%
              dplyr::summarise(
                denominator_count = dplyr::n_distinct(.data$subject_id),
                person_days = sum(.data$workingDays, na.rm = TRUE),
                outcome_count = sum(!is.na(.data$outcome_start_date))
              ) %>%
              dplyr::mutate(
                interval_number = as.integer(.env$m),
                window_start_days = as.integer(.env$window_start_days),
                window_end_days = as.integer(.env$window_end_days),
                analysis_interval = paste0("rolling_", .env$intvl)
              )
          } else {
            ir[[paste0(m, "_overall")]] <- dplyr::tibble()
          }

          if (length(strata) >= 1) {
            ir[[paste0(m, "_overall")]] <- ir[[paste0(m, "_overall")]] %>%
              omopgenerics::uniteStrata()
              
            for (k in seq_along(strata)) {
              ir[[paste0(m, "_", k)]] <- workingPop %>%
                dplyr::group_by(dplyr::pick(.env$strata[[k]])) %>%
                dplyr::summarise(
                  denominator_count = dplyr::n_distinct(.data$subject_id),
                  person_days = sum(.data$workingDays, na.rm = TRUE),
                  outcome_count = sum(!is.na(.data$outcome_start_date)),
                  .groups = "drop"
                ) %>%
                dplyr::mutate(
                  interval_number = as.integer(.env$m),
                  window_start_days = as.integer(.env$window_start_days),
                  window_end_days = as.integer(.env$window_end_days),
                  analysis_interval = paste0("rolling_", .env$intvl)
                ) %>%
                omopgenerics::uniteStrata()
                
              ir[[paste0(m, "_overall")]] <- dplyr::bind_rows(ir[[paste0(m, "_overall")]], ir[[paste0(m, "_", k)]])
            }
          }
        }
      }
    }

    ir <- dplyr::bind_rows(ir)
    if (nrow(ir) > 0) {
      ir <- ir %>%
        dplyr::mutate(
          person_years = round(.data$person_days / 365.25, 3),
          incidence_100000_pys = round(((.data$outcome_count / .data$person_years) * rateDenominator), 3)
        ) %>%
        dplyr::rename(
          !!paste0("incidence_", rateDenominator, "_pys") := "incidence_100000_pys"
        ) %>%
        dplyr::mutate(analysis_id = analysisId) %>%
        dplyr::relocate("analysis_id")
    } else {
      ir <- dplyr::tibble()
    }

    result <- list()
    result[["ir"]] <- ir
    result[["attrition"]] <- attrition
    return(result)
  })

  irsList <- purrr::list_flatten(irsList, name_spec = "{inner}")

  # attrition
  for (i in seq_along(studySpecsList)) {
    irsList[names(irsList) == "attrition"][[i]] <- dplyr::bind_rows(
      omopgenerics::attrition(cdm[[denominatorTable]]) %>%
        dplyr::rename("denominator_cohort_id" = "cohort_definition_id") %>%
        dplyr::filter(.data$denominator_cohort_id == studySpecsList[[i]]$denominator_cohort_id) %>%
        dplyr::mutate(analysis_id = studySpecsList[[i]]$analysis_id) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.integer)),
      irsList[names(irsList) == "attrition"][[i]] %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.integer))
    )
  }
  attrition <- irsList[names(irsList) == "attrition"]
  attrition <- dplyr::bind_rows(attrition, .id = NULL) %>%
    dplyr::select(-"denominator_cohort_id") %>%
    dplyr::relocate("analysis_id")

  # analysis settings
  analysisSettings <- studySpecs %>%
    dplyr::left_join(
      omopgenerics::settings(cdm[[denominatorTable]]) %>%
        dplyr::rename("cohort_id" = "cohort_definition_id") %>%
        dplyr::rename_with(.cols = dplyr::everything(), function(x) { paste0("denominator_", x) }),
      by = "denominator_cohort_id"
    ) |>
    dplyr::left_join(outcomeRef, by = "outcome_cohort_id") |>
    dplyr::group_by(dplyr::across(!c(
      "analysis_id", "outcome_cohort_id", "denominator_cohort_id", "outcome_cohort_name", "denominator_cohort_name", "interval"
    ))) |>
    dplyr::mutate(result_id = as.integer(dplyr::cur_group_id())) |>
    dplyr::ungroup()

  # incidence estimates
  irs <- irsList[names(irsList) == "ir"]
  irs <- dplyr::bind_rows(irs, .id = NULL)

  if (nrow(irs) > 0) {
    irs <- irs %>%
      dplyr::bind_cols(incRateCiExact(irs$outcome_count, irs$person_years, rateDenominator))
  }

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(paste0(tablePrefix, "_inc_")))
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(paste0(tablePrefix, "_analysis_")))

  ## attrition formatting
  attritionSR <- attrition |>
    dplyr::distinct() |>
    dplyr::inner_join(
      analysisSettings |> dplyr::select(c("analysis_id", "denominator_cohort_name", "outcome_cohort_name", "result_id")),
      by = "analysis_id"
    ) |>
    dplyr::select(!"analysis_id") |>
    omopgenerics::uniteGroup(cols = c("denominator_cohort_name", "outcome_cohort_name")) |>
    tidyr::pivot_longer(
      cols = c("number_records", "number_subjects", "excluded_records", "excluded_subjects"),
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

  ## result formatting
  if (nrow(irs) == 0) {
    irs <- omopgenerics::emptySummarisedResult()
  } else {
    if (!"strata_name" %in% colnames(irs)) {
      irs <- irs |> omopgenerics::uniteStrata()
    }
    irs <- irs |>
      dplyr::distinct() |>
      dplyr::inner_join(
        analysisSettings |> dplyr::select(c("analysis_id", "denominator_cohort_name", "outcome_cohort_name", "result_id")),
        by = "analysis_id"
      ) |>
      dplyr::select(!"analysis_id") |>
      omopgenerics::uniteGroup(cols = c("denominator_cohort_name", "outcome_cohort_name")) |>
      omopgenerics::uniteAdditional(cols = c("interval_number", "window_start_days", "window_end_days", "analysis_interval")) |>
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
        "estimate_type" = dplyr::if_else(grepl("count", .data$estimate_name), "integer", "numeric"),
        "cdm_name" = omopgenerics::cdmName(cdm)
      )
  }

  ## settings formatting
  analysisSettings <- analysisSettings |>
    dplyr::mutate(
      result_type = "rolling_incidence",
      package_name = "IncidencePrevalence",
      package_version = as.character(utils::packageVersion("IncidencePrevalence")),
      analysis_outcome_washout = as.character(.data$outcome_washout),
      analysis_repeated_events = as.character(.data$repeated_events)
    ) |>
    dplyr::select(!dplyr::ends_with("_cohort_id")) |>
    dplyr::select(!dplyr::ends_with("_cohort_definition_id")) |>
    dplyr::select(!c("denominator_cohort_name", "outcome_cohort_name", "outcome_washout", "repeated_events", "interval")) |>
    dplyr::select(
      c("result_id", "result_type", "package_name", "package_version", "analysis_outcome_washout", "analysis_repeated_events"),
      dplyr::starts_with("denominator_"), dplyr::starts_with("outcome_")
    ) |>
    dplyr::distinct()

  ## bind
  irs <- omopgenerics::newSummarisedResult(
    x = irs,
    settings = analysisSettings |> dplyr::mutate(dplyr::across(-"result_id", as.character))
  )

  attritionSR <- attritionSR |>
    omopgenerics::newSummarisedResult(
      settings = analysisSettings |>
        dplyr::mutate(result_type = "incidence_attrition") |>
        dplyr::mutate(dplyr::across(-"result_id", as.character))
    )

  irs <- omopgenerics::bind(irs, attritionSR)

  dur <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
  cli::cli_alert_success("Overall time taken: {floor(dur/60)} mins and {dur %% 60 %/% 1} secs")

  return(irs)
}

checkInputEstimateRollingIncidence <- function(cdm,
                                               denominatorTable,
                                               outcomeTable,
                                               denominatorCohortId,
                                               outcomeCohortId,
                                               interval,
                                               nIntervals,
                                               outcomeWashout,
                                               repeatedEvents) {
  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::validateCohortArgument(cohort = cdm[[denominatorTable]])
  if (!is.null(denominatorCohortId)) {
    denominatorCohortId <- omopgenerics::validateCohortIdArgument(
      denominatorCohortId, cdm[[denominatorTable]]
    )
  }
  omopgenerics::validateCohortArgument(cohort = cdm[[outcomeTable]])
  if (!is.null(outcomeCohortId)) {
    outcomeCohortId <- omopgenerics::validateCohortIdArgument(
      outcomeCohortId, cdm[[outcomeTable]]
    )
  }

  omopgenerics::assertTrue(all(interval %in% c("weeks", "months", "quarters", "years")))
  omopgenerics::assertNumeric(nIntervals, integerish = TRUE, min = 1)

  if (any(outcomeWashout != Inf)) {
    omopgenerics::assertNumeric(outcomeWashout[which(!is.infinite(outcomeWashout))], min = 0, max = 99999)
  }
  omopgenerics::assertLogical(repeatedEvents)

  return(list(denominatorCohortId, outcomeCohortId))
}
