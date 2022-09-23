# Copyright 2022 DARWIN EU (C)
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
#' @param cdm_ref CDMConnector CDM reference object
#' @param table_name_outcomes Name of the table with the outcome cohorts
#' @param cohort_ids_outcomes Outcome cohort ids
#' @param study_denominator_pop Tibble with denominator populations
#' @param cohort_ids_denominator_pops Cohort ids of denominator populations
#' @param time_intervals Time intervals for incidence estimates
#' @param outcome_washout_windows Clean windows
#' @param repetitive_events Repeated events
#' @param confidence_interval Method for confidence intervals
#' @param minimum_cell_count Minimum number of events to report- results
#' lower than this will be obscured. If NULL all results will be reported.
#' @param verbose Whether to report progress
#'
#' @return
#' @export
#'
#' @examples
collect_pop_incidence <- function(cdm_ref,
                                  table_name_outcomes,
                                  cohort_ids_outcomes,
                                  study_denominator_pop,
                                  cohort_ids_denominator_pops,
                                  time_intervals = "Months",
                                  outcome_washout_windows = 0,
                                  repetitive_events = FALSE,
                                  confidence_interval = "poisson",
                                  minimum_cell_count = 5,
                                  verbose = FALSE) {


  # help to avoid formatting errors
  if (is.numeric(cohort_ids_outcomes)) {
    cohort_ids_outcomes <- as.character(cohort_ids_outcomes)
  }
  if (is.numeric(cohort_ids_denominator_pops)) {
    cohort_ids_denominator_pops <- as.character(cohort_ids_denominator_pops)
  }
  if (is.character(time_intervals)) {
    time_intervals <- tolower(time_intervals)
  }
  if (is.character(confidence_interval)) {
    confidence_intervals <- tolower(confidence_interval)
  }




  ## check for standard types of user error
  error_message <- checkmate::makeAssertCollection()
  db_inherits_check <- inherits(cdm_ref, "cdm_reference")
  checkmate::assertTRUE(db_inherits_check,
    add = error_message
  )
  if (!isTRUE(db_inherits_check)) {
    "- cdm_ref must be a CDMConnector CDM reference object"
  }
  checkmate::assert_character(cohort_ids_outcomes,
    add = error_message,
    null.ok = TRUE
  )

  checkmate::assert_tibble(study_denominator_pop,
    add = error_message
  )
  checkmate::assertTRUE(all(study_denominator_pop$cohort_start_date <=
    study_denominator_pop$cohort_end_date))
  checkmate::assertTRUE(nrow(study_denominator_pop) > 0,
    add = error_message
  )
  checkmate::assertTRUE(!is.null(study_denominator_pop$cohort_definition_id) &
    sum(is.na(study_denominator_pop$cohort_definition_id)) == 0)
  checkmate::assertTRUE(!is.null(study_denominator_pop$person_id) &
    sum(is.na(study_denominator_pop$person_id)) == 0)
  checkmate::assertTRUE(!is.null(study_denominator_pop$cohort_start_date) &
    sum(is.na(study_denominator_pop$cohort_start_date)) == 0)
  checkmate::assertTRUE(!is.null(study_denominator_pop$cohort_end_date) &
    sum(is.na(study_denominator_pop$cohort_end_date)) == 0)
  checkmate::assert_number(minimum_cell_count, null.ok = TRUE)
  checkmate::assertTRUE(all(c(
    "cohort_definition_id",
    "person_id",
    "cohort_start_date", "cohort_end_date"
  ) %in%
    names(study_denominator_pop)))

  checkmate::assert_character(cohort_ids_denominator_pops,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assertTRUE(all(time_intervals %in% c("months", "years")),
    add = error_message
  )
  checkmate::assert_numeric(outcome_washout_windows,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_logical(repetitive_events,
    add = error_message
  )
  checkmate::assert_logical(verbose,
    add = error_message
  )
  checkmate::assert_choice(confidence_interval,
    choices = c("none","poisson"),
    add = error_message,
    null.ok = TRUE
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)

  study_specs <- tidyr::expand_grid(
    cohort_id_outcome = cohort_ids_outcomes,
    cohort_id_denominator_pop = cohort_ids_denominator_pops,
    time_interval = time_intervals,
    outcome_washout_window = outcome_washout_windows,
    repetitive_events = repetitive_events,
    confidence_interval = confidence_interval,
    verbose = verbose
  )

  if (is.null(outcome_washout_windows)) {
    study_specs$outcome_washout_window <- NA
  }

  study_specs <- study_specs %>%
    dplyr::mutate(incidence_analysis_id = as.character(dplyr::row_number()))

  study_specs <- split(
    study_specs,
    study_specs[, c("incidence_analysis_id")]
  )

  # get irs
  irs_list <- lapply(study_specs, function(x) {
    working_inc <- get_pop_incidence(
      cdm_ref = cdm_ref,
      table_name_outcome = table_name_outcomes,
      cohort_id_outcome = x$cohort_id_outcome,
      study_denominator_pop = study_denominator_pop,
      cohort_id_denominator_pop = x$cohort_id_denominator_pop,
      time_interval = x$time_interval,
      outcome_washout_window = x$outcome_washout_window,
      repetitive_events = x$repetitive_events,
      verbose = x$verbose
    )

    working_inc_ir <- working_inc[["ir"]] %>%
      dplyr::mutate(incidence_analysis_id = x$incidence_analysis_id) %>%
      dplyr::relocate(.data$incidence_analysis_id)

    working_inc_person_table <- working_inc[["person_table"]] %>%
      dplyr::mutate(incidence_analysis_id = x$incidence_analysis_id) %>%
      dplyr::relocate(.data$incidence_analysis_id)

    working_inc_analysis_settings <- working_inc[["analysis_settings"]] %>%
      dplyr::mutate(
      cohort_id_outcome = x$cohort_id_outcome,
      cohort_id_denominator_pop = x$cohort_id_denominator_pop,
      time_interval = x$time_interval,
      outcome_washout_window = x$outcome_washout_window,
      repetitive_events = x$repetitive_events,
      confidence_interval = .env$confidence_interval,
      minimum_cell_count = .env$minimum_cell_count,
      incidence_analysis_id = x$incidence_analysis_id
    ) %>%
      dplyr::relocate(.data$incidence_analysis_id)


    working_inc_attrition <- working_inc[["attrition"]] %>%
      dplyr::mutate(incidence_analysis_id = x$incidence_analysis_id ) %>%
      dplyr::relocate(.data$incidence_analysis_id)

    result <- list()
    result[["ir"]] <- working_inc_ir
    result[["analysis_settings"]] <- working_inc_analysis_settings
    result[["person_table"]] <- working_inc_person_table
    result[["attrition"]] <- working_inc_attrition

    return(result)
  })


  irs_list <- purrr::flatten(irs_list)

  # analysis settings
  analysis_settings <- irs_list[names(irs_list) == "analysis_settings"]
  # to tibble
  analysis_settings <- dplyr::bind_rows(analysis_settings,
                          .id = NULL
  )

  # incidence estimates
  irs <- irs_list[names(irs_list) == "ir"]
  # to tibble
  irs <- dplyr::bind_rows(irs,
    .id = NULL
  )

  # get confidence intervals
  if (confidence_interval != "none") {
    irs <- get_confidence_intervals(irs, confidence_interval)
  } else {
    irs <- irs %>%
      dplyr::mutate(ir_100000_pys_low = NA) %>%
      dplyr::mutate(ir_100000_pys_high = NA) %>%
      dplyr::relocate(.data$ir_100000_pys_low, .after = .data$ir_100000_pys) %>%
      dplyr::relocate(.data$ir_100000_pys_high, .after = .data$ir_100000_pys_low)
  }

  # obscure counts
  if (!is.null(minimum_cell_count)) {
  irs <- obscure_counts(irs, minimum_cell_count = minimum_cell_count, substitute = NA)
  } else {
    # no results obscured due to a low count
    irs <- irs %>%
      dplyr::mutate(cohort_obscured = "FALSE") %>%
      dplyr::mutate(result_obscured = "FALSE")
  }

  # person_table summary
  person_table <- irs_list[names(irs_list) == "person_table"]
  # to tibble
  person_table <- dplyr::bind_rows(person_table,
                                   .id = NULL
  )

  # attrition summary
  attrition <- irs_list[names(irs_list) == "attrition"]
  # to tibble
  attrition <- dplyr::bind_rows(attrition,
                          .id = NULL
  )

  # results to return as a list
  results <- list()
  results[["incidence_estimates"]] <- irs
  results[["analysis_settings"]] <- analysis_settings
  results[["person_table"]] <- person_table
  results[["attrition"]] <- attrition

  return(results)
}
