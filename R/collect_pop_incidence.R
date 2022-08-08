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
#' @param db Database connection via DBI::dbConnect()
#' @param results_schema_outcomes Name of the schema which contains the outcome table
#' @param table_name_outcomes Name of the table with the outcome cohorts
#' @param cohort_ids_outcomes Outcome cohort ids
#' @param study_denominator_pop Tibble with denominator populations
#' @param cohort_ids_denominator_pops Cohort ids of denominator populations
#' @param time_intervals Time intervals for incidence estimates
#' @param outcome_washout_windows Clean windows
#' @param repetitive_events Repeated events
#' @param confidence_intervals Method for confidence intervals
#' @param verbose Whether to report progress
#'
#' @return
#' @export
#'
#' @examples
collect_pop_incidence <- function(db,
                                  results_schema_outcomes,
                                  table_name_outcomes,
                                  cohort_ids_outcomes,
                                  study_denominator_pop,
                                  cohort_ids_denominator_pops,
                                  time_intervals = "Months",
                                  outcome_washout_windows = 0,
                                  repetitive_events = FALSE,
                                  confidence_intervals = "exact",
                                  verbose = FALSE) {


  # help to avoid formatting errors
  if (is.numeric(cohort_ids_outcomes)) {
    cohort_ids_outcomes <- as.character(cohort_ids_outcomes)
  }
  if (is.numeric(cohort_ids_denominator_pops)) {
    cohort_ids_denominator_pops <- as.character(cohort_ids_denominator_pops)
  }
  if (is.character(time_intervals)) {
    time_intervals <- stringr::str_to_sentence(time_intervals)
  }
  if (is.character(confidence_intervals)) {
    confidence_intervals <- stringr::str_to_lower(confidence_intervals)
  }




  ## check for standard types of user error
  error_message <- checkmate::makeAssertCollection()
  db_inherits_check <- inherits(db, "DBIConnection")
  checkmate::assertTRUE(db_inherits_check,
    add = error_message
  )
  if (!isTRUE(db_inherits_check)) {
    error_message$push(
      "- db must be a database connection via DBI::dbConnect()"
    )
  }
  checkmate::assert_character(results_schema_outcomes,
    add = error_message,
    null.ok = TRUE
  )
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
  checkmate::assertTRUE(!is.null(study_denominator_pop$age_strata) &
    sum(is.na(study_denominator_pop$age_strata)) == 0)
  checkmate::assertTRUE(!is.null(study_denominator_pop$sex_strata) &
    sum(is.na(study_denominator_pop$sex_strata)) == 0)
  checkmate::assertTRUE(
    !is.null(study_denominator_pop$required_days_prior_history) &
      sum(is.na(study_denominator_pop$required_days_prior_history)) == 0
  )
  checkmate::assertTRUE(all(c(
    "cohort_definition_id",
    "person_id",
    "cohort_start_date", "cohort_end_date",
    "age_strata", "sex_strata", "required_days_prior_history"
  ) %in%
    names(study_denominator_pop)))

  checkmate::assert_character(cohort_ids_denominator_pops,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assertTRUE(all(time_intervals %in% c("Months", "Years")),
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
  checkmate::assert_choice(confidence_intervals,
    choices = c("exact"),
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
    confidence_interval = confidence_intervals,
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
  irs <- lapply(study_specs, function(x) {
    get_pop_incidence(
      db = db,
      results_schema_outcome = results_schema_outcomes,
      table_name_outcome = table_name_outcomes,
      cohort_id_outcome = x$cohort_id_outcome,
      study_denominator_pop = study_denominator_pop,
      cohort_id_denominator_pop = x$cohort_id_denominator_pop,
      time_interval = x$time_interval,
      outcome_washout_window = x$outcome_washout_window,
      repetitive_events = x$repetitive_events,
      confidence_interval = x$confidence_interval,
      verbose = x$verbose
    ) %>%
      dplyr::mutate(
        cohort_id_outcome = x$cohort_id_outcome,
        cohort_id_denominator_pop = x$cohort_id_denominator_pop,
        time_interval = x$time_interval,
        outcome_washout_window = x$outcome_washout_window,
        repetitive_events = x$repetitive_events,
        confidence_interval = x$confidence_interval
      )
  })
  # to tibble and add specification for each cohort
  irs <- dplyr::bind_rows(irs,
    .id = "incidence_analysis_id"
  )

  return(irs)
}
