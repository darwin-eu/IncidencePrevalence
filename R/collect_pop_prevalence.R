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


#' Collect population prevalence estimates
#'
#' @param db Database connection via DBI::dbConnect()
#' @param results_schema_outcomes Name of the schema which contains
#' the outcome table
#' @param table_name_outcomes Name of the table with the outcome cohorts
#' @param cohort_ids_outcomes Outcome cohort ids
#' @param study_denominator_pop Tibble with denominator populations
#' @param cohort_ids_denominator_pops Cohort ids of denominator populations
#' @param periods Periods to compute the prevalence
#' @param time_intervals Time intervals for prevalence estimates
#' @param minimum_representative_proportions Minimum proportions that
#' individuals must have to contribute
#' @param confidence_interval Method for confidence intervals
#' @param minimum_cell_count Minimum number of events to report- results
#' lower than this will be obscured. If NULL all results will be reported.
#' @param verbose Whether to report progress
#'
#' @return
#' @export
#'
#' @examples
collect_pop_prevalence <- function(db,
                                  results_schema_outcomes,
                                  table_name_outcomes,
                                  cohort_ids_outcomes,
                                  study_denominator_pop,
                                  cohort_ids_denominator_pops,
                                  periods = "Point",
                                  time_intervals = "Months",
                                  minimum_representative_proportions = 0.5,
                                  confidence_interval = "none",
                                  minimum_cell_count = 5,
                                  verbose = FALSE) {


  # help to avoid formatting errors
  if (is.numeric(cohort_ids_outcomes)) {
    cohort_ids_outcomes <- as.character(cohort_ids_outcomes)
  }
  if (is.numeric(cohort_ids_denominator_pops)) {
    cohort_ids_denominator_pops <- as.character(cohort_ids_denominator_pops)
  }
  if (is.character(periods)) {
    periods <- stringr::str_to_sentence(periods)
  }
  if (is.character(time_intervals)) {
    time_intervals <- stringr::str_to_sentence(time_intervals)
  }
  if (is.character(confidence_interval)) {
    confidence_interval <- stringr::str_to_lower(confidence_interval)
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
  checkmate::assert_number(minimum_cell_count, null.ok = TRUE)
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
  checkmate::assertTRUE(all(periods %in% c("Point", "Month", "Year")),
    add = error_message
  )
  checkmate::assertTRUE(all(time_intervals %in% c("Months", "Years")),
    add = error_message
  )
  checkmate::assert_numeric(minimum_representative_proportions,
    add = error_message,
    lower = 0,
    upper = 1
  )
  checkmate::assert_logical(verbose,
    add = error_message
  )
  checkmate::assert_choice(confidence_interval,
    choices = c("none", "poisson"),
    add = error_message,
    null.ok = TRUE
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)


  study_specs <- tidyr::expand_grid(
    cohort_id_outcome = cohort_ids_outcomes,
    cohort_id_denominator_pop = cohort_ids_denominator_pops,
    period = periods,
    time_interval = time_intervals,
    minimum_representative_proportion = minimum_representative_proportions,
    verbose = verbose
  )

  study_specs <- study_specs %>%
    dplyr::mutate(prevalence_analysis_id = as.character(dplyr::row_number()))

  study_specs <- split(
    study_specs,
    study_specs[, c("prevalence_analysis_id")]
  )

  # get irs
  prs <- lapply(study_specs, function(x) {
    get_pop_prevalence(
      db = db,
      results_schema_outcome = results_schema_outcomes,
      table_name_outcome = table_name_outcomes,
      cohort_id_outcome = x$cohort_id_outcome,
      study_denominator_pop = study_denominator_pop,
      cohort_id_denominator_pop = x$cohort_id_denominator_pop,
      period = x$period,
      time_interval = x$time_interval,
      minimum_representative_proportion = x$minimum_representative_proportion,
      verbose = x$verbose
    ) %>%
      dplyr::mutate(
        cohort_id_outcome = x$cohort_id_outcome,
        cohort_id_denominator_pop = x$cohort_id_denominator_pop,
        period = x$period,
        time_interval = x$time_interval,
        minimum_representative_proportion = x$minimum_representative_proportion,
        confidence_interval = confidence_interval,
        minimum_cell_count= minimum_cell_count
      )
  })
  # to tibble and add specification for each cohort
  prs <- dplyr::bind_rows(prs,
    .id = "prevalence_analysis_id"
  )

  # get confidence intervals
  if(confidence_interval != "none"){
    prs <-get_confidence_intervals(prs, confidence_interval)
  } else {
    prs <- prs %>%
      dplyr::mutate(prev_low=NA) %>%
      dplyr::mutate(prev_high=NA)%>%
      dplyr::relocate(.data$prev_low, .after = .data$prev) %>%
      dplyr::relocate(.data$prev_high, .after = .data$prev_low)
  }

  # obscure counts
  if(!is.null(minimum_cell_count)){
  prs <- obscure_counts(prs,
                        minimum_cell_count = minimum_cell_count,
                        substitute = NA)
  } else {
    # no results obscured due to a low count
    prs <- prs %>%
      dplyr::mutate(cohort_obscured="FALSE") %>%
      dplyr::mutate(result_obscured="FALSE")
  }

  return(prs)
}
