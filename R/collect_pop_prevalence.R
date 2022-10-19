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


#' Collect population prevalence estimates
#'
#' @param cdm CDMConnector CDM reference object
#' @param table_name_denominator table_name_denominator
#' @param cohort_ids_denominator_pops Cohort ids of denominator populations
#' @param table_name_outcomes Name of the table with the outcome cohorts
#' @param cohort_ids_outcomes Outcome cohort ids
#' @param type type of prevalence, point or period
#' @param points where to compute the point prevalence
#' @param time_intervals Time intervals for prevalence estimates
#' @param full_periods_required If full period is required
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
collect_pop_prevalence <- function(cdm,
                                  table_name_denominator,
                                  cohort_ids_denominator_pops,
                                  table_name_outcomes,
                                  cohort_ids_outcomes,
                                  type = "point",
                                  time_intervals = "months",
                                  full_periods_required = TRUE,
                                  points = "start",
                                  minimum_representative_proportions = 0.5,
                                  confidence_interval = "binomial",
                                  minimum_cell_count = 5,
                                  verbose = FALSE) {


  # help to avoid formatting errors
  if (is.numeric(cohort_ids_outcomes)) {
    cohort_ids_outcomes <- as.character(cohort_ids_outcomes)
  }
  if (is.numeric(cohort_ids_denominator_pops)) {
    cohort_ids_denominator_pops <- as.character(cohort_ids_denominator_pops)
  }
  if (is.character(type)) {
    type <- tolower(type)
  }
  if (is.character(time_intervals)) {
    time_intervals <- tolower(time_intervals)
  }
  if (is.character(confidence_interval)) {
    confidence_interval <- tolower(confidence_interval)
  }
  if (is.character(points)) {
    points <- tolower(points)
  }

  ## check for standard types of user error
  error_message <- checkmate::makeAssertCollection()
  db_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(db_inherits_check,
    add = error_message
  )
  if (!isTRUE(db_inherits_check)) {
    error_message$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::assert_character(cohort_ids_outcomes,
    add = error_message,
    null.ok = TRUE
  )
  denominator_check<-table_name_denominator %in% names(cdm)
  checkmate::assertTRUE(denominator_check,
                        add = error_message)
  if (!isTRUE(denominator_check)) {
    error_message$push(
      "- `table_name_denominator` is not found in cdm"
    )
  }
  checkmate::assert_character(cohort_ids_denominator_pops,
                              add = error_message,
                              null.ok = TRUE
  )
  outcome_check<-table_name_outcomes %in% names(cdm)
  checkmate::assertTRUE(outcome_check,
                        add = error_message)
  if (!isTRUE(outcome_check)) {
    error_message$push(
      "- `table_name_outcomes` is not found in cdm"
    )
  }
  checkmate::assert_character(cohort_ids_outcomes,
                              add = error_message,
                              null.ok = TRUE
  )
  checkmate::assert_choice(type,
                           choices = c("point","period"),
                           add = error_message
  )
  checkmate::assertTRUE(all(time_intervals %in% c("days","weeks","months","quarters","years")),
                        add = error_message
  )
  checkmate::assertTRUE(all(points %in% c("start","middle","end")),
                        add = error_message
  )
  checkmate::assert_number(minimum_cell_count)
  checkmate::assert_numeric(minimum_representative_proportions,
    add = error_message,
    lower = 0,
    upper = 1
  )
  checkmate::assert_logical(verbose,
    add = error_message
  )
  checkmate::assert_logical(full_periods_required,
    add = error_message
  )
  checkmate::assert_choice(confidence_interval,
    choices = c("none", "binomial"),
    add = error_message,
    null.ok = TRUE
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)


  study_specs <- tidyr::expand_grid(
    cohort_id_outcome = cohort_ids_outcomes,
    cohort_id_denominator_pop = cohort_ids_denominator_pops,
    time_interval = time_intervals,
    point = points,
    minimum_representative_proportion = minimum_representative_proportions,
    verbose = verbose
  )

  study_specs <- study_specs %>%
    dplyr::mutate(prevalence_analysis_id = as.character(dplyr::row_number()))

  study_specs <- split(
    study_specs,
    study_specs[, c("prevalence_analysis_id")]
  )

  # get prs
  prs_list <- lapply(study_specs, function(x) {

    working_prev <- get_pop_prevalence(
      cdm = cdm,
      table_name_denominator=table_name_denominator,
      cohort_id_denominator_pop = x$cohort_id_denominator_pop,
      table_name_outcome = table_name_outcomes,
      cohort_id_outcome = x$cohort_id_outcome,
      type = type,
      time_interval = x$time_interval,
      full_periods_required = full_periods_required,
      point = x$point,
      minimum_representative_proportion = x$minimum_representative_proportion,
      verbose = x$verbose
    )

    working_prev_pr <- working_prev[["pr"]] %>%
      dplyr::mutate(prevalence_analysis_id = x$prevalence_analysis_id) %>%
      dplyr::relocate(.data$prevalence_analysis_id)

    working_prev_analysis_settings <- working_prev[["analysis_settings"]]  %>%
      dplyr::mutate(
        cohort_id_outcome = x$cohort_id_outcome,
        cohort_id_denominator_pop = x$cohort_id_denominator_pop,
        type = type,
        time_interval = x$time_interval,
        full_periods_required = full_periods_required,
        point = x$point,
        minimum_representative_proportion = x$minimum_representative_proportion,
        confidence_interval = confidence_interval,
        minimum_cell_count = minimum_cell_count,
        prevalence_analysis_id = x$prevalence_analysis_id) %>%
      dplyr::relocate(.data$prevalence_analysis_id)

    working_prev_attrition <- working_prev[["attrition"]] %>%
      dplyr::mutate(prevalence_analysis_id = x$prevalence_analysis_id ) %>%
      dplyr::relocate(.data$prevalence_analysis_id)

    working_prev_person_table <- working_prev[["person_table"]] %>%
      dplyr::mutate(prevalence_analysis_id = x$prevalence_analysis_id ) %>%
      dplyr::relocate(.data$prevalence_analysis_id)

    result <- list()
    result[["pr"]] <- working_prev_pr
    result[["analysis_settings"]] <- working_prev_analysis_settings
    result[["person_table"]] <- working_prev_person_table
    result[["attrition"]] <- working_prev_attrition

    return(result)

  })

  prs_list <- purrr::flatten(prs_list)

  # analysis settings
  analysis_settings <- prs_list[names(prs_list) == "analysis_settings"]
  # to tibble
  analysis_settings <- dplyr::bind_rows(analysis_settings,
                                        .id = NULL
  )

  # analysis settings
  person_table <- prs_list[names(prs_list) == "person_table"]
  # to tibble
  person_table <- dplyr::bind_rows(person_table,
                                   .id = NULL
  )

  # prevalence estimates
  prs <- prs_list[names(prs_list) == "pr"]
  # to tibble
  prs <- dplyr::bind_rows(prs,
                          .id = NULL
  )

  # get confidence intervals
  prs <- get_ci_prevalence(prs, confidence_interval) %>%
    dplyr::relocate(.data$prev_low, .after = .data$prev) %>%
    dplyr::relocate(.data$prev_high, .after = .data$prev_low)

  # obscure counts
  if (!is.null(minimum_cell_count)) {
  prs <- obscure_counts(prs,
                        minimum_cell_count = minimum_cell_count,
                        substitute = NA)
  } else {
    # no results obscured due to a low count
    prs <- prs %>%
      dplyr::mutate(cohort_obscured = "FALSE") %>%
      dplyr::mutate(result_obscured = "FALSE")
  }

  # attrition summary
  attrition <- prs_list[names(prs_list) == "attrition"]
  # to tibble
  attrition <- dplyr::bind_rows(attrition,
                                .id = NULL
  )

  # results to return as a list
  results <- list()
  results[["prevalence_estimates"]] <- prs
  results[["analysis_settings"]] <- analysis_settings
  results[["person_table"]] <- person_table
  results[["attrition"]] <- attrition

  return(results)

}
