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


#' Collect population incidence estimates
#'
#' @param cdm CDMConnector CDM reference object
#' @param table_name_denominator table_name_denominator
#' @param table_name_outcomes Name of the table with the outcome cohorts
#' @param cohort_ids_outcomes Outcome cohort ids
#' @param cohort_ids_denominator_pops Cohort ids of denominator populations
#' @param time_interval Time intervals for incidence estimates
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
collect_pop_incidence <- function(cdm,
                                  table_name_denominator,
                                  cohort_ids_denominator_pops,
                                  table_name_outcomes,
                                  cohort_ids_outcomes,
                                  time_interval = "Months",
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
  if (is.character(time_interval)) {
    time_interval <- tolower(time_interval)
  }
  if (is.character(confidence_interval)) {
    confidence_intervals <- tolower(confidence_interval)
  }


  ## check for standard types of user error
  error_message <- checkmate::makeAssertCollection()
  db_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(db_inherits_check,
    add = error_message
  )
  if (!isTRUE(db_inherits_check)) {
    "- cdm must be a CDMConnector CDM reference object"
  }

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
  checkmate::assert_choice(time_interval,
    choices = c("days","weeks","months","quarters","years"),
    add = error_message
  )
  checkmate::assert_numeric(outcome_washout_windows,
                            add = error_message,
                            null.ok = TRUE
  )
  checkmate::assert_logical(repetitive_events,
                            add = error_message
  )
  checkmate::assert_choice(confidence_interval,
                           choices = c("none","poisson"),
                           add = error_message,
                           null.ok = TRUE
  )
  checkmate::assert_number(minimum_cell_count)
  checkmate::assert_logical(verbose,
                            add = error_message
  )
  checkmate::reportAssertions(collection = error_message)


# further checks that there are the required data elements
  error_message <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(all(c(
    "cohort_definition_id",
    "subject_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %in%
    names(cdm[[table_name_denominator]] %>% utils::head(1) %>% dplyr::collect())))

  date_check<-nrow(cdm[[table_name_denominator]] %>%
                     dplyr::select("cohort_start_date", "cohort_end_date") %>%
                     dplyr::filter(.data$cohort_start_date>.data$cohort_end_date) %>%
                     dplyr::collect()) == 0
  checkmate::assertTRUE(date_check)
  if (!isTRUE(date_check)) {
    error_message$push(
      "- some end dates before start dates in denominator"
    )
  }
  denominator_count_check<-cdm[[table_name_denominator]] %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$cohort_ids_denominator_pops) %>%
    dplyr::count() %>%
    dplyr::pull() > 0
  checkmate::assertTRUE(denominator_count_check,
    add = error_message
  )
  if (!isTRUE(denominator_count_check)) {
    error_message$push(
      "- nobody found in `table_name_denominator` with one of the `cohort_ids_denominator_pops`"
    )
  }
  outcome_count_check<-cdm[[table_name_outcomes]] %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$cohort_ids_outcomes ) %>%
    dplyr::count() %>%
    dplyr::pull() > 0
  checkmate::assertTRUE(outcome_count_check,
                        add = error_message
  )
  if (!isTRUE(outcome_count_check)) {
    error_message$push(
      "- nobody found in `table_name_outcomes` with one of the `cohort_ids_outcomes`"
    )
  }

  missing_check<-nrow(cdm[[table_name_denominator]] %>%
    dplyr::filter(is.na(.data$cohort_definition_id) | is.na(.data$subject_id) |
                    is.na(.data$cohort_start_date)| is.na(.data$cohort_end_date)) %>%
      dplyr::collect())==0
  if (!isTRUE(missing_check)) {
    error_message$push(
      "- there is missing data in `table_name_denominator`"
    )
  }
  checkmate::reportAssertions(collection = error_message)




  study_specs <- tidyr::expand_grid(
    cohort_id_outcome = cohort_ids_outcomes,
    cohort_id_denominator_pop = cohort_ids_denominator_pops,
    time_interval = time_interval,
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
      cdm = cdm,
      table_name_denominator=table_name_denominator,
      cohort_id_denominator_pop = x$cohort_id_denominator_pop,
      table_name_outcome = table_name_outcomes,
      cohort_id_outcome = x$cohort_id_outcome,
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
