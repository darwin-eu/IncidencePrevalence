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

#' Compute study dates for incidence
#' @param start_date start date of the study
#' @param end_date end date of the study
#' @param time_interval interval to compute prevalence
#' @noRd
compute_study_days_inc <- function(start_date,
                                   end_date,
                                   time_interval) {
  if (time_interval == "weeks") {
    week_correction <- lubridate::days(1)
  } else {
    week_correction <- lubridate::days(0)
  }
  if (time_interval == "days") {
    study_days <- dplyr::tibble(start_time = seq.Date(
      from = start_date,
      to = end_date,
      by = "days"
    )) %>%
      dplyr::mutate(day = lubridate::day(.data$start_time)) %>%
      dplyr::mutate(month = lubridate::month(.data$start_time)) %>%
      dplyr::mutate(year = lubridate::year(.data$start_time)) %>%
      dplyr::mutate(time = dplyr::if_else(.data$month < 10,
        paste0(.data$year, "_0", .data$month),
        paste0(.data$year, "_", .data$month)
      )) %>%
      dplyr::mutate(time = dplyr::if_else(.data$day < 10,
        paste0(.data$time, "_0", .data$day),
        paste0(.data$time, "_", .data$day)
      )) %>%
      dplyr::mutate(end_time = .data$start_time) %>%
      dplyr::select("time", "start_time", "end_time")
  } else {
    study_days <- dplyr::tibble(dates = seq.Date(
      from = start_date,
      to = end_date,
      by = "days"
    )) %>%
      dplyr::mutate(isoweek = lubridate::isoweek(.data$dates)) %>%
      dplyr::mutate(month = lubridate::month(.data$dates)) %>%
      dplyr::mutate(quarter = quarters(.data$dates)) %>%
      dplyr::mutate(year = lubridate::year(.data$dates)) %>%
      dplyr::mutate(years = glue::glue("{year}")) %>%
      dplyr::mutate(months = dplyr::if_else(.data$month < 10,
        paste0(.data$year, "_0", .data$month),
        paste0(.data$year, "_", .data$month)
      )) %>%
      dplyr::mutate(quarters = glue::glue("{year}_{quarter}")) %>%
      dplyr::mutate(year = dplyr::if_else(.data$month == 1 & .data$isoweek > 50,
        .data$year - 1,
        .data$year
      )) %>%
      dplyr::mutate(weeks = dplyr::if_else(.data$isoweek < 10,
        paste0(.data$year, "_0", .data$isoweek),
        paste0(.data$year, "_", .data$isoweek)
      )) %>%
      dplyr::rename("time" = time_interval) %>%
      dplyr::mutate(time = as.character(.data$time)) %>%
      dplyr::group_by(.data$time) %>%
      dplyr::summarise(
        start_time = min(.data$dates, na.rm = TRUE),
        end_time = max(.data$dates, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
  }
  return(study_days)
}


#' Get population incidence estimates
#'
#' @param cdm CDMConnector CDM reference object
#' @param table_name_denominator table_name_denominator
#' @param table_name_outcome table_name_outcome
#' @param cohort_id_outcome cohort_id_outcome
#' @param cohort_id_denominator_pop cohort_id_denominator_pop
#' @param time_interval time_interval
#' @param outcome_washout_window outcome_washout_window
#' @param repetitive_events repetitive_events
#' @param verbose verbose
#'
#' @importFrom lubridate `%within%`
#' @return
#' @export
#'
#' @examples
get_pop_incidence <- function(cdm,
                              table_name_denominator,
                              cohort_id_denominator_pop,
                              table_name_outcome,
                              cohort_id_outcome,
                              time_interval,
                              outcome_washout_window,
                              repetitive_events,
                              verbose) {
  if (!is.null(outcome_washout_window)) {
    if (is.na(outcome_washout_window)) {
      outcome_washout_window <- NULL
    }
  }


  # ## check for standard types of user error
  # error_message <- checkmate::makeAssertCollection()
  # db_inherits_check <- inherits(cdm, "cdm_reference")
  # checkmate::assertTRUE(db_inherits_check,
  #   add = error_message
  # )
  # if (!isTRUE(db_inherits_check)) {
  #   error_message$push(
  #     "- cdm must be a CDMConnector CDM reference object"
  #   )
  # }
  # checkmate::assert_character(cohort_id_outcome,
  #   add = error_message,
  #   null.ok = TRUE
  # )
  #
  # checkmate::assert_tibble(study_denominator_pop,
  #   add = error_message
  # )
  # checkmate::assertTRUE(all(study_denominator_pop$cohort_start_date <=
  #   study_denominator_pop$cohort_end_date))
  # checkmate::assertTRUE(nrow(study_denominator_pop) > 0,
  #   add = error_message
  # )
  # checkmate::assertTRUE(!is.null(study_denominator_pop$cohort_definition_id) &
  #   sum(is.na(study_denominator_pop$cohort_definition_id)) == 0)
  # checkmate::assertTRUE(!is.null(study_denominator_pop$subject_id) &
  #   sum(is.na(study_denominator_pop$subject_id)) == 0)
  # checkmate::assertTRUE(!is.null(study_denominator_pop$cohort_start_date) &
  #   sum(is.na(study_denominator_pop$cohort_start_date)) == 0)
  # checkmate::assertTRUE(!is.null(study_denominator_pop$cohort_end_date) &
  #   sum(is.na(study_denominator_pop$cohort_end_date)) == 0)
  # checkmate::assertTRUE(all(c(
  #   "cohort_definition_id",
  #   "subject_id",
  #   "cohort_start_date", "cohort_end_date"
  # ) %in%
  #   names(study_denominator_pop)))
  #
  # checkmate::assert_character(cohort_id_denominator_pop,
  #   add = error_message,
  #   null.ok = TRUE
  # )
  # checkmate::assert_choice(time_interval,
  #   choices = c("days","weeks","months","quarters","years"),
  #   add = error_message
  # )
  # checkmate::assert_numeric(outcome_washout_window,
  #   add = error_message,
  #   null.ok = TRUE
  # )
  # checkmate::assert_logical(repetitive_events,
  #   add = error_message
  # )
  # checkmate::assert_logical(verbose,
  #   add = error_message
  # )
  # # report initial assertions
  # checkmate::reportAssertions(collection = error_message)
  #
  # if (verbose == TRUE) {
  #   message("Inputs checked and all initial assertions passed")
  # }

  ## Analysis code
  # bring in study population
  study_pop <- cdm[[table_name_denominator]]
  # if (!is.null(cohort_id_denominator_pop)) {
  study_pop <- study_pop %>%
    dplyr::filter(.data$cohort_definition_id ==
      .env$cohort_id_denominator_pop) %>%
    dplyr::select(-"cohort_definition_id") %>%
    dplyr::compute()

  # }

  # # check population n is above zero
  # # return error if not
  # error_message <- checkmate::makeAssertCollection()
  # checkmate::assertTRUE(nrow(study_pop) > 0,
  #   add = error_message
  # )
  # if (!nrow(study_pop) > 0) {
  #   error_message$push(
  #     glue::glue("- Zero rows in study_denominator_pop with
  #     cohort_id_denominator_pop={cohort_id_denominator_pop}")
  #   )
  # }
  # checkmate::reportAssertions(collection = error_message)
  #
  #   if (verbose == TRUE) {
  #     message("Check passed: one or more people in denominator")
  #   }


  #  link to outcome cohort
  # if (!is.null(cohort_id_outcome)) {
  #   outcome_db <- outcome_db %>%
  #     dplyr::filter(.data$cohort_definition_id == .env$cohort_id_outcome) %>%
  #     dplyr::compute()
  # }
  # error_message <- checkmate::makeAssertCollection()
  # checkmate::assertTRUE(outcome_db %>% dplyr::tally() %>% dplyr::pull() > 0,
  #   add = error_message
  # )
  # if (!(outcome_db %>% dplyr::tally() %>% dplyr::pull() > 0)) {
  #   error_message$push(
  #     glue::glue("- Zero rows for cohort_id_outcome={cohort_id_outcome}")
  #   )
  # }
  # checkmate::reportAssertions(collection = error_message)

  # keep outcomes of people in the denominator
  outcome <- cdm[[table_name_outcome]] %>%
    dplyr::filter(.data$outcome_id == .env$cohort_id_outcome) %>%
    dplyr::select(-"outcome_id") %>%
    dplyr::inner_join(study_pop,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::compute()

  # bring outcomes into memory
  if (verbose == TRUE) {
    message("Bringing outcomes into memory")
  }

  # start date
  start_date <- min(dplyr::pull(study_pop, "cohort_start_date"), na.rm = TRUE)
  # end date to the last day of last available full period
  end_date <- max(dplyr::pull(study_pop, "cohort_end_date"), na.rm = TRUE)

  # study dates
  study_days <- compute_study_days_inc(
    start_date = start_date,
    end_date = end_date,
    time_interval = time_interval
  )

  if (nrow(study_days) == 0) {
    stop("Not enough following to compute the desired incidence.")
  }

  study_pop <- study_pop %>%
    dplyr::anti_join(
      outcome,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::mutate(outcome_start_date = as.Date(NA))

  outcome <- outcome %>%
    dplyr::mutate(cohort_end_date = dplyr::if_else(
      !is.na(.data$outcome_start_date) & .data$outcome_start_date < .data$cohort_end_date,
      .data$outcome_start_date,
      .data$cohort_end_date
    ))

  if (is.null(outcome_washout_window)) {
    outcome <- outcome %>%
      dplyr::filter(is.na(.data$outcome_prev_end_date)) %>%
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)
  } else {
    outcome <- outcome %>%
      dplyr::mutate(outcome_prev_end_date = dplyr::if_else(
        is.na(.data$outcome_prev_end_date),
        .data$outcome_prev_end_date,
        as.Date(.data$outcome_prev_end_date + lubridate::days(1) + lubridate::days(.env$outcome_washout_window))
      )) %>%
      dplyr::mutate(cohort_start_date = dplyr::if_else(
        is.na(.data$outcome_prev_end_date) |
          (.data$cohort_start_date > .data$outcome_prev_end_date),
        .data$cohort_start_date,
        .data$outcome_prev_end_date
      )) %>%
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)
    if (repetitive_events == FALSE) {
      outcome <- outcome %>%
        dplyr::group_by(.data$subject_id) %>%
        dplyr::filter(is.na(.data$outcome_prev_end_date) |
          .data$outcome_start_date == min(.data$outcome_start_date, na.rm = TRUE)) %>%
        dplyr::ungroup()
    }
  }

  study_pop <- study_pop %>%
    dplyr::union_all(outcome %>%
      dplyr::select(-"outcome_prev_end_date") %>%
      dplyr::mutate(cohort_end_date = dplyr::if_else(
        !is.na(.data$outcome_start_date),
        .data$outcome_start_date,
        .data$cohort_end_date
      ))) %>%
    dplyr::collect()

  # fetch incidence rates
  # looping through each time interval
  ir <- list()
  for (i in seq_along(1:nrow(study_days))) {
    working_t_name <- study_days$time[i]
    working_t_start <- study_days$start_time[i]
    working_t_end <- study_days$end_time[i]
    working_t_days <- as.numeric(working_t_end - working_t_start) + 1

    # drop people with end_date prior to working_t_start
    # drop people with start_date after working_t_end
    working_pop <- study_pop %>%
      dplyr::filter(.data$cohort_end_date >= .env$working_t_start) %>%
      dplyr::filter(.data$cohort_start_date <= .env$working_t_end)

    # individuals start date for this period
    # which could be start of the period or later
    working_pop <- working_pop %>%
      dplyr::mutate(t_start_date = dplyr::if_else(.data$cohort_start_date <=
        .env$working_t_start, .env$working_t_start,
      .data$cohort_start_date
      ))

    # individuals end date for this period
    # end of the period or earlier
    working_pop <- working_pop %>%
      dplyr::mutate(
        t_end_date =
          dplyr::if_else(.data$cohort_end_date >= .env$working_t_end,
            .env$working_t_end,
            .data$cohort_end_date
          )
      )

    working_pop <- working_pop %>%
      dplyr::select("subject_id", "t_start_date", "t_end_date", "outcome_start_date")

    # compute working days and
    # erase outcome_start_date if not
    # inside interval
    working_pop <- working_pop %>%
      dplyr::mutate(working_days = as.numeric(difftime(.data$t_end_date + lubridate::days(1),
        .data$t_start_date,
        units = "days"
      ))) %>%
      dplyr::mutate(outcome_start_date = dplyr::if_else(.data$outcome_start_date <= .data$t_end_date,
        .data$outcome_start_date,
        as.Date(NA),
        .data$outcome_start_date
      )) %>%
      dplyr::mutate(outcome_start_date = dplyr::if_else(.data$outcome_start_date >= .data$t_start_date,
        .data$outcome_start_date,
        as.Date(NA),
        .data$outcome_start_date
      ))

    ir[[paste0(i)]] <- working_pop %>%
      dplyr::summarise(
        n_persons = dplyr::n_distinct(.data$subject_id),
        person_days = sum(.data$working_days),
        n_events = sum(!is.na(.data$outcome_start_date))
      ) %>%
      dplyr::mutate(person_years = .data$person_days / 365.25) %>%
      dplyr::mutate(ir_100000_pys = (.data$n_events / .data$person_years) * 100000) %>%
      dplyr::mutate(time = .env$working_t_name) %>%
      dplyr::mutate(start_time = .env$working_t_start) %>%
      dplyr::mutate(end_time = .env$working_t_end)
  }

  ir <- dplyr::bind_rows(ir)

  # study design related variables
  analysis_settings <- tibble::tibble(
    outcome_washout_window = .env$outcome_washout_window,
    repetitive_events = .env$repetitive_events,
    time_interval = .env$time_interval
  )

  study_pop <- study_pop %>%
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date")

  # return list
  results <- list()
  results[["ir"]] <- ir
  results[["analysis_settings"]] <- analysis_settings
  results[["person_table"]] <- study_pop
  results[["attrition"]] <- tibble::tibble(attrition = "attrition") # placeholder

  return(results)
}
