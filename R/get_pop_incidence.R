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
    study_days <- dplyr::tibble(start_time = seq.Date(from = start_date,
                                                  to = end_date,
                                                  by = "days")) %>%
      dplyr::mutate(day = lubridate::day(.data$start_time)) %>%
      dplyr::mutate(month = lubridate::month(.data$start_time)) %>%
      dplyr::mutate(year = lubridate::year(.data$start_time)) %>%
      dplyr::mutate(time = dplyr::if_else(.data$month < 10,
                                        paste0(.data$year,"_0",.data$month),
                                        paste0(.data$year,"_",.data$month))) %>%
      dplyr::mutate(time = dplyr::if_else(.data$day < 10,
                                        paste0(.data$time,"_0",.data$day),
                                        paste0(.data$time,"_",.data$day))) %>%
      dplyr::mutate(end_time = .data$start_time) %>%
      dplyr::select("time","start_time","end_time")
  } else {
    study_days <- dplyr::tibble(dates = seq.Date(from = start_date,
                                               to = end_date,
                                               by = "days")) %>%
      dplyr::mutate(isoweek = lubridate::isoweek(.data$dates)) %>%
      dplyr::mutate(month = lubridate::month(.data$dates)) %>%
      dplyr::mutate(quarter = quarters(.data$dates)) %>%
      dplyr::mutate(year = lubridate::year(.data$dates)) %>%
      dplyr::mutate(years = glue::glue("{year}")) %>%
      dplyr::mutate(months = dplyr::if_else(.data$month < 10,
                                          paste0(.data$year,"_0",.data$month),
                                          paste0(.data$year,"_",.data$month))) %>%
      dplyr::mutate(quarters = glue::glue("{year}_{quarter}")) %>%
      dplyr::mutate(year = dplyr::if_else(.data$month == 1 & .data$isoweek > 50,
                                        .data$year - 1,
                                        .data$year)) %>%
      dplyr::mutate(weeks = dplyr::if_else(.data$isoweek < 10,
                                         paste0(.data$year,"_0",.data$isoweek),
                                         paste0(.data$year,"_",.data$isoweek))) %>%
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
#' @param cdm_ref CDMConnector CDM reference object
#' @param table_name_outcome table_name_outcome
#' @param cohort_id_outcome cohort_id_outcome
#' @param study_denominator_pop study_denominator_pop
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
get_pop_incidence <- function(cdm_ref,
                              table_name_outcome,
                              cohort_id_outcome = NULL,
                              study_denominator_pop,
                              cohort_id_denominator_pop = NULL,
                              time_interval = c("months"),
                              outcome_washout_window = NULL,
                              repetitive_events = FALSE,
                              verbose = FALSE) {
  if (!is.null(outcome_washout_window)) {
    if (is.na(outcome_washout_window)) {
      outcome_washout_window <- NULL
    }
  }


  # help to avoid formatting errors
  if (is.numeric(cohort_id_outcome)) {
    cohort_id_outcome <- as.character(cohort_id_outcome)
  }
  if (is.numeric(cohort_id_denominator_pop)) {
    cohort_id_denominator_pop <- as.character(cohort_id_denominator_pop)
  }
  if (is.character(time_interval)) {
    time_interval <- tolower(time_interval)
  }

  ## check for standard types of user error
  error_message <- checkmate::makeAssertCollection()
  db_inherits_check <- inherits(cdm_ref, "cdm_reference")
  checkmate::assertTRUE(db_inherits_check,
    add = error_message
  )
  if (!isTRUE(db_inherits_check)) {
    error_message$push(
      "- cdm_ref must be a CDMConnector CDM reference object"
    )
  }
  checkmate::assert_character(cohort_id_outcome,
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
  checkmate::assertTRUE(all(c(
    "cohort_definition_id",
    "person_id",
    "cohort_start_date", "cohort_end_date"
  ) %in%
    names(study_denominator_pop)))

  checkmate::assert_character(cohort_id_denominator_pop,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_choice(time_interval,
    choices = c("days","weeks","months","quarters","years"),
    add = error_message
  )
  checkmate::assert_numeric(outcome_washout_window,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_logical(repetitive_events,
    add = error_message
  )
  checkmate::assert_logical(verbose,
    add = error_message
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)

  if (verbose == TRUE) {
    message("Inputs checked and all initial assertions passed")
  }

  ## Analysis code
  # bring in study population
  study_pop <- study_denominator_pop
  if (!is.null(cohort_id_denominator_pop)) {
    study_pop <- study_pop %>%
      dplyr::filter(.data$cohort_definition_id ==
        .env$cohort_id_denominator_pop)
  }

  # check population n is above zero
  # return error if not
  error_message <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(nrow(study_pop) > 0,
    add = error_message
  )
  if (!nrow(study_pop) > 0) {
    error_message$push(
      glue::glue("- Zero rows in study_denominator_pop with
      cohort_id_denominator_pop={cohort_id_denominator_pop}")
    )
  }
  checkmate::reportAssertions(collection = error_message)

  if (verbose == TRUE) {
    message("Check passed: one or more people in denominator")
  }


  #  link to outcome cohort
  outcome_db <- cdm_ref[[table_name_outcome]]
  if (!is.null(cohort_id_outcome)) {
    outcome_db <- outcome_db %>%
      dplyr::filter(.data$cohort_definition_id == .env$cohort_id_outcome) %>%
      dplyr::compute()
  }
  error_message <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(outcome_db %>% dplyr::tally() %>% dplyr::pull() > 0,
    add = error_message
  )
  if (!(outcome_db %>% dplyr::tally() %>% dplyr::pull() > 0)) {
    error_message$push(
      glue::glue("- Zero rows for cohort_id_outcome={cohort_id_outcome}")
    )
  }
  checkmate::reportAssertions(collection = error_message)

  # bring outcomes into memory
  if (verbose == TRUE) {
    message("Bringing outcomes into memory")
  }
  outcome <- outcome_db %>%
    dplyr::rename("person_id" = "subject_id") %>%
    dplyr::rename("outcome_start_date" = "cohort_start_date") %>%
    dplyr::rename("outcome_end_date" = "cohort_end_date") %>%
    dplyr::select("person_id", "outcome_start_date", "outcome_end_date") %>%
    dplyr::collect()

  # start date
  start_date <- min(study_pop$cohort_start_date)
  # end date to the last day of last available full period
  end_date <- max(study_pop$cohort_end_date)

  # study dates
  study_days <- compute_study_days_inc(
    start_date = start_date,
    end_date = end_date,
    time_interval = time_interval
    )

  if (nrow(study_days) == 0) {
    stop("Not enough following to compute the desired incidence.")
  }

  # select only the interesting variables of study_pop
  study_pop <- study_pop %>%
    dplyr::select("person_id", "cohort_start_date", "cohort_end_date")
  # get only the outcomes of the study population
  outcome <- outcome %>%
    dplyr::inner_join(study_pop %>%
      dplyr::select("person_id") %>%
      dplyr::distinct(),
    by = "person_id"
    )
  # get study dates of the individuals that present an outcome
  study_pop_outcome <- study_pop %>%
    dplyr::inner_join(outcome %>%
      dplyr::select("person_id") %>%
      dplyr::distinct(),
    by = "person_id"
    )

  if (nrow(outcome) > 0) {
    if (is.null(outcome_washout_window)) {
      study_pop <- study_pop %>%
        dplyr::anti_join(outcome, by = "person_id") %>%
        dplyr::mutate(outcome_start_date = base::as.Date(NA)) %>%
        dplyr::union_all(outcome %>%
          dplyr::group_by(.data$person_id) %>%
          dplyr::summarise(outcome_start_date = min(.data$outcome_start_date, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::left_join(study_pop_outcome,
            by = "person_id"
          ) %>%
          dplyr::mutate(cohort_end_date = dplyr::if_else(
            .data$outcome_start_date < .data$cohort_end_date,
            .data$outcome_start_date,
            .data$cohort_end_date
          )) %>%
          dplyr::filter(.data$cohort_end_date >= .data$cohort_start_date))
    } else {
      if (repetitive_events == TRUE) {
        # add the study dates to the outcome
        outcome <- outcome %>%
          dplyr::left_join(study_pop_outcome,
            by = "person_id"
          )
        # get last event previous the start date
        outcome_previous <- outcome %>%
          dplyr::filter(.data$outcome_start_date < .data$cohort_start_date) %>%
          dplyr::select("person_id", "outcome_end_date", "cohort_start_date")
        if (nrow(outcome_previous) > 0) {
          outcome_previous <- outcome_previous %>%
            dplyr::group_by(.data$person_id, .data$cohort_start_date) %>%
            dplyr::summarise(outcome_end_date = max(.data$outcome_end_date)) %>%
            dplyr::ungroup()
        }
        # get event after the start date in the period
        outcome_post <- outcome %>%
          dplyr::filter(.data$outcome_start_date >= .data$cohort_start_date) %>%
          dplyr::filter(.data$outcome_start_date <= .data$cohort_end_date) %>%
          dplyr::select("person_id", "cohort_start_date", "outcome_start_date", "outcome_end_date")

        if (nrow(outcome_post) > 0) {
          # get first event after the start date
          outcome_first <- outcome_post %>%
            dplyr::select(-"outcome_end_date") %>%
            dplyr::group_by(.data$person_id, .data$cohort_start_date) %>%
            dplyr::filter(.data$outcome_start_date == min(.data$outcome_start_date, na.rm = TRUE)) %>%
            dplyr::ungroup()
          # sort outcomes inside the observation period
          outcome_post <- outcome_post %>%
            dplyr::group_by(.data$person_id, .data$cohort_start_date) %>%
            dplyr::arrange(.data$outcome_start_date) %>%
            dplyr::mutate(index = dplyr::row_number()) %>%
            dplyr::ungroup()
        }

        if (nrow(outcome_post) == 0) {
          outcome_pairs <- outcome_previous %>%
            dplyr::rename("outcome_end_date_prev" = "outcome_end_date") %>%
            dplyr::mutate(outcome_start_date = as.Date(NA))
        } else if (nrow(outcome_previous) == 0) {
          outcome_pairs <- outcome_first %>%
            dplyr::mutate(outcome_end_date_prev = as.Date(NA)) %>%
            dplyr::union_all(outcome_post %>%
                               dplyr::select(-"outcome_end_date") %>%
                               dplyr::right_join(outcome_post %>%
                                                   dplyr::mutate(index = .data$index + 1) %>%
                                                   dplyr::rename("outcome_end_date_prev" = "outcome_end_date") %>%
                                                   dplyr::select("person_id", "outcome_end_date_prev", "index", "cohort_start_date"),
                                                 by = c("person_id", "index", "cohort_start_date")
                               ) %>%
                               dplyr::select("person_id", "outcome_end_date_prev", "cohort_start_date", "outcome_start_date"))
        } else {
          # get the pairs of previous + first outcome
          outcome_pairs <- outcome_previous %>%
            dplyr::rename("outcome_end_date_prev" = "outcome_end_date") %>%
            dplyr::full_join(outcome_first, by = c("person_id", "cohort_start_date"))
          # get next pairs
          outcome_pairs <- outcome_pairs %>%
            dplyr::union_all(outcome_post %>%
              dplyr::select(-"outcome_end_date") %>%
              dplyr::right_join(outcome_post %>%
                dplyr::mutate(index = .data$index + 1) %>%
                dplyr::rename("outcome_end_date_prev" = "outcome_end_date") %>%
                dplyr::select("person_id", "outcome_end_date_prev", "index", "cohort_start_date"),
              by = c("person_id", "index", "cohort_start_date")
              ) %>%
              dplyr::select("person_id", "outcome_end_date_prev", "cohort_start_date", "outcome_start_date"))
        }
      } else {
        # The same but just the first pair
        # add the study dates to the outcome
        outcome <- outcome %>%
          dplyr::left_join(study_pop_outcome %>%
            dplyr::group_by(.data$person_id) %>%
            dplyr::filter(.data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)) %>%
            dplyr::ungroup(),
          by = "person_id"
          )
        # get last event previous the start date
        outcome_previous <- outcome %>%
          dplyr::filter(.data$outcome_start_date < .data$cohort_start_date) %>%
          dplyr::select("person_id", "outcome_end_date", "cohort_start_date")
        if (nrow(outcome_previous) > 0) {
          outcome_previous <- outcome_previous %>%
            dplyr::group_by(.data$person_id) %>%
            dplyr::filter(.data$outcome_end_date == max(.data$outcome_end_date)) %>%
            dplyr::ungroup()
        }
        # get event after the start date
        outcome_post <- outcome %>%
          dplyr::filter(.data$outcome_start_date >= .data$cohort_start_date) %>%
          dplyr::filter(.data$outcome_start_date <= .data$cohort_end_date) %>%
          dplyr::select(-"outcome_end_date")
        if (nrow(outcome_post) > 0) {
          outcome_post <- outcome_post %>%
            dplyr::group_by(.data$person_id) %>%
            dplyr::filter(.data$outcome_start_date == min(.data$outcome_start_date, na.rm = TRUE)) %>%
            dplyr::ungroup()
        }
        # get the pairs of previous + first outcome
        if (nrow(outcome_post) == 0) {
          outcome_pairs <- outcome_previous %>%
            dplyr::rename("outcome_end_date_prev" = "outcome_end_date") %>%
            dplyr::mutate(outcome_start_date = as.Date(NA))
        } else if (nrow(outcome_previous) == 0) {
          outcome_pairs <- outcome_post %>%
            dplyr::select(-"cohort_end_date") %>%
            dplyr::mutate(outcome_end_date_prev = as.Date(NA))
        } else {
          outcome_pairs <- outcome_previous %>%
            dplyr::rename("outcome_end_date_prev" = "outcome_end_date") %>%
            dplyr::full_join(outcome_post, by = c("person_id", "cohort_start_date")) %>%
            dplyr::select("person_id", "outcome_end_date_prev", "cohort_start_date", "outcome_start_date")
        }
      }
      study_pop <- study_pop %>%
        dplyr::anti_join(outcome_pairs %>%
          dplyr::select("person_id"),
        by = "person_id"
        ) %>%
        dplyr::mutate(outcome_start_date = base::as.Date(NA)) %>%
        dplyr::union_all(outcome_pairs %>%
          dplyr::inner_join(study_pop,
            by = c("person_id", "cohort_start_date")
          ) %>%
          dplyr::mutate(outcome_end_date_prev = .data$outcome_end_date_prev + outcome_washout_window + 1) %>% # I think that it needs a + 1
          dplyr::mutate(cohort_start_date = dplyr::if_else(.data$cohort_start_date > .data$outcome_end_date_prev,
            .data$cohort_start_date,
            .data$outcome_end_date_prev,
            .data$cohort_start_date
          )) %>%
          dplyr::mutate(cohort_end_date = dplyr::if_else(.data$outcome_start_date < .data$cohort_end_date,
            .data$outcome_start_date,
            .data$cohort_end_date,
            .data$cohort_end_date
          )) %>%
          dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) %>%
          dplyr::select(-"outcome_end_date_prev"))
    }
  } else {
    study_pop <- study_pop %>%
      dplyr::mutate(outcome_start_date = as.Date(NA))
  }

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
      dplyr::select("person_id", "t_start_date", "t_end_date","outcome_start_date")

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
        n_persons = length(unique(working_pop$person_id)),
        person_days = sum(.data$working_days),
        person_years = (.data$person_days / 365.25),
        n_events = sum(!is.na(.data$outcome_start_date))
      ) %>%
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
      time_interval = .env$time_interval)

  study_pop <- study_pop %>%
    dplyr::select("person_id","cohort_start_date","cohort_end_date")

  # return list
  results <- list()
  results[["ir"]] <- ir
  results[["analysis_settings"]] <- analysis_settings
  results[["person_table"]] <- study_pop
  results[["attrition"]] <- tibble::tibble(attrition = "attrition") # placeholder

  return(results)
}
