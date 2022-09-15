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


#' Get population prevalence estimates
#'
#' @param db db
#' @param results_schema_outcome results_schema_outcome
#' @param table_name_outcome table_name_outcome
#' @param cohort_id_outcome cohort_id_outcome
#' @param study_denominator_pop study_denominator_pop
#' @param cohort_id_denominator_pop cohort_id_denominator_pop
#' @param period period
#' @param time_interval time_interval
#' @param minimum_representative_proportion minimum_representative_proportion
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
get_pop_prevalence <- function(db,
                               results_schema_outcome,
                               table_name_outcome,
                               cohort_id_outcome = NULL,
                               study_denominator_pop,
                               cohort_id_denominator_pop = NULL,
                               period = "point",
                               time_interval = c("months"),
                               minimum_representative_proportion = 0.5,
                               verbose = FALSE) {

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
  db_inherits_check <- inherits(db, "DBIConnection")
  checkmate::assertTRUE(db_inherits_check,
    add = error_message
  )
  if (!isTRUE(db_inherits_check)) {
    error_message$push(
      "- db must be a database connection via DBI::dbConnect()"
    )
  }
  checkmate::assert_character(results_schema_outcome,
    add = error_message,
    null.ok = TRUE
  )
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
  checkmate::assert_choice(period,
    choices = c("point", "month", "year"),
    add = error_message
  )
  checkmate::assert_choice(time_interval,
    choices = c("months", "years"),
    add = error_message
  )
  checkmate::assert_logical(verbose,
    add = error_message
  )
  checkmate::assert_numeric(minimum_representative_proportion,
    add = error_message,
    lower = 0,
    upper = 1
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)

  if (verbose == TRUE) {
    message("Inputs checked and all initial assertions passed")
  }

  ## Analysis code
  # bring in study popupulation
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

  # link to outcome cohort
  if (!is.null(results_schema_outcome)) {
    outcome_db <- dplyr::tbl(db, dplyr::sql(glue::glue(
      "SELECT * FROM {results_schema_outcome}.{table_name_outcome}"
    )))
  } else {
    outcome_db <- dplyr::tbl(db, "outcome")
  }
  error_message <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(outcome_db %>% dplyr::tally() %>% dplyr::pull() > 0,
    add = error_message
  )
  if (!outcome_db %>% dplyr::tally() %>% dplyr::pull() > 0) {
    error_message$push(
      glue::glue("- Zero rows in {results_schema_outcome}.{table_name_outcome}")
    )
  }
  checkmate::reportAssertions(collection = error_message)

  if (!is.null(cohort_id_outcome)) {
    outcome_db <- outcome_db %>%
      dplyr::filter(.data$cohort_definition_id == .env$cohort_id_outcome) %>%
      dplyr::compute()
  }
  error_message <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(outcome_db %>% dplyr::tally() %>% dplyr::pull() > 0,
    add = error_message
  )
  if (!outcome_db %>% dplyr::tally() %>% dplyr::pull() > 0) {
    error_message$push(
      glue::glue("- Zero rows in {results_schema_outcome}.{table_name_outcome}
                 for cohort_id_outcome={cohort_id_outcome}")
    )
  }
  checkmate::reportAssertions(collection = error_message)

  if (verbose == TRUE) {
    message("Check passed: one or more outcomes identified")
  }

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
  if (time_interval == "years") {
    end_date <- lubridate::floor_date(max(study_pop$cohort_end_date) +
                                        lubridate::days(1),
      unit = "years"
    ) - lubridate::days(1)
  }
  if (time_interval == "months") {
    end_date <- lubridate::floor_date(max(study_pop$cohort_end_date) +
                                        lubridate::days(1),
      unit = "months"
    ) - lubridate::days(1)
  }

  # will give error if no full months/ years
  error_message <- checkmate::makeAssertCollection()
  if (time_interval == "years") {
    n_time <- lubridate::interval(
      lubridate::ymd(start_date),
      lubridate::ymd(end_date)
    ) %/%
      lubridate::years(1)

    checkmate::assertTRUE(n_time > 0,
      add = error_message
    )
    if (!n_time > 0) {
      error_message$push(
        glue::glue("- Less than one full year between earliest study_start_date
                   and last study_end_date in study_denominator_pop")
      )
    }
  }
  if (time_interval == "months") {
    n_time <- lubridate::interval(
      lubridate::ymd(start_date),
      lubridate::ymd(end_date)
    ) %/%
      months(1)

    checkmate::assertTRUE(n_time > 0,
      add = error_message
    )
    if (!n_time > 0) {
      error_message$push(
        glue::glue("- Less than one full month between earliest study_start_date
                   and last study_end_date in study_denominator_pop")
      )
    }
  }
  checkmate::reportAssertions(collection = error_message)

  # fetch prevalence
  # looping through each time interval
  pr <- list()
  for (i in seq_along(1:(n_time + 1))) {
    if (time_interval == "years") {
      working_t_start <- start_date + lubridate::years(i - 1)
    }
    if (time_interval == "months") {
      working_t_start <- start_date + months(i - 1)
    }
    if (period == "point") {
      working_t_end <- working_t_start
    }
    if (period == "month") {
      working_t_end <- working_t_start + months(1) - lubridate::days(1)
    }
    if (period == "year") {
      working_t_end <- working_t_start + lubridate::years(1) - lubridate::days(1)
    }
    working_period <- as.numeric(working_t_end - working_t_start) + 1

    # drop people with end_date prior to working_t_start
    # drop people with start_date after working_t_end
    working_pop <- study_pop %>%
      dplyr::filter(.data$cohort_end_date >= .env$working_t_start) %>%
      dplyr::filter(.data$cohort_start_date <= .env$working_t_end)

    # individuals start date for this period
    # which could be start of the period or later
    working_pop <- working_pop %>%
      dplyr::mutate(t_start_date =
          dplyr::if_else(.data$cohort_start_date <= .env$working_t_start,
        .env$working_t_start,
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
      dplyr::left_join(outcome,
        by = "person_id"
      )

    working_pop <- working_pop %>%
      dplyr::select("person_id", "t_start_date",
                    "t_end_date", "outcome_start_date", "outcome_end_date")

    working_pop <- working_pop %>%
      dplyr::mutate(contribution = (as.numeric(difftime(.data$t_end_date,
                                                        .data$t_start_date,
                                                        units = "days")) + 1) /
                      working_period) %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(contribution = sum(.data$contribution)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$contribution >= .env$minimum_representative_proportion)

    denominator <- working_pop %>%
      dplyr::select("person_id") %>%
      dplyr::distinct() %>%
      nrow()

    numerator <- working_pop %>%
      dplyr::filter(.data$outcome_start_date <= .data$t_end_date) %>%
      dplyr::filter(.data$outcome_end_date >= .data$t_start_date) %>%
      dplyr::select("person_id") %>%
      dplyr::distinct() %>%
      nrow()

    pr[[paste0(i)]] <- dplyr::tibble(
      numerator = numerator,
      denominator = denominator,
      prev = numerator / denominator,
      calendar_month = ifelse(time_interval == "months",
        lubridate::month(working_t_start),
        NA
      ),
      calendar_year = lubridate::year(working_t_start)
    )
  }

  pr <- dplyr::bind_rows(pr)

  # study design related variables
  # add study design related variables
  analysis_settings <- tibble::tibble(
    period = .env$period,
    time_interval = .env$time_interval)

  study_pop <- study_pop %>%
    dplyr::select("person_id","cohort_start_date","cohort_end_date")

  results<-list()
  results[["pr"]]<-pr
  results[["analysis_settings"]]<-analysis_settings
  results[["person_table"]]<-study_pop
  results[["attrition"]]<-tibble::tibble(attrition="attrition") # placeholder

  return(results)
}
