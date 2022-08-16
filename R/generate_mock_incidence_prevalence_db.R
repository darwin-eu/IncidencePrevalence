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


#' Generate example database for IncidencePrevalence package
#'
#' @param person Person table.
#' @param observation_period observation_period table.
#' @param outcome outcome table.
#' @param sample_size number of unique patient
#' @param out_pre % of patient with an event
#' @param seed seed for simulating the dataset use same seed to get same dataset
#' @return DBIConnection to duckdb database with mock data
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(tibble)
#' library(IncidencePrevalence)
#' person_example <- tibble(
#'   person_id = "1",
#'   gender_concept_id = "8507",
#'   year_of_birth = 2000,
#'   month_of_birth = 06,
#'   day_of_birth = 01
#' )
#' observation_period_example <- tibble(
#'   observation_period_id = "1",
#'   person_id = "1",
#'   observation_period_start_date = as.Date("2010-01-01"),
#'   observation_period_end_date = as.Date("2015-06-01")
#' )
#' db <- generate_mock_incidence_prevalence_db(
#'   person = person_example,
#'   observation_period = observation_period_example
#' )
#' }
#'
generate_mock_incidence_prevalence_db <- function(person = NULL,
                                                  observation_period = NULL,
                                                  outcome = NULL,
                                                  sample_size = 1,
                                                  out_pre = 1,
                                                  seed = 444) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_tibble(person, null.ok = TRUE)
  checkmate::assert_tibble(observation_period, null.ok = TRUE)
  checkmate::assert_tibble(outcome, null.ok = TRUE)
  checkmate::assert_int(sample_size, lower = 1)
  checkmate::assert_numeric(out_pre, lower = 0, upper = 1)
  checkmate::assert_int(seed, lower = 1)
  checkmate::reportAssertions(collection = errorMessage)

  set.seed(seed)

  if (is.null(person) | is.null(observation_period)) {
    # person table
    id <- as.character(seq(1:sample_size))
    # person gender
    gender_id <- sample(c("8507", "8532"),
      sample_size,
      replace = TRUE
    )
    # person date of birth
    # random date of birth
    DOB <- sample(seq(as.Date("1920-01-01"),
      as.Date("2000-01-01"),
      by = "day"
    ),
    sample_size,
    replace = TRUE
    )
    # year, month, day
    DOB_year <- as.numeric(format(DOB, "%Y"))
    DOB_month <- as.numeric(format(DOB, "%m"))
    DOB_day <- as.numeric(format(DOB, "%d"))

    # observation_period table
    # create a list of observational_period_id
    obs_start_date <-
      sample(seq(as.Date("2005-01-01"), as.Date("2010-01-01"), by = "day"),
        sample_size,
        replace = TRUE
      ) # start date for the period
    obs_end_date <- obs_start_date + lubridate::days(sample(1:1000,
      sample_size,
      replace = TRUE
    ))
    if (is.null(person)) {
      person <- tibble::tibble(
        person_id = id,
        gender_concept_id = gender_id,
        year_of_birth = DOB_year,
        month_of_birth = DOB_month,
        day_of_birth = DOB_day
      )
    }

    if (is.null(observation_period)) {
      observation_period <- tibble::tibble(
        observation_period_id = id,
        person_id = id,
        observation_period_start_date = obs_start_date,
        observation_period_end_date = obs_end_date
      )
    }
  }

  if (is.null(outcome)) {
    # outcome table
    # note, only one outcome cohort
    subject_id <- sample(person$person_id,
      round(nrow(person) * out_pre, digits = 0),
      replace = FALSE
    )

    outcome <- observation_period %>%
      dplyr::rename("subject_id" = "person_id") %>%
      dplyr::filter(.data$subject_id %in% .env$subject_id) %>%
      dplyr::mutate(obs_days = as.numeric(difftime(.data$observation_period_end_date,
        .data$observation_period_start_date,
        units = "days"
      ))) %>%
      dplyr::mutate(days_to_outcome = round(stats::runif(length(.env$subject_id),
        min = 1,
        max = .data$obs_days
      ))) %>%
      dplyr::mutate(cohort_start_date = .data$observation_period_start_date +
        .data$days_to_outcome) %>%
      dplyr::mutate(cohort_end_date = .data$cohort_start_date +
        lubridate::days(1)) %>%
      dplyr::select(
        "subject_id",
        "cohort_start_date",
        "cohort_end_date"
      ) %>%
      dplyr::mutate(cohort_definition_id = c("1")) %>%
      dplyr::relocate(.data$cohort_definition_id)
  }

  # into in-memory database
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person",
      person,
      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db,
      "observation_period",
      observation_period,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "outcome",
      outcome,
      overwrite = TRUE
    )
  })


  return(db)
}
