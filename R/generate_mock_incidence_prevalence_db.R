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
                                                  sample_size = 100,
                                                  out_pre = 0.1,
                                                  seed = 444) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_tibble(person, null.ok = TRUE)
  checkmate::assert_tibble(observation_period, null.ok = TRUE)
  checkmate::assert_tibble(outcome, null.ok = TRUE)
  checkmate::assert_int(sample_size, lower = 1)
  checkmate::assert_numeric(out_pre, lower = 0, upper = 1)
  checkmate::assert_int(seed, lower = 1)
  checkmate::reportAssertions(collection = errorMessage)

  sample_size <- sample_size
  Prevalance <- out_pre
  unique_obs <- round(max(1, sample_size * 0.1), digits = 0)
  set.seed(seed)
  # person table
  id <- as.character(seq(1:sample_size))
  # person gender
  gender_concept_list <- c("8507", "8532")
  gender_id <-
    sample(gender_concept_list, sample_size, replace = TRUE)

  ##### person date of birth (DOB)####

  DOB <-
    sample(seq(as.Date("1920-01-01"), as.Date("2000-01-01"), by = "day"), sample_size, replace = TRUE) # creating a list of random person date of birth
  # list for year, month, day
  DOB_year <- as.numeric(format(DOB, "%Y"))
  DOB_month <- as.numeric(format(DOB, "%m"))
  DOB_day <- as.numeric(format(DOB, "%d"))

  # observation_period table
  obs_id <-
    as.character(seq(1:unique_obs)) # create a list of observational_period_id
  obs_start_date <-
    sample(seq(as.Date("2005-01-01"), as.Date("2010-01-01"), by = "day"), unique_obs, replace = TRUE) # start date for the period
  obs_end_date <-
    sample(seq(as.Date(max(obs_start_date)), as.Date("2021-01-01"), by = "day"), length(obs_start_date), replace = TRUE) # end date for the period
  obs_id_assign <-
    sample(obs_id, sample_size, replace = TRUE) # assigning observational_period_id to person
  obs_start_date_assign <-
    obs_start_date[as.numeric(obs_id_assign)] # assigning start date to person
  obs_end_date_assign <-
    obs_end_date[as.numeric(obs_id_assign)] # assigning end date to person

  # outcome table


  cohort_definition_list <-
    c("1") # define element in cohort definition
  cohort_definition <-
    sample(cohort_definition_list,
      round(sample_size * Prevalance, digits = 0),
      replace = TRUE
    ) # generate list of cohort definition to the prevalance level
  subject_id <-
    sample(id, round(sample_size * Prevalance, digits = 0), replace = FALSE)
  subject_date <-
    lapply(subject_id, function(i) {
      sample(seq(
        as.Date(obs_start_date_assign[as.numeric(i)]),
        as.Date(obs_end_date_assign[as.numeric(i)]),
        by = "day"
      ), 1)
    }) # generate list of cohort start date between obs_start and obs_end date

  cohort_start <- do.call("c", subject_date)

  subject_obs_end <-
    obs_end_date_assign[as.numeric(subject_id)] # subject obs end date for subject with an outcome


  # Loop to create cohort_end_date
  cohort_end <- list()
  for (i in 1:length(cohort_start)) {
    cohort_end[i] <- as.character(sample(seq(
      as.Date(cohort_start[i]),
      as.Date(subject_obs_end[i]),
      by = "day"
    ), 1))
  }


  cohort_end <- unlist(cohort_end)





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
      observation_period_id = obs_id_assign,
      person_id = id,
      observation_period_start_date = as.Date(obs_start_date_assign),
      observation_period_end_date = as.Date(obs_end_date_assign)
    )
  }

  if (is.null(outcome)) {
    outcome <- tibble::tibble(
      cohort_definition_id = cohort_definition,
      subject_id = subject_id,
      cohort_start_date = cohort_start,
      ## cohort_end_date = as.Date(cohort_end)
      cohort_end_date = cohort_start
    )
  }

  # into in-memory databse
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
