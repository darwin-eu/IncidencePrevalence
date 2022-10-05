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
#' @param strata strata table.
#' @param outcome outcome table.
#' @param sample_size number of unique patient
#' @param out_pre fraction of patient with an event
#' @param seed seed for simulating the data set use same seed to get same data set
#' @param age_beta the beta for the standardized age in logistics regression outcome model
#' @param gender_beta the beta for the gender flag in logistics regression outcome model
#' @param intercept the beta for the intercept in the logistics regression outcome model
#' @param earliest_date_of_birth the earliest date of birth of patient in person table format "dd-mm-yyyy"
#' @param latest_date_of_birth the latest date of birth for patient in person table format "dd-mm-yyyy"
#' @param earliest_observation_start_date the earliest observation start date for patient format "dd-mm-yyyy"
#' @param latest_observation_start_date the latest observation start date for patient format "dd-mm-yyyy"
#' @param min_days_to_observation_end the minimum number of days of the observational integer
#' @param max_days_to_observation_end the maximum number of days of the observation period integer
#' @param min_outcome_days the minimum number of days of the outcome period default set to 1
#' @param max_outcome_days the maximum number of days of the outcome period default set to 10
#' @param max_outcomes_per_person the maximum possible number of outcomes per person can have default set to 1
#' @return CDMConnector CDM reference object to duckdb database with mock data

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
                                                  strata = NULL,
                                                  outcome = NULL,
                                                  sample_size = 1,
                                                  out_pre = 1,
                                                  seed = 444,
                                                  age_beta = NULL,
                                                  gender_beta = NULL,
                                                  intercept = NULL,
                                                  earliest_date_of_birth = NULL,
                                                  latest_date_of_birth = NULL,
                                                  earliest_observation_start_date = NULL,
                                                  latest_observation_start_date = NULL,
                                                  min_days_to_observation_end = NULL,
                                                  max_days_to_observation_end = NULL,
                                                  min_outcome_days = 1,
                                                  max_outcome_days = 10,
                                                  max_outcomes_per_person = 1) {

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_tibble(person, null.ok = TRUE)
  checkmate::assert_tibble(observation_period, null.ok = TRUE)
  checkmate::assert_tibble(outcome, null.ok = TRUE)
  checkmate::assert_int(sample_size, lower = 1)
  checkmate::assert_numeric(out_pre, lower = 0, upper = 1)
  checkmate::assert_int(seed, lower = 1)
  checkmate::assert_numeric(age_beta, null.ok = TRUE)
  checkmate::assert_numeric(gender_beta, null.ok = TRUE)
  checkmate::assert_numeric(intercept, null.ok = TRUE)
  checkmate::assertDate(as.Date(earliest_date_of_birth), null.ok = TRUE)
  checkmate::assertDate(as.Date(latest_date_of_birth), null.ok = TRUE)
  checkmate::assertDate(as.Date(earliest_observation_start_date), null.ok = TRUE)
  checkmate::assertDate(as.Date(latest_observation_start_date), null.ok = TRUE)
  checkmate::assert_int(min_days_to_observation_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(max_days_to_observation_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(min_outcome_days, lower = 1)
  checkmate::assert_int(max_outcome_days, lower = 1)
  checkmate::assert_int(max_outcomes_per_person, lower = 1)
  if (!is.null(latest_date_of_birth) &
    !is.null(earliest_date_of_birth)) {
    checkmate::assertTRUE(latest_date_of_birth >= earliest_date_of_birth)
  }
  if (!is.null(earliest_observation_start_date) &
    !is.null(latest_observation_start_date)) {
    checkmate::assertTRUE(latest_observation_start_date >= earliest_observation_start_date)
  }
  if (!is.null(min_days_to_observation_end) &
    !is.null(max_days_to_observation_end)) {
    checkmate::assertTRUE(max_days_to_observation_end >= min_days_to_observation_end)
  }
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

    # Define earliest possible date of birth for person table
    if (is.null(earliest_date_of_birth)) {
      earliest_date_of_birth <- as.Date("1920-01-01")
    }
    # Define latest possible date of birth for person table
    if (is.null(latest_date_of_birth)) {
      latest_date_of_birth <- as.Date("2000-01-01")
    }

    DOB <- sample(seq(
      as.Date(earliest_date_of_birth),
      as.Date(latest_date_of_birth),
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

    # define earliest and latest observation start date for obs table
    # if not specified by user
    if (is.null(earliest_observation_start_date)) {
      earliest_observation_start_date <- as.Date("2005-01-01")
    }
    if (is.null(latest_observation_start_date)) {
      latest_observation_start_date <- as.Date("2020-01-01")
    }
    obs_start_date <-
      sample(seq(
        as.Date(earliest_observation_start_date),
        as.Date(latest_observation_start_date),
        by = "day"
      ),
      sample_size,
      replace = TRUE
      ) # start date for the period


    # define min and max day to observation end
    if (is.null(min_days_to_observation_end)) {
      min_days_to_observation_end <- 1
    }
    if (is.null(max_days_to_observation_end)) {
      max_days_to_observation_end <- 1000
    }

    obs_end_date <-
      obs_start_date + lubridate::days(
        sample(
          min_days_to_observation_end:max_days_to_observation_end,
          sample_size,
          replace = TRUE
        )
      )
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
    if (is.null(age_beta) || is.null(gender_beta) || is.null(intercept)) {
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
          lubridate::days(sample(min_outcome_days:max_outcome_days, 1))) %>%
        dplyr::select(
          "subject_id",
          "cohort_start_date",
          "cohort_end_date"
        ) %>%
        dplyr::mutate(cohort_definition_id = c("1")) %>%
        dplyr::relocate(.data$cohort_definition_id)
    } else { # outcome table
      # calculate outcome

      person_cal <-
        person %>%
        dplyr::mutate(age = floor(as.numeric(difftime(
          Sys.Date(), DOB,
          units = "weeks" # calculating age from dob
        )) / 52.25)) %>%
        dplyr::mutate_at("age", ~ (scale(.) %>% as.vector())) %>% # standardizing age variable to have mean 0
        dplyr::mutate(male_flag = ifelse(gender_id == "8507", 1, 0)) %>% # binary variable for gender
        dplyr::mutate(
          pre = exp(
            .env$age_beta * .data$age + .env$gender_beta * .data$male_flag + .env$intercept
          ) / (
            1 + exp(
              .env$age_beta * .data$age + .env$gender_beta * .data$male_flag + .env$intercept
            )
          )
        ) %>% # outcome pre calculator for each person in the person table
        dplyr::mutate(outcome_flag = sapply(.data$pre, function(x) {
          binary_flag <- stats::rbinom(
            n = 1,
            size = 1,
            prob = min(x, 1)
          )
          return(binary_flag)
        })) # generate binary outcome with adjust_pre

      outcome_1 <-
        person_cal %>% dplyr::filter(.data$outcome_flag == 1) # subset of person with outcome
      subject_id <- outcome_1$person_id # define subject_id

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
          lubridate::days(sample(min_outcome_days:max_outcome_days, 1))) %>%
        dplyr::select(
          "subject_id",
          "cohort_start_date",
          "cohort_end_date"
        ) %>%
        dplyr::mutate(cohort_definition_id = c("1")) %>%
        dplyr::relocate(.data$cohort_definition_id)



      }

    if (max_outcomes_per_person > 1) {
      #create empty table
      outcome1 <- data.frame()
      #seed for loops
      set.seed(seed)
      seed_outcome <- sample(1:99999, 1000)
      #work out minimum outcome start date for each subject in outcome table
      min_out_start_date <-
        stats::aggregate(cohort_end_date ~ subject_id, data = outcome, max)

      for (i in 1:(max_outcomes_per_person - 1)) {
        set.seed(seed_outcome[i])
        #create cohort start date and end date for the possible extra outcomes
        min_out_start_date <-
          min_out_start_date %>%
          dplyr::mutate(cohort_start_date = .data$cohort_end_date + lubridate::days(sample(1:100, 1))) %>%
          dplyr::mutate(cohort_end_date = .data$cohort_start_date + lubridate::days(sample(min_outcome_days:max_outcome_days, 1)))

        #randomly select which subject to have extra outcome                                                                                                                                           )))
        dup_outcome <-
          sample(0:1, nrow(outcome), replace = TRUE)
        #linking the id from outcome table for extra outcome
        dup_outcome_id <-
          outcome[rep(1:nrow(outcome), dup_outcome), c(1, 2)]
        #link extra outcome to cohort start and end date
        extra_outcome <- dplyr::inner_join(dup_outcome_id,
                                           min_out_start_date,
                                           by = "subject_id")
        #create table with extra outcomes
        outcome1 <- rbind(outcome1, extra_outcome)
        #work out minimum outcome start date for each subject in new outcome table
        min_out_start_date <-
          stats::aggregate(cohort_end_date ~ subject_id, data = rbind(outcome,outcome1), max)
      }

      outcome <- rbind(outcome,outcome1)

    }
  }

  if (is.null(strata)) {
  # add strata population
  # as a random sample, keep the same start and end dates
  strata <- dplyr::sample_frac(person, 0.8) %>%
    dplyr::left_join(observation_period, by="person_id") %>%
    dplyr::rename("subject_id"="person_id") %>%
    dplyr::mutate(cohort_definition_id="1") %>%
    dplyr::rename("cohort_start_date"="observation_period_start_date") %>%
    dplyr::rename("cohort_end_date"="observation_period_end_date") %>%
    dplyr::select("subject_id", "cohort_definition_id",
                  "cohort_start_date", "cohort_end_date")
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
    DBI::dbWriteTable(db, "strata",
                      strata,
                      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "outcome",
      outcome,
      overwrite = TRUE
    )
  })

  cdm <- CDMConnector::cdm_from_con(
    db,
    cdm_tables = c("person", "observation_period"),
    cohort_tables = c("strata","outcome"))


  return(cdm)
}
