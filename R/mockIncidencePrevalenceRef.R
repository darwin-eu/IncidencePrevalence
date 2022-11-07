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


#' Generate example database for IncidencePrevalence package
#'
#' @param personTable Person table.
#' @param observationPeriodTable observation_period table.
#' @param strataTable strata table.
#' @param outcomeTable outcome table.
#' @param sampleSize number of unique patient
#' @param outPre fraction of patient with an event
#' @param seed seed for simulating the data set use same seed to get same data set
#' @param ageBeta the beta for the standardized age in logistics regression outcome model
#' @param genderBeta the beta for the gender flag in logistics regression outcome model
#' @param intercept the beta for the intercept in the logistics regression outcome model
#' @param earliestDateOfBirth the earliest date of birth of patient in person table format "dd-mm-yyyy"
#' @param latestDateOfBirth the latest date of birth for patient in person table format "dd-mm-yyyy"
#' @param earliestObservationStartDate the earliest observation start date for patient format "dd-mm-yyyy"
#' @param latestObservationStartDate the latest observation start date for patient format "dd-mm-yyyy"
#' @param minDaysToObservationEnd the minimum number of days of the observational integer
#' @param maxDaysToObservationEnd the maximum number of days of the observation period integer
#' @param minOutcomeDays the minimum number of days of the outcome period default set to 1
#' @param maxOutcomeDays the maximum number of days of the outcome period default set to 10
#' @param maxOutcomes the maximum possible number of outcomes per person can have default set to 1
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
#' db <- mockIncidencePrevalenceRef(
#'   person = person_example,
#'   observation_period = observation_period_example
#' )
#' }
#'
mockIncidencePrevalenceRef <- function(personTable = NULL,
                                       observationPeriodTable = NULL,
                                       strataTable = NULL,
                                       outcomeTable = NULL,
                                       sampleSize = 1,
                                       outPre = 1,
                                       seed = 444,
                                       ageBeta = NULL,
                                       genderBeta = NULL,
                                       intercept = NULL,
                                       earliestDateOfBirth = NULL,
                                       latestDateOfBirth = NULL,
                                       earliestObservationStartDate = NULL,
                                       latestObservationStartDate = NULL,
                                       minDaysToObservationEnd = NULL,
                                       maxDaysToObservationEnd = NULL,
                                       minOutcomeDays = 1,
                                       maxOutcomeDays = 10,
                                       maxOutcomes = 1) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_tibble(personTable, null.ok = TRUE)
  checkmate::assert_tibble(observationPeriodTable, null.ok = TRUE)
  checkmate::assert_tibble(outcomeTable, null.ok = TRUE)
  checkmate::assert_int(sampleSize, lower = 1)
  checkmate::assert_numeric(outPre, lower = 0, upper = 1)
  checkmate::assert_int(seed, lower = 1)
  checkmate::assert_numeric(ageBeta, null.ok = TRUE)
  checkmate::assert_numeric(genderBeta, null.ok = TRUE)
  checkmate::assert_numeric(intercept, null.ok = TRUE)
  checkmate::assertDate(as.Date(earliestDateOfBirth), null.ok = TRUE)
  checkmate::assertDate(as.Date(latestDateOfBirth), null.ok = TRUE)
  checkmate::assertDate(as.Date(earliestObservationStartDate), null.ok = TRUE)
  checkmate::assertDate(as.Date(latestObservationStartDate), null.ok = TRUE)
  checkmate::assert_int(minDaysToObservationEnd, lower = 1, null.ok = TRUE)
  checkmate::assert_int(maxDaysToObservationEnd, lower = 1, null.ok = TRUE)
  checkmate::assert_int(minOutcomeDays, lower = 1)
  checkmate::assert_int(maxOutcomeDays, lower = 1)
  checkmate::assert_int(maxOutcomes, lower = 1)
  if (!is.null(latestDateOfBirth) &&
    !is.null(earliestDateOfBirth)) {
    checkmate::assertTRUE(latestDateOfBirth >= earliestDateOfBirth)
  }
  if (!is.null(earliestObservationStartDate) &&
    !is.null(latestObservationStartDate)) {
    checkmate::assertTRUE(latestObservationStartDate >=
                            earliestObservationStartDate)
  }
  if (!is.null(minDaysToObservationEnd) &&
    !is.null(maxDaysToObservationEnd)) {
    checkmate::assertTRUE(maxDaysToObservationEnd >= minDaysToObservationEnd)
  }
  checkmate::reportAssertions(collection = errorMessage)

  set.seed(seed)

  if (is.null(personTable) || is.null(observationPeriodTable)) {
    # person table
    id <- as.character(seq(1:sampleSize))
    # person gender
    genderId <- sample(c("8507", "8532"),
      sampleSize,
      replace = TRUE
    )

    # Define earliest possible date of birth for person table
    if (is.null(earliestDateOfBirth)) {
      earliestDateOfBirth <- as.Date("1920-01-01")
    }
    # Define latest possible date of birth for person table
    if (is.null(latestDateOfBirth)) {
      latestDateOfBirth <- as.Date("2000-01-01")
    }

    dateOfBirth <- sample(
      seq(
        as.Date(earliestDateOfBirth),
        as.Date(latestDateOfBirth),
        by = "day"
      ),
      sampleSize,
      replace = TRUE
    )
    # year, month, day
    dobYear <- as.numeric(format(dateOfBirth, "%Y"))
    dobMonth <- as.numeric(format(dateOfBirth, "%m"))
    dobDay <- as.numeric(format(dateOfBirth, "%d"))

    # observation_period table
    # create a list of observational_period_id

    # define earliest and latest observation start date for obs table
    # if not specified by user
    if (is.null(earliestObservationStartDate)) {
      earliestObservationStartDate <- as.Date("2005-01-01")
    }
    if (is.null(latestObservationStartDate)) {
      latestObservationStartDate <- as.Date("2020-01-01")
    }
    obsStartDate <-
      sample(
        seq(
          as.Date(earliestObservationStartDate),
          as.Date(latestObservationStartDate),
          by = "day"
        ),
        sampleSize,
        replace = TRUE
      ) # start date for the period


    # define min and max day to observation end
    if (is.null(minDaysToObservationEnd)) {
      minDaysToObservationEnd <- 1
    }
    if (is.null(maxDaysToObservationEnd)) {
      maxDaysToObservationEnd <- 1000
    }

    obsEndDate <-
      obsStartDate + lubridate::days(
        sample(
          minDaysToObservationEnd:maxDaysToObservationEnd,
          sampleSize,
          replace = TRUE
        )
      )
    if (is.null(personTable)) {
      personTable <- tibble::tibble(
        person_id = id,
        gender_concept_id = genderId,
        year_of_birth = dobYear,
        month_of_birth = dobMonth,
        day_of_birth = dobDay
      )
    }

    if (is.null(observationPeriodTable)) {
      observationPeriodTable <- tibble::tibble(
        observation_period_id = id,
        person_id = id,
        observation_period_start_date = obsStartDate,
        observation_period_end_date = obsEndDate
      )
    }
  }

  if (is.null(outcomeTable)) {
    if (is.null(ageBeta) || is.null(genderBeta) || is.null(intercept)) {
      # outcome table
      # note, only one outcome cohort
      subjectId <- sample(personTable$person_id,
        round(nrow(personTable) * outPre, digits = 0),
        replace = FALSE
      )

      outcomeTable <- observationPeriodTable %>%
        dplyr::rename("subject_id" = "person_id") %>%
        dplyr::filter(.data$subject_id %in% .env$subjectId) %>%
        dplyr::mutate(obs_days = as.numeric(difftime(
          .data$observation_period_end_date,
          .data$observation_period_start_date,
          units = "days"
        ))) %>%
        dplyr::mutate(days_to_outcome = round(stats::runif(
          length(.env$subjectId),
          min = 1,
          max = .data$obs_days
        ))) %>%
        dplyr::mutate(cohort_start_date = .data$observation_period_start_date +
          .data$days_to_outcome) %>%
        dplyr::mutate(cohort_end_date = .data$cohort_start_date +
          lubridate::days(sample(minOutcomeDays:maxOutcomeDays, 1))) %>%
        dplyr::select(
          "subject_id",
          "cohort_start_date",
          "cohort_end_date"
        ) %>%
        dplyr::mutate(cohort_definition_id = c("1")) %>%
        dplyr::relocate("cohort_definition_id")
    } else { # outcome table
      # calculate outcome

      personCal <-
        personTable %>%
        dplyr::mutate(age = floor(as.numeric(difftime(
          Sys.Date(), dateOfBirth,
          units = "weeks" # calculating age from dob
        )) / 52.25)) %>%
        # standardizing age variable to have mean 0
        dplyr::mutate_at("age", ~ (scale(.) %>%
                                     as.vector())) %>%
        # binary variable for gender
        dplyr::mutate(male_flag = ifelse(genderId == "8507", 1, 0)) %>%
        dplyr::mutate(
          pre = exp(
              .env$ageBeta * .data$age +
              .env$genderBeta * .data$male_flag +
              .env$intercept
          ) / (
            1 + exp(
              .env$ageBeta * .data$age +
              .env$genderBeta * .data$male_flag +
              .env$intercept
            )
          )
        ) %>% # outcome pre calculator for each person in the person table
        dplyr::mutate(outcome_flag = sapply(.data$pre, function(x) {
          binaryFlag <- stats::rbinom(
            n = 1,
            size = 1,
            prob = min(x, 1)
          )
          return(binaryFlag)
        })) # generate binary outcome with adjust_pre

      outcome1 <- personCal %>%
        dplyr::filter(.data$outcome_flag == 1) # subset of person with outcome
      subjectId <- outcome1$person_id # define subject_id

      outcomeTable <- observationPeriodTable %>%
        dplyr::rename("subject_id" = "person_id") %>%
        dplyr::filter(.data$subject_id %in% .env$subjectId) %>%
        dplyr::mutate(obs_days = as.numeric(difftime(
          .data$observation_period_end_date,
          .data$observation_period_start_date,
          units = "days"
        ))) %>%
        dplyr::mutate(days_to_outcome = round(stats::runif(
          length(.env$subjectId),
          min = 1,
          max = .data$obs_days
        ))) %>%
        dplyr::mutate(cohort_start_date = .data$observation_period_start_date +
          .data$days_to_outcome) %>%
        dplyr::mutate(cohort_end_date = .data$cohort_start_date +
          lubridate::days(sample(minOutcomeDays:maxOutcomeDays, 1))) %>%
        dplyr::select(
          "subject_id",
          "cohort_start_date",
          "cohort_end_date"
        ) %>%
        dplyr::mutate(cohort_definition_id = c("1")) %>%
        dplyr::relocate("cohort_definition_id")
    }

    if (maxOutcomes > 1) {
      # create empty table
      outcome1 <- data.frame()
      # seed for loops
      set.seed(seed)
      seedOutcome <- sample(1:99999, 1000)
      # work out minimum outcome start date for each subject in outcome table
      minOutStartDate <- stats::aggregate(cohort_end_date ~ subject_id,
                         data = outcomeTable, max)

      for (i in 1:(maxOutcomes)) {
        set.seed(seedOutcome[i])
        # create cohort start date and end date for the possible extra outcomes
        minOutStartDate <-
          minOutStartDate %>%
          dplyr::mutate(cohort_start_date = .data$cohort_end_date +
                          lubridate::days(sample(1:100, 1))) %>%
          dplyr::mutate(cohort_end_date = .data$cohort_start_date +
                          lubridate::days(sample(minOutcomeDays:maxOutcomeDays,
                                                 1)))

        # randomly select which subject to have extra outcome                                                                                                                                           )))
        dupOutcome <-
          sample(0:1, nrow(outcomeTable), replace = TRUE)
        # linking the id from outcome table for extra outcome
        dupOutcomeId <-
          outcomeTable[rep(seq_len(nrow(outcomeTable)), dupOutcome), c(1, 2)]
        # link extra outcome to cohort start and end date
        extraOutcome <- dplyr::inner_join(dupOutcomeId,
          minOutStartDate,
          by = "subject_id"
        )
        # create table with extra outcomes
        outcome1 <- rbind(outcome1, extraOutcome)
        # minimum outcome start date for each subject in new outcome table
        minOutStartDate <-
          stats::aggregate(cohort_end_date ~ subject_id,
                           data = rbind(outcomeTable, outcome1), max)
      }

      outcomeTable <- rbind(outcomeTable, outcome1)
    }
  }

  if (is.null(strataTable)) {
    # add strata population
    # as a random sample, keep the same start and end dates
    strataTable <- dplyr::sample_frac(personTable, 0.8) %>%
      dplyr::left_join(observationPeriodTable, by = "person_id") %>%
      dplyr::rename("subject_id" = "person_id") %>%
      dplyr::mutate(cohort_definition_id = "1") %>%
      dplyr::rename("cohort_start_date" = "observation_period_start_date") %>%
      dplyr::rename("cohort_end_date" = "observation_period_end_date") %>%
      dplyr::select(
        "subject_id", "cohort_definition_id",
        "cohort_start_date", "cohort_end_date"
      )
  }

  # into in-memory database
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person",
      personTable,
      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db,
      "observation_period",
      observationPeriodTable,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "strata",
      strataTable,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "outcome",
      outcomeTable,
      overwrite = TRUE
    )
  })

  cdm <- CDMConnector::cdm_from_con(
    db,
    cdm_tables = c("person", "observation_period"),
    cohort_tables = c("strata", "outcome")
  )


  return(cdm)
}