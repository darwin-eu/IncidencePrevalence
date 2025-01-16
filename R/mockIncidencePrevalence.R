# Copyright 2025 DARWIN EU®
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


#' Generate example subset of the OMOP CDM for
#' estimating incidence and prevalence
#'
#' @param personTable A tibble in the format of the person table.
#' @param observationPeriodTable A tibble in the format of the observation
#' period table.
#' @param targetCohortTable A tibble in the format of a cohort table which can
#' be used for stratification
#' @param outcomeTable A tibble in the format of a cohort table which can
#' be used for outcomes
#' @param sampleSize The number of unique patients.
#' @param outPre The fraction of patients with an event.
#' @param seed The seed for simulating the data set. Use the same
#' seed to get same data set.
#' @param earliestDateOfBirth The earliest date of birth of a patient in
#' person table.
#' @param latestDateOfBirth The latest date of birth of a patient in
#' person table.
#' @param earliestObservationStartDate The earliest observation start date
#' for patient format.
#' @param latestObservationStartDate The latest observation start date
#' for patient format.
#' @param minDaysToObservationEnd The minimum number of days of
#' the observational integer.
#' @param maxDaysToObservationEnd The maximum number of days of
#' the observation period integer.
#' @param minOutcomeDays The minimum number of days of the outcome
#' period default set to 1.
#' @param maxOutcomeDays The maximum number of days of the outcome
#' period default set to 10.
#' @param maxOutcomes The maximum possible number of outcomes per
#' person can have default set to 1.
#' @return A cdm reference to a duckdb database with mock data.

#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 100)
#' cdm
#' }
#'
mockIncidencePrevalence <- function(personTable = NULL,
                                    observationPeriodTable = NULL,
                                    targetCohortTable = NULL,
                                    outcomeTable = NULL,
                                    sampleSize = 1,
                                    outPre = 1,
                                    seed = 444,
                                    earliestDateOfBirth = NULL,
                                    latestDateOfBirth = NULL,
                                    earliestObservationStartDate = as.Date("1900-01-01"),
                                    latestObservationStartDate = as.Date("2010-01-01"),
                                    minDaysToObservationEnd = 1,
                                    maxDaysToObservationEnd = 4380,
                                    minOutcomeDays = 1,
                                    maxOutcomeDays = 10,
                                    maxOutcomes = 1) {
  rlang::check_installed("duckdb")
  rlang::check_installed("DBI")

  if (!is.null(personTable)) {
    omopgenerics::assertTrue(is.data.frame(personTable))
  }
  if (!is.null(observationPeriodTable)) {
    omopgenerics::assertTrue(is.data.frame(observationPeriodTable))
  }
  if (!is.null(outcomeTable)) {
    omopgenerics::assertTrue(is.data.frame(outcomeTable))
  }

  omopgenerics::assertNumeric(sampleSize, integerish = TRUE, min = 1)
  omopgenerics::assertNumeric(seed, integerish = TRUE, min = 1)
  omopgenerics::assertDate(as.Date(earliestDateOfBirth), null = TRUE)
  omopgenerics::assertDate(as.Date(latestDateOfBirth), null = TRUE)
  omopgenerics::assertDate(as.Date(earliestObservationStartDate), null = TRUE)
  omopgenerics::assertDate(as.Date(latestObservationStartDate), null = TRUE)
  omopgenerics::assertNumeric(minDaysToObservationEnd,
    integerish = TRUE,
    min = 1, null = TRUE
  )
  omopgenerics::assertNumeric(maxDaysToObservationEnd,
    integerish = TRUE,
    min = 1, null = TRUE
  )
  omopgenerics::assertNumeric(minOutcomeDays, integerish = TRUE, min = 1)
  omopgenerics::assertNumeric(maxOutcomeDays, integerish = TRUE, min = 1)
  omopgenerics::assertNumeric(maxOutcomes, integerish = TRUE, min = 1)

  if (!is.null(latestDateOfBirth) &&
    !is.null(earliestDateOfBirth)) {
    omopgenerics::assertTrue(latestDateOfBirth >= earliestDateOfBirth)
  }
  if (!is.null(earliestObservationStartDate) &&
    !is.null(latestObservationStartDate)) {
    omopgenerics::assertTrue(latestObservationStartDate >=
      earliestObservationStartDate)
  }
  if (!is.null(minDaysToObservationEnd) &&
    !is.null(maxDaysToObservationEnd)) {
    omopgenerics::assertTrue(maxDaysToObservationEnd >= minDaysToObservationEnd)
  }

  set.seed(seed)

  if (is.null(personTable) || is.null(observationPeriodTable)) {
    # person table mock ids
    id <- as.integer(seq(1:sampleSize))

    # person table mock values
    values <- seq(1:sampleSize)

    # person gender
    genderId <- as.integer(sample(c(8507, 8532),
      sampleSize,
      replace = TRUE
    ))

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
    dobYear <- as.integer(format(dateOfBirth, "%Y"))
    dobMonth <- as.integer(format(dateOfBirth, "%m"))
    dobDay <- as.integer(format(dateOfBirth, "%d"))

    # observation_period table
    # create a list of observational_period_id

    # define earliest and latest observation start date for obs table
    # if not specified by user
    if (is.null(earliestObservationStartDate)) {
      earliestObservationStartDate <- as.Date("1950-01-01")
    }
    if (is.null(latestObservationStartDate)) {
      latestObservationStartDate <- as.Date("1990-01-01")
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
      minDaysToObservationEnd <- 3652
    }
    if (is.null(maxDaysToObservationEnd)) {
      maxDaysToObservationEnd <- 36525
    }

    if (minDaysToObservationEnd == maxDaysToObservationEnd) {
      obsEndDate <- obsStartDate %>% clock::add_days(minDaysToObservationEnd)
    } else {
      obsEndDate <-
        obsStartDate %>% clock::add_days(
          sample(
            minDaysToObservationEnd:maxDaysToObservationEnd,
            sampleSize,
            replace = TRUE
          )
        )
    }

    if (is.null(personTable)) {
      personTable <- dplyr::tibble(
        person_id = id,
        gender_concept_id = genderId,
        year_of_birth = dobYear,
        month_of_birth = dobMonth,
        day_of_birth = dobDay,
        birth_datetime = dateOfBirth,
        race_concept_id = id,
        ethnicity_concept_id = id,
        location_id = id,
        provider_id = id,
        care_site_id = id,
        person_source_value = as.character(values),
        gender_source_value = as.character(values),
        gender_source_concept_id = id,
        race_source_value = as.character(values),
        race_source_concept_id = id,
        ethnicity_source_value = as.character(values),
        ethnicity_source_concept_id = id
      )
    }

    if (is.null(observationPeriodTable)) {
      observationPeriodTable <- dplyr::tibble(
        observation_period_id = id,
        person_id = id,
        observation_period_start_date = obsStartDate,
        observation_period_end_date = obsEndDate,
        period_type_concept_id = NA
      )
    }
  }

  if (is.null(outcomeTable)) {
    # outcome table
    # note, only one outcome cohort
    subjectId <- as.integer(sample(personTable$person_id,
      round(nrow(personTable) * outPre, digits = 0),
      replace = FALSE
    ))

    outcomeTable <- observationPeriodTable %>%
      dplyr::rename("subject_id" = "person_id") %>%
      dplyr::filter(.data$subject_id %in% .env$subjectId) %>%
      dplyr::mutate(obs_days = as.integer(difftime(
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
      dplyr::mutate(cohort_end_date = .data$cohort_start_date %>%
        clock::add_days(sample(minOutcomeDays:maxOutcomeDays, 1))) %>%
      dplyr::mutate(cohort_end_date = dplyr::if_else(.data$cohort_end_date > .data$observation_period_end_date,
        .data$observation_period_end_date, .data$cohort_end_date
      )) %>%
      dplyr::select(
        "subject_id",
        "cohort_start_date",
        "cohort_end_date"
      ) %>%
      dplyr::mutate(cohort_definition_id = c(1L)) %>%
      dplyr::relocate("cohort_definition_id")


    if (maxOutcomes > 1) {
      # create empty table
      outcome1 <- data.frame()
      # seed for loops
      set.seed(seed)
      seedOutcome <- sample.int(99999, 1000)
      # work out minimum outcome start date for each subject in outcome table
      minOutStartDate <- stats::aggregate(cohort_end_date ~ subject_id,
        data = outcomeTable, max
      )

      for (i in 1:(maxOutcomes)) {
        set.seed(seedOutcome[i])
        # create cohort start date and end date for the possible extra outcomes
        minOutStartDate <-
          minOutStartDate %>%
          dplyr::mutate(cohort_start_date = .data$cohort_end_date %>%
            clock::add_days(sample.int(100, 1))) %>%
          dplyr::mutate(cohort_end_date = .data$cohort_start_date %>%
            clock::add_days(sample(
              minOutcomeDays:maxOutcomeDays,
              1
            )))

        # randomly select which subject to have extra outcome                                                                                                                        )))
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
            data = rbind(outcomeTable, outcome1), max
          )
      }

      outcomeTable <- rbind(outcomeTable, outcome1)
    }
  }

  if (is.null(targetCohortTable)) {
    # add targe population
    # as a random sample, keep the same start and end dates
    targetCohortTable <- dplyr::sample_frac(personTable, 0.8) %>%
      dplyr::left_join(observationPeriodTable, by = "person_id") %>%
      dplyr::rename("subject_id" = "person_id") %>%
      dplyr::mutate(cohort_definition_id = 1L) %>%
      dplyr::rename("cohort_start_date" = "observation_period_start_date") %>%
      dplyr::rename("cohort_end_date" = "observation_period_end_date") %>%
      dplyr::select(
        "cohort_definition_id", "subject_id",
        "cohort_start_date", "cohort_end_date"
      )
  }

  # into in-memory database
  # add other tables required for snapshot
  cdmSource <- dplyr::tibble(
    cdm_source_name = "test_database",
    cdm_source_abbreviation = NA,
    cdm_holder = NA,
    source_description = NA,
    source_documentation_reference = NA,
    cdm_etl_reference = NA,
    source_release_date = NA,
    cdm_release_date = NA,
    cdm_version = NA,
    vocabulary_version = NA
  )

  vocabulary <- dplyr::tibble(
    vocabulary_id = "None",
    vocabulary_name = NA,
    vocabulary_reference = NA,
    vocabulary_version = "test",
    vocabulary_concept_id = NA
  )


  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbWriteTable(db, "person", personTable %>%
    dplyr::mutate(
      race_concept_id = NA_integer_,
      ethnicity_concept_id = NA_integer_
    ), overwrite = TRUE)
  DBI::dbWriteTable(db, "observation_period", observationPeriodTable %>%
    dplyr::mutate(period_type_concept_id = NA_integer_),
  overwrite = TRUE
  )
  DBI::dbWriteTable(db, "target", targetCohortTable %>%
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id)),
  overwrite = TRUE
  )
  DBI::dbWriteTable(db, "outcome", outcomeTable %>%
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id)),
  overwrite = TRUE
  )

  cdm <- CDMConnector::cdmFromCon(db, "main", "main",
    cohortTables = c("target", "outcome"),
    cdmName = "mock", .softValidation = TRUE
  )

  return(cdm)
}
