# Copyright 2023 DARWIN EU®
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

#' @importFrom rlang .data
#' @importFrom rlang ":="
getDenominatorCohorts <- function(cdm,
                                  startDate,
                                  endDate,
                                  minAge,
                                  maxAge,
                                  daysPriorObservation,
                                  targetCohortTable,
                                  targetCohortId,
                                  intermediateTable) {
  # make sure names are lowercase and keep variables required
  personDb <- dplyr::rename_with(cdm$person, tolower) %>%
    dplyr::select(
      "person_id", "gender_concept_id",
      "year_of_birth", "month_of_birth", "day_of_birth"
    )
  observationPeriodDb <- dplyr::rename_with(
    cdm$observation_period, tolower
  ) %>%
    dplyr::select(
      "person_id", "observation_period_id",
      "observation_period_start_date",
      "observation_period_end_date"
    )

  # stratify population on cohort
  if (!is.na(targetCohortTable)) {
    targetDb <- cdm[[targetCohortTable]] %>%
      dplyr::filter(.data$cohort_definition_id == .env$targetCohortId)

    # drop anyone not in the target cohort
    personDb <- personDb %>%
      dplyr::inner_join(
        targetDb %>%
          dplyr::rename("person_id" = "subject_id") %>%
          dplyr::select("person_id") %>%
          dplyr::distinct(),
        by = "person_id"
      )

      personDb <- personDb %>%
        CDMConnector::computeQuery(
          name = paste0(intermediateTable,"_working_person"),
          temporary = FALSE,
          schema = attr(cdm, "write_schema"),
          overwrite = TRUE
        )

    observationPeriodDb <- observationPeriodDb %>%
      dplyr::inner_join(
        targetDb %>%
          dplyr::rename("person_id" = "subject_id") %>%
          dplyr::select("person_id") %>%
          dplyr::distinct(),
        by = "person_id"
      )

    # update observation start date to cohort start date
    # if cohort start date is after observation start date
    # update observation end date to match cohort end date
    # if cohort end date is before observation start date
    observationPeriodDb <- observationPeriodDb %>%
      dplyr::inner_join(
        targetDb %>%
          dplyr::rename("person_id" = "subject_id") %>%
          dplyr::select(
            "person_id",
            "cohort_start_date",
            "cohort_end_date"
          ),
        by = "person_id"
      )

    # to deal with potential multiple observation periods
    # make sure outcome started during joined observation period
    # if not, drop
    observationPeriodDb <- observationPeriodDb %>%
      dplyr::filter(.data$observation_period_start_date <=
        .data$cohort_start_date &
        .data$observation_period_end_date >= .data$cohort_start_date)

    # set observation end to whatever came first of cohort end or obs end
    observationPeriodDb <- observationPeriodDb %>%
      dplyr::mutate(
        observation_period_end_date =
          dplyr::if_else(.data$observation_period_end_date >=
            .data$cohort_end_date,
          .data$cohort_end_date,
          .data$observation_period_end_date
          )
      ) %>%
      dplyr::select(!c("cohort_end_date")) %>%
      dplyr::rename("target_cohort_start_date" = "cohort_start_date")

      personDb <- personDb %>%
        CDMConnector::computeQuery(
          name = paste0(intermediateTable,"_working_obs_period"),
          temporary = FALSE,
          schema = attr(cdm, "write_schema"),
          overwrite = TRUE
        )
  }

  ## Identifying population of interest
  # filtering on database side
  # drop anyone missing year_of_birth or gender_concept_id
  studyPopDb <- personDb %>%
    dplyr::left_join(observationPeriodDb,
      by = "person_id", x_as = "x", y_as = "y"
    )

  attrition <- recordAttrition(
    table = studyPopDb,
    id = "person_id",
    reasonId = 1,
    reason = "Starting population"
  )

  studyPopDb <- studyPopDb %>%
    dplyr::filter(!is.na(.data$year_of_birth))

  attrition <- recordAttrition(
    table = studyPopDb,
    id = "person_id",
    reasonId = 2,
    reason = "Missing year of birth",
    existingAttrition = attrition
  )

  studyPopDb <- studyPopDb %>%
    dplyr::mutate(sex = dplyr::if_else(.data$gender_concept_id == "8507",
                                       "Male",
      dplyr::if_else(.data$gender_concept_id == "8532", "Female", NA)
    )) %>%
    dplyr::filter(!is.na(.data$sex))

    studyPopDb <- studyPopDb %>%
      CDMConnector::computeQuery(
        name = paste0(intermediateTable, "_i_1"),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )

  attrition <- recordAttrition(
    table = studyPopDb,
    id = "person_id",
    reasonId = 3,
    reason = "Missing sex",
    existingAttrition = attrition
  )

  # add date of birth
  # fill in missing day to start of month if only day missing,
  # month (January) if only month missing,
  # month (January) and day (to 1st of month) if both missing
  # ie to impute to the center of the period
  studyPopDb <- studyPopDb %>%
    dplyr::mutate(
      year_of_birth1 = as.character(as.integer(.data$year_of_birth)),
      month_of_birth1 = as.character(as.integer(
        dplyr::if_else(is.na(.data$month_of_birth),
          1L, .data$month_of_birth
        )
      )),
      day_of_birth1 = as.character(as.integer(
        dplyr::if_else(is.na(.data$day_of_birth),
          1L, .data$day_of_birth
        )
      ))
    ) %>%
    dplyr::mutate(dob = !!CDMConnector::asDate(paste0(
      as.character(.data$year_of_birth1), "-",
      as.character(.data$month_of_birth1), "-",
      as.character(.data$day_of_birth1)
    ))) %>%
    dplyr::select(-c("year_of_birth1", "month_of_birth1", "day_of_birth1"))

  # filter for those within the age limits (of all the age strata)
  # during the study
  lowerAgeLimit <- min(minAge)
  upperAgeLimit <- max(maxAge)

  # as character so we can insert into db
  startDateChar <- as.character(startDate)
  endDateChar <- as.character(endDate)

  studyPopDb <- studyPopDb %>%
    dplyr::mutate(
      lower_age_check = !!CDMConnector::dateadd("dob",
        {{ lowerAgeLimit }},
        interval = "year"
      ),
      upper_age_check = !!CDMConnector::dateadd("dob",
        {{ upperAgeLimit }},
        interval = "year"
      ),
      startDate = !!CDMConnector::asDate(.env$startDateChar),
      endDate = !!CDMConnector::asDate(.env$endDateChar),
    ) %>%
    dplyr::filter(
      # drop people too old even at study start
      .data$upper_age_check >= .data$startDate,
      # drop people too young even at study end
      .data$lower_age_check <= .data$endDate
    ) %>%
    dplyr::select(-c("lower_age_check", "upper_age_check"))

    studyPopDb <- studyPopDb %>%
      CDMConnector::computeQuery(
        name = paste0(intermediateTable, "_i_2"),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )

  attrition <- recordAttrition(
    table = studyPopDb,
    id = "person_id",
    reasonId = 4,
    reason = "Cannot satisfy age criteria during the study period based on year of birth",
    existingAttrition = attrition
  )

  studyPopDb <- studyPopDb %>%
    dplyr::filter(
      # drop people with observation_period_start_date after study end
      .data$observation_period_start_date <= .data$endDate &
        # drop people with observation_period_end_date before study start
        .data$observation_period_end_date >= .data$startDate
    )

    studyPopDb <- studyPopDb %>%
      CDMConnector::computeQuery(
        name = paste0(intermediateTable, "_i_3"),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )

  attrition <- recordAttrition(
    table = studyPopDb,
    id = "person_id",
    reasonId = 5,
    reason = "No observation time available during study period",
    existingAttrition = attrition
  )

  # finalise population (if we still have people)
  if ((studyPopDb %>% dplyr::count() %>% dplyr::pull()) > 0) {
    # for each min age, add the date at which they reach it
    minAgeDates <- glue::glue("CDMConnector::dateadd('dob',
                      {minAge[seq_along(minAge)]},
                      interval = 'year')") %>%
      rlang::parse_exprs() %>%
      rlang::set_names(glue::glue("date_min_age{minAge[seq_along(minAge)]}"))

    # for each max age, add the date at which they reach it
    # the day before their next birthday
    maxAgePlusOne <- maxAge + 1
    maxAgeDates <- glue::glue("CDMConnector::dateadd('dob',
                       {maxAgePlusOne},
                       interval = 'year')") %>%
      rlang::parse_exprs() %>%
      rlang::set_names(glue::glue("date_max_age{maxAge}"))

    maxAgeDatesMinusDay <- glue::glue("CDMConnector::dateadd('date_max_age{maxAge}',
                       -1, interval = 'day')") %>%
      rlang::parse_exprs() %>%
      rlang::set_names(glue::glue("date_max_age{maxAge}"))

    # for each prior_history requirement,
    # add the date at which they reach
    # observation start date + prior_history requirement
    priorHistoryDates <- glue::glue('CDMConnector::dateadd("observation_period_start_date",
                      {daysPriorObservation}, interval = "day")') %>%
      rlang::parse_exprs() %>%
      rlang::set_names(glue::glue("date_with_prior_history{daysPriorObservation}"))

    studyPopDb <- studyPopDb %>%
      dplyr::mutate(!!!minAgeDates, !!!maxAgeDates, !!!priorHistoryDates) %>%
      dplyr::collapse() %>%
      dplyr::mutate(!!!maxAgeDatesMinusDay)

      studyPopDb <- studyPopDb %>%
        CDMConnector::computeQuery(
          name = paste0(intermediateTable, "_i_4"),
          temporary = FALSE,
          schema = attr(cdm, "write_schema"),
          overwrite = TRUE
        )

    # keep people only if they
    # satisfy age criteria at some point in the study
    varLowerAgeLimit <- glue::glue("date_min_age{lowerAgeLimit}")
    varUpperAgeLimit <- glue::glue("date_max_age{upperAgeLimit}")
    studyPopDb <- studyPopDb %>%
      dplyr::filter(
        .data[[!!rlang::sym(varLowerAgeLimit)]] <=
          .data$endDate &
          .data[[!!rlang::sym(varUpperAgeLimit)]] >=
            .data$startDate
      )

    attrition <- recordAttrition(
      table = studyPopDb,
      id = "person_id",
      reasonId = 6,
      reason = "Doesn't satisfy age criteria during the study period",
      existingAttrition = attrition
    )


    varLowerPriorHistory <-
      glue::glue("date_with_prior_history{min(daysPriorObservation)}")

    if (!is.na(targetCohortTable)) {
      # update prior history date to whatever came first, that or target entry
      studyPopDb <- studyPopDb %>%
        dplyr::mutate(
          !!varLowerPriorHistory :=
            dplyr::if_else(.data$target_cohort_start_date >=
              .data[[varLowerPriorHistory]],
            .data$target_cohort_start_date,
            .data[[varLowerPriorHistory]]
            )
        )
    }

    # satisfy prior history criteria at some point in the study
    studyPopDb <- studyPopDb %>%
      dplyr::filter(
        .data[[!!rlang::sym(varLowerPriorHistory)]] <=
          .data$endDate,
        .data[[!!rlang::sym(varLowerPriorHistory)]] <=
          .data$observation_period_end_date
      )

      studyPopDb <- studyPopDb %>%
        CDMConnector::computeQuery(
          name = paste0(intermediateTable, "_i_5"),
          temporary = FALSE,
          schema = attr(cdm, "write_schema"),
          overwrite = TRUE
        )


    attrition <- recordAttrition(
      table = studyPopDb,
      id = "person_id",
      reasonId = 7,
      reason = "Prior history requirement not fulfilled during study period",
      existingAttrition = attrition
    )

    ## Get cohort start and end dates
    # Start date:
    # study start_date,
    # date_min_age,
    # date_with_prior_history
    # (whichever comes last)

    # cohort start dates
    # for every combination of min age and prior history required
    ageHistCombos <- expand.grid(
      minAge = minAge,
      daysPriorObservation = daysPriorObservation
    )

    minAgeHistDates <- glue::glue("dplyr::if_else(date_min_age{ageHistCombos$minAge} < date_with_prior_history{ageHistCombos$daysPriorObservation},
                                      date_with_prior_history{ageHistCombos$daysPriorObservation},
                                      date_min_age{ageHistCombos$minAge})") %>%
      rlang::parse_exprs() %>%
      rlang::set_names(glue::glue("last_of_min_age{ageHistCombos$minAge}prior_history{ageHistCombos$daysPriorObservation}"))


    minAgeHistStartDates <- glue::glue("dplyr::if_else(last_of_min_age{ageHistCombos$minAge}prior_history{ageHistCombos$daysPriorObservation} < .data$startDate,
                                       .data$startDate,
                                       last_of_min_age{ageHistCombos$minAge}prior_history{ageHistCombos$daysPriorObservation})") %>%
      rlang::parse_exprs() %>%
      rlang::set_names(glue::glue("date_min_age{ageHistCombos$minAge}prior_history{ageHistCombos$daysPriorObservation}"))

    studyPopDb <- studyPopDb %>%
      dplyr::mutate(!!!minAgeHistDates) %>%
      dplyr::collapse() %>%
      dplyr::mutate(!!!minAgeHistStartDates)

      studyPopDb <- studyPopDb %>%
        CDMConnector::computeQuery(
          name = paste0(intermediateTable, "_i_6"),
          temporary = FALSE,
          schema = attr(cdm, "write_schema"),
          overwrite = TRUE
        )

    # cohort end dates
    # study end date,
    # end of observation,
    # max.age
    # (whichever comes first)
    maxAgeObsPeriodDates <- glue::glue("dplyr::if_else(date_max_age{maxAge} < .data$observation_period_end_date,
                                       date_max_age{maxAge},
                                       .data$observation_period_end_date)") %>%
      rlang::parse_exprs() %>%
      rlang::set_names(glue::glue("first_of_max_age{maxAge}ObsPeriod"))

    maxAgeObsPeriodEndDates <- glue::glue("dplyr::if_else(first_of_max_age{maxAge}ObsPeriod < .data$endDate,
                                       first_of_max_age{maxAge}ObsPeriod,
                                       .data$endDate)") %>%
      rlang::parse_exprs() %>%
      rlang::set_names(glue::glue("date_max_age{maxAge}"))

    studyPopDb <- studyPopDb %>%
      dplyr::mutate(!!!maxAgeObsPeriodDates) %>%
      dplyr::collapse() %>%
      dplyr::mutate(!!!maxAgeObsPeriodEndDates)

      studyPopDb <- studyPopDb %>%
        CDMConnector::computeQuery(
          name = paste0(intermediateTable, "_cohorts"),
          temporary = FALSE,
          schema = attr(cdm, "write_schema"),
          overwrite = TRUE
        )
  }

  # table to return
  studyPopDb <- studyPopDb %>%
    CDMConnector::computeQuery(
      name = intermediateTable,
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )

  # remove intermediate tables
  CDMConnector::dropTable(
    cdm = cdm,
    name = tidyselect::starts_with(paste0(intermediateTable, "_"))
  )

  # return list with population and attrition
  dpop <- list()
  dpop[["denominator_population"]] <- studyPopDb
  dpop[["attrition"]] <- attrition

    return(dpop)
}
