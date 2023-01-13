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
                              daysPriorHistory,
                              strataTable,
                              strataCohortId,
                              sample) {

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

  # sample
  if (!is.null(sample)) {
    personDb <- personDb %>%
      dplyr::slice_sample(n = sample) %>%
      CDMConnector::computeQuery()
  }

  # stratify population on cohort
  if (!is.null(strataTable)) {
    strataDb <- cdm[[strataTable]] %>%
      dplyr::filter(.data$cohort_definition_id == .env$strataCohortId)

    # drop anyone not in the strata cohort
    personDb <- personDb %>%
      dplyr::inner_join(
        strataDb %>%
          dplyr::rename("person_id" = "subject_id") %>%
          dplyr::select("person_id") %>%
          dplyr::distinct(),
        by = "person_id"
      ) %>%
      CDMConnector::computeQuery()

    observationPeriodDb <- observationPeriodDb %>%
      dplyr::inner_join(
        strataDb %>%
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
        strataDb %>%
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

    observationPeriodDb <- observationPeriodDb %>%
      dplyr::mutate(
        observation_period_start_date =
          dplyr::if_else(.data$observation_period_start_date <=
            .data$cohort_start_date,
          .data$cohort_start_date,
          .data$observation_period_start_date
          ),
        observation_period_end_date =
          dplyr::if_else(.data$observation_period_end_date >=
            .data$cohort_end_date,
          .data$cohort_end_date,
          .data$observation_period_end_date
          )
      ) %>%
      dplyr::select(!c("cohort_start_date", "cohort_end_date")) %>%
      CDMConnector::computeQuery()
  }

  ## Identifying population of interest
  # filtering on database side
  # drop anyone missing year_of_birth or gender_concept_id
  attrition <- recordAttrition(
    table = personDb,
    id = "person_id",
    reason = "Starting population"
  )

  studyPopDb <- personDb %>%
    dplyr::left_join(observationPeriodDb,
      by = "person_id", x_as = "x", y_as = "y"
    ) %>%
    dplyr::filter(!is.na(.data$year_of_birth))

  attrition <- recordAttrition(
    table = studyPopDb,
    id = "person_id",
    reason = "Missing year of birth",
    existingAttrition = attrition
  )

  studyPopDb <- studyPopDb %>%
    dplyr::mutate(sex = dplyr::if_else(.data$gender_concept_id == "8507", "Male",
       dplyr::if_else(.data$gender_concept_id == "8532", "Female", NA)
    )) %>%
    dplyr::filter(!is.na(.data$sex)) %>%
    CDMConnector::computeQuery()

  attrition <- recordAttrition(
    table = studyPopDb,
    id = "person_id",
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
          )))
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

  startDate_ <- as.character(startDate)
  endDate_ <- as.character(endDate)

  studyPopDb <- studyPopDb %>%
    dplyr::mutate(lower_age_check = !!CDMConnector::dateadd("dob",
                                              {{lowerAgeLimit}},
                                              interval = "year"),
     upper_age_check = !!CDMConnector::dateadd("dob",
                                              {{upperAgeLimit}},
                                              interval = "year"),
     startDate = !!CDMConnector::asDate(.env$startDate_),
     endDate = !!CDMConnector::asDate(.env$endDate_),
     ) %>%
    dplyr::filter(
    # drop people too old even at study start
    .data$upper_age_check >= .data$startDate,
    # drop people too young even at study end
    .data$lower_age_check <= .data$endDate) %>%
    dplyr::select(-c("lower_age_check", "upper_age_check")) %>%
    CDMConnector::computeQuery()

  attrition <- recordAttrition(
    table = studyPopDb,
    id = "person_id",
    reason = "Cannot satisfy age criteria during the study period based on year of birth",
    existingAttrition = attrition
  )

  studyPopDb <- studyPopDb %>%
    dplyr::filter(
    # drop people with observation_period_start_date after study end
    .data$observation_period_start_date <= .data$endDate &
    # drop people with observation_period_end_date before study start
    .data$observation_period_end_date >= .data$startDate) %>%
    CDMConnector::computeQuery()

  attrition <- recordAttrition(
    table = studyPopDb,
    id = "person_id",
    reason = "No observation time available during study period",
    existingAttrition = attrition
  )

  # finalise population (if we still have people)
  if ((studyPopDb %>% dplyr::count() %>% dplyr::pull()) > 0) {
    # only if we have found people

    # add min ages
    # batch to more than one at a time
    minAgeBatches <- split(
      minAge,
      ceiling(seq_along(minAge) / 10)
    )
    # for each min age, add the date at which they reach it
    for (i in seq_along(minAgeBatches)) {
      j <- seq_along(minAgeBatches[[i]])
      e <- glue::glue("as.Date(CDMConnector::dateadd('dob', {minAgeBatches[[i]][j]}, interval = 'year'))") %>%
        rlang::parse_exprs() %>%
        rlang::set_names(glue::glue("date_min_age{minAgeBatches[[i]][j]}"))

      studyPopDb <- studyPopDb %>%
        dplyr::mutate(!!!e) %>%
        CDMConnector::computeQuery()
    }

    # for each max age, add the date at which they reach it
    # the day before their next birthday
    maxAgeBatches <- split(
      maxAge,
      ceiling(seq_along(maxAge) / 10)
    )
    maxAgeBatchesPlusOne <- split(
      maxAge+1,
      ceiling(seq_along(maxAge) / 10)
    )

    for (i in seq_along(maxAgeBatches)) {
      j <- seq_along(minAgeBatches[[i]])
      e1 <- glue::glue("CDMConnector::dateadd('dob', {maxAgeBatchesPlusOne[[i]][j]}, interval = 'year')") %>%
        rlang::parse_exprs() %>%
        rlang::set_names(glue::glue("date_max_age{maxAgeBatches[[i]][j]}"))

      e2 <- glue::glue("CDMConnector::dateadd('date_max_age{maxAgeBatches[[i]][j]}', -1, interval = 'day')") %>%
        rlang::parse_exprs() %>%
        rlang::set_names(glue::glue("date_max_age{maxAgeBatches[[i]][j]}"))

      studyPopDb <- studyPopDb %>%
        dplyr::mutate(!!!e1) %>%
        dplyr::mutate(!!!e2) %>%
        CDMConnector::computeQuery()
    }
  }


    # for each prior_history requirement,
    # add the date at which they reach
    # observation start date + prior_history requirement
    daysPriorHistoryBatches <- split(
      daysPriorHistory,
      ceiling(seq_along(daysPriorHistory) / 10)
    )

    for (i in seq_along(daysPriorHistoryBatches)) {
      j <- seq_along(daysPriorHistoryBatches[[i]])
      e <- glue::glue('CDMConnector::dateadd("observation_period_start_date", {daysPriorHistoryBatches[[i]][j]}, interval = "day")') %>%
        rlang::parse_exprs() %>%
        rlang::set_names(glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][j]}"))

      studyPopDb <- studyPopDb %>%
        dplyr::mutate(!!!e) %>%
        CDMConnector::computeQuery()
    }

    # keep people only if they satisfy
    # satisfy age criteria at some point in the study
    varLowerAgeLimit <- glue::glue("date_min_age{lowerAgeLimit}")
    varUpperAgeLimit <- glue::glue("date_max_age{upperAgeLimit}")
    studyPopDb <- studyPopDb %>%
      dplyr::filter(
        .data[[!!rlang::sym(varLowerAgeLimit)]] <=
          .data$endDate &
      .data[[!!rlang::sym(varUpperAgeLimit)]] >=
        .data$startDate)

    attrition <- recordAttrition(
      table = studyPopDb,
      id = "person_id",
      reason = "Doesn't satisfy age criteria during the study period",
      existingAttrition = attrition
    )

    # priory history criteria at some point in the study
    varLowerPriorHistory <-
      glue::glue("date_with_prior_history{min(daysPriorHistory)}")
    studyPopDb <- studyPopDb %>%
      dplyr::filter(
      .data[[!!rlang::sym(varLowerPriorHistory)]] <=
        .data$endDate,
      .data[[!!rlang::sym(varLowerPriorHistory)]] <=
        .data$observation_period_end_date)

    attrition <- recordAttrition(
      table = studyPopDb,
      id = "person_id",
      reason = "Prior history requirement not fullfilled during study period",
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
    ageHistCombos<- expand.grid(minAge = minAge,
                                daysPriorHistory = daysPriorHistory)
    ageHistBatchesAge <- split(
      ageHistCombos$minAge,
      ceiling(seq_along(ageHistCombos$minAge) / 10)
    )
    ageHistBatchesHist <- split(
      ageHistCombos$daysPriorHistory,
      ceiling(seq_along(ageHistCombos$daysPriorHistory) / 10)
    )

    for (i in seq_along(ageHistBatchesAge)) {
      j <- seq_along(ageHistBatchesAge[[i]])
      stopifnot(length(ageHistBatchesAge[[i]]) == length(ageHistBatchesHist[[i]]))

      e1 <- glue::glue('dplyr::if_else(date_min_age{ageHistBatchesAge[[i]][j]} < date_with_prior_history{ageHistBatchesHist[[i]][j]},
                                      date_with_prior_history{ageHistBatchesHist[[i]][j]},
                                      date_min_age{ageHistBatchesAge[[i]][j]})') %>%
        rlang::parse_exprs() %>%
        rlang::set_names(glue::glue("last_of_min_age{ageHistBatchesAge[[i]][j]}prior_history{ageHistBatchesHist[[i]][j]}"))


      e2 <- glue::glue('dplyr::if_else(last_of_min_age{ageHistBatchesAge[[i]][j]}prior_history{ageHistBatchesHist[[i]][j]} < .env$startDate,
                                       .env$startDate,
                                       last_of_min_age{ageHistBatchesAge[[i]][j]}prior_history{ageHistBatchesHist[[i]][j]})') %>%
        rlang::parse_exprs() %>%
        rlang::set_names(glue::glue("date_min_age{ageHistBatchesAge[[i]][j]}prior_history{ageHistBatchesHist[[i]][j]}"))

      studyPopDb <- studyPopDb %>%
        dplyr::mutate(!!!e1) %>%
        dplyr::mutate(!!!e2) %>%
        CDMConnector::computeQuery()
    }

    # cohort end dates
    # study end date,
    # end of observation,
    # max.age
    # (whichever comes first)
    for (i in seq_along(maxAgeBatches)) {
      j <- seq_along(maxAgeBatches[[i]])

      e1 <- glue::glue('dplyr::if_else(date_max_age{maxAgeBatches[[i]][j]} < .data$observation_period_end_date,
                                       date_max_age{maxAgeBatches[[i]][j]},
                                       .data$observation_period_end_date)') %>%
        rlang::parse_exprs() %>%
        rlang::set_names(glue::glue("first_of_max_age{maxAgeBatches[[i]][j]}ObsPeriod"))

      e2 <- glue::glue('dplyr::if_else(first_of_max_age{maxAgeBatches[[i]][j]}ObsPeriod < .data$endDate,
                                       first_of_max_age{maxAgeBatches[[i]][j]}ObsPeriod,
                                       .data$endDate)') %>%
        rlang::parse_exprs() %>%
        rlang::set_names(glue::glue("date_max_age{maxAgeBatches[[i]][j]}"))

      studyPopDb <- studyPopDb %>%
        dplyr::mutate(!!!e1) %>%
        dplyr::mutate(!!!e2)  %>%
        CDMConnector::computeQuery()
    }

  studyPopDb <- studyPopDb %>% CDMConnector::computeQuery()

  # return list
  dpop <- list()
  dpop[["denominator_population"]] <- studyPopDb
  dpop[["attrition"]] <- attrition

  return(dpop)
}
