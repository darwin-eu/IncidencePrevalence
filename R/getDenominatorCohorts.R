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
  sqlQueries <- list()

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
      dplyr::slice_sample(n = sample)
    sqlQueries[["getting_cohort_end"]] <- personDb %>%
      extractQuery(description = "sample")
    personDb <- personDb %>%
      dplyr::compute()
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
      )
    sqlQueries[["person_strata"]] <- personDb %>%
      extractQuery(description = "person_strata")
    personDb <- personDb %>%
      dplyr::compute()

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
          )
      ) %>%
      dplyr::mutate(
        observation_period_end_date =
          dplyr::if_else(.data$observation_period_end_date >=
            .data$cohort_end_date,
          .data$cohort_end_date,
          .data$observation_period_end_date
          )
      ) %>%
      dplyr::select(!c("cohort_start_date", "cohort_end_date"))

    sqlQueries[["obs_strata_start_end"]] <- observationPeriodDb %>%
      extractQuery(description = "obs_strata_start_end")
    observationPeriodDb <- observationPeriodDb %>%
      dplyr::compute()
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
      by = "person_id"
    ) %>%
    dplyr::filter(!is.na(.data$year_of_birth))

  attrition <- recordAttrition(
    table = studyPopDb,
    id = "person_id",
    reason = "Missing year of birth",
    existingAttrition = attrition
  )

  studyPopDb <- studyPopDb %>%
    dplyr::mutate(sex = ifelse(.data$gender_concept_id == "8507", "Male",
      ifelse(.data$gender_concept_id == "8532", "Female", NA)
    )) %>%
    dplyr::filter(!is.na(.data$sex))
  sqlQueries[["sex_added"]] <- studyPopDb %>%
    extractQuery(description = "sex_added")
  studyPopDb <- studyPopDb %>%
    dplyr::compute()

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
      year_of_birth1 =
        as.character(as.integer(.data$year_of_birth))
    ) %>%
    dplyr::mutate(
      month_of_birth1 =
        as.character(as.integer(
          dplyr::if_else(is.na(.data$month_of_birth),
            "01", .data$month_of_birth
          )
        ))
    ) %>%
    dplyr::mutate(
      day_of_birth1 =
        as.character(as.integer(
          dplyr::if_else(is.na(.data$day_of_birth),
            "01", .data$day_of_birth
          )
        ))
    ) %>%
    dplyr::mutate(dob = as.Date(paste0(
      .data$year_of_birth1, "/",
      .data$month_of_birth1, "/",
      .data$day_of_birth1
    ))) %>%
    dplyr::select(!c("year_of_birth1", "month_of_birth1", "day_of_birth1"))

  # filter for those within the age limits (of all the age strata)
  # during the study
  lowerAgeLimit <- min(minAge)
  upperAgeLimit <- max(maxAge)

  studyPopDb <- studyPopDb %>%
    dplyr::mutate(lower_age_check = as.Date(!!CDMConnector::dateadd("dob",
                                              {{lowerAgeLimit}},
                                              interval = "year"))) %>%
    dplyr::mutate(upper_age_check = as.Date(!!CDMConnector::dateadd("dob",
                                              {{upperAgeLimit}},
                                              interval = "year"))) %>%
    # drop people too old even at study start
    dplyr::filter(.data$upper_age_check >= .env$startDate) %>%
    # drop people too young even at study end
    dplyr::filter(.data$lower_age_check <= .env$endDate) %>%
    dplyr::select(!c("lower_age_check", "upper_age_check"))

  sqlQueries[["drop_on_age_and_obs_date"]] <- studyPopDb %>%
    extractQuery(description = "drop_on_age_from_year_of_birth")
  studyPopDb <- studyPopDb %>%
    dplyr::compute()

  attrition <- recordAttrition(
    table = studyPopDb,
    id = "person_id",
    reason = "Cannot satisfy age criteria during the study period based on year of birth",
    existingAttrition = attrition
  )

  studyPopDb <- studyPopDb %>%
    # drop people with observation_period_start_date after study end
    dplyr::filter(.data$observation_period_start_date <= .env$endDate) %>%
    # drop people with observation_period_end_date before study start
    dplyr::filter(.data$observation_period_end_date >= .env$startDate)

  sqlQueries[["drop_on_age_and_obs_date"]] <- studyPopDb %>%
    extractQuery(description = "drop_on_obs_date")
  studyPopDb <- studyPopDb %>%
    dplyr::compute()

  attrition <- recordAttrition(
    table = studyPopDb,
    id = "person_id",
    reason = "No observation time available during study period",
    existingAttrition = attrition
  )

  # finalise population (if we still have people)
  if ((studyPopDb %>% dplyr::count() %>% dplyr::pull()) > 0) {
    # only if we have found people

    # for each min age, add the date at which they reach it
    for (i in seq_along(minAge)) {
      workingMin <- minAge[[i]]
      variableName <- glue::glue("date_min_age{workingMin}")
      studyPopDb <- studyPopDb %>%
        dplyr::mutate("date_min_age{{workingMin}}" :=
                        as.Date(!!CDMConnector::dateadd("dob",
                                                        {{workingMin}},
                                                        interval = "year")))

      if (i %% 10 == 0) {
        # in case many options have been chosen
        # we'll use a temp table to keep the
        # sql queries manageable
        sqlQueries[[paste0("getting_min_age_", i)]] <- studyPopDb %>%
          extractQuery(description = paste0("getting_min_age_", i))
        studyPopDb <- dplyr::compute(studyPopDb)
      }
    }
    sqlQueries[["getting_min_age"]] <- studyPopDb %>%
      extractQuery(description = "getting_min_age")
    studyPopDb <- studyPopDb %>% dplyr::compute()

    # for each max age, add the date at which they reach it
    # the day before their next birthday
    for (i in seq_along(maxAge)) {
      workingMax <- maxAge[[i]]
      workingMaxPlusOne <- maxAge[[i]] + 1
      variableName <- glue::glue("date_max_age{workingMax}")

      studyPopDb <- studyPopDb %>%
        dplyr::mutate("date_max_age{{workingMax}}" := as.Date(!!CDMConnector::dateadd(
          "dob", {{workingMaxPlusOne}}, interval = "year"))) %>%
        dplyr::mutate("date_max_age{{workingMax}}" := as.Date(!!CDMConnector::dateadd(
          variableName, -1, interval = "day")))

      if (i %% 10 == 0) {
        sqlQueries[[paste0("getting_max_age_", i)]] <- studyPopDb %>%
          extractQuery(description = paste0("getting_max_age_", i))
        studyPopDb <- dplyr::compute(studyPopDb)
      }
    }
    sqlQueries[["getting_max_age"]] <- studyPopDb %>%
      extractQuery(description = "getting_max_age")
    studyPopDb <- studyPopDb %>% dplyr::compute()

    # for each prior_history requirement,
    # add the date at which they reach
    # observation start date + prior_history requirement
    for (i in seq_along(daysPriorHistory)) {
      workingDaysPriorHistory <- daysPriorHistory[[i]]
      variableName <-
        glue::glue("date_with_prior_history{workingDaysPriorHistory}")

      studyPopDb <- studyPopDb %>%
        dplyr::mutate(
          "date_with_prior_history{{workingDaysPriorHistory}}" :=
            as.Date(!!CDMConnector::dateadd("observation_period_start_date",
                                    {{workingDaysPriorHistory}},
                                    interval = "day")))

      if (i %% 5 == 0) {
        # in case many options have been chosen
        # we'll use a temp table to keep the
        # sql queries manageable
        sqlQueries[[paste0("getting_prior_history_", i)]] <- studyPopDb %>%
          extractQuery(description = paste0("getting_prior_history_", i))
        studyPopDb <- dplyr::compute(studyPopDb)
      }
    }
    sqlQueries[["getting_prior_history"]] <- studyPopDb %>%
      extractQuery(description = "getting_prior_history")
    studyPopDb <- studyPopDb %>% dplyr::compute()

    # keep people only if they satisfy
    # satisfy age criteria at some point in the study
    varLowerAgeLimit <- glue::glue("date_min_age{lowerAgeLimit}")
    varUpperAgeLimit <- glue::glue("date_max_age{upperAgeLimit}")
    studyPopDb <- studyPopDb %>%
      dplyr::filter(.data[[!!rlang::sym(varLowerAgeLimit)]] <=
        .env$endDate) %>%
      dplyr::filter(.data[[!!rlang::sym(varUpperAgeLimit)]] >=
        .env$startDate)

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
      dplyr::filter(.data[[!!rlang::sym(varLowerPriorHistory)]] <=
        .env$endDate) %>%
      dplyr::filter(.data[[!!rlang::sym(varLowerPriorHistory)]] <=
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
    for (i in seq_along(minAge)) {
      for (j in seq_along(daysPriorHistory)) {
        workingMin <- minAge[[i]]
        workingHistory <- daysPriorHistory[[j]]
        studyPopDb <- studyPopDb %>%
        dplyr::mutate("last_of_min_age{workingMin}prior_history{workingHistory}" :=
            dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{workingMin}")) <
              !!rlang::sym(glue::glue("date_with_prior_history{workingHistory}")),
            !!rlang::sym(glue::glue("date_with_prior_history{workingHistory}")),
            !!rlang::sym(glue::glue("date_min_age{workingMin}"))
            )) %>%
          dplyr::mutate("date_min_age{workingMin}prior_history{workingHistory}" :=
            dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{workingMin}prior_history{workingHistory}")) <
                             .env$startDate,
              .env$startDate,
              !!rlang::sym(
            glue::glue("last_of_min_age{workingMin}prior_history{workingHistory}"))
            ))
        if (j %% 5 == 0) {
          sqlQueries[[paste0("getting_cohort_start_ph_", j)]] <- studyPopDb %>%
            extractQuery(description = paste0("getting_cohort_start_ph_", j))
          studyPopDb <- studyPopDb %>% dplyr::compute()
        }
      }
      if (i %% 5 == 0) {
        sqlQueries[[paste0("getting_cohort_start_age_", i)]] <- studyPopDb %>%
          extractQuery(description = paste0("getting_cohort_start_age_", i))
        studyPopDb <- studyPopDb %>% dplyr::compute()
      }
    }
    sqlQueries[["getting_cohort_start"]] <- studyPopDb %>%
      extractQuery(description = "getting_cohort_start")
    studyPopDb <- studyPopDb %>% dplyr::compute()


    # cohort end dates
    # study end date,
    # end of observation,
    # max.age
    # (whichever comes first)
    for (i in seq_along(maxAge)) {
      workingMax <- maxAge[[i]]
      studyPopDb <- studyPopDb %>%
        dplyr::mutate("first_of_max_age{workingMax}ObsPeriod" :=
          dplyr::if_else(!!rlang::sym(
            glue::glue("date_max_age{workingMax}")) <
            .data$observation_period_end_date,
          !!rlang::sym(glue::glue("date_max_age{workingMax}")),
          .data$observation_period_end_date
          )) %>%
        dplyr::mutate("date_max_age{workingMax}" :=
          dplyr::if_else(!!rlang::sym(
            glue::glue("first_of_max_age{workingMax}ObsPeriod"))
                         < .env$endDate,
            !!rlang::sym(
              glue::glue("first_of_max_age{workingMax}ObsPeriod")),
            .env$endDate
          ))
      if (i %% 5 == 0) {
        sqlQueries[[paste0("getting_cohort_end_", i)]] <- studyPopDb %>%
          extractQuery(description = paste0("getting_cohort_end_", i))
        studyPopDb <- studyPopDb %>% dplyr::compute()
      }
    }
    sqlQueries[["getting_cohort_end"]] <- studyPopDb %>%
      extractQuery(description = "getting_cohort_end")
    studyPopDb <- studyPopDb %>% dplyr::compute()
  }
  if ((studyPopDb %>% dplyr::count() %>% dplyr::pull()) == 0) {
    message("-- No people found for denominator population")
  }

  # return list
  dpop <- list()
  dpop[["denominator_population"]] <- studyPopDb
  dpop[["attrition"]] <- attrition
  dpop[["sql_queries"]] <- sqlQueries

  return(dpop)
}
