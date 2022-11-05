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


#' Identify the denominator populations
#'
#' @param cdm CDMConnector CDM reference object
#' @param startDate Date indicating the start of the study period.
#' @param endDate Date indicating the end of the study period.
#' @param minAge Minimum ages for the cohort
#' @param maxAge Maximum ages for the cohort
#' @param daysPriorHistory Days of prior history required to enter
#' the study cohort.
#' @param strataTable strataTable
#' @param strataCohortId strataCohortId
#' @param sample sample n
#'
#' @return
#' @importFrom rlang .data
#' @importFrom rlang ":="
#' @export
#'
#' @examples
getDenominatorPop <- function(cdm,
                                startDate,
                                endDate,
                                minAge,
                                maxAge,
                                daysPriorHistory,
                                strataTable,
                                strataCohortId,
                                sample) {
  sql_queries <- list()

  # make sure names are lowercase and keep variables required
  person_db <- dplyr::rename_with(cdm$person, tolower) %>%
    dplyr::select(
      "person_id", "gender_concept_id",
      "year_of_birth", "month_of_birth", "day_of_birth"
    )
  observation_period_db <- dplyr::rename_with(
    cdm$observation_period, tolower
  ) %>%
    dplyr::select(
      "person_id", "observation_period_id",
      "observation_period_start_date",
      "observation_period_end_date"
    )

  # sample
  if (!is.null(sample)) {
    person_db <- person_db %>%
      dplyr::slice_sample(n = sample)
    sql_queries[["getting_cohort_end"]] <- person_db %>%
      extractQuery(description = "sample")
    person_db <- person_db %>%
      dplyr::compute()
  }

  # stratify population on cohort
  if (!is.null(strataTable)) {
    strata_db <- cdm[[strataTable]] %>%
      dplyr::filter(.data$cohort_definition_id == .env$strataCohortId)

    # drop anyone not in the strata cohort
    person_db <- person_db %>%
      dplyr::inner_join(strata_db %>%
        dplyr::rename("person_id" = "subject_id") %>%
        dplyr::select("person_id") %>%
        dplyr::distinct(),
      by = "person_id"
      )
    sql_queries[["person_strata"]] <- person_db %>%
      extractQuery(description = "person_strata")
    person_db <- person_db %>%
      dplyr::compute()

    observation_period_db <- observation_period_db %>%
      dplyr::inner_join(strata_db %>%
        dplyr::rename("person_id" = "subject_id") %>%
        dplyr::select("person_id") %>%
        dplyr::distinct(),
      by = "person_id"
      )

    # update observation start date to cohort start date
    # if cohort start date is after observation start date
    # update observation end date to match cohort end date
    # if cohort end date is before observation start date
    observation_period_db <- observation_period_db %>%
      dplyr::inner_join(strata_db %>%
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
    observation_period_db <- observation_period_db %>%
      dplyr::filter(.data$observation_period_start_date <=
                      .data$cohort_start_date &
        .data$observation_period_end_date >= .data$cohort_start_date)

    observation_period_db <- observation_period_db %>%
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

    sql_queries[["obs_strata_start_end"]] <- observation_period_db %>%
      extractQuery(description = "obs_strata_start_end")
    observation_period_db <- observation_period_db %>%
      dplyr::compute()
  }

  ## Identifying population of interest
  # filtering on database side
  # drop anyone missing year_of_birth or gender_concept_id
  attrition <- record_attrition(
    table = person_db,
    id = "person_id",
    reason = "Starting population"
  )

  study_pop_db <- person_db %>%
    dplyr::left_join(observation_period_db,
      by = "person_id"
    ) %>%
    dplyr::filter(!is.na(.data$year_of_birth))

  attrition <- record_attrition(
    table = study_pop_db,
    id = "person_id",
    reason = "Missing year of birth",
    existing_attrition = attrition
  )

  study_pop_db <- study_pop_db %>%
    dplyr::mutate(sex = ifelse(.data$gender_concept_id == "8507", "Male",
      ifelse(.data$gender_concept_id == "8532", "Female", NA)
    )) %>%
    dplyr::filter(!is.na(.data$sex))
  sql_queries[["sex_added"]] <- study_pop_db %>%
    extractQuery(description = "sex_added")
  study_pop_db <- study_pop_db %>%
    dplyr::compute()

  attrition <- record_attrition(
    table = study_pop_db,
    id = "person_id",
    reason = "Missing sex",
    existing_attrition = attrition
  )

  # add date of birth
  # fill in missing day to start of month if only day missing,
  # month (January) if only month missing,
  # month (January) and day (to 1st of month) if both missing
  # ie to impute to the center of the period
  study_pop_db <- study_pop_db %>%
    dplyr::mutate(year_of_birth1 =
                    as.character(as.integer(.data$year_of_birth))) %>%
    dplyr::mutate(month_of_birth1 =
                    as.character(as.integer(
                      dplyr::if_else(is.na(.data$month_of_birth),
                                     "01", .data$month_of_birth)))) %>%
    dplyr::mutate(day_of_birth1 =
                    as.character(as.integer(
                      dplyr::if_else(is.na(.data$day_of_birth),
                                     "01", .data$day_of_birth)))) %>%
    dplyr::mutate(dob = as.Date(paste0(
      .data$year_of_birth1, "/",
      .data$month_of_birth1, "/",
      .data$day_of_birth1
    ))) %>%
    dplyr::select(!c("year_of_birth1", "month_of_birth1", "day_of_birth1"))

  # filter for those within the age limits (of all the age strata)
  # during the study
  lower_age_limit <- min(minAge)
  upper_age_limit <- max(maxAge)

  sql_year_lower <- sqlAddYears(
    dialect = CDMConnector::dbms(attr(cdm, "dbcon")),
    years_to_add = lower_age_limit,
    variable = "dob"
  )
  sql_year_upper <- sqlAddYears(
    dialect = CDMConnector::dbms(attr(cdm, "dbcon")),
    years_to_add = upper_age_limit,
    variable = "dob"
  )

  study_pop_db <- study_pop_db %>%
    dplyr::mutate(lower_age_check = dplyr::sql(sql_year_lower)) %>%
    dplyr::mutate(upper_age_check = dplyr::sql(sql_year_upper)) %>%
    # drop people too old even at study start
    dplyr::filter(.data$upper_age_check >= .env$startDate) %>%
    # drop people too young even at study end
    dplyr::filter(.data$lower_age_check <= .env$endDate) %>%
    dplyr::select(!c("lower_age_check", "upper_age_check"))

  attrition <- record_attrition(
    table = study_pop_db,
    id = "person_id",
    reason = "Cannot satisfy age criteria during the study period based on year of birth",
    existing_attrition = attrition
  )

  study_pop_db <- study_pop_db %>%
    # drop people with observation_period_start_date after study end
    dplyr::filter(.data$observation_period_start_date <= .env$endDate) %>%
    # drop people with observation_period_end_date before study start
    dplyr::filter(.data$observation_period_end_date >= .env$startDate)

  sql_queries[["drop_on_age_and_obs_date"]] <- study_pop_db %>%
    extractQuery(description = "drop_on_age_and_obs_date")
  study_pop_db <- study_pop_db %>%
    dplyr::compute()

  attrition <- record_attrition(
    table = study_pop_db,
    id = "person_id",
    reason = "No observation time available during study period",
    existing_attrition = attrition
  )

  # finalise population (if we still have people)
  if ((study_pop_db %>% dplyr::count() %>% dplyr::pull()) > 0) {
    # only if we have found people

    # for each min age, add the date at which they reach it
    for (i in seq_along(minAge)) {
      working_min <- minAge[[i]]
      variable_name <- glue::glue("date_min_age_{working_min}")
      sql_year_add <- sqlAddYears(
        dialect = CDMConnector::dbms(attr(cdm, "dbcon")),
        years_to_add = working_min,
        variable = "dob"
      )
      study_pop_db <- study_pop_db %>%
        dplyr::mutate("date_min_age_{{working_min}}" :=
          as.Date(dplyr::sql(sql_year_add)))

      if (i %% 10 == 0) {
        # in case many options have been chosen
        # we'll use a temp table to keep the
        # sql queries manageable
        sql_queries[[paste0("getting_min_age_", i)]] <- study_pop_db %>%
          extractQuery(description = paste0("getting_min_age_", i))
        study_pop_db <- dplyr::compute(study_pop_db)
      }
    }
    sql_queries[["getting_min_age"]] <- study_pop_db %>%
      extractQuery(description = "getting_min_age")
    study_pop_db <- study_pop_db %>% dplyr::compute()

    # for each max age, add the date at which they reach it
    # the day before their next birthday
    for (i in seq_along(maxAge)) {
      working_max <- maxAge[[i]]
      working_max_plus_one <- maxAge[[i]] + 1
      variable_name <- glue::glue("date_max_age_{working_max}")
      sql_year_add <- sqlAddYears(
        dialect = CDMConnector::dbms(attr(cdm, "dbcon")),
        years_to_add = working_max_plus_one,
        variable = "dob"
      )
      sql_minus_day <- sqlAddDays(
        dialect = CDMConnector::dbms(attr(cdm, "dbcon")),
        days_to_add = -1,
        variable = variable_name
      )

      study_pop_db <- study_pop_db %>%
        dplyr::mutate("date_max_age_{{working_max}}" :=
          dplyr::sql(sql_year_add)) %>%
        dplyr::mutate("date_max_age_{{working_max}}" :=
          as.Date(dbplyr::sql(sql_minus_day)))

      if (i %% 10 == 0) {
        sql_queries[[paste0("getting_max_age_", i)]] <- study_pop_db %>%
          extractQuery(description = paste0("getting_max_age_", i))
        study_pop_db <- dplyr::compute(study_pop_db)
      }
    }
    sql_queries[["getting_max_age"]] <- study_pop_db %>%
      extractQuery(description = "getting_max_age")
    study_pop_db <- study_pop_db %>% dplyr::compute()

    # for each prior_history requirement,
    # add the date at which they reach
    # observation start date + prior_history requirement
    for (i in seq_along(daysPriorHistory)) {
      working_days_prior_history <- daysPriorHistory[[i]]
      variable_name <- glue::glue("date_with_prior_history_{working_days_prior_history}")
      sql_add_day <- sqlAddDays(
        dialect = CDMConnector::dbms(attr(cdm, "dbcon")),
        days_to_add = working_days_prior_history,
        variable = "observation_period_start_date"
      )

      study_pop_db <- study_pop_db %>%
        dplyr::mutate(
          "date_with_prior_history_{{working_days_prior_history}}" :=
            as.Date(dplyr::sql(sql_add_day))
        )

      if (i %% 5 == 0) {
        # in case many options have been chosen
        # we'll use a temp table to keep the
        # sql queries manageable
        sql_queries[[paste0("getting_prior_history_", i)]] <- study_pop_db %>%
          extractQuery(description = paste0("getting_prior_history_", i))
        study_pop_db <- dplyr::compute(study_pop_db)
      }
    }
    sql_queries[["getting_prior_history"]] <- study_pop_db %>%
      extractQuery(description = "getting_prior_history")
    study_pop_db <- study_pop_db %>% dplyr::compute()

    # keep people only if they satisfy
    # satisfy age criteria at some point in the study
    var_lower_age_limit <- glue::glue("date_min_age_{lower_age_limit}")
    var_upper_age_limit <- glue::glue("date_max_age_{upper_age_limit}")
    study_pop_db <- study_pop_db %>%
      dplyr::filter(.data[[!!rlang::sym(var_lower_age_limit)]] <=
                      .env$endDate) %>%
      dplyr::filter(.data[[!!rlang::sym(var_upper_age_limit)]] >=
                      .env$startDate)

    attrition <- record_attrition(
      table = study_pop_db,
      id = "person_id",
      reason = "Doesn't satisfy age criteria during the study period",
      existing_attrition = attrition
    )

    # priory history criteria at some point in the study
    var_lower_prior_history <- glue::glue("date_with_prior_history_{min(daysPriorHistory)}")
    study_pop_db <- study_pop_db %>%
      dplyr::filter(.data[[!!rlang::sym(var_lower_prior_history)]] <=
                      .env$endDate) %>%
      dplyr::filter(.data[[!!rlang::sym(var_lower_prior_history)]] <=
                      .data$observation_period_end_date)

    attrition <- record_attrition(
      table = study_pop_db,
      id = "person_id",
      reason = "Prior history requirement not fullfilled during study period",
      existing_attrition = attrition
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
        working_min <- minAge[[i]]
        working_history <- daysPriorHistory[[j]]
        study_pop_db <- study_pop_db %>%
          dplyr::mutate("last_of_min_age_{working_min}_prior_history_{working_history}" :=
            dplyr::if_else(!!rlang::sym(glue::glue("date_min_age_{working_min}")) <
              !!rlang::sym(glue::glue("date_with_prior_history_{working_history}")),
            !!rlang::sym(glue::glue("date_with_prior_history_{working_history}")),
            !!rlang::sym(glue::glue("date_min_age_{working_min}"))
            )) %>%
          dplyr::mutate("cohort_start_date_min_age_{working_min}_prior_history_{working_history}" :=
            dplyr::if_else(!!rlang::sym(glue::glue("last_of_min_age_{working_min}_prior_history_{working_history}")) < .env$startDate,
              .env$startDate,
              !!rlang::sym(glue::glue("last_of_min_age_{working_min}_prior_history_{working_history}"))
            ))
        if (j %% 5 == 0) {
          sql_queries[[paste0("getting_cohort_start_ph_", j)]] <- study_pop_db %>%
            extractQuery(description = paste0("getting_cohort_start_ph_", j))
          study_pop_db <- study_pop_db %>% dplyr::compute()
        }
      }
      if (i %% 5 == 0) {
        sql_queries[[paste0("getting_cohort_start_age_", i)]] <- study_pop_db %>%
          extractQuery(description = paste0("getting_cohort_start_age_", i))
        study_pop_db <- study_pop_db %>% dplyr::compute()
      }
    }
    sql_queries[["getting_cohort_start"]] <- study_pop_db %>%
      extractQuery(description = "getting_cohort_start")
    study_pop_db <- study_pop_db %>% dplyr::compute()


    # cohort end dates
    # study end date,
    # end of observation,
    # max.age
    # (whichever comes first)
    for (i in seq_along(maxAge)) {
      working_max <- maxAge[[i]]

      study_pop_db <- study_pop_db %>%
        dplyr::mutate("first_of_max_age_{working_max}_obs_period" :=
          dplyr::if_else(!!rlang::sym(glue::glue("date_max_age_{working_max}")) <
            .data$observation_period_end_date,
          !!rlang::sym(glue::glue("date_max_age_{working_max}")),
          .data$observation_period_end_date
          )) %>%
        dplyr::mutate("cohort_end_date_max_age_{working_max}" :=
          dplyr::if_else(!!rlang::sym(glue::glue("first_of_max_age_{working_max}_obs_period")) < .env$endDate,
            !!rlang::sym(glue::glue("first_of_max_age_{working_max}_obs_period")),
            .env$endDate
          ))
      if (i %% 5 == 0) {
        sql_queries[[paste0("getting_cohort_end_", i)]] <- study_pop_db %>%
          extractQuery(description = paste0("getting_cohort_end_", i))
        study_pop_db <- study_pop_db %>% dplyr::compute()
      }
    }
    sql_queries[["getting_cohort_end"]] <- study_pop_db %>%
      extractQuery(description = "getting_cohort_end")
    study_pop_db <- study_pop_db %>% dplyr::compute()
  }
  if ((study_pop_db %>% dplyr::count() %>% dplyr::pull()) == 0) {
    message("-- No people found for denominator population")
  }

  # return list
  dpop <- list()
  if ((study_pop_db %>% dplyr::count() %>% dplyr::pull()) == 0) {
    dpop[["denominator_population"]] <- tibble::tibble()
  } else {
    dpop[["denominator_population"]] <- study_pop_db
  }
  dpop[["attrition"]] <- attrition
  dpop[["sql_queries"]] <- sql_queries

  return(dpop)
}
