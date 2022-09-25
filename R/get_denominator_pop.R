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


#' Identify a single denominator population
#'
#' @param cdm_ref CDMConnector CDM reference object
#' @param start_date Date indicating the start of the study period. If NULL,
#'  the earliest observation_start_date in the observation_period table
#'  will be used.
#' @param end_date Date indicating the end of the study period. If NULL,
#'  the latest observation_end_date in the observation_period table
#'  will be used.
#' @param min_age Minimum age for the cohort
#' @param max_age Maximum age for the cohort.
#' @param days_prior_history Days of prior history required to enter
#' the study cohort.
#' @param table_name_strata table_name_strata
#' @param strata_cohort_id strata_cohort_id
#'
#' @return
#' @importFrom rlang .data
#' @export
#'
#' @examples
get_denominator_pop <- function(cdm_ref,
                                dialect,
                                start_date,
                                end_date,
                                min_age,
                                max_age ,
                                days_prior_history,
                                table_name_strata,
                                strata_cohort_id) {

  # make sure names are lowercase
  person_db <- dplyr::rename_with(cdm_ref$person, tolower)
  observation_period_db <- dplyr::rename_with(
    cdm_ref$observation_period, tolower
  )

  # stratify population on cohort
  if (!is.null(table_name_strata)) {
    strata_db <- cdm_ref[[table_name_strata]] %>%
        dplyr::filter(.data$cohort_definition_id == .env$strata_cohort_id)

    # drop anyone not in the strata cohort
    person_db <- person_db %>%
      dplyr::inner_join(strata_db %>%
                          dplyr::rename("person_id" = "subject_id") %>%
                          dplyr::select("person_id") %>%
                          dplyr::distinct(),
                by = "person_id") %>%
      dplyr::compute()
    observation_period_db <- observation_period_db %>%
      dplyr::inner_join(strata_db %>%
                          dplyr::rename("person_id" = "subject_id") %>%
                          dplyr::select("person_id") %>%
                          dplyr::distinct(),
                        by = "person_id") %>%
      dplyr::compute()

  # update observation start date to cohort start date
  # if cohort start date is after observation start date
  # update observation end date to match cohort end date
  # if cohort end date is before observation start date
   observation_period_db <- observation_period_db %>%
      dplyr::inner_join(strata_db %>%
                          dplyr::rename("person_id" = "subject_id") %>%
                          dplyr::select("person_id",
                                        "cohort_start_date",
                                        "cohort_end_date"),
                        by = "person_id")

   # to deal with potential multiple observation periods
   # make sure outcome started during joined observation period
   # if not, drop
   observation_period_db <- observation_period_db %>%
     dplyr::filter(.data$observation_period_start_date <= .data$cohort_start_date &
                     .data$observation_period_end_date >= .data$cohort_start_date)

   observation_period_db <- observation_period_db %>%
      dplyr::mutate(observation_period_start_date =
                      dplyr::if_else(.data$observation_period_start_date <=
                              .data$cohort_start_date,
                              .data$cohort_start_date,
                              .data$observation_period_start_date)) %>%
      dplyr::mutate(observation_period_end_date =
                      dplyr::if_else(.data$observation_period_end_date >=
                              .data$cohort_end_date,
                              .data$cohort_end_date,
                              .data$observation_period_end_date)) %>%
     dplyr::select(!c("cohort_start_date", "cohort_end_date")) %>%
     dplyr::compute()
  }

  ## Identifying population of interest
  # filtering on database side
  # drop anyone missing year_of_birth or gender_concept_id
  attrition <- create_attrition_tibble(person_db, NA)

  study_pop_db <- person_db %>%
    dplyr::left_join(observation_period_db,
      by = "person_id"
    ) %>%
    dplyr::filter(!is.na(.data$year_of_birth))

  attrition <- attrition <- dplyr::bind_rows(
    attrition,
    create_attrition_tibble(study_pop_db, "Missing year of birth")
  )

  study_pop_db <- study_pop_db %>%
    dplyr::mutate(sex = ifelse(.data$gender_concept_id == "8507", "Male",
      ifelse(.data$gender_concept_id == "8532", "Female", NA)
    )) %>%
    dplyr::filter(!is.na(.data$sex)) %>%
    dplyr::compute()

  attrition <- dplyr::bind_rows(
    attrition,
    create_attrition_tibble(study_pop_db, "Missing sex")
  )

  # attrition <- dplyr::bind_rows(
  #   attrition,
  #   create_attrition_tibble(study_pop_db, "Doesn't satisfy the sex criteria")
  # )


  # add date of birth
  # fill in missing day to start of month if only day missing,
  # month (January) if only month missing,
  # month (January) and day (to 1st of month) if both missing
  # ie to impute to the centre of the period
  study_pop_db <- study_pop_db %>%
    dplyr::mutate(year_of_birth1=as.character(as.integer(year_of_birth))) %>%
    dplyr::mutate(month_of_birth1=as.character(as.integer(dplyr::if_else(is.na(month_of_birth), "01" , month_of_birth)))) %>%
    dplyr::mutate(day_of_birth1=as.character(as.integer(dplyr::if_else(is.na(day_of_birth), "01" , day_of_birth)))) %>%
    dplyr::mutate(dob=as_date(paste0(year_of_birth1, "/",
                              month_of_birth1, "/",
                              day_of_birth1)
    )) %>%
    dplyr::select(!c("year_of_birth1", "month_of_birth1", "day_of_birth1"))

  # filter
  # on year for simplicity
  # add a year to either side to make sure we only drop people we don´t want
  last_year <- lubridate::year(end_date) + 1
  earliest_year <- lubridate::year(start_date) - 1

  lower_age_limit<-min(min_age)
  upper_age_limit<-max(max_age)

  sql_year_lower<-sql_add_years(dialect=dialect,
                                years_to_add=lower_age_limit,
                                variable="dob")
  sql_year_upper<-sql_add_years(dialect=dialect,
                years_to_add=upper_age_limit,
                variable="dob")

  study_pop_db <- study_pop_db %>%
    dplyr::mutate(upper_age_check = sql_year_upper) %>%
    dplyr::mutate(lower_age_check = sql_year_lower) %>%
    # drop people too old even at study start
    dplyr::filter(.data$upper_age_check >= .env$start_date) %>%
    # drop people too young even at study end
    dplyr::filter(.data$lower_age_check <= .env$end_date)

  attrition <- dplyr::bind_rows(
    attrition,
    create_attrition_tibble(study_pop_db, "Doesn't satisfy age criteria during the study period")
  )

  study_pop_db <- study_pop_db %>%
    # drop people with observation_period_star_date after study end
    dplyr::filter(.data$observation_period_start_date <= .env$end_date) %>%
    # drop people with observation_period_end_date before study start
    dplyr::filter(.data$observation_period_end_date >= .env$start_date) %>%
    dplyr::compute()

  attrition <- attrition <- dplyr::bind_rows(
    attrition,
    create_attrition_tibble(study_pop_db, "No observation time available during study period")
  )

  ## bring in to memory and finalise population
  # study_pop <- study_pop_db %>%
  #   dplyr::collect()

  if ((study_pop_db %>% dplyr::count() %>% dplyr::pull()) > 0) {
    # only if we have found people

    # for each min age, add the date at which they reach it
    for(i in 1:length(min_age)){
      working_min <- min_age[[i]]
      variable_name<-glue::glue("date_min_age_{working_min}")
      sql_year_add<-sql_add_years(dialect=dialect,
                                  years_to_add=working_min,
                                  variable="dob")
      study_pop_db<-study_pop_db %>%
        dplyr::mutate("date_min_age_{{working_min}}" :=
                        dplyr::sql(sql_year_add))
    }
    # for each max age, add the date at which they reach it
    # the day before their next birthday
    for(i in 1:length(max_age)){
      working_max <- max_age[[i]]
      working_max_plus_one <- max_age[[i]] + 1
      variable_name<-glue::glue("date_max_age_{working_max}")
      sql_year_add<-sql_add_years(dialect=dialect,
                   years_to_add=working_max_plus_one,
                   variable="dob")
      sql_minus_day<-sql_add_days(dialect=dialect,
                                 days_to_add=-1,
                                 variable=variable_name)

      study_pop_db<-study_pop_db %>%
        dplyr::mutate("date_max_age_{{working_max}}" :=
                 dplyr::sql(sql_year_add)) %>%
        dplyr::mutate("date_max_age_{{working_max}}" :=
                        as.Date(dbplyr::sql(sql_minus_day)))
    }
    # for each prior_history requirement,
    # add the date at which they reach
    # observation start date + prior_history requirement
    for(i in 1:length(days_prior_history)){
      working_days_prior_history <- days_prior_history[[i]]
      variable_name<-glue::glue("date_with_prior_history_{working_days_prior_history}")
      sql_add_day<-sql_add_days(dialect=dialect,
                                  days_to_add=working_days_prior_history,
                                  variable="observation_period_start_date")

      study_pop_db <- study_pop_db %>%
        dplyr::mutate(
          "date_with_prior_history_{{working_days_prior_history}}" :=
            dplyr::sql(sql_add_day))
    }

    # keep people only if
    # 1) they satisfy age criteria at some point in the study
    var_lower_age_limit <- glue::glue("date_min_age_{lower_age_limit}")
    var_upper_age_limit <- glue::glue("date_max_age_{upper_age_limit}")
    study_pop_db <- study_pop_db %>%
      dplyr::filter(.data[[!!rlang::sym(var_lower_age_limit)]] <= .env$end_date) %>%
      dplyr::filter(.data[[!!rlang::sym(var_upper_age_limit)]] >= .env$start_date)

    attrition <- dplyr::bind_rows(
      attrition,
      create_attrition_tibble(study_pop_db, "Doesn't satisfy age criteria during the study period")
    )

    # 2) and they satisfy priory history criteria at some point in the study
    var_lower_prior_history <- glue::glue("date_with_prior_history_{min(days_prior_history)}")
    study_pop_db <- study_pop_db %>%
      dplyr::filter(.data[[!!rlang::sym(var_lower_prior_history)]] <= .env$end_date) %>%
      dplyr::filter(.data[[!!rlang::sym(var_lower_prior_history)]] <= .data$observation_period_end_date)

    attrition <- dplyr::bind_rows(
      attrition,
      create_attrition_tibble(study_pop_db, "Prior history requirement not fullfilled during study period")
    )

    ## Get cohort start and end dates
    # Start date:
    # study start_date,
    # date_min_age,
    # date_with_prior_history
    # (whichever comes last)

    # cohort start dates
    # for every combination of min age and prior history required
    for(i in 1:length(min_age)){
    for(j in 1:length(days_prior_history)){
      working_min <- min_age[[i]]
      working_history <- days_prior_history[[j]]
      study_pop_db <- study_pop_db %>%
        dplyr::mutate("last_of_min_age_{working_min}_prior_history_{working_history}" :=
                 dplyr::if_else(!! rlang::sym(glue::glue("date_min_age_{working_min}")) <
                                !! rlang::sym(glue::glue("date_with_prior_history_{working_history}")),
                         !! rlang::sym(glue::glue("date_with_prior_history_{working_history}")),
                         !! rlang::sym(glue::glue("date_min_age_{working_min}"))
                 )) %>%
        dplyr::mutate("cohort_start_date_min_age_{working_min}_prior_history_{working_history}" :=
                 dplyr::if_else(!! rlang::sym(glue::glue("last_of_min_age_{working_min}_prior_history_{working_history}")) <.env$start_date,
                         .env$start_date,
                         !! rlang::sym(glue::glue("last_of_min_age_{working_min}_prior_history_{working_history}"))
                 ))
    }}


    # cohort end dates
    # study end date,
    # end of observation,
    # max.age
    # (whichever comes first)
    for(i in 1:length(max_age)){
    working_max <- max_age[[i]]

    study_pop_db <- study_pop_db %>%
      dplyr::mutate("first_of_max_age_{working_max}_obs_period" :=
               dplyr::if_else(!! rlang::sym(glue::glue("date_max_age_{working_max}")) <
                         .data$observation_period_end_date,
                       !! rlang::sym(glue::glue("date_max_age_{working_max}")),
                       .data$observation_period_end_date
               )) %>%
      dplyr::mutate("cohort_end_date_max_age_{working_max}" :=
               dplyr::if_else(!! rlang::sym(glue::glue("first_of_max_age_{working_max}_obs_period")) <.env$end_date,
                       !! rlang::sym(glue::glue("first_of_max_age_{working_max}_obs_period")),
                       .env$end_date
               ))
    }

    # # Exclude people who are eligible only after cohort_end_date
    # study_pop <- study_pop %>%
    #   dplyr::filter(.data$cohort_start_date <=
    #     .data$cohort_end_date)
    #
    # attrition <- dplyr::bind_rows(
    #   attrition,
    #   create_attrition_tibble(study_pop, "Prior history requirement not fullfilled during study period")
    # )

    # # variables to keep
    # study_pop <- study_pop %>%
    #   dplyr::select("person_id", "cohort_start_date", "cohort_end_date",
    #                 "sex")
    # order by id and start date
    # study_pop <-  study_pop[order(study_pop$person_id,
    #                               study_pop$cohort_start_date),]


  }
  if ((study_pop_db %>% dplyr::count() %>% dplyr::pull()) == 0) {
    message("-- No people found for denominator population")
  }


  # attrition
  attrition <- attrition %>%
    dplyr::mutate(excluded = dplyr::lag(.data$current_n) - .data$current_n)

  # combine the two age exclusions
  attrition <- attrition %>%
    dplyr::select(!"excluded") %>%
    dplyr::left_join(attrition %>%
                       dplyr::group_by(.data$reason) %>%
                       dplyr::summarise(excluded = sum(.data$excluded)),
                     by = "reason") %>%
    dplyr::mutate(seq = 1:length(.data$reason)) %>%
    dplyr::group_by(.data$reason) %>%
    dplyr::slice_tail() %>%
    dplyr::arrange(.data$seq) %>%
    dplyr::select(!"seq")

    # return list
    dpop <-list()
    if ((study_pop_db %>% dplyr::count() %>% dplyr::pull()) == 0) {
      dpop[["denominator_population"]] <- tibble::tibble()
    } else {
      dpop[["denominator_population"]] <- study_pop_db
    }
    dpop[["attrition"]] <- attrition

    return(dpop)
}

#' Creates an attrition tibble
#'
#' @param db_table Database table that contains a person_id
#' @param reason the reason for the attrition
#' @return a tibble
create_attrition_tibble <- function(db_table, reason = NULL) {
  checkmate::assertTRUE("tbl" %in% class(db_table), na.ok = FALSE)
  checkmate::assert_character(reason, null.ok = TRUE)

  tibble::tibble(
    current_n = db_table %>%
      dplyr::select("person_id") %>%
      dplyr::distinct() %>%
      dplyr::tally()  %>%
      dplyr::pull(),
    reason = reason
  )
}
