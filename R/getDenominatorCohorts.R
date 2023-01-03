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
      ) %>%
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
          ),
        observation_period_end_date =
          dplyr::if_else(.data$observation_period_end_date >=
            .data$cohort_end_date,
          .data$cohort_end_date,
          .data$observation_period_end_date
          )
      ) %>%
      dplyr::select(!c("cohort_start_date", "cohort_end_date")) %>%
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
    dplyr::filter(!is.na(.data$sex)) %>%
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
      year_of_birth1 = as.character(as.integer(.data$year_of_birth)),
      month_of_birth1 = as.character(as.integer(
          dplyr::if_else(is.na(.data$month_of_birth),
            "01", .data$month_of_birth
          )
        )),
      day_of_birth1 = as.character(as.integer(
          dplyr::if_else(is.na(.data$day_of_birth),
            "01", .data$day_of_birth
          )))
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
                                              interval = "year")),
     upper_age_check = as.Date(!!CDMConnector::dateadd("dob",
                                              {{upperAgeLimit}},
                                              interval = "year"))) %>%
    dplyr::filter(
    # drop people too old even at study start
    .data$upper_age_check >= .env$startDate,
    # drop people too young even at study end
    .data$lower_age_check <= .env$endDate) %>%
    dplyr::select(!c("lower_age_check", "upper_age_check")) %>%
    dplyr::compute()

  attrition <- recordAttrition(
    table = studyPopDb,
    id = "person_id",
    reason = "Cannot satisfy age criteria during the study period based on year of birth",
    existingAttrition = attrition
  )

  studyPopDb <- studyPopDb %>%
    dplyr::filter(
    # drop people with observation_period_start_date after study end
    .data$observation_period_start_date <= .env$endDate &
    # drop people with observation_period_end_date before study start
    .data$observation_period_end_date >= .env$startDate) %>%
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

    # add min ages
    # batch to more than one at a time
    minAgeBatches <- split(
      minAge,
      ceiling(seq_along(minAge) / 15)
    )
    # for each min age, add the date at which they reach it
    for (i in seq_along(minAgeBatches)) {
      if(length(minAgeBatches[[i]]) == 15){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[3]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[4]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[5]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[6]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[7]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[8]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[9]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[10]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[11]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[11]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[12]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[12]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[13]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[13]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[14]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[14]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[15]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[15]]}},
                                              interval = "year"))
          ) %>% dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 14){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[3]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[4]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[5]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[6]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[7]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[8]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[9]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[10]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[11]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[11]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[12]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[12]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[13]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[13]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[14]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[14]]}},
                                              interval = "year"))
          ) %>% dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 13){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[3]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[4]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[5]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[6]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[7]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[8]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[9]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[10]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[11]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[11]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[12]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[12]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[13]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[13]]}},
                                              interval = "year"))
          ) %>% dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 12){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[3]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[4]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[5]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[6]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[7]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[8]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[9]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[10]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[11]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[11]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[12]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[12]]}},
                                              interval = "year"))
          ) %>% dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 11){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[3]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[4]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[5]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[6]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[7]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[8]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[9]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[10]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[11]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[11]]}},
                                              interval = "year"))
          ) %>% dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 10){
      studyPopDb <- studyPopDb %>%
        dplyr::mutate(
         !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
            as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                            interval = "year")),
         !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
            as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                            interval = "year")),
         !!glue::glue("date_min_age{minAgeBatches[[i]][[3]]}") :=
           as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[3]]}},
                                           interval = "year")),
         !!glue::glue("date_min_age{minAgeBatches[[i]][[4]]}") :=
           as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[4]]}},
                                           interval = "year")),
         !!glue::glue("date_min_age{minAgeBatches[[i]][[5]]}") :=
           as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[5]]}},
                                           interval = "year")),
         !!glue::glue("date_min_age{minAgeBatches[[i]][[6]]}") :=
           as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[6]]}},
                                           interval = "year")),
         !!glue::glue("date_min_age{minAgeBatches[[i]][[7]]}") :=
           as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[7]]}},
                                           interval = "year")),
         !!glue::glue("date_min_age{minAgeBatches[[i]][[8]]}") :=
           as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[8]]}},
                                           interval = "year")),
         !!glue::glue("date_min_age{minAgeBatches[[i]][[9]]}") :=
           as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[9]]}},
                                           interval = "year")),
         !!glue::glue("date_min_age{minAgeBatches[[i]][[10]]}") :=
           as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[10]]}},
                                           interval = "year"))
                      ) %>% dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 9){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[3]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[4]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[5]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[6]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[7]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[8]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[9]]}},
                                              interval = "year"))
          ) %>% dplyr::compute()
      }  else if(length(minAgeBatches[[i]]) == 8){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[3]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[4]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[5]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[6]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[7]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[8]]}},
                                              interval = "year"))
          ) %>% dplyr::compute()
      }  else if(length(minAgeBatches[[i]]) == 7){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[3]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[4]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[5]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[6]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[7]]}},
                                              interval = "year"))
          ) %>% dplyr::compute()
      }   else if(length(minAgeBatches[[i]]) == 6){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[3]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[4]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[5]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[6]]}},
                                              interval = "year"))
          ) %>% dplyr::compute()
      }   else if(length(minAgeBatches[[i]]) == 5){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[3]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[4]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[5]]}},
                                              interval = "year"))
          ) %>% dplyr::compute()
      }   else if(length(minAgeBatches[[i]]) == 4){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[3]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[4]]}},
                                              interval = "year"))
          ) %>% dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 3){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[3]]}},
                                              interval = "year"))
          ) %>% dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 2){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                              interval = "year"))
          ) %>% dplyr::compute()
      } else {
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year"))
          ) %>% dplyr::compute()

      }
    }

    # for each max age, add the date at which they reach it
    # the day before their next birthday
    maxAgeBatches <- split(
      maxAge,
      ceiling(seq_along(maxAge) / 15)
    )
    maxAgeBatchesPlusOne <- split(
      maxAge+1,
      ceiling(seq_along(maxAge) / 15)
    )

    for (i in seq_along(maxAgeBatches)) {
      if(length(minAgeBatches[[i]]) == 15){
        # add year
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[2]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[3]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[4]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[5]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[6]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[7]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[8]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[9]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[10]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[11]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[12]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[12]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[13]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[13]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[14]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[14]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[15]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[15]]}}, interval = "year"))
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[12]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[12]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[13]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[13]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[14]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[14]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[15]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[15]]}"),
                                               -1, interval = "day"))
          ) %>%
          dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 14){
        # add year
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[2]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[3]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[4]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[5]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[6]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[7]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[8]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[9]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[10]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[11]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[12]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[12]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[13]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[13]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[14]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[14]]}}, interval = "year"))
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[12]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[12]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[13]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[13]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[14]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[14]]}"),
                                               -1, interval = "day"))
          ) %>%
          dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 13){
        studyPopDb<-studyPopDb %>%
        dplyr::mutate(
          !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
            as.Date(!!CDMConnector::dateadd(
              "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year")),
          !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
            as.Date(!!CDMConnector::dateadd(
              "dob", {{maxAgeBatchesPlusOne[[i]][[2]]}}, interval = "year")),
          !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
            as.Date(!!CDMConnector::dateadd(
              "dob", {{maxAgeBatchesPlusOne[[i]][[3]]}}, interval = "year")),
          !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
            as.Date(!!CDMConnector::dateadd(
              "dob", {{maxAgeBatchesPlusOne[[i]][[4]]}}, interval = "year")),
          !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
            as.Date(!!CDMConnector::dateadd(
              "dob", {{maxAgeBatchesPlusOne[[i]][[5]]}}, interval = "year")),
          !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
            as.Date(!!CDMConnector::dateadd(
              "dob", {{maxAgeBatchesPlusOne[[i]][[6]]}}, interval = "year")),
          !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
            as.Date(!!CDMConnector::dateadd(
              "dob", {{maxAgeBatchesPlusOne[[i]][[7]]}}, interval = "year")),
          !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
            as.Date(!!CDMConnector::dateadd(
              "dob", {{maxAgeBatchesPlusOne[[i]][[8]]}}, interval = "year")),
          !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
            as.Date(!!CDMConnector::dateadd(
              "dob", {{maxAgeBatchesPlusOne[[i]][[9]]}}, interval = "year")),
          !!glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}") :=
            as.Date(!!CDMConnector::dateadd(
              "dob", {{maxAgeBatchesPlusOne[[i]][[10]]}}, interval = "year")),
          !!glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}") :=
            as.Date(!!CDMConnector::dateadd(
              "dob", {{maxAgeBatchesPlusOne[[i]][[11]]}}, interval = "year")),
          !!glue::glue("date_max_age{maxAgeBatches[[i]][[12]]}") :=
            as.Date(!!CDMConnector::dateadd(
              "dob", {{maxAgeBatchesPlusOne[[i]][[12]]}}, interval = "year")),
          !!glue::glue("date_max_age{maxAgeBatches[[i]][[13]]}") :=
            as.Date(!!CDMConnector::dateadd(
              "dob", {{maxAgeBatchesPlusOne[[i]][[13]]}}, interval = "year"))
          ) %>%
        dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[12]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[12]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[13]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[13]]}"),
                                               -1, interval = "day"))
            ) %>%
          dplyr::compute()
      }  else if(length(minAgeBatches[[i]]) == 12){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[2]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[3]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[4]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[5]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[6]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[7]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[8]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[9]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[10]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[11]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[12]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[12]]}}, interval = "year"))
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[12]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[12]]}"),
                                               -1, interval = "day"))
          ) %>%
          dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 11){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[2]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[3]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[4]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[5]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[6]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[7]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[8]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[9]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[10]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[11]]}}, interval = "year"))
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[11]]}"),
                                               -1, interval = "day"))
          ) %>%
          dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 10){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[2]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[3]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[4]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[5]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[6]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[7]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[8]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[9]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[10]]}}, interval = "year"))
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}"),
                                               -1, interval = "day"))
          ) %>%
          dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 9){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[2]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[3]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[4]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[5]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[6]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[7]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[8]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[9]]}}, interval = "year"))
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}"),
                                               -1, interval = "day"))
          ) %>%
          dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 8){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[2]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[3]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[4]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[5]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[6]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[7]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[8]]}}, interval = "year"))
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}"),
                                               -1, interval = "day"))
          ) %>%
          dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 7){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[2]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[3]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[4]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[5]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[6]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[7]]}}, interval = "year"))
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}"),
                                               -1, interval = "day"))
          ) %>%
          dplyr::compute()
      }  else if(length(minAgeBatches[[i]]) == 6){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[2]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[3]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[4]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[5]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[6]]}}, interval = "year"))
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}"),
                                               -1, interval = "day"))
          ) %>%
          dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 5){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[2]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[3]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[4]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[5]]}}, interval = "year"))
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}"),
                                               -1, interval = "day"))
          ) %>%
          dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 4){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[2]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[3]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[4]]}}, interval = "year"))
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}"),
                                               -1, interval = "day"))
          ) %>%
          dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 3){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[2]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[3]]}}, interval = "year"))
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}"),
                                               -1, interval = "day"))
          ) %>%
          dplyr::compute()
      } else if(length(minAgeBatches[[i]]) == 2){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[2]]}}, interval = "year"))
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day")),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}"),
                                               -1, interval = "day"))
          ) %>%
          dplyr::compute()
      } else {
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd(
                "dob", {{maxAgeBatchesPlusOne[[i]][[1]]}}, interval = "year"))) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd( glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}"),
                                               -1, interval = "day"))) %>%
          dplyr::compute()
      }

    }


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
        studyPopDb <- dplyr::compute(studyPopDb)
      }
    }
    studyPopDb <- studyPopDb %>% dplyr::compute()

    # keep people only if they satisfy
    # satisfy age criteria at some point in the study
    varLowerAgeLimit <- glue::glue("date_min_age{lowerAgeLimit}")
    varUpperAgeLimit <- glue::glue("date_max_age{upperAgeLimit}")
    studyPopDb <- studyPopDb %>%
      dplyr::filter(
        .data[[!!rlang::sym(varLowerAgeLimit)]] <=
        .env$endDate &
      .data[[!!rlang::sym(varUpperAgeLimit)]] >=
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
      dplyr::filter(
      .data[[!!rlang::sym(varLowerPriorHistory)]] <=
        .env$endDate,
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
          studyPopDb <- studyPopDb %>% dplyr::compute()
        }
      }
      if (i %% 5 == 0) {
        studyPopDb <- studyPopDb %>% dplyr::compute()
      }
    }
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
        studyPopDb <- studyPopDb %>% dplyr::compute()
      }
    }
    studyPopDb <- studyPopDb %>% dplyr::compute()
  }

  # return list
  dpop <- list()
  dpop[["denominator_population"]] <- studyPopDb
  dpop[["attrition"]] <- attrition

  return(dpop)
}
