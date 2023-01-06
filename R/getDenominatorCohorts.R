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
      by = "person_id",
      x_as = "x",
      y_as = "y"
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
      .data$year_of_birth1, "/",
      .data$month_of_birth1, "/",
      .data$day_of_birth1
    ))) %>%
    dplyr::select(!c("year_of_birth1", "month_of_birth1", "day_of_birth1"))

  # filter for those within the age limits (of all the age strata)
  # during the study
  lowerAgeLimit <- min(minAge)
  upperAgeLimit <- max(maxAge)

  startDate_ <- stringr::str_replace_all(as.character(startDate), "-", "/")
  endDate_ <- stringr::str_replace_all(as.character(endDate), "-", "/")

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
     if(length(minAgeBatches[[i]]) == 10){
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
                      ) %>% CDMConnector::computeQuery()
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
          ) %>% CDMConnector::computeQuery()
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
          ) %>% CDMConnector::computeQuery()
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
          ) %>% CDMConnector::computeQuery()
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
          ) %>% CDMConnector::computeQuery()
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
          ) %>% CDMConnector::computeQuery()
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
          ) %>% CDMConnector::computeQuery()
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
          ) %>% CDMConnector::computeQuery()
      } else if(length(minAgeBatches[[i]]) == 2){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year")),
            !!glue::glue("date_min_age{minAgeBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[2]]}},
                                              interval = "year"))
          ) %>% CDMConnector::computeQuery()
      } else {
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{minAgeBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("dob", {{minAgeBatches[[i]][[1]]}},
                                              interval = "year"))
          ) %>% CDMConnector::computeQuery()

      }
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
     if(length(maxAgeBatches[[i]]) == 10){
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
          CDMConnector::computeQuery()
      } else if(length(maxAgeBatches[[i]]) == 9){
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
          CDMConnector::computeQuery()
      } else if(length(maxAgeBatches[[i]]) == 8){
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
          CDMConnector::computeQuery()
      } else if(length(maxAgeBatches[[i]]) == 7){
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
          CDMConnector::computeQuery()
      }  else if(length(maxAgeBatches[[i]]) == 6){
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
          CDMConnector::computeQuery()
      } else if(length(maxAgeBatches[[i]]) == 5){
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
          CDMConnector::computeQuery()
      } else if(length(maxAgeBatches[[i]]) == 4){
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
          CDMConnector::computeQuery()
      } else if(length(maxAgeBatches[[i]]) == 3){
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
          CDMConnector::computeQuery()
      } else if(length(maxAgeBatches[[i]]) == 2){
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
          CDMConnector::computeQuery()
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
      if(length(daysPriorHistoryBatches[[i]]) == 10){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[1]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[2]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[3]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[4]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[5]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[6]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[7]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[8]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[9]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[10]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[10]]}},
                                              interval = "day"))
          ) %>% CDMConnector::computeQuery()
      } else if(length(daysPriorHistoryBatches[[i]]) == 9){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[1]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[2]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[3]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[4]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[5]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[6]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[7]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[8]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[9]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[9]]}},
                                              interval = "day"))
          ) %>% CDMConnector::computeQuery()
      }  else if(length(daysPriorHistoryBatches[[i]]) == 8){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[1]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[2]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[3]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[4]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[5]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[6]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[7]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[8]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[8]]}},
                                              interval = "day"))
          ) %>% CDMConnector::computeQuery()
      }  else if(length(daysPriorHistoryBatches[[i]]) == 7){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[1]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[2]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[3]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[4]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[5]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[6]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[7]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[7]]}},
                                              interval = "day"))
          ) %>% CDMConnector::computeQuery()
      }   else if(length(daysPriorHistoryBatches[[i]]) == 6){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[1]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[2]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[3]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[4]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[5]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[6]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[6]]}},
                                              interval = "day"))
          ) %>% CDMConnector::computeQuery()
      }   else if(length(daysPriorHistoryBatches[[i]]) == 5){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[1]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[2]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[3]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[4]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[5]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[5]]}},
                                              interval = "day"))
          ) %>% CDMConnector::computeQuery()
      }   else if(length(daysPriorHistoryBatches[[i]]) == 4){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[1]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[2]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[3]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[4]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[4]]}},
                                              interval = "day"))
          ) %>% CDMConnector::computeQuery()
      } else if(length(daysPriorHistoryBatches[[i]]) == 3){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[1]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[2]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[3]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[3]]}},
                                              interval = "day"))
          ) %>% CDMConnector::computeQuery()
      } else if(length(daysPriorHistoryBatches[[i]]) == 2){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[1]]}},
                                              interval = "day")),
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[2]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[2]]}},
                                              interval = "day"))
          ) %>% CDMConnector::computeQuery()
      } else {
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("date_with_prior_history{daysPriorHistoryBatches[[i]][[1]]}") :=
              as.Date(!!CDMConnector::dateadd("observation_period_start_date", {{daysPriorHistoryBatches[[i]][[1]]}},
                                              interval = "day"))
          ) %>% CDMConnector::computeQuery()

      }
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
    ageHistCombos<- expand.grid(minAge=minAge,
                                daysPriorHistory=daysPriorHistory)
    ageHistBatchesAge <- split(
      ageHistCombos$minAge,
      ceiling(seq_along(ageHistCombos$minAge) / 10)
    )
    ageHistBatchesHist <- split(
      ageHistCombos$daysPriorHistory,
      ceiling(seq_along(ageHistCombos$daysPriorHistory) / 10)
    )

    for (i in seq_along(ageHistBatchesAge)) {
      if(length(ageHistBatchesAge[[i]]) == 10){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[4]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[4]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[5]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[5]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[6]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[6]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[7]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[7]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[7]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[7]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[8]]}prior_history{ageHistBatchesHist[[i]][[8]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[8]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[8]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[8]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[8]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[9]]}prior_history{ageHistBatchesHist[[i]][[9]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[9]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[9]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[9]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[9]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[10]]}prior_history{ageHistBatchesHist[[i]][[10]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[10]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[10]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[10]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[10]]}"))
              )
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[8]]}prior_history{ageHistBatchesHist[[i]][[8]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[8]]}prior_history{ageHistBatchesHist[[i]][[8]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[8]]}prior_history{ageHistBatchesHist[[i]][[8]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[9]]}prior_history{ageHistBatchesHist[[i]][[9]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[9]]}prior_history{ageHistBatchesHist[[i]][[9]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[9]]}prior_history{ageHistBatchesHist[[i]][[9]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[10]]}prior_history{ageHistBatchesHist[[i]][[10]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[10]]}prior_history{ageHistBatchesHist[[i]][[10]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[10]]}prior_history{ageHistBatchesHist[[i]][[10]]}"))
              )
          )  %>%
          CDMConnector::computeQuery()
      } else if(length(ageHistBatchesAge[[i]]) == 9){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[4]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[4]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[5]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[5]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[6]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[6]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[7]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[7]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[7]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[7]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[8]]}prior_history{ageHistBatchesHist[[i]][[8]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[8]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[8]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[8]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[8]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[9]]}prior_history{ageHistBatchesHist[[i]][[9]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[9]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[9]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[9]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[9]]}"))
              )
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[8]]}prior_history{ageHistBatchesHist[[i]][[8]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[8]]}prior_history{ageHistBatchesHist[[i]][[8]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[8]]}prior_history{ageHistBatchesHist[[i]][[8]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[9]]}prior_history{ageHistBatchesHist[[i]][[9]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[9]]}prior_history{ageHistBatchesHist[[i]][[9]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[9]]}prior_history{ageHistBatchesHist[[i]][[9]]}"))
              )
          )  %>%
          CDMConnector::computeQuery()
      } else if(length(ageHistBatchesAge[[i]]) == 8){
        studyPopDb <- studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[4]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[4]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[5]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[5]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[6]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[6]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[7]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[7]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[7]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[7]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[8]]}prior_history{ageHistBatchesHist[[i]][[8]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[8]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[8]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[8]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[8]]}"))
              )
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[8]]}prior_history{ageHistBatchesHist[[i]][[8]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[8]]}prior_history{ageHistBatchesHist[[i]][[8]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[8]]}prior_history{ageHistBatchesHist[[i]][[8]]}"))
              )
          )  %>%
          CDMConnector::computeQuery()
      } else if(length(ageHistBatchesAge[[i]]) == 7){
        studyPopDb <-  studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[4]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[4]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[5]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[5]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[6]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[6]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[7]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[7]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[7]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[7]]}"))
              )
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[7]]}prior_history{ageHistBatchesHist[[i]][[7]]}"))
              )
          )  %>%
          CDMConnector::computeQuery()
      } else if(length(ageHistBatchesAge[[i]]) == 6){
        studyPopDb <-  studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[4]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[4]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[5]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[5]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[6]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[6]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}"))
              )
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[6]]}prior_history{ageHistBatchesHist[[i]][[6]]}"))
              )
          )  %>%
          CDMConnector::computeQuery()
      } else if(length(ageHistBatchesAge[[i]]) == 5){
        studyPopDb <-  studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[4]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[4]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[5]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[5]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}"))
              )
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[5]]}prior_history{ageHistBatchesHist[[i]][[5]]}"))
              )
          )  %>%
          CDMConnector::computeQuery()
      } else if(length(ageHistBatchesAge[[i]]) == 4){
        studyPopDb <-   studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[4]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[4]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}"))
              )
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[4]]}prior_history{ageHistBatchesHist[[i]][[4]]}"))
              )
          )  %>%
          CDMConnector::computeQuery()
      } else if(length(ageHistBatchesAge[[i]]) == 3){
        studyPopDb <-  studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[3]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}"))
              )
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[3]]}prior_history{ageHistBatchesHist[[i]][[3]]}"))
              )
          )  %>%
          CDMConnector::computeQuery()
      } else if(length(ageHistBatchesAge[[i]]) == 2){
        studyPopDb <-   studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}"))
              ),
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[2]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}"))
              )
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}"))
              ),
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}")) <
                  .env$startDate,
                .env$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[2]]}prior_history{ageHistBatchesHist[[i]][[2]]}"))
              )
          )  %>%
          CDMConnector::computeQuery()
      } else if(length(ageHistBatchesAge[[i]]) == 1){
        studyPopDb <-   studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}")) <
                               !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_with_prior_history{ageHistBatchesHist[[i]][[1]]}")),
                             !!rlang::sym(glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}"))
              )
          ) %>%
          dplyr::mutate(
            !!glue::glue("date_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(glue::glue(
                "last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}")) <
                  .data$startDate,
                .data$startDate,
                !!rlang::sym(
                  glue::glue("last_of_min_age{ageHistBatchesAge[[i]][[1]]}prior_history{ageHistBatchesHist[[i]][[1]]}"))
              )
          )  %>%
          CDMConnector::computeQuery()
      }

    }

    # cohort end dates
    # study end date,
    # end of observation,
    # max.age
    # (whichever comes first)
    for (i in seq_along(maxAgeBatches)) {
      if(length(maxAgeBatches[[i]]) == 10){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[7]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[8]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[9]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[10]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}")),
                .data$observation_period_end_date
              )) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[7]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[7]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[8]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[8]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[9]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[9]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[10]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[10]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[10]]}ObsPeriod")),
                .data$endDate)
          )  %>%
          CDMConnector::computeQuery()

      } else if(length(maxAgeBatches[[i]]) == 9){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[7]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[8]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[9]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}")),
                .data$observation_period_end_date
              )) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[7]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[7]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[8]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[8]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[9]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[9]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[9]]}ObsPeriod")),
                .data$endDate)
          )  %>%
          CDMConnector::computeQuery()

      } else if(length(maxAgeBatches[[i]]) == 8){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[7]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[8]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}")),
                .data$observation_period_end_date
              )) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[7]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[7]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[8]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[8]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[8]]}ObsPeriod")),
                .data$endDate)
          )  %>%
          CDMConnector::computeQuery()

      } else if(length(maxAgeBatches[[i]]) == 7){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[7]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}")),
                .data$observation_period_end_date
              )) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[7]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[7]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[7]]}ObsPeriod")),
                .data$endDate)
          )  %>%
          CDMConnector::computeQuery()

      }  else if(length(maxAgeBatches[[i]]) == 6){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}")),
                .data$observation_period_end_date
              )) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[6]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[6]]}ObsPeriod")),
                .data$endDate)
          )  %>%
          CDMConnector::computeQuery()

      } else if(length(maxAgeBatches[[i]]) == 5){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}")),
                .data$observation_period_end_date
              )) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[5]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[5]]}ObsPeriod")),
                .data$endDate)
          )  %>%
          CDMConnector::computeQuery()


      } else if(length(maxAgeBatches[[i]]) == 4){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}")),
                .data$observation_period_end_date
              )) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[4]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[4]]}ObsPeriod")),
                .data$endDate)
          )  %>%
          CDMConnector::computeQuery()

      } else if(length(maxAgeBatches[[i]]) == 3){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}")),
                .data$observation_period_end_date
              )) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[3]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[3]]}ObsPeriod")),
                .data$endDate)
          )  %>%
          CDMConnector::computeQuery()
      } else if(length(maxAgeBatches[[i]]) == 2){
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")),
                .data$observation_period_end_date
              ),
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}")),
                .data$observation_period_end_date
              )) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod")),
                .data$endDate),
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[2]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[2]]}ObsPeriod")),
                .data$endDate)
          )  %>%
          CDMConnector::computeQuery()
      } else {
        studyPopDb<-studyPopDb %>%
          dplyr::mutate(
            !!glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")) <
                  .data$observation_period_end_date,
                !!rlang::sym(glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}")),
                .data$observation_period_end_date
              )) %>%
          dplyr::mutate(
            !!glue::glue("date_max_age{maxAgeBatches[[i]][[1]]}") :=
              dplyr::if_else(!!rlang::sym(
                glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod"))
                < .data$endDate,
                !!rlang::sym(
                  glue::glue("first_of_max_age{maxAgeBatches[[i]][[1]]}ObsPeriod")),
                .data$endDate)
          )  %>%
          CDMConnector::computeQuery()
      }
    }

    studyPopDb <- studyPopDb %>% CDMConnector::computeQuery()
  }

  # return list
  dpop <- list()
  dpop[["denominator_population"]] <- studyPopDb
  dpop[["attrition"]] <- attrition

  return(dpop)
}
