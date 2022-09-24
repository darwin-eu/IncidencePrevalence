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


#' Identify a set of denominator populations
#'
#' @param cdm_ref CDMConnector CDM reference object
#' @param study_start_date Date indicating the start of the study
#' period. If NULL,
#'  the earliest observation_start_date in the observation_period table
#'  will be used.
#' @param study_end_date Date indicating the end of the study
#' period. If NULL,
#'  the latest observation_end_date in the observation_period table
#'  will be used.
#' @param study_age_stratas List of age groups
#' @param study_sex_stratas Sex of the cohorts
#' @param study_days_prior_history Days of prior history required to enter
#' the study cohort.
#' @param table_name_strata table_name_strata
#' @param strata_cohort_id strata_cohort_id
#' @param verbose Either TRUE or FALSE.
#' If TRUE, progress will be reported.
#'
#' @return
#' @importFrom rlang .data
#' @export
#'
#' @examples
collect_denominator_pops <- function(cdm_ref,
                                     study_start_date = NULL,
                                     study_end_date = NULL,
                                     study_age_stratas = NULL,
                                     study_sex_stratas = "Both",
                                     study_days_prior_history = 0,
                                     table_name_strata = NULL,
                                     strata_cohort_id = NULL,
                                     verbose = FALSE) {
  if (verbose == TRUE) {
    start <- Sys.time()
    message("Progress: Checking inputs")
  }

  ## check for standard types of user error
  error_message <- checkmate::makeAssertCollection()
  cdm_inherits_check <- inherits(cdm_ref, "cdm_reference")
  checkmate::assertTRUE(cdm_inherits_check,
    add = error_message
  )
  if (!isTRUE(cdm_inherits_check)) {
    error_message$push(
      "- cdm_ref must be a CDMConnector CDM reference object"
    )
  }
  cdm_person_exists <- inherits(cdm_ref$person, 'tbl_dbi')
  checkmate::assertTRUE(cdm_person_exists, add = error_message)
  if (!isTRUE(cdm_person_exists)) {
    error_message$push(
      "- table `person` is not found"
    )
  }
  cdm_observation_period_exists <- inherits(cdm_ref$observation_period, 'tbl_dbi')
  checkmate::assertTRUE(cdm_observation_period_exists, add = error_message)
  if (!isTRUE(cdm_observation_period_exists)) {
    error_message$push(
      "- table `observation_period` is not found"
    )
  }
  checkmate::assert_date(study_start_date,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_date(study_end_date,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_list(study_age_stratas,
                         add = error_message,
                         null.ok = TRUE
  )
  if(!is.null(study_age_stratas)){
    for(i in 1:length(study_age_stratas)){
      checkmate::assertTRUE(length(study_age_stratas[[i]])==2)

      checkmate::assert_numeric(study_age_stratas[[i]][1],
                             add = error_message
      )
      checkmate::assert_numeric(study_age_stratas[[i]][2],
                                add = error_message
      )
      age_check<-study_age_stratas[[i]][1] <
                 study_age_stratas[[i]][2]
      checkmate::assertTRUE(age_check,
                            add = error_message
      )
      if (!isTRUE(age_check)) {
        error_message$push(
          "- upper age value must be higher than lower age value"
        )
      }
      checkmate::assertTRUE(study_age_stratas[[i]][1]>=0,
                            add = error_message
      )
      checkmate::assertTRUE(study_age_stratas[[i]][2]>=0,
                            add = error_message
      )

    }
  }
  checkmate::assert_vector(study_sex_stratas,
    add = error_message
  )
  sex_check <- all(study_sex_stratas %in% c("Male", "Female", "Both"))
  if (!isTRUE(sex_check)) {
    error_message$push(
      "- sex stratas must be from: Male, Female, and Both"
    )
  }
  checkmate::assert_numeric(study_days_prior_history,
    add = error_message
  )
  days_check <- all(study_days_prior_history >= 0)
  if (!isTRUE(days_check)) {
    error_message$push(
      "- study_days_prior_history cannot be negative"
    )
  }
  checkmate::assert_logical(verbose,
    add = error_message
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)


  # add broadest possible age group if no age strata were given
  if (is.null(study_age_stratas)) {
    study_age_stratas <- list(c(0, 150))
  }

  if (is.null(study_start_date)) {
    study_start_date <- cdm_ref$observation_period %>%
      dplyr::summarise(
        min(.data$observation_period_start_date,
            na.rm = TRUE
        )
      ) %>%
      dplyr::collect() %>%
      dplyr::pull()
  }
  if (is.null(study_end_date)) {
    study_end_date <- cdm_ref$observation_period %>%
      dplyr::summarise(
        max(.data$observation_period_end_date,
            na.rm = TRUE
        )
      ) %>%
      dplyr::collect() %>%
      dplyr::pull()
  }


  # summarise combinations of inputs in a tibble
  age_gr_df <- data.frame(do.call(rbind, study_age_stratas)) %>%
    dplyr::mutate(age_strata = paste0(.data$X1, ";", .data$X2))
  pop_specs <- tidyr::expand_grid(
    age_strata = age_gr_df$age_strata,
    sex_strata= .env$study_sex_stratas,
    study_days_prior_history = .env$study_days_prior_history,
    study_start_date = .env$study_start_date,
    study_end_date = .env$study_end_date
  ) %>%
    tidyr::separate(.data$age_strata,
      c("min_age", "max_age"),
      remove = FALSE
    ) %>%
    dplyr::mutate(min_age = as.numeric(.data$min_age)) %>%
    dplyr::mutate(max_age = as.numeric(.data$max_age))

  pop_specs <- pop_specs %>%
    dplyr::mutate(cohort_definition_id = as.character(dplyr::row_number()))

  # get the overall contributing population (without stratification)
  # we need to the output the corresponding dates when getting the denominator
  get_study_start_date<- unique(pop_specs$study_start_date)
  get_study_end_date<- unique(pop_specs$study_end_date)
  get_min_age <- unique(pop_specs$min_age)
  get_max_age <- unique(pop_specs$max_age)
  get_study_days_prior_history <- unique(pop_specs$study_days_prior_history)

  dpop <- get_denominator_pop(
    cdm_ref = cdm_ref,
    start_date = get_study_start_date,
    end_date = get_study_end_date,
    min_age = get_min_age,
    max_age = get_max_age,
    days_prior_history = get_study_days_prior_history,
    table_name_strata = table_name_strata,
    strata_cohort_id = strata_cohort_id
  )

  # build each of the cohorts of interest
  if(!is.null(dpop$denominator_population)){
  study_pops<-list()

  for(i in 1:length(pop_specs$cohort_definition_id)){
  working_dpop<-dpop$denominator_population

  if(pop_specs$sex_strata[[i]] %in% c("Male", "Female")){
    working_sex<-pop_specs$sex_strata[[i]]
    working_dpop<-working_dpop %>%
      dplyr::filter(.data$sex==.env$working_sex)
  }

  # cohort start
  working_dpop$cohort_start_date <- working_dpop[[glue::glue(
   "cohort_start_date_min_age_{pop_specs$min_age[[i]]}_prior_history_{pop_specs$study_days_prior_history[[i]]}")]]
  # cohort end
  working_dpop$cohort_end_date <- working_dpop[[glue::glue(
    "cohort_start_date_max_age_{pop_specs$max_age[[i]]}")]]

  working_dpop <- working_dpop %>%
     dplyr::select("person_id", "cohort_start_date", "cohort_end_date")

  working_dpop <- working_dpop %>%
    dplyr::filter(.data$cohort_start_date <=
      .data$cohort_end_date)

  # order by id and start date
  working_dpop <-  working_dpop[order(working_dpop$person_id,
                                      working_dpop$cohort_start_date),]

  study_pops[[i]] <-working_dpop %>%
    dplyr::mutate(cohort_definition_id = pop_specs$cohort_definition_id[[i]]) %>%
    dplyr::relocate(.data$cohort_definition_id)

  }
  study_pops<-dplyr::bind_rows(study_pops,
                                      .id = NULL
  )
  }

  #
  # # attrition summary
  # attrition <- study_populations[names(study_populations) == "attrition"]
  # # to tibble
  # attrition <- dplyr::bind_rows(attrition,
  #                               .id = NULL
  # )
  if(is.null(dpop$denominator_population)) {
    message("- No people found for any denominator population")
  }

  if (verbose == TRUE) {
    duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
    message(glue::glue(
      "Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }



  # results to return as a list
  results <- list()
  if(!is.null(dpop$denominator_population)) {
  results[["denominator_populations"]] <- study_pops
  } else {
    results[["denominator_populations"]] <- tibble::tibble()
  }
  results[["denominator_settings"]] <- pop_specs
  results[["attrition"]] <- tibble::tibble()

  return(results)
}
