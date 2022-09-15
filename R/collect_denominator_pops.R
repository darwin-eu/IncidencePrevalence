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
#' @param db CDMConnector CDM reference object
#' @param cdm_database_schema Name of the schema which contains the
#' omop cdm person and observation_period tables
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
#' @param strata_schema strata_schema
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
collect_denominator_pops <- function(db,
                                     cdm_database_schema,
                                     study_start_date = NULL,
                                     study_end_date = NULL,
                                     study_age_stratas = NULL,
                                     study_sex_stratas = "Both",
                                     study_days_prior_history = 0,
                                     strata_schema = NULL,
                                     table_name_strata = NULL,
                                     strata_cohort_id = NULL,
                                     verbose = FALSE) {
  if (verbose == TRUE) {
    start <- Sys.time()
  }

  if (verbose == TRUE) {
    message("Progress: Checking inputs")
  }

  ## check for standard types of user error
  error_message <- checkmate::makeAssertCollection()
  db_inherits_check <- inherits(db, "cdm_reference")
  checkmate::assertTRUE(db_inherits_check,
    add = error_message
  )
  if (!isTRUE(db_inherits_check)) {
    error_message$push(
      "- db must be a CDMConnector CDM reference object"
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
  # add check of age groups here
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
  checkmate::assert_logical(verbose,
    add = error_message
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)

  # add broadest possible age group if NULL
  if (is.null(study_age_stratas)) {
    study_age_stratas <- list(c(0, 150))
  }
  # combinations of inputs
  age_gr_df <- data.frame(do.call(rbind, study_age_stratas)) %>%
    dplyr::mutate(age_range = paste0(.data$X1, ";", .data$X2))
  pop_specs <- tidyr::expand_grid(
    age_range = age_gr_df$age_range,
    sex = .env$study_sex_stratas,
    study_days_prior_history = .env$study_days_prior_history,
    study_start_date = .env$study_start_date,
    study_end_date = .env$study_end_date
  ) %>%
    tidyr::separate(.data$age_range,
      c("min_age", "max_age"),
      remove = FALSE
    ) %>%
    dplyr::mutate(min_age = as.numeric(.data$min_age)) %>%
    dplyr::mutate(max_age = as.numeric(.data$max_age))
  if (is.null(study_start_date)) {
    pop_specs$study_start_date <- as.Date(NA)
  }
  if (is.null(study_end_date)) {
    pop_specs$study_end_date <- as.Date(NA)
  }
  pop_specs <- pop_specs %>%
    dplyr::mutate(cohort_definition_id = as.character(dplyr::row_number()))

  n_pops <- nrow(pop_specs)

  # to list
  pop_specs <- split(
    pop_specs,
    factor(pop_specs$cohort_definition_id,
      levels = unique(pop_specs$cohort_definition_id)
    )
  )

  # get each population
  study_populations <- lapply(pop_specs, function(x) {
    if (verbose == TRUE) {
      message(glue::glue(
        "Progress: Fetching population {x$cohort_definition_id} of {n_pops}"
      ))
      message(glue::glue(
        "Settings: {x$study_start_date}; {x$study_end_date}; {x$age_range}; {x$sex}; {x$study_days_prior_history}"
      ))
    }

    denominator_pop <- get_denominator_pop(
      db = db,
      cdm_database_schema = cdm_database_schema,
      start_date = x$study_start_date,
      end_date = x$study_end_date,
      min_age = x$min_age,
      max_age = x$max_age,
      sex = x$sex,
      days_prior_history = x$study_days_prior_history,
      strata_schema=strata_schema,
      table_name_strata=table_name_strata,
      strata_cohort_id=strata_cohort_id
    )

if(!is.null(denominator_pop$denominator_population)){
    denominator_pop$denominator_population <-
      denominator_pop$denominator_population  %>%
      dplyr::mutate(cohort_definition_id=x$cohort_definition_id) %>%
      dplyr::relocate(.data$cohort_definition_id)
}

    denominator_pop$denominator_settings <-
      denominator_pop$denominator_settings  %>%
      dplyr::mutate(cohort_definition_id=x$cohort_definition_id) %>%
      dplyr::relocate(.data$cohort_definition_id)

    denominator_pop$attrition <-
      denominator_pop$attrition  %>%
      dplyr::mutate(cohort_definition_id=x$cohort_definition_id) %>%
      dplyr::relocate(.data$cohort_definition_id)

    return(denominator_pop)
  })

  study_populations <- purrr::flatten(study_populations)

  # denominator settings
  denominator_settings<-study_populations[names(study_populations)=="denominator_settings"]
  # to tibble
  denominator_settings <- dplyr::bind_rows(denominator_settings,
                                        .id = NULL
  )

  # denominator population
  dpop<-study_populations[names(study_populations)=="denominator_population"]
  # to tibble
  dpop <- dplyr::bind_rows(dpop,
                          .id = NULL
  )

  # attrition summary
  attrition<-study_populations[names(study_populations)=="attrition"]
  # to tibble
  attrition <- dplyr::bind_rows(attrition,
                                .id = NULL
  )


  if (verbose == TRUE) {
    duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
    message(glue::glue(
      "Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }

  if (nrow(dpop) == 0) {
    message("- No people found for any denominator population")
  }

  # results to return as a list
  results<-list()
  results[["denominator_populations"]]<-dpop
  results[["denominator_settings"]]<-denominator_settings
  results[["attrition"]]<-attrition

  return(results)


}
