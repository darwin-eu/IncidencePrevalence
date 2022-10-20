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

#' Get population prevalence estimates
#'
#' @param cdm CDMConnector CDM reference object
#' @param table_name_denominator table_name_denominator
#' @param cohort_id_denominator_pop cohort_id_denominator_pop
#' @param table_name_outcome table_name_outcome
#' @param cohort_id_outcome cohort_id_outcome
#' @param type type
#' @param time_interval time_interval
#' @param full_periods_required full period requirement
#' @param point point where to compute prevalence inside interval
#' @param minimum_representative_proportion minimum_representative_proportion
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
get_pop_prevalence <- function(cdm,
                               table_name_denominator,
                               cohort_id_denominator_pop = NULL,
                               table_name_outcome,
                               cohort_id_outcome = NULL,
                               type = "point",
                               time_interval = "months",
                               full_periods_required = TRUE,
                               point = "start",
                               minimum_representative_proportion = 0.5,
                               verbose = FALSE) {

  ## Analysis code
  # bring in study population
  study_pop_db <- cdm[[table_name_denominator]]
  if (!is.null(cohort_id_denominator_pop)) {
    study_pop_db <- study_pop_db %>%
      dplyr::filter(.data$cohort_definition_id ==
                      .env$cohort_id_denominator_pop)
  }

  outcome_db <- cdm[[table_name_outcome]]
  if (!is.null(cohort_id_outcome)) {
    outcome_db <- outcome_db %>%
      dplyr::filter(.data$cohort_definition_id == .env$cohort_id_outcome) %>%
      dplyr::compute()
  }

  # keep outcomes of people in the denominator
  outcome_db <- outcome_db %>%
    dplyr::inner_join(study_pop_db %>%
                        dplyr::select("subject_id"),
                      by="subject_id")

  # bring outcomes into memory
  if (verbose == TRUE) {
    message("Bringing outcomes into memory")
  }

  study_pop<- study_pop_db %>% dplyr::collect()

  outcome <- outcome_db %>%
    dplyr::rename("subject_id" = "subject_id") %>%
    dplyr::rename("outcome_start_date" = "cohort_start_date") %>%
    dplyr::rename("outcome_end_date" = "cohort_end_date") %>%
    dplyr::select("subject_id", "outcome_start_date", "outcome_end_date") %>%
    dplyr::collect()

  if (time_interval == "days"){
    type <- "point"
  }

  # start date
  start_date <- min(study_pop$cohort_start_date)
  # end date
  end_date <- max(study_pop$cohort_end_date)
  # compute study_days as a function of inputs
  study_days <- computeStudyDays(
    startDate = start_date,
    endDate = end_date,
    timeInterval = time_interval,
    fullPeriodsRequired = full_periods_required,
    type = type,
    point = point
  )

  if (nrow(study_days) == 0){
    stop("Not enough following to compute the desired prevalence.")
  }

  # fetch prevalence
  # looping through each time interval
  pr <- list()
  for (i in seq_along(1:nrow(study_days))) {

    working_t_start <- study_days$start_time[i]
    if (type == "period"){
      working_t_end <- study_days$end_time[i]
      working_period <- as.numeric(working_t_end - working_t_start) + 1
    } else {
      working_t_end <- working_t_start
      working_period <- 1
    }

    # drop people with end_date prior to working_t_start
    # drop people with start_date after working_t_end
    working_pop <- study_pop %>%
      dplyr::filter(.data$cohort_end_date >= .env$working_t_start) %>%
      dplyr::filter(.data$cohort_start_date <= .env$working_t_end)

    # individuals start date for this period
    # which could be start of the period or later
    working_pop <- working_pop %>%
      dplyr::mutate(t_start_date =
                      dplyr::if_else(.data$cohort_start_date <= .env$working_t_start,
                                     .env$working_t_start,
                                     .data$cohort_start_date
                      ))


    # individuals end date for this period
    # end of the period or earlier
    working_pop <- working_pop %>%
      dplyr::mutate(
        t_end_date =
          dplyr::if_else(.data$cohort_end_date >= .env$working_t_end,
                         .env$working_t_end,
                         .data$cohort_end_date
          )
      )

    working_pop <- working_pop %>%
      dplyr::left_join(outcome,
                       by = "subject_id"
      )

    working_pop <- working_pop %>%
      dplyr::select("subject_id", "t_start_date",
                    "t_end_date", "outcome_start_date", "outcome_end_date")

    working_pop <- working_pop %>%
      dplyr::mutate(contribution = (as.numeric(difftime(.data$t_end_date,
                                                        .data$t_start_date,
                                                        units = "days")) + 1) /
                      working_period) %>%
      dplyr::group_by(.data$subject_id) %>%
      dplyr::mutate(contribution = sum(.data$contribution)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$contribution >= .env$minimum_representative_proportion)

    denominator <- working_pop %>%
      dplyr::select("subject_id") %>%
      dplyr::distinct() %>%
      nrow()

    numerator <- working_pop %>%
      dplyr::filter(.data$outcome_start_date <= .data$t_end_date) %>%
      dplyr::filter(.data$outcome_end_date >= .data$t_start_date) %>%
      dplyr::select("subject_id") %>%
      dplyr::distinct() %>%
      nrow()

    pr[[paste0(i)]] <- study_days[i,] %>%
      dplyr::mutate(numerator = numerator) %>%
      dplyr::mutate(denominator = denominator) %>%
      dplyr::mutate(prev = numerator / denominator) %>%
      dplyr::select("time","numerator","denominator","prev","start_time","end_time")
  }

  pr <- dplyr::bind_rows(pr)

  # study design related variables
  # add study design related variables
  analysis_settings <- tibble::tibble(
    type = .env$type,
    point = .env$point,
    time_interval = .env$time_interval,
    minimum_representative_proportion = .env$minimum_representative_proportion,
    full_periods_required = .env$full_periods_required
  )

  study_pop <- study_pop %>%
    dplyr::select("subject_id","cohort_start_date","cohort_end_date")

  results<-list()
  results[["pr"]]<-pr
  results[["analysis_settings"]]<-analysis_settings
  results[["person_table"]]<-study_pop
  results[["attrition"]]<-tibble::tibble(attrition="attrition") # placeholder

  return(results)
}
