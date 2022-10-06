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

#' Compute study dates for prevalence
#' @param start_date start date of the study
#' @param end_date end date of the study
#' @param type point or period prevalence
#' @param time_interval interval to compute prevalence
#' @param point point where to compute prevalence, if point prevalence#
#' @param full_period_required wether full periods are required, if period
#' prevalence
#' @noRd
compute_study_days <- function(start_date,
                               end_date,
                               type,
                               time_interval,
                               point,
                               full_period_required) {
  if (time_interval == "weeks"){
    week_correction <- lubridate::days(1)
  } else {
    week_correction <- lubridate::days(0)
  }
  if (time_interval == "days"){
    study_days<-dplyr::tibble(start_time=seq.Date(from=start_date,
                                                  to=end_date,
                                                  by="days")) %>%
      dplyr::mutate(day=lubridate::day(.data$start_time)) %>%
      dplyr::mutate(month=lubridate::month(.data$start_time)) %>%
      dplyr::mutate(year=lubridate::year(.data$start_time)) %>%
      dplyr::mutate(time=dplyr::if_else(.data$month < 10,
                                        paste0(.data$year,"_0",.data$month),
                                        paste0(.data$year,"_",.data$month))) %>%
      dplyr::mutate(time=dplyr::if_else(.data$day < 10,
                                        paste0(.data$time,"_0",.data$day),
                                        paste0(.data$time,"_",.data$day))) %>%
      dplyr::mutate(end_time = as.Date(NA)) %>%
      dplyr::select("time","start_time","end_time")
  } else if (type == "point"){
    unit <- substr(time_interval, 1, nchar(time_interval) - 1)
    start_day <- lubridate::floor_date(start_date, unit = unit) +
      week_correction +
      switch(point,
             "start" = lubridate::days(0),
             "middle" = switch(time_interval,
                               "weeks" = lubridate::days(3),
                               "months" = lubridate::days(14),
                               "quarters" = months(1) + lubridate::days(14),
                               "years" = months(6)
             ),
             "end" = switch(time_interval,
                            "weeks" = lubridate::days(6),
                            "months" = months(1) - lubridate::days(1),
                            "quarters" = months(3) - lubridate::days(1),
                            "years" = lubridate::years(1) - lubridate::days(1)
             )
      )
    if (end_date >= start_day){
      study_days<-dplyr::tibble(start_time=seq.Date(from=start_day,
                                                    to=end_date,
                                                    by=time_interval)) %>%
        dplyr::mutate(isoweek=lubridate::isoweek(.data$start_time)) %>%
        dplyr::mutate(month=lubridate::month(.data$start_time)) %>%
        dplyr::mutate(quarter=quarters(.data$start_time)) %>%
        dplyr::mutate(year=lubridate::year(.data$start_time)) %>%
        dplyr::mutate(years=glue::glue("{year}"))%>%
        dplyr::mutate(months=dplyr::if_else(.data$month < 10,
                                            paste0(.data$year,"_0",.data$month),
                                            paste0(.data$year,"_",.data$month))) %>%
        dplyr::mutate(quarters=glue::glue("{year}_{quarter}"))%>%
        dplyr::mutate(year=dplyr::if_else(.data$month == 1 & .data$isoweek > 50,
                                          .data$year - 1,
                                          .data$year)) %>%
        dplyr::mutate(weeks=dplyr::if_else(.data$isoweek < 10,
                                           paste0(.data$year,"_0",.data$isoweek),
                                           paste0(.data$year,"_",.data$isoweek))) %>%
        dplyr::rename("time" = time_interval) %>%
        dplyr::mutate(time = as.character(.data$time)) %>%
        dplyr::select("time","start_time") %>%
        dplyr::mutate(end_time = as.Date(NA)) %>%
        dplyr::filter(.data$start_time >= start_date)
    } else {
      study_days<-dplyr::tibble()
    }
  } else {
    study_days <- dplyr::tibble(dates=seq.Date(from=start_date,
                                               to=end_date,
                                               by="days")) %>%
      dplyr::mutate(isoweek=lubridate::isoweek(.data$dates)) %>%
      dplyr::mutate(month=lubridate::month(.data$dates)) %>%
      dplyr::mutate(quarter=quarters(.data$dates)) %>%
      dplyr::mutate(year=lubridate::year(.data$dates)) %>%
      dplyr::mutate(years=glue::glue("{year}"))%>%
      dplyr::mutate(months=dplyr::if_else(.data$month < 10,
                                          paste0(.data$year,"_0",.data$month),
                                          paste0(.data$year,"_",.data$month))) %>%
      dplyr::mutate(quarters=glue::glue("{year}_{quarter}"))%>%
      dplyr::mutate(year=dplyr::if_else(.data$month == 1 & .data$isoweek > 50,
                                        .data$year - 1,
                                        .data$year)) %>%
      dplyr::mutate(weeks=dplyr::if_else(.data$isoweek < 10,
                                         paste0(.data$year,"_0",.data$isoweek),
                                         paste0(.data$year,"_",.data$isoweek)))%>%
      dplyr::rename("time" = time_interval) %>%
      dplyr::mutate(time = as.character(.data$time)) %>%
      dplyr::group_by(.data$time) %>%
      dplyr::summarise(
        start_time = min(.data$dates, na.rm = TRUE),
        end_time = max(.data$dates, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
    if (full_period_required){
      study_days <- study_days %>%
        dplyr::filter(.data$start_time == lubridate::floor_date(.data$start_time, unit = time_interval) + week_correction) %>%
        dplyr::filter(.data$end_time == lubridate::floor_date(
          .data$end_time,
          unit = time_interval
        ) + week_correction + switch(time_interval,
                                     "weeks" = lubridate::days(6),
                                     "months" = months(1) - lubridate::days(1),
                                     "quarters" = months(3) - lubridate::days(1),
                                     "years" = lubridate::years(1) - lubridate::days(1)
        ))
    }
  }
  return(study_days)
}


#' Get population prevalence estimates
#'
#' @param cdm CDMConnector CDM reference object
#' @param table_name_denominator table_name_denominator
#' @param cohort_id_denominator_pop cohort_id_denominator_pop
#' @param table_name_outcome table_name_outcome
#' @param cohort_id_outcome cohort_id_outcome
#' @param type type
#' @param time_interval time_interval
#' @param full_period_required full period requirement
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
                               full_period_required = TRUE,
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
  study_days <- compute_study_days(
    start_date = start_date,
    end_date = end_date,
    type = type,
    time_interval = time_interval,
    point = point,
    full_period_required = full_period_required
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
    full_period_required = .env$full_period_required
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
