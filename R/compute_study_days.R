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

#' Compute study dates.
#'
#' @param start_date start date of the study
#' @param end_date end date of the study
#' @param time_interval interval to compute
#' @param full_periods_required whether full periods are required, if a period is given
#' @param type point or period, default value is period
#' @param point point of computation in case type = point
#'
#' @noRd
compute_study_days <- function(start_date,
                               end_date,
                               time_interval,
                               full_periods_required,
                               type = "period",
                               point = NULL) {
  if (time_interval == "weeks"){
    week_correction <- lubridate::days(1)
  } else {
    week_correction <- lubridate::days(0)
  }
  if (time_interval == "days") {
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
      #dplyr::mutate(end_time = as.Date(NA)) %>%
      dplyr::mutate(end_time = .data$start_time) %>%
      dplyr::select("time","start_time","end_time")
  } else if (type == "point") {
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
    if (full_periods_required){
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
