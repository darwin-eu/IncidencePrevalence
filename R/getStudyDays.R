# Copyright 2025 DARWIN EU®
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
#' @param startDate start date of the study
#' @param endDate end date of the study
#' @param timeInterval interval to compute, can be "weeks", "months",
#' "quarters", "years"
#' @param completeDatabaseIntervals whether full periods are required
#' @param type point or period, default value is period
#' @param timePoint timePoint of computation in case type = point,
#' can be "start", "middle", "end"
#'
#' @noRd
getStudyDays <- function(startDate,
                         endDate,
                         timeInterval,
                         completeDatabaseIntervals,
                         type = "period",
                         timePoint = NULL) {
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)
  unit <- substr(timeInterval, 1, nchar(timeInterval) - 1)

  if (timeInterval == "weeks") {
    weekCorrection <- 1
  } else {
    weekCorrection <- 0
  }

  if (type == "point") {
    startDay <- as.Date(clock::as_year_month_day(
      clock::calendar_start(
        helper_function_clock(startDate, unit),
        precision = unit
      )
    ))
    studyDays <- getStudyDaysElements(startDay, endDate, timeInterval) %>%
      dplyr::mutate(
        start_time =
          switch(timePoint,
            "start" = .data$start_time %>% clock::add_days(weekCorrection) %>%
              clock::add_days(0),
            "middle" = switch(timeInterval,
              "weeks" = .data$start_time %>% clock::add_days(weekCorrection) %>%
                clock::add_days(3),
              "months" = .data$start_time %>% clock::add_days(weekCorrection) %>%
                clock::add_days(14),
              "quarters" = .data$start_time %>% clock::add_days(weekCorrection) %>%
                clock::add_months(1) %>% clock::add_days(14),
              "years" = .data$start_time %>% clock::add_days(weekCorrection) %>%
                clock::add_months(6)
            ),
            "end" = switch(timeInterval,
              "weeks" = .data$start_time %>% clock::add_days(weekCorrection) %>%
                clock::add_days(6),
              "months" = .data$start_time %>% clock::add_days(weekCorrection) %>%
                clock::add_months(1) %>% clock::add_days(-1),
              "quarters" = .data$start_time %>% clock::add_days(weekCorrection) %>%
                clock::add_months(3) %>% clock::add_days(-1),
              "years" = .data$start_time %>% clock::add_days(weekCorrection) %>%
                clock::add_years(1) %>% clock::add_days(-1)
            )
          )
      ) %>%
      dplyr::rename("time" = .env$timeInterval) %>%
      dplyr::mutate(time = as.character(.data$time)) %>%
      dplyr::select("time", "start_time") %>%
      dplyr::mutate(end_time = .data$start_time) %>%
      dplyr::filter(.data$start_time >= startDate) %>%
      dplyr::filter(.data$start_time <= endDate)
  } else {
    studyDays <- getStudyDaysElements(startDate, endDate, "days")
    studyDays <- studyDays %>%
      dplyr::mutate(overall = "overall") %>%
      dplyr::rename("time" = .env$timeInterval) %>%
      dplyr::mutate(time = as.character(.data$time)) %>%
      dplyr::rename("dates" = "start_time") %>%
      dplyr::group_by(.data$time) %>%
      dplyr::summarise(
        start_time = min(.data$dates, na.rm = TRUE),
        end_time = max(.data$dates, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
    if (completeDatabaseIntervals == TRUE) {
      if (timeInterval == "weeks") {
        studyDays <- studyDays %>%
          dplyr::filter(difftime(studyDays$end_time,
            studyDays$start_time,
            units = "days"
          ) == 6)
      }
      if (timeInterval %in% c("months", "quarters", "years")) {
        studyDays <- studyDays %>%
          dplyr::filter(.data$start_time ==
            as.Date(clock::as_year_month_day(
              clock::calendar_start(
                helper_function_clock(.data$start_time, unit),
                precision = unit
              )
            )) %>% clock::add_days(weekCorrection)) %>%
          dplyr::filter(.data$end_time ==
            as.Date(clock::as_year_month_day(
              clock::calendar_end(
                helper_function_clock(.data$end_time, unit),
                precision = unit
              )
            )))
      }
    }
  }
  return(studyDays)
}

getStudyDaysElements <- function(s, e, i) {
  x <- dplyr::tibble(start_time = seq.Date(
    from = s,
    to = e,
    by = i
  ))
  x <- x %>%
    dplyr::mutate(isoweek = clock::get_week(
      clock::as_iso_year_week_day(.data$start_time)
    )) %>%
    dplyr::mutate(month = clock::get_month(.data$start_time)) %>%
    dplyr::mutate(quarter = quarters(.data$start_time)) %>%
    dplyr::mutate(year = clock::get_year(.data$start_time)) %>%
    dplyr::mutate(years = glue::glue("{year}")) %>%
    dplyr::mutate(months = dplyr::if_else(.data$month < 10,
      paste0(.data$year, "_0", .data$month),
      paste0(.data$year, "_", .data$month)
    )) %>%
    dplyr::mutate(quarters = glue::glue("{year}_{quarter}")) %>%
    dplyr::mutate(
      year =
        dplyr::if_else(.data$month == 1 & .data$isoweek > 50,
          .data$year - 1,
          .data$year
        )
    ) %>%
    dplyr::mutate(weeks = dplyr::if_else(.data$isoweek < 10,
      paste0(.data$year, "_0", .data$isoweek),
      paste0(.data$year, "_", .data$isoweek)
    ))
  return(x)
}

helper_function_clock <- function(startDate, unit) {
  if (unit == "week") {
    return(clock::as_year_week_day(startDate))
  }
  if (unit == "quarter") {
    return(clock::as_year_quarter_day(startDate))
  }
  if (unit == "month" | unit == "year") {
    return(clock::as_year_month_day(startDate))
  }
}
