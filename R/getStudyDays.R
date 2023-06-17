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

  if (timeInterval == "weeks") {
    weekCorrection <- lubridate::days(1)
  } else {
    weekCorrection <- lubridate::days(0)
  }
  if (type == "point") {
    unit <- substr(timeInterval, 1, nchar(timeInterval) - 1)
    startDay <- lubridate::floor_date(startDate, unit = unit)
    studyDays <- getStudyDaysElements(startDay, endDate, timeInterval) %>%
      dplyr::mutate(start_time = .data$start_time +
        weekCorrection +
        switch(timePoint,
          "start" = lubridate::days(0),
          "middle" = switch(timeInterval,
            "weeks" = lubridate::days(3),
            "months" = lubridate::days(14),
            "quarters" = months(1) + lubridate::days(14),
            "years" = months(6)
          ),
          "end" = switch(timeInterval,
            "weeks" = lubridate::days(6),
            "months" = months(1) - lubridate::days(1),
            "quarters" = months(3) - lubridate::days(1),
            "years" = lubridate::years(1) - lubridate::days(1)
          )
        )) %>%
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
            lubridate::floor_date(.data$start_time,
              unit = timeInterval
            ) +
              weekCorrection) %>%
          dplyr::filter(.data$end_time == lubridate::floor_date(
            .data$end_time,
            unit = timeInterval
          ) + weekCorrection + switch(timeInterval,
            "months" = months(1) - lubridate::days(1),
            "quarters" = months(3) - lubridate::days(1),
            "years" = lubridate::years(1) - lubridate::days(1)
          ))
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
    dplyr::mutate(isoweek = lubridate::isoweek(.data$start_time)) %>%
    dplyr::mutate(month = lubridate::month(.data$start_time)) %>%
    dplyr::mutate(quarter = quarters(.data$start_time)) %>%
    dplyr::mutate(year = lubridate::year(.data$start_time)) %>%
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
