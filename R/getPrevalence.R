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

getPrevalence <- function(cdm,
                          denominatorTable,
                          denominatorCohortId,
                          outcomeTable,
                          outcomeCohortId,
                          type,
                          weeks,
                          months,
                          quarters,
                          years,
                          overall,
                          completeDatabaseIntervals,
                          timePoint,
                          fullContribution,
                          level,
                          tablePrefix,
                          analysisId,
                          strata,
                          includeOverallStrata) {
  # keeping outcome of interest
  # of people in the denominator of interest
  studyPop <- cdm[[denominatorTable]] %>%
    dplyr::mutate(
      cohort_start_date = as.Date(.data$cohort_start_date),
      cohort_end_date = as.Date(.data$cohort_end_date)
    ) %>%
    dplyr::filter(.data$cohort_definition_id ==
      .env$denominatorCohortId) %>%
    dplyr::select(-"cohort_definition_id") %>%
    dplyr::left_join(
      cdm[[outcomeTable]] %>%
        dplyr::filter(.data$cohort_definition_id == .env$outcomeCohortId) %>%
        dplyr::rename(
          "outcome_start_date" = "cohort_start_date",
          "outcome_end_date" = "cohort_end_date"
        ) %>%
        dplyr::select(
          "subject_id", "outcome_start_date",
          "outcome_end_date"
        ),
      by = "subject_id"
    )

  studyPop <- studyPop %>%
    dplyr::collect()

  attrition <- recordAttrition(
    table = studyPop,
    id = "subject_id",
    reasonId = 11,
    reason = "Starting analysis population"
  )

  startEnd <- studyPop %>%
    dplyr::summarise(
      min = min(.data$cohort_start_date, na.rm = TRUE),
      max = max(.data$cohort_end_date, na.rm = TRUE)
    ) %>%
    dplyr::collect()

  # get studyDays as a function of inputs
  interval <- c(
    "weeks" = weeks,
    "months" = months,
    "quarters" = quarters,
    "years" = years,
    "overall" = overall
  )
  interval <- names(interval[interval])
  # study dates
  # based on the earliest start and latest end of those
  # in the relevant denominator
  studyDays <- list()
  if (nrow(studyPop) > 0) {
    startEnd <- studyPop %>%
      dplyr::summarise(
        min = as.Date(min(.data$cohort_start_date, na.rm = TRUE)),
        max = as.Date(max(.data$cohort_end_date, na.rm = TRUE))
      )
    if ("weeks" %in% interval) {
      studyDays[["weeks"]] <- getStudyDays(
        startDate = startEnd$min,
        endDate = startEnd$max,
        timeInterval = "weeks",
        completeDatabaseIntervals = completeDatabaseIntervals,
        type = type,
        timePoint = timePoint
      )
    }
    if ("months" %in% interval) {
      studyDays[["months"]] <- getStudyDays(
        startDate = startEnd$min,
        endDate = startEnd$max,
        timeInterval = "months",
        completeDatabaseIntervals = completeDatabaseIntervals,
        type = type,
        timePoint = timePoint
      )
    }
    if ("quarters" %in% interval) {
      studyDays[["quarters"]] <- getStudyDays(
        startDate = startEnd$min,
        endDate = startEnd$max,
        timeInterval = "quarters",
        completeDatabaseIntervals = completeDatabaseIntervals,
        type = type,
        timePoint = timePoint
      )
    }
    if ("years" %in% interval) {
      studyDays[["years"]] <- getStudyDays(
        startDate = startEnd$min,
        endDate = startEnd$max,
        timeInterval = "years",
        completeDatabaseIntervals = completeDatabaseIntervals,
        type = type,
        timePoint = timePoint
      )
    }
    if ("overall" %in% interval) {
      # note, full periods argument does not apply
      # for overall we just go from start to end
      studyDays[["overall"]] <- dplyr::tibble(
        time = "overall",
        start_time = startEnd$min,
        end_time = startEnd$max
      )
    }
  } else {
    # empty population
    studyDays[["none"]] <- dplyr::tibble()
  }

  if (nrow(dplyr::bind_rows(studyDays)) == 0) {
    # no study days
    studyDays[["none"]] <- dplyr::tibble()
  }

  if ("none" %in% names(studyDays)) {
    # if no study days we<U+00B4>ll return an empty tibble
    pr <- dplyr::tibble()

    attrition <- recordAttrition(
      table = dplyr::tibble(subject_id = integer()),
      id = "subject_id",
      reasonId = 12,
      reason = "Not observed during the complete database interval",
      existingAttrition = attrition
    )
    attrition <- recordAttrition(
      table = dplyr::tibble(subject_id = integer()),
      id = "subject_id",
      reasonId = 13,
      reason = "Do not satisfy full contribution requirement for any time interval",
      existingAttrition = attrition
    )
  }

  if (!"none" %in% names(studyDays)) {
    # drop for complete database intervals requirement
    minStartDate <- min(dplyr::bind_rows(studyDays)$start_time)
    maxStartDate <- max(dplyr::bind_rows(studyDays)$end_time)

    minStartDateChar <- as.character(minStartDate)
    maxStartDateChar <- as.character(maxStartDate)

    allstudyDays <- dplyr::bind_rows(studyDays)

    studyPop <- studyPop %>%
      dplyr::filter(
        .data$cohort_end_date >= as.Date(.env$minStartDateChar),
        .data$cohort_start_date <= as.Date(.env$maxStartDateChar)
      )

    attrition <- recordAttrition(
      table = studyPop,
      id = "subject_id",
      reasonId = 12,
      reason = "Not observed during the complete database interval",
      existingAttrition = attrition
    )

    # drop people who never fulfill contribution requirement
    if (fullContribution == TRUE) {
      checkExpression <- glue::glue("(.data$cohort_end_date >= allstudyDays$end_time[{seq_along(allstudyDays$end_time)}]) &
           (.data$cohort_start_date <= allstudyDays$start_time[{seq_along(allstudyDays$start_time)}])") %>%
        paste0(collapse = "|") %>%
        rlang::parse_expr()

      studyPop <- studyPop %>%
        dplyr::mutate(
          has_full_contribution = dplyr::if_else(!!checkExpression,
            1L,
            0L
          )
        ) %>%
        dplyr::filter(.data$has_full_contribution >= 1) %>%
        dplyr::select(-"has_full_contribution")

      attrition <- recordAttrition(
        table = studyPop,
        id = "subject_id",
        reasonId = 13,
        reason = "Do not satisfy full contribution requirement for an interval",
        existingAttrition = attrition
      )
    } else if (fullContribution == FALSE) { # otherwise nobody excluded
      attrition <- recordAttrition(
        table = studyPop,
        id = "subject_id",
        reasonId = 14,
        reason = "Do not satisfy full contribution requirement for an interval",
        existingAttrition = attrition
      )
    }

    # fetch prevalence
    # looping through each time interval
    pr <- list()
    for (j in seq_along(interval)) {
      workingInterval <- interval[j]
      workingStudyDays <- studyDays[[workingInterval]]
      for (i in seq_len(nrow(workingStudyDays))) {
        workingStart <- workingStudyDays$start_time[i]
        workingEnd <- workingStudyDays$end_time[i]

        if (fullContribution == TRUE) {
          # require presence for all of period
          # drop people with end_date not after workingEnd
          # and start_date not before workingStart
          workingPop <- studyPop %>%
            dplyr::filter(
              .data$cohort_end_date >= .env$workingEnd,
              .data$cohort_start_date <= .env$workingStart
            )
        } else {
          # otherwise include people if they can contribute a day
          # drop people with end_date prior to workingStart
          # and start_date after workingEnd
          workingPop <- studyPop %>%
            dplyr::filter(
              .data$cohort_end_date >= .env$workingStart,
              .data$cohort_start_date <= .env$workingEnd
            )
        }

        workingPop <- workingPop %>%
          dplyr::mutate(
            # individuals start date for this period
            # which could be start of the period or later
            cohort_start_date =
              dplyr::if_else(.data$cohort_start_date <= .env$workingStart,
                .env$workingStart,
                as.Date(.data$cohort_start_date)
              ),
            # individuals end date for this period
            # end of the period or earlier
            cohort_end_date =
              dplyr::if_else(.data$cohort_end_date >= .env$workingEnd,
                .env$workingEnd,
                as.Date(.data$cohort_end_date)
              )
          )

        if (length(strata) == 0 || includeOverallStrata == TRUE) {
          # include ongoing in current time of interest
          if(level == "person"){
          result <- workingPop %>%
            dplyr::summarise(
              denominator_count = dplyr::n_distinct(.data$subject_id),
              outcome_count = dplyr::n_distinct(.data$subject_id[
                !is.na(.data$outcome_start_date) &
                  .data$outcome_start_date <= .data$cohort_end_date &
                  .data$outcome_end_date >= .data$cohort_start_date
              ])
            )
          } else { # record level
            result <-  cbind(
              # count of cohort entries
              workingPop |>
              dplyr::select("subject_id", "cohort_start_date") |>
              dplyr::distinct() |>
              dplyr::summarise(denominator_count = dplyr::n()),
              # count of outcomes per cohort entry
                workingPop |>
                  dplyr::mutate(
                    has_outcome = dplyr::if_else(!is.na(.data$outcome_start_date) &
                                                   .data$outcome_start_date <= .data$cohort_end_date &
                                                   .data$outcome_end_date >= .data$cohort_start_date,
                                                 1, 0)
                  ) |>
                  dplyr::select("subject_id", "cohort_start_date", "has_outcome") |>
                  dplyr::distinct() |>
                  dplyr::summarise(outcome_count = sum(.data$has_outcome)))
          }

          pr[[paste0(i, "_", j)]] <- dplyr::tibble(
            denominator_count = result$denominator_count,
            outcome_count = result$outcome_count
          ) %>%
            dplyr::mutate(
              prevalence_start_date = .env$workingStart,
              prevalence_end_date = .env$workingEnd,
              analysis_interval = .env$workingInterval
            )
        } else {
          pr[[paste0(i, "_", j)]] <- dplyr::tibble()
        }

        if (length(strata) >= 1) {
          pr[[paste0(i, "_", j)]] <- pr[[paste0(i, "_", j)]] %>%
            omopgenerics::uniteStrata()
          for (k in seq_along(strata)) {
            pr[[paste0(i, "_", j, "_", k)]] <- dplyr::bind_rows(
              pr[[paste0(i, "_", j)]],
              getStratifiedPrevalenceResult(workingPop,
                workingStrata = strata[[k]], level
              )
            ) %>%
              dplyr::mutate(
                prevalence_start_date = .env$workingStart,
                prevalence_end_date = .env$workingEnd,
                analysis_interval = .env$workingInterval
              )
          }
        }
      }
    }
    pr <- dplyr::bind_rows(pr) %>%
      dplyr::mutate(prevalence = round((.data$outcome_count / .data$denominator_count), 5)) %>%
      dplyr::select(dplyr::any_of(c(
        "outcome_count", "denominator_count",
        "prevalence", "prevalence_start_date", "prevalence_end_date",
        "strata_name", "strata_level", "analysis_interval"
      )))
  }

  results <- list()
  results[["pr"]] <- pr
  results[["attrition"]] <- attrition

  return(results)
}

getStratifiedPrevalenceResult <- function(workingPop, workingStrata, level) {
  # include ongoing in current time of interest
  if(level == "person"){
  result <- workingPop %>%
    dplyr::group_by(dplyr::pick(.env$workingStrata)) %>%
    dplyr::summarise(
      denominator_count = dplyr::n_distinct(.data$subject_id),
      outcome_count = dplyr::n_distinct(.data$subject_id[
        !is.na(.data$outcome_start_date) &
          .data$outcome_start_date <= .data$cohort_end_date &
          .data$outcome_end_date >= .data$cohort_start_date
      ])
    ) %>%
    dplyr::ungroup()
  } else { # record level
    result <-  workingPop |>
      dplyr::group_by(dplyr::pick(.env$workingStrata)) |>
      dplyr::select(.env$workingStrata, "subject_id", "cohort_start_date") |>
      dplyr::distinct() |>
      dplyr::summarise(denominator_count = dplyr::n()) |>
      dplyr::left_join(
        workingPop |>
      dplyr::group_by(dplyr::pick(.env$workingStrata)) |>
      dplyr::mutate(
        has_outcome = dplyr::if_else(!is.na(.data$outcome_start_date) &
          .data$outcome_start_date <= .data$cohort_end_date &
          .data$outcome_end_date >= .data$cohort_start_date,
        1, 0)
      ) |>
      dplyr::select(.env$workingStrata, "subject_id", "cohort_start_date", "has_outcome") |>
      dplyr::distinct() |>
      dplyr::summarise(outcome_count = sum(.data$has_outcome)),
      by = workingStrata)

  }

  result <- result %>%
    omopgenerics::uniteStrata(cols = workingStrata)

  return(result)
}
