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


#' Collect population incidence estimates
#'
#' @param cdm A CDM reference object
#' @param denominatorTable A cohort table with a set of denominator cohorts
#' (for example, created using the `generateDenominatorCohortSet()`
#' function).
#' @param outcomeTable A cohort table in the cdm reference containing
#' a set of outcome cohorts.
#' @param denominatorCohortId The cohort definition ids of the denominator
#' cohorts of interest. If NULL all cohorts will be considered in the
#' analysis.
#' @param outcomeCohortId The cohort definition ids of the outcome
#' cohorts of interest. If NULL all cohorts will be considered in the
#' analysis.
#' @param outcomeCohortName Corresponding names for each outcomeCohortId.
#' @param interval Time intervals over which incidence is estimated. Can
#' be "weeks", "months", "quarters", "years", or "overall". ISO weeks will
#' be used for weeks. Calendar months, quarters, or years can be used, or an
#' overall estimate for the entire time period observed (from earliest cohort
#' start to last cohort end) can also be estimated. If more than one option is
#' chosen then results will be estimated for each chosen interval.
#' @param completeDatabaseIntervals TRUE/ FALSE. Where TRUE, incidence will
#' only be estimated for those intervals where the database
#' captures all the interval (based on the earliest and latest observation
#' period start dates, respectively).
#' @param outcomeWashout The number of days used for a 'washout' period
#' between the end of one outcome and an individual starting to contribute
#' time at risk. If NULL, no time can be contributed after an event has
#' occurred (whether during the study period or if occurring beforehand).
#' @param repeatedEvents TRUE/ FALSE. If TRUE, an individual will be able to
#' contribute multiple events during the study period (time while they are
#' present in an outcome cohort and any subsequent washout will be
#' excluded). If FALSE, an individual will only contribute time up to their
#' first event during the study period.
#' @param minCellCount The minimum number of events to reported, below which
#' results will be obscured. If 0, all results will be reported.
#' @param verbose Either TRUE or FALSE. If TRUE, progress will be reported.
#'
#' @return Incidence estimates
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here ")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' cdm$denominator <- generateDenominatorCohortSet(
#'   cdm = cdm,
#'   startDate = as.Date("2008-01-01"),
#'   endDate = as.Date("2018-01-01")
#' )
#' inc <- estimateIncidence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' }
estimateIncidence <- function(cdm,
                              denominatorTable,
                              outcomeTable,
                              denominatorCohortId = NULL,
                              outcomeCohortId = NULL,
                              outcomeCohortName = NULL,
                              interval = "years",
                              completeDatabaseIntervals = TRUE,
                              outcomeWashout = 0,
                              repeatedEvents = FALSE,
                              minCellCount = 5,
                              verbose = FALSE) {
  if (verbose == TRUE) {
    startCollect <- Sys.time()
  }
  # help to avoid formatting errors
  if (is.character(interval)) {
    interval <- tolower(interval)
  }

  ## check for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()
  cdmCheck <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdmCheck,
    add = errorMessage
  )
  if (!isTRUE(cdmCheck)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
    }
  checkmate::reportAssertions(collection = errorMessage)
  errorMessage <- checkmate::makeAssertCollection()
  denominatorCheck <- denominatorTable %in% names(cdm)
  checkmate::assertTRUE(denominatorCheck,
    add = errorMessage
  )
  if (!isTRUE(denominatorCheck)) {
    errorMessage$push(
      "- `denominatorTable` is not found in cdm"
    )
  }
  checkmate::assertIntegerish(denominatorCohortId,
    add = errorMessage,
    null.ok = TRUE
  )
  outcomeCheck <- outcomeTable %in% names(cdm)
  checkmate::assertTRUE(outcomeCheck,
    add = errorMessage
  )
  if (!isTRUE(outcomeCheck)) {
    errorMessage$push(
      "- `outcomeTable` is not found in cdm"
    )
  }
  checkmate::assertIntegerish(outcomeCohortId,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assertCharacter(outcomeCohortName,
                             add = errorMessage,
                             null.ok = TRUE
  )
  checkmate::assertTRUE(
    all(interval %in%
      c(
        "weeks", "months",
        "quarters", "years",
        "overall"
      )),
    add = errorMessage
  )
  checkmate::assert_logical(completeDatabaseIntervals,
    add = errorMessage
  )
  checkmate::assert_numeric(outcomeWashout,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assert_logical(repeatedEvents,
    add = errorMessage
  )
  checkmate::assert_number(minCellCount)
  checkmate::assert_logical(verbose,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  # if not given, use all denominator and outcome cohorts
  if (is.null(denominatorCohortId)) {
    denominatorCohortId <- cdm[[denominatorTable]] %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::pull()
  }
  if (is.null(outcomeCohortId)) {
    outcomeCohortId <- cdm[[outcomeTable]] %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::pull()
  }
  if (is.null(outcomeCohortName)) {
    outcomeCohortName <- NA
  }
   outcomeRef <- tibble::tibble(
      outcome_cohort_id = .env$outcomeCohortId,
      outcome_cohort_name = .env$outcomeCohortName
    )

  # further checks that there are the required data elements
  errorMessage <- checkmate::makeAssertCollection()
  denomCountCheck <- cdm[[denominatorTable]] %>%
    dplyr::filter(.data$cohort_definition_id %in%
                    .env$denominatorCohortId) %>%
    dplyr::count() %>%
    dplyr::pull() > 0
  if (!isTRUE(denomCountCheck)) {
    errorMessage$push(
      "- nobody in `denominatorTable` with one of the `denominatorCohortId`"
    )
  }
  outcomeCountCheck <- cdm[[outcomeTable]] %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$outcomeCohortId) %>%
    dplyr::count() %>%
    dplyr::pull() > 0
  checkmate::assertTRUE(outcomeCountCheck,
    add = errorMessage
  )
  if (!isTRUE(outcomeCountCheck)) {
    errorMessage$push(
      "- nobody in `outcomeTable` with one of the `outcomeCohortId`"
    )
  }
  checkmate::reportAssertions(collection = errorMessage)


  # get outcomes + cohort_start_date & cohort_end_date
  outcome <- cdm[[outcomeTable]] %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$outcomeCohortId) %>%
    dplyr::rename("outcome_cohort_id" = "cohort_definition_id") %>%
    dplyr::rename("outcome_start_date" = "cohort_start_date") %>%
    dplyr::rename("outcome_end_date" = "cohort_end_date") %>%
    dplyr::inner_join(
      cdm[[denominatorTable]] %>%
        dplyr::filter(.data$cohort_definition_id %in%
                        .env$denominatorCohortId) %>%
        dplyr::select(-"cohort_definition_id") %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    CDMConnector::computeQuery()

  outcome <- outcome %>%
    # most recent outcome starting before cohort start per person
    dplyr::filter(.data$outcome_start_date < .data$cohort_start_date) %>%
    dplyr::group_by(
      .data$subject_id,
      .data$cohort_start_date,
      .data$outcome_cohort_id
    ) %>%
    dplyr::filter(.data$outcome_start_date ==
                    max(.data$outcome_start_date, na.rm = TRUE)) %>%
    dplyr::union_all(
      # all starting during cohort period
      outcome %>%
        dplyr::filter(.data$outcome_start_date >= .data$cohort_start_date) %>%
        dplyr::filter(.data$outcome_start_date <= .data$cohort_end_date)
    ) %>%
    CDMConnector::computeQuery()

  outcome <- outcome %>%
    dplyr::group_by(
      .data$subject_id,
      .data$cohort_start_date,
      .data$outcome_cohort_id
    ) %>%
    dbplyr::window_order(.data$outcome_start_date) %>%
    dplyr::mutate(index = rank()) %>%
    dplyr::ungroup() %>%
    CDMConnector::computeQuery()

  # add to cdm reference
  cdm[[outcomeTable]] <- outcome %>%
    dplyr::select(-"outcome_end_date") %>%
    dplyr::full_join(
      outcome %>%
        dplyr::mutate(index = .data$index + 1) %>%
        dplyr::rename("outcome_prev_end_date" = "outcome_end_date") %>%
        dplyr::select(-"outcome_start_date"),
      by = c(
        "subject_id", "cohort_start_date",
        "cohort_end_date", "outcome_cohort_id", "index"
      )
    ) %>%
    dplyr::select(-"index") %>%
    CDMConnector::computeQuery()

  studySpecs <- tidyr::expand_grid(
    outcome_cohort_id = outcomeCohortId,
    denominator_cohort_id = denominatorCohortId,
    interval = interval,
    complete_database_intervals = completeDatabaseIntervals,
    outcome_washout = outcomeWashout,
    repeated_events = repeatedEvents
  )
  if (is.null(outcomeWashout)) {
    studySpecs$outcome_washout <- NA
  }
  studySpecs <- studySpecs %>%
    dplyr::mutate(analysis_id = as.character(dplyr::row_number()))
  studySpecs <- split(
    studySpecs,
    studySpecs[, c("analysis_id")]
  )

  # get irs
  counter <- 0
  irsList <- lapply(studySpecs, function(x) {
    if (verbose == TRUE) {
      counter <<- counter + 1
      message(glue::glue(
        "Getting incidence for analysis {counter} of {length(studySpecs)}"
      ))
    }

    workingInc <- getIncidence(
      cdm = cdm,
      denominatorTable = denominatorTable,
      denominatorCohortId = x$denominator_cohort_id,
      outcomeTable = outcomeTable,
      outcomeCohortId = x$outcome_cohort_id,
      interval = x$interval,
      completeDatabaseIntervals = x$complete_database_intervals,
      outcomeWashout = x$outcome_washout,
      repeatedEvents = x$repeated_events,
      verbose = verbose
    )

    workingIncIr <- workingInc[["ir"]] %>%
      dplyr::mutate(analysis_id = x$analysis_id) %>%
      dplyr::relocate("analysis_id")

    workingIncPersonTable <- workingInc[["person_table"]] %>%
      dplyr::mutate(analysis_id = !!x$analysis_id) %>%
      dplyr::relocate("analysis_id")

    workingIncAnalysisSettings <- workingInc[["analysis_settings"]] %>%
      dplyr::mutate(
        outcome_cohort_id = x$outcome_cohort_id,
        denominator_cohort_id = x$denominator_cohort_id,
        analysis_interval = x$interval,
        analysis_outcome_washout = x$outcome_washout,
        analysis_repeated_events = x$repeated_events,
        analysis_min_cell_count = .env$minCellCount,
        analysis_id = x$analysis_id
      ) %>%
      dplyr::relocate("analysis_id")

    workingIncAttrition <- workingInc[["attrition"]] %>%
      dplyr::mutate(analysis_id = x$analysis_id) %>%
      dplyr::relocate("analysis_id")

    result <- list()
    result[["ir"]] <- workingIncIr
    result[["analysis_settings"]] <- workingIncAnalysisSettings
    result[[paste0(
      "study_population_analyis_",
      x$analysis_id
    )]] <- workingIncPersonTable
    result[["attrition"]] <- workingIncAttrition


    return(result)
  })

  irsList <- purrr::flatten(irsList)

  # analysis settings
  analysisSettings <- irsList[names(irsList) == "analysis_settings"]
  analysisSettings <- dplyr::bind_rows(analysisSettings,
    .id = NULL
  )
  analysisSettings <- analysisSettings %>%
    dplyr::left_join(
      settings(cdm[[denominatorTable]]) %>%
        dplyr::rename("cohort_id" = "cohort_definition_id") %>%
        dplyr::rename_with(
          .cols = tidyselect::everything(),
          function(x) {
            paste0("denominator_", x)
          }
        ),
      by = "denominator_cohort_id"
    )

  # attrition
  # combine analysis attrition with the previous attrition for
  # the denominator cohort used
  for (i in seq_along(studySpecs)) {
    irsList[names(irsList) == "attrition"][[i]] <- dplyr::bind_rows(
      attrition(cdm[[denominatorTable]]) %>%
        dplyr::rename("denominator_cohort_id" = "cohort_definition_id") %>%
        dplyr::filter(.data$denominator_cohort_id ==
                      studySpecs[[i]]$denominator_cohort_id) %>%
        dplyr::mutate(analysis_id = studySpecs[[i]]$analysis_id),
      irsList[names(irsList) == "attrition"][[i]] %>%
        dplyr::mutate(step = "Estimating incidence")
    )
  }
  attrition <- irsList[names(irsList) == "attrition"]
  attrition <- dplyr::bind_rows(attrition,
    .id = NULL
  ) %>%
    dplyr::select(!"denominator_cohort_id")


  # incidence estimates
  irs <- irsList[names(irsList) == "ir"]
  # to tibble
  irs <- dplyr::bind_rows(irs,
    .id = NULL
  )

  # get confidence intervals
  if (nrow(irs) > 0) {
    irs <- irs %>%
      dplyr::bind_cols(incRateCiExact(
        irs$n_events,
        irs$person_years
      ))

    # obscure counts
    irs <- obscureCounts(irs, minCellCount = minCellCount, substitute = NA)
  }

  # person_table summary
  personTable <- irsList[stringr::str_detect(
    names(irsList),
    "study_population"
  )]

  # return results as an IncidencePrevalenceResult class
  attr(irs, "settings") <- analysisSettings %>%
    dplyr::left_join(outcomeRef, by = "outcome_cohort_id") %>%
    dplyr::relocate("outcome_cohort_id", .after = "analysis_id") %>%
    dplyr::relocate("outcome_cohort_name", .after = "outcome_cohort_id")
  attr(irs, "attrition") <- attrition
  attr(irs, "participants") <- personTable

  class(irs) <- c("IncidencePrevalenceResult", class(irs))

  if (verbose == TRUE) {
    dur <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
    message(glue::glue(
      "Overall time taken: {floor(dur/60)} mins and {dur %% 60 %/% 1} secs"
    ))
  }
  return(irs)
}



incRateCiExact <- function(ev, pt) {
  return(tibble::tibble(
    incidence_100000_pys_95CI_lower =
      ((stats::qchisq(p = 0.025, df = 2 * ev) / 2) / pt) * 100000,
    incidence_100000_pys_95CI_upper =
      ((stats::qchisq(p = 0.975, df = 2 * (ev + 1)) / 2) / pt) * 100000
  ))
}
