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
#' @param interval Time intervals over which incidence is estimated. Can
#' be "weeks", "months", "quarters", "years", or "overall". ISO weeks will
#' be used for weeks. Calendar months, quarters, or years can be used, or an
#' overall estimate for the entire time period observed (from earliest cohort
#' start to last cohort end) can also be estimated. If more than one option is
#' chosen then results will be estimated for each chosen interval.
#' @param completeDatabaseIntervals TRUE/ FALSE. Where TRUE, incidence will
#' only be estimated for those intervals where the denominator cohort
#' captures all the interval.
#' @param outcomeWashout The number of days used for a 'washout' period
#' between the end of one outcome and an individual starting to contribute
#' time at risk. If Inf, no time can be contributed after an event has
#' occurred (whether during the study period or if occurring beforehand).
#' @param repeatedEvents TRUE/ FALSE. If TRUE, an individual will be able to
#' contribute multiple events during the study period (time while they are
#' present in an outcome cohort and any subsequent washout will be
#' excluded). If FALSE, an individual will only contribute time up to their
#' first event during the study period.
#' @param strata Variables added to the denominator cohort table for which to
#' stratify estimates.
#' @param includeOverallStrata Whether to include an overall result as well as
#' strata specific results (when strata has been specified).
#' @param minCellCount The minimum number of events to reported, below which
#' results will be obscured. If 0, all results will be reported.
#' @param temporary If TRUE, temporary tables will be used throughout. If
#' FALSE, permanent tables will be created in the write_schema of the cdm.
#' @param returnParticipants Either TRUE or FALSE. If TRUE references to
#' participants from the analysis will be returned allowing for further
#' analysis. Note, if using permanent tables and returnParticipants is TRUE,
#' one table per analysis will be kept in the cdm write schema.
#'
#' @return Incidence estimates
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
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
                              interval = "years",
                              completeDatabaseIntervals = TRUE,
                              outcomeWashout = Inf,
                              repeatedEvents = FALSE,
                              minCellCount = 5,
                              strata = list(),
                              includeOverallStrata = TRUE,
                              temporary = TRUE,
                              returnParticipants = FALSE) {
  startCollect <- Sys.time()

  tablePrefix <- paste0(
    sample(letters, 5, TRUE) |> paste0(collapse = ""), "_inc"
  )

  # help to avoid formatting errors
  if (is.character(interval)) {
    interval <- tolower(interval)
  }

  checkInputEstimateIncidence(
    cdm, denominatorTable, outcomeTable, denominatorCohortId,
    outcomeCohortId, interval, completeDatabaseIntervals,
    outcomeWashout, repeatedEvents, minCellCount, temporary,
    returnParticipants
  )

  checkStrata(strata, cdm[[denominatorTable]])


  # if not given, use all denominator and outcome cohorts
  if (is.null(denominatorCohortId)) {
    denominatorCohortId <- CDMConnector::cohortCount(
      cdm[[denominatorTable]]) %>%
      dplyr::filter(.data$number_records > 0) %>%
      dplyr::pull("cohort_definition_id")
  }
  if (is.null(outcomeCohortId)) {
    outcomeCohortId <- CDMConnector::cohortCount(cdm[[outcomeTable]]) %>%
      dplyr::pull("cohort_definition_id")
  }

  if(denominatorTable == outcomeTable &&
     any(denominatorCohortId %in% outcomeCohortId)){
    cli::cli_abort("Denominator cohort can not be the same as the outcome cohort")
  }

  ## add outcome from attribute
  outcomeRef <- CDMConnector::cohortSet(cdm[[outcomeTable]]) %>%
    dplyr::filter(.env$outcomeCohortId %in% .data$cohort_definition_id) %>%
    dplyr::collect("cohort_definition_id", "cohort_name") %>%
    dplyr::rename("outcome_cohort_id" = "cohort_definition_id",
                  "outcome_cohort_name" = "cohort_name")
  if(nrow(outcomeRef) == 0){
    cli::cli_abort(c("Specified outcome IDs not found in the cohort set of
                    {paste0('cdm$', outcomeTable)}",
                   "i" = "Run CDMConnector::cohort_set({paste0('cdm$', outcomeTable)})
                   to check which IDs exist"))
  }


  # further checks that there are the required data elements
  checkInputEstimateIncidenceAdditional(
    cdm, denominatorTable, outcomeTable, denominatorCohortId,
    outcomeCohortId
  )

  # get outcomes + cohort_start_date & cohort_end_date
  outcome <- cdm[[outcomeTable]] %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$outcomeCohortId) %>%
    dplyr::rename(
      "outcome_cohort_id" = "cohort_definition_id",
      "outcome_start_date" = "cohort_start_date",
      "outcome_end_date" = "cohort_end_date"
    ) %>%
    dplyr::inner_join(
      cdm[[denominatorTable]] %>%
        dplyr::filter(.data$cohort_definition_id %in%
          .env$denominatorCohortId) %>%
        dplyr::select(c("subject_id", "cohort_start_date",
                      "cohort_end_date")) %>%
        dplyr::distinct(),
      by = "subject_id"
    )

  outcome <- outcome %>%
    CDMConnector::computeQuery(
      name = paste0(tablePrefix, "_inc_1"),
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )

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
    )

  outcome <- outcome %>%
    CDMConnector::computeQuery(
      name = paste0(tablePrefix, "_inc_2"),
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )


  outcome <- outcome %>%
    dplyr::group_by(
      .data$subject_id,
      .data$cohort_start_date,
      .data$outcome_cohort_id
    ) %>%
    dbplyr::window_order(.data$outcome_start_date) %>%
    dplyr::mutate(index = rank()) %>%
    dplyr::ungroup()

  outcome <- outcome %>%
    CDMConnector::computeQuery(
      name = paste0(tablePrefix, "_inc_3"),
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )

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
    dplyr::select(-"index")

  outcome <- outcome %>%
    CDMConnector::computeQuery(
      name = paste0(tablePrefix, "_inc_4"),
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )

  studySpecs <- tidyr::expand_grid(
    outcome_cohort_id = outcomeCohortId,
    denominator_cohort_id = denominatorCohortId,
    interval = interval,
    complete_database_intervals = completeDatabaseIntervals,
    outcome_washout = outcomeWashout,
    repeated_events = repeatedEvents
  )
  if (any(is.infinite(outcomeWashout))) {
    studySpecs$outcome_washout[
      which(is.infinite(studySpecs$outcome_washout))
    ] <- NA
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
    counter <<- counter + 1
    message(glue::glue(
      "Getting incidence for analysis {counter} of {length(studySpecs)}"
    ))


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
      tablePrefix = tablePrefix,
      returnParticipants = returnParticipants,
      analysisId = x$analysis_id,
      strata = strata,
      includeOverallStrata = includeOverallStrata
    )

    workingIncIr <- workingInc[["ir"]] %>%
      dplyr::mutate(analysis_id = x$analysis_id) %>%
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
    result[["attrition"]] <- workingIncAttrition
    if (returnParticipants == TRUE) {
      result[[paste0(
        "study_population_analyis_",
        x$analysis_id
      )]] <- workingInc[["person_table"]]
    }

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
      CDMConnector::cohortSet(cdm[[denominatorTable]]) %>%
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
      CDMConnector::cohortAttrition(cdm[[denominatorTable]]) %>%
        dplyr::rename("denominator_cohort_id" = "cohort_definition_id") %>%
        dplyr::filter(.data$denominator_cohort_id ==
          studySpecs[[i]]$denominator_cohort_id) %>%
        dplyr::mutate(analysis_id = studySpecs[[i]]$analysis_id) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.integer)),
      irsList[names(irsList) == "attrition"][[i]] %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.integer))
    )
  }
  attrition <- irsList[names(irsList) == "attrition"]
  attrition <- dplyr::bind_rows(attrition,
    .id = NULL
  ) %>%
    dplyr::select(-"denominator_cohort_id") %>%
    dplyr::relocate("analysis_id")


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
  if (returnParticipants == TRUE) {
    participantTables <- unname(purrr::as_vector(irsList[stringr::str_detect(
      names(irsList),
      "study_population"
    )]))
    # combine to a single participants
    # from 1st analysis
    participants <- dplyr::tbl(
      attr(cdm, "dbcon"),
      CDMConnector::inSchema(
        attr(cdm, "write_schema"),
        participantTables[[1]]
      )
    )


    if (length(participantTables) >= 2) {
      # join additional analyses
      participantTables <- participantTables[2:length(participantTables)]
      for (i in seq_along(participantTables)) {
        participants <- participants %>%
          dplyr::full_join(
            dplyr::tbl(
              attr(cdm, "dbcon"),
              CDMConnector::inSchema(
                attr(cdm, "write_schema"),
                participantTables[[i]]
              )
            ),
            by = "subject_id"
          ) %>%
          CDMConnector::computeQuery(
            name = paste0(tablePrefix, "_p_", i),
            temporary = FALSE,
            schema = attr(cdm, "write_schema"),
            overwrite = TRUE
          )
      }
    }

    # make sure to not overwrite any existing participant table (from
    # previous function calls)
    p <- 1 + length(stringr::str_subset(
      CDMConnector::listTables(attr(cdm, "dbcon"),
        schema = attr(cdm, "write_schema")
      ),
      "inc_participants"
    ))

    participants <- participants %>%
      CDMConnector::computeQuery(
        name = paste0("inc_participants", p),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )
    CDMConnector::dropTable(
      cdm = cdm,
      name = tidyselect::starts_with(paste0(tablePrefix, "_p_"))
    )
  }

  CDMConnector::dropTable(
    cdm = cdm,
    name = tidyselect::starts_with(paste0(tablePrefix, "_inc_"))
  )
  CDMConnector::dropTable(
    cdm = cdm,
    name = tidyselect::starts_with(paste0(tablePrefix, "_analysis_"))
  )

  analysisSettings <- analysisSettings %>%
    dplyr::left_join(outcomeRef, by = "outcome_cohort_id") %>%
    dplyr::relocate("outcome_cohort_id", .after = "analysis_id") %>%
    dplyr::relocate("outcome_cohort_name", .after = "outcome_cohort_id") %>%
    dplyr::mutate(cdm_name = attr(cdm, "cdm_name"))

  # add settings to estimates and attrition
  if (nrow(irs) >= 1) {
    irs <- irs %>%
      dplyr::left_join(analysisSettings, by = "analysis_id")
  }
  attrition <- attrition %>%
    dplyr::left_join(analysisSettings, by = "analysis_id")
  attrition <- obscureAttrition(attrition,
                                minCellCount = minCellCount
  )

  # return results as an IncidencePrevalenceResult class
  attr(irs, "attrition") <- attrition
  if (returnParticipants == TRUE) {
    attr(irs, "participants") <- participants
  }
  class(irs) <- c("IncidencePrevalenceResult", "IncidenceResult", class(irs))

  dur <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
  message(glue::glue(
    "Overall time taken: {floor(dur/60)} mins and {dur %% 60 %/% 1} secs"
  ))

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
