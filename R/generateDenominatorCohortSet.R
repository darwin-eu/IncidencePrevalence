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


#' Identify a set of denominator populations
#'
#' @description
#' `generateDenominatorCohortSet()` creates a set of cohorts that
#' can be used for the denominator population in analyses of incidence,
#' using `estimateIncidence()`, or prevalence, using `estimatePointPrevalence()`
#' or `estimatePeriodPrevalence()`.
#'
#' @param cdm A CDM reference object
#' @param startDate A date indicating the start of the study
#' period. If NULL, the earliest observation_start_date in the
#' observation_period table will be used.
#' @param endDate A date indicating the end of the study
#' period. If NULL, the latest observation end date in the observation period
#' table will be used.
#' @param ageGroup A list of age groups for which cohorts will be generated. A
#' value of `list(c(0,17), c(18,30))` would, for example, lead to the creation
#' of cohorts for those aged from 0 to 17 (up to the day before their 18th
#' birthday), and from 18 (starting the day of their 18th birthday) to 30 (up
#' to the day before their 31st birthday).
#' @param sex Sex of the cohorts. This can be one or more of: `"Male"`,
#' `"Female"`, or `"Both"`.
#' @param daysPriorHistory The number of days of prior history observed in
#' the database required for an individual to start contributing time in
#' a cohort.
#' @param strataTable A cohort table in the cdm reference to use
#' to limit cohort entry and exit (with individuals only contributing to a
#' cohort when they are contributing to the cohort in the strata table).
#' @param strataCohortId The cohort definition id for the cohort of interest
#'  in the strata table. Only one stratifying cohort is supported.
#' @param strataCohortName Corresponding name for the strata cohort.
#' @param sample An integer for which to take a random sample, using
#' `dplyr::slice_sample()`, of the people in the person table eligible to be
#' included in the cohort set.
#' @param tablePrefix The stem for the permanent tables that will
#' be created when creating the denominator cohorts. Permanent tables will be
#' created using this stem, and any existing tables that start with this
#' will be at risk of being dropped or overwritten. If NULL, temporary
#' tables will be used throughout.
#' @param verbose Either TRUE or FALSE. If TRUE, progress will be reported.
#'
#' @return A cohort reference
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
#' cdm$denominator <- generateDenominatorCohortSet(
#'   cdm = cdm,
#'   startDate = as.Date("2008-01-01"),
#'   endDate = as.Date("2018-01-01")
#' )
#' }
generateDenominatorCohortSet <- function(cdm,
                                         startDate = NULL,
                                         endDate = NULL,
                                         ageGroup = list(c(0, 150)),
                                         sex = "Both",
                                         daysPriorHistory = 0,
                                         strataTable = NULL,
                                         strataCohortId = NULL,
                                         strataCohortName = NULL,
                                         sample = NULL,
                                         tablePrefix = NULL,
                                         verbose = FALSE) {
  if (verbose == TRUE) {
    startCollect <- Sys.time()
  }

  checkInputGenerateDCS(
    cdm, startDate, endDate,
    ageGroup, sex, daysPriorHistory,
    strataTable, strataCohortId, strataCohortName,
    sample, tablePrefix, verbose
  )
  if (!is.null(tablePrefix)) {
    # drop table stem if exists
    CDMConnector::dropTable(cdm = cdm,
                            name = tidyselect::starts_with(tablePrefix))
  }

  # add broadest possible age group if no age strata were given
  if (is.null(startDate)) {
    startDate <- cdm$observation_period %>%
      dplyr::summarise(
        min(.data$observation_period_start_date,
          na.rm = TRUE
        )
      ) %>%
      dplyr::collect() %>%
      dplyr::pull()
  }
  if (is.null(endDate)) {
    endDate <- cdm$observation_period %>%
      dplyr::summarise(
        max(.data$observation_period_end_date,
          na.rm = TRUE
        )
      ) %>%
      dplyr::collect() %>%
      dplyr::pull()
  }

  # summarise combinations of inputs
  ageGrDf <- data.frame(do.call(rbind, ageGroup)) %>%
    dplyr::mutate(age_group = paste0(.data$X1, ";", .data$X2))
  popSpecs <- tidyr::expand_grid(
    age_group = ageGrDf$age_group,
    sex = .env$sex,
    days_prior_history = .env$daysPriorHistory,
    start_date = .env$startDate,
    end_date = .env$endDate
  )
  popSpecs <- popSpecs %>%
    tidyr::separate(.data$age_group,
      c("min_age", "max_age"),
      remove = FALSE
    ) %>%
    dplyr::mutate(min_age = as.numeric(.data$min_age)) %>%
    dplyr::mutate(max_age = as.numeric(.data$max_age)) %>%
    dplyr::mutate(cohort_definition_id = dplyr::row_number())

  # get the overall contributing population (without stratification)
  # we need to the output the corresponding dates when getting the denominator
  if (verbose == TRUE) {
    message("Getting overall denominator population")
  }

  dpop <- getDenominatorCohorts(
    cdm = cdm,
    startDate = unique(popSpecs$start_date),
    endDate = unique(popSpecs$end_date),
    minAge = unique(popSpecs$min_age),
    maxAge = unique(popSpecs$max_age),
    daysPriorHistory = unique(popSpecs$days_prior_history),
    strataTable = strataTable,
    strataCohortId = strataCohortId,
    sample = sample,
    tablePrefix
  )

  denominatorPopulationNrows <- dpop$denominator_population %>%
    dplyr::count() %>%
    dplyr::pull()

  # build each of the cohorts of interest
  if (verbose == TRUE) {
    message("Creating each denominator cohort of interest")
  }

  if (denominatorPopulationNrows == 0) {
    message("- No people found for any denominator population")
    studyPops <- dpop$denominator_population

    # attrition is the same for each group
    dpop$attrition <- Map(cbind,
      lapply(
        popSpecs$cohort_definition_id,
        function(x) dpop$attrition
      ),
      cohort_definition_id =
        length(popSpecs$cohort_definition_id)
    )

    cohortCount <- tibble::tibble(
      cohort_definition_id = popSpecs$cohort_definition_id,
      number_records = 0,
      number_subjects = 0
    )
  } else {
    # first, if all cohorts are Male or Female get number that will be excluded
    nFemale <- dpop$denominator_population %>%
      dplyr::filter(.data$sex == "Female") %>%
      dplyr::select("person_id") %>%
      dplyr::distinct() %>%
      dplyr::tally() %>%
      dplyr::pull()
    nMale <- dpop$denominator_population %>%
      dplyr::filter(.data$sex == "Male") %>%
      dplyr::select("person_id") %>%
      dplyr::distinct() %>%
      dplyr::tally() %>%
      dplyr::pull()

    # attrition so far was the same for each group
    # now we´ll make one for attrition record for each cohort
    dpop$attrition <- Map(cbind,
      lapply(
        popSpecs$cohort_definition_id,
        function(x) dpop$attrition
      ),
      cohort_definition_id =
        length(popSpecs$cohort_definition_id)
    )

    # count dropped for sex criteria
    for (i in seq_along(dpop$attrition)) {

      if (popSpecs$sex[[i]] == "Male") {
        dpop$attrition[[i]] <- recordAttrition(
          table = dpop$denominator_population %>%
            dplyr::filter(sex=="Male"),
          id = "person_id",
          reasonId = 8,
          reason = "Not Male",
          existingAttrition = dpop$attrition[[i]]
        )
      }
      if (popSpecs$sex[[i]] == "Female") {
        dpop$attrition[[i]] <- recordAttrition(
          table = dpop$denominator_population %>%
            dplyr::filter(sex=="Female"),
          id = "person_id",
          reasonId = 8,
          reason = "Not Female",
          existingAttrition = dpop$attrition[[i]]
        )
      }
    }

    studyPops <- list()
    cohortCount <- list()
    cli::cli_progress_bar(
      total = length(popSpecs$cohort_definition_id),
      format = " -- getting cohort dates for {cli::pb_bar} {cli::pb_current} of {cli::pb_total} cohorts"
    )
    for (i in seq_along(popSpecs$cohort_definition_id)) {
      if (verbose == TRUE) {
        cli::cli_progress_update()
      }
      workingDpop <- dpop$denominator_population

      if (popSpecs$sex[[i]] %in% c("Male", "Female")) {
        workingDpop <- workingDpop %>%
          dplyr::filter(.data$sex == local(popSpecs$sex[[i]]))
      }

      workingDpop <- workingDpop %>%
        dplyr::rename(
          # cohort start
          "cohort_start_date" =
            glue::glue("date_min_age{popSpecs$min_age[[i]]}prior_history{popSpecs$days_prior_history[[i]]}"),
          # cohort end
          "cohort_end_date" =
            glue::glue(
              "date_max_age{popSpecs$max_age[[i]]}"
            ),
          "subject_id" = "person_id"
        ) %>%
        dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") %>%
        dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)

      dpop$attrition[[i]] <- recordAttrition(
        table = workingDpop,
        id = "subject_id",
        reasonId = 10,
        reason = glue::glue("No observation time available after applying age and prior history criteria"),
        existingAttrition = dpop$attrition[[i]]
      )

      dpop$attrition[[i]]$cohort_definition_id <- popSpecs$cohort_definition_id[[i]]
      workingCount <- utils::tail(dpop$attrition[[i]]$number_records, 1)
      cohortCount[[i]] <- workingDpop %>%
        dplyr::summarise(number_records = dplyr::n(),
                         number_subjects = dplyr::n_distinct(.data$subject_id)) %>%
        dplyr::collect() %>%
        dplyr::mutate(
          cohort_definition_id = popSpecs$cohort_definition_id[[i]]
        ) %>%
        dplyr::relocate("cohort_definition_id")

      if (workingCount > 0) {
        studyPops[[paste0("cohort_definition_id_", i)]] <- workingDpop %>%
          dplyr::mutate(
            cohort_definition_id =
              local(popSpecs$cohort_definition_id[[i]])
          ) %>%
          dplyr::relocate("cohort_definition_id")
      }
    }
    cli::cli_progress_done()

    studyPops <- unionCohorts(cdm,
                              studyPops,
                              tablePrefix,
                              verbose)

  }


  if (!is.null(tablePrefix)) {
    # drop the intermediate tables that may have been created
    CDMConnector::dropTable(cdm = cdm,
                            name = tablePrefix)
    CDMConnector::dropTable(cdm = cdm,
                            name = tidyselect::starts_with(paste0(tablePrefix, "_i_")))
    if(!is.null(sample)){
    CDMConnector::dropTable(cdm = cdm,
                            name = paste0(
                              tablePrefix,
                              "_person_sample"
                            ))
    }
  }

  if (verbose == TRUE) {
    dur <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
    message(glue::glue(
      "Time taken to get denominator cohorts: {floor(dur/60)} min and {dur %% 60 %/% 1} sec"
    ))
  }

  # add strata info to settings
  if (is.null(strataCohortId)) {
    strataCohortId <- NA
  }
  if (is.null(strataCohortName)) {
    strataCohortName <- NA
  }

  # return results as a cohort_reference class
  cohortCount <- dplyr::bind_rows(cohortCount)
  attr(studyPops, "cohort_count") <- cohortCount

  cohortSet <- popSpecs %>%
    dplyr::mutate(strata_cohort_definition_id = .env$strataCohortId) %>%
    dplyr::mutate(strata_cohort_name = .env$strataCohortName) %>%
    dplyr::mutate(cohort_name = paste0("Denominator cohort ", .data$cohort_definition_id)) %>%
    dplyr::select(!c("min_age", "max_age")) %>%
    dplyr::relocate("cohort_definition_id")%>%
    dplyr::relocate("cohort_name", .after = "cohort_definition_id")
  attr(studyPops, "cohort_set") <- cohortSet

  cohortAttrition <- dplyr::bind_rows(dpop$attrition) %>%
    dplyr::as_tibble() %>%
    dplyr::relocate("cohort_definition_id")
  attr(studyPops, "cohort_attrition") <- cohortAttrition

  if(!is.null(tablePrefix)){
  insertAttributes(cdm = cdm,
                   tablePrefix = tablePrefix,
                   cohortCount = cohortCount,
                   cohortSet = cohortSet,
                   cohortAttrition = cohortAttrition)
  }

  class(studyPops) <- c("IncidencePrevalenceDenominator", "GeneratedCohortSet",
                        class(studyPops))

  return(studyPops)
}



# union cohort tables
# combine the set of separate cohort tables into a single table
unionCohorts <- function(cdm,
                         studyPops,
                         tablePrefix,
                         verbose
                         ){

  if (length(studyPops) != 0 && length(studyPops) < 10) {
    if (verbose == TRUE) {
      message(glue::glue("Unioning cohorts"))
    }
    studyPops <- Reduce(dplyr::union_all, studyPops)
    if (is.null(tablePrefix)) {
      studyPops <- studyPops %>%
        CDMConnector::computeQuery()
    } else {
      studyPops <- studyPops %>%
        CDMConnector::computeQuery(
          name = paste0(
            tablePrefix,
            "_denominator"
          ),
          temporary = FALSE,
          schema = attr(cdm, "write_schema"),
          overwrite = TRUE
        )
    }
  }
  if (length(studyPops) >= 10) {
    # if 10 or more
    # combine in batches in case of many subgroups
    studyPopsBatches <- split(
      studyPops,
      ceiling(seq_along(studyPops) / 10) # 10 in a batch
    )
    cli::cli_progress_bar(
      total = length(studyPopsBatches),
      format = " -- unioning {cli::pb_bar} {cli::pb_current} of {cli::pb_total} batched cohorts"
    )

    for (i in seq_along(studyPopsBatches)) {
      if (verbose == TRUE) {
        cli::cli_progress_update()
      }
      studyPopsBatches[[i]] <- Reduce(
        dplyr::union_all,
        studyPopsBatches[[i]]
      )

      if (is.null(tablePrefix)) {
        studyPopsBatches[[i]] <- studyPopsBatches[[i]] %>%
          CDMConnector::computeQuery()
      } else {
        studyPopsBatches[[i]] <- studyPopsBatches[[i]] %>%
          CDMConnector::computeQuery(
            name = paste0(
              tablePrefix,
              "_batch_", i
            ),
            temporary = FALSE,
            schema = attr(cdm, "write_schema"),
            overwrite = TRUE
          )
      }
    }
    cli::cli_progress_done()

    studyPops <- Reduce(dplyr::union_all, studyPopsBatches)
    if (is.null(tablePrefix)) {
      studyPops <- studyPops %>%
        CDMConnector::computeQuery()
    } else {
      studyPops <- studyPops %>%
        CDMConnector::computeQuery(
          name = paste0(
            tablePrefix,
            "_denominator"
          ),
          temporary = FALSE,
          schema = attr(cdm, "write_schema"),
          overwrite = TRUE
        )
    }

    # drop any batch permanent tables
    if (!is.null(tablePrefix)) {
      CDMConnector::dropTable(cdm = cdm,
                              name = tidyselect::starts_with(paste0(tablePrefix, "_batch_")))
    }
  }

  return(studyPops)


}


# insert attributes into database
insertAttributes <- function(cdm, tablePrefix,
                             cohortCount, cohortSet, cohortAttrition){
  DBI::dbWriteTable(attr(cdm, "dbcon"),
                    name = inSchema(attr(cdm, "write_schema"), paste0(tablePrefix, "_denominator_count")),
                    value = as.data.frame(cohortCount),
                    overwrite = TRUE)
  DBI::dbWriteTable(attr(cdm, "dbcon"),
                    name = inSchema(attr(cdm, "write_schema"), paste0(tablePrefix, "_denominator_set")),
                    value = as.data.frame(cohortSet),
                    overwrite = TRUE)
  DBI::dbWriteTable(attr(cdm, "dbcon"),
                    name = inSchema(attr(cdm, "write_schema"), paste0(tablePrefix, "_denominator_attrition")),
                    value = as.data.frame(cohortAttrition),
                    overwrite = TRUE)
}



