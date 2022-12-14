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
#' @param ageGroups A list of age groups for which cohorts will be generated. A
#' value of `list(c(0,17), c(18,30))` would, for example, lead to the creation of
#' cohorts for those aged from 0 to 17 (up to the day before their 18th
#' birthday), and from 18 (starting the day of their 18th birthday) to 30 (up
#' to the day before their 31st birthday).
#' @param sex Sex of the cohorts. This can be one or more of: `"Male"`,
#' `"Female"`, or `"Both"`.
#' @param daysPriorHistory The number of days of prior history observed in
#' the database required for an individual to start contributing time in
#' a cohort.
#' @param strataTable A cohort table in the cdm reference to use
#' to limit cohort entry and exit (with individuals only contributing to a
#' cohort when they are contibuting to the cohort in the stata table).
#' @param strataCohortId The cohort definition id for the cohort of interest
#'  in the strata table. Only one stratifying cohort is supported.
#' @param sample An integer for which to take a random sample, using
#' `dplyr::slice_sample()`, of the people in the person table eligible to be
#' included in the cohort set.
#' @param verbose Either TRUE or FALSE. If TRUE, progress will be reported.
#'
#' @return A cohort reference
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomia_dir())
#' cdm <- CDMConnector::cdm_from_con(
#'   con = con,
#'   cdm_schema = "main"
#' )
#'
#' dpop <- generateDenominatorCohortSet(
#'   cdm = cdm,
#'   startDate = as.Date("2008-01-01"),
#'   endDate = as.Date("2018-01-01")
#' )
#' }
generateDenominatorCohortSet <- function(cdm,
                               startDate = NULL,
                               endDate = NULL,
                               ageGroups = list(c(0, 150)),
                               sex = "Both",
                               daysPriorHistory = 0,
                               strataTable = NULL,
                               strataCohortId = NULL,
                               sample = NULL,
                               verbose = FALSE) {
  if (verbose == TRUE) {
    startCollect <- Sys.time()
    message("Progress: Checking inputs")
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
  cdmPersonCheck <- inherits(cdm$person, "tbl_dbi")
  checkmate::assertTRUE(cdmPersonCheck, add = errorMessage)
  if (!isTRUE(cdmPersonCheck)) {
    errorMessage$push(
      "- table `person` is not found"
    )
  }
  cdmObsPeriodCheck <- inherits(cdm$observation_period, "tbl_dbi")
  checkmate::assertTRUE(cdmObsPeriodCheck, add = errorMessage)
  if (!isTRUE(cdmObsPeriodCheck)) {
    errorMessage$push(
      "- table `observation_period` is not found"
    )
  }
  checkmate::assert_date(startDate,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assert_date(endDate,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assert_list(ageGroups,
    add = errorMessage
  )
  if (!is.null(ageGroups)) {
    for (i in seq_along(ageGroups)) {
      checkmate::assertTRUE(length(ageGroups[[i]]) == 2)
      checkmate::assert_numeric(ageGroups[[i]][1],
        add = errorMessage
      )
      checkmate::assert_numeric(ageGroups[[i]][2],
        add = errorMessage
      )
      ageCheck <- ageGroups[[i]][1] <
        ageGroups[[i]][2]
      checkmate::assertTRUE(ageCheck,
        add = errorMessage
      )
      if (!isTRUE(ageCheck)) {
        errorMessage$push(
          "- upper age value must be higher than lower age value"
        )
      }
      checkmate::assertTRUE(ageGroups[[i]][1] >= 0,
        add = errorMessage
      )
      checkmate::assertTRUE(ageGroups[[i]][2] >= 0,
        add = errorMessage
      )
    }
  }
  checkmate::assert_vector(sex,
    add = errorMessage
  )
  sexCheck <- all(sex %in% c("Male", "Female", "Both"))
  if (!isTRUE(sexCheck)) {
    errorMessage$push(
      "- sex stratas must be from: Male, Female, and Both"
    )
  }
  checkmate::assert_numeric(daysPriorHistory,
    add = errorMessage
  )
  daysCheck <- all(daysPriorHistory >= 0)
  if (!isTRUE(daysCheck)) {
    errorMessage$push(
      "- daysPriorHistory cannot be negative"
    )
  }
  if (!is.null(strataTable)) {
    strataTableCheck <- inherits(cdm[[strataTable]], "tbl_dbi")
    checkmate::assertTRUE(strataTableCheck, add = errorMessage)
    if (!isTRUE(strataTableCheck)) {
      errorMessage$push(
        "- table `strata` is not found"
      )
    }
    strataNamesCheck <- all(names(cdm[[strataTable]] %>%
      utils::head(1) %>%
        dplyr::collect()) %in%
      c(
        "cohort_definition_id", "subject_id",
        "cohort_start_date", "cohort_end_date"
      ))
    checkmate::assertTRUE(strataNamesCheck, add = errorMessage)
    if (!isTRUE(strataNamesCheck)) {
      errorMessage$push(
        "- table `strata` does not conform to specification"
      )
    }
  }
  checkmate::assertNumeric(sample,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assert_logical(verbose,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)
  if (verbose == TRUE) {
    message("Progress: All input checks passed")
    duration <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
    message(glue::glue(
  "Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }

  # add broadest possible age group if no age strata were given
  if (verbose == TRUE) {
    message("Progress: Prepare inputs for creating denominator populations")
    start <- Sys.time()
  }
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
  ageGrDf <- data.frame(do.call(rbind, ageGroups)) %>%
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


  if (verbose == TRUE) {
    message("Progress: Inputs prepared for creating denominator populations")
    duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
    message(glue::glue(
  "Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }

  # get the overall contributing population (without stratification)
  # we need to the output the corresponding dates when getting the denominator
  if (verbose == TRUE) {
    message("Progress: Run get_denominator_pop to get overall population")
    start <- Sys.time()
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
    sample = sample
  )

  if (verbose == TRUE) {
    message("Progress: Overall denominator population identified")
    duration <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
    message(glue::glue(
      "Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }

  sqlQueries <- dpop$sql_queries
  denominator_population_nrows <- dpop$denominator_population %>%
    dplyr::count() %>%
    dplyr::pull(.data$n)

  # build each of the cohorts of interest
  if (verbose == TRUE) {
    message("Progress: Create each denominator population of interest")
    start <- Sys.time()
  }

  if (denominator_population_nrows == 0) {
    message("- No people found for any denominator population")
    studyPops <- dpop$denominator_population

    # attrition is the same for each group
    dpop$attrition <- cbind(dpop$attrition,
          cohort_definition_id = rep(popSpecs$cohort_definition_id,
                                     each = nrow(dpop$attrition))) %>%
      dplyr::group_split(.data$cohort_definition_id)

  } else if (denominator_population_nrows > 0) {
    # first, if all cohorts are Male or Female get number that will be excluded
    if (all(popSpecs$sex == "Female")) {
      dpop$attrition <- recordAttrition(
        table = dpop$denominator_population %>%
          dplyr::filter(.data$sex == "Female"),
        id = "person_id",
        reason = "Not Female",
        existingAttrition = dpop$attrition
      )
    }
    if (all(popSpecs$sex == "Male")) {
      dpop$attrition <- recordAttrition(
        table = dpop$denominator_population %>%
          dplyr::filter(.data$sex == "Male"),
        id = "person_id",
        reason = "Not Male",
        existingAttrition = dpop$attrition
      )
    }

    # attrition so far is the same for each group
    dpop$attrition <- cbind(dpop$attrition,
          cohort_definition_id = rep(popSpecs$cohort_definition_id,
                                     each = nrow(dpop$attrition))) %>%
         dplyr::group_split(.data$cohort_definition_id)

    studyPops <- list()

    for (i in seq_along(popSpecs$cohort_definition_id)) {
      workingDpop <- dpop$denominator_population

      if (popSpecs$sex[[i]] %in% c("Male", "Female")) {
        workingDpop <- workingDpop %>%
          dplyr::filter(.data$sex == local(popSpecs$sex[[i]]))
        dpop$attrition[[i]] <- recordAttrition(
          table = workingDpop,
          id = "person_id",
          reason = glue::glue("Not {popSpecs$sex[[i]]}"),
          existingAttrition = dpop$attrition[[i]]
        )
      }

      # cohort start
      workingDpop <- workingDpop %>%
        dplyr::rename(
          "cohort_start_date" =
            glue::glue("date_min_age{popSpecs$min_age[[i]]}prior_history{popSpecs$days_prior_history[[i]]}")
        )
      # cohort end
      workingDpop <- workingDpop %>%
        dplyr::rename(
          cohort_end_date =
            glue::glue(
              "date_max_age{popSpecs$max_age[[i]]}"
            )
        )

      workingDpop <- workingDpop %>%
        dplyr::rename("subject_id" = "person_id") %>%
        dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") %>%
        dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)

      dpop$attrition[[i]] <- recordAttrition(
        table = workingDpop,
        id = "subject_id",
        reason = glue::glue("No observation time available after applying age and prior history criteria"),
        existingAttrition = dpop$attrition[[i]]
      )
      dpop$attrition[[i]]$cohort_definition_id <- popSpecs$cohort_definition_id[[i]]


      studyPops[[i]] <- workingDpop %>%
        dplyr::mutate(cohort_definition_id =
                        local(popSpecs$cohort_definition_id[[i]])) %>%
        dplyr::relocate("cohort_definition_id")
    }


    if (length(studyPops) > 20) {
      # combine in batches in case of many subgroups
      nBatches <- 20 # number in a batch
      studyPopsBatches <- split(
        studyPops,
        ceiling(seq_along(studyPops) / nBatches)
      )
      for (i in seq_along(studyPopsBatches)) {
        studyPopsBatches[[i]] <- Reduce(dplyr::union_all, studyPopsBatches[[i]])
        sqlQueries[[paste0("combine_cohorts_batch_", i)]] <-
          studyPopsBatches[[i]] %>%
          extractQuery(description = paste0("combine_cohorts_batch_", i))
        studyPopsBatches[[i]] <- studyPopsBatches[[i]] %>%
          dplyr::compute()
      }
      studyPops <- Reduce(dplyr::union_all, studyPopsBatches)
      sqlQueries[["combine_cohorts_batch"]] <- studyPops %>%
        extractQuery(description = "combine_cohorts_batch")
      studyPops <- studyPops %>% dplyr::compute()
    } else {
      # otherwise combine all at once
      studyPops <- Reduce(dplyr::union_all, studyPops)
      sqlQueries[["combine_cohorts"]] <- studyPops %>%
        extractQuery(description = "combine_cohorts")
      studyPops <- studyPops %>% dplyr::compute()
    }
  }

  if (verbose == TRUE) {
    message("Progress: Each denominator population of interest created")
    duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
    message(glue::glue(
      "Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }

  if (verbose == TRUE) {
    duration <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
    message(glue::glue(
      "Overall time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }
  # return results as a cohort_reference class

  attr(studyPops, "settings") <- popSpecs %>%
    dplyr::select(!c("min_age", "max_age"))
  attr(studyPops, "attrition") <- dplyr::bind_rows(dpop$attrition) %>%
    dplyr::mutate(step = "Generating denominator cohort set")
  sqlQueries <- unlist(sqlQueries)
  class(sqlQueries) <- c("sqlTrace", class(sqlQueries))
  attr(studyPops, "sql") <- sqlQueries
  attr(studyPops, "nrow") <- denominator_population_nrows

  class(studyPops) <- c("IncidencePrevalenceDenominator", "cohort_reference", class(studyPops))

  return(studyPops)
}
