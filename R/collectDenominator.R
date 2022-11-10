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
#' `collectDenominator()` creates a set of denominator cohorts
#'
#' @param cdm CDMConnector CDM reference object
#' @param startDate Date indicating the start of the study
#' period. If NULL,
#'  the earliest observation_start_date in the observation_period table
#'  will be used.
#' @param endDate Date indicating the end of the study
#' period. If NULL,
#'  the latest observation_end_date in the observation_period table
#'  will be used.
#' @param ageStrata List of age groups
#' @param sexStrata Sex of the cohorts
#' @param daysPriorHistory Days of prior history required to enter
#' the study cohort.
#' @param strataTable strataTable
#' @param strataId strata cohort definition id
#' @param sample sample n
#' @param verbose Either TRUE or FALSE.
#' If TRUE, progress will be reported.
#'
#' @return
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here ")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' dpop <- collectDenominator(
#'   cdm = cdm,
#'   startDate = as.Date("2008-01-01"),
#'   endDate = as.Date("2018-01-01")
#' )
#' }

collectDenominator <- function(cdm,
                               startDate = NULL,
                               endDate = NULL,
                               ageStrata = list(c(0, 150)),
                               sexStrata = "Both",
                               daysPriorHistory = 0,
                               strataTable = NULL,
                               strataId = NULL,
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
  checkmate::assert_list(ageStrata,
    add = errorMessage
  )
  if (!is.null(ageStrata)) {
    for (i in seq_along(ageStrata)) {
      checkmate::assertTRUE(length(ageStrata[[i]]) == 2)
      checkmate::assert_numeric(ageStrata[[i]][1],
        add = errorMessage
      )
      checkmate::assert_numeric(ageStrata[[i]][2],
        add = errorMessage
      )
      ageCheck <- ageStrata[[i]][1] <
        ageStrata[[i]][2]
      checkmate::assertTRUE(ageCheck,
        add = errorMessage
      )
      if (!isTRUE(ageCheck)) {
        errorMessage$push(
          "- upper age value must be higher than lower age value"
        )
      }
      checkmate::assertTRUE(ageStrata[[i]][1] >= 0,
        add = errorMessage
      )
      checkmate::assertTRUE(ageStrata[[i]][2] >= 0,
        add = errorMessage
      )
    }
  }
  checkmate::assert_vector(sexStrata,
    add = errorMessage
  )
  sexCheck <- all(sexStrata %in% c("Male", "Female", "Both"))
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
  ageGrDf <- data.frame(do.call(rbind, ageStrata)) %>%
    dplyr::mutate(age_strata = paste0(.data$X1, ";", .data$X2))
  popSpecs <- tidyr::expand_grid(
    age_strata = ageGrDf$age_strata,
    sex_strata = .env$sexStrata,
    days_prior_history = .env$daysPriorHistory,
    start_date = .env$startDate,
    end_date = .env$endDate
  )
  popSpecs <- popSpecs %>%
    tidyr::separate(.data$age_strata,
      c("min_age", "max_age"),
      remove = FALSE
    ) %>%
    dplyr::mutate(min_age = as.numeric(.data$min_age)) %>%
    dplyr::mutate(max_age = as.numeric(.data$max_age)) %>%
    dplyr::mutate(cohort_definition_id = as.character(dplyr::row_number()))


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
  dpop <- getDenominatorPop(
    cdm = cdm,
    startDate = unique(popSpecs$start_date),
    endDate = unique(popSpecs$end_date),
    minAge = unique(popSpecs$min_age),
    maxAge = unique(popSpecs$max_age),
    daysPriorHistory = unique(popSpecs$days_prior_history),
    strataTable = strataTable,
    strataId = strataId,
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

  # build each of the cohorts of interest
  if (verbose == TRUE) {
    message("Progress: Create each denominator population of interest")
    start <- Sys.time()
  }
  if (dpop$denominator_population %>% dplyr::count() %>% dplyr::pull() == 0) {
    message("- No people found for any denominator population")
  }

  if (dpop$denominator_population %>% dplyr::count() %>% dplyr::pull() > 0) {
    # first, if all cohorts are Male or Female get number that will be excluded
    if (all(popSpecs$sex_strata == "Female")) {
      dpop$attrition <- recordAttrition(
        table = dpop$denominator_population %>%
          dplyr::filter(.data$sex == "Female"),
        id = "person_id",
        reason = "Not Female",
        existingAttrition = dpop$attrition
      )
    }
    if (all(popSpecs$sex_strata == "Male")) {
      dpop$attrition <- recordAttrition(
        table = dpop$denominator_population %>%
          dplyr::filter(.data$sex == "Male"),
        id = "person_id",
        reason = "Not Male",
        existingAttrition = dpop$attrition
      )
    }

    studyPops <- list()

    for (i in seq_along(popSpecs$cohort_definition_id)) {
      workingDpop <- dpop$denominator_population

      if (popSpecs$sex_strata[[i]] %in% c("Male", "Female")) {

        workingDpop <- workingDpop %>%
          dplyr::filter(.data$sex == local(popSpecs$sex_strata[[i]]))
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

  # return results as a list
  results <- list()
  if (dpop$denominator_population %>% dplyr::count() %>% dplyr::pull() > 0) {
    results[["denominator_populations"]] <- studyPops
  } else {
    results[["denominator_populations"]] <- tibble::tibble()
  }
  results[["denominator_settings"]] <- popSpecs
  results[["attrition"]] <- dpop$attrition
  results[["sql_queries"]] <- unlist(sqlQueries)

  return(results)
}
