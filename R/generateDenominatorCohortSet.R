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
  checkmate::assert_list(ageGroup,
    add = errorMessage
  )
  if (!is.null(ageGroup)) {
    for (i in seq_along(ageGroup)) {
      checkmate::assertTRUE(length(ageGroup[[i]]) == 2)
      checkmate::assert_numeric(ageGroup[[i]][1],
        add = errorMessage
      )
      checkmate::assert_numeric(ageGroup[[i]][2],
        add = errorMessage
      )
      ageCheck <- ageGroup[[i]][1] <=
        ageGroup[[i]][2]
      checkmate::assertTRUE(ageCheck,
        add = errorMessage
      )
      if (!isTRUE(ageCheck)) {
        errorMessage$push(
          "- upper age value must be equal or higher than lower age value"
        )
      }
      checkmate::assertTRUE(ageGroup[[i]][1] >= 0,
        add = errorMessage
      )
      checkmate::assertTRUE(ageGroup[[i]][2] >= 0,
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
  checkmate::assertIntegerish(strataCohortId,
                              len = 1,
                              add = errorMessage,
                              null.ok = TRUE
  )
  checkmate::assertCharacter(strataCohortName,
                              len = 1,
                              add = errorMessage,
                              null.ok = TRUE
  )
  checkmate::assertNumeric(sample,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assertCharacter(tablePrefix,
                             len = 1,
                             add = errorMessage,
                             null.ok = TRUE
  )
  checkmate::assert_logical(verbose,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)


  if(!is.null(tablePrefix)){
    # drop table stem if exists
    dropTable(cdm,
               table = tablePrefix
               )
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
    message("Gettng overall denominator population")
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
                          lapply(popSpecs$cohort_definition_id,
                                 function(x) dpop$attrition),
                          cohort_definition_id =
                            length(popSpecs$cohort_definition_id))

    cohortCount <- tibble::tibble(cohort_definition_id = popSpecs$cohort_definition_id,
                                  n = 0)

  } else if (denominatorPopulationNrows > 0) {
    # first, if all cohorts are Male or Female get number that will be excluded
    nFemale<-dpop$denominator_population %>%
      dplyr::filter(.data$sex == "Female") %>%
      dplyr::select("person_id") %>%
      dplyr::distinct() %>%
      dplyr::tally() %>%
      dplyr::pull()
    nMale<-dpop$denominator_population %>%
      dplyr::filter(.data$sex == "Male") %>%
      dplyr::select("person_id") %>%
      dplyr::distinct() %>%
      dplyr::tally() %>%
      dplyr::pull()

    # attrition so far was the same for each group
    # now we´ll make one for attrition record for each cohort
    dpop$attrition <- Map(cbind,
                          lapply(popSpecs$cohort_definition_id,
                                 function(x) dpop$attrition),
                          cohort_definition_id =
                            length(popSpecs$cohort_definition_id))

    # count dropped for sex criteria
    for(i in seq_along(dpop$attrition)){
      if(popSpecs$sex[[i]]=="Male"){
        dpop$attrition[[i]]<- dplyr::bind_rows(dpop$attrition[[i]],
                                                tibble::tibble(current_n=.env$nMale,
                                                               reason="Not Male",
                                                               excluded=.env$nFemale))
      }
      if(popSpecs$sex[[i]]=="Female"){
        dpop$attrition[[i]]<- dplyr::bind_rows(dpop$attrition[[i]],
                                               tibble::tibble(current_n=.env$nFemale,
                                                              reason="Not Female",
                                                              excluded=.env$nMale))
      }
    }

    studyPops <- list()
    cohortCount <- list()
    cli::cli_progress_bar(total = length(popSpecs$cohort_definition_id),
      format = " -- getting cohort dates for {cli::pb_bar} {cli::pb_current} of {cli::pb_total} cohorts")
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
        reason = glue::glue("No observation time available after applying age and prior history criteria"),
        existingAttrition = dpop$attrition[[i]]
      )

      dpop$attrition[[i]]$cohort_definition_id <- popSpecs$cohort_definition_id[[i]]
      workingCount <- utils::tail(dpop$attrition[[i]]$current_n,1)
      cohortCount[[i]] <- tibble::tibble(
        cohort_definition_id = popSpecs$cohort_definition_id[[i]],
        n = workingCount)
      if(workingCount > 0) {
        studyPops[[paste0("cohort_definition_id_", i)]] <- workingDpop %>%
          dplyr::mutate(cohort_definition_id =
                          local(popSpecs$cohort_definition_id[[i]])) %>%
          dplyr::relocate("cohort_definition_id")
      }
    }
    cli::cli_progress_done()

    if (length(studyPops) != 0 && length(studyPops) < 10) {
      if (verbose == TRUE) {
        message(glue::glue("Unioning cohorts"))
      }
      studyPops <- Reduce(dplyr::union_all, studyPops)
      if(is.null(tablePrefix)){
      studyPops <- studyPops %>%
        CDMConnector::computeQuery()
      } else {
        studyPops <- studyPops %>%
          CDMConnector::computeQuery(name = paste0(tablePrefix,
                                            "_denominator"),
                                     temporary = FALSE,
                                     schema = attr(cdm, "write_schema"),
                                     overwrite = TRUE)
      }

    }
    if (length(studyPops) >= 10) {
        # if 10 or more
        # combine in batches in case of many subgroups
        nBatches <- 10 # number in a batch
        studyPopsBatches <- split(
          studyPops,
          ceiling(seq_along(studyPops) / nBatches)
        )
        cli::cli_progress_bar(total = length(studyPopsBatches),
                              format = " -- unioning {cli::pb_bar} {cli::pb_current} of {cli::pb_total} batched cohorts")

        for (i in seq_along(studyPopsBatches)) {
          if (verbose == TRUE) {
            cli::cli_progress_update()
          }
          studyPopsBatches[[i]] <- Reduce(dplyr::union_all,
                                          studyPopsBatches[[i]])

          if(is.null(tablePrefix)){
            studyPopsBatches[[i]]  <- studyPopsBatches[[i]] %>%
              CDMConnector::computeQuery()
          } else {
            studyPopsBatches[[i]]  <- studyPopsBatches[[i]] %>%
              CDMConnector::computeQuery(name = paste0(tablePrefix,
                                                       "_batch_", i),
                                         temporary = FALSE,
                                         schema = attr(cdm, "write_schema"),
                                         overwrite = TRUE)
          }
        }
        cli::cli_progress_done()

        studyPops <- Reduce(dplyr::union_all, studyPopsBatches)
        if(is.null(tablePrefix)){
          studyPops <- studyPops %>%
            CDMConnector::computeQuery()
        } else {
          studyPops <- studyPops %>%
            CDMConnector::computeQuery(name = paste0(tablePrefix,
                                                     "_denominator"),
                                       temporary = FALSE,
                                       schema = attr(cdm, "write_schema"),
                                       overwrite = TRUE)
        }

        # drop any batch permanent tables
        if(!is.null(tablePrefix)){
        for (i in seq_along(studyPopsBatches)) {
          dropTable(cdm,
                     table = paste0(tablePrefix,
                                    "_batch_", i)
          )
        }}
      }
  }


  if(!is.null(tablePrefix)){
    # drop the intermediate tables that may have been created
    dropTable(cdm,
               table = tablePrefix
    )

    for(i in 1:6){
      dropTable(cdm,
                 table = paste0(tablePrefix, "_", i)
      )
    }

    if(!is.null(sample)){
      dropTable(cdm,
                 table = paste0(tablePrefix,
                                "_person_sample")
      )
    }
  }



  if (verbose == TRUE) {
    dur <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
    message(glue::glue(
      "Time taken to get denominator cohorts: {floor(dur/60)} min and {dur %% 60 %/% 1} sec"
    ))
  }

  # add strata info to settings
  if(is.null(strataCohortId)){
    strataCohortId <- NA
  }
  if(is.null(strataCohortName)){
    strataCohortName <- NA
  }

  # return results as a cohort_reference class
  attr(studyPops, "settings") <- popSpecs %>%
    dplyr::mutate(strata_cohort_definition_id = .env$strataCohortId) %>%
    dplyr::mutate(strata_cohort_name = .env$strataCohortName) %>%
    dplyr::select(!c("min_age", "max_age")) %>%
    dplyr::relocate("cohort_definition_id")
  attr(studyPops, "attrition") <- dplyr::bind_rows(dpop$attrition) %>%
    dplyr::mutate(step = "Generating denominator cohort set") %>%
    dplyr::as_tibble()
  attr(studyPops, "cohortCount") <- dplyr::bind_rows(cohortCount)

  class(studyPops) <- c("IncidencePrevalenceDenominator", "cohort_reference", class(studyPops))

  return(studyPops)
}
