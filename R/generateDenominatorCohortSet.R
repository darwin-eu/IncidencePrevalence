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
#' @param name Name of the cohort table to be created.
#' @param cohortDateRange Two dates. The first indicating the earliest cohort
#' start date and the second indicating the latest possible cohort end date. If
#' NULL or the first date is set as missing, the earliest observation_start_date
#' in the observation_period table will be used for the former. If  NULL or the
#' second date is set as missing, the latest observation_end_date in the
#' observation_period table will be used for the latter.
#' @param ageGroup A list of age groups for which cohorts will be generated. A
#' value of `list(c(0,17), c(18,30))` would, for example, lead to the creation
#' of cohorts for those aged from 0 to 17, and from 18 to 30. In this example
#' an individual turning 18 during the time period would appear in both
#' cohorts (leaving the first cohort the day before their 18th birthday and
#' entering the second from the day of their 18th birthday).
#' @param sex Sex of the cohorts. This can be one or more of: `"Male"`,
#' `"Female"`, or `"Both"`.
#' @param daysPriorObservation The number of days of prior observation observed in
#' the database required for an individual to start contributing time in
#' a cohort.
#' @param requirementInteractions If TRUE, cohorts will be created for
#' all combinations of ageGroup, sex, and daysPriorObservation. If FALSE, only the
#' first value specified for the other factors will be used. Consequently,
#' order of values matters when requirementInteractions is FALSE.
#' @param overwrite Whether to overwrite any existing table with the same name
#'
#' @return A cohort reference
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm,
#'   name = "denominator",
#'   cohortDateRange = as.Date(c("2008-01-01", "2020-01-01"))
#' )
#' cdm$denominator
#' }
generateDenominatorCohortSet <- function(cdm,
                                         name,
                                         cohortDateRange = as.Date(c(NA, NA)),
                                         ageGroup = list(c(0, 150)),
                                         sex = "Both",
                                         daysPriorObservation = 0,
                                         requirementInteractions = TRUE,
                                         overwrite = TRUE){

  fetchDenominatorCohortSet(
    cdm = cdm,
    name = name,
    cohortDateRange = cohortDateRange,
    ageGroup = ageGroup,
    sex = sex,
    daysPriorObservation = daysPriorObservation,
    requirementInteractions = requirementInteractions,
    targetCohortTable = NULL,
    targetCohortId = NULL,
    overwrite = overwrite
  )

}

#' Identify a set of denominator populations using a target cohort
#'
#' @description
#' `generateTargetDenominatorCohortSet()` creates a set of cohorts that
#' can be used for the denominator population in analyses of incidence,
#' using `estimateIncidence()`, or prevalence, using `estimatePointPrevalence()`
#' or `estimatePeriodPrevalence()`.
#'
#' @param cdm A CDM reference object
#' @param name Name of the cohort table to be created.
#' @param targetCohortTable A cohort table in the cdm reference to use
#' to limit cohort entry and exit (with individuals only contributing to a
#' cohort when they are contributing to the cohort in the target table).
#' @param targetCohortId The cohort definition id for the cohort of interest
#'  in the target table. If targetCohortTable is specified, a single targetCohortId
#'  must also be specified.
#' @param cohortDateRange Two dates. The first indicating the earliest cohort
#' start date and the second indicating the latest possible cohort end date. If
#' NULL or the first date is set as missing, the earliest observation_start_date
#' in the observation_period table will be used for the former. If  NULL or the
#' second date is set as missing, the latest observation_end_date in the
#' observation_period table will be used for the latter.
#' @param ageGroup A list of age groups for which cohorts will be generated. A
#' value of `list(c(0,17), c(18,30))` would, for example, lead to the creation
#' of cohorts for those aged from 0 to 17, and from 18 to 30. In this example
#' an individual turning 18 during the time period would appear in both
#' cohorts (leaving the first cohort the day before their 18th birthday and
#' entering the second from the day of their 18th birthday).
#' @param sex Sex of the cohorts. This can be one or more of: `"Male"`,
#' `"Female"`, or `"Both"`.
#' @param daysPriorObservation The number of days of prior observation observed in
#' the database required for an individual to start contributing time in
#' a cohort.
#' @param requirementInteractions If TRUE, cohorts will be created for
#' all combinations of ageGroup, sex, and daysPriorObservation. If FALSE, only the
#' first value specified for the other factors will be used. Consequently,
#' order of values matters when requirementInteractions is FALSE.
#' @param overwrite Whether to overwrite any existing table with the same name
#'
#' @return A cohort reference
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
#' cdm <- generateTargetDenominatorCohortSet(
#'   cdm = cdm,
#'   name = "denominator",
#'   targetCohortTable = "target",
#'   cohortDateRange = as.Date(c("2008-01-01", "2020-01-01"))
#' )
#' cdm$denominator
#' }
generateTargetDenominatorCohortSet <- function(cdm,
                                               name,
                                               targetCohortTable,
                                               targetCohortId = NULL,
                                               cohortDateRange = as.Date(c(NA, NA)),
                                               ageGroup = list(c(0, 150)),
                                               sex = "Both",
                                               daysPriorObservation = 0,
                                               requirementInteractions = TRUE,
                                               overwrite = TRUE){

  fetchDenominatorCohortSet(
    cdm = cdm,
    name = name,
    cohortDateRange = cohortDateRange,
    ageGroup = ageGroup,
    sex = sex,
    daysPriorObservation = daysPriorObservation,
    requirementInteractions = requirementInteractions,
    targetCohortTable = targetCohortTable,
    targetCohortId = targetCohortId,
    overwrite = overwrite
  )

}



fetchDenominatorCohortSet <- function(cdm,
                                         name,
                                         cohortDateRange = as.Date(c(NA, NA)),
                                         ageGroup = list(c(0, 150)),
                                         sex = "Both",
                                         daysPriorObservation = 0,
                                         requirementInteractions = TRUE,
                                         targetCohortTable = NULL,
                                         targetCohortId = NULL,
                                         overwrite = FALSE) {
  startCollect <- Sys.time()

  checkInputGenerateDCS(
    cdm = cdm,
    name = name,
    cohortDateRange = cohortDateRange,
    ageGroup = ageGroup,
    sex = sex,
    daysPriorObservation = daysPriorObservation,
    requirementInteractions = requirementInteractions,
    targetCohortTable = targetCohortTable,
    targetCohortId = targetCohortId
  )

  if(any(is.na(cohortDateRange))){
    cohortDateRange <- getCohortDateRange(cdm = cdm,
                                          cohortDateRange = cohortDateRange)
  }

  # we'll use this as the stem for any intermediate tables we create along the way
  intermediateTable <- paste0("ip_d_",
                              tolower(paste0(sample(LETTERS, 4, replace = TRUE),
                                             collapse = "")))

  # define cohorts to generate
  ageGrDf <- data.frame(do.call(rbind, ageGroup)) %>%
    dplyr::mutate(age_group = paste0(.data$X1, ";", .data$X2))

  popSpecs <- buildPopSpecs(
    ageGrDf = ageGrDf,
    sex = sex,
    daysPriorObservation = daysPriorObservation,
    requirementInteractions = requirementInteractions
  ) %>%
    dplyr::mutate(
      min_age = as.numeric(.data$min_age),
      max_age = as.numeric(.data$max_age),
      start_date = cohortDateRange[1],
      end_date = cohortDateRange[2]
    )

  # get target cohort ids if not given
  if(!is.null(targetCohortTable) && is.null(targetCohortId)){
    targetCohortId <- sort(CDMConnector::cohortSet(cdm[[targetCohortTable]]) %>%
      dplyr::pull("cohort_definition_id"))
  }

  if(is.null(targetCohortId)){
    denominatorSet<- popSpecs %>%
      dplyr::mutate(
        targetCohortTable = as.character(NA), targetCohortId = as.numeric(NA)
      )
  } else {
  denominatorSet <- list()
  for(i in seq_along(targetCohortId)){
    denominatorSet[[i]] <- popSpecs %>%
      dplyr::mutate(
        targetCohortTable = targetCohortTable,
        targetCohortId = targetCohortId[[i]]
      )
  }
  }
  denominatorSet <- dplyr::bind_rows(denominatorSet) %>%
    dplyr::mutate(cohort_definition_id = dplyr::row_number())

# get results for a single target or, if no target, full result
  if(is.null(targetCohortId)){
    denominatorSet<-list(denominatorSet)
  } else {
    denominatorSet <- split(denominatorSet,
                            f = denominatorSet$targetCohortId,
                            drop = FALSE)
  }

  cohortRef <- NULL
  cohortSetRef <- NULL
  cohortCountRef <- NULL
  cohortAttritionRef <- NULL
for(i in 1:length(denominatorSet)){
  denom <-  fetchSingleTargetDenominatorCohortSet(cdm = cdm,
                                                     name = name,
                                                     intermediateTable = paste0(intermediateTable, i),
                                                     popSpecs = denominatorSet[[i]])
  cohortSetRef <- cohortSetRef %>%
    dplyr::union_all(attr(denom, "cohort_set"))
  cohortCountRef <- cohortCountRef %>%
    dplyr::union_all(attr(denom, "cohort_count"))
  cohortAttritionRef <- cohortAttritionRef %>%
    dplyr::union_all(attr(denom, "cohort_attrition"))
  if (sum(attr(denom, "cohort_count")$number_records) > 0) {
    if (!"tbl" %in% class(denom)) {
      denom <- Reduce(dplyr::union_all, denom)
    }
    cohortRef <- updateCohort(
      table = cohortRef,
      x = denom,
      name = name,
      cdm = cdm,
      overwrite = overwrite
    )
  }
  CDMConnector::dropTable(cdm = cdm, name = dplyr::starts_with(paste0(intermediateTable, i)))
}

  if (is.null(cohortRef)) {
    DBI::dbWriteTable(
      conn = attr(cdm, "dbcon"),
      name = CDMConnector::inSchema(schema = attr(cdm, "write_schema"), table =  name),
      value = dplyr::tibble(
        cohort_definition_id = as.integer(numeric()),
        subject_id = character(),
        cohort_start_date = date(),
        cohort_end_date = date()
      ),
      overwrite = overwrite
    )
    cohortRef <- dplyr::tbl(attr(cdm, "dbcon"), CDMConnector::inSchema(
      attr(cdm, "write_schema"), name
    ))
  }
  # create final cohort
  attr(cohortRef, "cdm_reference") <- cdm
  cdm[[name]] <- CDMConnector::newGeneratedCohortSet(
    cohortRef = cohortRef,
    cohortSetRef = cohortSetRef,
    cohortAttritionRef = cohortAttritionRef,
    cohortCountRef = cohortCountRef,
    overwrite = overwrite
  )
  class(cdm[[name]]) <- c("denominator_cohort_set", class(cdm[[name]]))

 # drop the intermediate tables
 CDMConnector::dropTable(
   cdm = cdm,
   name = dplyr::starts_with(paste0(intermediateTable))
 )

 dur <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
 cli::cli_alert_success(glue::glue(
   "Cohorts created in {floor(dur/60)} min and {dur %% 60 %/% 1} sec"
 ))

  return(cdm)
}



# Generates denominator cohorts for a single target id or no target
fetchSingleTargetDenominatorCohortSet <- function(cdm,
                                         name,
                                         intermediateTable,
                                         popSpecs) {


  if(all(is.na(popSpecs$targetCohortId))){
    cli::cli_alert_info("Creating denominator cohorts")
  } else {
    cli::cli_alert_info("Creating denominator cohorts: target cohort id {unique(popSpecs$targetCohortId)}")
  }

  tablePrefix <- intermediateTable

  # get the overall contributing population (without stratification)
  # we need to the output the corresponding dates when getting the denominator
  dpop <- getDenominatorCohorts(
    cdm = cdm,
    startDate = unique(popSpecs$start_date),
    endDate = unique(popSpecs$end_date),
    minAge = unique(popSpecs$min_age),
    maxAge = unique(popSpecs$max_age),
    daysPriorObservation = unique(popSpecs$days_prior_observation),
    targetCohortTable = unique(popSpecs$targetCohortTable),
    targetCohortId = unique(popSpecs$targetCohortId),
    intermediateTable = paste0(intermediateTable, "_gdc_")
  )

  denominatorPopulationNrows <- dpop$denominator_population %>%
    dplyr::count() %>%
    dplyr::pull()

  if (denominatorPopulationNrows == 0) {
    if(all(is.na(popSpecs$targetCohortId))){
      cli::cli_warn("- No people found for denominator population")
    } else {
      cli::cli_alert_info("- No people found for target cohort id {unique(popSpecs$targetCohortId)}")
    }

    studyPops <- dpop$denominator_population %>%
      dplyr::select(
        "cohort_definition_id" = "gender_concept_id",
        "subject_id" = "person_id",
        "cohort_start_date" = "observation_period_start_date",
        "cohort_end_date" = "observation_period_end_date"
      )

    # attrition is the same for each group
    dpop$attrition <- Reduce(
      dplyr::union_all,
      lapply(
        popSpecs$cohort_definition_id,
        function(x) {
          dpop$attrition %>%
            dplyr::mutate("cohort_definition_id" = .env$x) %>%
            dplyr::relocate("cohort_definition_id")
        }
      )
    )

    cohortCount <- tibble::tibble(
      cohort_definition_id = popSpecs$cohort_definition_id,
      number_records = as.integer(0),
      number_subjects = as.integer(0)
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
            dplyr::filter(.data$sex == "Male"),
          id = "person_id",
          reasonId = 8,
          reason = "Not Male",
          existingAttrition = dpop$attrition[[i]]
        )
      }
      if (popSpecs$sex[[i]] == "Female") {
        dpop$attrition[[i]] <- recordAttrition(
          table = dpop$denominator_population %>%
            dplyr::filter(.data$sex == "Female"),
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
      cli::cli_progress_update()
      workingDpop <- dpop$denominator_population

      if (popSpecs$sex[[i]] %in% c("Male", "Female")) {
        workingDpop <- workingDpop %>%
          dplyr::filter(.data$sex == local(popSpecs$sex[[i]]))
      }

      workingDpop <- workingDpop %>%
        dplyr::rename(
          # cohort start
          "cohort_start_date" =
            glue::glue("date_min_age{popSpecs$min_age[[i]]}prior_history{popSpecs$days_prior_observation[[i]]}"),
          # cohort end
          "cohort_end_date" =
            glue::glue(
              "date_max_age{popSpecs$max_age[[i]]}"
            ),
          "subject_id" = "person_id"
        ) %>%
        dplyr::select(dplyr::any_of(c("subject_id", "cohort_start_date", "cohort_end_date",
                                      "target_cohort_start_date")))

      if(!is.na(unique(popSpecs$targetCohortTable))){
        # make sure that cohort start was before or on target start
        # and update cohort start to target start
        workingDpop <- workingDpop %>%
          dplyr::filter(.data$cohort_start_date <=
                          .data$target_cohort_start_date) %>%
          dplyr::mutate(cohort_start_date = .data$target_cohort_start_date) %>%
          dplyr::select(dplyr::any_of(c("subject_id",
                                        "cohort_start_date",
                                        "cohort_end_date")))
      }

      workingDpop <- workingDpop %>%
        dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)

      dpop$attrition[[i]] <- recordAttrition(
        table = workingDpop,
        id = "subject_id",
        reasonId = 10,
        reason = glue::glue("No observation time available after applying age, prior observation and, if applicable, target criteria"),
        existingAttrition = dpop$attrition[[i]]
      )

      dpop$attrition[[i]]$cohort_definition_id <- popSpecs$cohort_definition_id[[i]]
      workingCount <- utils::tail(dpop$attrition[[i]]$number_records, 1)
      cohortCount[[i]] <- workingDpop %>%
        dplyr::summarise(
          number_records = dplyr::n(),
          number_subjects = dplyr::n_distinct(.data$subject_id)
        ) %>%
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

    studyPops <- unionCohorts(
      cdm = cdm,
      studyPops = studyPops,
      intermediateTable = paste0(intermediateTable, "_u_")
    )
  }

  # add target info to settings
  if (is.na(unique(popSpecs$targetCohortTable))) {
    targetCohortId <- NA
    targetCohortName <- NA
  } else {
    targetCohortName <- attr(cdm[[unique(popSpecs$targetCohortTable)]], "cohort_set") %>%
      dplyr::filter(.data$cohort_definition_id == !!unique(popSpecs$targetCohortId)) %>%
      dplyr::pull("cohort_name")
  }

  # return results as a cohort_reference class
  cohortCount <- dplyr::bind_rows(cohortCount)
  attr(studyPops, "cohort_count") <- cohortCount

  cohortSet <- popSpecs %>%
    dplyr::mutate(target_cohort_definition_id = unique(popSpecs$targetCohortId)) %>%
    dplyr::mutate(target_cohort_name = .env$targetCohortName) %>%
    dplyr::mutate(cohort_name = paste0("Denominator cohort ",
                                       .data$cohort_definition_id)) %>%
    dplyr::select(!c("min_age", "max_age","targetCohortTable", "targetCohortId")) %>%
    dplyr::relocate("cohort_definition_id") %>%
    dplyr::relocate("cohort_name", .after = "cohort_definition_id") %>%
    dplyr::mutate(age_group =
                    stringr::str_replace(.data$age_group, ";", " to "))
  attr(studyPops, "cohort_set") <- cohortSet

  cohortAttrition <- dplyr::bind_rows(dpop$attrition) %>%
    dplyr::as_tibble() %>%
    dplyr::relocate("cohort_definition_id")
  attr(studyPops, "cohort_attrition") <- cohortAttrition

  return(studyPops)
}

# If the user doesn't specify date range
# range to min and max of obs period
getCohortDateRange <- function(cdm, cohortDateRange){
  if (is.na(cohortDateRange[1])) {
    cohortDateRange[1] <- as.Date(cdm[["observation_period"]] %>%
                                    dplyr::summarise(
                                      min(.data$observation_period_start_date,
                                          na.rm = TRUE
                                      )
                                    ) %>%
                                    dplyr::collect() %>%
                                    dplyr::pull())
  }
  if (is.na(cohortDateRange[2])) {
    cohortDateRange[2] <- as.Date(cdm[["observation_period"]] %>%
                                    dplyr::summarise(
                                      max(.data$observation_period_end_date,
                                          na.rm = TRUE
                                      )
                                    ) %>%
                                    dplyr::collect() %>%
                                    dplyr::pull())
  }
  return(cohortDateRange)
}


# cohort specifications
buildPopSpecs <- function(ageGrDf,
                          sex,
                          daysPriorObservation,
                          requirementInteractions) {
  if (isTRUE(requirementInteractions)) {
    popSpecs <- tidyr::expand_grid(
      age_group = ageGrDf$age_group,
      sex = .env$sex,
      days_prior_observation = .env$daysPriorObservation
    )
  } else {
    popSpecs <- dplyr::bind_rows(
      dplyr::tibble(
        age_group = ageGrDf$age_group,
        sex = .env$sex[1],
        days_prior_observation = .env$daysPriorObservation[1]
      ),
      dplyr::tibble(
        age_group = ageGrDf$age_group[1],
        sex = .env$sex,
        days_prior_observation = .env$daysPriorObservation[1]
      ),
      dplyr::tibble(
        age_group = ageGrDf$age_group[1],
        sex = .env$sex[1],
        days_prior_observation = .env$daysPriorObservation
      )
    ) %>%
      dplyr::distinct()
  }

  popSpecs <- popSpecs %>%
    tidyr::separate(.data$age_group,
      c("min_age", "max_age"),
      remove = FALSE
    )

  return(popSpecs)
}


# union cohort tables
# combine the set of separate cohort tables into a single table
unionCohorts <- function(cdm,
                         studyPops,
                         intermediateTable) {
  # extract attributes
  # we need to combine these as well as the cohort tables
  allCohortSet <- list()
  allCohortCount <- list()
  allCohortAttrition <- list()
  for(i in seq_along(studyPops)){
    if(!is.null(attr(studyPops[[i]], "cohort_set"))){
      allCohortSet[[i]] <- CDMConnector::cohort_set(studyPops[[i]])
      allCohortCount[[i]] <- CDMConnector::cohort_count(studyPops[[i]])
      allCohortAttrition[[i]] <- CDMConnector::cohort_attrition(studyPops[[i]])
    }
  }

  if (length(studyPops) >= 10) {
    # if 10 or more
    # combine in batches in case of many subgroups
    studyPopsBatches <- split(
      studyPops,
      ceiling(seq_along(studyPops) / 10) # 10 in a batch
    )

    if(length(allCohortSet)>0){
    allCohortSetBatches <- split(
      allCohortSet,
      ceiling(seq_along(allCohortSet) / 10) # 10 in a batch
    )
    allCohortCountBatches <- split(
      allCohortCount,
      ceiling(seq_along(allCohortCount) / 10) # 10 in a batch
    )
    allCohortAttritionBatches <- split(
      allCohortAttrition,
      ceiling(seq_along(allCohortAttrition) / 10) # 10 in a batch
    )
    }

    cli::cli_progress_bar(
      total = length(studyPopsBatches),
      format = " -- unioning {cli::pb_bar} {cli::pb_current} of {cli::pb_total} batched cohorts"
    )

    for (i in seq_along(studyPopsBatches)) {
      cli::cli_progress_update()
      studyPopsBatches[[i]] <- Reduce(
        dplyr::union_all,
        studyPopsBatches[[i]]
      )

        studyPopsBatches[[i]] <- studyPopsBatches[[i]] %>%
          CDMConnector::computeQuery(
            name = paste0(
              intermediateTable,
              "_batch_", i
            ),
            temporary = FALSE,
            schema = attr(cdm, "write_schema"),
            overwrite = TRUE
          )

        if(length(allCohortSet) > 0){
          allCohortSetBatches[[i]] <- Reduce(dplyr::union_all,
                                             allCohortSetBatches[[i]]) %>%
            CDMConnector::computeQuery(
              name = paste0(intermediateTable, "_cohort_set",
                            "_batch_", i),
              temporary = FALSE,
              schema = attr(cdm, "write_schema"),
              overwrite = TRUE
            )
          allCohortCountBatches[[i]] <- Reduce(dplyr::union_all,
                                             allCohortCountBatches[[i]]) %>%
            CDMConnector::computeQuery(
              name = paste0(intermediateTable, "_cohort_count",
                            "_batch_", i),
              temporary = FALSE,
              schema = attr(cdm, "write_schema"),
              overwrite = TRUE
            )
          allCohortAttritionBatches[[i]] <- Reduce(dplyr::union_all,
                                               allCohortAttritionBatches[[i]]) %>%
            CDMConnector::computeQuery(
              name = paste0(intermediateTable, "_cohort_attrition",
                            "_batch_", i),
              temporary = FALSE,
              schema = attr(cdm, "write_schema"),
              overwrite = TRUE
            )
        }



    }
    cli::cli_progress_done()

     studyPops <- Reduce(dplyr::union_all, studyPopsBatches) %>%
        CDMConnector::computeQuery(
          name = intermediateTable,
          temporary = FALSE,
          schema = attr(cdm, "write_schema"),
          overwrite = TRUE
        )
     if(length(allCohortSet) > 0){
     allCohortSet <- Reduce(dplyr::union_all, allCohortSetBatches) %>%
       CDMConnector::computeQuery(
         name = paste0(intermediateTable, "_cohort_set"),
         temporary = FALSE,
         schema = attr(cdm, "write_schema"),
         overwrite = TRUE
       )
     allCohortCount <- Reduce(dplyr::union_all, allCohortCountBatches) %>%
       CDMConnector::computeQuery(
         name = paste0(intermediateTable, "_cohort_count"),
         temporary = FALSE,
         schema = attr(cdm, "write_schema"),
         overwrite = TRUE
       )
     allCohortAttrition <- Reduce(dplyr::union_all, allCohortAttritionBatches) %>%
       CDMConnector::computeQuery(
         name = paste0(intermediateTable, "_cohort_attrition"),
         temporary = FALSE,
         schema = attr(cdm, "write_schema"),
         overwrite = TRUE
       )
     }


      # drop intermediate tables
      CDMConnector::dropTable(
        cdm = cdm,
        name = tidyselect::starts_with(paste0(intermediateTable, "_b"))
      )
  }

  if(length(allCohortSet)>0){
    attr(studyPops, "cohort_set") <- allCohortSet
    attr(studyPops, "cohort_count") <- allCohortCount
    attr(studyPops, "cohort_attrition") <- allCohortAttrition
  }

  return(studyPops)
}


# insert attributes into database
insertAttributes <- function(cdm, tablePrefix, name,
                             cohortCount, cohortSet, cohortAttrition) {
  DBI::dbWriteTable(attr(cdm, "dbcon"),
    name = CDMConnector::inSchema(attr(cdm, "write_schema"),
                                  paste0(tablePrefix, "_count")),
    value = as.data.frame(cohortCount),
    overwrite = TRUE
  )
  DBI::dbWriteTable(attr(cdm, "dbcon"),
    name = CDMConnector::inSchema(attr(cdm, "write_schema"),
                                  paste0(tablePrefix, "_set")),
    value = as.data.frame(cohortSet),
    overwrite = TRUE
  )
  DBI::dbWriteTable(attr(cdm, "dbcon"),
    name = CDMConnector::inSchema(attr(cdm, "write_schema"),
                                  paste0(tablePrefix, "_attrition")),
    value = as.data.frame(cohortAttrition),
    overwrite = TRUE
  )
}

updateCohort <- function(table, x, name, cdm, overwrite) {
  if (is.null(table)) {
    table <- x %>%
      CDMConnector::computeQuery(
        name = name,
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = overwrite
      )
  } else {
    table <- CDMConnector::appendPermanent(
      x = x,
      name = name,
      schema = attr(cdm, "write_schema")
    )
  }
  return(table)
}
