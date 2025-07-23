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

checkInputGenerateDCS <- function(cdm,
                                  name,
                                  cohortDateRange,
                                  timeAtRisk,
                                  ageGroup,
                                  sex,
                                  daysPriorObservation,
                                  requirementsAtEntry,
                                  requirementInteractions,
                                  targetCohortTable,
                                  targetCohortId,
                                  call = parent.frame()) {
  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::validateNameArgument(name)

  if (!is.null(targetCohortTable)) {
    omopgenerics::validateCohortArgument(cohort = cdm[[targetCohortTable]])
  }

  omopgenerics::assertCharacter(name, length = 1)
  omopgenerics::assertDate(cohortDateRange, length = 2, na = TRUE)
  omopgenerics::assertList(timeAtRisk)
  for (i in seq_along(timeAtRisk)) {
    omopgenerics::assertTrue(length(timeAtRisk[[i]]) == 2)
    omopgenerics::assertNumeric(timeAtRisk[[i]][1], min = 0)
    omopgenerics::assertNumeric(timeAtRisk[[i]][2], min = 0)
    tarCheck <- timeAtRisk[[i]][1] <= timeAtRisk[[i]][2]
    omopgenerics::assertTrue(tarCheck,
      msg = "- upper time at risk value must be equal or higher than lower time at risk value"
    )
  }

  omopgenerics::validateAgeGroupArgument(ageGroup, overlap = TRUE)
  omopgenerics::assertChoice(sex, c("Male", "Female", "Both"))
  omopgenerics::assertNumeric(daysPriorObservation, min = 0)
  omopgenerics::assertLogical(requirementsAtEntry, length = 1)
  omopgenerics::assertLogical(requirementInteractions, length = 1)

  if (!is.null(targetCohortId)) {
    targetCohortId <- omopgenerics::validateCohortIdArgument(
      targetCohortId, cdm[[targetCohortTable]]
    )
  }

  return(targetCohortId)
}

checkInputEstimateIncidence <- function(cdm,
                                        denominatorTable,
                                        outcomeTable,
                                        censorTable,
                                        denominatorCohortId,
                                        outcomeCohortId,
                                        censorCohortId,
                                        interval,
                                        completeDatabaseIntervals,
                                        outcomeWashout,
                                        repeatedEvents) {
  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::validateCohortArgument(cohort = cdm[[denominatorTable]])
  if (!is.null(denominatorCohortId)) {
    denominatorCohortId <- omopgenerics::validateCohortIdArgument(
      denominatorCohortId, cdm[[denominatorTable]]
    )
  }
  omopgenerics::validateCohortArgument(cohort = cdm[[outcomeTable]])
  if (!is.null(outcomeCohortId)) {
    outcomeCohortId <- omopgenerics::validateCohortIdArgument(
      outcomeCohortId, cdm[[outcomeTable]]
    )
  }
  if(!is.null(censorTable)){
  omopgenerics::validateCohortArgument(cohort = cdm[[censorTable]])
  censorCohortId <- omopgenerics::validateCohortIdArgument(
      censorCohortId, cdm[[censorTable]])
    if (length(censorCohortId) > 1) {
      cli::cli_abort("Only one censor cohort can be used. Please specify a single cohort using censorCohortId argument.")
    }
  # one record per person
  if(nrow(omopgenerics::cohortCount(cdm[[censorTable]]) |>
    dplyr::filter(.data$cohort_definition_id == .env$censorCohortId) |>
    dplyr::filter(.data$number_records > .data$number_subjects)) > 0){
    cli::cli_abort("Censor cohort must contain only one record per person.")
  }


  }

  omopgenerics::assertTrue(all(interval %in%
    c(
      "weeks", "months",
      "quarters", "years",
      "overall"
    )))
  omopgenerics::assertLogical(completeDatabaseIntervals)

  if (any(outcomeWashout != Inf)) {
    omopgenerics::assertNumeric(outcomeWashout[which(!is.infinite(outcomeWashout))],
      min = 0, max = 99999
    )
  }

  omopgenerics::assertLogical(repeatedEvents)

  return(list(denominatorCohortId, outcomeCohortId, censorCohortId))
}

checkInputEstimateAdditional <- function(cdm,
                                         denominatorTable,
                                         outcomeTable,
                                         denominatorCohortId,
                                         outcomeCohortId) {
  checkEmpty <- omopgenerics::isTableEmpty(cdm[[denominatorTable]] %>%
    dplyr::filter(.data$cohort_definition_id %in%
      .env$denominatorCohortId))
  omopgenerics::assertTrue(!checkEmpty,
    msg = "- nobody in `denominatorTable` with one of the `denominatorCohortId`"
  )
}

checkInputEstimatePrevalence <- function(cdm,
                                         denominatorTable,
                                         outcomeTable,
                                         denominatorCohortId,
                                         outcomeCohortId,
                                         type,
                                         interval,
                                         completeDatabaseIntervals,
                                         fullContribution,
                                         timePoint,
                                         level) {
  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::validateCohortArgument(cohort = cdm[[denominatorTable]])
  if (!is.null(denominatorCohortId)) {
    denominatorCohortId <- omopgenerics::validateCohortIdArgument(
      denominatorCohortId, cdm[[denominatorTable]]
    )
  }
  omopgenerics::validateCohortArgument(cohort = cdm[[outcomeTable]])
  if (!is.null(outcomeCohortId)) {
    outcomeCohortId <- omopgenerics::validateCohortIdArgument(
      outcomeCohortId, cdm[[outcomeTable]]
    )
  }

  omopgenerics::assertChoice(type, c("point", "period"))
  # overall only for period prevalence
  if (type == "period") {
    omopgenerics::assertTrue(all(interval %in%
      c(
        "weeks", "months",
        "quarters", "years",
        "overall"
      )))
  } else {
    omopgenerics::assertTrue(all(interval %in%
      c(
        "weeks", "months",
        "quarters", "years"
      )))
  }
  omopgenerics::assertTrue(all(timePoint %in% c("start", "middle", "end")))
  omopgenerics::assertLogical(fullContribution)
  omopgenerics::assertLogical(completeDatabaseIntervals)
  omopgenerics::assertChoice(level, c("person", "record"))

  return(list(denominatorCohortId, outcomeCohortId))
}

checkStrata <- function(strata, table) {
  errorMessage <- "strata should be a list that point to columns in the denominator table"

  omopgenerics::assertList(strata,
    msg = "strata should be a list that point to columns in the denominator table"
  )

  if (length(strata) > 0) {
    unlistStrata <- unlist(strata)
    omopgenerics::assertCharacter(unlistStrata,
      msg = "strata should be a list that point to columns in the denominator table"
    )
    omopgenerics::assertChoice(unlistStrata, colnames(table),
      msg = "strata should be a list that point to columns in the denominator table"
    )
  }
}
