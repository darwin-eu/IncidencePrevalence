checkInputGenerateDCS <- function(cdm,
                                  name,
                                  cohortDateRange,
                                  timeAtRisk,
                                  ageGroup,
                                  sex,
                                  daysPriorObservation,
                                  requirementInteractions,
                                  targetCohortTable,
                                  targetCohortId,
                                  call = parent.frame()) {

  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::validateNameArgument(name)

  if (!is.null(targetCohortTable)) {
    cdmTargetCheck(cdm, targetCohortTable = targetCohortTable)
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
                             msg = "- upper age value must be equal or higher than lower age value")
  }

  omopgenerics::validateAgeGroupArgument(ageGroup, overlap = TRUE)
  omopgenerics::assertChoice(sex, c("Male", "Female", "Both"))
  omopgenerics::assertNumeric(daysPriorObservation, min = 0)
  omopgenerics::assertLogical(requirementInteractions, length = 1)

  if(!is.null(targetCohortId)) {
    targetCohortId <- omopgenerics::validateCohortIdArgument(
      targetCohortId, cdm[[targetCohortTable]])
  }

  return(targetCohortId)
}

checkInputEstimateIncidence <- function(cdm,
                                        denominatorTable,
                                        outcomeTable,
                                        denominatorCohortId,
                                        outcomeCohortId,
                                        interval,
                                        completeDatabaseIntervals,
                                        outcomeWashout,
                                        repeatedEvents) {

  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::assertChoice(denominatorTable, names(cdm),
                             msg = "- `denominatorTable` is not found in cdm")

  if(!is.null(denominatorCohortId)) {
    denominatorCohortId <- omopgenerics::validateCohortIdArgument(
      denominatorCohortId, cdm[[denominatorTable]])
  }

  omopgenerics::assertChoice(outcomeTable, names(cdm),
                             msg = paste0("outcomeTable ", outcomeTable, " is not found in cdm"))

  outcomeAttributeCheck <- (!is.null(
    omopgenerics::cohortCount(cdm[[outcomeTable]])
  ) &
    !is.null(
      omopgenerics::settings(cdm[[outcomeTable]])
    ))
  omopgenerics::assertTrue(outcomeAttributeCheck,
                           msg = "- `outcomeTable` is missing cohort_count or cohort_set attribute")

  if(!is.null(outcomeCohortId)) {
    outcomeCohortId <- omopgenerics::validateCohortIdArgument(
      outcomeCohortId, cdm[[outcomeTable]])
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
                              min = 0, max = 99999)
  }

  omopgenerics::assertLogical(repeatedEvents)

  return(list(denominatorCohortId, outcomeCohortId))
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
                           msg =  "- nobody in `denominatorTable` with one of the `denominatorCohortId`")

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
                                         timePoint) {
  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::assertChoice(denominatorTable, names(cdm),
                             msg = "- `denominatorTable` is not found in cdm")

  if(!is.null(denominatorCohortId)) {
    denominatorCohortId <- omopgenerics::validateCohortIdArgument(
      denominatorCohortId, cdm[[denominatorTable]])
  }

  omopgenerics::assertChoice(outcomeTable, names(cdm),
                             msg = "- `outcomeTable` is not found in cdm")

  outcomeAttributeCheck <- (!is.null(
  omopgenerics::cohortCount(cdm[[outcomeTable]])) &
    !is.null(
      omopgenerics::settings(cdm[[outcomeTable]])))
  omopgenerics::assertTrue(outcomeAttributeCheck,
                           msg = "- `outcomeTable` is missing cohort_count or cohort_set attribute")

  if(!is.null(outcomeCohortId)) {
    outcomeCohortId <- omopgenerics::validateCohortIdArgument(
      outcomeCohortId, cdm[[outcomeTable]])
  }

  omopgenerics::assertChoice(type, c("point", "period"))
  # overall only for period prevalence
  if(type == "period"){
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

  return(list(denominatorCohortId, outcomeCohortId))
}

cdmTargetCheck <- function(cdm, targetCohortTable) {
  cdmTargetCheck <- inherits(cdm[[targetCohortTable]], "tbl_dbi")
  omopgenerics::assertTrue(cdmTargetCheck,
                           msg = "- targetCohortTable table not found")
  omopgenerics::assertChoice(names(cdm[[targetCohortTable]] %>%
                                         utils::head(1) %>%
                                         dplyr::collect()),
                                 c(
                                   "cohort_definition_id", "subject_id",
                                   "cohort_start_date", "cohort_end_date"
                                 ))
}

checkStrata <- function(strata, table) {
  errorMessage <- "strata should be a list that point to columns in the denominator table"

  omopgenerics::assertList(strata,
                           msg = "strata should be a list that point to columns in the denominator table")

  if (length(strata) > 0) {
    unlistStrata <- unlist(strata)
    omopgenerics::assertCharacter(unlistStrata,
                                  msg = "strata should be a list that point to columns in the denominator table")
    omopgenerics::assertChoice(unlistStrata, colnames(table),
                               msg = "strata should be a list that point to columns in the denominator table")
  }
}
