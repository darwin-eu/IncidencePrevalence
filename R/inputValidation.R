checkInputGenerateDCS <- function(cdm,
                                  name,
                                  cohortDateRange,
                                  ageGroup,
                                  sex,
                                  daysPriorObservation,
                                  requirementInteractions,
                                  targetCohortTable,
                                  targetCohortId,
                                  call = parent.frame()) {
  cdmCheck(cdm)


  if(stringr::str_detect(name, "^[a-z0-9_]+$", negate = TRUE)){
    cli::cli_abort(c("name must be given in snake case",
                     "i" = "for example 'my_cohort' is allowed but 'MyCohort' is not"))
  }

  if(is.null(attr(cdm, "write_schema"))){
  cli::cli_abort("cdm must have a write_schema specified",
                 call = call)
  }
  if(is.null(attr(cdm, "write_schema"))){
    cli::cli_abort("cdm must have write schema specified")
  }
  if (!is.null(targetCohortTable)) {
    cdmTargetCheck(cdm, targetCohortTable = targetCohortTable)
  }

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::expect_character(name, len = 1)
  checkmate::expect_date(cohortDateRange, len = 2)
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
      "- sex must be from: Male, Female, and Both"
    )
  }
  checkmate::assert_numeric(daysPriorObservation,
    add = errorMessage
  )
  daysCheck <- all(daysPriorObservation >= 0)
  if (!isTRUE(daysCheck)) {
    errorMessage$push(
      "- daysPriorObservation cannot be negative"
    )
  }
  checkmate::assert_logical(requirementInteractions,
    len = 1,
    add = errorMessage
  )
  if (!is.null(targetCohortTable)) {
    targetCohortTableCheck <- inherits(cdm[[targetCohortTable]], "tbl_dbi")
    checkmate::assertTRUE(targetCohortTableCheck, add = errorMessage)
    if (!isTRUE(targetCohortTableCheck)) {
      errorMessage$push(
        "- targetCohortTable not found"
      )
    }
    targetNamesCheck <- all(names(cdm[[targetCohortTable]] %>%
      utils::head(1) %>%
      dplyr::collect()) %in%
      c(
        "cohort_definition_id", "subject_id",
        "cohort_start_date", "cohort_end_date"
      ))
    checkmate::assertTRUE(targetNamesCheck, add = errorMessage)
    if (!isTRUE(targetNamesCheck)) {
      errorMessage$push(
        "- targetCohortTable does not conform to cohort table specification"
      )
    }
  }
    checkmate::assertIntegerish(targetCohortId,
      add = errorMessage,
      null.ok = TRUE
    )

  return(checkmate::reportAssertions(collection = errorMessage))
}

checkInputEstimateIncidence <- function(cdm,
                                        denominatorTable,
                                        outcomeTable,
                                        denominatorCohortId,
                                        outcomeCohortId,
                                        interval,
                                        completeDatabaseIntervals,
                                        outcomeWashout,
                                        repeatedEvents,
                                        minCellCount,
                                        temporary,
                                        returnParticipants) {
  cdmCheck(cdm)

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
  outcomeAttributeCheck <- (!is.null(attr(
    cdm[[outcomeTable]],
    "cohort_count"
  )) &
    !is.null(attr(
      cdm[[outcomeTable]],
      "cohort_set"
    )))
  checkmate::assertTRUE(outcomeAttributeCheck,
    add = errorMessage
  )
  if (!isTRUE(outcomeAttributeCheck)) {
    errorMessage$push(
      "- `outcomeTable` is missing cohort_count or cohort_set attribute"
    )
  }

  checkmate::assertIntegerish(outcomeCohortId,
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
  if (any(outcomeWashout != Inf)) {
    checkmate::assert_numeric(outcomeWashout[which(!is.infinite(outcomeWashout))],
      add = errorMessage
    )
  }
  checkmate::assert_logical(repeatedEvents,
    add = errorMessage
  )
  checkmate::assert_number(minCellCount)
  checkmate::assert_logical(temporary,
    add = errorMessage
  )
  if (isTRUE(temporary)) {
    # returnParticipants only when we are using permanent tables
    checkmate::assert_false(returnParticipants,
      add = errorMessage
    )
  }
  checkmate::assert_logical(returnParticipants,
    add = errorMessage
  )
  return(checkmate::reportAssertions(collection = errorMessage))
}

checkInputEstimateIncidenceAdditional <- function(cdm,
                                                  denominatorTable,
                                                  outcomeTable,
                                                  denominatorCohortId,
                                                  outcomeCohortId) {
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
  return(checkmate::reportAssertions(collection = errorMessage))
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
                                         minCellCount,
                                         temporary,
                                         returnParticipants) {
  cdmCheck(cdm)

  errorMessage <- checkmate::makeAssertCollection()
  denomCheck <- denominatorTable %in% names(cdm)
  checkmate::assertTRUE(denomCheck,
    add = errorMessage
  )
  if (!isTRUE(denomCheck)) {
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
  outcomeAttributeCheck <- (!is.null(attr(
    cdm[[outcomeTable]],
    "cohort_count"
  )) &
    !is.null(attr(
      cdm[[outcomeTable]],
      "cohort_set"
    )))
  checkmate::assertTRUE(outcomeAttributeCheck,
    add = errorMessage
  )
  if (!isTRUE(outcomeAttributeCheck)) {
    errorMessage$push(
      "- `outcomeTable` is missing cohort_count or cohort_set attribute"
    )
  }
  checkmate::assertIntegerish(outcomeCohortId,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assert_choice(type,
    choices = c("point", "period"),
    add = errorMessage
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
  checkmate::assertTRUE(all(timePoint %in% c("start", "middle", "end")),
    add = errorMessage
  )
  checkmate::assert_number(minCellCount)
  checkmate::assert_logical(fullContribution,
    add = errorMessage
  )
  checkmate::assert_logical(completeDatabaseIntervals,
    add = errorMessage
  )
  checkmate::assert_logical(temporary,
    add = errorMessage
  )
  checkmate::assert_logical(returnParticipants,
    add = errorMessage
  )
  if (isTRUE(temporary)) {
    # returnParticipants only when we are using permanent tables
    checkmate::assert_false(returnParticipants,
      add = errorMessage
    )
  }
  return(checkmate::reportAssertions(collection = errorMessage))
}

cdmCheck <- function(cdm) {
  errorMessage <- checkmate::makeAssertCollection()
  cdmCheck <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdmCheck,
    add = errorMessage
  )
  if (!isTRUE(cdmCheck)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference"
    )
  }
  checkmate::reportAssertions(collection = errorMessage)

  errorMessage <- checkmate::makeAssertCollection()
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
  return(checkmate::reportAssertions(collection = errorMessage))
}

cdmTargetCheck <- function(cdm, targetCohortTable) {
  errorMessage <- checkmate::makeAssertCollection()
  cdmTargetCheck <- inherits(cdm[[targetCohortTable]], "tbl_dbi")
  checkmate::assertTRUE(cdmTargetCheck, add = errorMessage)
  if (!isTRUE(cdmTargetCheck)) {
    errorMessage$push(
      "- targetCohortTable table not found"
    )
  }
  return(checkmate::reportAssertions(collection = errorMessage))
}

checkStrata <- function(strata, table) {
  errorMessage <- "strata should be a list that point to columns in the denominator table"
  if (!is.list(strata)) {
    cli::cli_abort(errorMessage)
  }
  if (length(strata) > 0) {
    if (!is.character(unlist(strata))) {
      cli::cli_abort(errorMessage)
    }
    if (!all(unlist(strata) %in% colnames(table))) {
      cli::cli_abort(errorMessage)
    }
  }
}
