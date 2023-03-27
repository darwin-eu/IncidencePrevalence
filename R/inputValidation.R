checkInputGenerateDCS <- function(cdm,
                                  startDate,
                                  endDate,
                                  ageGroup,
                                  sex,
                                  daysPriorHistory,
                                  strataTable,
                                  strataCohortId,
                                  strataCohortName,
                                  sample,
                                  tablePrefix,
                                  verbose) {

  cdmCheck(cdm)

  errorMessage <- checkmate::makeAssertCollection()
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
      "- sex must be from: Male, Female, and Both"
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
                                        tablePrefix,
                                        returnParticipants,
                                        verbose) {

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
  if(any(outcomeWashout != Inf)){
  checkmate::assert_numeric(outcomeWashout[which(!is.infinite(outcomeWashout))],
    add = errorMessage
  )
  }
  checkmate::assert_logical(repeatedEvents,
    add = errorMessage
  )
  checkmate::assert_number(minCellCount)
  checkmate::assertCharacter(tablePrefix,
    len = 1,
    add = errorMessage,
    null.ok = TRUE
  )
  if(is.null(tablePrefix)){
    # returnParticipants only when we are using permanent tables
    checkmate::assert_false(returnParticipants,
                            add = errorMessage
    )
  }
  checkmate::assert_logical(returnParticipants,
    add = errorMessage
  )
  checkmate::assert_logical(verbose,
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
  return(checkmate::reportAssertions(collection = errorMessage))

}

checkInputEstimatePrevalence <- function(cdm,
                                         denominatorTable,
                                         outcomeTable,
                                         denominatorCohortId,
                                         outcomeCohortId,
                                         outcomeLookbackDays,
                                         type,
                                         interval,
                                         completeDatabaseIntervals,
                                         fullContribution,
                                         timePoint,
                                         minCellCount,
                                         tablePrefix,
                                         returnParticipants,
                                         verbose){

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
  checkmate::assertIntegerish(outcomeCohortId,
                              add = errorMessage,
                              null.ok = TRUE
  )
  checkmate::assert_numeric(outcomeLookbackDays,
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
  checkmate::assert_logical(verbose,
                            add = errorMessage
  )
  checkmate::assert_logical(completeDatabaseIntervals,
                            add = errorMessage
  )
  checkmate::assertCharacter(tablePrefix,
                             len = 1,
                             add = errorMessage,
                             null.ok = TRUE
  )
  checkmate::assert_logical(returnParticipants,
                            add = errorMessage
  )
  if(is.null(tablePrefix)){
    # returnParticipants only when we are using permanent tables
    checkmate::assert_false(returnParticipants,
                            add = errorMessage
    )
  }
  return(checkmate::reportAssertions(collection = errorMessage))

}

cdmCheck <- function(cdm){
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
