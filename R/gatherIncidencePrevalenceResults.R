#' Gather incidence and prevalence results
#'
#' @param resultList List of incididence and prevalence results
#' @param outcomeCohortId Vector of cohort id for outcomes cohort
#' @param outcomeCohortName Vector of names for outcomes cohort corresponding to
#' the outcomeCohortId
#' @param databaseName A database name to add to to the results
#'
#' @return A list of up to two tibbles, one with prevalence results and one
#' with incidence results. Note, where multiple results sets of the same type
#' have been passed to the function, the analysisId variable will have been
#' updated
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here ")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' dpop <- generateDenominatorCohortSet(
#'   cdm = cdm,
#'   startDate = as.Date("2008-01-01"),
#'   endDate = as.Date("2018-01-01")
#' )
#' cdm$denominator <- dpop$denominator_population
#' prev <- estimatePointPrevalence(
#' cdm = cdm,
#' denominatorTable = "denominator",
#' outcomeTable = "outcome"
#' )
#' inc <- estimateIncidence(
#'  cdm = cdm,
#'  denominatorTable = "denominator",
#'  outcomeTable = "outcome",
#'  interval = "months",
#'  outcomeWashout = 0
#'  )
#'  results <- gatherIncidencePrevalenceResults(resultList=list(prev, inc))
#' }
gatherIncidencePrevalenceResults <- function(resultList, outcomeCohortId = NULL,
                          outcomeCohortName = NULL, databaseName = NULL) {
  errorMessage <- checkmate::makeAssertCollection()
  objClass <- list()
  for (i in seq_along(resultList)) {
    objClass[[i]] <- inherits(resultList[[i]], "IncidencePrevalenceResult")
  }
  checkmate::assertTRUE(all(unlist(objClass) == TRUE),
    add = errorMessage
  )
  if (!isTRUE(all(unlist(objClass) == TRUE))) {
    errorMessage$push(
      "- resultList must only contain incidence or prevalence results"
    )
  }
  checkmate::assertCharacter(databaseName,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(outcomeCohortName,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertIntegerish(outcomeCohortId,
    add = errorMessage,
    null.ok = TRUE
  )
  if (!is.null(outcomeCohortId)) {
    checkmate::assertTRUE(length(outcomeCohortId) == length(outcomeCohortName))
  }
  checkmate::reportAssertions(collection = errorMessage)

  # create outcome ref
  if (!is.null(outcomeCohortId)) {
    outcomeRef <- tibble::tibble(
      outcome_cohort_id = .env$outcomeCohortId,
      outcome_cohort_name = .env$outcomeCohortName
    )
  }

  # add analysis settings to results
  estimates <- list()
  attrition <- list()
  for (i in seq_along(resultList)) {
    estimates[[i]] <- resultList[[i]] %>%
      dplyr::left_join(
        settings(resultList[[i]]) %>%
          dplyr::mutate(
            outcome_cohort_id =
              as.integer(.data$outcome_cohort_id)
          ),
        by = "analysis_id"
      )
    attrition[[i]] <- attrition(resultList[[i]]) %>%
      dplyr::left_join(
        settings(resultList[[i]]) %>%
          dplyr::mutate(
            outcome_cohort_id =
              as.integer(.data$outcome_cohort_id)
          ),
        by = "analysis_id"
      )
  }

  # combine results of same type (incidence or prevalence)
  resultType <- list()
  for (i in seq_along(resultList)) {
    if ("ir_100000_pys" %in% names(resultList[[i]])) {
      resultType[[i]] <- "Incidence"
    } else {
      resultType[[i]] <- "Prevalence"
    }
  }
  resultType <- unlist(resultType)


  if (any(resultType == "Prevalence")) {
    # combine prevalence estimates, updating analysis_id
    prevalence_estimates <- dplyr::bind_rows(
      estimates[resultType == "Prevalence"]
    ) %>%
      dplyr::group_by_at(dplyr::vars(tidyselect::starts_with(c(
        "analysis_", "denominator_",
        "outcome_"
      )))) %>%
      dplyr::mutate(analysis_id = dplyr::cur_group_id())
    # combine prevalence attrition
    prevalence_attrition <- dplyr::bind_rows(
      attrition[resultType == "Prevalence"]
    ) %>%
      dplyr::group_by_at(dplyr::vars(tidyselect::starts_with(c(
        "analysis_", "denominator_",
        "outcome_"
      )))) %>%
      dplyr::mutate(analysis_id = dplyr::cur_group_id())
    if (!is.null(outcomeCohortId)) {
      prevalence_estimates <- prevalence_estimates %>%
        dplyr::left_join(outcomeRef, by = "outcome_cohort_id")
      prevalence_attrition <- prevalence_attrition %>%
        dplyr::left_join(outcomeRef, by = "outcome_cohort_id")
    }
    if (!is.null(databaseName)) {
      prevalence_estimates <- prevalence_estimates %>%
        dplyr::mutate(database_name = .env$databaseName)
      prevalence_attrition <- prevalence_attrition %>%
        dplyr::mutate(database_name = .env$databaseName)
    }
  }

  # combine any incidence results, updating analysis_id
  if (any(resultType == "Incidence")) {
    incidence_estimates <- dplyr::bind_rows(
      estimates[resultType == "Incidence"]
    ) %>%
      dplyr::group_by_at(dplyr::vars(tidyselect::starts_with(c(
        "analysis_", "denominator_",
        "outcome_"
      )))) %>%
      dplyr::mutate(analysis_id = dplyr::cur_group_id()) %>%
      dplyr::mutate(database_name = .env$databaseName)
    # combine incidence attrition
    incidence_attrition <- dplyr::bind_rows(
      attrition[resultType == "Incidence"]
    ) %>%
      dplyr::group_by_at(dplyr::vars(tidyselect::starts_with(c(
        "analysis_", "denominator_",
        "outcome_"
      )))) %>%
      dplyr::mutate(analysis_id = dplyr::cur_group_id())

    if (!is.null(outcomeCohortId)) {
      incidence_estimates <- incidence_estimates %>%
        dplyr::left_join(outcomeRef, by = "outcome_cohort_id")
      incidence_estimates <- incidence_attrition %>%
        dplyr::left_join(outcomeRef, by = "outcome_cohort_id")
    }
    if (!is.null(databaseName)) {
      incidence_estimates <- incidence_estimates %>%
        dplyr::mutate(database_name = .env$databaseName)
      incidence_attrition <- incidence_attrition %>%
        dplyr::mutate(database_name = .env$databaseName)
    }
  }

  if (any(resultType == "Prevalence") & any(resultType == "Incidence")) {
    results <- list(
      prevalence_estimates = prevalence_estimates,
      prevalence_attrition = prevalence_attrition,
      incidence_estimates = incidence_estimates,
      incidence_attrition = incidence_attrition
    )
  } else if (any(resultType == "Prevalence")) {
    results <- list(
      prevalence_estimates = prevalence_estimates,
      prevalence_attrition = prevalence_attrition
    )
  } else {
    results <- list(
      incidence_estimates = incidence_estimates,
      incidence_attrition = incidence_attrition
    )
  }

  class(results) <- c("IncidencePrevalenceGatheredResult", class(results))

  return(results)
}
