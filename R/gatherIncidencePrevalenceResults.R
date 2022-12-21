#' Gather incidence and prevalence results
#'
#' @param cdm A CDM reference object
#' @param resultList List of incididence and prevalence results
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
gatherIncidencePrevalenceResults <- function(cdm, resultList, databaseName = NULL) {
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
    if ("incidence_100000_pys" %in% names(resultList[[i]])) {
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
      dplyr::mutate(analysis_id = dplyr::cur_group_id()) %>%
      dplyr::ungroup()
    # combine prevalence attrition
    prevalence_attrition <- dplyr::bind_rows(
      attrition[resultType == "Prevalence"]
    ) %>%
      dplyr::group_by_at(dplyr::vars(tidyselect::starts_with(c(
        "analysis_", "denominator_",
        "outcome_"
      )))) %>%
      dplyr::mutate(analysis_id = dplyr::cur_group_id()) %>%
      dplyr::ungroup()
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
      dplyr::mutate(database_name = .env$databaseName) %>%
      dplyr::ungroup()
    # combine incidence attrition
    incidence_attrition <- dplyr::bind_rows(
      attrition[resultType == "Incidence"]
    ) %>%
      dplyr::group_by_at(dplyr::vars(tidyselect::starts_with(c(
        "analysis_", "denominator_",
        "outcome_"
      )))) %>%
      dplyr::mutate(analysis_id = dplyr::cur_group_id()) %>%
      dplyr::ungroup()
    if (!is.null(databaseName)) {
      incidence_estimates <- incidence_estimates %>%
        dplyr::mutate(database_name = .env$databaseName)
      incidence_attrition <- incidence_attrition %>%
        dplyr::mutate(database_name = .env$databaseName)
    }
  }

  if (any(resultType == "Prevalence") && any(resultType == "Incidence")) {
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

  # add cdm snapshot to output
  results$cdm_snapshot <-  dplyr::as_tibble(do.call(cbind.data.frame,
                                                  CDMConnector::snapshot(cdm)))

  class(results) <- c("IncidencePrevalenceGatheredResult", class(results))

  return(results)
}
