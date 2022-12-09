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
gatherResults <- function(resultList, outcomeCohortId=NULL,
                          outcomeCohortName=NULL, databaseName=NULL){
  errorMessage <- checkmate::makeAssertCollection()
  objClass<-list()
  for(i in seq_along(resultList)){
    objClass[[i]] <- inherits(resultList[[i]], "IncidencePrevalenceResult")
  }
  checkmate::assertTRUE(all(unlist(objClass)==TRUE),
                        add = errorMessage)
  if (!isTRUE(all(unlist(objClass)==TRUE))) {
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
  if(!is.null(outcomeCohortId)){
    checkmate::assertTRUE(length(outcomeCohortId)==length(outcomeCohortName))
  }
  checkmate::reportAssertions(collection = errorMessage)

  # create outcome ref
  if(!is.null(outcomeCohortId)){
    outcomeRef <- tibble::tibble(outcome_cohort_id =outcomeCohortId,
                                 outcome_cohort_name=outcomeCohortName)
  }

  # add analysis settings to results
  for(i in seq_along(resultList)){
    resultList[[i]]<- resultList[[i]] %>%
      dplyr::left_join(settings(resultList[[i]]) %>%
                          dplyr::mutate(outcome_cohort_id=
                                          as.integer(outcome_cohort_id)),
                        by = "analysis_id")
  }

  # combine results of same type (incidence or prevalence)
  resultType<-list()
  for(i in seq_along(resultList)){
    if("ir_100000_pys" %in% names(resultList[[i]])){
      resultType[[i]]<-"Incidence"
    } else{
      resultType[[i]]<-"Prevalence"
    }
  }
  resultType<-unlist(resultType)

  # combine prevalence estimates, updating analysis_id
  if(any(resultType=="Prevalence")){
    prevalence <- dplyr::bind_rows(resultList[resultType=="Prevalence"]) %>%
      dplyr::group_by_at(dplyr::vars(starts_with(c("analysis_", "denominator_",
                                                   "outcome_")))) %>%
      dplyr::mutate(analysis_id = dplyr::cur_group_id())
    if(!is.null(outcomeCohortId)){
      prevalence <- prevalence %>%
        dplyr::left_join(outcomeRef, by="outcome_cohort_id")
    }
    if(!is.null(databaseName)){
      prevalence <- prevalence %>%
        dplyr::mutate(database_name = .env$databaseName)}
  }

  # combine any incidence results, updating analysis_id
  if(any(resultType=="Incidence")){
    incidence <- dplyr::bind_rows(resultList[resultType=="Incidence"]) %>%
      dplyr::group_by_at(dplyr::vars(starts_with(c("analysis_", "denominator_",
                                                   "outcome_")))) %>%
      dplyr::mutate(analysis_id = dplyr::cur_group_id()) %>%
      dplyr::mutate(database_name = .env$databaseName)
    if(!is.null(outcomeCohortId)){
      incidence <- incidence %>%
        dplyr::left_join(outcomeRef, by="outcome_cohort_id")
    }
    if(!is.null(databaseName)){
      incidence <- incidence %>%
        dplyr::mutate(database_name = .env$databaseName)}
  }

  if(any(resultType=="Prevalence") & any(resultType=="Incidence")){
    results<- list(prevalence_estimates=prevalence,
                   incidence_estimates=incidence)
  } else if(any(resultType=="Prevalence")){
    results<- list(prevalence_estimates=prevalence)
  } else {
    results<- list(incidence_estimates=incidence)
  }

  return(results)

}
