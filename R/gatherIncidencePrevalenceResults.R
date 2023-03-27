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


#' Gather incidence and prevalence results
#'
#' @param cdm A CDM reference object
#' @param resultList List of incidence and prevalence results
#'
#' @return A list of up to two tibbles, one with prevalence results and one
#' with incidence results. Note, where multiple results sets of the same type
#' have been passed to the function, the analysisId variable will have been
#' updated
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
#' cdm$denominator <- generateDenominatorCohortSet(
#'   cdm = cdm,
#'   startDate = as.Date("2008-01-01"),
#'   endDate = as.Date("2018-01-01")
#' )
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
#'  results <- gatherIncidencePrevalenceResults(cdm = cdm,
#'                  resultList=list(prev, inc))
#' }
gatherIncidencePrevalenceResults <- function(cdm, resultList) {
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
  checkmate::reportAssertions(collection = errorMessage)

  # add analysis settings to results
  # add result_id (as each set of results will have analysis ids)
  estimates <- list()
  attrition <- list()
  for (i in seq_along(resultList)) {
   if(inherits(resultList[[i]], "IncidenceResult")){
      estimates[[i]] <- resultList[[i]] %>%
        dplyr::left_join(
          incidenceSet(resultList[[i]]) %>%
            dplyr::mutate(
              outcome_cohort_id =
                as.integer(.data$outcome_cohort_id)
            ),
          by = "analysis_id"
        ) %>%
        dplyr::mutate(result_id=i) %>%
        dplyr::relocate("result_id")
      attrition[[i]] <- incidenceAttrition(resultList[[i]]) %>%
        dplyr::left_join(
          incidenceSet(resultList[[i]]) %>%
            dplyr::mutate(
              outcome_cohort_id =
                as.integer(.data$outcome_cohort_id)
            ),
          by = "analysis_id"
        ) %>%
        dplyr::mutate(result_id=i) %>%
        dplyr::relocate("result_id")
    } else {
      estimates[[i]] <- resultList[[i]] %>%
        dplyr::left_join(
          prevalenceSet(resultList[[i]]) %>%
            dplyr::mutate(
              outcome_cohort_id =
                as.integer(.data$outcome_cohort_id)
            ),
          by = "analysis_id"
        ) %>%
        dplyr::mutate(result_id=i) %>%
        dplyr::relocate("result_id")
      attrition[[i]] <- prevalenceAttrition(resultList[[i]]) %>%
        dplyr::left_join(
          prevalenceSet(resultList[[i]]) %>%
            dplyr::mutate(
              outcome_cohort_id =
                as.integer(.data$outcome_cohort_id)
            ),
          by = "analysis_id"
        ) %>%
        dplyr::mutate(result_id=i) %>%
        dplyr::relocate("result_id")
    }

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
    # combine prevalence estimates
    prevalence_estimates <- dplyr::bind_rows(
      estimates[resultType == "Prevalence"]
    )
    # combine prevalence attrition
    prevalence_attrition <- dplyr::bind_rows(
      attrition[resultType == "Prevalence"]
    )
  }

  # combine any incidence results, updating analysis_id
  if (any(resultType == "Incidence")) {
    incidence_estimates <- dplyr::bind_rows(
      estimates[resultType == "Incidence"]
    )
    # combine incidence attrition
    incidence_attrition <- dplyr::bind_rows(
      attrition[resultType == "Incidence"]
    )
  }


  results<-list()
  if (any(resultType == "Prevalence")) {
    results[[paste0(c("prevalence_estimates"),
           collapse = "_")]] <- prevalence_estimates
    results[[paste0(c("prevalence_attrition"),
                    collapse = "_")]] <- prevalence_attrition
  }
  if (any(resultType == "Incidence")) {
    results[[paste0(c("incidence_estimates"),
                    collapse = "_")]] <- incidence_estimates
    results[[paste0(c("incidence_attrition"),
                    collapse = "_")]] <- incidence_attrition
  }

  # add cdm snapshot to output
  results[[paste0(c("cdm_snapshot"),
                  collapse = "_")]]<-  dplyr::as_tibble(do.call(cbind.data.frame,
                                                  CDMConnector::snapshot(cdm)))

  attr(results, "cdm_name") <- attr(cdm, "cdm_name")
  class(results) <- c("IncidencePrevalenceGatheredResult", class(results))

  return(results)
}
