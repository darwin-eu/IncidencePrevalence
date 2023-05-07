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

#' Bind multiple incidence estimates into a single set of estimates
#'
#' @param ... Multiple incidence estimates, generated from `estimateIncidence()`
#'
#' @return Bound incidence estimates
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator"
#' )
#' inc1 <- estimateIncidence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' inc2 <- estimateIncidence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' incCombined <- bindIncidenceEstimates(inc1, inc2)
#' }
bindIncidenceEstimates <- function(...) {
  result <- list(...)
  label <- "IncidenceResult"
  checkNotEmpty(result, label)
  names <- deparse(substitute(list(...))) %>%
    {
      gsub("list\\(", "", .data)
    } %>%
    {
      gsub("\\)", "", .data)
    } %>%
    stringr::str_split(", ") %>%
    unlist()
  return(bindEstimates(result, names, label))
}

#' Bind multiple prevalence estimates into a single set of estimates
#'
#' @param ... Multiple prevalence estimates, generated from
#' `estimatePeriodPrevalence()` or `estimatePointPrevalence()`
#'
#' @return Bound prevalence estimates
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator"
#' )
#' prev1 <- estimatePeriodPrevalence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' prev2 <- estimatePointPrevalence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' prevCombined <- bindPrevalenceEstimates(prev1, prev2)
#' }
bindPrevalenceEstimates <- function(...) {
  result <- list(...)
  label <- "PrevalenceResult"
  checkNotEmpty(result, label)
  names <- deparse(substitute(list(...))) %>%
    {
      gsub("list\\(", "", .data)
    } %>%
    {
      gsub("\\)", "", .data)
    } %>%
    stringr::str_split(", ") %>%
    unlist()
  return(bindEstimates(result, names, label))
}

#' @noRd
bindEstimates <- function(result, names, label) {
  class1 <- checkClass(result, label)
  class2 <- checkClass(result, "IncidencePrevalenceResult")
  if (!all(class1 & class2)) {
    names <- names[!(class1 & class2)] %>% paste0(collapse = ", ")
    cli::cli_abort(paste0("Not valid ", label, "s: ", names))
  }
  settings <- dplyr::bind_rows(
    lapply(result, attr, which = "settings"),
    .id = "result_id"
  )
  attrition <- dplyr::bind_rows(
    lapply(result, attr, which = "attrition"),
    .id = "result_id"
  )
  result <- dplyr::bind_rows(result, .id = "result_id")
  attr(result, "settings") <- settings
  attr(result, "attrition") <- attrition
  return(result)
}

#' @noRd
checkClass <- function(result, classToCheck) {
  lapply(result, function(x) {
    classToCheck %in% class(x)
  }) %>%
    unlist()
}

#' @noRd
checkNotEmpty <- function(result, label) {
  if (length(result) == 0) {
    cli::cli_abort(paste0("No ", label, " provided"))
  }
}
