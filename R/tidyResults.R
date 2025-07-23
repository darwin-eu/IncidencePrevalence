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
#'
#' A tidy implementation of the summarised_result object for incidence results.
#'
#' @param result A summarised_result object created by the IncidencePrevalence package.
#' @param metadata If TRUE additional metadata columns will be included in the result.
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence()
#' inc <- estimateIncidence(cdm, "target", "outcome")
#' tidy_inc <- asIncidenceResult(inc)
#' }
#'
#' @return A tibble with a tidy version of the summarised_result object.
#'
#' @export
#'
asIncidenceResult <- function(result, metadata = FALSE) {
  if (nrow(result) == 0) {
    cli::cli_warn("No results available to tidy")
    return(result)
  }
  result <- omopgenerics::validateResultArgument(result)
  result <- result |>
    omopgenerics::filterSettings(
      .data$result_type %in% c("incidence", "incidence_attrition")
    )
  if (nrow(result) == 0) {
    cli::cli_warn("No incidence results found in result object")
    return(result)
  }

  incResults <- tidyResult(result, type = "incidence", attrition = FALSE, metadata = metadata) |>
    dplyr::select(!"result_id")

  class(incResults) <- c("tidy_incidence", class(incResults))

  incResults

}

#' A tidy implementation of the summarised_result object for prevalence results.
#'
#' @param result A summarised_result object created by the IncidencePrevalence package.
#' @param metadata If TRUE additional metadata columns will be included in the result.
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence()
#' prev <- estimatePointPrevalence(cdm, "target", "outcome")
#' tidy_prev <- asPrevalenceResult(prev)
#' }
#'
#' @return A tibble with a tidy version of the summarised_result object.
#'
#' @export
#'
asPrevalenceResult <- function(result, metadata = FALSE) {
  result <- omopgenerics::validateResultArgument(result)
  if (nrow(result) == 0) {
    cli::cli_warn("No prevalence results available to tidy")
    return(result)
  }
  result <- result |>
    omopgenerics::filterSettings(
      .data$result_type %in% c("prevalence", "prevalence_attrition")
    )
  if (nrow(result) == 0) {
    cli::cli_warn("No prevalence results found in result object")
    return(result)
  }

  prevResults <- tidyResult(result, type = "prevalence", attrition = FALSE, metadata = metadata)

  class(prevResults) <- c("tidy_prevalence", class(prevResults))

  prevResults |>
    dplyr::select(!"result_id")

}


# Helper function
tidyResult <- function(result, type, attrition = TRUE, metadata = FALSE) {
  result_estimates <- result |>
    omopgenerics::filterSettings(
      .data$result_type == .env$type
    )
  if(nrow(result_estimates) == 0){
    return(result_estimates)
  }
  result_estimates <- result_estimates |>
    omopgenerics::addSettings(settingsColumn =
                                omopgenerics::settingsColumns(result_estimates, metadata = metadata)) |>
    omopgenerics::splitAll() |>
    dplyr::select(!c("variable_name", "variable_level")) |>
    omopgenerics::pivotEstimates() |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::ends_with("_date"),
        .fns = ~ toDate(.x)
      ),
      dplyr::across(
        .cols = dplyr::contains("time|washout|days"),
        .fns = ~ as.numeric(.x)
      )
    ) |>
    dplyr::mutate(result_type = paste0("tidy_", type))
  if (attrition) {
    result_attrition <- result |>
      omopgenerics::filterSettings(
        .data$result_type == paste0(.env$type, "_attrition")
      ) |>
      omopgenerics::addSettings() |>
      omopgenerics::splitAll() |>
      dplyr::select(!c("variable_level")) |>
      omopgenerics::pivotEstimates() |>
      dplyr::relocate("count", .before = "reason_id") |>
      dplyr::mutate(result_type = paste0("tidy_", .env$type, "_attrition"))
    attr(result_estimates, "attrition") <- result_attrition
  }
  return(result_estimates)
}
