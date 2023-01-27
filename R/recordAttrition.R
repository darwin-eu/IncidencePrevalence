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


#' Record attrition in tibble
#'
#' @param table table that contains a person_id
#' @param id id variable
#' @param reason the reason for the attrition
#' @param existingAttrition previous attrition to append results
#' @return a tibble
recordAttrition <- function(table,
                            id = "person_id",
                            reason = NULL,
                            existingAttrition = NULL) {

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(any(class(table) %in%
                              c("tbl_dbi", "tbl", "data.frame", "tibble")))
  checkmate::assert_character(id, add = errorMessage)
  checkmate::assert_character(reason, null.ok = TRUE, add = errorMessage)
  if (!is.null(existingAttrition)) {
    checkmate::assertTRUE(any(class(existingAttrition) %in%
                                c("data.frame", "tbl")))
  }
  checkmate::reportAssertions(collection = errorMessage)

  attrition <- tibble::tibble(
    current_n = table %>%
      dplyr::select(.env$id) %>%
      dplyr::distinct() %>%
      dplyr::tally() %>%
      dplyr::pull(),
    reason = reason
  )

  if (!is.null(existingAttrition)) {
    attrition <- dplyr::bind_rows(existingAttrition, attrition) %>%
      dplyr::mutate(excluded = dplyr::lag(.data$current_n) - .data$current_n)
  } else {
    attrition <- attrition %>%
      dplyr::mutate(excluded = NA)
  }

  return(attrition)
}
