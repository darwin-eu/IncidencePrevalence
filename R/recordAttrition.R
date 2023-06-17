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


recordAttrition <- function(table,
                            id = "person_id",
                            existingAttrition = NULL,
                            reasonId,
                            reason) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(any(class(table) %in%
    c("tbl_dbi", "tbl", "data.frame", "tibble")))
  checkmate::assertCharacter(id, add = errorMessage)
  checkmate::assertIntegerish(reasonId, add = errorMessage)
  checkmate::assertCharacter(reason, null.ok = TRUE, add = errorMessage)
  if (!is.null(existingAttrition)) {
    checkmate::assertTRUE(any(class(existingAttrition) %in%
      c("data.frame", "tbl")))
  }
  checkmate::reportAssertions(collection = errorMessage)

  attrition <- dplyr::tibble(
    number_records = table %>%
      dplyr::tally() %>%
      dplyr::pull(),
    number_subjects = table %>%
      dplyr::select(.env$id) %>%
      dplyr::distinct() %>%
      dplyr::tally() %>%
      dplyr::pull(),
    reason_id = .env$reasonId,
    reason = .env$reason
  )

  if (!is.null(existingAttrition)) {
    attrition <- dplyr::bind_rows(existingAttrition, attrition) %>%
      dplyr::mutate(
        excluded_records =
          dplyr::lag(.data$number_records) - .data$number_records
      ) %>%
      dplyr::mutate(
        excluded_subjects =
          dplyr::lag(.data$number_subjects) - .data$number_subjects
      )
  } else {
    attrition <- attrition %>%
      dplyr::mutate(excluded_records = NA) %>%
      dplyr::mutate(excluded_subjects = NA)
  }

  return(attrition)
}
