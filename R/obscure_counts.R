# Copyright 2022 DARWIN EU®
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


#' Obscure the small number of counts
#'
#' @param x x
#' @param minimum_cell_count minimum_cell_count
#' @param substitute substitute
#'
#' @return
#' @export
#'
#' @examples
obscure_counts <- function(x,
                           minimum_cell_count = 5,
                           substitute = NA) {

  ## check for standard types of user error
  error_message <- checkmate::makeAssertCollection()

  checkmate::assert_tibble(x,
    add = error_message
  )
  checkmate::assertTRUE(
    all(c("n_events", "ir_100000_pys", "ir_100000_pys_low",
          "ir_100000_pys_high") %in% names(x)) ||
      all(c("numerator", "prev", "prev_low", "prev_high") %in% names(x))
  )
  checkmate::assertFALSE(
    all(c("n_events", "person_months", "ir") %in% names(x)) &&
      all(c("numerator", "denominator", "prev") %in% names(x))
  )

  checkmate::assert_numeric(minimum_cell_count,
    add = error_message
  )

  checkmate::assertTRUE(is.numeric(substitute) || is.na(substitute))

  # report initial assertions
  checkmate::reportAssertions(collection = error_message)

  # initialise result_obscurred as FALSE
  # will replace with true below if obscured
  x$cohort_obscured <- "FALSE"
  x$result_obscured <- "FALSE"

  if (c("n_events") %in% names(x)) {
    x[x$n_persons < minimum_cell_count, c("cohort_obscured")] <- "TRUE"
    x[x$n_events < minimum_cell_count, c("n_persons",
                                         "person_days",
                                         "person_years")] <- substitute
    x[x$n_events < minimum_cell_count, c("result_obscured")] <- "TRUE"
    x[x$n_events < minimum_cell_count, c("n_events", "ir_100000_pys",
                                     "ir_100000_pys_low",
                                     "ir_100000_pys_high")] <- substitute
  }
  if (c("numerator") %in% names(x)) {
    x[x$denominator < minimum_cell_count, c("cohort_obscured")] <- "TRUE"
    x[x$denominator < minimum_cell_count, c("denominator")] <- substitute
    x[x$numerator < minimum_cell_count, c("result_obscured")] <- "TRUE"
    x[x$numerator < minimum_cell_count, c("numerator",
                                      "prev",
                                      "prev_low", "prev_high")] <- substitute
  }

  return(x)
}
