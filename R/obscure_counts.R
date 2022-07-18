# Copyright 2022 DARWIN EU (C)
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


#' Get population incidence estimates
#'
#' @param x x
#' @param minimum_counts minimum_counts
#' @param substitute substitute
#'
#' @return
#' @export
#'
#' @examples
obscure_counts <- function(x,
                           minimum_counts = 5,
                           substitute = NA) {

  ## check for standard types of user error
  error_message <- checkmate::makeAssertCollection()

  checkmate::assert_tibble(x,
                           add = error_message
  )
  checkmate::assertTRUE(
    all(c("n_events","ir","ir_low","ir_high") %in% names(x))
  ||
    all(c("numerator","prev","prev_low","prev_high") %in% names(x))
    )

  checkmate::assert_numeric(minimum_counts,
                            add = error_message
  )

  checkmate::assertTRUE( is.numeric(substitute) || is.na(substitute) )

  # checkmate::assertTRUE(
  #   all(c("n_events","ir","ir_low","ir_high") %in% names(x))
  #   ||
  #     all(c("numerator","prev","prev_low","prev_high") %in% names(x))
  # )

  # report initial assertions
  checkmate::reportAssertions(collection = error_message)

  if(c("n_events") %in% names(x)){
    x[x$n_events < minimum_counts,c("n_events","ir","ir_low","ir_high")] <- substitute
  }
  if(c("numerator") %in% names(x)){
    x[x$numerator < minimum_counts,c("numerator","prev","prev_low","prev_high")] <- substitute
  }

  return(x)

}
