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


#' Get confidence intervals
#'
#' @param x x
#' @param confidence_intervals Confidence intervals type
#'
#' @return
#' @export
#'
#' @examples
get_confidence_intervals <- function(x,
                                     confidence_intervals) {

  ## check for standard types of user error
  error_message <- checkmate::makeAssertCollection()

  checkmate::assert_tibble(x,
    add = error_message
  )
  checkmate::assertTRUE(
    all(c("n_events", "person_years", "ir_100000_pys") %in% names(x)) ||
      all(c("numerator", "denominator", "prev") %in% names(x))
  )
  checkmate::assertFALSE(
    all(c("n_events", "person_years", "ir_100000_pys") %in% names(x)) &&
      all(c("numerator", "denominator", "prev") %in% names(x))
  )
  checkmate::assertFALSE(
    any(c("num", "den", "var", "var_low", "var_high") %in% names(x))
  )

  checkmate::assert_choice(confidence_intervals,
    choices = c("poisson"), # Add binomial at some point
    add = error_message
  )

  checkmate::reportAssertions(collection = error_message)

  if (c("n_events") %in% names(x)) {
    x <- x %>%
      dplyr::rename("num" = "n_events") %>%
      dplyr::rename("den" = "person_years") %>%
      dplyr::rename("var" = "ir_100000_pys")
    type <- "Incidence"
  } else if (c("numerator") %in% names(x)) {
    x <- x %>%
      dplyr::rename("num" = "numerator") %>%
      dplyr::rename("den" = "denominator") %>%
      dplyr::rename("var" = "prev")
    type <- "Prevalence"
  }

  if (confidence_intervals == "poisson") {
    x <- dplyr::bind_cols(x,
                          x %>%
                            dplyr::left_join(
                              x %>%
                                dplyr::filter(.data$num >= 1) %>%
                                dplyr::mutate(var_low = stats::qchisq(0.05 / 2, df = 2 * (.data$num - 1)) / 2 / .data$den) %>%
                                dplyr::mutate(var_high = stats::qchisq(1 - 0.05 / 2, df = 2 * .data$num) / 2 / .data$den),
                              by = names(x)
                            ) %>%
                            dplyr::select("var_low", "var_high") %>%
                            dplyr::mutate(var_low = dplyr::if_else(is.na(.data$var_low), 0, .data$var_low)) %>%
                            dplyr::mutate(var_high = dplyr::if_else(is.na(.data$var_high), 0, .data$var_high))
    )
  } else if (confidence_intervals == "binomial") {
    x <- dplyr::bind_cols(x,
                          x %>%
                            dplyr::left_join(
                              x %>%
                                dplyr::filter(.data$num >= 1) %>%
                                dplyr::mutate(var_low = .data$var - qnorm(0.975)*sqrt(.data$var*(1-.data$var)/.data$den)) %>%
                                dplyr::mutate(var_low = dplyr::if_else(.data$var_low < 0, 0, .data$var_low)) %>%
                                dplyr::mutate(var_high = .data$var + qnorm(0.975)*sqrt(.data$var*(1-.data$var)/.data$den)) %>%
                                dplyr::mutate(var_high = dplyr::if_else(.data$var_high > 1, 1, .data$var_high)),
                              by = names(x)
                            ) %>%
                            dplyr::select("var_low", "var_high") %>%
                            dplyr::mutate(var_low = dplyr::if_else(is.na(.data$var_low), 0, .data$var_low)) %>%
                            dplyr::mutate(var_high = dplyr::if_else(is.na(.data$var_high), 0, .data$var_high))
    )
  }

  x <- x %>%
    dplyr::relocate(.data$var_low, .after = .data$var) %>%
    dplyr::relocate(.data$var_high, .after = .data$var_low)

  if (type == "Incidence"){
    x <- x %>%
      dplyr::rename("n_events" = "num") %>%
      dplyr::rename("person_years" = "den") %>%
      dplyr::rename("ir_100000_pys" = "var") %>%
      dplyr::rename("ir_100000_pys_low" = "var_low") %>%
      dplyr::rename("ir_100000_pys_high" = "var_high")
  } else if (type == "Prevalence"){
    x <- x %>%
      dplyr::rename("numerator" = "num") %>%
      dplyr::rename("denominator" = "den") %>%
      dplyr::rename("prev" = "var") %>%
      dplyr::rename("prev_low" = "var_low") %>%
      dplyr::rename("prev_high" = "var_high")
  }

  return(x)
}
