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


#' Get confidence intervals for incidence estimates
#'
#' @noRd
getCiIncidence <- function(ir,
                             mode) {
  if (mode == "poisson") {
    ir <- ir %>%
      dplyr::left_join(
        ir %>%
          dplyr::filter(.data$n_events >= 1) %>%
          dplyr::mutate(
            ir_100000_pys_low =
              100000 *
                stats::qchisq(0.05 / 2, df = 2 * (.data$n_events - 1))
                / 2 / .data$person_years
          ) %>%
          dplyr::mutate(
            ir_100000_pys_high =
              100000 *
                stats::qchisq(1 - 0.05 / 2, df = 2 * .data$n_events)
                / 2 / .data$person_years
          ),
        by = names(ir)
      )
  } else if (mode == "none") {
    ir <- ir %>%
      dplyr::mutate(ir_100000_pys_low = NA) %>%
      dplyr::mutate(ir_100000_pys_high = NA)
  }

  return(ir)
}

#' Get confidence intervals for prevalence estimates
#'
#' @noRd
getCiPrevalence <- function(pr,
                              mode) {
  if (mode == "binomial") {
    pr <- pr %>%
      dplyr::left_join(
        pr %>%
          dplyr::filter(.data$prev < 1 & .data$prev > 0) %>%
          dplyr::mutate(prev_low = .data$prev - stats::qnorm(0.975) *
            sqrt(.data$prev * (1 - .data$prev) / .data$denominator)) %>%
          dplyr::mutate(prev_low = dplyr::if_else(
            .data$prev_low < 0,
            0,
            .data$prev_low
          )) %>%
          dplyr::mutate(prev_high = .data$prev + stats::qnorm(0.975) *
            sqrt(.data$prev * (1 - .data$prev) / .data$denominator)) %>%
          dplyr::mutate(prev_high = dplyr::if_else(
            .data$prev_high > 1,
            1,
            .data$prev_high
          )),
        by = names(pr)
      )
  } else if (mode == "none") {
    pr <- pr %>%
      dplyr::mutate(prev_low = NA) %>%
      dplyr::mutate(prev_high = NA)
  }

  return(pr)
}
