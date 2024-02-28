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

obscureCounts <- function(x,
                          minCellCount = 5,
                          substitute = NA) {
  ## check for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_true(tibble::is_tibble(x),
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(c("n_events", "incidence_100000_pys") %in% names(x)) ||
      all(c("n_cases", "prevalence") %in% names(x))
  )
  checkmate::assertFALSE(
    all(c("n_events", "person_months", "ir") %in% names(x)) &&
      all(c("n_cases", "n_population", "prevalence") %in% names(x))
  )
  checkmate::assert_numeric(minCellCount,
    add = errorMessage
  )
  checkmate::assertTRUE(is.numeric(substitute) || is.na(substitute))

  # report initial assertions
  checkmate::reportAssertions(collection = errorMessage)

  if (c("n_events") %in% names(x)) {
    # initialise result_obscurred as FALSE
    # will replace with true below if obscured
    x$cohort_obscured <- "FALSE"
    x$result_obscured <- "FALSE"

    x[x$n_persons < minCellCount & x$n_persons > 0, c("cohort_obscured")] <- "TRUE"
    x[x$n_persons < minCellCount & x$n_persons > 0, c(
      "n_persons",
      "person_days",
      "person_years"
    )] <- substitute
    x[x$n_events < minCellCount & x$n_events > 0, c("result_obscured")] <- "TRUE"
    x[x$n_events < minCellCount & x$n_events > 0, c(
      "n_events", "incidence_100000_pys",
      "incidence_100000_pys_95CI_lower",
      "incidence_100000_pys_95CI_upper"
    )] <- substitute
  }
  if (c("n_cases") %in% names(x)) {
    x$population_obscured <- "FALSE"
    x$cases_obscured <- "FALSE"
    x$result_obscured <- "FALSE"

    x[x$n_population < minCellCount & x$n_population > 0, c("population_obscured")] <- "TRUE"
    x[x$n_population < minCellCount & x$n_population > 0, c("n_population")] <- substitute
    x[x$n_cases < minCellCount & x$n_cases > 0, c("cases_obscured")] <- "TRUE"
    x[x$n_cases < minCellCount & x$n_cases > 0, c("n_cases")] <- substitute
    x[x$population_obscured == "TRUE" | x$cases_obscured== "TRUE", c("result_obscured")] <- "TRUE"
    x[x$population_obscured == "TRUE" | x$cases_obscured== "TRUE", c(
      "prevalence",
      "prevalence_95CI_lower",
      "prevalence_95CI_upper"
    )] <- substitute
  }

  return(x)
}


obscureAttrition <- function(x,
                            minCellCount = 5){
x <- x %>%
    dplyr::mutate(number_records = dplyr::if_else(.data$number_records < .env$minCellCount &
                                                    .data$number_records > 0,
                                                  "<5", as.character(.data$number_records)),
                  number_subjects = dplyr::if_else(.data$number_subjects < .env$minCellCount &
                                                     .data$number_subjects > 0,
                                                   "<5", as.character(.data$number_subjects)),
                  excluded_records = dplyr::if_else(.data$excluded_records < .env$minCellCount &
                                                      .data$excluded_records > 0,
                                                    "<5", as.character(.data$excluded_records)),
                  excluded_subjects = dplyr::if_else(.data$excluded_subjects < .env$minCellCount &
                                                       .data$excluded_subjects > 0,
                                                     "<5", as.character(.data$excluded_subjects)))

  }
