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


#' Generate example database for IncidencePrevalence package
#'
#' @param person Person table.
#' @param observation_period observation_period table.
#' @param outcome outcome table.
#' @return DBIConnection to duckdb database with mock data
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(tibble)
#' library(IncidencePrevalence)
#' person_example <- tibble(
#'   person_id = "1",
#'   gender_concept_id = "8507",
#'   year_of_birth = 2000,
#'   month_of_birth = 06,
#'   day_of_birth = 01
#' )
#' observation_period_example <- tibble(
#'   observation_period_id = "1",
#'   person_id = "1",
#'   observation_period_start_date = as.Date("2010-01-01"),
#'   observation_period_end_date = as.Date("2015-06-01")
#' )
#' db <- generate_mock_incidence_prevalence_db(
#'   person = person_example,
#'   observation_period = observation_period_example
#' )
#' }
#'
generate_mock_incidence_prevalence_db <- function(person = NULL,
                                                  observation_period = NULL,
                                                  outcome = NULL) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_tibble(person, null.ok = TRUE)
  checkmate::assert_tibble(observation_period, null.ok = TRUE)
  checkmate::assert_tibble(outcome, null.ok = TRUE)
  checkmate::reportAssertions(collection = errorMessage)


  if (is.null(person)) {
    person <- tibble::tibble(
      person_id = "1",
      gender_concept_id = "8507",
      year_of_birth = 2000,
      month_of_birth = 06,
      day_of_birth = 01
    )
  }

  if (is.null(observation_period)) {
    observation_period <- tibble::tibble(
      observation_period_id = "1",
      person_id = "1",
      observation_period_start_date = as.Date("2010-01-01"),
      observation_period_end_date = as.Date("2015-06-01")
    )
  }

  if (is.null(outcome)) {
    outcome <- tibble::tibble(
      cohort_definition_id = "1",
      subject_id = "1",
      cohort_start_date = c(as.Date("2010-01-04")),
      cohort_end_date = c(as.Date("2010-01-04"))
    )
  }

  # into in-memory databse
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person",
      person,
      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation_period",
      observation_period,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "outcome",
      outcome,
      overwrite = TRUE
    )
  })


  return(db)
}
