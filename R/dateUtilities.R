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
# limitations under the License.# utility function to support metaprogramming with date
addDaysQuery <- function(cdm,
                         variable, # name of the variable to use
                         number, # number of days or years, use a negative number to subtract da
                         type, # must be year or day
                         name_style) { # note, absolute values of numbers will be used

  if (!type %in% c("day", "year")) {
    cli::cli_abort("type must be day or year")
  }

  number <- as.integer(number)

  if (type == "day") {
    q <- glue::glue("as.Date(clock::add_days({variable} , {(number)}L))")
  }

  if (type == "year") {
    if (omopgenerics::sourceType(cdm) == "spark") {
      # https://github.com/darwin-eu-dev/IncidencePrevalence/issues/395
      number_days_to_years <- as.integer(number * 365)
      q <- glue::glue("as.Date(clock::add_days({variable}, {(number_days_to_years)}L))")
    } else {
      q <- glue::glue("as.Date(clock::add_years({variable}, {(number)}L))")
    }
  }

  q %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name_style))
}

minusDaysQuery <- function(cdm,
                           variable, # name of the variable to use
                           number, # number of days or years, use a negative number to subtract da
                           type, # must be year or day
                           name_style = NULL, # note, absolute values of numbers will be used
                           names = NULL) { # alternative to name_style, set of names

  if (!type %in% c("day", "year")) {
    cli::cli_abort("type must be day or year")
  }

  number <- as.integer(number)


  if (type == "day") {
    q <- glue::glue("as.Date(clock::add_days({variable} , {(number)}L))")
  }
  if (type == "year") {
    if (omopgenerics::sourceType(cdm) == "spark") {
      # https://github.com/darwin-eu-dev/IncidencePrevalence/issues/395
      number_days_to_years <- as.integer(number * 365)
      q <- glue::glue("as.Date(clock::add_days({variable}, {(number_days_to_years)}L))")
    } else {
      q <- glue::glue("as.Date(clock::add_years({variable} , {(number)}L))")
    }
  }

  q <- q %>%
    rlang::parse_exprs()

  if (!is.null(name_style)) {
    q <- q %>%
      rlang::set_names(glue::glue(name_style))
  }

  if (!is.null(names)) {
    q <- q %>%
      rlang::set_names(names)
  }

  q
}



# to solve note that "All declared Imports should be used."
redundant_fun <- function() {
  clock::add_days(as.Date("2000-01-01"), 1)
}
