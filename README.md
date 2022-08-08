
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IncidencePrevalence

<!-- badges: start -->

[![codecov.io](https://codecov.io/github/darwin-eu/IncidencePrevalence/coverage.svg?branch=main)](https://codecov.io/github/darwin-eu/IncidencePrevalence?branch=main)
[![R-CMD-check](https://github.com/darwin-eu/IncidencePrevalence/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/IncidencePrevalence/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Package overview

IncidencePrevalence contains functions for estimating population-level
incidence and prevalence using the OMOP common data model.

## Package installation

You can install the development version of IncidencePrevalence like so:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu/IncidencePrevalence")
```

## Example

``` r
library(DBI)
library(RPostgres)
library(IncidencePrevalence)

db <- DBI::dbConnect(RPostgres::Postgres(),
  dbname = Sys.getenv("SERVER"),
  port = Sys.getenv("PORT"),
  host = Sys.getenv("HOST"),
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD")
)

# where the tables with patient data
# in the format of the OMOP common data model 
# are in a schema called ´cdm´ 
dpop <- collect_denominator_pops(
  db = db,
  cdm_database_schema = "cdm"
)

# where the table with the outcome cohort is 
# in a table called ´outcome´
# schema called ´results´
inc <- collect_pop_incidence(db,
  results_schema_outcome = "results",
  table_name_outcomes = "outcome",
  study_denominator_pop = dpop
)

prev <- collect_pop_prevalence(
  db = db,
  results_schema_outcome = "results",
  table_name_outcomes = "outcome",
  study_denominator_pop = dpop
)
```

## Results specification

### From collect_pop_incidence

Format: one row per time period per incidence analysis

| Variable                    | Description                                                                                             |
|-----------------------------|---------------------------------------------------------------------------------------------------------|
| incidence_analysis_id       | ID identifying an incidence analysis                                                                    |
| cohort_id_denominator_pop   | ID identifying the denominator population used in the incidence analysis                                |
| cohort_id_outcome           | ID identifying the outcome population used in the incidence analysis                                    |
| n_persons                   | Number of people contributing to the given time period                                                  |
| person_days                 | Number of person days contributed in the given time period                                              |
| person_months               | Number of person months contributed in the given time period                                            |
| person_years                | Number of person years contributed in the given time period                                             |
| n_events                    | Number of events months occurring in the given time period                                              |
| ir_100000_pys               | Incidence rate per 100,000 person-years                                                                 |
| ir_100000_pys_low           | Lower bound of the 95% confidence interval for the incidence rate per 100,000 person-years              |
| ir_100000_pys_high          | Upper bound of the 95% confidence interval for the incidence rate per 100,000 person-years              |
| calendar_month              | The calendar month of the given time period (NA if period is in years)                                  |
| calendar_year               | The calendar year of the given time period                                                              |
| required_days_prior_history | The number of days that were required for the given analysis                                            |
| age_strata                  | The age strata for the given analysis                                                                   |
| sex_strata                  | The sex strata for the given analysis                                                                   |
| outcome_washout_window      | The days required where an event was not seen prior to starting time at risk                            |
| repetitive_events           | Whether follow up was censored at occurrence of first event (true) or not (false) in the given analysis |
| time_interval               | The type of period being used in the given analysis (months or years)                                   |
| confidence_interval         | The method for calculating the confidence interval in the given analysis                                |

### From collect_pop_prevalence

Format: one row per time period per prevalence analysis

| Variable                          | Description                                                                                   |
|-----------------------------------|-----------------------------------------------------------------------------------------------|
| incidence_analysis_id             | ID identifying an prevalence analysis                                                         |
| cohort_id_denominator_pop         | ID identifying the denominator population used in the prevalence analysis                     |
| cohort_id_outcome                 | ID identifying the outcome population used in the prevalence analysis                         |
| numerator                         | Number of people in the numerator for calculating prevalence                                  |
| denominator                       | Number of people in the denominator for calculating prevalence                                |
| prev                              | Estimate of prevalence                                                                        |
| prev_low                          | Lower bound of the 95% confidence interval for the estimate of prevalence                     |
| prev_high                         | Upper bound of the 95% confidence interval for the estimate of prevalence                     |
| calendar_year                     | The calendar year of the given time period                                                    |
| required_days_prior_history       | The number of days that were required for the given analysis                                  |
| age_strata                        | The age strata for the given analysis                                                         |
| sex_strata                        | The sex strata for the given analysis                                                         |
| period                            | Type of period used for calculating prevalence (“point”, “month”, or “year”)                  |
| time_interval                     | The type of period being used in the given analysis (months or years)                         |
| confidence_interval               | The method for calculating the confidence interval in the given analysis                      |
| minimum_representative_proportion | The proportion of time in the period required for an individual to contribute to the analysis |
