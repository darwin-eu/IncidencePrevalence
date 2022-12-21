
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IncidencePrevalence <a href='https://github.com/darwin-eu-dev/IncidencePrevalence'><img src='man/figures/hexsticker.png' align="right" height="145" /></a>

<!-- badges: start -->

[![codecov.io](https://codecov.io/github/darwin-eu/IncidencePrevalence/coverage.svg?branch=main)](https://codecov.io/github/darwin-eu/IncidencePrevalence?branch=main)
[![R-CMD-check](https://github.com/darwin-eu/IncidencePrevalence/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/IncidencePrevalence/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Package overview

IncidencePrevalence contains functions for estimating population-level
incidence and prevalence using the OMOP common data model.

## Package installation

You can install the latest version of IncidencePrevalence like so:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu/IncidencePrevalence")
```

## Example usage

### Create a reference to data in the OMOP CDM format

The IncidencePrevalence package is designed to work with data in the
OMOP CDM format, so our first step is to create a reference to the data
using the CDMConnector package. Here we´ll generate an example reference
with simulated data (to see how you would create a reference to your
database please consult the CDMConnector package documentation).

``` r
library(CDMConnector)
library(IncidencePrevalence)

# We first need to create a cdm_reference 
cdm<-mockIncidencePrevalenceRef(sampleSize=5000)
```

### Identify a denominator cohort

To identify a set of denominator cohorts we can use the
`generateDenominatorCohortSet` function. Here we want to identify
denominator populations for a study period between 2008 and 2018 and
with 180 days of prior history (observation time in the database). We
also wish to consider multiple age groups (from 0 to 64, and 65 to 100)
and multiple sex criteria (one cohort only males, one only females, and
one with both sexes included).

``` r
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2008-01-01"),
  endDate = as.Date("2018-01-01"),
  ageGroup = list(c(0,64),
                  c(65,100)),
  sex = c("Male", "Female", "Both"),
  daysPriorHistory = 180
)
```

This will then give us six denominator cohorts

``` r
settings(cdm$denominator)
#> # A tibble: 6 × 6
#>   cohort_definition_id age_group sex    days_prior_history start_date end_date  
#>                  <int> <chr>     <chr>               <dbl> <date>     <date>    
#> 1                    1 0;64      Male                  180 2008-01-01 2018-01-01
#> 2                    2 0;64      Female                180 2008-01-01 2018-01-01
#> 3                    3 0;64      Both                  180 2008-01-01 2018-01-01
#> 4                    4 65;100    Male                  180 2008-01-01 2018-01-01
#> 5                    5 65;100    Female                180 2008-01-01 2018-01-01
#> 6                    6 65;100    Both                  180 2008-01-01 2018-01-01
```

These cohorts will be in the typical OMOP CDM structure

``` r
cdm$denominator
#> # Source:   table<dbplyr_009> [?? x 4]
#> # Database: DuckDB 0.6.1 [eburn@Windows 10 x64:R 4.2.1/:memory:]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int> <chr>      <date>            <date>         
#>  1                    1 14         2010-04-17        2010-11-27     
#>  2                    1 15         2012-04-20        2012-08-08     
#>  3                    1 17         2015-03-01        2015-10-06     
#>  4                    1 25         2010-01-05        2010-02-11     
#>  5                    1 26         2010-11-03        2012-07-07     
#>  6                    1 30         2017-05-07        2017-10-30     
#>  7                    1 31         2011-09-26        2012-09-02     
#>  8                    1 42         2008-01-01        2008-02-18     
#>  9                    1 55         2008-05-24        2009-05-02     
#> 10                    1 56         2012-07-24        2013-11-02     
#> # … with more rows
```

### Estimating incidence and prevalence

Now we have identified our denominator population, we can calculate
incidence and prevalence as below. Note, in our example cdm reference we
already have an outcome cohort defined as for this package outcome
cohorts are required to have been defined externally.

For this example we´ll estimate incidence on a yearly basis, allowing
individuals to have multiple events but with an outcome washout of 180
days. We also require that only complete database intervals are
included, by which we mean that the database must have individuals
observed throughout a year for that year to be included in the analysis.
Note, we also specify a minimum cell count of 5, under which estimates
will be obscured.

``` r
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  repeatedEvents = TRUE,
  outcomeWashout = 180, 
  completeDatabaseIntervals = TRUE,
  minCellCount = 5
  )
dplyr::glimpse(inc)
#> Rows: 60
#> Columns: 12
#> $ analysis_id              <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", …
#> $ n_persons                <int> 194, 156, 174, 185, 192, 156, 165, 156, 141, …
#> $ person_days              <dbl> 29026, 21658, 23094, 30744, 27909, 23477, 258…
#> $ n_events                 <int> 60, 62, 54, 62, 70, 51, 60, 53, 54, 40, 56, 7…
#> $ incidence_start_date     <date> 2008-01-01, 2009-01-01, 2010-01-01, 2011-01-…
#> $ incidence_end_date       <date> 2008-12-31, 2009-12-31, 2010-12-31, 2011-12-…
#> $ person_years             <dbl> 79.46886, 59.29637, 63.22793, 84.17248, 76.41…
#> $ ir_100000_pys            <dbl> 75501.27, 104559.52, 85405.30, 73658.27, 9161…
#> $ ir_100000_pys_95CI_lower <dbl> 57615.43, 80165.18, 64159.09, 56473.38, 71414…
#> $ ir_100000_pys_95CI_upper <dbl> 97185.11, 134040.58, 111435.39, 94426.58, 115…
#> $ cohort_obscured          <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", …
#> $ result_obscured          <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", …
```

We could also estimate point prevalence, as of the start of each
calendar year like so:

``` r
prev_point <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  timePoint = "start",
  minCellCount = 5
)
dplyr::glimpse(prev_point)
#> Rows: 66
#> Columns: 10
#> $ analysis_id           <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"…
#> $ prevalence_start_date <date> 2008-01-01, 2009-01-01, 2010-01-01, 2011-01-01,…
#> $ prevalence_end_date   <date> 2008-01-01, 2009-01-01, 2010-01-01, 2011-01-01,…
#> $ n_cases               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ n_population          <dbl> 125, 102, 84, 103, 122, 86, 97, 102, 87, 86, 83,…
#> $ prevalence            <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ prevalence_95CI_lower <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ prevalence_95CI_upper <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ cohort_obscured       <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FA…
#> $ result_obscured       <chr> "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", …
```

And annual period prevalence where we again require complete database
intervals and, in addition, only include those people who are observed
in the data for the full year:

``` r
prev_period <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  completeDatabaseIntervals = TRUE, 
  fullContribution = TRUE,
  minCellCount = 5
)
dplyr::glimpse(prev_period)
#> Rows: 60
#> Columns: 10
#> $ analysis_id           <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"…
#> $ prevalence_start_date <date> 2008-01-01, 2009-01-01, 2010-01-01, 2011-01-01,…
#> $ prevalence_end_date   <date> 2008-12-31, 2009-12-31, 2010-12-31, 2011-12-31,…
#> $ n_cases               <dbl> 11, 9, 8, 19, 9, 9, 19, 11, 10, 10, 14, 22, 22, …
#> $ n_population          <dbl> 33, 22, 20, 42, 26, 27, 35, 24, 26, 21, 33, 38, …
#> $ prevalence            <dbl> 0.3333333, 0.4090909, 0.4000000, 0.4523810, 0.34…
#> $ prevalence_95CI_lower <dbl> 0.1975023, 0.2325582, 0.2188065, 0.3122339, 0.19…
#> $ prevalence_95CI_upper <dbl> 0.5039211, 0.6126518, 0.6134185, 0.6005088, 0.53…
#> $ cohort_obscured       <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FA…
#> $ result_obscured       <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FA…
```

### Combining and exporting results

After running different analyses We can use
`gatherIncidencePrevalenceResults()` to bring together the results,
adding outcome names and the database name to the output.

``` r
study_results <- gatherIncidencePrevalenceResults(
                    resultList=list(inc, prev_point, prev_period),
                    outcomeCohortId = 1,
                    outcomeCohortName = "example_outcome",
                    databaseName = "example_data")
dplyr::glimpse(study_results$incidence_estimates)
#> Rows: 60
#> Columns: 26
#> $ analysis_id                          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, …
#> $ n_persons                            <int> 194, 156, 174, 185, 192, 156, 165…
#> $ person_days                          <dbl> 29026, 21658, 23094, 30744, 27909…
#> $ n_events                             <int> 60, 62, 54, 62, 70, 51, 60, 53, 5…
#> $ incidence_start_date                 <date> 2008-01-01, 2009-01-01, 2010-01-…
#> $ incidence_end_date                   <date> 2008-12-31, 2009-12-31, 2010-12-…
#> $ person_years                         <dbl> 79.46886, 59.29637, 63.22793, 84.…
#> $ ir_100000_pys                        <dbl> 75501.27, 104559.52, 85405.30, 73…
#> $ ir_100000_pys_95CI_lower             <dbl> 57615.43, 80165.18, 64159.09, 564…
#> $ ir_100000_pys_95CI_upper             <dbl> 97185.11, 134040.58, 111435.39, 9…
#> $ cohort_obscured                      <chr> "FALSE", "FALSE", "FALSE", "FALSE…
#> $ result_obscured                      <chr> "FALSE", "FALSE", "FALSE", "FALSE…
#> $ analysis_outcome_washout             <dbl> 180, 180, 180, 180, 180, 180, 180…
#> $ analysis_repeated_events             <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRU…
#> $ analysis_interval                    <chr> "years", "years", "years", "years…
#> $ analysis_complete_database_intervals <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRU…
#> $ outcome_cohort_id                    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ denominator_cohort_id                <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, …
#> $ analysis_min_cell_count              <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ denominator_age_group                <chr> "0;64", "0;64", "0;64", "0;64", "…
#> $ denominator_sex                      <chr> "Male", "Male", "Male", "Male", "…
#> $ denominator_days_prior_history       <dbl> 180, 180, 180, 180, 180, 180, 180…
#> $ denominator_start_date               <date> 2008-01-01, 2008-01-01, 2008-01-…
#> $ denominator_end_date                 <date> 2018-01-01, 2018-01-01, 2018-01-…
#> $ database_name                        <chr> "example_data", "example_data", "…
#> $ outcome_cohort_name                  <chr> "example_outcome", "example_outco…
dplyr::glimpse(study_results$prevalence_estimates)
#> Rows: 126
#> Columns: 26
#> $ analysis_id                          <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
#> $ prevalence_start_date                <date> 2008-01-01, 2009-01-01, 2010-01-…
#> $ prevalence_end_date                  <date> 2008-01-01, 2009-01-01, 2010-01-…
#> $ n_cases                              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ n_population                         <dbl> 125, 102, 84, 103, 122, 86, 97, 1…
#> $ prevalence                           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ prevalence_95CI_lower                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ prevalence_95CI_upper                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ cohort_obscured                      <chr> "FALSE", "FALSE", "FALSE", "FALSE…
#> $ result_obscured                      <chr> "TRUE", "TRUE", "TRUE", "TRUE", "…
#> $ outcome_cohort_id                    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ denominator_cohort_id                <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ analysis_outcome_lookback_days       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ analysis_type                        <chr> "point", "point", "point", "point…
#> $ analysis_interval                    <chr> "years", "years", "years", "years…
#> $ analysis_complete_database_intervals <lgl> FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ analysis_time_point                  <chr> "start", "start", "start", "start…
#> $ analysis_full_contribution           <lgl> FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ analysis_min_cell_count              <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ denominator_age_group                <chr> "0;64", "0;64", "0;64", "0;64", "…
#> $ denominator_sex                      <chr> "Male", "Male", "Male", "Male", "…
#> $ denominator_days_prior_history       <dbl> 180, 180, 180, 180, 180, 180, 180…
#> $ denominator_start_date               <date> 2008-01-01, 2008-01-01, 2008-01-…
#> $ denominator_end_date                 <date> 2018-01-01, 2018-01-01, 2018-01-…
#> $ outcome_cohort_name                  <chr> "example_outcome", "example_outco…
#> $ database_name                        <chr> "example_data", "example_data", "…
```

After gathering results, we can export them as CSVs in a zip folder
using the `exportIncidencePrevalenceResults()` function.

``` r
exportIncidencePrevalenceResults(result=study_results, 
                  zipName="example_results",
                  outputFolder=here::here()) 
```
