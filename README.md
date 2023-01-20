
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IncidencePrevalence <a href='https://github.com/darwin-eu-dev/IncidencePrevalence'><img src='man/figures/hexsticker.png' align="right" height="185" /></a>

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
using the CDMConnector package.

``` r
library(CDMConnector)
library(IncidencePrevalence)
```

Creating a connection to a Postgres database would for example look
like:

``` r
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
                      host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
                      user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                      password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))

cdm <- CDMConnector::cdm_from_con(con, 
                    cdm_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"), 
                    write_schema = Sys.getenv("CDM5_POSTGRESQL_RESULT_SCHEMA"))
```

To see how you would create a reference to your database please consult
the CDMConnector package documentation. For this example though we´ll
work with simulated data, and we’ll generate an example cdm reference
like so:

``` r
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
#> # A tibble: 6 × 8
#>   cohort_definitio…¹ age_g…² sex   days_…³ start_date end_date   strat…⁴ strat…⁵
#>                <int> <chr>   <chr>   <dbl> <date>     <date>     <lgl>   <lgl>  
#> 1                  1 0;64    Male      180 2008-01-01 2018-01-01 NA      NA     
#> 2                  2 0;64    Fema…     180 2008-01-01 2018-01-01 NA      NA     
#> 3                  3 0;64    Both      180 2008-01-01 2018-01-01 NA      NA     
#> 4                  4 65;100  Male      180 2008-01-01 2018-01-01 NA      NA     
#> 5                  5 65;100  Fema…     180 2008-01-01 2018-01-01 NA      NA     
#> 6                  6 65;100  Both      180 2008-01-01 2018-01-01 NA      NA     
#> # … with abbreviated variable names ¹​cohort_definition_id, ²​age_group,
#> #   ³​days_prior_history, ⁴​strata_cohort_definition_id, ⁵​strata_cohort_name
```

These cohorts will be in the typical OMOP CDM structure

``` r
cdm$denominator
#> # Source:   table<dbplyr_008> [?? x 4]
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

### Instantiate an outcome cohort

Outcome cohorts for which we want to estimate incidence or prevalence
need to be created. If the outcome cohorts are defined as JSON, we can
use the CDMConnector package to read in and generate the cohorts.

``` r
outcome_cohorts <- CDMConnector::readCohortSet("path_to_outcome_cohort_definitions")
cdm <- CDMConnector::generateCohortSet(cdm = cdm, 
                                       cohortSet = outcome_cohorts,
                                       cohortTableName = "outcome_table_name"
                                       )
```

Note, in our example cdm reference we already have an outcome cohort
defined

``` r
cdm$outcome
#> # Source:   table<main.outcome> [?? x 4]
#> # Database: DuckDB 0.6.1 [eburn@Windows 10 x64:R 4.2.1/:memory:]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>    <chr>                <chr>      <date>            <date>         
#>  1 1                    1          2014-07-01        2014-07-09     
#>  2 1                    2          2018-06-25        2018-07-03     
#>  3 1                    3          2006-09-10        2006-09-18     
#>  4 1                    4          2010-10-06        2010-10-14     
#>  5 1                    5          2006-09-16        2006-09-24     
#>  6 1                    6          2019-06-08        2019-06-16     
#>  7 1                    7          2010-06-26        2010-07-04     
#>  8 1                    8          2008-09-21        2008-09-29     
#>  9 1                    9          2019-06-26        2019-07-04     
#> 10 1                    10         2019-03-21        2019-03-29     
#> # … with more rows
```

### Estimating incidence and prevalence

Now we have identified our denominator population, we can calculate
incidence and prevalence as below.

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
#> $ analysis_id                     <chr> "1", "1", "1", "1", "1", "1", "1", "1"…
#> $ n_persons                       <int> 194, 156, 174, 185, 192, 156, 165, 156…
#> $ person_days                     <dbl> 29026, 21658, 23094, 30744, 27909, 234…
#> $ n_events                        <int> 60, 62, 54, 62, 70, 51, 60, 53, 54, 40…
#> $ incidence_start_date            <date> 2008-01-01, 2009-01-01, 2010-01-01, 2…
#> $ incidence_end_date              <date> 2008-12-31, 2009-12-31, 2010-12-31, 2…
#> $ person_years                    <dbl> 79.46886, 59.29637, 63.22793, 84.17248…
#> $ incidence_100000_pys            <dbl> 75501.27, 104559.52, 85405.30, 73658.2…
#> $ incidence_100000_pys_95CI_lower <dbl> 57615.43, 80165.18, 64159.09, 56473.38…
#> $ incidence_100000_pys_95CI_upper <dbl> 97185.11, 134040.58, 111435.39, 94426.…
#> $ cohort_obscured                 <chr> "FALSE", "FALSE", "FALSE", "FALSE", "F…
#> $ result_obscured                 <chr> "FALSE", "FALSE", "FALSE", "FALSE", "F…
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
#> $ n_cases               <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ n_population          <int> 125, 102, 84, 103, 122, 86, 97, 102, 87, 86, 83,…
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
#> $ n_cases               <int> 11, 9, 8, NA, NA, NA, NA, NA, NA, NA, 14, 22, 22…
#> $ n_population          <int> 33, 22, 20, NA, NA, NA, NA, NA, NA, NA, 33, 38, …
#> $ prevalence            <dbl> 0.3333333, 0.4090909, 0.4000000, NA, NA, NA, NA,…
#> $ prevalence_95CI_lower <dbl> 0.1975023, 0.2325582, 0.2188065, NA, NA, NA, NA,…
#> $ prevalence_95CI_upper <dbl> 0.5039211, 0.6126518, 0.6134185, NA, NA, NA, NA,…
#> $ cohort_obscured       <chr> "FALSE", "FALSE", "FALSE", "TRUE", "TRUE", "TRUE…
#> $ result_obscured       <chr> "FALSE", "FALSE", "FALSE", "TRUE", "TRUE", "TRUE…
```

### Combining and exporting results

After running different analyses We can use
`gatherIncidencePrevalenceResults()` to bring together the results,
adding outcome names and the database name to the output.

``` r
study_results <- gatherIncidencePrevalenceResults(cdm=cdm,
                    resultList=list(inc, prev_point, prev_period),
                    databaseName = "my_database")
dplyr::glimpse(study_results$incidence_estimates)
#> Rows: 60
#> Columns: 28
#> $ analysis_id                             <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ n_persons                               <int> 194, 156, 174, 185, 192, 156, …
#> $ person_days                             <dbl> 29026, 21658, 23094, 30744, 27…
#> $ n_events                                <int> 60, 62, 54, 62, 70, 51, 60, 53…
#> $ incidence_start_date                    <date> 2008-01-01, 2009-01-01, 2010-…
#> $ incidence_end_date                      <date> 2008-12-31, 2009-12-31, 2010-…
#> $ person_years                            <dbl> 79.46886, 59.29637, 63.22793, …
#> $ incidence_100000_pys                    <dbl> 75501.27, 104559.52, 85405.30,…
#> $ incidence_100000_pys_95CI_lower         <dbl> 57615.43, 80165.18, 64159.09, …
#> $ incidence_100000_pys_95CI_upper         <dbl> 97185.11, 134040.58, 111435.39…
#> $ cohort_obscured                         <chr> "FALSE", "FALSE", "FALSE", "FA…
#> $ result_obscured                         <chr> "FALSE", "FALSE", "FALSE", "FA…
#> $ outcome_cohort_id                       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ outcome_cohort_name                     <lgl> NA, NA, NA, NA, NA, NA, NA, NA…
#> $ analysis_outcome_washout                <dbl> 180, 180, 180, 180, 180, 180, …
#> $ analysis_repeated_events                <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, …
#> $ analysis_interval                       <chr> "years", "years", "years", "ye…
#> $ analysis_complete_database_intervals    <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, …
#> $ denominator_cohort_id                   <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ analysis_min_cell_count                 <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ denominator_age_group                   <chr> "0;64", "0;64", "0;64", "0;64"…
#> $ denominator_sex                         <chr> "Male", "Male", "Male", "Male"…
#> $ denominator_days_prior_history          <dbl> 180, 180, 180, 180, 180, 180, …
#> $ denominator_start_date                  <date> 2008-01-01, 2008-01-01, 2008-…
#> $ denominator_end_date                    <date> 2018-01-01, 2018-01-01, 2018-…
#> $ denominator_strata_cohort_definition_id <lgl> NA, NA, NA, NA, NA, NA, NA, NA…
#> $ denominator_strata_cohort_name          <lgl> NA, NA, NA, NA, NA, NA, NA, NA…
#> $ database_name                           <chr> "my_database", "my_database", …
dplyr::glimpse(study_results$prevalence_estimates)
#> Rows: 126
#> Columns: 28
#> $ analysis_id                             <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
#> $ prevalence_start_date                   <date> 2008-01-01, 2009-01-01, 2010-…
#> $ prevalence_end_date                     <date> 2008-01-01, 2009-01-01, 2010-…
#> $ n_cases                                 <int> NA, NA, NA, NA, NA, NA, NA, NA…
#> $ n_population                            <int> 125, 102, 84, 103, 122, 86, 97…
#> $ prevalence                              <dbl> NA, NA, NA, NA, NA, NA, NA, NA…
#> $ prevalence_95CI_lower                   <dbl> NA, NA, NA, NA, NA, NA, NA, NA…
#> $ prevalence_95CI_upper                   <dbl> NA, NA, NA, NA, NA, NA, NA, NA…
#> $ cohort_obscured                         <chr> "FALSE", "FALSE", "FALSE", "FA…
#> $ result_obscured                         <chr> "TRUE", "TRUE", "TRUE", "TRUE"…
#> $ outcome_cohort_id                       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ outcome_cohort_name                     <lgl> NA, NA, NA, NA, NA, NA, NA, NA…
#> $ analysis_outcome_lookback_days          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ analysis_type                           <chr> "point", "point", "point", "po…
#> $ analysis_interval                       <chr> "years", "years", "years", "ye…
#> $ analysis_complete_database_intervals    <lgl> FALSE, FALSE, FALSE, FALSE, FA…
#> $ analysis_time_point                     <chr> "start", "start", "start", "st…
#> $ analysis_full_contribution              <lgl> FALSE, FALSE, FALSE, FALSE, FA…
#> $ analysis_min_cell_count                 <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ denominator_cohort_id                   <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ denominator_age_group                   <chr> "0;64", "0;64", "0;64", "0;64"…
#> $ denominator_sex                         <chr> "Male", "Male", "Male", "Male"…
#> $ denominator_days_prior_history          <dbl> 180, 180, 180, 180, 180, 180, …
#> $ denominator_start_date                  <date> 2008-01-01, 2008-01-01, 2008-…
#> $ denominator_end_date                    <date> 2018-01-01, 2018-01-01, 2018-…
#> $ denominator_strata_cohort_definition_id <lgl> NA, NA, NA, NA, NA, NA, NA, NA…
#> $ denominator_strata_cohort_name          <lgl> NA, NA, NA, NA, NA, NA, NA, NA…
#> $ database_name                           <chr> "my_database", "my_database", …
```

After gathering results, we can export them as CSVs in a zip folder
using the `exportIncidencePrevalenceResults()` function.

``` r
exportIncidencePrevalenceResults(result=study_results, 
                  zipName="example_results",
                  outputFolder=here::here()) 
```
