
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

When working with IncidencePrevalence, you will use CDMConnector to
manage your connection to the database. If you don´t already have this
installed then you can install it in the same way:

``` r
remotes::install_github("darwin-eu/CDMConnector")
```

## Example

First, we need to create a cdm_reference for the data we´ll be using.
Here we´ll use generate an example one, but to see how you would set
this up for your database please consult the CDMConnector documentation
at <https://odyosg.github.io/CDMConnector/>

``` r
library(CDMConnector)
library(IncidencePrevalence)
#> Loading required package: lubridate
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union

# We first need to create a cdm_reference 
cdm<-generate_mock_incidence_prevalence_db(sample_size=5000)
# and this is what this example data looks like
head(cdm$person)
#> # Source:   SQL [6 x 5]
#> # Database: DuckDB 0.3.5-dev1410 [eburn@Windows 10 x64:R 4.1.3/:memory:]
#>   person_id gender_concept_id year_of_birth month_of_birth day_of_birth
#>   <chr>     <chr>                     <dbl>          <dbl>        <dbl>
#> 1 1         8532                       1926              8            2
#> 2 2         8507                       1961              5           16
#> 3 3         8507                       1922             10           18
#> 4 4         8507                       1985              2            2
#> 5 5         8507                       1932              9           23
#> 6 6         8532                       1960              9           25
head(cdm$observation_period)
#> # Source:   SQL [6 x 4]
#> # Database: DuckDB 0.3.5-dev1410 [eburn@Windows 10 x64:R 4.1.3/:memory:]
#>   observation_period_id person_id observation_period_start_date observation_per~
#>   <chr>                 <chr>     <date>                        <date>          
#> 1 1                     1         2013-12-18                    2014-12-30      
#> 2 2                     2         2018-05-09                    2019-05-16      
#> 3 3                     3         2006-05-07                    2007-02-02      
#> 4 4                     4         2010-08-25                    2011-02-12      
#> 5 5                     5         2006-09-08                    2007-02-11      
#> 6 6                     6         2017-10-21                    2020-05-16
head(cdm$outcome)
#> # Source:   SQL [6 x 4]
#> # Database: DuckDB 0.3.5-dev1410 [eburn@Windows 10 x64:R 4.1.3/:memory:]
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>   <chr>                <chr>      <date>            <date>         
#> 1 1                    1          2014-07-01        2014-07-09     
#> 2 1                    2          2018-06-25        2018-07-03     
#> 3 1                    3          2006-09-10        2006-09-18     
#> 4 1                    4          2010-10-06        2010-10-14     
#> 5 1                    5          2006-09-16        2006-09-24     
#> 6 1                    6          2019-06-08        2019-06-16
```

To identify our denominator population we can use the
`collect_denominator_pops` function. Here for example, we want to
identify a denominator population for a study period between 2008 and
2018. To note, other options ave available when defining this population
which are summarised in the package vignettes.

``` r
dpop <- collect_denominator_pops(
  cdm = cdm,
  study_start_date = as.Date("2008-01-01"),
  study_end_date = as.Date("2018-01-01")
)
# this is what the data looks like
head(dpop$denominator_population)
#> # Source:   SQL [6 x 4]
#> # Database: DuckDB 0.3.5-dev1410 [eburn@Windows 10 x64:R 4.1.3/:memory:]
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>   <chr>                <chr>      <date>            <date>         
#> 1 1                    1          2013-12-18        2014-12-30     
#> 2 1                    4          2010-08-25        2011-02-12     
#> 3 1                    6          2017-10-21        2018-01-01     
#> 4 1                    8          2008-09-01        2008-10-15     
#> 5 1                    11         2008-12-18        2010-08-13     
#> 6 1                    12         2008-05-28        2010-06-13
head(dpop$attrition)
#> # A tibble: 6 x 3
#>   current_n reason                                                      excluded
#>       <dbl> <chr>                                                          <dbl>
#> 1      5000 Starting population                                               NA
#> 2      5000 Missing year of birth                                              0
#> 3      5000 Missing sex                                                        0
#> 4      5000 Cannot satisfy age criteria during the study period based ~        0
#> 5      3811 No observation time available during study period               1189
#> 6      3811 Doesn't satisfy age criteria during the study period               0
```

Now we have identified our denominator population, we can calculate
incidence, point prevalence, and period prevalence as below (given that
our mock cdm_reference also has an outcome cohort defined). Again
further details for each of these functions are provided in the
vignettes.

To do this, first we´ll add the denominator cohort to our cdm reference.

``` r
cdm$denominator <- dpop$denominator_population
```

Now we´ve added the denominator cohort to our cdm reference, we can go
on and estimate incidence and prevalence.

``` r
# where the table with the outcome cohort is 
# in a table called ´outcome´
# schema called ´results´
inc <- collect_pop_incidence(
  cdm = cdm,
  table_name_denominator = "denominator",
  table_name_outcomes = "outcome",
  cohort_ids_outcomes = "1",
  cohort_ids_denominator_pops="1"
)
head(inc$incidence_estimates)
#> # A tibble: 6 x 13
#>   incidence_analysis_~ n_persons person_days n_events person_years ir_100000_pys
#>   <chr>                    <int>       <dbl>    <int>        <dbl>         <dbl>
#> 1 1                          511       14385       33         39.4        83790.
#> 2 1                          478       12834       30         35.1        85379.
#> 3 1                          463       12950       22         35.5        62050.
#> 4 1                          439       12292       28         33.7        83200.
#> 5 1                          421       11720       34         32.1       105960.
#> 6 1                          393       10504       29         28.8       100840.
#> # ... with 7 more variables: ir_100000_pys_low <dbl>, ir_100000_pys_high <dbl>,
#> #   time <chr>, start_time <date>, end_time <date>, cohort_obscured <chr>,
#> #   result_obscured <chr>
```

``` r
prev_point <- collect_pop_prevalence(
  cdm = cdm,
  table_name_denominator = "denominator",
  cohort_ids_denominator_pops="1",
  table_name_outcomes = "outcome",
  cohort_ids_outcomes = "1",
  time_intervals = "months",
  type = "point"
)
head(prev_point$prevalence_estimates)
#> # A tibble: 6 x 11
#>   prevalence_analysis_id time   numerator denominator    prev prev_low prev_high
#>   <chr>                  <chr>      <int>       <int>   <dbl> <lgl>    <lgl>    
#> 1 1                      2008_~         8         488  0.0164 NA       NA       
#> 2 1                      2008_~         6         481  0.0125 NA       NA       
#> 3 1                      2008_~        11         486  0.0226 NA       NA       
#> 4 1                      2008_~        NA         492 NA      NA       NA       
#> 5 1                      2008_~         7         488  0.0143 NA       NA       
#> 6 1                      2008_~         7         475  0.0147 NA       NA       
#> # ... with 4 more variables: start_time <date>, end_time <date>,
#> #   cohort_obscured <chr>, result_obscured <chr>
```

``` r
prev_period <- collect_pop_prevalence(
  cdm = cdm,
  table_name_denominator = "denominator",
  table_name_outcomes = "outcome",
  cohort_ids_outcomes = "1",
  cohort_ids_denominator_pops="1",
  time_intervals = "months",
  type = "period"
)
head(prev_period$prevalence_estimates)
#> # A tibble: 6 x 11
#>   prevalence_analysis_id time    numerator denominator   prev prev_low prev_high
#>   <chr>                  <chr>       <int>       <int>  <dbl> <lgl>    <lgl>    
#> 1 1                      2008_01        35         479 0.0731 NA       NA       
#> 2 1                      2008_02        35         485 0.0722 NA       NA       
#> 3 1                      2008_03        30         482 0.0622 NA       NA       
#> 4 1                      2008_04        31         497 0.0624 NA       NA       
#> 5 1                      2008_05        39         483 0.0807 NA       NA       
#> 6 1                      2008_06        34         465 0.0731 NA       NA       
#> # ... with 4 more variables: start_time <date>, end_time <date>,
#> #   cohort_obscured <chr>, result_obscured <chr>
```
