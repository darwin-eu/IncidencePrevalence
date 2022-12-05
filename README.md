
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

First, we need to create a cdm reference for the data we´ll be using.
Here we´ll generate an example with simulated data, but to see how you
would set this up for your database please consult the CDMConnector
package documentation.

``` r
library(CDMConnector)
library(IncidencePrevalence)

# We first need to create a cdm_reference 
cdm<-mockIncidencePrevalenceRef(sampleSize=5000)
# this is what the person table for this example data looks like
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
```

To identify our denominator population we can use the
`generateDenominatorCohortSet` function. Here for example, we want to
identify a denominator population for a study period between 2008 and
2018. To note, other options ave available when defining this population
which are summarised in the package vignettes.

``` r
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2008-01-01"),
  endDate = as.Date("2018-01-01")
)
# this is what the resulting cohort data looks like
head(cdm$denominator)
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
```

We can also see the reasons people in the database did not enter the
cohort

``` r
attrition(cdm$denominator)
#> # A tibble: 7 x 3
#>   current_n reason                                                      excluded
#>       <dbl> <chr>                                                          <dbl>
#> 1      5000 Starting population                                               NA
#> 2      5000 Missing year of birth                                              0
#> 3      5000 Missing sex                                                        0
#> 4      5000 Cannot satisfy age criteria during the study period based ~        0
#> 5      3811 No observation time available during study period               1189
#> 6      3811 Doesn't satisfy age criteria during the study period               0
#> 7      3811 Prior history requirement not fullfilled during study peri~        0
```

Now we have identified our denominator population, we can calculate
incidence, point prevalence, and period prevalence as below (given that
our mock cdm reference also has an outcome cohort defined). Again
further details for each of these functions are provided in the
vignettes.

``` r
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome"
)
#> Warning: Missing values are always removed in SQL aggregation functions.
#> Use `na.rm = TRUE` to silence this warning
#> This warning is displayed once every 8 hours.
dplyr::glimpse(inc)
#> Rows: 120
#> Columns: 13
#> $ analysis_id        <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "~
#> $ n_persons          <int> 513, 480, 465, 441, 423, 395, 371, 366, 354, 345, 3~
#> $ person_days        <dbl> 14427, 12892, 13012, 12352, 11782, 10564, 10635, 10~
#> $ n_events           <int> 33, 30, 22, 28, 34, 29, 21, 25, 28, 25, 27, 27, 30,~
#> $ time               <chr> "2008_01", "2008_02", "2008_03", "2008_04", "2008_0~
#> $ start_time         <date> 2008-01-01, 2008-02-01, 2008-03-01, 2008-04-01, 20~
#> $ end_time           <date> 2008-01-31, 2008-02-29, 2008-03-31, 2008-04-30, 20~
#> $ person_years       <dbl> 39.49897, 35.29637, 35.62491, 33.81793, 32.25736, 2~
#> $ ir_100000_pys      <dbl> 83546.48, 84994.57, 61754.53, 82796.31, 105402.31, ~
#> $ ir_100000_pys_low  <dbl> 55414.04, 55024.79, 36489.44, 52614.60, 70420.16, 6~
#> $ ir_100000_pys_high <dbl> 114368.67, 117997.50, 90107.53, 116161.99, 143670.3~
#> $ cohort_obscured    <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE~
#> $ result_obscured    <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE~
```

``` r
prev_point <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "months"
)
dplyr::glimpse(prev_point)
#> Rows: 121
#> Columns: 11
#> $ analysis_id     <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",~
#> $ time            <chr> "2008_01", "2008_02", "2008_03", "2008_04", "2008_05",~
#> $ numerator       <dbl> 8, 6, 11, NA, 7, 7, 6, 6, 7, 9, 5, 7, 8, 9, 7, 9, 11, ~
#> $ denominator     <dbl> 488, 481, 486, 492, 488, 475, 466, 465, 460, 453, 458,~
#> $ prev            <dbl> 0.01639344, 0.01247401, 0.02263374, NA, 0.01434426, 0.~
#> $ prev_low        <dbl> 0.005127074, 0.002555355, 0.009410537, NA, 0.003794567~
#> $ prev_high       <dbl> 0.02765981, 0.02239267, 0.03585695, NA, 0.02489396, 0.~
#> $ start_time      <date> 2008-01-01, 2008-02-01, 2008-03-01, 2008-04-01, 2008-~
#> $ end_time        <date> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
#> $ cohort_obscured <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ~
#> $ result_obscured <chr> "FALSE", "FALSE", "FALSE", "TRUE", "FALSE", "FALSE", "~
```

``` r
prev_period <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "months"
)
dplyr::glimpse(prev_period)
#> Rows: 120
#> Columns: 11
#> $ analysis_id     <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",~
#> $ time            <chr> "2008_01", "2008_02", "2008_03", "2008_04", "2008_05",~
#> $ numerator       <dbl> 39, 36, 31, 32, 41, 34, 27, 30, 33, 33, 32, 32, 36, 36~
#> $ denominator     <dbl> 513, 509, 521, 513, 515, 505, 492, 493, 484, 483, 493,~
#> $ prev            <dbl> 0.07602339, 0.07072692, 0.05950096, 0.06237817, 0.0796~
#> $ prev_low        <dbl> 0.05308867, 0.04845522, 0.03918813, 0.04145056, 0.0562~
#> $ prev_high       <dbl> 0.09895812, 0.09299861, 0.07981379, 0.08330577, 0.1029~
#> $ start_time      <date> 2008-01-01, 2008-02-01, 2008-03-01, 2008-04-01, 2008-~
#> $ end_time        <date> 2008-01-31, 2008-02-29, 2008-03-31, 2008-04-30, 2008-~
#> $ cohort_obscured <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ~
#> $ result_obscured <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ~
```

For a result set we can also obtain extra related information.

We can see the attrition

``` r
attrition(prev_period)
#> # A tibble: 1 x 2
#>   analysis_id attrition
#>   <chr>       <chr>    
#> 1 1           attrition
```

We can also have a reference to the study participants that informed the
analysis

``` r
participants(prev_period)
#> $study_population_analyis_1
#> # Source:   SQL [?? x 3]
#> # Database: DuckDB 0.3.5-dev1410 [eburn@Windows 10 x64:R 4.1.3/:memory:]
#>    analysis_id subject_id cohort_start_date
#>    <chr>       <chr>      <date>           
#>  1 1           1          2013-12-18       
#>  2 1           4          2010-08-25       
#>  3 1           6          2017-10-21       
#>  4 1           8          2008-09-01       
#>  5 1           11         2008-12-18       
#>  6 1           12         2008-05-28       
#>  7 1           14         2009-10-19       
#>  8 1           15         2011-10-23       
#>  9 1           16         2010-01-30       
#> 10 1           17         2014-09-02       
#> # ... with more rows
```
