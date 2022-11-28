
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
# and this is what this example data looks like
head(cdm$person)
#> # Source:   SQL [6 x 5]
#> # Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-132-generic:R 4.1.2/:memory:]
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
#> # Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-132-generic:R 4.1.2/:memory:]
#>   observation_period_id person_id observation_period_start_date observation_pe…¹
#>   <chr>                 <chr>     <date>                        <date>          
#> 1 1                     1         2013-12-18                    2014-12-30      
#> 2 2                     2         2018-05-09                    2019-05-16      
#> 3 3                     3         2006-05-07                    2007-02-02      
#> 4 4                     4         2010-08-25                    2011-02-12      
#> 5 5                     5         2006-09-08                    2007-02-11      
#> 6 6                     6         2017-10-21                    2020-05-16      
#> # … with abbreviated variable name ¹​observation_period_end_date
head(cdm$outcome)
#> # Source:   SQL [6 x 4]
#> # Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-132-generic:R 4.1.2/:memory:]
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
# this is what the data looks like
head(cdm$denominator)
#> # Source:   SQL [6 x 4]
#> # Database: DuckDB 0.5.0 [unknown@Linux 5.4.0-132-generic:R 4.1.2/:memory:]
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>   <chr>                <chr>      <date>            <date>         
#> 1 1                    1          2013-12-18        2014-12-30     
#> 2 1                    4          2010-08-25        2011-02-12     
#> 3 1                    6          2017-10-21        2018-01-01     
#> 4 1                    8          2008-09-01        2008-10-15     
#> 5 1                    11         2008-12-18        2010-08-13     
#> 6 1                    12         2008-05-28        2010-06-13
head(attrition(cdm$denominator))
#> # A tibble: 6 × 3
#>   current_n reason                                                       exclu…¹
#>       <dbl> <chr>                                                          <dbl>
#> 1      5000 Starting population                                               NA
#> 2      5000 Missing year of birth                                              0
#> 3      5000 Missing sex                                                        0
#> 4      5000 Cannot satisfy age criteria during the study period based o…       0
#> 5      3811 No observation time available during study period               1189
#> 6      3811 Doesn't satisfy age criteria during the study period               0
#> # … with abbreviated variable name ¹​excluded
```

Now we have identified our denominator population, we can calculate
incidence, point prevalence, and period prevalence as below (given that
our mock cdm_reference also has an outcome cohort defined). Again
further details for each of these functions are provided in the
vignettes.

Now we´ve added the denominator cohort to our cdm reference, we can go
on and estimate incidence and prevalence.

``` r
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome"
)
head(inc$incidence_estimates)
#> # A tibble: 6 × 13
#>   incidenc…¹ n_per…² perso…³ n_eve…⁴ time  start_time end_time   perso…⁵ ir_10…⁶
#>   <chr>        <int>   <dbl>   <int> <chr> <date>     <date>       <dbl>   <dbl>
#> 1 1              513   14427      33 2008… 2008-01-01 2008-01-31    39.5  83546.
#> 2 1              480   12892      30 2008… 2008-02-01 2008-02-29    35.3  84995.
#> 3 1              465   13012      22 2008… 2008-03-01 2008-03-31    35.6  61755.
#> 4 1              441   12352      28 2008… 2008-04-01 2008-04-30    33.8  82796.
#> 5 1              423   11782      34 2008… 2008-05-01 2008-05-31    32.3 105402.
#> 6 1              395   10564      29 2008… 2008-06-01 2008-06-30    28.9 100267.
#> # … with 4 more variables: ir_100000_pys_low <dbl>, ir_100000_pys_high <dbl>,
#> #   cohort_obscured <chr>, result_obscured <chr>, and abbreviated variable
#> #   names ¹​incidence_analysis_id, ²​n_persons, ³​person_days, ⁴​n_events,
#> #   ⁵​person_years, ⁶​ir_100000_pys
```

``` r
prev_point <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "months"
)
head(prev_point$prevalence_estimates)
#> # A tibble: 6 × 11
#>   prevale…¹ time  numer…² denom…³    prev prev_low prev_…⁴ start_time end_time
#>   <chr>     <chr>   <dbl>   <dbl>   <dbl>    <dbl>   <dbl> <date>     <date>  
#> 1 1         2008…       8     488  0.0164  0.00513  0.0277 2008-01-01 NA      
#> 2 1         2008…       6     481  0.0125  0.00256  0.0224 2008-02-01 NA      
#> 3 1         2008…      11     486  0.0226  0.00941  0.0359 2008-03-01 NA      
#> 4 1         2008…      NA     492 NA      NA       NA      2008-04-01 NA      
#> 5 1         2008…       7     488  0.0143  0.00379  0.0249 2008-05-01 NA      
#> 6 1         2008…       7     475  0.0147  0.00390  0.0256 2008-06-01 NA      
#> # … with 2 more variables: cohort_obscured <chr>, result_obscured <chr>, and
#> #   abbreviated variable names ¹​prevalence_analysis_id, ²​numerator,
#> #   ³​denominator, ⁴​prev_high
```

``` r
prev_period <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "months"
)
head(prev_period$prevalence_estimates)
#> # A tibble: 6 × 11
#>   prevalenc…¹ time  numer…² denom…³   prev prev_…⁴ prev_…⁵ start_time end_time  
#>   <chr>       <chr>   <dbl>   <dbl>  <dbl>   <dbl>   <dbl> <date>     <date>    
#> 1 1           2008…      39     513 0.0760  0.0531  0.0990 2008-01-01 2008-01-31
#> 2 1           2008…      36     509 0.0707  0.0485  0.0930 2008-02-01 2008-02-29
#> 3 1           2008…      31     521 0.0595  0.0392  0.0798 2008-03-01 2008-03-31
#> 4 1           2008…      32     513 0.0624  0.0415  0.0833 2008-04-01 2008-04-30
#> 5 1           2008…      41     515 0.0796  0.0562  0.103  2008-05-01 2008-05-31
#> 6 1           2008…      34     505 0.0673  0.0455  0.0892 2008-06-01 2008-06-30
#> # … with 2 more variables: cohort_obscured <chr>, result_obscured <chr>, and
#> #   abbreviated variable names ¹​prevalence_analysis_id, ²​numerator,
#> #   ³​denominator, ⁴​prev_low, ⁵​prev_high
```
