
# IncidencePrevalence <img src="man/figures/hexsticker.png" align="right" height="139"/>

[![CRANstatus](https://www.r-pkg.org/badges/version/IncidencePrevalence)](https://CRAN.R-project.org/package=IncidencePrevalence)
[![codecov.io](https://codecov.io/github/darwin-eu/IncidencePrevalence/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu/IncidencePrevalence?branch=main)
[![R-CMD-check](https://github.com/darwin-eu/IncidencePrevalence/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/IncidencePrevalence/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html)

## Package overview

IncidencePrevalence contains functions for estimating population-level
incidence and prevalence using the OMOP common data model. For more
information on the package please see our paper in Pharmacoepidemiology
and Drug Safety.

> Raventós, B, Català, M, Du, M, et al. IncidencePrevalence: An R
> package to calculate population-level incidence rates and prevalence
> using the OMOP common data model. Pharmacoepidemiol Drug Saf. 2023;
> 1-11. doi: 10.1002/pds.5717

If you find the package useful in supporting your research study, please
consider citing this paper.

## Package installation

You can install the latest version of IncidencePrevalence from CRAN:

``` r
install.packages("CDMConnector")
```

Or from github:

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
  password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
)

cdm <- CDMConnector::cdm_from_con(con,
  cdm_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
  write_schema = Sys.getenv("CDM5_POSTGRESQL_RESULT_SCHEMA")
)
```

To see how you would create a reference to your database please consult
the CDMConnector package documentation. For this example though we´ll
work with simulated data, and we’ll generate an example cdm reference
like so:

``` r
cdm <- mockIncidencePrevalenceRef(sampleSize = 10000, 
                                  outPre = 0.3, 
                                  minOutcomeDays = 365, 
                                  maxOutcomeDays = 3650)
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
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01")),
  ageGroup = list(
    c(0, 64),
    c(65, 100)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorObservation = 180
)
```

This will then give us six denominator cohorts

``` r
cohortSet(cdm$denominator)
#> # A tibble: 6 × 9
#>   cohort_definition_id cohort_name        age_group sex   days_prior_observation
#>                  <int> <chr>              <chr>     <chr>                  <dbl>
#> 1                    1 Denominator cohor… 0 to 64   Male                     180
#> 2                    2 Denominator cohor… 0 to 64   Fema…                    180
#> 3                    3 Denominator cohor… 0 to 64   Both                     180
#> 4                    4 Denominator cohor… 65 to 100 Male                     180
#> 5                    5 Denominator cohor… 65 to 100 Fema…                    180
#> 6                    6 Denominator cohor… 65 to 100 Both                     180
#> # ℹ 4 more variables: start_date <date>, end_date <date>,
#> #   target_cohort_definition_id <dbl>, target_cohort_name <lgl>
```

These cohorts will be in the typical OMOP CDM structure

``` r
cdm$denominator
#> # Source:   table<denominator> [?? x 4]
#> # Database: DuckDB 0.8.1 [eburn@Windows 10 x64:R 4.2.1/:memory:]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int> <chr>      <date>            <date>         
#>  1                    1 3          2008-01-01        2008-04-02     
#>  2                    1 4          2009-12-16        2010-01-03     
#>  3                    1 17         2012-07-30        2014-09-09     
#>  4                    1 25         2014-06-27        2015-10-01     
#>  5                    1 26         2008-05-27        2009-02-06     
#>  6                    1 29         2012-07-12        2014-08-05     
#>  7                    1 31         2008-01-01        2008-12-15     
#>  8                    1 37         2016-07-28        2016-07-31     
#>  9                    1 48         2017-09-12        2017-10-13     
#> 10                    1 55         2015-04-13        2017-04-09     
#> # ℹ more rows
```

### Estimating incidence and prevalence

As well as a denominator cohort, an outcome cohort will need to be
identified. Defining outcome cohorts is done outside of the
IncdidencePrevalence package and our mock data already includes an
outcome cohort.

``` r
cdm$outcome
#> # Source:   table<outcome> [?? x 4]
#> # Database: DuckDB 0.8.1 [eburn@Windows 10 x64:R 4.2.1/:memory:]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>    <chr>                <chr>      <date>            <date>         
#>  1 1                    1          2016-05-20        2018-02-27     
#>  2 1                    2          2005-02-27        2006-12-07     
#>  3 1                    7          2012-05-12        2014-02-19     
#>  4 1                    14         2009-09-19        2011-06-29     
#>  5 1                    28         2014-03-11        2015-12-19     
#>  6 1                    32         2008-06-25        2010-04-04     
#>  7 1                    40         2014-07-09        2016-04-17     
#>  8 1                    43         2016-11-01        2018-08-11     
#>  9 1                    46         2008-04-30        2010-02-07     
#> 10 1                    51         2018-05-18        2020-02-25     
#> # ℹ more rows
```

Now we have identified our denominator population, we can calculate
incidence and prevalence as below. Note, in our example cdm reference we
already have an outcome cohort defined.

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
plotIncidence(inc, facet = c("denominator_age_group", "denominator_sex"))
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

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
plotPrevalence(prev_point, facet = c("denominator_age_group", "denominator_sex"))
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

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
plotPrevalence(prev_period, facet = c("denominator_age_group", "denominator_sex"))
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />
