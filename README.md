
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OmopPopEpi

<!-- badges: start -->

[![R-CMD-check](https://github.com/edward-burn/OmopPopEpi/workflows/R-CMD-check/badge.svg)](https://github.com/edward-burn/OmopPopEpi/actions)
<!-- badges: end -->

## Package overview

<img src="extras/OmopPopEpi_overview.png" width="100%" />

OmopPopEpi contains functions for estimating population-level incidence
and prevalence using the OMOP common data model. The package facilitates
the creation of the denominator populations for incidence and prevalence
calculations with the collect\_denominator\_pops() function. Prevalence
and incidence can then be calculated for a set of (previously
instanstiated) outcome cohorts, using the collect\_pop\_prevalence() and
collect\_pop\_incidence() functions. The obscure\_counts\_prevalence()
and obscure\_counts\_incidence() can then be used to enforce minimum
cell counts that may be required before results can be shared.

## Results specification

The results produced are as follows:  
*To add*

## Package installation

You can install the development version of OmopPopEpi like so:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu/OmopPopEpi")
```

## Example

``` r
library(OmopPopEpi)
#> Loading required package: lubridate
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
## basic example code
```
