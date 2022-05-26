## Setting up-----
# Functions and corresponding tests -----
# usethis::use_r("collect_denominator_pops")
# usethis::use_test("collect_denominator_pops")

# usethis::use_r("get_denominator_pop")
# usethis::use_test("get_denominator_pop")

# usethis::use_r("calculate_pop_incidence")
# usethis::use_test("calculate_pop_incidence")

# usethis::use_r("calculate_pop_prevalence")
# usethis::use_test("calculate_pop_prevalence")

## imports -----
# usethis::use_pipe()
# usethis::use_package("rlang")
# usethis::use_package("checkmate")
# usethis::use_package("dplyr")
# usethis::use_package("dbplyr")
# usethis::use_package("lubridate")
# usethis::use_package("glue")
# usethis::use_package("DBI")
# usethis::use_package("tidyr")
## usethis::use_package("dtplyr")
## usethis::use_package("stringr")


## readme -----
# usethis::use_readme_rmd()

## license -----
# usethis::use_mit_license()

## vignettes ------
# usethis::use_vignette("a01_Introduction_to_OmopPopEpi")
# usethis::use_vignette("a02_Calculating_incidence")
# usethis::use_vignette("a03_Calculating_prevalence")


# Maintenance -----

# run all testthat tests (after reloading with current code) - do all pass?
devtools::test()

# check code coverage - are all functions covered 100%?
detach("package:OmopPopEpi", unload=TRUE)
devtools::test_coverage()
# for more coverage details uncomment the following
# covr::report()
# cov <- covr::package_coverage(here::here())
# covr::zero_coverage(cov)

# run all examples - do they all run without error?
devtools::run_examples()

# check spelling throughout - any obvious typos to fix?
devtools::spell_check()
# spelling::update_wordlist() # if they are not true spelling mistakes we can add them to our wordlist

# Check documentation (as R CMD check)
devtools::check_man()

# fuller checks - any warnings or notes to be fixed?
# note, warning about qpdf can be ignored
devtools::check() # updates the documentation, then builds
devtools::check_rhub() # All os

# have you followed the style guide?
# note you can use stlyer to fix formatting
# ignore formatting warning for .datatable.aware
#  doesn´t pick up vars used in glue (So ignore warningn about duration)
lintr::lint_package()

devtools::build_readme()
devtools::document() #  Use roxygen to document a package.

# data for vignettes is precomputed, so first let´s do that (which might take a little while)
# source(here::here("extras", "precompute_vignette_data.R"))
# devtools::build_vignettes()

