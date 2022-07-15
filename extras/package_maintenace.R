# Maintenance -----

# run all testthat tests (after reloading with current code) - do all pass?
devtools::test()

# check code coverage - are all functions covered 100%?
detach("package:IncidencePrevalence", unload=TRUE)
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
# nb doesn´t pick up vars used in glue
lintr::lint_package()

devtools::build_readme()
devtools::document() #  Use roxygen to document a package.

devtools::build_vignettes()

# usethis::use_github_actions()
# usethis::use_github_action("lint")
# usethis::use_github_action("test-coverage")
