# Maintenance

# Before pushing code, please go through the below steps

# 1) Run tests
# run all testthat tests (after reloading with current code) - do all pass?
devtools::test()
# uncomment below to check code coverage - are all functions covered 100%?
# detach("package:IncidencePrevalence", unload=TRUE)
# devtools::test_coverage()

# 2) Code style
# have you followed the style guide?
# note you can use stlyer to fix formatting
# nb doesn´t pick up vars used in glue
lintr::lint_package(".",
                    linters = lintr::linters_with_defaults(
                      lintr::object_name_linter(styles = "camelCase")
                    )
)

# 3) Documentation
devtools::document() #  Use roxygen to document the package
devtools::build_readme()
devtools::build_vignettes()
# Check documentation (as R CMD check)
devtools::check_man()

# 4) Run check
# fuller checks - any warnings or notes to be fixed?
# note, warning about qpdf can be ignored
devtools::check() # updates the documentation, then builds



