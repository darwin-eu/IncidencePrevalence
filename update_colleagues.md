# Update: `estimateRollingIncidence()` has been implemented! 🎉

Hi team,

I'm excited to share that we've successfully implemented a new core feature in our fork of the DARWIN-EU **`IncidencePrevalence`** package: the **`estimateRollingIncidence()`** function!

### What's new?
While `estimateIncidence()` computes incidence rates across **absolute calendar time** (e.g., January 2010, February 2010), our new function calculates incidence rates **relative to each individual's cohort entry date**. 

This allows us to track "time-since-entry incidence" or "rolling incidence" out of the box (e.g., "Month 1 after cohort entry", "Month 2 after cohort entry", etc.).

### Key Features
- Dynamic relative calculation of temporal intervals (supports `weeks`, `months`, `quarters`, `years`).
- Robust handling of individual-level observation truncation and censoring (correctly handles partial contributions to relative windows).
- Supports all existing parameters (`outcomeWashout`, `repeatedEvents`, `strata`).
- Fully compliant with the DARWIN-EU ecosystem—outputs a standard `omopgenerics::SummarisedResult` object compatible with `plotIncidence()` and `tableIncidence()`.

### Code & Documentation
- **Source Code:** [`R/estimateRollingIncidence.R`](https://github.com/iomedhealth/IncidencePrevalence/blob/main/R/estimateRollingIncidence.R)
- **Documentation:** The function is fully documented in the package [Reference Guide](https://iomedhealth.github.io/IncidencePrevalence/reference/estimateRollingIncidence.html) and we've added a new section to the [Calculating Incidence Vignette](https://iomedhealth.github.io/IncidencePrevalence/articles/a05_Calculating_incidence.html).

### Example Usage

Here's a quick example of how to track the first 12 rolling months of incidence after a patient enters the denominator cohort, applying a 180-day washout period for repeated events:

```r
library(IncidencePrevalence)

# Assuming 'cdm' is your CDM reference and cohorts are already generated
rolling_inc <- estimateRollingIncidence(
  cdm               = cdm,
  denominatorTable  = "denominator",
  outcomeTable      = "outcome",
  interval          = "months",
  nIntervals        = 12,        # Track 12 rolling months per person
  outcomeWashout    = 180,       # 180-day washout between repeated events
  repeatedEvents    = TRUE
)

# You can plot it just like standard incidence!
plotIncidence(rolling_inc)
```

Let me know if you have any questions or run into any issues using the new function!
