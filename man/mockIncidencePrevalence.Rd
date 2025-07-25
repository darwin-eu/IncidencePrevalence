% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mockIncidencePrevalence.R
\name{mockIncidencePrevalence}
\alias{mockIncidencePrevalence}
\title{Generate example subset of the OMOP CDM for
estimating incidence and prevalence}
\usage{
mockIncidencePrevalence(
  personTable = NULL,
  observationPeriodTable = NULL,
  targetCohortTable = NULL,
  outcomeTable = NULL,
  censorTable = NULL,
  sampleSize = 1,
  outPre = 1,
  seed = 444,
  earliestDateOfBirth = NULL,
  latestDateOfBirth = NULL,
  earliestObservationStartDate = as.Date("1900-01-01"),
  latestObservationStartDate = as.Date("2010-01-01"),
  minDaysToObservationEnd = 1,
  maxDaysToObservationEnd = 4380,
  minOutcomeDays = 1,
  maxOutcomeDays = 10,
  maxOutcomes = 1
)
}
\arguments{
\item{personTable}{A tibble in the format of the person table.}

\item{observationPeriodTable}{A tibble in the format of the observation
period table.}

\item{targetCohortTable}{A tibble in the format of a cohort table which can
be used for stratification}

\item{outcomeTable}{A tibble in the format of a cohort table which can
be used for outcomes}

\item{censorTable}{A tibble in the format of a cohort table which can
be used for censoring}

\item{sampleSize}{The number of unique patients.}

\item{outPre}{The fraction of patients with an event.}

\item{seed}{The seed for simulating the data set. Use the same
seed to get same data set.}

\item{earliestDateOfBirth}{The earliest date of birth of a patient in
person table.}

\item{latestDateOfBirth}{The latest date of birth of a patient in
person table.}

\item{earliestObservationStartDate}{The earliest observation start date
for patient format.}

\item{latestObservationStartDate}{The latest observation start date
for patient format.}

\item{minDaysToObservationEnd}{The minimum number of days of
the observational integer.}

\item{maxDaysToObservationEnd}{The maximum number of days of
the observation period integer.}

\item{minOutcomeDays}{The minimum number of days of the outcome
period default set to 1.}

\item{maxOutcomeDays}{The maximum number of days of the outcome
period default set to 10.}

\item{maxOutcomes}{The maximum possible number of outcomes per
person can have default set to 1.}
}
\value{
A cdm reference to a duckdb database with mock data.
}
\description{
Generate example subset of the OMOP CDM for
estimating incidence and prevalence
}
\examples{
\donttest{
cdm <- mockIncidencePrevalence(sampleSize = 100)
cdm
}

}
