% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyResults.R
\name{asPrevalenceResult}
\alias{asPrevalenceResult}
\title{A tidy implementation of the summarised_result object for prevalence results.}
\usage{
asPrevalenceResult(result)
}
\arguments{
\item{result}{A summarised_result object created by the IncidencePrevalence package.}
}
\value{
A tibble with a tidy version of the summarised_result object.
}
\description{
A tidy implementation of the summarised_result object for prevalence results.
}
\examples{
\donttest{
cdm <- mockIncidencePrevalence()
prev <- estimatePointPrevalence(cdm, "target", "outcome")
tidy_prev <- asPrevalenceResult(prev)
}

}
