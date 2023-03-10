% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimatePrevalence.R
\name{estimatePointPrevalence}
\alias{estimatePointPrevalence}
\title{Estimate point prevalence}
\usage{
estimatePointPrevalence(
  cdm,
  denominatorTable,
  outcomeTable,
  denominatorCohortId = NULL,
  outcomeCohortId = NULL,
  outcomeCohortName = NULL,
  outcomeLookbackDays = 0,
  interval = "years",
  timePoint = "start",
  minCellCount = 5,
  tablePrefix = NULL,
  returnParticipants = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{cdm}{A CDM reference object}

\item{denominatorTable}{A cohort table with a set of denominator cohorts
(for example, created using the \code{generateDenominatorCohortSet()}
function).}

\item{outcomeTable}{A cohort table in the cdm reference containing
a set of outcome cohorts.}

\item{denominatorCohortId}{The cohort definition ids of the denominator
cohorts of interest. If NULL all cohorts will be considered in the
analysis.}

\item{outcomeCohortId}{The cohort definition ids of the outcome
cohorts of interest. If NULL all cohorts will be considered in the
analysis.}

\item{outcomeCohortName}{Corresponding names for each outcomeCohortId.}

\item{outcomeLookbackDays}{Days lookback when considering an outcome
as prevalent. If NULL any prior outcome will be considered as prevalent. If
0, only ongoing outcomes will be considered as prevalent.}

\item{interval}{Time intervals over which period prevalence is estimated. Can
be "weeks", "months", "quarters", or "years". ISO weeks will
be used for weeks. Calendar months, quarters, or years can be used as
the period. If more than one option is chosen then results will
be estimated for each chosen interval.}

\item{timePoint}{where to compute the point prevalence}

\item{minCellCount}{Minimum number of events to report- results
lower than this will be obscured. If NULL all results will be reported.}

\item{tablePrefix}{The stem for the permanent tables that will
be created when running the analysis. Permanent tables will be created using
this prefix, and any existing tables that start with this will be at risk of
being dropped or overwritten. If NULL, temporary tables will be
used throughout.}

\item{returnParticipants}{Either TRUE or FALSE. If TRUE references to
participants from the analysis will be returned allowing for further
analysis. Note, if using permanent tables and returnParticipants is TRUE,
one table per analysis will be kept in the cdm write schema (these
can be dropped at subsequently using dropStemTables() function)}

\item{verbose}{Whether to report progress}
}
\value{
Point prevalence estimates
}
\description{
Estimate point prevalence
}
\examples{
\dontrun{
db <- DBI::dbConnect(" Your database connection here ")
cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = "cdm schema name"
)
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2008-01-01"),
  endDate = as.Date("2018-01-01")
)
estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "months"
)
}
}
