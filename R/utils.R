
#' @param x Cohort for which to get attrition
#'
#' @export
attrition <- function(x) {
  UseMethod("attrition")
}

#' @export
attrition.IncidencePrevalenceDenominator <- function(x) {
  attr(x, "attrition")
}

#' @export
attrition.IncidencePrevalenceResult <- function(x) {
  attr(x, "attrition")
}

#' @param Cohort for which to get settings
#'
#' @export
settings <- function(x) {
  UseMethod("settings")
}

#' @export
settings.IncidencePrevalenceDenominator <- function(x) {
  attr(x, "settings")
}

#' @export
settings.IncidencePrevalenceResult <- function(x) {
  attr(x, "settings")
}

#' @param Cohort for which to get SQL trace
#'
#' @export
sqlTrace <- function(x) {
  UseMethod("sqlTrace")
}

#' @export
sqlTrace.IncidencePrevalenceDenominator <- function(x) {
  attr(x, "sql")
}

#' @export
sqlTrace.IncidencePrevalenceResult <- function(x) {
  attr(x, "sql")
}

#' @export
#' @importFrom utils str
print.sqlTrace <- function(x, ...) {
  cli::cat_line("<SQL query trace>")
  print(utils::str(x))
}

print.IncidencePrevalenceDenominator <- function(x, ...) {
  cli::cat_rule("IncidencePrevalence denominator generated cohort set")
  print(x)
}

#' @param x Cohort for which to get attrition
#'
#' @export
participants <- function(x) {
  UseMethod("participants")
}

#' @export
participants.IncidencePrevalenceResult <- function(x) {
  attr(x, "participants")
}
