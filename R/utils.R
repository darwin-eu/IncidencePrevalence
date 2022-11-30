
#' Cohort attrition
#'
#' @param x Cohort set for which to get attrition
#'
#' @return tibble with counts and reasons for attrition.
#' @export
#'
#' @examples
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


#' Settings associated with a cohort set
#'
#' @param x Cohort set for which to get settings
#'
#' @return tibble with settings used when generating the cohort set
#' @export
#'
#' @examples
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


#' SQL trace for a cohort
#'
#' @param x Cohort set for which to get associated SQL
#'
#' @return SQL used to generate the cohort set
#' @export
#'
#' @examples
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

#'  Participants contributing to an analysis
#'
#' @param x Result object
#'
#' @return References to tables with the study participants contributing to
#' a given analysis
#' @export
#'
#' @examples
participants <- function(x) {
  UseMethod("participants")
}

#' @export
participants.IncidencePrevalenceResult <- function(x) {
  attr(x, "participants")
}


extractQuery <- function(query, description = "") {
  sql <- dbplyr::sql_render(query)
  sql <- gsub("\"", "", sql)
  sql <- rbind(
    paste0("< SQL ", description, ">"),
    sql, " "
  )
  return(sql)
}

