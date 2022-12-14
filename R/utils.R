
#' Cohort attrition
#'
#' @param result Result for which to get attrition
#' @param analysisId ID of a specific analysis to return attrition for
#'
#' @return tibble with counts and reasons for attrition.
#' @export
#'
#' @examples
attrition <- function(result, analysisId=NULL) {
  UseMethod("attrition")
}

#' @export
attrition.IncidencePrevalenceDenominator <- function(result, analysisId=NULL) {
  attr(result, "attrition")
}

#' @export
attrition.IncidencePrevalenceResult <- function(result, analysisId=NULL) {
 attr <- attr(result, "attrition")
  if(!is.null(analysisId)){
    attr<-attr %>% dplyr::filter(.data$analysis_id==.env$analysisId)
  }
 return(attr)
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
#' @param result Result object
#' @param analysisId ID of a specific analysis to return participants for
#'
#' @return References to tables with the study participants contributing to
#' a given analysis
#' @export
#'
#' @examples
participants <- function(result, analysisId=NULL) {
  UseMethod("participants")
}

#' @export
participants.IncidencePrevalenceResult <- function(result, analysisId=NULL) {
  included<-attr(result, "participants")
  if(!is.null(analysisId)){
    included<-included[[paste0("study_population_analyis_",analysisId)]]
  }
 return(included)
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

