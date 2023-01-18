

#' Drop stem tables
#'
#' @param cdm A CDM reference object
#' @param computePermanentStem The stem for which to drop tables. For example,
#' a stem of "study_results" would lead to dropping any table starting with this
#' (ie both "study_results" and "study_results_analysis_1" would be dropped if
#' they exist in the write schema)
#'
#' @return NULL
#' @export
#'
#' @examples
#'  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
#'  cdm$dpop <- IncidencePrevalence::generateDenominatorCohortSet(
#'   cdm = cdm,
#'   computePermanent = TRUE,
#'   computePermanentStem = "result")
#' inc <- IncidencePrevalence::estimateIncidence(
#'   cdm = cdm,
#'   denominatorTable = "dpop",
#'   outcomeTable = "outcome",
#'   interval = "overall",
#'   computePermanent = TRUE,
#'   computePermanentStem = "result",
#'   returnParticipants = TRUE)
#' CDMConnector::listTables(attr(cdm, "dbcon"),
#'                           schema = attr(cdm, "write_schema")
#' )
#'
#' dropStemTables(cdm, computePermanentStem = "result")
#' CDMConnector::listTables(attr(cdm, "dbcon"),
#'                          schema = attr(cdm, "write_schema")

dropStemTables <- function(cdm,
                           computePermanentStem){

  ## check for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()
  cdmCheck <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdmCheck,
                        add = errorMessage
  )
  if (!isTRUE(cdmCheck)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::assertCharacter(computePermanentStem, len = 1,
                             add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)


# drop tables if they exist
 tablesToDrop <- CDMConnector::listTables(attr(cdm, "dbcon"),
   schema = attr(cdm, "write_schema")
 )[
   which(stringr::str_detect(
     CDMConnector::listTables(attr(cdm, "dbcon"),
       schema = attr(cdm, "write_schema")
     ),
     paste0(computePermanentStem)
   ))
 ]

 for(i in seq_along(tablesToDrop)){
   DBI::dbRemoveTable(attr(cdm, "dbcon"),
                      DBI::SQL(paste0(c(attr(cdm, "write_schema"),
                                        tablesToDrop[[i]]),
                                      collapse = ".")))
 }

 return(invisible(NULL))

}



dropTable <- function(cdm, table){

  for(i in seq_along(table)){
  # check table exists in write schema
  tableExists <- any(stringr::str_detect(
    CDMConnector::listTables(attr(cdm, "dbcon"),
                             schema = attr(cdm, "write_schema")),
    paste0(table[[i]], "\\b")))

  if(tableExists==TRUE){
    DBI::dbRemoveTable(attr(cdm, "dbcon"),
                       DBI::SQL(paste0(c(attr(cdm, "write_schema"),
                                         table[[i]]),
                                       collapse = ".")))
  }
  }

  return(invisible(NULL))

}
