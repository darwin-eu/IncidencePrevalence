#' Export IncidencePrevalence results
#'
#' @param result IncidencePrevalence results from gatherResults()
#' @param zipName name to give zip folder
#' @param outputFolder directory to save zip folder containing results as a set
#' of CSV files
#'
#' @return zip folder of results saved in the outputFolder
#' @export
#'
#' @examples
writeResultToDisk <- function(result, zipName, outputFolder) {

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(
    inherits(result, "IncidencePrevalenceGatheredResult"),
                        add = errorMessage
  )
  checkmate::assertCharacter(zipName,len = 1,
                             add = errorMessage)
  checkmate::assertDirectoryExists(outputFolder,
                              add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  tempDir <- zipName
  tempDirCreated <- FALSE
  if (!dir.exists(tempDir)) {
    dir.create(tempDir)
    tempDirCreated <- TRUE
  }

  # write results to disk
  lapply(names(result), FUN = function(checkResultName) {
    checkResult <- result[[checkResultName]]
    utils::write.csv(checkResult,
                     file = file.path(
                       tempDir,
                       paste0(checkResultName, ".csv")
                     ),
                     row.names = FALSE
    )
  })
  zip::zip(zipfile = file.path(outputFolder, paste0(zipName, ".zip")),
           files = list.files(tempDir, full.names = TRUE))
  if (tempDirCreated) {
    unlink(tempDir, recursive = TRUE)
  }

  invisible(result)
}
