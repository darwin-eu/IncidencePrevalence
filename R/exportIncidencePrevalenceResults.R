# Copyright 2023 DARWIN EU®
#
# This file is part of IncidencePrevalence
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


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
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
#' cdm$denominator <- generateDenominatorCohortSet(
#'   cdm = cdm,
#'   startDate = as.Date("2008-01-01"),
#'   endDate = as.Date("2018-01-01")
#' )
#' prev <- estimatePointPrevalence(
#' cdm = cdm,
#' denominatorTable = "denominator",
#' outcomeTable = "outcome"
#' )
#'  results <- gatherIncidencePrevalenceResults(cdm = cdm, resultList=list(prev))
#'  exportIncidencePrevalenceResults(result=results, zipName="test",
#'                                   outputFolder=tempdir())
#' }
exportIncidencePrevalenceResults <- function(result, zipName, outputFolder) {

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(
    inherits(result, "IncidencePrevalenceGatheredResult"),
                        add = errorMessage
  )
  checkmate::assertCharacter(zipName, len = 1,
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
                       paste0(attr(result, "cdm_name"), "_",
                              checkResultName, "_",
                              format(Sys.Date(), format="%Y%m%d"),
                              ".csv")),
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
