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
#' @param resultList Named list with results from estimateIncidence,
#' estimatePointPrevalence, or estimatePeriodPrevalence
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
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator"
#' )
#' prev <- estimatePointPrevalence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' exportIncidencePrevalenceResults(
#'   resultList = list("prevalence" = prev),
#'   zipName = "test",
#'   outputFolder = tempdir()
#' )
#' }
exportIncidencePrevalenceResults <- function(resultList,
                                             zipName,
                                             outputFolder) {
  errorMessage <- checkmate::makeAssertCollection()

  checkResultType <- list()
  for (i in seq_along(resultList)) {
    checkResultType[[i]] <- inherits(
      resultList[[i]],
      "IncidencePrevalenceResult"
    ) &
      nrow(resultList[[i]] >= 1)
  }
  checkResultType <- all(unlist(checkResultType)) == TRUE
  if (!isTRUE(checkResultType)) {
    errorMessage$push(
      "- All objects in resultList must be an IncidencePrevalenceResult"
    )
  }
  checkmate::assertTRUE(checkResultType,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  if(!dir.exists(outputFolder)){
    cli::cli_abort("{outputFolder} does not exist")
  }


  tempDir <- zipName
  tempDirCreated <- FALSE
  if (!dir.exists(tempDir)) {
    dir.create(tempDir)
    tempDirCreated <- TRUE
  }

  # write results to disk
  for (i in seq_along(resultList)) {
    workingResult <- resultList[[i]]
    workingName <- names(resultList)[[i]]
    if (is.null(workingName)) {
      workingName <- paste0("result_", i)
    }
    utils::write.csv(workingResult,
      file = file.path(
        tempDir,
        paste0(
          unique(workingResult$cdm_name), "_",
          workingName, "_",
          format(Sys.Date(), format = "%Y_%m_%d"),
          ".csv"
        )
      ),
      row.names = FALSE
    )
  }
  zip::zip(
    zipfile = file.path(outputFolder, paste0(zipName, ".zip")),
    files = list.files(tempDir, full.names = TRUE)
  )
  if (tempDirCreated) {
    unlink(tempDir, recursive = TRUE)
  }

  invisible(resultList)
}
