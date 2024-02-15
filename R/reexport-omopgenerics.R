# Copyright 2024 DARWIN EU®
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

#' @export
#' @importFrom omopgenerics settings
settings.IncidenceResult <- function(x) {
  settings <- attr(x, "settings")
  return(settings)
}

#' @export
#' @importFrom omopgenerics settings
settings.PrevalenceResult <- function(x) {
  settings <- attr(x, "settings")
  return(settings)
}

#' @export
#' @importFrom omopgenerics attrition
attrition.IncidenceResult <- function(x) {
  attrition <- attr(x, "attrition")
  return(attrition)
}

#' @export
#' @importFrom omopgenerics attrition
attrition.PrevalenceResult <- function(x) {
  attrition <- attr(x, "attrition")
  return(attrition)
}
