# Copyright 2025 DARWIN EU®
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

#' Plot incidence results
#'
#' @param result Incidence results
#' @param x Variable to plot on x axis
#' @param y Variable to plot on y axis.
#' @param line Whether to plot a line using `geom_line`
#' @param point Whether to plot points using `geom_point`
#' @param ribbon Whether to plot a ribbon using `geom_ribbon`
#' @param ymin Lower limit of error bars, if provided is plot using `geom_errorbar`
#' @param ymax Upper limit of error bars, if provided is plot using `geom_errorbar`
#' @param facet Variables to use for facets. To see available variables for
#' facetting use the function `availableIncidenceGrouping()`.
#' @param colour Variables to use for colours. To see available variables for
#' colouring use the function `availableIncidenceGrouping()`.
#'
#' @return A ggplot with the incidence results plotted
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
#' )
#' inc <- estimateIncidence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' plotIncidence(inc)
#' }
plotIncidence <- function(result,
                          x = "incidence_start_date",
                          y = "incidence_100000_pys",
                          line = FALSE,
                          point = TRUE,
                          ribbon = FALSE,
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper",
                          facet = NULL,
                          colour = NULL) {
  rlang::check_installed("visOmopResults", version = "1.0.0")

  if (nrow(result) == 0) {
    cli::cli_warn("Empty result object")
    return(emptyPlot())
  }

  # check if result is tidy or not
  if (inherits(result, "summarised_result")) {
    resultTidy <- result |>
      tidyResult(type = "incidence", attrition = FALSE)
  } else if (result |>
             dplyr::pull("result_type") |>
             unique() ==
             "tidy_incidence") {
    resultTidy <- result
  } else {
    cli::cli_abort("result must be either a summarised_result object output of
                   the estimateIncidence() function or a tidied result from the
                   asIncidenceResult() function.")
  }

  if (nrow(resultTidy) == 0) {
    cli::cli_warn("No incidence results available to plot")
    return(emptyPlot())
  }

  omopgenerics::assertChoice(y, c("incidence_100000_pys", "outcome_count", "denominator_count", "person_days"))

  plotEstimates(
    result = resultTidy,
    x = x,
    y = y,
    line = line,
    point = point,
    ribbon = ribbon,
    ymin = ymin,
    ymax = ymax,
    facet = facet,
    colour = colour,
    type = "incidence"
  )
}

#' Plot prevalence results
#'
#' @param result Prevalence results
#' @param x  Variable to plot on x axis
#' @param y Variable to plot on y axis.
#' @param line Whether to plot a line using `geom_line`
#' @param point Whether to plot points using `geom_point`
#' @param ribbon Whether to plot a ribbon using `geom_ribbon`
#' @param ymin Lower limit of error bars, if provided is plot using `geom_errorbar`
#' @param ymax Upper limit of error bars, if provided is plot using `geom_errorbar`
#' @param facet Variables to use for facets. To see available variables for
#' facetting use the function `availablePrevalenceGrouping()`.
#' @param colour Variables to use for colours. To see available variables for
#' colouring use the function `availablePrevalenceGrouping()`.
#'
#' @return A ggplot with the prevalence results plotted
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2014-01-01"), as.Date("2018-01-01"))
#' )
#' prev <- estimatePointPrevalence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' plotPrevalence(prev)
#' }
plotPrevalence <- function(result,
                           x = "prevalence_start_date",
                           y = "prevalence",
                           line = FALSE,
                           point = TRUE,
                           ribbon = FALSE,
                           ymin = "prevalence_95CI_lower",
                           ymax = "prevalence_95CI_upper",
                           facet = NULL,
                           colour = NULL) {
  if (nrow(result) == 0) {
    cli::cli_warn("Empty result object")
    return(emptyPlot())
  }

  rlang::check_installed("visOmopResults", version = "1.0.0")

  # check if result is tidy or not
  if (inherits(result, "summarised_result")) {
    resultTidy <- result |>
      tidyResult(type = "prevalence", attrition = FALSE)
  } else if (result |>
             dplyr::pull("result_type") |>
             unique() ==
             "tidy_prevalence") {
    resultTidy <- result
  } else {
    cli::cli_abort("result must be either a summarised_result object output of
                   the estimatePointPrevalence() or estimatePeriodPrevalence()
                   functions or a tidied result from the asPrevalebceResult()
                   function.")
  }

  if (nrow(resultTidy) == 0) {
    cli::cli_warn("No prevalence results available to plot")
    return(emptyPlot())
  }

  omopgenerics::assertChoice(y, c("prevalence", "denominator_count", "outcome_count"))

  plotEstimates(
    result = resultTidy,
    x = x,
    y = y,
    line = line,
    point = point,
    ribbon = ribbon,
    ymin = ymin,
    ymax = ymax,
    facet = facet,
    colour = colour,
    type = "prevalence"
  )
}

#' Bar plot of denominator counts, outcome counts, and person-time from
#' incidence results
#'
#' @param result Incidence results
#' @param x  Variable to plot on x axis
#' @param y Variable to plot on y axis.
#' @param facet Variables to use for facets. To see available variables for
#' facetting use the functions `availableIncidenceGrouping()`.
#' @param colour Variables to use for colours. To see available variables for
#' colouring use the function `availableIncidenceGrouping()`.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2014-01-01"), as.Date("2018-01-01"))
#' )
#' inc <- estimateIncidence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' plotIncidencePopulation(inc)
#' }
#'
plotIncidencePopulation <- function(result,
                                    x = "incidence_start_date",
                                    y = "denominator_count",
                                    facet = NULL,
                                    colour = NULL) {
  rlang::check_installed("visOmopResults", version = "1.0.0")
  result <- omopgenerics::validateResultArgument(result) |>
    omopgenerics::filterSettings(.data$result_type == "incidence")
  if (nrow(result) == 0) {
    cli::cli_warn("No incidence results available to plot")
    return(emptyPlot())
  }
  resultTidy <- tidyResult(result, "incidence", attrition = FALSE)
  return(
    plotPopulation(resultTidy, x, y, facet, colour, type = "incidence")
  )
}

#' Bar plot of denominator and outcome counts from prevalence results
#'
#' @param result Prevalence results
#' @param x  Variable to plot on x axis
#' @param y Variable to plot on y axis.
#' @param facet Variables to use for facets. To see available variables for
#' facetting use the functions `availablePrevalenceGrouping()`.
#' @param colour Variables to use for colours. To see available variables for
#' colouring use the function `availablePrevalenceGrouping()`.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2014-01-01"), as.Date("2018-01-01"))
#' )
#' prev <- estimatePointPrevalence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' plotPrevalencePopulation(prev)
#' }
#'
plotPrevalencePopulation <- function(result,
                                     x = "prevalence_start_date",
                                     y = "denominator_count",
                                     facet = NULL,
                                     colour = NULL) {
  rlang::check_installed("visOmopResults", version = "1.0.0")
  result <- omopgenerics::validateResultArgument(result) |>
    omopgenerics::filterSettings(.data$result_type == "prevalence")
  if (nrow(result) == 0) {
    cli::cli_warn("No prevalence results available to plot")
    return(emptyPlot())
  }
  resultTidy <- tidyResult(result, "prevalence", attrition = FALSE)
  return(
    plotPopulation(resultTidy, x, y, facet, colour, type = "prevalence")
  )
}

# helper functions
plotPopulation <- function(result,
                           x,
                           y = "denominator_count",
                           facet = NULL,
                           colour = NULL,
                           type) {
  labels <- c(
    "outcome_cohort_name", "denominator_count", "outcome_count", "person_days",
    "incidence_100000_pys", "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper",
    "prevalence", "prevalence_95CI_lower", "prevalence_95CI_upper",
    "incidence_start_date", "incidence_end_date", "prevalence_start_date", "prevalence_end_date"
  )

  labels <- labels[labels %in% colnames(result)]

  if ("analysis_interval" %in% colnames(result)) {
    if (all(unique(result$analysis_interval) == "years") & any(grepl("_date", c(x, y)))) {
      result <- result |>
        dplyr::mutate("date_years" = clock::get_year(.data[[paste0(type, "_start_date")]]))
      if (any(grepl("_date", x))) x[grepl("_date", x)] <- "date_years"
      if (any(grepl("_date", y))) y[grepl("_date", y)] <- "date_years"
    }
  }

  plot <- visOmopResults::barPlot(
    result = result,
    x = x,
    y = y,
    facet = facet,
    colour = colour,
    label = labels
  ) +
  visOmopResults::themeVisOmop()

  if (y == "person_days") {
    plot <- plot +
      ggplot2::ylab("Person-Days")
  }
  if (x == "person_days") {
    plot <- plot +
      ggplot2::xlab("Person-Days")
  }
  if (y == "person_years") {
    plot <- plot +
      ggplot2::ylab("Person-Years")
  }
  if (x == "person_years") {
    plot <- plot +
      ggplot2::xlab("Person-Years")
  }
  if (x == "date_years") {
    plot <- plot +
      ggplot2::scale_x_continuous(breaks = seq.int(min(result$date_years), max(result$date_years), by = 1)) +
      ggplot2::xlab("Date (years)")
  }
  if (y == "date_years") {
    plot <- plot +
      ggplot2::scale_x_continuous(breaks = seq.int(min(result$date_years), max(result$date_years), by = 1)) +
      ggplot2::ylab("Date (years)")
  }

  return(plot + visOmopResults::themeVisOmop())
}

plotEstimates <- function(result,
                          x,
                          y,
                          line,
                          point,
                          ribbon,
                          ymin,
                          ymax,
                          facet,
                          colour,
                          type) {
  rlang::check_installed("ggplot2", reason = "for plot functions")
  rlang::check_installed("scales", reason = "for plot functions")

  if (any(c(y, x) %in% c("prevalence"))) {
    ytype <- "percentage"
  } else {
    ytype <- "count"
  }

  labels <- c(
    "outcome_cohort_name", "denominator_count", "outcome_count", "person_days", y, ymin, ymax,
    "incidence_start_date", "incidence_end_date", "prevalence_start_date", "prevalence_end_date"
  )
  labels <- labels[labels %in% colnames(result)]

  # if interval is years --> use years in axis
  if ("analysis_interval" %in% colnames(result)) {
    if (all(unique(result$analysis_interval) == "years") & any(grepl("_date", c(x, y)))) {
      result <- result |>
        dplyr::mutate("date_years" = clock::get_year(.data[[paste0(type, "_start_date")]]))
      if (any(grepl("_date", x))) x[grepl("_date", x)] <- "date_years"
      if (any(grepl("_date", y))) y[grepl("_date", y)] <- "date_years"
    }
  }

  plot <- visOmopResults::scatterPlot(
    result = result,
    x = x,
    y = y,
    line = line,
    point = point,
    ribbon = ribbon,
    ymin = ymin,
    ymax = ymax,
    facet = facet,
    colour = colour,
    group = colour,
    label = labels
  )

  ylim <- c(0, NA)
  if (y == "prevalence") {
    if (ytype == "count") {
      plot <- plot +
        ggplot2::scale_y_continuous(labels = scales::comma)
    }
    if (ytype == "percentage") {
      plot <- plot +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
    }
  }
  if (x == "prevalence") {
    if (ytype == "count") {
      plot <- plot +
        ggplot2::scale_x_continuous(labels = scales::comma)
    }
    if (ytype == "percentage") {
      plot <- plot +
        ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 0.1))
    }
  }
  if (plot$labels$y == "Incidence 100000 pys") {
    plot <- plot + ggplot2::labs(y = "Incidence (100,000 person-years)")
  }
  if (plot$labels$x == "Incidence 100000 pys") {
    plot <- plot + ggplot2::labs(x = "Incidence (100,000 person-years)")
  }
  if (x == "date_years") {
    plot <- plot +
      ggplot2::scale_x_continuous(breaks = seq.int(min(result$date_years), max(result$date_years), by = 1)) +
      ggplot2::xlab("Date (years)")
  }
  if (y == "date_years") {
    plot <- plot +
      ggplot2::scale_x_continuous(breaks = seq.int(min(result$date_years), max(result$date_years), by = 1)) +
      ggplot2::ylab("Date (years)")
  }

  return(plot + visOmopResults::themeVisOmop())
}

addYLimits <- function(plot, ylim, ytype) {
  if (ytype == "count") {
    plot <- plot +
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        limits = ylim
      )
  }
  if (ytype == "percentage") {
    plot <- plot +
      ggplot2::scale_y_continuous(
        labels =
          scales::percent_format(accuracy = 0.1),
        limits = ylim
      )
  }
  return(plot)
}

emptyPlot <- function(title = "No result to plot",
                      subtitle = "") {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    )
}

toDate <- function(x) {
  tryCatch(
    expr = {
      x <- as.Date(x)
    },
    error = function(e) e,
    finally = return(x)
  )
}

#' Variables that can be used for faceting and colouring incidence plots
#'
#' @param result Incidence results
#' @param varying If FALSE, only variables with non-unique values will be
#' returned, otherwise all available variables will be returned
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2014-01-01"), as.Date("2018-01-01"))
#' )
#' inc <- estimateIncidence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' availableIncidenceGrouping(inc)
#' }
availableIncidenceGrouping <- function(result, varying = FALSE) {
  omopgenerics::assertLogical(varying, length = 1)
  result <- omopgenerics::validateResultArgument(result) |>
    omopgenerics::filterSettings(.data$result_type == "incidence")
  availableGrouping(result, varying)
}

#' Variables that can be used for faceting and colouring prevalence plots
#'
#' @param result Prevalence results
#' @param varying If FALSE, only variables with non-unique values will be
#' returned, otherwise all available variables will be returned
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2014-01-01"), as.Date("2018-01-01"))
#' )
#' prev <- estimatePointPrevalence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' availablePrevalenceGrouping(prev)
#' }
availablePrevalenceGrouping <- function(result, varying = FALSE) {
  omopgenerics::assertLogical(varying, length = 1)
  result <- omopgenerics::validateResultArgument(result) |>
    omopgenerics::filterSettings(.data$result_type == "prevalence")
  availableGrouping(result, varying)
}

availableGrouping <- function(result, varying) {
  cols <- c(
    "cdm_name", "outcome_cohort_name", omopgenerics::strataColumns(result),
    "denominator_age_group", "denominator_sex", "denominator_target_cohort_name",
    "denominator_days_prior_observation"
  )
  if (!varying) {
    resultTidy <- tidyResult(result, unique(omopgenerics::settings(result)$result_type), attrition = FALSE)
    pos <- lapply(as.list(cols), function(x, res = resultTidy) {
      res |>
        dplyr::pull(.data[[x]]) |>
        unique() |>
        length()
    }) |> unlist()
    cols <- cols[pos > 1]
  }
  return(cols)
}
